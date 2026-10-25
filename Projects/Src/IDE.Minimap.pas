unit IDE.Minimap;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE Minimap
}

interface

uses
  Messages,
  Types, Classes, Graphics, Controls,
  ScintInt,
  IDE.IDEScintEdit;

type
  TMinimap = class(TIDEScintEdit)
  private
    type
      TMinimapLayout = record
        LineCount, MinimapLinesFitting, ViewportStartLine, ViewportLineCount: Integer;
      end;
      TMinimapLayoutDrag = record
        Dragging: Boolean;
        StartMinimapLayout: TMinimapLayout;
        StartMemoTopLine, StartY: Integer;
      end;
    var
      FDrag: TMinimapLayoutDrag;
      FGutterWidth: Integer;
      FHovering, FHoveringSlider: Boolean;
      FLastSliderRect: TRect;
      FMemo: TIDEScintEdit;
      procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
      function GetMemoLineFromMinimapLine(const MinimapLine: Integer): Integer;
      function GetMinimapLayout: TMinimapLayout;
      function GetSliderRect(const MinimapLayout: TMinimapLayout): TRect;
      procedure SetDocPointer;
      procedure SetGutterWidth(const Value: Integer);
      procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
      procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateWnd; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Notify(const N: TSCNotification); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetMemo(const AMemo: TIDEScintEdit);
    procedure SyncScrollPosition;
    procedure UpdateFont(const AFont: TFont);
    procedure UpdateTheme;
    property GutterWidth: Integer read FGutterWidth write SetGutterWidth;
  end;

implementation

uses
  Windows, ActiveX,
  Math,
  ModernColors,
  IDE.HelperFunc;

const
  { VS Code's minimap with its default scale draws each character 1 device
    pixel wide below 200% scaling, and 2 at 200%. At 1 point, characters of
    this minimap have the same widths. See minimapScale and minimapCharWidth
    in EditorLayoutInfoComputer._computeMinimapLayout. }
  MinimapFontSize = 1;

{ TMinimap }

constructor TMinimap.Create(AOwner: TComponent);
begin
  inherited;
  ParentShowHint := False;
  ShowHint := False;
  TabStop := False;
end;

procedure TMinimap.CMMouseEnter(var Message: TMessage);
begin
  FHovering := True;
  Invalidate;
  inherited;
end;

procedure TMinimap.CMMouseLeave(var Message: TMessage);
begin
  FHovering := False;
  FHoveringSlider := False;
  Invalidate;
  inherited;
end;

procedure TMinimap.CreateWnd;
begin
  inherited;

  { Scintilla registers every window as a drop target, and a drop would insert
    text into the document }
  RevokeDragDrop(Handle);

  { No blank margins, scrollbars or caret on the minimap }
  Call(SCI_SETMARGINLEFT, 0, 0);
  Call(SCI_SETMARGINRIGHT, 0, 0);
  Call(SCI_SETVSCROLLBAR, 0, 0);
  Call(SCI_SETHSCROLLBAR, 0, 0);
  Call(SCI_SETCARETSTYLE, CARETSTYLE_INVISIBLE, 0);

  { Regular cursor always }
  SetCursorID(SC_CURSORARROW);

  { Grayscale anti-aliasing, because Windows' default ClearType gives the
    edges of characters a colored tint, which the contrast boost would make
    stronger }
  Call(SCI_SETFONTQUALITY, SC_EFF_QUALITY_ANTIALIASED, 0);

  { Use the entire gutter for a single margin showing change history }
  for var Margin := 0 to Call(SCI_GETMARGINS, 0, 0)-1 do
    Call(SCI_SETMARGINWIDTHN, Margin, 0);
  Call(SCI_SETMARGINTYPEN, mmChangeHistory, SC_MARGIN_BACK);
  Call(SCI_SETMARGINWIDTHN, mmChangeHistory, FGutterWidth);

  { Ensure the icon markers stay invisible now that the minimap has no margin
    for them. Without this Scintilla would display them as line markers instead. }
  for var Marker := mmiFirst to mmiLast do
    Call(SCI_MARKERDEFINE, Marker, SC_MARK_EMPTY);

  { No change notifications or undo selection history, don't need them }
  Call(SCI_SETMODEVENTMASK, 0, 0);
  Call(SCI_SETUNDOSELECTIONHISTORY, SC_UNDO_SELECTION_HISTORY_DISABLED, 0);

  { Ensure document is shared }
  SetDocPointer;
end;

function TMinimap.GetMemoLineFromMinimapLine(const MinimapLine: Integer): Integer;
begin
  { MinimapLine is below the last line when clicking below the end of the
    document. GetDocLineFromVisibleLine then returns a document line that
    doesn't exist, so we clamp. }
  const DocLine = Min(GetDocLineFromVisibleLine(MinimapLine), Lines.Count-1);
  Result := FMemo.GetVisibleLineFromDocLine(DocLine);
  { ^ Result can be off while the memo still has lines Scintilla hasn't
      wrapped yet, such as right after a resize with word wrap on.
      EnsureLineVisible would wrap them first, but it also unfolds. }
end;

function TMinimap.GetMinimapLayout: TMinimapLayout;
begin
  { When nothing changed, such as on a caret blink, each Scintilla call below
    just reads stored values, except GetDocLineFromVisibleLine, which does a
    binary search over the document lines when the memo folds or wraps. Even
    with 100,000 lines, this routine was measured to only take about 8
    microseconds. }

  Result.LineCount := GetVisibleLineFromDocLine(Lines.Count);
  Result.MinimapLinesFitting := LinesInWindow;

  const MemoTopLine = FMemo.TopLine;
  const MemoLastDocLine = FMemo.Lines.Count-1;
  { Wrapping and folding are per view, so the memo's visible lines can differ
    from the minimap's: convert through the document lines they share }
  const MemoFirstVisibleDocLine = FMemo.GetDocLineFromVisibleLine(MemoTopLine);
  const MemoLastFullyVisibleDocLine = Min(FMemo.GetDocLineFromVisibleLine(MemoTopLine +
    FMemo.LinesInWindow - 1), MemoLastDocLine);

  { The viewport is the slider: the minimap's lines of what the memo shows }
  Result.ViewportStartLine := GetVisibleLineFromDocLine(MemoFirstVisibleDocLine);
  Result.ViewportLineCount := GetVisibleLineFromDocLine(MemoLastFullyVisibleDocLine + 1) -
    Result.ViewportStartLine;
end;

function TMinimap.GetSliderRect(const MinimapLayout: TMinimapLayout): TRect;
begin
  { Like VS Code's with editor.scrollBeyondLastLine off, there's no slider
    while it can't move, such as while the memo shows the whole document. See
    maxMinimapSliderTop and sliderNeeded in the static create method of class
    MinimapLayout. Limitation: ViewportLineCount counts whole document lines,
    so with word wrap on, the slider can disappear and reappear while the memo
    scrolls through a short document with long wrapped lines. }
  const MaxMinimapSliderTop = Min(MinimapLayout.LineCount, MinimapLayout.MinimapLinesFitting) -
    MinimapLayout.ViewportLineCount; { Also see CalculateMinimapViewportStartLineFromDelta }
  if MaxMinimapSliderTop <= 0 then
    Exit(TRect.Empty);
  Result := Bounds(0, (MinimapLayout.ViewportStartLine - TopLine) * LineHeight, ClientWidth,
    MinimapLayout.ViewportLineCount * LineHeight);
end;

procedure TMinimap.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FDrag.Dragging and (FMemo <> nil) then begin
    const NewViewportStartLine = CalculateMinimapViewportStartLineFromDelta(
      FDrag.StartMinimapLayout.LineCount, FDrag.StartMinimapLayout.MinimapLinesFitting,
      FDrag.StartMinimapLayout.ViewportStartLine, FDrag.StartMinimapLayout.ViewportLineCount,
      (Y - FDrag.StartY) / LineHeight);
    { When the slider is back where it was at the start of the drag, use
      StartMemoTopLine to restore the top line, instead of using
      GetMemoLineFromMinimapLine. This makes it able to restore the second
      line of a wrapped line as the top line. Limitation: when the last line
      wraps into more lines than the memo can show, the drag stops at the
      start of that line. }
    if NewViewportStartLine = FDrag.StartMinimapLayout.ViewportStartLine then
      FMemo.TopLine := FDrag.StartMemoTopLine
    else
      FMemo.TopLine := GetMemoLineFromMinimapLine(NewViewportStartLine);
  end;
  const HoveringSlider = (FMemo <> nil) and PtInRect(GetSliderRect(GetMinimapLayout), Point(X, Y));
  if FHoveringSlider <> HoveringSlider then begin
    FHoveringSlider := HoveringSlider;
    Invalidate;
  end;
end;

procedure TMinimap.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FMemo) then
    FMemo := nil;
end;

procedure TMinimap.Notify(const N: TSCNotification);
begin
  { Undo and redo make every view scroll its caret into view, also horizontally.
    Detect and fix using SCN_UPDATEUI. Comes before the paint so doesn't
    cause flicker. }
  if (N.nmhdr.code = SCN_UPDATEUI) and (N.updated and SC_UPDATE_H_SCROLL <> 0) and
     (Call(SCI_GETXOFFSET, 0, 0) <> 0) then { Check because SCI_SETXOFFSET always repaints }
    Call(SCI_SETXOFFSET, 0, 0); { Safe: queues another SCN_UPDATEUI but Scintilla then discards it on return }
  inherited;
end;

procedure TMinimap.SetDocPointer;
begin
  if (FMemo <> nil) and HandleAllocated then begin
    DocumentPointer := FMemo.DocumentPointer;
    SyncScrollPosition;
  end;
end;

procedure TMinimap.SetGutterWidth(const Value: Integer);
begin
  if FGutterWidth <> Value then begin
    FGutterWidth := Value;
    if HandleAllocated then
      Call(SCI_SETMARGINWIDTHN, mmChangeHistory, FGutterWidth);
  end;
end;

procedure TMinimap.SetMemo(const AMemo: TIDEScintEdit);
{ Always calls SetDocPointer, so resyncs always }
begin
  if FMemo <> AMemo then begin
    if FDrag.Dragging then
      MouseCapture := False;
    if FMemo <> nil then
      FMemo.RemoveFreeNotification(Self);
    FMemo := AMemo;
    if FMemo <> nil then
      FMemo.FreeNotification(Self);
  end;
  SetDocPointer;
end;

procedure TMinimap.SyncScrollPosition;
begin
  if (FMemo = nil) or not HandleAllocated or not Visible then
    Exit;

  const MinimapLayout = GetMinimapLayout;
  TopLine := CalculateMinimapTopLine(MinimapLayout.LineCount, MinimapLayout.MinimapLinesFitting,
    MinimapLayout.ViewportStartLine, MinimapLayout.ViewportLineCount);

  const SliderRect = GetSliderRect(MinimapLayout);
  if SliderRect <> FLastSliderRect then begin
    FLastSliderRect := SliderRect;
    if FHovering then begin
      { The memo's scroll, or the minimap's own, may have moved the slider
        under the mouse, or away from it }
      var CursorPos: TPoint;
      FHoveringSlider := GetCursorPos(CursorPos) and PtInRect(SliderRect, ScreenToClient(CursorPos));
    end;
    if FHovering or FDrag.Dragging then
      Invalidate;
  end;
end;

procedure TMinimap.UpdateFont(const AFont: TFont);
begin
  Font.Assign(AFont);
  Font.Size := MinimapFontSize;
end;

procedure TMinimap.UpdateTheme;
begin
  UpdateThemeColorsAndStyleAttributes;
  Invalidate; { WMPaint depends on the theme }
end;

procedure TMinimap.WMCaptureChanged(var Message: TMessage);
begin
  if FDrag.Dragging then begin
    FDrag.Dragging := False;
    Invalidate;
  end;
  inherited;
end;

procedure TMinimap.WMPaint(var Message: TWMPaint);
{ Paints Scintilla's content into a bitmap first, so we can make changes to it
  ourselves afterwards. Uses WM_PRINTCLIENT because Scintilla's own WM_PAINT
  ignores Message.DC and paints straight to the screen. }

  procedure PrintClient(const DC: HDC);
  begin
    CallWindowProc(DefWndProc, Handle, WM_PRINTCLIENT, WPARAM(DC), PRF_CLIENT);
  end;

  procedure DrawSlider(const DC: HDC; const SliderRect: TRect);
  const
    { These give the colors of VS Code's slider }
    SliderAlphas: array[TThemeType, Boolean] of Byte = ((40, 70), (25, 33), (31, 54));
    SliderAlphasDragging: array[TThemeType] of Byte = (99, 45, 76);
  begin
    if SliderRect.IsEmpty then
      Exit;
    const SliderDC = CreateCompatibleDC(DC);
    if SliderDC = 0 then
      Exit;
    try
      const SliderBitmap = CreateCompatibleBitmap(DC, 1, 1);
      if SliderBitmap = 0 then
        Exit;
      try
        const OldBitmap = SelectObject(SliderDC, SliderBitmap);
        try
          SetPixel(SliderDC, 0, 0, COLORREF(ColorToRGB(Theme.Colors[tcFore])));
          var BlendFunction: TBlendFunction;
          BlendFunction.BlendOp := AC_SRC_OVER;
          BlendFunction.BlendFlags := 0;
          if FDrag.Dragging then
            BlendFunction.SourceConstantAlpha := SliderAlphasDragging[Theme.Typ]
          else
            BlendFunction.SourceConstantAlpha := SliderAlphas[Theme.Typ, FHoveringSlider];
          BlendFunction.AlphaFormat := 0;
          AlphaBlend(DC, SliderRect.Left, SliderRect.Top, SliderRect.Width,
            SliderRect.Height, SliderDC, 0, 0, 1, 1, BlendFunction);
        finally
          SelectObject(SliderDC, OldBitmap);
        end;
      finally
        DeleteObject(SliderBitmap);
      end;
    finally
      DeleteDC(SliderDC);
    end;
  end;

  function PaintToBitmap(const DC: HDC): Boolean;
  const
    { On a light background, anti-aliased text this small is too faint, so
      it's strengthened. On a dark background it's brighter than VS Code's
      minimap, so it's dimmed instead. }
    ContrastBoosts: array[Boolean] of Double = (2, -0.5);
  begin
    const MemoryDC = CreateCompatibleDC(DC);
    if MemoryDC = 0 then
      Exit(False);
    try
      const BitmapWidth = ClientWidth;
      const BitmapHeight = ClientHeight;
      var pvBits: Pointer;
      const Bitmap = CreateDIBSection(DC, CreateBitmapInfo(BitmapWidth, -BitmapHeight, 32),
        DIB_RGB_COLORS, pvBits, 0, 0);
      if Bitmap = 0 then
        Exit(False);
      try
        const OldBitmap = SelectObject(MemoryDC, Bitmap);
        try
          PrintClient(MemoryDC);
          GdiFlush; { See CreateDIBSection's documentation }
          BoostTextContrast(pvBits, BitmapWidth, BitmapHeight, FGutterWidth,
            Color, ContrastBoosts[(Theme <> nil) and Theme.Dark]);
          { ^ Color could be the wrong background for lines with a line marker and
              for text with an indicator, but the effect is negligible }
          if (FHovering or FDrag.Dragging) and (FMemo <> nil) and (Theme <> nil) then
            DrawSlider(MemoryDC, GetSliderRect(GetMinimapLayout));
          Result := BitBlt(DC, 0, 0, BitmapWidth, BitmapHeight, MemoryDC, 0, 0, SRCCOPY);
        finally
          SelectObject(MemoryDC, OldBitmap);
        end;
      finally
        DeleteObject(Bitmap);
      end;
    finally
      DeleteDC(MemoryDC);
    end;
  end;

begin
  { Similar to TWinControl.PaintHandler }
  var PaintStruct: TPaintStruct;
  var DC := Message.DC;
  if DC = 0 then
    DC := BeginPaint(Handle, PaintStruct);
  try
    if not PaintToBitmap(DC) then
      PrintClient(DC);
  finally
    if Message.DC = 0 then
      EndPaint(Handle, PaintStruct);
  end;
end;

procedure TMinimap.WndProc(var Message: TMessage);
begin
  { Scintilla would take the focus, change its own selection, or scroll and zoom
    itself }
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      begin
        { Like VS Code's minimap, acts on mouse down, not on mouse up, like a
          scrollbar track. See _pointerDownListener in its InnerMinimap class. }
        if FMemo <> nil then begin
          const ClickPoint = SmallPointToPoint(TWMMouse(Message).Pos);
          const MinimapLayout = GetMinimapLayout;
          if PtInRect(GetSliderRect(MinimapLayout), ClickPoint) then begin
            MouseCapture := True;
            FDrag.Dragging := True;
            FDrag.StartMinimapLayout := MinimapLayout;
            FDrag.StartMemoTopLine := FMemo.TopLine;
            FDrag.StartY := ClickPoint.Y;
            Invalidate;
          end else begin
            const ClickedMemoLine = GetMemoLineFromMinimapLine(TopLine + ClickPoint.Y div LineHeight);
            FMemo.TopLine := ClickedMemoLine - FMemo.LinesInWindow div 2;
          end;
        end;
      end;
    WM_LBUTTONUP:
      if FDrag.Dragging then
        MouseCapture := False;
    WM_RBUTTONDOWN, WM_RBUTTONDBLCLK, WM_RBUTTONUP,
    WM_MBUTTONDOWN, WM_MBUTTONDBLCLK, WM_MBUTTONUP:
      ;
    WM_MOUSEWHEEL, WM_MOUSEHWHEEL:
      if FMemo <> nil then
        Message.Result := SendMessage(FMemo.Handle, Message.Msg, Message.WParam, Message.LParam);
    WM_SETFOCUS:
      begin
        { Deny getting focus, for example by a screenreader. Prevents typed
          input, which would edit the shared document. Setting ReadOnly
          instead is not possible: it's document-level, so it would make the
          memo read-only too. Changing the focus here is safe:
          TCustomForm.WndProc does it too. }
        const LostFocusWnd = TWMSetFocus(Message).FocusedWnd;
        if ((LostFocusWnd = 0) or (Windows.SetFocus(LostFocusWnd) = 0)) and
           ((FMemo = nil) or (Windows.SetFocus(FMemo.Handle) = 0)) then
          inherited; { Should never happen }
      end;
  else
    inherited;
  end;
end;

end.
