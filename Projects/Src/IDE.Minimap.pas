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
  Classes, Graphics,
  ScintInt,
  IDE.IDEScintEdit;

type
  TMinimap = class(TIDEScintEdit)
  private
    FGutterWidth: Integer;
    FMemo: TIDEScintEdit;
    procedure SetDocPointer;
    procedure SetGutterWidth(const Value: Integer);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateWnd; override;
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

  { When nothing changed, such as on a caret blink, each Scintilla call below
    just reads stored values, except GetDocLineFromVisibleLine, which does a
    binary search over the document lines when the memo folds or wraps. Even
    with 100,000 lines, this routine was measured to only take about 8
    microseconds. }

  const VisibleLineCount = GetVisibleLineFromDocLine(Lines.Count);

  const MemoTopLine = FMemo.TopLine;
  const MemoLastDocLine = FMemo.Lines.Count-1;
  { Wrapping and folding are per view, so the memo's visible lines can differ
    from the minimap's: convert through the document lines they share }
  const MemoFirstVisibleDocLine = FMemo.GetDocLineFromVisibleLine(MemoTopLine);
  const MemoLastFullyVisibleDocLine = Min(FMemo.GetDocLineFromVisibleLine(MemoTopLine +
    FMemo.LinesInWindow - 1), MemoLastDocLine);

  { The viewport is the slider: the minimap's lines of what the memo shows }
  const ViewportStartLine = GetVisibleLineFromDocLine(MemoFirstVisibleDocLine);
  const ViewportLineCount = GetVisibleLineFromDocLine(MemoLastFullyVisibleDocLine + 1) -
    ViewportStartLine;

  TopLine := CalculateMinimapTopLine(VisibleLineCount, LinesInWindow,
    ViewportStartLine, ViewportLineCount);
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

procedure TMinimap.WMPaint(var Message: TWMPaint);
{ Paints Scintilla's content into a bitmap first, so we can make changes to it
  ourselves afterwards. Uses WM_PRINTCLIENT because Scintilla's own WM_PAINT
  ignores Message.DC and paints straight to the screen. }

  procedure PrintClient(const DC: HDC);
  begin
    CallWindowProc(DefWndProc, Handle, WM_PRINTCLIENT, WPARAM(DC), PRF_CLIENT);
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
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK, WM_LBUTTONUP,
    WM_RBUTTONDOWN, WM_RBUTTONDBLCLK, WM_RBUTTONUP,
    WM_MBUTTONDOWN, WM_MBUTTONDBLCLK, WM_MBUTTONUP,
    WM_MOUSEWHEEL, WM_MOUSEHWHEEL:
      ;
  else
    inherited;
  end;
end;

end.
