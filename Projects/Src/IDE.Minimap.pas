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

const
  MinimapGutterWidth = 8;
  { ^ VS Code's MINIMAP_GUTTER_WIDTH. Not scaled for DPI. VS Code draws it
      unscaled too (see dx in InnerMinimap._renderLine), but scales it when it
      calculates the minimap's width, which perhaps is a bug. We never scale
      it, for consistency, and to avoid complications. }

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
      FHovering, FHoveringSlider: Boolean;
      FLastSliderRect: TRect;
      FMemo: TIDEScintEdit;
      procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
      procedure GetDocLineRangeInView(out FirstLine, LastLine: Integer);
      function GetMemoLineFromMinimapLine(const MinimapLine: Integer): Integer;
      function GetMinimapLayout: TMinimapLayout;
      class function GetNextShownLine(const Edit: TIDEScintEdit;
        const StartLine: Integer): Integer; static;
      function GetSliderRect(const MinimapLayout: TMinimapLayout): TRect;
      procedure SetDocPointer;
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
    procedure SyncFolding;
    procedure SyncScrollPosition;
    procedure SyncSelections;
    procedure UpdateFont(const AFont: TFont);
    procedure UpdateTheme;
  end;

implementation

uses
  Windows, ActiveX,
  Math,
  ModernColors, ScintEdit,
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

  { No right blank margin, scrollbars or caret on the minimap }
  RightBlankMarginWidth := 0;
  VerticalScrollBar := False;
  HorizontalScrollBar := False;
  CaretStyle := scsInvisible;

  { Regular cursor always }
  SetCursorID(SC_CURSORARROW);

  { Grayscale anti-aliasing, because Windows' default ClearType gives the
    edges of characters a colored tint, which the contrast boost would make
    stronger }
  FontQuality := sfqAntialiased;

  { Scintilla makes each tab at least 2 pixels wide, which at this font size
    is a column or more, so tabs would be wider than in the memo }
  TabMinimumWidth := 0;

  { Turn automatic folding back off: SyncFolding handles it }
  AutomaticFold := [];

  { Use the gutter for a single margin showing change history }
  for var Margin := 0 to MarginCount-1 do
    MarginWidth[Margin] := 0;
  MarginType[mmChangeHistory] := smtBack;
  { Like VS Code, use 2 pixel wide change markers, not scaled for DPI }
  const ChangeHistoryWidth = 2 * 3; { SC_MARK_BAR shows a bar a third of its width }
  MarginWidth[mmChangeHistory] := ChangeHistoryWidth;
  { Add a blank margin between the change history margin and the text.
    Together with the previous call, this ensures a change marker has 2 blank
    pixels before it and the rest after it, like VS Code. }
  LeftBlankMarginWidth := MinimapGutterWidth - ChangeHistoryWidth;

  { Show change history as markers, but leave enabling it to the memo, because
    that's document-level. Must come before SetDocPointer, else this would
    disable it for the memo's document. }
  ChangeHistoryDisplay := schMarkers;

  { Ensure the icon markers stay invisible now that the minimap has no margin
    for them. Without this Scintilla would display them as line markers instead. }
  for var Marker: TScintMarkerNumber := mmiFirst to mmiLast do
    MarkerSymbol[Marker] := smsEmpty;

  { Like VS Code, draw occurrences as boxes. Also see TMainForm.SyncEditorOptions
    for squiggly boxes. }
  for var Indicator: TScintIndicatorNumber in [minWordAtCursorOccurrence, minSelTextOccurrence] do begin
    IndicatorStyle[Indicator] := sisFullBox;
    Call(SCI_INDICSETUNDER, Indicator, 0); { Over the text, like VS Code }
  end;

  { No change notifications or undo selection history, don't need them }
  ChangeNotifications := False;
  UndoSelectionHistory := False;

  { Ensure document is shared }
  SetDocPointer;
end;

procedure TMinimap.GetDocLineRangeInView(out FirstLine, LastLine: Integer);
{ From the top line through the line below the last fully shown one, hidden
  lines included. That line is usually partly shown. }
begin
  FirstLine := GetDocLineFromVisibleLine(TopLine);
  LastLine := Min(GetDocLineFromVisibleLine(TopLine + LinesInWindow), Lines.Count-1);
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

class function TMinimap.GetNextShownLine(const Edit: TIDEScintEdit;
  const StartLine: Integer): Integer;
{ Returns Edit's first shown line from StartLine on, or Edit.Lines.Count if
  there's none }
begin
  const VisibleLine = Edit.GetVisibleLineFromDocLine(StartLine);
  if VisibleLine < Edit.GetVisibleLineFromDocLine(Edit.Lines.Count) then
    Result := Edit.GetDocLineFromVisibleLine(VisibleLine)
  else { GetDocLineFromVisibleLine would return the last line }
    Result := Edit.Lines.Count;
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
     (XOffset <> 0) then { Check because setting XOffset always repaints }
    XOffset := 0; { Safe: queues another SCN_UPDATEUI but Scintilla then discards it on return }
  inherited;
end;

procedure TMinimap.SetDocPointer;
begin
  if (FMemo <> nil) and HandleAllocated then begin
    DocumentPointer := FMemo.DocumentPointer;
    SyncFolding; { Setting DocumentPointer resets the fold state }
    SyncScrollPosition;
    SyncSelections; { Uses the scroll position }
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

procedure TMinimap.SyncFolding;
begin
  if (FMemo = nil) or not HandleAllocated then
    Exit;

  { Determine the hidden lines from the lines the memo shows, not from the
    fold levels, because the memo may have folded with other levels: its
    styler sets them lazily. Assumes the memo hides only lines right after a
    contracted header, which holds for every way the IDE folds. }

  ShowLines(0, Lines.Count-1);

  var Header := FMemo.GetNextContractedFoldHeader(0);
  while Header >= 0 do begin
    const NextShownLine = GetNextShownLine(FMemo, Header + 1);
    HideLines(Header + 1, NextShownLine - 1);
    Header := FMemo.GetNextContractedFoldHeader(NextShownLine);
  end;
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

procedure TMinimap.SyncSelections;

  function IsWithinHiddenLines(const StartPosition, EndPosition: Integer): Boolean;
  begin
    { Lines are hidden if they take no visible lines }
    Result := GetVisibleLineFromDocLine(GetLineFromPosition(StartPosition)) =
      GetVisibleLineFromDocLine(GetLineFromPosition(EndPosition) + 1);
  end;

begin
  if (FMemo = nil) or not HandleAllocated then
    Exit;

  { Copies the memo's non-empty selections only. The minimap doesn't need the
    empty ones because its caret is invisible. }

  if FMemo.AllSelectionsEmpty then begin
    if (SelectionCount > 1) or not AllSelectionsEmpty then
      SetEmptySelection;
    Exit;
  end;

  { Adding thousands of selections takes noticeable time, so after the first
    ones this only copies those the minimap shows. Limitation: when the minimap
    then shows other lines, such as when it scrolls, grows, unfolds, or is
    shown, the selections it didn't copy stay missing until the memo's
    selections or content change. }
  const AlwaysCopiedSelectionCount = 1000;
  var FirstLineInView, LastLineInView: Integer;
  GetDocLineRangeInView(FirstLineInView, LastLineInView);
  const FirstPositionInView = GetPositionFromLine(FirstLineInView);
  const LastPositionInView = GetLineEndPosition(LastLineInView);

  { Updates the minimap's selections in place and adds or removes them only at
    the end: rebuilding them would take quadratic time, because adding a
    selection trims all others. Only sets a selection when it changed, because
    setting one repaints the minimap. }
  var MinimapSelection := 0;
  for var MemoSelection := 0 to FMemo.SelectionCount-1 do begin
    const CaretPosition = FMemo.SelectionCaretPosition[MemoSelection];
    const CaretVirtualSpace = FMemo.SelectionCaretVirtualSpace[MemoSelection];
    const AnchorPosition = FMemo.SelectionAnchorPosition[MemoSelection];
    const AnchorVirtualSpace = FMemo.SelectionAnchorVirtualSpace[MemoSelection];
    if (CaretPosition = AnchorPosition) and (CaretVirtualSpace = AnchorVirtualSpace) then
      Continue;
    if MinimapSelection >= AlwaysCopiedSelectionCount then begin
      const StartPosition = Min(CaretPosition, AnchorPosition);
      const EndPosition = Max(CaretPosition, AnchorPosition);
      if (StartPosition > LastPositionInView) or (EndPosition < FirstPositionInView) or
         IsWithinHiddenLines(StartPosition, EndPosition) then
        Continue;
    end;
    if MinimapSelection = SelectionCount then begin
      AddSelection(CaretPosition, AnchorPosition);
      { Adding removes the selections it trims to empty, so the new one can be
        at a lower index }
      MinimapSelection := SelectionCount-1;
    end;
    if (SelectionCaretPosition[MinimapSelection] <> CaretPosition) or
       (SelectionCaretVirtualSpace[MinimapSelection] <> CaretVirtualSpace) or
       (SelectionAnchorPosition[MinimapSelection] <> AnchorPosition) or
       (SelectionAnchorVirtualSpace[MinimapSelection] <> AnchorVirtualSpace) then begin
      SelectionCaretPosition[MinimapSelection] := CaretPosition;
      SelectionCaretVirtualSpace[MinimapSelection] := CaretVirtualSpace;
      SelectionAnchorPosition[MinimapSelection] := AnchorPosition;
      SelectionAnchorVirtualSpace[MinimapSelection] := AnchorVirtualSpace;
    end;
    Inc(MinimapSelection);
  end;
  for var Selection := SelectionCount-1 downto MinimapSelection do
    RemoveSelection(Selection);
end;

procedure TMinimap.UpdateFont(const AFont: TFont);
begin
  Font.Assign(AFont);
  Font.Size := MinimapFontSize;
end;

procedure TMinimap.UpdateTheme;
const
  { When changing these, also recompute the line highlight alphas in
    DrawLineHighlights in WMPaint }
  SquigglyAlphas: array[TThemeType] of Byte = (171, 217, 128);
  OccurrenceAlphas: array[TThemeType] of Byte = (42, 162, 42);
begin
  UpdateThemeColorsAndStyleAttributes;
  if Theme <> nil then begin
    const SquigglyAlpha = SquigglyAlphas[Theme.Typ];
    Call(SCI_INDICSETALPHA, minSquiggly, SquigglyAlpha);
    Call(SCI_INDICSETOUTLINEALPHA, minSquiggly, SquigglyAlpha);
    { Like VS Code, use one color for both kinds of occurrences }
    const OccurrenceAlpha = OccurrenceAlphas[Theme.Typ];
    for var Indicator: TScintIndicatorNumber in [minWordAtCursorOccurrence, minSelTextOccurrence] do begin
      Call(SCI_INDICSETFORE, Indicator, Theme.Colors[tcMinimapOccurrenceBack]);
      Call(SCI_INDICSETALPHA, Indicator, OccurrenceAlpha);
      Call(SCI_INDICSETOUTLINEALPHA, Indicator, OccurrenceAlpha);
    end;
    SetSelectionBackColor(Theme.Colors[tcMinimapSelBack]);
  end;
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

  procedure DrawAlphaRects(const DC: HDC; const Rects: array of TRect;
    const Color: TColor; const Alpha: Byte);
  begin
    if Length(Rects) = 0 then
      Exit;
    const ColorDC = CreateCompatibleDC(DC);
    if ColorDC = 0 then
      Exit;
    try
      const ColorBitmap = CreateCompatibleBitmap(DC, 1, 1);
      if ColorBitmap = 0 then
        Exit;
      try
        const OldBitmap = SelectObject(ColorDC, ColorBitmap);
        try
          SetPixel(ColorDC, 0, 0, COLORREF(ColorToRGB(Color)));
          var BlendFunction: TBlendFunction;
          BlendFunction.BlendOp := AC_SRC_OVER;
          BlendFunction.BlendFlags := 0;
          BlendFunction.SourceConstantAlpha := Alpha;
          BlendFunction.AlphaFormat := 0;
          for var Rect in Rects do
            AlphaBlend(DC, Rect.Left, Rect.Top, Rect.Width, Rect.Height,
              ColorDC, 0, 0, 1, 1, BlendFunction);
        finally
          SelectObject(ColorDC, OldBitmap);
        end;
      finally
        DeleteObject(ColorBitmap);
      end;
    finally
      DeleteDC(ColorDC);
    end;
  end;

  function LineHasIndicator(const IndicatorNumber: TScintIndicatorNumber;
    const Line: Integer): Boolean;
  begin
    const LineStartPos = GetPositionFromLine(Line);
    const LineEndPos = GetLineEndPosition(Line);
    if GetIndicatorAtPosition(IndicatorNumber, LineStartPos) then
      Exit(True);
    const RunEndPos = Call(SCI_INDICATOREND, IndicatorNumber, LineStartPos);
    { If the run without the indicator ends within the line, the run after it
      has the indicator. SCI_INDICATOREND returns 0 if no character has the
      indicator. }
    Result := (RunEndPos > LineStartPos) and (RunEndPos < LineEndPos);
  end;

  function GetLineRect(const Line, MinimapLineHeight: Integer): TRect;
  begin
    const LineTop = (GetVisibleLineFromDocLine(Line) - TopLine) * MinimapLineHeight;
    const LineBottom = (GetVisibleLineFromDocLine(Line + 1) - TopLine) * MinimapLineHeight;
    Result := Rect(MinimapGutterWidth, LineTop, ClientWidth, LineBottom);
  end;

  procedure DrawLineHighlights(const DC: HDC);
  { Like VS Code's _renderSelectionLineHighlights and
    _renderDecorationsLineHighlights, highlights each line of a selection
    except its last, and each line with an error or an occurrence, over the
    full width, gutter excluded. Unlike VS Code, draws the highlights over the
    boxes and the selection instead of under them. }
  const
    { VS Code draws a line highlight in the color of its box or selection at
      half its alpha. These alphas are half those the boxes in UpdateTheme and
      the minimap selection colors in ModernColors have after the contrast
      boost. }
    SelectionLineHighlightAlphas: array[TThemeType] of Byte = (115, 115, 115);
    SquigglyLineHighlightAlphas: array[TThemeType] of Byte = (128, 84, 96);
    OccurrenceLineHighlightAlphas: array[TThemeType] of Byte = (59, 59, 59);
  begin
    const ShowSquiggly = IndicatorStyle[minSquiggly] <> sisHidden;
    const MinimapLineHeight = LineHeight;

    var FirstLine, LastLine: Integer;
    GetDocLineRangeInView(FirstLine, LastLine);

    var LineHasSelectionHighlight: TArray<Boolean>;
    SetLength(LineHasSelectionHighlight, LastLine - FirstLine + 1);
    for var Selection := 0 to SelectionCount-1 do begin
      const SelectionStartLine = GetLineFromPosition(SelectionStartPosition[Selection]);
      const SelectionEndLine = GetLineFromPosition(SelectionEndPosition[Selection]);
      for var Line := Max(SelectionStartLine, FirstLine) to Min(SelectionEndLine-1, LastLine) do
        LineHasSelectionHighlight[Line - FirstLine] := True;
    end;

    var SelectionLineRects, SquigglyLineRects, OccurrenceLineRects: TArray<TRect>;

    var Line := FirstLine;
    while Line <= LastLine do begin
      { Like VS Code, a selection's highlight wins over an error's, and an
        error's over an occurrence's. VS Code also hides them on a
        selection's last line, which includes a caret's line. We don't. }
      if LineHasSelectionHighlight[Line - FirstLine] then
        SelectionLineRects := SelectionLineRects + [GetLineRect(Line, MinimapLineHeight)]
      else if ShowSquiggly and LineHasIndicator(minSquiggly, Line) then
        SquigglyLineRects := SquigglyLineRects + [GetLineRect(Line, MinimapLineHeight)]
      else if LineHasIndicator(minWordAtCursorOccurrence, Line) or
              LineHasIndicator(minSelTextOccurrence, Line) then
        OccurrenceLineRects := OccurrenceLineRects + [GetLineRect(Line, MinimapLineHeight)];

      Line := GetNextShownLine(Self, Line + 1);
    end;

    DrawAlphaRects(DC, SelectionLineRects, Theme.Colors[tcSelBack],
      SelectionLineHighlightAlphas[Theme.Typ]);
    DrawAlphaRects(DC, SquigglyLineRects, Theme.Colors[tcRed],
      SquigglyLineHighlightAlphas[Theme.Typ]);
    DrawAlphaRects(DC, OccurrenceLineRects, Theme.Colors[tcMinimapOccurrenceBack],
      OccurrenceLineHighlightAlphas[Theme.Typ]);
  end;

  procedure DrawSlider(const DC: HDC; const SliderRect: TRect);
  const
    { These give the colors of VS Code's slider }
    SliderAlphas: array[TThemeType, Boolean] of Byte = ((40, 70), (25, 33), (31, 54));
    SliderAlphasDragging: array[TThemeType] of Byte = (99, 45, 76);
  begin
    if SliderRect.IsEmpty then
      Exit;
    var SliderAlpha: Byte;
    if FDrag.Dragging then
      SliderAlpha := SliderAlphasDragging[Theme.Typ]
    else
      SliderAlpha := SliderAlphas[Theme.Typ, FHoveringSlider];
    DrawAlphaRects(DC, [SliderRect], Theme.Colors[tcFore], SliderAlpha);
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
          BoostTextContrast(pvBits, BitmapWidth, BitmapHeight, MinimapGutterWidth,
            Color, ContrastBoosts[(Theme <> nil) and Theme.Dark]);
          { ^ Color could be the wrong background for lines with a line marker and
              for text with an indicator, but the effect is negligible }
          if (FMemo <> nil) and (Theme <> nil) then begin
            DrawLineHighlights(MemoryDC);
            if FHovering or FDrag.Dragging then
              DrawSlider(MemoryDC, GetSliderRect(GetMinimapLayout));
          end;
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
