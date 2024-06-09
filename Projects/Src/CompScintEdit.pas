unit CompScintEdit;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE's TScintEdit
}

interface

uses
  Windows, Graphics, Classes, Generics.Collections, ScintInt, ScintEdit, ModernColors;

const
  { Memo marker numbers }
  mmIconHasEntry = 0;        { grey dot }
  mmIconEntryProcessed = 1;  { green dot }
  mmIconBreakpoint = 2;      { stop sign }
  mmIconBreakpointGood = 3;  { stop sign + check }
  mmIconBreakpointBad = 4;   { stop sign + X }
  mmLineError = 10;          { maroon line highlight }
  mmLineBreakpointBad = 11;  { ugly olive line highlight }
  mmLineStep = 12;           { blue line highlight }
  mmIconStep = 13;           { blue arrow }
  mmIconBreakpointStep = 14; { blue arrow on top of a stop sign + check }

  { Memo indicator numbers - Note: inSquiggly and inPendingSquiggly are 0 and 1
    in ScintStylerInnoSetup and must be first and second here. Also note: even
    though inSquiggly and inPendingSquiggly are exclusive we still need 2 indicators
    (instead of 1 indicator with 2 values) because inPendingSquiggly is always
    hidden and in inSquiggly is not. }
  inSquiggly = INDIC_CONTAINER;
  inPendingSquiggly = INDIC_CONTAINER+1;
  inWordAtCursorOccurrence = INDIC_CONTAINER+2;
  inSelTextOccurrence = INDIC_CONTAINER+3;
  inMax = inSelTextOccurrence;

  { Just some invalid value used to indicate an unknown/uninitialized compiler FileIndex value }
  UnknownCompilerFileIndex = -2;

type
  TLineState = (lnUnknown, lnHasEntry, lnEntryProcessed);
  PLineStateArray = ^TLineStateArray;
  TLineStateArray = array[0..0] of TLineState;
  TSaveEncoding = (seAuto, seUTF8WithBOM, seUTF8WithoutBOM);
  TCompScintIndicatorNumber = 0..inMax;

  TCompScintEdit = class(TScintEdit)
  private
    FTheme: TTheme;
    FOpeningFile: Boolean;
    FUsed: Boolean; { The IDE only shows 1 memo at a time so can't use .Visible to check if a memo is used }
    FIndicatorCount: array[TCompScintIndicatorNumber] of Integer;
    FIndicatorHash: array[TCompScintIndicatorNumber] of String;
  protected
    procedure CreateWnd; override;
  public
    property Theme: TTheme read FTheme write FTheme;
    property OpeningFile: Boolean read FOpeningFile write FOpeningFile;
    property Used: Boolean read FUsed write FUsed;
    procedure UpdateIndicators(const Ranges: TScintRangeList;
      const IndicatorNumber: TCompScintIndicatorNumber);
    procedure UpdateMemoMarkerColumnWidth(const AWidth: Integer);
    procedure UpdateThemeColorsAndStyleAttributes;
  end;

  TCompScintFileEdit = class(TCompScintEdit)
  private
    FBreakPoints: TList<Integer>;
    FCompilerFileIndex: Integer;
    FFilename: String;
    FFileLastWriteTime: TFileTime;
    FSaveEncoding: TSaveEncoding;
  public
    ErrorLine, ErrorCaretPosition: Integer;
    StepLine: Integer;
    LineState: PLineStateArray;
    LineStateCapacity, LineStateCount: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BreakPoints: TList<Integer> read FBreakPoints;
    property Filename: String read FFileName write FFilename;
    property CompilerFileIndex: Integer read FCompilerFileIndex write FCompilerFileIndex;
    property FileLastWriteTime: TFileTime read FFileLastWriteTime write FFileLastWriteTime;
    property SaveEncoding: TSaveEncoding read FSaveEncoding write FSaveEncoding;
  end;

  TCompScintEditNavItem = record
    Memo: TCompScintEdit;
    Line, Column, VirtualSpace: Integer;
    constructor Create(const AMemo: TCompScintEdit);
    function EqualMemoAndLine(const ANavItem: TCompScintEditNavItem): Boolean;
    procedure Invalidate;
    function Valid: Boolean;
  end;

  { Not using TStack since it lacks a way the keep a maximum amount of items by discarding the oldest }
  TCompScintEditNavStack = class(TList<TCompScintEditNavItem>)
  public
    function LinesDeleted(const AMemo: TCompScintEdit; const FirstLine, LineCount: Integer): Boolean;
    procedure LinesInserted(const AMemo: TCompScintEdit; const FirstLine, LineCount: Integer);
    procedure Optimize;
    function RemoveMemo(const AMemo: TCompScintEdit): Boolean;
    function RemoveMemoBadLines(const AMemo: TCompScintEdit): Boolean;
  end;

  TCompScintEditNavStacks = class
  private
    FBackNavStack: TCompScintEditNavStack;
    FForwardNavStack: TCompScintEditNavStack;
  public
    constructor Create;
    destructor Destroy; override;
    function AddNewBackForJump(const OldNavItem, NewNavItem: TCompScintEditNavItem): Boolean;
    procedure Clear;
    procedure Limit;
    function LinesDeleted(const AMemo: TCompScintEdit; const FirstLine, LineCount: Integer): Boolean;
    procedure LinesInserted(const AMemo: TCompScintEdit; const FirstLine, LineCount: Integer);
    function RemoveMemo(const AMemo: TCompScintEdit): Boolean;
    function RemoveMemoBadLines(const AMemo: TCompScintEdit): Boolean;
    property Back: TCompScintEditNavStack read FBackNavStack;
    property Forward: TCompScintEditNavStack read FForwardNavStack;
  end;

implementation

uses
  MD5;
  
{ TCompScintEdit }

procedure TCompScintEdit.CreateWnd;
const
  SC_MARK_BACKFORE = 3030;  { new marker type added in Inno Setup's Scintilla build }
begin
  inherited;

  { Some notes about future Scintilla versions:
    -At some point SCI_SETVIRTUALSPACEOPTIONS will support SCVS_NOWRAPLINESTART.
     Once it does this should be used in TCompileForm.SyncEditorOptions if CursorPastEOL
     is on and our own VK_LEFT handling in TCompileForm.MemoKeyDown should be removed.
    -At some point the documentation will say:
     "The selection can be simplified down to just the main selection by
     SCI_CANCEL which is normally mapped to the Esc key."
     Once it does our own VK_ESCAPE handling in TCompileForm.FormKeyDown should be
     reviewed. Note that our handling does a two phase simplification like VSCode and
     not a one phase simplification like Notepad++.
    -At some point the documentation will say:
     "The INDICATOR_* values used for dividing up indicators were previously
      INDIC_CONTAINER, INDIC_IME, INDIC_IME_MAX, and INDIC_MAX"
     Once it does replace our use of these INDIC_* with INDICATOR_*.
    -3.5.2: Investigate: SCFIND_CXX11REGEX. When compiled with CXX11_REGEX this
            flag may be set to use <regex> instead of Scintilla's basic regular
            expressions.
    -3.5.7: Use SCI_MULTIPLESELECTADDEACH to implement Ctrl+Shift+L (Select All
            Occurrences) and SCI_MULTIPLESELECTADDNEXT to implement Ctrl+D (Select
            Next Occurrence). If the selection is empty Scintilla will use word
            searching so call SCI_SETSEARCHFLAGS first to turn on case match and
            whole word in that case, and turn it off otherwise. This way it
            behaves same as TCompileForm.UpdateOccurrenceIndicators. Also requires
            calling SCI_TARGETWHOLEDOCUMENT.
            !!! Note https://github.com/notepad-plus-plus/notepad-plus-plus/pull/14330
    -5.0.1: Review using SCI_INDICSETSTROKEWIDTH for high DPI support on INDIC_SQUIGGLE }

  Call(SCI_SETCARETWIDTH, 2, 0);
  Call(SCI_AUTOCSETAUTOHIDE, 0, 0);
  Call(SCI_AUTOCSETCANCELATSTART, 0, 0);
  Call(SCI_AUTOCSETDROPRESTOFWORD, 1, 0);
  Call(SCI_AUTOCSETIGNORECASE, 1, 0);
  Call(SCI_AUTOCSETMAXHEIGHT, 12, 0);
  Call(SCI_AUTOCSETMULTI, SC_MULTIAUTOC_EACH, 0);

  Call(SCI_SETMULTIPLESELECTION, 1, 0);
  Call(SCI_SETADDITIONALSELECTIONTYPING, 1, 0);
  Call(SCI_SETMULTIPASTE, SC_MULTIPASTE_EACH, 0);

  Call(SCI_ASSIGNCMDKEY, Ord('Z') or ((SCMOD_SHIFT or SCMOD_CTRL) shl 16), SCI_REDO);
  Call(SCI_ASSIGNCMDKEY, SCK_UP or (SCMOD_ALT shl 16), SCI_MOVESELECTEDLINESUP);
  Call(SCI_ASSIGNCMDKEY, SCK_DOWN or (SCMOD_ALT shl 16), SCI_MOVESELECTEDLINESDOWN);

  Call(SCI_SETSCROLLWIDTH, 1024 * CallStr(SCI_TEXTWIDTH, 0, 'X'), 0);

  Call(SCI_INDICSETSTYLE, inSquiggly, INDIC_SQUIGGLE); { Overwritten by TCompForm.SyncEditorOptions }
  Call(SCI_INDICSETFORE, inSquiggly, clRed); { May be overwritten by UpdateThemeColorsAndStyleAttributes }
  Call(SCI_INDICSETSTYLE, inPendingSquiggly, INDIC_HIDDEN);

  Call(SCI_INDICSETSTYLE, inWordAtCursorOccurrence, INDIC_STRAIGHTBOX);
  Call(SCI_INDICSETFORE, inWordAtCursorOccurrence, clSilver); { May be overwritten by UpdateThemeColorsAndStyleAttributes }
  Call(SCI_INDICSETALPHA, inWordAtCursorOccurrence, 255);
  Call(SCI_INDICSETOUTLINEALPHA, inWordAtCursorOccurrence, 255);
  Call(SCI_INDICSETUNDER, inWordAtCursorOccurrence, 1);

  Call(SCI_INDICSETSTYLE, inSelTextOccurrence, INDIC_STRAIGHTBOX);
  Call(SCI_INDICSETFORE, inSelTextOccurrence, clSilver); { May be overwritten by UpdateThemeColorsAndStyleAttributes }
  Call(SCI_INDICSETALPHA, inSelTextOccurrence, 255);
  Call(SCI_INDICSETOUTLINEALPHA, inSelTextOccurrence, 255);
  Call(SCI_INDICSETUNDER, inSelTextOccurrence, 1);

  { Set up the gutter column with line numbers - avoid Scintilla's 'reverse arrow'
    cursor which is not a standard Windows cursor so is just confusing, especially
    because the line numbers are clickable to select lines. This cursor will also
    be used at the small extra margin after the final column which also selects
    lines so setting the normal cursor also avoids a flashing cursor when moving
    between the editor and the breakpoint column. Note: width of the column is set
    up by TScintEdit.UpdateLineNumbersWidth. }
  Call(SCI_SETMARGINCURSORN, 0, SC_CURSORARROW);

  { Set up the gutter column with breakpoint etc symbols - note: column 0 is the
    line numbers column and its width is set up by TScintEdit.UpdateLineNumbersWidth }
  Call(SCI_SETMARGINTYPEN, 1, SC_MARGIN_SYMBOL);
  Call(SCI_SETMARGINSENSITIVEN, 1, 1); { Makes it react to mouse clicks }
  Call(SCI_SETMARGINCURSORN, 1, SC_CURSORARROW);

  { Set 2 pixel margin between gutter and the main text - note: the first
    parameter is unused so the value '0' doesn't mean anything below }
  Call(SCI_SETMARGINLEFT, 0, 2);

  Call(SCI_MARKERDEFINE, mmLineError, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineError, clWhite);
  Call(SCI_MARKERSETBACK, mmLineError, clMaroon);
  Call(SCI_MARKERDEFINE, mmLineBreakpointBad, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineBreakpointBad, clLime);
  Call(SCI_MARKERSETBACK, mmLineBreakpointBad, clOlive);
  Call(SCI_MARKERDEFINE, mmLineStep, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineStep, clWhite);
  Call(SCI_MARKERSETBACK, mmLineStep, clBlue); { May be overwritten by UpdateThemeColorsAndStyleAttributes }
end;

procedure TCompScintEdit.UpdateIndicators(const Ranges: TScintRangeList;
  const IndicatorNumber: TCompScintIndicatorNumber);

  function HashRanges(const Ranges: TScintRangeList): String;
  begin
    if Ranges.Count > 0 then begin
      var Context: TMD5Context;
      MD5Init(Context);
      for var Range in Ranges do
        MD5Update(Context, Range, SizeOf(Range));
      Result := MD5DigestToString(MD5Final(Context));
    end else
      Result := '';
  end;

begin
  var NewCount := Ranges.Count;
  var NewHash: String;
  var GotNewHash := False;

  var Update := NewCount <> FIndicatorCount[IndicatorNumber];
  if not Update and (NewCount <> 0) then begin
    NewHash := HashRanges(Ranges);
    GotNewHash := True;
    Update := NewHash <> FIndicatorHash[IndicatorNumber];
  end;

  if Update then begin
    Self.ClearIndicators(IndicatorNumber);
    for var Range in Ranges do
      Self.SetIndicators(Range.StartPos, Range.EndPos, IndicatorNumber, True);

    if not GotNewHash then
      NewHash := HashRanges(Ranges);

    FIndicatorCount[IndicatorNumber] := NewCount;
    FIndicatorHash[IndicatorNumber] := NewHash;
  end;
end;

procedure TCompScintEdit.UpdateMemoMarkerColumnWidth(const AWidth: Integer);
begin
  Call(SCI_SETMARGINWIDTHN, 1, AWidth);
end;

procedure TCompScintEdit.UpdateThemeColorsAndStyleAttributes;
begin
  if FTheme <> nil then begin
    Font.Color := FTheme.Colors[tcFore];
    Color := FTheme.Colors[tcBack];
    Call(SCI_SETSELBACK, 1, FTheme.Colors[tcSelBack]);
    Call(SCI_INDICSETFORE, inSquiggly, FTheme.Colors[tcRed]);
    Call(SCI_INDICSETFORE, inWordAtCursorOccurrence, FTheme.Colors[tcWordAtCursorOccurrenceBack]);
    Call(SCI_INDICSETFORE, inSelTextOccurrence, FTheme.Colors[tcSelTextOccurrenceBack]);
    Call(SCI_MARKERSETBACK, mmLineStep, FTheme.Colors[tcBlue]);
  end;
  UpdateStyleAttributes;
end;

{ TCompScintFileEdit }

constructor TCompScintFileEdit.Create;
begin
  inherited;
  FBreakPoints := TList<Integer>.Create;
end;

destructor TCompScintFileEdit.Destroy;
begin
  FBreakPoints.Free;
  inherited;
end;

{ TCompScintEditNavItem }

constructor TCompScintEditNavItem.Create(const AMemo: TCompScintEdit);
begin
  Memo := AMemo;
  Line := AMemo.CaretLine;
  Column := AMemo.CaretColumn;
  VirtualSpace := AMemo.CaretVirtualSpace;
end;

function TCompScintEditNavItem.EqualMemoAndLine(
  const ANavItem: TCompScintEditNavItem): Boolean;
begin
  Result := (Memo = ANavItem.Memo) and (Line = ANavItem.Line);
end;

procedure TCompScintEditNavItem.Invalidate;
begin
  Memo := nil;
end;

function TCompScintEditNavItem.Valid: Boolean;
begin
  Result := (Memo <> nil) and (Line < Memo.Lines.Count); { Line check: see MemoLinesDeleted and RemoveMemoBadLinesFromNav }
end;

{ TCompScintEditNavStack }

function TCompScintEditNavStack.LinesDeleted(const AMemo: TCompScintEdit;
  const FirstLine, LineCount: Integer): Boolean;
begin
  Result := False;
  for var I := Count-1 downto 0 do begin
    var NavItem := Items[I];
    if NavItem.Memo = AMemo then begin
      var Line := NavItem.Line;
      if Line >= FirstLine then begin
        if Line < FirstLine + LineCount then begin
          Delete(I);
          Result := True;
        end else begin
          NavItem.Line := Line - LineCount;
          Items[I] := NavItem;
        end;
      end;
    end;
  end;
  if Result then
    Optimize;
end;

procedure TCompScintEditNavStack.LinesInserted(const AMemo: TCompScintEdit;
  const FirstLine, LineCount: Integer);
begin
  for var I := 0 to Count-1 do begin
    var NavItem := Items[I];
    if NavItem.Memo = AMemo then begin
      var Line := NavItem.Line;
      if Line >= FirstLine then begin
        NavItem.Line := Line + LineCount;
        Items[I] := NavItem;
      end;
    end;
  end;
end;

procedure TCompScintEditNavStack.Optimize;
begin
  { Turn two entries for the same memo and line which are next to each other
    into one entry, ignoring column differences (like Visual Studio 2022)
    Note: doesn't yet look at CompForm's FCurrentNavItem to see if a stack's top
    item is the same so it doesnt optimize that situation atm }
  for var I := Count-1 downto 1 do
    if Items[I].EqualMemoAndLine(Items[I-1]) then
      Delete(I);
end;

function TCompScintEditNavStack.RemoveMemo(
  const AMemo: TCompScintEdit): Boolean;
begin
  Result := False;
  for var I := Count-1 downto 0 do begin
    if Items[I].Memo = AMemo then begin
      Delete(I);
      Result := True;
    end;
  end;
  if Result then
    Optimize;
end;

function TCompScintEditNavStack.RemoveMemoBadLines(
  const AMemo: TCompScintEdit): Boolean;
begin
  Result := False;
  var LastGoodLine := AMemo.Lines.Count-1;
  for var I := Count-1 downto 0 do begin
    if (Items[I].Memo = AMemo) and (Items[I].Line > LastGoodLine) then begin
      Delete(I);
      Result := True;
    end;
  end;
  if Result then
    Optimize;
end;

{ TCompScintEditNavStacks }

constructor TCompScintEditNavStacks.Create;
begin
  inherited;
  FBackNavStack := TCompScintEditNavStack.Create;
  FForwardNavStack := TCompScintEditNavStack.Create;
end;

destructor TCompScintEditNavStacks.Destroy;
begin
  FForwardNavStack.Free;
  FBackNavStack.Free;
  inherited;
end;

function TCompScintEditNavStacks.AddNewBackForJump(const OldNavItem,
  NewNavItem: TCompScintEditNavItem): Boolean;
begin
  { Want a new item when changing tabs or moving at least 11 lines at once,
    similar to Visual Studio 2022, see:
    https://learn.microsoft.com/en-us/archive/blogs/zainnab/navigate-backward-and-navigate-forward
    Note: not doing the other stuff listed in the article atm }
  Result := (OldNavItem.Memo <> NewNavItem.Memo) or
            (Abs(OldNavItem.Line - NewNavItem.Line) >= 11);
  if Result then begin
    FBackNavStack.Add(OldNavItem);
    Limit;
  end;
end;

procedure TCompScintEditNavStacks.Clear;
begin
  FBackNavStack.Clear;
  FForwardNavStack.Clear;
end;

procedure TCompScintEditNavStacks.Limit;
begin
  { The dropdown showing both stacks + the current nav item should show at most
    16 items just like Visual Studio 2022 }
  if FBackNavStack.Count + FForwardNavStack.Count >= 15 then
    FBackNavStack.Delete(0);
end;

function TCompScintEditNavStacks.LinesDeleted(const AMemo: TCompScintEdit;
  const FirstLine, LineCount: Integer): Boolean;
begin
  Result := FBackNavStack.LinesDeleted(AMemo, FirstLine, LineCount);
  Result := FForwardNavStack.LinesDeleted(AMemo, FirstLine, LineCount) or Result;
end;

procedure TCompScintEditNavStacks.LinesInserted(const AMemo: TCompScintEdit;
  const FirstLine, LineCount: Integer);
begin
  FBackNavStack.LinesInserted(AMemo, FirstLine, LineCount);
  FForwardNavStack.LinesInserted(AMemo, FirstLine, LineCount);
end;

function TCompScintEditNavStacks.RemoveMemo(
  const AMemo: TCompScintEdit): Boolean;
begin
  Result := FBackNavStack.RemoveMemo(AMemo);
  Result := FForwardNavStack.RemoveMemo(AMemo) or Result;
end;

function TCompScintEditNavStacks.RemoveMemoBadLines(
  const AMemo: TCompScintEdit): Boolean;
begin
  Result := FBackNavStack.RemoveMemoBadLines(AMemo);
  Result := FForwardNavStack.RemoveMemoBadLines(AMemo) or Result;
end;

end.