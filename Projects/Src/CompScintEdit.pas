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
  Windows, Graphics, Classes, Menus, Generics.Collections,
  ScintInt, ScintEdit, ModernColors;

const
  { Memo margin numbers }
  mmLineNumbers = 0;
  mmIcons = 1;
  mmChangeHistory = 2;
  mmFolding = 3;

  { Memo icon and line marker numbers }
  mimHasEntry = 0;        { grey dot }
  mimEntryProcessed = 1;  { green dot }
  mimBreakpoint = 2;      { stop sign }
  mimBreakpointGood = 3;  { stop sign + check }
  mimBreakpointBad = 4;   { stop sign + X }
  mimStep = 5;            { blue arrow }
  mimBreakpointStep = 6;  { blue arrow on top of a stop sign + check }
  mimMask = $7F;

  mlmError = 10;          { maroon line highlight }
  mlmBreakpointBad = 11;  { ugly olive line highlight }
  mlmStep = 12;           { blue line highlight }

  { Memo indicator numbers - Note: inSquiggly and inPendingSquiggly are 0 and 1
    in ScintStylerInnoSetup and must be first and second here. Also note: even
    though inSquiggly and inPendingSquiggly are exclusive we still need 2 indicators
    (instead of 1 indicator with 2 values) because inPendingSquiggly is always
    hidden and in inSquiggly is not. }
  minSquiggly = INDICATOR_CONTAINER;
  minPendingSquiggly = INDICATOR_CONTAINER+1;
  minWordAtCursorOccurrence = INDICATOR_CONTAINER+2;
  minSelTextOccurrence = INDICATOR_CONTAINER+3;
  minMax = minSelTextOccurrence;

  { Just some invalid value used to indicate an unknown/uninitialized compiler FileIndex value }
  UnknownCompilerFileIndex = -2;

type
  TLineState = (lnUnknown, lnHasEntry, lnEntryProcessed); { Not related to TScintLineState }
  PLineStateArray = ^TLineStateArray;
  TLineStateArray = array[0..0] of TLineState;
  TSaveEncoding = (seAuto, seUTF8WithBOM, seUTF8WithoutBOM);
  TCompScintIndicatorNumber = 0..minMax;

   { Keymaps - Note: Scintilla's default keymap is the same or at least nearly
     the same as Visual Studio's }
  TCompScintKeyMappingType = (kmtDefault, kmtVSCode);

   { Commands which require more than 1 parameterless SCI_XXXXX and need help
     from the container }
  TCompScintComplexCommand = (ccNone, ccSelectNextOccurrence,
    ccSelectAllOccurrences, ccSelectAllFindMatches, ccSimplifySelection,
    ccUnfoldLine, ccFoldLine);

  TCompScintEdit = class(TScintEdit)
  private
    type
      TCompScintComplexCommands = TDictionary<TShortCut, TCompScintComplexCommand>;
      TCompScintComplexCommandsReversed = TDictionary<TCompScintComplexCommand, TShortCut>;
    var
      FKeyMappingType: TCompScintKeyMappingType;
      FComplexCommands: TCompScintComplexCommands;
      FComplexCommandsReversed: TCompScintComplexCommandsReversed;
      FUseFolding: Boolean;
      FTheme: TTheme;
      FOpeningFile: Boolean;
      FUsed: Boolean; { The IDE only shows 1 memo at a time so can't use .Visible to check if a memo is used }
      FIndicatorCount: array[TCompScintIndicatorNumber] of Integer;
      FIndicatorHash: array[TCompScintIndicatorNumber] of String;
      procedure AddComplexCommand(const ShortCut: TShortCut;
        Command: TCompScintComplexCommand);
      procedure SetUseFolding(const Value: Boolean);
      procedure SetKeyMappingType(const Value: TCompScintKeyMappingType);
      procedure UpdateComplexCommands;
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Theme: TTheme read FTheme write FTheme;
    property OpeningFile: Boolean read FOpeningFile write FOpeningFile;
    property Used: Boolean read FUsed write FUsed;
    function GetComplexCommand(const ShortCut: TShortCut): TCompScintComplexCommand;
    function GetComplexCommandShortCut(const Command: TCompScintComplexCommand): TShortCut;
    function GetRectExtendShiftState(const Desired: Boolean): TShiftState;
    procedure UpdateIndicators(const Ranges: TScintRangeList;
      const IndicatorNumber: TCompScintIndicatorNumber);
    procedure UpdateMarginsAndSquigglyWidths(const IconMarkersWidth,
      BaseChangeHistoryWidth, BaseFolderMarkersWidth, LeftBlankMarginWidth,
      RightBlankMarginWidth, SquigglyWidth: Integer);
    procedure UpdateThemeColorsAndStyleAttributes;
  published
    property KeyMappingType: TCompScintKeyMappingType read FKeyMappingType write SetKeyMappingType default kmtDefault;
    property UseFolding: Boolean read FUseFolding write SetUseFolding default True;
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
  SysUtils, MD5, IsscintInt;
  
{ TCompScintEdit }

constructor TCompScintEdit.Create(AOwner: TComponent);
begin
  inherited;

  FComplexCommands := TCompScintComplexCommands.Create;
  FComplexCommandsReversed := TCompScintComplexCommandsReversed.Create;

  FKeyMappingType := kmtDefault;
  UpdateComplexCommands;
  FUseFolding := True;
end;

destructor TCompScintEdit.Destroy;
begin
  FComplexCommandsReversed.Free;
  FComplexCommands.Free;
  
  inherited;
end;

procedure TCompScintEdit.CreateWnd;
begin
  inherited;

  { Some notes about future Scintilla versions:
    -Does it at some point become possible to change mouse shortcut Ctrl+Click
     to Alt+Click? And Alt+Shift+Drag instead of Alt+Drag for rect select?
    -What about using Calltips and SCN_DWELLSTART to show variable evalutions?
    -3.6.6: Investigate SCFIND_CXX11REGEX: C++ 11 <regex> support built by default.
            Can be disabled by defining NO_CXX11_REGEX. Good (?) overview at:
            https://cplusplus.com/reference/regex/ECMAScript/
    -5.2.3: "Applications should move to SCI_GETTEXTRANGEFULL, SCI_FINDTEXTFULL,
            and SCI_FORMATRANGEFULL from their predecessors as they will be
            deprecated." So our use of SCI_GETTEXTRANGE and SCI_FORMATRANGE needs
            to be updated but that also means we should do many more changes to
            replace all the Integer positions with a 'TScintPosition = type
            NativeInt'. Does not actually change anything until there's a
            64-bit build...
            Later SCI_GETSTYLEDTEXTFULL was also added but we don't use it at
            the time of writing. }

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

  AssignCmdKey('C', [ssCtrl], SCI_COPYALLOWLINE);
  AssignCmdKey(SCK_INSERT, [ssCtrl], SCI_COPYALLOWLINE);
  AssignCmdKey('X', [ssCtrl], SCI_CUTALLOWLINE);
  AssignCmdKey(SCK_DELETE, [ssShift], SCI_CUTALLOWLINE);
  AssignCmdKey('Z', [ssShift, ssCtrl], SCI_REDO);
  AssignCmdKey(SCK_UP, [ssAlt], SCI_MOVESELECTEDLINESUP);
  AssignCmdKey(SCK_DOWN, [ssAlt], SCI_MOVESELECTEDLINESDOWN);

  Call(SCI_SETSCROLLWIDTH, 1024 * CallStr(SCI_TEXTWIDTH, 0, 'X'), 0);

  Call(SCI_INDICSETSTYLE, minSquiggly, INDIC_SQUIGGLE); { Overwritten by TCompForm.SyncEditorOptions }
  Call(SCI_INDICSETFORE, minSquiggly, clRed); { May be overwritten by UpdateThemeColorsAndStyleAttributes }
  Call(SCI_INDICSETSTYLE, minPendingSquiggly, INDIC_HIDDEN);

  Call(SCI_INDICSETSTYLE, minWordAtCursorOccurrence, INDIC_STRAIGHTBOX);
  Call(SCI_INDICSETFORE, minWordAtCursorOccurrence, clSilver); { May be overwritten by UpdateThemeColorsAndStyleAttributes }
  Call(SCI_INDICSETALPHA, minWordAtCursorOccurrence, SC_ALPHA_OPAQUE);
  Call(SCI_INDICSETOUTLINEALPHA, minWordAtCursorOccurrence, SC_ALPHA_OPAQUE);
  Call(SCI_INDICSETUNDER, minWordAtCursorOccurrence, 1);

  Call(SCI_INDICSETSTYLE, minSelTextOccurrence, INDIC_STRAIGHTBOX);
  Call(SCI_INDICSETFORE, minSelTextOccurrence, clSilver); { May be overwritten by UpdateThemeColorsAndStyleAttributes }
  Call(SCI_INDICSETALPHA, minSelTextOccurrence, SC_ALPHA_OPAQUE);
  Call(SCI_INDICSETOUTLINEALPHA, minSelTextOccurrence, SC_ALPHA_OPAQUE);
  Call(SCI_INDICSETUNDER, minSelTextOccurrence, 1);

  { Set up the gutter column with line numbers - avoid Scintilla's 'reverse arrow'
    cursor which is not a standard Windows cursor so is just confusing, especially
    because the line numbers are clickable to select lines. Note: width of the
    column is set up for us by TScintEdit.UpdateLineNumbersWidth. }
  Call(SCI_SETMARGINCURSORN, mmLineNumbers, SC_CURSORARROW);

  { Set up the gutter column with breakpoint etc symbols }
  Call(SCI_SETMARGINTYPEN, mmIcons, SC_MARGIN_SYMBOL);
  Call(SCI_SETMARGINMASKN, mmIcons, mimMask);
  Call(SCI_SETMARGINSENSITIVEN, mmIcons, 1); { Makes it send SCN_MARGIN(RIGHT)CLICK instead of selecting lines }
  Call(SCI_SETMARGINCURSORN, mmIcons, SC_CURSORARROW);

  { Set up the gutter column with change history. Note: width of the column is
    set up by UpdateMarginsAndSquigglyWidths. Also see
    https://scintilla.org/ChangeHistory.html }
  Call(SCI_SETMARGINTYPEN, mmChangeHistory, SC_MARGIN_SYMBOL);
  Call(SCI_SETMARGINMASKN, mmChangeHistory, SC_MASK_HISTORY);
  Call(SCI_SETMARGINCURSORN, mmChangeHistory, SC_CURSORARROW);

  { Set up the gutter column with folding markers. Note: width of the column is
    set up by UpdateMarginsAndSquigglyWidths. }
  Call(SCI_SETMARGINTYPEN, mmFolding, SC_MARGIN_SYMBOL);
  Call(SCI_SETMARGINMASKN, mmFolding, LPARAM(SC_MASK_FOLDERS));
  Call(SCI_SETMARGINCURSORN, mmFolding, SC_CURSORARROW);
  Call(SCI_SETMARGINSENSITIVEN, mmFolding, 1);
  Call(SCI_SETAUTOMATICFOLD, SC_AUTOMATICFOLD_SHOW or SC_AUTOMATICFOLD_CLICK or SC_AUTOMATICFOLD_CHANGE, 0);
  Call(SCI_MARKERDEFINE, SC_MARKNUM_FOLDEROPEN, SC_MARK_ARROWDOWN);
  Call(SCI_MARKERDEFINE, SC_MARKNUM_FOLDER, SC_MARK_ARROW);
  Call(SCI_MARKERDEFINE, SC_MARKNUM_FOLDERSUB, SC_MARK_EMPTY);
  Call(SCI_MARKERDEFINE, SC_MARKNUM_FOLDERTAIL, SC_MARK_EMPTY);
  Call(SCI_MARKERDEFINE, SC_MARKNUM_FOLDEREND, SC_MARK_EMPTY);
  Call(SCI_MARKERDEFINE, SC_MARKNUM_FOLDEROPENMID, SC_MARK_EMPTY);
  Call(SCI_MARKERDEFINE, SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_EMPTY);
  FoldFlags := [{sffLevelNumbers, }sffLineAfterContracted]; { sffLevelNumbers can be used to debug fold levels}

  { Set up the line markers }
  Call(SCI_MARKERDEFINE, mlmError, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mlmError, clWhite);
  Call(SCI_MARKERSETBACK, mlmError, clMaroon);
  Call(SCI_MARKERDEFINE, mlmBreakpointBad, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mlmBreakpointBad, clLime);
  Call(SCI_MARKERSETBACK, mlmBreakpointBad, clOlive);
  Call(SCI_MARKERDEFINE, mlmStep, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mlmStep, clWhite);
  Call(SCI_MARKERSETBACK, mlmStep, clBlue); { May be overwritten by UpdateThemeColorsAndStyleAttributes }
end;

procedure TCompScintEdit.AddComplexCommand(const ShortCut: TShortCut;
  Command: TCompScintComplexCommand);
begin
  if Command = ccNone then
    raise Exception.Create('Command = ccNone');
  FComplexCommands.Add(ShortCut, Command);
  FComplexCommandsReversed.Add(Command, ShortCut);
end;

function TCompScintEdit.GetComplexCommand(
  const ShortCut: TShortCut): TCompScintComplexCommand;
begin
  if not FComplexCommands.TryGetValue(ShortCut, Result) then
    Result := ccNone;
end;

function TCompScintEdit.GetComplexCommandShortCut(
  const Command: TCompScintComplexCommand): TShortCut;
begin
  Result := FComplexCommandsReversed[Command];
end;

function TCompScintEdit.GetRectExtendShiftState(
  const Desired: Boolean): TShiftState;
begin
  Result := [ssShift, ssAlt];
  if ((FKeyMappingType = kmtVSCode) and Desired) or
     ((FKeyMappingType <> kmtVSCode) and not Desired) then
    Include(Result, ssCtrl);
end;

procedure TCompScintEdit.SetKeyMappingType(
  const Value: TCompScintKeyMappingType);
begin
  if FKeyMappingType <> Value then begin
    FKeyMappingType := Value;
    Call(SCI_RESETALLCMDKEYS, Ord(FKeyMappingType = kmtVSCode), 0);
    Call(SCI_SETMOUSEMAPPING, Ord(FKeyMappingType = kmtVSCode), 0);
    UpdateComplexCommands;
  end;
end;

procedure TCompScintEdit.UpdateComplexCommands;
begin
  FComplexCommands.Clear;
  FComplexCommandsReversed.Clear;

  if FKeyMappingType = kmtVSCode then begin
    { Use freed Ctrl+D and Ctrl+Shift+L }
    AddComplexCommand(ShortCut(KeyToKeyCode('D'), [ssCtrl]), ccSelectNextOccurrence);
    AddComplexCommand(ShortCut(KeyToKeyCode('L'), [ssShift, ssCtrl]), ccSelectAllOccurrences);
  end else begin
    AddComplexCommand(ShortCut(VK_OEM_PERIOD, [ssShift, ssAlt]), ccSelectNextOccurrence);
    AddComplexCommand(ShortCut(VK_OEM_1, [ssShift, ssAlt]), ccSelectAllOccurrences);
  end;

  AddComplexCommand(ShortCut(VK_RETURN, [ssAlt]), ccSelectAllFindMatches);
  AddComplexCommand(ShortCut(VK_ESCAPE, []), ccSimplifySelection);
  AddComplexCommand(ShortCut(VK_OEM_6, [ssShift, ssCtrl]), ccUnfoldLine);
  AddComplexCommand(ShortCut(VK_OEM_4, [ssShift, ssCtrl]), ccFoldLine);
end;

procedure TCompScintEdit.SetUseFolding(const Value: Boolean);
begin
  if FUseFolding <> Value then begin
    FUseFolding := Value;
    { If FUseFolding is True then caller must set the margin width using
      UpdateMarginsAndSquigglyWidths else we set it to 0 now }
    if not FUseFolding then begin
      Call(SCI_FOLDALL, SC_FOLDACTION_EXPAND, 0);
      Call(SCI_SETMARGINWIDTHN, 3, 0);
    end;
  end;
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

procedure TCompScintEdit.UpdateMarginsAndSquigglyWidths(const IconMarkersWidth,
  BaseChangeHistoryWidth, BaseFolderMarkersWidth, LeftBlankMarginWidth,
  RightBlankMarginWidth, SquigglyWidth: Integer);
begin
  Call(SCI_SETMARGINWIDTHN, mmIcons, IconMarkersWidth);

  var ChangeHistoryWidth: Integer;
  if ChangeHistory <> schDisabled then
    ChangeHistoryWidth := BaseChangeHistoryWidth
  else
    ChangeHistoryWidth := 0; { Current this is just the preprocessor output memo }
  Call(SCI_SETMARGINWIDTHN, mmChangeHistory, ChangeHistoryWidth);

  var FolderMarkersWidth: Integer;
  if FUseFolding then
    FolderMarkersWidth := BaseFolderMarkersWidth
  else
    FolderMarkersWidth := 0;
  Call(SCI_SETMARGINWIDTHN, mmFolding, FolderMarkersWidth);

  { Note: the first parameter is unused so the value '0' doesn't mean anything below }
  Call(SCI_SETMARGINLEFT, 0, LeftBlankMarginWidth);
  Call(SCI_SETMARGINRIGHT, 0, RightBlankMarginWidth);

  Call(SCI_INDICSETSTROKEWIDTH, minSquiggly, SquigglyWidth);
end;

procedure TCompScintEdit.UpdateThemeColorsAndStyleAttributes;
begin
  if FTheme <> nil then begin { Always True at the moment }
    Font.Color := FTheme.Colors[tcFore];
    Color := FTheme.Colors[tcBack];

    var SelBackColor := FTheme.Colors[tcSelBack];
    Call(SCI_SETELEMENTCOLOUR, SC_ELEMENT_SELECTION_BACK, SelBackColor);
    Call(SCI_SETELEMENTCOLOUR, SC_ELEMENT_SELECTION_ADDITIONAL_BACK, SelBackColor);
    Call(SCI_SETELEMENTCOLOUR, SC_ELEMENT_SELECTION_SECONDARY_BACK, SelBackColor);
    Call(SCI_SETELEMENTCOLOUR, SC_ELEMENT_SELECTION_INACTIVE_BACK, SelBackColor);
    Call(SCI_SETELEMENTCOLOUR, SC_ELEMENT_SELECTION_INACTIVE_ADDITIONAL_BACK, SelBackColor);

    Call(SCI_SETELEMENTCOLOUR, SC_ELEMENT_FOLD_LINE, FTheme.Colors[tcIndentGuideFore] or (70 shl 24));
    Call(SCI_SETFOLDMARGINCOLOUR, Ord(True), FTheme.Colors[tcBack]);
    Call(SCI_SETFOLDMARGINHICOLOUR, Ord(True), FTheme.Colors[tcBack]);

    Call(SCI_INDICSETFORE, minSquiggly, FTheme.Colors[tcRed]);
    Call(SCI_INDICSETFORE, minWordAtCursorOccurrence, FTheme.Colors[tcWordAtCursorOccurrenceBack]);
    Call(SCI_INDICSETFORE, minSelTextOccurrence, FTheme.Colors[tcSelTextOccurrenceBack]);

    Call(SCI_MARKERSETBACK, mlmStep, FTheme.Colors[tcBlue]);
    
    Call(SCI_MARKERSETFORE, SC_MARKNUM_HISTORY_REVERTED_TO_ORIGIN, FTheme.Colors[tcBlue]); { To reproduce: open a file, press enter, save, undo }
    Call(SCI_MARKERSETBACK, SC_MARKNUM_HISTORY_REVERTED_TO_ORIGIN, FTheme.Colors[tcBlue]);
    Call(SCI_MARKERSETFORE, SC_MARKNUM_HISTORY_SAVED, FTheme.Colors[tcGreen]);
    Call(SCI_MARKERSETBACK, SC_MARKNUM_HISTORY_SAVED, FTheme.Colors[tcGreen]);
    Call(SCI_MARKERSETFORE, SC_MARKNUM_HISTORY_MODIFIED, FTheme.Colors[tcReallyOrange]);
    Call(SCI_MARKERSETBACK, SC_MARKNUM_HISTORY_MODIFIED, FTheme.Colors[tcReallyOrange]);
    Call(SCI_MARKERSETFORE, SC_MARKNUM_HISTORY_REVERTED_TO_MODIFIED, FTheme.Colors[tcTeal]); { To reproduce: open a file, press space, press backspace, save, press enter, save, undo }
    Call(SCI_MARKERSETBACK, SC_MARKNUM_HISTORY_REVERTED_TO_MODIFIED, FTheme.Colors[tcTeal]);
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