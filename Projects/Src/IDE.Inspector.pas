unit IDE.Inspector;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TJvInspector wrapper, attached to a TLiveScriptObjectFactory, following
  the caret, creating new live objects for it, showing them in the inspector,
  and forwarding edits from it to the factory.
}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, StdCtrls, Generics.Collections,
  JvInspector, ModernColors, NewStaticText, ScintEdit,
  IDE.LiveScriptObjectFactory, IDE.ScriptModel, IDE.ScriptModel.Metadata, IDE.ScriptModel.Metadata.Extra;

type
  TInspectorRowKind = (irkParameter, irkParameterFlag, irkKey,
    irkKeyFlag {$IFDEF DEBUG}, irkDebugStatus, irkDebugSections, irkDebugEarlyExits,
    irkDebugCaretAt {$ENDIF});

  TInspectorRow = record
    Kind: TInspectorRowKind;
    Name: String;            { The parameter or key name }
    Index: Integer;          { The parameter index, or the line index for a
                               key, or -1 if known but not present in the
                               script }
    FlagName: String;        { irkParameterFlag and irkKeyFlag }
    LastValueSignature: String;
    CheckBox: Boolean;
  end;

  TCaretAtKind = (cakParameterSectionEntry, cakKeyValueSection);

  TCaretAt = record
    Valid: Boolean;
    Kind: TCaretAtKind;
    Name: String;   { Protects against a stale Index. As in the script, so not cleaned. }
    Index: Integer; { Protects against duplicated Name. -1 if multi-entry editing. }
  end;

  TInspectorGetBaseDirEvent = function: String of object;

  TInspectorGetSignToolsEvent = function: TStringList of object;

  TInspectorGetMainFactoryEvent = function: TLiveScriptObjectFactory of object;

  TInspector = class
  private
    class var
      FScriptBrowseFileTypeFilters: array [TScriptBrowseFileType] of record
        FilesName: String;
        Extensions: TArray<String>; { First is default }
      end;
    var
      FJvInspector: TJvInspector;
      FMessagesWnd: HWND;
      FNoteText: TNewStaticText;
      FFactory: TLiveScriptObjectFactory;
      FOnGetBaseDir: TInspectorGetBaseDirEvent;
      FOnGetSignTools: TInspectorGetSignToolsEvent;
      FOnGetMainFactory: TInspectorGetMainFactoryEvent;
      FCategoryNamesInDisplayOrder: TArray<String>;
      FLiveParameterSectionEntries: TLiveScriptParameterSectionEntries;
      FLiveKeyValueSection: TLiveScriptKeyValueSection;
      FLiveKeyValueSectionName: String;
      FLiveKeyValueSectionIsDirectiveSection: Boolean;
      FLiveKeyValueSectionHasSiblingOccurrences: Boolean;
      FLiveKeyValueSectionIndex: Integer; { Factory section index it was created for }
      FChangeCountAtCreation: Int64; { Factory ChangeCount at the live object's creation }
      FSelectionLineRangesAtCreation: TArray<TScintLineRange>; { GetSelectionLineRanges result at the live object's creation }
      FIndividualSelectionLineRangesAtCreation: TArray<TScintLineRange>; { Same but the individual line ranges }
      FCaretLineAtCreation: Integer; { Memo CaretLine at the live object's creation }
      FMixedSelection: Boolean;
      FRows: TList<TInspectorRow>;
      FRowSetSignature: String;
      FFollowCaret: Boolean;
      FCaretAt: TCaretAt;
      {$IFDEF DEBUG}
      FDebugStatusRowString: String;
      FUpdateFromCaretEarlyExitCount: Integer;
      {$ENDIF}
      FInEdit: Boolean;
      FFilterText: String;
      FShowAllKnownDirectives: Boolean;
      FShowAllKnownDirectivesSuppressedNote: Boolean;
      FQuoteNewParameterValues: Boolean;
      FQuoteNewDirectiveValues: Boolean;
    class constructor Create;
    class procedure SortValueList(var AValues: TArray<String>); static;
    procedure InvalidateChangedRows;
    function TryGetRow(const AItem: TJvCustomInspectorItem;
      out ARow: TInspectorRow): Boolean;
    function RowResolves(const ARow: TInspectorRow): Boolean;
    function SelectedRowResolves: Boolean;
    function GetSelectedRowValuePositions(
      const AMaxCount: Integer = 0): TArray<TValuePosition>;
    function GetRowValueSignature(const ARow: TInspectorRow): String;
    function RowGetAsOrdinal(const ARow: TInspectorRow): Int64; overload;
    procedure RowGetAsOrdinal(Sender: TJvCustomInspectorItem; var Value: Int64); overload;
    function RowGetAsString(const ARow: TInspectorRow): String; overload;
    procedure RowGetAsString(Sender: TJvCustomInspectorItem; var Value: String); overload;
    procedure RowSetAsOrdinal(Sender: TJvCustomInspectorItem; var Value: Int64);
    procedure RowSetAsString(Sender: TJvCustomInspectorItem; var Value: String);
    procedure RowRemove(const ARow: TInspectorRow);
    procedure ChoiceRowGetValueList(Item: TJvCustomInspectorItem; Values: TStrings);
    procedure DestDirRowGetValueList(Item: TJvCustomInspectorItem; Values: TStrings);
    procedure PermissionsRowGetValueList(Item: TJvCustomInspectorItem; Values: TStrings);
    procedure SignToolRowGetValueList(Item: TJvCustomInspectorItem; Values: TStrings);
    procedure ScriptValuesRowGetValueList(Item: TJvCustomInspectorItem; Values: TStrings);
    function ItemShouldBeBold(const AItem: TJvCustomInspectorItem): Boolean;
    procedure JvInspectorCustomizeItemCanvas(Item: TJvCustomInspectorItem;
      Canvas: TCanvas);
    procedure JvInspectorBeforeEdit(Sender: TObject;
      Item: TJvCustomInspectorItem; Edit: TEdit);
    procedure JvInspectorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure JvInspectorLeafNameDblClick(Item: TJvCustomInspectorItem);
    procedure JvInspectorEditButtonClick(Item: TJvCustomInspectorItem;
      var Value: String);
    procedure MessagesWndProc(var Message: TMessage);
    function RowMatchesCaretAt(const ARow: TInspectorRow): Boolean;
    procedure ApplyCaretAtTimerUpdate(const ACancel: Boolean);
    procedure ApplyCaretAt;
    function GetDividerWidth: Integer;
    procedure SetDividerWidth(const Value: Integer);
    procedure SetFilterText(const Value: String);
    procedure SetFollowCaret(const Value: Boolean);
    procedure SetQuoteNewDirectiveValues(const Value: Boolean);
    procedure SetQuoteNewParameterValues(const Value: Boolean);
    procedure SetShowAllKnownDirectives(const Value: Boolean);
    procedure SetShowAllKnownDirectivesSuppressedNote(const Value: Boolean);
    procedure UpdateNote;
  public
    constructor Create(const AJvInspector: TJvInspector;
      const ANoteText: TNewStaticText;
      const AFactory: TLiveScriptObjectFactory;
      const AShowAllKnownDirectives, AFollowCaret: Boolean;
      const AOnGetBaseDir: TInspectorGetBaseDirEvent;
      const AOnGetSignTools: TInspectorGetSignToolsEvent;
      const AOnGetMainFactory: TInspectorGetMainFactoryEvent);
    destructor Destroy; override;
    procedure ForceFinishEdit(const AForceCancel: Boolean = False);
    function GetSelectedHelpKeyword: String;
    function CanGoToSelectedRow: Boolean;
    function GoToSelectedRow: Boolean;
    function CanRemoveSelectedRow: Boolean;
    procedure RemoveSelectedRow;
    function ShowingDirectiveSection: Boolean;
    function ShowingParameterSectionEntry: Boolean;
    procedure SetActiveFactory(const AFactory: TLiveScriptObjectFactory;
      const AShowAllKnownDirectives, AShowAllKnownDirectivesSuppressedNote: Boolean);
    procedure UpdateFromCaret;
    procedure UpdateReadOnly;
    procedure UpdateTheme(const ATheme: TTheme; const AHighContrastActive: Boolean);
    property FilterText: String read FFilterText write SetFilterText;
    property FollowCaret: Boolean read FFollowCaret write SetFollowCaret;
    property ShowAllKnownDirectives: Boolean read FShowAllKnownDirectives
      write SetShowAllKnownDirectives;
    property ShowAllKnownDirectivesSuppressedNote: Boolean
      read FShowAllKnownDirectivesSuppressedNote
      write SetShowAllKnownDirectivesSuppressedNote;
    { These only apply to text values }
    property QuoteNewParameterValues: Boolean read FQuoteNewParameterValues
      write SetQuoteNewParameterValues;
    property QuoteNewDirectiveValues: Boolean read FQuoteNewDirectiveValues
      write SetQuoteNewDirectiveValues;
    property JvInspector: TJvInspector read FJvInspector;
    property DividerWidth: Integer read GetDividerWidth write SetDividerWidth;
  end;

implementation

uses
  SysUtils, UITypes, Themes, Forms, Generics.Defaults,
  BrowseFunc, NewUxTheme, PathFunc,
  Shared.CommonFunc, Shared.CommonFunc.Vcl,
  IDE.HelperFunc, IDE.Messages, IDE.LocalizeFunc;

type
  EInspectorValueRejected = class(EScriptModelError);

const
  WM_RemoveSelectedRow = WM_USER + 1;
  ApplyCaretAtTimerID = 1;

{ TInspector }

class constructor TInspector.Create;

  procedure BF(const AFileType: TScriptBrowseFileType; const AFilesName: String;
    const AExtensions: TArray<String>);
  begin
    FScriptBrowseFileTypeFilters[AFileType].FilesName := AFilesName;
    FScriptBrowseFileTypeFilters[AFileType].Extensions := AExtensions;
  end;

  procedure InitializeScriptBrowseFileTypeFilters;
  begin
    BF(bftDocs, SDocFiles, [SLitRtfExt, SLitTxtExt]);
    BF(bftIco, SIcoFiles, [SLitIcoExt]);
    BF(bftImages, SImageFiles, [SLitPngExt, SLitBmpExt]);
    BF(bftVclStyle, SVclStylesFiles, [SLitVsfExt]);
    BF(bftIsl, SIslFiles, [SLitIslExt]);
    BF(bftKey, SIsPublicKeyFiles, [SLitIsPublicKeyExt]);
    BF(bftTxt, STxtFiles, [SLitTxtExt]);
  end;

begin
  InitializeScriptBrowseFileTypeFilters;
end;

constructor TInspector.Create(const AJvInspector: TJvInspector;
  const ANoteText: TNewStaticText;
  const AFactory: TLiveScriptObjectFactory;
  const AShowAllKnownDirectives, AFollowCaret: Boolean;
  const AOnGetBaseDir: TInspectorGetBaseDirEvent;
  const AOnGetSignTools: TInspectorGetSignToolsEvent;
  const AOnGetMainFactory: TInspectorGetMainFactoryEvent);
{ Takes ownership of AJvInspector but not of ANoteText }
begin
  inherited Create;

  FNoteText := ANoteText;
  FFactory := AFactory;
  FOnGetBaseDir := AOnGetBaseDir;
  FOnGetSignTools := AOnGetSignTools;
  FOnGetMainFactory := AOnGetMainFactory;
  FShowAllKnownDirectives := AShowAllKnownDirectives;
  FFollowCaret := AFollowCaret;
  {$IFDEF DEBUG}
  FDebugStatusRowString := 'Not updated yet';
  {$ENDIF}
  FMessagesWnd := AllocateHWnd(MessagesWndProc);
  FRows := TList<TInspectorRow>.Create;

  { The metadata has the [Setup] categories in English alphabetical order,
    which localization breaks: resort just those }
  FCategoryNamesInDisplayOrder := ScriptCategoryNamesOrdered;
  var FirstSetupCategoryIndex, SetupCategoryCount: NativeInt;
  GetScriptSetupCategoryNamesRange(FirstSetupCategoryIndex, SetupCategoryCount);
  TArray.Sort<String>(FCategoryNamesInDisplayOrder, TComparer<String>.Construct(
    function(const Left, Right: String): Integer
    begin
      Result := LCompareText(LFmtMessage(Left), LFmtMessage(Right));
    end), FirstSetupCategoryIndex, SetupCategoryCount);

  FJvInspector := AJvInspector;
  FJvInspector.OnCustomizeItemCanvas := JvInspectorCustomizeItemCanvas;
  FJvInspector.BeforeEdit := JvInspectorBeforeEdit;
  FJvInspector.OnKeyDown := JvInspectorKeyDown;
  FJvInspector.OnEditorKeyDown := JvInspectorKeyDown;
  FJvInspector.OnLeafNameDblClick := JvInspectorLeafNameDblClick;
  FJvInspector.OnEditButtonClick := JvInspectorEditButtonClick;
  FJvInspector.OnGetAsOrdinal := RowGetAsOrdinal;
  FJvInspector.OnGetAsString := RowGetAsString;
  FJvInspector.OnSetAsOrdinal := RowSetAsOrdinal;
  FJvInspector.OnSetAsString := RowSetAsString;
  FJvInspector.OnGetValueList := ChoiceRowGetValueList;
end;

destructor TInspector.Destroy;
begin
  { Free the inspector before the objects its rows read from }
  FJvInspector.Free;
  FLiveParameterSectionEntries.Free;
  FLiveKeyValueSection.Free;
  FRows.Free;
  if FMessagesWnd <> 0 then
    DeallocateHWnd(FMessagesWnd);
  inherited;
end;

procedure TInspector.UpdateNote;

  procedure ShowNote(const AText: String);
  begin
    FNoteText.Caption := AText;
    FNoteText.Visible := True; { This updates any stale width }
    FNoteText.AdjustHeight;
  end;

  procedure HideNote;
  begin
    FNoteText.Visible := False;
  end;

begin
  if (FLiveParameterSectionEntries = nil) and (FLiveKeyValueSection = nil) then begin
    if FMixedSelection then
      ShowNote(LFmtMessage(SInspectorMixedSelectionNote))
    else
      ShowNote(LFmtMessage(SInspectorNothingToInspectNote));
  end else if ShowingDirectiveSection then begin
    if FShowAllKnownDirectives and FLiveKeyValueSectionHasSiblingOccurrences then
      ShowNote(LFmtMessage(SInspectorSiblingOccurrencesNote))
    else if FShowAllKnownDirectivesSuppressedNote then
      ShowNote(LFmtMessage(SInspectorShowAllKnownDirectivesSuppressedNote))
    else
      HideNote;
  end else
    HideNote;
end;

function TInspector.ItemShouldBeBold(
  const AItem: TJvCustomInspectorItem): Boolean;

  function RowShouldBeBold(const ARow: TInspectorRow): Boolean;
  begin
    Result := False;
    case ARow.Kind of
      irkParameter:
        Result := (FLiveParameterSectionEntries <> nil) and
          FLiveParameterSectionEntries.MemberPresent(ARow.Name, ARow.Index);
      irkParameterFlag:
        Result := (FLiveParameterSectionEntries <> nil) and
          (FLiveParameterSectionEntries.GetFlagCheckState(ARow.Name, ARow.Index,
             ARow.FlagName) <> fcsNone);
      irkKey:
        { Without ShowAllKnownDirectives only directives which are in the
          script get a row, so bold would say nothing }
        if FShowAllKnownDirectives then
          Result := (FLiveKeyValueSection <> nil) and
            FLiveKeyValueSection.MemberPresent(ARow.Name, ARow.Index);
      irkKeyFlag:
        { See above }
        if FShowAllKnownDirectives then
          Result := (FLiveKeyValueSection <> nil) and
            (FLiveKeyValueSection.GetFlagCheckState(ARow.Name, ARow.Index,
               ARow.FlagName) <> fcsNone);
    end;
  end;

begin
  var Row: TInspectorRow;
  Result := TryGetRow(AItem, Row) and RowShouldBeBold(Row);
end;

procedure TInspector.JvInspectorCustomizeItemCanvas(Item: TJvCustomInspectorItem;
  Canvas: TCanvas);
begin
  { Called just before it draws each row's name and again just before its
    value, and also when it measures text to decide on a hint }
  if ItemShouldBeBold(Item) then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
end;

procedure TInspector.JvInspectorBeforeEdit(Sender: TObject;
  Item: TJvCustomInspectorItem; Edit: TEdit);
begin
  { Bold the in-place editor's font as well }
  if ItemShouldBeBold(Item) then
    Edit.Font.Style := Edit.Font.Style + [fsBold];
end;

function TInspector.GetSelectedHelpKeyword: String;
begin
  Result := '';
  const Item = FJvInspector.Selected;
  var Row: TInspectorRow;
  if (Item <> nil) and TryGetRow(Item, Row) then begin
    case Row.Kind of
      irkParameter:
        Result := Row.Name;
      irkKey:
        begin
          Result := Row.Name;
          if (FLiveKeyValueSection <> nil) and FLiveKeyValueSection.Valid then begin
            { This resolves [LangOptions] directives which use a language name prefix }
            var Definition: TMemberDefinition;
            if FLiveKeyValueSection.Section.TryGetDefinition(Row.Name, Definition) then
              Result := Definition.Name;
          end;
        end;
      irkParameterFlag, irkKeyFlag:
        if (Row.Kind = irkParameterFlag) and SameText(Row.Name, 'Flags') then
          Result := Row.FlagName { Keyword presence guaranteed }
        else
          Result := Row.Name;
    end;
  end;
end;

procedure TInspector.JvInspectorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F1) and (Shift * [ssShift, ssAlt, ssCtrl] = []) then begin
    Key := 0;
    ShowHelp(GetSelectedHelpKeyword);
  end else if Key = VK_DELETE then begin
    const Modifiers = Shift * [ssShift, ssAlt, ssCtrl];
    const EditorActive = FJvInspector.EditorActive;
    if ((Modifiers = [ssCtrl]) or ((Modifiers = []) and not EditorActive)) and
       CanRemoveSelectedRow then begin
      if EditorActive then begin
        { Defer the removal until the key message unwinds: RemoveSelectedRow
          frees the edit control whose key event is still executing }
        if (FMessagesWnd <> 0) and PostMessage(FMessagesWnd, WM_RemoveSelectedRow, 0, 0) then
          Key := 0;
      end else begin
        Key := 0;
        RemoveSelectedRow;
      end;
    end;
  end;
end;

procedure TInspector.MessagesWndProc(var Message: TMessage);

  function AnyInputDown: Boolean;
  { Also handles mouse input }
  begin
    { The mouse buttons don't map to a scan code (see below) so check them separately }
    for var Key := VK_LBUTTON to VK_XBUTTON2 do
      if GetAsyncKeyState(Key) < 0 then
        Exit(True);
    { For the other codes only trust GetAsyncKeyState if the code maps to a
      scan code. This is because it was seen to report non-existing code 0
      as being down consistently in one debug session, but never in any other. }
    for var Key := VK_BACK to $FE do { This includes reserved and unassigned key codes }
      if (GetAsyncKeyState(Key) < 0) and (MapVirtualKey(Key, MAPVK_VK_TO_VSC) <> 0) then
        Exit(True);
    Result := False;
  end;

begin
  if Message.Msg = WM_RemoveSelectedRow then
    RemoveSelectedRow
  else if (Message.Msg = WM_TIMER) and (Message.WParam = ApplyCaretAtTimerID) then begin
    if AnyInputDown then
      Exit; { Keeps timer alive }
    ApplyCaretAtTimerUpdate(True); { Kills timer }
    ApplyCaretAt;
  end else
    Message.Result := DefWindowProc(FMessagesWnd, Message.Msg, Message.WParam, Message.LParam);
end;

function TInspector.CanGoToSelectedRow: Boolean;
begin
  Result := Length(GetSelectedRowValuePositions(1)) > 0;
end;

function TInspector.GetSelectedRowValuePositions(
  const AMaxCount: Integer): TArray<TValuePosition>;
{ Returns the value position of the selected row's parameter or key in every
  entry where it is present, in entry order, stopping once AMaxCount positions
  were collected (if AMaxCount <> 0) }
begin
  Result := [];
  const Item = FJvInspector.Selected;
  var Row: TInspectorRow;
  if (Item = nil) or not TryGetRow(Item, Row) then
    Exit;
  case Row.Kind of
    irkParameter, irkParameterFlag:
      if (FLiveParameterSectionEntries <> nil) and
         FLiveParameterSectionEntries.Valid then begin
        for var I := 0 to FLiveParameterSectionEntries.Count-1 do begin
          const LiveEntry = FLiveParameterSectionEntries.Entries[I];
          var Index := Row.Index;
          var Position: TValuePosition;
          if LiveEntry.Entry.TryResolve(Row.Name, Index) and
             LiveEntry.Entry.TryGetValuePosition(Index, Position) then begin
            Inc(Position.StartLineIndex, LiveEntry.FirstLine);
            Inc(Position.EndLineIndex, LiveEntry.FirstLine);
            Result := Result + [Position];
            if Length(Result) = AMaxCount then
              Exit;
          end;
        end;
      end;
    irkKey, irkKeyFlag:
      if (FLiveKeyValueSection <> nil) and FLiveKeyValueSection.Valid then begin
        const Section = FLiveKeyValueSection.Section;
        var Index := Row.Index;
        var Position: TValuePosition;
        if Section.TryResolve(Row.Name, Index) and
           Section.TryGetValuePosition(Index, Position) then begin
          var Line := FLiveKeyValueSection.FirstLine;
          for var I := 0 to Index-1 do
            Inc(Line, Section.GetLineCount(I));
          Inc(Position.StartLineIndex, Line);
          Inc(Position.EndLineIndex, Line);
          Result := Result + [Position];
        end;
      end;
  end;
end;

procedure TInspector.JvInspectorLeafNameDblClick(Item: TJvCustomInspectorItem);
begin
  GoToSelectedRow;
end;

procedure TInspector.JvInspectorEditButtonClick(Item: TJvCustomInspectorItem;
  var Value: String);

  function ScriptPathExpand(const ABaseDir, AValue: String;
    out AExpandedPath: String): Boolean;
  begin
    Result := False;

    { Fail when AValue is empty or uses ISPP syntax }
    if (AValue = '') or (Pos('{#', AValue) <> 0) then
      Exit;

    { Fail when AValue is relative and either the base dir is unknown, or a path
      prefix like 'compiler:' is used (see TSetupCompiler.PrependDirName) }
    if not PathIsRooted(AValue) and ((ABaseDir = '') or (PathPos(':', AValue) <> 0)) then
      Exit;

    Result := PathExpand(PathCombine(ABaseDir, AValue), AExpandedPath);
  end;

  function GetBaseDirForMember(const ADefinition: TMemberDefinition): String;
  begin
    { Get base directory }
    var BaseDir := '';
    if Assigned(FOnGetBaseDir) then
      BaseDir := FOnGetBaseDir;
    if BaseDir = '' then
      Exit('');
    var ExpandedBaseDir: String;
    if not PathExpand(BaseDir, ExpandedBaseDir) then
      Exit('');

    { SourceDir itself resolves against the plain base directory }
    if (ADefinition.ValueKind = mvkCompilerPath) and
       SameText(ADefinition.Name, 'SourceDir') then
      Exit(ExpandedBaseDir);

    { Combine with the main file's [Setup] SourceDir when present, like the
      compiler's PrependSourceDirName }
    var SourceDir := ExpandedBaseDir;
    const SourceDirValue = FindSetupDirectiveValue('SourceDir');
    if SourceDirValue <> '' then
      if not ScriptPathExpand(ExpandedBaseDir, SourceDirValue, SourceDir) then
        Exit('');

    { mvkCompilerDestFile: Additionally combine with the main file's [Setup]
      OutputDir, like the compiler's PrependDirName for OutputManifestFile.
      Else: Done. }
    if ADefinition.ValueKind <> mvkCompilerDestFile then
      Exit(SourceDir);
    const OutputDirValue = FindSetupDirectiveValue('OutputDir');
    var OutputDir: String;
    if not ScriptPathExpand(SourceDir, OutputDirValue, OutputDir) then
      Exit('');
    Result := OutputDir;
  end;

  procedure MakeRelative(var Path: String; const BaseDir: String);
  begin
    if BaseDir = '' then
      Exit;
    const Prefix = AddBackslash(BaseDir);
    if PathStartsWith(AddBackslash(Path), Prefix) then begin
      if Length(Path) > Length(Prefix) then
        Path := Copy(Path, Length(Prefix)+1, Maxint)
      else
        Path := '.';
    end;
  end;

begin
  if FFactory.Memo.ReadOnly then
    Exit;

  var Row: TInspectorRow;
  if not TryGetRow(Item, Row) then
    Exit;

  var Definition: TMemberDefinition;
  var SectionName := '';
  var Known := False;
  case Row.Kind of
    irkParameter:
      if (FLiveParameterSectionEntries <> nil) and FLiveParameterSectionEntries.Valid then begin
        Known := FLiveParameterSectionEntries.PrimaryEntry.TryGetDefinition(Row.Name, Definition);
        if Known then
          SectionName := FLiveParameterSectionEntries.PrimaryEntry.Metadata.SectionName;
      end;
    irkKey:
      if (FLiveKeyValueSection <> nil) and FLiveKeyValueSection.Valid then begin
        Known := FLiveKeyValueSection.Section.TryGetDefinition(Row.Name, Definition);
        if Known then
          SectionName := FLiveKeyValueSection.Section.Metadata.SectionName;
      end;
  end;
  if not Known then
    Exit;

  { Special: [Files] Source can only be browsed for if no entry is external }
  if SameText(SectionName, 'Files') and SameText(Definition.Name, 'Source') then begin
    if FLiveParameterSectionEntries.GetFlagCheckState('Flags', -1, 'external') <> fcsNone then begin
      MsgBox(LFmtMessage(SInspectorBrowseParamFlagError, ['Source', 'external']),
        LFmtMessage(SCompilerFormCaption), mbError, MB_OK);
      Exit;
    end;
  end;

  const Handle = GetParentForm(FJvInspector).Handle;

  if Definition.ValueKind in [mvkCompilerSourceFile, mvkCompilerSourceFiles, mvkCompilerPath, mvkCompilerDestFile] then begin
    { Determine base directory against which relative paths are resolved for this item }
    const BaseDir = GetBaseDirForMember(Definition);

    { Determine initial directory and file name }
    var S := Trim(Value);
    if Definition.ValueKind = mvkCompilerSourceFiles then
      S := ExtractStr(S, ',');
    if (S = '') and (Definition.ValueKind = mvkCompilerPath) and
       SameText(Definition.Name, 'SignedUninstallerDir') then
      S := FindSetupDirectiveValue('OutputDir'); { Special: blank SignedUninstallerDir means the output directory }
    var InitialDir := '';
    var InitialFileName := ''; { Not used by mvkCompilerSourceFiles/mvkCompilerPath }
    var ExpandedPath: String;
    if ScriptPathExpand(BaseDir, S, ExpandedPath) then begin
      if Definition.ValueKind = mvkCompilerPath then
        InitialDir := ExpandedPath
      else begin
        InitialDir := PathExtractDir(ExpandedPath);
        InitialFileName := ExpandedPath;
      end;
    end else begin
      InitialDir := BaseDir;
      if (InitialDir = '') and Assigned(FOnGetBaseDir) then
        InitialDir := FOnGetBaseDir;
    end;

    { Determine filter and default extension }
    var FileType: TScriptBrowseFileType;
    var Filter: String;
    var DefaultExt: String;
    if TryGetScriptBrowseFileType(SectionName, Definition.Name, FileType) then begin
      const FileTypeFilter = FScriptBrowseFileTypeFilters[FileType];
      Filter := FormatFileFilter(FileTypeFilter.FilesName, FileTypeFilter.Extensions);
      DefaultExt := FileTypeFilter.Extensions[0];
    end else begin
      Filter := Format(SLitAllFilesFilter, [LFmtMessage(SAllFiles)]);
      DefaultExt := '';
    end;

    { Browse }
    case Definition.ValueKind of
      mvkCompilerSourceFile:
        begin
          var FileName := InitialFileName;
          if NewGetOpenFileName('', FileName, InitialDir, Filter, DefaultExt, Handle) then begin
            MakeRelative(FileName, BaseDir);
            Value := FileName;
          end;
        end;
      mvkCompilerSourceFiles:
        begin
          const FileList = TStringList.Create;
          try
            if NewGetOpenFileNameMulti('', FileList, InitialDir, Filter, DefaultExt, Handle) then begin
              for var I := 0 to FileList.Count-1 do begin
                var FileName := FileList[I];
                MakeRelative(FileName, BaseDir);
                FileList[I] := FileName;
              end;
              Value := String.Join(',', FileList.ToStringArray);
            end;
          finally
            FileList.Free;
          end;
        end;
      mvkCompilerPath:
        begin
          var Directory := InitialDir;
          if BrowseForFolder('', Directory, Handle) then begin
            MakeRelative(Directory, BaseDir);
            Value := Directory;
          end;
        end;
      mvkCompilerDestFile:
        begin
          var FileName := InitialFileName;
          if NewGetSaveFileName('', FileName, InitialDir, Filter, DefaultExt, Handle) then begin
            MakeRelative(FileName, BaseDir);
            Value := FileName;
          end;
        end;
    end;
  end;
end;

function TInspector.GoToSelectedRow: Boolean;
begin
  Result := CanGoToSelectedRow;
  if not Result then
    Exit;

  const Memo = FFactory.Memo;
  Memo.SetFocus;
  if not Memo.Focused then
    Exit; { Validation rejected the focus change }

  { Losing focus may have committed an edit, so need to update }
  UpdateFromCaret;

  const Positions = GetSelectedRowValuePositions;
  if Length(Positions) > 0 then begin
    for var I := 0 to High(Positions) do begin
      const Range = TScintRange.Create(
        Memo.GetPositionRelativeCodeUnits(Memo.GetPositionFromLine(Positions[I].StartLineIndex), Positions[I].StartCharIndex),
        Memo.GetPositionRelativeCodeUnits(Memo.GetPositionFromLine(Positions[I].EndLineIndex), Positions[I].EndCharIndex));
      if I = 0 then
        Memo.Selection := Range
      else
        Memo.AddSelection(Range.EndPos, Range.StartPos);
    end;
    if Length(Positions) > 1 then begin
      Memo.MainSelection := 0;
      Memo.ScrollCaretIntoView;
    end;
  end;
end;

function TInspector.RowResolves(const ARow: TInspectorRow): Boolean;
begin
  Result := False;
  case ARow.Kind of
    irkParameter, irkParameterFlag:
      Result := (FLiveParameterSectionEntries <> nil) and
        FLiveParameterSectionEntries.MemberPresent(ARow.Name, ARow.Index);
    irkKey, irkKeyFlag:
      Result := (FLiveKeyValueSection <> nil) and
        FLiveKeyValueSection.MemberPresent(ARow.Name, ARow.Index);
  end;
end;

function TInspector.SelectedRowResolves: Boolean;
begin
  const Item = FJvInspector.Selected;
  var Row: TInspectorRow;
  Result := (Item <> nil) and TryGetRow(Item, Row) and RowResolves(Row);
end;

function TInspector.CanRemoveSelectedRow: Boolean;
begin
  Result := not FFactory.Memo.ReadOnly and SelectedRowResolves;
end;

procedure TInspector.RemoveSelectedRow;
begin
  if FFactory.Memo.ReadOnly then
    Exit;
  const Item = FJvInspector.Selected;
  var Row: TInspectorRow;
  if (Item = nil) or not TryGetRow(Item, Row) or not RowResolves(Row) then
    Exit;

  { Cancel any open in-place edit: a commit could re-add the value being
    removed }
  ForceFinishEdit(True);

  RowRemove(Row);
end;

function TInspector.ShowingDirectiveSection: Boolean;
begin
  Result := (FLiveKeyValueSection <> nil) and FLiveKeyValueSectionIsDirectiveSection;
end;

function TInspector.ShowingParameterSectionEntry: Boolean;
begin
  Result := FLiveParameterSectionEntries <> nil;
end;

procedure TInspector.ForceFinishEdit(const AForceCancel: Boolean);
{ Commits a pending in-place edit, silently reverting it if its value is
  rejected, or loudly reverting on other errors, or always reverting it if
  AForceCancel is set.
  Note: Editing only restarts on a selection change, so the row would be
  left selected without its editor and would ignore clicks. To avoid this
  the next UpdateFromCaret is forced to rebuild, which reselects the row. }
begin
  const Item = FJvInspector.Selected;
  if (Item <> nil) and Item.Editing then begin
    if AForceCancel or FFactory.Memo.ReadOnly then
      Item.DoneEdit(True)
    else begin
      try
        Item.DoneEdit;
      except
        on EInspectorValueRejected do
          Item.DoneEdit(True);
        else begin
          Application.HandleException(Self);
          Item.DoneEdit(True);
        end;
      end;
    end;
    FRowSetSignature := ''; { See explanation above }
  end;
end;

procedure TInspector.SetActiveFactory(const AFactory: TLiveScriptObjectFactory;
  const AShowAllKnownDirectives, AShowAllKnownDirectivesSuppressedNote: Boolean);
begin
  if AFactory = FFactory then begin
    { Still apply the settings: they may have changed independently. Also still
      update from caret. }
    ShowAllKnownDirectivesSuppressedNote := AShowAllKnownDirectivesSuppressedNote; { Also updates the note }
    if AShowAllKnownDirectives <> FShowAllKnownDirectives then
      ShowAllKnownDirectives := AShowAllKnownDirectives { Also updates from caret }
    else
      UpdateFromCaret;
    Exit;
  end;
  { Attach to a different factory = different memo = different tab }
  FFactory := AFactory;
  FShowAllKnownDirectives := AShowAllKnownDirectives;
  FShowAllKnownDirectivesSuppressedNote := AShowAllKnownDirectivesSuppressedNote;
  FRowSetSignature := ''; { Force rebuild even if row set stayed same }
  FCaretAt.Valid := False;
  ApplyCaretAtTimerUpdate(True); { Cancel any queued }
  UpdateFromCaret;
end;

procedure TInspector.InvalidateChangedRows;
begin
  for var I := 0 to FRows.Count-1 do begin
    var Row := FRows[I];
    const ValueSignature = GetRowValueSignature(Row);
    if ValueSignature <> Row.LastValueSignature then begin
      Row.LastValueSignature := ValueSignature;
      FRows[I] := Row;
      FJvInspector.InvalidateItemByTag(I+1); { See AddRow }
    end;
  end;
end;

procedure TInspector.UpdateFromCaret;

  function LiveObjectTextChanged: Boolean;
  begin
    if FFactory.ChangeCount < FChangeCountAtCreation then
      raise Exception.Create('Internal error: LiveObjectTextChanged: ChangeCount decreased');
    Result := FFactory.ChangeCount > FChangeCountAtCreation;
  end;

  function LineRangesCoverMultipleLines(
    const ALineRanges: TArray<TScintLineRange>): Boolean;
  begin
    Result := (Length(ALineRanges) > 1) or { The ranges are merged so two ranges always cover more than one line }
      ((Length(ALineRanges) = 1) and (ALineRanges[0].EndLine > ALineRanges[0].StartLine));
  end;

  function LineRangesEqual(const ALineRanges1,
    ALineRanges2: TArray<TScintLineRange>): Boolean;
  { Assumes ranges are sorted }
  begin
    if Length(ALineRanges1) <> Length(ALineRanges2) then
      Exit(False);
    for var I := 0 to High(ALineRanges1) do begin
      if (ALineRanges1[I].StartLine <> ALineRanges2[I].StartLine) or
         (ALineRanges1[I].EndLine <> ALineRanges2[I].EndLine) then
        Exit(False);
    end;
    Result := True;
  end;

  function ItemID(const AItem: TJvCustomInspectorItem;
    const AIncludeIndex: Boolean): String;
  { AIncludeIndex: Include the row index so the id is unique even for duplicated member names }
  begin
    if AItem is TJvInspectorCustomCategoryItem then
      Result := 'C|' + AItem.DisplayName
    else begin
      Result := 'R|' + AItem.DisplayName;
      if AIncludeIndex then begin
        var Row: TInspectorRow;
        if not TryGetRow(AItem, Row) then
          raise Exception.Create('Internal error: ItemID: Row not found');
        Result := Result + '|' + IntToStr(Row.Index);
      end;
    end;
  end;

  procedure SaveExpandedStates(const AStates: TDictionary<String, Boolean>;
    const AParent: TJvCustomInspectorItem);
  begin
    for var I := 0 to AParent.Count-1 do begin
      const Item = AParent.Items[I];
      if Item.Count > 0 then begin
        AStates.AddOrSetValue(ItemID(Item, False), Item.Expanded);
        SaveExpandedStates(AStates, Item);
      end;
    end;
  end;

  procedure RestoreExpandedStates(const AStates: TDictionary<String, Boolean>;
    const AParent: TJvCustomInspectorItem);
  begin
    for var I := 0 to AParent.Count-1 do begin
      const Item = AParent.Items[I];
      if Item.Count > 0 then begin
        var Expanded: Boolean;
        if AStates.TryGetValue(ItemID(Item, False), Expanded) then
          Item.Expanded := Expanded;
        RestoreExpandedStates(AStates, Item);
      end;
    end;
  end;

  function NewCategory(const AName: String): TJvCustomInspectorItem;
  begin
    Result := TJvInspectorCustomCategoryItem.Create(FJvInspector.Root);
    Result.DisplayName := LFmtMessage(AName); { These are localizable, because they use resourcestrings }
    Result.Expanded := True;
  end;

  function NameMatchesFilter(const AName: String): Boolean;
  begin
    Result := (FFilterText = '') or (PathStrFind(PChar(AName), Length(AName),
      PChar(FFilterText), Length(FFilterText)) >= 0);
  end;

  function AnyFlagMatchesFilter(const AFlagNames: TArray<String>): Boolean;
  begin
    Result := False;
    for var FlagName in AFlagNames do
      if NameMatchesFilter(FlagName) then
        Exit(True);
  end;

  function DefinitionMatchesFilter(const ADefinition: TMemberDefinition): Boolean;
  begin
    Result := NameMatchesFilter(ADefinition.Name) or
      ((ADefinition.ValueKind = mvkFlags) and AnyFlagMatchesFilter(ADefinition.KnownValues));
  end;

  function KeyRowMatchesFilter(const ARow: TInspectorRow): Boolean;
  begin
    Result := NameMatchesFilter(ARow.Name);
    if not Result then begin
      var Definition: TMemberDefinition;
      Result := FLiveKeyValueSection.Section.TryGetDefinition(ARow.Name, Definition) and
        (Definition.ValueKind = mvkFlags) and AnyFlagMatchesFilter(Definition.KnownValues);
    end;
  end;

  {$IFDEF DEBUG}
  procedure UpdateDebugStatusRowStringForParameterSectionEntries(const ASectionName: String);
  begin
    if FLiveParameterSectionEntries.Count = 1 then begin
      FDebugStatusRowString := Format('[%s] entry at lines %d-%d',
        [ASectionName, FLiveParameterSectionEntries.PrimaryFirstLine+1,
         FLiveParameterSectionEntries.PrimaryLastLine+1]);
    end else begin
      FDebugStatusRowString := Format('[%s] %d entries at lines',
        [ASectionName, FLiveParameterSectionEntries.Count]);
      for var I := 0 to FLiveParameterSectionEntries.Count-1 do begin
        const LiveEntry = FLiveParameterSectionEntries.Entries[I];
        if I > 0 then
          FDebugStatusRowString := FDebugStatusRowString + ',';
        FDebugStatusRowString := FDebugStatusRowString + Format(' %d-%d',
          [LiveEntry.FirstLine+1, LiveEntry.LastLine+1]);
      end;
    end;
  end;

  function RefusalReasonToString(const ARefusalReason: TRefusalReason): String;
  begin
    case ARefusalReason of
      rrLineOutOfRange: Result := 'The line number is out of range';
      rrNotInsideSection: Result := 'The line is not inside a section';
      rrInCodeSection: Result := 'The line is in the [Code] section';
      rrUnrecognizedSection: Result := 'The line is in an unrecognized section';
      rrNotParameterSection: Result := 'The line is not in a parameter section';
      rrComment: Result := 'The line is a comment';
      rrISPPDirective: Result := 'The line is an ISPP directive';
      rrMixedSelection: Result := 'The selection mixes section types or other content';
      rrSectionIndexOutOfRange: Result := 'The section index is out of range';
      rrNotKeyValueSection: Result := 'The section is not a key/value section';
    else
      Result := '';
    end;
  end;
  {$ENDIF}

  function AddRow(const AParent: TJvCustomInspectorItem;
    const ADisplayName: String; const ACheckBox: Boolean;
    const ARow: TInspectorRow): TJvCustomInspectorItem;
  begin
    if ACheckBox then
      Result := TJvInspectorBooleanItem.Create(AParent)
    else
      Result := TJvInspectorStringItem.Create(AParent);
    Result.DisplayName := ADisplayName;
    var Row := ARow;
    Row.CheckBox := ACheckBox;
    Row.LastValueSignature := GetRowValueSignature(Row);
    FRows.Add(Row);
    Result.Tag := FRows.Count;
  end;

  {$IFDEF DEBUG}
  procedure AddDebugRow(const AParent: TJvCustomInspectorItem;
    const ADisplayName: String; const AKind: TInspectorRowKind);
  begin
    var Row := Default(TInspectorRow);
    Row.Kind := AKind;
    Row.Index := -1;
    const Item = AddRow(AParent, ADisplayName, False, Row);
    Item.Flags := Item.Flags + [iifReadonly];
  end;
  {$ENDIF}

  function MakeParameterRow(const AName: String;
    const AIndex: Integer): TInspectorRow;
  begin
    Result := Default(TInspectorRow);
    Result.Kind := irkParameter;
    Result.Name := AName;
    Result.Index := AIndex;
  end;

  procedure AddParameterFlagRow(const AParent: TJvCustomInspectorItem;
    const AParameterName, AFlagName: String; const AIndex: Integer);
  begin
    var Row := Default(TInspectorRow);
    Row.Kind := irkParameterFlag;
    Row.Name := AParameterName;
    Row.FlagName := AFlagName;
    Row.Index := AIndex;
    AddRow(AParent, AFlagName, True, Row);
  end;

  procedure AddParameterRow(const AParent: TJvCustomInspectorItem;
    const ADefinition: TMemberDefinition; const AIndex: Integer);
  begin
    const Row = MakeParameterRow(ADefinition.Name, AIndex);
    const Item = AddRow(AParent, Row.Name, False, Row);
    if ADefinition.ValueKind = mvkFlags then begin
      const KeepAllFlags = NameMatchesFilter(ADefinition.Name);
      for var FlagName in ADefinition.KnownValues do
        if KeepAllFlags or NameMatchesFilter(FlagName) then
          AddParameterFlagRow(Item, ADefinition.Name, FlagName, AIndex); { Adds a child to Item }
    end else if ADefinition.ValueKind = mvkChoice then
      Item.Flags := Item.Flags + [iifValueList]
    else if ADefinition.ValueKind in [mvkCompilerSourceFile, mvkCompilerSourceFiles,
       mvkCompilerPath, mvkCompilerDestFile] then
      Item.Flags := Item.Flags + [iifEditButton]
    else if ADefinition.ValueKind = mvkPermissions then begin
      Item.Flags := Item.Flags + [iifValueList];
      Item.OnGetValueList := PermissionsRowGetValueList;
    end else if (FLiveParameterSectionEntries.Section = scFiles) and
       SameText(ADefinition.Name, 'DestDir') then begin
      Item.Flags := Item.Flags + [iifValueList];
      Item.OnGetValueList := DestDirRowGetValueList;
    end else if GetScriptSectionDefiningParameterValues(ADefinition.Name) <> scNone then begin
      Item.Flags := Item.Flags + [iifValueList];
      Item.OnGetValueList := ScriptValuesRowGetValueList;
    end;
  end;

  procedure AddParameterOccurrenceRows(const AParent: TJvCustomInspectorItem;
    const ADefinition: TMemberDefinition);
  begin
    var Added := False;
    if FLiveParameterSectionEntries.Count = 1 then begin
      { Normally a parameter will be present only once, but duplicates are still
        handled here, even though that doesn't compile }
      const PrimaryEntry = FLiveParameterSectionEntries.PrimaryEntry;
      for var I := 0 to PrimaryEntry.Count-1 do begin
        if (PrimaryEntry.Parameters[I].Kind = pkParameter) and
           SameText(PrimaryEntry.Parameters[I].Name, ADefinition.Name) then begin
          AddParameterRow(AParent, ADefinition, I);
          Added := True;
        end;
      end;
    end; { else: no special handling for duplicates }
    if not Added then
      AddParameterRow(AParent, ADefinition, -1);
  end;

  function CleanKeyName(const AKeyName: String): String;
  begin
    var Definition: TMemberDefinition;
    if FLiveKeyValueSection.Section.TryGetDefinition(AKeyName, Definition) and
       SameText(AKeyName, Definition.Name) then { Check for stripped language name prefix }
      Result := Definition.Name
    else
      Result := AKeyName;
  end;

  function MakeKeyRow(const AName: String;
    const AIndex: Integer): TInspectorRow;
  begin
    Result := Default(TInspectorRow);
    Result.Kind := irkKey;
    Result.Name := AName;
    Result.Index := AIndex;
  end;

  function KeyRowIsCheckBox(const ADefinition: TMemberDefinition;
    const AIndex: Integer): Boolean;
  { A yes/no key gets a true checkbox row only if its value is a simple yes/no and
    not something like an ISPP inline directive, else it falls back to a text & dropdown row. }
  begin
    var BoolValue := False;
    Result := (ADefinition.ValueKind = mvkYesNo) and
      ((AIndex < 0) or { Don't check unspecified keys, they don't have a value }
       TryStrToBoolean(FLiveKeyValueSection.Section.Lines[AIndex].Value, BoolValue));
  end;

  procedure AddKeyFlagRow(const AParent: TJvCustomInspectorItem;
    const AKeyName, AFlagName: String; const AIndex: Integer);
  begin
    var Row := Default(TInspectorRow);
    Row.Kind := irkKeyFlag;
    Row.Name := AKeyName;
    Row.FlagName := AFlagName;
    Row.Index := AIndex;
    AddRow(AParent, AFlagName, True, Row);
  end;

  procedure AddKeyRow(const AParent: TJvCustomInspectorItem;
    const ARow: TInspectorRow);
  begin
    var Definition: TMemberDefinition;
    const Known = FLiveKeyValueSection.Section.TryGetDefinition(ARow.Name, Definition);
    if Known and KeyRowIsCheckBox(Definition, ARow.Index) then
      AddRow(AParent, ARow.Name, True, ARow)
    else begin
      const Item = AddRow(AParent, ARow.Name, False, ARow);
      if Known then begin
        if Definition.ValueKind = mvkFlags then begin
          const KeepAllFlags = NameMatchesFilter(ARow.Name);
          for var FlagName in Definition.KnownValues do
            if KeepAllFlags or NameMatchesFilter(FlagName) then
              AddKeyFlagRow(Item, ARow.Name, FlagName, ARow.Index); { Adds a child to Item }
        end else if Definition.ValueKind in [mvkChoice, mvkYesNo] then
          Item.Flags := Item.Flags + [iifValueList]
        else if Definition.ValueKind in [mvkCompilerSourceFile, mvkCompilerSourceFiles,
           mvkCompilerPath, mvkCompilerDestFile] then
          Item.Flags := Item.Flags + [iifEditButton]
        else if SameText(Definition.Name, 'SignTool') and
           SameText(FLiveKeyValueSection.Section.Metadata.SectionName, 'Setup') then begin
          Item.Flags := Item.Flags + [iifValueList];
          Item.OnGetValueList := SignToolRowGetValueList;
        end;
      end;
    end;
  end;

  procedure AddUnknownParameterRows(const ACategoryName: String;
    var ACategoryItem: TJvCustomInspectorItem);
  begin
    const PrimaryEntry = FLiveParameterSectionEntries.PrimaryEntry;

    if FLiveParameterSectionEntries.Count > 1 then begin
      { Add all names used across the entries, addressed by name }
      const AddedNames = TStringList.Create;
      try
        AddedNames.CaseSensitive := False;
        for var EntryIndex := 0 to FLiveParameterSectionEntries.Count-1 do begin
          const LiveEntry = FLiveParameterSectionEntries.Entries[EntryIndex];
          for var I := 0 to LiveEntry.Entry.Count-1 do begin
            const Parameter = LiveEntry.Entry.Parameters[I];
            if Parameter.Kind = pkParameter then begin
              var Definition: TMemberDefinition;
              if not LiveEntry.Entry.TryGetDefinition(Parameter.Name, Definition) and
                 NameMatchesFilter(Parameter.Name) and
                 (AddedNames.IndexOf(Parameter.Name) < 0) then begin
                AddedNames.Add(Parameter.Name);
                if ACategoryItem = nil then
                  ACategoryItem := NewCategory(ACategoryName);
                const Row = MakeParameterRow(Parameter.Name, -1);
                AddRow(ACategoryItem, Row.Name, False, Row);
              end;
            end;
          end;
        end;
      finally
        AddedNames.Free;
      end;
    end else begin
      for var I := 0 to PrimaryEntry.Count-1 do begin
        const Parameter = PrimaryEntry.Parameters[I];
        if Parameter.Kind = pkParameter then begin
          var Definition: TMemberDefinition;
          if not PrimaryEntry.TryGetDefinition(Parameter.Name, Definition) and
             NameMatchesFilter(Parameter.Name) then begin
            if ACategoryItem = nil then
              ACategoryItem := NewCategory(ACategoryName);
            const Row = MakeParameterRow(Parameter.Name, I);
            AddRow(ACategoryItem, Row.Name, False, Row);
          end;
        end;
      end;
    end;
  end;

  procedure AddParameterSectionEntryRows;
  begin
    const PrimaryEntry = FLiveParameterSectionEntries.PrimaryEntry;
    const SectionName = SectionToSectionName(FLiveParameterSectionEntries.Section);

    { Every row belongs to a category: the known parameters are added in
      metadata order, each under its own category, and the parameters which are
      present but unknown in script order, all under the unknown category }
    for var CategoryName in FCategoryNamesInDisplayOrder do begin
      var CategoryItem: TJvCustomInspectorItem := nil;
      if PrimaryEntry.Metadata <> nil then begin
        for var Definition in PrimaryEntry.Metadata.Members do begin
          if Definition.Obsolete and
             not FLiveParameterSectionEntries.MemberPresent(Definition.Name, -1) then
            Continue; { Hide obsolete and unspecified }
          if not DefinitionMatchesFilter(Definition) then
            Continue;
          if SameText(GetScriptCategory(SectionName, Definition.Name, True,
               Definition.Obsolete), CategoryName) then begin
            if CategoryItem = nil then
              CategoryItem := NewCategory(CategoryName);
            AddParameterOccurrenceRows(CategoryItem, Definition);
          end;
        end;
      end;
      if IsScriptUnknownCategoryName(CategoryName) then
        AddUnknownParameterRows(CategoryName, CategoryItem);
    end;
  end;

  procedure AddKeyValueSectionRows;
  begin
    const Section = FLiveKeyValueSection.Section;

    var LineWillBeShown: TArray<Boolean>;
    SetLength(LineWillBeShown, Section.Count);

    const KeyRowsToShow = TList<TInspectorRow>.Create;
    try
      { First determine the keys to show and their order: with
        ShowAllKnownDirectives, show every known directive in metadata
        order. A repeated key gets a row per line. }
      if FShowAllKnownDirectives and (Section.Metadata <> nil) then begin
        for var Definition in Section.Metadata.Members do begin
          var Found := False;
          for var I := 0 to Section.Count-1 do begin
            if (Section.Lines[I].Kind = lkKeyValue) and
               SameText(Section.Lines[I].Name, Definition.Name) then begin
              KeyRowsToShow.Add(MakeKeyRow(Definition.Name, I));
              LineWillBeShown[I] := True;
              Found := True;
            end;
          end;
          { An unspecified key gets a row showing the compiler default,
            unless it is obsolete or another occurrence of the section might
            set it. Other occurrences aren't parsed/live so we can't check. }
          if not Found and not Definition.Obsolete and
             not FLiveKeyValueSectionHasSiblingOccurrences then
            KeyRowsToShow.Add(MakeKeyRow(Definition.Name, -1));
        end;
      end;

      { The remaining keys, in script order }
      for var I := 0 to Section.Count-1 do begin
        if (Section.Lines[I].Kind = lkKeyValue) and not LineWillBeShown[I] then begin
          var Name := Section.Lines[I].Name;
          if not FShowAllKnownDirectives then
            Name := CleanKeyName(Name); { Could be known key, make clean if needed }
          KeyRowsToShow.Add(MakeKeyRow(Name, I));
        end;
      end;

      { Determination done. Add by category the same way as entry rows are,
        with every row belonging to a category, in the order determined above. }
      for var CategoryName in FCategoryNamesInDisplayOrder do begin
        var CategoryItem: TJvCustomInspectorItem := nil;
        for var Row in KeyRowsToShow do begin
          if not KeyRowMatchesFilter(Row) then
            Continue;
          var Definition: TMemberDefinition;
          const Known = Section.TryGetDefinition(Row.Name, Definition);
          if SameText(GetScriptCategory(FLiveKeyValueSectionName, Row.Name, Known,
               Known and Definition.Obsolete), CategoryName) then begin
            if CategoryItem = nil then
              CategoryItem := NewCategory(CategoryName);
            AddKeyRow(CategoryItem, Row);
          end;
        end;
      end;
    finally
      KeyRowsToShow.Free;
    end;
  end;

  function FindItemByID(const AID: String; const AIDIncludesIndex: Boolean;
    const AParent: TJvCustomInspectorItem): TJvCustomInspectorItem;
  begin
    Result := nil;
    for var I := 0 to AParent.Count-1 do begin
      const Item = AParent.Items[I];
      if ItemID(Item, AIDIncludesIndex) = AID then
        Exit(Item);
      Result := FindItemByID(AID, AIDIncludesIndex, Item);
      if Result <> nil then
        Exit;
    end;
  end;

  procedure ExpandParentsKeptByChildMatch(const AParent: TJvCustomInspectorItem);
  { Make sure the category and Flags parent row are expanded when a row or
    flag was found through the filter }
  begin
    for var I := 0 to AParent.Count-1 do begin
      const Item = AParent.Items[I];
      if Item.Count > 0 then begin
        if (Item is TJvInspectorCustomCategoryItem) or
           not NameMatchesFilter(Item.DisplayName) then
          Item.Expanded := True;
        ExpandParentsKeptByChildMatch(Item);
      end;
    end;
  end;

  procedure RebuildRows;
  { Items must not be added, removed, expanded or collapsed while an in-place
    edit is open: JvInspector's RebuildVisible can then reselect the wrong
    item and break the edit. Safe here because Clear ends the edit before the
    items change. }
  begin
    var SelectedIDWithIndex := '';
    var SelectedIDWithoutIndex := '';
    if FJvInspector.Selected <> nil then begin
      SelectedIDWithIndex := ItemID(FJvInspector.Selected, True);
      SelectedIDWithoutIndex := ItemID(FJvInspector.Selected, False);
    end;

    FJvInspector.BeginUpdate;
    try
      const ExpandedStates = TDictionary<String, Boolean>.Create;
      try
        SaveExpandedStates(ExpandedStates, FJvInspector.Root);
        FJvInspector.Clear;
        FRows.Clear;

        {$IFDEF DEBUG}
        const DebugCategory = NewCategory('Debug');
        AddDebugRow(DebugCategory, 'Status', irkDebugStatus);
        AddDebugRow(DebugCategory, 'Sections', irkDebugSections);
        AddDebugRow(DebugCategory, 'Early exits', irkDebugEarlyExits);
        AddDebugRow(DebugCategory, 'Caret at', irkDebugCaretAt);
        {$ENDIF}

        if FLiveParameterSectionEntries <> nil then
          AddParameterSectionEntryRows
        else if FLiveKeyValueSection <> nil then
          AddKeyValueSectionRows;

        RestoreExpandedStates(ExpandedStates, FJvInspector.Root);
        if FFilterText <> '' then
          ExpandParentsKeptByChildMatch(FJvInspector.Root);
      finally
        ExpandedStates.Free;
      end;
    finally
      FJvInspector.EndUpdate;
    end;

    if SelectedIDWithIndex <> '' then begin
      { Restore selection: prefer the id with the row index so it always reselects
        the correct one if there are duplicated member names, but fall back to the
        one without: an edit may have shifted the index }
      var Item := FindItemByID(SelectedIDWithIndex, True, FJvInspector.Root);
      if Item = nil then
        Item := FindItemByID(SelectedIDWithoutIndex, False, FJvInspector.Root);
      FJvInspector.Selected := Item;
      { Also restore marker if it's at selection }
      const SelectedItem = FJvInspector.Selected;
      var Row: TInspectorRow;
      if (SelectedItem <> nil) and TryGetRow(SelectedItem, Row) and RowMatchesCaretAt(Row) then
        FJvInspector.MarkedItem := SelectedItem
      else if FCaretAt.Valid and (SelectedItem = nil) then { See UpdateCaretAt for same check and solution }
        ApplyCaretAtTimerUpdate(False);
    end;
  end;

  function GetCaretAt: TCaretAt;
  begin
    Result.Valid := False;
    const CaretLine = FFactory.Memo.CaretLine;
    if (FLiveParameterSectionEntries <> nil) and FLiveParameterSectionEntries.Valid then begin
      const Memo = FFactory.Memo;
      const CaretCharIndex = Memo.GetCodeUnitCount(
        Memo.GetPositionFromLine(CaretLine), Memo.CaretPosition);
      const MultiEntry = FLiveParameterSectionEntries.Count > 1;
      for var I := 0 to FLiveParameterSectionEntries.Count-1 do begin
        const LiveEntry = FLiveParameterSectionEntries.Entries[I];
        if (CaretLine >= LiveEntry.FirstLine) and
           (CaretLine <= LiveEntry.LastLine) then begin
          var Index: Integer;
          if LiveEntry.Entry.TryGetParameterIndex(CaretLine - LiveEntry.FirstLine,
               CaretCharIndex, Index) then begin
            const Parameter = LiveEntry.Entry.Parameters[Index];
            if Parameter.Kind = pkParameter then begin
              Result.Valid := True;
              Result.Kind := cakParameterSectionEntry;
              Result.Name := Parameter.Name;
              if MultiEntry then
                Result.Index := -1 { Rows are addressed by name, see AddParameterOccurrenceRows }
              else
                Result.Index := Index;
            end;
          end;
          Break;
        end;
      end;
    end else if (FLiveKeyValueSection <> nil) and FLiveKeyValueSection.Valid then begin
      const Section = FLiveKeyValueSection.Section;
      var Line := FLiveKeyValueSection.FirstLine;
      if CaretLine >= Line then begin
        for var I := 0 to Section.Count-1 do begin
          Inc(Line, Section.GetLineCount(I));
          if CaretLine < Line then begin
            { This is where the caret is at }
            if Section.Lines[I].Kind = lkKeyValue then begin
              Result.Valid := True;
              Result.Kind := cakKeyValueSection;
              Result.Name := Section.Lines[I].Name;
              Result.Index := I;
            end;
            Exit;
          end;
        end;
      end;
    end;
  end;

  procedure UpdateCaretAt;
  begin
    if not FFollowCaret then
      Exit;
    const CaretAt = GetCaretAt;
    if (CaretAt.Valid <> FCaretAt.Valid) or
       (CaretAt.Valid and
        ((CaretAt.Kind <> FCaretAt.Kind) or
         (CaretAt.Name <> FCaretAt.Name) or
         (CaretAt.Index <> FCaretAt.Index))) then begin
      { The caret moved to a different member (or no member). Update CaretAt and
        queue its application, or cancel any queued. }
      FCaretAt := CaretAt;
      ApplyCaretAtTimerUpdate(not CaretAt.Valid); { Also always clears marker }
    end else if CaretAt.Valid and (FJvInspector.Selected = nil) then begin
      { The caret is still at the member, but there's no selection anymore,
        which also means there's no marker anymore. Reapply it. Because
        there's no selection this doesn't interfere with the user. Useful
        when for example a filter was entered which hid the selected and
        marked item, but then the filter was edited again in a way that
        made the member's item reappear, unselected and unmarked. }
      ApplyCaretAtTimerUpdate(False);
    end;
  end;

begin
  if FInEdit then
    Exit;

  FJvInspector.ReadOnly := FFactory.Memo.ReadOnly;

  const CaretLine = FFactory.Memo.CaretLine;
  var IndividualSelectionLineRanges: TArray<TScintLineRange>;
  const SelectionLineRanges = FFactory.Memo.GetSelectionLineRanges(
    IndividualSelectionLineRanges);
  { If the selection covers several lines now, or did when the live objects
    were created, the caret no longer tells whether the objects are stale, so
    the early exits below must compare the selection instead. The caret line
    is still compared as well: it decides the fallback to single-entry
    editing when the selection doesn't actually contain entries, and it can
    change while both range sets stay identical, for example because
    GetSelectionLineRange drops the end line when a selection ends at the
    start of it. }
  const UseSelectionTest = LineRangesCoverMultipleLines(SelectionLineRanges) or
    LineRangesCoverMultipleLines(FSelectionLineRangesAtCreation);
  const SelectionTestPassed = UseSelectionTest and
    LineRangesEqual(SelectionLineRanges, FSelectionLineRangesAtCreation) and
    LineRangesEqual(IndividualSelectionLineRanges, FIndividualSelectionLineRangesAtCreation) and
    (CaretLine = FCaretLineAtCreation);

  { Without a memo change or a forced rebuild, a caret move within the same
    entry or key/value section, or an unchanged multi-line selection, changes
    nothing, so keep the model and the rows.
    The signature check must precede LiveObjectTextChanged: right after
    SetActiveFactory the live object still belongs to the previous factory. }
  if (FLiveParameterSectionEntries <> nil) and FLiveParameterSectionEntries.Valid and
     (FRowSetSignature <> '') and not LiveObjectTextChanged and
     (SelectionTestPassed or
      (not UseSelectionTest and
       (CaretLine >= FLiveParameterSectionEntries.PrimaryFirstLine) and
       (CaretLine <= FLiveParameterSectionEntries.PrimaryLastLine))) then begin
    UpdateCaretAt;
    {$IFDEF DEBUG}
    Inc(FUpdateFromCaretEarlyExitCount);
    InvalidateChangedRows; { Repaint the early exit count }
    {$ENDIF}
    Exit;
  end;
  if (FLiveKeyValueSection <> nil) and FLiveKeyValueSection.Valid and
     (FRowSetSignature <> '') and not LiveObjectTextChanged and
     (SelectionTestPassed or not UseSelectionTest) then begin
    { Resolved by section index instead of the entry's line-range test above:
      the section's range covers the body only, so it misses the header line }
    var SectionIndex: Integer;
    if FFactory.TryGetSectionAtLine(CaretLine, SectionIndex) and
       (SectionIndex = FLiveKeyValueSectionIndex) then begin
      UpdateCaretAt;
      {$IFDEF DEBUG}
      Inc(FUpdateFromCaretEarlyExitCount);
      InvalidateChangedRows; { See above }
      {$ENDIF}
      Exit;
    end;
  end;

  FreeAndNil(FLiveParameterSectionEntries);
  FreeAndNil(FLiveKeyValueSection);
  {$IFDEF DEBUG}
  FUpdateFromCaretEarlyExitCount := 0;
  {$ENDIF}

  { Build row set signature for the selected entry or section }
  var RowSetSignature: String; { The actual value this gets doesn't matter, as long as it's unique for any unique row set }
  var Entries: TLiveScriptParameterSectionEntries;
  var EntryRefusalReason: TRefusalReason;
  if FFactory.TryCreateParameterSectionEntries(SelectionLineRanges,
       IndividualSelectionLineRanges, CaretLine, Entries, EntryRefusalReason) then begin
    FLiveParameterSectionEntries := Entries;
    FChangeCountAtCreation := FFactory.ChangeCount;
    FSelectionLineRangesAtCreation := SelectionLineRanges;
    FIndividualSelectionLineRangesAtCreation := IndividualSelectionLineRanges;
    FCaretLineAtCreation := CaretLine;
    FLiveParameterSectionEntries.QuoteNewValues := FQuoteNewParameterValues;
    const SectionName = SectionToSectionName(FLiveParameterSectionEntries.Section);
    {$IFDEF DEBUG}
    UpdateDebugStatusRowStringForParameterSectionEntries(SectionName);
    {$ENDIF}
    { Rows address parameters by index, so the signature includes each entry's
      indexes, with '@' separating the entries }
    RowSetSignature := 'E|' + SectionName;
    for var I := 0 to FLiveParameterSectionEntries.Count-1 do begin
      RowSetSignature := RowSetSignature + '|@';
      const Entry = FLiveParameterSectionEntries.Entries[I].Entry;
      for var J := 0 to Entry.Count-1 do begin
        const Parameter = Entry.Parameters[J];
        if Parameter.Kind = pkParameter then
          RowSetSignature := RowSetSignature + '|' + IntToStr(J) + ':' + Parameter.Name;
      end;
    end;
  end else begin
    var SectionIndex: Integer;
    var KeyValueSection: TLiveScriptKeyValueSection;
    var SectionRefusalReason: TRefusalReason;
    { A mixed selection is refused outright: the caret's section must not be
      inspected instead }
    if (EntryRefusalReason <> rrMixedSelection) and
       FFactory.TryGetSectionAtLine(CaretLine, SectionIndex) and
       FFactory.TryCreateKeyValueSection(SectionIndex, KeyValueSection,
         SectionRefusalReason) then begin
      const Header = FFactory.SectionHeaders[SectionIndex];
      FLiveKeyValueSection := KeyValueSection;
      FLiveKeyValueSectionIndex := SectionIndex;
      FChangeCountAtCreation := FFactory.ChangeCount;
      FSelectionLineRangesAtCreation := SelectionLineRanges;
      FIndividualSelectionLineRangesAtCreation := IndividualSelectionLineRanges;
      FCaretLineAtCreation := CaretLine;
      FLiveKeyValueSectionName := Header.Name;
      FLiveKeyValueSectionIsDirectiveSection := Header.Section in DirectiveSections;
      FLiveKeyValueSection.QuoteNewValues := FQuoteNewDirectiveValues and
        FLiveKeyValueSectionIsDirectiveSection;
      {$IFDEF DEBUG}
      FDebugStatusRowString := Format('[%s] section at line %d',
        [Header.Name, Header.Line+1]);
      {$ENDIF}
      var OccurrenceIndex, OccurrenceCount: Integer;
      FFactory.GetSectionOccurrence(SectionIndex, OccurrenceIndex, OccurrenceCount);
      FLiveKeyValueSectionHasSiblingOccurrences := OccurrenceCount > 1;
      {$IFDEF DEBUG}
      if FLiveKeyValueSectionHasSiblingOccurrences then
        FDebugStatusRowString := FDebugStatusRowString + Format(' (occurrence %d of %d)',
          [OccurrenceIndex, OccurrenceCount]);
      {$ENDIF}
      { Like the entry signature above, plus the occurrence count and
        whether unspecified known directives are offered, which also
        decide the row set }
      RowSetSignature := 'D|' + IntToStr(OccurrenceCount) + '|' +
        IntToStr(Ord(FShowAllKnownDirectives)) + '|' + Header.Name;
      const Model = FLiveKeyValueSection.Section;
      for var I := 0 to Model.Count-1 do begin
        if Model.Lines[I].Kind = lkKeyValue then begin
          RowSetSignature := RowSetSignature + '|' + IntToStr(I) + ':' + Model.Lines[I].Name;
          { Put AddKeyRow's decision into the structure }
          var Definition: TMemberDefinition;
          if Model.TryGetDefinition(Model.Lines[I].Name, Definition) and
             KeyRowIsCheckBox(Definition, I) then
            RowSetSignature := RowSetSignature + '!';
        end;
      end;
    end else begin
      { Prefer the entry refusal }
      FSelectionLineRangesAtCreation := [];
      FIndividualSelectionLineRangesAtCreation := [];
      FCaretLineAtCreation := -1;
      FMixedSelection := EntryRefusalReason = rrMixedSelection;
      {$IFDEF DEBUG}
      FDebugStatusRowString := RefusalReasonToString(EntryRefusalReason);
      {$ENDIF}
      RowSetSignature := 'N|' + IntToStr(Ord(EntryRefusalReason));
    end;
  end;

  UpdateCaretAt;

  { Re-sync any open in-place editor. Done before any rebuild: RebuildRows'
    Clear deselects, and a deselect applies a stale editor's text back over
    the memo edit unless the editor was re-synced first }
  FJvInspector.ResyncEditor;

  if RowSetSignature <> FRowSetSignature then begin
    { Row set changes, need to rebuild the inspector's rows }
    FRowSetSignature := RowSetSignature;
    RebuildRows;
    FJvInspector.Invalidate;
  end else begin
    { Row set stayed same, just need to invalidate to show updated values }
    InvalidateChangedRows;
  end;

  UpdateNote;
end;

function TInspector.RowMatchesCaretAt(const ARow: TInspectorRow): Boolean;
const
  RowKindForCaretAtKind: array [TCaretAtKind] of TInspectorRowKind =
    (irkParameter, irkKey);
begin
  Result := FCaretAt.Valid and
    (ARow.Kind = RowKindForCaretAtKind[FCaretAt.Kind]) and
    (ARow.Index = FCaretAt.Index) and
    SameText(ARow.Name, FCaretAt.Name); { TInspectorRow uses clean names for known members, TCaretAt always uses names as in the script }
end;

procedure TInspector.ApplyCaretAtTimerUpdate(const ACancel: Boolean);
{ Always clears the marker first }
const
  ApplyCaretAtTimerInterval = 100;
begin
  FJvInspector.MarkedItem := nil;
  if ACancel then
    KillTimer(FMessagesWnd, ApplyCaretAtTimerID)
  else
    SetTimer(FMessagesWnd, ApplyCaretAtTimerID, ApplyCaretAtTimerInterval, nil);
end;

procedure TInspector.ApplyCaretAt;

  function FindCaretAtItem(const AParent: TJvCustomInspectorItem): TJvCustomInspectorItem;
  begin
    Result := nil;
    for var I := 0 to AParent.Count-1 do begin
      const Item = AParent.Items[I];
      var Row: TInspectorRow;
      if TryGetRow(Item, Row) then begin
        if RowMatchesCaretAt(Row) then
          Exit(Item);
      end else begin
        { A category: find inside }
        Result := FindCaretAtItem(Item);
        if Result <> nil then
          Exit;
      end;
    end;
  end;

  function SelectedIsItemOrDescendant(const AItem: TJvCustomInspectorItem): Boolean;
  begin
    Result := False;
    { Move up from the selection towards the root, looking for AItem }
    var SelectedOrAncestor := FJvInspector.Selected;
    while SelectedOrAncestor <> nil do begin
      if SelectedOrAncestor = AItem then
        Exit(True);
      SelectedOrAncestor := SelectedOrAncestor.Parent;
    end;
  end;

begin
  if not FFollowCaret or not FCaretAt.Valid or FJvInspector.Focused or FInEdit then
    Exit;

  const Item = FindCaretAtItem(FJvInspector.Root);

  if Item <> nil then begin
    if SelectedIsItemOrDescendant(Item) then begin
      { The selection already belongs to the caret's member, so leave it in
        place and only add the marker if the member's own row is selected }
      if FJvInspector.Selected = Item then
        FJvInspector.MarkedItem := Item;
    end else begin
      { Ensure visibility first }
      var Parent := Item.Parent;
      while (Parent <> nil) and (Parent <> FJvInspector.Root) do begin
        Parent.Expanded := True;
        Parent := Parent.Parent;
      end;
      { Scroll, select and mark }
      Item.ScrollInView(True);
      FJvInspector.Selected := Item;
      FJvInspector.MarkedItem := Item;
    end;
  end;
end;

procedure TInspector.UpdateReadOnly;
begin
  FJvInspector.ReadOnly := FFactory.Memo.ReadOnly;
end;

function TInspector.TryGetRow(const AItem: TJvCustomInspectorItem;
  out ARow: TInspectorRow): Boolean;
begin
  const Index = AItem.Tag-1;
  Result := (Index >= 0) and (Index < FRows.Count);
  if Result then
    ARow := FRows[Index];
end;

function TInspector.GetRowValueSignature(const ARow: TInspectorRow): String;
begin
  if ARow.CheckBox then
    Result := IntToStr(RowGetAsOrdinal(ARow))
  else
    Result := RowGetAsString(ARow);
end;

function TInspector.RowGetAsOrdinal(const ARow: TInspectorRow): Int64;
begin
  Result := 0;
  case ARow.Kind of
    irkParameterFlag:
      if FLiveParameterSectionEntries <> nil then begin
        case FLiveParameterSectionEntries.GetFlagCheckState(ARow.Name, ARow.Index,
               ARow.FlagName) of
          fcsSome: Result := TJvInspectorBooleanItem.IndeterminateOrdinal;
          fcsAll: Result := 1;
        end;
      end;
    irkKey:
      if (FLiveKeyValueSection <> nil) and FLiveKeyValueSection.Valid then begin
        if FLiveKeyValueSection.MemberPresent(ARow.Name, ARow.Index) then begin
          var BoolValue := False;
          if TryStrToBoolean(FLiveKeyValueSection.GetValue(ARow.Name, ARow.Index),
               BoolValue) and BoolValue then
            Result := 1;
        end else if SameText(FLiveKeyValueSection.Section.DefaultValue(ARow.Name),
                      SYes) then
          Result := 1;
      end;
    irkKeyFlag:
      if (FLiveKeyValueSection <> nil) and FLiveKeyValueSection.Valid then begin
        if FLiveKeyValueSection.MemberPresent(ARow.Name, ARow.Index) then begin
          if FLiveKeyValueSection.GetFlagCheckState(ARow.Name, ARow.Index,
               ARow.FlagName) = fcsAll then
            Result := 1;
        end else if ScriptValueIncludesFlag(
                      FLiveKeyValueSection.Section.DefaultValue(ARow.Name),
                      ARow.FlagName) then
          Result := 1; { Not present in the script: show the compiler default }
      end;
  end;
end;

procedure TInspector.RowGetAsOrdinal(Sender: TJvCustomInspectorItem;
  var Value: Int64);
begin
  Value := 0;
  var Row: TInspectorRow;
  if TryGetRow(Sender, Row) then
    Value := RowGetAsOrdinal(Row);
end;

function TInspector.RowGetAsString(const ARow: TInspectorRow): String;
begin
  Result := '';
  case ARow.Kind of
    irkParameter:
      { A parameter not present in the script, or without a common value,
        shows empty }
      if FLiveParameterSectionEntries <> nil then
        Result := FLiveParameterSectionEntries.GetValue(ARow.Name, ARow.Index);
    irkKey:
      if (FLiveKeyValueSection <> nil) and FLiveKeyValueSection.Valid then begin
        if FLiveKeyValueSection.MemberPresent(ARow.Name, ARow.Index) then
          Result := FLiveKeyValueSection.GetValue(ARow.Name, ARow.Index)
        else
          Result := FLiveKeyValueSection.Section.DefaultValue(ARow.Name); { Not present in the script: show the compiler default }
      end;
    {$IFDEF DEBUG}
    irkDebugStatus:
      Result := FDebugStatusRowString;
    irkDebugSections:
      begin
        for var I := 0 to FFactory.SectionCount-1 do begin
          const Header = FFactory.SectionHeaders[I];
          if Result <> '' then
            Result := Result + ', ';
          Result := Result + Header.Name + '@' + IntToStr(Header.Line+1);
        end;
      end;
    irkDebugEarlyExits:
      Result := IntToStr(FUpdateFromCaretEarlyExitCount);
    irkDebugCaretAt:
      if FCaretAt.Valid then
        Result := FCaretAt.Name + '@' + IntToStr(FCaretAt.Index)
      else
        Result := 'None';
    {$ENDIF}
  end;
end;

procedure TInspector.RowGetAsString(Sender: TJvCustomInspectorItem;
  var Value: String);
begin
  Value := '';
  var Row: TInspectorRow;
  if TryGetRow(Sender, Row) then
    Value := RowGetAsString(Row);
end;

procedure TInspector.RowSetAsOrdinal(Sender: TJvCustomInspectorItem;
  var Value: Int64);
begin
  var Row: TInspectorRow;
  if not TryGetRow(Sender, Row) then
    raise Exception.Create('Internal error: RowSetAsOrdinal: unknown row');
  if FFactory.Memo.ReadOnly then
    raise Exception.Create(LFmtMessage(SInspectorReadOnlyError));
  FInEdit := True;
  try
    case Row.Kind of
      irkParameterFlag:
        { May adjust related flags as well }
        if FLiveParameterSectionEntries <> nil then
          FLiveParameterSectionEntries.SetFlag(Row.Name, Row.Index,
            Row.FlagName, Value <> 0);
      irkKey:
        if FLiveKeyValueSection <> nil then begin
          var NewValue := SNo;
          if Value <> 0 then
            NewValue := SYes;
          FLiveKeyValueSection.SetValue(Row.Name, Row.Index, NewValue);
        end;
      irkKeyFlag:
        { May adjust related flags as well }
        if FLiveKeyValueSection <> nil then
          FLiveKeyValueSection.SetFlag(Row.Name, Row.Index, Row.FlagName,
            Value <> 0);
    else
      raise Exception.Create('Internal error: RowSetAsOrdinal: unexpected row kind');
    end;
  finally
    FInEdit := False;
  end;
  InvalidateChangedRows;
end;

procedure TInspector.RowSetAsString(Sender: TJvCustomInspectorItem;
  var Value: String);
{ Runs inside the in-place editor's Apply: must not (indirectly) change the
  inspector's selection or end the edit. Setting FInEdit below makes
  UpdateFromCaret exit early while the memo is being changed. }

  procedure ValidateValue(const ARowName, AValue: String;
    const ADefinition: TMemberDefinition);
  begin
    if (AValue <> '') and (Pos('{', AValue) = 0) and
       (ADefinition.ValueKind = mvkInteger) then begin
      { Validate if the value is a valid integer. Strips underscore digit
        separators because the compiler accepts them for some values. }
      var IntegerValue: Int64;
      if not TryStrToInt64(StringReplace(AValue, '_', '', [rfReplaceAll]), IntegerValue) then
        raise EInspectorValueRejected.Create(LFmtMessage(SInspectorIntegerValueError, [ARowName]));
    end;
  end;

begin
  var Row: TInspectorRow;
  if not TryGetRow(Sender, Row) then
    raise Exception.Create('Internal error: RowSetAsString: unknown row');
  if FFactory.Memo.ReadOnly then
    raise Exception.Create(LFmtMessage(SInspectorReadOnlyError));
  FInEdit := True;
  try
    case Row.Kind of
      irkParameter:
        if (FLiveParameterSectionEntries <> nil) and
           FLiveParameterSectionEntries.Valid then begin
          var Definition: TMemberDefinition;
          if FLiveParameterSectionEntries.PrimaryEntry.TryGetDefinition(Row.Name, Definition) then
            ValidateValue(Row.Name, Value, Definition);
          FLiveParameterSectionEntries.SetValue(Row.Name, Row.Index, Value);
        end;
      irkKey:
        if (FLiveKeyValueSection <> nil) and FLiveKeyValueSection.Valid then begin
          var Definition: TMemberDefinition;
          if FLiveKeyValueSection.Section.TryGetDefinition(Row.Name, Definition) then
            ValidateValue(Row.Name, Value, Definition);
          FLiveKeyValueSection.SetValue(Row.Name, Row.Index, Value);
        end;
    else
      raise Exception.Create('Internal error: RowSetAsString: unexpected row kind');
    end;
  finally
    FInEdit := False;
  end;
  InvalidateChangedRows;
end;

procedure TInspector.RowRemove(const ARow: TInspectorRow);
begin
  FInEdit := True; { See RowSetAsString }
  try
    case ARow.Kind of
      irkParameter, irkParameterFlag:
        FLiveParameterSectionEntries.Remove(ARow.Name, ARow.Index);
      irkKey, irkKeyFlag:
        FLiveKeyValueSection.Remove(ARow.Name, ARow.Index);
    end;
  finally
    FInEdit := False;
  end;
  UpdateFromCaret;
end;

class procedure TInspector.SortValueList(var AValues: TArray<String>);
{ Sorts using same sort as autocompletion and Scintilla, so using CompareText.
  Also see BuildAutoCompleteWordList. }
begin
  TArray.Sort<String>(AValues, TComparer<String>.Construct(
    function(const A, B: String): Integer
    begin
      Result := CompareText(A, B);
    end));
end;

procedure TInspector.ChoiceRowGetValueList(Item: TJvCustomInspectorItem;
  Values: TStrings);
begin
  var Row: TInspectorRow;
  if not TryGetRow(Item, Row) then
    Exit;
  var Definition: TMemberDefinition;
  if (FLiveParameterSectionEntries <> nil) and FLiveParameterSectionEntries.Valid then begin
    if not FLiveParameterSectionEntries.PrimaryEntry.TryGetDefinition(Row.Name, Definition) then
      raise Exception.Create('Internal error: ChoiceRowGetValueList: unknown parameter');
  end else if (FLiveKeyValueSection <> nil) and FLiveKeyValueSection.Valid then begin
    if not FLiveKeyValueSection.Section.TryGetDefinition(Row.Name, Definition) then
      raise Exception.Create('Internal error: ChoiceRowGetValueList: unknown key');
  end else
    Exit;
  var KnownValues := Copy(Definition.KnownValues);
  SortValueList(KnownValues);
  Values.AddStrings(KnownValues);
end;

procedure TInspector.DestDirRowGetValueList(Item: TJvCustomInspectorItem;
  Values: TStrings);
begin
  var CommonValues := Copy(FilesDestDirCommonValues);
  SortValueList(CommonValues);
  Values.AddStrings(CommonValues);
end;

procedure TInspector.PermissionsRowGetValueList(Item: TJvCustomInspectorItem;
  Values: TStrings);

  function SwapAccessTypesIfNeeded(const AValue: String;
    const AKnownValues: TArray<String>): String;
  { Switches '-read' to '-readexec', or vice versa, as needed }
  const
    ReadAccessType = '-read';
    ReadExecAccessType = '-readexec';
  begin
    { Figure out which type to use }
    var UseReadExec := False;
    for var KnownValue in AKnownValues do begin
      if PathEndsWith(KnownValue, ReadExecAccessType) then begin
        UseReadExec := True;
        Break;
      end;
    end;
    { Replace as needed }
    Result := '';
    var S := AValue;
    while True do begin
      var P := ExtractStr(S, ' ');
      if P = '' then
        Break;
      if UseReadExec and PathEndsWith(P, ReadAccessType) then
        P := Copy(P, 1, Length(P)-Length(ReadAccessType)) + ReadExecAccessType
      else if not UseReadExec and PathEndsWith(P, ReadExecAccessType) then
        P := Copy(P, 1, Length(P)-Length(ReadExecAccessType)) + ReadAccessType;
      if Result = '' then
        Result := P
      else
        Result := Result + ' ' + P;
    end;
  end;

begin
  var Row: TInspectorRow;
  if not TryGetRow(Item, Row) then
    Exit;
  var Definition: TMemberDefinition;
  if (FLiveParameterSectionEntries <> nil) and FLiveParameterSectionEntries.Valid then begin
    if not FLiveParameterSectionEntries.PrimaryEntry.TryGetDefinition(Row.Name, Definition) then
      raise Exception.Create('Internal error: PermissionsRowGetValueList: unknown parameter');
  end else
    Exit;
  var MainFactory: TLiveScriptObjectFactory := nil;
  if Assigned(FOnGetMainFactory) then
    MainFactory := FOnGetMainFactory;
  const SL = TStringList.Create;
  try
    SL.CaseSensitive := False;
    SL.UseLocale := False; { Make sure it uses CompareText and not AnsiCompareText }
    SL.Duplicates := dupIgnore; { Also removes the duplicates a swapped access type can cause }
    SL.Sorted := True;
    SL.AddStrings(Definition.KnownValues);
    for var Value in CollectParameterValuesFromFactories([FFactory, MainFactory],
       Row.Name) do
      SL.Add(SwapAccessTypesIfNeeded(Value, Definition.KnownValues));
    Values.AddStrings(SL);
  finally
    SL.Free;
  end;
end;

procedure TInspector.SignToolRowGetValueList(Item: TJvCustomInspectorItem;
  Values: TStrings);
begin
  if not Assigned(FOnGetSignTools) then
    Exit;
  const SignTools = FOnGetSignTools;
  var SignToolNames: TArray<String>;
  SetLength(SignToolNames, SignTools.Count);
  for var I := 0 to SignTools.Count-1 do
    SignToolNames[I] := SignTools.Names[I];
  SortValueList(SignToolNames);
  Values.AddStrings(SignToolNames);
end;

procedure TInspector.ScriptValuesRowGetValueList(Item: TJvCustomInspectorItem;
  Values: TStrings);
begin
  var Row: TInspectorRow;
  if not TryGetRow(Item, Row) or
     (GetScriptSectionDefiningParameterValues(Row.Name) = scNone) then
    Exit;
  var MainFactory: TLiveScriptObjectFactory := nil;
  if Assigned(FOnGetMainFactory) then
    MainFactory := FOnGetMainFactory;
  const SortedValues = CollectParameterValuesFromFactories([FFactory, MainFactory],
    Row.Name);
  Values.AddStrings(SortedValues);
end;

function TInspector.GetDividerWidth: Integer;
begin
  Result := FJvInspector.Divider;
end;

procedure TInspector.SetDividerWidth(const Value: Integer);
begin
  FJvInspector.Divider := Value;
end;

procedure TInspector.SetQuoteNewDirectiveValues(const Value: Boolean);
begin
  FQuoteNewDirectiveValues := Value;
  if ShowingDirectiveSection then
    FLiveKeyValueSection.QuoteNewValues := Value;
end;

procedure TInspector.SetQuoteNewParameterValues(const Value: Boolean);
begin
  FQuoteNewParameterValues := Value;
  if ShowingParameterSectionEntry then
    FLiveParameterSectionEntries.QuoteNewValues := Value;
end;

procedure TInspector.SetFilterText(const Value: String);
begin
  if Value <> FFilterText then begin
    FFilterText := Value;
    FRowSetSignature := ''; { Force a rebuild, see UpdateFromCaret's early exit }
    UpdateFromCaret;
  end;
end;

procedure TInspector.SetFollowCaret(const Value: Boolean);
begin
  if Value <> FFollowCaret then begin
    FFollowCaret := Value;
    FCaretAt.Valid := False;
    if Value then
      UpdateFromCaret
    else
      ApplyCaretAtTimerUpdate(True); { Cancel any queued }
  end;
end;

procedure TInspector.SetShowAllKnownDirectives(const Value: Boolean);
begin
  if Value <> FShowAllKnownDirectives then begin
    FShowAllKnownDirectives := Value;
    FRowSetSignature := ''; { Force a rebuild, see UpdateFromCaret's early exit }
    UpdateFromCaret;
  end;
end;

procedure TInspector.SetShowAllKnownDirectivesSuppressedNote(const Value: Boolean);
begin
  if Value <> FShowAllKnownDirectivesSuppressedNote then begin
    FShowAllKnownDirectivesSuppressedNote := Value;
    UpdateNote;
  end;
end;

procedure TInspector.UpdateTheme(const ATheme: TTheme; const AHighContrastActive: Boolean);
begin
  if not AHighContrastActive then begin
    FJvInspector.BackgroundColor := ATheme.Colors[tcBack];
    FJvInspector.NameColor := ATheme.Colors[tcFore];
    FJvInspector.ValueColor := ATheme.Colors[tcFore];
    FJvInspector.CategoryColor := ATheme.Colors[tcToolBack];
    FJvInspector.CategoryTextColor := ATheme.Colors[tcFore];
    FJvInspector.DividerColor := ATheme.Colors[tcToolBack];
    FJvInspector.CategoryDividerColor := FJvInspector.DividerColor;
    FJvInspector.SelectedColor := ATheme.Colors[tcSelBack];
    FJvInspector.SelectedTextColor := ATheme.Colors[tcFore];
    FJvInspector.HideSelectColor := ATheme.Colors[tcToolBack];
    FJvInspector.HideSelectTextColor := ATheme.Colors[tcFore];

    { Calling SetWindowTheme manually because our SetControlWindowTheme
      would remove all VCL styling, but we still need it to theme the
      inspector's in-place editor and dropdown }
    if UseThemes then begin
      if ATheme.Dark then
        SetWindowTheme(FJvInspector.Handle, 'DarkMode_Explorer', nil)
      else
        SetWindowTheme(FJvInspector.Handle, nil, nil);
    end;

    FJvInspector.Invalidate;
  end;
end;

end.
