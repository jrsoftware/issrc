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
  JvInspector, ModernColors, NewStaticText,
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
    Name: String;   { Protects against a stale Index. As in the script, so not cleaned }
    Index: Integer; { Protects against duplicated Name }
  end;

  TInspectorGetBaseDirEvent = function: String of object;

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
      FLiveParameterSectionEntry: TLiveScriptParameterSectionEntry;
      FLiveKeyValueSection: TLiveScriptKeyValueSection;
      FLiveKeyValueSectionName: String;
      FLiveKeyValueSectionIsDirectiveSection: Boolean;
      FLiveKeyValueSectionHasSiblingOccurrences: Boolean;
      FLiveKeyValueSectionIndex: Integer; { Factory section index it was created for }
      FChangeCountAtCreation: Int64; { Factory ChangeCount at the live object's creation }
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
    procedure InvalidateChangedRows;
    function TryGetRow(const AItem: TJvCustomInspectorItem;
      out ARow: TInspectorRow): Boolean;
    function TryGetRowParameterSectionEntry(const ARow: TInspectorRow;
      out AEntry: TScriptModelParameterSectionEntry; out AIndex: Integer): Boolean;
    function TryGetRowKeyValueSection(const ARow: TInspectorRow;
      out ASection: TScriptModelKeyValueSection; out AIndex: Integer): Boolean;
    function GetRowValueSignature(const ARow: TInspectorRow): String;
    function RowGetAsOrdinal(const ARow: TInspectorRow): Int64; overload;
    procedure RowGetAsOrdinal(Sender: TJvCustomInspectorItem; var Value: Int64); overload;
    function RowGetAsString(const ARow: TInspectorRow): String; overload;
    procedure RowGetAsString(Sender: TJvCustomInspectorItem; var Value: String); overload;
    procedure RowSetAsOrdinal(Sender: TJvCustomInspectorItem; var Value: Int64);
    procedure RowSetAsString(Sender: TJvCustomInspectorItem; var Value: String);
    procedure RowRemove(const AEntry: TScriptModelParameterSectionEntry;
      const ASection: TScriptModelKeyValueSection; const AIndex: Integer);
    procedure ChoiceRowGetValueList(Item: TJvCustomInspectorItem; Values: TStrings);
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
      const AOnGetBaseDir: TInspectorGetBaseDirEvent);
    destructor Destroy; override;
    procedure ForceFinishEdit(const AForceCancel: Boolean = False);
    function GetSelectedHelpKeyword: String;
    function TryGetSelectedRowPosition: Boolean; overload;
    function TryGetSelectedRowPosition(out ALine, ACharIndex: Integer): Boolean; overload;
    procedure GoToSelectedRow;
    function TryResolveSelectedRow(out AEntry: TScriptModelParameterSectionEntry;
      out ASection: TScriptModelKeyValueSection; out AIndex: Integer): Boolean; overload;
    function TryResolveSelectedRow: Boolean; overload;
    function CanRemoveSelectedRow: Boolean;
    procedure RemoveSelectedRow;
    function ShowingDirectiveSection: Boolean;
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
  SysUtils, StrUtils, UITypes, Themes, Forms, Generics.Defaults,
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
  const AOnGetBaseDir: TInspectorGetBaseDirEvent);
{ Takes ownership of AJvInspector but not of ANoteText }
begin
  inherited Create;

  FNoteText := ANoteText;
  FFactory := AFactory;
  FOnGetBaseDir := AOnGetBaseDir;
  FShowAllKnownDirectives := AShowAllKnownDirectives;
  FFollowCaret := AFollowCaret;
  {$IFDEF DEBUG}
  FDebugStatusRowString := 'Not updated yet';
  {$ENDIF}
  FMessagesWnd := AllocateHWnd(MessagesWndProc);
  FRows := TList<TInspectorRow>.Create;

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
  FLiveParameterSectionEntry.Free;
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
  if (FLiveParameterSectionEntry = nil) and (FLiveKeyValueSection = nil) then
    ShowNote(LFmtMessage(SInspectorNothingToInspectNote))
  else if ShowingDirectiveSection then begin
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
        begin
          var Entry: TScriptModelParameterSectionEntry;
          var Index: Integer;
          Result := TryGetRowParameterSectionEntry(ARow, Entry, Index);
        end;
      irkParameterFlag:
        begin
          var Entry: TScriptModelParameterSectionEntry;
          var Index: Integer;
          Result := TryGetRowParameterSectionEntry(ARow, Entry, Index) and
            Entry.FlagIncluded(Index, ARow.FlagName);
        end;
      irkKey:
        { Without ShowAllKnownDirectives only directives which are in the
          script get a row, so bold would say nothing }
        if FShowAllKnownDirectives then begin
          var Section: TScriptModelKeyValueSection;
          var Index: Integer;
          Result := TryGetRowKeyValueSection(ARow, Section, Index);
        end;
      irkKeyFlag:
        { See above }
        if FShowAllKnownDirectives then begin
          var Section: TScriptModelKeyValueSection;
          var Index: Integer;
          Result := TryGetRowKeyValueSection(ARow, Section, Index) and
            Section.FlagIncluded(Index, ARow.FlagName);
        end;
    end;
  end;

begin
  var Row: TInspectorRow;
  Result := TryGetRow(AItem, Row) and RowShouldBeBold(Row);
end;

function TInspector.TryGetRowParameterSectionEntry(const ARow: TInspectorRow;
  out AEntry: TScriptModelParameterSectionEntry; out AIndex: Integer): Boolean;
begin
  AEntry := nil;
  AIndex := -1;
  if (FLiveParameterSectionEntry = nil) or not FLiveParameterSectionEntry.Valid then
    Exit(False);
  AEntry := FLiveParameterSectionEntry.Entry;
  AIndex := ARow.Index;
  Result := AEntry.TryResolve(ARow.Name, AIndex);
end;

function TInspector.TryGetRowKeyValueSection(const ARow: TInspectorRow;
  out ASection: TScriptModelKeyValueSection; out AIndex: Integer): Boolean;
begin
  ASection := nil;
  AIndex := -1;
  if (FLiveKeyValueSection = nil) or not FLiveKeyValueSection.Valid then
    Exit(False);
  ASection := FLiveKeyValueSection.Section;
  AIndex := ARow.Index;
  Result := ASection.TryResolve(ARow.Name, AIndex);
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
    for var Key := 0 to 255 do
      if GetAsyncKeyState(Key) < 0 then
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

function TInspector.TryGetSelectedRowPosition: Boolean;
begin
  var Line, CharIndex: Integer;
  Result := TryGetSelectedRowPosition(Line, CharIndex);
end;

function TInspector.TryGetSelectedRowPosition(out ALine,
  ACharIndex: Integer): Boolean;
begin
  ALine := -1;
  ACharIndex := 0; { Stays 0 for keys }
  const Item = FJvInspector.Selected;
  var Row: TInspectorRow;
  if (Item = nil) or not TryGetRow(Item, Row) then
    Exit(False);
  case Row.Kind of
    irkParameter, irkParameterFlag:
      begin
        var Entry: TScriptModelParameterSectionEntry;
        var Index: Integer;
        if TryGetRowParameterSectionEntry(Row, Entry, Index) then begin
          ALine := FLiveParameterSectionEntry.FirstLine;
          var LineIndex, CharIndex: Integer;
          if Entry.TryGetParameterPosition(Index, LineIndex, CharIndex) then begin
            Inc(ALine, LineIndex);
            ACharIndex := CharIndex;
          end;
        end;
      end;
    irkKey, irkKeyFlag:
      begin
        var Section: TScriptModelKeyValueSection;
        var Index: Integer;
        if TryGetRowKeyValueSection(Row, Section, Index) then begin
          var Line := FLiveKeyValueSection.FirstLine;
          for var I := 0 to Index-1 do
            Inc(Line, Section.GetLineCount(I));
          ALine := Line;
        end;
      end;
  end;
  Result := ALine >= 0;
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
      if (FLiveParameterSectionEntry <> nil) and FLiveParameterSectionEntry.Valid then begin
        Known := FLiveParameterSectionEntry.Entry.TryGetDefinition(Row.Name, Definition);
        if Known then
          SectionName := FLiveParameterSectionEntry.Entry.Metadata.SectionName;
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

  { Special: [Files] Source can only be browsed for if not external }
  if SameText(SectionName, 'Files') and SameText(Definition.Name, 'Source') then begin
    const Entry = FLiveParameterSectionEntry.Entry; { Always exists }
    var FlagsIndex := -1;
    if Entry.TryResolve('Flags', FlagsIndex) and
       Entry.FlagIncluded(FlagsIndex, 'external') then begin
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

procedure TInspector.GoToSelectedRow;
begin
  var Line, CharIndex: Integer;
  if TryGetSelectedRowPosition(Line, CharIndex) then begin
    const Memo = FFactory.Memo;
    Memo.CaretPosition := Memo.GetPositionRelativeCodeUnits(
      Memo.GetPositionFromLine(Line), CharIndex);
    Memo.SetFocus;
  end;
end;

function TInspector.TryResolveSelectedRow(
  out AEntry: TScriptModelParameterSectionEntry;
  out ASection: TScriptModelKeyValueSection; out AIndex: Integer): Boolean;
begin
  Result := False;
  AEntry := nil;
  ASection := nil;
  AIndex := -1;
  const Item = FJvInspector.Selected;
  var Row: TInspectorRow;
  if (Item = nil) or not TryGetRow(Item, Row) then
    Exit;
  case Row.Kind of
    irkParameter, irkParameterFlag:
      Result := TryGetRowParameterSectionEntry(Row, AEntry, AIndex);
    irkKey, irkKeyFlag:
      Result := TryGetRowKeyValueSection(Row, ASection, AIndex);
  end;
end;

function TInspector.TryResolveSelectedRow: Boolean;
begin
  var Entry: TScriptModelParameterSectionEntry;
  var Section: TScriptModelKeyValueSection;
  var Index: Integer;
  Result := TryResolveSelectedRow(Entry, Section, Index);
end;

function TInspector.CanRemoveSelectedRow: Boolean;
begin
  Result := not FFactory.Memo.ReadOnly and TryResolveSelectedRow;
end;

procedure TInspector.RemoveSelectedRow;
begin
  var Entry: TScriptModelParameterSectionEntry;
  var Section: TScriptModelKeyValueSection;
  var Index: Integer;
  if FFactory.Memo.ReadOnly or
     not TryResolveSelectedRow(Entry, Section, Index) then
    Exit;

  { Cancel any open in-place edit: a commit could re-add the value being
    removed }
  ForceFinishEdit(True);

  RowRemove(Entry, Section, Index);
end;

function TInspector.ShowingDirectiveSection: Boolean;
begin
  Result := (FLiveKeyValueSection <> nil) and FLiveKeyValueSectionIsDirectiveSection;
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

  function ItemKey(const AItem: TJvCustomInspectorItem;
    const AIncludeIndex: Boolean): String;
  { AIncludeIndex: Include the row index so the key is unique even for duplicated member names }
  begin
    if AItem is TJvInspectorCustomCategoryItem then
      Result := 'C|' + AItem.DisplayName
    else begin
      Result := 'R|' + AItem.DisplayName;
      if AIncludeIndex then begin
        var Row: TInspectorRow;
        if not TryGetRow(AItem, Row) then
          raise Exception.Create('Internal error: ItemKey: Row not found');
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
        AStates.AddOrSetValue(ItemKey(Item, False), Item.Expanded);
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
        if AStates.TryGetValue(ItemKey(Item, False), Expanded) then
          Item.Expanded := Expanded;
        RestoreExpandedStates(AStates, Item);
      end;
    end;
  end;

  function NewCategory(const AName: String): TJvCustomInspectorItem;
  begin
    Result := TJvInspectorCustomCategoryItem.Create(FJvInspector.Root);
    Result.DisplayName := LFmtMessage(AName); { These are localizable, see IDE.Messages }
    Result.Expanded := True;
  end;

  function NameMatchesFilter(const AName: String): Boolean;
  begin
    Result := (FFilterText = '') or ContainsText(AName, FFilterText);
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
      Item.Flags := Item.Flags + [iifEditButton];
  end;

  procedure AddParameterOccurrenceRows(const AParent: TJvCustomInspectorItem;
    const ADefinition: TMemberDefinition);
  begin
    { Normally a parameter will be present only once, but duplicates are still
      handled here, even though that doesn't compile }
    const Entry = FLiveParameterSectionEntry.Entry;
    var Found := False;
    for var I := 0 to Entry.Count-1 do begin
      if (Entry.Parameters[I].Kind = pkParameter) and
         SameText(Entry.Parameters[I].Name, ADefinition.Name) then begin
        AddParameterRow(AParent, ADefinition, I);
        Found := True;
      end;
    end;
    if not Found then
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
          Item.Flags := Item.Flags + [iifEditButton];
      end;
    end;
  end;

  procedure AddParameterSectionEntryRows;
  begin
    const Entry = FLiveParameterSectionEntry.Entry;

    { Known and uncategorized parameters first, in metadata order }
    if Entry.Metadata <> nil then begin
      const SectionName = Entry.Metadata.SectionName;
      for var Definition in Entry.Metadata.Members do begin
        if Definition.Obsolete and not Entry.Has(Definition.Name) then
          Continue; { Hide obsolete and unspecified }
        if not DefinitionMatchesFilter(Definition) then
          Continue;
        var CategoryName: String;
        if not TryGetScriptCategory(SectionName, Definition.Name, CategoryName) then
          AddParameterOccurrenceRows(FJvInspector.Root, Definition);
      end;
    end;

    { Present but unknown parameters }
    for var I := 0 to Entry.Count-1 do begin
      const Parameter = Entry.Parameters[I];
      if Parameter.Kind = pkParameter then begin
        var Definition: TMemberDefinition;
        if not Entry.TryGetDefinition(Parameter.Name, Definition) and
           NameMatchesFilter(Parameter.Name) then begin
          const Row = MakeParameterRow(Parameter.Name, I);
          AddRow(FJvInspector.Root, Row.Name, False, Row);
        end;
      end;
    end;

    { Known and categorized parameters, in metadata order }
    if Entry.Metadata <> nil then begin
      const SectionName = Entry.Metadata.SectionName;
      for var CategoryName in ScriptCategoryNamesOrdered do begin
        var CategoryItem: TJvCustomInspectorItem := nil;
        for var Definition in Entry.Metadata.Members do begin
          if Definition.Obsolete and not Entry.Has(Definition.Name) then
            Continue;
          if not DefinitionMatchesFilter(Definition) then
            Continue;
          var DefinitionCategory: String;
          if TryGetScriptCategory(SectionName, Definition.Name, DefinitionCategory) and
             SameText(DefinitionCategory, CategoryName) then begin
            if CategoryItem = nil then
              CategoryItem := NewCategory(CategoryName);
            AddParameterOccurrenceRows(CategoryItem, Definition);
          end;
        end;
      end;
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

      { Determination done. Add by category the same way as entry rows are. }

      { Uncategorized first, in the order determined above }
      for var Row in KeyRowsToShow do begin
        if not KeyRowMatchesFilter(Row) then
          Continue;
        var CategoryName: String;
        if not TryGetScriptCategory(FLiveKeyValueSectionName, Row.Name, CategoryName) then
          AddKeyRow(FJvInspector.Root, Row);
      end;

      { Categorized keys, also in the order determined above }
      for var CategoryName in ScriptCategoryNamesOrdered do begin
        var CategoryItem: TJvCustomInspectorItem := nil;
        for var Row in KeyRowsToShow do begin
          if not KeyRowMatchesFilter(Row) then
            Continue;
          var KeyCategory: String;
          if TryGetScriptCategory(FLiveKeyValueSectionName, Row.Name, KeyCategory) and
             SameText(KeyCategory, CategoryName) then begin
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

  function FindItemByKey(const AKey: String; const AKeyIncludesIndex: Boolean;
    const AParent: TJvCustomInspectorItem): TJvCustomInspectorItem;
  begin
    Result := nil;
    for var I := 0 to AParent.Count-1 do begin
      const Item = AParent.Items[I];
      if ItemKey(Item, AKeyIncludesIndex) = AKey then
        Exit(Item);
      Result := FindItemByKey(AKey, AKeyIncludesIndex, Item);
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
    var SelectedKeyWithIndex := '';
    var SelectedKeyWithoutIndex := '';
    if FJvInspector.Selected <> nil then begin
      SelectedKeyWithIndex := ItemKey(FJvInspector.Selected, True);
      SelectedKeyWithoutIndex := ItemKey(FJvInspector.Selected, False);
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

        if FLiveParameterSectionEntry <> nil then
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

    if SelectedKeyWithIndex <> '' then begin
      { Restore selection: prefer the key with the row index so it always reselects
        the correct one if there are duplicated member names, but fall back to the
        one without: an edit may have shifted the index }
      var Item := FindItemByKey(SelectedKeyWithIndex, True, FJvInspector.Root);
      if Item = nil then
        Item := FindItemByKey(SelectedKeyWithoutIndex, False, FJvInspector.Root);
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
    if (FLiveParameterSectionEntry <> nil) and FLiveParameterSectionEntry.Valid then begin
      const Memo = FFactory.Memo;
      const CaretCharIndex = Memo.GetCodeUnitCount(
        Memo.GetPositionFromLine(CaretLine), Memo.CaretPosition);
      const Entry = FLiveParameterSectionEntry.Entry;
      var Index: Integer;
      if Entry.TryGetParameterIndexAt(
           CaretLine - FLiveParameterSectionEntry.FirstLine,
           CaretCharIndex, Index) then begin
        const Parameter = Entry.Parameters[Index];
        if Parameter.Kind = pkParameter then begin
          Result.Valid := True;
          Result.Kind := cakParameterSectionEntry;
          Result.Name := Parameter.Name;
          Result.Index := Index;
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

  { Without a memo change or a forced rebuild, a caret move within the same
    entry or key/value section changes nothing, so keep the model and the rows.
    The signature check must precede LiveObjectTextChanged: right after
    SetActiveFactory the live object still belongs to the previous factory. }
  if (FLiveParameterSectionEntry <> nil) and FLiveParameterSectionEntry.Valid and
     (FRowSetSignature <> '') and not LiveObjectTextChanged and
     (CaretLine >= FLiveParameterSectionEntry.FirstLine) and
     (CaretLine <= FLiveParameterSectionEntry.LastLine) then begin
    UpdateCaretAt;
    {$IFDEF DEBUG}
    Inc(FUpdateFromCaretEarlyExitCount);
    InvalidateChangedRows; { Repaint the early exit count }
    {$ENDIF}
    Exit;
  end;
  if (FLiveKeyValueSection <> nil) and FLiveKeyValueSection.Valid and
     (FRowSetSignature <> '') and not LiveObjectTextChanged then begin
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

  FreeAndNil(FLiveParameterSectionEntry);
  FreeAndNil(FLiveKeyValueSection);
  {$IFDEF DEBUG}
  FUpdateFromCaretEarlyExitCount := 0;
  {$ENDIF}

  { Build row set signature for the selected entry or section }
  var RowSetSignature: String; { The actual value this gets doesn't matter, as long as it's unique for any unique row set }
  var Entry: TLiveScriptParameterSectionEntry;
  var EntryRefusalReason: TRefusalReason;
  if FFactory.TryCreateParameterSectionEntry(CaretLine, Entry, EntryRefusalReason) then begin
    FLiveParameterSectionEntry := Entry;
    FChangeCountAtCreation := FFactory.ChangeCount;
    FLiveParameterSectionEntry.Entry.QuoteNewValues := FQuoteNewParameterValues;
    const SectionName = SectionToSectionName(FLiveParameterSectionEntry.Section);
    {$IFDEF DEBUG}
    FDebugStatusRowString := Format('[%s] entry at lines %d-%d',
      [SectionName, FLiveParameterSectionEntry.FirstLine+1,
       FLiveParameterSectionEntry.LastLine+1]);
    {$ENDIF}
    { Rows address parameters by index, so the signature includes the indexes }
    RowSetSignature := 'E|' + SectionName;
    for var I := 0 to FLiveParameterSectionEntry.Entry.Count-1 do begin
      const Parameter = FLiveParameterSectionEntry.Entry.Parameters[I];
      if Parameter.Kind = pkParameter then
        RowSetSignature := RowSetSignature + '|' + IntToStr(I) + ':' + Parameter.Name;
    end;
  end else begin
    var SectionIndex: Integer;
    var KeyValueSection: TLiveScriptKeyValueSection;
    var SectionRefusalReason: TRefusalReason;
    if FFactory.TryGetSectionAtLine(CaretLine, SectionIndex) and
       FFactory.TryCreateKeyValueSection(SectionIndex, KeyValueSection,
         SectionRefusalReason) then begin
      const Header = FFactory.SectionHeaders[SectionIndex];
      FLiveKeyValueSection := KeyValueSection;
      FLiveKeyValueSectionIndex := SectionIndex;
      FChangeCountAtCreation := FFactory.ChangeCount;
      FLiveKeyValueSectionName := Header.Name;
      FLiveKeyValueSectionIsDirectiveSection := Header.Section in DirectiveSections;
      FLiveKeyValueSection.Section.QuoteNewValues := FQuoteNewDirectiveValues and
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
      begin
        var Entry: TScriptModelParameterSectionEntry;
        var Index: Integer;
        if TryGetRowParameterSectionEntry(ARow, Entry, Index) and
           Entry.FlagIncluded(Index, ARow.FlagName) then
          Result := 1;
      end;
    irkKey:
      begin
        var Section: TScriptModelKeyValueSection;
        var Index: Integer;
        if TryGetRowKeyValueSection(ARow, Section, Index) then begin
          var BoolValue := False;
          if TryStrToBoolean(Section.Lines[Index].Value, BoolValue) and BoolValue then
            Result := 1;
        end else if (Section <> nil) and
                    SameText(Section.DefaultValue(ARow.Name), SYes) then
          Result := 1;
      end;
    irkKeyFlag:
      begin
        var Section: TScriptModelKeyValueSection;
        var Index: Integer;
        if TryGetRowKeyValueSection(ARow, Section, Index) then begin
          if Section.FlagIncluded(Index, ARow.FlagName) then
            Result := 1;
        end else if (Section <> nil) and
                    ScriptValueIncludesFlag(Section.DefaultValue(ARow.Name), ARow.FlagName) then
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
      begin
        var Entry: TScriptModelParameterSectionEntry;
        var Index: Integer;
        if TryGetRowParameterSectionEntry(ARow, Entry, Index) then
          Result := Entry.Parameters[Index].Value;
        { else: Not present in the script: show empty }
      end;
    irkKey:
      begin
        var Section: TScriptModelKeyValueSection;
        var Index: Integer;
        if TryGetRowKeyValueSection(ARow, Section, Index) then
          Result := Section.Lines[Index].Value
        else if Section <> nil then
          Result := Section.DefaultValue(ARow.Name); { Not present in the script: show the compiler default }
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
        begin
          var Entry: TScriptModelParameterSectionEntry;
          var Index: Integer;
          if TryGetRowParameterSectionEntry(Row, Entry, Index) then
            Entry.SetFlag(Index, Row.FlagName, Value <> 0) { May adjust related flags as well }
          else if (Entry <> nil) and (Row.Index < 0) and (Value <> 0) then begin
            { Group Add's and SetFlag's writes into a single undo action }
            FFactory.Memo.BeginUndoAction;
            try
              Entry.SetFlag(Entry.Add(Row.Name, ''), Row.FlagName, True);
            finally
              FFactory.Memo.EndUndoAction;
            end;
          end;
        end;
      irkKey:
        begin
          var Section: TScriptModelKeyValueSection;
          var Index: Integer;
          var NewValue := SNo;
          if Value <> 0 then
            NewValue := SYes;
          if TryGetRowKeyValueSection(Row, Section, Index) then
            Section.SetValue(Index, NewValue)
          else if (Section <> nil) and (Row.Index < 0) and
                  not SameText(NewValue, Section.DefaultValue(Row.Name)) then { Skip unchanged from default, also see below }
            Section.Add(Row.Name, NewValue);
        end;
      irkKeyFlag:
        begin
          var Section: TScriptModelKeyValueSection;
          var Index: Integer;
          if TryGetRowKeyValueSection(Row, Section, Index) then
            Section.SetFlag(Index, Row.FlagName, Value <> 0) { May adjust related flags as well }
          else if (Section <> nil) and (Row.Index < 0) and (Value <> 0) then begin
            { Group Add's and SetFlag's writes into a single undo action. The
              new directive is seeded with the compiler default so the flags
              shown as checked stay checked. }
            FFactory.Memo.BeginUndoAction;
            try
              Section.SetFlag(Section.Add(Row.Name, Section.DefaultValue(Row.Name)),
                Row.FlagName, True);
            finally
              FFactory.Memo.EndUndoAction;
            end;
          end;
          { else: ignore unchecking a default-checked flag of an absent directive,
            can't write valid script text for that (currently applies only to
            WizardStyle defaulting to 'classic') }
        end;
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
        begin
          var Entry: TScriptModelParameterSectionEntry;
          var Index: Integer;
          const Found = TryGetRowParameterSectionEntry(Row, Entry, Index);
          if Entry <> nil then begin
            var Definition: TMemberDefinition;
            if Entry.TryGetDefinition(Row.Name, Definition) then
              ValidateValue(Row.Name, Value, Definition);
            if Found then
              Entry.SetValue(Index, Value)
            else if (Row.Index < 0) and (Value <> '') then
              Entry.Add(Row.Name, Value);
          end;
        end;
      irkKey:
        begin
          var Section: TScriptModelKeyValueSection;
          var Index: Integer;
          const Found = TryGetRowKeyValueSection(Row, Section, Index);
          if Section <> nil then begin
            var Definition: TMemberDefinition;
            if Section.TryGetDefinition(Row.Name, Definition) then
              ValidateValue(Row.Name, Value, Definition);
            if Found then
              Section.SetValue(Index, Value)
            else if (Row.Index < 0) and (Value <> '') and
                    (Value <> Section.DefaultValue(Row.Name)) then { Same as above, but case sensitive }
              Section.Add(Row.Name, Value);
          end;
        end;
    else
      raise Exception.Create('Internal error: RowSetAsString: unexpected row kind');
    end;
  finally
    FInEdit := False;
  end;
  InvalidateChangedRows;
end;

procedure TInspector.RowRemove(const AEntry: TScriptModelParameterSectionEntry;
  const ASection: TScriptModelKeyValueSection; const AIndex: Integer);
begin
  FInEdit := True; { See RowSetAsString }
  try
    if AEntry <> nil then
      AEntry.Remove(AIndex)
    else
      ASection.Remove(AIndex);
  finally
    FInEdit := False;
  end;
  UpdateFromCaret;
end;

procedure TInspector.ChoiceRowGetValueList(Item: TJvCustomInspectorItem;
  Values: TStrings);
begin
  var Row: TInspectorRow;
  if not TryGetRow(Item, Row) then
    Exit;
  var Definition: TMemberDefinition;
  if (FLiveParameterSectionEntry <> nil) and FLiveParameterSectionEntry.Valid then begin
    if not FLiveParameterSectionEntry.Entry.TryGetDefinition(Row.Name, Definition) then
      raise Exception.Create('Internal error: ChoiceRowGetValueList: unknown parameter');
  end else if (FLiveKeyValueSection <> nil) and FLiveKeyValueSection.Valid then begin
    if not FLiveKeyValueSection.Section.TryGetDefinition(Row.Name, Definition) then
      raise Exception.Create('Internal error: ChoiceRowGetValueList: unknown key');
  end else
    Exit;
  { Sort using same sort as autocompletion and Scintilla, so using CompareText.
    Also see TInnoSetupStyler.BuildWordList. }
  var KnownValues := Copy(Definition.KnownValues);
  TArray.Sort<String>(KnownValues, TComparer<String>.Construct(
    function(const A, B: String): Integer
    begin
      Result := CompareText(A, B);
    end));
  for var KnownValue in KnownValues do
    Values.Add(KnownValue);
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
    FLiveKeyValueSection.Section.QuoteNewValues := Value;
end;

procedure TInspector.SetQuoteNewParameterValues(const Value: Boolean);
begin
  FQuoteNewParameterValues := Value;
  if FLiveParameterSectionEntry <> nil then
    FLiveParameterSectionEntry.Entry.QuoteNewValues := Value;
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
