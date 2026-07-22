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
  Classes, Graphics, Controls, StdCtrls, Generics.Collections,
  JvInspector, ModernColors,
  IDE.LiveScriptObjectFactory, IDE.ScriptModel, IDE.ScriptModel.Metadata;

type
  TInspectorRowKind = (irkParameter, irkParameterFlag, irkKey,
    irkKeyFlag {$IFDEF DEBUG}, irkDebugStatus, irkDebugSections, irkDebugEarlyExits{$ENDIF});

  TInspectorRow = record
    Kind: TInspectorRowKind;
    Name: String;            { The parameter or key name }
    FlagName: String;        { irkParameterFlag and irkKeyFlag }
    NameIndex: Integer;      { The parameter index, or the line index for a
                               key. -1 if known but not present in the
                               script }
  end;

  TInspector = class
  private
    FJvInspector: TJvInspector;
    FFactory: TLiveScriptObjectFactory;
    FLiveParameterSectionEntry: TLiveScriptParameterSectionEntry;
    FLiveKeyValueSection: TLiveScriptKeyValueSection;
    FLiveKeyValueSectionName: String;
    FLiveKeyValueSectionIsDirectiveSection: Boolean;
    FLiveKeyValueSectionHasSiblingOccurrences: Boolean;
    FLiveKeyValueSectionIndex: Integer; { Factory section index it was created for }
    FChangeCountAtCreation: Int64; { Factory ChangeCount at the live object's creation }
    FRows: TList<TInspectorRow>;
    FRowSetSignature: String;
    {$IFDEF DEBUG}
    FDebugStatusRowString: String;
    FUpdateFromCaretEarlyExitCount: Integer;
    {$ENDIF}
    FInEdit: Boolean;
    FShowAllKnownDirectives: Boolean;
    FQuoteNewParameterValues: Boolean;
    FQuoteNewDirectiveValues: Boolean;
    function TryGetRow(const AItem: TJvCustomInspectorItem;
      out ARow: TInspectorRow): Boolean;
    function TryGetRowParameterSectionEntry(const ARow: TInspectorRow;
      out AEntry: TScriptModelParameterSectionEntry; out AIndex: Integer): Boolean;
    function TryGetRowKeyValueSection(const ARow: TInspectorRow;
      out ASection: TScriptModelKeyValueSection; out AIndex: Integer): Boolean;
    procedure RowGetAsOrdinal(Sender: TJvCustomInspectorItem; var Value: Int64);
    procedure RowGetAsString(Sender: TJvCustomInspectorItem; var Value: String);
    procedure RowSetAsOrdinal(Sender: TJvCustomInspectorItem; var Value: Int64);
    procedure RowSetAsString(Sender: TJvCustomInspectorItem; var Value: String);
    procedure ChoiceRowGetValueList(Item: TJvCustomInspectorItem; Values: TStrings);
    function ItemShouldBeBold(const AItem: TJvCustomInspectorItem): Boolean;
    procedure PainterSetItemColors(Item: TJvCustomInspectorItem;
      Canvas: TCanvas);
    procedure JvInspectorBeforeEdit(Sender: TObject;
      Item: TJvCustomInspectorItem; Edit: TEdit);
    procedure JvInspectorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    function GetDividerWidth: Integer;
    procedure SetDividerWidth(const Value: Integer);
    procedure SetQuoteNewDirectiveValues(const Value: Boolean);
    procedure SetQuoteNewParameterValues(const Value: Boolean);
    procedure SetShowAllKnownDirectives(const Value: Boolean);
  public
    constructor Create(const AJvInspector: TJvInspector;
      const AFactory: TLiveScriptObjectFactory;
      const AShowAllKnownDirectives: Boolean);
    destructor Destroy; override;
    procedure ForceFinishEdit;
    procedure SetActiveFactory(const AFactory: TLiveScriptObjectFactory;
      const AShowAllKnownDirectives: Boolean);
    procedure UpdateFromCaret;
    procedure UpdateReadOnly;
    procedure UpdateTheme(const ATheme: TTheme);
    property ShowAllKnownDirectives: Boolean read FShowAllKnownDirectives
      write SetShowAllKnownDirectives;
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
  Windows, SysUtils, UITypes, Themes, Forms, Generics.Defaults,
  NewUxTheme,
  Shared.CommonFunc,
  IDE.HelperFunc, IDE.Messages, IDE.LocalizeFunc, IDE.ScriptModel.Metadata.Extra;

type
  EInspectorValueRejected = class(EScriptModelError);

{ TInspector }

constructor TInspector.Create(const AJvInspector: TJvInspector;
  const AFactory: TLiveScriptObjectFactory;
  const AShowAllKnownDirectives: Boolean);
{ Takes ownership of AJvInspector }
begin
  inherited Create;

  FFactory := AFactory;
  FShowAllKnownDirectives := AShowAllKnownDirectives;
  {$IFDEF DEBUG}
  FDebugStatusRowString := 'Not updated yet';
  {$ENDIF}
  FRows := TList<TInspectorRow>.Create;

  FJvInspector := AJvInspector;
  FJvInspector.OnSetItemColors := PainterSetItemColors;
  FJvInspector.BeforeEdit := JvInspectorBeforeEdit;
  FJvInspector.OnKeyDown := JvInspectorKeyDown;
  FJvInspector.OnEditorKeyDown := JvInspectorKeyDown;
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
  inherited;
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
  AIndex := ARow.NameIndex;
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
  AIndex := ARow.NameIndex;
  Result := ASection.TryResolve(ARow.Name, AIndex);
end;

procedure TInspector.PainterSetItemColors(Item: TJvCustomInspectorItem;
  Canvas: TCanvas);
begin
  { Called just before it draws each row's name and again just before its value }
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

procedure TInspector.JvInspectorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  function GetSelectedHelpKeyword: String;
  begin
    Result := '';
    const Item = FJvInspector.Selected;
    var Row: TInspectorRow;
    if (Item <> nil) and TryGetRow(Item, Row) then begin
      case Row.Kind of
        irkParameter, irkKey, irkKeyFlag:
          Result := Row.Name; { A key's (=directive's) flags have no own help topic }
        irkParameterFlag:
          Result := Row.FlagName;
      end;
    end;
  end;

begin
  if (Key = VK_F1) and (Shift * [ssShift, ssAlt, ssCtrl] = []) then begin
    Key := 0;
    ShowHelp(GetSelectedHelpKeyword);
  end;
end;

procedure TInspector.ForceFinishEdit;
{ Commits a pending in-place edit, silently reverting it if its value is
  rejected, or loudly reverting on other errors }
begin
  const Item = FJvInspector.Selected;
  if (Item <> nil) and Item.Editing then begin
    if FFactory.Memo.ReadOnly then
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
  end;
end;

procedure TInspector.SetActiveFactory(const AFactory: TLiveScriptObjectFactory;
  const AShowAllKnownDirectives: Boolean);
begin
  if AFactory = FFactory then begin
    { Still apply the setting: it may have changed independently. Also still
      update from caret. }
    if AShowAllKnownDirectives <> FShowAllKnownDirectives then
      ShowAllKnownDirectives := AShowAllKnownDirectives { Also updates from caret }
    else
      UpdateFromCaret;
    Exit;
  end;
  { Attach to a different factory = different memo = different tab }
  FFactory := AFactory;
  FShowAllKnownDirectives := AShowAllKnownDirectives;
  FRowSetSignature := ''; { Force rebuild even if row set stayed same }
  UpdateFromCaret;
end;

procedure TInspector.UpdateFromCaret;

  function LiveObjectTextChanged: Boolean;
  begin
    if FFactory.ChangeCount < FChangeCountAtCreation then
      raise Exception.Create('Internal error: LiveObjectTextChanged: ChangeCount decreased');
    Result := FFactory.ChangeCount > FChangeCountAtCreation;
  end;

  function ItemKey(const AItem: TJvCustomInspectorItem): String;
  begin
    if AItem is TJvInspectorCustomCategoryItem then
      Result := 'C|' + AItem.DisplayName
    else
      Result := 'R|' + AItem.DisplayName;
  end;

  procedure SaveExpandedStates(const AStates: TDictionary<String, Boolean>;
    const AParent: TJvCustomInspectorItem);
  begin
    for var I := 0 to AParent.Count-1 do begin
      const Item = AParent.Items[I];
      if Item.Count > 0 then begin
        AStates.AddOrSetValue(ItemKey(Item), Item.Expanded);
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
        if AStates.TryGetValue(ItemKey(Item), Expanded) then
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
    const ADisplayName: String; const ABoolean: Boolean;
    const ARow: TInspectorRow): TJvCustomInspectorItem;
  begin
    if ABoolean then
      Result := TJvInspectorBooleanItem.Create(AParent)
    else
      Result := TJvInspectorStringItem.Create(AParent);
    Result.DisplayName := ADisplayName;
    FRows.Add(ARow);
    Result.Tag := FRows.Count;
  end;

  {$IFDEF DEBUG}
  procedure AddDebugRow(const AParent: TJvCustomInspectorItem;
    const ADisplayName: String; const AKind: TInspectorRowKind);
  begin
    var Row: TInspectorRow;
    Row.Kind := AKind;
    Row.Name := '';
    Row.FlagName := '';
    Row.NameIndex := -1;
    const Item = AddRow(AParent, ADisplayName, False, Row);
    Item.Flags := Item.Flags + [iifReadonly];
  end;
  {$ENDIF}

  function MakeParameterRow(const AName: String;
    const ANameIndex: Integer): TInspectorRow;
  begin
    Result.Kind := irkParameter;
    Result.Name := AName;
    Result.FlagName := '';
    Result.NameIndex := ANameIndex;
  end;

  procedure AddParameterFlagRow(const AParent: TJvCustomInspectorItem;
    const AParameterName, AFlagName: String; const ANameIndex: Integer);
  begin
    var Row: TInspectorRow;
    Row.Kind := irkParameterFlag;
    Row.Name := AParameterName;
    Row.FlagName := AFlagName;
    Row.NameIndex := ANameIndex;
    AddRow(AParent, AFlagName, True, Row);
  end;

  procedure AddParameterRow(const AParent: TJvCustomInspectorItem;
    const ADefinition: TMemberDefinition; const ANameIndex: Integer);
  begin
    const Row = MakeParameterRow(ADefinition.Name, ANameIndex);
    const Item = AddRow(AParent, Row.Name, False, Row);
    if ADefinition.ValueKind = mvkFlags then begin
      for var FlagName in ADefinition.KnownValues do
        AddParameterFlagRow(Item, ADefinition.Name, FlagName, ANameIndex); { Adds a child to Item }
    end else if ADefinition.ValueKind = mvkChoice then
      Item.Flags := Item.Flags + [iifValueList];
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

  function MakeKeyRow(const AName: String;
    const ANameIndex: Integer): TInspectorRow;
  begin
    Result.Kind := irkKey;
    Result.Name := AName;
    Result.FlagName := '';
    Result.NameIndex := ANameIndex;
  end;

  function KeyRowIsCheckBox(const ADefinition: TMemberDefinition;
    const ANameIndex: Integer): Boolean;
  { A yes/no key gets a true checkbox row only if its value is a simple yes/no and
    not something like an ISPP inline directive, else it falls back to a text & dropdown row. }
  begin
    var BoolValue := False;
    Result := (ADefinition.ValueKind = mvkYesNo) and
      ((ANameIndex < 0) or { Don't check unspecified keys, they don't have a value }
       TryStrToBoolean(FLiveKeyValueSection.Section.Lines[ANameIndex].Value, BoolValue));
  end;

  procedure AddKeyFlagRow(const AParent: TJvCustomInspectorItem;
    const AKeyName, AFlagName: String; const ANameIndex: Integer);
  begin
    var Row: TInspectorRow;
    Row.Kind := irkKeyFlag;
    Row.Name := AKeyName;
    Row.FlagName := AFlagName;
    Row.NameIndex := ANameIndex;
    AddRow(AParent, AFlagName, True, Row);
  end;

  procedure AddKeyRow(const AParent: TJvCustomInspectorItem;
    const ARow: TInspectorRow);
  begin
    var Definition: TMemberDefinition;
    const Known = FLiveKeyValueSection.Section.TryGetDefinition(ARow.Name, Definition);
    if Known and KeyRowIsCheckBox(Definition, ARow.NameIndex) then
      AddRow(AParent, ARow.Name, True, ARow)
    else begin
      const Item = AddRow(AParent, ARow.Name, False, ARow);
      if Known then begin
        if Definition.ValueKind = mvkFlags then begin
          for var FlagName in Definition.KnownValues do
            AddKeyFlagRow(Item, ARow.Name, FlagName, ARow.NameIndex); { Adds a child to Item }
        end else if Definition.ValueKind in [mvkChoice, mvkYesNo] then
          Item.Flags := Item.Flags + [iifValueList];
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
        if not Entry.TryGetDefinition(Parameter.Name, Definition) then begin
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
              KeyRowsToShow.Add(MakeKeyRow(Section.Lines[I].Name, I));
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
        if (Section.Lines[I].Kind = lkKeyValue) and not LineWillBeShown[I] then
          KeyRowsToShow.Add(MakeKeyRow(Section.Lines[I].Name, I));
      end;

      { Determination done. Add by category the same way as entry rows are. }

      { Uncategorized first, in the order determined above }
      for var Row in KeyRowsToShow do begin
        var CategoryName: String;
        if not TryGetScriptCategory(FLiveKeyValueSectionName, Row.Name, CategoryName) then
          AddKeyRow(FJvInspector.Root, Row);
      end;

      { Categorized keys, also in the order determined above }
      for var CategoryName in ScriptCategoryNamesOrdered do begin
        var CategoryItem: TJvCustomInspectorItem := nil;
        for var Row in KeyRowsToShow do begin
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

  function FindItemByKey(const AKey: String;
    const AParent: TJvCustomInspectorItem): TJvCustomInspectorItem;
  begin
    Result := nil;
    for var I := 0 to AParent.Count-1 do begin
      const Item = AParent.Items[I];
      if ItemKey(Item) = AKey then
        Exit(Item);
      Result := FindItemByKey(AKey, Item);
      if Result <> nil then
        Exit;
    end;
  end;

  procedure RebuildRows;
  begin
    var SelectedKey := '';
    if FJvInspector.Selected <> nil then
      SelectedKey := ItemKey(FJvInspector.Selected);

    FJvInspector.BeginUpdate;
    try
      const ExpandedStates = TDictionary<String, Boolean>.Create;
      try
        SaveExpandedStates(ExpandedStates, FJvInspector.Root);
        FJvInspector.Clear;
        FRows.Clear;

        if FLiveParameterSectionEntry <> nil then
          AddParameterSectionEntryRows
        else if FLiveKeyValueSection <> nil then
          AddKeyValueSectionRows;

        {$IFDEF DEBUG}
        const DebugCategory = NewCategory('Debug');
        AddDebugRow(DebugCategory, 'Status', irkDebugStatus);
        AddDebugRow(DebugCategory, 'Sections', irkDebugSections);
        AddDebugRow(DebugCategory, 'Early exits', irkDebugEarlyExits);
        {$ENDIF}

        RestoreExpandedStates(ExpandedStates, FJvInspector.Root);
      finally
        ExpandedStates.Free;
      end;
    finally
      FJvInspector.EndUpdate;
    end;

    if SelectedKey <> '' then
      FJvInspector.Selected := FindItemByKey(SelectedKey, FJvInspector.Root);
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
    {$IFDEF DEBUG}
    Inc(FUpdateFromCaretEarlyExitCount);
    FJvInspector.Invalidate; { Repaint the early exit count }
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
      {$IFDEF DEBUG}
      Inc(FUpdateFromCaretEarlyExitCount);
      FJvInspector.Invalidate; { See above }
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

  { Re-sync any open in-place editor. Done before any rebuild: RebuildRows'
    Clear deselects, and a deselect applies a stale editor's text back over
    the memo edit unless the editor was re-synced first }
  FJvInspector.RefreshValues;

  if RowSetSignature <> FRowSetSignature then begin
    { Row set changes, need to rebuild the inspector's rows }
    FRowSetSignature := RowSetSignature;
    RebuildRows;
  end; { else: Row set stayed same, just need to Invalidate to show updated values }

  FJvInspector.Invalidate;
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

procedure TInspector.RowGetAsOrdinal(Sender: TJvCustomInspectorItem;
  var Value: Int64);
begin
  Value := 0;
  var Row: TInspectorRow;
  if not TryGetRow(Sender, Row) then
    Exit;
  case Row.Kind of
    irkParameterFlag:
      begin
        var Entry: TScriptModelParameterSectionEntry;
        var Index: Integer;
        if TryGetRowParameterSectionEntry(Row, Entry, Index) and
           Entry.FlagIncluded(Index, Row.FlagName) then
          Value := 1;
      end;
    irkKey:
      begin
        var Section: TScriptModelKeyValueSection;
        var Index: Integer;
        if TryGetRowKeyValueSection(Row, Section, Index) then begin
          var BoolValue := False;
          if TryStrToBoolean(Section.Lines[Index].Value, BoolValue) and BoolValue then
            Value := 1;
        end else if (Section <> nil) and
                    SameText(Section.DefaultValue(Row.Name), SYes) then
          Value := 1;
      end;
    irkKeyFlag:
      begin
        var Section: TScriptModelKeyValueSection;
        var Index: Integer;
        if TryGetRowKeyValueSection(Row, Section, Index) then begin
          if Section.FlagIncluded(Index, Row.FlagName) then
            Value := 1;
        end else if (Section <> nil) and
                    ScriptValueIncludesFlag(Section.DefaultValue(Row.Name), Row.FlagName) then
          Value := 1; { Not present in the script: show the compiler default }
      end;
  end;
end;

procedure TInspector.RowGetAsString(Sender: TJvCustomInspectorItem;
  var Value: String);
begin
  Value := '';
  var Row: TInspectorRow;
  if not TryGetRow(Sender, Row) then
    Exit;
  case Row.Kind of
    irkParameter:
      begin
        var Entry: TScriptModelParameterSectionEntry;
        var Index: Integer;
        if TryGetRowParameterSectionEntry(Row, Entry, Index) then
          Value := Entry.Parameters[Index].Value;
        { else: Not present in the script: show empty }
      end;
    irkKey:
      begin
        var Section: TScriptModelKeyValueSection;
        var Index: Integer;
        if TryGetRowKeyValueSection(Row, Section, Index) then
          Value := Section.Lines[Index].Value
        else if Section <> nil then
          Value := Section.DefaultValue(Row.Name); { Not present in the script: show the compiler default }
      end;
    {$IFDEF DEBUG}
    irkDebugStatus:
      Value := FDebugStatusRowString;
    irkDebugSections:
      begin
        for var I := 0 to FFactory.SectionCount-1 do begin
          const Header = FFactory.SectionHeaders[I];
          if Value <> '' then
            Value := Value + ', ';
          Value := Value + Header.Name + '@' + IntToStr(Header.Line+1);
        end;
      end;
    irkDebugEarlyExits:
      Value := IntToStr(FUpdateFromCaretEarlyExitCount);
    {$ENDIF}
  end;
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
          else if (Entry <> nil) and (Row.NameIndex < 0) and (Value <> 0) then begin
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
          else if (Section <> nil) and (Row.NameIndex < 0) and
                  not SameText(NewValue, Section.DefaultValue(Row.Name)) then { Skip unchanged from default, also see below }
            Section.Add(Row.Name, NewValue);
        end;
      irkKeyFlag:
        begin
          var Section: TScriptModelKeyValueSection;
          var Index: Integer;
          if TryGetRowKeyValueSection(Row, Section, Index) then
            Section.SetFlag(Index, Row.FlagName, Value <> 0) { May adjust related flags as well }
          else if (Section <> nil) and (Row.NameIndex < 0) and (Value <> 0) then begin
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
        end;
    else
      raise Exception.Create('Internal error: RowSetAsOrdinal: unexpected row kind');
    end;
  finally
    FInEdit := False;
  end;
  FJvInspector.Invalidate;
end;

procedure TInspector.RowSetAsString(Sender: TJvCustomInspectorItem;
  var Value: String);

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
            else if (Row.NameIndex < 0) and (Value <> '') then
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
            else if (Row.NameIndex < 0) and (Value <> '') and
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
  FJvInspector.Invalidate;
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
  if (FLiveKeyValueSection <> nil) and FLiveKeyValueSectionIsDirectiveSection then
    FLiveKeyValueSection.Section.QuoteNewValues := Value;
end;

procedure TInspector.SetQuoteNewParameterValues(const Value: Boolean);
begin
  FQuoteNewParameterValues := Value;
  if FLiveParameterSectionEntry <> nil then
    FLiveParameterSectionEntry.Entry.QuoteNewValues := Value;
end;

procedure TInspector.SetShowAllKnownDirectives(const Value: Boolean);
begin
  if Value <> FShowAllKnownDirectives then begin
    FShowAllKnownDirectives := Value;
    FRowSetSignature := ''; { Force a rebuild, see UpdateFromCaret's early exit }
    UpdateFromCaret;
  end;
end;

procedure TInspector.UpdateTheme(const ATheme: TTheme);
begin
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

end.
