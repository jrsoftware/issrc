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
  Classes, Graphics, Controls, StdCtrls, Generics.Collections, TypInfo,
  JvInspector, ModernColors,
  IDE.LiveScriptObjectFactory, IDE.ScriptModel, IDE.ScriptModel.Metadata;

type
  TInspectorRowKind = (irkEntryValue, irkEntryFlag, irkDirective,
    irkDirectiveFlag);

  TInspectorRow = record
    Kind: TInspectorRowKind;
    Name: String;            { irkEntryValue and irkEntryFlag: parameter name,
                               irkDirective and irkDirectiveFlag: directive name }
    FlagName: String;        { irkEntryFlag and irkDirectiveFlag }
    NameIndex: Integer;      { irkDirective and irkDirectiveFlag: line index,
                               irkEntryValue and irkEntryFlag: parameter index
                               -1 if known but not present in the script }
  end;

  TInspector = class
  private
    FJvInspector: TJvInspector;
    FFactory: TLiveScriptObjectFactory;
    FLiveEntry: TLiveScriptEntry;
    FLiveDirectiveSection: TLiveScriptDirectiveSection;
    FLiveDirectiveSectionName: String;
    FLiveDirectiveSectionHasSiblingOccurrences: Boolean;
    FLiveDirectiveSectionIndex: Integer; { Factory section index it was created for }
    FChangeCountAtCreation: Int64; { Factory ChangeCount at the live object's creation }
    FRows: TList<TInspectorRow>;
    FRowsByData: TDictionary<TJvInspectorEventData, Integer>; { Reverse lookup of a FRows index }
    FRowSetSignature: String;
    {$IFDEF DEBUG}
    FDebugStatusRowString: String;
    FUpdateFromCaretEarlyExitCount: Integer;
    {$ENDIF}
    FInEdit: Boolean;
    FShowAllKnownDirectives: Boolean;
    FQuoteNewParameterValues: Boolean;
    FQuoteNewDirectiveValues: Boolean;
    function TryGetRow(const Sender: TJvInspectorEventData;
      out ARow: TInspectorRow): Boolean; overload;
    function TryGetRow(const AItem: TJvCustomInspectorItem;
      out ARow: TInspectorRow): Boolean; overload;
    function TryGetRowParameterEntry(const ARow: TInspectorRow;
      out AEntry: TScriptParameterEntry; out AIndex: Integer): Boolean;
    function TryGetRowDirectiveSection(const ARow: TInspectorRow;
      out ASection: TScriptDirectiveSection; out AIndex: Integer): Boolean;
    procedure RowGetAsOrdinal(Sender: TJvInspectorEventData; var Value: Int64);
    procedure RowGetAsString(Sender: TJvInspectorEventData; var Value: String);
    procedure RowSetAsOrdinal(Sender: TJvInspectorEventData; var Value: Int64);
    procedure RowSetAsString(Sender: TJvInspectorEventData; var Value: String);
    procedure ChoiceRowGetValueList(Item: TJvCustomInspectorItem; Values: TStrings);
    {$IFDEF DEBUG}
    procedure DebugStatusRowGetAsString(Sender: TJvInspectorEventData; var Value: String);
    procedure DebugSectionsRowGetAsString(Sender: TJvInspectorEventData; var Value: String);
    procedure DebugEarlyExitsRowGetAsString(Sender: TJvInspectorEventData; var Value: String);
    {$ENDIF}
    function ItemValueIsFromScript(const AItem: TJvCustomInspectorItem): Boolean;
    procedure PainterSetItemColors(Item: TJvCustomInspectorItem;
      Canvas: TCanvas);
    procedure JvInspectorBeforeEdit(Sender: TObject;
      Item: TJvCustomInspectorItem; Edit: TCustomEdit);
    procedure JvInspectorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    function GetDividerWidth: Integer;
    function GetWidth: Integer;
    procedure SetDividerWidth(const Value: Integer);
    procedure SetShowAllKnownDirectives(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
  public
    constructor Create(const AJvInspector: TJvInspector;
      const AFactory: TLiveScriptObjectFactory;
      const AShowAllKnownDirectives: Boolean);
    destructor Destroy; override;
    procedure SetActiveFactory(const AFactory: TLiveScriptObjectFactory;
      const AShowAllKnownDirectives: Boolean);
    procedure UpdateFromCaret;
    procedure UpdateReadOnly;
    procedure UpdateTheme(const ATheme: TTheme);
    property ShowAllKnownDirectives: Boolean read FShowAllKnownDirectives
      write SetShowAllKnownDirectives;
    { These only apply to text values }
    property QuoteNewParameterValues: Boolean read FQuoteNewParameterValues
      write FQuoteNewParameterValues;
    property QuoteNewDirectiveValues: Boolean read FQuoteNewDirectiveValues
      write FQuoteNewDirectiveValues;
    property JvInspector: TJvInspector read FJvInspector;
    property Width: Integer read GetWidth write SetWidth;
    property DividerWidth: Integer read GetDividerWidth write SetDividerWidth;
  end;

implementation

uses
  Windows, SysUtils, UITypes, Themes, Generics.Defaults,
  NewUxTheme,
  Shared.CommonFunc,
  IDE.HelperFunc, IDE.Messages, IDE.LocalizeFunc;

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
  FRowsByData := TDictionary<TJvInspectorEventData, Integer>.Create;

  FJvInspector := AJvInspector;
  FJvInspector.OnSetItemColors := PainterSetItemColors;
  FJvInspector.BeforeEdit := JvInspectorBeforeEdit;
  FJvInspector.OnKeyDown := JvInspectorKeyDown;
  FJvInspector.OnEditorKeyDown := JvInspectorKeyDown;
end;

destructor TInspector.Destroy;
begin
  { Free the inspector before the objects its rows read from }
  FJvInspector.Free;
  FLiveEntry.Free;
  FLiveDirectiveSection.Free;
  FRowsByData.Free;
  FRows.Free;
  inherited;
end;

function TInspector.ItemValueIsFromScript(
  const AItem: TJvCustomInspectorItem): Boolean;

  function RowValueIsFromScript(const ARow: TInspectorRow): Boolean;
  begin
    Result := False;
    case ARow.Kind of
      irkEntryValue:
        begin
          var Entry: TScriptParameterEntry;
          var Index: Integer;
          Result := TryGetRowParameterEntry(ARow, Entry, Index);
        end;
      irkEntryFlag:
        begin
          var Entry: TScriptParameterEntry;
          var Index: Integer;
          Result := TryGetRowParameterEntry(ARow, Entry, Index) and
            Entry.FlagIncluded(Index, ARow.FlagName);
        end;
      irkDirective:
        begin
          var Section: TScriptDirectiveSection;
          var Index: Integer;
          Result := TryGetRowDirectiveSection(ARow, Section, Index);
        end;
      irkDirectiveFlag:
        begin
          var Section: TScriptDirectiveSection;
          var Index: Integer;
          Result := TryGetRowDirectiveSection(ARow, Section, Index) and
            Section.FlagIncluded(Index, ARow.FlagName);
        end;
    end;
  end;

begin
  var Row: TInspectorRow;
  Result := TryGetRow(AItem, Row) and RowValueIsFromScript(Row);
end;

function TInspector.TryGetRowParameterEntry(const ARow: TInspectorRow;
  out AEntry: TScriptParameterEntry; out AIndex: Integer): Boolean;
begin
  AEntry := nil;
  AIndex := -1;
  if (FLiveEntry = nil) or not FLiveEntry.Valid then
    Exit(False);
  AEntry := FLiveEntry.Entry;
  AIndex := ARow.NameIndex;
  Result := AEntry.TryResolve(ARow.Name, AIndex);
end;

function TInspector.TryGetRowDirectiveSection(const ARow: TInspectorRow;
  out ASection: TScriptDirectiveSection; out AIndex: Integer): Boolean;
begin
  ASection := nil;
  AIndex := -1;
  if (FLiveDirectiveSection = nil) or not FLiveDirectiveSection.Valid then
    Exit(False);
  ASection := FLiveDirectiveSection.Section;
  AIndex := ARow.NameIndex;
  Result := ASection.TryResolve(ARow.Name, AIndex);
end;

procedure TInspector.PainterSetItemColors(Item: TJvCustomInspectorItem;
  Canvas: TCanvas);
begin
  { Called by the DotNET painter just before it draws each row's name and again
    just before its value }
  if ItemValueIsFromScript(Item) then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
end;

type
  TControlAccess = class(TControl);

procedure TInspector.JvInspectorBeforeEdit(Sender: TObject;
  Item: TJvCustomInspectorItem; Edit: TCustomEdit);
begin
  { Bold the in-place editor's font as well }
  if ItemValueIsFromScript(Item) then
    TControlAccess(Edit).Font.Style := TControlAccess(Edit).Font.Style + [fsBold];
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
        irkEntryValue, irkDirective, irkDirectiveFlag:
          Result := Row.Name; { A directive's flags have no own help topic }
        irkEntryFlag:
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

procedure TInspector.SetActiveFactory(const AFactory: TLiveScriptObjectFactory;
  const AShowAllKnownDirectives: Boolean);
begin
  if AFactory = FFactory then
    Exit;
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

  function ExpandedStateKey(const AItem: TJvCustomInspectorItem): String;
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
        AStates.AddOrSetValue(ExpandedStateKey(Item), Item.Expanded);
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
        if AStates.TryGetValue(ExpandedStateKey(Item), Expanded) then
          Item.Expanded := Expanded;
        RestoreExpandedStates(AStates, Item);
      end;
    end;
  end;

  function NewCategory(const AName: String): TJvCustomInspectorItem;
  begin
    Result := TJvInspectorCustomCategoryItem.Create(FJvInspector.Root, nil);
    Result.DisplayName := LFmtMessage(AName); { These are localizable, see IDE.Messages }
    Result.Expanded := True;
  end;

  {$IFDEF DEBUG}
  procedure AddDebugRow(const AParent: TJvCustomInspectorItem;
    const ADisplayName: String; const AOnGetAsString: TJvInspAsString);
  begin
    const Item = TJvInspectorEventData.New(AParent, ADisplayName,
      TypeInfo(String));
    TJvInspectorEventData(Item.Data).OnGetAsString := AOnGetAsString;
    Item.Flags := Item.Flags + [iifReadonly];
  end;

  function RefusalReasonToString(const ARefusalReason: TLiveScriptRefusalReason): String;
  begin
    case ARefusalReason of
      rrLineOutOfRange: Result := 'The line number is out of range';
      rrNotInsideSection: Result := 'The line is not inside a section';
      rrInCodeSection: Result := 'The line is in the [Code] section';
      rrUnrecognizedSection: Result := 'The line is in an unrecognized section';
      rrDirectiveStyleSection: Result := 'The line is in a directive-style section';
      rrComment: Result := 'The line is a comment';
      rrISPPDirective: Result := 'The line is an ISPP directive';
      rrSectionIndexOutOfRange: Result := 'The section index is out of range';
      rrNotDirectiveStyleSection: Result := 'The section is not a directive-style section';
    else
      Result := '';
    end;
  end;
  {$ENDIF}

  function AddRow(const AParent: TJvCustomInspectorItem;
    const ADisplayName: String; const ATypeInfo: PTypeInfo;
    const ARow: TInspectorRow): TJvCustomInspectorItem;
  begin
    Result := TJvInspectorEventData.New(AParent, ADisplayName, ATypeInfo);
    const Data = TJvInspectorEventData(Result.Data);
    FRows.Add(ARow);
    FRowsByData.Add(Data, Integer(FRows.Count)-1);
    if ATypeInfo = TypeInfo(Boolean) then begin
      Data.OnGetAsOrdinal := RowGetAsOrdinal;
      Data.OnSetAsOrdinal := RowSetAsOrdinal;
    end else begin
      Data.OnGetAsString := RowGetAsString;
      Data.OnSetAsString := RowSetAsString;
    end;
  end;

  procedure MakeDropDown(const AItem: TJvCustomInspectorItem;
    const AOnGetValueList: TInspectorItemGetValueListEvent);
  begin
    AItem.Flags := AItem.Flags + [iifValueList];
    AItem.OnGetValueList := AOnGetValueList;
  end;

  function AddEntryValueRow(const AParent: TJvCustomInspectorItem;
    const AParameterName: String; const ANameIndex: Integer): TJvCustomInspectorItem;
  begin
    var Row: TInspectorRow;
    Row.Kind := irkEntryValue;
    Row.Name := AParameterName;
    Row.FlagName := '';
    Row.NameIndex := ANameIndex;
    Result := AddRow(AParent, AParameterName, TypeInfo(String), Row);
  end;

  procedure AddEntryFlagRow(const AParent: TJvCustomInspectorItem;
    const AParameterName, AFlagName: String; const ANameIndex: Integer);
  begin
    var Row: TInspectorRow;
    Row.Kind := irkEntryFlag;
    Row.Name := AParameterName;
    Row.FlagName := AFlagName;
    Row.NameIndex := ANameIndex;
    AddRow(AParent, AFlagName, TypeInfo(Boolean), Row);
  end;

  procedure AddParameterRow(const AParent: TJvCustomInspectorItem;
    const ADefinition: TScriptParameterDefinition; const ANameIndex: Integer);
  begin
    const Item = AddEntryValueRow(AParent, ADefinition.Name, ANameIndex);
    if ADefinition.ValueKind = pvkFlags then begin
      for var FlagName in ADefinition.KnownValues do
        AddEntryFlagRow(Item, ADefinition.Name, FlagName, ANameIndex); { Adds a child to Item }
    end else if ADefinition.ValueKind = pvkChoice then
      MakeDropDown(Item, ChoiceRowGetValueList);
  end;

  procedure AddParameterRows(const AParent: TJvCustomInspectorItem;
    const ADefinition: TScriptParameterDefinition);
  begin
    { Normally a parameter will be present only once, but duplicates are still
      handled here, even though that doesn't compile }
    const Entry = FLiveEntry.Entry;
    var Found := False;
    for var I := 0 to Entry.Count-1 do begin
      if (Entry.Parameters[I].Kind = sepParameter) and
         SameText(Entry.Parameters[I].Name, ADefinition.Name) then begin
        AddParameterRow(AParent, ADefinition, I);
        Found := True;
      end;
    end;
    if not Found then
      AddParameterRow(AParent, ADefinition, -1);
  end;

  function MakeDirectiveRow(const AName: String;
    const ANameIndex: Integer): TInspectorRow;
  begin
    Result.Kind := irkDirective;
    Result.Name := AName;
    Result.FlagName := '';
    Result.NameIndex := ANameIndex;
  end;

  function DirectiveRowIsCheckBox(const ADefinition: TScriptParameterDefinition;
    const ANameIndex: Integer): Boolean;
  { A yes/no directive gets a true checkbox row only if its value is a simple yes/no and
    not something like an ISPP inline directive, else it falls back to a text & dropdown row. }
  begin
    var BoolValue := False;
    Result := (ADefinition.ValueKind = pvkYesNo) and
      ((ANameIndex < 0) or { Don't check unspecified directives, they don't have a value }
       TryStrToBoolean(FLiveDirectiveSection.Section.Lines[ANameIndex].Value, BoolValue));
  end;

  procedure AddDirectiveFlagRow(const AParent: TJvCustomInspectorItem;
    const ADirectiveName, AFlagName: String; const ANameIndex: Integer);
  begin
    var Row: TInspectorRow;
    Row.Kind := irkDirectiveFlag;
    Row.Name := ADirectiveName;
    Row.FlagName := AFlagName;
    Row.NameIndex := ANameIndex;
    AddRow(AParent, AFlagName, TypeInfo(Boolean), Row);
  end;

  procedure AddDirectiveRow(const AParent: TJvCustomInspectorItem;
    const ARow: TInspectorRow);
  begin
    var Definition: TScriptParameterDefinition;
    const Known = FLiveDirectiveSection.Section.TryGetDefinition(ARow.Name, Definition);
    if Known and DirectiveRowIsCheckBox(Definition, ARow.NameIndex) then
      AddRow(AParent, ARow.Name, TypeInfo(Boolean), ARow)
    else begin
      const Item = AddRow(AParent, ARow.Name, TypeInfo(String), ARow);
      if Known then begin
        if Definition.ValueKind = pvkFlags then begin
          for var FlagName in Definition.KnownValues do
            AddDirectiveFlagRow(Item, ARow.Name, FlagName, ARow.NameIndex); { Adds a child to Item }
        end else if Definition.ValueKind in [pvkChoice, pvkYesNo] then
          MakeDropDown(Item, ChoiceRowGetValueList);
      end;
    end;
  end;

  procedure AddEntryRows;
  begin
    const Entry = FLiveEntry.Entry;

    { Known and uncategorized parameters first, in metadata order }
    if Entry.Metadata <> nil then begin
      const SectionName = Entry.Metadata.SectionName;
      for var Definition in Entry.Metadata.Parameters do begin
        if Definition.Obsolete and not Entry.Has(Definition.Name) then
          Continue; { Hide obsolete and unspecified }
        var CategoryName: String;
        if not TryGetScriptCategory(SectionName, Definition.Name, CategoryName) then
          AddParameterRows(FJvInspector.Root, Definition);
      end;
    end;

    { Present but unknown parameters }
    for var I := 0 to Entry.Count-1 do begin
      const Parameter = Entry.Parameters[I];
      if Parameter.Kind = sepParameter then begin
        var Definition: TScriptParameterDefinition;
        if not Entry.TryGetDefinition(Parameter.Name, Definition) then
          AddEntryValueRow(FJvInspector.Root, Parameter.Name, I);
      end;
    end;

    { Known and categorized parameters, in metadata order }
    if Entry.Metadata <> nil then begin
      const SectionName = Entry.Metadata.SectionName;
      for var CategoryName in ScriptCategoryNamesOrdered do begin
        var CategoryItem: TJvCustomInspectorItem := nil;
        for var Definition in Entry.Metadata.Parameters do begin
          if Definition.Obsolete and not Entry.Has(Definition.Name) then
            Continue;
          var DefinitionCategory: String;
          if TryGetScriptCategory(SectionName, Definition.Name, DefinitionCategory) and
             SameText(DefinitionCategory, CategoryName) then begin
            if CategoryItem = nil then
              CategoryItem := NewCategory(CategoryName);
            AddParameterRows(CategoryItem, Definition);
          end;
        end;
      end;
    end;
  end;

  procedure AddDirectiveRows;
  begin
    const Section = FLiveDirectiveSection.Section;

    var LineWillBeShown: TArray<Boolean>;
    SetLength(LineWillBeShown, Section.Count);

    const DirectivesToShow = TList<TInspectorRow>.Create;
    try
      { First determine the directives to show and their order: with
        ShowAllKnownDirectives, show every known directive in metadata
        order. A repeated directive gets a row per line. }
      if FShowAllKnownDirectives and (Section.Metadata <> nil) then begin
        for var Definition in Section.Metadata.Parameters do begin
          var Found := False;
          for var I := 0 to Section.Count-1 do begin
            if (Section.Lines[I].Kind = sdlDirective) and
               SameText(Section.Lines[I].Name, Definition.Name) then begin
              DirectivesToShow.Add(MakeDirectiveRow(Section.Lines[I].Name, I));
              LineWillBeShown[I] := True;
              Found := True;
            end;
          end;
          { An unspecified directive gets a row showing the compiler default,
            unless it is obsolete or another occurrence of the section might
            set it. Other occurrences aren't parsed/live so we can't check. }
          if not Found and not Definition.Obsolete and
             not FLiveDirectiveSectionHasSiblingOccurrences then
            DirectivesToShow.Add(MakeDirectiveRow(Definition.Name, -1));
        end;
      end;

      { The remaining directives, in script order }
      for var I := 0 to Section.Count-1 do begin
        if (Section.Lines[I].Kind = sdlDirective) and not LineWillBeShown[I] then
          DirectivesToShow.Add(MakeDirectiveRow(Section.Lines[I].Name, I));
      end;

      { Determination done. Add by category the same way as entry rows are. }

      { Uncategorized first, in the order determined above }
      for var Row in DirectivesToShow do begin
        var CategoryName: String;
        if not TryGetScriptCategory(FLiveDirectiveSectionName, Row.Name, CategoryName) then
          AddDirectiveRow(FJvInspector.Root, Row);
      end;

      { Categorized directives, also in the order determined above }
      for var CategoryName in ScriptCategoryNamesOrdered do begin
        var CategoryItem: TJvCustomInspectorItem := nil;
        for var Row in DirectivesToShow do begin
          var DirectiveCategory: String;
          if TryGetScriptCategory(FLiveDirectiveSectionName, Row.Name, DirectiveCategory) and
             SameText(DirectiveCategory, CategoryName) then begin
            if CategoryItem = nil then
              CategoryItem := NewCategory(CategoryName);
            AddDirectiveRow(CategoryItem, Row);
          end;
        end;
      end;
    finally
      DirectivesToShow.Free;
    end;
  end;

  procedure RebuildRows;
  begin
    FJvInspector.BeginUpdate;
    try
      const ExpandedStates = TDictionary<String, Boolean>.Create;
      try
        SaveExpandedStates(ExpandedStates, FJvInspector.Root);
        FJvInspector.Clear;
        FRows.Clear;
        FRowsByData.Clear;

        if FLiveEntry <> nil then
          AddEntryRows
        else if FLiveDirectiveSection <> nil then
          AddDirectiveRows;

        {$IFDEF DEBUG}
        const DebugCategory = NewCategory('Debug');
        AddDebugRow(DebugCategory, 'Status', DebugStatusRowGetAsString);
        AddDebugRow(DebugCategory, 'Sections', DebugSectionsRowGetAsString);
        AddDebugRow(DebugCategory, 'Early exits', DebugEarlyExitsRowGetAsString);
        {$ENDIF}

        RestoreExpandedStates(ExpandedStates, FJvInspector.Root);
      finally
        ExpandedStates.Free;
      end;
    finally
      FJvInspector.EndUpdate;
    end;
  end;

begin
  if FInEdit then
    Exit;

  FJvInspector.ReadOnly := FFactory.Memo.ReadOnly;

  const CaretLine = FFactory.Memo.CaretLine;

  { Without a memo change or a forced rebuild, a caret move within the same
    entry or directive section changes nothing, so keep the model and the rows.
    The signature check must precede LiveObjectTextChanged: right after
    SetActiveFactory the live object still belongs to the previous factory. }
  if (FLiveEntry <> nil) and FLiveEntry.Valid and
     (FRowSetSignature <> '') and not LiveObjectTextChanged and
     (CaretLine >= FLiveEntry.FirstLine) and
     (CaretLine <= FLiveEntry.LastLine) then begin
    {$IFDEF DEBUG}
    Inc(FUpdateFromCaretEarlyExitCount);
    FJvInspector.Invalidate; { Repaint the early exit count }
    {$ENDIF}
    Exit;
  end;
  if (FLiveDirectiveSection <> nil) and FLiveDirectiveSection.Valid and
     (FRowSetSignature <> '') and not LiveObjectTextChanged then begin
    { Resolved by section index instead of the entry's line-range test above:
      the section's range covers the body only, so it misses the header line }
    var SectionIndex: Integer;
    if FFactory.TryGetSectionAtLine(CaretLine, SectionIndex) and
       (SectionIndex = FLiveDirectiveSectionIndex) then begin
      {$IFDEF DEBUG}
      Inc(FUpdateFromCaretEarlyExitCount);
      FJvInspector.Invalidate; { See above }
      {$ENDIF}
      Exit;
    end;
  end;

  FreeAndNil(FLiveEntry);
  FreeAndNil(FLiveDirectiveSection);
  {$IFDEF DEBUG}
  FUpdateFromCaretEarlyExitCount := 0;
  {$ENDIF}

  { Build row set signature for the selected entry or section }
  var RowSetSignature: String; { The actual value this gets doesn't matter, as long as it's unique for any unique row set }
  var Entry: TLiveScriptEntry;
  var EntryRefusalReason: TLiveScriptRefusalReason;
  if FFactory.TryCreateEntry(CaretLine, Entry, EntryRefusalReason) then begin
    FLiveEntry := Entry;
    FChangeCountAtCreation := FFactory.ChangeCount;
    FLiveEntry.Entry.QuoteNewValues := FQuoteNewParameterValues;
    const SectionName = ParameterSectionToSectionName(FLiveEntry.Section);
    {$IFDEF DEBUG}
    FDebugStatusRowString := Format('[%s] entry at lines %d-%d',
      [SectionName, FLiveEntry.FirstLine+1, FLiveEntry.LastLine+1]);
    {$ENDIF}
    { Rows address parameters by index, so the signature includes the indexes }
    RowSetSignature := 'E|' + SectionName;
    for var I := 0 to FLiveEntry.Entry.Count-1 do begin
      const Parameter = FLiveEntry.Entry.Parameters[I];
      if Parameter.Kind = sepParameter then
        RowSetSignature := RowSetSignature + '|' + IntToStr(I) + ':' + Parameter.Name;
    end;
  end else begin
    var SectionIndex: Integer;
    var DirectiveSection: TLiveScriptDirectiveSection;
    var SectionRefusalReason: TLiveScriptRefusalReason;
    if FFactory.TryGetSectionAtLine(CaretLine, SectionIndex) and
       FFactory.TryCreateDirectiveSection(SectionIndex, DirectiveSection,
         SectionRefusalReason) then begin
      const Header = FFactory.Sections[SectionIndex];
      FLiveDirectiveSection := DirectiveSection;
      FLiveDirectiveSectionIndex := SectionIndex;
      FChangeCountAtCreation := FFactory.ChangeCount;
      FLiveDirectiveSection.Section.QuoteNewValues := FQuoteNewDirectiveValues;
      FLiveDirectiveSectionName := Header.Name;
      {$IFDEF DEBUG}
      FDebugStatusRowString := Format('[%s] section at line %d',
        [Header.Name, Header.Line+1]);
      {$ENDIF}
      var OccurrenceIndex, OccurrenceCount: Integer;
      FFactory.GetSectionOccurrence(SectionIndex, OccurrenceIndex, OccurrenceCount);
      FLiveDirectiveSectionHasSiblingOccurrences := OccurrenceCount > 1;
      {$IFDEF DEBUG}
      if FLiveDirectiveSectionHasSiblingOccurrences then
        FDebugStatusRowString := FDebugStatusRowString + Format(' (occurrence %d of %d)',
          [OccurrenceIndex, OccurrenceCount]);
      {$ENDIF}
      { Like the entry signature above, plus the occurrence count and
        whether unspecified known directives are offered, which also
        decide the row set }
      RowSetSignature := 'D|' + IntToStr(OccurrenceCount) + '|' +
        IntToStr(Ord(FShowAllKnownDirectives)) + '|' + Header.Name;
      const Model = FLiveDirectiveSection.Section;
      for var I := 0 to Model.Count-1 do begin
        if Model.Lines[I].Kind = sdlDirective then begin
          RowSetSignature := RowSetSignature + '|' + IntToStr(I) + ':' + Model.Lines[I].Name;
          { Put AddDirectiveRow's decision into the structure }
          var Definition: TScriptParameterDefinition;
          if Model.TryGetDefinition(Model.Lines[I].Name, Definition) and
             DirectiveRowIsCheckBox(Definition, I) then
            RowSetSignature := RowSetSignature + '!';
        end;
      end;
    end else begin
      { Prefer the entry refusal: it explains the caret line, while a failed
        TryCreateDirectiveSection could only say the section is not
        directive-style }
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

function TInspector.TryGetRow(const Sender: TJvInspectorEventData;
  out ARow: TInspectorRow): Boolean;
begin
  var Index: Integer;
  Result := FRowsByData.TryGetValue(Sender, Index);
  if Result then
    ARow := FRows[Index];
end;

function TInspector.TryGetRow(const AItem: TJvCustomInspectorItem;
  out ARow: TInspectorRow): Boolean;
begin
  Result := (AItem.Data is TJvInspectorEventData) and
    TryGetRow(TJvInspectorEventData(AItem.Data), ARow);
end;

procedure TInspector.RowGetAsOrdinal(Sender: TJvInspectorEventData;
  var Value: Int64);
begin
  Value := 0;
  var Row: TInspectorRow;
  if not TryGetRow(Sender, Row) then
    Exit;
  case Row.Kind of
    irkEntryFlag:
      begin
        var Entry: TScriptParameterEntry;
        var Index: Integer;
        if TryGetRowParameterEntry(Row, Entry, Index) and
           Entry.FlagIncluded(Index, Row.FlagName) then
          Value := 1;
      end;
    irkDirective:
      begin
        var Section: TScriptDirectiveSection;
        var Index: Integer;
        if TryGetRowDirectiveSection(Row, Section, Index) then begin
          var BoolValue := False;
          if TryStrToBoolean(Section.Lines[Index].Value, BoolValue) and BoolValue then
            Value := 1;
        end else if (Section <> nil) and
                    SameText(Section.DefaultValue(Row.Name), SYes) then
          Value := 1;
      end;
    irkDirectiveFlag:
      begin
        var Section: TScriptDirectiveSection;
        var Index: Integer;
        if TryGetRowDirectiveSection(Row, Section, Index) then begin
          if Section.FlagIncluded(Index, Row.FlagName) then
            Value := 1;
        end else if (Section <> nil) and
                    ScriptValueIncludesFlag(Section.DefaultValue(Row.Name), Row.FlagName) then
          Value := 1; { Not present in the script: show the compiler default }
      end;
  end;
end;

procedure TInspector.RowGetAsString(Sender: TJvInspectorEventData;
  var Value: String);
begin
  Value := '';
  var Row: TInspectorRow;
  if not TryGetRow(Sender, Row) then
    Exit;
  case Row.Kind of
    irkEntryValue:
      begin
        var Entry: TScriptParameterEntry;
        var Index: Integer;
        if TryGetRowParameterEntry(Row, Entry, Index) then
          Value := Entry.Parameters[Index].Value;
        { else: Not present in the script: show empty }
      end;
    irkDirective:
      begin
        var Section: TScriptDirectiveSection;
        var Index: Integer;
        if TryGetRowDirectiveSection(Row, Section, Index) then
          Value := Section.Lines[Index].Value
        else if Section <> nil then
          Value := Section.DefaultValue(Row.Name); { Not present in the script: show the compiler default }
      end;
  end;
end;

procedure TInspector.RowSetAsOrdinal(Sender: TJvInspectorEventData;
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
      irkEntryFlag:
        begin
          var Entry: TScriptParameterEntry;
          var Index: Integer;
          if TryGetRowParameterEntry(Row, Entry, Index) then
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
      irkDirective:
        begin
          var Section: TScriptDirectiveSection;
          var Index: Integer;
          var NewValue := SNo;
          if Value <> 0 then
            NewValue := SYes;
          if TryGetRowDirectiveSection(Row, Section, Index) then
            Section.SetValue(Index, NewValue)
          else if (Section <> nil) and (Row.NameIndex < 0) and
                  not SameText(NewValue, Section.DefaultValue(Row.Name)) then { Skip unchanged from default, also see below }
            Section.Add(Row.Name, NewValue);
        end;
      irkDirectiveFlag:
        begin
          var Section: TScriptDirectiveSection;
          var Index: Integer;
          if TryGetRowDirectiveSection(Row, Section, Index) then
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

procedure TInspector.RowSetAsString(Sender: TJvInspectorEventData;
  var Value: String);

  procedure ValidateValue(const ARowName, AValue: String;
    const ADefinition: TScriptParameterDefinition);
  begin
    if (AValue <> '') and (Pos('{', AValue) = 0) and
       (ADefinition.ValueKind = pvkInteger) then begin
      { Validate if the value is a valid integer. Strips underscore digit
        separators because the compiler accepts them for some values. }
      var IntegerValue: Int64;
      if not TryStrToInt64(StringReplace(AValue, '_', '', [rfReplaceAll]), IntegerValue) then
        raise EScriptModelError.Create(LFmtMessage(SInspectorIntegerValueError, [ARowName]));
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
      irkEntryValue:
        begin
          var Entry: TScriptParameterEntry;
          var Index: Integer;
          const Found = TryGetRowParameterEntry(Row, Entry, Index);
          if Entry <> nil then begin
            var Definition: TScriptParameterDefinition;
            if Entry.TryGetDefinition(Row.Name, Definition) then
              ValidateValue(Row.Name, Value, Definition);
            if Found then
              Entry.SetValue(Index, Value)
            else if (Row.NameIndex < 0) and (Value <> '') then
              Entry.Add(Row.Name, Value);
          end;
        end;
      irkDirective:
        begin
          var Section: TScriptDirectiveSection;
          var Index: Integer;
          const Found = TryGetRowDirectiveSection(Row, Section, Index);
          if Section <> nil then begin
            var Definition: TScriptParameterDefinition;
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
  var Definition: TScriptParameterDefinition;
  if (FLiveEntry <> nil) and FLiveEntry.Valid then begin
    if not FLiveEntry.Entry.TryGetDefinition(Row.Name, Definition) then
      raise Exception.Create('Internal error: ChoiceRowGetValueList: unknown parameter');
  end else if (FLiveDirectiveSection <> nil) and FLiveDirectiveSection.Valid then begin
    if not FLiveDirectiveSection.Section.TryGetDefinition(Row.Name, Definition) then
      raise Exception.Create('Internal error: ChoiceRowGetValueList: unknown directive');
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

{$IFDEF DEBUG}
procedure TInspector.DebugStatusRowGetAsString(Sender: TJvInspectorEventData;
  var Value: String);
begin
  Value := FDebugStatusRowString;
end;

procedure TInspector.DebugSectionsRowGetAsString(Sender: TJvInspectorEventData;
  var Value: String);
begin
  Value := '';
  for var I := 0 to FFactory.SectionCount-1 do begin
    const Section = FFactory.Sections[I];
    if Value <> '' then
      Value := Value + ', ';
    Value := Value + Section.Name + '@' + IntToStr(Section.Line+1);
  end;
end;

procedure TInspector.DebugEarlyExitsRowGetAsString(Sender: TJvInspectorEventData;
  var Value: String);
begin
  Value := IntToStr(FUpdateFromCaretEarlyExitCount);
end;
{$ENDIF}

function TInspector.GetWidth: Integer;
begin
  Result := FJvInspector.Width;
end;

function TInspector.GetDividerWidth: Integer;
begin
  Result := FJvInspector.Divider;
end;

procedure TInspector.SetDividerWidth(const Value: Integer);
begin
  FJvInspector.Divider := Value;
end;

procedure TInspector.SetShowAllKnownDirectives(const Value: Boolean);
begin
  if Value <> FShowAllKnownDirectives then begin
    FShowAllKnownDirectives := Value;
    FRowSetSignature := ''; { Force a rebuild, see UpdateFromCaret's early exit }
    UpdateFromCaret;
  end;
end;

procedure TInspector.SetWidth(const Value: Integer);
begin
  FJvInspector.Width := Value;
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
