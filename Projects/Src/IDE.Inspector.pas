unit IDE.Inspector;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TJvInspector wrapper, attached to a TLiveScriptObjectFactory, following
  the caret, creating new live objects for it, showing them in the inspector,
  and forwarding edits from it to the factory.

  The painter must be a TJvInspectorDotNETPainter.
}

interface

uses
  Classes, Generics.Collections, TypInfo,
  JvInspector, ModernColors,
  IDE.LiveScriptObjectFactory, IDE.ScriptModel, IDE.ScriptModel.Metadata;

type
  TInspectorRowKind = (irkEntryValue, irkEntryFlag, irkDirective);

  TInspectorRow = record
    Kind: TInspectorRowKind;
    Name: String;            { irkEntryValue and irkEntryFlag: parameter name,
                               irkDirective: directive name }
    FlagName: String;        { irkEntryFlag }
    NameIndex: Integer;      { irkDirective: line index,
                               irkEntryValue and irkEntryFlag: parameter index
                               -1 if known but not present in the script }
  end;

  TInspector = class
  private
    FJvInspector: TJvInspector;
    FPainter: TJvInspectorDotNETPainter;
    FFactory: TLiveScriptObjectFactory;
    FLiveEntry: TLiveScriptEntry;
    FRows: TList<TInspectorRow>;
    FRowsByData: TDictionary<TJvInspectorEventData, Integer>; { Reverse lookup of a FRows index }
    FRowSetSignature: String;
    FDebugStatusRowString: String;
    FInEdit: Boolean;
    function TryGetRow(const Sender: TJvInspectorEventData;
      out ARow: TInspectorRow): Boolean; overload;
    function TryGetRow(const AItem: TJvCustomInspectorItem;
      out ARow: TInspectorRow): Boolean; overload;
    function TryGetRow(const ARow: TInspectorRow;
      out AEntry: TScriptParameterEntry; out AIndex: Integer): Boolean; overload;
    procedure RowGetAsOrdinal(Sender: TJvInspectorEventData; var Value: Int64);
    procedure RowGetAsString(Sender: TJvInspectorEventData; var Value: String);
    procedure RowSetAsOrdinal(Sender: TJvInspectorEventData; var Value: Int64);
    procedure RowSetAsString(Sender: TJvInspectorEventData; var Value: String);
    procedure ChoiceRowGetValueList(Item: TJvCustomInspectorItem; Values: TStrings);
    procedure DebugStatusRowGetAsString(Sender: TJvInspectorEventData; var Value: String);
    procedure DebugSectionsRowGetAsString(Sender: TJvInspectorEventData; var Value: String);
    function GetDividerWidth: Integer;
    function GetWidth: Integer;
    procedure SetDividerWidth(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  public
    constructor Create(const AJvInspector: TJvInspector;
      const AFactory: TLiveScriptObjectFactory);
    destructor Destroy; override;
    procedure SetActiveFactory(const AFactory: TLiveScriptObjectFactory);
    procedure UpdateFromCaret;
    procedure UpdateTheme(const ATheme: TTheme);
    property JvInspector: TJvInspector read FJvInspector;
    property Width: Integer read GetWidth write SetWidth;
    property DividerWidth: Integer read GetDividerWidth write SetDividerWidth;
  end;

implementation

uses
  SysUtils, Themes,
  NewUxTheme,
  IDE.Messages, IDE.LocalizeFunc;

{ TInspector }

constructor TInspector.Create(const AJvInspector: TJvInspector;
  const AFactory: TLiveScriptObjectFactory);
{ Takes ownership of AJvInspector }
begin
  inherited Create;

  FFactory := AFactory;
  FDebugStatusRowString := 'Not updated yet';
  FRows := TList<TInspectorRow>.Create;
  FRowsByData := TDictionary<TJvInspectorEventData, Integer>.Create;

  FJvInspector := AJvInspector;
  FPainter := FJvInspector.Painter as TJvInspectorDotNETPainter;
  FJvInspector.Root.SortKind := iskNone;
end;

destructor TInspector.Destroy;
begin
  { Free the inspector before the objects its rows read from }
  FJvInspector.Free;
  FLiveEntry.Free;
  FRowsByData.Free;
  FRows.Free;
  inherited;
end;

function TInspector.TryGetRow(const ARow: TInspectorRow;
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

procedure TInspector.SetActiveFactory(const AFactory: TLiveScriptObjectFactory);
begin
  if AFactory = FFactory then
    Exit;
  { Attach to a different factory = different memo = different tab }
  FFactory := AFactory;
  FRowSetSignature := ''; { Force rebuild even if row set stayed same }
  UpdateFromCaret;
end;

procedure TInspector.UpdateFromCaret;

  function NewCategory(const AName: String): TJvCustomInspectorItem;
  begin
    Result := TJvInspectorCustomCategoryItem.Create(FJvInspector.Root, nil);
    Result.DisplayName := LFmtMessage(AName); { These are localizable, see IDE.Messages }
    Result.Expanded := True;
  end;

  procedure AddDebugRow(const AParent: TJvCustomInspectorItem;
    const ADisplayName: String; const AOnGetAsString: TJvInspAsString);
  begin
    const Item = TJvInspectorEventData.New(AParent, ADisplayName,
      TypeInfo(String));
    TJvInspectorEventData(Item.Data).OnGetAsString := AOnGetAsString;
    Item.Flags := Item.Flags + [iifReadonly];
  end;

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
      (Result as TJvInspectorBooleanItem).ShowAsCheckBox := True;
    end else begin
      Data.OnGetAsString := RowGetAsString;
      Data.OnSetAsString := RowSetAsString;
    end;
  end;

  procedure MakeDropDown(const AItem: TJvCustomInspectorItem;
    const AOnGetValueList: TInspectorItemGetValueListEvent);
  begin
    AItem.Flags := AItem.Flags + [iifValueList, iifAllowNonListValues];
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

  procedure RebuildRows;
  begin
    FJvInspector.BeginUpdate;
    try
      FJvInspector.Clear;
      FRows.Clear;
      FRowsByData.Clear;

      if FLiveEntry <> nil then
        AddEntryRows;

      const DebugCategory = NewCategory('Debug');
      AddDebugRow(DebugCategory, 'Status', DebugStatusRowGetAsString);
      AddDebugRow(DebugCategory, 'Sections', DebugSectionsRowGetAsString);
    finally
      FJvInspector.EndUpdate;
    end;
  end;

begin
  if FInEdit then
    Exit;
  FreeAndNil(FLiveEntry);

  { Build row set signature for the selected entry or section }
  const CaretLine = FFactory.Memo.CaretLine;
  var RowSetSignature: String; { The actual value this gets doesn't matter, as long as it's unique for any unique row set }
  var Entry: TLiveScriptEntry;
  var EntryRefusalReason: String;
  if FFactory.TryCreateEntry(CaretLine, Entry, EntryRefusalReason) then begin
    FLiveEntry := Entry;
    const SectionName = ParameterSectionToSectionName(FLiveEntry.Section);
    FDebugStatusRowString := Format('[%s] entry at lines %d-%d',
      [SectionName, FLiveEntry.FirstLine+1, FLiveEntry.LastLine+1]);
    { Rows address parameters by index, so the signature includes the indexes }
    RowSetSignature := 'E|' + SectionName;
    for var I := 0 to FLiveEntry.Entry.Count-1 do begin
      const Parameter = FLiveEntry.Entry.Parameters[I];
      if Parameter.Kind = sepParameter then
        RowSetSignature := RowSetSignature + '|' + IntToStr(I) + ':' + Parameter.Name;
    end;
  end else begin
    FDebugStatusRowString := EntryRefusalReason;
    RowSetSignature := 'N|' + EntryRefusalReason;
  end;

  if RowSetSignature <> FRowSetSignature then begin
    { Row set changes, need to rebuild the inspector's rows }
    FRowSetSignature := RowSetSignature;
    RebuildRows;
  end; { else: Row set stayed same, just need to Invalidate to show updated values }

  FJvInspector.Invalidate;
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
        if TryGetRow(Row, Entry, Index) and
           Entry.FlagIncluded(Index, Row.FlagName) then
          Value := 1;
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
        if TryGetRow(Row, Entry, Index) then
          Value := Entry.Parameters[Index].Value;
        { else: a parameter not present in the script shows empty }
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
    raise Exception.Create('Internal error: RowSetAsOrdinal: memo is read-only');
  FInEdit := True;
  try
    case Row.Kind of
      irkEntryFlag:
        begin
          var Entry: TScriptParameterEntry;
          var Index: Integer;
          if TryGetRow(Row, Entry, Index) then
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
begin
  var Row: TInspectorRow;
  if not TryGetRow(Sender, Row) then
    raise Exception.Create('Internal error: RowSetAsString: unknown row');
  if FFactory.Memo.ReadOnly then
    raise Exception.Create('Internal error: RowSetAsString: memo is read-only');
  FInEdit := True;
  try
    case Row.Kind of
      irkEntryValue:
        begin
          var Entry: TScriptParameterEntry;
          var Index: Integer;
          const Found = TryGetRow(Row, Entry, Index);
          if Entry <> nil then begin
            var Definition: TScriptParameterDefinition;
            if (Value <> '') and (Pos('{', Value) = 0) and
               Entry.TryGetDefinition(Row.Name, Definition) and
               (Definition.ValueKind = pvkInteger) then begin
              { Validate if the value is a valid integer. Strips underscore digit
                separators because the compiler accepts them for some values. }
              var IntegerValue: Int64;
              if not TryStrToInt64(StringReplace(Value, '_', '', [rfReplaceAll]), IntegerValue) then
                raise EScriptModelError.Create(LFmtMessage(SInspectorIntegerValueError, [Row.Name]));
            end;
            if Found then
              Entry.SetValue(Index, Value)
            else if (Row.NameIndex < 0) and (Value <> '') then
              Entry.Add(Row.Name, Value);
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
  if TryGetRow(Item, Row) and (FLiveEntry <> nil) and FLiveEntry.Valid then begin
    var Definition: TScriptParameterDefinition;
    if not FLiveEntry.Entry.TryGetDefinition(Row.Name, Definition) then
      raise Exception.Create('Internal error: ChoiceRowGetValueList: unknown parameter');
    for var ChoiceName in Definition.KnownValues do
      Values.Add(ChoiceName);
  end;
end;

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

procedure TInspector.SetWidth(const Value: Integer);
begin
  FJvInspector.Width := Value;
end;

procedure TInspector.UpdateTheme(const ATheme: TTheme);
begin
  FPainter.BackgroundColor := ATheme.Colors[tcBack];
  FPainter.NameFont.Color := ATheme.Colors[tcFore];
  FPainter.ValueFont.Color := ATheme.Colors[tcFore];
  FPainter.CategoryColor := ATheme.Colors[tcToolBack];
  FPainter.CategoryFont.Color := ATheme.Colors[tcFore];
  FPainter.DividerColor := ATheme.Colors[tcToolBack];
  FPainter.CategoryDividerColor := FPainter.DividerColor;
  FPainter.SelectedColor := ATheme.Colors[tcSelBack];
  FPainter.SelectedFont.Color := ATheme.Colors[tcFore];
  FPainter.HideSelectColor := ATheme.Colors[tcToolBack];
  FPainter.HideSelectFont.Color := ATheme.Colors[tcFore];

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
