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
  Generics.Collections,
  JvInspector,
  IDE.LiveScriptObjectFactory, IDE.ScriptModel.Metadata;

type
  TInspectorRowKind = (irkEntryValue, irkEntryFlag, irkDirective);

  TInspectorRow = record
    Kind: TInspectorRowKind;
    Name: String;            { irkEntryValue and irkEntryFlag: parameter name,
                               irkDirective: directive name }
    FlagName: String;        { irkEntryFlag }
    DirectiveIndex: Integer; { irkDirective: line index in the section model,
                               -1 for an unspecified known directive }
  end;

  TInspector = class
  private
    FJvInspector: TJvInspector;
    FFactory: TLiveScriptObjectFactory;
    FEntry: TLiveScriptEntry;
    FRows: TList<TInspectorRow>;
    FRowsByData: TDictionary<TJvInspectorEventData, Integer>;
    FRowSetSignature: String;
    FDebugStatusRowString: String;
    FInEdit: Boolean;
    procedure DebugSectionsRowGetAsString(Sender: TJvInspectorEventData; var Value: String);
    procedure DebugStatusRowGetAsString(Sender: TJvInspectorEventData; var Value: String);
  public
    constructor Create(const AJvInspector: TJvInspector;
      const AFactory: TLiveScriptObjectFactory);
    destructor Destroy; override;
    procedure SetActiveFactory(const AFactory: TLiveScriptObjectFactory);
    procedure UpdateFromCaret;
  end;

implementation

uses
  SysUtils;

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
  FJvInspector.Root.SortKind := iskNone;
end;

destructor TInspector.Destroy;
begin
  { Free the inspector before the objects its rows read from }
  FJvInspector.Free;
  FEntry.Free;
  FRowsByData.Free;
  FRows.Free;
  inherited;
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

  procedure RebuildRows;

    function NewCategory(const AName: String): TJvCustomInspectorItem;
    begin
      Result := TJvInspectorCustomCategoryItem.Create(FJvInspector.Root, nil);
      Result.DisplayName := AName;
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

  begin
    FJvInspector.BeginUpdate;
    try
      FJvInspector.Clear;
      FRows.Clear;
      FRowsByData.Clear;

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
  FreeAndNil(FEntry);

  { Build row set signature for the selected entry or section }
  const CaretLine = FFactory.Memo.CaretLine;
  var RowSetSignature: String; { The actual value this gets doesn't matter, as long as it's unique for any unique row set }
  var Entry: TLiveScriptEntry;
  var EntryRefusalReason: String;
  if FFactory.TryCreateEntry(CaretLine, Entry, EntryRefusalReason) then begin
    FEntry := Entry;
    const SectionName = ParameterSectionToSectionName(FEntry.Section);
    FDebugStatusRowString := Format('[%s] entry at lines %d-%d',
      [SectionName, FEntry.FirstLine+1, FEntry.LastLine+1]);
    { Only unknown parameters and obsolete ones change which rows are shown }
    RowSetSignature := 'E|' + SectionName;
    for var I := 0 to FEntry.Entry.ParameterCount-1 do begin
      const Parameter = FEntry.Entry.Parameters[I];
      if Parameter.HasName then begin
        var Definition: TScriptParameterDefinition;
        if not FEntry.Entry.TryGetParameterDefinition(Parameter.Name, Definition) or
           Definition.Obsolete then
          RowSetSignature := RowSetSignature + '|' + Parameter.Name;
      end;
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

end.
