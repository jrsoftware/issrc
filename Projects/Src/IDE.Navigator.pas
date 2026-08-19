unit IDE.Navigator;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE Navigator
}

interface

uses
  Messages,
  Classes, Controls, StdCtrls,
  ScintEdit,
  IDE.LiveScriptObjectFactory;

type
  TNavigator = class
  private
    FComboBox: TComboBox;
    FFactory: TLiveScriptObjectFactory;
    FChangeCountAtSet: Int64; { Factory ChangeCount at the last section list compose, -1 to force one }
  public
    constructor Create(const AComboBox: TComboBox;
      const AFactory: TLiveScriptObjectFactory);
    procedure SetActiveFactory(const AFactory: TLiveScriptObjectFactory);
    procedure UpdateFromCaret;
  end;

implementation

uses
  Windows,
  IDE.Messages, IDE.LocalizeFunc;

{ TNavigator }

constructor TNavigator.Create(const AComboBox: TComboBox;
  const AFactory: TLiveScriptObjectFactory);
{ Doesn't take ownership of the combobox }
begin
  inherited Create;

  FComboBox := AComboBox;
  FFactory := AFactory;
  FChangeCountAtSet := -1;
  UpdateFromCaret;
end;

procedure TNavigator.SetActiveFactory(const AFactory: TLiveScriptObjectFactory);
begin
  { Attach to a different factory = different memo = different tab }
  FFactory := AFactory;
  FChangeCountAtSet := -1; { Force rebuild }
  UpdateFromCaret;
end;

procedure TNavigator.UpdateFromCaret;

  function ItemsDiffer(const AComboBox: TComboBox;
    const AItems: TArray<String>): Boolean;
  begin
    const Items = AComboBox.Items;
    if Items.Count <> Length(AItems) then
      Exit(True);
    for var I := 0 to Items.Count-1 do
      if Items[I] <> AItems[I] then
        Exit(True);
    Result := False;
  end;

  procedure SetComboBoxItems(const AComboBox: TComboBox;
    const AItems: TArray<String>);
  begin
    if ItemsDiffer(AComboBox, AItems) then begin
      AComboBox.Items.BeginUpdate;
      try
        AComboBox.Items.Clear;
        for var Item in AItems do
          AComboBox.Items.Add(Item);
      finally
        AComboBox.Items.EndUpdate;
      end;
    end;
  end;

begin
  const ChangeCount = FFactory.ChangeCount;
  if FChangeCountAtSet <> ChangeCount then begin
    var Headers: TArray<String>;
    SetLength(Headers, FFactory.SectionCount);
    for var I := 0 to FFactory.SectionCount-1 do
      Headers[I] := FFactory.SectionHeaders[I].Name;
    SetComboBoxItems(FComboBox, Headers);
    FChangeCountAtSet := ChangeCount;
  end;

  const CaretLine = FFactory.Memo.CaretLine;
  var NewItemIndex: Integer;
  if not FFactory.TryGetSectionAtLine(CaretLine, NewItemIndex) then
    NewItemIndex := -1;
  if FComboBox.ItemIndex <> NewItemIndex then
    FComboBox.ItemIndex := NewItemIndex;
end;

end.
