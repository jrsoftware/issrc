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
  StdCtrls,
  IDE.LiveScriptObjectFactory;

type
  TNavigator = class
  private
    FComboBox: TComboBox;
    FFactory: TLiveScriptObjectFactory;
    FChangeCountAtSet: Int64; { Factory ChangeCount at the last section list compose, -1 to force one }
    procedure ComboBoxSelect(Sender: TObject);
    procedure GoToComboBoxItem(const AComboBox: TComboBox;
      const AFocusMemo: Boolean);
  public
    constructor Create(const AComboBox: TComboBox;
      const AFactory: TLiveScriptObjectFactory);
    destructor Destroy; override;
    procedure SetActiveFactory(const AFactory: TLiveScriptObjectFactory);
    procedure UpdateFromCaret;
  end;

implementation

{ TNavigator }

constructor TNavigator.Create(const AComboBox: TComboBox;
  const AFactory: TLiveScriptObjectFactory);
{ Doesn't take ownership of the combobox }
begin
  inherited Create;

  FComboBox := AComboBox;
  FFactory := AFactory;
  FChangeCountAtSet := -1;
  FComboBox.OnSelect := ComboBoxSelect;
  UpdateFromCaret;
end;

destructor TNavigator.Destroy;
begin
  FComboBox.OnSelect := nil;
  inherited Destroy;
end;

procedure TNavigator.ComboBoxSelect(Sender: TObject);
{ This is called on:
  -Arrow down/up etc, with the list open (=browsing)
  -Same but with the list closed (=picking)
  -Mouse click, in which case the call could arrive both before and after the list closes (=picking) }
begin
  const ComboBox = Sender as TComboBox;
  if ComboBox.DroppedDown then
    Exit; { Only browsing, not picking }
  GoToComboBoxItem(ComboBox, False); { Jump, but keep focus }
end;

procedure TNavigator.GoToComboBoxItem(const AComboBox: TComboBox;
  const AFocusMemo: Boolean);
begin
  const Index = AComboBox.ItemIndex;
  if Index < 0 then
    Exit;

  const Memo = FFactory.Memo;
  if AFocusMemo then begin
    Memo.SetFocus;
    if not Memo.Focused then
      Exit; { Validation rejected the focus change }
  end;

  if Index >= FFactory.SectionCount then
    Exit;

  const Line = FFactory.GetSectionFirstSignificantLine(Index);
  Memo.EnsurePositionInViewVertically(Memo.GetPositionFromLine(Line));
  Memo.CaretLine := Line;
end;

procedure TNavigator.SetActiveFactory(const AFactory: TLiveScriptObjectFactory);
begin
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
    const AItems: TArray<String>; const AItemIndex: Integer);
  begin
    if ItemsDiffer(AComboBox, AItems) then begin
      AComboBox.Items.BeginUpdate;
      try
        AComboBox.Items.Clear;
        for var Item in AItems do
          AComboBox.Items.Add(Item);
        { EndUpdate's repaint must not show the empty selection Clear left }
        AComboBox.ItemIndex := AItemIndex;
      finally
        AComboBox.Items.EndUpdate;
      end;
    end;
  end;

begin
  const CaretLine = FFactory.Memo.CaretLine;
  var NewItemIndex: Integer;
  if not FFactory.TryGetSectionAtLine(CaretLine, NewItemIndex) then
    NewItemIndex := -1;

  const ChangeCount = FFactory.ChangeCount;
  if FChangeCountAtSet <> ChangeCount then begin
    var Headers: TArray<String>;
    SetLength(Headers, FFactory.SectionCount);
    for var I := 0 to FFactory.SectionCount-1 do
      Headers[I] := FFactory.SectionHeaders[I].Name;
    SetComboBoxItems(FComboBox, Headers, NewItemIndex);
    FChangeCountAtSet := ChangeCount;
  end;

  if FComboBox.ItemIndex <> NewItemIndex then
    FComboBox.ItemIndex := NewItemIndex;
end;

end.
