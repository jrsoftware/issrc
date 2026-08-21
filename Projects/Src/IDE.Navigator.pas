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
  IDE.LiveScriptObjectFactory;

type
  TNavigator = class
  private
    FComboBox: TComboBox;
    FSavedComboBoxWindowProc: TWndMethod;
    FFactory: TLiveScriptObjectFactory;
    FItemIndexBeforeDropDown: Integer;
    FDropDownAccepted: Boolean;
    FJustClosedUp: Boolean;
    FPendingPickComboBox: TComboBox;
    FChangeCountAtSectionsSet: Int64; { -1 to force one }
    procedure HandleCloseUpDone;
    procedure ComboBoxDropDown(Sender: TObject);
    procedure ComboBoxCloseUp(Sender: TObject);
    procedure ComboBoxSelect(Sender: TObject);
    procedure ComboBoxWindowProc(var Message: TMessage);
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
  FChangeCountAtSectionsSet := -1;
  FComboBox.OnDropDown := ComboBoxDropDown;
  FComboBox.OnCloseUp := ComboBoxCloseUp;
  FComboBox.OnSelect := ComboBoxSelect;
  FSavedComboBoxWindowProc := FComboBox.WindowProc;
  FComboBox.WindowProc := ComboBoxWindowProc;
  UpdateFromCaret;
end;

destructor TNavigator.Destroy;
begin
  TThread.RemoveQueuedEvents(HandleCloseUpDone);
  FComboBox.OnDropDown := nil;
  FComboBox.OnCloseUp := nil;
  FComboBox.OnSelect := nil;
  FComboBox.WindowProc := FSavedComboBoxWindowProc;
  inherited Destroy;
end;

{ Drop down handling: Ending a drop down accepts it (Enter, a click, or
  F4) or cancels it (for example Escape). An accept is a pick and must
  jump, even when the current item did not change. A cancel must not
  jump. CBN_SELENDOK and CBN_SELENDCANCEL are the notifications that
  tell an accept from a cancel, but the VCL has no events for them, so
  ComboBoxWindowProc watches them and updates FDropDownAccepted.
  Both notifications are sent before CBN_CLOSEUP: see the CComboBox docs
  (https://learn.microsoft.com/en-us/cpp/mfc/reference/ccombobox-class).

  A close up does not tell whether the pick's selection change has been
  reported yet. The CBN_CLOSEUP docs say: "a CBN_SELCHANGE notification
  code may occur either before or after a CBN_CLOSEUP notification code"
  (https://learn.microsoft.com/en-us/windows/win32/controls/cbn-closeup).
  The same was seen for the selection change of a cancel's restore.

  So at an accept's close up there are two cases: 1) If the selection
  differs from before the drop down, then its selection change has
  already become visible: ComboBoxCloseUp jumps at once. 2) If no
  selection change is visible yet, that can mean two things: the
  selection notification still comes, or the current item was picked
  and none comes at all. ComboBoxCloseUp then only remembers the pick
  (FPendingPickComboBox), and the jump is done by whichever comes
  first: the selection notification in ComboBoxSelect, or
  HandleCloseUpDone, queued to run after all the close up's
  notifications. HandleCloseUpDone also updates from the caret: after
  a cancel the restored selection is stale when ISIDE's debugger moved
  the caret while the list was dropped down, for example to a
  breakpoint.

  With all this done, one issue is left: to ComboBoxSelect, a selection
  notification that arrives after the close up (the pick's, or the
  cancel's restore) looks the same as picking in the closed list,
  but needs different handling. FJustClosedUp solves this. Set at
  a close up and cleared by the first selection notification after it,
  or by UpdateFromCaret, which HandleCloseUpDone calls after every
  close up, it lets ComboBoxSelect know the difference:
  FJustClosedUp = False if the user is picking in the closed list.

  An accepted drop-down pick's jump also moves the focus to the memo.
  A pick in the closed list (arrow keys, mouse wheel, type-ahead)
  jumps but keeps the focus in the combobox, so the user can keep
  picking.

  For reference, ComboBoxSelect is called on:
  -Arrow down/up etc, with the list open (=browsing)
  -Same but with the list closed (=picking)
  -Mouse click (=picking)
  -The selection restore after a cancel, arriving after the close up }

procedure TNavigator.ComboBoxWindowProc(var Message: TMessage);
begin
  if Message.Msg = CN_COMMAND then
    case TWMCommand(Message).NotifyCode of
      CBN_SELENDOK: FDropDownAccepted := True;
      CBN_SELENDCANCEL: FDropDownAccepted := False;
    end;
  FSavedComboBoxWindowProc(Message);
end;

procedure TNavigator.ComboBoxDropDown(Sender: TObject);
begin
  FItemIndexBeforeDropDown := (Sender as TComboBox).ItemIndex;
  FDropDownAccepted := False;
end;

procedure TNavigator.ComboBoxCloseUp(Sender: TObject);
begin
  const ComboBox = Sender as TComboBox;
  if FDropDownAccepted then begin
    if ComboBox.ItemIndex <> FItemIndexBeforeDropDown then
      GoToComboBoxItem(ComboBox, True)
    else
      FPendingPickComboBox := ComboBox;
  end;
  FJustClosedUp := True;
  TThread.ForceQueue(nil, HandleCloseUpDone);
end;

procedure TNavigator.ComboBoxSelect(Sender: TObject);
begin
  const ComboBox = Sender as TComboBox;
  if ComboBox.DroppedDown then
    Exit;
  if FJustClosedUp then begin
    FJustClosedUp := False;
    if FDropDownAccepted then begin
      FPendingPickComboBox := nil;
      GoToComboBoxItem(ComboBox, True);
    end;
  end else
    GoToComboBoxItem(ComboBox, False);
end;

procedure TNavigator.HandleCloseUpDone;
begin
  if FPendingPickComboBox <> nil then begin
    const ComboBox = FPendingPickComboBox;
    FPendingPickComboBox := nil;
    GoToComboBoxItem(ComboBox, True);
  end;
  UpdateFromCaret;
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
  FChangeCountAtSectionsSet := -1; { Force rebuild }
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
  FJustClosedUp := False;

  const CaretLine = FFactory.Memo.CaretLine;
  var NewItemIndex: Integer;
  if not FFactory.TryGetSectionAtLine(CaretLine, NewItemIndex) then
    NewItemIndex := -1;

  const ChangeCount = FFactory.ChangeCount;
  if FChangeCountAtSectionsSet <> ChangeCount then begin
    var Headers: TArray<String>;
    SetLength(Headers, FFactory.SectionCount);
    for var I := 0 to FFactory.SectionCount-1 do
      Headers[I] := '[' + FFactory.SectionHeaders[I].Name + ']';
    SetComboBoxItems(FComboBox, Headers, NewItemIndex);
    FChangeCountAtSectionsSet := ChangeCount;
  end;

  if FComboBox.ItemIndex <> NewItemIndex then
    FComboBox.ItemIndex := NewItemIndex;
end;

end.
