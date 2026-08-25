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
  Windows, Messages,
  Classes, Controls, StdCtrls,
  IDE.LiveScriptObjectFactory;

type
  TNavigatorComboBoxItemsChangedEvent = procedure(Sender: TObject;
    const AComboBox: TComboBox) of object;

  TNavigator = class
  private
    FComboBox: TComboBox;  { Sections }
    FComboBox2: TComboBox; { Routines }
    FSavedComboBoxWindowProc: TWndMethod;
    FSavedComboBox2WindowProc: TWndMethod;
    FFactory: TLiveScriptObjectFactory;
    FItemIndexBeforeDropDown: Integer;
    FDropDownAccepted: Boolean;
    FJustClosedUp: Boolean;
    FPendingPickComboBox: TComboBox;
    FChangeCountAtSectionsSet: Int64; { -1 to force rebuild }
    FChangeCountAtRoutinesSet: Int64;
    FLiveCodeSection: TLiveScriptCodeSection;
    FLiveCodeSectionIndex: Integer; { Factory section index it was created for }
    FMessagesWnd: HWND;
    FRebuildRoutinesPending: Boolean;
    FCaretInCodeSection: Boolean;
    FOnCaretInCodeSectionChange: TNotifyEvent;
    FOnComboBoxItemsChanged: TNavigatorComboBoxItemsChangedEvent;
    procedure HandleCloseUpDone;
    procedure ComboBoxDropDown(Sender: TObject);
    procedure ComboBoxCloseUp(Sender: TObject);
    procedure ComboBoxSelect(Sender: TObject);
    procedure ComboBoxWindowProc(var Message: TMessage);
    procedure ComboBox2WindowProc(var Message: TMessage);
    procedure GoToComboBoxItem(const AComboBox: TComboBox;
      const AFocusMemo: Boolean);
    procedure TrackDropDownAcceptance(const Message: TMessage);
    function HandleComboBoxKeyDown(const AComboBox: TComboBox;
      const Message: TMessage): Boolean;
    procedure RebuildRoutinesTimerUpdate(const ACancel: Boolean);
    procedure MessagesWndProc(var Message: TMessage);
    procedure UpdateFromCaret(const AIgnoreDroppedDown,
      AForceRebuildNow: Boolean); overload;
  public
    constructor Create(const AComboBox, AComboBox2: TComboBox;
      const AFactory: TLiveScriptObjectFactory;
      const AOnCaretInCodeSectionChange: TNotifyEvent;
      const AOnComboBoxItemsChanged: TNavigatorComboBoxItemsChangedEvent);
    destructor Destroy; override;
    procedure SetActiveFactory(const AFactory: TLiveScriptObjectFactory);
    procedure UpdateFromCaret; overload;
    property CaretInCodeSection: Boolean read FCaretInCodeSection;
  end;

implementation

uses
  SysUtils,
  IDE.HelperFunc, IDE.ScriptModel, IDE.ScriptModel.Metadata.Extra;

const
  RebuildRoutinesTimerID = 1;

{ TNavigator }

constructor TNavigator.Create(const AComboBox, AComboBox2: TComboBox;
  const AFactory: TLiveScriptObjectFactory;
  const AOnCaretInCodeSectionChange: TNotifyEvent;
  const AOnComboBoxItemsChanged: TNavigatorComboBoxItemsChangedEvent);
{ Doesn't take ownership of the comboboxes }
begin
  inherited Create;

  FComboBox := AComboBox;
  FComboBox2 := AComboBox2;
  FFactory := AFactory;
  FOnCaretInCodeSectionChange := AOnCaretInCodeSectionChange;
  FOnComboBoxItemsChanged := AOnComboBoxItemsChanged;
  FChangeCountAtSectionsSet := -1;
  FMessagesWnd := AllocateHWnd(MessagesWndProc);
  FComboBox.OnDropDown := ComboBoxDropDown;
  FComboBox.OnCloseUp := ComboBoxCloseUp;
  FComboBox.OnSelect := ComboBoxSelect;
  FComboBox2.OnDropDown := ComboBoxDropDown;
  FComboBox2.OnCloseUp := ComboBoxCloseUp;
  FComboBox2.OnSelect := ComboBoxSelect;
  FSavedComboBoxWindowProc := FComboBox.WindowProc;
  FComboBox.WindowProc := ComboBoxWindowProc;
  FSavedComboBox2WindowProc := FComboBox2.WindowProc;
  FComboBox2.WindowProc := ComboBox2WindowProc;
end;

destructor TNavigator.Destroy;
begin
  TThread.RemoveQueuedEvents(HandleCloseUpDone);
  FComboBox.OnDropDown := nil;
  FComboBox.OnCloseUp := nil;
  FComboBox.OnSelect := nil;
  FComboBox.WindowProc := FSavedComboBoxWindowProc;
  FComboBox2.OnDropDown := nil;
  FComboBox2.OnCloseUp := nil;
  FComboBox2.OnSelect := nil;
  FComboBox2.WindowProc := FSavedComboBox2WindowProc;
  if FMessagesWnd <> 0 then
    DeallocateHWnd(FMessagesWnd);
  TLiveScriptObjectFactory.ReleaseAndNil(FLiveCodeSection);
  inherited Destroy;
end;

procedure TNavigator.RebuildRoutinesTimerUpdate(const ACancel: Boolean);
const
  RebuildRoutinesTimerInterval = 100;
begin
  if ACancel then
    KillTimer(FMessagesWnd, RebuildRoutinesTimerID)
  else
    SetTimer(FMessagesWnd, RebuildRoutinesTimerID, RebuildRoutinesTimerInterval, nil);
  FRebuildRoutinesPending := not ACancel;
end;

procedure TNavigator.MessagesWndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_TIMER) and (Message.WParam = RebuildRoutinesTimerID) then begin
    if AnyInputDown or FComboBox.DroppedDown or FComboBox2.DroppedDown then
      Exit; { Keeps timer alive }
    RebuildRoutinesTimerUpdate(True); { Kills timer }
    UpdateFromCaret(False, True);
  end else
    Message.Result := DefWindowProc(FMessagesWnd, Message.Msg, Message.WParam, Message.LParam);
end;

{ Drop down handling: Ending a drop down accepts it (Enter, a click, or
  F4) or cancels it (for example Escape). An accept is a pick and must
  jump, even when the current item did not change. A cancel must not
  jump. CBN_SELENDOK and CBN_SELENDCANCEL are the notifications that
  tell an accept from a cancel, but the VCL has no events for them, so
  TrackDropDownAcceptance watches them and updates FDropDownAccepted.
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

  While tracking, the code makes use of the fact that only one drop
  down can be open at a time.

  For reference, ComboBoxSelect is called on:
  -Arrow down/up etc, with the list open (=browsing)
  -Same but with the list closed (=picking)
  -Mouse click (=picking)
  -The selection restore after a cancel, arriving after the close up }

procedure TNavigator.TrackDropDownAcceptance(const Message: TMessage);
begin
  if Message.Msg = CN_COMMAND then
    case TWMCommand(Message).NotifyCode of
      CBN_SELENDOK: FDropDownAccepted := True;
      CBN_SELENDCANCEL: FDropDownAccepted := False;
    end;
end;

function TNavigator.HandleComboBoxKeyDown(const AComboBox: TComboBox;
  const Message: TMessage): Boolean;
begin
  if (Message.Msg <> WM_KEYDOWN) or
     (GetKeyState(VK_SHIFT) < 0) or (GetKeyState(VK_CONTROL) < 0) then
    Exit(False);

  var OtherComboBox: TComboBox := nil;
  case TWMKeyDown(Message).CharCode of
    VK_RIGHT: if AComboBox = FComboBox then OtherComboBox := FComboBox2;
    VK_LEFT: if AComboBox = FComboBox2 then OtherComboBox := FComboBox;
  end;

  Result := (OtherComboBox <> nil) and OtherComboBox.CanFocus;

  if Result then begin
    const WasDroppedDown = AComboBox.DroppedDown;
    OtherComboBox.SetFocus;
    if WasDroppedDown then
      OtherComboBox.DroppedDown := True;
  end;
end;

procedure TNavigator.ComboBoxWindowProc(var Message: TMessage);
begin
  if HandleComboBoxKeyDown(FComboBox, Message) then
    Exit;
  TrackDropDownAcceptance(Message);
  FSavedComboBoxWindowProc(Message);
end;

procedure TNavigator.ComboBox2WindowProc(var Message: TMessage);
begin
  if HandleComboBoxKeyDown(FComboBox2, Message) then
    Exit;
  TrackDropDownAcceptance(Message);
  FSavedComboBox2WindowProc(Message);
end;

procedure TNavigator.ComboBoxDropDown(Sender: TObject);
begin
  if FRebuildRoutinesPending then begin
    RebuildRoutinesTimerUpdate(True); { Kills timer }
    UpdateFromCaret(True, True); { Make sure it ignores DroppedDown and force a rebuild }
    { ^ This updates the items just in time and might also have changed the width
      of the combobox (in TMainForm's NavigatorComboBoxItemsChanged). Both these
      things worked fine when tested, also for the width of the list which is
      about to drop down. }
  end;

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

  function TryFocusMemo: Boolean;
  begin
    Result := True;
    if AFocusMemo then begin
      const Memo = FFactory.Memo;
      Memo.SetFocus;
      Result := Memo.Focused; { False if validation rejected the focus change }
    end;
  end;

  procedure GoToLine(const ALine: Integer);
  begin
    const Memo = FFactory.Memo;
    Memo.EnsurePositionInViewVertically(Memo.GetPositionFromLine(ALine));
    Memo.CaretLine := ALine;
  end;

  procedure GoToSection(const AIndex: Integer);
  begin
    if AIndex < 0 then
      Exit;

    if not TryFocusMemo then
      Exit;

    if AIndex >= FFactory.SectionCount then
      Exit;

    GoToLine(FFactory.GetSectionFirstSignificantLine(AIndex));
  end;

  procedure GoToRoutine(const AIndex: Integer);
  begin
    if AIndex < 0 then
      Exit;

    if FRebuildRoutinesPending or (FLiveCodeSection = nil) or not FLiveCodeSection.Valid then
      Exit;

    if not TryFocusMemo then
      Exit;

    const Section = FLiveCodeSection.Section;
    if AIndex >= Section.RoutineCount then
      Exit;

    GoToLine(FLiveCodeSection.FirstLine + Section.Routines[AIndex].FirstLine);
  end;

begin
  if AComboBox = FComboBox then
    GoToSection(AComboBox.ItemIndex)
  else
    GoToRoutine(AComboBox.ItemIndex);
end;

procedure TNavigator.SetActiveFactory(const AFactory: TLiveScriptObjectFactory);
begin
  if AFactory = FFactory then begin
    UpdateFromCaret;
    Exit;
  end;
  { Attach to a different factory = different memo = different tab }
  FFactory := AFactory;
  FChangeCountAtSectionsSet := -1; { Force rebuild }
  { A close up must not jump: the comboboxes still hold the previous tab's items }
  FDropDownAccepted := False;
  FPendingPickComboBox := nil;
  { Ensure UpdateFromCaret doesn't skip the update }
  if FComboBox.DroppedDown then
    FComboBox.DroppedDown := False;
  if FComboBox2.DroppedDown then
    FComboBox2.DroppedDown := False;
  { Update }
  RebuildRoutinesTimerUpdate(True); { Cancel any queued }
  UpdateFromCaret(False, True); { Force rebuild }
end;

procedure TNavigator.UpdateFromCaret(const AIgnoreDroppedDown,
  AForceRebuildNow: Boolean);

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
      if Assigned(FOnComboBoxItemsChanged) then
        FOnComboBoxItemsChanged(Self, AComboBox);
    end;
  end;

begin
  FJustClosedUp := False;

  const CaretLine = FFactory.Memo.CaretLine;
  var NewSectionIndex: Integer;
  if not FFactory.TryGetSectionAtLine(CaretLine, NewSectionIndex) then
    NewSectionIndex := -1;

  const ChangeCount = FFactory.ChangeCount;
  const AnyDroppedDown = not AIgnoreDroppedDown and (FComboBox.DroppedDown or FComboBox2.DroppedDown);

  { Neither combobox is updated while either one is dropped down, so the
    debugger moving the caret while the user browses, for example to a
    breakpoint, cannot move the selection or change the list under them. Note
    that breakpoints can be set on parameter section entries as well. }
  if not AnyDroppedDown then begin
    if FChangeCountAtSectionsSet <> ChangeCount then begin
      { Update to new sections }
      var Sections: TArray<String>;
      SetLength(Sections, FFactory.SectionCount);
      for var I := 0 to FFactory.SectionCount-1 do
        Sections[I] := '[' + FFactory.SectionHeaders[I].Name + ']';
      SetComboBoxItems(FComboBox, Sections, NewSectionIndex);
      FChangeCountAtSectionsSet := ChangeCount;
    end;

    if FComboBox.ItemIndex <> NewSectionIndex then
      FComboBox.ItemIndex := NewSectionIndex;
  end;

  if not AnyDroppedDown then begin
    const CaretInCodeSection = (NewSectionIndex >= 0) and
      (FFactory.SectionHeaders[NewSectionIndex].Section = scCode);
    if CaretInCodeSection then begin

      { Rebuild if needed }
      const TextChanged = FChangeCountAtRoutinesSet <> ChangeCount;
      const Rebuild = (FLiveCodeSection = nil) or
        (FLiveCodeSectionIndex <> NewSectionIndex) or TextChanged;
      const RebuildNow = AForceRebuildNow or
        (Rebuild and ((FLiveCodeSection = nil) or (FLiveCodeSectionIndex <> NewSectionIndex)));

      if RebuildNow then begin
        RebuildRoutinesTimerUpdate(True); { Cancel any queued }
        TLiveScriptObjectFactory.ReleaseAndNil(FLiveCodeSection);
        var RefusalReason: TRefusalReason;
        if FFactory.TryAcquireCodeSection(NewSectionIndex, FLiveCodeSection,
             RefusalReason) then
          FLiveCodeSectionIndex := NewSectionIndex;
      end else if Rebuild then
        RebuildRoutinesTimerUpdate(False);

      { Determine caret routine index }
      var NewRoutineIndex := -1;
      if FLiveCodeSection <> nil then begin
        var CaretRoutine: TCodeSectionRoutine;
        if FLiveCodeSection.TryGetRoutine(CaretLine, CaretRoutine) then begin
          const Section = FLiveCodeSection.Section;
          for var I := 0 to Section.RoutineCount-1 do begin
            if Section.Routines[I] = CaretRoutine then begin
              NewRoutineIndex := I;
              Break;
            end;
          end;
        end;
      end;

      if RebuildNow then begin
        { Update to new routines }
        var Routines: TArray<String> := [];
        if FLiveCodeSection <> nil then begin
          const Section = FLiveCodeSection.Section;
          SetLength(Routines, Section.RoutineCount);
          for var I := 0 to Section.RoutineCount-1 do begin
            const Routine = Section.Routines[I];
            if Routine.BodilessType = btForward then
              Routines[I] := Routine.Name + ' (forward)' { Do not localize }
            else
              Routines[I] := Routine.Name;
          end;
        end;
        SetComboBoxItems(FComboBox2, Routines, NewRoutineIndex);
      end;

      { Select caret routine }
      if FComboBox2.ItemIndex <> NewRoutineIndex then
        FComboBox2.ItemIndex := NewRoutineIndex;
    end else begin
      RebuildRoutinesTimerUpdate(True); { Cancel any queued }
      TLiveScriptObjectFactory.ReleaseAndNil(FLiveCodeSection);
      SetComboBoxItems(FComboBox2, [], -1);
    end;

    FChangeCountAtRoutinesSet := ChangeCount;

    if FCaretInCodeSection <> CaretInCodeSection then begin
      FCaretInCodeSection := CaretInCodeSection;
      if Assigned(FOnCaretInCodeSectionChange) then
        FOnCaretInCodeSectionChange(Self);
    end;
  end;
end;

procedure TNavigator.UpdateFromCaret;
begin
  UpdateFromCaret(False, False);
end;

end.
