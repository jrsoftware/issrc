unit IDE.MainForm.NavigationHelper;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler form - Navigation helper which has the UAH helper as ancestor

  Not used by MainForm: it uses IDE.MainForm.FinalHelper instead
}

interface

uses
  Messages,
  Menus,
  IDE.MainForm, IDE.MainForm.UAHHelper,
  IDE.IDEScintEdit;

type
  TMainFormNavigationHelper = class helper(TMainFormUAHHelper) for TMainForm
    procedure RemoveMemoFromNavigation(const AMemo: TIDEScintEdit);
    procedure RemoveMemoBadLinesFromNavigation(const AMemo: TIDEScintEdit);
    procedure UpdateNavigationButtons;
    procedure NavigateBack;
    procedure NavigateForward;
    procedure HandleNavigationAppCommand(var Message: TMessage);
    procedure UpdateNavigationMenu(const Menu: TMenuItem);
    procedure UpdateBackNavigationStack;
    { Private }
    procedure _NavigationMenuItemClick(Sender: TObject);
  end;

implementation

uses
  Windows,
  SysUtils, TypInfo, ComCtrls,
  IDE.HelperFunc, IDE.ScintStylerInnoSetup;

procedure TMainFormNavigationHelper.RemoveMemoFromNavigation(const AMemo: TIDEScintEdit);
begin
  if FNavStacks.RemoveMemo(AMemo) then
    UpdateNavigationButtons;
  if FCurrentNavItem.Memo = AMemo then
    FCurrentNavItem.Invalidate;
end;

procedure TMainFormNavigationHelper.RemoveMemoBadLinesFromNavigation(const AMemo: TIDEScintEdit);
begin
  if FNavStacks.RemoveMemoBadLines(AMemo) then
    UpdateNavigationButtons;
  { We do NOT update FCurrentNav here so it might point to a line that's
    deleted until next UpdateCaretPosPanelAndBackStack by UpdateMemoUI }
end;

procedure TMainFormNavigationHelper.UpdateNavigationButtons;
begin
  ForwardNavButton.Enabled := FNavStacks.Forward.Count > 0;
  BackNavButton.Enabled := (FNavStacks.Back.Count > 0) or
                           ForwardNavButton.Enabled; { for the dropdown }
end;

procedure TMainFormNavigationHelper.NavigateBack;
begin
  { Delphi does not support BTNS_WHOLEDROPDOWN so we can't be like VS which
    can have a disabled back nav button with an enabled dropdown. To avoid
    always showing two dropdowns we keep the back button enabled when we need
    the dropdown. So we need to check for this. }
  if FNavStacks.Back.Count = 0 then begin
    Beep;
    Exit;
  end;

  FNavStacks.Forward.Add(FCurrentNavItem);
  var NewNavItem := FNavStacks.Back.ExtractAt(FNavStacks.Back.Count-1);
  UpdateNavigationButtons;
  FCurrentNavItem := NewNavItem; { Must be done *before* moving }
  MoveCaretAndActivateMemo(NewNavItem.Memo,
    NewNavItem.Memo.GetPositionFromLineColumn(NewNavItem.Line, NewNavItem.Column), False, True, NewNavItem.VirtualSpace);
end;

procedure TMainFormNavigationHelper.NavigateForward;
begin
  FNavStacks.Back.Add(FCurrentNavItem);
  var NewNavItem := FNavStacks.Forward.ExtractAt(FNavStacks.Forward.Count-1);
  UpdateNavigationButtons;
  FCurrentNavItem := NewNavItem; { Must be done *before* moving }
  MoveCaretAndActivateMemo(NewNavItem.Memo,
    NewNavItem.Memo.GetPositionFromLineColumn(NewNavItem.Line, NewNavItem.Column), False, True, NewNavItem.VirtualSpace);
end;

procedure TMainFormNavigationHelper.HandleNavigationAppCommand(var Message: TMessage);
begin
  var Command := GET_APPCOMMAND_LPARAM(Integer(Message.LParam));

  if Command = APPCOMMAND_BROWSER_BACKWARD then begin
    if BackNavButton.Enabled then
      BackNavButton.Click;
    Message.Result := 1;
  end else if Command = APPCOMMAND_BROWSER_FORWARD then begin
    if ForwardNavButton.Enabled then
      ForwardNavButton.Click;
    Message.Result := 1;
  end;
end;

procedure TMainFormNavigationHelper._NavigationMenuItemClick(Sender: TObject);
begin
  var MenuItem := Sender as TMenuItem;
  var Clicks := Abs(MenuItem.Tag);
  if Clicks > 0 then begin
    var ButtonToClick: TToolButton;
    if MenuItem.Tag > 0 then
      ButtonToClick := ForwardNavButton
    else
      ButtonToClick := BackNavButton;
    while Clicks > 0 do begin
      if not ButtonToClick.Enabled then
        raise Exception.Create('not ButtonToClick.Enabled');
      ButtonToClick.Click;
      Dec(Clicks);
    end;
  end;
end;

procedure TMainFormNavigationHelper.UpdateNavigationMenu(const Menu: TMenuItem);

  procedure AddNavItemToMenu(const NavItem: TIDEScintEditNavItem; const Checked: Boolean;
    const ClicksNeeded: NativeInt; const Menu: TMenuItem);
  begin
    if NavItem.Line >= NavItem.Memo.Lines.Count then
      raise Exception.Create('NavItem.Line >= NavItem.Memo.Lines.Count');
    var LineInfo :=  NavItem.Memo.Lines[NavItem.Line];
    if LineInfo.Trim = '' then
      LineInfo := Format('Line %d', [NavItem.Line+1]);

    var Caption: String;
    if MemosTabSet.Visible then
      Caption := Format('%s: %s', [MemosTabSet.Tabs[MemoToTabIndex(NavItem.Memo)], LineInfo])
    else
      Caption := LineInfo;

    var MenuItem := TMenuItem.Create(Menu);
    MenuItem.Caption := DoubleAmp(Caption);
    MenuItem.Checked := Checked;
    MenuItem.RadioItem := True;
    MenuItem.Tag := ClicksNeeded;
    MenuItem.OnClick := _NavigationMenuItemClick;
    Menu.Add(MenuItem);
  end;

begin
  Menu.Clear;

  { Setup dropdown. The result should end up being just like Visual Studio 2022
    which means from top to bottom:
    - Furthest (=oldest) forward item
    - ...
    - Closest (=next) forward item
    - Current position in the active memo, checked
    - Closest (=next) back item
    - ...
    - Furthest (=oldest) back item
    The Tag parameter should be set to the amount of clicks needed to get to
    the item, positive for forward and negative for back }

  for var I := 0 to FNavStacks.Forward.Count-1 do
    AddNavItemToMenu(FNavStacks.Forward[I], False, FNavStacks.Forward.Count-I, Menu);
  AddNavItemToMenu(FCurrentNavItem, True, 0, Menu);
  for var I := FNavStacks.Back.Count-1 downto 0 do
    AddNavItemToMenu(FNavStacks.Back[I], False, -(FNavStacks.Back.Count-I), Menu);
end;

procedure TMainFormNavigationHelper.UpdateBackNavigationStack;
begin
  { Update NavStacks.Back if needed and remember new position }
  var NewNavItem := TIDEScintEditNavItem.Create(FActiveMemo); { This is a record so no need to free }
  if FCurrentNavItem.Valid and FNavStacks.AddNewBackForJump(FCurrentNavItem, NewNavItem) then
    UpdateNavigationButtons;
  FCurrentNavItem := NewNavItem;
end;

end.
