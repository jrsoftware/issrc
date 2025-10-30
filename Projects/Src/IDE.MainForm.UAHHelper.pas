unit IDE.MainForm.UAHHelper;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler form - UAH helper which has the MRU helper as ancestor

  This adds support for dark menu bars, meaning just the row which displays
  'File', 'Edit', etc. Support for dark menus, meaning the actual popups,
  is enabled using SetPreferredAppMode and FlushMenuThemes in IDE.MainForm

  Not used by MainForm: it uses IDE.MainForm.FinalHelper instead
}

interface

uses
  Messages,
  NewUxTheme,
  IDE.MainForm, IDE.MainForm.MRUHelper;

type
  TMainFormUAHHelper = class helper(TMainFormMRUHelper) for TMainForm
    procedure UAHDrawMenu(const UAHMenu: PUAHMenu);
    procedure UAHDrawMenuItem(const UAHDrawMenuItem: PUAHDrawMenuItem);
    procedure UAHDrawMenuBottomLine;
  end;

implementation

uses
  Windows,
  Classes,
  ModernColors;

procedure TMainFormUAHHelper.UAHDrawMenu(const UAHMenu: PUAHMenu);
begin
  var MenuBarInfo: TMenuBarInfo;
  MenuBarInfo.cbSize := SizeOf(MenuBarInfo);
  GetMenuBarInfo(Handle, Integer(OBJID_MENU), 0, MenuBarInfo);

  var WindowRect: TRect;
  GetWindowRect(Handle, WindowRect);

  var Rect := MenuBarInfo.rcBar;
  OffsetRect(Rect, -WindowRect.Left, -WindowRect.Top);

  FillRect(UAHMenu.hdc, Rect, FMenuDarkBackgroundBrush.Handle);
end;

procedure TMainFormUAHHelper.UAHDrawMenuItem(const UAHDrawMenuItem: PUAHDrawMenuItem);
const
  ODS_NOACCEL = $100;
  DTT_TEXTCOLOR = 1;
  MENU_BARITEM = 8;
  MBI_NORMAL = 1;
var
  Buffer: array of Char;
begin
  var MenuItemInfo: TMenuItemInfo;
  MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
  MenuItemInfo.fMask := MIIM_STRING;
  MenuItemInfo.dwTypeData := nil;
  GetMenuItemInfo(UAHDrawMenuItem.um.hmenu, UAHDrawMenuItem.umi.iPosition, True, MenuItemInfo);
  Inc(MenuItemInfo.cch);
  SetLength(Buffer, MenuItemInfo.cch);
  MenuItemInfo.dwTypeData := @Buffer[0];
  GetMenuItemInfo(UAHDrawMenuItem.um.hmenu, UAHDrawMenuItem.umi.iPosition, True, MenuItemInfo);

  var dwFlags: DWORD := DT_CENTER or DT_SINGLELINE or DT_VCENTER;
  if (UAHDrawMenuItem.dis.itemState and ODS_NOACCEL) <> 0 then
    dwFlags := dwFlags or DT_HIDEPREFIX;

  var Inactive := (UAHDrawMenuItem.dis.itemState and ODS_INACTIVE) <> 0;

  var TextColor: TThemeColor;
  if Inactive then
    TextColor := tcMarginFore
  else
    TextColor := tcFore;

  var opts: TDTTOpts;
  opts.dwSize := SizeOf(opts);
  opts.dwFlags := DTT_TEXTCOLOR;
  opts.crText := FTheme.Colors[TextColor];

  var Brush: HBrush;
  { ODS_HOTLIGHT can be set when the menu is inactive so we check Inactive as well. }
  if not Inactive and ((UAHDrawMenuItem.dis.itemState and (ODS_HOTLIGHT or ODS_SELECTED)) <> 0) then
    Brush := FMenuDarkHotOrSelectedBrush.Handle
  else
    Brush := FMenuDarkBackgroundBrush.Handle;

  FillRect(UAHDrawMenuItem.um.hdc, UAHDrawMenuItem.dis.rcItem, Brush);
  DrawThemeTextEx(FMenuThemeData, UAHDrawMenuItem.um.hdc, MENU_BARITEM, MBI_NORMAL, MenuItemInfo.dwTypeData, MenuItemInfo.cch, dwFlags, @UAHDrawMenuItem.dis.rcItem, opts);
end;

{ Should be removed if the main menu ever gets removed }
procedure TMainFormUAHHelper.UAHDrawMenuBottomLine;
begin
  if not (csDestroying in ComponentState) and (FTheme <> nil) and FTheme.Dark then begin
    var ClientRect: TRect;
    Windows.GetClientRect(Handle, ClientRect);
    MapWindowPoints(Handle, 0, ClientRect, 2);

    var WindowRect: TRect;
    GetWindowRect(Handle, WindowRect);

    var Rect := ClientRect;
    OffsetRect(Rect, -WindowRect.Left, -WindowRect.Top);

    Rect.Bottom := Rect.Top;
    Dec(Rect.Top);

    var DC := GetWindowDC(Handle);
    FillRect(DC, Rect, FMenuDarkBackgroundBrush.Handle);
    ReleaseDC(Handle, DC);
  end;
end;

end.
