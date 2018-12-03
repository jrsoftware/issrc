unit TaskDialog;

{
  Inno Setup
  Copyright (C) 1997-2018 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TaskDialogMsgBox function integrating with CmnFunc's MsgBox functions
}

interface

uses
  Windows, CmnFunc;

function TaskDialogMsgBox(const Text, Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal; const ShieldButton: Integer): Integer;

implementation

uses
  SysUtils, Commctrl, CmnFunc2, InstFunc, PathFunc;

var
  TaskDialogIndirectFunc: function(const pTaskConfig: TTaskDialogConfig;
    pnButton: PInteger; pnRadioButton: PInteger;
    pfVerificationFlagChecked: PBOOL): HRESULT; stdcall;

function ShieldButtonCallback(hwnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM; lpRefData: LONG_PTR): HResult; stdcall;
begin
  if (msg = TDN_CREATED) and (lpRefData <> 0) then
    SendMessage(hwnd, TDM_SET_BUTTON_ELEVATION_REQUIRED_STATE, lpRefData, 1);
  Result := S_OK;
end;

{.$DEFINE TESTBUTTONS}

function DoTaskDialog(const hWnd: HWND; const Text, Caption, Icon: PWideChar; const Buttons: Cardinal; const ShieldButton: Integer; const RightToLeft: Boolean; var ModalResult: Integer): Boolean;
var
  Config: TTaskDialogConfig;
{$IFDEF TESTBUTTONS}
  Buttons: TTaskDialogButtons;
  Button: TTaskDialogButtonItem;
{$ENDIF}
begin
  if Assigned(TaskDialogIndirectFunc) then begin
    try
      ZeroMemory(@Config, Sizeof(Config));
      Config.cbSize := SizeOf(Config);
      if RightToLeft then
        Config.dwFlags := Config.dwFlags or TDF_RTL_LAYOUT;
      Config.hwndParent := hWnd;
      Config.dwCommonButtons := Buttons;
      Config.pszWindowTitle := Caption;
      Config.pszMainIcon := Icon;
      Config.pszContent := Text;
      if ShieldButton <> 0 then begin
        Config.pfCallback := ShieldButtonCallback;
        Config.lpCallbackData := ShieldButton;
      end;
{$IFDEF TESTBUTTONS}
      Buttons := TTaskDialogButtons.Create(nil, TTaskDialogButtonItem);
      try
        //Config.dwFlags := Config.dwFlags or TDF_USE_COMMAND_LINKS;
        Button := TTaskDialogButtonItem(Buttons.Add);
        Button.ModalResult := 999;
        Button.Caption := 'My OK';
        Config.pButtons := Buttons.Buttons;
        Config.cButtons := Buttons.Count;
{$ENDIF}
//fixme: disable stuff?
        Result := TaskDialogIndirectFunc(Config, @ModalResult, nil, nil) = S_OK;
{$IFDEF TESTBUTTONS}
      finally
        Buttons.Free;
      end;
{$ENDIF}
    except
      Result := False;
    end;
  end else
    Result := False;
end;

function TaskDialogMsgBox(const Text, Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal; const ShieldButton: Integer): Integer;
var
  Icon: PChar;
  TDButtons: Cardinal;
begin
  case Typ of
    mbInformation: Icon := TD_INFORMATION_ICON;
    mbError: Icon := TD_WARNING_ICON;
    mbCriticalError: Icon := TD_ERROR_ICON;
  else
    Icon := nil; { No other TD_ constant available, MS recommends to use no icon for questions now and the old icon should only be used for help entries }
  end;
  case Buttons of
    MB_YESNOCANCEL: TDButtons := TDCBF_YES_BUTTON or TDCBF_NO_BUTTON or TDCBF_CANCEL_BUTTON;
  else
    begin
      InternalError('TaskDialogMsgBox: Invalid Buttons');
      TDButtons := 0; { Silence compiler }
    end;
  end;
 if not DoTaskDialog(0 {fixme}, PChar(Text), GetMessageBoxCaption(PChar(Caption), Typ), Icon, TDButtons, ShieldButton, GetMessageBoxRightToLeft, Result) then
    Result := MsgBox(Text, Caption, Typ, Buttons);
end;

procedure InitCommonControls; external comctl32 name 'InitCommonControls';

initialization
  InitCommonControls;
  TaskDialogIndirectFunc := GetProcAddress(GetModuleHandle(comctl32), 'TaskDialogIndirect');

end.
