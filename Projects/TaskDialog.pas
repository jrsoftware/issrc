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
  CmnFunc;

function TaskDialogMsgBox(const Instruction, TaskDialogText, MsgBoxText, Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal; const ButtonLabels: array of String; const ShieldButton: Integer): Integer;

implementation

uses
  Windows, Classes, StrUtils, Forms, Dialogs, SysUtils, Commctrl, CmnFunc2, InstFunc, PathFunc;

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


function DoTaskDialog(const hWnd: HWND; const Instruction, Text, Caption, Icon: PWideChar; const CommonButtons: Cardinal; const ButtonLabels: array of String; const ButtonIDs: array of Integer; const ShieldButton: Integer; const RightToLeft: Boolean; var ModalResult: Integer): Boolean;
var
  Config: TTaskDialogConfig;
  NButtonLabelsAvailable: Integer;
  ButtonItems: TTaskDialogButtons;
  ButtonItem: TTaskDialogButtonItem;
  I: Integer;
begin
  if Assigned(TaskDialogIndirectFunc) then begin
    try
      ZeroMemory(@Config, Sizeof(Config));
      Config.cbSize := SizeOf(Config);
      if RightToLeft then
        Config.dwFlags := Config.dwFlags or TDF_RTL_LAYOUT;
      Config.hwndParent := hWnd;
      Config.dwCommonButtons := CommonButtons;
      Config.pszWindowTitle := Caption;
      Config.pszMainIcon := Icon;
      Config.pszMainInstruction := Instruction;
      Config.pszContent := Text;
      if ShieldButton <> 0 then begin
        Config.pfCallback := ShieldButtonCallback;
        Config.lpCallbackData := ShieldButton;
      end;
      ButtonItems := nil;
      try
        NButtonLabelsAvailable := Length(ButtonLabels);
        if NButtonLabelsAvailable <> 0 then begin
          ButtonItems := TTaskDialogButtons.Create(nil, TTaskDialogButtonItem);
          Config.dwFlags := Config.dwFlags or TDF_USE_COMMAND_LINKS;
          for I := 0 to NButtonLabelsAvailable-1 do begin
            ButtonItem := TTaskDialogButtonItem(ButtonItems.Add);
            ButtonItem.Caption := ButtonLabels[I];
            ButtonItem.ModalResult := ButtonIDs[I];
          end;
          Config.pButtons := ButtonItems.Buttons;
          Config.cButtons := ButtonItems.Count;
        end;
        Result := TaskDialogIndirectFunc(Config, @ModalResult, nil, nil) = S_OK;
      finally
        ButtonItems.Free;
      end;
    except
      Result := False;
    end;
  end else
    Result := False;
end;

function TaskDialogMsgBox(const Instruction, TaskDialogText, MsgBoxText, Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal; const ButtonLabels: array of String; const ShieldButton: Integer): Integer;
var
  Icon: PChar;
  TDCommonButtons: Cardinal;
  NButtonLabelsAvailable: Integer;
  ButtonIDs: array of Integer;
begin
  case Typ of
    mbInformation: Icon := TD_INFORMATION_ICON;
    mbError: Icon := TD_WARNING_ICON;
    mbCriticalError: Icon := TD_ERROR_ICON;
  else
    Icon := nil; { No other TD_ constant available, MS recommends to use no icon for questions now and the old icon should only be used for help entries }
  end;
  NButtonLabelsAvailable := Length(ButtonLabels);
  case Buttons of
    MB_YESNOCANCEL:
      begin
        if NButtonLabelsAvailable = 0 then
          TDCommonButtons := TDCBF_YES_BUTTON or TDCBF_NO_BUTTON or TDCBF_CANCEL_BUTTON
        else begin
          TDCommonButtons := TDCBF_CANCEL_BUTTON;
          ButtonIDs := [IDYES, IDNO];
        end;
      end;
    else
      begin
        InternalError('TaskDialogMsgBox: Invalid Buttons');
        TDCommonButtons := 0; { Silence compiler }
      end;
  end;
  if Length(ButtonIDs) <> NButtonLabelsAvailable then
    InternalError('TaskDialogMsgBox: Invalid ButtonLabels');
  if not DoTaskDialog(Application.Handle {fixme}, PChar(Instruction), PChar(TaskDialogText), GetMessageBoxCaption(PChar(Caption), Typ), Icon, TDCommonButtons, ButtonLabels, ButtonIDs, ShieldButton, GetMessageBoxRightToLeft, Result) then
    Result := MsgBox(MsgBoxText, IfThen(Instruction <> '', Instruction, Caption), Typ, Buttons);
end;

procedure InitCommonControls; external comctl32 name 'InitCommonControls';

initialization
  InitCommonControls;
  TaskDialogIndirectFunc := GetProcAddress(GetModuleHandle(comctl32), 'TaskDialogIndirect');

end.
