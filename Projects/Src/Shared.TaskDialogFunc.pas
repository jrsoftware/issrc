unit Shared.TaskDialogFunc;

{
  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TaskDialogMsgBox function integrating with CmnFunc's MsgBox functions
}

interface

uses
  Windows, Shared.CommonFunc.Vcl;

function TaskDialogMsgBox(const Icon, Instruction, Text, Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal; const ButtonLabels: array of String; const ShieldButton: Integer; const VerificationText: String = ''; const pfVerificationFlagChecked: PBOOL = nil): Integer;

implementation

uses
  Classes, StrUtils, Math, Forms, Dialogs, SysUtils,
  Commctrl, Shared.CommonFunc, {$IFDEF SETUPPROJ} Setup.InstFunc, {$ENDIF} PathFunc;

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

function DoTaskDialog(const hWnd: HWND; const Instruction, Text, Caption, Icon: PWideChar; const CommonButtons: Cardinal; const ButtonLabels: array of String; const ButtonIDs: array of Integer; const ShieldButton: Integer; const RightToLeft: Boolean; const TriggerMessageBoxCallbackFuncFlags: LongInt; var ModalResult: Integer; const VerificationText: PWideChar; const pfVerificationFlagChecked: PBOOL): Boolean;
var
  Config: TTaskDialogConfig;
  NButtonLabelsAvailable: Integer;
  ButtonItems: TTaskDialogButtons;
  ButtonItem: TTaskDialogButtonItem;
  I: Integer;
  ActiveWindow: Windows.HWND;
  WindowList: Pointer;
begin
  if Assigned(TaskDialogIndirectFunc) then begin
    ZeroMemory(@Config, Sizeof(Config));
    Config.cbSize := SizeOf(Config);
    if RightToLeft then
      Config.dwFlags := Config.dwFlags or TDF_RTL_LAYOUT;
    Config.hInstance := HInstance;
    Config.hwndParent := hWnd;
    Config.dwCommonButtons := CommonButtons;
    Config.pszWindowTitle := Caption;
    Config.pszMainIcon := Icon;
    Config.pszMainInstruction := Instruction;
    Config.pszContent := Text;
    if VerificationText <> '' then
      Config.pszVerificationText := VerificationText;
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
      TriggerMessageBoxCallbackFunc(TriggerMessageBoxCallbackFuncFlags, False);
      ActiveWindow := GetActiveWindow;
      WindowList := DisableTaskWindows(Config.hwndParent);
      try
        Result := TaskDialogIndirectFunc(Config, @ModalResult, nil, pfVerificationFlagChecked) = S_OK;
      finally
        EnableTaskWindows(WindowList);
        SetActiveWindow(ActiveWindow);
        TriggerMessageBoxCallbackFunc(TriggerMessageBoxCallbackFuncFlags, True);
      end;
    finally
      ButtonItems.Free;
    end;
  end else
    Result := False;
end;

procedure DoInternalError(const Msg: String);
begin
  {$IFDEF SETUPPROJ}
    InternalError(Msg);
  {$ELSE}
    raise Exception.Create(Msg);
  {$ENDIF}
end;

function TaskDialogMsgBox(const Icon, Instruction, Text, Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal; const ButtonLabels: array of String; const ShieldButton: Integer; const VerificationText: String = ''; const pfVerificationFlagChecked: PBOOL = nil): Integer;
var
  IconP: PChar;
  TDCommonButtons: Cardinal;
  NButtonLabelsAvailable: Integer;
  ButtonIDs: array of Integer;
begin
  Application.Restore; { See comments in AppMessageBox }
  if Icon <> '' then
    IconP := PChar(Icon)
  else begin
    case Typ of
      mbInformation: IconP := TD_INFORMATION_ICON;
      mbError: IconP := TD_WARNING_ICON;
      mbCriticalError: IconP := TD_ERROR_ICON;
    else
      IconP := nil; { No other TD_ constant available, MS recommends to use no icon for questions now and the old icon should only be used for help entries }
    end;
  end;
  NButtonLabelsAvailable := Length(ButtonLabels);
  case Buttons of
    MB_OK, MB_OKCANCEL:
      begin
        if NButtonLabelsAvailable = 0 then
          TDCommonButtons := TDCBF_OK_BUTTON
        else begin
          TDCommonButtons := 0;
          ButtonIDs := [IDOK];
        end;
        if Buttons = MB_OKCANCEL then
          TDCommonButtons := TDCommonButtons or TDCBF_CANCEL_BUTTON;
      end;
    MB_YESNO, MB_YESNOCANCEL:
      begin
        if NButtonLabelsAvailable = 0 then
          TDCommonButtons := TDCBF_YES_BUTTON or TDCBF_NO_BUTTON
        else begin
          TDCommonButtons := 0;
          ButtonIDs := [IDYES, IDNO];
        end;
        if Buttons = MB_YESNOCANCEL then
          TDCommonButtons := TDCommonButtons or TDCBF_CANCEL_BUTTON;
      end;
    MB_RETRYCANCEL:
      begin
        if NButtonLabelsAvailable = 0 then
          TDCommonButtons := TDCBF_RETRY_BUTTON
        else begin
          TDCommonButtons := 0;
          ButtonIDs := [IDRETRY];
        end;
        TDCommonButtons := TDCommonButtons or TDCBF_CANCEL_BUTTON;
      end;
    MB_ABORTRETRYIGNORE:
      begin
        if NButtonLabelsAvailable = 0 then
          DoInternalError('TaskDialogMsgBox: Invalid ButtonLabels')
        else
          ButtonIDs := [IDRETRY, IDIGNORE, IDABORT]; { Notice the order, abort label must be last }
        TDCommonButtons := 0;
      end;
    else
      begin
        DoInternalError('TaskDialogMsgBox: Invalid Buttons');
        TDCommonButtons := 0; { Silence compiler }
      end;
  end;
  if Length(ButtonIDs) <> NButtonLabelsAvailable then
    DoInternalError('TaskDialogMsgBox: Invalid ButtonLabels');
  if not DoTaskDialog(GetOwnerWndForMessageBox, PChar(Instruction), PChar(Text),
           GetMessageBoxCaption(PChar(Caption), Typ), IconP, TDCommonButtons, ButtonLabels, ButtonIDs, ShieldButton,
           GetMessageBoxRightToLeft, IfThen(Typ in [mbError, mbCriticalError], MB_ICONSTOP, 0), Result, PChar(VerificationText), pfVerificationFlagChecked) then //note that MB_ICONEXCLAMATION (used by mbError) includes MB_ICONSTOP (used by mbCriticalError)
    Result := 0;
end;

procedure InitCommonControls; external comctl32 name 'InitCommonControls';

initialization
  InitCommonControls;
  TaskDialogIndirectFunc := GetProcAddress(GetModuleHandle(comctl32), 'TaskDialogIndirect');

end.
