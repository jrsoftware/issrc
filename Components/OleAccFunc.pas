unit OleAccFunc;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  MSAA function loading and helpers
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Controls;

const
  CLSID_AccPropServices: TGUID = '{B5F8350B-0548-48B1-A6EE-88BD00B4A5E7}';
  PROPID_ACC_NAME: TGUID = '{608D3DF8-8128-4AA7-A428-F55E49267291}';

type
  { Sets an MSAA accessible name on a control. Frees automatically. }
  TWinControlMSAANameHook = class(TComponent)
  private
    FControl: TWinControl;
    FMSAAName: String;
    FSavedWindowProc: TWndMethod;
    procedure WindowProc(var Message: TMessage);
  public
    constructor Create(const AControl: TWinControl; const AMSAAName: String); reintroduce;
    destructor Destroy; override;
  end;

var
  NotifyWinEventFunc: procedure(event: DWORD; hwnd: HWND; idObject: DWORD;
    idChild: Integer); stdcall;
  LresultFromObjectFunc: function(const riid: TGUID; wParam: WPARAM;
    punk: IUnknown): LRESULT; stdcall;
  CreateStdAccessibleObjectFunc: function(hwnd: HWND; idObject: Integer;
    const riid: TGUID; out ppvObject: Pointer): HRESULT; stdcall;

function InitializeOleAcc: Boolean;
procedure SetOrClearNameForMSAA(const AWindow: HWND; const AName: string);

implementation

uses
  Winapi.ActiveX, Winapi.oleacc, System.SysUtils, PathFunc;

var
  OleAccInited: Boolean;
  OleAccAvailable: Boolean;

function InitializeOleAcc: Boolean;

  function GetSystemDir: String;
  var
    Buf: array[0..MAX_PATH-1] of Char;
  begin
    GetSystemDirectory(Buf, SizeOf(Buf) div SizeOf(Buf[0]));
    Result := StrPas(Buf);
  end;

begin
  if not OleAccInited then begin
    const M = LoadLibrary(PChar(AddBackslash(GetSystemDir) + 'oleacc.dll'));
    if M <> 0 then begin
      LresultFromObjectFunc := GetProcAddress(M, 'LresultFromObject');
      CreateStdAccessibleObjectFunc := GetProcAddress(M, 'CreateStdAccessibleObject');
      if Assigned(LresultFromObjectFunc) and
         Assigned(CreateStdAccessibleObjectFunc) then
        OleAccAvailable := True;
    end;
    OleAccInited := True;
  end;
  Result := OleAccAvailable;
end;

procedure SetOrClearNameForMSAA(const AWindow: HWND; const AName: string);
{ Annotates the name property of a control. Call this again with AName set
  to an empty string before destroying the control. Also see
  https://learn.microsoft.com/en-us/windows/win32/winauto/ensure-that-ui-elements-are-named-correctly }
begin
  var Services: IAccPropServices;
  if (CoCreateInstance(CLSID_AccPropServices, nil, CLSCTX_INPROC_SERVER,
      IAccPropServices, Services) = S_OK) and (Services <> nil) then begin
    if AName <> '' then
      Services.SetHwndPropStr(wireHWND(AWindow)^, DWORD(OBJID_CLIENT),
        CHILDID_SELF, PROPID_ACC_NAME, PChar(AName))
    else begin
      var PropId := PROPID_ACC_NAME;
      Services.ClearHwndProps(wireHWND(AWindow)^, DWORD(OBJID_CLIENT),
        CHILDID_SELF, PropId, 1);
    end;
  end;
end;

{ TWinControlMSAANameHook }

constructor TWinControlMSAANameHook.Create(const AControl: TWinControl;
  const AMSAAName: String);
begin
  inherited Create(AControl);
  FControl := AControl;
  FMSAAName := AMSAAName;
  FSavedWindowProc := FControl.WindowProc;
  FControl.WindowProc := WindowProc;
  if FControl.HandleAllocated then
    SetOrClearNameForMSAA(FControl.Handle, FMSAAName);
end;

destructor TWinControlMSAANameHook.Destroy;
begin
  FControl.WindowProc := FSavedWindowProc;
  inherited;
end;

procedure TWinControlMSAANameHook.WindowProc(var Message: TMessage);
begin
  if Message.Msg = WM_DESTROY then
    SetOrClearNameForMSAA(FControl.Handle, '');
  FSavedWindowProc(Message);
  if Message.Msg = WM_CREATE then
    SetOrClearNameForMSAA(FControl.Handle, FMSAAName);
end;

{ Note: This COM initialization code based on code from DBTables }
var
  SaveInitProc: Pointer;
  NeedToUninitialize: Boolean;

procedure InitCOM;
begin
  if SaveInitProc <> nil then TProcedure(SaveInitProc);
  NeedToUninitialize := SUCCEEDED(CoInitialize(nil));
end;

initialization
  if not IsLibrary then begin
    SaveInitProc := InitProc;
    InitProc := @InitCOM;
  end;
  NotifyWinEventFunc := GetProcAddress(GetModuleHandle(user32), 'NotifyWinEvent');

finalization
  if NeedToUninitialize then
    CoUninitialize;
end.
