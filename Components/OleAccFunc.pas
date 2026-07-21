unit OleAccFunc;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  MSAA function loading
}

interface

uses
  Winapi.Windows;

const
  CLSID_AccPropServices: TGUID = '{B5F8350B-0548-48B1-A6EE-88BD00B4A5E7}';
  PROPID_ACC_NAME: TGUID = '{608D3DF8-8128-4AA7-A428-F55E49267291}';

var
  NotifyWinEventFunc: procedure(event: DWORD; hwnd: HWND; idObject: DWORD;
    idChild: Integer); stdcall;
  LresultFromObjectFunc: function(const riid: TGUID; wParam: WPARAM;
    punk: IUnknown): LRESULT; stdcall;
  CreateStdAccessibleObjectFunc: function(hwnd: HWND; idObject: Integer;
    const riid: TGUID; out ppvObject: Pointer): HRESULT; stdcall;

function InitializeOleAcc: Boolean;

implementation

uses
  Winapi.ActiveX, System.SysUtils, PathFunc;

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
