unit OleAccFunc;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  MSAA function loading and COM initialization
}

interface

uses
  Windows;

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
  ActiveX, SysUtils, PathFunc;

var
  OleAccInited: BOOL;
  OleAccAvailable: BOOL;

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
