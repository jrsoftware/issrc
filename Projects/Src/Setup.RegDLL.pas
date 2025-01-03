unit Setup.RegDLL;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Registers 32-bit/64-bit DLL-based OLE servers in a child process (regsvr32.exe)
}

interface

uses
  Windows;

procedure RegisterServer(const AUnregister: Boolean; const AIs64Bit: Boolean;
  const Filename: String; const AFailCriticalErrors: Boolean);

implementation

uses
  SysUtils, Forms, PathFunc, Shared.CommonFunc.Vcl, Shared.CommonFunc, Setup.InstFunc, SetupLdrAndSetup.Messages, Shared.SetupMessageIDs,
  Setup.LoggingFunc, SetupLdrAndSetup.RedirFunc, Setup.MainFunc;

function WaitForAndCloseProcessHandle(var AProcessHandle: THandle): DWORD;
var
  WaitResult: DWORD;
begin
  try
    repeat
      { Process any pending messages first because MsgWaitForMultipleObjects
        (called below) only returns when *new* messages arrive }
      Application.ProcessMessages;
      WaitResult := MsgWaitForMultipleObjects(1, AProcessHandle, False, INFINITE, QS_ALLINPUT);
    until WaitResult <> WAIT_OBJECT_0+1;
    if WaitResult = WAIT_FAILED then
      Win32ErrorMsg('MsgWaitForMultipleObjects');
    if not GetExitCodeProcess(AProcessHandle, Result) then
      Win32ErrorMsg('GetExitCodeProcess');
  finally
    CloseHandle(AProcessHandle);
  end;
end;

procedure RegisterServerUsingRegSvr32(const AUnregister: Boolean;
  const AIs64Bit: Boolean; const Filename: String);
var
  SysDir, CmdLine: String;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ExitCode: DWORD;
begin
  SysDir := GetSystemDir;
  CmdLine := '"' + AddBackslash(SysDir) + 'regsvr32.exe"';
  if AUnregister then
    CmdLine := CmdLine + ' /u';
  CmdLine := CmdLine + ' /s "' + Filename + '"';
  if AIs64Bit then
    Log('Spawning 64-bit RegSvr32: ' + CmdLine)
  else
    Log('Spawning 32-bit RegSvr32: ' + CmdLine);

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  if not CreateProcessRedir(AIs64Bit, nil, PChar(CmdLine), nil, nil, False,
     CREATE_DEFAULT_ERROR_MODE, nil, PChar(SysDir), StartupInfo,
     ProcessInfo) then
    Win32ErrorMsg('CreateProcess');
  CloseHandle(ProcessInfo.hThread);
  ExitCode := WaitForAndCloseProcessHandle(ProcessInfo.hProcess);
  if ExitCode <> 0 then
    raise Exception.Create(FmtSetupMessage1(msgErrorRegSvr32Failed,
      Format('0x%x', [ExitCode])));
end;

procedure RegisterServer(const AUnregister: Boolean; const AIs64Bit: Boolean;
  const Filename: String; const AFailCriticalErrors: Boolean);
var
  WindowDisabler: TWindowDisabler;
begin
  if AIs64Bit and not IsWin64 then
    InternalError('Cannot register 64-bit DLLs on this version of Windows');

  { Disable windows so the user can't utilize our UI while the child process
    is running }
  WindowDisabler := TWindowDisabler.Create;
  try
    { To get the "WRP Mitigation" compatibility hack which a lot of DLLs
      require, we must use regsvr32.exe to handle the (un)registration. }
    RegisterServerUsingRegSvr32(AUnregister, AIs64Bit, Filename);
  finally
    WindowDisabler.Free;
  end;
end;

end.
