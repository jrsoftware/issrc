unit RegDLL;

{
  Inno Setup
  Copyright (C) 1997-2008 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Registers 32-bit/64-bit DLL-based OLE servers in a child process (either
  regsvr32.exe or our own _RegDLL.tmp, depending on the Windows version)

  $jrsoftware: issrc/Projects/RegDLL.pas,v 1.7 2008/10/11 20:30:45 jr Exp $
}

interface

uses
  Windows;

procedure RegisterServer(const AUnregister: Boolean; const AIs64Bit: Boolean;
  const Filename: String; const AFailCriticalErrors: Boolean);

implementation

uses
  SysUtils, Forms, PathFunc, CmnFunc, CmnFunc2, InstFunc, Msgs, MsgIDs,
  Logging, RedirFunc, Main;

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

{$IFNDEF UNICODE}
procedure RegisterServerUsingRegDLL(const AUnregister: Boolean;
  const Filename: String; const AFailCriticalErrors: Boolean);
type
  { This record is prepared and passed to the child process via shared memory.
    The child process modifies the 'out' fields as appropriate. }
  PRegProcessData = ^TRegProcessData;
  TRegProcessData = record
    Version: DWORD;                     { [in]  = RegProcessDataVersion }
    Size: DWORD;                        { [in]  = SizeOf(TRegProcessData) }
    Result: DWORD;                      { [out] Result of registration (rr*) }
    ResultCode: DWORD;                  { [out] Extended error code }
    Unregister: BOOL;                   { [in]  False if registering, True if unreg. }
    FailCriticalErrors: BOOL;           { [in]  Controls error mode }
    Filename: array[0..4095] of AnsiChar;   { [in]  DLL filename }
    Directory: array[0..4095] of AnsiChar;  { [in]  Directory to change to }
  end;
const
  RegProcessDataVersion = 3;

  { TRegProcessData Result values }
  rrOleInitializeFailed = 1;
  rrLoadLibraryFailed = 2;
  rrGetProcAddressFailed = 3;
  rrDllFunctionCalled = 4;

  { Process exit codes, full DWORD }
  ecSuccess = $1C9B28DA;

  RegFunctionNames: array[Boolean] of PChar =
    ('DllRegisterServer', 'DllUnregisterServer');

  InheritableSecurity: TSecurityAttributes = (
    nLength: SizeOf(InheritableSecurity); lpSecurityDescriptor: nil;
    bInheritHandle: True);
var
  TrueValue: BOOL;
  MutexHandle, MapHandle: THandle;
  Data: PRegProcessData;
  ExeFilename, CmdLine: String;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  WaitResult, ExitCode: DWORD;
begin
  Log('Spawning _RegDLL.tmp');
  MapHandle := 0;
  Data := nil;
  { This TrueValue nonsense is to work around a bug in CreateMutex:
    it doesn't interpret -1 (what D3+ passes for True) as TRUE, only 1. }
  Integer(TrueValue) := 1;
  MutexHandle := CreateMutex(@InheritableSecurity, TrueValue, nil);
  if MutexHandle = 0 then
    Win32ErrorMsg('CreateMutex');
  try
    MapHandle := CreateFileMapping(INVALID_HANDLE_VALUE, @InheritableSecurity,
      PAGE_READWRITE, 0, SizeOf(Data^), nil);
    if MapHandle = 0 then
      Win32ErrorMsg('CreateFileMapping');
    Data := MapViewOfFile(MapHandle, FILE_MAP_WRITE, 0, 0, SizeOf(Data^));
    if Data = nil then
      Win32ErrorMsg('MapViewOfFile');

    FillChar(Data^, SizeOf(Data^), 0);
    Data.Version := RegProcessDataVersion;
    Data.Size := SizeOf(Data^);
    Data.Unregister := AUnregister;
    Data.FailCriticalErrors := AFailCriticalErrors;
    StrPLCopy(Data.Filename, Filename, (SizeOf(Data.Filename) div SizeOf(Data.Filename[0])) - 1);
    { For consistency with regsvr32-based registration, we no longer change
      to the directory containing the DLL: }
    //StrPLCopy(Data.Directory, PathExtractDir(Filename), (SizeOf(Data.Directory) div SizeOf(Data.Directory[0])) - 1);
    { We are the initial owner of the mutex. Release the mutex after all
      changes have been made to the record. This should impose a "release"
      memory barrier on architectures that require it. }
    if not ReleaseMutex(MutexHandle) then
      Win32ErrorMsg('ReleaseMutex');

    ExeFilename := AddBackslash(TempInstallDir) + '_isetup\_RegDLL.tmp';
    CmdLine := Format('_RegDLL.tmp %u %u', [MapHandle, MutexHandle]);
    FillChar(StartupInfo, SizeOf(StartupInfo), 0);
    StartupInfo.cb := SizeOf(StartupInfo);
    if not CreateProcess(PChar(ExeFilename), PChar(CmdLine), nil, nil,
       True, CREATE_DEFAULT_ERROR_MODE, nil, PChar(GetSystemDir),
       StartupInfo, ProcessInfo) then
      Win32ErrorMsg('CreateProcess');
    CloseHandle(ProcessInfo.hThread);
    ExitCode := WaitForAndCloseProcessHandle(ProcessInfo.hProcess);
    if ExitCode <> ecSuccess then
      InternalErrorFmt('REGDLL failed with exit code 0x%x', [ExitCode]);
    { Acquire the mutex ourself to ensure that all writes made to the record
      by the child process are visible to us in a multiprocessor scenario.
      (If we successfully obtain the mutex, then the child process has called
      ReleaseMutex, which imposes a "release" memory barrier, flushing prior
      writes.) }
    WaitResult := WaitForSingleObject(MutexHandle, INFINITE);
    if WaitResult <> WAIT_OBJECT_0 then
      InternalErrorFmt('REGDLL mutex wait failed (%d, %d)', [WaitResult, GetLastError]);

    case Data.Result of
      rrOleInitializeFailed: begin
          RaiseOleError('OleInitialize', Data.ResultCode);
        end;
      rrLoadLibraryFailed: begin
          Win32ErrorMsgEx('LoadLibrary', Data.ResultCode);
        end;
      rrGetProcAddressFailed: begin
          Win32ErrorMsgEx('GetProcAddress', Data.ResultCode);
        end;
      rrDllFunctionCalled: begin
          if FAILED(Data.ResultCode) then
            RaiseOleError(RegFunctionNames[AUnregister], Data.ResultCode);
        end;
    else
      InternalErrorFmt('REGDLL returned unknown result code %d', [Data.Result]);
    end;
  finally
    if Assigned(Data) then
      UnmapViewOfFile(Data);
    if MapHandle <> 0 then
      CloseHandle(MapHandle);
    CloseHandle(MutexHandle);
  end;
end;
{$ENDIF}

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
    { On Windows Vista, to get the "WRP Mitigation" compatibility hack which
      a lot of DLLs a require, we must use regsvr32.exe to handle the
      (un)registration.
      On Windows 2000/XP/2003, use regsvr32.exe as well for behavioral &
      error message consistency.
      On earlier versions, keep using RegDLL. Windows 95 and NT 4.0 (no SP)
      do not ship with regsvr32.exe. Windows 98 and Me do, but I prefer to
      treat all 9x OSes the same to simplify testing.
      Unicode build: Always use regsvr32.exe since there is no Unicode
      version of RegDLL, and we don't have to worry about pre-2000 Windows
      versions anyway. }
    {$IFDEF UNICODE}
    RegisterServerUsingRegSvr32(AUnregister, AIs64Bit, Filename);
    {$ELSE}
    if (WindowsVersion >= Cardinal($05000000)) or AIs64Bit then
      RegisterServerUsingRegSvr32(AUnregister, AIs64Bit, Filename)
    else
      RegisterServerUsingRegDLL(AUnregister, Filename, AFailCriticalErrors);
    {$ENDIF}
  finally
    WindowDisabler.Free;
  end;
end;

end.
