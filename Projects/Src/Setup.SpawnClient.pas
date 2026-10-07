unit Setup.SpawnClient;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Spawn client

  NOTE: These functions are NOT thread-safe. Do not call them from multiple
  threads simultaneously.
}

interface

uses
  Windows, SysUtils, Messages, Setup.InstFunc, Shared.CommonFunc;

function GetSpawnServerFirstProcessWnd: HWND;
procedure InitializeSpawnClient(const AServerSharedMemoryID: String);
function InstExecEx(const RunAsOriginalUser: Boolean;
  const DisableFsRedir: Boolean; const Filename, Params, WorkingDir: String;
  const Wait: TExecWait; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; const OutputReader: TCreateProcessOutputReader;
  var ResultCode: DWORD): Boolean;
function InstShellExecEx(const RunAsOriginalUser: Boolean;
  const Verb, Filename, Params, WorkingDir: String;
  const Wait: TExecWait; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; var ResultCode: DWORD): Boolean;
function IsSpawnServerPresent: Boolean;
function StopSpawnServerProcess(const AExitCode: DWORD): Boolean;

implementation

uses
  Classes, SHA256, Setup.SpawnCommon;

var
  SpawnServerSharedMemory: PSpawnServerSharedMemory;

procedure WriteLongintToStream(const M: TMemoryStream; const Value: Longint);
begin
  M.WriteBuffer(Value, SizeOf(Value));
end;

procedure WriteStringToStream(const M: TMemoryStream; const Value: String);
var
  Len: Integer;
begin
  Len := Length(Value);
  if Len > $FFFF then
    InternalError('WriteStringToStream: Length limit exceeded');
  WriteLongintToStream(M, Len);
  if Len > 0 then
    M.WriteBuffer(Value[1], Len * SizeOf(Value[1]));
end;

procedure AllowSpawnServerToSetForegroundWindow;
{ This is called to allow processes started by the spawn server process to
  come to the foreground, above the current process's windows. The effect
  normally lasts until new input is generated (a keystroke or click, not
  simply mouse movement).
  Note: If the spawn server process has no visible windows, it seems this
  isn't needed; the process can set the foreground window as it pleases.
  If it does have a visible window, though, it definitely is needed (e.g. in
  the /DebugSpawnServer case). Let's not rely on any undocumented behavior and
  call AllowSetForegroundWindow unconditionally. }
var
  PID: DWORD;
  AllowSetForegroundWindowFunc: function(dwProcessId: DWORD): BOOL; stdcall;
begin
  if GetWindowThreadProcessId(SpawnServerSharedMemory.ServerWnd, @PID) <> 0 then begin
    AllowSetForegroundWindowFunc := GetProcAddress(GetModuleHandle(user32),
      'AllowSetForegroundWindow');
    if Assigned(AllowSetForegroundWindowFunc) then
      AllowSetForegroundWindowFunc(PID);
  end;
end;

function CallSpawnServer(var M: TMemoryStream;
  const ProcessMessagesProc: TProcedure; var ResultCode: DWORD): Boolean;

  procedure CheckIfServerWndValid;
  begin
    { ServerWnd can be 0 if the server shut down cleanly (though it shouldn't
      do so while we're still running), and it can be nonzero but invalid if
      the server process was killed or is shutting down right at this moment }
    const Wnd = SpawnServerSharedMemory.ServerWnd;
    if (Wnd = 0) or not IsWindow(Wnd) then begin
      { Zero to stop an invalid handle from being accessed any further }
      SpawnServerSharedMemory.ServerWnd := 0;
      InternalErrorFmt('CallSpawnServer: Server window invalid ($%x)', [Wnd]);
    end;
  end;

begin
  CheckIfServerWndValid;

  const SM = SpawnServerSharedMemory;
  const DataSize = M.Size;
  if (DataSize <= 0) or (DataSize > SizeOf(SM.LockedFields.Data)) then
    InternalError('CallSpawnServer: Data size out of range');

  { Acquire lock. This fails for reentrant invocations, or if a previous
    call raised an exception outside of the smsClientHandlingResult state. We
    don't bother trying to recover from such exceptions because the error
    condition is likely permanent (e.g., an invalid ServerWnd won't become
    valid again). }
  if SM.TryChangeLockState(smsFree, smsClientPreparing) <> smsFree then
    InternalError('CallSpawnServer: State not free');

  SM.LockedFields.RequestResult := smrUnknown;
  SM.LockedFields.ExecResult := False;
  SM.LockedFields.ExecResultCode := DWORD(-1);
  SM.LockedFields.DataSize := Cardinal(DataSize);
  SM.LockedFields.DataHash := SHA256Buf(M.Memory^, Cardinal(DataSize));
  Move(M.Memory^, SM.LockedFields.Data, NativeInt(DataSize));
  FreeAndNil(M);  { It isn't needed anymore }

  { Release lock; the server will acquire it next }
  AllowSpawnServerToSetForegroundWindow;
  SM.TryChangeLockState(smsClientPreparing, smsHandOffToServer);

  { Tell the server to begin processing the request, retrying if the server
    isn't yet ready to accept requests.
    It's technically possible that the server already began processing the
    request if another process mischievously sent a
    WM_SpawnServer_ProcessRequest message to it. No harm is caused by that;
    MsgResult will still be SPAWN_MSGRESULT_OK. }
  while True do begin
    ProcessMessagesProc;
    AllowSpawnServerToSetForegroundWindow;
    const MsgResult = SendMessage(SM.ServerWnd, WM_SpawnServer_ProcessRequest,
      0, 0);
    if MsgResult = SPAWN_MSGRESULT_OK then
      Break;
    if MsgResult <> SPAWN_MSGRESULT_NOT_READY then begin
      { 0 likely means SendMessage failed; re-check if ServerWnd is valid }
      if MsgResult = 0 then
        CheckIfServerWndValid;
      InternalErrorFmt('CallSpawnServer: Unexpected response ($%x)',
        [MsgResult]);
    end;
    Sleep(100);
  end;

  { The server should have picked up the request and acquired the lock,
    advancing the state to smsServerProcessing. After the server's call to
    InstExec/InstShellExec returns, the server will release the lock,
    advancing the state to smsHandBackToClient. Loop until we're able to
    reacquire the lock. }
  while True do begin
    const PrevState = SM.TryChangeLockState(
      smsHandBackToClient, smsClientHandlingResult);
    case PrevState of
      smsServerProcessing: ;
      smsHandBackToClient: Break;
    else
      InternalErrorFmt('CallSpawnServer: Unexpected state (%d)',
        [Ord(PrevState)]);
    end;
    ProcessMessagesProc;
    WaitMessageWithTimeout(10);
    ProcessMessagesProc;
    CheckIfServerWndValid;
  end;

  { Lock was successfully reacquired }
  try
    case SM.LockedFields.RequestResult of
      smrOutOfMemory: OutOfMemoryError;
      smrExecReturned: ;
    else
      InternalErrorFmt('CallSpawnServer: Unexpected request result (%d)',
        [Ord(SM.LockedFields.RequestResult)]);
    end;
    ResultCode := SM.LockedFields.ExecResultCode;
    Result := SM.LockedFields.ExecResult;
  finally
    { Release lock }
    SM.TryChangeLockState(smsClientHandlingResult, smsFree);
  end;
end;

function InstExecEx(const RunAsOriginalUser: Boolean;
  const DisableFsRedir: Boolean; const Filename, Params, WorkingDir: String;
  const Wait: TExecWait; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; const OutputReader: TCreateProcessOutputReader;
  var ResultCode: DWORD): Boolean;
var
  M: TMemoryStream;
begin
  if not RunAsOriginalUser or not Assigned(SpawnServerSharedMemory) then begin
    Result := InstExec(DisableFsRedir, Filename, Params, WorkingDir,
      Wait, ShowCmd, ProcessMessagesProc, OutputReader, ResultCode);
    Exit;
  end;

  M := TMemoryStream.Create;
  try
    WriteLongintToStream(M, Ord(False));  { IsShellExec=False }
    WriteLongintToStream(M, Ord(DisableFsRedir));
    WriteStringToStream(M, Filename);
    WriteStringToStream(M, Params);
    WriteStringToStream(M, WorkingDir);
    WriteLongintToStream(M, Ord(Wait));
    WriteLongintToStream(M, ShowCmd);
    WriteStringToStream(M, GetCurrentDir);

    Result := CallSpawnServer(M, ProcessMessagesProc, ResultCode);
  finally
    M.Free;
  end;
end;

function InstShellExecEx(const RunAsOriginalUser: Boolean;
  const Verb, Filename, Params, WorkingDir: String;
  const Wait: TExecWait; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; var ResultCode: DWORD): Boolean;
var
  M: TMemoryStream;
begin
  if not RunAsOriginalUser or not Assigned(SpawnServerSharedMemory) then begin
    Result := InstShellExec(Verb, Filename, Params, WorkingDir,
      Wait, ShowCmd, ProcessMessagesProc, ResultCode);
    Exit;
  end;

  M := TMemoryStream.Create;
  try
    WriteLongintToStream(M, Ord(True));  { IsShellExec=True }
    WriteStringToStream(M, Verb);
    WriteStringToStream(M, Filename);
    WriteStringToStream(M, Params);
    WriteStringToStream(M, WorkingDir);
    WriteLongintToStream(M, Ord(Wait));
    WriteLongintToStream(M, ShowCmd);
    WriteStringToStream(M, GetCurrentDir);

    Result := CallSpawnServer(M, ProcessMessagesProc, ResultCode);
  finally
    M.Free;
  end;
end;

procedure InitializeSpawnClient(const AServerSharedMemoryID: String);
begin
  if Length(AServerSharedMemoryID) <> 15 then
    InternalError('InitializeSpawnClient: Wrong ID length');
  for var C in AServerSharedMemoryID do
    if not CharInSet(C, ['0'..'9', 'A'..'Z']) then
      InternalError('InitializeSpawnClient: Invalid characters in ID');

  const ObjectName = TSpawnServerSharedMemory.ObjectNamePrefix +
    AServerSharedMemoryID;
  const H = OpenFileMapping(FILE_MAP_WRITE, False, PChar(ObjectName));
  if H = 0 then
    Win32ErrorMsg('OpenFileMapping');
  var SM: PSpawnServerSharedMemory;
  try
    SM := MapViewOfFile(H, FILE_MAP_WRITE, 0, 0, SizeOf(SM^));
    if SM = nil then
      Win32ErrorMsg('MapViewOfFile');
  finally
    { Keeping the handle open isn't necessary; the object stays alive as long
      as there are mapped views }
    CloseHandle(H);
  end;
  try
    if (SM.StructSize <> SizeOf(SM^)) or
       (SM.VersionNumber <> SM.ExpectedVersionNumber) then
      InternalError('InitializeSpawnClient: Wrong size or version');
    if AtomicCmpExchange(Integer(SM.ClientConnected), 1, 0) <> 0 then
      InternalError('InitializeSpawnClient: Client already connected');
  except
    UnmapViewOfFile(SM);
    raise;
  end;
  SpawnServerSharedMemory := SM;

  const Wnd = SM.ServerWnd;
  if Wnd <> 0 then
    PostMessage(Wnd, WM_SpawnServer_ClientConnected, 0, 0);
end;

function IsSpawnServerPresent: Boolean;
begin
  Result := Assigned(SpawnServerSharedMemory);
end;

function GetSpawnServerFirstProcessWnd: HWND;
begin
  if Assigned(SpawnServerSharedMemory) then
    Result := SpawnServerSharedMemory.FirstProcessWnd
  else
    Result := 0;
end;

function StopSpawnServerProcess(const AExitCode: DWORD): Boolean;
begin
  if not Assigned(SpawnServerSharedMemory) then
    Exit(False);

  SpawnServerSharedMemory.ExitNowExitCode := AExitCode;
  AtomicExchange(Integer(SpawnServerSharedMemory.ExitNowRequested), Ord(True));

  { Post a message to wake the server's RespawnProcess function from a
    waiting-for-message state so that it re-checks ExitNowRequested.
    (The server doesn't have any handling for this specific message; the point
    is just to wake it up.) }
  const Wnd = SpawnServerSharedMemory.ServerWnd;
  Result := (Wnd <> 0) and PostMessage(Wnd, WM_SpawnServer_ExitNow, 0, 0);
end;

end.
