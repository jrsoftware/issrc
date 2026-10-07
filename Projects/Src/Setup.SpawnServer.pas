unit Setup.SpawnServer;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Spawn server
}

interface

uses
  Windows, SysUtils, Messages, Setup.SpawnCommon;

type
  TSpawnServer = class
  private
    FReadyToServe: Boolean;
    FWnd: HWND;
    FSharedMemoryID: String;
    FSharedMemoryMapping: THandle;
    FSharedMemory: PSpawnServerSharedMemory;
    function HandleExec: TSpawnServerSharedMemory.TRequestResult;
    procedure WndProc(var Message: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    property SharedMemoryID: String read FSharedMemoryID;
  end;

procedure EnterSpawnServerDebugMode;
function NeedToRespawnSelfElevated(const ARequireAdministrator,
  AEmulateHighestAvailable: Boolean): Boolean;
procedure RespawnProcess(const AElevate: Boolean;
  const AExeFilename, AParams: String; const ASpawnServer: TSpawnServer;
  var AExitCode: Integer);
procedure UnsetRespawnBlockEnvironmentVariable;

implementation

{ For debugging only; remove 'x' to enable the define: }
{x$DEFINE SPAWNSERVER_RESPAWN_ALWAYS}

uses
  Classes, Forms, ShellApi, PathFunc, SHA256, Shared.CommonFunc,
  SetupLdrAndSetup.InstFunc, Setup.InstFunc, Setup.MainFunc;

type
  TPtrAndSize = record
    Ptr: ^Byte;
    Size: Cardinal;
  end;

function AtomicReadBool(var ATarget: LongBool): LongBool;
{ Reads the most up-to-date value of ATarget (not a stale cached value), with
  full memory barrier }
begin
  Integer(Result) := AtomicCmpExchange(Integer(ATarget), -2, -2);
end;

procedure ProcessMessagesProc;
begin
  Application.ProcessMessages;
end;

function ExtractBytes(var Data: TPtrAndSize; const Bytes: Cardinal;
  var Value: Pointer): Boolean;
begin
  if Data.Size < Bytes then
    Result := False
  else begin
    Value := Data.Ptr;
    Dec(Data.Size, Bytes);
    Inc(Data.Ptr, Bytes);
    Result := True;
  end;
end;

function ExtractInteger(var Data: TPtrAndSize; var Value: Integer): Boolean;
var
  P: Pointer;
begin
  Result := ExtractBytes(Data, SizeOf(Integer), P);
  if Result then
    Value := Integer(P^);
end;

function ExtractString(var Data: TPtrAndSize; var Value: String): Boolean;
var
  Len: Longint;
  P: Pointer;
begin
  Result := ExtractInteger(Data, Len);
  if Result then begin
    if (Len < 0) or (Len > $FFFF) then
      Result := False
    else begin
      Result := ExtractBytes(Data, Cardinal(Len) * SizeOf(Value[1]), P);
      if Result then
        SetString(Value, PChar(P), Len);
    end;
  end;
end;

const
  TokenElevationTypeDefault = 1;  { User does not have a split token (they're
                                    not an admin, or UAC is turned off) }
  TokenElevationTypeFull = 2;     { Has split token, process running elevated }
  TokenElevationTypeLimited = 3;  { Has split token, process not running
                                    elevated }

function GetTokenElevationType: DWORD;
{ Returns token elevation type (TokenElevationType* constant). In case of
  failure, 0 is returned. }
const
  TokenElevationType = 18;
var
  Token: THandle;
  ElevationType: DWORD;
  ReturnLength: DWORD;
begin
  Result := 0;
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token) then begin
    ElevationType := 0;
    if GetTokenInformation(Token, TTokenInformationClass(TokenElevationType),
       @ElevationType, SizeOf(ElevationType), ReturnLength) then
      Result := ElevationType;
    CloseHandle(Token);
  end;
end;

function NeedToRespawnSelfElevated(const ARequireAdministrator,
  AEmulateHighestAvailable: Boolean): Boolean;
{$IFNDEF SPAWNSERVER_RESPAWN_ALWAYS}
var
  ElevationType: DWORD;
begin
  Result := False;
  if not IsAdminLoggedOn then begin
    if ARequireAdministrator then
      Result := True
    else if AEmulateHighestAvailable then begin
      { Emulate the "highestAvailable" requestedExecutionLevel: respawn if
        the user has a split token and the process isn't running elevated.
        (An inverted test for TokenElevationTypeLimited is used, so that if
        GetTokenElevationType unexpectedly fails or returns some value we
        don't recognize, we default to respawning.) }
      ElevationType := GetTokenElevationType;
      if (ElevationType <> TokenElevationTypeDefault) and
         (ElevationType <> TokenElevationTypeFull) then
        Result := True;
    end;
  end;
end;
{$ELSE}
begin
  { For debugging/testing only: }
  Result := True;
end;
{$ENDIF}

const
  RespawnBlockEnvironmentVariableName =
    'ISETUP_RESPAWNPROCESS_WAS_ALREADY_CALLED';

procedure SetRespawnBlockEnvironmentVariable(const AValue: PChar);
begin
  if not SetEnvironmentVariable(RespawnBlockEnvironmentVariableName, AValue) then
    Win32ErrorMsg('SetEnvironmentVariable');
end;

procedure UnsetRespawnBlockEnvironmentVariable;
{ Drops the "respawn block" variable from the current process's environment,
  if it exists. This is called by RespawnProcess after it has started the new
  process, and also when Setup/Uninstall has determined it doesn't need to
  call RespawnProcess, so that the no-longer-needed environment variable
  doesn't get passed on to any other Inno Setup (un)installers (not respawns
  of this one) that this process or a child process might start later. }
begin
  SetRespawnBlockEnvironmentVariable(nil);
end;

procedure RespawnProcess(const AElevate: Boolean;
  const AExeFilename, AParams: String; const ASpawnServer: TSpawnServer;
  var AExitCode: Integer);
{ Spawns a new, possibly elevated process.
  Notes:
  1. When AElevate=True is passed, the spawned process may not actually be
     elevated / running as administrator. If UAC is disabled, "runas"
     behaves like "open". Also, if a non-admin user is a member of a special
     system group like Backup Operators, they can select their own user account
     at a UAC dialog. Therefore, it is critical that the caller include some
     kind of protection against respawning more than once.
  2. If AExeFilename is on a network drive, the ShellExecuteEx function is
     smart enough to substitute it with a UNC path. }
begin
  { Be extra careful not to accidentally turn into a fork bomb:
    Setup and Uninstall won't call RespawnProcess when they find a command
    line parameter indicating they've already respawned (/SPAWNSM= and
    /INITPROCWND= respectively). But just in case the command line parameter
    isn't passed, received, or parsed correctly for some reason, we employ a
    second defense: an environment variable that's passed on to the child
    process indicating RespawnProcess was called. If we find that variable is
    set here, then we raise a fatal internal error.
    One thing to note: With ShellExecuteEx, when a non-elevated process starts
    an elevated process, the new process's environment is reset, so the
    variable set here will be lost. However, if ShellExecuteEx were to be
    called again by the elevated process, the environment wouldn't be reset,
    so the variable should make it through. So, a second respawn may not be
    stopped, but a third respawn would be. }
  if GetEnv(RespawnBlockEnvironmentVariableName) <> '' then
    InternalError('Setup/Uninstall process was already respawned');
  SetRespawnBlockEnvironmentVariable('1');

  const ExpandedExeFilename = GetFinalFileName(AExeFilename);
  const WorkingDir = GetFinalCurrentDir;

  var ProcessHandle: THandle;
  if AElevate then begin
    if not SameText(PathExtractExt(ExpandedExeFilename), '.exe') then
      InternalError('Cannot respawn self, not named .exe');
    var Info := Default(TShellExecuteInfo);
    Info.cbSize := SizeOf(Info);
    Info.fMask := SEE_MASK_FLAG_NO_UI or SEE_MASK_FLAG_DDEWAIT or
      SEE_MASK_NOCLOSEPROCESS or SEE_MASK_NOZONECHECKS;
    Info.lpVerb := 'runas';
    Info.lpFile := PChar(ExpandedExeFilename);
    Info.lpParameters := PChar(AParams);
    Info.lpDirectory := PChar(WorkingDir);
    Info.nShow := SW_SHOWNORMAL;
    if not ShellExecuteEx(@Info) then begin
      { Don't display error message if user clicked Cancel at UAC dialog }
      if GetLastError = ERROR_CANCELLED then
        Abort;
      Win32ErrorMsg('ShellExecuteEx');
    end;
    if Info.hProcess = 0 then
      InternalError('ShellExecuteEx returned hProcess=0');
    ProcessHandle := Info.hProcess;
  end else begin
    { Use CreateProcess for non-elevated respawns because it will work with
      extensions other than .exe (in case users have been giving their
      installers non-.exe filenames) }
    var CommandLine := '"' + ExpandedExeFilename + '"';
    if AParams <> '' then
      CommandLine := CommandLine + ' ' + AParams;
    var StartupInfo := Default(TStartupInfo);
    var ProcessInfo: TProcessInformation;
    StartupInfo.cb := SizeOf(StartupInfo);
    if not CreateProcess(nil, PChar(CommandLine), nil, nil, False, 0, nil,
       PChar(WorkingDir), StartupInfo, ProcessInfo) then
      Win32ErrorMsg('CreateProcess');
    CloseHandle(ProcessInfo.hThread);
    ProcessHandle := ProcessInfo.hProcess;
  end;

  { Wait for the process to terminate, processing messages in the meantime }
  try
    UnsetRespawnBlockEnvironmentVariable;
    { Only start accepting spawn requests after ShellExecuteEx has returned
      and the environment variable is unset. See also TSpawnServer.WndProc. }
    if Assigned(ASpawnServer) then
      ASpawnServer.FReadyToServe := True;
    var WaitResult: DWORD;
    repeat
      ProcessMessagesProc;
      if Assigned(ASpawnServer) and
         AtomicReadBool(ASpawnServer.FSharedMemory.ExitNowRequested) then begin
        DWORD(AExitCode) := ASpawnServer.FSharedMemory.ExitNowExitCode;
        Exit;
      end;
      WaitResult := MsgWaitForMultipleObjects(1, ProcessHandle, False,
        INFINITE, QS_ALLINPUT);
    until WaitResult <> WAIT_OBJECT_0+1;
    if WaitResult = WAIT_FAILED then
      Win32ErrorMsg('MsgWaitForMultipleObjects');
    { Now that the process has exited, process any remaining messages.
      (If our window is handling notify messages (ANotifyWndPresent=False)
      then there may be an asynchronously-sent "restart request" message
      still queued if MWFMO saw the process terminate before checking for
      new messages.) }
    ProcessMessagesProc;
    if not GetExitCodeProcess(ProcessHandle, DWORD(AExitCode)) then
      Win32ErrorMsg('GetExitCodeProcess');
  finally
    CloseHandle(ProcessHandle);
  end;
end;

procedure EnterSpawnServerDebugMode;
{ For debugging purposes only: Creates a spawn server window, but does not
  start a new process. Displays the server window handle in the taskbar.
  Terminates when F11 is pressed. }
var
  Server: TSpawnServer;
begin
  Server := TSpawnServer.Create;
  try
    { The UInt32 cast prevents sign extension }
    Application.Title := Format('Wnd=$%x', [UInt32(Server.FWnd)]);
    while True do begin
      ProcessMessagesProc;
      if (GetFocus = Application.Handle) and (GetKeyState(VK_F11) < 0) then
        Break;
      WaitMessage;
    end;
  finally
    Server.Free;
  end;
  Halt(1);
end;

{ TSpawnServer }

constructor TSpawnServer.Create;

  procedure CreateSharedMemoryMapping;
  const
    FiveDigitsRange = 36 * 36 * 36 * 36 * 36;
  begin
    var FailureCount := 0;
    while True do begin
      FSharedMemoryID := '';
      for var I := 0 to 2 do
        FSharedMemoryID := FSharedMemoryID +
          UIntToBase36Str(TStrongRandom.GenerateUInt32Range(FiveDigitsRange), 5);

      const ObjectName = TSpawnServerSharedMemory.ObjectNamePrefix +
        FSharedMemoryID;
      { Safety: An extra 64KB ($10000) is reserved but not committed to ensure
        any overrun of Data will always trigger an AV }
      SetLastError(ERROR_SUCCESS);
      const H = CreateFileMapping(INVALID_HANDLE_VALUE, nil,
        SEC_RESERVE or PAGE_READWRITE, 0, SizeOf(FSharedMemory^) + $10000,
        PChar(ObjectName));
      const ErrorCode = GetLastError;
      { ERROR_ALREADY_EXISTS means an existing object was opened; we treat
        that the same as a failure and retry. ERROR_ACCESS_DENIED is also
        possible if the object name exists but the DACL denies access. }
      if H <> 0 then begin
        if ErrorCode <> ERROR_ALREADY_EXISTS then begin
          FSharedMemoryMapping := H;
          Exit;
        end;
        CloseHandle(H);
      end;
      Inc(FailureCount);
      if FailureCount >= 10 then
        Win32ErrorMsgEx('CreateFileMapping', ErrorCode);
    end;
  end;

begin
  inherited;
  FWnd := AllocateHWnd(WndProc);
  if FWnd = 0 then
    RaiseFunctionFailedError('AllocateHWnd');

  CreateSharedMemoryMapping;
  FSharedMemory := MapViewOfFile(FSharedMemoryMapping, FILE_MAP_WRITE, 0, 0,
    SizeOf(FSharedMemory^));
  if FSharedMemory = nil then
    Win32ErrorMsg('MapViewOfFile');
  if VirtualAlloc(FSharedMemory, SizeOf(FSharedMemory^), MEM_COMMIT,
     PAGE_READWRITE) = nil then
    Win32ErrorMsg('VirtualAlloc');
  FSharedMemory.StructSize := SizeOf(FSharedMemory^);
  FSharedMemory.VersionNumber := FSharedMemory.ExpectedVersionNumber;
  FSharedMemory.ServerWnd := UInt32(FWnd);
  if SetupLdrMode then
    FSharedMemory.FirstProcessWnd := UInt32(SetupLdrWnd)
  else
    FSharedMemory.FirstProcessWnd := UInt32(FWnd);
  MemoryBarrier;
end;

destructor TSpawnServer.Destroy;
begin
  if Assigned(FSharedMemory) then begin
    FSharedMemory.ServerWnd := 0;
    FSharedMemory.FirstProcessWnd := 0;
    UnmapViewOfFile(FSharedMemory);
  end;
  CloseHandleAndZero(FSharedMemoryMapping);
  if FWnd <> 0 then
    DeallocateHWnd(FWnd);
  inherited;
end;

function TSpawnServer.HandleExec: TSpawnServerSharedMemory.TRequestResult;
var
  Data: TPtrAndSize;
  EDisableFsRedir: Integer;
  EVerb, EFilename, EParams, EWorkingDir: String;
  EWait, EShowCmd: Integer;
  ClientCurrentDir, SaveCurrentDir: String;
begin
  Result := smrInvalidData;
  Data.Ptr := @FSharedMemory.LockedFields.Data[0];
  Data.Size := FSharedMemory.LockedFields.DataSize;
  if Data.Size > SizeOf(FSharedMemory.LockedFields.Data) then
    Exit;

  const ActualDataHash = SHA256Buf(Data.Ptr^, Data.Size);
  if not SHA256DigestsEqual(FSharedMemory.LockedFields.DataHash, ActualDataHash) then
    Exit;

  var IsShellExec: LongBool;
  if not ExtractInteger(Data, Integer(IsShellExec)) then Exit;
  if IsShellExec then begin
    if not ExtractString(Data, EVerb) then Exit;
  end
  else begin
    if not ExtractInteger(Data, EDisableFsRedir) then Exit;
  end;
  if not ExtractString(Data, EFilename) then Exit;
  if not ExtractString(Data, EParams) then Exit;
  if not ExtractString(Data, EWorkingDir) then Exit;
  if not ExtractInteger(Data, EWait) then Exit;
  if not ExtractInteger(Data, EShowCmd) then Exit;
  if not ExtractString(Data, ClientCurrentDir) then Exit;
  if Data.Size <> 0 then Exit;

  SaveCurrentDir := GetCurrentDir;
  try
    SetCurrentDir(ClientCurrentDir);

    if IsShellExec then begin
      FSharedMemory.LockedFields.ExecResult := InstShellExec(EVerb,
        EFilename, EParams, EWorkingDir, TExecWait(EWait), EShowCmd,
        ProcessMessagesProc, FSharedMemory.LockedFields.ExecResultCode);
    end
    else begin
      FSharedMemory.LockedFields.ExecResult := InstExec(EDisableFsRedir <> 0,
        EFilename, EParams, EWorkingDir, TExecWait(EWait), EShowCmd,
        ProcessMessagesProc, nil, FSharedMemory.LockedFields.ExecResultCode);
    end;
  finally
    SetCurrentDir(SaveCurrentDir);
  end;
  Result := smrExecReturned;
end;

procedure TSpawnServer.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SpawnServer_ClientConnected:
      begin
        { Once the client has finished mapping its view, we no longer need to
          keep our handle to the file mapping object open. This should be the
          last handle to the object, so closing it will remove the named entry
          from Object Manager's BaseNamedObjects directory. }
        if AtomicReadBool(FSharedMemory.ClientConnected) then
          CloseHandleAndZero(FSharedMemoryMapping);
      end;
    WM_SpawnServer_ProcessRequest:
      begin
        { If FReadyToServe is False, that tells us RestartProcess's
          ShellExecuteEx call must not have returned yet and is processing
          messages. We're not ready to accept requests until after
          ShellExecuteEx has returned and the "respawn block" environment
          variable has been unset.
          (ShellExecuteEx does process messages while a UAC dialog is up, but
          it isn't known whether the function continues to process some
          messages after starting the process. We're assuming that it could
          and defending against it.) }
        if not FReadyToServe then
          Message.Result := SPAWN_MSGRESULT_NOT_READY
        else begin
          Message.Result := SPAWN_MSGRESULT_OK;
          { Acquire lock. If this doesn't succeed (e.g., because the state is
            already smsServerProcessing) then we intentionally still return
            SPAWN_MSGRESULT_OK; see comments in CallSpawnServer. }
          if FSharedMemory.TryChangeLockState(
             smsHandOffToServer, smsServerProcessing) = smsHandOffToServer then begin
            { After advancing the state, unblock the client }
            ReplyMessage(Message.Result);
            try
              FSharedMemory.LockedFields.RequestResult := HandleExec;
            except
              if ExceptObject is EOutOfMemory then
                FSharedMemory.LockedFields.RequestResult := smrOutOfMemory
              else
                { Shouldn't get here; we don't explicitly raise any exceptions }
                FSharedMemory.LockedFields.RequestResult := smrException;
            end;
            { Release lock }
            FSharedMemory.TryChangeLockState(smsServerProcessing, smsHandBackToClient);
          end;
        end;
      end;
  else
    Message.Result := DefWindowProc(FWnd, Message.Msg, Message.WParam,
      Message.LParam);
  end;
end;

end.
