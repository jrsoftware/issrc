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
    procedure SetFirstProcessWnd(const Value: HWND);
    procedure WndProc(var Message: TMessage);
  public
    constructor Create(const AClientRequiresAdmin: Boolean);
    destructor Destroy; override;
    property FirstProcessWnd: HWND write SetFirstProcessWnd;
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
{x$DEFINE SPAWNSERVER_DEBUGMODE}

uses
  Classes, Forms, ShellApi, PathFunc, SHA256, Shared.CommonFunc,
  SetupLdrAndSetup.InstFunc, Setup.InstFunc;

type
  TPtrAndSize = record
    Ptr: ^Byte;
    Size: Cardinal;
  end;

function ConvertStringSecurityDescriptorToSecurityDescriptor(
  StringSecurityDescriptor: LPCWSTR; StringSDRevision: DWORD;
  var SecurityDescriptor: Pointer; SecurityDescriptorSize: PULONG): BOOL; stdcall;
  external advapi32 name 'ConvertStringSecurityDescriptorToSecurityDescriptorW';

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
    if (Len < 0) or (Len > $7FFE) then
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
{ For debugging purposes only: Creates a spawn server, but does not start a
  new process. Displays the server's shared memory ID in a message box, and
  terminates when the message box is closed. }
begin
  {$IFDEF SPAWNSERVER_DEBUGMODE}
  const Server = TSpawnServer.Create(False);
  try
    Server.FReadyToServe := True;
    MessageBox(0, PChar(Format('/SPAWNSM="%s"', [Server.SharedMemoryID])),
      'Setup: Spawn server debug mode', MB_OK or MB_ICONINFORMATION);
  finally
    Server.Free;
  end;
  {$ENDIF}
  Halt(1);
end;

{ TSpawnServer }

constructor TSpawnServer.Create(const AClientRequiresAdmin: Boolean);

  function CreateSecurityDescriptorForMapping: Pointer;
  { Since the file mapping object is named and appears in Object Manager's
    BaseNamedObjects directory (albeit briefly because we close the handle
    after the client connects), it should be given a secure DACL.

    The default DACL assigned by CreateFileMapping (on Windows 11 25H2) when
    no security descriptor is supplied includes the following access-allowed
    ACEs:
    - SYSTEM (FILE_MAP_ALL_ACCESS)
        This is fine.
    - The current user SID (FILE_MAP_ALL_ACCESS)
        This is problematic when the current process (the spawn server
        process) was started initially elevated (i.e., with the Administrators
        group in its access token). We shouldn't allow non-elevated (medium
        integrity) processes running as the same user to access an elevated
        spawn server.
    - The logon session SID (FILE_MAP_ALL_ACCESS)
        This is unwanted because it allows any process in the same session to
        open the object, regardless of user SID.

    This function constructs a custom security descriptor with a DACL
    containing the following access-allowed ACEs:
    - SYSTEM (FILE_MAP_ALL_ACCESS)
    - Administrators (FILE_MAP_ALL_ACCESS)
    - The current user SID (FILE_MAP_WRITE)
        Exception: If the current process (the spawn server process) has the
        Administrators group in its access token, then the current user SID
        ACE is excluded to prevent non-elevated processes from opening the
        object. It is also excluded when the spawn client will be running with
        PrivilegesRequired=admin, because in that case it's redundant; the
        Administrators ACE will already grant them access.
        Note that access is restricted to FILE_MAP_WRITE. This prohibits
        low-integrity processes from opening the object (unless Setup itself
        is started as a low-integrity process, which we don't support):
        - A low-integrity process cannot open the object for read-only access
          because that would require FILE_MAP_READ.
        - A low-integrity process cannot open the object for read/write access
          even though the DACL grants FILE_MAP_WRITE because by default
          Windows Mandatory Integrity Control gives the object an implicit
          label of medium integrity + No-Write-Up policy (i.e., processes with
          an integrity level below medium cannot request write access).
          However, rather than rely on that implicit label, we specify a
          stronger explicit label; see below.

    The custom security descriptor also includes a mandatory label with an
    integrity level matching the current process's integrity level. This
    ensures that if the current process was initially started with an
    integrity level of high but without the Administrators group in its access
    token, medium-integrity processes are blocked from accessing the object
    even though the current user SID ACE would normally grant them access.
    A process can receive such an access token when a standard user is a
    member of the Backup Operators group and selects their own account at a
    UAC dialog; elevation sets the token's integrity level to high as usual,
    but adds the Backup Operators group to the token, not Administrators.
  }

    procedure AppendAllowACE(var S: String; const ASid: String;
      const AAccessMask: DWORD);
    begin
      S := S + Format('(A;;0x%x;;;%s)', [AAccessMask, ASid]);
    end;

  const
    SDDL_REVISION_1 = 1;
  begin
    var StringSD := 'D:';  { Means DACL follows }
    AppendAllowACE(StringSD, 'SY', FILE_MAP_ALL_ACCESS);  { SYSTEM }
    AppendAllowACE(StringSD, 'BA', FILE_MAP_ALL_ACCESS);  { Administrators }
    if not IsAdminLoggedOn and not AClientRequiresAdmin then begin
      var CurrentUserSid := GetCurrentUserSid;
      { We don't expect GetCurrentUserSid to ever fail, but fall back to
        "Owner Rights" if it does }
      if CurrentUserSid = '' then
        CurrentUserSid := 'OW';
      AppendAllowACE(StringSD, CurrentUserSid, FILE_MAP_WRITE);
    end;

    const IntegrityLevelSid = GetCurrentProcessIntegrityLevelSid;
    { All processes should have an integrity level, and querying it is not
      expected to ever fail }
    if IntegrityLevelSid = '' then
      RaiseFunctionFailedError('GetCurrentProcessIntegrityLevelSid');
    StringSD := StringSD + 'S:(ML;;NWNRNX;;;' + IntegrityLevelSid + ')';

    if not ConvertStringSecurityDescriptorToSecurityDescriptor(
       PChar(StringSD), SDDL_REVISION_1, Result, nil) then
      Win32ErrorMsg('ConvertStringSecurityDescriptorToSecurityDescriptor');
  end;

  procedure CreateSharedMemoryMapping(const ASecurityDescriptor: Pointer);
  const
    FiveDigitsRange = 36 * 36 * 36 * 36 * 36;
  begin
    var SecurityAttr: TSecurityAttributes;
    SecurityAttr.nLength := SizeOf(SecurityAttr);
    SecurityAttr.lpSecurityDescriptor := ASecurityDescriptor;
    SecurityAttr.bInheritHandle := False;

    var FailureCount := 0;
    while True do begin
      FSharedMemoryID := '';
      for var I := 0 to 2 do
        FSharedMemoryID := FSharedMemoryID +
          UIntToBase36Str(TStrongRandom.GenerateUInt32Range(FiveDigitsRange), 5);

      const ObjectName = TSpawnServerSharedMemory.ObjectNamePrefix +
        FSharedMemoryID;
      { Safety: An extra 64KB ($10000) is reserved but not committed to ensure
        an overrun of Data will AV rather than step outside the mapped view }
      SetLastError(ERROR_SUCCESS);
      const H = CreateFileMapping(INVALID_HANDLE_VALUE, @SecurityAttr,
        SEC_RESERVE or PAGE_READWRITE, 0,
        SizeOf(FSharedMemory^) + TSpawnServerSharedMemory.ExtraReservedSize,
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
  inherited Create;
  FWnd := AllocateHWnd(WndProc);
  if FWnd = 0 then
    RaiseFunctionFailedError('AllocateHWnd');

  const SecurityDescriptor = CreateSecurityDescriptorForMapping;
  try
    CreateSharedMemoryMapping(SecurityDescriptor);
  finally
    LocalFree(SecurityDescriptor);
  end;

  var SM: PSpawnServerSharedMemory;
  SM := MapViewOfFile(FSharedMemoryMapping, FILE_MAP_WRITE, 0, 0,
    SizeOf(SM^) + TSpawnServerSharedMemory.ExtraReservedSize);
  if SM = nil then
    Win32ErrorMsg('MapViewOfFile');
  try
    if VirtualAlloc(SM, SizeOf(SM^), MEM_COMMIT, PAGE_READWRITE) = nil then
      Win32ErrorMsg('VirtualAlloc');
  except
    UnmapViewOfFile(SM);
    raise;
  end;
  { Only assign FSharedMemory after pages are committed since Destroy writes
    to it }
  FSharedMemory := SM;
  FSharedMemory.StructSize := SizeOf(FSharedMemory^);
  FSharedMemory.VersionNumber := FSharedMemory.ExpectedVersionNumber;
  FSharedMemory.ServerWnd := UInt32(FWnd);
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

procedure TSpawnServer.SetFirstProcessWnd(const Value: HWND);
begin
  FSharedMemory.FirstProcessWnd := UInt32(Value);
  MemoryBarrier;
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
  Data.Size := FSharedMemory.LockedFields.DataSize;
  if (Data.Size <= 0) or (Data.Size > SizeOf(FSharedMemory.LockedFields.Data)) then
    Exit;
  { Sanity check: To be really sure Data isn't being modified while we're
    reading it (which should never happen due to locking via LockState), we
    make a copy, hash-check the copy, and read only from the copy }
  var DataCopy: TBytes;
  SetLength(DataCopy, NativeInt(Data.Size));
  Move(FSharedMemory.LockedFields.Data, DataCopy[0], Length(DataCopy));
  Data.Ptr := @DataCopy[0];

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
  if not ExtractInteger(Data, EWait) or (EWait < Ord(Low(TExecWait))) or
     (EWait > Ord(High(TExecWait))) then Exit;
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
        EFilename, EParams, EWorkingDir, TExecWait(EWait), EShowCmd, False,
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
        { If FReadyToServe is False, that tells us RespawnProcess's
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
            { Post a no-op message back to the client to try to make its
              WaitMessageWithTimeout call return immediately; see comments in
              CallSpawnServer. Windows' UIPI does allow WM_NULL to be sent to
              higher-integrity processes. }
            const ClientWakeWnd = FSharedMemory.ClientWakeWnd;
            if ClientWakeWnd <> 0 then
              PostMessage(ClientWakeWnd, WM_NULL, 0, 0);
          end;
        end;
      end;
  else
    Message.Result := DefWindowProc(FWnd, Message.Msg, Message.WParam,
      Message.LParam);
  end;
end;

end.
