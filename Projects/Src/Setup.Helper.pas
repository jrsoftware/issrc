unit Setup.Helper;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Interface to 64-bit helper

  NOTE: These functions are NOT thread-safe. Do not call them from multiple
  threads simultaneously.
}

interface

uses
  Windows, SysUtils, Shared.Struct;

function GetHelperResourceName: String;
function HelperGrantPermission(const AObjectType: DWORD;
  const AObjectName: String; const AEntries: TGrantPermissionEntry;
  const AEntryCount: Integer; const AInheritance: DWORD): DWORD;
procedure HelperRegisterTypeLibrary(const AUnregister: Boolean;
  Filename: String);
procedure SetHelperExeFilename(const Filename: String);
procedure StopHelper(const DelayAfterStopping: Boolean);

implementation

{x$DEFINE HELPERDEBUG}

uses
  Forms, Shared.Int64Em, Shared.CommonFunc.Vcl, Shared.CommonFunc, PathFunc, Setup.MainFunc, Setup.InstFunc,
  Setup.LoggingFunc, SetupLdrAndSetup.Messages, Shared.SetupMessageIDs;

const
  HELPER_VERSION = 105;

const
  REQUEST_PING = 1;
  REQUEST_GRANT_PERMISSION = 2;
  REQUEST_REGISTER_SERVER = 3;  { no longer used }
  REQUEST_REGISTER_TYPE_LIBRARY = 4;

type
  TRequestGrantPermissionData = record
    ObjectType: DWORD;
    EntryCount: DWORD;
    Inheritance: DWORD;
    ObjectName: array[0..4095] of WideChar;
    Entries: array[0..MaxGrantPermissionEntries-1] of TGrantPermissionEntry;
  end;
  TRequestRegisterServerData = record
    Unregister: BOOL;
    FailCriticalErrors: BOOL;
    Filename: array[0..4095] of WideChar;
    Directory: array[0..4095] of WideChar;
  end;
  TRequestRegisterTypeLibraryData = record
    Unregister: BOOL;
    Filename: array[0..4095] of WideChar;
  end;

  TRequestData = record
    SequenceNumber: DWORD;
    Command: DWORD;
    DataSize: DWORD;
    case Integer of
      0: (Data: array[0..0] of Byte);
      1: (GrantPermissionData: TRequestGrantPermissionData);
      2: (RegisterServerData: TRequestRegisterServerData);
      3: (RegisterTypeLibraryData: TRequestRegisterTypeLibraryData);
  end;

  TResponseData = record
    SequenceNumber: DWORD;
    StatusCode: DWORD;
    ErrorCode: DWORD;
    DataSize: DWORD;
    Data: array[0..0] of Byte;  { currently, no data is ever returned }
  end;

  THelper = class
  private
    FRunning, FNeedsRestarting: Boolean;
    FProcessHandle, FPipe: THandle;
    FProcessID: DWORD;
    FCommandSequenceNumber: DWORD;
    FProcessMessagesProc: procedure of object;
    FRequest: TRequestData;
    FResponse: TResponseData;
    procedure Call(const ACommand, ADataSize: DWORD);
    procedure InternalCall(const ACommand, ADataSize: DWORD;
      const AllowProcessMessages: Boolean);
    procedure Start;
  public
    destructor Destroy; override;
    function GrantPermission(const AObjectType: DWORD;
      const AObjectName: String; const AEntries: TGrantPermissionEntry;
      const AEntryCount: Integer; const AInheritance: DWORD): DWORD;
    procedure RegisterTypeLibrary(const AUnregister: Boolean;
      Filename: String);
    procedure Stop(const DelayAfterStopping: Boolean);
  end;

var
  HelperMainInstance: THelper;
  HelperExeFilename: String;
  HelperPipeNameSequence: LongWord;

function GetHelperResourceName: String;
begin
  {$R Setup.HelperEXEs.res}
  if paX64 in MachineTypesSupportedBySystem then
    Result := 'HELPER_EXE_AMD64'
  else
    Result := '';
end;

procedure SetHelperExeFilename(const Filename: String);
begin
  HelperExeFilename := Filename;
end;

procedure StopHelper(const DelayAfterStopping: Boolean);
begin
  HelperMainInstance.Stop(DelayAfterStopping);
end;

function HelperGrantPermission(const AObjectType: DWORD;
  const AObjectName: String; const AEntries: TGrantPermissionEntry;
  const AEntryCount: Integer; const AInheritance: DWORD): DWORD;
begin
  Result := HelperMainInstance.GrantPermission(AObjectType, AObjectName,
    AEntries, AEntryCount, AInheritance);
end;

procedure HelperRegisterTypeLibrary(const AUnregister: Boolean;
  Filename: String);
begin
  HelperMainInstance.RegisterTypeLibrary(AUnregister, Filename);
end;

procedure FillWideCharBuffer(var Buf: array of WideChar; const S: String);
begin
  if High(Buf) <= 0 then
    InternalError('FillWideCharBuffer: Invalid Buf');
  if Length(S) > High(Buf) then
    InternalError('FillWideCharBuffer: String too long');
  StrPLCopy(Buf, S, High(Buf));
end;

{ THelper }

destructor THelper.Destroy;
begin
  Stop(False);
  inherited;
end;

procedure THelper.Start;
const
  FILE_FLAG_FIRST_PIPE_INSTANCE = $00080000;
  InheritableSecurity: TSecurityAttributes = (
    nLength: SizeOf(InheritableSecurity); lpSecurityDescriptor: nil;
    bInheritHandle: True);
var
  PerformanceCount: Integer64;
  PipeName: String;
  Pipe, RemotePipe: THandle;
  Mode: DWORD;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  Log('Starting 64-bit helper process.');

  { We don't *have* to check IsWin64 here; the helper *should* run fine without
    the new APIs added in 2003 SP1. But let's be consistent and disable 64-bit
    functionality across the board when the user is running 64-bit Windows and
    IsWin64=False. }
  if not IsWin64 then
    InternalError('Cannot utilize 64-bit features on this version of Windows');

  if HelperExeFilename = '' then
    InternalError('64-bit helper EXE wasn''t extracted');

  repeat
    { Generate a very unique pipe name }
    Inc(HelperPipeNameSequence);
    FCommandSequenceNumber := GetTickCount;
    if not QueryPerformanceCounter(TLargeInteger(PerformanceCount)) then
      GetSystemTimeAsFileTime(TFileTime(PerformanceCount));
    PipeName := Format('\\.\pipe\InnoSetup64BitHelper-%.8x-%.8x-%.8x-%.8x%.8x',
      [GetCurrentProcessId, HelperPipeNameSequence, FCommandSequenceNumber,
       PerformanceCount.Hi, PerformanceCount.Lo]);

    { Create the pipe }
    Pipe := CreateNamedPipe(PChar(PipeName),
      PIPE_ACCESS_DUPLEX or FILE_FLAG_OVERLAPPED or FILE_FLAG_FIRST_PIPE_INSTANCE,
      PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE or PIPE_WAIT,
      1, 8192, 8192, 0, nil);
    if Pipe <> INVALID_HANDLE_VALUE then
      Break;
    { Loop if there's a name clash (ERROR_PIPE_BUSY), otherwise raise error }
    if GetLastError <> ERROR_PIPE_BUSY then
      Win32ErrorMsg('CreateNamedPipe');
  until False;

  try
    { Create an inheritable handle to the pipe for the helper to use }
    RemotePipe := CreateFile(PChar(PipeName), GENERIC_READ or GENERIC_WRITE,
      0, @InheritableSecurity, OPEN_EXISTING, 0, 0);
    if RemotePipe = INVALID_HANDLE_VALUE then
      Win32ErrorMsg('CreateFile');
    try
      Mode := PIPE_READMODE_MESSAGE or PIPE_WAIT;
      if not SetNamedPipeHandleState(RemotePipe, Mode, nil, nil) then
        Win32ErrorMsg('SetNamedPipeHandleState');

      FillChar(StartupInfo, SizeOf(StartupInfo), 0);
      StartupInfo.cb := SizeOf(StartupInfo);
      if not CreateProcess(PChar(HelperExeFilename),
         PChar(Format('helper %d 0x%x', [HELPER_VERSION, RemotePipe])), nil,
         nil, True, CREATE_DEFAULT_ERROR_MODE or CREATE_NO_WINDOW, nil,
         PChar(GetSystemDir), StartupInfo, ProcessInfo) then
        Win32ErrorMsg('CreateProcess');

      FRunning := True;
      FNeedsRestarting := False;
      FProcessHandle := ProcessInfo.hProcess;
      FProcessID := ProcessInfo.dwProcessId;
      FPipe := Pipe;
      Pipe := 0;  { ensure the 'except' section can't close it now } 
      CloseHandle(ProcessInfo.hThread);
      LogFmt('Helper process PID: %u', [FProcessID]);
    finally
      { We don't need a handle to RemotePipe after creating the process }
      CloseHandle(RemotePipe);
    end;
  except
    if Pipe <> 0 then
      CloseHandle(Pipe);
    raise;
  end;
end;

procedure THelper.Stop(const DelayAfterStopping: Boolean);
{ Stops the helper process if it's running }
var
  ExitCode: DWORD;
begin
  if not FRunning then
    Exit;

  { Before attempting to stop anything, set FNeedsRestarting to ensure
    Call can never access a partially-stopped helper }
  FNeedsRestarting := True;

  LogFmt('Stopping 64-bit helper process. (PID: %u)', [FProcessID]);
  { Closing our handle to the pipe will cause the helper's blocking ReadFile
    call to return False, and the process to exit }
  CloseHandle(FPipe);
  FPipe := 0;
  while WaitForSingleObject(FProcessHandle, 10000) = WAIT_TIMEOUT do begin
    { It should never have to resort to terminating the process, but if the
      process for some unknown reason didn't exit in response to our closing
      the pipe, it should be safe to kill it since it most likely isn't doing
      anything other than waiting for a request. }
    Log('Helper isn''t responding; killing it.');
    TerminateProcess(FProcessHandle, 1);
  end;
  if GetExitCodeProcess(FProcessHandle, ExitCode) then begin
    if ExitCode = 0 then
      Log('Helper process exited.')
    else
      LogFmt('Helper process exited with failure code: 0x%x', [ExitCode]);
  end
  else
    Log('Helper process exited, but failed to get exit code.');
  CloseHandle(FProcessHandle);
  FProcessHandle := 0;
  FProcessID := 0;
  FRunning := False;

  { Give it extra time to fully terminate to ensure that the EXE isn't still
    locked on SMP systems when we try to delete it in DeinitSetup.
    (Note: I'm not 100% certain this is needed; I don't have an SMP AMD64
    system to test on. It didn't seem to be necessary on IA64, but I suspect
    that may be because it doesn't execute x86 code in true SMP fashion.)
    This also limits the rate at which new helper processes can be spawned,
    which is probably a good thing. }
  if DelayAfterStopping then
    Sleep(250);
end;

procedure THelper.InternalCall(const ACommand, ADataSize: DWORD;
  const AllowProcessMessages: Boolean);
var
  RequestSize, BytesRead, LastError: DWORD;
  OverlappedEvent: THandle;
  Res: BOOL;
  Overlapped: TOverlapped;
begin
  Inc(FCommandSequenceNumber);
  { On entry, only Request.Data needs to be filled }
  FRequest.SequenceNumber := FCommandSequenceNumber;
  FRequest.Command := ACommand;
  FRequest.DataSize := ADataSize;
  RequestSize := Cardinal(@TRequestData(nil^).Data) + ADataSize;
  try
    {$IFDEF HELPERDEBUG}
    LogFmt('Helper[%u]: Sending request (size: %u): Seq=%u, Command=%u, DataSize=%u',
      [FProcessID, RequestSize, FRequest.SequenceNumber, FRequest.Command,
       FRequest.DataSize]);
    {$ENDIF}
    { Create event object to use in our Overlapped structure. (Technically,
      I'm not sure we need the event object -- we could just wait on the pipe
      object instead, however the SDK docs discourage this.) } 
    OverlappedEvent := CreateEvent(nil, True, False, nil);
    if OverlappedEvent = 0 then
      Win32ErrorMsg('CreateEvent');
    try
      FillChar(Overlapped, SizeOf(Overlapped), 0);
      Overlapped.hEvent := OverlappedEvent;
      if not TransactNamedPipe(FPipe, @FRequest, RequestSize, @FResponse,
         SizeOf(FResponse), BytesRead, @Overlapped) then begin
        if GetLastError <> ERROR_IO_PENDING then
          Win32ErrorMsg('TransactNamedPipe');
        { Operation is pending; wait for it to complete.
          (Note: Waiting is never optional. The system will modify Overlapped
          when the operation completes; if we were to return early for whatever
          reason, the stack would get corrupted.) }
        try
          if AllowProcessMessages and Assigned(FProcessMessagesProc) then begin
            repeat
              { Process any pending messages first because MsgWaitForMultipleObjects
                (called below) only returns when *new* messages arrive }
              FProcessMessagesProc;
            until MsgWaitForMultipleObjects(1, OverlappedEvent, False,
              INFINITE, QS_ALLINPUT) <> WAIT_OBJECT_0+1;
          end;
        finally
          { Call GetOverlappedResult with bWait=True, even if exception occurred }
          Res := GetOverlappedResult(FPipe, Overlapped, BytesRead, True);
          LastError := GetLastError;
        end;
        if not Res then
          Win32ErrorMsgEx('TransactNamedPipe/GetOverlappedResult', LastError);
      end;
    finally
      CloseHandle(OverlappedEvent);
    end;
    {$IFDEF HELPERDEBUG}
    LogFmt('Helper[%u]: Got response (size: %u): Seq=%u, StatusCode=%u, ErrorCode=%u, DataSize=%u',
      [FProcessID, BytesRead, FResponse.SequenceNumber, FResponse.StatusCode,
       FResponse.ErrorCode, FResponse.DataSize]);
    {$ENDIF}
    if (Cardinal(BytesRead) < Cardinal(@TResponseData(nil^).Data)) or
       (FResponse.DataSize <> Cardinal(BytesRead) - Cardinal(@TResponseData(nil^).Data)) then
      InternalError('Helper: Response message has wrong size');
    if FResponse.SequenceNumber <> FRequest.SequenceNumber then
      InternalError('Helper: Wrong sequence number');
    if FResponse.StatusCode = 0 then
      InternalError('Helper: Command did not execute');
  except
    { If an exception occurred, then the helper may have crashed or is in some
      weird state. Attempt to stop it now, and also set FNeedsRestarting
      to ensure it's restarted on the next call in case our stop attempt here
      fails for some reason. }
    FNeedsRestarting := True;
    Log('Exception while communicating with helper:' + SNewLine + GetExceptMessage);
    Stop(True);
    raise;
  end;
end;

procedure THelper.Call(const ACommand, ADataSize: DWORD);
begin
  { Start/restart helper if needed }
  if not FRunning or FNeedsRestarting then begin
    Stop(True);
    Start;
  end
  else begin
    { It is running -- or so we think. Before sending the specified request,
      send a ping request to verify that it's still alive. It may have somehow
      died since we last talked to it (unlikely, though). }
    try
      InternalCall(REQUEST_PING, 0, False);
    except
      { Don't propagate any exception; just log it and restart the helper }
      Log('Ping failed; helper seems to have died.');
      Stop(True);
      Start;
    end;
  end;

  InternalCall(ACommand, ADataSize, True);
end;

{ High-level interface functions }

function THelper.GrantPermission(const AObjectType: DWORD;
  const AObjectName: String; const AEntries: TGrantPermissionEntry;
  const AEntryCount: Integer; const AInheritance: DWORD): DWORD;
begin
  if (AEntryCount < 1) or
     (AEntryCount > SizeOf(FRequest.GrantPermissionData.Entries) div SizeOf(FRequest.GrantPermissionData.Entries[0])) then
    InternalError('HelperGrantPermission: Invalid entry count');

  FRequest.GrantPermissionData.ObjectType := AObjectType;
  FRequest.GrantPermissionData.EntryCount := AEntryCount;
  FRequest.GrantPermissionData.Inheritance := AInheritance;
  FillWideCharBuffer(FRequest.GrantPermissionData.ObjectName, AObjectName);
  Move(AEntries, FRequest.GrantPermissionData.Entries,
    AEntryCount * SizeOf(FRequest.GrantPermissionData.Entries[0]));

  Call(REQUEST_GRANT_PERMISSION, SizeOf(FRequest.GrantPermissionData));

  Result := FResponse.ErrorCode;
end;

procedure THelper.RegisterTypeLibrary(const AUnregister: Boolean;
  Filename: String);
{ Registers or unregisters the specified type library inside the helper.
  Raises an exception on failure. }
begin
  Filename := PathExpand(Filename);

  FRequest.RegisterTypeLibraryData.Unregister := AUnregister;
  FillWideCharBuffer(FRequest.RegisterTypeLibraryData.Filename, Filename);

  { Stop the helper before and after the call to be 100% sure the state of the
    helper is clean prior to and after registering. Can't trust foreign code. }
  Stop(False);
  Call(REQUEST_REGISTER_TYPE_LIBRARY, SizeOf(FRequest.RegisterTypeLibraryData));
  Stop(False);

  case FResponse.StatusCode of
    1: begin
         { The LoadTypeLib call failed }
         RaiseOleError('LoadTypeLib', FResponse.ErrorCode);
       end;
    2: begin
         { The call to RegisterTypeLib was made; possibly succeeded }
         if (FResponse.ErrorCode <> S_OK) or AUnregister then
           RaiseOleError('RegisterTypeLib', FResponse.ErrorCode);
       end;
    3: begin
         { The ITypeLib::GetLibAttr call failed }
         RaiseOleError('ITypeLib::GetLibAttr', FResponse.ErrorCode);
       end;
    4: begin
         { The call to UnRegisterTypeLib was made; possibly succeeded }
         if (FResponse.ErrorCode <> S_OK) or not AUnregister then
           RaiseOleError('UnRegisterTypeLib', FResponse.ErrorCode);
       end;
  else
    InternalError('HelperRegisterTypeLibrary: StatusCode invalid');
  end;
end;

initialization
  HelperMainInstance := THelper.Create;
finalization
  FreeAndNil(HelperMainInstance);
end.
