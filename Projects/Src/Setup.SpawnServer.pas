unit Setup.SpawnServer;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Spawn server
}

interface

uses
  Windows, SysUtils, Messages;

type
  TSpawnServer = class
  private
    FWnd: HWND;
    FSequenceNumber: Word;
    FCallStatus: Word;
    FResultCode: Integer;
    FNotifyRestartRequested: Boolean;
    FNotifyNewLanguage: Integer;
    function HandleExec(const IsShellExec: Boolean; const ADataPtr: Pointer;
      const ADataSize: Cardinal): LRESULT;
    procedure WndProc(var Message: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    property NotifyNewLanguage: Integer read FNotifyNewLanguage;
    property NotifyRestartRequested: Boolean read FNotifyRestartRequested;
    property Wnd: HWND read FWnd;
  end;

procedure EnterSpawnServerDebugMode;
function NeedToRespawnSelfElevated(const ARequireAdministrator,
  AEmulateHighestAvailable: Boolean): Boolean;
procedure RespawnSelfElevated(const AExeFilename, AParams: String;
  var AExitCode: DWORD);

implementation

{ For debugging only; remove 'x' to enable the define: }
{x$DEFINE SPAWNSERVER_RESPAWN_ALWAYS}

uses
  Classes, Forms, ShellApi, Shared.Int64Em, PathFunc, Shared.CommonFunc, Setup.InstFunc, Setup.SpawnCommon;

type
  TPtrAndSize = record
    Ptr: ^Byte;
    Size: Cardinal;
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

function ExtractLongint(var Data: TPtrAndSize; var Value: Longint): Boolean;
var
  P: Pointer;
begin
  Result := ExtractBytes(Data, SizeOf(Longint), P);
  if Result then
    Value := Longint(P^);
end;

function ExtractString(var Data: TPtrAndSize; var Value: String): Boolean;
var
  Len: Longint;
  P: Pointer;
begin
  Result := ExtractLongint(Data, Len);
  if Result then begin
    if (Len < 0) or (Len > $FFFF) then
      Result := False
    else begin
      Result := ExtractBytes(Data, Len * SizeOf(Value[1]), P);
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

function GetFinalFileName(const Filename: String): String;
{ Calls GetFinalPathNameByHandle to expand any SUBST'ed drives, network drives,
  and symbolic links in Filename. This is needed for elevation to succeed when
  Setup is started from a SUBST'ed drive letter. }

  function ConvertToNormalPath(P: PChar): String;
  begin
    Result := P;
    if StrLComp(P, '\\?\', 4) = 0 then begin
      Inc(P, 4);
      if (PathStrNextChar(P) = P + 1) and (P[1] = ':') and PathCharIsSlash(P[2]) then
        Result := P
      else if StrLIComp(P, 'UNC\', 4) = 0 then begin
        Inc(P, 4);
        Result := '\\' + P;
      end;
    end;
  end;

const
  FILE_SHARE_DELETE = $00000004;
var
  GetFinalPathNameByHandleFunc: function(hFile: THandle; lpszFilePath: PWideChar;
    cchFilePath: DWORD; dwFlags: DWORD): DWORD; stdcall;
  Attr, FlagsAndAttributes: DWORD;
  H: THandle;
  Res: Integer;
  Buf: array[0..4095] of Char;
begin
  GetFinalPathNameByHandleFunc := GetProcAddress(GetModuleHandle(kernel32),
    'GetFinalPathNameByHandleW');
  if Assigned(GetFinalPathNameByHandleFunc) then begin
    Attr := GetFileAttributes(PChar(Filename));
    if Attr <> $FFFFFFFF then begin
      { Backup semantics must be requested in order to open a directory }
      if Attr and FILE_ATTRIBUTE_DIRECTORY <> 0 then
        FlagsAndAttributes := FILE_FLAG_BACKUP_SEMANTICS
      else
        FlagsAndAttributes := 0;
      { Use zero access mask and liberal sharing mode to ensure success }
      H := CreateFile(PChar(Filename), 0, FILE_SHARE_READ or FILE_SHARE_WRITE or
        FILE_SHARE_DELETE, nil, OPEN_EXISTING, FlagsAndAttributes, 0);
      if H <> INVALID_HANDLE_VALUE then begin
        Res := GetFinalPathNameByHandleFunc(H, Buf, SizeOf(Buf) div SizeOf(Buf[0]), 0);
        CloseHandle(H);
        if (Res > 0) and (Res < (SizeOf(Buf) div SizeOf(Buf[0])) - 16) then begin
          { ShellExecuteEx fails with error 3 on \\?\UNC\ paths, so try to
            convert the returned path from \\?\ form }
          Result := ConvertToNormalPath(Buf);
          Exit;
        end;
      end;
    end;
  end;
  Result := Filename;
end;

function GetFinalCurrentDir: String;
var
  Res: Integer;
  Buf: array[0..MAX_PATH-1] of Char;
begin
  DWORD(Res) := GetCurrentDirectory(SizeOf(Buf) div SizeOf(Buf[0]), Buf);
  if (Res > 0) and (Res < SizeOf(Buf) div SizeOf(Buf[0])) then
    Result := GetFinalFileName(Buf)
  else begin
    RaiseFunctionFailedError('GetCurrentDirectory');
    Result := '';
  end;
end;

procedure RespawnSelfElevated(const AExeFilename, AParams: String;
  var AExitCode: DWORD);
{ Spawns a new process using the "runas" verb.
  Notes:
  1. Despite the function's name, the spawned process may not actually be
     elevated / running as administrator. If UAC is disabled, "runas"
     behaves like "open". Also, if a non-admin user is a member of a special
     system group like Backup Operators, they can select their own user account
     at a UAC dialog. Therefore, it is critical that the caller include some
     kind of protection against respawning more than once.
  2. If AExeFilename is on a network drive, the ShellExecuteEx function is
     smart enough to substitute it with a UNC path. }
const
  SEE_MASK_NOZONECHECKS = $00800000;
var
  ExpandedExeFilename, WorkingDir: String;
  Info: TShellExecuteInfo;
  WaitResult: DWORD;
begin
  if not SameText(PathExtractExt(AExeFilename), '.exe') then
    InternalError('Cannot respawn self, not named .exe');
  ExpandedExeFilename := GetFinalFileName(AExeFilename);
  WorkingDir := GetFinalCurrentDir;
  FillChar(Info, SizeOf(Info), 0);
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

  { Wait for the process to terminate, processing messages in the meantime }
  try
    repeat
      ProcessMessagesProc;
      WaitResult := MsgWaitForMultipleObjects(1, Info.hProcess, False,
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
    if not GetExitCodeProcess(Info.hProcess, AExitCode) then
      Win32ErrorMsg('GetExitCodeProcess');
  finally
    CloseHandle(Info.hProcess);
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
    Application.Title := Format('Wnd=$%x', [Server.FWnd]);
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
begin
  inherited;
  FNotifyNewLanguage := -1;
  FWnd := AllocateHWnd(WndProc);
  if FWnd = 0 then
    RaiseFunctionFailedError('AllocateHWnd');
end;

destructor TSpawnServer.Destroy;
begin
  if FWnd <> 0 then
    DeallocateHWnd(FWnd);
  inherited;
end;

function TSpawnServer.HandleExec(const IsShellExec: Boolean;
  const ADataPtr: Pointer; const ADataSize: Cardinal): LRESULT;
var
  Data: TPtrAndSize;
  EDisableFsRedir: Longint;
  EVerb, EFilename, EParams, EWorkingDir: String;
  EWait, EShowCmd: Longint;
  ClientCurrentDir, SaveCurrentDir: String;
  ExecResult: Boolean;
begin
  { Recursive calls aren't supported }
  if FCallStatus = SPAWN_STATUS_RUNNING then begin
    Result := SPAWN_MSGRESULT_ALREADY_IN_CALL;
    Exit;
  end;

  Result := SPAWN_MSGRESULT_INVALID_DATA;
  Data.Ptr := ADataPtr;
  Data.Size := ADataSize;
  if IsShellExec then begin
    if not ExtractString(Data, EVerb) then Exit;
  end
  else begin
    if not ExtractLongint(Data, EDisableFsRedir) then Exit;
  end;
  if not ExtractString(Data, EFilename) then Exit;
  if not ExtractString(Data, EParams) then Exit;
  if not ExtractString(Data, EWorkingDir) then Exit;
  if not ExtractLongint(Data, EWait) then Exit;
  if not ExtractLongint(Data, EShowCmd) then Exit;
  if not ExtractString(Data, ClientCurrentDir) then Exit;
  if Data.Size <> 0 then Exit;

  Inc(FSequenceNumber);
  FResultCode := -1;
  FCallStatus := SPAWN_STATUS_RUNNING;
  try
    SaveCurrentDir := GetCurrentDir;
    try
      SetCurrentDir(ClientCurrentDir);

      Result := SPAWN_MSGRESULT_SUCCESS_BITS or FSequenceNumber;
      { Send back the result code now to unblock the client }
      ReplyMessage(Result);

      if IsShellExec then begin
        ExecResult := InstShellExec(EVerb, EFilename, EParams, EWorkingDir,
          TExecWait(EWait), EShowCmd, ProcessMessagesProc, FResultCode);
      end
      else begin
        ExecResult := InstExec(EDisableFsRedir <> 0, EFilename, EParams, EWorkingDir,
          TExecWait(EWait), EShowCmd, ProcessMessagesProc, nil, FResultCode);
      end;
      if ExecResult then
        FCallStatus := SPAWN_STATUS_RETURNED_TRUE
      else
        FCallStatus := SPAWN_STATUS_RETURNED_FALSE;
    finally
      SetCurrentDir(SaveCurrentDir);
    end;
  finally
    { If the status is still SPAWN_STATUS_RUNNING here, then an unexpected
      exception must've occurred }
    if FCallStatus = SPAWN_STATUS_RUNNING then
      FCallStatus := SPAWN_STATUS_EXCEPTION;
  end;
end;

procedure TSpawnServer.WndProc(var Message: TMessage);
var
  Res: LRESULT;
begin
  case Message.Msg of
    WM_COPYDATA:
      begin
        try
          case TWMCopyData(Message).CopyDataStruct.dwData of
            CD_SpawnServer_Exec,
            CD_SpawnServer_ShellExec:
              begin
                Message.Result := HandleExec(
                  TWMCopyData(Message).CopyDataStruct.dwData = CD_SpawnServer_ShellExec,
                  TWMCopyData(Message).CopyDataStruct.lpData,
                  TWMCopyData(Message).CopyDataStruct.cbData);
              end;
          end;
        except
          if ExceptObject is EOutOfMemory then
            Message.Result := SPAWN_MSGRESULT_OUT_OF_MEMORY
          else
            { Shouldn't get here; we don't explicitly raise any exceptions }
            Message.Result := SPAWN_MSGRESULT_UNEXPECTED_EXCEPTION;
        end;
      end;
    WM_SpawnServer_Query:
      begin
        Res := SPAWN_MSGRESULT_INVALID_SEQUENCE_NUMBER;
        if Message.LParam = FSequenceNumber then begin
          Res := SPAWN_MSGRESULT_INVALID_QUERY_OPERATION;
          case Message.WParam of
            SPAWN_QUERY_STATUS:
              Res := SPAWN_MSGRESULT_SUCCESS_BITS or FCallStatus;
            SPAWN_QUERY_RESULTCODE_LO:
              Res := SPAWN_MSGRESULT_SUCCESS_BITS or LongRec(FResultCode).Lo;
            SPAWN_QUERY_RESULTCODE_HI:
              Res := SPAWN_MSGRESULT_SUCCESS_BITS or LongRec(FResultCode).Hi;
          end;
        end;
        Message.Result := Res;
      end;
    WM_USER + 150: begin
        { Got a SetupNotifyWnd message. (See similar handling in SetupLdr.dpr) }
        if Message.WParam = 10000 then
          FNotifyRestartRequested := True
        else if Message.WParam = 10001 then
          FNotifyNewLanguage := Message.LParam;
      end;
  else
    Message.Result := DefWindowProc(FWnd, Message.Msg, Message.WParam,
      Message.LParam);
  end;
end;

end.
