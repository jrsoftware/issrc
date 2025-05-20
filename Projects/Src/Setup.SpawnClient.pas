unit Setup.SpawnClient;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Spawn client

  NOTE: These functions are NOT thread-safe. Do not call them from multiple
  threads simultaneously.
}

interface

uses
  Windows, SysUtils, Messages, Setup.InstFunc, Shared.CommonFunc;

procedure InitializeSpawnClient(const AServerWnd: HWND);
function InstExecEx(const RunAsOriginalUser: Boolean;
  const DisableFsRedir: Boolean; const Filename, Params, WorkingDir: String;
  const Wait: TExecWait; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; const OutputReader: TCreateProcessOutputReader;
  var ResultCode: Integer): Boolean;
function InstShellExecEx(const RunAsOriginalUser: Boolean;
  const Verb, Filename, Params, WorkingDir: String;
  const Wait: TExecWait; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; var ResultCode: Integer): Boolean;

implementation

uses
  Classes, Setup.SpawnCommon;

var
  SpawnServerPresent: Boolean;
  SpawnServerWnd: HWND;

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
  if GetWindowThreadProcessId(SpawnServerWnd, @PID) <> 0 then begin
    AllowSetForegroundWindowFunc := GetProcAddress(GetModuleHandle(user32),
      'AllowSetForegroundWindow');
    if Assigned(AllowSetForegroundWindowFunc) then
      AllowSetForegroundWindowFunc(PID);
  end;
end;

function QuerySpawnServer(const SequenceNumber: Word;
  const Operation: Integer): Word;
var
  MsgResult: LRESULT;
begin
  MsgResult := SendMessage(SpawnServerWnd, WM_SpawnServer_Query, Operation,
    SequenceNumber);
  if MsgResult and not $FFFF <> SPAWN_MSGRESULT_SUCCESS_BITS then
    InternalErrorFmt('QuerySpawnServer: Unexpected response: $%x', [MsgResult]);
  Result := Word(MsgResult);
end;

function CallSpawnServer(const CopyDataMsg: DWORD; var M: TMemoryStream;
  const ProcessMessagesProc: TProcedure; var ResultCode: Integer): Boolean;
var
  CopyDataStruct: TCopyDataStruct;
  MsgResult: LRESULT;
  SequenceNumber: Word;
  Status: Word;
  LastQueryTime, NowTime: DWORD;
begin
  CopyDataStruct.dwData := CopyDataMsg;
  CopyDataStruct.cbData := M.Size;
  CopyDataStruct.lpData := M.Memory;
  AllowSpawnServerToSetForegroundWindow;
  MsgResult := SendMessage(SpawnServerWnd, WM_COPYDATA, 0, LPARAM(@CopyDataStruct));
  FreeAndNil(M);  { it isn't needed anymore, might as well free now }
  if MsgResult = SPAWN_MSGRESULT_OUT_OF_MEMORY then
    OutOfMemoryError;
  if MsgResult and not $FFFF <> SPAWN_MSGRESULT_SUCCESS_BITS then
    InternalErrorFmt('CallSpawnServer: Unexpected response: $%x', [MsgResult]);
  SequenceNumber := Word(MsgResult);

  LastQueryTime := GetTickCount;
  repeat
    ProcessMessagesProc;
    { Now that the queue is empty (we mustn't break without first processing
      messages found by a previous MsgWaitForMultipleObjects call), see if
      the status changed, but only if at least 10 ms has elapsed since the
      last query }
    NowTime := GetTickCount;
    if Cardinal(NowTime - LastQueryTime) >= Cardinal(10) then begin
      LastQueryTime := NowTime;
      Status := QuerySpawnServer(SequenceNumber, SPAWN_QUERY_STATUS);
      case Status of
        SPAWN_STATUS_RUNNING: ;
        SPAWN_STATUS_RETURNED_TRUE, SPAWN_STATUS_RETURNED_FALSE: Break;
      else
        InternalErrorFmt('CallSpawnServer: Unexpected status: %d', [Status]);
      end;
    end;
    { Delay for 10 ms, or until a message arrives }
    MsgWaitForMultipleObjects(0, THandle(nil^), False, 10, QS_ALLINPUT);
  until False;

  ResultCode := QuerySpawnServer(SequenceNumber, SPAWN_QUERY_RESULTCODE_LO) or
    (QuerySpawnServer(SequenceNumber, SPAWN_QUERY_RESULTCODE_HI) shl 16);
  Result := (Status = SPAWN_STATUS_RETURNED_TRUE);
end;

function InstExecEx(const RunAsOriginalUser: Boolean;
  const DisableFsRedir: Boolean; const Filename, Params, WorkingDir: String;
  const Wait: TExecWait; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; const OutputReader: TCreateProcessOutputReader;
  var ResultCode: Integer): Boolean;
var
  M: TMemoryStream;
begin
  if not RunAsOriginalUser or not SpawnServerPresent then begin
    Result := InstExec(DisableFsRedir, Filename, Params, WorkingDir,
      Wait, ShowCmd, ProcessMessagesProc, OutputReader, ResultCode);
    Exit;
  end;

  M := TMemoryStream.Create;
  try
    WriteLongintToStream(M, Ord(DisableFsRedir));
    WriteStringToStream(M, Filename);
    WriteStringToStream(M, Params);
    WriteStringToStream(M, WorkingDir);
    WriteLongintToStream(M, Ord(Wait));
    WriteLongintToStream(M, ShowCmd);
    WriteStringToStream(M, GetCurrentDir);

    Result := CallSpawnServer(CD_SpawnServer_Exec, M, ProcessMessagesProc,
      ResultCode);
  finally
    M.Free;
  end;
end;

function InstShellExecEx(const RunAsOriginalUser: Boolean;
  const Verb, Filename, Params, WorkingDir: String;
  const Wait: TExecWait; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; var ResultCode: Integer): Boolean;
var
  M: TMemoryStream;
begin
  if not RunAsOriginalUser or not SpawnServerPresent then begin
    Result := InstShellExec(Verb, Filename, Params, WorkingDir,
      Wait, ShowCmd, ProcessMessagesProc, ResultCode);
    Exit;
  end;

  M := TMemoryStream.Create;
  try
    WriteStringToStream(M, Verb);
    WriteStringToStream(M, Filename);
    WriteStringToStream(M, Params);
    WriteStringToStream(M, WorkingDir);
    WriteLongintToStream(M, Ord(Wait));
    WriteLongintToStream(M, ShowCmd);
    WriteStringToStream(M, GetCurrentDir);

    Result := CallSpawnServer(CD_SpawnServer_ShellExec, M, ProcessMessagesProc,
      ResultCode);
  finally
    M.Free;
  end;
end;

procedure InitializeSpawnClient(const AServerWnd: HWND);
begin
  SpawnServerWnd := AServerWnd;
  SpawnServerPresent := True;
end;

end.
