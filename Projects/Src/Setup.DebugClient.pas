unit Setup.DebugClient;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Debug client functions (client=Setup, server=debugger/IDE)
}

interface

uses
  Windows, SysUtils, Messages, Shared.DebugStruct;

var
  Debugging: Boolean;
  DebugClientCompiledCodeText: AnsiString;
  DebugClientCompiledCodeDebugInfo: AnsiString;

type
  TDebugNotifyGetCallStack = function(var CallStackCount: Cardinal): String of object;

function DebugNotify(Kind: TDebugEntryKind; Index: Integer;
  var ADebugContinueStepOver: Boolean; const GetCallStack: TDebugNotifyGetCallStack = nil): Boolean;
procedure DebugNotifyException(Exception: String; Kind: TDebugEntryKind; Index: Integer);
function DebugNotifyIntermediate(Kind: TDebugEntryKind; Index: Integer;
  var ADebugContinueStepOver: Boolean): Boolean;
procedure DebugNotifyLogMessage(const Msg: String);
procedure DebugNotifyTempDir(const Dir: String);
procedure DebugNotifyUninstExe(UninstExe: String);
procedure EndDebug;
procedure SetDebugServerWnd(Wnd: HWND; WantCodeText: Boolean);

implementation

uses
  Forms, Classes, Shared.CommonFunc, Shared.Struct, Setup.InstFunc, Setup.MainFunc;

type
  TDummyClass = class
  private
    class procedure DebugClientWndProc(var Message: TMessage);
  end;

var
  DebugServerWnd: HWND;
  DebugClientWnd: HWND;
  DebugContinue: Boolean;
  DebugContinueStepOver: Boolean;

procedure SetDebugServerWnd(Wnd: HWND; WantCodeText: Boolean);
var
  DebuggerVersion: Cardinal;
  I: Integer;
begin
  { First, verify that the debugger/IDE is the same version as Setup.
    A mismatch is possible when debugging an uninstaller if the uninstaller
    EXE was created by an installer built with a later version of IS. We can't
    continue in such a case because the debugger would send over updated
    "compiled code text" that is incompatible with this version of Setup. }
  DebuggerVersion := SendMessage(Wnd, WM_Debugger_QueryVersion, 0, 0);
  if DebuggerVersion <> SetupBinVersion then
    raise Exception.CreateFmt('Cannot debug. Debugger version ($%.8x) does ' +
      'not match Setup version ($%.8x)', [DebuggerVersion, SetupBinVersion]);

  Debugging := True;
  DebugServerWnd := Wnd;
  DebugClientWnd := AllocateHWnd(TDummyClass.DebugClientWndProc);
  if DebugClientWnd = 0 then
    InternalError('Failed to create DebugClientWnd');

  { Unprivileged processes can't send messages to elevated processes by default.
    Allow the debugger (which normally runs unprivileged) to send messages to us. }
  for I := Low(DebugClientMessages) to High(DebugClientMessages) do
    AddToWindowMessageFilterEx(DebugClientWnd, DebugClientMessages[I]);

  SendMessage(DebugServerWnd, WM_Debugger_Hello, WPARAM(DebugClientWnd), LPARAM(WantCodeText));
end;

procedure EndDebug;
begin
  Debugging := False;
  if DebugServerWnd <> 0 then begin
    SendMessage(DebugServerWnd, WM_Debugger_Goodbye, 0, 0);
    DebugServerWnd := 0;
  end;
  if DebugClientWnd <> 0 then begin
    DeallocateHWnd(DebugClientWnd);
    DebugClientWnd := 0;
  end;
end;

function InternalDebugNotify(DebuggerMsg: UINT; Kind: TDebugEntryKind;
  Index: Integer; var ADebugContinueStepOver: Boolean;
  const GetCallStack: TDebugNotifyGetCallStack = nil; const GetCallStackData: Pointer = nil): Boolean;
{ Returns True if the debugger paused. ADebugContinueStepOver is set to True
  if the debugger paused and the user resumed via Step Over, False otherwise. }
var
  SaveAppTitle, CallStack: String;
  WindowList: Pointer;
  Msg: TMsg;
  TopWindow: HWND;
  CallStackCount: Cardinal;
begin
  Result := False;
  ADebugContinueStepOver := False;
  if not Debugging then
    Exit;

  DebugContinue := False;

  if SendMessage(DebugServerWnd, DebuggerMsg, Ord(Kind), Index) = 0 then begin
    { Don't pause }
    Exit;
  end;

  if Assigned(GetCallStack) then begin
    CallStack := GetCallStack(CallStackCount);
    SendMessage(DebugServerWnd, WM_Debugger_CallStackCount, CallStackCount, 0);
    SendCopyDataMessageStr(DebugServerWnd, DebugClientWnd, CD_Debugger_CallStackW, CallStack);
  end;

  Result := True;

  { Wait until we get clearance to continue }
  SaveAppTitle := Application.Title;
  WindowList := DisableTaskWindows(0);
  try
    Application.Title := '[Paused] ' + SaveAppTitle;
    while not DebugContinue do begin
      case Integer(GetMessage(Msg, 0, 0, 0)) of
        -1: Break; { if GetMessage failed }
        0: begin
             { Repost WM_QUIT messages }
             PostQuitMessage(Msg.WParam);
             Break;
           end;
      end;
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
    ADebugContinueStepOver := DebugContinueStepOver;
  finally
    EnableTaskWindows(WindowList);
    Application.Title := SaveAppTitle;
  end;

  { Bring us back to the foreground, unless we've been detached }
  if Debugging then begin
    TopWindow := GetThreadTopWindow;
    if TopWindow = 0 then
      TopWindow := Application.Handle;
    if TopWindow <> 0 then begin
      { First ask the debugger to call SetForegroundWindow() on our window. If
        we don't do this then Windows (98/2000+) will prevent our window from
        becoming activated if the debugger is currently in the foreground. }
      SendMessage(DebugServerWnd, WM_Debugger_SetForegroundWindow, WPARAM(TopWindow), 0);
      { Now call SetForegroundWindow() ourself. Why? When a remote thread
        calls SetForegroundWindow(), the request is queued; the window doesn't
        actually become active until the next time the window's thread checks
        the message queue. This call causes the window to become active
        immediately. }
      SetForegroundWindow(TopWindow);
    end;
  end;
end;

function DebugNotify(Kind: TDebugEntryKind; Index: Integer;
  var ADebugContinueStepOver: Boolean;
  const GetCallStack: TDebugNotifyGetCallStack = nil): Boolean;
begin
  Result := InternalDebugNotify(WM_Debugger_Stepped, Kind, Index,
    ADebugContinueStepOver, GetCallStack);
end;

function DebugNotifyIntermediate(Kind: TDebugEntryKind; Index: Integer;
  var ADebugContinueStepOver: Boolean): Boolean;
begin
  Result := InternalDebugNotify(WM_Debugger_SteppedIntermediate, Kind, Index,
    ADebugContinueStepOver, nil);
end;

procedure DebugNotifyException(Exception: String; Kind: TDebugEntryKind; Index: Integer);
var
  B: Boolean;
begin
  SendCopyDataMessageStr(DebugServerWnd, DebugClientWnd, CD_Debugger_ExceptionW,
    Exception);
  InternalDebugNotify(WM_Debugger_Exception, Kind, Index, B);
end;

procedure DebugNotifyTempDir(const Dir: String);
begin
  SendCopyDataMessageStr(DebugServerWnd, DebugClientWnd, CD_Debugger_TempDirW, Dir);
end;

procedure DebugNotifyUninstExe(UninstExe: String);
begin
  SendCopyDataMessageStr(DebugServerWnd, DebugClientWnd, CD_Debugger_UninstExeW, UninstExe);
end;

procedure DebugNotifyLogMessage(const Msg: String);
begin
  SendCopyDataMessageStr(DebugServerWnd, DebugClientWnd, CD_Debugger_LogMessageW, Msg);
end;

class procedure TDummyClass.DebugClientWndProc(var Message: TMessage);
var
  VariableDebugEntry: TVariableDebugEntry;
  EvaluateExp, EvaluateResult: String;
begin
  try
    case Message.Msg of
      WM_DebugClient_Detach: begin
          Debugging := False;
          DebugServerWnd := 0;
          { If it's paused, force it to continue }
          DebugContinue := True;
          DebugContinueStepOver := False;
          { Make the GetMessage call in DebugNotify return immediately }
          PostMessage(0, 0, 0, 0);
        end;
      WM_DebugClient_Continue: begin
          DebugContinue := True;
          DebugContinueStepOver := Message.wParam = 1;
          { Make the GetMessage call in DebugNotify return immediately }
          PostMessage(0, 0, 0, 0);
        end;
      WM_DebugClient_SetForegroundWindow: begin
          SetForegroundWindow(HWND(Message.WParam));
        end;
      WM_COPYDATA: begin
          case TWMCopyData(Message).CopyDataStruct.dwData of
            CD_DebugClient_EvaluateConstantW: begin
                try
                  SetString(EvaluateExp, PChar(TWMCopyData(Message).CopyDataStruct.lpData),
                    TWMCopyData(Message).CopyDataStruct.cbData div SizeOf(Char));
                  try
                    Inc(DisableCodeConsts);
                    try
                      EvaluateResult := ExpandConst(EvaluateExp);
                    finally
                      Dec(DisableCodeConsts);
                    end;
                    Message.Result := 1;
                  except
                    EvaluateResult := GetExceptMessage;
                    Message.Result := 2;
                  end;
                  SendCopyDataMessageStr(DebugServerWnd, DebugClientWnd, CD_Debugger_ReplyW,
                    EvaluateResult);
                except
                  { don't propagate exceptions }
                end;
              end;
            CD_DebugClient_EvaluateVariableEntry: begin
                try
                  Move(TWMCopyData(Message).CopyDataStruct.lpData^, VariableDebugEntry, SizeOf(VariableDebugEntry));
                  try
                    if CodeRunner = nil then
                      raise Exception.Create('Cannot evaluate variable because [Code] isn''t running yet');
                    EvaluateResult := CodeRunner.EvaluateUsedVariable(VariableDebugEntry.Param1,
                      VariableDebugEntry.Param2, VariableDebugEntry.Param3, VariableDebugEntry.Param4);
                    Message.Result := 1;
                  except
                    EvaluateResult := GetExceptMessage;
                    Message.Result := 2;
                  end;
                  SendCopyDataMessageStr(DebugServerWnd, DebugClientWnd, CD_Debugger_ReplyW,
                    EvaluateResult);
                except
                  { don't propagate exceptions }
                end;
              end;
            CD_DebugClient_CompiledCodeTextA: begin
                try
                  DebugClientCompiledCodeText := '';
                  SetString(DebugClientCompiledCodeText, PAnsiChar(TWMCopyData(Message).CopyDataStruct.lpData),
                    TWMCopyData(Message).CopyDataStruct.cbData div SizeOf(AnsiChar));
                  Message.Result := 1;
                except
                  { don't propagate exceptions }
                end;
              end;
            CD_DebugClient_CompiledCodeDebugInfoA: begin
                try
                  DebugClientCompiledCodeDebugInfo := '';
                  SetString(DebugClientCompiledCodeDebugInfo, PAnsiChar(TWMCopyData(Message).CopyDataStruct.lpData),
                    TWMCopyData(Message).CopyDataStruct.cbData div SizeOf(AnsiChar));
                  Message.Result := 1;
                except
                  { don't propagate exceptions }
                end;
              end;
          end;
        end;
    else
      with Message do
        Result := DefWindowProc(DebugClientWnd, Msg, WParam, LParam);
    end;
  except
    Application.HandleException(nil);
  end
end;

end.
