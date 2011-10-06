unit DebugClient;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Debug info stuff

  $jrsoftware: issrc/Projects/DebugClient.pas,v 1.29 2010/10/30 19:50:37 jr Exp $
}

interface

uses
  Windows, SysUtils, Messages, DebugStruct;

var
  Debugging: Boolean;
  DebugClientCompiledCodeText: AnsiString;
  DebugClientCompiledCodeDebugInfo: AnsiString;

function DebugNotify(Kind: TDebugEntryKind; Index: Integer;
  var ADebugContinueStepOver: Boolean): Boolean;
procedure DebugNotifyException(Exception: String; Kind: TDebugEntryKind; Index: Integer);
function DebugNotifyIntermediate(Kind: TDebugEntryKind; Index: Integer;
  var ADebugContinueStepOver: Boolean): Boolean;
procedure DebugNotifyLogMessage(const Msg: String);
procedure DebugNotifyTempDir(const Dir: String);
procedure DebugNotifyUninstExe(UninstExe: String);
procedure EndDebug;
procedure SetDebugWnd(Wnd: HWND; WantCodeText: Boolean);

implementation

uses
  Forms, Classes, CmnFunc2, Struct, InstFunc, Main;

type
  TDummyClass = class
  private
    class procedure DebugClientWndProc(var Message: TMessage);
  end;

var
  DebugWnd: HWND;
  DebugClientWnd: HWND;
  DebugContinue: Boolean;
  DebugContinueStepOver: Boolean;

procedure SetDebugWnd(Wnd: HWND; WantCodeText: Boolean);
var
  DebuggerVersion: Cardinal;
  I: Integer;
begin
  { First, verify that the debugger/compiler is the same version as Setup.
    A mismatch is possible when debugging an uninstaller if the uninstaller
    EXE was created by an installer built with a later version of IS. We can't
    continue in such a case because the debugger would send over updated
    "compiled code text" that is incompatible with this version of Setup. }
  DebuggerVersion := SendMessage(Wnd, WM_Debugger_QueryVersion, 0, 0);
  if DebuggerVersion <> SetupBinVersion then
    raise Exception.CreateFmt('Cannot debug. Debugger version ($%.8x) does ' +
      'not match Setup version ($%.8x)', [DebuggerVersion, SetupBinVersion]);

  Debugging := True;
  DebugWnd := Wnd;
  DebugClientWnd := AllocateHWnd(TDummyClass.DebugClientWndProc);
  if DebugClientWnd = 0 then
    InternalError('Failed to create DebugClientWnd');

  { On Vista, unprivileged processes can't send messages to elevated processes
    by default. Allow the debugger (which normally runs unprivileged) to send
    messages to us. }
  for I := Low(DebugClientMessages) to High(DebugClientMessages) do
    AddToWindowMessageFilterEx(DebugClientWnd, DebugClientMessages[I]);

  SendMessage(DebugWnd, WM_Debugger_Hello, WPARAM(DebugClientWnd), LPARAM(WantCodeText));
end;

procedure EndDebug;
begin
  Debugging := False;
  if DebugWnd <> 0 then begin
    SendMessage(DebugWnd, WM_Debugger_Goodbye, 0, 0);
    DebugWnd := 0;
  end;
  if DebugClientWnd <> 0 then begin
    DeallocateHWnd(DebugClientWnd);
    DebugClientWnd := 0;
  end;
end;

function InternalDebugNotify(DebuggerMsg: UINT; Kind: TDebugEntryKind;
  Index: Integer; var ADebugContinueStepOver: Boolean): Boolean;
{ Returns True if the debugger paused. ADebugContinueStepOver is set to True
  if the debugger paused and the user resumed via Step Over, False otherwise. }
var
  SaveAppTitle: String;
  WindowList: Pointer;
  Msg: TMsg;
  TopWindow: HWND;
begin
  Result := False;
  ADebugContinueStepOver := False;
  if not Debugging then
    Exit;

  DebugContinue := False;

  if SendMessage(DebugWnd, DebuggerMsg, Ord(Kind), Index) = 0 then begin
    { Don't pause }
    Exit;
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
    if TopWindow <> 0 then begin
      { First ask the debugger to call SetForegroundWindow() on our window. If
        we don't do this then Windows (98/2000+) will prevent our window from
        becoming activated if the debugger is currently in the foreground. }
      SendMessage(DebugWnd, WM_Debugger_SetForegroundWindow, WPARAM(TopWindow), 0);
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
  var ADebugContinueStepOver: Boolean): Boolean;
begin
  Result := InternalDebugNotify(WM_Debugger_Stepped, Kind, Index,
    ADebugContinueStepOver);
end;

function DebugNotifyIntermediate(Kind: TDebugEntryKind; Index: Integer;
  var ADebugContinueStepOver: Boolean): Boolean;
begin
  Result := InternalDebugNotify(WM_Debugger_SteppedIntermediate, Kind, Index,
    ADebugContinueStepOver);
end;

procedure DebugNotifyException(Exception: String; Kind: TDebugEntryKind; Index: Integer);
var
  B: Boolean;
begin
  SendCopyDataMessageStr(DebugWnd, DebugClientWnd, CD_Debugger_ExceptionW,
    Exception);
  InternalDebugNotify(WM_Debugger_Exception, Kind, Index, B);
end;

procedure DebugNotifyTempDir(const Dir: String);
begin
  SendCopyDataMessageStr(DebugWnd, DebugClientWnd, CD_Debugger_TempDirW, Dir);
end;

procedure DebugNotifyUninstExe(UninstExe: String);
begin
  SendCopyDataMessageStr(DebugWnd, DebugClientWnd, CD_Debugger_UninstExeW, UninstExe);
end;

procedure DebugNotifyLogMessage(const Msg: String);
begin
  SendCopyDataMessageStr(DebugWnd, DebugClientWnd, CD_Debugger_LogMessageW, Msg);
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
          DebugWnd := 0;
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
                  SendCopyDataMessageStr(DebugWnd, DebugClientWnd, CD_Debugger_ReplyW,
                    EvaluateResult);
                except
                  { don't propogate exceptions }
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
                  SendCopyDataMessageStr(DebugWnd, DebugClientWnd, CD_Debugger_ReplyW,
                    EvaluateResult);
                except
                  { don't propogate exceptions }
                end;
              end;
            CD_DebugClient_CompiledCodeTextA: begin
                try
                  DebugClientCompiledCodeText := '';
                  SetString(DebugClientCompiledCodeText, PAnsiChar(TWMCopyData(Message).CopyDataStruct.lpData),
                    TWMCopyData(Message).CopyDataStruct.cbData div SizeOf(AnsiChar));
                  Message.Result := 1;
                except
                  { don't propogate exceptions }
                end;
              end;
            CD_DebugClient_CompiledCodeDebugInfoA: begin
                try
                  DebugClientCompiledCodeDebugInfo := '';
                  SetString(DebugClientCompiledCodeDebugInfo, PAnsiChar(TWMCopyData(Message).CopyDataStruct.lpData),
                    TWMCopyData(Message).CopyDataStruct.cbData div SizeOf(AnsiChar));
                  Message.Result := 1;
                except
                  { don't propogate exceptions }
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
