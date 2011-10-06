unit DebugStruct;

{
  Inno Setup
  Copyright (C) 1997-2007 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Debug info stuff

  $jrsoftware: issrc/Projects/DebugStruct.pas,v 1.20 2009/04/21 13:46:04 mlaan Exp $
}

interface

uses
  Windows, Messages, SysUtils;

const
  { Debug client -> debugger messages }
  WM_Debugger_Hello = WM_USER + $700;
  WM_Debugger_Goodbye = WM_USER + $701;
  WM_Debugger_Stepped = WM_USER + $702;
  WM_Debugger_SteppedIntermediate = WM_USER + $703;
  WM_Debugger_Exception = WM_USER + $704;
  WM_Debugger_SetForegroundWindow = WM_USER + $705;
  WM_Debugger_QueryVersion = WM_USER + $706;
  { Debug client -> debugger WM_COPYDATA messages }
  CD_Debugger_ReplyW = $700;
  CD_Debugger_ExceptionW = $701;
  CD_Debugger_UninstExeW = $702;
  CD_Debugger_LogMessageW = $703;
  CD_Debugger_TempDirW = $704;

  { Debugger -> debug client messages }
  WM_DebugClient_Detach = WM_USER + $800;
  WM_DebugClient_Continue = WM_USER + $801;
  WM_DebugClient_SetForegroundWindow = WM_USER + $803;
  { List of all messages the debugger may send the debug client }
  DebugClientMessages: array[0..3] of UINT = (
    WM_COPYDATA,
    WM_DebugClient_Detach,
    WM_DebugClient_Continue,
    WM_DebugClient_SetForegroundWindow);
  { Debugger -> debug client WM_COPYDATA messages }
  CD_DebugClient_EvaluateConstantW = $800;
  CD_DebugClient_EvaluateVariableEntry = $801;
  CD_DebugClient_CompiledCodeTextA = $802;
  CD_DebugClient_CompiledCodeDebugInfoA = $803;

{ The current format of the 'debug info' is as follows:
  1. A TDebugInfoHeader record.
  2. A variable number (TDebugInfoHeader.DebugEntryCount) of TDebugEntry
     records.
  3. A variable number (TDebugInfoHeader.VariableDebugEntryCount) of
     TVariableDebugEntry records.
  4. The ROPS compiled code, the format of which is defined by ROPS.
     TDebugInfoHeader.CompiledCodeTextLength specifies the size in bytes.
  5. Additional debug info for the ROPS compiled code, the format of which is
     defined by ROPS. TDebugInfoHeader.CompiledCodeDebugInfoLength specifies
     the size in bytes.
}

const
  DebugInfoHeaderID = $64787369;
  DebugInfoHeaderVersion = 4;

type
  PDebugInfoHeader = ^TDebugInfoHeader;
  TDebugInfoHeader = packed record
    ID: Cardinal;      { = DebugInfoHeaderID }
    Version: Integer;  { = DebugInfoHeaderVersion }
    DebugEntryCount: Integer;
    VariableDebugEntryCount: Integer;
    CompiledCodeTextLength: Integer;
    CompiledCodeDebugInfoLength: Integer;
  end;

  { TDebugEntrys associate section entries with line numbers }
  TDebugEntryKind = (deDir, deFile, deIcon, deIni, deRegistry, deInstallDelete,
    deUninstallDelete, deRun, deUninstallRun, deCodeLine);
  PDebugEntry = ^TDebugEntry;
  TDebugEntry = packed record
    LineNumber: Integer;
    Kind: Integer;  { TDebugEntryKind }
    Index: Integer;
  end;

  { TVariableDebugEntrys associate [Code] section variable referenes with line
    numbers & column positions }
  PVariableDebugEntry = ^TVariableDebugEntry;
  TVariableDebugEntry = packed record
    LineNumber, Col: Integer;
    Param1, Param2, Param3: Integer;
    Param4: array [0..127] of AnsiChar;
  end;

function GetThreadTopWindow: HWND;
function SendCopyDataMessage(DestWnd, SourceWnd: HWND; CopyDataMsg: DWORD;
  Data: Pointer; Size: Cardinal): LRESULT;
function SendCopyDataMessageStr(DestWnd, SourceWnd: HWND; CopyDataMsg: DWORD;
  Data: AnsiString): LRESULT;{$IFDEF UNICODE} overload;{$ENDIF}
{$IFDEF UNICODE}
function SendCopyDataMessageStr(DestWnd, SourceWnd: HWND; CopyDataMsg: DWORD;
  Data: UnicodeString): LRESULT; overload;
{$ENDIF}

implementation

function EnumProc(Wnd: HWND; lParam: LPARAM): BOOL; stdcall;
begin
  if IsWindowVisible(Wnd) then begin
    HWND(Pointer(lParam)^) := Wnd;
    Result := False;
  end
  else
    Result := True;
end;

function GetThreadTopWindow: HWND;
begin
  Result := 0;
  EnumThreadWindows(GetCurrentThreadId, @EnumProc, LPARAM(@Result));
end;

function SendCopyDataMessage(DestWnd, SourceWnd: HWND; CopyDataMsg: DWORD;
  Data: Pointer; Size: Cardinal): LRESULT;
var
  CopyDataStruct: TCopyDataStruct;
begin
  CopyDataStruct.dwData := CopyDataMsg;
  CopyDataStruct.cbData := Size;
  CopyDataStruct.lpData := Data;
  Result := SendMessage(DestWnd, WM_COPYDATA, WPARAM(SourceWnd),
    LPARAM(@CopyDataStruct));
end;

function SendCopyDataMessageStr(DestWnd, SourceWnd: HWND; CopyDataMsg: DWORD;
  Data: AnsiString): LRESULT;
begin
  { Windows 95/98/Me bug workaround: Call UniqueString to ensure the string is
    in writable memory. Amazingly enough, sending a WM_COPYDATA message with a
    read-only buffer causes a fatal page fault error. }
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
     IsBadWritePtr(Pointer(Data), Length(Data)*SizeOf(Data[1])) then
    UniqueString(Data);
  Result := SendCopyDataMessage(DestWnd, SourceWnd, CopyDataMsg,
    Pointer(Data), Length(Data)*SizeOf(Data[1]));
end;

{$IFDEF UNICODE}
function SendCopyDataMessageStr(DestWnd, SourceWnd: HWND; CopyDataMsg: DWORD;
  Data: UnicodeString): LRESULT;
begin
  { Windows 95/98/Me bug workaround: Call UniqueString to ensure the string is
    in writable memory. Amazingly enough, sending a WM_COPYDATA message with a
    read-only buffer causes a fatal page fault error. }
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
     IsBadWritePtr(Pointer(Data), Length(Data)*SizeOf(Data[1])) then
    UniqueString(Data);
  Result := SendCopyDataMessage(DestWnd, SourceWnd, CopyDataMsg,
    Pointer(Data), Length(Data)*SizeOf(Data[1]));
end;
{$ENDIF}

end.
