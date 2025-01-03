unit IDE.HtmlHelpFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Functions for HTML Help
}

interface

uses
  Windows;

const
  HH_DISPLAY_TOPIC = $0000;
  HH_KEYWORD_LOOKUP = $000D;

type
  THtmlHelp = function(hwndCaller: HWND; pszFile: PChar; uCommand: UINT; dwData: DWORD): HWND; stdcall;

  PHH_AKLink = ^THH_AKLink;
  THH_AKLINK = record
    cbStruct: Integer;
    fReserved: Bool;
    pszKeywords: PChar;
    pszUrl: PChar;
    pszMsgText: PChar;
    pszMsgTitle: PChar;
    pszWindow: PChar;
    fIndexOnFail: Bool;
  end;

var
  HtmlHelp: THtmlHelp;

procedure InitHtmlHelpLibrary;
procedure FreeHtmlHelpLibrary;

implementation

uses
  Messages, SysUtils, Shared.CommonFunc, PathFunc;

var
  HHCtrl: THandle;

procedure InitHtmlHelpLibrary;
begin
  if HHCtrl = 0 then begin
    HHCtrl := LoadLibrary(PChar(AddBackslash(GetSystemDir) + 'hhctrl.ocx'));
    if HHCtrl <> 0 then
      HtmlHelp := GetProcAddress(HHCtrl, 'HtmlHelpW')
    else
      HtmlHelp := nil;
   end;
end;

procedure FreeHtmlHelpLibrary;
begin
  if HHCtrl <> 0 then begin
    HtmlHelp := nil;
    FreeLibrary(HHCtrl);
    HHCtrl := 0;
  end;
end;

function CloseHtmlHelpWindowsEnumProc(Wnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  PID: DWORD;
  ClassName: array[0..31] of Char;
  MsgResult: DWORD_PTR;
begin
  if (GetWindowThreadProcessId(Wnd, @PID) <> 0) and
     (PID = GetCurrentProcessId) then begin
    if (GetClassName(Wnd, ClassName, SizeOf(ClassName) div SizeOf(ClassName[0])) > 0) and
       (StrIComp(ClassName, 'HH Parent') = 0) then begin
      { Consider only enabled windows. If an HTML Help window is disabled
        because it's waiting on a modal dialog (e.g. Properties) then it's
        probably not safe to close it. }
      if IsWindowEnabled(Wnd) then
        SendMessageTimeout(Wnd, WM_CLOSE, 0, 0, SMTO_BLOCK, 7500, @MsgResult);
    end;
  end;
  Result := True;
end;

procedure CloseHtmlHelpWindows;
begin
  { Note: We don't call HtmlHelp(HH_CLOSE_ALL) here because its operation is
    asynchronous. (See: http://helpware.net/FAR/far_faq.htm#HH_CLOSE_ALL)
    If HHCTRL.OCX is unloaded too quickly after HH_CLOSE_ALL, a crash can
    occur. Even if HHCTRL.OCX isn't explicitly unloaded, it still *might* be
    possible for the main thread to exit while the HH thread is still in the
    process of closing the window and cleaning up the temporary file.
    Therefore, we use a different approach: we find the window(s) and send a
    WM_CLOSE message synchronously. }
  if Assigned(HtmlHelp) then
    EnumWindows(@CloseHtmlHelpWindowsEnumProc, 0);
end;

initialization
finalization
  { Must explicitly close any open HTML Help window before terminating,
    otherwise it leaves behind a temporary file. (Most apps don't bother
    doing this, including IE and Outlook Express.) }
  CloseHtmlHelpWindows;
end.
