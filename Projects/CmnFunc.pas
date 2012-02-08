unit CmnFunc;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Common VCL functions
}

{$B-}

interface

{$I VERSION.INC}

uses
  Windows, Messages, SysUtils, Forms, Graphics, Controls, StdCtrls, Classes;

type
  TWindowDisabler = class
  private
    FFallbackWnd, FOwnerWnd: HWND;
    FPreviousActiveWnd, FPreviousFocusWnd: HWND;
    FWindowList: Pointer;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { Note: This type is also present in ScriptFunc_C.pas }
  TMsgBoxType = (mbInformation, mbConfirmation, mbError, mbCriticalError);

  TMsgBoxCallbackFunc = procedure(const Flags: LongInt; const After: Boolean;
    const Param: LongInt);

{ Useful constant }
const
  EnableColor: array[Boolean] of TColor = (clBtnFace, clWindow);

procedure UpdateHorizontalExtent(const ListBox: TCustomListBox);
function MinimizePathName(const Filename: String; const Font: TFont;
  MaxLen: Integer): String;
function AppMessageBox(const Text, Caption: PChar; Flags: Longint): Integer;
function MsgBoxP(const Text, Caption: PChar; const Typ: TMsgBoxType;
  const Buttons: Cardinal): Integer;
function MsgBox(const Text, Caption: String; const Typ: TMsgBoxType;
  const Buttons: Cardinal): Integer;
function MsgBoxFmt(const Text: String; const Args: array of const;
  const Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal): Integer;
procedure ReactivateTopWindow;
procedure SetMessageBoxCaption(const Typ: TMsgBoxType; const NewCaption: PChar);
procedure SetMessageBoxRightToLeft(const ARightToLeft: Boolean);
procedure SetMessageBoxCallbackFunc(const AFunc: TMsgBoxCallbackFunc; const AParam: LongInt);

implementation

uses
  Consts, PathFunc, CmnFunc2;

var
  MessageBoxCaptions: array[TMsgBoxType] of PChar;
  MessageBoxRightToLeft: Boolean;
  MessageBoxCallbackFunc: TMsgBoxCallbackFunc;
  MessageBoxCallbackParam: LongInt;
  MessageBoxCallbackActive: Boolean;

type
  TListBoxAccess = class(TCustomListBox);

procedure UpdateHorizontalExtent(const ListBox: TCustomListBox);
var
  I: Integer;
  Extent, MaxExtent: Longint;
  DC: HDC;
  Size: TSize;
  TextMetrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    SelectObject(DC, TListBoxAccess(ListBox).Font.Handle);

    //Q66370 says tmAveCharWidth should be added to extent
    GetTextMetrics(DC, TextMetrics);

    MaxExtent := 0;
    for I := 0 to ListBox.Items.Count-1 do begin
      GetTextExtentPoint32(DC, PChar(ListBox.Items[I]), Length(ListBox.Items[I]), Size);
      Extent := Size.cx + TextMetrics.tmAveCharWidth;
      if Extent > MaxExtent then
        MaxExtent := Extent;
    end;

  finally
    ReleaseDC(0, DC);
  end;

  if MaxExtent > SendMessage(ListBox.Handle, LB_GETHORIZONTALEXTENT, 0, 0) then
    SendMessage(ListBox.Handle, LB_SETHORIZONTALEXTENT, MaxExtent, 0);
end;

function MinimizePathName(const Filename: String; const Font: TFont;
  MaxLen: Integer): String;

  procedure CutFirstDirectory(var S: String);
  var
    P: Integer;
  begin
    if Copy(S, 1, 4) = '...\' then
      Delete(S, 1, 4);
    P := PathPos('\', S);
    if P <> 0 then
    begin
      Delete(S, 1, P);
      S := '...\' + S;
    end
    else
      S := '';
  end;

var
  DC: HDC;
  Drive, Dir, Name: String;
  DriveLen: Integer;
begin
  DC := GetDC(0);
  try
    SelectObject(DC, Font.Handle);

    Result := FileName;
    Dir := PathExtractPath(Result);
    Name := PathExtractName(Result);

    DriveLen := PathDrivePartLength(Dir);
    { Include any slash following drive part, or a leading slash if DriveLen=0 }
    if (DriveLen < Length(Dir)) and PathCharIsSlash(Dir[DriveLen+1]) then
      Inc(DriveLen);
    Drive := Copy(Dir, 1, DriveLen);
    Delete(Dir, 1, DriveLen);

    while ((Dir <> '') or (Drive <> '')) and (GetTextWidth(DC, Result, False) > MaxLen) do
    begin
      if Dir <> '' then
        CutFirstDirectory(Dir);
      { If there's no directory left, minimize the drive part.
        'C:\...\filename' -> '...\filename' }
      if (Dir = '') and (Drive <> '') then
      begin
        Drive := '';
        Dir := '...\';
      end;
      Result := Drive + Dir + Name;
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure SetMessageBoxCaption(const Typ: TMsgBoxType; const NewCaption: PChar);
begin
  StrDispose(MessageBoxCaptions[Typ]);
  MessageBoxCaptions[Typ] := nil;
  if Assigned(NewCaption) then
    MessageBoxCaptions[Typ] := StrNew(NewCaption);
end;

procedure SetMessageBoxRightToLeft(const ARightToLeft: Boolean);
begin
  MessageBoxRightToLeft := ARightToLeft;
end;

procedure SetMessageBoxCallbackFunc(const AFunc: TMsgBoxCallbackFunc; const AParam: LongInt);
begin
  MessageBoxCallbackFunc := AFunc;
  MessageBoxCallbackParam := AParam;
end;

procedure TriggerMessageBoxCallbackFunc(const Flags: LongInt; const After: Boolean);
begin
  if Assigned(MessageBoxCallbackFunc) and not MessageBoxCallbackActive then begin
    MessageBoxCallbackActive := True;
    try
      MessageBoxCallbackFunc(Flags, After, MessageBoxCallbackParam);
    finally
      MessageBoxCallbackActive := False;
    end;
  end;
end;

{$IFNDEF IS_D4}
function MoveAppWindowToActiveWindowMonitor(var OldRect: TRect): Boolean;
{ This moves the application window (Application.Handle) to the same monitor
  as the active window, so that a subsequent call to Application.MessageBox
  displays the message box on that monitor. Based on code from D4+'s
  TApplication.MessageBox. }
type
  HMONITOR = type THandle;
  TMonitorInfo = record
    cbSize: DWORD;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFlags: DWORD;
  end;
const
  MONITOR_DEFAULTTONEAREST = $00000002;
var
  ActiveWindow: HWND;
  Module: HMODULE;
  MonitorFromWindow: function(hwnd: HWND; dwFlags: DWORD): HMONITOR; stdcall;
  GetMonitorInfo: function(hMonitor: HMONITOR; var lpmi: TMonitorInfo): BOOL; stdcall;
  MBMonitor, AppMonitor: HMONITOR;
  Info: TMonitorInfo;
begin
  Result := False;
  ActiveWindow := GetActiveWindow;
  if ActiveWindow = 0 then Exit;
  Module := GetModuleHandle(user32);
  MonitorFromWindow := GetProcAddress(Module, 'MonitorFromWindow');
  GetMonitorInfo := GetProcAddress(Module, 'GetMonitorInfoA');
  if Assigned(MonitorFromWindow) and Assigned(GetMonitorInfo) then begin
    MBMonitor := MonitorFromWindow(ActiveWindow, MONITOR_DEFAULTTONEAREST);
    AppMonitor := MonitorFromWindow(Application.Handle, MONITOR_DEFAULTTONEAREST);
    if MBMonitor <> AppMonitor then begin
      Info.cbSize := SizeOf(Info);
      if GetMonitorInfo(MBMonitor, Info) then begin
        GetWindowRect(Application.Handle, OldRect);
        SetWindowPos(Application.Handle, 0,
          Info.rcMonitor.Left + ((Info.rcMonitor.Right - Info.rcMonitor.Left) div 2),
          Info.rcMonitor.Top + ((Info.rcMonitor.Bottom - Info.rcMonitor.Top) div 2),
          0, 0, SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOSIZE or SWP_NOZORDER);
        Result := True;
      end;
    end;
  end;
end;
{$ENDIF}

function AppMessageBox(const Text, Caption: PChar; Flags: Longint): Integer;
var
  ActiveWindow: HWND;
  WindowList: Pointer;
{$IFNDEF IS_D4}
  DidMove: Boolean;
  OldRect: TRect;
{$ENDIF}
begin
  if MessageBoxRightToLeft then
    Flags := Flags or (MB_RTLREADING or MB_RIGHT);

  { If the application window isn't currently visible, show the message box
    with no owner window so it'll get a taskbar button } 
  if (GetWindowLong(Application.Handle, GWL_STYLE) and WS_VISIBLE = 0) or
     (GetWindowLong(Application.Handle, GWL_EXSTYLE) and WS_EX_TOOLWINDOW <> 0) then begin
    ActiveWindow := GetActiveWindow;
    WindowList := DisableTaskWindows(0);
    try
      { Note: DisableTaskWindows doesn't disable invisible windows.
        MB_TASKMODAL will ensure that Application.Handle gets disabled too. }
      Result := MessageBox(0, Text, Caption, Flags or MB_TASKMODAL);
    finally
      EnableTaskWindows(WindowList);
      SetActiveWindow(ActiveWindow);
    end;
    Exit;
  end;

  TriggerMessageBoxCallbackFunc(Flags, False);
  try
{$IFDEF IS_D4}
    { On Delphi 4+, simply call Application.MessageBox }
    Result := Application.MessageBox(Text, Caption, Flags);
{$ELSE}
    { Use custom implementation on Delphi 2 and 3. The Flags parameter is
      incorrectly declared as a Word on Delphi 2's Application.MessageBox, and
      there is no support for multiple monitors. }
    DidMove := MoveAppWindowToActiveWindowMonitor(OldRect);
    try
      ActiveWindow := GetActiveWindow;
      WindowList := DisableTaskWindows(0);
      try
        Result := MessageBox(Application.Handle, Text, Caption, Flags);
      finally
        EnableTaskWindows(WindowList);
        SetActiveWindow(ActiveWindow);
      end;
    finally
      if DidMove then
        SetWindowPos(Application.Handle, 0,
          OldRect.Left + ((OldRect.Right - OldRect.Left) div 2),
          OldRect.Top + ((OldRect.Bottom - OldRect.Top) div 2),
          0, 0, SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOSIZE or SWP_NOZORDER);
    end;
{$ENDIF}
  finally
    TriggerMessageBoxCallbackFunc(Flags, True);
  end;
end;

function MsgBoxP(const Text, Caption: PChar; const Typ: TMsgBoxType;
  const Buttons: Cardinal): Integer;
const
  IconFlags: array[TMsgBoxType] of Cardinal =
    (MB_ICONINFORMATION, MB_ICONQUESTION, MB_ICONEXCLAMATION, MB_ICONSTOP);
  {$IFNDEF Delphi3orHigher}
  DefaultCaptions: array[TMsgBoxType] of Word =
    (SMsgDlgInformation, SMsgDlgConfirm, SMsgDlgError, SMsgDlgError);
  {$ELSE}
  DefaultCaptions: array[TMsgBoxType] of Pointer =
    (@SMsgDlgInformation, @SMsgDlgConfirm, @SMsgDlgError, @SMsgDlgError);
  {$ENDIF}
var
  C: PChar;
  NewCaption: String;
begin
  C := Caption;
  if (C = nil) or (C[0] = #0) then begin
    C := MessageBoxCaptions[Typ];
    if C = nil then begin
      {$IFNDEF Delphi3orHigher}
      NewCaption := LoadStr(DefaultCaptions[Typ]);
      {$ELSE}
      NewCaption := LoadResString(DefaultCaptions[Typ]);
      {$ENDIF}
      C := PChar(NewCaption);
    end;
  end;
  Result := AppMessageBox(Text, C, Buttons or IconFlags[Typ]);
end;

function MsgBox(const Text, Caption: String; const Typ: TMsgBoxType;
  const Buttons: Cardinal): Integer;
begin
  Result := MsgBoxP(PChar(Text), PChar(Caption), Typ, Buttons);
end;

function MsgBoxFmt(const Text: String; const Args: array of const;
  const Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal): Integer;
begin
  Result := MsgBox(Format(Text, Args), Caption, Typ, Buttons);
end;

function ReactivateTopWindowEnumProc(Wnd: HWND; LParam: LPARAM): BOOL; stdcall;
begin
  { Stop if we encounter the application window; don't consider it or any
    windows below it }
  if Wnd = Application.Handle then
    Result := False
  else
  if IsWindowVisible(Wnd) and IsWindowEnabled(Wnd) and
     (GetWindowLong(Wnd, GWL_EXSTYLE) and (WS_EX_TOPMOST or WS_EX_TOOLWINDOW) = 0) then begin
    SetActiveWindow(Wnd);
    Result := False;
  end
  else
    Result := True;
end;

procedure ReactivateTopWindow;
{ If the application window is active, reactivates the top window owned by the
  current thread. Tool windows and windows that are invisible, disabled, or
  topmost are not considered. }
begin
  if GetActiveWindow = Application.Handle then
    EnumThreadWindows(GetCurrentThreadId, @ReactivateTopWindowEnumProc, 0);
end;

procedure FreeCaptions; far;
var
  T: TMsgBoxType;
begin
  for T := Low(T) to High(T) do begin
    StrDispose(MessageBoxCaptions[T]);
    MessageBoxCaptions[T] := nil;
  end;
end;

{ TWindowDisabler }

const
  WindowDisablerWndClassName = 'TWindowDisabler-Window';
var
  WindowDisablerWndClassAtom: TAtom;

function WindowDisablerWndProc(Wnd: HWND; Msg: UINT; WParam: WPARAM;
  LParam: LPARAM): LRESULT; stdcall;
begin
  if Msg = WM_CLOSE then
    { If the fallback window becomes focused (e.g. by Alt+Tabbing onto it) and
      Alt+F4 is pressed, we must not pass the message to DefWindowProc because
      it would destroy the window }
    Result := 0
  else
    Result := DefWindowProc(Wnd, Msg, WParam, LParam);
end;

constructor TWindowDisabler.Create;
const
  WndClass: TWndClass = (
    style: 0;
    lpfnWndProc: @WindowDisablerWndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: COLOR_WINDOW + 1;
    lpszMenuName: nil;
    lpszClassName: WindowDisablerWndClassName);
begin
  inherited Create;
  FPreviousActiveWnd := GetActiveWindow;
  FPreviousFocusWnd := GetFocus;
  FWindowList := DisableTaskWindows(0);

  { Create the "fallback" window.
    When a child process hides its last window, Windows will try to activate
    the top-most enabled window on the desktop. If all of our windows were
    disabled, it would end up bringing some other application to the
    foreground. This gives Windows an enabled window to re-activate, which
    is invisible to the user. }
  if WindowDisablerWndClassAtom = 0 then
    WindowDisablerWndClassAtom := Windows.RegisterClass(WndClass);
  if WindowDisablerWndClassAtom <> 0 then begin
    { Create an invisible owner window for the fallback window so that it
      doesn't display a taskbar button. (We can't just give it the
      WS_EX_TOOLWINDOW style because Windows skips tool windows when searching
      for a new window to activate.) }
    FOwnerWnd := CreateWindowEx(0, WindowDisablerWndClassName, '',
      WS_POPUP or WS_DISABLED, 0, 0, 0, 0, HWND_DESKTOP, 0, HInstance, nil);
    if FOwnerWnd <> 0 then begin
      { Note: We give the window a valid title since the user can see it in the
        Alt+Tab list (on 2000 and XP, but not Vista, which appears to exclude
        zero-width/height windows). }
      FFallbackWnd := CreateWindowEx(0, WindowDisablerWndClassName,
        PChar(Application.Title), WS_POPUP, 0, 0, 0, 0, FOwnerWnd, 0,
        HInstance, nil);
      if FFallbackWnd <> 0 then
        ShowWindow(FFallbackWnd, SW_SHOWNA);
    end;
  end;

  { Take the focus away from whatever has it. While you can't click controls
    inside a disabled window, keystrokes will still reach the focused control
    (e.g. you can press Space to re-click a focused button). }
  SetFocus(0);
end;

destructor TWindowDisabler.Destroy;
begin
  EnableTaskWindows(FWindowList);
  { Re-activate the previous window. But don't do this if GetActiveWindow
    returns zero, because that means another application is in the foreground
    (possibly a child process spawned by us that is still running). }
  if GetActiveWindow <> 0 then begin
    if FPreviousActiveWnd <> 0 then
      SetActiveWindow(FPreviousActiveWnd);
    { If the active window never changed, then the above SetActiveWindow call
      won't have an effect. Explicitly restore the focus. }
    if FPreviousFocusWnd <> 0 then
      SetFocus(FPreviousFocusWnd);
  end;
  if FOwnerWnd <> 0 then
    DestroyWindow(FOwnerWnd);  { will destroy FFallbackWnd too }
  inherited;
end;

initialization
finalization
  FreeCaptions;
end.
