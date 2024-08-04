unit Shared.CmnFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Common VCL functions
}

{$B-}

interface

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

  { Note: This type is also present in Compiler.ScriptFunc.pas }
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
function GetMessageBoxCaption(const Caption: PChar; const Typ: TMsgBoxType): PChar;
procedure SetMessageBoxRightToLeft(const ARightToLeft: Boolean);
function GetMessageBoxRightToLeft: Boolean;
procedure SetMessageBoxCallbackFunc(const AFunc: TMsgBoxCallbackFunc; const AParam: LongInt);
procedure TriggerMessageBoxCallbackFunc(const Flags: LongInt; const After: Boolean);

implementation

uses
  Consts, PathFunc, Shared.CmnFunc2;

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

function GetMessageBoxCaption(const Caption: PChar; const Typ: TMsgBoxType): PChar;
const
 DefaultCaptions: array[TMsgBoxType] of PChar =
   ('Information', 'Confirm', 'Error', 'Error');
begin
  Result := Caption;
  if (Result = nil) or (Result[0] = #0) then begin
    Result := MessageBoxCaptions[Typ];
    if Result = nil then
      Result := DefaultCaptions[Typ];
  end;
end;

procedure SetMessageBoxRightToLeft(const ARightToLeft: Boolean);
begin
  MessageBoxRightToLeft := ARightToLeft;
end;

function GetMessageBoxRightToLeft: Boolean;
begin
  Result := MessageBoxRightToLeft;
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

function AppMessageBox(const Text, Caption: PChar; Flags: Longint): Integer;
var
  ActiveWindow: HWND;
  WindowList: Pointer;
begin
  if MessageBoxRightToLeft then
    Flags := Flags or (MB_RTLREADING or MB_RIGHT);

  TriggerMessageBoxCallbackFunc(Flags, False);
  try
    { If the application window isn't currently visible, show the message box
      with no owner window so it'll get a taskbar button } 
    if IsIconic(Application.Handle) or
       (GetWindowLong(Application.Handle, GWL_STYLE) and WS_VISIBLE = 0) or
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

    Result := Application.MessageBox(Text, Caption, Flags);
  finally
    TriggerMessageBoxCallbackFunc(Flags, True);
  end;
end;

function MsgBoxP(const Text, Caption: PChar; const Typ: TMsgBoxType;
  const Buttons: Cardinal): Integer;
const
  IconFlags: array[TMsgBoxType] of Cardinal =
    (MB_ICONINFORMATION, MB_ICONQUESTION, MB_ICONEXCLAMATION, MB_ICONSTOP);
begin
  Result := AppMessageBox(Text, GetMessageBoxCaption(Caption, Typ), Buttons or IconFlags[Typ]);
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
