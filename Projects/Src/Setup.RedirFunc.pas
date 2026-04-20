unit Setup.RedirFunc;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Functions for dealing with WOW64 file system redirection.
  Used only by the Setup project.

  The *Redir functions are counterparts to common functions that offer
  built-in support for disabling FS redirection.
}

interface

uses
  Windows;

type
  TPreviousFsRedirectionState = record
    {$IFNDEF WIN64}
    DidDisable: Boolean;
    OldValue: PVOID;
    {$ENDIF}
  end;

function DisableFsRedirectionIf(const Disable: Boolean;
  var PreviousState: TPreviousFsRedirectionState): Boolean;
procedure RestoreFsRedirection(const PreviousState: TPreviousFsRedirectionState);

function CreateProcessRedir(const DisableFsRedir: Boolean;
  const lpApplicationName: PChar; const lpCommandLine: PChar;
  const lpProcessAttributes, lpThreadAttributes: PSecurityAttributes;
  const bInheritHandles: BOOL; const dwCreationFlags: DWORD;
  const lpEnvironment: Pointer; const lpCurrentDirectory: PChar;
  const lpStartupInfo: TStartupInfo;
  var lpProcessInformation: TProcessInformation): BOOL;

implementation

uses
  Shared.CommonFunc;

{$IFNDEF WIN64}
function Wow64DisableWow64FsRedirection_static(var OldValue: PVOID): BOOL; stdcall;
  external kernel32 name 'Wow64DisableWow64FsRedirection';
function Wow64RevertWow64FsRedirection_static(OldValue: PVOID): BOOL; stdcall;
  external kernel32 name 'Wow64RevertWow64FsRedirection';
{$ENDIF}

function DisableFsRedirectionIf(const Disable: Boolean;
  var PreviousState: TPreviousFsRedirectionState): Boolean;
{ If Disable is False, the function does not change the redirection state and
  always returns True.
  If Disable is True, the function attempts to disable WOW64 file system
  redirection, so that c:\windows\system32 goes to the 64-bit System directory
  instead of the 32-bit one.
  Returns True if successful, False if not. For extended error information when
  False is returned, call GetLastError. }
begin
  {$IFNDEF WIN64}
  PreviousState.DidDisable := False;
  if not Disable then
    Result := True
  else begin
    { Note: Disassembling Wow64DisableWow64FsRedirection and the Rtl function
      it calls, it doesn't appear as if it can ever actually fail on 64-bit
      Windows. But it always fails on the 32-bit version of Windows Server
      2003 SP1 (with error code 1 - ERROR_INVALID_FUNCTION). }
    Result := Wow64DisableWow64FsRedirection_static(PreviousState.OldValue);
    if Result then
      PreviousState.DidDisable := True;
  end;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

procedure RestoreFsRedirection(const PreviousState: TPreviousFsRedirectionState);
{ Restores the previous WOW64 file system redirection state after a call to
  DisableFsRedirectionIf. There is no indication of failure (which is
  extremely unlikely). }
begin
  {$IFNDEF WIN64}
  if PreviousState.DidDisable then
    Wow64RevertWow64FsRedirection_static(PreviousState.OldValue);
  {$ENDIF}
end;

{ *Redir functions }

function CreateProcessRedir(const DisableFsRedir: Boolean;
  const lpApplicationName: PChar; const lpCommandLine: PChar;
  const lpProcessAttributes, lpThreadAttributes: PSecurityAttributes;
  const bInheritHandles: BOOL; const dwCreationFlags: DWORD;
  const lpEnvironment: Pointer; const lpCurrentDirectory: PChar;
  const lpStartupInfo: TStartupInfo;
  var lpProcessInformation: TProcessInformation): BOOL;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := CreateProcess(lpApplicationName, lpCommandLine,
      lpProcessAttributes, lpThreadAttributes,
      bInheritHandles, dwCreationFlags, lpEnvironment,
      lpCurrentDirectory, lpStartupInfo, lpProcessInformation);
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

{$IFNDEF WIN64}
initialization
  { FormatMessage might be called with FS redirection disabled, so ensure
    that all the DLLs FormatMessage searches in for messages (e.g. netmsg.dll,
    ws03res.dll) are pre-loaded by calling it now with a randomly-chosen
    message ID -- one that won't result in a match and cause the function to
    return early.
    (Note: Presently, FormatMessage loads the DLLs as "data files" so it
    actually may not matter whether it gets 32- or 64-bit versions. But let's
    be on the safe side.) }
  Win32ErrorString($4C783AFB);
{$ENDIF}
end.
