unit SetupLdrAndSetup.RedirFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Functions for dealing with WOW64 file system redirection.
  Used only by the Setup and SetupLdr projects.

  The *Redir functions are counterparts to common functions that offer
  built-in support for disabling FS redirection.
}

interface

uses
  Windows, SysUtils, Shared.FileClass, Shared.VerInfoFunc;

type
  TPreviousFsRedirectionState = record
    DidDisable: Boolean;
    OldValue: Pointer;
  end;

function AreFsRedirectionFunctionsAvailable: Boolean;
function DisableFsRedirectionIf(const Disable: Boolean;
  var PreviousState: TPreviousFsRedirectionState): Boolean;
procedure RestoreFsRedirection(const PreviousState: TPreviousFsRedirectionState);

function CreateDirectoryRedir(const DisableFsRedir: Boolean; const Filename: String): BOOL;
function CreateProcessRedir(const DisableFsRedir: Boolean;
  const lpApplicationName: PChar; const lpCommandLine: PChar;
  const lpProcessAttributes, lpThreadAttributes: PSecurityAttributes;
  const bInheritHandles: BOOL; const dwCreationFlags: DWORD;
  const lpEnvironment: Pointer; const lpCurrentDirectory: PChar;
  const lpStartupInfo: TStartupInfo;
  var lpProcessInformation: TProcessInformation): BOOL;
function CopyFileRedir(const DisableFsRedir: Boolean;
  const ExistingFilename, NewFilename: String; const FailIfExists: BOOL): BOOL;
function DeleteFileRedir(const DisableFsRedir: Boolean; const Filename: String): BOOL;
function DirExistsRedir(const DisableFsRedir: Boolean; const Filename: String): Boolean;
function FileOrDirExistsRedir(const DisableFsRedir: Boolean; const Filename: String): Boolean;
function FindFirstFileRedir(const DisableFsRedir: Boolean; const Filename: String;
  var FindData: TWin32FindData): THandle;
function GetFileAttributesRedir(const DisableFsRedir: Boolean; const Filename: String): DWORD;
function GetShortNameRedir(const DisableFsRedir: Boolean; const Filename: String): String;
function GetVersionNumbersRedir(const DisableFsRedir: Boolean; const Filename: String;
  var VersionNumbers: TFileVersionNumbers): Boolean;
function IsDirectoryAndNotReparsePointRedir(const DisableFsRedir: Boolean;
  const Name: String): Boolean;
function MoveFileRedir(const DisableFsRedir: Boolean;
  const ExistingFilename, NewFilename: String): BOOL;
function MoveFileExRedir(const DisableFsRedir: Boolean;
  const ExistingFilename, NewFilename: String; const Flags: DWORD): BOOL;
function NewFileExistsRedir(const DisableFsRedir: Boolean; const Filename: String): Boolean;
function RemoveDirectoryRedir(const DisableFsRedir: Boolean; const Filename: String): BOOL;
function SetFileAttributesRedir(const DisableFsRedir: Boolean; const Filename: String;
  const Attrib: DWORD): BOOL;
function SetNTFSCompressionRedir(const DisableFsRedir: Boolean; const FileOrDir: String; Compress: Boolean): Boolean;

type
  TFileRedir = class(TFile)
  private
    FDisableFsRedir: Boolean;
  protected
    function CreateHandle(const AFilename: String;
      ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
      ASharing: TFileSharing): THandle; override;
  public
    constructor Create(const DisableFsRedir: Boolean; const AFilename: String;
      ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
      ASharing: TFileSharing);
  end;

  TTextFileReaderRedir = class(TTextFileReader)
  private
    FDisableFsRedir: Boolean;
  protected
    function CreateHandle(const AFilename: String;
      ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
      ASharing: TFileSharing): THandle; override;
  public
    constructor Create(const DisableFsRedir: Boolean; const AFilename: String;
      ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
      ASharing: TFileSharing);
  end;

  TTextFileWriterRedir = class(TTextFileWriter)
  private
    FDisableFsRedir: Boolean;
  protected
    function CreateHandle(const AFilename: String;
      ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
      ASharing: TFileSharing): THandle; override;
  public
    constructor Create(const DisableFsRedir: Boolean; const AFilename: String;
      ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
      ASharing: TFileSharing);
  end;

implementation

uses
  Shared.CommonFunc, PathFunc;

var
  Wow64DisableWow64FsRedirectionFunc: function(var OldValue: Pointer): BOOL; stdcall;
  Wow64RevertWow64FsRedirectionFunc: function(OldValue: Pointer): BOOL; stdcall;
  FsRedirectionFunctionsAvailable: Boolean;

function AreFsRedirectionFunctionsAvailable: Boolean;
begin
  Result := FsRedirectionFunctionsAvailable;
end;

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
  PreviousState.DidDisable := False;
  if not Disable then
    Result := True
  else begin
    if FsRedirectionFunctionsAvailable then begin
      { Note: Disassembling Wow64DisableWow64FsRedirection and the Rtl function
        it calls, it doesn't appear as if it can ever actually fail on 64-bit
        Windows. But it always fails on the 32-bit version of Windows Server
        2003 SP1 (with error code 1 - ERROR_INVALID_FUNCTION). }
      Result := Wow64DisableWow64FsRedirectionFunc(PreviousState.OldValue);
      if Result then
        PreviousState.DidDisable := True;
    end
    else begin
      { Should never happen }
      SetLastError(ERROR_INVALID_FUNCTION);
      Result := False;
    end;
  end;
end;

procedure RestoreFsRedirection(const PreviousState: TPreviousFsRedirectionState);
{ Restores the previous WOW64 file system redirection state after a call to
  DisableFsRedirectionIf. There is no indication of failure (which is
  extremely unlikely). }
begin
  if PreviousState.DidDisable then
    Wow64RevertWow64FsRedirectionFunc(PreviousState.OldValue);
end;

{ *Redir functions }

function CreateDirectoryRedir(const DisableFsRedir: Boolean; const Filename: String): BOOL;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := CreateDirectory(PChar(Filename), nil);
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

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

function CopyFileRedir(const DisableFsRedir: Boolean;
  const ExistingFilename, NewFilename: String; const FailIfExists: BOOL): BOOL;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := CopyFile(PChar(ExistingFilename), PChar(NewFilename), FailIfExists);
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

function DeleteFileRedir(const DisableFsRedir: Boolean; const Filename: String): BOOL;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := Windows.DeleteFile(PChar(Filename));
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

function DirExistsRedir(const DisableFsRedir: Boolean; const Filename: String): Boolean;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := DirExists(Filename);
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

function FileOrDirExistsRedir(const DisableFsRedir: Boolean; const Filename: String): Boolean;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := FileOrDirExists(Filename);
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

function FindFirstFileRedir(const DisableFsRedir: Boolean; const Filename: String;
  var FindData: TWin32FindData): THandle;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := INVALID_HANDLE_VALUE;
    Exit;
  end;
  try
    Result := FindFirstFile(PChar(Filename), FindData);
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

function GetFileAttributesRedir(const DisableFsRedir: Boolean; const Filename: String): DWORD;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := $FFFFFFFF;
    Exit;
  end;
  try
    Result := GetFileAttributes(PChar(Filename));
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

function GetShortNameRedir(const DisableFsRedir: Boolean; const Filename: String): String;
var
  PrevState: TPreviousFsRedirectionState;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := Filename;
    Exit;
  end;
  try
    Result := GetShortName(Filename);
  finally
    RestoreFsRedirection(PrevState);
  end;
end;

function GetVersionNumbersRedir(const DisableFsRedir: Boolean; const Filename: String;
  var VersionNumbers: TFileVersionNumbers): Boolean;
var
  PrevState: TPreviousFsRedirectionState;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := GetVersionNumbers(Filename, VersionNumbers);
  finally
    RestoreFsRedirection(PrevState);
  end;
end;

function IsDirectoryAndNotReparsePointRedir(const DisableFsRedir: Boolean;
  const Name: String): Boolean;
var
  PrevState: TPreviousFsRedirectionState;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := IsDirectoryAndNotReparsePoint(Name);
  finally
    RestoreFsRedirection(PrevState);
  end;
end;

function MoveFileRedir(const DisableFsRedir: Boolean;
  const ExistingFilename, NewFilename: String): BOOL;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := MoveFile(PChar(ExistingFilename), PChar(NewFilename));
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

function MoveFileExRedir(const DisableFsRedir: Boolean;
  const ExistingFilename, NewFilename: String; const Flags: DWORD): BOOL;
var
  NewFilenameP: PChar;
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if (NewFilename = '') and (Flags and MOVEFILE_DELAY_UNTIL_REBOOT <> 0) then
    NewFilenameP := nil
  else
    NewFilenameP := PChar(NewFilename);
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := MoveFileEx(PChar(ExistingFilename), NewFilenameP, Flags);
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

function NewFileExistsRedir(const DisableFsRedir: Boolean; const Filename: String): Boolean;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := NewFileExists(Filename);
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

function RemoveDirectoryRedir(const DisableFsRedir: Boolean; const Filename: String): BOOL;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := RemoveDirectory(PChar(Filename));
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

function SetFileAttributesRedir(const DisableFsRedir: Boolean; const Filename: String;
  const Attrib: DWORD): BOOL;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := SetFileAttributes(PChar(Filename), Attrib);
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

function SetNTFSCompressionRedir(const DisableFsRedir: Boolean; const FileOrDir: String; Compress: Boolean): Boolean;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := SetNTFSCompression(FileOrDir, Compress);
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

{ TFileRedir }

constructor TFileRedir.Create(const DisableFsRedir: Boolean; const AFilename: String;
  ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
  ASharing: TFileSharing);
begin
  FDisableFsRedir := DisableFsRedir; 
  inherited Create(AFilename, ACreateDisposition, AAccess, ASharing);
end;

function TFileRedir.CreateHandle(const AFilename: String;
  ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
  ASharing: TFileSharing): THandle;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(FDisableFsRedir, PrevState) then begin
    Result := INVALID_HANDLE_VALUE;
    Exit;
  end;
  try
    Result := inherited CreateHandle(AFilename, ACreateDisposition, AAccess,
      ASharing);
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

{ TTextFileReaderRedir }

constructor TTextFileReaderRedir.Create(const DisableFsRedir: Boolean; const AFilename: String;
  ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
  ASharing: TFileSharing);
begin
  FDisableFsRedir := DisableFsRedir; 
  inherited Create(AFilename, ACreateDisposition, AAccess, ASharing);
end;

function TTextFileReaderRedir.CreateHandle(const AFilename: String;
  ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
  ASharing: TFileSharing): THandle;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(FDisableFsRedir, PrevState) then begin
    Result := INVALID_HANDLE_VALUE;
    Exit;
  end;
  try
    Result := inherited CreateHandle(AFilename, ACreateDisposition, AAccess,
      ASharing);
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

{ TTextFileWriterRedir }

constructor TTextFileWriterRedir.Create(const DisableFsRedir: Boolean; const AFilename: String;
  ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
  ASharing: TFileSharing);
begin
  FDisableFsRedir := DisableFsRedir; 
  inherited Create(AFilename, ACreateDisposition, AAccess, ASharing);
end;

function TTextFileWriterRedir.CreateHandle(const AFilename: String;
  ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
  ASharing: TFileSharing): THandle;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(FDisableFsRedir, PrevState) then begin
    Result := INVALID_HANDLE_VALUE;
    Exit;
  end;
  try
    Result := inherited CreateHandle(AFilename, ACreateDisposition, AAccess,
      ASharing);
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

initialization
  Wow64DisableWow64FsRedirectionFunc := GetProcAddress(GetModuleHandle(kernel32),
    'Wow64DisableWow64FsRedirection');
  Wow64RevertWow64FsRedirectionFunc := GetProcAddress(GetModuleHandle(kernel32),
    'Wow64RevertWow64FsRedirection');
  FsRedirectionFunctionsAvailable := Assigned(Wow64DisableWow64FsRedirectionFunc) and
    Assigned(Wow64RevertWow64FsRedirectionFunc);

  { For GetVersionNumbersRedir: Pre-load shell32.dll since GetFileVersionInfo
    and GetFileVersionInfoSize will try to load it when reading version info
    on 16-bit files. We can't allow the DLL be loaded for the first time while
    FS redirection is disabled. }
  SafeLoadLibrary(AddBackslash(GetSystemDir) + 'shell32.dll', SEM_NOOPENFILEERRORBOX);

  { FormatMessage might be called with FS redirection disabled, so ensure
    that all the DLLs FormatMessage searches in for messages (e.g. netmsg.dll,
    ws03res.dll) are pre-loaded by calling it now with a randomly-chosen
    message ID -- one that won't result in a match and cause the function to
    return early.
    (Note: Presently, FormatMessage loads the DLLs as "data files" so it
    actually may not matter whether it gets 32- or 64-bit versions. But let's
    be on the safe side.) }
  Win32ErrorString($4C783AFB);
end.
