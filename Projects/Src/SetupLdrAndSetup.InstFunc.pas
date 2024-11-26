unit SetupLdrAndSetup.InstFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Misc. installation functions. Used only by the Setup and SetupLdr projects.
}

interface

uses
  Windows, SysUtils, Shared.Struct, Shared.CommonFunc;

type
  TDetermineDefaultLanguageResult = (ddNoMatch, ddMatch, ddMatchLangParameter);
  TGetLanguageEntryProc = function(Index: Integer; var Entry: PSetupLanguageEntry): Boolean;

function CreateTempDir(const LimitCurrentUserSidAccess: Boolean;
  var Protected: Boolean): String; overload;
function CreateTempDir(const LimitCurrentUserSidAccess: Boolean): String; overload;
procedure DelayDeleteFile(const DisableFsRedir: Boolean; const Filename: String;
  const MaxTries, FirstRetryDelayMS, SubsequentRetryDelayMS: Integer);
function DetermineDefaultLanguage(const GetLanguageEntryProc: TGetLanguageEntryProc;
  const Method: TSetupLanguageDetectionMethod; const LangParameter: String;
  var ResultIndex: Integer): TDetermineDefaultLanguageResult;
function RestartComputer: Boolean;

{ The following are not called by other SetupLdr units: they are only called by the
  code below and by other Setup units }
function CreateSafeDirectory(const LimitCurrentUserSidAccess: Boolean; Path: String;
  var ErrorCode: DWORD; out Protected: Boolean): Boolean; overload;
function CreateSafeDirectory(const LimitCurrentUserSidAccess: Boolean; Path: String;
  var ErrorCode: DWORD): Boolean; overload;
function IntToBase32(Number: Longint): String;
function GenerateUniqueName(const DisableFsRedir: Boolean; Path: String;
  const Extension: String): String;

implementation

uses
  PathFunc, SetupLdrAndSetup.Messages, Shared.SetupMessageIDs, SetupLdrAndSetup.RedirFunc;

function ConvertStringSecurityDescriptorToSecurityDescriptorW(
  StringSecurityDescriptor: PWideChar;
  StringSDRevision: DWORD; var ppSecurityDescriptor: Pointer;
  dummy: Pointer): BOOL; stdcall; external advapi32;

function CreateSafeDirectory(const LimitCurrentUserSidAccess: Boolean; Path: String;
  var ErrorCode: DWORD; out Protected: Boolean): Boolean; overload;
{ Creates a protected directory if
  -permissions are supported
  -it's a subdirectory of c:\WINDOWS\TEMP, or
  -it's on a local drive and LimitCurrentUserSidAccess is True (latter is true atm if elevated and not debugging)
  otherwise creates a normal directory. }
const
  SDDL_REVISION_1 = 1;
begin
  Path := PathExpand(Path);
  var Drive := PathExtractDrive(Path);
  var FileSystemFlags: DWORD;

  if GetVolumeInformation(PChar(AddBackslash(Drive)), nil, 0, nil, DWORD(nil^), FileSystemFlags, nil, 0) and
     ((FileSystemFlags and FILE_PERSISTENT_ACLS) <> 0) then begin
    var IsUnderWindowsTemp := Pos(PathLowercase(AddBackslash(GetSystemWinDir) + 'TEMP\'),
      PathLowercase(Path)) = 1;
    var IsLocalTempToProtect := LimitCurrentUserSidAccess and (Drive <> '') and
      not PathCharIsSlash(Drive[1]) and
      (GetDriveType(PChar(AddBackslash(Drive))) <> DRIVE_REMOTE);
    Protected := IsUnderWindowsTemp or IsLocalTempToProtect;
  end else
    Protected := False;

  if Protected then begin
    var StringSecurityDescriptor :=
      // D: adds a Discretionary ACL ("DACL", i.e. access control via SIDs)
      // P: prevents DACL from being modified by inheritable ACEs
      // AI: says automatic propagation of inheritable ACEs to child objects
      //     is supported; always supposed to be set on Windows 2000+ ACLs
      'D:PAI';
    var CurrentUserSid := GetCurrentUserSid;
    if CurrentUserSid = '' then
      CurrentUserSid := 'OW'; // OW: owner rights
    { Omit the CurrentUserSid ACE if the current user is SYSTEM, because
      there's already a fixed Full Control ACE for SYSTEM below }
    if not SameText(CurrentUserSid, 'S-1-5-18') then begin
      // A: "allow"
      // OICI: "object and container inherit",
      //    i.e. files and directories created within the new directory
      //    inherit these permissions
      var AccessRights := 'FA'; // FILE_ALL_ACCESS (Full Control)
      if LimitCurrentUserSidAccess then
        AccessRights := 'FRFX'; // FILE_GENERIC_READ | FILE_GENERIC_EXECUTE
      StringSecurityDescriptor := StringSecurityDescriptor +
        '(A;OICI;' + AccessRights + ';;;' + CurrentUserSid + ')'; // current user
    end;
    StringSecurityDescriptor := StringSecurityDescriptor +
      '(A;OICI;FA;;;BA)' + // BA: built-in Administrators group
      '(A;OICI;FA;;;SY)'; // SY: local SYSTEM account

    var pSecurityDescriptor: Pointer;
    if not ConvertStringSecurityDescriptorToSecurityDescriptorW(
      PWideChar(StringSecurityDescriptor), SDDL_REVISION_1, pSecurityDescriptor, nil
    ) then begin
      ErrorCode := GetLastError;
      Result := False;
      Exit;
    end;

    var SecurityAttr: TSecurityAttributes;
    SecurityAttr.nLength := SizeOf(SecurityAttr);
    SecurityAttr.bInheritHandle := False;
    SecurityAttr.lpSecurityDescriptor := pSecurityDescriptor;

    Result := CreateDirectory(PChar(Path), @SecurityAttr);
    if not Result then
      ErrorCode := GetLastError;

    LocalFree(pSecurityDescriptor);
  end else begin
    Result := CreateDirectory(PChar(Path), nil);
    if not Result then
      ErrorCode := GetLastError;
  end;
end;

function CreateSafeDirectory(const LimitCurrentUserSidAccess: Boolean; Path: String;
  var ErrorCode: DWORD): Boolean; overload;
begin
  var Protected: Boolean;
  Result := CreateSafeDirectory(LimitCurrentUserSidAccess, Path, ErrorCode, Protected);
end;

function IntToBase32(Number: Longint): String;
const
  Table: array[0..31] of Char = '0123456789ABCDEFGHIJKLMNOPQRSTUV';
var
  I: Integer;
begin
  Result := '';
  for I := 0 to 4 do begin
    Insert(Table[Number and 31], Result, 1);
    Number := Number shr 5;
  end;
end;

function GenerateUniqueName(const DisableFsRedir: Boolean; Path: String;
  const Extension: String): String;
var
  Rand, RandOrig: Longint;
  Filename: String;
begin
  Path := AddBackslash(Path);
  RandOrig := Random($2000000);
  Rand := RandOrig;
  repeat
    Inc(Rand);
    if Rand > $1FFFFFF then Rand := 0;
    if Rand = RandOrig then
      { practically impossible to go through 33 million possibilities,
        but check "just in case"... }
      raise Exception.Create(FmtSetupMessage1(msgErrorTooManyFilesInDir,
        RemoveBackslashUnlessRoot(Path)));
    { Generate a random name }
    Filename := Path + 'is-' + IntToBase32(Rand) + Extension;
  until not FileOrDirExistsRedir(DisableFsRedir, Filename);
  Result := Filename;
end;

function CreateTempDir(const LimitCurrentUserSidAccess: Boolean;
  var Protected: Boolean): String;
{ This is called by SetupLdr, Setup, and Uninstall. }
var
  Dir: String;
  ErrorCode: DWORD;
begin
  while True do begin
    Dir := GenerateUniqueName(False, GetTempDir, '.tmp');
    if CreateSafeDirectory(LimitCurrentUserSidAccess, Dir, ErrorCode, Protected) then
      Break;
    if ErrorCode <> ERROR_ALREADY_EXISTS then
      raise Exception.Create(FmtSetupMessage(msgLastErrorMessage,
        [FmtSetupMessage1(msgErrorCreatingDir, Dir), IntToStr(ErrorCode),
         Win32ErrorString(ErrorCode)]));
  end;
  Result := Dir;
end;

function CreateTempDir(const LimitCurrentUserSidAccess: Boolean): String;
begin
  var Protected: Boolean;
  Result := CreateTempDir(LimitCurrentUserSidAccess, Protected);
end;

{ Work around problem in D2's declaration of the function }
function NewAdjustTokenPrivileges(TokenHandle: THandle; DisableAllPrivileges: BOOL;
  const NewState: TTokenPrivileges; BufferLength: DWORD;
  PreviousState: PTokenPrivileges; ReturnLength: PDWORD): BOOL; stdcall;
  external advapi32 name 'AdjustTokenPrivileges';

function RestartComputer: Boolean;
{ Restarts the computer. }
var
  Token: THandle;
  TokenPriv: TTokenPrivileges;
const
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';  { don't localize }
begin
  if not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
     Token) then begin
    Result := False;
    Exit;
  end;

  LookupPrivilegeValue(nil, SE_SHUTDOWN_NAME, TokenPriv.Privileges[0].Luid);

  TokenPriv.PrivilegeCount := 1;
  TokenPriv.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

  NewAdjustTokenPrivileges(Token, False, TokenPriv, 0, nil, nil);

  { Cannot test the return value of AdjustTokenPrivileges. }
  if GetLastError <> ERROR_SUCCESS then begin
    Result := False;
    Exit;
  end;

  Result := ExitWindowsEx(EWX_REBOOT, 0);

  { ExitWindowsEx returns True immediately. The system then asynchronously
    sends WM_QUERYENDSESSION messages to all processes, including the current
    process. The current process is not killed until it has received
    WM_QUERYENDSESSION and WM_ENDSESSION messages. }
end;

procedure DelayDeleteFile(const DisableFsRedir: Boolean; const Filename: String;
  const MaxTries, FirstRetryDelayMS, SubsequentRetryDelayMS: Integer);
{ Attempts to delete Filename up to MaxTries times, retrying if the file is
  in use. It sleeps FirstRetryDelayMS msec after the first try, and
  SubsequentRetryDelayMS msec after subsequent tries. }
var
  I: Integer;
begin
  for I := 0 to MaxTries-1 do begin
    if I = 1 then
      Sleep(FirstRetryDelayMS)
    else if I > 1 then
      Sleep(SubsequentRetryDelayMS);
    if DeleteFileRedir(DisableFsRedir, Filename) or
       (GetLastError = ERROR_FILE_NOT_FOUND) or
       (GetLastError = ERROR_PATH_NOT_FOUND) then
      Break;
  end;
end;

function DetermineDefaultLanguage(const GetLanguageEntryProc: TGetLanguageEntryProc;
  const Method: TSetupLanguageDetectionMethod; const LangParameter: String;
  var ResultIndex: Integer): TDetermineDefaultLanguageResult;
{ Finds the index of the language entry that most closely matches the user's
  UI language / locale. If no match is found, ResultIndex is set to 0. }

  function GetCodePageFromLangID(const ALangID: LANGID): Integer;
  const
    LOCALE_RETURN_NUMBER = $20000000;
  var
    CodePage: DWORD;
  begin
    if GetLocaleInfo(ALangID, LOCALE_IDEFAULTANSICODEPAGE or LOCALE_RETURN_NUMBER,
       PChar(@CodePage), SizeOf(CodePage) div SizeOf(Char)) > 0 then
      Result := Integer(CodePage)
    else
      Result := -1;
  end;

var
  I: Integer;
  LangEntry: PSetupLanguageEntry;
  UILang: LANGID;
begin
  ResultIndex := 0;
  Result := ddNoMatch;

  if LangParameter <> '' then begin
    { Use the language specified on the command line, if available }
    I := 0;
    while GetLanguageEntryProc(I, LangEntry) do begin
      if CompareText(LangParameter, LangEntry.Name) = 0 then begin
        ResultIndex := I;
        Result := ddMatchLangParameter;
        Exit;
      end;
      Inc(I);
    end;
  end;

  case Method of
    ldUILanguage: UILang := GetUILanguage;
    ldLocale: UILang := GetUserDefaultLangID;
  else
    { ldNone }
    UILang := 0;
  end;
  if UILang <> 0 then begin
    { Look for a primary + sub language ID match }
    I := 0;
    while GetLanguageEntryProc(I, LangEntry) do begin
      if LangEntry.LanguageID = UILang then begin
        ResultIndex := I;
        Result := ddMatch;
        Exit;
      end;
      Inc(I);
    end;
    { Look for just a primary language ID match }
    I := 0;
    while GetLanguageEntryProc(I, LangEntry) do begin
      if (LangEntry.LanguageID and $3FF) = (UILang and $3FF) then begin
        { On Unicode, there is no LanguageCodePage filter, so we have to check
          the language IDs to ensure we don't return Simplified Chinese on a
          Traditional Chinese system, or vice versa.
          If the default ANSI code pages associated with the language IDs are
          equal, then there is no Simplified/Traditional discrepancy.
           Simplified Chinese LANGIDs ($0804, $1004)        use CP 936
          Traditional Chinese LANGIDs ($0404, $0C04, $1404) use CP 950 }
        if ((UILang and $3FF) <> LANG_CHINESE) or
           (GetCodePageFromLangID(LangEntry.LanguageID) = GetCodePageFromLangID(UILang)) then
        begin
          ResultIndex := I;
          Result := ddMatch;
          Exit;
        end;
      end;
      Inc(I);
    end;
  end;
end;

end.
