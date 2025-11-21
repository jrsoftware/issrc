unit SetupLdrAndSetup.InstFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
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

function CreateTempDir(const Extension: String;
  const LimitCurrentUserSidAccess: Boolean; var Protected: Boolean): String; overload;
function CreateTempDir(const Extension: String;
  const LimitCurrentUserSidAccess: Boolean): String; overload;
procedure DelayDeleteFile({$IFDEF SETUPPROJ}const DisableFsRedir: Boolean;{$ENDIF} const Filename: String;
  const MaxTries, FirstRetryDelayMS, SubsequentRetryDelayMS: Cardinal);
function DetermineDefaultLanguage(const GetLanguageEntryProc: TGetLanguageEntryProc;
  const Method: TSetupLanguageDetectionMethod; const LangParameter: String;
  var ResultIndex: Integer): TDetermineDefaultLanguageResult;
function GetFinalCurrentDir: String;
function GetFinalFileName(const Filename: String): String;
procedure RaiseFunctionFailedError(const FunctionName: String);
function RestartComputer: Boolean;
procedure SplitNewParamStr(const Index: Integer; var AName, AValue: String);

{$IFDEF SETUPPROJ}
{ The following are not called by other SetupLdr units: they are only called by the
  code below and by other Setup units - so the implementations exist below but they
  are not included here in the interface, for clarity }
function CreateSafeDirectory(const LimitCurrentUserSidAccess: Boolean; Path: String;
  var ErrorCode: DWORD; out Protected: Boolean): Boolean; overload;
function CreateSafeDirectory(const LimitCurrentUserSidAccess: Boolean; Path: String;
  var ErrorCode: DWORD): Boolean; overload;
function UIntToBase36Str(AValue: UInt32; const ADigits: Integer): String;
function GenerateUniqueName({$IFDEF SETUPPROJ}const DisableFsRedir: Boolean;{$ENDIF} Path: String;
  const Extension: String): String;
{$ENDIF}

implementation

uses
  PathFunc, SetupLdrAndSetup.Messages, Shared.SetupMessageIDs{$IFDEF SETUPPROJ}, Setup.RedirFunc{$ENDIF};

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

function UIntToBase36Str(AValue: UInt32; const ADigits: Integer): String;
begin
  Result := StringOfChar('0', ADigits);
  for var I := High(Result) downto Low(Result) do begin
    var Digit := AValue mod 36;
    if Digit < 10 then
      Inc(Digit, Ord('0'))
    else
      Inc(Digit, Ord('A') - 10);
    Result[I] := Chr(Digit);
    AValue := AValue div 36;
  end;
end;

function GenerateUniqueName({$IFDEF SETUPPROJ}const DisableFsRedir: Boolean;{$ENDIF} Path: String;
  const Extension: String): String;
const
  FiveDigitsRange = 36 * 36 * 36 * 36 * 36;
begin
  Path := AddBackslash(Path);
  var Filename: String;
  var AttemptNumber := 0;
  repeat
    { If 50 attempts were made and every generated name was found to exist
      already, then stop trying, because something really strange is going
      on -- like the file system is claiming everything exists regardless of
      name. }
    Inc(AttemptNumber);
    if AttemptNumber > 50 then
      raise Exception.Create(FmtSetupMessage1(msgErrorTooManyFilesInDir,
        RemoveBackslashUnlessRoot(Path)));

    Filename := Path + 'is-' +
      UIntToBase36Str(TStrongRandom.GenerateUInt32Range(FiveDigitsRange), 5) +
      UIntToBase36Str(TStrongRandom.GenerateUInt32Range(FiveDigitsRange), 5) +
      Extension;
  until not {$IFDEF SETUPPROJ}FileOrDirExistsRedir(DisableFsRedir, Filename){$ELSE}FileOrDirExists(Filename){$ENDIF};
  Result := Filename;
end;

function CreateTempDir(const Extension: String;
  const LimitCurrentUserSidAccess: Boolean; var Protected: Boolean): String;
{ This is called by SetupLdr, Setup, and Uninstall. }
var
  Dir: String;
  ErrorCode: DWORD;
begin
  while True do begin
    Dir := GenerateUniqueName({$IFDEF SETUPPROJ}False,{$ENDIF} GetTempDir, Extension);
    if CreateSafeDirectory(LimitCurrentUserSidAccess, Dir, ErrorCode, Protected) then
      Break;
    if ErrorCode <> ERROR_ALREADY_EXISTS then
      raise Exception.Create(FmtSetupMessage(msgLastErrorMessage,
        [FmtSetupMessage1(msgErrorCreatingDir, Dir), IntToStr(ErrorCode),
         Win32ErrorString(ErrorCode)]));
  end;
  Result := Dir;
end;

function CreateTempDir(const Extension: String;
  const LimitCurrentUserSidAccess: Boolean): String;
begin
  var Protected: Boolean;
  Result := CreateTempDir(Extension, LimitCurrentUserSidAccess, Protected);
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

procedure DelayDeleteFile({$IFDEF SETUPPROJ}const DisableFsRedir: Boolean;{$ENDIF} const Filename: String;
  const MaxTries, FirstRetryDelayMS, SubsequentRetryDelayMS: Cardinal);
{ Attempts to delete Filename up to MaxTries times, retrying if the file is
  in use. It sleeps FirstRetryDelayMS msec after the first try, and
  SubsequentRetryDelayMS msec after subsequent tries. }
begin
  for var I := 0 to MaxTries-1 do begin
    if I = 1 then
      Sleep(FirstRetryDelayMS)
    else if I > 1 then
      Sleep(SubsequentRetryDelayMS);
    if {$IFDEF SETUPPROJ}DeleteFileRedir(DisableFsRedir, Filename){$ELSE}Windows.DeleteFile(PChar(Filename)){$ENDIF} or
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

procedure SplitNewParamStr(const Index: Integer; var AName, AValue: String);
{ Reads a command line parameter. If it is in the form "/PARAM=VALUE" then
  AName is set to "/PARAM=" and AValue is set to "VALUE". Otherwise, the full
  parameter is stored in AName, and AValue is set to an empty string. }
var
  S: String;
  P: Integer;
begin
  S := NewParamStr(Index);
  if (S <> '') and (S[1] = '/') then begin
    P := PathPos('=', S);
    if P <> 0 then begin
      AName := Copy(S, 1, P);
      AValue := Copy(S, P+1, Maxint);
      Exit;
    end;
  end;
  AName := S;
  AValue := '';
end;

procedure RaiseFunctionFailedError(const FunctionName: String);
begin
  raise Exception.Create(FmtSetupMessage1(msgErrorFunctionFailedNoCode,
    FunctionName));
end;

function GetFinalFileName(const Filename: String): String;
{ Calls GetFinalPathNameByHandle to expand any SUBST'ed drives, network drives,
  and symbolic links in Filename. This is needed for elevation to succeed when
  Setup is started from a SUBST'ed drive letter. }

  function ConvertToNormalPath(P: PChar): String;
  begin
    Result := P;
    if StrLComp(P, '\\?\', 4) = 0 then begin
      Inc(P, 4);
      if (PathStrNextChar(P) = P + 1) and (P[1] = ':') and PathCharIsSlash(P[2]) then
        Result := P
      else if StrLIComp(P, 'UNC\', 4) = 0 then begin
        Inc(P, 4);
        Result := '\\' + P;
      end;
    end;
  end;

const
  FILE_SHARE_DELETE = $00000004;
var
  GetFinalPathNameByHandleFunc: function(hFile: THandle; lpszFilePath: PWideChar;
    cchFilePath: DWORD; dwFlags: DWORD): DWORD; stdcall;
  Attr, FlagsAndAttributes: DWORD;
  H: THandle;
  Buf: array[0..4095] of Char;
begin
  GetFinalPathNameByHandleFunc := GetProcAddress(GetModuleHandle(kernel32),
    'GetFinalPathNameByHandleW');
  if Assigned(GetFinalPathNameByHandleFunc) then begin
    Attr := GetFileAttributes(PChar(Filename));
    if Attr <> INVALID_FILE_ATTRIBUTES then begin
      { Backup semantics must be requested in order to open a directory }
      if Attr and FILE_ATTRIBUTE_DIRECTORY <> 0 then
        FlagsAndAttributes := FILE_FLAG_BACKUP_SEMANTICS
      else
        FlagsAndAttributes := 0;
      { Use zero access mask and liberal sharing mode to ensure success }
      H := CreateFile(PChar(Filename), 0, FILE_SHARE_READ or FILE_SHARE_WRITE or
        FILE_SHARE_DELETE, nil, OPEN_EXISTING, FlagsAndAttributes, 0);
      if H <> INVALID_HANDLE_VALUE then begin
        const Res = GetFinalPathNameByHandleFunc(H, Buf, SizeOf(Buf) div SizeOf(Buf[0]), 0);
        CloseHandle(H);
        if (Res > 0) and (Res < (SizeOf(Buf) div SizeOf(Buf[0])) - 16) then begin
          { ShellExecuteEx fails with error 3 on \\?\UNC\ paths, so try to
            convert the returned path from \\?\ form }
          Result := ConvertToNormalPath(Buf);
          Exit;
        end;
      end;
    end;
  end;
  Result := Filename;
end;

function GetFinalCurrentDir: String;
var
  Res: Integer;
  Buf: array[0..MAX_PATH-1] of Char;
begin
  DWORD(Res) := GetCurrentDirectory(SizeOf(Buf) div SizeOf(Buf[0]), Buf);
  if (Res > 0) and (Res < SizeOf(Buf) div SizeOf(Buf[0])) then
    Result := GetFinalFileName(Buf)
  else begin
    RaiseFunctionFailedError('GetCurrentDirectory');
    Result := '';
  end;
end;

end.
