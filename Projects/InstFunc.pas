unit InstFunc;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Misc. installation functions
}

interface

uses
  Windows, SysUtils, Struct, Int64Em, MD5, SHA1, CmnFunc2;

{$I VERSION.INC}

type
  PSimpleStringListArray = ^TSimpleStringListArray;
  TSimpleStringListArray = array[0..$1FFFFFFE] of String;
  TSimpleStringList = class
  private
    FList: PSimpleStringListArray;
    FCount, FCapacity: Integer;
    function Get(Index: Integer): String;
    procedure SetCapacity(NewCapacity: Integer);
  public
    destructor Destroy; override;
    procedure Add(const S: String);
    procedure AddIfDoesntExist(const S: String);
    procedure Clear;
    function IndexOf(const S: String): Integer;

    property Count: Integer read FCount;
    property Items[Index: Integer]: String read Get; default;
  end;

  TDeleteDirProc = function(const DisableFsRedir: Boolean; const DirName: String;
    const Param: Pointer): Boolean;
  TDeleteFileProc = function(const DisableFsRedir: Boolean; const FileName: String;
    const Param: Pointer): Boolean;

  TEnumFROFilenamesProc = procedure(const Filename: String; Param: Pointer);

  { Must keep this in synch with ScriptFunc_C: }
  TExecWait = (ewNoWait, ewWaitUntilTerminated, ewWaitUntilIdle);

  TDetermineDefaultLanguageResult = (ddNoMatch, ddMatch, ddMatchLangParameter);
  TGetLanguageEntryProc = function(Index: Integer; var Entry: PSetupLanguageEntry): Boolean;

function CheckForMutexes(Mutexes: String): Boolean;
function CreateTempDir: String;
function DecrementSharedCount(const RegView: TRegView; const Filename: String): Boolean;
procedure DelayDeleteFile(const DisableFsRedir: Boolean; const Filename: String;
  const MaxTries, FirstRetryDelayMS, SubsequentRetryDelayMS: Integer);
function DelTree(const DisableFsRedir: Boolean; const Path: String;
  const IsDir, DeleteFiles, DeleteSubdirsAlso, BreakOnError: Boolean;
  const DeleteDirProc: TDeleteDirProc; const DeleteFileProc: TDeleteFileProc;
  const Param: Pointer): Boolean;
function DetermineDefaultLanguage(const GetLanguageEntryProc: TGetLanguageEntryProc;
  const Method: TSetupLanguageDetectionMethod; const LangParameter: String;
  var ResultIndex: Integer): TDetermineDefaultLanguageResult;
procedure EnumFileReplaceOperationsFilenames(const EnumFunc: TEnumFROFilenamesProc;
  Param: Pointer);
function GenerateNonRandomUniqueFilename(Path: String; var Filename: String): Boolean;
function GenerateUniqueName(const DisableFsRedir: Boolean; Path: String;
  const Extension: String): String;
function GetComputerNameString: String;
function GetFileDateTime(const DisableFsRedir: Boolean; const Filename: String;
  var DateTime: TFileTime): Boolean;
function GetMD5OfFile(const DisableFsRedir: Boolean; const Filename: String): TMD5Digest;
function GetMD5OfAnsiString(const S: AnsiString): TMD5Digest;
{$IFDEF UNICODE}
function GetMD5OfUnicodeString(const S: UnicodeString): TMD5Digest;
{$ENDIF}
function GetSHA1OfFile(const DisableFsRedir: Boolean; const Filename: String): TSHA1Digest;
function GetSHA1OfAnsiString(const S: AnsiString): TSHA1Digest;
{$IFDEF UNICODE}
function GetSHA1OfUnicodeString(const S: UnicodeString): TSHA1Digest;
{$ENDIF}
function GetRegRootKeyName(const RootKey: HKEY): String;
function GetSpaceOnDisk(const DisableFsRedir: Boolean; const DriveRoot: String;
  var FreeBytes, TotalBytes: Integer64): Boolean;
function GetSpaceOnNearestMountPoint(const DisableFsRedir: Boolean;
  const StartDir: String; var FreeBytes, TotalBytes: Integer64): Boolean;
function GetUserNameString: String;
procedure IncrementSharedCount(const RegView: TRegView; const Filename: String;
  const AlreadyExisted: Boolean);
function InstExec(const DisableFsRedir: Boolean; const Filename, Params: String;
  WorkingDir: String; const Wait: TExecWait; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; var ResultCode: Integer): Boolean;
function InstShellExec(const Verb, Filename, Params: String; WorkingDir: String;
  const Wait: TExecWait; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; var ResultCode: Integer): Boolean;
procedure InternalError(const Id: String);
procedure InternalErrorFmt(const S: String; const Args: array of const);
function IsDirEmpty(const DisableFsRedir: Boolean; const Dir: String): Boolean;
function IsProtectedSystemFile(const DisableFsRedir: Boolean;
  const Filename: String): Boolean;
function MakePendingFileRenameOperationsChecksum: TMD5Digest;
function ModifyPifFile(const Filename: String; const CloseOnExit: Boolean): Boolean;
procedure RaiseFunctionFailedError(const FunctionName: String);
procedure RaiseOleError(const FunctionName: String; const ResultCode: HRESULT);
procedure RefreshEnvironment;
function ReplaceSystemDirWithSysWow64(const Path: String): String;
function ReplaceSystemDirWithSysNative(Path: String; const IsWin64: Boolean): String;
procedure UnregisterFont(const FontName, FontFilename: String);
function RestartComputer: Boolean;
procedure RestartReplace(const DisableFsRedir: Boolean; TempFile, DestFile: String);
procedure SplitNewParamStr(const Index: Integer; var AName, AValue: String);
procedure Win32ErrorMsg(const FunctionName: String);
procedure Win32ErrorMsgEx(const FunctionName: String; const ErrorCode: DWORD);

implementation

uses
  Messages, ShellApi, PathFunc, Msgs, MsgIDs, FileClass, RedirFunc;

procedure InternalError(const Id: String);
begin
  raise Exception.Create(FmtSetupMessage1(msgErrorInternal2, Id));
end;

procedure InternalErrorFmt(const S: String; const Args: array of const);
begin
  InternalError(Format(S, Args));
end;

procedure Win32ErrorMsgEx(const FunctionName: String; const ErrorCode: DWORD);
begin
  raise Exception.Create(FmtSetupMessage(msgErrorFunctionFailedWithMessage,
    [FunctionName, IntToStr(ErrorCode), Win32ErrorString(ErrorCode)]));
end;

procedure Win32ErrorMsg(const FunctionName: String);
begin
  Win32ErrorMsgEx(FunctionName, GetLastError);
end;

procedure RaiseOleError(const FunctionName: String; const ResultCode: HRESULT);
begin
  raise Exception.Create(FmtSetupMessage(msgErrorFunctionFailedWithMessage,
    [FunctionName, IntToHexStr8(ResultCode), Win32ErrorString(ResultCode)]));
end;

procedure RaiseFunctionFailedError(const FunctionName: String);
begin
  raise Exception.Create(FmtSetupMessage1(msgErrorFunctionFailedNoCode,
    FunctionName));
end;

function GetRegRootKeyName(const RootKey: HKEY): String;
begin
  case RootKey of
    HKEY_CLASSES_ROOT: Result := 'HKEY_CLASSES_ROOT';
    HKEY_CURRENT_USER: Result := 'HKEY_CURRENT_USER';
    HKEY_LOCAL_MACHINE: Result := 'HKEY_LOCAL_MACHINE';
    HKEY_USERS: Result := 'HKEY_USERS';
    HKEY_PERFORMANCE_DATA: Result := 'HKEY_PERFORMANCE_DATA';
    HKEY_CURRENT_CONFIG: Result := 'HKEY_CURRENT_CONFIG';
    HKEY_DYN_DATA: Result := 'HKEY_DYN_DATA';
  else
    { unknown - shouldn't get here }
    Result := Format('[%x]', [Cardinal(RootKey)]);
  end;
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

function GenerateNonRandomUniqueFilename(Path: String; var Filename: String): Boolean;
{ Returns True if it overwrote an existing file. }
var
  Rand, RandOrig: Longint;
  F: THandle;
  Success: Boolean;
  FN: String;
begin
  Path := AddBackslash(Path);
  RandOrig := $123456;
  Rand := RandOrig;
  Success := False;
  Result := False;
  repeat
    Inc(Rand);
    if Rand > $1FFFFFF then Rand := 0;
    if Rand = RandOrig then
      { practically impossible to go through 33 million possibilities,
        but check "just in case"... }
      raise Exception.Create(FmtSetupMessage1(msgErrorTooManyFilesInDir,
        RemoveBackslashUnlessRoot(Path)));
    { Generate a random name }
    FN := Path + '_iu' + IntToBase32(Rand) + '.tmp';
    if DirExists(FN) then Continue;
    Success := True;
    Result := NewFileExists(FN);
    if Result then begin
      F := CreateFile(PChar(FN), GENERIC_READ or GENERIC_WRITE, 0,
        nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
      Success := F <> INVALID_HANDLE_VALUE;
      if Success then CloseHandle(F);
    end;
  until Success;
  Filename := FN;
end;

function CreateTempDir: String;
var
  Dir: String;
  ErrorCode: DWORD;
begin
  while True do begin
    Dir := GenerateUniqueName(False, GetTempDir, '.tmp');
    if CreateDirectory(PChar(Dir), nil) then
      Break;
    ErrorCode := GetLastError;
    if ErrorCode <> ERROR_ALREADY_EXISTS then
      raise Exception.Create(FmtSetupMessage(msgLastErrorMessage,
        [FmtSetupMessage1(msgErrorCreatingDir, Dir), IntToStr(ErrorCode),
         Win32ErrorString(ErrorCode)]));
  end;
  Result := Dir;
end;

function ReplaceSystemDirWithSysWow64(const Path: String): String;
{ If the user is running 64-bit Windows and Path begins with
  'x:\windows\system32' it replaces it with 'x:\windows\syswow64', like the
  file system redirector would do. Otherwise, Path is returned unchanged. }
var
  SysWow64Dir, SysDir: String;
  L: Integer;
begin
  SysWow64Dir := GetSysWow64Dir;
  if SysWow64Dir <> '' then begin
    SysDir := GetSystemDir;
    { x:\windows\system32 -> x:\windows\syswow64
      x:\windows\system32\ -> x:\windows\syswow64\
      x:\windows\system32\filename -> x:\windows\syswow64\filename
      x:\windows\system32x -> x:\windows\syswow64x  <- yes, like Windows! }
    L := Length(SysDir);
    if (Length(Path) = L) or
       ((Length(Path) > L) and not PathCharIsTrailByte(Path, L+1)) then begin
                               { ^ avoid splitting a double-byte character }
      if PathCompare(Copy(Path, 1, L), SysDir) = 0 then begin
        Result := SysWow64Dir + Copy(Path, L+1, Maxint);
        Exit;
      end;
    end;
  end;
  Result := Path;
end;

function ReplaceSystemDirWithSysNative(Path: String; const IsWin64: Boolean): String;
{ If the user is running 64-bit Windows Vista or newer and Path
  begins with 'x:\windows\system32\' it replaces it with
  'x:\windows\sysnative\' and if Path equals 'x:\windows\system32'
  it replaces it with 'x:\windows\sysnative'. Otherwise, Path is
  returned unchanged. }
var
  SysNativeDir, SysDir: String;
  L: Integer;
begin
  SysNativeDir := GetSysNativeDir(IsWin64);
  if SysNativeDir <> '' then begin
    SysDir := GetSystemDir;
    if PathCompare(Path, SysDir) = 0 then begin
    { x:\windows\system32 -> x:\windows\sysnative }
      Result := SysNativeDir;
      Exit;
    end else begin
    { x:\windows\system32\ -> x:\windows\sysnative\
      x:\windows\system32\filename -> x:\windows\sysnative\filename }
      SysDir := AddBackslash(SysDir);
      L := Length(SysDir);
      if (Length(Path) = L) or
         ((Length(Path) > L) and not PathCharIsTrailByte(Path, L+1)) then begin
                                 { ^ avoid splitting a double-byte character }
        if PathCompare(Copy(Path, 1, L), SysDir) = 0 then begin
          Result := SysNativeDir + Copy(Path, L, Maxint);
          Exit;
        end;
      end;
    end;
  end;
  Result := Path;
end;

procedure RestartReplace(const DisableFsRedir: Boolean; TempFile, DestFile: String);
{ Renames TempFile to DestFile the next time Windows is started. If DestFile
  already existed, it will be overwritten. If DestFile is '' then TempFile
  will be deleted, however this is only supported by 95/98 and NT, not
  Windows 3.1x. }
var
  WinDir, WinInitFile, TempWinInitFile: String;
  OldF: TTextFileReader;
  NewF: TTextFileWriter;
  L, L2: String;
  RenameSectionFound, WriteLastLine: Boolean;
begin
  TempFile := PathExpand(TempFile);
  if DestFile <> '' then
    DestFile := PathExpand(DestFile);

  if not UsingWinNT then begin
    { Because WININIT.INI allows multiple entries with the same name,
      it must manually parse the file instead of using
      WritePrivateProfileString }
    WinDir := GetWinDir;
    WinInitFile := AddBackslash(WinDir) + 'WININIT.INI';
    TempWinInitFile := GenerateUniqueName(False, WinDir, '.tmp');
    try
      OldF := nil;
      NewF := nil;
      try
        { Flush Windows' cache for the file first }
        WritePrivateProfileString(nil, nil, nil, PChar(WinInitFile));
        OldF := TTextFileReader.Create(WinInitFile, fdOpenAlways, faRead,
          fsRead);
        NewF := TTextFileWriter.Create(TempWinInitFile, fdCreateAlways,
          faWrite, fsNone);
        RenameSectionFound := False;
        WriteLastLine := False;
        while not OldF.Eof do begin
          L := OldF.ReadLine;
          WriteLastLine := True;
          L2 := Trim(L);
          if (L2 <> '') and (L2[1] = '[') then begin
            if CompareText(L2, '[rename]') = 0 then
              RenameSectionFound := True
            else
            if RenameSectionFound then
              Break;
          end;
          NewF.WriteLine(L);
          WriteLastLine := False;
        end;
        if not RenameSectionFound then
          NewF.WriteLine('[rename]');
        if DestFile <> '' then
          L2 := GetShortName(DestFile)
        else
          L2 := 'NUL';
        NewF.WriteLine(L2 + '=' + GetShortName(TempFile));
        if WriteLastLine then
          NewF.WriteLine(L);
        while not OldF.Eof do begin
          L := OldF.ReadLine;
          NewF.WriteLine(L);
        end;
      finally
        NewF.Free;
        OldF.Free;
      end;
      { Strip any read-only attribute }
      SetFileAttributes(PChar(WinInitFile), FILE_ATTRIBUTE_ARCHIVE);
      if not DeleteFile(WinInitFile) then
        Win32ErrorMsg('DeleteFile');
      if not MoveFile(PChar(TempWinInitFile), PChar(WinInitFile)) then
        Win32ErrorMsg('MoveFile');
    except
      DeleteFile(TempWinInitFile);
      raise;
    end;
  end
  else begin
    if not DisableFsRedir then begin
      { Work around WOW64 bug present in the IA64 and x64 editions of Windows
        XP (3790) and Server 2003 prior to SP1 RC2: MoveFileEx writes filenames
        to the registry verbatim without mapping system32->syswow64. }
      TempFile := ReplaceSystemDirWithSysWow64(TempFile);
      if DestFile <> '' then
        DestFile := ReplaceSystemDirWithSysWow64(DestFile);
    end;
    if not MoveFileExRedir(DisableFsRedir, TempFile, DestFile,
       MOVEFILE_DELAY_UNTIL_REBOOT or MOVEFILE_REPLACE_EXISTING) then
      Win32ErrorMsg('MoveFileEx');
  end;
end;

function DelTree(const DisableFsRedir: Boolean; const Path: String;
  const IsDir, DeleteFiles, DeleteSubdirsAlso, BreakOnError: Boolean;
  const DeleteDirProc: TDeleteDirProc; const DeleteFileProc: TDeleteFileProc;
  const Param: Pointer): Boolean;
{ Deletes the specified directory including all files and subdirectories in
  it (including those with hidden, system, and read-only attributes). Returns
  True if it was able to successfully remove everything. If BreakOnError is
  set to True it will stop and return False the first time a delete failed or
  DeleteDirProc/DeleteFileProc returned False.  }
var
  BasePath, FindSpec: String;
  H: THandle;
  FindData: TWin32FindData;
  S: String;
begin
  Result := True;
  if DeleteFiles and
     (not IsDir or IsDirectoryAndNotReparsePointRedir(DisableFsRedir, Path)) then begin
    if IsDir then begin
      BasePath := AddBackslash(Path);
      FindSpec := BasePath + '*';
    end
    else begin
      BasePath := PathExtractPath(Path);
      FindSpec := Path;
    end;
    H := FindFirstFileRedir(DisableFsRedir, FindSpec, FindData);
    if H <> INVALID_HANDLE_VALUE then begin
      try
        repeat
          S := FindData.cFileName;
          if (S <> '.') and (S <> '..') then begin
            if FindData.dwFileAttributes and FILE_ATTRIBUTE_READONLY <> 0 then begin
              { Strip the read-only attribute if this is a file, or if it's a
                directory and we're deleting subdirectories also }
              if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0) or DeleteSubdirsAlso then
                SetFileAttributesRedir(DisableFsRedir, BasePath + S,
                  FindData.dwFileAttributes and not FILE_ATTRIBUTE_READONLY);
            end;
            if FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then begin
              if Assigned(DeleteFileProc) then begin
                if not DeleteFileProc(DisableFsRedir, BasePath + S, Param) then
                  Result := False;
              end
              else begin
                if not DeleteFileRedir(DisableFsRedir, BasePath + S) then
                  Result := False;
              end;
            end
            else begin
              if DeleteSubdirsAlso then
                if not DelTree(DisableFsRedir, BasePath + S, True, True, True, BreakOnError,
                   DeleteDirProc, DeleteFileProc, Param) then
                  Result := False;
            end;
          end;
        until (BreakOnError and not Result) or not FindNextFile(H, FindData);
      finally
        Windows.FindClose(H);
      end;
    end;
  end;
  if (not BreakOnError or Result) and IsDir then begin
    if Assigned(DeleteDirProc) then begin
      if not DeleteDirProc(DisableFsRedir, Path, Param) then
        Result := False;
    end
    else begin
      if not RemoveDirectoryRedir(DisableFsRedir, Path) then
        Result := False;
    end;
  end;
end;

function IsDirEmpty(const DisableFsRedir: Boolean; const Dir: String): Boolean;
{ Returns True if Dir contains no files or subdirectories.
  Note: If Dir does not exist or lacks list permission, False will be
  returned. }
var
  H: THandle;
  FindData: TWin32FindData;
begin
  H := FindFirstFileRedir(DisableFsRedir, AddBackslash(Dir) + '*', FindData);
  if H <> INVALID_HANDLE_VALUE then begin
    try
      Result := True;
      while True do begin
        if FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then begin
          { Found a file }
          Result := False;
          Break;
        end;
        if (StrComp(FindData.cFileName, '.') <> 0) and
           (StrComp(FindData.cFileName, '..') <> 0) then begin
          { Found a subdirectory }
          Result := False;
          Break;
        end;
        if not FindNextFile(H, FindData) then begin
          if GetLastError <> ERROR_NO_MORE_FILES then begin
            { Exited the loop early due to some unexpected error. The directory
              might not be empty, so return False }
            Result := False;
          end;
          Break;
        end;
      end;
    finally
      Windows.FindClose(H);
    end;
  end
  else begin
    { The directory may not exist, or it may lack list permission }
    Result := False;
  end;
end;

procedure IncrementSharedCount(const RegView: TRegView; const Filename: String;
  const AlreadyExisted: Boolean);
const
  SharedDLLsKey = NEWREGSTR_PATH_SETUP + '\SharedDLLs';  {don't localize}
var
  ErrorCode: Longint;
  K: HKEY;
  Disp, Size, Count, CurType, NewType: DWORD;
  CountStr: String;
  FilenameP: PChar;
begin
  ErrorCode := RegCreateKeyExView(RegView, HKEY_LOCAL_MACHINE, SharedDLLsKey, 0, nil,
    REG_OPTION_NON_VOLATILE, KEY_QUERY_VALUE or KEY_SET_VALUE, nil, K, @Disp);
  if ErrorCode <> ERROR_SUCCESS then
    raise Exception.Create(FmtSetupMessage(msgErrorRegOpenKey,
        [GetRegRootKeyName(HKEY_LOCAL_MACHINE), SharedDLLsKey]) + SNewLine2 +
      FmtSetupMessage(msgErrorFunctionFailedWithMessage,
        ['RegCreateKeyEx', IntToStr(ErrorCode), Win32ErrorString(ErrorCode)]));
  FilenameP := PChar(Filename);
  Count := 0;
  NewType := REG_DWORD;
  try
    if RegQueryValueEx(K, FilenameP, nil, @CurType, nil, @Size) = ERROR_SUCCESS then
      case CurType of
        REG_SZ:
          if RegQueryStringValue(K, FilenameP, CountStr) then begin
            Count := StrToInt(CountStr);
            NewType := REG_SZ;
          end;
        REG_BINARY: begin
            if (Size >= 1) and (Size <= 4) then begin
              if RegQueryValueEx(K, FilenameP, nil, nil, @Count, @Size) <> ERROR_SUCCESS then
                { ^ relies on the high 3 bytes of Count being initialized to 0 }
                Abort;
              NewType := REG_BINARY;
            end;
          end;
        REG_DWORD: begin
            Size := SizeOf(DWORD);
            if RegQueryValueEx(K, FilenameP, nil, nil, @Count, @Size) <> ERROR_SUCCESS then
              Abort;
          end;
      end;
  except
    Count := 0;
  end;
  if Integer(Count) < 0 then Count := 0;  { just in case... }
  if (Count = 0) and AlreadyExisted then
    Inc(Count);
  Inc(Count);
  case NewType of
    REG_SZ: begin
        CountStr := IntToStr(Count);
        RegSetValueEx(K, FilenameP, 0, NewType, PChar(CountStr), (Length(CountStr)+1)*SizeOf(CountStr[1]));
      end;
    REG_BINARY, REG_DWORD:
      RegSetValueEx(K, FilenameP, 0, NewType, @Count, SizeOf(Count));
  end;
  RegCloseKey(K);
end;

function DecrementSharedCount(const RegView: TRegView;
  const Filename: String): Boolean;
{ Attempts to decrement the shared file reference count of Filename. Returns
  True if the count reached zero (meaning it's OK to delete the file). }
const
  SharedDLLsKey = NEWREGSTR_PATH_SETUP + '\SharedDLLs';  {don't localize}
var
  ErrorCode: Longint;
  K: HKEY;
  CountRead: Boolean;
  Count, CurType, Size: DWORD;
  CountStr: String;
begin
  Result := False;

  ErrorCode := RegOpenKeyExView(RegView, HKEY_LOCAL_MACHINE, SharedDLLsKey, 0,
    KEY_QUERY_VALUE or KEY_SET_VALUE, K);
  if ErrorCode = ERROR_FILE_NOT_FOUND then
    Exit;
  if ErrorCode <> ERROR_SUCCESS then
    raise Exception.Create(FmtSetupMessage(msgErrorRegOpenKey,
        [GetRegRootKeyName(HKEY_LOCAL_MACHINE), SharedDLLsKey]) + SNewLine2 +
      FmtSetupMessage(msgErrorFunctionFailedWithMessage,
        ['RegOpenKeyEx', IntToStr(ErrorCode), Win32ErrorString(ErrorCode)]));
  try
    if RegQueryValueEx(K, PChar(Filename), nil, @CurType, nil, @Size) <> ERROR_SUCCESS then
      Exit;

    CountRead := False;
    Count := 0;
    try
      case CurType of
        REG_SZ:
          if RegQueryStringValue(K, PChar(Filename), CountStr) then begin
            Count := StrToInt(CountStr);
            CountRead := True;
          end;
        REG_BINARY: begin
            if (Size >= 1) and (Size <= 4) then begin
              if RegQueryValueEx(K, PChar(Filename), nil, nil, @Count, @Size) = ERROR_SUCCESS then
                { ^ relies on the high 3 bytes of Count being initialized to 0 }
                CountRead := True;
            end;
          end;
        REG_DWORD: begin
            Size := SizeOf(DWORD);
            if RegQueryValueEx(K, PChar(Filename), nil, nil, @Count, @Size) = ERROR_SUCCESS then
              CountRead := True;
          end;
      end;
    except
      { don't propogate exceptions (e.g. from StrToInt) }
    end;
    { If we failed to read the count, or it's in some type we don't recognize,
      don't touch it }
    if not CountRead then
      Exit;

    Dec(Count);
    if Integer(Count) <= 0 then begin
      Result := True;
      RegDeleteValue(K, PChar(Filename));
    end
    else begin
      case CurType of
        REG_SZ: begin
            CountStr := IntToStr(Count);
            RegSetValueEx(K, PChar(Filename), 0, REG_SZ, PChar(CountStr), (Length(CountStr)+1)*SizeOf(Char));
          end;
        REG_BINARY, REG_DWORD:
          RegSetValueEx(K, PChar(Filename), 0, CurType, @Count, SizeOf(Count));
      end;
    end;
  finally
    RegCloseKey(K);
  end;
end;

function GetFileDateTime(const DisableFsRedir: Boolean; const Filename: String;
  var DateTime: TFileTime): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Handle := FindFirstFileRedir(DisableFsRedir, Filename, FindData);
  if Handle <> INVALID_HANDLE_VALUE then begin
    Windows.FindClose(Handle);
    if FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then begin
      DateTime := FindData.ftLastWriteTime;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
  DateTime.dwLowDateTime := 0;
  DateTime.dwHighDateTime := 0;
end;

function GetMD5OfFile(const DisableFsRedir: Boolean; const Filename: String): TMD5Digest;
{ Gets MD5 sum of the file Filename. An exception will be raised upon
  failure. }
var
  F: TFile;
  NumRead: Cardinal;
  Context: TMD5Context;
  Buf: array[0..65535] of Byte;
begin
  MD5Init(Context);
  F := TFileRedir.Create(DisableFsRedir, Filename, fdOpenExisting, faRead, fsReadWrite);
  try
    while True do begin
      NumRead := F.Read(Buf, SizeOf(Buf));
      if NumRead = 0 then Break;
      MD5Update(Context, Buf, NumRead);
    end;
  finally
    F.Free;
  end;
  Result := MD5Final(Context);
end;

function GetSHA1OfFile(const DisableFsRedir: Boolean; const Filename: String): TSHA1Digest;
{ Gets SHA-1 sum of the file Filename. An exception will be raised upon
  failure. }
var
  F: TFile;
  NumRead: Cardinal;
  Context: TSHA1Context;
  Buf: array[0..65535] of Byte;
begin
  SHA1Init(Context);
  F := TFileRedir.Create(DisableFsRedir, Filename, fdOpenExisting, faRead, fsReadWrite);
  try
    while True do begin
      NumRead := F.Read(Buf, SizeOf(Buf));
      if NumRead = 0 then Break;
      SHA1Update(Context, Buf, NumRead);
    end;
  finally
    F.Free;
  end;
  Result := SHA1Final(Context);
end;

function GetMD5OfAnsiString(const S: AnsiString): TMD5Digest;
begin
  Result := MD5Buf(Pointer(S)^, Length(S)*SizeOf(S[1]));
end;

{$IFDEF UNICODE}
function GetMD5OfUnicodeString(const S: UnicodeString): TMD5Digest;
begin
  Result := MD5Buf(Pointer(S)^, Length(S)*SizeOf(S[1]));
end;
{$ENDIF}

function GetSHA1OfAnsiString(const S: AnsiString): TSHA1Digest;
begin
  Result := SHA1Buf(Pointer(S)^, Length(S)*SizeOf(S[1]));
end;

{$IFDEF UNICODE}
function GetSHA1OfUnicodeString(const S: UnicodeString): TSHA1Digest;
begin
  Result := SHA1Buf(Pointer(S)^, Length(S)*SizeOf(S[1]));
end;
{$ENDIF}

var
  SFCInitialized: Boolean;
  SfcIsFileProtectedFunc: function(RpcHandle: THandle; ProtFileName: PWideChar): BOOL; stdcall;

function IsProtectedSystemFile(const DisableFsRedir: Boolean;
  const Filename: String): Boolean;
{ Returns True if the specified file is protected by Windows File Protection
  (and therefore can't be replaced). }
var
  M: HMODULE;
  FN: String;
{$IFNDEF UNICODE}
  Buf: array[0..4095] of WideChar;
{$ENDIF}
begin
  if not SFCInitialized then begin
    M := SafeLoadLibrary(PChar(AddBackslash(GetSystemDir) + 'sfc.dll'),
      SEM_NOOPENFILEERRORBOX);
    if M <> 0 then
      SfcIsFileProtectedFunc := GetProcAddress(M, 'SfcIsFileProtected');
    SFCInitialized := True;
  end;
  if Assigned(SfcIsFileProtectedFunc) then begin
    { The function only accepts fully qualified paths. Also, as of
      IA-64 2003 SP1 and x64 XP, it does not respect file system redirection,
      so a call to ReplaceSystemDirWithSysWow64 is needed. }
    FN := PathExpand(Filename);
    if not DisableFsRedir then
      FN := ReplaceSystemDirWithSysWow64(FN);
{$IFDEF UNICODE}
    Result := SfcIsFileProtectedFunc(0, PChar(FN));
{$ELSE}
    Buf[MultiByteToWideChar(CP_ACP, 0, PChar(FN), Length(FN), Buf,
      (SizeOf(Buf) div SizeOf(Buf[0])) - 1)] := #0;
    Result := (Buf[0] <> #0) and SfcIsFileProtectedFunc(0, Buf);
{$ENDIF}
  end
  else begin
    { Windows File Protection doesn't exist on Windows 95/98/NT4 }
    Result := False;
  end;
end;

procedure HandleProcessWait(ProcessHandle: THandle; const Wait: TExecWait;
  const ProcessMessagesProc: TProcedure; var ResultCode: Integer);
begin
  try
    if Wait = ewWaitUntilIdle then begin
      repeat
        ProcessMessagesProc;
      until WaitForInputIdle(ProcessHandle, 50) <> WAIT_TIMEOUT;
    end;
    if Wait = ewWaitUntilTerminated then begin
      { Wait until the process returns, but still process any messages that
        arrive. }
      repeat
        { Process any pending messages first because MsgWaitForMultipleObjects
          (called below) only returns when *new* messages arrive }
        ProcessMessagesProc;
      until MsgWaitForMultipleObjects(1, ProcessHandle, False, INFINITE, QS_ALLINPUT) <> WAIT_OBJECT_0+1;
      { Process messages once more in case MsgWaitForMultipleObjects saw the
        process terminate and new messages arrive simultaneously. (Can't leave
        unprocessed messages waiting, or a subsequent call to WaitMessage
        won't see them.) }
      ProcessMessagesProc;
    end;
    { Get the exit code. Will be set to STILL_ACTIVE if not yet available }
    if not GetExitCodeProcess(ProcessHandle, DWORD(ResultCode)) then
      ResultCode := -1;  { just in case }
  finally
    CloseHandle(ProcessHandle);
  end;
end;

function InstExec(const DisableFsRedir: Boolean; const Filename, Params: String;
  WorkingDir: String; const Wait: TExecWait; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; var ResultCode: Integer): Boolean;
var
  CmdLine: String;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  if Filename = '>' then
    CmdLine := Params
  else begin
    CmdLine := '"' + Filename + '"';
    if Params <> '' then
      CmdLine := CmdLine + ' ' + Params;
    if (CompareText(PathExtractExt(Filename), '.bat') = 0) or
       (CompareText(PathExtractExt(Filename), '.cmd') = 0) then begin
      { Use our own handling for .bat and .cmd files since passing them straight
        to CreateProcess on Windows NT 4.0 has problems: it doesn't properly
        quote the command line it passes to cmd.exe. This didn't work before:
          Filename: "c:\batch.bat"; Parameters: """abc"""
        And other Windows versions might have unknown quirks too, since
        CreateProcess isn't documented to accept .bat files in the first place. }
      if UsingWinNT then
        { With cmd.exe, the whole command line must be quoted for quoted
          parameters to work. For example, this fails:
            cmd.exe /c "z:\blah.bat" "test"
          But this works:
            cmd.exe /c ""z:\blah.bat" "test""
        }
        CmdLine := '"' + AddBackslash(GetSystemDir) + 'cmd.exe" /C "' + CmdLine + '"'
      else
        CmdLine := '"' + AddBackslash(GetWinDir) + 'COMMAND.COM" /C ' + CmdLine;
    end;
    if WorkingDir = '' then
      WorkingDir := PathExtractDir(Filename);
  end;
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := ShowCmd;
  if WorkingDir = '' then
    WorkingDir := GetSystemDir;

  Result := CreateProcessRedir(DisableFsRedir, nil, PChar(CmdLine), nil, nil, False,
    CREATE_DEFAULT_ERROR_MODE, nil, PChar(WorkingDir), StartupInfo, ProcessInfo);
  if not Result then begin
    ResultCode := GetLastError;
    Exit;
  end;

  { Don't need the thread handle, so close it now }
  CloseHandle(ProcessInfo.hThread);
  HandleProcessWait(ProcessInfo.hProcess, Wait, ProcessMessagesProc, ResultCode);
end;

function InstShellExec(const Verb, Filename, Params: String; WorkingDir: String;
  const Wait: TExecWait; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; var ResultCode: Integer): Boolean;
var
  Info: TShellExecuteInfo;
begin
  if WorkingDir = '' then begin
    WorkingDir := PathExtractDir(Filename);
    if WorkingDir = '' then
      WorkingDir := GetSystemDir;
  end;

  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(Info);
  Info.fMask := SEE_MASK_FLAG_NO_UI or SEE_MASK_FLAG_DDEWAIT or
    SEE_MASK_NOCLOSEPROCESS;
  if Verb <> '' then
    Info.lpVerb := PChar(Verb);
  Info.lpFile := PChar(Filename);
  Info.lpParameters := PChar(Params);
  Info.lpDirectory := PChar(WorkingDir);
  Info.nShow := ShowCmd;
  Result := ShellExecuteEx(@Info);
  if not Result then begin
    ResultCode := GetLastError;
    Exit;
  end;

  ResultCode := STILL_ACTIVE;
  { A process handle won't always be returned, e.g. if DDE was used }
  if Info.hProcess <> 0 then
    HandleProcessWait(Info.hProcess, Wait, ProcessMessagesProc, ResultCode);
end;

function CheckForMutexes(Mutexes: String): Boolean;

  function MutexPos(const S: String): Integer;
  var
    I: Integer;
  begin
    for I := 1 to Length(S) do begin
      if (S[I] = ',') and ((I = 1) or (S[I-1] <> '\')) then begin
        Result := I;
        Exit;
      end;
    end;
    Result := 0;
  end;

{ Returns True if any of the mutexes in the comma-separated Mutexes string
  exist }
var
  I: Integer;
  M: String;
  H: THandle;
begin
  Result := False;
  repeat
    I := MutexPos(Mutexes);
    if I = 0 then I := Maxint;
    M := Trim(Copy(Mutexes, 1, I-1));
    if M <> '' then begin
      StringChange(M, '\,', ',');
      H := OpenMutex(SYNCHRONIZE, False, PChar(M));
      if H <> 0 then begin
        CloseHandle(H);
        Result := True;
        Break;
      end;
    end;
    Delete(Mutexes, 1, I);
  until Mutexes = '';
end;

function ModifyPifFile(const Filename: String; const CloseOnExit: Boolean): Boolean;
{ Changes the "Close on exit" setting of a .pif file. Returns True if it was
  able to make the change. }
var
  F: TFile;
  B: Byte;
begin
  { Note: Specs on the .pif format were taken from
    http://smsoft.chat.ru/en/pifdoc.htm }
  Result := False;
  F := TFile.Create(Filename, fdOpenExisting, faReadWrite, fsNone);
  try
    { Is it a valid .pif file? }
    if F.Size.Lo >= $171 then begin
      F.Seek($63);
      F.ReadBuffer(B, SizeOf(B));
      { Toggle the "Close on exit" bit }
      if (B and $10 <> 0) <> CloseOnExit then begin
        B := B xor $10;
        F.Seek($63);
        F.WriteBuffer(B, SizeOf(B));
      end;
      Result := True;
    end;
  finally
    F.Free;
  end;
end;

function GetComputerNameString: String;
var
  Buf: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  Size: DWORD;
begin
  Size := SizeOf(Buf) div SizeOf(Buf[0]);
  if GetComputerName(Buf, Size) then
    Result := Buf
  else
    Result := '';
end;

function GetUserNameString: String;
var
  Buf: array[0..256] of Char;  { 256 = UNLEN }
  BufSize: DWORD;
begin
  BufSize := SizeOf(Buf) div SizeOf(Buf[0]);
  if GetUserName(Buf, BufSize) then
    Result := Buf
  else
    Result := '';
end;

{ Work around problem in D2's declaration of the function }
function NewAdjustTokenPrivileges(TokenHandle: THandle; DisableAllPrivileges: BOOL;
  const NewState: TTokenPrivileges; BufferLength: DWORD;
  PreviousState: PTokenPrivileges; ReturnLength: PDWORD): BOOL; stdcall;
  external advapi32 name 'AdjustTokenPrivileges';

function RestartComputer: Boolean;
{ Restarts the computer. On Windows 9x/Me, the function will NOT return if it
  is successful. }
var
  Token: THandle;
  TokenPriv: TTokenPrivileges;
const
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';  { don't localize }
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    if not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
       {$IFNDEF Delphi3orHigher} @Token {$ELSE} Token {$ENDIF}) then begin
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
  end;

  Result := ExitWindowsEx(EWX_REBOOT, 0);

  { On Windows 9x/Me:
    ExitWindowsEx synchronously sends WM_QUERYENDSESSION messages to all
    processes except the current process. If any WM_QUERYENDSESSION handler
    blocks the shutdown, it returns False. Otherwise, it kills the current
    process and does not return.
    On NT platforms:
    ExitWindowsEx returns True immediately. The system then asynchronously
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

function MakePendingFileRenameOperationsChecksum: TMD5Digest;
{ Calculates a checksum of the current PendingFileRenameOperations registry
  value (on NT 4+ platforms) or of the current WININIT.INI file (on non-NT
  platforms). The caller can use this checksum to determine if
  PendingFileRenameOperations or WININIT.INI was changed (perhaps by another
  program). }
var
  Context: TMD5Context;
  K: HKEY;
  S: String;
  WinInitFile: String;
  F: TFile;
  Buf: array[0..4095] of Byte;
  BytesRead: Cardinal;
begin
  MD5Init(Context);
  try
    if UsingWinNT then begin
      if RegOpenKeyExView(rvDefault, HKEY_LOCAL_MACHINE, 'SYSTEM\CurrentControlSet\Control\Session Manager',
         0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        if RegQueryMultiStringValue(K, 'PendingFileRenameOperations', S) then
          MD5Update(Context, S[1], Length(S)*SizeOf(S[1]));
        { When "PendingFileRenameOperations" is full, it spills over into
          "PendingFileRenameOperations2" }
        if RegQueryMultiStringValue(K, 'PendingFileRenameOperations2', S) then
          MD5Update(Context, S[1], Length(S)*SizeOf(S[1]));
        RegCloseKey(K);
      end;
    end
    else begin
      WinInitFile := AddBackslash(GetWinDir) + 'WININIT.INI';
      if NewFileExists(WinInitFile) then begin
        F := TFile.Create(WinInitFile, fdOpenExisting, faRead, fsRead);
        try
          while True do begin
            BytesRead := F.Read(Buf, SizeOf(Buf));
            if BytesRead = 0 then
              Break;
            MD5Update(Context, Buf, BytesRead);
          end;
        finally
          F.Free;
        end;
      end;
    end;
  except
    { don't propogate exceptions }
  end;
  Result := MD5Final(Context);
end;

procedure EnumFileReplaceOperationsFilenames(const EnumFunc: TEnumFROFilenamesProc;
  Param: Pointer);
{ Enumerates all the filenames in the current PendingFileRenameOperations
  registry value or WININIT.INI file. The function does not distinguish between
  source and destination filenames; it enumerates both. }

  procedure DoNT;

    procedure DoValue(const K: HKEY; const ValueName: PChar);
    var
      S: String;
      P, PEnd: PChar;
    begin
      if not RegQueryMultiStringValue(K, ValueName, S) then
        Exit;
      P := PChar(S);
      PEnd := P + Length(S);
      while P < PEnd do begin
        if P[0] = '!' then
          { Note: '!' means that MoveFileEx was called with the
            MOVEFILE_REPLACE_EXISTING flag }
          Inc(P);
        if StrLComp(P, '\??\', 4) = 0 then begin
          Inc(P, 4);
          if P[0] <> #0 then
            EnumFunc(P, Param);
        end;
        Inc(P, StrLen(P) + 1);
      end;
    end;

  var
    K: HKEY;
  begin
    if RegOpenKeyExView(rvDefault, HKEY_LOCAL_MACHINE, 'SYSTEM\CurrentControlSet\Control\Session Manager',
       0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      try
        DoValue(K, 'PendingFileRenameOperations');
        { When "PendingFileRenameOperations" is full, it spills over into
          "PendingFileRenameOperations2" }
        DoValue(K, 'PendingFileRenameOperations2');
      finally
        RegCloseKey(K);
      end;
    end;
  end;

  procedure DoNonNT;
  var
    WinInitFile: String;
    F: TTextFileReader;
    Line, Filename: String;
    InRenameSection: Boolean;
    P: Integer;
  begin
    WinInitFile := AddBackslash(GetWinDir) + 'WININIT.INI';
    if not NewFileExists(WinInitFile) then
      Exit;
    try
      F := TTextFileReader.Create(WinInitFile, fdOpenExisting, faRead, fsRead);
      try
        InRenameSection := False;
        while not F.Eof do begin
          Line := Trim(F.ReadLine);
          if (Line = '') or (Line[1] = ';') then
            Continue;
          if Line[1] = '[' then begin
            InRenameSection := (CompareText(Line, '[rename]') = 0);
          end
          else if InRenameSection then begin
            P := Pos('=', Line);
            if P > 0 then begin
              Filename := Copy(Line, 1, P-1);
              if (Filename <> '') and (CompareText(Filename, 'NUL') <> 0) then
                EnumFunc(Filename, Param);
              Filename := Copy(Line, P+1, Maxint);
              if (Filename <> '') and (CompareText(Filename, 'NUL') <> 0) then
                EnumFunc(Filename, Param);
            end;
          end;
        end;
      finally
        F.Free;
      end;
    except
      { ignore exceptions }
    end;
  end;

begin
  if UsingWinNT then
    DoNT
  else
    DoNonNT;
end;

procedure UnregisterFont(const FontName, FontFilename: String);
const
  FontsKeys: array[Boolean] of PChar =
    (NEWREGSTR_PATH_SETUP + '\Fonts',
     'Software\Microsoft\Windows NT\CurrentVersion\Fonts');
var
  K: HKEY;
begin
  if RegOpenKeyExView(rvDefault, HKEY_LOCAL_MACHINE, FontsKeys[UsingWinNT],
     0, KEY_SET_VALUE, K) = ERROR_SUCCESS then begin
    RegDeleteValue(K, PChar(FontName));
    RegCloseKey(K);
  end;
  if RemoveFontResource(PChar(FontFilename)) then
    SendNotifyMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
end;

function GetSpaceOnDisk(const DisableFsRedir: Boolean; const DriveRoot: String;
  var FreeBytes, TotalBytes: Integer64): Boolean;
var
  GetDiskFreeSpaceExFunc: function(lpDirectoryName: PChar;
    lpFreeBytesAvailable: PLargeInteger; lpTotalNumberOfBytes: PLargeInteger;
    lpTotalNumberOfFreeBytes: PLargeInteger): BOOL; stdcall;
  PrevState: TPreviousFsRedirectionState;
  SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters: Cardinal;
begin
  { NOTE: The docs claim that GetDiskFreeSpace supports UNC paths on
    Windows 95 OSR2 and later. But that does not seem to be the case in my
    tests; it fails with error 50 on Windows 95 through Me.
    GetDiskFreeSpaceEx, however, *does* succeed with UNC paths, so use it
    if available. }
  GetDiskFreeSpaceExFunc := GetProcAddress(GetModuleHandle(kernel32),
    {$IFDEF UNICODE}'GetDiskFreeSpaceExW'{$ELSE}'GetDiskFreeSpaceExA'{$ENDIF});
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    if Assigned(@GetDiskFreeSpaceExFunc) then begin
      Result := GetDiskFreeSpaceExFunc(PChar(AddBackslash(PathExpand(DriveRoot))),
        @TLargeInteger(FreeBytes), @TLargeInteger(TotalBytes), nil);
    end
    else begin
      Result := GetDiskFreeSpace(PChar(AddBackslash(PathExtractDrive(PathExpand(DriveRoot)))),
        DWORD(SectorsPerCluster), DWORD(BytesPerSector), DWORD(FreeClusters),
        DWORD(TotalClusters));
      if Result then begin
        { Windows 95/98 cap the result of GetDiskFreeSpace at 2GB, but NT 4.0
          does not, so we must use a 64-bit multiply operation to avoid an
          overflow. }
        Multiply32x32to64(BytesPerSector * SectorsPerCluster, FreeClusters,
          FreeBytes);
        Multiply32x32to64(BytesPerSector * SectorsPerCluster, TotalClusters,
          TotalBytes);
      end;
    end;
  finally
    RestoreFsRedirection(PrevState);
  end;
end;

function GetSpaceOnNearestMountPoint(const DisableFsRedir: Boolean;
  const StartDir: String; var FreeBytes, TotalBytes: Integer64): Boolean;
{ Gets the free and total space available on the specified directory. If that
  fails (e.g. if the directory does not exist), then it strips off the last
  component of the path and tries again. This repeats until it reaches the
  root. Returns True if successful. }
var
  Dir: String;
  LastLen: Integer;
begin
  Result := False;
  Dir := RemoveBackslashUnlessRoot(StartDir);
  LastLen := 0;
  while Length(Dir) <> LastLen do begin
    if GetSpaceOnDisk(DisableFsRedir, Dir, FreeBytes, TotalBytes) then begin
      Result := True;
      Break;
    end;
    LastLen := Length(Dir);
    Dir := PathExtractDir(Dir);
  end;
end;

procedure RefreshEnvironment;
{ Notifies other applications (Explorer) that environment variables have
  changed. Based on code from KB article 104011.
  Note: Win9x's Explorer ignores this message. }
var
  MsgResult: DWORD;
begin
  { Note: We originally used SendNotifyMessage to broadcast the message but it
    turned out that while it worked fine on NT 4 and 2000 it didn't work on XP
    -- the string "Environment" in lParam would be garbled on the receiving
    end (why I'm not exactly sure). We now use SendMessageTimeout as directed
    in the KB article 104011. It isn't as elegant since it could cause us to
    be delayed if another app is hung, but it'll have to do. }
  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
    LPARAM(PChar('Environment')), SMTO_ABORTIFHUNG, 5000, MsgResult);
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

function DetermineDefaultLanguage(const GetLanguageEntryProc: TGetLanguageEntryProc;
  const Method: TSetupLanguageDetectionMethod; const LangParameter: String;
  var ResultIndex: Integer): TDetermineDefaultLanguageResult;
{ Finds the index of the language entry that most closely matches the user's
  UI language / locale. If no match is found, ResultIndex is set to 0. }

{$IFDEF UNICODE}
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
{$ENDIF}

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
{$IFNDEF UNICODE}
        if (LangEntry.LanguageCodePage = 0) or (LangEntry.LanguageCodePage = GetACP) then
{$ENDIF}
        begin
          ResultIndex := I;
          Result := ddMatch;
          Exit;
        end;
      end;
      Inc(I);
    end;
    { Look for just a primary language ID match }
    I := 0;
    while GetLanguageEntryProc(I, LangEntry) do begin
      if (LangEntry.LanguageID and $3FF) = (UILang and $3FF) then begin
{$IFNDEF UNICODE}
        if (LangEntry.LanguageCodePage = 0) or (LangEntry.LanguageCodePage = GetACP) then
{$ELSE}
        { On Unicode, there is no LanguageCodePage filter, so we have to check
          the language IDs to ensure we don't return Simplified Chinese on a
          Traditional Chinese system, or vice versa.
          If the default ANSI code pages associated with the language IDs are
          equal, then there is no Simplified/Traditional discrepancy.
           Simplified Chinese LANGIDs ($0804, $1004)        use CP 936
          Traditional Chinese LANGIDs ($0404, $0C04, $1404) use CP 950 }
        if ((UILang and $3FF) <> LANG_CHINESE) or
           (GetCodePageFromLangID(LangEntry.LanguageID) = GetCodePageFromLangID(UILang)) then
{$ENDIF}
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

{ TSimpleStringList }

procedure TSimpleStringList.Add(const S: String);
var
  Delta: Integer;
begin
  if FCount = FCapacity then begin
    if FCapacity > 64 then Delta := FCapacity div 4 else
      if FCapacity > 8 then Delta := 16 else
        Delta := 4;
    SetCapacity(FCapacity + Delta);
  end;
  FList^[FCount] := S;
  Inc(FCount);
end;

procedure TSimpleStringList.AddIfDoesntExist(const S: String);
begin
  if IndexOf(S) = -1 then
    Add(S);
end;

procedure TSimpleStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(Pointer));
  if NewCapacity > FCapacity then
    FillChar(FList^[FCapacity], (NewCapacity - FCapacity) * SizeOf(Pointer), 0);
  FCapacity := NewCapacity;
end;

procedure TSimpleStringList.Clear;
begin
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TSimpleStringList.Get(Index: Integer): String;
begin
  Result := FList^[Index];
end;

function TSimpleStringList.IndexOf(const S: String): Integer;
{ Note: This is case-sensitive, unlike TStringList.IndexOf }
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FCount-1 do
    if FList^[I] = S then begin
      Result := I;
      Break;
    end;
end;

destructor TSimpleStringList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

end.
