unit Setup.InstFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Misc. installation functions. Used only by the Setup project.
}

interface

uses
  Windows, SysUtils, Shared.Int64Em, SHA256, Shared.CommonFunc;

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

  { Must keep this in synch with Compiler.ScriptFunc.pas: }
  TExecWait = (ewNoWait, ewWaitUntilTerminated, ewWaitUntilIdle);

function CheckForMutexes(const Mutexes: String): Boolean;
procedure CreateMutexes(const Mutexes: String);
function DecrementSharedCount(const RegView: TRegView; const Filename: String): Boolean;
function DelTree(const DisableFsRedir: Boolean; const Path: String;
  const IsDir, DeleteFiles, DeleteSubdirsAlso, BreakOnError: Boolean;
  const DeleteDirProc: TDeleteDirProc; const DeleteFileProc: TDeleteFileProc;
  const Param: Pointer): Boolean;
procedure EnumFileReplaceOperationsFilenames(const EnumFunc: TEnumFROFilenamesProc;
  Param: Pointer);
function GenerateNonRandomUniqueTempDir(const LimitCurrentUserSidAccess: Boolean;
  Path: String; var TempDir: String): Boolean;
function GetComputerNameString: String;
function GetFileDateTime(const DisableFsRedir: Boolean; const Filename: String;
  var DateTime: TFileTime): Boolean;
function GetSHA256OfFile(const DisableFsRedir: Boolean; const Filename: String): TSHA256Digest;
function GetSHA256OfAnsiString(const S: AnsiString): TSHA256Digest;
function GetSHA256OfUnicodeString(const S: UnicodeString): TSHA256Digest;
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
  const ProcessMessagesProc: TProcedure; const OutputReader: TCreateProcessOutputReader;
  var ResultCode: Integer): Boolean;
function InstShellExec(const Verb, Filename, Params: String; WorkingDir: String;
  const Wait: TExecWait; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; var ResultCode: Integer): Boolean;
procedure InternalError(const Id: String);
procedure InternalErrorFmt(const S: String; const Args: array of const);
function IsDirEmpty(const DisableFsRedir: Boolean; const Dir: String): Boolean;
function IsProtectedSystemFile(const DisableFsRedir: Boolean;
  const Filename: String): Boolean;
function MakePendingFileRenameOperationsChecksum: TSHA256Digest;
function ModifyPifFile(const Filename: String; const CloseOnExit: Boolean): Boolean;
function PathHasInvalidCharacters(const S: String;
  const AllowDriveLetterColon: Boolean): Boolean;
procedure RaiseFunctionFailedError(const FunctionName: String);
procedure RaiseOleError(const FunctionName: String; const ResultCode: HRESULT);
procedure RefreshEnvironment;
function ReplaceSystemDirWithSysWow64(const Path: String): String;
function ReplaceSystemDirWithSysNative(Path: String; const IsWin64: Boolean): String;
procedure UnregisterFont(const FontName, FontFilename: String; const PerUserFont: Boolean);
procedure RestartReplace(const DisableFsRedir: Boolean; TempFile, DestFile: String);
procedure SplitNewParamStr(const Index: Integer; var AName, AValue: String);
procedure Win32ErrorMsg(const FunctionName: String);
procedure Win32ErrorMsgEx(const FunctionName: String; const ErrorCode: DWORD);
function ForceDirectories(const DisableFsRedir: Boolean; Dir: String): Boolean;

implementation

uses
  Messages, ShellApi, PathFunc, SetupLdrAndSetup.InstFunc, SetupLdrAndSetup.Messages,
  Shared.SetupMessageIDs, Shared.FileClass, SetupLdrAndSetup.RedirFunc, Shared.SetupTypes,
  Classes, RegStr, Math;

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
    HKEY_AUTO: InternalError('GetRegRootKeyName called for HKEY_AUTO');
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

function GenerateNonRandomUniqueTempDir(const LimitCurrentUserSidAccess: Boolean;
  Path: String; var TempDir: String): Boolean;
{ Creates a new temporary directory with a non-random name. Returns True if an
  existing directory was re-created. This is called by Uninstall. A non-random
  name is used because the uninstaller EXE isn't able to delete itself; if it were
  random, there would be one directory added each time an uninstaller is run. }
var
  Rand, RandOrig: Longint; { These are actually NOT random in any way }
  ErrorCode: DWORD;
begin
  Path := AddBackslash(Path);
  RandOrig := $123456;
  Rand := RandOrig;
  repeat
    Result := False;
    Inc(Rand);
    if Rand > $1FFFFFF then Rand := 0;
    if Rand = RandOrig then
      { practically impossible to go through 33 million possibilities,
        but check "just in case"... }
      raise Exception.Create(FmtSetupMessage1(msgErrorTooManyFilesInDir,
        RemoveBackslashUnlessRoot(Path)));
    { Generate a "random" name }
    TempDir := Path + 'iu-' + IntToBase32(Rand) + '.tmp';
    if DirExists(TempDir) then begin
      if not DeleteDirTree(TempDir) then Continue;
      Result := True;
    end else if NewFileExists(TempDir) then
      if not DeleteFile(TempDir) then Continue;

    if CreateSafeDirectory(LimitCurrentUserSidAccess, TempDir, ErrorCode) then Break;
    if ErrorCode <> ERROR_ALREADY_EXISTS then
      raise Exception.Create(FmtSetupMessage(msgLastErrorMessage,
        [FmtSetupMessage1(msgErrorCreatingDir, TempDir), IntToStr(ErrorCode),
         Win32ErrorString(ErrorCode)]));
  until False; // continue until a new directory was created
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
{ If Path begins with 'x:\windows\system32\' it replaces it with
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
  will be deleted.. }
begin
  TempFile := PathExpand(TempFile);
  if DestFile <> '' then
    DestFile := PathExpand(DestFile);

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
  SharedDLLsKey = REGSTR_PATH_SETUP + '\SharedDLLs';  {don't localize}
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
  SharedDLLsKey = REGSTR_PATH_SETUP + '\SharedDLLs';  {don't localize}
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
      { don't propagate exceptions (e.g. from StrToInt) }
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

function GetSHA256OfFile(const DisableFsRedir: Boolean; const Filename: String): TSHA256Digest;
{ Gets SHA-256 sum as a string of the file Filename. An exception will be raised upon
  failure. }
var
  Buf: array[0..65535] of Byte;
begin
  var Context: TSHA256Context;
  SHA256Init(Context);
  var F := TFileRedir.Create(DisableFsRedir, Filename, fdOpenExisting, faRead, fsReadWrite);
  try
    while True do begin
      var NumRead := F.Read(Buf, SizeOf(Buf));
      if NumRead = 0 then
        Break;
      SHA256Update(Context, Buf, NumRead);
    end;
  finally
    F.Free;
  end;
  Result := SHA256Final(Context);
end;

function GetSHA256OfAnsiString(const S: AnsiString): TSHA256Digest;
begin
  Result := SHA256Buf(Pointer(S)^, Length(S)*SizeOf(S[1]));
end;

function GetSHA256OfUnicodeString(const S: UnicodeString): TSHA256Digest;
begin
  Result := SHA256Buf(Pointer(S)^, Length(S)*SizeOf(S[1]));
end;

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
    Result := SfcIsFileProtectedFunc(0, PChar(FN));
  end
  else
    Result := False; { Should never happen }
end;

procedure HandleProcessWait(ProcessHandle: THandle; const Wait: TExecWait;
  const ProcessMessagesProc: TProcedure; const OutputReader: TCreateProcessOutputReader;
  var ResultCode: Integer);
begin
  try
    if Wait = ewWaitUntilIdle then begin
      repeat
        ProcessMessagesProc;
      until WaitForInputIdle(ProcessHandle, 50) <> WAIT_TIMEOUT;
    end;
    if Wait = ewWaitUntilTerminated then begin
      { Wait until the process returns, but still process any messages that
        arrive and read the output if requested. }
      var WaitMilliseconds := IfThen(OutputReader <> nil, 50, INFINITE);
      var WaitResult: DWORD := 0;
      repeat
        { Process any pending messages first because MsgWaitForMultipleObjects
          (called below) only returns when *new* messages arrive, unless there's
          a timeout }
        if WaitResult <> WAIT_TIMEOUT then
          ProcessMessagesProc;
        if OutputReader <> nil then
          OutputReader.Read(False);
        WaitResult := MsgWaitForMultipleObjects(1, ProcessHandle, False,
          WaitMilliseconds, QS_ALLINPUT);
      until (WaitResult <> WAIT_OBJECT_0+1) and (WaitResult <> WAIT_TIMEOUT);
      { Process messages once more in case MsgWaitForMultipleObjects saw the
        process terminate and new messages arrive simultaneously. (Can't leave
        unprocessed messages waiting, or a subsequent call to WaitMessage
        won't see them.) }
      ProcessMessagesProc;
      if OutputReader <> nil then
        OutputReader.Read(True);
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
  const ProcessMessagesProc: TProcedure; const OutputReader: TCreateProcessOutputReader;
  var ResultCode: Integer): Boolean;
var
  CmdLine: String;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  {Also see IsppFuncs' Exec }

  if Filename = '>' then
    CmdLine := Params
  else begin
    CmdLine := '"' + Filename + '"';
    if Params <> '' then
      CmdLine := CmdLine + ' ' + Params;
    if SameText(PathExtractExt(Filename), '.bat') or
       SameText(PathExtractExt(Filename), '.cmd') then begin
      { Use our own handling for .bat and .cmd files since passing them straight
        to CreateProcess on Windows NT 4.0 has problems: it doesn't properly
        quote the command line it passes to cmd.exe. This didn't work before:
          Filename: "c:\batch.bat"; Parameters: """abc"""
        And other Windows versions might have unknown quirks too, since
        CreateProcess isn't documented to accept .bat files in the first place. }
      { With cmd.exe, the whole command line must be quoted for quoted
        parameters to work. For example, this fails:
          cmd.exe /c "z:\blah.bat" "test"
        But this works:
          cmd.exe /c ""z:\blah.bat" "test""
      }
      CmdLine := '"' + AddBackslash(GetSystemDir) + 'cmd.exe" /C "' + CmdLine + '"'
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

  var InheritHandles := False;
  var dwCreationFlags: DWORD := CREATE_DEFAULT_ERROR_MODE;

  if (OutputReader <> nil) and (Wait = ewWaitUntilTerminated) then begin
    OutputReader.UpdateStartupInfo(StartupInfo);
    InheritHandles := True;
    dwCreationFlags := dwCreationFlags or CREATE_NO_WINDOW;
  end;

  Result := CreateProcessRedir(DisableFsRedir, nil, PChar(CmdLine), nil, nil,
    InheritHandles, dwCreationFlags, nil, PChar(WorkingDir),
    StartupInfo, ProcessInfo);
  if not Result then begin
    ResultCode := GetLastError;
    Exit;
  end;

  { Don't need the thread handle, so close it now }
  CloseHandle(ProcessInfo.hThread);
  if OutputReader <> nil then
    OutputReader.NotifyCreateProcessDone;
  HandleProcessWait(ProcessInfo.hProcess, Wait, ProcessMessagesProc,
    OutputReader, ResultCode);
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
    HandleProcessWait(Info.hProcess, Wait, ProcessMessagesProc, nil, ResultCode);
end;

function CheckForOrCreateMutexes(Mutexes: String; const Create: Boolean): Boolean;

  function MutexPos(const S: String): Integer;
  begin
    for var I := 1 to Length(S) do
      if (S[I] = ',') and ((I = 1) or (S[I-1] <> '\')) then
        Exit(I);
    Result := 0;
  end;

{ Returns True if any of the mutexes in the comma-separated Mutexes string
  exist and Create is False }
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
      if Create then begin
        CreateMutex(M)
      end else begin
        H := OpenMutex(SYNCHRONIZE, False, PChar(M));
        if H <> 0 then begin
          CloseHandle(H);
          Result := True;
          Break;
        end;
      end;
    end;
    Delete(Mutexes, 1, I);
  until Mutexes = '';
end;

function CheckForMutexes(const Mutexes: String): Boolean;
begin
  Result := CheckForOrCreateMutexes(Mutexes, False);
end;

procedure CreateMutexes(const Mutexes: String);
begin
  CheckForOrCreateMutexes(Mutexes, True);
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

function MakePendingFileRenameOperationsChecksum: TSHA256Digest;
{ Calculates a checksum of the current PendingFileRenameOperations registry
  value The caller can use this checksum to determine if
  PendingFileRenameOperations was changed (perhaps by another program). }
var
  Context: TSHA256Context;
  K: HKEY;
  S: String;
begin
  SHA256Init(Context);
  try
    if RegOpenKeyExView(rvDefault, HKEY_LOCAL_MACHINE, 'SYSTEM\CurrentControlSet\Control\Session Manager',
       0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      if RegQueryMultiStringValue(K, 'PendingFileRenameOperations', S) then
        SHA256Update(Context, S[1], Length(S)*SizeOf(S[1]));
      { When "PendingFileRenameOperations" is full, it spills over into
        "PendingFileRenameOperations2" }
      if RegQueryMultiStringValue(K, 'PendingFileRenameOperations2', S) then
        SHA256Update(Context, S[1], Length(S)*SizeOf(S[1]));
      RegCloseKey(K);
    end;
  except
    { don't propagate exceptions }
  end;
  Result := SHA256Final(Context);
end;

procedure EnumFileReplaceOperationsFilenames(const EnumFunc: TEnumFROFilenamesProc;
  Param: Pointer);
{ Enumerates all the filenames in the current PendingFileRenameOperations
  registry value or WININIT.INI file. The function does not distinguish between
  source and destination filenames; it enumerates both. }

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

procedure UnregisterFont(const FontName, FontFilename: String; const PerUserFont: Boolean);
var
  RootKey, K: HKEY;
begin
  if PerUserFont then
    RootKey := HKEY_CURRENT_USER
  else
    RootKey := HKEY_LOCAL_MACHINE;

  if RegOpenKeyExView(rvDefault, RootKey, 'Software\Microsoft\Windows NT\CurrentVersion\Fonts',
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
    'GetDiskFreeSpaceExW');
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
        { The result of GetDiskFreeSpace does not cap at 2GB, so we must use a
          64-bit multiply operation to avoid an overflow. }
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
  changed. Based on code from KB article 104011. }
var
  MsgResult: DWORD_PTR;
begin
  { Note: We originally used SendNotifyMessage to broadcast the message but it
    turned out that while it worked fine on NT 4 and 2000 it didn't work on XP
    -- the string "Environment" in lParam would be garbled on the receiving
    end (why I'm not exactly sure). We now use SendMessageTimeout as directed
    in the KB article 104011. It isn't as elegant since it could cause us to
    be delayed if another app is hung, but it'll have to do. }
  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
    LPARAM(PChar('Environment')), SMTO_ABORTIFHUNG, 5000, @MsgResult);
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

function ForceDirectories(const DisableFsRedir: Boolean; Dir: String): Boolean;
begin
  Dir := RemoveBackslashUnlessRoot(Dir);
  if (PathExtractPath(Dir) = Dir) or DirExistsRedir(DisableFsRedir, Dir) then
    Result := True
  else
    Result := ForceDirectories(DisableFsRedir, PathExtractPath(Dir)) and
      CreateDirectoryRedir(DisableFsRedir, Dir);
end;

function PathHasInvalidCharacters(const S: String;
  const AllowDriveLetterColon: Boolean): Boolean;
{ Checks the specified path for characters that are never allowed in paths,
  or characters and path components that are accepted by the system but might
  present a security problem (such as '..' and sometimes ':').
  Specifically, True is returned if S includes any of the following:
  - Control characters (0-31)
  - One of these characters: /*?"<>|
    (This means forward slashes and the prefixes '\\?\' and '\??\' are never
    allowed.)
  - Colons (':'), except when AllowDriveLetterColon=True and the string's
    first character is a letter and the second character is the only colon.
    (This blocks NTFS alternate data stream names.)
  - A component with a trailing dot or space

  Due to the last rule above, '.' and '..' components are never allowed, nor
  are components like these:
    'file '
    'file.'
    'file. . .'
    'file . . '
  When expanding paths (with no '\\?\' prefix used), Windows 11 23H2 silently
  removes all trailing dots and spaces from the end of the string. Therefore,
  if used at the end of a path, all of the above cases yield just 'file'.
  On preceding components of the path, nothing is done with spaces; if there
  is exactly one dot at the end, it is removed (e.g., 'dir.\file' becomes
  'dir\file'), while multiple dots are left untouched ('dir..\file' doesn't
  change).
  By rejecting trailing dots and spaces up front, we avoid all that weirdness
  and the problems that could arise from it.

  Since ':' is considered invalid (except in the one case noted above), it's
  not possible to sneak in disallowed dots/spaces by including an NTFS
  alternate data stream name. The function will return True in these cases:
    '..:streamname'
    'file :streamname'
}
begin
  Result := True;
  for var I := Low(S) to High(S) do begin
    var C := S[I];
    if Ord(C) < 32 then
      Exit;
    case C of
      #32, '.':
        begin
          if (I = High(S)) or PathCharIsSlash(S[I+1]) then
            Exit;
        end;
      ':':
        begin
          { The A-Z check ensures that '.:streamname', ' :streamname', and
            '\:streamname' are disallowed. }
          if not AllowDriveLetterColon or (I <> Low(S)+1) or
             not CharInSet(S[Low(S)], ['A'..'Z', 'a'..'z']) then
            Exit;
        end;
      '/', '*', '?', '"', '<', '>', '|': Exit;
    end;
  end;
  Result := False;
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
