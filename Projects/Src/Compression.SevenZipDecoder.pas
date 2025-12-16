unit Compression.SevenZipDecoder;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Interface to the 7-Zip Decoder OBJ in Compression.SevenZipDecoder\7ZipDecode,
  used by Setup.
}

interface

uses
  SysUtils;

type
  TOnExtractionProgress = function(const ArchiveName, FileName: string; const Progress, ProgressMax: Int64): Boolean of object;

  ESevenZipError = class(Exception);

procedure SevenZipError(const ExceptMessage: String; const LogMessage: String = '');

procedure Extract7ZipArchiveRedir(const DisableFsRedir: Boolean;
  const ArchiveFileName, DestDir, Password: String; const FullPaths: Boolean;
  const OnExtractionProgress: TOnExtractionProgress);

implementation

uses
  Windows, Forms,
  PathFunc, UnsignedFunc,
  Shared.SetupMessageIDs, Shared.CommonFunc, SetupLdrAndSetup.Messages,
  Setup.RedirFunc, Setup.LoggingFunc, Setup.MainFunc, Setup.InstFunc;

type
  TSevenZipDecodeState = record
    DisableFsRedir: Boolean;
    ExpandedArchiveFileName, ExpandedDestDir: String;
    LogBuffer: AnsiString;
    ExtractedArchiveName: String;
    OnExtractionProgress: TOnExtractionProgress;
    LastReportedProgress, LastReportedProgressMax: UInt64;
    Aborted: Boolean;
  end;

var
  State: TSevenZipDecodeState;

{ Compiled by Visual Studio 2022 using compile.bat }
{$IFNDEF WIN64}
{ To enable source debugging recompile using compile-bcc32c.bat and turn off the VISUALSTUDIO define below
  Note that in a speed test the code produced by bcc32c was about 33% slower }
{$L Src\Compression.SevenZipDecoder\7zDecode\IS7zDec-x86.obj}
{$ELSE}
{$L Src\Compression.SevenZipDecoder\7zDecode\IS7zDec-x64.obj}
{$ENDIF}

{$DEFINE VISUALSTUDIO}

function IS_7zDec(const fileName: PChar; const fullPaths: Bool): Integer; {$IFNDEF WIN64} cdecl; external name '_IS_7zDec'; {$ELSE} external name 'IS_7zDec'; {$ENDIF}

{$IFNDEF WIN64}
function __CreateDirectoryW(lpPathName: LPCWSTR;
  lpSecurityAttributes: PSecurityAttributes): BOOL; cdecl;
{$ELSE}
function _CreateDirectoryW(lpPathName: LPCWSTR;
  lpSecurityAttributes: PSecurityAttributes): BOOL;
{$ENDIF}
begin
  var ExpandedDir: String;
  if ValidateAndCombinePath(State.ExpandedDestDir, lpPathName, ExpandedDir) then
    Result := CreateDirectoryRedir(State.DisableFsRedir, ExpandedDir, lpSecurityAttributes)
  else begin
    Result := False;
    SetLastError(ERROR_ACCESS_DENIED);
  end;
end;

{ Never actually called but still required by the linker }
{$IFNDEF WIN64}
function __CreateFileA(lpFileName: LPCSTR; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; cdecl;
{$ELSE}
function _CreateFileA(lpFileName: LPCSTR; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle;
{$ENDIF}
begin
  { Return an error if we do ever get called which is unwanted because it should
    use CreateFileW and not CreateFileA }
  Result := INVALID_HANDLE_VALUE;
  SetLastError(ERROR_INVALID_FUNCTION);
end;

{$IFNDEF WIN64}
function __CreateFileW(lpFileName: LPCWSTR; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; cdecl;
{$ELSE}
function _CreateFileW(lpFileName: LPCWSTR; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle;
{$ENDIF}
begin
  { Filenames read from archives aren't validated at all by the SDK's 7zMain.c,
    so we have to handle that ourself. Most importantly, we need to make sure a
    malicious archive cannot create files outside of the destination directory. }
  var ExpandedFileName: String;
  if ((dwDesiredAccess = GENERIC_READ) and
      PathExpand(lpFileName, ExpandedFileName) and
      (PathCompare(ExpandedFileName, State.ExpandedArchiveFileName) = 0)) or
     ((dwDesiredAccess = GENERIC_WRITE) and
      ValidateAndCombinePath(State.ExpandedDestDir, lpFileName, ExpandedFileName)) then
    Result := CreateFileRedir(State.DisableFsRedir, ExpandedFileName,
      dwDesiredAccess, dwShareMode, lpSecurityAttributes,
      dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile)
  else begin
    Result := INVALID_HANDLE_VALUE;
    SetLastError(ERROR_ACCESS_DENIED);
  end;
end;

{$IFDEF VISUALSTUDIO}

{$IFNDEF WIN64}
function __FileTimeToLocalFileTime(lpFileTime: PFileTime; var lpLocalFileTime: TFileTime): BOOL; cdecl;
{$ELSE}
function _FileTimeToLocalFileTime(lpFileTime: PFileTime; var lpLocalFileTime: TFileTime): BOOL;
{$ENDIF}
begin
  Result := FileTimeToLocalFileTime(lpFileTime, lpLocalFileTime);
end;

{ Never actually called but still required by the linker }
{$IFNDEF WIN64}
function __GetFileSize(hFile: THandle; lpFileSizeHigh: Pointer): DWORD; cdecl;
{$ELSE}
function _GetFileSize(hFile: THandle; lpFileSizeHigh: Pointer): DWORD;
{$ENDIF}
begin
  Result := GetFileSize(hFile, lpFileSizeHigh);
end;

{$IFNDEF WIN64}
function __ReadFile(hFile: THandle; var Buffer; nNumberOfBytesToRead: DWORD;
  var lpNumberOfBytesRead: DWORD; lpOverlapped: POverlapped): BOOL; cdecl;
{$ELSE}
function _ReadFile(hFile: THandle; var Buffer; nNumberOfBytesToRead: DWORD;
  var lpNumberOfBytesRead: DWORD; lpOverlapped: POverlapped): BOOL;
{$ENDIF}
begin
  Result := ReadFile(hFile, Buffer, nNumberOfBytesToRead, lpNumberOfBytesRead, lpOverlapped);
end;

{$IFNDEF WIN64}
function __GetFileAttributesW(lpFileName: LPCWSTR): DWORD; cdecl;
{$ELSE}
function _GetFileAttributesW(lpFileName: LPCWSTR): DWORD;
{$ENDIF}
begin
  { See above }
  var ExpandedFileName: String;
  if ValidateAndCombinePath(State.ExpandedDestDir, lpFileName, ExpandedFileName) then
    Result := GetFileAttributesRedir(State.DisableFsRedir, ExpandedFileName)
  else begin
    Result := INVALID_FILE_ATTRIBUTES;
    SetLastError(ERROR_ACCESS_DENIED);
  end;
end;

{$IFNDEF WIN64}
function __SetFileAttributesW(lpFileName: LPCWSTR; dwFileAttributes: DWORD): BOOL; cdecl;
{$ELSE}
function _SetFileAttributesW(lpFileName: LPCWSTR; dwFileAttributes: DWORD): BOOL;
{$ENDIF}
begin
  { See above }
  var ExpandedFileName: String;
  if ValidateAndCombinePath(State.ExpandedDestDir, lpFileName, ExpandedFileName) then
    Result := SetFileAttributesRedir(State.DisableFsRedir, ExpandedFileName, dwFileAttributes)
  else begin
    Result := False;
    SetLastError(ERROR_ACCESS_DENIED);
  end;
end;

{$IFNDEF WIN64}
function __SetFilePointer(hFile: THandle; lDistanceToMove: Longint;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: DWORD): DWORD; cdecl;
{$ELSE}
function _SetFilePointer(hFile: THandle; lDistanceToMove: Longint;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: DWORD): DWORD;
{$ENDIF}
begin
  Result := SetFilePointer(hFile, lDistanceToMove, lpDistanceToMoveHigh, dwMoveMethod);
end;

{$IFNDEF WIN64}
function __SetFileTime(hFile: THandle;
  lpCreationTime, lpLastAccessTime, lpLastWriteTime: PFileTime): BOOL; cdecl;
{$ELSE}
function _SetFileTime(hFile: THandle;
  lpCreationTime, lpLastAccessTime, lpLastWriteTime: PFileTime): BOOL;
{$ENDIF}
begin
  Result := SetFileTime(hFile, lpCreationTime, lpLastAccessTime, lpLastWriteTime);
end;

{$IFNDEF WIN64}
function __WriteFile(hFile: THandle; const Buffer; nNumberOfBytesToWrite: DWORD;
  var lpNumberOfBytesWritten: DWORD; lpOverlapped: POverlapped): BOOL; cdecl;
{$ELSE}
function _WriteFile(hFile: THandle; const Buffer; nNumberOfBytesToWrite: DWORD;
  var lpNumberOfBytesWritten: DWORD; lpOverlapped: POverlapped): BOOL;
{$ENDIF}
begin
  Result := WriteFile(hFile, Buffer, nNumberOfBytesToWrite, lpNumberOfBytesWritten, lpOverlapped);
end;

{$IFNDEF WIN64}
function __CloseHandle(hObject: THandle): BOOL; cdecl;
{$ELSE}
function _CloseHandle(hObject: THandle): BOOL;
{$ENDIF}
begin
  Result := CloseHandle(hObject);
end;

{$IFNDEF WIN64}
function __GetLastError: DWORD; cdecl;
{$ELSE}
function _GetLastError: DWORD;
{$ENDIF}
begin
  Result := GetLastError;
end;

{$IFNDEF WIN64}
function __LocalFree(hMem: HLOCAL): HLOCAL; cdecl;
{$ELSE}
function _LocalFree(hMem: HLOCAL): HLOCAL;
{$ENDIF}
begin
  Result := LocalFree(hMem);
end;

{$IFNDEF WIN64}
function __FormatMessageA(dwFlags: DWORD; lpSource: Pointer; dwMessageId: DWORD; dwLanguageId: DWORD;
  lpBuffer: LPSTR; nSize: DWORD; Arguments: Pointer): DWORD; cdecl;
{$ELSE}
function _FormatMessageA(dwFlags: DWORD; lpSource: Pointer; dwMessageId: DWORD; dwLanguageId: DWORD;
  lpBuffer: LPSTR; nSize: DWORD; Arguments: Pointer): DWORD;
{$ENDIF}
begin
  Result := FormatMessageA(dwFlags, lpSource, dwMessageId, dwLanguageId, lpBuffer, nSize, Arguments);
end;

{$IFNDEF WIN64}
function __WideCharToMultiByte(CodePage: UINT; dwFlags: DWORD;
  lpWideCharStr: LPWSTR; cchWideChar: Integer; lpMultiByteStr: LPSTR;
  cchMultiByte: Integer; lpDefaultChar: LPCSTR; lpUsedDefaultChar: PBOOL): Integer; cdecl;
{$ELSE}
function _WideCharToMultiByte(CodePage: UINT; dwFlags: DWORD;
  lpWideCharStr: LPWSTR; cchWideChar: Integer; lpMultiByteStr: LPSTR;
  cchMultiByte: Integer; lpDefaultChar: LPCSTR; lpUsedDefaultChar: PBOOL): Integer;
{$ENDIF}
begin
  Result := WideCharToMultiByte(CodePage, dwFlags, lpWideCharStr, cchWideChar, lpMultiByteStr, cchMultiByte, lpDefaultChar, lpUsedDefaultChar);
end;

{$IFNDEF WIN64}
//https://github.com/rust-lang/compiler-builtins/issues/403
procedure __allshl; register; external 'ntdll.dll' name '_allshl';
procedure __aullshr; register; external 'ntdll.dll' name '_aullshr';
{$ENDIF}
{$ELSE}
procedure __aullrem; stdcall; external 'ntdll.dll' name '_aullrem';
procedure __aulldiv; stdcall; external 'ntdll.dll' name '_aulldiv';
{$ENDIF}

{$IFNDEF WIN64}
function _memcpy(dest, src: Pointer; n: NativeUInt): Pointer; cdecl;
{$ELSE}
function memcpy(dest, src: Pointer; n: NativeUInt): Pointer;
{$ENDIF}
begin
  UMove(src^, dest^, n);
  Result := dest;
end;

{$IFNDEF WIN64}
function _memset(dest: Pointer; c: Integer; n: NativeUInt): Pointer; cdecl;
{$ELSE}
function memset(dest: Pointer; c: Integer; n: NativeUInt): Pointer;
{$ENDIF}
begin
  UFillChar(dest^, n, c);
  Result := dest;
end;

{$IFNDEF WIN64}
function _malloc(size: NativeUInt): Pointer; cdecl;
{$ELSE}
function malloc(size: NativeUInt): Pointer;
{$ENDIF}
begin
  if size > NativeUInt(High(NativeInt)) then
    Result := nil
  else begin
    try
      GetMem(Result, NativeInt(size));
    except
      on EOutOfMemory do
        Result := nil;
    end;
  end;
end;

{$IFNDEF WIN64}
procedure _free(address: Pointer); cdecl;
{$ELSE}
procedure free(address: Pointer);
{$ENDIF}
begin
  FreeMem(address);
end;

{$IFNDEF WIN64}
function _wcscmp(string1, string2: PChar): Integer; cdecl;
{$ELSE}
function wcscmp(string1, string2: PChar): Integer;
{$ENDIF}
begin
  Result := StrComp(string1, string2);
end;

procedure Log(const S: AnsiString);
begin
  if S <> '' then
    Setup.LoggingFunc.Log(UTF8ToString(S));
end;

{$IFNDEF WIN64}
function __fputs(str: PAnsiChar; unused: Pointer): Integer; cdecl;
{$ELSE}
function _fputs(str: PAnsiChar; unused: Pointer): Integer;
{$ENDIF}

  function FindNewLine(const S: AnsiString): Integer;
  begin
    { 7zMain.c always sends #10 as newline but its call to FormatMessage can cause #13#10 anyway  }
    var N := Length(S);
    for var I := 1 to N do
      if CharInSet(S[I], [#13, #10]) then
        Exit(I);
    Result := 0;
  end;

begin
  try
    State.LogBuffer := State.LogBuffer + str;
    var P := FindNewLine(State.LogBuffer);
    while P <> 0 do begin
      Log(Copy(State.LogBuffer, 1, P-1));
      if (State.LogBuffer[P] = #13) and (P < Length(State.LogBuffer)) and (State.LogBuffer[P+1] = #10) then
        Inc(P);
      Delete(State.LogBuffer, 1, P);
      P := FindNewLine(State.LogBuffer);
    end;
    Result := 0;
  except
    Result := -1; { EOF }
  end;
end;

{$IFNDEF WIN64}
procedure _ReportProgress(const FileName: PChar; const Progress, ProgressMax: UInt64; var Abort: Bool); cdecl;
{$ELSE}
procedure ReportProgress(const FileName: PChar; const Progress, ProgressMax: UInt64; var Abort: Bool);
{$ENDIF}
begin
  try
    if Assigned(State.OnExtractionProgress) then
      if not State.OnExtractionProgress(State.ExtractedArchiveName, FileName, Progress, ProgressMax) then
        Abort := True;

    if not Abort and DownloadTemporaryFileOrExtractArchiveProcessMessages then
      Application.ProcessMessages;
  except
    Abort := True;
  end;

  if Abort then
    State.Aborted := True;
end;

procedure SevenZipError(const ExceptMessage, LogMessage: String);
{ LogMessage may be non-localized or empty but ExceptMessage may be neither.
  ExceptMessage should not already contain msgErrorExtractionFailed.
  Should not be called from a secondary thread if LogMessage is not empty. }
begin
  if LogMessage <> '' then
    LogFmt('ERROR: %s', [LogMessage]); { Just like 7zMain.c }
  raise ESevenZipError.Create(ExceptMessage);
end;

procedure Extract7ZipArchiveRedir(const DisableFsRedir: Boolean;
  const ArchiveFileName, DestDir, Password: String; const FullPaths: Boolean;
  const OnExtractionProgress: TOnExtractionProgress);

  procedure BadResultError(const Res: Integer);
  const
    SZ_ERROR_DATA = 1;
    SZ_ERROR_MEM = 2;
    SZ_ERROR_CRC = 3;
    SZ_ERROR_UNSUPPORTED = 4;
    SZ_ERROR_ARCHIVE = 16;
    SZ_ERROR_NO_ARCHIVE = 17;
  begin
    { Logging already done by 7zMain.c }

    case Res of
      SZ_ERROR_UNSUPPORTED, SZ_ERROR_NO_ARCHIVE:
        SevenZipError(SetupMessages[msgArchiveUnsupportedFormat]);
      SZ_ERROR_DATA, SZ_ERROR_CRC, SZ_ERROR_ARCHIVE:
        SevenZipError(SetupMessages[msgArchiveIsCorrupted]);
      SZ_ERROR_MEM:
        SevenZipError(Win32ErrorString(DWORD(E_OUTOFMEMORY)));
    else
      SevenZipError(Res.ToString);
    end;
  end;

begin
  LogArchiveExtractionModeOnce;

  if ArchiveFileName = '' then
    InternalError('Extract7ZipArchive: Invalid ArchiveFileName value');
  if DestDir = '' then
    InternalError('Extract7ZipArchive: Invalid DestDir value');
  if Password <> '' then
    InternalError('Extract7ZipArchive: Invalid Password value');

  LogFmt('Extracting 7-Zip archive %s to %s. Full paths? %s', [ArchiveFileName,
    RemoveBackslashUnlessRoot(DestDir), SYesNo[FullPaths]]);

  if not ForceDirectories(DisableFsRedir, DestDir) then
    SevenZipError(FmtSetupMessage1(msgErrorCreatingDir, DestDir), 'Failed to create destination directory');

  State.DisableFsRedir := DisableFsRedir;
  State.ExpandedArchiveFileName := PathExpand(ArchiveFileName);
  State.ExpandedDestDir := AddBackslash(PathExpand(DestDir));
  State.LogBuffer := '';
  State.ExtractedArchiveName := PathExtractName(ArchiveFileName);
  State.OnExtractionProgress := OnExtractionProgress;
  State.LastReportedProgress := 0;
  State.LastReportedProgressMax := 0;
  State.Aborted := False;

  var Res := IS_7zDec(PChar(ArchiveFileName), FullPaths);

  if State.LogBuffer <> '' then
    Log(State.LogBuffer);

  if State.Aborted then
    Abort
  else if Res <> 0 then
    BadResultError(Res);
end;

end.
