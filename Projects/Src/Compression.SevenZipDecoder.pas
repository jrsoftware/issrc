unit Compression.SevenZipDecoder;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
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

procedure Extract7ZipArchive(const ArchiveFileName, DestDir, Password: String; const FullPaths: Boolean;
  const OnExtractionProgress: TOnExtractionProgress);

implementation

uses
  Windows, Forms,
  PathFunc, UnsignedFunc,
  Shared.SetupMessageIDs, Shared.CommonFunc, SetupLdrAndSetup.Messages,
  Setup.LoggingFunc, Setup.MainFunc, Setup.InstFunc;

type
  TSevenZipDecodeState = record
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

function IS_7zDec(const fileName: PChar; const fullPaths: Bool): Integer; cdecl; external name {$IFNDEF WIN64} '_IS_7zDec' {$ELSE} 'IS_7zDec' {$ENDIF};


function {$IFNDEF WIN64} __CreateDirectoryW {$ELSE} _CreateDirectoryW {$ENDIF}(
  lpPathName: LPCWSTR;
  lpSecurityAttributes: PSecurityAttributes): BOOL; cdecl;
begin
  var ExpandedDir: String;
  if ValidateAndCombinePath(State.ExpandedDestDir, lpPathName, ExpandedDir) then
    Result := CreateDirectory(PChar(ExpandedDir), lpSecurityAttributes)
  else begin
    Result := False;
    SetLastError(ERROR_ACCESS_DENIED);
  end;
end;

{ Never actually called but still required by the linker }
function {$IFNDEF WIN64} __CreateFileA {$ELSE} _CreateFileA {$ENDIF}(
  lpFileName: LPCSTR; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; cdecl;
begin
  { Return an error if we do ever get called which is unwanted because it should
    use CreateFileW and not CreateFileA }
  Result := INVALID_HANDLE_VALUE;
  SetLastError(ERROR_INVALID_FUNCTION);
end;

function {$IFNDEF WIN64} __CreateFileW {$ELSE} _CreateFileW {$ENDIF}(
  lpFileName: LPCWSTR; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; cdecl;
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
    Result := CreateFile(PChar(ExpandedFileName), dwDesiredAccess, dwShareMode, lpSecurityAttributes,
      dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile)
  else begin
    Result := INVALID_HANDLE_VALUE;
    SetLastError(ERROR_ACCESS_DENIED);
  end;
end;

{$IFDEF VISUALSTUDIO}

function {$IFNDEF WIN64} __FileTimeToLocalFileTime {$ELSE} _FileTimeToLocalFileTime {$ENDIF}(
  lpFileTime: PFileTime; var lpLocalFileTime: TFileTime): BOOL; cdecl;
begin
  Result := FileTimeToLocalFileTime(lpFileTime, lpLocalFileTime);
end;

{ Never actually called but still required by the linker }
function {$IFNDEF WIN64} __GetFileSize {$ELSE} _GetFileSize {$ENDIF}(
  hFile: THandle; lpFileSizeHigh: Pointer): DWORD; cdecl;
begin
  Result := GetFileSize(hFile, lpFileSizeHigh);
end;

function {$IFNDEF WIN64} __ReadFile {$ELSE} _ReadFile {$ENDIF}(
  hFile: THandle; var Buffer; nNumberOfBytesToRead: DWORD;
  var lpNumberOfBytesRead: DWORD; lpOverlapped: POverlapped): BOOL; cdecl;
begin
  Result := ReadFile(hFile, Buffer, nNumberOfBytesToRead, lpNumberOfBytesRead, lpOverlapped);
end;

function {$IFNDEF WIN64} __GetFileAttributesW {$ELSE} _GetFileAttributesW {$ENDIF}(
  lpFileName: LPCWSTR): DWORD; cdecl;
begin
  { See above }
  var ExpandedFileName: String;
  if ValidateAndCombinePath(State.ExpandedDestDir, lpFileName, ExpandedFileName) then
    Result := GetFileAttributes(PChar(ExpandedFileName))
  else begin
    Result := INVALID_FILE_ATTRIBUTES;
    SetLastError(ERROR_ACCESS_DENIED);
  end;
end;

function {$IFNDEF WIN64} __SetFileAttributesW {$ELSE} _SetFileAttributesW {$ENDIF}(
  lpFileName: LPCWSTR; dwFileAttributes: DWORD): BOOL; cdecl;
begin
  { See above }
  var ExpandedFileName: String;
  if ValidateAndCombinePath(State.ExpandedDestDir, lpFileName, ExpandedFileName) then
    Result := SetFileAttributes(PChar(ExpandedFileName), dwFileAttributes)
  else begin
    Result := False;
    SetLastError(ERROR_ACCESS_DENIED);
  end;
end;

function {$IFNDEF WIN64} __SetFilePointer {$ELSE} _SetFilePointer {$ENDIF}(
  hFile: THandle; lDistanceToMove: Longint;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: DWORD): DWORD; cdecl;
begin
  Result := SetFilePointer(hFile, lDistanceToMove, lpDistanceToMoveHigh, dwMoveMethod);
end;

function {$IFNDEF WIN64} __SetFileTime {$ELSE} _SetFileTime {$ENDIF}(
  hFile: THandle;
  lpCreationTime, lpLastAccessTime, lpLastWriteTime: PFileTime): BOOL; cdecl;
begin
  Result := SetFileTime(hFile, lpCreationTime, lpLastAccessTime, lpLastWriteTime);
end;

function {$IFNDEF WIN64} __WriteFile {$ELSE} _WriteFile {$ENDIF}(
  hFile: THandle; const Buffer; nNumberOfBytesToWrite: DWORD;
  var lpNumberOfBytesWritten: DWORD; lpOverlapped: POverlapped): BOOL; cdecl;
begin
  Result := WriteFile(hFile, Buffer, nNumberOfBytesToWrite, lpNumberOfBytesWritten, lpOverlapped);
end;

function {$IFNDEF WIN64} __CloseHandle {$ELSE} _CloseHandle {$ENDIF}(
  hObject: THandle): BOOL; cdecl;
begin
  Result := CloseHandle(hObject);
end;

function {$IFNDEF WIN64} __GetLastError {$ELSE} _GetLastError {$ENDIF}: DWORD; cdecl;
begin
  Result := GetLastError;
end;

function {$IFNDEF WIN64} __LocalFree {$ELSE} _LocalFree {$ENDIF}(
  hMem: HLOCAL): HLOCAL; cdecl;
begin
  Result := LocalFree(hMem);
end;

function {$IFNDEF WIN64} __FormatMessageA {$ELSE} _FormatMessageA {$ENDIF}(
  dwFlags: DWORD; lpSource: Pointer; dwMessageId: DWORD; dwLanguageId: DWORD;
  lpBuffer: LPSTR; nSize: DWORD; Arguments: Pointer): DWORD; cdecl;
begin
  Result := FormatMessageA(dwFlags, lpSource, dwMessageId, dwLanguageId, lpBuffer, nSize, Arguments);
end;

function {$IFNDEF WIN64} __WideCharToMultiByte {$ELSE} _WideCharToMultiByte {$ENDIF}(
  CodePage: UINT; dwFlags: DWORD; lpWideCharStr: LPWSTR; cchWideChar: Integer; lpMultiByteStr: LPSTR;
  cchMultiByte: Integer; lpDefaultChar: LPCSTR; lpUsedDefaultChar: PBOOL): Integer; cdecl;
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

function {$IFNDEF WIN64} _memcpy {$ELSE} memcpy {$ENDIF}(
  dest, src: Pointer; n: NativeUInt): Pointer; cdecl;
begin
  UMove(src^, dest^, n);
  Result := dest;
end;

function {$IFNDEF WIN64} _memset {$ELSE} memset {$ENDIF}(
  dest: Pointer; c: Integer; n: NativeUInt): Pointer; cdecl;
begin
  UFillChar(dest^, n, c);
  Result := dest;
end;

function {$IFNDEF WIN64} _malloc {$ELSE} malloc {$ENDIF}(
  size: NativeUInt): Pointer; cdecl;
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

procedure {$IFNDEF WIN64} _free {$ELSE} free {$ENDIF}(address: Pointer); cdecl;
begin
  FreeMem(address);
end;

function {$IFNDEF WIN64} _wcscmp {$ELSE} wcscmp {$ENDIF}(
  string1, string2: PChar): Integer; cdecl;
begin
  Result := StrComp(string1, string2);
end;

procedure Log(const S: AnsiString);
begin
  if S <> '' then
    Setup.LoggingFunc.Log(UTF8ToString(S));
end;

function {$IFNDEF WIN64} __fputs {$ELSE} _fputs {$ENDIF}(
  str: PAnsiChar; unused: Pointer): Integer; cdecl;

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

procedure {$IFNDEF WIN64} _ReportProgress {$ELSE} ReportProgress {$ENDIF}(
  const FileName: PChar; const Progress, ProgressMax: UInt64; var Abort: Bool); cdecl;
begin
  try
    if Assigned(State.OnExtractionProgress) then begin
      const MaxInt64 = High(Int64);
      var ReportProgress, ReportProgressMax: Int64;
      if Progress > MaxInt64 then
        ReportProgress := MaxInt64
      else
        ReportProgress := Int64(Progress);
      if ProgressMax > MaxInt64 then
        ReportProgressMax := MaxInt64
      else
        ReportProgressMax := Int64(ProgressMax);
      if not State.OnExtractionProgress(State.ExtractedArchiveName, FileName, ReportProgress, ReportProgressMax) then
        Abort := True;
    end;

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

procedure Extract7ZipArchive(const ArchiveFileName, DestDir, Password: String; const FullPaths: Boolean;
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

  if not ForceDirectories(False, DestDir) then
    SevenZipError(FmtSetupMessage1(msgErrorCreatingDir, DestDir), 'Failed to create destination directory');

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
