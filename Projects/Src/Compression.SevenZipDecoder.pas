unit Compression.SevenZipDecoder;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Interface to the 7-Zip Decoder OBJ in Compression.SevenZipDecoder\7ZipDecode,
  used by Setup.
}

interface

type
  TOnExtractionProgress = function(const ArchiveName, FileName: string; const Progress, ProgressMax: Int64): Boolean of object;

procedure Extract7ZipArchive(const ArchiveFileName, DestDir: String;
  const FullPaths: Boolean; const OnExtractionProgress: TOnExtractionProgress);

implementation

uses
  Windows, SysUtils, Forms,
  PathFunc,
  Shared.SetupMessageIDs, SetupLdrAndSetup.Messages, Setup.LoggingFunc, Setup.MainFunc, Setup.InstFunc;

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

{ Compiled by Visual Studio 2022 using compile.bat
  To enable source debugging recompile using compile-bcc32c.bat and turn off the VISUALSTUDIO define below
  Note that in a speed test the code produced by bcc32c was about 33% slower }
{$L Src\Compression.SevenZipDecoder\7zDecode\IS7zDec.obj}
{$DEFINE VISUALSTUDIO}

function IS_7zDec(const fileName: PChar; const fullPaths: Bool): Integer; cdecl; external name '_IS_7zDec';

function ValidateAndCombinePath(const ADestDir, AFilename: String;
  out AResultingPath: String): Boolean;
{ Filenames read from archives aren't validated at all by the SDK's 7zMain.c,
  so we have to handle that ourself. Most importantly, we need to make sure a
  malicious archive cannot create files outside of the destination directory.
  Returns True if all security checks pass, with the combination of ADestDir
  and AFilename in AResultingPath.
  ADestDir is assumed to be normalized already and have a trailing backslash.
  AFilename may be a file or directory name. }
begin
  { - Don't allow empty names
    - Don't allow forward slashes or repeated slashes
      (archives use '/' on disk but 7zMain.c changes them to '\')
    - Don't allow rooted (non-relative to current directory) names
    - Don't allow trailing slash
    - Don't allow invalid characters/dots/spaces (this catches '..') }
  Result := False;
  if (AFilename <> '') and
     (AFilename = PathNormalizeSlashes(AFilename)) and
     not PathIsRooted(AFilename) and
     not PathCharIsSlash(AFilename[High(AFilename)]) and
     not PathHasInvalidCharacters(AFilename, False) then begin
    { Our validity checks passed. Now pass the combined path to PathExpand
      (GetFullPathName) to see if it thinks the path needs normalization.
      If the returned path isn't exactly what was passed in, then consider
      the name invalid.
      One way that can happen is if the path ends in an MS-DOS device name:
      PathExpand('c:\path\NUL') returns '\\.\NUL'. Obviously we don't want
      devices being opened, so that must be rejected. }
    var CombinedPath := ADestDir + AFilename;
    var TestExpandedPath: String;
    if PathExpand(CombinedPath, TestExpandedPath) and
       (CombinedPath = TestExpandedPath) then begin
      AResultingPath := CombinedPath;
      Result := True;
    end;
  end;
end;

function __CreateDirectoryW(lpPathName: LPCWSTR;
  lpSecurityAttributes: PSecurityAttributes): BOOL; cdecl;
begin
  var ExpandedDir: String;
  if ValidateAndCombinePath(State.ExpandedDestDir, lpPathName, ExpandedDir) then
    Result := CreateDirectoryW(PChar(ExpandedDir), lpSecurityAttributes)
  else begin
    Result := False;
    SetLastError(ERROR_ACCESS_DENIED);
  end;
end;

{ Never actually called but still required by the linker }
function __CreateFileA(lpFileName: LPCSTR; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; cdecl;
begin
  { Return an error if we do ever get called which is unwanted because it should
    use CreateFileW and not CreateFileA }
  Result := INVALID_HANDLE_VALUE;
  SetLastError(ERROR_INVALID_FUNCTION);
end;

function __CreateFileW(lpFileName: LPCWSTR; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; cdecl;
begin
  var ExpandedFileName: String;
  if ((dwDesiredAccess = GENERIC_READ) and
      PathExpand(lpFileName, ExpandedFileName) and
      (PathCompare(ExpandedFileName, State.ExpandedArchiveFileName) = 0)) or
     ((dwDesiredAccess = GENERIC_WRITE) and
      ValidateAndCombinePath(State.ExpandedDestDir, lpFileName, ExpandedFileName)) then
    Result := CreateFileW(PChar(ExpandedFileName), dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile)
  else begin
    Result := INVALID_HANDLE_VALUE;
    SetLastError(ERROR_ACCESS_DENIED);
  end;
end;

{$IFDEF VISUALSTUDIO}

function __FileTimeToLocalFileTime(lpFileTime: PFileTime; var lpLocalFileTime: TFileTime): BOOL; cdecl;
begin
  Result := FileTimeToLocalFileTime(lpFileTime, lpLocalFileTime);
end;

{ Never actually called but still required by the linker }
function __GetFileSize(hFile: THandle; lpFileSizeHigh: Pointer): DWORD; cdecl;
begin
  Result := GetFileSize(hFile, lpFileSizeHigh);
end;

function __ReadFile(hFile: THandle; var Buffer; nNumberOfBytesToRead: DWORD;
  var lpNumberOfBytesRead: DWORD; lpOverlapped: POverlapped): BOOL; cdecl;
begin
  Result := ReadFile(hFile, Buffer, nNumberOfBytesToRead, lpNumberOfBytesRead, lpOverlapped);
end;

function __SetFileAttributesW(lpFileName: LPCWSTR; dwFileAttributes: DWORD): BOOL; cdecl;
begin
  Result := SetFileAttributesW(lpFileName, dwFileAttributes);
end;

function __SetFilePointer(hFile: THandle; lDistanceToMove: Longint;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: DWORD): DWORD; cdecl;
begin
  Result := SetFilePointer(hFile, lDistanceToMove, lpDistanceToMoveHigh, dwMoveMethod);
end;

function __SetFileTime(hFile: THandle;
  lpCreationTime, lpLastAccessTime, lpLastWriteTime: PFileTime): BOOL; cdecl;
begin
  Result := SetFileTime(hFile, lpCreationTime, lpLastAccessTime, lpLastWriteTime);
end;

function __WriteFile(hFile: THandle; const Buffer; nNumberOfBytesToWrite: DWORD;
  var lpNumberOfBytesWritten: DWORD; lpOverlapped: POverlapped): BOOL; cdecl;
begin
  Result := WriteFile(hFile, Buffer, nNumberOfBytesToWrite, lpNumberOfBytesWritten, lpOverlapped);
end;

function __CloseHandle(hObject: THandle): BOOL; cdecl;
begin
  Result := CloseHandle(hObject);
end;

function __GetLastError: DWORD; cdecl;
begin
  Result := GetLastError;
end;

function __LocalFree(hMem: HLOCAL): HLOCAL; cdecl;
begin
  Result := LocalFree(hMem);
end;

function __FormatMessageA(dwFlags: DWORD; lpSource: Pointer; dwMessageId: DWORD; dwLanguageId: DWORD;
  lpBuffer: LPSTR; nSize: DWORD; Arguments: Pointer): DWORD; cdecl;
begin
  Result := FormatMessageA(dwFlags, lpSource, dwMessageId, dwLanguageId, lpBuffer, nSize, Arguments);
end;

function __WideCharToMultiByte(CodePage: UINT; dwFlags: DWORD;
  lpWideCharStr: LPWSTR; cchWideChar: Integer; lpMultiByteStr: LPSTR;
  cchMultiByte: Integer; lpDefaultChar: LPCSTR; lpUsedDefaultChar: PBOOL): Integer; cdecl;
begin
  Result := WideCharToMultiByte(CodePage, dwFlags, lpWideCharStr, cchWideChar, lpMultiByteStr, cchMultiByte, lpDefaultChar, lpUsedDefaultChar);
end;

//https://github.com/rust-lang/compiler-builtins/issues/403
procedure __allshl; register; external 'ntdll.dll' name '_allshl';
procedure __aullshr; register; external 'ntdll.dll' name '_aullshr';
{$ELSE}
procedure __aullrem; stdcall; external 'ntdll.dll' name '_aullrem';
procedure __aulldiv; stdcall; external 'ntdll.dll' name '_aulldiv';
{$ENDIF}

function _memcpy(dest, src: Pointer; n: Cardinal): Pointer; cdecl;
begin
  Move(src^, dest^, n);
  Result := dest;
end;

function _memset(dest: Pointer; c: Integer; n: Cardinal): Pointer; cdecl;
begin
  FillChar(dest^, n, c);
  Result := dest;
end;

function _malloc(size: NativeUInt): Pointer; cdecl;
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

procedure _free(address: Pointer); cdecl;
begin
  FreeMem(address);
end;

function _wcscmp(string1, string2: PChar): Integer; cdecl;
begin
  Result := StrComp(string1, string2);
end;

procedure Log(const S: AnsiString);
begin
  if S <> '' then
    Setup.LoggingFunc.Log(UTF8ToString(S));
end;

function __fputs(str: PAnsiChar; unused: Pointer): Integer; cdecl;

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

procedure _ReportProgress(const FileName: PChar; const Progress, ProgressMax: UInt64; var Abort: Bool); cdecl;
begin
  if Assigned(State.OnExtractionProgress) then begin
    { Make sure script isn't called crazy often because that would slow the download significantly. Only report:
      -At start or finish
      -Or if somehow Progress decreased or Max changed
      -Or if at least 512 KB progress was made since last report
    }
    if (Progress = 0) or (Progress = ProgressMax) or
       (Progress < State.LastReportedProgress) or (ProgressMax <> State.LastReportedProgressMax) or
       ((Progress - State.LastReportedProgress) > 524288) then begin
      try
        if not State.OnExtractionProgress(State.ExtractedArchiveName, FileName, Progress, ProgressMax) then
          Abort := True;
      finally
        State.LastReportedProgress := Progress;
        State.LastReportedProgressMax := ProgressMax;
      end;
    end;
  end;

  if not Abort and DownloadTemporaryFileOrExtract7ZipArchiveProcessMessages then
    Application.ProcessMessages;

  if Abort then
    State.Aborted := True;
end;

procedure Extract7ZipArchive(const ArchiveFileName, DestDir: String;
  const FullPaths: Boolean; const OnExtractionProgress: TOnExtractionProgress);
begin
  if ArchiveFileName = '' then
    InternalError('Extract7ZipArchive: Invalid ArchiveFileName value');
  if DestDir = '' then
    InternalError('Extract7ZipArchive: Invalid DestDir value');

  LogFmt('Extracting 7-Zip archive %s to %s. Full paths? %s', [ArchiveFileName, DestDir, SYesNo[FullPaths]]);

  var SaveCurDir := GetCurrentDir;
  if not ForceDirectories(False, DestDir) or not SetCurrentDir(DestDir) then
    raise Exception.Create(FmtSetupMessage(msgErrorExtractionFailed, ['-1']));
  try
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
      raise Exception.Create(SetupMessages[msgErrorExtractionAborted])
    else if Res <> 0 then
      raise Exception.Create(FmtSetupMessage(msgErrorExtractionFailed, [Res.ToString]))
  finally
    SetCurrentDir(SaveCurDir);
  end;
end;

end.
