unit Setup.ExtractFileFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Installation procedures: ExtractTemporaryFile
}

interface

procedure ExtractTemporaryFile(const BaseName: String);
function ExtractTemporaryFiles(const Pattern: String): Integer;

implementation

uses
  Windows, SysUtils,
  PathFunc,
  Shared.CommonFunc, Shared.FileClass, Shared.Struct,
  SetupLdrAndSetup.RedirFunc,
  Setup.InstFunc, Setup.FileExtractor, Setup.LoggingFunc, Setup.MainFunc;

procedure InternalExtractTemporaryFile(const DestName: String;
  const CurFile: PSetupFileEntry; const CurFileLocation: PSetupFileLocationEntry;
  const CreateDirs: Boolean);
var
  DestFile: String;
  DestF: TFile;
  CurFileDate: TFileTime;
begin
  DestFile := AddBackslash(TempInstallDir) + DestName;

  Log('Extracting temporary file: ' + DestFile);

  { Does not disable FS redirection, like everything else working on the temp dir }

  if CreateDirs then
    ForceDirectories(False, PathExtractPath(DestFile));
  DestF := TFile.Create(DestFile, fdCreateAlways, faWrite, fsNone);
  try
    try
      FileExtractor.SeekTo(CurFileLocation^, nil);
      FileExtractor.DecompressFile(CurFileLocation^, DestF, nil,
        not (foDontVerifyChecksum in CurFile^.Options));

      if floTimeStampInUTC in CurFileLocation^.Flags then
        CurFileDate := CurFileLocation^.SourceTimeStamp
      else
        LocalFileTimeToFileTime(CurFileLocation^.SourceTimeStamp, CurFileDate);
      SetFileTime(DestF.Handle, nil, nil, @CurFileDate);
    finally
      DestF.Free;
    end;
  except
    DeleteFile(DestFile);
    raise;
  end;
  AddAttributesToFile(False, DestFile, CurFile^.Attribs);
end;

procedure ExtractTemporaryFile(const BaseName: String);

  function EscapeBraces(const S: String): String;
  { Changes all '{' to '{{'. Uses ConstLeadBytes^ for the lead byte table. }
  var
    I: Integer;
  begin
    Result := S;
    I := 1;
    while I <= Length(Result) do begin
      if Result[I] = '{' then begin
        Insert('{', Result, I);
        Inc(I);
      end;
      Inc(I);
    end;
  end;

var
  EscapedBaseName: String;
  CurFileNumber: Integer;
  CurFile: PSetupFileEntry;
begin
  { We compare BaseName to the filename portion of TSetupFileEntry.DestName
    which has braces escaped, but BaseName does not; escape it to match }
  EscapedBaseName := EscapeBraces(BaseName);
  for CurFileNumber := 0 to Entries[seFile].Count-1 do begin
    CurFile := PSetupFileEntry(Entries[seFile][CurFileNumber]);
    if (CurFile^.LocationEntry <> -1) and (CompareText(PathExtractName(CurFile^.DestName), EscapedBaseName) = 0) then begin
      InternalExtractTemporaryFile(BaseName, CurFile, Entries[seFileLocation][CurFile^.LocationEntry], False);
      Exit;
    end;
  end;
  InternalErrorFmt('ExtractTemporaryFile: The file "%s" was not found', [BaseName]);
end;

function ExtractTemporaryFiles(const Pattern: String): Integer;
var
  LowerPattern, DestName: String;
  CurFileNumber: Integer;
  CurFile: PSetupFileEntry;
begin
  if Length(Pattern) >= MAX_PATH then
    InternalError('ExtractTemporaryFiles: Pattern too long');

  LowerPattern := PathLowercase(Pattern);
  Result := 0;

  for CurFileNumber := 0 to Entries[seFile].Count-1 do begin
    CurFile := PSetupFileEntry(Entries[seFile][CurFileNumber]);
    if CurFile^.LocationEntry <> -1 then begin
      { Use ExpandConstEx2 to unescape any braces not in an embedded constant,
        while leaving constants unexpanded }
      DestName := ExpandConstEx2(CurFile^.DestName, [''], False);
      if WildcardMatch(PChar(PathLowercase(DestName)), PChar(LowerPattern)) then begin
        Delete(DestName, 1, PathDrivePartLengthEx(DestName, True)); { Remove any drive part }
        if Pos('{tmp}\', DestName) = 1 then
          Delete(DestName, 1, Length('{tmp}\'));
        if Pos(':', DestName) <> 0 then
          InternalError('ExtractTemporaryFiles: Invalid character in matched file name');
        InternalExtractTemporaryFile(DestName, CurFile, Entries[seFileLocation][CurFile^.LocationEntry], True);
        Inc(Result);
      end;
    end;
  end;

  if Result = 0 then
    InternalErrorFmt('ExtractTemporaryFiles: No files matching "%s" found', [Pattern]);
end;

end.
