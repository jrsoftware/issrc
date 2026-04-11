unit Setup.CompactFunc;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Post-install compact.exe /EXE:LZX support
}

interface

uses
  Setup.UninstallLog;

procedure CompactInstalledFiles(const UninstLog: TUninstallLog);

implementation

uses
  Windows, SysUtils, Classes,
  Shared.CommonFunc,
  SetupLdrAndSetup.InstFunc, SetupLdrAndSetup.Messages,
  Setup.InstFunc, Setup.MainFunc, Setup.LoggingFunc,
  Setup.Install.HelperFunc;

type
  TCompactCandidate = record
    Path: String;
    IsDir: Boolean;
  end;

function SupportsLZXCompression: Boolean;
begin
  Result := IsWindows8OrLater;
  if Result then
    Log('compact.exe LZX support check: supported by OS version.')
  else
    Log('compact.exe LZX support check: unsupported by OS version.');
end;

function ExpandCompactExeFilename: String;
begin
  Result := ApplyPathRedirRules(IsWin64, ExpandConst('{sys}\compact.exe'), tpCurrent, [rfNormalPath]);
end;

function ExecCompactCommand(const Params: String; out ResultCode: DWORD): Boolean;
begin
  LogFmt('Executing compact.exe with parameters: %s', [Params]);
  Result := InstExec(ExpandCompactExeFilename, Params, '', ewWaitUntilTerminated,
    SW_HIDE, ProcessMessagesProc, ResultCode);
end;

procedure AddCompactCandidate(var Candidates: array of TCompactCandidate; var Count: Integer;
  const Path: String; const IsDir: Boolean);
begin
  if Path = '' then
    Exit;
  if Count > High(Candidates) then
    InternalError('AddCompactCandidate: candidate buffer too small');
  Candidates[Count].Path := Path;
  Candidates[Count].IsDir := IsDir;
  Inc(Count);
end;

function BuildCompactCandidates: TArray<TCompactCandidate>;
var
  Count: Integer;
begin
  SetLength(Result, Entries[seDir].Count + Entries[seFile].Count + 1);
  Count := 0;

  if shCreateAppDir in SetupHeader.Options then
    AddCompactCandidate(Result, Count,
      ApplyPathRedirRules(InstallDefault64Bit, WizardDirValue, tpCurrent, [rfNormalPath]), True);

  for var I := 0 to Entries[seDir].Count - 1 do
    with PSetupDirEntry(Entries[seDir][I])^ do
      if ShouldProcessEntry(WizardComponents, WizardTasks, Components, Tasks, Languages, Check) then
        AddCompactCandidate(Result, Count,
          RemoveBackslashUnlessRoot(ApplyPathRedirRules(InstallDefault64Bit,
            ExpandConst(DirName), tpCurrent, [rfNormalPath])), True);

  for var I := 0 to Entries[seFile].Count - 1 do
    with PSetupFileEntry(Entries[seFile][I])^ do
      if ((FileType <> ftUninstExe) or EvalDirectiveCheck(SetupHeader.Uninstallable)) and
         ShouldProcessFileEntry(WizardComponents, WizardTasks, @PSetupFileEntry(Entries[seFile][I])^, False) and
         not (foDontCopy in Options) and not (foDeleteAfterInstall in Options) and
         not (foRestartReplace in Options) then begin
        if FileType = ftUninstExe then
          AddCompactCandidate(Result, Count, UninstallExeFilename, False)
        else if not (foExternalSizePreset in Options) or not (foExtractArchive in Options) then
          AddCompactCandidate(Result, Count,
            ApplyPathRedirRules(IsEntryBitness64Bit(Bitness), ExpandConst(DestName), tpCurrent, [rfNormalPath]), False);
      end;

  SetLength(Result, Count);
end;

function ShouldCompactPath(const Candidate: TCompactCandidate): Boolean;
begin
  if Candidate.Path = '' then
    Exit(False);
  if Candidate.IsDir then
    Result := DirExists(Candidate.Path)
  else
    Result := NewFileExists(Candidate.Path);
end;

function CompactCandidate(const Candidate: TCompactCandidate): Boolean;
var
  Params: String;
  ResultCode: DWORD;
begin
  if Candidate.IsDir then
    Params := '/C /S /I /F /EXE:LZX "' + Candidate.Path + '"'
  else
    Params := '/C /I /F /EXE:LZX "' + Candidate.Path + '"';

  Result := ExecCompactCommand(Params, ResultCode);
  if Result then begin
    if ResultCode = 0 then
      LogFmt('Successfully compacted: %s', [Candidate.Path])
    else begin
      LogFmt('compact.exe returned exit code %d for: %s', [ResultCode, Candidate.Path]);
      Result := False;
    end;
  end
  else
    LogFmt('Failed to execute compact.exe for: %s', [Candidate.Path]);
end;

procedure RecordCompactRun(const UninstLog: TUninstallLog);
begin
  if ExpandCompactExeFilename <> '' then
    UninstLog.Add(utRun, [ExpandCompactExeFilename,
      '/U /I /F /EXE:A /S "' + ApplyPathRedirRules(InstallDefault64Bit, WizardDirValue, tpCurrent, [rfNormalPath]) + '"',
      '', '', ''], utRun_Hidden or utRun_NoWait);
end;

procedure CompactInstalledFiles(const UninstLog: TUninstallLog);
begin
  if not SupportsLZXCompression then
    Exit;

  const CompactExeFilename = ExpandCompactExeFilename;
  if not NewFileExists(CompactExeFilename) then begin
    LogFmt('Skipping post-install compaction because compact.exe was not found: %s', [CompactExeFilename]);
    Exit;
  end;

  SetStatusLabelText(SetupMessages[msgStatusCompactFiles]);
  SetFilenameLabelText('', False);
  ProcessEvents;

  var AnyFailures := False;
  var Candidates := BuildCompactCandidates;
  for var I := 0 to High(Candidates) do begin
    if not ShouldCompactPath(Candidates[I]) then
      Continue;
    SetFilenameLabelText(Candidates[I].Path, True);
    ProcessEvents;
    if not CompactCandidate(Candidates[I]) then
      AnyFailures := True;
  end;

  SetFilenameLabelText('', False);
  if AnyFailures then
    Log('Post-install compaction completed with one or more failures. Continuing normally.')
  else
    Log('Post-install compaction completed successfully.');
end;

end.
