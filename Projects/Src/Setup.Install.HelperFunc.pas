unit Setup.Install.HelperFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Installation helper functions which don't need install state such as UninstLog and RegisterFileList

  Only to be called by Setup.Install: if you want to reuse any of these functione from another unit
  you should move the function so somewhere else, like Setup.InstFunc
}

interface

uses
  Windows, SHA256, Shared.FileClass, Shared.Struct, Setup.UninstallLog;

type
  TSetupUninstallLog = class(TUninstallLog)
  protected
    procedure HandleException; override;
  end;

  TRegErrorFunc = (reRegSetValueEx, reRegCreateKeyEx, reRegOpenKeyEx);

procedure SetFilenameLabelText(const S: String; const CallUpdate: Boolean);
procedure SetStatusLabelText(const S: String;
  const ClearFilenameLabelText: Boolean = True);
procedure InstallMessageBoxCallback(const Flags: LongInt; const After: Boolean;
  const Param: LongInt);
procedure CalcFilesSize(var InstallFilesSize, AfterInstallFilesSize: Int64);
procedure InitProgressGauge(const InstallFilesSize: Int64);
procedure UpdateProgressGauge;
procedure FinishProgressGauge(const HideGauge: Boolean);
procedure SetProgress(const AProgress: Int64);
procedure IncProgress(const N: Int64);
function CurProgress: Int64;
procedure ProcessEvents;
procedure InternalProgressProc(const Bytes: Cardinal);
procedure ExternalProgressProc64(const Bytes, MaxProgress: Int64);
procedure JustProcessEventsProc64(const Bytes, Param: Int64);
function AbortRetryIgnoreTaskDialogMsgBox(const Text: String;
  const RetryIgnoreAbortButtonLabels: array of String): Boolean;
function FileTimeToStr(const AFileTime: TFileTime): String;
function TryToGetSHA256OfFile(const DisableFsRedir: Boolean; const Filename: String;
  var Sum: TSHA256Digest): Boolean;
procedure CopySourceFileToDestFile(const SourceF, DestF: TFile;
  [ref] const Verification: TSetupFileVerification; const ISSigSourceFilename: String;
  const AExpectedSize: Int64);
function ShortenOrExpandFontFilename(const Filename: String): String;
function GetLocalTimeAsStr: String;
procedure PackCustomMessagesIntoString(var S: String);
function PackCompiledCodeTextIntoString(const CompiledCodeText: AnsiString): String;
procedure RegError(const Func: TRegErrorFunc; const RootKey: HKEY;
  const KeyName: String; const ErrorCode: Longint);
procedure WriteMsgData(const F: TFile);
procedure MarkExeHeader(const F: TFile; const ModeID: Longint);
procedure ProcessInstallDeleteEntries;
procedure ProcessNeedRestartEvent;
procedure ProcessComponentEntries;
procedure ProcessTasksEntries;
procedure ShutdownApplications;

implementation

uses
  Classes, SysUtils, Forms,
  NewProgressBar, PathFunc, RestartManager, TaskbarProgressFunc,
  Shared.CommonFunc, Shared.CommonFunc.Vcl, Shared.SetupMessageIDs, Shared.SetupTypes,
  SetupLdrAndSetup.Messages,
  Setup.InstFunc, Setup.ISSigVerifyFunc, Setup.LoggingFunc, Setup.MainFunc, Setup.ScriptRunner,
  Setup.WizardForm;

procedure TSetupUninstallLog.HandleException;
begin
  Application.HandleException(Self);
end;

procedure SetFilenameLabelText(const S: String; const CallUpdate: Boolean);
begin
  WizardForm.FilenameLabel.Caption := MinimizePathName(S, WizardForm.FilenameLabel.Font, WizardForm.FileNameLabel.Width);
  if CallUpdate then
    WizardForm.FilenameLabel.Update;
end;

procedure SetStatusLabelText(const S: String;
  const ClearFilenameLabelText: Boolean = True);
begin
  if WizardForm.StatusLabel.Caption <> S then begin
    WizardForm.StatusLabel.Caption := S;
    WizardForm.StatusLabel.Update;
  end;
  if ClearFilenameLabelText then
    SetFilenameLabelText('', True);
end;

procedure InstallMessageBoxCallback(const Flags: LongInt; const After: Boolean;
  const Param: LongInt);
const
  States: array [TNewProgressBarState] of TTaskbarProgressState =
    (tpsNormal, tpsError, tpsPaused);
var
  NewState: TNewProgressBarState;
begin
  if After then
    NewState := npbsNormal
  else if (Flags and MB_ICONSTOP) <> 0 then
    NewState := npbsError
  else
    NewState := npbsPaused;

  with WizardForm.ProgressGauge do begin
    State := NewState;
    Invalidate;
  end;
  SetAppTaskbarProgressState(States[NewState]);
end;

procedure CalcFilesSize(var InstallFilesSize, AfterInstallFilesSize: Int64);
var
  N: Integer;
  CurFile: PSetupFileEntry;
begin
  InstallFilesSize := 0;
  AfterInstallFilesSize := InstallFilesSize;
  for N := 0 to Entries[seFile].Count-1 do begin
    CurFile := PSetupFileEntry(Entries[seFile][N]);
    if ShouldProcessFileEntry(WizardComponents, WizardTasks, CurFile, False) then begin
      with CurFile^ do begin
        var FileSize: Int64;
        if LocationEntry <> -1 then  { not an "external" file }
          FileSize := PSetupFileLocationEntry(Entries[seFileLocation][
           LocationEntry])^.OriginalSize
        else
          FileSize := ExternalSize;
        Inc(InstallFilesSize, FileSize);
        if not (foDeleteAfterInstall in Options) then
          Inc(AfterInstallFilesSize, FileSize);
      end;
    end;
  end;
end;

var
  CurProgressValue: Int64;
  ProgressShiftCount: Cardinal;

procedure InitProgressGauge(const InstallFilesSize: Int64);
begin
  { Calculate the MaxValue for the progress meter }
  var NewMaxValue: Int64 := 1000 * Entries[seIcon].Count;
  if Entries[seIni].Count <> 0 then Inc(NewMaxValue, 1000);
  if Entries[seRegistry].Count <> 0 then Inc(NewMaxValue, 1000);
  Inc(NewMaxValue, InstallFilesSize);
  { To avoid progress updates that are too small to result in any visible
    change, divide the Max value by 2 until it's under 1500 }
  ProgressShiftCount := 0;
  while NewMaxValue >= 1500 do begin
    NewMaxValue := NewMaxValue shr 1;
    Inc(ProgressShiftCount);
  end;
  WizardForm.ProgressGauge.Max := NewMaxValue;
  SetMessageBoxCallbackFunc(InstallMessageBoxCallback, 0);
end;

procedure UpdateProgressGauge;
begin
  var NewPosition := CurProgressValue;
  NewPosition := NewPosition shr ProgressShiftCount;
  if WizardForm.ProgressGauge.Position <> NewPosition then begin
    WizardForm.ProgressGauge.Position := NewPosition;
    WizardForm.ProgressGauge.Update;
  end;
  SetAppTaskbarProgressValue(NewPosition, WizardForm.ProgressGauge.Max);

  if (CodeRunner <> nil) and CodeRunner.FunctionExists('CurInstallProgressChanged', True) then begin
    try
      CodeRunner.RunProcedures('CurInstallProgressChanged', [NewPosition,
        WizardForm.ProgressGauge.Max], False);
    except
      Log('CurInstallProgressChanged raised an exception.');
      Application.HandleException(nil);
    end;
  end;
end;

procedure FinishProgressGauge(const HideGauge: Boolean);
begin
  SetMessageBoxCallbackFunc(nil, 0);

  if HideGauge then
    WizardForm.ProgressGauge.Visible := False;

  SetAppTaskbarProgressState(tpsNoProgress);
end;

procedure SetProgress(const AProgress: Int64);
begin
  CurProgressValue := AProgress;
  UpdateProgressGauge;
end;

procedure IncProgress(const N: Int64);
begin
  Inc(CurProgressValue, N);
  UpdateProgressGauge;
end;

function CurProgress: Int64;
begin
  Result := CurProgressValue;
end;

procedure ProcessEvents;
{ Processes any waiting events. Must call this this periodically or else
  events like clicking the Cancel button won't be processed.
  Calls Abort if NeedToAbortInstall is True, which is usually the result of
  the user clicking Cancel and the form closing. }
begin
  if NeedToAbortInstall then Abort;
  Application.ProcessMessages;
  if NeedToAbortInstall then Abort;
end;

procedure InternalProgressProc(const Bytes: Cardinal);
begin
  IncProgress(Bytes);
  ProcessEvents;
end;

procedure ExternalProgressProc64(const Bytes, MaxProgress: Int64);
begin
  var NewProgress := CurProgress;
  Inc(NewProgress, Bytes);
  { In case the source file was larger than we thought it was, stop the
    progress bar at the maximum amount. Also see CopySourceFileToDestFile. }
  if NewProgress > MaxProgress then
    NewProgress := MaxProgress;
  SetProgress(NewProgress);
  
  ProcessEvents;
end;

procedure JustProcessEventsProc64(const Bytes, Param: Int64);
begin
  ProcessEvents;
end;

function AbortRetryIgnoreTaskDialogMsgBox(const Text: String;
  const RetryIgnoreAbortButtonLabels: array of String): Boolean;
{ Returns True if Ignore was selected, False if Retry was selected, or
  calls Abort if Abort was selected. }
begin
  Result := False;
  case LoggedTaskDialogMsgBox('', SetupMessages[msgAbortRetryIgnoreSelectAction], Text, '',
         mbError, MB_ABORTRETRYIGNORE, RetryIgnoreAbortButtonLabels, 0, True, IDABORT) of
    IDABORT: Abort;
    IDRETRY: ;
    IDIGNORE: Result := True;
  else
    Log('LoggedTaskDialogMsgBox returned an unexpected value. Assuming Abort.');
    Abort;
  end;
end;

function FileTimeToStr(const AFileTime: TFileTime): String;
{ Converts a TFileTime into a string for log purposes. }
var
  FT: TFileTime;
  ST: TSystemTime;
begin
  FileTimeToLocalFileTime(AFileTime, FT);
  if FileTimeToSystemTime(FT, ST) then
    Result := Format('%.4u-%.2u-%.2u %.2u:%.2u:%.2u.%.3u',
      [ST.wYear, ST.wMonth, ST.wDay, ST.wHour, ST.wMinute, ST.wSecond,
       ST.wMilliseconds])
  else
    Result := '(invalid)';
end;

function TryToGetSHA256OfFile(const DisableFsRedir: Boolean; const Filename: String;
  var Sum: TSHA256Digest): Boolean;
{ Like GetSHA256OfFile but traps exceptions locally. Returns True if successful. }
begin
  try
    Sum := GetSHA256OfFile(DisableFsRedir, Filename);
    Result := True;
  except
    Result := False;
  end;
end;

procedure CopySourceFileToDestFile(const SourceF, DestF: TFile;
  [ref] const Verification: TSetupFileVerification; const ISSigSourceFilename: String;
  const AExpectedSize: Int64);
{ Copies all bytes from SourceF to DestF, incrementing process meter as it
  goes. Assumes file pointers of both are 0. }
var
  BufSize: Cardinal;
  Buf: array[0..16383] of Byte;
  Context: TSHA256Context;
begin
  var ExpectedFileHash: TSHA256Digest;
  if Verification.Typ <> fvNone then begin
    if Verification.Typ = fvHash then
      ExpectedFileHash := Verification.Hash
    else
      DoISSigVerify(SourceF, nil, ISSigSourceFilename, True, Verification.ISSigAllowedKeys, ExpectedFileHash);
    { ExpectedFileHash checked below after copy }
    SHA256Init(Context);
  end;

  var MaxProgress := CurProgress;
  Inc(MaxProgress, AExpectedSize);
  var BytesLeft := SourceF.Size;

  { To avoid file system fragmentation, preallocate all of the bytes in the
    destination file }
  DestF.Seek(BytesLeft);
  DestF.Truncate;
  DestF.Seek(0);

  while True do begin
    BufSize := SizeOf(Buf);
    if BytesLeft < BufSize then
      BufSize := BytesLeft;
    if BufSize = 0 then
      Break;

    SourceF.ReadBuffer(Buf, BufSize);
    DestF.WriteBuffer(Buf, BufSize);
    Dec(BytesLeft, BufSize);

    if Verification.Typ <> fvNone then
      SHA256Update(Context, Buf, BufSize);

    ExternalProgressProc64(BufSize, MaxProgress);
  end;

  if Verification.Typ <> fvNone then begin
    if not SHA256DigestsEqual(SHA256Final(Context), ExpectedFileHash) then
      VerificationError(veFileHashIncorrect);
    Log(VerificationSuccessfulLogMessage);
  end;

  { In case the source file was shorter than we thought it was, bump the
    progress bar to the maximum amount }
  SetProgress(MaxProgress);
end;

function ShortenOrExpandFontFilename(const Filename: String): String;
{ Expands Filename, except if it's in the Fonts directory, in which case it
  removes the path }
var
  FontDir: String;
begin
  Result := PathExpand(Filename);
  FontDir := GetShellFolder(False, sfFonts);
  if FontDir <> '' then
    if PathCompare(PathExtractDir(Result), FontDir) = 0 then
      Result := PathExtractName(Result);
end;

function GetLocalTimeAsStr: String;
var
  SysTime: TSystemTime;
begin
  GetLocalTime(SysTime);
  SetString(Result, PChar(@SysTime), SizeOf(SysTime) div SizeOf(Char));
end;

procedure PackCustomMessagesIntoString(var S: String);
var
  M: TMemoryStream;
  Count, I, N: Integer;
begin
  M := TMemoryStream.Create;
  try
    Count := 0;
    M.WriteBuffer(Count, SizeOf(Count));  { overwritten later }
    for I := 0 to Entries[seCustomMessage].Count-1 do begin
      with PSetupCustomMessageEntry(Entries[seCustomMessage][I])^ do begin
        if (LangIndex = -1) or (LangIndex = ActiveLanguage) then begin
          N := Length(Name);
          M.WriteBuffer(N, SizeOf(N));
          M.WriteBuffer(Name[1], N*SizeOf(Name[1]));
          N := Length(Value);
          M.WriteBuffer(N, SizeOf(N));
          M.WriteBuffer(Value[1], N*SizeOf(Value[1]));
          Inc(Count);
        end;
      end;
    end;
    M.Seek(0, soFromBeginning);
    M.WriteBuffer(Count, SizeOf(Count));
    SetString(S, PChar(M.Memory), M.Size div SizeOf(Char));
  finally
    M.Free;
  end;
end;

function PackCompiledCodeTextIntoString(const CompiledCodeText: AnsiString): String;
var
  N: Integer;
begin
  N := Length(CompiledCodeText);
  if N mod 2 = 1 then
    Inc(N); { This will lead to 1 extra byte being moved but that's ok since it is the #0 }
  N := N div 2;
  SetString(Result, PChar(Pointer(CompiledCodeText)), N);
end;

procedure RegError(const Func: TRegErrorFunc; const RootKey: HKEY;
  const KeyName: String; const ErrorCode: Longint);
const
  ErrorMsgs: array[TRegErrorFunc] of TSetupMessageID =
    (msgErrorRegWriteKey, msgErrorRegCreateKey, msgErrorRegOpenKey);
  FuncNames: array[TRegErrorFunc] of String =
    ('RegSetValueEx', 'RegCreateKeyEx', 'RegOpenKeyEx');
begin
  raise Exception.Create(FmtSetupMessage(ErrorMsgs[Func],
      [GetRegRootKeyName(RootKey), KeyName]) + SNewLine2 +
    FmtSetupMessage(msgErrorFunctionFailedWithMessage,
      [FuncNames[Func], IntToStr(ErrorCode), Win32ErrorString(ErrorCode)]));
end;

procedure WriteMsgData(const F: TFile);
var
  MsgLangOpts: TMessagesLangOptions;
  LangEntry: PSetupLanguageEntry;
begin
  FillChar(MsgLangOpts, SizeOf(MsgLangOpts), 0);
  MsgLangOpts.ID := MessagesLangOptionsID;
  StrPLCopy(MsgLangOpts.DialogFontName, LangOptions.DialogFontName,
    (SizeOf(MsgLangOpts.DialogFontName) div SizeOf(MsgLangOpts.DialogFontName[0])) - 1);
  MsgLangOpts.DialogFontSize := LangOptions.DialogFontSize;
  if LangOptions.RightToLeft then
    Include(MsgLangOpts.Flags, lfRightToLeft);
  LangEntry := Entries[seLanguage][ActiveLanguage];
  F.WriteBuffer(LangEntry.Data[1], Length(LangEntry.Data));
  F.WriteBuffer(MsgLangOpts, SizeOf(MsgLangOpts));
end;

procedure MarkExeHeader(const F: TFile; const ModeID: Longint);
begin
  F.Seek(SetupExeModeOffset);
  F.WriteBuffer(ModeID, SizeOf(ModeID));
end;

procedure ProcessInstallDeleteEntries;
var
  I: Integer;
begin
  for I := 0 to Entries[seInstallDelete].Count-1 do
    with PSetupDeleteEntry(Entries[seInstallDelete][I])^ do
      if ShouldProcessEntry(WizardComponents, WizardTasks, Components, Tasks, Languages, Check) then begin
        DebugNotifyEntry(seInstallDelete, I);
        NotifyBeforeInstallEntry(BeforeInstall);
        case DeleteType of
          dfFiles, dfFilesAndOrSubdirs:
            DelTree(InstallDefaultDisableFsRedir, ExpandConst(Name), False, True, DeleteType = dfFilesAndOrSubdirs, False,
              nil, nil, nil);
          dfDirIfEmpty:
            DelTree(InstallDefaultDisableFsRedir, ExpandConst(Name), True, False, False, False, nil, nil, nil);
        end;
        NotifyAfterInstallEntry(AfterInstall);
      end;
end;

procedure ProcessNeedRestartEvent;
begin
  if (CodeRunner <> nil) and CodeRunner.FunctionExists('NeedRestart', True) then begin
    if not NeedsRestart then begin
      try
        if CodeRunner.RunBooleanFunctions('NeedRestart', [''], bcTrue, False, False) then begin
          NeedsRestart := True;
          Log('Will restart because NeedRestart returned True.');
        end;
      except
        Log('NeedRestart raised an exception.');
        Application.HandleException(nil);
      end;
    end
    else
      Log('Not calling NeedRestart because a restart has already been deemed necessary.');
  end;
end;

procedure ProcessComponentEntries;
var
  I: Integer;
begin
  for I := 0 to Entries[seComponent].Count-1 do begin
    with PSetupComponentEntry(Entries[seComponent][I])^ do begin
      if ShouldProcessEntry(WizardComponents, nil, Name, '', Languages, '') and (coRestart in Options) then begin
        NeedsRestart := True;
        Break;
      end;
    end;
  end;
end;

procedure ProcessTasksEntries;
var
  I: Integer;
begin
  for I := 0 to Entries[seTask].Count-1 do begin
    with PSetupTaskEntry(Entries[seTask][I])^ do begin
      if ShouldProcessEntry(nil, WizardTasks, '', Name, Languages, '') and (toRestart in Options) then begin
        NeedsRestart := True;
        Break;
      end;
    end;
  end;
end;

procedure ShutdownApplications;
const
  ERROR_FAIL_SHUTDOWN = 351;
  ForcedStrings: array [Boolean] of String = ('', ' (forced)');
  ForcedActionFlag: array [Boolean] of ULONG = (0, RmForceShutdown);
var
  Forced: Boolean;
  Error: DWORD;
begin
  Forced := InitForceCloseApplications or
            ((shForceCloseApplications in SetupHeader.Options) and not InitNoForceCloseApplications);

  Log('Shutting down applications using our files.' + ForcedStrings[Forced]);

  RmDoRestart := True;

  Error := RmShutdown(RmSessionHandle, ForcedActionFlag[Forced], nil);
  while Error = ERROR_FAIL_SHUTDOWN do begin
    Log('Some applications could not be shut down.');
    if AbortRetryIgnoreTaskDialogMsgBox(
         SetupMessages[msgErrorCloseApplications],
         [SetupMessages[msgAbortRetryIgnoreRetry], SetupMessages[msgAbortRetryIgnoreIgnore], SetupMessages[msgAbortRetryIgnoreCancel]]) then
      Break;
    Log('Retrying to shut down applications using our files.' + ForcedStrings[Forced]);
    Error := RmShutdown(RmSessionHandle, ForcedActionFlag[Forced], nil);
  end;

  { Close session on all errors except for ERROR_FAIL_SHUTDOWN, should still call RmRestart in that case. }
  if (Error <> ERROR_SUCCESS) and (Error <> ERROR_FAIL_SHUTDOWN) then begin
    RmEndSession(RmSessionHandle);
    LogFmt('RmShutdown returned an error: %d', [Error]);
    RmDoRestart := False;
  end;
end;

end.
