unit Setup.Uninstall;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Uninstaller
}

interface

procedure RunUninstaller;
procedure HandleUninstallerEndSession;

implementation

uses
  Windows, SysUtils, Messages, Forms, PathFunc, Shared.CommonFunc.Vcl,
  Shared.CommonFunc, Setup.UninstallLog, SetupLdrAndSetup.Messages,
  Shared.SetupMessageIDs, SetupLdrAndSetup.InstFunc, Setup.InstFunc, Shared.Struct,
  Shared.SetupEntFunc, Setup.UninstallProgressForm, Setup.UninstallSharedFileForm,
  Shared.FileClass, Setup.ScriptRunner, Setup.DebugClient, Shared.SetupSteps,
  Setup.LoggingFunc, Setup.MainFunc, Setup.SpawnServer;

type
  TExtUninstallLog = class(TUninstallLog)
  private
    FLastUpdateTime: DWORD;
    FNoSharedFileDlgs: Boolean;
    FRemoveSharedFiles: Boolean;
  protected
    procedure HandleException; override;
    function ShouldRemoveSharedFile(const Filename: String): Boolean; override;
    procedure StatusUpdate(StartingCount, CurCount: Integer); override;
  end;

const
  WM_KillFirstPhase = WM_USER + 333;

var
  UninstallExitCode: DWORD = 1;
  UninstExeFilename, UninstDataFilename, UninstMsgFilename: String;
  UninstDataFile: TFile;
  UninstLog: TExtUninstallLog = nil;
  Title: String;
  DidRespawn, SecondPhase: Boolean;
  EnableLogging, Silent, VerySilent, NoRestart: Boolean;
  LogFilename: String;
  InitialProcessWnd, FirstPhaseWnd, DebugServerWnd: HWND;
  OldWindowProc: Pointer;

procedure ShowExceptionMsg;
var
  Msg: String;
begin
  if ExceptObject is EAbort then
    Exit;
  Msg := GetExceptMessage;
  Log('Exception message:');
  LoggedAppMessageBox(PChar(Msg), Pointer(SetupMessages[msgErrorTitle]),
    MB_OK or MB_ICONSTOP, True, IDOK);
    { ^ use a Pointer cast instead of a PChar cast so that it will use "nil"
      if SetupMessages[msgErrorTitle] is empty due to the messages not being
      loaded yet. MessageBox displays 'Error' as the caption if the lpCaption
      parameter is nil. }
end;

procedure TExtUninstallLog.HandleException;
begin
  ShowExceptionMsg;
end;

function TExtUninstallLog.ShouldRemoveSharedFile(const Filename: String): Boolean;
const
  SToAll: array[Boolean] of String = ('', ' to All');
begin
  if Silent or VerySilent then
    Result := True
  else begin
    if not FNoSharedFileDlgs then begin
      { FNoSharedFileDlgs will be set to True if a "...to All" button is clicked }
      FRemoveSharedFiles := ExecuteRemoveSharedFileDlg(Filename,
        FNoSharedFileDlgs);
      LogFmt('Remove shared file %s? User chose %s%s', [Filename, SYesNo[FRemoveSharedFiles], SToAll[FNoSharedFileDlgs]]);
    end;
    Result := FRemoveSharedFiles;
  end;
end;

procedure InitializeUninstallProgressForm;
begin
  UninstallProgressForm := AppCreateForm(TUninstallProgressForm) as TUninstallProgressForm;
  UninstallProgressForm.Initialize(Title, UninstLog.AppName, ufModernStyle in UninstLog.Flags);
  if CodeRunner <> nil then begin
    try
      CodeRunner.RunProcedures('InitializeUninstallProgressForm', [''], False);
    except
      Log('InitializeUninstallProgressForm raised an exception (fatal).');
      raise;
    end;
  end;
  if not VerySilent then begin
    UninstallProgressForm.Show;
    { Ensure the form is fully painted now in case
      CurUninstallStepChanged(usUninstall) take a long time to return }
    UninstallProgressForm.Update;
  end;
end;

procedure TExtUninstallLog.StatusUpdate(StartingCount, CurCount: Integer);
var
  NowTime: DWORD;
begin
  { Only update the progress bar if it's at the beginning or end, or if
    30 ms has passed since the last update (so that updating the progress
    bar doesn't slow down the actual uninstallation process). }
  NowTime := GetTickCount;
  if (Cardinal(NowTime - FLastUpdateTime) >= Cardinal(30)) or
     (StartingCount = CurCount) or (CurCount = 0) then begin
    FLastUpdateTime := NowTime;
    UninstallProgressForm.UpdateProgress(StartingCount - CurCount, StartingCount);
  end;
  Application.ProcessMessages;
end;

function LoggedMessageBoxFmt1(const ID: TSetupMessageID; const Arg1: String;
  const Title: String; const Flags: UINT; const Suppressible: Boolean;
  const Default: Integer): Integer;
begin
  Result := LoggedAppMessageBox(PChar(FmtSetupMessage1(ID, Arg1)), PChar(Title),
    Flags, Suppressible, Default);
end;

procedure RaiseLastError(const S: String);
var
  ErrorCode: DWORD;
begin
  ErrorCode := GetLastError;
  raise Exception.Create(FmtSetupMessage(msgLastErrorMessage,
    [S, IntToStr(ErrorCode), Win32ErrorString(ErrorCode)]));
end;

function Exec(const Filename: String; const Parms: String): THandle;
var
  CmdLine: String;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  CmdLine := '"' + Filename + '" ' + Parms;

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  if not CreateProcess(nil, PChar(CmdLine), nil, nil, False, 0, nil, nil,
     StartupInfo, ProcessInfo) then
    RaiseLastError(SetupMessages[msgLdrCannotExecTemp]);
  CloseHandle(ProcessInfo.hThread);
  Result := ProcessInfo.hProcess;
end;

function ProcessMsgs: Boolean;
var
  Msg: TMsg;
begin
  Result := False;
  while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do begin
    if Msg.Message = WM_QUIT then begin
      Result := True;
      Break;
    end;
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;

function FirstPhaseWindowProc(Wnd: HWND; Msg: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
begin
  Result := 0;
  case Msg of
    WM_QUERYENDSESSION: ;  { Return zero to deny any shutdown requests }
    WM_KillFirstPhase: begin
        PostQuitMessage(0);
        { If we got WM_KillFirstPhase, the second phase must have been
          successful (up until now, at least). Set an exit code of 0. }
        UninstallExitCode := 0;
      end;
  else
    Result := CallWindowProc(OldWindowProc, Wnd, Msg, wParam, lParam);
  end;
end;

procedure DeleteUninstallDataFiles;
var
  ProcessWnd: HWND;
  ProcessID: DWORD;
  Process: THandle;
begin
  Log('Deleting Uninstall data files.');

  { Truncate the .dat file to zero bytes just before relinquishing exclusive
    access to it }
  try
    UninstDataFile.Seek(0);
    UninstDataFile.Truncate;
  except
    { ignore any exceptions, just in case }
  end;
  FreeAndNil(UninstDataFile);

  { Delete the .dat and .msg files }
  DeleteFile(UninstDataFilename);
  DeleteFile(UninstMsgFilename);

  { Tell the first phase to terminate, then delete its .exe }
  if FirstPhaseWnd <> 0 then begin
    if InitialProcessWnd <> 0 then
      { If the first phase respawned, wait on the initial process }
      ProcessWnd := InitialProcessWnd
    else
      ProcessWnd := FirstPhaseWnd;
    ProcessID := 0;
    if GetWindowThreadProcessId(ProcessWnd, @ProcessID) <> 0 then
      Process := OpenProcess(SYNCHRONIZE, False, ProcessID)
    else
      Process := 0;  { shouldn't get here }
    SendNotifyMessage(FirstPhaseWnd, WM_KillFirstPhase, 0, 0);
    if Process <> 0 then begin
      WaitForSingleObject(Process, INFINITE);
      CloseHandle(Process);
    end;
    { Sleep for a bit to allow pre-Windows 2000 Add/Remove Programs to finish
      bringing itself to the foreground before we take it back below. Also
      helps the DelayDeleteFile call succeed on the first try. }
    if not Debugging then
      Sleep(500);
  end;
  UninstallExitCode := 0;
  DelayDeleteFile(False, UninstExeFilename, 13, 50, 250);
  if Debugging then
    DebugNotifyUninstExe('');
  { Pre-Windows 2000 Add/Remove Programs will try to bring itself to the
    foreground after the first phase terminates. Take it back. }
  Application.BringToFront;
end;

procedure ProcessCommandLine;
var
  WantToSuppressMsgBoxes, ParamIsAutomaticInternal: Boolean;
  I: Integer;
  ParamName, ParamValue: String;
begin
  WantToSuppressMsgBoxes := False;

  { NewParamsForCode will hold all params except automatic internal ones like /SECONDPHASE= and /DEBUGWND=
    Actually currently only needed in the second phase, but setting always anyway
    Also see Main.InitializeSetup }
  NewParamsForCode.Add(NewParamStr(0));

  for I := 1 to NewParamCount do begin
    SplitNewParamStr(I, ParamName, ParamValue);
    ParamIsAutomaticInternal := False;
    if CompareText(ParamName, '/Log') = 0 then begin
      EnableLogging := True;
      LogFilename := '';
    end else if CompareText(ParamName, '/Log=') = 0 then begin
      EnableLogging := True;
      LogFilename := ParamValue;
    end else if CompareText(ParamName, '/INITPROCWND=') = 0 then begin
      ParamIsAutomaticInternal := True;
      DidRespawn := True;
      InitialProcessWnd := StrToInt(ParamValue);
    end else if CompareText(ParamName, '/SECONDPHASE=') = 0 then begin
      ParamIsAutomaticInternal := True;
      SecondPhase := True;
      UninstExeFilename := ParamValue;
    end else if CompareText(ParamName, '/FIRSTPHASEWND=') = 0 then begin
      ParamIsAutomaticInternal := True;
      FirstPhaseWnd := StrToInt(ParamValue)
    end else if CompareText(ParamName, '/SILENT') = 0 then
      Silent := True
    else if CompareText(ParamName, '/VERYSILENT') = 0 then
      VerySilent := True
    else if CompareText(ParamName, '/NoRestart') = 0 then
      NoRestart := True
    else if CompareText(ParamName, '/SuppressMsgBoxes') = 0 then
      WantToSuppressMsgBoxes := True
    else if CompareText(ParamName, '/DEBUGWND=') = 0 then begin
      ParamIsAutomaticInternal := True;
      DebugServerWnd := StrToInt(ParamValue);
    end;
    if not ParamIsAutomaticInternal then
      NewParamsForCode.Add(NewParamStr(I));
  end;

  if WantToSuppressMsgBoxes and (Silent or VerySilent) then
    InitSuppressMsgBoxes := True;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep; HandleException: Boolean);
begin
  if CodeRunner <> nil then begin
    try
      CodeRunner.RunProcedures('CurUninstallStepChanged', [Ord(CurUninstallStep)], False);
    except
      if HandleException then begin
        Log('CurUninstallStepChanged raised an exception.');
        ShowExceptionMsg;
      end
      else begin
        Log('CurUninstallStepChanged raised an exception (fatal).');
        raise;
      end;
    end;
  end;
end;

function OpenUninstDataFile(const AAccess: TFileAccess): TFile;
begin
  Result := nil;  { avoid warning }
  try
    Result := TFile.Create(UninstDataFilename, fdOpenExisting, AAccess, fsNone);
  except
    on E: EFileError do begin
      SetLastError(E.ErrorCode);
      RaiseLastError(FmtSetupMessage1(msgUninstallOpenError,
        UninstDataFilename));
    end;
  end;
end;

function RespawnFirstPhaseIfNeeded: Boolean;
var
  F: TFile;
  Flags: TUninstallLogFlags;
  RequireAdmin: Boolean;
begin
  Result := False;
  if DidRespawn then
    Exit;

  F := OpenUninstDataFile(faRead);
  try
    Flags := ReadUninstallLogFlags(F, UninstDataFilename);
  finally
    F.Free;
  end;
  RequireAdmin := (ufAdminInstalled in Flags) or (ufPowerUserInstalled in Flags);

  if NeedToRespawnSelfElevated(RequireAdmin, False) then begin
    RespawnSelfElevated(UninstExeFilename,
      Format('/INITPROCWND=$%x ', [Application.Handle]) + GetCmdTail,
      UninstallExitCode);
    Result := True;
  end;
end;

procedure RunFirstPhase;
var
  TempDir, TempFile: String;
  TempDirExisted: Boolean;
  Wnd: HWND;
  ProcessHandle: THandle;
begin
  { Copy self to a subdirectory of the TEMP directory with a name like
    _iu14D2N.tmp. The actual uninstallation process must be done from
    somewhere outside the application directory since EXE's can't delete
    themselves while they are running. }
  TempDirExisted := GenerateNonRandomUniqueTempDir(IsAdmin, GetTempDir, TempDir);
  TempFile := AddBackslash(TempDir) + '_unins.tmp';
  if not TempDirExisted then
    try
      RestartReplace(False, TempFile, '');
      RestartReplace(False, TempDir, '');
    except
      { ignore exceptions }
    end;
  if not CopyFile(PChar(UninstExeFilename), PChar(TempFile), False) then
    RaiseLastError(SetupMessages[msgLdrCannotCreateTemp]);
  { Don't want any attribute like read-only transferred }
  SetFileAttributes(PChar(TempFile), FILE_ATTRIBUTE_NORMAL);

  { Create first phase window. This window waits for a WM_KillFirstPhase
    message from the second phase process, and terminates itself in
    response. The reason the first phase doesn't just terminate
    immediately is because the Control Panel Add/Remove applet refreshes
    its list as soon as the program terminates. So it waits until the
    uninstallation is complete before terminating. }
  Wnd := CreateWindowEx(0, 'STATIC', '', 0, 0, 0, 0, 0, HWND_DESKTOP, 0,
    HInstance, nil);
  Longint(OldWindowProc) := SetWindowLong(Wnd, GWL_WNDPROC,
    Longint(@FirstPhaseWindowProc));
  try
    { Execute the copy of itself ("second phase") }
    ProcessHandle := Exec(TempFile, Format('/SECONDPHASE="%s" /FIRSTPHASEWND=$%x ',
      [NewParamStr(0), Wnd]) + GetCmdTail);

    { Wait till the second phase process unexpectedly dies or is ready
      for the first phase to terminate. }
    repeat until ProcessMsgs or (MsgWaitForMultipleObjects(1,
      ProcessHandle, False, INFINITE, QS_ALLINPUT) <> WAIT_OBJECT_0+1);
    CloseHandle(ProcessHandle);
  finally
    DestroyWindow(Wnd);
  end;
end;

procedure AssignCustomMessages(AData: Pointer; ADataSize: Cardinal);

  procedure Corrupted;
  begin
    InternalError('Custom message data corrupted');
  end;

  procedure Read(var Buf; const Count: Cardinal);
  begin
    if Count > ADataSize then
      Corrupted;
    Move(AData^, Buf, Count);
    Dec(ADataSize, Count);
    Inc(Cardinal(AData), Count);
  end;

  procedure ReadString(var S: String);
  var
    N: Integer;
  begin
    Read(N, SizeOf(N));
    if (N < 0) or (N > $FFFFF) then  { sanity check }
      Corrupted;
    SetString(S, nil, N);
    if N <> 0 then
      Read(Pointer(S)^, N * SizeOf(S[1]));
  end;

var
  Count, I: Integer;
  CustomMessageEntry: PSetupCustomMessageEntry;
begin
  Read(Count, SizeOf(Count));
  Entries[seCustomMessage].Capacity := Count;
  for I := 0 to Count-1 do begin
    CustomMessageEntry := AllocMem(SizeOf(TSetupCustomMessageEntry));
    try
      ReadString(CustomMessageEntry.Name);
      ReadString(CustomMessageEntry.Value);
      CustomMessageEntry.LangIndex := -1;
    except
      SEFreeRec(CustomMessageEntry, SetupCustomMessageEntryStrings, SetupCustomMessageEntryAnsiStrings);
      raise;
    end;
    Entries[seCustomMessage].Add(CustomMessageEntry);
  end;
end;

function ExtractCompiledCodeText(S: String): AnsiString;
begin
  SetString(Result, PAnsiChar(Pointer(S)), Length(S)*SizeOf(S[1]));
end;

procedure RunSecondPhase;
const
  RemovedMsgs: array[Boolean] of TSetupMessageID =
    (msgUninstalledMost, msgUninstalledAll);
var
  RestartSystem: Boolean;
  CompiledCodeData: array[0..6] of String;
  CompiledCodeText: AnsiString;
  Res, RemovedAll, UninstallNeedsRestart: Boolean;
  StartTime: DWORD;
begin
  RestartSystem := False;
  AllowUninstallerShutdown := True;

  try
    if DebugServerWnd <> 0 then
      SetDebugServerWnd(DebugServerWnd, True);

    if EnableLogging then begin
      try
        if LogFilename = '' then
          StartLogging('Uninstall')
        else
          StartLoggingWithFixedFilename(LogFilename);
      except
        on E: Exception do begin
          E.Message := 'Error creating log file:' + SNewLine2 + E.Message;
          raise;
        end;
      end;
    end;
    Log('Setup version: ' + SetupTitle + ' version ' + SetupVersion);
    Log('Original Uninstall EXE: ' + UninstExeFilename);
    Log('Uninstall DAT: ' + UninstDataFilename);
    Log('Uninstall command line: ' + GetCmdTail);
    LogWindowsVersion;

    { Open the .dat file for read access }
    UninstDataFile := OpenUninstDataFile(faRead);

    { Load contents of the .dat file }
    UninstLog := TExtUninstallLog.Create;
    UninstLog.Load(UninstDataFile, UninstDataFilename);

    Title := FmtSetupMessage1(msgUninstallAppFullTitle, UninstLog.AppName);

    { If install was done in Win64, verify that we're still running Win64.
      This test shouldn't fail unless the user somehow downgraded their
      Windows version, or they're running an uninstaller from another machine
      (which they definitely shouldn't be doing). }
    if (ufWin64 in UninstLog.Flags) and not IsWin64 then begin
      LoggedAppMessageBox(PChar(SetupMessages[msgUninstallOnlyOnWin64]), PChar(Title),
        MB_OK or MB_ICONEXCLAMATION, True, IDOK);
      Abort;
    end;

    { Check if admin privileges are needed to uninstall }
    if (ufAdminInstalled in UninstLog.Flags) and not IsAdmin then begin
      LoggedAppMessageBox(PChar(SetupMessages[msgOnlyAdminCanUninstall]), PChar(Title),
        MB_OK or MB_ICONEXCLAMATION, True, IDOK);
      Abort;
    end;

    { Reopen the .dat file for exclusive, read/write access and keep it
      open for the duration of the uninstall process to prevent a second
      instance of the same uninstaller from running. }
    FreeAndNil(UninstDataFile);
    UninstDataFile := OpenUninstDataFile(faReadWrite);

    if not UninstLog.ExtractLatestRecData(utCompiledCode,
         SetupBinVersion or Longint($80000000), CompiledCodeData) then
      InternalError('Cannot find utCompiledCode record for this version of the uninstaller');
    if DebugServerWnd <> 0 then
      CompiledCodeText := DebugClientCompiledCodeText
    else
      CompiledCodeText := ExtractCompiledCodeText(CompiledCodeData[0]);

    InitializeAdminInstallMode(ufAdminInstallMode in UninstLog.Flags);

    { Initialize install mode }
    if UninstLog.InstallMode64Bit then begin
      { Sanity check: InstallMode64Bit should never be set without ufWin64 }
      if not IsWin64 then
        InternalError('Install was done in 64-bit mode but not running 64-bit Windows now');
      Initialize64BitInstallMode(True);
    end
    else
      Initialize64BitInstallMode(False);

    { Create temporary directory and extract 64-bit helper EXE if necessary }
    CreateTempInstallDirAndExtract64BitHelper;

    if CompiledCodeText <> '' then begin
      { Setup some global variables which are accessible to [Code] }

      InitMainNonSHFolderConsts;
      LoadSHFolderDLL;

      UninstallExeFilename := UninstExeFilename;
      UninstallExpandedAppId := UninstLog.AppId;
      UninstallSilent := Silent or VerySilent;

      UninstallExpandedApp := CompiledCodeData[2];
      UninstallExpandedGroup := CompiledCodeData[3];
      UninstallExpandedGroupName := CompiledCodeData[4];
      UninstallExpandedLanguage := CompiledCodeData[5];
      AssignCustomMessages(Pointer(CompiledCodeData[6]), Length(CompiledCodeData[6])*SizeOf(CompiledCodeData[6][1]));

      CodeRunner := TScriptRunner.Create();
      CodeRunner.NamingAttribute := CodeRunnerNamingAttribute;
      CodeRunner.OnLog := CodeRunnerOnLog;
      CodeRunner.OnLogFmt := CodeRunnerOnLogFmt;
      CodeRunner.OnDllImport := CodeRunnerOnDllImport;
      CodeRunner.OnDebug := CodeRunnerOnDebug;
      CodeRunner.OnDebugIntermediate := CodeRunnerOnDebugIntermediate;
      CodeRunner.OnException := CodeRunnerOnException;
      CodeRunner.LoadScript(CompiledCodeText, DebugClientCompiledCodeDebugInfo);
    end;
    try
      try
        if CodeRunner <> nil then begin
          try
            Res := CodeRunner.RunBooleanFunctions('InitializeUninstall', [''], bcFalse, False, True);
          except
            Log('InitializeUninstall raised an exception (fatal).');
            raise;
          end;
          if not Res then begin
            Log('InitializeUninstall returned False; aborting.');
            Abort;
          end;
        end;

        { Confirm uninstall }
        if not Silent and not VerySilent then begin
          if LoggedMessageBoxFmt1(msgConfirmUninstall, UninstLog.AppName, Title,
             MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON2, True, IDYES) <> IDYES then
            Abort;
        end;

        CurUninstallStepChanged(usAppMutexCheck, False);

        { Is the app running? }
        while UninstLog.CheckMutexes do
          { Yes, tell user to close it }
          if LoggedMessageBoxFmt1(msgUninstallAppRunningError, UninstLog.AppName, Title,
             MB_OKCANCEL or MB_ICONEXCLAMATION, True, IDCANCEL) <> IDOK then
            Abort;
   
        { Check for active WM_QUERYENDSESSION/WM_ENDSESSION }
        while AcceptedQueryEndSessionInProgress do begin
          Sleep(10);
          Application.ProcessMessages;
        end;

        { Disable Uninstall shutdown }
        AllowUninstallerShutdown := False;
        ShutdownBlockReasonCreate(Application.Handle,
          FmtSetupMessage1(msgShutdownBlockReasonUninstallingApp, UninstLog.AppName));

        { Create and show the progress form }
        InitializeUninstallProgressForm;

        CurUninstallStepChanged(usUninstall, False);

        { Start the actual uninstall process }
        StartTime := GetTickCount;
        UninstLog.FLastUpdateTime := StartTime;
        RemovedAll := UninstLog.PerformUninstall(True, DeleteUninstallDataFiles);

        LogFmt('Removed all? %s', [SYesNo[RemovedAll]]);

        UninstallNeedsRestart := UninstLog.NeedRestart or (ufAlwaysRestart in UninstLog.Flags);
        if (CodeRunner <> nil) and CodeRunner.FunctionExists('UninstallNeedRestart', True) then begin
          if not UninstallNeedsRestart then begin
            try
              if CodeRunner.RunBooleanFunctions('UninstallNeedRestart', [''], bcTrue, False, False) then begin
                UninstallNeedsRestart := True;
                Log('Will restart because UninstallNeedRestart returned True.');
              end;
            except
              Log('UninstallNeedRestart raised an exception.');
              ShowExceptionMsg;
            end;
          end
          else
            Log('Not calling UninstallNeedRestart because a restart has already been deemed necessary.');
        end;

        LogFmt('Need to restart Windows? %s', [SYesNo[UninstallNeedsRestart]]);

        { Ensure at least 1 second has passed since the uninstall process
          began, then destroy the form }
        if not VerySilent then begin
          while True do begin
            Application.ProcessMessages;
            if Cardinal(GetTickCount - StartTime) >= Cardinal(1000) then
              Break;
            { Delay for 10 ms, or until a message arrives }
            MsgWaitForMultipleObjects(0, THandle(nil^), False, 10, QS_ALLINPUT);
          end;
        end;
        FreeAndNil(UninstallProgressForm);

        CurUninstallStepChanged(usPostUninstall, True);

        if not UninstallNeedsRestart then begin
          if not Silent and not VerySilent then
            LoggedMessageBoxFmt1(RemovedMsgs[RemovedAll], UninstLog.AppName,
              Title, MB_ICONINFORMATION or MB_OK or MB_SETFOREGROUND, True, IDOK);
        end
        else begin
          if not NoRestart then begin
            if VerySilent or
               (LoggedMessageBoxFmt1(msgUninstalledAndNeedsRestart, UninstLog.AppName,
                  Title, MB_ICONQUESTION or MB_YESNO or MB_SETFOREGROUND, True, IDYES) = IDYES) then
              RestartSystem := True;
          end;
          if not RestartSystem then
            Log('Will not restart Windows automatically.');
        end;

        CurUninstallStepChanged(usDone, True);
      except
        { Show any pending exception here *before* DeinitializeUninstall
          is called, which could display an exception message of its own }
        ShowExceptionMsg;
        Abort;
      end;
    finally
      { Free the form here, too, in case an exception occurred above }
      try
        FreeAndNil(UninstallProgressForm);
      except
        ShowExceptionMsg;
      end;
      if CodeRunner <> nil then begin
        try
          CodeRunner.RunProcedures('DeinitializeUninstall', [''], False);
        except
          Log('DeinitializeUninstall raised an exception.');
          ShowExceptionMsg;
        end;
      end;
      ShutdownBlockReasonDestroy(Application.Handle);
    end;
  finally
    FreeAndNil(CodeRunner);
    UnloadSHFolderDLL;
    RemoveTempInstallDir;
    UninstLog.Free;
    FreeAndNil(UninstDataFile);
  end;

  if RestartSystem then begin
    if not Debugging then begin
      Log('Restarting Windows.');
      RestartInitiatedByThisProcess := True;
      if not RestartComputer then begin
        LoggedAppMessageBox(PChar(SetupMessages[msgErrorRestartingComputer]),
          PChar(SetupMessages[msgErrorTitle]), MB_OK or MB_ICONEXCLAMATION,
          True, IDOK);
      end;
    end
    else
      Log('Not restarting Windows because Uninstall is being run from the debugger.');
  end;
end;

procedure RunUninstaller;
var
  F: TFile;
  UninstallerMsgTail: TUninstallerMsgTail;
begin
  { Set default title; it's set again below after the messages are read }
  Application.Title := 'Uninstall';
  Application.MainFormOnTaskBar := True;

  try
    InitializeCommonVars;

    SetCurrentDir(GetSystemDir);

    UninstExeFilename := NewParamStr(0);
    ProcessCommandLine;  { note: may change UninstExeFile }
    UninstDataFilename := PathChangeExt(UninstExeFilename, '.dat');
    UninstMsgFilename := PathChangeExt(UninstExeFilename, '.msg');

    { Initialize messages }
    F := TFile.Create(UninstExeFilename, fdOpenExisting, faRead, fsRead);
    try
      F.Seek(F.Size.Lo - SizeOf(UninstallerMsgTail));
      F.ReadBuffer(UninstallerMsgTail, SizeOf(UninstallerMsgTail));
      if UninstallerMsgTail.ID <> UninstallerMsgTailID then begin
        { No valid UninstallerMsgTail record found at the end of the EXE;
          load messages from an external .msg file. }
        LoadSetupMessages(UninstMsgFilename, 0, True);
      end
      else
        LoadSetupMessages(UninstExeFilename, UninstallerMsgTail.Offset, True);
    finally
      F.Free;
    end;
    LangOptions.DialogFontName := MessagesLangOptions.DialogFontName;
    LangOptions.DialogFontSize := MessagesLangOptions.DialogFontSize;
    LangOptions.RightToLeft := lfRightToLeft in MessagesLangOptions.Flags;
    SetMessageBoxRightToLeft(LangOptions.RightToLeft);
    SetMessageBoxCaption(mbInformation, PChar(SetupMessages[msgInformationTitle]));
    SetMessageBoxCaption(mbConfirmation, PChar(SetupMessages[msgConfirmTitle]));
    SetMessageBoxCaption(mbError, PChar(SetupMessages[msgErrorTitle]));
    SetMessageBoxCaption(mbCriticalError, PChar(SetupMessages[msgErrorTitle]));
    Application.Title := SetupMessages[msgUninstallAppTitle];

    { Verify that uninstall data file exists }
    if not NewFileExists(UninstDataFilename) then begin
      LoggedMessageBoxFmt1(msgUninstallNotFound, UninstDataFilename,
        SetupMessages[msgUninstallAppTitle], MB_ICONSTOP or MB_OK, True, IDOK);
      Abort;
    end;

    if not SecondPhase then begin
      if not RespawnFirstPhaseIfNeeded then
        RunFirstPhase;
    end
    else
      RunSecondPhase;
  except
    ShowExceptionMsg;
  end;

  { Call EndDebug after all exception messages have been shown and logged in
    the IDE's Debug Output }
  EndDebug;

  Halt(UninstallExitCode);
end;

procedure HandleUninstallerEndSession;
begin
  { If second phase, remove the temp dir. The self copy made by the first phase will be
    deleted on restart. }
  if SecondPhase then begin
    Log('Detected restart. Removing temporary directory.');

    try
      UnloadSHFolderDLL;
      RemoveTempInstallDir;
    except
      ShowExceptionMsg;
    end;

    EndDebug;

    { Don't use Halt. See Setup.dpr }
    TerminateProcess(GetCurrentProcess, UninstallExitCode);
  end;
end;

end.
