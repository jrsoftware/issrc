unit Setup.MainForm;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

interface

uses
  Windows, SysUtils, Classes,
  Shared.SetupSteps;

type
  TMainForm = class(TComponent)
  private
    class procedure AppOnGetActiveFormHandle(var AHandle: HWND);
  public
    CurStep: TSetupStep;
    destructor Destroy; override;
    procedure Close;
    procedure Finish(const FromPreparingPage: Boolean);
    function Install: Boolean;
    procedure SetStep(const AStep: TSetupStep; const HandleExceptions: Boolean);
    class procedure ShowException(Sender: TObject; E: Exception);
    class procedure ShowExceptionMsg(const S: String);
  end;

var
  MainForm: TMainForm;

implementation

uses
  Messages, ShlObj,
  Forms,
  SHA256, RestartManager,
  Shared.Struct, Shared.CommonFunc, Shared.CommonFunc.Vcl, Shared.SetupMessageIDs,
  SetupLdrAndSetup.Messages, Setup.Install,
  Setup.MainFunc, Setup.InstFunc, Setup.WizardForm, Setup.LoggingFunc, Shared.SetupTypes;

destructor TMainForm.Destroy;
begin
  MainForm := nil;  { just to detect use-after-free }
  inherited;
end;

class procedure TMainForm.ShowExceptionMsg(const S: String);
begin
  Log('Exception message:');
  LoggedAppMessageBox(PChar(S), PChar(Application.Title), MB_OK or MB_ICONSTOP, True, IDOK);
end;

class procedure TMainForm.ShowException(Sender: TObject; E: Exception);
begin
  ShowExceptionMsg(AddPeriod(E.Message));
end;

procedure TMainForm.SetStep(const AStep: TSetupStep; const HandleExceptions: Boolean);
begin
  CurStep := AStep;
  if CodeRunner <> nil then begin
    try
      CodeRunner.RunProcedures('CurStepChanged', [Ord(CurStep)], False);
    except
      if HandleExceptions then begin
        Log('CurStepChanged raised an exception.');
        Application.HandleException(Self);
      end
      else begin
        Log('CurStepChanged raised an exception (fatal).');
        raise;
      end;
    end;
  end;
end;

procedure TerminateApp;
begin
  { Work around shell32 bug: Don't use PostQuitMessage/Application.Terminate
    here.
    When ShellExecute is called with the name of a folder, it internally
    creates a window used for DDE communication with Windows Explorer. After
    ShellExecute returns, this window eventually receives a posted WM_DDE_ACK
    message back from the DDE server (Windows Explorer), and in response, it
    tries to flush the queue of DDE messages by using a PeekMessage loop.
    Problem is, PeekMessage will return WM_QUIT messages posted with
    PostQuitMessage regardless of the message range specified, and the loop was
    not written with this in mind.
    In previous IS versions, this was causing our WM_QUIT message to be eaten
    if Application.Terminate was called very shortly after a shellexec [Run]
    entry was processed (e.g. if DisableFinishedPage=yes).
    A WM_QUIT message posted with PostMessage instead of PostQuitMessage will
    not be returned by a GetMessage/PeekMessage call with a message range that
    does not include WM_QUIT. }
  PostMessage(0, WM_QUIT, 0, 0);
end;

function TMainForm.Install: Boolean;

  procedure ProcessRunEntries;
  var
    CheckIfRestartNeeded: Boolean;
    ChecksumBefore, ChecksumAfter: TSHA256Digest;
    WindowDisabler: TWindowDisabler;
    I: Integer;
    RunEntry: PSetupRunEntry;
  begin
    if Entries[seRun].Count <> 0 then begin
      CheckIfRestartNeeded := (shRestartIfNeededByRun in SetupHeader.Options) and
        not NeedsRestart;
      if CheckIfRestartNeeded then
        ChecksumBefore := MakePendingFileRenameOperationsChecksum;
      var WizardWasHidden := False;
      WindowDisabler := nil;
      try
        for I := 0 to Entries[seRun].Count-1 do begin
          RunEntry := PSetupRunEntry(Entries[seRun][I]);
          if not(roPostInstall in RunEntry.Options) and
             ShouldProcessRunEntry(WizardComponents, WizardTasks, RunEntry) then begin
            { Disable windows during execution of [Run] entries so that a nice
              "beep" is produced if the user tries clicking on WizardForm }
            if WindowDisabler = nil then
              WindowDisabler := TWindowDisabler.Create;
            if RunEntry.StatusMsg <> '' then begin
              try
                WizardForm.StatusLabel.Caption := ExpandConst(RunEntry.StatusMsg);
              except
                { Don't die if the expansion fails with an exception. Just
                  display the exception message, and proceed with the default
                  status message. }
                Application.HandleException(Self);
                WizardForm.StatusLabel.Caption := SetupMessages[msgStatusRunProgram];
              end;
            end
            else
              WizardForm.StatusLabel.Caption := SetupMessages[msgStatusRunProgram];
            WizardForm.StatusLabel.Update;
            if roHideWizard in RunEntry.Options then begin
              if WizardForm.Visible and not WizardWasHidden then begin
                WizardWasHidden := True;
                WizardForm.Hide;
              end;
            end
            else begin
              if WizardWasHidden then begin
                WizardWasHidden := False;
                WizardForm.Visible := True;
              end;
            end;
            DebugNotifyEntry(seRun, I);
            NotifyBeforeInstallEntry(RunEntry.BeforeInstall);
            ProcessRunEntry(RunEntry);
            NotifyAfterInstallEntry(RunEntry.AfterInstall);
          end;
        end;
      finally
        if WizardWasHidden then
          WizardForm.Visible := True;
        WindowDisabler.Free;
        if CheckIfRestartNeeded then begin
          ChecksumAfter := MakePendingFileRenameOperationsChecksum;
          if not SHA256DigestsEqual(ChecksumBefore, ChecksumAfter) then
            NeedsRestart := True;
        end;
      end;
      if WizardForm.WindowState <> wsMinimized then  { VCL bug workaround }
        Application.BringToFront;
    end;
  end;

  procedure RestartApplications;
  const
    ERROR_FAIL_RESTART = 353;
  var
    Error: DWORD;
    WindowDisabler: TWindowDisabler;
  begin
    if not NeedsRestart then begin
      WizardForm.StatusLabel.Caption := SetupMessages[msgStatusRestartingApplications];
      WizardForm.StatusLabel.Update;

      Log('Attempting to restart applications.');

      { Disable windows during application restart so that a nice
        "beep" is produced if the user tries clicking on WizardForm }
      WindowDisabler := TWindowDisabler.Create;
      try
        Error := RmRestart(RmSessionHandle, 0, nil);
      finally
        WindowDisabler.Free;
      end;
      if WizardForm.WindowState <> wsMinimized then  { VCL bug workaround }
        Application.BringToFront;

      if Error = ERROR_FAIL_RESTART then
        Log('One or more applications could not be restarted.')
      else if Error <> ERROR_SUCCESS then begin
        RmEndSession(RmSessionHandle);
        RmSessionStarted := False;
        LogFmt('RmRestart returned an error: %d', [Error]);
      end;
    end else
      Log('Need to restart Windows, not attempting to restart applications');
  end;

var
  Succeeded, ChangesEnvironment, ChangesAssociations: Boolean;
  S: String;
begin
  Result := False;
  try
    if not WizardForm.ValidateDirEdit then
      Abort;
    WizardDirValue := WizardForm.DirEdit.Text;
    if not WizardForm.ValidateGroupEdit then
      Abort;
    WizardGroupValue := WizardForm.GroupEdit.Text;
    WizardNoIcons := WizardForm.NoIconsCheck.Checked;
    WizardSetupType := WizardForm.GetSetupType();
    WizardForm.GetComponents(WizardComponents, WizardDeselectedComponents);
    WizardForm.GetTasks(WizardTasks, WizardDeselectedTasks);
    WizardPreparingYesRadio := WizardForm.PreparingYesRadio.Checked;
    if InitSaveInf <> '' then
      SaveInf(InitSaveInf);

    Application.Restore;
    if InstallMode = imSilent then
      WizardForm.Visible := True;
    WizardForm.Update;

    SetStep(ssInstall, False);
    
    ChangesEnvironment := EvalDirectiveCheck(SetupHeader.ChangesEnvironment);
    ChangesAssociations := EvalDirectiveCheck(SetupHeader.ChangesAssociations);

    PerformInstall(Succeeded, ChangesEnvironment, ChangesAssociations);
    if not Succeeded then begin
      { The user canceled the install or there was a fatal error }
      TerminateApp;
      Exit;
    end;
    { Can't cancel at any point after PerformInstall, so disable the button }
    WizardForm.CancelButton.Enabled := False;

    ProcessRunEntries;

    if RmDoRestart and
       (InitRestartApplications or
        ((shRestartApplications in SetupHeader.Options) and not InitNoRestartApplications)) then
      RestartApplications;

    SetStep(ssPostInstall, True);

    { Notify Windows of assocations/environment changes *after* ssPostInstall
      since user might set more stuff there }
    if ChangesAssociations then
      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
    if ChangesEnvironment then
      RefreshEnvironment;

    if InstallMode <> imNormal then
      WizardForm.Hide;

    LogFmt('Need to restart Windows? %s', [SYesNo[NeedsRestart]]);
    if NeedsRestart and not InitNoRestart then begin
      with WizardForm do begin
        ChangeFinishedLabel(ExpandSetupMessage(msgFinishedRestartLabel));
        YesRadio.Visible := True;
        NoRadio.Visible := True;
      end;
    end else begin
      if CreatedIcon then
        S := ExpandSetupMessage(msgFinishedLabel)
      else
        S := ExpandSetupMessage(msgFinishedLabelNoIcons);
      with WizardForm do begin
        ChangeFinishedLabel(S + SNewLine2 + SetupMessages[msgClickFinish]);
        if not NeedsRestart then begin
          UpdateRunList(WizardComponents, WizardTasks);
          RunList.Visible := RunList.Items.Count > 0;
        end;
      end;
    end;

    if InstallMode = imNormal then
      Application.Restore;

    Result := True;
  except
    { If an exception was raised, display the message, then terminate }
    Application.HandleException(Self);
    SetupExitCode := ecNextStepError;
    TerminateApp;
  end;
end;

procedure ProcessMessagesProc; far;
begin
  Application.ProcessMessages;
end;

procedure TMainForm.Finish(const FromPreparingPage: Boolean);

  procedure WaitForForegroundLoss;

    function IsForegroundProcess: Boolean;
    var
      W: HWND;
      PID: DWORD;
    begin
      W := GetForegroundWindow;
      Result := False;
      if (W <> 0) and (GetWindowThreadProcessId(W, @PID) <> 0) then
        Result := (PID = GetCurrentProcessId);
    end;

  var
    StartTick: DWORD;
  begin
    StartTick := GetTickCount;
    while IsForegroundProcess do begin
      { Stop if it's taking too long (e.g. if the spawned process never
        displays a window) }
      if Cardinal(GetTickCount - StartTick) >= Cardinal(1000) then
        Break;
      ProcessMessagesProc;
      WaitMessageWithTimeout(10);
      ProcessMessagesProc;
    end;
  end;

  procedure ProcessPostInstallRunEntries;
  var
    WindowDisabler: TWindowDisabler;
    ProcessedNoWait: Boolean;
    I: Integer;
    RunEntry: PSetupRunEntry;
  begin
    WindowDisabler := nil;
    try
      ProcessedNoWait := False;
      with WizardForm do begin
        for I := 0 to RunList.Items.Count-1 do begin
          if RunList.Checked[I] then begin
            { Disable windows before processing the first entry }
            if WindowDisabler = nil then
              WindowDisabler := TWindowDisabler.Create;
            RunEntry := PSetupRunEntry(Entries[seRun][Integer(RunList.ItemObject[I])]);
            DebugNotifyEntry(seRun, Integer(RunList.ItemObject[I]));
            NotifyBeforeInstallEntry(RunEntry.BeforeInstall);
            ProcessRunEntry(RunEntry);
            NotifyAfterInstallEntry(RunEntry.AfterInstall);
            if RunEntry.Wait = rwNoWait then
              ProcessedNoWait := True;
          end;
        end;
      end;
      { Give nowait processes some time to bring themselves to the
        foreground before Setup exits. Without this delay, the application
        underneath Setup can end up coming to the foreground instead.
        (Note: Windows are already disabled at this point.) }
      if ProcessedNoWait then
        WaitForForegroundLoss;
    finally
      WindowDisabler.Free;
    end;
  end;

var
  S: String;
begin
  try
    { Deactivate WizardForm so another application doesn't come to the
      foreground when Hide is called. (Needed by WaitForForegroundLoss.) }
    if GetForegroundWindow = WizardForm.Handle then
      SetActiveWindow(Application.Handle);
    WizardForm.Hide;

    if not FromPreparingPage and not NeedsRestart then begin
      ProcessPostInstallRunEntries;
    end else begin
      if FromPreparingPage then
        SetupExitCode := ecPrepareToInstallFailedRestartNeeded
      else if InitRestartExitCode <> 0 then
        SetupExitCode := InitRestartExitCode;

      if InitNoRestart then
        RestartSystem := False
      else begin
        case InstallMode of
          imNormal:
            if FromPreparingPage then
              RestartSystem := WizardForm.PreparingYesRadio.Checked
            else
              RestartSystem := WizardForm.YesRadio.Checked;
          imSilent:
            begin
              if FromPreparingPage then
                S := WizardForm.PrepareToInstallFailureMessage + SNewLine +
                  SNewLine + SNewLine + ExpandSetupMessage(msgPrepareToInstallNeedsRestart)
              else
                S := ExpandSetupMessage(msgFinishedRestartMessage);
              RestartSystem :=
                LoggedMsgBox(S, '', mbConfirmation, MB_YESNO, True, IDYES) = IDYES;
            end;
          imVerySilent:
            RestartSystem := True;
        end;
      end;
      if not RestartSystem then
        Log('Will not restart Windows automatically.');
    end;

    SetStep(ssDone, True);
  except
    Application.HandleException(Self);
    SetupExitCode := ecNextStepError;
  end;
  TerminateApp;
end;

procedure TMainForm.Close;

  function ConfirmCancel(const DefaultConfirm: Boolean): Boolean;
  var
    Cancel, Confirm: Boolean;
  begin
    Cancel := True;
    Confirm := DefaultConfirm;
    WizardForm.CallCancelButtonClick(Cancel, Confirm);
    Result := Cancel and (not Confirm or ExitSetupMsgBox);
  end;

begin
  if Assigned(WizardForm) and WizardForm.HandleAllocated and
     IsWindowVisible(WizardForm.Handle) and IsWindowEnabled(WizardForm.Handle) and
     WizardForm.CancelButton.CanFocus then begin
    case CurStep of
      ssPreInstall:
        if ConfirmCancel((WizardForm.CurPageID <> wpPreparing) or (WizardForm.PrepareToInstallFailureMessage = '')) then begin
          if WizardForm.CurPageID = wpPreparing then
            SetupExitCode := ecPrepareToInstallFailed
          else
            SetupExitCode := ecCancelledBeforeInstall;
          TerminateApp;
        end;
      ssInstall:
        if (shAllowCancelDuringInstall in SetupHeader.Options) and not InitNoCancel then
          if ConfirmCancel(True) then
            NeedToAbortInstall := True;
    end;
  end;
end;

class procedure TMainForm.AppOnGetActiveFormHandle(var AHandle: HWND);
begin
  { IDE's TMainForm has this too; see comments there }
  if Application.MainFormOnTaskBar then begin
    AHandle := GetActiveWindow;
    if ((AHandle = 0) or (AHandle = Application.Handle)) and
       Assigned(Application.MainForm) and
       Application.MainForm.HandleAllocated then
      AHandle := GetLastActivePopup(Application.MainFormHandle);
  end;
end;

initialization
  Application.OnGetActiveFormHandle := TMainForm.AppOnGetActiveFormHandle;
end.
