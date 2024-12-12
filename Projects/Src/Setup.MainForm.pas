unit Setup.MainForm;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Background form
}

interface

uses
  Windows, Messages, SysUtils, Classes,
  Shared.Struct, Setup.MainFunc, Setup.SetupForm, Shared.SetupSteps;

type
  TMainForm = class(TSetupForm)
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormPaint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    IsMinimized, HideWizard: Boolean;
    class procedure AppOnGetActiveFormHandle(var AHandle: HWND);
    function MainWindowHook(var Message: TMessage): Boolean;
    procedure UpdateWizardFormVisibility(const IgnoreMinimizedState: Boolean = False);
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
  public
    CurStep: TSetupStep;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Finish(const FromPreparingPage: Boolean);
    procedure InitializeWizard;
    function Install: Boolean;
    procedure RestoreApp;
    procedure SetStep(const AStep: TSetupStep; const HandleExceptions: Boolean);
    class procedure ShowException(Sender: TObject; E: Exception);
    class procedure ShowExceptionMsg(const S: String);
    procedure ShowAboutBox;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Forms, Graphics, ShlObj, SHA256, RestartManager,
  Shared.CommonFunc, Shared.CommonFunc.Vcl, Shared.SetupMessageIDs,
  SetupLdrAndSetup.Messages, SetupLdrAndSetup.RedirFunc, Setup.Install,
  Setup.InstFunc, Setup.WizardForm, Setup.LoggingFunc, Shared.SetupTypes;

{$R *.DFM}

constructor TMainForm.Create(AOwner: TComponent);
var
  SystemMenu: HMenu;
begin
  inherited;

  InitializeFont;

  if shWindowVisible in SetupHeader.Options then begin
    { Should the main window not be sizable? }
    if not(shWindowShowCaption in SetupHeader.Options) then
      BorderStyle := bsNone
    else
    if not(shWindowResizable in SetupHeader.Options) then
      BorderStyle := bsSingle;

    { Make the main window full-screen. If the window is resizable, limit it
      to just the work area because full-screen resizable windows don't cover
      over the taskbar. }
    BoundsRect := GetRectOfPrimaryMonitor(BorderStyle = bsSizeable);
    { Before maximizing the window, ensure Handle is created now so the correct
      'restored' position is saved properly }
    HandleNeeded;

    { Maximize the window so that the taskbar is still accessible }
    if shWindowStartMaximized in SetupHeader.Options then
      WindowState := wsMaximized;
  end
  else begin
    Application.ShowMainForm := False;
  end;

  if shDisableWelcomePage in SetupHeader.Options then
    Caption := FmtSetupMessage1(msgSetupWindowTitle, ExpandedAppVerName)
  else
    Caption := FmtSetupMessage1(msgSetupWindowTitle, ExpandedAppName);

  { Append the 'About Setup' item to the system menu }
  SystemMenu := GetSystemMenu(Handle, False);
  AppendMenu(SystemMenu, MF_SEPARATOR, 0, nil);
  AppendMenu(SystemMenu, MF_STRING, 9999, PChar(SetupMessages[msgAboutSetupMenuItem]));

  Application.HookMainWindow(MainWindowHook);

  if Application.ShowMainForm then
    { Show this form now, so that the focus stays on the wizard form that
      InitializeWizard (called in the .dpr) shows }
    Visible := True;
end;

destructor TMainForm.Destroy;
begin
  Application.UnhookMainWindow(MainWindowHook);
  inherited;
end;

procedure TMainForm.WMSysCommand(var Message: TWMSysCommand);
begin
  if Message.CmdType = 9999 then
    ShowAboutBox
  else
    inherited;
end;

procedure TMainForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  { Since the form paints its entire client area in FormPaint, there is
    no need for the VCL to ever erase the client area with the brush color.
    Doing so only slows it down, so this message handler disables that default
    behavior. }
  Message.Result := 0;
end;

procedure TMainForm.FormPaint(Sender: TObject);

  function BlendRGB(const Color1, Color2: TColor; const Blend: Integer): TColor;
  { Blends Color1 and Color2. Blend must be between 0 and 255; 0 = all Color1,
    255 = all Color2. }
  type
    TColorBytes = array[0..3] of Byte;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to 2 do
      TColorBytes(Result)[I] := Integer(TColorBytes(Color1)[I] +
        ((TColorBytes(Color2)[I] - TColorBytes(Color1)[I]) * Blend) div 255);
  end;

var
  C1, C2: TColor;
  CS: TPoint;
  Z: Integer;
  DrawTextFlags: UINT;
  R, R2: TRect;
begin
  with Canvas do begin
    { Draw the blue background }
    if SetupHeader.BackColor = SetupHeader.BackColor2 then begin
      Brush.Color := SetupHeader.BackColor;
      FillRect(ClientRect);
    end
    else begin
      C1 := ColorToRGB(SetupHeader.BackColor);
      C2 := ColorToRGB(SetupHeader.BackColor2);
      CS := ClientRect.BottomRight;
      for Z := 0 to 255 do begin
        Brush.Color := BlendRGB(C1, C2, Z);
        if not(shBackColorHorizontal in SetupHeader.Options) then
          FillRect(Rect(0, MulDiv(CS.Y, Z, 255), CS.X, MulDiv(CS.Y, Z+1, 255)))
        else
          FillRect(Rect(MulDiv(CS.X, Z, 255), 0, MulDiv(CS.X, Z+1, 255), CS.Y));
      end;
    end;

    { Draw the application name and copyright }
    SetBkMode(Handle, TRANSPARENT);

    DrawTextFlags := DT_WORDBREAK or DT_NOPREFIX or DT_NOCLIP;
    if RightToLeft then
      DrawTextFlags := DrawTextFlags or (DT_RIGHT or DT_RTLREADING);
    SetFontNameSize(Font, LangOptions.TitleFontName,
      LangOptions.TitleFontSize, 'Arial', 29);
    if IsMultiByteString(AnsiString(ExpandedAppName)) then
      { Don't use italics on Japanese characters }
      Font.Style := [fsBold]
    else
      Font.Style := [fsBold, fsItalic];
    R := ClientRect;
    InflateRect(R, -8, -8);
    R2 := R;
    if RightToLeft then
      OffsetRect(R2, -4, 4)
    else
      OffsetRect(R2, 4, 4);
    Font.Color := clBlack;
    DrawText(Handle, PChar(ExpandedAppName), -1, R2, DrawTextFlags);
    Font.Color := clWhite;
    DrawText(Handle, PChar(ExpandedAppName), -1, R, DrawTextFlags);

    DrawTextFlags := DrawTextFlags xor DT_RIGHT;
    SetFontNameSize(Font, LangOptions.CopyrightFontName,
      LangOptions.CopyrightFontSize, 'Arial', 8);
    Font.Style := [];
    R := ClientRect;
    InflateRect(R, -6, -6);
    R2 := R;
    DrawText(Handle, PChar(ExpandedAppCopyright), -1, R2, DrawTextFlags or
      DT_CALCRECT);
    R.Top := R.Bottom - (R2.Bottom - R2.Top);
    R2 := R;
    if RightToLeft then
      OffsetRect(R2, -1, 1)
    else
      OffsetRect(R2, 1, 1);
    Font.Color := clBlack;
    DrawText(Handle, PChar(ExpandedAppCopyright), -1, R2, DrawTextFlags);
    Font.Color := clWhite;
    DrawText(Handle, PChar(ExpandedAppCopyright), -1, R, DrawTextFlags);
  end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  { Needs to redraw the background whenever the form is resized }
  Repaint;
end;

procedure TMainForm.ShowAboutBox;
var
  S: String;
begin
  { Removing the About box or modifying any existing text inside it is a
    violation of the Inno Setup license agreement; see LICENSE.TXT.
    However, adding additional lines to the end of the About box is
    permitted. }
  S := SetupTitle + ' version ' + SetupVersion + SNewLine;
  if SetupTitle <> 'Inno Setup' then
    S := S + (SNewLine + 'Based on Inno Setup' + SNewLine);
  S := S + ('Copyright (C) 1997-2024 Jordan Russell' + SNewLine +
    'Portions Copyright (C) 2000-2024 Martijn Laan' + SNewLine +
    'All rights reserved.' + SNewLine2 +
    'Inno Setup home page:' + SNewLine +
    'https://www.innosetup.com/');
  S := S + SNewLine2 + 'RemObjects Pascal Script home page:' + SNewLine +
    'https://www.remobjects.com/ps';
  if SetupMessages[msgAboutSetupNote] <> '' then
    S := S + SNewLine2 + SetupMessages[msgAboutSetupNote];
  if SetupMessages[msgTranslatorNote] <> '' then
    S := S + SNewLine2 + SetupMessages[msgTranslatorNote];
  StringChangeEx(S, '(C)', #$00A9, True);
  LoggedMsgBox(S, SetupMessages[msgAboutSetupTitle], mbInformation, MB_OK, False, 0);
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

procedure TMainForm.InitializeWizard;
begin
  WizardForm := TWizardForm.Create(Application);
  if CodeRunner <> nil then begin
    try
      CodeRunner.RunProcedures('InitializeWizard', [''], False);
    except
      Log('InitializeWizard raised an exception (fatal).');
      raise;
    end;
  end;
  WizardForm.FlipSizeAndCenterIfNeeded(shWindowVisible in SetupHeader.Options, MainForm, True);
  WizardForm.SetCurPage(wpWelcome);
  if InstallMode = imNormal then begin
    WizardForm.ClickToStartPage; { this won't go past wpReady  }
    SetActiveWindow(Application.Handle);  { ensure taskbar button is selected }
    WizardForm.Show;
  end
  else
    WizardForm.ClickThroughPages;
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
              if WizardForm.Visible and not HideWizard then begin
                HideWizard := True;
                UpdateWizardFormVisibility;
              end;
            end
            else begin
              if HideWizard then begin
                HideWizard := False;
                UpdateWizardFormVisibility;
              end;
            end;
            DebugNotifyEntry(seRun, I);
            NotifyBeforeInstallEntry(RunEntry.BeforeInstall);
            ProcessRunEntry(RunEntry);
            NotifyAfterInstallEntry(RunEntry.AfterInstall);
          end;
        end;
      finally
        if HideWizard then begin
          HideWizard := False;
          UpdateWizardFormVisibility;
        end;
        WindowDisabler.Free;
        if CheckIfRestartNeeded then begin
          ChecksumAfter := MakePendingFileRenameOperationsChecksum;
          if not SHA256DigestsEqual(ChecksumBefore, ChecksumAfter) then
            NeedsRestart := True;
        end;
      end;
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
    Update;
    if InstallMode = imSilent then begin
      SetActiveWindow(Application.Handle);  { ensure taskbar button is selected }
      WizardForm.Show;
    end;
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

    if InstallMode = imNormal then begin
      Application.Restore;
      Update;
    end;

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

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

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
  { Note: Setting CanClose to True causes Application.Terminate to be called;
    we don't want that. }
  CanClose := False;
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

procedure TMainForm.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := Message.Result or DLGC_WANTTAB;
end;

function EWP(Wnd: HWND; Param: LPARAM): BOOL; stdcall;
begin
  { Note: GetParent is not used here because the other windows are not
    actually child windows since they don't have WS_CHILD set. }
  if GetWindowLong(Wnd, GWL_HWNDPARENT) <> Param then
    Result := True
  else begin
    Result := False;
    BringWindowToTop(Wnd);
  end;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { If, for some reason, the user doesn't have a mouse and the main form was
    activated, there would normally be no way to reactivate the child form.
    But this reactivates the form if the user hits a key on the keyboard }
  if not(ssAlt in Shift) then begin
    Key := 0;
    EnumThreadWindows(GetCurrentThreadId, @EWP, Handle);
  end;
end;

procedure TMainForm.UpdateWizardFormVisibility(
  const IgnoreMinimizedState: Boolean = False);
var
  ShouldShow: Boolean;
begin
  { Note: We don't adjust WizardForm.Visible because on Delphi 3+, if all forms
    have Visible set to False, the application taskbar button disappears. }
  if Assigned(WizardForm) and WizardForm.HandleAllocated then begin
    ShouldShow := WizardForm.Showing and not HideWizard and
      (IgnoreMinimizedState or not IsIconic(Application.Handle));
    if (GetWindowLong(WizardForm.Handle, GWL_STYLE) and WS_VISIBLE <> 0) <> ShouldShow then begin
      if ShouldShow then
        ShowWindow(WizardForm.Handle, SW_SHOW)
      else
        ShowWindow(WizardForm.Handle, SW_HIDE);
    end;
  end;
end;

function TMainForm.MainWindowHook(var Message: TMessage): Boolean;
var
  IsIcon: Boolean;
begin
  Result := False;
  case Message.Msg of
    WM_WINDOWPOSCHANGED: begin
        { When the application window is minimized or restored, also hide or
          show WizardForm.
          Note: MainForm is hidden/shown automatically because its owner
          window is Application.Handle. }
        IsIcon := IsIconic(Application.Handle);
        if IsMinimized <> IsIcon then begin
          IsMinimized := IsIcon;
          UpdateWizardFormVisibility;
        end;
      end;
  end;
end;

procedure TMainForm.WMShowWindow(var Message: TWMShowWindow);
begin
  inherited;
  { When showing, ensure WizardForm is the active window, not MainForm }
  if Message.Show and (GetActiveWindow = Handle) and
     Assigned(WizardForm) and WizardForm.HandleAllocated and
     IsWindowVisible(WizardForm.Handle) then
    SetActiveWindow(WizardForm.Handle);
end;

procedure TMainForm.RestoreApp;
{ Restores the app if it is currently minimized, and tries to make its taskbar
  button blink (by attempting to bring it to the foreground, which Windows
  normally blocks). This should be called before displaying any dialogs that
  aren't user-initiated (like NewDiskForm). }
begin
  if IsIconic(Application.Handle) then begin
    { If called alone, Application.Restore annoyingly brings WizardForm to the
      foreground even if you're actively clicking/typing in the foreground
      app. Evidently the SW_RESTORE command used by Application.Restore
      bypasses Windows' usual foreground-stealing protections. However, if
      we show WizardForm in advance (and leave the application window still
      minimized), then SW_RESTORE doesn't bring WizardForm to the foreground
      (not sure why).
      Calling ShowWindow(Application.Handle, SW_SHOWNOACTIVATE) before
      Application.Restore also works, but I worry that's relying on an
      implementation detail: Application.Restore could be a no-op if it finds
      the application window isn't minimized. (In fact, it used to be, until
      the Forms unit added that fake IsIconic function.) }
    UpdateWizardFormVisibility(True);
    Application.Restore;
  end;
  Application.BringToFront;
end;

class procedure TMainForm.AppOnGetActiveFormHandle(var AHandle: HWND);
begin
  { IDE's TMainForm has this too; see comments there }
  if Assigned(Screen.ActiveForm) and
     (Screen.ActiveForm.FormStyle <> fsMDIChild) and
     Screen.ActiveForm.HandleAllocated then
    AHandle := Screen.ActiveForm.Handle;
end;

initialization
  Application.OnGetActiveFormHandle := TMainForm.AppOnGetActiveFormHandle;
end.
