program Setup;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Setup program
}

uses
  SafeDLLPath in 'Src\SafeDLLPath.pas',
  XPTheme in 'Src\XPTheme.pas',
  Forms,
  Windows,
  SysUtils,
  Messages,
  RichEditViewer in '..\Components\RichEditViewer.pas',
  CmnFunc in 'Src\CmnFunc.pas',
  CmnFunc2 in 'Src\CmnFunc2.pas',
  Main in 'Src\Main.pas' {MainForm},
  Install in 'Src\Install.pas',
  Msgs in 'Src\Msgs.pas',
  MsgIDs in 'Src\MsgIDs.pas',
  Undo in 'Src\Undo.pas',
  Struct in 'Src\Struct.pas',
  NewDisk in 'Src\NewDisk.pas' {NewDiskForm},
  InstFunc in 'Src\InstFunc.pas',
  InstFnc2 in 'Src\InstFnc2.pas',
  Wizard in 'Src\Wizard.pas' {WizardForm},
  ScriptFunc_R in 'Src\ScriptFunc_R.pas',
  ScriptFunc in 'Src\ScriptFunc.pas',
  SetupTypes in 'Src\SetupTypes.pas',
  ScriptRunner in 'Src\ScriptRunner.pas',
  ScriptDlg in 'Src\ScriptDlg.pas',
  ScriptClasses_R in 'Src\ScriptClasses_R.pas',
  SelLangForm in 'Src\SelLangForm.pas' {SelectLanguageForm},
  Extract in 'Src\Extract.pas',
  Int64Em in 'Src\Int64Em.pas',
  SelFolderForm in 'Src\SelFolderForm.pas' {SelectFolderForm},
  Compress in 'Src\Compress.pas',
  CompressZlib in 'Src\CompressZlib.pas',
  bzlib in 'Src\bzlib.pas',
  LZMADecomp in 'Src\LZMADecomp.pas',
  FileClass in 'Src\FileClass.pas',
  MD5 in 'Src\MD5.pas',
  SHA1 in 'Src\SHA1.pas',
  Logging in 'Src\Logging.pas',
  DebugClient in 'Src\DebugClient.pas',
  DebugStruct in 'Src\DebugStruct.pas',
  ArcFour in 'Src\ArcFour.pas',
  Uninstall in 'Src\Uninstall.pas',
  UninstProgressForm in 'Src\UninstProgressForm.pas' {UninstProgressForm},
  UninstSharedFileForm in 'Src\UninstSharedFileForm.pas' {UninstSharedFileForm},
  SimpleExpression in 'Src\SimpleExpression.pas',
  UIStateForm in 'Src\UIStateForm.pas',
  SetupForm in 'Src\SetupForm.pas',
  RegSvr in 'Src\RegSvr.pas',
  BrowseFunc in 'Src\BrowseFunc.pas',
  RedirFunc in 'Src\RedirFunc.pas',
  SecurityFunc in 'Src\SecurityFunc.pas',
  Helper in 'Src\Helper.pas',
  VerInfo in 'Src\VerInfo.pas',
  RegDLL in 'Src\RegDLL.pas',
  ResUpdate in 'Src\ResUpdate.pas',
  SpawnCommon in 'Src\SpawnCommon.pas',
  SpawnServer in 'Src\SpawnServer.pas',
  SpawnClient in 'Src\SpawnClient.pas',
  TaskDialog in 'Src\TaskDialog.pas',
  BidiUtils in '..\Components\BidiUtils.pas',
  PathFunc in '..\Components\PathFunc.pas',
  BidiCtrls in '..\Components\BidiCtrls.pas',
  BitmapImage in '..\Components\BitmapImage.pas',
  FolderTreeView in '..\Components\FolderTreeView.pas',
  NewCheckListBox in '..\Components\NewCheckListBox.pas',
  NewNotebook in '..\Components\NewNotebook.pas',
  NewProgressBar in '..\Components\NewProgressBar.pas',
  NewStaticText in '..\Components\NewStaticText.pas',
  PasswordEdit in '..\Components\PasswordEdit.pas',
  TmSchema in '..\Components\TmSchema.pas',
  RestartManager in '..\Components\RestartManager.pas',
  Resample in '..\Components\Resample.pas',
  ASMInline in '..\Components\ASMInline.pas',
  TaskbarProgressFunc in 'Src\TaskbarProgressFunc.pas',
  DotNet in 'Src\DotNet.pas',
  SetupEnt in 'Src\SetupEnt.pas',
  Msi in 'Src\Msi.pas',
  DotNetVersion in 'Src\DotNetVersion.pas',
  NewUxTheme in '..\Components\NewUxTheme.pas';

{$SETPEOSVERSION 6.1}
{$SETPESUBSYSVERSION 6.1}
{$WEAKLINKRTTI ON}

{$R Res\Setup.icon.res}
{$R Res\Setup.images.res}
{$R Res\Setup.version.res}

procedure ShowExceptionMsg;
var
  S: String;
begin
  if ExceptObject is EAbort then begin
    Log('Got EAbort exception.');
    Exit;
  end;
  S := GetExceptMessage;
  Log('Exception message:');
  LoggedAppMessageBox(PChar(S), Pointer(SetupMessages[msgErrorTitle]),
    MB_OK or MB_ICONSTOP, True, IDOK);
    { ^ use a Pointer cast instead of a PChar cast so that it will use "nil"
      if SetupMessages[msgErrorTitle] is empty due to the messages not being
      loaded yet. MessageBox displays 'Error' as the caption if the lpCaption
      parameter is nil. }
end;

type
  TDummyClass = class
  private
    class function AntiShutdownHook(var Message: TMessage): Boolean;
  end;

class function TDummyClass.AntiShutdownHook(var Message: TMessage): Boolean;
begin
  { This causes Setup/Uninstall/RegSvr to all deny shutdown attempts.
    - If we were to return 1, Windows will send us a WM_ENDSESSION message and
      TApplication.WndProc will call Halt in response. This is no good because
      it would cause an unclean shutdown of Setup, and it would also prevent
      the right exit code from being returned.
      Even if TApplication.WndProc didn't call Halt, it is my understanding
      that Windows could kill us off after sending us the WM_ENDSESSION message
      (see the Remarks section of the WM_ENDSESSION docs).
    - SetupLdr denys shutdown attempts as well, so there is little point in
      Setup trying to handle them. (Depending on the version of Windows, we
      may never even get a WM_QUERYENDSESSION message because of that.)
    Note: TSetupForm also has a WM_QUERYENDSESSION handler of its own to
    prevent CloseQuery from being called. }
  Result := False;
  case Message.Msg of
    WM_QUERYENDSESSION: begin
        { Return zero, except if RestartInitiatedByThisProcess is set
          (which means we called RestartComputer previously) }
        if RestartInitiatedByThisProcess or (IsUninstaller and AllowUninstallerShutdown) then begin
          AcceptedQueryEndSessionInProgress := True;
          Message.Result := 1
        end else
          Message.Result := 0;
        Result := True;
      end;
    WM_ENDSESSION: begin
        { Should only get here if RestartInitiatedByThisProcess is set or an
          Uninstaller shutdown was allowed, or if the user forced a shutdown.
          Skip the default handling which calls Halt. No code of ours depends
          on the Halt call to clean up, and it could theoretically result in
          obscure reentrancy bugs.
          Example: I've found that combo boxes pump incoming sent messages
          when they are destroyed*; if one of those messages were a
          WM_ENDSESSION, the Halt call could cause another destructor to be
          to be entered (DoneApplication frees all still-existing forms)
          before the combo box's destructor has returned.
          * arguably a Windows bug. The internal ComboLBox window is created
          as a child (WS_CHILD) of GetDesktopWindow(); when it is destroyed,
          a WM_PARENTNOTIFY(WM_DESTROY) message is sent to the desktop window.
          Because the desktop window is on a separate thread, pending sent
          messages are dispatched during the SendMessage call. }
        if Bool(Message.wParam) = True then begin
          if not RestartInitiatedByThisProcess and IsUninstaller then
            HandleUninstallerEndSession;
        end else
          AcceptedQueryEndSessionInProgress := False;
        Result := True;
      end;
    WM_STYLECHANGING: begin
        { On Delphi 2009, we must suppress some of the VCL's manipulation of
          the application window styles in order to prevent the taskbar button
          from re-appearing after SetTaskbarButtonVisibility(False) was used
          to hide it.
          - The VCL tries to clear WS_EX_TOOLWINDOW whenever a form handle is
            created (see TCustomForm.CreateParams). Since
            SetTaskbarButtonVisibility uses the WS_EX_TOOLWINDOW style
            internally to hide the taskbar button, we can't allow that.
          - The VCL tries to set WS_EX_APPWINDOW on the application window
            after the main form is created (see ChangeAppWindow in Forms).
            The WS_EX_APPWINDOW style forces the window to show a taskbar
            button, overriding WS_EX_TOOLWINDOW, so don't allow that either.
            (It appears to be redundant anyway.) }
        if Integer(Message.WParam) = GWL_EXSTYLE then begin
          { SetTaskbarButtonVisibility sets TaskbarButtonHidden }
          if TaskbarButtonHidden then
            PStyleStruct(Message.LParam).styleNew :=
              PStyleStruct(Message.LParam).styleNew or WS_EX_TOOLWINDOW;
          PStyleStruct(Message.LParam).styleNew :=
            PStyleStruct(Message.LParam).styleNew and not WS_EX_APPWINDOW;
        end;
      end;
  end;
end;

procedure DisableWindowGhosting;
var
  Proc: procedure; stdcall;
begin
  Proc := GetProcAddress(GetModuleHandle(user32), 'DisableProcessWindowsGhosting');
  if Assigned(Proc) then
    Proc;
end;

procedure SelectMode;
{ Determines whether we should run as Setup, Uninstall, or RegSvr }
var
  ParamName, ParamValue: String;
  Mode: (smSetup, smUninstaller, smRegSvr);
  F: TFile;
  ID: Longint;
  I: Integer;
begin
  { When SignedUninstaller=yes, the EXE header specifies uninstaller mode by
    default. Use Setup mode instead if we're being called from SetupLdr. }
  SplitNewParamStr(1, ParamName, ParamValue);
  if CompareText(ParamName, '/SL5=') = 0 then
    Exit;

  Mode := smSetup;

  for I := 1 to NewParamCount do begin
    if CompareText(NewParamStr(I), '/UNINSTMODE') = 0 then begin
      Mode := smUninstaller;
      Break;
    end;
    if CompareText(NewParamStr(I), '/REGSVRMODE') = 0 then begin
      Mode := smRegSvr;
      Break;
    end;
  end;

  if Mode = smSetup then begin
    { No mode specified on the command line; check the EXE header for one }
    F := TFile.Create(NewParamStr(0), fdOpenExisting, faRead, fsRead);
    try
      F.Seek(SetupExeModeOffset);
      F.ReadBuffer(ID, SizeOf(ID));
    finally
      F.Free;
    end;
    case ID of
      SetupExeModeUninstaller: Mode := smUninstaller;
      SetupExeModeRegSvr: Mode := smRegSvr;
    end;
  end;

  case Mode of
    smUninstaller: begin
        IsUninstaller := True;
        AllowUninstallerShutdown := False;
        RunUninstaller;
        { Shouldn't get here; RunUninstaller should Halt itself }
        Halt(1);
      end;
    smRegSvr: begin
        try
          RunRegSvr;
        except
          ShowExceptionMsg;
        end;
        Halt;
      end;
  end;
end;

begin
  { Delphi 2009 initially sets WS_EX_TOOLWINDOW on the application window.
    That will prevent our ShowWindow(Application.Handle, SW_SHOW) calls from
    actually displaying the taskbar button as intended, so clear it. }
  SetWindowLong(Application.Handle, GWL_EXSTYLE,
    GetWindowLong(Application.Handle, GWL_EXSTYLE) and not WS_EX_TOOLWINDOW);

  try
    SetErrorMode(SEM_FAILCRITICALERRORS);
    DisableWindowGhosting;
    Application.HookMainWindow(TDummyClass.AntiShutdownHook);
    TRichEditViewer.CustomShellExecute := ShellExecuteAsOriginalUser;
    SelectMode; { Only returns if we should run as Setup }
  except
    { Halt on any exception }
    ShowExceptionMsg;
    Halt(ecInitializationError);
  end;

  { Initialize.
    Note: There's no need to localize the following line since it's changed in
    InitializeSetup }
  Application.Title := 'Setup';
  { On Delphi 3+, the application window by default isn't visible until a form
    is shown. Force it visible like Delphi 2. Note that due to the way
    TApplication.UpdateVisible is coded, this should be permanent; if a form
    is shown and hidden, the application window should still be visible. }
  ShowWindow(Application.Handle, SW_SHOW);
  Application.OnException := TMainForm.ShowException;
  try
    Application.Initialize;
    InitializeSetup;
    Application.CreateForm(TMainForm, MainForm);
  MainForm.InitializeWizard;
  except
    { Halt on any exception }
    ShowExceptionMsg;
    try
      DeinitSetup(False);
    except
      { don't propagate any exceptions, so that Halt is always called }
      ShowExceptionMsg;
    end;
    if SetupExitCode <> 0 then
      Halt(SetupExitCode)
    else
      Halt(ecInitializationError);
  end;

  { Run }
  try
    Application.Run;
  except
    { Show any exception and continue }
    ShowExceptionMsg;
  end;

  { Deinitialize (clean up) }
  try
    DeinitSetup(SetupExitCode = 0);
  except
    { Show any exception and continue }
    ShowExceptionMsg;
  end;

  Halt(SetupExitCode);
end.
