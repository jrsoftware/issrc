program Compil32;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler
}

uses
  SafeDLLPath in '..\Components\SafeDLLPath.pas',
  Windows,
  SysUtils,
  Forms,
  PathFunc in '..\Components\PathFunc.pas',
  IDE.MainForm in 'Src\IDE.MainForm.pas' {MainForm},
  Shared.CommonFunc.Vcl in 'Src\Shared.CommonFunc.Vcl.pas',
  Shared.CommonFunc in 'Src\Shared.CommonFunc.pas',
  IDE.HelperFunc in 'Src\IDE.HelperFunc.pas',
  IDE.Messages in 'Src\IDE.Messages.pas',
  Shared.CompilerInt in 'Src\Shared.CompilerInt.pas',
  IDE.OptionsForm in 'Src\IDE.OptionsForm.pas' {OptionsForm},
  IDE.StartupForm in 'Src\IDE.StartupForm.pas' {StartupForm},
  IDE.Wizard.WizardForm in 'Src\IDE.Wizard.WizardForm.pas' {WizardForm},
  IDE.Wizard.WizardFileForm in 'Src\IDE.Wizard.WizardFileForm.pas' {WizardFileForm},
  IDE.FileAssocFunc in 'Src\IDE.FileAssocFunc.pas',
  NewUxTheme.TmSchema in '..\Components\NewUxTheme.TmSchema.pas',
  NewUxTheme in '..\Components\NewUxTheme.pas',
  Shared.DebugStruct in 'Src\Shared.DebugStruct.pas',
  BrowseFunc in '..\Components\BrowseFunc.pas',
  IDE.SignToolsForm in 'Src\IDE.SignToolsForm.pas' {SignToolsForm},
  IDE.InputQueryComboForm in 'Src\IDE.InputQueryComboForm.pas',
  IDE.InputQueryMemoForm in 'Src\IDE.InputQueryMemoForm.pas',
  ScintInt in '..\Components\ScintInt.pas',
  ScintEdit in '..\Components\ScintEdit.pas',
  IDE.ScintStylerInnoSetup in 'Src\IDE.ScintStylerInnoSetup.pas',
  ModernColors in '..\Components\ModernColors.pas',
  IDE.MsgBoxDesignerForm in 'Src\IDE.MsgBoxDesignerForm.pas' {MsgBoxDesignerForm},
  IDE.IDEScintEdit in 'Src\IDE.IDEScintEdit.pas',
  IDE.FilesDesignerForm in 'Src\IDE.FilesDesignerForm.pas' {FilesDesignerForm},
  IDE.Wizard.WizardFormFilesHelper in 'Src\IDE.Wizard.WizardFormFilesHelper.pas',
  NewTabSet in '..\Components\NewTabSet.pas',
  NewStaticText in '..\Components\NewStaticText.pas',
  BidiUtils in '..\Components\BidiUtils.pas',
  DropListBox in '..\Components\DropListBox.pas',
  NewCheckListBox in '..\Components\NewCheckListBox.pas',
  NewNotebook in '..\Components\NewNotebook.pas',
  TaskbarProgressFunc in '..\Components\TaskbarProgressFunc.pas',
  IDE.HtmlHelpFunc in 'Src\IDE.HtmlHelpFunc.pas',
  UIStateForm in '..\Components\UIStateForm.pas',
  Shared.LangOptionsSectionDirectives in 'Src\Shared.LangOptionsSectionDirectives.pas',
  Shared.SetupMessageIDs in 'Src\Shared.SetupMessageIDs.pas',
  Shared.SetupSectionDirectives in 'Src\Shared.SetupSectionDirectives.pas',
  Shared.ConfigIniFile in 'Src\Shared.ConfigIniFile.pas',
  Shared.SignToolsFunc in 'Src\Shared.SignToolsFunc.pas',
  Shared.FileClass in 'Src\Shared.FileClass.pas',
  Shared.Int64Em in 'Src\Shared.Int64Em.pas',
  Shared.TaskDialogFunc in 'Src\Shared.TaskDialogFunc.pas',
  IDE.RegistryDesignerForm in 'Src\IDE.RegistryDesignerForm.pas' {RegistryDesignerForm},
  IDE.Wizard.WizardFormRegistryHelper in 'Src\IDE.Wizard.WizardFormRegistryHelper.pas',
  ScintInt.InnoSetup in '..\Components\ScintInt.InnoSetup.pas',
  Shared.ScriptFunc in 'Src\Shared.ScriptFunc.pas',
  Shared.SetupSteps in 'Src\Shared.SetupSteps.pas',
  Shared.Struct in 'Src\Shared.Struct.pas',
  SHA256 in '..\Components\SHA256.pas',
  Shared.DotNetVersion in 'Src\Shared.DotNetVersion.pas',
  isxclasses_wordlists_generated in '..\ISHelp\isxclasses_wordlists_generated.pas',
  IDE.ImagesModule in 'Src\IDE.ImagesModule.pas' {ImagesModule: TDataModule};

{$SETPEOSVERSION 6.1}
{$SETPESUBSYSVERSION 6.1}
{$WEAKLINKRTTI ON}

{$R Res\Compil32.docicon.res}
{$R Res\Compil32.manifest.res}
{$R Res\Compil32.versionandicon.res}

procedure SetAppUserModelID;
var
  Func: function(AppID: PWideChar): HRESULT; stdcall;
begin
  { For the IDE to be pinnable and show a Jump List, it is necessary to
    explicitly assign an AppUserModelID because by default the taskbar excludes
    applications that have "Setup" in their name. }
  Func := GetProcAddress(GetModuleHandle('shell32.dll'),
    'SetCurrentProcessExplicitAppUserModelID');
  if Assigned(Func) then
    Func('JR.InnoSetup.IDE.6');
end;

procedure RegisterApplicationRestart;
const
  RESTART_MAX_CMD_LINE = 1024;
  RESTART_NO_CRASH = $1;
  RESTART_NO_HANG = $2;
  RESTART_NO_PATCH = $4;
  RESTART_NO_REBOOT = $8;
var
  Func: function(pwzCommandLine: PWideChar; dwFlags: DWORD): HRESULT; stdcall;
  CommandLine: WideString;
begin
  { Allow Restart Manager to restart us after updates. }

  Func := GetProcAddress(GetModuleHandle('kernel32.dll'),
    'RegisterApplicationRestart');
  if Assigned(Func) then begin
    { Rebuild the command line, can't just use an exact copy since it might contain
      relative path names but Restart Manager doesn't restore the working
      directory. }
    if CommandLineWizard then
      CommandLine := '/WIZARD'
    else begin
      CommandLine := CommandLineFilename;
      if CommandLine <> '' then
        CommandLine := '"' + CommandLine + '"';
      if CommandLineCompile then
        CommandLine := '/CC ' + CommandLine;
    end;

    if Length(CommandLine) > RESTART_MAX_CMD_LINE then
      CommandLine := '';

    Func(PWideChar(CommandLine), RESTART_NO_CRASH or RESTART_NO_HANG or RESTART_NO_REBOOT);
  end;
end;

procedure CreateMutexes;
{ Creates the two mutexes used by Inno Setup's own installer/uninstaller to
  see if the compiler is still running.
  One of the mutexes is created in the global name space (which makes it
  possible to access the mutex across user sessions in Windows XP); the other
  is created in the session name space (because versions of Windows NT prior
  to 4.0 TSE don't have a global name space and don't support the 'Global\'
  prefix). }
const
  MutexName = 'InnoSetupCompilerAppMutex';
begin
  CreateMutex(MutexName);
  CreateMutex('Global\' + MutexName); { don't localize }
end;

var
  InitialCurDir: String;

procedure CheckParams;

  procedure Error;
  begin
    MessageBox(0, SCompilerCommandLineHelp3, SCompilerFormCaption,
      MB_OK or MB_ICONEXCLAMATION);
    Halt(1);
  end;

var
  P, I: Integer;
  S: String;
  Dummy: Boolean;
begin
  P := NewParamCount;
  I := 1;
  while I <= P do begin
    S := NewParamStr(I);
    if CompareText(S, '/CC') = 0 then
      CommandLineCompile := True
    else if CompareText(S, '/WIZARD') = 0 then begin
      if I = P then
        Error;
      CommandLineWizard := True;
      CommandLineWizardName := NewParamStr(I+1);
      Inc(I);
    end
    else if CompareText(S, '/ASSOC') = 0 then begin
      try
        RegisterISSFileAssociation(False, Dummy);
      except
        MessageBox(0, PChar(GetExceptMessage), nil, MB_OK or MB_ICONSTOP);
        Halt(2);
      end;
      Halt;
    end
    else if CompareText(S, '/UNASSOC') = 0 then begin
      try
        UnregisterISSFileAssociation(True);
      except
        MessageBox(0, PChar(GetExceptMessage), nil, MB_OK or MB_ICONSTOP);
        Halt(2);
      end;
      Halt;
    end
    else if (S = '') or (S[1] = '/') or (CommandLineFilename <> '') then
      Error
    else
      CommandLineFilename := PathExpand(PathCombine(InitialCurDir, S));
    Inc(I);
  end;
  if (CommandLineCompile or CommandLineWizard) and (CommandLineFilename = '') then
    Error;
end;

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  InitialCurDir := GetCurrentDir;
  if not SetCurrentDir(PathExtractDir(NewParamStr(0))) then
    SetCurrentDir(GetSystemDir);

  SetAppUserModelID;
  CreateMutexes;
  Application.Initialize;
  CheckParams;
  RegisterApplicationRestart;

  if not CommandLineWizard then
    Application.MainFormOnTaskBar := True;

  { The 'with' is so that the Delphi IDE doesn't mess with these }
  with Application do begin
    if CommandLineWizard then
      Title := CommandLineWizardName
    else
      Title := SCompilerFormCaption;
  end;

  Application.CreateForm(TImagesModule, ImagesModule);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
