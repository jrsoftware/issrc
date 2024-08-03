program Compil32;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler
}

uses
  Shared.SafeDLLPath in 'Src\Shared.SafeDLLPath.pas',
  Windows,
  SysUtils,
  Forms,
  PathFunc in '..\Components\PathFunc.pas',
  Compil32.CompileForm in 'Src\Compil32\Compil32.CompileForm.pas' {CompileForm},
  Shared.CmnFunc in 'Src\Shared.CmnFunc.pas',
  Shared.CmnFunc2 in 'Src\Shared.CmnFunc2.pas',
  Compil32.HelpFunc in 'Src\Compil32\Compil32.HelpFunc.pas',
  Compil32.Messages in 'Src\Compil32\Compil32.Messages.pas',
  Shared.CompInt in 'Src\Shared.CompInt.pas',
  Compil32.OptionsForm in 'Src\Compil32\Compil32.OptionsForm.pas' {OptionsForm},
  Compil32.StartupForm in 'Src\Compil32\Compil32.StartupForm.pas' {StartupForm},
  Compil32.Wizard.WizardForm in 'Src\Compil32\Compil32.Wizard.WizardForm.pas' {WizardForm},
  Compil32.Wizard.WizardFileForm in 'Src\Compil32\Compil32.Wizard.WizardFileForm.pas' {WizardFileForm},
  Compil32.FileAssocFunc in 'Src\Compil32\Compil32.FileAssocFunc.pas',
  TmSchema in '..\Components\TmSchema.pas',
  NewUxTheme in '..\Components\NewUxTheme.pas',
  Shared.DebugStruct in 'Src\Shared.DebugStruct.pas',
  Shared.BrowseFunc in 'Src\Shared.BrowseFunc.pas',
  Compil32.SignToolsForm in 'Src\Compil32\Compil32.SignToolsForm.pas' {SignToolsForm},
  Compil32.InputQueryComboForm in 'Src\Compil32\Compil32.InputQueryComboForm.pas',
  ScintInt in '..\Components\ScintInt.pas',
  ScintEdit in '..\Components\ScintEdit.pas',
  Compil32.ScintStylerInnoSetup in 'Src\Compil32\Compil32.ScintStylerInnoSetup.pas',
  ModernColors in '..\Components\ModernColors.pas',
  Compil32.MsgBoxDesignerForm in 'Src\Compil32\Compil32.MsgBoxDesignerForm.pas' {MsgBoxDesignerForm},
  Compil32.CompScintEdit in 'Src\Compil32\Compil32.CompScintEdit.pas',
  Compil32.FilesDesignerForm in 'Src\Compil32\Compil32.FilesDesignerForm.pas' {FilesDesignerForm},
  Compil32.Wizard.WizardFormFilesHelper in 'Src\Compil32\Compil32.Wizard.WizardFormFilesHelper.pas',
  NewTabSet in '..\Components\NewTabSet.pas',
  NewStaticText in '..\Components\NewStaticText.pas',
  BidiUtils in '..\Components\BidiUtils.pas',
  DropListBox in '..\Components\DropListBox.pas',
  NewCheckListBox in '..\Components\NewCheckListBox.pas',
  NewNotebook in '..\Components\NewNotebook.pas',
  Shared.TaskbarProgressFunc in 'Src\Shared.TaskbarProgressFunc.pas',
  Compil32.HtmlHelpFunc in 'Src\Compil32\Compil32.HtmlHelpFunc.pas',
  Shared.UIStateForm in 'Src\Shared.UIStateForm.pas',
  Shared.LangOptionsSectionDirectives in 'Src\Shared.LangOptionsSectionDirectives.pas',
  Shared.MsgIDs in 'Src\Shared.MsgIDs.pas',
  Shared.SetupSectionDirectives in 'Src\Shared.SetupSectionDirectives.pas',
  Shared.CompTypes in 'Src\Shared.CompTypes.pas',
  Shared.FileClass in 'Src\Shared.FileClass.pas',
  Shared.Int64Em in 'Src\Shared.Int64Em.pas',
  Shared.Compress in 'Src\Shared.Compress.pas',
  Shared.TaskDialog in 'Src\Shared.TaskDialog.pas',
  Compil32.RegistryDesignerForm in 'Src\Compil32\Compil32.RegistryDesignerForm.pas' {RegistryDesignerForm},
  Compil32.Wizard.WizardFormRegistryHelper in 'Src\Compil32\Compil32.Wizard.WizardFormRegistryHelper.pas',
  MD5 in '..\Components\MD5.pas',
  IsscintInt in '..\Components\IsscintInt.pas',
  Shared.ScriptFunc in 'Src\Shared.ScriptFunc.pas',
  Shared.SetupTypes in 'Src\Shared.SetupTypes.pas',
  Shared.Struct in 'Src\Shared.Struct.pas',
  SHA1 in '..\Components\SHA1.pas',
  Shared.DotNetVersion in 'Src\Shared.DotNetVersion.pas',
  isxclasses_wordlists_generated in '..\ISHelp\isxclasses_wordlists_generated.pas';

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
        UnregisterISSFileAssociation;
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
  InitialCurDir := GetCurrentDir;
  if not SetCurrentDir(PathExtractDir(NewParamStr(0))) then
    SetCurrentDir(GetSystemDir);

  SetAppUserModelID;
  CreateMutexes;
  Application.Initialize;
  CheckParams;
  RegisterApplicationRestart;

  { The 'with' is so that the Delphi IDE doesn't mess with these }
  with Application do begin
    if CommandLineWizard then
      Title := CommandLineWizardName
    else
      Title := SCompilerFormCaption;
  end;

  Application.CreateForm(TCompileForm, CompileForm);
  Application.Run;
end.
