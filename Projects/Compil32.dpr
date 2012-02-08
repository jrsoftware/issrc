program Compil32;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler
}

uses
  SafeDLLPath in 'SafeDLLPath.pas',
  Windows,
  SysUtils,
  Forms,
  PathFunc,
  CompForm in 'CompForm.pas' {CompileForm},
  CmnFunc in 'CmnFunc.pas',
  CmnFunc2 in 'CmnFunc2.pas',
  CompMsgs in 'CompMsgs.pas',
  CompInt in 'CompInt.pas',
  CompOptions in 'CompOptions.pas' {OptionsForm},
  CompStartup in 'CompStartup.pas' {StartupForm},
  CompWizard in 'CompWizard.pas' {WizardForm},
  CompWizardFile in 'CompWizardFile.pas' {WizardFileForm},
  CompFileAssoc in 'CompFileAssoc.pas',
  TmSchemaISX in '..\Components\TmSchemaISX.pas',
  UxThemeISX in '..\Components\UxThemeISX.pas',
  DebugStruct in 'DebugStruct.pas',
  BrowseFunc in 'BrowseFunc.pas',
  CompSignTools in 'CompSignTools.pas' {SignToolsForm},
  ScintInt in '..\Components\ScintInt.pas',
  ScintEdit in '..\Components\ScintEdit.pas',
  ScintStylerInnoSetup in '..\Components\ScintStylerInnoSetup.pas';

{$R *.res}
{$R Compil32.manifest.res}
{$R CompDocIcon.res}

procedure SetAppUserModelID;
var
  Func: function(AppID: PWideChar): HRESULT; stdcall;
begin
  { On Windows 7, for the IDE to be pinnable and show a Jump List, it is
    necessary to explicitly assign an AppUserModelID because by default the
    taskbar excludes applications that have "Setup" in their name. }
  Func := GetProcAddress(GetModuleHandle('shell32.dll'),
    'SetCurrentProcessExplicitAppUserModelID');
  if Assigned(Func) then
    Func('JR.InnoSetup.IDE.5');
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
{ Creates the two mutexes used by the Inno Setup's own installer/uninstaller to
  see if the compiler is still running.
  One of the mutexes is created in the global name space (which makes it
  possible to access the mutex across user sessions in Windows XP); the other
  is created in the session name space (because versions of Windows NT prior
  to 4.0 TSE don't have a global name space and don't support the 'Global\'
  prefix). }
const
  MutexName = 'InnoSetupCompilerAppMutex';
  SECURITY_DESCRIPTOR_REVISION = 1;  { Win32 constant not defined in Delphi 3 }
var
  SecurityDesc: TSecurityDescriptor;
  SecurityAttr: TSecurityAttributes;
begin
  { By default on Windows NT, created mutexes are accessible only by the user
    running the process. We need our mutexes to be accessible to all users, so
    that the mutex detection can work across user sessions in Windows XP. To
    do this we use a security descriptor with a null DACL. }
  InitializeSecurityDescriptor(@SecurityDesc, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@SecurityDesc, True, nil, False);
  SecurityAttr.nLength := SizeOf(SecurityAttr);
  SecurityAttr.lpSecurityDescriptor := @SecurityDesc;
  SecurityAttr.bInheritHandle := False;
  CreateMutex(@SecurityAttr, False, MutexName);
  CreateMutex(@SecurityAttr, False, 'Global\' + MutexName);  { don't localize }
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
        RegisterISSFileAssociation;
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
