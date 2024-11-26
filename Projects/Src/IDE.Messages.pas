unit IDE.Messages;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE Messages

  Some but not all language-specific text used by the Compiler IDE is in here.
}

interface

const
  SNewLine = #13#10;  { line break }
  SNewLine2 = #13#10#13#10;  { double line break }

  { Compiler-specific messages }
  SCompilerCommandLineHelp3 = 'Command line usage:' + SNewLine +
    SNewLine +
    'compil32 /cc <script file>' + SNewLine +
    'compil32 /wizard <wizard name> <script file>' + SNewLine +
    SNewLine +
    'Examples:' + SNewLine +
    'compil32 /cc c:\isetup\sample32\sample1.iss' + SNewLine +
    'compil32 /cc "C:\Inno Setup\Sample32\My script.iss"' + SNewLine +
    'compil32 /wizard "My Script Wizard" c:\temp.iss';

  { Compiler form labels }
  SCompilerFormCaption = 'Inno Setup Compiler';
  SCompilerScriptFileLabel = 'Script &File:';
  SCompilerStatusLabel = 'Status &Messages:';
  SCompilerScriptBrowseButton = '&Browse...';
  SCompilerStartButton = '&Start';
  SCompilerExitButton = 'E&xit';
  SCompilerOpenFilter = 'Inno Setup Script files (*.iss)|*.iss|All files|*.*';
  SCompilerExampleScripts = 'Example scripts...';
  SCompilerMoreFiles = 'More files...';

  { Compiler Script Wizard }
  SWizardDefaultName = 'Inno Setup Script Wizard';
  SWizardWelcome = 'Welcome';
  SWizardAppInfo = 'Application Information';
  SWizardAppInfo2 = 'Please specify some basic information about your application.';
  SWizardAppDir = 'Application Folder';
  SWizardAppDir2 = 'Please specify folder information about your application.';
  SWizardAppFiles = 'Application Files';
  SWizardAppFiles2 = 'Please specify the files that are part of your application.';
  SWizardAppFiles3 = 'Please specify the source folder.';
  SWizardAppFilesSubDirsMessage = 'Should files in subfolders of "%s" also be included?';
  SWizardAppExeFilter = 'Application files (*.exe)|*.exe|All files|*.*';
  SWizardAppExeDefaultExt = 'exe';
  SWizardAppAssoc = 'Application File Association';
  SWizardAppAssoc2 = 'Please specify which file association should be created for your application.';
  SWizardAppIcons = 'Application Shortcuts';
  SWizardAppIcons2 = 'Please specify which shortcuts should be created for your application.';
  SWizardAppDocs = 'Application Documentation';
  SWizardAppDocs2 = 'Please specify which documentation files should be shown by Setup during installation.';
  SWizardAppDocsFilter = 'Documentation files (*.rtf,*.txt)|*.rtf;*.txt|All files|*.*';
  SWizardAppDocsDefaultExt = 'rtf';
  SWizardAppRegFilter = 'Registry files (*.reg)|*.reg|All files|*.*';
  SWizardAppRegDefaultExt = 'reg';
  SWizardPrivilegesRequired = 'Setup Install Mode';
  SWizardPrivilegesRequired2 = 'Please specify in which install mode Setup should run.';
  SWizardAppRegistry = 'Application Registry Keys And Values';
  SWizardAppRegistry2 = 'Please specify the registry keys and values that are part of your application.';
  SWizardLanguages = 'Setup Languages';
  SWizardLanguages2 = 'Please specify which Setup languages should be included.';
  SWizardCompiler = 'Compiler Settings';
  SWizardCompiler2 = 'Please specify some basic compiler settings.';
  SWizardCompilerSetupIconFileFilter = 'Icon files (*.ico)|*.ico|All files|*.*';
  SWizardCompilerSetupIconFileDefaultExt = 'ico';
  SWizardCompilerOutputDir = 'Please specify the folder.';
  SWizardISPP = 'Inno Setup Preprocessor';
  SWizardISPP2 = 'Please specify whether Inno Setup Preprocessor should be used.';
  SWizardISPPLabel = 'The [name] can use #define compiler directives to simplify your script. Although this is not necessary, it will make it easier to manually change the script later.' + SNewLine2 + 'Do you want the [name] to use #define compiler directives?';
  SWizardISPPCheck = '&Yes, use #define compiler directives';
  SWizardFinished = 'Finished';

  SWizardNextButton = '&Next';
  SWizardFinishButton = '&Finish';
  SWizardCancelMessage = 'The [name] is not complete. If you quit now, the new script file will not be generated.'#13#13'Exit the [name]?';

  SWizardAllFilesFilter = 'All files|*.*';

  SWizardAppNameError = 'Please specify the application name.';
  SWizardAppVersionError = 'Please specify the application version.';
  SWizardAppRootDirError = 'Please specify the application destination base folder.';
  SWizardAppDirNameError = 'Please specify the application folder name.';
  SWizardAppExeError = 'Please specify the application main executable file.';
  SWizardAppGroupNameError = 'Please specify the application Start Menu group name.';
  SWizardFileDestRootDirError = 'Please specify the destination base folder.';
  SWizardFileAppDestRootDirError = 'Please specify a destination base folder other than the application folder';
  SWizardLanguagesSelError = 'Please select at least one language.';

  SWizardScriptHeader = '; Script generated by the [name].' + SNewLine  + '; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!';

  { Status messages }
  SCompilerStatusStarting = '*** Starting compile.  [%s]';
  SCompilerStatusFinished = '*** Finished.  [%s, %s elapsed]';
  SCompilerStatusErrorAborted = '*** Compile aborted.';
  SCompilerStatusReset = '*** Log size limit reached, list reset.';

  SCompilerNeedCompiledExe = 'Cannot run Setup at this time. Please compile Setup successfully to completion first, with output enabled';
  SCompilerNeedUninstExe = 'Cannot run Uninstall at this time. Please run Setup successfully to completion first';
  SCompilerExecuteSetupError2 = 'Error executing "%s":' + SNewLine2 + '%d: %s';  

  { Line parsing }
  SCompilerIllegalNullChar = 'Illegal null character on line %d';

implementation

end.
