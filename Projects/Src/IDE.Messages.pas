unit IDE.Messages;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
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
    'iside /cc <script file>' + SNewLine +
    'iside /wizard <wizard name> <script file>' + SNewLine +
    SNewLine +
    'Examples:' + SNewLine +
    'iside /cc c:\isetup\sample32\sample1.iss' + SNewLine +
    'iside /cc "C:\Inno Setup\Sample32\My script.iss"' + SNewLine +
    'iside /wizard "My Script Wizard" c:\temp.iss';

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
  SWizardAppFilesDownloadSourcePrompt = 'URL:';
  SWizardAppFilesDownloadExtractArchiveMessage = 'Is the file to download an archive which should be extracted?';
  SWizardAppFilesDownloadDestNamePrompt = 'Name of the file:';
  SWizardAppFilesDownloadArchiveDestNamePrompt = 'Name of the file (extension should match archive format):';
  SWizardAppFilesDownloadExternalSizePrompt = 'Approximate size of the file in megabytes:';
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
  SWizardWizardStyle = 'Wizard Style';
  SWizardWizardStyle2 = 'Please specify which wizard style should be used.';
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

  SWizardSourceURLLabel = '&Source URL:';
  
  SWizardCompileNewScriptPrompt = 'Would you like to compile the new script now?';

  { Status messages }
  SCompilerStatusStarting = '*** Starting compile.  [%s]';
  SCompilerStatusFinished = '*** Finished.  [%s, %s elapsed]';
  SCompilerStatusErrorAborted = '*** Compile aborted.';
  SCompilerStatusReset = '*** Log size limit reached, list reset.';

  SCompilerNeedCompiledExe = 'Cannot run Setup at this time. Please compile Setup successfully to completion first, with output enabled';
  SCompilerNeedUninstExe = 'Cannot run Uninstall at this time. Please run Setup successfully to completion first';
  SCompilerExecuteSetupError2 = 'Error executing "%s":' + SNewLine2 + '%d: %s';  
  SCompilerAbortCompileConfirm = 'Are you sure you want to abort the compile?';

  SCompilerErrorFilePrefix = 'File: %s';
  SCompilerErrorLinePrefix = 'Line %d:';
  SCompilerErrorTitle = 'Compiler Error';

  { Line parsing }
  SCompilerIllegalNullChar = 'Illegal null character on line %d';

  { Find & Replace }
  SFindNotFound = 'Cannot find "%s"';
  SFindInvalidRegEx = 'Invalid regular expression "%s"';
  SFindResultLinePrefix = '  Line %d: ';
  SFindResultFileHeader = '%s (%d hits):';
  SFindResultSummary = 'Find "%s" (%d hits in %d files)';
  SReplaceCount = '%d occurrence(s) replaced.';

  { File association }
  SAssocSuccess = 'The .iss extension was successfully associated for %s with:' + SNewLine  + '%s';
  SAssocTitle = 'Associate';

  { Registry Designer }
  SRegistryDesignerScriptHas = 'Script has %s';
  SRegistryDesignerScriptHasSet = 'Script has %s set';

  { MsgBox Designer }
  SMsgBoxDesignerPreviewCaption = 'Setup';
  SMsgBoxDesignerDefaultText = 'Your message text.';
  SMsgBoxDesignerDefaultInputText = '<Enter your text here...>';
  SMsgBoxDesignerReturnValues = ' Return values ';
  SMsgBoxDesignerReturnValuesDefault = ' Return values /  -------  / Default ';
  SMsgBoxDesignerReturnValuesShield = ' Return values /  Shield ';
  SMsgBoxDesignerReturnValuesShieldDefault = ' Return values /  Shield  / Default ';

  { Sign Tools }
  SSignToolNamePrompt = 'Name of the Sign Tool:';
  SSignToolInvalidName = 'Invalid name.';
  SSignToolDuplicateName = 'Duplicate name.';
  SSignToolCommandPrompt = 'Command of the Sign Tool:';
  SSignToolInvalidCommand = 'Invalid command.';

  { Tools }
  SToolsInsertGuidConfirm = 'The generated GUID will be inserted into the editor at the cursor position. Continue?';
  SToolsNotInCodeSectionConfirm = 'The generated Pascal script will be inserted into the editor at the cursor position, but the cursor is not in the [Code] section. Continue anyway?';

  { Menu captions }
  SMenuNewWithWizard = '&New...';
  SMenuNew = '&New';

  { Navigation }
  SNavLineNumber = 'Line %d';
  SNavItemCaption = '%s: %s';

  { Caption status indicators }
  SCompilerCaption32Bit = '(32-bit)';
  SCompilerCaptionCompiling = '[Compiling]';
  SCompilerCaptionRunning = '[Running]';
  SCompilerCaptionPaused = '[Paused]';

  { File operations }
  SCompilerOpenFileErrorRemoveFromMRU = 'There was an error opening the file. Remove it from the list?';
  SCompilerFileChangedSavePrompt = 'The text in the %s file has changed.' + SNewLine2 + 'Do you want to save the changes?';
  SCompilerStopCompileBeforeCommand = 'Please stop the compile process before performing this command.';
  SCompilerCompileAlreadyInProgress = 'A compile is already in progress.';
  SCompilerIncludedFileChangedSavePrompt = 'The text in the %s file has changed and must be saved before compiling.' + SNewLine2 + 'Save the changes and continue?';
  SCompilerSaveScriptBeforeCompile = 'Would you like to save the script before compiling?' + SNewLine2 + 'If you answer No, the compiled installation will be placed under your My Documents folder by default.';
  SCompilerIncludedFileOpenAsTab = 'The selected file is an #include file. Go to its tab instead of opening it as the new main file?';
  SCompilerIncludedFileNotAvailableAsTab = 'The selected file is not available as a tab. Opening as the new main file instead.';
  SCompilerSaveErrorCreateFile = 'Error creating file (code %d). Could not save file';
  SCompilerSaveErrorCreateBackup = 'Error creating backup file. Could not save file';
  SCompilerSaveErrorRemoveExisting = 'Error removing existing file (code %d). Could not save file';
  SCompilerSaveErrorRenameTemp = 'Error renaming temporary file (code %d). Could not save file';
  SCompilerStatusFailedToOpenIncludedFile = 'Failed to open included file: %s';
  SCompilerPrinterDocumentStartError = 'Can not start printer document.';
  SCompilerClearRecentFilesConfirm = 'Are you sure you want to clear the list of recently opened files?';
  SCompilerFileModifiedOutside = 'The %s file has been modified outside of the source editor. You might want to reload it.';
  SCompilerFileModifiedReload = 'The %s file has been modified outside of the source editor.' + SNewLine2 + 'Do you want to reload the file?';
  SCompilerFileModifiedReloadChanged = 'The %s file has been modified outside of the source editor. Changes have also been made in the source editor.' + SNewLine2 + 'Do you want to reload the file and lose the changes made in the source editor?';
  SCompilerFileNotOpened = 'File not opened.';
  SGotoLineTitle = 'Go to Line';
  SGotoLinePrompt = 'Line number:';

  { License }
  SCompilerCopyLicenseKeyBeforePurchase = 'Do you want to copy your current license key to the clipboard before opening our order page? You will need it to be able to renew it.';
  SCompilerLicenseRegisterSuccess = 'New commercial license key has been registered:' + SNewLine2 + '%s' + SNewLine2 + 'Thanks for your support!';
  SCompilerRemoveLicenseConfirm = 'Are you sure you want to remove your commercial license key and revert to non-commercial use only?';
  SCompilerLicenseKeyRemoved = 'Commercial license key has been removed.';

  { Debugger }
  SDebugExitCodeHex = '%s exit code: 0x%.8x';
  SDebugExitCodeDecimal = '%s exit code: %u';
  SDebugExitCodeGetFailed = 'Unable to get %s exit code (%s failed)';
  SDebugExitCodeStillRunning = '%s is still running; can''t get exit code';
  SDebugExitCodeWaitFailed = 'Unable to get %s exit code (%s failed)';
  SCompilerStopDebugTargetBeforeCommand = 'Please stop the running %s process before performing this command.';
  SCompilerDetachDebuggerConfirm = 'This command will detach the debugger from the running %s process. Continue?';
  SCompilerModifiedWhileRunningWarning = 'The changes you made will not take effect until you re-compile.' + SNewLine2 + 'Continue running anyway?';
  SCompilerPauseAlreadyPending = 'A pause is already pending.';
  SCompilerNoCodeGeneratedForLine = 'No code was generated for the current line.';
  SCompilerTerminateTitle = 'Terminate';
  SCompilerTerminateProcessConfirm = 'This will unconditionally terminate the running %s process. Continue?';
  SCompilerTerminateProcessSetupNote = 'Note that if %s is currently in the installation phase, any changes made to the system thus far will not be undone, nor will uninstall data be written.';
  SEvaluateTitle = 'Evaluate';
  SEvaluatePrompt = 'Constant to evaluate (e.g., "%s"):';
  SEvaluateResultTitle = 'Evaluate Result';
  SEvaluateErrorTitle = 'Evaluate Error';
  SEvaluateUnknownError = 'An unknown error occurred.';
  SRuntimeErrorLine = 'Line %d:' + SNewLine + '%s';
  SRuntimeErrorTitle = 'Runtime Error';
  SDebugTargetStarted = '*** %s started';
  SDebugTerminatingProcess = '*** Terminating process';
  SDebugRemovingTempDir = '*** Removing left-over temporary directory: %s';
  SDebugFailedToRemoveTempDir = '*** Failed to remove temporary directory';
  SDebugExitCodeText = '*** %s';
  SDebugCodeCallStack = '*** [%s] Call Stack';
 
implementation

end.
