unit IDE.Messages;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE Messages

  Language-specific text used by the Compiler IDE is in here, except the About
  box, internal errors, DEBUG messages, and text in .dfm files.
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
  SCompilerLibraryLoadError = 'Could not load %0:s: %1:s';

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
  SCompilerUntitledFile = 'Untitled';
  SCompilerPreprocessorOutput = 'Preprocessor Output';
  SCompilerRunParametersTitle = 'Run Parameters';
  SCompilerRunParametersPrompt = 'Command line parameters for %0:s and %1:s:';

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
  SWizardAppAssocDefaultName = '%s File';
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
  SWizardISPPLabel = 'The %0:s can use %1:s compiler directives to simplify your script. Although this is not necessary, it will make it easier to manually change the script later.' + SNewLine2 + 'Do you want the %0:s to use %1:s compiler directives?';
  SWizardISPPCheck = '&Yes, use %s compiler directives';
  SWizardFinished = 'Finished';

  SWizardNextButton = '&Next';
  SWizardFinishButton = '&Finish';
  SWizardCancelMessage = 'The %0:s is not complete. If you quit now, the new script file will not be generated.'#13#13'Exit the %0:s?';

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

  SWizardSourceURLLabel = '&Source URL:';
  
  SWizardScriptHeader = '; Script generated by the %s.' + SNewLine  + '; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!';
  SWizardScriptCommentUniqueAppId = 'NOTE: The value of %0:s uniquely identifies this application. Do not use the same %0:s value in installers for other applications.';
  SWizardScriptCommentGenerateGuid = '(To generate a new GUID, click Tools | Generate GUID inside the IDE.)';
  SWizardScriptCommentArchitecturesAllowed = '"%s" specifies that Setup cannot run on anything but x64 and Windows 11 on Arm.';
  SWizardScriptCommentArchitecturesInstallIn64BitMode1 = '"%s" requests that the install be done in "64-bit mode" on x64 or Windows 11 on Arm.';
  SWizardScriptCommentArchitecturesInstallIn64BitMode2 = 'This means it should use the native 64-bit Program Files directory and the 64-bit view of the registry.';
  SWizardScriptCommentChangeTo64BitInstaller = 'Uncomment the following line to use a 64-bit installer.';
  SWizardScriptCommentArchiveExtractionEnhanced = 'Use "%s" if all your archives are .7z files.';
  SWizardScriptCommentArchiveExtractionEnhancedNoPassword = 'Use "%s" if all your archives are not password-protected.';
  SWizardScriptCommentChangeToLowest = 'Uncomment the following line to run in non administrative install mode (install for current user only).';
  SWizardScriptCommentChangeToAdmin = 'Remove the following line to run in administrative install mode (install for all users).';
  SWizardScriptCommentSharedSystemFiles = 'NOTE: Don''t use "%s" on any shared system files.';
  SWizardScriptCommentVerifyDownloads = 'NOTE: Use the "%0:s" flag or the "%1:s" parameter to verify downloads.';
  SWizardScriptCommentRegistryDataFromFile = 'Registry data from file %s';
  SWizardScriptCommentCouldNotImport = 'COULD NOT IMPORT %s';
  SWizardScriptCommentEndOfRegistryDataFromFile = 'End of registry data from file %s';
  SWizardScriptCommentKeysFilteredDuePrivilegesRequired = 'SOME KEYS FILTERED DUE TO %s SETTINGS!';
  SWizardScriptCommentValuesWithUnsupportedTypesSkipped = 'SOME VALUES WITH UNSUPPORTED TYPES SKIPPED!';

  SWizardCompileNewScriptPrompt = 'Would you like to compile the new script now?';

  SWizardDefaultAppName = 'My Program';
  SWizardDefaultAppPublisher = 'My Company, Inc.';

  SWizardDirApplication = 'Application directory';
  SWizardDirProgramFiles = 'Program Files directory';
  SWizardDirCommonFiles = 'Common Files directory';
  SWizardDirWindows = 'Windows directory';
  SWizardDirWindowsSystem = 'Windows system directory';
  SWizardDirSetupSource = 'Setup source directory';
  SWizardDirSystemDriveRoot = 'System drive root directory';
  SWizardDirCustom = '(Custom)';

  { Status messages }
  SCompilerStatusStarting = '*** Starting compile.  [%s]';
  SCompilerStatusFinished = '*** Finished.  [%0:s, %1:s elapsed]';
  SCompilerStatusErrorAborted = '*** Compile aborted.';
  SCompilerStatusReset = '*** Log size limit reached, list reset.';

  SCompilerNeedCompiledExe = 'Cannot run Setup at this time. Please compile Setup successfully to completion first, with output enabled';
  SCompilerNeedUninstExe = 'Cannot run Uninstall at this time. Please run Setup successfully to completion first';
  SCompilerExecuteSetupError2 = 'Error executing "%0:s":' + SNewLine2 + '%1:d: %2:s';
  SCompilerAbortCompileConfirm = 'Are you sure you want to abort the compile?';

  SCompilerErrorFilePrefix = 'File %s:';
  SCompilerErrorLinePrefix = 'Line %d:';
  SCompilerErrorTitle = 'Compiler Error';

  { Line parsing }
  SCompilerIllegalNullChar = 'Illegal null character on line %d';

  { Find & Replace }
  SFindNotFound = 'Cannot find "%s"';
  SFindInvalidRegEx = 'Invalid regular expression "%s"';
  SFindResultLinePrefix = '  Line %d: ';
  SFindResultFileHeader = '%0:s (%1:d hits):';
  SFindResultSummary = 'Find "%0:s" (%1:d hits in %2:d files)';
  SReplaceCount = '%d occurrence(s) replaced.';

  { File association }
  SAssocSuccessCurrentUser = 'The .iss extension was successfully associated for the current user with:' + SNewLine + '%s';
  SAssocSuccessAllUsers = 'The .iss extension was successfully associated for all users with:' + SNewLine + '%s';
  SAssocError = 'Error creating file association:' + SNewLine + '%0:d - %1:s';
  SAssocUnableForAllUsers = 'Unable to associate for all users without administrative privileges. Do you want to associate only for yourself instead?';
  SAssocTitle = 'Associate';
  SAssocInnoSetupScript = 'Inno Setup Script';
  SAssocInnoSetupScriptCompile = 'Compi&le';

  { Registry Designer }
  SRegistryDesignerScriptHas = 'Script has %s';
  SRegistryDesignerScriptHasSet = 'Script has %s set';
  SRegistryDesignerInvalidFileFormat = 'Invalid file format.';

  { MsgBox Designer }
  SMsgBoxDesignerPreviewCaption = 'Setup';
  SMsgBoxDesignerDefaultText = 'Your message text.';
  SMsgBoxDesignerDefaultInputText = '<Enter your text here...>';
  SMsgBoxDesignerReturnValues = ' Return values ';
  SMsgBoxDesignerReturnValuesDefault = ' Return values /  -------  / Default ';
  SMsgBoxDesignerReturnValuesShield = ' Return values /  Shield ';
  SMsgBoxDesignerReturnValuesShieldDefault = ' Return values /  Shield  / Default ';
  SMsgBoxDesignerInstructionText = 'Instruction Text';
  SMsgBoxDesignerMessageText = 'Message Text';
  SMsgBoxDesignerUserClicked = 'user clicked %s';
  SMsgBoxDesignerButtonOK = 'OK';
  SMsgBoxDesignerButtonYes = 'Yes';
  SMsgBoxDesignerButtonNo = 'No';
  SMsgBoxDesignerButtonCancel = 'Cancel';
  SMsgBoxDesignerButtonRetry = 'Retry';
  SMsgBoxDesignerButtonIgnore = 'Ignore';
  SMsgBoxDesignerButtonAbort = 'Abort';
  SMsgBoxDesignerCommentDisplayMessageBox = '// Display a message box';

  { Sign Tools }
  SSignToolNamePrompt = 'Name of the Sign Tool:';
  SSignToolInvalidName = 'Invalid name.';
  SSignToolDuplicateName = 'Duplicate name.';
  SSignToolCommandPrompt = 'Command of the Sign Tool:';
  SSignToolInvalidCommand = 'Invalid command.';

  { Tools }
  SToolsInsertGuidConfirm = 'The generated GUID will be inserted into the editor at the cursor position. Continue?';
  SToolsNotInCodeSectionConfirm = 'The generated Pascal script will be inserted into the editor at the cursor position, but the cursor is not in the %s section. Continue anyway?';

  { Options }
  SOptionsKeyMappingDelphi = 'Classic';
  SOptionsKeyMappingVisualStudio = 'Visual Studio / Visual Studio Code';
  SOptionsMemoKeyMappingDefault = 'Classic / Visual Studio';
  SOptionsMemoKeyMappingVSCode = 'Visual Studio Code';
  SOptionsThemeLight = 'Light';
  SOptionsThemeDark = 'Dark';
  SOptionsThemeClassic = 'Classic';

  { Menu captions }
  SMenuNewWithWizard = '&New...';
  SMenuNew = '&New';

  { Navigation }
  SNavLineNumber = 'Line %d';
  SNavItemCaption = '%0:s: %1:s';
  SNavBack = 'Back (%s)';
  SNavForward = 'Forward (%s)';

  { Shortcuts }
  SShortCutCtrl = 'Ctrl+';
  SShortCutShift = 'Shift+';
  SShortCutAlt = 'Alt+';
  SShortCutNumpad = 'Num %s';

  { Caption status indicators }
  SCompilerCaption32Bit = '(32-bit)';
  SCompilerCaptionCompiling = '[Compiling]';
  SCompilerCaptionRunning = '[Running]';
  SCompilerCaptionPaused = '[Paused]';

  { Status bar }
  SStatusReadOnly = 'Read only';
  SStatusModified = 'Modified';
  SStatusOverwrite = 'Overwrite';
  SStatusInsert = 'Insert';
  SStatusTabsClosed = 'Tabs closed: %d';
  SStatusEstimatedTimeRemaining = ' Estimated time remaining: %0:.2d%1:s%2:.2d%3:s%4:.2d     Average KB/sec: %5:.0n';

  { File operations }
  SCompilerOpenFileErrorRemoveFromMRU = 'There was an error opening the file. Remove it from the list?';
  SCompilerFileChangedSavePrompt = 'The text in the %s file has changed.' + SNewLine2 + 'Do you want to save the changes?';
  SCompilerStopCompileBeforeCommand = 'Please stop the compile process before performing this command.';
  SCompilerCompileAlreadyInProgress = 'A compile is already in progress.';
  SCompilerIncludedFileChangedSavePrompt = 'The text in the %s file has changed and must be saved before compiling.' + SNewLine2 + 'Save the changes and continue?';
  SCompilerSaveScriptBeforeCompile = 'Would you like to save the script before compiling?' + SNewLine2 + 'If you answer No, the compiled installation will be placed under your My Documents folder by default.';
  SCompilerIncludedFileOpenAsTab = 'The selected file is an %s file. Go to its tab instead of opening it as the new main file?';
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

  { Update panel }
  SUpdatePanelVersionUpdated = 'Your version of Inno Setup has been updated! <a id="%s">See what''s new</a>.';
  SUpdatePanelVSCodeShortcutsAdded = 'VS Code-style editor shortcuts added! Use the <a id="%s">Editor Keys option</a> in Options dialog.';
  SUpdatePanelIdeasBoardOpen = '<a id="%s">Ideas board is open!</a> Share your ideas and vote on others, this month only.';
  SUpdatePanelRunningAfterEntitlementEnded = 'Running a version released after your update entitlement ended. <a id="%0:s">Renew license</a>, <a id="%1:s">remove key</a>, or <a id="%2:s">exit</a>.';
  SUpdatePanelEntitlementEndingSoon = 'Your update entitlement is ending soon. Please <a id="%s">renew your license</a>. Thanks!';
  SUpdatePanelEntitlementEnded = 'Your update entitlement has ended. Please <a id="%s">renew your license</a>. Thanks!';
  SUpdatePanelUsingCommercially = 'Using Inno Setup commercially? Please <a id="%s">purchase a license</a>. Thanks!';

  { Debugger }
  SDebugTargetSetup = 'Setup';
  SDebugTargetUninstall = 'Uninstall';
  SDebugExitCodeHex = '%0:s exit code: 0x%1:.8x';
  SDebugExitCodeDecimal = '%0:s exit code: %1:u';
  SDebugExitCodeGetFailed = 'Unable to get %0:s exit code (%1:s failed)';
  SDebugExitCodeStillRunning = '%s is still running; can''t get exit code';
  SDebugExitCodeWaitFailed = 'Unable to get %0:s exit code (%1:s failed)';
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
  SEvaluateHintSuccess = '%0:s = "%1:s"';
  SEvaluateHintException = '%0:s = Exception: %1:s';
  SEvaluateHintUnknownError = '%s = Unknown error';
  SEvaluateHintUnknownError2 = 'Unknown error';
  SRuntimeErrorLine = 'Line %0:d:' + SNewLine + '%1:s';
  SRuntimeErrorTitle = 'Runtime Error';
  SDebugTargetStarted = '*** %s started';
  SDebugTerminatingProcess = '*** Terminating process';
  SDebugRemovingTempDir = '*** Removing left-over temporary directory: %s';
  SDebugFailedToRemoveTempDir = '*** Failed to remove temporary directory';
  SDebugExitCodeText = '*** %s';
  SDebugCodeCallStack = '*** [%s] Call Stack';

  { License - first 3 duplicated in Shared.LicenseFunc for ISCC }
  SLicenseeExpired = '%s (Update entitlement ended)';
  SLicenseeExpiredButUpdated = '%s (Update entitlement ended but updated anyway)';
  SLicenseeNonCommercial = 'Non-commercial use only';
  SLicenseTypeSingleUser = 'Single User';
  SLicenseTypeTeam = 'Team';
  SLicenseTypeEnterprise = 'Enterprise';
  SLicenseTypeDescription = 'Inno Setup %s License';
  SLicenseDescriptionNameAndType = '%0:s, %1:s.';
  SLicenseDescriptionUpdatesUntil = 'Includes updates until %s, major and minor.';
  SLicenseDescriptionAllFutureUpdates = 'Includes all future updates, major and minor.';
 
implementation

end.
