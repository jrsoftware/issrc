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
  { Do not localize - should not use %1 etc }
  SNewLine = #13#10;  { line break }
  SNewLine2 = #13#10#13#10;  { double line break }
  SLitComment = '; ';
  SLitCodeComment = '// ';
  SLitStatusEventPrefix = '*** ';
  SLitIssExt = 'iss';
  SLitExeExt = 'exe';
  SLitRtfExt = 'rtf';
  SLitRegExt = 'reg';
  SLitIcoExt = 'ico';
  SLit7zExt  = '7z';
  SLitExtAndAllFilter = '%0:s (*.%1:s)|*.%1:s|%2:s|*.*';
  SLitDocsAndAllFilter = '%s (*.rtf,*.txt)|*.rtf;*.txt|%s|*.*';
  SLitAllFilesFilter = '%s|*.*';

  { Compiler-specific messages }
  SCompilerCommandLineHelpUsage = 'Command line usage:';
  SCompilerCommandLineHelpExamples = 'Examples:';
  SCompilerCommandLineHelpScriptFile = 'script file';
  SCompilerCommandLineHelpWizardName = 'wizard name';
  SCompilerCommandLineHelpMyScript = 'My script';
  SCompilerCommandLineHelpMyScriptWizard = 'My Script Wizard';
  SCompilerLibraryLoadError = 'Could not load %1: %2';

  { Compiler form labels }
  SCompilerFormCaption = 'Inno Setup Compiler';
  SCompilerExampleScripts = 'Example scripts...';
  SCompilerMoreFiles = 'More files...';
  SCompilerUntitledFile = 'Untitled';
  SCompilerPreprocessorOutput = 'Preprocessor Output';
  SCompilerRunParametersTitle = 'Run Parameters';
  SCompilerRunParametersPrompt = 'Command line parameters for %1 and %2:';

  { File filter names }
  SIssFiles = 'Inno Setup Script files';
  SAllFiles = 'All files';
  SExeFiles = 'Application files';
  SDocFiles = 'Documentation files';
  SRegFiles = 'Registry files';
  SIcoFiles = 'Icon files';

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
  SWizardAppFilesSubDirsMessage = 'Should files in subfolders of "%1" also be included?';
  SWizardAppFilesDownloadSourcePrompt = 'URL:';
  SWizardAppFilesDownloadExtractArchiveMessage = 'Is the file to download an archive which should be extracted?';
  SWizardAppFilesDownloadDestNamePrompt = 'Name of the file:';
  SWizardAppFilesDownloadArchiveDestNamePrompt = 'Name of the file (extension should match archive format):';
  SWizardAppFilesDownloadExternalSizePrompt = 'Approximate size of the file in megabytes:';
  SWizardAppAssoc = 'Application File Association';
  SWizardAppAssoc2 = 'Please specify which file association should be created for your application.';
  SWizardAppAssocDefaultName = '%1 File';
  SWizardAppIcons = 'Application Shortcuts';
  SWizardAppIcons2 = 'Please specify which shortcuts should be created for your application.';
  SWizardAppDocs = 'Application Documentation';
  SWizardAppDocs2 = 'Please specify which documentation files should be shown by Setup during installation.';
  SWizardPrivilegesRequired = 'Setup Install Mode';
  SWizardPrivilegesRequired2 = 'Please specify in which install mode Setup should run.';
  SWizardAppRegistry = 'Application Registry Keys And Values';
  SWizardAppRegistry2 = 'Please specify the registry keys and values that are part of your application.';
  SWizardLanguages = 'Setup Languages';
  SWizardLanguages2 = 'Please specify which Setup languages should be included.';
  SWizardCompiler = 'Compiler Settings';
  SWizardCompiler2 = 'Please specify some basic compiler settings.';
  SWizardCompilerOutputDir = 'Please specify the folder.';
  SWizardWizardStyle = 'Wizard Style';
  SWizardWizardStyle2 = 'Please specify which wizard style should be used.';
  SWizardISPP = 'Inno Setup Preprocessor';
  SWizardISPP2 = 'Please specify whether Inno Setup Preprocessor should be used.';
  SWizardISPPLabel = 'The %1 can use %2 compiler directives to simplify your script. Although this is not necessary, it will make it easier to manually change the script later.%n%nDo you want the %1 to use %2 compiler directives?';
  SWizardISPPCheck = '&Yes, use %1 compiler directives';
  SWizardFinished = 'Finished';

  SWizardNextButton = '&Next';
  SWizardFinishButton = '&Finish';
  SWizardCancelMessage = 'The %1 is not complete. If you quit now, the new script file will not be generated.%n%nExit the %1?';

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

  SWizardScriptHeader1 = 'Script generated by the %1.';
  SWizardScriptHeader2 = 'SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!';
  SWizardScriptCommentUniqueAppId = 'NOTE: The value of %1 uniquely identifies this application. Do not use the same %1 value in installers for other applications.';
  SWizardScriptCommentGenerateGuid = '(To generate a new GUID, click Tools | Generate GUID inside the IDE.)';
  SWizardScriptCommentArchitecturesAllowed = '"%1" specifies that Setup cannot run on anything but x64 and Windows 11 on Arm.';
  SWizardScriptCommentArchitecturesInstallIn64BitMode1 = '"%1" requests that the install be done in "64-bit mode" on x64 or Windows 11 on Arm.';
  SWizardScriptCommentArchitecturesInstallIn64BitMode2 = 'This means it should use the native 64-bit Program Files directory and the 64-bit view of the registry.';
  SWizardScriptCommentChangeTo64BitInstaller = 'Uncomment the following line to use a 64-bit installer.';
  SWizardScriptCommentArchiveExtractionEnhanced = 'Use "%1" if all your archives are .%2 files.';
  SWizardScriptCommentArchiveExtractionEnhancedNoPassword = 'Use "%1" if all your archives are not password-protected.';
  SWizardScriptCommentChangeToLowest = 'Uncomment the following line to run in non administrative install mode (install for current user only).';
  SWizardScriptCommentChangeToAdmin = 'Remove the following line to run in administrative install mode (install for all users).';
  SWizardScriptCommentSharedSystemFiles = 'NOTE: Don''t use "%1" on any shared system files.';
  SWizardScriptCommentVerifyDownloads = 'NOTE: Use the "%1" flag or the "%2" parameter to verify downloads.';
  SWizardScriptCommentRegistryDataFromFile = 'Registry data from file %1';
  SWizardScriptCommentCouldNotImport = 'COULD NOT IMPORT %1';
  SWizardScriptCommentEndOfRegistryDataFromFile = 'End of registry data from file %1';
  SWizardScriptCommentKeysFilteredDuePrivilegesRequired = 'SOME KEYS FILTERED DUE TO %1 SETTINGS!';
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
  SCompilerStatusStarting = 'Starting compile.%1[%2]';
  SCompilerStatusFinished = 'Finished.%1[%2, %3 elapsed]';
  SCompilerStatusErrorAborted = 'Compile aborted.';
  SCompilerStatusReset = 'Log size limit reached, list reset.';
  SDebugTargetStarted = '%1 started';
  SDebugTerminatingProcess = 'Terminating process';
  SDebugRemovingTempDir = 'Removing left-over temporary directory: %1';
  SDebugFailedToRemoveTempDir = 'Failed to remove temporary directory';
  SDebugCodeCallStack = '%1 Call Stack';

  SCompilerNeedCompiledExe = 'Cannot run Setup at this time. Please compile Setup successfully to completion first, with output enabled';
  SCompilerNeedUninstExe = 'Cannot run Uninstall at this time. Please run Setup successfully to completion first';
  SCompilerExecuteSetupError2 = 'Error executing "%1":%n%n%2: %3';
  SCompilerAbortCompileConfirm = 'Are you sure you want to abort the compile?';

  SCompilerErrorFilePrefix = 'File %1:';
  SCompilerErrorLinePrefix = 'Line %1:';
  SCompilerErrorTitle = 'Compiler Error';

  { Line parsing }
  SCompilerIllegalNullChar = 'Illegal null character on line %1';

  { Find & Replace }
  SFindNotFound = 'Cannot find "%1"';
  SFindInvalidRegEx = 'Invalid regular expression "%1"';
  SFindResultLinePrefix = 'Line %1:';
  SFindResultFileHeader = '%1 (%2 hits):';
  SFindResultSummary = 'Find "%1" (%2 hits in %3 files)';
  SReplaceCount = '%1 occurrence(s) replaced.';

  { File association }
  SAssocSuccessCurrentUser = 'The .%1 extension was successfully associated for the current user with:%n%2';
  SAssocSuccessAllUsers = 'The .%1 extension was successfully associated for all users with:%n%2';
  SAssocError = 'Error creating file association:%n%1 - %2';
  SAssocUnableForAllUsers = 'Unable to associate for all users without administrative privileges. Do you want to associate only for yourself instead?';
  SAssocTitle = 'Associate';
  SAssocInnoSetupScript = 'Inno Setup Script';
  SAssocInnoSetupScriptCompile = 'Compi&le';

  { Registry Designer }
  SDesignerScriptHas = '&Script has %1';
  SRegistryDesignerScriptHasSet = 'Script has %1 set';
  SRegistryDesignerInvalidFileFormat = 'Invalid file format.';

  { MsgBox Designer }
  SMsgBoxDesignerPreviewCaption = 'Setup';
  SMsgBoxDesignerDefaultText = 'Your message text.';
  SMsgBoxDesignerDefaultInputText = 'Enter your text here...';
  SMsgBoxDesignerReturnValues = 'Return values';
  SMsgBoxDesignerReturnValuesDefault = 'Default';
  SMsgBoxDesignerReturnValuesShield = 'Shield';
  SMsgBoxDesignerInstructionText = 'Instruction Text';
  SMsgBoxDesignerMessageText = 'Message Text';
  SMsgBoxDesignerUserClicked = 'user clicked %1';
  SMsgBoxDesignerButtonOK = 'OK';
  SMsgBoxDesignerButtonYes = 'Yes';
  SMsgBoxDesignerButtonNo = 'No';
  SMsgBoxDesignerButtonCancel = 'Cancel';
  SMsgBoxDesignerButtonRetry = 'Retry';
  SMsgBoxDesignerButtonIgnore = 'Ignore';
  SMsgBoxDesignerButtonAbort = 'Abort';
  SMsgBoxDesignerCommentDisplayMessageBox = 'Display a message box';

  { Sign Tools }
  SSignToolNamePrompt = 'Name of the Sign Tool:';
  SSignToolInvalidName = 'Invalid name.';
  SSignToolDuplicateName = 'Duplicate name.';
  SSignToolCommandPrompt = 'Command of the Sign Tool:';
  SSignToolInvalidCommand = 'Invalid command.';

  { Tools }
  SToolsInsertGuidConfirm = 'The generated GUID will be inserted into the editor at the cursor position. Continue?';
  SToolsNotInCodeSectionConfirm = 'The generated Pascal script will be inserted into the editor at the cursor position, but the cursor is not in the %1 section. Continue anyway?';

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
  SNavLineNumber = 'Line %1';
  SNavItemCaption = '%1: %2';
  SNavBack = 'Back (%1)';
  SNavForward = 'Forward (%1)';

  { Shortcuts }
  SShortCutCtrl = 'Ctrl+';
  SShortCutShift = 'Shift+';
  SShortCutAlt = 'Alt+';
  SShortCutNumpad = 'Num %1';

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
  SStatusTabsClosed = 'Tabs closed: %1';
  SStatusEstimatedTimeRemaining = 'Estimated time remaining: %1';
  SStatusAverage = 'Average KB/sec: %1';

  { File operations }
  SCompilerOpenFileErrorRemoveFromMRU = 'There was an error opening the file. Remove it from the list?';
  SCompilerFileChangedSavePrompt = 'The text in the %1 file has changed.%n%nDo you want to save the changes?';
  SCompilerStopCompileBeforeCommand = 'Please stop the compile process before performing this command.';
  SCompilerCompileAlreadyInProgress = 'A compile is already in progress.';
  SCompilerIncludedFileChangedSavePrompt = 'The text in the %1 file has changed and must be saved before compiling.%n%nSave the changes and continue?';
  SCompilerSaveScriptBeforeCompile = 'Would you like to save the script before compiling?%n%nIf you answer No, the compiled installation will be placed under your My Documents folder by default.';
  SCompilerIncludedFileOpenAsTab = 'The selected file is an %1 file. Go to its tab instead of opening it as the new main file?';
  SCompilerIncludedFileNotAvailableAsTab = 'The selected file is not available as a tab. Opening as the new main file instead.';
  SCompilerSaveErrorCreateFile = 'Error creating file (code %1). Could not save file';
  SCompilerSaveErrorCreateBackup = 'Error creating backup file. Could not save file';
  SCompilerSaveErrorRemoveExisting = 'Error removing existing file (code %1). Could not save file';
  SCompilerSaveErrorRenameTemp = 'Error renaming temporary file (code %1). Could not save file';
  SCompilerStatusFailedToOpenIncludedFile = 'Failed to open included file: %1';
  SCompilerPrinterDocumentStartError = 'Cannot start printer document.';
  SCompilerClearRecentFilesConfirm = 'Are you sure you want to clear the list of recently opened files?';
  SCompilerFileModifiedOutside = 'The %1 file has been modified outside of the source editor. You might want to reload it.';
  SCompilerFileModifiedReload = 'The %1 file has been modified outside of the source editor.%n%nDo you want to reload the file?';
  SCompilerFileModifiedReloadChanged = 'The %1 file has been modified outside of the source editor. Changes have also been made in the source editor.%n%nDo you want to reload the file and lose the changes made in the source editor?';
  SCompilerFileNotOpened = 'File not opened.';
  SGotoLineTitle = 'Go to Line';
  SGotoLinePrompt = 'Line number:';

  { License }
  SCompilerCopyLicenseKeyBeforePurchase = 'Do you want to copy your current license key to the clipboard before opening our order page? You will need it to be able to renew it.';
  SCompilerLicenseRegisterSuccess = 'New commercial license key has been registered:%n%n%1%n%nThanks for your support!';
  SCompilerRemoveLicenseConfirm = 'Are you sure you want to remove your commercial license key and revert to non-commercial use only?';
  SCompilerLicenseKeyRemoved = 'Commercial license key has been removed.';

  { Update panel }
  SUpdatePanelVersionUpdated = 'Your version of Inno Setup has been updated! <a id="%1">See what''s new</a>.';
  SUpdatePanelVSCodeShortcutsAdded = 'VS Code-style editor shortcuts added! Use the <a id="%1">Editor Keys option</a> in Options dialog.';
  SUpdatePanelIdeasBoardOpen = '<a id="%1">Ideas board is open!</a> Share your ideas and vote on others, this month only.';
  SUpdatePanelRunningAfterEntitlementEnded = 'Running a version released after your update entitlement ended. <a id="%1">Renew license</a>, <a id="%2">remove key</a>, or <a id="%3">exit</a>.';
  SUpdatePanelEntitlementEndingSoon = 'Your update entitlement is ending soon. Please <a id="%1">renew your license</a>. Thanks!';
  SUpdatePanelEntitlementEnded = 'Your update entitlement has ended. Please <a id="%1">renew your license</a>. Thanks!';
  SUpdatePanelUsingCommercially = 'Using Inno Setup commercially? Please <a id="%1">purchase a license</a>. Thanks!';

  { Debugger }
  SDebugTargetSetup = 'Setup';
  SDebugTargetUninstall = 'Uninstall';
  SDebugExitCodeHex = '%1 exit code: 0x%2';
  SDebugExitCodeDecimal = '%1 exit code: %2';
  SDebugExitCodeGetFailed = 'Unable to get %1 exit code (%2 failed)';
  SDebugExitCodeStillRunning = '%1 is still running; can''t get exit code';
  SDebugExitCodeWaitFailed = 'Unable to get %1 exit code (%2 failed)';
  SCompilerStopDebugTargetBeforeCommand = 'Please stop the running %1 process before performing this command.';
  SCompilerDetachDebuggerConfirm = 'This command will detach the debugger from the running %1 process. Continue?';
  SCompilerModifiedWhileRunningWarning = 'The changes you made will not take effect until you re-compile.%n%nContinue running anyway?';
  SCompilerPauseAlreadyPending = 'A pause is already pending.';
  SCompilerNoCodeGeneratedForLine = 'No code was generated for the current line.';
  SCompilerTerminateProcessConfirm = 'This will unconditionally terminate the running %1 process. Continue?';
  SCompilerTerminateProcessSetupNote = 'Note that if %1 is currently in the installation phase, any changes made to the system thus far will not be undone, nor will uninstall data be written.';
  SEvaluateTitle = 'Evaluate';
  SEvaluatePrompt = 'Constant to evaluate (for example, "%1"):';
  SEvaluateResultTitle = 'Evaluate Result';
  SEvaluateErrorTitle = 'Evaluate Error';
  SEvaluateUnknownError = 'An unknown error occurred.';
  SEvaluateHintSuccess = '%1 = "%2"';
  SEvaluateHintException = '%1 = Exception: %2';
  SEvaluateHintUnknownError = '%1 = Unknown error';
  SEvaluateHintUnknownError2 = 'Unknown error';
  SRuntimeErrorLine = 'Line %1:%n%2';
  SRuntimeErrorTitle = 'Runtime Error';

  { License - first 3 duplicated in Shared.LicenseFunc for ISCC }
  SLicenseeExpired = '%1 (Update entitlement ended)';
  SLicenseeExpiredButUpdated = '%1 (Update entitlement ended but updated anyway)';
  SLicenseeNonCommercial = 'Non-commercial use only';
  SLicenseTypeSingleUser = 'Single User';
  SLicenseTypeTeam = 'Team';
  SLicenseTypeEnterprise = 'Enterprise';
  SLicenseTypeDescription = 'Inno Setup %1 License';
  SLicenseDescriptionNameAndType = '%1, %2.';
  SLicenseDescriptionUpdatesUntil = 'Includes updates until %1, major and minor.';
  SLicenseDescriptionAllFutureUpdates = 'Includes all future updates, major and minor.';

implementation

end.
