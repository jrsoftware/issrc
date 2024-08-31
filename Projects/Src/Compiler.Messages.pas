unit Compiler.Messages;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler Messages

  All language-specific text used by the compiler is in here. If you want to
  translate it into another language, all you need to change is this unit.
}

interface

const
  SNewLine = #13#10;  { line break }
  SNewLine2 = #13#10#13#10;  { double line break }
  
  { Compiler-specific messages }
  SCompilerVersion = 'version %s';

  SCompilerNotOnWin32s = 'The 32-bit compiler will not run on Win32s.';

  { Status messages }
  SCompilerStatusPreprocessing = 'Preprocessing';
  SCompilerStatusPreprocessorStatus = '   %s';
  SBuiltinPreprocessStatusIncludingFile = 'Including file: %s';
  SCompilerStatusCreatingOutputDir = 'Creating output directory: %s';
  SCompilerStatusCreatingSignedUninstallerDir = 'Creating signed uninstaller directory: %s';
  SCompilerStatusDeletingPrevious = 'Deleting %s from output directory';
  SCompilerStatusParsingSectionLine = 'Parsing [%s] section, line %d';
  SCompilerStatusParsingSectionLineFile = 'Parsing [%s] section, line %d of %s';
  SCompilerStatusFilesVerInfo = '   Reading version info: %s';
  SCompilerStatusReadingFile = 'Reading file (%s)';
  SCompilerStatusPreparingSetupExe = 'Preparing Setup program executable';
  SCompilerStatusSkippingPreparingSetupExe = 'Skipping preparing Setup program executable, output is disabled';
  SCompilerStatusSignedUninstallerNew = '   Creating new signed uninstaller file: %s';
  SCompilerStatusSignedUninstallerExisting = '   Using existing signed uninstaller file: %s';
  SCompilerStatusDeterminingCodePages = 'Determining language code pages';
  SCompilerStatusConvertCodePage = '   Conversion code page: %d';
  SCompilerStatusReadingDefaultMessages = 'Reading default messages from Default.isl';
  SCompilerStatusParsingMessages = 'Parsing [LangOptions], [Messages], and [CustomMessages] sections';
  SCompilerStatusReadingCode = 'Reading [Code] section';
  SCompilerStatusCompilingCode = 'Compiling [Code] section';
  SCompilerStatusReadingInFile = '   Reading file: %s';
  SCompilerStatusReadingInScriptMsgs = '   Messages in script file';
  SCompilerStatusCreateSetupFiles = 'Creating setup files';
  SCompilerStatusSkippingCreateSetupFiles = 'Skipping creating setup files, output is disabled';
  SCompilerStatusCreateManifestFile = 'Creating manifest file';
  SCompilerStatusFilesCompressing = '   Compressing: %s';
  SCompilerStatusFilesCompressingVersion = '   Compressing: %s   (%u.%u.%u.%u)';
  SCompilerStatusFilesStoring = '   Storing: %s';
  SCompilerStatusFilesStoringVersion = '   Storing: %s   (%u.%u.%u.%u)';
  SCompilerStatusCompressingSetupExe = '   Compressing Setup program executable';
  SCompilerStatusUpdatingVersionInfo = '   Updating version info (%s)';
  SCompilerStatusUpdatingManifest = '   Updating manifest (%s)';
  SCompilerStatusUpdatingIcons = '   Updating icons (%s)';
  SCompilerStatusCreatingDisk = '   Creating disk %d';
  SCompilerStatusError = 'ERROR:';
  SCompilerStatusWarning = 'Warning: ';
  SCompilerStatusSigningSetup = '   Signing Setup program executable';
  SCompilerStatusSigningSourceFile = '   Signing: %s';
  SCompilerStatusSourceFileAlreadySigned = '   Skipping signing, already signed: %s';
  SCompilerStatusSigning = '   Running Sign Tool %s: %s';
  SCompilerStatusSigningWithDelay = '   Running Sign Tool %s in %d milliseconds: %s';
  SCompilerStatusWillRetrySigning = '   Sign Tool command failed (%s). Will retry (%d tries left).';

  SCompilerSuccessfulMessage2 = 'The setup images were successfully created ' +
    'in the output directory:' + SNewLine +
    '%s' + SNewLine +
    SNewLine +
    'Would you like to test the installation now?';
  SCompilerSuccessfulTitle = 'Compile Successful';

  SCompilerAborted = 'Compile aborted. Please correct the problem and try again.';

  { Fatal errors }
  SCompilerScriptMissing2 = 'Specified script file does not exist';
  SCompilerOutputNotEmpty2 = 'Output directory must be empty prior to ' +
    'compilation of any non-Setup files. Files named SETUP.* are ' +
    'automatically deleted at the start of compilation.';
  SCompilerUnknownFilenamePrefix = 'Unknown filename prefix "%s"';
  SCompilerSourceFileDoesntExist = 'Source file "%s" does not exist';
  SCompilerSourceFileNotSigned = 'Source file "%s" is not signed';
  SCompilerCopyError3 = 'Could not copy "%s" to "%s".' + SNewLine2 + 'Error %d: %s';
  SCompilerReadError = 'Could not read "%s".' + SNewLine2 + 'Error: %s';
  SCompilerCompressError2 = 'An internal error occurred while trying to compress "%s"';
  SCompilerNotEnoughSpaceOnFirstDisk = 'There is not enough space on the first disk to copy all of the required files';
  SCompilerSetup0Mismatch = 'Internal error SC1';
  SCompilerMustUseDiskSpanning = 'Disk spanning must be enabled in order to create an installation larger than %d bytes in size';
  SCompilerCompileCodeError = 'An error occurred while trying to compile the [Code] section:' + SNewLine2 + '%s';
  SCompilerFunctionFailedWithCode = '%s failed. Error %d: %s';

  { [Setup] }
  SCompilerUnknownDirective = 'Unrecognized [%s] section directive "%s"';
  SCompilerEntryObsolete = 'The [%s] section directive "%s" is obsolete and ignored in this version of Inno Setup.';
  SCompilerEntrySuperseded2 = 'The [%s] section directive "%s" has been superseded by "%s" in this version of Inno Setup.';
  SCompilerEntryMissing2 = 'Required [%s] section directive "%s" not specified';
  SCompilerEntryInvalid2 = 'Value of [%s] section directive "%s" is invalid';
  SCompilerEntryAlreadySpecified = '[%s] section directive "%s" already specified';
  SCompilerAppVersionOrAppVerNameRequired = 'The [Setup] section must include an AppVersion or AppVerName directive';
  SCompilerMinVersionWinMustBeZero = 'Minimum non NT version specified by MinVersion must be 0. (Windows 95/98/Me are no longer supported.)';
  SCompilerMinVersionNTTooLow = 'Minimum version specified by MinVersion must be at least %s otherwise Setup will never run. (Windows Vista/Server 2008 are no longer supported.)';
  SCompilerOnlyBelowVersionNTTooLow = 'Minimum version specified by OnlyBelowVersion must be higher than %s if not 0 otherwise Setup will never run. (Windows Vista/Server 2008 are no longer supported.)';
  SCompilerMinVersionRecommendation = 'Minimum version is set to %s but using %s instead (which is the default) is recommended.';
  SCompilerDiskSliceSizeInvalid = 'DiskSliceSize must be between %d and %d, or "max"';
  SCompilerDiskClusterSizeInvalid = 'DiskClusterSize must be between 1 and 32768';
  SCompilerInstallModeObsolete = 'The [%s] section directive "%s" is obsolete and ignored in this version of Inno Setup. Use command line parameters instead.';
  SCompilerMessagesFileObsolete = 'The MessagesFile directive is obsolete and no longer supported. Use the [Languages] section instead.';
  SCompilerMustUseDisableStartupPrompt = 'DisableStartupPrompt must be set to "yes" when AppName includes constants';
  SCompilerMustNotUsePreviousLanguage = 'UsePreviousLanguage must be set to "no" when AppId includes constants';
  SCompilerMustNotUsePreviousPrivileges = 'UsePreviousPrivileges must be set to "no" when AppId includes constants and PrivilegesRequiredOverridesAllowed allows "dialog"';
  SCompilerDirectiveNotUsingDefault = 'The [Setup] section directive "%s" is not assuming a default value because %s includes constants.';
  SCompilerDirectiveNotUsingPreferredDefault = 'The [Setup] section directive "%s" is defaulting to %s because %s includes constants.';
  SCompilerDirectivePatternTooLong = 'The [Setup] section directive "%s" contains a pattern that is too long';
  SCompilerOutputBaseFileNameSetup = 'Setting the [Setup] section directive "OutputBaseFileName" to "setup" is not recommended: all executables named "setup.exe" are shimmed by Windows application compatibility to load additional DLLs, such as version.dll.' + ' These DLLs are loaded unsafely by Windows and can be hijacked. Use a different name, for example "mysetup".';
  SCompilerWizImageRenamed = 'Wizard image "%s" has been renamed. Use "%s" instead or consider removing the directive to use modern built-in wizard images.';
  SCompilerArchitectureIdentifierInvalid = 'Architecture identifier "%s" is invalid';
  SCompilerArchitectureIdentifierDeprecatedWarning = 'Architecture identifier "%s" is deprecated. ' +
    'Substituting "%s", but note that "%s" is preferred in most cases. See the "Architecture Identifiers" topic in help file for more information.';

  { Signing }
  SCompilerSignatureNeeded = 'Signed uninstaller mode is enabled. Using ' +
    'an external code-signing tool, please attach your digital signature ' +
    'to the following executable file:' + SNewLine2 + '%s' + SNewLine2 +
    'and compile again';
  SCompilerSignatureInvalid = 'Digital signature appears to be invalid';
  SCompilerSignedFileContentsMismatchRetry = 'The contents of the signed file:' +
    SNewLine2 + '%s' + SNewLine2 + 'differ unexpectedly from the original ' +
    'file. Try deleting the signed file and compiling again. If this error ' +
    'persists, please report the problem';
  SCompilerSignedFileContentsMismatch = 'The contents of the signed file:' +
    SNewLine2 + '%s' + SNewLine2 + 'differ unexpectedly from the original ' +
    'file';
  SCompilerNoSetupLdrSignError = 'The SignTool and SignedUninstaller directives may not be set when UseSetupLdr is set to "no"';
  SCompilerSignToolFileNameSequenceNotFound = 'Unable to run Sign Tool %s: $f sequence is missing.';
  SCompilerSignToolCreateProcessFailed = 'Failed to execute Sign Tool command.' +
    SNewLine2 + 'Error %d: %s';
  SCompilerSignToolNonZeroExitCode = 'Sign Tool command failed with exit code 0x%x';
  SCompilerSignToolSucceededButNoSignature = 'The Sign Tool command returned an ' +
    'exit code of 0, but the file does not have a digital signature';

  { Line parsing }
  SCompilerLineTooLong = 'Line too long';
  SCompilerSectionTagInvalid = 'Invalid section tag';
  SCompilerSectionBadEndTag = 'Not inside "%s" section, but an end tag for ' +
    'it was encountered';
  SCompilerTextNotInSection = 'Text is not inside a section';
  SCompilerInvalidDirective = 'Invalid compiler directive' +
    SNewLine2 + 'To be able to use compiler directives other than ''#include'', you need Inno Setup Preprocessor (ISPP) which is currently not installed.' +
    SNewLine2 + 'To install ISPP, reinstall Inno Setup and enable the ISPP option.';
  SCompilerErrorOpeningIncludeFile = 'Couldn''t open include file "%s": %s';
  SCompilerRecursiveInclude = 'Recursive include of "%s"';
  SCompilerIllegalNullChar = 'Illegal null character on line %d';
  SCompilerISPPMissing = 'ISPP.dll is missing';

  { Constant checks }
  SCompilerTwoBraces = 'Use two consecutive "{" characters if you are trying ' +
    'to embed a single "{" and not a constant';
  SCompilerUnknownConst = 'Unknown constant "%s".' +
    SNewLine2 + SCompilerTwoBraces;
  SCompilerUnterminatedConst = 'A "}" is missing at the end of the constant "%s".' +
    SNewLine2 + SCompilerTwoBraces;
  SCompilerConstCannotUse = 'The constant "%s" cannot be used here';
  SCompilerBadEnvConst = 'Invalid environment constant "%s"';
  SCompilerBadRegConst = 'Invalid registry constant "%s"';
  SCompilerBadIniConst = 'Invalid INI constant "%s"';
  SCompilerBadParamConst = 'Invalid command line parameter constant "%s"';
  SCompilerBadCodeConst = 'Invalid code constant "%s"';
  SCompilerBadDriveConst = 'Invalid drive constant "%s"';
  SCompilerBadCustomMessageConst = 'Invalid custom message constant "%s"';
  SCompilerBadBoolConst = 'Invalid boolean constant "%s"';
  SCompilerConstantRenamed = 'Constant "%s" has been renamed. Use "%s" instead.';
  SCompilerCommonConstantRenamed = 'Constant "%s" has been renamed. Use "%s" instead or consider using its "auto" form.';

  { Special warnings }
  SCompilerMissingRunOnceIdsWarning = 'There are [%s] section entries without a %s parameter. '+
    'By assigning a string to %1:s, you can ensure that a particular [%0:s] entry will only be executed once during uninstallation. ' +
    'See the "[%0:s]" topic in help file for more information.';
  SCompilerUsedUserAreasWarning = 'The [%s] section directive "%s" is set to "%s" but per-user areas (%s) are used by the script. ' +
    'Regardless of the version of Windows, if the installation is running in administrative install mode then you should be careful about making any per-user area changes: such changes may not achieve what you are intending. ' +
    'See the "UsedUserAreasWarning" topic in help file for more information.';
  SCompilerOnlyBelowVersionParameterNTTooLowWarning = 'Minimum version specified by the OnlyBelowVersion parameter of an entry should be higher than %s if not 0 otherwise the entry is never processed. The entry should probably be removed. (Windows Vista/Server 2008 are no longer supported.)';

  { Directive parsing }
  SCompilerDirectiveNameMissing = 'Missing directive name';
  SCompilerDirectiveHasNoValue = 'Directive "%s" has no value';

  { Parameter parsing }
  SCompilerParamHasNoValue = 'Specified parameter "%s" has no value';
  SCompilerParamQuoteError = 'Mismatched or misplaced quotes on parameter "%s"';
  SCompilerParamMissingClosingQuote = 'Missing closing quote on parameter "%s"';
  SCompilerParamDataTooLong = 'Data on parameter "%s" is too long';
  SCompilerParamUnknownParam = 'Unrecognized parameter name "%s"';
  SCompilerParamDuplicated = 'Cannot have multiple "%s" parameters';
  SCompilerParamEmpty2 = 'Parameter "%s" is empty';
  SCompilerParamNotSpecified = 'Required parameter "%s" not specified';
  SCompilerParamNoQuotes2 = 'Parameter "%s" cannot include quotes (")';
  SCompilerParamNoBackslash = 'Parameter "%s" cannot include backslashes (\)';
  SCompilerParamNoPrecedingBackslash = 'Parameter "%s" cannot begin with a backslash (\)';
  SCompilerParamInvalid2 = 'Parameter "%s" is not a valid value';

  { Flags }
  SCompilerParamUnknownFlag2 = 'Parameter "%s" includes an unknown flag';
  SCompilerParamErrorBadCombo2 = 'Parameter "%s" cannot have both the "%s" and "%s" flags';
  SCompilerParamErrorBadCombo3 = 'Parameter "%s" cannot have both the "%s" and "%s" flags on the same source file';
  SCompilerParamUnsupportedFlag = 'Parameter "%s" includes a flag that is not supported in this section';
  SCompilerParamFlagMissing = 'Flag "%s" must be used if flag "%s" is used';
  SCompilerParamFlagMissing2 = 'Flag "%s" must be used if parameter "%s" is used';
  SCompilerParamFlagMissing3 = 'Flag "%s" must be used if flags "%s" and "%s" are both used';

  { Types, components, tasks, check, beforeinstall, afterinstall }
  SCompilerParamUnknownType = 'Parameter "%s" includes an unknown type';
  SCompilerParamUnknownComponent = 'Parameter "%s" includes an unknown component';
  SCompilerParamUnknownTask = 'Parameter "%s" includes an unknown task';
  SCompilerExpressionError = 'Directive or parameter "%s" expression error: %s';
  SCompilerBadCheckOrInstall = 'Invalid Check, BeforeInstall or AfterInstall parameter "%s"';

  { Permissions }
  SCompilerPermissionsInvalidValue = 'Parameter "Permissions" includes a malformed value: "%s"';
  SCompilerPermissionsUnknownSid = 'Parameter "Permissions" includes an unknown SID: "%s"';
  SCompilerPermissionsUnknownMask = 'Parameter "Permissions" includes an unknown access type: "%s"';
  SCompilerPermissionsValueLimitExceeded = 'Parameter "Permissions" cannot include more than %d values';
  SCompilerPermissionsTooMany = 'Too many unique "Permissions" parameter values';

  { [Code] }
  SCompilerCodeUnsupportedEventFunction = 'Event function named "%s" is no longer supported. Create a "%s" function instead';
  SCompilerCodeFunctionRenamed = 'Support function "%s" has been renamed. Use "%s" instead.';
  SCompilerCodeFunctionRenamedWithAlternative = 'Support function "%s" has been renamed. Use "%s" instead or consider using "%s".';
  SCompilerCodeFunctionDeprecatedWithAlternativeAndDocs = 'Support function "%s" is deprecated. Use "%s" instead, but note that "%s" is preferred in most cases. See the "%s" topic in help file for more information.';

  { [Types] }
  SCompilerTypesCustomTypeAlreadyDefined = 'A custom type has already been defined';

  { [Components], [Tasks], [Languages] }
  SCompilerComponentsOrTasksBadName = 'Parameter "Name" includes invalid characters.' + SNewLine2 +
    'It may only include alphanumeric characters, underscores, slashes (/), and/or backslashes (\), may not start with a number and may not start or end with a slash or a backslash. Names ''not'', ''and'' and ''or'' are reserved';
  SCompilerComponentsInvalidLevel = 'Component cannot be more than one level below the preceding component';
  SCompilerTasksInvalidLevel = 'Task cannot be more than one level below the preceding task'; 
  SCompilerLanguagesBadName = 'Parameter "Name" includes invalid characters.' + SNewLine2 + 'It may only include alphanumeric characters and/or underscores, and may not start with a number. Names ''not'', ''and'' and ''or'' are reserved';

  { [Languages] }
  SCompilerParamUnknownLanguage = 'Parameter "%s" includes an unknown language';

  { [Messages] }
  SCompilerMessagesMissingEquals = 'Missing "=" separator between message name and text';
  SCompilerMessagesNotRecognizedDefault = 'Message name "%s" in Default.isl is not recognized by this version of Inno Setup';
  SCompilerMessagesNotRecognizedWarning = 'Message name "%s" is not recognized by this version of Inno Setup. Ignoring.';
  SCompilerMessagesNotRecognizedInFileWarning = 'Message name "%s" in "%s" is not recognized by this version of Inno Setup. Ignoring.';
  SCompilerMessagesMissingDefaultMessage = 'A message named "%s" has not been defined in Default.isl. It is required by this version of Inno Setup';
  SCompilerMessagesMissingMessageWarning = 'A message named "%s" has not been defined for the "%s" language. Will use the English message from Default.isl.';

  { [CustomMessages] }
  SCompilerCustomMessageBadName = 'Custom message name may only include alphanumeric characters and/or underscores, and cannot begin with a number';
  SCompilerCustomMessagesMissingLangWarning = 'Custom message "%s" has not been defined for the "%s" language. Will use the custom message from the first language in which it was defined: "%s".';
  SCompilerCustomMessagesMissingName = 'A custom message named "%s" has not been defined';

  { [Messages] & [LangOptions] }
  SCompilerUnknownLanguage = 'Unknown language name "%s"';
  SCompilerCantSpecifyLanguage = 'A language name may not be specified in a messages file';
  SCompilerCantSpecifyLangOption = 'Language option "%s" cannot be applied to all languages';

  { [Files] }
  SCompilerFilesTmpBadFlag = 'Parameter "Flags" cannot have the "%s" flag on ' +
    'a file copied to the {tmp} directory, or when the "deleteafterinstall" or ' +
    '"dontcopy" flag is used';
  SCompilerFilesWildcardNotMatched = 'No files found matching "%s"';
  SCompilerFilesDestNameCantBeSpecified = 'Parameter "DestName" cannot be specified if ' +
    'the "Source" parameter contains wildcards';
  SCompilerFilesStrongAssemblyNameMustBeSpecified = 'Parameter "StrongAssemblyName" must be specified if ' +
    'the flag "gacinstall" is used';
  SCompilerFilesCantHaveExternalExclude = 'Parameter "Excludes" may not be used when ' +
    'the "external" flag is used';
  SCompilerFilesCantHaveNonExternalExternalSize = 'Parameter "ExternalSize" may only be used when ' +
    'the "external" flag is used';
  SCompilerFilesExcludeTooLong = 'Parameter "Excludes" contains a pattern that is too long';
  SCompilerFilesUnsafeFile = 'Unsafe file detected: %s.' + SNewLine2 +
    'See the "Unsafe Files" topic in the help file for more information';
  SCompilerFilesSystemDirUsed = 'Attempt to deploy DLL file from own Windows System directory.' + SNewLine2 +
    'See the "Unsafe Files" topic in the help file for more information on why this is dangerous and should be avoided';
  SCompilerFilesSystemDirNotUsed = 'Attempt to deploy registered file %s to a location other than {sys}.' + SNewLine2 +
    'See the "Unsafe Files" topic in the help file for more information on why this is dangerous and should be avoided';
  SCompilerFilesIgnoreVersionUsedUnsafely =
    'Unsafe flag usage on file "%s": The "ignoreversion" flag should not be ' +
    'used on files installed to the Windows System directory ("{sys}").';
  SCompilerFilesWarningCopyMode = '"CopyMode: %s" has been superseded by "Flags: %s" in ' +
    'this version of Inno Setup. Behaving as if "Flags: %s" were specified.';
  SCompilerFilesWarningASISOO = '"CopyMode: alwaysskipifsameorolder" is deprecated and ' +
    'ignored in this version of Inno Setup. It is now the default behavior.';
  SCompilerFilesWarningSharedFileSysWow64 = 'DestDir should not be set to ' +
    '"{syswow64}" when the "sharedfile" flag is used. See the "sharedfile" ' +
    'documentation in the help file for details.';

  { [Icons] }
  SCompilerIconsNamePathNotSpecified = 'Parameter "Name" must include a path for the icon, ' +
    'for example, "{group}\My Icon"';
  SCompilerIconsIconIndexInvalid = 'Parameter "IconIndex" is not a valid integer';

  { [Registry] }
  SCompilerRegistryDeleteKeyProhibited = 'The "uninsdeletekey" and ' +
    '"deletekey" flags are prohibited on the specified key because the ' +
    'results would be disastrous. (You probably mean to delete a value instead.)';

  { [Run] }
  SCompilerRunCantUseRunOnceId = 'Parameter "RunOnceId" can only be used in ' +
    'an [UninstallRun] section';
  SCompilerRunFlagObsolete = 'Flag "%s" is obsolete. Use "%s" instead.';
  SCompilerRunMultipleWaitFlags = 'Parameter "Flags" cannot include multiple "wait" flags';

  { [UninstallRun] }
  SCompilerUninstallRunCantUseDescription = 'Parameter "Description" can only be used in ' +
    'a [Run] section';

implementation

end.
