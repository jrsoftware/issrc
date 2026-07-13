unit IDE.ScriptModel.Metadata;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Per-section metadata tables for the script model: parameter and directive
  names, value kinds, and flag lists, plus the rules layer registrations.

  Tables for [Setup] and [LangOptions] are generated from enums at startup,
  and are enriched with default values. [Messages] and [CustomMessages] do
  not have tables.
}

interface

uses
  Generics.Collections,
  Shared.SetupSectionDirectives;

type
  TScriptParameterValueKind = (pvkString, pvkInteger, pvkVersion,
    pvkChoice, pvkFlags, pvkYesNo);

  TScriptParameterDefinition = record
    Name: String;
    ValueKind: TScriptParameterValueKind;
    KnownValues: TArray<String>; { For pvkFlags: the known flags
                                   For pvkChoice: the known choices (other choices might still be valid, like a scripted expression)
                                   For pvkYesNo: 'yes' and 'no' (other values might still be valid, like an ISPP inline directive) }
    DefaultValue: String; { The default of a directive, empty for parameters }
    Obsolete: Boolean;    { To be hidden unless explicitly present in the script }
  end;

  { Rule: including a flag must also include other flags, for example checking
    extractarchive on a [Files] entry also requires external and ignoreversion }
  TScriptFlagIncludesRule = record
    ParameterName: String;
    FlagName: String;
    AlsoIncludedFlagNames: TArray<String>; { the other flags to include }
  end;

  { Rule: giving a parameter a non-empty value must also include a flag, for example
    setting Verb on a [Run] entry also requires the shellexec flag }
  TScriptParameterIncludesFlagRule = record
    ParameterName: String;      { the parameter whose value triggers the rule }
    FlagParameterName: String;  { the flag-list parameter that holds the flag }
    FlagName: String;           { the flag to include }
  end;

  TScriptSectionMetadata = class
  private
    FSectionName: String;
    FParameters: TArray<TScriptParameterDefinition>;
    FParametersByName: TDictionary<String, Integer>; { FParameters index by name }
    FFlagIncludesRules: TArray<TScriptFlagIncludesRule>;
    FParameterIncludesFlagRules: TArray<TScriptParameterIncludesFlagRule>;
  public
    constructor Create(const ASectionName: String;
      const AParameters: TArray<TScriptParameterDefinition>;
      const AFlagIncludesRules: TArray<TScriptFlagIncludesRule>;
      const AParameterIncludesFlagRules: TArray<TScriptParameterIncludesFlagRule> = nil);
    destructor Destroy; override;
    function TryGetParameter(const AName: String;
      out ADefinition: TScriptParameterDefinition): Boolean;
    property SectionName: String read FSectionName;
    property Parameters: TArray<TScriptParameterDefinition> read FParameters;
    property FlagIncludesRules: TArray<TScriptFlagIncludesRule> read FFlagIncludesRules;
    property ParameterIncludesFlagRules: TArray<TScriptParameterIncludesFlagRule>
      read FParameterIncludesFlagRules;
  end;

const
  SetupSectionDirectivesYesNo = [
    ssAllowCancelDuringInstall, ssAllowNetworkDrive, ssAllowNoIcons, ssAllowRootDirectory,
    ssAllowUNCPath, ssAlwaysRestart, ssAlwaysShowComponentsList, ssAlwaysShowDirOnReadyPage,
    ssAlwaysShowGroupOnReadyPage, ssAlwaysUsePersonalGroup, ssAppendDefaultDirName,
    ssAppendDefaultGroupName, ssASLRCompatible, ssCreateAppDir, ssDEPCompatible,
    ssDisableFinishedPage, ssDisableReadyMemo, ssDisableReadyPage, ssDisableStartupPrompt,
    ssDisableWelcomePage, ssDiskSpanning, ssDontMergeDuplicateFiles, ssEnableDirDoesntExistWarning,
    ssFlatComponentsList, ssMergeDuplicateFiles, ssMissingMessagesWarning,
    ssMissingRunOnceIdsWarning, ssNotRecognizedMessagesWarning, ssOutput, ssRedirectionGuard,
    ssRestartApplications, ssRestartIfNeededByRun, ssSetupLogging, ssShowComponentSizes,
    ssShowTasksTreeLines, ssSignedUninstaller, ssSignToolRunMinimized, ssSolidCompression,
    ssTerminalServicesAware, ssTimeStampsInUTC, ssUpdateUninstallLogAppName, ssUninstallLogging,
    ssUninstallRestartComputer, ssUsedUserAreasWarning, ssUsePreviousLanguage, ssUsePreviousPrivileges,
    ssUserInfoPage, ssWizardImageStretch, ssWizardKeepAspectRatio];

  SetupSectionDirectivesYesNoOrScripted = [ssChangesAssociations, ssChangesEnvironment,
    ssCreateUninstallRegKey, ssUninstallable, ssUsePreviousAppDir, ssUsePreviousGroup,
    ssUsePreviousSetupType, ssUsePreviousTasks, ssUsePreviousUserInfo];

  SetupSectionDirectivesAutoYesNo = [
    ssDirExistsWarning, ssDisableDirPage, ssDisableProgramGroupPage, ssShowLanguageDialog];

  SYes = 'yes';
  SNo = 'no';
  SAuto = 'auto';

function TryGetScriptSectionMetadata(const ASectionName: String;
  out AMetadata: TScriptSectionMetadata): Boolean;

function ScriptCategoryNamesOrdered: TArray<String>;

function TryGetScriptCategory(const ASectionName, AName: String;
  out ACategoryName: String): Boolean;

implementation

uses
  SysUtils, TypInfo, Generics.Defaults,
  Shared.LangOptionsSectionDirectives;

var
  SectionMetadataList: TObjectList<TScriptSectionMetadata>;

{ TScriptSectionMetadata }

constructor TScriptSectionMetadata.Create(const ASectionName: String;
  const AParameters: TArray<TScriptParameterDefinition>;
  const AFlagIncludesRules: TArray<TScriptFlagIncludesRule>;
  const AParameterIncludesFlagRules: TArray<TScriptParameterIncludesFlagRule>);
begin
  inherited Create;
  FSectionName := ASectionName;
  FParameters := AParameters;
  FParametersByName := TDictionary<String, Integer>.Create(TIStringComparer.Ordinal);
  for var I := 0 to High(FParameters) do
    FParametersByName.Add(FParameters[I].Name, Integer(I)); { Add raises on a duplicate name }
  FFlagIncludesRules := AFlagIncludesRules;
  FParameterIncludesFlagRules := AParameterIncludesFlagRules;
end;

destructor TScriptSectionMetadata.Destroy;
begin
  FParametersByName.Free;
  inherited;
end;

function TScriptSectionMetadata.TryGetParameter(const AName: String;
  out ADefinition: TScriptParameterDefinition): Boolean;
begin
  var I: Integer;
  Result := FParametersByName.TryGetValue(AName, I);
  if Result then
    ADefinition := FParameters[I];
end;

function TryGetScriptSectionMetadata(const ASectionName: String;
  out AMetadata: TScriptSectionMetadata): Boolean;
begin
  for var Metadata in SectionMetadataList do begin
    if SameText(Metadata.SectionName, ASectionName) then begin
      AMetadata := Metadata;
      Exit(True);
    end;
  end;
  Result := False;
end;

var
  ScriptCategoryDictionary: TDictionary<String, String>;

function ScriptCategoryDictionaryKey(const ASectionName, AName: String): String;
begin
  Result := ASectionName + '.' + AName;
end;

var
  ScriptCategoryNameOrderedList: TArray<String>;

function ScriptCategoryNamesOrdered: TArray<String>;
begin
  Result := Copy(ScriptCategoryNameOrderedList);
end;

function TryGetScriptCategory(const ASectionName, AName: String;
  out ACategoryName: String): Boolean;
begin
  Result := ScriptCategoryDictionary.TryGetValue(
    ScriptCategoryDictionaryKey(ASectionName, AName), ACategoryName);
end;

procedure InitializeScriptCategories;

  procedure CD(const AName: String; const AMemberNames: TArray<String>;
    const ASectionNames: TArray<String>);
  begin
    if Length(ASectionNames) = 0 then
      raise Exception.CreateFmt('Internal error: Category %s has no section names', [AName]);
    ScriptCategoryNameOrderedList := ScriptCategoryNameOrderedList + [AName];
    for var SectionName in ASectionNames do begin
      for var MemberName in AMemberNames do begin
        ScriptCategoryDictionary.Add(ScriptCategoryDictionaryKey(SectionName, MemberName),
          AName);
      end;
    end;
  end;

begin
  const SetupSection: TArray<String> = ['Setup'];

  CD('Compiler', ['ASLRCompatible', 'DEPCompatible',
    'DisablePrecompiledFileVerifications', 'DiskClusterSize', 'DiskSliceSize',
    'DiskSpanning', 'Encryption', 'EncryptionKeyDerivation',
    'MergeDuplicateFiles', 'MissingMessagesWarning', 'MissingRunOnceIdsWarning',
    'NotRecognizedMessagesWarning', 'Output', 'OutputBaseFilename', 'OutputDir',
    'OutputManifestFile', 'ReserveBytes', 'SignedUninstaller',
    'SignedUninstallerDir', 'SignTool', 'SignToolMinimumTimeBetween',
    'SignToolRetryCount', 'SignToolRetryDelay', 'SignToolRunMinimized',
    'SlicesPerDisk', 'SourceDir', 'TerminalServicesAware',
    'UsedUserAreasWarning', 'UseSetupLdr', 'VersionInfoCompany',
    'VersionInfoCopyright', 'VersionInfoDescription',
    'VersionInfoOriginalFileName', 'VersionInfoProductName',
    'VersionInfoProductTextVersion', 'VersionInfoProductVersion',
    'VersionInfoTextVersion', 'VersionInfoVersion'],
    SetupSection);

  CD('Compression', ['Compression', 'CompressionThreads',
    'InternalCompressLevel', 'LZMAAlgorithm', 'LZMABlockSize',
    'LZMADictionarySize', 'LZMAMatchFinder', 'LZMANumBlockThreads',
    'LZMANumFastBytes', 'LZMAUseSeparateProcess', 'SolidCompression'],
    SetupSection);

  CD('Installer', ['AllowCancelDuringInstall', 'AllowNetworkDrive',
    'AllowNoIcons', 'AllowRootDirectory', 'AllowUNCPath', 'AlwaysRestart',
    'AlwaysShowComponentsList', 'AlwaysShowDirOnReadyPage',
    'AlwaysShowGroupOnReadyPage', 'AlwaysUsePersonalGroup',
    'AppendDefaultDirName', 'AppendDefaultGroupName', 'AppComments',
    'AppContact', 'AppId', 'AppModifyPath', 'AppMutex', 'AppName',
    'AppPublisher', 'AppPublisherURL', 'AppReadmeFile', 'AppSupportPhone',
    'AppSupportURL', 'AppUpdatesURL', 'AppVerName', 'AppVersion',
    'ArchitecturesAllowed', 'ArchitecturesInstallIn64BitMode',
    'ArchiveExtraction', 'ChangesAssociations', 'ChangesEnvironment',
    'CloseApplications', 'CloseApplicationsFilter',
    'CloseApplicationsFilterExcludes', 'CreateAppDir', 'CreateUninstallRegKey',
    'DefaultDialogFontName', 'DefaultDirName', 'DefaultGroupName',
    'DefaultUserInfoName', 'DefaultUserInfoOrg', 'DefaultUserInfoSerial',
    'DirExistsWarning', 'DisableDirPage', 'DisableFinishedPage',
    'DisableProgramGroupPage', 'DisableReadyMemo', 'DisableReadyPage',
    'DisableStartupPrompt', 'DisableWelcomePage', 'EnableDirDoesntExistWarning',
    'ExtraDiskSpaceRequired', 'InfoAfterFile', 'InfoBeforeFile',
    'LanguageDetectionMethod', 'LicenseFile', 'MinVersion', 'OnlyBelowVersion',
    'Password', 'PrivilegesRequired', 'PrivilegesRequiredOverridesAllowed',
    'RedirectionGuard', 'RestartApplications', 'RestartIfNeededByRun',
    'SetupArchitecture', 'SetupLogging', 'SetupMutex', 'ShowLanguageDialog',
    'TimeStampRounding', 'TimeStampsInUTC', 'TouchDate', 'TouchTime',
    'Uninstallable', 'UninstallDisplayIcon', 'UninstallDisplayName',
    'UninstallDisplaySize', 'UninstallFilesDir', 'UninstallLogging',
    'UninstallLogMode', 'UninstallRestartComputer', 'UpdateUninstallLogAppName',
    'UsePreviousAppDir', 'UsePreviousGroup', 'UsePreviousLanguage',
    'UsePreviousPrivileges', 'UsePreviousSetupType', 'UsePreviousTasks',
    'UsePreviousUserInfo', 'UserInfoPage'],
    SetupSection);

  CD('Cosmetic', ['AppCopyright', 'FlatComponentsList', 'SetupIconFile',
    'ShowComponentSizes', 'ShowTasksTreeLines', 'WizardBackColor',
    'WizardBackColorDynamicDark', 'WizardBackImageFile',
    'WizardBackImageFileDynamicDark', 'WizardBackImageOpacity',
    'WizardImageAlphaFormat', 'WizardImageBackColor',
    'WizardImageBackColorDynamicDark', 'WizardImageFile',
    'WizardImageFileDynamicDark', 'WizardImageOpacity', 'WizardImageStretch',
    'WizardKeepAspectRatio', 'WizardSizePercent', 'WizardSmallImageBackColor',
    'WizardSmallImageBackColorDynamicDark', 'WizardSmallImageFile',
    'WizardSmallImageFileDynamicDark', 'WizardStyle', 'WizardStyleFile',
    'WizardStyleFileDynamicDark'],
    SetupSection);

  const CommonSections: TArray<String> = ['Components', 'Dirs', 'Files',
    'Icons', 'INI', 'InstallDelete', 'ISSigKeys', 'Languages', 'Registry',
    'Run', 'Tasks', 'Types', 'UninstallDelete', 'UninstallRun'];

  CD('Common', ['Check', 'Components', 'Tasks', 'Languages', 'MinVersion',
    'OnlyBelowVersion', 'BeforeInstall', 'AfterInstall'],
    CommonSections);
end;

procedure InitializeSectionMetadata;

  function PD(const AName: String; const AValueKind: TScriptParameterValueKind;
    const AKnownValues: TArray<String> = nil;
    const AObsolete: Boolean = False;
    const ADefaultValue: String = ''): TScriptParameterDefinition;
  begin
    Result.Name := AName;
    Result.ValueKind := AValueKind;
    Result.KnownValues := AKnownValues;
    Result.Obsolete := AObsolete;
    Result.DefaultValue := ADefaultValue;
  end;

  function FIR(const AParameterName, AFlagName: String;
    const AAlsoIncludedFlagNames: TArray<String>): TScriptFlagIncludesRule;
  begin
    Result.ParameterName := AParameterName;
    Result.FlagName := AFlagName;
    Result.AlsoIncludedFlagNames := AAlsoIncludedFlagNames;
  end;

  function PIF(const AParameterName, AFlagParameterName,
    AFlagName: String): TScriptParameterIncludesFlagRule;
  begin
    Result.ParameterName := AParameterName;
    Result.FlagParameterName := AFlagParameterName;
    Result.FlagName := AFlagName;
  end;

  function SetupSectionDirectiveDefaultValue(
    const SetupSectionDirective: TSetupSectionDirective): String;
  const
    SetupSectionDirectivesDefaultYes = [
      ssAllowCancelDuringInstall, ssAllowNetworkDrive, ssAllowUNCPath,
      ssAlwaysShowComponentsList, ssAppendDefaultDirName, ssAppendDefaultGroupName,
      ssASLRCompatible, ssCloseApplications, ssCreateAppDir, ssCreateUninstallRegKey,
      ssDEPCompatible, ssDisableStartupPrompt, ssDisableWelcomePage,
      ssFlatComponentsList, ssMergeDuplicateFiles, ssMissingMessagesWarning,
      ssMissingRunOnceIdsWarning, ssNotRecognizedMessagesWarning, ssOutput,
      ssRedirectionGuard, ssRestartApplications, ssRestartIfNeededByRun,
      ssShowComponentSizes, ssShowLanguageDialog, ssSignedUninstaller,
      ssTerminalServicesAware, ssTimeStampsInUTC, ssUninstallable,
      ssUpdateUninstallLogAppName, ssUsedUserAreasWarning, ssUsePreviousAppDir,
      ssUsePreviousGroup, ssUsePreviousLanguage, ssUsePreviousPrivileges,
      ssUsePreviousSetupType, ssUsePreviousTasks, ssUsePreviousUserInfo,
      ssWizardImageStretch, ssWizardKeepAspectRatio];

    SetupSectionDirectivesDefaultNo = [
      ssAllowNoIcons, ssAllowRootDirectory, ssAlwaysRestart,
      ssAlwaysShowDirOnReadyPage, ssAlwaysShowGroupOnReadyPage,
      ssAlwaysUsePersonalGroup, ssChangesAssociations, ssChangesEnvironment,
      ssDisableFinishedPage, ssDisableReadyMemo, ssDisableReadyPage, ssDiskSpanning,
      ssDontMergeDuplicateFiles, ssEnableDirDoesntExistWarning, ssEncryption,
      ssLZMAUseSeparateProcess, ssSetupLogging, ssShowTasksTreeLines,
      ssSignToolRunMinimized, ssSolidCompression, ssUninstallLogging,
      ssUninstallRestartComputer, ssUserInfoPage];

    SetupSectionDirectivesDefaultAuto = [
      ssArchiveExtraction, ssCompressionThreads, ssDirExistsWarning,
      ssDisableDirPage, ssDisableProgramGroupPage];
  begin
    if SetupSectionDirective in SetupSectionDirectivesDefaultYes then
      Exit(SYes);
    if SetupSectionDirective in SetupSectionDirectivesDefaultNo then
      Exit(SNo);
    if SetupSectionDirective in SetupSectionDirectivesDefaultAuto then
      Exit(SAuto);
    case SetupSectionDirective of
      { directives with a fixed value-list default }
      ssCompression: Result := 'lzma2/max';
      ssInternalCompressLevel: Result := 'normal';
      ssEncryptionKeyDerivation: Result := 'pbkdf2/220000';
      ssPrivilegesRequired: Result := 'admin';
      ssUninstallLogMode: Result := 'append';
      ssLanguageDetectionMethod: Result := 'uilanguage';
      ssSetupArchitecture: Result := 'x86';
      ssWizardStyle: Result := 'classic';
      ssWizardImageAlphaFormat: Result := 'none';
      { directives with a fixed text or numeric default }
      ssOutputBaseFilename: Result := 'mysetup';
      ssOutputDir: Result := 'Output';
      ssDefaultGroupName: Result := '(Default)';
      ssUninstallFilesDir: Result := '{app}';
      ssDefaultUserInfoName: Result := '{sysuserinfoname}';
      ssDefaultUserInfoOrg: Result := '{sysuserinfoorg}';
      ssDefaultDialogFontName: Result := 'Segoe UI';
      ssCloseApplicationsFilter: Result := '*.exe,*.dll,*.chm';
      ssExtraDiskSpaceRequired, ssReserveBytes, ssSignToolMinimumTimeBetween,
        ssOnlyBelowVersion: Result := '0';
      ssDiskClusterSize: Result := '512';
      ssSlicesPerDisk, ssLZMANumBlockThreads: Result := '1';
      ssDiskSliceSize: Result := '2100000000';
      ssTimeStampRounding: Result := '2';
      ssTouchDate, ssTouchTime: Result := 'current';
      ssSignToolRetryCount: Result := '2';
      ssSignToolRetryDelay: Result := '500';
      ssWizardImageOpacity, ssWizardBackImageOpacity: Result := '255';
      ssWizardSizePercent: Result := '120,120';
      ssVersionInfoVersion: Result := '0.0.0.0';
      ssMinVersion: Result := '6.1sp1';
    else
      Result := '';
    end;
  end;

  procedure AddSetupSectionMetadata;
  begin
    var Definitions: TArray<TScriptParameterDefinition>;
    SetLength(Definitions, Ord(High(TSetupSectionDirective))+1);
    for var Directive := Low(TSetupSectionDirective) to High(TSetupSectionDirective) do begin
      var ValueKind := pvkString;
      var KnownValues: TArray<String> := nil;
      if Directive in SetupSectionDirectivesYesNo then begin
        ValueKind := pvkYesNo;
        KnownValues := [SYes, SNo]; { For AddDirectiveRow's fallback }
      end else if Directive in SetupSectionDirectivesYesNoOrScripted then begin
        ValueKind := pvkChoice; { Also allows free typing }
        KnownValues := [SYes, SNo];
      end else if Directive in SetupSectionDirectivesAutoYesNo then begin
        ValueKind := pvkChoice; { See above }
        KnownValues := [SAuto, SYes, SNo];
      end;
      Definitions[Ord(Directive)] := PD(
        Copy(GetEnumName(TypeInfo(TSetupSectionDirective), Ord(Directive)),
          Length(SetupSectionDirectivePrefix)+1, MaxInt),
        ValueKind, KnownValues, Directive in SetupSectionDirectivesObsolete,
        SetupSectionDirectiveDefaultValue(Directive));
    end;
    SectionMetadataList.Add(TScriptSectionMetadata.Create('Setup', Definitions, nil));
  end;

  function LangOptionsSectionDirectiveDefaultValue(
    const LangOptionsSectionDirective: TLangOptionsSectionDirective): String;
  begin
    case LangOptionsSectionDirective of
      lsRightToLeft: Result := SNo;
      lsLanguageName: Result := 'English';
      lsLanguageID: Result := '$0409';
      lsLanguageCodePage: Result := '0';
      lsDialogFontSize: Result := '9';
      lsDialogFontBaseScaleWidth: Result := '7';
      lsDialogFontBaseScaleHeight: Result := '15';
      lsWelcomeFontName: Result := 'Segoe UI';
      lsWelcomeFontSize: Result := '14';
    else
      Result := ''; { lsDialogFontName has no default, same for all obsolete }
    end;
  end;

  procedure AddLangOptionsSectionMetadata;
  const
    LangOptionsSectionDirectivesYesNo = [lsRightToLeft];
  begin
    var Definitions: TArray<TScriptParameterDefinition>;
    SetLength(Definitions, Ord(High(TLangOptionsSectionDirective))+1);
    for var Directive := Low(TLangOptionsSectionDirective) to High(TLangOptionsSectionDirective) do begin
      var ValueKind := pvkString;
      var KnownValues: TArray<String> := nil;
      if Directive in LangOptionsSectionDirectivesYesNo then begin
        ValueKind := pvkYesNo;
        KnownValues := [SYes, SNo]; { See AddSetupSectionMetadata }
      end;
      Definitions[Ord(Directive)] := PD(
        Copy(GetEnumName(TypeInfo(TLangOptionsSectionDirective), Ord(Directive)),
          LangOptionsSectionDirectivePrefixLength+1, MaxInt),
        ValueKind, KnownValues, Directive in LangOptionsSectionDirectivesObsolete,
        LangOptionsSectionDirectiveDefaultValue(Directive));
    end;
    SectionMetadataList.Add(TScriptSectionMetadata.Create('LangOptions', Definitions, nil));
  end;

begin
  const AttribsFlagNames: TArray<String> = ['readonly', 'hidden', 'system',
    'notcontentindexed'];

  const FilesFlagNames: TArray<String> = [
    '32bit', '64bit', 'allowunsafefiles', 'comparetimestamp', 'confirmoverwrite',
    'createallsubdirs', 'deleteafterinstall', 'dontcopy', 'dontverifychecksum', 'download',
    'external', 'extractarchive', 'fontisnttruetype', 'gacinstall', 'ignoreversion',
    'isreadme', 'issigverify', 'nocompression', 'noencryption', 'noregerror', 'notimestamp',
    'onlyifdestfileexists', 'onlyifdoesntexist', 'overwritereadonly', 'promptifolder',
    'recursesubdirs', 'regserver', 'regtypelib', 'replacesameversion', 'restartreplace',
    'setntfscompression', 'sharedfile', 'sign', 'signcheck', 'signonce',
    'skipifsourcedoesntexist', 'solidbreak', 'sortfilesbyextension',
    'sortfilesbyname', 'touch', 'uninsneveruninstall', 'uninsnosharedfileprompt',
    'uninsremovereadonly', 'uninsrestartdelete', 'unsetntfscompression'];

  SectionMetadataList.Add(TScriptSectionMetadata.Create('Files',
    [PD('AfterInstall', pvkString),
    PD('Attribs', pvkFlags, AttribsFlagNames),
    PD('BeforeInstall', pvkString),
    PD('Check', pvkString),
    PD('Components', pvkString),
    PD('CopyMode', pvkString, nil, True),
    { Order must match IDE.Wizard.WizardFileForm! }
    PD('DestDir', pvkChoice, ['{app}', '{autopf}', '{autocf}', '{win}', '{sys}', '{src}', '{sd}']),
    PD('DestName', pvkString),
    PD('DownloadISSigSource', pvkString),
    PD('DownloadPassword', pvkString),
    PD('DownloadUserName', pvkString),
    PD('Excludes', pvkString),
    PD('ExternalSize', pvkInteger),
    PD('ExtractArchivePassword', pvkString),
    PD('Flags', pvkFlags, FilesFlagNames),
    PD('FontInstall', pvkString),
    PD('Hash', pvkString),
    PD('ISSigAllowedKeys', pvkString),
    PD('Languages', pvkString),
    PD('MinVersion', pvkVersion),
    PD('OnlyBelowVersion', pvkVersion),
    PD('Permissions', pvkString),
    PD('Source', pvkString),
    PD('StrongAssemblyName', pvkString),
    PD('Tasks', pvkString)],
    [FIR('Flags', 'extractarchive', ['external', 'ignoreversion']),
    FIR('Flags', 'download', ['external', 'ignoreversion']),
    FIR('Flags', 'createallsubdirs', ['recursesubdirs']),
    FIR('Flags', 'dontverifychecksum', ['nocompression']),
    FIR('Flags', 'uninsnosharedfileprompt', ['sharedfile'])],
    [PIF('ExternalSize', 'Flags', 'external'),
    PIF('ISSigAllowedKeys', 'Flags', 'issigverify')]));

  AddSetupSectionMetadata;
  AddLangOptionsSectionMetadata;
end;

initialization
  SectionMetadataList := TObjectList<TScriptSectionMetadata>.Create;
  InitializeSectionMetadata;
  ScriptCategoryDictionary := TDictionary<String, String>.Create(TIStringComparer.Ordinal);
  InitializeScriptCategories;
finalization
  ScriptCategoryDictionary.Free;
  SectionMetadataList.Free;
end.
