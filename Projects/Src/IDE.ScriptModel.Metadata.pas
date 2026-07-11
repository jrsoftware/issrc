unit IDE.ScriptModel.Metadata;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Per-section metadata tables for the script model: parameter names, value
  kinds, and flag lists, plus the rules layer registrations.
}

interface

type
  TScriptParameterValueKind = (pvkString, pvkInteger, pvkVersion,
    pvkChoice, pvkFlags);

  TScriptParameterDefinition = record
    Name: String;
    ValueKind: TScriptParameterValueKind;
    KnownValues: TArray<String>; { For pvkFlags: the known tokens; for pvkChoice: the known single values }
    Obsolete: Boolean;      { Documented as obsolete: to be hidden unless explicitly present in the script }
  end;

  { Rule: including a flag in a delimited parameter must also include other flags,
    for example checking extractarchive on a [Files] entry also checks
    external and ignoreversion }
  TScriptFlagIncludesRule = record
    ParameterName: String;
    FlagName: String;
    AlsoIncludedFlagNames: TArray<String>; { the other flags to include }
  end;

  { Rule: giving a parameter a non-empty value must also include a flag, for example
    setting Verb on a [Run] entry also checks the shellexec flag }
  TScriptParameterIncludesFlagRule = record
    ParameterName: String;      { the parameter whose value triggers the rule }
    FlagParameterName: String;  { the flag-list parameter that holds the flag }
    FlagName: String;           { the flag to include }
  end;

  TScriptSectionMetadata = class
  private
    FSectionName: String;
    FParameters: TArray<TScriptParameterDefinition>;
    FFlagIncludesRules: TArray<TScriptFlagIncludesRule>;
    FParameterIncludesFlagRules: TArray<TScriptParameterIncludesFlagRule>;
  public
    constructor Create(const ASectionName: String;
      const AParameters: TArray<TScriptParameterDefinition>;
      const AFlagIncludesRules: TArray<TScriptFlagIncludesRule>;
      const AParameterIncludesFlagRules: TArray<TScriptParameterIncludesFlagRule> = nil);
    function TryGetParameter(const AName: String;
      out ADefinition: TScriptParameterDefinition): Boolean;
    property SectionName: String read FSectionName;
    property Parameters: TArray<TScriptParameterDefinition> read FParameters;
    property FlagIncludesRules: TArray<TScriptFlagIncludesRule> read FFlagIncludesRules;
    property ParameterIncludesFlagRules: TArray<TScriptParameterIncludesFlagRule>
      read FParameterIncludesFlagRules;
  end;

function TryGetScriptSectionMetadata(const ASectionName: String;
  out AMetadata: TScriptSectionMetadata): Boolean;

{ The inspector shows a named group of parameters or directives as a
  collapsible category. Membership is by parameter or directive name within the
  category's own sections: Common spans the parameter sections, while the
  Compiler, Installer, Compression and Cosmetic groups apply to [Setup] only.
  Growing or adding a category is a table edit in InitializeScriptCategories }

{ The category names in the order they should be shown }
function ScriptCategoryNames: TArray<String>;
{ The category a parameter or directive name belongs to in the given section,
  if any }
function TryGetScriptCategory(const ASectionName, AName: String;
  out ACategoryName: String): Boolean;

implementation

uses
  SysUtils, Generics.Collections, Generics.Defaults;

var
  SectionMetadataList: TObjectList<TScriptSectionMetadata>;
  { The category names, in the order they should be shown }
  ScriptCategoryNameOrderedList: TArray<String>;
  { Category name per member, keyed by section and member name: the inspector
    looks a category up for every row it builds, on every caret move }
  ScriptCategoryDictionary: TDictionary<String, String>;

{ TScriptSectionMetadata }

constructor TScriptSectionMetadata.Create(const ASectionName: String;
  const AParameters: TArray<TScriptParameterDefinition>;
  const AFlagIncludesRules: TArray<TScriptFlagIncludesRule>;
  const AParameterIncludesFlagRules: TArray<TScriptParameterIncludesFlagRule>);
begin
  inherited Create;
  FSectionName := ASectionName;
  FParameters := AParameters;
  FFlagIncludesRules := AFlagIncludesRules;
  FParameterIncludesFlagRules := AParameterIncludesFlagRules;
end;

function TScriptSectionMetadata.TryGetParameter(const AName: String;
  out ADefinition: TScriptParameterDefinition): Boolean;
begin
  for var Parameter in FParameters do begin
    if SameText(Parameter.Name, AName) then begin
      ADefinition := Parameter;
      Exit(True);
    end;
  end;
  Result := False;
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

{ Key into ScriptCategoryDictionary, which is case-insensitive like the other
  metadata lookups. Section, parameter and directive names are identifiers, so a
  dot cannot occur in either half and the key is unambiguous }
function ScriptCategoryKey(const ASectionName, AName: String): String;
begin
  Result := ASectionName + '.' + AName;
end;

function ScriptCategoryNames: TArray<String>;
begin
  Result := Copy(ScriptCategoryNameOrderedList);
end;

function TryGetScriptCategory(const ASectionName, AName: String;
  out ACategoryName: String): Boolean;
begin
  Result := ScriptCategoryDictionary.TryGetValue(
    ScriptCategoryKey(ASectionName, AName), ACategoryName);
end;

procedure InitializeScriptCategories;

  procedure CD(const AName: String; const AMemberNames: TArray<String>;
    const ASectionNames: TArray<String>);
  begin
    if Length(ASectionNames) = 0 then
      raise Exception.CreateFmt('Category %s has no section names', [AName]);
    ScriptCategoryNameOrderedList := ScriptCategoryNameOrderedList + [AName];
    for var SectionName in ASectionNames do begin
      for var MemberName in AMemberNames do begin
        { Add raises on a duplicate key, so a name cannot end up in two categories
          for the same section }
        ScriptCategoryDictionary.Add(ScriptCategoryKey(SectionName, MemberName),
          AName);
      end;
    end;
  end;

begin
  { Common groups the parameters shared across the parameter sections so the
    section-specific parameters stand out; it spans every parameter section but
    not [Setup], whose directives are all divided over Compiler, Installer,
    Compression and Cosmetic the way isetup.xml does, including the MinVersion
    and OnlyBelowVersion directives, which are unrelated to the parameters of
    the same name. Those four are scoped to [Setup] so a directive name that is
    also a parameter name elsewhere (such as ExtraDiskSpaceRequired in
    [Components] or LicenseFile in [Languages]) does not group under them there }
  const SetupOnly: TArray<String> = ['Setup'];
  const CommonSections: TArray<String> = ['Components', 'Dirs', 'Files',
    'Icons', 'INI', 'InstallDelete', 'ISSigKeys', 'Languages', 'Registry',
    'Run', 'Tasks', 'Types', 'UninstallDelete', 'UninstallRun'];
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
    'VersionInfoTextVersion', 'VersionInfoVersion'], SetupOnly);
  CD('Compression', ['Compression', 'CompressionThreads',
    'InternalCompressLevel', 'LZMAAlgorithm', 'LZMABlockSize',
    'LZMADictionarySize', 'LZMAMatchFinder', 'LZMANumBlockThreads',
    'LZMANumFastBytes', 'LZMAUseSeparateProcess', 'SolidCompression'],
    SetupOnly);
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
    'UsePreviousUserInfo', 'UserInfoPage'], SetupOnly);
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
    'WizardStyleFileDynamicDark'], SetupOnly);
  CD('Common', ['Check', 'Components', 'Tasks', 'Languages', 'MinVersion',
    'OnlyBelowVersion', 'BeforeInstall', 'AfterInstall'], CommonSections);
end;

procedure InitializeSectionMetadata;

  function PD(const AName: String; const AValueKind: TScriptParameterValueKind;
    const AKnownValues: TArray<String> = nil;
    const AObsolete: Boolean = False): TScriptParameterDefinition;
  begin
    Result.Name := AName;
    Result.ValueKind := AValueKind;
    Result.KnownValues := AKnownValues;
    Result.Obsolete := AObsolete;
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

begin
  { Parameter names match TInnoSetupStyler's per-section lists, value kinds
    follow the documentation in ISHelp\isetup.xml }

  const AttribsFlagNames: TArray<String> = ['readonly', 'hidden', 'system',
    'notcontentindexed'];

  const FilesFlagNames: TArray<String> = [
    '32bit', '64bit', 'allowunsafefiles', 'comparetimestamp', 'confirmoverwrite',
    'createallsubdirs', 'deleteafterinstall', 'dontcopy', 'dontverifychecksum', 'download',
    'external', 'extractarchive', 'fontisnttruetype', 'gacinstall', 'ignoreversion',
    'isreadme', 'issigverify', 'nocompression', 'noencryption', 'notimestamp', 'noregerror',
    'onlyifdestfileexists', 'onlyifdoesntexist', 'overwritereadonly', 'promptifolder',
    'recursesubdirs', 'regserver', 'regtypelib', 'replacesameversion', 'restartreplace',
    'setntfscompression', 'sharedfile', 'sign', 'signcheck', 'signonce',
    'skipifsourcedoesntexist', 'solidbreak', 'sortfilesbyextension',
    'sortfilesbyname', 'touch', 'uninsnosharedfileprompt', 'uninsremovereadonly',
    'uninsrestartdelete', 'uninsneveruninstall', 'unsetntfscompression'];

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
