unit IDE.ScriptModel.Metadata;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Per-section metadata tables for the script model: parameter and directive
  names, value kinds, and flag lists, plus the rules layer registrations.

  Tables for [Setup] and [LangOptions] are generated from enums at startup,
  and are enriched with value kinds, choice values, and default values.
  [Messages] and [CustomMessages] do not have tables.
}

interface

uses
  Generics.Collections,
  Shared.SetupSectionDirectives;

type
  TMemberValueKind = (mvkString, mvkInteger, mvkVersion,
    mvkChoice, mvkFlags, mvkYesNo);

  TMemberDefinition = record
    Name: String;
    ValueKind: TMemberValueKind;
    KnownValues: TArray<String>; { For mvkFlags: the known flags
                                   For mvkChoice: the known choices (other choices might still be valid, like a scripted expression)
                                   For mvkYesNo: 'yes' and 'no' (other values might still be valid, like an ISPP inline directive) }
    DefaultValue: String; { The default of a directive, empty for parameters }
    Obsolete: Boolean;    { To be hidden unless explicitly present in the script }
  end;

  { Rule: including FlagName also includes or excludes OtherFlagNames.
    Includes rules are applied in one direction only. Excludes rules are also
    applied in reverse (including a listed other flag excludes FlagName),
    but the listed other flags do not exclude each other. }
  TFlagRule = record
    ParameterName: String;
    FlagName: String;
    OtherFlagNames: TArray<String>; { the other flags to include or exclude }
  end;

  { Rule: giving a parameter a non-empty value must also include a flag }
  TParameterIncludesFlagRule = record
    ParameterName: String;      { the parameter whose value triggers the rule }
    FlagParameterName: String;  { the flag-list parameter that holds the flag }
    FlagName: String;           { the flag to include }
  end;

  TScriptModelSectionMetadata = class
  private
    FSectionName: String;
    FMembers: TArray<TMemberDefinition>;
    FMembersByName: TDictionary<String, Integer>; { FMembers index by name }
    FFlagIncludesRules: TArray<TFlagRule>;
    FFlagExcludesRules: TArray<TFlagRule>;
    FParameterIncludesFlagRules: TArray<TParameterIncludesFlagRule>;
  public
    constructor Create(const ASectionName: String;
      const AMembers: TArray<TMemberDefinition>;
      const AFlagIncludesRules: TArray<TFlagRule> = nil;
      const AFlagExcludesRules: TArray<TFlagRule> = nil;
      const AParameterIncludesFlagRules: TArray<TParameterIncludesFlagRule> = nil);
    destructor Destroy; override;
    function TryGetMember(const AName: String;
      out ADefinition: TMemberDefinition): Boolean;
    property SectionName: String read FSectionName;
    property Members: TArray<TMemberDefinition> read FMembers;
    property FlagIncludesRules: TArray<TFlagRule> read FFlagIncludesRules;
    property FlagExcludesRules: TArray<TFlagRule> read FFlagExcludesRules;
    property ParameterIncludesFlagRules: TArray<TParameterIncludesFlagRule>
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

  { We don't list true/false/1/0 }
  SYes = 'yes';
  SNo = 'no';
  SAuto = 'auto';

function TryGetScriptModelSectionMetadata(const ASectionName: String;
  out AMetadata: TScriptModelSectionMetadata): Boolean;

function ScriptCategoryNamesOrdered: TArray<String>;

function TryGetScriptCategory(const ASectionName, AName: String;
  out ACategoryName: String): Boolean;

implementation

uses
  SysUtils, TypInfo, Generics.Defaults,
  Shared.LangOptionsSectionDirectives;

var
  SectionMetadataList: TObjectList<TScriptModelSectionMetadata>;

{ TScriptModelSectionMetadata }

constructor TScriptModelSectionMetadata.Create(const ASectionName: String;
  const AMembers: TArray<TMemberDefinition>;
  const AFlagIncludesRules: TArray<TFlagRule>;
  const AFlagExcludesRules: TArray<TFlagRule>;
  const AParameterIncludesFlagRules: TArray<TParameterIncludesFlagRule>);
begin
  inherited Create;
  FSectionName := ASectionName;
  FMembers := AMembers;
  FMembersByName := TDictionary<String, Integer>.Create(TIStringComparer.Ordinal);
  for var I := 0 to High(FMembers) do
    FMembersByName.Add(FMembers[I].Name, Integer(I)); { Add raises on a duplicate name }
  FFlagIncludesRules := AFlagIncludesRules;
  FFlagExcludesRules := AFlagExcludesRules;
  FParameterIncludesFlagRules := AParameterIncludesFlagRules;
end;

destructor TScriptModelSectionMetadata.Destroy;
begin
  FMembersByName.Free;
  inherited;
end;

function TScriptModelSectionMetadata.TryGetMember(const AName: String;
  out ADefinition: TMemberDefinition): Boolean;
begin
  var I: Integer;
  Result := FMembersByName.TryGetValue(AName, I);
  if Result then
    ADefinition := FMembers[I];
end;

function TryGetScriptModelSectionMetadata(const ASectionName: String;
  out AMetadata: TScriptModelSectionMetadata): Boolean;
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

  function MD(const AName: String; const AValueKind: TMemberValueKind;
    const AKnownValues: TArray<String> = nil;
    const AObsolete: Boolean = False;
    const ADefaultValue: String = ''): TMemberDefinition;
  begin
    Result.Name := AName;
    Result.ValueKind := AValueKind;
    Result.KnownValues := AKnownValues;
    Result.Obsolete := AObsolete;
    Result.DefaultValue := ADefaultValue;
  end;

  function FR(const AParameterName, AFlagName: String;
    const AOtherFlagNames: TArray<String>): TFlagRule;
  begin
    Result.ParameterName := AParameterName;
    Result.FlagName := AFlagName;
    Result.OtherFlagNames := AOtherFlagNames;
  end;

  function PIF(const AParameterName, AFlagParameterName,
    AFlagName: String): TParameterIncludesFlagRule;
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

const
  LZMALevels: TArray<String> = ['fast', 'normal', 'max', 'ultra', 'ultra64'];

  function CompressionValues: TArray<String>;
  const
    ZipAlgos: TArray<String> = ['zip', 'bzip'];
    LZMAAlgos: TArray<String> = ['lzma', 'lzma2'];
  begin
    Result := ['none'];
    for var Algo in ZipAlgos do begin
      Result := Result + [Algo];
      for var Level := 1 to 9 do
        Result := Result + [Algo + '/' + Level.ToString];
    end;
    for var Algo in LZMAAlgos do begin
      Result := Result + [Algo];
      for var Level in LZMALevels do
        Result := Result + [Algo + '/' + Level];
    end;
  end;

  function SetupSectionDirectiveFlagValues(
    const SetupSectionDirective: TSetupSectionDirective): TArray<String>;
  { The flag-list directives: their value is a space separated list of flags,
    with no free typing allowed. Expression directives like ArchitecturesAllowed
    are not included here. Intentionally unsorted. }
  begin
    case SetupSectionDirective of
      ssDisablePrecompiledFileVerifications: Result := ['setup',
        'setupcustomstyle', 'setupldr', 'is7z', 'isbunzip', 'isunzlib',
        'islzma'];
      ssPrivilegesRequiredOverridesAllowed: Result := ['commandline', 'dialog'];
      ssWizardStyle: Result := ['classic', 'modern', 'light', 'dark', 'dynamic',
        'excludelightbuttons', 'excludelightcontrols', 'hidebevels',
        'includetitlebar', 'polar', 'slate', 'stellar', 'windows11', 'zircon'];
    else
      Result := nil;
    end;
  end;

  function SetupSectionDirectiveChoiceValues(
    const SetupSectionDirective: TSetupSectionDirective): TArray<String>;
  { Multi-value directives like WizardStyle and expression directives like
    ArchitecturesAllowed are not included here }
  begin
    case SetupSectionDirective of
      ssArchiveExtraction: Result := ['auto', 'basic', 'enhanced/nopassword', 'enhanced', 'full'];
      ssCloseApplications: Result := ['force', SYes, SNo];
      ssCompression: Result := CompressionValues;
      ssEncryption: Result := ['full', SYes, SNo];
      ssInternalCompressLevel: Result := ['none'] + LZMALevels; { We don't list 0 }
      ssLanguageDetectionMethod: Result := ['uilanguage', 'locale', 'none'];
      ssLZMAAlgorithm: Result := ['0', '1'];
      ssLZMAMatchFinder: Result := ['BT', 'HC'];
      ssLZMAUseSeparateProcess: Result := ['x86', SYes, SNo];
      ssPrivilegesRequired: Result := ['admin', 'lowest']; { We don't list none/poweruser }
      ssSetupArchitecture: Result := ['x86', 'x64'];
      ssUninstallLogMode: Result := ['append', 'new', 'overwrite'];
      ssUseSetupLdr: Result := ['x86', 'x64', SYes, SNo];
      ssWizardImageAlphaFormat: Result := ['none', 'defined', 'premultiplied'];
    else
      Result := nil;
    end;
  end;

  procedure AddSetupSectionMetadata;
  const
    { Directives with a plain integer value only. Directives with richer forms
      like DiskSliceSize's 'max' and CompressionThreads' 'auto' are not included
      here }
    SetupSectionDirectivesInteger = [
      ssDiskClusterSize, ssExtraDiskSpaceRequired, ssLZMABlockSize,
      ssLZMADictionarySize, ssLZMANumBlockThreads, ssLZMANumFastBytes,
      ssReserveBytes, ssSignToolMinimumTimeBetween, ssSignToolRetryCount,
      ssSignToolRetryDelay, ssSlicesPerDisk, ssTimeStampRounding,
      ssUninstallDisplaySize, ssWizardBackImageOpacity, ssWizardImageOpacity];
    SetupSectionDirectivesVersion = [ssMinVersion, ssOnlyBelowVersion];
  begin
    var Members: TArray<TMemberDefinition>;
    SetLength(Members, Ord(High(TSetupSectionDirective))+1);
    for var Directive := Low(TSetupSectionDirective) to High(TSetupSectionDirective) do begin
      var ValueKind := mvkString;
      var KnownValues: TArray<String> := nil;
      if Directive in SetupSectionDirectivesYesNo then begin
        ValueKind := mvkYesNo;
        KnownValues := [SYes, SNo]; { For AddDirectiveRow's fallback }
      end else if Directive in SetupSectionDirectivesYesNoOrScripted then begin
        ValueKind := mvkChoice; { Also allows free typing }
        KnownValues := [SYes, SNo];
      end else if Directive in SetupSectionDirectivesAutoYesNo then begin
        ValueKind := mvkChoice; { Also allows free typing }
        KnownValues := [SAuto, SYes, SNo];
      end else if Directive in SetupSectionDirectivesInteger then
        ValueKind := mvkInteger
      else if Directive in SetupSectionDirectivesVersion then
        ValueKind := mvkVersion
      else begin
        KnownValues := SetupSectionDirectiveFlagValues(Directive);
        if KnownValues <> nil then
          ValueKind := mvkFlags
        else begin
          KnownValues := SetupSectionDirectiveChoiceValues(Directive);
          if KnownValues <> nil then
            ValueKind := mvkChoice;
        end;
      end;
      Members[Ord(Directive)] := MD(
        Copy(GetEnumName(TypeInfo(TSetupSectionDirective), Ord(Directive)),
          Length(SetupSectionDirectivePrefix)+1, MaxInt),
        ValueKind, KnownValues, Directive in SetupSectionDirectivesObsolete,
        SetupSectionDirectiveDefaultValue(Directive));
    end;
    SectionMetadataList.Add(TScriptModelSectionMetadata.Create('Setup', Members,
      nil,
      [FR('WizardStyle', 'classic', ['modern']),
       FR('WizardStyle', 'light', ['dark', 'dynamic']),
       FR('WizardStyle', 'dark', ['dynamic']), { Completes the mutually-exclusive theme trio }
       FR('WizardStyle', 'excludelightbuttons', ['excludelightcontrols']),
       FR('WizardStyle', 'polar', ['slate', 'stellar', 'windows11', 'zircon']),
       { Complete the mutually-exclusive style quintet }
       FR('WizardStyle', 'slate', ['stellar', 'windows11', 'zircon']),
       FR('WizardStyle', 'stellar', ['windows11', 'zircon']),
       FR('WizardStyle', 'windows11', ['zircon'])]));
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
    LangOptionsSectionDirectivesInteger = [lsDialogFontBaseScaleHeight,
      lsDialogFontBaseScaleWidth, lsDialogFontSize, lsLanguageCodePage,
      lsLanguageID, lsWelcomeFontSize];
  begin
    var Members: TArray<TMemberDefinition>;
    SetLength(Members, Ord(High(TLangOptionsSectionDirective))+1);
    for var Directive := Low(TLangOptionsSectionDirective) to High(TLangOptionsSectionDirective) do begin
      var ValueKind := mvkString;
      var KnownValues: TArray<String> := nil;
      if Directive in LangOptionsSectionDirectivesYesNo then begin
        ValueKind := mvkYesNo;
        KnownValues := [SYes, SNo]; { For AddDirectiveRow's fallback }
      end else if Directive in LangOptionsSectionDirectivesInteger then
        ValueKind := mvkInteger;
      Members[Ord(Directive)] := MD(
        Copy(GetEnumName(TypeInfo(TLangOptionsSectionDirective), Ord(Directive)),
          LangOptionsSectionDirectivePrefixLength+1, MaxInt),
        ValueKind, KnownValues, Directive in LangOptionsSectionDirectivesObsolete,
        LangOptionsSectionDirectiveDefaultValue(Directive));
    end;
    SectionMetadataList.Add(TScriptModelSectionMetadata.Create('LangOptions', Members));
  end;

begin
  const AttribsFlagNames: TArray<String> = ['hidden', 'notcontentindexed',
    'readonly', 'system'];

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

  SectionMetadataList.Add(TScriptModelSectionMetadata.Create('Files',
    [MD('AfterInstall', mvkString),
    MD('Attribs', mvkFlags, AttribsFlagNames),
    MD('BeforeInstall', mvkString),
    MD('Check', mvkString),
    MD('Components', mvkString),
    MD('CopyMode', mvkString, nil, True),
    { Order must match IDE.Wizard.WizardFileForm! }
    MD('DestDir', mvkChoice, ['{app}', '{autopf}', '{autocf}', '{win}', '{sys}', '{src}', '{sd}']),
    MD('DestName', mvkString),
    MD('DownloadISSigSource', mvkString),
    MD('DownloadPassword', mvkString),
    MD('DownloadUserName', mvkString),
    MD('Excludes', mvkString),
    MD('ExternalSize', mvkInteger),
    MD('ExtractArchivePassword', mvkString),
    MD('Flags', mvkFlags, FilesFlagNames),
    MD('FontInstall', mvkString),
    MD('Hash', mvkString),
    MD('ISSigAllowedKeys', mvkString),
    MD('Languages', mvkString),
    MD('MinVersion', mvkVersion),
    MD('OnlyBelowVersion', mvkVersion),
    MD('Permissions', mvkString),
    MD('Source', mvkString),
    MD('StrongAssemblyName', mvkString),
    MD('Tasks', mvkString)],
    [FR('Flags', 'extractarchive', ['external', 'ignoreversion']),
     FR('Flags', 'download', ['external', 'ignoreversion']),
     FR('Flags', 'createallsubdirs', ['recursesubdirs']),
     FR('Flags', 'dontverifychecksum', ['nocompression']),
     FR('Flags', 'uninsnosharedfileprompt', ['sharedfile'])],
    [FR('Flags', '32bit', ['64bit']),
     FR('Flags', 'deleteafterinstall', ['gacinstall', 'isreadme', 'regserver',
       'regtypelib', 'restartreplace', 'sharedfile', 'uninsneveruninstall']), { These exclude deleteafterinstall, but not each other }
     FR('Flags', 'download', ['comparetimestamp', 'skipifsourcedoesntexist']),
     FR('Flags', 'external', ['sign', 'signcheck', 'signonce']),
     FR('Flags', 'ignoreversion', ['replacesameversion']),
     FR('Flags', 'issigverify', ['sign', 'signonce']),
     FR('Flags', 'notimestamp', ['comparetimestamp', 'touch']),
     FR('Flags', 'setntfscompression', ['unsetntfscompression']),
     FR('Flags', 'sign', ['signcheck', 'signonce']),
     FR('Flags', 'signcheck', ['signonce'])], { Completes the mutually-exclusive sign trio }
    [PIF('DownloadISSigSource', 'Flags', 'download'),
     PIF('DownloadISSigSource', 'Flags', 'issigverify'),
     PIF('DownloadPassword', 'Flags', 'download'),
     PIF('DownloadUserName', 'Flags', 'download'),
     PIF('ExternalSize', 'Flags', 'external'),
     PIF('ExtractArchivePassword', 'Flags', 'extractarchive'),
     PIF('ISSigAllowedKeys', 'Flags', 'issigverify'),
     PIF('StrongAssemblyName', 'Flags', 'gacinstall')]));

  SectionMetadataList.Add(TScriptModelSectionMetadata.Create('Components',
    [MD('Check', mvkString),
    MD('Description', mvkString),
    MD('ExtraDiskSpaceRequired', mvkInteger),
    MD('Flags', mvkFlags, ['checkablealone', 'disablenouninstallwarning',
       'dontinheritcheck', 'exclusive', 'fixed', 'restart']),
    MD('Languages', mvkString),
    MD('MinVersion', mvkVersion),
    MD('Name', mvkString),
    MD('OnlyBelowVersion', mvkVersion),
    MD('Types', mvkString)],
    nil,
    [FR('Flags', 'dontinheritcheck', ['exclusive'])]));

  SectionMetadataList.Add(TScriptModelSectionMetadata.Create('Dirs',
    [MD('AfterInstall', mvkString),
    MD('Attribs', mvkFlags, AttribsFlagNames),
    MD('BeforeInstall', mvkString),
    MD('Check', mvkString),
    MD('Components', mvkString),
    MD('Flags', mvkFlags, ['deleteafterinstall', 'setntfscompression',
       'uninsalwaysuninstall', 'uninsneveruninstall', 'unsetntfscompression']),
    MD('Languages', mvkString),
    MD('MinVersion', mvkVersion),
    MD('Name', mvkString),
    MD('OnlyBelowVersion', mvkVersion),
    MD('Permissions', mvkString),
    MD('Tasks', mvkString)],
    nil,
    [FR('Flags', 'setntfscompression', ['unsetntfscompression']),
     FR('Flags', 'uninsalwaysuninstall', ['uninsneveruninstall'])]));

  SectionMetadataList.Add(TScriptModelSectionMetadata.Create('Icons',
    [MD('AfterInstall', mvkString),
    MD('AppUserModelID', mvkString),
    MD('AppUserModelToastActivatorCLSID', mvkString),
    MD('BeforeInstall', mvkString),
    MD('Check', mvkString),
    MD('Comment', mvkString),
    MD('Components', mvkString),
    MD('Filename', mvkString),
    MD('Flags', mvkFlags, ['closeonexit', 'createonlyiffileexists',
       'dontcloseonexit', 'excludefromshowinnewinstall', 'preventpinning',
       'runmaximized', 'runminimized', 'uninsneveruninstall', 'useapppaths']),
    MD('HotKey', mvkString),
    MD('IconFilename', mvkString),
    MD('IconIndex', mvkInteger),
    MD('Languages', mvkString),
    MD('MinVersion', mvkVersion),
    MD('Name', mvkString),
    MD('OnlyBelowVersion', mvkVersion),
    MD('Parameters', mvkString),
    MD('Tasks', mvkString),
    MD('WorkingDir', mvkString)]));

  SectionMetadataList.Add(TScriptModelSectionMetadata.Create('INI',
    [MD('AfterInstall', mvkString),
    MD('BeforeInstall', mvkString),
    MD('Check', mvkString),
    MD('Components', mvkString),
    MD('Filename', mvkString),
    MD('Flags', mvkFlags, ['createkeyifdoesntexist', 'uninsdeleteentry',
       'uninsdeletesection', 'uninsdeletesectionifempty']),
    MD('Key', mvkString),
    MD('Languages', mvkString),
    MD('MinVersion', mvkVersion),
    MD('OnlyBelowVersion', mvkVersion),
    MD('Section', mvkString),
    MD('String', mvkString),
    MD('Tasks', mvkString)],
    nil,
    [FR('Flags', 'uninsdeletesection', ['uninsdeleteentry',
       'uninsdeletesectionifempty'])]));

  const DeleteSectionParameters: TArray<TMemberDefinition> = [
    MD('AfterInstall', mvkString),
    MD('BeforeInstall', mvkString),
    MD('Check', mvkString),
    MD('Components', mvkString),
    MD('Languages', mvkString),
    MD('MinVersion', mvkVersion),
    MD('Name', mvkString),
    MD('OnlyBelowVersion', mvkVersion),
    MD('Tasks', mvkString),
    MD('Type', mvkChoice, ['files', 'filesandordirs', 'dirifempty'])];
  SectionMetadataList.Add(TScriptModelSectionMetadata.Create('InstallDelete',
    DeleteSectionParameters));
  SectionMetadataList.Add(TScriptModelSectionMetadata.Create('UninstallDelete',
    DeleteSectionParameters));

  SectionMetadataList.Add(TScriptModelSectionMetadata.Create('ISSigKeys',
    [MD('Group', mvkString),
    MD('KeyFile', mvkString),
    MD('KeyID', mvkString),
    MD('Name', mvkString),
    MD('PublicX', mvkString),
    MD('PublicY', mvkString),
    MD('RuntimeID', mvkString)]));

  SectionMetadataList.Add(TScriptModelSectionMetadata.Create('Languages',
    [MD('InfoAfterFile', mvkString),
    MD('InfoBeforeFile', mvkString),
    MD('LicenseFile', mvkString),
    MD('MessagesFile', mvkString),
    MD('Name', mvkString)]));

  SectionMetadataList.Add(TScriptModelSectionMetadata.Create('Registry',
    [MD('AfterInstall', mvkString),
    MD('BeforeInstall', mvkString),
    MD('Check', mvkString),
    MD('Components', mvkString),
    MD('Flags', mvkFlags, ['createvalueifdoesntexist', 'deletekey',
       'deletevalue', 'dontcreatekey', 'noerror', 'preservestringtype',
       'uninsclearvalue', 'uninsdeletekey', 'uninsdeletekeyifempty',
       'uninsdeletevalue']),
    MD('Languages', mvkString),
    MD('MinVersion', mvkVersion),
    MD('OnlyBelowVersion', mvkVersion),
    MD('Permissions', mvkString),
    MD('Root', mvkChoice, ['HKA', 'HKA32', 'HKA64', 'HKCC', 'HKCC32',
       'HKCC64', 'HKCR', 'HKCR32', 'HKCR64', 'HKCU', 'HKCU32', 'HKCU64',
       'HKLM', 'HKLM32', 'HKLM64', 'HKU', 'HKU32', 'HKU64']),
    MD('Subkey', mvkString),
    MD('Tasks', mvkString),
    MD('ValueData', mvkString),
    MD('ValueName', mvkString),
    MD('ValueType', mvkChoice, ['none', 'string', 'expandsz', 'multisz',
       'dword', 'qword', 'binary'])],
    nil,
    [FR('Flags', 'uninsdeletekey', ['uninsclearvalue', 'uninsdeletekeyifempty',
       'uninsdeletevalue']),
     FR('Flags', 'uninsclearvalue', ['uninsdeletevalue'])]));

  SectionMetadataList.Add(TScriptModelSectionMetadata.Create('Run',
    [MD('AfterInstall', mvkString),
    MD('BeforeInstall', mvkString),
    MD('Check', mvkString),
    MD('Components', mvkString),
    MD('Description', mvkString),
    MD('Filename', mvkString),
    MD('Flags', mvkFlags, ['32bit', '64bit', 'dontlogparameters', 'hidewizard',
       'logoutput', 'nowait', 'postinstall', 'runascurrentuser',
       'runasoriginaluser', 'runhidden', 'runmaximized', 'runminimized',
       'shellexec', 'skipifdoesntexist', 'skipifnotsilent', 'skipifsilent',
       'unchecked', 'waituntilidle', 'waituntilterminated']),
    MD('Languages', mvkString),
    MD('MinVersion', mvkVersion),
    MD('OnLog', mvkString),
    MD('OnlyBelowVersion', mvkVersion),
    MD('Parameters', mvkString),
    MD('StatusMsg', mvkString),
    MD('Tasks', mvkString),
    MD('Verb', mvkString),
    MD('WorkingDir', mvkString)],
    [FR('Flags', 'unchecked', ['postinstall'])],
    [FR('Flags', '32bit', ['64bit']),
     FR('Flags', 'logoutput', ['nowait', 'runasoriginaluser', 'shellexec',
       'waituntilidle']),
     FR('Flags', 'nowait', ['waituntilidle', 'waituntilterminated']),
     FR('Flags', 'runascurrentuser', ['runasoriginaluser']),
     FR('Flags', 'shellexec', ['32bit', '64bit']),
     FR('Flags', 'waituntilidle', ['waituntilterminated'])],
    [PIF('Verb', 'Flags', 'shellexec'),
     PIF('OnLog', 'Flags', 'logoutput')]));

  SectionMetadataList.Add(TScriptModelSectionMetadata.Create('Tasks',
    [MD('Check', mvkString),
    MD('Components', mvkString),
    MD('Description', mvkString),
    MD('Flags', mvkFlags, ['checkablealone', 'checkedonce', 'dontinheritcheck',
       'exclusive', 'restart', 'unchecked']),
    MD('GroupDescription', mvkString),
    MD('Languages', mvkString),
    MD('MinVersion', mvkVersion),
    MD('Name', mvkString),
    MD('OnlyBelowVersion', mvkVersion)],
    nil,
    [FR('Flags', 'dontinheritcheck', ['exclusive'])]));

  SectionMetadataList.Add(TScriptModelSectionMetadata.Create('Types',
    [MD('Check', mvkString),
    MD('Description', mvkString),
    MD('Flags', mvkFlags, ['iscustom']),
    MD('Languages', mvkString),
    MD('MinVersion', mvkVersion),
    MD('Name', mvkString),
    MD('OnlyBelowVersion', mvkVersion)]));

  SectionMetadataList.Add(TScriptModelSectionMetadata.Create('UninstallRun',
    [MD('AfterInstall', mvkString),
    MD('BeforeInstall', mvkString),
    MD('Check', mvkString),
    MD('Components', mvkString),
    MD('Filename', mvkString),
    MD('Flags', mvkFlags, ['32bit', '64bit', 'dontlogparameters', 'hidewizard',
       'logoutput', 'nowait', 'runascurrentuser', 'runhidden', 'runmaximized',
       'runminimized', 'shellexec', 'skipifdoesntexist', 'waituntilidle',
       'waituntilterminated']),
    MD('Languages', mvkString),
    MD('MinVersion', mvkVersion),
    MD('OnlyBelowVersion', mvkVersion),
    MD('Parameters', mvkString),
    MD('RunOnceId', mvkString),
    MD('StatusMsg', mvkString, nil, True),
    MD('Tasks', mvkString),
    MD('Verb', mvkString),
    MD('WorkingDir', mvkString)],
    nil,
    [FR('Flags', '32bit', ['64bit']),
     FR('Flags', 'logoutput', ['nowait', 'shellexec', 'waituntilidle']),
     FR('Flags', 'nowait', ['waituntilidle', 'waituntilterminated']),
     FR('Flags', 'shellexec', ['32bit', '64bit']),
     FR('Flags', 'waituntilidle', ['waituntilterminated'])],
    [PIF('Verb', 'Flags', 'shellexec')]));

  AddSetupSectionMetadata;
  AddLangOptionsSectionMetadata;
end;

initialization
  SectionMetadataList := TObjectList<TScriptModelSectionMetadata>.Create;
  InitializeSectionMetadata;
  ScriptCategoryDictionary := TDictionary<String, String>.Create(TIStringComparer.Ordinal);
  InitializeScriptCategories;
finalization
  ScriptCategoryDictionary.Free;
  SectionMetadataList.Free;
end.
