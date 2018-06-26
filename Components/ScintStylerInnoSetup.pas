unit ScintStylerInnoSetup;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TInnoSetupStyler: styler for Inno Setup scripts
}

interface

uses
  SysUtils, Classes, Graphics, ScintEdit;

type
  { Internally-used types }
  TInnoSetupStylerParamInfo = record
    Name: TScintRawString;
  end;
  TInnoSetupStylerSpanState = (spNone, spBraceComment, spStarComment);

  TInnoSetupStylerSection = (
    scNone,            { Not inside a section (start of file, or last section was closed) }
    scUnknown,         { Inside an unrecognized section }
    scThirdParty,      { Inside a '_' section (reserved for third-party tools) }
    scCode,
    scComponents,
    scCustomMessages,
    scDirs,
    scFiles,
    scIcons,
    scINI,
    scInstallDelete,
    scLangOptions,
    scLanguages,
    scMessages,
    scRegistry,
    scRun,
    scSetup,
    scTasks,
    scTypes,
    scUninstallDelete,
    scUninstallRun);

  TInnoSetupStylerStyle = (stDontUse { Makes stDefault equal 1 instead of 0
      which makes sure ApplyStyle doesn't overwrite already applied stDefault
      styles which is needed for PreStyleInlineISPPDirectives to work properly
      when the inline directive is inside a comment or string.
      Note: using 'stDefault = 1' isn't supported by Delphi 3. },
    stDefault, stCompilerDirective,
    stComment, stSection, stSymbol, stKeyword, stParameterValue,
    stEventFunction, stConstant, stMessageArg,
    stPascalReservedWord, stPascalString, stPascalNumber,
    stISPPReservedWord, stISPPString, stISPPNumber);

  TInnoSetupStyler = class(TScintCustomStyler)
  private
    FKeywordList: array[TInnoSetupStylerSection] of AnsiString;
    FIsppInstalled: Boolean;
    procedure ApplyPendingSquigglyFromToIndex(const StartIndex, EndIndex: Integer);
    procedure ApplyPendingSquigglyFromIndex(const StartIndex: Integer);
    procedure ApplySquigglyFromIndex(const StartIndex: Integer);
    procedure BuildKeywordListFromEnumType(const Section: TInnoSetupStylerSection;
      const EnumTypeInfo: Pointer);
    procedure BuildKeywordListFromParameters(const Section: TInnoSetupStylerSection;
      const Parameters: array of TInnoSetupStylerParamInfo);
    procedure CommitStyleSq(const Style: TInnoSetupStylerStyle;
      const Squigglify: Boolean);
    procedure CommitStyleSqPending(const Style: TInnoSetupStylerStyle);
    function GetKeywordList(Section: TInnoSetupStylerSection): AnsiString;
    procedure HandleCodeSection(var SpanState: TInnoSetupStylerSpanState);
    procedure HandleKeyValueSection(const Section: TInnoSetupStylerSection);
    procedure HandleParameterSection(const ValidParameters: array of TInnoSetupStylerParamInfo);
    procedure HandleCompilerDirective(const InlineDirective: Boolean;
      const InlineDirectiveEndIndex: Integer; var OpenCount: ShortInt);
    procedure PreStyleInlineISPPDirectives;
    procedure SkipWhitespace;
    procedure SquigglifyUntilChars(const Chars: TScintRawCharSet;
      const Style: TInnoSetupStylerStyle);
    procedure StyleConstsUntilChars(const Chars: TScintRawCharSet;
      const NonConstStyle: TInnoSetupStylerStyle; var BraceLevel: Integer);
  protected
    procedure CommitStyle(const Style: TInnoSetupStylerStyle);
    procedure GetStyleAttributes(const Style: Integer;
      var Attributes: TScintStyleAttributes); override;
    function LineTextSpans(const S: TScintRawString): Boolean; override;
    procedure StyleNeeded; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetSectionFromLineState(const LineState: TScintLineState): TInnoSetupStylerSection;
    class function IsParamSection(const Section: TInnoSetupStylerSection): Boolean;
    class function IsSymbolStyle(const Style: TScintStyleNumber): Boolean;
    property KeywordList[Section: TInnoSetupStylerSection]: AnsiString read GetKeywordList;
    property IsppInstalled: Boolean read FIsppInstalled write FIsppInstalled;
  end;

implementation

uses
  TypInfo;

type
  TInnoSetupStylerLineState = record
    Section, NextLineSection: TInnoSetupStylerSection;
    SpanState: TInnoSetupStylerSpanState;
    OpenCompilerDirectivesCount: ShortInt;
  end;

  TSetupSectionDirective = (
    ssAllowCancelDuringInstall,
    ssAllowNetworkDrive,
    ssAllowNoIcons,
    ssAllowRootDirectory,
    ssAllowUNCPath,
    ssAlwaysRestart,
    ssAlwaysShowComponentsList,
    ssAlwaysShowDirOnReadyPage,
    ssAlwaysShowGroupOnReadyPage,
    ssAlwaysUsePersonalGroup,
    ssAppCopyright,
    ssAppendDefaultDirName,
    ssAppendDefaultGroupName,
    ssAppComments,
    ssAppContact,
    ssAppId,
    ssAppModifyPath,
    ssAppMutex,
    ssAppName,
    ssAppPublisher,
    ssAppPublisherURL,
    ssAppReadmeFile,
    ssAppSupportPhone,
    ssAppSupportURL,
    ssAppUpdatesURL,
    ssAppVerName,
    ssAppVersion,
    ssArchitecturesAllowed,
    ssArchitecturesInstallIn64BitMode,
    ssASLRCompatible,
    ssBackColor,
    ssBackColor2,
    ssBackColorDirection,
    ssBackSolid,
    ssChangesAssociations,
    ssChangesEnvironment,
    ssCloseApplications,
    ssCloseApplicationsFilter,
    ssCompression,
    ssCompressionThreads,
    ssCreateAppDir,
    ssCreateUninstallRegKey,
    ssDefaultDialogFontName,
    ssDefaultDirName,
    ssDefaultGroupName,
    ssDefaultUserInfoName,
    ssDefaultUserInfoOrg,
    ssDefaultUserInfoSerial,
    ssDEPCompatible,
    ssDirExistsWarning,
    ssDisableDirPage,
    ssDisableFinishedPage,
    ssDisableProgramGroupPage,
    ssDisableReadyMemo,
    ssDisableReadyPage,
    ssDisableStartupPrompt,
    ssDisableWelcomePage,
    ssDiskClusterSize,
    ssDiskSliceSize,
    ssDiskSpanning,
    ssDontMergeDuplicateFiles,
    ssEnableDirDoesntExistWarning,
    ssEncryption,
    ssExtraDiskSpaceRequired,
    ssFlatComponentsList,
    ssInfoAfterFile,
    ssInfoBeforeFile,
    ssInternalCompressLevel,
    ssLanguageDetectionMethod,
    ssLicenseFile,
    ssLZMAAlgorithm,
    ssLZMABlockSize,
    ssLZMADictionarySize,
    ssLZMAMatchFinder,
    ssLZMANumBlockThreads,
    ssLZMANumFastBytes,
    ssLZMAUseSeparateProcess,
    ssMergeDuplicateFiles,
    ssMessagesFile,
    ssMinVersion,
    ssOnlyBelowVersion,
    ssOutput,
    ssOutputBaseFilename,
    ssOutputDir,
    ssOutputManifestFile,
    ssPassword,
    ssPrivilegesRequired,
    ssReserveBytes,
    ssRestartApplications,
    ssRestartIfNeededByRun,
    ssSetupIconFile,
    ssSetupLogging,
    ssSetupMutex,
    ssShowComponentSizes,
    ssShowLanguageDialog,
    ssShowTasksTreeLines,
    ssShowUndisplayableLanguages,
    ssSignedUninstaller,
    ssSignedUninstallerDir,
    ssSignTool,
    ssSignToolMinimumTimeBetween,
    ssSignToolRetryCount,
    ssSignToolRetryDelay,
    ssSlicesPerDisk,
    ssSolidCompression,
    ssSourceDir,
    ssTerminalServicesAware,
    ssTimeStampRounding,
    ssTimeStampsInUTC,
    ssTouchDate,
    ssTouchTime,
    ssUpdateUninstallLogAppName,
    ssUninstallable,
    ssUninstallDisplayIcon,
    ssUninstallDisplayName,
    ssUninstallDisplaySize,
    ssUninstallFilesDir,
    ssUninstallIconFile,
    ssUninstallLogMode,
    ssUninstallRestartComputer,
    ssUninstallStyle,
    ssUsedUserAreasWarning,
    ssUsePreviousAppDir,
    ssUsePreviousGroup,
    ssUsePreviousLanguage,
    ssUsePreviousSetupType,
    ssUsePreviousTasks,
    ssUsePreviousUserInfo,
    ssUseSetupLdr,
    ssUserInfoPage,
    ssVersionInfoCompany,
    ssVersionInfoCopyright,
    ssVersionInfoDescription,
    ssVersionInfoOriginalFileName,
    ssVersionInfoProductName,
    ssVersionInfoProductVersion,
    ssVersionInfoProductTextVersion,
    ssVersionInfoTextVersion,
    ssVersionInfoVersion,
    ssWindowResizable,
    ssWindowShowCaption,
    ssWindowStartMaximized,
    ssWindowVisible,
    ssWizardImageAlphaFormat,
    ssWizardImageBackColor,
    ssWizardImageFile,
    ssWizardImageStretch,
    ssWizardSmallImageBackColor,
    ssWizardSmallImageFile,
    ssWizardStyle);

  TLangOptionsSectionDirective = (
    lsCopyrightFontName,
    lsCopyrightFontSize,
    lsDialogFontName,
    lsDialogFontSize,
    lsDialogFontStandardHeight,
    lsLanguageCodePage,
    lsLanguageID,
    lsLanguageName,
    lsRightToLeft,
    lsTitleFontName,
    lsTitleFontSize,
    lsWelcomeFontName,
    lsWelcomeFontSize);

const
  ComponentsSectionParameters: array[0..8] of TInnoSetupStylerParamInfo = (
    (Name: 'Check'),
    (Name: 'Description'),
    (Name: 'ExtraDiskSpaceRequired'),
    (Name: 'Flags'),
    (Name: 'Languages'),
    (Name: 'MinVersion'),
    (Name: 'Name'),
    (Name: 'OnlyBelowVersion'),
    (Name: 'Types'));

  DeleteSectionParameters: array[0..9] of TInnoSetupStylerParamInfo = (
    (Name: 'AfterInstall'),
    (Name: 'BeforeInstall'),
    (Name: 'Check'),
    (Name: 'Components'),
    (Name: 'Languages'),
    (Name: 'MinVersion'),
    (Name: 'Name'),
    (Name: 'OnlyBelowVersion'),
    (Name: 'Tasks'),
    (Name: 'Type'));

  DirsSectionParameters: array[0..11] of TInnoSetupStylerParamInfo = (
    (Name: 'AfterInstall'),
    (Name: 'Attribs'),
    (Name: 'BeforeInstall'),
    (Name: 'Check'),
    (Name: 'Components'),
    (Name: 'Flags'),
    (Name: 'Languages'),
    (Name: 'MinVersion'),
    (Name: 'Name'),
    (Name: 'OnlyBelowVersion'),
    (Name: 'Permissions'),
    (Name: 'Tasks'));

  FilesSectionParameters: array[0..18] of TInnoSetupStylerParamInfo = (
    (Name: 'AfterInstall'),
    (Name: 'Attribs'),
    (Name: 'BeforeInstall'),
    (Name: 'Check'),
    (Name: 'Components'),
    (Name: 'CopyMode'),
    (Name: 'DestDir'),
    (Name: 'DestName'),
    (Name: 'Excludes'),
    (Name: 'ExternalSize'),
    (Name: 'Flags'),
    (Name: 'FontInstall'),
    (Name: 'Languages'),
    (Name: 'MinVersion'),
    (Name: 'OnlyBelowVersion'),
    (Name: 'Permissions'),
    (Name: 'Source'),
    (Name: 'StrongAssemblyName'),
    (Name: 'Tasks'));

  IconsSectionParameters: array[0..17] of TInnoSetupStylerParamInfo = (
    (Name: 'AfterInstall'),
    (Name: 'AppUserModelID'),
    (Name: 'BeforeInstall'),
    (Name: 'Check'),
    (Name: 'Comment'),
    (Name: 'Components'),
    (Name: 'Filename'),
    (Name: 'Flags'),
    (Name: 'HotKey'),
    (Name: 'IconFilename'),
    (Name: 'IconIndex'),
    (Name: 'Languages'),
    (Name: 'MinVersion'),
    (Name: 'Name'),
    (Name: 'OnlyBelowVersion'),
    (Name: 'Parameters'),
    (Name: 'Tasks'),
    (Name: 'WorkingDir'));

  INISectionParameters: array[0..12] of TInnoSetupStylerParamInfo = (
    (Name: 'AfterInstall'),
    (Name: 'BeforeInstall'),
    (Name: 'Check'),
    (Name: 'Components'),
    (Name: 'Filename'),
    (Name: 'Flags'),
    (Name: 'Key'),
    (Name: 'Languages'),
    (Name: 'MinVersion'),
    (Name: 'OnlyBelowVersion'),
    (Name: 'Section'),
    (Name: 'String'),
    (Name: 'Tasks'));

  LanguagesSectionParameters: array[0..4] of TInnoSetupStylerParamInfo = (
    (Name: 'InfoAfterFile'),
    (Name: 'InfoBeforeFile'),
    (Name: 'LicenseFile'),
    (Name: 'MessagesFile'),
    (Name: 'Name'));

  RegistrySectionParameters: array[0..14] of TInnoSetupStylerParamInfo = (
    (Name: 'AfterInstall'),
    (Name: 'BeforeInstall'),
    (Name: 'Check'),
    (Name: 'Components'),
    (Name: 'Flags'),
    (Name: 'Languages'),
    (Name: 'MinVersion'),
    (Name: 'OnlyBelowVersion'),
    (Name: 'Permissions'),
    (Name: 'Root'),
    (Name: 'Subkey'),
    (Name: 'Tasks'),
    (Name: 'ValueData'),
    (Name: 'ValueName'),
    (Name: 'ValueType'));

  RunSectionParameters: array[0..15] of TInnoSetupStylerParamInfo = (
    (Name: 'AfterInstall'),
    (Name: 'BeforeInstall'),
    (Name: 'Check'),
    (Name: 'Components'),
    (Name: 'Description'),
    (Name: 'Filename'),
    (Name: 'Flags'),
    (Name: 'Languages'),
    (Name: 'MinVersion'),
    (Name: 'OnlyBelowVersion'),
    (Name: 'Parameters'),
    (Name: 'RunOnceId'),
    (Name: 'StatusMsg'),
    (Name: 'Tasks'),
    (Name: 'Verb'),
    (Name: 'WorkingDir'));

  TasksSectionParameters: array[0..8] of TInnoSetupStylerParamInfo = (
    (Name: 'Check'),
    (Name: 'Components'),
    (Name: 'Description'),
    (Name: 'Flags'),
    (Name: 'GroupDescription'),
    (Name: 'Languages'),
    (Name: 'MinVersion'),
    (Name: 'Name'),
    (Name: 'OnlyBelowVersion'));

  TypesSectionParameters: array[0..6] of TInnoSetupStylerParamInfo = (
    (Name: 'Check'),
    (Name: 'Description'),
    (Name: 'Flags'),
    (Name: 'Languages'),
    (Name: 'MinVersion'),
    (Name: 'Name'),
    (Name: 'OnlyBelowVersion'));

const
  inSquiggly = 0;
  inPendingSquiggly = 1;

  AllChars = [#0..#255];
  WhitespaceChars = [#0..' '];
  AlphaChars = ['A'..'Z', 'a'..'z'];
  DigitChars = ['0'..'9'];
  HexDigitChars = DigitChars + ['A'..'F', 'a'..'f'];
  AlphaUnderscoreChars = AlphaChars + ['_'];
  AlphaDigitChars = AlphaChars + DigitChars;
  AlphaDigitUnderscoreChars = AlphaChars + DigitChars + ['_'];

  PascalIdentFirstChars = AlphaUnderscoreChars;
  PascalIdentChars = AlphaDigitUnderscoreChars;

  ISPPIdentFirstChars = AlphaUnderscoreChars;
  ISPPIdentChars = AlphaDigitUnderscoreChars;

function SameRawText(const S1, S2: TScintRawString): Boolean;
var
  Len, I: Integer;
  C1, C2: AnsiChar;
begin
  Len := Length(S1);
  if Length(S2) <> Len then begin
    Result := False;
    Exit;
  end;
  for I := 1 to Len do begin
    C1 := S1[I];
    C2 := S2[I];
    if C1 in ['A'..'Z'] then
      Inc(C1, 32);
    if C2 in ['A'..'Z'] then
      Inc(C2, 32);
    if C1 <> C2 then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;


function GetASCIISortedInsertPos(const SL: TStringList; const S: String): Integer;
var
  L, H, I, C: Integer;
begin
  L := 0;
  H := SL.Count - 1;
  while L <= H do begin
    I := (L + H) div 2;
    C := CompareText(SL[I], S);
    if C = 0 then begin
      L := I;
      Break;
    end;
    if C < 0 then
      L := I + 1
    else
      H := I - 1;
  end;
  Result := L;
end;

function MapSectionNameString(const S: TScintRawString): TInnoSetupStylerSection;
type
  TSectionMapEntry = record
    Name: TScintRawString;
    Value: TInnoSetupStylerSection;
  end;
const
  SectionMap: array[0..17] of TSectionMapEntry = (
    (Name: 'Code'; Value: scCode),
    (Name: 'Components'; Value: scComponents),
    (Name: 'CustomMessages'; Value: scCustomMessages),
    (Name: 'Dirs'; Value: scDirs),
    (Name: 'Files'; Value: scFiles),
    (Name: 'Icons'; Value: scIcons),
    (Name: 'INI'; Value: scINI),
    (Name: 'InstallDelete'; Value: scInstallDelete),
    (Name: 'LangOptions'; Value: scLangOptions),
    (Name: 'Languages'; Value: scLanguages),
    (Name: 'Messages'; Value: scMessages),
    (Name: 'Registry'; Value: scRegistry),
    (Name: 'Run'; Value: scRun),
    (Name: 'Setup'; Value: scSetup),
    (Name: 'Tasks'; Value: scTasks),
    (Name: 'Types'; Value: scTypes),
    (Name: 'UninstallDelete'; Value: scUninstallDelete),
    (Name: 'UninstallRun'; Value: scUninstallRun));
var
  I: Integer;
begin
  if (S <> '') and (S[1] = '_') then
    Result := scThirdParty
  else begin
    Result := scUnknown;
    for I := Low(SectionMap) to High(SectionMap) do
      if SameRawText(S, SectionMap[I].Name) then begin
        Result := SectionMap[I].Value;
        Break;
      end;
  end;
end;

{ TInnoSetupStyler }

constructor TInnoSetupStyler.Create(AOwner: TComponent);
begin
  inherited;
  BuildKeywordListFromParameters(scComponents, ComponentsSectionParameters);
  BuildKeywordListFromParameters(scDirs, DirsSectionParameters);
  BuildKeywordListFromParameters(scFiles, FilesSectionParameters);
  BuildKeywordListFromParameters(scIcons, IconsSectionParameters);
  BuildKeywordListFromParameters(scINI, INISectionParameters);
  BuildKeywordListFromParameters(scInstallDelete, DeleteSectionParameters);
  BuildKeywordListFromEnumType(scLangOptions, TypeInfo(TLangOptionsSectionDirective));
  BuildKeywordListFromParameters(scLanguages, LanguagesSectionParameters);
  BuildKeywordListFromParameters(scRegistry, RegistrySectionParameters);
  BuildKeywordListFromParameters(scRun, RunSectionParameters);
  BuildKeywordListFromEnumType(scSetup, TypeInfo(TSetupSectionDirective));
  BuildKeywordListFromParameters(scTasks, TasksSectionParameters);
  BuildKeywordListFromParameters(scTypes, TypesSectionParameters);
  BuildKeywordListFromParameters(scUninstallDelete, DeleteSectionParameters);
  BuildKeywordListFromParameters(scUninstallRun, RunSectionParameters);
end;

procedure TInnoSetupStyler.ApplyPendingSquigglyFromToIndex(const StartIndex, EndIndex: Integer);
begin
  if (CaretIndex >= StartIndex) and (CaretIndex <= EndIndex + 1) then
    ApplyIndicators([inPendingSquiggly], StartIndex, EndIndex)
  else
    ApplyIndicators([inSquiggly], StartIndex, EndIndex);
end;

procedure TInnoSetupStyler.ApplyPendingSquigglyFromIndex(const StartIndex: Integer);
begin
  ApplyPendingSquigglyFromToIndex(StartIndex, CurIndex - 1);
end;

procedure TInnoSetupStyler.ApplySquigglyFromIndex(const StartIndex: Integer);
begin
  ApplyIndicators([inSquiggly], StartIndex, CurIndex - 1);
end;

procedure TInnoSetupStyler.BuildKeywordListFromEnumType(
  const Section: TInnoSetupStylerSection; const EnumTypeInfo: Pointer);
var
  SL: TStringList;
  I: Integer;
  S: String;
  A, WordList: AnsiString;
begin
  SL := TStringList.Create;
  try
    { Scintilla uses an ASCII binary search so the list must be in
      ASCII sort order (case-insensitive). (TStringList's Sort method is
      not suitable as it uses AnsiCompareText.) }
    for I := 0 to GetTypeData(EnumTypeInfo).MaxValue do begin
      S := Copy(GetEnumName(EnumTypeInfo, I), 3, Maxint);
      SL.Insert(GetASCIISortedInsertPos(SL, S), S);
    end;
    for I := 0 to SL.Count-1 do begin
      A := AnsiString(SL[I]);
      if I = 0 then
        WordList := A
      else
        WordList := WordList + ' ' + A;
    end;
  finally
    SL.Free;
  end;
  FKeywordList[Section] := WordList;
end;

procedure TInnoSetupStyler.BuildKeywordListFromParameters(
  const Section: TInnoSetupStylerSection;
  const Parameters: array of TInnoSetupStylerParamInfo);
var
  SL: TStringList;
  I: Integer;
  S: String;
  A, WordList: AnsiString;
begin
  SL := TStringList.Create;
  try
    { Scintilla uses an ASCII binary search so the list must be in
      ASCII sort order (case-insensitive). (TStringList's Sort method is
      not suitable as it uses AnsiCompareText.) }
    for I := 0 to High(Parameters) do begin
      S := String(Parameters[I].Name);
      SL.Insert(GetASCIISortedInsertPos(SL, S), S);
    end;
    for I := 0 to SL.Count-1 do begin
      A := AnsiString(SL[I]);
      if I = 0 then
        WordList := A
      else
        WordList := WordList + ' ' + A;
    end;
  finally
    SL.Free;
  end;
  FKeywordList[Section] := WordList;
end;

procedure TInnoSetupStyler.CommitStyle(const Style: TInnoSetupStylerStyle);
begin
  inherited CommitStyle(Ord(Style));
end;

procedure TInnoSetupStyler.CommitStyleSq(const Style: TInnoSetupStylerStyle;
  const Squigglify: Boolean);
begin
  if Squigglify then
    ApplySquigglyFromIndex(StyleStartIndex);
  CommitStyle(Style);
end;

procedure TInnoSetupStyler.CommitStyleSqPending(const Style: TInnoSetupStylerStyle);
begin
  ApplyPendingSquigglyFromIndex(StyleStartIndex);
  CommitStyle(Style);
end;

function TInnoSetupStyler.GetKeywordList(Section: TInnoSetupStylerSection): AnsiString;
begin
  Result := FKeywordList[Section];
end;

class function TInnoSetupStyler.GetSectionFromLineState(
  const LineState: TScintLineState): TInnoSetupStylerSection;
begin
  Result := TInnoSetupStylerLineState(LineState).Section;
end;

procedure TInnoSetupStyler.GetStyleAttributes(const Style: Integer;
  var Attributes: TScintStyleAttributes);
const
  STYLE_BRACELIGHT = 34;
  STYLE_IDENTGUIDE = 37;
begin
  if (Style >= 0) and (Style <= Ord(High(TInnoSetupStylerStyle))) then begin
    case TInnoSetupStylerStyle(Style) of
      stCompilerDirective: Attributes.ForeColor := $4040C0;
      stComment: Attributes.ForeColor := clGreen;
      stSection: Attributes.FontStyle := [fsBold];
      stSymbol: Attributes.ForeColor := $707070;
      stKeyword, stPascalReservedWord, stISPPReservedWord: Attributes.ForeColor := clBlue;
      //stParameterValue: Attributes.ForeColor := clTeal;
      stEventFunction: Attributes.FontStyle := [fsBold];
      stConstant: Attributes.ForeColor := $C00080;
      stMessageArg: Attributes.ForeColor := $FF8000;
      stPascalString, stPascalNumber, stISPPString, stISPPNumber: Attributes.ForeColor := clMaroon;
    end;
  end
  else begin
    case Style of
      STYLE_BRACELIGHT: Attributes.BackColor := $E0E0E0;
      STYLE_IDENTGUIDE: Attributes.ForeColor := clSilver;
    end;
  end;
end;

procedure TInnoSetupStyler.HandleCodeSection(var SpanState: TInnoSetupStylerSpanState);

  function FinishConsumingBraceComment: Boolean;
  begin
    ConsumeCharsNot(['}']);
    Result := ConsumeChar('}');
    CommitStyle(stComment);
  end;

  function FinishConsumingStarComment: Boolean;
  begin
    Result := False;
    while True do begin
      ConsumeCharsNot(['*']);
      if not ConsumeChar('*') then
        Break;
      if ConsumeChar(')') then begin
        Result := True;
        Break;
      end;
    end;
    CommitStyle(stComment);
  end;

const
  PascalReservedWords: array[0..42] of TScintRawString = (
    'and', 'array', 'as', 'begin', 'case', 'const', 'div',
    'do', 'downto', 'else', 'end', 'except', 'external',
    'finally', 'for', 'forward', 'function', 'goto', 'if', 'in', 'is',
    'label', 'mod', 'nil', 'not', 'of', 'or', 'procedure',
    'program', 'record', 'repeat', 'set', 'shl', 'shr',
    'then', 'to', 'try', 'type', 'until', 'var', 'while',
    'with', 'xor');
  EventFunctions: array[0..22] of TScintRawString = (
    'InitializeSetup', 'DeinitializeSetup', 'CurStepChanged',
    'NextButtonClick', 'BackButtonClick', 'ShouldSkipPage',
    'CurPageChanged', 'CheckPassword', 'NeedRestart',
    'UpdateReadyMemo', 'RegisterPreviousData', 'CheckSerial',
    'InitializeWizard', 'GetCustomSetupExitCode',
    'InitializeUninstall', 'DeinitializeUninstall',
    'CurUninstallStepChanged', 'UninstallNeedRestart',
    'CancelButtonClick', 'InitializeUninstallProgressForm',
    'PrepareToInstall', 'RegisterExtraCloseApplicationsResources',
    'CurInstallProgressChanged');
var
  S: TScintRawString;
  I: Integer;
  C: AnsiChar;
begin
  case SpanState of
    spBraceComment:
      if not FinishConsumingBraceComment then
        Exit;
    spStarComment:
      if not FinishConsumingStarComment then
        Exit;
  end;

  SpanState := spNone;
  SkipWhitespace;
  while not EndOfLine do begin
    if CurChar in PascalIdentFirstChars then begin
      S := ConsumeString(PascalIdentChars);
      for I := Low(PascalReservedWords) to High(PascalReservedWords) do
        if SameRawText(S, PascalReservedWords[I]) then begin
          CommitStyle(stPascalReservedWord);
          Break;
        end;
      for I := Low(EventFunctions) to High(EventFunctions) do
        if SameRawText(S, EventFunctions[I]) then begin
          CommitStyle(stEventFunction);
          Break;
        end;
      CommitStyle(stDefault);
    end
    else if ConsumeChars(DigitChars) then begin
      if not CurCharIs('.') or not NextCharIs('.') then begin
        if ConsumeChar('.') then
          ConsumeChars(DigitChars);
        C := CurChar;
        if C in ['E', 'e'] then begin
          ConsumeChar(C);
          if not ConsumeChar('-') then
            ConsumeChar('+');
          if not ConsumeChars(DigitChars) then
            CommitStyleSqPending(stPascalNumber);
        end;
      end;
      CommitStyle(stPascalNumber);
    end
    else begin
      C := CurChar;
      ConsumeChar(C);
      case C of
        ';', ':', '=', '+', '-', '*', '/', '<', '>', ',', '(', ')',
        '.', '[', ']', '@', '^':
          begin
            if (C = '/') and ConsumeChar('/') then begin
              ConsumeAllRemaining;
              CommitStyle(stComment);
            end
            else if (C = '(') and ConsumeChar('*') then begin
              if not FinishConsumingStarComment then begin
                SpanState := spStarComment;
                Exit;
              end;
            end
            else
              CommitStyle(stSymbol);
          end;
        '''':
          begin
            while True do begin
              ConsumeCharsNot([C]);
              if not ConsumeChar(C) then begin
                CommitStyleSqPending(stPascalString);
                Break;
              end;
              if not ConsumeChar(C) then begin
                CommitStyle(stPascalString);
                Break;
              end;
            end;
          end;
        '{':
          begin
            if not FinishConsumingBraceComment then begin
              SpanState := spBraceComment;
              Exit;
            end;
          end;
        '$':
          begin
            if not ConsumeChars(HexDigitChars) then
              CommitStyleSqPending(stPascalNumber);
            CommitStyle(stPascalNumber);
          end;
        '#':
          begin
            if ConsumeChar('$') then begin
              if not ConsumeChars(HexDigitChars) then
                 CommitStyleSqPending(stPascalString);
            end
            else if not ConsumeChars(DigitChars) then
              CommitStyleSqPending(stPascalString);
            CommitStyle(stPascalString);
          end;
      else
        { Illegal character }
        CommitStyleSq(stSymbol, True);
      end;
    end;
    SkipWhitespace;
  end;
end;

procedure TInnoSetupStyler.HandleCompilerDirective(const InlineDirective: Boolean; const InlineDirectiveEndIndex: Integer; var OpenCount: ShortInt);

  function EndOfDirective: Boolean;
  begin
    Result := EndOfLine or (InlineDirective and (CurIndex > InlineDirectiveEndIndex));
  end;

  procedure FinishDirectiveNameOrShorthand(const RequiresParameter: Boolean);
  begin
    if RequiresParameter then begin
      ConsumeChars(WhitespaceChars); { This will give the whitespace the stCompilerDirective style instead of stDefault but that's ok }
      if EndOfDirective then
        CommitStyleSqPending(stCompilerDirective)
      else
        CommitStyle(stCompilerDirective);
    end else
      CommitStyle(stCompilerDirective);
  end;

  function FinishConsumingStarComment: Boolean;
  begin
    Result := False;
    while True do begin
      ConsumeCharsNot(['*']);
      if not ConsumeChar('*') then
        Break;
      if ConsumeChar('/') then begin
        Result := True;
        Break;
      end;
    end;
    if Result then
      CommitStyle(stComment)
    else
      CommitStyleSqPending(stComment);
  end;

const
  ISPPReservedWords: array[0..16] of TScintRawString = (
    'private', 'protected', 'public', 'any', 'int',
    'str', 'func', 'option', 'parseroption', 'inlinestart',
    'inlineend', 'message', 'warning', 'error',
    'verboselevel', 'include', 'spansymbol');
type
  TISPPDirective = record
    Name: TScintRawString;
    RequiresParameter: Boolean;
    OpenCountChange: ShortInt;
  end;
const
  ISPPDirectives: array[0..22] of TISPPDirective = (
    (Name: 'define'; RequiresParameter: True; OpenCountChange: 0),
    (Name: 'dim'; RequiresParameter: True; OpenCountChange: 0),
    (Name: 'redim'; RequiresParameter: True; OpenCountChange: 0),
    (Name: 'undef'; RequiresParameter: True; OpenCountChange: 0),
    (Name: 'include'; RequiresParameter: True; OpenCountChange: 0),
    (Name: 'file'; RequiresParameter: True; OpenCountChange: 0),
    (Name: 'emit'; RequiresParameter: True; OpenCountChange: 0),
    (Name: 'expr'; RequiresParameter: True; OpenCountChange: 0),
    (Name: 'insert'; RequiresParameter: True; OpenCountChange: 0),
    (Name: 'append'; RequiresParameter: False; OpenCountChange: 0),
    (Name: 'if'; RequiresParameter: True; OpenCountChange: 1),
    (Name: 'elif'; RequiresParameter: False { bug in ISPP? }; OpenCountChange: 0),
    (Name: 'else'; RequiresParameter: False; OpenCountChange: 0),
    (Name: 'endif'; RequiresParameter: False; OpenCountChange: -1),
    (Name: 'ifdef'; RequiresParameter: True; OpenCountChange: 1),
    (Name: 'ifndef'; RequiresParameter: True; OpenCountChange: 1),
    (Name: 'ifexist'; RequiresParameter: True; OpenCountChange: 1),
    (Name: 'ifnexist'; RequiresParameter: True; OpenCountChange: 1),
    (Name: 'for'; RequiresParameter: True; OpenCountChange: 0),
    (Name: 'sub'; RequiresParameter: True; OpenCountChange: 1),
    (Name: 'endsub'; RequiresParameter: False; OpenCountChange: -1),
    (Name: 'pragma'; RequiresParameter: False; OpenCountChange: 0),
    (Name: 'error'; RequiresParameter: False; OpenCountChange: 0));
  ISPPDirectiveShorthands: TScintRawCharSet =
    [':' {define},
     'x' {undef},
     '+' {include},
     '=' {emit},
     '!' {expr}];
var
  S: TScintRawString;
  StartIndex, I: Integer;
  C: AnsiChar;
  NeedIspp, ForDirectiveExpressionsNext: Boolean;
begin
  StartIndex := CurIndex;
  if InlineDirective then begin
    ConsumeChar('{');
    NeedIspp := True;
  end else
    NeedIspp := False; { Might be updated later to True later }
  ForDirectiveExpressionsNext := False;
  ConsumeChar('#');
  CommitStyle(stCompilerDirective);

  { Directive name or shorthand }
  SkipWhiteSpace;
  if ConsumeCharIn(ISPPDirectiveShorthands) then begin
    NeedIspp := True;
    FinishDirectiveNameOrShorthand(True); { All shorthands require a parameter }
  end
  else begin
    S := ConsumeString(ISPPIdentChars);
    for I := Low(ISPPDirectives) to High(ISPPDirectives) do
      if SameRawText(S, ISPPDirectives[I].Name) then begin
        NeedIspp := not SameRawText(S, 'include'); { Built-in preprocessor only supports '#include' }
        ForDirectiveExpressionsNext := SameRawText(S, 'for'); { #for uses ';' as an expressions list separator so we need to remember that ';' doesn't start a comment until the list is done }
        Inc(OpenCount, ISPPDirectives[I].OpenCountChange);
        if OpenCount < 0 then begin
          CommitStyleSq(stCompilerDirective, True);
          OpenCount := 0; { Reset so that next doesn't automatically gets error as well }
        end;
        FinishDirectiveNameOrShorthand(ISPPDirectives[I].RequiresParameter);
        Break;
      end;
    if InlineDirective then
      CommitStyle(stDefault) { #emit shorthand was used (='#' directly followed by an expression): not an error }
    else
      CommitStyleSqPending(stCompilerDirective);
  end;

  { Rest of the directive }
  SkipWhitespace;
  if not NeedIspp then
    NeedIspp := CurChar <> '"'; { Built-in preprocessor requires a '"' quoted string after the '#include' and doesn't support anything else }
  while not EndOfDirective do begin
    if CurChar in ISPPIdentFirstChars then begin
      S := ConsumeString(ISPPIdentChars);
      for I := Low(ISPPReservedWords) to High(ISPPReservedWords) do
        if SameRawText(S, ISPPReservedWords[I]) then begin
          CommitStyle(stISPPReservedWord);
          Break;
        end;
      CommitStyle(stDefault)
    end
    else if ConsumeChars(DigitChars) then begin
      if not CurCharIs('.') or not NextCharIs('.') then begin
        if ConsumeChar('.') then
          ConsumeChars(DigitChars);
        C := CurChar;
        if C in ['X', 'x'] then begin
          ConsumeChar(C);
          if not ConsumeChars(HexDigitChars) then
            CommitStyleSqPending(stISPPNumber);
        end;
        ConsumeChars(['L', 'U', 'l', 'u']);
      end;
      CommitStyle(stISPPNumber);
    end
    else begin
      C := CurChar;
      ConsumeChar(C);
      case C of
        '!', '&', '=', '|', '^', '>', '<', '+', '-', '/', '%', '*',
        '?', ':', ',', '.', '~', '(', '[', '{', ')', ']', '}', '@',
        '#':
          begin
            if (C = '}') and ForDirectiveExpressionsNext then
              ForDirectiveExpressionsNext := False;
            if (C = '/') and ConsumeChar('*') then
              FinishConsumingStarComment
            else if InlineDirective and (C = '}') then
              CommitStyle(stCompilerDirective) (* Closing '}' of the ISPP inline directive *)
            else
              CommitStyle(stSymbol);
          end;
        ';':
          begin
            if ForDirectiveExpressionsNext then
              CommitStyle(stSymbol)
            else begin
              if not InlineDirective then
                ConsumeAllRemaining
              else
                ConsumeCharsNot(['}']);
              CommitStyle(stComment);
            end;
          end;
        '''', '"':
          begin
            while True do begin
              ConsumeCharsNot([C]);
              if not ConsumeChar(C) then begin
                CommitStyleSqPending(stISPPString);
                Break;
              end;
              if not ConsumeChar(C) then begin
                CommitStyle(stISPPString);
                Break;
              end;
            end;
          end;
      else
        { Illegal character }
        CommitStyleSq(stSymbol, True);
      end;
    end;
    SkipWhitespace;
  end;

  if NeedIspp and not IsppInstalled then begin
    if InlineDirective then
      ApplyPendingSquigglyFromToIndex(StartIndex + 1, InlineDirectiveEndIndex - 1)
    else
      ApplyPendingSquigglyFromIndex(StartIndex + 1);
  end;
end;

procedure TInnoSetupStyler.HandleParameterSection(
  const ValidParameters: array of TInnoSetupStylerParamInfo);
var
  ParamsSpecified: set of 0..31;
  S: TScintRawString;
  I, ParamValueIndex, BraceLevel: Integer;
  NamePresent, ValidName, DuplicateName, ColonPresent: Boolean;
begin
  ParamsSpecified := [];
  while not EndOfLine do begin
    { Squigglify any bogus characters before the parameter name }
    SquigglifyUntilChars(AlphaChars + [':'], stDefault);

    { Parameter name }
    S := ConsumeString(AlphaDigitChars);
    NamePresent := (S <> '');
    ValidName := False;
    DuplicateName := False;
    for I := Low(ValidParameters) to High(ValidParameters) do
      if SameRawText(S, ValidParameters[I].Name) then begin
        ValidName := True;
        DuplicateName := (I in ParamsSpecified);
        Include(ParamsSpecified, I);
        Break;
      end;
    if DuplicateName then
      CommitStyleSqPending(stKeyword)
    else if ValidName then
      CommitStyle(stKeyword)
    else
      CommitStyleSqPending(stDefault);
    SkipWhitespace;

    { If there's a semicolon with no colon, squigglify the semicolon }
    if ConsumeChar(';') then begin
      CommitStyleSq(stSymbol, True);
      SkipWhitespace;
      Continue;
    end;

    { Colon }
    ColonPresent := ConsumeChar(':');
    CommitStyleSq(stSymbol, not NamePresent);
    SkipWhitespace;

    { Parameter value. This consumes until a ';' is found or EOL is reached. }
    ParamValueIndex := CurIndex;
    BraceLevel := 0;
    if ConsumeChar('"') then begin
      while True do begin
        StyleConstsUntilChars(['"'], stParameterValue, BraceLevel);
        { If no closing quote exists, squigglify the whole value and break }
        if not ConsumeChar('"') then begin
          ApplyPendingSquigglyFromIndex(ParamValueIndex);
          Break;
        end;
        { Quote found, now break, unless there are two quotes in a row }
        if not ConsumeChar('"') then
          Break;
      end;
    end
    else begin
      while True do begin
        StyleConstsUntilChars([';', '"'], stParameterValue, BraceLevel);
        { Squigglify any quote characters inside an unquoted string }
        if ConsumeChar('"') then
          ApplySquigglyFromIndex(CurIndex - 1)
        else
          Break;
      end;
    end;
    CommitStyle(stParameterValue);
    if not ColonPresent then
      ApplySquigglyFromIndex(ParamValueIndex);
    { Squigglify any characters between a quoted string and the next ';' }
    SquigglifyUntilChars([';'], stDefault);

    { Semicolon }
    ConsumeChar(';');
    CommitStyle(stSymbol);
    SkipWhitespace;
  end;
end;

procedure TInnoSetupStyler.HandleKeyValueSection(const Section: TInnoSetupStylerSection);

  procedure StyleMessageArgs;
  begin
    while True do begin
      ConsumeCharsNot(['%']);
      CommitStyle(stDefault);
      if not ConsumeChar('%') then
        Break;
      if CurCharIn(['1'..'9', '%', 'n']) then begin
        ConsumeChar(CurChar);
        CommitStyle(stMessageArg);
      end;
    end;
  end;

var
  S: String;
  I, BraceLevel: Integer;
begin
  { Squigglify any bogus characters at the start of the line }
  SquigglifyUntilChars(AlphaUnderscoreChars, stDefault);
  if EndOfLine then
    Exit;

  S := String(ConsumeString(AlphaDigitUnderscoreChars));
  { Was that a language name? }
  if (Section in [scCustomMessages, scLangOptions, scMessages]) and
     CurCharIs('.') then begin
    CommitStyle(stDefault);
    ConsumeChar('.');
    CommitStyle(stSymbol);
    { Squigglify any spaces or bogus characters between the '.' and key name }
    if ConsumeCharsNot(AlphaUnderscoreChars) then
      CommitStyleSq(stDefault, True);
    S := String(ConsumeString(AlphaDigitUnderscoreChars));
  end;

  case Section of
    scLangOptions:
      I := GetEnumValue(TypeInfo(TLangOptionsSectionDirective), 'ls' + S);
    scSetup:
      I := GetEnumValue(TypeInfo(TSetupSectionDirective), 'ss' + S);
  else
    I := -1;
  end;
  if I <> -1 then
    CommitStyle(stKeyword)
  else begin
    if Section in [scLangOptions, scSetup] then
      CommitStyleSqPending(stDefault)
    else
      CommitStyle(stDefault);
  end;
  SquigglifyUntilChars(['='], stDefault);

  ConsumeChar('=');
  CommitStyle(stSymbol);
  SkipWhitespace;

  if Section in [scCustomMessages, scMessages] then
    StyleMessageArgs
  else begin
    BraceLevel := 0;
    StyleConstsUntilChars([], stDefault, BraceLevel);
  end;
end;

class function TInnoSetupStyler.IsParamSection(
  const Section: TInnoSetupStylerSection): Boolean;
begin
  Result := not (Section in [scCustomMessages, scLangOptions, scMessages, scSetup]);
end;

class function TInnoSetupStyler.IsSymbolStyle(const Style: TScintStyleNumber): Boolean;
begin
  Result := (Style = Ord(stSymbol));
end;

function TInnoSetupStyler.LineTextSpans(const S: TScintRawString): Boolean;
var
  I: Integer;
begin
  { Note: To match ISPP behavior, require length of at least 3 }
  I := Length(S);
  Result := (I > 2) and (S[I] = '\') and (S[I-1] in WhitespaceChars);
end;

procedure TInnoSetupStyler.PreStyleInlineISPPDirectives;

  function IsLineCommented: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to TextLength do begin
      { In ISPP, only ';' and '//' inhibit processing of inline directives }
      if (Text[I] = ';') or
         ((I < TextLength) and (Text[I] = '/') and (Text[I+1] = '/')) then begin
        Result := True;
        Break;
      end;
      if not(Text[I] in WhitespaceChars) then
        Break;
    end;
  end;

const
  LineEndChars = [#10, #13];
var
  I, StartIndex: Integer;
  Valid: Boolean;
  Dummy: ShortInt;
begin
  { Style span symbols, then replace them with spaces to prevent any further
    processing }
  for I := 3 to TextLength do begin
    if ((I = TextLength) or (Text[I+1] in LineEndChars)) and
       (Text[I] = '\') and (Text[I-1] in WhitespaceChars) and
       not(Text[I-2] in LineEndChars) then begin
      ReplaceText(I, I, ' ');
      ApplyStyle(Ord(stSymbol), I, I);
      if not IsppInstalled then
        ApplyIndicators([inSquiggly], I, I);
    end;
  end;

  { Style all '{#' ISPP inline directives before anything else }
  if not IsLineCommented then begin
    I := 1;
    while I < TextLength do begin
      if (Text[I] = '{') and (Text[I+1] = '#') then begin
        StartIndex := I;
        Valid := False;
        while I <= TextLength do begin
          Inc(I);
          if Text[I-1] = '}' then begin
            Valid := True;
            Break;
          end;
        end;
        ResetCurIndexTo(StartIndex);
        try
          HandleCompilerDirective(True, I - 1, Dummy);
        finally
          ResetCurIndexTo(0);
        end;
        if not Valid then
          ApplyPendingSquigglyFromToIndex(StartIndex, I - 1);
        { Replace the directive with spaces to prevent any further processing }
        ReplaceText(StartIndex, I - 1, ' ');
      end
      else
        Inc(I);
    end;
  end;
end;

procedure TInnoSetupStyler.SkipWhitespace;
begin
  ConsumeChars(WhitespaceChars);
  CommitStyle(stDefault);
end;

procedure TInnoSetupStyler.SquigglifyUntilChars(const Chars: TScintRawCharSet;
  const Style: TInnoSetupStylerStyle);
var
  IsWhitespace: Boolean;
begin
  { Consume and squigglify all non-whitespace characters until one of Chars
    is encountered }
  while not EndOfLine and not CurCharIn(Chars) do begin
    IsWhitespace := CurCharIn(WhitespaceChars);
    ConsumeChar(CurChar);
    if IsWhitespace then
      CommitStyle(stDefault)
    else
      CommitStyleSq(Style, True);
  end;
  CommitStyle(stDefault);
end;

procedure TInnoSetupStyler.StyleConstsUntilChars(const Chars: TScintRawCharSet;
  const NonConstStyle: TInnoSetupStylerStyle; var BraceLevel: Integer);
var
  C: AnsiChar;
begin
  while not EndOfLine and not CurCharIn(Chars) do begin
    if BraceLevel = 0 then
      CommitStyle(NonConstStyle);
    C := CurChar;
    ConsumeChar(C);
    if C = '{' then begin
      if not ConsumeChar('{') then
        Inc(BraceLevel);
    end;
    if (C = '}') and (BraceLevel > 0) then begin
      Dec(BraceLevel);
      if BraceLevel = 0 then
        CommitStyle(stConstant);
    end;
  end;
end;

procedure TInnoSetupStyler.StyleNeeded;
var
  NewLineState: TInnoSetupStylerLineState;
  Section, NewSection: TInnoSetupStylerSection;
  SectionEnd: Boolean;
  S: TScintRawString;
begin
  NewLineState := TInnoSetupStylerLineState(LineState);
  if NewLineState.NextLineSection <> scNone then begin
    NewLineState.Section := NewLineState.NextLineSection;
    NewLineState.NextLineSection := scNone;
  end;
  Section := NewLineState.Section;

  PreStyleInlineISPPDirectives;

  SkipWhitespace;
  if (Section <> scCode) and ConsumeChar(';') then begin
    ConsumeAllRemaining;
    CommitStyle(stComment);
  end
  else if CurCharIs('/') and NextCharIs('/') then begin
    ConsumeAllRemaining;
    CommitStyleSq(stComment, not IsppInstalled and (Section <> scCode))
  end
  else if ConsumeChar('[') then begin
    SectionEnd := ConsumeChar('/');
    S := ConsumeString(AlphaUnderscoreChars);
    if ConsumeChar(']') then begin
      NewSection := MapSectionNameString(S);
      { Unknown section names and erroneously-placed end tags get squigglified }
      CommitStyleSq(stSection, (NewSection = scUnknown) or
        (SectionEnd and (NewSection <> Section)));
      if not SectionEnd then
        NewLineState.NextLineSection := NewSection;
    end
    else
      CommitStyleSqPending(stDefault);
    { Section tags themselves are not associated with any section }
    Section := scNone;
    SquigglifyUntilChars([], stDefault);
  end
  else if CurCharIs('#') then
    HandleCompilerDirective(False, -1, NewLineState.OpenCompilerDirectivesCount)
  else begin
    case Section of
      scUnknown: ;
      scThirdParty: ;
      scCode: HandleCodeSection(NewLineState.SpanState);
      scComponents: HandleParameterSection(ComponentsSectionParameters);
      scCustomMessages: HandleKeyValueSection(Section);
      scDirs: HandleParameterSection(DirsSectionParameters);
      scFiles: HandleParameterSection(FilesSectionParameters);
      scIcons: HandleParameterSection(IconsSectionParameters);
      scINI: HandleParameterSection(INISectionParameters);
      scInstallDelete: HandleParameterSection(DeleteSectionParameters);
      scLangOptions: HandleKeyValueSection(Section);
      scLanguages: HandleParameterSection(LanguagesSectionParameters);
      scMessages: HandleKeyValueSection(Section);
      scRegistry: HandleParameterSection(RegistrySectionParameters);
      scRun: HandleParameterSection(RunSectionParameters);
      scSetup: HandleKeyValueSection(Section);
      scTasks: HandleParameterSection(TasksSectionParameters);
      scTypes: HandleParameterSection(TypesSectionParameters);
      scUninstallDelete: HandleParameterSection(DeleteSectionParameters);
      scUninstallRun: HandleParameterSection(RunSectionParameters);
    end;
  end;

  NewLineState.Section := Section;
  LineState := TScintLineState(NewLineState);
end;

end.
