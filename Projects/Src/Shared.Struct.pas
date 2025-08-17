unit Shared.Struct;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Various records and other types that are shared by the ISCmplr, Setup,
  SetupLdr, and Uninst projects
}

interface

uses
  Windows, SHA256;

const
  SetupTitle = 'Inno Setup';
  SetupVersion = '6.5.2-dev';
  SetupBinVersion = (6 shl 24) + (5 shl 16) + (2 shl 8) + 0;

type
  TSetupID = array[0..63] of AnsiChar;
  TUninstallLogID = array[0..63] of AnsiChar;
  TMessagesHdrID = array[0..63] of AnsiChar;
  TMessagesLangOptionsID = array[1..8] of AnsiChar;
  TCompID = array[1..4] of AnsiChar;
  TDiskSliceID = array[1..8] of AnsiChar;
const
  { SetupID is used by the Setup program to check if the SETUP.0 file is
    compatible with it. If you make any modifications to the records in
    this file it's recommended you change SetupID. Any change will do (like
    changing the letters or numbers), as long as your format is
    unrecognizable by the standard Inno Setup. }
  SetupID: TSetupID = 'Inno Setup Setup Data (6.5.2)';
  UninstallLogID: array[Boolean] of TUninstallLogID =
    ('Inno Setup Uninstall Log (b)', 'Inno Setup Uninstall Log (b) 64-bit');
  MessagesHdrID: TMessagesHdrID = 'Inno Setup Messages (6.5.0) (u)';
  MessagesLangOptionsID: TMessagesLangOptionsID = '!mlo!001';
  ZLIBID: TCompID = 'zlb'#26;
  DiskSliceID: TDiskSliceID = 'idska32'#26;
type
  TSetupVersionDataVersion = packed record
    Build: Word;
    Minor, Major: Byte;
  end;
  TSetupVersionData = packed record
    WinVersion, NTVersion: Cardinal;
    NTServicePack: Word;
  end;
  TSetupHeaderOption = (shDisableStartupPrompt, shCreateAppDir,
    shAllowNoIcons, shAlwaysRestart, shAlwaysUsePersonalGroup,
    shEnableDirDoesntExistWarning,
    shPassword, shAllowRootDirectory, shDisableFinishedPage, shUsePreviousAppDir,
    shUsePreviousGroup, shUpdateUninstallLogAppName,
    shUsePreviousSetupType, shDisableReadyMemo, shAlwaysShowComponentsList,
    shFlatComponentsList, shShowComponentSizes, shUsePreviousTasks,
    shDisableReadyPage, shAlwaysShowDirOnReadyPage, shAlwaysShowGroupOnReadyPage,
    shAllowUNCPath, shUserInfoPage, shUsePreviousUserInfo,
    shUninstallRestartComputer, shRestartIfNeededByRun, shShowTasksTreeLines,
    shAllowCancelDuringInstall, shWizardImageStretch, shAppendDefaultDirName,
    shAppendDefaultGroupName, shSetupLogging,
    shSignedUninstaller, shUsePreviousLanguage, shDisableWelcomePage,
    shCloseApplications, shRestartApplications, shAllowNetworkDrive,
    shForceCloseApplications, shAppNameHasConsts, shUsePreviousPrivileges,
    shWizardResizable, shUninstallLogging);
  TSetupLanguageDetectionMethod = (ldUILanguage, ldLocale, ldNone);
  TSetupCompressMethod = (cmStored, cmZip, cmBzip, cmLZMA, cmLZMA2);
  TSetupKDFSalt = array[0..15] of Byte;
  TSetupEncryptionKey = array[0..31] of Byte;
  TSetupEncryptionNonce = record
    RandomXorStartOffset: Int64;
    RandomXorFirstSlice: Int32;
    RemainingRandom: array[0..2] of Int32;
  end;
  TSetupProcessorArchitecture = (paUnknown, paX86, paX64, paArm32, paArm64);
  TSetupProcessorArchitectures = set of TSetupProcessorArchitecture;
  TSetupDisablePage = (dpAuto, dpNo, dpYes);
  TSetupPrivilegesRequired = (prNone, prPowerUser, prAdmin, prLowest);
  TSetupPrivilegesRequiredOverride = (proCommandLine, proDialog);
  TSetupPrivilegesRequiredOverrides = set of TSetupPrivilegesRequiredOverride;
  TSetupWizardStyle = (wsClassic, wsModern);
const
  SetupProcessorArchitectureNames: array[TSetupProcessorArchitecture] of String =
    ('Unknown', 'x86', 'x64', 'Arm32', 'Arm64');

type
  { Should not contain strings }
  TSetupEncryptionHeader = packed record
    EncryptionUse: (euNone, euFiles, euFull);
    KDFSalt: TSetupKDFSalt;
    KDFIterations: Integer;
    BaseNonce: TSetupEncryptionNonce;
    PasswordTest: Integer;
  end;

const
  SetupHeaderStrings = 34;
  SetupHeaderAnsiStrings = 4;
type
  TSetupHeader = packed record
    AppName, AppVerName, AppId, AppCopyright, AppPublisher, AppPublisherURL,
      AppSupportPhone, AppSupportURL, AppUpdatesURL, AppVersion, DefaultDirName,
      DefaultGroupName, BaseFilename, UninstallFilesDir, UninstallDisplayName,
      UninstallDisplayIcon, AppMutex, DefaultUserInfoName, DefaultUserInfoOrg,
      DefaultUserInfoSerial, AppReadmeFile, AppContact, AppComments,
      AppModifyPath, CreateUninstallRegKey, Uninstallable, CloseApplicationsFilter,
      SetupMutex, ChangesEnvironment, ChangesAssociations,
      ArchitecturesAllowed, ArchitecturesInstallIn64BitMode, CloseApplicationsFilterExcludes,
      SevenZipLibraryName: String;
    LicenseText, InfoBeforeText, InfoAfterText, CompiledCodeText: AnsiString;
    NumLanguageEntries, NumCustomMessageEntries, NumPermissionEntries,
      NumTypeEntries, NumComponentEntries, NumTaskEntries, NumDirEntries,
      NumISSigKeyEntries, NumFileEntries, NumFileLocationEntries, NumIconEntries, NumIniEntries,
      NumRegistryEntries, NumInstallDeleteEntries, NumUninstallDeleteEntries,
      NumRunEntries, NumUninstallRunEntries: Integer;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    WizardStyle: TSetupWizardStyle;
    WizardSizePercentX, WizardSizePercentY: Integer;
    WizardImageAlphaFormat: (afIgnored, afDefined, afPremultiplied); // Must be same as Graphics.TAlphaFormat
    ExtraDiskSpaceRequired: Int64;
    SlicesPerDisk: Integer;
    UninstallLogMode: (lmAppend, lmNew, lmOverwrite);
    DirExistsWarning: (ddAuto, ddNo, ddYes);
    PrivilegesRequired: TSetupPrivilegesRequired;
    PrivilegesRequiredOverridesAllowed: TSetupPrivilegesRequiredOverrides;
    ShowLanguageDialog: (slYes, slNo, slAuto);
    LanguageDetectionMethod: TSetupLanguageDetectionMethod;
    CompressMethod: TSetupCompressMethod;
    DisableDirPage, DisableProgramGroupPage: TSetupDisablePage;
    UninstallDisplaySize: Int64;
    Options: set of TSetupHeaderOption;
  end;
const
  SetupPermissionEntryStrings = 0;
  SetupPermissionEntryAnsiStrings = 1;
type
  PSetupPermissionEntry = ^TSetupPermissionEntry;
  TSetupPermissionEntry = packed record
    Permissions: AnsiString;  { an array of TGrantPermissionEntry's }
  end;
const
  SetupLanguageEntryStrings = 6;
  SetupLanguageEntryAnsiStrings = 4;
type
  PSetupLanguageEntry = ^TSetupLanguageEntry;
  TSetupLanguageEntry = packed record
    Name, LanguageName, DialogFontName, TitleFontName, WelcomeFontName,
      CopyrightFontName: String;
    Data, LicenseText, InfoBeforeText, InfoAfterText: AnsiString;
    LanguageID: Cardinal;
    DialogFontSize: Integer;
    TitleFontSize: Integer;
    WelcomeFontSize: Integer;
    CopyrightFontSize: Integer;
    RightToLeft: Boolean;
  end;
const
  SetupCustomMessageEntryStrings = 2;
  SetupCustomMessageEntryAnsiStrings = 0;
type
  PSetupCustomMessageEntry = ^TSetupCustomMessageEntry;
  TSetupCustomMessageEntry = packed record
    Name, Value: String;
    LangIndex: Integer;
  end;
const
  SetupTypeEntryStrings = 4;
  SetupTypeEntryAnsiStrings = 0;
type
  TSetupTypeOption = (toIsCustom);
  TSetupTypeOptions = set of TSetupTypeOption;
  TSetupTypeType = (ttUser, ttDefaultFull, ttDefaultCompact, ttDefaultCustom);
  PSetupTypeEntry = ^TSetupTypeEntry;
  TSetupTypeEntry = packed record
    Name, Description, Languages, CheckOnce: String;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: TSetupTypeOptions;
    Typ: TSetupTypeType;
    { internally used: }
    Size: Int64;
  end;
const
  SetupComponentEntryStrings = 5;
  SetupComponentEntryAnsiStrings = 0;
type
  PSetupComponentEntry = ^TSetupComponentEntry;
  TSetupComponentEntry = packed record
    Name, Description, Types, Languages, CheckOnce: String;
    ExtraDiskSpaceRequired: Int64;
    Level: Integer;
    Used: Boolean;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: set of (coFixed, coRestart, coDisableNoUninstallWarning,
      coExclusive, coDontInheritCheck);
    { internally used: }
    Size: Int64;
  end;
const
  SetupTaskEntryStrings = 6;
  SetupTaskEntryAnsiStrings = 0;
type
  PSetupTaskEntry = ^TSetupTaskEntry;
  TSetupTaskEntry = packed record
    Name, Description, GroupDescription, Components, Languages, Check: String;
    Level: Integer;
    Used: Boolean;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: set of (toExclusive, toUnchecked, toRestart, toCheckedOnce,
      toDontInheritCheck);
  end;
const
  SetupDirEntryStrings = 7;
  SetupDirEntryAnsiStrings = 0;
type
  PSetupDirEntry = ^TSetupDirEntry;
  TSetupDirEntry = packed record
    DirName: String;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    Attribs: Integer;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    PermissionsEntry: Smallint;
    Options: set of (doUninsNeverUninstall, doDeleteAfterInstall,
      doUninsAlwaysUninstall, doSetNTFSCompression, doUnsetNTFSCompression);
  end;
const
  SetupISSigKeyEntryStrings = 3;
  SetupISSigKeyEntryAnsiStrings = 0;
type
  PSetupISSigKeyEntry = ^TSetupISSigKeyEntry;
  TSetupISSigKeyEntry = packed record
    PublicX, PublicY, RuntimeID: String;
  end;
const
  SetupFileEntryStrings = 15;
  SetupFileEntryAnsiStrings = 1;
type
  PSetupFileEntry = ^TSetupFileEntry;
  TSetupFileVerificationType = (fvNone, fvHash, fvISSig);
  TSetupFileVerification = packed record
    ISSigAllowedKeys: AnsiString; { Must be first }
    Hash: TSHA256Digest;
    Typ: TSetupFileVerificationType;
  end;
  TSetupFileEntry = packed record
    SourceFilename, DestName, InstallFontName, StrongAssemblyName, Components,
    Tasks, Languages, Check, AfterInstall, BeforeInstall, Excludes,
    DownloadISSigSource, DownloadUserName, DownloadPassword, ExtractArchivePassword: String;
    Verification: TSetupFileVerification; { Must be first after strings }
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    LocationEntry: Integer;
    Attribs: Integer;
    ExternalSize: Int64;
    PermissionsEntry: Smallint;
    Options: set of (foConfirmOverwrite, foUninsNeverUninstall, foRestartReplace,
      foDeleteAfterInstall, foRegisterServer, foRegisterTypeLib, foSharedFile,
      foCompareTimeStamp, foFontIsntTrueType,
      foSkipIfSourceDoesntExist, foOverwriteReadOnly, foOverwriteSameVersion,
      foCustomDestName, foOnlyIfDestFileExists, foNoRegError,
      foUninsRestartDelete, foOnlyIfDoesntExist, foIgnoreVersion,
      foPromptIfOlder, foDontCopy, foUninsRemoveReadOnly,
      foRecurseSubDirsExternal, foReplaceSameVersionIfContentsDiffer,
      foDontVerifyChecksum, foUninsNoSharedFilePrompt, foCreateAllSubDirs,
      fo32Bit, fo64Bit, foExternalSizePreset, foSetNTFSCompression,
      foUnsetNTFSCompression, foGacInstall, foDownload,
      foExtractArchive);
    FileType: (ftUserFile, ftUninstExe);
  end;
const
  SetupFileLocationEntryStrings = 0;
  SetupFileLocationEntryAnsiStrings = 0;
type
  PSetupFileLocationEntry = ^TSetupFileLocationEntry;
  TSetupFileLocationEntry = packed record
    FirstSlice, LastSlice: Integer;
    StartOffset: Int64;
    ChunkSuboffset: Int64;
    OriginalSize: Int64;
    ChunkCompressedSize: Int64;
    SHA256Sum: TSHA256Digest;
    SourceTimeStamp: TFileTime;
    FileVersionMS, FileVersionLS: DWORD;
    Flags: set of (floVersionInfoValid, floTimeStampInUTC, floCallInstructionOptimized,
      floChunkEncrypted, floChunkCompressed);
  end;
const
  SetupIconEntryStrings = 13;
  SetupIconEntryAnsiStrings = 0;
type
  TSetupIconCloseOnExit = (icNoSetting, icYes, icNo);
  PSetupIconEntry = ^TSetupIconEntry;
  TSetupIconEntry = packed record
    IconName, Filename, Parameters, WorkingDir, IconFilename, Comment: String;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    AppUserModelID: String;
    AppUserModelToastActivatorCLSID: TGUID;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    IconIndex, ShowCmd: Integer;
    CloseOnExit: TSetupIconCloseOnExit;
    HotKey: Word;
    Options: set of (ioUninsNeverUninstall, ioCreateOnlyIfFileExists,
      ioUseAppPaths, ioExcludeFromShowInNewInstall,
      ioPreventPinning, ioHasAppUserModelToastActivatorCLSID);
  end;
const
  SetupIniEntryStrings = 10;
  SetupIniEntryAnsiStrings = 0;
type
  PSetupIniEntry = ^TSetupIniEntry;
  TSetupIniEntry = packed record
    Filename, Section, Entry, Value: String;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: set of (ioCreateKeyIfDoesntExist, ioUninsDeleteEntry,
      ioUninsDeleteEntireSection, ioUninsDeleteSectionIfEmpty,
      { internally used: }
      ioHasValue);
  end;
const
  SetupRegistryEntryStrings = 9;
  SetupRegistryEntryAnsiStrings = 0;
type
  PSetupRegistryEntry = ^TSetupRegistryEntry;
  TSetupRegistryEntry = packed record
    Subkey, ValueName, ValueData: String;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    RootKey: HKEY;
    PermissionsEntry: Smallint;
    Typ: (rtNone, rtString, rtExpandString, rtDWord, rtBinary, rtMultiString, rtQWord);
    Options: set of (roCreateValueIfDoesntExist, roUninsDeleteValue,
      roUninsClearValue, roUninsDeleteEntireKey, roUninsDeleteEntireKeyIfEmpty,
      roPreserveStringType, roDeleteKey, roDeleteValue, roNoError,
      roDontCreateKey, ro32Bit, ro64Bit);
  end;
const
  SetupDeleteEntryStrings = 7;
  SetupDeleteEntryAnsiStrings = 0;
type
  TSetupDeleteType = (dfFiles, dfFilesAndOrSubdirs, dfDirIfEmpty);
  PSetupDeleteEntry = ^TSetupDeleteEntry;
  TSetupDeleteEntry = packed record
    Name: String;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    DeleteType: TSetupDeleteType;
  end;
const
  SetupRunEntryStrings = 13;
  SetupRunEntryAnsiStrings = 0;
type
  PSetupRunEntry = ^TSetupRunEntry;
  TSetupRunEntry = packed record
    Name, Parameters, WorkingDir, RunOnceId, StatusMsg, Verb: String;
    Description, Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    ShowCmd: Integer;
    Wait: (rwWaitUntilTerminated, rwNoWait, rwWaitUntilIdle);
    Options: set of (roShellExec, roSkipIfDoesntExist,
      roPostInstall, roUnchecked, roSkipIfSilent, roSkipIfNotSilent,
      roHideWizard, roRun32Bit, roRun64Bit, roRunAsOriginalUser,
      roDontLogParameters, roLogOutput);
  end;

const
  MaxGrantPermissionEntries = 32;  { must keep in synch with Helper.c }
type
  { TGrantPermissionEntry is stored inside string fields named 'Permissions' }
  TGrantPermissionSid = record  { must keep in synch with Helper.c }
    Authority: TSIDIdentifierAuthority;
    SubAuthCount: Byte;
    SubAuth: array[0..1] of DWORD;
  end;
  TGrantPermissionEntry = record  { must keep in synch with Helper.c }
    Sid: TGrantPermissionSid;
    AccessMask: DWORD;
  end;

  { A TDiskSliceHeader record follows DiskSliceID in a SETUP-*.BIN file }
  TDiskSliceHeader = packed record
    TotalSize: Int64;
  end;

  { A TMessageHeader record follows MessagesHdrID in a SETUP.MSG file }
  TMessagesHeader = packed record
    NumMessages: Cardinal;
    TotalSize: Cardinal;
    NotTotalSize: Cardinal;
    CRCMessages: Longint;
  end;

  { TSetupLdrOffsetTable is stored inside SetupLdr's SetupLdrOffsetTableResID
    RCDATA resource }
  PSetupLdrOffsetTable = ^TSetupLdrOffsetTable;
  TSetupLdrOffsetTable = record
    ID: array[1..12] of AnsiChar;   { = SetupLdrOffsetTableID }
    Version: UInt32;                { = SetupLdrOffsetTableVersion }
    TotalSize: Int64;               { Minimum expected size of setup.exe }
    OffsetEXE: Int64;               { Offset of compressed setup.e32 }
    UncompressedSizeEXE: UInt32;    { Size of setup.e32 before compression }
    CRCEXE: Int32;                  { CRC of setup.e32 before compression }
    Offset0: Int64;                 { Offset of embedded setup-0.bin data }
    Offset1: Int64;                 { Offset of embedded setup-1.bin data,
                                      or 0 when DiskSpanning=yes }
    ReservedPadding: UInt32;        { Not set (just provides padding) }
    TableCRC: Int32;                { CRC of all prior fields in this record }
  end;

  { TMessagesLangOptions is a simplified version of TSetupLanguageEntry that
    is used by the uninstaller and RegSvr }
  TMessagesLangOptions = packed record
    ID: TMessagesLangOptionsID;
    DialogFontName: array[0..31] of Char;
    DialogFontSize: Integer;
    Flags: set of (lfRightToLeft);
  end;

  TUninstallerMsgTail = packed record
    ID: Longint;
    Offset: Longint;
  end;
const
  SetupLdrOffsetTableResID = 11111;
  SetupLdrOffsetTableID = 'rDlPtS'#$CD#$E6#$D7#$7B#$0B#$2A;
  SetupLdrOffsetTableVersion = 2;
  SetupExeModeOffset = $30;
  SetupExeModeUninstaller = $6E556E49;
  SetupExeModeRegSvr = $53526E49;
  UninstallerMsgTailID = $67734D49;

implementation

end.
