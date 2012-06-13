unit Struct;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Various records and other types that are shared by the ISCmplr, Setup,
  SetupLdr, and Uninst projects
}

interface

uses
  Windows, Int64Em, SHA1;

const
  SetupTitle = 'Inno Setup';
  SetupVersion = '5.5.1 '{$IFDEF UNICODE}+'(u)'{$ELSE}+'(a)'{$ENDIF};
  SetupBinVersion = (5 shl 24) + (5 shl 16) + (1 shl 8) + 0;

type
  TSetupID = array[0..63] of AnsiChar;
  TUninstallLogID = array[0..63] of AnsiChar;
  TMessagesHdrID = array[0..63] of AnsiChar;
  TMessagesLangOptionsID = array[1..8] of AnsiChar;
  TCompID = array[1..4] of AnsiChar;
  TDiskSliceID = array[1..8] of AnsiChar;
const
  { SetupID is used by the Setup program to check if the SETUP.0 file is
    compatible with with it. If you make any modifications to the records in
    this file it's recommended you change SetupID. Any change will do (like
    changing the letters or numbers), as long as your format is
    unrecognizable by the standard Inno Setup. }
  SetupID: TSetupID = 'Inno Setup Setup Data (5.5.0)'{$IFDEF UNICODE}+' (u)'{$ENDIF};
  UninstallLogID: array[Boolean] of TUninstallLogID =
    ('Inno Setup Uninstall Log (b)', 'Inno Setup Uninstall Log (b) 64-bit');
  MessagesHdrID: TMessagesHdrID = 'Inno Setup Messages (5.5.0)'{$IFDEF UNICODE}+' (u)'{$ENDIF};
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
    shWindowVisible, shWindowShowCaption, shWindowResizable,
    shWindowStartMaximized, shEnableDirDoesntExistWarning,
    shPassword, shAllowRootDirectory, shDisableFinishedPage,
    shChangesAssociations, shUsePreviousAppDir,
    shBackColorHorizontal, shUsePreviousGroup, shUpdateUninstallLogAppName,
    shUsePreviousSetupType, shDisableReadyMemo, shAlwaysShowComponentsList,
    shFlatComponentsList, shShowComponentSizes, shUsePreviousTasks,
    shDisableReadyPage, shAlwaysShowDirOnReadyPage, shAlwaysShowGroupOnReadyPage,
    shAllowUNCPath, shUserInfoPage, shUsePreviousUserInfo,
    shUninstallRestartComputer, shRestartIfNeededByRun, shShowTasksTreeLines,
    shAllowCancelDuringInstall, shWizardImageStretch, shAppendDefaultDirName,
    shAppendDefaultGroupName, shEncryptionUsed, shChangesEnvironment,
    {$IFNDEF UNICODE}shShowUndisplayableLanguages, {$ENDIF}shSetupLogging,
    shSignedUninstaller, shUsePreviousLanguage, shDisableWelcomePage,
    shCloseApplications, shRestartApplications, shAllowNetworkDrive);
  TSetupLanguageDetectionMethod = (ldUILanguage, ldLocale, ldNone);
  TSetupCompressMethod = (cmStored, cmZip, cmBzip, cmLZMA, cmLZMA2);
  TSetupSalt = array[0..7] of Byte;
  TSetupProcessorArchitecture = (paUnknown, paX86, paX64, paIA64);
  TSetupProcessorArchitectures = set of TSetupProcessorArchitecture;
  TSetupDisablePage = (dpAuto, dpNo, dpYes);
const
  SetupProcessorArchitectureNames: array[TSetupProcessorArchitecture] of String =
    ('Unknown', 'x86', 'x64', 'Itanium');

const
  SetupHeaderStrings = 27;
  SetupHeaderAnsiStrings = 4;
type
  TSetupHeader = packed record
    AppName, AppVerName, AppId, AppCopyright, AppPublisher, AppPublisherURL,
      AppSupportPhone, AppSupportURL, AppUpdatesURL, AppVersion, DefaultDirName,
      DefaultGroupName, BaseFilename, UninstallFilesDir, UninstallDisplayName,
      UninstallDisplayIcon, AppMutex, DefaultUserInfoName, DefaultUserInfoOrg,
      DefaultUserInfoSerial, AppReadmeFile, AppContact, AppComments,
      AppModifyPath, CreateUninstallRegKey, Uninstallable,
      CloseApplicationsFilter: String;
    LicenseText, InfoBeforeText, InfoAfterText, CompiledCodeText: AnsiString;
{$IFNDEF UNICODE}
    LeadBytes: set of AnsiChar;
{$ENDIF}
    NumLanguageEntries, NumCustomMessageEntries, NumPermissionEntries,
      NumTypeEntries, NumComponentEntries, NumTaskEntries, NumDirEntries,
      NumFileEntries, NumFileLocationEntries, NumIconEntries, NumIniEntries,
      NumRegistryEntries, NumInstallDeleteEntries, NumUninstallDeleteEntries,
      NumRunEntries, NumUninstallRunEntries: Integer;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    BackColor, BackColor2, WizardImageBackColor: Longint;
    PasswordHash: TSHA1Digest;
    PasswordSalt: TSetupSalt;
    ExtraDiskSpaceRequired: Integer64;
    SlicesPerDisk: Integer;
    UninstallLogMode: (lmAppend, lmNew, lmOverwrite);
    DirExistsWarning: (ddAuto, ddNo, ddYes);
    PrivilegesRequired: (prNone, prPowerUser, prAdmin, prLowest);
    ShowLanguageDialog: (slYes, slNo, slAuto);
    LanguageDetectionMethod: TSetupLanguageDetectionMethod;
    CompressMethod: TSetupCompressMethod;
    ArchitecturesAllowed, ArchitecturesInstallIn64BitMode: TSetupProcessorArchitectures;
    DisableDirPage, DisableProgramGroupPage: TSetupDisablePage;
    UninstallDisplaySize: Integer64;
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
{$IFNDEF UNICODE}
    { Note: LanguageName is Unicode }
{$ENDIF}
    Name, LanguageName, DialogFontName, TitleFontName, WelcomeFontName,
      CopyrightFontName: String;
    Data, LicenseText, InfoBeforeText, InfoAfterText: AnsiString;
    LanguageID{$IFNDEF UNICODE}, LanguageCodePage{$ENDIF}: Cardinal;
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
    Name, Description, Languages, Check: String;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: TSetupTypeOptions;
    Typ: TSetupTypeType;
    { internally used: }
    Size: Integer64;
  end;
const
  SetupComponentEntryStrings = 5;
  SetupComponentEntryAnsiStrings = 0;
type
  PSetupComponentEntry = ^TSetupComponentEntry;
  TSetupComponentEntry = packed record
    Name, Description, Types, Languages, Check: String;
    ExtraDiskSpaceRequired: Integer64;
    Level: Integer;
    Used: Boolean;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: set of (coFixed, coRestart, coDisableNoUninstallWarning,
      coExclusive, coDontInheritCheck);
    { internally used: }
    Size: Integer64;
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
  SetupFileEntryStrings = 10;
  SetupFileEntryAnsiStrings = 0;
type
  PSetupFileEntry = ^TSetupFileEntry;
  TSetupFileEntry = packed record
    SourceFilename, DestName, InstallFontName, StrongAssemblyName: String;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    LocationEntry: Integer;
    Attribs: Integer;
    ExternalSize: Integer64;
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
      foUnsetNTFSCompression, foGacInstall);
    FileType: (ftUserFile, ftUninstExe);
  end;
const
  SetupFileLocationEntryStrings = 0;
  SetupFileLocationEntryAnsiStrings = 0;
type
  PSetupFileLocationEntry = ^TSetupFileLocationEntry;
  TSetupFileLocationEntry = packed record
    FirstSlice, LastSlice: Integer;
    StartOffset: Longint;
    ChunkSuboffset: Integer64;
    OriginalSize: Integer64;
    ChunkCompressedSize: Integer64;
    SHA1Sum: TSHA1Digest;
    TimeStamp: TFileTime;
    FileVersionMS, FileVersionLS: DWORD;
    Flags: set of (foVersionInfoValid, foVersionInfoNotValid, foTimeStampInUTC,
      foIsUninstExe, foCallInstructionOptimized, foTouch, foChunkEncrypted,
      foChunkCompressed, foSolidBreak);
  end;
  TSetupIconCloseOnExit = (icNoSetting, icYes, icNo);
const
  SetupIconEntryStrings = 13;
  SetupIconEntryAnsiStrings = 0;
type
  PSetupIconEntry = ^TSetupIconEntry;
  TSetupIconEntry = packed record
    IconName, Filename, Parameters, WorkingDir, IconFilename, Comment: String;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    AppUserModelID: String;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    IconIndex, ShowCmd: Integer;
    CloseOnExit: TSetupIconCloseOnExit;
    HotKey: Word;
    Options: set of (ioUninsNeverUninstall, ioCreateOnlyIfFileExists,
      ioUseAppPaths, ioFolderShortcut, ioExcludeFromShowInNewInstall,
      ioPreventPinning);
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
      roHideWizard, roRun32Bit, roRun64Bit, roRunAsOriginalUser);
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
    TotalSize: Cardinal;
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
  TSetupLdrOffsetTable = packed record
    ID: array[1..12] of AnsiChar;   { = SetupLdrOffsetTableID }
    Version: LongWord;              { = SetupLdrOffsetTableVersion }
    TotalSize: LongWord;            { Minimum expected size of setup.exe }
    OffsetEXE: LongWord;            { Offset of compressed setup.e32 }
    UncompressedSizeEXE: LongWord;  { Size of setup.e32 before compression }
    CRCEXE: Longint;                { CRC of setup.e32 before compression }
    Offset0: LongWord;              { Offset of embedded setup-0.bin data }
    Offset1: LongWord;              { Offset of embedded setup-1.bin data,
                                      or 0 when DiskSpanning=yes }
    TableCRC: Longint;              { CRC of all prior fields in this record }
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
  SetupLdrOffsetTableVersion = 1;
  SetupExeModeOffset = $30;
  SetupExeModeUninstaller = $6E556E49;
  SetupExeModeRegSvr = $53526E49;
  UninstallerMsgTailID = $67734D49;

implementation

end.
