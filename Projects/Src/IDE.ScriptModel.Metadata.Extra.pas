unit IDE.ScriptModel.Metadata.Extra;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Extra metadata tables and functions, not used by IDE.ScriptModel.pas
}

interface

uses
  TypInfo,
  ScintEdit,
  Shared.SetupSectionDirectives;

const
  AlphaChars = ['A'..'Z', 'a'..'z'];
  AlphaUnderscoreChars = AlphaChars + ['_'];

  WhitespaceChars = [#0..' '];
  DigitChars = ['0'..'9'];
  HexDigitChars = DigitChars + ['A'..'F', 'a'..'f'];
  AlphaDigitChars = AlphaChars + DigitChars;
  AlphaDigitUnderscoreChars = AlphaChars + DigitChars + ['_'];

  PascalIdentFirstChars = AlphaUnderscoreChars;
  PascalIdentChars = AlphaDigitUnderscoreChars;

  ISPPIdentFirstChars = AlphaUnderscoreChars;
  ISPPIdentChars = AlphaDigitUnderscoreChars;

  InnoSetupSectionPrefixLength = 2;

type
  TInnoSetupSection = (
    scNone,            { Not inside a section (start of file, or previous section was closed )
                         Section tags themselves are not associated with any section! }
    scUnknown,         { Inside an unrecognized section }
    scThirdParty,      { Inside a '_' section (reserved for third-party tools) }
    scCode,
    scCodeBlock,       { Block headers themselves are still associated with scCode }
    scComponents,
    scCustomMessages,
    scDirs,
    scISSigKeys,
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

  TISPPDirective = record
    Name: TScintRawString;
    RequiresParameter: Boolean;
    OpenCountChange: ShortInt;
  end;

var
  ISPPDirectives: array of TISPPDirective; { Initialized below. Note this list is *not* only used to build a word list, but also in HandleCompilerDirective. }

const
  ISPPPragmaSubDirectives: array of TScintRawString = [
    'error', 'include', 'inlineend', 'inlinestart', 'message',
    'option', 'parseroption', 'spansymbol', 'verboselevel', 'warning'
  ];

  { The following and some others below are not used by StyleNeeded and therefore
    simply of type AnsiString instead of TScintRawString }
  ConstantsWithParam: array of AnsiString = [
    'cm', 'code', 'drive', 'ini', 'param', 'reg'
  ];

  Constants: array of AnsiString = [
    { Doesn't include constants with non-word chars.
      Also doesn't include the *32 and *64 variants like commonpf32 or dotnet2064 }
    '{', 'app', 'win', 'sys', 'sysnative', 'syswow64', 'src', 'sd', 'commonpf',
    'commoncf', 'tmp', 'commonfonts', 'dao', 'dotnet11', 'dotnet20', 'dotnet40',
    'group', 'localappdata', 'userappdata', 'commonappdata', 'usercf',
    'userdesktop', 'commondesktop', 'userdocs', 'commondocs', 'userfavorites',
    'userfonts', 'userpf', 'userprograms', 'commonprograms', 'usersavedgames',
    'usersendto', 'userstartmenu', 'commonstartmenu', 'userstartup',
    'commonstartup', 'usertemplates', 'commontemplates', 'autoappdata',
    'autocf', 'autodesktop', 'autodocs', 'autofonts', 'autopf', 'autoprograms',
    'autostartmenu', 'autostartup', 'autotemplates', 'cmd', 'computername',
    'groupname', 'wizardhwnd', 'language', 'srcexe', 'uninstallexe',
    'sysuserinfoname', 'sysuserinfoorg', 'userinfoname', 'userinfoorg',
    'userinfoserial', 'username', 'log'
  ];

  ISPPPredefinedVariables: array of AnsiString = [
    { Doesn't include predefined variables without a value, like __WIN32__ }
    { From TPreprocessor.LookupPredefined - excludes __FILE__, 'PREPROCVER',
      and __(P)OPT_*__ }
    '__FILENAME__', '__PATHFILENAME__', '__DIR__', '__LINE__', '__INCLUDE__',
    { From ISPreprocessScript - excludes 'Ver' }
    'CompilerPath', 'SourcePath', 'SysPath',
    { Special }
    '__COUNTER__'
  ];

  ISPPFunctions: array of AnsiString = [
    { Excludes deprecated aliases and undocumented functions.
      Includes void functions because they work with for example #expr,
      and because comma expressions make them work with for example #define. }
    { From ISPPBuiltins.iss }
    'int TypeOf2(any Expr)',
    'str GetFileCompanyString(str FileName)',
    'str GetFileDescriptionString(str FileName)',
    'str GetFileVersionString(str FileName)',
    'str GetFileCopyrightString(str FileName)',
    'str GetFileOriginalFilenameString(str FileName)',
    'str GetFileProductVersionString(str FileName)',
    'str GetVersionComponents(str FileName, *Major, *Minor, *Revision, *Build)',
    'str GetPackedVersion(str FileName, *Version)',
    'str GetVersionNumbers(str FileName, *VersionMS, *VersionLS)',
    'int PackVersionNumbers(int VersionMS, int VersionLS)',
    'int PackVersionComponents(int Major, int Minor, int Revision, int Build)',
    'void UnpackVersionNumbers(int Version, *VersionMS, *VersionLS)',
    'void UnpackVersionComponents(int Version, *Major, *Minor, *Revision, *Build)',
    'str VersionToStr(int Version)',
    'int StrToVersion(str Version)',
    'int EncodeVer(int Major, int Minor, int Revision = 0, int Build = -1)',
    'str DecodeVer(int Version, int Digits = 3)',
    'int FindSection(str Section = "Files")',
    'int FindSectionEnd(str Section = "Files")',
    'int FindCode',
    'str ExtractFilePath(str PathName)',
    'str ExtractFileDir(str PathName)',
    'str ExtractFileExt(str PathName)',
    'str ExtractFileName(str PathName)',
    'str ChangeFileExt(str FileName, str NewExt)',
    'str RemoveFileExt(str FileName)',
    'str AddBackslash(str S)',
    'str RemoveBackslashUnlessRoot(str S)',
    'void Delete(str* S, int Index, int Count = MaxInt)',
    'void Insert(str* S, int Index, str Substr)',
    'int YesNo(str S)',
    'int IsDirSet(str SetupDirective)',
    'int Power(int X, int P = 2)',
    'int Min(int A, int B, int C = MaxInt)',
    'int Max(int A, int B, int C = MinInt)',
    'int SameText(str S1, str S2)',
    'void EmitLanguagesSection',
    { From RegisterFunction - excludes ReadEnv }
    'int Int(any Value, int? Default)',
    'str Str(any Value)',
    'int FileExists(str FileName)',
    'int DirExists(str DirName)',
    'int ForceDirectories(str DirPath)',
    'int FileSize(str FileName)',
    'str ReadIni(str FileName, str Section, str Key, str? Default)',
    'void WriteIni(str FileName, str Section, str Key, any Value)',
    'any ReadReg(int RootKey, str SubKey, str? Name, any? Default)',
    'int Exec(str Filename, str? Params, str? WorkingDir, int? Wait, int? ShowCmd)',
    'str ExecAndGetFirstLine(str Filename, str? Params, str? WorkingDir)',
    'str Copy(str S, int Index, int? Count)',
    'int Pos(str SubStr, str S)',
    'int RPos(str SubStr, str S)',
    'int Len(str S)',
    'str GetVersionNumbersString(str FileName)',
    'int ComparePackedVersion(int Version1, int Version2)',
    'int SamePackedVersion(int Version1, int Version2)',
    'str GetStringFileInfo(str FileName, str StringName, int? LangCodePage)',
    'void SaveToFile(str FileName)',
    'int Find(int StartLine, str Str1, int? Flags1, str? Str2, int? Flags2, str? Str3, int? Flags3)',
    'str SetupSetting(str DirectiveName)',
    'void SetSetupSetting(str DirectiveName, str Value)',
    'str LowerCase(str S)',
    'str UpperCase(str S)',
    'int EntryCount(str Section)',
    'str GetEnv(str Name)',
    'void DeleteFile(str FileName)',
    'int DeleteFileNow(str FileName)',
    'int CopyFile(str ExistingFile, str NewFile)',
    'int FindFirst(str Pattern, int Attrs)',
    'int FindNext(int Handle)',
    'str FindGetFileName(int Handle)',
    'void FindClose(int Handle)',
    'int FileOpen(str FileName)',
    'str FileRead(int Handle)',
    'void FileReset(int Handle)',
    'int FileEof(int Handle)',
    'void FileClose(int Handle)',
    'int SaveStringToFile(str Filename, str S, int? Append, int? UTF8)',
    'str GetDateTimeString(str DateTimeFormat, str? DateSeparator, str? TimeSeparator)',
    'str GetFileDateTimeString(str Filename, str DateTimeFormat, str? DateSeparator, str? TimeSeparator)',
    'str GetMD5OfFile(str FileName)',
    'str GetMD5OfString(str S)',
    'str GetMD5OfUnicodeString(str S)',
    'str GetSHA1OfFile(str FileName)',
    'str GetSHA1OfString(str S)',
    'str GetSHA1OfUnicodeString(str S)',
    'str GetSHA256OfFile(str FileName)',
    'str GetSHA256OfString(str S)',
    'str GetSHA256OfUnicodeString(str S)',
    'str Format(str Format, ...)',
    'str Trim(str S)',
    'str StringChange(str S, str OldPattern, str NewPattern)',
    'int IsWin64',
    'void Message(str S)',
    'void Warning(str S)',
    'void Error(str S)',
    'str AddQuotes(str S)',
    'int SameStr(str S1, str S2)',
    'int Is64BitPEImage(str FileName)',
    { Special }
    'int Defined(<ident>)',
    'int TypeOf(<ident>)',
    'int DimOf(<ident>)'
  ];

  ISPPConstants: array of AnsiString = [
    { From TPreprocessor.LookupPredefined and ISPreprocessScript - these are
      predefined variables but with a constant value }
    'PREPROCVER', 'Ver',
    { From ISPPBuiltins.iss }
    { General - excludes 'void' }
    'NewLine', 'Tab', 'True', 'False', 'Yes', 'No', 'MaxInt', 'MinInt', 'NULL',
    { TypeOf constants }
    'TYPE_ERROR', 'TYPE_NULL', 'TYPE_INTEGER', 'TYPE_STRING', 'TYPE_MACRO', 'TYPE_FUNC', 'TYPE_ARRAY',
    { ReadReg constants }
    'HKEY_CLASSES_ROOT', 'HKEY_CURRENT_USER', 'HKEY_LOCAL_MACHINE', 'HKEY_USERS',
    'HKEY_CURRENT_CONFIG',
    'HKEY_CLASSES_ROOT_64', 'HKEY_CURRENT_USER_64', 'HKEY_LOCAL_MACHINE_64',
    'HKEY_USERS_64', 'HKEY_CURRENT_CONFIG_64',
    'HKEY_CLASSES_ROOT_32', 'HKEY_CURRENT_USER_32', 'HKEY_LOCAL_MACHINE_32',
    'HKEY_USERS_32', 'HKEY_CURRENT_CONFIG_32',
    'HKCR', 'HKCU', 'HKLM', 'HKU', 'HKCC',
    'HKCR64', 'HKCU64', 'HKLM64', 'HKU64', 'HKCC64',
    'HKCR32', 'HKCU32', 'HKLM32', 'HKU32', 'HKCC32',
    { Exec constants }
    'SW_HIDE', 'SW_SHOWNORMAL', 'SW_NORMAL', 'SW_SHOWMINIMIZED', 'SW_SHOWMAXIMIZED',
    'SW_MAXIMIZE', 'SW_SHOWNOACTIVATE', 'SW_SHOW', 'SW_MINIMIZE', 'SW_SHOWMINNOACTIVE',
    'SW_SHOWNA', 'SW_RESTORE', 'SW_SHOWDEFAULT', 'SW_MAX',
    { Find constants }
    'FIND_MATCH', 'FIND_BEGINS', 'FIND_ENDS', 'FIND_CONTAINS', 'FIND_CASESENSITIVE',
    'FIND_SENSITIVE', 'FIND_AND', 'FIND_OR', 'FIND_NOT', 'FIND_TRIM',
    { FindFirst constants }
    'faReadOnly', 'faHidden', 'faSysFile', 'faVolumeID', 'faDirectory', 'faArchive',
    'faSymLink', 'faAnyFile',
    { GetStringFileInfo standard names }
    'COMPANY_NAME', 'FILE_DESCRIPTION', 'FILE_VERSION', 'INTERNAL_NAME', 'LEGAL_COPYRIGHT',
    'ORIGINAL_FILENAME', 'PRODUCT_NAME', 'PRODUCT_VERSION'
  ];

  PascalConstants: array of AnsiString = [
    { ROPS - should not include ScriptClasses constants, see below }
    'varEmpty', 'varNull', 'varSmallInt', 'varInteger', 'varSingle', 'varDouble',
    'varCurrency', 'varDate', 'varOleStr', 'varDispatch', 'varError', 'varBoolean',
    'varVariant', 'varUnknown', 'varShortInt', 'varByte', 'varWord', 'varLongWord',
    'varInt64', 'varStrArg', 'varAny', 'varString', 'varTypeMask', 'varArray',
    'varByRef', 'varUString', 'False', 'True',
    { ScriptFunc }
    'MaxInt', 'wpWelcome', 'wpLicense', 'wpPassword', 'wpInfoBefore',
    'wpUserInfo', 'wpSelectDir', 'wpSelectComponents', 'wpSelectProgramGroup',
    'wpSelectTasks', 'wpReady', 'wpPreparing', 'wpInstalling', 'wpInfoAfter',
    'wpFinished', 'MB_OK', 'MB_OKCANCEL', 'MB_ABORTRETRYIGNORE', 'MB_YESNOCANCEL',
    'MB_YESNO', 'MB_RETRYCANCEL', 'MB_DEFBUTTON1', 'MB_DEFBUTTON2', 'MB_DEFBUTTON3',
    'MB_SETFOREGROUND', 'IDOK', 'IDCANCEL', 'IDABORT', 'IDRETRY', 'IDIGNORE',
    'IDYES', 'IDNO', 'HWND_BROADCAST', 'HKEY_AUTO', 'HKEY_AUTO_32', 'HKEY_AUTO_64',
    'HKEY_CLASSES_ROOT', 'HKEY_CLASSES_ROOT_32', 'HKEY_CLASSES_ROOT_64',
    'HKEY_CURRENT_USER', 'HKEY_CURRENT_USER_32', 'HKEY_CURRENT_USER_64',
    'HKEY_LOCAL_MACHINE', 'HKEY_LOCAL_MACHINE_32', 'HKEY_LOCAL_MACHINE_64',
    'HKEY_USERS', 'HKEY_USERS_32', 'HKEY_USERS_64', 'HKEY_PERFORMANCE_DATA',
    'HKEY_CURRENT_CONFIG', 'HKEY_CURRENT_CONFIG_32', 'HKEY_CURRENT_CONFIG_64',
    'HKEY_DYN_DATA', 'HKA', 'HKA32', 'HKA64', 'HKCR', 'HKCR32', 'HKCR64', 'HKCU',
    'HKCU32', 'HKCU64', 'HKLM', 'HKLM32', 'HKLM64', 'HKU', 'HKU32', 'HKU64',
    'HKCC', 'HKCC32', 'HKCC64', 'SW_HIDE', 'SW_SHOWNORMAL', 'SW_SHOWMINIMIZED',
    'SW_SHOWMAXIMIZED', 'SW_SHOWMINNOACTIVE', 'SW_SHOW', 'FILE_ATTRIBUTE_READONLY',
    'FILE_ATTRIBUTE_HIDDEN', 'FILE_ATTRIBUTE_SYSTEM', 'FILE_ATTRIBUTE_DIRECTORY',
    'FILE_ATTRIBUTE_ARCHIVE', 'FILE_ATTRIBUTE_DEVICE', 'FILE_ATTRIBUTE_NORMAL',
    'FILE_ATTRIBUTE_TEMPORARY', 'FILE_ATTRIBUTE_SPARSE_FILE','FILE_ATTRIBUTE_REPARSE_POINT',
    'FILE_ATTRIBUTE_COMPRESSED', 'FILE_ATTRIBUTE_OFFLINE', 'FILE_ATTRIBUTE_NOT_CONTENT_INDEXED',
    'FILE_ATTRIBUTE_ENCRYPTED', 'VER_NT_WORKSTATION', 'VER_NT_DOMAIN_CONTROLLER',
    'VER_NT_SERVER', 'VER_SUITE_SMALLBUSINESS', 'VER_SUITE_ENTERPRISE', 'VER_SUITE_BACKOFFICE',
    'VER_SUITE_COMMUNICATIONS', 'VER_SUITE_TERMINAL', 'VER_SUITE_SMALLBUSINESS_RESTRICTED',
    'VER_SUITE_EMBEDDEDNT', 'VER_SUITE_DATACENTER', 'VER_SUITE_SINGLEUSERTS',
    'VER_SUITE_PERSONAL', 'VER_SUITE_BLADE', 'VER_SUITE_EMBEDDED_RESTRICTED',
    'VER_SUITE_SECURITY_APPLIANCE',
    'SIID_DOCNOASSOC', 'SIID_DOCASSOC', 'SIID_APPLICATION', 'SIID_FOLDER', 'SIID_FOLDEROPEN',
    'SIID_DRIVE525', 'SIID_DRIVE35', 'SIID_DRIVEREMOVE', 'SIID_DRIVEFIXED', 'SIID_DRIVENET',
    'SIID_DRIVENETDISABLED', 'SIID_DRIVECD', 'SIID_DRIVERAM', 'SIID_WORLD', 'SIID_SERVER',
    'SIID_PRINTER', 'SIID_MYNETWORK', 'SIID_FIND', 'SIID_HELP', 'SIID_SHARE', 'SIID_LINK',
    'SIID_SLOWFILE', 'SIID_RECYCLER', 'SIID_RECYCLERFULL', 'SIID_MEDIACDAUDIO', 'SIID_LOCK',
    'SIID_AUTOLIST', 'SIID_PRINTERNET', 'SIID_SERVERSHARE', 'SIID_PRINTERFAX',
    'SIID_PRINTERFAXNET', 'SIID_PRINTERFILE', 'SIID_STACK', 'SIID_MEDIASVCD',
    'SIID_STUFFEDFOLDER', 'SIID_DRIVEUNKNOWN', 'SIID_DRIVEDVD', 'SIID_MEDIADVD',
    'SIID_MEDIADVDRAM', 'SIID_MEDIADVDRW', 'SIID_MEDIADVDR', 'SIID_MEDIADVDROM',
    'SIID_MEDIACDAUDIOPLUS', 'SIID_MEDIACDRW', 'SIID_MEDIACDR', 'SIID_MEDIACDBURN',
    'SIID_MEDIABLANKCD', 'SIID_MEDIACDROM', 'SIID_AUDIOFILES', 'SIID_IMAGEFILES',
    'SIID_VIDEOFILES', 'SIID_MIXEDFILES', 'SIID_FOLDERBACK', 'SIID_FOLDERFRONT', 'SIID_SHIELD',
    'SIID_WARNING', 'SIID_INFO', 'SIID_ERROR', 'SIID_KEY', 'SIID_SOFTWARE', 'SIID_RENAME',
    'SIID_DELETE', 'SIID_MEDIAAUDIODVD', 'SIID_MEDIAMOVIEDVD', 'SIID_MEDIAENHANCEDCD',
    'SIID_MEDIAENHANCEDDVD', 'SIID_MEDIAHDDVD', 'SIID_MEDIABLURAY', 'SIID_MEDIAVCD',
    'SIID_MEDIADVDPLUSR', 'SIID_MEDIADVDPLUSRW', 'SIID_DESKTOPPC', 'SIID_MOBILEPC',
    'SIID_USERS', 'SIID_MEDIASMARTMEDIA', 'SIID_MEDIACOMPACTFLASH', 'SIID_DEVICECELLPHONE',
    'SIID_DEVICECAMERA', 'SIID_DEVICEVIDEOCAMERA', 'SIID_DEVICEAUDIOPLAYER',
    'SIID_NETWORKCONNECT', 'SIID_INTERNET', 'SIID_ZIPFILE', 'SIID_SETTINGS', 'SIID_DRIVEHDDVD',
    'SIID_DRIVEBD', 'SIID_MEDIAHDDVDROM', 'SIID_MEDIAHDDVDR', 'SIID_MEDIAHDDVDRAM',
    'SIID_MEDIABDROM', 'SIID_MEDIABDR', 'SIID_MEDIABDRE', 'SIID_CLUSTEREDDRIVE'
    //undocumented: irInstall
    { ScriptClasses: see PascalConstants_Isxclasses in isxclasses_wordlists_generated }
  ];

  PascalInterfaces: array of AnsiString = [
    { ROPS }
    'IUnknown', 'IInterface', 'IDispatch'
  ];

  PascalReservedWords: array of TScintRawString = [
    'and', 'array', 'as', 'begin', 'case', 'const', 'div', 'do', 'downto',
    'else', 'end', 'except', 'external', 'finally', 'for', 'forward', 'function',
    'goto', 'if', 'in', 'is', 'label', 'mod', 'nil', 'not', 'of', 'or', 'out',
    'procedure', 'program', 'record', 'repeat', 'set', 'shl', 'shr', 'then',
    'to', 'try', 'type', 'until', 'var', 'while', 'with', 'xor', 'delayload',
    'loadwithalteredsearchpath', 'stdcall', 'cdecl', 'register', 'pascal', 'safecall',
    'setuponly', 'uninstallonly', 'event'
  ];

  PascalTypes: array of AnsiString = [
    { ROPS - should not include ScriptClasses types, see below }
    'Byte', 'Boolean', 'LongBool', 'WordBool', 'ByteBool', 'AnsiChar', 'Char',
    'WideChar', 'WideString', 'UnicodeString', 'AnsiString', 'String', 'ShortInt',
    'Word', 'SmallInt', 'LongInt', 'LongWord', 'Integer', 'Cardinal', 'Int64', 'UInt64',
    'Single', 'Double', 'Extended', 'Currency', 'PAnsiChar', 'Variant',
    'TVariantArray', 'NativeInt', 'NativeUInt',
    //undocumented: NativeString, AnyString, AnyMethod, ___Pointer, tbtString, !NotificationVariant
    'TVarType',
    //undocumented: TIFException
    { ScriptFunc's real enums, values done via PascalRealEnumValues instead of PascalEnumValues}
    'TMsgBoxType', 'TSetupMessageID', 'TSetupStep', 'TUninstallStep',
    'TSetupProcessorArchitecture', 'TDotNetVersion',
    { ScriptFunc's non real enums and other types - also see PascalEnumValues below }
    'TArrayOfString', 'TArrayOfChar', 'TArrayOfBoolean', 'TArrayOfInteger', 'TArrayOfGraphic',
    'DWORD', 'UINT', 'BOOL', 'LONG', 'ULONG', 'HANDLE', 'COLORREF',
    'INT_PTR', 'LONG_PTR', 'DWORD_PTR', 'UINT_PTR', 'ULONG_PTR',
    'LRESULT', 'HKEY', 'HINSTANCE', 'HMODULE', 'WPARAM', 'LPARAM', 'SIZE_T', 'SSIZE_T',
    'TFileTime', 'TSplitType', 'TExecWait', 'TExecOutput', 'TFindRec', 'TWindowsVersion',
    'TOnDownloadProgress', 'TOnExtractionProgress', 'TOnLog', 'TPathRedirTargetProcess',
    { ScriptClasses: see PascalTypes_Isxclasses in isxclasses_wordlists_generated +
      also the following from USPC_comobj.pas }
    'HResult', 'TGUID', 'TCLSID', 'TIID'
  ];

  PascalEnumValues: array of AnsiString = [
    { ScriptFunc's values of non real enums - also see PascalTypes above }
    'stAll', 'stExcludeEmpty', 'stExcludeLastEmpty',
    'ewNoWait', 'ewWaitUntilTerminated', 'ewWaitUntilIdle',
    'tpCurrent', 'tpNativeBit', 'tp32Bit', 'tp32BitPreferSystem32'
    { ScriptClasses: see PascalEnumValues_Isxclasses in isxclasses_wordlists_generated }
  ];

var
  PascalRealEnumValues: array of PTypeInfo; { Initialized below }

const
  PascalVariables: array of AnsiString = [
    { ROPS }
    'Result',
    { ScriptClasses }
    'WizardForm', 'MainForm', 'UninstallProgressForm'
  ];

  BasicEventFunctions: array of TScintRawString = [
    'InitializeSetup', 'InitializeWizard', 'DeinitializeSetup', 'CurStepChanged',
    'CurInstallProgressChanged', 'NextButtonClick', 'BackButtonClick',
    'CancelButtonClick', 'ShouldSkipPage', 'CurPageChanged', 'CheckPassword',
    'NeedRestart', 'UpdateReadyMemo', 'RegisterPreviousData', 'CheckSerial',
    'GetCustomSetupExitCode', 'PrepareToInstall',
    'RegisterExtraCloseApplicationsResources', 'InitializeUninstall',
    'InitializeUninstallProgressForm', 'DeinitializeUninstall',
    'CurUninstallStepChanged', 'UninstallNeedRestart'
  ];

  FullEventFunctions: array of AnsiString = [
    'function InitializeSetup: Boolean;',
    'procedure InitializeWizard;',
    'procedure DeinitializeSetup;',
    'procedure CurStepChanged(CurStep: TSetupStep);',
    'procedure CurInstallProgressChanged(CurProgress, MaxProgress: Integer);',
    'function NextButtonClick(CurPageID: Integer): Boolean;',
    'function BackButtonClick(CurPageID: Integer): Boolean;',
    'procedure CancelButtonClick(CurPageID: Integer; var Cancel, Confirm: Boolean);',
    'function ShouldSkipPage(PageID: Integer): Boolean;',
    'procedure CurPageChanged(CurPageID: Integer);',
    'function CheckPassword(Password: String): Boolean;',
    'function NeedRestart: Boolean;',
    'function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;',
    'procedure RegisterPreviousData(PreviousDataKey: Integer);',
    'function CheckSerial(Serial: String): Boolean;',
    'function GetCustomSetupExitCode: Integer;',
    'function PrepareToInstall(var NeedsRestart: Boolean): String;',
    'procedure RegisterExtraCloseApplicationsResources;',
    'function InitializeUninstall: Boolean;',
    'procedure InitializeUninstallProgressForm;',
    'procedure DeinitializeUninstall;',
    'procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);',
    'function UninstallNeedRestart: Boolean;'
  ];

  EventFunctionsParameters: array of AnsiString = [
    'CurStep', 'CurProgress', 'MaxProgress', 'CurPageID', 'Cancel', 'Confirm',
    'PageID', 'Password', 'Space', 'NewLine', 'MemoUserInfoInfo',
    'MemoDirInfo', 'MemoTypeInfo', 'MemoComponentsInfo', 'MemoGroupInfo',
    'MemoTasksInfo', 'PreviousDataKey', 'Serial', 'NeedsRestart',
    'CurUninstallStep'
  ];

  ISPPReservedWords: array[0..17] of TScintRawString = (
    'private', 'protected', 'public', 'any', 'int',
    'str', 'func', 'array', 'option', 'parseroption', 'inlinestart',
    'inlineend', 'message', 'warning', 'error',
    'verboselevel', 'include', 'spansymbol');

  ISPPDirectiveShorthands: TScintRawCharSet =
    [':' {define},
     '+' {include},
     '=' {emit},
     '%' {env},
     '!' {expr},
     '?' {if},
     '^' {else},
     '.' {endif}];

  KeyValueSections = [scSetup, scLangOptions, scMessages, scCustomMessages];

  ParameterSections = [scComponents, scDirs, scISSigKeys, scFiles, scIcons,
    scINI, scInstallDelete, scLanguages, scRegistry, scRun, scTasks, scTypes,
    scUninstallDelete, scUninstallRun];

type
  TSetupSectionDirectiveValue = record
    Directive: TSetupSectionDirective;
    Values: TArray<TScintRawString>;
  end;

var
  SetupSectionExpressionDirectivesValues: array of TSetupSectionDirectiveValue; { Initialized below }

function SectionToSectionName(const ASection: TInnoSetupSection): String;

implementation

uses
  Shared.CommonFunc.Vcl, Shared.DotNetVersion, Shared.SetupMessageIDs,
  Shared.SetupSteps, Shared.Struct;

function ISPPD(const Name: TScintRawString; const RequiresParameter: Boolean; const OpenCountChange: ShortInt): TISPPDirective;
begin
  Result.Name := Name;
  Result.RequiresParameter := RequiresParameter;
  Result.OpenCountChange := OpenCountChange;
end;

function SSDV(const Directive: TSetupSectionDirective; const Values: TArray<TScintRawString>): TSetupSectionDirectiveValue;
begin
  Result.Directive := Directive;
  Result.Values := Values;
end;

function SectionToSectionName(const ASection: TInnoSetupSection): String;
begin
  Result := Copy(GetEnumName(TypeInfo(TInnoSetupSection), Ord(ASection)),
    InnoSetupSectionPrefixLength+1, MaxInt);
end;

initialization
  ISPPDirectives := [
    ISPPD('preproc', True, 0),
    ISPPD('define', True, 0),
    ISPPD('dim', True, 0),
    ISPPD('redim', True, 0),
    ISPPD('undef', True, 0),
    ISPPD('include', True, 0),
    ISPPD('file', True, 0),
    ISPPD('emit', True, 0),
    ISPPD('echo', True, 0),
    ISPPD('env', True, 0),
    ISPPD('expr', True, 0),
    ISPPD('call', True, 0),
    ISPPD('insert', True, 0),
    ISPPD('append', False, 0),
    ISPPD('if', True, 1), { also see #? OpenCount handling in HandleCompilerDirective }
    ISPPD('elif', False { bug in ISPP? }, 0),
    ISPPD('else', False, 0),
    ISPPD('endif', False, -1), { also see #. OpenCount handling in HandleCompilerDirective }
    ISPPD('ifdef', True, 1),
    ISPPD('ifndef', True, 1),
    ISPPD('ifexist', True, 1),
    ISPPD('ifnexist', True, 1),
    ISPPD('for', True, 0),
    ISPPD('sub', True, 1),
    ISPPD('endsub', False, -1),
    ISPPD('pragma', False, 0),
    ISPPD('error', False, 0)];

  SetLength(PascalRealEnumValues, 6);
  PascalRealEnumValues[0] := TypeInfo(TMsgBoxType);
  PascalRealEnumValues[1] := TypeInfo(TSetupMessageID);
  PascalRealEnumValues[2] := TypeInfo(TSetupStep);
  PascalRealEnumValues[3] := TypeInfo(TUninstallStep);
  PascalRealEnumValues[4] := TypeInfo(TSetupProcessorArchitecture);
  PascalRealEnumValues[5] := TypeInfo(TDotNetVersion);
  { TPathRedirTargetProcess: see PascalEnumValues }

  const ArchitecturesExpressionValues: TArray<TScintRawString> = [
    'not', 'and', 'or',
    'arm32compatible', 'arm64', 'win64',
    'x64', 'x64os', 'x64compatible',
    'x86', 'x86os', 'x86compatible'];

  SetupSectionExpressionDirectivesValues := [
    SSDV(ssArchitecturesAllowed, ArchitecturesExpressionValues),
    SSDV(ssArchitecturesInstallIn64BitMode, ArchitecturesExpressionValues)];

end.
