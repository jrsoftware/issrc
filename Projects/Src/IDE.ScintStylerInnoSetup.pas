unit IDE.ScintStylerInnoSetup;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TInnoSetupStyler: styler for Inno Setup scripts
}

interface

uses
  SysUtils, Classes, Graphics, Generics.Collections, TypInfo,
  ScintEdit, ModernColors, Shared.ScriptFunc;

const
  InnoSetupStylerWordListSeparator = #9;
  InnoSetupStylerWordListTypeSeparator = '!'; { Must sort before numbers - so the default '?' is not ok }

  { AutoComplete word types }
  awtSection = 0;
  awtParameter = 1;
  awtDirective = 2;
  awtFlag = 3;
  awtPreprocessorDirective = 4;
  awtConstant = 5;
  awtScriptFunction = 10;
  awtScriptType = 11;
  awtScriptVariable = 12;
  awtScriptConstant = 13;
  awtScriptInterface = 14;
  awtScriptProperty = 15;
  awtScriptEvent = 16;
  awtScriptKeyword = 17;
  awtScriptEnumValue = 18;

type
  TInnoSetupStylerSection = (
    scNone,            { Not inside a section (start of file, or previous section was closed )
                         Section tags themselves are not associated with any section! }
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

  { Internally-used types }
  TInnoSetupStylerSpanState = (spNone, spBraceComment, spStarComment);

  { Starts at 1 instead of 0 to make sure ApplyStyle doesn't overwrite already applied stDefault
    styles which is needed for PreStyleInlineISPPDirectives to work properly when the inline
    directive is inside a comment or string }
  TInnoSetupStylerStyle = (stDefault = 1, stCompilerDirective,
    stComment, stSection, stSymbol, stKeyword, stParameterValue,
    stEventFunction, stConstant, stMessageArg,
    stPascalReservedWord, stPascalString, stPascalNumber,
    stISPPReservedWord, stISPPString, stISPPNumber);

  TWordsBySection = TObjectDictionary<TInnoSetupStylerSection, TStringList>;
  TFunctionDefinition = record
    ScriptFuncWithoutHeader: AnsiString;
    WasFunction, HasParams: Boolean;
    constructor Create(const ScriptFunc: AnsiString);
  end;
  TFunctionDefinitions = array of TFunctionDefinition;
  TFunctionDefinitionsByName = TDictionary<String, TFunctionDefinitions>;

  TInnoSetupStyler = class(TScintCustomStyler)
  private
    FEventFunctionsWordList: array[Boolean] of AnsiString;
    FKeywordsWordList, FFlagsWordList: array[TInnoSetupStylerSection] of AnsiString;
    FFlagsWords: TWordsBySection;
    FISPPDirectivesWordList, FConstantsWordList: AnsiString;
    FSectionsWordList: AnsiString;
    FScriptFunctionsByName: array[Boolean] of TFunctionDefinitionsByName; { Only has functions with at least 1 parameter }
    FScriptWordList: array[Boolean] of AnsiString;
    FISPPInstalled: Boolean;
    FTheme: TTheme;
    procedure AddWordToList(const SL: TStringList; const Word: AnsiString;
      const Typ: Integer);
    procedure ApplyPendingSquigglyFromToIndex(const StartIndex, EndIndex: Integer);
    procedure ApplyPendingSquigglyFromIndex(const StartIndex: Integer);
    procedure ApplySquigglyFromIndex(const StartIndex: Integer);
    procedure BuildConstantsWordList;
    procedure BuildEventFunctionsWordList;
    procedure BuildFlagsWordList(const Section: TInnoSetupStylerSection;
     const Flags: array of TScintRawString);
    procedure BuildISPPDirectivesWordList;
    procedure BuildKeywordsWordList(const Section: TInnoSetupStylerSection;
      const Parameters: array of TScintRawString);
    procedure BuildKeywordsWordListFromTypeInfo(const Section: TInnoSetupStylerSection;
      const EnumTypeInfo: Pointer; const PrefixLength: Integer);
    procedure BuildScriptFunctionsLists(const ScriptFuncTable: TScriptTable;
      const ClassMembers: Boolean; const SL: TStringList);
    function BuildWordList(const WordStringList: TStringList): AnsiString;
    procedure BuildSectionsWordList;
    procedure CommitStyleSq(const Style: TInnoSetupStylerStyle;
      const Squigglify: Boolean);
    procedure CommitStyleSqPending(const Style: TInnoSetupStylerStyle);
    function GetEventFunctionsWordList(Procedures: Boolean): AnsiString;
    function GetFlagsWordList(Section: TInnoSetupStylerSection): AnsiString;
    function GetKeywordsWordList(Section: TInnoSetupStylerSection): AnsiString;
    procedure HandleCodeSection(var SpanState: TInnoSetupStylerSpanState);
    procedure HandleKeyValueSection(const Section: TInnoSetupStylerSection);
    procedure HandleParameterSection(const ValidParameters: array of TScintRawString);
    procedure HandleCompilerDirective(const InlineDirective: Boolean;
      const InlineDirectiveEndIndex: Integer; var OpenCount: ShortInt);
    procedure PreStyleInlineISPPDirectives;
    procedure SkipWhitespace;
    procedure SquigglifyUntilChars(const Chars: TScintRawCharSet;
      const Style: TInnoSetupStylerStyle);
    procedure StyleConstsUntilChars(const Chars: TScintRawCharSet;
      const NonConstStyle: TInnoSetupStylerStyle; var BraceLevel: Integer);
    procedure SetISPPInstalled(const Value: Boolean);
    function GetScriptWordList(ClassOrRecordMembers: Boolean): AnsiString;
  protected
    procedure CommitStyle(const Style: TInnoSetupStylerStyle);
    procedure GetFoldLevel(const LineState, PreviousLineState: TScintLineState;
      var Level: Integer; var Header, EnableHeaderOnPrevious: Boolean); override;
    procedure GetStyleAttributes(const Style: Integer;
      var Attributes: TScintStyleAttributes); override;
    function LineTextSpans(const S: TScintRawString): Boolean; override;
    procedure StyleNeeded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetSectionFromLineState(const LineState: TScintLineState): TInnoSetupStylerSection;
    class function IsCommentOrPascalStringStyle(const Style: TScintStyleNumber): Boolean;
    class function IsParamSection(const Section: TInnoSetupStylerSection): Boolean;
    class function IsSymbolStyle(const Style: TScintStyleNumber): Boolean;
    function GetScriptFunctionDefinition(const ClassMember: Boolean;
      const Name: String; const Index: Integer; out Count: Integer): TFunctionDefinition; overload;
    function GetScriptFunctionDefinition(const ClassMember: Boolean;
      const Name: String; const Index: Integer): TFunctionDefinition; overload;
    function SectionHasFlag(const Section: TInnoSetupStylerSection; const Flag: String): Boolean;
    property ConstantsWordList: AnsiString read FConstantsWordList;
    property EventFunctionsWordList[Procedures: Boolean]: AnsiString read GetEventFunctionsWordList;
    property FlagsWordList[Section: TInnoSetupStylerSection]: AnsiString read GetFlagsWordList;
    property ISPPDirectivesWordList: AnsiString read FISPPDirectivesWordList;
    property ISPPInstalled: Boolean read FISPPInstalled write SetISPPInstalled;
    property KeywordsWordList[Section: TInnoSetupStylerSection]: AnsiString read GetKeywordsWordList;
    property ScriptWordList[ClassOrRecordMembers: Boolean]: AnsiString read GetScriptWordList;
    property SectionsWordList: AnsiString read FSectionsWordList;
    property Theme: TTheme read FTheme write FTheme;
  end;

implementation

uses
  Generics.Defaults,
  Shared.SetupMessageIDs, ScintInt, Shared.SetupSectionDirectives, Shared.LangOptionsSectionDirectives,
  Shared.CommonFunc.Vcl, Shared.SetupSteps, Shared.Struct, Shared.DotNetVersion, isxclasses_wordlists_generated;

type
  { Size must be <= SizeOf(TScintLineState) }
  TInnoSetupStylerLineState = record
    Section, NextLineSection: TInnoSetupStylerSection;
    SpanState: TInnoSetupStylerSpanState;
    OpenCompilerDirectivesCount: ShortInt;
  end;

type
  TSectionMapItem = record
    Name: TScintRawString;
    Section: TInnoSetupStylerSection;
  end;

const
  SectionMap: array[0..17] of TSectionMapItem = (
    (Name: 'Code'; Section: scCode),
    (Name: 'Components'; Section: scComponents),
    (Name: 'CustomMessages'; Section: scCustomMessages),
    (Name: 'Dirs'; Section: scDirs),
    (Name: 'Files'; Section: scFiles),
    (Name: 'Icons'; Section: scIcons),
    (Name: 'INI'; Section: scINI),
    (Name: 'InstallDelete'; Section: scInstallDelete),
    (Name: 'LangOptions'; Section: scLangOptions),
    (Name: 'Languages'; Section: scLanguages),
    (Name: 'Messages'; Section: scMessages),
    (Name: 'Registry'; Section: scRegistry),
    (Name: 'Run'; Section: scRun),
    (Name: 'Setup'; Section: scSetup),
    (Name: 'Tasks'; Section: scTasks),
    (Name: 'Types'; Section: scTypes),
    (Name: 'UninstallDelete'; Section: scUninstallDelete),
    (Name: 'UninstallRun'; Section: scUninstallRun));

  ComponentsSectionParameters: array of TScintRawString = [
    'Check', 'Description', 'ExtraDiskSpaceRequired', 'Flags', 'Languages',
    'MinVersion', 'Name', 'OnlyBelowVersion', 'Types'
  ];

  ComponentsSectionFlags: array of TScintRawString = [
    'checkablealone', 'disablenouninstallwarning', 'dontinheritcheck', 'exclusive',
    'fixed', 'restart'
  ];

  DeleteSectionParameters: array of TScintRawString = [
    'AfterInstall', 'BeforeInstall', 'Check', 'Components', 'Languages',
    'MinVersion', 'Name', 'OnlyBelowVersion', 'Tasks', 'Type'
  ];

  DeleteSectionTypes: array of TScintRawString = [
    'files', 'filesandordirs', 'dirifempty'
  ];

  DirsSectionParameters: array of TScintRawString = [
    'AfterInstall', 'Attribs', 'BeforeInstall', 'Check', 'Components', 'Flags',
    'Languages', 'MinVersion', 'Name', 'OnlyBelowVersion', 'Permissions', 'Tasks'
  ];

  DirsSectionFlags: array of TScintRawString = [
    'deleteafterinstall', 'setntfscompression', 'uninsalwaysuninstall',
    'uninsneveruninstall', 'unsetntfscompression'
  ];

  FilesSectionParameters: array of TScintRawString = [
    'AfterInstall', 'Attribs', 'BeforeInstall', 'Check', 'Components', 'CopyMode',
    'DestDir', 'DestName', 'Excludes', 'ExternalSize', 'Flags', 'FontInstall',
    'Languages', 'MinVersion', 'OnlyBelowVersion', 'Permissions', 'Source',
    'StrongAssemblyName', 'Tasks'
  ];

  FilesSectionFlags: array of TScintRawString = [
    '32bit', '64bit', 'allowunsafefiles', 'comparetimestamp', 'confirmoverwrite',
    'createallsubdirs', 'deleteafterinstall', 'dontcopy', 'dontverifychecksum',
    'external', 'fontisnttruetype', 'gacinstall', 'ignoreversion', 'isreadme',
    'nocompression', 'noencryption', 'noregerror', 'onlyifdestfileexists',
    'onlyifdoesntexist', 'overwritereadonly', 'promptifolder', 'recursesubdirs',
    'regserver', 'regtypelib', 'replacesameversion', 'restartreplace',
    'setntfscompression', 'sharedfile', 'sign', 'signcheck', 'signonce',
    'skipifsourcedoesntexist', 'solidbreak', 'sortfilesbyextension',
    'sortfilesbyname', 'touch', 'uninsnosharedfileprompt', 'uninsremovereadonly',
    'uninsrestartdelete', 'uninsneveruninstall', 'unsetntfscompression'
  ];

  IconsSectionParameters: array of TScintRawString = [
    'AfterInstall', 'AppUserModelID', 'AppUserModelToastActivatorCLSID',
    'BeforeInstall', 'Check', 'Comment', 'Components', 'Filename', 'Flags',
    'HotKey', 'IconFilename', 'IconIndex', 'Languages', 'MinVersion', 'Name',
    'OnlyBelowVersion', 'Parameters', 'Tasks', 'WorkingDir'
  ];

  IconsSectionFlags: array of TScintRawString = [
    'closeonexit', 'createonlyiffileexists', 'dontcloseonexit',
    'excludefromshowinnewinstall', 'foldershortcut', 'preventpinning',
    'runmaximized', 'runminimized', 'uninsneveruninstall', 'useapppaths'
  ];

  INISectionParameters: array of TScintRawString = [
    'AfterInstall', 'BeforeInstall', 'Check', 'Components', 'Filename',
    'Flags', 'Key', 'Languages', 'MinVersion', 'OnlyBelowVersion', 'Section',
    'String', 'Tasks'
  ];

  INISectionFlags: array of TScintRawString = [
    'createkeyifdoesntexist', 'uninsdeleteentry', 'uninsdeletesection',
    'uninsdeletesectionifempty'
  ];

  LanguagesSectionParameters: array of TScintRawString = [
    'InfoAfterFile', 'InfoBeforeFile', 'LicenseFile', 'MessagesFile', 'Name'
  ];

  RegistrySectionParameters: array of TScintRawString = [
    'AfterInstall', 'BeforeInstall', 'Check', 'Components', 'Flags', 'Languages',
    'MinVersion', 'OnlyBelowVersion', 'Permissions', 'Root', 'Subkey', 'Tasks',
    'ValueData', 'ValueName', 'ValueType'
  ];

  RegistrySectionFlags: array of TScintRawString = [
    'createvalueifdoesntexist', 'deletekey', 'deletevalue', 'dontcreatekey',
    'noerror', 'preservestringtype', 'uninsclearvalue', 'uninsdeletekey',
    'uninsdeletekeyifempty', 'uninsdeletevalue'
  ];

  RunSectionParameters: array of TScintRawString = [
    'AfterInstall', 'BeforeInstall', 'Check', 'Components', 'Description',
    'Filename', 'Flags', 'Languages', 'MinVersion', 'OnlyBelowVersion',
    'Parameters', 'StatusMsg', 'Tasks', 'Verb', 'WorkingDir'
  ];

  RunSectionFlags: array of TScintRawString = [
    '32bit', '64bit', 'dontlogparameters', 'hidewizard', 'logoutput', 'nowait',
    'postinstall', 'runascurrentuser', 'runasoriginaluser', 'runhidden',
    'runmaximized', 'runminimized', 'shellexec', 'skipifdoesntexist', 'skipifnotsilent',
    'skipifsilent', 'unchecked', 'waituntilidle', 'waituntilterminated'
  ];

  UninstallRunSectionParameters: array of TScintRawString = [
    'AfterInstall', 'BeforeInstall', 'Check', 'Components', 'Filename', 'Flags',
    'Languages', 'MinVersion', 'OnlyBelowVersion', 'Parameters', 'RunOnceId',
    'Tasks', 'Verb', 'WorkingDir'
  ];

  UninstallRunSectionFlags: array of TScintRawString = [
    '32bit', '64bit', 'dontlogparameters', 'hidewizard', 'logoutput', 'nowait',
    'runascurrentuser', 'runhidden', 'runmaximized', 'runminimized', 'shellexec',
    'skipifdoesntexist', 'waituntilidle', 'waituntilterminated'
  ];

  TasksSectionParameters: array of TScintRawString = [
    'Check', 'Components', 'Description', 'Flags', 'GroupDescription', 'Languages',
    'MinVersion', 'Name', 'OnlyBelowVersion'
  ];

  TasksSectionFlags: array of TScintRawString = [
    'checkablealone', 'checkedonce', 'dontinheritcheck', 'exclusive', 'restart',
    'unchecked'
  ];

  TypesSectionParameters: array of TScintRawString = [
    'Check', 'Description', 'Flags', 'Languages', 'MinVersion', 'Name',
    'OnlyBelowVersion'
  ];

  TypesSectionFlags: array of TScintRawString = [
    'iscustom'
  ];

type
  TISPPDirective = record
    Name: TScintRawString;
    RequiresParameter: Boolean;
    OpenCountChange: ShortInt;
  end;

const
  ISPPDirectives: array[0..23] of TISPPDirective = (
    (Name: 'preproc'; RequiresParameter: True; OpenCountChange: 0),
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

  { The following and some others below are not used by StyleNeeded and therefore
    simply of type AnsiString instead of TScintRawString }
  ConstantsWithParam: array of AnsiString = [
    'cm', 'code', 'drive', 'ini', 'param', 'reg'
  ];

  Constants: array of AnsiString = [
    { #emit and #file handled separately by BuildConstantsWordList.
      Also doesnt include constants with non words chars. }
    '{', 'app', 'win', 'sys', 'sysnative', 'syswow64', 'src', 'sd', 'commonpf',
    'commoncf', 'tmp', 'commonfonts', 'dao', 'dotnet11', 'dotnet20', 'dotnet40',
    'group', 'localappdata', 'userappdata', 'commonappdata', 'usercf',
    'userdesktop', 'commondesktop', 'userdocs', 'commondocs', 'userfavorites',
    'userfonts', 'userpf', 'userprograms', 'commonprograms', 'usersavedgames',
    'userstartmenu', 'commonstartmenu', 'userstartup', 'commonstartup',
    'usertemplates', 'commontemplates', 'autoappdata', 'autocf', 'autodesktop',
    'autodocs', 'autofonts', 'autopf', 'autoprograms', 'autostartmenu', 'cmd',
    'computername', 'groupname', 'hwnd', 'wizardhwnd', 'language', 'srcexe',
    'uninstallexe', 'sysuserinfoname', 'sysuserinfoorg', 'userinfoname',
    'userinfoorg', 'userinfoserial', 'username', 'log'
  ];

  PascalConstants: array of AnsiString = [
    { ROPS }
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
    'VER_SUITE_SECURITY_APPLIANCE'
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
    'goto', 'if', 'in', 'is', 'label', 'mod', 'nil', 'not', 'of', 'or',
    'procedure', 'program', 'record', 'repeat', 'set', 'shl', 'shr', 'then',
    'to', 'try', 'type', 'until', 'var', 'while', 'with', 'xor', 'delayload',
    'loadwithalteredsearchpath', 'stdcall', 'cdecl', 'register', 'pascal',
    'setuponly', 'uninstallonly', 'event'
  ];

  PascalTypes: array of AnsiString = [
    { ROPS }
    'Byte', 'Boolean', 'LongBool', 'WordBool', 'ByteBool', 'AnsiChar', 'Char',
    'WideChar', 'WideString', 'UnicodeString', 'AnsiString', 'String', 'ShortInt',
    'Word', 'SmallInt', 'LongInt', 'LongWord', 'Integer', 'Cardinal', 'Int64',
    'Single', 'Double', 'Extended', 'Currency', 'PAnsiChar', 'Variant',
    'TVariantArray',
    //undocumented: NativeString, AnyString, AnyMethod, ___Pointer, tbtString, NativeString, !NotificationVariant
    'TVarType',
    //undocumented: TIFException
    { ScriptFunc's real enums, values done via PascalRealEnumValues instead of PascalEnumValues}
    'TMsgBoxType', 'TSetupMessageID', 'TSetupStep', 'TUninstallStep',
    'TSetupProcessorArchitecture', 'TDotNetVersion',
    { ScriptFunc's non real enums and other types - also see PascalEnumValues below }
    'TArrayOfString', 'TArrayOfChar', 'TArrayOfBoolean', 'TArrayOfInteger', 'DWORD',
    'UINT', 'BOOL', 'DWORD_PTR', 'UINT_PTR', 'INT_PTR', 'TFileTime',
    'TSplitType', 'TExecWait', 'TExecOutput', 'TFindRec', 'TWindowsVersion',
    'TOnDownloadProgress', 'TOnExtractionProgress', 'TOnLog'
    { ScriptClasses: see PascalTypes_Isxclasses in isxclasses_wordlists_generated }
  ];

  PascalEnumValues: array of AnsiString = [
    { ScriptFunc's values of non real enums - also see PascalTypes above }
    'stAll', 'stExcludeEmpty', 'stExcludeLastEmpty',
    'ewNoWait', 'ewWaitUntilTerminated', 'ewWaitUntilIdle'
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

{ TFunctionDefinition }

constructor TFunctionDefinition.Create(const ScriptFunc: AnsiString);
begin
  ScriptFuncWithoutHeader := RemoveScriptFuncHeader(ScriptFunc, WasFunction);
  HasParams := ScriptFuncHasParameters(ScriptFunc);
end;

{ TInnoSetupStyler }

constructor TInnoSetupStyler.Create(AOwner: TComponent);

  procedure BuildFlagsWordLists;
  begin
    BuildFlagsWordList(scFiles, FilesSectionFlags);
    BuildFlagsWordList(scComponents, ComponentsSectionFlags);
    BuildFlagsWordList(scDirs, DirsSectionFlags);
    BuildFlagsWordList(scIcons, IconsSectionFlags);
    BuildFlagsWordList(scINI, INISectionFlags);
    BuildFlagsWordList(scRegistry, RegistrySectionFlags);
    BuildFlagsWordList(scRun, RunSectionFlags);
    BuildFlagsWordList(scTasks, TasksSectionFlags);
    BuildFlagsWordList(scTypes, TypesSectionFlags);
    BuildFlagsWordList(scUninstallRun, UninstallRunSectionFlags);
    { Bit of a trick }
    BuildFlagsWordList(scInstallDelete, DeleteSectionTypes);
    BuildFlagsWordList(scUninstallDelete, DeleteSectionTypes);
  end;

  procedure BuildKeywordsWordLists;
  begin
    BuildKeywordsWordList(scFiles, FilesSectionParameters);
    BuildKeywordsWordList(scIcons, IconsSectionParameters);
    BuildKeywordsWordList(scINI, INISectionParameters);
    BuildKeywordsWordList(scInstallDelete, DeleteSectionParameters);
    BuildKeywordsWordListFromTypeInfo(scLangOptions, TypeInfo(TLangOptionsSectionDirective), LangOptionsSectionDirectivePrefixLength);
    BuildKeywordsWordList(scLanguages, LanguagesSectionParameters);
    BuildKeywordsWordList(scRegistry, RegistrySectionParameters);
    BuildKeywordsWordList(scRun, RunSectionParameters);
    BuildKeywordsWordListFromTypeInfo(scSetup, TypeInfo(TSetupSectionDirective), SetupSectionDirectivePrefixLength);
    BuildKeywordsWordList(scTasks, TasksSectionParameters);
    BuildKeywordsWordList(scTypes, TypesSectionParameters);
    BuildKeywordsWordList(scUninstallDelete, DeleteSectionParameters);
    BuildKeywordsWordList(scUninstallRun, UninstallRunSectionParameters);
    BuildKeywordsWordListFromTypeInfo(scMessages, TypeInfo(TSetupMessageID), SetupMessageIDPrefixLength);
  end;

  procedure BuildScriptLists;
  begin
    { Builds FScriptFunctionsByName (for calltips) and FScriptWordList (for autocomplete) }
    var SL := TStringList.Create;
    try
      { Add stuff from ScriptFunc }
      var ClassMembers := False;
      for var ScriptFuncTable in ScriptFuncTables do
        BuildScriptFunctionsLists(ScriptFuncTable, ClassMembers, SL);
      BuildScriptFunctionsLists(DelphiScriptFuncTable, ClassMembers, SL);
      BuildScriptFunctionsLists(ROPSScriptFuncTable, ClassMembers, SL);
      { Add stuff from this unit }
      for var S in PascalConstants do
        AddWordToList(SL, S, awtScriptConstant);
      for var S in PascalConstants_Isxclasses do
        AddWordToList(SL, S, awtScriptConstant);
      for var S in PascalInterfaces do
        AddWordToList(SL, S, awtScriptInterface);
      for var S in PascalReservedWords do
        AddWordToList(SL, S, awtScriptKeyword);
      for var S in PascalTypes do
        AddWordToList(SL, S, awtScriptType);
      for var S in PascalTypes_Isxclasses do
        AddWordToList(SL, S, awtScriptType);
      for var S in PascalEnumValues do
        AddWordToList(SL, S, awtScriptEnumValue);
      for var S in PascalEnumValues_Isxclasses do
        AddWordToList(SL, S, awtScriptEnumValue);
      for var TypeInfo in PascalRealEnumValues do begin
        var TypeData := GetTypeData(TypeInfo);
        for var I := TypeData.MinValue to TypeData.MaxValue do
          AddWordToList(SL, AnsiString(GetEnumName(TypeInfo, I)), awtScriptEnumValue);
      end;
      for var S in PascalVariables do
        AddWordToList(SL, S, awtScriptVariable);
      for var S in EventFunctionsParameters  do
        AddWordToList(SL, S, awtScriptVariable);
      FScriptWordList[False] := BuildWordList(SL);

      { Add stuff from Isxclasses }
      SL.Clear;
      ClassMembers := True;
      BuildScriptFunctionsLists(PascalMembers_Isxclasses, ClassMembers, SL);
      for var S in PascalProperties_Isxclasses do
        AddWordToList(SL, S, awtScriptProperty);
      FScriptWordList[True] := BuildWordList(SL);
    finally
      SL.Free;
    end;
  end;

begin
  inherited;
  FFlagsWords := TWordsBySection.Create([doOwnsValues]);
  BuildConstantsWordList;
  BuildEventFunctionsWordList;
  BuildFlagsWordLists;
  BuildISPPDirectivesWordList;
  BuildKeywordsWordLists;
  BuildSectionsWordList;
  FScriptFunctionsByName[False] := TFunctionDefinitionsByName.Create(TIStringComparer.Ordinal);
  FScriptFunctionsByName[True] := TFunctionDefinitionsByName.Create(TIStringComparer.Ordinal);
  BuildScriptLists;
end;

destructor TInnoSetupStyler.Destroy;
begin
  FScriptFunctionsByName[False].Free;
  FScriptFunctionsByName[True].Free;
  FFlagsWords.Free;
  inherited;
end;

procedure TInnoSetupStyler.AddWordToList(const SL: TStringList;
  const Word: AnsiString; const Typ: Integer);
begin
  if Typ >= 0 then
    SL.Add(Format('%s%s%d', [Word, InnoSetupStylerWordListTypeSeparator, Typ]))
  else
    SL.Add(String(Word));
end;

procedure TInnoSetupStyler.ApplyPendingSquigglyFromToIndex(const StartIndex, EndIndex: Integer);
begin
  if (CaretIndex >= StartIndex) and (CaretIndex <= EndIndex + 1) then
    ApplyStyleByteIndicators([inPendingSquiggly], StartIndex, EndIndex)
  else
    ApplyStyleByteIndicators([inSquiggly], StartIndex, EndIndex);
end;

procedure TInnoSetupStyler.ApplyPendingSquigglyFromIndex(const StartIndex: Integer);
begin
  ApplyPendingSquigglyFromToIndex(StartIndex, CurIndex - 1);
end;

procedure TInnoSetupStyler.ApplySquigglyFromIndex(const StartIndex: Integer);
begin
  ApplyStyleByteIndicators([inSquiggly], StartIndex, CurIndex - 1);
end;

function TInnoSetupStyler.BuildWordList(const WordStringList: TStringList): AnsiString;
begin
  { Scintilla uses an ASCII binary search so the list must be in ASCII sort
    order (case-insensitive). }
  WordStringList.CaseSensitive := False;
  WordStringList.UseLocale := False; { Make sure it uses CompareText and not AnsiCompareText }
  WordStringList.Sort;

  Result := '';
  for var S in WordStringList do begin
    var A := AnsiString(S);
    if Result = '' then
      Result := A
    else
      Result := Result + InnoSetupStylerWordListSeparator + A;
  end;
end;

procedure TInnoSetupStyler.BuildSectionsWordList;
begin
  var SL := TStringList.Create;
  try
    for var Section in SectionMap do
      AddWordToList(SL, '[' + Section.Name + ']', awtSection);
    FSectionsWordList := BuildWordList(SL);
  finally
    SL.Free;
  end;
end;

procedure TInnoSetupStyler.BuildKeywordsWordList(
  const Section: TInnoSetupStylerSection;
  const Parameters: array of TScintRawString);
begin
  var SL := TStringList.Create;
  try
    for var Parameter in Parameters do
      AddWordToList(SL, Parameter, awtParameter);
    FKeywordsWordList[Section] := BuildWordList(SL);
  finally
    SL.Free;
  end;
end;

procedure TInnoSetupStyler.BuildKeywordsWordListFromTypeInfo(
  const Section: TInnoSetupStylerSection; const EnumTypeInfo: Pointer;
  const PrefixLength: Integer);
begin
  var SL := TStringList.Create;
  try
    for var I := 0 to GetTypeData(EnumTypeInfo).MaxValue do
      AddWordToList(SL, AnsiString(Copy(GetEnumName(EnumTypeInfo, I), PrefixLength+1, Maxint)), awtDirective);
    FKeywordsWordList[Section] := BuildWordList(SL);
  finally
    SL.Free;
  end;
end;

procedure TInnoSetupStyler.BuildFlagsWordList(const Section: TInnoSetupStylerSection;
  const Flags: array of TScintRawString);
begin
  { Build FFlagsWords }
  var SL1 := TStringList.Create;
  try
    SL1.CaseSensitive := False;
    for var Flag in Flags do
      SL1.Add(String(Flag));
    FFlagsWords.Add(Section, SL1);
    SL1 := nil; //SL1 is now owned by FFlagsWords
  except
    SL1.Free;
    raise;
  end;

  { Build FFlagsWordList }
  var SL2 := TStringList.Create;
  try
    for var Flag in Flags do
      AddWordToList(SL2, Flag, awtFlag);
    FFlagsWordList[Section] := BuildWordList(SL2);
  finally
    SL2.Free;
  end;
end;

procedure TInnoSetupStyler.BuildScriptFunctionsLists(
  const ScriptFuncTable: TScriptTable; const ClassMembers: Boolean;
  const SL: TStringList);
begin
  for var ScriptFunc in ScriptFuncTable do begin
    var FunctionDefinition := TFunctionDefinition.Create(ScriptFunc);
    var ScriptFuncName := ExtractScriptFuncWithoutHeaderName(FunctionDefinition.ScriptFuncWithoutHeader);
    var DoAddWordToList := True;
    var Key := String(ScriptFuncName);
    if not FScriptFunctionsByName[ClassMembers].TryAdd(Key, [FunctionDefinition]) then begin
      { Function has multiple prototypes }
      var ScriptFunctions := FScriptFunctionsByName[ClassMembers][Key];
      var N := Length(ScriptFunctions);
      SetLength(ScriptFunctions, N+1);
      ScriptFunctions[N] := FunctionDefinition;
      FScriptFunctionsByName[ClassMembers][Key] := ScriptFunctions;
      DoAddWordToList := False; { Already added it when the first prototype was found }
    end;
    if DoAddWordToList then
      AddWordToList(SL, ScriptFuncName, awtScriptFunction);
  end;
end;

procedure TInnoSetupStyler.BuildISPPDirectivesWordList;
begin
  var SL := TStringList.Create;
  try
    for var ISPPDirective in ISPPDirectives do
      AddWordToList(SL, '#' + ISPPDirective.Name, awtPreprocessorDirective);
    FISPPDirectivesWordList := BuildWordList(SL);
  finally
    SL.Free;
  end;
end;

procedure TInnoSetupStyler.BuildConstantsWordList;
begin
  var SL := TStringList.Create;
  try
    for var Constant in Constants do
      AddWordToList(SL, '{' + Constant + '}', awtConstant);
    if ISPPInstalled then begin
      AddWordToList(SL, '{#', awtConstant);
      AddWordToList(SL, '{#file ', awtConstant);
    end;
    for var ConstantWithParam in ConstantsWithParam do
      AddWordToList(SL, '{' + ConstantWithParam, awtConstant);
    FConstantsWordList := BuildWordList(SL);
  finally
    SL.Free;
  end;
end;

procedure TInnoSetupStyler.BuildEventFunctionsWordList;
begin
  var SLFunctions: TStringList := nil;
  var SLProcedures: TStringList := nil;
  try
    SLFunctions := TStringList.Create;
    SLProcedures := TStringList.Create;
    for var FullEventFunction in FullEventFunctions do begin
      var WasFunction: Boolean;
      var S := RemoveScriptFuncHeader(FullEventFunction, WasFunction);
      if WasFunction then
        AddWordToList(SLFunctions, S, awtScriptEvent)
      else
        AddWordToList(SLProcedures, S, awtScriptEvent);
    end;
    FEventFunctionsWordList[False] := BuildWordList(SLFunctions);
    FEventFunctionsWordList[True] := BuildWordList(SLProcedures);
  finally
    SLProcedures.Free;
    SLFunctions.Free;
  end;
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

function TInnoSetupStyler.GetEventFunctionsWordList(Procedures: Boolean): AnsiString;
begin
  Result := FEventFunctionsWordList[Procedures];
end;

function TInnoSetupStyler.GetFlagsWordList(Section: TInnoSetupStylerSection): AnsiString;
begin
  Result := FFlagsWordList[Section];
end;

procedure TInnoSetupStyler.GetFoldLevel(const LineState, PreviousLineState: TScintLineState;
      var Level: Integer; var Header, EnableHeaderOnPrevious: Boolean);
begin
  { Set folding per section. Lines outside of a section (=lines at the start of
    the document and section tags and section end tags and lines after section
    end tags) get level 0 with header flags for section tags. Other lines
    (=lines inside a section) get level 1. }

  var Section := TInnoSetupStyler.GetSectionFromLineState(LineState);
  if Section = scNone then begin
    Level := 0;
    Header := False; { Might be set to True via EnableHeaderOnPrevious below when we know about next line }
    EnableHeaderOnPrevious := False;
  end else begin
    Level := 1;
    Header := False;
    var PreviousSection := TInnoSetupStyler.GetSectionFromLineState(PreviousLineState);
    EnableHeaderOnPrevious := PreviousSection = scNone;
  end;
end;

function TInnoSetupStyler.GetKeywordsWordList(Section: TInnoSetupStylerSection): AnsiString;
begin
  Result := FKeywordsWordList[Section];
end;

function TInnoSetupStyler.GetScriptFunctionDefinition(const ClassMember: Boolean;
  const Name: String; const Index: Integer; out Count: Integer): TFunctionDefinition;
begin
  var ScriptFunctions: TFunctionDefinitions;
  if FScriptFunctionsByName[ClassMember].TryGetValue(Name, ScriptFunctions) then begin
    Count := Length(ScriptFunctions);
    var ResultIndex := Index;
    if ResultIndex >= Count then
      ResultIndex := Count-1;
    Result := ScriptFunctions[ResultIndex]
  end else
    Count := 0;
end;

function TInnoSetupStyler.GetScriptFunctionDefinition(
  const ClassMember: Boolean; const Name: String;
  const Index: Integer): TFunctionDefinition;
begin
  var Count: Integer;
  Result := GetScriptFunctionDefinition(ClassMember, Name, Index, Count);
end;

function TInnoSetupStyler.GetScriptWordList(
  ClassOrRecordMembers: Boolean): AnsiString;
begin
  Result := FScriptWordList[ClassOrRecordMembers];
end;

class function TInnoSetupStyler.GetSectionFromLineState(
  const LineState: TScintLineState): TInnoSetupStylerSection;
begin
  Result := TInnoSetupStylerLineState(LineState).Section;
end;

procedure TInnoSetupStyler.GetStyleAttributes(const Style: Integer;
  var Attributes: TScintStyleAttributes);
begin
  if FTheme <> nil then begin
    if (Style >= 0) and (Style <= Ord(High(TInnoSetupStylerStyle))) then begin
      if not FTheme.Modern then begin
        { Check for some exceptions }
        case TInnoSetupStylerStyle(Style) of
          stCompilerDirective, stISPPReservedWord: begin Attributes.ForeColor := $4040C0; Exit; end;
          stMessageArg: begin Attributes.ForeColor := $FF8000; Exit; end;
          stPascalString, stPascalNumber, stISPPString, stISPPNumber: begin Attributes.ForeColor := clMaroon; Exit; end;
        end;
      end;
      case TInnoSetupStylerStyle(Style) of
        stCompilerDirective, stISPPReservedWord: Attributes.ForeColor := FTheme.Colors[tcRed];
        stComment: Attributes.ForeColor := FTheme.Colors[tcGreen];
        stSection: Attributes.FontStyle := [fsBold];
        stSymbol: Attributes.ForeColor := FTheme.Colors[tcGray];
        stKeyword, stPascalReservedWord: Attributes.ForeColor := FTheme.Colors[tcBlue];
        //stParameterValue: Attributes.ForeColor := FTheme.Colors[tcTeal];
        stEventFunction: Attributes.FontStyle := [fsBold];
        stConstant: Attributes.ForeColor := FTheme.Colors[tcPurple];
        stMessageArg: Attributes.ForeColor := FTheme.Colors[tcRed];
        stPascalString, stPascalNumber, stISPPString, stISPPNumber: Attributes.ForeColor := FTheme.Colors[tcOrange];
      end;
    end else begin
      case Style of
        STYLE_LINENUMBER: { Also sets the background colour for the margin with the markers like mmIconBreakpoint }
          begin
            Attributes.ForeColor := FTheme.Colors[tcMarginFore];
            Attributes.BackColor := FTheme.Colors[tcMarginBack];
          end;
        STYLE_BRACEBAD: Attributes.ForeColor := FTheme.Colors[tcRed];
        STYLE_BRACELIGHT: Attributes.BackColor := FTheme.Colors[tcBraceBack];
        STYLE_INDENTGUIDE: Attributes.ForeColor := FTheme.Colors[tcIndentGuideFore];
      end;
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
      var S := ConsumeString(PascalIdentChars);
      for var Word in PascalReservedWords do
        if SameRawText(S, Word) then begin
          CommitStyle(stPascalReservedWord);
          Break;
        end;
      for var EventFunction in BasicEventFunctions do
        if SameRawText(S, EventFunction) then begin
          CommitStyle(stEventFunction);
          Break;
        end;
      CommitStyle(stDefault);
    end else if ConsumeChars(DigitChars) then begin
      if not CurCharIs('.') or not NextCharIs('.') then begin
        if ConsumeChar('.') then
          ConsumeChars(DigitChars);
        var C := CurChar;
        if C in ['E', 'e'] then begin
          ConsumeChar(C);
          if not ConsumeChar('-') then
            ConsumeChar('+');
          if not ConsumeChars(DigitChars) then
            CommitStyleSqPending(stPascalNumber);
        end;
      end;
      CommitStyle(stPascalNumber);
    end else begin
      var C := CurChar;
      ConsumeChar(C);
      case C of
        ';', ':', '=', '+', '-', '*', '/', '<', '>', ',', '(', ')',
        '.', '[', ']', '@', '^':
          begin
            if (C = '/') and ConsumeChar('/') then begin
              ConsumeAllRemaining;
              CommitStyle(stComment);
            end else if (C = '(') and ConsumeChar('*') then begin
              if not FinishConsumingStarComment then begin
                SpanState := spStarComment;
                Exit;
              end;
            end else
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
            end else if not ConsumeChars(DigitChars) then
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

  procedure ConsumeISPPString(const Terminator: AnsiChar; const AllowEscapedTerminator: Boolean);
  begin
    while True do begin
      ConsumeCharsNot([Terminator]);
      if not ConsumeChar(Terminator) then begin
        { Non terminated string found }
        CommitStyleSqPending(stISPPString);
        Break;
      end;
      { Terminated string found and consumed. Now check if the terminator is actually escaped by doubling, if allowed }
      if not AllowEscapedTerminator or not ConsumeChar(Terminator) then begin
        { Doubling not allowed or no double terminator found, so we're done }
        CommitStyle(stISPPString);
        Break;
      end;
      { The terminator was doubled so we should continue to find the real terminator }
    end;

  end;

const
  ISPPReservedWords: array[0..16] of TScintRawString = (
    'private', 'protected', 'public', 'any', 'int',
    'str', 'func', 'option', 'parseroption', 'inlinestart',
    'inlineend', 'message', 'warning', 'error',
    'verboselevel', 'include', 'spansymbol');
  ISPPDirectiveShorthands: TScintRawCharSet =
    [':' {define},
     'x' {undef},
     '+' {include},
     '=' {emit},
     '!' {expr}];
begin
  var StartIndex := CurIndex;
  var NeedIspp: Boolean;
  if InlineDirective then begin
    ConsumeChar('{');
    NeedIspp := True;
  end else
    NeedIspp := False; { Might be updated later to True later }
  var ForDirectiveExpressionsNext := False;
  var DoIncludeFileNotationCheck := False;
  var ErrorDirective := False;
  ConsumeChar('#');
  CommitStyle(stCompilerDirective);

  { Directive name or shorthand }
  SkipWhiteSpace;
  var C := CurChar;
  if ConsumeCharIn(ISPPDirectiveShorthands) then begin
    DoIncludeFileNotationCheck := C = '+'; { We need to check the include file notation  }
    NeedIspp := True;
    FinishDirectiveNameOrShorthand(True); { All shorthands require a parameter }
  end else begin
    var S := ConsumeString(ISPPIdentChars);
    for var ISPPDirective in ISPPDirectives do
      if SameRawText(S, ISPPDirective.Name) then begin
        if SameRawText(S, 'error') then
          ErrorDirective := True
        else if SameRawText(S, 'include') then
          DoIncludeFileNotationCheck := True { See above }
        else
          NeedIspp := True; { Built-in preprocessor only supports '#include' }
        ForDirectiveExpressionsNext := SameRawText(S, 'for'); { #for uses ';' as an expressions list separator so we need to remember that ';' doesn't start a comment until the list is done }
        Inc(OpenCount, ISPPDirective.OpenCountChange);
        if OpenCount < 0 then begin
          CommitStyleSq(stCompilerDirective, True);
          OpenCount := 0; { Reset so that next doesn't automatically gets error as well }
        end;
        FinishDirectiveNameOrShorthand(ISPPDirective.RequiresParameter);
        Break;
      end;
    if InlineDirective then
      CommitStyle(stDefault) { #emit shorthand was used (='#' directly followed by an expression): not an error }
    else
      CommitStyleSqPending(stCompilerDirective);
  end;

  { Rest of the directive }
  if ErrorDirective then begin
    SkipWhitespace;
    while not EndOfDirective do begin
      C := CurChar;
      ConsumeChar(C);
      if InlineDirective and (C = '}') then
        CommitStyle(stCompilerDirective)
      else
        CommitStyle(stISPPString);
    end;
  end else begin
    SkipWhitespace;
    while not EndOfDirective do begin
      if DoIncludeFileNotationCheck then begin
        if CurChar <> '"' then begin
          NeedIspp := True; { Built-in preprocessor requires a '"' quoted string after the '#include' and doesn't support anything else }
          if CurChar = '<' then { Check for ISPP's special bracket notation for include files }
            ConsumeISPPString('>', False); { Consume now instead of using regular consumption }
        end;
        DoIncludeFileNotationCheck := False;
      end;
      if CurChar in ISPPIdentFirstChars then begin
        var S := ConsumeString(ISPPIdentChars);
        for var ISPPReservedWord in ISPPReservedWords do
          if SameRawText(S, ISPPReservedWord) then begin
            CommitStyle(stISPPReservedWord);
            Break;
          end;
        CommitStyle(stDefault)
      end else if ConsumeChars(DigitChars) then begin
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
      end else begin
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
            ConsumeISPPString(C, True);
        else
          { Illegal character }
          CommitStyleSq(stSymbol, True);
        end;
      end;
      SkipWhitespace;
    end;
  end;

  if NeedIspp and not ISPPInstalled then begin
    if InlineDirective then
      ApplyPendingSquigglyFromToIndex(StartIndex + 1, InlineDirectiveEndIndex - 1)
    else
      ApplyPendingSquigglyFromIndex(StartIndex + 1);
  end;
end;

procedure TInnoSetupStyler.HandleParameterSection(
  const ValidParameters: array of TScintRawString);
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
      if SameRawText(S, ValidParameters[I]) then begin
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
    end else begin
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
    scCustomMessages:
      I := 0;
    scLangOptions:
      I := GetEnumValue(TypeInfo(TLangOptionsSectionDirective), 'ls' + S);
    scMessages:
      I := GetEnumValue(TypeInfo(TSetupMessageID), 'msg' + S);
    scSetup:
      I := GetEnumValue(TypeInfo(TSetupSectionDirective), 'ss' + S);
  else
    I := -1;
  end;
  if I <> -1 then
    CommitStyle(stKeyword)
  else begin
    if Section in [scLangOptions, scMessages, scSetup] then
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
    CommitStyle(stDefault);
  end;
end;

class function TInnoSetupStyler.IsCommentOrPascalStringStyle(const Style: TScintStyleNumber): Boolean;
begin
  Result := Style in [Ord(stComment), Ord(stPascalString)];
end;

class function TInnoSetupStyler.IsParamSection(
  const Section: TInnoSetupStylerSection): Boolean;
begin
  Result := not (Section in [scCustomMessages, scLangOptions, scMessages, scSetup, scCode]);
end;

class function TInnoSetupStyler.IsSymbolStyle(const Style: TScintStyleNumber): Boolean;
begin
  Result := Style = Ord(stSymbol);
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
      if not ISPPInstalled then
        ApplyStyleByteIndicators([inSquiggly], I, I);
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
      end else
        Inc(I);
    end;
  end;
end;

function TInnoSetupStyler.SectionHasFlag(const Section: TInnoSetupStylerSection;
  const Flag: String): Boolean;
begin
  var SL := FFlagsWords[Section];
  Result := (SL <> nil) and not SL.CaseSensitive and (SL.IndexOf(Flag) <> -1);
end;

procedure TInnoSetupStyler.SetISPPInstalled(const Value: Boolean);
begin
  if Value <> FISPPInstalled then begin
    FISPPInstalled := Value;
    BuildConstantsWordList;
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

  function MapSectionNameString(const S: TScintRawString): TInnoSetupStylerSection;
  begin
    if (S <> '') and (S[1] = '_') then
      Result := scThirdParty
    else begin
      Result := scUnknown;
      for var Section in SectionMap do
        if SameRawText(S, Section.Name) then begin
          Result := Section.Section;
          Break;
        end;
    end;
  end;

var
  NewLineState: TInnoSetupStylerLineState;
  Section, NewSection: TInnoSetupStylerSection;
  SectionEnd: Boolean;
  S: TScintRawString;
begin
  NewLineState := TInnoSetupStylerLineState(LineState);
  if NewLineState.NextLineSection <> scNone then begin
    { Previous line started a section }
    NewLineState.Section := NewLineState.NextLineSection;
    NewLineState.NextLineSection := scNone;
  end;
  Section := NewLineState.Section;

  PreStyleInlineISPPDirectives;

  SkipWhitespace;
  if (Section <> scCode) and ConsumeChar(';') then begin
    ConsumeAllRemaining;
    CommitStyle(stComment);
  end else if CurCharIs('/') and NextCharIs('/') then begin
    ConsumeAllRemaining;
    CommitStyleSq(stComment, not ISPPInstalled and (Section <> scCode))
  end else if ConsumeChar('[') then begin
    SectionEnd := ConsumeChar('/');
    S := ConsumeString(AlphaUnderscoreChars);
    if ConsumeChar(']') then begin
      NewSection := MapSectionNameString(S);
      { Unknown section names and erroneously-placed end tags get squigglified }
      CommitStyleSq(stSection, (NewSection = scUnknown) or
        (SectionEnd and (NewSection <> Section)));
      if not SectionEnd then
        NewLineState.NextLineSection := NewSection;
    end else
      CommitStyleSqPending(stDefault);
    { Section tags themselves are not associated with any section }
    Section := scNone;
    SquigglifyUntilChars([], stDefault);
  end else if CurCharIs('#') then
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
      scUninstallRun: HandleParameterSection(UninstallRunSectionParameters);
    end;
  end;

  NewLineState.Section := Section;
  LineState := TScintLineState(NewLineState);
end;

initialization
  SetLength(PascalRealEnumValues, 6);
  PascalRealEnumValues[0] := TypeInfo(TMsgBoxType);
  PascalRealEnumValues[1] := TypeInfo(TSetupMessageID);
  PascalRealEnumValues[2] := TypeInfo(TSetupStep);
  PascalRealEnumValues[3] := TypeInfo(TUninstallStep);
  PascalRealEnumValues[4] := TypeInfo(TSetupProcessorArchitecture);
  PascalRealEnumValues[5] := TypeInfo(TDotNetVersion);

end.
