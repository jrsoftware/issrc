unit ScriptFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Script support functions (listings - used by Compil32, ISCmplr, and Setup)

  Script constants, types, etc (listings - used by Compil32)
}

interface

type
  TScriptFuncTableID = (sftScriptDlg, sftNewDisk, sftBrowseFunc, sftCmnFunc,
    sftCmnFunc2, sftInstall, sftInstFunc, sftInstFnc2, sftMain, sftMsgs,
    sftSystem, sftSysUtils, sftVerInfo, sftWindows, sftOle2, sftLogging,
    sftOther);
  TScriptTable = array of AnsiString;

var
  ScriptFuncTables: array [TScriptFuncTableID] of TScriptTable; { Initialized below }

  DelphiScriptFuncTable: TScriptTable =
  [
    'function FmtMessage(const S: String; const Args: array of String): String;',
    'function FindFirst(const FileName: String; var FindRec: TFindRec): Boolean;',
    'function FindNext(var FindRec: TFindRec): Boolean;',
    'procedure FindClose(var FindRec: TFindRec);',
    'function Format(const Format: String; const Args: array of const): String;',
    'procedure GetWindowsVersionEx(var Version: TWindowsVersion);'
  ];

{$IFDEF COMPIL32PROJ}

  { These are just for Compil32 and should not be used by ISCmplr or Setup because
    they're already registered by TPSPascalCompiler.DefineStandardProcedures and
    TPSExec.RegisterStandardProc and RegisterDll_Compiletime and RegisterDLLRuntimeEx }
  ROPSScriptFuncTable: TScriptTable =
  [
    'function StrToIntDef(S: String; Def: LongInt): LongInt;',
    'function StrToInt(S: String): LongInt;',
    'function StrToInt64Def(S: String; Def: Int64): Int64;',
    'function StrToInt64(S: String): Int64;',
    'function StrToFloat(S: String): Extended;',
    'function IntToStr(I: Int64): String;',
    'function FloatToStr(E: Extended): String;',
    'function Copy(S: AnyString; Index, Count: Integer): String;',
    'function Length(S: AnyString): LongInt;',
    'procedure SetLength(var S: AnyString; L: LongInt);',
    'function Lowercase(S: AnyString): String;',
    'function Uppercase(S: AnyString): String;',
    'function AnsiLowercase(S: AnyString): String;',
    'function AnsiUppercase(S: AnyString): String;',
    'function StringOfChar(C: Char; I : LongInt): String;',
    'procedure Delete(var S: AnyString; Index, Count: Integer);',
    'procedure Insert(Source: AnyString; var Dest: AnyString; Index: Integer);',
    'function Pos(SubStr, S: AnyString): Integer;',
    'function GetArrayLength(var Arr: Array): LongInt;',
    'procedure SetArrayLength(var Arr: Array; I: LongInt);',
    'function Trim(const S: AnyString): AnyString;',
    'function Null: Variant;',
    'function Unassigned: Variant;',
    'function VarIsEmpty(const V: Variant): Boolean;',
    'function VarIsClear(const V: Variant): Boolean;',
    'function VarIsNull(const V: Variant): Boolean;',
    'function VarType(const V: Variant): TVarType;',
    'function VarArrayGet(var S: Variant; I: Integer): Variant;',
    'procedure VarArraySet(C: Variant; I: Integer; var S: Variant);',
    'function IDispatchInvoke(Self: IDispatch; PropertySet: Boolean; const Name: String; Par: array of Variant): Variant;',
    'procedure UnloadDll(S: String);',
    'function DllGetLastError: LongInt;',
    { Special functions: undocumented but listing anyway }
    'function Low(var X): Int64;',
    'function High(var X): Int64;',
    'procedure Dec(var X: Ordinal);',
    'procedure Inc(var X: Ordinal);',
    'procedure Include(var S: Set; I: Ordinal);',
    'procedure Exclude(var S: Set; I: Ordinal);',
    'function SizeOf(var X): LongInt;'
  ];

  { ROPSUndocumentedScriptFuncTable: TScriptTable =
  [
    'function StrGet(var S: String; I: Integer): Char;',
    'function StrGet2(S: String; I: Integer): Char;',
    'procedure StrSet(C: Char; I: Integer; var S: String);',
    'function WStrGet(var S: AnyString; I: Integer): WideChar;',
    'procedure WStrSet(C: AnyString; I: Integer; var S: AnyString);',
    'function Sin(E: Extended): Extended;',
    'function Cos(E: Extended): Extended;',
    'function Sqrt(E: Extended): Extended;',
    'function Round(E: Extended): LongInt;',
    'function Trunc(E: Extended): LongInt;',
    'function Int(E: Extended): Extended;',
    'function Pi: Extended;',
    'function Abs(E: Extended): Extended;',
    'function PadL(S: AnyString; I: LongInt): AnyString;',
    'function PadR(S: AnyString; I: LongInt): AnyString;',
    'function PadZ(S: AnyString; I: LongInt): AnyString;',
    'function Replicate(C: Char; I: LongInt): String;',
    'procedure RaiseLastException;',
    'procedure RaiseException(Ex: TIFException; Param: String);',
    'function ExceptionType: TIFException;',
    'function ExceptionParam: String;',
    'function ExceptionProc: Cardinal;',
    'function ExceptionPos: Cardinal;',
    'function ExceptionToString(er: TIFException; Param: String): String;',
    'function Int64ToStr(I: Int64): String;'
  ]; }

  { All of the following are also just for Compil32 }

  ScriptConstsTable: TScriptTable = [
    { ROPS }
    'varEmpty', 'varNull', 'varSmallInt', 'varInteger', 'varSingle', 'varDouble',
    'varCurrency', 'varDate', 'varOleStr', 'varDispatch', 'varError', 'varBoolean',
    'varVariant', 'varUnknown', 'varShortInt', 'varByte', 'varWord', 'varLongWord',
    'varInt64', 'varStrArg', 'varAny', 'varString', 'varTypeMask', 'varArray',
    'varByRef', 'varUString',
    { ScriptFunc_C }
    'MaxInt', 'irInstall', 'wpWelcome', 'wpLicense', 'wpPassword', 'wpInfoBefore',
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
    { ScriptClasses_C }
    'clHotLight'
  ];

  ScriptInterfacesTable: TScriptTable = [
    { ROPS }
    'IUnknown',
    'IInterface',
    'IDispatch'
  ];

  ScriptKeywordsTable: TScriptTable = [
    { ROPS }
    'and', 'array', 'as', 'begin', 'case', 'chr', 'class', 'const', 'constructor',
    'destructor', 'div', 'do', 'downto', 'else', 'end', 'except', 'exit', 'export',
    'external', 'finalization', 'finally', 'for', 'forward', 'function', 'goto',
    'if', 'implementation', 'in', 'inherited', 'initialization', 'interface', 'is',
    'label', 'mod', 'nil', 'not', 'of', 'or', 'ord', 'out', 'override', 'private',
    'procedure', 'program', 'property', 'protected', 'public', 'published',
    'record', 'repeat', 'set', 'shl', 'shr', 'then', 'to', 'try', 'type', 'unit',
    'until', 'uses', 'var', 'virtual', 'while', 'with', 'xor',
    'False', 'True',
    'delayload', 'loadwithalteredsearchpath', 'stdcall', 'cdecl', 'register', 'pascal'
  ];

  ScriptTypesTable: TScriptTable = [
    { ROPS }
    'Byte', 'Boolean', 'LongBool', 'WordBool', 'ByteBool', 'AnsiChar', 'Char',
    'WideChar', 'WideString', 'UnicodeString', 'AnsiString', 'String', 'ShortInt',
    'Word', 'SmallInt', 'LongInt', 'LongWord', 'Integer', 'Cardinal', 'Int64',
    'Single', 'Double', 'Extended', 'Currency', 'PAnsiChar', 'Variant',
    'TVariantArray',
    //undocumented: NativeString, AnyString, AnyMethod, ___Pointer, tbtString, NativeString, !NotificationVariant
    'TVarType',
    //undocumented: TIFException
    { ScriptFunc_C }
    'TArrayOfString','TArrayOfChar','TArrayOfBoolean','TArrayOfInteger', 'DWORD',
    'UINT', 'BOOL', 'DWORD_PTR', 'UINT_PTR', 'INT_PTR', 'TFileTime', 'TMsgBoxType',
    'TSetupMessageID','TSetupStep','TUninstallStep','TSetupProcessorArchitecture',
    'TDotNetVersion', 'TExecWait', 'TExecOutput', 'TFindRec', 'TWindowsVersion',
    'TOnDownloadProgress', 'TOnLog'
  ];

  ScriptVariablesTable: TScriptTable = [
    { ScriptClasses_C }
    'WizardForm',
    'MainForm',
    'UninstallProgressForm'
  ];

{$ENDIF}

function ScriptFuncHasParameters(const ScriptFunc: AnsiString): Boolean;
function RemoveScriptFuncHeader(const ScriptFunc: AnsiString): AnsiString; overload;
function RemoveScriptFuncHeader(const ScriptFunc: AnsiString; out WasFunction: Boolean): AnsiString; overload;
function ExtractScriptFuncWithoutHeaderName(const ScriptFuncWithoutHeader: AnsiString): AnsiString;
function ExtractScriptFuncName(const ScriptFunc: AnsiString): AnsiString;

implementation

uses
  SysUtils, AnsiStrings;

function ScriptFuncHasParameters(const ScriptFunc: AnsiString): Boolean;
begin
  const C: AnsiString = '(';

  Result := Pos(C, ScriptFunc) <> 0;
end;

function RemoveScriptFuncHeader(const ScriptFunc: AnsiString): AnsiString;
begin
  var Dummy: Boolean;
  Result := RemoveScriptFuncHeader(ScriptFunc, Dummy);
end;

function RemoveScriptFuncHeader(const ScriptFunc: AnsiString; out WasFunction: Boolean): AnsiString;
begin
  Result := ScriptFunc;

  const H1: AnsiString = 'function ';
  const H2: AnsiString = 'procedure ';

  WasFunction := CompareText(Copy(Result, 1, Length(H1)), H1) = 0;

  if WasFunction then
    Delete(Result, 1, Length(H1))
  else if CompareText(Copy(Result, 1, Length(H2)), H2) = 0 then
    Delete(Result, 1, Length(H2))
  else
    raise Exception.CreateFmt('Invalid FunctionDefinition: %s', [Result]);
end;

function ExtractScriptFuncWithoutHeaderName(const ScriptFuncWithoutHeader: AnsiString): AnsiString;
begin
  Result := ScriptFuncWithoutHeader;

  const C1: AnsiString = '(';
  const C2: AnsiString = ':';
  const C3: AnsiString = ';';

  var P := Pos(C1, Result);
  if P = 0 then
    P := Pos(C2, Result);
  if P = 0 then
    P := Pos(C3, Result);
  if P = 0 then
    raise Exception.CreateFmt('Invalid FunctionDefinitionWithoutHeader: %s', [Result]);

  Delete(Result, P, Maxint);
end;

function ExtractScriptFuncName(const ScriptFunc: AnsiString): AnsiString;
begin
  Result := ExtractScriptFuncWithoutHeaderName(RemoveScriptFuncHeader(ScriptFunc));
end;

{$IFDEF COMPIL32PROJ}
{$IFDEF DEBUG}
function IsCleanScriptFunc(const ScriptFunc: AnsiString): Boolean;
begin
  const GoodTerminator: AnsiString = ';';
  const BadType1: AnsiString = 'string';
  const BadType2: AnsiString = 'Longint';

  Result := (Pos(GoodTerminator, ScriptFunc) <> 0) and
            (Pos(BadType1, ScriptFunc) = 0) and (Pos(BadType2, ScriptFunc) = 0) and
            (ScriptFunc[Length(ScriptFunc)] = ';');
end;

procedure CheckIsCleanScriptFuncTable(const ScriptFuncTable: TScriptTable);
begin
  if Length(ScriptFuncTable) = 0 then
    raise Exception.Create('Length(ScriptFuncTable) = 0');
  for var AScriptFunc in ScriptFuncTable do
    if not IsCleanScriptFunc(AScriptFunc) then
      raise Exception.CreateFmt('not IsCleanScriptFunc: %s', [AScriptFunc]);
end;
{$ENDIF}
{$ENDIF}

initialization

  { ScriptDlg }
  ScriptFuncTables[sftScriptDlg] :=
  [
    'function PageFromID(const ID: Integer): TWizardPage;',
    'function PageIndexFromID(const ID: Integer): Integer;',
    'function CreateCustomPage(const AfterID: Integer; const ACaption, ADescription: String): TWizardPage;',
    'function CreateInputQueryPage(const AfterID: Integer; const ACaption, ADescription, ASubCaption: String): TInputQueryWizardPage;',
    'function CreateInputOptionPage(const AfterID: Integer; const ACaption, ADescription, ASubCaption: String; Exclusive, ListBox: Boolean): TInputOptionWizardPage;',
    'function CreateInputDirPage(const AfterID: Integer; const ACaption, ADescription, ASubCaption: String; AAppendDir: Boolean; ANewFolderName: String): TInputDirWizardPage;',
    'function CreateInputFilePage(const AfterID: Integer; const ACaption, ADescription, ASubCaption: String): TInputFileWizardPage;',
    'function CreateOutputMsgPage(const AfterID: Integer; const ACaption, ADescription, AMsg: String): TOutputMsgWizardPage;',
    'function CreateOutputMsgMemoPage(const AfterID: Integer; const ACaption, ADescription, ASubCaption: String; const AMsg: AnsiString): TOutputMsgMemoWizardPage;',
    'function CreateOutputProgressPage(const ACaption, ADescription: String): TOutputProgressWizardPage;',
    'function CreateOutputMarqueeProgressPage(const ACaption, ADescription: String): TOutputMarqueeProgressWizardPage;',
    'function CreateDownloadPage(const ACaption, ADescription: String; const OnDownloadProgress: TOnDownloadProgress): TDownloadWizardPage;',
    'function ScaleX(X: Integer): Integer;',
    'function ScaleY(Y: Integer): Integer;',
    'function CreateCustomForm: TSetupForm;'
  ];

  { NewDisk }
  ScriptFuncTables[sftNewDisk] :=
  [
    'function SelectDisk(const DiskNumber: Integer; const AFilename: String; var Path: String): Boolean;'
  ];

  { BrowseFunc }
  ScriptFuncTables[sftBrowseFunc] :=
  [
    'function BrowseForFolder(const Prompt: String; var Directory: String; const NewFolderButton: Boolean): Boolean;',
    'function GetOpenFileName(const Prompt: String; var FileName: String; const InitialDirectory, Filter, DefaultExtension: String): Boolean;',
    'function GetOpenFileNameMulti(const Prompt: String; const FileNameList: TStrings; const InitialDirectory, Filter, DefaultExtension: String): Boolean;',
    'function GetSaveFileName(const Prompt: String; var FileName: String; const InitialDirectory, Filter, DefaultExtension: String): Boolean;'
  ];

  { CmnFunc }
  ScriptFuncTables[sftCmnFunc] :=
  [
    'function MinimizePathName(const Filename: String; const Font: TFont; MaxLen: Integer): String;'
  ];

   { CmnFunc2 }
  ScriptFuncTables[sftCmnFunc2] :=
  [
    'function FileExists(const Name: String): Boolean;',
    'function DirExists(const Name: String): Boolean;',
    'function FileOrDirExists(const Name: String): Boolean;',
    'function GetIniString(const Section, Key, Default, Filename: String): String;',
    'function GetIniInt(const Section, Key: String; const Default, Min, Max: LongInt; const Filename: String): LongInt;',
    'function GetIniBool(const Section, Key: String; const Default: Boolean; const Filename: String): Boolean;',
    'function IniKeyExists(const Section, Key, Filename: String): Boolean;',
    'function IsIniSectionEmpty(const Section, Filename: String): Boolean;',
    'function SetIniString(const Section, Key, Value, Filename: String): Boolean;',
    'function SetIniInt(const Section, Key: String; const Value: LongInt; const Filename: String): Boolean;',
    'function SetIniBool(const Section, Key: String; const Value: Boolean; const Filename: String): Boolean;',
    'procedure DeleteIniEntry(const Section, Key, Filename: String);',
    'procedure DeleteIniSection(const Section, Filename: String);',
    'function GetEnv(const EnvVar: String): String;',
    'function GetCmdTail: String;',
    'function ParamCount: Integer;',
    'function ParamStr(Index: Integer): String;',
    'function AddBackslash(const S: String): String;',
    'function RemoveBackslash(const S: String): String;',
    'function RemoveBackslashUnlessRoot(const S: String): String;',
    'function AddQuotes(const S: String): String;',
    'function RemoveQuotes(const S: String): String;',
    'function GetShortName(const LongName: String): String;',
    'function GetWinDir: String;',
    'function GetSystemDir: String;',
    'function GetSysWow64Dir: String;',
    'function GetSysNativeDir: String;',
    'function GetTempDir: String;',
    'function StringChange(var S: String; const FromStr, ToStr: String): Integer;',
    'function StringChangeEx(var S: String; const FromStr, ToStr: String; const SupportDBCS: Boolean): Integer;',
    'function UsingWinNT: Boolean;',
    'function FileCopy(const ExistingFile, NewFile: String; const FailIfExists: Boolean): Boolean;',
    'function ConvertPercentStr(var S: String): Boolean;',
    'function RegValueExists(const RootKey: Integer; const SubKeyName, ValueName: String): Boolean;',
    'function RegQueryStringValue(const RootKey: Integer; const SubKeyName, ValueName: String; var ResultStr: String): Boolean;',
    'function RegQueryMultiStringValue(const RootKey: Integer; const SubKeyName, ValueName: String; var ResultStr: String): Boolean;',
    'function RegDeleteKeyIncludingSubkeys(const RootKey: Integer; const SubkeyName: String): Boolean;',
    'function RegDeleteKeyIfEmpty(const RootKey: Integer; const SubkeyName: String): Boolean;',
    //not really in CmnFunc2
    'function RegKeyExists(const RootKey: Integer; const SubKeyName: String): Boolean;',
    'function RegDeleteValue(const RootKey: Integer; const SubKeyName, ValueName: String): Boolean;',
    'function RegGetSubkeyNames(const RootKey: Integer; const SubKeyName: String; var Names: TArrayOfString): Boolean;',
    'function RegGetValueNames(const RootKey: Integer; const SubKeyName: String; var Names: TArrayOfString): Boolean;',
    'function RegQueryDWordValue(const RootKey: Integer; const SubKeyName, ValueName: String; var ResultDWord: Cardinal): Boolean;',
    'function RegQueryBinaryValue(const RootKey: Integer; const SubKeyName, ValueName: String; var ResultStr: AnsiString): Boolean;',
    'function RegWriteStringValue(const RootKey: Integer; const SubKeyName, ValueName, Data: String): Boolean;',
    'function RegWriteExpandStringValue(const RootKey: Integer; const SubKeyName, ValueName, Data: String): Boolean;',
    'function RegWriteMultiStringValue(const RootKey: Integer; const SubKeyName, ValueName, Data: String): Boolean;',
    'function RegWriteDWordValue(const RootKey: Integer; const SubKeyName, ValueName: String; const Data: Cardinal): Boolean;',
    'function RegWriteBinaryValue(const RootKey: Integer; const SubKeyName, ValueName: String; const Data: AnsiString): Boolean;',
    //
    'function IsAdmin: Boolean;',
    'function IsAdminLoggedOn: Boolean;', { old name of IsAdmin }
    'function IsPowerUserLoggedOn: Boolean;',
    'function IsAdminInstallMode: Boolean;',
    'function FontExists(const FaceName: String): Boolean;',
    'function GetUILanguage: Integer;',
    'function AddPeriod(const S: String): String;',
    'function CharLength(const S: String; const Index: Integer): Integer;',
    'function SetNTFSCompression(const FileOrDir: String; Compress: Boolean): Boolean;',
    'function IsWildcard(const Pattern: String): Boolean;',
    'function WildcardMatch(const Text, Pattern: String): Boolean;'
  ];

  { Install }
  ScriptFuncTables[sftInstall] :=
  [
    'procedure ExtractTemporaryFile(const FileName: String);',
    'function ExtractTemporaryFiles(const Pattern: String): Integer;',
    'function DownloadTemporaryFile(const Url, FileName, RequiredSHA256OfFile: String; const OnDownloadProgress: TOnDownloadProgress): Int64;',
    'function DownloadTemporaryFileSize(const Url: String): Int64;',
    'function DownloadTemporaryFileDate(const Url: String): String;',
    'procedure SetDownloadCredentials(const User, Pass: String);'
  ];

  { InstFunc }
  ScriptFuncTables[sftInstFunc] :=
  [
    'function CheckForMutexes(Mutexes: String): Boolean;',
    'function DecrementSharedCount(const Is64Bit: Boolean; const Filename: String): Boolean;',
    'procedure DelayDeleteFile(const Filename: String; const Tries: Integer);',
    'function DelTree(const Path: String; const IsDir, DeleteFiles, DeleteSubdirsAlso: Boolean): Boolean;',
    'function GenerateUniqueName(Path: String; const Extension: String): String;',
    'function GetComputerNameString: String;',
    //function GetFileDateTime(const Filename: String; var DateTime: TFileTime): Boolean;
    'function GetMD5OfFile(const Filename: String): String;',
    'function GetMD5OfString(const S: AnsiString): String;',
    'function GetMD5OfUnicodeString(const S: String): String;',
    'function GetSHA1OfFile(const Filename: String): String;',
    'function GetSHA1OfString(const S: AnsiString): String;',
    'function GetSHA1OfUnicodeString(const S: String): String;',
    'function GetSHA256OfFile(const Filename: String): String;',
    'function GetSHA256OfString(const S: AnsiString): String;',
    'function GetSHA256OfUnicodeString(const S: String): String;',
    'function GetSpaceOnDisk(const DriveRoot: String; const InMegabytes: Boolean; var Free, Total: Cardinal): Boolean;',
    'function GetSpaceOnDisk64(const DriveRoot: String; var Free, Total: Int64): Boolean;',
    'function GetUserNameString: String;',
    //function GrantPermissionOnFile(const Filename: String; const Entries: TGrantPermissionEntry; const EntryCount: Integer): Boolean;
    //function GrantPermissionOnKey(const RootKey: HKEY; const Subkey: String; const Entries: TGrantPermissionEntry; const EntryCount: Integer): Boolean;
    'procedure IncrementSharedCount(const Is64Bit: Boolean; const Filename: String; const AlreadyExisted: Boolean);',
    'function Exec(const Filename, Params, WorkingDir: String; const ShowCmd: Integer; const Wait: TExecWait; var ResultCode: Integer): Boolean;',
    'function ExecAndCaptureOutput(const Filename, Params, WorkingDir: String; const ShowCmd: Integer; const Wait: TExecWait; var ResultCode: Integer; var Output: TExecOutput): Boolean;',
    'function ExecAndLogOutput(const Filename, Params, WorkingDir: String; const ShowCmd: Integer; const Wait: TExecWait; var ResultCode: Integer; const OnLog: TOnLog): Boolean;',
    'function ExecAsOriginalUser(const Filename, Params, WorkingDir: String; const ShowCmd: Integer; const Wait: TExecWait; var ResultCode: Integer): Boolean;',
    'function ShellExec(const Verb, Filename, Params, WorkingDir: String; const ShowCmd: Integer; const Wait: TExecWait; var ErrorCode: Integer): Boolean;',
    'function ShellExecAsOriginalUser(const Verb, Filename, Params, WorkingDir: String; const ShowCmd: Integer; const Wait: TExecWait; var ErrorCode: Integer): Boolean;',
    'function IsProtectedSystemFile(const Filename: String): Boolean;',
    'function MakePendingFileRenameOperationsChecksum: String;',
    'function ModifyPifFile(const Filename: String; const CloseOnExit: Boolean): Boolean;',
    'procedure RegisterServer(const Is64Bit: Boolean; const Filename: String; const FailCriticalErrors: Boolean);',
    'function UnregisterServer(const Is64Bit: Boolean; const Filename: String; const FailCriticalErrors: Boolean): Boolean;',
    'procedure UnregisterFont(const FontName, FontFilename: String; const PerUserFont: Boolean);',
    //procedure RestartComputer;
    'procedure RestartReplace(const TempFile, DestFile: String);',
    //procedure Win32ErrorMsg(const FunctionName: String);
    'function ForceDirectories(Dir: String): Boolean;'
  ];

  { InstFnc2 }
  ScriptFuncTables[sftInstFnc2] :=
  [
    'function CreateShellLink(const Filename, Description, ShortcutTo, Parameters, WorkingDir, IconFilename: String; const IconIndex, ShowCmd: Integer): String;',
    'procedure RegisterTypeLibrary(const Is64Bit: Boolean; const Filename: String);',
    'function UnregisterTypeLibrary(const Is64Bit: Boolean; const Filename: String): Boolean;',
    'function UnpinShellLink(const Filename: String): Boolean;'
  ];

  { Main }
  ScriptFuncTables[sftMain] :=
  [
    'function GetWizardForm: TWizardForm;',
    'function GetMainForm: TMainForm;',
    'function ActiveLanguage: String;',
    'function WizardIsComponentSelected(const Components: String): Boolean;',
    'function IsComponentSelected(const Components: String): Boolean;', { old name of WizardIsComponentSelected }
    'function WizardIsTaskSelected(const Tasks: String): Boolean;',
    'function IsTaskSelected(const Tasks: String): Boolean;', { old name of WizardIsTaskSelected }
    'function ExpandConstant(const S: String): String;',
    'function ExpandConstantEx(const S: String; const CustomConst, CustomValue: String): String;',
    'function ExitSetupMsgBox: Boolean;',
    'function GetShellFolderByCSIDL(const Folder: Integer; const Create: Boolean): String;',
    'function InstallOnThisVersion(const MinVersion, OnlyBelowVersion: String): Boolean;',
    'function GetWindowsVersion: Cardinal;',
    'function GetWindowsVersionString: String;',
    'function MsgBox(const Text: String; const Typ: TMsgBoxType; const Buttons: Integer): Integer;',
    'function SuppressibleMsgBox(const Text: String; const Typ: TMsgBoxType; const Buttons, Default: Integer): Integer;',
    'function TaskDialogMsgBox(const Instruction, Text: String; const Typ: TMsgBoxType; const Buttons: Cardinal; const ButtonLabels: TArrayOfString; const ShieldButton: Integer): Integer;',
    'function SuppressibleTaskDialogMsgBox(const Instruction, Text: String; const Typ: TMsgBoxType; const Buttons: Cardinal; const ButtonLabels: TArrayOfString; const ShieldButton: Integer;'+' const Default: Integer): Integer;',
    'function IsWin64: Boolean;',
    'function Is64BitInstallMode: Boolean;',
    'function ProcessorArchitecture: TSetupProcessorArchitecture;',
    'function IsArm32Compatible: Boolean;',
    'function IsArm64: Boolean;',
    'function IsX64: Boolean;',
    'function IsX64OS: Boolean;',
    'function IsX64Compatible: Boolean;',
    'function IsX86: Boolean;',
    'function IsX86OS: Boolean;',
    'function IsX86Compatible: Boolean;',
    'function CustomMessage(const MsgName: String): String;',
    'function RmSessionStarted: Boolean;',
    'function RegisterExtraCloseApplicationsResource(const DisableFsRedir: Boolean; const AFilename: String): Boolean;'
  ];

  { Msgs }
  ScriptFuncTables[sftMsgs] :=
  [
    'function SetupMessage(const ID: TSetupMessageID): String;'
  ];

  { System }
  ScriptFuncTables[sftSystem] :=
  [
    'function Random(const Range: Integer): Integer;',
    'function FileSize(const Name: String; var Size: Integer): Boolean;',
    'function FileSize64(const Name: String; var Size: Int64): Boolean;',
    'procedure Set8087CW(NewCW: Word);',
    'function Get8087CW: Word;',
    'function UTF8Encode(const S: String): AnsiString;',
    'function UTF8Decode(const S: AnsiString): String;'
  ];

  { SysUtils }
  ScriptFuncTables[sftSysUtils] :=
  [
    'procedure Beep;',
    'function TrimLeft(const S: String): String;',
    'function TrimRight(const S: String): String;',
    'function GetCurrentDir: String;',
    'function SetCurrentDir(const Dir: String): Boolean;',
    'function ExpandFileName(const FileName: String): String;',
    'function ExpandUNCFileName(const FileName: String): String;',
    'function ExtractRelativePath(const BaseName, DestName: String): String;',
    'function ExtractFileDir(const FileName: String): String;',
    'function ExtractFileDrive(const FileName: String): String;',
    'function ExtractFileExt(const FileName: String): String;',
    'function ExtractFileName(const FileName: String): String;',
    'function ExtractFilePath(const FileName: String): String;',
    'function ChangeFileExt(const FileName, Extension: String): String;',
    'function FileSearch(const Name, DirList: String): String;',
    'function RenameFile(const OldName, NewName: String): Boolean;',
    'function DeleteFile(const FileName: String): Boolean;',
    'function CreateDir(const Dir: String): Boolean;',
    'function RemoveDir(const Dir: String): Boolean;',
    'function CompareStr(const S1, S2: String): Integer;',
    'function CompareText(const S1, S2: String): Integer;',
    'function SameStr(const S1, S2: String): Boolean;',
    'function SameText(const S1, S2: String): Boolean;',
    'function GetDateTimeString(const DateTimeFormat: String; const DateSeparator, TimeSeparator: Char): String;',
    'function SysErrorMessage(ErrorCode: Integer): String;'
  ];

  { VerInfo }
  ScriptFuncTables[sftVerInfo] :=
  [
    'function GetVersionNumbers(const Filename: String; var VersionMS, VersionLS: Cardinal): Boolean;',
    'function GetVersionComponents(const Filename: String; var Major, Minor, Revision, Build: Word): Boolean;',
    'function GetVersionNumbersString(const Filename: String; var Version: String): Boolean;',
    'function GetPackedVersion(const Filename: String; var Version: Int64): Boolean;',
    'function PackVersionNumbers(const VersionMS, VersionLS: Cardinal): Int64;',
    'function PackVersionComponents(const Major, Minor, Revision, Build: Word): Int64;',
    'function ComparePackedVersion(const Version1, Version2: Int64): Integer;',
    'function SamePackedVersion(const Version1, Version2: Int64): Boolean;',
    'procedure UnpackVersionNumbers(const Version: Int64; var VersionMS, VersionLS: Cardinal);',
    'procedure UnpackVersionComponents(const Version: Int64; var Major, Minor, Revision, Build: Word);',
    'function VersionToStr(const Version: Int64): String;',
    'function StrToVersion(const VersionString: String; var Version: Int64): Boolean;'
  ];

  { Windows }
  ScriptFuncTables[sftWindows] :=
  [
    'procedure Sleep(const Milliseconds: LongInt);',
    'function FindWindowByClassName(const ClassName: String): HWND;',
    'function FindWindowByWindowName(const WindowName: String): HWND;',
    'function SendMessage(const Wnd: HWND; const Msg, WParam, LParam: LongInt): LongInt;',
    'function PostMessage(const Wnd: HWND; const Msg, WParam, LParam: LongInt): Boolean;',
    'function SendNotifyMessage(const Wnd: HWND; const Msg, WParam, LParam: LongInt): Boolean;',
    'function RegisterWindowMessage(const Name: String): LongInt;',
    'function SendBroadcastMessage(const Msg, WParam, LParam: LongInt): LongInt;',
    'function PostBroadcastMessage(const Msg, WParam, LParam: LongInt): Boolean;',
    'function SendBroadcastNotifyMessage(const Msg, WParam, LParam: LongInt): Boolean;',
    'function LoadDLL(const DLLName: String; var ErrorCode: Integer): LongInt;',
    'function CallDLLProc(const DLLHandle: LongInt; const ProcName: String; const Param1, Param2: LongInt; var Result: LongInt): Boolean;',
    'function FreeDLL(const DLLHandle: LongInt): Boolean;',
    'procedure CreateMutex(const Name: String);',
    'procedure OemToCharBuff(var S: AnsiString);',
    'procedure CharToOemBuff(var S: AnsiString);'
  ];

  { Ole2 }
  ScriptFuncTables[sftOle2] :=
  [
    'procedure CoFreeUnusedLibraries;'
  ];

  { Logging }
  ScriptFuncTables[sftLogging] :=
  [
    'procedure Log(const S: String);'
  ];

  { Other }
  ScriptFuncTables[sftOther] :=
  [
    'procedure BringToFrontAndRestore;',
    'function WizardDirValue: String;',
    'function WizardGroupValue: String;',
    'function WizardNoIcons: Boolean;',
    'function WizardSetupType(const Description: Boolean): String;',
    'function WizardSelectedComponents(const Descriptions: Boolean): String;',
    'function WizardSelectedTasks(const Descriptions: Boolean): String;',
    'procedure WizardSelectComponents(const Components: String);',
    'procedure WizardSelectTasks(const Tasks: String);',
    'function WizardSilent: Boolean;',
    'function IsUninstaller: Boolean;',
    'function UninstallSilent: Boolean;',
    'function CurrentFilename: String;',
    'function CurrentSourceFilename: String;',
    'function CastStringToInteger(var S: String): LongInt;',
    'function CastIntegerToString(const L: LongInt): String;',
    'procedure Abort;',
    'function GetExceptionMessage: String;',
    'procedure RaiseException(const Msg: String);',
    'procedure ShowExceptionMessage;',
    'function Terminated: Boolean;',
    'function GetPreviousData(const ValueName, DefaultValueData: String): String;',
    'function SetPreviousData(const PreviousDataKey: Integer; const ValueName, ValueData: String): Boolean;',
    'function LoadStringFromFile(const FileName: String; var S: AnsiString): Boolean;',
    'function LoadStringFromLockedFile(const FileName: String; var S: AnsiString): Boolean;',
    'function LoadStringsFromFile(const FileName: String; var S: TArrayOfString): Boolean;',
    'function LoadStringsFromLockedFile(const FileName: String; var S: TArrayOfString): Boolean;',
    'function SaveStringToFile(const FileName: String; const S: AnsiString; const Append: Boolean): Boolean;',
    'function SaveStringsToFile(const FileName: String; const S: TArrayOfString; const Append: Boolean): Boolean;',
    'function SaveStringsToUTF8File(const FileName: String; const S: TArrayOfString; const Append: Boolean): Boolean;',
    'function SaveStringsToUTF8FileWithoutBOM(const FileName: String; const S: TArrayOfString; const Append: Boolean): Boolean;',
    'function EnableFsRedirection(const Enable: Boolean): Boolean;',
    'function GetUninstallProgressForm: TUninstallProgressForm;',
    'function CreateCallback(Method: AnyMethod): Longword;',
    'function IsDotNetInstalled(const MinVersion: TDotNetVersion; const MinServicePack: Cardinal): Boolean;',
    'function IsMsiProductInstalled(const UpgradeCode: String; const PackedMinVersion: Int64): Boolean;',
    'function InitializeBitmapImageFromIcon(const BitmapImage: TBitmapImage; const IconFilename: String; const BkColor: TColor; const AscendingTrySizes: TArrayOfInteger): Boolean;'
  ];

  {$IFDEF COMPIL32PROJ}
  {$IFDEF DEBUG}
  for var ScriptFuncTable in ScriptFuncTables do
    CheckIsCleanScriptFuncTable(ScriptFuncTable);
  CheckIsCleanScriptFuncTable(DelphiScriptFuncTable);
  CheckIsCleanScriptFuncTable(ROPSScriptFuncTable);
  {$ENDIF}
  {$ENDIF}

end.
