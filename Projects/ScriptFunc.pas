unit ScriptFunc;

{
  Inno Setup
  Copyright (C) 1997-2019 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Script support functions
}

interface

const

  { ScriptDlg }
  ScriptDlgTable: array [0..12] of AnsiString =
  (
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
    'function ScaleX(X: Integer): Integer;',
    'function ScaleY(Y: Integer): Integer;',
    'function CreateCustomForm: TSetupForm;'
  );

  { NewDisk }
  NewDiskTable: array [0..0] of AnsiString =
  (
    'function SelectDisk(const DiskNumber: Integer; const AFilename: String; var Path: String): Boolean;'
  );

  { BrowseFunc }
  BrowseFuncTable: array [0..3] of AnsiString =
  (
    'function BrowseForFolder(const Prompt: String; var Directory: String; const NewFolderButton: Boolean): Boolean;',
    'function GetOpenFileName(const Prompt: String; var FileName: String; const InitialDirectory, Filter, DefaultExtension: String): Boolean;',
    'function GetOpenFileNameMulti(const Prompt: String; const FileNameList: TStrings; const InitialDirectory, Filter, DefaultExtension: String): Boolean;',
    'function GetSaveFileName(const Prompt: String; var FileName: String; const InitialDirectory, Filter, DefaultExtension: String): Boolean;'
  );

  { CmnFunc }
  CmnFuncTable: array [0..0] of AnsiString =
  (
    'function MinimizePathName(const Filename: String; const Font: TFont; MaxLen: Integer): String;'
  );

   { CmnFunc2 }
  CmnFunc2Table: array [0..59] of AnsiString =
  (
    'function FileExists(const Name: String): Boolean;',
    'function DirExists(const Name: String): Boolean;',
    'function FileOrDirExists(const Name: String): Boolean;',
    'function GetIniString(const Section, Key, Default, Filename: String): String;',
    'function GetIniInt(const Section, Key: String; const Default, Min, Max: Longint; const Filename: String): Longint;',
    'function GetIniBool(const Section, Key: String; const Default: Boolean; const Filename: String): Boolean;',
    'function IniKeyExists(const Section, Key, Filename: String): Boolean;',
    'function IsIniSectionEmpty(const Section, Filename: String): Boolean;',
    'function SetIniString(const Section, Key, Value, Filename: String): Boolean;',
    'function SetIniInt(const Section, Key: String; const Value: Longint; const Filename: String): Boolean;',
    'function SetIniBool(const Section, Key: String; const Value: Boolean; const Filename: String): Boolean;',
    'procedure DeleteIniEntry(const Section, Key, Filename: String);',
    'procedure DeleteIniSection(const Section, Filename: String);',
    'function GetEnv(const EnvVar: String): String;',
    'function GetCmdTail: String;',
    'function ParamCount: Integer;',
    'function ParamStr(Index: Integer): string;',
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
  );

  { Install }
  InstallTable: array [0..1] of AnsiString =
  (
    'procedure ExtractTemporaryFile(const FileName: String);',
    'function ExtractTemporaryFiles(const Pattern: String): Integer;'
  );

  { InstFunc }
{$IFNDEF PS_NOINT64}
  InstFuncTable: array [0..27] of AnsiString =
{$ELSE}
  InstFuncTable: array [0..26] of AnsiString =
{$ENDIF}
  (
    'function CheckForMutexes(Mutexes: String): Boolean;',
    'function DecrementSharedCount(const Is64Bit: Boolean; const Filename: String): Boolean;',
    'procedure DelayDeleteFile(const Filename: String; const Tries: Integer);',
    'function DelTree(const Path: String; const IsDir, DeleteFiles, DeleteSubdirsAlso: Boolean): Boolean;',
    'function GenerateUniqueName(Path: String; const Extension: String): String;',
    'function GetComputerNameString: String;',
    //function GetFileDateTime(const Filename: string; var DateTime: TFileTime): Boolean;
    'function GetMD5OfFile(const Filename: String): String;',
    'function GetMD5OfString(const S: AnsiString): String;',
    'function GetMD5OfUnicodeString(const S: String): String;',
    'function GetSHA1OfFile(const Filename: String): String;',
    'function GetSHA1OfString(const S: AnsiString): String;',
    'function GetSHA1OfUnicodeString(const S: String): String;',
    'function GetSpaceOnDisk(const DriveRoot: String; const InMegabytes: Boolean; var Free, Total: Cardinal): Boolean;',
{$IFNDEF PS_NOINT64}
    'function GetSpaceOnDisk64(const DriveRoot: String; var Free, Total: Int64): Boolean;',
{$ENDIF}
    'function GetUserNameString: String;',
    //function GrantPermissionOnFile(const Filename: String; const Entries: TGrantPermissionEntry; const EntryCount: Integer): Boolean;
    //function GrantPermissionOnKey(const RootKey: HKEY; const Subkey: String; const Entries: TGrantPermissionEntry; const EntryCount: Integer): Boolean;
    'procedure IncrementSharedCount(const Is64Bit: Boolean; const Filename: String; const AlreadyExisted: Boolean);',
    'function Exec(const Filename, Params, WorkingDir: String; const ShowCmd: Integer; const Wait: TExecWait; var ResultCode: Integer): Boolean;',
    'function ExecAsOriginalUser(const Filename, Params, WorkingDir: String; const ShowCmd: Integer; const Wait: TExecWait; var ResultCode: Integer): Boolean;',
    'function ShellExec(const Verb, Filename, Params, WorkingDir: String; const ShowCmd: Integer; const Wait: TExecWait; var ErrorCode: Integer): Boolean;',
    'function ShellExecAsOriginalUser(const Verb, Filename, Params, WorkingDir: String; const ShowCmd: Integer; const Wait: TExecWait; var ErrorCode: Integer): Boolean;',
    'function IsProtectedSystemFile(const Filename: String): Boolean;',
    'function MakePendingFileRenameOperationsChecksum: String;',
    'function ModifyPifFile(const Filename: String; const CloseOnExit: Boolean): Boolean;',
    'procedure RegisterServer(const Is64Bit: Boolean; const Filename: String; const FailCriticalErrors: Boolean);',
    'function UnregisterServer(const Is64Bit: Boolean; const Filename: String; const FailCriticalErrors: Boolean): Boolean;',
    'procedure UnregisterFont(const FontName, FontFilename: String);',
    //procedure RestartComputer;
    'procedure RestartReplace(const TempFile, DestFile: String);',
    //procedure Win32ErrorMsg(const FunctionName: String);
    'function ForceDirectories(Dir: string): Boolean;'
  );
  
  { InstFnc2 }
  InstFnc2Table: array [0..3] of AnsiString =
  (
    'function CreateShellLink(const Filename, Description, ShortcutTo, Parameters, WorkingDir, IconFilename: String; const IconIndex, ShowCmd: Integer): String;',
    'procedure RegisterTypeLibrary(const Is64Bit: Boolean; const Filename: String);',
    'function UnregisterTypeLibrary(const Is64Bit: Boolean; const Filename: String): Boolean;',
    'function UnpinShellLink(const Filename: String): Boolean;'
  );

  { Main }
  MainTable: array [0..28] of AnsiString =
  (
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
    'procedure GetWindowsVersionEx(var Version: TWindowsVersion);',
    'function GetWindowsVersionString: String;',
    'function MsgBox(const Text: String; const Typ: TMsgBoxType; const Buttons: Integer): Integer;',
    'function SuppressibleMsgBox(const Text: String; const Typ: TMsgBoxType; const Buttons, Default: Integer): Integer;',
    'function TaskDialogMsgBox(const Instruction, Text: String; const Typ: TMsgBoxType; const Buttons: Cardinal; const ButtonLabels: TArrayOfString; const ShieldButton: Integer): Integer;',
    'function SuppressibleTaskDialogMsgBox(const Instruction, Text: String; const Typ: TMsgBoxType; const Buttons: Cardinal; const ButtonLabels: TArrayOfString; const ShieldButton: Integer;'+' const Default: Integer): Integer;',
    'function IsWin64: Boolean;',
    'function Is64BitInstallMode: Boolean;',
    'function ProcessorArchitecture: TSetupProcessorArchitecture;',
    'function IsX86: Boolean;',
    'function IsX64: Boolean;',
    'function IsIA64: Boolean;',
    'function IsARM64: Boolean;',
    'function CustomMessage(const MsgName: String): String;',
    'function RmSessionStarted: Boolean;',
    'function RegisterExtraCloseApplicationsResource(const DisableFsRedir: Boolean; const AFilename: String): Boolean;'
  );

  { Msgs }
  MsgsTable: array[0..0] of AnsiString =
  (
    'function SetupMessage(const ID: TSetupMessageID): String;'
  );
  MsgsDelphiTable: array[0..0] of AnsiString =
  (
    'function FmtMessage(const S: String; const Args: array of String): String;'
  );

  { System }
  SystemTable: array [0..3] of AnsiString =
  (
    'function Random(const Range: Integer): Integer;',
    'function FileSize(const Name: String; var Size: Integer): Boolean;',
    'procedure Set8087CW(NewCW: Word);',
    'function Get8087CW: Word;'
  );

  { SysUtils }
  SysUtilsTable: array [0..25] of AnsiString =
  (
    'procedure Beep;',
    'function Trim(const S: string): string;',
    'function TrimLeft(const S: string): string;',
    'function TrimRight(const S: string): string;',
    'function GetCurrentDir: string;',
    'function SetCurrentDir(const Dir: string): Boolean;',
    'function ExpandFileName(const FileName: string): string;',
    'function ExpandUNCFileName(const FileName: string): string;',
    'function ExtractRelativePath(const BaseName, DestName: string): string;',
    'function ExtractFileDir(const FileName: string): string;',
    'function ExtractFileDrive(const FileName: string): string;',
    'function ExtractFileExt(const FileName: string): string;',
    'function ExtractFileName(const FileName: string): string;',
    'function ExtractFilePath(const FileName: string): string;',
    'function ChangeFileExt(const FileName, Extension: string): string;',
    'function FileSearch(const Name, DirList: string): string;',
    'function RenameFile(const OldName, NewName: string): Boolean;',
    'function DeleteFile(const FileName: string): Boolean;',
    'function CreateDir(const Dir: string): Boolean;',
    'function RemoveDir(const Dir: string): Boolean;',
    'function CompareStr(const S1, S2: string): Integer;',
    'function CompareText(const S1, S2: string): Integer;',
    'function SameStr(const S1, S2: string): Boolean;',
    'function SameText(const S1, S2: string): Boolean;',
    'function GetDateTimeString(const DateTimeFormat: String; const DateSeparator, TimeSeparator: Char): String;',
    'function SysErrorMessage(ErrorCode: Integer): String;'
  );
  SysUtilsDelphiTable: array [0..3] of AnsiString =
  (
    'function FindFirst(const FileName: String; var FindRec: TFindRec): Boolean;',
    'function FindNext(var FindRec: TFindRec): Boolean;',
    'procedure FindClose(var FindRec: TFindRec);',
    'function Format(const Format: string; const Args: array of const): string;'
  );

  { VerInfo }
  VerInfoTable: array [0..1] of AnsiString =
  (
    'function GetVersionNumbers(const Filename: String; var VersionMS, VersionLS: Cardinal): Boolean;',
    'function GetVersionNumbersString(const Filename: String; var Version: String): Boolean;'
  );

  { Windows }
  WindowsTable: array [0..15] of AnsiString =
  (
    'procedure Sleep(const Milliseconds: LongInt);',
    'function FindWindowByClassName(const ClassName: String): HWND;',
    'function FindWindowByWindowName(const WindowName: String): HWND;',
    'function SendMessage(const Wnd: HWND; const Msg, WParam, LParam: Longint): Longint;',
    'function PostMessage(const Wnd: HWND; const Msg, WParam, LParam: Longint): Boolean;',
    'function SendNotifyMessage(const Wnd: HWND; const Msg, WParam, LParam: Longint): Boolean;',
    'function RegisterWindowMessage(const Name: String): Longint;',
    'function SendBroadcastMessage(const Msg, WParam, LParam: Longint): Longint;',
    'function PostBroadcastMessage(const Msg, WParam, LParam: Longint): Boolean;',
    'function SendBroadcastNotifyMessage(const Msg, WParam, LParam: Longint): Boolean;',
    'function LoadDLL(const DLLName: String; var ErrorCode: Integer): Longint;',
    'function CallDLLProc(const DLLHandle: Longint; const ProcName: String; const Param1, Param2: Longint; var Result: Longint): Boolean;',
    'function FreeDLL(const DLLHandle: Longint): Boolean;',
    'procedure CreateMutex(const Name: String);',
    'procedure OemToCharBuff(var S: AnsiString);',
    'procedure CharToOemBuff(var S: AnsiString);'
  );

  { Ole2 }
  Ole2Table: array [0..0] of AnsiString =
  (
    'procedure CoFreeUnusedLibraries;'
  );

  { Logging }
  LoggingTable: array [0..0] of AnsiString =
  (
    'procedure Log(const S: String);'
  );

  { Other }
  OtherTable: array [0..30] of AnsiString =
  (
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
    'function CastStringToInteger(var S: String): Longint;',
    'function CastIntegerToString(const L: Longint): String;',
    'procedure Abort;',
    'function GetExceptionMessage: String;',
    'procedure RaiseException(const Msg: String);',
    'procedure ShowExceptionMessage;',
    'function Terminated: Boolean;',
    'function GetPreviousData(const ValueName, DefaultValueData: String): String;',
    'function SetPreviousData(const PreviousDataKey: Integer; const ValueName, ValueData: String): Boolean;',
    'function LoadStringFromFile(const FileName: String; var S: AnsiString): Boolean;',
    'function LoadStringsFromFile(const FileName: String; var S: TArrayOfString): Boolean;',
    'function SaveStringToFile(const FileName: String; const S: AnsiString; const Append: Boolean): Boolean;',
    'function SaveStringsToFile(const FileName: String; const S: TArrayOfString; const Append: Boolean): Boolean;',
    'function SaveStringsToUTF8File(const FileName: String; const S: TArrayOfString; const Append: Boolean): Boolean;',
    'function EnableFsRedirection(const Enable: Boolean): Boolean;',
    'function GetUninstallProgressForm: TUninstallProgressForm;',
    'function CreateCallback(Method: AnyMethod): Longword;'
  );

implementation

end.
