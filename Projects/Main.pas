unit Main;

{
  Inno Setup
  Copyright (C) 1997-2011 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Background form
}

interface

{$I VERSION.INC}

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  SetupForm, StdCtrls, Struct, DebugStruct, Int64Em, CmnFunc, CmnFunc2,
  SetupTypes, ScriptRunner, BidiUtils, RestartManager;

type
  TMainForm = class(TSetupForm)
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormPaint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    IsMinimized, HideWizard: Boolean;
    function MainWindowHook(var Message: TMessage): Boolean;
    procedure UpdateWizardFormVisibility;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
  public
    { Public declarations }
    CurStep: TSetupStep;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Finish(const FromPreparingPage: Boolean);
    procedure InitializeWizard;
    function Install: Boolean;
    procedure SetStep(const AStep: TSetupStep; const HandleExceptions: Boolean);
    class procedure ShowException(Sender: TObject; E: Exception);
    class procedure ShowExceptionMsg(const S: String);
    procedure ShowAboutBox;
  end;

  TEntryType = (seLanguage, seCustomMessage, sePermission, seType, seComponent,
    seTask, seDir, seFile, seFileLocation, seIcon, seIni, seRegistry,
    seInstallDelete, seUninstallDelete, seRun, seUninstallRun);

const
  EntryStrings: array[TEntryType] of Integer = (SetupLanguageEntryStrings,
    SetupCustomMessageEntryStrings, SetupPermissionEntryStrings,
    SetupTypeEntryStrings, SetupComponentEntryStrings, SetupTaskEntryStrings,
    SetupDirEntryStrings, SetupFileEntryStrings, SetupFileLocationEntryStrings,
    SetupIconEntryStrings, SetupIniEntryStrings, SetupRegistryEntryStrings,
    SetupDeleteEntryStrings, SetupDeleteEntryStrings, SetupRunEntryStrings,
    SetupRunEntryStrings);

  EntryAnsiStrings: array[TEntryType] of Integer = (SetupLanguageEntryAnsiStrings,
    SetupCustomMessageEntryAnsiStrings, SetupPermissionEntryAnsiStrings,
    SetupTypeEntryAnsiStrings, SetupComponentEntryAnsiStrings, SetupTaskEntryAnsiStrings,
    SetupDirEntryAnsiStrings, SetupFileEntryAnsiStrings, SetupFileLocationEntryAnsiStrings,
    SetupIconEntryAnsiStrings, SetupIniEntryAnsiStrings, SetupRegistryEntryAnsiStrings,
    SetupDeleteEntryAnsiStrings, SetupDeleteEntryAnsiStrings, SetupRunEntryAnsiStrings,
    SetupRunEntryAnsiStrings);

  { Exit codes that are assigned to the SetupExitCode variable.
    Note: SetupLdr also returns exit codes with the same numbers. }
  ecInitializationError = 1;     { Setup failed to initialize. }
  ecCancelledBeforeInstall = 2;  { User clicked Cancel before the actual
                                   installation started. }
  ecNextStepError = 3;           { A fatal exception occurred while moving to
                                   the next step. }
  ecInstallationError = 4;       { A fatal exception occurred during
                                   installation. }
  ecInstallationCancelled = 5;   { User clicked Cancel during installation,
                                   or clicked Abort at an Abort-Retry-Ignore
                                   dialog. }
  ecKilledByDebugger = 6;        { User killed the Setup process from within
                                   the debugger. }
  ecPrepareToInstallFailed = 7;  { Stopped on Preparing to Install page;
                                   restart not needed. }
  ecPrepareToInstallFailedRestartNeeded = 8;
                                 { Stopped on Preparing to Install page;
                                   restart needed. }

var
  MainForm: TMainForm;

  { Variables for command line parameters }
  SetupLdrMode: Boolean;
  SetupLdrOriginalFilename: String;
  SetupLdrOffset0, SetupLdrOffset1: Longint;
  SetupNotifyWndPresent: Boolean;
  SetupNotifyWnd: HWND;
  InitLang: String;
  InitDir, InitProgramGroup: String;
  InitLoadInf, InitSaveInf: String;
  InitNoIcons, InitSilent, InitVerySilent, InitNoRestart, InitNoCloseApplications,
    InitNoRestartApplications, InitNoCancel: Boolean;
  InitSetupType: String;
  InitComponents, InitTasks: TStringList;
  InitComponentsSpecified: Boolean;
  InitDeselectAllTasks: Boolean;
  InitPassword: String;
  InitRestartExitCode: Integer;
  InitSuppressMsgBoxes: Boolean;
  DetachedUninstMsgFile: Boolean;

  { Debugger }
  OriginalEntryIndexes: array[TEntryType] of TList;

  { 'Constants' }
  SourceDir, TempInstallDir, WinDir, WinSystemDir, WinSysWow64Dir, SystemDrive,
    ProgramFiles32Dir, CommonFiles32Dir, ProgramFiles64Dir, CommonFiles64Dir,
    CmdFilename, SysUserInfoName, SysUserInfoOrg, UninstallExeFilename: String;

  { Uninstall 'constants' }
  UninstallExpandedAppId, UninstallExpandedApp, UninstallExpandedGroup,
  UninstallExpandedGroupName, UninstallExpandedLanguage: String;
  UninstallSilent: Boolean;

  { Variables read in from the SETUP.0 file }
  SetupHeader: TSetupHeader;
  LangOptions: TSetupLanguageEntry;
  Entries: array[TEntryType] of TList;
  WizardImage: TBitmap;
  WizardSmallImage: TBitmap;
  CloseApplicationsFilterList: TStringList;

  { User options }
  ActiveLanguage: Integer = -1;
  ActiveLicenseText, ActiveInfoBeforeText, ActiveInfoAfterText: AnsiString;
  WizardUserInfoName, WizardUserInfoOrg, WizardUserInfoSerial, WizardDirValue, WizardGroupValue: String;
  WizardNoIcons, WizardPreparingYesRadio: Boolean;
  WizardSetupType: PSetupTypeEntry;
  WizardComponents, WizardDeselectedComponents, WizardTasks, WizardDeselectedTasks: TStringList;
  NeedToAbortInstall: Boolean;

  { Check/BeforeInstall/AfterInstall 'contants' }
  CheckOrInstallCurrentFileName: String;

  { RestartManager API state.
    Note: the handle and key might change while running, see TWizardForm.QueryRestartManager. }
  RmSessionStarted, RmFoundApplications, RmDoRestart: Boolean;
  RmSessionHandle: DWORD;
  RmSessionKey: array[0..CCH_RM_SESSION_KEY] of WideChar;

  { Other }
  ShowLanguageDialog: Boolean;
  InstallMode: (imNormal, imSilent, imVerySilent);
  HasIcons, IsNT, IsWin64, Is64BitInstallMode, IsAdmin, IsPowerUserOrAdmin,
    NeedPassword, NeedSerial, NeedsRestart, RestartSystem,
    IsUninstaller, AllowUninstallerShutdown, AcceptedQueryEndSessionInProgress: Boolean;
  InstallDefaultDisableFsRedir, ScriptFuncDisableFsRedir: Boolean;
  InstallDefaultRegView: TRegView = rvDefault;
  HasCustomType, HasComponents, HasTasks: Boolean;
  ProcessorArchitecture: TSetupProcessorArchitecture = paUnknown;
  WindowsVersion: Cardinal;
  NTServicePackLevel: Word;
  WindowsProductType: Byte;
  WindowsSuiteMask: Word;
  MinimumSpace: Integer64;
  DeleteFilesAfterInstallList, DeleteDirsAfterInstallList: TStringList;
  ExpandedAppName, ExpandedAppVerName, ExpandedAppCopyright, ExpandedAppMutex: String;
  DisableCodeConsts: Integer;
  SetupExitCode: Integer;
  CreatedIcon: Boolean;
  RestartInitiatedByThisProcess: Boolean;
{$IFDEF IS_D12}
  TaskbarButtonHidden: Boolean;
{$ENDIF}

  CodeRunner: TScriptRunner;

function CodeRunnerOnDebug(const Position: LongInt;
  var ContinueStepOver: Boolean): Boolean;
function CodeRunnerOnDebugIntermediate(const Position: LongInt;
  var ContinueStepOver: Boolean): Boolean;
procedure CodeRunnerOnDllImport(var DllName: String; var ForceDelayLoad: Boolean);
procedure CodeRunnerOnException(const Exception: AnsiString; const Position: LongInt);
procedure CreateTempInstallDir;
procedure DebugNotifyEntry(EntryType: TEntryType; Number: Integer);
procedure DeinitSetup(const AllowCustomSetupExitCode: Boolean);
function ExitSetupMsgBox: Boolean;
function ExpandConst(const S: String): String;
function ExpandConstEx(const S: String; const CustomConsts: array of String): String;
function ExpandConstIfPrefixed(const S: String): String;
function GetCustomMessageValue(const AName: String; var AValue: String): Boolean;
function GetRealShellFolder(const Common: Boolean; const ID: TShellFolderID;
  ReadOnly: Boolean): String;
function GetShellFolder(Common: Boolean; const ID: TShellFolderID;
  ReadOnly: Boolean): String;
function GetShellFolderByCSIDL(Folder: Integer; const Create: Boolean): String;
function GetUninstallRegKeyBaseName(const ExpandedAppId: String): String;
function GetPreviousData(const ExpandedAppID, ValueName, DefaultValueData: String): String;
procedure Initialize64BitInstallMode(const A64Bit: Boolean);
procedure InitializeCommonVars;
procedure InitializeSetup;
procedure InitMainNonSHFolderConsts;
function InstallOnThisVersion(const MinVersion: TSetupVersionData;
  const OnlyBelowVersion: TSetupVersionData): TInstallOnThisVersionResult;
function IsRecurseableDirectory(const FindData: TWin32FindData): Boolean;
procedure LoadSHFolderDLL;
function LoggedAppMessageBox(const Text, Caption: PChar; const Flags: Longint;
  const Suppressible: Boolean; const Default: Integer): Integer;
function LoggedMsgBox(const Text, Caption: String; const Typ: TMsgBoxType;
  const Buttons: Cardinal; const Suppressible: Boolean; const Default: Integer): Integer;
procedure LogWindowsVersion;
procedure NotifyAfterInstallEntry(const AfterInstall: String);
procedure NotifyAfterInstallFileEntry(const FileEntry: PSetupFileEntry);
procedure NotifyBeforeInstallEntry(const BeforeInstall: String);
procedure NotifyBeforeInstallFileEntry(const FileEntry: PSetupFileEntry);
function PreviousInstallCompleted(const WizardComponents, WizardTasks: TStringList): Boolean;
procedure RegisterResourcesWithRestartManager(const WizardComponents, WizardTasks: TStringList);
procedure RemoveTempInstallDir;
procedure SaveResourceToTempFile(const ResName, Filename: String);
procedure SetActiveLanguage(const I: Integer);
procedure SetTaskbarButtonVisibility(const AVisible: Boolean);
function ShouldDisableFsRedirForFileEntry(const FileEntry: PSetupFileEntry): Boolean;
function ShouldDisableFsRedirForRunEntry(const RunEntry: PSetupRunEntry): Boolean;
function EvalDirectiveCheck(const Expression: String): Boolean;
function ShouldProcessEntry(const WizardComponents, WizardTasks: TStringList;
  const Components, Tasks, Languages, Check: String): Boolean;
function ShouldProcessFileEntry(const WizardComponents, WizardTasks: TStringList;
  const FileEntry: PSetupFileEntry; const IgnoreCheck: Boolean): Boolean;
function ShouldProcessIconEntry(const WizardComponents, WizardTasks: TStringList;
  const WizardNoIcons: Boolean; const IconEntry: PSetupIconEntry): Boolean;
function ShouldProcessRunEntry(const WizardComponents, WizardTasks: TStringList;
  const RunEntry: PSetupRunEntry): Boolean;
function TestPassword(const Password: String): Boolean;
procedure UnloadSHFolderDLL;
function WindowsVersionAtLeast(const AMajor, AMinor: Byte): Boolean;

implementation

uses
  ShellAPI, ShlObj,
  Msgs, MsgIDs, Install, InstFunc, InstFnc2, RedirFunc, PathFunc,
  Compress, CompressZlib, bzlib, LZMADecomp, ArcFour, SetupEnt, SelLangForm,
  Wizard, DebugClient, VerInfo, Extract, FileClass, Logging, MD5, SHA1,
  SimpleExpression, Helper, SpawnClient, SpawnServer, LibFusion;

{$R *.DFM}

var
  ShellFolders: array[Boolean, TShellFolderID] of String;
  ShellFoldersRead: array[Boolean, TShellFolderID] of Boolean;
  SHFolderDLLHandle: HMODULE;
  SHGetFolderPathFunc: function(hwndOwner: HWND; nFolder: Integer;
    hToken: THandle; dwFlags: DWORD; pszPath: PChar): HRESULT; stdcall;

  DecompressorDLLHandle: HMODULE;
  DecryptDLLHandle: HMODULE;

type
  TDummyClass = class
    public
      class function ExpandCheckOrInstallConstant(Sender: TSimpleExpression;
        const Constant: String): String;
      class function EvalInstallIdentifier(Sender: TSimpleExpression;
        const Name: String; const Parameters: array of const): Boolean;
      class function EvalComponentOrTaskIdentifier(Sender: TSimpleExpression;
        const Name: String; const Parameters: array of const): Boolean;
      class function EvalLanguageIdentifier(Sender: TSimpleExpression;
        const Name: String; const Parameters: array of const): Boolean;
      class function EvalCheckIdentifier(Sender: TSimpleExpression;
        const Name: String; const Parameters: array of const): Boolean;
  end;

{ Misc. functions }

function WindowsVersionAtLeast(const AMajor, AMinor: Byte): Boolean;
begin
  Result := (WindowsVersion >= Cardinal((AMajor shl 24) or (AMinor shl 16)));
end;

function GetUninstallRegKeyBaseName(const ExpandedAppId: String): String;
{$IFDEF UNICODE}
var
  UseAnsiCRC32: Boolean;
  S: AnsiString;
  I: Integer;
{$ENDIF}
begin
  { Set uninstall registry key base name }
  Result := ExpandedAppId;
  { Uninstall registry keys can only be up to 63 characters, otherwise Win95
    ignores them. Limit to 57 since Setup will add _isXXX to the end later. }
  if Length(Result) > 57 then begin
    { Only keep the first 48 characters, then add an tilde and the CRC
      of the original string (to make the trimmed string unique). The
      resulting string is 57 characters long. On Unicode, only do this if we
      can get a CRC32 compatible with ANSI versions, else there's no point
      in shortening since Unicode doesn't run on Win95. }
{$IFDEF UNICODE}
    UseAnsiCRC32 := True;
    for I := 1 to Length(Result) do begin
      if Ord(Result[I]) > 126 then begin
        UseAnsiCRC32 := False;
        Break;
      end;
    end;
    if UseAnsiCRC32 then begin
      S := AnsiString(Result);
      FmtStr(Result, '%.48s~%.8x', [Result, GetCRC32(S[1], Length(S)*SizeOf(S[1]))]);
    end;
{$ELSE}
    FmtStr(Result, '%.48s~%.8x', [Result, GetCRC32(Result[1], Length(Result)*SizeOf(Result[1]))]);
{$ENDIF}
  end;
end;

{ Based on FindPreviousData in Wizard.pas }
function GetPreviousData(const ExpandedAppID, ValueName, DefaultValueData: String): String;
const
  RootKeys: array[0..1] of HKEY = (HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE);
var
  I: Integer;
  H: HKEY;
  UninstallRegKeyBaseName: String;
begin
  Result := DefaultValueData;
  if ExpandedAppId <> '' then begin
    UninstallRegKeyBaseName := GetUninstallRegKeyBaseName(ExpandedAppId);
    for I := 0 to 1 do begin
      if RegOpenKeyExView(InstallDefaultRegView, RootKeys[I],
         PChar(Format('%s\%s_is1', [NEWREGSTR_PATH_UNINSTALL, UninstallRegKeyBaseName])),
         0, KEY_QUERY_VALUE, H) = ERROR_SUCCESS then begin
        try
          RegQueryStringValue (H, PChar(ValueName), Result);
        finally
          RegCloseKey (H);
        end;
        Break;
      end;
    end;
  end;
end;

function TestPassword(const Password: String): Boolean;
var
  Context: TSHA1Context;
  Hash: TSHA1Digest;
begin
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar('PasswordCheckHash')^, Length('PasswordCheckHash'));
  SHA1Update(Context, SetupHeader.PasswordSalt, SizeOf(SetupHeader.PasswordSalt));
  SHA1Update(Context, Pointer(Password)^, Length(Password)*SizeOf(Password[1]));
  Hash := SHA1Final(Context);
  Result := SHA1DigestsEqual(Hash, SetupHeader.PasswordHash);
end;

class function TDummyClass.ExpandCheckOrInstallConstant(Sender: TSimpleExpression;
  const Constant: String): String;
begin
  Result := ExpandConst(Constant);
end;

class function TDummyClass.EvalInstallIdentifier(Sender: TSimpleExpression;
  const Name: String; const Parameters: array of const): Boolean;
begin
  CodeRunner.RunProcedure(AnsiString(Name), Parameters, True);
  Result := True;  { Result doesn't matter }
end;

procedure NotifyInstallEntry(const Install: String);

  procedure EvalInstall(const Expression: String);
  var
    SimpleExpression: TSimpleExpression;
  begin
    try
      SimpleExpression := TSimpleExpression.Create;
      try
        SimpleExpression.Expression := Expression;
        SimpleExpression.OnEvalIdentifier := TDummyClass.EvalInstallIdentifier;
        SimpleExpression.OnExpandConstant := TDummyClass.ExpandCheckOrInstallConstant;
        SimpleExpression.ParametersAllowed := True;
        SimpleExpression.SingleIdentifierMode := True;
        SimpleExpression.Eval;
      finally
        SimpleExpression.Free;
      end;
    except
      InternalError(Format('Expression error ''%s''', [GetExceptMessage]));
    end;
  end;

begin
  if Install <> '' then begin
    try
      if CodeRunner = nil then
        InternalError('"BeforeInstall" or "AfterInstall" parameter with no CodeRunner');
      EvalInstall(Install);
    except
      { Don't allow exceptions raised by Before/AfterInstall functions to be propagated out }
      Application.HandleException(nil);
    end;
  end;
end;

procedure NotifyBeforeInstallEntry(const BeforeInstall: String);
begin
  NotifyInstallEntry(BeforeInstall);
end;

procedure NotifyBeforeInstallFileEntry(const FileEntry: PSetupFileEntry);
begin
  CheckOrInstallCurrentFileName := FileEntry.DestName;
  NotifyInstallEntry(FileEntry.BeforeInstall);
  CheckOrInstallCurrentFileName := '';
end;

procedure NotifyAfterInstallEntry(const AfterInstall: String);
begin
  NotifyInstallEntry(AfterInstall);
end;

procedure NotifyAfterInstallFileEntry(const FileEntry: PSetupFileEntry);
begin
  CheckOrInstallCurrentFileName := FileEntry.DestName;
  NotifyInstallEntry(FileEntry.AfterInstall);
  CheckOrInstallCurrentFileName := '';
end;

class function TDummyClass.EvalComponentOrTaskIdentifier(Sender: TSimpleExpression;
  const Name: String; const Parameters: array of const): Boolean;
var
  WizardItems: TStringList;
begin
  WizardItems := TStringList(Sender.Tag);
  Result := ListContains(WizardItems, Name);
end;

class function TDummyClass.EvalLanguageIdentifier(Sender: TSimpleExpression;
  const Name: String; const Parameters: array of const): Boolean;
begin
  Result := CompareText(PSetupLanguageEntry(Entries[seLanguage][ActiveLanguage]).Name, Name) = 0;
end;

class function TDummyClass.EvalCheckIdentifier(Sender: TSimpleExpression;
  const Name: String; const Parameters: array of const): Boolean;
begin
  Result := CodeRunner.RunBooleanFunction(AnsiString(Name), Parameters, True, False);
end;

function EvalCheck(const Expression: String): Boolean;
var
  SimpleExpression: TSimpleExpression;
begin
  try
    SimpleExpression := TSimpleExpression.Create;
    try
      SimpleExpression.Lazy := True;
      SimpleExpression.Expression := Expression;
      SimpleExpression.OnEvalIdentifier := TDummyClass.EvalCheckIdentifier;
      SimpleExpression.OnExpandConstant := TDummyClass.ExpandCheckOrInstallConstant;
      SimpleExpression.ParametersAllowed := True;
      SimpleExpression.SilentOrAllowed := False;
      SimpleExpression.SingleIdentifierMode := False;
      Result := SimpleExpression.Eval;
    finally
      SimpleExpression.Free;
    end;
  except
    InternalError(Format('Expression error ''%s''', [GetExceptMessage]));
    Result := False;
  end;
end;

function EvalDirectiveCheck(const Expression: String): Boolean;
begin
  if not TryStrToBoolean(Expression, Result) then
    Result := EvalCheck(Expression);
end;

function ShouldProcessEntry(const WizardComponents, WizardTasks: TStringList;
  const Components, Tasks, Languages, Check: String): Boolean;

  function EvalExpression(const Expression: String;
    OnEvalIdentifier: TSimpleExpressionOnEvalIdentifier; Tag: LongInt): Boolean;
  var
    SimpleExpression: TSimpleExpression;
  begin
    try
      SimpleExpression := TSimpleExpression.Create;
      try
        SimpleExpression.Lazy := True;
        SimpleExpression.Expression := Expression;
        SimpleExpression.OnEvalIdentifier := OnEvalIdentifier;
        SimpleExpression.ParametersAllowed := False;
        SimpleExpression.SilentOrAllowed := True;
        SimpleExpression.SingleIdentifierMode := False;
        SimpleExpression.Tag := Tag;
        Result := SimpleExpression.Eval;
      finally
        SimpleExpression.Free;
      end;
    except
      InternalError(Format('Expression error ''%s''', [GetExceptMessage]));
      Result := False;
    end;
  end;

var
  ProcessComponent, ProcessTask, ProcessLanguage: Boolean;
begin
  if (Components <> '') or (Tasks <> '') or (Languages <> '') or (Check <> '') then begin
    if (Components <> '') and (WizardComponents <> nil) then
      ProcessComponent := EvalExpression(Components, TDummyClass.EvalComponentOrTaskIdentifier, LongInt(WizardComponents))
    else
      ProcessComponent := True;

    if (Tasks <> '') and (WizardTasks <> nil) then
      ProcessTask := EvalExpression(Tasks, TDummyClass.EvalComponentOrTaskIdentifier, LongInt(WizardTasks))
    else
      ProcessTask := True;

    if Languages <> '' then
      ProcessLanguage := EvalExpression(Languages, TDummyClass.EvalLanguageIdentifier, 0)
    else
      ProcessLanguage := True;

    Result := ProcessComponent and ProcessTask and ProcessLanguage;
    if Result and (Check <> '') then begin
      try
        if CodeRunner = nil then
          InternalError('"Check" parameter with no CodeRunner');
        Result := EvalCheck(Check);
      except
        { Don't allow exceptions raised by Check functions to be propagated out }
        Application.HandleException(nil);
        Result := False;
      end;
    end;
  end else
    Result := True;
end;

function ShouldProcessFileEntry(const WizardComponents, WizardTasks: TStringList;
  const FileEntry: PSetupFileEntry; const IgnoreCheck: Boolean): Boolean;
begin
  if foDontCopy in FileEntry.Options then begin
    Result := False;
    Exit;
  end;
  CheckOrInstallCurrentFileName := FileEntry.DestName;
  if IgnoreCheck then
    Result := ShouldProcessEntry(WizardComponents, WizardTasks, FileEntry.Components, FileEntry.Tasks, FileEntry.Languages, '')
  else
    Result := ShouldProcessEntry(WizardComponents, WizardTasks, FileEntry.Components, FileEntry.Tasks, FileEntry.Languages, FileEntry.Check);
  CheckOrInstallCurrentFileName := '';
end;

function ShouldProcessRunEntry(const WizardComponents, WizardTasks: TStringList;
  const RunEntry: PSetupRunEntry): Boolean;
begin
  if (InstallMode <> imNormal) and (roSkipIfSilent in RunEntry.Options) then
    Result := False
  else if (InstallMode = imNormal) and (roSkipIfNotSilent in RunEntry.Options) then
    Result := False
  else
    Result := ShouldProcessEntry(WizardComponents, WizardTasks, RunEntry.Components, RunEntry.Tasks, RunEntry.Languages, RunEntry.Check);
end;

function ShouldProcessIconEntry(const WizardComponents, WizardTasks: TStringList;
  const WizardNoIcons: Boolean; const IconEntry: PSetupIconEntry): Boolean;
begin
  if WizardNoIcons and (IconEntry.Tasks = '') and
     (Copy(IconEntry.IconName, 1, 8) = '{group}\') then
    Result := False
  else
    Result := ShouldProcessEntry(WizardComponents, WizardTasks, IconEntry.Components, IconEntry.Tasks, IconEntry.Languages, IconEntry.Check);
end;

function ShouldDisableFsRedirForFileEntry(const FileEntry: PSetupFileEntry): Boolean;
begin
  Result := InstallDefaultDisableFsRedir;
  if fo32Bit in FileEntry.Options then
    Result := False;
  if fo64Bit in FileEntry.Options then begin
    if not IsWin64 then
      InternalError('Cannot install files to 64-bit locations on this version of Windows');
    Result := True;
  end;
end;

function SlashesToBackslashes(const S: String): String;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(Result) do
    if Result[I] = '/' then
      Result[I] := '\';
end;

procedure LoadInf(const FileName: String);
const
  Section = 'Setup';
var
  S: String;
begin
  //saved infs
  InitLang := GetIniString(Section, 'Lang', InitLang, FileName);
  InitDir := GetIniString(Section, 'Dir', InitDir, FileName);
  InitProgramGroup := GetIniString(Section, 'Group', InitProgramGroup, FileName);
  InitNoIcons := GetIniBool(Section, 'NoIcons', InitNoIcons, FileName);
  InitSetupType := GetIniString(Section, 'SetupType', InitSetupType, FileName);
  S := GetIniString(Section, 'Components', '$', FileName);
  if S <> '$' then begin
    InitComponentsSpecified := True;
    SetStringsFromCommaString(InitComponents, SlashesToBackslashes(S));
  end;
  S := GetIniString(Section, 'Tasks', '$', FileName);
  if S <> '$' then begin
    InitDeselectAllTasks := True;
    SetStringsFromCommaString(InitTasks, SlashesToBackslashes(S));
  end;
  //non saved infs (=non user settable)
  InitSilent := GetIniBool(Section, 'Silent', InitSilent, FileName);
  InitVerySilent := GetIniBool(Section, 'VerySilent', InitVerySilent, FileName);
  InitNoRestart := GetIniBool(Section, 'NoRestart', InitNoRestart, FileName);
  InitNoCloseApplications := GetIniBool(Section, 'NoCloseApplications', InitNoCloseApplications, FileName);
  InitNoRestartApplications := GetIniBool(Section, 'NoRestartApplications', InitNoRestartApplications, FileName);
  InitPassword := GetIniString(Section, 'Password', InitPassword, FileName);
  InitRestartExitCode := GetIniInt(Section, 'RestartExitCode', InitRestartExitCode, 0, 0, FileName);
  InitSaveInf := GetIniString(Section, 'SaveInf', InitSaveInf, FileName);
end;

procedure SaveInf(const FileName: String);
const
  Section = 'Setup';
begin
  SetIniString(Section, 'Lang',
    PSetupLanguageEntry(Entries[seLanguage][ActiveLanguage]).Name, FileName);
  SetIniString(Section, 'Dir', WizardDirValue, FileName);
  SetIniString(Section, 'Group', WizardGroupValue, FileName);
  SetIniBool(Section, 'NoIcons', WizardNoIcons, FileName);
  if WizardSetupType <> nil then begin
    SetIniString(Section, 'SetupType', WizardSetupType.Name, FileName);
    SetIniString(Section, 'Components', StringsToCommaString(WizardComponents), FileName);
  end
  else begin
    DeleteIniEntry(Section, 'SetupType', FileName);
    DeleteIniEntry(Section, 'Components', FileName);
  end;
  SetIniString(Section, 'Tasks', StringsToCommaString(WizardTasks), FileName);
end;

function GetCustomMessageValue(const AName: String; var AValue: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Entries[seCustomMessage].Count-1 do begin
    with PSetupCustomMessageEntry(Entries[seCustomMessage][I])^ do begin
      if (CompareText(Name, AName) = 0) and
         ((LangIndex = -1) or (LangIndex = ActiveLanguage)) then begin
        Result := True;
        AValue := Value;
        { don't stop looping, last item counts }
      end;
    end;
  end;
end;

function ExpandIndividualConst(const Cnst: String;
  const CustomConsts: array of String): String;
{ Cnst must be the name of a single constant, without the braces.
  For example: app
  IsPath is set to True if the result is a path which needs special trailing-
  backslash handling. }

  procedure NoUninstallConstError(const C: String);
  begin
    InternalError(Format('Cannot evaluate "%s" constant during Uninstall', [C]));
  end;

  function ExpandEnvConst(C: String): String;
  var
    I: Integer;
    VarName, Default: String;
  begin
    Delete(C, 1, 1);
    I := ConstPos('|', C);  { check for 'default' value }
    if I = 0 then
      I := Length(C)+1;
    VarName := Copy(C, 1, I-1);
    Default := Copy(C, I+1, Maxint);
    Result := '';
    if ConvertConstPercentStr(VarName) and ConvertConstPercentStr(Default) then begin
      Result := GetEnv(ExpandConstEx(VarName, CustomConsts));
      if Result = '' then
        Result := ExpandConstEx(Default, CustomConsts);
    end;
  end;

  function ExpandRegConst(C: String): String;
  { Expands a registry-value constant in the form:
    reg:HKxx\SubkeyName,ValueName|DefaultValue }
  type
    TKeyNameConst = packed record
      KeyName: String;
      KeyConst: HKEY;
    end;
  const
    KeyNameConsts: array[0..4] of TKeyNameConst = (
      (KeyName: 'HKCR'; KeyConst: HKEY_CLASSES_ROOT),
      (KeyName: 'HKCU'; KeyConst: HKEY_CURRENT_USER),
      (KeyName: 'HKLM'; KeyConst: HKEY_LOCAL_MACHINE),
      (KeyName: 'HKU';  KeyConst: HKEY_USERS),
      (KeyName: 'HKCC'; KeyConst: HKEY_CURRENT_CONFIG));
  var
    Z, Subkey, Value, Default: String;
    I, J, L: Integer;
    RegView: TRegView; 
    RootKey: HKEY;
    K: HKEY;
  begin
    Delete(C, 1, 4);  { skip past 'reg:' }
    I := ConstPos('\', C);
    if I <> 0 then begin
      Z := Copy(C, 1, I-1);
      if Z <> '' then begin
        RegView := InstallDefaultRegView;
        L := Length(Z);
        if L >= 2 then begin
          { Check for '32' or '64' suffix }
          if (Z[L-1] = '3') and (Z[L] = '2') then begin
            RegView := rv32Bit;
            SetLength(Z, L-2);
          end
          else if (Z[L-1] = '6') and (Z[L] = '4') then begin
            if not IsWin64 then
              InternalError('Cannot access a 64-bit key in a "reg" constant on this version of Windows');
            RegView := rv64Bit;
            SetLength(Z, L-2);
          end;
        end;
        RootKey := 0;
        for J := Low(KeyNameConsts) to High(KeyNameConsts) do
          if CompareText(KeyNameConsts[J].KeyName, Z) = 0 then begin
            RootKey := KeyNameConsts[J].KeyConst;
            Break;
          end;
        if RootKey <> 0 then begin
          Z := Copy(C, I+1, Maxint);
          I := ConstPos('|', Z);  { check for a 'default' data }
          if I = 0 then
            I := Length(Z)+1;
          Default := Copy(Z, I+1, Maxint);
          SetLength(Z, I-1);
          I := ConstPos(',', Z);  { comma separates subkey and value }
          if I <> 0 then begin
            Subkey := Copy(Z, 1, I-1);
            Value := Copy(Z, I+1, Maxint);
            if ConvertConstPercentStr(Subkey) and ConvertConstPercentStr(Value) and
               ConvertConstPercentStr(Default) then begin
              Result := ExpandConstEx(Default, CustomConsts);
              if RegOpenKeyExView(RegView, RootKey,
                 PChar(ExpandConstEx(Subkey, CustomConsts)),
                 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
                RegQueryStringValue(K, PChar(ExpandConstEx(Value, CustomConsts)),
                  Result);
                RegCloseKey(K);
              end;
              Exit;
            end;
          end;
        end;
      end;
    end;
    { it will only reach here if there was a parsing error }
    InternalError('Failed to parse "reg" constant');
  end;

  function ExpandIniConst(C: String): String;
  { Expands an INI-value constant in the form:
    filename,section,key|defaultvalue }
  var
    Z, Filename, Section, Key, Default: String;
    I: Integer;
  begin
    Delete(C, 1, 4);  { skip past 'ini:' }
    I := ConstPos(',', C);
    if I <> 0 then begin
      Z := Copy(C, 1, I-1);
      if Z <> '' then begin
        Filename := Z;
        Z := Copy(C, I+1, Maxint);
        I := ConstPos('|', Z);  { check for a 'default' data }
        if I = 0 then
          I := Length(Z)+1;
        Default := Copy(Z, I+1, Maxint);
        SetLength(Z, I-1);
        I := ConstPos(',', Z);  { comma separates section and key }
        if I <> 0 then begin
          Section := Copy(Z, 1, I-1);
          Key := Copy(Z, I+1, Maxint);
          if ConvertConstPercentStr(Filename) and ConvertConstPercentStr(Section) and ConvertConstPercentStr(Key) and
             ConvertConstPercentStr(Default) then begin
            Filename := ExpandConstEx(Filename, CustomConsts);
            Section := ExpandConstEx(Section, CustomConsts);
            Key := ExpandConstEx(Key, CustomConsts);
            Default := ExpandConstEx(Default, CustomConsts);
            Result := GetIniString(Section, Key, Default, Filename);
            Exit;
          end;
        end;
      end;
    end;
    { it will only reach here if there was a parsing error }
    InternalError('Failed to parse "ini" constant');
  end;

  function ExpandParamConst(C: String): String;
  { Expands an commandline-parameter-value constant in the form:
    parametername|defaultvalue }

    function GetParamString(const Param, Default: String): String;
    var
      I, PCount: Integer;
      Z: String;
    begin
      PCount := NewParamCount();
      for I := 1 to PCount do begin
        Z := NewParamStr(I);
        if StrLIComp(PChar(Z), PChar('/'+Param+'='), Length(Param)+2) = 0 then begin
          Delete(Z, 1, Length(Param)+2);
          Result := Z;
          Exit;
        end;
      end;

      Result := Default;
    end;

  var
    Z, Param, Default: String;
    I: Integer;
  begin
    Delete(C, 1, 6);  { skip past 'param:' }
    Z := C;
    I := ConstPos('|', Z);  { check for a 'default' data }
    if I = 0 then
      I := Length(Z)+1;
    Default := Copy(Z, I+1, Maxint);
    SetLength(Z, I-1);
    Param := Z;
    if ConvertConstPercentStr(Param) and ConvertConstPercentStr(Default) then begin
      Param := ExpandConstEx(Param, CustomConsts);
      Default := ExpandConstEx(Default, CustomConsts);
      Result := GetParamString(Param, Default);
      Exit;
    end;
    { it will only reach here if there was a parsing error }
    InternalError('Failed to parse "param" constant');
  end;

  function ExpandCodeConst(C: String): String;
  { Expands an Pascal-script-value constant in the form:
    parametername|defaultvalue }

    function GetCodeString(const ScriptFunc, Default: String): String;
    begin
      if (CodeRunner <> nil) then
        Result := CodeRunner.RunStringFunction(AnsiString(ScriptFunc), [Default], True, Default)
      else begin
        InternalError('"code" constant with no CodeRunner');
        Result := '';
      end;
    end;

  var
    Z, ScriptFunc, Default: String;
    I: Integer;
  begin
    if DisableCodeConsts <> 0 then
      raise Exception.Create('Cannot evaluate "code" constant because of possible side effects');

    Delete(C, 1, 5);  { skip past 'code:' }
    Z := C;
    I := ConstPos('|', Z);  { check for a 'default' data }
    if I = 0 then
      I := Length(Z)+1;
    Default := Copy(Z, I+1, Maxint);
    SetLength(Z, I-1);
    ScriptFunc := Z;
    if ConvertConstPercentStr(ScriptFunc) and ConvertConstPercentStr(Default) then begin
      Default := ExpandConstEx(Default, CustomConsts);
      Result := GetCodeString(ScriptFunc, Default);
      Exit;
    end;
    { it will only reach here if there was a parsing error }
    InternalError('Failed to parse "code" constant');
  end;

  function ExpandDriveConst(C: String): String;
  begin
    Delete(C, 1, 6);  { skip past 'drive:' }
    if ConvertConstPercentStr(C) then begin
      Result := PathExtractDrive(ExpandConstEx(C, CustomConsts));
      Exit;
    end;
    { it will only reach here if there was a parsing error }
    InternalError('Failed to parse "drive" constant');
  end;

  function ExpandCustomMessageConst(C: String): String;
  var
    I, ArgCount: Integer;
    MsgName: String;
    ArgValues: array[0..8] of String;  { %1 through %9 }
  begin
    Delete(C, 1, 3);  { skip past 'cm:' }
    I := ConstPos(',', C);
    if I = 0 then
      MsgName := C
    else
      MsgName := Copy(C, 1, I-1);

    { Prepare arguments. Excess arguments are ignored. }
    ArgCount := 0;
    while (I > 0) and (ArgCount <= High(ArgValues)) do begin
      Delete(C, 1, I);
      I := ConstPos(',', C);
      if I = 0 then
        ArgValues[ArgCount] := C
      else
        ArgValues[ArgCount] := Copy(C, 1, I-1);
      if not ConvertConstPercentStr(ArgValues[ArgCount]) then
        InternalError('Failed to parse "cm" constant');
      ArgValues[ArgCount] := ExpandConstEx(ArgValues[ArgCount], CustomConsts);
      Inc(ArgCount);
    end;

    { Look up the message value }
    if not GetCustomMessageValue(MsgName, Result) then
      InternalError(Format('Unknown custom message name "%s" in "cm" constant', [MsgName]));

    { Expand the message }
    Result := FmtMessage(PChar(Result), Slice(ArgValues, ArgCount));
  end;

const
  FolderConsts: array[Boolean, TShellFolderID] of String =
    (('userdesktop', 'userstartmenu', 'userprograms', 'userstartup',
      'sendto', 'fonts', 'userappdata', 'userdocs', 'usertemplates',
      'userfavorites', 'localappdata'),
     ('commondesktop', 'commonstartmenu', 'commonprograms', 'commonstartup',
      'sendto', 'fonts', 'commonappdata', 'commondocs', 'commontemplates',
      'commonfavorites', 'localappdata'));
  NoUninstallConsts: array[0..6] of String =
    ('src', 'srcexe', 'userinfoname', 'userinfoorg', 'userinfoserial', 'hwnd',
     'wizardhwnd');
var
  Z: String;
  B: Boolean;
  SF: TShellFolderID;
  K: Integer;
begin
  if IsUninstaller then
    for K := Low(NoUninstallConsts) to High(NoUninstallConsts) do
      if NoUninstallConsts[K] = Cnst then
        NoUninstallConstError(NoUninstallConsts[K]);

  if Cnst = '\' then Result := '\'
  else if Cnst = 'app' then begin
    if IsUninstaller then begin
      if UninstallExpandedApp = '' then
        InternalError('An attempt was made to expand the "app" constant but Setup didn''t create the "app" dir');
      Result := UninstallExpandedApp;
    end else begin
      if WizardDirValue = '' then
        InternalError('An attempt was made to expand the "app" constant before it was initialized');
      Result := WizardDirValue;
    end;
  end
  else if Cnst = 'win' then Result := WinDir
  else if Cnst = 'sys' then Result := WinSystemDir
  else if Cnst = 'syswow64' then begin
    if WinSysWow64Dir <> '' then
      Result := WinSysWow64Dir
    else begin
      if IsWin64 then  { sanity check }
        InternalError('Cannot expand "syswow64" constant because there is no SysWOW64 directory');
      Result := WinSystemDir;
    end;
  end
  else if Cnst = 'src' then Result := SourceDir
  else if Cnst = 'srcexe' then Result := SetupLdrOriginalFilename
  else if Cnst = 'tmp' then Result := TempInstallDir
  else if Cnst = 'sd' then Result := SystemDrive
  else if Cnst = 'pf' then begin
    if Is64BitInstallMode then
      Result := ProgramFiles64Dir
    else
      Result := ProgramFiles32Dir;
  end
  else if Cnst = 'cf' then begin
    if Is64BitInstallMode then
      Result := CommonFiles64Dir
    else
      Result := CommonFiles32Dir;
  end
  else if Cnst = 'pf32' then Result := ProgramFiles32Dir
  else if Cnst = 'cf32' then Result := CommonFiles32Dir
  else if Cnst = 'pf64' then begin
    if IsWin64 then
      Result := ProgramFiles64Dir
    else
      InternalError('Cannot expand "pf64" constant on this version of Windows');
  end
  else if Cnst = 'cf64' then begin
    if IsWin64 then
      Result := CommonFiles64Dir
    else
      InternalError('Cannot expand "cf64" constant on this version of Windows');
  end
  else if Cnst = 'dao' then Result := ExpandConst('{cf}\Microsoft Shared\DAO')
  else if Cnst = 'cmd' then Result := CmdFilename
  else if Cnst = 'computername' then Result := GetComputerNameString
  else if Cnst = 'username' then Result := GetUserNameString
  else if Cnst = 'groupname' then begin
    if IsUninstaller then begin
      if UninstallExpandedGroupName = '' then
        InternalError('Cannot expand "groupname" constant because it was not available at install time');
      Result := UninstallExpandedGroupName;
    end
    else begin
      if WizardGroupValue = '' then
        InternalError('An attempt was made to expand the "groupname" constant before it was initialized');
      Result := WizardGroupValue;
    end;
  end
  else if Cnst = 'sysuserinfoname' then Result := SysUserInfoName
  else if Cnst = 'sysuserinfoorg' then Result := SysUserInfoOrg
  else if Cnst = 'userinfoname' then Result := WizardUserInfoName
  else if Cnst = 'userinfoorg' then Result := WizardUserInfoOrg
  else if Cnst = 'userinfoserial' then Result := WizardUserInfoSerial
  else if Cnst = 'uninstallexe' then Result := UninstallExeFilename
  else if Cnst = 'group' then begin
    if IsUninstaller then begin
      if UninstallExpandedGroup = '' then
        InternalError('Cannot expand "group" constant because it was not available at install time');
      Result := UninstallExpandedGroup;
    end
    else begin
      if WizardGroupValue = '' then
        InternalError('An attempt was made to expand the "group" constant before it was initialized');
      Z := GetShellFolder(not(shAlwaysUsePersonalGroup in SetupHeader.Options),
        sfPrograms, False);
      if Z = '' then
        InternalError('Failed to expand "group" constant');
      Result := AddBackslash(Z) + WizardGroupValue;
    end;
  end
  else if Cnst = 'language' then begin
    if IsUninstaller then
      Result := UninstallExpandedLanguage
    else
      Result := PSetupLanguageEntry(Entries[seLanguage][ActiveLanguage]).Name
  end
  else if Cnst = 'hwnd' then begin
    if Assigned(MainForm) then
      Result := IntToStr(MainForm.Handle)
    else
      Result := '0';
  end
  else if Cnst = 'wizardhwnd' then begin
    if Assigned(WizardForm) then
      Result := IntToStr(WizardForm.Handle)
    else
      Result := '0';
  end
  else if Cnst = 'log' then Result := GetLogFileName
  else if Cnst = 'dotnet11' then Result := GetDotNetVersionRoot(rv32Bit, dt11)
  else if Cnst = 'dotnet20' then Result := GetDotNetVersionRoot(InstallDefaultRegView, dt20)
  else if Cnst = 'dotnet2032' then Result := GetDotNetVersionRoot(rv32Bit, dt20)
  else if Cnst = 'dotnet2064' then begin
    if IsWin64 then
      Result := GetDotNetVersionRoot(rv64Bit, dt20)
    else
      InternalError('Cannot expand "dotnet2064" constant on this version of Windows');
  end
  else if Cnst = 'dotnet40' then Result := GetDotNetVersionRoot(InstallDefaultRegView, dt40)
  else if Cnst = 'dotnet4032' then Result := GetDotNetVersionRoot(rv32Bit, dt40)
  else if Cnst = 'dotnet4064' then begin
    if IsWin64 then
      Result := GetDotNetVersionRoot(rv64Bit, dt40)
    else
      InternalError('Cannot expand "dotnet4064" constant on this version of Windows');
  end
  else if (Cnst <> '') and (Cnst[1] = '%') then Result := ExpandEnvConst(Cnst)
  else if StrLComp(PChar(Cnst), 'reg:', 4) = 0 then Result := ExpandRegConst(Cnst)
  else if StrLComp(PChar(Cnst), 'ini:', 4) = 0 then Result := ExpandIniConst(Cnst)
  else if StrLComp(PChar(Cnst), 'param:', 6) = 0 then Result := ExpandParamConst(Cnst)
  else if StrLComp(PChar(Cnst), 'code:', 5) = 0 then Result := ExpandCodeConst(Cnst)
  else if StrLComp(PChar(Cnst), 'drive:', 6) = 0 then Result := ExpandDriveConst(Cnst)
  else if StrLComp(PChar(Cnst), 'cm:', 3) = 0 then Result := ExpandCustomMessageConst(Cnst)
  else begin
    { Shell folder constants }
    for B := False to True do
      for SF := Low(SF) to High(SF) do
        if Cnst = FolderConsts[B, SF] then begin
          Z := GetShellFolder(B, SF, False);
          if Z = '' then
            InternalError(Format('Failed to expand shell folder constant "%s"', [Cnst]));
          Result := Z;
          Exit;
        end;
    { Custom constants }
    if Cnst <> '' then begin
      K := 0;
      while K < High(CustomConsts) do begin
        if Cnst = CustomConsts[K] then begin
          Result := CustomConsts[K+1];
          Exit;
        end;
        Inc(K, 2);
      end;
    end;
    { Unknown constant }
    InternalError(Format('Unknown constant "%s"', [Cnst]));
  end;
end;

function ExpandConst(const S: String): String;
begin
  Result := ExpandConstEx(S, ['']);
end;

function ExpandConstEx(const S: String; const CustomConsts: array of String): String;
var
  I, Start: Integer;
  Cnst, ReplaceWith: String;
begin
  Result := S;
  I := 1;
  while I <= Length(Result) do begin
    if Result[I] = '{' then begin
      if (I < Length(Result)) and (Result[I+1] = '{') then begin
        { Change '{{' to '{' if not in an embedded constant }
        Inc(I);
        Delete(Result, I, 1);
      end
      else begin
        Start := I;
        { Find the closing brace, skipping over any embedded constants }
        I := SkipPastConst(Result, I);
        if I = 0 then  { unclosed constant? }
          InternalError('Unclosed constant');
        Dec(I);  { 'I' now points to the closing brace }

        { Now translate the constant }
        Cnst := Copy(Result, Start+1, I-(Start+1));
        ReplaceWith := ExpandIndividualConst(Cnst, CustomConsts);
        Delete(Result, Start, (I+1)-Start);
        Insert(ReplaceWith, Result, Start);
        I := Start + Length(ReplaceWith);
        if (ReplaceWith <> '') and (PathLastChar(ReplaceWith)^ = '\') and
           (I <= Length(Result)) and (Result[I] = '\') then
          Delete(Result, I, 1);
      end;
    end
    else begin
{$IFNDEF UNICODE}
      if Result[I] in ConstLeadBytes^ then
        Inc(I);
{$ENDIF}
      Inc(I);
    end;
  end;
end;

function ExpandConstIfPrefixed(const S: String): String;
const
  ExpandPrefix = 'expand:';
begin
  if Pos(ExpandPrefix, S) = 1 then begin
    Inc(DisableCodeConsts);
    try
      Result := ExpandConst(Copy(S, Length(ExpandPrefix)+1, Maxint));
    finally
      Dec(DisableCodeConsts);
    end;
  end
  else
    Result := S;
end;

procedure InitMainNonSHFolderConsts;

  function GetPath(const RegView: TRegView; const Name: PChar): String;
  var
    H: HKEY;
  begin
    if RegOpenKeyExView(RegView, HKEY_LOCAL_MACHINE, NEWREGSTR_PATH_SETUP, 0,
       KEY_QUERY_VALUE, H) = ERROR_SUCCESS then begin
      if not RegQueryStringValue(H, Name, Result) then
        Result := '';
      RegCloseKey(H);
    end
    else
      Result := '';
  end;

  procedure ReadSysUserInfo;
  const
    Paths: array[Boolean] of PChar = (NEWREGSTR_PATH_SETUP,
      'SOFTWARE\Microsoft\Windows NT\CurrentVersion');
  var
    RegView: TRegView;
    K: HKEY;
  begin
    { Windows Vista and Server 2008 x64 are bugged: the owner and organization
      are set to "Microsoft" on the 32-bit key. So on 64-bit Windows, read
      from the 64-bit key. (The bug doesn't exist on 64-bit XP or Server 2003,
      but it's safe to read the 64-bit key on those versions too.) }
    if IsWin64 then
      RegView := rv64Bit
    else
      RegView := rvDefault;
    if RegOpenKeyExView(RegView, HKEY_LOCAL_MACHINE, Paths[IsNT], 0, KEY_QUERY_VALUE,
       K) = ERROR_SUCCESS then begin
      RegQueryStringValue(K, 'RegisteredOwner', SysUserInfoName);
      RegQueryStringValue(K, 'RegisteredOrganization', SysUserInfoOrg);
      RegCloseKey(K);
    end;
  end;

begin
  { Read Windows and Windows System dirs }
  WinDir := GetWinDir;
  WinSystemDir := GetSystemDir;
  WinSysWow64Dir := GetSysWow64Dir;

  { Get system drive }
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    SystemDrive := GetEnv('SystemDrive')  {don't localize}
  else
    SystemDrive := '';
  if SystemDrive = '' then begin
    SystemDrive := PathExtractDrive(WinDir);
    if SystemDrive = '' then
      { In some rare case that PathExtractDrive failed, just default to C }
      SystemDrive := 'C:';
  end;

  { Get 32-bit Program Files and Common Files dirs }
  ProgramFiles32Dir := GetPath(rv32Bit, 'ProgramFilesDir');
  if ProgramFiles32Dir = '' then
    ProgramFiles32Dir := SystemDrive + '\Program Files';  {don't localize}
  CommonFiles32Dir := GetPath(rv32Bit, 'CommonFilesDir');
  if CommonFiles32Dir = '' then
    CommonFiles32Dir := AddBackslash(ProgramFiles32Dir) + 'Common Files';  {don't localize}

  { Get 64-bit Program Files and Common Files dirs }
  if IsWin64 then begin
    ProgramFiles64Dir := GetPath(rv64Bit, 'ProgramFilesDir');
    if ProgramFiles64Dir = '' then
      InternalError('Failed to get path of 64-bit Program Files directory');
    CommonFiles64Dir := GetPath(rv64Bit, 'CommonFilesDir');
    if CommonFiles64Dir = '' then
      InternalError('Failed to get path of 64-bit Common Files directory');
  end;

  { Get path of command interpreter }
  if IsNT then
    CmdFilename := AddBackslash(WinSystemDir) + 'cmd.exe'
  else
    CmdFilename := AddBackslash(WinDir) + 'COMMAND.COM';

  { Get user info from system }
  ReadSysUserInfo;
end;

procedure SaveStreamToTempFile(const Strm: TCustomMemoryStream;
  const Filename: String);
var
  ErrorCode: DWORD;
begin
  try
    Strm.SaveToFile(Filename);
  except
    { Display more useful error message than 'Stream write error' etc. }
    on EStreamError do begin
      ErrorCode := GetLastError;
      raise Exception.Create(FmtSetupMessage(msgLastErrorMessage,
        [SetupMessages[msgLdrCannotCreateTemp], IntToStr(ErrorCode),
         Win32ErrorString(ErrorCode)]));
    end;
  end;
end;

procedure SaveResourceToTempFile(const ResName, Filename: String);
var
  ResStrm: TResourceStream;
begin
  ResStrm := TResourceStream.Create(HInstance, ResName, RT_RCDATA);
  try
    SaveStreamToTempFile(ResStrm, Filename);
  finally
    ResStrm.Free;
  end;
end;

procedure CreateTempInstallDir;
{ Initializes TempInstallDir and extracts the 64-bit helper into it if needed.
  This is called by Setup, Uninstall, and RegSvr. }
var
  Subdir, ResName, Filename: String;
  ErrorCode: DWORD;
begin
  TempInstallDir := CreateTempDir;
  Log('Created temporary directory: ' + TempInstallDir);
  if Debugging then
    DebugNotifyTempDir(TempInstallDir);

  { Create _isetup subdirectory to hold our internally-used files to ensure
    they won't use any DLLs the install creator might've dumped into
    TempInstallDir }
  Subdir := AddBackslash(TempInstallDir) + '_isetup';
  if not CreateDirectory(PChar(Subdir), nil) then begin
    ErrorCode := GetLastError;
    raise Exception.Create(FmtSetupMessage(msgLastErrorMessage,
      [FmtSetupMessage1(msgErrorCreatingDir, Subdir), IntToStr(ErrorCode),
       Win32ErrorString(ErrorCode)]));
  end;

  { Extract RegDLL EXE }
  {$IFNDEF UNICODE}
  {$R RegDLLEXE.res}
  Filename := Subdir + '\_RegDLL.tmp';
  SaveResourceToTempFile('REGDLL_EXE', Filename);
  {$ENDIF}

  { Extract 64-bit helper EXE, if one is available for the current processor
    architecture }
  ResName := GetHelperResourceName;
  if ResName <> '' then begin
    Filename := Subdir + '\_setup64.tmp';
    SaveResourceToTempFile(ResName, Filename);
    SetHelperExeFilename(Filename);
  end;
end;

function TempDeleteFileProc(const DisableFsRedir: Boolean;
  const FileName: String; const Param: Pointer): Boolean;
var
  Elapsed: DWORD;
label Retry;
begin
Retry:
  Result := DeleteFileRedir(DisableFsRedir, FileName);
  if not Result and
     (GetLastError <> ERROR_FILE_NOT_FOUND) and
     (GetLastError <> ERROR_PATH_NOT_FOUND) then begin
    { If we get here, the file is probably still in use. On an SMP machine,
      it's possible for an EXE to remain locked by Windows for a short time
      after it terminates, causing DeleteFile to fail with ERROR_ACCESS_DENIED.
      (I'm not sure this issue can really be seen here in practice; I could
      only reproduce it consistently by calling DeleteFile() *immediately*
      after waiting on the process handle.)
      Retry if fewer than 2 seconds have passed since DelTree started,
      otherwise assume the error must be permanent and give up. 2 seconds
      ought to be more than enough for the SMP case. }
    Elapsed := GetTickCount - DWORD(Param);
    if Cardinal(Elapsed) < Cardinal(2000) then begin
      Sleep(50);
      goto Retry;
    end;
  end;
end;

procedure RemoveTempInstallDir;
{ Removes TempInstallDir and all its contents. Stops the 64-bit helper first
  if necessary. }
begin
  { Stop 64-bit helper if it's running }
  StopHelper(False);
  SetHelperExeFilename('');

  if TempInstallDir <> '' then begin
    if Debugging then
      DebugNotifyTempDir('');
    if not DelTree(False, TempInstallDir, True, True, True, False, nil,
       TempDeleteFileProc, Pointer(GetTickCount())) then
      Log('Failed to remove temporary directory: ' + TempInstallDir);
  end;
end;

procedure LoadSHFolderDLL;
var
  Filename: String;
  ExistingFileVersion, NewFileVersion: TFileVersionNumbers;
const
  shfolder = 'shfolder.dll';
begin
  Filename := AddBackslash(TempInstallDir) + '_isetup\_shfoldr.dll';
  {$R _shfoldr.res}  { Link in the .res file containing the DLL image }
  SaveResourceToTempFile('SHFOLDERDLL', Filename);
  if not GetVersionNumbers(Filename, NewFileVersion) then
    InternalError('Failed to get version numbers of _shfoldr.dll');
  { Does the system already have the same version or a newer version of
    shfolder.dll? If so, use it instead of the one we just extracted. }
  if GetVersionNumbers(shfolder, ExistingFileVersion) and
     (((ExistingFileVersion.MS > NewFileVersion.MS) or
       ((ExistingFileVersion.MS = NewFileVersion.MS) and
        (ExistingFileVersion.LS > NewFileVersion.LS)))) or
      ((ExistingFileVersion.MS = NewFileVersion.MS) and
       (ExistingFileVersion.LS = NewFileVersion.LS)) then
    Filename := shfolder;
  { Ensure shell32.dll is pre-loaded so it isn't loaded/freed for each
    individual SHGetFolderPath call }
  SafeLoadLibrary(shell32, SEM_NOOPENFILEERRORBOX);
  SHFolderDLLHandle := SafeLoadLibrary(Filename, SEM_NOOPENFILEERRORBOX);
  if SHFolderDLLHandle = 0 then
    InternalError(Format('Failed to load DLL "%s"', [Filename]));
  @SHGetFolderPathFunc := GetProcAddress(SHFolderDLLHandle, {$IFDEF UNICODE}'SHGetFolderPathW'{$ELSE}'SHGetFolderPathA'{$ENDIF});
  if @SHGetFolderPathFunc = nil then
    InternalError('Failed to get address of SHGetFolderPath function');
end;

procedure UnloadSHFolderDLL;
begin
  @SHGetFolderPathFunc := nil;
  if SHFolderDLLHandle <> 0 then begin
    FreeLibrary(SHFolderDLLHandle);
    SHFolderDLLHandle := 0;
  end;
end;

function GetShellFolderByCSIDL(Folder: Integer; const Create: Boolean): String;
const
  CSIDL_FLAG_CREATE = $8000;
  SHGFP_TYPE_CURRENT = 0;
var
  Res: HRESULT;
  Buf: array[0..MAX_PATH-1] of Char;
begin
  if Create then
    Folder := Folder or CSIDL_FLAG_CREATE;

  { Work around a nasty bug in Windows Vista (still present in SP1) and
    Windows Server 2008: When a folder ID resolves to the root directory of a
    drive ('X:\') and the CSIDL_FLAG_CREATE flag is passed, SHGetFolderPath
    fails with code 0x80070005.
    So on Vista only, first try calling the function without CSIDL_FLAG_CREATE.
    If and only if that fails, call it again with the flag.
    Note: The calls *must* be issued in this order; if it's called with the
    flag first, it seems to permanently cache the failure code, causing future
    calls that don't include the flag to fail as well. }
  if (WindowsVersion shr 16 >= $0600) and
     (Folder and CSIDL_FLAG_CREATE <> 0) then
    Res := SHGetFolderPathFunc(0, Folder and not CSIDL_FLAG_CREATE, 0,
      SHGFP_TYPE_CURRENT, Buf)
  else
    Res := E_FAIL;  { always issue the call below }

  if Res <> S_OK then
    Res := SHGetFolderPathFunc(0, Folder, 0, SHGFP_TYPE_CURRENT, Buf);
  if Res = S_OK then
    Result := RemoveBackslashUnlessRoot(PathExpand(Buf))
  else begin
    Result := '';
    LogFmt('Warning: SHGetFolderPath failed with code 0x%.8x on folder 0x%.4x',
      [Res, Folder]);
  end;
end;

function GetRealShellFolder(const Common: Boolean; const ID: TShellFolderID;
  ReadOnly: Boolean): String;

  procedure GetFolder(const Common: Boolean);
  const
    CSIDL_COMMON_STARTMENU = $0016;
    CSIDL_COMMON_PROGRAMS = $0017;
    CSIDL_COMMON_STARTUP = $0018;
    CSIDL_COMMON_DESKTOPDIRECTORY = $0019;
    CSIDL_APPDATA = $001A;
    CSIDL_LOCAL_APPDATA = $001C;
    CSIDL_COMMON_FAVORITES = $001F;
    CSIDL_COMMON_APPDATA = $0023;
    CSIDL_COMMON_TEMPLATES = $002D;
    CSIDL_COMMON_DOCUMENTS = $002E;
    FolderIDs: array[Boolean, TShellFolderID] of Integer = (
      { User }
      (CSIDL_DESKTOPDIRECTORY, CSIDL_STARTMENU, CSIDL_PROGRAMS, CSIDL_STARTUP,
       CSIDL_SENDTO, CSIDL_FONTS, CSIDL_APPDATA, CSIDL_PERSONAL,
       CSIDL_TEMPLATES, CSIDL_FAVORITES, CSIDL_LOCAL_APPDATA),
      { Common }
      (CSIDL_COMMON_DESKTOPDIRECTORY, CSIDL_COMMON_STARTMENU, CSIDL_COMMON_PROGRAMS, CSIDL_COMMON_STARTUP,
       CSIDL_SENDTO, CSIDL_FONTS, CSIDL_COMMON_APPDATA, CSIDL_COMMON_DOCUMENTS,
       CSIDL_COMMON_TEMPLATES, CSIDL_COMMON_FAVORITES, CSIDL_LOCAL_APPDATA));
  var
    Z: String;
  begin
    if not ShellFoldersRead[Common, ID] then begin
      { Note: Must pass Create=True or else SHGetFolderPath fails if the
        specified CSIDL is valid but doesn't currently exist. }
      Z := GetShellFolderByCSIDL(FolderIDs[Common, ID], not ReadOnly);
      ShellFolders[Common, ID] := Z;
      if not ReadOnly or (Z <> '') then
        ShellFoldersRead[Common, ID] := True;
    end;
    Result := ShellFolders[Common, ID];
  end;

begin
  Result := '';
  GetFolder(Common);
  if (Result = '') and Common then
    { If it failed to get the path of a Common CSIDL, try getting the
      User version of the CSIDL instead. (Many of the Common CSIDLS are
      unsupported by Win9x.) }
    GetFolder(False);
end;

function GetShellFolder(Common: Boolean; const ID: TShellFolderID;
  ReadOnly: Boolean): String;
begin
  { If the user isn't an administrator, or is running Windows 9x, always fall
    back to user folders, except in the case of sfAppData (which is writable
    by Users on XP) and sfDocs (which is writable by Users on 2000 & XP) }
  if Common and (not IsAdmin or (SetupHeader.PrivilegesRequired = prLowest) or not IsNT) and
     not(ID in [sfAppData, sfDocs]) then
    Common := False;
  Result := GetRealShellFolder(Common, ID, ReadOnly);
end;

function InstallOnThisVersion(const MinVersion: TSetupVersionData;
  const OnlyBelowVersion: TSetupVersionData): TInstallOnThisVersionResult;
var
  Ver, Ver2, MinVer, OnlyBelowVer: Cardinal;
begin
  Ver := WindowsVersion;
  if IsNT then begin
    MinVer := MinVersion.NTVersion;
    OnlyBelowVer := OnlyBelowVersion.NTVersion;
  end
  else begin
    MinVer := 0;
    OnlyBelowVer := 0;
  end;
  Result := irInstall;
  if MinVer = 0 then
    Result := irNotOnThisPlatform
  else begin
    if Ver < MinVer then
      Result := irVersionTooLow
    else if (IsNT and (LongRec(Ver).Hi = LongRec(MinVer).Hi) and
        (NTServicePackLevel < MinVersion.NTServicePack)) then
      Result := irServicePackTooLow
    else begin
      if OnlyBelowVer <> 0 then begin
        Ver2 := Ver;
        { A build number of 0 on OnlyBelowVersion means 'match any build' }
        if LongRec(OnlyBelowVer).Lo = 0 then
          Ver2 := Ver2 and $FFFF0000;  { set build number to zero on Ver2 also }
        if not IsNT then begin
          if Ver2 >= OnlyBelowVer then
            Result := irVerTooHigh;
        end
        else begin
          { Note: When OnlyBelowVersion includes a service pack level, the
            version number test changes from a "<" to "<=" operation. Thus,
            on Windows 2000 SP4, 5.0 and 5.0.2195 will fail, but 5.0sp5 and
            5.0.2195sp5 will pass. }
          if (Ver2 > OnlyBelowVer) or
             ((Ver2 = OnlyBelowVer) and
              (OnlyBelowVersion.NTServicePack = 0)) or
             ((LongRec(Ver).Hi = LongRec(OnlyBelowVer).Hi) and
              (OnlyBelowVersion.NTServicePack <> 0) and
              (NTServicePackLevel >= OnlyBelowVersion.NTServicePack)) then
            Result := irVerTooHigh;
        end;
      end;
    end;
  end;
end;

function GetSizeOfComponent(const ComponentName: String; const ExtraDiskSpaceRequired: Integer64): Integer64;
var
  ComponentNameAsList: TStringList;
  FileEntry: PSetupFileEntry;
  I: Integer;
begin
  Result := ExtraDiskSpaceRequired;

  ComponentNameAsList := TStringList.Create();
  try
    ComponentNameAsList.Add(ComponentName);
    for I := 0 to Entries[seFile].Count-1 do begin
      FileEntry := PSetupFileEntry(Entries[seFile][I]);
      with FileEntry^ do begin
        if (Components <> '') and
           ((Tasks = '') and (Check = '')) then begin {don't count tasks or scripted entries}
          if ShouldProcessFileEntry(ComponentNameAsList, nil, FileEntry, True) then begin
            if LocationEntry <> -1 then
              Inc6464(Result, PSetupFileLocationEntry(Entries[seFileLocation][LocationEntry])^.OriginalSize)
            else
              Inc6464(Result, ExternalSize);
            end;
        end;
      end;
    end;
  finally
    ComponentNameAsList.Free();
  end;
end;

function GetSizeOfType(const TypeName: String; const IsCustom: Boolean): Integer64;
var
  ComponentTypes: TStringList;
  I: Integer;
begin
  Result.Hi := 0;
  Result.Lo := 0;
  ComponentTypes := TStringList.Create();

  for I := 0 to Entries[seComponent].Count-1 do begin
    with PSetupComponentEntry(Entries[seComponent][I])^ do begin
      SetStringsFromCommaString(ComponentTypes, Types);
      { For custom types, only count fixed components. Otherwise count all. }
      if IsCustom then begin
        if (coFixed in Options) and ListContains(ComponentTypes, TypeName) then
          Inc6464(Result, Size);
      end else begin
        if ListContains(ComponentTypes, TypeName) then
          Inc6464(Result, Size);
      end;
    end;
  end;

  ComponentTypes.Free();
end;

function IsRecurseableDirectory(const FindData: TWin32FindData): Boolean;
{ Returns True if FindData is a directory that may be recursed into.
  Intended only for use when processing external+recursesubdirs file entries. }
begin
  Result :=
    (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and
    (FindData.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN = 0) and
    (StrComp(FindData.cFileName, '.') <> 0) and
    (StrComp(FindData.cFileName, '..') <> 0);
end;

type
  TEnumFilesProc = function(const DisableFsRedir: Boolean; const Filename: String;
    const Param: Pointer): Boolean;

function DummyDeleteDirProc(const DisableFsRedir: Boolean; const Filename: String;
    const Param: Pointer): Boolean;
begin
  { We don't actually want to delete the dir, so just return success. }
  Result := True;
end;

{ Enumerates the files we're going to install and delete. Returns True on success.
  Likewise EnumFilesProc should return True on success and return False
  to break the enum and to cause EnumFiles to return False instead of True. }
function EnumFiles(const EnumFilesProc: TEnumFilesProc;
  const WizardComponents, WizardTasks: TStringList; const Param: Pointer): Boolean;

  function RecurseExternalFiles(const DisableFsRedir: Boolean;
    const SearchBaseDir, SearchSubDir, SearchWildcard: String;
    const SourceIsWildcard: Boolean; const CurFile: PSetupFileEntry): Boolean;
  var
    SearchFullPath, DestName: String;
    H: THandle;
    FindData: TWin32FindData;
  begin
    SearchFullPath := SearchBaseDir + SearchSubDir + SearchWildcard;
    Result := True;

    H := FindFirstFileRedir(DisableFsRedir, SearchFullPath, FindData);
    if H <> INVALID_HANDLE_VALUE then begin
      try
        repeat
          if FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then begin

            if SourceIsWildcard then
              if FindData.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN <> 0 then
                Continue;

            DestName := ExpandConst(CurFile^.DestName);
            if not(foCustomDestName in CurFile^.Options) then
              DestName := DestName + SearchSubDir + FindData.cFileName
            else if SearchSubDir <> '' then
              DestName := PathExtractPath(DestName) + SearchSubDir + PathExtractName(DestName);
            if not EnumFilesProc(DisableFsRedir, DestName, Param) then begin
              Result := False;
              Exit;
            end;
          end;
        until not FindNextFile(H, FindData);
      finally
        Windows.FindClose(H);
      end;
    end;

    if foRecurseSubDirsExternal in CurFile^.Options then begin
      H := FindFirstFileRedir(DisableFsRedir, SearchBaseDir + SearchSubDir + '*', FindData);
      if H <> INVALID_HANDLE_VALUE then begin
        try
          repeat
            if IsRecurseableDirectory(FindData) then
              if not RecurseExternalFiles(DisableFsRedir, SearchBaseDir,
                 SearchSubDir + FindData.cFileName + '\', SearchWildcard,
                 SourceIsWildcard, CurFile) then begin
                Result := False;
                Exit;
              end;
          until not FindNextFile(H, FindData);
        finally
          Windows.FindClose(H);
        end;
      end;
    end;
  end;

var
  I: Integer;
  CurFile: PSetupFileEntry;
  DisableFsRedir: Boolean;
  SourceWildcard: String;
begin
  Result := True;

  { [Files] }
  for I := 0 to Entries[seFile].Count-1 do begin
    CurFile := PSetupFileEntry(Entries[seFile][I]);
    if (CurFile^.FileType = ftUserFile) and
       ShouldProcessFileEntry(WizardComponents, WizardTasks, CurFile, False) then begin
      DisableFsRedir := ShouldDisableFsRedirForFileEntry(CurFile);
      if CurFile^.LocationEntry <> -1 then begin
        { Non-external file }
        if not EnumFilesProc(DisableFsRedir, ExpandConst(CurFile^.DestName), Param) then begin
          Result := False;
          Exit;
        end;
      end
      else begin
        { External file }
        SourceWildcard := ExpandConst(CurFile^.SourceFilename);
        if not RecurseExternalFiles(DisableFsRedir, PathExtractPath(SourceWildcard), '',
           PathExtractName(SourceWildcard), IsWildcard(SourceWildcard), CurFile) then begin
          Result := False;
          Exit;
        end;
      end;
    end;
  end;

  { [InstallDelete] }
    for I := 0 to Entries[seInstallDelete].Count-1 do
      with PSetupDeleteEntry(Entries[seInstallDelete][I])^ do
        if ShouldProcessEntry(WizardComponents, WizardTasks, Components, Tasks, Languages, Check) then begin
          case DeleteType of
            dfFiles, dfFilesAndOrSubdirs:
              if not DelTree(InstallDefaultDisableFsRedir, ExpandConst(Name), False, True, DeleteType = dfFilesAndOrSubdirs, True,
                 DummyDeleteDirProc, EnumFilesProc, nil) then begin
                Result := False;
                Exit;
              end;
            dfDirIfEmpty:
              if not DelTree(InstallDefaultDisableFsRedir, ExpandConst(Name), True, False, False, True,
                 DummyDeleteDirProc, EnumFilesProc, nil) then begin
                Result := False;
                Exit;
              end;
          end;
        end;
end;

procedure EnumProc(const Filename: String; Param: Pointer);
begin
  TStringList(Param).Add(PathLowercase(Filename));
end;

var
  CheckForFileSL: TStringList;

function CheckForFile(const DisableFsRedir: Boolean; const AFilename: String;
  const Param: Pointer): Boolean;
var
  Filename: String;
  J: Integer;
begin
  Filename := AFilename;
  if IsNT then begin
    if not DisableFsRedir then
      Filename := ReplaceSystemDirWithSysWow64(Filename);
  end
  else
    Filename := GetShortName(Filename);
  Filename := PathLowercase(Filename);
  for J := 0 to CheckForFileSL.Count-1 do begin
    if CheckForFileSL[J] = Filename then begin
      LogFmt('Found pending rename or delete that matches one of our files: %s', [Filename]);
      Result := False; { Break the enum, just need to know if any matches }
      Exit;
    end;
  end;
  Result := True; { Success! }
end;

{ Checks if no file we're going to install or delete has a pending rename or delete. }
function PreviousInstallCompleted(const WizardComponents, WizardTasks: TStringList): Boolean;
begin
  Result := True;
  if Entries[seFile].Count = 0 then
    Exit;
  CheckForFileSL := TStringList.Create;
  try
    EnumFileReplaceOperationsFilenames(EnumProc, CheckForFileSL);
    if CheckForFileSL.Count = 0 then
      Exit;
    Result := EnumFiles(CheckForFile, WizardComponents, WizardTasks, nil);
  finally
    CheckForFileSL.Free;
  end;
end;

type
  TArrayOfPWideChar = array[0..(MaxInt div SizeOf(PWideChar))-1] of PWideChar;
  PArrayOfPWideChar = ^TArrayOfPWideChar;

var
  RegisterFileFilenames: PArrayOfPWideChar;
  RegisterFileFilenamesMax, RegisterFileFilenamesCount: Integer;

function RegisterFile(const DisableFsRedir: Boolean; const AFilename: String;
  const Param: Pointer): Boolean;
var
  Filename: String;
  I, Len: Integer;
  Match: Boolean;
begin
  Filename := AFilename;

  { First: check filter. }
  if Filename <> '' then begin
    Match := False;
    for I := 0 to CloseApplicationsFilterList.Count-1 do begin
      if WildcardMatch(PChar(PathExtractName(Filename)), PChar(CloseApplicationsFilterList[I])) then begin
        Match := True;
        Break;
      end;
    end;
    if not Match then begin
      Result := True;
      Exit;
    end;
  end;

  { Secondly: check if we need to register this batch, either because the batch is full
    or because we're done scanning and have leftovers. }
  if ((Filename <> '') and (RegisterFileFilenamesCount = RegisterFileFilenamesMax)) or
     ((Filename = '') and (RegisterFileFilenamesCount > 0)) then begin
    if RmRegisterResources(RmSessionHandle, RegisterFileFilenamesCount, RegisterFileFilenames, 0, nil, 0, nil) = ERROR_SUCCESS then begin
      for I := 0 to RegisterFileFilenamesCount-1 do
        FreeMem(RegisterFileFilenames[I]);
      RegisterFileFilenamesCount := 0;
    end else begin
      RmEndSession(RmSessionHandle);
      RmSessionStarted := False;
    end;
  end;

  { Finally: add this file to the batch. }
  if RmSessionStarted and (FileName <> '') then begin
    { From MSDN: "Installers should not disable file system redirection before calling
      the Restart Manager API. This means that a 32-bit installer run on 64-bit Windows
      is unable register a file in the %windir%\system32 directory." This is incorrect,
      we can register such files by using the Sysnative alias. Note: the Sysnative alias
      is only available on Windows Vista and newer, but so is Restart Manager. }
    if DisableFsRedir then
      Filename := ReplaceSystemDirWithSysNative(Filename, IsWin64);

    Len := Length(Filename);
    GetMem(RegisterFileFilenames[RegisterFileFilenamesCount], (Len + 1) * SizeOf(RegisterFileFilenames[RegisterFileFilenamesCount][0]));
    {$IFNDEF UNICODE}
      RegisterFileFilenames[RegisterFileFilenamesCount][MultiByteToWideChar(CP_ACP, 0, PChar(Filename), Len, RegisterFileFilenames[RegisterFileFilenamesCount], Len)] := #0;
    {$ELSE}
      StrPCopy(RegisterFileFilenames[RegisterFileFilenamesCount], Filename);
    {$ENDIF}
    Inc(RegisterFileFilenamesCount);
  end;

  Result := RmSessionStarted; { Break the enum if there was an error, else continue. }
end;

{ Register all files we're going to install or delete. Ends RmSession on errors. }
procedure RegisterResourcesWithRestartManager(const WizardComponents, WizardTasks: TStringList);
var
  I: Integer;
begin
  { Note: MSDN says we shouldn't call RmRegisterResources for each file because of speed, but calling
    it once for all files adds extra memory usage, so calling it in batches. }
  RegisterFileFilenamesMax := 1000;
  GetMem(RegisterFileFilenames, RegisterFileFilenamesMax * SizeOf(RegisterFileFilenames[0]));
  try
    EnumFiles(RegisterFile, WizardComponents, WizardTasks, nil);
    { Don't forget to register leftovers. }
    if RmSessionStarted then
      RegisterFile(False, '', nil);
  finally
    for I := 0 to RegisterFileFilenamesCount-1 do
      FreeMem(RegisterFileFilenames[I]);
    FreeMem(RegisterFileFilenames);
  end;
end;

procedure DebugNotifyEntry(EntryType: TEntryType; Number: Integer);
var
  Kind: TDebugEntryKind;
  B: Boolean;
begin
  if not Debugging then Exit;
  case EntryType of
    seDir: Kind := deDir;
    seFile: Kind := deFile;
    seIcon: Kind := deIcon;
    seIni: Kind := deIni;
    seRegistry: Kind := deRegistry;
    seInstallDelete: Kind := deInstallDelete;
    seUninstallDelete: Kind := deUninstallDelete;
    seRun: Kind := deRun;
    seUninstallRun: Kind := deUninstallRun;
  else
    Exit;
  end;
  DebugNotify(Kind, Integer(OriginalEntryIndexes[EntryType][Number]), B);
end;

procedure CodeRunnerOnDllImport(var DllName: String; var ForceDelayLoad: Boolean);
var
  S, BaseName, FullName: String;
  FirstFile: Boolean;
  P: Integer;
begin
  while True do begin
    if Pos('setup:', DllName) = 1 then begin
      if IsUninstaller then begin
        DllName := '';
        ForceDelayLoad := True;
        Exit;
      end;
      Delete(DllName, 1, Length('setup:'));
    end
    else if Pos('uninstall:', DllName) = 1 then begin
      if not IsUninstaller then begin
        DllName := '';
        ForceDelayLoad := True;
        Exit;
      end;
      Delete(DllName, 1, Length('uninstall:'));
    end
    else
      Break;
  end;

  if Pos('files:', DllName) = 1 then begin
    if IsUninstaller then begin
      { Uninstall doesn't do 'files:' }
      DllName := '';
      ForceDelayLoad := True;
    end
    else begin
      S := Copy(DllName, Length('files:')+1, Maxint);
      FirstFile := True;
      repeat
        P := ConstPos(',', S);
        if P = 0 then
          BaseName := S
        else begin
          BaseName := Copy(S, 1, P-1);
          Delete(S, 1, P);
        end;
        BaseName := ExpandConst((BaseName));
        FullName := AddBackslash(TempInstallDir) + BaseName;
        if not NewFileExists(FullName) then
          ExtractTemporaryFile(BaseName);
        if FirstFile then begin
          DllName := FullName;
          FirstFile := False;
        end;
      until P = 0;
    end;
  end
  else
    DllName := ExpandConst(DllName);
end;

function CodeRunnerOnDebug(const Position: LongInt;
  var ContinueStepOver: Boolean): Boolean;
begin
  Result := DebugNotify(deCodeLine, Position, ContinueStepOver);
end;

function CodeRunnerOnDebugIntermediate(const Position: LongInt;
  var ContinueStepOver: Boolean): Boolean;
begin
  Result := DebugNotifyIntermediate(deCodeLine, Position, ContinueStepOver);
end;

procedure CodeRunnerOnException(const Exception: AnsiString; const Position: LongInt);
begin
  if Debugging then
    DebugNotifyException(String(Exception), deCodeLine, Position);
end;

procedure SetActiveLanguage(const I: Integer);
{ Activates the specified language }
const
{$IFDEF UNICODE}
  { UNICODE requires 2000+, so we can just use the English name }
  SMSPGothic = 'MS PGothic';
{$ELSE}
  { "MS PGothic" in Japanese (CP 932) }
  SMSPGothic = #$82'l'#$82'r '#$82'o'#$83'S'#$83'V'#$83'b'#$83'N';
{$ENDIF}
var
  LangEntry: PSetupLanguageEntry;
  J: Integer;
begin
  if ActiveLanguage = I then
    Exit;

  LangEntry := Entries[seLanguage][I];

  AssignSetupMessages(LangEntry.Data[1], Length(LangEntry.Data));

  ActiveLanguage := I;
  Finalize(LangOptions);  { prevent leak on D2 }
  LangOptions := LangEntry^;

  { Hack for Japanese: Override the default fonts on older versions of Windows
    that don't (fully) support font linking }
  if (LangOptions.LanguageID = $0411) and
{$IFNDEF UNICODE}
     (GetACP = 932) and
{$ENDIF}
     (WindowsVersion < Cardinal($05010000)) and
     FontExists(SMSPGothic) then begin
    { Windows <= 2000: Verdana can't display Japanese }
    LangOptions.WelcomeFontName := SMSPGothic;
    LangOptions.WelcomeFontSize := 12;
{$IFNDEF UNICODE}
    if WindowsVersion < Cardinal($05000000) then begin
      { Windows 9x/Me/NT 4.0: MS Sans Serif can't display Japanese }
      LangOptions.DialogFontName := SMSPGothic;
      LangOptions.DialogFontSize := 9;
      LangOptions.TitleFontName := SMSPGothic;
      LangOptions.TitleFontSize := 29;
      LangOptions.CopyrightFontName := SMSPGothic;
      LangOptions.CopyrightFontSize := 9;
    end;
{$ENDIF}
  end;

  if LangEntry.LicenseText <> '' then
    ActiveLicenseText := LangEntry.LicenseText
  else
    ActiveLicenseText := SetupHeader.LicenseText;

  if LangEntry.InfoBeforeText <> '' then
    ActiveInfoBeforeText := LangEntry.InfoBeforeText
  else
    ActiveInfoBeforeText := SetupHeader.InfoBeforeText;

  if LangEntry.InfoAfterText <> '' then
    ActiveInfoAfterText := LangEntry.InfoAfterText
  else
    ActiveInfoAfterText := SetupHeader.InfoAfterText;

  SetMessageBoxRightToLeft(LangOptions.RightToLeft);
  SetMessageBoxCaption(mbInformation, PChar(SetupMessages[msgInformationTitle]));
  SetMessageBoxCaption(mbConfirmation, PChar(SetupMessages[msgConfirmTitle]));
  SetMessageBoxCaption(mbError, PChar(SetupMessages[msgErrorTitle]));
  SetMessageBoxCaption(mbCriticalError, PChar(SetupMessages[msgErrorTitle]));
  Application.Title := SetupMessages[msgSetupAppTitle];

  for J := 0 to Entries[seType].Count-1 do begin
    with PSetupTypeEntry(Entries[seType][J])^ do begin
      case Typ of
        ttDefaultFull: Description := SetupMessages[msgFullInstallation];
        ttDefaultCompact: Description := SetupMessages[msgCompactInstallation];
        ttDefaultCustom: Description := SetupMessages[msgCustomInstallation];
      end;
    end;
  end;

  { Tell the first instance to change its language too. (It's possible for
    the first instance to display messages after Setup terminates, e.g. if it
    fails to restart the computer.) }
  if SetupNotifyWndPresent then
    SendNotifyMessage(SetupNotifyWnd, WM_USER + 150, 10001, I);
end;

function GetLanguageEntryProc(Index: Integer; var Entry: PSetupLanguageEntry): Boolean;
begin
  Result := False;
  if Index < Entries[seLanguage].Count then begin
    Entry := Entries[seLanguage][Index];
    Result := True;
  end;
end;

procedure ActivateDefaultLanguage;
{ Auto-detects the most appropriate language and activates it.
  Also initializes the ShowLanguageDialog variable.
  Note: A like-named version of this function is also present in SetupLdr.dpr. }
var
  I: Integer;
begin
  case DetermineDefaultLanguage(GetLanguageEntryProc,
     SetupHeader.LanguageDetectionMethod, InitLang, I) of
    ddNoMatch: ShowLanguageDialog := (SetupHeader.ShowLanguageDialog <> slNo);
    ddMatch: ShowLanguageDialog := (SetupHeader.ShowLanguageDialog = slYes);
  else
    { ddMatchLangParameter }
    ShowLanguageDialog := False;
  end;
  SetActiveLanguage(I);
end;

procedure SetTaskbarButtonVisibility(const AVisible: Boolean);
var
  ExStyle: Longint;
begin
  { The taskbar button is hidden by setting the WS_EX_TOOLWINDOW style on the
    application window. We can't simply hide the window because on D3+ the VCL
    would just show it again in TApplication.UpdateVisible when the first form
    is shown. }
{$IFDEF IS_D12}
  TaskbarButtonHidden := not AVisible;  { see WM_STYLECHANGING hook in Setup.dpr }
{$ENDIF}
  if (GetWindowLong(Application.Handle, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = 0) <> AVisible then begin
    SetWindowPos(Application.Handle, 0, 0, 0, 0, 0, SWP_NOSIZE or
      SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    ExStyle := GetWindowLong(Application.Handle, GWL_EXSTYLE);
    if AVisible then
      ExStyle := ExStyle and not WS_EX_TOOLWINDOW
    else
      ExStyle := ExStyle or WS_EX_TOOLWINDOW;
    SetWindowLong(Application.Handle, GWL_EXSTYLE, ExStyle);
    if AVisible then
      { Show and activate when becoming visible }
      ShowWindow(Application.Handle, SW_SHOW)
    else
      SetWindowPos(Application.Handle, 0, 0, 0, 0, 0, SWP_NOSIZE or
        SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  end;
end;

procedure LogWindowsVersion;
var
  SP: String;
begin
  if NTServicePackLevel <> 0 then begin
    SP := ' SP' + IntToStr(Hi(NTServicePackLevel));
    if Lo(NTServicePackLevel) <> 0 then
      SP := SP + '.' + IntToStr(Lo(NTServicePackLevel));
  end;
  LogFmt('Windows version: %u.%u.%u%s  (NT platform: %s)', [WindowsVersion shr 24,
    (WindowsVersion shr 16) and $FF, WindowsVersion and $FFFF, SP, SYesNo[IsNT]]);
  LogFmt('64-bit Windows: %s', [SYesNo[IsWin64]]);
  LogFmt('Processor architecture: %s', [SetupProcessorArchitectureNames[ProcessorArchitecture]]);

  if IsNT then begin
    if IsAdmin then
      Log('User privileges: Administrative')
    else if IsPowerUserOrAdmin then
      Log('User privileges: Power User')
    else
      Log('User privileges: None');
  end;
end;

function GetMessageBoxResultText(const AResult: Integer): String;
const
  IDTRYAGAIN = 10;
  IDCONTINUE = 11;
begin
  case AResult of
    IDOK: Result := 'OK';
    IDCANCEL: Result := 'Cancel';
    IDABORT: Result := 'Abort';
    IDRETRY: Result := 'Retry';
    IDIGNORE: Result := 'Ignore';
    IDYES: Result := 'Yes';
    IDNO: Result := 'No';
    IDTRYAGAIN: Result := 'Try Again';
    IDCONTINUE: Result := 'Continue';
  else
    Result := IntToStr(AResult);
  end;
end;

function GetButtonsText(const Buttons: Cardinal): String;
const
  { We don't use this type, but end users are liable to in [Code] }
  MB_CANCELTRYCONTINUE = $00000006;
begin
  case Buttons and MB_TYPEMASK of
    MB_OK: Result := 'OK';
    MB_OKCANCEL: Result := 'OK/Cancel';
    MB_ABORTRETRYIGNORE: Result := 'Abort/Retry/Ignore';
    MB_YESNOCANCEL: Result := 'Yes/No/Cancel';
    MB_YESNO: Result := 'Yes/No';
    MB_RETRYCANCEL: Result := 'Retry/Cancel';
    MB_CANCELTRYCONTINUE: Result := 'Cancel/Try Again/Continue';
  else
    Result := IntToStr(Buttons and MB_TYPEMASK);
  end;
end;

procedure LogSuppressedMessageBox(const Text: PChar; const Buttons: Cardinal;
  const Default: Integer);
begin
  Log(Format('Defaulting to %s for suppressed message box (%s):' + SNewLine,
    [GetMessageBoxResultText(Default), GetButtonsText(Buttons)]) + Text);
end;

procedure LogMessageBox(const Text: PChar; const Buttons: Cardinal);
begin
  Log(Format('Message box (%s):' + SNewLine,
    [GetButtonsText(Buttons)]) + Text);
end;

function LoggedAppMessageBox(const Text, Caption: PChar; const Flags: Longint;
  const Suppressible: Boolean; const Default: Integer): Integer;
begin
  if InitSuppressMsgBoxes and Suppressible then begin
    LogSuppressedMessageBox(Text, Flags, Default);
    Result := Default;
  end else begin
    LogMessageBox(Text, Flags);
    Result := AppMessageBox(Text, Caption, Flags);
    if Result <> 0 then
      LogFmt('User chose %s.', [GetMessageBoxResultText(Result)])
    else
      Log('AppMessageBox failed.');
  end;
end;

function LoggedMsgBox(const Text, Caption: String; const Typ: TMsgBoxType;
  const Buttons: Cardinal; const Suppressible: Boolean; const Default: Integer): Integer;
begin
  if InitSuppressMsgBoxes and Suppressible then begin
    LogSuppressedMessageBox(PChar(Text), Buttons, Default);
    Result := Default;
  end else begin
    LogMessageBox(PChar(Text), Buttons);
    Result := MsgBox(Text, Caption, Typ, Buttons);
    if Result <> 0 then
      LogFmt('User chose %s.', [GetMessageBoxResultText(Result)])
    else
      Log('MsgBox failed.');
  end;
end;

procedure RestartComputerFromThisProcess;
begin
  RestartInitiatedByThisProcess := True;
  { Note: Depending on the OS, RestartComputer may not return if successful }
  if not RestartComputer then begin
    { Hack for when called from RespawnSetupElevated: re-show the
      application's taskbar button } 
    ShowWindow(Application.Handle, SW_SHOW);
    { If another app denied the shutdown, we probably lost the foreground;
      try to take it back. (Note: Application.BringToFront can't be used
      because we have no visible forms, and MB_SETFOREGROUND doesn't make
      the app's taskbar button blink.) }
    SetForegroundWindow(Application.Handle);
    LoggedMsgBox(SetupMessages[msgErrorRestartingComputer], '', mbError,
      MB_OK, True, IDOK);
  end;
end;

procedure RespawnSetupElevated(const AParams: String);
{ Starts a new, elevated Setup(Ldr) process and waits until it terminates.
  Does not return; either calls Halt or raises an exception. }
var
  Cancelled: Boolean;
  Server: TSpawnServer;
  ParamNotifyWnd: HWND;
  RespawnResults: record
    ExitCode: DWORD;
    NotifyRestartRequested: Boolean;
    NotifyNewLanguage: Integer;
  end;
begin
  { Hide the taskbar button }
  SetWindowPos(Application.Handle, 0, 0, 0, 0, 0, SWP_NOSIZE or
    SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Cancelled := False;
  try
    Server := TSpawnServer.Create;
    try
      if SetupNotifyWndPresent then
        ParamNotifyWnd := SetupNotifyWnd
      else
        ParamNotifyWnd := Server.Wnd;
      RespawnSelfElevated(SetupLdrOriginalFilename,
        Format('/SPAWNWND=$%x /NOTIFYWND=$%x ', [Server.Wnd, ParamNotifyWnd]) +
        AParams, RespawnResults.ExitCode);
      RespawnResults.NotifyRestartRequested := Server.NotifyRestartRequested;
      RespawnResults.NotifyNewLanguage := Server.NotifyNewLanguage;
    finally
      Server.Free;
    end;
  except
    { If the user clicked Cancel on the dialog, halt with special exit code }
    if ExceptObject is EAbort then
      Cancelled := True
    else begin
      { Otherwise, re-show the taskbar button and re-raise }
      ShowWindow(Application.Handle, SW_SHOW);
      raise;
    end;
  end;
  if Cancelled then
    Halt(ecCancelledBeforeInstall);

  if not SetupNotifyWndPresent then begin
    { In the UseSetupLdr=no case, there is no notify window handle to pass to
      RespawnSelfElevated, so it hosts one itself. Process the results. }
    try
      if (RespawnResults.NotifyNewLanguage >= 0) and
         (RespawnResults.NotifyNewLanguage < Entries[seLanguage].Count) then
        SetActiveLanguage(RespawnResults.NotifyNewLanguage);
      if RespawnResults.NotifyRestartRequested then begin
        { Note: Depending on the OS, this may not return if successful }
        RestartComputerFromThisProcess;
      end;
    except
      { In the unlikely event that something above raises an exception, handle
        it here so the right exit code will still be returned below }
      ShowWindow(Application.Handle, SW_SHOW);
      Application.HandleException(nil);
    end;
  end;

  Halt(RespawnResults.ExitCode);
end;

procedure InitializeCommonVars;
{ Initializes variables shared between Setup and Uninstall }
begin
  IsAdmin := IsAdminLoggedOn;
  IsPowerUserOrAdmin := IsAdmin or IsPowerUserLoggedOn;
  Randomize;
end;

procedure Initialize64BitInstallMode(const A64Bit: Boolean);
{ Initializes Is64BitInstallMode and other global variables that depend on it }
begin
  LogFmt('64-bit install mode: %s', [SYesNo[A64Bit]]);
  Is64BitInstallMode := A64Bit;
  InstallDefaultDisableFsRedir := A64Bit;
  ScriptFuncDisableFsRedir := A64Bit;
  if A64Bit then
    InstallDefaultRegView := rv64Bit
  else
    InstallDefaultRegView := rv32Bit;
end;

procedure InitializeSetup;
{ Initializes various vars used by the setup. This is called in the project
  source. }
var
  DecompressorDLL, DecryptDLL: TMemoryStream;

  function ExtractLongWord(var S: String): LongWord;
  var
    P: Integer;
  begin
    P := PathPos(',', S);
    if P = 0 then
      raise Exception.Create('ExtractLongWord: Missing comma');
    Result := LongWord(StrToInt(Copy(S, 1, P-1)));
    Delete(S, 1, P);
  end;

  function ArchitecturesToStr(const Architectures: TSetupProcessorArchitectures): String;

    procedure AppendLine(var S: String; const L: String);
    begin
      if S <> '' then
        S := S + #13#10 + L
      else
        S := L;
    end;

  var
    I: TSetupProcessorArchitecture;
  begin
    Result := '';
    for I := Low(I) to High(I) do
      if I in Architectures then
        AppendLine(Result, SetupProcessorArchitectureNames[I]);
  end;

  procedure AbortInit(const Msg: TSetupMessageID);
  begin
    LoggedMsgBox(SetupMessages[Msg], '', mbCriticalError, MB_OK, True, IDOK);
    Abort;
  end;

  procedure AbortInitFmt1(const Msg: TSetupMessageID; const Arg1: String);
  begin
    LoggedMsgBox(FmtSetupMessage(Msg, [Arg1]), '', mbCriticalError, MB_OK, True, IDOK);
    Abort;
  end;

  procedure AbortInitServicePackRequired(const ServicePack: Word);
  begin
    LoggedMsgBox(FmtSetupMessage(msgWindowsServicePackRequired, ['Windows',
      IntToStr(Hi(ServicePack))]), '', mbCriticalError, MB_OK, True, IDOK);
    Abort;
  end;

  procedure ReadFileIntoStream(const Stream: TStream;
    const R: TCompressedBlockReader);
  type
    PBuffer = ^TBuffer;
    TBuffer = array[0..8191] of Byte;
  var
    Buf: PBuffer;
    BytesLeft, Bytes: Longint;
  begin
    New(Buf);
    try
      R.Read(BytesLeft, SizeOf(BytesLeft));
      while BytesLeft > 0 do begin
        Bytes := BytesLeft;
        if Bytes > SizeOf(Buf^) then Bytes := SizeOf(Buf^);
        R.Read(Buf^, Bytes);
        Stream.WriteBuffer(Buf^, Bytes);
        Dec(BytesLeft, Bytes);
      end;
    finally
      Dispose(Buf);
    end;
  end;

  procedure ReadWizardImage(var WizardImage: TBitmap; const R: TCompressedBlockReader);
  var
    MemStream: TMemoryStream;
  begin
    MemStream := TMemoryStream.Create;
    try
      ReadFileIntoStream(MemStream, R);
      MemStream.Seek(0, soFromBeginning);
      WizardImage := TBitmap.Create;
      WizardImage.LoadFromStream(MemStream);
    finally
      MemStream.Free;
    end;
  end;

  procedure LoadDecompressorDLL;
  var
    Filename: String;
  begin
    Filename := AddBackslash(TempInstallDir) + '_isetup\_isdecmp.dll';
    SaveStreamToTempFile(DecompressorDLL, Filename);
    FreeAndNil(DecompressorDLL);
    DecompressorDLLHandle := SafeLoadLibrary(Filename, SEM_NOOPENFILEERRORBOX);
    if DecompressorDLLHandle = 0 then
      InternalError(Format('Failed to load DLL "%s"', [Filename]));
    case SetupHeader.CompressMethod of
      cmZip:
        if not ZlibInitDecompressFunctions(DecompressorDLLHandle) then
          InternalError('ZlibInitDecompressFunctions failed');
      cmBzip:
        if not BZInitDecompressFunctions(DecompressorDLLHandle) then
          InternalError('BZInitDecompressFunctions failed');
    end;
  end;

  procedure LoadDecryptDLL;
  var
    Filename: String;
  begin
    Filename := AddBackslash(TempInstallDir) + '_isetup\_iscrypt.dll';
    SaveStreamToTempFile(DecryptDLL, Filename);
    FreeAndNil(DecryptDLL);
    DecryptDLLHandle := SafeLoadLibrary(Filename, SEM_NOOPENFILEERRORBOX);
    if DecryptDLLHandle = 0 then
      InternalError(Format('Failed to load DLL "%s"', [Filename]));
    if not ArcFourInitFunctions(DecryptDLLHandle) then
      InternalError('ISCryptInitFunctions failed');
  end;

var
  Reader: TCompressedBlockReader;

  procedure ReadEntriesWithoutVersion(const EntryType: TEntryType;
    const Count: Integer; const Size: Integer);
  var
    I: Integer;
    P: Pointer;
  begin
    Entries[EntryType].Capacity := Count;
    for I := 0 to Count-1 do begin
      P := AllocMem(Size);
      SECompressedBlockRead(Reader, P^, Size, EntryStrings[EntryType],
        EntryAnsiStrings[EntryType]);
      Entries[EntryType].Add(P);
    end;
  end;

  procedure ReadEntries(const EntryType: TEntryType; const Count: Integer;
    const Size: Integer; const MinVersionOfs, OnlyBelowVersionOfs: Integer);
  var
    I: Integer;
    P: Pointer;
  begin
    if Debugging then begin
      OriginalEntryIndexes[EntryType] := TList.Create;
      OriginalEntryIndexes[EntryType].Capacity := Count;
    end;
    Entries[EntryType].Capacity := Count;
    for I := 0 to Count-1 do begin
      P := AllocMem(Size);
      SECompressedBlockRead(Reader, P^, Size, EntryStrings[EntryType],
        EntryAnsiStrings[Entrytype]);
      if (MinVersionOfs = -1) or
         (InstallOnThisVersion(TSetupVersionData((@PByteArray(P)[MinVersionOfs])^),
          TSetupVersionData((@PByteArray(P)[OnlyBelowVersionOfs])^)) = irInstall) then begin
        Entries[EntryType].Add(P);
        if Debugging then
          OriginalEntryIndexes[EntryType].Add(Pointer(I));
      end
      else
        SEFreeRec(P, EntryStrings[EntryType], EntryAnsiStrings[EntryType]);
    end;
  end;

  function HandleInitPassword(const NeedPassword: Boolean): Boolean;
  { Handles InitPassword and returns the updated value of NeedPassword }
  { Also see Wizard.CheckPassword }
  var
    S: String;
    PasswordOk: Boolean;
  begin
    Result := NeedPassword;

    if NeedPassword and (InitPassword <> '') then begin
      PasswordOk := False;
      S := InitPassword;
      if shPassword in SetupHeader.Options then
        PasswordOk := TestPassword(S);
      if not PasswordOk and (CodeRunner <> nil) then
        PasswordOk := CodeRunner.RunBooleanFunction('CheckPassword', [S], False, PasswordOk);

      if PasswordOk then begin
        Result := False;
        if shEncryptionUsed in SetupHeader.Options then
          FileExtractor.CryptKey := S;
      end;
    end;
  end;

  procedure SetupInstallMode;
  begin
    if InitVerySilent then
      InstallMode := imVerySilent
    else if InitSilent then
      InstallMode := imSilent;

    if InstallMode <> imNormal then begin
      if InstallMode = imVerySilent then begin
        Application.ShowMainForm := False;
        SetTaskbarButtonVisibility(False);
      end;
      SetupHeader.Options := SetupHeader.Options - [shWindowVisible];
    end;
  end;

  function RecurseExternalGetSizeOfFiles(const DisableFsRedir: Boolean;
    const SearchBaseDir, SearchSubDir, SearchWildcard: String;
    const SourceIsWildcard: Boolean; const RecurseSubDirs: Boolean): Integer64;
  var
    SearchFullPath: String;
    H: THandle;
    FindData: TWin32FindData;
    I: Integer64;
  begin
    SearchFullPath := SearchBaseDir + SearchSubDir + SearchWildcard;
    Result.Hi := 0;
    Result.Lo := 0;

    H := FindFirstFileRedir(DisableFsRedir, SearchFullPath, FindData);
    if H <> INVALID_HANDLE_VALUE then begin
      repeat
        if FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then begin

          if SourceIsWildcard then
            if FindData.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN <> 0 then
              Continue;

          I.Hi := FindData.nFileSizeHigh;
          I.Lo := FindData.nFileSizeLow;
          Inc6464(Result, I);
        end;
      until not FindNextFile(H, FindData);
      Windows.FindClose(H);
    end;

    if RecurseSubDirs then begin
      H := FindFirstFileRedir(DisableFsRedir, SearchBaseDir + SearchSubDir + '*', FindData);
      if H <> INVALID_HANDLE_VALUE then begin
        try
          repeat
            if IsRecurseableDirectory(FindData) then begin
              I := RecurseExternalGetSizeOfFiles(DisableFsRedir, SearchBaseDir,
                SearchSubDir + FindData.cFileName + '\', SearchWildcard,
                SourceIsWildcard, RecurseSubDirs);
              Inc6464(Result, I);
            end;
          until not FindNextFile(H, FindData);
        finally
          Windows.FindClose(H);
        end;
      end;
    end;
  end;

var
  ParamName, ParamValue: String;
  StartParam: Integer;
  I: Integer;
  IsRespawnedProcess, EnableLogging, WantToSuppressMsgBoxes, Res: Boolean;
  DebugWndValue: HWND;
  LogFilename: String;
  SetupFilename: String;
  SetupFile: TFile;
  TestID: TSetupID;
  NameAndVersionMsg: String;
  NextAllowedLevel: Integer;
  LastShownComponentEntry, ComponentEntry: PSetupComponentEntry;
  MinimumTypeSpace: Integer64;
  SourceWildcard: String;
begin
  InitializeCommonVars;

  { Based on SetupLdr or not?
    Parameters for launching SetupLdr-based installation are:
    /SL5="<handle to SetupLdr's notify window>,<setup 0 data offset>,
          <setup 1 data offset>,<original exe filename>"
  }
  SplitNewParamStr(1, ParamName, ParamValue);
  if CompareText(ParamName, '/SL5=') = 0 then begin
    StartParam := 2;
    SetupLdrMode := True;
    SetupNotifyWnd := ExtractLongWord(ParamValue);
    SetupNotifyWndPresent := True;
    SetupLdrOffset0 := ExtractLongWord(ParamValue);
    SetupLdrOffset1 := ExtractLongWord(ParamValue);
    SetupLdrOriginalFilename := ParamValue;
  end
  else begin
    StartParam := 1;
    SetupLdrOriginalFilename := NewParamStr(0);
  end;
  SourceDir := PathExtractDir(SetupLdrOriginalFilename);

  IsRespawnedProcess := False;
  EnableLogging := False;
  WantToSuppressMsgBoxes := False;
  DebugWndValue := 0;
  for I := StartParam to NewParamCount do begin
    SplitNewParamStr(I, ParamName, ParamValue);
    if CompareText(ParamName, '/Log') = 0 then begin
      EnableLogging := True;
      LogFilename := '';
    end
    else
    if CompareText(ParamName, '/Log=') = 0 then begin
      EnableLogging := True;
      LogFilename := ParamValue;
    end
    else
    if CompareText(ParamName, '/Silent') = 0 then
      InitSilent := True
    else
    if CompareText(ParamName, '/VerySilent') = 0 then
      InitVerySilent := True
    else
    if CompareText(ParamName, '/NoRestart') = 0 then
      InitNoRestart := True
    else
    if CompareText(ParamName, '/NoCloseApplications') = 0 then
      InitNoCloseApplications := True
    else
    if CompareText(ParamName, '/NoRestartApplications') = 0 then
      InitNoRestartApplications := True
    else
    if CompareText(ParamName, '/NoIcons') = 0 then
      InitNoIcons := True
    else
    if CompareText(ParamName, '/NoCancel') = 0 then
      InitNoCancel := True
    else
    if CompareText(ParamName, '/Lang=') = 0 then
      InitLang := ParamValue
    else
    if CompareText(ParamName, '/Type=') = 0 then
      InitSetupType := ParamValue
    else
    if CompareText(ParamName, '/Components=') = 0 then begin
      InitComponentsSpecified := True;
      SetStringsFromCommaString(InitComponents, SlashesToBackslashes(ParamValue));
    end
    else
    if CompareText(ParamName, '/Tasks=') = 0 then begin
      InitDeselectAllTasks := True;
      SetStringsFromCommaString(InitTasks, SlashesToBackslashes(ParamValue));
    end
    else
    if CompareText(ParamName, '/MergeTasks=') = 0 then begin
      InitDeselectAllTasks := False;
      SetStringsFromCommaString(InitTasks, SlashesToBackslashes(ParamValue));
    end
    else
    if CompareText(ParamName, '/LoadInf=') = 0 then
      InitLoadInf := PathExpand(ParamValue)
    else
    if CompareText(ParamName, '/SaveInf=') = 0 then
      InitSaveInf := PathExpand(ParamValue)
    else
    if CompareText(ParamName, '/DIR=') = 0 then
      InitDir := ParamValue
    else
    if CompareText(ParamName, '/GROUP=') = 0 then
      InitProgramGroup := ParamValue
    else
    if CompareText(ParamName, '/Password=') = 0 then
      InitPassword := ParamValue
    else
    if CompareText(ParamName, '/RestartExitCode=') = 0 then
      InitRestartExitCode := StrToIntDef(ParamValue, 0)
    else
    if CompareText(ParamName, '/SuppressMsgBoxes') = 0 then
      WantToSuppressMsgBoxes := True
    else
    if CompareText(ParamName, '/DETACHEDMSG') = 0 then  { for debugging }
      DetachedUninstMsgFile := True
    else
    if CompareText(ParamName, '/SPAWNWND=') = 0 then begin
      IsRespawnedProcess := True;
      InitializeSpawnClient(StrToInt(ParamValue));
    end
    else
    if CompareText(ParamName, '/NOTIFYWND=') = 0 then begin
      { /NOTIFYWND= takes precedence over any previously set SetupNotifyWnd }
      SetupNotifyWnd := StrToInt(ParamValue);
      SetupNotifyWndPresent := True;
    end
    else
    if CompareText(ParamName, '/DebugSpawnServer') = 0 then  { for debugging }
      EnterSpawnServerDebugMode  { does not return }
    else
    if CompareText(ParamName, '/DEBUGWND=') = 0 then
      DebugWndValue := StrToInt(ParamValue);
  end;

  if InitLoadInf <> '' then
    LoadInf(InitLoadInf);

  if WantToSuppressMsgBoxes and (InitSilent or InitVerySilent) then
    InitSuppressMsgBoxes := True;

  { Assign some default messages that may be used before the messages are read }
  SetupMessages[msgSetupFileMissing] := SSetupFileMissing;
  SetupMessages[msgSetupFileCorrupt] := SSetupFileCorrupt;
  SetupMessages[msgSetupFileCorruptOrWrongVer] := SSetupFileCorruptOrWrongVer;

  { Read setup-0.bin, or from EXE }
  if not SetupLdrMode then begin
    SetupFilename := PathChangeExt(SetupLdrOriginalFilename, '') + '-0.bin';
    if not NewFileExists(SetupFilename) then
      AbortInitFmt1(msgSetupFileMissing, PathExtractName(SetupFilename));
  end
  else
    SetupFilename := SetupLdrOriginalFilename;
  SetupFile := TFile.Create(SetupFilename, fdOpenExisting, faRead, fsRead);
  try
    SetupFile.Seek(SetupLdrOffset0);
    if SetupFile.Read(TestID, SizeOf(TestID)) <> SizeOf(TestID) then
      AbortInit(msgSetupFileCorruptOrWrongVer);
    if TestID <> SetupID then
      AbortInit(msgSetupFileCorruptOrWrongVer);
    try
      Reader := TCompressedBlockReader.Create(SetupFile, TLZMA1Decompressor);
      try
        { Header }
        SECompressedBlockRead(Reader, SetupHeader, SizeOf(SetupHeader),
          SetupHeaderStrings, SetupHeaderAnsiStrings);
        { Language entries }
        ReadEntriesWithoutVersion(seLanguage, SetupHeader.NumLanguageEntries,
          SizeOf(TSetupLanguageEntry));
        { CustomMessage entries }
        ReadEntriesWithoutVersion(seCustomMessage, SetupHeader.NumCustomMessageEntries,
          SizeOf(TSetupCustomMessageEntry));
        { Permission entries }
        ReadEntriesWithoutVersion(sePermission, SetupHeader.NumPermissionEntries,
          SizeOf(TSetupPermissionEntry));
        { Type entries }
        ReadEntries(seType, SetupHeader.NumTypeEntries, SizeOf(TSetupTypeEntry),
          Integer(@PSetupTypeEntry(nil).MinVersion),
          Integer(@PSetupTypeEntry(nil).OnlyBelowVersion));

        ActivateDefaultLanguage;

        { Start a new, elevated Setup(Ldr) process if needed }
        if not IsRespawnedProcess and
           NeedToRespawnSelfElevated(not (SetupHeader.PrivilegesRequired in [prNone, prLowest]),
             SetupHeader.PrivilegesRequired <> prLowest) then begin
          FreeAndNil(Reader);
          FreeAndNil(SetupFile);
          RespawnSetupElevated(GetCmdTailEx(StartParam));
          { Note: RespawnSetupElevated does not return; it either calls Halt
            or raises an exception. }
        end;

        { Application.Handle is now known to be the main window. Set the shutdown block reason. }
        ShutdownBlockReasonCreate(Application.Handle, SetupMessages[msgWizardInstalling]);

        { Initialize debug client }
        if DebugWndValue <> 0 then
          SetDebugWnd(DebugWndValue, False);

        { Initialize logging }
        if EnableLogging or (shSetupLogging in SetupHeader.Options) then begin
          try
            if LogFilename = '' then
              StartLogging('Setup')
            else
              StartLoggingWithFixedFilename(LogFilename);
          except
            on E: Exception do begin
              E.Message := 'Error creating log file:' + SNewLine2 + E.Message;
              raise;
            end;
          end;
        end;
        Log('Setup version: ' + SetupTitle + ' version ' + SetupVersion);
        Log('Original Setup EXE: ' + SetupLdrOriginalFilename);
        Log('Setup command line: ' + GetCmdTail);
        LogWindowsVersion;

        NeedPassword := shPassword in SetupHeader.Options;
        NeedSerial := False;
        NeedsRestart := shAlwaysRestart in SetupHeader.Options;

        { Component entries }
        ReadEntries(seComponent, SetupHeader.NumComponentEntries, SizeOf(TSetupComponentEntry),
          -1, -1);
        { Task entries }
        ReadEntries(seTask, SetupHeader.NumTaskEntries, SizeOf(TSetupTaskEntry),
          -1, -1);
        { Dir entries }
        ReadEntries(seDir, SetupHeader.NumDirEntries, SizeOf(TSetupDirEntry),
          Integer(@PSetupDirEntry(nil).MinVersion),
          Integer(@PSetupDirEntry(nil).OnlyBelowVersion));
        { File entries }
        ReadEntries(seFile, SetupHeader.NumFileEntries, SizeOf(TSetupFileEntry),
          Integer(@PSetupFileEntry(nil).MinVersion),
          Integer(@PSetupFileEntry(nil).OnlyBelowVersion));
        { Icon entries }
        ReadEntries(seIcon, SetupHeader.NumIconEntries, SizeOf(TSetupIconEntry),
          Integer(@PSetupIconEntry(nil).MinVersion),
          Integer(@PSetupIconEntry(nil).OnlyBelowVersion));
        { INI entries }
        ReadEntries(seIni, SetupHeader.NumIniEntries, SizeOf(TSetupIniEntry),
          Integer(@PSetupIniEntry(nil).MinVersion),
          Integer(@PSetupIniEntry(nil).OnlyBelowVersion));
        { Registry entries }
        ReadEntries(seRegistry, SetupHeader.NumRegistryEntries, SizeOf(TSetupRegistryEntry),
          Integer(@PSetupRegistryEntry(nil).MinVersion),
          Integer(@PSetupRegistryEntry(nil).OnlyBelowVersion));
        { InstallDelete entries }
        ReadEntries(seInstallDelete, SetupHeader.NumInstallDeleteEntries, SizeOf(TSetupDeleteEntry),
          Integer(@PSetupDeleteEntry(nil).MinVersion),
          Integer(@PSetupDeleteEntry(nil).OnlyBelowVersion));
        { UninstallDelete entries }
        ReadEntries(seUninstallDelete, SetupHeader.NumUninstallDeleteEntries, SizeOf(TSetupDeleteEntry),
          Integer(@PSetupDeleteEntry(nil).MinVersion),
          Integer(@PSetupDeleteEntry(nil).OnlyBelowVersion));
        { Run entries }
        ReadEntries(seRun, SetupHeader.NumRunEntries, SizeOf(TSetupRunEntry),
          Integer(@PSetupRunEntry(nil).MinVersion),
          Integer(@PSetupRunEntry(nil).OnlyBelowVersion));
        { UninstallRun entries }
        ReadEntries(seUninstallRun, SetupHeader.NumUninstallRunEntries, SizeOf(TSetupRunEntry),
          Integer(@PSetupRunEntry(nil).MinVersion),
          Integer(@PSetupRunEntry(nil).OnlyBelowVersion));

        { Wizard image }
        ReadWizardImage(WizardImage, Reader);
        ReadWizardImage(WizardSmallImage, Reader);
        { Decompressor DLL }
        DecompressorDLL := nil;
        if SetupHeader.CompressMethod in [cmZip, cmBzip] then begin
          DecompressorDLL := TMemoryStream.Create;
          ReadFileIntoStream(DecompressorDLL, Reader);
        end;
        { Decryption DLL }
        DecryptDLL := nil;
        if shEncryptionUsed in SetupHeader.Options then begin
          DecryptDLL := TMemoryStream.Create;
          ReadFileIntoStream(DecryptDLL, Reader);
        end;
      finally
        Reader.Free;
      end;
      Reader := TCompressedBlockReader.Create(SetupFile, TLZMA1Decompressor);
      try
        { File location entries }
        ReadEntriesWithoutVersion(seFileLocation, SetupHeader.NumFileLocationEntries,
          SizeOf(TSetupFileLocationEntry));
      finally
        Reader.Free;
      end;
    except
      on ECompressDataError do
        AbortInit(msgSetupFileCorrupt);
    end;
  finally
    SetupFile.Free;
  end;

  { Show "Select Language" dialog if necessary }
  if ShowLanguageDialog and (Entries[seLanguage].Count > 1) and
     not InitSilent and not InitVerySilent then begin
    if not AskForLanguage then
      Abort;
  end;

  { Check processor architecture }
  if (SetupHeader.ArchitecturesAllowed <> []) and
     not(ProcessorArchitecture in SetupHeader.ArchitecturesAllowed) then
    AbortInitFmt1(msgOnlyOnTheseArchitectures, ArchitecturesToStr(SetupHeader.ArchitecturesAllowed));

  { Check Windows version }
  case InstallOnThisVersion(SetupHeader.MinVersion, SetupHeader.OnlyBelowVersion) of
    irInstall: ;
    irServicePackTooLow:
      AbortInitServicePackRequired(SetupHeader.MinVersion.NTServicePack);
  else
    AbortInit(msgWindowsVersionNotSupported);
  end;

  { Set Is64BitInstallMode if we're on Win64 and the processor architecture is
    one on which a "64-bit mode" install should be performed }
  if ProcessorArchitecture in SetupHeader.ArchitecturesInstallIn64BitMode then begin
    if not IsWin64 then begin
      { A 64-bit processor was detected and 64-bit install mode was requested,
        but IsWin64 is False, indicating required WOW64 APIs are not present }
      AbortInitFmt1(msgMissingWOW64APIs, '1');
    end;
    Initialize64BitInstallMode(True);
  end
  else
    Initialize64BitInstallMode(False);

  { Check if the user lacks the required privileges }
  case SetupHeader.PrivilegesRequired of
    prPowerUser:
      if not IsPowerUserOrAdmin then AbortInit(msgPowerUserPrivilegesRequired);
    prAdmin:
      if not IsAdmin then AbortInit(msgAdminPrivilegesRequired);
  end;

  { Init main constants, not depending on _shfoldr.dll }
  InitMainNonSHFolderConsts;

  { Create temporary directory and extract 64-bit helper EXE if necessary }
  CreateTempInstallDir;

  { Extract "_shfoldr.dll" to TempInstallDir, and load it }
  LoadSHFolderDLL;

  { Extract "_isdecmp.dll" to TempInstallDir, and load it }
  if SetupHeader.CompressMethod in [cmZip, cmBzip] then
    LoadDecompressorDLL;

  { Extract "_iscrypt.dll" to TempInstallDir, and load it }
  if shEncryptionUsed in SetupHeader.Options then
    LoadDecryptDLL;

  { Start RestartManager session }
  if (shCloseApplications in SetupHeader.Options) and not InitNoCloseApplications then begin
    InitRestartManagerLibrary;
    { Note from Old New Thing: "The RmStartSession function doesn't properly
      null-terminate the session key <...>. To work around this bug, we pre-fill
      the buffer with null characters <...>." Our key is pre-filled too since
      it's global. }
    if UseRestartManager and (RmStartSession(@RmSessionHandle, 0, RmSessionKey) = ERROR_SUCCESS) then begin
      RmSessionStarted := True;
      SetStringsFromCommaString(CloseApplicationsFilterList, SetupHeader.CloseApplicationsFilter);
    end;
  end;

  { Set install mode }
  SetupInstallMode;

  { Load and initialize code }
  if SetupHeader.CompiledCodeText <> '' then begin
    CodeRunner := TScriptRunner.Create();
    try
      CodeRunner.OnDllImport := CodeRunnerOnDllImport;
      CodeRunner.OnDebug := CodeRunnerOnDebug;
      CodeRunner.OnDebugIntermediate := CodeRunnerOnDebugIntermediate;
      CodeRunner.OnException := CodeRunnerOnException;
      CodeRunner.LoadScript(SetupHeader.CompiledCodeText, DebugClientCompiledCodeDebugInfo);
      if not NeedPassword then
        NeedPassword := CodeRunner.FunctionExists('CheckPassword');
      NeedPassword := HandleInitPassword(NeedPassword);
      if not NeedSerial then
        NeedSerial := CodeRunner.FunctionExists('CheckSerial');
    except
      { Don't let DeinitSetup see a partially-initialized CodeRunner }
      FreeAndNil(CodeRunner);
      raise;
    end;
    try
      Res := CodeRunner.RunBooleanFunction('InitializeSetup', [''], False, True);
    except
      Log('InitializeSetup raised an exception (fatal).');
      raise;
    end;
    if not Res then begin
      Log('InitializeSetup returned False; aborting.');
      Abort;
    end;
  end
  else
    NeedPassword := HandleInitPassword(NeedPassword);

  { Expand AppName, AppVerName, and AppCopyright now since they're used often,
    especially by the background window painting. }
  ExpandedAppName := ExpandConst(SetupHeader.AppName);
  if SetupHeader.AppVerName <> '' then
    ExpandedAppVerName := ExpandConst(SetupHeader.AppVerName)
  else begin
    if not GetCustomMessageValue('NameAndVersion', NameAndVersionMsg) then
      NameAndVersionMsg := '%1 %2';  { just in case }
    ExpandedAppVerName := FmtMessage(PChar(NameAndVersionMsg),
      [ExpandedAppName, ExpandConst(SetupHeader.AppVersion)]);
  end;
  ExpandedAppCopyright := ExpandConst(SetupHeader.AppCopyright);
  ExpandedAppMutex := ExpandConst(SetupHeader.AppMutex);

  { Update the shutdown block reason now that we have ExpandedAppName. }
  ShutdownBlockReasonCreate(Application.Handle,
    FmtSetupMessage1(msgShutdownBlockReasonInstallingApp, ExpandedAppName));

  { Check if app is running }
  while CheckForMutexes(ExpandedAppMutex) do
    if LoggedMsgBox(FmtSetupMessage1(msgSetupAppRunningError, ExpandedAppName),
       SetupMessages[msgSetupAppTitle], mbError, MB_OKCANCEL, True, IDCANCEL) <> IDOK then
      Abort;

  { Remove types that fail their 'languages' or 'check'. Can't do this earlier
    because the InitializeSetup call above can't be done earlier. }
  for I := 0 to Entries[seType].Count-1 do begin
    if not ShouldProcessEntry(nil, nil, '', '', PSetupTypeEntry(Entries[seType][I]).Languages, PSetupTypeEntry(Entries[seType][I]).Check) then begin
      SEFreeRec(Entries[seType][I], EntryStrings[seType], EntryAnsiStrings[seType]);
      { Don't delete it yet so that the entries can be processed sequentially }
      Entries[seType][I] := nil;
    end;
  end;
  { Delete the nil-ed items now }
  Entries[seType].Pack();

  { Remove components }
  NextAllowedLevel := 0;
  LastShownComponentEntry := nil;
  for I := 0 to Entries[seComponent].Count-1 do begin
    ComponentEntry := PSetupComponentEntry(Entries[seComponent][I]);
    if (ComponentEntry.Level <= NextAllowedLevel) and
       (InstallOnThisVersion(ComponentEntry.MinVersion, ComponentEntry.OnlyBelowVersion) = irInstall) and
       ShouldProcessEntry(nil, nil, '', '', ComponentEntry.Languages, ComponentEntry.Check) then begin
      NextAllowedLevel := ComponentEntry.Level + 1;
      LastShownComponentEntry := ComponentEntry;
    end
    else begin
      { Not showing }
      if Assigned(LastShownComponentEntry) and
         (ComponentEntry.Level = LastShownComponentEntry.Level) and
         (CompareText(ComponentEntry.Name, LastShownComponentEntry.Name) = 0) then begin
        { It's a duplicate of the last shown item. Leave NextAllowedLevel
          alone, so that any child items that follow can attach to the last
          shown item. }
      end
      else begin
        { Not a duplicate of the last shown item, so the next item must be
          at the same level or less }
        if NextAllowedLevel > ComponentEntry.Level then
          NextAllowedLevel := ComponentEntry.Level;
        { Clear LastShownComponentEntry so that no subsequent item can be
          considered a duplicate of it. Needed in this case:
            foo         (shown)
            foo\childA  (not shown)
            foo         (not shown)
            foo\childB
          "foo\childB" should be hidden, not made a child of "foo" #1. }
        LastShownComponentEntry := nil;
      end;
      Entries[seComponent][I] := nil;
      SEFreeRec(ComponentEntry, EntryStrings[seComponent], EntryAnsiStrings[seComponent]);
    end;
  end;
  Entries[seComponent].Pack();

  { Set misc. variables }
  HasCustomType := False;
  for I := 0 to Entries[seType].Count-1 do begin
    if toIsCustom in PSetupTypeEntry(Entries[seType][I]).Options then begin
      HasCustomType := True;
      Break;
    end;
  end;

  HasComponents := Entries[seComponent].Count <> 0;

  HasIcons := Entries[seIcon].Count <> 0;

  HasTasks := Entries[seTask].Count <> 0;

  { Calculate minimum disk space. If there are setup types, find the smallest
    type and add the size of all files that don't belong to any component. Otherwise
    calculate minimum disk space by adding all of the file's sizes. Also for each
    "external" file, check the file size now, and store it the ExternalSize field
    of the TSetupFileEntry record, except if an ExternalSize was specified by the
    script. }

  MinimumSpace := SetupHeader.ExtraDiskSpaceRequired;

  for I := 0 to Entries[seFile].Count-1 do begin
    with PSetupFileEntry(Entries[seFile][I])^ do begin
      if LocationEntry <> -1 then begin { not an "external" file }
        if Components = '' then { no types or a file that doesn't belong to any component }
          if (Tasks = '') and (Check = '') then {don't count tasks and scripted entries}
            Inc6464(MinimumSpace, PSetupFileLocationEntry(Entries[seFileLocation][LocationEntry])^.OriginalSize)
      end else begin
        if not(foExternalSizePreset in Options) then begin
          try
            if FileType <> ftUserFile then
              SourceWildcard := NewParamStr(0)
            else
              SourceWildcard := ExpandConst(SourceFilename);
            ExternalSize := RecurseExternalGetSizeOfFiles(
              ShouldDisableFsRedirForFileEntry(PSetupFileEntry(Entries[seFile][I])),
              PathExtractPath(SourceWildcard),
              '', PathExtractName(SourceWildcard), IsWildcard(SourceWildcard),
              foRecurseSubDirsExternal in Options);
          except
            { Ignore exceptions. One notable exception we want to ignore is
              the one about "app" not being initialized. }
          end;
        end;
        if Components = '' then { no types or a file that doesn't belong to any component }
          if (Tasks = '') and (Check = '') then {don't count tasks or scripted entries}
            Inc6464(MinimumSpace, ExternalSize);
      end;
    end;
  end;

  for I := 0 to Entries[seComponent].Count-1 do
    with PSetupComponentEntry(Entries[seComponent][I])^ do
      Size := GetSizeOfComponent(Name, ExtraDiskSpaceRequired);

  if Entries[seType].Count > 0 then begin
    for I := 0 to Entries[seType].Count-1 do begin
      with PSetupTypeEntry(Entries[seType][I])^ do begin
        Size := GetSizeOfType(Name, toIsCustom in Options);
        if (I = 0) or (Compare64(Size, MinimumTypeSpace) < 0) then
          MinimumTypeSpace := Size;
      end;
    end;
    Inc6464(MinimumSpace, MinimumTypeSpace);
  end;
end;

procedure DeinitSetup(const AllowCustomSetupExitCode: Boolean);
var
  I: Integer;
begin
  Log('Deinitializing Setup.');

  if Assigned(CodeRunner) then begin
    if AllowCustomSetupExitCode then begin
      try
        SetupExitCode := CodeRunner.RunIntegerFunction('GetCustomSetupExitCode',
          [''], False, SetupExitCode);
      except
        Log('GetCustomSetupExitCode raised an exception.');
        Application.HandleException(nil);
      end;
    end;
    try
      CodeRunner.RunProcedure('DeinitializeSetup', [''], False);
    except
      Log('DeinitializeSetup raised an exception.');
      Application.HandleException(nil);
    end;
    FreeAndNil(CodeRunner);
  end;

  for I := 0 to DeleteFilesAfterInstallList.Count-1 do
    DeleteFileRedir(DeleteFilesAfterInstallList.Objects[I] <> nil,
      DeleteFilesAfterInstallList[I]);
  DeleteFilesAfterInstallList.Clear;
  for I := DeleteDirsAfterInstallList.Count-1 downto 0 do
    RemoveDirectoryRedir(DeleteDirsAfterInstallList.Objects[I] <> nil,
      DeleteDirsAfterInstallList[I]);
  DeleteDirsAfterInstallList.Clear;

  FreeFileExtractor;

  { End RestartManager session }
  if RmSessionStarted then
    RmEndSession(RmSessionHandle);

  { Free the _iscrypt.dll handle }
  if DecryptDLLHandle <> 0 then
    FreeLibrary(DecryptDLLHandle);

  { Free the _isdecmp.dll handle }
  if DecompressorDLLHandle <> 0 then
    FreeLibrary(DecompressorDLLHandle);

  { Free the shfolder.dll handles }
  UnloadSHFolderDLL;

  { Remove TempInstallDir, stopping the 64-bit helper first if necessary }
  RemoveTempInstallDir;

  { An attempt to restart while debugging is most likely an accident;
    don't allow it }
  if RestartSystem and Debugging then begin
    Log('Not restarting Windows because Setup is being run from the debugger.');
    RestartSystem := False;
  end;

  EndDebug;

  ShutdownBlockReasonDestroy(Application.Handle);

  if RestartSystem then begin
    Log('Restarting Windows.');
    if SetupNotifyWndPresent then begin
      { Send a special message back to the first instance telling it to
        restart the system after Setup returns }
      SendNotifyMessage(SetupNotifyWnd, WM_USER + 150, 10000, 0);
    end
    else begin
      { There is no other instance, so initiate the restart ourself.
        Note: Depending on the OS, this may not return if successful. }
      RestartComputerFromThisProcess;
    end;
  end;
end;

function BlendRGB(const Color1, Color2: TColor; const Blend: Integer): TColor;
{ Blends Color1 and Color2. Blend must be between 0 and 255; 0 = all Color1,
  255 = all Color2. }
type
  TColorBytes = array[0..3] of Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to 2 do
    TColorBytes(Result)[I] := Integer(TColorBytes(Color1)[I] +
      ((TColorBytes(Color2)[I] - TColorBytes(Color1)[I]) * Blend) div 255);
end;

function ExitSetupMsgBox: Boolean;
begin
  Result := LoggedMsgBox(SetupMessages[msgExitSetupMessage], SetupMessages[msgExitSetupTitle],
    mbConfirmation, MB_YESNO or MB_DEFBUTTON2, False, 0) = IDYES;
end;

procedure TerminateApp;
begin
  { Work around shell32 bug: Don't use PostQuitMessage/Application.Terminate
    here.
    When ShellExecute is called with the name of a folder, it internally
    creates a window used for DDE communication with Windows Explorer. After
    ShellExecute returns, this window eventually receives a posted WM_DDE_ACK
    message back from the DDE server (Windows Explorer), and in response, it
    tries to flush the queue of DDE messages by using a PeekMessage loop.
    Problem is, PeekMessage will return WM_QUIT messages posted with
    PostQuitMessage regardless of the message range specified, and the loop was
    not written with this in mind.
    In previous IS versions, this was causing our WM_QUIT message to be eaten
    if Application.Terminate was called very shortly after a shellexec [Run]
    entry was processed (e.g. if DisableFinishedPage=yes).
    A WM_QUIT message posted with PostMessage instead of PostQuitMessage will
    not be returned by a GetMessage/PeekMessage call with a message range that
    does not include WM_QUIT. }
  PostMessage(0, WM_QUIT, 0, 0);
end;


{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
var
  SystemMenu: HMenu;
begin
  inherited;

  InitializeFont;

  if shWindowVisible in SetupHeader.Options then begin
    { Should the main window not be sizable? }
    if not(shWindowShowCaption in SetupHeader.Options) then
      BorderStyle := bsNone
    else
    if not(shWindowResizable in SetupHeader.Options) then
      BorderStyle := bsSingle;

    { Make the main window full-screen. If the window is resizable, limit it
      to just the work area because on recent versions of Windows (e.g. 2000)
      full-screen resizable windows don't cover over the taskbar. }
    BoundsRect := GetRectOfPrimaryMonitor(BorderStyle = bsSizeable);
    { Before maximizing the window, ensure Handle is created now so the correct
      'restored' position is saved properly }
    HandleNeeded;

    { Maximize the window so that the taskbar is still accessible }
    if shWindowStartMaximized in SetupHeader.Options then
      WindowState := wsMaximized;
  end
  else begin
    Application.ShowMainForm := False;
  end;

  Caption := FmtSetupMessage1(msgSetupWindowTitle, ExpandedAppName);

  { Append the 'About Setup' item to the system menu }
  SystemMenu := GetSystemMenu(Handle, False);
  AppendMenu(SystemMenu, MF_SEPARATOR, 0, nil);
  AppendMenu(SystemMenu, MF_STRING, 9999, PChar(SetupMessages[msgAboutSetupMenuItem]));

  Application.HookMainWindow(MainWindowHook);

  if Application.ShowMainForm then
    { Show this form now, so that the focus stays on the wizard form that
      InitializeWizard (called in the .dpr) shows }
    Visible := True;
end;

destructor TMainForm.Destroy;
begin
  Application.UnhookMainWindow(MainWindowHook);
  inherited;
end;

procedure TMainForm.WMSysCommand(var Message: TWMSysCommand);
begin
  if Message.CmdType = 9999 then
    ShowAboutBox
  else
    inherited;
end;

procedure TMainForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  { Since the form paints its entire client area in FormPaint, there is
    no need for the VCL to ever erase the client area with the brush color.
    Doing so only slows it down, so this message handler disables that default
    behavior. }
  Message.Result := 0;
end;

procedure TMainForm.FormPaint(Sender: TObject);
var
  C1, C2: TColor;
  CS: TPoint;
  Z: Integer;
  DrawTextFlags: UINT;
  R, R2: TRect;
begin
  with Canvas do begin
    { Draw the blue background }
    if SetupHeader.BackColor = SetupHeader.BackColor2 then begin
      Brush.Color := SetupHeader.BackColor;
      FillRect(ClientRect);
    end
    else begin
      C1 := ColorToRGB(SetupHeader.BackColor);
      C2 := ColorToRGB(SetupHeader.BackColor2);
      CS := ClientRect.BottomRight;
      for Z := 0 to 255 do begin
        Brush.Color := BlendRGB(C1, C2, Z);
        if not(shBackColorHorizontal in SetupHeader.Options) then
          FillRect(Rect(0, MulDiv(CS.Y, Z, 255), CS.X, MulDiv(CS.Y, Z+1, 255)))
        else
          FillRect(Rect(MulDiv(CS.X, Z, 255), 0, MulDiv(CS.X, Z+1, 255), CS.Y));
      end;
    end;

    { Draw the application name and copyright }
    SetBkMode(Handle, TRANSPARENT);

    DrawTextFlags := DT_WORDBREAK or DT_NOPREFIX or DT_NOCLIP;
    if RightToLeft then
      DrawTextFlags := DrawTextFlags or (DT_RIGHT or DT_RTLREADING);
    SetFontNameSize(Font, LangOptions.TitleFontName,
      LangOptions.TitleFontSize, 'Arial', 29);
    if IsMultiByteString(AnsiString(ExpandedAppName)) then
      { Don't use italics on Japanese characters }
      Font.Style := [fsBold]
    else
      Font.Style := [fsBold, fsItalic];
    R := ClientRect;
    InflateRect(R, -8, -8);
    R2 := R;
    if RightToLeft then
      OffsetRect(R2, -4, 4)
    else
      OffsetRect(R2, 4, 4);
    Font.Color := clBlack;
    DrawText(Handle, PChar(ExpandedAppName), -1, R2, DrawTextFlags);
    Font.Color := clWhite;
    DrawText(Handle, PChar(ExpandedAppName), -1, R, DrawTextFlags);

    DrawTextFlags := DrawTextFlags xor DT_RIGHT;
    SetFontNameSize(Font, LangOptions.CopyrightFontName,
      LangOptions.CopyrightFontSize, 'Arial', 8);
    Font.Style := [];
    R := ClientRect;
    InflateRect(R, -6, -6);
    R2 := R;
    DrawText(Handle, PChar(ExpandedAppCopyright), -1, R2, DrawTextFlags or
      DT_CALCRECT);
    R.Top := R.Bottom - (R2.Bottom - R2.Top);
    R2 := R;
    if RightToLeft then
      OffsetRect(R2, -1, 1)
    else
      OffsetRect(R2, 1, 1);
    Font.Color := clBlack;
    DrawText(Handle, PChar(ExpandedAppCopyright), -1, R2, DrawTextFlags);
    Font.Color := clWhite;
    DrawText(Handle, PChar(ExpandedAppCopyright), -1, R, DrawTextFlags);
  end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  { Needs to redraw the background whenever the form is resized }
  Repaint;
end;

procedure TMainForm.ShowAboutBox;
var
  S: String;
begin
  { Removing the About box or modifying any existing text inside it is a
    violation of the Inno Setup license agreement; see LICENSE.TXT.
    However, adding additional lines to the end of the About box is
    permitted. }
  S := SetupTitle + ' version ' + SetupVersion + SNewLine;
  if SetupTitle <> 'Inno Setup' then
    S := S + (SNewLine + 'Based on Inno Setup' + SNewLine);
  S := S + ('Copyright (C) 1997-2012 Jordan Russell' + SNewLine +
    'Portions Copyright (C) 2000-2012 Martijn Laan' + SNewLine +
    'All rights reserved.' + SNewLine2 +
    'Inno Setup home page:' + SNewLine +
    'http://www.innosetup.com/');
  S := S + SNewLine2 + 'RemObjects Pascal Script home page:' + SNewLine +
    'http://www.remobjects.com/ps';
  if SetupMessages[msgAboutSetupNote] <> '' then
    S := S + SNewLine2 + SetupMessages[msgAboutSetupNote];
  if SetupMessages[msgTranslatorNote] <> '' then
    S := S + SNewLine2 + SetupMessages[msgTranslatorNote];
{$IFDEF UNICODE}
  StringChangeEx(S, '(C)', #$00A9, True);
{$ENDIF}
  LoggedMsgBox(S, SetupMessages[msgAboutSetupTitle], mbInformation, MB_OK, False, 0);
end;

class procedure TMainForm.ShowExceptionMsg(const S: String);
begin
  Log('Exception message:');
  LoggedAppMessageBox(PChar(S), PChar(Application.Title), MB_OK or MB_ICONSTOP, True, IDOK);
end;

class procedure TMainForm.ShowException(Sender: TObject; E: Exception);
begin
  ShowExceptionMsg(AddPeriod(E.Message));
end;

procedure ProcessMessagesProc; far;
begin
  Application.ProcessMessages;
end;

procedure TMainForm.SetStep(const AStep: TSetupStep; const HandleExceptions: Boolean);
begin
  CurStep := AStep;
  if CodeRunner <> nil then begin
    try
      CodeRunner.RunProcedure('CurStepChanged', [Ord(CurStep)], False);
    except
      if HandleExceptions then begin
        Log('CurStepChanged raised an exception.');
        Application.HandleException(Self);
      end
      else begin
        Log('CurStepChanged raised an exception (fatal).');
        raise;
      end;
    end;
  end;
end;

procedure TMainForm.InitializeWizard;
begin
  WizardForm := TWizardForm.Create(Application);
  if CodeRunner <> nil then begin
    try
      CodeRunner.RunProcedure('InitializeWizard', [''], False);
    except
      Log('InitializeWizard raised an exception (fatal).');
      raise;
    end;
  end;
  WizardForm.FlipControlsIfNeeded;
  WizardForm.SetCurPage(wpWelcome);
  if InstallMode = imNormal then begin
    WizardForm.ClickToStartPage; { this won't go past wpReady  }
    SetActiveWindow(Application.Handle);  { ensure taskbar button is selected }
    WizardForm.Show;
  end
  else
    WizardForm.ClickThroughPages;
end;

function ShouldDisableFsRedirForRunEntry(const RunEntry: PSetupRunEntry): Boolean;
begin
  Result := InstallDefaultDisableFsRedir;
  if roRun32Bit in RunEntry.Options then
    Result := False;
  if roRun64Bit in RunEntry.Options then begin
    if not IsWin64 then
      InternalError('Cannot run files in 64-bit locations on this version of Windows');
    Result := True;
  end;
end;

procedure ProcessRunEntry(const RunEntry: PSetupRunEntry);
var
  RunAsOriginalUser: Boolean;
  ExpandedFilename, ExpandedParameters: String;
  Wait: TExecWait;
  DisableFsRedir: Boolean;
  ErrorCode: Integer;
begin
  try
    Log('-- Run entry --');
    RunAsOriginalUser := (roRunAsOriginalUser in RunEntry.Options);
    if RunAsOriginalUser then
      Log('Run as: Original user')
    else
      Log('Run as: Current user');
    if not(roShellExec in RunEntry.Options) then
      Log('Type: Exec')
    else
      Log('Type: ShellExec');
    ExpandedFilename := ExpandConst(RunEntry.Name);
    Log('Filename: ' + ExpandedFilename);
    ExpandedParameters := ExpandConst(RunEntry.Parameters);
    if ExpandedParameters <> '' then
      Log('Parameters: ' + ExpandedParameters);

    Wait := ewWaitUntilTerminated;
    case RunEntry.Wait of
      rwNoWait: Wait := ewNoWait;
      rwWaitUntilIdle: Wait := ewWaitUntilIdle;
    end;

    if not(roShellExec in RunEntry.Options) then begin
      DisableFsRedir := ShouldDisableFsRedirForRunEntry(RunEntry);
      if not(roSkipIfDoesntExist in RunEntry.Options) or
         NewFileExistsRedir(DisableFsRedir, ExpandedFilename) then begin
        if not InstExecEx(RunAsOriginalUser, DisableFsRedir, ExpandedFilename,
           ExpandedParameters, ExpandConst(RunEntry.WorkingDir),
           Wait, RunEntry.ShowCmd, ProcessMessagesProc, ErrorCode) then
          raise Exception.Create(FmtSetupMessage1(msgErrorExecutingProgram, ExpandedFilename) +
            SNewLine2 + FmtSetupMessage(msgErrorFunctionFailedWithMessage,
            ['CreateProcess', IntToStr(ErrorCode), Win32ErrorString(ErrorCode)]));
        if Wait = ewWaitUntilTerminated then
          Log(Format('Process exit code: %u', [ErrorCode]));
      end
      else
        Log('File doesn''t exist. Skipping.');
    end
    else begin
      if not(roSkipIfDoesntExist in RunEntry.Options) or FileOrDirExists(ExpandedFilename) then begin
        if not InstShellExecEx(RunAsOriginalUser, ExpandConst(RunEntry.Verb),
           ExpandedFilename, ExpandedParameters, ExpandConst(RunEntry.WorkingDir),
           Wait, RunEntry.ShowCmd, ProcessMessagesProc, ErrorCode) then
          raise Exception.Create(FmtSetupMessage1(msgErrorExecutingProgram, ExpandedFilename) +
            SNewLine2 + FmtSetupMessage(msgErrorFunctionFailedWithMessage,
            ['ShellExecuteEx', IntToStr(ErrorCode), Win32ErrorString(ErrorCode)]));
      end
      else
        Log('File/directory doesn''t exist. Skipping.');
    end;
  except
    Application.HandleException(nil);
  end;
end;

function TMainForm.Install: Boolean;

  procedure ProcessRunEntries;
  var
    CheckIfRestartNeeded: Boolean;
    ChecksumBefore, ChecksumAfter: TMD5Digest;
    WindowDisabler: TWindowDisabler;
    I: Integer;
    RunEntry: PSetupRunEntry;
  begin
    if Entries[seRun].Count <> 0 then begin
      CheckIfRestartNeeded := (shRestartIfNeededByRun in SetupHeader.Options) and
        not NeedsRestart;
      if CheckIfRestartNeeded then
        ChecksumBefore := MakePendingFileRenameOperationsChecksum;
      WindowDisabler := nil;
      try
        for I := 0 to Entries[seRun].Count-1 do begin
          RunEntry := PSetupRunEntry(Entries[seRun][I]);
          if not(roPostInstall in RunEntry.Options) and
             ShouldProcessRunEntry(WizardComponents, WizardTasks, RunEntry) then begin
            { Disable windows during execution of [Run] entries so that a nice
              "beep" is produced if the user tries clicking on WizardForm }
            if WindowDisabler = nil then
              WindowDisabler := TWindowDisabler.Create;
            if RunEntry.StatusMsg <> '' then begin
              try
                WizardForm.StatusLabel.Caption := ExpandConst(RunEntry.StatusMsg);
              except
                { Don't die if the expansion fails with an exception. Just
                  display the exception message, and proceed with the default
                  status message. }
                Application.HandleException(Self);
                WizardForm.StatusLabel.Caption := SetupMessages[msgStatusRunProgram];
              end;
            end
            else
              WizardForm.StatusLabel.Caption := SetupMessages[msgStatusRunProgram];
            WizardForm.StatusLabel.Update;
            if roHideWizard in RunEntry.Options then begin
              if WizardForm.Visible and not HideWizard then begin
                HideWizard := True;
                UpdateWizardFormVisibility;
              end;
            end
            else begin
              if HideWizard then begin
                HideWizard := False;
                UpdateWizardFormVisibility;
              end;
            end;
            DebugNotifyEntry(seRun, I);
            NotifyBeforeInstallEntry(RunEntry.BeforeInstall);
            ProcessRunEntry(RunEntry);
            NotifyAfterInstallEntry(RunEntry.AfterInstall);
          end;
        end;
      finally
        if HideWizard then begin
          HideWizard := False;
          UpdateWizardFormVisibility;
        end;
        WindowDisabler.Free;
        if CheckIfRestartNeeded then begin
          ChecksumAfter := MakePendingFileRenameOperationsChecksum;
          if not MD5DigestsEqual(ChecksumBefore, ChecksumAfter) then
            NeedsRestart := True;
        end;
      end;
      Application.BringToFront;
    end;
  end;

  procedure RestartApplications;
  const
    ERROR_FAIL_RESTART = 353;
  var
    Error: DWORD;
    WindowDisabler: TWindowDisabler;
  begin
    if not NeedsRestart then begin
      WizardForm.StatusLabel.Caption := SetupMessages[msgStatusRestartingApplications];
      WizardForm.StatusLabel.Update;

      Log('Attempting to restart applications.');

      { Disable windows during application restart so that a nice
        "beep" is produced if the user tries clicking on WizardForm }
      WindowDisabler := TWindowDisabler.Create;
      try
        Error := RmRestart(RmSessionHandle, 0, nil);
      finally
        WindowDisabler.Free;
      end;
      Application.BringToFront;

      if Error = ERROR_FAIL_RESTART then
        Log('One or more applications could not be restarted.')
      else if Error <> ERROR_SUCCESS then begin
        RmEndSession(RmSessionHandle);
        RmSessionStarted := False;
        LogFmt('RmRestart returned an error: %d', [Error]);
      end;
    end else
      Log('Need to restart Windows, not attempting to restart applications');
  end;

var
  Succeeded: Boolean;
  S: String;
begin
  Result := False;
  try
    if not WizardForm.ValidateDirEdit then
      Abort;
    WizardDirValue := WizardForm.DirEdit.Text;
    if not WizardForm.ValidateGroupEdit then
      Abort;
    WizardGroupValue := WizardForm.GroupEdit.Text;
    WizardNoIcons := WizardForm.NoIconsCheck.Checked;
    WizardSetupType := WizardForm.GetSetupType();
    WizardForm.GetComponents(WizardComponents, WizardDeselectedComponents);
    WizardForm.GetTasks(WizardTasks, WizardDeselectedTasks);
    WizardPreparingYesRadio := WizardForm.PreparingYesRadio.Checked;
    if InitSaveInf <> '' then
      SaveInf(InitSaveInf);

    Application.Restore;
    Update;
    if InstallMode = imSilent then begin
      SetActiveWindow(Application.Handle);  { ensure taskbar button is selected }
      WizardForm.Show;
    end;
    WizardForm.Update;

    SetStep(ssInstall, False);

    PerformInstall(Succeeded);
    if not Succeeded then begin
      { The user canceled the install or there was a fatal error }
      TerminateApp;
      Exit;
    end;
    { Can't cancel at any point after PerformInstall, so disable the button }
    WizardForm.CancelButton.Enabled := False;

    ProcessRunEntries;

    if RmDoRestart and (shRestartApplications in SetupHeader.Options) and not InitNoRestartApplications then
      RestartApplications;

    SetStep(ssPostInstall, True);

    { Notify Windows of assocations/environment changes *after* ssPostInstall
      since user might set more stuff there }
    if shChangesAssociations in SetupHeader.Options then
      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
    if shChangesEnvironment in SetupHeader.Options then
      RefreshEnvironment;

    if InstallMode <> imNormal then
      WizardForm.Hide;

    LogFmt('Need to restart Windows? %s', [SYesNo[NeedsRestart]]);
    if NeedsRestart and not InitNoRestart then begin
      with WizardForm do begin
        ChangeFinishedLabel(ExpandSetupMessage(msgFinishedRestartLabel));
        YesRadio.Visible := True;
        NoRadio.Visible := True;
      end;
    end else begin
      if CreatedIcon then
        S := ExpandSetupMessage(msgFinishedLabel)
      else
        S := ExpandSetupMessage(msgFinishedLabelNoIcons);
      with WizardForm do begin
        ChangeFinishedLabel(S + SNewLine2 + SetupMessages[msgClickFinish]);
        if not NeedsRestart then begin
          UpdateRunList(WizardComponents, WizardTasks);
          RunList.Visible := RunList.Items.Count > 0;
        end;
      end;
    end;

    if InstallMode = imNormal then begin
      Application.Restore;
      Update;
    end;

    Result := True;
  except
    { If an exception was raised, display the message, then terminate }
    Application.HandleException(Self);
    SetupExitCode := ecNextStepError;
    TerminateApp;
  end;
end;

procedure TMainForm.Finish(const FromPreparingPage: Boolean);

  procedure WaitForForegroundLoss;

    function IsForegroundProcess: Boolean;
    var
      W: HWND;
      PID: DWORD;
    begin
      W := GetForegroundWindow;
      Result := False;
      if (W <> 0) and (GetWindowThreadProcessId(W, @PID) <> 0) then
        Result := (PID = GetCurrentProcessId);
    end;

  var
    StartTick: DWORD;
  begin
    StartTick := GetTickCount;
    while IsForegroundProcess do begin
      { Stop if it's taking too long (e.g. if the spawned process never
        displays a window) }
      if Cardinal(GetTickCount - StartTick) >= Cardinal(1000) then
        Break;
      ProcessMessagesProc;
      WaitMessageWithTimeout(10);
      ProcessMessagesProc;
    end;
  end;

  procedure ProcessPostInstallRunEntries;
  var
    WindowDisabler: TWindowDisabler;
    ProcessedNoWait: Boolean;
    I: Integer;
    RunEntry: PSetupRunEntry;
  begin
    WindowDisabler := nil;
    try
      ProcessedNoWait := False;
      with WizardForm do begin
        for I := 0 to RunList.Items.Count-1 do begin
          if RunList.Checked[I] then begin
            { Disable windows before processing the first entry }
            if WindowDisabler = nil then
              WindowDisabler := TWindowDisabler.Create;
            RunEntry := PSetupRunEntry(Entries[seRun][Integer(RunList.ItemObject[I])]);
            DebugNotifyEntry(seRun, Integer(RunList.ItemObject[I]));
            NotifyBeforeInstallEntry(RunEntry.BeforeInstall);
            ProcessRunEntry(RunEntry);
            NotifyAfterInstallEntry(RunEntry.AfterInstall);
            if RunEntry.Wait = rwNoWait then
              ProcessedNoWait := True;
          end;
        end;
      end;
      { Give nowait processes some time to bring themselves to the
        foreground before Setup exits. Without this delay, the application
        underneath Setup can end up coming to the foreground instead.
        (Note: Windows are already disabled at this point.) }
      if ProcessedNoWait then
        WaitForForegroundLoss;
    finally
      WindowDisabler.Free;
    end;
  end;

var
  S: String;
begin
  try
    { Deactivate WizardForm so another application doesn't come to the
      foreground when Hide is called. (Needed by WaitForForegroundLoss.) }
    if GetForegroundWindow = WizardForm.Handle then
      SetActiveWindow(Application.Handle);
    WizardForm.Hide;

    if not FromPreparingPage and not NeedsRestart then begin
      ProcessPostInstallRunEntries;
    end else begin
      if FromPreparingPage then
        SetupExitCode := ecPrepareToInstallFailedRestartNeeded
      else if InitRestartExitCode <> 0 then
        SetupExitCode := InitRestartExitCode;

      if InitNoRestart then
        RestartSystem := False
      else begin
        case InstallMode of
          imNormal:
            if FromPreparingPage then
              RestartSystem := WizardForm.PreparingYesRadio.Checked
            else
              RestartSystem := WizardForm.YesRadio.Checked;
          imSilent:
            begin
              if FromPreparingPage then
                S := WizardForm.PrepareToInstallFailureMessage + SNewLine +
                  SNewLine + SNewLine + ExpandSetupMessage(msgFinishedRestartMessage)
              else
                S := ExpandSetupMessage(msgFinishedRestartMessage);
              RestartSystem :=
                LoggedMsgBox(S, '', mbConfirmation, MB_YESNO, True, IDYES) = IDYES;
            end;
          imVerySilent:
            RestartSystem := True;
        end;
      end;
      if not RestartSystem then
        Log('Will not restart Windows automatically.');
    end;

    SetStep(ssDone, True);
  except
    Application.HandleException(Self);
    SetupExitCode := ecNextStepError;
  end;
  TerminateApp;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  function ConfirmCancel(const DefaultConfirm: Boolean): Boolean;
  var
    Cancel, Confirm: Boolean;
  begin
    Cancel := True;
    Confirm := DefaultConfirm;
    WizardForm.CallCancelButtonClick(Cancel, Confirm);
    Result := Cancel and (not Confirm or ExitSetupMsgBox);
  end;

begin
  { Note: Setting CanClose to True causes Application.Terminate to be called;
    we don't want that. }
  CanClose := False;
  if Assigned(WizardForm) and WizardForm.HandleAllocated and
     IsWindowVisible(WizardForm.Handle) and IsWindowEnabled(WizardForm.Handle) and
     WizardForm.CancelButton.CanFocus then begin
    case CurStep of
      ssPreInstall:
        if ConfirmCancel((WizardForm.CurPageID <> wpPreparing) or (WizardForm.PrepareToInstallFailureMessage = '')) then begin
          if WizardForm.CurPageID = wpPreparing then
            SetupExitCode := ecPrepareToInstallFailed
          else
            SetupExitCode := ecCancelledBeforeInstall;
          TerminateApp;
        end;
      ssInstall:
        if (shAllowCancelDuringInstall in SetupHeader.Options) and not InitNoCancel then
          if ConfirmCancel(True) then
            NeedToAbortInstall := True;
    end;
  end;
end;

procedure TMainForm.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := Message.Result or DLGC_WANTTAB;
end;

function EWP(Wnd: HWND; Param: LPARAM): BOOL; stdcall;
begin
  { Note: GetParent is not used here because the other windows are not
    actually child windows since they don't have WS_CHILD set. }
  if GetWindowLong(Wnd, GWL_HWNDPARENT) <> Param then
    Result := True
  else begin
    Result := False;
    BringWindowToTop(Wnd);
  end;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { If, for some reason, the user doesn't have a mouse and the main form was
    activated, there would normally be no way to reactivate the child form.
    But this reactivates the form if the user hits a key on the keyboard }
  if not(ssAlt in Shift) then begin
    Key := 0;
    EnumThreadWindows(GetCurrentThreadId, @EWP, Handle);
  end;
end;

procedure TMainForm.UpdateWizardFormVisibility;
var
  ShouldShow: Boolean;
begin
  { Note: We don't adjust WizardForm.Visible because on Delphi 3+, if all forms
    have Visible set to False, the application taskbar button disappears. }
  if Assigned(WizardForm) and WizardForm.HandleAllocated then begin
    ShouldShow := WizardForm.Showing and not HideWizard and
      not IsIconic(Application.Handle);
    if (GetWindowLong(WizardForm.Handle, GWL_STYLE) and WS_VISIBLE <> 0) <> ShouldShow then begin
      if ShouldShow then
        ShowWindow(WizardForm.Handle, SW_SHOW)
      else
        ShowWindow(WizardForm.Handle, SW_HIDE);
    end;
  end;
end;

function TMainForm.MainWindowHook(var Message: TMessage): Boolean;
var
  IsIcon: Boolean;
begin
  Result := False;
  case Message.Msg of
    WM_WINDOWPOSCHANGED: begin
        { When the application window is minimized or restored, also hide or
          show WizardForm.
          Note: MainForm is hidden/shown automatically because its owner
          window is Application.Handle. }
        IsIcon := IsIconic(Application.Handle);
        if IsMinimized <> IsIcon then begin
          IsMinimized := IsIcon;
          UpdateWizardFormVisibility;
        end;
      end;
  end;
end;

procedure TMainForm.WMShowWindow(var Message: TWMShowWindow);
begin
  inherited;
  { When showing, ensure WizardForm is the active window, not MainForm }
  if Message.Show and (GetActiveWindow = Handle) and
     Assigned(WizardForm) and WizardForm.HandleAllocated and
     IsWindowVisible(WizardForm.Handle) then
    SetActiveWindow(WizardForm.Handle);
end;


procedure InitIsWin64AndProcessorArchitecture;
const
  PROCESSOR_ARCHITECTURE_INTEL = 0;
  PROCESSOR_ARCHITECTURE_IA64 = 6;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
var
  KernelModule: HMODULE;
  GetNativeSystemInfoFunc: procedure(var lpSystemInfo: TSystemInfo); stdcall;
  IsWow64ProcessFunc: function(hProcess: THandle; var Wow64Process: BOOL): BOOL; stdcall;
  Wow64Process: BOOL;
  SysInfo: TSystemInfo;
begin
  { The system is considered a "Win64" system if all of the following
    conditions are true:
    1. GetNativeSystemInfo is available.
    2. IsWow64Process is available, and returns True for the current process.
    3. Wow64DisableWow64FsRedirection is available.
    4. Wow64RevertWow64FsRedirection is available.
    5. GetSystemWow64DirectoryA is available.
    6. RegDeleteKeyExA is available.
    The system does not have to be one of the known 64-bit architectures
    (AMD64, IA64) to be considered a "Win64" system. }

  IsWin64 := False;
  KernelModule := GetModuleHandle(kernel32);
  GetNativeSystemInfoFunc := GetProcAddress(KernelModule, 'GetNativeSystemInfo');
  if Assigned(GetNativeSystemInfoFunc) then begin
    GetNativeSystemInfoFunc(SysInfo);
    IsWow64ProcessFunc := GetProcAddress(KernelModule, 'IsWow64Process');
    if Assigned(IsWow64ProcessFunc) and
       IsWow64ProcessFunc(GetCurrentProcess, Wow64Process) and
       Wow64Process then begin
      if AreFsRedirectionFunctionsAvailable and
         (GetProcAddress(KernelModule, 'GetSystemWow64DirectoryA') <> nil) and
         (GetProcAddress(GetModuleHandle(advapi32), 'RegDeleteKeyExA') <> nil) then
        IsWin64 := True;
    end;
  end
  else
    GetSystemInfo(SysInfo);

  case SysInfo.wProcessorArchitecture of
    PROCESSOR_ARCHITECTURE_INTEL: ProcessorArchitecture := paX86;
    PROCESSOR_ARCHITECTURE_IA64: ProcessorArchitecture := paIA64;
    PROCESSOR_ARCHITECTURE_AMD64: ProcessorArchitecture := paX64;
  else
    ProcessorArchitecture := paUnknown;
  end;
end;

procedure InitWindowsVersion;

  procedure ReadServicePackFromRegistry;
  var
    K: HKEY;
    Size, Typ, SP: DWORD;
  begin
    if RegOpenKeyExView(rvDefault, HKEY_LOCAL_MACHINE, 'System\CurrentControlSet\Control\Windows',
       0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      Size := SizeOf(SP);
      if (RegQueryValueEx(K, 'CSDVersion', nil, @Typ, @SP, @Size) = ERROR_SUCCESS) and
         (Typ = REG_DWORD) and (Size = SizeOf(SP)) then
        NTServicePackLevel := Word(SP);
      RegCloseKey(K);
    end;
  end;

  procedure ReadProductTypeFromRegistry;
  const
    VER_NT_WORKSTATION = 1;
    VER_NT_DOMAIN_CONTROLLER = 2;
    VER_NT_SERVER = 3;
  var
    K: HKEY;
    S: String;
  begin
    if RegOpenKeyExView(rvDefault, HKEY_LOCAL_MACHINE, 'System\CurrentControlSet\Control\ProductOptions',
       0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      { See MS KB article 152078 for details on this key } 
      if RegQueryStringValue(K, 'ProductType', S) then begin
        if CompareText(S, 'WinNT') = 0 then
          WindowsProductType := VER_NT_WORKSTATION
        else if CompareText(S, 'LanmanNT') = 0 then
          WindowsProductType := VER_NT_DOMAIN_CONTROLLER
        else if CompareText(S, 'ServerNT') = 0 then
          WindowsProductType := VER_NT_SERVER;
      end;
      RegCloseKey(K);
    end;
  end;

type
  TOSVersionInfoEx = packed record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of Char;
    wServicePackMajor: Word;
    wServicePackMinor: Word;
    wSuiteMask: Word;
    wProductType: Byte;
    wReserved: Byte;
  end;
var
  OSVersionInfo: TOSVersionInfo;
  OSVersionInfoEx: TOSVersionInfoEx;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  if GetVersionEx(OSVersionInfo) then begin
    WindowsVersion := (Byte(OSVersionInfo.dwMajorVersion) shl 24) or
      (Byte(OSVersionInfo.dwMinorVersion) shl 16) or
      Word(OSVersionInfo.dwBuildNumber);
    { ^ Note: We MUST clip dwBuildNumber to 16 bits for Win9x compatibility }
    if IsNT then begin
      if OSVersionInfo.dwMajorVersion >= 5 then begin
        { OSVERSIONINFOEX is only available starting in Windows 2000 }
        OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
        if GetVersionEx(POSVersionInfo(@OSVersionInfoEx)^) then begin
          NTServicePackLevel := (Byte(OSVersionInfoEx.wServicePackMajor) shl 8) or
            Byte(OSVersionInfoEx.wServicePackMinor);
          WindowsProductType := OSVersionInfoEx.wProductType;
          WindowsSuiteMask := OSVersionInfoEx.wSuiteMask;
        end;
      end
      else if OSVersionInfo.dwMajorVersion = 4 then begin
        { Read from the registry on NT 4 }
        ReadServicePackFromRegistry;
        ReadProductTypeFromRegistry;
      end;
    end;
  end;
end;

procedure CreateEntryLists;
var
  I: TEntryType;
begin
  for I := Low(I) to High(I) do
    Entries[I] := TList.Create;
end;

procedure FreeEntryLists;
var
  I: TEntryType;
  List: TList;
  J: Integer;
  P: Pointer;
begin
  for I := High(I) downto Low(I) do begin
    List := Entries[I];
    if Assigned(List) then begin
      Entries[I] := nil;
      for J := List.Count-1 downto 0 do begin
        P := List[J];
        if EntryStrings[I] <> 0 then
          SEFreeRec(P, EntryStrings[I], EntryAnsiStrings[I])
        else
          FreeMem(P);
      end;
      List.Free;
    end;
    FreeAndNil(OriginalEntryIndexes[I]);
  end;
end;

initialization
  IsNT := UsingWinNT;
  InitIsWin64AndProcessorArchitecture;
  InitWindowsVersion;
{$IFNDEF UNICODE}
  ConstLeadBytes := @SetupHeader.LeadBytes;
{$ENDIF}
  InitComponents := TStringList.Create();
  InitTasks := TStringList.Create();
  WizardComponents := TStringList.Create();
  WizardDeselectedComponents := TStringList.Create();
  WizardTasks := TStringList.Create();
  WizardDeselectedTasks := TStringList.Create();
  CreateEntryLists;
  DeleteFilesAfterInstallList := TStringList.Create;
  DeleteDirsAfterInstallList := TStringList.Create;
  CloseApplicationsFilterList := TStringList.Create;

finalization
  FreeAndNil(WizardImage);
  FreeAndNil(WizardSmallImage);
  FreeAndNil(CloseApplicationsFilterList);
  FreeAndNil(DeleteDirsAfterInstallList);
  FreeAndNil(DeleteFilesAfterInstallList);
  FreeEntryLists;
  FreeAndNil(WizardDeselectedTasks);
  FreeAndNil(WizardTasks);
  FreeAndNil(WizardDeselectedComponents);
  FreeAndNil(WizardComponents);
  FreeAndNil(InitTasks);
  FreeAndNil(InitComponents);
end.
