unit Compiler.SetupCompiler;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler
}

{x$DEFINE STATICPREPROC}
{ For debugging purposes, remove the 'x' to have it link the ISPP code into this
  program and not depend on ISPP.dll. You will also need to add the Src
  folder to the Delphi Compiler Search path in the project options. Most useful
  when combined with IDE.MainForm's or ISCC's STATICCOMPILER. }

interface

uses
  Windows, SysUtils, Classes, Generics.Collections,
  SimpleExpression, SHA256, ChaCha20,
  Shared.Struct, Shared.CompilerInt, Shared.PreprocInt, Shared.SetupMessageIDs,
  Shared.SetupSectionDirectives, Shared.VerInfoFunc, Shared.Int64Em, Shared.DebugStruct,
  Compiler.ScriptCompiler, Compiler.StringLists, Compression.LZMACompressor;

type
  EISCompileError = class(Exception);

  TParamFlags = set of (piRequired, piNoEmpty, piNoQuotes);

  TParamInfo = record
    Name: String;
    Flags: TParamFlags;
  end;
  TParamValue = record
    Found: Boolean;
    Data: String;
  end;

  TEnumIniSectionProc = procedure(const Line: PChar; const Ext: Integer) of object;

  TAllowedConst = (acOldData, acBreak);
  TAllowedConsts = set of TAllowedConst;

  TPreLangData = class
  public
    Name: String;
    LanguageCodePage: Integer;
  end;

  TLangData = class
  public
    MessagesDefined: array[TSetupMessageID] of Boolean;
    Messages: array[TSetupMessageID] of String;
  end;

  TNameAndAccessMask = record
    Name: String;
    Mask: DWORD;
  end;

  TCheckOrInstallKind = (cikCheck, cikDirectiveCheck, cikInstall);

  TSetupCompiler = class
  private
    ScriptFiles: TStringList;
    PreprocOptionsString: String;
    PreprocCleanupProc: TPreprocCleanupProc;
    PreprocCleanupProcData: Pointer;

    LanguageEntries,
    CustomMessageEntries,
    PermissionEntries,
    TypeEntries,
    ComponentEntries,
    TaskEntries,
    DirEntries,
    FileEntries,
    FileLocationEntries,
    IconEntries,
    IniEntries,
    RegistryEntries,
    InstallDeleteEntries,
    UninstallDeleteEntries,
    RunEntries,
    UninstallRunEntries: TList;

    FileLocationEntryFilenames: THashStringList;
    WarningsList: THashStringList;
    ExpectedCustomMessageNames: TStringList;
    MissingMessagesWarning, MissingRunOnceIdsWarning, MissingRunOnceIds, NotRecognizedMessagesWarning, UsedUserAreasWarning: Boolean;
    UsedUserAreas: TStringList;

    PreprocIncludedFilenames: TStringList;
    PreprocOutput: String;

    DefaultLangData: TLangData;
    PreLangDataList, LangDataList: TList;
    SignToolList: TList;
    SignTools, SignToolsParams: TStringList;
    SignToolRetryCount, SignToolRetryDelay, SignToolMinimumTimeBetween: Integer;
    SignToolRunMinimized: Boolean;
    LastSignCommandStartTick: DWORD;

    OutputDir, OutputBaseFilename, OutputManifestFile, SignedUninstallerDir,
      ExeFilename: String;
    Output, FixedOutput, FixedOutputDir, FixedOutputBaseFilename: Boolean;
    CompressMethod: TSetupCompressMethod;
    InternalCompressLevel, CompressLevel: Integer;
    InternalCompressProps, CompressProps: TLZMACompressorProps;
    UseSolidCompression: Boolean;
    DontMergeDuplicateFiles: Boolean;
    Password: String;
    CryptKey: TSetupEncryptionKey;
    TimeStampsInUTC: Boolean;
    TimeStampRounding: Integer;
    TouchDateOption: (tdCurrent, tdNone, tdExplicit);
    TouchDateYear, TouchDateMonth, TouchDateDay: Integer;
    TouchTimeOption: (ttCurrent, ttNone, ttExplicit);
    TouchTimeHour, TouchTimeMinute, TouchTimeSecond: Integer;

    SetupHeader: TSetupHeader;

    SetupDirectiveLines: array[TSetupSectionDirective] of Integer;
    UseSetupLdr, DiskSpanning, TerminalServicesAware, DEPCompatible, ASLRCompatible: Boolean;
    DiskSliceSize, DiskClusterSize, SlicesPerDisk, ReserveBytes: Longint;
    LicenseFile, InfoBeforeFile, InfoAfterFile, WizardImageFile: String;
    WizardSmallImageFile: String;
    DefaultDialogFontName: String;

    VersionInfoVersion, VersionInfoProductVersion: TFileVersionNumbers;
    VersionInfoVersionOriginalValue, VersionInfoCompany, VersionInfoCopyright,
      VersionInfoDescription, VersionInfoTextVersion, VersionInfoProductName, VersionInfoOriginalFileName,
      VersionInfoProductTextVersion, VersionInfoProductVersionOriginalValue: String;
    SetupIconFilename: String;

    CodeText: TStringList;
    CodeCompiler: TScriptCompiler;
    CompiledCodeText: AnsiString;

    CompileWasAlreadyCalled: Boolean;
    LineFilename: String;
    LineNumber: Integer;

    DebugInfo, CodeDebugInfo: TMemoryStream;
    DebugEntryCount, VariableDebugEntryCount: Integer;
    CompiledCodeTextLength, CompiledCodeDebugInfoLength: Integer;

    GotPrevFilename: Boolean;
    PrevFilename: String;
    PrevFileIndex: Integer;

    TotalBytesToCompress, BytesCompressedSoFar: Integer64;
    CompressionInProgress: Boolean;
    CompressionStartTick: DWORD;

    CachedUserDocsDir: String;

    procedure AddStatus(const S: String; const Warning: Boolean = False);
    procedure AddStatusFmt(const Msg: String; const Args: array of const;
      const Warning: Boolean);
    procedure AbortCompile(const Msg: String);
    procedure AbortCompileOnLine(const Msg: String);
    procedure AbortCompileOnLineFmt(const Msg: String;
      const Args: array of const);
    procedure AbortCompileParamError(const Msg, ParamName: String);
    function PrependDirName(const Filename, Dir: String): String;
    function PrependSourceDirName(const Filename: String): String;
    procedure DoCallback(const Code: Integer; var Data: TCompilerCallbackData;
      const IgnoreCallbackResult: Boolean = False);
    procedure EnumIniSection(const EnumProc: TEnumIniSectionProc;
      const SectionName: String; const Ext: Integer; const Verbose, SkipBlankLines: Boolean;
      const Filename: String; const LangSection: Boolean = False; const LangSectionPre: Boolean = False);
    function EvalCheckOrInstallIdentifier(Sender: TSimpleExpression; const Name: String;
      const Parameters: array of const): Boolean;
    procedure CheckCheckOrInstall(const ParamName, ParamData: String;
      const Kind: TCheckOrInstallKind);
    function CheckConst(const S: String; const MinVersion: TSetupVersionData;
      const AllowedConsts: TAllowedConsts): Boolean;
    procedure CheckCustomMessageDefinitions;
    procedure CheckCustomMessageReferences;
    procedure EnumTypesProc(const Line: PChar; const Ext: Integer);
    procedure EnumComponentsProc(const Line: PChar; const Ext: Integer);
    procedure EnumTasksProc(const Line: PChar; const Ext: Integer);
    procedure EnumDirsProc(const Line: PChar; const Ext: Integer);
    procedure EnumIconsProc(const Line: PChar; const Ext: Integer);
    procedure EnumINIProc(const Line: PChar; const Ext: Integer);
    procedure EnumLangOptionsPreProc(const Line: PChar; const Ext: Integer);
    procedure EnumLangOptionsProc(const Line: PChar; const Ext: Integer);
    procedure EnumLanguagesPreProc(const Line: PChar; const Ext: Integer);
    procedure EnumLanguagesProc(const Line: PChar; const Ext: Integer);
    procedure EnumRegistryProc(const Line: PChar; const Ext: Integer);
    procedure EnumDeleteProc(const Line: PChar; const Ext: Integer);
    procedure EnumFilesProc(const Line: PChar; const Ext: Integer);
    procedure EnumRunProc(const Line: PChar; const Ext: Integer);
    procedure EnumSetupProc(const Line: PChar; const Ext: Integer);
    procedure EnumMessagesProc(const Line: PChar; const Ext: Integer);
    procedure EnumCustomMessagesProc(const Line: PChar; const Ext: Integer);
    procedure ExtractParameters(S: PChar; const ParamInfo: array of TParamInfo;
      var ParamValues: array of TParamValue);
    function FindLangEntryIndexByName(const AName: String; const Pre: Boolean): Integer;
    function FindSignToolIndexByName(const AName: String): Integer;
    function GetLZMAExeFilename(const Allow64Bit: Boolean): String;
    procedure InitBzipDLL;
    procedure InitPreLangData(const APreLangData: TPreLangData);
    procedure InitLanguageEntry(var ALanguageEntry: TSetupLanguageEntry);
    procedure InitLZMADLL;
    procedure InitPreprocessor;
    procedure InitZipDLL;
    procedure PopulateLanguageEntryData;
    procedure ProcessMinVersionParameter(const ParamValue: TParamValue;
      var AMinVersion: TSetupVersionData);
    procedure ProcessOnlyBelowVersionParameter(const ParamValue: TParamValue;
      var AOnlyBelowVersion: TSetupVersionData);
    procedure ProcessPermissionsParameter(ParamData: String;
      const AccessMasks: array of TNameAndAccessMask; var PermissionsEntry: Smallint);
    function EvalArchitectureIdentifier(Sender: TSimpleExpression; const Name: String;
      const Parameters: array of const): Boolean;
    function EvalComponentIdentifier(Sender: TSimpleExpression; const Name: String;
      const Parameters: array of const): Boolean;
    function EvalTaskIdentifier(Sender: TSimpleExpression; const Name: String;
      const Parameters: array of const): Boolean;
    function EvalLanguageIdentifier(Sender: TSimpleExpression; const Name: String;
      const Parameters: array of const): Boolean;
    procedure ProcessExpressionParameter(const ParamName,
      ParamData: String; OnEvalIdentifier: TSimpleExpressionOnEvalIdentifier;
      SlashConvert: Boolean; var ProcessedParamData: String);
    procedure ProcessWildcardsParameter(const ParamData: String;
      const AWildcards: TStringList; const TooLongMsg: String);
    procedure ReadDefaultMessages;
    procedure ReadMessagesFromFilesPre(const AFiles: String; const ALangIndex: Integer);
    procedure ReadMessagesFromFiles(const AFiles: String; const ALangIndex: Integer);
    procedure ReadMessagesFromScriptPre;
    procedure ReadMessagesFromScript;
    function ReadScriptFile(const Filename: String; const UseCache: Boolean;
      const AnsiConvertCodePage: Cardinal): TScriptFileLines;
    procedure RenamedConstantCallback(const Cnst, CnstRenamed: String);
    procedure EnumCodeProc(const Line: PChar; const Ext: Integer);
    procedure ReadCode;
    procedure CodeCompilerOnLineToLineInfo(const Line: LongInt; var Filename: String; var FileLine: LongInt);
    procedure CodeCompilerOnUsedLine(const Filename: String; const Line, Position: LongInt; const IsProcExit: Boolean);
    procedure CodeCompilerOnUsedVariable(const Filename: String; const Line, Col, Param1, Param2, Param3: LongInt; const Param4: AnsiString);
    procedure CodeCompilerOnError(const Msg: String; const ErrorFilename: String; const ErrorLine: LongInt);
    procedure CodeCompilerOnWarning(const Msg: String);
    procedure CompileCode;
    function FilenameToFileIndex(const AFileName: String): Integer;
    procedure ReadTextFile(const Filename: String; const LangIndex: Integer; var Text: AnsiString);
    procedure SeparateDirective(const Line: PChar; var Key, Value: String);
    procedure ShiftDebugEntryIndexes(AKind: TDebugEntryKind);
    procedure Sign(AExeFilename: String);
    procedure SignCommand(const AName, ACommand, AParams, AExeFilename: String; const RetryCount, RetryDelay, MinimumTimeBetween: Integer; const RunMinimized: Boolean);
    procedure WriteDebugEntry(Kind: TDebugEntryKind; Index: Integer; StepOutMarker: Boolean = False);
    procedure WriteCompiledCodeText(const CompiledCodeText: Ansistring);
    procedure WriteCompiledCodeDebugInfo(const CompiledCodeDebugInfo: AnsiString);
    function CreateMemoryStreamsFromFiles(const ADirectiveName, AFiles: String): TObjectList<TCustomMemoryStream>;
    function CreateMemoryStreamsFromResources(const AResourceNamesPrefixes, AResourceNamesPostfixes: array of String): TObjectList<TCustomMemoryStream>;
  public
    AppData: Longint;
    CallbackProc: TCompilerCallbackProc;
    CompilerDir, SourceDir, OriginalSourceDir: String;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure AbortCompileFmt(const Msg: String; const Args: array of const);
    procedure AddBytesCompressedSoFar(const Value: Cardinal); overload;
    procedure AddBytesCompressedSoFar(const Value: Integer64); overload;
    procedure AddPreprocOption(const Value: String);
    procedure AddSignTool(const Name, Command: String);
    procedure CallIdleProc(const IgnoreCallbackResult: Boolean = False);
    procedure Compile;
    function GetBytesCompressedSoFar: Integer64;
    function GetDebugInfo: TMemoryStream;
    function GetDiskSliceSize:Longint;
    function GetDiskSpanning: Boolean;
    function GetEncryptionBaseNonce: TSetupEncryptionNonce;
    function GetExeFilename: String;
    function GetLineFilename: String;
    function GetLineNumber: Integer;
    function GetOutputBaseFileName: String;
    function GetOutputDir: String;
    function GetPreprocIncludedFilenames: TStringList;
    function GetPreprocOutput: String;
    function GetSlicesPerDisk: Longint;
    procedure SetBytesCompressedSoFar(const Value: Integer64);
    procedure SetOutput(Value: Boolean);
    procedure SetOutputBaseFilename(const Value: String);
    procedure SetOutputDir(const Value: String);
  end;

implementation

uses
  Commctrl, TypInfo, AnsiStrings, Math, WideStrUtils,
  PathFunc, Shared.CommonFunc, Compiler.Messages, Shared.SetupEntFunc,
  Shared.FileClass, Compression.Base, Compression.Zlib, Compression.bzlib,
  Shared.LangOptionsSectionDirectives, Shared.ResUpdateFunc, Compiler.ExeUpdateFunc,
{$IFDEF STATICPREPROC}
  ISPP.Preprocess,
{$ENDIF}
  Shared.SetupTypes, Compiler.CompressionHandler, Compiler.HelperFunc, Compiler.BuiltinPreproc;

type
  TLineInfo = class
  public
    FileName: String;
    FileLineNumber: Integer;
  end;

  TSignTool = class
    Name, Command: String;
  end;

var
  ZipInitialized, BzipInitialized, LZMAInitialized: Boolean;
  PreprocessorInitialized: Boolean;
  PreprocessScriptProc: TPreprocessScriptProc;

const
  ParamCommonFlags = 'Flags';
  ParamCommonComponents = 'Components';
  ParamCommonTasks = 'Tasks';
  ParamCommonLanguages = 'Languages';
  ParamCommonCheck = 'Check';
  ParamCommonBeforeInstall = 'BeforeInstall';
  ParamCommonAfterInstall = 'AfterInstall';
  ParamCommonMinVersion = 'MinVersion';
  ParamCommonOnlyBelowVersion = 'OnlyBelowVersion';

  DefaultTypeEntryNames: array[0..2] of PChar = ('full', 'compact', 'custom');

  MaxDiskSliceSize = 2100000000;

function ExtractStr(var S: String; const Separator: Char): String;
var
  I: Integer;
begin
  repeat
    I := PathPos(Separator, S);
    if I = 0 then I := Length(S)+1;
    Result := Trim(Copy(S, 1, I-1));
    S := Trim(Copy(S, I+1, Maxint));
  until (Result <> '') or (S = '');
end;

{ TSetupCompiler }

constructor TSetupCompiler.Create(AOwner: TComponent);
begin
  inherited Create;
  ScriptFiles := TStringList.Create;
  LanguageEntries := TLowFragList.Create;
  CustomMessageEntries := TLowFragList.Create;
  PermissionEntries := TLowFragList.Create;
  TypeEntries := TLowFragList.Create;
  ComponentEntries := TLowFragList.Create;
  TaskEntries := TLowFragList.Create;
  DirEntries := TLowFragList.Create;
  FileEntries := TLowFragList.Create;
  FileLocationEntries := TLowFragList.Create;
  IconEntries := TLowFragList.Create;
  IniEntries := TLowFragList.Create;
  RegistryEntries := TLowFragList.Create;
  InstallDeleteEntries := TLowFragList.Create;
  UninstallDeleteEntries := TLowFragList.Create;
  RunEntries := TLowFragList.Create;
  UninstallRunEntries := TLowFragList.Create;
  FileLocationEntryFilenames := THashStringList.Create;
  WarningsList := THashStringList.Create;
  WarningsList.IgnoreDuplicates := True;
  ExpectedCustomMessageNames := TStringList.Create;
  UsedUserAreas := TStringList.Create;
  UsedUserAreas.Sorted := True;
  UsedUserAreas.Duplicates := dupIgnore;
  PreprocIncludedFilenames := TStringList.Create;
  DefaultLangData := TLangData.Create;
  PreLangDataList := TLowFragList.Create;
  LangDataList := TLowFragList.Create;
  SignToolList := TLowFragList.Create;
  SignTools := TStringList.Create;
  SignToolsParams := TStringList.Create;
  DebugInfo := TMemoryStream.Create;
  CodeDebugInfo := TMemoryStream.Create;
  CodeText := TStringList.Create;
  CodeCompiler := TScriptCompiler.Create;
  CodeCompiler.NamingAttribute := 'Event';
  CodeCompiler.OnLineToLineInfo := CodeCompilerOnLineToLineInfo;
  CodeCompiler.OnUsedLine := CodeCompilerOnUsedLine;
  CodeCompiler.OnUsedVariable := CodeCompilerOnUsedVariable;
  CodeCompiler.OnError := CodeCompilerOnError;
  CodeCompiler.OnWarning := CodeCompilerOnWarning;
end;

destructor TSetupCompiler.Destroy;
var
  I: Integer;
begin
  CodeCompiler.Free;
  CodeText.Free;
  CodeDebugInfo.Free;
  DebugInfo.Free;
  SignToolsParams.Free;
  SignTools.Free;
  if Assigned(SignToolList) then begin
    for I := 0 to SignToolList.Count-1 do
      TSignTool(SignToolList[I]).Free;
    SignToolList.Free;
  end;
  LangDataList.Free;
  PreLangDataList.Free;
  DefaultLangData.Free;
  PreprocIncludedFilenames.Free;
  UsedUserAreas.Free;
  ExpectedCustomMessageNames.Free;
  WarningsList.Free;
  FileLocationEntryFilenames.Free;
  UninstallRunEntries.Free;
  RunEntries.Free;
  UninstallDeleteEntries.Free;
  InstallDeleteEntries.Free;
  RegistryEntries.Free;
  IniEntries.Free;
  IconEntries.Free;
  FileLocationEntries.Free;
  FileEntries.Free;
  DirEntries.Free;
  TaskEntries.Free;
  ComponentEntries.Free;
  TypeEntries.Free;
  PermissionEntries.Free;
  CustomMessageEntries.Free;
  LanguageEntries.Free;
  ScriptFiles.Free;
  inherited Destroy;
end;

function TSetupCompiler.CreateMemoryStreamsFromFiles(const ADirectiveName, AFiles: String): TObjectList<TCustomMemoryStream>;

  procedure AddFile(const Filename: String);
  begin
    AddStatus(Format(SCompilerStatusReadingInFile, [FileName]));
    Result.Add(CreateMemoryStreamFromFile(FileName));
  end;

var
  Filename, SearchSubDir: String;
  AFilesList: TStringList;
  I: Integer;
  H: THandle;
  FindData: TWin32FindData;
begin
  Result := TObjectList<TCustomMemoryStream>.Create;
  try
    { In older versions only one file could be listed and comma's could be used so
      before treating AFiles as a list, first check if it's actually a single file
      with a comma in its name. }
    Filename := PrependSourceDirName(AFiles);
    if NewFileExists(Filename) then
       AddFile(Filename)
    else begin
      AFilesList := TStringList.Create;
      try
        ProcessWildcardsParameter(AFiles, AFilesList,
          Format(SCompilerDirectivePatternTooLong, [ADirectiveName]));
        for I := 0 to AFilesList.Count-1 do begin
          Filename := PrependSourceDirName(AFilesList[I]);
          if IsWildcard(FileName) then begin
            H := FindFirstFile(PChar(Filename), FindData);
            if H <> INVALID_HANDLE_VALUE then begin
              try
                SearchSubDir := PathExtractPath(Filename);
                repeat
                  if FindData.dwFileAttributes and (FILE_ATTRIBUTE_DIRECTORY or FILE_ATTRIBUTE_HIDDEN) <> 0 then
                    Continue;
                   AddFile(SearchSubDir + FindData.cFilename);
                until not FindNextFile(H, FindData);
              finally
                Windows.FindClose(H);
              end;
            end;
          end else
            AddFile(Filename);  { use the case specified in the script }
        end;
      finally
        AFilesList.Free;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TSetupCompiler.CreateMemoryStreamsFromResources(const AResourceNamesPrefixes, AResourceNamesPostfixes: array of String): TObjectList<TCustomMemoryStream>;
var
  I, J: Integer;
begin
  Result := TObjectList<TCustomMemoryStream>.Create;
  try
    for I := 0 to Length(AResourceNamesPrefixes)-1 do
      for J := 0 to Length(AResourceNamesPostfixes)-1 do
        Result.Add(TResourceStream.Create(HInstance, AResourceNamesPrefixes[I]+AResourceNamesPostfixes[J], RT_RCDATA));
  except
    Result.Free;
    raise;
  end;
end;

procedure TSetupCompiler.InitPreprocessor;
{$IFNDEF STATICPREPROC}
var
  Filename: String;
  Attr: DWORD;
  M: HMODULE;
{$ENDIF}
begin
  if PreprocessorInitialized then
    Exit;
{$IFNDEF STATICPREPROC}
  Filename := CompilerDir + 'ISPP.dll';
  Attr := GetFileAttributes(PChar(Filename));
  if (Attr = $FFFFFFFF) and (GetLastError = ERROR_FILE_NOT_FOUND) then begin
    { ISPP unavailable; fall back to built-in preprocessor }
  end
  else begin
    M := SafeLoadLibrary(Filename, SEM_NOOPENFILEERRORBOX);
    if M = 0 then
      AbortCompileFmt('Failed to load preprocessor DLL "%s" (%d)',
        [Filename, GetLastError]);
    PreprocessScriptProc := GetProcAddress(M, 'ISPreprocessScriptW');
    if not Assigned(PreprocessScriptProc) then
      AbortCompileFmt('Failed to get address of functions in "%s"', [Filename]);
  end;
{$ELSE}
  PreprocessScriptProc := ISPreprocessScript;
{$ENDIF}
  PreprocessorInitialized := True;
end;

procedure TSetupCompiler.InitZipDLL;
var
  M: HMODULE;
begin
  if ZipInitialized then
    Exit;
  M := SafeLoadLibrary(CompilerDir + 'iszlib.dll', SEM_NOOPENFILEERRORBOX);
  if M = 0 then
    AbortCompileFmt('Failed to load iszlib.dll (%d)', [GetLastError]);
  if not ZlibInitCompressFunctions(M) then
    AbortCompile('Failed to get address of functions in iszlib.dll');
  ZipInitialized := True;
end;

procedure TSetupCompiler.InitBzipDLL;
var
  M: HMODULE;
begin
  if BzipInitialized then
    Exit;
  M := SafeLoadLibrary(CompilerDir + 'isbzip.dll', SEM_NOOPENFILEERRORBOX);
  if M = 0 then
    AbortCompileFmt('Failed to load isbzip.dll (%d)', [GetLastError]);
  if not BZInitCompressFunctions(M) then
    AbortCompile('Failed to get address of functions in isbzip.dll');
  BzipInitialized := True;
end;

procedure TSetupCompiler.InitLZMADLL;
var
  M: HMODULE;
begin
  if LZMAInitialized then
    Exit;
  M := SafeLoadLibrary(CompilerDir + 'islzma.dll', SEM_NOOPENFILEERRORBOX);
  if M = 0 then
    AbortCompileFmt('Failed to load islzma.dll (%d)', [GetLastError]);
  if not LZMAInitCompressFunctions(M) then
    AbortCompile('Failed to get address of functions in islzma.dll');
  LZMAInitialized := True;
end;

function TSetupCompiler.GetBytesCompressedSoFar: Integer64;
begin
  Result := BytesCompressedSoFar;
end;

function TSetupCompiler.GetDebugInfo: TMemoryStream;
begin
  Result := DebugInfo;
end;

function TSetupCompiler.GetDiskSliceSize: Longint;
begin
  Result := DiskSliceSize;
end;

function TSetupCompiler.GetDiskSpanning: Boolean;
begin
  Result := DiskSpanning;
end;

function TSetupCompiler.GetEncryptionBaseNonce: TSetupEncryptionNonce;
begin
  Result := SetupHeader.EncryptionBaseNonce;
end;

function TSetupCompiler.GetExeFilename: String;
begin
  Result := ExeFilename;
end;

function TSetupCompiler.GetLineFilename: String;
begin
  Result := LineFilename;
end;

function TSetupCompiler.GetLineNumber: Integer;
begin
  Result := LineNumber;
end;

function TSetupCompiler.GetLZMAExeFilename(const Allow64Bit: Boolean): String;
const
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  ExeFilenames: array[Boolean] of String = ('islzma32.exe', 'islzma64.exe');
var
  UseX64Exe: Boolean;
  GetNativeSystemInfoFunc: procedure(var lpSystemInfo: TSystemInfo); stdcall;
  SysInfo: TSystemInfo;
begin
  UseX64Exe := False;
  if Allow64Bit then begin
    GetNativeSystemInfoFunc := GetProcAddress(GetModuleHandle(kernel32),
      'GetNativeSystemInfo');
    if Assigned(GetNativeSystemInfoFunc) then begin
      GetNativeSystemInfoFunc(SysInfo);
      if SysInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64 then
        UseX64Exe := True;
    end;
  end;
  Result := CompilerDir + ExeFilenames[UseX64Exe];
end;

function TSetupCompiler.GetOutputBaseFileName: String;
begin
  Result := OutputBaseFileName;
end;

function TSetupCompiler.GetOutputDir: String;
begin
  Result := OutputDir;
end;

function TSetupCompiler.GetPreprocIncludedFilenames: TStringList;
begin
  Result := PreprocIncludedFilenames;
end;

function TSetupCompiler.GetPreprocOutput: String;
begin
  Result := PreprocOutput;
end;

function TSetupCompiler.GetSlicesPerDisk: Longint;
begin
  Result := SlicesPerDisk;
end;

function TSetupCompiler.FilenameToFileIndex(const AFilename: String): Integer;
begin
  if not GotPrevFilename or (PathCompare(AFilename, PrevFilename) <> 0) then begin
    { AFilename is non-empty when an include file is being read or when the compiler is reading
      CustomMessages/LangOptions/Messages sections from a messages file. Since these sections don't
      generate debug entries we can treat an empty AFileName as the main script and a non-empty
      AFilename as an include file. This works even when command-line compilation is used. }
    if AFilename = '' then
      PrevFileIndex := -1
    else begin
      PrevFileIndex := PreprocIncludedFilenames.IndexOf(AFilename);
      if PrevFileIndex = -1 then
        AbortCompileFmt('Failed to find index of file (%s)', [AFilename]);
    end;
    PrevFilename := AFilename;
    GotPrevFilename := True;
  end;
  Result := PrevFileIndex;
end;

procedure TSetupCompiler.WriteDebugEntry(Kind: TDebugEntryKind; Index: Integer; StepOutMarker: Boolean = False);
var
  Rec: TDebugEntry;
begin
  Rec.FileIndex := FilenameToFileIndex(LineFilename);
  Rec.LineNumber := LineNumber;
  Rec.Kind := Ord(Kind);
  Rec.Index := Index;
  Rec.StepOutMarker := StepOutMarker;
  DebugInfo.WriteBuffer(Rec, SizeOf(Rec));
  Inc(DebugEntryCount);
end;

procedure TSetupCompiler.WriteCompiledCodeText(const CompiledCodeText: AnsiString);
begin
  CompiledCodeTextLength := Length(CompiledCodeText);
  CodeDebugInfo.WriteBuffer(CompiledCodeText[1], CompiledCodeTextLength);
end;

procedure TSetupCompiler.WriteCompiledCodeDebugInfo(const CompiledCodeDebugInfo: AnsiString);
begin
  CompiledCodeDebugInfoLength := Length(CompiledCodeDebugInfo);
  CodeDebugInfo.WriteBuffer(CompiledCodeDebugInfo[1], CompiledCodeDebugInfoLength);
end;

procedure TSetupCompiler.ShiftDebugEntryIndexes(AKind: TDebugEntryKind);
{ Increments the Index field of each debug entry of the specified kind by 1.
  This has to be called when a new entry is inserted at the *front* of an
  *Entries array, since doing that causes the indexes of existing entries to
  shift. }
var
  Rec: PDebugEntry;
  I: Integer;
begin
  Cardinal(Rec) := Cardinal(DebugInfo.Memory) + SizeOf(TDebugInfoHeader);
  for I := 0 to DebugEntryCount-1 do begin
    if Rec.Kind = Ord(AKind) then
      Inc(Rec.Index);
    Inc(Rec);
  end;
end;

procedure TSetupCompiler.DoCallback(const Code: Integer;
  var Data: TCompilerCallbackData; const IgnoreCallbackResult: Boolean);
begin
  case CallbackProc(Code, Data, AppData) of
    iscrSuccess: ;
    iscrRequestAbort: if not IgnoreCallbackResult then Abort;
  else
    AbortCompile('CallbackProc return code invalid');
  end;
end;

procedure TSetupCompiler.CallIdleProc(const IgnoreCallbackResult: Boolean);
const
  ProgressMax = 1024;
var
  Data: TCompilerCallbackData;
  MillisecondsElapsed: Cardinal;
  X: Integer64;
begin
  Data.SecondsRemaining := -1;
  Data.BytesCompressedPerSecond := 0;
  if ((BytesCompressedSoFar.Lo = 0) and (BytesCompressedSoFar.Hi = 0)) or
     ((TotalBytesToCompress.Lo = 0) and (TotalBytesToCompress.Hi = 0)) then begin
    { Optimization(?) and avoid division by zero when TotalBytesToCompress=0 }
    Data.CompressProgress := 0;
  end
  else begin
    Data.CompressProgress := Trunc((Comp(BytesCompressedSoFar) * ProgressMax) /
      Comp(TotalBytesToCompress));
    { In case one of the files got bigger since we checked the sizes... }
    if Data.CompressProgress > ProgressMax then
      Data.CompressProgress := ProgressMax;

    if CompressionInProgress then begin
      MillisecondsElapsed := GetTickCount - CompressionStartTick;
      if MillisecondsElapsed >= Cardinal(1000) then begin
        X := BytesCompressedSoFar;
        Mul64(X, 1000);
        Div64(X, MillisecondsElapsed);
        if (X.Hi = 0) and (Longint(X.Lo) >= 0) then
          Data.BytesCompressedPerSecond := X.Lo
        else
          Data.BytesCompressedPerSecond := Maxint;
        if Compare64(BytesCompressedSoFar, TotalBytesToCompress) < 0 then begin
          { Protect against division by zero }
          if Data.BytesCompressedPerSecond <> 0 then begin
            X := TotalBytesToCompress;
            Dec6464(X, BytesCompressedSoFar);
            Inc64(X, Data.BytesCompressedPerSecond-1);  { round up }
            Div64(X, Data.BytesCompressedPerSecond);
            if (X.Hi = 0) and (Longint(X.Lo) >= 0) then
              Data.SecondsRemaining := X.Lo
            else
              Data.SecondsRemaining := Maxint;
          end;
        end
        else begin
          { In case one of the files got bigger since we checked the sizes... }
          Data.SecondsRemaining := 0;
        end;
      end;
    end;
  end;
  Data.CompressProgressMax := ProgressMax;
  DoCallback(iscbNotifyIdle, Data, IgnoreCallbackResult);
end;

type
  PPreCompilerData = ^TPreCompilerData;
  TPreCompilerData = record
    Compiler: TSetupCompiler;
    MainScript: Boolean;
    InFiles: TStringList;
    OutLines: TScriptFileLines;
    AnsiConvertCodePage: Cardinal;
    CurInLine: String;
    ErrorSet: Boolean;
    ErrorMsg, ErrorFilename: String;
    ErrorLine, ErrorColumn: Integer;
    LastPrependDirNameResult: String;
  end;

procedure PreErrorProc(CompilerData: TPreprocCompilerData; ErrorMsg: PChar;
  ErrorFilename: PChar; ErrorLine: Integer; ErrorColumn: Integer); stdcall; forward;

function LoadFile(CompilerData: TPreprocCompilerData; AFilename: PChar;
  ErrorFilename: PChar; ErrorLine: Integer; ErrorColumn: Integer; FromPreProcessor: Boolean): TPreprocFileHandle;
var
  Data: PPreCompilerData;
  Filename: String;
  I: Integer;
  Lines: TLowFragStringList;
  F: TTextFileReader;
  L: String;
begin
  Data := CompilerData;
  Filename := AFilename;
  if Filename = '' then begin
    { Reject any attempt by the preprocessor to load the main script }
    PreErrorProc(CompilerData, 'Invalid parameter passed to PreLoadFileProc',
      ErrorFilename, ErrorLine, ErrorColumn);
    Result := -1;
    Exit;
  end;

  Filename := PathExpand(Filename);
  for I := 0 to Data.InFiles.Count-1 do
    if PathCompare(Data.InFiles[I], Filename) = 0 then begin
      Result := I;
      Exit;
    end;

  Lines := TLowFragStringList.Create;
  try
    if FromPreProcessor then begin
      Data.Compiler.AddStatus(Format(SCompilerStatusReadingInFile, [Filename]));
      if Data.MainScript then
        Data.Compiler.PreprocIncludedFilenames.Add(Filename);
    end;
    F := TTextFileReader.Create(Filename, fdOpenExisting, faRead, fsRead);
    try
      F.CodePage := Data.AnsiConvertCodePage;
      while not F.Eof do begin
         L := F.ReadLine;
        for I := 1 to Length(L) do
          if L[I] = #0 then
            raise Exception.CreateFmt(SCompilerIllegalNullChar, [Lines.Count + 1]);
        Lines.Add(L);
      end;
    finally
      F.Free;
    end;
  except
    Lines.Free;
    PreErrorProc(CompilerData, PChar(Format(SCompilerErrorOpeningIncludeFile,
      [Filename, GetExceptMessage])), ErrorFilename, ErrorLine, ErrorColumn);
    Result := -1;
    Exit;
  end;
  Result := Data.InFiles.AddObject(Filename, Lines);
end;

function PreLoadFileProc(CompilerData: TPreprocCompilerData; AFilename: PChar;
  ErrorFilename: PChar; ErrorLine: Integer; ErrorColumn: Integer): TPreprocFileHandle;
  stdcall;
begin
  Result := LoadFile(CompilerData, AFilename, ErrorFilename, ErrorLine, ErrorColumn, True);
end;

function PreLineInProc(CompilerData: TPreprocCompilerData;
  FileHandle: TPreprocFileHandle; LineIndex: Integer): PChar; stdcall;
var
  Data: PPreCompilerData;
  Lines: TLowFragStringList;
begin
  Data := CompilerData;
  if (FileHandle >= 0) and (FileHandle < Data.InFiles.Count) and
     (LineIndex >= 0) then begin
    Lines := TLowFragStringList(Data.InFiles.Objects[FileHandle]);
    if LineIndex < Lines.Count then begin
      Data.CurInLine := Lines[LineIndex];
      Result := PChar(Data.CurInLine);
    end
    else
      Result := nil;
  end
  else begin
    PreErrorProc(CompilerData, 'Invalid parameter passed to LineInProc',
      nil, 0, 0);
    Result := nil;
  end;
end;

procedure PreLineOutProc(CompilerData: TPreprocCompilerData;
  Filename: PChar; LineNumber: Integer; Text: PChar); stdcall;
var
  Data: PPreCompilerData;
begin
  Data := CompilerData;
  Data.OutLines.Add(Filename, LineNumber, Text);
end;

procedure PreStatusProc(CompilerData: TPreprocCompilerData;
  StatusMsg: PChar; Warning: BOOL); stdcall;
var
  Data: PPreCompilerData;
begin
  Data := CompilerData;
  Data.Compiler.AddStatus(Format(SCompilerStatusPreprocessorStatus, [StatusMsg]), Warning);
end;

procedure PreErrorProc(CompilerData: TPreprocCompilerData; ErrorMsg: PChar;
  ErrorFilename: PChar; ErrorLine: Integer; ErrorColumn: Integer); stdcall;
var
  Data: PPreCompilerData;
begin
  Data := CompilerData;
  if not Data.ErrorSet then begin
    Data.ErrorMsg := ErrorMsg;
    Data.ErrorFilename := ErrorFilename;
    Data.ErrorLine := ErrorLine;
    Data.ErrorColumn := ErrorColumn;
    Data.ErrorSet := True;
  end;
end;

function PrePrependDirNameProc(CompilerData: TPreprocCompilerData;
  Filename: PChar; Dir: PChar; ErrorFilename: PChar; ErrorLine: Integer;
  ErrorColumn: Integer): PChar; stdcall;
var
  Data: PPreCompilerData;
begin
  Data := CompilerData;
  try
    Data.LastPrependDirNameResult := Data.Compiler.PrependDirName(
      PChar(Filename), PChar(Dir));
    Result := PChar(Data.LastPrependDirNameResult);
  except
    PreErrorProc(CompilerData, PChar(GetExceptMessage), ErrorFilename,
      ErrorLine, ErrorColumn);
    Result := nil;
  end;
end;

procedure PreIdleProc(CompilerData: TPreprocCompilerData); stdcall;
var
  Data: PPreCompilerData;
begin
  Data := CompilerData;
  Data.Compiler.CallIdleProc(True); { Doesn't allow an Abort }
end;

function TSetupCompiler.ReadScriptFile(const Filename: String;
  const UseCache: Boolean; const AnsiConvertCodePage: Cardinal): TScriptFileLines;

  function ReadMainScriptLines: TLowFragStringList;
  var
    Reset: Boolean;
    Data: TCompilerCallbackData;
  begin
    Result := TLowFragStringList.Create;
    try
      Reset := True;
      while True do begin
        Data.Reset := Reset;
        Data.LineRead := nil;
        DoCallback(iscbReadScript, Data);
        if Data.LineRead = nil then
          Break;
        Result.Add(Data.LineRead);
        Reset := False;
      end;
    except
      Result.Free;
      raise;
    end;
  end;

  function SelectPreprocessor(const Lines: TLowFragStringList): TPreprocessScriptProc;
  var
    S: String;
  begin
    { Don't allow ISPPCC to be used if ISPP.dll is missing }
    if (PreprocOptionsString <> '') and not Assigned(PreprocessScriptProc) then
      raise Exception.Create(SCompilerISPPMissing);

    { By default, only pass the main script through ISPP }
    if (Filename = '') and Assigned(PreprocessScriptProc) then
      Result := PreprocessScriptProc
    else
      Result := BuiltinPreprocessScript;

    { Check for (and remove) #preproc override directive on the first line }
    if Lines.Count > 0 then begin
      S := Trim(Lines[0]);
      if S = '#preproc builtin' then begin
        Lines[0] := '';
        Result := BuiltinPreprocessScript;
      end
      else if S = '#preproc ispp' then begin
        Lines[0] := '';
        Result := PreprocessScriptProc;
        if not Assigned(Result) then
          raise Exception.Create(SCompilerISPPMissing);
      end;
    end;
  end;

  procedure PreprocessLines(const OutLines: TScriptFileLines);
  var
    LSourcePath, LCompilerPath: String;
    Params: TPreprocessScriptParams;
    Data: TPreCompilerData;
    FileLoaded: Boolean;
    ResultCode, CleanupResultCode, I: Integer;
    PreProc: TPreprocessScriptProc;
  begin
    LSourcePath := OriginalSourceDir;
    LCompilerPath := CompilerDir;
    FillChar(Params, SizeOf(Params), 0);
    Params.Size := SizeOf(Params);
    Params.InterfaceVersion := 3;
    Params.CompilerBinVersion := SetupBinVersion;
    Params.Filename := PChar(Filename);
    Params.SourcePath := PChar(LSourcePath);
    Params.CompilerPath := PChar(LCompilerPath);
    Params.Options := PChar(PreprocOptionsString);
    Params.CompilerData := @Data;
    Params.LoadFileProc := PreLoadFileProc;
    Params.LineInProc := PreLineInProc;
    Params.LineOutProc := PreLineOutProc;
    Params.StatusProc := PreStatusProc;
    Params.ErrorProc := PreErrorProc;
    Params.PrependDirNameProc := PrePrependDirNameProc;
    Params.IdleProc := PreIdleProc;

    FillChar(Data, SizeOf(Data), 0);
    Data.Compiler := Self;
    Data.OutLines := OutLines;
    Data.AnsiConvertCodePage := AnsiConvertCodePage;
    Data.InFiles := TStringList.Create;
    try
      if Filename = '' then begin
        Data.MainScript := True;
        Data.InFiles.AddObject('', ReadMainScriptLines);
        FileLoaded := True;
      end
      else
        FileLoaded := (LoadFile(Params.CompilerData, PChar(Filename),
          PChar(LineFilename), LineNumber, 0, False) = 0);

      ResultCode := ispePreprocessError;
      if FileLoaded then begin
        PreProc := SelectPreprocessor(TLowFragStringList(Data.InFiles.Objects[0]));
        if Filename = '' then
          AddStatus(SCompilerStatusPreprocessing);
        ResultCode := PreProc(Params);
        if Filename = '' then begin
          PreprocOutput := Data.Outlines.Text;
          { Defer cleanup of main script until after compilation }
          PreprocCleanupProcData := Params.PreprocCleanupProcData;
          PreprocCleanupProc := Params.PreprocCleanupProc;
        end
        else if Assigned(Params.PreprocCleanupProc) then begin
          CleanupResultCode := Params.PreprocCleanupProc(Params.PreprocCleanupProcData);
          if CleanupResultCode <> 0 then
            AbortCompileFmt('Preprocessor cleanup function for "%s" failed with code %d',
              [Filename, CleanupResultCode]);
        end;
      end;

      if Data.ErrorSet then begin
        LineFilename := Data.ErrorFilename;
        LineNumber := Data.ErrorLine;
        if Data.ErrorColumn > 0 then   { hack for now... }
          Insert(Format('Column %d:' + SNewLine, [Data.ErrorColumn]),
            Data.ErrorMsg, 1);
        AbortCompile(Data.ErrorMsg);
      end;
      case ResultCode of
        ispeSuccess: ;
        ispeSilentAbort: Abort;
      else
        AbortCompileFmt('Preprocess function failed with code %d', [ResultCode]);
      end;
    finally
      for I := Data.InFiles.Count-1 downto 0 do
        Data.InFiles.Objects[I].Free;
      Data.InFiles.Free;
    end;
  end;

var
  I: Integer;
  Lines: TScriptFileLines;
begin
  if UseCache then
    for I := 0 to ScriptFiles.Count-1 do
      if PathCompare(ScriptFiles[I], Filename) = 0 then begin
        Result := TScriptFileLines(ScriptFiles.Objects[I]);
        Exit;
      end;

  Lines := TScriptFileLines.Create;
  try
    PreprocessLines(Lines);
  except
    Lines.Free;
    raise;
  end;
  if UseCache then
    ScriptFiles.AddObject(Filename, Lines);
  Result := Lines;
end;

procedure TSetupCompiler.EnumIniSection(const EnumProc: TEnumIniSectionProc;
  const SectionName: String; const Ext: Integer; const Verbose, SkipBlankLines: Boolean;
  const Filename: String; const LangSection, LangSectionPre: Boolean);
var
  FoundSection: Boolean;
  LastSection: String;

  procedure DoFile(Filename: String);
  const
    PreCodePage = 1252;
  var
    UseCache: Boolean;
    AnsiConvertCodePage: Cardinal;
    Lines: TScriptFileLines;
    SaveLineFilename, L: String;
    SaveLineNumber, LineIndex, I: Integer;
    Line: PScriptFileLine;
  begin
    if Filename <> '' then
      Filename := PathExpand(PrependSourceDirName(Filename));

    UseCache := not (LangSection and LangSectionPre);
    AnsiConvertCodePage := 0;
    if LangSection then begin
      { During a Pre pass on an .isl file, use code page 1252 for translation.
        Previously, the system code page was used, but on DBCS that resulted in
        "Illegal null character" errors on files containing byte sequences that
        do not form valid lead/trail byte combinations (i.e. most languages). }
      if LangSectionPre then begin
        if not IsValidCodePage(PreCodePage) then  { just in case }
          AbortCompileFmt('Code page %u unsupported', [PreCodePage]);
        AnsiConvertCodePage := PreCodePage;
      end else if Ext >= 0 then begin
        { Ext = LangIndex, except for Default.isl for which its -2 when default
          messages are read but no special conversion is needed for those. }
        AnsiConvertCodePage := TPreLangData(PreLangDataList[Ext]).LanguageCodePage;
      end;
    end;

    Lines := ReadScriptFile(Filename, UseCache, AnsiConvertCodePage);
    try
      SaveLineFilename := LineFilename;
      SaveLineNumber := LineNumber;

      for LineIndex := 0 to Lines.Count-1 do begin
        Line := Lines[LineIndex];
        LineFilename := Line.LineFilename;
        LineNumber := Line.LineNumber;
        L := Trim(Line.LineText);
        { Check for blank lines or comments }
        if (not FoundSection or SkipBlankLines) and ((L = '') or (L[1] = ';')) then Continue;
        if (L <> '') and (L[1] = '[') then begin
          { Section tag }
          I := Pos(']', L);
          if (I < 3) or (I <> Length(L)) then
            AbortCompileOnLine(SCompilerSectionTagInvalid);
          L := Copy(L, 2, I-2);
          if L[1] = '/' then begin
            L := Copy(L, 2, Maxint);
            if (LastSection = '') or (CompareText(L, LastSection) <> 0) then
              AbortCompileOnLineFmt(SCompilerSectionBadEndTag, [L]);
            FoundSection := False;
            LastSection := '';
          end
          else begin
            FoundSection := (CompareText(L, SectionName) = 0);
            LastSection := L;
          end;
        end
        else begin
          if not FoundSection then begin
            if LastSection = '' then
              AbortCompileOnLine(SCompilerTextNotInSection);
            Continue;  { not on the right section }
          end;
          if Verbose then begin
            if LineFilename = '' then
              AddStatus(Format(SCompilerStatusParsingSectionLine,
                [SectionName, LineNumber]))
            else
              AddStatus(Format(SCompilerStatusParsingSectionLineFile,
                [SectionName, LineNumber, LineFilename]));
          end;
          EnumProc(PChar(Line.LineText), Ext);
        end;
      end;

      LineFilename := SaveLineFilename;
      LineNumber := SaveLineNumber;
    finally
      if not UseCache then
        Lines.Free;
    end;
  end;

begin
  FoundSection := False;
  LastSection := '';
  DoFile(Filename);
end;

procedure TSetupCompiler.ExtractParameters(S: PChar;
  const ParamInfo: array of TParamInfo; var ParamValues: array of TParamValue);

  function GetParamIndex(const AName: String): Integer;
  var
    I: Integer;
  begin
    for I := 0 to High(ParamInfo) do
      if CompareText(ParamInfo[I].Name, AName) = 0 then begin
        Result := I;
        if ParamValues[I].Found then
          AbortCompileParamError(SCompilerParamDuplicated, ParamInfo[I].Name);
        ParamValues[I].Found := True;
        Exit;
      end;
    { Unknown parameter }
    AbortCompileOnLineFmt(SCompilerParamUnknownParam, [AName]);
    Result := -1;
  end;

var
  I, ParamIndex: Integer;
  ParamName, Data: String;
begin
  for I := 0 to High(ParamValues) do begin
    ParamValues[I].Found := False;
    ParamValues[I].Data := '';
  end;

  while True do begin
    { Parameter name }
    SkipWhitespace(S);
    if S^ = #0 then
      Break;
    ParamName := ExtractWords(S, ':');
    ParamIndex := GetParamIndex(ParamName);
    if S^ <> ':' then
      AbortCompileOnLineFmt(SCompilerParamHasNoValue, [ParamName]);
    Inc(S);

    { Parameter value }
    SkipWhitespace(S);
    if S^ <> '"' then begin
      Data := ExtractWords(S, ';');
      if Pos('"', Data) <> 0 then
        AbortCompileOnLineFmt(SCompilerParamQuoteError, [ParamName]);
      if S^ = ';' then
        Inc(S);
    end
    else begin
      Inc(S);
      Data := '';
      while True do begin
        if S^ = #0 then
          AbortCompileOnLineFmt(SCompilerParamMissingClosingQuote, [ParamName]);
        if S^ = '"' then begin
          Inc(S);
          if S^ <> '"' then
            Break;
        end;
        Data := Data + S^;
        Inc(S);
      end;
      SkipWhitespace(S);
      case S^ of
        #0 : ;
        ';': Inc(S);
      else
        AbortCompileOnLineFmt(SCompilerParamQuoteError, [ParamName]);
      end;
    end;

    { Assign the data }
    if (piNoEmpty in ParamInfo[ParamIndex].Flags) and (Data = '') then
      AbortCompileParamError(SCompilerParamEmpty2, ParamInfo[ParamIndex].Name);
    if (piNoQuotes in ParamInfo[ParamIndex].Flags) and (Pos('"', Data) <> 0) then
      AbortCompileParamError(SCompilerParamNoQuotes2, ParamInfo[ParamIndex].Name);
    ParamValues[ParamIndex].Data := Data;
  end;

  { Check for missing required parameters }
  for I := 0 to High(ParamInfo) do begin
    if (piRequired in ParamInfo[I].Flags) and
       not ParamValues[I].Found then
      AbortCompileParamError(SCompilerParamNotSpecified, ParamInfo[I].Name);
  end;
end;

procedure TSetupCompiler.AddStatus(const S: String; const Warning: Boolean);
var
  Data: TCompilerCallbackData;
begin
  Data.StatusMsg := PChar(S);
  Data.Warning := Warning;
  DoCallback(iscbNotifyStatus, Data);
end;

procedure TSetupCompiler.AddStatusFmt(const Msg: String; const Args: array of const;
  const Warning: Boolean);
begin
  AddStatus(Format(Msg, Args), Warning);
end;

procedure TSetupCompiler.AbortCompile(const Msg: String);
begin
  raise EISCompileError.Create(Msg);
end;

procedure TSetupCompiler.AbortCompileFmt(const Msg: String; const Args: array of const);
begin
  AbortCompile(Format(Msg, Args));
end;

procedure TSetupCompiler.AbortCompileOnLine(const Msg: String);
{ AbortCompileOnLine is now equivalent to AbortCompile }
begin
  AbortCompile(Msg);
end;

procedure TSetupCompiler.AbortCompileOnLineFmt(const Msg: String;
  const Args: array of const);
begin
  AbortCompileOnLine(Format(Msg, Args));
end;

procedure TSetupCompiler.AbortCompileParamError(const Msg, ParamName: String);
begin
  AbortCompileOnLineFmt(Msg, [ParamName]);
end;

function TSetupCompiler.PrependDirName(const Filename, Dir: String): String;

  function GetShellFolderPathCached(const FolderID: Integer;
    var CachedDir: String): String;
  var
    S: String;
  begin
    if CachedDir = '' then begin
      S := GetShellFolderPath(FolderID);
      if S = '' then
        AbortCompileFmt('Failed to get shell folder path (0x%.4x)', [FolderID]);
      S := AddBackslash(PathExpand(S));
      CachedDir := S;
    end;
    Result := CachedDir;
  end;

const
  CSIDL_PERSONAL = $0005;
var
  P: Integer;
  Prefix: String;
begin
  P := PathPos(':', Filename);
  if (P = 0) or
     ((P = 2) and CharInSet(UpCase(Filename[1]), ['A'..'Z'])) then begin
    if (Filename = '') or not IsRelativePath(Filename) then
      Result := Filename
    else
      Result := Dir + Filename;
  end
  else begin
    Prefix := Copy(Filename, 1, P-1);
    if Prefix = 'compiler' then
      Result := CompilerDir + Copy(Filename, P+1, Maxint)
    else if Prefix = 'userdocs' then
      Result := GetShellFolderPathCached(CSIDL_PERSONAL, CachedUserDocsDir) +
        Copy(Filename, P+1, Maxint)
    else begin
      AbortCompileFmt(SCompilerUnknownFilenamePrefix, [Copy(Filename, 1, P)]);
      Result := Filename;  { avoid warning }
    end;
  end;
end;

function TSetupCompiler.PrependSourceDirName(const Filename: String): String;
begin
  Result := PrependDirName(Filename, SourceDir);
end;

procedure TSetupCompiler.RenamedConstantCallback(const Cnst, CnstRenamed: String);
begin
  if Pos('common', LowerCase(CnstRenamed)) <> 0 then
    WarningsList.Add(Format(SCompilerCommonConstantRenamed, [Cnst, CnstRenamed]))
  else
    WarningsList.Add(Format(SCompilerConstantRenamed, [Cnst, CnstRenamed]));
end;

function TSetupCompiler.CheckConst(const S: String; const MinVersion: TSetupVersionData;
  const AllowedConsts: TAllowedConsts): Boolean;
{ Returns True if S contains constants. Aborts compile if they are invalid. }

  function CheckEnvConst(C: String): Boolean;
  { based on ExpandEnvConst in Main.pas }
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
    if ConvertConstPercentStr(VarName) and ConvertConstPercentStr(Default) then begin
      CheckConst(VarName, MinVersion, AllowedConsts);
      CheckConst(Default, MinVersion, AllowedConsts);
      Result := True;
      Exit;
    end;
    { it will only reach here if there was a parsing error }
    Result := False;
  end;

  function CheckRegConst(C: String): Boolean;
  { based on ExpandRegConst in Main.pas }
  type
    TKeyNameConst = packed record
      KeyName: String;
      KeyConst: HKEY;
    end;
  const
    KeyNameConsts: array[0..5] of TKeyNameConst = (
      (KeyName: 'HKA';  KeyConst: HKEY_AUTO),
      (KeyName: 'HKCR'; KeyConst: HKEY_CLASSES_ROOT),
      (KeyName: 'HKCU'; KeyConst: HKEY_CURRENT_USER),
      (KeyName: 'HKLM'; KeyConst: HKEY_LOCAL_MACHINE),
      (KeyName: 'HKU';  KeyConst: HKEY_USERS),
      (KeyName: 'HKCC'; KeyConst: HKEY_CURRENT_CONFIG));
  var
    Z, Subkey, Value, Default: String;
    I, J, L: Integer;
    RootKey: HKEY;
  begin
    Delete(C, 1, 4);  { skip past 'reg:' }
    I := ConstPos('\', C);
    if I <> 0 then begin
      Z := Copy(C, 1, I-1);
      if Z <> '' then begin
        L := Length(Z);
        if L >= 2 then begin
          { Check for '32' or '64' suffix }
          if ((Z[L-1] = '3') and (Z[L] = '2')) or
             ((Z[L-1] = '6') and (Z[L] = '4')) then
            SetLength(Z, L-2);
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
              CheckConst(Subkey, MinVersion, AllowedConsts);
              CheckConst(Value, MinVersion, AllowedConsts);
              CheckConst(Default, MinVersion, AllowedConsts);
              Result := True;
              Exit;
            end;
          end;
        end;
      end;
    end;
    { it will only reach here if there was a parsing error }
    Result := False;
  end;

  function CheckIniConst(C: String): Boolean;
  { based on ExpandIniConst in Main.pas }
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
          if ConvertConstPercentStr(Filename) and ConvertConstPercentStr(Section) and
             ConvertConstPercentStr(Key) and ConvertConstPercentStr(Default) then begin
            CheckConst(Filename, MinVersion, AllowedConsts);
            CheckConst(Section, MinVersion, AllowedConsts);
            CheckConst(Key, MinVersion, AllowedConsts);
            CheckConst(Default, MinVersion, AllowedConsts);
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
    { it will only reach here if there was a parsing error }
    Result := False;
  end;

  function CheckParamConst(C: String): Boolean;
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
      CheckConst(Param, MinVersion, AllowedConsts);
      CheckConst(Default, MinVersion, AllowedConsts);
      Result := True;
      Exit;
    end;
    { it will only reach here if there was a parsing error }
    Result := False;
  end;

  function CheckCodeConst(C: String): Boolean;
  var
    Z, ScriptFunc, Param: String;
    I: Integer;
  begin
    Delete(C, 1, 5);  { skip past 'code:' }
    Z := C;
    I := ConstPos('|', Z);  { check for optional parameter }
    if I = 0 then
      I := Length(Z)+1;
    Param := Copy(Z, I+1, Maxint);
    SetLength(Z, I-1);
    ScriptFunc := Z;
    if ConvertConstPercentStr(ScriptFunc) and ConvertConstPercentStr(Param) then begin
      CheckConst(Param, MinVersion, AllowedConsts);
      CodeCompiler.AddExport(ScriptFunc, 'String @String', False, True, LineFileName, LineNumber);
      Result := True;
      Exit;
    end;
    { it will only reach here if there was a parsing error }
    Result := False;
  end;

  function CheckDriveConst(C: String): Boolean;
  begin
    Delete(C, 1, 6);  { skip past 'drive:' }
    if ConvertConstPercentStr(C) then begin
      CheckConst(C, MinVersion, AllowedConsts);
      Result := True;
      Exit;
    end;
    { it will only reach here if there was a parsing error }
    Result := False;
  end;

  function CheckCustomMessageConst(C: String): Boolean;
  var
    MsgName, Arg: String;
    I, ArgCount: Integer;
    Found: Boolean;
    LineInfo: TLineInfo;
  begin
    Delete(C, 1, 3);  { skip past 'cm:' }
    I := ConstPos(',', C);
    if I = 0 then
      MsgName := C
    else
      MsgName := Copy(C, 1, I-1);

    { Check each argument }
    ArgCount := 0;
    while I > 0 do begin
      if ArgCount >= 9 then begin
        { Can't have more than 9 arguments (%1 through %9) }
        Result := False;
        Exit;
      end;
      Delete(C, 1, I);
      I := ConstPos(',', C);
      if I = 0 then
        Arg := C
      else
        Arg := Copy(C, 1, I-1);
      if not ConvertConstPercentStr(Arg) then begin
        Result := False;
        Exit;
      end;
      CheckConst(Arg, MinVersion, AllowedConsts);
      Inc(ArgCount);
    end;

    Found := False;
    for I := 0 to ExpectedCustomMessageNames.Count-1 do begin
      if CompareText(ExpectedCustomMessageNames[I], MsgName) = 0 then begin
        Found := True;
        Break;
      end;
    end;
    if not Found then begin
      LineInfo := TLineInfo.Create;
      LineInfo.FileName := LineFileName;
      LineInfo.FileLineNumber := LineNumber;
      ExpectedCustomMessageNames.AddObject(MsgName, LineInfo);
    end;
    Result := True;
  end;

const
  UserConsts: array[0..0] of String = (
    'username');
  Consts: array[0..41] of String = (
    'src', 'srcexe', 'tmp', 'app', 'win', 'sys', 'sd', 'groupname', 'commonfonts',
    'commonpf', 'commonpf32', 'commonpf64', 'commoncf', 'commoncf32', 'commoncf64',
    'autopf', 'autopf32', 'autopf64', 'autocf', 'autocf32', 'autocf64',
    'computername', 'dao', 'cmd', 'wizardhwnd', 'sysuserinfoname', 'sysuserinfoorg',
    'userinfoname', 'userinfoorg', 'userinfoserial', 'uninstallexe',
    'language', 'syswow64', 'sysnative', 'log', 'dotnet11', 'dotnet20', 'dotnet2032',
    'dotnet2064', 'dotnet40', 'dotnet4032', 'dotnet4064');
  UserShellFolderConsts: array[0..13] of String = (
    'userdesktop', 'userstartmenu', 'userprograms', 'userstartup',
    'userappdata', 'userdocs', 'usertemplates', 'userfavorites', 'usersendto', 'userfonts',
    'localappdata', 'userpf', 'usercf', 'usersavedgames');
  ShellFolderConsts: array[0..16] of String = (
    'group', 'commondesktop', 'commonstartmenu', 'commonprograms', 'commonstartup',
    'commonappdata', 'commondocs', 'commontemplates',
    'autodesktop', 'autostartmenu', 'autoprograms', 'autostartup',
    'autoappdata', 'autodocs', 'autotemplates', 'autofavorites', 'autofonts');
  AllowedConstsNames: array[TAllowedConst] of String = (
    'olddata', 'break');
var
  I, Start, K: Integer;
  C: TAllowedConst;
  Cnst: String;
label 1;
begin
  Result := False;
  I := 1;
  while I <= Length(S) do begin
    if S[I] = '{' then begin
      if (I < Length(S)) and (S[I+1] = '{') then
        Inc(I)
      else begin
        Result := True;

        Start := I;
        { Find the closing brace, skipping over any embedded constants }
        I := SkipPastConst(S, I);
        if I = 0 then  { unclosed constant? }
          AbortCompileOnLineFmt(SCompilerUnterminatedConst, [Copy(S, Start+1, Maxint)]);
        Dec(I);  { 'I' now points to the closing brace }

        { Now check the constant }
        Cnst := Copy(S, Start+1, I-(Start+1));
        if Cnst <> '' then begin
          HandleRenamedConstants(Cnst, RenamedConstantCallback);
          if Cnst = '\' then
            goto 1;
          if Cnst[1] = '%' then begin
            if not CheckEnvConst(Cnst) then
              AbortCompileOnLineFmt(SCompilerBadEnvConst, [Cnst]);
            goto 1;
          end;
          if Copy(Cnst, 1, 4) = 'reg:' then begin
            if not CheckRegConst(Cnst) then
              AbortCompileOnLineFmt(SCompilerBadRegConst, [Cnst]);
            goto 1;
          end;
          if Copy(Cnst, 1, 4) = 'ini:' then begin
            if not CheckIniConst(Cnst) then
              AbortCompileOnLineFmt(SCompilerBadIniConst, [Cnst]);
            goto 1;
          end;
          if Copy(Cnst, 1, 6) = 'param:' then begin
            if not CheckParamConst(Cnst) then
              AbortCompileOnLineFmt(SCompilerBadParamConst, [Cnst]);
            goto 1;
          end;
          if Copy(Cnst, 1, 5) = 'code:' then begin
            if not CheckCodeConst(Cnst) then
              AbortCompileOnLineFmt(SCompilerBadCodeConst, [Cnst]);
            goto 1;
          end;
          if Copy(Cnst, 1, 6) = 'drive:' then begin
            if not CheckDriveConst(Cnst) then
              AbortCompileOnLineFmt(SCompilerBadDriveConst, [Cnst]);
            goto 1;
          end;
          if Copy(Cnst, 1, 3) = 'cm:' then begin
            if not CheckCustomMessageConst(Cnst) then
              AbortCompileOnLineFmt(SCompilerBadCustomMessageConst, [Cnst]);
            goto 1;
          end;
          for K := Low(UserConsts) to High(UserConsts) do
            if Cnst = UserConsts[K] then begin
              UsedUserAreas.Add(Cnst);
              goto 1;
            end;
          for K := Low(Consts) to High(Consts) do
            if Cnst = Consts[K] then
              goto 1;
          for K := Low(UserShellFolderConsts) to High(UserShellFolderConsts) do
            if Cnst = UserShellFolderConsts[K] then begin
              UsedUserAreas.Add(Cnst);
              goto 1;
            end;
          for K := Low(ShellFolderConsts) to High(ShellFolderConsts) do
            if Cnst = ShellFolderConsts[K] then
              goto 1;
          for C := Low(C) to High(C) do
            if Cnst = AllowedConstsNames[C] then begin
              if not(C in AllowedConsts) then
                AbortCompileOnLineFmt(SCompilerConstCannotUse, [Cnst]);
              goto 1;
            end;
         end;
         AbortCompileOnLineFmt(SCompilerUnknownConst, [Cnst]);

      1:{ Constant is OK }
      end;
    end;
    Inc(I);
  end;
end;

function TSetupCompiler.EvalCheckOrInstallIdentifier(Sender: TSimpleExpression;
  const Name: String; const Parameters: array of const): Boolean;
var
  IsCheck: Boolean;
  Decl: String;
  I: Integer;
begin
  IsCheck := Boolean(Sender.Tag);

  if IsCheck then
    Decl := 'Boolean'
  else
    Decl := '0';

  for I := Low(Parameters) to High(Parameters) do begin
    if Parameters[I].VType = vtUnicodeString then
      Decl := Decl + ' @String'
    else if Parameters[I].VType = vtInteger then
      Decl := Decl + ' @LongInt'
    else if Parameters[I].VType = vtBoolean then
      Decl := Decl + ' @Boolean'
    else
      raise Exception.Create('Internal Error: unknown parameter type');
  end;

  CodeCompiler.AddExport(Name, Decl, False, True, LineFileName, LineNumber);

  Result := True; { Result doesn't matter }
end;

procedure TSetupCompiler.CheckCheckOrInstall(const ParamName, ParamData: String;
  const Kind: TCheckOrInstallKind);
var
  SimpleExpression: TSimpleExpression;
  IsCheck, BoolResult: Boolean;
begin
  if ParamData <> '' then begin
    if (Kind <> cikDirectiveCheck) or not TryStrToBoolean(ParamData, BoolResult) then begin
      IsCheck := Kind in [cikCheck, cikDirectiveCheck];
      { Check the expression in ParamData and add exports while
        evaluating. Use non-Lazy checking to make sure everything is evaluated. }
      try
        SimpleExpression := TSimpleExpression.Create;
        try
          SimpleExpression.Lazy := False;
          SimpleExpression.Expression := ParamData;
          SimpleExpression.OnEvalIdentifier := EvalCheckOrInstallIdentifier;
          SimpleExpression.SilentOrAllowed := False;
          SimpleExpression.SingleIdentifierMode := not IsCheck;
          SimpleExpression.ParametersAllowed := True;
          SimpleExpression.Tag := Integer(IsCheck);
          SimpleExpression.Eval;
        finally
          SimpleExpression.Free;
        end;
      except
        AbortCompileOnLineFmt(SCompilerExpressionError, [ParamName,
          GetExceptMessage]);
      end;
    end;
  end
  else begin
    if Kind = cikDirectiveCheck then
      AbortCompileOnLineFmt(SCompilerEntryInvalid2, ['Setup', ParamName]); 
  end;
end;

function ExtractFlag(var S: String; const FlagStrs: array of PChar): Integer;
var
  I: Integer;
  F: String;
begin
  F := ExtractStr(S, ' ');
  if F = '' then begin
    Result := -2;
    Exit;
  end;

  Result := -1;
  for I := 0 to High(FlagStrs) do
    if StrIComp(FlagStrs[I], PChar(F)) = 0 then begin
      Result := I;
      Break;
    end;
end;

function ExtractType(var S: String; const TypeEntries: TList): Integer;
var
  I: Integer;
  F: String;
begin
  F := ExtractStr(S, ' ');
  if F = '' then begin
    Result := -2;
    Exit;
  end;

  Result := -1;
  if TypeEntries.Count <> 0 then begin
    for I := 0 to TypeEntries.Count-1 do
      if CompareText(PSetupTypeEntry(TypeEntries[I]).Name, F) = 0 then begin
        Result := I;
        Break;
      end;
  end else begin
    for I := 0 to High(DefaultTypeEntryNames) do
      if StrIComp(DefaultTypeEntryNames[I], PChar(F)) = 0 then begin
        Result := I;
        Break;
      end;
  end;
end;

function ExtractLangIndex(SetupCompiler: TSetupCompiler; var S: String;
  const LanguageEntryIndex: Integer; const Pre: Boolean): Integer;
var
  I: Integer;
begin
  if LanguageEntryIndex = -1 then begin
    { Message in the main script }
    I := Pos('.', S);
    if I = 0 then begin
      { No '.'; apply to all languages }
      Result := -1;
    end
    else begin
      { Apply to specified language }
      Result := SetupCompiler.FindLangEntryIndexByName(Copy(S, 1, I-1), Pre);
      S := Copy(S, I+1, Maxint);
    end;
  end
  else begin
    { Inside a language file }
    if Pos('.', S) <> 0 then
      SetupCompiler.AbortCompileOnLine(SCompilerCantSpecifyLanguage);
    Result := LanguageEntryIndex;
  end;
end;

function TSetupCompiler.EvalArchitectureIdentifier(Sender: TSimpleExpression;
  const Name: String; const Parameters: array of const): Boolean;
const
  ArchIdentifiers: array[0..8] of String = (
    'arm32compatible', 'arm64', 'win64',
    'x64', 'x64os', 'x64compatible',
    'x86', 'x86os', 'x86compatible');
begin
  for var ArchIdentifier in ArchIdentifiers do begin
    if Name = ArchIdentifier then begin
      if ArchIdentifier = 'x64' then
        WarningsList.Add(Format(SCompilerArchitectureIdentifierDeprecatedWarning, ['x64', 'x64os', 'x64compatible']));
      Exit(True); { Result doesn't matter }
    end;
  end;

  raise Exception.CreateFmt(SCompilerArchitectureIdentifierInvalid, [Name]);
end;

{ Sets the Used properties while evaluating }
function TSetupCompiler.EvalComponentIdentifier(Sender: TSimpleExpression; const Name: String;
  const Parameters: array of const): Boolean;
var
  Found: Boolean;
  ComponentEntry: PSetupComponentEntry;
  I: Integer;
begin
  Found := False;
  for I := 0 to ComponentEntries.Count-1 do begin
    ComponentEntry := PSetupComponentEntry(ComponentEntries[I]);
    if CompareText(ComponentEntry.Name, Name) = 0 then begin
      ComponentEntry.Used := True;
      Found := True;
      { Don't Break; there may be multiple components with the same name }
    end;
  end;
  if not Found then
    raise Exception.CreateFmt(SCompilerParamUnknownComponent, [ParamCommonComponents]);
  Result := True;  { Result doesn't matter }
end;

{ Sets the Used properties while evaluating }
function TSetupCompiler.EvalTaskIdentifier(Sender: TSimpleExpression; const Name: String;
  const Parameters: array of const): Boolean;
var
  Found: Boolean;
  TaskEntry: PSetupTaskEntry;
  I: Integer;
begin
  Found := False;
  for I := 0 to TaskEntries.Count-1 do begin
    TaskEntry := PSetupTaskEntry(TaskEntries[I]);
    if CompareText(TaskEntry.Name, Name) = 0 then begin
      TaskEntry.Used := True;
      Found := True;
      { Don't Break; there may be multiple tasks with the same name }
    end;
  end;
  if not Found then
    raise Exception.CreateFmt(SCompilerParamUnknownTask, [ParamCommonTasks]);
  Result := True;  { Result doesn't matter }
end;

function TSetupCompiler.EvalLanguageIdentifier(Sender: TSimpleExpression; const Name: String;
  const Parameters: array of const): Boolean;
var
  LanguageEntry: PSetupLanguageEntry;
  I: Integer;
begin
  for I := 0 to LanguageEntries.Count-1 do begin
    LanguageEntry := PSetupLanguageEntry(LanguageEntries[I]);
    if CompareText(LanguageEntry.Name, Name) = 0 then begin
      Result := True; { Result doesn't matter }
      Exit;
    end;
  end;
  raise Exception.CreateFmt(SCompilerParamUnknownLanguage, [ParamCommonLanguages]);
end;

procedure TSetupCompiler.ProcessExpressionParameter(const ParamName,
  ParamData: String; OnEvalIdentifier: TSimpleExpressionOnEvalIdentifier;
  SlashConvert: Boolean; var ProcessedParamData: String);
var
  SimpleExpression: TSimpleExpression;
begin
  ProcessedParamData := Trim(ParamData);

  if ProcessedParamData <> '' then begin
    if SlashConvert then
      StringChange(ProcessedParamData, '/', '\');
    { Check the expression in ParamData. Use non-Lazy checking to make sure
      everything is evaluated. }
    try
      SimpleExpression := TSimpleExpression.Create;
      try
        SimpleExpression.Lazy := False;
        SimpleExpression.Expression := ProcessedParamData;
        SimpleExpression.OnEvalIdentifier := OnEvalIdentifier;
        SimpleExpression.SilentOrAllowed := True;
        SimpleExpression.SingleIdentifierMode := False;
        SimpleExpression.ParametersAllowed := False;
        SimpleExpression.Eval;
      finally
        SimpleExpression.Free;
      end;
    except
      AbortCompileOnLineFmt(SCompilerExpressionError, [ParamName,
        GetExceptMessage]);
    end;
  end;
end;

procedure TSetupCompiler.ProcessWildcardsParameter(const ParamData: String;
  const AWildcards: TStringList; const TooLongMsg: String);
var
  S, AWildcard: String;
begin
  S := PathLowercase(ParamData);
  while True do begin
    AWildcard := ExtractStr(S, ',');
    if AWildcard = '' then
      Break;
    { Impose a reasonable limit on the length of the string so
      that WildcardMatch can't overflow the stack }
    if Length(AWildcard) >= MAX_PATH then
      AbortCompileOnLine(TooLongMsg);
    AWildcards.Add(AWildcard);
  end;
end;

procedure TSetupCompiler.ProcessMinVersionParameter(const ParamValue: TParamValue;
  var AMinVersion: TSetupVersionData);
begin
  if ParamValue.Found then
    if not StrToSetupVersionData(ParamValue.Data, AMinVersion) then
      AbortCompileParamError(SCompilerParamInvalid2, ParamCommonMinVersion);
end;

procedure TSetupCompiler.ProcessOnlyBelowVersionParameter(const ParamValue: TParamValue;
  var AOnlyBelowVersion: TSetupVersionData);
begin
  if ParamValue.Found then begin
    if not StrToSetupVersionData(ParamValue.Data, AOnlyBelowVersion) then
      AbortCompileParamError(SCompilerParamInvalid2, ParamCommonOnlyBelowVersion);
    if (AOnlyBelowVersion.NTVersion <> 0) and
       (AOnlyBelowVersion.NTVersion <= $06010000) then
      WarningsList.Add(Format(SCompilerOnlyBelowVersionParameterNTTooLowWarning, ['6.1']));
  end;
end;

procedure TSetupCompiler.ProcessPermissionsParameter(ParamData: String;
  const AccessMasks: array of TNameAndAccessMask; var PermissionsEntry: Smallint);

  procedure GetSidFromName(const AName: String; var ASid: TGrantPermissionSid);
  type
    TKnownSid = record
      Name: String;
      Sid: TGrantPermissionSid;
    end;
  const
    SECURITY_WORLD_SID_AUTHORITY = 1;
    SECURITY_WORLD_RID = $00000000;
    SECURITY_CREATOR_SID_AUTHORITY = 3;
    SECURITY_CREATOR_OWNER_RID = $00000000;
    SECURITY_NT_AUTHORITY = 5;
    SECURITY_AUTHENTICATED_USER_RID = $0000000B;
    SECURITY_LOCAL_SYSTEM_RID = $00000012;
    SECURITY_LOCAL_SERVICE_RID = $00000013;
    SECURITY_NETWORK_SERVICE_RID = $00000014;
    SECURITY_BUILTIN_DOMAIN_RID = $00000020;
    DOMAIN_ALIAS_RID_ADMINS = $00000220;
    DOMAIN_ALIAS_RID_USERS = $00000221;
    DOMAIN_ALIAS_RID_GUESTS = $00000222;
    DOMAIN_ALIAS_RID_POWER_USERS = $00000223;
    DOMAIN_ALIAS_RID_IIS_IUSRS = $00000238;
    KnownSids: array[0..10] of TKnownSid = (
      (Name: 'admins';
       Sid: (Authority: (Value: (0, 0, 0, 0, 0, SECURITY_NT_AUTHORITY));
             SubAuthCount: 2;
             SubAuth: (SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS))),
      (Name: 'authusers';
       Sid: (Authority: (Value: (0, 0, 0, 0, 0, SECURITY_NT_AUTHORITY));
             SubAuthCount: 1;
             SubAuth: (SECURITY_AUTHENTICATED_USER_RID, 0))),
      (Name: 'creatorowner';
       Sid: (Authority: (Value: (0, 0, 0, 0, 0, SECURITY_CREATOR_SID_AUTHORITY));
             SubAuthCount: 1;
             SubAuth: (SECURITY_CREATOR_OWNER_RID, 0))),
      (Name: 'everyone';
       Sid: (Authority: (Value: (0, 0, 0, 0, 0, SECURITY_WORLD_SID_AUTHORITY));
             SubAuthCount: 1;
             SubAuth: (SECURITY_WORLD_RID, 0))),
      (Name: 'guests';
       Sid: (Authority: (Value: (0, 0, 0, 0, 0, SECURITY_NT_AUTHORITY));
             SubAuthCount: 2;
             SubAuth: (SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_GUESTS))),
      (Name: 'iisiusrs';
       Sid: (Authority: (Value: (0, 0, 0, 0, 0, SECURITY_NT_AUTHORITY));
             SubAuthCount: 2;
             SubAuth: (SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_IIS_IUSRS))),
      (Name: 'networkservice';
       Sid: (Authority: (Value: (0, 0, 0, 0, 0, SECURITY_NT_AUTHORITY));
             SubAuthCount: 1;
             SubAuth: (SECURITY_NETWORK_SERVICE_RID, 0))),
      (Name: 'powerusers';
       Sid: (Authority: (Value: (0, 0, 0, 0, 0, SECURITY_NT_AUTHORITY));
             SubAuthCount: 2;
             SubAuth: (SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_POWER_USERS))),
      (Name: 'service';
       Sid: (Authority: (Value: (0, 0, 0, 0, 0, SECURITY_NT_AUTHORITY));
             SubAuthCount: 1;
             SubAuth: (SECURITY_LOCAL_SERVICE_RID, 0))),
      (Name: 'system';
       Sid: (Authority: (Value: (0, 0, 0, 0, 0, SECURITY_NT_AUTHORITY));
             SubAuthCount: 1;
             SubAuth: (SECURITY_LOCAL_SYSTEM_RID, 0))),
      (Name: 'users';
       Sid: (Authority: (Value: (0, 0, 0, 0, 0, SECURITY_NT_AUTHORITY));
             SubAuthCount: 2;
             SubAuth: (SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_USERS)))
    );
  var
    I: Integer;
  begin
    for I := Low(KnownSids) to High(KnownSids) do
      if CompareText(AName, KnownSids[I].Name) = 0 then begin
        ASid := KnownSids[I].Sid;
        Exit;
      end;
    AbortCompileOnLineFmt(SCompilerPermissionsUnknownSid, [AName]);
  end;

  procedure GetAccessMaskFromName(const AName: String; var AAccessMask: DWORD);
  var
    I: Integer;
  begin
    for I := Low(AccessMasks) to High(AccessMasks) do
      if CompareText(AName, AccessMasks[I].Name) = 0 then begin
        AAccessMask := AccessMasks[I].Mask;
        Exit;
      end;
    AbortCompileOnLineFmt(SCompilerPermissionsUnknownMask, [AName]);
  end;

var
  Perms, E: AnsiString;
  S: String;
  PermsCount, P, I: Integer;
  Entry: TGrantPermissionEntry;
  NewPermissionEntry: PSetupPermissionEntry;
begin
  { Parse }
  PermsCount := 0;
  while True do begin
    S := ExtractStr(ParamData, ' ');
    if S = '' then
      Break;
    P := Pos('-', S);
    if P = 0 then
      AbortCompileOnLineFmt(SCompilerPermissionsInvalidValue, [S]);
    FillChar(Entry, SizeOf(Entry), 0);
    GetSidFromName(Copy(S, 1, P-1), Entry.Sid);
    GetAccessMaskFromName(Copy(S, P+1, Maxint), Entry.AccessMask);
    SetString(E, PAnsiChar(@Entry), SizeOf(Entry));
    Perms := Perms + E;
    Inc(PermsCount);
    if PermsCount > MaxGrantPermissionEntries then
      AbortCompileOnLineFmt(SCompilerPermissionsValueLimitExceeded, [MaxGrantPermissionEntries]);
  end;

  if Perms = '' then begin
    { No permissions }
    PermissionsEntry := -1;
  end
  else begin
    { See if there's already an identical permissions entry }
    for I := 0 to PermissionEntries.Count-1 do
      if PSetupPermissionEntry(PermissionEntries[I]).Permissions = Perms then begin
        PermissionsEntry := I;
        Exit;
      end;
    { If not, create a new one }
    PermissionEntries.Expand;
    NewPermissionEntry := AllocMem(SizeOf(NewPermissionEntry^));
    NewPermissionEntry.Permissions := Perms;
    I := PermissionEntries.Add(NewPermissionEntry);
    if I > High(PermissionsEntry) then
      AbortCompileOnLine(SCompilerPermissionsTooMany);
    PermissionsEntry := I;
  end;
end;

procedure TSetupCompiler.ReadTextFile(const Filename: String; const LangIndex: Integer;
  var Text: AnsiString);
var
  F: TFile;
  Size: Cardinal;
  UnicodeFile, RTFFile: Boolean;
  AnsiConvertCodePage: Integer;
  S: RawByteString;
  U: String;
begin
  try
    F := TFile.Create(Filename, fdOpenExisting, faRead, fsRead);
    try
      Size := F.Size.Lo;
      SetLength(S, Size);
      F.ReadBuffer(S[1], Size);

      UnicodeFile := ((Size >= 2) and (PWord(Pointer(S))^ = $FEFF)) or
                     ((Size >= 3) and (S[1] = #$EF) and (S[2] = #$BB) and (S[3] = #$BF));
      RTFFile := Copy(S, 1, 6) = '{\rtf1';

      if not UnicodeFile and not RTFFile and IsUTF8String(S) then begin
        S := #$EF + #$BB + #$BF + S;
        UnicodeFile := True;
      end;

      if not UnicodeFile and not RTFFile and (LangIndex >= 0) then begin
        AnsiConvertCodePage := TPreLangData(PreLangDataList[LangIndex]).LanguageCodePage;
        if AnsiConvertCodePage <> 0 then begin
          AddStatus(Format(SCompilerStatusConvertCodePage , [AnsiConvertCodePage]));
          { Convert the ANSI text to Unicode. }
          SetCodePage(S, AnsiConvertCodePage, False);
          U := String(S);
          { Store the Unicode text in Text with a UTF16 BOM. }
          Size := Length(U)*SizeOf(U[1]);
          SetLength(Text, Size+2);
          PWord(Pointer(Text))^ := $FEFF;
          Move(U[1], Text[3], Size);
        end else
          Text := S;
      end else
        Text := S;
    finally
      F.Free;
    end;
  except
    raise Exception.CreateFmt(SCompilerReadError, [Filename, GetExceptMessage]);
  end;
end;

{ Note: result Value may include leading/trailing whitespaces if it was quoted! }
procedure TSetupCompiler.SeparateDirective(const Line: PChar;
  var Key, Value: String);
var
  P: PChar;
begin
  Key := '';
  Value := '';
  P := Line;
  SkipWhitespace(P);
  if P^ <> #0 then begin
    Key := ExtractWords(P, '=');
    if Key = '' then
      AbortCompileOnLine(SCompilerDirectiveNameMissing);
    if P^ <> '=' then
      AbortCompileOnLineFmt(SCompilerDirectiveHasNoValue, [Key]);
    Inc(P);
    SkipWhitespace(P);
    Value := ExtractWords(P, #0);
    { If Value is surrounded in quotes, remove them. Note that unlike parameter
      values, for backward compatibility we don't require embedded quotes to be
      doubled, nor do we require surrounding quotes when there's a quote in
      the middle of the value. Does *not* remove whitespace after removing quotes! }
    if (Length(Value) >= 2) and
       (Value[1] = '"') and (Value[Length(Value)] = '"') then
      Value := Copy(Value, 2, Length(Value)-2);
  end;
end;

procedure TSetupCompiler.SetBytesCompressedSoFar(const Value: Integer64);
begin
  BytesCompressedSoFar := Value;
end;

procedure TSetupCompiler.SetOutput(Value: Boolean);
begin
  Output := Value;
  FixedOutput := True;
end;

procedure TSetupCompiler.SetOutputBaseFilename(const Value: String);
begin
  OutputBaseFilename := Value;
  FixedOutputBaseFilename := True;
end;

procedure TSetupCompiler.SetOutputDir(const Value: String);
begin
  OutputDir := Value;
  FixedOutputDir := True;
end;

procedure TSetupCompiler.EnumSetupProc(const Line: PChar; const Ext: Integer);
var
  KeyName, Value: String;
  I: Integer;
  Directive: TSetupSectionDirective;

  procedure Invalid;
  begin
    AbortCompileOnLineFmt(SCompilerEntryInvalid2, ['Setup', KeyName]);
  end;

  function StrToBool(const S: String): Boolean;
  begin
    Result := False;
    if not TryStrToBoolean(S, Result) then
      Invalid;
  end;

  function StrToIntRange(const S: String; const AMin, AMax: Integer): Integer;
  var
    E: Integer;
  begin
    Val(S, Result, E);
    if (E <> 0) or (Result < AMin) or (Result > AMax) then
      Invalid;
  end;

  procedure SetSetupHeaderOption(const Option: TSetupHeaderOption);
  begin
    if not StrToBool(Value) then
      Exclude(SetupHeader.Options, Option)
    else
      Include(SetupHeader.Options, Option);
  end;

  function ExtractNumber(var P: PChar): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to 3 do begin  { maximum of 4 digits }
      if not CharInSet(P^, ['0'..'9']) then begin
        if I = 0 then
          Invalid;
        Break;
      end;
      Result := (Result * 10) + (Ord(P^) - Ord('0'));
      Inc(P);
    end;
  end;

  procedure StrToTouchDate(const S: String);
  var
    P: PChar;
    Year, Month, Day: Integer;
    ST: TSystemTime;
    FT: TFileTime;
  begin
    if CompareText(S, 'current') = 0 then begin
      TouchDateOption := tdCurrent;
      Exit;
    end;
    if CompareText(S, 'none') = 0 then begin
      TouchDateOption := tdNone;
      Exit;
    end;

    P := PChar(S);
    Year := ExtractNumber(P);
    if (Year < 1980) or (Year > 2107) or (P^ <> '-') then
      Invalid;
    Inc(P);
    Month := ExtractNumber(P);
    if (Month < 1) or (Month > 12) or (P^ <> '-') then
      Invalid;
    Inc(P);
    Day := ExtractNumber(P);
    if (Day < 1) or (Day > 31) or (P^ <> #0) then
      Invalid;

    { Verify that the day is valid for the specified month & year }
    FillChar(ST, SizeOf(ST), 0);
    ST.wYear := Year;
    ST.wMonth := Month;
    ST.wDay := Day;
    if not SystemTimeToFileTime(ST, FT) then
      Invalid;

    TouchDateOption := tdExplicit;
    TouchDateYear := Year;
    TouchDateMonth := Month;
    TouchDateDay := Day;
  end;

  procedure StrToTouchTime(const S: String);
  var
    P: PChar;
    Hour, Minute, Second: Integer;
  begin
    if CompareText(S, 'current') = 0 then begin
      TouchTimeOption := ttCurrent;
      Exit;
    end;
    if CompareText(S, 'none') = 0 then begin
      TouchTimeOption := ttNone;
      Exit;
    end;

    P := PChar(S);
    Hour := ExtractNumber(P);
    if (Hour > 23) or (P^ <> ':') then
      Invalid;
    Inc(P);
    Minute := ExtractNumber(P);
    if Minute > 59 then
      Invalid;
    if P^ = #0 then
      Second := 0
    else begin
      if P^ <> ':' then
        Invalid;
      Inc(P);
      Second := ExtractNumber(P);
      if (Second > 59) or (P^ <> #0) then
        Invalid;
    end;

    TouchTimeOption := ttExplicit;
    TouchTimeHour := Hour;
    TouchTimeMinute := Minute;
    TouchTimeSecond := Second;
  end;

  function StrToPrivilegesRequiredOverrides(S: String): TSetupPrivilegesRequiredOverrides;
  const
    Overrides: array[0..1] of PChar = ('commandline', 'dialog');
  begin
    Result := [];
    while True do
      case ExtractFlag(S, Overrides) of
        -2: Break;
        -1: Invalid;
        0: Include(Result, proCommandLine);
        1: Result := Result + [proCommandLine, proDialog];
      end;
  end;

  procedure StrToPercentages(const S: String; var X, Y: Integer; const Min, Max: Integer);
  var
    I: Integer;
  begin
    I := Pos(',', S);
    if I = Length(S) then Invalid;
    if I <> 0 then begin
      X := StrToIntDef(Copy(S, 1, I-1), -1);
      Y := StrToIntDef(Copy(S, I+1, Maxint), -1);
    end else begin
      X := StrToIntDef(S, -1);
      Y := X;
    end;
    if (X < Min) or (X > Max) or (Y < Min) or (Y > Max) then
      Invalid;
  end;

var
  P: Integer;
  AIncludes: TStringList;
  SignTool, SignToolParams: String;
begin
  SeparateDirective(Line, KeyName, Value);

  if KeyName = '' then
    Exit;
  I := GetEnumValue(TypeInfo(TSetupSectionDirective), 'ss' + KeyName);
  if I = -1 then
    AbortCompileOnLineFmt(SCompilerUnknownDirective, ['Setup', KeyName]);
  Directive := TSetupSectionDirective(I);
  if (Directive <> ssSignTool) and (SetupDirectiveLines[Directive] <> 0) then
    AbortCompileOnLineFmt(SCompilerEntryAlreadySpecified, ['Setup', KeyName]);
  SetupDirectiveLines[Directive] := LineNumber;
  case Directive of
    ssAllowCancelDuringInstall: begin
        SetSetupHeaderOption(shAllowCancelDuringInstall);
      end;
    ssAllowNetworkDrive: begin
        SetSetupHeaderOption(shAllowNetworkDrive);
      end;
    ssAllowNoIcons: begin
        SetSetupHeaderOption(shAllowNoIcons);
      end;
    ssAllowRootDirectory: begin
        SetSetupHeaderOption(shAllowRootDirectory);
      end;
    ssAllowUNCPath: begin
        SetSetupHeaderOption(shAllowUNCPath);
      end;
    ssAlwaysRestart: begin
        SetSetupHeaderOption(shAlwaysRestart);
      end;
    ssAlwaysUsePersonalGroup: begin
        SetSetupHeaderOption(shAlwaysUsePersonalGroup);
      end;
    ssAlwaysShowComponentsList: begin
        SetSetupHeaderOption(shAlwaysShowComponentsList);
      end;
    ssAlwaysShowDirOnReadyPage: begin
        SetSetupHeaderOption(shAlwaysShowDirOnReadyPage);
      end;
    ssAlwaysShowGroupOnReadyPage: begin
        SetSetupHeaderOption(shAlwaysShowGroupOnReadyPage);
      end;
    ssAppCopyright: begin
        SetupHeader.AppCopyright := Value;
      end;
    ssAppComments: begin
        SetupHeader.AppComments := Value;
      end;
    ssAppContact: begin
        SetupHeader.AppContact := Value;
      end;
    ssAppendDefaultDirName: begin
        SetSetupHeaderOption(shAppendDefaultDirName);
      end;
    ssAppendDefaultGroupName: begin
        SetSetupHeaderOption(shAppendDefaultGroupName);
      end;
    ssAppId: begin
        if Value = '' then
          Invalid; 
        SetupHeader.AppId := Value;
      end;
    ssAppModifyPath: begin
        SetupHeader.AppModifyPath := Value;
      end;
    ssAppMutex: begin
        SetupHeader.AppMutex := Trim(Value);
      end;
    ssAppName: begin
        if Value = '' then
          Invalid;
        SetupHeader.AppName := Value;
      end;
    ssAppPublisher: begin
        SetupHeader.AppPublisher := Value;
      end;
    ssAppPublisherURL: begin
        SetupHeader.AppPublisherURL := Value;
      end;
    ssAppReadmeFile: begin
        SetupHeader.AppReadmeFile := Value;
      end;
    ssAppSupportPhone: begin
        SetupHeader.AppSupportPhone := Value;
      end;
    ssAppSupportURL: begin
        SetupHeader.AppSupportURL := Value;
      end;
    ssAppUpdatesURL: begin
        SetupHeader.AppUpdatesURL := Value;
      end;
    ssAppVerName: begin
        if Value = '' then
          Invalid;
        SetupHeader.AppVerName := Value;
      end;
    ssAppVersion: begin
        SetupHeader.AppVersion := Value;
      end;
    ssArchitecturesAllowed: begin
        ProcessExpressionParameter(KeyName, LowerCase(Value),
          EvalArchitectureIdentifier, False, SetupHeader.ArchitecturesAllowed);
      end;
    ssArchitecturesInstallIn64BitMode: begin
        ProcessExpressionParameter(KeyName, LowerCase(Value),
          EvalArchitectureIdentifier, False, SetupHeader.ArchitecturesInstallIn64BitMode);
      end;
    ssASLRCompatible: begin
        ASLRCompatible := StrToBool(Value);
      end;
    ssBackColor,
    ssBackColor2,
    ssBackColorDirection,
    ssBackSolid: begin
        WarningsList.Add(Format(SCompilerEntryObsolete, ['Setup', KeyName]));
      end;
    ssChangesAssociations: begin
        SetupHeader.ChangesAssociations := Value;
      end;
    ssChangesEnvironment: begin
        SetupHeader.ChangesEnvironment := Value;
      end;
    ssCloseApplications: begin
        if CompareText(Value, 'force') = 0 then begin
          Include(SetupHeader.Options, shCloseApplications);
          Include(SetupHeader.Options, shForceCloseApplications);
        end else begin
          SetSetupHeaderOption(shCloseApplications);
          Exclude(SetupHeader.Options, shForceCloseApplications);
        end;
      end;
    ssCloseApplicationsFilter: begin
        if Value = '' then
          Invalid;
        AIncludes := TStringList.Create;
        try
          ProcessWildcardsParameter(Value, AIncludes,
            Format(SCompilerDirectivePatternTooLong, ['CloseApplicationsFilter']));
          SetupHeader.CloseApplicationsFilter := StringsToCommaString(AIncludes);
        finally
          AIncludes.Free;
        end;
      end;
    ssCompression: begin
        Value := LowerCase(Trim(Value));
        if Value = 'none' then begin
          CompressMethod := cmStored;
          CompressLevel := 0;
        end
        else if Value = 'zip' then begin
          CompressMethod := cmZip;
          CompressLevel := 7;
        end
        else if Value = 'bzip' then begin
          CompressMethod := cmBzip;
          CompressLevel := 9;
        end
        else if Value = 'lzma' then begin
          CompressMethod := cmLZMA;
          CompressLevel := clLZMAMax;
        end
        else if Value = 'lzma2' then begin
          CompressMethod := cmLZMA2;
          CompressLevel := clLZMAMax;
        end
        else if Copy(Value, 1, 4) = 'zip/' then begin
          I := StrToIntDef(Copy(Value, 5, Maxint), -1);
          if (I < 1) or (I > 9) then
            Invalid;
          CompressMethod := cmZip;
          CompressLevel := I;
        end
        else if Copy(Value, 1, 5) = 'bzip/' then begin
          I := StrToIntDef(Copy(Value, 6, Maxint), -1);
          if (I < 1) or (I > 9) then
            Invalid;
          CompressMethod := cmBzip;
          CompressLevel := I;
        end
        else if Copy(Value, 1, 5) = 'lzma/' then begin
          if not LZMAGetLevel(Copy(Value, 6, Maxint), I) then
            Invalid;
          CompressMethod := cmLZMA;
          CompressLevel := I;
        end
        else if Copy(Value, 1, 6) = 'lzma2/' then begin
          if not LZMAGetLevel(Copy(Value, 7, Maxint), I) then
            Invalid;
          CompressMethod := cmLZMA2;
          CompressLevel := I;
        end
        else
          Invalid;
      end;
    ssCompressionThreads: begin
        if CompareText(Value, 'auto') = 0 then
          { do nothing; it's the default }
        else begin
          if StrToIntRange(Value, 1, 64) = 1 then begin
            InternalCompressProps.NumThreads := 1;
            CompressProps.NumThreads := 1;
          end;
        end;
      end;
    ssCreateAppDir: begin
        SetSetupHeaderOption(shCreateAppDir);
      end;
    ssCreateUninstallRegKey: begin
        SetupHeader.CreateUninstallRegKey := Value;
      end;
    ssDefaultDialogFontName: begin
        DefaultDialogFontName := Trim(Value);
      end;
    ssDefaultDirName: begin
        SetupHeader.DefaultDirName := Value;
      end;
    ssDefaultGroupName: begin
        SetupHeader.DefaultGroupName := Value;
      end;
    ssDefaultUserInfoName: begin
        SetupHeader.DefaultUserInfoName := Value;
      end;
    ssDefaultUserInfoOrg: begin
        SetupHeader.DefaultUserInfoOrg := Value;
      end;
    ssDefaultUserInfoSerial: begin
        SetupHeader.DefaultUserInfoSerial := Value;
      end;
    ssDEPCompatible: begin
        DEPCompatible := StrToBool(Value);
      end;
    ssDirExistsWarning: begin
        if CompareText(Value, 'auto') = 0 then
          SetupHeader.DirExistsWarning := ddAuto
        else if StrToBool(Value) then
          { ^ exception will be raised if Value is invalid }
          SetupHeader.DirExistsWarning := ddYes
        else
          SetupHeader.DirExistsWarning := ddNo;
      end;
    ssDisableDirPage: begin
        if CompareText(Value, 'auto') = 0 then
          SetupHeader.DisableDirPage := dpAuto
        else if StrToBool(Value) then
          { ^ exception will be raised if Value is invalid }
          SetupHeader.DisableDirPage := dpYes
        else
          SetupHeader.DisableDirPage := dpNo;
      end;
    ssDisableFinishedPage: begin
        SetSetupHeaderOption(shDisableFinishedPage);
      end;
    ssDisableProgramGroupPage: begin
        if CompareText(Value, 'auto') = 0 then
          SetupHeader.DisableProgramGroupPage := dpAuto
        else if StrToBool(Value) then
          { ^ exception will be raised if Value is invalid }
          SetupHeader.DisableProgramGroupPage := dpYes
        else
          SetupHeader.DisableProgramGroupPage := dpNo;
      end;
    ssDisableReadyMemo: begin
        SetSetupHeaderOption(shDisableReadyMemo);
      end;
    ssDisableReadyPage: begin
        SetSetupHeaderOption(shDisableReadyPage);
      end;
    ssDisableStartupPrompt: begin
        SetSetupHeaderOption(shDisableStartupPrompt);
      end;
    ssDisableWelcomePage: begin
        SetSetupHeaderOption(shDisableWelcomePage);
      end;
    ssDiskClusterSize: begin
        Val(Value, DiskClusterSize, I);
        if I <> 0 then
          Invalid;
        if (DiskClusterSize < 1) or (DiskClusterSize > 32768) then
          AbortCompileOnLine(SCompilerDiskClusterSizeInvalid);
      end;
    ssDiskSliceSize: begin
        if CompareText(Value, 'max') = 0 then
          DiskSliceSize := MaxDiskSliceSize
        else begin
          Val(Value, DiskSliceSize, I);
          if I <> 0 then
            Invalid;
          if (DiskSliceSize < 262144) or (DiskSliceSize > MaxDiskSliceSize) then
            AbortCompileFmt(SCompilerDiskSliceSizeInvalid, [262144, MaxDiskSliceSize]);
        end;
      end;
    ssDiskSpanning: begin
        DiskSpanning := StrToBool(Value);
      end;
    ssDontMergeDuplicateFiles: begin  { obsolete; superseded by "MergeDuplicateFiles" }
        if SetupDirectiveLines[ssMergeDuplicateFiles] = 0 then
          DontMergeDuplicateFiles := StrToBool(Value);
        WarningsList.Add(Format(SCompilerEntrySuperseded2, ['Setup', KeyName,
           'MergeDuplicateFiles']));
      end;
    ssEnableDirDoesntExistWarning: begin
        SetSetupHeaderOption(shEnableDirDoesntExistWarning);
      end;
    ssEncryption: begin
        SetSetupHeaderOption(shEncryptionUsed);
      end;
    ssEncryptionKeyDerivation: begin
        if Value = 'pbkdf2' then
          SetupHeader.EncryptionKDFIterations := 200000
        else if Copy(Value, 1, 7) = 'pbkdf2/' then begin
          I := StrToIntDef(Copy(Value, 8, Maxint), -1);
          if I < 1 then
            Invalid;
          SetupHeader.EncryptionKDFIterations := I;
        end else
          Invalid;
      end;
    ssExtraDiskSpaceRequired: begin
        if not StrToInteger64(Value, SetupHeader.ExtraDiskSpaceRequired) then
          Invalid;
      end;
    ssFlatComponentsList: begin
        SetSetupHeaderOption(shFlatComponentsList);
      end;
    ssInfoBeforeFile: begin
        InfoBeforeFile := Value;
      end;
    ssInfoAfterFile: begin
        InfoAfterFile := Value;
      end;
    ssInternalCompressLevel: begin
        Value := Trim(Value);
        if (Value = '0') or (CompareText(Value, 'none') = 0) then
          InternalCompressLevel := 0
        else if not LZMAGetLevel(Value, InternalCompressLevel) then
          Invalid;
      end;
    ssLanguageDetectionMethod: begin
        if CompareText(Value, 'uilanguage') = 0 then
          SetupHeader.LanguageDetectionMethod := ldUILanguage
        else if CompareText(Value, 'locale') = 0 then
          SetupHeader.LanguageDetectionMethod := ldLocale
        else if CompareText(Value, 'none') = 0 then
          SetupHeader.LanguageDetectionMethod := ldNone
        else
          Invalid;
      end;
    ssLicenseFile: begin
        LicenseFile := Value;
      end;
    ssLZMAAlgorithm: begin
        CompressProps.Algorithm := StrToIntRange(Value, 0, 1);
      end;
    ssLZMABlockSize: begin
        CompressProps.BlockSize := StrToIntRange(Value, 1024, 262144) * 1024; //search Lzma2Enc.c for kMaxSize to see this limit: 262144*1024==1<<28
      end;
    ssLZMADictionarySize: begin
        var MaxDictionarySize := 1024 shl 20; //1 GB - same as MaxDictionarySize in LZMADecomp.pas - lower than the LZMA SDK allows (search Lzma2Enc.c for kLzmaMaxHistorySize to see this limit: Cardinal(15 shl 28) = 3.8 GB) because Setup can't allocate that much memory
        CompressProps.DictionarySize := StrToIntRange(Value, 4, MaxDictionarySize div 1024) * 1024;
      end;
    ssLZMAMatchFinder: begin
        if CompareText(Value, 'BT') = 0 then
          I := 1
        else if CompareText(Value, 'HC') = 0 then
          I := 0
        else
          Invalid;
        CompressProps.BTMode := I;
      end;
    ssLZMANumBlockThreads: begin
        CompressProps.NumBlockThreads := StrToIntRange(Value, 1, 32);
      end;
    ssLZMANumFastBytes: begin
        CompressProps.NumFastBytes := StrToIntRange(Value, 5, 273);
      end;
    ssLZMAUseSeparateProcess: begin
        if CompareText(Value, 'x86') = 0 then
          CompressProps.WorkerProcessFilename := GetLZMAExeFilename(False)
        else if StrToBool(Value) then
          CompressProps.WorkerProcessFilename := GetLZMAExeFilename(True)
        else
          CompressProps.WorkerProcessFilename := '';
      end;
    ssMergeDuplicateFiles: begin
        DontMergeDuplicateFiles := not StrToBool(Value);
      end;
    ssMessagesFile: begin
        AbortCompileOnLine(SCompilerMessagesFileObsolete);
      end;
    ssMinVersion: begin
        if not StrToSetupVersionData(Value, SetupHeader.MinVersion) then
          Invalid;
        if SetupHeader.MinVersion.WinVersion <> 0 then
          AbortCompileOnLine(SCompilerMinVersionWinMustBeZero);
        if SetupHeader.MinVersion.NTVersion < $06010000 then
          AbortCompileOnLineFmt(SCompilerMinVersionNTTooLow, ['6.1']);
      end;
    ssMissingMessagesWarning: begin
        MissingMessagesWarning := StrToBool(Value);
      end;
    ssMissingRunOnceIdsWarning: begin
        MissingRunOnceIdsWarning := StrToBool(Value);
      end;
    ssOnlyBelowVersion: begin
        if not StrToSetupVersionData(Value, SetupHeader.OnlyBelowVersion) then
          Invalid;
        if (SetupHeader.OnlyBelowVersion.NTVersion <> 0) and
           (SetupHeader.OnlyBelowVersion.NTVersion <= $06010000) then
          AbortCompileOnLineFmt(SCompilerOnlyBelowVersionNTTooLow, ['6.1']);
      end;
    ssOutput: begin
        if not FixedOutput then
          Output := StrToBool(Value);
    end;
    ssOutputBaseFilename: begin
        if not FixedOutputBaseFilename then
          OutputBaseFilename := Value;
      end;
    ssOutputDir: begin
        if not FixedOutputDir then
          OutputDir := Value;
      end;
    ssOutputManifestFile: begin
        OutputManifestFile := Value;
      end;
    ssPassword: begin
        Password := Value;
      end;
    ssPrivilegesRequired: begin
        if CompareText(Value, 'none') = 0 then
          SetupHeader.PrivilegesRequired := prNone
        else if CompareText(Value, 'poweruser') = 0 then
          SetupHeader.PrivilegesRequired := prPowerUser
        else if CompareText(Value, 'admin') = 0 then
          SetupHeader.PrivilegesRequired := prAdmin
        else if CompareText(Value, 'lowest') = 0 then
          SetupHeader.PrivilegesRequired := prLowest
        else
          Invalid;
      end;
    ssPrivilegesRequiredOverridesAllowed: begin
        SetupHeader.PrivilegesRequiredOverridesAllowed := StrToPrivilegesRequiredOverrides(Value);
      end;
    ssReserveBytes: begin
        Val(Value, ReserveBytes, I);
        if (I <> 0) or (ReserveBytes < 0) then
          Invalid;
      end;
    ssRestartApplications: begin
        SetSetupHeaderOption(shRestartApplications);
      end;
    ssRestartIfNeededByRun: begin
        SetSetupHeaderOption(shRestartIfNeededByRun);
      end;
    ssSetupIconFile: begin
        SetupIconFilename := Value;
      end;
    ssSetupLogging: begin
        SetSetupHeaderOption(shSetupLogging);
      end;
    ssSetupMutex: begin
        SetupHeader.SetupMutex := Trim(Value);
      end;
    ssShowComponentSizes: begin
        SetSetupHeaderOption(shShowComponentSizes);
      end;
    ssShowLanguageDialog: begin
        if CompareText(Value, 'auto') = 0 then
          SetupHeader.ShowLanguageDialog := slAuto
        else if StrToBool(Value) then
          SetupHeader.ShowLanguageDialog := slYes
        else
          SetupHeader.ShowLanguageDialog := slNo;
      end;
    ssShowTasksTreeLines: begin
        SetSetupHeaderOption(shShowTasksTreeLines);
      end;
    ssShowUndisplayableLanguages: begin
        WarningsList.Add(Format(SCompilerEntryObsolete, ['Setup', KeyName]));
      end;
    ssSignedUninstaller: begin
        SetSetupHeaderOption(shSignedUninstaller);
      end;
    ssSignedUninstallerDir: begin
        if Value = '' then
          Invalid;
        SignedUninstallerDir := Value;
      end;
    ssSignTool: begin
        P := Pos(' ', Value);
        if (P <> 0) then begin
          SignTool := Copy(Value, 1, P-1);
          SignToolParams := Copy(Value, P+1, MaxInt);
        end else begin
          SignTool := Value;
          SignToolParams := '';
        end;
        if FindSignToolIndexByName(SignTool) = -1 then
          Invalid;
        SignTools.Add(SignTool);
        SignToolsParams.Add(SignToolParams);
      end;
    ssSignToolMinimumTimeBetween: begin
        I := StrToIntDef(Value, -1);
        if I < 0 then
          Invalid;
        SignToolMinimumTimeBetween := I;
      end;
    ssSignToolRetryCount: begin
        I := StrToIntDef(Value, -1);
        if I < 0 then
          Invalid;
        SignToolRetryCount := I;
      end;
    ssSignToolRetryDelay: begin
        I := StrToIntDef(Value, -1);
        if I < 0 then
          Invalid;
        SignToolRetryDelay := I;
      end;
    ssSignToolRunMinimized: begin
        SignToolRunMinimized := StrToBool(Value);
      end;
    ssSlicesPerDisk: begin
        I := StrToIntDef(Value, -1);
        if (I < 1) or (I > 26) then
          Invalid;
        SlicesPerDisk := I;
      end;
    ssSolidCompression: begin
        UseSolidCompression := StrToBool(Value);
      end;
    ssSourceDir: begin
        if Value = '' then
          Invalid;
        SourceDir := PrependDirName(Value, OriginalSourceDir);
      end;
    ssTerminalServicesAware: begin
        TerminalServicesAware := StrToBool(Value);
      end;
    ssTimeStampRounding: begin
        I := StrToIntDef(Value, -1);
        { Note: We can't allow really high numbers here because it gets
          multiplied by 10000000 }
        if (I < 0) or (I > 60) then
          Invalid;
        TimeStampRounding := I;
      end;
    ssTimeStampsInUTC: begin
        TimeStampsInUTC := StrToBool(Value);
      end;
    ssTouchDate: begin
        StrToTouchDate(Value);
      end;
    ssTouchTime: begin
        StrToTouchTime(Value);
      end;
    ssUpdateUninstallLogAppName: begin
        SetSetupHeaderOption(shUpdateUninstallLogAppName);
      end;
    ssUninstallable: begin
        SetupHeader.Uninstallable := Value;
      end;
    ssUninstallDisplayIcon: begin
        SetupHeader.UninstallDisplayIcon := Value;
      end;
    ssUninstallDisplayName: begin
        SetupHeader.UninstallDisplayName := Value;
      end;
    ssUninstallDisplaySize: begin
        if not StrToInteger64(Value, SetupHeader.UninstallDisplaySize) or
           ((SetupHeader.UninstallDisplaySize.Lo = 0) and (SetupHeader.UninstallDisplaySize.Hi = 0)) then
          Invalid;
      end;
    ssUninstallFilesDir: begin
        if Value = '' then
          Invalid;
        SetupHeader.UninstallFilesDir := Value;
      end;
    ssUninstallIconFile: begin
        WarningsList.Add(Format(SCompilerEntryObsolete, ['Setup', KeyName]));
      end;
    ssUninstallLogging: begin
        SetSetupHeaderOption(shUninstallLogging);
      end;
    ssUninstallLogMode: begin
        if CompareText(Value, 'append') = 0 then
          SetupHeader.UninstallLogMode := lmAppend
        else if CompareText(Value, 'new') = 0 then
          SetupHeader.UninstallLogMode := lmNew
        else if CompareText(Value, 'overwrite') = 0 then
          SetupHeader.UninstallLogMode := lmOverwrite
        else
          Invalid;
      end;
    ssUninstallRestartComputer: begin
        SetSetupHeaderOption(shUninstallRestartComputer);
      end;
    ssUninstallStyle: begin
        WarningsList.Add(Format(SCompilerEntryObsolete, ['Setup', KeyName]));
      end;
    ssUsePreviousAppDir: begin
        SetSetupHeaderOption(shUsePreviousAppDir);
      end;
    ssNotRecognizedMessagesWarning: begin
        NotRecognizedMessagesWarning := StrToBool(Value);
      end;
    ssUsedUserAreasWarning: begin
        UsedUserAreasWarning := StrToBool(Value);
      end;
    ssUsePreviousGroup: begin
        SetSetupHeaderOption(shUsePreviousGroup);
      end;
    ssUsePreviousLanguage: begin
        SetSetupHeaderOption(shUsePreviousLanguage);
      end;
    ssUsePreviousPrivileges: begin
        SetSetupHeaderOption(shUsePreviousPrivileges);
      end;
    ssUsePreviousSetupType: begin
        SetSetupHeaderOption(shUsePreviousSetupType);
      end;
    ssUsePreviousTasks: begin
        SetSetupHeaderOption(shUsePreviousTasks);
      end;
    ssUsePreviousUserInfo: begin
        SetSetupHeaderOption(shUsePreviousUserInfo);
      end;
    ssUseSetupLdr: begin
        UseSetupLdr := StrToBool(Value);
      end;
    ssUserInfoPage: begin
        SetSetupHeaderOption(shUserInfoPage);
      end;
    ssVersionInfoCompany: begin
        VersionInfoCompany := Value;
      end;
    ssVersionInfoCopyright: begin
        VersionInfoCopyright := Value;
      end;
    ssVersionInfoDescription: begin
        VersionInfoDescription := Value;
      end;
    ssVersionInfoOriginalFileName: begin
        VersionInfoOriginalFileName := Value;
      end;
    ssVersionInfoProductName: begin
        VersionInfoProductName := Value;
      end;
    ssVersionInfoProductVersion: begin
        VersionInfoProductVersionOriginalValue := Value;
        if not StrToVersionNumbers(Value, VersionInfoProductVersion) then
          Invalid;
      end;
    ssVersionInfoProductTextVersion: begin
        VersionInfoProductTextVersion := Value;
      end;
    ssVersionInfoTextVersion: begin
        VersionInfoTextVersion := Value;
      end;
    ssVersionInfoVersion: begin
        VersionInfoVersionOriginalValue := Value;
        if not StrToVersionNumbers(Value, VersionInfoVersion) then
          Invalid;
      end;
    ssWindowResizable,
    ssWindowShowCaption,
    ssWindowStartMaximized,
    ssWindowVisible: begin
        WarningsList.Add(Format(SCompilerEntryObsolete, ['Setup', KeyName]));
      end;
    ssWizardImageAlphaFormat: begin
        if CompareText(Value, 'none') = 0 then
          SetupHeader.WizardImageAlphaFormat := afIgnored
        else if CompareText(Value, 'defined') = 0 then
          SetupHeader.WizardImageAlphaFormat := afDefined
        else if CompareText(Value, 'premultiplied') = 0 then
          SetupHeader.WizardImageAlphaFormat := afPremultiplied
        else
          Invalid;
    end;
    ssWizardImageBackColor, ssWizardSmallImageBackColor: begin
        WarningsList.Add(Format(SCompilerEntryObsolete, ['Setup', KeyName]));
      end;
    ssWizardImageStretch: begin
        SetSetupHeaderOption(shWizardImageStretch);
      end;
    ssWizardImageFile: begin
        WizardImageFile := Value;
      end;
    ssWizardResizable: begin
        SetSetupHeaderOption(shWizardResizable);
      end;
    ssWizardSmallImageFile: begin
        WizardSmallImageFile := Value;
      end;
    ssWizardSizePercent: begin
        StrToPercentages(Value, SetupHeader.WizardSizePercentX,
          SetupHeader.WizardSizePercentY, 100, 150)
      end;
    ssWizardStyle: begin
        if CompareText(Value, 'classic') = 0 then
          SetupHeader.WizardStyle := wsClassic
        else if CompareText(Value, 'modern') = 0 then
          SetupHeader.WizardStyle := wsModern
        else
          Invalid;
      end;
  end;
end;

function TSetupCompiler.FindLangEntryIndexByName(const AName: String;
  const Pre: Boolean): Integer;
var
  I: Integer;
begin
  if Pre then begin
    for I := 0 to PreLangDataList.Count-1 do begin
      if TPreLangData(PreLangDataList[I]).Name = AName then begin
        Result := I;
        Exit;
      end;
    end;
    AbortCompileOnLineFmt(SCompilerUnknownLanguage, [AName]);
  end;

  for I := 0 to LanguageEntries.Count-1 do begin
    if PSetupLanguageEntry(LanguageEntries[I]).Name = AName then begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
  AbortCompileOnLineFmt(SCompilerUnknownLanguage, [AName]);
end;

function TSetupCompiler.FindSignToolIndexByName(const AName: String): Integer;
var
  I: Integer;
begin
  for I := 0 to SignToolList.Count-1 do begin
    if TSignTool(SignToolList[I]).Name = AName then begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TSetupCompiler.EnumLangOptionsPreProc(const Line: PChar; const Ext: Integer);

  procedure ApplyToLangEntryPre(const KeyName, Value: String;
    const PreLangData: TPreLangData; const AffectsMultipleLangs: Boolean);
  var
    I: Integer;
    Directive: TLangOptionsSectionDirective;

    procedure Invalid;
    begin
      AbortCompileOnLineFmt(SCompilerEntryInvalid2, ['LangOptions', KeyName]);
    end;

    function StrToIntCheck(const S: String): Integer;
    var
      E: Integer;
    begin
      Val(S, Result, E);
      if E <> 0 then
        Invalid;
    end;

  begin
    I := GetEnumValue(TypeInfo(TLangOptionsSectionDirective), 'ls' + KeyName);
    if I = -1 then
      AbortCompileOnLineFmt(SCompilerUnknownDirective, ['LangOptions', KeyName]);
    Directive := TLangOptionsSectionDirective(I);
    case Directive of
      lsLanguageCodePage: begin
          if AffectsMultipleLangs then
            AbortCompileOnLineFmt(SCompilerCantSpecifyLangOption, [KeyName]);
          PreLangData.LanguageCodePage := StrToIntCheck(Value);
          if (PreLangData.LanguageCodePage <> 0) and
             not IsValidCodePage(PreLangData.LanguageCodePage) then
            Invalid;
        end;
    end;
  end;

var
  KeyName, Value: String;
  I, LangIndex: Integer;
begin
  SeparateDirective(Line, KeyName, Value);
  LangIndex := ExtractLangIndex(Self, KeyName, Ext, True);
  if LangIndex = -1 then begin
    for I := 0 to PreLangDataList.Count-1 do
      ApplyToLangEntryPre(KeyName, Value, TPreLangData(PreLangDataList[I]),
        PreLangDataList.Count > 1);
  end else
    ApplyToLangEntryPre(KeyName, Value, TPreLangData(PreLangDataList[LangIndex]), False);
end;

procedure TSetupCompiler.EnumLangOptionsProc(const Line: PChar; const Ext: Integer);

  procedure ApplyToLangEntry(const KeyName, Value: String;
    var LangOptions: TSetupLanguageEntry; const AffectsMultipleLangs: Boolean);
  var
    I: Integer;
    Directive: TLangOptionsSectionDirective;

    procedure Invalid;
    begin
      AbortCompileOnLineFmt(SCompilerEntryInvalid2, ['LangOptions', KeyName]);
    end;

    function StrToIntCheck(const S: String): Integer;
    var
      E: Integer;
    begin
      Val(S, Result, E);
      if E <> 0 then
        Invalid;
    end;

    function ConvertLanguageName(N: String): String;
    var
      I, J, L: Integer;
      W: Word;
    begin
      N := Trim(N);
      if N = '' then
        Invalid;
      Result := '';
      I := 1;
      while I <= Length(N) do begin
        if N[I] = '<' then begin
          { Handle embedded Unicode characters ('<nnnn>') }
          if (I+5 > Length(N)) or (N[I+5] <> '>') then
            Invalid;
          for J := I+1 to I+4 do
            if not CharInSet(UpCase(N[J]), ['0'..'9', 'A'..'F']) then
              Invalid;
          W := StrToIntCheck('$' + Copy(N, I+1, 4));
          Inc(I, 6);
        end
        else begin
          W := Ord(N[I]);
          Inc(I);
        end;
        L := Length(Result);
        SetLength(Result, L + (SizeOf(Word) div SizeOf(Char)));
        Word((@Result[L+1])^) := W;
      end;
    end;

  begin
    I := GetEnumValue(TypeInfo(TLangOptionsSectionDirective), 'ls' + KeyName);
    if I = -1 then
      AbortCompileOnLineFmt(SCompilerUnknownDirective, ['LangOptions', KeyName]);
    Directive := TLangOptionsSectionDirective(I);
    case Directive of
      lsCopyrightFontName: begin
          LangOptions.CopyrightFontName := Trim(Value);
        end;
      lsCopyrightFontSize: begin
          LangOptions.CopyrightFontSize := StrToIntCheck(Value);
        end;
      lsDialogFontName: begin
          LangOptions.DialogFontName := Trim(Value);
        end;
      lsDialogFontSize: begin
          LangOptions.DialogFontSize := StrToIntCheck(Value);
        end;
      lsDialogFontStandardHeight: begin
          WarningsList.Add(Format(SCompilerEntryObsolete, ['LangOptions', KeyName]));
        end;
      lsLanguageCodePage: begin
          if AffectsMultipleLangs then
            AbortCompileOnLineFmt(SCompilerCantSpecifyLangOption, [KeyName]);
          StrToIntCheck(Value);
        end;
      lsLanguageID: begin
          if AffectsMultipleLangs then
            AbortCompileOnLineFmt(SCompilerCantSpecifyLangOption, [KeyName]);
          LangOptions.LanguageID := StrToIntCheck(Value);
        end;
      lsLanguageName: begin
          if AffectsMultipleLangs then
            AbortCompileOnLineFmt(SCompilerCantSpecifyLangOption, [KeyName]);
          LangOptions.LanguageName := ConvertLanguageName(Value);
        end;
      lsRightToLeft: begin
          if not TryStrToBoolean(Value, LangOptions.RightToLeft) then
            Invalid;
        end;
      lsTitleFontName: begin
          LangOptions.TitleFontName := Trim(Value);
        end;
      lsTitleFontSize: begin
          LangOptions.TitleFontSize := StrToIntCheck(Value);
        end;
      lsWelcomeFontName: begin
          LangOptions.WelcomeFontName := Trim(Value);
        end;
      lsWelcomeFontSize: begin
          LangOptions.WelcomeFontSize := StrToIntCheck(Value);
        end;
    end;
  end;

var
  KeyName, Value: String;
  I, LangIndex: Integer;
begin
  SeparateDirective(Line, KeyName, Value);
  LangIndex := ExtractLangIndex(Self, KeyName, Ext, False);
  if LangIndex = -1 then begin
    for I := 0 to LanguageEntries.Count-1 do
      ApplyToLangEntry(KeyName, Value, PSetupLanguageEntry(LanguageEntries[I])^,
        LanguageEntries.Count > 1);
  end else
    ApplyToLangEntry(KeyName, Value, PSetupLanguageEntry(LanguageEntries[LangIndex])^, False);
end;

procedure TSetupCompiler.EnumTypesProc(const Line: PChar; const Ext: Integer);

  function IsCustomTypeAlreadyDefined: Boolean;
  var
    I: Integer;
  begin
    for I := 0 to TypeEntries.Count-1 do
      if toIsCustom in PSetupTypeEntry(TypeEntries[I]).Options then begin
        Result := True;
        Exit;
      end;
    Result := False;
  end;

type
  TParam = (paFlags, paName, paDescription, paLanguages, paCheck, paMinVersion,
    paOnlyBelowVersion);
const
  ParamTypesName = 'Name';
  ParamTypesDescription = 'Description';
  ParamInfo: array[TParam] of TParamInfo = (
    (Name: ParamCommonFlags; Flags: []),
    (Name: ParamTypesName; Flags: [piRequired, piNoEmpty]),
    (Name: ParamTypesDescription; Flags: [piRequired, piNoEmpty]),
    (Name: ParamCommonLanguages; Flags: []),
    (Name: ParamCommonCheck; Flags: []),
    (Name: ParamCommonMinVersion; Flags: []),
    (Name: ParamCommonOnlyBelowVersion; Flags: []));
  Flags: array[0..0] of PChar = (
    'iscustom');
var
  Values: array[TParam] of TParamValue;
  NewTypeEntry: PSetupTypeEntry;
begin
  ExtractParameters(Line, ParamInfo, Values);

  NewTypeEntry := AllocMem(SizeOf(TSetupTypeEntry));
  try
    with NewTypeEntry^ do begin
      MinVersion := SetupHeader.MinVersion;
      Typ := ttUser;

      { Flags }
      while True do
        case ExtractFlag(Values[paFlags].Data, Flags) of
          -2: Break;
          -1: AbortCompileParamError(SCompilerParamUnknownFlag2, ParamCommonFlags);
          0: Include(Options, toIsCustom);
        end;

      { Name }
      Name := LowerCase(Values[paName].Data);

      { Description }
      Description := Values[paDescription].Data;

      { Common parameters }
      ProcessExpressionParameter(ParamCommonLanguages, Values[paLanguages].Data, EvalLanguageIdentifier, False, Languages);
      Check := Values[paCheck].Data;
      ProcessMinVersionParameter(Values[paMinVersion], MinVersion);
      ProcessOnlyBelowVersionParameter(Values[paOnlyBelowVersion], OnlyBelowVersion);

      if (toIsCustom in Options) and IsCustomTypeAlreadyDefined then
        AbortCompileOnLine(SCompilerTypesCustomTypeAlreadyDefined);

      CheckConst(Description, MinVersion, []);
      CheckCheckOrInstall(ParamCommonCheck, Check, cikCheck);
    end;
  except
    SEFreeRec(NewTypeEntry, SetupTypeEntryStrings, SetupTypeEntryAnsiStrings);
    raise;
  end;
  TypeEntries.Add(NewTypeEntry);
end;

procedure TSetupCompiler.EnumComponentsProc(const Line: PChar; const Ext: Integer);

  procedure AddToCommaText(var CommaText: String; const S: String);
  begin
    if CommaText <> '' then
      CommaText := CommaText + ',';
    CommaText := CommaText + S;
  end;

type
  TParam = (paFlags, paName, paDescription, paExtraDiskSpaceRequired, paTypes,
    paLanguages, paCheck, paMinVersion, paOnlyBelowVersion);
const
  ParamComponentsName = 'Name';
  ParamComponentsDescription = 'Description';
  ParamComponentsExtraDiskSpaceRequired = 'ExtraDiskSpaceRequired';
  ParamComponentsTypes = 'Types';
  ParamInfo: array[TParam] of TParamInfo = (
    (Name: ParamCommonFlags; Flags: []),
    (Name: ParamComponentsName; Flags: [piRequired, piNoEmpty]),
    (Name: ParamComponentsDescription; Flags: [piRequired, piNoEmpty]),
    (Name: ParamComponentsExtraDiskSpaceRequired; Flags: []),
    (Name: ParamComponentsTypes; Flags: []),
    (Name: ParamCommonLanguages; Flags: []),
    (Name: ParamCommonCheck; Flags: []),
    (Name: ParamCommonMinVersion; Flags: []),
    (Name: ParamCommonOnlyBelowVersion; Flags: []));
  Flags: array[0..5] of PChar = (
    'fixed', 'restart', 'disablenouninstallwarning', 'exclusive',
    'dontinheritcheck', 'checkablealone');
var
  Values: array[TParam] of TParamValue;
  NewComponentEntry: PSetupComponentEntry;
  PrevLevel, I: Integer;
begin
  ExtractParameters(Line, ParamInfo, Values);

  NewComponentEntry := AllocMem(SizeOf(TSetupComponentEntry));
  try
    with NewComponentEntry^ do begin
      MinVersion := SetupHeader.MinVersion;

      { Flags }
      while True do
        case ExtractFlag(Values[paFlags].Data, Flags) of
          -2: Break;
          -1: AbortCompileParamError(SCompilerParamUnknownFlag2, ParamCommonFlags);
          0: Include(Options, coFixed);
          1: Include(Options, coRestart);
          2: Include(Options, coDisableNoUninstallWarning);
          3: Include(Options, coExclusive);
          4: Include(Options, coDontInheritCheck);
          5: Used := True;
        end;

      { Name }
      Name := LowerCase(Values[paName].Data);
      StringChange(Name, '/', '\');
      if not IsValidIdentString(Name, True, False) then
        AbortCompileOnLine(SCompilerComponentsOrTasksBadName);
      Level := CountChars(Name, '\');
      if ComponentEntries.Count > 0 then
        PrevLevel := PSetupComponentEntry(ComponentEntries[ComponentEntries.Count-1]).Level
      else
        PrevLevel := -1;
      if Level > PrevLevel + 1 then
        AbortCompileOnLine(SCompilerComponentsInvalidLevel);

      { Description }
      Description := Values[paDescription].Data;

      { ExtraDiskSpaceRequired }
      if Values[paExtraDiskSpaceRequired].Found then begin
        if not StrToInteger64(Values[paExtraDiskSpaceRequired].Data, ExtraDiskSpaceRequired) then
          AbortCompileParamError(SCompilerParamInvalid2, ParamComponentsExtraDiskSpaceRequired);
      end;

      { Types }
      while True do begin
        I := ExtractType(Values[paTypes].Data, TypeEntries);
        case I of
          -2: Break;
          -1: AbortCompileParamError(SCompilerParamUnknownType, ParamComponentsTypes);
          else begin
            if TypeEntries.Count <> 0 then
              AddToCommaText(Types, PSetupTypeEntry(TypeEntries[I]).Name)
            else
              AddToCommaText(Types, DefaultTypeEntryNames[I]);
          end;
        end;
      end;

      { Common parameters }
      ProcessExpressionParameter(ParamCommonLanguages, Values[paLanguages].Data, EvalLanguageIdentifier, False, Languages);
      Check := Values[paCheck].Data;
      ProcessMinVersionParameter(Values[paMinVersion], MinVersion);
      ProcessOnlyBelowVersionParameter(Values[paOnlyBelowVersion], OnlyBelowVersion);

      if (coDontInheritCheck in Options) and (coExclusive in Options) then
        AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
          [ParamCommonFlags, 'dontinheritcheck', 'exclusive']);

      CheckConst(Description, MinVersion, []);
      CheckCheckOrInstall(ParamCommonCheck, Check, cikCheck);
    end;
  except
    SEFreeRec(NewComponentEntry, SetupComponentEntryStrings, SetupComponentEntryAnsiStrings);
    raise;
  end;
  ComponentEntries.Add(NewComponentEntry);
end;

procedure TSetupCompiler.EnumTasksProc(const Line: PChar; const Ext: Integer);
type
  TParam = (paFlags, paName, paDescription, paGroupDescription, paComponents,
    paLanguages, paCheck, paMinVersion, paOnlyBelowVersion);
const
  ParamTasksName = 'Name';
  ParamTasksDescription = 'Description';
  ParamTasksGroupDescription = 'GroupDescription';
  ParamInfo: array[TParam] of TParamInfo = (
    (Name: ParamCommonFlags; Flags: []),
    (Name: ParamTasksName; Flags: [piRequired, piNoEmpty, piNoQuotes]),
    (Name: ParamTasksDescription; Flags: [piRequired, piNoEmpty]),
    (Name: ParamTasksGroupDescription; Flags: [piNoEmpty]),
    (Name: ParamCommonComponents; Flags: []),
    (Name: ParamCommonLanguages; Flags: []),
    (Name: ParamCommonCheck; Flags: []),
    (Name: ParamCommonMinVersion; Flags: []),
    (Name: ParamCommonOnlyBelowVersion; Flags: []));
  Flags: array[0..5] of PChar = (
    'exclusive', 'unchecked', 'restart', 'checkedonce', 'dontinheritcheck',
    'checkablealone');
var
  Values: array[TParam] of TParamValue;
  NewTaskEntry: PSetupTaskEntry;
  PrevLevel: Integer;
begin
  ExtractParameters(Line, ParamInfo, Values);

  NewTaskEntry := AllocMem(SizeOf(TSetupTaskEntry));
  try
    with NewTaskEntry^ do begin
      MinVersion := SetupHeader.MinVersion;

      { Flags }
      while True do
        case ExtractFlag(Values[paFlags].Data, Flags) of
          -2: Break;
          -1: AbortCompileParamError(SCompilerParamUnknownFlag2, ParamCommonFlags);
          0: Include(Options, toExclusive);
          1: Include(Options, toUnchecked);
          2: Include(Options, toRestart);
          3: Include(Options, toCheckedOnce);
          4: Include(Options, toDontInheritCheck);
          5: Used := True;
        end;

      { Name }
      Name := LowerCase(Values[paName].Data);
      StringChange(Name, '/', '\');
      if not IsValidIdentString(Name, True, False) then
        AbortCompileOnLine(SCompilerComponentsOrTasksBadName);
      Level := CountChars(Name, '\');
      if TaskEntries.Count > 0 then
        PrevLevel := PSetupTaskEntry(TaskEntries[TaskEntries.Count-1]).Level
      else
        PrevLevel := -1;
      if Level > PrevLevel + 1 then
        AbortCompileOnLine(SCompilerTasksInvalidLevel);

      { Description }
      Description := Values[paDescription].Data;

      { GroupDescription }
      GroupDescription := Values[paGroupDescription].Data;

      { Common parameters }
      ProcessExpressionParameter(ParamCommonComponents, Values[paComponents].Data, EvalComponentIdentifier, True, Components);
      ProcessExpressionParameter(ParamCommonLanguages, Values[paLanguages].Data, EvalLanguageIdentifier, False, Languages);
      Check := Values[paCheck].Data;
      ProcessMinVersionParameter(Values[paMinVersion], MinVersion);
      ProcessOnlyBelowVersionParameter(Values[paOnlyBelowVersion], OnlyBelowVersion);

      if (toDontInheritCheck in Options) and (toExclusive in Options) then
        AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
          [ParamCommonFlags, 'dontinheritcheck', 'exclusive']);

      CheckConst(Description, MinVersion, []);
      CheckConst(GroupDescription, MinVersion, []);
      CheckCheckOrInstall(ParamCommonCheck, Check, cikCheck);
    end;
  except
    SEFreeRec(NewTaskEntry, SetupTaskEntryStrings, SetupTaskEntryAnsiStrings);
    raise;
  end;
  TaskEntries.Add(NewTaskEntry);
end;

const
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $00002000;

procedure TSetupCompiler.EnumDirsProc(const Line: PChar; const Ext: Integer);
type
  TParam = (paFlags, paName, paAttribs, paPermissions, paComponents, paTasks,
    paLanguages, paCheck, paBeforeInstall, paAfterInstall, paMinVersion,
    paOnlyBelowVersion);
const
  ParamDirsName = 'Name';
  ParamDirsAttribs = 'Attribs';
  ParamDirsPermissions = 'Permissions';
  ParamInfo: array[TParam] of TParamInfo = (
    (Name: ParamCommonFlags; Flags: []),
    (Name: ParamDirsName; Flags: [piRequired, piNoEmpty, piNoQuotes]),
    (Name: ParamDirsAttribs; Flags: []),
    (Name: ParamDirsPermissions; Flags: []),
    (Name: ParamCommonComponents; Flags: []),
    (Name: ParamCommonTasks; Flags: []),
    (Name: ParamCommonLanguages; Flags: []),
    (Name: ParamCommonCheck; Flags: []),
    (Name: ParamCommonBeforeInstall; Flags: []),
    (Name: ParamCommonAfterInstall; Flags: []),
    (Name: ParamCommonMinVersion; Flags: []),
    (Name: ParamCommonOnlyBelowVersion; Flags: []));
  Flags: array[0..4] of PChar = (
    'uninsneveruninstall', 'deleteafterinstall', 'uninsalwaysuninstall',
    'setntfscompression', 'unsetntfscompression');
  AttribsFlags: array[0..3] of PChar = (
    'readonly', 'hidden', 'system', 'notcontentindexed');
  AccessMasks: array[0..2] of TNameAndAccessMask = (
    (Name: 'full'; Mask: $1F01FF),
    (Name: 'modify'; Mask: $1301BF),
    (Name: 'readexec'; Mask: $1200A9));
var
  Values: array[TParam] of TParamValue;
  NewDirEntry: PSetupDirEntry;
begin
  ExtractParameters(Line, ParamInfo, Values);

  NewDirEntry := AllocMem(SizeOf(TSetupDirEntry));
  try
    with NewDirEntry^ do begin
      MinVersion := SetupHeader.MinVersion;

      { Flags }
      while True do
        case ExtractFlag(Values[paFlags].Data, Flags) of
          -2: Break;
          -1: AbortCompileParamError(SCompilerParamUnknownFlag2, ParamCommonFlags);
          0: Include(Options, doUninsNeverUninstall);
          1: Include(Options, doDeleteAfterInstall);
          2: Include(Options, doUninsAlwaysUninstall);
          3: Include(Options, doSetNTFSCompression);
          4: Include(Options, doUnsetNTFSCompression);
        end;

      { Name }
      DirName := Values[paName].Data;

      { Attribs }
      while True do
        case ExtractFlag(Values[paAttribs].Data, AttribsFlags) of
          -2: Break;
          -1: AbortCompileParamError(SCompilerParamUnknownFlag2, ParamDirsAttribs);
          0: Attribs := Attribs or FILE_ATTRIBUTE_READONLY;
          1: Attribs := Attribs or FILE_ATTRIBUTE_HIDDEN;
          2: Attribs := Attribs or FILE_ATTRIBUTE_SYSTEM;
          3: Attribs := Attribs or FILE_ATTRIBUTE_NOT_CONTENT_INDEXED;
        end;

      { Permissions }
      ProcessPermissionsParameter(Values[paPermissions].Data, AccessMasks,
        PermissionsEntry);

      { Common parameters }
      ProcessExpressionParameter(ParamCommonComponents, Values[paComponents].Data, EvalComponentIdentifier, True, Components);
      ProcessExpressionParameter(ParamCommonTasks, Values[paTasks].Data, EvalTaskIdentifier, True, Tasks);
      ProcessExpressionParameter(ParamCommonLanguages, Values[paLanguages].Data, EvalLanguageIdentifier, False, Languages);
      Check := Values[paCheck].Data;
      BeforeInstall := Values[paBeforeInstall].Data;
      AfterInstall := Values[paAfterInstall].Data;
      ProcessMinVersionParameter(Values[paMinVersion], MinVersion);
      ProcessOnlyBelowVersionParameter(Values[paOnlyBelowVersion], OnlyBelowVersion);

      if (doUninsNeverUninstall in Options) and
         (doUninsAlwaysUninstall in Options) then
        AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
          [ParamCommonFlags, 'uninsneveruninstall', 'uninsalwaysuninstall']);

      if (doSetNTFSCompression in Options) and
         (doUnsetNTFSCompression in Options) then
        AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
          [ParamCommonFlags, 'setntfscompression', 'unsetntfscompression']);

      CheckCheckOrInstall(ParamCommonCheck, Check, cikCheck);
      CheckCheckOrInstall(ParamCommonBeforeInstall, BeforeInstall, cikInstall);
      CheckCheckOrInstall(ParamCommonAfterInstall, AfterInstall, cikInstall);
      CheckConst(DirName, MinVersion, []);
    end;
  except
    SEFreeRec(NewDirEntry, SetupDirEntryStrings, SetupDirEntryAnsiStrings);
    raise;
  end;
  WriteDebugEntry(deDir, DirEntries.Count);
  DirEntries.Add(NewDirEntry);
end;

type
  TMenuKeyCap = (mkcBkSp, mkcTab, mkcEsc, mkcEnter, mkcSpace, mkcPgUp,
    mkcPgDn, mkcEnd, mkcHome, mkcLeft, mkcUp, mkcRight, mkcDown, mkcIns,
    mkcDel, mkcShift, mkcCtrl, mkcAlt);

var
  MenuKeyCaps: array[TMenuKeyCap] of string = (
    'BkSp', 'Tab', 'Esc', 'Enter', 'Space', 'PgUp',
    'PgDn', 'End', 'Home', 'Left', 'Up', 'Right',
    'Down', 'Ins', 'Del', 'Shift+', 'Ctrl+', 'Alt+');

procedure TSetupCompiler.EnumIconsProc(const Line: PChar; const Ext: Integer);

  function HotKeyToText(HotKey: Word): string;

    function GetSpecialName(HotKey: Word): string;
    var
      ScanCode: Integer;
      KeyName: array[0..255] of Char;
    begin
      Result := '';
      ScanCode := MapVirtualKey(WordRec(HotKey).Lo, 0) shl 16;
      if ScanCode <> 0 then
      begin
        GetKeyNameText(ScanCode, KeyName, SizeOf(KeyName));
        if (KeyName[1] = #0) and (KeyName[0] <> #0) then
          GetSpecialName := KeyName;
      end;
    end;

  var
    Name: string;
  begin
    case WordRec(HotKey).Lo of
      $08, $09:
        Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcBkSp) + WordRec(HotKey).Lo - $08)];
      $0D: Name := MenuKeyCaps[mkcEnter];
      $1B: Name := MenuKeyCaps[mkcEsc];
      $20..$28:
        Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcSpace) + WordRec(HotKey).Lo - $20)];
      $2D..$2E:
        Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcIns) + WordRec(HotKey).Lo - $2D)];
      $30..$39: Name := Chr(WordRec(HotKey).Lo - $30 + Ord('0'));
      $41..$5A: Name := Chr(WordRec(HotKey).Lo - $41 + Ord('A'));
      $60..$69: Name := Chr(WordRec(HotKey).Lo - $60 + Ord('0'));
      $70..$87: Name := 'F' + IntToStr(WordRec(HotKey).Lo - $6F);
    else
      Name := GetSpecialName(HotKey);
    end;
    if Name <> '' then
    begin
      Result := '';
      if HotKey and (HOTKEYF_SHIFT shl 8) <> 0 then Result := Result + MenuKeyCaps[mkcShift];
      if HotKey and (HOTKEYF_CONTROL shl 8) <> 0 then Result := Result + MenuKeyCaps[mkcCtrl];
      if HotKey and (HOTKEYF_ALT shl 8) <> 0 then Result := Result + MenuKeyCaps[mkcAlt];
      Result := Result + Name;
    end
    else Result := '';
  end;

  function TextToHotKey(Text: string): Word;

    function CompareFront(var Text: string; const Front: string): Boolean;
    begin
      Result := False;
      if CompareText(Copy(Text, 1, Length(Front)), Front) = 0 then
      begin
        Result := True;
        Delete(Text, 1, Length(Front));
      end;
    end;

  var
    Key: Word;
    Shift: Word;
  begin
    Result := 0;
    Shift := 0;
    while True do
    begin
      if CompareFront(Text, MenuKeyCaps[mkcShift]) then Shift := Shift or HOTKEYF_SHIFT
      else if CompareFront(Text, '^') then Shift := Shift or HOTKEYF_CONTROL
      else if CompareFront(Text, MenuKeyCaps[mkcCtrl]) then Shift := Shift or HOTKEYF_CONTROL
      else if CompareFront(Text, MenuKeyCaps[mkcAlt]) then Shift := Shift or HOTKEYF_ALT
      else Break;
    end;
    if Text = '' then Exit;
    for Key := $08 to $255 do { Copy range from table in HotKeyToText }
      if AnsiCompareText(Text, HotKeyToText(Key)) = 0 then
      begin
        Result := Key or (Shift shl 8);
        Exit;
      end;
  end;

type
  TParam = (paFlags, paName, paFilename, paParameters, paWorkingDir, paHotKey,
    paIconFilename, paIconIndex, paComment, paAppUserModelID, paAppUserModelToastActivatorCLSID,
    paComponents, paTasks, paLanguages, paCheck, paBeforeInstall, paAfterInstall, paMinVersion,
    paOnlyBelowVersion);
const
  ParamIconsName = 'Name';
  ParamIconsFilename = 'Filename';
  ParamIconsParameters = 'Parameters';
  ParamIconsWorkingDir = 'WorkingDir';
  ParamIconsHotKey = 'HotKey';
  ParamIconsIconFilename = 'IconFilename';
  ParamIconsIconIndex = 'IconIndex';
  ParamIconsComment = 'Comment';
  ParamIconsAppUserModelID = 'AppUserModelID';
  ParamIconsAppUserModelToastActivatorCLSID = 'AppUserModelToastActivatorCLSID';
  ParamInfo: array[TParam] of TParamInfo = (
    (Name: ParamCommonFlags; Flags: []),
    (Name: ParamIconsName; Flags: [piRequired, piNoEmpty, piNoQuotes]),
    (Name: ParamIconsFilename; Flags: [piRequired, piNoEmpty, piNoQuotes]),
    (Name: ParamIconsParameters; Flags: []),
    (Name: ParamIconsWorkingDir; Flags: [piNoQuotes]),
    (Name: ParamIconsHotKey; Flags: []),
    (Name: ParamIconsIconFilename; Flags: [piNoQuotes]),
    (Name: ParamIconsIconIndex; Flags: []),
    (Name: ParamIconsComment; Flags: []),
    (Name: ParamIconsAppUserModelID; Flags: []),
    (Name: ParamIconsAppUserModelToastActivatorCLSID; Flags: []),
    (Name: ParamCommonComponents; Flags: []),
    (Name: ParamCommonTasks; Flags: []),
    (Name: ParamCommonLanguages; Flags: []),
    (Name: ParamCommonCheck; Flags: []),
    (Name: ParamCommonBeforeInstall; Flags: []),
    (Name: ParamCommonAfterInstall; Flags: []),
    (Name: ParamCommonMinVersion; Flags: []),
    (Name: ParamCommonOnlyBelowVersion; Flags: []));
  Flags: array[0..8] of PChar = (
    'uninsneveruninstall', 'runminimized', 'createonlyiffileexists',
    'useapppaths', 'closeonexit', 'dontcloseonexit', 'runmaximized',
    'excludefromshowinnewinstall', 'preventpinning');
var
  Values: array[TParam] of TParamValue;
  NewIconEntry: PSetupIconEntry;
  S: String;
begin
  ExtractParameters(Line, ParamInfo, Values);

  NewIconEntry := AllocMem(SizeOf(TSetupIconEntry));
  try
    with NewIconEntry^ do begin
      MinVersion := SetupHeader.MinVersion;
      ShowCmd := SW_SHOWNORMAL;

      { Flags }
      while True do
        case ExtractFlag(Values[paFlags].Data, Flags) of
          -2: Break;
          -1: AbortCompileParamError(SCompilerParamUnknownFlag2, ParamCommonFlags);
          0: Include(Options, ioUninsNeverUninstall);
          1: ShowCmd := SW_SHOWMINNOACTIVE;
          2: Include(Options, ioCreateOnlyIfFileExists);
          3: Include(Options, ioUseAppPaths);
          4: CloseOnExit := icYes;
          5: CloseOnExit := icNo;
          6: ShowCmd := SW_SHOWMAXIMIZED;
          7: Include(Options, ioExcludeFromShowInNewInstall);
          8: Include(Options, ioPreventPinning);
        end;

      { Name }
      IconName := Values[paName].Data;

      { Filename }
      Filename := Values[paFilename].Data;

      { Parameters }
      Parameters := Values[paParameters].Data;

      { WorkingDir }
      WorkingDir := Values[paWorkingDir].Data;

      { HotKey }
      if Values[paHotKey].Found then begin
        HotKey := TextToHotKey(Values[paHotKey].Data);
        if HotKey = 0 then
          AbortCompileParamError(SCompilerParamInvalid2, ParamIconsHotKey);
      end;

      { IconFilename }
      IconFilename := Values[paIconFilename].Data;

      { IconIndex }
      if Values[paIconIndex].Found then begin
        try
          IconIndex := StrToInt(Values[paIconIndex].Data);
        except
          AbortCompileOnLine(SCompilerIconsIconIndexInvalid);
        end;
      end;

      { Comment }
      Comment := Values[paComment].Data;

      { AppUserModel }
      AppUserModelID := Values[paAppUserModelID].Data;
      S := Values[paAppUserModelToastActivatorCLSID].Data;
      if S <> '' then begin
        AppUserModelToastActivatorCLSID := StringToGUID('{' + S + '}');
        Include(Options, ioHasAppUserModelToastActivatorCLSID);
      end;

      { Common parameters }
      ProcessExpressionParameter(ParamCommonComponents, Values[paComponents].Data, EvalComponentIdentifier, True, Components);
      ProcessExpressionParameter(ParamCommonTasks, Values[paTasks].Data, EvalTaskIdentifier, True, Tasks);
      ProcessExpressionParameter(ParamCommonLanguages, Values[paLanguages].Data, EvalLanguageIdentifier, False, Languages);
      Check := Values[paCheck].Data;
      BeforeInstall := Values[paBeforeInstall].Data;
      AfterInstall := Values[paAfterInstall].Data;
      ProcessMinVersionParameter(Values[paMinVersion], MinVersion);
      ProcessOnlyBelowVersionParameter(Values[paOnlyBelowVersion], OnlyBelowVersion);

      if Pos('"', IconName) <> 0 then
        AbortCompileParamError(SCompilerParamNoQuotes2, ParamIconsName);
      if PathPos('\', IconName) = 0 then
        AbortCompileOnLine(SCompilerIconsNamePathNotSpecified);

      if (IconIndex <> 0) and (IconFilename = '') then
        IconFilename := Filename;

      CheckCheckOrInstall(ParamCommonCheck, Check, cikCheck);
      CheckCheckOrInstall(ParamCommonBeforeInstall, BeforeInstall, cikInstall);
      CheckCheckOrInstall(ParamCommonAfterInstall, AfterInstall, cikInstall);
      S := IconName;
      if Copy(S, 1, 8) = '{group}\' then
        Delete(S, 1, 8);
      CheckConst(S, MinVersion, []);
      CheckConst(Filename, MinVersion, []);
      CheckConst(Parameters, MinVersion, []);
      CheckConst(WorkingDir, MinVersion, []);
      CheckConst(IconFilename, MinVersion, []);
      CheckConst(Comment, MinVersion, []);
      CheckConst(AppUserModelID, MinVersion, []);
    end;
  except
    SEFreeRec(NewIconEntry, SetupIconEntryStrings, SetupIconEntryAnsiStrings);
    raise;
  end;
  WriteDebugEntry(deIcon, IconEntries.Count);
  IconEntries.Add(NewIconEntry);
end;

procedure TSetupCompiler.EnumINIProc(const Line: PChar; const Ext: Integer);
type
  TParam = (paFlags, paFilename, paSection, paKey, paString, paComponents,
    paTasks, paLanguages, paCheck, paBeforeInstall, paAfterInstall,
    paMinVersion, paOnlyBelowVersion);
const
  ParamIniFilename = 'Filename';
  ParamIniSection = 'Section';
  ParamIniKey = 'Key';
  ParamIniString = 'String';
  ParamInfo: array[TParam] of TParamInfo = (
    (Name: ParamCommonFlags; Flags: []),
    (Name: ParamIniFilename; Flags: [piRequired, piNoQuotes]),
    (Name: ParamIniSection; Flags: [piRequired, piNoEmpty]),
    (Name: ParamIniKey; Flags: [piNoEmpty]),
    (Name: ParamIniString; Flags: []),
    (Name: ParamCommonComponents; Flags: []),
    (Name: ParamCommonTasks; Flags: []),
    (Name: ParamCommonLanguages; Flags: []),
    (Name: ParamCommonCheck; Flags: []),
    (Name: ParamCommonBeforeInstall; Flags: []),
    (Name: ParamCommonAfterInstall; Flags: []),
    (Name: ParamCommonMinVersion; Flags: []),
    (Name: ParamCommonOnlyBelowVersion; Flags: []));
  Flags: array[0..3] of PChar = (
    'uninsdeleteentry', 'uninsdeletesection', 'createkeyifdoesntexist',
    'uninsdeletesectionifempty');
var
  Values: array[TParam] of TParamValue;
  NewIniEntry: PSetupIniEntry;
begin
  ExtractParameters(Line, ParamInfo, Values);

  NewIniEntry := AllocMem(SizeOf(TSetupIniEntry));
  try
    with NewIniEntry^ do begin
      MinVersion := SetupHeader.MinVersion;

      { Flags }
      while True do
        case ExtractFlag(Values[paFlags].Data, Flags) of
          -2: Break;
          -1: AbortCompileParamError(SCompilerParamUnknownFlag2, ParamCommonFlags);
          0: Include(Options, ioUninsDeleteEntry);
          1: Include(Options, ioUninsDeleteEntireSection);
          2: Include(Options, ioCreateKeyIfDoesntExist);
          3: Include(Options, ioUninsDeleteSectionIfEmpty);
        end;

      { Filename }
      Filename := Values[paFilename].Data;

      { Section }
      Section := Values[paSection].Data;

      { Key }
      Entry := Values[paKey].Data;

      { String }
      if Values[paString].Found then begin
        Value := Values[paString].Data;
        Include(Options, ioHasValue);
      end;

      { Common parameters }
      ProcessExpressionParameter(ParamCommonComponents, Values[paComponents].Data, EvalComponentIdentifier, True, Components);
      ProcessExpressionParameter(ParamCommonTasks, Values[paTasks].Data, EvalTaskIdentifier, True, Tasks);
      ProcessExpressionParameter(ParamCommonLanguages, Values[paLanguages].Data, EvalLanguageIdentifier, False, Languages);
      Check := Values[paCheck].Data;
      BeforeInstall := Values[paBeforeInstall].Data;
      AfterInstall := Values[paAfterInstall].Data;
      ProcessMinVersionParameter(Values[paMinVersion], MinVersion);
      ProcessOnlyBelowVersionParameter(Values[paOnlyBelowVersion], OnlyBelowVersion);

      if (ioUninsDeleteEntry in Options) and
         (ioUninsDeleteEntireSection in Options) then
        AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
          [ParamCommonFlags, 'uninsdeleteentry', 'uninsdeletesection']);
      if (ioUninsDeleteEntireSection in Options) and
         (ioUninsDeleteSectionIfEmpty in Options) then
        AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
          [ParamCommonFlags, 'uninsdeletesection', 'uninsdeletesectionifempty']);

      CheckCheckOrInstall(ParamCommonCheck, Check, cikCheck);
      CheckCheckOrInstall(ParamCommonBeforeInstall, BeforeInstall, cikInstall);
      CheckCheckOrInstall(ParamCommonAfterInstall, AfterInstall, cikInstall);
      CheckConst(Filename, MinVersion, []);
      CheckConst(Section, MinVersion, []);
      CheckConst(Entry, MinVersion, []);
      CheckConst(Value, MinVersion, []);
    end;
  except
    SEFreeRec(NewIniEntry, SetupIniEntryStrings, SetupIniEntryAnsiStrings);
    raise;
  end;
  WriteDebugEntry(deIni, IniEntries.Count);
  IniEntries.Add(NewIniEntry);
end;

procedure TSetupCompiler.EnumRegistryProc(const Line: PChar; const Ext: Integer);
type
  TParam = (paFlags, paRoot, paSubkey, paValueType, paValueName, paValueData,
    paPermissions, paComponents, paTasks, paLanguages, paCheck, paBeforeInstall,
    paAfterInstall, paMinVersion, paOnlyBelowVersion);
const
  ParamRegistryRoot = 'Root';
  ParamRegistrySubkey = 'Subkey';
  ParamRegistryValueType = 'ValueType';
  ParamRegistryValueName = 'ValueName';
  ParamRegistryValueData = 'ValueData';
  ParamRegistryPermissions = 'Permissions';
  ParamInfo: array[TParam] of TParamInfo = (
    (Name: ParamCommonFlags; Flags: []),
    (Name: ParamRegistryRoot; Flags: [piRequired]),
    (Name: ParamRegistrySubkey; Flags: [piRequired, piNoEmpty]),
    (Name: ParamRegistryValueType; Flags: []),
    (Name: ParamRegistryValueName; Flags: []),
    (Name: ParamRegistryValueData; Flags: []),
    (Name: ParamRegistryPermissions; Flags: []),
    (Name: ParamCommonComponents; Flags: []),
    (Name: ParamCommonTasks; Flags: []),
    (Name: ParamCommonLanguages; Flags: []),
    (Name: ParamCommonCheck; Flags: []),
    (Name: ParamCommonBeforeInstall; Flags: []),
    (Name: ParamCommonAfterInstall; Flags: []),
    (Name: ParamCommonMinVersion; Flags: []),
    (Name: ParamCommonOnlyBelowVersion; Flags: []));
  Flags: array[0..9] of PChar = (
    'createvalueifdoesntexist', 'uninsdeletevalue', 'uninsdeletekey',
    'uninsdeletekeyifempty', 'uninsclearvalue', 'preservestringtype',
    'deletekey', 'deletevalue', 'noerror', 'dontcreatekey');
  AccessMasks: array[0..2] of TNameAndAccessMask = (
    (Name: 'full'; Mask: $F003F),
    (Name: 'modify'; Mask: $3001F), { <- same access that Power Users get by default on HKLM\SOFTWARE }
    (Name: 'read'; Mask: $20019));

  function ConvertBinaryString(const S: String): String;
    procedure Invalid;
    begin
      AbortCompileParamError(SCompilerParamInvalid2, ParamRegistryValueData);
    end;
  var
    I: Integer;
    C: Char;
    B: Byte;
    N: Integer;
    procedure EndByte;
    begin
      case N of
        0: ;
        2: begin
             Result := Result + Chr(B);
             N := 0;
             B := 0;
           end;
      else
        Invalid;
      end;
    end;
  begin
    Result := '';
    N := 0;
    B := 0;
    for I := 1 to Length(S) do begin
      C := UpCase(S[I]);
      case C of
        ' ': EndByte;
        '0'..'9': begin
               Inc(N);
               if N > 2 then
                 Invalid;
               B := (B shl 4) or (Ord(C) - Ord('0'));
             end;
        'A'..'F': begin
               Inc(N);
               if N > 2 then
                 Invalid;
               B := (B shl 4) or (10 + Ord(C) - Ord('A'));
             end;
      else
        Invalid;
      end;
    end;
    EndByte;
  end;

  function ConvertDWordString(const S: String): String;
  var
    DW: DWORD;
    E: Integer;
  begin
    Result := Trim(S);
    { Only check if it doesn't start with a constant }
    if (Result = '') or (Result[1] <> '{') then begin
      Val(Result, DW, E);
      if E <> 0 then
        AbortCompileParamError(SCompilerParamInvalid2, ParamRegistryValueData);
      { Not really necessary, but sanitize the value }
      Result := Format('$%x', [DW]);
    end;
  end;

  function ConvertQWordString(const S: String): String;
  var
    QW: Integer64;
  begin
    Result := Trim(S);
    { Only check if it doesn't start with a constant }
    if (Result = '') or (Result[1] <> '{') then begin
      if not StrToInteger64(Result, QW) then
        AbortCompileParamError(SCompilerParamInvalid2, ParamRegistryValueData);
      { Not really necessary, but sanitize the value }
      Result := Integer64ToStr(QW);
    end;
  end;

var
  Values: array[TParam] of TParamValue;
  NewRegistryEntry: PSetupRegistryEntry;
  S, AData: String;
begin
  ExtractParameters(Line, ParamInfo, Values);

  NewRegistryEntry := AllocMem(SizeOf(TSetupRegistryEntry));
  try
    with NewRegistryEntry^ do begin
      MinVersion := SetupHeader.MinVersion;

      { Flags }
      while True do
        case ExtractFlag(Values[paFlags].Data, Flags) of
          -2: Break;
          -1: AbortCompileParamError(SCompilerParamUnknownFlag2, ParamCommonFlags);
          0: Include(Options, roCreateValueIfDoesntExist);
          1: Include(Options, roUninsDeleteValue);
          2: Include(Options, roUninsDeleteEntireKey);
          3: Include(Options, roUninsDeleteEntireKeyIfEmpty);
          4: Include(Options, roUninsClearValue);
          5: Include(Options, roPreserveStringType);
          6: Include(Options, roDeleteKey);
          7: Include(Options, roDeleteValue);
          8: Include(Options, roNoError);
          9: Include(Options, roDontCreateKey);
        end;

      { Root }
      S := Uppercase(Trim(Values[paRoot].Data));
      if Length(S) >= 2 then begin
        { Check for '32' or '64' suffix }
        if (S[Length(S)-1] = '3') and (S[Length(S)] = '2') then begin
          Include(Options, ro32Bit);
          SetLength(S, Length(S)-2);
        end
        else if (S[Length(S)-1] = '6') and (S[Length(S)] = '4') then begin
          Include(Options, ro64Bit);
          SetLength(S, Length(S)-2);
        end;
      end;
      if S = 'HKA' then
        RootKey := HKEY_AUTO
      else if S = 'HKCR' then
        RootKey := HKEY_CLASSES_ROOT
      else if S = 'HKCU' then begin
        UsedUserAreas.Add(S);
        RootKey := HKEY_CURRENT_USER;
      end else if S = 'HKLM' then
        RootKey := HKEY_LOCAL_MACHINE
      else if S = 'HKU' then
        RootKey := HKEY_USERS
      else if S = 'HKCC' then
        RootKey := HKEY_CURRENT_CONFIG
      else
        AbortCompileParamError(SCompilerParamInvalid2, ParamRegistryRoot);

      { Subkey }
      if (Values[paSubkey].Data <> '') and (Values[paSubkey].Data[1] = '\') then
        AbortCompileParamError(SCompilerParamNoPrecedingBackslash, ParamRegistrySubkey);
      Subkey := Values[paSubkey].Data;

      { ValueType }
      if Values[paValueType].Found then begin
        Values[paValueType].Data := Uppercase(Trim(Values[paValueType].Data));
        if Values[paValueType].Data = 'NONE' then
          Typ := rtNone
        else if Values[paValueType].Data = 'STRING' then
          Typ := rtString
        else if Values[paValueType].Data = 'EXPANDSZ' then
          Typ := rtExpandString
        else if Values[paValueType].Data = 'MULTISZ' then
          Typ := rtMultiString
        else if Values[paValueType].Data = 'DWORD' then
          Typ := rtDWord
        else if Values[paValueType].Data = 'QWORD' then
          Typ := rtQWord
        else if Values[paValueType].Data = 'BINARY' then
          Typ := rtBinary
        else
          AbortCompileParamError(SCompilerParamInvalid2, ParamRegistryValueType);
      end;

      { ValueName }
      ValueName := Values[paValueName].Data;

      { ValueData }
      AData := Values[paValueData].Data;

      { Permissions }
      ProcessPermissionsParameter(Values[paPermissions].Data, AccessMasks,
        PermissionsEntry);

      { Common parameters }
      ProcessExpressionParameter(ParamCommonComponents, Values[paComponents].Data, EvalComponentIdentifier, True, Components);
      ProcessExpressionParameter(ParamCommonTasks, Values[paTasks].Data, EvalTaskIdentifier, True, Tasks);
      ProcessExpressionParameter(ParamCommonLanguages, Values[paLanguages].Data, EvalLanguageIdentifier, False, Languages);
      Check := Values[paCheck].Data;
      BeforeInstall := Values[paBeforeInstall].Data;
      AfterInstall := Values[paAfterInstall].Data;
      ProcessMinVersionParameter(Values[paMinVersion], MinVersion);
      ProcessOnlyBelowVersionParameter(Values[paOnlyBelowVersion], OnlyBelowVersion);

      if (roUninsDeleteEntireKey in Options) and
         (roUninsDeleteEntireKeyIfEmpty in Options) then
        AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
          [ParamCommonFlags, 'uninsdeletekey', 'uninsdeletekeyifempty']);
      if (roUninsDeleteEntireKey in Options) and
         (roUninsClearValue in Options) then
        AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
          [ParamCommonFlags, 'uninsclearvalue', 'uninsdeletekey']);
      if (roUninsDeleteValue in Options) and
         (roUninsDeleteEntireKey in Options) then
        AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
          [ParamCommonFlags, 'uninsdeletevalue', 'uninsdeletekey']);
      if (roUninsDeleteValue in Options) and
         (roUninsClearValue in Options) then
        AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
          [ParamCommonFlags, 'uninsdeletevalue', 'uninsclearvalue']);

      { Safety checks }
      if ((roUninsDeleteEntireKey in Options) or (roDeleteKey in Options)) and
         (CompareText(Subkey, 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment') = 0) then
        AbortCompileOnLine(SCompilerRegistryDeleteKeyProhibited);

      case Typ of
        rtString, rtExpandString, rtMultiString:
          ValueData := AData;
        rtDWord:
          ValueData := ConvertDWordString(AData);
        rtQWord:
          ValueData := ConvertQWordString(AData);
        rtBinary:
          ValueData := ConvertBinaryString(AData);
      end;

      CheckCheckOrInstall(ParamCommonCheck, Check, cikCheck);
      CheckCheckOrInstall(ParamCommonBeforeInstall, BeforeInstall, cikInstall);
      CheckCheckOrInstall(ParamCommonAfterInstall, AfterInstall, cikInstall);
      CheckConst(Subkey, MinVersion, []);
      CheckConst(ValueName, MinVersion, []);
      case Typ of
        rtString, rtExpandString:
          CheckConst(ValueData, MinVersion, [acOldData]);
        rtMultiString:
          CheckConst(ValueData, MinVersion, [acOldData, acBreak]);
        rtDWord:
          CheckConst(ValueData, MinVersion, []);
      end;
    end;
  except
    SEFreeRec(NewRegistryEntry, SetupRegistryEntryStrings, SetupRegistryEntryAnsiStrings);
    raise;
  end;
  WriteDebugEntry(deRegistry, RegistryEntries.Count);
  RegistryEntries.Add(NewRegistryEntry);
end;

procedure TSetupCompiler.EnumDeleteProc(const Line: PChar; const Ext: Integer);
type
  TParam = (paType, paName, paComponents, paTasks, paLanguages, paCheck,
    paBeforeInstall, paAfterInstall, paMinVersion, paOnlyBelowVersion);
const
  ParamDeleteType = 'Type';
  ParamDeleteName = 'Name';
  ParamInfo: array[TParam] of TParamInfo = (
    (Name: ParamDeleteType; Flags: [piRequired]),
    (Name: ParamDeleteName; Flags: [piRequired, piNoEmpty]),
    (Name: ParamCommonComponents; Flags: []),
    (Name: ParamCommonTasks; Flags: []),
    (Name: ParamCommonLanguages; Flags: []),
    (Name: ParamCommonCheck; Flags: []),
    (Name: ParamCommonBeforeInstall; Flags: []),
    (Name: ParamCommonAfterInstall; Flags: []),
    (Name: ParamCommonMinVersion; Flags: []),
    (Name: ParamCommonOnlyBelowVersion; Flags: []));
  Types: array[TSetupDeleteType] of PChar = (
    'files', 'filesandordirs', 'dirifempty');
var
  Values: array[TParam] of TParamValue;
  NewDeleteEntry: PSetupDeleteEntry;
  Valid: Boolean;
  J: TSetupDeleteType;
begin
  ExtractParameters(Line, ParamInfo, Values);

  NewDeleteEntry := AllocMem(SizeOf(TSetupDeleteEntry));
  try
    with NewDeleteEntry^ do begin
      MinVersion := SetupHeader.MinVersion;

      { Type }
      Values[paType].Data := Trim(Values[paType].Data);
      Valid := False;
      for J := Low(J) to High(J) do
        if StrIComp(Types[J], PChar(Values[paType].Data)) = 0 then begin
          DeleteType := J;
          Valid := True;
          Break;
        end;
      if not Valid then
        AbortCompileParamError(SCompilerParamInvalid2, ParamDeleteType);

      { Name }
      Name := Values[paName].Data;

      { Common parameters }
      ProcessExpressionParameter(ParamCommonComponents, Values[paComponents].Data, EvalComponentIdentifier, True, Components);
      ProcessExpressionParameter(ParamCommonTasks, Values[paTasks].Data, EvalTaskIdentifier, True, Tasks);
      ProcessExpressionParameter(ParamCommonLanguages, Values[paLanguages].Data, EvalLanguageIdentifier, False, Languages);
      Check := Values[paCheck].Data;
      BeforeInstall := Values[paBeforeInstall].Data;
      AfterInstall := Values[paAfterInstall].Data;
      ProcessMinVersionParameter(Values[paMinVersion], MinVersion);
      ProcessOnlyBelowVersionParameter(Values[paOnlyBelowVersion], OnlyBelowVersion);

      CheckCheckOrInstall(ParamCommonCheck, Check, cikCheck);
      CheckCheckOrInstall(ParamCommonBeforeInstall, BeforeInstall, cikInstall);
      CheckCheckOrInstall(ParamCommonAfterInstall, AfterInstall, cikInstall);
      CheckConst(Name, MinVersion, []);
    end;
  except
    SEFreeRec(NewDeleteEntry, SetupDeleteEntryStrings, SetupDeleteEntryAnsiStrings);
    raise;
  end;
  if Ext = 0 then begin
    WriteDebugEntry(deInstallDelete, InstallDeleteEntries.Count);
    InstallDeleteEntries.Add(NewDeleteEntry);
  end
  else begin
    WriteDebugEntry(deUninstallDelete, UninstallDeleteEntries.Count);
    UninstallDeleteEntries.Add(NewDeleteEntry);
  end;
end;

procedure TSetupCompiler.EnumFilesProc(const Line: PChar; const Ext: Integer);

  function EscapeBraces(const S: String): String;
  { Changes all '{' to '{{' }
  var
    I: Integer;
  begin
    Result := S;
    I := 1;
    while I <= Length(Result) do begin
      if Result[I] = '{' then begin
        Insert('{', Result, I);
        Inc(I);
      end;
      Inc(I);
    end;
  end;

type
  TParam = (paFlags, paSource, paDestDir, paDestName, paCopyMode, paAttribs,
    paPermissions, paFontInstall, paExcludes, paExternalSize, paStrongAssemblyName,
    paComponents, paTasks, paLanguages, paCheck, paBeforeInstall, paAfterInstall,
    paMinVersion, paOnlyBelowVersion);
const
  ParamFilesSource = 'Source';
  ParamFilesDestDir = 'DestDir';
  ParamFilesDestName = 'DestName';
  ParamFilesCopyMode = 'CopyMode';
  ParamFilesAttribs = 'Attribs';
  ParamFilesPermissions = 'Permissions';
  ParamFilesFontInstall = 'FontInstall';
  ParamFilesExcludes = 'Excludes';
  ParamFilesExternalSize = 'ExternalSize';
  ParamFilesStrongAssemblyName = 'StrongAssemblyName';
  ParamInfo: array[TParam] of TParamInfo = (
    (Name: ParamCommonFlags; Flags: []),
    (Name: ParamFilesSource; Flags: [piRequired, piNoEmpty, piNoQuotes]),
    (Name: ParamFilesDestDir; Flags: [piNoEmpty, piNoQuotes]),
    (Name: ParamFilesDestName; Flags: [piNoEmpty, piNoQuotes]),
    (Name: ParamFilesCopyMode; Flags: []),
    (Name: ParamFilesAttribs; Flags: []),
    (Name: ParamFilesPermissions; Flags: []),
    (Name: ParamFilesFontInstall; Flags: [piNoEmpty]),
    (Name: ParamFilesExcludes; Flags: []),
    (Name: ParamFilesExternalSize; Flags: []),
    (Name: ParamFilesStrongAssemblyName; Flags: [piNoEmpty]),
    (Name: ParamCommonComponents; Flags: []),
    (Name: ParamCommonTasks; Flags: []),
    (Name: ParamCommonLanguages; Flags: []),
    (Name: ParamCommonCheck; Flags: []),
    (Name: ParamCommonBeforeInstall; Flags: []),
    (Name: ParamCommonAfterInstall; Flags: []),
    (Name: ParamCommonMinVersion; Flags: []),
    (Name: ParamCommonOnlyBelowVersion; Flags: []));
  Flags: array[0..40] of PChar = (
    'confirmoverwrite', 'uninsneveruninstall', 'isreadme', 'regserver',
    'sharedfile', 'restartreplace', 'deleteafterinstall',
    'comparetimestamp', 'fontisnttruetype', 'regtypelib', 'external',
    'skipifsourcedoesntexist', 'overwritereadonly', 'onlyifdestfileexists',
    'recursesubdirs', 'noregerror', 'allowunsafefiles', 'uninsrestartdelete',
    'onlyifdoesntexist', 'ignoreversion', 'promptifolder', 'dontcopy',
    'uninsremovereadonly', 'sortfilesbyextension', 'touch', 'replacesameversion',
    'noencryption', 'nocompression', 'dontverifychecksum',
    'uninsnosharedfileprompt', 'createallsubdirs', '32bit', '64bit',
    'solidbreak', 'setntfscompression', 'unsetntfscompression',
    'sortfilesbyname', 'gacinstall', 'sign', 'signonce', 'signcheck');
  SignFlags: array[TSetupFileLocationSign] of String = (
    '', 'sign', 'signonce', 'signcheck');
  AttribsFlags: array[0..3] of PChar = (
    'readonly', 'hidden', 'system', 'notcontentindexed');
  AccessMasks: array[0..2] of TNameAndAccessMask = (
    (Name: 'full'; Mask: $1F01FF),
    (Name: 'modify'; Mask: $1301BF),
    (Name: 'readexec'; Mask: $1200A9));
var
  Values: array[TParam] of TParamValue;
  NewFileEntry, PrevFileEntry: PSetupFileEntry;
  NewFileLocationEntry: PSetupFileLocationEntry;
  VersionNumbers: TFileVersionNumbers;
  SourceWildcard, ADestDir, ADestName, AInstallFontName, AStrongAssemblyName: String;
  AExcludes: TStringList;
  ReadmeFile, ExternalFile, SourceIsWildcard, RecurseSubdirs,
    AllowUnsafeFiles, Touch, NoCompression, NoEncryption, SolidBreak: Boolean;
  Sign: TSetupFileLocationSign;
type
  PFileListRec = ^TFileListRec;
  TFileListRec = record
    Name: String;
    Size: Integer64;
  end;
  PDirListRec = ^TDirListRec;
  TDirListRec = record
    Name: String;
  end;

  procedure CheckForUnsafeFile(const Filename, SourceFile: String;
    const IsRegistered: Boolean);
  { This generates errors on "unsafe files" }
  const
    UnsafeSysFiles: array[0..13] of String = (
      'ADVAPI32.DLL', 'COMCTL32.DLL', 'COMDLG32.DLL', 'GDI32.DLL',
      'KERNEL32.DLL', 'MSCOREE.DLL', 'RICHED32.DLL', 'SHDOCVW.DLL',
      'SHELL32.DLL', 'SHLWAPI.DLL', 'URLMON.DLL', 'USER32.DLL', 'UXTHEME.DLL',
      'WININET.DLL');
    UnsafeNonSysRegFiles: array[0..5] of String = (
      'COMCAT.DLL', 'MSVBVM50.DLL', 'MSVBVM60.DLL', 'OLEAUT32.DLL',
      'OLEPRO32.DLL', 'STDOLE2.TLB');
  var
    SourceFileDir, SysWow64Dir: String;
    I: Integer;
  begin
    if AllowUnsafeFiles then
      Exit;
    if ADestDir = '{sys}\' then begin
      { Files that must NOT be deployed to the user's System directory }
      { Any DLL deployed from system's own System directory }
      if not ExternalFile and
         SameText(PathExtractExt(Filename), '.DLL') then begin
        SourceFileDir := PathExpand(PathExtractDir(SourceFile));
        SysWow64Dir := GetSysWow64Dir;
        if (PathCompare(SourceFileDir, GetSystemDir) = 0) or
           ((SysWow64Dir <> '') and ((PathCompare(SourceFileDir, SysWow64Dir) = 0))) then
        AbortCompileOnLine(SCompilerFilesSystemDirUsed);
      end;
      { CTL3D32.DLL }
      if not ExternalFile and
         (CompareText(Filename, 'CTL3D32.DLL') = 0) and
         (NewFileEntry^.MinVersion.WinVersion <> 0) and
         FileSizeAndCRCIs(SourceFile, 27136, $28A66C20) then
        AbortCompileOnLineFmt(SCompilerFilesUnsafeFile, ['CTL3D32.DLL, Windows NT-specific version']);
      { Remaining files }
      for I := Low(UnsafeSysFiles) to High(UnsafeSysFiles) do
        if CompareText(Filename, UnsafeSysFiles[I]) = 0 then
          AbortCompileOnLineFmt(SCompilerFilesUnsafeFile, [UnsafeSysFiles[I]]);
    end
    else begin
      { Files that MUST be deployed to the user's System directory }
      if IsRegistered then
        for I := Low(UnsafeNonSysRegFiles) to High(UnsafeNonSysRegFiles) do
          if CompareText(Filename, UnsafeNonSysRegFiles[I]) = 0 then
            AbortCompileOnLineFmt(SCompilerFilesSystemDirNotUsed, [UnsafeNonSysRegFiles[I]]);
    end;
  end;

  function IsExcluded(Text: String): Boolean;

    function CountBackslashes(S: PChar): Integer;
    begin
      Result := 0;
      while True do begin
        S := PathStrScan(S, '\');
        if S = nil then
          Break;
        Inc(Result);
        Inc(S);
      end;
    end;

  var
    I, J, TB, PB: Integer;
    T, P, TStart, TEnd: PChar;
    MatchFront: Boolean;
  begin
    if AExcludes.Count > 0 then begin
      Text := PathLowercase(Text);
      UniqueString(Text);
      T := PChar(Text);
      TB := CountBackslashes(T);

      for I := 0 to AExcludes.Count-1 do begin
        P := PChar(AExcludes[I]);

        { Leading backslash in an exclude pattern means 'match at the front
          instead of the end' }
        MatchFront := False;
        if P^ = '\' then begin
          MatchFront := True;
          Inc(P);
        end;

        PB := CountBackslashes(P);
        { The text must contain at least as many backslashes as the pattern
          for a match to be possible }
        if TB >= PB then begin
          TStart := T;
          if not MatchFront then begin
            { If matching at the end, advance TStart so that TStart and P point
              to the same number of components }
            for J := 1 to TB - PB do
              TStart := PathStrScan(TStart, '\') + 1;
            TEnd := nil;
          end
          else begin
            { If matching at the front, clip T to the same number of
              components as P }
            TEnd := T;
            for J := 1 to PB do
              TEnd := PathStrScan(TEnd, '\') + 1;
            TEnd := PathStrScan(TEnd, '\');
            if Assigned(TEnd) then
              TEnd^ := #0;
          end;

          if WildcardMatch(TStart, P) then begin
            Result := True;
            Exit;
          end;

          { Put back any backslash that was temporarily null'ed }
          if Assigned(TEnd) then
            TEnd^ := '\';
        end;
      end;
    end;

    Result := False;
  end;

  procedure AddToFileList(const FileList: TList; const Filename: String;
    const SizeLo, SizeHi: LongWord);
  var
    Rec: PFileListRec;
  begin
    FileList.Expand;
    New(Rec);
    Rec.Name := Filename;
    Rec.Size.Lo := SizeLo;
    Rec.Size.Hi := SizeHi;
    FileList.Add(Rec);
  end;

  procedure AddToDirList(const DirList: TList; const Dirname: String);
  var
    Rec: PDirListRec;
  begin
    DirList.Expand;
    New(Rec);
    Rec.Name := Dirname;
    DirList.Add(Rec);
  end;

  procedure BuildFileList(const SearchBaseDir, SearchSubDir, SearchWildcard: String;
    FileList, DirList: TList; CreateAllSubDirs: Boolean);
  { Searches for any non excluded files matching "SearchBaseDir + SearchSubDir + SearchWildcard"
    and adds them to FileList. }
  var
    SearchFullPath, FileName: String;
    H: THandle;
    FindData: TWin32FindData;
    OldFileListCount, OldDirListCount: Integer;
  begin
    SearchFullPath := SearchBaseDir + SearchSubDir + SearchWildcard;
    OldFileListCount := FileList.Count;
    OldDirListCount := DirList.Count;

    H := FindFirstFile(PChar(SearchFullPath), FindData);
    if H <> INVALID_HANDLE_VALUE then begin
      try
        repeat
          if FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0 then
            Continue;

          if SourceIsWildcard then begin
            if FindData.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN <> 0 then
              Continue;
            FileName := FindData.cFileName;
          end
          else
            FileName := SearchWildcard;  { use the case specified in the script }

          if IsExcluded(SearchSubDir + FileName) then
            Continue;

          AddToFileList(FileList, SearchSubDir + FileName, FindData.nFileSizeLow,
            FindData.nFileSizeHigh);

          CallIdleProc;
        until not SourceIsWildcard or not FindNextFile(H, FindData);
      finally
        Windows.FindClose(H);
      end;
    end else
      CallIdleProc;

    if RecurseSubdirs then begin
      H := FindFirstFile(PChar(SearchBaseDir + SearchSubDir + '*'), FindData);
      if H <> INVALID_HANDLE_VALUE then begin
        try
          repeat
            if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and
               (FindData.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN = 0) and
               (StrComp(FindData.cFileName, '.') <> 0) and
               (StrComp(FindData.cFileName, '..') <> 0) and
               not IsExcluded(SearchSubDir + FindData.cFileName) then
              BuildFileList(SearchBaseDir, SearchSubDir + FindData.cFileName + '\',
                SearchWildcard, FileList, DirList, CreateAllSubDirs);
          until not FindNextFile(H, FindData);
        finally
          Windows.FindClose(H);
        end;
      end;
    end;

    if SearchSubDir <> '' then begin
      { If both FileList and DirList didn't change size, this subdir won't be
        created during install, so add it to DirList now if CreateAllSubDirs is set }
      if CreateAllSubDirs and (FileList.Count = OldFileListCount) and
         (DirList.Count = OldDirListCount) then
        AddToDirList(DirList, SearchSubDir);
    end;
  end;

  procedure ApplyNewSign(var Sign: TSetupFileLocationSign;
    const NewSign: TSetupFileLocationSign; const ErrorMessage: String);
  begin
    if not (Sign in [fsNoSetting, NewSign]) then
      AbortCompileOnLineFmt(ErrorMessage,
        [ParamCommonFlags, SignFlags[Sign], SignFlags[NewSign]])
    else
      Sign := NewSign;
  end;

  procedure ProcessFileList(const FileListBaseDir: String; FileList: TList);
  var
    FileListRec: PFileListRec;
    CheckName: String;
    SourceFile: String;
    I, J: Integer;
    NewRunEntry: PSetupRunEntry;
  begin
    for I := 0 to FileList.Count-1 do begin
      FileListRec := FileList[I];

      if NewFileEntry = nil then begin
        NewFileEntry := AllocMem(SizeOf(TSetupFileEntry));
        SEDuplicateRec(PrevFileEntry, NewFileEntry,
          SizeOf(TSetupFileEntry), SetupFileEntryStrings, SetupFileEntryAnsiStrings);
      end;

      if Ext = 0 then begin
        if ADestName = '' then begin
          if not ExternalFile then
            NewFileEntry^.DestName := ADestDir + EscapeBraces(FileListRec.Name)
          else
            { Don't append the filename to DestName on 'external' files;
              it will be determined during installation }
            NewFileEntry^.DestName := ADestDir;
        end
        else begin
          if not ExternalFile then
            NewFileEntry^.DestName := ADestDir + EscapeBraces(PathExtractPath(FileListRec.Name)) +
              ADestName
          else
            NewFileEntry^.DestName := ADestDir + ADestName;
            { ^ user is already required to escape '{' in DestName }
          Include(NewFileEntry^.Options, foCustomDestName);
        end;
      end
      else
        NewFileEntry^.DestName := '';

      SourceFile := FileListBaseDir + FileListRec.Name;

      NewFileLocationEntry := nil;
      if not ExternalFile then begin
        if not DontMergeDuplicateFiles then begin
          { See if the source filename is already in the list of files to
            be compressed. If so, merge it. }
          J := FileLocationEntryFilenames.CaseInsensitiveIndexOf(SourceFile);
          if J <> -1 then begin
            NewFileLocationEntry := FileLocationEntries[J];
            NewFileEntry^.LocationEntry := J;
          end;
        end;
        if NewFileLocationEntry = nil then begin
          NewFileLocationEntry := AllocMem(SizeOf(TSetupFileLocationEntry));
          SetupHeader.CompressMethod := CompressMethod;
          FileLocationEntries.Add(NewFileLocationEntry);
          FileLocationEntryFilenames.Add(SourceFile);
          NewFileEntry^.LocationEntry := FileLocationEntries.Count-1;
          if NewFileEntry^.FileType = ftUninstExe then
            Include(NewFileLocationEntry^.Flags, foIsUninstExe);
          Inc6464(TotalBytesToCompress, FileListRec.Size);
          if SetupHeader.CompressMethod <> cmStored then
            Include(NewFileLocationEntry^.Flags, foChunkCompressed);
          if shEncryptionUsed in SetupHeader.Options then
            Include(NewFileLocationEntry^.Flags, foChunkEncrypted);
          if SolidBreak and UseSolidCompression then begin
            Include(NewFileLocationEntry^.Flags, foSolidBreak);
            { If the entry matches multiple files, it should only break prior
              to compressing the first one }
            SolidBreak := False;
          end;
        end;
        if Touch then
          Include(NewFileLocationEntry^.Flags, foApplyTouchDateTime);
        { Note: "nocompression"/"noencryption" on one file makes all merged
          copies uncompressed/unencrypted too }
        if NoCompression then
          Exclude(NewFileLocationEntry^.Flags, foChunkCompressed);
        if NoEncryption then
          Exclude(NewFileLocationEntry^.Flags, foChunkEncrypted);
        if Sign <> fsNoSetting then
          ApplyNewSign(NewFileLocationEntry.Sign, Sign, SCompilerParamErrorBadCombo3);
      end
      else begin
        NewFileEntry^.SourceFilename := SourceFile;
        NewFileEntry^.LocationEntry := -1;
      end;

      { Read version info }
      if not ExternalFile and not(foIgnoreVersion in NewFileEntry^.Options) and
         (NewFileLocationEntry^.Flags * [foVersionInfoValid, foVersionInfoNotValid] = []) then begin
        AddStatus(Format(SCompilerStatusFilesVerInfo, [SourceFile]));
        if GetVersionNumbers(SourceFile, VersionNumbers) then begin
          NewFileLocationEntry^.FileVersionMS := VersionNumbers.MS;
          NewFileLocationEntry^.FileVersionLS := VersionNumbers.LS;
          Include(NewFileLocationEntry^.Flags, foVersionInfoValid);
        end
        else
          Include(NewFileLocationEntry^.Flags, foVersionInfoNotValid);
      end;

      { Safety checks }
      if Ext = 0 then begin
        if ADestName <> '' then
          CheckName := ADestName
        else
          CheckName := PathExtractName(FileListRec.Name);

        CheckForUnsafeFile(CheckName, SourceFile,
          (foRegisterServer in NewFileEntry^.Options) or
          (foRegisterTypeLib in NewFileEntry^.Options));
        if (ADestDir = '{sys}\') and (foIgnoreVersion in NewFileEntry^.Options) and
           not SameText(PathExtractExt(CheckName), '.scr') then
          WarningsList.Add(Format(SCompilerFilesIgnoreVersionUsedUnsafely, [CheckName]));
      end;

      if ReadmeFile then begin
        NewRunEntry := AllocMem(Sizeof(TSetupRunEntry));
        NewRunEntry.Name := NewFileEntry.DestName;
        NewRunEntry.Components := NewFileEntry.Components;
        NewRunEntry.Tasks := NewFileEntry.Tasks;
        NewRunEntry.Languages := NewFileEntry.Languages;
        NewRunEntry.Check := NewFileEntry.Check;
        NewRunEntry.BeforeInstall := '';
        NewRunEntry.AfterInstall := '';
        NewRunEntry.MinVersion := NewFileEntry.MinVersion;
        NewRunEntry.OnlyBelowVersion := NewFileEntry.OnlyBelowVersion;
        NewRunEntry.Options := [roShellExec, roSkipIfDoesntExist, roPostInstall,
          roSkipIfSilent, roRunAsOriginalUser];
        NewRunEntry.ShowCmd := SW_SHOWNORMAL;
        NewRunEntry.Wait := rwNoWait;
        NewRunEntry.Verb := '';
        RunEntries.Insert(0, NewRunEntry);
        ShiftDebugEntryIndexes(deRun);  { because we inserted at the front }
      end;

      WriteDebugEntry(deFile, FileEntries.Count);
      FileEntries.Expand;
      PrevFileEntry := NewFileEntry;
      { nil before adding so there's no chance it could ever be double-freed }
      NewFileEntry := nil;
      FileEntries.Add(PrevFileEntry);

      CallIdleProc;
    end;
  end;

  procedure SortFileList(FileList: TList; L: Integer; const R: Integer;
    const ByExtension, ByName: Boolean);

    function Compare(const F1, F2: PFileListRec): Integer;

      function ComparePathStr(P1, P2: PChar): Integer;
      { Like CompareStr, but sorts backslashes correctly ('A\B' < 'AB\B') }
      var
        C1, C2: Char;
      begin
        repeat
          C1 := P1^;
          if C1 = '\' then
            C1 := #1;
          C2 := P2^;
          if C2 = '\' then
            C2 := #1;
          Result := Ord(C1) - Ord(C2);
          if Result <> 0 then
            Break;
          if C1 = #0 then
            Break;
          Inc(P1);
          Inc(P2);
        until False;
      end;

    var
      S1, S2: String;
    begin
      { Optimization: First check if we were passed the same string }
      if Pointer(F1.Name) = Pointer(F2.Name) then begin
        Result := 0;
        Exit;
      end;
      S1 := AnsiUppercase(F1.Name);  { uppercase to mimic NTFS's sort order }
      S2 := AnsiUppercase(F2.Name);
      if ByExtension then
        Result := CompareStr(PathExtractExt(S1), PathExtractExt(S2))
      else
        Result := 0;
      if ByName and (Result = 0) then
        Result := CompareStr(PathExtractName(S1), PathExtractName(S2));
      if Result = 0 then begin
        { To avoid randomness in the sorting, sort by path and then name }
        Result := ComparePathStr(PChar(PathExtractPath(S1)),
          PChar(PathExtractPath(S2)));
        if Result = 0 then
          Result := CompareStr(S1, S2);
      end;
    end;

  var
    I, J: Integer;
    P: PFileListRec;
  begin
    repeat
      I := L;
      J := R;
      P := FileList[(L + R) shr 1];
      repeat
        while Compare(FileList[I], P) < 0 do
          Inc(I);
        while Compare(FileList[J], P) > 0 do
          Dec(J);
        if I <= J then begin
          FileList.Exchange(I, J);
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        SortFileList(FileList, L, J, ByExtension, ByName);
      L := I;
    until I >= R;
  end;

  procedure ProcessDirList(DirList: TList);
  var
    DirListRec: PDirListRec;
    NewDirEntry: PSetupDirEntry;
    BaseFileEntry: PSetupFileEntry;
    I: Integer;
  begin
    if NewFileEntry <> nil then
      { If NewFileEntry is still assigned it means ProcessFileList didn't
        process any files (i.e. only directories were matched) }
      BaseFileEntry := NewFileEntry
    else
      BaseFileEntry := PrevFileEntry;

    if not(foDontCopy in BaseFileEntry.Options) then begin
      for I := 0 to DirList.Count-1 do begin
        DirListRec := DirList[I];

        NewDirEntry := AllocMem(Sizeof(TSetupDirEntry));
        NewDirEntry.DirName := ADestDir + EscapeBraces(DirListRec.Name);
        NewDirEntry.Components := BaseFileEntry.Components;
        NewDirEntry.Tasks := BaseFileEntry.Tasks;
        NewDirEntry.Languages := BaseFileEntry.Languages;
        NewDirEntry.Check := BaseFileEntry.Check;
        NewDirEntry.BeforeInstall := '';
        NewDirEntry.AfterInstall := '';
        NewDirEntry.MinVersion := BaseFileEntry.MinVersion;
        NewDirEntry.OnlyBelowVersion := BaseFileEntry.OnlyBelowVersion;
        NewDirEntry.Attribs := 0;
        NewDirEntry.PermissionsEntry := -1;
        NewDirEntry.Options := [];

        DirEntries.Add(NewDirEntry);
      end;
    end;
  end;

var
  FileList, DirList: TList;
  SortFilesByExtension, SortFilesByName: Boolean;
  I: Integer;
begin
  CallIdleProc;

  if Ext = 0 then
    ExtractParameters(Line, ParamInfo, Values);

  AExcludes := TStringList.Create();
  try
    PrevFileEntry := nil;
    NewFileEntry := AllocMem(SizeOf(TSetupFileEntry));
    try
      with NewFileEntry^ do begin
        MinVersion := SetupHeader.MinVersion;
        PermissionsEntry := -1;

        ADestName := '';
        ADestDir := '';
        AInstallFontName := '';
        AStrongAssemblyName := '';
        ReadmeFile := False;
        ExternalFile := False;
        RecurseSubdirs := False;
        AllowUnsafeFiles := False;
        Touch := False;
        SortFilesByExtension := False;
        NoCompression := False;
        NoEncryption := False;
        SolidBreak := False;
        ExternalSize.Hi := 0;
        ExternalSize.Lo := 0;
        SortFilesByName := False;
        Sign := fsNoSetting;

        case Ext of
          0: begin
               { Flags }
               while True do
                 case ExtractFlag(Values[paFlags].Data, Flags) of
                   -2: Break;
                   -1: AbortCompileParamError(SCompilerParamUnknownFlag2, ParamCommonFlags);
                   0: Include(Options, foConfirmOverwrite);
                   1: Include(Options, foUninsNeverUninstall);
                   2: ReadmeFile := True;
                   3: Include(Options, foRegisterServer);
                   4: Include(Options, foSharedFile);
                   5: Include(Options, foRestartReplace);
                   6: Include(Options, foDeleteAfterInstall);
                   7: Include(Options, foCompareTimeStamp);
                   8: Include(Options, foFontIsntTrueType);
                   9: Include(Options, foRegisterTypeLib);
                   10: ExternalFile := True;
                   11: Include(Options, foSkipIfSourceDoesntExist);
                   12: Include(Options, foOverwriteReadOnly);
                   13: Include(Options, foOnlyIfDestFileExists);
                   14: RecurseSubdirs := True;
                   15: Include(Options, foNoRegError);
                   16: AllowUnsafeFiles := True;
                   17: Include(Options, foUninsRestartDelete);
                   18: Include(Options, foOnlyIfDoesntExist);
                   19: Include(Options, foIgnoreVersion);
                   20: Include(Options, foPromptIfOlder);
                   21: Include(Options, foDontCopy);
                   22: Include(Options, foUninsRemoveReadOnly);
                   23: SortFilesByExtension := True;
                   24: Touch := True;
                   25: Include(Options, foReplaceSameVersionIfContentsDiffer);
                   26: NoEncryption := True;
                   27: NoCompression := True;
                   28: Include(Options, foDontVerifyChecksum);
                   29: Include(Options, foUninsNoSharedFilePrompt);
                   30: Include(Options, foCreateAllSubDirs);
                   31: Include(Options, fo32Bit);
                   32: Include(Options, fo64Bit);
                   33: SolidBreak := True;
                   34: Include(Options, foSetNTFSCompression);
                   35: Include(Options, foUnsetNTFSCompression);
                   36: SortFilesByName := True;
                   37: Include(Options, foGacInstall);
                   38: ApplyNewSign(Sign, fsYes, SCompilerParamErrorBadCombo2);
                   39: ApplyNewSign(Sign, fsOnce, SCompilerParamErrorBadCombo2);
                   40: ApplyNewSign(Sign, fsCheck, SCompilerParamErrorBadCombo2);
                 end;

               { Source }
               SourceWildcard := Values[paSource].Data;

               { DestDir }
               if Values[paDestDir].Found then
                 ADestDir := Values[paDestDir].Data
               else begin
                 if foDontCopy in Options then
                   { DestDir is optional when the 'dontcopy' flag is used }
                   ADestDir := '{tmp}'
                 else
                   AbortCompileParamError(SCompilerParamNotSpecified, ParamFilesDestDir);
               end;

               { DestName }
               if ConstPos('\', Values[paDestName].Data) <> 0 then
                 AbortCompileParamError(SCompilerParamNoBackslash, ParamFilesDestName);
               ADestName := Values[paDestName].Data;

               { CopyMode }
               if Values[paCopyMode].Found then begin
                 Values[paCopyMode].Data := Trim(Values[paCopyMode].Data);
                 if CompareText(Values[paCopyMode].Data, 'normal') = 0 then begin
                   Include(Options, foPromptIfOlder);
                   WarningsList.Add(Format(SCompilerFilesWarningCopyMode,
                     ['normal', 'promptifolder', 'promptifolder']));
                 end
                 else if CompareText(Values[paCopyMode].Data, 'onlyifdoesntexist') = 0 then begin
                   Include(Options, foOnlyIfDoesntExist);
                   WarningsList.Add(Format(SCompilerFilesWarningCopyMode,
                     ['onlyifdoesntexist', 'onlyifdoesntexist',
                      'onlyifdoesntexist']));
                 end
                 else if CompareText(Values[paCopyMode].Data, 'alwaysoverwrite') = 0 then begin
                   Include(Options, foIgnoreVersion);
                   WarningsList.Add(Format(SCompilerFilesWarningCopyMode,
                     ['alwaysoverwrite', 'ignoreversion', 'ignoreversion']));
                 end
                 else if CompareText(Values[paCopyMode].Data, 'alwaysskipifsameorolder') = 0 then begin
                   WarningsList.Add(SCompilerFilesWarningASISOO);
                 end
                 else if CompareText(Values[paCopyMode].Data, 'dontcopy') = 0 then begin
                   Include(Options, foDontCopy);
                   WarningsList.Add(Format(SCompilerFilesWarningCopyMode,
                     ['dontcopy', 'dontcopy', 'dontcopy']));
                 end
                 else
                   AbortCompileParamError(SCompilerParamInvalid2, ParamFilesCopyMode);
               end;

               { Attribs }
               while True do
                 case ExtractFlag(Values[paAttribs].Data, AttribsFlags) of
                   -2: Break;
                   -1: AbortCompileParamError(SCompilerParamUnknownFlag2, ParamFilesAttribs);
                   0: Attribs := Attribs or FILE_ATTRIBUTE_READONLY;
                   1: Attribs := Attribs or FILE_ATTRIBUTE_HIDDEN;
                   2: Attribs := Attribs or FILE_ATTRIBUTE_SYSTEM;
                   3: Attribs := Attribs or FILE_ATTRIBUTE_NOT_CONTENT_INDEXED;
                 end;

               { Permissions }
               ProcessPermissionsParameter(Values[paPermissions].Data, AccessMasks,
                 PermissionsEntry);

               { FontInstall }
               AInstallFontName := Values[paFontInstall].Data;

               { StrongAssemblyName }
               AStrongAssemblyName := Values[paStrongAssemblyName].Data;

               { Excludes }
               ProcessWildcardsParameter(Values[paExcludes].Data, AExcludes, SCompilerFilesExcludeTooLong);

               { ExternalSize }
               if Values[paExternalSize].Found then begin
                 if not ExternalFile then
                   AbortCompileOnLine(SCompilerFilesCantHaveNonExternalExternalSize);
                 if not StrToInteger64(Values[paExternalSize].Data, ExternalSize) then
                   AbortCompileParamError(SCompilerParamInvalid2, ParamFilesExternalSize);
                 Include(Options, foExternalSizePreset);
               end;

               { Common parameters }
               ProcessExpressionParameter(ParamCommonComponents, Values[paComponents].Data, EvalComponentIdentifier, True, Components);
               ProcessExpressionParameter(ParamCommonTasks, Values[paTasks].Data, EvalTaskIdentifier, True, Tasks);
               ProcessExpressionParameter(ParamCommonLanguages, Values[paLanguages].Data, EvalLanguageIdentifier, False, Languages);
               Check := Values[paCheck].Data;
               BeforeInstall := Values[paBeforeInstall].Data;
               AfterInstall := Values[paAfterInstall].Data;
               ProcessMinVersionParameter(Values[paMinVersion], MinVersion);
               ProcessOnlyBelowVersionParameter(Values[paOnlyBelowVersion], OnlyBelowVersion);
             end;
          1: begin
               SourceWildcard := '';
               FileType := ftUninstExe;
               { Ordinary hash comparison on unins*.exe won't really work since
                 Setup modifies the file after extracting it. Force same
                 version to always be overwritten by including the special
                 foOverwriteSameVersion option. }
               Options := [foOverwriteSameVersion];
               ExternalFile := True;
             end;
        end;

        if (ADestDir = '{tmp}') or (Copy(ADestDir, 1, 4) = '{tmp}\') then
          Include(Options, foDeleteAfterInstall);
        if foDeleteAfterInstall in Options then begin
          if foRestartReplace in Options then
            AbortCompileOnLineFmt(SCompilerFilesTmpBadFlag, ['restartreplace']);
          if foUninsNeverUninstall in Options then
            AbortCompileOnLineFmt(SCompilerFilesTmpBadFlag, ['uninsneveruninstall']);
          if foRegisterServer in Options then
            AbortCompileOnLineFmt(SCompilerFilesTmpBadFlag, ['regserver']);
          if foRegisterTypeLib in Options then
            AbortCompileOnLineFmt(SCompilerFilesTmpBadFlag, ['regtypelib']);
          if foSharedFile in Options then
            AbortCompileOnLineFmt(SCompilerFilesTmpBadFlag, ['sharedfile']);
          if foGacInstall in Options then
            AbortCompileOnLineFmt(SCompilerFilesTmpBadFlag, ['gacinstall']);
          Include(Options, foUninsNeverUninstall);
        end;

        if (fo32Bit in Options) and (fo64Bit in Options) then
          AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
            [ParamCommonFlags, '32bit', '64bit']);

        if AInstallFontName <> '' then begin
          if not(foFontIsntTrueType in Options) then
            AInstallFontName := AInstallFontName + ' (TrueType)';
          InstallFontName := AInstallFontName;
        end;

        if (foGacInstall in Options) and (AStrongAssemblyName = '') then
          AbortCompileOnLine(SCompilerFilesStrongAssemblyNameMustBeSpecified);
        if AStrongAssemblyName <> '' then
          StrongAssemblyName := AStrongAssemblyName;

        if not NoCompression and (foDontVerifyChecksum in Options) then
          AbortCompileOnLineFmt(SCompilerParamFlagMissing, ['nocompression', 'dontverifychecksum']);

        if ExternalFile then begin
          if (AExcludes.Count > 0) then
            AbortCompileOnLine(SCompilerFilesCantHaveExternalExclude)
          else if Sign <> fsNoSetting then
            AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
              [ParamCommonFlags, 'external', SignFlags[Sign]]);
        end;
        
        if (SignTools.Count = 0) and (Sign in [fsYes, fsOnce]) then
          Sign := fsNoSetting;

        if not RecurseSubdirs and (foCreateAllSubDirs in Options) then
          AbortCompileOnLineFmt(SCompilerParamFlagMissing, ['recursesubdirs', 'createallsubdirs']);

        if (foSetNTFSCompression in Options) and
           (foUnsetNTFSCompression in Options) then
          AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
            [ParamCommonFlags, 'setntfscompression', 'unsetntfscompression']);

        if (foSharedFile in Options) and
           (Copy(ADestDir, 1, Length('{syswow64}')) = '{syswow64}') then
          WarningsList.Add(SCompilerFilesWarningSharedFileSysWow64);

        SourceIsWildcard := IsWildcard(SourceWildcard);
        if ExternalFile then begin
          if RecurseSubdirs then
            Include(Options, foRecurseSubDirsExternal);
          CheckConst(SourceWildcard, MinVersion, []);
        end;
        if (ADestName <> '') and SourceIsWildcard then
          AbortCompileOnLine(SCompilerFilesDestNameCantBeSpecified);
        CheckConst(ADestDir, MinVersion, []);
        ADestDir := AddBackslash(ADestDir);
        CheckConst(ADestName, MinVersion, []);
        if not ExternalFile then
          SourceWildcard := PrependSourceDirName(SourceWildcard);

        CheckCheckOrInstall(ParamCommonCheck, Check, cikCheck);
        CheckCheckOrInstall(ParamCommonBeforeInstall, BeforeInstall, cikInstall);
        CheckCheckOrInstall(ParamCommonAfterInstall, AfterInstall, cikInstall);
      end;

      FileList := TLowFragList.Create();
      DirList := TLowFragList.Create();
      try
        if not ExternalFile then begin
          BuildFileList(PathExtractPath(SourceWildcard), '', PathExtractName(SourceWildcard), FileList, DirList, foCreateAllSubDirs in NewFileEntry.Options);
          if FileList.Count > 1 then
            SortFileList(FileList, 0, FileList.Count-1, SortFilesByExtension, SortFilesByName);
        end else
          AddToFileList(FileList, SourceWildcard, 0, 0);

        if FileList.Count > 0 then begin
          if not ExternalFile then
            ProcessFileList(PathExtractPath(SourceWildcard), FileList)
          else
            ProcessFileList('', FileList);
        end;

        if DirList.Count > 0 then begin
           { Dirs found that need to be created. Can only happen if not external. }
          ProcessDirList(DirList);
        end;

        if (FileList.Count = 0) and (DirList.Count = 0) then begin
          { Nothing found. Can only happen if not external. }
          if not(foSkipIfSourceDoesntExist in NewFileEntry^.Options) then begin
            if SourceIsWildcard then
              AbortCompileOnLineFmt(SCompilerFilesWildcardNotMatched, [SourceWildcard])
            else
              AbortCompileOnLineFmt(SCompilerSourceFileDoesntExist, [SourceWildcard]);
          end;
        end;
      finally
        for I := DirList.Count-1 downto 0 do
          Dispose(PDirListRec(DirList[I]));
        DirList.Free();
        for I := FileList.Count-1 downto 0 do
          Dispose(PFileListRec(FileList[I]));
        FileList.Free();
      end;
    finally
      { If NewFileEntry is still assigned at this point, either an exception
        occurred or no files were matched } 
      SEFreeRec(NewFileEntry, SetupFileEntryStrings, SetupFileEntryAnsiStrings);
    end;
  finally
    AExcludes.Free();
  end;
end;

procedure TSetupCompiler.EnumRunProc(const Line: PChar; const Ext: Integer);
type
  TParam = (paFlags, paFilename, paParameters, paWorkingDir, paRunOnceId,
    paDescription, paStatusMsg, paVerb, paComponents, paTasks, paLanguages,
    paCheck, paBeforeInstall, paAfterInstall, paMinVersion, paOnlyBelowVersion);
const
  ParamRunFilename = 'Filename';
  ParamRunParameters = 'Parameters';
  ParamRunWorkingDir = 'WorkingDir';
  ParamRunRunOnceId = 'RunOnceId';
  ParamRunDescription = 'Description';
  ParamRunStatusMsg = 'StatusMsg';
  ParamRunVerb = 'Verb';
  ParamInfo: array[TParam] of TParamInfo = (
    (Name: ParamCommonFlags; Flags: []),
    (Name: ParamRunFilename; Flags: [piRequired, piNoEmpty, piNoQuotes]),
    (Name: ParamRunParameters; Flags: []),
    (Name: ParamRunWorkingDir; Flags: []),
    (Name: ParamRunRunOnceId; Flags: []),
    (Name: ParamRunDescription; Flags: []),
    (Name: ParamRunStatusMsg; Flags: []),
    (Name: ParamRunVerb; Flags: []),
    (Name: ParamCommonComponents; Flags: []),
    (Name: ParamCommonTasks; Flags: []),
    (Name: ParamCommonLanguages; Flags: []),
    (Name: ParamCommonCheck; Flags: []),
    (Name: ParamCommonBeforeInstall; Flags: []),
    (Name: ParamCommonAfterInstall; Flags: []),
    (Name: ParamCommonMinVersion; Flags: []),
    (Name: ParamCommonOnlyBelowVersion; Flags: []));
  Flags: array[0..19] of PChar = (
    'nowait', 'waituntilidle', 'shellexec', 'skipifdoesntexist',
    'runminimized', 'runmaximized', 'showcheckbox', 'postinstall',
    'unchecked', 'skipifsilent', 'skipifnotsilent', 'hidewizard',
    'runhidden', 'waituntilterminated', '32bit', '64bit', 'runasoriginaluser',
    'runascurrentuser', 'dontlogparameters', 'logoutput');
var
  Values: array[TParam] of TParamValue;
  NewRunEntry: PSetupRunEntry;
  WaitFlagSpecified, RunAsOriginalUser, RunAsCurrentUser: Boolean;
begin
  ExtractParameters(Line, ParamInfo, Values);

  NewRunEntry := AllocMem(SizeOf(TSetupRunEntry));
  try
    with NewRunEntry^ do begin
      MinVersion := SetupHeader.MinVersion;
      ShowCmd := SW_SHOWNORMAL;
      WaitFlagSpecified := False;
      RunAsOriginalUser := False;
      RunAsCurrentUser := False;

      { Flags }
      while True do
        case ExtractFlag(Values[paFlags].Data, Flags) of
          -2: Break;
          -1: AbortCompileParamError(SCompilerParamUnknownFlag2, ParamCommonFlags);
          0: begin
               if WaitFlagSpecified then
                 AbortCompileOnLine(SCompilerRunMultipleWaitFlags);
               Wait := rwNoWait;
               WaitFlagSpecified := True;
             end;
          1: begin
               if WaitFlagSpecified then
                 AbortCompileOnLine(SCompilerRunMultipleWaitFlags);
               Wait := rwWaitUntilIdle;
               WaitFlagSpecified := True;
             end;
          2: Include(Options, roShellExec);
          3: Include(Options, roSkipIfDoesntExist);
          4: ShowCmd := SW_SHOWMINNOACTIVE;
          5: ShowCmd := SW_SHOWMAXIMIZED;
          6: begin
               if (Ext = 1) then
                 AbortCompileParamError(SCompilerParamUnsupportedFlag, ParamCommonFlags);
               WarningsList.Add(Format(SCompilerRunFlagObsolete, ['showcheckbox', 'postinstall']));
               Include(Options, roPostInstall);
             end;
          7: begin
               if (Ext = 1) then
                 AbortCompileParamError(SCompilerParamUnsupportedFlag, ParamCommonFlags);
               Include(Options, roPostInstall);
             end;
          8: begin
               if (Ext = 1) then
                 AbortCompileParamError(SCompilerParamUnsupportedFlag, ParamCommonFlags);
               Include(Options, roUnchecked);
             end;
          9: begin
               if (Ext = 1) then
                 AbortCompileParamError(SCompilerParamUnsupportedFlag, ParamCommonFlags);
               Include(Options, roSkipIfSilent);
             end;
          10: begin
               if (Ext = 1) then
                 AbortCompileParamError(SCompilerParamUnsupportedFlag, ParamCommonFlags);
               Include(Options, roSkipIfNotSilent);
             end;
          11: Include(Options, roHideWizard);
          12: ShowCmd := SW_HIDE;
          13: begin
               if WaitFlagSpecified then
                 AbortCompileOnLine(SCompilerRunMultipleWaitFlags);
               Wait := rwWaitUntilTerminated;
               WaitFlagSpecified := True;
             end;
          14: Include(Options, roRun32Bit);
          15: Include(Options, roRun64Bit);
          16: begin
               if (Ext = 1) then
                 AbortCompileParamError(SCompilerParamUnsupportedFlag, ParamCommonFlags);
               RunAsOriginalUser := True;
             end;
          17: RunAsCurrentUser := True;
          18: Include(Options, roDontLogParameters);
          19: Include(Options, roLogOutput);
        end;

      if not WaitFlagSpecified then begin
        if roShellExec in Options then
          Wait := rwNoWait
        else
          Wait := rwWaitUntilTerminated;
      end;

      if RunAsOriginalUser and RunAsCurrentUser then
        AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
          [ParamCommonFlags, 'runasoriginaluser', 'runascurrentuser']);
      if RunAsOriginalUser or
         (not RunAsCurrentUser and (roPostInstall in Options)) then
        Include(Options, roRunAsOriginalUser);

      if roLogOutput in Options then begin
        if roShellExec in Options then
          AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
            [ParamCommonFlags, 'logoutput', 'shellexec']);
        if (Wait <> rwWaitUntilTerminated) then
          AbortCompileOnLineFmt(SCompilerParamFlagMissing,
            ['waituntilterminated', 'logoutput']);
        if RunAsOriginalUser then
          AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
            [ParamCommonFlags, 'logoutput', 'runasoriginaluser']);
        if roRunAsOriginalUser in Options then
          AbortCompileOnLineFmt(SCompilerParamFlagMissing3,
            ['runascurrentuser', 'logoutput', 'postinstall']);
      end;

      { Filename }
      Name := Values[paFilename].Data;

      { Parameters }
      Parameters := Values[paParameters].Data;

      { WorkingDir }
      WorkingDir := Values[paWorkingDir].Data;

      { RunOnceId }
      if Values[paRunOnceId].Data <> '' then begin
        if Ext = 0 then
          AbortCompileOnLine(SCompilerRunCantUseRunOnceId);
      end else if Ext = 1 then
        MissingRunOnceIds := True;
      RunOnceId := Values[paRunOnceId].Data;

      { Description }
      if (Ext = 1) and (Values[paDescription].Data <> '') then
        AbortCompileOnLine(SCompilerUninstallRunCantUseDescription);
      Description := Values[paDescription].Data;

      { StatusMsg }
      StatusMsg := Values[paStatusMsg].Data;

      { Verb }
      if not (roShellExec in Options) and Values[paVerb].Found then
        AbortCompileOnLineFmt(SCompilerParamFlagMissing2,
          ['shellexec', 'Verb']);
      Verb := Values[paVerb].Data;

      { Common parameters }
      ProcessExpressionParameter(ParamCommonComponents, Values[paComponents].Data, EvalComponentIdentifier, True, Components);
      ProcessExpressionParameter(ParamCommonTasks, Values[paTasks].Data, EvalTaskIdentifier, True, Tasks);
      ProcessExpressionParameter(ParamCommonLanguages, Values[paLanguages].Data, EvalLanguageIdentifier, False, Languages);
      Check := Values[paCheck].Data;
      BeforeInstall := Values[paBeforeInstall].Data;
      AfterInstall := Values[paAfterInstall].Data;
      ProcessMinVersionParameter(Values[paMinVersion], MinVersion);
      ProcessOnlyBelowVersionParameter(Values[paOnlyBelowVersion], OnlyBelowVersion);

      if (roRun32Bit in Options) and (roRun64Bit in Options) then
        AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
          [ParamCommonFlags, '32bit', '64bit']);
      if (roRun32Bit in Options) and (roShellExec in Options) then
        AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
          [ParamCommonFlags, '32bit', 'shellexec']);
      if (roRun64Bit in Options) and (roShellExec in Options) then
        AbortCompileOnLineFmt(SCompilerParamErrorBadCombo2,
          [ParamCommonFlags, '64bit', 'shellexec']);

      CheckCheckOrInstall(ParamCommonCheck, Check, cikCheck);
      CheckCheckOrInstall(ParamCommonBeforeInstall, BeforeInstall, cikInstall);
      CheckCheckOrInstall(ParamCommonAfterInstall, AfterInstall, cikInstall);
      CheckConst(Name, MinVersion, []);
      CheckConst(Parameters, MinVersion, []);
      CheckConst(WorkingDir, MinVersion, []);
      CheckConst(RunOnceId, MinVersion, []);
      CheckConst(Description, MinVersion, []);
      CheckConst(StatusMsg, MinVersion, []);
      CheckConst(Verb, MinVersion, []);
    end;
  except
    SEFreeRec(NewRunEntry, SetupRunEntryStrings, SetupRunEntryAnsiStrings);
    raise;
  end;
  if Ext = 0 then begin
    WriteDebugEntry(deRun, RunEntries.Count);
    RunEntries.Add(NewRunEntry)
  end
  else begin
    WriteDebugEntry(deUninstallRun, UninstallRunEntries.Count);
    UninstallRunEntries.Add(NewRunEntry);
  end;
end;

type
  TLanguagesParam = (paName, paMessagesFile, paLicenseFile, paInfoBeforeFile, paInfoAfterFile);
const
  ParamLanguagesName = 'Name';
  ParamLanguagesMessagesFile = 'MessagesFile';
  ParamLanguagesLicenseFile = 'LicenseFile';
  ParamLanguagesInfoBeforeFile = 'InfoBeforeFile';
  ParamLanguagesInfoAfterFile = 'InfoAfterFile';
  LanguagesParamInfo: array[TLanguagesParam] of TParamInfo = (
    (Name: ParamLanguagesName; Flags: [piRequired, piNoEmpty]),
    (Name: ParamLanguagesMessagesFile; Flags: [piRequired, piNoEmpty]),
    (Name: ParamLanguagesLicenseFile; Flags: [piNoEmpty]),
    (Name: ParamLanguagesInfoBeforeFile; Flags: [piNoEmpty]),
    (Name: ParamLanguagesInfoAfterFile; Flags: [piNoEmpty]));

procedure TSetupCompiler.EnumLanguagesPreProc(const Line: PChar; const Ext: Integer);
var
  Values: array[TLanguagesParam] of TParamValue;
  NewPreLangData: TPreLangData;
  Filename: String;
begin
  ExtractParameters(Line, LanguagesParamInfo, Values);

  PreLangDataList.Expand;
  NewPreLangData := nil;
  try
    NewPreLangData := TPreLangData.Create;
    Filename := '';
    InitPreLangData(NewPreLangData);

    { Name }
    if not IsValidIdentString(Values[paName].Data, False, False) then
      AbortCompileOnLine(SCompilerLanguagesBadName);
    NewPreLangData.Name := Values[paName].Data;

    { MessagesFile }
    Filename := Values[paMessagesFile].Data;
  except
    NewPreLangData.Free;
    raise;
  end;
  PreLangDataList.Add(NewPreLangData);

  ReadMessagesFromFilesPre(Filename, PreLangDataList.Count-1);
end;

procedure TSetupCompiler.EnumLanguagesProc(const Line: PChar; const Ext: Integer);
var
  Values: array[TLanguagesParam] of TParamValue;
  NewLanguageEntry: PSetupLanguageEntry;
  NewLangData: TLangData;
  Filename: String;
begin
  ExtractParameters(Line, LanguagesParamInfo, Values);

  LanguageEntries.Expand;
  LangDataList.Expand;
  NewLangData := nil;
  NewLanguageEntry := AllocMem(SizeOf(TSetupLanguageEntry));
  try
    NewLangData := TLangData.Create;
    Filename := '';
    InitLanguageEntry(NewLanguageEntry^);

    { Name }
    if not IsValidIdentString(Values[paName].Data, False, False) then
      AbortCompileOnLine(SCompilerLanguagesBadName);
    NewLanguageEntry.Name := Values[paName].Data;

    { MessagesFile }
    Filename := Values[paMessagesFile].Data;

    { LicenseFile }
    if (Values[paLicenseFile].Data <> '') then begin
      AddStatus(Format(SCompilerStatusReadingInFile, [Values[paLicenseFile].Data]));
      ReadTextFile(PrependSourceDirName(Values[paLicenseFile].Data), LanguageEntries.Count,
        NewLanguageEntry.LicenseText);
    end;

    { InfoBeforeFile }
    if (Values[paInfoBeforeFile].Data <> '') then begin
      AddStatus(Format(SCompilerStatusReadingInFile, [Values[paInfoBeforeFile].Data]));
      ReadTextFile(PrependSourceDirName(Values[paInfoBeforeFile].Data), LanguageEntries.Count,
        NewLanguageEntry.InfoBeforeText);
    end;

    { InfoAfterFile }
    if (Values[paInfoAfterFile].Data <> '') then begin
      AddStatus(Format(SCompilerStatusReadingInFile, [Values[paInfoAfterFile].Data]));
      ReadTextFile(PrependSourceDirName(Values[paInfoAfterFile].Data), LanguageEntries.Count,
        NewLanguageEntry.InfoAfterText);
    end;
  except
    NewLangData.Free;
    SEFreeRec(NewLanguageEntry, SetupLanguageEntryStrings, SetupLanguageEntryAnsiStrings);
    raise;
  end;
  LanguageEntries.Add(NewLanguageEntry);
  LangDataList.Add(NewLangData);

  ReadMessagesFromFiles(Filename, LanguageEntries.Count-1);
end;

procedure TSetupCompiler.EnumMessagesProc(const Line: PChar; const Ext: Integer);
var
  P, P2: PChar;
  I, ID, LangIndex: Integer;
  N, M: String;
begin
  P := StrScan(Line, '=');
  if P = nil then
    AbortCompileOnLine(SCompilerMessagesMissingEquals);
  SetString(N, Line, P - Line);
  N := Trim(N);
  LangIndex := ExtractLangIndex(Self, N, Ext, False);
  ID := GetEnumValue(TypeInfo(TSetupMessageID), 'msg' + N);
  if ID = -1 then begin
    if LangIndex = -2 then
      AbortCompileOnLineFmt(SCompilerMessagesNotRecognizedDefault, [N])
    else begin
      if NotRecognizedMessagesWarning then begin
        if LineFilename = '' then
          WarningsList.Add(Format(SCompilerMessagesNotRecognizedWarning, [N]))
        else
          WarningsList.Add(Format(SCompilerMessagesNotRecognizedInFileWarning,
            [N, LineFilename]));
      end;
      Exit;
    end;
  end;
  Inc(P);
  M := P;
  { Replace %n with actual CR/LF characters }
  P2 := PChar(M);
  while True do begin
    P2 := StrPos(P2, '%n');
    if P2 = nil then Break;
    P2[0] := #13;
    P2[1] := #10;
    Inc(P2, 2);
  end;
  if LangIndex = -2 then begin
    { Special -2 value means store in DefaultLangData }
    DefaultLangData.Messages[TSetupMessageID(ID)] := M;
    DefaultLangData.MessagesDefined[TSetupMessageID(ID)] := True;
  end
  else begin
    for I := 0 to LangDataList.Count-1 do begin
      if (LangIndex <> -1) and (I <> LangIndex) then
        Continue;
      TLangData(LangDataList[I]).Messages[TSetupMessageID(ID)] := M;
      TLangData(LangDataList[I]).MessagesDefined[TSetupMessageID(ID)] := True;
    end;
  end;
end;

procedure TSetupCompiler.EnumCustomMessagesProc(const Line: PChar; const Ext: Integer);

  function ExpandNewlines(const S: String): String;
  { Replaces '%n' with #13#10 }
  var
    L, I: Integer;
  begin
    Result := S;
    L := Length(Result);
    I := 1;
    while I < L do begin
      if Result[I] = '%' then begin
        if Result[I+1] = 'n' then begin
          Result[I] := #13;
          Result[I+1] := #10;
        end;
        Inc(I);
      end;
      Inc(I);
    end;
  end;

var
  P: PChar;
  LangIndex: Integer;
  N: String;
  I: Integer;
  ExistingCustomMessageEntry, NewCustomMessageEntry: PSetupCustomMessageEntry;
begin
  P := StrScan(Line, '=');
  if P = nil then
    AbortCompileOnLine(SCompilerMessagesMissingEquals);
  SetString(N, Line, P - Line);
  N := Trim(N);
  LangIndex := ExtractLangIndex(Self, N, Ext, False);
  Inc(P);

  CustomMessageEntries.Expand;
  NewCustomMessageEntry := AllocMem(SizeOf(TSetupCustomMessageEntry));
  try
    if not IsValidIdentString(N, False, True) then
      AbortCompileOnLine(SCompilerCustomMessageBadName);

    { Delete existing entries}
    for I := CustomMessageEntries.Count-1 downto 0 do begin
      ExistingCustomMessageEntry := CustomMessageEntries[I];
      if (CompareText(ExistingCustomMessageEntry.Name, N) = 0) and
         ((LangIndex = -1) or (ExistingCustomMessageEntry.LangIndex = LangIndex)) then begin
        SEFreeRec(ExistingCustomMessageEntry, SetupCustomMessageEntryStrings,
          SetupCustomMessageEntryAnsiStrings);
        CustomMessageEntries.Delete(I);
      end;
    end;

    { Setup the new one }
    NewCustomMessageEntry.Name := N;
    NewCustomMessageEntry.Value := ExpandNewlines(P);
    NewCustomMessageEntry.LangIndex := LangIndex;
  except
    SEFreeRec(NewCustomMessageEntry, SetupCustomMessageEntryStrings, SetupCustomMessageEntryAnsiStrings);
    raise;
  end;
  CustomMessageEntries.Add(NewCustomMessageEntry);
end;

procedure TSetupCompiler.CheckCustomMessageDefinitions;
{ Checks 'language completeness' of custom message constants }
var
  MissingLang, Found: Boolean;
  I, J, K: Integer;
  CustomMessage1, CustomMessage2: PSetupCustomMessageEntry;
begin
  for I := 0 to CustomMessageEntries.Count-1 do begin
    CustomMessage1 := PSetupCustomMessageEntry(CustomMessageEntries[I]);
    if CustomMessage1.LangIndex <> -1 then begin
      MissingLang := False;
      for J := 0 to LanguageEntries.Count-1 do begin
        { Check whether the outer custom message name exists for this language }
        Found := False;
        for K := 0 to CustomMessageEntries.Count-1 do begin
          CustomMessage2 := PSetupCustomMessageEntry(CustomMessageEntries[K]);
          if CompareText(CustomMessage1.Name, CustomMessage2.Name) = 0 then begin
            if (CustomMessage2.LangIndex = -1) or (CustomMessage2.LangIndex = J) then begin
              Found := True;
              Break;
            end;
          end;
        end;
        if not Found then begin
          WarningsList.Add(Format(SCompilerCustomMessagesMissingLangWarning,
            [CustomMessage1.Name, PSetupLanguageEntry(LanguageEntries[J]).Name,
             PSetupLanguageEntry(LanguageEntries[CustomMessage1.LangIndex]).Name]));
          MissingLang := True;
        end;
      end;
      if MissingLang then begin
        { The custom message CustomMessage1.Name is not 'language complete'.
          Force it to be by setting CustomMessage1.LangIndex to -1. This will
          cause languages that do not define the custom message to use this
          one (i.e. the first definition of it). Note: Languages that do define
          the custom message in subsequent entries will override this entry,
          since Setup looks for the *last* matching entry. }
        CustomMessage1.LangIndex := -1;
      end;
    end;
  end;
end;

procedure TSetupCompiler.CheckCustomMessageReferences;
{ Checks existence of expected custom message constants }
var
  LineInfo: TLineInfo;
  Found: Boolean;
  S: String;
  I, J: Integer;
begin
  for I := 0 to ExpectedCustomMessageNames.Count-1 do begin
    Found := False;
    S := ExpectedCustomMessageNames[I];
    for J := 0 to CustomMessageEntries.Count-1 do begin
      if CompareText(PSetupCustomMessageEntry(CustomMessageEntries[J]).Name, S) = 0 then begin
        Found := True;
        Break;
      end;
    end;
    if not Found then begin
      LineInfo := TLineInfo(ExpectedCustomMessageNames.Objects[I]);
      LineFilename := LineInfo.Filename;
      LineNumber := LineInfo.FileLineNumber;
      AbortCompileFmt(SCompilerCustomMessagesMissingName, [S]);
    end;
  end;
end;

procedure TSetupCompiler.InitPreLangData(const APreLangData: TPreLangData);
{ Initializes a TPreLangData object with the default settings }
begin
  with APreLangData do begin
    Name := 'default';
    LanguageCodePage := 0;
  end;
end;

procedure TSetupCompiler.InitLanguageEntry(var ALanguageEntry: TSetupLanguageEntry);
{ Initializes a TSetupLanguageEntry record with the default settings }
begin
  with ALanguageEntry do begin
    Name := 'default';
    LanguageName := 'English';
    LanguageID := $0409;  { U.S. English }
    DialogFontName := DefaultDialogFontName;
    DialogFontSize := 8;
    TitleFontName := 'Arial';
    TitleFontSize := 29;
    WelcomeFontName := 'Verdana';
    WelcomeFontSize := 12;
    CopyrightFontName := 'Arial';
    CopyrightFontSize := 8;
    LicenseText := '';
    InfoBeforeText := '';
    InfoAfterText := '';
  end;
end;

procedure TSetupCompiler.ReadMessagesFromFilesPre(const AFiles: String;
  const ALangIndex: Integer);
var
  S, Filename: String;
begin
  S := AFiles;
  while True do begin
    Filename := ExtractStr(S, ',');
    if Filename = '' then
      Break;
    Filename := PathExpand(PrependSourceDirName(Filename));
    AddStatus(Format(SCompilerStatusReadingInFile, [Filename]));
    EnumIniSection(EnumLangOptionsPreProc, 'LangOptions', ALangIndex, False, True, Filename, True, True);
    CallIdleProc;
  end;
end;

procedure TSetupCompiler.ReadMessagesFromFiles(const AFiles: String;
  const ALangIndex: Integer);
var
  S, Filename: String;
begin
  S := AFiles;
  while True do begin
    Filename := ExtractStr(S, ',');
    if Filename = '' then
      Break;
    Filename := PathExpand(PrependSourceDirName(Filename));
    AddStatus(Format(SCompilerStatusReadingInFile, [Filename]));
    EnumIniSection(EnumLangOptionsProc, 'LangOptions', ALangIndex, False, True, Filename, True, False);
    CallIdleProc;
    EnumIniSection(EnumMessagesProc, 'Messages', ALangIndex, False, True, Filename, True, False);
    CallIdleProc;
    EnumIniSection(EnumCustomMessagesProc, 'CustomMessages', ALangIndex, False, True, Filename, True, False);
    CallIdleProc;
  end;
end;

procedure TSetupCompiler.ReadDefaultMessages;
var
  J: TSetupMessageID;
begin
  { Read messages from Default.isl into DefaultLangData }
  EnumIniSection(EnumMessagesProc, 'Messages', -2, False, True, 'compiler:Default.isl', True, False);
  CallIdleProc;

  { Check for missing messages in Default.isl }
  for J := Low(DefaultLangData.Messages) to High(DefaultLangData.Messages) do
    if not DefaultLangData.MessagesDefined[J] then
      AbortCompileFmt(SCompilerMessagesMissingDefaultMessage,
        [Copy(GetEnumName(TypeInfo(TSetupMessageID), Ord(J)), 4, Maxint)]);
        { ^ Copy(..., 4, Maxint) is to skip past "msg" }
end;

procedure TSetupCompiler.ReadMessagesFromScriptPre;

  procedure CreateDefaultLanguageEntryPre;
  var
    NewPreLangData: TPreLangData;
  begin
    PreLangDataList.Expand;
    NewPreLangData := nil;
    try
      NewPreLangData := TPreLangData.Create;
      InitPreLangData(NewPreLangData);
    except
      NewPreLangData.Free;
      raise;
    end;
    PreLangDataList.Add(NewPreLangData);

    ReadMessagesFromFilesPre('compiler:Default.isl', PreLangDataList.Count-1);
  end;

begin
  { If there were no [Languages] entries, take this opportunity to create a
    default language }
  if PreLangDataList.Count = 0 then begin
    CreateDefaultLanguageEntryPre;
    CallIdleProc;
  end;

  { Then read the [LangOptions] section in the script }
  AddStatus(SCompilerStatusReadingInScriptMsgs);
  EnumIniSection(EnumLangOptionsPreProc, 'LangOptions', -1, False, True, '', True, False);
  CallIdleProc;
end;

procedure TSetupCompiler.ReadMessagesFromScript;

  procedure CreateDefaultLanguageEntry;
  var
    NewLanguageEntry: PSetupLanguageEntry;
    NewLangData: TLangData;
  begin
    LanguageEntries.Expand;
    LangDataList.Expand;
    NewLangData := nil;
    NewLanguageEntry := AllocMem(SizeOf(TSetupLanguageEntry));
    try
      NewLangData := TLangData.Create;
      InitLanguageEntry(NewLanguageEntry^);
    except
      NewLangData.Free;
      SEFreeRec(NewLanguageEntry, SetupLanguageEntryStrings, SetupLanguageEntryAnsiStrings);
      raise;
    end;
    LanguageEntries.Add(NewLanguageEntry);
    LangDataList.Add(NewLangData);

    ReadMessagesFromFiles('compiler:Default.isl', LanguageEntries.Count-1);
  end;

  function IsOptional(const MessageID: TSetupMessageID): Boolean;
  begin
    Result := False; { Currently there are no optional messages }
  end;

var
  I: Integer;
  LangData: TLangData;
  J: TSetupMessageID;
begin
  { If there were no [Languages] entries, take this opportunity to create a
    default language }
  if LanguageEntries.Count = 0 then begin
    CreateDefaultLanguageEntry;
    CallIdleProc;
  end;

  { Then read the [LangOptions] & [Messages] & [CustomMessages] sections in the script }
  AddStatus(SCompilerStatusReadingInScriptMsgs);
  EnumIniSection(EnumLangOptionsProc, 'LangOptions', -1, False, True, '', True, False);
  CallIdleProc;
  EnumIniSection(EnumMessagesProc, 'Messages', -1, False, True, '', True, False);
  CallIdleProc;
  EnumIniSection(EnumCustomMessagesProc, 'CustomMessages', -1, False, True, '', True, False);
  CallIdleProc;

  { Check for missing messages }
  for I := 0 to LanguageEntries.Count-1 do begin
    LangData := LangDataList[I];
    for J := Low(LangData.Messages) to High(LangData.Messages) do
      if not LangData.MessagesDefined[J] and not IsOptional(J) then begin
        { Use the message from Default.isl }
        if MissingMessagesWarning and not (J in [msgHelpTextNote, msgTranslatorNote]) then
          WarningsList.Add(Format(SCompilerMessagesMissingMessageWarning,
            [Copy(GetEnumName(TypeInfo(TSetupMessageID), Ord(J)), 4, Maxint),
             PSetupLanguageEntry(LanguageEntries[I]).Name]));
            { ^ Copy(..., 4, Maxint) is to skip past "msg" }
        LangData.Messages[J] := DefaultLangData.Messages[J];
      end;
  end;
  CallIdleProc;
end;

procedure TSetupCompiler.PopulateLanguageEntryData;
{ Fills in each language entry's Data field, based on the messages in
  LangDataList }
type
  PMessagesDataStructure = ^TMessagesDataStructure;
  TMessagesDataStructure = packed record
    ID: TMessagesHdrID;
    Header: TMessagesHeader;
    MsgData: array[0..0] of Byte;
  end;
var
  L: Integer;
  LangData: TLangData;
  M: TMemoryStream;
  I: TSetupMessageID;
  Header: TMessagesHeader;
begin
  for L := 0 to LanguageEntries.Count-1 do begin
    LangData := LangDataList[L];

    M := TMemoryStream.Create;
    try
      M.WriteBuffer(MessagesHdrID, SizeOf(MessagesHdrID));
      FillChar(Header, SizeOf(Header), 0);
      M.WriteBuffer(Header, SizeOf(Header));  { overwritten later }
      for I := Low(LangData.Messages) to High(LangData.Messages) do
        M.WriteBuffer(PChar(LangData.Messages[I])^, (Length(LangData.Messages[I]) + 1) * SizeOf(LangData.Messages[I][1]));

      Header.NumMessages := Ord(High(LangData.Messages)) - Ord(Low(LangData.Messages)) + 1;
      Header.TotalSize := M.Size;
      Header.NotTotalSize := not Header.TotalSize;
      Header.CRCMessages := GetCRC32(PMessagesDataStructure(M.Memory).MsgData,
        M.Size - (SizeOf(MessagesHdrID) + SizeOf(Header)));
      PMessagesDataStructure(M.Memory).Header := Header;

      SetString(PSetupLanguageEntry(LanguageEntries[L]).Data, PAnsiChar(M.Memory),
        M.Size);
    finally
      M.Free;
    end;
  end;
end;

procedure TSetupCompiler.EnumCodeProc(const Line: PChar; const Ext: Integer);
var
  CodeTextLineInfo: TLineInfo;
begin
  CodeTextLineInfo := TLineInfo.Create;
  CodeTextLineInfo.Filename := LineFilename;
  CodeTextLineInfo.FileLineNumber := LineNumber;
  CodeText.AddObject(Line, CodeTextLineInfo);
end;

procedure TSetupCompiler.ReadCode;
begin
  { Read [Code] section }
  AddStatus(SCompilerStatusReadingCode);
  EnumIniSection(EnumCodeProc, 'Code', 0, False, False, '', False, False);
  CallIdleProc;
end;

procedure TSetupCompiler.CodeCompilerOnLineToLineInfo(const Line: LongInt; var Filename: String; var FileLine: LongInt);
var
  CodeTextLineInfo: TLineInfo;
begin
  if (Line > 0) and (Line <= CodeText.Count) then begin
    CodeTextLineInfo := TLineInfo(CodeText.Objects[Line-1]);
    Filename := CodeTextLineInfo.Filename;
    FileLine := CodeTextLineInfo.FileLineNumber;
  end;
end;

procedure TSetupCompiler.CodeCompilerOnUsedLine(const Filename: String; const Line, Position: LongInt; const IsProcExit: Boolean);
var
  OldLineFilename: String;
  OldLineNumber: Integer;
begin
  OldLineFilename := LineFilename;
  OldLineNumber := LineNumber;
  try
    LineFilename := Filename;
    LineNumber := Line;
    WriteDebugEntry(deCodeLine, Position, IsProcExit);
  finally
    LineFilename := OldLineFilename;
    LineNumber := OldLineNumber;
  end;
end;

procedure TSetupCompiler.CodeCompilerOnUsedVariable(const Filename: String; const Line, Col, Param1, Param2, Param3: LongInt; const Param4: AnsiString);
var
  Rec: TVariableDebugEntry;
begin
  if Length(Param4)+1 <= SizeOf(Rec.Param4) then begin
    Rec.FileIndex := FilenameToFileIndex(Filename);
    Rec.LineNumber := Line;
    Rec.Col := Col;
    Rec.Param1 := Param1;
    Rec.Param2 := Param2;
    Rec.Param3 := Param3;
    FillChar(Rec.Param4, SizeOf(Rec.Param4), 0);
    AnsiStrings.StrPCopy(Rec.Param4, Param4);
    CodeDebugInfo.WriteBuffer(Rec, SizeOf(Rec));
    Inc(VariableDebugEntryCount);
  end;
end;

procedure TSetupCompiler.CodeCompilerOnError(const Msg: String; const ErrorFilename: String; const ErrorLine: LongInt);
begin
  LineFilename := ErrorFilename;
  LineNumber := ErrorLine;
  AbortCompile(Msg);
end;

procedure TSetupCompiler.CodeCompilerOnWarning(const Msg: String);
begin
  WarningsList.Add(Msg);
end;

procedure TSetupCompiler.CompileCode;
var
  CodeStr: String;
  CompiledCodeDebugInfo: AnsiString;
begin
  { Compile CodeText }
  if (CodeText.Count > 0) or (CodeCompiler.ExportCount > 0) then begin
    if CodeText.Count > 0 then
      AddStatus(SCompilerStatusCompilingCode);

    //don't forget highlighter!
    //setup
    CodeCompiler.AddExport('InitializeSetup', 'Boolean', True, False, '', 0);
    CodeCompiler.AddExport('DeinitializeSetup', '0', True, False, '', 0);
    CodeCompiler.AddExport('CurStepChanged', '0 @TSetupStep', True, False, '', 0);
    CodeCompiler.AddExport('NextButtonClick', 'Boolean @LongInt', True, False, '', 0);
    CodeCompiler.AddExport('BackButtonClick', 'Boolean @LongInt', True, False, '', 0);
    CodeCompiler.AddExport('CancelButtonClick', '0 @LongInt !Boolean !Boolean', True, False, '', 0);
    CodeCompiler.AddExport('ShouldSkipPage', 'Boolean @LongInt', True, False, '', 0);
    CodeCompiler.AddExport('CurPageChanged', '0 @LongInt', True, False, '', 0);
    CodeCompiler.AddExport('CheckPassword', 'Boolean @String', True, False, '', 0);
    CodeCompiler.AddExport('NeedRestart', 'Boolean', True, False, '', 0);
    CodeCompiler.AddExport('RegisterPreviousData', '0 @LongInt', True, False, '', 0);
    CodeCompiler.AddExport('CheckSerial', 'Boolean @String', True, False, '', 0);
    CodeCompiler.AddExport('InitializeWizard', '0', True, False, '', 0);
    CodeCompiler.AddExport('RegisterExtraCloseApplicationsResources', '0', True, False, '', 0);
    CodeCompiler.AddExport('CurInstallProgressChanged', '0 @LongInt @LongInt', True, False, '', 0);
    CodeCompiler.AddExport('UpdateReadyMemo', 'String @String @String @String @String @String @String @String @String', True, False, '', 0);
    CodeCompiler.AddExport('GetCustomSetupExitCode', 'LongInt', True, False, '', 0);
    CodeCompiler.AddExport('PrepareToInstall', 'String !Boolean', True, False, '', 0);
    //uninstall
    CodeCompiler.AddExport('InitializeUninstall', 'Boolean', True, False, '', 0);
    CodeCompiler.AddExport('DeinitializeUninstall', '0', True, False, '', 0);
    CodeCompiler.AddExport('CurUninstallStepChanged', '0 @TUninstallStep', True, False, '', 0);
    CodeCompiler.AddExport('UninstallNeedRestart', 'Boolean', True, False, '', 0);
    CodeCompiler.AddExport('InitializeUninstallProgressForm', '0', True, False, '', 0);

    CodeStr := CodeText.Text;
    { Remove trailing CR-LF so that ROPS will never report an error on
      line CodeText.Count, one past the last actual line }
    if Length(CodeStr) >= Length(#13#10) then
      SetLength(CodeStr, Length(CodeStr) - Length(#13#10));
    CodeCompiler.Compile(CodeStr, CompiledCodeText, CompiledCodeDebugInfo);

    if CodeCompiler.FunctionFound('SkipCurPage') then
      AbortCompileFmt(SCompilerCodeUnsupportedEventFunction, ['SkipCurPage',
        'ShouldSkipPage']);

    WriteCompiledCodeText(CompiledCodeText);
    WriteCompiledCodeDebugInfo(CompiledCodeDebugInfo);
  end else begin
    CompiledCodeText := '';

    { Check if there were references to [Code] functions despite there being
      no [Code] section }
    CodeCompiler.CheckExports();
  end;
end;

procedure TSetupCompiler.AddBytesCompressedSoFar(const Value: Cardinal);
begin
  Inc64(BytesCompressedSoFar, Value);
end;

procedure TSetupCompiler.AddBytesCompressedSoFar(const Value: Integer64);
begin
  Inc6464(BytesCompressedSoFar, Value);
end;

procedure TSetupCompiler.AddPreprocOption(const Value: String);
begin
  PreprocOptionsString := PreprocOptionsString + Value + #0;
end;

procedure TSetupCompiler.AddSignTool(const Name, Command: String);
var
  SignTool: TSignTool;
begin
  SignToolList.Expand;
  SignTool := TSignTool.Create();
  SignTool.Name := Name;
  SignTool.Command := Command;
  SignToolList.Add(SignTool);
end;

procedure TSetupCompiler.Sign(AExeFilename: String);
var
  I, SignToolIndex: Integer;
  SignTool: TSignTool;
begin
  for I := 0 to SignTools.Count - 1 do begin
    SignToolIndex := FindSignToolIndexByName(SignTools[I]); //can't fail, already checked
    SignTool := TSignTool(SignToolList[SignToolIndex]);
    SignCommand(SignTool.Name, SignTool.Command, SignToolsParams[I], AExeFilename, SignToolRetryCount, SignToolRetryDelay, SignToolMinimumTimeBetween, SignToolRunMinimized);
  end;
end;

procedure SignCommandLog(const S: String; const Error, FirstLine: Boolean; const Data: NativeInt);
begin
  if S <> '' then begin
    var SetupCompiler := TSetupCompiler(Data);
    SetupCompiler.AddStatus('   ' + S, Error);
  end;
end;

procedure TSetupCompiler.SignCommand(const AName, ACommand, AParams, AExeFilename: String; const RetryCount, RetryDelay, MinimumTimeBetween: Integer; const RunMinimized: Boolean);

  function FmtCommand(S: PChar; const AParams, AFileName: String; var AFileNameSequenceFound: Boolean): String;
  var
    P: PChar;
    Z: String;
  begin
    Result := '';
    AFileNameSequenceFound := False;
    if S = nil then Exit;
    while True do begin
      P := StrScan(S, '$');
      if P = nil then begin
        Result := Result + S;
        Break;
      end;
      if P <> S then begin
        SetString(Z, S, P - S);
        Result := Result + Z;
        S := P;
      end;
      Inc(P);
      if (P^ = 'p') then begin
        Result := Result + AParams;
        Inc(S, 2);
      end
      else if (P^ = 'f') then begin
        Result := Result + '"' + AFileName + '"';
        AFileNameSequenceFound := True;
        Inc(S, 2);
      end
      else if (P^ = 'q') then begin
        Result := Result + '"';
        Inc(S, 2);
      end
      else begin
        Result := Result + '$';
        Inc(S);
        if P^ = '$' then
          Inc(S);
      end;
    end;
  end;

  procedure InternalSignCommand(const AFormattedCommand: String;
    const Delay: Cardinal);
  begin
    {Also see IsppFuncs' Exec }

    if Delay <> 0 then begin
      AddStatus(Format(SCompilerStatusSigningWithDelay, [AName, Delay, AFormattedCommand]));
      Sleep(Delay);
    end else
      AddStatus(Format(SCompilerStatusSigning, [AName, AFormattedCommand]));

    LastSignCommandStartTick := GetTickCount;

    var StartupInfo: TStartupInfo;
    FillChar(StartupInfo, SizeOf(StartupInfo), 0);
    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := IfThen(RunMinimized, SW_SHOWMINNOACTIVE, SW_SHOWNORMAL);

    var OutputReader := TCreateProcessOutputReader.Create(SignCommandLog, NativeInt(Self));
    try
      var InheritHandles := True;
      var dwCreationFlags: DWORD := CREATE_DEFAULT_ERROR_MODE or CREATE_NO_WINDOW;
      OutputReader.UpdateStartupInfo(StartupInfo);

      var ProcessInfo: TProcessInformation;
      if not CreateProcess(nil, PChar(AFormattedCommand), nil, nil, InheritHandles,
         dwCreationFlags, nil, PChar(CompilerDir), StartupInfo, ProcessInfo) then begin
        var LastError := GetLastError;
        AbortCompileFmt(SCompilerSignToolCreateProcessFailed, [LastError,
          Win32ErrorString(LastError)]);
      end;

      { Don't need the thread handle, so close it now }
      CloseHandle(ProcessInfo.hThread);
      OutputReader.NotifyCreateProcessDone;

      try
        while True do begin
          case WaitForSingleObject(ProcessInfo.hProcess, 50) of
            WAIT_OBJECT_0: Break;
            WAIT_TIMEOUT:
              begin
                OutputReader.Read(False);
                CallIdleProc(True); { Doesn't allow an Abort }
              end;
          else
            AbortCompile('Sign: WaitForSingleObject failed');
          end;
        end;
        OutputReader.Read(True);
        var ExitCode: DWORD;
        if not GetExitCodeProcess(ProcessInfo.hProcess, ExitCode) then
          AbortCompile('Sign: GetExitCodeProcess failed');
        if ExitCode <> 0 then
          AbortCompileFmt(SCompilerSignToolNonZeroExitCode, [ExitCode]);
      finally
        CloseHandle(ProcessInfo.hProcess);
      end;
    finally
      OutputReader.Free;
    end;
  end;

var
  Params, Command: String;
  MinimumTimeBetweenDelay: Integer;
  I: Integer;
  FileNameSequenceFound1, FileNameSequenceFound2: Boolean;
begin
  Params := FmtCommand(PChar(AParams), '', AExeFileName, FileNameSequenceFound1);
  Command := FmtCommand(PChar(ACommand), Params, AExeFileName, FileNameSequenceFound2);
  
  if not FileNameSequenceFound1 and not FileNameSequenceFound2 then
    AbortCompileFmt(SCompilerSignToolFileNameSequenceNotFound, [AName]);
  
  for I := 0 to RetryCount do begin
    try
      if (MinimumTimeBetween <> 0) and (LastSignCommandStartTick <> 0) then begin
        MinimumTimeBetweenDelay := MinimumTimeBetween - Integer(GetTickCount - LastSignCommandStartTick);
        if MinimumTimeBetweenDelay < 0 then
          MinimumTimeBetweenDelay := 0;
      end else
        MinimumTimeBetweenDelay := 0;
      InternalSignCommand(Command, MinimumTimeBetweenDelay);
      Break;
    except on E: Exception do
      if I < RetryCount then begin
        AddStatus(Format(SCompilerStatusWillRetrySigning, [E.Message, RetryCount-I]));
        Sleep(RetryDelay);
      end else
        raise;
    end;
  end;
end;

procedure TSetupCompiler.Compile;

  procedure InitDebugInfo;
  var
    Header: TDebugInfoHeader;
  begin
    DebugEntryCount := 0;
    VariableDebugEntryCount := 0;
    DebugInfo.Clear;
    CodeDebugInfo.Clear;
    Header.ID := DebugInfoHeaderID;
    Header.Version := DebugInfoHeaderVersion;
    Header.DebugEntryCount := 0;
    Header.CompiledCodeTextLength := 0;
    Header.CompiledCodeDebugInfoLength := 0;
    DebugInfo.WriteBuffer(Header, SizeOf(Header));
  end;

  procedure FinalizeDebugInfo;
  var
    Header: TDebugInfoHeader;
  begin
    DebugInfo.CopyFrom(CodeDebugInfo, 0);
    { Update the header }
    DebugInfo.Seek(0, soFromBeginning);
    DebugInfo.ReadBuffer(Header, SizeOf(Header));
    Header.DebugEntryCount := DebugEntryCount;
    Header.VariableDebugEntryCount := VariableDebugEntryCount;
    Header.CompiledCodeTextLength := CompiledCodeTextLength;
    Header.CompiledCodeDebugInfoLength := CompiledCodeDebugInfoLength;
    DebugInfo.Seek(0, soFromBeginning);
    DebugInfo.WriteBuffer(Header, SizeOf(Header));
  end;

  procedure EmptyOutputDir(const Log: Boolean);

    procedure DelFile(const Filename: String);
    begin
      if DeleteFile(OutputDir + Filename) and Log then
        AddStatus(Format(SCompilerStatusDeletingPrevious, [Filename]));
    end;

  var
    H: THandle;
    FindData: TWin32FindData;
    N: String;
    I: Integer;
    HasNumbers: Boolean;
  begin
    { Delete SETUP.* and SETUP-*.BIN if they existed in the output directory }
    if OutputBaseFilename <> '' then begin
      DelFile(OutputBaseFilename + '.exe');
      if OutputDir <> '' then begin
        H := FindFirstFile(PChar(OutputDir + OutputBaseFilename + '-*.bin'), FindData);
        if H <> INVALID_HANDLE_VALUE then begin
          try
            repeat
              if FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then begin
                N := FindData.cFileName;
                if PathStartsWith(N, OutputBaseFilename) then begin
                  I := Length(OutputBaseFilename) + 1;
                  if (I <= Length(N)) and (N[I] = '-') then begin
                    Inc(I);
                    HasNumbers := False;
                    while (I <= Length(N)) and CharInSet(N[I], ['0'..'9']) do begin
                      HasNumbers := True;
                      Inc(I);
                    end;
                    if HasNumbers then begin
                      if (I <= Length(N)) and CharInSet(UpCase(N[I]), ['A'..'Z']) then
                        Inc(I);
                      if CompareText(Copy(N, I, Maxint), '.bin') = 0 then
                        DelFile(N);
                    end;
                  end;
                end;
              end;
            until not FindNextFile(H, FindData);
          finally
            Windows.FindClose(H);
          end;
        end;
      end;
    end;
  end;

  procedure FreeListItems(const List: TList; const NumStrings, NumAnsiStrings: Integer);
  var
    I: Integer;
  begin
    for I := List.Count-1 downto 0 do begin
      SEFreeRec(List[I], NumStrings, NumAnsiStrings);
      List.Delete(I);
    end;
  end;

  procedure FreePreLangData;
  var
    I: Integer;
  begin
    for I := PreLangDataList.Count-1 downto 0 do begin
      TPreLangData(PreLangDataList[I]).Free;
      PreLangDataList.Delete(I);
    end;
  end;

  procedure FreeLangData;
  var
    I: Integer;
  begin
    for I := LangDataList.Count-1 downto 0 do begin
      TLangData(LangDataList[I]).Free;
      LangDataList.Delete(I);
    end;
  end;

  procedure FreeScriptFiles;
  var
    I: Integer;
    SL: TObject;
  begin
    for I := ScriptFiles.Count-1 downto 0 do begin
      SL := ScriptFiles.Objects[I];
      ScriptFiles.Delete(I);
      SL.Free;
    end;
  end;

  procedure FreeLineInfoList(L: TStringList);
  var
    I: Integer;
    LineInfo: TLineInfo;
  begin
    for I := L.Count-1 downto 0 do begin
      LineInfo := TLineInfo(L.Objects[I]);
      L.Delete(I);
      LineInfo.Free;
    end;
  end;

type
  PCopyBuffer = ^TCopyBuffer;
  TCopyBuffer = array[0..32767] of Char;
var
  SetupFile: TFile;
  ExeFile: TFile;
  LicenseText, InfoBeforeText, InfoAfterText: AnsiString;
  WizardImages, WizardSmallImages: TObjectList<TCustomMemoryStream>;
  DecompressorDLL: TMemoryStream;

  SetupLdrOffsetTable: TSetupLdrOffsetTable;
  SizeOfExe, SizeOfHeaders: Longint;

  function WriteSetup0(const F: TFile): Longint;

    procedure WriteStream(Stream: TCustomMemoryStream; W: TCompressedBlockWriter);
    var
      Size: Longint;
    begin
      Size := Stream.Size;
      W.Write(Size, SizeOf(Size));
      W.Write(Stream.Memory^, Size);
    end;

  var
    Pos: Cardinal;
    J: Integer;
    W: TCompressedBlockWriter;
  begin
    Pos := F.Position.Lo;

    F.WriteBuffer(SetupID, SizeOf(SetupID));

    SetupHeader.NumLanguageEntries := LanguageEntries.Count;
    SetupHeader.NumCustomMessageEntries := CustomMessageEntries.Count;
    SetupHeader.NumPermissionEntries := PermissionEntries.Count;
    SetupHeader.NumTypeEntries := TypeEntries.Count;
    SetupHeader.NumComponentEntries := ComponentEntries.Count;
    SetupHeader.NumTaskEntries := TaskEntries.Count;
    SetupHeader.NumDirEntries := DirEntries.Count;
    SetupHeader.NumFileEntries := FileEntries.Count;
    SetupHeader.NumFileLocationEntries := FileLocationEntries.Count;
    SetupHeader.NumIconEntries := IconEntries.Count;
    SetupHeader.NumIniEntries := IniEntries.Count;
    SetupHeader.NumRegistryEntries := RegistryEntries.Count;
    SetupHeader.NumInstallDeleteEntries := InstallDeleteEntries.Count;
    SetupHeader.NumUninstallDeleteEntries := UninstallDeleteEntries.Count;
    SetupHeader.NumRunEntries := RunEntries.Count;
    SetupHeader.NumUninstallRunEntries := UninstallRunEntries.Count;
    SetupHeader.LicenseText := LicenseText;
    SetupHeader.InfoBeforeText := InfoBeforeText;
    SetupHeader.InfoAfterText := InfoAfterText;
    SetupHeader.CompiledCodeText := CompiledCodeText;

    W := TCompressedBlockWriter.Create(F, TLZMACompressor, InternalCompressLevel,
      InternalCompressProps);
    try
      SECompressedBlockWrite(W, SetupHeader, SizeOf(SetupHeader),
        SetupHeaderStrings, SetupHeaderAnsiStrings);

      for J := 0 to LanguageEntries.Count-1 do
        SECompressedBlockWrite(W, LanguageEntries[J]^, SizeOf(TSetupLanguageEntry),
          SetupLanguageEntryStrings, SetupLanguageEntryAnsiStrings);
      for J := 0 to CustomMessageEntries.Count-1 do
        SECompressedBlockWrite(W, CustomMessageEntries[J]^, SizeOf(TSetupCustomMessageEntry),
          SetupCustomMessageEntryStrings, SetupCustomMessageEntryAnsiStrings);
      for J := 0 to PermissionEntries.Count-1 do
        SECompressedBlockWrite(W, PermissionEntries[J]^, SizeOf(TSetupPermissionEntry),
          SetupPermissionEntryStrings, SetupPermissionEntryAnsiStrings);
      for J := 0 to TypeEntries.Count-1 do
        SECompressedBlockWrite(W, TypeEntries[J]^, SizeOf(TSetupTypeEntry),
          SetupTypeEntryStrings, SetupTypeEntryAnsiStrings);
      for J := 0 to ComponentEntries.Count-1 do
        SECompressedBlockWrite(W, ComponentEntries[J]^, SizeOf(TSetupComponentEntry),
          SetupComponentEntryStrings, SetupComponentEntryAnsiStrings);
      for J := 0 to TaskEntries.Count-1 do
        SECompressedBlockWrite(W, TaskEntries[J]^, SizeOf(TSetupTaskEntry),
          SetupTaskEntryStrings, SetupTaskEntryAnsiStrings);
      for J := 0 to DirEntries.Count-1 do
        SECompressedBlockWrite(W, DirEntries[J]^, SizeOf(TSetupDirEntry),
          SetupDirEntryStrings, SetupDirEntryAnsiStrings);
      for J := 0 to FileEntries.Count-1 do
        SECompressedBlockWrite(W, FileEntries[J]^, SizeOf(TSetupFileEntry),
          SetupFileEntryStrings, SetupFileEntryAnsiStrings);
      for J := 0 to IconEntries.Count-1 do
        SECompressedBlockWrite(W, IconEntries[J]^, SizeOf(TSetupIconEntry),
          SetupIconEntryStrings, SetupIconEntryAnsiStrings);
      for J := 0 to IniEntries.Count-1 do
        SECompressedBlockWrite(W, IniEntries[J]^, SizeOf(TSetupIniEntry),
          SetupIniEntryStrings, SetupIniEntryAnsiStrings);
      for J := 0 to RegistryEntries.Count-1 do
        SECompressedBlockWrite(W, RegistryEntries[J]^, SizeOf(TSetupRegistryEntry),
          SetupRegistryEntryStrings, SetupRegistryEntryAnsiStrings);
      for J := 0 to InstallDeleteEntries.Count-1 do
        SECompressedBlockWrite(W, InstallDeleteEntries[J]^, SizeOf(TSetupDeleteEntry),
          SetupDeleteEntryStrings, SetupDeleteEntryAnsiStrings);
      for J := 0 to UninstallDeleteEntries.Count-1 do
        SECompressedBlockWrite(W, UninstallDeleteEntries[J]^, SizeOf(TSetupDeleteEntry),
          SetupDeleteEntryStrings, SetupDeleteEntryAnsiStrings);
      for J := 0 to RunEntries.Count-1 do
        SECompressedBlockWrite(W, RunEntries[J]^, SizeOf(TSetupRunEntry),
          SetupRunEntryStrings, SetupRunEntryAnsiStrings);
      for J := 0 to UninstallRunEntries.Count-1 do
        SECompressedBlockWrite(W, UninstallRunEntries[J]^, SizeOf(TSetupRunEntry),
          SetupRunEntryStrings, SetupRunEntryAnsiStrings);

      W.Write(WizardImages.Count, SizeOf(Integer));
      for J := 0 to WizardImages.Count-1 do
        WriteStream(WizardImages[J], W);
      W.Write(WizardSmallImages.Count, SizeOf(Integer));
      for J := 0 to WizardSmallImages.Count-1 do
        WriteStream(WizardSmallImages[J], W);
      if SetupHeader.CompressMethod in [cmZip, cmBzip] then
        WriteStream(DecompressorDLL, W);

      W.Finish;
    finally
      W.Free;
    end;

    if not DiskSpanning then
      W := TCompressedBlockWriter.Create(F, TLZMACompressor, InternalCompressLevel,
        InternalCompressProps)
    else
      W := TCompressedBlockWriter.Create(F, nil, 0, nil);
      { ^ When disk spanning is enabled, the Setup Compiler requires that
        FileLocationEntries be a fixed size, so don't compress them }
    try
      for J := 0 to FileLocationEntries.Count-1 do
        W.Write(FileLocationEntries[J]^, SizeOf(TSetupFileLocationEntry));
      W.Finish;
    finally
      W.Free;
    end;

    Result := F.Position.Lo - Pos;
  end;

  function CreateSetup0File: Longint;
  var
    F: TFile;
  begin
    F := TFile.Create(OutputDir + OutputBaseFilename + '-0.bin',
      fdCreateAlways, faWrite, fsNone);
    try
      Result := WriteSetup0(F);
    finally
      F.Free;
    end;
  end;

  function RoundToNearestClusterSize(const L: Longint): Longint;
  begin
    Result := (L div DiskClusterSize) * DiskClusterSize;
    if L mod DiskClusterSize <> 0 then
      Inc(Result, DiskClusterSize);
  end;

  procedure CompressFiles(const FirstDestFile: String;
    const BytesToReserveOnFirstDisk: Longint);
  var
    CurrentTime: TSystemTime;

    procedure ApplyTouchDateTime(var FT: TFileTime);
    var
      ST: TSystemTime;
    begin
      if (TouchDateOption = tdNone) and (TouchTimeOption = ttNone) then
        Exit;  { nothing to do }
      if not FileTimeToSystemTime(FT, ST) then
        AbortCompile('ApplyTouch: FileTimeToSystemTime call failed');
      case TouchDateOption of
        tdCurrent: begin
            ST.wYear := CurrentTime.wYear;
            ST.wMonth := CurrentTime.wMonth;
            ST.wDay := CurrentTime.wDay;
          end;
        tdExplicit: begin
            ST.wYear := TouchDateYear;
            ST.wMonth := TouchDateMonth;
            ST.wDay := TouchDateDay;
          end;
      end;
      case TouchTimeOption of
        ttCurrent: begin
            ST.wHour := CurrentTime.wHour;
            ST.wMinute := CurrentTime.wMinute;
            ST.wSecond := CurrentTime.wSecond;
            ST.wMilliseconds := CurrentTime.wMilliseconds;
          end;
        ttExplicit: begin
            ST.wHour := TouchTimeHour;
            ST.wMinute := TouchTimeMinute;
            ST.wSecond := TouchTimeSecond;
            ST.wMilliseconds := 0;
          end;
      end;
      if not SystemTimeToFileTime(ST, FT) then
        AbortCompile('ApplyTouch: SystemTimeToFileTime call failed');
    end;

    function GetCompressorClass(const UseCompression: Boolean): TCustomCompressorClass;
    begin
      if not UseCompression then
        Result := TStoredCompressor
      else begin
        case SetupHeader.CompressMethod of
          cmStored: begin
              Result := TStoredCompressor;
            end;
          cmZip: begin
              InitZipDLL;
              Result := TZCompressor;
            end;
          cmBzip: begin
              InitBzipDLL;
              Result := TBZCompressor;
            end;
          cmLZMA: begin
              Result := TLZMACompressor;
            end;
          cmLZMA2: begin
              Result := TLZMA2Compressor;
            end;
        else
          AbortCompile('GetCompressorClass: Unknown CompressMethod');
          Result := nil;
        end;
      end;
    end;

    procedure FinalizeChunk(const CH: TCompressionHandler;
      const LastFileLocationEntry: Integer);
    var
      I: Integer;
      FL: PSetupFileLocationEntry;
    begin
      if CH.ChunkStarted then begin
        CH.EndChunk;
        { Set LastSlice and ChunkCompressedSize on all file location
          entries that are part of the chunk }
        for I := 0 to LastFileLocationEntry do begin
          FL := FileLocationEntries[I];
          if (FL.StartOffset = CH.ChunkStartOffset) and (FL.FirstSlice = CH.ChunkFirstSlice) then begin
            FL.LastSlice := CH.CurSlice;
            FL.ChunkCompressedSize := CH.ChunkBytesWritten;
          end;
        end;
      end;
    end;

  const
    StatusFilesStoringOrCompressingVersionStrings: array [Boolean] of String = (
     SCompilerStatusFilesStoringVersion,
     SCompilerStatusFilesCompressingVersion);
    StatusFilesStoringOrCompressingStrings: array [Boolean] of String = (
     SCompilerStatusFilesStoring,
     SCompilerStatusFilesCompressing);
  var
    CH: TCompressionHandler;
    ChunkCompressed: Boolean;
    I: Integer;
    FL: PSetupFileLocationEntry;
    FT: TFileTime;
    SourceFile: TFile;
    SignatureAddress, SignatureSize: Cardinal;
    HdrChecksum, ErrorCode: DWORD;
  begin
    if (SetupHeader.CompressMethod in [cmLZMA, cmLZMA2]) and
       (CompressProps.WorkerProcessFilename <> '') then
      AddStatus(Format('   Using separate process for LZMA compression (%s)',
        [PathExtractName(CompressProps.WorkerProcessFilename)]));

    if TimeStampsInUTC then
      GetSystemTime(CurrentTime)
    else
      GetLocalTime(CurrentTime);

    ChunkCompressed := False;  { avoid warning }
    CH := TCompressionHandler.Create(Self, FirstDestFile);
    try
      if DiskSpanning then begin
        if not CH.ReserveBytesOnSlice(BytesToReserveOnFirstDisk) then
          AbortCompile(SCompilerNotEnoughSpaceOnFirstDisk);
      end;

      CompressionStartTick := GetTickCount;
      CompressionInProgress := True;

      for I := 0 to FileLocationEntries.Count-1 do begin
        FL := FileLocationEntries[I];

        if FL.Sign <> fsNoSetting then begin
          var SignatureFound := False;
          if FL.Sign in [fsOnce, fsCheck] then begin
            { Check the file for a signature }
            SourceFile := TFile.Create(FileLocationEntryFilenames[I],
              fdOpenExisting, faRead, fsRead);
            try
              if ReadSignatureAndChecksumFields(SourceFile, DWORD(SignatureAddress),
                   DWORD(SignatureSize), HdrChecksum) or
                 ReadSignatureAndChecksumFields64(SourceFile, DWORD(SignatureAddress),
                   DWORD(SignatureSize), HdrChecksum) then
                SignatureFound := SignatureSize <> 0;
            finally
              SourceFile.Free;
            end;
          end;

          if (FL.Sign = fsYes) or ((FL.Sign = fsOnce) and not SignatureFound) then begin
            AddStatus(Format(SCompilerStatusSigningSourceFile, [FileLocationEntryFilenames[I]]));
            Sign(FileLocationEntryFilenames[I]);
            CallIdleProc;
          end else if FL.Sign = fsOnce then
            AddStatus(Format(SCompilerStatusSourceFileAlreadySigned, [FileLocationEntryFilenames[I]]))
          else if (FL.Sign = fsCheck) and not SignatureFound then
            AbortCompileFmt(SCompilerSourceFileNotSigned, [FileLocationEntryFilenames[I]]);
        end;

        if foVersionInfoValid in FL.Flags then
          AddStatus(Format(StatusFilesStoringOrCompressingVersionStrings[foChunkCompressed in FL.Flags],
            [FileLocationEntryFilenames[I],
             LongRec(FL.FileVersionMS).Hi, LongRec(FL.FileVersionMS).Lo,
             LongRec(FL.FileVersionLS).Hi, LongRec(FL.FileVersionLS).Lo]))
        else
          AddStatus(Format(StatusFilesStoringOrCompressingStrings[foChunkCompressed in FL.Flags],
            [FileLocationEntryFilenames[I]]));
        CallIdleProc;
        
        SourceFile := TFile.Create(FileLocationEntryFilenames[I],
          fdOpenExisting, faRead, fsRead);
        try
          if CH.ChunkStarted then begin
            { End the current chunk if one of the following conditions is true:
              - we're not using solid compression
              - the "solidbreak" flag was specified on this file
              - the compression or encryption status of this file is
                different from the previous file(s) in the chunk }
            if not UseSolidCompression or
               (foSolidBreak in FL.Flags) or
               (ChunkCompressed <> (foChunkCompressed in FL.Flags)) or
               (CH.ChunkEncrypted <> (foChunkEncrypted in FL.Flags)) then
              FinalizeChunk(CH, I-1);
          end;
          { Start a new chunk if needed }
          if not CH.ChunkStarted then begin
            ChunkCompressed := (foChunkCompressed in FL.Flags);
            CH.NewChunk(GetCompressorClass(ChunkCompressed), CompressLevel,
              CompressProps, foChunkEncrypted in FL.Flags, CryptKey);
          end;

          FL.FirstSlice := CH.ChunkFirstSlice;
          FL.StartOffset := CH.ChunkStartOffset;
          FL.ChunkSuboffset := CH.ChunkBytesRead;
          FL.OriginalSize := SourceFile.Size;

          if not GetFileTime(SourceFile.Handle, nil, nil, @FT) then begin
            ErrorCode := GetLastError;
            AbortCompileFmt(SCompilerFunctionFailedWithCode,
              ['CompressFiles: GetFileTime', ErrorCode, Win32ErrorString(ErrorCode)]);
          end;
          if TimeStampsInUTC then begin
            FL.SourceTimeStamp := FT;
            Include(FL.Flags, foTimeStampInUTC);
          end
          else
            FileTimeToLocalFileTime(FT, FL.SourceTimeStamp);
          if foApplyTouchDateTime in FL.Flags then
            ApplyTouchDateTime(FL.SourceTimeStamp);
          if TimeStampRounding > 0 then
            Dec64(Integer64(FL.SourceTimeStamp), Mod64(Integer64(FL.SourceTimeStamp), TimeStampRounding * 10000000));

          if ChunkCompressed and IsX86OrX64Executable(SourceFile) then
            Include(FL.Flags, foCallInstructionOptimized);

          CH.CompressFile(SourceFile, FL.OriginalSize,
            foCallInstructionOptimized in FL.Flags, FL.SHA256Sum);
        finally
          SourceFile.Free;
        end;
      end;
      { Finalize the last chunk }
      FinalizeChunk(CH, FileLocationEntries.Count-1);

      CH.Finish;
    finally
      CompressionInProgress := False;
      CH.Free;
    end;

    { Ensure progress bar is full, in case a file shrunk in size }
    BytesCompressedSoFar := TotalBytesToCompress;
    CallIdleProc;
  end;

  procedure CopyFileOrAbort(const SourceFile, DestFile: String);
  var
    ErrorCode: DWORD;
  begin
    if not CopyFile(PChar(SourceFile), PChar(DestFile), False) then begin
      ErrorCode := GetLastError;
      AbortCompileFmt(SCompilerCopyError3, [SourceFile, DestFile,
        ErrorCode, Win32ErrorString(ErrorCode)]);
    end;
  end;

  function InternalSignSetupE32(const Filename: String;
    var UnsignedFile: TMemoryFile; const UnsignedFileSize: Cardinal;
    const MismatchMessage: String): Boolean;
  var
    SignedFile, TestFile, OldFile: TMemoryFile;
    SignedFileSize: Cardinal;
    SignatureAddress, SignatureSize: Cardinal;
    HdrChecksum: DWORD;
  begin
    SignedFile := TMemoryFile.Create(Filename);
    try
      SignedFileSize := SignedFile.CappedSize;

      { Check the file for a signature }
      if not ReadSignatureAndChecksumFields(SignedFile, DWORD(SignatureAddress),
         DWORD(SignatureSize), HdrChecksum) then
        AbortCompile('ReadSignatureAndChecksumFields failed');
      if SignatureAddress = 0 then begin
        { No signature found. Return False to inform the caller that the file
          needs to be signed, but first make sure it isn't somehow corrupted. }
        if (SignedFileSize = UnsignedFileSize) and
           CompareMem(UnsignedFile.Memory, SignedFile.Memory, UnsignedFileSize) then begin
          Result := False;
          Exit;
        end;
        AbortCompileFmt(MismatchMessage, [Filename]);
      end;
      if (SignedFileSize <= UnsignedFileSize) or
         (SignatureAddress <> UnsignedFileSize) or
         (SignatureSize <> SignedFileSize - UnsignedFileSize) or
         (SignatureSize >= Cardinal($100000)) then
        AbortCompile(SCompilerSignatureInvalid);

      { Sanity check: Remove the signature (in memory) and verify that
        the signed file is identical byte-for-byte to the original }
      TestFile := TMemoryFile.CreateFromMemory(SignedFile.Memory^, SignedFileSize);
      try
        { Carry checksum over from UnsignedFile to TestFile. We used to just
          zero it in TestFile, but that didn't work if the user modified
          Setup.e32 with a res-editing tool that sets a non-zero checksum. }  
        if not ReadSignatureAndChecksumFields(UnsignedFile, DWORD(SignatureAddress),
           DWORD(SignatureSize), HdrChecksum) then
          AbortCompile('ReadSignatureAndChecksumFields failed (2)');
        if not UpdateSignatureAndChecksumFields(TestFile, 0, 0, HdrChecksum) then
          AbortCompile('UpdateSignatureAndChecksumFields failed');
        if not CompareMem(UnsignedFile.Memory, TestFile.Memory, UnsignedFileSize) then
          AbortCompileFmt(MismatchMessage, [Filename]);
      finally
        TestFile.Free;
      end;
    except
      SignedFile.Free;
      raise;
    end;

    { Replace UnsignedFile with the signed file }
    OldFile := UnsignedFile;
    UnsignedFile := SignedFile;
    OldFile.Free;
    Result := True;
  end;

  procedure SignSetupE32(var UnsignedFile: TMemoryFile);
  var
    UnsignedFileSize: Cardinal;
    ModeID: Longint;
    Filename, TempFilename: String;
    F: TFile;
    LastError: DWORD;
  begin
    UnsignedFileSize := UnsignedFile.CappedSize;

    UnsignedFile.Seek(SetupExeModeOffset);
    ModeID := SetupExeModeUninstaller;
    UnsignedFile.WriteBuffer(ModeID, SizeOf(ModeID));

    if SignTools.Count > 0 then begin
      Filename := SignedUninstallerDir + 'uninst.e32.tmp';

      F := TFile.Create(Filename, fdCreateAlways, faWrite, fsNone);
      try
        F.WriteBuffer(UnsignedFile.Memory^, UnsignedFileSize);
      finally
        F.Free;
      end;

      try
        Sign(Filename);
        if not InternalSignSetupE32(Filename, UnsignedFile, UnsignedFileSize,
           SCompilerSignedFileContentsMismatch) then
          AbortCompile(SCompilerSignToolSucceededButNoSignature);
      finally
        DeleteFile(Filename);
      end;
    end else begin
      Filename := SignedUninstallerDir + Format('uninst-%s-%s.e32', [SetupVersion,
        Copy(SHA256DigestToString(SHA256Buf(UnsignedFile.Memory^, UnsignedFileSize)), 1, 10)]);

      if not NewFileExists(Filename) then begin
        { Create new signed uninstaller file }
        AddStatus(Format(SCompilerStatusSignedUninstallerNew, [Filename]));
        TempFilename := Filename + '.tmp';
        F := TFile.Create(TempFilename, fdCreateAlways, faWrite, fsNone);
        try
          F.WriteBuffer(UnsignedFile.Memory^, UnsignedFileSize);
        finally
          F.Free;
        end;
        if not MoveFile(PChar(TempFilename), PChar(Filename)) then begin
          LastError := GetLastError;
          DeleteFile(TempFilename);
          TFile.RaiseError(LastError);
        end;
      end
      else begin
        { Use existing signed uninstaller file }
        AddStatus(Format(SCompilerStatusSignedUninstallerExisting, [Filename]));
      end;

      if not InternalSignSetupE32(Filename, UnsignedFile, UnsignedFileSize,
         SCompilerSignedFileContentsMismatchRetry) then
        AbortCompileFmt(SCompilerSignatureNeeded, [Filename]);
    end;
  end;

  procedure PrepareSetupE32(var M: TMemoryFile);
  var
    TempFilename, E32Filename, ConvertFilename: String;
    ConvertFile: TFile;
  begin
    TempFilename := '';
    try
      E32Filename := CompilerDir + 'SETUP.E32';
      { make a copy and update icons, version info and if needed manifest }
      ConvertFilename := OutputDir + OutputBaseFilename + '.e32.tmp';
      CopyFileOrAbort(E32Filename, ConvertFilename);
      SetFileAttributes(PChar(ConvertFilename), FILE_ATTRIBUTE_ARCHIVE);
      TempFilename := ConvertFilename;
      if SetupIconFilename <> '' then begin
        AddStatus(Format(SCompilerStatusUpdatingIcons, ['SETUP.E32']));
        LineNumber := SetupDirectiveLines[ssSetupIconFile];
        { This also deletes the UninstallImage resource. Removing it makes UninstallProgressForm use the custom icon instead. }
        UpdateIcons(ConvertFileName, PrependSourceDirName(SetupIconFilename), True);
        LineNumber := 0;
      end;
      AddStatus(Format(SCompilerStatusUpdatingVersionInfo, ['SETUP.E32']));
      ConvertFile := TFile.Create(ConvertFilename, fdOpenExisting, faReadWrite, fsNone);
      try
        UpdateVersionInfo(ConvertFile, TFileVersionNumbers(nil^), VersionInfoProductVersion, VersionInfoCompany,
          '', '', VersionInfoCopyright, VersionInfoProductName, VersionInfoProductTextVersion, VersionInfoOriginalFileName,
          False);
      finally
        ConvertFile.Free;
      end;
      M := TMemoryFile.Create(ConvertFilename);
      UpdateSetupPEHeaderFields(M, TerminalServicesAware, DEPCompatible, ASLRCompatible);
      if shSignedUninstaller in SetupHeader.Options then
        SignSetupE32(M);
    finally
      if TempFilename <> '' then
        DeleteFile(TempFilename);
    end;
  end;

  procedure CompressSetupE32(const M: TMemoryFile; const DestF: TFile;
    var UncompressedSize: LongWord; var CRC: Longint);
  { Note: This modifies the contents of M. }
  var
    Writer: TCompressedBlockWriter;
  begin
    AddStatus(SCompilerStatusCompressingSetupExe);
    UncompressedSize := M.CappedSize;
    CRC := GetCRC32(M.Memory^, UncompressedSize);
    TransformCallInstructions(M.Memory^, UncompressedSize, True, 0);
    Writer := TCompressedBlockWriter.Create(DestF, TLZMACompressor, InternalCompressLevel,
      InternalCompressProps);
    try
      Writer.Write(M.Memory^, UncompressedSize);
      Writer.Finish;
    finally
      Writer.Free;
    end;
  end;

  procedure AddDefaultSetupType(Name: String; Options: TSetupTypeOptions; Typ: TSetupTypeType);
  var
    NewTypeEntry: PSetupTypeEntry;
  begin
    NewTypeEntry := AllocMem(SizeOf(TSetupTypeEntry));
    NewTypeEntry.Name := Name;
    NewTypeEntry.Description := ''; //set at runtime
    NewTypeEntry.Check := '';
    NewTypeEntry.MinVersion := SetupHeader.MinVersion;
    NewTypeEntry.OnlyBelowVersion := SetupHeader.OnlyBelowVersion;
    NewTypeEntry.Options := Options;
    NewTypeEntry.Typ := Typ;
    TypeEntries.Add(NewTypeEntry);
  end;

  procedure MkDirs(Dir: string);
  begin
    Dir := RemoveBackslashUnlessRoot(Dir);
    if (PathExtractPath(Dir) = Dir) or DirExists(Dir) then
      Exit;
    MkDirs(PathExtractPath(Dir));
    MkDir(Dir);
  end;

  procedure CreateManifestFile;

    function FileTimeToString(const FileTime: TFileTime; const UTC: Boolean): String;
    var
      ST: TSystemTime;
    begin
      if FileTimeToSystemTime(FileTime, ST) then
        Result := Format('%.4u-%.2u-%.2u %.2u:%.2u:%.2u.%.3u',
          [ST.wYear, ST.wMonth, ST.wDay, ST.wHour, ST.wMinute, ST.wSecond,
           ST.wMilliseconds])
      else
        Result := '(invalid)';
      if UTC then
        Result := Result + ' UTC';
    end;

    function SliceToString(const ASlice: Integer): String;
    begin
      Result := IntToStr(ASlice div SlicesPerDisk + 1);
      if SlicesPerDisk <> 1 then
        Result := Result + Chr(Ord('a') + ASlice mod SlicesPerDisk);
    end;

  const
    EncryptedStrings: array [Boolean] of String = ('no', 'yes');
  var
    F: TTextFileWriter;
    FL: PSetupFileLocationEntry;
    S: String;
    I: Integer;
  begin
    F := TTextFileWriter.Create(PrependDirName(OutputManifestFile, OutputDir),
      fdCreateAlways, faWrite, fsRead);
    try
      S := 'Index' + #9 + 'SourceFilename' + #9 + 'TimeStamp' + #9 +
        'Version' + #9 + 'SHA256Sum' + #9 + 'OriginalSize' + #9 +
        'FirstSlice' + #9 + 'LastSlice' + #9 + 'StartOffset' + #9 +
        'ChunkSuboffset' + #9 + 'ChunkCompressedSize' + #9 + 'Encrypted';
      F.WriteLine(S);

      for I := 0 to FileLocationEntries.Count-1 do begin
        FL := FileLocationEntries[I];
        S := IntToStr(I) + #9 + FileLocationEntryFilenames[I] + #9 +
          FileTimeToString(FL.SourceTimeStamp, foTimeStampInUTC in FL.Flags) + #9;
        if foVersionInfoValid in FL.Flags then
          S := S + Format('%u.%u.%u.%u', [FL.FileVersionMS shr 16,
            FL.FileVersionMS and $FFFF, FL.FileVersionLS shr 16,
            FL.FileVersionLS and $FFFF]);
        S := S + #9 + SHA256DigestToString(FL.SHA256Sum) + #9 +
          Integer64ToStr(FL.OriginalSize) + #9 +
          SliceToString(FL.FirstSlice) + #9 +
          SliceToString(FL.LastSlice) + #9 +
          IntToStr(FL.StartOffset) + #9 +
          Integer64ToStr(FL.ChunkSuboffset) + #9 +
          Integer64ToStr(FL.ChunkCompressedSize) + #9 +
          EncryptedStrings[foChunkEncrypted in FL.Flags];
        F.WriteLine(S);
      end;
    finally
      F.Free;
    end;
  end;

  procedure CallPreprocessorCleanupProc;
  var
    ResultCode: Integer;
  begin
    if Assigned(PreprocCleanupProc) then begin
      ResultCode := PreprocCleanupProc(PreprocCleanupProcData);
      if ResultCode <> 0 then
        AddStatusFmt(SCompilerStatusWarning +
          'Preprocessor cleanup function failed with code %d.', [ResultCode], True);
    end;
  end;

  procedure UpdateTimeStamp(H: THandle);
  var
    FT: TFileTime;
  begin
    GetSystemTimeAsFileTime(FT);
    SetFileTime(H, nil, nil, @FT);
  end;

  procedure GenerateEncryptionKDFSalt(out Salt: TSetupKDFSalt);
  begin
    GenerateRandomBytes(Salt, SizeOf(Salt));
  end;

  procedure GenerateEncryptionBaseNonce(out Nonce: TSetupEncryptionNonce);
  begin
    GenerateRandomBytes(Nonce, SizeOf(Nonce));
  end;

  { This function assumes EncryptionKey is based on the password }
  procedure GeneratePasswordTest(const EncryptionKey: TSetupEncryptionKey;
    const EncryptionBaseNonce: TSetupEncryptionNonce; out PasswordTest: Integer);
  begin
    { Create a special nonce that cannot collide with encrypted-file nonces }
    var Nonce := EncryptionBaseNonce;
    Nonce.RandomXorFirstSlice := Nonce.RandomXorFirstSlice xor -1;

    { Encrypt a value of 0 so Setup can do same and compare the results to test the password }
    var Context: TChaCha20Context;
    XChaCha20Init(Context, EncryptionKey[0], Length(EncryptionKey), Nonce, SizeOf(Nonce), 0);
    PasswordTest := 0;
    XChaCha20Crypt(Context, PasswordTest, PasswordTest, SizeOf(PasswordTest));
  end;

const
  BadFilePathChars = '/*?"<>|';
  BadFileNameChars = BadFilePathChars + ':';
var
  SetupE32: TMemoryFile;
  I: Integer;
  AppNameHasConsts, AppVersionHasConsts, AppPublisherHasConsts,
    AppCopyrightHasConsts, AppIdHasConsts, Uninstallable: Boolean;
  PrivilegesRequiredValue: String;
begin
  { Sanity check: A single TSetupCompiler instance cannot be used to do
    multiple compiles. A separate instance must be used for each compile,
    otherwise some settings (e.g. DefaultLangData, VersionInfo*) would be
    carried over from one compile to another. }
  if CompileWasAlreadyCalled then
    AbortCompile('Compile was already called');
  CompileWasAlreadyCalled := True;

  CompilerDir := AddBackslash(PathExpand(CompilerDir));
  InitPreprocessor;
  InitLZMADLL;

  WizardImages := nil;
  WizardSmallImages := nil;
  SetupE32 := nil;
  DecompressorDLL := nil;

  try
    Finalize(SetupHeader);
    FillChar(SetupHeader, SizeOf(SetupHeader), 0);
    InitDebugInfo;
    PreprocIncludedFilenames.Clear;

    { Initialize defaults }
    OriginalSourceDir := AddBackslash(PathExpand(SourceDir));
    if not FixedOutput then
      Output := True;
    if not FixedOutputDir then
      OutputDir := 'Output';
    if not FixedOutputBaseFilename then
      OutputBaseFilename := 'mysetup';
    InternalCompressLevel := clLZMANormal;
    InternalCompressProps := TLZMACompressorProps.Create;
    CompressMethod := cmLZMA2;
    CompressLevel := clLZMAMax;
    CompressProps := TLZMACompressorProps.Create;
    UseSetupLdr := True;
    TerminalServicesAware := True;
    DEPCompatible := True;
    ASLRCompatible := True;
    DiskSliceSize := MaxDiskSliceSize;
    DiskClusterSize := 512;
    SlicesPerDisk := 1;
    ReserveBytes := 0;
    TimeStampRounding := 2;
    SetupHeader.MinVersion.WinVersion := 0;
    SetupHeader.MinVersion.NTVersion := $06010000;
    SetupHeader.MinVersion.NTServicePack := $100;
    SetupHeader.Options := [shDisableStartupPrompt, shCreateAppDir,
      shUsePreviousAppDir, shUsePreviousGroup,
      shUsePreviousSetupType, shAlwaysShowComponentsList, shFlatComponentsList,
      shShowComponentSizes, shUsePreviousTasks, shUpdateUninstallLogAppName,
      shAllowUNCPath, shUsePreviousUserInfo, shRestartIfNeededByRun,
      shAllowCancelDuringInstall, shWizardImageStretch, shAppendDefaultDirName,
      shAppendDefaultGroupName, shUsePreviousLanguage, shCloseApplications,
      shRestartApplications, shAllowNetworkDrive, shDisableWelcomePage,
      shUsePreviousPrivileges];
    SetupHeader.PrivilegesRequired := prAdmin;
    SetupHeader.UninstallFilesDir := '{app}';
    SetupHeader.DefaultUserInfoName := '{sysuserinfoname}';
    SetupHeader.DefaultUserInfoOrg := '{sysuserinfoorg}';
    SetupHeader.DisableDirPage := dpAuto;
    SetupHeader.DisableProgramGroupPage := dpAuto;
    SetupHeader.CreateUninstallRegKey := 'yes';
    SetupHeader.Uninstallable := 'yes';
    SetupHeader.ChangesEnvironment := 'no';
    SetupHeader.ChangesAssociations := 'no';
    DefaultDialogFontName := 'Tahoma';
    SignToolRetryCount := 2;
    SignToolRetryDelay := 500;
    SetupHeader.CloseApplicationsFilter := '*.exe,*.dll,*.chm';
    SetupHeader.WizardImageAlphaFormat := afIgnored;
    MissingRunOnceIdsWarning := True;
    MissingMessagesWarning := True;
    NotRecognizedMessagesWarning := True;
    UsedUserAreasWarning := True;
    SetupHeader.WizardStyle := wsClassic;
    SetupHeader.EncryptionKDFIterations := 200000;

    { Read [Setup] section }
    EnumIniSection(EnumSetupProc, 'Setup', 0, True, True, '', False, False);
    CallIdleProc;

    { Verify settings set in [Setup] section }
    if SetupDirectiveLines[ssAppName] = 0 then
      AbortCompileFmt(SCompilerEntryMissing2, ['Setup', 'AppName']);
    if (SetupHeader.AppVerName = '') and (SetupHeader.AppVersion = '') then
      AbortCompile(SCompilerAppVersionOrAppVerNameRequired);
    LineNumber := SetupDirectiveLines[ssAppName];
    AppNameHasConsts := CheckConst(SetupHeader.AppName, SetupHeader.MinVersion, []);
    if AppNameHasConsts then begin
      Include(SetupHeader.Options, shAppNameHasConsts);
      if not(shDisableStartupPrompt in SetupHeader.Options) then begin
        { AppName has constants so DisableStartupPrompt must be used }
        LineNumber := SetupDirectiveLines[ssDisableStartupPrompt];
        AbortCompile(SCompilerMustUseDisableStartupPrompt);
      end;
    end;
    if SetupHeader.AppId = '' then
      SetupHeader.AppId := SetupHeader.AppName
    else
      LineNumber := SetupDirectiveLines[ssAppId];
    AppIdHasConsts := CheckConst(SetupHeader.AppId, SetupHeader.MinVersion, []);
    if AppIdHasConsts and (shUsePreviousLanguage in SetupHeader.Options) then begin
      { AppId has constants so UsePreviousLanguage must not be used }
      LineNumber := SetupDirectiveLines[ssUsePreviousLanguage];
      AbortCompile(SCompilerMustNotUsePreviousLanguage);
    end;
    if AppIdHasConsts and (proDialog in SetupHeader.PrivilegesRequiredOverridesAllowed) and (shUsePreviousPrivileges in SetupHeader.Options) then begin
      { AppId has constants so UsePreviousPrivileges must not be used }
      LineNumber := SetupDirectiveLines[ssUsePreviousPrivileges];
      AbortCompile(SCompilerMustNotUsePreviousPrivileges);
    end;
    LineNumber := SetupDirectiveLines[ssAppVerName];
    CheckConst(SetupHeader.AppVerName, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssAppComments];
    CheckConst(SetupHeader.AppComments, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssAppContact];
    CheckConst(SetupHeader.AppContact, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssAppCopyright];
    AppCopyrightHasConsts := CheckConst(SetupHeader.AppCopyright, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssAppModifyPath];
    CheckConst(SetupHeader.AppModifyPath, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssAppPublisher];
    AppPublisherHasConsts := CheckConst(SetupHeader.AppPublisher, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssAppPublisherURL];
    CheckConst(SetupHeader.AppPublisherURL, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssAppReadmeFile];
    CheckConst(SetupHeader.AppReadmeFile, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssAppSupportPhone];
    CheckConst(SetupHeader.AppSupportPhone, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssAppSupportURL];
    CheckConst(SetupHeader.AppSupportURL, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssAppUpdatesURL];
    CheckConst(SetupHeader.AppUpdatesURL, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssAppVersion];
    AppVersionHasConsts := CheckConst(SetupHeader.AppVersion, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssAppMutex];
    CheckConst(SetupHeader.AppMutex, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssSetupMutex];
    CheckConst(SetupHeader.SetupMutex, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssDefaultDirName];
    CheckConst(SetupHeader.DefaultDirName, SetupHeader.MinVersion, []);
    if SetupHeader.DefaultDirName = '' then begin
      if shCreateAppDir in SetupHeader.Options then
        AbortCompileFmt(SCompilerEntryMissing2, ['Setup', 'DefaultDirName'])
      else
        SetupHeader.DefaultDirName := '?ERROR?';
    end;
    LineNumber := SetupDirectiveLines[ssDefaultGroupName];
    CheckConst(SetupHeader.DefaultGroupName, SetupHeader.MinVersion, []);
    if SetupHeader.DefaultGroupName = '' then
      SetupHeader.DefaultGroupName := '(Default)';
    LineNumber := SetupDirectiveLines[ssUninstallDisplayName];
    CheckConst(SetupHeader.UninstallDisplayName, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssUninstallDisplayIcon];
    CheckConst(SetupHeader.UninstallDisplayIcon, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssUninstallFilesDir];
    CheckConst(SetupHeader.UninstallFilesDir, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssDefaultUserInfoName];
    CheckConst(SetupHeader.DefaultUserInfoName, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssDefaultUserInfoOrg];
    CheckConst(SetupHeader.DefaultUserInfoOrg, SetupHeader.MinVersion, []);
    LineNumber := SetupDirectiveLines[ssDefaultUserInfoSerial];
    CheckConst(SetupHeader.DefaultUserInfoSerial, SetupHeader.MinVersion, []);
    if not DiskSpanning then begin
      DiskSliceSize := MaxDiskSliceSize;
      DiskClusterSize := 1;
      SlicesPerDisk := 1;
      ReserveBytes := 0;
    end;
    SetupHeader.SlicesPerDisk := SlicesPerDisk;
    if SetupDirectiveLines[ssVersionInfoDescription] = 0 then begin
      { Use AppName as VersionInfoDescription if possible. If not possible,
        warn about this since AppName is a required directive }
      if not AppNameHasConsts then
        VersionInfoDescription := UnescapeBraces(SetupHeader.AppName) + ' Setup'
      else
        WarningsList.Add(Format(SCompilerDirectiveNotUsingDefault,
          ['VersionInfoDescription', 'AppName']));
    end;
    if SetupDirectiveLines[ssVersionInfoCompany] = 0 then begin
      { Use AppPublisher as VersionInfoCompany if possible, otherwise warn }
      if not AppPublisherHasConsts then
        VersionInfoCompany := UnescapeBraces(SetupHeader.AppPublisher)
      else
        WarningsList.Add(Format(SCompilerDirectiveNotUsingDefault,
          ['VersionInfoCompany', 'AppPublisher']));
    end;
    if SetupDirectiveLines[ssVersionInfoCopyright] = 0 then begin
      { Use AppCopyright as VersionInfoCopyright if possible, otherwise warn }
      if not AppCopyrightHasConsts then
        VersionInfoCopyright := UnescapeBraces(SetupHeader.AppCopyright)
      else
        WarningsList.Add(Format(SCompilerDirectiveNotUsingDefault,
          ['VersionInfoCopyright', 'AppCopyright']));
    end;
    if SetupDirectiveLines[ssVersionInfoTextVersion] = 0 then
      VersionInfoTextVersion := VersionInfoVersionOriginalValue;
    if SetupDirectiveLines[ssVersionInfoProductName] = 0 then begin
      { Use AppName as VersionInfoProductName if possible, otherwise warn }
      if not AppNameHasConsts then
        VersionInfoProductName := UnescapeBraces(SetupHeader.AppName)
      else
        WarningsList.Add(Format(SCompilerDirectiveNotUsingDefault,
          ['VersionInfoProductName', 'AppName']));
    end;
    if VersionInfoProductVersionOriginalValue = '' then
      VersionInfoProductVersion := VersionInfoVersion;
    if SetupDirectiveLines[ssVersionInfoProductTextVersion] = 0 then begin
      { Note: This depends on the initialization of VersionInfoTextVersion above }
      if VersionInfoProductVersionOriginalValue = '' then begin
        VersionInfoProductTextVersion := VersionInfoTextVersion;
        if SetupHeader.AppVersion <> '' then begin
          if not AppVersionHasConsts then
            VersionInfoProductTextVersion := UnescapeBraces(SetupHeader.AppVersion)
          else
            WarningsList.Add(Format(SCompilerDirectiveNotUsingPreferredDefault,
              ['VersionInfoProductTextVersion', 'VersionInfoTextVersion', 'AppVersion']));
        end;
      end
      else
        VersionInfoProductTextVersion := VersionInfoProductVersionOriginalValue;
    end;
    if (shEncryptionUsed in SetupHeader.Options) and (Password = '') then begin
      LineNumber := SetupDirectiveLines[ssEncryption];
      AbortCompileFmt(SCompilerEntryMissing2, ['Setup', 'Password']);
    end;
    if (SetupDirectiveLines[ssSignedUninstaller] = 0) and (SignTools.Count > 0) then
      Include(SetupHeader.Options, shSignedUninstaller);
    if not UseSetupLdr and
       ((SignTools.Count > 0) or (shSignedUninstaller in SetupHeader.Options)) then
      AbortCompile(SCompilerNoSetupLdrSignError);
    LineNumber := SetupDirectiveLines[ssCreateUninstallRegKey];
    CheckCheckOrInstall('CreateUninstallRegKey', SetupHeader.CreateUninstallRegKey, cikDirectiveCheck);
    LineNumber := SetupDirectiveLines[ssUninstallable];
    CheckCheckOrInstall('Uninstallable', SetupHeader.Uninstallable, cikDirectiveCheck);
    LineNumber := SetupDirectiveLines[ssChangesEnvironment];
    CheckCheckOrInstall('ChangesEnvironment', SetupHeader.ChangesEnvironment, cikDirectiveCheck);
    LineNumber := SetupDirectiveLines[ssChangesAssociations];
    CheckCheckOrInstall('ChangesAssociations', SetupHeader.ChangesAssociations, cikDirectiveCheck);
    if Output and (OutputDir = '') then begin
      LineNumber := SetupDirectiveLines[ssOutput];
      AbortCompileOnLineFmt(SCompilerEntryInvalid2, ['Setup', 'OutputDir']);
    end;
    if (Output and (OutputBaseFileName = '')) or (PathLastDelimiter(BadFileNameChars + '\', OutputBaseFileName) <> 0) then begin
      LineNumber := SetupDirectiveLines[ssOutputBaseFileName];
      AbortCompileOnLineFmt(SCompilerEntryInvalid2, ['Setup', 'OutputBaseFileName']);
    end else if OutputBaseFileName = 'setup' then { Warn even if Output is False }
      WarningsList.Add(SCompilerOutputBaseFileNameSetup);
    if (SetupDirectiveLines[ssOutputManifestFile] <> 0) and
       ((Output and (OutputManifestFile = '')) or (PathLastDelimiter(BadFilePathChars, OutputManifestFile) <> 0)) then begin
      LineNumber := SetupDirectiveLines[ssOutputManifestFile];
      AbortCompileOnLineFmt(SCompilerEntryInvalid2, ['Setup', 'OutputManifestFile']);
    end;
    if shAlwaysUsePersonalGroup in SetupHeader.Options then
      UsedUserAreas.Add('AlwaysUsePersonalGroup');
    if SetupDirectiveLines[ssWizardSizePercent] = 0 then begin
      if SetupHeader.WizardStyle = wsModern then
        SetupHeader.WizardSizePercentX := 120
      else
        SetupHeader.WizardSizePercentX := 100;
      SetupHeader.WizardSizePercentY := SetupHeader.WizardSizePercentX;
    end;
    if (SetupDirectiveLines[ssWizardResizable] = 0) and (SetupHeader.WizardStyle = wsModern) then
      Include(SetupHeader.Options, shWizardResizable);
    if (SetupHeader.MinVersion.NTVersion shr 16 = $0601) and (SetupHeader.MinVersion.NTServicePack < $100) then
      WarningsList.Add(Format(SCompilerMinVersionRecommendation, ['6.1', '6.1sp1']));

    LineNumber := 0;

    SourceDir := AddBackslash(PathExpand(SourceDir));
    if not FixedOutputDir then
      OutputDir := PrependSourceDirName(OutputDir);
    OutputDir := RemoveBackslashUnlessRoot(PathExpand(OutputDir));
    LineNumber := SetupDirectiveLines[ssOutputDir];
    if not DirExists(OutputDir) then begin
      AddStatus(Format(SCompilerStatusCreatingOutputDir, [OutputDir]));
      MkDirs(OutputDir);
    end;
    LineNumber := 0;
    OutputDir := AddBackslash(OutputDir);

    if SignedUninstallerDir = '' then
      SignedUninstallerDir := OutputDir
    else begin
      SignedUninstallerDir := RemoveBackslashUnlessRoot(PathExpand(PrependSourceDirName(SignedUninstallerDir)));
      if not DirExists(SignedUninstallerDir) then begin
        AddStatus(Format(SCompilerStatusCreatingSignedUninstallerDir, [SignedUninstallerDir]));
        MkDirs(SignedUninstallerDir);
      end;
      SignedUninstallerDir := AddBackslash(SignedUninstallerDir);
    end;

    if Password <> '' then begin
      GenerateEncryptionKDFSalt(SetupHeader.EncryptionKDFSalt);
      GenerateEncryptionKey(Password,  SetupHeader.EncryptionKDFSalt, SetupHeader.EncryptionKDFIterations, CryptKey);
      GenerateEncryptionBaseNonce(SetupHeader.EncryptionBaseNonce);
      GeneratePasswordTest(CryptKey, SetupHeader.EncryptionBaseNonce, SetupHeader.PasswordTest);
      Include(SetupHeader.Options, shPassword);
    end;

    { Read text files }
    if LicenseFile <> '' then begin
      LineNumber := SetupDirectiveLines[ssLicenseFile];
      AddStatus(Format(SCompilerStatusReadingFile, ['LicenseFile']));
      ReadTextFile(PrependSourceDirName(LicenseFile), -1, LicenseText);
    end;
    if InfoBeforeFile <> '' then begin
      LineNumber := SetupDirectiveLines[ssInfoBeforeFile];
      AddStatus(Format(SCompilerStatusReadingFile, ['InfoBeforeFile']));
      ReadTextFile(PrependSourceDirName(InfoBeforeFile), -1, InfoBeforeText);
    end;
    if InfoAfterFile <> '' then begin
      LineNumber := SetupDirectiveLines[ssInfoAfterFile];
      AddStatus(Format(SCompilerStatusReadingFile, ['InfoAfterFile']));
      ReadTextFile(PrependSourceDirName(InfoAfterFile), -1, InfoAfterText);
    end;
    LineNumber := 0;
    CallIdleProc;

    { Read wizard image }
    LineNumber := SetupDirectiveLines[ssWizardImageFile];
    AddStatus(Format(SCompilerStatusReadingFile, ['WizardImageFile']));
    if WizardImageFile <> '' then begin
      if SameText(WizardImageFile, 'compiler:WizModernImage.bmp') then begin
        WarningsList.Add(Format(SCompilerWizImageRenamed, [WizardImageFile, 'compiler:WizClassicImage.bmp']));
        WizardImageFile := 'compiler:WizClassicImage.bmp';
      end;
      WizardImages := CreateMemoryStreamsFromFiles('WizardImageFile', WizardImageFile)
    end else
      WizardImages := CreateMemoryStreamsFromResources(['WizardImage'], ['150']);
    LineNumber := SetupDirectiveLines[ssWizardSmallImageFile];
    AddStatus(Format(SCompilerStatusReadingFile, ['WizardSmallImageFile']));
    if WizardSmallImageFile <> '' then begin
      if SameText(WizardSmallImageFile, 'compiler:WizModernSmallImage.bmp') then begin
        WarningsList.Add(Format(SCompilerWizImageRenamed, [WizardSmallImageFile, 'compiler:WizClassicSmallImage.bmp']));
        WizardSmallImageFile := 'compiler:WizClassicSmallImage.bmp';
      end;
      WizardSmallImages := CreateMemoryStreamsFromFiles('WizardSmallImage', WizardSmallImageFile)
    end else
      WizardSmallImages := CreateMemoryStreamsFromResources(['WizardSmallImage'], ['250']);
    LineNumber := 0;

    { Prepare Setup executable & signed uninstaller data }
    if Output then begin
      AddStatus(SCompilerStatusPreparingSetupExe);
      PrepareSetupE32(SetupE32);
    end else
      AddStatus(SCompilerStatusSkippingPreparingSetupExe);

    { Read languages:

      0. Determine final code pages:
      Unicode Setup uses Unicode text and does not depend on the system code page. To
      provide Setup with Unicode text without requiring Unicode .isl files (but still
      supporting Unicode .iss, license and info files), the compiler converts the .isl
      files to Unicode during compilation. It also does this if it finds ANSI plain text
      license and info files. To be able to do this it needs to know the language's code
      page but as seen above it can't simply take this from the current .isl. And license
      and info files do not even have a language code page setting.

      This means the Unicode compiler has to do an extra phase: following the logic above
      it first determines the final language code page for each language, storing these
      into an extra list called PreDataList, and then it continues as normal while using
      the final language code page for any conversions needed.

      Note: it must avoid caching the .isl files while determining the code pages, since
      the conversion is done *before* the caching.

      1. Read Default.isl messages:

      ReadDefaultMessages calls EnumMessages for Default.isl's [Messages], with Ext set to -2.
      These messages are stored in DefaultLangData to be used as defaults for missing messages
      later on. EnumLangOptions isn't called, the defaults will (at run-time) be displayed
      using the code page of the language with the missing messages. EnumMessages for
      Default.isl's [CustomMessages] also isn't called at this point, missing custom messages
      are handled differently.

      2. Read [Languages] section and the .isl files the entries reference:

      EnumLanguages is called for the script. For each [Languages] entry its parameters
      are read and for the MessagesFiles parameter ReadMessagesFromFiles is called. For
      each file ReadMessagesFromFiles first calls EnumLangOptions, then EnumMessages for
      [Messages], and finally another EnumMessages for [CustomMessages], all with Ext set
      to the index of the language.

      All the [LangOptions] and [Messages] data is stored in single structures per language,
      namely LanguageEntries[Ext] (langoptions) and LangDataList[Ext] (messages), any 'double'
      directives or messages overwrite each other. This means if that for example the first
      messages file does not specify a code page, but the second does, the language will
      automatically use the code page of the second file. And vice versa.
      
      The [CustomMessages] data is stored in a single list for all languages, with each
      entry having a LangIndex property saying to which language it belongs. If a 'double'
      custom message is found, the existing one is removed from the list.

      3. Read [LangOptions] & [Messages] & [CustomMessages] in the script:

      ReadMessagesFromScript is called and this will first call CreateDefaultLanguageEntry
      if no languages have been defined. CreateDefaultLanguageEntry first creates a language
      with all settings set to the default, and then it calles ReadMessagesFromFiles for
      Default.isl for this language. ReadMessagesFromFiles works as described above.
      Note this is just like the script creator creating an entry for Default.isl.

      ReadMessagesFromScript then first calls EnumLangOptions, then EnumMessages for
      [Messages], and finally another EnumMessages for [CustomMessages] for the script.
      Note this is just like ReadMessagesFromFiles does for files, except that Ext is set
      to -1. This causes it to accept language identifiers ('en.LanguageCodePage=...'):
      if the identifier is set the read data is stored only for that language in the
      structures described above. If the identifier is not set, the read data is stored
      for all languages either by writing to all structures (langoptions/messages) or by
      adding an entry with LangIndex set to -1 (custommessages). This for example means
      all language code pages read so far could be overwritten from the script.

      ReadMessagesFromScript then checks for any missing messages and uses the messages
      read in the very beginning to provide defaults.

      After ReadMessagesFromScript returns, the read messages stored in the LangDataList
      entries are streamed into the LanguageEntry.Data fields by PopulateLanguageEntryData.

      4. Check 'language completeness' of custom message constants:
      CheckCustomMessageDefinitions is used to check for missing custom messages and
      where necessary it 'promotes' a custom message by resetting its LangIndex property
      to -1. }

    { 0. Determine final language code pages }
    AddStatus(SCompilerStatusDeterminingCodePages);

    { 0.1. Read [Languages] section and [LangOptions] in the .isl files the
      entries reference }
    EnumIniSection(EnumLanguagesPreProc, 'Languages', 0, True, True, '', False, True);
    CallIdleProc;

    { 0.2. Read [LangOptions] in the script }
    ReadMessagesFromScriptPre;

    { 1. Read Default.isl messages }
    AddStatus(SCompilerStatusReadingDefaultMessages);
    ReadDefaultMessages;

    { 2. Read [Languages] section and the .isl files the entries reference }
    EnumIniSection(EnumLanguagesProc, 'Languages', 0, True, True, '', False, False);
    CallIdleProc;

    { 3. Read [LangOptions] & [Messages] & [CustomMessages] in the script }
    AddStatus(SCompilerStatusParsingMessages);
    ReadMessagesFromScript;
    PopulateLanguageEntryData;

    { 4. Check 'language completeness' of custom message constants }
    CheckCustomMessageDefinitions;

    { Read (but not compile) [Code] section }
    ReadCode;

    { Read [Types] section }
    EnumIniSection(EnumTypesProc, 'Types', 0, True, True, '', False, False);
    CallIdleProc;

    { Read [Components] section }
    EnumIniSection(EnumComponentsProc, 'Components', 0, True, True, '', False, False);
    CallIdleProc;

    { Read [Tasks] section }
    EnumIniSection(EnumTasksProc, 'Tasks', 0, True, True, '', False, False);
    CallIdleProc;

    { Read [Dirs] section }
    EnumIniSection(EnumDirsProc, 'Dirs', 0, True, True, '', False, False);
    CallIdleProc;

    { Read [Icons] section }
    EnumIniSection(EnumIconsProc, 'Icons', 0, True, True, '', False, False);
    CallIdleProc;

    { Read [INI] section }
    EnumIniSection(EnumINIProc, 'INI', 0, True, True, '', False, False);
    CallIdleProc;

    { Read [Registry] section }
    EnumIniSection(EnumRegistryProc, 'Registry', 0, True, True, '', False, False);
    CallIdleProc;

    { Read [InstallDelete] section }
    EnumIniSection(EnumDeleteProc, 'InstallDelete', 0, True, True, '', False, False);
    CallIdleProc;

    { Read [UninstallDelete] section }
    EnumIniSection(EnumDeleteProc, 'UninstallDelete', 1, True, True, '', False, False);
    CallIdleProc;

    { Read [Run] section }
    EnumIniSection(EnumRunProc, 'Run', 0, True, True, '', False, False);
    CallIdleProc;

    { Read [UninstallRun] section }
    EnumIniSection(EnumRunProc, 'UninstallRun', 1, True, True, '', False, False);
    CallIdleProc;

    if MissingRunOnceIdsWarning and MissingRunOnceIds then
      WarningsList.Add(Format(SCompilerMissingRunOnceIdsWarning, ['UninstallRun', 'RunOnceId']));

    { Read [Files] section }
    if not TryStrToBoolean(SetupHeader.Uninstallable, Uninstallable) or Uninstallable then
      EnumFilesProc('', 1);
    EnumIniSection(EnumFilesProc, 'Files', 0, True, True, '', False, False);
    CallIdleProc;

    if UsedUserAreasWarning and (UsedUserAreas.Count > 0) and
       (SetupHeader.PrivilegesRequired in [prPowerUser, prAdmin]) then begin
      if SetupHeader.PrivilegesRequired = prPowerUser then
        PrivilegesRequiredValue := 'poweruser'
      else
        PrivilegesRequiredValue := 'admin';
      WarningsList.Add(Format(SCompilerUsedUserAreasWarning, ['Setup',
        'PrivilegesRequired', PrivilegesRequiredValue, UsedUserAreas.CommaText]));
    end;

    { Read decompressor DLL. Must be done after [Files] is parsed, since
      SetupHeader.CompressMethod isn't set until then }
    case SetupHeader.CompressMethod of
      cmZip: begin
          AddStatus(Format(SCompilerStatusReadingFile, ['isunzlib.dll']));
          DecompressorDLL := CreateMemoryStreamFromFile(CompilerDir + 'isunzlib.dll');
        end;
      cmBzip: begin
          AddStatus(Format(SCompilerStatusReadingFile, ['isbunzip.dll']));
          DecompressorDLL := CreateMemoryStreamFromFile(CompilerDir + 'isbunzip.dll');
        end;
    end;

    { Add default types if necessary }
    if (ComponentEntries.Count > 0) and (TypeEntries.Count = 0) then begin
      AddDefaultSetupType(DefaultTypeEntryNames[0], [], ttDefaultFull);
      AddDefaultSetupType(DefaultTypeEntryNames[1], [], ttDefaultCompact);
      AddDefaultSetupType(DefaultTypeEntryNames[2], [toIsCustom], ttDefaultCustom);
    end;

    { Check existence of expected custom message constants }
    CheckCustomMessageReferences;

    { Compile CodeText }
    CompileCode;
    CallIdleProc;

    { Clear any existing setup* files out of the output directory first (even
      if output is disabled. }
    EmptyOutputDir(True);
    if OutputManifestFile <> '' then
      DeleteFile(PrependDirName(OutputManifestFile, OutputDir));

    { Create setup files }
    if Output then begin
      AddStatus(SCompilerStatusCreateSetupFiles);
      ExeFilename := OutputDir + OutputBaseFilename + '.exe';
      try
        if not UseSetupLdr then begin
          SetupFile := TFile.Create(ExeFilename, fdCreateAlways, faWrite, fsNone);
          try
            SetupFile.WriteBuffer(SetupE32.Memory^, SetupE32.Size.Lo);
            SizeOfExe := SetupFile.Size.Lo;
          finally
            SetupFile.Free;
          end;
          CallIdleProc;

          if not DiskSpanning then begin
            { Create SETUP-0.BIN and SETUP-1.BIN }
            CompressFiles('', 0);
            CreateSetup0File;
          end
          else begin
            { Create SETUP-0.BIN and SETUP-*.BIN }
            SizeOfHeaders := CreateSetup0File;
            CompressFiles('', RoundToNearestClusterSize(SizeOfExe) +
              RoundToNearestClusterSize(SizeOfHeaders) +
              RoundToNearestClusterSize(ReserveBytes));
            { CompressFiles modifies setup header data, so go back and
              rewrite it }
            if CreateSetup0File <> SizeOfHeaders then
              { Make sure new and old size match. No reason why they
                shouldn't but check just in case }
              AbortCompile(SCompilerSetup0Mismatch);
          end;
        end
        else begin
          CopyFileOrAbort(CompilerDir + 'SETUPLDR.E32', ExeFilename);
          { if there was a read-only attribute, remove it }
          SetFileAttributes(PChar(ExeFilename), FILE_ATTRIBUTE_ARCHIVE);
          if SetupIconFilename <> '' then begin
            { update icons }
            AddStatus(Format(SCompilerStatusUpdatingIcons, ['SETUP.EXE']));
            LineNumber := SetupDirectiveLines[ssSetupIconFile];
            UpdateIcons(ExeFilename, PrependSourceDirName(SetupIconFilename), False);
            LineNumber := 0;
          end;
          SetupFile := TFile.Create(ExeFilename, fdOpenExisting, faReadWrite, fsNone);
          try
            UpdateSetupPEHeaderFields(SetupFile, TerminalServicesAware, DEPCompatible, ASLRCompatible);
            SizeOfExe := SetupFile.Size.Lo;
          finally
            SetupFile.Free;
          end;
          CallIdleProc;

          { When disk spanning isn't used, place the compressed files inside
            SETUP.EXE }
          if not DiskSpanning then
            CompressFiles(ExeFilename, 0);

          ExeFile := TFile.Create(ExeFilename, fdOpenExisting, faReadWrite, fsNone);
          try
            ExeFile.SeekToEnd;

            { Move the data from SETUP.E?? into the SETUP.EXE, and write
              header data }
            FillChar(SetupLdrOffsetTable, SizeOf(SetupLdrOffsetTable), 0);
            SetupLdrOffsetTable.ID := SetupLdrOffsetTableID;
            SetupLdrOffsetTable.Version := SetupLdrOffsetTableVersion;
            SetupLdrOffsetTable.Offset0 := ExeFile.Position.Lo;
            SizeOfHeaders := WriteSetup0(ExeFile);
            SetupLdrOffsetTable.OffsetEXE := ExeFile.Position.Lo;
            CompressSetupE32(SetupE32, ExeFile, SetupLdrOffsetTable.UncompressedSizeEXE,
              SetupLdrOffsetTable.CRCEXE);
            SetupLdrOffsetTable.TotalSize := ExeFile.Size.Lo;
            if DiskSpanning then begin
              SetupLdrOffsetTable.Offset1 := 0;
              { Compress the files in SETUP-*.BIN after we know the size of
                SETUP.EXE }
              CompressFiles('',
                RoundToNearestClusterSize(SetupLdrOffsetTable.TotalSize) +
                RoundToNearestClusterSize(ReserveBytes));
              { CompressFiles modifies setup header data, so go back and
                rewrite it }
              ExeFile.Seek(SetupLdrOffsetTable.Offset0);
              if WriteSetup0(ExeFile) <> SizeOfHeaders then
                { Make sure new and old size match. No reason why they
                  shouldn't but check just in case }
                AbortCompile(SCompilerSetup0Mismatch);
            end
            else
              SetupLdrOffsetTable.Offset1 := SizeOfExe;
            SetupLdrOffsetTable.TableCRC := GetCRC32(SetupLdrOffsetTable,
              SizeOf(SetupLdrOffsetTable) - SizeOf(SetupLdrOffsetTable.TableCRC));

            { Write SetupLdrOffsetTable to SETUP.EXE }
            if SeekToResourceData(ExeFile, Cardinal(RT_RCDATA), SetupLdrOffsetTableResID) <> SizeOf(SetupLdrOffsetTable) then
              AbortCompile('Wrong offset table resource size');
            ExeFile.WriteBuffer(SetupLdrOffsetTable, SizeOf(SetupLdrOffsetTable));

            { Update version info }
            AddStatus(Format(SCompilerStatusUpdatingVersionInfo, ['SETUP.EXE']));
            UpdateVersionInfo(ExeFile, VersionInfoVersion, VersionInfoProductVersion, VersionInfoCompany,
              VersionInfoDescription, VersionInfoTextVersion,
              VersionInfoCopyright, VersionInfoProductName, VersionInfoProductTextVersion, VersionInfoOriginalFileName,
              True);

            { Update manifest if needed }
            if UseSetupLdr then begin
              AddStatus(Format(SCompilerStatusUpdatingManifest, ['SETUP.EXE']));
              PreventCOMCTL32Sideloading(ExeFile);
            end;

            { For some reason, on Win95 the date/time of the EXE sometimes
              doesn't get updated after it's been written to so it has to
              manually set it. (I don't get it!!) }
            UpdateTimeStamp(ExeFile.Handle);
          finally
            ExeFile.Free;
          end;
        end;

        { Sign }
        if SignTools.Count > 0 then begin
          AddStatus(SCompilerStatusSigningSetup);
          Sign(ExeFileName);
        end;
      except
        EmptyOutputDir(False);
        raise;
      end;
      CallIdleProc;

      { Create manifest file }
      if OutputManifestFile <> '' then begin
        AddStatus(SCompilerStatusCreateManifestFile);
        CreateManifestFile;
        CallIdleProc;
      end;
    end else begin
      AddStatus(SCompilerStatusSkippingCreateSetupFiles);
      ExeFilename := '';
    end;

    { Finalize debug info }
    FinalizeDebugInfo;

    { Done }
    AddStatus('');
    for I := 0 to WarningsList.Count-1 do
      AddStatus(SCompilerStatusWarning + WarningsList[I], True);
    asm jmp @1; db 0,'Inno Setup Compiler, Copyright (C) 1997-2025 Jordan Russell, '
                  db 'Portions Copyright (C) 2000-2025 Martijn Laan',0; @1: end;
    { Note: Removing or modifying the copyright text is a violation of the
      Inno Setup license agreement; see LICENSE.TXT. }
  finally
    CallPreprocessorCleanupProc;
    UsedUserAreas.Clear;
    WarningsList.Clear;
    { Free all the data }
    DecompressorDLL.Free;
    SetupE32.Free;
    WizardSmallImages.Free;
    WizardImages.Free;
    FreeListItems(LanguageEntries, SetupLanguageEntryStrings, SetupLanguageEntryAnsiStrings);
    FreeListItems(CustomMessageEntries, SetupCustomMessageEntryStrings, SetupCustomMessageEntryAnsiStrings);
    FreeListItems(PermissionEntries, SetupPermissionEntryStrings, SetupPermissionEntryAnsiStrings);
    FreeListItems(TypeEntries, SetupTypeEntryStrings, SetupTypeEntryAnsiStrings);
    FreeListItems(ComponentEntries, SetupComponentEntryStrings, SetupComponentEntryAnsiStrings);
    FreeListItems(TaskEntries, SetupTaskEntryStrings, SetupTaskEntryAnsiStrings);
    FreeListItems(DirEntries, SetupDirEntryStrings, SetupDirEntryAnsiStrings);
    FreeListItems(FileEntries, SetupFileEntryStrings, SetupFileEntryAnsiStrings);
    FreeListItems(FileLocationEntries, SetupFileLocationEntryStrings, SetupFileLocationEntryAnsiStrings);
    FreeListItems(IconEntries, SetupIconEntryStrings, SetupIconEntryAnsiStrings);
    FreeListItems(IniEntries, SetupIniEntryStrings, SetupIniEntryAnsiStrings);
    FreeListItems(RegistryEntries, SetupRegistryEntryStrings, SetupRegistryEntryAnsiStrings);
    FreeListItems(InstallDeleteEntries, SetupDeleteEntryStrings, SetupDeleteEntryAnsiStrings);
    FreeListItems(UninstallDeleteEntries, SetupDeleteEntryStrings, SetupDeleteEntryAnsiStrings);
    FreeListItems(RunEntries, SetupRunEntryStrings, SetupRunEntryAnsiStrings);
    FreeListItems(UninstallRunEntries, SetupRunEntryStrings, SetupRunEntryAnsiStrings);
    FileLocationEntryFilenames.Clear;
    FreeLineInfoList(ExpectedCustomMessageNames);
    FreeLangData;
    FreePreLangData;
    FreeScriptFiles;
    FreeLineInfoList(CodeText);
    FreeAndNil(CompressProps);
    FreeAndNil(InternalCompressProps);
  end;
end;

end.
