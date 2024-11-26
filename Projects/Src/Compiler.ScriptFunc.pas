unit Compiler.ScriptFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Script support functions (compile time - used by ISCmplr)
}

interface

uses
  Generics.Collections, uPSCompiler, uPSUtils;

procedure ScriptFuncLibraryRegister_C(ScriptCompiler: TPSPascalCompiler;
  ObsoleteFunctionWarnings: TDictionary<String, String>);

implementation

uses
  Windows, SysUtils, TypInfo,
  Shared.CommonFunc, Shared.SetupMessageIDs, Shared.Struct,
  Shared.SetupTypes, Shared.SetupSteps, Shared.ScriptFunc, Compiler.Messages, Shared.DotNetVersion;

{ This type copied from CmnFunc.pas. We don't actually 'use' CmnFunc since
  it would cause VCL units to be linked in. }
type
  TMsgBoxType = (mbInformation, mbConfirmation, mbError, mbCriticalError);

procedure ScriptFuncLibraryRegister_C(ScriptCompiler: TPSPascalCompiler;
  ObsoleteFunctionWarnings: TDictionary<String, String>);

  procedure RegisterType(const Name, Value: tbtstring);
  begin
    ScriptCompiler.AddTypeS(Name, Value);
  end;

  procedure RegisterFunctionTable(const FunctionTable: array of tbtstring);
  begin
    for var Func in FunctionTable do
      ScriptCompiler.AddFunction(Func);
  end;

  procedure RegisterDelphiFunctionTable(const FunctionTable: array of tbtstring);
  var
    I: Integer;
  begin
    for I := Low(FunctionTable) to High(FunctionTable) do
      ScriptCompiler.AddDelphiFunction(FunctionTable[I]);
  end;

  procedure RegisterConst(const Name: tbtstring; const Value: LongInt);
  var
    C: TPSConstant;
  begin
    C := ScriptCompiler.AddConstant(Name, ScriptCompiler.FindType('Longint'));
    C.Value.tU32 := Value;
  end;

  procedure RegisterRealEnum(const Name: tbtstring; const TypeInfo: PTypeInfo);
  var
    TypeData: PTypeData;
    S: tbtstring;
    I: Integer;
  begin
    TypeData := GetTypeData(TypeInfo);
    if (TypeInfo.Kind <> tkEnumeration) or (TypeData.MinValue <> 0) then
      raise Exception.Create('Internal error: RegisterRealEnum not passed a valid enum type');
    S := '(';
    for I := 0 to TypeData.MaxValue do begin
      if I > 0 then
        S := S + ',';
      S := S + tbtstring(GetEnumName(TypeInfo, I));
    end;
    S := S + ')';
    ScriptCompiler.AddTypeS(Name, S);
  end;

begin
  RegisterType('TArrayOfString', 'array of String');
  RegisterType('TArrayOfChar', 'array of Char');
  RegisterType('TArrayOfBoolean', 'array of Boolean');
  RegisterType('TArrayOfInteger', 'array of Integer');

  RegisterType('DWORD', 'LongWord');
  RegisterType('UINT', 'LongWord');
  RegisterType('BOOL', 'LongBool');
  { Note: In a native 64-bit build, these must be expanded to 64 bits }
  RegisterType('DWORD_PTR', 'LongWord');
  RegisterType('UINT_PTR', 'LongWord');
  RegisterType('INT_PTR', 'Longint');

  RegisterType('TFileTime',
    'record' +
    '  dwLowDateTime: DWORD;' +
    '  dwHighDateTime: DWORD;' +
    'end');

  RegisterRealEnum('TMsgBoxType', TypeInfo(TMsgBoxType));
  RegisterRealEnum('TSetupMessageID', TypeInfo(TSetupMessageID));
  RegisterRealEnum('TSetupStep', TypeInfo(TSetupStep));
  RegisterRealEnum('TUninstallStep', TypeInfo(TUninstallStep));
  RegisterRealEnum('TSetupProcessorArchitecture', TypeInfo(TSetupProcessorArchitecture));
  RegisterRealEnum('TDotNetVersion', TypeInfo(TDotNetVersion));

  RegisterType('TSplitType', '(stAll, stExcludeEmpty, stExcludeLastEmpty)'); //must be compatible with System.SysUtils.TStringSplitOptions

  RegisterType('TExecWait', '(ewNoWait, ewWaitUntilTerminated, ewWaitUntilIdle)');

  RegisterType('TExecOutput',
    'record' +
    '  StdOut: TArrayOfString;' +
    '  StdErr: TArrayOfString;' +
    '  Error: Boolean;' +
    'end');

  RegisterType('TFindRec',
    'record' +
    '  Name: String;' +
    '  Attributes: LongWord;' +
    '  SizeHigh: LongWord;' +
    '  SizeLow: LongWord;' +
    '  CreationTime: TFileTime;' +
    '  LastAccessTime: TFileTime;' +
    '  LastWriteTime: TFileTime;' +
    '  AlternateName: String;' +
    '  FindHandle: THandle;' +
    'end');
  RegisterType('TWindowsVersion',
    'record' +
    '  Major: Cardinal;' +
    '  Minor: Cardinal;' +
    '  Build: Cardinal;' +
    '  ServicePackMajor: Cardinal;' +
    '  ServicePackMinor: Cardinal;' +
    '  NTPlatform: Boolean;' +
    '  ProductType: Byte;' +
    '  SuiteMask: Word;' +
    'end');

  RegisterType('TOnDownloadProgress', 'function(const Url, FileName: String; const Progress, ProgressMax: Int64): Boolean;');
  RegisterType('TOnExtractionProgress', 'function(const ArchiveName, FileName: String; const Progress, ProgressMax: Int64): Boolean;');
  RegisterType('TOnLog', 'procedure(const S: String; const Error, FirstLine: Boolean);');

  for var ScriptFuncTable in ScriptFuncTables do
    RegisterFunctionTable(ScriptFuncTable);
  RegisterDelphiFunctionTable(DelphiScriptFuncTable);
  ObsoleteFunctionWarnings.Add('IsAdminLoggedOn', Format(SCompilerCodeFunctionRenamedWithAlternative, ['IsAdminLoggedOn', 'IsAdmin', 'IsAdminInstallMode']));
  ObsoleteFunctionWarnings.Add('IsComponentSelected', Format(SCompilerCodeFunctionRenamed, ['IsComponentSelected', 'WizardIsComponentSelected']));
  ObsoleteFunctionWarnings.Add('IsTaskSelected', Format(SCompilerCodeFunctionRenamed, ['IsTaskSelected', 'WizardIsTaskSelected']));
  ObsoleteFunctionWarnings.Add('IsX64', Format(SCompilerCodeFunctionDeprecatedWithAlternativeAndDocs, ['IsX64', 'IsX64OS', 'IsX64Compatible', 'Architecture Identifiers']));
  ObsoleteFunctionWarnings.Add('FileCopy', Format(SCompilerCodeFunctionRenamed, ['FileCopy', 'CopyFile']));

  RegisterConst('MaxInt', MaxInt);

  ScriptCompiler.AddConstantN('irInstall', 'Boolean').SetUInt(1);

  RegisterConst('wpWelcome', wpWelcome);
  RegisterConst('wpLicense', wpLicense);
  RegisterConst('wpPassword', wpPassword);
  RegisterConst('wpInfoBefore', wpInfoBefore);
  RegisterConst('wpUserInfo', wpUserInfo);
  RegisterConst('wpSelectDir', wpSelectDir);
  RegisterConst('wpSelectComponents', wpSelectComponents);
  RegisterConst('wpSelectProgramGroup', wpSelectProgramGroup);
  RegisterConst('wpSelectTasks', wpSelectTasks);
  RegisterConst('wpReady', wpReady);
  RegisterConst('wpPreparing', wpPreparing);
  RegisterConst('wpInstalling', wpInstalling);
  RegisterConst('wpInfoAfter', wpInfoAfter);
  RegisterConst('wpFinished', wpFinished);

  RegisterConst('MB_OK', MB_OK);
  RegisterConst('MB_OKCANCEL', MB_OKCANCEL);
  RegisterConst('MB_ABORTRETRYIGNORE', MB_ABORTRETRYIGNORE);
  RegisterConst('MB_YESNOCANCEL', MB_YESNOCANCEL);
  RegisterConst('MB_YESNO', MB_YESNO);
  RegisterConst('MB_RETRYCANCEL', MB_RETRYCANCEL);
  RegisterConst('MB_DEFBUTTON1', MB_DEFBUTTON1);
  RegisterConst('MB_DEFBUTTON2', MB_DEFBUTTON2);
  RegisterConst('MB_DEFBUTTON3', MB_DEFBUTTON3);
  RegisterConst('MB_SETFOREGROUND', MB_SETFOREGROUND);

  RegisterConst('IDOK', IDOK);
  RegisterConst('IDCANCEL', IDCANCEL);
  RegisterConst('IDABORT', IDABORT);
  RegisterConst('IDRETRY', IDRETRY);
  RegisterConst('IDIGNORE', IDIGNORE);
  RegisterConst('IDYES', IDYES);
  RegisterConst('IDNO', IDNO);

  RegisterConst('HWND_BROADCAST', HWND_BROADCAST);

  RegisterConst('HKEY_AUTO', LongInt(HKEY_AUTO));
  RegisterConst('HKEY_AUTO_32', LongInt(HKEY_AUTO or CodeRootKeyFlag32Bit));
  RegisterConst('HKEY_AUTO_64', LongInt(HKEY_AUTO or CodeRootKeyFlag64Bit));
  RegisterConst('HKEY_CLASSES_ROOT', LongInt(HKEY_CLASSES_ROOT));
  RegisterConst('HKEY_CLASSES_ROOT_32', LongInt(HKEY_CLASSES_ROOT or CodeRootKeyFlag32Bit));
  RegisterConst('HKEY_CLASSES_ROOT_64', LongInt(HKEY_CLASSES_ROOT or CodeRootKeyFlag64Bit));
  RegisterConst('HKEY_CURRENT_USER', LongInt(HKEY_CURRENT_USER));
  RegisterConst('HKEY_CURRENT_USER_32', LongInt(HKEY_CURRENT_USER or CodeRootKeyFlag32Bit));
  RegisterConst('HKEY_CURRENT_USER_64', LongInt(HKEY_CURRENT_USER or CodeRootKeyFlag64Bit));
  RegisterConst('HKEY_LOCAL_MACHINE', LongInt(HKEY_LOCAL_MACHINE));
  RegisterConst('HKEY_LOCAL_MACHINE_32', LongInt(HKEY_LOCAL_MACHINE or CodeRootKeyFlag32Bit));
  RegisterConst('HKEY_LOCAL_MACHINE_64', LongInt(HKEY_LOCAL_MACHINE or CodeRootKeyFlag64Bit));
  RegisterConst('HKEY_USERS', LongInt(HKEY_USERS));
  RegisterConst('HKEY_USERS_32', LongInt(HKEY_USERS or CodeRootKeyFlag32Bit));
  RegisterConst('HKEY_USERS_64', LongInt(HKEY_USERS or CodeRootKeyFlag64Bit));
  RegisterConst('HKEY_PERFORMANCE_DATA', LongInt(HKEY_PERFORMANCE_DATA));
  RegisterConst('HKEY_CURRENT_CONFIG', LongInt(HKEY_CURRENT_CONFIG));
  RegisterConst('HKEY_CURRENT_CONFIG_32', LongInt(HKEY_CURRENT_CONFIG or CodeRootKeyFlag32Bit));
  RegisterConst('HKEY_CURRENT_CONFIG_64', LongInt(HKEY_CURRENT_CONFIG or CodeRootKeyFlag64Bit));
  RegisterConst('HKEY_DYN_DATA', LongInt(HKEY_DYN_DATA));

  RegisterConst('HKA', LongInt(HKEY_AUTO));
  RegisterConst('HKA32', LongInt(HKEY_AUTO or CodeRootKeyFlag32Bit));
  RegisterConst('HKA64', LongInt(HKEY_AUTO or CodeRootKeyFlag64Bit));
  RegisterConst('HKCR', LongInt(HKEY_CLASSES_ROOT));
  RegisterConst('HKCR32', LongInt(HKEY_CLASSES_ROOT or CodeRootKeyFlag32Bit));
  RegisterConst('HKCR64', LongInt(HKEY_CLASSES_ROOT or CodeRootKeyFlag64Bit));
  RegisterConst('HKCU', LongInt(HKEY_CURRENT_USER));
  RegisterConst('HKCU32', LongInt(HKEY_CURRENT_USER or CodeRootKeyFlag32Bit));
  RegisterConst('HKCU64', LongInt(HKEY_CURRENT_USER or CodeRootKeyFlag64Bit));
  RegisterConst('HKLM', LongInt(HKEY_LOCAL_MACHINE));
  RegisterConst('HKLM32', LongInt(HKEY_LOCAL_MACHINE or CodeRootKeyFlag32Bit));
  RegisterConst('HKLM64', LongInt(HKEY_LOCAL_MACHINE or CodeRootKeyFlag64Bit));
  RegisterConst('HKU', LongInt(HKEY_USERS));
  RegisterConst('HKU32', LongInt(HKEY_USERS or CodeRootKeyFlag32Bit));
  RegisterConst('HKU64', LongInt(HKEY_USERS or CodeRootKeyFlag64Bit));
  RegisterConst('HKCC', LongInt(HKEY_CURRENT_CONFIG));
  RegisterConst('HKCC32', LongInt(HKEY_CURRENT_CONFIG or CodeRootKeyFlag32Bit));
  RegisterConst('HKCC64', LongInt(HKEY_CURRENT_CONFIG or CodeRootKeyFlag64Bit));

  RegisterConst('SW_HIDE', SW_HIDE);
  RegisterConst('SW_SHOWNORMAL', SW_SHOWNORMAL);
  RegisterConst('SW_SHOWMINIMIZED', SW_SHOWMINIMIZED);
  RegisterConst('SW_SHOWMAXIMIZED', SW_SHOWMAXIMIZED);
  RegisterConst('SW_SHOWMINNOACTIVE', SW_SHOWMINNOACTIVE);
  RegisterConst('SW_SHOW', SW_SHOW);

  RegisterConst('FILE_ATTRIBUTE_READONLY', FILE_ATTRIBUTE_READONLY);
  RegisterConst('FILE_ATTRIBUTE_HIDDEN', FILE_ATTRIBUTE_HIDDEN);
  RegisterConst('FILE_ATTRIBUTE_SYSTEM', FILE_ATTRIBUTE_SYSTEM);
  RegisterConst('FILE_ATTRIBUTE_DIRECTORY', FILE_ATTRIBUTE_DIRECTORY);
  RegisterConst('FILE_ATTRIBUTE_ARCHIVE', FILE_ATTRIBUTE_ARCHIVE);
  RegisterConst('FILE_ATTRIBUTE_DEVICE', $00000040);
  RegisterConst('FILE_ATTRIBUTE_NORMAL', FILE_ATTRIBUTE_NORMAL);
  RegisterConst('FILE_ATTRIBUTE_TEMPORARY', FILE_ATTRIBUTE_TEMPORARY);
  RegisterConst('FILE_ATTRIBUTE_SPARSE_FILE', $00000200);
  RegisterConst('FILE_ATTRIBUTE_REPARSE_POINT', $00000400);
  RegisterConst('FILE_ATTRIBUTE_COMPRESSED', $00000800);
  RegisterConst('FILE_ATTRIBUTE_OFFLINE', $00001000);
  RegisterConst('FILE_ATTRIBUTE_NOT_CONTENT_INDEXED', $00002000);
  RegisterConst('FILE_ATTRIBUTE_ENCRYPTED', $00004000);

  RegisterConst('VER_NT_WORKSTATION', $0000001);
  RegisterConst('VER_NT_DOMAIN_CONTROLLER', $0000002);
  RegisterConst('VER_NT_SERVER', $0000003);

  RegisterConst('VER_SUITE_SMALLBUSINESS', $00000001);
  RegisterConst('VER_SUITE_ENTERPRISE', $00000002);
  RegisterConst('VER_SUITE_BACKOFFICE', $00000004);
  RegisterConst('VER_SUITE_COMMUNICATIONS', $00000008);
  RegisterConst('VER_SUITE_TERMINAL', $00000010);
  RegisterConst('VER_SUITE_SMALLBUSINESS_RESTRICTED', $00000020);
  RegisterConst('VER_SUITE_EMBEDDEDNT', $00000040);
  RegisterConst('VER_SUITE_DATACENTER', $00000080);
  RegisterConst('VER_SUITE_SINGLEUSERTS', $00000100);
  RegisterConst('VER_SUITE_PERSONAL', $00000200);
  RegisterConst('VER_SUITE_BLADE', $00000400);
  RegisterConst('VER_SUITE_EMBEDDED_RESTRICTED', $00000800);
  RegisterConst('VER_SUITE_SECURITY_APPLIANCE', $00001000);
end;

end.
