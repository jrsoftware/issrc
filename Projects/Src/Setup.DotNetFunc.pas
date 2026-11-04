unit Setup.DotNetFunc;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  .NET functions

  Fusion code based on LibFusion.pas by RemObject Software
  License:
  // LibFusion.pas
  // copyright (c) 2009 by RemObjects Software
  //
  // Uses InnoSetup License

  Also see https://learn.microsoft.com/en-us/dotnet/framework/unmanaged-api/fusion
           https://learn.microsoft.com/en-us/windows/win32/sbscs/side-by-side-assembly-api

  IsDotNetInstalled code based on http://www.kynosarges.de/DotNetVersion.html by Cristoph Nahr
  License:
  // I’m placing this small bit of code in the public domain, so you may embed it in your own
  // projects, modify and redistribute it as you see fit.

  Also see https://docs.microsoft.com/en-us/dotnet/framework/migration-guide/how-to-determine-which-versions-are-installed
}

interface

uses
  Ole2, SysUtils, Windows, Shared.CommonFunc, Shared.DotNetVersion, Shared.Struct;

type
  IAssemblyCache = class(IUnknown)
    function UninstallAssembly(dwFlags: DWORD; pszAssemblyName: PChar; pRefData: Pointer; var pulDisposition: ULONG): HRESULT; virtual; stdcall; abstract;
    function QueryAssemblyInfo(dwFlags: DWORD; pszAssemblyName: PChar; pAsmInfo: Pointer): HRESULT; virtual; stdcall; abstract;
    function CreateAssemblyCacheItem(dwFlags: DWORD; pvReserved: Pointer; var ppAsmItem: Pointer; pszAssemblyName: PChar): HRESULT; virtual; stdcall; abstract;
    function CreateAssemblyScavenger(var ppUnkReserved: Pointer): HRESULT; virtual; stdcall; abstract;
    function InstallAssembly(dwFlags: DWORD; pszManifestFilePath: PChar; pRefData: Pointer): HRESULT; virtual; stdcall; abstract;
  end;

  TAssemblyCacheInfo = class
  private
    fDll: THandle;
    fCache: IAssemblyCache;
  public
    constructor Create(const RegView: TRegView);
    destructor Destroy; override;
    property Cache: IAssemblyCache read FCache;
    procedure InstallAssembly(const FileName: string);
    procedure UninstallAssembly(const StrongAssemblyName: string); // Full name! in 'AssemblyName, version=1.0.0.0, culture=neutral, publickeytoken=abcdef123456' format
  end;

  TDotNetBaseVersion = (netbase11, netbase20, netbase40, netbaseHighestKnown);

function GetDotNetInstallRoot(const RegView: TRegView): String;
function GetDotNetVersionInstallRoot(const RegView: TRegView; const Version: TDotNetBaseVersion): String;
function IsDotNetInstalled(const RegView: TRegView; const MinVersion: TDotNetVersion; const MinServicePack: DWORD): Boolean;
function IsDotNetRuntimeInstalled(const Runtime: String; const MinVersion: Int64; const Architecture: TSetupProcessorArchitecture): Boolean;

implementation

uses
  Setup.InstFunc, Setup.MainFunc, Shared.VerInfoFunc, PathFunc;

var
  DotNetRoot: array [TRegView] of String;
  DotNetVersionRoot: array [TRegView, TDotNetBaseVersion] of String;

{ GetDotNet(Version)InstallRoot }

function GetDotNetInstallRoot(const RegView: TRegView): String;
var
  K: HKEY;
begin
  if DotNetRoot[RegView] = '' then begin
    if RegOpenKeyExView(RegView, HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\.NETFramework', 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      RegQueryStringValue(K, 'InstallRoot', DotNetRoot[RegView]);
      RegCloseKey(K);
    end;
    if DotNetRoot[RegView] = '' then
      InternalError('.NET Framework not found');
  end;
  Result := DotNetRoot[RegView];
end;

function GetDotNetVersionInstallRoot(const RegView: TRegView; const Version: TDotNetBaseVersion): String;
const
  VersionStrings: array [TDotNetBaseVersion] of String = ('1.1', '2.0', '4.0', '');
var
  K: HKEY;
begin
  if DotNetVersionRoot[RegView, Version] = '' then begin
    GetDotNetInstallRoot(RegView);
    if (Version in [netbase40, netbaseHighestKnown]) and (RegOpenKeyExView(RegView, HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\.NETFramework\Policy\v4.0', 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS) then begin
      DotNetVersionRoot[RegView, Version] := AddBackslash(DotNetRoot[RegView]) + 'v4.0.30319';
      RegCloseKey(K);
    end else if (Version in [netbase20, netbaseHighestKnown]) and (RegOpenKeyExView(RegView, HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\.NETFramework\Policy\v2.0', 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS) then begin
      DotNetVersionRoot[RegView, Version] := AddBackslash(DotNetRoot[RegView]) + 'v2.0.50727';
      RegCloseKey(K);
    end else if (Version in [netbase11, netbaseHighestKnown]) and (RegOpenKeyExView(RegView, HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\.NETFramework\Policy\v1.1', 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS) then begin
      DotNetVersionRoot[RegView, Version] := AddBackslash(DotNetRoot[RegView]) + 'v1.1.4322';
      RegCloseKey(K);
    end;
    if DotNetVersionRoot[RegView, Version] = '' then begin
      if Version <> netbaseHighestKnown then
        InternalError(Format('.NET Framework version %s not found', [VersionStrings[Version]]))
      else
        InternalError('.NET Framework not found');
    end;
  end;
  Result := DotNetVersionRoot[RegView, Version];
end;


{ TAssemblyCacheInfo }

constructor TAssemblyCacheInfo.Create(const RegView: TRegView);
type
  TCreateAssemblyCache = function (var ppAsmCache: IAssemblyCache; dwReserved: Integer): Integer; stdcall;
var
  FileName: string;
  Proc: TCreateAssemblyCache;
begin
  inherited Create;
  FileName := AddBackslash(GetDotNetVersionInstallRoot(RegView, netbaseHighestKnown)) + 'Fusion.dll';
  fDll := SafeLoadLibrary(PChar(FileName), SEM_NOOPENFILEERRORBOX);
  if fDll = 0 then
    InternalError(Format('Failed to load .NET Framework DLL "%s"', [FileName]));
  Proc := GetProcAddress(fDll, 'CreateAssemblyCache');
  if not Assigned(Proc) then
    InternalError('Failed to get address of .NET Framework CreateAssemblyCache function');
  Proc(fCache, 0);
  if fCache = nil then
    InternalError('.NET Framework CreateAssemblyCache function failed');
end;

destructor TAssemblyCacheInfo.Destroy;
begin
  if fCache <> nil then
    fCache.Release;
  fCache := nil;
  FreeLibrary(fDll);
  inherited;
end;

procedure TAssemblyCacheInfo.InstallAssembly(const FileName: string);
const
  IASSEMBLYCACHE_INSTALL_FLAG_FORCE_REFRESH = 2;
begin
  const lOleString = StringToOleStr(FileName);
  try
    const OleResult = fCache.InstallAssembly(IASSEMBLYCACHE_INSTALL_FLAG_FORCE_REFRESH, lOleString, nil);
    if Failed(OleResult) then
      RaiseOleError('InstallAssembly', OleResult);
  finally
    SysFreeString(lOleString);
  end;
end;

procedure TAssemblyCacheInfo.UninstallAssembly(
  const StrongAssemblyName: string);
begin
  const lOleString = StringToOleStr(StrongAssemblyName);
  try
    const OleResult = fCache.UninstallAssembly(0, lOleString, nil, ULONG(nil^));
    if Failed(OleResult) then
      RaiseOleError('UninstallAssembly', OleResult);
  finally
    SysFreeString(lOleString);
  end;
end;

{ IsDotNetInstalled }

function IsDotNetInstalled(const RegView: TRegView; const MinVersion: TDotNetVersion; const MinServicePack: DWORD): Boolean;

  function GetVersionString(const Version: TDotNetVersion): String;
  begin
    case Version of
      net11: Result := 'v1.1';
      net20: Result := 'v2.0';
      net30: Result := 'v3.0';
      net35: Result := 'v3.5';
      net4Client: Result := 'v4\Client';
      net4Full: Result := 'v4\Full';
      net45: Result := 'v4.5';
      net451: Result := 'v4.5.1';
      net452: Result := 'v4.5.2';
      net46: Result := 'v4.6';
      net461: Result := 'v4.6.1';
      net462: Result := 'v4.6.2';
      net47: Result := 'v4.7';
      net471: Result := 'v4.7.1';
      net472: Result := 'v4.7.2';
      net48: Result := 'v4.8';
      net481: Result := 'v4.8.1';
      else
        InternalError('IsDotNetInstalled: Invalid Version');
    end;
  end;

  function QueryDWord(const SubKey, ValueName: String; var Value: DWORD): Boolean;
  var
    K: HKEY;
    Typ, Size: DWORD;
  begin
    if RegOpenKeyExView(RegView, HKEY_LOCAL_MACHINE, PChar(SubKey), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      Size := SizeOf(Value);
      Result := (RegQueryValueEx(K, PChar(ValueName), nil, @Typ, PByte(@Value), @Size) = ERROR_SUCCESS) and (Typ = REG_DWORD);
      RegCloseKey(K);
    end else
      Result := False;
  end;

var
  VersionString, VersionKey, SubKey: String;
  Install, InstalledRelease, InstalledServicePack, RequiredRelease: DWORD;
  Success: Boolean;
begin
  VersionString := GetVersionString(MinVersion);
  RequiredRelease := 0;

  // .NET 1.1 and 2.0 embed release number in version key
  if VersionString = 'v1.1' then
    VersionKey := 'v1.1.4322'
  else if VersionString = 'v2.0' then
    VersionKey := 'v2.0.50727'
  else begin
    // .NET 4.5 and newer install as update to .NET 4.0 Full
    if Pos('v4.', VersionString) = 1 then begin
      VersionKey := 'v4\Full';
      if VersionString = 'v4.5' then
        RequiredRelease := 378389
      else if VersionString = 'v4.5.1' then
        RequiredRelease := 378675 // 378758 on Windows 8 and older
      else if VersionString = 'v4.5.2' then
        RequiredRelease := 379893
      else if VersionString = 'v4.6' then
        RequiredRelease := 393295 // 393297 on Windows 8.1 and older
      else if VersionString = 'v4.6.1' then
        RequiredRelease := 394254 // 394271 before Win10 November Update
      else if VersionString = 'v4.6.2' then
        RequiredRelease := 394802 // 394806 before Win10 Anniversary Update
      else if VersionString = 'v4.7' then
        RequiredRelease := 460798 // 460805 before Win10 Creators Update
      else if VersionString = 'v4.7.1' then
        RequiredRelease := 461308 // 461310 before Win10 Fall Creators Update
      else if VersionString = 'v4.7.2' then
        RequiredRelease := 461808 // 461814 before Win10 April 2018 Update
      else if VersionString = 'v4.8' then
        RequiredRelease := 528040 // 528049 before Win10 May 2019 Update
      else if VersionString = 'v4.8.1' then
        RequiredRelease := 533320 // 533325 before Win11 2022 Update
      else
        InternalError('IsDotNetInstalled: Invalid VersionString');
    end else
      VersionKey := VersionString;
  end;

  SubKey := 'SOFTWARE\Microsoft\NET Framework Setup\NDP\' + VersionKey;

  if Pos('v3.0', VersionString) = 1 then
    Success := QueryDWord(SubKey + '\Setup', 'InstallSuccess', Install)
  else
    Success := QueryDWord(SubKey, 'Install', Install);

  if Success and (Install = 1) then begin
    if Pos('v4', VersionString) = 1 then
      Success := QueryDWord(SubKey, 'Servicing', InstalledServicePack)
    else
      Success := QueryDWord(SubKey, 'SP', InstalledServicePack);

      if Success and (InstalledServicePack >= MinServicePack) then begin
        if RequiredRelease > 0 then
          Success := QueryDWord(SubKey, 'Release', InstalledRelease) and (InstalledRelease >= RequiredRelease);
        Result := Success;
      end else
        Result := False;
  end else
    Result := False;
end;

{ IsDotNetRuntimeInstalled }

function IsDotNetRuntimeInstalled(const Runtime: String; const MinVersion: Int64; const Architecture: TSetupProcessorArchitecture): Boolean;
var
  ArchName, InstallDir, RuntimeDir, Name: String;
  K: HKEY;
  H: THandle;
  FindData: TWin32FindData;
  VersionNumbers: TFileVersionNumbers;
  Version: Int64;
begin
  Result := False;

  case Architecture of
    paX86: ArchName := 'x86';
    paX64: ArchName := 'x64';
    paArm64: ArchName := 'arm64';
  else
    Exit;
  end;

  // Like the .NET host, no default location for Arm64 on x64 Windows: skip architectures the system can't run
  if not (Architecture in MachineTypesSupportedBySystem) then
    Exit;

  // Same lookup as the .NET host: the registered install location, which is always in the 32-bit
  // view, else the default location. See https://github.com/dotnet/designs/blob/main/accepted/2020/install-locations.md
  InstallDir := '';
  if RegOpenKeyExView(rv32Bit, HKEY_LOCAL_MACHINE, PChar('SOFTWARE\dotnet\Setup\InstalledVersions\' + ArchName),
     0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
    if not RegQueryStringValue(K, 'InstallLocation', InstallDir) or not PathIsRooted(InstallDir) then
      InstallDir := '';
    RegCloseKey(K);
  end;
  if InstallDir = '' then begin
    if Architecture = paX86 then
      InstallDir := AddBackslash(ProgramFiles32Dir) + 'dotnet'
    else if IsWin64 then begin
      InstallDir := AddBackslash(ProgramFiles64Dir) + 'dotnet';
      // x64 .NET installs into a subdirectory on Arm64 Windows
      if (Architecture = paX64) and (ProcessorArchitecture = paArm64) then
        InstallDir := InstallDir + '\x64';
    end else
      Exit;
  end;

  // Each version is a subdirectory such as shared\Microsoft.NETCore.App\8.0.0
  RuntimeDir := AddBackslash(InstallDir) + 'shared\' + Runtime + '\';
  H := FindFirstFile(PChar(RuntimeDir + '*'), FindData);
  if H <> INVALID_HANDLE_VALUE then begin
    try
      repeat
        Name := FindData.cFileName;
        // Prerelease versions such as 10.0.0-rc.1.25451.107 are skipped
        if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and (Name <> '.') and (Name <> '..') and
           (Pos('-', Name) = 0) and StrToVersionNumbers(Name, VersionNumbers) then begin
          Version := VersionNumbersToInt64(VersionNumbers);
          // Apps only roll forward to a runtime with the same major version. Like the .NET host,
          // ignore a directory without .deps.json, which could be left over from an uninstall.
          if (Version shr 48 = MinVersion shr 48) and (Version >= MinVersion) and
             NewFileExists(RuntimeDir + Name + '\' + Runtime + '.deps.json') then
            Exit(True);
        end;
      until not FindNextFile(H, FindData);
    finally
      Windows.FindClose(H);
    end;
  end;
end;

end.
