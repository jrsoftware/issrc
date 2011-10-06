unit LibFusion;

{
  Inno Setup
  Copyright (C) 1997-2009 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  .NET functions

  Also see http://msdn.microsoft.com/en-us/library/ms404523.aspx
           http://msdn.microsoft.com/en-us/library/aa376204(VS.85).aspx

  $jrsoftware: issrc/Projects/LibFusion.pas,v 1.6 2010/04/15 08:35:26 mlaan Exp $

  Based on LibFusion.pas by RemObject Software updated for D2 compatibility, for
  Inno Setup support functions usage and some other improvements.

  // LibFusion.pas
  // copyright (c) 2009 by RemObjects Software
  //
  // Uses InnoSetup License
}

interface

uses
  Ole2, SysUtils, Windows, CmnFunc2;

type
  IAssemblyCache = class(Ole2.IUnknown)
    function UninstallAssembly(dwFlags: Integer; pszAssemblyName: PWideChar; pvReserved: Integer; var pulDisposition: Integer): Integer; virtual; stdcall; abstract;
    function QueryAssemblyInfo(dwFlags: Integer; pszAssemblyName: PWideChar; pAsmInfo: Integer): Integer; virtual; stdcall; abstract;
    function CreateAssemblyCacheItem(dwFlags: Integer; pvReserved: Integer; var ppAsmItem: Integer; pszAssemblyName: PWideChar): Integer; virtual; stdcall; abstract;
    function CreateAssemblyScavenger(var ppAsmScavenger: Pointer): Integer; virtual; stdcall; abstract;
    function InstallAssembly(dwFlags: Integer; pszManifestFilePath: PWideChar; pvReserved: Integer): Integer; virtual; stdcall; abstract;
  end;

  TAssemblyCacheInfo = class
  private
    fDll: THandle;
    fCache: IAssemblyCache;
  public
    constructor Create(RegView: TRegView);
    destructor Destroy; override;
    property Cache: IAssemblyCache read FCache;
    procedure InstallAssembly(const FileName: string);
    procedure UninstallAssembly(const StrongAssemblyName: string); // Full name! in 'AssemblyName, version=1.0.0.0, culture=neutral, publickeytoken=abcdef123456' format
  end;

  TDotNetVersion = (dt11, dt20, dt40, dtHighestKnown);

function GetDotNetRoot(RegView: TRegView): String;
function GetDotNetVersionRoot(RegView: TRegView; Version: TDotNetVersion): String;

implementation

uses
  InstFunc, PathFunc;

var
  DotNetRoot: array [TRegView] of String;
  DotNetVersionRoot: array [TRegView, TDotNetVersion] of String;

function GetDotNetRoot(RegView: TRegView): String;
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

function GetDotNetVersionRoot(RegView: TRegView; Version: TDotNetVersion): String;
const
  VersionStrings: array [TDotNetVersion] of String = ('1.1', '2.0', '4.0', '');
var
  K: HKEY;
begin
  if DotNetVersionRoot[RegView, Version] = '' then begin
    GetDotNetRoot(RegView);
    if (Version in [dt40, dtHighestKnown]) and (RegOpenKeyExView(RegView, HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\.NETFramework\Policy\v4.0', 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS) then begin
      DotNetVersionRoot[RegView, Version] := AddBackslash(DotNetRoot[RegView]) + 'v4.0.30319';
      RegCloseKey(K);
    end else if (Version in [dt20, dtHighestKnown]) and (RegOpenKeyExView(RegView, HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\.NETFramework\Policy\v2.0', 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS) then begin
      DotNetVersionRoot[RegView, Version] := AddBackslash(DotNetRoot[RegView]) + 'v2.0.50727';
      RegCloseKey(K);
    end else if (Version in [dt11, dtHighestKnown]) and (RegOpenKeyExView(RegView, HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\.NETFramework\Policy\v1.1', 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS) then begin
      DotNetVersionRoot[RegView, Version] := AddBackslash(DotNetRoot[RegView]) + 'v1.1.4322';
      RegCloseKey(K);
    end;
    if DotNetVersionRoot[RegView, Version] = '' then begin
      if Version <> dtHighestKnown then
        InternalError(Format('.NET Framework version %s not found', [VersionStrings[Version]]))
      else
        InternalError('.NET Framework not found');
    end;
  end;
  Result := DotNetVersionRoot[RegView, Version];
end;

constructor TAssemblyCacheInfo.Create(RegView: TRegView);
type
  TCreateAssemblyCache = function (var ppAsmCache: IAssemblyCache; dwReserved: Integer): Integer; stdcall;
var
  FileName: string;
  Proc: TCreateAssemblyCache;
begin
  inherited Create;
  FileName := AddBackslash(GetDotNetVersionRoot(RegView, dtHighestKnown)) + 'Fusion.dll';
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
  inherited Destroy;
end;

procedure TAssemblyCacheInfo.InstallAssembly(const FileName: string);
const
  IASSEMBLYCACHE_INSTALL_FLAG_FORCE_REFRESH = 2;
var
  lOleString: PWideChar;
  OleResult: HRESULT;
begin
  lOleString := StringToOleStr(FileName);
  try
    OleResult := fCache.InstallAssembly(IASSEMBLYCACHE_INSTALL_FLAG_FORCE_REFRESH, lOleString, 0);
    if Failed(OleResult) then
      RaiseOleError('InstallAssembly', OleResult);
  finally
    SysFreeString(lOleString);
  end;
end;

procedure TAssemblyCacheInfo.UninstallAssembly(
  const StrongAssemblyName: string);
var
  lOleString: PWideChar;
  OleResult: HRESULT;
begin
  lOleString := StringToOleStr(StrongAssemblyName);
  try
    OleResult := fCache.UninstallAssembly(0, lOleString, 0, Integer(nil^));
    if Failed(OleResult) then
      RaiseOleError('UninstallAssembly', OleResult);
  finally
    SysFreeString(lOleString);
  end;
end;

end.
