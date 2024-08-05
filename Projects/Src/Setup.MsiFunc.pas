unit Setup.MsiFunc;

{
  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  MSI functions
}

interface

function IsMsiProductInstalled(const UpgradeCode: String; const PackedMinVersion: Int64; var ErrorCode: Cardinal): Boolean;

implementation

uses
  Windows, SysUtils, Shared.CommonFunc, PathFunc, Shared.VerInfoFunc;

var
  MsiLoaded: Boolean;
  MsiLibrary: HMODULE;
  MsiEnumRelatedProductsFunc: function(lpUpgradeCode: PChar; dwReserved, iProductIndex: DWORD; lpProductBuf: PChar): UINT; stdcall;
  MsiGetProductInfoFunc: function(szProduct, szAttribute, lpValueBuf: PChar; var pcchValueBuf: DWORD): UINT; stdcall;
  MsiLibraryLastError, MsiEnumRelatedProductsFuncLastError, MsiGetProductInfoFuncLastError: Cardinal;

function IsMsiProductInstalled(const UpgradeCode: String; const PackedMinVersion: Int64; var ErrorCode: Cardinal): Boolean;
var
  ProductCode: array[0..38] of Char;
  VersionStringSize: DWORD;
  VersionString: String;
  VersionNumbers: TFileVersionNumbers;
  PackedVersion: Int64;
begin
  Result := False;

  if not MsiLoaded then begin
    MsiLibrary := SafeLoadLibrary(AddBackslash(GetSystemDir) + 'msi.dll', SEM_NOOPENFILEERRORBOX);
    if MsiLibrary <> 0 then begin
      MsiEnumRelatedProductsFunc := GetProcAddress(MsiLibrary, 'MsiEnumRelatedProductsW');
      MsiEnumRelatedProductsFuncLastError := GetLastError;
      MsiGetProductInfoFunc := GetProcAddress(MsiLibrary, 'MsiGetProductInfoW');
      MsiGetProductInfoFuncLastError := GetLastError;
    end else
      MsiLibraryLastError := GetLastError;
    MsiLoaded := True;
  end;

  if MsiLibrary = 0 then
    ErrorCode := MsiLibraryLastError
  else if not Assigned(MsiEnumRelatedProductsFunc) then
    ErrorCode := MsiEnumRelatedProductsFuncLastError
  else if not Assigned(MsiGetProductInfoFunc) then
    ErrorCode := MsiGetProductInfoFuncLastError
  else
    ErrorCode := ERROR_SUCCESS;
  if ErrorCode <> ERROR_SUCCESS then
    Exit;

  ErrorCode := MsiEnumRelatedProductsFunc(PChar(UpgradeCode), 0, 0, ProductCode);
  if ErrorCode <> ERROR_SUCCESS then begin
    if ErrorCode = ERROR_NO_MORE_ITEMS then
      ErrorCode := ERROR_SUCCESS;  { Not installed so should just return False without an error }
    Exit;
  end;

  VersionStringSize := 16;
  SetLength(VersionString, VersionStringSize);
  ErrorCode := MsiGetProductInfoFunc(ProductCode, 'VersionString', PChar(VersionString), VersionStringSize);
  if ErrorCode = ERROR_MORE_DATA then begin
    Inc(VersionStringSize);
    SetLength(VersionString, VersionStringSize);
    ErrorCode := MsiGetProductInfoFunc(ProductCode, 'VersionString', PChar(VersionString), VersionStringSize);
  end;
  if ErrorCode <> ERROR_SUCCESS then
    Exit;
  SetLength(VersionString, VersionStringSize);

  if not StrToVersionNumbers(VersionString, VersionNumbers) then begin
    ErrorCode := ERROR_BAD_FORMAT;
    Exit;
  end;

  PackedVersion := (Int64(VersionNumbers.MS) shl 32) or VersionNumbers.LS;
  Result := PackedVersion >= PackedMinVersion;
  ErrorCode := ERROR_SUCCESS;
end;

end.
