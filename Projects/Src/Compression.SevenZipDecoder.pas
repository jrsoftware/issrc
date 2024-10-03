unit Compression.SevenZipDecoder;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Interface to the 7-Zip Decoder OBJ in Compression.SevenZipDecoder\7ZipDecode,
  used by Setup.
}

interface

procedure SevenZipDecode(const FileName, DestDir: String;
  const FullPaths: Boolean);

implementation

uses
  Windows, SysUtils, Setup.LoggingFunc;

{$L Src\Compression.SevenZipDecoder\7zDecode\IS7zDec.obj}

function IS_7zDec(const fileName: PChar; const fullPaths: Bool): Integer; cdecl; external name '_IS_7zDec';

{.$DEFINE VISUALSTUDIO}

//https://github.com/rust-lang/compiler-builtins/issues/403
{$IFDEF VISUALSTUDIO}
procedure __allshl; register; external 'ntdll.dll' name '_allshl';
procedure __aullshr; register; external 'ntdll.dll' name '_aullshr';
{$ENDIF}
procedure __aullrem; stdcall; external 'ntdll.dll' name '_aullrem';
procedure __aulldiv; stdcall; external 'ntdll.dll' name '_aulldiv';

function _memcpy(dest, src: Pointer; n: Cardinal): Pointer; cdecl;
begin
  Move(src^, dest^, n);
  Result := dest;
end;

function _memset(dest: Pointer; c: Integer; n: Cardinal): Pointer; cdecl;
begin
  FillChar(dest^, n, c);
  Result := dest;
end;

function _malloc(size: Cardinal): Pointer; cdecl;
begin
  if size <> 0 then
    Result := VirtualAlloc(nil, size, MEM_COMMIT, PAGE_READWRITE)
  else
    Result := nil;
end;

procedure _free(address: Pointer); cdecl;
begin
  if Assigned(address) then
    VirtualFree(address, 0, MEM_RELEASE);
end;

function _wcscmp(string1, string2: PChar): Integer; cdecl;
begin
  Result := StrComp(string1, string2);
end;

function _fputs(str: PAnsiChar; unused: Pointer): Integer; cdecl;
begin
  Log(UTF8ToString(str));
  Result := 1;
end;

procedure SevenZipDecode(const FileName, DestDir: String;
  const FullPaths: Boolean);
begin
  var SaveCurDir := GetCurrentDir;
  SetCurrentDir(DestDir);
  IS_7zDec(PChar(FileName), FullPaths);
  SetCurrentDir(SaveCurDir);
end;

end.
