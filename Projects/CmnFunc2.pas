unit CmnFunc2;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Common non-VCL functions
}

{$B-,R-}

interface

{$I VERSION.INC}

uses
  Windows, SysUtils;

{ Delphi 2.01's RegStr unit should never be used because it contains many
  wrong declarations. Delphi 3's RegStr unit doesn't have this problem, but
  for backward compatibility, it defines a few of the correct registry key
  constants here. }
const
  { Do NOT localize any of these }
  NEWREGSTR_PATH_SETUP = 'Software\Microsoft\Windows\CurrentVersion';
  NEWREGSTR_PATH_EXPLORER = NEWREGSTR_PATH_SETUP + '\Explorer';
  NEWREGSTR_PATH_SPECIAL_FOLDERS = NEWREGSTR_PATH_EXPLORER + '\Shell Folders';
  NEWREGSTR_PATH_UNINSTALL = NEWREGSTR_PATH_SETUP + '\Uninstall';
  NEWREGSTR_VAL_UNINSTALLER_DISPLAYNAME = 'DisplayName';
  NEWREGSTR_VAL_UNINSTALLER_COMMANDLINE = 'UninstallString';

  KEY_WOW64_64KEY = $0100;

type
{$IFNDEF UNICODE}
  PLeadByteSet = ^TLeadByteSet;
  TLeadByteSet = set of AnsiChar;
{$ENDIF}

  TOneShotTimer = {$IFDEF UNICODE} record {$ELSE} object {$ENDIF}
  private
    FLastElapsed: Cardinal;
    FStartTick: DWORD;
    FTimeout: Cardinal;
  public
    function Expired: Boolean;
    procedure SleepUntilExpired;
    procedure Start(const Timeout: Cardinal);
    function TimeElapsed: Cardinal;
    function TimeRemaining: Cardinal;
  end;

  TRegView = (rvDefault, rv32Bit, rv64Bit);
const
  RegViews64Bit = [rv64Bit];

function NewFileExists(const Name: String): Boolean;
function DirExists(const Name: String): Boolean;
function FileOrDirExists(const Name: String): Boolean;
function IsDirectoryAndNotReparsePoint(const Name: String): Boolean;
function GetIniString(const Section, Key: String; Default: String; const Filename: String): String;
function GetIniInt(const Section, Key: String; const Default, Min, Max: Longint; const Filename: String): Longint;
function GetIniBool(const Section, Key: String; const Default: Boolean; const Filename: String): Boolean;
function IniKeyExists(const Section, Key, Filename: String): Boolean;
function IsIniSectionEmpty(const Section, Filename: String): Boolean;
function SetIniString(const Section, Key, Value, Filename: String): Boolean;
function SetIniInt(const Section, Key: String; const Value: Longint; const Filename: String): Boolean;
function SetIniBool(const Section, Key: String; const Value: Boolean; const Filename: String): Boolean;
procedure DeleteIniEntry(const Section, Key, Filename: String);
procedure DeleteIniSection(const Section, Filename: String);
function GetEnv(const EnvVar: String): String;
function GetCmdTail: String;
function GetCmdTailEx(StartIndex: Integer): String;
function NewParamCount: Integer;
function NewParamStr(Index: Integer): string;
function AddQuotes(const S: String): String;
function RemoveQuotes(const S: String): String;
function GetShortName(const LongName: String): String;
function GetWinDir: String;
function GetSystemDir: String;
function GetSysWow64Dir: String;
function GetSysNativeDir(const IsWin64: Boolean): String;
function GetTempDir: String;
function StringChange(var S: String; const FromStr, ToStr: String): Integer;
function StringChangeEx(var S: String; const FromStr, ToStr: String;
  const SupportDBCS: Boolean): Integer;
function AdjustLength(var S: String; const Res: Cardinal): Boolean;
function UsingWinNT: Boolean;
function ConvertConstPercentStr(var S: String): Boolean;
function ConvertPercentStr(var S: String): Boolean;
function ConstPos(const Ch: Char; const S: String): Integer;
function SkipPastConst(const S: String; const Start: Integer): Integer;
function RegQueryStringValue(H: HKEY; Name: PChar; var ResultStr: String): Boolean;
function RegQueryMultiStringValue(H: HKEY; Name: PChar; var ResultStr: String): Boolean;
function RegValueExists(H: HKEY; Name: PChar): Boolean;
function RegCreateKeyExView(const RegView: TRegView; hKey: HKEY; lpSubKey: PChar;
  Reserved: DWORD; lpClass: PChar; dwOptions: DWORD; samDesired: REGSAM;
  lpSecurityAttributes: PSecurityAttributes; var phkResult: HKEY;
  lpdwDisposition: PDWORD): Longint;
function RegOpenKeyExView(const RegView: TRegView; hKey: HKEY; lpSubKey: PChar;
  ulOptions: DWORD; samDesired: REGSAM; var phkResult: HKEY): Longint;
function RegDeleteKeyView(const RegView: TRegView; const Key: HKEY; const Name: PChar): Longint;
function RegDeleteKeyIncludingSubkeys(const RegView: TRegView; const Key: HKEY; const Name: PChar): Longint;
function RegDeleteKeyIfEmpty(const RegView: TRegView; const RootKey: HKEY; const SubkeyName: PChar): Longint;
function GetShellFolderPath(const FolderID: Integer): String;
function IsAdminLoggedOn: Boolean;
function IsPowerUserLoggedOn: Boolean;
function IsMultiByteString(const S: AnsiString): Boolean;
function FontExists(const FaceName: String): Boolean;
{$IFNDEF IS_D5}
procedure FreeAndNil(var Obj);
function SafeLoadLibrary(const Filename: String; ErrorMode: UINT): HMODULE;
{$ENDIF}
function GetUILanguage: LANGID;
function RemoveAccelChar(const S: String): String;
function GetTextWidth(const DC: HDC; S: String; const Prefix: Boolean): Integer;
function AddPeriod(const S: String): String;
function GetExceptMessage: String;
function GetPreferredUIFont: String;
function IsWildcard(const Pattern: String): Boolean;
function WildcardMatch(const Text, Pattern: PChar): Boolean;
function IntMax(const A, B: Integer): Integer;
function Win32ErrorString(ErrorCode: Integer): String;
{$IFNDEF UNICODE}
procedure GetLeadBytes(var ALeadBytes: TLeadByteSet);
{$ENDIF}
{$IFNDEF IS_D3}
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;
{$ENDIF}
function DeleteDirTree(const Dir: String): Boolean;
function SetNTFSCompression(const FileOrDir: String; Compress: Boolean): Boolean;
procedure AddToWindowMessageFilterEx(const Wnd: HWND; const Msg: UINT);
{$IFNDEF UNICODE}
type
  TSysCharSet = set of AnsiChar;
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
{$ENDIF}
function ShutdownBlockReasonCreate(Wnd: HWND; const Reason: String): Boolean;
function ShutdownBlockReasonDestroy(Wnd: HWND): Boolean;
function TryStrToBoolean(const S: String; var BoolResult: Boolean): Boolean;
procedure WaitMessageWithTimeout(const Milliseconds: DWORD);
function MoveFileReplace(const ExistingFileName, NewFileName: String): Boolean;
procedure TryEnableAutoCompleteFileSystem(Wnd: HWND);
function IsWebPackage(const PackageSource: String): Boolean;
function ExtractWebFileName(const PackageSource: String): String;

{$IFNDEF UNICODE}
var
  ConstLeadBytes: PLeadByteSet = nil;
{$ENDIF}

implementation

uses
  {$IFNDEF Delphi3orHigher} OLE2, ShlObj, {$ENDIF} PathFunc;

{$IFDEF Delphi3orHigher}

{ Avoid including Variants (via ActiveX and ShlObj) in SetupLdr (SetupLdr uses CmnFunc2), saving 26 KB. }

const
  shell32 = 'shell32.dll';

type
  PSHItemID = ^TSHItemID;
  _SHITEMID = record
    cb: Word;                         { Size of the ID (including cb itself) }
    abID: array[0..0] of Byte;        { The item ID (variable length) }
  end;
  TSHItemID = _SHITEMID;
  SHITEMID = _SHITEMID;

  PItemIDList = ^TItemIDList;
  _ITEMIDLIST = record
     mkid: TSHItemID;
   end;
  TItemIDList = _ITEMIDLIST;
  ITEMIDLIST = _ITEMIDLIST;

  IMalloc = interface(IUnknown)
    ['{00000002-0000-0000-C000-000000000046}']
    function Alloc(cb: Longint): Pointer; stdcall;
    function Realloc(pv: Pointer; cb: Longint): Pointer; stdcall;
    procedure Free(pv: Pointer); stdcall;
    function GetSize(pv: Pointer): Longint; stdcall;
    function DidAlloc(pv: Pointer): Integer; stdcall;
    procedure HeapMinimize; stdcall;
  end;

function SHGetMalloc(var ppMalloc: IMalloc): HResult; stdcall; external shell32 name 'SHGetMalloc';
function SHGetSpecialFolderLocation(hwndOwner: HWND; nFolder: Integer;
  var ppidl: PItemIDList): HResult; stdcall; external shell32 name 'SHGetSpecialFolderLocation';
function SHGetPathFromIDList(pidl: PItemIDList; pszPath: PChar): BOOL; stdcall;
  external shell32 name {$IFDEF UNICODE}'SHGetPathFromIDListW'{$ELSE}'SHGetPathFromIDListA'{$ENDIF};

{$ENDIF}

function InternalGetFileAttr(const Name: String): Integer;
begin
  Result := GetFileAttributes(PChar(RemoveBackslashUnlessRoot(Name)));
end;

function NewFileExists(const Name: String): Boolean;
{ Returns True if the specified file exists.
  This function is better than Delphi's FileExists function because it works
  on files in directories that don't have "list" permission. There is, however,
  one other difference: FileExists allows wildcards, but this function does
  not. }
var
  Attr: Integer;
begin
  Attr := GetFileAttributes(PChar(Name));
  Result := (Attr <> -1) and (Attr and faDirectory = 0);
end;

function DirExists(const Name: String): Boolean;
{ Returns True if the specified directory name exists. The specified name
  may include a trailing backslash.
  NOTE: Delphi's FileCtrl unit has a similar function called DirectoryExists.
  However, the implementation is different between Delphi 1 and 2. (Delphi 1
  does not count hidden or system directories as existing.) }
var
  Attr: Integer;
begin
  Attr := InternalGetFileAttr(Name);
  Result := (Attr <> -1) and (Attr and faDirectory <> 0);
end;

function FileOrDirExists(const Name: String): Boolean;
{ Returns True if the specified directory or file name exists. The specified
  name may include a trailing backslash. }
begin
  Result := InternalGetFileAttr(Name) <> -1;
end;

function IsDirectoryAndNotReparsePoint(const Name: String): Boolean;
{ Returns True if the specified directory exists and is NOT a reparse point. }
const
  FILE_ATTRIBUTE_REPARSE_POINT = $00000400;
var
  Attr: DWORD;
begin
  Attr := GetFileAttributes(PChar(Name));
  Result := (Attr <> $FFFFFFFF) and
    (Attr and FILE_ATTRIBUTE_DIRECTORY <> 0) and
    (Attr and FILE_ATTRIBUTE_REPARSE_POINT = 0);
end;

function GetIniString(const Section, Key: String; Default: String;
  const Filename: String): String;
var
  BufSize, Len: Integer;
begin
  { On Windows 9x, Get*ProfileString can modify the lpDefault parameter, so
    make sure it's unique and not read-only }
  UniqueString(Default);
  BufSize := 256;
  while True do begin
    SetString(Result, nil, BufSize);
    if Filename <> '' then
      Len := GetPrivateProfileString(PChar(Section), PChar(Key), PChar(Default),
        @Result[1], BufSize, PChar(Filename))
    else
      Len := GetProfileString(PChar(Section), PChar(Key), PChar(Default),
        @Result[1], BufSize);
    { Work around bug present on Windows NT/2000 (not 95): When lpDefault is
      too long to fit in the buffer, nSize is returned (null terminator
      counted) instead of nSize-1 (what it's supposed to return). So don't
      trust the returned length; calculate it ourself.
      Note: This also ensures the string can never include embedded nulls. }
    if Len <> 0 then
      Len := StrLen(PChar(Result));
    { Break if the string fits, or if it's apparently 64 KB or longer.
      No point in increasing buffer size past 64 KB because the length
      returned by Windows 2000 seems to be mod 65536. And Windows 95 returns
      0 on values longer than ~32 KB.
      Note: The docs say the function returns "nSize minus one" if the buffer
      is too small, but I'm willing to bet it can be "minus two" if the last
      character is double-byte. Let's just be extremely paranoid and check for
      BufSize-8. }
    if (Len < BufSize-8) or (BufSize >= 65536) then begin
      SetLength(Result, Len);
      Break;
    end;
    { Otherwise double the buffer size and try again }
    BufSize := BufSize * 2;
  end;
end;

function GetIniInt(const Section, Key: String;
  const Default, Min, Max: Longint; const Filename: String): Longint;
{ Reads a Longint from an INI file. If the Longint read is not between Min/Max
  then it returns Default. If Min=Max then Min/Max are ignored }
var
  S: String;
  E: Integer;
begin
  S := GetIniString(Section, Key, '', Filename);
  if S = '' then
    Result := Default
  else begin
    Val(S, Result, E);
    if (E <> 0) or ((Min <> Max) and ((Result < Min) or (Result > Max))) then
      Result := Default;
  end;
end;

function GetIniBool(const Section, Key: String; const Default: Boolean;
  const Filename: String): Boolean;
begin
  Result := GetIniInt(Section, Key, Ord(Default), 0, 0, Filename) <> 0;
end;

function IniKeyExists(const Section, Key, Filename: String): Boolean;
  function Equals(const Default: PChar): Boolean;
  var
    Test: array[0..7] of Char;
  begin
    Test[0] := #0;
    if Filename <> '' then
      GetPrivateProfileString(PChar(Section), PChar(Key), Default,
        Test, SizeOf(Test) div SizeOf(Test[0]), PChar(Filename))
    else
      GetProfileString(PChar(Section), PChar(Key), Default,
        Test, SizeOf(Test) div SizeOf(Test[0]));
    Result := lstrcmp(Test, Default) = 0;
  end;
begin
  { If the key does not exist, a default string is returned both times. }
  Result := not Equals('x1234x') or not Equals('x5678x');  { <- don't change }
end;

function IsIniSectionEmpty(const Section, Filename: String): Boolean;
var
  Test: array[0..255] of Char;
begin
  Test[0] := #0;
  if Filename <> '' then
    GetPrivateProfileString(PChar(Section), nil, '', Test,
      SizeOf(Test) div SizeOf(Test[0]), PChar(Filename))
  else
    GetProfileString(PChar(Section), nil, '', Test,
      SizeOf(Test) div SizeOf(Test[0]));
  Result := Test[0] = #0;
end;

function SetIniString(const Section, Key, Value, Filename: String): Boolean;
begin
  if Filename <> '' then
    Result := WritePrivateProfileString(PChar(Section), PChar(Key),
      PChar(Value), PChar(Filename))
  else
    Result := WriteProfileString(PChar(Section), PChar(Key),
      PChar(Value));
end;

function SetIniInt(const Section, Key: String; const Value: Longint;
  const Filename: String): Boolean;
begin
  Result := SetIniString(Section, Key, IntToStr(Value), Filename);
end;

function SetIniBool(const Section, Key: String; const Value: Boolean;
  const Filename: String): Boolean;
begin
  Result := SetIniInt(Section, Key, Ord(Value), Filename);
end;

procedure DeleteIniEntry(const Section, Key, Filename: String);
begin
  if Filename <> '' then
    WritePrivateProfileString(PChar(Section), PChar(Key),
      nil, PChar(Filename))
  else
    WriteProfileString(PChar(Section), PChar(Key),
      nil);
end;

procedure DeleteIniSection(const Section, Filename: String);
begin
  if Filename <> '' then
    WritePrivateProfileString(PChar(Section), nil, nil,
      PChar(Filename))
  else
    WriteProfileString(PChar(Section), nil, nil);
end;

function GetEnv(const EnvVar: String): String;
{ Gets the value of the specified environment variable. (Just like TP's GetEnv) }
var
  Res: DWORD;
begin
  SetLength(Result, 255);
  repeat
    Res := GetEnvironmentVariable(PChar(EnvVar), PChar(Result), Length(Result));
    if Res = 0 then begin
      Result := '';
      Break;
    end;
  until AdjustLength(Result, Res);
end;

function GetParamStr(const P: PChar; var Param: String): PChar;

  function Extract(P: PChar; const Buffer: PChar; var Len: Integer): PChar;
  var
    InQuote: Boolean;
  begin
    Len := 0;
    InQuote := False;
    while (P^ <> #0) and ((P^ > ' ') or InQuote) do begin
      if P^ = '"' then
        InQuote := not InQuote
      else begin
        if Assigned(Buffer) then
          Buffer[Len] := P^;
        Inc(Len);
      end;
      Inc(P);
    end;
    Result := P;
  end;

var
  Len: Integer;
  Buffer: String;
begin
  Extract(P, nil, Len);
  SetString(Buffer, nil, Len);
  Result := Extract(P, @Buffer[1], Len);
  Param := Buffer;
  while (Result^ <> #0) and (Result^ <= ' ') do
    Inc(Result);
end;

function GetCmdTail: String;
{ Returns all command line parameters passed to the process as a single
  string. }
var
  S: String;
begin
  Result := GetParamStr(GetCommandLine, S);
end;

function GetCmdTailEx(StartIndex: Integer): String;
{ Returns all command line parameters passed to the process as a single
  string, starting with StartIndex (one-based). }
var
  P: PChar;
  S: String;
begin
  P := GetParamStr(GetCommandLine, S);
  while (StartIndex > 1) and (P^ <> #0) do begin
    P := GetParamStr(P, S);
    Dec(StartIndex);
  end;
  Result := P;
end;

function NewParamCount: Integer;
var
  P: PChar;
  S: String;
begin
  P := GetParamStr(GetCommandLine, S);
  Result := 0;
  while P^ <> #0 do begin
    Inc(Result);
    P := GetParamStr(P, S);
  end;
end;

function NewParamStr(Index: Integer): string;
{ Returns the Indexth command line parameter, or an empty string if Index is
  out of range.
  Differences from Delphi's ParamStr:
  - No limits on parameter length
  - Doesn't ignore empty parameters ("")
  - Handles the empty argv[0] case like MSVC: if GetCommandLine() returns
    " a b" then NewParamStr(1) should return "a", not "b" }
var
  Buffer: array[0..MAX_PATH-1] of Char;
  S: String;
  P: PChar;
begin
  if Index = 0 then begin
    SetString(Result, Buffer, GetModuleFileName(0, Buffer, SizeOf(Buffer) div SizeOf(Buffer[0])));
  end
  else begin
    P := GetCommandLine;
    while True do begin
      if P^ = #0 then begin
        S := '';
        Break;
      end;
      P := GetParamStr(P, S);
      if Index = 0 then Break;
      Dec(Index);
    end;
    Result := S;
  end;
end;

function AddQuotes(const S: String): String;
{ Adds a quote (") character to the left and right sides of the string if
  the string contains a space and it didn't have quotes already. This is
  primarily used when spawning another process with a long filename as one of
  the parameters. }
begin
  Result := Trim(S);
  if (PathPos(' ', Result) <> 0) and
     ((Result[1] <> '"') or (PathLastChar(Result)^ <> '"')) then
    Result := '"' + Result + '"';
end;

function RemoveQuotes(const S: String): String;
{ Opposite of AddQuotes; removes any quotes around the string. }
begin
  Result := S;
  while (Result <> '') and (Result[1] = '"') do
    Delete(Result, 1, 1);
  while (Result <> '') and (PathLastChar(Result)^ = '"') do
    SetLength(Result, Length(Result)-1);
end;

function ConvertPercentStr(var S: String): Boolean;
{ Expands all %-encoded characters in the string (see RFC 2396). Returns True
  if all were successfully expanded. }
var
  I, C, E: Integer;
  N: String;
begin
  Result := True;
  I := 1;
  while I <= Length(S) do begin
    if S[I] = '%' then begin
      N := Copy(S, I, 3);
      if Length(N) <> 3 then begin
        Result := False;
        Break;
      end;
      N[1] := '$';
      Val(N, C, E);
      if E <> 0 then begin
        Result := False;
        Break;
      end;
      { delete the two numbers following '%', and replace '%' with the character }
      Delete(S, I+1, 2);
      S[I] := Chr(C);
    end;
    Inc(I);
  end;
end;

function SkipPastConst(const S: String; const Start: Integer): Integer;
{ Returns the character index following the Inno Setup constant embedded
  into the string S at index Start.
  If the constant is not closed (missing a closing brace), it returns zero. }
var
  L, BraceLevel, LastOpenBrace: Integer;
begin
  Result := Start;
  L := Length(S);
  if Result < L then begin
    Inc(Result);
    if S[Result] = '{' then begin
      Inc(Result);
      Exit;
    end
    else begin
      BraceLevel := 1;
      LastOpenBrace := -1;
      while Result <= L do begin
        case S[Result] of
          '{': begin
                   if LastOpenBrace <> Result-1 then begin
                     Inc(BraceLevel);
                     LastOpenBrace := Result;
                   end
                   else
                     { Skip over '{{' when in an embedded constant }
                     Dec(BraceLevel);
                 end;
          '}': begin
                 Dec(BraceLevel);
                 if BraceLevel = 0 then begin
                   Inc(Result);
                   Exit;
                 end;
               end;
        else
          {$IFNDEF UNICODE}
          if S[Result] in ConstLeadBytes^ then
            Inc(Result);
          {$ENDIF}
        end;
        Inc(Result);
      end;
    end;
  end;
  Result := 0;
end;

function ConvertConstPercentStr(var S: String): Boolean;
{ Same as ConvertPercentStr, but is designed to ignore embedded Inno Setup
  constants. Any '%' characters between braces are not translated. Two
  consecutive braces are ignored. }
var
  I, C, E: Integer;
  N: String;
begin
  Result := True;
  I := 1;
  while I <= Length(S) do begin
    case S[I] of
      '{': begin
             I := SkipPastConst(S, I);
             if I = 0 then begin
               Result := False;
               Break;
             end;
             Dec(I);  { ...since there's an Inc below }
           end;
      '%': begin
             N := Copy(S, I, 3);
             if Length(N) <> 3 then begin
               Result := False;
               Break;
             end;
             N[1] := '$';
             Val(N, C, E);
             if E <> 0 then begin
               Result := False;
               Break;
             end;
             { delete the two numbers following '%', and replace '%' with the character }
             Delete(S, I+1, 2);
             S[I] := Chr(C);
           end;
    else
      {$IFNDEF UNICODE}
      if S[I] in ConstLeadBytes^ then
        Inc(I);
      {$ENDIF}
    end;
    Inc(I);
  end;
end;

function ConstPos(const Ch: Char; const S: String): Integer;
{ Like the standard Pos function, but skips over any Inno Setup constants
  embedded in S }
var
  I, L: Integer;
begin
  Result := 0;
  I := 1;
  L := Length(S);
  while I <= L do begin
    if S[I] = Ch then begin
      Result := I;
      Break;
    end
    else if S[I] = '{' then begin
      I := SkipPastConst(S, I);
      if I = 0 then
        Break;
    end
    else begin
      {$IFNDEF UNICODE}
      if S[I] in ConstLeadBytes^ then
        Inc(I);
      {$ENDIF}
      Inc(I);
    end;
  end;
end;

function GetShortName(const LongName: String): String;
{ Gets the short version of the specified long filename. If the file does not
  exist, or some other error occurs, it returns LongName. }
var
  Res: DWORD;
begin
  SetLength(Result, MAX_PATH);
  repeat
    Res := GetShortPathName(PChar(LongName), PChar(Result), Length(Result));
    if Res = 0 then begin
      Result := LongName;
      Break;
    end;
  until AdjustLength(Result, Res);
end;

function GetWinDir: String;
{ Returns fully qualified path of the Windows directory. Only includes a
  trailing backslash if the Windows directory is the root directory. }
var
  Buf: array[0..MAX_PATH-1] of Char;
begin
  GetWindowsDirectory(Buf, SizeOf(Buf) div SizeOf(Buf[0]));
  Result := StrPas(Buf);
end;

function GetSystemDir: String;
{ Returns fully qualified path of the Windows System directory. Only includes a
  trailing backslash if the Windows System directory is the root directory. }
var
  Buf: array[0..MAX_PATH-1] of Char;
begin
  GetSystemDirectory(Buf, SizeOf(Buf) div SizeOf(Buf[0]));
  Result := StrPas(Buf);
end;

function GetSysWow64Dir: String;
{ Returns fully qualified path of the SysWow64 directory on 64-bit Windows.
  Returns '' if there is no SysWow64 directory (e.g. running 32-bit Windows). }
var
  GetSystemWow64DirectoryFunc: function(
    lpBuffer: {$IFDEF UNICODE} PWideChar {$ELSE} PAnsiChar {$ENDIF};
    uSize: UINT): UINT; stdcall;
  Res: Integer;
  Buf: array[0..MAX_PATH] of Char;
begin
  Result := '';
  GetSystemWow64DirectoryFunc := GetProcAddress(GetModuleHandle(kernel32),
    {$IFDEF UNICODE}
      'GetSystemWow64DirectoryW'
    {$ELSE}
      'GetSystemWow64DirectoryA'
    {$ENDIF} );
  { Note: This function does exist on 32-bit XP, but always returns 0 }
  if Assigned(GetSystemWow64DirectoryFunc) then begin
    Res := GetSystemWow64DirectoryFunc(Buf, SizeOf(Buf) div SizeOf(Buf[0]));
    if (Res > 0) and (Res < SizeOf(Buf) div SizeOf(Buf[0])) then
      Result := Buf;
  end;
end;

function GetSysNativeDir(const IsWin64: Boolean): String;
{ Returns the special Sysnative alias, without trailing backslash.
  Returns '' if there is no Sysnative alias. }
begin
  { From MSDN: 32-bit applications can access the native system directory by
    substituting %windir%\Sysnative for %windir%\System32. WOW64 recognizes
    Sysnative as a special alias used to indicate that the file system should
    not redirect the access. The Sysnative alias was added starting
    with Windows Vista. }
  if IsWin64 and (Lo(GetVersion) >= 6) then
    { Note: Avoiding GetWinDir here as that might not return the real Windows
      directory under Terminal Services }
    Result := PathExpand(AddBackslash(GetSystemDir) + '..\Sysnative') { Do not localize }
  else
    Result := '';
end;

function GetTempDir: String;
{ Returns fully qualified path of the temporary directory, with trailing
  backslash. This does not use the Win32 function GetTempPath, due to platform
  differences. }
label 1;
begin
  Result := GetEnv('TMP');
  if (Result <> '') and DirExists(Result) then
    goto 1;
  Result := GetEnv('TEMP');
  if (Result <> '') and DirExists(Result) then
    goto 1;
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    { Like Windows 2000's GetTempPath, return USERPROFILE when TMP and TEMP
      are not set }
    Result := GetEnv('USERPROFILE');
    if (Result <> '') and DirExists(Result) then
      goto 1;
  end;
  Result := GetWinDir;
1:Result := AddBackslash(PathExpand(Result));
end;

function StringChangeEx(var S: String; const FromStr, ToStr: String;
  const SupportDBCS: Boolean): Integer;
{ Changes all occurrences in S of FromStr to ToStr. If SupportDBCS is True
  (recommended), double-byte character sequences in S are recognized and
  handled properly. Otherwise, the function behaves in a binary-safe manner.
  Returns the number of times FromStr was matched and changed. }
var
  FromStrLen, I, EndPos, J: Integer;
  IsMatch: Boolean;
label 1;
begin
  Result := 0;
  if FromStr = '' then Exit;
  FromStrLen := Length(FromStr);
  I := 1;
1:EndPos := Length(S) - FromStrLen + 1;
  while I <= EndPos do begin
    IsMatch := True;
    J := 0;
    while J < FromStrLen do begin
      if S[J+I] <> FromStr[J+1] then begin
        IsMatch := False;
        Break;
      end;
      Inc(J);
    end;
    if IsMatch then begin
      Inc(Result);
      Delete(S, I, FromStrLen);
      Insert(ToStr, S, I);
      Inc(I, Length(ToStr));
      goto 1;
    end;
    if SupportDBCS then
      Inc(I, PathCharLength(S, I))
    else
      Inc(I);
  end;
end;

function StringChange(var S: String; const FromStr, ToStr: String): Integer;
{ Same as calling StringChangeEx with SupportDBCS=False }
begin
  Result := StringChangeEx(S, FromStr, ToStr, False);
end;

function AdjustLength(var S: String; const Res: Cardinal): Boolean;
{ Returns True if successful. Returns False if buffer wasn't large enough,
  and called AdjustLength to resize it. }
begin
  Result := Integer(Res) < Length(S);
  SetLength(S, Res);
end;

function UsingWinNT: Boolean;
{ Returns True if system is running any version of Windows NT. Never returns
  True on Windows 95 or 3.1. }
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
end;

function InternalRegQueryStringValue(H: HKEY; Name: PChar; var ResultStr: String;
  Type1, Type2: DWORD): Boolean;
var
  Typ, Size: DWORD;
  Len: Integer;
  S: String;
  ErrorCode: Longint;
label 1;
begin
  Result := False;
1:Size := 0;
  if (RegQueryValueEx(H, Name, nil, @Typ, nil, @Size) = ERROR_SUCCESS) and
     ((Typ = Type1) or (Typ = Type2)) then begin
    if Size = 0 then begin
      { It's an empty string with no null terminator.
        (Must handle those here since we can't pass a nil lpData pointer on
        the second RegQueryValueEx call.) }
      ResultStr := '';
      Result := True;
    end
    else begin
      { Paranoia: Impose reasonable upper limit on Size to avoid potential
        integer overflows below }
      if Cardinal(Size) >= Cardinal($70000000) then
        OutOfMemoryError;
      { Note: If Size isn't a multiple of SizeOf(S[1]), we have to round up
        here so that RegQueryValueEx doesn't overflow the buffer }
      Len := (Size + (SizeOf(S[1]) - 1)) div SizeOf(S[1]);
      SetString(S, nil, Len);
      ErrorCode := RegQueryValueEx(H, Name, nil, @Typ, @S[1], @Size);
      if ErrorCode = ERROR_MORE_DATA then begin
        { The data must've increased in size since the first RegQueryValueEx
          call. Start over. }
        goto 1;
      end;
      if (ErrorCode = ERROR_SUCCESS) and
         ((Typ = Type1) or (Typ = Type2)) then begin
        { If Size isn't a multiple of SizeOf(S[1]), we disregard the partial
          character, like RegGetValue }
        Len := Size div SizeOf(S[1]);
        { Remove any null terminators from the end and trim the string to the
          returned length.
          Note: We *should* find 1 null terminator, but it's possible for
          there to be more or none if the value was written that way. }
        while (Len <> 0) and (S[Len] = #0) do
          Dec(Len);
        { In a REG_MULTI_SZ value, each individual string is null-terminated,
          so add 1 null (back) to the end, unless there are no strings (Len=0) }
        if (Typ = REG_MULTI_SZ) and (Len <> 0) then
          Inc(Len);
        SetLength(S, Len);
        if (Typ = REG_MULTI_SZ) and (Len <> 0) then
          S[Len] := #0;
        ResultStr := S;
        Result := True;
      end;
    end;
  end;
end;

function RegQueryStringValue(H: HKEY; Name: PChar; var ResultStr: String): Boolean;
{ Queries the specified REG_SZ or REG_EXPAND_SZ registry key/value, and returns
  the value in ResultStr. Returns True if successful. When False is returned,
  ResultStr is unmodified. }
begin
  Result := InternalRegQueryStringValue(H, Name, ResultStr, REG_SZ,
    REG_EXPAND_SZ);
end;

function RegQueryMultiStringValue(H: HKEY; Name: PChar; var ResultStr: String): Boolean;
{ Queries the specified REG_MULTI_SZ registry key/value, and returns the value
  in ResultStr. Returns True if successful. When False is returned, ResultStr
  is unmodified. }
begin
  Result := InternalRegQueryStringValue(H, Name, ResultStr, REG_MULTI_SZ,
    REG_MULTI_SZ);
end;

function RegValueExists(H: HKEY; Name: PChar): Boolean;
{ Returns True if the specified value exists. Requires KEY_QUERY_VALUE access
  to the key. }
var
  I: Integer;
  EnumName: array[0..1] of Char;
  Count: DWORD;
  ErrorCode: Longint;
begin
  Result := RegQueryValueEx(H, Name, nil, nil, nil, nil) = ERROR_SUCCESS;
  if Result and ((Name = nil) or (Name^ = #0)) and
     (Win32Platform <> VER_PLATFORM_WIN32_NT) then begin
    { On Win9x/Me a default value always exists according to RegQueryValueEx,
      so it must use RegEnumValue instead to check if a default value
      really exists }
    Result := False;
    I := 0;
    while True do begin
      Count := SizeOf(EnumName) div SizeOf(EnumName[0]);
      ErrorCode := RegEnumValue(H, I, EnumName, Count, nil, nil, nil, nil);
      if (ErrorCode <> ERROR_SUCCESS) and (ErrorCode <> ERROR_MORE_DATA) then
        Break;
      { is it the default value? }
      if (ErrorCode = ERROR_SUCCESS) and (EnumName[0] = #0) then begin
        Result := True;
        Break;
      end;
      Inc(I);
    end;
  end;
end;

function RegCreateKeyExView(const RegView: TRegView; hKey: HKEY; lpSubKey: PChar;
  Reserved: DWORD; lpClass: PChar; dwOptions: DWORD; samDesired: REGSAM;
  lpSecurityAttributes: PSecurityAttributes; var phkResult: HKEY;
  lpdwDisposition: PDWORD): Longint;
begin
  if RegView = rv64Bit then
    samDesired := samDesired or KEY_WOW64_64KEY;
  Result := RegCreateKeyEx(hKey, lpSubKey, Reserved, lpClass, dwOptions,
    samDesired, lpSecurityAttributes, phkResult, lpdwDisposition);
end;

function RegOpenKeyExView(const RegView: TRegView; hKey: HKEY; lpSubKey: PChar;
  ulOptions: DWORD; samDesired: REGSAM; var phkResult: HKEY): Longint;
begin
  if RegView = rv64Bit then
    samDesired := samDesired or KEY_WOW64_64KEY;
  Result := RegOpenKeyEx(hKey, lpSubKey, ulOptions, samDesired, phkResult);
end;

var
  RegDeleteKeyExFunc: function(hKey: HKEY;
    lpSubKey: {$IFDEF UNICODE} PWideChar {$ELSE} PAnsiChar {$ENDIF};
    samDesired: REGSAM; Reserved: DWORD): Longint; stdcall;

function RegDeleteKeyView(const RegView: TRegView; const Key: HKEY;
  const Name: PChar): Longint;
begin
  if RegView <> rv64Bit then
    Result := RegDeleteKey(Key, Name)
  else begin
    if @RegDeleteKeyExFunc = nil then
      RegDeleteKeyExFunc := GetProcAddress(GetModuleHandle(advapi32),
        {$IFDEF UNICODE}
          'RegDeleteKeyExW'
        {$ELSE}
          'RegDeleteKeyExA'
        {$ENDIF} );
    if Assigned(RegDeleteKeyExFunc) then
      Result := RegDeleteKeyExFunc(Key, Name, KEY_WOW64_64KEY, 0)
    else
      Result := ERROR_PROC_NOT_FOUND;
  end;
end;

function RegDeleteKeyIncludingSubkeys(const RegView: TRegView; const Key: HKEY;
  const Name: PChar): Longint;
{ Deletes the specified key and all subkeys.
  Returns ERROR_SUCCESS if the key was successful deleted. }
var
  H: HKEY;
  KeyName: String;
  I, KeyNameCount: DWORD;
  ErrorCode: Longint;
begin
  if (Name = nil) or (Name[0] = #0) then begin
    Result := ERROR_INVALID_PARAMETER;
    Exit;
  end;
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    if RegOpenKeyExView(RegView, Key, Name, 0, KEY_ENUMERATE_SUB_KEYS, H) = ERROR_SUCCESS then begin
      try
        SetString(KeyName, nil, 256);
        I := 0;
        while True do begin
          KeyNameCount := Length(KeyName);
          ErrorCode := RegEnumKeyEx(H, I, @KeyName[1], KeyNameCount, nil, nil, nil, nil);
          if ErrorCode = ERROR_MORE_DATA then begin
            { Double the size of the buffer and try again }
            if Length(KeyName) >= 65536 then begin
              { Sanity check: If we tried a 64 KB buffer and it's still saying
                there's more data, something must be seriously wrong. Bail. }
              Break;
            end;
            SetString(KeyName, nil, Length(KeyName) * 2);
            Continue;
          end;
          if ErrorCode <> ERROR_SUCCESS then
            Break;
          if RegDeleteKeyIncludingSubkeys(RegView, H, PChar(KeyName)) <> ERROR_SUCCESS then
            Inc(I);
        end;
      finally
        RegCloseKey(H);
      end;
    end;
  end;
  Result := RegDeleteKeyView(RegView, Key, Name);
end;

function RegDeleteKeyIfEmpty(const RegView: TRegView; const RootKey: HKEY;
  const SubkeyName: PChar): Longint;
{ Deletes the specified subkey if it has no subkeys or values.
  Returns ERROR_SUCCESS if the key was successful deleted, ERROR_DIR_NOT_EMPTY
  if it was not deleted because it contained subkeys or values, or possibly
  some other Win32 error code. }
var
  K: HKEY;
  NumSubkeys, NumValues: DWORD;
begin
  Result := RegOpenKeyExView(RegView, RootKey, SubkeyName, 0, KEY_QUERY_VALUE, K);
  if Result <> ERROR_SUCCESS then
    Exit;
  Result := RegQueryInfoKey(K, nil, nil, nil, @NumSubkeys, nil, nil,
    @NumValues, nil, nil, nil, nil);
  RegCloseKey(K);
  if Result <> ERROR_SUCCESS then
    Exit;
  if (NumSubkeys = 0) and (NumValues = 0) then
    Result := RegDeleteKeyView(RegView, RootKey, SubkeyName)
  else
    Result := ERROR_DIR_NOT_EMPTY;
end;

function GetShellFolderPath(const FolderID: Integer): String;
var
  pidl: PItemIDList;
  Buffer: array[0..MAX_PATH-1] of Char;
  Malloc: IMalloc;
begin
  Result := '';
  if FAILED(SHGetMalloc(Malloc)) then
    Malloc := nil;
  if SUCCEEDED(SHGetSpecialFolderLocation(0, FolderID, pidl)) then begin
    if SHGetPathFromIDList(pidl, Buffer) then
      Result := Buffer;
    if Assigned(Malloc) then
      Malloc.Free(pidl);
  end;
end;

function IsMemberOfGroup(const DomainAliasRid: DWORD): Boolean;
{ Returns True if the logged-on user is a member of the specified local
  group. Always returns True on Windows 9x/Me. }
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  SE_GROUP_ENABLED           = $00000004;
  SE_GROUP_USE_FOR_DENY_ONLY = $00000010;
var
  Sid: PSID;
  CheckTokenMembership: function(TokenHandle: THandle; SidToCheck: PSID;
    var IsMember: BOOL): BOOL; stdcall;
  IsMember: BOOL;
  Token: THandle;
  GroupInfoSize: DWORD;
  GroupInfo: PTokenGroups;
  I: Integer;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then begin
    Result := True;
    Exit;
  end;

  Result := False;

  if not AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
     SECURITY_BUILTIN_DOMAIN_RID, DomainAliasRid,
     0, 0, 0, 0, 0, 0, Sid) then
    Exit;
  try
    { Use CheckTokenMembership if available. MSDN states:
      "The CheckTokenMembership function should be used with Windows 2000 and
      later to determine whether a specified SID is present and enabled in an
      access token. This function eliminates potential misinterpretations of
      the active group membership if changes to access tokens are made in
      future releases." }
    CheckTokenMembership := nil;
    if Lo(GetVersion) >= 5 then
      CheckTokenMembership := GetProcAddress(GetModuleHandle(advapi32),
        'CheckTokenMembership');
    if Assigned(CheckTokenMembership) then begin
      if CheckTokenMembership(0, Sid, IsMember) then
        Result := IsMember;
    end
    else begin
      GroupInfo := nil;
      if not OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True,
         {$IFDEF Delphi3orHigher} Token {$ELSE} @Token {$ENDIF}) then begin
        if GetLastError <> ERROR_NO_TOKEN then
          Exit;
        if not OpenProcessToken(GetCurrentProcess, TOKEN_QUERY,
           {$IFDEF Delphi3orHigher} Token {$ELSE} @Token {$ENDIF}) then
          Exit;
      end;
      try
        GroupInfoSize := 0;
        if not GetTokenInformation(Token, TokenGroups, nil, 0, GroupInfoSize) and
           (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
          Exit;

        GetMem(GroupInfo, GroupInfoSize);
        if not GetTokenInformation(Token, TokenGroups, GroupInfo,
           GroupInfoSize, GroupInfoSize) then
          Exit;

        for I := 0 to GroupInfo.GroupCount-1 do begin
          if EqualSid(Sid, GroupInfo.Groups[I].Sid) and
             (GroupInfo.Groups[I].Attributes and (SE_GROUP_ENABLED or
              SE_GROUP_USE_FOR_DENY_ONLY) = SE_GROUP_ENABLED) then begin
            Result := True;
            Break;
          end;
        end;
      finally
        FreeMem(GroupInfo);
        CloseHandle(Token);
      end;
    end;
  finally
    FreeSid(Sid);
  end;
end;

function IsAdminLoggedOn: Boolean;
{ Returns True if the logged-on user is a member of the Administrators local
  group. Always returns True on Windows 9x/Me. }
const
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
begin
  Result := IsMemberOfGroup(DOMAIN_ALIAS_RID_ADMINS);
end;

function IsPowerUserLoggedOn: Boolean;
{ Returns True if the logged-on user is a member of the Power Users local
  group. Always returns True on Windows 9x/Me. }
const
  DOMAIN_ALIAS_RID_POWER_USERS = $00000223;
begin
  Result := IsMemberOfGroup(DOMAIN_ALIAS_RID_POWER_USERS);
end;

function IsMultiByteString(const S: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(S) do
    if IsDBCSLeadByte(Ord(S[I])) then begin
      Result := True;
      Break;
    end;
end;

function FontExistsCallback(const lplf: TLogFont; const lptm: TTextMetric;
  dwType: DWORD; lpData: LPARAM): Integer; stdcall;
begin
  Boolean(Pointer(lpData)^) := True;
  Result := 1;
end;

function FontExists(const FaceName: String): Boolean;
var
  DC: HDC;
begin
  Result := False;
  DC := GetDC(0);
  try
    EnumFonts(DC, PChar(FaceName), @FontExistsCallback, @Result);
  finally
    ReleaseDC(0, DC);
  end;
end;

{$IFNDEF IS_D5}
procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;
{$ENDIF}

{$IFNDEF IS_D5}
function SafeLoadLibrary(const Filename: String; ErrorMode: UINT): HMODULE;
var
  SaveErrorMode: UINT;
  SaveFPUControlWord: Word;
begin
  SaveErrorMode := SetErrorMode(ErrorMode);
  try
    asm
      FNSTCW SaveFPUControlWord
    end;
    try
      Result := LoadLibrary(PChar(Filename));
    finally
      asm
        FNCLEX
        FLDCW SaveFPUControlWord
      end;
    end;
  finally
    SetErrorMode(SaveErrorMode);
  end;
end;
{$ENDIF}

function GetUILanguage: LANGID;
{ Platform-independent version of GetUserDefaultUILanguage. May return 0 in
  case of failure. }
var
  GetUserDefaultUILanguage: function: LANGID; stdcall;
  K: HKEY;
  S: String;
  E: Integer;
begin
  GetUserDefaultUILanguage := GetProcAddress(GetModuleHandle(kernel32),
    'GetUserDefaultUILanguage');
  if Assigned(GetUserDefaultUILanguage) then
    { This function is available on Windows 2000, Me, and later }
    Result := GetUserDefaultUILanguage
  else begin
    if Win32Platform = VER_PLATFORM_WIN32_NT then begin
      { Windows NT 4.0 }
      if RegOpenKeyExView(rvDefault, HKEY_USERS, '.DEFAULT\Control Panel\International',
         0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        RegQueryStringValue(K, 'Locale', S);
        RegCloseKey(K);
      end;
    end
    else begin
      { Windows 95/98 }
      if RegOpenKeyExView(rvDefault, HKEY_CURRENT_USER, 'Control Panel\Desktop\ResourceLocale',
         0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        RegQueryStringValue(K, '', S);
        RegCloseKey(K);
      end;
    end;
    Val('$' + S, Result, E);
    if E <> 0 then
      Result := 0;
  end;
end;

function RemoveAccelChar(const S: String): String;
var
  I: Integer;
begin
  Result := S;
  I := 1;
  while I <= Length(Result) do begin
    if Result[I] = '&' then begin
      System.Delete(Result, I, 1);
      if I > Length(Result) then
        Break;
    end;
    Inc(I, PathCharLength(Result, I));
  end;
end;

function GetTextWidth(const DC: HDC; S: String; const Prefix: Boolean): Integer;
{ Returns the width of the specified string using the font currently selected
  into DC. If Prefix is True, it first removes "&" characters as necessary. }
var
  Size: TSize;
begin
  { This procedure is 10x faster than using DrawText with the DT_CALCRECT flag }
  if Prefix then
    S := RemoveAccelChar(S);
  GetTextExtentPoint32(DC, PChar(S), Length(S), Size);
  Result := Size.cx;
end;

function AddPeriod(const S: String): String;
begin
  Result := S;
  if (Result <> '') and (PathLastChar(Result)^ > '.') then
    Result := Result + '.';
end;

function GetExceptMessage: String;
var
  E: TObject;
begin
  E := ExceptObject;
  if E = nil then
    Result := '[ExceptObject=nil]'   { should never get here }
  else if E is Exception then
    Result := AddPeriod(Exception(E).Message)   { usual case }
  else
    Result := E.ClassName;   { shouldn't get here under normal circumstances } 
end;

function GetPreferredUIFont: String;
{ Gets the preferred UI font. Returns Microsoft Sans Serif, or MS Sans Serif
  if it doesn't exist.
  Microsoft Sans Serif (which is available on Windows 2000 and later) has two
  advantages over MS Sans Serif:
  1) On Windows XP, it can display password dots in edit boxes.
  2) In my tests on Japanese XP, Microsoft Sans Serif can display Japanese
     characters (MS Sans Serif cannot). }
begin
  if FontExists('Microsoft Sans Serif') then
    Result := 'Microsoft Sans Serif'
  else
    Result := 'MS Sans Serif';
end;

function IsWildcard(const Pattern: String): Boolean;
begin
  Result := (Pos('*', Pattern) <> 0) or (Pos('?', Pattern) <> 0);
end;

function WildcardMatch(const Text, Pattern: PChar): Boolean;
{ General-purpose wildcard matching function based on the widely used wildcat()
  code by Rich $alz. In this implementation, however, the only supported
  pattern matching characters are ? and *.
  Note that this function uses Unix shell semantics -- e.g. a dot always
  matches a dot (so a pattern of '*.*' won't match 'file'), and ? always
  matches exactly 1 character (so '?????' won't match 'file').
  Also note: The InternalWildcardMatch function can recursively call itself
  for each non-consecutive * character in the pattern. With enough *
  characters, the stack could overflow. So ideally the caller should impose a
  limit on either the length of the pattern string or the number of *
  characters in it. }
type
  TWildcardMatchResult = (wmFalse, wmTrue, wmAbort);

  function InternalWildcardMatch(T, P: PChar): TWildcardMatchResult;
  begin
    while P^ <> #0 do begin
      if (T^ = #0) and (P^ <> '*') then begin
        Result := wmAbort;
        Exit;
      end;
      case P^ of
        '?': ;  { Match any character }
        '*': begin
               Inc(P);
               while P^ = '*' do begin
                 { Consecutive stars act just like one }
                 Inc(P);
               end;
               if P^ = #0 then begin
                 { Trailing star matches everything }
                 Result := wmTrue;
                 Exit;
               end;
               while T^ <> #0 do begin
                 Result := InternalWildcardMatch(T, P);
                 if Result <> wmFalse then
                   Exit;
                 T := PathStrNextChar(T);
               end;
               Result := wmAbort;
               Exit;
             end;
      else
        if not PathCharCompare(T, P) then begin
          Result := wmFalse;
          Exit;
        end;
      end;
      T := PathStrNextChar(T);
      P := PathStrNextChar(P);
    end;
    if T^ = #0 then
      Result := wmTrue
    else
      Result := wmFalse;
  end;

begin
  Result := (InternalWildcardMatch(Text, Pattern) = wmTrue);
end;

function IntMax(const A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Win32ErrorString(ErrorCode: Integer): String;
{ Like SysErrorMessage but also passes the FORMAT_MESSAGE_IGNORE_INSERTS flag
  which allows the function to succeed on errors like 129 }
var
  Len: Integer;
  Buffer: array[0..1023] of Char;
begin
  Len := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_ARGUMENT_ARRAY, nil,
    ErrorCode, 0, Buffer, SizeOf(Buffer) div SizeOf(Buffer[0]), nil);
  while (Len > 0) and ((Buffer[Len-1] <= ' ') or (Buffer[Len-1] = '.')) do
    Dec(Len);
  SetString(Result, Buffer, Len);
end;

{$IFNDEF UNICODE}
procedure GetLeadBytes(var ALeadBytes: TLeadByteSet);
var
  AnsiCPInfo: TCPInfo;
  I: Integer;
  J: Byte;
begin
  ALeadBytes := [];
  if GetCPInfo(CP_ACP, AnsiCPInfo) then
    with AnsiCPInfo do begin
      I := 0;
      while (I < MAX_LEADBYTES) and ((LeadByte[I] or LeadByte[I+1]) <> 0) do begin
        for J := LeadByte[I] to LeadByte[I+1] do
          Include(ALeadBytes, AnsiChar(J));
        Inc(I, 2);
      end;
    end;
end;
{$ENDIF}

{$IFNDEF IS_D3}
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,1
        SHR     ECX,1
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;
{$ENDIF}

function DeleteDirTree(const Dir: String): Boolean;
{ Removes the specified directory including any files/subdirectories inside
  it. Returns True if successful. }
var
  H: THandle;
  FindData: TWin32FindData;
  FN: String;
begin
  if (Dir <> '') and (Pos(#0, Dir) = 0) and  { sanity/safety checks }
     IsDirectoryAndNotReparsePoint(Dir) then begin
    H := FindFirstFile(PChar(AddBackslash(Dir) + '*'), FindData);
    if H <> INVALID_HANDLE_VALUE then begin
      try
        repeat
          if (StrComp(FindData.cFileName, '.') <> 0) and
             (StrComp(FindData.cFileName, '..') <> 0) then begin
            FN := AddBackslash(Dir) + FindData.cFileName;
            if FindData.dwFileAttributes and FILE_ATTRIBUTE_READONLY <> 0 then
              SetFileAttributes(PChar(FN), FindData.dwFileAttributes and not FILE_ATTRIBUTE_READONLY);
            if FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then
              Windows.DeleteFile(PChar(FN))
            else
              DeleteDirTree(FN);
          end;
        until not FindNextFile(H, FindData);
      finally
        Windows.FindClose(H);
      end;
    end;
  end;
  Result := RemoveDirectory(PChar(Dir));
end;

function SetNTFSCompression(const FileOrDir: String; Compress: Boolean): Boolean;
{ Changes the NTFS compression state of a file or directory. If False is
  returned, GetLastError can be called to get extended error information. }
const
  COMPRESSION_FORMAT_NONE = 0;
  COMPRESSION_FORMAT_DEFAULT = 1;
  FSCTL_SET_COMPRESSION = $9C040;
  Compressions: array[Boolean] of Word = (COMPRESSION_FORMAT_NONE, COMPRESSION_FORMAT_DEFAULT);
var
  Handle: THandle;
  BytesReturned, LastError: DWORD;
begin
  Handle := CreateFile(PChar(FileOrDir), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if Handle <> INVALID_HANDLE_VALUE then begin
    Result := DeviceIoControl(Handle, FSCTL_SET_COMPRESSION, @Compressions[Compress],
      SizeOf(Compressions[Compress]), nil, 0, BytesReturned, nil);
    { Save the error code from DeviceIoControl as CloseHandle may overwrite it
      (Windows 95's CloseHandle always sets it to zero) }
    LastError := GetLastError;
    CloseHandle(Handle);
    SetLastError(LastError);
  end else
    Result := False;
end;

var
  ChangeWindowMessageFilterInited: BOOL;
  ChangeWindowMessageFilterFunc: function(msg: UINT; dwFlag: DWORD): BOOL; stdcall;
  ChangeWindowMessageFilterExInited: BOOL;
  ChangeWindowMessageFilterExFunc: function(hWnd: HWND; msg: UINT;
    action: DWORD; pChangeFilterStruct: Pointer): BOOL; stdcall;

procedure AddToWindowMessageFilter(const Msg: UINT);
{ Adds a single message number to the process-wide message filter on Windows
  Vista and later. Has no effect on prior Windows versions. }
const
  MSGFLT_ADD = 1;
begin
  if not ChangeWindowMessageFilterInited then begin
    ChangeWindowMessageFilterFunc := GetProcAddress(GetModuleHandle(user32),
      'ChangeWindowMessageFilter');
    InterlockedExchange(Integer(ChangeWindowMessageFilterInited), Ord(True));
  end;
  if Assigned(ChangeWindowMessageFilterFunc) then
    ChangeWindowMessageFilterFunc(Msg, MSGFLT_ADD);
end;

procedure AddToWindowMessageFilterEx(const Wnd: HWND; const Msg: UINT);
{ Adds a single message number to Wnd's window-specific message filter, which
  is supported on Windows 7 and later. On Windows Vista, it falls back to
  modifying the process-wide message filter. }
const
  MSGFLT_ALLOW = 1;
begin
  if not ChangeWindowMessageFilterExInited then begin
    ChangeWindowMessageFilterExFunc := GetProcAddress(GetModuleHandle(user32),
      'ChangeWindowMessageFilterEx');
    InterlockedExchange(Integer(ChangeWindowMessageFilterExInited), Ord(True));
  end;
  if Assigned(ChangeWindowMessageFilterExFunc) then
    ChangeWindowMessageFilterExFunc(Wnd, Msg, MSGFLT_ALLOW, nil)
  else
    AddToWindowMessageFilter(Msg);
end;

{$IFNDEF UNICODE}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

function ShutdownBlockReasonCreate(Wnd: HWND; const Reason: String): Boolean;
var
  ShutdownBlockReasonCreateFunc: function(Wnd: HWND; pwszReason: LPCWSTR): Bool; stdcall;
{$IFNDEF UNICODE}
  Buf: array[0..4095] of WideChar;
{$ENDIF}
begin
  { MSDN doesn't say whether you must call Destroy before a second Create, but it does say a Destroy
    without a previous Create is a no-op, so call Destroy for safety. }
  ShutdownBlockReasonDestroy(Wnd);

  ShutdownBlockReasonCreateFunc := GetProcAddress(GetModuleHandle(user32), 'ShutdownBlockReasonCreate');
  if Assigned(ShutdownBlockReasonCreateFunc) then begin
{$IFDEF UNICODE}
    Result := ShutdownBlockReasonCreateFunc(Wnd, PChar(Reason));
{$ELSE}
    Buf[MultiByteToWideChar(CP_ACP, 0, PChar(Reason), Length(Reason), Buf,
      (SizeOf(Buf) div SizeOf(Buf[0])) - 1)] := #0;
    Result := ShutdownBlockReasonCreateFunc(Wnd, Buf);
{$ENDIF}
  end else
    Result := False;
end;

{ As MSDN says: if ShutdownBlockReasonCreate was not previously called, this function is a no-op. }
function ShutdownBlockReasonDestroy(Wnd: HWND): Boolean;
var
  ShutdownBlockReasonDestroyFunc: function(Wnd: HWND): Bool; stdcall;
begin
  ShutdownBlockReasonDestroyFunc := GetProcAddress(GetModuleHandle(user32), 'ShutdownBlockReasonDestroy');
  Result := Assigned(ShutdownBlockReasonDestroyFunc) and ShutdownBlockReasonDestroyFunc(Wnd);
end;

function TryStrToBoolean(const S: String; var BoolResult: Boolean): Boolean;
begin
  if (S = '0') or (CompareText(S, 'no') = 0) or (CompareText(S, 'false') = 0) then begin
    BoolResult := False;
    Result := True;
  end
  else if (S = '1') or (CompareText(S, 'yes') = 0) or (CompareText(S, 'true') = 0) then begin
    BoolResult := True;
    Result := True;
  end
  else
    Result := False;
end;

procedure WaitMessageWithTimeout(const Milliseconds: DWORD);
{ Like WaitMessage, but times out if a message isn't received before
  Milliseconds ms have elapsed. }
begin
  MsgWaitForMultipleObjects(0, THandle(nil^), False, Milliseconds, QS_ALLINPUT);
end;

function MoveFileReplace(const ExistingFileName, NewFileName: String): Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    Result := MoveFileEx(PChar(ExistingFileName), PChar(NewFileName),
      MOVEFILE_REPLACE_EXISTING);
  end
  else begin
    Result := DeleteFile(PChar(NewFileName));
    if Result or (GetLastError = ERROR_FILE_NOT_FOUND) then
      Result := MoveFile(PChar(ExistingFileName), PChar(NewFileName));
  end;
end;

var
  SHAutoCompleteInitialized: Boolean;
  SHAutoCompleteFunc: function(hwndEdit: HWND; dwFlags: dWord): LongInt; stdcall;

procedure TryEnableAutoCompleteFileSystem(Wnd: HWND);
const
  SHACF_FILESYSTEM = $1;
var
  M: HMODULE;
begin
  if not SHAutoCompleteInitialized then begin
    M := SafeLoadLibrary(PChar(AddBackslash(GetSystemDir) + 'shlwapi.dll'),
      SEM_NOOPENFILEERRORBOX);
    if M <> 0 then
      SHAutoCompleteFunc := GetProcAddress(M, 'SHAutoComplete');
    SHAutoCompleteInitialized := True;
  end;

  if Assigned(SHAutoCompleteFunc) then
    SHAutoCompleteFunc(Wnd, SHACF_FILESYSTEM);
end;

function IsWebPackage(const PackageSource: String): Boolean;
{ Returns True if the PackageSource is a web package. }
var
  I: Integer;
begin
  I := Pos(':', PackageSource);
  if I <= 2 then
    Result := False
  else
    Result := True; // check for HTTP or FTP ?
end;


function ExtractWebFileName(const PackageSource: String): String;
{ Returns the filename for the specified web package. }
var
  I: Integer;
begin
  I := PathLastDelimiter('/', PackageSource);
  if I > 0 then
    Result := Copy(PackageSource, I + 1, Length(PackageSource))
  else
    Result := PathExtractName(PackageSource);
end;

{ TOneShotTimer }

function TOneShotTimer.Expired: Boolean;
begin
  Result := (TimeRemaining = 0);
end;

procedure TOneShotTimer.SleepUntilExpired;
var
  Remaining: Cardinal;
begin
  while True do begin
    Remaining := TimeRemaining;
    if Remaining = 0 then
      Break;
    Sleep(Remaining);
  end;
end;

procedure TOneShotTimer.Start(const Timeout: Cardinal);
begin
  FStartTick := GetTickCount;
  FTimeout := Timeout;
  FLastElapsed := 0;
end;

function TOneShotTimer.TimeElapsed: Cardinal;
var
  Elapsed: Cardinal;
begin
  Elapsed := GetTickCount - FStartTick;
  if Elapsed > FLastElapsed then
    FLastElapsed := Elapsed;
  Result := FLastElapsed;
end;

function TOneShotTimer.TimeRemaining: Cardinal;
var
  Elapsed: Cardinal;
begin
  Elapsed := TimeElapsed;
  if Elapsed < FTimeout then
    Result := FTimeout - Elapsed
  else
    Result := 0;
end;

end.
