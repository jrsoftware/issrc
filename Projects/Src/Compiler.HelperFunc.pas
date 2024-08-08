unit Compiler.HelperFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Additional compiler functions
}

interface

uses
  Windows, Classes, Shared.FileClass;

type
  TColor = $7FFFFFFF-1..$7FFFFFFF;

const
  clScrollBar = TColor(COLOR_SCROLLBAR or $80000000);
  clBackground = TColor(COLOR_BACKGROUND or $80000000);
  clActiveCaption = TColor(COLOR_ACTIVECAPTION or $80000000);
  clInactiveCaption = TColor(COLOR_INACTIVECAPTION or $80000000);
  clMenu = TColor(COLOR_MENU or $80000000);
  clWindow = TColor(COLOR_WINDOW or $80000000);
  clWindowFrame = TColor(COLOR_WINDOWFRAME or $80000000);
  clMenuText = TColor(COLOR_MENUTEXT or $80000000);
  clWindowText = TColor(COLOR_WINDOWTEXT or $80000000);
  clCaptionText = TColor(COLOR_CAPTIONTEXT or $80000000);
  clActiveBorder = TColor(COLOR_ACTIVEBORDER or $80000000);
  clInactiveBorder = TColor(COLOR_INACTIVEBORDER or $80000000);
  clAppWorkSpace = TColor(COLOR_APPWORKSPACE or $80000000);
  clHighlight = TColor(COLOR_HIGHLIGHT or $80000000);
  clHighlightText = TColor(COLOR_HIGHLIGHTTEXT or $80000000);
  clBtnFace = TColor(COLOR_BTNFACE or $80000000);
  clBtnShadow = TColor(COLOR_BTNSHADOW or $80000000);
  clGrayText = TColor(COLOR_GRAYTEXT or $80000000);
  clBtnText = TColor(COLOR_BTNTEXT or $80000000);
  clInactiveCaptionText = TColor(COLOR_INACTIVECAPTIONTEXT or $80000000);
  clBtnHighlight = TColor(COLOR_BTNHIGHLIGHT or $80000000);
  cl3DDkShadow = TColor(COLOR_3DDKSHADOW or $80000000);
  cl3DLight = TColor(COLOR_3DLIGHT or $80000000);
  clInfoText = TColor(COLOR_INFOTEXT or $80000000);
  clInfoBk = TColor(COLOR_INFOBK or $80000000);

  clBlack = TColor($000000);
  clMaroon = TColor($000080);
  clGreen = TColor($008000);
  clOlive = TColor($008080);
  clNavy = TColor($800000);
  clPurple = TColor($800080);
  clTeal = TColor($808000);
  clGray = TColor($808080);
  clSilver = TColor($C0C0C0);
  clRed = TColor($0000FF);
  clLime = TColor($00FF00);
  clYellow = TColor($00FFFF);
  clBlue = TColor($FF0000);
  clFuchsia = TColor($FF00FF);
  clAqua = TColor($FFFF00);
  clLtGray = TColor($C0C0C0);
  clDkGray = TColor($808080);
  clWhite = TColor($FFFFFF);
  clNone = TColor($1FFFFFFF);
  clDefault = TColor($20000000);

function IdentToColor(const Ident: string; var Color: Longint): Boolean;
function StringToColor(const S: string): TColor;
function IsRelativePath(const Filename: String): Boolean;
function CreateMemoryStreamFromFile(const Filename: String): TMemoryStream;
function FileSizeAndCRCIs(const Filename: String; const Size: Cardinal;
  const CRC: Longint): Boolean;
function IsX86OrX64Executable(const F: TFile): Boolean;
function CountChars(const S: String; C: Char): Integer;
function IsValidIdentString(const S: String; AllowBackslash, AllowOperators: Boolean): Boolean;
procedure SkipWhitespace(var S: PChar);
function ExtractWords(var S: PChar; const Sep: Char): String;
function UnescapeBraces(const S: String): String;
procedure GenerateRandomBytes(var Buffer; Bytes: Cardinal);

implementation

uses
  SysUtils, Shared.CommonFunc, Shared.Int64Em,
  Compression.Base, Compiler.Messages;

type
  TColorEntry = record
    Value: TColor;
    Name: string;
  end;

const
  Colors: array[0..41] of TColorEntry = (
    (Value: clBlack; Name: 'clBlack'),
    (Value: clMaroon; Name: 'clMaroon'),
    (Value: clGreen; Name: 'clGreen'),
    (Value: clOlive; Name: 'clOlive'),
    (Value: clNavy; Name: 'clNavy'),
    (Value: clPurple; Name: 'clPurple'),
    (Value: clTeal; Name: 'clTeal'),
    (Value: clGray; Name: 'clGray'),
    (Value: clSilver; Name: 'clSilver'),
    (Value: clRed; Name: 'clRed'),
    (Value: clLime; Name: 'clLime'),
    (Value: clYellow; Name: 'clYellow'),
    (Value: clBlue; Name: 'clBlue'),
    (Value: clFuchsia; Name: 'clFuchsia'),
    (Value: clAqua; Name: 'clAqua'),
    (Value: clWhite; Name: 'clWhite'),
    (Value: clScrollBar; Name: 'clScrollBar'),
    (Value: clBackground; Name: 'clBackground'),
    (Value: clActiveCaption; Name: 'clActiveCaption'),
    (Value: clInactiveCaption; Name: 'clInactiveCaption'),
    (Value: clMenu; Name: 'clMenu'),
    (Value: clWindow; Name: 'clWindow'),
    (Value: clWindowFrame; Name: 'clWindowFrame'),
    (Value: clMenuText; Name: 'clMenuText'),
    (Value: clWindowText; Name: 'clWindowText'),
    (Value: clCaptionText; Name: 'clCaptionText'),
    (Value: clActiveBorder; Name: 'clActiveBorder'),
    (Value: clInactiveBorder; Name: 'clInactiveBorder'),
    (Value: clAppWorkSpace; Name: 'clAppWorkSpace'),
    (Value: clHighlight; Name: 'clHighlight'),
    (Value: clHighlightText; Name: 'clHighlightText'),
    (Value: clBtnFace; Name: 'clBtnFace'),
    (Value: clBtnShadow; Name: 'clBtnShadow'),
    (Value: clGrayText; Name: 'clGrayText'),
    (Value: clBtnText; Name: 'clBtnText'),
    (Value: clInactiveCaptionText; Name: 'clInactiveCaptionText'),
    (Value: clBtnHighlight; Name: 'clBtnHighlight'),
    (Value: cl3DDkShadow; Name: 'cl3DDkShadow'),
    (Value: cl3DLight; Name: 'cl3DLight'),
    (Value: clInfoText; Name: 'clInfoText'),
    (Value: clInfoBk; Name: 'clInfoBk'),
    (Value: clNone; Name: 'clNone'));

function IdentToColor(const Ident: string; var Color: Longint): Boolean;
var
  I: Integer;
begin
  for I := Low(Colors) to High(Colors) do
    if CompareText(Colors[I].Name, Ident) = 0 then
    begin
      Result := True;
      Color := Longint(Colors[I].Value);
      Exit;
    end;
  Result := False;
end;

function StringToColor(const S: string): TColor;
begin
  if not IdentToColor(S, Longint(Result)) then
    Result := TColor(StrToInt(S));
end;

function IsRelativePath(const Filename: String): Boolean;
var
  L: Integer;
begin
  Result := True;
  L := Length(Filename);
  if ((L >= 1) and (Filename[1] = '\')) or
     ((L >= 2) and CharInSet(Filename[1], ['A'..'Z', 'a'..'z']) and (Filename[2] = ':')) then
    Result := False;
end;

function CreateMemoryStreamFromFile(const Filename: String): TMemoryStream;
{ Creates a TMemoryStream and loads the contents of the specified file into it }
var
  F: TFile;
  SizeOfFile: Cardinal;
begin
  Result := TMemoryStream.Create;
  try
    { Why not use TMemoryStream.LoadFromFile here?
      1. On Delphi 2 it opens files for exclusive access (not good).
      2. It doesn't give specific error messages. }
    F := TFile.Create(Filename, fdOpenExisting, faRead, fsRead);
    try
      SizeOfFile := F.CappedSize;
      Result.SetSize(SizeOfFile);
      F.ReadBuffer(Result.Memory^, SizeOfFile);
    finally
      F.Free;
    end;
  except
    Result.Free;
    raise Exception.CreateFmt(SCompilerReadError, [Filename, GetExceptMessage]);
  end;
end;

function FileSizeAndCRCIs(const Filename: String; const Size: Cardinal;
  const CRC: Longint): Boolean;
var
  F: TFile;
  SizeOfFile: Integer64;
  Buf: AnsiString;
begin
  Result := False;
  try
    F := TFile.Create(Filename, fdOpenExisting, faRead, fsRead);
    try
      SizeOfFile := F.Size;
      if (SizeOfFile.Lo = Size) and (SizeOfFile.Hi = 0) then begin
        SetLength(Buf, Size);
        F.ReadBuffer(Buf[1], Size);
        if GetCRC32(Buf[1], Size) = CRC then
          Result := True;
      end;
    finally
      F.Free;
    end;
  except
  end;
end;

const
  IMAGE_NT_SIGNATURE = $00004550;  { 'PE'#0#0 }
  IMAGE_NT_OPTIONAL_HDR32_MAGIC = $10b;
type
  TImageFileHeader = packed record
    Machine: Word;
    NumberOfSections: Word;
    TimeDateStamp: DWORD;
    PointerToSymbolTable: DWORD;
    NumberOfSymbols: DWORD;
    SizeOfOptionalHeader: Word;
    Characteristics: Word;
  end;

function IsX86OrX64Executable(const F: TFile): Boolean;
const
  IMAGE_FILE_MACHINE_I386 = $014C;
  IMAGE_FILE_MACHINE_AMD64 = $8664;
var
  DosHeader: array[0..63] of Byte;
  PEHeaderOffset: Longint;
  PESigAndHeader: packed record
    Sig: DWORD;
    Machine: Word;
  end;
begin
  Result := False;
  if F.Read(DosHeader, SizeOf(DosHeader)) = SizeOf(DosHeader) then begin
    if (DosHeader[0] = Ord('M')) and (DosHeader[1] = Ord('Z')) then begin
      PEHeaderOffset := PLongint(@DosHeader[60])^;
      if PEHeaderOffset > 0 then begin
        F.Seek(PEHeaderOffset);
        if F.Read(PESigAndHeader, SizeOf(PESigAndHeader)) = SizeOf(PESigAndHeader) then begin
          if (PESigAndHeader.Sig = IMAGE_NT_SIGNATURE) and
             ((PESigAndHeader.Machine = IMAGE_FILE_MACHINE_I386) or
              (PESigAndHeader.Machine = IMAGE_FILE_MACHINE_AMD64)) then
            Result := True;
        end;
      end;
    end;
  end;
  F.Seek(0);
end;

function CountChars(const S: String; C: Char): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = C then
      Inc(Result);
end;

function IsValidIdentString(const S: String; AllowBackslash, AllowOperators: Boolean): Boolean;
var
  I, N: Integer;
begin
  if S = '' then
    Result := False
  else if not AllowOperators and ((CompareText(S, 'not') = 0) or
     (CompareText(S, 'and') = 0) or (CompareText(S, 'or') = 0)) then
    Result := False
  else begin
    N := Length(S);
    for I := 1 to N do
      if not (CharInSet(S[I], ['A'..'Z', 'a'..'z', '_']) or
              ((I > 1) and CharInSet(S[I], ['0'..'9'])) or
              (AllowBackslash and (I > 1) and (I < N) and (S[I] = '\'))) then begin
        Result := False;
        Exit;
      end;
    Result := True;
  end;
end;

procedure SkipWhitespace(var S: PChar);
begin
  while CharInSet(S^, [#1..' ']) do
    Inc(S);
end;

function ExtractWords(var S: PChar; const Sep: Char): String;
{ Extracts characters from S until it reaches the character Sep or the end
  of S. The returned string has trailing whitespace characters trimmed off. }
var
  StartPos, EndPos: PChar;
begin
  StartPos := S;
  EndPos := S;
  while (S^ <> #0) and (S^ <> Sep) do begin
    if S^ > ' ' then
      EndPos := S + 1;
    Inc(S);
  end;
  SetString(Result, StartPos, EndPos - StartPos);
end;

function UnescapeBraces(const S: String): String;
{ Changes all '{{' to '{'. Assumes that S does not contain any constants; you
  should check before calling. }
var
  I: Integer;
begin
  Result := S;
  I := 1;
  while I < Length(Result) do begin
    if Result[I] = '{' then begin
      Inc(I);
      if Result[I] = '{' then
        Delete(Result, I, 1);
    end
    else
      Inc(I);
  end;
end;

type
  HCRYPTPROV = DWORD;

const
  PROV_RSA_FULL = 1;
  CRYPT_VERIFYCONTEXT = $F0000000;

function CryptAcquireContext(var phProv: HCRYPTPROV; pszContainer: PAnsiChar;
  pszProvider: PAnsiChar; dwProvType: DWORD; dwFlags: DWORD): BOOL;
  stdcall; external advapi32 name 'CryptAcquireContextA';
function CryptReleaseContext(hProv: HCRYPTPROV; dwFlags: DWORD): BOOL;
  stdcall; external advapi32 name 'CryptReleaseContext';
function CryptGenRandom(hProv: HCRYPTPROV; dwLen: DWORD; pbBuffer: Pointer): BOOL;
  stdcall; external advapi32 name 'CryptGenRandom';

var
  CryptProv: HCRYPTPROV;

procedure GenerateRandomBytes(var Buffer; Bytes: Cardinal);
var
  ErrorCode: DWORD;
begin
  if CryptProv = 0 then begin
    if not CryptAcquireContext(CryptProv, nil, nil, PROV_RSA_FULL,
       CRYPT_VERIFYCONTEXT) then begin
      ErrorCode := GetLastError;
      raise Exception.CreateFmt(SCompilerFunctionFailedWithCode,
        ['CryptAcquireContext', ErrorCode, Win32ErrorString(ErrorCode)]);
    end;
    { Note: CryptProv is released in the 'finalization' section of this unit }
  end;
  FillChar(Buffer, Bytes, 0);
  if not CryptGenRandom(CryptProv, Bytes, @Buffer) then begin
    ErrorCode := GetLastError;
    raise Exception.CreateFmt(SCompilerFunctionFailedWithCode,
      ['CryptGenRandom', ErrorCode, Win32ErrorString(ErrorCode)]);
  end;
end;

initialization
finalization
  if CryptProv <> 0 then begin
    CryptReleaseContext(CryptProv, 0);
    CryptProv := 0;
  end;
end.