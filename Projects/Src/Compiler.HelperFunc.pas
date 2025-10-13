unit Compiler.HelperFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Additional compiler functions
}

interface

uses
  Windows, Classes, SysUtils, UITypes,
  Shared.FileClass;

const
  clScrollBar = TColors.SysScrollBar;
  clBackground = TColors.SysBackground;
  clActiveCaption = TColors.SysActiveCaption;
  clInactiveCaption = TColors.SysInactiveCaption;
  clMenu = TColors.SysMenu;
  clWindow = TColors.SysWindow;
  clWindowFrame = TColors.SysWindowFrame;
  clMenuText = TColors.SysMenuText;
  clWindowText = TColors.SysWindowText;
  clCaptionText = TColors.SysCaptionText;
  clActiveBorder = TColors.SysActiveBorder;
  clInactiveBorder = TColors.SysInactiveBorder;
  clAppWorkSpace = TColors.SysAppWorkSpace;
  clHighlight = TColors.SysHighlight;
  clHighlightText = TColors.SysHighlightText;
  clBtnFace = TColors.SysBtnFace;
  clBtnShadow = TColors.SysBtnShadow;
  clGrayText = TColors.SysGrayText;
  clBtnText = TColors.SysBtnText;
  clInactiveCaptionText = TColors.SysInactiveCaptionText;
  clBtnHighlight = TColors.SysBtnHighlight;
  cl3DDkShadow = TColors.Sys3DDkShadow;
  cl3DLight = TColors.Sys3DLight;
  clInfoText = TColors.SysInfoText;
  clInfoBk = TColors.SysInfoBk;

  clBlack = TColors.Black;
  clMaroon = TColors.Maroon;
  clGreen = TColors.Green;
  clOlive = TColors.Olive;
  clNavy = TColors.Navy;
  clPurple = TColors.Purple;
  clTeal = TColors.Teal;
  clGray = TColors.Gray;
  clSilver = TColors.Silver;
  clRed = TColors.Red;
  clLime = TColors.Lime;
  clYellow = TColors.Yellow;
  clBlue = TColors.Blue;
  clFuchsia = TColors.Fuchsia;
  clAqua = TColors.Aqua;
  clLtGray = TColors.LtGray;
  clDkGray = TColors.DkGray;
  clWhite = TColors.White;
  clNone = TColors.SysNone;
  clDefault = TColors.SysDefault;

function StringToColor(const S: string): TColor;
function IsRelativePath(const Filename: String): Boolean;
function CreateMemoryStreamFromFile(const Filename: String; const CheckTrust: Boolean = False;
  const OnCheckedTrust: TProc<Boolean> = nil): TMemoryStream;
function FileSizeAndCRCIs(const Filename: String; const Size: Cardinal;
  const CRC: Longint): Boolean;
function IsX86OrX64Executable(const F: TFile): Boolean;
function CountChars(const S: String; C: Char): Integer;
function IsValidIdentString(const S: String; AllowBackslash, AllowOperators: Boolean): Boolean;
procedure SkipWhitespace(var S: PChar);
function ExtractWords(var S: PChar; const Sep: Char): String;
function UnescapeBraces(const S: String): String;

implementation

uses
  TrustFunc, Shared.CommonFunc,
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

function StringToColor(const S: string): TColor;

  function IdentToColor(const Ident: string; var Color: LongInt): Boolean;
  begin
    for var I := Low(Colors) to High(Colors) do
      if CompareText(Colors[I].Name, Ident) = 0 then
      begin
        Result := True;
        Color := LongInt(Colors[I].Value);
        Exit;
      end;
    Result := False;
  end;

begin
  if not IdentToColor(S, Longint(Result)) then begin
    var Hex := S;
    if (Length(Hex) = 7) and (Hex[1] = '#') then
      Hex := '$' + Copy(Hex, 6, 2)  + Copy(Hex, 4, 2) + Copy(Hex, 2, 2);
    Result := TColor(StrToInt(Hex));
  end;
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

function CreateMemoryStreamFromFile(const Filename: String; const CheckTrust: Boolean;
  const OnCheckedTrust: TProc<Boolean>): TMemoryStream;
{ Creates a TMemoryStream and loads the contents of the specified file into it }
var
  F: TFile;
  SizeOfFile: Cardinal;
begin
  Result := TMemoryStream.Create;
  try
    var FS: TFileStream;
    if CheckTrust then begin
      try
        FS := CheckFileTrust(Filename, [cftoKeepOpen]);
      except
        raise Exception.CreateFmt(SCompilerCheckPrecompiledFileTrustError, [GetExceptMessage]);
      end;
    end else
      FS := nil;
    if Assigned(OnCheckedTrust) then
      OnCheckedTrust(CheckTrust);
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
    finally
      FS.Free;
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
  Buf: AnsiString;
begin
  Result := False;
  try
    F := TFile.Create(Filename, fdOpenExisting, faRead, fsRead);
    try
      if F.Size = Size then begin
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

end.
