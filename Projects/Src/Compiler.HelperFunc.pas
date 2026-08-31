unit Compiler.HelperFunc;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Additional compiler functions
}

interface

uses
  Windows, Classes, SysUtils,
  Shared.FileClass;

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
  PathFunc, TrustFunc, Shared.CommonFunc,
  Compression.Base, Compiler.Messages;

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
    try
      if Assigned(OnCheckedTrust) then
        OnCheckedTrust(CheckTrust);

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
      PEHeaderOffset := PInteger(@DosHeader[60])^;
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
