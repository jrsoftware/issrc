unit TrustFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Trust support functions using ISSigFunc and key texts from TrustFunc.AllowedPublicKeys.inc

  In Inno Setup these functions are only used by Compil32, ISCC, and ISCmplr. Verification of
  the user's files by ISCmplr and Setup is done by calling ISSigFunc directly and uses the
  user's key texts.
}

{.$DEFINE TRUSTALL}

interface

uses
  System.Classes;

type
  TCheckFileTrustOption = (cftoKeepOpen, cftoTrustAllOnDebug);
  TCheckFileTrustOptions = set of TCheckFileTrustOption;
  TLoadTrustedLibraryOption = (ltloTrustAllOnDebug);
  TLoadTrustedLibraryOptions = set of TLoadTrustedLibraryOption;

function CheckFileTrust(const Filename: String; const Options: TCheckFileTrustOptions): TFileStream;
function LoadTrustedLibrary(const Filename: String; const Options: TLoadTrustedLibraryOptions): HMODULE;

implementation

uses
  Winapi.Windows, System.SysUtils {$IFNDEF TRUSTALL}, ECDSA, SHA256, ISSigFunc, PathFunc {$ENDIF};

function Win32ErrorString(ErrorCode: Cardinal): String;
{ Like SysErrorMessage but also passes the FORMAT_MESSAGE_IGNORE_INSERTS flag
  which allows the function to succeed on errors like 129 }
var
  Buffer: array[0..1023] of Char;
begin
  var Len := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_ARGUMENT_ARRAY, nil,
    ErrorCode, 0, Buffer, SizeOf(Buffer) div SizeOf(Buffer[0]), nil);
  while (Len > 0) and ((Buffer[Len-1] <= ' ') or (Buffer[Len-1] = '.')) do
    Dec(Len);
  SetString(Result, Buffer, Len);
end;

function CheckFileTrust(const Filename: String; const Options: TCheckFileTrustOptions): TFileStream;
{$IFNDEF TRUSTALL}
var
  AllowedKeys: array of TECDSAKey;
{$ENDIF}
begin
  var Attr := GetFileAttributes(PChar(Filename));
  if (Attr = INVALID_FILE_ATTRIBUTES) or (Attr and faDirectory <> 0) then
    raise Exception.Create(Win32ErrorString(ERROR_FILE_NOT_FOUND));
{$IFNDEF TRUSTALL}
{$IFDEF DEBUG}
  if cftoTrustAllOnDebug in Options then
    Exit(nil);
{$ENDIF}
  var ExpectedFileName: String;
  var ExpectedFileSize: Int64;
  var ExpectedFileHash: TSHA256Digest;

  var AllowedPublicKey1Text, AllowedPublicKey2Text: String;
  {$I TrustFunc.AllowedPublicKeys.inc}
  var Key1: TECDSAKey := nil;
  var Key2: TECDSAKey := nil;
  try
    { Import keys }
    Key1 := TECDSAKey.Create;
    if ISSigImportKeyText(Key1, AllowedPublicKey1Text, False) <> ikrSuccess then
      raise Exception.Create('ISSigImportKeyText failed');
    if AllowedPublicKey2Text <> '' then begin
      Key2 := TECDSAKey.Create;
      if ISSigImportKeyText(Key2, AllowedPublicKey2Text, False) <> ikrSuccess then
        raise Exception.Create('ISSigImportKeyText failed');
    end;

    if Key2 <> nil then
      AllowedKeys := [Key1, Key2]
    else
      AllowedKeys := [Key1];

    { Verify signature }
    if not ISSigVerifySignature(Filename, AllowedKeys, ExpectedFileName, ExpectedFileSize, ExpectedFileHash,
      nil,
      procedure(const Filename, SigFilename: String)
      begin
        raise Exception.CreateFmt('Signature file "%s" does not exist', [SigFilename]);
      end,
      procedure(const Filename, SigFilename: String; const VerifyResult: TISSigVerifySignatureResult)
      begin
        raise Exception.CreateFmt('Signature file "%s" is not valid', [SigFilename]);
      end
    ) then
      raise Exception.Create('Unexpected ISSigVerifySignature result');
  finally
    Key2.Free;
    Key1.Free;
  end;
  
  { Verify file, keeping open afterwards if requested
    Also see Setup.ScriptFunc's ISSigVerify which can also keep open afterwards }
  if (ExpectedFileName <> '') and not PathSame(PathExtractName(Filename), ExpectedFileName) then
    raise Exception.CreateFmt('File "%s" is not trusted (incorrect name).', [Filename]);
  var F := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    if F.Size <> ExpectedFileSize then
      raise Exception.CreateFmt('File "%s" is not trusted (incorrect size).', [Filename]);
    if not SHA256DigestsEqual(ISSigCalcStreamHash(F), ExpectedFileHash) then
      raise Exception.CreateFmt('File "%s" is not trusted (incorrect hash).', [Filename]);
  except
    FreeAndNil(F);
    raise;
  end;
  if not (cftoKeepOpen in Options) then
    FreeAndNil(F);

  Result := F;
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function DoLoadLibrary(const Filename: String): HMODULE;
begin
  Result := SafeLoadLibrary(PChar(Filename), SEM_NOOPENFILEERRORBOX);
  if Result = 0 then
    raise Exception.Create(Win32ErrorString(GetLastError));
end;

function LoadTrustedLibrary(const Filename: String; const Options: TLoadTrustedLibraryOptions): HMODULE;
begin
  var CheckFileTrustOptions: TCheckFileTrustOptions := [cftoKeepOpen];
  if ltloTrustAllOnDebug in Options then
    Include(CheckFileTrustOptions, cftoTrustAllOnDebug);
  const F = CheckFileTrust(Filename, CheckFileTrustOptions);
  try
    Result := DoLoadLibrary(Filename);
  finally
    F.Free;
  end;
end;

end.
