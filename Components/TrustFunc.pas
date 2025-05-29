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
  TCheckFileTrustOption = (cftoKeepOpen);
  TCheckFileTrustOptions = set of TCheckFileTrustOption;
  TLoadTrustedLibraryOption = (ltloTrustAllOnDebug);
  TLoadTrustedLibraryOptions = set of TLoadTrustedLibraryOption;

function CheckFileTrust(const FileName: String; const Options: TCheckFileTrustOptions): TFileStream;
function LoadTrustedLibrary(const FileName: String; const Options: TLoadTrustedLibraryOptions): HMODULE;

implementation

uses
  Winapi.Windows, System.SysUtils {$IFNDEF TRUSTALL}, ECDSA, SHA256, ISSigFunc {$ENDIF};

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

function CheckFileTrust(const FileName: String; const Options: TCheckFileTrustOptions): TFileStream;
{$IFNDEF TRUSTALL}
var
  AllowedKeys: array of TECDSAKey;
{$ENDIF}
begin
  var Attr := GetFileAttributes(PChar(FileName));
  if (Attr = INVALID_FILE_ATTRIBUTES) or (Attr and faDirectory <> 0) then
    raise Exception.Create(Win32ErrorString(ERROR_FILE_NOT_FOUND));
{$IFNDEF TRUSTALL}
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
    if not ISSigVerifySignature(Filename, AllowedKeys, ExpectedFileSize, ExpectedFileHash,
      nil,
      procedure(const Filename, SigFilename: String)
      begin
        raise Exception.CreateFmt('Signature file "%s" does not exist', [SigFileName]);
      end,
      procedure(const SigFilename: String; const VerifyResult: TISSigVerifySignatureResult)
      begin
        raise Exception.CreateFmt('Signature file "%s" is not valid', [SigFileName]);
      end
    ) then
      raise Exception.Create('Unexpected ISSigVerifySignature result');
  finally
    Key2.Free;
    Key1.Free;
  end;
  
  { Verify file, keeping open afterwards if requested
    Also see Setup.ScriptFunc's ISSigVerify }
  var F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if F.Size <> ExpectedFileSize then
      raise Exception.CreateFmt('File "%s" is not trusted (incorrect size).',
        [FileName]);
    if not SHA256DigestsEqual(ISSigCalcStreamHash(F), ExpectedFileHash) then
      raise Exception.CreateFmt('File "%s" is not trusted (incorrect hash).',
        [FileName]);
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

function DoLoadLibrary(const FileName: String): HMODULE;
begin
  Result := SafeLoadLibrary(PChar(FileName), SEM_NOOPENFILEERRORBOX);
  if Result = 0 then
    raise Exception.Create(Win32ErrorString(GetLastError));
end;

function LoadTrustedLibrary(const FileName: String; const Options: TLoadTrustedLibraryOptions): HMODULE;
begin
{$IFDEF DEBUG}
  if ltloTrustAllOnDebug in Options then begin
    Result := DoLoadLibrary(FileName);
    Exit;
  end;
{$ENDIF}
  const F = CheckFileTrust(FileName, [cftoKeepOpen]);
  try
    Result := DoLoadLibrary(FileName);
  finally
    F.Free;
  end;
end;

end.
