unit TrustFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Trust support functions using ISSigFunc and key texts from TrustFunc.AllowedPublicKeys.inc
}

{.$DEFINE TRUSTALL}

interface

procedure CheckFileTrust(const FileName: String; const CheckExists: Boolean = True);
function LoadTrustedLibrary(const FileName: String; const TrustAllOnDebug: Boolean = False): HMODULE;

implementation

uses
  Winapi.Windows, System.SysUtils, System.Classes {$IFNDEF TRUSTALL}, ECDSA, SHA256, ISSigFunc {$ENDIF};

procedure CheckFileTrust(const FileName: String; const CheckExists: Boolean);
{$IFNDEF TRUSTALL}
var
  AllowedKeys: array of TECDSAKey;
{$ENDIF}
begin
  if CheckExists then begin
    var Attr := GetFileAttributes(PChar(FileName));
    if (Attr = INVALID_FILE_ATTRIBUTES) or (Attr and faDirectory <> 0) then
      raise Exception.CreateFmt('File "%s" does not exist.',
        [FileName]);
  end;
{$IFNDEF TRUSTALL}
  var ExpectedFileSize: Int64;
  var ExpectedFileHash: TSHA256Digest;

  var AllowedPublicKey1Text, AllowedPublicKey2Text: String;
  {$I TrustFunc.AllowedPublicKeys.inc}
  var Key1: TECDSAKey := nil;
  var Key2: TECDSAKey := nil;
  try
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

    ISSigVerifySignature(Filename, AllowedKeys, ExpectedFileSize, ExpectedFileHash, nil, nil,
      procedure(const SigFilename: String; const VerifyResult: TISSigVerifySignatureResult)
      begin
        if VerifyResult <> vsrSuccess then
          raise Exception.CreateFmt('Signature file "%s" is not valid', [SigFileName]);
      end);
  finally
    Key2.Free;
    Key1.Free;
  end;
  
  const F = TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if F.Size <> ExpectedFileSize then
      raise Exception.CreateFmt('File "%s" is not trusted (incorrect size).',
        [FileName]);
    if not SHA256DigestsEqual(ISSigCalcStreamHash(F), ExpectedFileHash) then
      raise Exception.CreateFmt('File "%s" is not trusted (incorrect hash).',
        [FileName]);
  finally
    F.Free;
  end;
{$ENDIF}
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

function DoLoadLibrary(const FileName: String): HMODULE;
begin
  Result := SafeLoadLibrary(PChar(FileName), SEM_NOOPENFILEERRORBOX);
  if Result = 0 then
    raise Exception.Create(Win32ErrorString(GetLastError));
end;

function LoadTrustedLibrary(const FileName: String; const TrustAllOnDebug: Boolean): HMODULE;
begin
{$IFDEF DEBUG}
  if TrustAllOnDebug then begin
    Result := DoLoadLibrary(FileName);
    Exit;
  end;
{$ENDIF}
  { First open a temporary regular handle to the library to protect it from changes
    between the trust check and the load }
  const F = TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    CheckFileTrust(FileName, False);
    Result := DoLoadLibrary(FileName);
  finally
    F.Free;
  end;
end;

end.
