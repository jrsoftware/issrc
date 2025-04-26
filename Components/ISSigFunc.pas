unit ISSigFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Functions for creating/verifying .issig signatures and importing/exporting
  text-based keys
}

interface

uses
  Windows, SysUtils, Classes, ECDSA, SHA256;

type
  TISSigVerifySignatureResult = (vsrSuccess, vsrMalformed, vsrKeyNotFound,
    vsrBadSignature);
  TISSigImportKeyResult = (ikrSuccess, ikrMalformed, ikrNotPrivateKey);

{ Preferred, hardened functions for loading/saving .issig and key file text }
function ISSigLoadTextFromFile(const AFilename: String): String;
procedure ISSigSaveTextToFile(const AFilename, AText: String);

function ISSigCreateSignatureText(const AKey: TECDSAKey;
  const AFileSize: Int64; const AFileHash: TSHA256Digest): String;
function ISSigVerifySignatureText(const AAllowedKeys: array of TECDSAKey;
  const AText: String; out AFileSize: Int64;
  out AFileHash: TSHA256Digest): TISSigVerifySignatureResult; overload;
function ISSigVerifySignatureText(const AAllowedKeys: array of TECDSAKey;
  const AText: String; out AFileSize: Int64;
  out AFileHash: TSHA256Digest; out AKeyUsedID: String): TISSigVerifySignatureResult; overload;

procedure ISSigExportPrivateKeyText(const AKey: TECDSAKey;
  var APrivateKeyText: String);
procedure ISSigExportPublicKeyText(const AKey: TECDSAKey;
  var APublicKeyText: String);
procedure ISSigConvertPublicKeyToStrings(const APublicKey: TECDSAPublicKey;
  out APublicX, APublicY: String);
function ISSigParsePrivateKeyText(const AText: String;
  out APrivateKey: TECDSAPrivateKey): TISSigImportKeyResult;
function ISSigParsePublicKeyText(const AText: String;
  out APublicKey: TECDSAPublicKey): TISSigImportKeyResult;
function ISSigImportKeyText(const AKey: TECDSAKey; const AText: String;
  const ANeedPrivateKey: Boolean): TISSigImportKeyResult;
function ISSigImportPublicKey(const AKey: TECDSAKey;
  const AKeyID, APublicX, APublicY: String): TISSigImportKeyResult;

procedure ISSigCheckValidKeyID(const AKeyID: String);
procedure ISSigCheckValidPublicXOrY(const APublicXOrY: String);
function ISSigIsValidKeyIDForPublicXY(const AKeyID, APublicX, APublicY: String): Boolean;

function ISSigCalcStreamHash(const AStream: TStream): TSHA256Digest;

implementation

uses
  StringScanner;

const
  ISSigTextFileLengthLimit = 500;

  NonControlASCIICharsSet = [#32..#126];
  DigitsSet = ['0'..'9'];
  HexDigitsSet = DigitsSet + ['a'..'f'];

function ECDSAInt256ToString(const Value: TECDSAInt256): String;
begin
  Result := SHA256DigestToString(TSHA256Digest(Value));
end;

function ECDSAInt256FromString(const S: String): TECDSAInt256;
begin
  TSHA256Digest(Result) := SHA256DigestFromString(S);
end;

function CalcHashToSign(const AFileSize: Int64;
  const AFileHash: TSHA256Digest): TSHA256Digest;
begin
  var Context: TSHA256Context;
  SHA256Init(Context);
  SHA256Update(Context, AFileSize, SizeOf(AFileSize));
  SHA256Update(Context, AFileHash, SizeOf(AFileHash));
  Result := SHA256Final(Context);
end;

function CalcKeyID(const APublicKey: TECDSAPublicKey): TSHA256Digest;
begin
  Result := SHA256Buf(APublicKey, SizeOf(APublicKey));
end;

function ConsumeLineValue(var SS: TStringScanner; const AIdent: String;
  var AValue: String; const AMinValueLength, AMaxValueLength: Integer;
  const AAllowedChars: TSysCharSet): Boolean;
begin
  Result := False;
  if SS.Consume(AIdent) and SS.Consume(' ') then
    if SS.ConsumeMultiToString(AAllowedChars, AValue, AMinValueLength,
       AMaxValueLength) > 0 then begin
      { CRLF and LF line breaks are allowed (but not CR) }
      SS.Consume(#13);
      Result := SS.Consume(#10);
    end;
end;

function ISSigLoadTextFromFile(const AFilename: String): String;
{ Reads the specified file's contents into a string. This is intended only for
  loading .issig and key files. If the file appears to be invalid (e.g., if
  it is too large or contains invalid characters), then an empty string is
  returned, which will be reported as malformed when it is processed by
  ISSigVerifySignatureText or ISSigImportKeyText. }
begin
  var U: UTF8String;
  SetLength(U, ISSigTextFileLengthLimit + 1);

  const F = TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    const BytesRead = F.Read(U[Low(U)], Length(U));
    if BytesRead >= Length(U) then
      Exit('');
    SetLength(U, BytesRead);
  finally
    F.Free;
  end;

  { Defense-in-depth: Reject any non-CRLF control characters up front, as well
    as any non-ASCII characters (to avoid any possible issues with converting
    invalid multibyte characters) }
  for var C in U do
    if not CharInSet(C, [#10, #13, #32..#126]) then
      Exit('');

  Result := String(U);
end;

procedure ISSigSaveTextToFile(const AFilename, AText: String);
begin
  const F = TFileStream.Create(AFilename, fmCreate or fmShareExclusive);
  try
    const U = UTF8String(AText);
    if U <> '' then
      F.WriteBuffer(U[Low(U)], Length(U));
  finally
    F.Free;
  end;
end;

function ISSigCreateSignatureText(const AKey: TECDSAKey;
  const AFileSize: Int64; const AFileHash: TSHA256Digest): String;
begin
  { File size is limited to 16 digits (enough for >9 EB) }
  if (AFileSize < 0) or (AFileSize > 9_999_999_999_999_999) then
    raise Exception.Create('File size out of range');

  var PublicKey: TECDSAPublicKey;
  AKey.ExportPublicKey(PublicKey);

  const HashToSign = CalcHashToSign(AFileSize, AFileHash);
  var Sig: TECDSASignature;
  AKey.SignHash(HashToSign, Sig);

  Result := Format(
    'format issig-v1'#13#10 +
    'file-size %d'#13#10 +
    'file-hash %s'#13#10 +
    'key-id %s'#13#10 +
    'sig-r %s'#13#10 +
    'sig-s %s'#13#10,
    [AFileSize,
     SHA256DigestToString(AFileHash),
     SHA256DigestToString(CalcKeyID(PublicKey)),
     ECDSAInt256ToString(Sig.Sig_r),
     ECDSAInt256ToString(Sig.Sig_s)]);
end;

function ISSigVerifySignatureText(const AAllowedKeys: array of TECDSAKey;
  const AText: String; out AFileSize: Int64;
  out AFileHash: TSHA256Digest; out AKeyUsedID: String): TISSigVerifySignatureResult;
var
  TextValues: record
    Format, FileSize, FileHash, KeyID, Sig_r, Sig_s: String;
  end;
begin
  { To be extra safe, clear the "out" parameters just in case the caller isn't
    properly checking the function result }
  AFileSize := -1;
  FillChar(AFileHash, SizeOf(AFileHash), 0);
  AKeyUsedID := '';

  if Length(AText) > ISSigTextFileLengthLimit then
    Exit(vsrMalformed)
  else if Length(AAllowedKeys) = 0 then
    Exit(vsrKeyNotFound);

  var SS := TStringScanner.Create(AText);
  if not ConsumeLineValue(SS, 'format', TextValues.Format, 8, 8, NonControlASCIICharsSet) or
     (TextValues.Format <> 'issig-v1') or
     not ConsumeLineValue(SS, 'file-size', TextValues.FileSize, 1, 16, DigitsSet) or
     not ConsumeLineValue(SS, 'file-hash', TextValues.FileHash, 64, 64, HexDigitsSet) or
     not ConsumeLineValue(SS, 'key-id', TextValues.KeyID, 64, 64, HexDigitsSet) or
     not ConsumeLineValue(SS, 'sig-r', TextValues.Sig_r, 64, 64, HexDigitsSet) or
     not ConsumeLineValue(SS, 'sig-s', TextValues.Sig_s, 64, 64, HexDigitsSet) or
     not SS.ReachedEnd then
    Exit(vsrMalformed);

  { Don't allow leading zeros on file-size }
  if (Length(TextValues.FileSize) > 1) and
     (TextValues.FileSize[Low(TextValues.FileSize)] = '0') then
    Exit(vsrMalformed);

  { Find the key that matches the key ID }
  var KeyUsed: TECDSAKey := nil;
  const KeyID = SHA256DigestFromString(TextValues.KeyID);
  for var K in AAllowedKeys do begin
    var PublicKey: TECDSAPublicKey;
    K.ExportPublicKey(PublicKey);
    if SHA256DigestsEqual(KeyID, CalcKeyID(PublicKey)) then begin
      KeyUsed := K;
      Break;
    end;
  end;
  if KeyUsed = nil then
    Exit(vsrKeyNotFound);
  AKeyUsedID := TextValues.KeyID;

  const UnverifiedFileSize = StrToInt64(TextValues.FileSize);
  const UnverifiedFileHash = SHA256DigestFromString(TextValues.FileHash);
  const HashToSign = CalcHashToSign(UnverifiedFileSize, UnverifiedFileHash);
  var Sig: TECDSASignature;
  Sig.Sig_r := ECDSAInt256FromString(TextValues.Sig_r);
  Sig.Sig_s := ECDSAInt256FromString(TextValues.Sig_s);
  if KeyUsed.VerifySignature(HashToSign, Sig) then begin
    AFileSize := UnverifiedFileSize;
    AFileHash := UnverifiedFileHash;
    Result := vsrSuccess;
  end else
    Result := vsrBadSignature;
end;

function ISSigVerifySignatureText(const AAllowedKeys: array of TECDSAKey;
  const AText: String; out AFileSize: Int64;
  out AFileHash: TSHA256Digest): TISSigVerifySignatureResult;
begin
  var KeyUsedID: String;
  Result := ISSigVerifySignatureText(AAllowedKeys, AText, AFileSize, AFileHash, KeyUsedID);
end;

procedure ISSigExportPrivateKeyText(const AKey: TECDSAKey;
  var APrivateKeyText: String);
begin
  var PrivateKey: TECDSAPrivateKey;
  try
    AKey.ExportPrivateKey(PrivateKey);

    APrivateKeyText := Format(
      'format issig-private-key'#13#10 +
      'key-id %s'#13#10 +
      'public-x %s'#13#10 +
      'public-y %s'#13#10 +
      'private-d %s'#13#10,
      [SHA256DigestToString(CalcKeyID(PrivateKey.PublicKey)),
       ECDSAInt256ToString(PrivateKey.PublicKey.Public_x),
       ECDSAInt256ToString(PrivateKey.PublicKey.Public_y),
       ECDSAInt256ToString(PrivateKey.Private_d)]);
  finally
    PrivateKey.Clear;
  end;
end;

procedure ISSigExportPublicKeyText(const AKey: TECDSAKey;
  var APublicKeyText: String);
begin
  var PublicKey: TECDSAPublicKey;
  try
    AKey.ExportPublicKey(PublicKey);

    APublicKeyText := Format(
      'format issig-public-key'#13#10 +
      'key-id %s'#13#10 +
      'public-x %s'#13#10 +
      'public-y %s'#13#10,
      [SHA256DigestToString(CalcKeyID(PublicKey)),
       ECDSAInt256ToString(PublicKey.Public_x),
       ECDSAInt256ToString(PublicKey.Public_y)]);
  finally
    PublicKey.Clear;
  end;
end;

procedure ISSigConvertPublicKeyToStrings(const APublicKey: TECDSAPublicKey;
  out APublicX, APublicY: String);
begin
  APublicX := ECDSAInt256ToString(APublicKey.Public_x);
  APublicY := ECDSAInt256ToString(APublicKey.Public_y);
end;

function InternalParseKeyText(const AText: String;
  out APrivateKey: TECDSAPrivateKey;
  const ANeedPrivateKey: Boolean): TISSigImportKeyResult;
var
  TextValues: record
    Format, KeyID, Public_x, Public_y, Private_d: String;
  end;
begin
  Result := ikrMalformed;
  if Length(AText) > ISSigTextFileLengthLimit then
    Exit;

  var SS := TStringScanner.Create(AText);
  if not ConsumeLineValue(SS, 'format', TextValues.Format, 16, 17, NonControlASCIICharsSet) then
    Exit;
  var HasPrivateKey := False;
  if TextValues.Format = 'issig-private-key' then
    HasPrivateKey := True
  else if TextValues.Format = 'issig-public-key' then
    { already False }
  else
    Exit;

  if not ConsumeLineValue(SS, 'key-id', TextValues.KeyID, 64, 64, HexDigitsSet) or
     not ConsumeLineValue(SS, 'public-x', TextValues.Public_x, 64, 64, HexDigitsSet) or
     not ConsumeLineValue(SS, 'public-y', TextValues.Public_y, 64, 64, HexDigitsSet) then
    Exit;
  if HasPrivateKey then
    if not ConsumeLineValue(SS, 'private-d', TextValues.Private_d, 64, 64, HexDigitsSet) then
      Exit;
  if not SS.ReachedEnd then
    Exit;

  APrivateKey.Clear;  { just because Private_d isn't always set }
  APrivateKey.PublicKey.Public_x := ECDSAInt256FromString(TextValues.Public_x);
  APrivateKey.PublicKey.Public_y := ECDSAInt256FromString(TextValues.Public_y);

  { Verify that the key ID is correct for the public key values }
  if not SHA256DigestsEqual(SHA256DigestFromString(TextValues.KeyID),
     CalcKeyID(APrivateKey.PublicKey)) then
    Exit;

  if ANeedPrivateKey then begin
    if not HasPrivateKey then
      Exit(ikrNotPrivateKey);
    APrivateKey.Private_d := ECDSAInt256FromString(TextValues.Private_d);
  end;
  Result := ikrSuccess;
end;

function ISSigParsePrivateKeyText(const AText: String;
  out APrivateKey: TECDSAPrivateKey): TISSigImportKeyResult;
begin
  Result := InternalParseKeyText(AText, APrivateKey, True);
end;

function ISSigParsePublicKeyText(const AText: String;
  out APublicKey: TECDSAPublicKey): TISSigImportKeyResult;
begin
  var PrivateKey: TECDSAPrivateKey;  { only PublicKey part is used }
  Result := InternalParseKeyText(AText, PrivateKey, False);
  if Result = ikrSuccess then
    APublicKey := PrivateKey.PublicKey;
end;

function ISSigImportKeyText(const AKey: TECDSAKey; const AText: String;
  const ANeedPrivateKey: Boolean): TISSigImportKeyResult;
begin
  var PrivateKey: TECDSAPrivateKey;
  try
    Result := InternalParseKeyText(AText, PrivateKey, ANeedPrivateKey);
    if Result = ikrSuccess then begin
      if ANeedPrivateKey then
        AKey.ImportPrivateKey(PrivateKey)
      else
        AKey.ImportPublicKey(PrivateKey.PublicKey);
    end;
  finally
    PrivateKey.Clear;
  end;
end;

function ISSigImportPublicKey(const AKey: TECDSAKey;
  const AKeyID, APublicX, APublicY: String): TISSigImportKeyResult;
begin
  var Publickey: TECDSAPublickey;
  PublicKey.Public_x := ECDSAInt256FromString(APublicX);
  PublicKey.Public_y := ECDSAInt256FromString(APublicY);

  if AKeyID <> '' then begin
    { Verify that the key ID is correct for the public key values }
    if not SHA256DigestsEqual(SHA256DigestFromString(AKeyID),
       CalcKeyID(PublicKey)) then
      Exit(ikrMalformed);
  end;

  AKey.ImportPublicKey(PublicKey);
  Result := ikrSuccess;
end;

procedure ISSigCheckValidKeyID(const AKeyID: String);
begin
  SHA256DigestFromString(AKeyID);
end;

procedure ISSigCheckValidPublicXOrY(const APublicXOrY: String);
begin
  ECDSAInt256FromString(APublicXOrY);
end;

function ISSigIsValidKeyIDForPublicXY(const AKeyID, APublicX, APublicY: String): Boolean;
begin
  var PublicKey: TECDSAPublicKey;
  PublicKey.Public_x := ECDSAInt256FromString(APublicX);
  PublicKey.Public_y := ECDSAInt256FromString(APublicY);

  Result := SHA256DigestsEqual(SHA256DigestFromString(AKeyID),
     CalcKeyID(PublicKey));
end;

function ISSigCalcStreamHash(const AStream: TStream): TSHA256Digest;
var
  Buf: array[0..$FFFF] of Byte;
begin
  var Context: TSHA256Context;
  SHA256Init(Context);
  while True do begin
    const BytesRead = Cardinal(AStream.Read(Buf, SizeOf(Buf)));
    if BytesRead = 0 then
      Break;
    SHA256Update(Context, Buf, BytesRead);
  end;
  Result := SHA256Final(Context);
end;

end.
