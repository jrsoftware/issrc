unit Shared.EncryptionFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Encryption functions used by ISCmplr, Setup, and SetupLdr
}

interface

uses
  ChaCha20, Shared.Struct;

type
  TSpecialCryptContextType = (sccPasswordTest, sccCompressedBlocks1, sccCompressedBlocks2);

procedure GenerateEncryptionKey(const Password: String; const Salt: TSetupKDFSalt;
  const Iterations: Integer; out Key: TSetupEncryptionKey);
procedure InitCryptContext(const CryptKey: TSetupEncryptionKey;
  const EncryptionBaseNonce: TSetupEncryptionNonce; const StartOffset: Int64; const FirstSlice: Int32;
  out CryptContext: TChaCha20Context); overload;
procedure InitCryptContext(const CryptKey: TSetupEncryptionKey;
  const EncryptionBaseNonce: TSetupEncryptionNonce; const Typ: TSpecialCryptContextType;
  out CryptContext: TChaCha20Context); overload;
procedure GeneratePasswordTest(const CryptKey: TSetupEncryptionKey;
  const EncryptionBaseNonce: TSetupEncryptionNonce; out PasswordTest: Integer);
function TestPassword(const CryptKey: TSetupEncryptionKey;
  const EncryptionBaseNonce: TSetupEncryptionNonce; const ExpectedPasswordTest: Integer): Boolean;

implementation

uses
  SysUtils, PBKDF2;

procedure GenerateEncryptionKey(const Password: String; const Salt: TSetupKDFSalt;
  const Iterations: Integer; out Key: TSetupEncryptionKey);
begin
  var SaltBytes: TBytes;
  var SaltSize := SizeOf(Salt);
  SetLength(SaltBytes, SaltSize);
  Move(Salt[0], SaltBytes[0], SaltSize);
  var KeyLength := SizeOf(Key);
  var KeyBytes := PBKDF2SHA256(Password, SaltBytes, Iterations, KeyLength);
  Move(KeyBytes[0], Key[0], KeyLength);
end;

procedure InitCryptContext(const CryptKey: TSetupEncryptionKey;
  const EncryptionBaseNonce: TSetupEncryptionNonce; const StartOffset: Int64; const FirstSlice: Int32;
  out CryptContext: TChaCha20Context);
begin
  { Create the unique nonce from the base nonce }
  var Nonce := EncryptionBaseNonce;
  Nonce.RandomXorStartOffset := Nonce.RandomXorStartOffset xor StartOffset;
  Nonce.RandomXorFirstSlice := Nonce.RandomXorFirstSlice xor FirstSlice;

  XChaCha20Init(CryptContext, CryptKey[0], Length(CryptKey), Nonce, SizeOf(Nonce), 0);
end;

procedure InitCryptContext(const CryptKey: TSetupEncryptionKey;
  const EncryptionBaseNonce: TSetupEncryptionNonce; const Typ: TSpecialCryptContextType;
  out CryptContext: TChaCha20Context); overload;
begin
  const SpecialFirstSlice = -1-(Ord(Typ)-Ord(Low(Typ)));
  InitCryptContext(CryptKey, EncryptionBaseNonce, 0, SpecialFirstSlice, CryptContext);
end;

{ This function assumes CryptKey is based on the password }
procedure GeneratePasswordTest(const CryptKey: TSetupEncryptionKey;
  const EncryptionBaseNonce: TSetupEncryptionNonce; out PasswordTest: Integer);
begin
  var Context: TChaCha20Context;
  InitCryptContext(CryptKey, EncryptionBaseNonce, sccPasswordTest, Context);

  { Encrypt a value of 0 so Setup can do same and compare the results to test the password }
  PasswordTest := 0;
  XChaCha20Crypt(Context, PasswordTest, PasswordTest, SizeOf(PasswordTest));
end;

function TestPassword(const CryptKey: TSetupEncryptionKey;
  const EncryptionBaseNonce: TSetupEncryptionNonce; const ExpectedPasswordTest: Integer): Boolean;
begin
  var ActualPasswordTest: Integer;
  GeneratePasswordTest(CryptKey, EncryptionBaseNonce, ActualPasswordTest);
  Result := ActualPasswordTest = ExpectedPasswordTest;
end;

end.
