unit Shared.EncryptionFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Encryption function used by ISCmplr, Setup, and SetupLdr
}

interface

uses
  Shared.Struct;

procedure GenerateEncryptionKey(const Password: String; const Salt: TSetupKDFSalt;
  const Iterations: Integer; out Key: TSetupEncryptionKey);
function TestPassword(const EncryptionKey: TSetupEncryptionKey;
  const EncryptionBaseNonce: TSetupEncryptionNonce; const ExpectedPasswordTest: Integer): Boolean;

implementation

uses
  SysUtils, ChaCha20, PBKDF2;

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

{ This function assumes EncryptionKey is based on the password }
function TestPassword(const EncryptionKey: TSetupEncryptionKey;
  const EncryptionBaseNonce: TSetupEncryptionNonce; const ExpectedPasswordTest: Integer): Boolean;
begin
  { Do same as compiler did in GeneratePasswordTest and compare results }
  var Nonce := EncryptionBaseNonce;
  Nonce.RandomXorFirstSlice := Nonce.RandomXorFirstSlice xor -1;

  var Context: TChaCha20Context;
  XChaCha20Init(Context, EncryptionKey[0], Length(EncryptionKey), Nonce, SizeOf(Nonce), 0);
  var ActualPasswordTest := 0;
  XChaCha20Crypt(Context, ActualPasswordTest, ActualPasswordTest, SizeOf(ActualPasswordTest));

  Result := ActualPasswordTest = ExpectedPasswordTest;
end;


end.
