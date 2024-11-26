unit PBKDF2;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  PBKDF2-HMAC-SHA256 password-based key derivation
}

interface

uses
  System.SysUtils;

function PBKDF2SHA256(Password: TBytes; const Salt: TBytes; const Iterations, KeyLength: Integer): TBytes; overload;
function PBKDF2SHA256(const Password: String; const Salt: TBytes; const Iterations, KeyLength: Integer): TBytes; overload;

implementation

uses
  System.Hash, System.Math;

function PBKDF2SHA256(Password: TBytes; const Salt: TBytes; const Iterations, KeyLength: Integer): TBytes;
begin
  var HashVersion := THashSHA2.TSHA2Version.SHA256;

  var Hash := THashSHA2.Create(HashVersion); { This is a record so no need to free }
  var HashSize := Hash.GetHashSize;

  if Length(Password) > Hash.GetBlockSize then begin
    { Pre-hash password so THashSHA2.GetHMACAsBytes wont do this over and over again }
    Hash.Update(Password);
    Password := Hash.HashAsBytes;
  end;

  SetLength(Result, KeyLength);
  var BytesDone := 0;

  var L := Ceil(KeyLength / HashSize);

  for var Block := 1 to L do begin
    var SaltAndBlock := Salt + [Byte(Block shr 24), Byte(Block shr 16), Byte(Block shr 8), Byte(Block)];
    var U := THashSHA2.GetHMACAsBytes(SaltAndBlock, Password, HashVersion);
    var F := U;

    for var I := 2 to Iterations do begin
      U := THashSHA2.GetHMACAsBytes(U, Password, HashVersion);
      for var J := 0 to High(F) do
        F[J] := F[J] xor U[J];
    end;

    var BytesLeft := KeyLength - BytesDone;
    var BytesToCopy := Min(BytesLeft, Length(F));
    Move(F[0], Result[BytesDone], BytesToCopy);
    Inc(BytesDone, BytesToCopy);
  end;
end;

function PBKDF2SHA256(const Password: String; const Salt: TBytes; const Iterations, KeyLength: Integer): TBytes;

  function StringToBytes(const S: String): TBytes;
  begin
    var N := Length(S)*SizeOf(S[1]);
    SetLength(Result, N);
    if N > 0 then
      Move(S[1], Result[0], N);
  end;

begin
  Result := PBKDF2SHA256(StringToBytes(Password), Salt, Iterations, KeyLength);
end;

{.$DEFINE TEST}

{$IFDEF TEST}

{$C+}

procedure TestPBKDF2SHA256;

  function AnsiStringToBytes(const S: AnsiString): TBytes;
  begin
    var N := Length(S)*SizeOf(S[1]);
    SetLength(Result, N);
    if N > 0 then
      Move(S[1], Result[0], N);
  end;

  procedure Test(const Key1, Key2: TBytes);
  begin
    Assert(Length(Key1) = Length(Key2));
    for var I := 0 to High(Key1) do
      Assert(Key1[I] = Key2[I]);
  end;

begin
  //https://stackoverflow.com/a/5136918/301485
  var Password := AnsiStringToBytes('password');
  var Salt := AnsiStringToBytes('salt');
  Test(PBKDF2SHA256(Password, Salt, 1, 32),
       [$12, $0f, $b6, $cf, $fc, $f8, $b3, $2c, $43, $e7, $22, $52, $56, $c4, $f8, $37, $a8, $65, $48, $c9, $2c, $cc, $35, $48, $08, $05, $98, $7c, $b7, $0b, $e1, $7b]);
  Test(PBKDF2SHA256(Password, Salt, 4096, 32),
       [$c5, $e4, $78, $d5, $92, $88, $c8, $41, $aa, $53, $0d, $b6, $84, $5c, $4c, $8d, $96, $28, $93, $a0, $01, $ce, $4e, $11, $a4, $96, $38, $73, $aa, $98, $13, $4a]);

  Password := AnsiStringToBytes('passwordPASSWORDpassword');
  Salt := AnsiStringToBytes('saltSALTsaltSALTsaltSALTsaltSALTsalt');
  Test(PBKDF2SHA256(Password, Salt, 4096, 40),
       [$34, $8c, $89, $db, $cb, $d3, $2b, $2f, $32, $d8, $14, $b8, $11, $6e, $84, $cf, $2b, $17, $34, $7e, $bc, $18, $00, $18, $1c, $4e, $2a, $1f, $b8, $dd, $53, $e1, $c6, $35, $51, $8c, $7d, $ac, $47, $e9]);

  Password := AnsiStringToBytes('pass'#0'word');
  Salt := AnsiStringToBytes('sa'#0'lt');
  Test(PBKDF2SHA256(Password, Salt, 4096, 16),
       [$89, $b6, $9d, $05, $16, $f8, $29, $89, $3c, $69, $62, $26, $65, $0a, $86, $87]);

  //https://en.wikipedia.org/wiki/PBKDF2 - but with SHA256 instead of SHA1 - first tested using SHA1
  Password := AnsiStringToBytes('plnlrtfpijpuhqylxbgqiiyipieyxvfsavzgxbbcfusqkozwpngsyejqlmjsytrmd');
  Salt := [$a0, $09, $c1, $a4, $85, $91, $2c, $6a, $e6, $30, $d3, $e7, $44, $24, $0b, $04];
  Test(PBKDF2SHA256(Password, Salt, 1000, 16),
       [$28, $86, $9b, $5f, $31, $ae, $29, $23, $6f, $16, $4c, $5c, $b3, $3e, $2e, $3b]);
end;

initialization
  TestPBKDF2SHA256;

{$ENDIF}

end.