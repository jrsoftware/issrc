unit PBKDF2;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  PBKDF2-HMAC-SHA256 password-based key derivation
}

interface

uses
  System.SysUtils;

function PBKDF2SHA256(const Password: TBytes; const Salt: TBytes; const Iterations, KeyLength: Integer): TBytes; overload;
function PBKDF2SHA256(const Password: String; const Salt: TBytes; const Iterations, KeyLength: Integer): TBytes; overload;

implementation

uses
  System.Hash, System.Math;

function PBKDF2SHA256(const Password: TBytes; const Salt: TBytes; const Iterations, KeyLength: Integer): TBytes;
begin
  const HashVersion = THashSHA2.TSHA2Version.SHA256;

  var Hash := THashSHA2.Create(HashVersion); { This is a record so no need to free }
  const HashSize = Hash.GetHashSize;
  var WorkingPassword := Copy(Password);
  try

    if Length(WorkingPassword) > Hash.GetBlockSize then begin
      { Pre-hash password so THashSHA2.GetHMACAsBytes wont do this over and over again }
      Hash.Update(WorkingPassword);
      const NewPassword = Hash.HashAsBytes;
      { Security: don't leave the old copy on the heap }
      FillChar(WorkingPassword[0], Length(WorkingPassword), 0);
      WorkingPassword := NewPassword;
    end;

    SetLength(Result, KeyLength);
    var BytesDone := 0;

    const L = Ceil(KeyLength / HashSize);

    for var Block := 1 to L do begin
      const SaltAndBlock = Salt + [Byte(Block shr 24), Byte(Block shr 16), Byte(Block shr 8), Byte(Block)];
      var U := THashSHA2.GetHMACAsBytes(SaltAndBlock, WorkingPassword, HashVersion);
      var F := Copy(U);

      for var I := 2 to Iterations do begin
        const NewU = THashSHA2.GetHMACAsBytes(U, WorkingPassword, HashVersion);
        { Security: don't leave key derivation intermediates on the heap }
        FillChar(U[0], Length(U), 0);
        U := NewU;
        for var J := 0 to High(F) do
          F[J] := F[J] xor U[J];
      end;

      const BytesLeft = KeyLength - BytesDone;
      const BytesToCopy = Min(BytesLeft, Length(F));
      Move(F[0], Result[BytesDone], BytesToCopy);
      { Security: don't leave key derivation intermediates on the heap }
      FillChar(U[0], Length(U), 0);
      FillChar(F[0], Length(F), 0);
      Inc(BytesDone, BytesToCopy);
    end;
  finally
    { Security: zero the password bytes created by Copy or HashAsBytes }
    if Length(WorkingPassword) > 0 then
      FillChar(WorkingPassword[0], Length(WorkingPassword), 0);
  end;
end;

function PBKDF2SHA256(const Password: String; const Salt: TBytes; const Iterations, KeyLength: Integer): TBytes;

  function StringToBytes(const S: String): TBytes;
  begin
    const N = Length(S)*SizeOf(S[1]);
    SetLength(Result, N);
    if N > 0 then
      Move(S[1], Result[0], N);
  end;

begin
  var PasswordBytes := StringToBytes(Password);
  try
    Result := PBKDF2SHA256(PasswordBytes, Salt, Iterations, KeyLength);
  finally
    { Security: zero the password bytes created by StringToBytes }
    if Length(PasswordBytes) > 0 then
      FillChar(PasswordBytes[0], Length(PasswordBytes), 0);
  end;
end;

end.