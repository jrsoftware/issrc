unit SHA1.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for SHA1

  Runs a self-test if DEBUG is defined
}

interface

procedure SHA1RunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, {$ENDIF} System.SysUtils, SHA1, UnsignedFunc;

{$C+}

procedure SHA1RunTests;

  procedure Test(const S: AnsiString; const ExpectedHex: String);
  begin
    var Digest: TSHA1Digest;
    if S = '' then
      Digest := SHA1Buf(Pointer(nil)^, 0)
    else
      Digest := SHA1Buf(S[1], ULength(S)*SizeOf(S[1]));
    Assert(SameText(SHA1DigestToString(Digest), ExpectedHex));
  end;

begin
  //https://datatracker.ietf.org/doc/html/rfc3174 - test vectors plus other well-known values
  Test('', 'da39a3ee5e6b4b0d3255bfef95601890afd80709');
  Test('a', '86f7e437faa5a7fce15d1ddcb9eaeaea377667b8');
  Test('abc', 'a9993e364706816aba3e25717850c26c9cd0d89d');
  Test('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq',
       '84983e441c3bd26ebaae4aa1f95129e5e54670f1');

  { Streamed update should match one-shot hashing }
  var Ctx: TSHA1Context;
  SHA1Init(Ctx);
  const S: AnsiString = 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq';
  SHA1Update(Ctx, S[1], 28);
  SHA1Update(Ctx, S[29], ULength(S) - 28);
  const Streamed = SHA1Final(Ctx);
  const OneShot = SHA1Buf(S[1], ULength(S));
  Assert(SHA1DigestsEqual(Streamed, OneShot));

  { SHA1DigestsEqual must return False when digests differ }
  var Different := OneShot;
  Different[0] := Different[0] xor $01;
  Assert(not SHA1DigestsEqual(OneShot, Different));
end;

{$IFDEF DEBUG}
initialization
  try
    SHA1RunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}

end.
