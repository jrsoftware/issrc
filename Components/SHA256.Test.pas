unit SHA256.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for SHA256

  Runs a self-test if DEBUG is defined
}

interface

procedure SHA256RunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, {$ENDIF} System.SysUtils, SHA256, UnsignedFunc;

{$C+}

procedure SHA256RunTests;

  procedure Test(const S: AnsiString; const ExpectedHex: String);
  begin
    var Digest: TSHA256Digest;
    if S = '' then
      Digest := SHA256Buf(Pointer(nil)^, 0)
    else
      Digest := SHA256Buf(S[1], ULength(S)*SizeOf(S[1]));
    Assert(SameText(SHA256DigestToString(Digest), ExpectedHex));
  end;

begin
  //https://csrc.nist.gov/csrc/media/projects/cryptographic-standards-and-guidelines/documents/examples/sha_all.pdf
  Test('', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855');
  Test('abc', 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad');
  Test('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq',
       '248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1');

  { Streamed update should match one-shot hashing }
  var Ctx: TSHA256Context;
  SHA256Init(Ctx);
  const S: AnsiString = 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq';
  SHA256Update(Ctx, S[1], 28);
  SHA256Update(Ctx, S[29], ULength(S) - 28);
  const Streamed = SHA256Final(Ctx);
  const OneShot = SHA256Buf(S[1], ULength(S));
  Assert(SHA256DigestsEqual(Streamed, OneShot));

  { SHA256DigestsEqual must return False when digests differ }
  var Different := OneShot;
  Different[0] := Different[0] xor $01;
  Assert(not SHA256DigestsEqual(OneShot, Different));

  { SHA256DigestToString/SHA256DigestFromString round trip - mixed case input must parse }
  const HexUpper = 'BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD';
  const HexLower = 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad';
  const FromUpper = SHA256DigestFromString(HexUpper);
  const FromLower = SHA256DigestFromString(HexLower);
  Assert(SHA256DigestsEqual(FromUpper, FromLower));
  Assert(SameText(SHA256DigestToString(FromLower), HexLower));

  {$IFDEF ISTESTTOOLPROJ}
  { Invalid lengths and characters must raise }
  var Caught := False;
  try
    SHA256DigestFromString('abc');
  except
    on EConvertError do Caught := True;
  end;
  Assert(Caught);

  Caught := False;
  try
    SHA256DigestFromString('ZZ7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad');
  except
    on EConvertError do Caught := True;
  end;
  Assert(Caught);
  {$ENDIF}
end;

{$IFDEF DEBUG}
initialization
  try
    SHA256RunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}

end.
