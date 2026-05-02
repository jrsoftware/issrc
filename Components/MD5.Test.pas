unit MD5.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for MD5

  Runs a self-test if DEBUG is defined
}

interface

procedure MD5RunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, {$ENDIF} System.SysUtils, MD5, UnsignedFunc;

{$C+}

procedure MD5RunTests;

  procedure Test(const S: AnsiString; const ExpectedHex: String);
  begin
    var Digest: TMD5Digest;
    if S = '' then
      Digest := MD5Buf(Pointer(nil)^, 0)
    else
      Digest := MD5Buf(S[1], ULength(S)*SizeOf(S[1]));
    Assert(SameText(MD5DigestToString(Digest), ExpectedHex));
  end;

begin
  //https://datatracker.ietf.org/doc/html/rfc1321 - Appendix A.5 test suite
  Test('', 'd41d8cd98f00b204e9800998ecf8427e');
  Test('a', '0cc175b9c0f1b6a831c399e269772661');
  Test('abc', '900150983cd24fb0d6963f7d28e17f72');
  Test('message digest', 'f96b697d7cb7938d525a2f31aaf161d0');
  Test('abcdefghijklmnopqrstuvwxyz', 'c3fcd3d76192e4007dfb496cca67e13b');
  Test('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789',
       'd174ab98d277d9f5a5611c2c9f419d9f');
  Test('12345678901234567890123456789012345678901234567890123456789012345678901234567890',
       '57edf4a22be3c955ac49da2e2107b67a');

  { Streamed update should match one-shot hashing }
  var Ctx: TMD5Context;
  MD5Init(Ctx);
  const S: AnsiString = 'message digest';
  MD5Update(Ctx, S[1], 7);
  MD5Update(Ctx, S[8], ULength(S) - 7);
  const Streamed = MD5Final(Ctx);
  const OneShot = MD5Buf(S[1], ULength(S));
  Assert(MD5DigestsEqual(Streamed, OneShot));

  { MD5DigestsEqual must return False when digests differ }
  var Different := OneShot;
  Different[0] := Different[0] xor $01;
  Assert(not MD5DigestsEqual(OneShot, Different));
end;

{$IFDEF DEBUG}
{$IFNDEF ISTESTTOOLPROJ}
initialization
  try
    MD5RunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}
{$ENDIF}

end.
