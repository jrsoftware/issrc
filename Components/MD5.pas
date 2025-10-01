unit MD5;

{
  MD5.pas: System.Hash.pas wrapper in the style of the old version of this file:

  MD5.pas: Translated from C to Delphi by Jordan Russell on 2004-03-16.
  Still in the public domain. The original C code was taken from dpkg.
}

(*
 * This code implements the MD5 message-digest algorithm.
 * The algorithm is due to Ron Rivest.  This code was
 * written by Colin Plumb in 1993, no copyright is claimed.
 * This code is in the public domain; do with it what you wish.
 *
 * Equivalent code is available from RSA Data Security, Inc.
 * This code has been tested against that, and is equivalent,
 * except that you don't need to include two pages of legalese
 * with every copy.
 *
 * To compute the message digest of a chunk of bytes, declare an
 * MD5Context structure, pass it to MD5Init, call MD5Update as
 * needed on buffers full of bytes, and then call MD5Final, which
 * will fill a supplied 16-byte array with the digest.
 *
 * Changed so as no longer to depend on Colin Plumb's `usual.h' header
 * definitions; now uses stuff from dpkg's config.h.
 *  - Ian Jackson <ian@chiark.greenend.org.uk>.
 * Still in the public domain.
 *)

interface

uses
  System.Hash;

type
  TMD5Context = record
    hash: THashMD5;
  end;
  TMD5Digest = array[0..15] of Byte;

procedure MD5Init(var ctx: TMD5Context);
procedure MD5Update(var ctx: TMD5Context; const buffer; len: Cardinal);
function MD5Final(var ctx: TMD5Context): TMD5Digest;

function MD5Buf(const Buffer; Len: Cardinal): TMD5Digest;
function MD5DigestsEqual(const A, B: TMD5Digest): Boolean;
function MD5DigestToString(const D: TMD5Digest): String;

implementation

procedure MD5Init(var ctx: TMD5Context);
begin
  ctx.hash := THashMD5.Create;
end;

procedure MD5Update(var ctx: TMD5Context; const buffer; len: Cardinal);
begin
  ctx.hash.Update(buffer, len);
end;

function MD5Final(var ctx: TMD5Context): TMD5Digest;
begin
  var HashAsBytes := ctx.hash.HashAsBytes;
  Move(HashAsBytes[0], Result[0], SizeOf(Result));
end;

{ New functions by JR: }

function MD5Buf(const Buffer; Len: Cardinal): TMD5Digest;
var
  Context: TMD5Context;
begin
  MD5Init(Context);
  MD5Update(Context, Buffer, Len);
  Result := MD5Final(Context);
end;

function MD5DigestsEqual(const A, B: TMD5Digest): Boolean;
var
  I: Integer;
begin
  for I := Low(TMD5Digest) to High(TMD5Digest) do
    if A[I] <> B[I] then begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

function MD5DigestToString(const D: TMD5Digest): String;
const
  Digits: array[0..15] of Char = '0123456789abcdef';
var
  Buf: array[0..31] of Char;
  P: PChar;
  I: Integer;
begin
  P := @Buf[0];
  for I := 0 to 15 do begin
    P^ := Digits[D[I] shr 4];
    Inc(P);
    P^ := Digits[D[I] and 15];
    Inc(P);
  end;
  SetString(Result, Buf, 32);
end;

end.
