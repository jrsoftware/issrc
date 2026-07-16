unit SHA1;

{
  SHA1.pas: System.Hash.pas wrapper in the style of the old version of this file:

  SHA1.pas: SHA-1 hash implementation, based on RFC 3174 and MD5.pas
  Author: Jordan Russell, 2010-02-24
  License for SHA1.pas: Public domain, no copyright claimed
}

interface

uses
  System.Hash;

type
  TSHA1Context = record
    hash: THashSHA1;
  end;
  TSHA1Digest = array[0..19] of Byte;

procedure SHA1Init(var ctx: TSHA1Context);
procedure SHA1Update(var ctx: TSHA1Context; const buffer; len: Cardinal);
function SHA1Final(var ctx: TSHA1Context): TSHA1Digest;

function SHA1Buf(const Buffer; Len: Cardinal): TSHA1Digest;
function SHA1DigestsEqual(const A, B: TSHA1Digest): Boolean;
function SHA1DigestToString(const D: TSHA1Digest): String;

implementation

procedure SHA1Init(var ctx: TSHA1Context);
begin
  ctx.hash := THashSHA1.Create;
end;

procedure SHA1Update(var ctx: TSHA1Context; const buffer; len: Cardinal);
begin
  ctx.hash.Update(buffer, len);
end;

function SHA1Final(var ctx: TSHA1Context): TSHA1Digest;
begin
  var HashAsBytes := ctx.hash.HashAsBytes;
  Move(HashAsBytes[0], Result[0], SizeOf(Result));
end;

{ New functions by JR: }

function SHA1Buf(const Buffer; Len: Cardinal): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, Buffer, Len);
  Result := SHA1Final(Context);
end;

function SHA1DigestsEqual(const A, B: TSHA1Digest): Boolean;
var
  I: Integer;
begin
  for I := Low(TSHA1Digest) to High(TSHA1Digest) do
    if A[I] <> B[I] then begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

function SHA1DigestToString(const D: TSHA1Digest): String;
const
  Digits: array[0..15] of Char = '0123456789abcdef';
var
  Buf: array[0..39] of Char;
  P: PChar;
  I: Integer;
begin
  P := @Buf[0];
  for I := 0 to 19 do begin
    P^ := Digits[D[I] shr 4];
    Inc(P);
    P^ := Digits[D[I] and 15];
    Inc(P);
  end;
  SetString(Result, Buf, 40);
end;

end.
