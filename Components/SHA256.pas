unit SHA256;

{
  SHA256.pas: System.Hash.pas wrapper in the style of MD5.pas and SHA1.pas 
  Author: Martijn Laan
  License for SHA256.pas: Public domain, no copyright claimed
}

interface

uses
  System.Hash;

type
  TSHA256Context = record
    hash: THashSHA2;
  end;
  TSHA256Digest = array[0..31] of Byte;

procedure SHA256Init(var ctx: TSHA256Context);
procedure SHA256Update(var ctx: TSHA256Context; const buffer; len: Cardinal);
function SHA256Final(var ctx: TSHA256Context): TSHA256Digest;

function SHA256Buf(const Buffer; Len: Cardinal): TSHA256Digest;
function SHA256DigestsEqual(const A, B: TSHA256Digest): Boolean;
function SHA256DigestToString(const D: TSHA256Digest): String;

implementation

procedure SHA256Init(var ctx: TSHA256Context);
begin
  ctx.hash := THashSHA2.Create(THashSHA2.TSHA2Version.SHA256);
end;

procedure SHA256Update(var ctx: TSHA256Context; const buffer; len: Cardinal);
begin
  ctx.hash.Update(buffer, len);
end;

function SHA256Final(var ctx: TSHA256Context): TSHA256Digest;
begin
  var HashAsBytes := ctx.hash.HashAsBytes;
  Move(HashAsBytes[0], Result[0], SizeOf(Result));
end;

function SHA256Buf(const Buffer; Len: Cardinal): TSHA256Digest;
var
  Context: TSHA256Context;
begin
  SHA256Init(Context);
  SHA256Update(Context, Buffer, Len);
  Result := SHA256Final(Context);
end;

function SHA256DigestsEqual(const A, B: TSHA256Digest): Boolean;
var
  I: Integer;
begin
  for I := Low(TSHA256Digest) to High(TSHA256Digest) do
    if A[I] <> B[I] then begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

function SHA256DigestToString(const D: TSHA256Digest): String;
const
  Digits: array[0..15] of Char = '0123456789abcdef';
var
  Buf: array[0..63] of Char;
  P: PChar;
  I: Integer;
begin
  P := @Buf;
  for I := 0 to 31 do begin
    P^ := Digits[D[I] shr 4];
    Inc(P);
    P^ := Digits[D[I] and 15];
    Inc(P);
  end;
  SetString(Result, Buf, 64);
end;

end.
