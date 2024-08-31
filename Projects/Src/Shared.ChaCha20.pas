unit Shared.ChaCha20;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  ChaCha20 and XChaCha20 encryption/decryption

  Initially based on https://github.com/Ginurx/chacha20-c/tree/master
}

interface

uses
  Windows;

type
  TChaChaCtx = array[0..15] of Cardinal;

  TChaChaContext = record
    ctx, keystream: TChaChaCtx;
    position: 0..64;
    count64: Boolean;
  end;

procedure ChaCha20Init(var Context: TChaChaContext; const Key;
 const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal;
 const Count: Cardinal);
procedure ChaCha20Crypt(var Context: TChaChaContext; const InBuffer;
  var OutBuffer; const Length: Cardinal);

procedure XChaCha20Init(var Context: TChaChaContext; const Key;
 const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal;
 const Count: Cardinal);
procedure XChaCha20Crypt(var Context: TChaChaContext; const InBuffer;
  var OutBuffer; const Length: Cardinal);

implementation

uses
  SysUtils, Math;

{$C+}

procedure ChaCha20InitCtx(var ctx: TChaChaCtx; const Key;
  const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal;
  const Count: Cardinal);
begin
  Assert(KeyLength = 32);
  Assert(NonceLength in [0, 8, 12]);
  {$IFDEF DEBUG}
  ZeroMemory(@Ctx, SizeOf(Ctx));
  {$ENDIF}
  ctx[0] := $61707865;
  ctx[1] := $3320646e;
  ctx[2] := $79622d32;
  ctx[3] := $6b206574;
  Move(Key, ctx[4], KeyLength);
  ctx[12] := Count;
  if NonceLength = 12 then
    Move(Nonce, ctx[13], 12)
  else if NonceLength = 8 then begin
    ctx[13] := 0;
    Move(Nonce, ctx[14], 8)
  end else
    ZeroMemory(@ctx[13], 12);
end;

procedure ChaCha20Init(var Context: TChaChaContext; const Key;
  const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal;
  const Count: Cardinal);
begin
  ChaCha20InitCtx(Context.ctx, Key, KeyLength, Nonce, NonceLength, Count);
  Context.position := 64;
  Context.count64 := NonceLength <> 12;
end;

procedure ChaCha20RunRounds(var ctx, keystream: TChaChaCtx);

  function ROTL(const x: Cardinal; const n: Byte): Cardinal;
  begin
    Result := (x shl n) or (x shr (32 - n));
  end;

  procedure CHACHA20_QR(var a, b, c, d: Cardinal);
  begin
    Inc(a, b); d := d xor a; d := ROTL(d, 16);
    Inc(c, d); b := b xor c; b := ROTL(b, 12);
    Inc(a, b); d := d xor a; d := ROTL(d, 8);
    Inc(c, d); b := b xor c; b := ROTL(b, 7);
  end;

begin
  for var i := 0 to 15 do
    keystream[i] := ctx[i];

  for var i := 0 to 9 do begin
    CHACHA20_QR(keystream[0], keystream[4], keystream[8], keystream[12]);  // column 0
    CHACHA20_QR(keystream[1], keystream[5], keystream[9], keystream[13]);  // column 1
    CHACHA20_QR(keystream[2], keystream[6], keystream[10], keystream[14]); // column 2
    CHACHA20_QR(keystream[3], keystream[7], keystream[11], keystream[15]); // column 3
    CHACHA20_QR(keystream[0], keystream[5], keystream[10], keystream[15]); // diagonal 1 (main diagonal)
    CHACHA20_QR(keystream[1], keystream[6], keystream[11], keystream[12]); // diagonal 2
    CHACHA20_QR(keystream[2], keystream[7], keystream[8], keystream[13]);  // diagonal 3
    CHACHA20_QR(keystream[3], keystream[4], keystream[9], keystream[14]);  // diagonal 4
  end;
end;

procedure ChaCha20BlockNext(var ctx, keystream: TChaChaCtx; const count64: Boolean);
begin
  ChaCha20RunRounds(ctx, keystream);

  for var i := 0 to 15 do
    keystream[i] := keystream[i] + ctx[i];

  if count64 then begin
    if ctx[12] < High(Cardinal) then
      ctx[12] := ctx[12] + 1
    else begin
      ctx[12] := 0;
      Assert(ctx[13] < High(Cardinal));
      ctx[13] := ctx[13] + 1;
    end;
  end else begin
    Assert(ctx[12] < High(Cardinal));
    ctx[12] := ctx[12] + 1;
  end;
end;

procedure ChaCha20Crypt(var Context: TChaChaContext; const InBuffer;
  var OutBuffer; const Length: Cardinal);
begin
  var InBuf: PByte := @InBuffer;
  var OutBuf: PByte := @OutBuffer;
  var KeyStream: PByte := @Context.keystream;

  for var I := 0 to Length-1 do begin
    if Context.position >= 64 then begin
      ChaCha20BlockNext(Context.ctx, Context.keystream, Context.count64);
      Context.position := 0;
    end;
    OutBuf[I] := InBuf[I] xor KeyStream[Context.position];
    Inc(Context.position);
  end;
end;

procedure HChaCha20(const Key; const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal);
begin
  Assert(KeyLength = 32);
  Assert(NonceLength = 16);
  var NonceBytes: PByte := @Nonce;
  var ctx: TChaChaCtx;
  ChaCha20InitCtx(ctx, Key, KeyLength, NonceBytes[4], 12, PCardinal(NonceBytes)^);
  var keystream: TChaChaCtx;
  ChaCha20RunRounds(ctx, keystream);
  //return only the first and last rows of keystream, in little endian, resulting in a 256-bit key
end;

procedure XChaCha20Init(var Context: TChaChaContext; const Key;
 const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal;
 const Count: Cardinal);
begin
  Assert(NonceLength = 24);
  HChaCha20(Key, KeyLength, Nonce, 16);
  //..
end;

procedure XChaCha20Crypt(var Context: TChaChaContext; const InBuffer;
  var OutBuffer; const Length: Cardinal);
begin
  ChaCha20Crypt(Context, InBuffer, OutBuffer, Length);
end;

{$DEFINE TEST}

{$IFDEF TEST}
initialization
  var Buf: AnsiString := 'Ladies and Gentlemen of the class of ''99: If I could offer you only one tip for the future, sunscreen would be it.';
  var BufSize := Length(Buf)*SizeOf(Buf[1]);
  var Key: TBytes := [$00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b, $0c, $0d, $0e, $0f, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1a, $1b, $1c, $1d, $1e, $1f];
  var Nonce: TBytes := [$00, $00, $00, $00, $00, $00, $00, $4a, $00, $00, $00, $00];
  var Counter := 1;
  var Ctx: TChaChaContext;
  var CipherText: TBytes := [$6e, $2e, $35, $9a, $25, $68, $f9, $80, $41, $ba, $07, $28, $dd, $0d, $69, $81, $e9, $7e, $7a, $ec, $1d, $43, $60, $c2, $0a, $27, $af, $cc, $fd, $9f, $ae, $0b, $f9, $1b, $65, $c5, $52, $47, $33, $ab, $8f, $59, $3d, $ab, $cd, $62, $b3, $57, $16, $39, $d6, $24, $e6, $51, $52, $ab, $8f, $53, $0c, $35, $9f, $08, $61, $d8, $07, $ca, $0d, $bf, $50, $0d, $6a, $61, $56, $a3, $8e, $08, $8a, $22, $b6, $5e, $52, $bc, $51, $4d, $16, $cc, $f8, $06, $81, $8c, $e9, $1a, $b7, $79, $37, $36, $5a, $f9, $0b, $bf, $74, $a3, $5b, $e6, $b4, $0b, $8e, $ed, $f2, $78, $5e, $42, $87, $4d];

  ChaCha20Init(Ctx, Key[0], Length(Key), Nonce[0], Length(Nonce), Counter);
  ChaCha20Crypt(Ctx, Buf[1], Buf[1], 10);
  ChaCha20Crypt(Ctx, Buf[11], Buf[11], BufSize-10);
  Assert(Length(Buf) = Length(CipherText));
  for var I := 0 to Length(Buf)-1 do
    Assert(Buf[I+1] = AnsiChar(CipherText[I]));

  Key := [$00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b, $0c, $0d, $0e, $0f, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1a, $1b, $1c, $1d, $1e, $1f];
  Nonce := [$00, $00, $00, $09, $00, $00, $00, $4a, $00, $00, $00, $00, $31, $41, $59, $27];
  HChaCha20(Key[0], Length(Key), Nonce[0], Length(Nonce));
  //82413b42 27b27bfe d30e4250 8a877d73 a0f9e4d5 8a74a853 c12ec413 26d3ecdc

  Key := [$80, $81, $82, $83, $84, $85, $86, $87, $88, $89, $8a, $8b, $8c, $8d, $8e, $8f, $90, $91, $92, $93, $94, $95, $96, $97, $98, $99, $9a, $9b, $9c, $9d, $9e, $9f];
  Nonce := [$40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4a, $4b, $4c, $4d, $4e, $4f, $50, $51, $52, $53, $54, $55, $56, $58];
  XChaCha20Init(Ctx, Key[0], Length(Key), Nonce[0], Length(Nonce), Counter);

{$ENDIF}

end.
