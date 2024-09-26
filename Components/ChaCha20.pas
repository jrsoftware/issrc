unit ChaCha20;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  ChaCha20 and XChaCha20 encryption/decryption

  Initially based on https://github.com/Ginurx/chacha20-c/tree/master
}

interface

type
  TChaCha20Ctx = array[0..15] of Cardinal;

  TChaCha20Context = record
    ctx, keystream: TChaCha20Ctx;
    position: 0..64;
    count64: Boolean;
  end;

procedure ChaCha20Init(var Context: TChaCha20Context; const Key;
 const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal;
 const Count: Cardinal);
procedure ChaCha20Crypt(var Context: TChaCha20Context; const InBuffer;
  var OutBuffer; const Length: Cardinal);

procedure XChaCha20Init(var Context: TChaCha20Context; const Key;
 const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal;
 const Count: Cardinal);
procedure XChaCha20Crypt(var Context: TChaCha20Context; const InBuffer;
  var OutBuffer; const Length: Cardinal);

implementation

uses
  System.SysUtils;

{$C+}

procedure ChaCha20InitCtx(var ctx: TChaCha20Ctx; const Key;
  const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal;
  const Count: Cardinal);
begin
  Assert(KeyLength = 32);
  Assert(NonceLength in [0, 8, 12]);
  {$IFDEF DEBUG}
  FillChar(ctx[0], SizeOf(ctx), 1);
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
    FillChar(ctx[13], 12, 0);
end;

procedure ChaCha20Init(var Context: TChaCha20Context; const Key;
  const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal;
  const Count: Cardinal);
begin
  ChaCha20InitCtx(Context.ctx, Key, KeyLength, Nonce, NonceLength, Count);
  Context.position := 64;
  Context.count64 := NonceLength <> 12;
end;

procedure ChaCha20RunRounds(var ctx, keystream: TChaCha20Ctx);

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
  Move(ctx, keystream, SizeOf(ctx));

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

procedure ChaCha20Crypt(var Context: TChaCha20Context; const InBuffer;
  var OutBuffer; const Length: Cardinal);

  procedure ChaCha20BlockNext(var ctx, keystream: TChaCha20Ctx; const count64: Boolean);
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

begin
  if Length = 0 then
    Exit;

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

procedure HChaCha20(const Key; const KeyLength: Cardinal; const Nonce;
  const NonceLength: Cardinal; out SubKey: TBytes);
begin
  Assert(NonceLength = 16);
  var NonceBytes: PByte := @Nonce;
  var ctx: TChaCha20Ctx;
  ChaCha20InitCtx(ctx, Key, KeyLength, NonceBytes[4], 12, PCardinal(NonceBytes)^);
  var keystream: TChaCha20Ctx;
  ChaCha20RunRounds(ctx, keystream);
  SetLength(SubKey, 32);
  Move(keystream[0], SubKey[0], 16);
  Move(keystream[12], SubKey[16], 16);
end;

procedure XChaCha20Init(var Context: TChaCha20Context; const Key;
 const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal;
 const Count: Cardinal);
begin
  Assert(NonceLength = 24);
  var SubKey: TBytes;
  HChaCha20(Key, KeyLength, Nonce, 16, SubKey);
  var NonceBytes: PByte := @Nonce;
  ChaCha20Init(Context, SubKey[0], Length(SubKey), NonceBytes[16], 8, Count);
end;

procedure XChaCha20Crypt(var Context: TChaCha20Context; const InBuffer;
  var OutBuffer; const Length: Cardinal);
begin
  ChaCha20Crypt(Context, InBuffer, OutBuffer, Length);
end;

{.$DEFINE TEST}

{$IFDEF TEST}

procedure TestChaCha20;
begin
  //https://datatracker.ietf.org/doc/html/rfc7539#section-2.4.2
  var Buf: AnsiString := 'Ladies and Gentlemen of the class of ''99: If I could offer you only one tip for the future, sunscreen would be it.';
  var BufSize := Length(Buf)*SizeOf(Buf[1]);
  var Key: TBytes := [$00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b, $0c, $0d, $0e, $0f, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1a, $1b, $1c, $1d, $1e, $1f];
  var Nonce: TBytes := [$00, $00, $00, $00, $00, $00, $00, $4a, $00, $00, $00, $00];
  var Counter := 1;
  var Ctx: TChaCha20Context;

  ChaCha20Init(Ctx, Key[0], Length(Key), Nonce[0], Length(Nonce), Counter);
  ChaCha20Crypt(Ctx, Buf[1], Buf[1], 10);
  ChaCha20Crypt(Ctx, Buf[11], Buf[11], BufSize-10);

  var CipherText: TBytes := [$6e, $2e, $35, $9a, $25, $68, $f9, $80, $41, $ba, $07, $28, $dd, $0d, $69, $81, $e9, $7e, $7a, $ec, $1d, $43, $60, $c2, $0a, $27, $af, $cc, $fd, $9f, $ae, $0b, $f9, $1b, $65, $c5, $52, $47, $33, $ab, $8f, $59, $3d, $ab, $cd, $62, $b3, $57, $16, $39, $d6, $24, $e6, $51, $52, $ab, $8f, $53, $0c, $35, $9f, $08, $61, $d8, $07, $ca, $0d, $bf, $50, $0d, $6a, $61, $56, $a3, $8e, $08, $8a, $22, $b6, $5e, $52, $bc, $51, $4d, $16, $cc, $f8, $06, $81, $8c, $e9, $1a, $b7, $79, $37, $36, $5a, $f9, $0b, $bf, $74, $a3, $5b, $e6, $b4, $0b, $8e, $ed, $f2, $78, $5e, $42, $87, $4d];

  Assert(Length(Buf) = Length(CipherText));
  for var I := 0 to Length(Buf)-1 do
    Assert(Byte(Buf[I+1]) = CipherText[I]);
end;

procedure TestHChaCha20;
begin
  //https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-xchacha-03#section-2.2.1
  var Key: TBytes := [$00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b, $0c, $0d, $0e, $0f, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1a, $1b, $1c, $1d, $1e, $1f];
  var Nonce: TBytes := [$00, $00, $00, $09, $00, $00, $00, $4a, $00, $00, $00, $00, $31, $41, $59, $27];
  var SubKey: TBytes;

  HChaCha20(Key[0], Length(Key), Nonce[0], Length(Nonce), SubKey);

  var ExpectedSubKey: TBytes := [$82, $41, $3b, $42, $27, $b2, $7b, $fe, $d3, $0e, $42, $50, $8a, $87, $7d, $73, $a0, $f9, $e4, $d5, $8a, $74, $a8, $53, $c1, $2e, $c4, $13, $26, $d3, $ec, $dc];

  Assert(Length(SubKey) = Length(ExpectedSubKey));
  for var I := 0 to Length(SubKey)-1 do
    Assert(SubKey[I] = ExpectedSubKey[I]);
end;

procedure TestXChaCha20;
begin
  //https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-xchacha-03#appendix-A.2
  var Buf: AnsiString := 'The dhole (pronounced "dole") is also known as the Asiatic wild dog, red dog, and whistling dog.'+' It is about the size of a German shepherd but looks more like a long-legged fox. This highly elusive and skilled jumper is classified with wolves, coyotes, jackals, and foxes in the taxonomic family Canidae.';
  var BufSize := Length(Buf)*SizeOf(Buf[1]);
  var Key: TBytes := [$80, $81, $82, $83, $84, $85, $86, $87, $88, $89, $8a, $8b, $8c, $8d, $8e, $8f, $90, $91, $92, $93, $94, $95, $96, $97, $98, $99, $9a, $9b, $9c, $9d, $9e, $9f];
  var Nonce: TBytes := [$40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4a, $4b, $4c, $4d, $4e, $4f, $50, $51, $52, $53, $54, $55, $56, $58];
  var Counter := 0;
  var Ctx: TChaCha20Context;

  XChaCha20Init(Ctx, Key[0], Length(Key), Nonce[0], Length(Nonce), Counter);
  XChaCha20Crypt(Ctx, Buf[1], Buf[1], BufSize);

  var CipherText: TBytes := [$45, $59, $ab, $ba, $4e, $48, $c1, $61, $02, $e8, $bb, $2c, $05, $e6, $94, $7f, $50, $a7, $86, $de, $16, $2f, $9b, $0b, $7e, $59, $2a, $9b, $53, $d0, $d4, $e9, $8d, $8d, $64, $10, $d5, $40, $a1, $a6, $37, $5b, $26, $d8, $0d, $ac, $e4, $fa, $b5, $23, $84, $c7, $31, $ac, $bf, $16, $a5, $92, $3c, $0c, $48, $d3, $57, $5d, $4d, $0d, $2c, $67, $3b, $66, $6f, $aa, $73, $10, $61, $27, $77, $01, $09, $3a, $6b, $f7, $a1, $58, $a8, $86, $42, $92, $a4, $1c, $48, $e3, $a9, $b4, $c0, $da, $ec, $e0, $f8, $d9, $8d, $0d, $7e, $05, $b3, $7a, $30, $7b, $bb, $66, $33, $31, $64, $ec, $9e, $1b, $24, $ea, $0d, $6c, $3f, $fd, $dc, $ec, $4f, $68, $e7, $44, $30, $56, $19, $3a, $03, $c8, $10, $e1, $13, $44, $ca, $06, $d8, $ed, $8a, $2b, $fb, $1e, $8d, $48, $cf, $a6, $bc, $0e, $b4, $e2, $46, $4b, $74, $81, $42, $40, $7c, $9f, $43, $1a, $ee, $76, $99, $60, $e1, $5b, $a8, $b9, $68, $90, $46, $6e, $f2, $45, $75, $99, $85, $23, $85, $c6, $61, $f7, $52, $ce, $20, $f9, $da, $0c, $09, $ab, $6b, $19, $df, $74, $e7, $6a, $95, $96, $74, $46, $f8, $d0, $fd, $41, $5e, $7b, $ee, $2a, $12, $a1, $14, $c2, $0e, $b5, $29, $2a, $e7, $a3, $49, $ae, $57, $78, $20, $d5, $52, $0a, $1f, $3f, $b6, $2a, $17, $ce, $6a, $7e, $68, $fa, $7c, $79, $11, $1d, $88, $60, $92, $0b, $c0, $48, $ef, $43, $fe, $84, $48, $6c, $cb, $87, $c2, $5f, $0a, $e0, $45, $f0, $cc, $e1, $e7, $98, $9a, $9a, $a2, $20, $a2, $8b, $dd, $48, $27, $e7, $51, $a2, $4a, $6d, $5c, $62, $d7, $90, $a6, $63, $93, $b9, $31, $11, $c1, $a5, $5d, $d7, $42, $1a, $10, $18, $49, $74, $c7, $c5];

  Assert(Length(Buf) = Length(CipherText));
  for var I := 0 to Length(Buf)-1 do
    Assert(Byte(Buf[I+1]) = CipherText[I]);
end;

initialization
  TestChaCha20;
  TestHChaCha20;
  TestXChaCha20;

{$ENDIF}

end.
