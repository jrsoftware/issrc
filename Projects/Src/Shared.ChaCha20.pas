unit Shared.ChaCha20;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  ChaCha20 encryption/decryption

  Based on https://github.com/marcizhu/ChaCha20/blob/master/ChaCha20.h
  Copyright (c) 2022 Marc Izquierdo
  MIT License
}

interface

uses
  Windows;

type
  TChaChaCtx = array[0..15] of Cardinal;

  TChaChaContext = record
    ctx, keystream: TChaChaCtx;
  end;

procedure ChaCha20Init(var Context: TChaChaContext; const Key;
 const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal;
 const Count: Cardinal);
procedure ChaCha20Crypt(var Context: TChaChaContext; const InBuffer;
  var OutBuffer; Length: Cardinal);

implementation

uses
  SysUtils, Math;

procedure ChaCha20Init(var Context: TChaChaContext; const Key;
  const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal;
  const Count: Cardinal);
begin
  Assert(KeyLength = 32);
  Assert((NonceLength = 0) or (NonceLength = 12));
  {$IFDEF DEBUG}
  ZeroMemory(@Context, SizeOf(Context));
  {$ENDIF}
  Context.ctx[0] := $61707865;
  Context.ctx[1] := $3320646e;
  Context.ctx[2] := $79622d32;
  Context.ctx[3] := $6b206574;
  Move(Key, Context.ctx[4], KeyLength);
  Context.ctx[12] := Count;
  if NonceLength = 12 then
    Move(Nonce, Context.ctx[13], NonceLength)
  else
    ZeroMemory(@Context.ctx[13], 12);
end;

procedure ChaCha20Crypt(var Context: TChaChaContext; const InBuffer;
  var OutBuffer; Length: Cardinal);

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

  procedure ChaCha20BlockNext(var ctx, keystream: TChaChaCtx);
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

    for var i := 0 to 15 do
      keystream[i] := keystream[i] + ctx[i];

    Assert(ctx[12] < High(Cardinal));
    ctx[12] := ctx[12] + 1;
  end;

begin
  var InBuf: PByte := @InBuffer;
  var OutBuf: PByte := @OutBuffer;
  while Length > 0 do begin
    ChaCha20BlockNext(Context.ctx, Context.keystream);

    var KeyStream: PByte := @Context.keystream;
    var BlockSize := Min(Length, 64);
    for var I := 0 to  BlockSize - 1 do
      OutBuf[I] := InBuf[I] xor KeyStream[i];

    InBuf := PByte(NativeUInt(InBuf)+BlockSize);
    OutBuf := PByte(NativeUInt(OutBuf)+BlockSize);
    Dec(Length, BlockSize);
  end;
end;

end.
