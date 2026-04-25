unit ChaCha20;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  ChaCha20 and XChaCha20 encryption/decryption

  Initially based on https://github.com/Ginurx/chacha20-c/tree/master
}

interface

uses
  System.SysUtils;

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

procedure HChaCha20(const Key; const KeyLength: Cardinal; const Nonce;
  const NonceLength: Cardinal; out SubKey: TBytes);

procedure XChaCha20Init(var Context: TChaCha20Context; const Key;
 const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal;
 const Count: Cardinal);
procedure XChaCha20Crypt(var Context: TChaCha20Context; const InBuffer;
  var OutBuffer; const Length: Cardinal);

implementation

uses
  UnsignedFunc;

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
  UMove(Key, ctx[4], KeyLength);
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
  var KeyStream := PByte(@Context.keystream);

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
  try
    SetLength(SubKey, 32);
    Move(keystream[0], SubKey[0], 16);
    Move(keystream[12], SubKey[16], 16);
  finally
    { Security: don't leave key material on the stack }
    FillChar(ctx, SizeOf(ctx), 0);
    FillChar(keystream, SizeOf(keystream), 0);
  end;
end;

procedure XChaCha20Init(var Context: TChaCha20Context; const Key;
 const KeyLength: Cardinal; const Nonce; const NonceLength: Cardinal;
 const Count: Cardinal);
begin
  Assert(NonceLength = 24);
  var SubKey: TBytes;
  HChaCha20(Key, KeyLength, Nonce, 16, SubKey);
  try
    var NonceBytes: PByte := @Nonce;
    ChaCha20Init(Context, SubKey[0], ULength(SubKey), NonceBytes[16], 8, Count);
  finally
    { Security: don't leave derived key in heap memory }
    if Length(SubKey) > 0 then
      FillChar(SubKey[0], Length(SubKey), 0);
  end;
end;

procedure XChaCha20Crypt(var Context: TChaCha20Context; const InBuffer;
  var OutBuffer; const Length: Cardinal);
begin
  ChaCha20Crypt(Context, InBuffer, OutBuffer, Length);
end;

end.
