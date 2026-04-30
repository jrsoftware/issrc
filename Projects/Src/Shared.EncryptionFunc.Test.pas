unit Shared.EncryptionFunc.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for Shared.EncryptionFunc

  Runs a self-test if DEBUG is defined
}

interface

procedure SharedEncryptionFuncRunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, {$ENDIF} System.SysUtils, ChaCha20, Shared.Struct, Shared.EncryptionFunc;

{$C+}

procedure SharedEncryptionFuncRunTests;

  function KeysEqual(const A, B: TSetupEncryptionKey): Boolean;
  begin
    for var I := 0 to High(A) do
      if A[I] <> B[I] then
        Exit(False);
    Result := True;
  end;

var
  Stream1, Stream2: array[0..63] of Byte;
  SpecialStreams: array[TSpecialCryptContextType] of array[0..15] of Byte;
begin
  var Salt: TSetupKDFSalt;
  for var I := 0 to High(Salt) do
    Salt[I] := Byte(I);

  var Nonce: TSetupEncryptionNonce;
  FillChar(Nonce, SizeOf(Nonce), 0);
  Nonce.RandomXorStartOffset := $0102030405060708;
  Nonce.RandomXorFirstSlice := $11223344;

  { GenerateEncryptionKey is deterministic for fixed inputs }
  var Key1, Key2: TSetupEncryptionKey;
  GenerateEncryptionKey('test', Salt, 1, Key1);
  GenerateEncryptionKey('test', Salt, 1, Key2);
  Assert(KeysEqual(Key1, Key2));

  { Different password yields a different key }
  var KeyAlt: TSetupEncryptionKey;
  GenerateEncryptionKey('different', Salt, 1, KeyAlt);
  Assert(not KeysEqual(Key1, KeyAlt));

  { Different salt yields a different key }
  var SaltAlt: TSetupKDFSalt;
  for var I := 0 to High(SaltAlt) do
    SaltAlt[I] := Byte(I + 1);
  GenerateEncryptionKey('test', SaltAlt, 1, KeyAlt);
  Assert(not KeysEqual(Key1, KeyAlt));

  { Different iteration count yields a different key }
  GenerateEncryptionKey('test', Salt, 2, KeyAlt);
  Assert(not KeysEqual(Key1, KeyAlt));

  { GeneratePasswordTest / TestPassword roundtrip }
  var PasswordTestValue: Integer;
  GeneratePasswordTest(Key1, Nonce, PasswordTestValue);
  Assert(TestPassword(Key1, Nonce, PasswordTestValue));
  Assert(not TestPassword(Key1, Nonce, PasswordTestValue + 1));

  { GeneratePasswordTest must use the sccPasswordTest slice. Reproducing
    its behavior using the special-context InitCryptContext overload must
    yield the same value, otherwise the password-check on the Setup side
    would silently disagree with the value the compiler wrote. }
  var ManualCtx: TChaCha20Context;
  InitCryptContext(Key1, Nonce, sccPasswordTest, ManualCtx);
  var ManualPasswordTest: Integer := 0;
  XChaCha20Crypt(ManualCtx, ManualPasswordTest, ManualPasswordTest, SizeOf(ManualPasswordTest));
  Assert(ManualPasswordTest = PasswordTestValue);

  { A key derived from a different password fails the test }
  GenerateEncryptionKey('wrong', Salt, 1, KeyAlt);
  Assert(not TestPassword(KeyAlt, Nonce, PasswordTestValue));

  { Same key with a different nonce produces a different test value }
  var Nonce2 := Nonce;
  Nonce2.RandomXorStartOffset := Nonce2.RandomXorStartOffset xor 1;
  var PasswordTestValue2: Integer;
  GeneratePasswordTest(Key1, Nonce2, PasswordTestValue2);
  Assert(PasswordTestValue <> PasswordTestValue2);

  { InitCryptContext (offset/slice variant): identical inputs produce identical keystreams }
  var Ctx1, Ctx2: TChaCha20Context;
  InitCryptContext(Key1, Nonce, 0, 0, Ctx1);
  InitCryptContext(Key1, Nonce, 0, 0, Ctx2);
  FillChar(Stream1, SizeOf(Stream1), 0);
  FillChar(Stream2, SizeOf(Stream2), 0);
  XChaCha20Crypt(Ctx1, Stream1, Stream1, SizeOf(Stream1));
  XChaCha20Crypt(Ctx2, Stream2, Stream2, SizeOf(Stream2));
  for var I := 0 to High(Stream1) do
    Assert(Stream1[I] = Stream2[I]);

  { Different offset/slice produces a different keystream }
  InitCryptContext(Key1, Nonce, 1, 0, Ctx2);
  FillChar(Stream2, SizeOf(Stream2), 0);
  XChaCha20Crypt(Ctx2, Stream2, Stream2, SizeOf(Stream2));
  var AnyDiffer := False;
  for var I := 0 to High(Stream1) do
    if Stream1[I] <> Stream2[I] then begin
      AnyDiffer := True;
      Break;
    end;
  Assert(AnyDiffer);

  { InitCryptContext (special-context variant): each TSpecialCryptContextType produces a
    distinct keystream and none of them collide with offset 0 / slice 0 }
  for var T := Low(TSpecialCryptContextType) to High(TSpecialCryptContextType) do begin
    var Ctx: TChaCha20Context;
    InitCryptContext(Key1, Nonce, T, Ctx);
    FillChar(SpecialStreams[T], SizeOf(SpecialStreams[T]), 0);
    XChaCha20Crypt(Ctx, SpecialStreams[T], SpecialStreams[T], SizeOf(SpecialStreams[T]));
  end;
  for var T1 := Low(TSpecialCryptContextType) to High(TSpecialCryptContextType) do begin
    for var T2 := Low(TSpecialCryptContextType) to High(TSpecialCryptContextType) do begin
      if T1 <> T2 then begin
        AnyDiffer := False;
        for var I := 0 to High(SpecialStreams[T1]) do
          if SpecialStreams[T1][I] <> SpecialStreams[T2][I] then begin
            AnyDiffer := True;
            Break;
          end;
        Assert(AnyDiffer);
      end;
    end;
    { Stream1 still holds the offset 0 / slice 0 keystream from earlier }
    AnyDiffer := False;
    for var I := 0 to High(SpecialStreams[T1]) do
      if SpecialStreams[T1][I] <> Stream1[I] then begin
        AnyDiffer := True;
        Break;
      end;
    Assert(AnyDiffer);
  end;
end;

{$IFDEF DEBUG}
initialization
  try
    SharedEncryptionFuncRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}

end.
