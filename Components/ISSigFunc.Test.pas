unit ISSigFunc.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for ISSigFunc

  Runs a self-test if DEBUG is defined
}

interface

procedure ISSigFuncRunTests;

implementation

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  ECDSA, SHA256, ISSigFunc;

{$C+}

procedure ISSigFuncRunTests;

  function MakeHash(const AByte: Byte): TSHA256Digest;
  begin
    FillChar(Result, SizeOf(Result), AByte);
  end;

  function ReplaceFirst(const AText, AOld, ANew: String): String;
  begin
    const Idx = Pos(AOld, AText);
    Assert(Idx > 0);
    Result := Copy(AText, 1, Idx - 1) + ANew +
      Copy(AText, Idx + Length(AOld), MaxInt);
  end;

  procedure TestVerify(const AAllowedKeys: array of TECDSAKey;
    const AText: String; const AExpected: TISSigVerifySignatureResult);
  begin
    var FileName: String;
    var FileSize: Int64;
    var FileHash: TSHA256Digest;
    Assert(ISSigVerifySignatureText(AAllowedKeys, AText, FileName, FileSize,
      FileHash) = AExpected);
  end;

  procedure TestParsePublic(const AText: String;
    const AExpected: TISSigImportKeyResult);
  begin
    var PublicKey: TECDSAPublicKey;
    Assert(ISSigParsePublicKeyText(AText, PublicKey) = AExpected);
  end;

  procedure TestParsePrivate(const AText: String;
    const AExpected: TISSigImportKeyResult);
  begin
    var PrivateKey: TECDSAPrivateKey;
    try
      Assert(ISSigParsePrivateKeyText(AText, PrivateKey) = AExpected);
    finally
      PrivateKey.Clear;
    end;
  end;

  procedure TestImportPublic(const AKeyID, APublicX, APublicY: String;
    const AExpected: TISSigImportKeyResult);
  begin
    const Key = TECDSAKey.Create;
    try
      Assert(ISSigImportPublicKey(Key, AKeyID, APublicX, APublicY) = AExpected);
    finally
      Key.Free;
    end;
  end;

  {$IFDEF ISTESTTOOLPROJ}
  procedure TestCreateSignatureTextRaises(const AKey: TECDSAKey; const AFileName: String;
    const AFileSize: Int64; const AFileHash: TSHA256Digest);
  begin
    var Caught := False;
    try
      ISSigCreateSignatureText(AKey, AFileName, AFileSize, AFileHash);
    except
      on Exception do Caught := True;
    end;
    Assert(Caught);
  end;

  procedure TestCheckValidKeyIDRaises(const AValue: String);
  begin
    var Caught := False;
    try
      ISSigCheckValidKeyID(AValue);
    except
      on EConvertError do Caught := True;
    end;
    Assert(Caught);
  end;

  procedure TestCheckValidPublicXOrYRaises(const AValue: String);
  begin
    var Caught := False;
    try
      ISSigCheckValidPublicXOrY(AValue);
    except
      on EConvertError do Caught := True;
    end;
    Assert(Caught);
  end;
  {$ENDIF}

  function MakeTempFileName: String;
  begin
    var Path: String;
    SetLength(Path, MAX_PATH);
    SetLength(Path, GetTempPath(MAX_PATH, PChar(Path)));
    SetLength(Result, MAX_PATH);
    if Winapi.Windows.GetTempFileName(PChar(Path), 'iss', 0, PChar(Result)) = 0 then
      RaiseLastOSError;
    SetLength(Result, StrLen(PChar(Result)));
  end;

  procedure TestLoadTextFromFile(const ABytes: array of Byte; const AExpected: String);
  begin
    const FileName = MakeTempFileName;
    try
      const F = TFileStream.Create(FileName, fmCreate);
      try
        if Length(ABytes) > 0 then
          F.WriteBuffer(ABytes[0], Length(ABytes));
      finally
        F.Free;
      end;
      Assert(ISSigLoadTextFromFile(FileName) = AExpected);
    finally
      Winapi.Windows.DeleteFile(PChar(FileName));
    end;
  end;

begin
  const Key1 = TECDSAKey.Create;
  try
    Key1.GenerateKeyPair;

    const Key2 = TECDSAKey.Create;
    try
      Key2.GenerateKeyPair;

      const TestFileName = 'test.exe';
      const TestFileSize = 12345;
      const TestHash = MakeHash($AB);
      const SigText = ISSigCreateSignatureText(Key1, TestFileName, TestFileSize,
        TestHash);

      { Round-trip: signature created with Key1 verifies with Key1 in the
        allowed-key set, and the parsed values match what was signed }
      var FileName: String;
      var FileSize: Int64;
      var FileHash: TSHA256Digest;
      var KeyUsedID: String;
      Assert(ISSigVerifySignatureText([Key1], SigText, FileName, FileSize,
        FileHash, KeyUsedID) = vsrSuccess);
      Assert(FileName = TestFileName);
      Assert(FileSize = TestFileSize);
      Assert(SHA256DigestsEqual(FileHash, TestHash));
      Assert(Length(KeyUsedID) = 64);

      { Multiple allowed keys: success when the matching key is anywhere in
        the set }
      TestVerify([Key2, Key1], SigText, vsrSuccess);
      TestVerify([Key1, Key2], SigText, vsrSuccess);

      { Empty allowed-key set is reported as vsrKeyNotFound }
      TestVerify([], SigText, vsrKeyNotFound);

      { Unrelated key in the allowed-key set is reported as vsrKeyNotFound }
      TestVerify([Key2], SigText, vsrKeyNotFound);

      { Tampered file-size: signature still parses but no longer verifies }
      const SigBadSize = ReplaceFirst(SigText,
        'file-size 12345', 'file-size 12346');
      TestVerify([Key1], SigBadSize, vsrBad);

      { Tampered file-hash: flip the first hex digit of the hash }
      const HashStr = SHA256DigestToString(TestHash);
      var BadHashStr := HashStr;
      if BadHashStr[Low(BadHashStr)] = '0' then
        BadHashStr[Low(BadHashStr)] := '1'
      else
        BadHashStr[Low(BadHashStr)] := '0';
      const SigBadHash = ReplaceFirst(SigText,
        'file-hash ' + HashStr, 'file-hash ' + BadHashStr);
      TestVerify([Key1], SigBadHash, vsrBad);

      { Tampered file-name }
      const SigBadName = ReplaceFirst(SigText,
        'file-name "test.exe"', 'file-name "other.ex"');
      TestVerify([Key1], SigBadName, vsrBad);

      { Malformed: empty input }
      TestVerify([Key1], '', vsrMalformed);

      { Malformed: truncated (missing trailing fields) }
      const TruncIdx = Pos('sig-r', SigText);
      Assert(TruncIdx > 0);
      TestVerify([Key1], Copy(SigText, 1, TruncIdx - 1), vsrMalformed);

      { Malformed: reordered fields (file-size before file-name) }
      const SigReordered = StringReplace(SigText,
        'file-name "test.exe"'#13#10'file-size 12345'#13#10,
        'file-size 12345'#13#10'file-name "test.exe"'#13#10, []);
      TestVerify([Key1], SigReordered, vsrMalformed);

      { Malformed: unknown format string }
      TestVerify([Key1], ReplaceFirst(SigText, 'format issig-v2',
        'format issig-vX'), vsrMalformed);

      { Malformed: leading zero on file-size is rejected }
      const SigLeadingZero = ReplaceFirst(SigText,
        'file-size 12345', 'file-size 012345');
      TestVerify([Key1], SigLeadingZero, vsrMalformed);

      { Malformed: extra trailing content after the last field }
      TestVerify([Key1], SigText + 'extra'#13#10, vsrMalformed);

      { Round-trip: export/parse public key and import back, then verify }
      var PubText: String;
      ISSigExportPublicKeyText(Key1, PubText);
      var ParsedPub: TECDSAPublicKey;
      Assert(ISSigParsePublicKeyText(PubText, ParsedPub) = ikrSuccess);
      const ImportedPubKey = TECDSAKey.Create;
      try
        ImportedPubKey.ImportPublicKey(ParsedPub);
        TestVerify([ImportedPubKey], SigText, vsrSuccess);
      finally
        ImportedPubKey.Free;
      end;

      { Round-trip: export private key, parse it, import the private key, sign
        a fresh signature with the imported key, and verify it with Key1 }
      var PrivText: String;
      try
        ISSigExportPrivateKeyText(Key1, PrivText);
        var ParsedPriv: TECDSAPrivateKey;
        try
          Assert(ISSigParsePrivateKeyText(PrivText, ParsedPriv) = ikrSuccess);
          const ImportedPrivKey = TECDSAKey.Create;
          try
            ImportedPrivKey.ImportPrivateKey(ParsedPriv);
            const SigText2 = ISSigCreateSignatureText(ImportedPrivKey,
              'other.dat', 999, MakeHash($CD));
            TestVerify([Key1], SigText2, vsrSuccess);
          finally
            ImportedPrivKey.Free;
          end;
        finally
          ParsedPriv.Clear;
        end;
      finally
        ISSigWipeString(PrivText);
      end;

      { ISSigParsePrivateKeyText on a public-only text returns ikrNotPrivateKey }
      TestParsePrivate(PubText, ikrNotPrivateKey);

      { ISSigParsePublicKeyText accepts a private key text (only the public
        part is used) }
      ISSigExportPrivateKeyText(Key1, PrivText);
      try
        TestParsePublic(PrivText, ikrSuccess);
      finally
        ISSigWipeString(PrivText);
      end;

      { Malformed key text }
      TestParsePublic('', ikrMalformed);
      TestParsePublic('garbage'#13#10, ikrMalformed);
      TestParsePublic(ReplaceFirst(PubText, 'format issig-public-key',
        'format issig-bogus-key'), ikrMalformed);
      TestParsePrivate('', ikrMalformed);
      TestParsePrivate('garbage'#13#10, ikrMalformed);

      { ISSigIsValidKeyIDForPublicXY: true for a matching trio,
        false for a mismatched one }
      var Pub: TECDSAPublicKey;
      Key1.ExportPublicKey(Pub);
      var PubX, PubY: String;
      ISSigConvertPublicKeyToStrings(Pub, PubX, PubY);
      const Key1ID = SHA256DigestToString(SHA256Buf(Pub, SizeOf(Pub)));
      Assert(ISSigIsValidKeyIDForPublicXY(Key1ID, PubX, PubY));

      var Pub2: TECDSAPublicKey;
      Key2.ExportPublicKey(Pub2);
      const Key2ID = SHA256DigestToString(SHA256Buf(Pub2, SizeOf(Pub2)));
      Assert(not ISSigIsValidKeyIDForPublicXY(Key2ID, PubX, PubY));

      { ISSigImportPublicKey: success when KeyID matches the X/Y values, success
        with empty KeyID (verification skipped), ikrMalformed when KeyID does
        not match }
      TestImportPublic(Key1ID, PubX, PubY, ikrSuccess);
      TestImportPublic('', PubX, PubY, ikrSuccess);
      TestImportPublic(Key2ID, PubX, PubY, ikrMalformed);

      {$IFDEF ISTESTTOOLPROJ}
      { ISSigCreateSignatureText: each kind of invalid input must raise.
        File name longer than 1000 UTF-8 bytes, contains a quote, contains a
        control character, negative file size, file size above the 16-digit
        limit }
      TestCreateSignatureTextRaises(Key1, StringOfChar('x', 1001), TestFileSize, TestHash);
      TestCreateSignatureTextRaises(Key1, 'has"quote', TestFileSize, TestHash);
      TestCreateSignatureTextRaises(Key1, 'has'#1'control', TestFileSize, TestHash);
      TestCreateSignatureTextRaises(Key1, TestFileName, -1, TestHash);
      TestCreateSignatureTextRaises(Key1, TestFileName, 10000000000000000, TestHash);
      {$ENDIF}
    finally
      Key2.Free;
    end;
  finally
    Key1.Free;
  end;

  { ISSigSaveTextToFile/ISSigLoadTextFromFile round-trip }
  const RoundTripFileName = MakeTempFileName;
  try
    const RoundTripText = 'roundtrip text'#13#10;
    ISSigSaveTextToFile(RoundTripFileName, RoundTripText);
    Assert(ISSigLoadTextFromFile(RoundTripFileName) = RoundTripText);
  finally
    Winapi.Windows.DeleteFile(PChar(RoundTripFileName));
  end;

  { ISSigLoadTextFromFile defenses-in-depth }
  TestLoadTextFromFile([], '');                                     { empty file }
  TestLoadTextFromFile([Ord('h'), Ord('i'), $0D, $0A], 'hi'#13#10); { ASCII text }
  TestLoadTextFromFile([Ord('a'), $01, Ord('b')], '');              { control char }
  TestLoadTextFromFile([Ord('a'), $FF, Ord('b')], '');              { byte never in UTF-8 }

  { File at the 2500-byte limit is loaded; over the limit returns '' }
  var Buf: TBytes;
  SetLength(Buf, 2500);
  FillChar(Buf[0], Length(Buf), Ord('a'));
  TestLoadTextFromFile(Buf, StringOfChar('a', 2500));
  SetLength(Buf, 2501);
  TestLoadTextFromFile(Buf, '');

  { ISSigCheckValidKeyID and ISSigCheckValidPublicXOrY accept a valid 64-hex
    string. They are thin wrappers around SHA256DigestFromString and
    ECDSAInt256FromString respectively. }
  ISSigCheckValidKeyID(StringOfChar('0', 64));
  ISSigCheckValidKeyID(StringOfChar('a', 64));
  ISSigCheckValidPublicXOrY(StringOfChar('0', 64));
  ISSigCheckValidPublicXOrY(StringOfChar('a', 64));

  {$IFDEF ISTESTTOOLPROJ}
  { Wrong-length / out-of-charset input must raise. ISSigCheckValidKeyID and
    ISSigCheckValidPublicXOrY both ultimately call SHA256DigestFromString,
    which accepts mixed case, so the same set of inputs is invalid for both. }
  const Invalid = ['', 'abc', StringOfChar('z', 64), StringOfChar('0', 63),
    StringOfChar('0', 65)];
  for var S in Invalid do begin
    TestCheckValidKeyIDRaises(S);
    TestCheckValidPublicXOrYRaises(S);
  end;
  {$ENDIF}
end;

{$IFDEF DEBUG}
{$IFNDEF ISTESTTOOLPROJ}
initialization
  try
    ISSigFuncRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}
{$ENDIF}

end.
