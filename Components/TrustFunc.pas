unit TrustFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

{.$DEFINE TRUSTALL}

interface

function TrustedFileExists(const FileName: String): Boolean;

implementation

uses
  Winapi.Windows, System.SysUtils {$IFNDEF TRUSTALL}, System.Classes, ECDSA, SHA256, ISSigFunc {$ENDIF};

function TrustedFileExists(const FileName: String): Boolean;
begin
  var Attr := GetFileAttributes(PChar(FileName));
  Result := (Attr <> INVALID_FILE_ATTRIBUTES) and (Attr and faDirectory = 0);
{$IFNDEF TRUSTALL}
  if Result then begin
    try
      const AllowedPublicKey1Text = '''
format issig-public-key
key-id abcdef0ab475e78d6d8a259b08b1a1875d3381ea522eb6928defd15cf4d94808
public-x acb1f30b47cab5a79e7964df28e52e893dc4d12fd2056811b20a73186576071e
public-y 2edbc9a82bc94e1a54fe5812cba13e4b1384d46eb5fa0df52c7b80776be1bcb2

''';
      const AllowedPublicKey2Text = '''
format issig-public-key
key-id c2587f3885b12463bafdadb799f23435f26c03944c1afc1716aabc6a43f2426f
public-x f9a30c72189077370a8846015ac3ec1e9a1cf425d2996d34dc25bd4f4923dd1b
public-y f754897b7819da5bbbc5ac568311eee922fbea492578748e07f453dc1289c532

''';
      var Key1: TECDSAKey := nil;
      var Key2: TECDSAKey := nil;
      try
        Key1 := TECDSAKey.Create;
        if ISSigImportKeyText(Key1, AllowedPublicKey1Text, False) <> ikrSuccess then
          raise Exception.Create('ISSigImportKeyText failed');
        Key2 := TECDSAKey.Create;
        if ISSigImportKeyText(Key2, AllowedPublicKey2Text, False) <> ikrSuccess then
          raise Exception.Create('ISSigImportKeyText failed');

        const SigFileName = FileName + '.issig';
        const SigText = ISSigLoadTextFromFile(SigFileName);

        var ExpectedFileSize: Int64;
        var ExpectedFileHash: TSHA256Digest;
        if ISSigVerifySignatureText([Key1, Key2], SigText, ExpectedFileSize,
           ExpectedFileHash) <> vsrSuccess then
          raise Exception.CreateFmt('Signature file "%s" is not valid',
            [SigFileName]);

        const F = TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
        try
          if F.Size <> ExpectedFileSize then
            raise Exception.CreateFmt('File "%s" is not trusted (incorrect size).',
              [FileName]);
          if not SHA256DigestsEqual(ISSigCalcStreamHash(F), ExpectedFileHash) then
            raise Exception.CreateFmt('File "%s" is not trusted (incorrect hash).',
              [FileName]);
        finally
          F.Free;
        end;
      finally
        Key2.Free;
        Key1.Free;
      end;
    except
      Result := False;
    end;
  end;
{$ENDIF}
end;

end.
