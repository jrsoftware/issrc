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
key-id def0147c3bbc17ab99bf7b7a9c2de1390283f38972152418d7c2a4a7d7131a38
public-x e3e943066aff8f28d2219fd71c9ffff4c8d1aa26bc4225434be67180ab5e242d
public-y e419041c3f54551e86a1c47f387005cd535dfc9d64339b30d37f9a4f7866b650

''';
      const AllowedPublicKey2Text = '''
format issig-public-key
key-id def020edee3c4835fd54d85eff8b66d4d899b22a777353ca4a114b652e5e7a28
public-x 515dc7d6c16d4a46272ceb3d158c5630a96466ab4d948e72c2029d737c823097
public-y f3c21f6b5156c52a35f6f28016ee3e31a3ded60c325b81fb7b1f88c221081a61

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
