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
      const
        AllowedPublicKeyText = '''
format issig-public-key
key-id c2587f3885b12463bafdadb799f23435f26c03944c1afc1716aabc6a43f2426f
public-x f9a30c72189077370a8846015ac3ec1e9a1cf425d2996d34dc25bd4f4923dd1b
public-y f754897b7819da5bbbc5ac568311eee922fbea492578748e07f453dc1289c532

''';
      const Key = TECDSAKey.Create;
      try
        if ISSigImportKeyText(Key, AllowedPublicKeyText, False) <> ikrSuccess then
          raise Exception.Create('ISSigImportKeyText failed');

        const SigFileName = FileName + '.issig';
        const SigText = ISSigLoadTextFromFile(SigFileName);

        var ExpectedFileSize: Int64;
        var ExpectedFileHash: TSHA256Digest;
        if ISSigVerifySignatureText([Key], SigText, ExpectedFileSize,
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
        Key.Free;
      end;
    except
      Result := False;
    end;
  end;
{$ENDIF}
end;

end.
