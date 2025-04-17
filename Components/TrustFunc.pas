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
      var AllowedPublicKey1Text, AllowedPublicKey2Text: String;
      {$I TrustFunc.AllowedPublicKeys.inc}
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
