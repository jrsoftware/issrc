unit TrustFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

{$IFDEF DEBUG}
{$DEFINE TRUSTALL}
{$ENDIF}

interface

function TrustedFileExists(const FileName: String): Boolean;

implementation


uses
  Winapi.Windows, System.SysUtils {$IFNDEF TRUSTALL}, System.Classes, System.Hash {$ENDIF};

{$IFNDEF TRUSTALL}
function GetSHA256OfFileAsString(const FileName: String): String;
begin
  var FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    var Hash := THashSHA2.Create;
    var HashBytes := Hash.GetHashBytes(FileStream);
    Result := THash.DigestAsString(HashBytes);
  finally
    FileStream.Free;
  end;
end;
{$ENDIF}

function TrustedFileExists(const FileName: String): Boolean;
begin
{$IFNDEF TRUSTALL}
  try
    var Hash := GetSHA256OfFileAsString(FileName);
    Result := (Hash = '363b1d094b0ee65683f925d69453abd5c09ecb2089b5928784b0d2bf97206550') or //ISCmplr.dll
              (Hash = '5ae5dcd47ae9cd0929e0d6b2591e2ecc14cb8dfe4e04fb37a6cef5f1896edd11') or //isscint.dll
              (Hash = '474f175192a72e42c8963bcf608e7f9644e126d2588775f7807bc593dd0bad58') or //ISPP.dll
              (Hash = '8072e83385afc4a84006271a87a11fc0a22b149cbd77322669ca56c470d28ced') or //isbzip.dll
              (Hash = 'b252471e95f0853902b15ae71a90574f9b168f8d4a0c474b20537511f90220a5') or //islzma.dll
              (Hash = '14c0d4a2a41572384f8309cdf03de5c6e7ed46bef64cce70d989b2665eff1a47');   //iszlib.dll
  except
    Result := False;
  end;
{$ELSE}
  var Attr := GetFileAttributes(PChar(FileName));
  Result := (Attr <> INVALID_FILE_ATTRIBUTES) and (Attr and faDirectory = 0);
{$ENDIF}
end;

end.
