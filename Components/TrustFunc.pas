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

function TrustedFile(const FileName: string): Boolean;

implementation

{$IFNDEF TRUSTALL}
uses
  SysUtils, Classes, Hash;

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

function TrustedFile(const FileName: string): Boolean;
begin
{$IFNDEF TRUSTALL}
  try
    var Hash := GetSHA256OfFileAsString(FileName);
    Result := (Hash = '363b1d094b0ee65683f925d69453abd5c09ecb2089b5928784b0d2bf97206550') or //ISCmplr.dll
              (Hash = '5ae5dcd47ae9cd0929e0d6b2591e2ecc14cb8dfe4e04fb37a6cef5f1896edd11') or //isscint.dll
              (Hash = 'todo') //ISPP.dll
  except
    Result := False;
  end;
{$ELSE}
  Result := True;
{$ENDIF}
end;

end.
