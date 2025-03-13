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
              (Hash = '062c808fab6f6f948652fd5708ccca4b4b91c33b8a66af1c3c6da5cdd94f113c');   //isscint.dll
  except
    Result := False;
  end;
{$ELSE}
  Result := True;
{$ENDIF}
end;

end.
