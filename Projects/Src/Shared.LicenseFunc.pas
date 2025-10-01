unit Shared.LicenseFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  License functions used by both IDE and ISCC units
}

interface

type
  TLicenseType = (ltSingle, ltTeam, ltEnterprise);
  TLicense = record
    Key, Name: String;
    Typ: TLicenseType;
    ExpirationDate: TDateTime;
  end;
  { "Expired" means update entitlement ended }
  TLicenseState = (lsNotLicensed, lsLicensed, lsExpiring, lsExpired, lsExpiredButUpdated);

procedure ReadLicense;
procedure WriteLicense;
procedure RemoveLicense;

function ParseLicenseKey(const LicenseKey: String; out License: TLicense): Boolean;
function UpdateLicense(const LicenseKey: String): Boolean; overload;
procedure UpdateLicense(const ALicense: TLicense); overload;

function IsLicensed: Boolean;
function GetLicenseKey: String;
function GetChunkedLicenseKey: String;
function GetLicenseState: TLicenseState;
function GetLicenseeName: String;
function GetLicenseeDescription: String;
function GetLicenseTypeDescription: String;
function GetLicenseDescription(const Prefix, Separator: String): String;

implementation

uses
 Windows,
 SysUtils, Classes, DateUtils, NetEncoding, RegularExpressions,
 ECDSA, SHA256, UnsignedFunc, Shared.ConfigIniFile;

var
  License: TLicense;

procedure ReadLicense;
begin
  const Ini = TConfigIniFile.Create;
  try
    UpdateLicense(Ini.ReadString('License', 'LicenseKey', ''));
  finally
    Ini.Free;
  end;
end;

procedure WriteLicense;
begin
  const Ini = TConfigIniFile.Create;
  try
    Ini.WriteString('License', 'LicenseKey', License.Key);
  finally
    Ini.Free;
  end;
end;

procedure RemoveLicense;
begin
  const Ini = TConfigIniFile.Create;
  try
    Ini.DeleteKey('License', 'LicenseKey');
  finally
    Ini.Free;
  end;

  UpdateLicense('');
end;

function ParseLicenseKey(const LicenseKey: String; out License: TLicense): Boolean;

  function ECDSAInt256FromString(const S: String): TECDSAInt256;
  begin
    TSHA256Digest(Result) := SHA256DigestFromString(S);
  end;

  function TryDateFromDBDate(const S: string; out D: TDate): Boolean;
  begin
    const N = S.Length;
    if N = 8 then begin
      const Year = Word.Parse(Copy(S, 1, 4));
      const Month = Word.Parse(Copy(S, 5, 2));
      const Day = Word.Parse(Copy(S, 7, 2));
      D := EncodeDate(Year, Month, Day);
      Result := True;
    end else if N = 0 then begin
      D := 0;
      Result := True;
    end else
      Result := False;
  end;

begin
  Result := False;

  const CleanLicenseKey = TRegEx.Replace(LicenseKey, '\s+', '');
  const N = Length(CleanLicenseKey);
  if N > 92 then begin { 92 = (64/3*4 rounded to a multiple of 4) + 4 }
    if (Copy(CleanLicenseKey, 1, 2) = 'in') and (Copy(CleanLicenseKey, N-1, 2) = 'no') then begin
      var EncodedKey := Copy(CleanLicenseKey, 3, N-4); { Strip 'in' and 'no' }
      EncodedKey := EncodedKey + StringOfChar('=', (4 - Length(EncodedKey) mod 4) mod 4); { Restore base64 padding }
      const DecodedKey = TNetEncoding.Base64.DecodeStringToBytes(EncodedKey);
      if Length(DecodedKey) > 64 then begin
        var Signature := Default(TECDSASignature);
        Move(DecodedKey[0], Signature.Sig_r[0], 32);
        Move(DecodedKey[32], Signature.Sig_s[0], 32);
        const LicenseBytes = Copy(DecodedKey, 64, MaxInt);
        const LicenseHash = SHA256Buf(LicenseBytes[0], ULength(LicenseBytes));

        var PublicKey: TECDSAPublickey;
        PublicKey.Public_x := ECDSAInt256FromString('76873a71a4d5cae3dfdb52f7e434582c25151e56338d6d7fd5423d1216dc3274');
        PublicKey.Public_y := ECDSAInt256FromString('4459f8d7c0e6c03e34806a4a4b949e0c16387fb8ff2f71d2d62ce6a29c713018');

        const ECDSAKey = TECDSAKey.Create;
        var Verified: Boolean;
        try
          ECDSAKey.ImportPublicKey(PublicKey);
          Verified := ECDSAKey.VerifySignature(LicenseHash, Signature);
        finally
          ECDSAKey.Free;
        end;

        if Verified then begin
          const LicenseString = TEncoding.UTF8.GetString(LicenseBytes);
          if LicenseString.StartsWith('1'#9) then begin
            const LicenseData = LicenseString.Split([#9]);
            if Length(LicenseData) = 4 then begin
              const LicenseeName = LicenseData[1];
              const LicenseType = LicenseData[2].ToInteger;
              var ExpirationDate: TDate;
              if TryDateFromDBDate(LicenseData[3], ExpirationDate) and (LicenseeName <> '') and
                 (LicenseType >= Ord(Low(TLicenseType))) and (LicenseType <= Ord(High(TLicenseType))) then begin
                License.Key := CleanLicenseKey;
                License.Name := LicenseeName;
                License.Typ := TLicenseType(LicenseType);
                License.ExpirationDate := ExpirationDate;
                Result := True;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function UpdateLicense(const LicenseKey: String): Boolean;
begin
  if LicenseKey <> '' then
    Result := ParseLicenseKey(LicenseKey, License)
  else begin
    License := Default(TLicense);
    Result := True;
  end;
end;

procedure UpdateLicense(const ALicense: TLicense);
begin
  License := ALicense;
end;

function IsLicensed: Boolean;
begin
  Result := GetLicenseKey <> '';
end;

function GetLicenseKey: String;
begin
  Result := License.Key;
end;

function GetChunkedLicenseKey: String;
begin
  const Output = TStringList.Create;
  try
    var StartIndex := 1;
    const ChunkSize = 28;
    while StartIndex <= Length(License.Key) do begin
      Output.Add(Copy(License.Key, StartIndex, ChunkSize));
      StartIndex := StartIndex + ChunkSize;
    end;
    Result := Output.Text.Trim;
  finally
    Output.Free;
  end;
end;

var
  LinkerTimeStamp: TDateTime;
  ReadLinkerTimeStamp: Boolean;

function GetLicenseState: TLicenseState;
begin
  if not IsLicensed then
    Result := lsNotLicensed
  else if License.ExpirationDate <> 0 then begin
    if not ReadLinkerTimeStamp then begin
      try
        LinkerTimeStamp := PImageNtHeaders(HInstance + Cardinal(PImageDosHeader(HInstance)._lfanew)).FileHeader.TimeDateStamp / SecsPerDay + UnixDateDelta;
      except
      end;
      ReadLinkerTimeStamp := True;
    end;
    if (LinkerTimeStamp <> 0) and (LinkerTimeStamp > License.ExpirationDate) then
      Result := lsExpiredButUpdated
    else begin
      const CurrentDate = Date;
      if License.ExpirationDate < CurrentDate then
        Result := lsExpired
      else if License.ExpirationDate < IncMonth(CurrentDate, 1) then
        Result := lsExpiring
      else
        Result := lsLicensed;
     end;
  end else
    Result := lsLicensed;
end;

function GetLicenseeName: String;
begin
  Result := License.Name;
end;

function GetLicenseeDescription: String;
begin
  const LicenseState = GetLicenseState;
  if LicenseState <> lsNotLicensed then begin
    Result := GetLicenseeName;
    if LicenseState = lsExpired then
      Result := Result + ' (Update entitlement ended)'
    else if LicenseState = lsExpiredButUpdated then
      Result := Result + ' (Update entitlement ended but updated anyway)';
  end else
    Result := 'Non-commercial use only';
end;

function GetLicenseTypeDescription: String;
begin
  case License.Typ of
    ltSingle: Result := 'Single User';
    ltTeam: Result := 'Team';
    ltEnterprise: Result := 'Enterprise';
  else
    raise Exception.Create('Unknown License.Typ');
  end;
  Result := 'Inno Setup ' + Result + ' License';
end;

function GetLicenseDescription(const Prefix, Separator: String): String;
begin
  if IsLicensed then begin
    Result := Prefix + GetLicenseeName + ', ' + GetLicenseTypeDescription + '.' + Separator;
    if License.ExpirationDate <> 0 then
      Result := Result + 'Includes updates until ' + DateToStr(License.ExpirationDate) + ', major and minor.'
    else
      Result := Result + 'Includes all future updates, major and minor.';
  end else
    Result := GetLicenseeDescription + '.';
end;

end.
