unit Shared.SetupTypes;

{
  Inno Setup
  Copyright (C) 1997-2018 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Types and functions used by both ISCmplr-only and Setup-only units
}

interface

uses
  SysUtils, Classes, Shared.Struct;

const
  { Predefined page identifiers }
  wpWelcome = 1;
  wpLicense = 2;
  wpPassword = 3;
  wpInfoBefore = 4;
  wpUserInfo = 5;
  wpSelectDir = 6;
  wpSelectComponents = 7;
  wpSelectProgramGroup = 8;
  wpSelectTasks = 9;
  wpReady = 10;
  wpPreparing = 11;
  wpInstalling = 12;
  wpInfoAfter = 13;
  wpFinished = 14;

type
  TInstallOnThisVersionResult = (irInstall, irNotOnThisPlatform,
    irVersionTooLow, irServicePackTooLow, irVerTooHigh);

  TRenamedConstantCallBack = procedure(const Cnst, CnstRenamed: String) of object;

const
  crHand = 1;

  CodeRootKeyFlagMask  = $7F000000;
  CodeRootKeyFlag32Bit = $01000000;
  CodeRootKeyFlag64Bit = $02000000;
  CodeRootKeyValidFlags = CodeRootKeyFlag32Bit or CodeRootKeyFlag64Bit;

  HKEY_AUTO = 1; { Any value will work as long as it isn't 0 and doesn't match a predefined key handle (8xxxxxxx) nor includes any of the CodeRootKeyValidFlags flags. }

function StringsToCommaString(const Strings: TStrings): String;
procedure SetStringsFromCommaString(const Strings: TStrings; const Value: String);
function StrToSetupVersionData(const S: String; var VerData: TSetupVersionData): Boolean;
procedure HandleRenamedConstants(var Cnst: String; const RenamedConstantCallback: TRenamedConstantCallback);
procedure GenerateEncryptionKey(const Password: String; const Salt: TSetupKDFSalt;
  const Iterations: Integer; out Key: TSetupEncryptionKey);

implementation

uses
  PBKDF2, Shared.CommonFunc;

function QuoteStringIfNeeded(const S: String): String;
{ Used internally by StringsToCommaString. Adds quotes around the string if
  needed, and doubles any embedded quote characters.
  Note: No lead byte checking is done since spaces/commas/quotes aren't used
  as trail bytes in any of the Far East code pages (CJK). }
var
  Len, QuoteCount, I: Integer;
  HasSpecialChars: Boolean;
  P: PChar;
begin
  Len := Length(S);
  HasSpecialChars := False;
  QuoteCount := 0;
  for I := 1 to Len do begin
    case S[I] of
      #0..' ', ',': HasSpecialChars := True;
      '"': Inc(QuoteCount);
    end;
  end;
  if not HasSpecialChars and (QuoteCount = 0) then begin
    Result := S;
    Exit;
  end;

  SetString(Result, nil, Len + QuoteCount + 2);
  P := Pointer(Result);
  P^ := '"';
  Inc(P);
  for I := 1 to Len do begin
    if S[I] = '"' then begin
      P^ := '"';
      Inc(P);
    end;
    P^ := S[I];
    Inc(P);
  end;
  P^ := '"';
end;

function StringsToCommaString(const Strings: TStrings): String;
{ Creates a comma-delimited string from Strings.
  Note: Unlike Delphi 2's TStringList.CommaText property, this function can
  handle an unlimited number of characters. }
var
  I: Integer;
  S: String;
begin
  if (Strings.Count = 1) and (Strings[0] = '') then
    Result := '""'
  else begin
    Result := '';
    for I := 0 to Strings.Count-1 do begin
      S := QuoteStringIfNeeded(Strings[I]);
      if I = 0 then
        Result := S
      else
        Result := Result + ',' + S;
    end;
  end;
end;

procedure SetStringsFromCommaString(const Strings: TStrings; const Value: String);
{ Replaces Strings with strings from the comma- or space-delimited Value.
  Note: No lead byte checking is done since spaces/commas/quotes aren't used
  as trail bytes in any of the Far East code pages (CJK).
  Also, this isn't bugged like Delphi 3+'s TStringList.CommaText property --
  SetStringsFromCommaString(..., 'a,') will add two items, not one. }
var
  P, PStart, PDest: PChar;
  CharCount: Integer;
  S: String;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    P := PChar(Value);
    while CharInSet(P^, [#1..' ']) do
      Inc(P);
    if P^ <> #0 then begin
      while True do begin
        if P^ = '"' then begin
          Inc(P);
          PStart := P;
          CharCount := 0;
          while P^ <> #0 do begin
            if P^ = '"' then begin
              Inc(P);
              if P^ <> '"' then Break;
            end;
            Inc(CharCount);
            Inc(P);
          end;
          P := PStart;
          SetString(S, nil, CharCount);
          PDest := Pointer(S);
          while P^ <> #0 do begin
            if P^ = '"' then begin
              Inc(P);
              if P^ <> '"' then Break;
            end;
            PDest^ := P^;
            Inc(P);
            Inc(PDest);
          end;
        end
        else begin
          PStart := P;
          while (P^ > ' ') and (P^ <> ',') do
            Inc(P);
          SetString(S, PStart, P - PStart);
        end;
        Strings.Add(S);
        while CharInSet(P^, [#1..' ']) do
          Inc(P);
        if P^ = #0 then
          Break;
        if P^ = ',' then begin
          repeat
            Inc(P);
          until not CharInSet(P^, [#1..' ']);
        end;
      end;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

function StrToSetupVersionData(const S: String; var VerData: TSetupVersionData): Boolean;

  procedure Split(const Str: String; var Ver: TSetupVersionDataVersion;
    var ServicePack: Word);
  var
    I, J: Integer;
    Z, B: String;
    HasBuild: Boolean;
  begin
    Cardinal(Ver) := 0;
    ServicePack := 0;
    Z := Lowercase(Str);
    I := Pos('sp', Z);
    if I <> 0 then begin
      J := StrToInt(Copy(Z, I+2, Maxint));
      if (J < Low(Byte)) or (J > High(Byte)) then
        Abort;
      ServicePack := J shl 8;
      { ^ Shift left 8 bits because we're setting the "major" service pack
        version number. This parser doesn't currently accept "minor" service
        pack version numbers. }
      SetLength(Z, I-1);
    end;
    I := Pos('.', Z);
    if I = Length(Z) then Abort;
    if I <> 0 then begin
      J := StrToInt(Copy(Z, 1, I-1));
      if (J < 0) or (J > 127) then
        Abort;
      Ver.Major := J;
      Z := Copy(Z, I+1, Maxint);
      I := Pos('.', Z);
      HasBuild := I <> 0;
      if not HasBuild then
        I := Length(Z)+1;
      B := Copy(Z, I+1, Maxint);
      Z := Copy(Z, 1, I-1);
      J := StrToInt(Z);
      if (J < 0) or (J > 99) then Abort;
      Ver.Minor := J;
      if HasBuild then begin
        J := StrToInt(B);
        if (J < Low(Ver.Build)) or (J > High(Ver.Build)) then
          Abort;
        Ver.Build := J;
      end;
    end
    else begin  { no minor version specified }
      J := StrToInt(Z);
      if (J < 0) or (J > 127) then
        Abort;
      Ver.Major := J;
    end;
  end;
var
  I: Integer;
  SP: Word;
begin
  try
    VerData.WinVersion := 0;
    I := Pos(',', S);
    if I <> 0 then begin
      Split(Trim(Copy(S, 1, I-1)),
        TSetupVersionDataVersion(VerData.WinVersion), SP);
      if SP <> 0 then Abort;  { only NT has service packs }
    end;
    Split(Trim(Copy(S, I+1, Maxint)),
      TSetupVersionDataVersion(VerData.NTVersion), VerData.NTServicePack);
    Result := True;
  except
    if (ExceptObject is EAbort) or (ExceptObject is EConvertError) then
      Result := False
    else
      raise;
  end;
end;

procedure HandleRenamedConstants(var Cnst: String; const RenamedConstantCallback: TRenamedConstantCallback);
var
  CnstRenamed: String;
begin
  if Cnst = 'fonts' then
    CnstRenamed := 'commonfonts'
  else if Cnst = 'sendto' then
    CnstRenamed := 'usersendto'
  else if Cnst = 'pf' then
    CnstRenamed := 'commonpf'
  else if Cnst = 'pf32' then
    CnstRenamed := 'commonpf32'
  else if Cnst = 'pf64' then
    CnstRenamed := 'commonpf64'
  else if Cnst = 'cf' then
    CnstRenamed := 'commoncf'
  else if Cnst = 'cf32' then
    CnstRenamed := 'commoncf32'
  else if Cnst = 'cf64' then
    CnstRenamed := 'commoncf64'
  else
    CnstRenamed := '';

  if CnstRenamed <> '' then begin
    if Assigned(RenamedConstantCallback) then
      RenamedConstantCallback(Cnst, CnstRenamed);
    Cnst := CnstRenamed;
  end;
end;

procedure GenerateEncryptionKey(const Password: String; const Salt: TSetupKDFSalt;
  const Iterations: Integer; out Key: TSetupEncryptionKey);
begin
  var SaltBytes: TBytes;
  var SaltSize := SizeOf(Salt);
  SetLength(SaltBytes, SaltSize);
  Move(Salt[0], SaltBytes[0], SaltSize);
  var KeyLength := SizeOf(Key);
  var KeyBytes := PBKDF2SHA256(Password, SaltBytes, Iterations, KeyLength);
  Move(KeyBytes[0], Key[0], KeyLength);
end;

end.
