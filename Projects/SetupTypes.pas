unit SetupTypes;

{
  Inno Setup
  Copyright (C) 1997-2012 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Types and functions used by both ISCmplr-only and Setup-only units
}

interface

uses
  Classes;

type
  TSetupStep = (ssPreInstall, ssInstall, ssPostInstall, ssDone);

  TUninstallStep = (usAppMutexCheck, usUninstall, usPostUninstall, usDone);

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
  TShellFolderID = (sfDesktop, sfStartMenu, sfPrograms, sfStartup, sfSendTo,
    sfFonts, sfAppData, sfDocs, sfTemplates, sfFavorites, sfLocalAppData);

  TInstallOnThisVersionResult = (irInstall, irNotOnThisPlatform,
    irVerTooLow, irVerTooHigh);

const
  crHand = 1;

  CodeRootKeyFlagMask  = $7F000000;
  CodeRootKeyFlag32Bit = $01000000;
  CodeRootKeyFlag64Bit = $02000000;
  CodeRootKeyValidFlags = CodeRootKeyFlag32Bit or CodeRootKeyFlag64Bit;

function StringsToCommaString(const Strings: TStrings): String;
procedure SetStringsFromCommaString(const Strings: TStrings; const Value: String);

implementation

uses
  SysUtils, CmnFunc2;

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

end.
