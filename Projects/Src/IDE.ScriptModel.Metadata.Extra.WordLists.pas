unit IDE.ScriptModel.Metadata.Extra.WordLists;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Word lists for auto completion and other purposes
}

interface

uses
  Classes;

const
  InnoSetupStylerWordListSeparator = #9;
  InnoSetupStylerWordListTypeSeparator = '!'; { Must sort before numbers - so the default '?' is not ok }

  { AutoComplete word types }
  awtSection = 0;
  awtParameter = 1;
  awtDirective = 2;
  awtFlagOrSetupDirectiveValue = 3;
  awtPreprocessorDirective = 4;
  awtPreprocessorSubDirective = 5;
  awtConstant = 6;
  awtScriptFunction = 10;
  awtScriptType = 11;
  awtScriptVariable = 12;
  awtScriptConstant = 13;
  awtScriptInterface = 14;
  awtScriptProperty = 15;
  awtScriptEvent = 16;
  awtScriptKeyword = 17;
  awtScriptEnumValue = 18;
  awtISPPFunction = 30;
  awtISPPVariable = 31;
  awtISPPConstant = 32;

procedure AddWordToList(const SL: TStringList; const Word: AnsiString;
  const Typ: Integer);

function BuildWordList(const Values: array of AnsiString): AnsiString; overload;

function BuildWordList(const WordStringList: TStringList): AnsiString; overload;

implementation

uses
  SysUtils;

procedure AddWordToList(const SL: TStringList;
  const Word: AnsiString; const Typ: Integer);
begin
  if Typ >= 0 then
    SL.Add(Format('%s%s%d', [Word, InnoSetupStylerWordListTypeSeparator, Typ]))
  else
    SL.Add(String(Word));
end;

function BuildWordList(const Values: array of AnsiString): AnsiString;
begin
  const SL = TStringList.Create;
  try
    for var Value in Values do
      AddWordToList(SL, Value, awtFlagOrSetupDirectiveValue);
    Result := BuildWordList(SL);
  finally
    SL.Free;
  end;
end;

function BuildWordList(const WordStringList: TStringList): AnsiString;
begin
  { Scintilla uses an ASCII binary search so the list must be in ASCII sort
    order (case-insensitive). }
  WordStringList.CaseSensitive := False;
  WordStringList.UseLocale := False; { Make sure it uses CompareText and not AnsiCompareText }
  WordStringList.Sort;

  Result := '';
  for var S in WordStringList do begin
    var A := AnsiString(S);
    if Result = '' then
      Result := A
    else
      Result := Result + InnoSetupStylerWordListSeparator + A;
  end;
end;

end.
