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
  { AutoComplete words lists are strings for Scintilla, using the following
    separators }
  AutoCompleteWordListSeparator = #9;
  AutoCompleteWordListTypeSeparator = '!'; { Must sort before numbers - so the default '?' is not ok }

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

var
  ISPPDirectivesAutoCompleteWordList: AnsiString;
  ISPPPragmaAutoCompleteWordList: AnsiString;
  SectionsAutoCompleteWordList: AnsiString;

procedure AddAutoCompleteWordToList(const SL: TStringList; const Word: AnsiString;
  const Typ: Integer);

function BuildAutoCompleteWordList(const Values: array of AnsiString): AnsiString; overload;

function BuildAutoCompleteWordList(const WordStringList: TStringList): AnsiString; overload;

function GetEventFunctionsAutoCompleteWordList(Procedures: Boolean): AnsiString;

implementation

uses
  SysUtils,
  Shared.ScriptFunc,
  IDE.ScriptModel.Metadata.Extra;

var
  FEventFunctionsAutoCompleteWordList: array[Boolean] of AnsiString;

procedure AddAutoCompleteWordToList(const SL: TStringList;
  const Word: AnsiString; const Typ: Integer);
begin
  if Typ >= 0 then
    SL.Add(Format('%s%s%d', [Word, AutoCompleteWordListTypeSeparator, Typ]))
  else
    SL.Add(String(Word));
end;

function BuildAutoCompleteWordList(const Values: array of AnsiString): AnsiString;
begin
  const SL = TStringList.Create;
  try
    for var Value in Values do
      AddAutoCompleteWordToList(SL, Value, awtFlagOrSetupDirectiveValue);
    Result := BuildAutoCompleteWordList(SL);
  finally
    SL.Free;
  end;
end;

function BuildAutoCompleteWordList(const WordStringList: TStringList): AnsiString;
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
      Result := Result + AutoCompleteWordListSeparator + A;
  end;
end;

procedure BuildSectionsAutoCompleteWordList;
begin
  var SL := TStringList.Create;
  try
    for var Section in SectionMap do
      AddAutoCompleteWordToList(SL, '[' + AnsiString(Section.Name) + ']', awtSection);
    SectionsAutoCompleteWordList := BuildAutoCompleteWordList(SL);
  finally
    SL.Free;
  end;
end;

procedure BuildISPPDirectivesAutoCompleteWordList;
begin
  var SL := TStringList.Create;
  try
    for var ISPPDirective in ISPPDirectives do
      AddAutoCompleteWordToList(SL, '#' + ISPPDirective.Name, awtPreprocessorDirective);
    ISPPDirectivesAutoCompleteWordList := BuildAutoCompleteWordList(SL);
  finally
    SL.Free;
  end;
end;

procedure BuildISPPPragmaAutoCompleteWordList;
begin
  var SL := TStringList.Create;
  try
    for var ISPPPragmaSubDirective in ISPPPragmaSubDirectives do
      AddAutoCompleteWordToList(SL, ISPPPragmaSubDirective, awtPreprocessorSubDirective);
    ISPPPragmaAutoCompleteWordList := BuildAutoCompleteWordList(SL);
  finally
    SL.Free;
  end;
end;

procedure BuildEventFunctionsAutoCompleteWordList;
begin
  var SLFunctions: TStringList := nil;
  var SLProcedures: TStringList := nil;
  try
    SLFunctions := TStringList.Create;
    SLProcedures := TStringList.Create;
    for var FullEventFunction in FullEventFunctions do begin
      var HeaderKind: TScriptFuncHeaderKind;
      var S := RemoveScriptFuncHeader(FullEventFunction, HeaderKind);
      if HeaderKind = hkFunction then
        AddAutoCompleteWordToList(SLFunctions, S, awtScriptEvent)
      else if HeaderKind = hkProcedure then
        AddAutoCompleteWordToList(SLProcedures, S, awtScriptEvent)
      else
        raise Exception.Create('Internal error: got invalid HeaderKind for event function');
    end;
    FEventFunctionsAutoCompleteWordList[False] := BuildAutoCompleteWordList(SLFunctions);
    FEventFunctionsAutoCompleteWordList[True] := BuildAutoCompleteWordList(SLProcedures);
  finally
    SLProcedures.Free;
    SLFunctions.Free;
  end;
end;

function GetEventFunctionsAutoCompleteWordList(Procedures: Boolean): AnsiString;
begin
  Result := FEventFunctionsAutoCompleteWordList[Procedures];
end;

initialization
  BuildEventFunctionsAutoCompleteWordList;
  BuildISPPDirectivesAutoCompleteWordList;
  BuildISPPPragmaAutoCompleteWordList;
  BuildSectionsAutoCompleteWordList;
end.
