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
  Classes, Generics.Collections,
  IDE.ScriptModel.Metadata.Extra;

{ Word lists for auto completion }

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
  ConstantsAutoCompleteWordList: AnsiString;
  ISPPDirectivesAutoCompleteWordList: AnsiString;
  ISPPPragmaAutoCompleteWordList: AnsiString;
  SectionsAutoCompleteWordList: AnsiString;

function GetEventFunctionsAutoCompleteWordList(Procedures: Boolean): AnsiString;
function GetMemberValuesAutoCompleteWordList(const Section: TInnoSetupSection;
  const MemberName: String): AnsiString;

procedure AddAutoCompleteWordToList(const SL: TStringList; const Word: AnsiString;
  const Typ: Integer);
function BuildAutoCompleteWordList(const Values: array of AnsiString): AnsiString; overload;
function BuildAutoCompleteWordList(const WordStringList: TStringList): AnsiString; overload;

{ Word lists for other purposes }

type
  TWordsBySection = TObjectDictionary<TInnoSetupSection, TStringList>;

var
  FlagsWords: TWordsBySection;

function CreateWordsBySectionList: TStringList;

{ Initialization }

procedure InitializeWordLists(const ISPPInstalled: Boolean);

implementation

uses
  SysUtils, Generics.Defaults,
  Shared.ScriptFunc, Shared.Struct,
  IDE.ScriptModel.Metadata;

var
  FISPPInstalled: Boolean;
  FEventFunctionsAutoCompleteWordList: array[Boolean] of AnsiString;
  MemberValuesAutoCompleteWordLists: TDictionary<String, AnsiString>;

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

procedure BuildConstantsAutoCompleteWordList;
begin
  var SL := TStringList.Create;
  try
    for var Constant in Constants do
      if Constant = '{' then
        AddAutoCompleteWordToList(SL, '{{', awtConstant)
      else
        AddAutoCompleteWordToList(SL, '{' + Constant + '}', awtConstant);
    if FISPPInstalled then begin
      AddAutoCompleteWordToList(SL, '{#', awtConstant);
      AddAutoCompleteWordToList(SL, '{#file ', awtConstant);
      for var ISPPPredefinedVariable in ISPPPredefinedVariables do
        AddAutoCompleteWordToList(SL, '{#' + ISPPPredefinedVariable + '}', awtConstant);
    end;
    for var ConstantWithParam in ConstantsWithParam do
      AddAutoCompleteWordToList(SL, AnsiString('{' + ConstantWithParam), awtConstant);
    ConstantsAutoCompleteWordList := BuildAutoCompleteWordList(SL);
  finally
    SL.Free;
  end;
end;

function MemberValuesKey(const Section: TInnoSetupSection;
  const MemberName: String): String;
begin
  Result := IntToStr(Ord(Section)) + ':' + MemberName;
end;

procedure BuildMemberValuesAutoCompleteWordListsAndFlagsWords;
begin
  { Builds MemberValuesAutoCompleteWordLists (for autocomplete) and FlagsWords (for
    flag validation) from all members having known values in the metadata.
    Such a member just works, except for one case needing an extra change
    in InitiateAutoComplete: values containing characters outside of
    InnoSetupStylerAutoCompleteStartOrContinueChars (like Permissions' '-')
    need extra continue chars set in ChooseWordList.
    Note: Flags has additional special treatment, validating the words
    before the caret, see FlagsWords and FoundNonFlagWord. }
  MemberValuesAutoCompleteWordLists := TDictionary<String, AnsiString>.Create(TIStringComparer.Ordinal);
  FlagsWords := TWordsBySection.Create([doOwnsValues]);
  for var Section := Low(TInnoSetupSection) to High(TInnoSetupSection) do
    FlagsWords.Add(Section, CreateWordsBySectionList);
  for var Item in SectionMap do begin
    if not (Item.Section in DirectiveSections + ParameterSections) then
      Continue; { [Messages], [CustomMessages], and [Code] have no metadata }
    var Metadata: TScriptModelSectionMetadata;
    if not TryGetScriptModelSectionMetadata(Item.Name, Metadata) then
      raise Exception.CreateFmt('Internal error: no script model metadata for section [%s]',
        [Item.Name]);
    for var Member in Metadata.Members do begin
      if Length(Member.KnownValues) = 0 then
        Continue;
      var Values: TArray<AnsiString>;
      SetLength(Values, Length(Member.KnownValues));
      for var I := 0 to High(Member.KnownValues) do
        Values[I] := AnsiString(Member.KnownValues[I]);
      MemberValuesAutoCompleteWordLists.Add(MemberValuesKey(Item.Section, Member.Name),
        BuildAutoCompleteWordList(Values));
      if Member.Name = 'Flags' then begin
        const SL = FlagsWords[Item.Section];
        for var Value in Values do
          SL.Add(String(Value));
      end;
    end;
    if Item.Section = scSetup then begin
      { The expression directives (like ArchitecturesAllowed) have no known
        values in the metadata, their values come from the extra metadata }
      for var DirectiveValue in SetupSectionExpressionDirectivesValues do
        MemberValuesAutoCompleteWordLists.Add(
          MemberValuesKey(scSetup, Metadata.Members[Ord(DirectiveValue.Directive)].Name),
          BuildAutoCompleteWordList(DirectiveValue.Values));
    end;
  end;
end;

function GetMemberValuesAutoCompleteWordList(const Section: TInnoSetupSection;
  const MemberName: String): AnsiString;
begin
  if not MemberValuesAutoCompleteWordLists.TryGetValue(MemberValuesKey(Section, MemberName), Result) then
    Result := '';
end;

function CreateWordsBySectionList: TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  Result.UseLocale := False; { Make sure it uses CompareText and not AnsiCompareText }
  Result.Sorted := True;
end;

procedure InitializeWordLists(const ISPPInstalled: Boolean);
begin
  FISPPInstalled := ISPPInstalled;
  BuildConstantsAutoCompleteWordList;
  BuildEventFunctionsAutoCompleteWordList;
  BuildISPPDirectivesAutoCompleteWordList;
  BuildISPPPragmaAutoCompleteWordList;
  BuildMemberValuesAutoCompleteWordListsAndFlagsWords;
  BuildSectionsAutoCompleteWordList;
end;

initialization
finalization
  FlagsWords.Free;
  MemberValuesAutoCompleteWordLists.Free;
end.
