unit IDE.ScriptModel.Metadata.Extra.WordLists;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Word lists for auto completion and other purposes, not used by IDE.ScriptModel.pas
}

interface

uses
  Classes, Generics.Collections,
  IDE.ScriptModel.Metadata.Extra;

{ Word lists for auto completion }

const
  AutoCompleteWordListSeparator = #9;
  AutoCompleteWordListTypeSeparator = '!'; { Must sort before numbers }

  { AutoComplete word types }
  awtSectionName = 0;
  awtParameterName = 1;
  awtKeyName = 2;
  awtMemberValue = 3;
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
  MemberNamesAutoCompleteWordList: array[TInnoSetupSection] of AnsiString;
  SectionsAutoCompleteWordList: AnsiString;

function GetEventFunctionsAutoCompleteWordList(const Procedures: Boolean): AnsiString;
function GetMemberValuesAutoCompleteWordList(const Section: TInnoSetupSection;
  const MemberName: String): AnsiString;

procedure AddAutoCompleteWordToList(const SL: TStringList; const Word: AnsiString;
  const Typ: Integer);
function BuildAutoCompleteWordList(const Values: array of AnsiString;
  const Typ: Integer): AnsiString;
function InternalBuildAutoCompleteWordList(const WordStringList: TStringList): AnsiString;

{ Word lists for other purposes }

type
  TWordsBySection = TObjectDictionary<TInnoSetupSection, TStringList>;

var
  FlagsWords: TWordsBySection;
  NoHighlightAtCursorWords: TWordsBySection;
  ParameterNames: array[TInnoSetupSection] of TArray<AnsiString>;

{ Initialization }

procedure InitializeWordLists(const ISPPInstalled: Boolean);

implementation

uses
  SysUtils, TypInfo, Generics.Defaults,
  Shared.LangOptionsSectionDirectives, Shared.ScriptFunc,
  Shared.SetupMessageIDs, Shared.SetupSectionDirectives, Shared.Struct,
  IDE.ScriptModel.Metadata;

var
  EventFunctionsAutoCompleteWordList: array[Boolean] of AnsiString;
  MemberValuesAutoCompleteWordLists: TDictionary<String, AnsiString>;

procedure AddAutoCompleteWordToList(const SL: TStringList;
  const Word: AnsiString; const Typ: Integer);
begin
  if Typ >= 0 then
    SL.Add(Format('%s%s%d', [Word, AutoCompleteWordListTypeSeparator, Typ]))
  else
    SL.Add(String(Word));
end;

function InternalBuildAutoCompleteWordList(const WordStringList: TStringList): AnsiString;
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

function BuildAutoCompleteWordList(const Values: array of AnsiString;
  const Typ: Integer): AnsiString;
begin
  const SL = TStringList.Create;
  try
    for var Value in Values do
      AddAutoCompleteWordToList(SL, Value, Typ);
    Result := InternalBuildAutoCompleteWordList(SL);
  finally
    SL.Free;
  end;
end;

function GetEventFunctionsAutoCompleteWordList(const Procedures: Boolean): AnsiString;
begin
  Result := EventFunctionsAutoCompleteWordList[Procedures];
end;

function MemberValuesKey(const Section: TInnoSetupSection;
  const MemberName: String): String;
begin
  Result := IntToStr(Ord(Section)) + ':' + MemberName;
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

  procedure BuildSectionParameterNameLists;
  begin
    for var Item in SectionMap do begin
      if not (Item.Section in ParameterSections) then
        Continue;
      var Metadata: TScriptModelSectionMetadata;
      if not TryGetScriptModelSectionMetadata(Item.Name, Metadata) then
        raise Exception.CreateFmt('Internal error: no script model metadata for section [%s]',
          [Item.Name]);
      var SectionParameterNames: TArray<AnsiString>;
      SetLength(SectionParameterNames, Length(Metadata.Members));
      var N := 0;
      for var Member in Metadata.Members do begin
        if (Item.Section = scUninstallRun) and (Member.Name = 'StatusMsg') then
          Continue;
        SectionParameterNames[N] := AnsiString(Member.Name);
        Inc(N);
      end;
      SetLength(SectionParameterNames, N);
      ParameterNames[Item.Section] := SectionParameterNames;
    end;
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
      if ISPPInstalled then begin
        AddAutoCompleteWordToList(SL, '{#', awtConstant);
        AddAutoCompleteWordToList(SL, '{#file ', awtConstant);
        for var ISPPPredefinedVariable in ISPPPredefinedVariables do
          AddAutoCompleteWordToList(SL, '{#' + ISPPPredefinedVariable + '}', awtConstant);
      end;
      for var ConstantWithParam in ConstantsWithParam do
        AddAutoCompleteWordToList(SL, AnsiString('{' + ConstantWithParam), awtConstant);
      ConstantsAutoCompleteWordList := InternalBuildAutoCompleteWordList(SL);
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
      EventFunctionsAutoCompleteWordList[False] := InternalBuildAutoCompleteWordList(SLFunctions);
      EventFunctionsAutoCompleteWordList[True] := InternalBuildAutoCompleteWordList(SLProcedures);
    finally
      SLProcedures.Free;
      SLFunctions.Free;
    end;
  end;

  procedure BuildISPPDirectivesAutoCompleteWordList;
  begin
    var SL := TStringList.Create;
    try
      for var ISPPDirective in ISPPDirectives do
        AddAutoCompleteWordToList(SL, '#' + ISPPDirective.Name, awtPreprocessorDirective);
      ISPPDirectivesAutoCompleteWordList := InternalBuildAutoCompleteWordList(SL);
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
      ISPPPragmaAutoCompleteWordList := InternalBuildAutoCompleteWordList(SL);
    finally
      SL.Free;
    end;
  end;

  procedure BuildMemberNamesAutoCompleteWordListFromParameterNames(
    const Section: TInnoSetupSection);
  begin
    const SL1 = NoHighlightAtCursorWords[Section];
    const SL2 = TStringList.Create;
    try
      for var ParameterName in ParameterNames[Section] do begin
        SL1.Add(String(ParameterName));
        AddAutoCompleteWordToList(SL2, ParameterName, awtParameterName);
      end;
      MemberNamesAutoCompleteWordList[Section] := InternalBuildAutoCompleteWordList(SL2);
    finally
      SL2.Free;
    end;
  end;

  procedure BuildMemberNamesAutoCompleteWordListFromTypeInfo(
    const Section: TInnoSetupSection; const EnumTypeInfo: Pointer;
    const PrefixLength: Integer);
  begin
    const SL1 = NoHighlightAtCursorWords[Section];
    const SL2 = TStringList.Create;
    try
      for var I := 0 to GetTypeData(EnumTypeInfo).MaxValue do begin
        const KeyName = Copy(GetEnumName(EnumTypeInfo, I), PrefixLength+1, MaxInt);
        SL1.Add(KeyName);
        AddAutoCompleteWordToList(SL2, AnsiString(KeyName), awtKeyName);
      end;
      MemberNamesAutoCompleteWordList[Section] := InternalBuildAutoCompleteWordList(SL2);
    finally
      SL2.Free;
    end;
  end;

  procedure BuildMemberNamesAutoCompleteWordLists;
  begin
    { Builds MemberNamesAutoCompleteWordList (for autocomplete) and NoHighlightAtCursorWords }
    for var Section in ParameterSections do
      BuildMemberNamesAutoCompleteWordListFromParameterNames(Section);
    BuildMemberNamesAutoCompleteWordListFromTypeInfo(scLangOptions, TypeInfo(TLangOptionsSectionDirective), LangOptionsSectionDirectivePrefixLength);
    BuildMemberNamesAutoCompleteWordListFromTypeInfo(scSetup, TypeInfo(TSetupSectionDirective), Length(SetupSectionDirectivePrefix));
    BuildMemberNamesAutoCompleteWordListFromTypeInfo(scMessages, TypeInfo(TSetupMessageID), SetupMessageIDPrefixLength);
  end;

  procedure BuildMemberValuesAutoCompleteWordListsAndFlagsWords;
  begin
    { Builds MemberValuesAutoCompleteWordLists (for autocomplete) and FlagsWords
      (for flag validation) from all members having known values in the metadata.
      Such a member just works, except for one case needing an extra change
      in InitiateAutoComplete: values containing characters outside of
      AutoCompleteStartOrContinueChars (like Permissions' '-' need extra continue
      chars set in ChooseWordList.
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
          BuildAutoCompleteWordList(Values, awtMemberValue));
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
            BuildAutoCompleteWordList(DirectiveValue.Values, awtMemberValue));
      end;
    end;
  end;

  procedure BuildSectionsAutoCompleteWordList;
  begin
    var SL := TStringList.Create;
    try
      for var Section in SectionMap do
        AddAutoCompleteWordToList(SL, '[' + AnsiString(Section.Name) + ']', awtSectionName);
      SectionsAutoCompleteWordList := InternalBuildAutoCompleteWordList(SL);
    finally
      SL.Free;
    end;
  end;

begin
  BuildSectionParameterNameLists;
  BuildConstantsAutoCompleteWordList;
  BuildEventFunctionsAutoCompleteWordList;
  BuildISPPDirectivesAutoCompleteWordList;
  BuildISPPPragmaAutoCompleteWordList;
  BuildMemberNamesAutoCompleteWordLists; { Requires BuildSectionParameterNameLists }
  BuildMemberValuesAutoCompleteWordListsAndFlagsWords;
  BuildSectionsAutoCompleteWordList;
end;

initialization
  NoHighlightAtCursorWords := TWordsBySection.Create([doOwnsValues]);
  for var Section := Low(TInnoSetupSection) to High(TInnoSetupSection) do
    NoHighlightAtCursorWords.Add(Section, CreateWordsBySectionList);
finalization
  FlagsWords.Free;
  MemberValuesAutoCompleteWordLists.Free;
  NoHighlightAtCursorWords.Free;
end.
