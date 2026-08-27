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
  ISPPExpressionAutoCompleteWordList: AnsiString;
  ISPPPragmaAutoCompleteWordList: AnsiString;
  MemberNamesAutoCompleteWordList: array[TInnoSetupSection] of AnsiString;
  SectionsAutoCompleteWordList: AnsiString;

function GetEventFunctionsAutoCompleteWordList(const Procedures: Boolean): AnsiString;
function GetMemberValuesAutoCompleteWordList(const Section: TInnoSetupSection;
  const MemberName: String): AnsiString;
function GetScriptAutoCompleteWordList(const ClassOrRecordMembers: Boolean): AnsiString;

procedure AddAutoCompleteWordToList(const SL: TStringList;
  const Word: AnsiString; const Typ: Integer);
function BuildAutoCompleteWordList(const WordStringList: TStringList;
  const Sort: Boolean = True): AnsiString; overload;
function BuildAutoCompleteWordList(const Values: array of AnsiString;
  const Typ: Integer; const Sort: Boolean = True): AnsiString; overload;
function MergeAutoCompleteWordLists(const BaseWordList,
  ExtraWordList: AnsiString): AnsiString;

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
  SysUtils, StrUtils, TypInfo, Generics.Defaults,
  Shared.LangOptionsSectionDirectives, Shared.ScriptFunc,
  Shared.SetupMessageIDs, Shared.SetupSectionDirectives, Shared.Struct,
  IDE.ScriptModel.Metadata, IDE.ScriptModel.Metadata.Extra.FunctionDefinitions,
  isxclasses_wordlists_generated;

var
  WordListsInitialized: Boolean;
  EventFunctionsAutoCompleteWordList: array[Boolean] of AnsiString;
  MemberValuesAutoCompleteWordLists: TDictionary<String, AnsiString>;
  ScriptAutoCompleteWordList: array[Boolean] of AnsiString;

procedure AddAutoCompleteWordToList(const SL: TStringList;
  const Word: AnsiString; const Typ: Integer);
begin
  if Typ >= 0 then
    SL.Add(Format('%s%s%d', [Word, AutoCompleteWordListTypeSeparator, Typ]))
  else
    SL.Add(String(Word));
end;

function BuildAutoCompleteWordList(const WordStringList: TStringList;
  const Sort: Boolean = True): AnsiString;
begin
  if Sort then begin
    { Scintilla uses an ASCII binary search so the list must be in ASCII sort
      order (case-insensitive) }
    WordStringList.CaseSensitive := False;
    WordStringList.UseLocale := False; { Make sure it uses CompareText and not AnsiCompareText }
    WordStringList.Sort;
  end;

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
  const Typ: Integer; const Sort: Boolean = True): AnsiString;
begin
  const SL = TStringList.Create;
  try
    for var Value in Values do
      AddAutoCompleteWordToList(SL, Value, Typ);
    Result := BuildAutoCompleteWordList(SL, Sort);
  finally
    SL.Free;
  end;
end;

function MergeAutoCompleteWordLists(const BaseWordList,
  ExtraWordList: AnsiString): AnsiString;
begin
  { Merges two word lists which are both in BuildAutoCompleteWordList's
    sort order into one list in that order, case-insensitively deduped on the
    full entry (word plus type suffix): an extra entry colliding with a base
    entry or an already merged extra entry is dropped }
  if ExtraWordList = '' then
    Exit(BaseWordList);

  const BaseEntries = SplitString(String(BaseWordList), AutoCompleteWordListSeparator);
  const ExtraEntries = SplitString(String(ExtraWordList), AutoCompleteWordListSeparator);

  Result := '';
  var LastMergedEntry := '';
  var BaseIndex := 0;
  var ExtraIndex := 0;
  while (BaseIndex < Length(BaseEntries)) or (ExtraIndex < Length(ExtraEntries)) do begin
    var Entry: String;
    if (ExtraIndex >= Length(ExtraEntries)) or
       ((BaseIndex < Length(BaseEntries)) and
        (CompareText(BaseEntries[BaseIndex], ExtraEntries[ExtraIndex]) <= 0)) then begin
      Entry := BaseEntries[BaseIndex];
      Inc(BaseIndex);
    end else begin
      Entry := ExtraEntries[ExtraIndex];
      Inc(ExtraIndex);
      if CompareText(Entry, LastMergedEntry) = 0 then
        Continue;
    end;
    if Result = '' then
      Result := AnsiString(Entry)
    else
      Result := Result + AutoCompleteWordListSeparator + AnsiString(Entry);
    LastMergedEntry := Entry;
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

function GetScriptAutoCompleteWordList(const ClassOrRecordMembers: Boolean): AnsiString;
begin
  Result := ScriptAutoCompleteWordList[ClassOrRecordMembers];
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
      ConstantsAutoCompleteWordList := BuildAutoCompleteWordList(SL);
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
      EventFunctionsAutoCompleteWordList[False] := BuildAutoCompleteWordList(SLFunctions);
      EventFunctionsAutoCompleteWordList[True] := BuildAutoCompleteWordList(SLProcedures);
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

  procedure BuildISPPExpressionAutoCompleteWordList;
  begin
    const SL = TStringList.Create;
    try
      for var ISPPFunctionName in ISPPFunctionsByName.Keys do
        AddAutoCompleteWordToList(SL, AnsiString(ISPPFunctionName), awtISPPFunction);
      for var ISPPPredefinedVariable in ISPPPredefinedVariables do
        AddAutoCompleteWordToList(SL, ISPPPredefinedVariable, awtISPPVariable);
      for var ISPPConstant in ISPPConstants do
        AddAutoCompleteWordToList(SL, ISPPConstant, awtISPPConstant);
      ISPPExpressionAutoCompleteWordList := BuildAutoCompleteWordList(SL);
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
      MemberNamesAutoCompleteWordList[Section] := BuildAutoCompleteWordList(SL2);
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
      MemberNamesAutoCompleteWordList[Section] := BuildAutoCompleteWordList(SL2);
    finally
      SL2.Free;
    end;
  end;

  procedure BuildMemberNamesAutoCompleteWordLists;
  begin
    { Builds MemberNamesAutoCompleteWordList (for autocomplete) and NoHighlightAtCursorWords }
    NoHighlightAtCursorWords := TWordsBySection.Create([doOwnsValues]);
    for var Section := Low(TInnoSetupSection) to High(TInnoSetupSection) do
      NoHighlightAtCursorWords.Add(Section, CreateWordsBySectionList);
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
      AutoCompleteStartOrContinueChars (like Permissions' '-') need extra continue
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
          BuildAutoCompleteWordList(Values, awtMemberValue,
            not Member.KnownValuesCustomSorted));
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

  procedure BuildScriptAutoCompleteWordListsAndNoHighlightAtCursorWords;
  begin
    { Builds ScriptAutoCompleteWordList (for autocomplete) and NoHighlightAtCursorWords for [Code] }
    const SL1 = NoHighlightAtCursorWords[scCode];
    const SL2 = TStringList.Create;
    try
      { Add stuff from ScriptFunc }
      for var ScriptFuncName in ScriptFunctionsByName[False].Keys do
        AddAutoCompleteWordToList(SL2, AnsiString(ScriptFuncName), awtScriptFunction);
      { Add stuff from Metadata.Extra and Isxclasses }
      for var S in PascalConstants do
        AddAutoCompleteWordToList(SL2, S, awtScriptConstant);
      for var S in PascalConstants_Isxclasses do
        AddAutoCompleteWordToList(SL2, S, awtScriptConstant);
      for var S in PascalInterfaces do
        AddAutoCompleteWordToList(SL2, S, awtScriptInterface);
      for var S in PascalReservedWords do begin
        SL1.Add(String(S));
        AddAutoCompleteWordToList(SL2, S, awtScriptKeyword);
      end;
      for var S in PascalTypes do
        AddAutoCompleteWordToList(SL2, S, awtScriptType);
      for var S in PascalTypes_Isxclasses do
        AddAutoCompleteWordToList(SL2, S, awtScriptType);
      for var S in PascalEnumValues do
        AddAutoCompleteWordToList(SL2, S, awtScriptEnumValue);
      for var S in PascalEnumValues_Isxclasses do
        AddAutoCompleteWordToList(SL2, S, awtScriptEnumValue);
      for var TypeInfo in PascalRealEnumValues do begin
        var TypeData := GetTypeData(TypeInfo);
        for var I := TypeData.MinValue to TypeData.MaxValue do
          AddAutoCompleteWordToList(SL2, AnsiString(GetEnumName(TypeInfo, I)), awtScriptEnumValue);
      end;
      for var S in PascalVariables do
        AddAutoCompleteWordToList(SL2, S, awtScriptVariable);
      for var S in EventFunctionsParameters  do
        AddAutoCompleteWordToList(SL2, S, awtScriptVariable);
      ScriptAutoCompleteWordList[False] := BuildAutoCompleteWordList(SL2);

      { Add stuff from Isxclasses }
      SL2.Clear;
      for var ScriptFuncName in ScriptFunctionsByName[True].Keys do
        AddAutoCompleteWordToList(SL2, AnsiString(ScriptFuncName), awtScriptFunction);
      for var S in PascalProperties_Isxclasses do
        AddAutoCompleteWordToList(SL2, S, awtScriptProperty);
      ScriptAutoCompleteWordList[True] := BuildAutoCompleteWordList(SL2);
    finally
      SL2.Free;
    end;
  end;

  procedure BuildSectionsAutoCompleteWordList;
  begin
    var SL := TStringList.Create;
    try
      for var Section in SectionMap do
        AddAutoCompleteWordToList(SL, '[' + AnsiString(Section.Name) + ']', awtSectionName);
      SectionsAutoCompleteWordList := BuildAutoCompleteWordList(SL);
    finally
      SL.Free;
    end;
  end;

begin
  if WordListsInitialized then
    Exit;
  WordListsInitialized := True;

  { Needed by BuildISPPExpressionAutoCompleteWordList and
    BuildScriptAutoCompleteWordListsAndNoHighlightAtCursorWords }
  InitializeFunctionDefinitions;

  BuildSectionParameterNameLists;
  BuildConstantsAutoCompleteWordList;
  BuildEventFunctionsAutoCompleteWordList;
  BuildISPPDirectivesAutoCompleteWordList;
  BuildISPPPragmaAutoCompleteWordList;
  BuildISPPExpressionAutoCompleteWordList;
  BuildMemberNamesAutoCompleteWordLists; { Requires BuildSectionParameterNameLists }
  BuildMemberValuesAutoCompleteWordListsAndFlagsWords;
  BuildScriptAutoCompleteWordListsAndNoHighlightAtCursorWords;
  BuildSectionsAutoCompleteWordList;
end;

initialization
finalization
  FlagsWords.Free;
  MemberValuesAutoCompleteWordLists.Free;
  NoHighlightAtCursorWords.Free;
end.
