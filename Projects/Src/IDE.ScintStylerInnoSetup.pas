unit IDE.ScintStylerInnoSetup;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TInnoSetupStyler: styler for Inno Setup scripts
}

interface

uses
  SysUtils, Classes, Graphics, Generics.Collections, TypInfo,
  ScintEdit, ModernColors, Shared.ScriptFunc, Shared.SetupSectionDirectives,
  IDE.ScriptModel.Metadata.Extra;

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

type
  { Internally-used types }
  TInnoSetupStylerSpanState = (spNone, spBraceComment, spStarComment);

  { Starts at 1 instead of 0 to make sure ApplyStyle doesn't overwrite already applied stDefault
    styles which is needed for PreStyleInlineISPPDirectives to work properly when the inline
    directive is inside a comment or string. This is done by added a dummy 'st0' style. If done by
    using 'stDefault = 1' then this enum looses its TypeInfo. }
  TInnoSetupStylerStyle = (st0, stDefault, stCompilerDirective,
    stComment, stSection, stSymbol, stKeyword, stParameterValue,
    stEventFunction, stConstant, stMessageArg,
    stPascalReservedWord, stPascalString, stPascalNumber,
    stISPPReservedWord, stISPPString, stISPPNumber);

  TWordsBySection = TObjectDictionary<TInnoSetupSection, TStringList>;
  TFunctionDefinition = record
    ScriptFuncWithoutHeader: AnsiString;
    HeaderKind: TScriptFuncHeaderKind;
    HasParams: Boolean;
    constructor Create(const ScriptFunc: AnsiString);
    {$WARN DUPLICATE_CTOR_DTOR OFF} { Don't care about C++ }
    constructor CreateISPP(const ISPPScriptFunc: AnsiString);
    {.$WARN DUPLICATE_CTOR_DTOR ON} { Restoring doesn't work }
  end;
  TFunctionDefinitions = array of TFunctionDefinition;
  TFunctionDefinitionsByName = TDictionary<String, TFunctionDefinitions>;

  TInnoSetupStyler = class(TScintCustomStyler)
  private
    FSectionParameters: array[TInnoSetupSection] of TArray<TScintRawString>;
    FEventFunctionsWordList: array[Boolean] of AnsiString;
    FKeywordsWordList, FFlagsWordList: array[TInnoSetupSection] of AnsiString;
    FNoHighlightAtCursorWords: TWordsBySection;
    FFlagsWords: TWordsBySection;
    FISPPDirectivesWordList, FISPPPragmaWordList, FConstantsWordList: AnsiString;
    FISPPFunctionsByName: TFunctionDefinitionsByName;
    FISPPExpressionWordList: AnsiString;
    FScriptFunctionsByName: array[Boolean] of TFunctionDefinitionsByName;
    FScriptWordList: array[Boolean] of AnsiString;
    FSectionsWordList: AnsiString;
    FSetupSectionDirectiveValueWordList: array[TSetupSectionDirective] of AnsiString;
    FISPPInstalled: Boolean;
    FTheme: TTheme;
    procedure AddWordToList(const SL: TStringList; const Word: AnsiString;
      const Typ: Integer);
    procedure ApplyPendingSquigglyFromToIndex(const StartIndex, EndIndex: Integer);
    procedure ApplyPendingSquigglyFromIndex(const StartIndex: Integer);
    procedure ApplySquigglyFromIndex(const StartIndex: Integer);
    procedure BuildConstantsWordList;
    procedure BuildEventFunctionsWordList;
    procedure BuildFlagsWordList(const Section: TInnoSetupSection;
     const Flags: array of TScintRawString);
    procedure BuildISPPDirectivesWordList;
    procedure BuildISPPPragmaWordList;
    procedure BuildISPPExpressionWordList;
    procedure BuildKeywordsWordList(const Section: TInnoSetupSection;
      const Parameters: array of TScintRawString);
    procedure BuildKeywordsWordListFromTypeInfo(const Section: TInnoSetupSection;
      const EnumTypeInfo: Pointer; const PrefixLength: Integer);
    procedure BuildScriptFunctionsLists(const ScriptFuncTable: TScriptTable;
      const ClassMembers: Boolean; const SL: TStringList);
    procedure BuildSectionsWordList;
    function BuildWordList(const Values: array of TScintRawString): AnsiString; overload;
    function BuildWordList(const WordStringList: TStringList): AnsiString; overload;
    procedure CommitStyleSq(const Style: TInnoSetupStylerStyle;
      const Squigglify: Boolean);
    procedure CommitStyleSqPending(const Style: TInnoSetupStylerStyle);
    function GetEventFunctionsWordList(Procedures: Boolean): AnsiString;
    function GetFlagsWordList(Section: TInnoSetupSection): AnsiString;
    class function GetFunctionDefinition(const FunctionsByName: TFunctionDefinitionsByName;
      const Name: String; const Index: Integer; out Count: Integer): TFunctionDefinition; static;
    function GetKeywordsWordList(Section: TInnoSetupSection): AnsiString;
    procedure HandleCodeSection(var SpanState: TInnoSetupStylerSpanState; var CodeBlockHeader: Boolean);
    procedure HandleKeyValueSection(const Section: TInnoSetupSection);
    procedure HandleParameterSection(const ValidParameters: array of TScintRawString);
    procedure HandleCompilerDirective(const InlineDirective: Boolean;
      const InlineDirectiveEndIndex: Integer; var OpenCount: ShortInt);
    procedure PreStyleInlineISPPDirectives;
    procedure SkipWhitespace;
    procedure SquigglifyUntilChars(const Chars: TScintRawCharSet;
      const Style: TInnoSetupStylerStyle);
    procedure StyleConstsUntilChars(const Chars: TScintRawCharSet;
      const NonConstStyle: TInnoSetupStylerStyle; var BraceLevel: Integer);
    procedure SetISPPInstalled(const Value: Boolean);
    function GetScriptWordList(ClassOrRecordMembers: Boolean): AnsiString;
    function GetSetupSectionDirectiveValueIsMultiValue(SetupSectionDirective: TSetupSectionDirective): Boolean;
    function GetSetupSectionDirectiveValueWordList(SetupSectionDirective: TSetupSectionDirective): AnsiString;
  protected
    procedure CommitStyle(const Style: TInnoSetupStylerStyle);
    procedure GetFoldLevel(const LineState, PreviousLineState: TScintLineState;
      var Level: Integer; var Header, EnableHeaderOnPrevious: Boolean); override;
    procedure GetStyleAttributes(const Style: Integer;
      var Attributes: TScintStyleAttributes); override;
    function LineTextSpans(const S: TScintRawString): Boolean; override;
    procedure StyleNeeded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetSectionFromLineState(const LineState: TScintLineState; const ReturnCodeBlockAsCode: Boolean = True): TInnoSetupSection; static;
    class function IsCommentOrKeywordStyle(const Style: TScintStyleNumber): Boolean; static;
    class function IsCommentOrISPPStringStyle(const Style: TScintStyleNumber): Boolean; static;
    class function IsCommentOrPascalStringStyle(const Style: TScintStyleNumber): Boolean; static;
    class function IsISPPIdentChar(const C: AnsiChar): Boolean; static;
    class function IsParamSection(const Section: TInnoSetupSection): Boolean; static;
    class function IsSymbolStyle(const Style: TScintStyleNumber): Boolean; static;
    class function LineSectionHeader(const LineState: TScintLineState; out Section: TInnoSetupSection): Boolean; static;
    class function LineSpans(const S: TScintRawString): Boolean; static;
    function GetISPPFunctionDefinition(const Name: String;
      const Index: Integer; out Count: Integer): TFunctionDefinition;
    function GetScriptFunctionDefinition(const ClassMember: Boolean;
      const Name: String; const Index: Integer; out Count: Integer): TFunctionDefinition; overload;
    function GetScriptFunctionDefinition(const ClassMember: Boolean;
      const Name: String; const Index: Integer): TFunctionDefinition; overload;
    function SectionHasFlag(const Section: TInnoSetupSection; const Flag: String): Boolean;
    function HighlightAtCursorAllowed(const Section: TInnoSetupSection; const Word: String): Boolean;
    property ConstantsWordList: AnsiString read FConstantsWordList;
    property EventFunctionsWordList[Procedures: Boolean]: AnsiString read GetEventFunctionsWordList;
    property FlagsWordList[Section: TInnoSetupSection]: AnsiString read GetFlagsWordList;
    property ISPPDirectivesWordList: AnsiString read FISPPDirectivesWordList;
    property ISPPPragmaWordList: AnsiString read FISPPPragmaWordList;
    property ISPPExpressionWordList: AnsiString read FISPPExpressionWordList;
    property ISPPInstalled: Boolean read FISPPInstalled write SetISPPInstalled;
    property KeywordsWordList[Section: TInnoSetupSection]: AnsiString read GetKeywordsWordList;
    property ScriptWordList[ClassOrRecordMembers: Boolean]: AnsiString read GetScriptWordList;
    property SectionsWordList: AnsiString read FSectionsWordList;
    property SetupSectionDirectiveValueIsMultiValue[SetupSectionDirective: TSetupSectionDirective]: Boolean read GetSetupSectionDirectiveValueIsMultiValue;
    property SetupSectionDirectiveValueWordList[SetupSectionDirective: TSetupSectionDirective]: AnsiString read GetSetupSectionDirectiveValueWordList;
    property Theme: TTheme read FTheme write FTheme;
  end;

implementation

uses
  Generics.Defaults,
  Shared.SetupMessageIDs, ScintInt, Shared.LangOptionsSectionDirectives,
  IDE.ScriptModel.Metadata,
  isxclasses_wordlists_generated;

type
  { Size must be <= SizeOf(TScintLineState) }
  TInnoSetupStylerLineState = record
    Section, NextLineSection: TInnoSetupSection;
    SpanState: TInnoSetupStylerSpanState;
    OpenCompilerDirectivesCount: ShortInt;
  end;

type
  TSectionMapItem = record
    Name: TScintRawString;
    Section: TInnoSetupSection;
  end;

var
  SectionMap: array of TSectionMapItem; { Initialized below }

const
  inSquiggly = 0;
  inPendingSquiggly = 1;

function SameRawText(const S1, S2: TScintRawString): Boolean;
var
  Len, I: Integer;
  C1, C2: AnsiChar;
begin
  Len := Length(S1);
  if Length(S2) <> Len then begin
    Result := False;
    Exit;
  end;
  for I := 1 to Len do begin
    C1 := S1[I];
    C2 := S2[I];
    if C1 in ['A'..'Z'] then
      Inc(C1, 32);
    if C2 in ['A'..'Z'] then
      Inc(C2, 32);
    if C1 <> C2 then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

{ TFunctionDefinition }

constructor TFunctionDefinition.Create(const ScriptFunc: AnsiString);
begin
  ScriptFuncWithoutHeader := RemoveScriptFuncHeader(ScriptFunc, HeaderKind);
  HasParams := ScriptFuncHasParameters(ScriptFunc);
end;

constructor TFunctionDefinition.CreateISPP(const ISPPScriptFunc: AnsiString);
begin
  ScriptFuncWithoutHeader := RemoveISPPScriptFuncHeader(ISPPScriptFunc, HeaderKind);
  HasParams := ScriptFuncHasParameters(ISPPScriptFunc);
end;

{ TInnoSetupStyler }

constructor TInnoSetupStyler.Create(AOwner: TComponent);

  procedure BuildSectionParameterLists;
  begin
    for var Item in SectionMap do begin
      if not (Item.Section in ParameterSections) then
        Continue;
      var Metadata: TScriptModelSectionMetadata;
      if not TryGetScriptModelSectionMetadata(String(Item.Name), Metadata) then
        raise Exception.CreateFmt('Internal error: no script model metadata for section [%s]',
          [String(Item.Name)]);
      var Parameters: TArray<TScintRawString>;
      SetLength(Parameters, Length(Metadata.Members));
      var N := 0;
      for var Member in Metadata.Members do begin
        if (Item.Section = scUninstallRun) and SameText(Member.Name, 'StatusMsg') then
          Continue;
        Parameters[N] := TScintRawString(Member.Name);
        Inc(N);
      end;
      SetLength(Parameters, N);
      FSectionParameters[Item.Section] := Parameters;
    end;
  end;

  procedure BuildFlagsWordLists;
  begin
    { Builds FFlagsWordList (for autocomplete) and FFlagsWords }
    for var Item in SectionMap do begin
      var Metadata: TScriptModelSectionMetadata;
      if not TryGetScriptModelSectionMetadata(String(Item.Name), Metadata) then
        Continue;
      var Member: TMemberDefinition;
      if (not Metadata.TryGetMember('Flags', Member) and
          not Metadata.TryGetMember('Type', Member)) or
         not (Member.ValueKind in [mvkFlags, mvkChoice]) then
        Continue;
      var Flags: TArray<TScintRawString>;
      SetLength(Flags, Length(Member.KnownValues));
      for var I := 0 to High(Member.KnownValues) do
        Flags[I] := TScintRawString(Member.KnownValues[I]);
      BuildFlagsWordList(Item.Section, Flags);
    end;
  end;

  procedure BuildKeywordsWordLists;
  begin
    { Builds FKeywordsWordList (for autocomplete) and FNoHighlightAtCursorWords }
    BuildKeywordsWordList(scISSigKeys, FSectionParameters[scISSigKeys]);
    BuildKeywordsWordList(scFiles, FSectionParameters[scFiles]);
    BuildKeywordsWordList(scComponents, FSectionParameters[scComponents]);
    BuildKeywordsWordList(scDirs, FSectionParameters[scDirs]);
    BuildKeywordsWordList(scIcons, FSectionParameters[scIcons]);
    BuildKeywordsWordList(scINI, FSectionParameters[scINI]);
    BuildKeywordsWordList(scInstallDelete, FSectionParameters[scInstallDelete]);
    BuildKeywordsWordListFromTypeInfo(scLangOptions, TypeInfo(TLangOptionsSectionDirective), LangOptionsSectionDirectivePrefixLength);
    BuildKeywordsWordList(scLanguages, FSectionParameters[scLanguages]);
    BuildKeywordsWordList(scRegistry, FSectionParameters[scRegistry]);
    BuildKeywordsWordList(scRun, FSectionParameters[scRun]);
    BuildKeywordsWordListFromTypeInfo(scSetup, TypeInfo(TSetupSectionDirective), Length(SetupSectionDirectivePrefix));
    BuildKeywordsWordList(scTasks, FSectionParameters[scTasks]);
    BuildKeywordsWordList(scTypes, FSectionParameters[scTypes]);
    BuildKeywordsWordList(scUninstallDelete, FSectionParameters[scUninstallDelete]);
    BuildKeywordsWordList(scUninstallRun, FSectionParameters[scUninstallRun]);
    BuildKeywordsWordListFromTypeInfo(scMessages, TypeInfo(TSetupMessageID), SetupMessageIDPrefixLength);
  end;

  procedure BuildScriptLists;
  begin
    { Builds FScriptFunctionsByName (for calltips) and FScriptWordList (for autocomplete)
       and FNoHighlightAtCursorWords }
    const SL1 = FNoHighlightAtCursorWords[scCode];
    const SL2 = TStringList.Create;
    try
      { Add stuff from ScriptFunc }
      var ClassMembers := False;
      for var ScriptFuncTable in ScriptFuncTables do
        BuildScriptFunctionsLists(ScriptFuncTable, ClassMembers, SL2);
      BuildScriptFunctionsLists(DelphiScriptFuncTable, ClassMembers, SL2);
      BuildScriptFunctionsLists(ROPSScriptFuncTable, ClassMembers, SL2);
      { Add stuff from this unit }
      for var S in PascalConstants do
        AddWordToList(SL2, S, awtScriptConstant);
      for var S in PascalConstants_Isxclasses do
        AddWordToList(SL2, S, awtScriptConstant);
      for var S in PascalInterfaces do
        AddWordToList(SL2, S, awtScriptInterface);
      for var S in PascalReservedWords do begin
        SL1.Add(String(S));
        AddWordToList(SL2, S, awtScriptKeyword);
      end;
      for var S in PascalTypes do
        AddWordToList(SL2, S, awtScriptType);
      for var S in PascalTypes_Isxclasses do
        AddWordToList(SL2, S, awtScriptType);
      for var S in PascalEnumValues do
        AddWordToList(SL2, S, awtScriptEnumValue);
      for var S in PascalEnumValues_Isxclasses do
        AddWordToList(SL2, S, awtScriptEnumValue);
      for var TypeInfo in PascalRealEnumValues do begin
        var TypeData := GetTypeData(TypeInfo);
        for var I := TypeData.MinValue to TypeData.MaxValue do
          AddWordToList(SL2, AnsiString(GetEnumName(TypeInfo, I)), awtScriptEnumValue);
      end;
      for var S in PascalVariables do
        AddWordToList(SL2, S, awtScriptVariable);
      for var S in EventFunctionsParameters  do
        AddWordToList(SL2, S, awtScriptVariable);
      FScriptWordList[False] := BuildWordList(SL2);

      { Add stuff from Isxclasses }
      SL2.Clear;
      ClassMembers := True;
      BuildScriptFunctionsLists(PascalMembers_Isxclasses, ClassMembers, SL2);
      for var S in PascalProperties_Isxclasses do
        AddWordToList(SL2, S, awtScriptProperty);
      FScriptWordList[True] := BuildWordList(SL2);
    finally
      SL2.Free;
    end;
  end;

  procedure BuildSetupDirectiveValueWordLists;
  begin
    var Metadata: TScriptModelSectionMetadata;
    if not TryGetScriptModelSectionMetadata('Setup', Metadata) then
      raise Exception.Create('Internal error: BuildSetupDirectiveValueWordLists: no metadata');
    for var Directive := Low(TSetupSectionDirective) to High(TSetupSectionDirective) do begin
      const KnownValues = Metadata.Members[Ord(Directive)].KnownValues;
      if KnownValues <> nil then begin
        var Values: TArray<TScintRawString>;
        SetLength(Values, Length(KnownValues));
        for var I := 0 to High(KnownValues) do
          Values[I] := TScintRawString(KnownValues[I]);
        FSetupSectionDirectiveValueWordList[Directive] := BuildWordList(Values);
      end;
    end;

    for var Item in SetupSectionExpressionDirectivesValues do
      FSetupSectionDirectiveValueWordList[Item.Directive] := BuildWordList(Item.Values);
  end;

  function CreateWordsBySectionList: TStringList;
  begin
    Result := TStringList.Create;
    Result.CaseSensitive := False;
    Result.UseLocale := False; { Make sure it uses CompareText and not AnsiCompareText }
    Result.Sorted := True;
  end;

begin
  inherited;
  BuildSectionParameterLists;
  FNoHighlightAtCursorWords := TWordsBySection.Create([doOwnsValues]);
  FFlagsWords := TWordsBySection.Create([doOwnsValues]);
  for var Section := Low(TInnoSetupSection) to High(TInnoSetupSection) do begin
    FNoHighlightAtCursorWords.Add(Section, CreateWordsBySectionList);
    FFlagsWords.Add(Section, CreateWordsBySectionList);
  end;
  BuildConstantsWordList;
  BuildEventFunctionsWordList;
  BuildFlagsWordLists;
  BuildISPPDirectivesWordList;
  BuildISPPPragmaWordList;
  FISPPFunctionsByName := TFunctionDefinitionsByName.Create(TIStringComparer.Ordinal);
  BuildISPPExpressionWordList;
  BuildKeywordsWordLists;
  BuildSectionsWordList;
  BuildSetupDirectiveValueWordLists;
  FScriptFunctionsByName[False] := TFunctionDefinitionsByName.Create(TIStringComparer.Ordinal);
  FScriptFunctionsByName[True] := TFunctionDefinitionsByName.Create(TIStringComparer.Ordinal);
  BuildScriptLists;
end;

destructor TInnoSetupStyler.Destroy;
begin
  FScriptFunctionsByName[False].Free;
  FScriptFunctionsByName[True].Free;
  FISPPFunctionsByName.Free;
  FFlagsWords.Free;
  FNoHighlightAtCursorWords.Free;
  inherited;
end;

procedure TInnoSetupStyler.AddWordToList(const SL: TStringList;
  const Word: AnsiString; const Typ: Integer);
begin
  if Typ >= 0 then
    SL.Add(Format('%s%s%d', [Word, InnoSetupStylerWordListTypeSeparator, Typ]))
  else
    SL.Add(String(Word));
end;

procedure TInnoSetupStyler.ApplyPendingSquigglyFromToIndex(const StartIndex, EndIndex: Integer);
begin
  if (CaretIndex >= StartIndex) and (CaretIndex <= EndIndex + 1) then
    ApplyStyleByteIndicators([inPendingSquiggly], StartIndex, EndIndex)
  else
    ApplyStyleByteIndicators([inSquiggly], StartIndex, EndIndex);
end;

procedure TInnoSetupStyler.ApplyPendingSquigglyFromIndex(const StartIndex: Integer);
begin
  ApplyPendingSquigglyFromToIndex(StartIndex, CurIndex - 1);
end;

procedure TInnoSetupStyler.ApplySquigglyFromIndex(const StartIndex: Integer);
begin
  ApplyStyleByteIndicators([inSquiggly], StartIndex, CurIndex - 1);
end;

function TInnoSetupStyler.BuildWordList(const Values: array of TScintRawString): AnsiString;
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

function TInnoSetupStyler.BuildWordList(const WordStringList: TStringList): AnsiString;
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

procedure TInnoSetupStyler.BuildSectionsWordList;
begin
  var SL := TStringList.Create;
  try
    for var Section in SectionMap do
      AddWordToList(SL, '[' + Section.Name + ']', awtSection);
    FSectionsWordList := BuildWordList(SL);
  finally
    SL.Free;
  end;
end;

procedure TInnoSetupStyler.BuildKeywordsWordList(
  const Section: TInnoSetupSection;
  const Parameters: array of TScintRawString);
begin
  const SL1 = FNoHighlightAtCursorWords[Section];
  const SL2 = TStringList.Create;
  try
    for var Parameter in Parameters do begin
      SL1.Add(String(Parameter));
      AddWordToList(SL2, Parameter, awtParameter);
    end;
    FKeywordsWordList[Section] := BuildWordList(SL2);
  finally
    SL2.Free;
  end;
end;

procedure TInnoSetupStyler.BuildKeywordsWordListFromTypeInfo(
  const Section: TInnoSetupSection; const EnumTypeInfo: Pointer;
  const PrefixLength: Integer);
begin
  const SL1 = FNoHighlightAtCursorWords[Section];
  const SL2 = TStringList.Create;
  try
    for var I := 0 to GetTypeData(EnumTypeInfo).MaxValue do begin
      const Parameter = Copy(GetEnumName(EnumTypeInfo, I), PrefixLength+1, MaxInt);
      SL1.Add(Parameter);
      AddWordToList(SL2, AnsiString(Parameter), awtDirective);
    end;
    FKeywordsWordList[Section] := BuildWordList(SL2);
  finally
    SL2.Free;
  end;
end;

procedure TInnoSetupStyler.BuildFlagsWordList(const Section: TInnoSetupSection;
  const Flags: array of TScintRawString);
begin
  const SL1 = FFlagsWords[Section];
  const SL2 = TStringList.Create;
  try
    for var Flag in Flags do begin
      SL1.Add(String(Flag));
      AddWordToList(SL2, Flag, awtFlagOrSetupDirectiveValue);
    end;
    FFlagsWordList[Section] := BuildWordList(SL2);
  finally
    SL2.Free;
  end;
end;

procedure TInnoSetupStyler.BuildScriptFunctionsLists(
  const ScriptFuncTable: TScriptTable; const ClassMembers: Boolean;
  const SL: TStringList);
begin
  for var ScriptFunc in ScriptFuncTable do begin
    const FunctionDefinition = TFunctionDefinition.Create(ScriptFunc);
    const ScriptFuncName = ExtractScriptFuncWithoutHeaderName(FunctionDefinition.ScriptFuncWithoutHeader);
    var DoAddWordToList := True;
    const Key = String(ScriptFuncName);
    if not FScriptFunctionsByName[ClassMembers].TryAdd(Key, [FunctionDefinition]) then begin
      { Function has multiple prototypes }
      var ScriptFunctions := FScriptFunctionsByName[ClassMembers][Key];
      const N = Length(ScriptFunctions);
      SetLength(ScriptFunctions, N+1);
      ScriptFunctions[N] := FunctionDefinition;
      FScriptFunctionsByName[ClassMembers][Key] := ScriptFunctions;
      DoAddWordToList := False; { Already added it when the first prototype was found }
    end;
    if DoAddWordToList then
      AddWordToList(SL, ScriptFuncName, awtScriptFunction);
  end;
end;

procedure TInnoSetupStyler.BuildISPPDirectivesWordList;
begin
  var SL := TStringList.Create;
  try
    for var ISPPDirective in ISPPDirectives do
      AddWordToList(SL, '#' + ISPPDirective.Name, awtPreprocessorDirective);
    FISPPDirectivesWordList := BuildWordList(SL);
  finally
    SL.Free;
  end;
end;

procedure TInnoSetupStyler.BuildISPPPragmaWordList;
begin
  var SL := TStringList.Create;
  try
    for var ISPPPragmaSubDirective in ISPPPragmaSubDirectives do
      AddWordToList(SL, ISPPPragmaSubDirective, awtPreprocessorSubDirective);
    FISPPPragmaWordList := BuildWordList(SL);
  finally
    SL.Free;
  end;
end;

procedure TInnoSetupStyler.BuildISPPExpressionWordList;
begin
  const SL = TStringList.Create;
  try
    for var ISPPFunction in ISPPFunctions do begin
      const FunctionDefinition = TFunctionDefinition.CreateISPP(ISPPFunction);
      const ISPPScriptFuncName = ExtractISPPScriptFuncWithoutHeaderName(FunctionDefinition.ScriptFuncWithoutHeader);
      const Key = String(ISPPScriptFuncName);
      if not FISPPFunctionsByName.TryAdd(Key, [FunctionDefinition]) then
        raise Exception.CreateFmt('Internal error: duplicate ISPP function "%s"', [ISPPScriptFuncName]);
      AddWordToList(SL, ISPPScriptFuncName, awtISPPFunction);
    end;
    for var ISPPPredefinedVariable in ISPPPredefinedVariables do
      AddWordToList(SL, ISPPPredefinedVariable, awtISPPVariable);
    for var ISPPConstant in ISPPConstants do
      AddWordToList(SL, ISPPConstant, awtISPPConstant);
    FISPPExpressionWordList := BuildWordList(SL);
  finally
    SL.Free;
  end;
end;

procedure TInnoSetupStyler.BuildConstantsWordList;
begin
  var SL := TStringList.Create;
  try
    for var Constant in Constants do
      if Constant = '{' then
        AddWordToList(SL, '{{', awtConstant)
      else
        AddWordToList(SL, '{' + Constant + '}', awtConstant);
    if ISPPInstalled then begin
      AddWordToList(SL, '{#', awtConstant);
      AddWordToList(SL, '{#file ', awtConstant);
      for var ISPPPredefinedVariable in ISPPPredefinedVariables do
        AddWordToList(SL, '{#' + ISPPPredefinedVariable + '}', awtConstant);
    end;
    for var ConstantWithParam in ConstantsWithParam do
      AddWordToList(SL, '{' + ConstantWithParam, awtConstant);
    FConstantsWordList := BuildWordList(SL);
  finally
    SL.Free;
  end;
end;

procedure TInnoSetupStyler.BuildEventFunctionsWordList;
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
        AddWordToList(SLFunctions, S, awtScriptEvent)
      else if HeaderKind = hkProcedure then
        AddWordToList(SLProcedures, S, awtScriptEvent)
      else
        raise Exception.Create('Internal error: got invalid HeaderKind for event function');
    end;
    FEventFunctionsWordList[False] := BuildWordList(SLFunctions);
    FEventFunctionsWordList[True] := BuildWordList(SLProcedures);
  finally
    SLProcedures.Free;
    SLFunctions.Free;
  end;
end;

procedure TInnoSetupStyler.CommitStyle(const Style: TInnoSetupStylerStyle);
begin
  inherited CommitStyle(TScintStyleNumber(Ord(Style)));
end;

procedure TInnoSetupStyler.CommitStyleSq(const Style: TInnoSetupStylerStyle;
  const Squigglify: Boolean);
begin
  if Squigglify then
    ApplySquigglyFromIndex(StyleStartIndex);
  CommitStyle(Style);
end;

procedure TInnoSetupStyler.CommitStyleSqPending(const Style: TInnoSetupStylerStyle);
begin
  ApplyPendingSquigglyFromIndex(StyleStartIndex);
  CommitStyle(Style);
end;

function TInnoSetupStyler.GetEventFunctionsWordList(Procedures: Boolean): AnsiString;
begin
  Result := FEventFunctionsWordList[Procedures];
end;

function TInnoSetupStyler.GetFlagsWordList(Section: TInnoSetupSection): AnsiString;
begin
  Result := FFlagsWordList[Section];
end;

procedure TInnoSetupStyler.GetFoldLevel(const LineState, PreviousLineState: TScintLineState;
      var Level: Integer; var Header, EnableHeaderOnPrevious: Boolean);
begin
  { Set folding per section. Lines outside of a section (=lines at the start of
    the document and section tags and section end tags and lines after section
    end tags) get level 0 with header flags for section tags. Other lines
    (=lines inside a section) get level 1. }

  var Section := TInnoSetupStyler.GetSectionFromLineState(LineState, False);
  if Section = scNone then begin
    Level := 0;
    Header := False; { Might be set to True via EnableHeaderOnPrevious below when we know about next line }
    EnableHeaderOnPrevious := False;
  end else begin
    Level := 1;
    Header := False;
    var PreviousSection := TInnoSetupStyler.GetSectionFromLineState(PreviousLineState, False);
    if Section = scCodeBlock then begin
      Inc(Level);
      EnableHeaderOnPrevious := PreviousSection = scCode;
    end else
      EnableHeaderOnPrevious := PreviousSection = scNone;
  end;
end;

function TInnoSetupStyler.GetKeywordsWordList(Section: TInnoSetupSection): AnsiString;
begin
  Result := FKeywordsWordList[Section];
end;

{ Result is undefined if out Count = 0 }
class function TInnoSetupStyler.GetFunctionDefinition(
  const FunctionsByName: TFunctionDefinitionsByName; const Name: String;
  const Index: Integer; out Count: Integer): TFunctionDefinition;
begin
  var FunctionDefinitions: TFunctionDefinitions;
  if FunctionsByName.TryGetValue(Name, FunctionDefinitions) then begin
    Count := Integer(Length(FunctionDefinitions));
    var ResultIndex := Index;
    if ResultIndex >= Count then
      ResultIndex := Count-1;
    Result := FunctionDefinitions[ResultIndex]
  end else
    Count := 0;
end;

function TInnoSetupStyler.GetISPPFunctionDefinition(const Name: String;
  const Index: Integer; out Count: Integer): TFunctionDefinition;
begin
  Result := GetFunctionDefinition(FISPPFunctionsByName, Name, Index, Count);
end;

function TInnoSetupStyler.GetScriptFunctionDefinition(const ClassMember: Boolean;
  const Name: String; const Index: Integer; out Count: Integer): TFunctionDefinition;
begin
  Result := GetFunctionDefinition(FScriptFunctionsByName[ClassMember], Name, Index, Count);
end;

function TInnoSetupStyler.GetScriptFunctionDefinition(
  const ClassMember: Boolean; const Name: String;
  const Index: Integer): TFunctionDefinition;
begin
  var Count: Integer;
  Result := GetScriptFunctionDefinition(ClassMember, Name, Index, Count);
end;

function TInnoSetupStyler.GetScriptWordList(
  ClassOrRecordMembers: Boolean): AnsiString;
begin
  Result := FScriptWordList[ClassOrRecordMembers];
end;

class function TInnoSetupStyler.GetSectionFromLineState(
  const LineState: TScintLineState; const ReturnCodeBlockAsCode: Boolean = True): TInnoSetupSection;
begin
  Result := TInnoSetupStylerLineState(LineState).Section;
  if ReturnCodeBlockAsCode and (Result = scCodeBlock) then
    Result := scCode;
end;

function TInnoSetupStyler.GetSetupSectionDirectiveValueIsMultiValue(
  SetupSectionDirective: TSetupSectionDirective): Boolean;
{ "MultiValue" means a directive like WizardStyle which accepts a space separated list of values }
begin
  Result := SetupSectionDirective in [ssArchitecturesAllowed,
    ssArchitecturesInstallIn64BitMode, ssDisablePrecompiledFileVerifications,
    ssPrivilegesRequiredOverridesAllowed, ssWizardStyle];
end;

function TInnoSetupStyler.GetSetupSectionDirectiveValueWordList(
  SetupSectionDirective: TSetupSectionDirective): AnsiString;
begin
  Result := FSetupSectionDirectiveValueWordList[SetupSectionDirective];
end;

procedure TInnoSetupStyler.GetStyleAttributes(const Style: Integer;
  var Attributes: TScintStyleAttributes);
begin
  if FTheme <> nil then begin
    if (Style >= 0) and (Style <= Ord(High(TInnoSetupStylerStyle))) then begin
      if not FTheme.Modern then begin
        { Check for some exceptions }
        case TInnoSetupStylerStyle(Style) of
          stCompilerDirective, stISPPReservedWord: begin Attributes.ForeColor := $4040C0; Exit; end;
          stMessageArg: begin Attributes.ForeColor := $FF8000; Exit; end;
          stPascalString, stPascalNumber, stISPPString, stISPPNumber: begin Attributes.ForeColor := clMaroon; Exit; end;
        end;
      end;
      case TInnoSetupStylerStyle(Style) of
        stCompilerDirective, stISPPReservedWord: Attributes.ForeColor := FTheme.Colors[tcRed];
        stComment: Attributes.ForeColor := FTheme.Colors[tcGreen];
        stSection: Attributes.FontStyle := [fsBold];
        stSymbol: Attributes.ForeColor := FTheme.Colors[tcGray];
        stKeyword, stPascalReservedWord: Attributes.ForeColor := FTheme.Colors[tcBlue];
        //stParameterValue: Attributes.ForeColor := FTheme.Colors[tcTeal];
        stEventFunction: Attributes.FontStyle := [fsBold];
        stConstant: Attributes.ForeColor := FTheme.Colors[tcPurple];
        stMessageArg: Attributes.ForeColor := FTheme.Colors[tcRed];
        stPascalString, stPascalNumber, stISPPString, stISPPNumber: Attributes.ForeColor := FTheme.Colors[tcOrange];
      end;
    end else begin
      case Style of
        STYLE_LINENUMBER: { Also sets the background colour for the margin with the markers like mmIconBreakpoint }
          begin
            Attributes.ForeColor := FTheme.Colors[tcMarginFore];
            Attributes.BackColor := FTheme.Colors[tcMarginBack];
          end;
        STYLE_BRACEBAD: Attributes.ForeColor := FTheme.Colors[tcRed];
        STYLE_BRACELIGHT: Attributes.BackColor := FTheme.Colors[tcBraceBack];
        STYLE_INDENTGUIDE: Attributes.ForeColor := FTheme.Colors[tcIndentGuideFore];
      end;
    end;
  end;
end;

procedure TInnoSetupStyler.HandleCodeSection(var SpanState: TInnoSetupStylerSpanState; var CodeBlockHeader: Boolean);

  function FinishConsumingBraceComment: Boolean;
  begin
    ConsumeCharsNot(['}']);
    Result := ConsumeChar('}');
    CommitStyle(stComment);
  end;

  function FinishConsumingStarComment: Boolean;
  begin
    Result := False;
    while True do begin
      ConsumeCharsNot(['*']);
      if not ConsumeChar('*') then
        Break;
      if ConsumeChar(')') then begin
        Result := True;
        Break;
      end;
    end;
    CommitStyle(stComment);
  end;

begin
  case SpanState of
    spBraceComment:
      if not FinishConsumingBraceComment then
        Exit;
    spStarComment:
      if not FinishConsumingStarComment then
        Exit;
  end;

  SpanState := spNone;
  SkipWhitespace;
  while not EndOfLine do begin
    if CurChar in PascalIdentFirstChars then begin
      var S := ConsumeString(PascalIdentChars);
      for var Word in PascalReservedWords do
        if SameRawText(S, Word) then begin
          if SameRawText(S, 'function') or SameRawText(S, 'procedure') or SameRawText(S, 'type') then
            CodeBlockHeader := True; { Global 'var' and 'const' blocks are currently not detected }
          CommitStyle(stPascalReservedWord);
          Break;
        end;
      for var EventFunction in BasicEventFunctions do
        if SameRawText(S, EventFunction) then begin
          CommitStyle(stEventFunction);
          Break;
        end;
      CommitStyle(stDefault);
    end else if ConsumeChars(DigitChars) then begin
      if not CurCharIs('.') or not NextCharIs('.') then begin
        if ConsumeChar('.') then
          ConsumeChars(DigitChars);
        var C := CurChar;
        if C in ['E', 'e'] then begin
          ConsumeChar(C);
          if not ConsumeChar('-') then
            ConsumeChar('+');
          if not ConsumeChars(DigitChars) then
            CommitStyleSqPending(stPascalNumber);
        end;
      end;
      CommitStyle(stPascalNumber);
    end else begin
      var C := CurChar;
      ConsumeChar(C);
      case C of
        ';', ':', '=', '+', '-', '*', '/', '<', '>', ',', '(', ')',
        '.', '[', ']', '@', '^':
          begin
            if (C = '/') and ConsumeChar('/') then begin
              ConsumeAllRemaining;
              CommitStyle(stComment);
            end else if (C = '(') and ConsumeChar('*') then begin
              if not FinishConsumingStarComment then begin
                SpanState := spStarComment;
                Exit;
              end;
            end else
              CommitStyle(stSymbol);
          end;
        '''':
          begin
            while True do begin
              ConsumeCharsNot([C]);
              if not ConsumeChar(C) then begin
                CommitStyleSqPending(stPascalString);
                Break;
              end;
              if not ConsumeChar(C) then begin
                CommitStyle(stPascalString);
                Break;
              end;
            end;
          end;
        '{':
          begin
            if not FinishConsumingBraceComment then begin
              SpanState := spBraceComment;
              Exit;
            end;
          end;
        '$':
          begin
            ConsumeChars(HexDigitChars);
            CommitStyle(stPascalNumber);
          end;
        '#':
          begin
            if ConsumeChar('$') then begin
              if not ConsumeChars(HexDigitChars) then
                 CommitStyleSqPending(stPascalString);
            end else if not ConsumeChars(DigitChars) then
              CommitStyleSqPending(stPascalString);
            CommitStyle(stPascalString);
          end;
      else
        { Illegal character }
        CommitStyleSq(stSymbol, True);
      end;
    end;
    SkipWhitespace;
  end;
end;

procedure TInnoSetupStyler.HandleCompilerDirective(const InlineDirective: Boolean; const InlineDirectiveEndIndex: Integer; var OpenCount: ShortInt);

  function EndOfDirective: Boolean;
  begin
    Result := EndOfLine or (InlineDirective and (CurIndex > InlineDirectiveEndIndex));
  end;

  procedure FinishDirectiveNameOrShorthand(const RequiresParameter: Boolean);
  begin
    if RequiresParameter then begin
      ConsumeChars(WhitespaceChars); { This will give the whitespace the stCompilerDirective style instead of stDefault but that's ok }
      if EndOfDirective then
        CommitStyleSqPending(stCompilerDirective)
      else
        CommitStyle(stCompilerDirective);
    end else
      CommitStyle(stCompilerDirective);
  end;

  function FinishConsumingStarComment: Boolean;
  begin
    Result := False;
    while True do begin
      ConsumeCharsNot(['*']);
      if not ConsumeChar('*') then
        Break;
      if ConsumeChar('/') then begin
        Result := True;
        Break;
      end;
    end;
    if Result then
      CommitStyle(stComment)
    else
      CommitStyleSqPending(stComment);
  end;

  procedure ConsumeISPPString(const Terminator: AnsiChar; const AllowEscapedTerminator: Boolean);
  begin
    while True do begin
      ConsumeCharsNot([Terminator]);
      if not ConsumeChar(Terminator) then begin
        { Non terminated string found }
        CommitStyleSqPending(stISPPString);
        Break;
      end;
      { Terminated string found and consumed. Now check if the terminator is actually escaped by doubling, if allowed }
      if not AllowEscapedTerminator or not ConsumeChar(Terminator) then begin
        { Doubling not allowed or no double terminator found, so we're done }
        CommitStyle(stISPPString);
        Break;
      end;
      { The terminator was doubled so we should continue to find the real terminator }
    end;

  end;

begin
  var StartIndex := CurIndex;
  var NeedIspp: Boolean;
  if InlineDirective then begin
    ConsumeChar('{');
    NeedIspp := True;
  end else
    NeedIspp := False; { Might be updated later to True later }
  var ForDirectiveExpressionsNext := False;
  var DoIncludeFileNotationCheck := False;
  var ErrorDirective := False;
  ConsumeChar('#');
  CommitStyle(stCompilerDirective);

  { Directive name or shorthand }
  SkipWhiteSpace;
  var C := CurChar;
  if ConsumeCharIn(ISPPDirectiveShorthands) then begin
    DoIncludeFileNotationCheck := C = '+'; { We need to check the include file notation  }
    NeedIspp := True;
    if C = '?' then begin { if }
      Inc(OpenCount);
      FinishDirectiveNameOrShorthand(True);
    end else if C = '.' then begin { endif }
      Inc(OpenCount, -1);
      if OpenCount < 0 then begin
        CommitStyleSq(stCompilerDirective, True);
        OpenCount := 0; { See below }
      end;
      FinishDirectiveNameOrShorthand(False);
    end else
      FinishDirectiveNameOrShorthand(C <> '^'); { All shorthands except ^ (else) require a parameter }
  end else begin
    var S := ConsumeString(ISPPIdentChars);
    for var ISPPDirective in ISPPDirectives do
      if SameRawText(S, ISPPDirective.Name) then begin
        if SameRawText(S, 'error') then
          ErrorDirective := True
        else if SameRawText(S, 'include') then
          DoIncludeFileNotationCheck := True { See above }
        else
          NeedIspp := True; { Built-in preprocessor only supports '#include' }
        ForDirectiveExpressionsNext := SameRawText(S, 'for'); { #for uses ';' as an expressions list separator so we need to remember that ';' doesn't start a comment until the list is done }
        Inc(OpenCount, ISPPDirective.OpenCountChange);
        if OpenCount < 0 then begin
          CommitStyleSq(stCompilerDirective, True);
          OpenCount := 0; { Reset so that next doesn't automatically gets error as well }
        end;
        FinishDirectiveNameOrShorthand(ISPPDirective.RequiresParameter);
        Break;
      end;
    if InlineDirective then
      CommitStyle(stDefault) { #emit shorthand was used (='#' directly followed by an expression): not an error }
    else
      CommitStyleSqPending(stCompilerDirective);
  end;

  { Rest of the directive }
  if ErrorDirective then begin
    SkipWhitespace;
    while not EndOfDirective do begin
      C := CurChar;
      ConsumeChar(C);
      if InlineDirective and (C = '}') then
        CommitStyle(stCompilerDirective)
      else
        CommitStyle(stISPPString);
    end;
  end else begin
    SkipWhitespace;
    while not EndOfDirective do begin
      if DoIncludeFileNotationCheck then begin
        if CurChar <> '"' then begin
          NeedIspp := True; { Built-in preprocessor requires a '"' quoted string after the '#include' and doesn't support anything else }
          if CurChar = '<' then { Check for ISPP's special bracket notation for include files }
            ConsumeISPPString('>', False); { Consume now instead of using regular consumption }
        end;
        DoIncludeFileNotationCheck := False;
      end;
      if CurChar in ISPPIdentFirstChars then begin
        var S := ConsumeString(ISPPIdentChars);
        for var ISPPReservedWord in ISPPReservedWords do
          if SameRawText(S, ISPPReservedWord) then begin
            CommitStyle(stISPPReservedWord);
            Break;
          end;
        CommitStyle(stDefault)
      end else if ConsumeChars(DigitChars) then begin
        if not CurCharIs('.') or not NextCharIs('.') then begin
          if ConsumeChar('.') then
            ConsumeChars(DigitChars);
          C := CurChar;
          if C in ['X', 'x'] then begin
            ConsumeChar(C);
            if not ConsumeChars(HexDigitChars) then
              CommitStyleSqPending(stISPPNumber);
          end;
          ConsumeChars(['L', 'U', 'l', 'u']);
        end;
        CommitStyle(stISPPNumber);
      end else begin
        C := CurChar;
        ConsumeChar(C);
        case C of
          '!', '&', '=', '|', '^', '>', '<', '+', '-', '/', '%', '*',
          '?', ':', ',', '.', '~', '(', '[', '{', ')', ']', '}', '@',
          '#':
            begin
              if (C = '}') and ForDirectiveExpressionsNext then
                ForDirectiveExpressionsNext := False;
              if (C = '/') and ConsumeChar('*') then
                FinishConsumingStarComment
              else if InlineDirective and (C = '}') then
                CommitStyle(stCompilerDirective) (* Closing '}' of the ISPP inline directive *)
              else
                CommitStyle(stSymbol);
            end;
          ';':
            begin
              if ForDirectiveExpressionsNext then
                CommitStyle(stSymbol)
              else begin
                if not InlineDirective then
                  ConsumeAllRemaining
                else
                  ConsumeCharsNot(['}']);
                CommitStyle(stComment);
              end;
            end;
          '''', '"':
            ConsumeISPPString(C, True);
        else
          { Illegal character }
          CommitStyleSq(stSymbol, True);
        end;
      end;
      SkipWhitespace;
    end;
  end;

  if NeedIspp and not ISPPInstalled then begin
    if InlineDirective then
      ApplyPendingSquigglyFromToIndex(StartIndex + 1, InlineDirectiveEndIndex - 1)
    else
      ApplyPendingSquigglyFromIndex(StartIndex + 1);
  end;
end;

procedure TInnoSetupStyler.HandleParameterSection(
  const ValidParameters: array of TScintRawString);
const
  MaxParameters = 32;
var
  ParamsSpecified: set of 0..MaxParameters-1;
  S: TScintRawString;
  ParamValueIndex, BraceLevel: Integer;
  NamePresent, ValidName, DuplicateName, ColonPresent: Boolean;
begin
  if Length(ValidParameters) > MaxParameters then
    raise Exception.Create('Internal error: too many valid parameters');

  ParamsSpecified := [];
  while not EndOfLine do begin
    { Squigglify any bogus characters before the parameter name }
    SquigglifyUntilChars(AlphaChars + [':'], stDefault);

    { Parameter name }
    S := ConsumeString(AlphaDigitChars);
    NamePresent := (S <> '');
    ValidName := False;
    DuplicateName := False;
    for var I := Low(ValidParameters) to High(ValidParameters) do
      if SameRawText(S, ValidParameters[I]) then begin
        ValidName := True;
        DuplicateName := (I in ParamsSpecified);
        Include(ParamsSpecified, I);
        Break;
      end;
    if DuplicateName then
      CommitStyleSqPending(stKeyword)
    else if ValidName then
      CommitStyle(stKeyword)
    else
      CommitStyleSqPending(stDefault);
    SkipWhitespace;

    { If there's a semicolon with no colon, squigglify the semicolon }
    if ConsumeChar(';') then begin
      CommitStyleSq(stSymbol, True);
      SkipWhitespace;
      Continue;
    end;

    { Colon }
    ColonPresent := ConsumeChar(':');
    CommitStyleSq(stSymbol, not NamePresent);
    SkipWhitespace;

    { Parameter value. This consumes until a ';' is found or EOL is reached. }
    ParamValueIndex := CurIndex;
    BraceLevel := 0;
    if ConsumeChar('"') then begin
      while True do begin
        StyleConstsUntilChars(['"'], stParameterValue, BraceLevel);
        { If no closing quote exists, squigglify the whole value and break }
        if not ConsumeChar('"') then begin
          ApplyPendingSquigglyFromIndex(ParamValueIndex);
          Break;
        end;
        { Quote found, now break, unless there are two quotes in a row }
        if not ConsumeChar('"') then
          Break;
      end;
    end else begin
      while True do begin
        StyleConstsUntilChars([';', '"'], stParameterValue, BraceLevel);
        { Squigglify any quote characters inside an unquoted string }
        if ConsumeChar('"') then
          ApplySquigglyFromIndex(CurIndex - 1)
        else
          Break;
      end;
    end;
    CommitStyle(stParameterValue);
    if not ColonPresent then
      ApplySquigglyFromIndex(ParamValueIndex);
    { Squigglify any characters between a quoted string and the next ';' }
    SquigglifyUntilChars([';'], stDefault);

    { Semicolon }
    ConsumeChar(';');
    CommitStyle(stSymbol);
    SkipWhitespace;
  end;
end;

procedure TInnoSetupStyler.HandleKeyValueSection(const Section: TInnoSetupSection);

  procedure StyleMessageArgs;
  begin
    while True do begin
      ConsumeCharsNot(['%']);
      CommitStyle(stDefault);
      if not ConsumeChar('%') then
        Break;
      if CurCharIn(['1'..'9', '%', 'n']) then begin
        ConsumeChar(CurChar);
        CommitStyle(stMessageArg);
      end;
    end;
  end;

var
  S: String;
  I, BraceLevel: Integer;
begin
  { Squigglify any bogus characters at the start of the line }
  SquigglifyUntilChars(AlphaUnderscoreChars, stDefault);
  if EndOfLine then
    Exit;

  S := String(ConsumeString(AlphaDigitUnderscoreChars));
  { Was that a language name? }
  if (Section in [scCustomMessages, scLangOptions, scMessages]) and
     CurCharIs('.') then begin
    CommitStyle(stDefault);
    ConsumeChar('.');
    CommitStyle(stSymbol);
    { Squigglify any spaces or bogus characters between the '.' and key name }
    if ConsumeCharsNot(AlphaUnderscoreChars) then
      CommitStyleSq(stDefault, True);
    S := String(ConsumeString(AlphaDigitUnderscoreChars));
  end;

  case Section of
    scCustomMessages:
      I := 0;
    scLangOptions:
      I := GetEnumValue(TypeInfo(TLangOptionsSectionDirective), 'ls' + S);
    scMessages:
      I := GetEnumValue(TypeInfo(TSetupMessageID), 'msg' + S);
    scSetup:
      I := GetEnumValue(TypeInfo(TSetupSectionDirective), 'ss' + S);
  else
    I := -1;
  end;
  if I <> -1 then
    CommitStyle(stKeyword)
  else begin
    if Section in [scLangOptions, scMessages, scSetup] then
      CommitStyleSqPending(stDefault)
    else
      CommitStyle(stDefault);
  end;
  SquigglifyUntilChars(['='], stDefault);

  ConsumeChar('=');
  CommitStyle(stSymbol);
  SkipWhitespace;

  if Section in [scCustomMessages, scMessages] then
    StyleMessageArgs
  else begin
    BraceLevel := 0;
    StyleConstsUntilChars([], stDefault, BraceLevel);
    CommitStyle(stDefault);
  end;
end;

class function TInnoSetupStyler.IsCommentOrKeywordStyle(const Style: TScintStyleNumber): Boolean;
begin
  Result := Style in [Ord(stComment), Ord(stKeyword)];
end;

class function TInnoSetupStyler.IsCommentOrISPPStringStyle(const Style: TScintStyleNumber): Boolean;
begin
  Result := Style in [Ord(stComment), Ord(stISPPString)];
end;

class function TInnoSetupStyler.IsCommentOrPascalStringStyle(const Style: TScintStyleNumber): Boolean;
begin
  Result := Style in [Ord(stComment), Ord(stPascalString)];
end;

class function TInnoSetupStyler.IsISPPIdentChar(const C: AnsiChar): Boolean;
begin
  Result := C in ISPPIdentChars;
end;

class function TInnoSetupStyler.IsParamSection(
  const Section: TInnoSetupSection): Boolean;
begin
  Result := not (Section in [scCustomMessages, scLangOptions, scMessages, scSetup, scCode, scCodeBlock]);
end;

class function TInnoSetupStyler.IsSymbolStyle(const Style: TScintStyleNumber): Boolean;
begin
  Result := Style = Ord(stSymbol);
end;

class function TInnoSetupStyler.LineSectionHeader(const LineState: TScintLineState;
  out Section: TInnoSetupSection): Boolean;
{ Returns True if the line opens a section for the lines after it, also
  returning that section (scNone if it does not). A line starting a section
  has NextLineSection <> scNone. Exception: a code-block begin line inside
  [Code] has NextLineSection = scCodeBlock without being a section header }
begin
  Section := TInnoSetupStylerLineState(LineState).NextLineSection;
  Result := not (Section in [scNone, scCodeBlock]);
  if not Result then
    Section := scNone;
end;

class function TInnoSetupStyler.LineSpans(const S: TScintRawString): Boolean;
var
  I: Integer;
begin
  { Note: To match ISPP behavior, require length of at least 3 }
  I := Length(S);
  Result := (I > 2) and (S[I] = '\') and (S[I-1] in WhitespaceChars);
end;

{ Having a LineTextSpans is required by TScintCustomStyler }
function TInnoSetupStyler.LineTextSpans(const S: TScintRawString): Boolean;
begin
  Result := LineSpans(S);
end;

procedure TInnoSetupStyler.PreStyleInlineISPPDirectives;

  function IsLineCommented: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to TextLength do begin
      { In ISPP, only ';' and '//' inhibit processing of inline directives }
      if (Text[I] = ';') or
         ((I < TextLength) and (Text[I] = '/') and (Text[I+1] = '/')) then begin
        Result := True;
        Break;
      end;
      if not(Text[I] in WhitespaceChars) then
        Break;
    end;
  end;

const
  LineEndChars = [#10, #13];
var
  I, StartIndex: Integer;
  Valid: Boolean;
begin
  { Style span symbols, then replace them with spaces to prevent any further
    processing }
  for I := 3 to TextLength do begin
    if ((I = TextLength) or (Text[I+1] in LineEndChars)) and
       (Text[I] = '\') and (Text[I-1] in WhitespaceChars) and
       not(Text[I-2] in LineEndChars) then begin
      ReplaceText(I, I, ' ');
      ApplyStyle(Ord(stSymbol), I, I);
      if not ISPPInstalled then
        ApplyStyleByteIndicators([inSquiggly], I, I);
    end;
  end;

  { Style all '{#' ISPP inline directives before anything else }
  if not IsLineCommented then begin
    I := 1;
    while I < TextLength do begin
      if (Text[I] = '{') and (Text[I+1] = '#') then begin
        StartIndex := I;
        Valid := False;
        while I <= TextLength do begin
          Inc(I);
          if Text[I-1] = '}' then begin
            Valid := True;
            Break;
          end;
        end;
        ResetCurIndexTo(StartIndex);
        try
          var OpenCount: ShortInt := 0;
          HandleCompilerDirective(True, I - 1, OpenCount);
        finally
          ResetCurIndexTo(0);
        end;
        if not Valid then
          ApplyPendingSquigglyFromToIndex(StartIndex, I - 1);
        { Replace the directive with spaces to prevent any further processing }
        ReplaceText(StartIndex, I - 1, ' ');
      end else
        Inc(I);
    end;
  end;
end;

function TInnoSetupStyler.SectionHasFlag(const Section: TInnoSetupSection;
  const Flag: String): Boolean;
begin
  Result := FFlagsWords[Section].IndexOf(Flag) <> -1;
end;

function TInnoSetupStyler.HighlightAtCursorAllowed(const Section: TInnoSetupSection;
  const Word: string): Boolean;
begin
  Result := FNoHighlightAtCursorWords[Section].IndexOf(Word) = -1;
end;

procedure TInnoSetupStyler.SetISPPInstalled(const Value: Boolean);
begin
  if Value <> FISPPInstalled then begin
    FISPPInstalled := Value;
    BuildConstantsWordList;
  end;
end;

procedure TInnoSetupStyler.SkipWhitespace;
begin
  ConsumeChars(WhitespaceChars);
  CommitStyle(stDefault);
end;

procedure TInnoSetupStyler.SquigglifyUntilChars(const Chars: TScintRawCharSet;
  const Style: TInnoSetupStylerStyle);
var
  IsWhitespace: Boolean;
begin
  { Consume and squigglify all non-whitespace characters until one of Chars
    is encountered }
  while not EndOfLine and not CurCharIn(Chars) do begin
    IsWhitespace := CurCharIn(WhitespaceChars);
    ConsumeChar(CurChar);
    if IsWhitespace then
      CommitStyle(stDefault)
    else
      CommitStyleSq(Style, True);
  end;
  CommitStyle(stDefault);
end;

procedure TInnoSetupStyler.StyleConstsUntilChars(const Chars: TScintRawCharSet;
  const NonConstStyle: TInnoSetupStylerStyle; var BraceLevel: Integer);
var
  C: AnsiChar;
begin
  while not EndOfLine and not CurCharIn(Chars) do begin
    if BraceLevel = 0 then
      CommitStyle(NonConstStyle);
    C := CurChar;
    ConsumeChar(C);
    if C = '{' then begin
      if not ConsumeChar('{') then
        Inc(BraceLevel);
    end;
    if (C = '}') and (BraceLevel > 0) then begin
      Dec(BraceLevel);
      if BraceLevel = 0 then
        CommitStyle(stConstant);
    end;
  end;
end;

procedure TInnoSetupStyler.StyleNeeded;

  function MapSectionNameString(const S: TScintRawString): TInnoSetupSection;
  begin
    if (S <> '') and (S[1] = '_') then
      Result := scThirdParty
    else begin
      Result := scUnknown;
      for var Section in SectionMap do
        if SameRawText(S, Section.Name) then begin
          Result := Section.Section;
          Break;
        end;
    end;
  end;

  function CheckSectionEnd(const NewSection, Section: TInnoSetupSection): Boolean;
  begin
    Result := (NewSection = Section) or ((NewSection = scCode) and (Section = scCodeBlock));
  end;

begin
  var NewLineState := TInnoSetupStylerLineState(LineState);
  if NewLineState.NextLineSection <> scNone then begin
    { Previous line started a section }
    NewLineState.Section := NewLineState.NextLineSection;
    NewLineState.NextLineSection := scNone;
  end;
  var Section := NewLineState.Section;

  PreStyleInlineISPPDirectives;

  const IsCodeSection = Section in [scCode, scCodeBlock];

  SkipWhitespace;
  if not IsCodeSection and ConsumeChar(';') then begin
    ConsumeAllRemaining;
    CommitStyle(stComment);
  end else if CurCharIs('/') and NextCharIs('/') then begin
    ConsumeAllRemaining;
    CommitStyleSq(stComment, not ISPPInstalled and not IsCodeSection)
  end else if ConsumeChar('[') then begin
    const SectionEnd = ConsumeChar('/');
    const S = ConsumeString(AlphaUnderscoreChars);
    if ConsumeChar(']') then begin
      const NewSection = MapSectionNameString(S);
      { Unknown section names and erroneously-placed end tags get squigglified }
      CommitStyleSq(stSection, (NewSection = scUnknown) or
        (SectionEnd and not CheckSectionEnd(NewSection, Section)));
      if not SectionEnd then
        NewLineState.NextLineSection := NewSection;
    end else
      CommitStyleSqPending(stDefault);
    { Section tags themselves are not associated with any section }
    Section := scNone;
    SquigglifyUntilChars([], stDefault);
  end else if CurCharIs('#') then
    HandleCompilerDirective(False, -1, NewLineState.OpenCompilerDirectivesCount)
  else if IsCodeSection then begin
    var CodeBlockHeader := False;
    HandleCodeSection(NewLineState.SpanState, CodeBlockHeader);
    if CodeBlockHeader then begin
      Section := scCode;
      NewLineState.NextLineSection := scCodeBlock;
    end;
  end else begin
    case Section of
      scUnknown: ;
      scThirdParty: ;
      scComponents: HandleParameterSection(FSectionParameters[Section]);
      scCustomMessages: HandleKeyValueSection(Section);
      scDirs: HandleParameterSection(FSectionParameters[Section]);
      scISSigKeys: HandleParameterSection(FSectionParameters[Section]);
      scFiles: HandleParameterSection(FSectionParameters[Section]);
      scIcons: HandleParameterSection(FSectionParameters[Section]);
      scINI: HandleParameterSection(FSectionParameters[Section]);
      scInstallDelete: HandleParameterSection(FSectionParameters[Section]);
      scLangOptions: HandleKeyValueSection(Section);
      scLanguages: HandleParameterSection(FSectionParameters[Section]);
      scMessages: HandleKeyValueSection(Section);
      scRegistry: HandleParameterSection(FSectionParameters[Section]);
      scRun: HandleParameterSection(FSectionParameters[Section]);
      scSetup: HandleKeyValueSection(Section);
      scTasks: HandleParameterSection(FSectionParameters[Section]);
      scTypes: HandleParameterSection(FSectionParameters[Section]);
      scUninstallDelete: HandleParameterSection(FSectionParameters[Section]);
      scUninstallRun: HandleParameterSection(FSectionParameters[Section]);
    end;
  end;

  NewLineState.Section := Section;
  LineState := TScintLineState(NewLineState);
end;

function SMI(const Section: TInnoSetupSection): TSectionMapItem;
begin
  Result.Name := TScintRawString(SectionToSectionName(Section));
  Result.Section := Section;
end;

initialization
  for var Section := Low(TInnoSetupSection) to High(TInnoSetupSection) do
    if not (Section in [scNone, scUnknown, scThirdParty, scCodeBlock]) then
      SectionMap := SectionMap + [SMI(Section)];

end.
