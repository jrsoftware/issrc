unit IDE.MainForm.AutoCompleteAndCallTipsHelper;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler form - Auto complete & call tips helper which has the tools helper as ancestor

  Not used by MainForm: it uses IDE.MainForm.FinalHelper instead
}

interface

uses
  Menus,
  ScintEdit,
  IDE.MainForm, IDE.MainForm.ToolsHelper,
  IDE.ScriptModel.Metadata.Extra.FunctionDefinitions;

type
  TMainFormAutoCompleteAndCallTipsHelper = class helper(TMainFormToolsHelper) for TMainForm
    procedure InitiateAutoComplete(const AMemo: TScintEdit; const Key: AnsiChar);
    procedure AutoCompleteAndCallTipsHandleCharAdded(const AMemo: TScintEdit; const Ch: AnsiChar);
    function BuildUserDefinedFunctionDefinitions(const AMemo: TScintEdit;
      const ALine: Integer; const AClassMember: Boolean): TFunctionDefinitionsWithName;
    procedure CallTipsHandleArrowClick(const AMemo: TScintEdit; const Up: Boolean);
    procedure CallTipsHandleCtrlSpace(const AMemo: TScintEdit);
    procedure CallTipsHandleUpdateUI(const AMemo: TScintEdit);
    class function IsInISPPLineContext(const AMemo: TScintEdit;
      const LinePos, ScanEndPos: Integer;
      out IsPragmaContext: Boolean): Boolean; static;
    { Private }
    function _TryAcquireAndHoldCodeSectionAtLine(const AMemo: TScintEdit;
      const ALine: Integer): Boolean;
    class function _InitiateAutoCompleteOrCallTipAllowedAtPos(const AMemo: TScintEdit;
      const WordStartLinePos, PositionBeforeWordStartPos: Integer;
      const ISPPExpressionContext: Boolean): Boolean; static;
    procedure _UpdateCallTipFunctionDefinition(const AMemo: TScintEdit; const Pos: Integer = -1);
    procedure _InitiateCallTip(const AMemo: TScintEdit; const Key: AnsiChar);
    procedure _CountCallTipBracesAndCommas(const AMemo: TScintEdit; out Braces, Commas: Integer);
    procedure _ContinueCallTip(const AMemo: TScintEdit);
  end;

implementation

uses
  SysUtils, Classes, Math, TypInfo,
  Shared.ScriptFunc, Shared.SetupSectionDirectives,
  IDE.LiveScriptObjectFactory, IDE.ScintStylerInnoSetup, IDE.ScriptModel,
  IDE.ScriptModel.Metadata, IDE.ScriptModel.Metadata.Extra,
  IDE.ScriptModel.Metadata.Extra.WordLists;

const
  AutoCompleteWordChars = AlphaDigitUnderscoreChars;
  AutoCompleteStartOrContinueChars = AutoCompleteWordChars + ['#', '{', '[', '<'];

function CreateSortedCaseInsensitiveStringList: TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  Result.UseLocale := False; { Make sure it uses CompareText and not AnsiCompareText }
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
end;

function TMainFormAutoCompleteAndCallTipsHelper._TryAcquireAndHoldCodeSectionAtLine(
  const AMemo: TScintEdit; const ALine: Integer): Boolean;
begin
  Result := False;
  const Factory = LiveScriptObjectFactoryForMemo(AMemo);
  var SectionIndex: Integer;
  if not Factory.TryGetSectionAtLine(ALine, SectionIndex) then
    Exit;
  var CodeSection: TLiveScriptCodeSection;
  if not Factory.TryAcquireCodeSection(SectionIndex, CodeSection) then
    Exit;

  { The acquired section is held between autocompletions, call tips, and hover
    hints so it can still be reused later, like after Navigator's debounce or
    by a second autocompletion or hover hint without an edit in between }
  var PreviousCodeSection := FAutoCompleteAndCallTipsLiveCodeSection;
  FAutoCompleteAndCallTipsLiveCodeSection := CodeSection;
  TLiveScriptObjectFactory.ReleaseAndNil(PreviousCodeSection); { Frees or (if just reacquired above) decrements acquire count again }
  Result := True;
end;

class function TMainFormAutoCompleteAndCallTipsHelper._InitiateAutoCompleteOrCallTipAllowedAtPos(const AMemo: TScintEdit;
  const WordStartLinePos, PositionBeforeWordStartPos: Integer;
  const ISPPExpressionContext: Boolean): Boolean;
begin
  if PositionBeforeWordStartPos < WordStartLinePos then
    Exit(True);
  const Style = AMemo.GetStyleAtPosition(PositionBeforeWordStartPos);
  if ISPPExpressionContext then
    Result := not TInnoSetupStyler.IsCommentOrISPPStringStyle(Style)
  else
    Result := not TInnoSetupStyler.IsCommentOrPascalStringStyle(Style);
end;

class function TMainFormAutoCompleteAndCallTipsHelper.IsInISPPLineContext(
  const AMemo: TScintEdit; const LinePos, ScanEndPos: Integer;
  out IsPragmaContext: Boolean): Boolean;

  function SkipChars(const Pos: Integer; const Chars: TSysCharSet): Integer;
  begin
    Result := Pos;
    while (Result < ScanEndPos) and (AMemo.GetByteAtPosition(Result) in Chars) do
      Result := AMemo.GetPositionAfter(Result);
  end;

begin
  { Allow autocompletion if the text before ScanEndPos on the line is an
    ISPP directive context because it starts with for example "#define X ",
    "#define X=", "#:X ", "#:X=", "#emit ", "#=", "#dim X[" or "#pragma ".
    IsPragmaContext is set to True for "#pragma ". }
  Result := False;
  IsPragmaContext := False;

  if LinePos >= ScanEndPos then
    Exit;

  var Pos := LinePos;

  { Skip leading whitespace }
  Pos := SkipChars(Pos, WhitespaceChars);

  { Require '#' as first non-whitespace character }
  if (Pos >= ScanEndPos) or (AMemo.GetByteAtPosition(Pos) <> '#') then
    Exit;
  Pos := AMemo.GetPositionAfter(Pos);
  if Pos >= ScanEndPos then
    Exit;

  var Directive: String;
  var ExpectIdent: Boolean;

  { Read the directive shorthand or name, and remember if an identifier is to be expected }
  case AMemo.GetByteAtPosition(Pos) of
    ':':
      begin
        Directive := 'define';
        ExpectIdent := True;
        Pos := AMemo.GetPositionAfter(Pos);
      end;
    '=' { emit }, '!' { expr }, '?' { if }:
      Exit(True); { #=, #! and #? begin the expression immediately and do not require any whitespace }
  else
    begin
      const DirectiveEndPos = AMemo.GetWordEndPosition(Pos, True);
      Directive := AMemo.GetTextRange(Pos, DirectiveEndPos);
      Pos := DirectiveEndPos;

      { Require at least one whitespace character after the directive name }
      if (Pos >= ScanEndPos) or (AMemo.GetByteAtPosition(Pos) > ' ') then
        Exit;

      { Check for #pragma }
      if SameText(Directive, 'pragma') then begin
        { #pragma does not support expressions, but only sub-directives like
          "message", so should check we aren't beyond that already }
        { Skip whitespace after "pragma" }
        Pos := SkipChars(Pos, WhitespaceChars);
        { Skip the sub-directive word if any }
        Pos := SkipChars(Pos, ISPPIdentChars);
        IsPragmaContext := Pos = ScanEndPos;
        Exit(IsPragmaContext);
      end;
      
      { Check for expression-supporting directives }
      ExpectIdent := SameText(Directive, 'define') or SameText(Directive, 'dim') or SameText(Directive, 'redim');
      if not ExpectIdent and not SameText(Directive, 'if') and not SameText(Directive, 'elif') and
         not SameText(Directive, 'emit') and not SameText(Directive, 'echo') and
         not SameText(Directive, 'expr') and not SameText(Directive, 'call') and
         not SameText(Directive, 'insert') then
        Exit;
    end;
  end;

  { Most directives do not expect an identifier, and whitespace after the
    directive name is sufficient }
  if not ExpectIdent then
    Exit(True); { Return True }

  { Skip whitespace }
  Pos := SkipChars(Pos, WhitespaceChars);
  if Pos >= ScanEndPos then
    Exit;

  { Skip the identifier (not using GetWordEndPosition because '[' is a word char) }
  Pos := SkipChars(Pos, ISPPIdentChars);
  if Pos >= ScanEndPos then
    Exit;

  { For define: skip optional parameter list }
  if SameText(Directive, 'define') and (AMemo.GetByteAtPosition(Pos) = '(') then begin
    Pos := AMemo.GetPositionAfter(Pos);
    var Braces := 1;
    while (Pos < ScanEndPos) and (Braces > 0) do begin
      const C = AMemo.GetByteAtPosition(Pos);
      if C = '(' then
        Inc(Braces)
      else if C = ')' then
        Dec(Braces);
      Pos := AMemo.GetPositionAfter(Pos);
    end;
    if Braces > 0 then
      Exit;
  end;

  { Determine the alternative (non-whitespace) separator:
    '=' for #define (like "#define X=expr") and '[' for #dim/#redim (like "#dim X[expr]"). }
  var AlternativeSepChar: AnsiChar := '[';
  if SameText(Directive, 'define') then
    AlternativeSepChar := '=';

  { Require at least one whitespace character or the separator after the identifier or param list }
  if (Pos >= ScanEndPos) or ((AMemo.GetByteAtPosition(Pos) > ' ') and (AMemo.GetByteAtPosition(Pos) <> AlternativeSepChar)) then
    Exit;
  Result := True;
end;

procedure TMainFormAutoCompleteAndCallTipsHelper.InitiateAutoComplete(const AMemo: TScintEdit; const Key: AnsiChar);

  function OnlyWhiteSpaceBeforeWord(const LinePos, WordStartPos: Integer): Boolean;
  begin
    { Only allow autocompletion if no non-whitespace characters exist before the current word on the line }
    var I := WordStartPos;
    Result := False;
    while I > LinePos do begin
      I := AMemo.GetPositionBefore(I);
      if I < LinePos then
        Exit;  { shouldn't get here }
      const C = AMemo.GetByteAtPosition(I);
      if C > ' ' then
        Exit;
    end;
    Result := True;
  end;

  function SkipLanguagePrefixBeforeWord(const LinePos, WordStartPos: Integer): Integer;
  begin
    { Also see TScriptModelSectionMetadata.TryGetMember }
    Result := WordStartPos;
    if WordStartPos <= LinePos then
      Exit;
    const DotPos = AMemo.GetPositionBefore(WordStartPos);
    if AMemo.GetByteAtPosition(DotPos) <> '.' then
      Exit;
    { Like TryGetMember's P > 1: needs at least one character before the '.' }
    const LanguageStartPos = AMemo.GetWordStartPosition(DotPos, True);
    if LanguageStartPos < DotPos then
      Result := LanguageStartPos;
  end;

  function ExtendCharsBefore(const LinePos, CaretPos, CharsBefore: Integer): Integer;
  begin
    { CharsBefore counts to the word start before the caret, without accounting
      for FAutoCompleteExtraContinueChars. Extend it now, towards the start
      of the line. This for example extends from 'modify' to 'users-modify'. }
    var ValueStartPos := CaretPos - CharsBefore;
    while ValueStartPos > LinePos do begin
      const PosBefore = AMemo.GetPositionBefore(ValueStartPos);
      const C = AMemo.GetByteAtPosition(PosBefore);
      if not (C in FAutoCompleteExtraContinueChars) and
         not (C in AutoCompleteWordChars) then
        Break;
      ValueStartPos := PosBefore;
    end;
    Result := CaretPos - ValueStartPos;
  end;

  function CanAutoStartAtWord(const CharsBefore: Integer; const CaretInsideWord: Boolean): Boolean;
  begin
    { Don't auto start autocompletion after a character is typed if there are any
      word characters adjacent to the character }
    Result := (CharsBefore <= 1) and not CaretInsideWord;
  end;

  function StyleAllowsAutoStart(const LinePos, WordStartPos: Integer;
    const ISPPExpressionContext: Boolean): Boolean;
  begin
    const PositionBeforeWordStartPos = AMemo.GetPositionBefore(WordStartPos);
    AMemo.StyleNeeded(PositionBeforeWordStartPos); { Make sure the typed character has been styled }
    Result := _InitiateAutoCompleteOrCallTipAllowedAtPos(AMemo, LinePos,
      PositionBeforeWordStartPos, ISPPExpressionContext);
  end;

  function CanAutoCompleteValue(const Value: String): Boolean;
  begin
    for var C in Value do
      if not CharInSet(C, AutoCompleteWordChars) then
        Exit(False);
    Result := True;
  end;

  function GetAutoCompleteSignToolValues: TArray<AnsiString>;
  begin
    SetLength(Result, FSignTools.Count);
    var Count := 0;
    for var I := 0 to FSignTools.Count-1 do begin
      const Name = FSignTools.Names[I];
      if CanAutoCompleteValue(Name) then begin
        Result[Count] := AnsiString(Name);
        Inc(Count);
      end;
    end;
    SetLength(Result, Count);
  end;

  function GetAutoCompleteScriptValues(const ParameterName: String): TArray<AnsiString>;
  begin
    const Values = CollectParameterValuesFromFactories(
      [LiveScriptObjectFactoryForMemo(AMemo), LiveScriptObjectFactoryForMainMemo],
      ParameterName);
    SetLength(Result, Length(Values));
    var Count := 0;
    for var Value in Values do begin
      if CanAutoCompleteValue(Value) then begin
        Result[Count] := AnsiString(Value);
        Inc(Count);
      end;
    end;
    SetLength(Result, Count);
    if IsScriptBooleanExpressionParameter(ParameterName) then
      Result := Result + BooleanExpressionOperatorValues;
  end;

  function ParameterHasAutoCompleteValues(const ParameterWord: String;
    const Section: TInnoSetupSection): Boolean;
  begin
    { A parameter has autocomplete values if there's either a known list
      of possible values, or the possible values can be determined from
      the script }
    Result :=
      (GetMemberValuesAutoCompleteWordList(Section, ParameterWord) <> '') or
      (GetScriptSectionDefiningParameterValues(ParameterWord) <> scNone);
  end;

  type
    TLineScanResult = record
      { These are all 'preceding'. So for example if FoundWord is True then there's a
        word before the current word. }
      FoundSemicolon: Boolean;
      FoundWord: Boolean;
      FoundNonInlineISPPDirectiveWord: Boolean;
      FoundNonFlagWord: Boolean;
      FoundMemberName: String;
      FoundMultipleSetupDirectiveValues: Boolean;
    end;

  function LineScanBackwards(const LinePos, WordStartPos: Integer;
    const Section: TInnoSetupSection; const IsParamSection: Boolean;
    out Res: TLineScanResult): Boolean;
  begin
    Result := False;
    Res := Default(TLineScanResult);
    var I := WordStartPos;
    while I > LinePos do begin
      I := AMemo.GetPositionBefore(I);
      if I < LinePos then
        Exit;  { shouldn't get here }
      const C = AMemo.GetByteAtPosition(I);

      { Note: The first time we get here C equals the character before the current word,
        like a space before the current flag }

      if IsParamSection and (C in [';', ':']) and
        TInnoSetupStyler.IsSymbolStyle(AMemo.GetStyleAtPosition(I)) then begin { Make sure it's an stSymbol ';' or ':' and not one inside a quoted string or comment }
        Res.FoundSemicolon := C = ';';
        if not Res.FoundSemicolon then begin
          const ParameterWordEndPos = I;
          const ParameterWordStartPos = AMemo.GetWordStartPosition(ParameterWordEndPos, True);
          const ParameterWord = AMemo.GetTextRange(ParameterWordStartPos, ParameterWordEndPos);
          if ParameterHasAutoCompleteValues(ParameterWord, Section) then
            Res.FoundMemberName := ParameterWord;
        end;
        if Res.FoundSemicolon or (Res.FoundMemberName <> '') then
          Break;
      end;

      if ((Section in DirectiveSections) and (C = '=')) or
         ((Section in [scLangOptions, scMessages]) and (C = '.')) then begin
        { Verify that a word (language or directive name) precedes the '.' or '=', then check for
          any non-whitespace characters before the word. Among other things, this ensures
          we're not inside a comment. }
        const NameStartPos = AMemo.GetWordStartPosition(I, True);
        if NameStartPos >= I then
          Exit;
        var NameOrPrefixStartPos := NameStartPos;
        if (Section = scLangOptions) and (C = '=') then
          NameOrPrefixStartPos := SkipLanguagePrefixBeforeWord(LinePos, NameStartPos);
        if not OnlyWhiteSpaceBeforeWord(LinePos, NameOrPrefixStartPos) then
          Exit;
        if C = '=' then begin
          const NameEndPos = AMemo.GetWordEndPosition(NameStartPos, True);
          Res.FoundMemberName := AMemo.GetTextRange(NameStartPos, NameEndPos);
        end;
        Break;
      end else if C > ' ' then begin
        if IsParamSection then begin
          { Remember there's a word before the current word (or before that when we get
            here again), and whether it's an inline ISPP directive or a valid flag,
            but either way continue looking before it instead of stopping }
          Res.FoundWord := True;
          const PrecedingEndPos = AMemo.GetWordEndPosition(I, True);
          const PrecedingStartPos = AMemo.GetWordStartPosition(I, True);
          const PrecedingWord = AMemo.GetTextRange(PrecedingStartPos, PrecedingEndPos);
          const PrecedingWordIsInlineISPPDirective = PrecedingWord.StartsWith('{#');
          if not PrecedingWordIsInlineISPPDirective then
            Res.FoundNonInlineISPPDirectiveWord := True;
          const CanBeFlag = GetMemberValuesAutoCompleteWordList(Section, 'Flags') <> '';
          if not (CanBeFlag and ((FlagsWords[Section].IndexOf(PrecedingWord) <> -1) or
             PrecedingWordIsInlineISPPDirective)) then
            Res.FoundNonFlagWord := True;
          I := PrecedingStartPos;
        end else if Section = scSetup then begin
          { Continue looking for '='. We don't do a verification like it does for
            flags above because we don't know the directive name yet. In fact, we
            don't even know whether we are before or after the '='. As a workaround
            we check for the expected style before '=', which is stKeyword or stComment,
            and only continue if we don't find that. }
          if not TInnoSetupStyler.IsCommentOrKeywordStyle(AMemo.GetStyleAtPosition(I)) then begin
            Res.FoundMultipleSetupDirectiveValues := True;
            I := AMemo.GetWordStartPosition(I, True);
          end else
            Exit;
        end else
          Exit; { Non-whitespace which should not be there }
      end;
    end;
    Result := True;
  end;

  function BuildUserDefinedWordList(const Line: Integer;
    const ClassMember: Boolean): AnsiString;
  const
    TypeWordTypes: array[Boolean] of TAutoCompleteWordType = (awtScriptType, awtScriptInterface);
  begin
    Result := '';
    if not _TryAcquireAndHoldCodeSectionAtLine(AMemo, Line) then
      Exit;

    const Words = CreateSortedCaseInsensitiveStringList;
    try
      const Section = FAutoCompleteAndCallTipsLiveCodeSection.Section;
      if ClassMember then begin
        for var I := 0 to Section.InterfaceMethodCount-1 do
          AddAutoCompleteWordToList(Words,
            AnsiString(Section.InterfaceMethods[I].Name), awtScriptFunction);
      end else begin
        for var I := 0 to Section.RoutineCount-1 do
          AddAutoCompleteWordToList(Words,
            AnsiString(Section.Routines[I].Name), awtScriptFunction);
        for var I := 0 to Section.TypeCount-1 do begin
          const Declaration = Section.Types[I];
          AddAutoCompleteWordToList(Words, AnsiString(Declaration.Name),
            TypeWordTypes[Declaration.TypeText = 'interface']);
        end;
        for var I := 0 to Section.EnumerationValueCount-1 do
          AddAutoCompleteWordToList(Words,
            AnsiString(Section.EnumerationValues[I].Name), awtScriptEnumValue);
        for var I := 0 to Section.ConstantCount-1 do
          AddAutoCompleteWordToList(Words,
            AnsiString(Section.Constants[I].Name), awtScriptConstant);
        for var I := 0 to Section.GlobalVariableCount-1 do
          AddAutoCompleteWordToList(Words,
            AnsiString(Section.GlobalVariables[I].Name), awtScriptVariable);
      end;
      Result := BuildAutoCompleteWordList(Words, False);
    finally
      Words.Free;
    end;

    var Routine: TCodeSectionRoutine;
    if not ClassMember and
       FAutoCompleteAndCallTipsLiveCodeSection.TryGetRoutine(Line, Routine, True) then begin
      const RoutineWords = CreateSortedCaseInsensitiveStringList;
      try
        for var I := 0 to Routine.ParameterCount-1 do
          AddAutoCompleteWordToList(RoutineWords,
            AnsiString(Routine.Parameters[I].Name), awtScriptFunctionParameter);
        for var I := 0 to Routine.LocalCount-1 do
          AddAutoCompleteWordToList(RoutineWords,
            AnsiString(Routine.Locals[I].Name), awtScriptFunctionVariable);
        if Routine.Kind = rkFunction then
          AddAutoCompleteWordToList(RoutineWords, 'Result', awtScriptFunctionVariable);
        { RoutineWords (locals) shadows Result (globals) }
        Result := MergeScopedAutoCompleteWordLists(Result,
          BuildAutoCompleteWordList(RoutineWords, False));
      finally
        RoutineWords.Free;
      end;
    end;
  end;

  function DotIsPartOfNumberOrRange(const LinePos, DotPos: Integer): Boolean;
  begin
    { Ask the styler if the dot is part of a number }
    AMemo.StyleNeeded(DotPos);
    if TInnoSetupStyler.IsPascalNumberStyle(AMemo.GetStyleAtPosition(DotPos)) then
      Exit(True);
    { Check if the dot is part of a range ourselves }
    const PositionBeforeDotPos = AMemo.GetPositionBefore(DotPos);
    Result := (PositionBeforeDotPos >= LinePos) and
      (AMemo.GetByteAtPosition(PositionBeforeDotPos) = '.');
  end;

  function ChooseCodeWordList(const LinePos, WordStartPos: Integer;
    out WordList: AnsiString): Boolean;
  begin
    Result := False;

    { Space can only initiate autocompletion after non whitespace }
    if (Key = ' ') and OnlyWhiteSpaceBeforeWord(LinePos, WordStartPos) then
      Exit;

    if (Key <> #0) and not StyleAllowsAutoStart(LinePos, WordStartPos, False) then
      Exit;

    const PositionBeforeWordStartPos = AMemo.GetPositionBefore(WordStartPos);

    { Autocomplete event functions if the current word on the line has
      exactly 1 space before it which has the word 'function' or
      'procedure' before it which has only whitespace before it }
    if (PositionBeforeWordStartPos >= LinePos) and (AMemo.GetByteAtPosition(PositionBeforeWordStartPos) <= ' ') then begin
      const FunctionWordEndPos = PositionBeforeWordStartPos;
      const FunctionWordStartPos = AMemo.GetWordStartPosition(FunctionWordEndPos, True);
      if OnlyWhiteSpaceBeforeWord(LinePos, FunctionWordStartPos) then begin
        const FunctionWord = AMemo.GetTextRange(FunctionWordStartPos, FunctionWordEndPos);
        if SameText(FunctionWord, 'procedure') then
          WordList := GetEventFunctionsAutoCompleteWordList(True)
        else if SameText(FunctionWord, 'function') then
          WordList := GetEventFunctionsAutoCompleteWordList(False);
      end;
    end;

    { If no event function was found then autocomplete script functions,
      types, etc, or class members if the current word has a dot before it,
      merging in the current section's own functions, types, etc. }
    if WordList = '' then begin
      var ClassOrRecordMember := False;
      if (PositionBeforeWordStartPos >= LinePos) and (AMemo.GetByteAtPosition(PositionBeforeWordStartPos) = '.') then begin
        if DotIsPartOfNumberOrRange(LinePos, PositionBeforeWordStartPos) then
          Exit;
        ClassOrRecordMember := True;
      end;
      const UserDefinedWordList = BuildUserDefinedWordList(
        AMemo.GetLineFromPosition(LinePos), ClassOrRecordMember);
      if ClassOrRecordMember then
        WordList := MergeAutoCompleteWordLists(
          GetScriptAutoCompleteWordList(True), UserDefinedWordList)
      else begin
        { UserDefinedWordList (globals shadowed by locals) shadows GetScriptAutoCompleteWordList (built-ins) }
        WordList := MergeScopedAutoCompleteWordLists(
          GetScriptAutoCompleteWordList(False), UserDefinedWordList);
      end;
    end;

    if WordList = '' then
      Exit;
    Result := True;
  end;

  function CanAutoComplete(const Res: TLineScanResult;
    const Section: TInnoSetupSection): Boolean;
  begin
    Result := False;
    { No member found before the current word means a parameter name is being
      typed: don't autocomplete it if anything other than inline ISPP
      directives (which might expand to full parameters) stands between the
      last ';' (or start of the line) and the current word }
    if (Res.FoundMemberName = '') and Res.FoundNonInlineISPPDirectiveWord then
      Exit;
    { Don't autocomplete a flag if anything other than other flags or inline
      ISPP directives stands between 'Flags:' and the current word }
    if SameText(Res.FoundMemberName, 'Flags') and Res.FoundNonFlagWord then
      Exit;
    { Don't autocomplete a value of a parameter accepting a single value only
      (like Type) if anything stands between the parameter name and the
      current word }
    if ParameterValueIsSingleValue(Res.FoundMemberName, Section) and Res.FoundWord then
      Exit;
    { A space can only initiate autocompletion after ';' or a member name }
    if (Key = ' ') and not (Res.FoundSemicolon or (Res.FoundMemberName <> '')) then
      Exit;
    Result := True;
  end;

  function ChooseWordList(const Res: TLineScanResult;
    const Section: TInnoSetupSection; const IsParamSection: Boolean;
    out WordList, FillupChars: AnsiString;
    var ExtraContinueChars: TSysCharSet;
    var AutoCompleteOrder: TScintAutoCompleteOrder): Boolean;
  begin
    Result := False;
    if Res.FoundMemberName <> '' then begin
      { Autocompleting a value }
      if Section = scSetup then begin
        const V = GetEnumValue(TypeInfo(TSetupSectionDirective), SetupSectionDirectivePrefix + Res.FoundMemberName);
        if V <> -1 then begin
          const Directive = TSetupSectionDirective(V);
          if not Res.FoundMultipleSetupDirectiveValues or
            SetupSectionDirectiveValueIsMultiValue(Directive) then begin
            if Directive = ssSignTool then
              WordList := BuildAutoCompleteWordList(GetAutoCompleteSignToolValues, awtMemberValue)
            else
              WordList := GetMemberValuesAutoCompleteWordList(Section, Res.FoundMemberName);
            if Directive in [ssArchiveExtraction, ssCompression] then
              ExtraContinueChars := ['/'];
          end;
        end;
        if WordList = '' then
          Exit;
      end else begin
        WordList := GetMemberValuesAutoCompleteWordList(Section, Res.FoundMemberName);
        if WordList <> '' then begin
          if SameText(Res.FoundMemberName, 'Permissions') then
            ExtraContinueChars := ['-'];
        end else begin
          WordList := BuildAutoCompleteWordList(
            GetAutoCompleteScriptValues(Res.FoundMemberName), awtMemberValue);
          if WordList = '' then
            Exit;
        end;
      end;
      if MemberKnownValuesAreCustomSorted(Res.FoundMemberName, Section) then
        AutoCompleteOrder := sacoCustom;
      FillupChars := ' ';
    end else begin
      { Autocompleting a name }
      WordList := MemberNamesAutoCompleteWordList[Section];
      if WordList = '' then { [CustomMessages] }
        Exit;
      if IsParamSection then
        FillupChars := ':'
      else
        FillupChars := '=';
    end;
    Result := True;
  end;

begin
  if AMemo.AutoCompleteActive or AMemo.ReadOnly then
    Exit;

  if Key = #0 then begin
    { If a character is typed then Scintilla will handle selections but
      otherwise we should empty them and also make sure the caret is visible
      before we start autocompletion }
    AMemo.SetEmptySelections;
    AMemo.ScrollCaretIntoView;
  end;

  const CaretPos = AMemo.CaretPosition;
  const Line = AMemo.GetLineFromPosition(CaretPos);
  const LinePos = AMemo.GetPositionFromLine(Line);

  var CharsBefore: Integer;
  var WordList: AnsiString;
  var FillupChars: AnsiString;
  var ExtraContinueChars: TSysCharSet := [];
  var AutoCompleteOrder := sacoPreSorted;

  var IsPragmaContext: Boolean;
  if FMemosStyler.ISPPInstalled and IsInISPPLineContext(AMemo, LinePos, CaretPos, IsPragmaContext) and not IsPragmaContext then begin
    { Calculate CharsBefore without using GetWordStartPosition and GetWordEndPosition because '[' is a word char }
    CharsBefore := 0;
    var WordStartPos := CaretPos;
    while WordStartPos > LinePos do begin
      const PosBefore = AMemo.GetPositionBefore(WordStartPos);
      if not (AMemo.GetByteAtPosition(PosBefore) in ISPPIdentChars) then
        Break;
      WordStartPos := PosBefore;
      Inc(CharsBefore);
    end;

    { Note that the ISPPIdentChars check is the ISPP equivalent of the
      WordEndPos > CaretPos check below: don't auto start when the caret is
      inside the identifier }
    if (Key <> #0) and
       (not CanAutoStartAtWord(CharsBefore, AMemo.GetByteAtPosition(CaretPos) in ISPPIdentChars) or
        not StyleAllowsAutoStart(LinePos, WordStartPos, True)) then
      Exit;
    WordList := ISPPExpressionAutoCompleteWordList;
  end else if FMemosStyler.ISPPInstalled and IsPragmaContext then begin
    const WordStartPos = AMemo.GetWordStartPosition(CaretPos, True);
    const WordEndPos = AMemo.GetWordEndPosition(CaretPos, True);

    CharsBefore := CaretPos - WordStartPos;
    if (Key <> #0) and
       (not CanAutoStartAtWord(CharsBefore, WordEndPos > CaretPos) or
        not StyleAllowsAutoStart(LinePos, WordStartPos, True)) then
      Exit;
    WordList := ISPPPragmaAutoCompleteWordList;
    FillupChars := ' ';
  end else begin
    const WordStartPos = AMemo.GetWordStartPosition(CaretPos, True);
    const WordEndPos = AMemo.GetWordEndPosition(CaretPos, True);

    CharsBefore := CaretPos - WordStartPos;
    if (Key <> #0) and not CanAutoStartAtWord(CharsBefore, WordEndPos > CaretPos) then
      Exit;
    case AMemo.GetByteAtPosition(WordStartPos) of
      '#':
        begin
          if not OnlyWhiteSpaceBeforeWord(LinePos, WordStartPos) then
            Exit;
          WordList := ISPPDirectivesAutoCompleteWordList;
          FillupChars := ' ';
        end;
      '{':
        begin
          WordList := ConstantsAutoCompleteWordList;
          FillupChars := '\:';
        end;
      '[':
        begin
          if not OnlyWhiteSpaceBeforeWord(LinePos, WordStartPos) then
            Exit;
          WordList := SectionsAutoCompleteWordList;
        end;
      else
        begin
          const Section = TInnoSetupStyler.GetSectionFromLineState(AMemo.Lines.State[Line]);
          if Section in [scUnknown, scThirdParty] then
            Exit
          else if Section = scCode then begin
            if not ChooseCodeWordList(LinePos, WordStartPos, WordList) then
              Exit;
          end else begin
            const IsParamSection = Section in ParameterSections;
            var Res: TLineScanResult;
            if not LineScanBackwards(LinePos, WordStartPos, Section, IsParamSection, Res) or
               not CanAutoComplete(Res, Section) or
               not ChooseWordList(Res, Section, IsParamSection, WordList, FillupChars, ExtraContinueChars, AutoCompleteOrder) then
              Exit;
          end;
        end;
    end;
  end;

  FAutoCompleteExtraContinueChars := ExtraContinueChars; { Used by ExtendCharsBefore (above) and AutoCompleteAndCallTipsHandleCharAdded (below) }
  if FAutoCompleteExtraContinueChars <> [] then
    CharsBefore := ExtendCharsBefore(LinePos, CaretPos, CharsBefore);

  AMemo.SetAutoCompleteFillupChars(FillupChars);
  AMemo.SetAutoCompleteOrder(AutoCompleteOrder);
  AMemo.ShowAutoComplete(CharsBefore, WordList);
end;

function TMainFormAutoCompleteAndCallTipsHelper.BuildUserDefinedFunctionDefinitions(
  const AMemo: TScintEdit; const ALine: Integer;
  const AClassMember: Boolean): TFunctionDefinitionsWithName;

  function IsAsciiString(const S: String): Boolean;
  begin
    for var C in S do
      if C > #127 then
        Exit(False);
    Result := True;
  end;

  procedure AddDefinition(const Prototypes: TStringList; const Name: String;
    const Kind: TCodeSectionRoutineKind; const Prototype: String;
    const HasParameters: Boolean);
  begin
    if not IsAsciiString(Prototype) or (Prototypes.IndexOf(Prototype) >= 0) then
      Exit;
    Prototypes.Add(Prototype);
    var HeaderKind: TScriptFuncHeaderKind;
    if Kind = rkFunction then
      HeaderKind := hkFunction
    else
      HeaderKind := hkProcedure;
    var UserDefinedFunctionDefinition: TFunctionDefinitionWithName;
    UserDefinedFunctionDefinition.Name := Name;
    UserDefinedFunctionDefinition.Definition :=
      TFunctionDefinition.CreateUserDefined(Prototype, HeaderKind, HasParameters);
    Result := Result + [UserDefinedFunctionDefinition];
  end;

begin
  Result := [];
  if not _TryAcquireAndHoldCodeSectionAtLine(AMemo, ALine) then
    Exit;

  const Prototypes = CreateSortedCaseInsensitiveStringList;
  try
    const Section = FAutoCompleteAndCallTipsLiveCodeSection.Section;
    if AClassMember then begin
      for var I := 0 to Section.InterfaceMethodCount-1 do begin
        const Method = Section.InterfaceMethods[I];
        AddDefinition(Prototypes, Method.Name, Method.Kind, Method.Prototype,
          Method.ParameterCount > 0);
      end;
    end else begin
      for var I := 0 to Section.RoutineCount-1 do begin
        const Routine = Section.Routines[I];
        AddDefinition(Prototypes, Routine.Name, Routine.Kind, Routine.Prototype,
          Routine.ParameterCount > 0);
      end;
    end;
  finally
    Prototypes.Free;
  end;
end;

procedure TMainFormAutoCompleteAndCallTipsHelper._UpdateCallTipFunctionDefinition(const AMemo: TScintEdit;
  const Pos: Integer { = -1 });
begin
  { Based on SciTE 5.50's SciTEBase::FillFunctionDefinition }

  if Pos > 0 then
    FCallTipState.LastPosCallTip := Pos;

  // Should get current api definition
  const CurrentCallTipWord = AMemo.ConvertRawStringToString(FCallTipState.CurrentCallTipWord);
  var FunctionDefinition: TFunctionDefinition;
  if FCallTipState.ISPPExpressionContext then
    FunctionDefinition := GetISPPFunctionDefinition(CurrentCallTipWord, FCallTipState.CurrentCallTip, FCallTipState.MaxCallTips)
  else begin
    const UserDefined = BuildUserDefinedFunctionDefinitions(AMemo,
      AMemo.GetLineFromPosition(FCallTipState.LastPosCallTip),
      FCallTipState.ClassOrRecordMember);
    FunctionDefinition := GetScriptFunctionDefinition(FCallTipState.ClassOrRecordMember, CurrentCallTipWord, FCallTipState.CurrentCallTip, UserDefined, FCallTipState.MaxCallTips);
  end;
  if ((FCallTipState.MaxCallTips = 1) and FunctionDefinition.HasParameters) or //if there's a single definition then only show if it has a parameter
     (FCallTipState.MaxCallTips > 1) then begin                            //if there's multiple then show always just like MemoHintShow, so even the one without parameters if it exists
    FCallTipState.FunctionDefinition := FunctionDefinition.ScriptFuncWithoutHeader;
    if FCallTipState.MaxCallTips > 1 then
      FCallTipState.FunctionDefinition := AnsiString(Format(#1'%d of %d'#2'%s', [FCallTipState.CurrentCallTip+1, FCallTipState.MaxCallTips, FCallTipState.FunctionDefinition]));

    AMemo.ShowCallTip(FCallTipState.LastPosCallTip - Length(FCallTipState.CurrentCallTipWord), FCallTipState.FunctionDefinition);
    _ContinueCallTip(AMemo);
  end;
end;

procedure TMainFormAutoCompleteAndCallTipsHelper._InitiateCallTip(const AMemo: TScintEdit; const Key: AnsiChar);
begin
  var Pos := AMemo.CaretPosition;

  const Line = AMemo.GetLineFromPosition(Pos);
  const LinePos = AMemo.GetPositionFromLine(Line);
  var IsPragmaContext: Boolean;
  const ISPPExpressionContext = FMemosStyler.ISPPInstalled and
    IsInISPPLineContext(AMemo, LinePos, AMemo.GetPositionBefore(Pos), IsPragmaContext) and not IsPragmaContext;

  if (not ISPPExpressionContext and (TInnoSetupStyler.GetSectionFromLineState(AMemo.Lines.State[Line]) <> scCode)) or
     ((Key <> #0) and not _InitiateAutoCompleteOrCallTipAllowedAtPos(AMemo,
       LinePos, AMemo.GetPositionBefore(Pos), ISPPExpressionContext)) then
    Exit;

  { Based on SciTE 5.50's SciTEBase::StartCallTip }

  FCallTipState.CurrentCallTip := 0;
  FCallTipState.CurrentCallTipWord := '';
  var LineText := AMemo.RawCaretLineText;
  var Current := AMemo.CaretColumn;
  var CallTipWordCharacters := AMemo.WordCharsAsSet;
  if ISPPExpressionContext then
    Exclude(CallTipWordCharacters, '['); { Also see InitiateAutoComplete }

  {$ZEROBASEDSTRINGS ON}
  repeat
    var Braces := 0;
    while ((Current > 0) and ((Braces <> 0) or not (LineText[Current-1] = '('))) do begin
      if LineText[Current-1] = '(' then
        Dec(Braces)
      else if LineText[Current-1] = ')' then
        Inc(Braces);
      Dec(Current);
      Dec(Pos);
    end;
    if Current > 0 then begin
      Dec(Current);
      Dec(Pos);
    end else
      Break;
    while (Current > 0) and (LineText[Current-1] <= ' ') do begin
      Dec(Current);
      Dec(Pos);
    end
  until not ((Current > 0) and not CharInSet(LineText[Current-1], CallTipWordCharacters));
  {$ZEROBASEDSTRINGS OFF}
  if Current <= 0 then
    Exit;

  FCallTipState.StartCallTipWord := Current - 1;
  {$ZEROBASEDSTRINGS ON}
  while (FCallTipState.StartCallTipWord > 0) and CharInSet(LineText[FCallTipState.StartCallTipWord-1], CallTipWordCharacters) do
    Dec(FCallTipState.StartCallTipWord);
  FCallTipState.ISPPExpressionContext := ISPPExpressionContext;
  if ISPPExpressionContext then
    FCallTipState.ClassOrRecordMember := False { Value doesn't really matter }
  else
    FCallTipState.ClassOrRecordMember := (FCallTipState.StartCallTipWord > 0) and (LineText[FCallTipState.StartCallTipWord-1] = '.');
  {$ZEROBASEDSTRINGS OFF}

  SetLength(LineText, Current);
  FCallTipState.CurrentCallTipWord := Copy(LineText, FCallTipState.StartCallTipWord+1, MaxInt);

  FCallTipState.FunctionDefinition := '';
  _UpdateCallTipFunctionDefinition(AMemo, Pos);
end;

procedure TMainFormAutoCompleteAndCallTipsHelper._CountCallTipBracesAndCommas(
  const AMemo: TScintEdit; out Braces, Commas: Integer);
begin
  { Based on SciTE 5.50's SciTEBase::ContinueCallTip }

  const Line = AMemo.RawCaretLineText;
  const Current = AMemo.CaretColumn;

  Braces := 0;
  Commas := 0;
  for var I := FCallTipState.StartCallTipWord to Current-1 do begin
    {$ZEROBASEDSTRINGS ON}
    if CharInSet(Line[I], ['(', '[']) then
      Inc(Braces)
    else if CharInSet(Line[I], [')', ']']) and (Braces > 0) then
      Dec(Braces)
    else if (Braces = 1) and (Line[I] = ',') then
      Inc(Commas);
    {$ZEROBASEDSTRINGS OFF}
  end;
end;

procedure TMainFormAutoCompleteAndCallTipsHelper._ContinueCallTip(const AMemo: TScintEdit);
begin
  { Based on SciTE 5.50's SciTEBase::ContinueCallTip }

  var Braces, Commas: Integer;
  _CountCallTipBracesAndCommas(AMemo, Braces {unused}, Commas);

  {$ZEROBASEDSTRINGS ON}
  var StartHighlight := 0;
  const FunctionDefinition = FCallTipState.FunctionDefinition;
  const FunctionDefinitionLength = Length(FunctionDefinition);
  while (StartHighlight < FunctionDefinitionLength) and not (FunctionDefinition[StartHighlight] = '(') do
    Inc(StartHighlight);
  if (StartHighlight < FunctionDefinitionLength) and (FunctionDefinition[StartHighlight] = '(') then
    Inc(StartHighlight);
  while (StartHighlight < FunctionDefinitionLength) and (Commas > 0) do begin
    if FunctionDefinition[StartHighlight] in [',', ';'] then
      Dec(Commas);
    // If it reached the end of the argument list it means that the user typed in more
    // arguments than the ones listed in the calltip
    if FunctionDefinition[StartHighlight] = ')' then
      Commas := 0
    else
      Inc(StartHighlight);
  end;
  if (StartHighlight < FunctionDefinitionLength) and (FunctionDefinition[StartHighlight] in [',', ';']) then
    Inc(StartHighlight);
  var EndHighlight := StartHighlight;
  while (EndHighlight < FunctionDefinitionLength) and not (FunctionDefinition[EndHighlight] in [',', ';']) and not (FunctionDefinition[EndHighlight] = ')') do
    Inc(EndHighlight);
  {$ZEROBASEDSTRINGS OFF}

  AMemo.SetCallTipHighlight(StartHighlight, EndHighlight);
end;

procedure TMainFormAutoCompleteAndCallTipsHelper.CallTipsHandleUpdateUI(const AMemo: TScintEdit);
begin
  { This helper should be called on SCN_UPDATEUI to refresh the calltip
    highlight on horizontal caret movement, and also on edits which
    don't call SCN_CHARADDED, like Backspace. Note: this code seems
    to be missing from SciTE 5.50: it simply doesn't update highlight
    in these cases (nor update the current state's BraceCount).

    Note: SCN_UPDATEUI also fires after SCN_CHARADDED. In this case we
    still update the already-up-to-date highlight here. This keeps things
    simple and not dependent on any specific notification order.

    Also note: Scintilla itself handles calltip cancellation when
    moving to another line, or when using Home/End, and probably
    all other cases where it can do so without knowing the
    language specific rules for calltips. }

  if not AMemo.CallTipActive then
    Exit;

  var Braces, Commas: Integer;
  _CountCallTipBracesAndCommas(AMemo, Braces, Commas {unused});

  FCallTipState.BraceCount := Braces;

  if FCallTipState.BraceCount < 1 then
    AMemo.CancelCallTip
  else
    _ContinueCallTip(AMemo);
end;

procedure TMainFormAutoCompleteAndCallTipsHelper.AutoCompleteAndCallTipsHandleCharAdded(
  const AMemo: TScintEdit; const Ch: AnsiChar);
begin
  { Based on SciTE 5.50's SciTEBase::CharAdded but with an altered interaction
    between calltips and autocomplete. Also see CallTipsHandleUpdateUI for
    additional calltips code. }

  var DoAutoComplete := False;

  if AMemo.CallTipActive then begin
    if Ch = ')' then begin
      Dec(FCallTipState.BraceCount);
      if FCallTipState.BraceCount < 1 then
        AMemo.CancelCallTip
      else if FOptions.AutoCallTips then
        _InitiateCallTip(AMemo, Ch);
    end else if Ch = '(' then begin
      Inc(FCallTipState.BraceCount);
      if FOptions.AutoCallTips then
        _InitiateCallTip(AMemo, Ch);
    end else
      _ContinueCallTip(AMemo);
  end else if AMemo.AutoCompleteActive then begin
    if Ch = '(' then begin
      Inc(FCallTipState.BraceCount);
      if FOptions.AutoCallTips then begin
        _InitiateCallTip(AMemo, Ch);
        if not AMemo.CallTipActive then begin
          { Normally the calltip activation means any active autocompletion gets
            cancelled by Scintilla but if the current word has no call tip then
            we should make sure ourselves that the added brace still cancels
            the currently active autocompletion }
          DoAutoComplete := True;
        end;
      end;
    end else if Ch = ')' then
      Dec(FCallTipState.BraceCount)
    else
      DoAutoComplete := True;
  end else if Ch = '(' then begin
    FCallTipState.BraceCount := 1;
    if FOptions.AutoCallTips then
      _InitiateCallTip(AMemo, Ch);
  end else
    DoAutoComplete := True;

  if DoAutoComplete then begin
    if Ch in AutoCompleteStartOrContinueChars then begin
      if not AMemo.AutoCompleteActive and FOptions.AutoAutoComplete and not (Ch in ['0'..'9']) then
        InitiateAutoComplete(AMemo, Ch);
    end else begin
      const ContinueAutoComplete = AMemo.AutoCompleteActive and (Ch in FAutoCompleteExtraContinueChars);
      if not ContinueAutoComplete then begin
        const RestartAutoComplete = (Ch in [' ', '.', '!', '=']) and
          (FOptions.AutoAutoComplete or AMemo.AutoCompleteActive);
        AMemo.CancelAutoComplete;
        if RestartAutoComplete then
          InitiateAutoComplete(AMemo, Ch);
      end;
    end;
  end;
end;

procedure TMainFormAutoCompleteAndCallTipsHelper.CallTipsHandleArrowClick(const AMemo: TScintEdit;
  const Up: Boolean);
begin
  { Based on SciTE 5.50's SciTEBase::Notify SA::Notification::CallTipClick }
  if Up and (FCallTipState.CurrentCallTip > 0) then begin
    Dec(FCallTipState.CurrentCallTip);
    _UpdateCallTipFunctionDefinition(AMemo);
  end else if not Up and (FCallTipState.CurrentCallTip + 1 < FCallTipState.MaxCallTips) then begin
    Inc(FCallTipState.CurrentCallTip);
    _UpdateCallTipFunctionDefinition(AMemo);
  end;
end;

procedure TMainFormAutoCompleteAndCallTipsHelper.CallTipsHandleCtrlSpace(const AMemo: TScintEdit);
begin
  { Based on SciTE 5.50's SciTEBase::MenuCommand IDM_SHOWCALLTIP }
  if AMemo.CallTipActive then begin
    FCallTipState.CurrentCallTip := IfThen(FCallTipState.CurrentCallTip + 1 = FCallTipState.MaxCallTips, 0, FCallTipState.CurrentCallTip + 1);
    _UpdateCallTipFunctionDefinition(AMemo);
  end else begin
    FCallTipState.BraceCount := 1; { Missing in SciTE, see https://sourceforge.net/p/scintilla/bugs/2446/ }
    _InitiateCallTip(AMemo, #0);
  end;
end;

end.
