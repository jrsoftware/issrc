unit IDE.MainForm.AutoCompleteAndCallTipsHelper;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler form - Auto complete & call tips helper which has the tools helper as ancestor

  Not used by MainForm: it uses IDE.MainForm.FinalHelper instead
}

interface

uses
  Menus,
  ScintEdit,
  IDE.MainForm, IDE.MainForm.ToolsHelper;

type
  TMainFormAutoCompleteAndCallTipsHelper = class helper(TMainFormToolsHelper) for TMainForm
    procedure InitiateAutoComplete(const AMemo: TScintEdit; const Key: AnsiChar);
    procedure AutoCompleteAndCallTipsHandleCharAdded(const AMemo: TScintEdit; const Ch: AnsiChar);
    procedure CallTipsHandleArrowClick(const AMemo: TScintEdit; const Up: Boolean);
    procedure CallTipsHandleCtrlSpace(const AMemo: TScintEdit);
    { Private }
    function _InitiateAutoCompleteOrCallTipAllowedAtPos(const AMemo: TScintEdit;
      const WordStartLinePos, PositionBeforeWordStartPos: Integer): Boolean;
    procedure _UpdateCallTipFunctionDefinition(const AMemo: TScintEdit; const Pos: Integer = -1);
    procedure _InitiateCallTip(const AMemo: TScintEdit; const Key: AnsiChar);
    procedure _ContinueCallTip(const AMemo: TScintEdit);
  end;

implementation

uses
  SysUtils, Math, TypInfo,
  Shared.SetupSectionDirectives,
  IDE.ScintStylerInnoSetup;

function TMainFormAutoCompleteAndCallTipsHelper._InitiateAutoCompleteOrCallTipAllowedAtPos(const AMemo: TScintEdit;
  const WordStartLinePos, PositionBeforeWordStartPos: Integer): Boolean;
begin
  Result := (PositionBeforeWordStartPos < WordStartLinePos) or
            not FMemosStyler.IsCommentOrPascalStringStyle(AMemo.GetStyleAtPosition(PositionBeforeWordStartPos));
end;

procedure TMainFormAutoCompleteAndCallTipsHelper.InitiateAutoComplete(const AMemo: TScintEdit; const Key: AnsiChar);

  function OnlyWhiteSpaceBeforeWord(const AMemo: TScintEdit; const LinePos, WordStartPos: Integer): Boolean;
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

  const WordStartPos = AMemo.GetWordStartPosition(CaretPos, True);
  const WordEndPos = AMemo.GetWordEndPosition(CaretPos, True);
  const CharsBefore = CaretPos - WordStartPos;

  { Don't auto start autocompletion after a character is typed if there are any
    word characters adjacent to the character }
  if Key <> #0 then begin
    if CharsBefore > 1 then
      Exit;
    if WordEndPos > CaretPos then
      Exit;
  end;

  var WordList: AnsiString;

  case AMemo.GetByteAtPosition(WordStartPos) of
    '#':
      begin
        if not OnlyWhiteSpaceBeforeWord(AMemo, LinePos, WordStartPos) then
          Exit;
        WordList := FMemosStyler.ISPPDirectivesWordList;
        AMemo.SetAutoCompleteFillupChars(' ');
      end;
    '{':
      begin
        WordList := FMemosStyler.ConstantsWordList;
        AMemo.SetAutoCompleteFillupChars('\:');
      end;
    '[':
      begin
        if not OnlyWhiteSpaceBeforeWord(AMemo, LinePos, WordStartPos) then
          Exit;
        WordList := FMemosStyler.SectionsWordList;
        AMemo.SetAutoCompleteFillupChars('');
      end;
    else
      begin
        const Section = FMemosStyler.GetSectionFromLineState(AMemo.Lines.State[Line]);
        if Section = scCode then begin
          { Space can only initiate autocompletion after non whitespace }
          if (Key = ' ') and OnlyWhiteSpaceBeforeWord(AMemo, LinePos, WordStartPos) then
            Exit;

          const PositionBeforeWordStartPos = AMemo.GetPositionBefore(WordStartPos);
          if Key <> #0 then begin
            AMemo.StyleNeeded(PositionBeforeWordStartPos); { Make sure the typed character has been styled }
            if not _InitiateAutoCompleteOrCallTipAllowedAtPos(AMemo, LinePos, PositionBeforeWordStartPos) then
              Exit;
          end;

          WordList := '';

          { Autocomplete event functions if the current word on the line has
            exactly 1 space before it which has the word 'function' or
            'procedure' before it which has only whitespace before it }
          if (PositionBeforeWordStartPos >= LinePos) and (AMemo.GetByteAtPosition(PositionBeforeWordStartPos) <= ' ') then begin
            const FunctionWordEndPos = PositionBeforeWordStartPos;
            const FunctionWordStartPos = AMemo.GetWordStartPosition(FunctionWordEndPos, True);
            if OnlyWhiteSpaceBeforeWord(AMemo, LinePos, FunctionWordStartPos) then begin
              const FunctionWord = AMemo.GetTextRange(FunctionWordStartPos, FunctionWordEndPos);
              if SameText(FunctionWord, 'procedure') then
                WordList := FMemosStyler.EventFunctionsWordList[True]
              else if SameText(FunctionWord, 'function') then
                WordList := FMemosStyler.EventFunctionsWordList[False];
              if WordList <> '' then
                AMemo.SetAutoCompleteFillupChars('');
            end;
          end;

          { If no event function was found then autocomplete script functions,
            types, etc if the current word has no dot before it }
          if WordList = '' then begin
            const ClassOrRecordMember = (PositionBeforeWordStartPos >= LinePos) and (AMemo.GetByteAtPosition(PositionBeforeWordStartPos) = '.');
            WordList := FMemosStyler.ScriptWordList[ClassOrRecordMember];
            AMemo.SetAutoCompleteFillupChars('');
          end;

          if WordList = '' then
            Exit;
        end else begin
          const IsParamSection = FMemosStyler.IsParamSection(Section);

          { Autocomplete if the current word on the line has only whitespace
            before it, or else also: after the last ';' or after 'Flags:' or
            'Type:' in parameterized sections }
          var FoundSemicolon := False;
          var FoundFlagsOrType := False;
          var FoundSetupDirectiveName := '';
          var I := WordStartPos;
          while I > LinePos do begin
            I := AMemo.GetPositionBefore(I);
            if I < LinePos then
              Exit;  { shouldn't get here }
            const C = AMemo.GetByteAtPosition(I);

            { Note: The first time we get here C equals the character before the current word,
              like a space before the current flag }

            if IsParamSection and (C in [';', ':']) and
               FMemosStyler.IsSymbolStyle(AMemo.GetStyleAtPosition(I)) then begin { Make sure it's an stSymbol ';' or ':' and not one inside a quoted string or comment }
              FoundSemicolon := C = ';';
              if not FoundSemicolon then begin
                const ParameterWordEndPos = I;
                const ParameterWordStartPos = AMemo.GetWordStartPosition(ParameterWordEndPos, True);
                const ParameterWord = AMemo.GetTextRange(ParameterWordStartPos, ParameterWordEndPos);
                FoundFlagsOrType := SameText(ParameterWord, 'Flags') or
                                    ((Section in [scInstallDelete, scUninstallDelete]) and SameText(ParameterWord, 'Type'));
              end else
                FoundFlagsOrType := False;
              if FoundSemicolon or FoundFlagsOrType then
                Break;
            end;

            if ((Section = scLangOptions) and (C = '.')) or ((Section = scSetup) and (C = '=')) then begin
              { Verify that a word (language or directive name) precedes the '.' or '=', then check for
                any non-whitespace characters before the word. Among other things, this ensures
                we're not inside a comment. }
              const NameStartPos = AMemo.GetWordStartPosition(I, True);
              if (NameStartPos >= I) or not OnlyWhiteSpaceBeforeWord(AMemo, LinePos, NameStartPos) then
                Exit;
              if Section = scSetup then begin
                const NameEndPos = AMemo.GetWordEndPosition(NameStartPos, True);
                FoundSetupDirectiveName := AMemo.GetTextRange(NameStartPos, NameEndPos);
              end;
              Break;
            end else if C > ' ' then begin
              if IsParamSection and not (Section in [scInstallDelete, scUninstallDelete]) and
                 (FMemosStyler.FlagsWordList[Section] <> '') then begin
                { Verify word before the current word (or before that when we get here again) is
                  a valid flag and if so, continue looking before it instead of stopping }
                const FlagEndPos = AMemo.GetWordEndPosition(I, True);
                const FlagStartPos = AMemo.GetWordStartPosition(I, True);
                const FlagWord = AMemo.GetTextRange(FlagStartPos, FlagEndPos);
                if FMemosStyler.SectionHasFlag(Section, FlagWord) or FlagWord.StartsWith('{#') then
                  I := FlagStartPos
                else
                  Exit;
              end else if Section = scSetup then begin
                { Continue looking for '='. We don't do a verification like it does for
                  flags above because we don't know the directive name yet. In fact, we
                  don't even know whether we are before or after the '='. As a workaround
                  we check for the expected style before '=', which is stKeyword or stComment,
                  and only autocomplete if we don't find that. }
                if not FMemosStyler.IsCommentOrKeywordStyle(AMemo.GetStyleAtPosition(I)) then
                  I := AMemo.GetWordStartPosition(I, True)
                else
                  Exit;
              end else
                Exit; { Non-whitespace which should not be there }
            end;
          end;
          { Space can only initiate autocompletion after ';' or 'Flags:' or 'Type:' or a [Setup] directive }
          if (Key = ' ') and not (FoundSemicolon or FoundFlagsOrType or (FoundSetupDirectiveName <> '')) then
            Exit;

          if FoundSetupDirectiveName <> '' then begin
            const V = GetEnumValue(TypeInfo(TSetupSectionDirective), SetupSectionDirectivePrefix + FoundSetupDirectiveName);
            if V <> -1 then begin
              WordList := FMemosStyler.SetupSectionDirectiveValueWordList[TSetupSectionDirective(V)];
              if WordList = '' then
                Exit;
              AMemo.SetAutoCompleteFillupChars(' ');
            end else
              Exit;
          end else if FoundFlagsOrType then begin
            WordList := FMemosStyler.FlagsWordList[Section];
            if WordList = '' then
              Exit;
            AMemo.SetAutoCompleteFillupChars(' ');
          end else begin
            WordList := FMemosStyler.KeywordsWordList[Section];
            if WordList = '' then { CustomMessages }
              Exit;
            if IsParamSection then
              AMemo.SetAutoCompleteFillupChars(':')
            else
              AMemo.SetAutoCompleteFillupChars('=');
          end;
        end;
      end;
  end;
  AMemo.ShowAutoComplete(CharsBefore, WordList);
end;

procedure TMainFormAutoCompleteAndCallTipsHelper._UpdateCallTipFunctionDefinition(const AMemo: TScintEdit;
  const Pos: Integer { = -1 });
begin
  { Based on SciTE 5.50's SciTEBase::FillFunctionDefinition }

  if Pos > 0 then
    FCallTipState.LastPosCallTip := Pos;

  // Should get current api definition
  var FunctionDefinition := FMemosStyler.GetScriptFunctionDefinition(FCallTipState.ClassOrRecordMember, FCallTipState.CurrentCallTipWord, FCallTipState.CurrentCallTip, FCallTipState.MaxCallTips);
  if ((FCallTipState.MaxCallTips = 1) and FunctionDefinition.HasParams) or //if there's a single definition then only show if it has a parameter
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

  if (FMemosStyler.GetSectionFromLineState(AMemo.Lines.State[AMemo.GetLineFromPosition(Pos)]) <> scCode) or
     ((Key <> #0) and not _InitiateAutoCompleteOrCallTipAllowedAtPos(AMemo,
       AMemo.GetPositionFromLine(AMemo.GetLineFromPosition(Pos)),
       AMemo.GetPositionBefore(Pos))) then
    Exit;

  { Based on SciTE 5.50's SciTEBase::StartAutoComplete }

  FCallTipState.CurrentCallTip := 0;
  FCallTipState.CurrentCallTipWord := '';
  var Line := AMemo.CaretLineText;
  var Current := AMemo.CaretPositionInLine;
  const CallTipWordCharacters = AMemo.WordCharsAsSet;

  {$ZEROBASEDSTRINGS ON}
  repeat
    var Braces := 0;
		while ((Current > 0) and ((Braces <> 0) or not (Line[Current-1] = '('))) do begin
			if Line[Current-1] = '(' then
			  Dec(Braces)
			else if Line[Current-1] = ')' then
				Inc(Braces);
			Dec(Current);
			Dec(Pos);
    end;
    if Current > 0 then begin
      Dec(Current);
      Dec(Pos);
    end else
      Break;
    while (Current > 0) and (Line[Current-1] <= ' ') do begin
      Dec(Current);
      Dec(Pos);
    end
  until not ((Current > 0) and not CharInSet(Line[Current-1], CallTipWordCharacters));
  {$ZEROBASEDSTRINGS OFF}
  if Current <= 0 then
    Exit;

	FCallTipState.StartCallTipWord := Current - 1;
  {$ZEROBASEDSTRINGS ON}
	while (FCallTipState.StartCallTipWord > 0) and CharInSet(Line[FCallTipState.StartCallTipWord-1], CallTipWordCharacters) do
    Dec(FCallTipState.StartCallTipWord);
  FCallTipState.ClassOrRecordMember := (FCallTipState.StartCallTipWord > 0) and (Line[FCallTipState.StartCallTipWord-1] = '.');
  {$ZEROBASEDSTRINGS OFF}

  SetLength(Line, Current);
  FCallTipState.CurrentCallTipWord := Line.Substring(FCallTipState.StartCallTipWord); { Substring is zero-based }

  FCallTipState.FunctionDefinition := '';
  _UpdateCallTipFunctionDefinition(AMemo, Pos);
end;

procedure TMainFormAutoCompleteAndCallTipsHelper._ContinueCallTip(const AMemo: TScintEdit);
begin
  { Based on SciTE 5.50's SciTEBase::ContinueCallTip }

	const Line = AMemo.CaretLineText;
	const Current = AMemo.CaretPositionInLine;

	var Braces := 0;
	var Commas := 0;
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

procedure TMainFormAutoCompleteAndCallTipsHelper.AutoCompleteAndCallTipsHandleCharAdded(
  const AMemo: TScintEdit; const Ch: AnsiChar);
begin
  { Based on SciTE 5.50's SciTEBase::CharAdded but with an altered interaction
    between calltips and autocomplete }

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
    case Ch of
      'A'..'Z', 'a'..'z', '_', '#', '{', '[', '<', '0'..'9':
        if not AMemo.AutoCompleteActive and FOptions.AutoAutoComplete and not (Ch in ['0'..'9']) then
          InitiateAutoComplete(AMemo, Ch);
    else
      const RestartAutoComplete = (Ch in [' ', '.', '=']) and
        (FOptions.AutoAutoComplete or AMemo.AutoCompleteActive);
      AMemo.CancelAutoComplete;
      if RestartAutoComplete then
        InitiateAutoComplete(AMemo, Ch);
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
