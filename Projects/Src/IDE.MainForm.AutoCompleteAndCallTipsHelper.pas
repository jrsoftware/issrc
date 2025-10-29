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
  IDE.MainForm, IDE.MainForm.ToolsHelper,
  IDE.IDEScintEdit;

type
  TMainFormAutoCompleteAndCallTipsHelper = class helper(TMainFormToolsHelper) for TMainForm
    procedure InitiateAutoComplete(const Key: AnsiChar);
    procedure AutoCompleteAndCallTipsHandleCharAdded(const Ch: AnsiChar);
    procedure CallTipsHandleArrowClick(const Up: Boolean);
    procedure CallTipsHandleCtrlSpace;
    { Private }
    function _InitiateAutoCompleteOrCallTipAllowedAtPos(const AMemo: TIDEScintEdit;
      const WordStartLinePos, PositionBeforeWordStartPos: Integer): Boolean;
    procedure _UpdateCallTipFunctionDefinition(const Pos: Integer = -1);
    procedure _InitiateCallTip(const Key: AnsiChar);
    procedure _ContinueCallTip;
  end;

implementation

uses
  SysUtils, Math,
  IDE.ScintStylerInnoSetup;

function TMainFormAutoCompleteAndCallTipsHelper._InitiateAutoCompleteOrCallTipAllowedAtPos(const AMemo: TIDEScintEdit;
  const WordStartLinePos, PositionBeforeWordStartPos: Integer): Boolean;
begin
  Result := (PositionBeforeWordStartPos < WordStartLinePos) or
            not FMemosStyler.IsCommentOrPascalStringStyle(AMemo.GetStyleAtPosition(PositionBeforeWordStartPos));
end;

procedure TMainFormAutoCompleteAndCallTipsHelper.InitiateAutoComplete(const Key: AnsiChar);

  function OnlyWhiteSpaceBeforeWord(const Memo: TIDEScintEdit; const LinePos, WordStartPos: Integer): Boolean;
  var
    I: Integer;
    C: AnsiChar;
  begin
    { Only allow autocompletion if no non-whitespace characters exist before the current word on the line }
    I := WordStartPos;
    Result := False;
    while I > LinePos do begin
      I := FActiveMemo.GetPositionBefore(I);
      if I < LinePos then
        Exit;  { shouldn't get here }
      C := FActiveMemo.GetByteAtPosition(I);
      if C > ' ' then
        Exit;
    end;
    Result := True;
  end;

var
  CaretPos, Line, LinePos, WordStartPos, WordEndPos, CharsBefore,
    LangNamePos: Integer;
  Section: TInnoSetupStylerSection;
  IsParamSection: Boolean;
  WordList: AnsiString;
  FoundSemicolon, FoundFlagsOrType, FoundDot: Boolean;
  C: AnsiChar;
begin
  if FActiveMemo.AutoCompleteActive or FActiveMemo.ReadOnly then
    Exit;

  if Key = #0 then begin
    { If a character is typed then Scintilla will handle selections but
      otherwise we should empty them and also make sure the caret is visible
      before we start autocompletion }
    FActiveMemo.SetEmptySelections;
    FActiveMemo.ScrollCaretIntoView;
  end;

  CaretPos := FActiveMemo.CaretPosition;
  Line := FActiveMemo.GetLineFromPosition(CaretPos);
  LinePos := FActiveMemo.GetPositionFromLine(Line);

  WordStartPos := FActiveMemo.GetWordStartPosition(CaretPos, True);
  WordEndPos := FActiveMemo.GetWordEndPosition(CaretPos, True);
  CharsBefore := CaretPos - WordStartPos;

  { Don't auto start autocompletion after a character is typed if there are any
    word characters adjacent to the character }
  if Key <> #0 then begin
    if CharsBefore > 1 then
      Exit;
    if WordEndPos > CaretPos then
      Exit;
  end;

  case FActiveMemo.GetByteAtPosition(WordStartPos) of
    '#':
      begin
        if not OnlyWhiteSpaceBeforeWord(FActiveMemo, LinePos, WordStartPos) then
          Exit;
        WordList := FMemosStyler.ISPPDirectivesWordList;
        FActiveMemo.SetAutoCompleteFillupChars(' ');
      end;
    '{':
      begin
        WordList := FMemosStyler.ConstantsWordList;
        FActiveMemo.SetAutoCompleteFillupChars('\:');
      end;
    '[':
      begin
        if not OnlyWhiteSpaceBeforeWord(FActiveMemo, LinePos, WordStartPos) then
          Exit;
        WordList := FMemosStyler.SectionsWordList;
        FActiveMemo.SetAutoCompleteFillupChars('');
      end;
    else
      begin
        Section := FMemosStyler.GetSectionFromLineState(FActiveMemo.Lines.State[Line]);
        if Section = scCode then begin
          { Space can only initiate autocompletion after non whitespace }
          if (Key = ' ') and OnlyWhiteSpaceBeforeWord(FActiveMemo, LinePos, WordStartPos) then
            Exit;

          var PositionBeforeWordStartPos := FActiveMemo.GetPositionBefore(WordStartPos);
          if Key <> #0 then begin
            FActiveMemo.StyleNeeded(PositionBeforeWordStartPos); { Make sure the typed character has been styled }
            if not _InitiateAutoCompleteOrCallTipAllowedAtPos(FActiveMemo, LinePos, PositionBeforeWordStartPos) then
              Exit;
          end;

          WordList := '';

          { Autocomplete event functions if the current word on the line has
            exactly 1 space before it which has the word 'function' or
            'procedure' before it which has only whitespace before it }
          if (PositionBeforeWordStartPos >= LinePos) and (FActiveMemo.GetByteAtPosition(PositionBeforeWordStartPos) <= ' ') then begin
            var FunctionWordEndPos := PositionBeforeWordStartPos;
            var FunctionWordStartPos := FActiveMemo.GetWordStartPosition(FunctionWordEndPos, True);
            if OnlyWhiteSpaceBeforeWord(FActiveMemo, LinePos, FunctionWordStartPos) then begin
              var FunctionWord := FActiveMemo.GetTextRange(FunctionWordStartPos, FunctionWordEndPos);
              if SameText(FunctionWord, 'procedure') then
                WordList := FMemosStyler.EventFunctionsWordList[True]
              else if SameText(FunctionWord, 'function') then
                WordList := FMemosStyler.EventFunctionsWordList[False];
              if WordList <> '' then
                FActiveMemo.SetAutoCompleteFillupChars('');
            end;
          end;

          { If no event function was found then autocomplete script functions,
            types, etc if the current word has no dot before it }
          if WordList = '' then begin
            var ClassOrRecordMember := (PositionBeforeWordStartPos >= LinePos) and (FActiveMemo.GetByteAtPosition(PositionBeforeWordStartPos) = '.');
            WordList := FMemosStyler.ScriptWordList[ClassOrRecordMember];
            FActiveMemo.SetAutoCompleteFillupChars('');
          end;

          if WordList = '' then
            Exit;
        end else begin
          IsParamSection := FMemosStyler.IsParamSection(Section);

          { Autocomplete if the current word on the line has only whitespace
            before it, or else also: after the last ';' or after 'Flags:' or
            'Type:' in parameterized sections }
          FoundSemicolon := False;
          FoundFlagsOrType := False;
          FoundDot := False;
          var I := WordStartPos;
          while I > LinePos do begin
            I := FActiveMemo.GetPositionBefore(I);
            if I < LinePos then
              Exit;  { shouldn't get here }
            C := FActiveMemo.GetByteAtPosition(I);

            if IsParamSection and (C in [';', ':']) and
               FMemosStyler.IsSymbolStyle(FActiveMemo.GetStyleAtPosition(I)) then begin { Make sure it's an stSymbol ';' or ':' and not one inside a quoted string }
              FoundSemicolon := C = ';';
              if not FoundSemicolon then begin
                var ParameterWordEndPos := I;
                var ParameterWordStartPos := FActiveMemo.GetWordStartPosition(ParameterWordEndPos, True);
                var ParameterWord := FActiveMemo.GetTextRange(ParameterWordStartPos, ParameterWordEndPos);
                FoundFlagsOrType := SameText(ParameterWord, 'Flags') or
                                    ((Section in [scInstallDelete, scUninstallDelete]) and SameText(ParameterWord, 'Type'));
              end else
                FoundFlagsOrType := False;
              if FoundSemicolon or FoundFlagsOrType then
                Break;
            end;
            if (Section = scLangOptions) and (C = '.') and not FoundDot then begin
              { Verify that a word (language name) precedes the '.', then check for
                any non-whitespace characters before the word }
              LangNamePos := FActiveMemo.GetWordStartPosition(I, True);
              if LangNamePos >= I then
                Exit;
              I := LangNamePos;
              FoundDot := True;
            end else if C > ' ' then begin
              if IsParamSection and not (Section in [scInstallDelete, scUninstallDelete]) and
                 (FMemosStyler.FlagsWordList[Section] <> '') then begin
                { Verify word before the current word (or before that when we get here again) is
                  a valid flag and if so, continue looking before it instead of stopping }
                var FlagEndPos := FActiveMemo.GetWordEndPosition(I, True);
                var FlagStartPos := FActiveMemo.GetWordStartPosition(I, True);
                var FlagWord := FActiveMemo.GetTextRange(FlagStartPos, FlagEndPos);
                if FMemosStyler.SectionHasFlag(Section, FlagWord) then
                  I := FlagStartPos
                else
                  Exit;
              end else
                Exit;
            end;
          end;
          { Space can only initiate autocompletion after ';' or 'Flags:' or 'Type:' in parameterized sections }
          if (Key = ' ') and not (FoundSemicolon or FoundFlagsOrType) then
            Exit;

          if FoundFlagsOrType then begin
            WordList := FMemosStyler.FlagsWordList[Section];
            if WordList = '' then
              Exit;
            FActiveMemo.SetAutoCompleteFillupChars(' ');
          end else begin
            WordList := FMemosStyler.KeywordsWordList[Section];
            if WordList = '' then { CustomMessages }
              Exit;
            if IsParamSection then
              FActiveMemo.SetAutoCompleteFillupChars(':')
            else
              FActiveMemo.SetAutoCompleteFillupChars('=');
          end;
        end;
      end;
  end;
  FActiveMemo.ShowAutoComplete(CharsBefore, WordList);
end;

procedure TMainFormAutoCompleteAndCallTipsHelper._UpdateCallTipFunctionDefinition(const Pos: Integer { = -1 });
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

    FActiveMemo.ShowCallTip(FCallTipState.LastPosCallTip - Length(FCallTipState.CurrentCallTipWord), FCallTipState.FunctionDefinition);
    _ContinueCallTip;
  end;
end;

procedure TMainFormAutoCompleteAndCallTipsHelper._InitiateCallTip(const Key: AnsiChar);
begin
  var Pos := FActiveMemo.CaretPosition;

  if (FMemosStyler.GetSectionFromLineState(FActiveMemo.Lines.State[FActiveMemo.GetLineFromPosition(Pos)]) <> scCode) or
     ((Key <> #0) and not _InitiateAutoCompleteOrCallTipAllowedAtPos(FActiveMemo,
       FActiveMemo.GetPositionFromLine(FActiveMemo.GetLineFromPosition(Pos)),
       FActiveMemo.GetPositionBefore(Pos))) then
    Exit;

  { Based on SciTE 5.50's SciTEBase::StartAutoComplete }

  FCallTipState.CurrentCallTip := 0;
  FCallTipState.CurrentCallTipWord := '';
  var Line := FActiveMemo.CaretLineText;
  var Current := FActiveMemo.CaretPositionInLine;
  var CallTipWordCharacters := FActiveMemo.WordCharsAsSet;

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
  _UpdateCallTipFunctionDefinition(Pos);
end;

procedure TMainFormAutoCompleteAndCallTipsHelper._ContinueCallTip;
begin
  { Based on SciTE 5.50's SciTEBase::ContinueCallTip }

	var Line := FActiveMemo.CaretLineText;
	var Current := FActiveMemo.CaretPositionInLine;

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
  var FunctionDefinition := FCallTipState.FunctionDefinition;
  var FunctionDefinitionLength := Length(FunctionDefinition);
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

	FActiveMemo.SetCallTipHighlight(StartHighlight, EndHighlight);
end;

procedure TMainFormAutoCompleteAndCallTipsHelper.AutoCompleteAndCallTipsHandleCharAdded(
  const Ch: AnsiChar);
begin
  { Based on SciTE 5.50's SciTEBase::CharAdded but with an altered interaction
    between calltips and autocomplete }

  var DoAutoComplete := False;

  if FActiveMemo.CallTipActive then begin
    if Ch = ')' then begin
      Dec(FCallTipState.BraceCount);
      if FCallTipState.BraceCount < 1 then
        FActiveMemo.CancelCallTip
      else if FOptions.AutoCallTips then
        _InitiateCallTip(Ch);
    end else if Ch = '(' then begin
      Inc(FCallTipState.BraceCount);
      if FOptions.AutoCallTips then
        _InitiateCallTip(Ch);
    end else
      _ContinueCallTip;
  end else if FActiveMemo.AutoCompleteActive then begin
    if Ch = '(' then begin
      Inc(FCallTipState.BraceCount);
      if FOptions.AutoCallTips then begin
        _InitiateCallTip(Ch);
        if not FActiveMemo.CallTipActive then begin
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
      _InitiateCallTip(Ch);
  end else
    DoAutoComplete := True;

  if DoAutoComplete then begin
    case Ch of
      'A'..'Z', 'a'..'z', '_', '#', '{', '[', '<', '0'..'9':
        if not FActiveMemo.AutoCompleteActive and FOptions.AutoAutoComplete and not (Ch in ['0'..'9']) then
          InitiateAutoComplete(Ch);
    else
      var RestartAutoComplete := (Ch in [' ', '.']) and
        (FOptions.AutoAutoComplete or FActiveMemo.AutoCompleteActive);
      FActiveMemo.CancelAutoComplete;
      if RestartAutoComplete then
        InitiateAutoComplete(Ch);
    end;
  end;
end;

procedure TMainFormAutoCompleteAndCallTipsHelper.CallTipsHandleArrowClick(const Up: Boolean);
begin
  { Based on SciTE 5.50's SciTEBase::Notify SA::Notification::CallTipClick }
  if Up and (FCallTipState.CurrentCallTip > 0) then begin
    Dec(FCallTipState.CurrentCallTip);
    _UpdateCallTipFunctionDefinition;
  end else if not Up and (FCallTipState.CurrentCallTip + 1 < FCallTipState.MaxCallTips) then begin
    Inc(FCallTipState.CurrentCallTip);
    _UpdateCallTipFunctionDefinition;
  end;
end;

procedure TMainFormAutoCompleteAndCallTipsHelper.CallTipsHandleCtrlSpace;
begin
  { Based on SciTE 5.50's SciTEBase::MenuCommand IDM_SHOWCALLTIP }
  if FActiveMemo.CallTipActive then begin
    FCallTipState.CurrentCallTip := IfThen(FCallTipState.CurrentCallTip + 1 = FCallTipState.MaxCallTips, 0, FCallTipState.CurrentCallTip + 1);
    _UpdateCallTipFunctionDefinition;
  end else begin
    FCallTipState.BraceCount := 1; { Missing in SciTE, see https://sourceforge.net/p/scintilla/bugs/2446/ }
    _InitiateCallTip(#0);
  end;
end;

end.
