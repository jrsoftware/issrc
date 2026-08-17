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
  Shared.Struct, IDE.ScriptModel.Metadata.Extra,
  IDE.ScriptModel.Metadata.Extra.WordLists;

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

  TInnoSetupStyler = class(TScintCustomStyler)
  private
    FISPPInstalled: Boolean;
    FTheme: TTheme;
    procedure ApplyPendingSquigglyFromToIndex(const StartIndex, EndIndex: Integer);
    procedure ApplyPendingSquigglyFromIndex(const StartIndex: Integer);
    procedure ApplySquigglyFromIndex(const StartIndex: Integer);
    procedure CommitStyleSq(const Style: TInnoSetupStylerStyle;
      const Squigglify: Boolean);
    procedure CommitStyleSqPending(const Style: TInnoSetupStylerStyle);
    procedure HandleCodeSection(var SpanState: TInnoSetupStylerSpanState; var CodeBlockHeader: Boolean);
    procedure HandleKeyValueSection(const Section: TInnoSetupSection);
    procedure HandleParameterSection(const ValidParameterNames: array of AnsiString);
    procedure HandleCompilerDirective(const InlineDirective: Boolean;
      const InlineDirectiveEndIndex: Integer; var OpenCount: ShortInt);
    procedure PreStyleInlineISPPDirectives;
    procedure SkipWhitespace;
    procedure SquigglifyUntilChars(const Chars: TScintRawCharSet;
      const Style: TInnoSetupStylerStyle);
    procedure StyleConstsUntilChars(const Chars: TScintRawCharSet;
      const NonConstStyle: TInnoSetupStylerStyle; var BraceLevel: Integer);
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
    class function IsSymbolStyle(const Style: TScintStyleNumber): Boolean; static;
    class function LineSectionHeader(const LineState: TScintLineState; out Section: TInnoSetupSection): Boolean; static;
    class function LineSpans(const S: TScintRawString): Boolean; static;
    property ISPPInstalled: Boolean read FISPPInstalled write FISPPInstalled;
    property Theme: TTheme read FTheme write FTheme;
  end;

implementation

uses
  Generics.Defaults,
  Shared.SetupMessageIDs, ScintInt, Shared.LangOptionsSectionDirectives,
  IDE.ScriptModel.Metadata, IDE.ScriptModel.Metadata.Extra.FunctionDefinitions,
  isxclasses_wordlists_generated;

type
  { Size must be <= SizeOf(TScintLineState) }
  TInnoSetupStylerLineState = record
    Section, NextLineSection: TInnoSetupSection;
    SpanState: TInnoSetupStylerSpanState;
    OpenCompilerDirectivesCount: ShortInt;
  end;

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

{ TInnoSetupStyler }

constructor TInnoSetupStyler.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TInnoSetupStyler.Destroy;
begin
  inherited;
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

class function TInnoSetupStyler.GetSectionFromLineState(
  const LineState: TScintLineState; const ReturnCodeBlockAsCode: Boolean = True): TInnoSetupSection;
begin
  Result := TInnoSetupStylerLineState(LineState).Section;
  if ReturnCodeBlockAsCode and (Result = scCodeBlock) then
    Result := scCode;
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
  const ValidParameterNames: array of AnsiString);
const
  MaxParameters = 32;
var
  ParamsSpecified: set of 0..MaxParameters-1;
  S: TScintRawString;
  ParamValueIndex, BraceLevel: Integer;
  NamePresent, ValidName, DuplicateName, ColonPresent: Boolean;
begin
  if Length(ValidParameterNames) > MaxParameters then
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
    for var I := Low(ValidParameterNames) to High(ValidParameterNames) do
      if SameRawText(S, TScintRawString(ValidParameterNames[I])) then begin
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
        if SameRawText(S, TScintRawString(Section.Name)) then begin
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
  end else if Section in KeyValueSections then
    HandleKeyValueSection(Section)
  else if Section in ParameterSections then
    HandleParameterSection(ParameterNames[Section]);

  NewLineState.Section := Section;
  LineState := TScintLineState(NewLineState);
end;

end.
