unit IDE.ScriptModel;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Script model which can parse and store a single entry of a parameter
  section.
  
  Uses the InnoIDE storage technique:
  - Parameter sections: a section entry is an ordered list of
    named parameters where everything parsed (known or unknown) is
    preserved and edits touch only what changed. Parsing has no error
    path and creates a structure which can always be serialized back
    into the parsed lines, byte-identical. Editing keeps line spanning.
  - Directive sections: a section occurrence is an ordered list of
    its logical lines, so after joining physical lines for line
    spanning. Editing does not keep line spanning. Does keep whitespace
    and quotes.

  Supports an OnChange event to get notified of changes.
}

interface

uses
  SysUtils, Classes, Generics.Collections,
  IDE.ScriptModel.Metadata;

type
  EScriptModelError = class(Exception);

  TScriptLineKind = (slkBlank, slkComment, slkISPPDirective, slkActual);

  TScriptEntryParameterKind = (sepParameter, sepOther);

  { A single parameter of an entry in a parameter section: either a Name: Value
    or another kind of chunk of text between ';' separators }
  TScriptEntryParameter = class
  private
    FRawText: String; { The original text }
    FName: String;    { May be empty }
    FValueStartIndex: Integer; { Index in FRawText of the first character after the ':', or 0 }
    procedure SetRawText(const ARawText: String);
    function GetKind: TScriptEntryParameterKind;
    function GetRawValue: String;
    function GetValue: String;
  public
    property Kind: TScriptEntryParameterKind read GetKind;
    property Name: String read FName;
    property RawText: String read FRawText;
    property RawValue: String read GetRawValue;
    property Value: String read GetValue;
  end;

  { A remembered ISPP line-spanning break }
  TScriptEntryBreak = record
    ParameterIndex: Integer; { Parameter the physical line started with }
    Indent: String;          { Leading whitespace of the physical line as written }
  end;

  { An entry of a parameter section }
  TScriptParameterEntry = class
  private
    FMetadata: TScriptSectionMetadata; { May be nil }
    FParameters: TObjectList<TScriptEntryParameter>;
    FBreaks: TList<TScriptEntryBreak>; { Line spanning }
    FIndent: String; { First line indent }
    FOriginalLines: TArray<String>; { Before modification }
    FModified: Boolean;
    FOnChange: TNotifyEvent;
    FUpdateLevel: Integer;
    FPendingChange: Boolean;
    FQuoteNewValues: Boolean;
    procedure ApplyFlagRules(const AParameterName, AIncludedFlagName: String);
    procedure ApplyParameterFlagRules(const AParameterName, AValue: String);
    function AppendParameterInternal(const AName, AValue: String;
      const AQuoteNewValue: Boolean): Integer;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetBreakParameterIndex(Index: Integer): Integer;
    function GetNamedParameter(const AIndex: Integer): TScriptEntryParameter;
    function GetParameter(Index: Integer): TScriptEntryParameter;
    procedure MarkModified;
    procedure SetFlagInternal(const AParameterName, AFlagName: String;
      const AInclude: Boolean); overload;
    procedure SetFlagInternal(const AIndex: Integer; const AFlagName: String;
      const AInclude: Boolean); overload;
    procedure SetValueInternal(const AIndex: Integer; const AValue: String);
  public
    constructor Create(const AMetadata: TScriptSectionMetadata);
    destructor Destroy; override;
    procedure Parse(const ALines: array of String);
    function GetLines: TArray<String>;
    function Count: Integer;
    function IndexOf(const AName: String): Integer;
    function Has(const AName: String): Boolean;
    function TryResolve(const AName: String;
      var AIndex: Integer): Boolean;
    function TryGetValue(const AName: String; out AValue: String): Boolean;
    procedure SetValue(const AIndex: Integer; const AValue: String);
    function Add(const AName, AValue: String): Integer;
    procedure Remove(const AIndex: Integer);
    function FlagIncluded(const AIndex: Integer; const AFlagName: String): Boolean;
    procedure SetFlag(const AIndex: Integer; const AFlagName: String;
      const AInclude: Boolean);
    function TryGetDefinition(const AName: String;
      out ADefinition: TScriptParameterDefinition): Boolean;
    function BreakCount: Integer;
    property BreakParameterIndexes[Index: Integer]: Integer read GetBreakParameterIndex;
    property Indent: String read FIndent;
    property Metadata: TScriptSectionMetadata read FMetadata;
    property Modified: Boolean read FModified;
    property Parameters[Index: Integer]: TScriptEntryParameter read GetParameter;
    property QuoteNewValues: Boolean read FQuoteNewValues write FQuoteNewValues;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TScriptDirectiveLineKind = (sdlDirective, sdlOther);

  { A single logical line in a directive section: either a Name=Value or
    another kind of line (comment, ISPP directive, blank, or anything else) }
  TScriptDirectiveSectionLine = class
  private
    FKind: TScriptDirectiveLineKind;
    FOriginalLines: TArray<String>; { The original lines }
    FNameText: String;              { Original name }
    FName: String;                  { Trimmed name }
    FRawValue: String;              { Original value }
    FModified: Boolean;
    function GetValue: String;
  public
    property Kind: TScriptDirectiveLineKind read FKind;
    property Name: String read FName;
    property RawValue: String read FRawValue;
    property Value: String read GetValue;
  end;

  { A single occurrence of a directive-style section }
  TScriptDirectiveSection = class
  private
    FMetadata: TScriptSectionMetadata; { May be nil }
    FLines: TObjectList<TScriptDirectiveSectionLine>;
    FOnChange: TNotifyEvent;
    FQuoteNewValues: Boolean;
    procedure Changed;
    function GetNamedLine(const AIndex: Integer): TScriptDirectiveSectionLine;
    function GetLine(Index: Integer): TScriptDirectiveSectionLine;
  public
    constructor Create(const AMetadata: TScriptSectionMetadata);
    destructor Destroy; override;
    procedure Parse(const ALines: array of String);
    function GetLines: TArray<String>;
    function Count: Integer;
    function IndexOf(const AName: String): Integer;
    function TryResolve(const AName: String;
      var AIndex: Integer): Boolean;
    function TryGetValue(const AName: String; out AValue: String): Boolean;
    procedure SetValue(const AIndex: Integer; const AValue: String);
    function Add(const AName, AValue: String): Integer;
    procedure Remove(const AIndex: Integer);
    function TryGetDefinition(const AName: String;
      out ADefinition: TScriptParameterDefinition): Boolean;
    function DefaultValue(const AName: String): String;
    property Lines[Index: Integer]: TScriptDirectiveSectionLine read GetLine;
    property Metadata: TScriptSectionMetadata read FMetadata;
    property QuoteNewValues: Boolean read FQuoteNewValues write FQuoteNewValues;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

function ScriptLineSpans(const S: String): Boolean;
function ClassifyScriptLine(const S: String): TScriptLineKind;
function JoinSpannedScriptLines(const ALines: array of String): String;
function UnquoteScriptParameterValue(const S: String): String;
function QuoteScriptParameterValueIfNeeded(const S: String;
  const AAlwaysQuote: Boolean = False): String;
function UnquoteScriptDirectiveValue(const S: String): String;
function TryParseScriptDirectiveLine(const S: String;
  out ANameText, ARawValue: String): Boolean;
function ContainsLineBreak(const S: String): Boolean;

implementation

{ Line helpers }

function ScriptLineSpans(const S: String): Boolean;
begin
  { Matches TInnoSetupStyler.LineTextSpans and ISPP's
    TPreprocessor.InternalQueueLine. Like the styler, this assumes the default
    span symbol '\', which ISPP lets scripts change }
  const L = Length(S);
  Result := (L > 2) and (S[L] = '\') and (S[L-1] <= ' ');
end;

{ True if a line ending in S would be read back as an ISPP line span by
  TPreprocessor.InternalQueueLine: S ends in '\' preceded by whitespace, or is
  just '\' (the whitespace then comes from the separator written before the
  value) }
function ScriptValueEndsInContinuation(const S: String): Boolean;
begin
  const L = Length(S);
  Result := (L > 0) and (S[L] = '\') and ((L = 1) or (S[L-1] <= ' '));
end;

function ScriptValueIsQuoted(const S: String): Boolean;
begin
  const Trimmed = Trim(S);
  Result := (Length(Trimmed) >= 2) and (Trimmed[1] = '"') and
    (Trimmed[Length(Trimmed)] = '"');
end;

function ClassifyScriptLine(const S: String): TScriptLineKind;
begin
  { Matches TInnoSetupStyler.StyleNeeded's order of checks at the start of a line }
  const T = TrimLeft(S);
  if T = '' then
    Result := slkBlank
  else if T[1] = ';' then
    Result := slkComment
  else if (Length(T) >= 2) and (T[1] = '/') and (T[2] = '/') then
    Result := slkComment
  else if T[1] = '#' then
    Result := slkISPPDirective
  else
    Result := slkActual;
end;

function JoinSpannedScriptLines(const ALines: array of String): String;
begin
  if Length(ALines) = 1 then
    Exit(ALines[0]);
  { Matches ISPP's TPreprocessor.InternalQueueLine }
  Result := '';
  for var I := 0 to High(ALines) do begin
    var S := ALines[I];
    if (I < High(ALines)) and ScriptLineSpans(S) then
      SetLength(S, Length(S)-1);
    Result := Result + TrimLeft(S);
  end;
end;

function UnquoteScriptParameterValue(const S: String): String;
begin
  Result := Trim(S);
  if (Length(Result) >= 2) and (Result[1] = '"') and
     (Result[Length(Result)] = '"') then begin
    Result := Copy(Result, 2, Length(Result)-2);
    Result := StringReplace(Result, '""', '"', [rfReplaceAll]);
  end;
end;

function QuoteScriptParameterValueIfNeeded(const S: String;
  const AAlwaysQuote: Boolean): String;

  function NeedsQuoting: Boolean;
  begin
    if S = '' then
      Exit(False);
    { Leading or trailing whitespace would be lost without quotes }
    if (S[1] <= ' ') or (S[Length(S)] <= ' ') then
      Exit(True);
    { ';' would end the parameter, and '"' inside an unquoted value is invalid }
    for var I := 1 to Length(S) do
      if CharInSet(S[I], [';', '"']) then
        Exit(True);
    { It may cause (unwanted) line spanning if not quoted }
    if ScriptValueEndsInContinuation(S) then
      Exit(True);
    Result := False;
  end;

begin
  if AAlwaysQuote or NeedsQuoting then
    Result := '"' + StringReplace(S, '"', '""', [rfReplaceAll]) + '"'
  else
    Result := S;
end;

function QuoteScriptDirectiveValueIfNeeded(const AValue: String;
  const AAlwaysQuote: Boolean = False): String;
begin
  { Directive values only need quotes to keep leading or trailing whitespace,
    to keep a value that itself looks quoted from losing those quotes on
    read-back, or to keep a trailing '\' from being read back as a line
    continuation }
  if AAlwaysQuote or (AValue <> Trim(AValue)) or
     ScriptValueIsQuoted(AValue) or
     ScriptValueEndsInContinuation(AValue) then
    Result := '"' + AValue + '"'
  else
    Result := AValue;
end;

function ShouldQuoteNewValue(const AQuoteNewValues: Boolean;
  const AMetadata: TScriptSectionMetadata; const AName: String): Boolean;
begin
  if not AQuoteNewValues then
    Exit(False);
  var Definition: TScriptParameterDefinition;
  if (AMetadata <> nil) and AMetadata.TryGetParameter(AName, Definition) then
    Result := Definition.ValueKind in [pvkString, pvkChoice]
  else
    Result := True;
end;

function UnquoteScriptDirectiveValue(const S: String): String;
begin
  Result := Trim(S);
  { If the value is surrounded in quotes, remove them, just like
    TSetupCompiler.SeparateDirective. Unlike parameter values, embedded
    quotes are not doubled so there is nothing else to do }
  if ScriptValueIsQuoted(Result) then
    Result := Copy(Result, 2, Length(Result)-2);
end;

function TryParseScriptDirectiveLine(const S: String;
  out ANameText, ARawValue: String): Boolean;
begin
  const P = Pos('=', S);
  Result := (P > 0) and (Trim(Copy(S, 1, P-1)) <> '');
  if Result then begin
    ANameText := Copy(S, 1, P-1);
    ARawValue := Copy(S, P+1, MaxInt);
  end;
end;

function LeadingWhitespace(const S: String): String;
begin
  var I := 1;
  while (I <= Length(S)) and (S[I] <= ' ') do
    Inc(I);
  Result := Copy(S, 1, I-1);
end;

function TrailingWhitespace(const S: String): String;
begin
  var I := Length(S);
  while (I >= 1) and (S[I] <= ' ') do
    Dec(I);
  Result := Copy(S, I+1, MaxInt);
end;

function ContainsLineBreak(const S: String): Boolean;
begin
  for var I := 1 to Length(S) do
    if CharInSet(S[I], [#13, #10]) then
      Exit(True);
  Result := False;
end;

{ Finds a whole token in a delimited value such as Flags, case-insensitively.
  Tokens are delimited by literal spaces and trimmed of remaining whitespace,
  matching the compiler's ExtractFlag }
function FindScriptFlagToken(const AValue, AFlagName: String;
  out AStartIndex, ALength: Integer): Boolean;
begin
  const L = Length(AValue);
  var I := 1;
  while I <= L do begin
    while (I <= L) and (AValue[I] = ' ') do
      Inc(I);
    const ChunkStart = I;
    while (I <= L) and (AValue[I] <> ' ') do
      Inc(I);
    var TokenStart := ChunkStart;
    while (TokenStart < I) and (AValue[TokenStart] <= ' ') do
      Inc(TokenStart);
    var TokenEnd := I; { Exclusive }
    while (TokenEnd > TokenStart) and (AValue[TokenEnd-1] <= ' ') do
      Dec(TokenEnd);
    if (TokenEnd > TokenStart) and
       SameText(Copy(AValue, TokenStart, TokenEnd-TokenStart), AFlagName) then begin
      AStartIndex := TokenStart;
      ALength := TokenEnd-TokenStart;
      Exit(True);
    end;
  end;
  Result := False;
end;

{ TScriptEntryParameter }

procedure TScriptEntryParameter.SetRawText(const ARawText: String);
begin
  FRawText := ARawText;
  FName := '';
  FValueStartIndex := 0;
  { Parse any 'Name: Value' shape: optional whitespace, a name of letters and
    digits (matching TInnoSetupStyler.HandleParameterSection), optional
    whitespace, then ':'. Also see IsValidScriptParameterName. }
  const L = Length(FRawText);
  var I := 1;
  while (I <= L) and (FRawText[I] <= ' ') do
    Inc(I);
  const NameStart = I;
  while (I <= L) and CharInSet(FRawText[I], ['A'..'Z', 'a'..'z', '0'..'9']) do
    Inc(I);
  const NameEnd = I-1;
  while (I <= L) and (FRawText[I] <= ' ') do
    Inc(I);
  if (NameEnd >= NameStart) and (I <= L) and (FRawText[I] = ':') then begin
    FName := Copy(FRawText, NameStart, NameEnd-NameStart+1);
    FValueStartIndex := I+1;
  end;
end;

function TScriptEntryParameter.GetKind: TScriptEntryParameterKind;
begin
  if FName <> '' then
    Result := sepParameter
  else
    Result := sepOther;
end;

function TScriptEntryParameter.GetRawValue: String;
begin
  if FValueStartIndex > 0 then
    Result := Copy(FRawText, FValueStartIndex, MaxInt)
  else
    Result := '';
end;

function TScriptEntryParameter.GetValue: String;
begin
  Result := UnquoteScriptParameterValue(GetRawValue);
end;

{ TScriptParameterEntry }

constructor TScriptParameterEntry.Create(const AMetadata: TScriptSectionMetadata);
begin
  inherited Create;
  FMetadata := AMetadata;
  FParameters := TObjectList<TScriptEntryParameter>.Create;
  FBreaks := TList<TScriptEntryBreak>.Create;
  FQuoteNewValues := True;
end;

destructor TScriptParameterEntry.Destroy;
begin
  FBreaks.Free;
  FParameters.Free;
  inherited;
end;

procedure TScriptParameterEntry.Parse(const ALines: array of String);
begin
  FParameters.Clear;
  FBreaks.Clear;
  FIndent := '';
  FModified := False;
  SetLength(FOriginalLines, Length(ALines));
  for var I := 0 to High(ALines) do
    FOriginalLines[I] := ALines[I];
  if Length(ALines) = 0 then
    Exit;

  { Join the physical lines like ISPP's TPreprocessor.InternalQueueLine does.
    The first line's leading whitespace is kept as the entry's indentation.
    Remembers where each line started within the joined line, for later use. }
  var Joined := '';
  var LineStartOffsets: TArray<Integer>;
  SetLength(LineStartOffsets, Length(ALines));
  for var I := 0 to High(ALines) do begin
    var S := ALines[I];
    if (I < High(ALines)) and ScriptLineSpans(S) then
      SetLength(S, Length(S)-1);
    if I = 0 then begin
      FIndent := LeadingWhitespace(S);
      S := Copy(S, Length(FIndent)+1, MaxInt);
    end else
      S := TrimLeft(S);
    LineStartOffsets[I] := Length(Joined)+1;
    Joined := Joined + S;
  end;

  { Split the joined text into chunks at ';', respecting quoted values
    like TInnoSetupStyler.HandleParameterSection (a doubled '""' toggles twice,
    so it needs no special handling) }
  const ChunkStartOffsets = TList<Integer>.Create;
  try
    if Joined <> '' then begin
      var ChunkStart := 1;
      var InQuotes := False;
      var I := 1;
      while True do begin
        if (I > Length(Joined)) or ((Joined[I] = ';') and not InQuotes) then begin
          const Parameter = TScriptEntryParameter.Create;
          Parameter.SetRawText(Copy(Joined, ChunkStart, I-ChunkStart));
          FParameters.Add(Parameter);
          ChunkStartOffsets.Add(ChunkStart);
          if I > Length(Joined) then
            Break;
          ChunkStart := I+1;
        end else if Joined[I] = '"' then
          InQuotes := not InQuotes;
        Inc(I);
      end;
    end;

    { Map each physical line break to the parameter containing its offset;
      a break that fell between parameters belongs to the following one }
    for var I := 1 to High(ALines) do begin
      const Offset = LineStartOffsets[I];
      var ParameterIndex := Integer(FParameters.Count);
      for var K := 0 to Integer(ChunkStartOffsets.Count)-1 do begin
        if Offset <= ChunkStartOffsets[K] + Length(FParameters[K].RawText) - 1 then begin
          ParameterIndex := K;
          Break;
        end;
      end;
      var EntryBreak: TScriptEntryBreak;
      EntryBreak.ParameterIndex := ParameterIndex;
      EntryBreak.Indent := LeadingWhitespace(ALines[I]);
      FBreaks.Add(EntryBreak);
    end;
  finally
    ChunkStartOffsets.Free;
  end;
end;

function TScriptParameterEntry.GetLines: TArray<String>;
begin
  if not FModified then
    Exit(Copy(FOriginalLines));

  { Reassemble the parameters in order and re-create line spanning }
  const LineList = TList<String>.Create;
  try
    var Line := FIndent;
    var LineHasParameters := False;
    var IsFirstLine := True;
    var ParameterIndex := 0;
    for var B := 0 to FBreaks.Count do begin
      var Boundary: Integer;
      if B < FBreaks.Count then
        Boundary := FBreaks[B].ParameterIndex
      else
        Boundary := Integer(FParameters.Count);
      if Boundary < ParameterIndex then
        Boundary := ParameterIndex
      else if Boundary > FParameters.Count then
        Boundary := Integer(FParameters.Count);

      while ParameterIndex < Boundary do begin
        var ChunkText := FParameters[ParameterIndex].RawText;
        if LineHasParameters then
          Line := Line + ';' + ChunkText
        else begin
          { The first parameter of a continuation line lost its leading
            whitespace to the previous line's continuation backslash }
          if not IsFirstLine then
            ChunkText := TrimLeft(ChunkText);
          Line := Line + ChunkText;
        end;
        LineHasParameters := True;
        Inc(ParameterIndex);
      end;

      if B < FBreaks.Count then begin
        { End this line with a continuation: the whitespace before the
          backslash comes from the next parameter's leading whitespace }
        var SuffixWhitespace: String := ' ';
        if Boundary < FParameters.Count then begin
          const NextLeadingWhitespace = LeadingWhitespace(FParameters[Boundary].RawText);
          if NextLeadingWhitespace <> '' then
            SuffixWhitespace := NextLeadingWhitespace;
        end;
        if LineHasParameters then
          Line := Line + ';';
        Line := Line + SuffixWhitespace + '\';
        if Length(Line) <= 2 then
          Line := ' ' + Line; { Too short to be recognized as spanning }
        LineList.Add(Line);
        Line := FBreaks[B].Indent;
        LineHasParameters := False;
        IsFirstLine := False;
      end;
    end;
    LineList.Add(Line);
    Result := LineList.ToArray;
  finally
    LineList.Free;
  end;
end;

function TScriptParameterEntry.Count: Integer;
begin
  Result := Integer(FParameters.Count);
end;

function TScriptParameterEntry.GetParameter(Index: Integer): TScriptEntryParameter;
begin
  Result := FParameters[Index];
end;

function TScriptParameterEntry.GetNamedParameter(
  const AIndex: Integer): TScriptEntryParameter;
begin
  Result := FParameters[AIndex];
  if Result.Kind <> sepParameter then
    raise EScriptModelError.Create('Internal error: Parameter has no name');
end;

function TScriptParameterEntry.BreakCount: Integer;
begin
  Result := Integer(FBreaks.Count);
end;

function TScriptParameterEntry.GetBreakParameterIndex(Index: Integer): Integer;
begin
  Result := FBreaks[Index].ParameterIndex;
end;

function TScriptParameterEntry.IndexOf(const AName: String): Integer;
{ With duplicate parameters the first one wins }
begin
  for var I := 0 to Count-1 do
    if (FParameters[I].Kind = sepParameter) and SameText(FParameters[I].Name, AName) then
      Exit(I);
  Result := -1;
end;

function TScriptParameterEntry.Has(const AName: String): Boolean;
begin
  Result := IndexOf(AName) >= 0;
end;

function TScriptParameterEntry.TryResolve(const AName: String;
  var AIndex: Integer): Boolean;
{ Pass -1 as AIndex to look the parameter up by name }
begin
  if AIndex < 0 then
    AIndex := IndexOf(AName);
  Result := (AIndex >= 0) and (AIndex < Count) and
    (FParameters[AIndex].Kind = sepParameter) and
    SameText(FParameters[AIndex].Name, AName);
end;

function TScriptParameterEntry.TryGetValue(const AName: String;
  out AValue: String): Boolean;
begin
  const I = IndexOf(AName);
  Result := I >= 0;
  if Result then
    AValue := FParameters[I].Value;
end;

procedure TScriptParameterEntry.BeginUpdate;
begin
  Inc(FUpdateLevel);
end;

procedure TScriptParameterEntry.EndUpdate;
begin
  Dec(FUpdateLevel);
  if (FUpdateLevel = 0) and FPendingChange then begin
    FPendingChange := False;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TScriptParameterEntry.MarkModified;
begin
  FModified := True;
  FPendingChange := True;
end;

procedure TScriptParameterEntry.SetValueInternal(const AIndex: Integer;
  const AValue: String);
{ Keeps quotes and surrounding whitespace }
begin
  if ContainsLineBreak(AValue) then
    raise EScriptModelError.Create('Internal error: Value must not contain line breaks');
  const Parameter = GetNamedParameter(AIndex);
  const OldRawValue = Parameter.RawValue;
  const NewValueText = QuoteScriptParameterValueIfNeeded(AValue,
    ScriptValueIsQuoted(OldRawValue));
  const Leading = LeadingWhitespace(OldRawValue);
  const Trailing = TrailingWhitespace(Copy(OldRawValue, Length(Leading)+1, MaxInt));
  Parameter.SetRawText(Copy(Parameter.RawText, 1, Parameter.FValueStartIndex-1) +
    Leading + NewValueText + Trailing);
  MarkModified;
end;

function TScriptParameterEntry.AppendParameterInternal(const AName,
  AValue: String; const AQuoteNewValue: Boolean): Integer;

  { See TScriptEntryParameter.SetRawText }
  function IsValidScriptParameterName(const S: String): Boolean;
  begin
    if S = '' then
      Exit(False);
    for var I := 1 to Length(S) do
      if not CharInSet(S[I], ['A'..'Z', 'a'..'z', '0'..'9']) then
        Exit(False);
    Result := True;
  end;

begin
  { Sanity checks }
  if not IsValidScriptParameterName(AName) then
    raise EScriptModelError.Create('Internal error: Invalid parameter name');
  if ContainsLineBreak(AValue) then
    raise EScriptModelError.Create('Internal error: Value must not contain line breaks');

  const NewValueText = QuoteScriptParameterValueIfNeeded(AValue, AQuoteNewValue);
  var RawText := AName + ': ' + NewValueText;
  if FParameters.Count > 0 then
    RawText := ' ' + RawText;
  const Parameter = TScriptEntryParameter.Create;
  Parameter.SetRawText(RawText);
  FParameters.Add(Parameter);
  MarkModified;
  Result := Integer(FParameters.Count)-1;
end;

procedure TScriptParameterEntry.ApplyParameterFlagRules(const AParameterName,
  AValue: String);
begin
  { Clearing the value leaves the flag in place }
  if (FMetadata = nil) or (Trim(AValue) = '') then
    Exit;
  for var Rule in FMetadata.ParameterIncludesFlagRules do
    if SameText(Rule.ParameterName, AParameterName) then
      SetFlagInternal(Rule.FlagParameterName, Rule.FlagName, True);
end;

procedure TScriptParameterEntry.SetValue(const AIndex: Integer;
  const AValue: String);
begin
  BeginUpdate;
  try
    const Name = GetNamedParameter(AIndex).Name;
    SetValueInternal(AIndex, AValue);
    ApplyParameterFlagRules(Name, AValue);
  finally
    EndUpdate;
  end;
end;

function TScriptParameterEntry.Add(const AName, AValue: String): Integer;
begin
  BeginUpdate;
  try
    Result := AppendParameterInternal(AName, AValue,
      ShouldQuoteNewValue(FQuoteNewValues, FMetadata, AName));
    ApplyParameterFlagRules(AName, AValue);
  finally
    EndUpdate;
  end;
end;

procedure TScriptParameterEntry.Remove(const AIndex: Integer);
begin
  GetNamedParameter(AIndex); { This is a sanity check: GetNamedParameter raises if not named }
  BeginUpdate;
  try
    FParameters.Delete(AIndex);
    { Update breaks }
    for var B := FBreaks.Count-1 downto 0 do begin
      if FBreaks[B].ParameterIndex = AIndex then
        FBreaks.Delete(B)
      else if FBreaks[B].ParameterIndex > AIndex then begin
        var EntryBreak := FBreaks[B];
        Dec(EntryBreak.ParameterIndex);
        FBreaks[B] := EntryBreak;
      end;
    end;
    if (AIndex = 0) and (FParameters.Count > 0) then
      FParameters[0].SetRawText(TrimLeft(FParameters[0].RawText)); { Make sure leftover whitespace is removed }
    MarkModified;
  finally
    EndUpdate;
  end;
end;

function IsValidScriptFlagName(const S: String): Boolean;
begin
  if S = '' then
    Exit(False);
  for var I := 1 to Length(S) do
    if (S[I] <= ' ') or CharInSet(S[I], [';', '"']) then
      Exit(False);
  Result := True;
end;

function TScriptParameterEntry.FlagIncluded(const AIndex: Integer;
  const AFlagName: String): Boolean;
begin
  var StartIndex, TokenLength: Integer;
  Result := FindScriptFlagToken(GetNamedParameter(AIndex).Value, AFlagName,
    StartIndex, TokenLength);
end;

procedure TScriptParameterEntry.ApplyFlagRules(const AParameterName,
  AIncludedFlagName: String);
begin
  if FMetadata = nil then
    Exit;
  { Includes rules run in the forward direction only }
  for var Rule in FMetadata.FlagIncludesRules do begin
    if SameText(Rule.ParameterName, AParameterName) and
       SameText(Rule.FlagName, AIncludedFlagName) then begin
      for var ImpliedFlagName in Rule.OtherFlagNames do
        SetFlagInternal(AParameterName, ImpliedFlagName, True);
    end;
  end;
  for var Rule in FMetadata.FlagExcludesRules do begin
    if SameText(Rule.ParameterName, AParameterName) then begin
      if SameText(Rule.FlagName, AIncludedFlagName) then begin
        { Forward: FlagName was included, exclude the other flags }
        for var ExcludedFlagName in Rule.OtherFlagNames do
          SetFlagInternal(AParameterName, ExcludedFlagName, False);
      end else begin
        { Reverse: if a listed other flag was included, exclude
          FlagName, but not the other listed flags }
        for var ExcludedFlagName in Rule.OtherFlagNames do
          if SameText(ExcludedFlagName, AIncludedFlagName) then
            SetFlagInternal(AParameterName, Rule.FlagName, False);
      end;
    end;
  end;
end;

procedure TScriptParameterEntry.SetFlagInternal(const AParameterName,
  AFlagName: String; const AInclude: Boolean);
{ Like the by-index overload below, but the flag-list parameter doesn't have to
  exist yet. If that is the case then including adds it and excluding is a noop }
begin
  const I = IndexOf(AParameterName);
  if I >= 0 then
    SetFlagInternal(I, AFlagName, AInclude)
  else if AInclude then begin
    { Sanity check }
    if not IsValidScriptFlagName(AFlagName) then
      raise EScriptModelError.Create('Internal error: Invalid flag name');
    AppendParameterInternal(AParameterName, AFlagName, False);
    ApplyFlagRules(AParameterName, AFlagName);
  end;
end;

procedure TScriptParameterEntry.SetFlagInternal(const AIndex: Integer;
  const AFlagName: String; const AInclude: Boolean);
{ Includes or excludes a flag in an existing flag-list parameter. Including
  also runs the flag rules, so extra other flags could be turned on or off as well. }
begin
  { Sanity check }
  if not IsValidScriptFlagName(AFlagName) then
    raise EScriptModelError.Create('Internal error: Invalid flag name');

  const Parameter = GetNamedParameter(AIndex);
  const OldValue = Parameter.Value;
  var StartIndex, TokenLength: Integer;
  const Found = FindScriptFlagToken(OldValue, AFlagName, StartIndex, TokenLength);
  if AInclude then begin
    if Found then
      Exit;
    var NewValue := OldValue;
    if NewValue <> '' then
      NewValue := NewValue + ' ';
    SetValueInternal(AIndex, NewValue + AFlagName);
    ApplyFlagRules(Parameter.Name, AFlagName);
  end else begin
    if not Found then
      Exit;
    { Duplicates of the flag are all removed so it really ends up excluded }
    var NewValue := OldValue;
    repeat
      { Remove only the token itself plus one adjacent whitespace run }
      var RemoveEnd := StartIndex + TokenLength - 1;
      if RemoveEnd < Length(NewValue) then begin
        while (RemoveEnd < Length(NewValue)) and (NewValue[RemoveEnd+1] <= ' ') do
          Inc(RemoveEnd);
      end else begin
        while (StartIndex > 1) and (NewValue[StartIndex-1] <= ' ') do
          Dec(StartIndex);
      end;
      NewValue := Copy(NewValue, 1, StartIndex-1) +
        Copy(NewValue, RemoveEnd+1, MaxInt);
    until not FindScriptFlagToken(NewValue, AFlagName, StartIndex, TokenLength);
    if Trim(NewValue) = '' then
      Remove(AIndex) { Nothing left so remove the whole parameter }
    else
      SetValueInternal(AIndex, NewValue);
  end;
end;

procedure TScriptParameterEntry.SetFlag(const AIndex: Integer;
  const AFlagName: String; const AInclude: Boolean);
begin
  BeginUpdate;
  try
    SetFlagInternal(AIndex, AFlagName, AInclude);
  finally
    EndUpdate;
  end;
end;

function TScriptParameterEntry.TryGetDefinition(const AName: String;
  out ADefinition: TScriptParameterDefinition): Boolean;
begin
  Result := (FMetadata <> nil) and FMetadata.TryGetParameter(AName, ADefinition);
end;

{ TScriptDirectiveSectionLine }

function TScriptDirectiveSectionLine.GetValue: String;
begin
  Result := UnquoteScriptDirectiveValue(FRawValue);
end;

{ TScriptDirectiveSection }

constructor TScriptDirectiveSection.Create(const AMetadata: TScriptSectionMetadata);
begin
  inherited Create;
  FMetadata := AMetadata;
  FLines := TObjectList<TScriptDirectiveSectionLine>.Create;
  FQuoteNewValues := False;
end;

destructor TScriptDirectiveSection.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TScriptDirectiveSection.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TScriptDirectiveSection.Parse(const ALines: array of String);
begin
  FLines.Clear;
  var I := 0;
  while I <= High(ALines) do begin
    { Join the physical lines like ISPP's TPreprocessor.InternalQueueLine does }
    var Last := I;
    while (Last < High(ALines)) and ScriptLineSpans(ALines[Last]) do
      Inc(Last);
    const Line = TScriptDirectiveSectionLine.Create;
    SetLength(Line.FOriginalLines, Last-I+1);
    for var J := I to Last do
      Line.FOriginalLines[J-I] := ALines[J];
    const Joined = JoinSpannedScriptLines(Line.FOriginalLines);
    Line.FKind := sdlOther;
    if ClassifyScriptLine(Joined) = slkActual then begin
      var NameText, RawValue: String;
      if TryParseScriptDirectiveLine(Joined, NameText, RawValue) then begin
        Line.FKind := sdlDirective;
        Line.FNameText := NameText;
        Line.FName := Trim(NameText);
        Line.FRawValue := RawValue;
      end;
    end;
    FLines.Add(Line);
    I := Last+1;
  end;
end;

function TScriptDirectiveSection.GetLines: TArray<String>;
begin
  const LineList = TList<String>.Create;
  try
    for var Line in FLines do begin
      if Line.FModified then
        LineList.Add(Line.FNameText + '=' + Line.FRawValue) { Does not keep line spanning }
      else
        LineList.AddRange(Line.FOriginalLines);
    end;
    Result := LineList.ToArray;
  finally
    LineList.Free;
  end;
end;

function TScriptDirectiveSection.Count: Integer;
begin
  Result := Integer(FLines.Count);
end;

function TScriptDirectiveSection.GetLine(Index: Integer): TScriptDirectiveSectionLine;
begin
  Result := FLines[Index];
end;

function TScriptDirectiveSection.IndexOf(const AName: String): Integer;
{ With duplicate directives the last one wins. Also see
  TLiveScriptObjectFactory.TryGetSetupDirectiveValue which does the same. }
begin
  Result := -1;
  for var I := 0 to Count-1 do
    if (FLines[I].Kind = sdlDirective) and SameText(FLines[I].Name, AName) then
      Result := I;
end;

function TScriptDirectiveSection.TryResolve(const AName: String;
  var AIndex: Integer): Boolean;
{ Pass -1 as AIndex to look the directive up by name }
begin
  if AIndex < 0 then
    AIndex := IndexOf(AName);
  Result := (AIndex >= 0) and (AIndex < Count) and
    (FLines[AIndex].Kind = sdlDirective) and
    SameText(FLines[AIndex].Name, AName);
end;

function TScriptDirectiveSection.TryGetValue(const AName: String;
  out AValue: String): Boolean;
begin
  const I = IndexOf(AName);
  Result := I >= 0;
  if Result then
    AValue := FLines[I].Value;
end;

function TScriptDirectiveSection.GetNamedLine(
  const AIndex: Integer): TScriptDirectiveSectionLine;
begin
  Result := FLines[AIndex];
  if Result.Kind <> sdlDirective then
    raise EScriptModelError.Create('Internal error: Line is not a directive');
end;

procedure TScriptDirectiveSection.SetValue(const AIndex: Integer;
  const AValue: String);
begin
  if ContainsLineBreak(AValue) then
    raise EScriptModelError.Create('Internal error: Value must not contain line breaks');
  const Line = GetNamedLine(AIndex);
  { Keep any whitespace between the '=' and the old value, and keep quotes }
  Line.FRawValue := LeadingWhitespace(Line.FRawValue) +
    QuoteScriptDirectiveValueIfNeeded(AValue, ScriptValueIsQuoted(Line.FRawValue));
  Line.FModified := True;
  Changed;
end;

function TScriptDirectiveSection.Add(const AName,
  AValue: String): Integer;
begin
  { Sanity checks }
  if (AName <> Trim(AName)) or ContainsLineBreak(AName) or
     (Pos('=', AName) > 0) or (ClassifyScriptLine(AName) <> slkActual) then
    raise EScriptModelError.Create('Internal error: Invalid directive name');
  if ContainsLineBreak(AValue) then
    raise EScriptModelError.Create('Internal error: Value must not contain line breaks');

  const Line = TScriptDirectiveSectionLine.Create;
  Line.FKind := sdlDirective;
  Line.FNameText := AName;
  Line.FName := AName;
  { A newly added directive is quoted according to the section's option }
  Line.FRawValue := QuoteScriptDirectiveValueIfNeeded(AValue,
    ShouldQuoteNewValue(FQuoteNewValues, FMetadata, AName));
  Line.FModified := True;
  { Insert after the last directive so trailing comments or blank lines stay
    at the end. With no directives yet, append at the end. }
  Result := Count;
  for var I := Count-1 downto 0 do
    if FLines[I].Kind = sdlDirective then begin
      Result := I+1;
      Break;
    end;
  FLines.Insert(Result, Line);
  Changed;
end;

procedure TScriptDirectiveSection.Remove(const AIndex: Integer);
begin
  GetNamedLine(AIndex);
  FLines.Delete(AIndex);
  Changed;
end;

function TScriptDirectiveSection.TryGetDefinition(const AName: String;
  out ADefinition: TScriptParameterDefinition): Boolean;
begin
  Result := (FMetadata <> nil) and FMetadata.TryGetParameter(AName, ADefinition);
end;

function TScriptDirectiveSection.DefaultValue(const AName: String): String;
begin
  Result := '';
  var Definition: TScriptParameterDefinition;
  if TryGetDefinition(AName, Definition) then
    Result := Definition.DefaultValue;
end;

end.
