unit IDE.ScriptModel;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Script model which can parse and store a single entry of a parameter
  section, or single occurrence of a key/value section.

  Uses the InnoIDE storage technique:
  - Parameter sections: a section entry is an ordered list of
    named parameters where everything parsed (known or unknown) is
    preserved and edits touch only what changed. Parsing has no error
    path and creates a structure which can always be serialized back
    into the parsed lines, byte-identical. Editing keeps line spanning.
  - Key/value sections: a section occurrence is an ordered list of
    its logical lines, so after joining physical lines for line
    spanning. Editing does not keep line spanning. Does keep leading
    whitespace and quotes.

  Supports an OnChange event to get notified of changes.

  Also contains a read-only model for a single occurrence of a [Code]
  section, which uses the ROPS tokenizer to parse it into lists of
  user-defined declarations.
}

interface

uses
  SysUtils, Classes, Generics.Collections,
  IDE.ScriptModel.Metadata;

type
  EScriptModelError = class(Exception);

  TScriptLineKind = (slkBlank, slkComment, slkISPPDirective, slkActual);

  TValuePosition = record
    StartLineIndex, StartCharIndex, EndLineIndex, EndCharIndex: Integer;
  end;

  TParameterSectionEntryParameterKind = (pkParameter, pkOther);

  { A single parameter of an entry in a parameter section: either a Name: Value
    or another kind of chunk of text between ';' separators }
  TParameterSectionEntryParameter = class
  private
    FRawText: String; { The original text }
    FName: String;    { May be empty }
    FValueStartIndex: Integer; { Index in FRawText of the first character after the ':', or 0 }
    procedure SetRawText(const ARawText: String);
    function GetKind: TParameterSectionEntryParameterKind;
    function GetRawValue: String;
    function GetValue: String;
  public
    property Kind: TParameterSectionEntryParameterKind read GetKind;
    property Name: String read FName;
    property RawText: String read FRawText;
    property RawValue: String read GetRawValue;
    property Value: String read GetValue;
  end;

  { A remembered ISPP line span }
  TParameterSectionEntryLineSpan = record
    ParameterIndex: Integer; { Parameter the physical line started with }
    Indent: String;          { Leading whitespace of the physical line as written }
  end;

  { An entry of a parameter section }
  TScriptModelParameterSectionEntry = class
  private
    FMetadata: TScriptModelSectionMetadata; { May be nil }
    FParameters: TObjectList<TParameterSectionEntryParameter>;
    FLineSpans: TList<TParameterSectionEntryLineSpan>;
    FIndent: String; { First line indent }
    FOriginalLines: TArray<String>; { Before modification }
    FLineStartOffsets: TArray<Integer>; { Before modification, set by Parse }
    FChunkStartOffsets: TArray<Integer>; { Before modification, set by Parse }
    FModified: Boolean;
    FOnChange: TNotifyEvent;
    FUpdateLevel: Integer;
    FPendingChange: Boolean;
    FQuoteNewValues: Boolean;
    procedure ApplyFlagRules(const AParameterName, AIncludedFlagName: String);
    procedure ApplyParameterIncludesFlagRules(const AParameterName, AValue: String);
    function AppendParameterInternal(const AName, AValue: String;
      const AQuoteNewValue: Boolean): Integer;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetLineSpanParameterIndex(Index: Integer): Integer;
    function GetNamedParameter(
      const AIndex: Integer): TParameterSectionEntryParameter;
    function GetParameter(Index: Integer): TParameterSectionEntryParameter;
    procedure MarkModified;
    procedure SetFlagInternal(const AParameterName, AFlagName: String;
      const AInclude: Boolean); overload;
    procedure SetFlagInternal(const AIndex: Integer; const AFlagName: String;
      const AInclude: Boolean); overload;
    function SetValueInternal(const AIndex: Integer;
      const AValue: String): Boolean;
  public
    constructor Create(const AMetadata: TScriptModelSectionMetadata);
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
      out ADefinition: TMemberDefinition): Boolean;
    function TryGetParameterIndex(const AOriginalLineIndex, AOriginalCharIndex: Integer;
      out AParameterIndex: Integer): Boolean;
    function TryGetValuePosition(const AParameterIndex: Integer;
      out APosition: TValuePosition): Boolean;
    function LineSpanCount: Integer;
    property LineSpanParameterIndexes[Index: Integer]: Integer read GetLineSpanParameterIndex;
    property Indent: String read FIndent;
    property Metadata: TScriptModelSectionMetadata read FMetadata;
    property Modified: Boolean read FModified;
    property Parameters[Index: Integer]: TParameterSectionEntryParameter read GetParameter;
    property QuoteNewValues: Boolean read FQuoteNewValues write FQuoteNewValues;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TKeyValueSectionLineKind = (lkKeyValue, lkOther);

  { A single logical line in a key/value section: either a Name=Value or
    another kind of line (comment, ISPP directive, blank, or anything else) }
  TKeyValueSectionLine = class
  private
    FKind: TKeyValueSectionLineKind;
    FOriginalLines: TArray<String>; { The original lines }
    FLineStartOffsets: TArray<Integer>; { Before modification, set by Parse }
    FNameText: String;              { Original name }
    FName: String;                  { Trimmed name }
    FRawValue: String;              { Original value }
    FModified: Boolean;
    function GetValue: String;
  public
    property Kind: TKeyValueSectionLineKind read FKind;
    property Name: String read FName;
    property RawValue: String read FRawValue;
    property Value: String read GetValue;
  end;

  { A single occurrence of a key/value section }
  TScriptModelKeyValueSection = class
  private
    FMetadata: TScriptModelSectionMetadata; { May be nil }
    FLines: TObjectList<TKeyValueSectionLine>;
    FOnChange: TNotifyEvent;
    FUpdateLevel: Integer;
    FPendingChange: Boolean;
    FQuoteNewValues: Boolean;
    procedure ApplyFlagRules(const AIndex: Integer;
      const AIncludedFlagName: String);
    procedure BeginUpdate;
    procedure Changed;
    procedure EndUpdate;
    function GetNamedLine(const AIndex: Integer): TKeyValueSectionLine;
    function GetLine(Index: Integer): TKeyValueSectionLine;
    procedure SetFlagInternal(const AIndex: Integer; const AFlagName: String;
      const AInclude: Boolean);
  public
    constructor Create(const AMetadata: TScriptModelSectionMetadata);
    destructor Destroy; override;
    procedure Parse(const ALines: array of String);
    function GetLines: TArray<String>;
    function GetLineCount(const AIndex: Integer): Integer;
    function Count: Integer;
    function IndexOf(const AName: String): Integer;
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
      out ADefinition: TMemberDefinition): Boolean;
    function TryGetValuePosition(const AIndex: Integer;
      out APosition: TValuePosition): Boolean;
    function DefaultValue(const AName: String): String;
    property Lines[Index: Integer]: TKeyValueSectionLine read GetLine;
    property Metadata: TScriptModelSectionMetadata read FMetadata;
    property QuoteNewValues: Boolean read FQuoteNewValues write FQuoteNewValues;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCodeSectionRoutineKind = (rkProcedure, rkFunction);
  TCodeSectionRoutineBodilessType = (btNo, btForward, btExternal);

  { A user-defined type, constant or global variable, a parameter of a
    user-defined routine or interface method, or a local variable of a
    user-defined routine }
  TCodeSectionDeclaration = class
  private
    FName: String;
    FTypeText: String; { Constants: 'String' even for characters, 'Integer' for any width, '' when not inferable. Parameters: '' when untyped. }
    FLine: Integer;
  public
    property Name: String read FName;
    property TypeText: String read FTypeText;
    property Line: Integer read FLine;
  end;

  { A user-defined procedure or function }
  TCodeSectionRoutine = class
  private
    FName: String;
    FKind: TCodeSectionRoutineKind;
    FResultTypeText: String;
    FPrototype: String;
    FParameters: TObjectList<TCodeSectionDeclaration>;
    FLocals: TObjectList<TCodeSectionDeclaration>;
    FFirstLine, FLastLine: Integer; { Always set }
    FBodyFirstLine: Integer;        { -1 for a bodiless routine and until its 'begin' is seen }
    FBodilessType: TCodeSectionRoutineBodilessType;
    function GetParameter(Index: Integer): TCodeSectionDeclaration;
    function GetLocal(Index: Integer): TCodeSectionDeclaration;
  public
    constructor Create;
    destructor Destroy; override;
    function ParameterCount: Integer;
    function LocalCount: Integer;
    property Name: String read FName;
    property Kind: TCodeSectionRoutineKind read FKind;
    property ResultTypeText: String read FResultTypeText;
    property Prototype: String read FPrototype;
    property Parameters[Index: Integer]: TCodeSectionDeclaration read GetParameter;
    property Locals[Index: Integer]: TCodeSectionDeclaration read GetLocal;
    property FirstLine: Integer read FFirstLine;
    property BodyFirstLine: Integer read FBodyFirstLine;
    property LastLine: Integer read FLastLine;
    property BodilessType: TCodeSectionRoutineBodilessType read FBodilessType;
  end;

  { A method of a user-defined interface type }
  TCodeSectionInterfaceMethod = class
  private
    FName: String;
    FKind: TCodeSectionRoutineKind;
    FDeclarationTypeIndex: Integer; { For anonymous interfaces this is the type it is nested in, so not an interface }
    FResultTypeText: String;
    FPrototype: String;
    FParameters: TObjectList<TCodeSectionDeclaration>;
    FLine: Integer;
    function GetParameter(Index: Integer): TCodeSectionDeclaration;
  public
    constructor Create;
    destructor Destroy; override;
    function ParameterCount: Integer;
    property Name: String read FName;
    property Kind: TCodeSectionRoutineKind read FKind;
    property DeclarationTypeIndex: Integer read FDeclarationTypeIndex;
    property ResultTypeText: String read FResultTypeText;
    property Prototype: String read FPrototype;
    property Parameters[Index: Integer]: TCodeSectionDeclaration read GetParameter;
    property Line: Integer read FLine;
  end;

  { A value of a user-defined enumeration type }
  TCodeSectionEnumerationValue = class
  private
    FName: String;
    FDeclarationTypeIndex: Integer; { For anonymous enumerations this is the type it is nested in, so not an enumeration }
    FLine: Integer;
  public
    property Name: String read FName;
    property DeclarationTypeIndex: Integer read FDeclarationTypeIndex;
    property Line: Integer read FLine;
  end;

  { A single occurrence of a [Code] section. Read-only. All line numbers are
    0-based indexes into the lines given to Parse. Parse never raises on
    malformed input: invalid code is simply skipped as long as it tokenizes,
    and after a tokenizer error it restarts at the next line, except for
    unterminated comment errors. All the time it keeps the declarations found. }
  TScriptModelCodeSection = class
  private
    FRoutines: TObjectList<TCodeSectionRoutine>;
    FTypes: TObjectList<TCodeSectionDeclaration>;
    FEnumerationValues: TObjectList<TCodeSectionEnumerationValue>;
    FInterfaceMethods: TObjectList<TCodeSectionInterfaceMethod>;
    FConstants: TObjectList<TCodeSectionDeclaration>;
    FGlobalVariables: TObjectList<TCodeSectionDeclaration>;
    function GetRoutine(Index: Integer): TCodeSectionRoutine;
    function GetType(Index: Integer): TCodeSectionDeclaration;
    function GetEnumerationValue(Index: Integer): TCodeSectionEnumerationValue;
    function GetInterfaceMethod(Index: Integer): TCodeSectionInterfaceMethod;
    function GetConstant(Index: Integer): TCodeSectionDeclaration;
    function GetGlobalVariable(Index: Integer): TCodeSectionDeclaration;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const ALines: array of String);
    function RoutineCount: Integer;
    function TypeCount: Integer;
    function EnumerationValueCount: Integer;
    function InterfaceMethodCount: Integer;
    function ConstantCount: Integer;
    function GlobalVariableCount: Integer;
    function TryGetRoutine(const ALine: Integer;
      out ARoutine: TCodeSectionRoutine;
      const AFromBodyOnly: Boolean = False): Boolean;
    property Routines[Index: Integer]: TCodeSectionRoutine read GetRoutine;
    property Types[Index: Integer]: TCodeSectionDeclaration read GetType;
    property EnumerationValues[Index: Integer]: TCodeSectionEnumerationValue read GetEnumerationValue;
    property InterfaceMethods[Index: Integer]: TCodeSectionInterfaceMethod read GetInterfaceMethod;
    property Constants[Index: Integer]: TCodeSectionDeclaration read GetConstant;
    property GlobalVariables[Index: Integer]: TCodeSectionDeclaration read GetGlobalVariable;
  end;

function ClassifyScriptLine(const S: String): TScriptLineKind;
function JoinSpannedScriptLines(const ALines: array of String): String; overload;
function JoinSpannedScriptLines(const ALines: array of String;
  out ALineStartOffsets: TArray<Integer>): String; overload;
function ContainsLineBreak(const S: String): Boolean;
function ScriptValueIncludesFlag(const AValue, AFlagName: String): Boolean;

{ These are in the interface only for the Test unit }
function ScriptLineSpans(const S: String): Boolean;
function UnquoteParameterValue(const S: String): String;
function QuoteParameterValueIfNeeded(const S: String;
  const AAlwaysQuote: Boolean = False): String;
function UnquoteKeyValueValue(const S: String): String;
function TryParseKeyValueLine(const S: String;
  out ANameText, ARawValue: String): Boolean;
function PrepareCodeSectionText(const ALines: array of String): AnsiString;

implementation

uses
  uPSUtils;

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

function JoinSpannedScriptLines(const ALines: array of String): String;
begin
  var LineStartOffsets: TArray<Integer>;
  Result := JoinSpannedScriptLines(ALines, LineStartOffsets);
end;

function JoinSpannedScriptLines(const ALines: array of String;
  out ALineStartOffsets: TArray<Integer>): String;
begin
  SetLength(ALineStartOffsets, Length(ALines));
  if Length(ALines) = 1 then begin
    ALineStartOffsets[0] := Length(LeadingWhitespace(ALines[0]))+1;
    Exit(ALines[0]);
  end;
  { Matches ISPP's TPreprocessor.InternalQueueLine }
  const Builder = TStringBuilder.Create;
  try
    for var I := 0 to High(ALines) do begin
      var S := ALines[I];
      if (I < High(ALines)) and ScriptLineSpans(S) then
        SetLength(S, Length(S)-1);
      ALineStartOffsets[I] := Builder.Length+1;
      Builder.Append(TrimLeft(S));
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

procedure DoGetValuePosition(const ARawValueOffset: Integer;
  const ARawValue: String; const ALineStartOffsets: TArray<Integer>;
  const AOriginalLines: TArray<String>; out APosition: TValuePosition);
{ Shared helper for the TryGetValuePosition functions. ARawValueOffset tells
  where the raw value starts within the join of AOriginalLines, and
  ALineStartOffsets where each line's own content starts within that join. }

  procedure OffsetToPosition(const AOffset: Integer;
    out ALineIndex, ACharIndex: Integer);
  { Finds the offset's character in the original lines: it is in the last line
    whose content starts at or before the offset }
  begin
    ALineIndex := 0;
    for var I := 1 to High(ALineStartOffsets) do begin
      if ALineStartOffsets[I] > AOffset then
        Break;
      ALineIndex := Integer(I);
    end;
    ACharIndex := AOffset - ALineStartOffsets[ALineIndex] +
      Length(LeadingWhitespace(AOriginalLines[ALineIndex]));
  end;

begin
  { The start is past the value's leading whitespace }
  const Offset = ARawValueOffset + Length(LeadingWhitespace(ARawValue));
  OffsetToPosition(Offset, APosition.StartLineIndex, APosition.StartCharIndex);

  { The end is one past the value's last non-whitespace character }
  const TrimmedLength = Length(Trim(ARawValue));
  if TrimmedLength > 0 then begin
    OffsetToPosition(Offset + TrimmedLength - 1, APosition.EndLineIndex,
      APosition.EndCharIndex);
    Inc(APosition.EndCharIndex);
  end else begin
    APosition.EndLineIndex := APosition.StartLineIndex;
    APosition.EndCharIndex := APosition.StartCharIndex;
  end;
end;

function UnquoteParameterValue(const S: String): String;
begin
  Result := Trim(S);
  if (Length(Result) >= 2) and (Result[1] = '"') and
     (Result[Length(Result)] = '"') then begin
    Result := Copy(Result, 2, Length(Result)-2);
    Result := StringReplace(Result, '""', '"', [rfReplaceAll]);
  end;
end;

function QuoteParameterValueIfNeeded(const S: String;
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

function QuoteKeyValueValueIfNeeded(const AValue: String;
  const AAlwaysQuote: Boolean = False): String;
begin
  { Key/value values only need quotes to keep leading or trailing whitespace,
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
  const AMetadata: TScriptModelSectionMetadata; const AName: String): Boolean;
begin
  if not AQuoteNewValues then
    Exit(False);
  var Definition: TMemberDefinition;
  if (AMetadata <> nil) and AMetadata.TryGetMember(AName, Definition) then
    Result := Definition.ValueKind in [mvkString, mvkChoice,
      mvkCompilerSourceFile, mvkCompilerSourceFiles, mvkCompilerPath,
      mvkCompilerDestFile]
  else
    Result := True;
end;

function UnquoteKeyValueValue(const S: String): String;
begin
  Result := Trim(S);
  { If the value is surrounded in quotes, remove them, just like
    TSetupCompiler.SeparateDirective. Unlike parameter values, embedded
    quotes are not doubled so there is nothing else to do }
  if ScriptValueIsQuoted(Result) then
    Result := Copy(Result, 2, Length(Result)-2);
end;

function TryParseKeyValueLine(const S: String;
  out ANameText, ARawValue: String): Boolean;
begin
  const P = Pos('=', S);
  Result := (P > 0) and (Trim(Copy(S, 1, P-1)) <> '');
  if Result then begin
    ANameText := Copy(S, 1, P-1);
    ARawValue := Copy(S, P+1, MaxInt);
  end;
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

function ScriptValueIncludesFlag(const AValue, AFlagName: String): Boolean;
begin
  var StartIndex, TokenLength: Integer;
  Result := FindScriptFlagToken(AValue, AFlagName, StartIndex, TokenLength);
end;

function RemoveScriptFlagTokens(const AValue, AFlagName: String): String;
begin
  Result := AValue;

  { Duplicates of the flag are all removed so it really ends up excluded }
  var StartIndex, TokenLength: Integer;
  while FindScriptFlagToken(Result, AFlagName, StartIndex, TokenLength) do begin
    { Remove only the token itself plus one adjacent whitespace run }
    var RemoveEnd := StartIndex + TokenLength - 1;
    if RemoveEnd < Length(Result) then begin
      while (RemoveEnd < Length(Result)) and (Result[RemoveEnd+1] <= ' ') do
        Inc(RemoveEnd);
    end else begin
      while (StartIndex > 1) and (Result[StartIndex-1] <= ' ') do
        Dec(StartIndex);
    end;
    Result := Copy(Result, 1, StartIndex-1) + Copy(Result, RemoveEnd+1, MaxInt);
  end;
end;

{ TParameterSectionEntryParameter }

procedure TParameterSectionEntryParameter.SetRawText(const ARawText: String);
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

function TParameterSectionEntryParameter.GetKind:
  TParameterSectionEntryParameterKind;
begin
  if FName <> '' then
    Result := pkParameter
  else
    Result := pkOther;
end;

function TParameterSectionEntryParameter.GetRawValue: String;
begin
  if FValueStartIndex > 0 then
    Result := Copy(FRawText, FValueStartIndex, MaxInt)
  else
    Result := '';
end;

function TParameterSectionEntryParameter.GetValue: String;
begin
  Result := UnquoteParameterValue(GetRawValue);
end;

{ TScriptModelParameterSectionEntry }

constructor TScriptModelParameterSectionEntry.Create(
  const AMetadata: TScriptModelSectionMetadata);
begin
  inherited Create;
  FMetadata := AMetadata;
  FParameters := TObjectList<TParameterSectionEntryParameter>.Create;
  FLineSpans := TList<TParameterSectionEntryLineSpan>.Create;
  FQuoteNewValues := True;
end;

destructor TScriptModelParameterSectionEntry.Destroy;
begin
  FLineSpans.Free;
  FParameters.Free;
  inherited;
end;

procedure TScriptModelParameterSectionEntry.Parse(
  const ALines: array of String);
begin
  FParameters.Clear;
  FLineSpans.Clear;
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
  SetLength(FLineStartOffsets, Length(ALines));
  for var I := 0 to High(ALines) do begin
    var S := ALines[I];
    if (I < High(ALines)) and ScriptLineSpans(S) then
      SetLength(S, Length(S)-1);
    if I = 0 then begin
      FIndent := LeadingWhitespace(S);
      S := Copy(S, Length(FIndent)+1, MaxInt);
    end else
      S := TrimLeft(S);
    FLineStartOffsets[I] := Length(Joined)+1;
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
          const Parameter = TParameterSectionEntryParameter.Create;
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
    FChunkStartOffsets := ChunkStartOffsets.ToArray;
  finally
    ChunkStartOffsets.Free;
  end;

  { Map each physical line break to the parameter containing its offset;
    a break that fell between parameters belongs to the following one }
  for var I := 1 to High(ALines) do begin
    const Offset = FLineStartOffsets[I];
    var ParameterIndex := Integer(FParameters.Count);
    for var J := 0 to High(FChunkStartOffsets) do begin
      if Offset <= FChunkStartOffsets[J] + Length(FParameters[J].RawText) - 1 then begin
        ParameterIndex := Integer(J);
        Break;
      end;
    end;
    var LineSpan: TParameterSectionEntryLineSpan;
    LineSpan.ParameterIndex := ParameterIndex;
    LineSpan.Indent := LeadingWhitespace(ALines[I]);
    FLineSpans.Add(LineSpan);
  end;
end;

function TScriptModelParameterSectionEntry.GetLines: TArray<String>;
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
    for var B := 0 to FLineSpans.Count do begin
      var Boundary: Integer;
      if B < FLineSpans.Count then
        Boundary := FLineSpans[B].ParameterIndex
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

      if (B < FLineSpans.Count) and LineHasParameters then begin
        if Boundary = Integer(FParameters.Count) then begin
          { Make sure 'Source: x; \<EOL>  ' doesn't become 'Source: x; ; \',
            and 'Source: x \<EOL>  ' doesn't gain a ';' and
            'Source: x; Flags: touch \<EOL>  ' with Flags removed doesn't
            become 'Source: x\' }
          if (Line = '') or (Line[Length(Line)] > ' ') then
            Line := Line + ' ';
          Line := Line + '\';
        end else begin
          { End this line with a continuation: the whitespace before the
            backslash comes from the next parameter's leading whitespace }
          var SuffixWhitespace := LeadingWhitespace(FParameters[Boundary].RawText);
          if SuffixWhitespace = '' then
            SuffixWhitespace := ' ';
          Line := Line + ';' + SuffixWhitespace + '\';
        end;
        LineList.Add(Line);
        Line := FLineSpans[B].Indent;
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

function TScriptModelParameterSectionEntry.Count: Integer;
begin
  Result := Integer(FParameters.Count);
end;

function TScriptModelParameterSectionEntry.GetParameter(
  Index: Integer): TParameterSectionEntryParameter;
begin
  Result := FParameters[Index];
end;

function TScriptModelParameterSectionEntry.GetNamedParameter(
  const AIndex: Integer): TParameterSectionEntryParameter;
begin
  Result := FParameters[AIndex];
  if Result.Kind <> pkParameter then
    raise EScriptModelError.Create('Internal error: Parameter has no name');
end;

function TScriptModelParameterSectionEntry.LineSpanCount: Integer;
begin
  Result := Integer(FLineSpans.Count);
end;

function TScriptModelParameterSectionEntry.GetLineSpanParameterIndex(
  Index: Integer): Integer;
begin
  Result := FLineSpans[Index].ParameterIndex;
end;

function TScriptModelParameterSectionEntry.TryGetParameterIndex(
  const AOriginalLineIndex, AOriginalCharIndex: Integer;
  out AParameterIndex: Integer): Boolean;
{ The inverse of TryGetValuePosition: returns the index of the
  parameter occupying the position. Cannot be used after modification:
  uses information from Parse. }
begin
  if FModified or (AOriginalLineIndex < 0) or (AOriginalLineIndex > High(FOriginalLines)) or
     (FParameters.Count = 0) then
    Exit(False);

  var Offset := FLineStartOffsets[AOriginalLineIndex] + AOriginalCharIndex -
    Length(LeadingWhitespace(FOriginalLines[AOriginalLineIndex])); { FLineStartOffsets excludes each line's leading whitespace, so take it off }
  if Offset < FLineStartOffsets[AOriginalLineIndex] then
    Offset := FLineStartOffsets[AOriginalLineIndex];

  { The parameter at the offset is the last one starting at or before it }
  AParameterIndex := 0;
  for var I := 1 to High(FChunkStartOffsets) do begin
    if FChunkStartOffsets[I] > Offset then
      Break;
    AParameterIndex := Integer(I);
  end;
  Result := True;
end;

function TScriptModelParameterSectionEntry.TryGetValuePosition(
  const AParameterIndex: Integer; out APosition: TValuePosition): Boolean;
{ The inverse of TryGetParameterIndex: returns the start and end of the indexed
  parameter's value, excluding whitespace surrounding it. Cannot be used after
  modification: uses information from Parse. }
begin
  if FModified or (AParameterIndex < 0) or
     (AParameterIndex >= Integer(FParameters.Count)) then
    Exit(False);
  const Parameter = FParameters[AParameterIndex];
  if Parameter.Kind <> pkParameter then
    Exit(False);
  DoGetValuePosition(
    FChunkStartOffsets[AParameterIndex] + Parameter.FValueStartIndex - 1,
    Parameter.RawValue, FLineStartOffsets, FOriginalLines, APosition);
  Result := True;
end;

function TScriptModelParameterSectionEntry.IndexOf(
  const AName: String): Integer;
{ With duplicate parameters the first one wins }
begin
  for var I := 0 to Count-1 do
    if (FParameters[I].Kind = pkParameter) and SameText(FParameters[I].Name, AName) then
      Exit(I);
  Result := -1;
end;

function TScriptModelParameterSectionEntry.Has(const AName: String): Boolean;
begin
  Result := IndexOf(AName) >= 0;
end;

function TScriptModelParameterSectionEntry.TryResolve(const AName: String;
  var AIndex: Integer): Boolean;
{ AName is required. AIndex is a hint: it is kept when the parameter at it
  matches AName, otherwise, such as when -1 or stale, AName is looked up by
  name }

  function Valid: Boolean;
  begin
    Result := (AIndex >= 0) and (AIndex < Count) and
      (FParameters[AIndex].Kind = pkParameter) and
      SameText(FParameters[AIndex].Name, AName);
  end;

begin
  Result := Valid;
  if not Result then begin
    AIndex := IndexOf(AName);
    Result := Valid;
  end;
end;

function TScriptModelParameterSectionEntry.TryGetValue(const AName: String;
  out AValue: String): Boolean;
begin
  const I = IndexOf(AName);
  Result := I >= 0;
  if Result then
    AValue := FParameters[I].Value;
end;

procedure TScriptModelParameterSectionEntry.BeginUpdate;
begin
  Inc(FUpdateLevel);
end;

procedure TScriptModelParameterSectionEntry.EndUpdate;
begin
  Dec(FUpdateLevel);
  if (FUpdateLevel = 0) and FPendingChange then begin
    FPendingChange := False;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TScriptModelParameterSectionEntry.MarkModified;
begin
  FModified := True;
  FPendingChange := True;
end;

function TScriptModelParameterSectionEntry.SetValueInternal(
  const AIndex: Integer; const AValue: String): Boolean;
{ Keeps quotes and surrounding whitespace. Returns True if the value changed. }
begin
  if ContainsLineBreak(AValue) then
    raise EScriptModelError.Create('Internal error: Value must not contain line breaks');
  const Parameter = GetNamedParameter(AIndex);
  const OldRawValue = Parameter.RawValue;
  const NewValueText = QuoteParameterValueIfNeeded(AValue,
    ScriptValueIsQuoted(OldRawValue));
  const Leading = LeadingWhitespace(OldRawValue);
  const Trailing = TrailingWhitespace(Copy(OldRawValue, Length(Leading)+1, MaxInt));
  const NewRawText = Copy(Parameter.RawText, 1, Parameter.FValueStartIndex-1) +
    Leading + NewValueText + Trailing;
  if NewRawText = Parameter.RawText then
    Exit(False);
  Parameter.SetRawText(NewRawText);
  MarkModified;
  Result := True;
end;

function TScriptModelParameterSectionEntry.AppendParameterInternal(const AName,
  AValue: String; const AQuoteNewValue: Boolean): Integer;

  { See TParameterSectionEntryParameter.SetRawText }
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

  const NewValueText = QuoteParameterValueIfNeeded(AValue, AQuoteNewValue);
  const Parameter = TParameterSectionEntryParameter.Create;
  const LastIndex = Integer(FParameters.Count)-1;
  if (LastIndex >= 0) and (FParameters[LastIndex].Kind = pkOther) and
     (Trim(FParameters[LastIndex].RawText) = '') then begin
    { Make sure something like 'Source: x;' (which is two parameters) becomes
      'Source: x; NewParam: y;' and not 'Source: x;; NewParam: y'. }
    Parameter.SetRawText(' ' + AName + ': ' + NewValueText);
    FParameters.Insert(LastIndex, Parameter);
    Result := LastIndex;
  end else begin
    var RawText := AName + ': ' + NewValueText;
    if FParameters.Count > 0 then
      RawText := ' ' + RawText;
    Parameter.SetRawText(RawText);
    FParameters.Add(Parameter);
    Result := Integer(FParameters.Count)-1;
  end;
  MarkModified;
end;

procedure TScriptModelParameterSectionEntry.ApplyParameterIncludesFlagRules(
  const AParameterName, AValue: String);
begin
  { Clearing the value leaves the flag in place }
  if (FMetadata = nil) or (Trim(AValue) = '') then
    Exit;
  for var Rule in FMetadata.ParameterIncludesFlagRules do
    if SameText(Rule.ParameterName, AParameterName) then
      SetFlagInternal(Rule.FlagParameterName, Rule.FlagName, True);
end;

procedure TScriptModelParameterSectionEntry.SetValue(const AIndex: Integer;
  const AValue: String);
begin
  BeginUpdate;
  try
    const Name = GetNamedParameter(AIndex).Name;
    if SetValueInternal(AIndex, AValue) then
      ApplyParameterIncludesFlagRules(Name, AValue);
  finally
    EndUpdate;
  end;
end;

function TScriptModelParameterSectionEntry.Add(
  const AName, AValue: String): Integer;
begin
  BeginUpdate;
  try
    Result := AppendParameterInternal(AName, AValue,
      ShouldQuoteNewValue(FQuoteNewValues, FMetadata, AName));
    ApplyParameterIncludesFlagRules(AName, AValue);
  finally
    EndUpdate;
  end;
end;

procedure TScriptModelParameterSectionEntry.Remove(const AIndex: Integer);
begin
  GetNamedParameter(AIndex); { This is a sanity check: GetNamedParameter raises if not named }
  BeginUpdate;
  try
    FParameters.Delete(AIndex);
    { Update breaks }
    for var B := FLineSpans.Count-1 downto 0 do begin
      if FLineSpans[B].ParameterIndex = AIndex then
        FLineSpans.Delete(B)
      else if FLineSpans[B].ParameterIndex > AIndex then begin
        var LineSpan := FLineSpans[B];
        Dec(LineSpan.ParameterIndex);
        FLineSpans[B] := LineSpan;
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

function TScriptModelParameterSectionEntry.FlagIncluded(const AIndex: Integer;
  const AFlagName: String): Boolean;
begin
  Result := ScriptValueIncludesFlag(GetNamedParameter(AIndex).Value, AFlagName);
end;

procedure TScriptModelParameterSectionEntry.ApplyFlagRules(const AParameterName,
  AIncludedFlagName: String);
begin
  if FMetadata = nil then
    Exit;
  { Includes rules run in the forward direction only }
  for var Rule in FMetadata.FlagIncludesRules do begin
    if SameText(Rule.MemberName, AParameterName) and
       SameText(Rule.FlagName, AIncludedFlagName) then begin
      for var ImpliedFlagName in Rule.OtherFlagNames do
        SetFlagInternal(AParameterName, ImpliedFlagName, True);
    end;
  end;
  for var Rule in FMetadata.FlagExcludesRules do begin
    if SameText(Rule.MemberName, AParameterName) then begin
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

procedure TScriptModelParameterSectionEntry.SetFlagInternal(
  const AParameterName, AFlagName: String; const AInclude: Boolean);
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

procedure TScriptModelParameterSectionEntry.SetFlagInternal(
  const AIndex: Integer; const AFlagName: String; const AInclude: Boolean);
{ Includes or excludes a flag in an existing flag-list parameter. Including
  also runs the flag rules, so extra other flags could be turned on or off as well. }
begin
  { Sanity check }
  if not IsValidScriptFlagName(AFlagName) then
    raise EScriptModelError.Create('Internal error: Invalid flag name');

  const Parameter = GetNamedParameter(AIndex);
  const OldValue = Parameter.Value;
  const Found = ScriptValueIncludesFlag(OldValue, AFlagName);
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
    const NewValue = RemoveScriptFlagTokens(OldValue, AFlagName);
    if Trim(NewValue) = '' then
      Remove(AIndex) { Nothing left so remove the whole parameter }
    else
      SetValueInternal(AIndex, NewValue);
  end;
end;

procedure TScriptModelParameterSectionEntry.SetFlag(const AIndex: Integer;
  const AFlagName: String; const AInclude: Boolean);
begin
  BeginUpdate;
  try
    SetFlagInternal(AIndex, AFlagName, AInclude);
  finally
    EndUpdate;
  end;
end;

function TScriptModelParameterSectionEntry.TryGetDefinition(const AName: String;
  out ADefinition: TMemberDefinition): Boolean;
begin
  Result := (FMetadata <> nil) and FMetadata.TryGetMember(AName, ADefinition);
end;

{ TKeyValueSectionLine }

function TKeyValueSectionLine.GetValue: String;
begin
  Result := UnquoteKeyValueValue(FRawValue);
end;

{ TScriptModelKeyValueSection }

constructor TScriptModelKeyValueSection.Create(
  const AMetadata: TScriptModelSectionMetadata);
begin
  inherited Create;
  FMetadata := AMetadata;
  FLines := TObjectList<TKeyValueSectionLine>.Create;
  FQuoteNewValues := False;
end;

destructor TScriptModelKeyValueSection.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TScriptModelKeyValueSection.BeginUpdate;
begin
  Inc(FUpdateLevel);
end;

procedure TScriptModelKeyValueSection.EndUpdate;
begin
  Dec(FUpdateLevel);
  if (FUpdateLevel = 0) and FPendingChange then begin
    FPendingChange := False;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TScriptModelKeyValueSection.Changed;
begin
  if FUpdateLevel > 0 then
    FPendingChange := True
  else if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TScriptModelKeyValueSection.Parse(const ALines: array of String);
begin
  FLines.Clear;
  var I := 0;
  while I <= High(ALines) do begin
    { Join the physical lines like ISPP's TPreprocessor.InternalQueueLine does }
    var Last := I;
    while (Last < High(ALines)) and ScriptLineSpans(ALines[Last]) do
      Inc(Last);
    const Line = TKeyValueSectionLine.Create;
    SetLength(Line.FOriginalLines, Last-I+1);
    for var J := I to Last do
      Line.FOriginalLines[J-I] := ALines[J];
    const Joined = JoinSpannedScriptLines(Line.FOriginalLines, Line.FLineStartOffsets);
    Line.FKind := lkOther;
    if ClassifyScriptLine(Joined) = slkActual then begin
      var NameText, RawValue: String;
      if TryParseKeyValueLine(Joined, NameText, RawValue) then begin
        Line.FKind := lkKeyValue;
        Line.FNameText := NameText;
        Line.FName := Trim(NameText);
        Line.FRawValue := RawValue;
      end;
    end;
    FLines.Add(Line);
    I := Last+1;
  end;
end;

function TScriptModelKeyValueSection.GetLines: TArray<String>;
begin
  const LineList = TList<String>.Create;
  try
    for var Line in FLines do begin
      if Line.FModified then 
        LineList.Add(Line.FNameText + '=' + Line.FRawValue) { Does not keep line spanning. Also see GetLineCount. }
      else
        LineList.AddRange(Line.FOriginalLines);
    end;
    Result := LineList.ToArray;
  finally
    LineList.Free;
  end;
end;

function TScriptModelKeyValueSection.GetLineCount(const AIndex: Integer): Integer;
{ The number of lines GetLines returns for the line at AIndex }
begin
  const Line = FLines[AIndex];
  if Line.FModified then
    Result := 1 { GetLines does not keep line spanning }
  else
    Result := Integer(Length(Line.FOriginalLines));
end;

function TScriptModelKeyValueSection.Count: Integer;
begin
  Result := Integer(FLines.Count);
end;

function TScriptModelKeyValueSection.GetLine(
  Index: Integer): TKeyValueSectionLine;
begin
  Result := FLines[Index];
end;

function TScriptModelKeyValueSection.IndexOf(const AName: String): Integer;
{ With duplicate keys the last one wins. Also see
  TLiveScriptObjectFactory.TryGetSetupDirectiveValue which does the same. }
begin
  Result := -1;
  for var I := 0 to Count-1 do
    if (FLines[I].Kind = lkKeyValue) and SameText(FLines[I].Name, AName) then
      Result := I;
end;

function TScriptModelKeyValueSection.TryResolve(const AName: String;
  var AIndex: Integer): Boolean;
{ AName is required. AIndex is a hint: it is kept when the line at it
  matches AName, otherwise, such as when -1 or stale, AName is looked up by
  name }

  function Valid: Boolean;
  begin
    Result := (AIndex >= 0) and (AIndex < Count) and
      (FLines[AIndex].Kind = lkKeyValue) and
      SameText(FLines[AIndex].Name, AName);
  end;

begin
  Result := Valid;
  if not Result then begin
    AIndex := IndexOf(AName);
    Result := Valid;
  end;
end;

function TScriptModelKeyValueSection.TryGetValue(const AName: String;
  out AValue: String): Boolean;
begin
  const I = IndexOf(AName);
  Result := I >= 0;
  if Result then
    AValue := FLines[I].Value;
end;

function TScriptModelKeyValueSection.GetNamedLine(
  const AIndex: Integer): TKeyValueSectionLine;
begin
  Result := FLines[AIndex];
  if Result.Kind <> lkKeyValue then
    raise EScriptModelError.Create('Internal error: Line is not a key/value');
end;

procedure TScriptModelKeyValueSection.SetValue(const AIndex: Integer;
  const AValue: String);
begin
  if ContainsLineBreak(AValue) then
    raise EScriptModelError.Create('Internal error: Value must not contain line breaks');
  const Line = GetNamedLine(AIndex);
  { Keep any whitespace between the '=' and the old value, and keep quotes }
  const NewRawValue = LeadingWhitespace(Line.FRawValue) +
    QuoteKeyValueValueIfNeeded(AValue, ScriptValueIsQuoted(Line.FRawValue));
  if NewRawValue = Line.FRawValue then
    Exit;
  Line.FRawValue := NewRawValue;
  Line.FModified := True;
  Changed;
end;

function TScriptModelKeyValueSection.Add(const AName,
  AValue: String): Integer;
begin
  { Sanity checks }
  if (AName <> Trim(AName)) or ContainsLineBreak(AName) or
     (Pos('=', AName) > 0) or (ClassifyScriptLine(AName) <> slkActual) then
    raise EScriptModelError.Create('Internal error: Invalid key name');
  if ContainsLineBreak(AValue) then
    raise EScriptModelError.Create('Internal error: Value must not contain line breaks');

  const Line = TKeyValueSectionLine.Create;
  Line.FKind := lkKeyValue;
  Line.FNameText := AName;
  Line.FName := AName;
  { A newly added key/value value is quoted according to the section's option }
  Line.FRawValue := QuoteKeyValueValueIfNeeded(AValue,
    ShouldQuoteNewValue(FQuoteNewValues, FMetadata, AName));
  Line.FModified := True;
  { Insert after the last key so trailing comments or blank lines stay
    at the end. With no keys yet, append at the end. }
  Result := Count;
  for var I := Count-1 downto 0 do
    if FLines[I].Kind = lkKeyValue then begin
      Result := I+1;
      Break;
    end;
  FLines.Insert(Result, Line);
  Changed;
end;

procedure TScriptModelKeyValueSection.Remove(const AIndex: Integer);
begin
  GetNamedLine(AIndex);
  FLines.Delete(AIndex);
  Changed;
end;

function TScriptModelKeyValueSection.FlagIncluded(const AIndex: Integer;
  const AFlagName: String): Boolean;
begin
  Result := ScriptValueIncludesFlag(GetNamedLine(AIndex).Value, AFlagName);
end;

procedure TScriptModelKeyValueSection.ApplyFlagRules(const AIndex: Integer;
  const AIncludedFlagName: String);
{ Like TScriptModelParameterSectionEntry.ApplyFlagRules, but a key's rules
  always target the key itself, so the rules work on the same line }
begin
  if FMetadata = nil then
    Exit;
  const Name = GetNamedLine(AIndex).Name;
  { Includes rules run in the forward direction only }
  for var Rule in FMetadata.FlagIncludesRules do begin
    if SameText(Rule.MemberName, Name) and
       SameText(Rule.FlagName, AIncludedFlagName) then begin
      for var ImpliedFlagName in Rule.OtherFlagNames do
        SetFlagInternal(AIndex, ImpliedFlagName, True);
    end;
  end;
  for var Rule in FMetadata.FlagExcludesRules do begin
    if SameText(Rule.MemberName, Name) then begin
      if SameText(Rule.FlagName, AIncludedFlagName) then begin
        { Forward: FlagName was included, exclude the other flags }
        for var ExcludedFlagName in Rule.OtherFlagNames do
          SetFlagInternal(AIndex, ExcludedFlagName, False);
      end else begin
        { Reverse: if a listed other flag was included, exclude
          FlagName, but not the other listed flags }
        for var ExcludedFlagName in Rule.OtherFlagNames do
          if SameText(ExcludedFlagName, AIncludedFlagName) then
            SetFlagInternal(AIndex, Rule.FlagName, False);
      end;
    end;
  end;
end;

procedure TScriptModelKeyValueSection.SetFlagInternal(const AIndex: Integer;
  const AFlagName: String; const AInclude: Boolean);
{ Includes or excludes a flag in an existing key's value. Including also
  runs the flag rules, so other flags could be turned on or off as well. }
begin
  { Sanity check }
  if not IsValidScriptFlagName(AFlagName) then
    raise EScriptModelError.Create('Internal error: Invalid flag name');

  const OldValue = GetNamedLine(AIndex).Value;
  const Found = ScriptValueIncludesFlag(OldValue, AFlagName);
  if AInclude then begin
    if Found then
      Exit;
    var NewValue := OldValue;
    if NewValue <> '' then
      NewValue := NewValue + ' ';
    SetValue(AIndex, NewValue + AFlagName);
    ApplyFlagRules(AIndex, AFlagName);
  end else begin
    if not Found then
      Exit;
    const NewValue = RemoveScriptFlagTokens(OldValue, AFlagName);
    if Trim(NewValue) = '' then
      Remove(AIndex) { Nothing left so remove the whole key/value }
    else
      SetValue(AIndex, NewValue);
  end;
end;

procedure TScriptModelKeyValueSection.SetFlag(const AIndex: Integer;
  const AFlagName: String; const AInclude: Boolean);
begin
  BeginUpdate;
  try
    SetFlagInternal(AIndex, AFlagName, AInclude);
  finally
    EndUpdate;
  end;
end;

function TScriptModelKeyValueSection.TryGetDefinition(const AName: String;
  out ADefinition: TMemberDefinition): Boolean;
begin
  Result := (FMetadata <> nil) and FMetadata.TryGetMember(AName, ADefinition);
end;

function TScriptModelKeyValueSection.TryGetValuePosition(const AIndex: Integer;
  out APosition: TValuePosition): Boolean;
{ Returns the start and end of the line's value, excluding whitespace
  surrounding it. Cannot be used after modification of the line: uses
  information from Parse. }
begin
  if (AIndex < 0) or (AIndex >= Count) then
    Exit(False);
  const Line = FLines[AIndex];
  if Line.FModified or (Line.Kind <> lkKeyValue) then
    Exit(False);
  DoGetValuePosition(Length(Line.FNameText) + 2, Line.FRawValue,
    Line.FLineStartOffsets, Line.FOriginalLines, APosition);
  Result := True;
end;

function TScriptModelKeyValueSection.DefaultValue(const AName: String): String;
begin
  Result := '';
  var Definition: TMemberDefinition;
  if TryGetDefinition(AName, Definition) then
    Result := Definition.DefaultValue;
end;

{ TCodeSectionRoutine }

constructor TCodeSectionRoutine.Create;
begin
  inherited Create;
  FParameters := TObjectList<TCodeSectionDeclaration>.Create;
  FLocals := TObjectList<TCodeSectionDeclaration>.Create;
end;

destructor TCodeSectionRoutine.Destroy;
begin
  FLocals.Free;
  FParameters.Free;
  inherited;
end;

function TCodeSectionRoutine.ParameterCount: Integer;
begin
  Result := Integer(FParameters.Count);
end;

function TCodeSectionRoutine.GetParameter(
  Index: Integer): TCodeSectionDeclaration;
begin
  Result := FParameters[Index];
end;

function TCodeSectionRoutine.LocalCount: Integer;
begin
  Result := Integer(FLocals.Count);
end;

function TCodeSectionRoutine.GetLocal(
  Index: Integer): TCodeSectionDeclaration;
begin
  Result := FLocals[Index];
end;

{ TCodeSectionInterfaceMethod }

constructor TCodeSectionInterfaceMethod.Create;
begin
  inherited Create;
  FParameters := TObjectList<TCodeSectionDeclaration>.Create;
end;

destructor TCodeSectionInterfaceMethod.Destroy;
begin
  FParameters.Free;
  inherited;
end;

function TCodeSectionInterfaceMethod.ParameterCount: Integer;
begin
  Result := Integer(FParameters.Count);
end;

function TCodeSectionInterfaceMethod.GetParameter(
  Index: Integer): TCodeSectionDeclaration;
begin
  Result := FParameters[Index];
end;

{ TScriptModelCodeSection }

function PrepareCodeSectionText(const ALines: array of String): AnsiString;
{ Prepares a [Code] section's lines for the ROPS tokenizer, matching
  TScriptCompiler.Compile: joins the lines with CRLF and UTF-8 encodes the
  result. ISPP directive lines are blanked first, keeping the line count.
  Spanned code lines are joined like TPreprocessor.InternalQueueLine does,
  onto the group's first line, with blank lines keeping the line count.
  Inline ISPP directives need no treatment because the tokenizer does not
  error on those (it sees them as comments). }
begin
  const Builder = TStringBuilder.Create;
  try
    var I := 0;
    while I <= High(ALines) do begin
      if I > 0 then
        Builder.Append(#13#10);
      const Line = ALines[I];
      if ClassifyScriptLine(Line) = slkISPPDirective then begin
        { Blank the directive line and its spanned continuation lines: an
          unblanked '#' would cost an error resync, and a spanned directive's
          continuation lines would be scanned as code }
        var Spans := ScriptLineSpans(Line);
        Inc(I);
        while Spans and (I <= High(ALines)) do begin
          Builder.Append(#13#10);
          Spans := ScriptLineSpans(ALines[I]);
          Inc(I);
        end;
      end else if ScriptLineSpans(Line) then begin
        { Join like TPreprocessor.InternalQueueLine }
        var Joined := TrimLeft(Copy(Line, 1, Length(Line)-1));
        var LineBreaks := 0;
        var Spans := True;
        Inc(I);
        while Spans and (I <= High(ALines)) do begin
          Spans := ScriptLineSpans(ALines[I]);
          if Spans then
            Joined := Joined + TrimLeft(Copy(ALines[I], 1, Length(ALines[I])-1))
          else
            Joined := Joined + TrimLeft(ALines[I]);
          Inc(LineBreaks);
          Inc(I);
        end;
        Builder.Append(Joined);
        for var J := 1 to LineBreaks do
          Builder.Append(#13#10);
      end else begin
        Builder.Append(Line);
        Inc(I);
      end;
    end;
    Result := Utf8Encode(Builder.ToString);
  finally
    Builder.Free;
  end;
end;

type
  TPSPascalParserHelper = class helper for TPSPascalParser
    function NextTokensAreRoutineName: Boolean;
  end;

function TPSPascalParserHelper.NextTokensAreRoutineName: Boolean;
{ Tests whether an identifier not followed by '=' comes after the current
  token, leaving the parser on the current one. An identifier followed by '='
  is the next type declaration's name instead. TPSPascalParser has no
  lookahead of its own, so its state is saved and put back. }
begin
  const SavedLastEnterPos = FLastEnterPos;
  const SavedRow = FRow;
  const SavedRealPosition = FRealPosition;
  const SavedTokenLength = FTokenLength;
  const SavedTokenID = FTokenId;
  const SavedToken = FToken;
  const SavedOriginalToken = FOriginalToken;
  Next;
  Result := CurrTokenID = CSTI_Identifier;
  if Result then begin
    Next;
    Result := CurrTokenID <> CSTI_Equal;
  end;
  FLastEnterPos := SavedLastEnterPos;
  FRow := SavedRow;
  FRealPosition := SavedRealPosition;
  FTokenLength := SavedTokenLength;
  FTokenId := SavedTokenID;
  FToken := SavedToken;
  FOriginalToken := SavedOriginalToken;
end;

constructor TScriptModelCodeSection.Create;
begin
  inherited Create;
  FRoutines := TObjectList<TCodeSectionRoutine>.Create;
  FTypes := TObjectList<TCodeSectionDeclaration>.Create;
  FEnumerationValues := TObjectList<TCodeSectionEnumerationValue>.Create;
  FInterfaceMethods := TObjectList<TCodeSectionInterfaceMethod>.Create;
  FConstants := TObjectList<TCodeSectionDeclaration>.Create;
  FGlobalVariables := TObjectList<TCodeSectionDeclaration>.Create;
end;

destructor TScriptModelCodeSection.Destroy;
begin
  FGlobalVariables.Free;
  FConstants.Free;
  FInterfaceMethods.Free;
  FEnumerationValues.Free;
  FTypes.Free;
  FRoutines.Free;
  inherited;
end;

procedure TScriptModelCodeSection.Parse(const ALines: array of String);

  function AdvanceWithSkip(const S: String; var I: Integer): Boolean;
  { Advances past the comment or whitespace at I and returns True, or past
    the content character or string literal at I and returns False }
  begin
    Result := True;
    if S[I] <= ' ' then begin
      { Whitespace }
      while (I <= Length(S)) and (S[I] <= ' ') do
        Inc(I);
    end else if S[I] = '{' then begin
      { Comment }
      while (I <= Length(S)) and (S[I] <> '}') do
        Inc(I);
      Inc(I);
    end else if (S[I] = '(') and (I < Length(S)) and (S[I+1] = '*') then begin
      (* Comment: the opener's '*' may close it, like the ROPS tokenizer *)
      Inc(I);
      while (I < Length(S)) and not ((S[I] = '*') and (S[I+1] = ')')) do
        Inc(I);
      Inc(I, 2);
    end else if (S[I] = '/') and (I < Length(S)) and (S[I+1] = '/') then begin
      // Comment
      while (I <= Length(S)) and not CharInSet(S[I], [#13, #10]) do
        Inc(I);
    end else begin
      Result := False;
      if S[I] = '''' then begin
        { String literal: skip containing comment chars. Does not need special
          handling for a string like 'hel''lo', that's just two of these loops. }
        repeat
          Inc(I);
        until (I > Length(S)) or (S[I] = '''');
      end;
      if I <= Length(S) then
        Inc(I);
    end;
  end;

  function SliceCleanText(const AText: AnsiString;
    const AStartPos, AEndPos: Integer): String;
  { Returns byte positions [AStartPos, AEndPos) of the tokenized buffer as a
    String, cleaned up for display: comments and whitespace both collapse to
    a single space, and are removed at the start, at the end, and before
    ')', ';', ',' and ']' }
  begin
    const S = UTF8ToString(Copy(AText, AStartPos+1, AEndPos-AStartPos));
    const Builder = TStringBuilder.Create;
    try
      var I := 1;
      var PendingSeparator := False;
      while I <= Length(S) do begin
        const StartI = I;
        if AdvanceWithSkip(S, I) then
          PendingSeparator := True { Wait till actual content, helps to merge consecutive comments and whitespace }
        else begin
          if PendingSeparator and (Builder.Length > 0) then
            if not CharInSet(S[StartI], [')', ';', ',', ']']) or
               ((Builder.Chars[Builder.Length-1] = '*') and (S[StartI] = ')')) then { Don't remove space between * and ) because it would make it look like a comment }
              Builder.Append(' ');
          PendingSeparator := False;
          Builder.Append(S, StartI-1, I-StartI); { Append is 0-based }
        end;
      end;
      Result := Builder.ToString;
    finally
      Builder.Free;
    end;
  end;

  function IsRoutineHeaderStart(const AParser: TPSPascalParser;
    const ALastTokenID: TPSPasToken): Boolean;
  { Tests the token the parser is on }
  begin
    Result := (AParser.CurrTokenID = CSTII_function) or (AParser.CurrTokenID = CSTII_procedure); { Local routines don't exist, so simple check }
    if Result and (ALastTokenID in [CSTI_Equal, CSTI_Colon, CSTII_of]) then begin
      { A function or procedure keyword after one of these tokens is part of
        a procedural type. ROPS never names one, so a routine name after it
        means the type is still being typed and a routine starts here. }
      Result := AParser.NextTokensAreRoutineName;
    end;
  end;

  function IsDeclarationBlockStart(const ATokenID: TPSPasToken): Boolean;
  begin
    Result := ATokenID in [CSTII_const, CSTII_type, CSTII_var, CSTII_Label];
  end;

  function IsParameterModifier(const ATokenID: TPSPasToken): Boolean;
  begin
    Result := ATokenID in [CSTII_const, CSTII_var];
  end;

  procedure ParseTypeBlock(const AParser: TPSPascalParser;
    const AText: AnsiString; const ALineOffset: Integer;
    var ALastTokenID: TPSPasToken;
    const AResumedAfterError: Boolean = False); forward;

  procedure ParseConstBlock(const AParser: TPSPascalParser;
    const ALineOffset: Integer; var ALastTokenID: TPSPasToken;
    const AResumedAfterError: Boolean = False); forward;

  procedure ParseVarBlock(const AParser: TPSPascalParser;
    const AText: AnsiString; const ALineOffset: Integer;
    const AVariables: TObjectList<TCodeSectionDeclaration>;
    var ALastTokenID: TPSPasToken;
    const AResumedAfterError: Boolean = False); forward;

  function TryParseDeclarationBlock(const AParser: TPSPascalParser;
    const AText: AnsiString; const ALineOffset: Integer;
    const AVarBlockVariables: TObjectList<TCodeSectionDeclaration>;
    var ALastTokenID: TPSPasToken;
    out AResumeBlockTokenID: TPSPasToken): Boolean;
  { Parses the block the parser is on, and returns False without moving the
    parser when it is not on a block this keeps declarations from. ROPS has
    no local 'type' or 'const' blocks, so those are always top-level ones.
    AVarBlockVariables is the list a 'var' block's variables are added to:
    a routine's locals when the block sits between its header and its
    'begin', and the global list otherwise.
    AResumeBlockTokenID is the block's keyword when the block ended on what
    may be a tokenizer error, and CSTI_EOF otherwise. }
  begin
    const BlockTokenID = AParser.CurrTokenID;
    AResumeBlockTokenID := CSTI_EOF;
    Result := True;
    case BlockTokenID of
      CSTII_type: ParseTypeBlock(AParser, AText, ALineOffset, ALastTokenID);
      CSTII_const: ParseConstBlock(AParser, ALineOffset, ALastTokenID);
      CSTII_var: ParseVarBlock(AParser, AText, ALineOffset, AVarBlockVariables,
        ALastTokenID);
    else
      Result := False;
    end;
    if Result and (AParser.CurrTokenID = CSTI_EOF) then
      AResumeBlockTokenID := BlockTokenID;
  end;

  type
    TParsedParameter = record
      Name: String;
      TypeText: String; { '' when untyped }
      Line: Integer;
    end;

    TParsedRoutineHeader = record
      Name: String;
      Kind: TCodeSectionRoutineKind;
      ResultTypeText: String;
      Prototype: String;
      Parameters: TArray<TParsedParameter>;
      FirstLine: Integer;
      Terminated: Boolean;
    end;

  procedure AddParsedParameters(const AText: AnsiString;
    const ANames: TArray<String>; const ANameLines: TArray<Integer>;
    const ATypeStartPos, ATypeEndPos: Integer;
    var AParameters: TArray<TParsedParameter>);
  begin
    var TypeText := '';
    if ATypeStartPos >= 0 then
      TypeText := SliceCleanText(AText, ATypeStartPos, ATypeEndPos);
    for var I := 0 to High(ANames) do begin
      var Parameter: TParsedParameter;
      Parameter.Name := ANames[I];
      Parameter.TypeText := TypeText;
      Parameter.Line := ANameLines[I];
      AParameters := AParameters + [Parameter];
    end;
  end;

  function ParseRoutineHeader(const AParser: TPSPascalParser;
    const AText: AnsiString; const ALineOffset: Integer;
    const AStopAtEnd: Boolean; var ALastTokenID: TPSPasToken;
    out AHeader: TParsedRoutineHeader): Boolean;
  begin
    const TokenID = AParser.CurrTokenID;
    const FirstLine = ALineOffset + Integer(AParser.Row)-1;
    const StartPos = Integer(AParser.CurrTokenPos);
    AParser.Next;
    if AParser.CurrTokenID <> CSTI_Identifier then begin
      ALastTokenID := TokenID;
      Exit(False);
    end;

    AHeader := Default(TParsedRoutineHeader);
    AHeader.Name := UTF8ToString(AParser.OriginalToken);
    if TokenID = CSTII_function then
      AHeader.Kind := rkFunction
    else
      AHeader.Kind := rkProcedure;
    AHeader.FirstLine := FirstLine;
    ALastTokenID := CSTI_Identifier;
    AParser.Next;

    { Parse the rest of the prototype until the terminating ';', remembering
      the position of a function's result type and collecting the parameter
      list's name groups on the way }
    var ParameterListSeen := False;
    var InParameterList := False;
    var BraceDepth := 0;
    var ResultTypeColonSeen := False;
    var ResultTypeStartPos := -1;
    var EndPos := -1;
    { Next 4 collect the group of names which share one type, as in 'A, B: Integer', reset each time }
    var ParameterNames: TArray<String> := [];
    var ParameterNameLines: TArray<Integer> := [];
    var ParameterColonSeen := False;
    var ParameterTypeStartPos := -1;
    while AParser.CurrTokenID <> CSTI_EOF do begin
      const PrototypeTokenID = AParser.CurrTokenID;
      if ResultTypeColonSeen and (ResultTypeStartPos < 0) then
        ResultTypeStartPos := Integer(AParser.CurrTokenPos);
      if ParameterColonSeen and (ParameterTypeStartPos < 0) then
        ParameterTypeStartPos := Integer(AParser.CurrTokenPos);
      { Unterminated: cut by a new declaration, its own 'begin', the 'end' of
        the interface it is a method of, or a declaration block. 'const' and
        'var' cut only outside parameter lists because they may be parameter
        modifiers; 'type' and 'label' never appear in one. }
      if IsRoutineHeaderStart(AParser, ALastTokenID) or
         (PrototypeTokenID = CSTII_begin) or
         (AStopAtEnd and (PrototypeTokenID = CSTII_end)) or
         (IsDeclarationBlockStart(PrototypeTokenID) and
          (not IsParameterModifier(PrototypeTokenID) or (BraceDepth = 0))) then
        Break;
      if PrototypeTokenID = CSTI_OpenRound then begin
        Inc(BraceDepth);
        { The first '(' before the result-type colon starts the parameter list;
          one after it belongs to a procedural result type }
        if (BraceDepth = 1) and not ParameterListSeen and
           not ResultTypeColonSeen then begin
          ParameterListSeen := True;
          InParameterList := True;
        end;
      end else if (PrototypeTokenID = CSTI_CloseRound) and (BraceDepth > 0) then begin
        Dec(BraceDepth);
        if InParameterList and (BraceDepth = 0) then begin
          { Parameter list done, add final found parameters }
          AddParsedParameters(AText, ParameterNames, ParameterNameLines,
            ParameterTypeStartPos, Integer(AParser.CurrTokenPos),
            AHeader.Parameters);
          InParameterList := False;
        end;
      end else if (PrototypeTokenID = CSTI_Colon) and (BraceDepth = 0) then
        ResultTypeColonSeen := True
      else if InParameterList and (BraceDepth = 1) then begin
        { Collect the group's names until its ':', after which its type runs
          to the group's ';' or the list's ')'. The 'const', 'var' and 'out'
          modifiers fall through, keeping them out of name and type. }
        if PrototypeTokenID = CSTI_Identifier then begin
          if not ParameterColonSeen then begin
            ParameterNames := ParameterNames + [UTF8ToString(AParser.OriginalToken)];
            ParameterNameLines := ParameterNameLines + [ALineOffset + Integer(AParser.Row)-1];
          end;
        end else if PrototypeTokenID = CSTI_Colon then
          ParameterColonSeen := True
        else if PrototypeTokenID = CSTI_Semicolon then begin
          { Parameter group done, add found parameters }
          AddParsedParameters(AText, ParameterNames, ParameterNameLines,
            ParameterTypeStartPos, Integer(AParser.CurrTokenPos),
            AHeader.Parameters);
          { Reset for next group }
          ParameterNames := [];
          ParameterNameLines := [];
          ParameterColonSeen := False;
          ParameterTypeStartPos := -1;
        end;
      end;
      { Known limitation: for a function using an inline structured result type
        such as 'function F: record A: Integer; end;' it takes the first ';'
        as the end of the type, truncating Prototype and ResultTypeText.
        The body is still found: the 'begin' search skips 'end;'. }
      const Terminated = (PrototypeTokenID = CSTI_Semicolon) and (BraceDepth = 0);
      if Terminated then
        EndPos := Integer(AParser.CurrTokenPos)+1; { Skip ';' }
      ALastTokenID := PrototypeTokenID;
      AParser.Next;
      if Terminated then
        Break;
    end;
    if InParameterList then begin
      { Header cut inside parameter list, don't forget to add found parameters }
      AddParsedParameters(AText, ParameterNames, ParameterNameLines,
        ParameterTypeStartPos, Integer(AParser.CurrTokenPos), AHeader.Parameters);
    end;

    var ResultTypeEndPos: Integer;
    AHeader.Terminated := EndPos >= 0;
    if AHeader.Terminated then
      ResultTypeEndPos := EndPos-1 { Move back before ';' }
    else begin
      { Malformed or unterminated header; keep what is there }
      EndPos := Integer(AParser.CurrTokenPos);
      ResultTypeEndPos := EndPos;
    end;
    AHeader.Prototype := SliceCleanText(AText, StartPos, EndPos);
    if (AHeader.Kind = rkFunction) and (ResultTypeStartPos >= 0) then
      AHeader.ResultTypeText := SliceCleanText(AText, ResultTypeStartPos, ResultTypeEndPos);
    Result := True;
  end;

  procedure RoutineHeaderParametersToRoutineParameters(const AHeader: TParsedRoutineHeader;
    const AParameters: TObjectList<TCodeSectionDeclaration>);
  begin
    for var HeaderParameter in AHeader.Parameters do begin
      const Declaration = TCodeSectionDeclaration.Create;
      AParameters.Add(Declaration);
      Declaration.FName := HeaderParameter.Name;
      Declaration.FTypeText := HeaderParameter.TypeText;
      Declaration.FLine := HeaderParameter.Line;
    end;
  end;

  procedure ParseRoutine(const AParser: TPSPascalParser;
    const AText: AnsiString; const ALineOffset: Integer;
    var ALastTokenID: TPSPasToken; out AOpenRoutine: TCodeSectionRoutine;
    out ABeginFound: Boolean; out AResumeBlockTokenID: TPSPasToken);
  { AOpenRoutine equals the added routine when its body 'end' was not found
    due to a tokenizer error or due to reaching EOF. Otherwise it equals nil.
    ABeginFound tells whether the body's 'begin' was found.
    AResumeBlockTokenID is as in TryParseDeclarationBlock, for a block found
    while searching for the body. }
  begin
    AOpenRoutine := nil;
    ABeginFound := False;
    AResumeBlockTokenID := CSTI_EOF;
    var Header: TParsedRoutineHeader;
    if ParseRoutineHeader(AParser, AText, ALineOffset, False, ALastTokenID,
         Header) then begin
      const Routine = TCodeSectionRoutine.Create;
      FRoutines.Add(Routine);
      Routine.FBodyFirstLine := -1;
      Routine.FLastLine := -1;
      Routine.FName := Header.Name;
      Routine.FKind := Header.Kind;
      Routine.FFirstLine := Header.FirstLine;
      Routine.FPrototype := Header.Prototype;
      Routine.FResultTypeText := Header.ResultTypeText;
      RoutineHeaderParametersToRoutineParameters(Header, Routine.FParameters);

      if Header.Terminated or
         (AParser.CurrTokenID = CSTII_begin) or IsDeclarationBlockStart(AParser.CurrTokenID) then begin { A header cut by its own 'begin' or a declaration block still gets its body searched for and parsed }
        { Handle trailing decoration }
        var DecorationLastLine := -1;
        while AParser.CurrTokenID in [CSTII_Forward, CSTII_External, CSTII_Export] do begin
          if AParser.CurrTokenID = CSTII_Forward then
            Routine.FBodilessType := btForward
          else if AParser.CurrTokenID = CSTII_External then
            Routine.FBodilessType := btExternal;
          ALastTokenID := AParser.CurrTokenID;
          DecorationLastLine := ALineOffset + Integer(AParser.Row)-1;
          AParser.Next;
          { Consume the rest of the decoration until its ';'. A declaration
            block ends it instead of being consumed: it is never part of a
            decoration, and the main loop parses it. }
          while (AParser.CurrTokenID <> CSTI_EOF) and
                not IsRoutineHeaderStart(AParser, ALastTokenID) do begin
            if IsDeclarationBlockStart(AParser.CurrTokenID) then
              Break;
            const DecorationTokenID = AParser.CurrTokenID;
            ALastTokenID := DecorationTokenID;
            DecorationLastLine := ALineOffset + Integer(AParser.Row)-1;
            AParser.Next;
            if DecorationTokenID = CSTI_Semicolon then
              Break;
          end;
        end;
        { Search for the body and parse it }
        if Routine.FBodilessType <> btNo then
          Routine.FLastLine := DecorationLastLine
        else begin
          { Search for 'begin'. A block on the way whose declarations are
            kept is parsed rather than skipped, a 'var' block into the
            routine's locals, so they survive a body still being typed. The
            search goes on past the block either way, because the 'begin'
            may still follow. }
          while (AParser.CurrTokenID <> CSTI_EOF) and
                (AParser.CurrTokenID <> CSTII_begin) and
                not IsRoutineHeaderStart(AParser, ALastTokenID) do begin
            if not TryParseDeclarationBlock(AParser, AText, ALineOffset,
                     Routine.FLocals, ALastTokenID, AResumeBlockTokenID) then begin
              ALastTokenID := AParser.CurrTokenID;
              AParser.Next;
            end;
          end;
          if AParser.CurrTokenID = CSTII_begin then begin
            ABeginFound := True;
            { Search for matching 'end' }
            Routine.FBodyFirstLine := ALineOffset + Integer(AParser.Row)-1;
            ALastTokenID := CSTII_begin;
            AParser.Next;
            var BlockDepth := 1;
            while (AParser.CurrTokenID <> CSTI_EOF) and
                  not IsRoutineHeaderStart(AParser, ALastTokenID) and
                  not IsDeclarationBlockStart(AParser.CurrTokenID) do begin
              const BodyTokenID = AParser.CurrTokenID;
              if BodyTokenID in [CSTII_begin, CSTII_case, CSTII_Try] then
                Inc(BlockDepth)
              else if BodyTokenID = CSTII_end then begin
                Dec(BlockDepth);
                if BlockDepth = 0 then
                  Routine.FLastLine := ALineOffset + Integer(AParser.Row)-1;
              end;
              ALastTokenID := BodyTokenID;
              AParser.Next;
              if BlockDepth = 0 then
                Break;
            end;
          end;
        end;
      end;
      if Routine.FLastLine < 0 then begin
        { No body 'end' found: take the line before the next declaration,
          or to the section's last line. This way a body still being
          typed still reports the routine. }
        if AParser.CurrTokenID = CSTI_EOF then begin { Could be tokenizer error }
          Routine.FLastLine := Integer(High(ALines));
          AOpenRoutine := Routine;
        end else begin
          Routine.FLastLine := ALineOffset + Integer(AParser.Row)-2;
          if Routine.FLastLine < Routine.FFirstLine then
            Routine.FLastLine := Routine.FFirstLine;
          if Routine.FLastLine < Routine.FBodyFirstLine then
            Routine.FLastLine := Routine.FBodyFirstLine; { A cut on the 'begin' line itself }
        end;
      end;
    end;
  end;

  type
    TPendingDeclaration = record
      Name: String; { Empty when there is none }
      Line: Integer;
    end;

  function BlockHasNextDeclaration(const AParser: TPSPascalParser;
    const APendingDeclaration: TPendingDeclaration;
    var ALastTokenID: TPSPasToken): Boolean;
  { Advances a declaration block to its next declaration, and returns False
    at the block's end. Tokens that cannot start a declaration are skipped,
    so a malformed one does not hide the finished ones below it. }
  begin
    if APendingDeclaration.Name <> '' then
      Exit(True);
    while AParser.CurrTokenID <> CSTI_Identifier do begin
      if (AParser.CurrTokenID = CSTI_EOF) or
         (AParser.CurrTokenID = CSTII_begin) or
         IsDeclarationBlockStart(AParser.CurrTokenID) or
         IsRoutineHeaderStart(AParser, ALastTokenID) then
        Exit(False);
      ALastTokenID := AParser.CurrTokenID;
      AParser.Next;
    end;
    Result := True;
  end;

  procedure ParseEnumerationValues(const AParser: TPSPascalParser;
    const ALineOffset, ATypeIndex: Integer; var ALastTokenID: TPSPasToken;
    var APendingDeclaration: TPendingDeclaration);
  { Parses an enumeration's value list, starting on its '(' and consuming the
    closing ')' so the caller's brace depth stays balanced. ROPS allows nothing
    but identifiers separated by commas, so any other token means the list is
    still being typed and what follows it is not part of it. }
  begin
    repeat
      ALastTokenID := AParser.CurrTokenID; { The '(' or ',' }
      AParser.Next;
      if AParser.CurrTokenID <> CSTI_Identifier then
        Break;
      const Name = UTF8ToString(AParser.OriginalToken);
      const Line = ALineOffset + Integer(AParser.Row)-1;
      ALastTokenID := CSTI_Identifier;
      AParser.Next;
      if AParser.CurrTokenID = CSTI_Equal then begin
        { Not a value after all but the next type's name }
        APendingDeclaration.Name := Name;
        APendingDeclaration.Line := Line;
        Exit;
      end;
      const Value = TCodeSectionEnumerationValue.Create;
      FEnumerationValues.Add(Value);
      Value.FName := Name;
      Value.FDeclarationTypeIndex := ATypeIndex;
      Value.FLine := Line;
    until AParser.CurrTokenID <> CSTI_Comma;
    if AParser.CurrTokenID = CSTI_CloseRound then begin
      ALastTokenID := CSTI_CloseRound;
      AParser.Next;
    end;
  end;

  procedure ParseTypeBlock(const AParser: TPSPascalParser;
    const AText: AnsiString; const ALineOffset: Integer;
    var ALastTokenID: TPSPasToken; const AResumedAfterError: Boolean = False);
  { Parses a type block. A declaration starts at an identifier followed by
    '=', so a definition still being typed is ended by the next one instead
    of swallowing it.
    Known limitation: an inline 'interface' type elsewhere (ROPS allows one in
    a var declaration) is not consumed, so its methods are seen as routines. }
  begin
    if not AResumedAfterError then begin
      ALastTokenID := AParser.CurrTokenID;
      AParser.Next;
    end;
    var PendingDeclaration: TPendingDeclaration;
    PendingDeclaration.Name := '';
    while BlockHasNextDeclaration(AParser, PendingDeclaration, ALastTokenID) do begin
      var Name: String;
      var Line: Integer;
      if PendingDeclaration.Name <> '' then begin
        Name := PendingDeclaration.Name;
        Line := PendingDeclaration.Line;
        PendingDeclaration.Name := '';
      end else begin
        Name := UTF8ToString(AParser.OriginalToken);
        Line := ALineOffset + Integer(AParser.Row)-1;
        ALastTokenID := CSTI_Identifier;
        AParser.Next;
      end;
      if AParser.CurrTokenID <> CSTI_Equal then
        Continue; { Not a declaration: the identifier after it may still be one }
      ALastTokenID := CSTI_Equal;
      AParser.Next;

      { Add type with its name, line and type }
      const Declaration = TCodeSectionDeclaration.Create;
      const DeclarationIndex = Integer(FTypes.Add(Declaration));
      Declaration.FName := Name;
      Declaration.FLine := Line;
      case AParser.CurrTokenID of
        CSTII_record: Declaration.FTypeText := 'record';
        CSTII_interface: Declaration.FTypeText := 'interface';
        CSTII_array: Declaration.FTypeText := 'array';
        CSTII_set: Declaration.FTypeText := 'set';
        CSTII_procedure: Declaration.FTypeText := 'procedure';
        CSTII_function: Declaration.FTypeText := 'function';
        CSTI_OpenRound: Declaration.FTypeText := 'enumeration';
        CSTI_Identifier: Declaration.FTypeText := UTF8ToString(AParser.OriginalToken);
      end;

      { Parse the rest of the definition, remembering only its interface
        methods and enumeration values }
      var BraceDepth := 0;
      var OpenStructs: TArray<TPSPasToken> := []; { CSTII_record/CSTII_interface, innermost last }
      while AParser.CurrTokenID <> CSTI_EOF do begin
        const DefinitionTokenID = AParser.CurrTokenID;
        if IsRoutineHeaderStart(AParser, ALastTokenID) then begin
          { Ends an unterminated definition, unless directly in an interface:
            its methods are indistinguishable from routine headers }
          const InOpenInterface = (Length(OpenStructs) > 0) and
            (OpenStructs[High(OpenStructs)] = CSTII_interface);
          if not InOpenInterface then
            Break;
          { Trailing decoration such as 'safecall' is left out of the prototype }
          var Header: TParsedRoutineHeader;
          if ParseRoutineHeader(AParser, AText, ALineOffset, True, ALastTokenID,
               Header) then begin
            const Method = TCodeSectionInterfaceMethod.Create;
            FInterfaceMethods.Add(Method);
            Method.FName := Header.Name;
            Method.FKind := Header.Kind;
            Method.FDeclarationTypeIndex := DeclarationIndex;
            Method.FResultTypeText := Header.ResultTypeText;
            Method.FPrototype := Header.Prototype;
            Method.FLine := Header.FirstLine;
            RoutineHeaderParametersToRoutineParameters(Header, Method.FParameters);
          end;
          Continue;
        end else if IsDeclarationBlockStart(DefinitionTokenID) and
                    (BraceDepth = 0) then begin
          { Also ends an unterminated definition, with no interface exemption:
            a block start is never an interface member. The depth guard
            keeps a procedural type's 'var'/'const' parameters out. }
          Break;
        end else if (DefinitionTokenID = CSTI_OpenRound) and
                    (ALastTokenID in [CSTI_Equal, CSTI_Colon, CSTII_of]) then begin
          { A '(' where a type is expected is an enumeration's value list. An
            anonymous enumeration's values belong to the type being declared,
            like an anonymous interface's methods do. }
          ParseEnumerationValues(AParser, ALineOffset, DeclarationIndex,
            ALastTokenID, PendingDeclaration);
          if PendingDeclaration.Name <> '' then
            Break;
          Continue;
        end else if DefinitionTokenID = CSTI_Identifier then begin
          { An identifier followed by '=' is the next declaration, so it ends
            this one, also while a record, an interface or a parameter list is
            still open: ROPS takes '=' only in a type or const declaration and
            in an expression, and a type definition holds neither. }
          const NextName = UTF8ToString(AParser.OriginalToken);
          const NextLine = ALineOffset + Integer(AParser.Row)-1;
          ALastTokenID := CSTI_Identifier;
          AParser.Next;
          if AParser.CurrTokenID = CSTI_Equal then begin
            PendingDeclaration.Name := NextName;
            PendingDeclaration.Line := NextLine;
            Break;
          end;
          Continue;
        end;
        if DefinitionTokenID in [CSTII_record, CSTII_interface] then
          OpenStructs := OpenStructs + [DefinitionTokenID]
        else if (DefinitionTokenID = CSTII_end) and (Length(OpenStructs) > 0) then
          SetLength(OpenStructs, Length(OpenStructs)-1)
        else if DefinitionTokenID = CSTI_OpenRound then
          Inc(BraceDepth)
        else if (DefinitionTokenID = CSTI_CloseRound) and (BraceDepth > 0) then
          Dec(BraceDepth);
        const Terminated = (DefinitionTokenID = CSTI_Semicolon) and
          (BraceDepth = 0) and (Length(OpenStructs) = 0);
        ALastTokenID := DefinitionTokenID;
        AParser.Next;
        if Terminated then
          Break;
      end;
    end;
  end;

  procedure ParseConstBlock(const AParser: TPSPascalParser;
    const ALineOffset: Integer; var ALastTokenID: TPSPasToken;
    const AResumedAfterError: Boolean = False);
  { Parses a const block, just like ParseTypeBlock. Also infers the type of
    the constant. }
  begin
    if not AResumedAfterError then begin
      ALastTokenID := AParser.CurrTokenID;
      AParser.Next;
    end;
    var PendingDeclaration: TPendingDeclaration;
    PendingDeclaration.Name := '';
    while BlockHasNextDeclaration(AParser, PendingDeclaration, ALastTokenID) do begin
      var Name: String;
      var Line: Integer;
      if PendingDeclaration.Name <> '' then begin
        Name := PendingDeclaration.Name;
        Line := PendingDeclaration.Line;
        PendingDeclaration.Name := '';
      end else begin
        Name := UTF8ToString(AParser.OriginalToken);
        Line := ALineOffset + Integer(AParser.Row)-1;
        ALastTokenID := CSTI_Identifier;
        AParser.Next;
      end;
      if AParser.CurrTokenID <> CSTI_Equal then
        Continue; { Not a declaration: the identifier after it may still be one }
      const EqualLine = ALineOffset + Integer(AParser.Row)-1;
      ALastTokenID := CSTI_Equal;
      AParser.Next;

      { Add constant with its name and line }
      const Declaration = TCodeSectionDeclaration.Create;
      FConstants.Add(Declaration);
      Declaration.FName := Name;
      Declaration.FLine := Line;

      { Scan the value for the operators and operand kinds it uses. Their
        order does not affect the type, so no expression tree is needed. A
        const value cannot hold a parameter list, so unlike elsewhere no
        bracket depth is tracked. }
      var HasComparison := False;
      var HasIntegerOnlyOperator := False;
      var HasIntegerOperand := False;
      var HasExtendedOperand := False;
      var HasStringOperand := False;
      var HasBooleanOperand := False;
      var Inferable := True;
      while AParser.CurrTokenID <> CSTI_EOF do begin
        const ValueTokenID = AParser.CurrTokenID;
        if IsRoutineHeaderStart(AParser, ALastTokenID) or
           (ValueTokenID = CSTII_begin) or
           IsDeclarationBlockStart(ValueTokenID) then
          Break; { Unterminated: cut by a new declaration, a 'begin', or a declaration block }
        if ValueTokenID = CSTI_Identifier then begin
          { An identifier followed by '=' is the next declaration, so it ends
            an unterminated value. Unlike in a type block the '=' can also be
            a comparison, as in 'IsOne = A = 1'. Only a name below the line
            the value starts on is taken as the next declaration, so a
            comparison written on one line stays one. }
          const NextName = UTF8ToString(AParser.OriginalToken);
          const NextLine = ALineOffset + Integer(AParser.Row)-1;
          ALastTokenID := CSTI_Identifier;
          AParser.Next;
          if (AParser.CurrTokenID = CSTI_Equal) and (NextLine > EqualLine) then begin
            PendingDeclaration.Name := NextName;
            PendingDeclaration.Line := NextLine;
            Break;
          end;
          if SameText(NextName, 'True') or SameText(NextName, 'False') then
            HasBooleanOperand := True
          else begin
            { If an already declared constant is used, lookup its type.
              On dplicate names (invalid in ROPS), we just take the newest. }
            var FoundTypeText := '';
            for var I := FConstants.Count-2 downto 0 do begin
              if SameText(FConstants[I].Name, NextName) then begin
                FoundTypeText := FConstants[I].TypeText;
                Break;
              end;
            end;
            if FoundTypeText = 'Integer' then
              HasIntegerOperand := True
            else if FoundTypeText = 'Extended' then
              HasExtendedOperand := True
            else if FoundTypeText = 'String' then
              HasStringOperand := True
            else if FoundTypeText = 'Boolean' then
              HasBooleanOperand := True
            else
              Inferable := False;
          end;
          Continue;
        end;
        const Terminated = (ValueTokenID = CSTI_Semicolon);
        if not Terminated then begin
          case ValueTokenID of
            CSTI_Integer, CSTI_HexInt: HasIntegerOperand := True;
            CSTI_Real: HasExtendedOperand := True;
            CSTI_String, CSTI_Char: HasStringOperand := True;
            CSTI_Equal, CSTI_NotEqual, CSTI_Greater, CSTI_GreaterEqual,
            CSTI_Less, CSTI_LessEqual: HasComparison := True;
            CSTII_mod, CSTII_shl, CSTII_shr: HasIntegerOnlyOperator := True;
            CSTI_Plus, CSTI_Minus, CSTI_Multiply, CSTI_Divide, CSTII_div,
            CSTII_and, CSTII_or, CSTII_xor, CSTII_not, CSTI_OpenRound,
            CSTI_CloseRound: ; { Takes its operands' type, and so do '/' and 'div': without PS_DELPHIDIV, ROPS divides two integers into an integer. }
          else
            Inferable := False;
          end;
        end;
        ALastTokenID := ValueTokenID;
        AParser.Next;
        if Terminated then
          Break;
      end;
      if HasComparison then
        { A comparison is 'Boolean' whatever its operands, also unresolved
          ones: ROPS makes it the outermost operator }
        Declaration.FTypeText := 'Boolean'
      else if Inferable then begin
        { mod, shl and shr take integers only. Otherwise all operands must be
          of one type, except that an integer next to a real widens to
          'Extended'. An operand mix no operator accepts gets no type. }
        if HasIntegerOnlyOperator then begin
          if HasIntegerOperand and
             not (HasExtendedOperand or HasStringOperand or HasBooleanOperand) then
            Declaration.FTypeText := 'Integer';
        end else if HasStringOperand then begin
          if not (HasIntegerOperand or HasExtendedOperand or HasBooleanOperand) then
            Declaration.FTypeText := 'String';
        end else if HasBooleanOperand then begin
          if not (HasIntegerOperand or HasExtendedOperand) then
            Declaration.FTypeText := 'Boolean';
        end else if HasExtendedOperand then
          Declaration.FTypeText := 'Extended'
        else if HasIntegerOperand then
          Declaration.FTypeText := 'Integer';
      end;
    end;
  end;

  procedure ParseVarBlock(const AParser: TPSPascalParser;
    const AText: AnsiString; const ALineOffset: Integer;
    const AVariables: TObjectList<TCodeSectionDeclaration>;
    var ALastTokenID: TPSPasToken; const AResumedAfterError: Boolean = False);
  { Parses a var block, adding its variables to AVariables: the global list
    or a routine's locals. Known limitation: an inline structured type such
    as 'record A: Integer; end;' is not consumed as a whole, so its fields
    become groups of their own. }
  begin
    if not AResumedAfterError then begin
      ALastTokenID := AParser.CurrTokenID;
      AParser.Next;
    end;
    var PendingDeclaration: TPendingDeclaration;
    PendingDeclaration.Name := '';
    while BlockHasNextDeclaration(AParser, PendingDeclaration, ALastTokenID) do begin
      { Collect the group of names which share one type, as in 'A, B: Integer' }
      var Names: TArray<String> := [];
      var NameLines: TArray<Integer> := [];
      while True do begin
        if PendingDeclaration.Name <> '' then begin
          Names := Names + [PendingDeclaration.Name];
          NameLines := NameLines + [PendingDeclaration.Line];
          PendingDeclaration.Name := '';
        end else if AParser.CurrTokenID = CSTI_Identifier then begin
          Names := Names + [UTF8ToString(AParser.OriginalToken)];
          NameLines := NameLines + [ALineOffset + Integer(AParser.Row)-1];
          ALastTokenID := CSTI_Identifier;
          AParser.Next;
        end else
          Break; { The group is still being typed }
        if AParser.CurrTokenID <> CSTI_Comma then
          Break;
        ALastTokenID := CSTI_Comma;
        AParser.Next;
      end;

      { Parse the group's type }
      var TypeStartPos := -1;
      var TypeEndPos := -1;
      if AParser.CurrTokenID = CSTI_Colon then begin
        ALastTokenID := CSTI_Colon;
        AParser.Next;
        TypeStartPos := Integer(AParser.CurrTokenPos);
        var RoundDepth := 0;
        var SquareDepth := 0;
        while AParser.CurrTokenID <> CSTI_EOF do begin
          const TypeTokenID = AParser.CurrTokenID;
          if IsRoutineHeaderStart(AParser, ALastTokenID) or
             (TypeTokenID = CSTII_begin) or
             (IsDeclarationBlockStart(TypeTokenID) and
              (not IsParameterModifier(TypeTokenID) or (RoundDepth = 0))) then begin
            TypeEndPos := Integer(AParser.CurrTokenPos);
            Break; { Unterminated: cut like a routine header. The modifiers cut only outside parameter lists; brackets never hold a block keyword. }
          end;
          if (TypeTokenID = CSTI_Identifier) and (RoundDepth = 0) then begin
            const NextName = UTF8ToString(AParser.OriginalToken);
            const NextLine = ALineOffset + Integer(AParser.Row)-1;
            const NextStartPos = Integer(AParser.CurrTokenPos);
            ALastTokenID := CSTI_Identifier;
            AParser.Next;
            if (AParser.CurrTokenID = CSTI_Colon) or
               ((AParser.CurrTokenID = CSTI_Comma) and (SquareDepth = 0)) then begin
              { The identifier is the next group's first name, so it ends an
                unterminated type. A ':' is never legal inside brackets, so an
                unclosed '[' does not block that cut; a ',' is (array bounds). }
              PendingDeclaration.Name := NextName;
              PendingDeclaration.Line := NextLine;
              TypeEndPos := NextStartPos;
              Break;
            end;
            Continue;
          end;
          if TypeTokenID = CSTI_OpenRound then
            Inc(RoundDepth)
          else if (TypeTokenID = CSTI_CloseRound) and (RoundDepth > 0) then
            Dec(RoundDepth)
          else if TypeTokenID = CSTI_OpenBlock then
            Inc(SquareDepth)
          else if (TypeTokenID = CSTI_CloseBlock) and (SquareDepth > 0) then
            Dec(SquareDepth);
          const Terminated = (TypeTokenID = CSTI_Semicolon) and (RoundDepth = 0);
          if Terminated then
            TypeEndPos := Integer(AParser.CurrTokenPos);
          ALastTokenID := TypeTokenID;
          AParser.Next;
          if Terminated then
            Break;
        end;
        if TypeEndPos < 0 then
          TypeEndPos := Integer(AParser.CurrTokenPos); { Unterminated at the end: keep what is there }
      end;

      { Add one variable per name, with the group's type }
      for var I := 0 to High(Names) do begin
        const Declaration = TCodeSectionDeclaration.Create;
        AVariables.Add(Declaration);
        Declaration.FName := Names[I];
        Declaration.FLine := NameLines[I];
        if TypeStartPos >= 0 then
          Declaration.FTypeText := SliceCleanText(AText, TypeStartPos, TypeEndPos);
      end;
    end;
  end;

  function VarBlockVariables(const AOpenRoutine: TCodeSectionRoutine):
    TObjectList<TCodeSectionDeclaration>;
  { The list a 'var' block parses into: the open routine's locals while its
    'begin' is still missing, and the global list otherwise }
  begin
    if AOpenRoutine <> nil then
      Result := AOpenRoutine.FLocals
    else
      Result := FGlobalVariables;
  end;

begin
  FRoutines.Clear;
  FTypes.Clear;
  FEnumerationValues.Clear;
  FInterfaceMethods.Clear;
  FConstants.Clear;
  FGlobalVariables.Clear;

  var Text := PrepareCodeSectionText(ALines);
  const Parser = TPSPascalParser.Create;
  try
    Parser.SetText(Text);
    var LineOffset := 0; { Line index of the buffer's first line, advanced by each resync below }
    var LastTokenID := CSTI_EOF;
    var ResumeBlockTokenID := CSTI_EOF; { CSTII_type/const/var when that block was cut by a tokenizer error }
    var OpenRoutine: TCodeSectionRoutine := nil;
    var OpenRoutineBeginFound := False;
    while True do begin

      { Important goal: Finished code below code being typed should not get lost
        while typing, whenever possible. This applies both to a type definition
        still being typed and to a routine whose body is still missing.

        Also: Parser simplicity is more important than exactly matching the ROPS
        grammar. For example, ROPS accepts anonymous types in some contexts but
        not others, without any clear reason why. The parser therefore reproduces
        such distinctions only when doing so simplifies its code. }

      while Parser.CurrTokenID <> CSTI_EOF do begin { CSTI_EOF means error or EOF, told apart below }
        const TokenID = Parser.CurrTokenID;
        const RoutineHeaderStart = IsRoutineHeaderStart(Parser, LastTokenID);
        { A declaration block start ends the open routine only when its
          'begin' was found: before it the 'begin' may still follow the
          block, like in ParseRoutine's own search for it }
        const EndsOpenRoutine = RoutineHeaderStart or
          (OpenRoutineBeginFound and IsDeclarationBlockStart(TokenID));
        if (OpenRoutine <> nil) and EndsOpenRoutine then begin
          OpenRoutine.FLastLine := LineOffset + Integer(Parser.Row)-2;
          OpenRoutine := nil;
        end;
        { The resync restarts at top-level context, so a routine still
          searching for its body finds its 'begin' here. Its body has
          started, so a 'var' block below is a top-level one and not the
          routine's own. }
        if (OpenRoutine <> nil) and (TokenID = CSTII_begin) then
          OpenRoutineBeginFound := True;
        { A block ending at EOF could be a tokenizer error: the resync below
          then resumes the block }
        if RoutineHeaderStart then
          ParseRoutine(Parser, Text, LineOffset, LastTokenID, OpenRoutine,
            OpenRoutineBeginFound, ResumeBlockTokenID)
        else if not TryParseDeclarationBlock(Parser, Text, LineOffset,
                    VarBlockVariables(OpenRoutine), LastTokenID, ResumeBlockTokenID) then begin
          LastTokenID := TokenID;
          Parser.Next;
        end;
      end;

      { On a tokenizer error CurrTokenPos is still at the errored token,
        instead of at the end of the text }
      if Integer(Parser.CurrTokenPos) >= Length(Text) then
        Break;
      const ErrorPos = Integer(Parser.CurrTokenPos);

      { '{' or '(' means there was just an unterminated comment }
      if CharInSet(Text[ErrorPos+1], ['{', '(']) then
        Break;

      { Resync code starts from here.

        Known limitation: when a tokenizer error cut a routine before its
        'begin', that 'begin' and its 'end' are never taken as the routine's
        body, so BodyFirstLine stays -1. Fixing that would add too much
        complexity for little gain. }

      { Some other error: keep what was found so far, and search for
        start of the next line }
      var ResyncPos := ErrorPos+1;
      while (ResyncPos <= Length(Text)) and (Text[ResyncPos] <> #10) do
        Inc(ResyncPos);
      if ResyncPos >= Length(Text) then
        Break; { No next line }

      { Restart parse, at top-level context: after an error inside an
        interface, its remaining methods are therefore seen as routines }
      Inc(LineOffset, Integer(Parser.Row));
      Text := Copy(Text, ResyncPos+1, MaxInt);
      Parser.SetText(Text);
      LastTokenID := CSTI_EOF;

      { The restart would skip a cut block's remaining declarations: they
        follow no block keyword. So resume the block. Stays set when the
        resumed block errors right away, for the next resync. }
      if ResumeBlockTokenID <> CSTI_EOF then begin
        case ResumeBlockTokenID of
          CSTII_type: ParseTypeBlock(Parser, Text, LineOffset, LastTokenID, True);
          CSTII_const: ParseConstBlock(Parser, LineOffset, LastTokenID, True);
          CSTII_var: ParseVarBlock(Parser, Text, LineOffset,
            VarBlockVariables(OpenRoutine), LastTokenID, True);
        end;
        if Parser.CurrTokenID <> CSTI_EOF then
          ResumeBlockTokenID := CSTI_EOF;
      end;
    end;
  finally
    Parser.Free;
  end;
end;

function TScriptModelCodeSection.RoutineCount: Integer;
begin
  Result := Integer(FRoutines.Count);
end;

function TScriptModelCodeSection.GetRoutine(
  Index: Integer): TCodeSectionRoutine;
begin
  Result := FRoutines[Index];
end;

function TScriptModelCodeSection.TypeCount: Integer;
begin
  Result := Integer(FTypes.Count);
end;

function TScriptModelCodeSection.GetType(
  Index: Integer): TCodeSectionDeclaration;
begin
  Result := FTypes[Index];
end;

function TScriptModelCodeSection.EnumerationValueCount: Integer;
begin
  Result := Integer(FEnumerationValues.Count);
end;

function TScriptModelCodeSection.GetEnumerationValue(
  Index: Integer): TCodeSectionEnumerationValue;
begin
  Result := FEnumerationValues[Index];
end;

function TScriptModelCodeSection.InterfaceMethodCount: Integer;
begin
  Result := Integer(FInterfaceMethods.Count);
end;

function TScriptModelCodeSection.GetInterfaceMethod(
  Index: Integer): TCodeSectionInterfaceMethod;
begin
  Result := FInterfaceMethods[Index];
end;

function TScriptModelCodeSection.ConstantCount: Integer;
begin
  Result := Integer(FConstants.Count);
end;

function TScriptModelCodeSection.GetConstant(
  Index: Integer): TCodeSectionDeclaration;
begin
  Result := FConstants[Index];
end;

function TScriptModelCodeSection.GlobalVariableCount: Integer;
begin
  Result := Integer(FGlobalVariables.Count);
end;

function TScriptModelCodeSection.GetGlobalVariable(
  Index: Integer): TCodeSectionDeclaration;
begin
  Result := FGlobalVariables[Index];
end;

function TScriptModelCodeSection.TryGetRoutine(const ALine: Integer;
  out ARoutine: TCodeSectionRoutine; const AFromBodyOnly: Boolean): Boolean;
begin
  { Multiple routines on one physical line: the first one wins. AFromBodyOnly
    matches from the body's 'begin' onwards but still ends at LastLine, so a
    body missing its 'end' matches to the end of the routine's span }
  for var Routine in FRoutines do begin
    var MatchFirstLine := Routine.FirstLine;
    if AFromBodyOnly then
      MatchFirstLine := Routine.BodyFirstLine;
    if (MatchFirstLine >= 0) and (ALine >= MatchFirstLine) and
       (ALine <= Routine.LastLine) then begin
      ARoutine := Routine;
      Exit(True);
    end;
  end;
  ARoutine := nil;
  Result := False;
end;

end.
