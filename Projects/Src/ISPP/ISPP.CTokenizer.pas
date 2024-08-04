{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff

  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

unit ISPP.CTokenizer;

interface

uses SysUtils;

type

  EParsingError = class(Exception)
    Position: Integer;
  end;

  TTokenKind = (tkError, tkEOF, tkIdent, tkNumber, tkString, opGreater,
    opLess,
    opGreaterEqual,
    opLessEqual,
    opEqual,
    opNotEqual,

    opOr,
    opAnd,

    opAdd,
    opSubtract,
    opBwOr,
    opXor,
    opMul,
    opDiv,
    opBwAnd,
    opShl,
    opShr,
    opMod,
    opNot,
    opBwNot,

    opAssign,
    opAgnAdd,
    opAgnSub,
    opAgnOr,
    opAgnXor,
    opAgnMul,
    opAgnDiv,
    opAgnAnd,
    opAgnShl,
    opAgnShr,
    opAgnMod,

    opInc,
    opDec,


    tkOpenParen,
    tkOpenBracket,
    tkOpenBrace,
    tkCloseParen,
    tkCloseBracket,
    tkCloseBrace,

    tkPeriod,
    tkComma,
    tkColon,
    tkSemicolon,
    tkQuestion,
    tkPtr);

  TTokenKinds = set of TTokenKind;

  TCTokenizer = class(TObject)
  private
    FEscapeSequences: Boolean;
    FExprStart: PChar;
    FIdent: string;
    FToken: TTokenKind;
    FNextTokenKnown: Boolean;
    FNextToken: TTokenKind;
    FNextTokenPos: PChar;
    FNextIdent: string;
    FStoredPos: PChar;
    procedure IllegalChar(C: Char);
    function InternalNextToken: TTokenKind;
  protected
    FExpr: PChar;
    FExprOffset: Integer;
    procedure EndOfExpr;
    procedure Error(const Message: string);
    procedure ErrorFmt(const Message: string; Args: array of const);
  public
    constructor Create(const Expression: string;
      EscapeSequences: Boolean);
    procedure SkipBlanks;
    function NextToken: TTokenKind;
    function NextTokenExpect(Expected: TTokenKinds): TTokenKind;
    function TokenInt: Longint;
    function PeekAtNextToken: TTokenKind;
    function PeekAtNextTokenString: string;
    procedure Store;
    procedure Restore;
    procedure SetPos(NewPos: PChar);
    property Token: TTokenKind read FToken;
    property TokenString: string read FIdent;
  end;

const
  ExpressionStartTokens = [tkOpenParen, tkIdent, tkNumber, tkString, opNot,
    opBwNot, opAdd, opSubtract, opInc, opDec, tkPtr];

implementation

uses
  ISPP.Consts, Shared.CommonFunc;

{ TCTokenizer }

constructor TCTokenizer.Create(const Expression: string;
  EscapeSequences: Boolean);
begin
  FExpr := PChar(Expression);
  FExprStart := FExpr;
  FEscapeSequences := EscapeSequences;
end;

procedure TCTokenizer.SkipBlanks;
begin
  while CharInSet(FExpr^, [#1..#32]) do Inc(FExpr);
  if (FExpr^ = '/') and (FExpr[1] = '*') then
  begin
    Inc(FExpr, 2);
    while True do
    begin
      while not CharInSet(FExpr^, [#0, '*']) do Inc(FExpr);
      if (FExpr^ = '*') then
        if FExpr[1] = '/' then
        begin
          Inc(FExpr, 2);
          SkipBlanks;
          Exit;
        end
        else
          Inc(FExpr)
      else
        Error('Unterminated comment');
    end;
  end
end;

function TCTokenizer.InternalNextToken: TTokenKind;

  procedure Promote(T: TTokenKind);
  begin
    Result := T;
    Inc(FExpr);
  end;

  function GetString(QuoteChar: Char): string;
  var
    P: PChar;
    S: string;
    I: Integer;
    C: Byte;

    procedure Unterminated;
    begin
      if FExpr^ = #0 then
        Error('Unterminated string');
    end;

  begin
    Inc(FExpr);
    Result := '';
    while True do
    begin
      P := FExpr;
      while not CharInSet(FExpr^, [#0, '\', QuoteChar]) do Inc(FExpr);
      SetString(S, P, FExpr - P);
      Result := Result + S;
      Unterminated;
      if FExpr^ = QuoteChar then
      begin
        Inc(FExpr);
        Break;
      end;
      Inc(FExpr);
      Unterminated;
      case FExpr^ of
        #0: Unterminated;
        '0'..'7':// octal 400 = $100
          begin
            C := 0;
            I := 0;
            while CharInSet(FExpr^, ['0'..'7']) and (I < 3) do
            begin
              Inc(I);
              C := (C shl 3) + (Ord(FExpr^) - Ord('0'));
              Inc(FExpr);
              Unterminated;
            end;
            Result := Result + Char(C);
            Continue;
          end;
        'a': Result := Result + #7;
        'b': Result := Result + #8;
        'f': Result := Result + #12;
        'n': Result := Result + #10;
        'r': Result := Result + #13;
        't': Result := Result + #9;
        'v': Result := Result + #11;
        'x':
          begin
            Inc(FExpr);
            C := 0;
            I := 0;
            while CharInSet(FExpr^, ['0'..'9', 'A'..'F', 'a'..'f']) and (I < 2) do
            begin
              Inc(I);
              C := C shl 4;
              case FExpr^ of
                '0'..'9': C := C + (Ord(FExpr^) - Ord('0'));
                'A'..'F': C := C + (Ord(FExpr^) - Ord('A')) + $0A;
              else
                C := C + (Ord(FExpr^) - Ord('a')) + $0A;
              end;
              Inc(FExpr);
              Unterminated;
            end;
            Result := Result + Char(C);
            Continue;
          end;
      else
        Result := Result + FExpr^
      end;
      Inc(FExpr);
    end;
    SkipBlanks;
    if FExpr^ = QuoteChar then
      Result := Result + GetString(QuoteChar);
  end;

var
  P: PChar;
begin
  SkipBlanks;
  Result := tkError;
  case FExpr^ of
    #0:
      begin
        Result := tkEOF;
        Exit;
      end;
    '!': if FExpr[1] = '=' then Promote(opNotEqual) else Result := opNot;
    '&':
      case FExpr[1] of
        '&': Promote(opAnd);
        '=': Promote(opAgnAnd)
      else
        Result := opBwAnd
      end;
    '|':
      case FExpr[1] of
        '|': Promote(opOr);
        '=': Promote(opAgnOr)
      else
        Result := opBwOr
      end;
    '^': if FExpr[1] = '=' then Promote(opAgnXor) else Result := opXor;
    '=': if FExpr[1] = '=' then Promote(opEqual) else Result := opAssign;
    '>':
      case FExpr[1] of
        '>':
          begin
            Promote(opShr);
            if FExpr[1] = '=' then Promote(opAgnShr);
          end;
        '=': Promote(opGreaterEqual)
      else
        Result := opGreater
      end;
    '<':
      case FExpr[1] of
        '<':
          begin
            Promote(opShl);
            if FExpr[1] = '=' then Promote(opAgnShl);
          end;
        '=': Promote(opLessEqual)
      else
        Result := opLess
      end;
    '+':
      case FExpr[1] of
        '=': Promote(opAgnAdd);
        '+': Promote(opInc)
      else
        Result := opAdd
      end;
    '-':
      case FExpr[1] of
        '=': Promote(opAgnSub);
        '-': Promote(opDec)
      else
        Result := opSubtract
      end;
    '/': if FExpr[1] = '=' then Promote(opAgnDiv) else Result := opDiv;
    '%': if FExpr[1] = '=' then Promote(opAgnMod) else Result := opMod;
    '*': if FExpr[1] = '=' then Promote(opAgnMul) else Result := opMul;
    '?': Result := tkQuestion;
    ':': Result := tkColon;
    ';': Result := tkSemicolon;
    ',': Result := tkComma;
    '.': Result := tkPeriod;
    '~': Result := opBwNot;
    '(': Result := tkOpenParen;
    '[': Result := tkOpenBracket;
    '{': Result := tkOpenBrace;
    ')': Result := tkCloseParen;
    ']': Result := tkCloseBracket;
    '}': Result := tkCloseBrace;
    '@': Result := tkPtr;
    'A'..'Z', '_', 'a'..'z':
      begin
        P := FExpr;
        repeat
          Inc(FExpr)
        until not CharInSet(FExpr^, ['0'..'9', 'A'..'Z', '_', 'a'..'z']);
        SetString(FIdent, P, FExpr - P);
        Result := tkIdent;
        Exit;
      end;
    '0'..'9':
      begin
        P := FExpr;
        repeat
          Inc(FExpr)
        until not CharInSet(FExpr^, ['0'..'9', 'A'..'F', 'X', 'a'..'f', 'x']);
        SetString(FIdent, P, FExpr - P);
        while CharInSet(FExpr^, ['L', 'U', 'l', 'u']) do Inc(FExpr);
        Result := tkNumber;
        Exit;
      end;
    '"', '''':
      begin
        if FEscapeSequences then
          FIdent := GetString(FExpr^)
        else
          FIdent := AnsiExtractQuotedStr(FExpr, FExpr^);
        Result := tkString;
        Exit;
      end;
  end;
  if Result = tkError then IllegalChar(FExpr^);
  Inc(FExpr)
end;

function TCTokenizer.PeekAtNextToken: TTokenKind;
var
  P: PChar;
  SaveIdent: string;
begin
  if not FNextTokenKnown then
  begin
    P := FExpr;
    SaveIdent := FIdent;
    FNextToken := InternalNextToken;
    FNextIdent := FIdent;
    FIdent := SaveIdent;
    FNextTokenPos := FExpr;
    FExpr := P;
    FNextTokenKnown := True;
  end;
  Result := FNextToken;
end;

function TCTokenizer.NextToken: TTokenKind;
begin
  if FNextTokenKnown then
  begin
    FToken := FNextToken;
    FIdent := FNextIdent;
    FExpr := FNextTokenPos;
    FNextTokenKnown := False;
  end
  else
    FToken := InternalNextToken;
  Result := FToken;
end;

function TCTokenizer.PeekAtNextTokenString: string;
begin
  PeekAtNextToken;
  Result := FNextIdent;
end;

function TCTokenizer.TokenInt: Longint;
var
  E: Integer;
begin
  Val(FIdent, Result, E);
  if E <> 0 then
    Error('Cannot convert to integer');
end;

procedure TCTokenizer.Restore;
begin
  FExpr := FStoredPos;
  FNextTokenKnown := False;
end;

procedure TCTokenizer.Store;
begin
  FStoredPos := FExpr;
end;

function TCTokenizer.NextTokenExpect(Expected: TTokenKinds): TTokenKind;

  function GetFriendlyTokenDesc(T: TTokenKind; Found: Boolean): string;
  const
    TokenNames: array[TTokenKind] of string =
      ('illegal character', 'end of expression', 'identifier', 'number', 'string literal',
      'right angle bracket (">")',
      'left angle bracket ("<")',
      'greater-or-equal-to operator (">=")',
      'less-or-equal-to operator ("<=")',
      'equality operator ("==")',
      'inequality operator ("!=")',
      'logical OR operator ("||")',
      'logical AND operator ("&&")',
      'plus sign ("+")',
      'minus sign ("-")',
      'OR sign ("|")',
      'XOR operator ("^")',
      'star sign ("*")',
      'slash ("/")',
      'AND sign ("&")',
      'SHL operator ("<<")',
      'SHR operator (">>")',
      'percent sign ("%")',
      'exclamation sign ("!")',
      'tilde ("~")',

      'equal sign ("=")',
      'compound assignment operator ("+=")',
      'compound assignment operator ("-=")',
      'compound assignment operator ("|=")',
      'compound assignment operator ("^=")',
      'compound assignment operator ("*=")',
      'compound assignment operator ("/=")',
      'compound assignment operator ("&=")',
      'compound assignment operator ("<<=")',
      'compound assignment operator (">>=")',
      'compound assignment operator ("%=")',

      'increment operator ("++")',
      'decrement operator ("--")',

      'opening parenthesis ("(")',
      'opening bracket ("[")',
      'opening brace ("{")',
      'closing parenthesis (")")',
      'closing bracket ("]")',
      'closing brace ("}")',

      'period (".")',
      'comma (",")',
      'colon (":")',
      'semicolon (";")',
      'question sign ("?")',
      'call-context-of operator ("@")');

  begin
    case T of
      tkIdent:
        if Found then
          Result := Format('identifier "%s"', [TokenString])
        else
          Result := 'identifier';
      tkNumber:
        if Found then
          Result := Format('number %d (0x%0:.2x)', [TokenInt])
        else
          Result := 'number';
    else
      Result := TokenNames[T];
    end;
  end;

  function Capitalize(const S: string): string;
  begin
    if (S <> '') and CharInSet(S[1], ['a'..'z']) then
      Result := UpCase(S[1]) + Copy(S, 2, MaxInt)
    else
      Result := S;
  end;

var
  M1, M2: string;
  I: TTokenKind;
  C: Integer;
begin
  Result := NextToken;
  if not (Result in Expected) then
  begin
    C := 0;
    if Expected * ExpressionStartTokens = ExpressionStartTokens then
    begin
      M2 := 'expression';
      Expected := Expected - ExpressionStartTokens;
    end;
    for I := Low(TTokenKind) to High(TTokenKind) do
      if I in Expected then
      begin
        Inc(C);
        if M2 <> '' then
        begin
          if M1 <> '' then M1 := M1 + ', ';
          M1 := M1 + M2;
          M2 := '';
        end;
        M2 := GetFriendlyTokenDesc(I, False);
      end;
    if M2 <> '' then
      if M1 <> '' then
      begin
        if C > 2 then M1 := M1 + ',';
        M1 := M1 + ' or ' + M2
      end
      else
        M1 := M2;
    Error(Capitalize(Format('%s expected but %s found', [M1, GetFriendlyTokenDesc(Token, True)])));
  end;
end;

procedure TCTokenizer.EndOfExpr;
begin
  NextTokenExpect([tkEOF, tkSemicolon])
end;


procedure TCTokenizer.Error(const Message: string);
begin
  var E := EParsingError.Create(Message);
  if FExprOffset <> -1 then
    E.Position := FExprOffset + (FExpr - FExprStart) + 1;
  raise E;
end;

procedure TCTokenizer.ErrorFmt(const Message: string;
  Args: array of const);
begin
  Error(Format(Message, Args));
end;

procedure TCTokenizer.IllegalChar(C: Char);
begin                                             
  raise EParsingError.CreateFmt(SIllegalChar, [C, Ord(C)]);
end;

procedure TCTokenizer.SetPos(NewPos: PChar);
begin
  FExpr := NewPos;
  FNextTokenKnown := False;
end;

end.
