unit ISPP.CTokenizer.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for ISPP.CTokenizer

  Runs a self-test if DEBUG is defined
}

interface

procedure ISPPCTokenizerRunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, {$ENDIF} System.SysUtils, ISPP.CTokenizer;

{$C+}

procedure ISPPCTokenizerRunTests;

  function NewTokenizer(const Expression: String;
    const EscapeSequences: Boolean = True): TCTokenizer;
  begin
    Result := TCTokenizer.Create(Expression, EscapeSequences);
  end;

  procedure ExpectSingle(const Expression: String; const Expected: TTokenKind);
  begin
    const Tokenizer = NewTokenizer(Expression);
    try
      Assert(Tokenizer.NextToken = Expected);
      Assert(Tokenizer.NextToken = tkEOF);
    finally
      Tokenizer.Free;
    end;
  end;

  procedure ExpectSequence(const Expression: String;
    const Expected: array of TTokenKind);
  begin
    const Tokenizer = NewTokenizer(Expression);
    try
      for var I := 0 to High(Expected) do
        Assert(Tokenizer.NextToken = Expected[I]);
      Assert(Tokenizer.NextToken = tkEOF);
    finally
      Tokenizer.Free;
    end;
  end;

  {$IFDEF ISTESTTOOLPROJ}
  procedure ExpectErrorAt(const Expression: String;
    const ExpectedPosition: Integer; const EscapeSequences: Boolean = True);
  begin
    const Tokenizer = NewTokenizer(Expression, EscapeSequences);
    try
      var Caught := False;
      try
        while Tokenizer.NextToken <> tkEOF do { keep going } ;
      except
        on E: EParsingError do begin
          Caught := True;
          Assert(E.Position = ExpectedPosition);
        end;
      end;
      Assert(Caught);
    finally
      Tokenizer.Free;
    end;
  end;
  {$ENDIF}

begin
  { Empty / whitespace-only input is just tkEOF }
  ExpectSingle('', tkEOF);
  ExpectSingle('   '#9#13#10, tkEOF);

  { Single-character tokens }
  ExpectSingle('(', tkOpenParen);
  ExpectSingle('[', tkOpenBracket);
  ExpectSingle('{', tkOpenBrace);
  ExpectSingle(')', tkCloseParen);
  ExpectSingle(']', tkCloseBracket);
  ExpectSingle('}', tkCloseBrace);
  ExpectSingle('.', tkPeriod);
  ExpectSingle(',', tkComma);
  ExpectSingle(':', tkColon);
  ExpectSingle(';', tkSemicolon);
  ExpectSingle('?', tkQuestion);
  ExpectSingle('@', tkPtr);
  ExpectSingle('~', opBwNot);

  { Operators that have alone / doubled / followed-by-'=' variants. The
    tokenizer must pick the longest match. }
  ExpectSingle('+', opAdd);
  ExpectSingle('++', opInc);
  ExpectSingle('+=', opAgnAdd);
  ExpectSingle('-', opSubtract);
  ExpectSingle('--', opDec);
  ExpectSingle('-=', opAgnSub);
  ExpectSingle('*', opMul);
  ExpectSingle('*=', opAgnMul);
  ExpectSingle('/', opDiv);
  ExpectSingle('/=', opAgnDiv);
  ExpectSingle('%', opMod);
  ExpectSingle('%=', opAgnMod);
  ExpectSingle('&', opBwAnd);
  ExpectSingle('&&', opAnd);
  ExpectSingle('&=', opAgnAnd);
  ExpectSingle('|', opBwOr);
  ExpectSingle('||', opOr);
  ExpectSingle('|=', opAgnOr);
  ExpectSingle('^', opXor);
  ExpectSingle('^=', opAgnXor);
  ExpectSingle('!', opNot);
  ExpectSingle('!=', opNotEqual);
  ExpectSingle('=', opAssign);
  ExpectSingle('==', opEqual);
  ExpectSingle('<', opLess);
  ExpectSingle('<=', opLessEqual);
  ExpectSingle('<<', opShl);
  ExpectSingle('<<=', opAgnShl);
  ExpectSingle('>', opGreater);
  ExpectSingle('>=', opGreaterEqual);
  ExpectSingle('>>', opShr);
  ExpectSingle('>>=', opAgnShr);

  { Identifiers: alpha or '_' followed by alphanumerics/'_' }
  var Tokenizer := NewTokenizer('_foo123 BAR_baz');
  try
    Assert(Tokenizer.NextToken = tkIdent);
    Assert(Tokenizer.TokenString = '_foo123');
    Assert(Tokenizer.NextToken = tkIdent);
    Assert(Tokenizer.TokenString = 'BAR_baz');
    Assert(Tokenizer.NextToken = tkEOF);
  finally
    Tokenizer.Free;
  end;

  { Numbers: decimal, hex, with optional U/L suffixes (which are consumed
    but not preserved in TokenString) }
  Tokenizer := NewTokenizer('123 0x1A 0XFFul 42L');
  try
    Assert(Tokenizer.NextToken = tkNumber);
    Assert(Tokenizer.TokenString = '123');
    Assert(Tokenizer.NextToken = tkNumber);
    Assert(Tokenizer.TokenString = '0x1A');
    Assert(Tokenizer.NextToken = tkNumber);
    Assert(Tokenizer.TokenString = '0XFF');
    Assert(Tokenizer.NextToken = tkNumber);
    Assert(Tokenizer.TokenString = '42');
    Assert(Tokenizer.NextToken = tkEOF);
  finally
    Tokenizer.Free;
  end;

  { Adjacent string literals of the same quote kind concatenate (the GetString
    routine recurses across whitespace separating the two) }
  Tokenizer := NewTokenizer('"abc"  "def"');
  try
    Assert(Tokenizer.NextToken = tkString);
    Assert(Tokenizer.TokenString = 'abcdef');
    Assert(Tokenizer.NextToken = tkEOF);
  finally
    Tokenizer.Free;
  end;

  { String literals (no escape sequences): handled by AnsiExtractQuotedStr.
    Doubled quote-of-the-same-kind means a literal quote. Adjacent literals
    of the different quote kind do not concatenate. }
  Tokenizer := NewTokenizer('"hello" ''single-quoted'' "a""b"', False);
  try
    Assert(Tokenizer.NextToken = tkString);
    Assert(Tokenizer.TokenString = 'hello');
    Assert(Tokenizer.NextToken = tkString);
    Assert(Tokenizer.TokenString = 'single-quoted');
    Assert(Tokenizer.NextToken = tkString);
    Assert(Tokenizer.TokenString = 'a"b');
    Assert(Tokenizer.NextToken = tkEOF);
  finally
    Tokenizer.Free;
  end;

  { String literals with escape sequences: \n, \t, \\, \", \xNN, \NNN (octal),
    and the catch-all (any other character is taken literally) }
  Tokenizer := NewTokenizer('"a\nb"');
  try
    Assert(Tokenizer.NextToken = tkString);
    Assert(Tokenizer.TokenString = 'a' + #10 + 'b');
    Assert(Tokenizer.NextToken = tkEOF);
  finally
    Tokenizer.Free;
  end;

  Tokenizer := NewTokenizer('"\a\b\f\n\r\t\v"');
  try
    Assert(Tokenizer.NextToken = tkString);
    Assert(Tokenizer.TokenString = #7 + #8 + #12 + #10 + #13 + #9 + #11);
    Assert(Tokenizer.NextToken = tkEOF);
  finally
    Tokenizer.Free;
  end;

  { Hex escape: \x consumes up to 2 hex digits }
  Tokenizer := NewTokenizer('"\x41\x42\x4a"');
  try
    Assert(Tokenizer.NextToken = tkString);
    Assert(Tokenizer.TokenString = 'ABJ');
    Assert(Tokenizer.NextToken = tkEOF);
  finally
    Tokenizer.Free;
  end;

  { Octal escape: digits 0..7, up to 3 digits }
  Tokenizer := NewTokenizer('"\101\102"');
  try
    Assert(Tokenizer.NextToken = tkString);
    Assert(Tokenizer.TokenString = 'AB');
    Assert(Tokenizer.NextToken = tkEOF);
  finally
    Tokenizer.Free;
  end;

  { Escape: any other character after '\' is taken literally, including
    the backslash, the quote character, and an apostrophe }
  Tokenizer := NewTokenizer('"\\\"\''"');
  try
    Assert(Tokenizer.NextToken = tkString);
    Assert(Tokenizer.TokenString = '\"''');
    Assert(Tokenizer.NextToken = tkEOF);
  finally
    Tokenizer.Free;
  end;

  { Empty string is a valid token }
  Tokenizer := NewTokenizer('""');
  try
    Assert(Tokenizer.NextToken = tkString);
    Assert(Tokenizer.TokenString = '');
    Assert(Tokenizer.NextToken = tkEOF);
  finally
    Tokenizer.Free;
  end;

  { /* ... */ comments are treated as whitespace
    '*' inside the comment without a following '/' is part of the comment }
  Tokenizer := NewTokenizer('a/* a * b */b');
  try
    Assert(Tokenizer.NextToken = tkIdent);
    Assert(Tokenizer.TokenString = 'a');
    Assert(Tokenizer.NextToken = tkIdent);
    Assert(Tokenizer.TokenString = 'b');
    Assert(Tokenizer.NextToken = tkEOF);
  finally
    Tokenizer.Free;
  end;

  { A representative sequence of mixed tokens }
  ExpectSequence('foo(1, 2) + bar.baz[3]; ?:',
    [tkIdent, tkOpenParen, tkNumber, tkComma, tkNumber, tkCloseParen,
     opAdd, tkIdent, tkPeriod, tkIdent, tkOpenBracket, tkNumber, tkCloseBracket,
     tkSemicolon, tkQuestion, tkColon]);

  { NextTokenExpect is the parser-facing API: it must both validate and consume
    the expected token, leaving TokenString/TokenInt available to callers. }
  Tokenizer := NewTokenizer('alpha 123');
  try
    Assert(Tokenizer.NextTokenExpect([tkIdent]) = tkIdent);
    Assert(Tokenizer.TokenString = 'alpha');
    Assert(Tokenizer.NextTokenExpect([tkNumber]) = tkNumber);
    Assert(Tokenizer.TokenString = '123');
    Assert(Tokenizer.TokenInt = 123);
    Assert(Tokenizer.NextTokenExpect([tkEOF]) = tkEOF);
  finally
    Tokenizer.Free;
  end;

  {$IFDEF ISTESTTOOLPROJ}
  { Errors: illegal character. The bad character is at column 1 (1-based). }
  ExpectErrorAt('$', 1);
  { After consuming 'a', the bad character '$' sits at column 2. }
  ExpectErrorAt('a$', 2);
  { Skip whitespace before reporting the bad character }
  ExpectErrorAt('  $', 3);

  { Errors: unterminated string (in escape mode the scan stops at #0). The
    Position points one past the last character read. }
  ExpectErrorAt('"abc', 5);
  ExpectErrorAt('''abc', 5);
  { Backslash followed by end-of-string is also unterminated }
  ExpectErrorAt('"\', 3);
  { Hex escape followed by end-of-string is unterminated as well }
  ExpectErrorAt('"\x', 4);

  { Errors: unterminated /* comment */ }
  ExpectErrorAt('/* abc', 7);
  {$ENDIF}

  { PeekAtNextToken does not consume; a subsequent NextToken returns the same
    token and TokenString }
  Tokenizer := NewTokenizer('foo bar');
  try
    Assert(Tokenizer.PeekAtNextToken = tkIdent);
    Assert(Tokenizer.PeekAtNextTokenString = 'foo');
    { Peek again: still the same answer }
    Assert(Tokenizer.PeekAtNextToken = tkIdent);
    Assert(Tokenizer.PeekAtNextTokenString = 'foo');
    { Now actually consume }
    Assert(Tokenizer.NextToken = tkIdent);
    Assert(Tokenizer.TokenString = 'foo');
  finally
    Tokenizer.Free;
  end;

  { Store / Restore: Restore rewinds to the position captured by Store and
    drops any peeked-ahead state }
  Tokenizer := NewTokenizer('a b c');
  try
    Assert(Tokenizer.NextToken = tkIdent);
    Assert(Tokenizer.TokenString = 'a');
    Tokenizer.Store;
    Assert(Tokenizer.NextToken = tkIdent);
    Assert(Tokenizer.TokenString = 'b');
    Assert(Tokenizer.PeekAtNextToken = tkIdent);
    Assert(Tokenizer.PeekAtNextTokenString = 'c');
    Tokenizer.Restore;
    Assert(Tokenizer.NextToken = tkIdent);
    Assert(Tokenizer.TokenString = 'b');
  finally
    Tokenizer.Free;
  end;

  { SetPos is used by the preprocessor to jump back to saved expression
    slices; it must also discard any peeked token from the old position. }
  const SetPosExpression = 'one two';
  Tokenizer := NewTokenizer(SetPosExpression);
  try
    Assert(Tokenizer.NextToken = tkIdent);
    Assert(Tokenizer.TokenString = 'one');
    Assert(Tokenizer.NextToken = tkIdent);
    Assert(Tokenizer.TokenString = 'two');
    Assert(Tokenizer.PeekAtNextToken = tkEOF);
    Tokenizer.SetPos(PChar(SetPosExpression) + Length('one '));
    Assert(Tokenizer.NextToken = tkIdent);
    Assert(Tokenizer.TokenString = 'two');
  finally
    Tokenizer.Free;
  end;
end;

{$IFDEF DEBUG}
initialization
  try
    ISPPCTokenizerRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}

end.
