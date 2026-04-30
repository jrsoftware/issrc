unit SimpleExpression.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for SimpleExpression

  Runs a self-test if DEBUG is defined
}

interface

procedure SimpleExpressionRunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, {$ENDIF} System.SysUtils, SimpleExpression;

{$C+}

type
  TSimpleExpressionTestHandler = class
    EvalACallCount: Integer;
    function EvalNone(Sender: TSimpleExpression; const Name: String;
      const Parameters: array of const): Boolean;
    function EvalA(Sender: TSimpleExpression; const Name: String;
      const Parameters: array of const): Boolean;
    function EvalIntParams(Sender: TSimpleExpression; const Name: String;
      const Parameters: array of const): Boolean;
    function EvalBoolParams(Sender: TSimpleExpression; const Name: String;
      const Parameters: array of const): Boolean;
    function EvalStringParams(Sender: TSimpleExpression; const Name: String;
      const Parameters: array of const): Boolean;
    function ExpandConstant(Sender: TSimpleExpression;
      const Constant: String): String;
  end;

function TSimpleExpressionTestHandler.EvalNone(Sender: TSimpleExpression;
  const Name: String; const Parameters: array of const): Boolean;
begin
  Result := False;
end;

function TSimpleExpressionTestHandler.EvalA(Sender: TSimpleExpression;
  const Name: String; const Parameters: array of const): Boolean;
begin
  Inc(EvalACallCount);
  Result := SameText(Name, 'a');
end;

function TSimpleExpressionTestHandler.EvalIntParams(Sender: TSimpleExpression;
  const Name: String; const Parameters: array of const): Boolean;
begin
  Assert(SameText(Name, 'eval'));
  Assert(Length(Parameters) = 2);
  Assert(Parameters[0].VType = vtInteger);
  Assert(Parameters[1].VType = vtInteger);
  Result := Parameters[0].VInteger = Parameters[1].VInteger;
end;

function TSimpleExpressionTestHandler.EvalBoolParams(Sender: TSimpleExpression;
  const Name: String; const Parameters: array of const): Boolean;
begin
  Assert(SameText(Name, 'eval'));
  Assert(Length(Parameters) = 2);
  Assert(Parameters[0].VType = vtBoolean);
  Assert(Parameters[1].VType = vtBoolean);
  Result := Parameters[0].VBoolean and Parameters[1].VBoolean;
end;

function TSimpleExpressionTestHandler.EvalStringParams(Sender: TSimpleExpression;
  const Name: String; const Parameters: array of const): Boolean;
begin
  Assert(SameText(Name, 'eval'));
  Assert(Length(Parameters) = 2);
  Assert(Parameters[0].VType = vtUnicodeString);
  Assert(Parameters[1].VType = vtUnicodeString);
  Result := UnicodeString(Parameters[0].VUnicodeString) = UnicodeString(Parameters[1].VUnicodeString);
end;

function TSimpleExpressionTestHandler.ExpandConstant(Sender: TSimpleExpression;
  const Constant: String): String;
begin
  Result := 'expanded:' + Constant;
end;

procedure SimpleExpressionRunTests;

  procedure Test(const Expression: String; const ExpectedResult: Boolean;
    const Handler: TSimpleExpressionOnEvalIdentifier);
  begin
    const Evaluator = TSimpleExpression.Create;
    try
      Evaluator.Expression := Expression;
      Evaluator.OnEvalIdentifier := Handler;
      Assert(Evaluator.Eval = ExpectedResult);
    finally
      Evaluator.Free;
    end;
  end;

  {$IFDEF ISTESTTOOLPROJ}
  procedure TestException(const Expression: String);
  begin
    const Evaluator = TSimpleExpression.Create;
    try
      Evaluator.Expression := Expression;
      var Caught := False;
      try
        Evaluator.Eval;
      except
        on ESimpleExpressionError do Caught := True;
      end;
      Assert(Caught);
    finally
      Evaluator.Free;
    end;
  end;
  {$ENDIF}

begin
  const Handler = TSimpleExpressionTestHandler.Create;
  try
    { Without OnEvalIdentifier assigned, identifiers default to True }
    Test('a', True, nil);

    { EvalNone: every identifier evaluates to False }
    Test('a', False, Handler.EvalNone);
    Test('not a', True, Handler.EvalNone);
    Test('a and b', False, Handler.EvalNone);
    Test('a or b', False, Handler.EvalNone);

    { EvalA: only 'a' is True }
    Test('a', True, Handler.EvalA);
    Test('b', False, Handler.EvalA);
    Test('not a', False, Handler.EvalA);
    Test('not b', True, Handler.EvalA);
    Test('a and b', False, Handler.EvalA);
    Test('a or b', True, Handler.EvalA);
    Test('(a or b) and a', True, Handler.EvalA);
    Test('(a or b) and b', False, Handler.EvalA);
    Test('not (a and b)', True, Handler.EvalA);

    { Operator precedence: 'and' binds tighter than 'or' }
    Test('a or b and c', True, Handler.EvalA);
    Test('b and a or b', False, Handler.EvalA);
    Test('b or a and b', False, Handler.EvalA);

    { Identifiers are case-insensitive at the keyword level }
    Test('NOT a', False, Handler.EvalA);
    Test('a AND b', False, Handler.EvalA);
    Test('a OR b', True, Handler.EvalA);

    { Whitespace handling }
    Test('   a   and   b  ', True, nil);

    { Identifier characters allow letter/digit/underscore/backslash }
    Test('foo_bar', True, nil);
    Test('a1', True, nil);
    Test('HKLM\Software\Foo', True, nil);

    { ParametersAllowed }
    var Evaluator := TSimpleExpression.Create;
    try
      Evaluator.OnEvalIdentifier := Handler.EvalIntParams; { checks 2 ints are equal }
      Evaluator.ParametersAllowed := True;
      Evaluator.Expression := 'eval(1, 1)';
      Assert(Evaluator.Eval);
      Evaluator.Expression := 'eval(1, 2)';
      Assert(not Evaluator.Eval);
      Evaluator.OnEvalIdentifier := Handler.EvalBoolParams; { checks 2 bools are both True }
      Evaluator.Expression := 'eval(true, true)';
      Assert(Evaluator.Eval);
      Evaluator.Expression := 'eval(true, false)';
      Assert(not Evaluator.Eval);
    finally
      Evaluator.Free;
    end;

    { SilentOrAllowed: adjacent identifiers are treated as 'or' }
    Evaluator := TSimpleExpression.Create;
    try
      Evaluator.OnEvalIdentifier := Handler.EvalA;
      Evaluator.SilentOrAllowed := True;
      Evaluator.Expression := 'b a';
      Assert(Evaluator.Eval);
      Evaluator.Expression := 'b c';
      Assert(not Evaluator.Eval);
    finally
      Evaluator.Free;
    end;

    { Lazy: short-circuited identifiers are not evaluated via OnEvalIdentifier }
    Evaluator := TSimpleExpression.Create;
    try
      Evaluator.OnEvalIdentifier := Handler.EvalA;

      { Lazy = False: every identifier is evaluated even after the result is known }
      Evaluator.Lazy := False;
      Handler.EvalACallCount := 0;
      Evaluator.Expression := 'a or b';
      Assert(Evaluator.Eval);
      Assert(Handler.EvalACallCount = 2);

      { Lazy = True: 'b' is skipped because 'a' already determined the result }
      Evaluator.Lazy := True;
      Handler.EvalACallCount := 0;
      Evaluator.Expression := 'a or b';
      Assert(Evaluator.Eval);
      Assert(Handler.EvalACallCount = 1);

      { Lazy = True: 'a' is skipped because 'b' already made the term False }
      Handler.EvalACallCount := 0;
      Evaluator.Expression := 'b and a';
      Assert(not Evaluator.Eval);
      Assert(Handler.EvalACallCount = 1);
    finally
      Evaluator.Free;
    end;

    { OnExpandConstant: ExpandConstant() inside a parameter list calls the handler }
    Evaluator := TSimpleExpression.Create;
    try
      Evaluator.OnEvalIdentifier := Handler.EvalStringParams; { checks 2 strings are equal }
      Evaluator.OnExpandConstant := Handler.ExpandConstant; { prefixes 'expanded:' }
      Evaluator.ParametersAllowed := True;
      Evaluator.Expression := 'eval(''expanded:foo'', ExpandConstant(''foo''))';
      Assert(Evaluator.Eval);
      Evaluator.Expression := 'eval(''something else'', ExpandConstant(''foo''))';
      Assert(not Evaluator.Eval);
    finally
      Evaluator.Free;
    end;

    { Without OnExpandConstant the constant string is passed through unchanged }
    Evaluator := TSimpleExpression.Create;
    try
      Evaluator.OnEvalIdentifier := Handler.EvalStringParams; { see above }
      Evaluator.ParametersAllowed := True;
      Evaluator.Expression := 'eval(''foo'', ExpandConstant(''foo''))';
      Assert(Evaluator.Eval);
    finally
      Evaluator.Free;
    end;

    {$IFDEF ISTESTTOOLPROJ}
    { SingleIdentifierMode: only a single identifier is accepted }
    Evaluator := TSimpleExpression.Create;
    try
      Evaluator.OnEvalIdentifier := Handler.EvalA;
      Evaluator.SingleIdentifierMode := True;
      Evaluator.Expression := 'a';
      Assert(Evaluator.Eval);
      Evaluator.Expression := 'b';
      Assert(not Evaluator.Eval);
      Evaluator.Expression := 'a and b';
      var Caught := False;
      try
        Evaluator.Eval;
      except
        on ESimpleExpressionError do Caught := True;
      end;
      Assert(Caught);
    finally
      Evaluator.Free;
    end;

    { 'true' / 'false' are recognised as tiBoolean and so are rejected as a top-level expression }
    TestException('true');
    TestException('false');

    { Errors }
    TestException('');
    TestException('a and');
    TestException('(a');
    TestException('a)');
    TestException('a $');
    {$ENDIF}
  finally
    Handler.Free;
  end;
end;

{$IFDEF DEBUG}
initialization
  try
    SimpleExpressionRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}

end.
