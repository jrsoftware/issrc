{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff

  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

unit ISPP.Parser;

interface

uses
  ISPP.Intf, ISPP.Base, ISPP.IdentMan, ISPP.CTokenizer;

type

  TParser = class(TCTokenizer)
  private
    FIdentMan: IIdentManager;
    FOptions: PIsppParserOptions;
    function CheckLValue(const LValue: TIsppVariant): TIsppVariant;
    function PrefixIncDec(LValue: TIsppVariant; Dec: Boolean): TIsppVariant;
    function PostfixIncDec(LValue: TIsppVariant; Dec: Boolean): TIsppVariant;
    function AssignOperation(LValue, RValue: TIsppVariant; Op: TTokenKind): TIsppVariant;
    function PerformOperation(Op1, Op2: TIsppVariant; Op: TTokenKind): TIsppVariant;
    function UnaryOperation(Op: TTokenKind; Op1: TIsppVariant): TIsppVariant;
  protected
    function Chain(Level: Byte; DoEval: Boolean): TIsppVariant;
    function Factor(DoEval: Boolean): TIsppVariant;
    function Assignment(DoEval: Boolean): TIsppVariant;
    function Conditional(DoEval: Boolean): TIsppVariant;
    function Sequentional(DoEval: Boolean): TIsppVariant;
  public
    constructor Create(const IdentMan: IIdentManager; const Expression: string;
      Offset: Integer; Options: PIsppParserOptions);
    function Evaluate: TIsppVariant;
    function Expr(StopOnComma: Boolean): TIsppVariant;
    function IntExpr(StopOnComma: Boolean): Int64;
    function StrExpr(StopOnComma: Boolean): string;
  end;

function Parse(const VarMan: IIdentManager; const AExpr: string; Offset: Integer; Options: PIsppParserOptions): TIsppVariant;
function ParseStr(const VarMan: IIdentManager; const AExpr: string; Offset: Integer; Options: PIsppParserOptions): string;
function ParseInt(const VarMan: IIdentManager; const AExpr: string; Offset: Integer; Options: PIsppParserOptions): Int64;

implementation

uses
  SysUtils, ISPP.Sessions, ISPP.Consts, ISPP.VarUtils;

function Parse(const VarMan: IIdentManager; const AExpr: string; Offset: Integer; Options: PIsppParserOptions): TIsppVariant;
begin
  with TParser.Create(VarMan, AExpr, Offset, Options) do
  try
    Result := Evaluate
  finally
    Free
  end;
end;

function ParseStr(const VarMan: IIdentManager; const AExpr: string; Offset: Integer; Options: PIsppParserOptions): string;
begin
  with TParser.Create(VarMan, AExpr, Offset, Options) do
  try
    Result := StrExpr(True);
    EndOfExpr;
  finally
    Free
  end;
end;

function ParseInt(const VarMan: IIdentManager; const AExpr: string; Offset: Integer; Options: PIsppParserOptions): Int64;
begin
  with TParser.Create(VarMan, AExpr, Offset, Options) do
  try
    Result := IntExpr(True);
    EndOfExpr;
  finally
    Free
  end;
end;

{ TParser }

constructor TParser.Create(const IdentMan: IIdentManager;
  const Expression: string; Offset: Integer; Options: PIsppParserOptions);
begin
  inherited Create(Expression, not (optPascalStrings in Options^.Options));
  FExprOffset := Offset;
  FIdentMan := IdentMan;
  FOptions := Options;
end;

function TParser.Evaluate: TIsppVariant;
begin
  Result := Expr(False);
  MakeRValue(Result);
  EndOfExpr;
end;

function TParser.Sequentional(DoEval: Boolean): TIsppVariant;
begin
  Result := Assignment(DoEval);
  while PeekAtNextToken = tkComma do
  begin
    NextToken;
    Result := Assignment(DoEval)
  end;
end;

function TParser.Expr(StopOnComma: Boolean): TIsppVariant;
begin
  if StopOnComma then
    Result := Assignment(True)
  else
    Result := Sequentional(True)
end;

function TParser.Factor(DoEval: Boolean): TIsppVariant;

  procedure PopulateCallContext(const CallContext: ICallContext);
  const
    Brackets: array[TArgGroupingStyle, Boolean] of TTokenKind =
      ((tkError, tkError), (tkOpenParen, tkCloseParen),
      (tkOpenBracket, tkCloseBracket), (tkOpenBrace, tkCloseBrace));
  type
    TArgNamingState = (ansUnknown, ansNamed, ansUnnamed);
  var
    V: TIsppVariant;
    ArgName: string;
    ArgNamingState: TArgNamingState;
    T: TTokenKind;
    ArgFound: Boolean;

    procedure GetExpression;
    begin
      V := Assignment(DoEval);
      Store;
      T := NextTokenExpect([tkComma, Brackets[CallContext.GroupingStyle, True]]);
      Restore;
      ArgFound := True;
    end;

  begin
    ArgNamingState := ansUnknown;
    ArgFound := False;
    if PeekAtNextToken = Brackets[CallContext.GroupingStyle, False] then
    begin
      NextToken;
      V := NULL;
      ArgName := '';
      T := PeekAtNextToken;
      while True do
        case T of
          tkComma:
            begin
              NextToken;
              CallContext.Add(ArgName, V);
              V := NULL;
              T := PeekAtNextToken;
            end;
          tkCloseParen, tkCloseBracket, tkCloseBrace:
            begin
              NextToken;
              if ArgFound then CallContext.Add(ArgName, V);
              V := NULL;
              Break
            end;
          tkIdent:
            begin
              Store;
              NextToken;
              ArgName := TokenString;
              if PeekAtNextToken <> opAssign then
              begin
                if ArgNamingState = ansNamed then Error(SActualParamsNamingConflict);
                ArgNamingState := ansUnnamed;
                ArgName := '';
                Restore;
                GetExpression;
              end
              else
              begin
                if ArgNamingState = ansUnnamed then Error(SActualParamsNamingConflict);
                ArgNamingState := ansNamed;
                NextToken;
                GetExpression;
              end;
            end;
        else
          begin
            if ArgNamingState = ansNamed then Error(SActualParamsNamingConflict);
            ArgNamingState := ansUnnamed;
            ArgName := '';
            GetExpression;
          end;
        end;
    end;
  end;

var
  I: Int64;
  IdentType: TIdentType;
  CallContext: ICallContext;
  Op: TTokenKind;
  ParenthesesUsed: Boolean;
begin
  FillChar(Result, SizeOf(Result), 0);
  case NextTokenExpect(ExpressionStartTokens) of
    tkOpenParen:
      begin
        Result := Sequentional(DoEval);
        NextTokenExpect([tkCloseParen])
      end;
    tkPtr:
      begin
        NextTokenExpect([tkIdent]);
        Result.Typ := evCallContext;
        if not (FIdentMan.GetIdent(TokenString, Result.AsCallContext) in
          [itVariable, itMacro, itFunc]) then
          Error('Variable, macro, or function required');
      end;
    tkIdent:
      begin
        CallContext := nil;
        IdentType := FIdentMan.GetIdent(TokenString, CallContext);
        case IdentType of
          itUnknown:
            if (optAllowUndeclared in FOptions.Options) and not
              (PeekAtNextToken in [tkOpenParen, tkOpenBracket, tkOpenBrace]) then
            begin
              Result.Typ := evNull;
              WarningMsg(SUndeclaredIdentifier, [TokenString]);
            end
            else
              ErrorFmt(SUndeclaredIdentifier, [TokenString]);
          itVariable, itMacro, itFunc:
          begin
            PopulateCallContext(CallContext);
            if DoEval then
              Result := CallContext.Call
          end;
          itDefinedFunc:
          begin
            ParenthesesUsed := PeekAtNextToken = tkOpenParen;
            if ParenthesesUsed then NextToken;
            NextTokenExpect([tkIdent]);
            if DoEval then
              MakeBool(Result, FIdentMan.Defined(TokenString));
            if ParenthesesUsed then NextTokenExpect([tkCloseParen])
          end;
          itTypeOfFunc:
          begin
            ParenthesesUsed := PeekAtNextToken = tkOpenParen;
            if ParenthesesUsed then NextToken;
            NextTokenExpect([tkIdent]);
            if DoEval then
              MakeInt(Result, FIdentMan.TypeOf(TokenString));
            if ParenthesesUsed then NextTokenExpect([tkCloseParen]);
          end;
          itDimOfFunc:
          begin
            ParenthesesUsed := PeekAtNextToken = tkOpenParen;
            if ParenthesesUsed then NextToken;
            NextTokenExpect([tkIdent]);
            if DoEval then
              MakeInt(Result, FIdentMan.DimOf(TokenString));
            if ParenthesesUsed then NextTokenExpect([tkCloseParen])
          end;
        end;
      end;
    tkNumber:
      begin
        if not TryStrToInt64(TokenString, I) then
          ErrorFmt(SCannotConvertToInteger, [TokenString]);
        MakeInt(Result, I);
      end;
    tkString: MakeStr(Result, TokenString);
    opInc, opDec:
      begin
        Op := Token;
        if DoEval then
          Result := PrefixIncDec(CheckLValue(Factor(True)), Op = opDec)
        else
          Result := Factor(False);
      end;
  else
      begin
        Op := Token;
        if DoEval then
          Result := UnaryOperation(Op, Factor(True))
        else
          Factor(False)
      end;
  end;
  Op := PeekAtNextToken;
  while Op in [opInc, opDec] do
  begin
    if DoEval then
      Result := PostfixIncDec(CheckLValue(Result), Op = opDec);
    NextToken;
    Op := PeekAtNextToken;
  end;
end;

function TParser.PerformOperation(Op1, Op2: TIsppVariant; Op: TTokenKind): TIsppVariant;
var
  A, B: Int64;
  AsBool: Boolean;
begin
  MakeRValue(Op1);
  MakeRValue(Op2);
  if Op1.Typ = evNull then
    case Op2.Typ of
      evNull:
        begin
          MakeInt(Op1, 0);
          MakeInt(Op2, 0);
        end;
      evInt: MakeInt(Op1, 0);
      evStr: MakeStr(Op1, '');
    end
  else
    if Op2.Typ = evNull then
      case Op1.Typ of
        evInt: MakeInt(Op2, 0);
        evStr: MakeStr(Op2, '');
      end;
  if (Op1.Typ <> Op2.Typ) or ((Op in [opSubtract..opShr]) and (Op1.Typ = evStr))
    then Error(SOperatorNotApplicableToThisOpera);
  AsBool := False;
  with Result do
  try
    if Op1.Typ = evStr then
    begin
      if Op = opAdd then MakeStr(Result, Op1.AsStr + Op2.AsStr)
      else
      begin
        Typ := evInt;
        A := CompareText(Op1.AsStr, Op2.AsStr);
        case Op of
          opGreater: AsBool := A > 0;
          opLess: AsBool := A < 0;
          opGreaterEqual: AsBool := A >= 0;
          opLessEqual: AsBool := A <= 0;
          opEqual: AsBool := A = 0;
          opNotEqual: AsBool := A <> 0;
        end;
        AsInt := Int64(AsBool)
      end;
    end
    else
      if Op1.Typ = evInt then
      begin
        A := Op1.AsInt;
        B := Op2.AsInt;
        Typ := evInt;
        case Op of
          opGreater: AsBool := A > B;
          opLess: AsBool := A < B;
          opGreaterEqual: AsBool := A >= B;
          opLessEqual: AsBool := A <= B;
          opEqual: AsBool := A = B;
          opNotEqual: AsBool := A <> B;
          opAdd: AsInt := A + B;
          opSubtract: AsInt := A - B;
          opOr: AsBool := (A <> 0) or (B <> 0);
          opBwOr: AsInt := A or B;
          opXor: AsInt := A xor B;
          opMul: AsInt := A * B;
          opDiv: AsInt := A div B;
          opAnd: AsBool := (A <> 0) and (B <> 0);
          opBwAnd: AsInt := A and B;
          opShl: AsInt := A shl B;
          opShr: AsInt := A shr B;
          opMod: AsInt := A mod B;
        end;
        if Op in [opGreater..opNotEqual, opOr, opAnd] then AsInt := Int64(AsBool)
      end
  except
    on E: Exception do Error(E.Message);
  end;
end;

function TParser.UnaryOperation(Op: TTokenKind; Op1: TIsppVariant): TIsppVariant;
var
  A: Int64;
begin
  MakeRValue(Op1);
  A := 0; // satisfy compiler
  case Op1.Typ of
    evNull:;
    evInt: A := Op1.AsInt
  else
    Error(SWrongUnaryOperator);
  end;
  case Op of
    opNot: MakeBool(Result, A = 0);
    opBwNot: MakeInt(Result, not A);
    opAdd: MakeInt(Result, A);
    opSubtract: MakeInt(Result, -A)
  end;
end;

type
  TShortCircuitEvalMode = (scemNone, scemStandard, scemOptional);

const
  OperatorPrecedence: array[0..9] of record
    Operators: set of TTokenKind;
    SCBE: TShortCircuitEvalMode;
    SCBEValue: Boolean;
  end =
    ((Operators: [opOr];                SCBE: scemStandard; SCBEValue: True),
     (Operators: [opAnd];               SCBE: scemStandard; SCBEValue: False),
     (Operators: [opBwOr];              SCBE: scemNone; SCBEValue: False),
     (Operators: [opXor];               SCBE: scemNone; SCBEValue: False),
     (Operators: [opBwAnd];             SCBE: scemNone; SCBEValue: False),
     (Operators: [opEqual, opNotEqual]; SCBE: scemNone; SCBEValue: False),
     (Operators: [opLess, opLessEqual,
       opGreater, opGreaterEqual];      SCBE: scemNone; SCBEValue: False),
     (Operators: [opShl, opShr];        SCBE: scemOptional; SCBEValue: False),
     (Operators: [opAdd, opSubtract];   SCBE: scemNone; SCBEValue: False),
     (Operators: [opMul, opDiv, opMod]; SCBE: scemOptional; SCBEValue: False));

function TParser.Chain(Level: Byte; DoEval: Boolean): TIsppVariant;

  function CallNext: TIsppVariant;
  begin
    if Level = High(OperatorPrecedence) then
      Result := Factor(DoEval)
    else
      Result := Chain(Level + 1, DoEval);
  end;

var
  Operator: TTokenKind;
  R: Shortint;
begin
  Result := CallNext;
  while PeekAtNextToken in OperatorPrecedence[Level].Operators do
  begin
    if DoEval and (OperatorPrecedence[Level].SCBE <> scemNone) and
      (GetOption(FOptions.Options, 'B') or // short circuit bool eval
       GetOption(FOptions.Options, 'M')) then // short circuit mul eval
    begin
      with GetRValue(Result) do
        case Typ of
          evInt: if AsInt = 0 then R := 0 else R := 1;
          evStr: R := -1
        else
          R := 0;
        end;
      if R <> -1 then
      begin
        if (OperatorPrecedence[Level].SCBE = scemStandard) and GetOption(FOptions.Options, 'B')
          or (OperatorPrecedence[Level].SCBE = scemOptional) and GetOption(FOptions.Options, 'M') then
          DoEval := not (OperatorPrecedence[Level].SCBEValue = Boolean(R))
      end;
    end;
    Operator := NextToken;
    if DoEval then
      Result := PerformOperation(Result, CallNext, Operator)
    else
      CallNext;
  end;
end;

function TParser.IntExpr(StopOnComma: Boolean): Int64;
var
  V: TIsppVariant;
begin
  Result := 0;
  if StopOnComma then
    V := Assignment(True)
  else
    V := Sequentional(True);
  MakeRValue(V);
  if V.Typ = evInt then
    Result := V.AsInt
  else
    Error(SIntegerExpressionExpected);
end;

function TParser.StrExpr(StopOnComma: Boolean): string;
var
  V: TIsppVariant;
begin
  if StopOnComma then
    V := Assignment(True)
  else
    V := Sequentional(True);
  MakeRValue(V);
  case V.Typ of
    evNull: Result := '';
    evStr: Result := V.AsStr;
  else
    Error(SStringExpressionExpected);
  end;
end;

function TParser.Assignment(DoEval: Boolean): TIsppVariant;
var
  Op: TTokenKind;
begin
  Result := Conditional(DoEval);
  while PeekAtNextToken in [opAssign..opAgnMod] do
  begin
    Op := NextToken;
    if DoEval then
      Result := AssignOperation(CheckLValue(Result), Assignment(DoEval), Op)
    else
      Assignment(DoEval)
  end;
end;

function TParser.Conditional(DoEval: Boolean): TIsppVariant;
var
  R: Boolean;
  T, F: TIsppVariant;
begin
  Result := Chain(0, DoEval);
  while PeekAtNextToken = tkQuestion do
  begin
    NextToken;
    if DoEval then
      with GetRValue(Result) do
        case Typ of
          evNull: R := False;
          evInt: R := AsInt <> 0;
        else
          R := AsStr <> '';
        end
    else
      R := False;
    T := Sequentional(DoEval and R);
    NextTokenExpect([tkColon]);
    F := Conditional(DoEval and not R);
    if DoEval then
      if R then
        Result := T
      else
        Result := F;
  end;
end;

function TParser.AssignOperation(LValue, RValue: TIsppVariant;
  Op: TTokenKind): TIsppVariant;
begin
  SimplifyLValue(LValue);
  if Op = opAssign then
  begin
    LValue.AsPtr^ := GetRValue(RValue);
    Result := LValue;
  end
  else
  begin
    Result := PerformOperation(LValue, RValue, TTokenKind(Ord(Op) - (Ord(opAgnAdd) - Ord(opAdd))));
    LValue.AsPtr^ := Result;
  end;
end;

function TParser.PostfixIncDec(LValue: TIsppVariant; Dec: Boolean): TIsppVariant;
var
  V: TIsppVariant;
begin
  Result := GetRValue(LValue);
  SimplifyLValue(LValue);
  if Dec then MakeInt(V, -1) else MakeInt(V, 1);
  LValue.AsPtr^ := PerformOperation(Result, V, opAdd);
end;

function TParser.PrefixIncDec(LValue: TIsppVariant;
  Dec: Boolean): TIsppVariant;
var
  V: TIsppVariant;
begin
  SimplifyLValue(LValue);
  if Dec then MakeInt(V, -1) else MakeInt(V, 1);
  LValue.AsPtr^ := PerformOperation(GetRValue(LValue), V, opAdd);
  Result := LValue;
end;

function TParser.CheckLValue(const LValue: TIsppVariant): TIsppVariant;
begin
  if LValue.Typ <> evLValue then Error(SLValueRequired);
  Result := LValue;
end;

end.


