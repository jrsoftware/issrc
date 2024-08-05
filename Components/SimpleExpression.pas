unit SimpleExpression;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Evaluator for simple boolean expressions

  Grammar:
  -expression = term ('or' term)*
  -term       = factor ('and' factor)*
  -factor     = '(' expression ')' | 'not' factor | identifier ( '(' parameters ')' )
  -identifier = letter | '_' (letter | number | '_' | '\')*
  -parameters = string | number | boolean (',' string | number | boolean )*

  As a special optional rule it can insert an 'or' if an identifier is encountered
  at the place where an 'or' could be.

  Function calls within parameter lists are currently not supported, except for calls
  to the special ExpandConstant function.
}

interface

type
  TSimpleExpression = class;

  TSimpleExpressionOnEvalIdentifier = function(Sender: TSimpleExpression;
    const Name: String; const Parameters: array of const): Boolean of object;
  TSimpleExpressionOnExpandConstant = function(Sender: TSimpleExpression;
    const Constant: String): String of object;

  TSimpleExpression = class
    private
      FExpression: String;
      FLazy: Boolean;
      FOnEvalIdentifier: TSimpleExpressionOnEvalIdentifier;
      FOnExpandConstant: TSimpleExpressionOnExpandConstant;
      FParametersAllowed: Boolean;
      FSingleIdentifierMode: Boolean;
      FSilentOrAllowed: Boolean;
      FTag: LongInt;
      FText: PChar;
      FTokenId: (tiEOF, tiOpenRound, tiCloseRound, tiComma, tiNot, tiAnd, tiOr, tiIdentifier, tiString, tiInteger, tiBoolean);
      FToken: String;
      function FReadParameters(var Parameters: array of const): Integer;
      function FEvalIdentifier(const InLazyBranch: Boolean): Boolean;
      function FEvalFactor(const InLazyBranch: Boolean): Boolean;
      function FEvalTerm(const InLazyBranch: Boolean): Boolean;
      function FEvalExpression(const InLazyBranch: Boolean): Boolean;
      procedure Next;
    public
      function Eval: Boolean;
      property Expression: String read FExpression write FExpression;
      property Lazy: Boolean read FLazy write FLazy;
      property OnEvalIdentifier: TSimpleExpressionOnEvalIdentifier read FOnEvalIdentifier write FOnEvalIdentifier;
      property OnExpandConstant: TSimpleExpressionOnExpandConstant read FOnExpandConstant write FOnExpandConstant;
      property ParametersAllowed: Boolean read FParametersAllowed write FParametersAllowed;
      property SilentOrAllowed: Boolean read FSilentOrAllowed write FSilentOrAllowed;
      property SingleIdentifierMode: Boolean read FSingleIdentifierMode write FSingleIdentifierMode;
      property Tag: LongInt read FTag write FTag;
    end;

implementation

uses
  SysUtils;

procedure AssignStringToVarRec(var VarRec: TVarRec; const S: String);
begin
  VarRec.VType := vtUnicodeString;
  UnicodeString(VarRec.VUnicodeString) := S;
end;

{---}

procedure TSimpleExpression.Next;
var
  P: PChar;
begin
  { Ignore whitespace }
  while CharInSet(FText^ , [#1..#32]) do
    Inc(FText);
  case FText^ of
    #0:
      begin
        FToken := '';
        FTokenId := tiEOF;
      end;
    '(':
      begin
        FToken := FText^;
        FTokenId := tiOpenRound;
        Inc(FText);
      end;
    ')':
      begin
        FToken := FText^;
        FTokenId := tiCloseRound;
        Inc(FText);
      end;
    ',':
      begin
        FToken := FText^;
        FTokenId := tiComma;
        Inc(FText);
      end;
    'A'..'Z', 'a'..'z', '_':
      begin
        P := FText;
        Inc(FText);
        while CharInSet(FText^ , ['0'..'9', 'A'..'Z', 'a'..'z', '_', '\']) do
          Inc(FText);
        SetString(FToken, P, FText - P);
        if CompareText(FToken, 'not') = 0 then
          FTokenId := tiNot
        else if CompareText(FToken, 'and') = 0 then
          FTokenId := tiAnd
        else if CompareText(FToken, 'or') = 0 then
          FTokenId := tiOr
        else if CompareText(FToken, 'true') = 0 then
          FTokenId := tiBoolean
        else if CompareText(FToken, 'false') = 0 then
          FTokenId := tiBoolean
        else
          FTokenId := tiIdentifier;
      end;
    '0'..'9':
      begin
        P := FText;
        Inc(FText);
        while CharInSet(FText^ , ['0'..'9']) do
          Inc(FText);
        SetString(FToken, P, FText - P);
        FTokenId := tiInteger;
      end;
    '''':
      begin
        FToken := '';
        while True do begin
          Inc(FText);
          case FText^ of
            #0: raise Exception.Create('Unexpected end of expression while reading string constant');
            #10, #13: raise Exception.Create('Unterminated string');
          else
            if FText^ = '''' then begin
              Inc(FText);
              if FText^ <> '''' then
                Break;
            end;
            FToken := FToken + FText^;
          end;
        end;
        FTokenId := tiString;
      end;
  else
    raise Exception.CreateFmt('Invalid symbol ''%s'' found', [FText^]);
  end;
end;

function TSimpleExpression.FReadParameters(var Parameters: array of const): Integer;
var
  I: Integer;
begin
  I := 0;

  while FTokenId in [tiIdentifier, tiString, tiInteger, tiBoolean] do begin
    if I <= High(Parameters) then begin
      if FTokenId = tiIdentifier then begin
        { Currently only calls to 'ExpandConstant' are supported in parameter lists }
        if CompareText(FToken, 'ExpandConstant') <> 0 then
          raise Exception.Create('Can only call function "ExpandConstant" within parameter lists');
        Next;
        if FTokenId <> tiOpenRound then
          raise Exception.CreateFmt('Invalid token ''%s'' found', [FToken]);
        Next;
        if FTokenId <> tiString then
          raise Exception.CreateFmt('Invalid token ''%s'' found', [FToken]);
        if Assigned(FOnExpandConstant) then
          AssignStringToVarRec(Parameters[I], FOnExpandConstant(Self, FToken))
        else
          AssignStringToVarRec(Parameters[I], FToken);
        Next;
        if FTokenId <> tiCloseRound then
          raise Exception.CreateFmt('Invalid token ''%s'' found', [FToken]);
      end else if FTokenId = tiString then begin
        AssignStringToVarRec(Parameters[I], FToken);
      end else if FTokenId = tiInteger then begin
        Parameters[I].VType := vtInteger;
        Parameters[I].vInteger := StrToInt(FToken);
      end else begin
        Parameters[I].VType := vtBoolean;
        Parameters[I].vBoolean := CompareText(FToken, 'true') = 0;
      end;
      Inc(I);
    end else
      raise Exception.Create('Maximum number of parameters exceeded');

    Next;
    if FTokenId <> tiComma then
      Break
    else
      Next;
  end;

  Result := I;
end;

function TSimpleExpression.FEvalIdentifier(const InLazyBranch: Boolean): Boolean;
var
  Name: String;
  Parameters: array[0..9] of TVarRec;
  ParameterCount: Integer;
  I: Integer;
begin
  Name := FToken;
  Next;

  FillChar(Parameters, SizeOf(Parameters), 0);
  try
    if FParametersAllowed and (FTokenId = tiOpenRound) then begin
      Next;
      ParameterCount := FReadParameters(Parameters);
      if FTokenId <> tiCloseRound then
        raise Exception.CreateFmt('Invalid token ''%s'' found', [FToken]);
      Next;
    end else
      ParameterCount := 0;

    if not Lazy or not InLazyBranch then begin
      if Assigned(FOnEvalIdentifier) then
        Result := FOnEvalIdentifier(Self, Name, Slice(Parameters, ParameterCount))
      else
        Result := True;
    end else
      Result := True; { Lazy and in lazy branch, just return something }
  finally
    for I := High(Parameters) downto Low(Parameters) do
      if Parameters[I].VType = vtUnicodeString then
        AssignStringToVarRec(Parameters[I], '');
  end
end;

function TSimpleExpression.FEvalFactor(const InLazyBranch: Boolean): Boolean;
begin
  case FTokenId of
    tiOpenRound:
      begin
        Next;
        Result := FEvalExpression(InLazyBranch);
        if FTokenId <> tiCloseRound then
          raise Exception.Create('Invalid token');
        Next;
      end;
    tiNot:
      begin
        Next;
        Result := not FEvalFactor(InLazyBranch);
      end;
    tiIdentifier:
      begin
        Result := FEvalIdentifier(InLazyBranch);
      end;
    else
      raise Exception.CreateFmt('Invalid token ''%s'' found', [FToken]);
  end;
end;

function TSimpleExpression.FEvalTerm(const InLazyBranch: Boolean): Boolean;
begin
  Result := FEvalFactor(InLazyBranch);
  while FTokenId = tiAnd do begin
    Next;
    if not Result then begin
      { End term result known, but continue parsing }
      FEvalFactor(True)
    end else
      Result := FEvalFactor(InLazyBranch);
  end;
end;

function TSimpleExpression.FEvalExpression(const InLazyBranch: Boolean): Boolean;
begin
  Result := FEvalTerm(InLazyBranch);
  while (FTokenId = tiOr) or
     (FSilentOrAllowed and (FTokenId = tiIdentifier)) do begin
    if FTokenId = tiOr then
      Next;
    if Result then begin
      { End expression result known, but continue parsing }
      FEvalTerm(True)
    end else
      Result := FEvalTerm(InLazyBranch);
  end;
end;

{---}

function TSimpleExpression.Eval: Boolean;
begin
  FText := PChar(FExpression);
  Next;

  if not FSingleIdentifierMode then
    Result := FEvalExpression(False)
  else begin
    if FTokenId <> tiIdentifier then
      raise Exception.CreateFmt('Invalid token ''%s'' found', [FToken]);
    Result := FEvalIdentifier(False);
  end;

  if FTokenID <> tiEOF then
    raise Exception.CreateFmt('Invalid token ''%s'' found', [FToken]);
end;

end.
