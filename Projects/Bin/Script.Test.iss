[Setup]
AppName=Script.Test
AppVersion=1.0
DefaultDirName={autopf}\Script.Test
OutputDir=.
OutputBaseFilename=Script.Test-Setup
SetupArchitecture={#arch}
PrivilegesRequired=lowest

[Code]

program Script_Test; { Test 'program' keyword parses }

procedure CheckTrue(const Cond: Boolean);
begin
  if not Cond then
    RaiseException('CheckTrue test failed');
end;

procedure CheckFalse(const Cond: Boolean);
begin
  if Cond then
    RaiseException('CheckFalse test failed');
end;

procedure CheckEqualsInt64(const Expected, Actual: Int64);
begin
  if Expected <> Actual then
    RaiseException(
      'CheckEqualsInt64 test failed: expected ' + IntToStr(Expected) +
        ', got ' + IntToStr(Actual));
end;

procedure CheckEqualsUInt64(const Expected, Actual: UInt64);
begin
  if Expected <> Actual then
    RaiseException(
      'CheckEqualsUInt64 test failed: expected ' + IntToStr(Expected) +
        ', got ' + IntToStr(Actual));
end;

procedure CheckEqualsString(const Expected, Actual: String);
begin
  if Expected <> Actual then
    RaiseException(
      'CheckEqualsString test failed: expected "' + Expected +
        '", got "' + Actual + '"');
end;

procedure CheckEqualsFloat(const Expected, Actual, Tolerance: Extended);
begin
  if Abs(Expected - Actual) > Tolerance then
    RaiseException(
      'CheckEqualsFloat test failed: expected ' + FloatToStr(Expected) +
        ', got ' + FloatToStr(Actual));
end;

{ Call inside an except block; raises if ExceptionType doesn't match. }
procedure CheckRaisedCode(const ExpectedCode: TIFException);
begin
  if ExceptionType <> ExpectedCode then
    RaiseException(
      'CheckRaisedCode test failed: expected exception ' +
        IntToStr(Ord(ExpectedCode)) + ', got ' + IntToStr(Ord(ExceptionType)) +
        ' [' + ExceptionToString(ExceptionType, ExceptionParam) + ']');
end;

{ Test 'export' keyword parses }
procedure Test_Lexical_Export; export;
begin
end;

procedure Test_Lexical;
begin
  { Keywords not exercisable from [Code]:
    unit, interface, implementation, finalization, initialization, class,
    inherited, constructor, destructor, virtual, override, private, protected,
    public, published, '^' dereference token. }

  { Numeric literals }
  CheckEqualsInt64(255, $FF);
  CheckEqualsInt64(255, 0 + $FF);
  CheckEqualsFloat(1.5e2, 150.0, 0.0);
  CheckEqualsFloat(1.5E-1, 0.15, 1e-9);
  CheckEqualsFloat(2.0, 1.0 + 1.0, 0.0);
  { '..' range token not eaten by real-literal parser }
  case 5 of
    1..10: CheckTrue(True);
  end;

  { Character literals }
  CheckEqualsString('A', #65);
  CheckEqualsString('A', #$41);
  CheckEqualsString('AB', #65#66);
  CheckEqualsString('a''b', 'a''b');

  (* paren-star comment *)
  // line comment

  { '@' address-of token: exercised by procedural variable tests }
  { Other keyswords: exercised by other tests }
end;

procedure RunAllTests;
begin
  Test_Lexical;
end;

function InitializeSetup: Boolean;
var
  ResultText: String;
begin
  try
    RunAllTests;
    ResultText := 'OK';
  except
    ResultText := GetExceptionMessage;
  end;
  SaveStringToFile(ExpandConstant('{src}\Script.Test-Result.txt'), ResultText, False);
  Result := False;
end;