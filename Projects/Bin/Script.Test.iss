#ifndef arch
  #define arch "x64"
#endif

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

procedure CheckTrue(const Value: Boolean);
begin
  if not Value then
    RaiseException('CheckTrue test failed');
end;

procedure CheckFalse(const Value: Boolean);
begin
  if Value then
    RaiseException('CheckFalse test failed');
end;

procedure CheckEqualsInt64(const Expected, Actual: Int64);
begin
  if Expected <> Actual then
    RaiseException(Format('CheckEqualsInt64 test failed: expected %d, got %d', [Expected, Actual]));
end;

procedure CheckEqualsUInt64(const Expected, Actual: UInt64);
begin
  if Expected <> Actual then
    RaiseException(Format('CheckEqualsUInt64 test failed: expected %u, got %u', [Expected, Actual]));
end;

procedure CheckEqualsString(const Expected, Actual: String);
begin
  if Expected <> Actual then
    RaiseException(Format('CheckEqualsString test failed: expected "%s", got "%s"', [Expected, Actual]));
end;

procedure CheckEqualsFloat(const Expected, Actual, Tolerance: Extended);
begin
  if Abs(Expected - Actual) > Tolerance then
    RaiseException(Format('CheckEqualsFloat test failed: expected %g, got %g', [Expected, Actual]));
end;

{ Call inside an except block; raises if ExceptionType doesn't match. }
procedure CheckRaisedCode(const ExpectedCode: TIFException);
begin
  if ExceptionType <> ExpectedCode then
    RaiseException(Format('CheckRaisedCode test failed: expected exception %d, got %d [%s]', [Ord(ExpectedCode), Ord(ExceptionType), ExceptionToString(ExceptionType, ExceptionParam)]));
end;

{ Test 'export' keyword parses }
procedure Test_Lexical_Export; export;
begin
end;

{ Test attribute syntax parses }
<event('InitializeWizard')>
procedure Test_Lexical_Attribute;
begin
  { Never reached because InitializeSetup returns False before the wizard
    is created. The test is that the attribute syntax compiles. }
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

procedure Test_BaseTypeSizes;
var
  VBoolean: Boolean;
  VByte: Byte;
  VShortInt: ShortInt;
  VAnsiChar: AnsiChar;
  VWord: Word;
  VSmallInt: SmallInt;
  VWideChar: WideChar;
  VChar: Char;
  VInteger: Integer;
  VLongInt: LongInt;
  VLongWord: LongWord;
  VCardinal: Cardinal;
  VSingle: Single;
  VDouble: Double;
  VInt64: Int64;
  VUInt64: UInt64;
  VCurrency: Currency;
  VByteBool: ByteBool;
  VWordBool: WordBool;
  VLongBool: LongBool;
begin
  CheckEqualsInt64(1, SizeOf(VBoolean));
  CheckEqualsInt64(1, SizeOf(VByte));
  CheckEqualsInt64(1, SizeOf(VShortInt));
  CheckEqualsInt64(1, SizeOf(VAnsiChar));
  CheckEqualsInt64(2, SizeOf(VWord));
  CheckEqualsInt64(2, SizeOf(VSmallInt));
  CheckEqualsInt64(2, SizeOf(VWideChar));
  CheckEqualsInt64(2, SizeOf(VChar));
  CheckEqualsInt64(4, SizeOf(VInteger));
  CheckEqualsInt64(4, SizeOf(VLongInt));
  CheckEqualsInt64(4, SizeOf(VLongWord));
  CheckEqualsInt64(4, SizeOf(VCardinal));
  CheckEqualsInt64(4, SizeOf(VSingle));
  CheckEqualsInt64(8, SizeOf(VDouble));
  CheckEqualsInt64(8, SizeOf(VInt64));
  CheckEqualsInt64(8, SizeOf(VUInt64));
  CheckEqualsInt64(8, SizeOf(VCurrency));
  CheckEqualsInt64(1, SizeOf(VByteBool));
  CheckEqualsInt64(2, SizeOf(VWordBool));
  CheckEqualsInt64(4, SizeOf(VLongBool));
end;

procedure Test_ArchDependentSizes;
var
  VExtended: Extended;
  VNativeInt: NativeInt;
  VNativeUInt: NativeUInt;
begin
  #if arch == "x64"
    CheckEqualsInt64(8, SizeOf(VExtended));
    CheckEqualsInt64(8, SizeOf(VNativeInt));
    CheckEqualsInt64(8, SizeOf(VNativeUInt));
  #else
    CheckEqualsInt64(10, SizeOf(VExtended));
    CheckEqualsInt64(4, SizeOf(VNativeInt));
    CheckEqualsInt64(4, SizeOf(VNativeUInt));
  #endif
end;

procedure Test_IntegerBoundaries;
var
  VShortInt: ShortInt;
  VSmallInt: SmallInt;
  VInteger: Integer;
  VInt64: Int64;
  VByte: Byte;
  VWord: Word;
  VCardinal: Cardinal;
  VUInt64: UInt64;
  VBoolean: Boolean;
begin
  { Signed integer boundaries }
  CheckEqualsInt64(-128, Low(VShortInt));
  CheckEqualsInt64(127, High(VShortInt));
  CheckEqualsInt64(-32768, Low(VSmallInt));
  CheckEqualsInt64(32767, High(VSmallInt));
  CheckEqualsInt64(-2147483648, Low(VInteger));
  CheckEqualsInt64(2147483647, High(VInteger));
  CheckEqualsInt64(-9223372036854775808, Low(VInt64));
  CheckEqualsInt64(9223372036854775807, High(VInt64));

  { Unsigned integer boundaries }
  CheckEqualsUInt64(0, Low(VByte));
  CheckEqualsUInt64(255, High(VByte));
  CheckEqualsUInt64(0, Low(VWord));
  CheckEqualsUInt64(65535, High(VWord));
  CheckEqualsUInt64(0, Low(VCardinal));
  CheckEqualsUInt64(4294967295, High(VCardinal));
  CheckEqualsUInt64(0, Low(VUInt64));
  CheckEqualsUInt64(18446744073709551615, High(VUInt64));

  { Same boundaries using hex literals }
  CheckEqualsInt64(-$80, Low(VShortInt));
  CheckEqualsInt64($7F, High(VShortInt));
  CheckEqualsInt64(-$8000, Low(VSmallInt));
  CheckEqualsInt64($7FFF, High(VSmallInt));
  CheckEqualsInt64(-$80000000, Low(VInteger));
  CheckEqualsInt64($7FFFFFFF, High(VInteger));
  CheckEqualsInt64(-$8000000000000000, Low(VInt64)); { also see special case below }
  CheckEqualsInt64($7FFFFFFFFFFFFFFF, High(VInt64));
  CheckEqualsUInt64($0, Low(VByte));
  CheckEqualsUInt64($FF, High(VByte));
  CheckEqualsUInt64($0, Low(VWord));
  CheckEqualsUInt64($FFFF, High(VWord));
  CheckEqualsUInt64($0, Low(VCardinal));
  CheckEqualsUInt64($FFFFFFFF, High(VCardinal));
  CheckEqualsUInt64($0, Low(VUInt64));
  CheckEqualsUInt64($FFFFFFFFFFFFFFFF, High(VUInt64));

  { Special case, works like this in Delphi as well }
  CheckEqualsInt64($8000000000000000, Low(VInt64));

  { Boolean value mapping }
  VBoolean := False;
  CheckEqualsUInt64(0, Ord(VBoolean));
  VBoolean := True;
  CheckEqualsUInt64(1, Ord(VBoolean));
end;

procedure Test_CurrencyPrecision;
var
  C: Currency;
begin
  C := 1.5;
  CheckEqualsFloat(1.5, C, 0);
  C := 0.0001;
  CheckEqualsFloat(0.0001, C, 1e-18);
end;

procedure Test_BooleanLikeTypes;
var
  BB: ByteBool;
  WB: WordBool;
  LB: LongBool;
begin
  BB := True;
  CheckTrue(BB);
  BB := False;
  CheckFalse(BB);

  WB := True;
  CheckTrue(WB);
  WB := False;
  CheckFalse(WB);

  LB := False;
  CheckFalse(LB);
  LB := True;
  CheckTrue(LB);
end;

type
  TSmall = (eA, eB, eC);
  TByteSet = set of Byte;

function GetByteSetCount(const S: TByteSet): Integer;
var
  I: Byte;
begin
  Result := 0;
  for I := 0 to 255 do
    if I in S then
      Result := Result + 1;
end;

procedure Test_EnumerationsAndSets;
var
  VTSmall: TSmall;
  S, T: TByteSet;
begin
  { Enumerations }
  CheckEqualsUInt64(0, Ord(eA));
  VTSmall := eA;
  CheckEqualsUInt64(0, Ord(VTSmall));
  VTSmall := eC;
  CheckEqualsUInt64(2, Ord(VTSmall));
  CheckEqualsInt64(1, SizeOf(VTSmall));
  VTSmall := TSmall(2);
  CheckEqualsUInt64(2, Ord(VTSmall));

  { Sets }
  S := [1, 3, 5];
  T := [3, 4];
  //CheckTrue(3 in S); { Skipped: not supported }
  CheckTrue(Byte(3) in S);
  CheckTrue(not (Byte(4) in S));
  { ROPS bug: 'for I: Byte := 0 to 255' infinite-loops because the
    loop variable wraps from 255 to 0 instead of terminating.
    Re-enable once the for-loop overflow is fixed. }
  CheckEqualsInt64(4, GetByteSetCount(S + T));
  CheckEqualsInt64(1, GetByteSetCount(S * T));
  CheckEqualsInt64(2, GetByteSetCount(S - T));
  CheckTrue(S = [1, 3, 5]);
  //CheckTrue([1, 3] <= S); { Skipped: not supported }
  T := [1, 3];
  CheckTrue(T <= S);

  { Include/Exclude }
  S := [];
  Include(S, Byte(7));
  CheckTrue(Byte(7) in S);
  Exclude(S, Byte(7));
  CheckFalse(Byte(7) in S);

  { Skipped: '..' range syntax in set literals not supported }
end;

procedure RunAllTests;
begin
  Test_Lexical;
  Test_BaseTypeSizes;
  Test_ArchDependentSizes;
  Test_IntegerBoundaries;
  Test_CurrencyPrecision;
  Test_BooleanLikeTypes;
  Test_EnumerationsAndSets;
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
  Log(ResultText);
  SaveStringToFile(ExpandConstant('{src}\Script.Test-Result.txt'), ResultText, False);
  Result := False;
end;
