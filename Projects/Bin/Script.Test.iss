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
    unit, interface, implementation, uses, finalization, initialization, class,
    inherited, constructor, destructor, virtual, override, private, protected,
    public, published, '^' dereference token. }

  { Numeric literals }
  CheckEqualsInt64(255, $FF);
  CheckEqualsInt64(255, 0 + $FF);
  CheckEqualsFloat(1.5e2, 150.0, 0.0);
  CheckEqualsFloat(1.5E+2, 150.0, 0.0);
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
  CheckEqualsInt64($0100, Ord(#$0100));

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
  VAnsiString: AnsiString;
  VString: String;
  VWideString: WideString;
  VUnicodeString: UnicodeString;
  VNativeString: NativeString;
  VPAnsiChar: PAnsiChar;
  VDynArray: array of Integer;
begin
  #if arch == "x64"
    CheckEqualsInt64(8, SizeOf(VExtended));
    CheckEqualsInt64(8, SizeOf(VNativeInt));
    CheckEqualsInt64(8, SizeOf(VNativeUInt));
    CheckEqualsInt64(8, SizeOf(VAnsiString));
    CheckEqualsInt64(8, SizeOf(VString));
    CheckEqualsInt64(8, SizeOf(VWideString));
    CheckEqualsInt64(8, SizeOf(VUnicodeString));
    CheckEqualsInt64(8, SizeOf(VNativeString));
    CheckEqualsInt64(8, SizeOf(VPAnsiChar));
    CheckEqualsInt64(8, SizeOf(VDynArray));
  #else
    CheckEqualsInt64(10, SizeOf(VExtended));
    CheckEqualsInt64(4, SizeOf(VNativeInt));
    CheckEqualsInt64(4, SizeOf(VNativeUInt));
    CheckEqualsInt64(4, SizeOf(VAnsiString));
    CheckEqualsInt64(4, SizeOf(VString));
    CheckEqualsInt64(4, SizeOf(VWideString));
    CheckEqualsInt64(4, SizeOf(VUnicodeString));
    CheckEqualsInt64(4, SizeOf(VNativeString));
    CheckEqualsInt64(4, SizeOf(VPAnsiChar));
    CheckEqualsInt64(4, SizeOf(VDynArray));
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
  VByteBool: ByteBool;
  VWordBool: WordBool;
  VLongBool: LongBool;
begin
  VByteBool := True;
  CheckTrue(VByteBool);
  VByteBool := False;
  CheckFalse(VByteBool);

  VWordBool := True;
  CheckTrue(VWordBool);
  VWordBool := False;
  CheckFalse(VWordBool);

  VLongBool := False;
  CheckFalse(VLongBool);
  VLongBool := True;
  CheckTrue(VLongBool);
end;

type
  TSmall = (eA, eB, eC);
  TByteSet = set of Byte;
  TSmallSet = set of TSmall;

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
  VTByteSet, VTByteSet2: TByteSet;
  VTSmallSet: TSmallSet;
begin
  { Enumerations }
  CheckEqualsUInt64(0, Ord(eA));
  VTSmall := eA;
  CheckEqualsUInt64(0, Ord(VTSmall));
  VTSmall := eC;
  CheckEqualsUInt64(2, Ord(VTSmall));
  VTSmall := TSmall(2);
  CheckEqualsUInt64(2, Ord(VTSmall));

  { Sets }
  VTByteSet := [1, 3, 5];
  VTByteSet2 := [3, 4];
  //CheckTrue(3 in VTByteSet); { Skipped: not supported }
  CheckTrue(Byte(3) in VTByteSet);
  CheckTrue(not (Byte(4) in VTByteSet));
  CheckEqualsInt64(4, GetByteSetCount(VTByteSet + VTByteSet2));
  CheckEqualsInt64(1, GetByteSetCount(VTByteSet * VTByteSet2));
  CheckEqualsInt64(2, GetByteSetCount(VTByteSet - VTByteSet2));
  CheckTrue(VTByteSet = [1, 3, 5]);
  //CheckTrue([1, 3] <= VTByteSet); { Skipped: not supported }
  VTByteSet2 := [1, 3];
  CheckTrue(VTByteSet2 <= VTByteSet);

  { Enum-backed sets }
  VTSmallSet := [eA, eC];
  CheckTrue(eA in VTSmallSet);
  CheckTrue(not (eB in VTSmallSet));
  Include(VTSmallSet, eB);
  CheckTrue(eB in VTSmallSet);
  VTSmallSet := VTSmallSet - [eA];
  CheckFalse(eA in VTSmallSet);

  { Include/Exclude }
  VTByteSet := [];
  Include(VTByteSet, Byte(7));
  CheckTrue(Byte(7) in VTByteSet);
  Exclude(VTByteSet, Byte(7));
  CheckFalse(Byte(7) in VTByteSet);

  { Enum and set sizes }
  CheckEqualsInt64(1, SizeOf(VTSmall));
  CheckEqualsInt64(32, SizeOf(VTByteSet));
  CheckEqualsInt64(1, SizeOf(VTSmallSet));

  { Skipped: '..' range syntax in set literals not supported }
end;

type
  TRec = record
    A: Integer;
    B: String;
    C: Double;
  end;
  TIntArr = array of Integer;
  T2D = array of array of Integer;
  { Skipped: 'type Integer' syntax not supported }

procedure Test_RecordsAndArrays;
var
  R1, R2: TRec;
  SA: array[-2..2] of Integer;
  DA: array of Integer;
  DA2: TIntArr;
  M: T2D;
begin
  { Records - field layout and copy }
  R1.A := 42;
  R1.B := 'hello';
  R1.C := 1.5;
  R2 := R1;
  CheckEqualsInt64(42, R2.A);
  CheckEqualsString('hello', R2.B);
  CheckEqualsFloat(1.5, R2.C, 0.0);
  #if arch == "x64"
    CheckEqualsInt64(20, SizeOf(R1));
  #else
    CheckEqualsInt64(16, SizeOf(R1));
  #endif

  { Static arrays with non-zero low bound }
  SA[-2] := 100;
  SA[2] := 500;
  CheckEqualsInt64(100, SA[-2]);
  CheckEqualsInt64(-2, Low(SA));
  CheckEqualsInt64(2, High(SA));
  CheckEqualsInt64(5, Length(SA));

  { Dynamic arrays }
  CheckEqualsInt64(0, Length(DA));
  SetLength(DA, 4);
  CheckEqualsInt64(4, Length(DA));
  CheckEqualsInt64(3, High(DA));
  CheckEqualsInt64(0, Low(DA));
  DA[2] := 7;
  CheckEqualsInt64(7, DA[2]);

  { Dynamic array grow preserves contents and zero-inits new slots }
  SetLength(DA, 6);
  CheckEqualsInt64(6, Length(DA));
  CheckEqualsInt64(7, DA[2]);
  CheckEqualsInt64(0, DA[5]);

  { Dynamic array shrink to zero }
  SetLength(DA, 0);
  CheckEqualsInt64(0, Length(DA));

  { Dynamic array literals }
  DA2 := [10, 20, 30];
  CheckEqualsInt64(3, Length(DA2));
  CheckEqualsInt64(10, DA2[0]);
  CheckEqualsInt64(30, DA2[2]);

  { Multi-dimensional arrays }
  SetLength(M, 2);
  SetLength(M[0], 3);
  SetLength(M[1], 3);
  M[0][0] := 1;
  M[0][1] := 2;
  M[0][2] := 3;
  M[1][0] := 4;
  M[1][1] := 5;
  M[1][2] := 6;
  CheckEqualsInt64(6, M[1][2]);
  CheckEqualsInt64(3, Length(M[0]));
end;

type
  TIntAlias = Integer;

procedure Test_TypeAliases;
var
  VIntAlias: TIntAlias;
begin
  VIntAlias := 5;
  CheckEqualsInt64(5, VIntAlias);
end;

procedure Test_ParameterMode_Basic(X: Integer);
begin
  X := 99;
end;

procedure Test_ParameterMode_Var(var X: Integer);
begin
  X := 99;
end;

procedure Test_ParameterMode_Out(out X: Integer);
begin
  { ROPS does not clear out params on entry (unlike Delphi) }
  CheckEqualsInt64(1, X);
  X := 99;
end;

procedure Test_ParameterMode_Const(const X: Integer);
begin
end;

procedure Test_ParameterModes;
var
  V: Integer;
begin
  V := 1;
  Test_ParameterMode_Basic(V);
  CheckEqualsInt64(1, V);

  V := 1;
  Test_ParameterMode_Var(V);
  CheckEqualsInt64(99, V);

  V := 1;
  Test_ParameterMode_Out(V);
  CheckEqualsInt64(99, V);

  V := 1;
  Test_ParameterMode_Const(V);
  CheckEqualsInt64(1, V);
end;

procedure Test_ImplicitTypeWidening;
var
  VByte: Byte;
  VInteger: Integer;
  VInt64: Int64;
  VSingle: Single;
  VDouble: Double;
  VExtended: Extended;
  VCurrency: Currency;
  VChar: Char;
  VString: String;
begin
  { Integer widening chain: Byte -> Integer -> Int64 }
  VByte := 200;
  VInteger := VByte;
  CheckEqualsInt64(200, VInteger);
  VInt64 := VInteger;
  CheckEqualsInt64(200, VInt64);

  { Float widening chain: Single -> Double -> Extended }
  VSingle := 1.5;
  VDouble := VSingle;
  CheckEqualsFloat(1.5, VDouble, 1e-9);
  VExtended := VDouble;
  CheckEqualsFloat(1.5, VExtended, 1e-9);

  { Integer -> float promotion in assignment context }
  VInteger := 7;
  VDouble := VInteger;
  CheckEqualsFloat(7.0, VDouble, 0.0);
  VInt64 := 123456789;
  VExtended := VInt64;
  CheckEqualsFloat(123456789.0, VExtended, 0.5);

  { Integer -> Currency in assignment }
  VInteger := 42;
  VCurrency := VInteger;
  CheckEqualsFloat(42.0, VCurrency, 0.0);

  { Char -> String implicit conversion }
  VChar := 'X';
  VString := VChar;
  CheckEqualsString('X', VString);
end;

procedure Test_StringTypeInteractions;
var
  VAnsiString, VAnsiString2: AnsiString;
  VString: String;
  VWideString: WideString;
  VUnicodeString: UnicodeString;
  VNativeString: NativeString;
  VPAnsiChar: PAnsiChar;
begin
  { String -> AnsiString and back }
  VString := 'hello';
  VAnsiString := VString;
  CheckEqualsInt64(5, Length(VAnsiString));
  VString := VAnsiString;
  CheckEqualsString('hello', VString);

  { String -> WideString and back }
  VString := 'world';
  VWideString := VString;
  CheckEqualsInt64(5, Length(VWideString));
  VString := VWideString;
  CheckEqualsString('world', VString);

  { String -> UnicodeString and back }
  VString := 'test';
  VUnicodeString := VString;
  CheckEqualsInt64(4, Length(VUnicodeString));
  VString := VUnicodeString;
  CheckEqualsString('test', VString);

  { Cross-string-type assignment }
  VAnsiString := 'ansi';
  VWideString := VAnsiString;
  CheckEqualsString('ansi', VWideString);
  VUnicodeString := VWideString;
  CheckEqualsString('ansi', VUnicodeString);

  { NativeString maps to the native Unicode string type }
  VString := 'native';
  VNativeString := VString;
  CheckEqualsInt64(6, Length(VNativeString));
  VString := VNativeString;
  CheckEqualsString('native', VString);

  { PAnsiChar -> AnsiString round-trip }
  VAnsiString := 'pchar';
  VPAnsiChar := VAnsiString; { ROPS needs a variable to keep the string alive } 
  VAnsiString2 := VPAnsiChar;
  CheckEqualsString('pchar', VAnsiString2);
end;

procedure Test_SignedUnsignedBoundaries;
var
  VShortInt: ShortInt;
  VInteger: Integer;
  VInt64: Int64;
begin
  { Negative ShortInt -> Integer sign-extends }
  VShortInt := -128;
  VInteger := VShortInt;
  CheckEqualsInt64(-128, VInteger);

  { Negative Integer -> Int64 sign-extends }
  VInteger := -2147483648;
  VInt64 := VInteger;
  CheckEqualsInt64(-2147483648, VInt64);
end;

procedure Test_EmptyStringEdgeCases;
var
  S: String;
begin
  CheckEqualsInt64(0, Length(''));
  CheckEqualsInt64(0, Length(S));
  CheckEqualsInt64(0, Pos('x', S));
  CheckEqualsInt64(0, Pos('', 'hello'));
  CheckEqualsString('', Copy(S, 1, 5));
  CheckEqualsString('', Copy('hello', 1, 0));
  CheckEqualsString('', Copy('hello', 10, 5));
  CheckEqualsString('hello', S + 'hello');
  CheckEqualsString('hello', 'hello' + S);
end;

procedure Test_IntegerOverflowWraparound;
var
  VByte: Byte;
  VShortInt: ShortInt;
  VSmallInt: SmallInt;
  VInteger: Integer;
  VInt64: Int64;
begin
  { Direct arithmetic overflow wraps }
  VInteger := 2147483647;
  VInteger := VInteger + 1;
  CheckEqualsInt64(-2147483648, VInteger);

  VInteger := -2147483648;
  VInteger := VInteger - 1;
  CheckEqualsInt64(2147483647, VInteger);

  { Narrowing casts truncate to low bits }
  VInt64 := 256;
  VByte := Byte(VInt64);
  CheckEqualsUInt64(0, VByte);

  VInt64 := -1;
  VByte := Byte(VInt64);
  CheckEqualsUInt64(255, VByte);

  VInt64 := 128;
  VShortInt := ShortInt(VInt64);
  CheckEqualsInt64(-128, VShortInt);

  VInt64 := -129;
  VShortInt := ShortInt(VInt64);
  CheckEqualsInt64(127, VShortInt);

  VInt64 := 32768;
  VSmallInt := SmallInt(VInt64);
  CheckEqualsInt64(-32768, VSmallInt);

  VInt64 := -32769;
  VSmallInt := SmallInt(VInt64);
  CheckEqualsInt64(32767, VSmallInt);

  VInt64 := 2147483648;
  VInteger := Integer(VInt64);
  CheckEqualsInt64(-2147483648, VInteger);

  VInt64 := -2147483649;
  VInteger := Integer(VInt64);
  CheckEqualsInt64(2147483647, VInteger);
end;

const
  PI_TIMES_2 = 6.283185307;
  CA = 10;
  CB = CA + 5;
  CC = CA * CB div 3;
  CD = CA > CB;
  CE = not False;
  CF = (CA shl 2) or $0F;
  CG = CA mod 3;

procedure Test_ConstantsAndConstantExpressions;
begin
  { Global constant }
  CheckEqualsFloat(PI_TIMES_2, 2 * Pi, 1e-6);

  { Constant expressions evaluated at compile time }
  CheckEqualsInt64(15, CB);
  CheckEqualsInt64(50, CC);
  CheckFalse(CD);
  CheckTrue(CE);
  CheckEqualsInt64(47, CF);
  CheckEqualsInt64(1, CG);
end;

var
  GlobalVarInteger: Integer;
  GlobalVarString: String;
  GlobalVarArray: array of Integer;

procedure Test_CheckGlobalsAndLocalsZeroed;
var
  I: Integer;
  R: TRec;
  V: Variant;
  DA: array of Integer;
begin
  { Global var initial values }
  CheckEqualsInt64(0, GlobalVarInteger);
  CheckEqualsInt64(0, Length(GlobalVarString));
  CheckEqualsInt64(0, Length(GlobalVarArray));

  { ROPS zero-initializes all types, unlike Delphi, so test various }
  CheckEqualsInt64(0, I);
  CheckEqualsInt64(0, R.A);
  CheckEqualsInt64(0, Length(R.B));
  CheckTrue(VarIsClear(V));
  CheckEqualsInt64(0, Length(DA));
  { String already tested in Test_EmptyStringEdgeCases }
end;

procedure Test_Variants;
var
  V: Variant;
begin
  V := Unassigned;
  CheckEqualsInt64(varEmpty, VarType(V));
  V := Null;
  CheckEqualsInt64(varNull, VarType(V));
  V := Integer(42);
  CheckEqualsInt64(varInteger, VarType(V));
  V := True;
  CheckEqualsInt64(varBoolean, VarType(V));
  V := Double(1.5);
  CheckEqualsInt64(varDouble, VarType(V));
  V := Int64(42);
  CheckEqualsInt64(varInt64, VarType(V));
  V := 'hello';
  CheckEqualsInt64(varUString, VarType(V));
end;

procedure Test_WithScoping;
var
  R1, R2: TRec;
begin
  R1.A := 0;
  R1.B := '';
  with R1 do begin
    A := 11;
    B := 'changed';
  end;
  CheckEqualsInt64(11, R1.A);
  CheckEqualsString('changed', R1.B);

  { Nested with: innermost wins }
  R1.A := 0;
  R2.A := 0;
  with R1 do
    with R2 do
      A := 1;
  CheckEqualsInt64(1, R2.A);
  CheckEqualsInt64(0, R1.A);
end;

procedure Test_IntegerArithmeticAndPrecedence;
begin
  { Integer arithmetic }
  CheckEqualsInt64(7, 3 + 4);
  CheckEqualsInt64(-1, 3 - 4);
  CheckEqualsInt64(12, 3 * 4);
  CheckEqualsInt64(2, 5 div 2);
  CheckEqualsInt64(1, 5 mod 2);
  CheckEqualsInt64(8, 1 shl 3);
  CheckEqualsInt64(2, 16 shr 3);
  CheckEqualsInt64(14, 10 or 6);
  CheckEqualsInt64(2, 10 and 6);
  CheckEqualsInt64(12, 10 xor 6);
  CheckEqualsInt64(-11, not 10);
  CheckEqualsInt64(-10, -(10));

  { Operator precedence: multiplicative binds tighter than additive }
  CheckEqualsInt64(14, 2 + 3 * 4);
  CheckEqualsInt64(10, 2 * 3 + 4);
  CheckEqualsInt64(7, 1 + 12 div 2);
  CheckEqualsInt64(3, 1 + 8 mod 3);

  { 'and' is multiplicative, 'or' is additive }
  CheckTrue(True or True and False);
  CheckFalse(False and True or False);

  { 'not' (unary) binds tighter than 'and' (multiplicative) }
  CheckFalse(not True and True);
  CheckTrue(not False and True);

  { Relational binds loosest }
  CheckTrue(2 + 3 = 5);
  CheckTrue(10 - 3 > 5);
  CheckTrue(2 * 3 < 2 + 5);

  { 'shl'/'shr' are multiplicative }
  CheckEqualsInt64(9, 1 + 1 shl 3);
  CheckEqualsInt64(5, 1 + 8 shr 1);

  { 'xor' is additive, same level as '+'/'-'/'or' }
  CheckEqualsInt64(4, 1 + 5 xor 2);
  CheckEqualsInt64(16, 5 xor 2 + 3 * 3);

  { Operator associativity: same-precedence operators evaluate left-to-right }
  CheckEqualsInt64(5, 10 - 3 - 2);
  CheckEqualsInt64(-4, 1 - 2 - 3);
  CheckEqualsInt64(2, 100 div 10 div 5);
  CheckEqualsInt64(0, 17 mod 5 mod 2);
  CheckEqualsInt64(32, 1 shl 1 shl 4);
  CheckEqualsInt64(6, 1 + 2 + 3);
  CheckEqualsInt64(7, 1 or 2 or 4);

  { Compound expressions mixing unary and binary operators }
  CheckEqualsInt64(-9, -(1 + 2) * 3);
  CheckEqualsInt64(7, -(-7));
  CheckEqualsInt64(2, not (not 2));
  CheckEqualsInt64(-3, -(1 + 2));
  CheckEqualsInt64(15, (1 + 2) * (3 + 2));
  CheckTrue(not (1 = 2) and (3 > 0));
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
  Test_RecordsAndArrays;
  Test_TypeAliases;
  Test_ParameterModes;
  Test_ImplicitTypeWidening;
  Test_StringTypeInteractions;
  Test_SignedUnsignedBoundaries;
  Test_EmptyStringEdgeCases;
  Test_IntegerOverflowWraparound;
  Test_ConstantsAndConstantExpressions;
  Test_CheckGlobalsAndLocalsZeroed;
  Test_Variants;
  Test_WithScoping;
  Test_IntegerArithmeticAndPrecedence;
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
