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
  { Other keywords: exercised by other tests }
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
  RA: array of TRec;
begin
  { Records - field layout and copy }
  R1.A := 42;
  R1.B := 'hello';
  R1.C := 1.5;
  R2 := R1;
  CheckEqualsInt64(42, R2.A);
  CheckEqualsString('hello', R2.B);
  CheckEqualsFloat(1.5, R2.C, 0.0);
  CheckTrue(R1 = R2);
  R2.A := 43;
  CheckTrue(R1 <> R2);
  R2.A := 42;
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

  { Record array grow initializes fields }
  SetLength(RA, 1);
  RA[0].A := 42;
  RA[0].B := 'test';
  SetLength(RA, 3);
  CheckEqualsInt64(42, RA[0].A);
  CheckEqualsString('test', RA[0].B);
  CheckEqualsInt64(0, RA[2].A);
  CheckEqualsInt64(0, Length(RA[2].B));
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

  { Mixed-type comparisons }
  VByte := 200;
  VInt64 := 201;
  CheckTrue(VByte < VInt64);
  VSingle := 1.5;
  VDouble := 2.5;
  CheckTrue(VSingle < VDouble);
end;

procedure Test_StringTypeInteractions;
var
  VAnsiString, VAnsiString2: AnsiString;
  VString: String;
  VWideString: WideString;
  VUnicodeString, VUnicodeString2: UnicodeString;
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

  { UnicodeString equality }
  VUnicodeString := 'hello';
  VUnicodeString2 := 'hel' + 'lo';
  CheckTrue(VUnicodeString = VUnicodeString2);
  CheckTrue(VUnicodeString <> 'hellp');

  { String comparison }
  CheckTrue('a' = 'a');
  CheckTrue('a' <> 'A');
  CheckTrue('aa' < 'ab');
  CheckTrue('aa' <= 'ab');
  CheckTrue('z' > 'a');
  CheckTrue('z' >= 'a');
  VString := 'aa';
  CheckTrue(VString < 'ab');
  CheckTrue('ab' > VString);

  { String concatenation }
  CheckEqualsString('hello world', 'hello ' + 'world');
  CheckEqualsString('A1', 'A' + IntToStr(1));
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
  GlobalVarBoolean: Boolean;
  GlobalVarEnum: TSmall;
  GlobalVarSet: TSmallSet;
  GlobalVarStaticArray: array[1..2] of Integer;
  GlobalVarExtended: Extended;
  GlobalVarCurrency: Currency;
  GlobalVarString: String;
  GlobalVarArray: array of Integer;

procedure Test_CheckGlobalsAndLocalsZeroed;
var
  I: Integer;
  B: Boolean;
  E: TSmall;
  S: TSmallSet;
  SA: array[1..2] of Integer;
  X: Extended;
  C: Currency;
  R: TRec;
  V: Variant;
  DA: array of Integer;
begin
  { Global var initial values }
  CheckEqualsInt64(0, GlobalVarInteger);
  CheckFalse(GlobalVarBoolean);
  CheckEqualsInt64(0, Ord(GlobalVarEnum));
  CheckTrue(GlobalVarSet = []);
  CheckEqualsInt64(0, GlobalVarStaticArray[1]);
  CheckEqualsInt64(0, GlobalVarStaticArray[2]);
  CheckEqualsFloat(0.0, GlobalVarExtended, 0.0);
  CheckEqualsFloat(0.0, GlobalVarCurrency, 0.0);
  CheckEqualsInt64(0, Length(GlobalVarString));
  CheckEqualsInt64(0, Length(GlobalVarArray));

  { ROPS zero-initializes all types, unlike Delphi, so test various }
  CheckEqualsInt64(0, I);
  CheckFalse(B);
  CheckEqualsInt64(0, Ord(E));
  CheckTrue(S = []);
  CheckEqualsInt64(0, SA[1]);
  CheckEqualsInt64(0, SA[2]);
  CheckEqualsFloat(0.0, X, 0.0);
  CheckEqualsFloat(0.0, C, 0.0);
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

  { Comma-separated with: last wins }
  R1.A := 0;
  R2.A := 0;
  with R1, R2 do
    A := 42;
  CheckEqualsInt64(0, R1.A);
  CheckEqualsInt64(42, R2.A);
end;

procedure Test_IntegerArithmeticAndPrecedence;
var
  VInteger: Integer;
  VExtended: Extended;
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
  VInteger := 10;
  CheckEqualsInt64(-10, -VInteger);
  CheckEqualsInt64(-11, not VInteger);

  { Operator precedence: multiplicative binds tighter than additive }
  CheckEqualsInt64(14, 2 + 3 * 4);
  CheckEqualsInt64(10, 2 * 3 + 4);
  CheckEqualsInt64(7, 1 + 12 div 2);
  CheckEqualsInt64(3, 1 + 8 mod 3);

  { 'and' is multiplicative, 'or' is additive }
  CheckTrue(True or True and False); { emits warnings }
  CheckFalse(False and True or False); { emits warnings }

  { 'not' (unary) binds tighter than 'and' (multiplicative) }
  CheckFalse(not True and True); { emits warning }
  CheckTrue(not False and True); { emits warning }

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

  { / on int operands behaves like div) }
  CheckEqualsInt64(2, 5 / 2);

  { Real promotion }
  CheckEqualsFloat(2.5, 5 / 2.0, 1e-9);
  CheckEqualsFloat(2.5, 5.0 / 2, 1e-9);
  VExtended := 1 + 1.5;
  CheckEqualsFloat(2.5, VExtended, 1e-9);
end;

procedure Test_Int64Arithmetic;
var
  VInt64, VInt64_2,VInt64High: Int64;
  VUInt64: UInt64;
begin
  { Int64 arithmetic and boundaries }
  VInt64 := 1000000000;
  VInt64_2 := VInt64 * VInt64;
  CheckTrue(VInt64_2 > 0);
  CheckEqualsInt64(0, VInt64_2 - VInt64*VInt64);

  { UInt64 arithmetic and unsigned comparison }
  VInt64High := High(VInt64);
  VUInt64 := UInt64(VInt64High) + UInt64(2);
  CheckTrue(VUInt64 > UInt64(VInt64High));
  CheckEqualsInt64(2, Int64(VUInt64 - UInt64(VInt64High)));
  CheckTrue(UInt64(0) - UInt64(1) = UInt64($FFFFFFFFFFFFFFFF));

  { Int64 shifts }
  VInt64 := Int64(1);
  CheckEqualsInt64($100000000, VInt64 shl 32);
  VInt64 := Int64($100000000);
  CheckEqualsInt64(1, VInt64 shr 32);
end;

var
  Test_BooleanShortCircuit_SideEffectCalled: Boolean;

function Test_BooleanShortCircuit_SideEffect: Boolean;
begin
  Test_BooleanShortCircuit_SideEffectCalled := True;
  Result := True;
end;

procedure Test_BooleanShortCircuit;
var
  LeftSide: Boolean;
begin
  { and short-circuits when LHS is False }
  Test_BooleanShortCircuit_SideEffectCalled := False;
  LeftSide := False;
  if LeftSide and Test_BooleanShortCircuit_SideEffect then ;
  CheckFalse(Test_BooleanShortCircuit_SideEffectCalled);

  { or short-circuits when LHS is True }
  Test_BooleanShortCircuit_SideEffectCalled := False;
  LeftSide := True;
  if LeftSide or Test_BooleanShortCircuit_SideEffect then ;
  CheckFalse(Test_BooleanShortCircuit_SideEffectCalled);

  { and evaluates RHS when LHS is True }
  Test_BooleanShortCircuit_SideEffectCalled := False;
  LeftSide := True;
  if LeftSide and Test_BooleanShortCircuit_SideEffect then ;
  CheckTrue(Test_BooleanShortCircuit_SideEffectCalled);

  { or evaluates RHS when LHS is False }
  Test_BooleanShortCircuit_SideEffectCalled := False;
  LeftSide := False;
  if LeftSide or Test_BooleanShortCircuit_SideEffect then ;
  CheckTrue(Test_BooleanShortCircuit_SideEffectCalled);
end;

procedure Test_ExplicitTypeCasts;
var
  VInt64: Int64;
  VByte: Byte;
  VDouble: Double;
  VSingle: Single;
begin
  { Byte -> Int64 via explicit cast }
  VByte := 200;
  VInt64 := Int64(VByte);
  CheckEqualsInt64(200, VInt64);

  { Double -> Single (value fits) }
  VDouble := 1.5;
  VSingle := Single(VDouble);
  CheckEqualsFloat(1.5, VSingle, 1e-6);

  { Single -> Double }
  VSingle := 3.14;
  VDouble := Double(VSingle);
  CheckEqualsFloat(3.14, VDouble, 1e-5);

  { String type casts covered by Test_StringTypeInteractions }
end;

procedure Test_ConstantExpressionCasts;
begin
  { integer constant narrowing }
  CheckEqualsUInt64(42, Byte(256 + 42));
  CheckEqualsInt64(7, Integer(Int64($100000007)));

  { integer constant widening }
  CheckEqualsInt64(200, Int64(Byte(200)));

  { enum <-> integer constants }
  CheckEqualsUInt64(2, Ord(TSmall(2)));
  CheckEqualsInt64(2, Integer(eC));
  CheckEqualsUInt64(0, Byte(eA));

  { float constant casts }
  CheckEqualsFloat(1.5, Double(1.5), 0.0);
  CheckEqualsFloat(1.5, Single(1.5), 1e-6);
end;

function Test_IdentifierCaseInsensitivity_Func: Integer;
begin
  Result := 7;
end;

procedure Test_IdentifierCaseInsensitivity;
var
  FooBar: Integer;
begin
  { lowercase access }
  FooBar := 42;
  CheckEqualsInt64(42, foobar);

  { uppercase write, mixed-case read }
  FOOBAR := 99;
  CheckEqualsInt64(99, FooBar);

  { function names are case-insensitive }
  CheckEqualsInt64(7, test_identifiercaseinsensitivity_func);
  CheckEqualsInt64(7, TEST_IDENTIFIERCASEINSENSITIVITY_FUNC);
end;

procedure Test_IfElse;
var
  I: Integer;
begin
  { Basic if/then/else }
  if True then I := 1 else I := 2;
  CheckEqualsInt64(1, I);

  if False then I := 10 else I := 20;
  CheckEqualsInt64(20, I);

  { Dangling else binds to nearest if }
  I := 0;
  if True then
    if False then
      I := 1
    else
      I := 2;
  CheckEqualsInt64(2, I);

  { Outer else when outer condition is false }
  I := 0;
  if False then
    if True then
      I := 1
    else
      I := 2
  else
    I := 3;
  CheckEqualsInt64(3, I);
end;

procedure Test_WhileLoop;
var
  I, Count: Integer;
begin
  I := 0;
  Count := 0;
  while I < 5 do begin
    Count := Count + 1;
    I := I + 1;
  end;
  CheckEqualsInt64(5, Count);
  CheckEqualsInt64(5, I);

  { False condition never enters body }
  Count := 0;
  while False do
    Count := Count + 1;
  CheckEqualsInt64(0, Count);
end;

procedure Test_RepeatUntil;
var
  Count: Integer;
begin
  { Body runs at least once even when condition starts true }
  Count := 0;
  repeat
    Count := Count + 1;
  until True;
  CheckEqualsInt64(1, Count);

  { Normal repeat until }
  Count := 0;
  repeat
    Count := Count + 1;
  until Count = 5;
  CheckEqualsInt64(5, Count);
end;

procedure Test_ForLoop;
var
  I, Sum: Integer;
begin
  { for..to counts correctly }
  Sum := 0;
  for I := 1 to 5 do
    Sum := Sum + I;
  CheckEqualsInt64(15, Sum);

  { for..downto counts correctly }
  Sum := 0;
  for I := 5 downto 1 do
    Sum := Sum + I;
  CheckEqualsInt64(15, Sum);

  { for..to preserves direction }
  Sum := 0;
  for I := 1 to 3 do
    Sum := Sum * 10 + I;
  CheckEqualsInt64(123, Sum);

  { for..downto preserves direction }
  Sum := 0;
  for I := 3 downto 1 do
    Sum := Sum * 10 + I;
  CheckEqualsInt64(321, Sum);

  { Empty body for..to: post-loop value is H + 1 }
  for I := 0 to 3 do
    ;
  CheckEqualsInt64(4, I);

  { Empty body for..downto: post-loop value is L - 1 }
  for I := 3 downto 0 do
    ;
  CheckEqualsInt64(-1, I);
end;

procedure Test_VariantControlFlow;
var
  V: Variant;
  Sum: Integer;
begin
  { if Variant then }
  V := True;
  if V then
    Sum := 1
  else
    Sum := 0;
  CheckEqualsInt64(1, Sum);

  { while Variant do }
  V := Integer(3);
  Sum := 0;
  while V > 0 do begin
    Sum := Sum + V;
    V := V - 1;
  end;
  CheckEqualsInt64(6, Sum);

  { repeat until Variant }
  V := Integer(0);
  Sum := 0;
  repeat
    Sum := Sum + 1;
    V := Sum >= 3;
  until V;
  CheckEqualsInt64(3, Sum);

  { for Variant }
  Sum := 0;
  for V := 1 to 5 do
    Sum := Sum + V;
  CheckEqualsInt64(6, V);
  CheckEqualsInt64(15, Sum);
end;

procedure Test_CaseStatement_CaseTest(X: Integer; var S: String);
begin
  case X of
    0:    S := 'zero';
    1, 2: S := 'small';
    3..5: S := 'mid';
  else
    S := 'big';
  end;
end;

procedure Test_CaseStatement;
var
  S: String;
begin
  Test_CaseStatement_CaseTest(0, S);
  CheckEqualsString('zero', S);
  Test_CaseStatement_CaseTest(1, S);
  CheckEqualsString('small', S);
  Test_CaseStatement_CaseTest(2, S);
  CheckEqualsString('small', S);
  Test_CaseStatement_CaseTest(4, S);
  CheckEqualsString('mid', S);
  Test_CaseStatement_CaseTest(6, S);
  CheckEqualsString('big', S);
end;

procedure Test_CaseElseMultipleStatements_CaseElseMulti(X: Integer; var Sum: Integer);
begin
  Sum := 0;
  case X of
    1: Sum := 100;
  else
    Sum := Sum + 10;
    Sum := Sum + 20;
    Sum := Sum + 30;
  end;
end;

procedure Test_CaseElseMultipleStatements;
var
  R: Integer;
begin
  Test_CaseElseMultipleStatements_CaseElseMulti(99, R);
  CheckEqualsInt64(60, R);

  Test_CaseElseMultipleStatements_CaseElseMulti(1, R);
  CheckEqualsInt64(100, R);
end;

procedure Test_GotoLabel;
label SkipForward, BackLoop, SkipFromIf;
var
  I, Count: Integer;
begin
  { Forward goto }
  I := 0;
  goto SkipForward;
  I := 1;
  SkipForward:
  CheckEqualsInt64(0, I);

  { Backward goto simulating a loop }
  Count := 0;
  BackLoop:
  Count := Count + 1;
  if Count < 3 then
    goto BackLoop;
  CheckEqualsInt64(3, Count);

  { goto out of nested if }
  I := 0;
  if True then begin
    if True then
      goto SkipFromIf;
    I := 1;
  end;
  SkipFromIf:
  CheckEqualsInt64(0, I);
end;

function Test_ExitEarlyReturn_EarlyExit(X: Integer): Integer;
begin
  Result := -1;
  if X > 0 then Exit;
  Result := X;
end;

procedure Test_ExitEarlyReturn;
begin
  CheckEqualsInt64(-1, Test_ExitEarlyReturn_EarlyExit(5));
  CheckEqualsInt64(-3, Test_ExitEarlyReturn_EarlyExit(-3));
end;

{ Forward declaration: A is declared forward, B calls A before A's body }
procedure Test_ForwardDeclarations_A; forward;

procedure Test_ForwardDeclarations_B(var R: Integer);
begin
  Test_ForwardDeclarations_A;
  R := 1;
end;

procedure Test_ForwardDeclarations_A;
begin
  { Body provided after forward declaration }
end;

procedure Test_ForwardDeclarations;
var
  R: Integer;
begin
  R := 0;
  Test_ForwardDeclarations_B(R);
  CheckEqualsInt64(1, R);
  Test_ForwardDeclarations_A;
end;

function Test_FunctionResults_Int: Integer;
begin
  Result := 42;
end;

function Test_FunctionResults_Int64: Int64;
begin
  Result := 12345678901;
end;

function Test_FunctionResults_Single: Single;
begin
  Result := 1.5;
end;

function Test_FunctionResults_Double: Double;
begin
  Result := 1.5;
end;

function Test_FunctionResults_String: String;
begin
  Result := 'hello';
end;

function Test_FunctionResults_AnsiString: AnsiString;
begin
  Result := 'a';
end;

function Test_FunctionResults_Boolean: Boolean;
begin
  Result := True;
end;

function Test_FunctionResults_Variant: Variant;
begin
  Result := 'v';
end;

function Test_FunctionResults_Rec: TRec;
begin
  Result.A := 1;
  Result.B := 's';
  Result.C := 1.0;
end;

function Test_FunctionResults_Arr: TIntArr;
begin
  SetLength(Result, 3);
  Result[0] := 1;
  Result[1] := 2;
  Result[2] := 3;
end;

procedure Test_FunctionResults;
var
  Arr: TIntArr;
  Rec: TRec;
begin
  CheckEqualsInt64(42, Test_FunctionResults_Int);
  CheckEqualsInt64(12345678901, Test_FunctionResults_Int64);
  CheckEqualsFloat(1.5, Test_FunctionResults_Single, 0.0);
  CheckEqualsFloat(1.5, Test_FunctionResults_Double, 0.0);
  CheckEqualsString('hello', Test_FunctionResults_String);
  CheckEqualsString('a', Test_FunctionResults_AnsiString);
  CheckTrue(Test_FunctionResults_Boolean);
  CheckEqualsString('v', Test_FunctionResults_Variant);

  Rec := Test_FunctionResults_Rec;
  CheckEqualsInt64(1, Rec.A);
  CheckEqualsString('s', Rec.B);
  CheckEqualsFloat(1.0, Rec.C, 0.0);

  { Record field directly from function result }
  if Test_FunctionResults_Rec.A = 1 then
    CheckTrue(True)
  else
    RaiseException('FRec.A field comparison broken');
  CheckEqualsString('s', Test_FunctionResults_Rec.B);

  Arr := Test_FunctionResults_Arr;
  CheckEqualsInt64(3, Length(Arr));
  CheckEqualsInt64(1, Arr[0]);
  CheckEqualsInt64(2, Arr[1]);
  CheckEqualsInt64(3, Arr[2]);
end;

function Test_Recursion_Fact(N: Integer): Int64;
begin
  if N <= 1 then
    Result := 1
  else
    Result := N * Test_Recursion_Fact(N - 1);
end;

procedure Test_Recursion;
begin
  CheckEqualsInt64(1, Test_Recursion_Fact(0));
  CheckEqualsInt64(1, Test_Recursion_Fact(1));
  CheckEqualsInt64(120, Test_Recursion_Fact(5));
end;

procedure Test_RegisteredProcs;
begin
  { String -> String }
  CheckEqualsString('abc', Trim('  abc  '));

  { String -> Integer }
  CheckEqualsInt64(3, Length('abc'));
  CheckEqualsInt64(42, StrToInt('42'));

  { Integer -> String }
  CheckEqualsString('42', IntToStr(42));

  { String, String -> Integer }
  CheckEqualsInt64(0, CompareText('abc', 'ABC'));

  { Char, Integer -> String }
  CheckEqualsString('AAA', StringOfChar('A', 3));

  { String, array of const -> String }
  CheckEqualsString('x=1 y=hi', Format('x=%d y=%s', [1, 'hi']));
end;

procedure Test_RegisteredMethods;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    { Method with String param returning Integer (Add) }
    List.Add('cherry');
    List.Add('apple');
    List.Add('banana');

    { Read-only Integer property (Count) }
    CheckEqualsInt64(3, List.Count);

    { Indexed String property (Strings[]) }
    CheckEqualsString('cherry', List[0]);

    { Boolean property write then read (Sorted) }
    List.Sorted := True;
    CheckTrue(List.Sorted);
    CheckEqualsString('apple', List[0]);

    { Method with String param returning Integer on sorted list (IndexOf) }
    CheckEqualsInt64(2, List.IndexOf('cherry'));

    { String property write then read (CommaText) }
    CheckEqualsString('apple,banana,cherry', List.CommaText);

    { Method with Integer param, no return (Delete) }
    List.Delete(0);
    CheckEqualsInt64(2, List.Count);
    CheckEqualsString('banana', List[0]);
  finally
    List.Free;
  end;
end;

var
  GlobalForIdentRes: Integer;

function Test_IdentifierResolution_ReadLocal: Integer;
var
  GlobalForIdentRes: Integer;
begin
  GlobalForIdentRes := 99;
  Result := GlobalForIdentRes;
end;

procedure Test_IdentifierResolution;
begin
  GlobalForIdentRes := 1;
  CheckEqualsInt64(99, Test_IdentifierResolution_ReadLocal);
  { Global was not modified by the local }
  CheckEqualsInt64(1, GlobalForIdentRes);
end;

{ External DLL declarations - compile-only witness, not called }
procedure Test_ExternalDll_Default;   external 'GetLastError@kernel32.dll';
procedure Test_ExternalDll_Register;  external 'GetLastError@kernel32.dll register';
procedure Test_ExternalDll_Pascal;    external 'GetLastError@kernel32.dll pascal';
procedure Test_ExternalDll_Cdecl;     external 'GetLastError@kernel32.dll cdecl';
procedure Test_ExternalDll_Stdcall;   external 'GetLastError@kernel32.dll stdcall';
procedure Test_ExternalDll_Delay;     external 'GetLastError@kernel32.dll stdcall delayload';
procedure Test_ExternalDll_AltSearch; external 'GetLastError@kernel32.dll stdcall loadwithalteredsearchpath';
procedure Test_ExternalDll_BothOpts;  external 'GetLastError@kernel32.dll stdcall delayload loadwithalteredsearchpath';
procedure Test_ExternalDll_QuotedDll; external 'GetLastError@"kernel32.dll" stdcall';

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
  Test_Int64Arithmetic;
  Test_BooleanShortCircuit;
  Test_ExplicitTypeCasts;
  Test_ConstantExpressionCasts;
  Test_IdentifierCaseInsensitivity;
  Test_IfElse;
  Test_WhileLoop;
  Test_RepeatUntil;
  Test_ForLoop;
  Test_VariantControlFlow;
  Test_CaseStatement;
  Test_CaseElseMultipleStatements;
  Test_GotoLabel;
  Test_ExitEarlyReturn;
  Test_ForwardDeclarations;
  Test_FunctionResults;
  Test_Recursion;
  Test_RegisteredProcs;
  Test_RegisteredMethods;
  Test_IdentifierResolution;
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
