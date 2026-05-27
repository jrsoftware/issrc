#define CheckTrue(Value) \
  Value ? 0 : Error('CheckTrue test failed')
//
#define CheckFalse(Value) \
  !Value ? 0 : Error('CheckFalse test failed')
//
#define CheckEqualsInt(Expected, Actual) \
  Expected == Actual ? 0 : \
  Error('CheckEqualsInt test failed: expected ' + Str(Expected) + ', got ' + Str(Actual))
//
#define CheckEqualsString(Expected, Actual) \
  SameStr(Expected, Actual) ? 0 : \
  Error('CheckEqualsString test failed: expected "' + Expected + '", got "' + Actual + '"')
//
// Numeric literals
//
#call CheckEqualsInt(42, 42)
#call CheckEqualsInt(-1, -1)
#call CheckEqualsInt(255, 0xFF)
#call CheckEqualsInt(255, 0XFF)
#call CheckEqualsInt(0, 0x0)
#call CheckEqualsInt(0x7FFFFFFFFFFFFFFF, MaxInt)
#call CheckEqualsInt(0x8000000000000000, MinInt)
#call CheckEqualsInt(42, 42L)
#call CheckEqualsInt(42, 42U)
#call CheckEqualsInt(42, 42LU)
//
// String literals (Pascal mode)
//
#call CheckEqualsString('hello', 'hello')
#call CheckEqualsString('it''s', 'it''s')
#call CheckEqualsString("hello", "hello")
#call CheckEqualsString("hello", 'hello')
#call CheckEqualsInt(0, Len(''))
#call CheckEqualsInt(2, Len("\n"))
#call CheckFalse(SameStr("\n", NewLine))
//
// String literals (C mode)
//
#pragma parseroption -p-
#call CheckEqualsString("hello world", "hello " "world")
#call CheckEqualsString("hello world", 'hello ' 'world')
#call CheckEqualsInt(0, Len(""))
#call CheckFalse(SameStr("\0", "0"))
#call CheckFalse(SameStr("\a", "a"))
#call CheckFalse(SameStr("\b", "b"))
#call CheckFalse(SameStr("\f", "f"))
#call CheckFalse(SameStr("\n", "n"))
#call CheckTrue(SameStr("\n", NewLine))
#call CheckFalse(SameStr("\r", "r"))
#call CheckFalse(SameStr("\t", "t"))
#call CheckTrue(SameStr("\t", Tab))
#call CheckFalse(SameStr("\v", "v"))
#call CheckTrue(SameStr("\z", "z")) ; Unknown escape passes character through
#call CheckEqualsInt(1, Len("\\"))
#call CheckEqualsString("A", "\x41")
#call CheckEqualsString("A", "\101")
#pragma parseroption -p+
//
// NULL
//
#define NullVariable
#call CheckEqualsInt(TYPE_NULL, TypeOf(NullVariable))
#call CheckTrue(!NULL)
#call CheckEqualsInt(0, NULL + 0)
#call CheckEqualsString('', NULL + '')
#call CheckEqualsInt(0, NullVariable + 0)
#call CheckEqualsString('', NullVariable + '')
#call CheckEqualsInt(TYPE_NULL, TypeOf(void))
#undef NullVariable
//
// Type checking
//
#define TypeCheckInt = 42
#define TypeCheckString = 'hello'
#define TypeCheckNull
#define TypeCheckMacro() 0
#dim TypeCheckArray[1]
#call CheckEqualsInt(TYPE_INTEGER, TypeOf(TypeCheckInt))
#call CheckEqualsInt(TYPE_STRING, TypeOf(TypeCheckString))
#call CheckEqualsInt(TYPE_NULL, TypeOf(TypeCheckNull))
#call CheckEqualsInt(TYPE_MACRO, TypeOf(TypeCheckMacro))
#call CheckEqualsInt(TYPE_FUNC, TypeOf(Int))
#call CheckEqualsInt(TYPE_ERROR, TypeOf(UndefinedIdentifier_XYZ))
#call CheckEqualsInt(TYPE_ARRAY, TypeOf(TypeCheckArray))
#call CheckEqualsInt(TYPE_INTEGER, TypeOf2(1 + 1))
#call CheckEqualsInt(TYPE_STRING, TypeOf2('a' + 'b'))
#call CheckEqualsInt(TYPE_NULL, TypeOf2(NULL))
#call CheckTrue(Defined(TypeCheckInt))
#call CheckFalse(Defined(UndefinedIdentifier_XYZ))
#call CheckTrue(Defined(ISPP_INVOKED))
#call CheckTrue(Defined(WINDOWS))
#call CheckTrue(Defined(UNICODE))
#call CheckTrue(Defined(__WIN32__))
#call CheckTrue(Defined(__FILENAME__))
#call CheckTrue(Defined(PREPROCVER))
#call CheckEqualsInt(TypeOf(TypeCheckInt), TypeOf TypeCheckInt)
#call CheckEqualsInt(Defined(TypeCheckInt), Defined TypeCheckInt)
#undef TypeCheckInt
#undef TypeCheckString
#undef TypeCheckNull
#undef TypeCheckMacro
#undef TypeCheckArray
//
// Type conversion
//
#call CheckEqualsInt(42, Int('42'))
#call CheckEqualsInt(0, Int(''))
#call CheckEqualsInt(0, Int(NULL))
#call CheckEqualsInt(-1, Int('not_a_number', -1))
#call CheckEqualsString('42', Str(42))
#call CheckEqualsString('', Str(NULL))
#call CheckEqualsString('hello', Str('hello'))
#call CheckEqualsInt(42, Int(42))
//
// Arithmetic
//
#call CheckEqualsInt(5, 2 + 3)
#call CheckEqualsInt(2, 5 - 3)
#call CheckEqualsInt(6, 2 * 3)
#call CheckEqualsInt(3, 7 / 2)
#call CheckEqualsInt(1, 7 % 3)
#call CheckEqualsInt(-3, -7 / 2)
#call CheckEqualsInt(-1, -7 % 3)
//
// Bitwise
//
#define BitwiseTestValue = 0xAB
#call CheckEqualsInt(0x0F, 0xFF & 0x0F)
#call CheckEqualsInt(0xFF, 0xF0 | 0x0F)
#call CheckEqualsInt(0xF0, 0xFF ^ 0x0F)
#call CheckEqualsInt(-1, ~0)
#call CheckEqualsInt(8, 1 << 3)
#call CheckEqualsInt(2, 16 >> 3)
#call CheckEqualsInt(BitwiseTestValue, BitwiseTestValue & BitwiseTestValue)
#call CheckEqualsInt(BitwiseTestValue, BitwiseTestValue | BitwiseTestValue)
#call CheckEqualsInt(0, BitwiseTestValue ^ BitwiseTestValue)
#call CheckEqualsInt(BitwiseTestValue, ~~BitwiseTestValue)
#undef BitwiseTestValue
//
// Integer comparison
//
#call CheckEqualsInt(1, 5 == 5)
#call CheckEqualsInt(0, 5 == 6)
#call CheckEqualsInt(1, 5 != 6)
#call CheckEqualsInt(0, 5 != 5)
#call CheckEqualsInt(1, 3 < 5)
#call CheckEqualsInt(0, 5 < 3)
#call CheckEqualsInt(1, 3 <= 5)
#call CheckEqualsInt(1, 5 <= 5)
#call CheckEqualsInt(0, 5 <= 3)
#call CheckEqualsInt(1, 5 > 3)
#call CheckEqualsInt(0, 3 > 5)
#call CheckEqualsInt(1, 5 >= 3)
#call CheckEqualsInt(1, 5 >= 5)
#call CheckEqualsInt(0, 3 >= 5)
//
// String comparison
//
#call CheckEqualsInt(1, 'abc' == 'ABC')
#call CheckEqualsInt(0, 'abc' == 'def')
#call CheckEqualsInt(1, 'abc' != 'def')
#call CheckEqualsInt(0, 'abc' != 'ABC')
#call CheckEqualsInt(1, 'a' < 'b')
#call CheckEqualsInt(0, 'b' < 'a')
#call CheckEqualsInt(1, 'a' <= 'b')
#call CheckEqualsInt(1, 'a' <= 'A')
#call CheckEqualsInt(1, 'b' > 'a')
#call CheckEqualsInt(0, 'a' > 'b')
#call CheckEqualsInt(1, 'b' >= 'a')
#call CheckEqualsInt(1, 'A' >= 'a')
//
// Logical operators
//
#call CheckFalse(0 && 0)
#call CheckFalse(0 && 1)
#call CheckFalse(1 && 0)
#call CheckTrue(1 && 1)
#call CheckFalse(0 || 0)
#call CheckTrue(0 || 1)
#call CheckTrue(1 || 0)
#call CheckTrue(1 || 1)
#call CheckTrue(!!1)
#call CheckTrue(42 && 7)
#call CheckTrue(42 || 0)
//
// Unary operators
//
#define UnaryTestValue = 42
#call CheckEqualsInt(42, +UnaryTestValue)
#call CheckEqualsInt(-42, -UnaryTestValue)
#call CheckEqualsInt(0, !UnaryTestValue)
#call CheckEqualsInt(1, !0)
#call CheckEqualsInt(~42, ~UnaryTestValue)
#call CheckEqualsInt(0, +NULL)
#call CheckEqualsInt(0, -NULL)
#call CheckEqualsInt(1, !NULL)
#call CheckEqualsInt(-1, ~NULL)
#undef UnaryTestValue
//
// String concatenation
//
#call CheckEqualsString('hello world', 'hello' + ' ' + 'world')
#call CheckEqualsString('a', '' + 'a')
#call CheckEqualsString('a', 'a' + '')
//
// Null coercion in binary operations
//
#call CheckEqualsInt(1, NULL + 1)
#call CheckEqualsString('abc', NULL + 'abc')
#call CheckEqualsInt(0, NULL + NULL)
#call CheckEqualsInt(1, NULL == 0)
#call CheckEqualsInt(1, NULL == '')
//
// Operator precedence
//
#call CheckEqualsInt(14, 2 + 3 * 4)
#call CheckEqualsInt(5, 10 - 3 - 2)
#call CheckEqualsInt(1, 2 + 3 == 5)
#call CheckEqualsInt(11, 1 | 2 + 8)
#call CheckEqualsInt(1, 1 < 2 & 1 < 2)
#call CheckEqualsInt(0, 0 || 1 && 0)
#call CheckEqualsInt(7, 1 | 2 ^ 4)
#call CheckEqualsInt(3, 1 ^ 2 & 3)
#call CheckEqualsInt(1, 1 == 1 ^ 0)
#call CheckEqualsInt(1, 4 >> 1 < 4)
#call CheckEqualsInt(4, 1 << 1 + 1)
//
// Expression comments
//
#call CheckEqualsInt(3, 1 + /* comment */ 2)
#call CheckEqualsInt(42, 42) ; trailing semicolon comment
