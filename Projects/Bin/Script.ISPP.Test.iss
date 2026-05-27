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
#expr CheckEqualsInt(42, 42)
#expr CheckEqualsInt(-1, -1)
#expr CheckEqualsInt(255, 0xFF)
#expr CheckEqualsInt(255, 0XFF)
#expr CheckEqualsInt(0, 0x0)
#expr CheckEqualsInt(0x7FFFFFFFFFFFFFFF, MaxInt)
#expr CheckEqualsInt(0x8000000000000000, MinInt)
#expr CheckEqualsInt(42, 42L)
#expr CheckEqualsInt(42, 42U)
#expr CheckEqualsInt(42, 42LU)
//
// String literals (Pascal mode)
//
#expr CheckEqualsString('hello', 'hello')
#expr CheckEqualsString('it''s', 'it''s')
#expr CheckEqualsString("hello", "hello")
#expr CheckEqualsString("hello", 'hello')
#expr CheckEqualsInt(0, Len(''))
#expr CheckEqualsInt(2, Len("\n"))
#expr CheckFalse(SameStr("\n", NewLine))
//
// String literals (C mode)
//
#pragma parseroption -p-
#expr CheckEqualsString("hello world", "hello " "world")
#expr CheckEqualsString("hello world", 'hello ' 'world')
#expr CheckEqualsInt(0, Len(""))
#expr CheckFalse(SameStr("\0", "0"))
#expr CheckFalse(SameStr("\a", "a"))
#expr CheckFalse(SameStr("\b", "b"))
#expr CheckFalse(SameStr("\f", "f"))
#expr CheckFalse(SameStr("\n", "n"))
#expr CheckTrue(SameStr("\n", NewLine))
#expr CheckFalse(SameStr("\r", "r"))
#expr CheckFalse(SameStr("\t", "t"))
#expr CheckTrue(SameStr("\t", Tab))
#expr CheckFalse(SameStr("\v", "v"))
#expr CheckTrue(SameStr("\z", "z")) ; Unknown escape passes character through
#expr CheckEqualsInt(1, Len("\\"))
#expr CheckEqualsString("A", "\x41")
#expr CheckEqualsString("A", "\101")
#pragma parseroption -p+
//
// NULL
//
#define NullVariable
#expr CheckEqualsInt(TYPE_NULL, TypeOf(NullVariable))
#expr CheckTrue(!NULL)
#expr CheckEqualsInt(0, NULL + 0)
#expr CheckEqualsString('', NULL + '')
#expr CheckEqualsInt(0, NullVariable + 0)
#expr CheckEqualsString('', NullVariable + '')
#expr CheckEqualsInt(TYPE_NULL, TypeOf(void))
#undef NullVariable
//
// Type checking
//
#define TypeCheckInt = 42
#define TypeCheckString = 'hello'
#define TypeCheckNull
#define TypeCheckMacro() 0
#dim TypeCheckArray[1]
#expr CheckEqualsInt(TYPE_INTEGER, TypeOf(TypeCheckInt))
#expr CheckEqualsInt(TYPE_STRING, TypeOf(TypeCheckString))
#expr CheckEqualsInt(TYPE_NULL, TypeOf(TypeCheckNull))
#expr CheckEqualsInt(TYPE_MACRO, TypeOf(TypeCheckMacro))
#expr CheckEqualsInt(TYPE_FUNC, TypeOf(Int))
#expr CheckEqualsInt(TYPE_ERROR, TypeOf(UndefinedIdentifier_XYZ))
#expr CheckEqualsInt(TYPE_ARRAY, TypeOf(TypeCheckArray))
#expr CheckEqualsInt(TYPE_INTEGER, TypeOf2(1 + 1))
#expr CheckEqualsInt(TYPE_STRING, TypeOf2('a' + 'b'))
#expr CheckEqualsInt(TYPE_NULL, TypeOf2(NULL))
#expr CheckTrue(Defined(TypeCheckInt))
#expr CheckFalse(Defined(UndefinedIdentifier_XYZ))
#expr CheckTrue(Defined(ISPP_INVOKED))
#expr CheckTrue(Defined(WINDOWS))
#expr CheckTrue(Defined(UNICODE))
#expr CheckTrue(Defined(__WIN32__))
#expr CheckTrue(Defined(__FILENAME__))
#expr CheckTrue(Defined(PREPROCVER))
#expr CheckEqualsInt(TypeOf(TypeCheckInt), TypeOf TypeCheckInt)
#expr CheckEqualsInt(Defined(TypeCheckInt), Defined TypeCheckInt)
#undef TypeCheckInt
#undef TypeCheckString
#undef TypeCheckNull
#undef TypeCheckMacro
#undef TypeCheckArray
//
// Type conversion
//
#expr CheckEqualsInt(42, Int('42'))
#expr CheckEqualsInt(0, Int(''))
#expr CheckEqualsInt(0, Int(NULL))
#expr CheckEqualsInt(-1, Int('not_a_number', -1))
#expr CheckEqualsString('42', Str(42))
#expr CheckEqualsString('', Str(NULL))
#expr CheckEqualsString('hello', Str('hello'))
#expr CheckEqualsInt(42, Int(42))
//
// Arithmetic
//
#expr CheckEqualsInt(5, 2 + 3)
#expr CheckEqualsInt(2, 5 - 3)
#expr CheckEqualsInt(6, 2 * 3)
#expr CheckEqualsInt(3, 7 / 2)
#expr CheckEqualsInt(1, 7 % 3)
#expr CheckEqualsInt(-3, -7 / 2)
#expr CheckEqualsInt(-1, -7 % 3)
//
// Bitwise
//
#define BitwiseTestValue = 0xAB
#expr CheckEqualsInt(0x0F, 0xFF & 0x0F)
#expr CheckEqualsInt(0xFF, 0xF0 | 0x0F)
#expr CheckEqualsInt(0xF0, 0xFF ^ 0x0F)
#expr CheckEqualsInt(-1, ~0)
#expr CheckEqualsInt(8, 1 << 3)
#expr CheckEqualsInt(2, 16 >> 3)
#expr CheckEqualsInt(BitwiseTestValue, BitwiseTestValue & BitwiseTestValue)
#expr CheckEqualsInt(BitwiseTestValue, BitwiseTestValue | BitwiseTestValue)
#expr CheckEqualsInt(0, BitwiseTestValue ^ BitwiseTestValue)
#expr CheckEqualsInt(BitwiseTestValue, ~~BitwiseTestValue)
#undef BitwiseTestValue
//
// Integer comparison
//
#expr CheckEqualsInt(1, 5 == 5)
#expr CheckEqualsInt(0, 5 == 6)
#expr CheckEqualsInt(1, 5 != 6)
#expr CheckEqualsInt(0, 5 != 5)
#expr CheckEqualsInt(1, 3 < 5)
#expr CheckEqualsInt(0, 5 < 3)
#expr CheckEqualsInt(1, 3 <= 5)
#expr CheckEqualsInt(1, 5 <= 5)
#expr CheckEqualsInt(0, 5 <= 3)
#expr CheckEqualsInt(1, 5 > 3)
#expr CheckEqualsInt(0, 3 > 5)
#expr CheckEqualsInt(1, 5 >= 3)
#expr CheckEqualsInt(1, 5 >= 5)
#expr CheckEqualsInt(0, 3 >= 5)
//
// String comparison
//
#expr CheckEqualsInt(1, 'abc' == 'ABC')
#expr CheckEqualsInt(0, 'abc' == 'def')
#expr CheckEqualsInt(1, 'abc' != 'def')
#expr CheckEqualsInt(0, 'abc' != 'ABC')
#expr CheckEqualsInt(1, 'a' < 'b')
#expr CheckEqualsInt(0, 'b' < 'a')
#expr CheckEqualsInt(1, 'a' <= 'b')
#expr CheckEqualsInt(1, 'a' <= 'A')
#expr CheckEqualsInt(1, 'b' > 'a')
#expr CheckEqualsInt(0, 'a' > 'b')
#expr CheckEqualsInt(1, 'b' >= 'a')
#expr CheckEqualsInt(1, 'A' >= 'a')
//
// Logical operators
//
#expr CheckFalse(0 && 0)
#expr CheckFalse(0 && 1)
#expr CheckFalse(1 && 0)
#expr CheckTrue(1 && 1)
#expr CheckFalse(0 || 0)
#expr CheckTrue(0 || 1)
#expr CheckTrue(1 || 0)
#expr CheckTrue(1 || 1)
#expr CheckTrue(!!1)
#expr CheckTrue(42 && 7)
#expr CheckTrue(42 || 0)
//
// Unary operators
//
#define UnaryTestValue = 42
#expr CheckEqualsInt(42, +UnaryTestValue)
#expr CheckEqualsInt(-42, -UnaryTestValue)
#expr CheckEqualsInt(0, !UnaryTestValue)
#expr CheckEqualsInt(1, !0)
#expr CheckEqualsInt(~42, ~UnaryTestValue)
#expr CheckEqualsInt(0, +NULL)
#expr CheckEqualsInt(0, -NULL)
#expr CheckEqualsInt(1, !NULL)
#expr CheckEqualsInt(-1, ~NULL)
#undef UnaryTestValue
//
// String concatenation
//
#expr CheckEqualsString('hello world', 'hello' + ' ' + 'world')
#expr CheckEqualsString('a', '' + 'a')
#expr CheckEqualsString('a', 'a' + '')
//
// Null coercion in binary operations
//
#expr CheckEqualsInt(1, NULL + 1)
#expr CheckEqualsString('abc', NULL + 'abc')
#expr CheckEqualsInt(0, NULL + NULL)
#expr CheckEqualsInt(1, NULL == 0)
#expr CheckEqualsInt(1, NULL == '')
//
// Operator precedence
//
#expr CheckEqualsInt(14, 2 + 3 * 4)
#expr CheckEqualsInt(5, 10 - 3 - 2)
#expr CheckEqualsInt(1, 2 + 3 == 5)
#expr CheckEqualsInt(11, 1 | 2 + 8)
#expr CheckEqualsInt(1, 1 < 2 & 1 < 2)
#expr CheckEqualsInt(0, 0 || 1 && 0)
#expr CheckEqualsInt(7, 1 | 2 ^ 4)
#expr CheckEqualsInt(3, 1 ^ 2 & 3)
#expr CheckEqualsInt(1, 1 == 1 ^ 0)
#expr CheckEqualsInt(1, 4 >> 1 < 4)
#expr CheckEqualsInt(4, 1 << 1 + 1)
//
// Expression comments
//
#expr CheckEqualsInt(3, 1 + /* comment */ 2)
#expr CheckEqualsInt(42, 42) ; trailing semicolon comment
