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
#call CheckEqualsInt(-43, ~UnaryTestValue)
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
#call CheckEqualsInt(20, (2 + 3) * 4)
#call CheckEqualsInt(5, 10 - 3 - 2)
#call CheckTrue(1 || 0 && 0)
#call CheckFalse(0 && 1 | 1)
#call CheckEqualsInt(3, 2 | 1 + 2)
#call CheckEqualsInt(3, 1 | 1 ^ 3)
#call CheckEqualsInt(6, 4 ^ 6 & 3)
#call CheckEqualsInt(0, 0 & 1 == 0)
#call CheckFalse(1 == 2 < 2)
#call CheckEqualsInt(1, 4 >> 1 < 4)
#call CheckEqualsInt(4, 1 << 1 + 1)
//
// Expression comments
//
#call CheckEqualsInt(3, 1 + /* comment */ 2)
#call CheckEqualsInt(42, 42) ; trailing semicolon comment
//
// Variable definition
//
#define DefinitionExplicit = 42
#call CheckEqualsInt(42, DefinitionExplicit)
#define DefinitionNull
#call CheckEqualsInt(TYPE_NULL, TypeOf(DefinitionNull))
#define DefinitionString = 'hello'
#call CheckEqualsString('hello', DefinitionString)
#define DefinitionRedefine = 1
#define DefinitionRedefine = 2
#call CheckEqualsInt(2, DefinitionRedefine)
#undef DefinitionExplicit
#undef DefinitionNull
#undef DefinitionString
#undef DefinitionRedefine
//
// Assignment operators
//
#define AssignTarget = 10
#call CheckEqualsInt(5, (AssignTarget = 5))
#call CheckEqualsInt(5, AssignTarget)
#call CheckEqualsInt(8, AssignTarget += 3)
#call CheckEqualsInt(8, AssignTarget)
#call CheckEqualsInt(6, AssignTarget -= 2)
#call CheckEqualsInt(6, AssignTarget)
#call CheckEqualsInt(18, AssignTarget *= 3)
#call CheckEqualsInt(18, AssignTarget)
#call CheckEqualsInt(9, AssignTarget /= 2)
#call CheckEqualsInt(9, AssignTarget)
#call CheckEqualsInt(1, AssignTarget %= 4)
#call CheckEqualsInt(1, AssignTarget)
#define AssignTarget = 0xFF
#call CheckEqualsInt(0x0F, AssignTarget &= 0x0F)
#call CheckEqualsInt(0x0F, AssignTarget)
#call CheckEqualsInt(0xFF, AssignTarget |= 0xF0)
#call CheckEqualsInt(0xFF, AssignTarget)
#call CheckEqualsInt(0xF0, AssignTarget ^= 0x0F)
#call CheckEqualsInt(0xF0, AssignTarget)
#call CheckEqualsInt(0xF00, AssignTarget <<= 4)
#call CheckEqualsInt(0xF00, AssignTarget)
#call CheckEqualsInt(0xF0, AssignTarget >>= 4)
#call CheckEqualsInt(0xF0, AssignTarget)
#define AssignStringTarget = 'hello'
#call CheckEqualsString('hello world', AssignStringTarget += ' world')
#call CheckEqualsString('hello world', AssignStringTarget)
#undef AssignTarget
#undef AssignStringTarget
//
// Prefix increment and decrement
//
#define PrefixTarget = 5
#call CheckEqualsInt(6, ++PrefixTarget)
#call CheckEqualsInt(6, PrefixTarget)
#call CheckEqualsInt(5, --PrefixTarget)
#call CheckEqualsInt(5, PrefixTarget)
#define PrefixNullTarget
#call CheckEqualsInt(1, ++PrefixNullTarget)
#call CheckEqualsInt(1, PrefixNullTarget)
#define PrefixNullTarget2
#call CheckEqualsInt(-1, --PrefixNullTarget2)
#call CheckEqualsInt(-1, PrefixNullTarget2)
#undef PrefixTarget
#undef PrefixNullTarget
#undef PrefixNullTarget2
//
// Postfix increment and decrement
//
#define PostfixTarget = 5
#call CheckEqualsInt(5, PostfixTarget++)
#call CheckEqualsInt(6, PostfixTarget)
#call CheckEqualsInt(6, PostfixTarget--)
#call CheckEqualsInt(5, PostfixTarget)
#call PostfixTarget++
#call PostfixTarget++
#call CheckEqualsInt(7, PostfixTarget)
#undef PostfixTarget
//
// Ternary conditional
//
#call CheckEqualsString('yes', 1 ? 'yes' : 'no')
#call CheckEqualsString('no', 0 ? 'yes' : 'no')
#call CheckEqualsString('c', 0 ? 'a' : 0 ? 'b' : 'c')
#call CheckEqualsString('a', 1 ? 'a' : 0 ? 'b' : 'c')
#call CheckEqualsString('b', 0 ? 'a' : 1 ? 'b' : 'c')
#define TernaryCounter = 0
#call 1 ? (TernaryCounter = TernaryCounter + 1) : (TernaryCounter = TernaryCounter + 10)
#call CheckEqualsInt(1, TernaryCounter)
#call 0 ? (TernaryCounter = TernaryCounter + 10) : (TernaryCounter = TernaryCounter + 1)
#call CheckEqualsInt(2, TernaryCounter)
#undef TernaryCounter
//
// Comma operator
//
#call CheckEqualsInt(3, (1, 2, 3))
#define CommaAccumulator = ''
#call (CommaAccumulator += '1', CommaAccumulator += '2', CommaAccumulator += '3')
#call CheckEqualsString('123', CommaAccumulator)
#undef CommaAccumulator
//
// Short-circuit evaluation
//
#define ShortCircuitCounter = 0
#call CheckFalse(0 && (ShortCircuitCounter = ShortCircuitCounter + 1))
#call CheckEqualsInt(0, ShortCircuitCounter)
#call CheckTrue(1 || (ShortCircuitCounter = ShortCircuitCounter + 1))
#call CheckEqualsInt(0, ShortCircuitCounter)
#call CheckTrue(1 && (ShortCircuitCounter = ShortCircuitCounter + 1))
#call CheckEqualsInt(1, ShortCircuitCounter)
#call CheckFalse(0 || (ShortCircuitCounter = ShortCircuitCounter + 1, 0))
#call CheckEqualsInt(2, ShortCircuitCounter)
#undef ShortCircuitCounter
//
// #undef
//
#define UndefTarget = 42
#call CheckTrue(Defined(UndefTarget))
#undef UndefTarget
#call CheckFalse(Defined(UndefTarget))
//
// #define shorthand
//
#: AliasDefineTest = 42
#call CheckEqualsInt(42, AliasDefineTest)
#undef AliasDefineTest
//
// Line spanning
//
#define SpanResult = 10 + \
  20 + \
  12
#call CheckEqualsInt(42, SpanResult)
#undef SpanResult
#pragma spansymbol "_"
#define SpanCustomResult = 100 + _
  200 + _
  42
#call CheckEqualsInt(342, SpanCustomResult)
#undef SpanCustomResult
#pragma spansymbol "\"
#define SpanRestoredResult = 1 + \
  2
#call CheckEqualsInt(3, SpanRestoredResult)
#undef SpanRestoredResult
