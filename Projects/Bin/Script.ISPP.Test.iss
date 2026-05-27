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
//
// #if / #endif
//
#define IfResult = 0
#if 1
#define IfResult = 1
#endif
#call CheckEqualsInt(1, IfResult)
#define IfResult = 0
#if 0
#define IfResult = 1
#endif
#call CheckEqualsInt(0, IfResult)
#define IfResult = 0
#if 1 + 1 == 2
#define IfResult = 1
#endif
#call CheckEqualsInt(1, IfResult)
#undef IfResult
//
// #if / #else / #endif
//
#define IfElseResult = 0
#if 1
#define IfElseResult = 10
#else
#define IfElseResult = 20
#endif
#call CheckEqualsInt(10, IfElseResult)
#define IfElseResult = 0
#if 0
#define IfElseResult = 10
#else
#define IfElseResult = 20
#endif
#call CheckEqualsInt(20, IfElseResult)
#undef IfElseResult
//
// #if / #elif / #else / #endif
//
#define ElifResult = 0
#if 1
#define ElifResult = 10
#elif 1
#define ElifResult = 20
#else
#define ElifResult = 30
#endif
#call CheckEqualsInt(10, ElifResult)
#define ElifResult = 0
#if 0
#define ElifResult = 10
#elif 1
#define ElifResult = 20
#else
#define ElifResult = 30
#endif
#call CheckEqualsInt(20, ElifResult)
#define ElifResult = 0
#if 0
#define ElifResult = 10
#elif 0
#define ElifResult = 20
#else
#define ElifResult = 30
#endif
#call CheckEqualsInt(30, ElifResult)
#define ElifResult = 0
#if 0
#define ElifResult = 10
#elif 0
#define ElifResult = 20
#elif 1
#define ElifResult = 30
#elif 1
#define ElifResult = 40
#else
#define ElifResult = 50
#endif
#call CheckEqualsInt(30, ElifResult)
#define ElifSkipCounter = 0
#define ElifResult = 0
#if 1
#define ElifResult = 10
#elif (ElifSkipCounter = ElifSkipCounter + 1)
#define ElifResult = 20
#endif
#call CheckEqualsInt(10, ElifResult)
#call CheckEqualsInt(0, ElifSkipCounter)
#undef ElifSkipCounter
#undef ElifResult
//
// #ifdef / #ifndef
//
#define IfdefTarget = 42
#define IfdefResult = 0
#ifdef IfdefTarget
#define IfdefResult = 1
#endif
#call CheckEqualsInt(1, IfdefResult)
#define IfdefResult = 0
#ifdef UndefinedIdentifier_XYZ
#define IfdefResult = 1
#endif
#call CheckEqualsInt(0, IfdefResult)
#define IfdefResult = 0
#ifndef IfdefTarget
#define IfdefResult = 1
#endif
#call CheckEqualsInt(0, IfdefResult)
#define IfdefResult = 0
#ifndef UndefinedIdentifier_XYZ
#define IfdefResult = 1
#endif
#call CheckEqualsInt(1, IfdefResult)
#define IfdefMacro() 0
#define IfdefResult = 0
#ifdef IfdefMacro
#define IfdefResult = 1
#endif
#call CheckEqualsInt(1, IfdefResult)
#define IfdefVoid
#define IfdefResult = 0
#ifdef IfdefVoid
#define IfdefResult = 1
#endif
#call CheckEqualsInt(1, IfdefResult)
#undef IfdefVoid
#undef IfdefMacro
#undef IfdefTarget
#undef IfdefResult
//
// #ifexist / #ifnexist
//
#define IfexistResult = 0
#ifexist __PATHFILENAME__
#define IfexistResult = 1
#endif
#call CheckEqualsInt(1, IfexistResult)
#define IfexistResult = 0
#ifexist "nonexistent_file_xyz_12345.tmp"
#define IfexistResult = 1
#endif
#call CheckEqualsInt(0, IfexistResult)
#define IfexistResult = 0
#ifnexist "nonexistent_file_xyz_12345.tmp"
#define IfexistResult = 1
#endif
#call CheckEqualsInt(1, IfexistResult)
#define IfexistResult = 0
#ifnexist __PATHFILENAME__
#define IfexistResult = 1
#endif
#call CheckEqualsInt(0, IfexistResult)
#undef IfexistResult
//
// Nested conditionals
//
#define NestResult = 0
#if 1
#if 0
#if 1
#define NestResult = 10
#endif
#endif
#endif
#call CheckEqualsInt(0, NestResult)
#define NestResult = 0
#if 1
#if 1
#if 1
#define NestResult = 10
#endif
#endif
#endif
#call CheckEqualsInt(10, NestResult)
#define NestResult = 0
#if 0
#if 1
#define NestResult = 10
#endif
#define NestResult = 20
#endif
#call CheckEqualsInt(0, NestResult)
#define NestDefined = 1
#define NestResult = 0
#if 1
#ifdef NestDefined
#define NestResult = 10
#endif
#endif
#call CheckEqualsInt(10, NestResult)
// bug: #elif expression is evaluated even when suppressed by an outer #if 0,
// because the short-circuit guard checks only the innermost conditional block
// state, not the outer conditional blocks
//#define NestElifCounter = 0
//#if 0
//#if 0
//#elif (NestElifCounter = NestElifCounter + 1)
//#endif
//#endif
//#call CheckEqualsInt(0, NestElifCounter)
//#undef NestElifCounter
#undef NestDefined
#undef NestResult
//
// Inline conditionals
//
{#if 1}; INLINE_COND_TRUE yes{#else}; INLINE_COND_TRUE no{#endif}
{#if 0}; INLINE_COND_FALSE yes{#else}; INLINE_COND_FALSE no{#endif}
{#if 1}{#if 1}; INLINE_COND_NESTED inner{#endif}{#endif}
{#if 0}; INLINE_COND_ELIF first{#elif 1}; INLINE_COND_ELIF second{#else}; INLINE_COND_ELIF third{#endif}
{#emit '; INLINE_POS before '}{#if 1}middle{#else}other{#endif}{#emit ' after'}
#call CheckTrue(Find(0, 'INLINE_COND_TRUE yes', FIND_CONTAINS) >= 0)
#call CheckTrue(Find(0, 'INLINE_COND_TRUE no', FIND_CONTAINS) < 0)
#call CheckTrue(Find(0, 'INLINE_COND_FALSE no', FIND_CONTAINS) >= 0)
#call CheckTrue(Find(0, 'INLINE_COND_FALSE yes', FIND_CONTAINS) < 0)
#call CheckTrue(Find(0, 'INLINE_COND_NESTED inner', FIND_CONTAINS) >= 0)
#call CheckTrue(Find(0, 'INLINE_COND_ELIF second', FIND_CONTAINS) >= 0)
#call CheckTrue(Find(0, 'INLINE_COND_ELIF first', FIND_CONTAINS) < 0)
#call CheckTrue(Find(0, 'INLINE_COND_ELIF third', FIND_CONTAINS) < 0)
#call CheckTrue(Find(0, 'INLINE_POS before middle after', FIND_CONTAINS) >= 0)
#call CheckTrue(Find(0, 'INLINE_POS before other after', FIND_CONTAINS) < 0)
// bug: same as the nested #elif bug in the simple-directive path above,
// but via ProcessInlineDirectives
//#define InlineNestElifCounter = 0
//{#if 0}{#if 0}x{#elif (InlineNestElifCounter = InlineNestElifCounter + 1)}y{#endif}{#endif}
//#call CheckEqualsInt(0, InlineNestElifCounter)
//#undef InlineNestElifCounter
{#? 1}; INLINE_SHORTHAND_IF yes{#^}; INLINE_SHORTHAND_IF no{#.}
#call CheckTrue(Find(0, 'INLINE_SHORTHAND_IF yes', FIND_CONTAINS) >= 0)
#call CheckTrue(Find(0, 'INLINE_SHORTHAND_IF no', FIND_CONTAINS) < 0)
//
// Directive shorthands for conditionals
//
#define ShorthandIfResult = 0
#? 1 == 1 ; #if
#define ShorthandIfResult = 1
#. ; #endif
#call CheckEqualsInt(1, ShorthandIfResult)
#define ShorthandIfResult = 0
#? 0 ; #if
#define ShorthandIfResult = 10
#^ ; #else
#define ShorthandIfResult = 20
#. ; #endif
#call CheckEqualsInt(20, ShorthandIfResult)
#undef ShorthandIfResult
//
// Basic macros
//
#define ParameterlessMacro() 42
#call CheckEqualsInt(42, ParameterlessMacro())
#define SingleParamMacro(X) X * 2
#call CheckEqualsInt(10, SingleParamMacro(5))
#define MultiParamMacro(X, Y) X + Y
#call CheckEqualsInt(7, MultiParamMacro(3, 4))
#define StringParamMacro(S) S + '!'
#call CheckEqualsString('hello!', StringParamMacro('hello'))
#undef ParameterlessMacro
#undef SingleParamMacro
#undef MultiParamMacro
#undef StringParamMacro
//
// Default parameter values
//
#define DefaultParamMacro(X, Y = 10) X + Y
#call CheckEqualsInt(15, DefaultParamMacro(5))
#call CheckEqualsInt(25, DefaultParamMacro(5, 20))
#undef DefaultParamMacro
//
// Typed parameters
//
#define IntTypedMacro(int X) X + 1
#call CheckEqualsInt(43, IntTypedMacro(42))
#define StrTypedMacro(str S) Len(S)
#call CheckEqualsInt(5, StrTypedMacro('hello'))
#define AnyTypedMacro(any X) TypeOf(X)
#call CheckEqualsInt(TYPE_INTEGER, AnyTypedMacro(42))
#call CheckEqualsInt(TYPE_STRING, AnyTypedMacro('hello'))
#undef IntTypedMacro
#undef StrTypedMacro
#undef AnyTypedMacro
//
// By-reference parameters
//
#define ByRefMacro(int *X) X = X + 10
#define ByRefTarget = 5
#call ByRefMacro(ByRefTarget)
#call CheckEqualsInt(15, ByRefTarget)
#undef ByRefMacro
#undef ByRefTarget
//
// Local array
//
#define LocalArrayMacro(X, Y) \
  Local[0] = X, \
  Local[1] = Y, \
  Local[0] + Local[1]
#call CheckEqualsInt(30, LocalArrayMacro(10, 20))
#define LocalArrayHighIndex() \
  Local[15] = 99, \
  Local[15]
#call CheckEqualsInt(99, LocalArrayHighIndex())
#define LocalArrayNotPreserved() Local[0] + 0
#call CheckEqualsInt(0, LocalArrayNotPreserved())
#undef LocalArrayMacro
#undef LocalArrayHighIndex
#undef LocalArrayNotPreserved
//
// Recursion
//
#define Factorial(N) N <= 1 ? 1 : N * Factorial(N - 1)
#call CheckEqualsInt(120, Factorial(5))
#call CheckEqualsInt(1, Factorial(0))
#undef Factorial
//
// @ operator with func parameter
//
#define ApplyFuncInt(func F, int X) F(X)
#call CheckEqualsString('42', ApplyFuncInt(@Str, 42))
#define Exclaim(str S) S + '!'
#define ApplyFuncStr(func F, str X) F(X)
#call CheckEqualsString('hi!', ApplyFuncStr(@Exclaim, 'hi'))
#define GetTypeOfParam(func F) TypeOf(F)
#call CheckEqualsInt(TYPE_FUNC, GetTypeOfParam(@Str))
#undef ApplyFuncInt
#undef ApplyFuncStr
#undef Exclaim
#undef GetTypeOfParam
//
// @ operator with array parameter
//
#dim FuncTestArray[3] {10, 20, 30}
#define SumThree(array A) A[0] + A[1] + A[2]
#call CheckEqualsInt(60, SumThree(@FuncTestArray))
#define GetDimension(array A) DimOf(A)
#call CheckEqualsInt(3, GetDimension(@FuncTestArray))
#dim FuncTestArray2[7]
#call CheckEqualsInt(7, GetDimension(@FuncTestArray2))
#define GetDimOfScalar(int V) DimOf(V)
#call CheckEqualsInt(0, GetDimOfScalar(42))
#define GetTypeOfArray(array A) TypeOf(A)
#call CheckEqualsInt(TYPE_ARRAY, GetTypeOfArray(@FuncTestArray))
#undef FuncTestArray
#undef FuncTestArray2
#undef SumThree
#undef GetDimension
#undef GetDimOfScalar
#undef GetTypeOfArray
//
// @ operator with built-in function (CopyExpVar evCallContext)
//
#define TestCopyExpFunc(str S) S + '!'
#call Int(@TestCopyExpFunc)
#call CheckEqualsString('test!', TestCopyExpFunc('test'))
#undef TestCopyExpFunc
//
// #sub / #endsub
//
// bug: #sub body starts with #define private which leaks to the caller's
// default scope after the call; re-enable these tests after fix
//#sub SimpleSub
//  #emit '; SUB_SIMPLE_MARKER'
//#endsub
//#call SimpleSub()
//#call CheckTrue(Find(0, 'SUB_SIMPLE_MARKER', FIND_CONTAINS) >= 0)
//#define SubArgValue = 'test42'
//#sub SubWithVariable
//  #emit '; SUB_VARIABLE_MARKER ' + SubArgValue
//#endsub
//#call SubWithVariable()
//#call CheckTrue(Find(0, 'SUB_VARIABLE_MARKER test42', FIND_CONTAINS) >= 0)
//#sub SubScopeTest
//  #define ScopeLeakVar = 99
//#endsub
//#call SubScopeTest()
//#call CheckFalse(Defined(ScopeLeakVar))
//#undef SimpleSub
//#undef SubArgValue
//#undef SubWithVariable
//#undef SubScopeTest
//
// #for loop
//
#define ForCounter
#sub ForEmitSub
  #emit '; FOR_EMIT_MARKER_' + Str(ForCounter)
#endsub
#for {ForCounter = 0; ForCounter < 5; ForCounter++} ForEmitSub
#call CheckEqualsInt(5, ForCounter)
#call CheckTrue(Find(0, 'FOR_EMIT_MARKER_0', FIND_CONTAINS) >= 0)
#call CheckTrue(Find(0, 'FOR_EMIT_MARKER_4', FIND_CONTAINS) >= 0)
#call CheckTrue(Find(0, 'FOR_EMIT_MARKER_5', FIND_CONTAINS) < 0)
#define Accumulator = 0
#for {ForCounter = 1; ForCounter <= 5; ForCounter++} Accumulator += ForCounter
#call CheckEqualsInt(15, Accumulator)
#define ZeroIterResult = 0
#for {ForCounter = 0; 0; ForCounter++} ZeroIterResult = 1
#call CheckEqualsInt(0, ZeroIterResult)
#undef ForCounter
#undef ForEmitSub
#undef Accumulator
#undef ZeroIterResult
//
// #emit
//
#emit '; EMIT_MARKER_' + Str(42)
#call CheckTrue(Find(0, 'EMIT_MARKER_42', FIND_CONTAINS) >= 0)
{#emit '; INLINE_DEFAULT '}{#42}
#call CheckTrue(Find(0, 'INLINE_DEFAULT 42', FIND_CONTAINS) >= 0)
#expr '; EXPR_NOEMIT_MARKER' ; #expr does not #emit
#call CheckTrue(Find(0, 'EXPR_NOEMIT_MARKER', FIND_CONTAINS) < 0)
//
// Named parameters in function calls
//
#define NamedParamFunc(int A, int B = 10) A + B
#call CheckEqualsInt(8, NamedParamFunc(A = 5, B = 3))
#call CheckEqualsInt(8, NamedParamFunc(B = 3, A = 5))
#call CheckEqualsInt(15, NamedParamFunc(A = 5))
#undef NamedParamFunc
//
// Directive shorthands and the echo alias
//
#= '; EMIT_ALIAS_MARKER' ; #emit
#call CheckTrue(Find(0, 'EMIT_ALIAS_MARKER', FIND_CONTAINS) >= 0)
#! 'EXPR_ALIAS_NOOUTPUT' ; #expr
#call CheckTrue(Find(0, 'EXPR_ALIAS_NOOUTPUT', FIND_CONTAINS) < 0)
#echo '; ECHO_ALIAS_MARKER' ; #emit
#call CheckTrue(Find(0, 'ECHO_ALIAS_MARKER', FIND_CONTAINS) >= 0)
//
// #dim / #redim
//
#dim DimBasicArray[3]
#define DimBasicArray[0] = 10
#define DimBasicArray[1] = 20
#define DimBasicArray[2] = 30
#call CheckEqualsInt(10, DimBasicArray[0])
#call CheckEqualsInt(20, DimBasicArray[1])
#call CheckEqualsInt(30, DimBasicArray[2])
#dim DimInitArray[3] {100, 200, 300}
#call CheckEqualsInt(100, DimInitArray[0])
#call CheckEqualsInt(200, DimInitArray[1])
#call CheckEqualsInt(300, DimInitArray[2])
#call CheckEqualsInt(3, DimOf(DimBasicArray))
#call CheckEqualsInt(3, DimOf(DimInitArray))
#call CheckEqualsInt(TYPE_ARRAY, TypeOf(DimBasicArray))
#call CheckEqualsInt(TYPE_ARRAY, TypeOf(DimInitArray))
#call CheckEqualsInt(DimOf(DimBasicArray), DimOf DimBasicArray)
#call CheckEqualsInt(TypeOf(DimBasicArray), TypeOf DimBasicArray)
#redim DimBasicArray[5]
#call CheckEqualsInt(5, DimOf(DimBasicArray))
#call CheckEqualsInt(10, DimBasicArray[0])
#call CheckEqualsInt(20, DimBasicArray[1])
#call CheckEqualsInt(30, DimBasicArray[2])
#call CheckEqualsInt(TYPE_NULL, TypeOf2(DimBasicArray[3]))
#call CheckEqualsInt(TYPE_NULL, TypeOf2(DimBasicArray[4]))
#redim DimBasicArray[2]
#call CheckEqualsInt(2, DimOf(DimBasicArray))
#call CheckEqualsInt(10, DimBasicArray[0])
#call CheckEqualsInt(20, DimBasicArray[1])
#undef DimBasicArray
#undef DimInitArray
//
// Scope
//
#define public ScopePublicVar = 'public_value'
#define protected ScopeProtectedVar = 'protected_value'
#define private ScopePrivateVar = 'private_value'
#call CheckEqualsString('public_value', ScopePublicVar)
#call CheckEqualsString('protected_value', ScopeProtectedVar)
#call CheckEqualsString('private_value', ScopePrivateVar)
#undef ScopePublicVar
#undef ScopeProtectedVar
#undef ScopePrivateVar
#define protected
#define DefaultScopeTest = 42
#call CheckEqualsInt(42, DefaultScopeTest)
#undef public DefaultScopeTest
#call CheckTrue(Defined(DefaultScopeTest))
#undef protected DefaultScopeTest
#call CheckFalse(Defined(DefaultScopeTest))
#define public
#define public SameNameVar = 12
#define protected SameNameVar = 13
#define private SameNameVar = 14
#call CheckEqualsInt(14, SameNameVar)
#undef private SameNameVar
#call CheckEqualsInt(13, SameNameVar)
#undef protected SameNameVar
#call CheckEqualsInt(12, SameNameVar)
#undef SameNameVar
#call CheckFalse(Defined(SameNameVar))
//
// Predefined variables
//
#call CheckTrue(Defined(ISPP_INVOKED))
#call CheckTrue(Defined(WINDOWS))
#call CheckTrue(Defined(UNICODE))
#call CheckTrue(Defined(__WIN32__))
#call CheckTrue(Len(__FILENAME__) > 0)
#call CheckTrue(__LINE__ > 0)
#call CheckTrue(Len(__DIR__) > 0)
#define CounterFirst = __COUNTER__
#define CounterSecond = __COUNTER__
#call CheckEqualsInt(CounterFirst + 1, CounterSecond)
#undef CounterFirst
#undef CounterSecond
#call CheckTrue(PREPROCVER >= 0x01000000)
#call CheckEqualsInt(PREPROCVER, Ver)
#call CheckTrue(Len(CompilerPath) > 0)
#call CheckTrue(Len(SourcePath) > 0)
#call CheckTrue(Len(SysPath) > 0)
#call CheckEqualsInt(TYPE_STRING, TypeOf2(__INCLUDE__))
#ifdef ISCC_INVOKED
#call CheckEqualsInt(TYPE_NULL, TypeOf(ISCC_INVOKED))
#endif
#call CheckTrue(Defined(__OPT_C__))
#call CheckTrue(Defined(__POPT_B__))
//
// #include scoping
//
#define protected MainScopeProtectedVar = 'main_protected'
#define private MainScopePrivateVar = 'main_private'
#include "Script.ISPP.Include.Test.iss"
#call CheckFalse(Defined(IncludeProtectedVar))
#call CheckFalse(Defined(IncludePrivateVar))
#call CheckTrue(IncludeSeesMainProtected)
#call CheckFalse(IncludeSeesMainPrivate)
#call CheckTrue(Len(IncludePathFilename) > 0)
#undef MainScopeProtectedVar
#undef MainScopePrivateVar
#undef IncludeSeesMainProtected
#undef IncludeSeesMainPrivate
#undef IncludePathFilename
//
// Directive shorthand for #include
//
#+ "Script.ISPP.Include.Test.iss"
#call CheckTrue(Defined(IncludePathFilename))
#undef IncludeSeesMainProtected
#undef IncludeSeesMainPrivate
#undef IncludePathFilename
//
// String functions
//
#call CheckEqualsString('ell', Copy('hello', 2, 3))
#call CheckEqualsString('ello', Copy('hello', 2))
#call CheckEqualsInt(3, Pos('ll', 'hello'))
#call CheckEqualsInt(0, Pos('LL', 'hello'))
#call CheckEqualsInt(0, Pos('x', 'hello'))
#call CheckEqualsInt(4, RPos('l', 'hello'))
#call CheckEqualsInt(5, Len('hello'))
#call CheckEqualsInt(0, Len(''))
#call CheckEqualsString('hello', LowerCase('HELLO'))
#call CheckEqualsString('HELLO', UpperCase('hello'))
#call CheckEqualsString('hello', Trim('  hello  '))
#call CheckEqualsString('aYbYc', StringChange('aXbXc', 'X', 'Y'))
#call CheckEqualsString('aYbxc', StringChange('aXbxc', 'X', 'Y'))
#call CheckEqualsString('hello', AddQuotes('hello'))
#call CheckEqualsString('"hello world"', AddQuotes('hello world'))
#call CheckTrue(SameStr('abc', 'abc'))
#call CheckFalse(SameStr('abc', 'ABC'))
#call CheckTrue(SameText('abc', 'ABC'))
#call CheckFalse(SameText('abc', 'def'))
//
// Format function
//
#call CheckEqualsString('hello', Format('hello'))
#call CheckEqualsString('world', Format('%s', 'world'))
#call CheckEqualsString('42', Format('%d', 42))
#call CheckEqualsString('age is 25', Format('%s is %d', 'age', 25))
#call CheckEqualsString('FF', Format('%x', 255))
#call CheckEqualsString('100%', Format('100%%'))
#call CheckEqualsString('abc', Format('%s%s%s', 'a', 'b', 'c'))
#define FormatTestName = 'Alice'
#define FormatTestAge = 30
#call CheckEqualsString('Alice is 30', Format('%s is %d', FormatTestName, FormatTestAge))
#undef FormatTestName
#undef FormatTestAge
//
// Path macros
//
#call CheckEqualsString('C:\Dir\', ExtractFilePath('C:\Dir\File.txt'))
#call CheckEqualsString('', ExtractFilePath('File.txt'))
#call CheckEqualsString('C:\Dir', ExtractFileDir('C:\Dir\File.txt'))
#call CheckEqualsString('txt', ExtractFileExt('File.txt'))
#call CheckEqualsString('File.txt', ExtractFileName('C:\Dir\File.txt'))
#call CheckEqualsString('File.txt', ExtractFileName('File.txt'))
#call CheckEqualsString('File.log', ChangeFileExt('File.txt', 'log'))
#call CheckEqualsString('File', RemoveFileExt('File.txt'))
#call CheckEqualsString('C:\Dir\', AddBackslash('C:\Dir'))
#call CheckEqualsString('C:\Dir\', AddBackslash('C:\Dir\'))
#call CheckEqualsString('C:\Dir', RemoveBackslashUnlessRoot('C:\Dir\'))
#call CheckEqualsString('C:\', RemoveBackslashUnlessRoot('C:\'))
//
// Delete / Insert macros
//
#define DeleteTarget = 'hello'
#call Delete(DeleteTarget, 2, 2)
#call CheckEqualsString('hlo', DeleteTarget)
#define InsertTarget = 'hello'
#call Insert(InsertTarget, 2, 'XX')
#call CheckEqualsString('hXXello', InsertTarget)
#undef DeleteTarget
#undef InsertTarget
//
// Version macros
//
#define VersionPacked = PackVersionComponents(1, 2, 3, 4)
#define VersionMajor
#define VersionMinor
#define VersionRevision
#define VersionBuild
#call UnpackVersionComponents(VersionPacked, VersionMajor, VersionMinor, VersionRevision, VersionBuild)
#call CheckEqualsInt(1, VersionMajor)
#call CheckEqualsInt(2, VersionMinor)
#call CheckEqualsInt(3, VersionRevision)
#call CheckEqualsInt(4, VersionBuild)
#define VersionMS = 0x00010002
#define VersionLS = 0x00030004
#define VersionFromNumbers = PackVersionNumbers(VersionMS, VersionLS)
#call CheckEqualsInt(VersionPacked, VersionFromNumbers)
#define UnpackedMS
#define UnpackedLS
#call UnpackVersionNumbers(VersionFromNumbers, UnpackedMS, UnpackedLS)
#call CheckEqualsInt(VersionMS, UnpackedMS)
#call CheckEqualsInt(VersionLS, UnpackedLS)
#call CheckEqualsString('1.2.3.4', VersionToStr(PackVersionComponents(1, 2, 3, 4)))
#call CheckEqualsInt(PackVersionComponents(1, 2, 3, 4), StrToVersion('1.2.3.4'))
#call CheckEqualsString('6.4.0', DecodeVer(EncodeVer(6, 4)))
#call CheckEqualsString('1.2.3.4', DecodeVer(EncodeVer(1, 2, 3, 4), 4))
#call CheckEqualsString('6.4', DecodeVer(EncodeVer(6, 4), 2))
#call CheckTrue(ComparePackedVersion(PackVersionComponents(1, 0, 0, 0), PackVersionComponents(2, 0, 0, 0)) < 0)
#call CheckEqualsInt(0, ComparePackedVersion(PackVersionComponents(1, 2, 3, 4), PackVersionComponents(1, 2, 3, 4)))
#call CheckTrue(ComparePackedVersion(PackVersionComponents(2, 0, 0, 0), PackVersionComponents(1, 0, 0, 0)) > 0)
#call CheckTrue(SamePackedVersion(PackVersionComponents(1, 2, 3, 4), PackVersionComponents(1, 2, 3, 4)))
#call CheckFalse(SamePackedVersion(PackVersionComponents(1, 0, 0, 0), PackVersionComponents(2, 0, 0, 0)))
#undef VersionPacked
#undef VersionMajor
#undef VersionMinor
#undef VersionRevision
#undef VersionBuild
#undef VersionMS
#undef VersionLS
#undef VersionFromNumbers
#undef UnpackedMS
#undef UnpackedLS
//
// Math macros
//
#call CheckEqualsInt(8, Power(2, 3))
#call CheckEqualsInt(25, Power(5))
#call CheckEqualsInt(1, Power(2, 0))
#call CheckEqualsInt(1, Min(3, 1, 2))
// bug: Min 3-arg form returns B when A >= B, ignoring C even if C < B
//#call CheckEqualsInt(1, Min(3, 2, 1))
#call CheckEqualsInt(3, Max(1, 3, 2))
// bug: Max 3-arg form returns B when A <= B, ignoring C even if C > B
//#call CheckEqualsInt(3, Max(1, 2, 3))
#call CheckEqualsInt(3, Min(5, 3))
#call CheckEqualsInt(5, Max(5, 3))
//
// Constants
//
#call CheckEqualsInt(1, True)
#call CheckEqualsInt(0, False)
#call CheckEqualsInt(1, Yes)
#call CheckEqualsInt(0, No)
//
// Utility macros
//
#call CheckTrue(YesNo('yes'))
#call CheckTrue(YesNo('true'))
#call CheckTrue(YesNo('1'))
#call CheckFalse(YesNo('no'))
#call CheckTrue(IsDirSet('Uninstallable'))
