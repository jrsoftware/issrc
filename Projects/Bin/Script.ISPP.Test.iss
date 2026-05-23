#define CheckTrue(Value) \
  Value ? 0 : Error('CheckTrue test failed')

#define CheckFalse(Value) \
  !Value ? 0 : Error('CheckFalse test failed')

#define CheckEqualsInt(Expected, Actual) \
  Expected == Actual ? 0 : \
  Error('CheckEqualsInt test failed: expected ' + Str(Expected) + ', got ' + Str(Actual))

#define CheckEqualsString(Expected, Actual) \
  SameStr(Expected, Actual) ? 0 : \
  Error('CheckEqualsString test failed: expected "' + Expected + '", got "' + Actual + '"')

; Numeric literals

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

; String literals (Pascal mode)

#expr CheckEqualsString('hello', 'hello')
#expr CheckEqualsString('it''s', 'it''s')
#expr CheckEqualsString("hello", "hello")
#expr CheckEqualsString("hello", 'hello')
#expr CheckEqualsInt(0, Len(''))
#expr CheckEqualsInt(2, Len("\n"))
#expr CheckFalse(SameStr("\n", NewLine))

; String literals (C mode)

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
