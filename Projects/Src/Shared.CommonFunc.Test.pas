unit Shared.CommonFunc.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for Shared.CommonFunc

  Runs a self-test if DEBUG is defined
}

interface

procedure SharedCommonFuncRunTests;

implementation

uses
  Winapi.Windows, System.SysUtils, Shared.CommonFunc;

{$C+}

procedure SharedCommonFuncRunTests;
begin
  { AddQuotes: only adds quotes when there is a space and the string is not
    already quoted }
  Assert(AddQuotes('') = '');
  Assert(AddQuotes('foo') = 'foo');
  Assert(AddQuotes('foo bar') = '"foo bar"');
  Assert(AddQuotes('"foo bar"') = '"foo bar"');
  Assert(AddQuotes('  foo bar  ') = '"foo bar"');
  { Mismatched quotes (only one side) still get wrapped }
  Assert(AddQuotes('"foo bar') = '"' + '"foo bar' + '"');
  Assert(AddQuotes('foo bar"') = '"' + 'foo bar"' + '"');
  { Embedded quote isn't escaped or treated as a boundary quote }
  Assert(AddQuotes('a"b c') = '"a"b c"');

  { RemoveQuotes: strips matching/unmatched leading and trailing quotes }
  Assert(RemoveQuotes('') = '');
  Assert(RemoveQuotes('foo') = 'foo');
  Assert(RemoveQuotes('"foo"') = 'foo');
  Assert(RemoveQuotes('""foo""') = 'foo');
  Assert(RemoveQuotes('"foo') = 'foo');
  Assert(RemoveQuotes('foo"') = 'foo');
  Assert(RemoveQuotes('"') = '');
  Assert(RemoveQuotes('""') = '');
  { Only boundary quote runs are stripped; embedded quotes survive }
  Assert(RemoveQuotes('"foo"bar"') = 'foo"bar');

  { StringChange / StringChangeEx: returns count and rewrites in place;
    advances past the replacement (no overlap with ToStr); a zero-length
    FromStr is a no-op }
  var S: String;
  S := 'aaa';
  Assert(StringChange(S, 'a', 'b') = 3);
  Assert(S = 'bbb');
  S := 'foofoofoo';
  Assert(StringChange(S, 'foo', 'bar') = 3);
  Assert(S = 'barbarbar');

  { Overlap: with FromStr='aa' and ToStr='b', 'aaa' becomes 'ba' }
  S := 'aaa';
  Assert(StringChange(S, 'aa', 'b') = 1);
  Assert(S = 'ba');

  { Empty FromStr is a no-op, same if no matches }
  S := 'foo';
  Assert(StringChange(S, '', 'X') = 0);
  Assert(S = 'foo');
  Assert(StringChange(S, 'xyz', 'abc') = 0);
  Assert(S = 'foo');

  { ToStr longer than FromStr (string grows) }
  S := 'a.b.c';
  Assert(StringChange(S, '.', '...') = 2);
  Assert(S = 'a...b...c');

  { ConvertPercentStr: each '%hh' decodes to the matching byte }
  S := 'a%20b%41%42%43';
  Assert(ConvertPercentStr(S));
  Assert(S = 'a bABC');

  S := 'no percents here';
  Assert(ConvertPercentStr(S));
  Assert(S = 'no percents here');

  { Truncated escape (less than 2 hex digits after '%') fails }
  S := 'a%2';
  Assert(not ConvertPercentStr(S));
  S := 'a%';
  Assert(not ConvertPercentStr(S));

  { Non-hex digits after '%' fail }
  S := 'a%XYb';
  Assert(not ConvertPercentStr(S));

  { ConvertConstPercentStr: '%' inside braces is left alone, but '%' outside
    is decoded as usual }
  S := '{const%20}%20x';
  Assert(ConvertConstPercentStr(S));
  Assert(S = '{const%20} x');

  { '{{' at the start of an embedded constant is consumed by SkipPastConst }
  S := '{{not-a-const}%20';
  Assert(ConvertConstPercentStr(S));
  Assert(S = '{{not-a-const} ');

  { Nested constants are skipped as a unit, so a percent escape inside remains
    literal while one after the constant is decoded }
  S := '{outer{%21}}%21';
  Assert(ConvertConstPercentStr(S));
  Assert(S = '{outer{%21}}!');

  { Unclosed brace fails }
  S := '{open';
  Assert(not ConvertConstPercentStr(S));

  { Bad percent escape outside braces still fails }
  S := '{good}%X';
  Assert(not ConvertConstPercentStr(S));

  { ConstPos: skips chars inside Inno Setup constants }
  Assert(ConstPos(',', '') = 0);
  Assert(ConstPos(',', 'no comma here') = 0);
  Assert(ConstPos(',', 'a,b') = 2);
  Assert(ConstPos(',', '{a,b}') = 0);   { the only ',' is inside braces }
  Assert(ConstPos(',', 'a{b,c}d,e') = 8);
  { An unclosed brace only hides following characters; a match before it is
    still found }
  Assert(ConstPos(';', 'a;{open') = 2);
  Assert(ConstPos(';', 'a{open;') = 0);

  (* SkipPastConst: returns the index past the matching '}' *)
  Assert(SkipPastConst('a{b}c', 2) = 5);
  Assert(SkipPastConst('a{}b', 2) = 4);
  Assert(SkipPastConst('a{b{c}d}e', 2) = 9);
  { Unclosed brace returns 0 }
  Assert(SkipPastConst('a{bc', 2) = 0);
  Assert(SkipPastConst('a{{b', 2) = 4);
  { Start at end-of-string returns 0 }
  Assert(SkipPastConst('a{', 2) = 0);

  { RemoveAccelChar: '&' is stripped, '&&' collapses to a single '&', a
    trailing '&' is stripped }
  Assert(RemoveAccelChar('') = '');
  Assert(RemoveAccelChar('File') = 'File');
  Assert(RemoveAccelChar('F&ile') = 'File');
  Assert(RemoveAccelChar('File&') = 'File');
  Assert(RemoveAccelChar('a&&b') = 'a&b');
  Assert(RemoveAccelChar('&&') = '&');
  Assert(RemoveAccelChar('&a&b&c') = 'abc');

  { AddPeriod: adds '.' only when the last character is greater than '.';
    idempotent on already-terminated strings }
  Assert(AddPeriod('') = '');
  Assert(AddPeriod('Hello') = 'Hello.');
  Assert(AddPeriod('Hello.') = 'Hello.');
  Assert(AddPeriod('Hello!') = 'Hello!');     { '!' (33) < '.' (46) }
  Assert(AddPeriod('Hello?') = 'Hello?.');    { '?' (63) > '.' (46) }

  { IsWildcard: only the last path component is checked, so '?' or '*' in a
    drive prefix like '\\?\' must not register }
  Assert(not IsWildcard(''));
  Assert(not IsWildcard('foo'));
  Assert(IsWildcard('*.txt'));
  Assert(IsWildcard('foo?'));
  Assert(IsWildcard('foo*'));
  Assert(not IsWildcard('\\?\C:\Windows'));
  Assert(IsWildcard('\\?\C:\Win*'));

  { WildcardMatch: '?' matches exactly one character, '*' matches any run
    (including empty); a literal must match exactly }
  Assert(WildcardMatch('', ''));
  Assert(not WildcardMatch('a', ''));
  Assert(WildcardMatch('foo', 'foo'));
  Assert(not WildcardMatch('foo', 'bar'));
  Assert(not WildcardMatch('foo', 'fo'));
  Assert(not WildcardMatch('fo', 'foo'));
  Assert(WildcardMatch('foo', '*'));
  Assert(WildcardMatch('', '*'));
  Assert(WildcardMatch('foo', 'f*'));
  Assert(WildcardMatch('foo', '*o'));
  Assert(WildcardMatch('foo', 'f*o'));
  Assert(not WildcardMatch('foo', 'f*p')); { recursive '*' search exhausts }
  Assert(WildcardMatch('abcdef', 'a*d*f'));
  Assert(WildcardMatch('fo', 'f*o')); { '*' can match empty }
  Assert(WildcardMatch('foo', '**')); { consecutive '*' act as one }
  Assert(WildcardMatch('foo', '?oo'));
  Assert(WildcardMatch('foo', 'f?o'));
  Assert(not WildcardMatch('fo', '?oo'));
  Assert(not WildcardMatch('foo', '????'));
  { '*.*' requires a literal dot to be present }
  Assert(WildcardMatch('file.txt', '*.*'));
  Assert(not WildcardMatch('file', '*.*'));

  { TryStrToBoolean: case-insensitive yes/no/true/false plus '0'/'1';
    BoolResult must be left untouched on failure }
  var BoolResult: Boolean;
  BoolResult := False;
  Assert(TryStrToBoolean('1', BoolResult) and BoolResult);
  Assert(TryStrToBoolean('yes', BoolResult) and BoolResult);
  Assert(TryStrToBoolean('YES', BoolResult) and BoolResult);
  Assert(TryStrToBoolean('true', BoolResult) and BoolResult);
  BoolResult := True;
  Assert(TryStrToBoolean('0', BoolResult) and not BoolResult);
  Assert(TryStrToBoolean('no', BoolResult) and not BoolResult);
  Assert(TryStrToBoolean('false', BoolResult) and not BoolResult);

  { Unrecognised values: function returns False and BoolResult is preserved }
  BoolResult := True;
  Assert(not TryStrToBoolean('', BoolResult));
  Assert(BoolResult);
  Assert(not TryStrToBoolean('2', BoolResult));
  Assert(BoolResult);

  { CompareInt64: ordering across the full Int64 range, including signs }
  Assert(CompareInt64(0, 0) = 0);
  Assert(CompareInt64(5, 5) = 0);
  Assert(CompareInt64(5, 4) = 1);
  Assert(CompareInt64(4, 5) = -1);
  Assert(CompareInt64(-1, 1) = -1);
  Assert(CompareInt64(1, -1) = 1);
  Assert(CompareInt64(High(Int64), Low(Int64)) = 1);
  Assert(CompareInt64(Low(Int64), High(Int64)) = -1);
  Assert(CompareInt64(Low(Int64), Low(Int64)) = 0);

  { HighLowToInt64 / HighLowToUInt64: pack two UInt32s into a 64-bit value;
    Int64 reinterprets the same bit pattern as signed }
  Assert(HighLowToInt64(0, 0) = 0);
  Assert(HighLowToInt64(0, 1) = 1);
  Assert(HighLowToInt64(0, $FFFFFFFF) = $FFFFFFFF);
  Assert(HighLowToInt64(1, 0) = Int64($100000000));
  { Both halves all-ones: as Int64 this is -1, as UInt64 it is the maximum }
  Assert(HighLowToInt64($FFFFFFFF, $FFFFFFFF) = -1);
  Assert(HighLowToUInt64(0, 0) = 0);
  Assert(HighLowToUInt64(0, $FFFFFFFF) = $FFFFFFFF);
  Assert(HighLowToUInt64(1, 0) = UInt64($100000000));
  Assert(HighLowToUInt64($FFFFFFFF, $FFFFFFFF) = High(UInt64));

  { FileTimeToUInt64: same packing as HighLowToUInt64 }
  var FileTime: TFileTime;
  FileTime.dwLowDateTime := 0;
  FileTime.dwHighDateTime := 0;
  Assert(FileTimeToUInt64(FileTime) = 0);
  FileTime.dwLowDateTime := $00000002;
  FileTime.dwHighDateTime := $00000001;
  Assert(FileTimeToUInt64(FileTime) = UInt64($100000002));
  FileTime.dwLowDateTime := $FFFFFFFF;
  FileTime.dwHighDateTime := $FFFFFFFF;
  Assert(FileTimeToUInt64(FileTime) = High(UInt64));

  { Small conversion helpers used in logging, registry view selection, and
    buffer sizing. These tests pin exact mapping and boundary semantics. }
  Assert(BitsFrom64BitBoolean(False) = 32);
  Assert(BitsFrom64BitBoolean(True) = 64);

  Assert(RegViewFrom64BitBoolean(False) = rv32Bit);
  Assert(RegViewFrom64BitBoolean(True) = rv64Bit);

  Assert(IntMax(5, 3) = 5);
  Assert(IntMax(3, 5) = 5);
  Assert(IntMax(-5, -3) = -3);
  Assert(IntMax(MaxInt, -MaxInt) = MaxInt);

  { AdjustLength: True if the original buffer was strictly larger than Res
    (i.e. the API filled less than the buffer); False if it was equal or
    smaller. Either way, S is resized to Res. }
  S := 'abcdef';
  Assert(AdjustLength(S, 3));
  Assert(S = 'abc');

  S := 'abc';
  Assert(not AdjustLength(S, 3));    { Res = Length: not strictly larger }
  Assert(Length(S) = 3);

  S := 'abc';
  Assert(not AdjustLength(S, 10));   { Res > Length: buffer was too small }
  Assert(Length(S) = 10);

  { TFileTimeHelper: HasTime returns True if at least one half is non-zero;
    Clear zeroes both halves }
  FileTime.dwLowDateTime := 0;
  FileTime.dwHighDateTime := 0;
  Assert(not FileTime.HasTime);
  FileTime.dwLowDateTime := 1;
  Assert(FileTime.HasTime);
  FileTime.Clear;
  Assert(not FileTime.HasTime);
  Assert(FileTime.dwLowDateTime = 0);
  Assert(FileTime.dwHighDateTime = 0);
  FileTime.dwHighDateTime := 1;
  Assert(FileTime.HasTime);
  FileTime.Clear;
  Assert(not FileTime.HasTime);
end;

{$IFDEF DEBUG}
initialization
  try
    SharedCommonFuncRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}

end.
