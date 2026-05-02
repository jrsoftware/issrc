unit StringScanner.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for StringScanner

  Runs a self-test if DEBUG is defined
}

interface

procedure StringScannerRunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, {$ENDIF} System.SysUtils, StringScanner;

{$C+}

procedure StringScannerRunTests;
begin
  { Empty input: ReachedEnd, RemainingCount = 0, every Consume fails }
  var Scanner := TStringScanner.Create('');
  Assert(Scanner.ReachedEnd);
  Assert(Scanner.RemainingCount = 0);
  Assert(not Scanner.Consume('a'));
  Assert(not Scanner.Consume('any'));
  Assert(Scanner.ConsumeMulti(['a'..'z']) = 0);

  { Default record instance: safety checks prevent out-of-bounds reads }
  const DefaultScanner = Default(TStringScanner);
  Assert(DefaultScanner.ReachedEnd);
  Assert(DefaultScanner.RemainingCount = 0);
  Assert(not DefaultScanner.Consume('a'));

  { Consume(Char) advances on match, leaves position alone on mismatch }
  Scanner := TStringScanner.Create('abc');
  Assert(Scanner.RemainingCount = 3);
  Assert(Scanner.Consume('a'));
  Assert(Scanner.RemainingCount = 2);
  Assert(not Scanner.Consume('x'));
  Assert(Scanner.RemainingCount = 2);
  Assert(Scanner.Consume('b'));
  Assert(Scanner.Consume('c'));
  Assert(Scanner.ReachedEnd);

  { Consume(String) advances on match, leaves position alone on mismatch }
  Scanner := TStringScanner.Create('hello world');
  Assert(Scanner.Consume('hello'));
  Assert(Scanner.RemainingCount = 6);
  Assert(not Scanner.Consume('xyz'));
  Assert(Scanner.RemainingCount = 6);
  Assert(Scanner.Consume(' '));
  Assert(Scanner.Consume('world'));
  Assert(Scanner.ReachedEnd);

  { Consume(String) longer than remaining input must fail without consuming }
  Scanner := TStringScanner.Create('ab');
  Assert(not Scanner.Consume('abc'));
  Assert(Scanner.RemainingCount = 2);

  { ConsumeMulti respects AMaxChars }
  Scanner := TStringScanner.Create('abcdef123');
  Assert(Scanner.ConsumeMulti(['a'..'z'], False, 1, 3) = 3);
  Assert(Scanner.RemainingCount = 6);

  { ConsumeMulti returns 0 and consumes nothing if AMinChars not satisfied }
  Scanner := TStringScanner.Create('xy');
  Assert(Scanner.ConsumeMulti(['a'..'z'], False, 5) = 0);
  Assert(Scanner.RemainingCount = 2);

  { AMinChars = 0 still requires at least one match }
  Scanner := TStringScanner.Create('123');
  Assert(Scanner.ConsumeMulti(['a'..'z'], False, 0) = 0);
  Assert(Scanner.RemainingCount = 3);

  { AAllowAllCharsAboveFF accepts non-Latin1 codepoints }
  Scanner := TStringScanner.Create('ab' + #$3042 + '1');
  Assert(Scanner.ConsumeMulti(['a'..'z'], True) = 3);
  Assert(Scanner.RemainingCount = 1);

  { ConsumeMultiToString captures the consumed run }
  Scanner := TStringScanner.Create('abc123');
  var Captured: String;
  Assert(Scanner.ConsumeMultiToString(['a'..'z'], Captured) = 3);
  Assert(Captured = 'abc');
  Assert(Scanner.ConsumeMultiToString(['0'..'9'], Captured) = 3);
  Assert(Captured = '123');
  Assert(Scanner.ReachedEnd);

  { ConsumeMultiToString clears ACapturedString when no chars consumed }
  Scanner := TStringScanner.Create('123');
  Captured := 'leftover';
  Assert(Scanner.ConsumeMultiToString(['a'..'z'], Captured) = 0);
  Assert(Captured = '');

  {$IFDEF ISTESTTOOLPROJ}
  { Invalid AMinChars must raise }
  Scanner := TStringScanner.Create('abc');
  var Caught := False;
  try
    Scanner.ConsumeMulti(['a'..'z'], False, -1);
  except
    on EStringScannerError do Caught := True;
  end;
  Assert(Caught);

  Caught := False;
  try
    Scanner.ConsumeMulti(['a'..'z'], False, 5, 3);
  except
    on EStringScannerError do Caught := True;
  end;
  Assert(Caught);
  {$ENDIF}
end;

{$IFDEF DEBUG}
{$IFNDEF ISTESTTOOLPROJ}
initialization
  try
    StringScannerRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}
{$ENDIF}

end.
