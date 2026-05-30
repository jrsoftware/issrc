unit Shared.VerInfoFunc.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for Shared.VerInfoFunc

  Runs a self-test if DEBUG is defined
}

interface

procedure SharedVerInfoFuncRunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, System.SysUtils, {$ENDIF} Shared.VerInfoFunc;

{$C+}

procedure SharedVerInfoFuncRunTests;

  procedure TestStrToVersionNumbers(const S: String; const ExpectedResult: Boolean;
    const ExpectedMS, ExpectedLS: UInt32);
  begin
    var V: TFileVersionNumbers;
    Assert(StrToVersionNumbers(S, V) = ExpectedResult);
    if ExpectedResult then begin
      Assert(V.MS = ExpectedMS);
      Assert(V.LS = ExpectedLS);
    end;
  end;

  procedure TestVersionNumbersToInt64(const MS, LS: UInt32; const ExpectedResult: Int64);
  begin
    var V: TFileVersionNumbers;
    V.MS := MS;
    V.LS := LS;
    Assert(VersionNumbersToInt64(V) = ExpectedResult);
  end;

begin
  TestStrToVersionNumbers('', True, 0, 0);
  TestStrToVersionNumbers('   ', True, 0, 0);
  TestStrToVersionNumbers('1', True, $00010000, 0);
  TestStrToVersionNumbers('1.2', True, $00010002, 0);
  TestStrToVersionNumbers('1.2.3', True, $00010002, $00030000);
  TestStrToVersionNumbers('1.2.3.4', True, $00010002, $00030004);
  TestStrToVersionNumbers('65535.65535.65535.65535', True, $FFFFFFFF, $FFFFFFFF);
  TestStrToVersionNumbers(' 1 . 2 . 3 . 4 ', True, $00010002, $00030004);
  TestStrToVersionNumbers('1.2.3.4.', True, $00010002, $00030004);
  TestStrToVersionNumbers('1.2.3.4.5', True, $00010002, $00030004);

  TestStrToVersionNumbers('65536', False, 0, 0);
  TestStrToVersionNumbers('1.2.3.65536', False, 0, 0);
  TestStrToVersionNumbers('-1', False, 0, 0);
  TestStrToVersionNumbers('abc', False, 0, 0);
  TestStrToVersionNumbers('1.x', False, 0, 0);
  TestStrToVersionNumbers('1..2', False, 0, 0);
  TestStrToVersionNumbers('.5', False, 0, 0);
  TestStrToVersionNumbers('.', False, 0, 0);

  TestVersionNumbersToInt64(0, 0, 0);
  TestVersionNumbersToInt64(0, $00000001, 1);
  TestVersionNumbersToInt64(0, $FFFFFFFF, $FFFFFFFF);
  TestVersionNumbersToInt64($00000001, 0, Int64($100000000));
  TestVersionNumbersToInt64($FFFFFFFF, $FFFFFFFF, -1);
  TestVersionNumbersToInt64($00010002, $00030004, Int64($0001000200030004));

  var V, V2: TFileVersionNumbers;
  Assert(StrToVersionNumbers('1.0.0.0', V) and StrToVersionNumbers('1.0.0.1', V2));
  Assert(VersionNumbersToInt64(V2) > VersionNumbersToInt64(V));
  Assert(StrToVersionNumbers('1.65535.65535.65535', V) and StrToVersionNumbers('2.0.0.0', V2));
  Assert(VersionNumbersToInt64(V2) > VersionNumbersToInt64(V));
end;

{$IFDEF DEBUG}
{$IFNDEF ISTESTTOOLPROJ}
initialization
  try
    SharedVerInfoFuncRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}
{$ENDIF}

end.
