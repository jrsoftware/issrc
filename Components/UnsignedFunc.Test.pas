unit UnsignedFunc.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for UnsignedFunc

  Runs a self-test if DEBUG is defined
}

interface

procedure UnsignedFuncRunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, {$ENDIF} System.SysUtils, UnsignedFunc;

{$C+}

procedure UnsignedFuncRunTests;
  var
    Source, Dest, BufA, BufB: array[0..15] of Byte;
begin
  { ULength: one assertion per actually-used overload pins down overload resolution }
  const SUnicode: String = 'hello';
  Assert(ULength(SUnicode) = 5);
  const SAnsi: RawByteString = 'hello world';
  Assert(ULength(SAnsi) = 11);
  const SWide: WideString = 'foobar';
  Assert(ULength(SWide) = 6);
  const Bytes: TBytes = [1, 2, 3, 4, 5, 6, 7, 8];
  Assert(ULength(Bytes) = 8);

  { UMove copies bytes correctly for small counts }
  for var I := 0 to High(Source) do
    Source[I] := Byte(I + 1);
  FillChar(Dest, SizeOf(Dest), 0);
  UMove(Source, Dest, SizeOf(Source));
  for var I := 0 to High(Source) do
    Assert(Dest[I] = Source[I]);

  { UFillChar fills the requested range }
  FillChar(Dest, SizeOf(Dest), 0);
  UFillChar(Dest, SizeOf(Dest), $42);
  for var I := 0 to High(Dest) do
    Assert(Dest[I] = $42);

  { UCompareMem returns True for equal buffers, and False when any byte differs }
  for var I := 0 to High(BufA) do begin
    BufA[I] := Byte(I);
    BufB[I] := Byte(I);
  end;
  Assert(UCompareMem(@BufA, @BufB, SizeOf(BufA)));
  BufB[10] := BufB[10] xor $01;
  Assert(not UCompareMem(@BufA, @BufB, SizeOf(BufA)));

  { UCompareMem with identical pointers must short-circuit to True even for nil }
  Assert(UCompareMem(@BufA, @BufA, SizeOf(BufA)));
  Assert(UCompareMem(nil, nil, 16));
end;

{$IFDEF DEBUG}
initialization
  try
    UnsignedFuncRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}

end.
