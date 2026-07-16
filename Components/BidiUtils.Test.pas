unit BidiUtils.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for BidiUtils

  Runs a self-test if DEBUG is defined
}

interface

procedure BidiUtilsRunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, {$ENDIF} System.SysUtils, System.Types, BidiUtils;

{$C+}

procedure BidiUtilsRunTests;

  function RectsEqual(const A, B: TRect): Boolean;
  begin
    Result := (A.Left = B.Left) and (A.Top = B.Top) and
              (A.Right = B.Right) and (A.Bottom = B.Bottom);
  end;

  procedure TestFlip(const ARect, AParentRect, AExpected: TRect;
    const AUseRightToLeft: Boolean);
  begin
    var R := ARect;
    FlipRect(R, AParentRect, AUseRightToLeft);
    Assert(RectsEqual(R, AExpected));
  end;

begin
  const Parent = Rect(0, 0, 100, 30);

  { UseRightToLeft = False leaves Rect unchanged }
  TestFlip(Rect(10, 5, 30, 25), Parent, Rect(10, 5, 30, 25), False);

  { Top and Bottom are never modified }
  TestFlip(Rect(10, 5, 30, 25), Parent, Rect(70, 5, 90, 25), True);

  { Flush-left rect mirrors to flush-right }
  TestFlip(Rect(0, 0, 30, 30), Parent, Rect(70, 0, 100, 30), True);

  { Flush-right rect mirrors to flush-left }
  TestFlip(Rect(70, 0, 100, 30), Parent, Rect(0, 0, 30, 30), True);

  { Full-width rect is its own mirror }
  TestFlip(Parent, Parent, Parent, True);

  { Zero-width rect: width preserved, position mirrored about its left edge }
  TestFlip(Rect(50, 0, 50, 30), Parent, Rect(50, 0, 50, 30), True);
  TestFlip(Rect(20, 0, 20, 30), Parent, Rect(80, 0, 80, 30), True);

  { Rect extending past ParentRect on the left }
  TestFlip(Rect(-10, 0, 30, 30), Parent, Rect(70, 0, 110, 30), True);

  { Rect extending past ParentRect on the right }
  TestFlip(Rect(70, 0, 110, 30), Parent, Rect(-10, 0, 30, 30), True);

  { Non-zero-origin ParentRect: flip works relative to ParentRect.Left/Right }
  const Offset = Rect(10, 0, 110, 30);
  TestFlip(Rect(20, 0, 40, 30), Offset, Rect(80, 0, 100, 30), True);

  { Double flip is the identity (FlipRect is its own inverse) }
  var R := Rect(15, 5, 45, 25);
  FlipRect(R, Parent, True);
  FlipRect(R, Parent, True);
  Assert(RectsEqual(R, Rect(15, 5, 45, 25)));
end;

{$IFDEF DEBUG}
{$IFNDEF ISTESTTOOLPROJ}
initialization
  try
    BidiUtilsRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}
{$ENDIF}

end.
