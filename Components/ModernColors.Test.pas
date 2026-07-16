unit ModernColors.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for ModernColors

  Runs a self-test if DEBUG is defined
}

interface

procedure ModernColorsRunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, System.SysUtils, {$ENDIF} System.UITypes, ModernColors;

{$C+}

procedure ModernColorsRunTests;

  procedure TestColor(const ATheme: TTheme; const AThemeColor: TThemeColor;
    const AExpected: TColor);
  begin
    Assert(ATheme.Colors[AThemeColor] = AExpected);
  end;

begin
  const Theme = TTheme.Create;
  try
    { Dark and Modern track Typ correctly }
    Theme.Typ := ttModernLight;
    Assert(not Theme.Dark);
    Assert(Theme.Modern);

    Theme.Typ := ttModernDark;
    Assert(Theme.Dark);
    Assert(Theme.Modern);

    Theme.Typ := ttClassic;
    Assert(not Theme.Dark);
    Assert(not Theme.Modern);

    { System colors are passed through (not RGB-swapped) }
    Theme.Typ := ttModernLight;
    TestColor(Theme, tcToolBack, TColorRec.SysBtnFace);

    { Non-system color is RGB-swapped: ModernColors.pas defines CRed as
      $FF0000 (RRGGBB), which becomes Delphi's BGR $0000FF after the swap }
    Theme.Typ := ttClassic;
    TestColor(Theme, tcRed, $0000FF);
  finally
    Theme.Free;
  end;
end;

{$IFDEF DEBUG}
{$IFNDEF ISTESTTOOLPROJ}
initialization
  try
    ModernColorsRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}
{$ENDIF}

end.
