unit ModernColors;

{
  Inno Setup
  Copyright (C) 1997-2019 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Modern colors for dark and light themes
}

interface

uses
  Graphics;

const
  { Microsoft Azure DevOps based foreground colors, these work for both dark and light themes.
    The red and blue colors also match those used by Microsoft's VS Image Library quite nicely. }
  clModernRed = $3D29CC;
  clModernGreen = $339933;
  clModernBlue = $D47800;
  clModernPurple = $933B77;
  clModernOrange = $5E88E5;
  clModernYellow = $1DCBF2;

  { From VS2017 }
  clModernTeal = $B0C94E;

  { From Inno Setup }
  clModernGray = $707070;
  
  { Monokai Pro dark theme colors }
  clModernDarkFore = clWhite;
  clModernDarkBack = $2C282C;
  clModernDarkHiBack = $3E3A3D;

  { Other dark theme colors }
  clModernDarkHiFore = clModernGray;

  { Light theme colors }
  clModernLightFore = clBlack;
  clModernLightBack = clWhite;
  clModernLightLoFore = clModernGray;
  clModernLightLoBack = $FAFAFA;
  
implementation

end.
