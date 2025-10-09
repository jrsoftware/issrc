unit ModernColors;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Colors for modern dark and light themes, with classic theme support
}

interface

uses
  Vcl.Graphics;

type
  TThemeType = (ttModernLight, ttModernDark, ttClassic);
  TThemeColor = (tcFore, tcBack, tcToolBack, tcSelBack, tcIntelliBack,
                 tcWordAtCursorOccurrenceBack, tcSelTextOccurrenceBack,
                 tcMarginFore, tcMarginBack, tcSplitterBack, tcBraceBack, tcIndentGuideFore,
                 tcRed, tcGreen, tcBlue, tcOrange, tcReallyOrange, tcPurple,
                 tcTeal, tcGray);

  TTheme = class
  private
    FType: TThemeType;
    function FGetDark: Boolean;
    function FGetModern: Boolean;
    function FGetColor(Color: TThemeColor): TColor;
  public
    property Colors[Color: TThemeColor]: TCOlor read FGetColor;
    property Dark: Boolean read FGetDark;
    property Modern: Boolean read FGetModern;
    property Typ: TThemeType read FType write FType;
  end;

implementation

uses
  Winapi.Windows;

function TTheme.FGetColor(Color: TThemeColor): TColor;
const
  { D = Dark, L = Light, M = Modern, C = Classic
    All colors should be either RGB or a system color such as clBtnFace - so no clWhite etc! }

  DFore = $D6D6D6;           { VSCode Modern Dark, 2 tints lightened using color-hex.com }
  DBack = $1F1F1F;           { VSCode Modern Dark }
  { If you combine this unit with a dark VCL Style then the following color should match the style's
    window background color. Value can be found using BitmapStyleDesigner.exe from BDS\Bin. Open the
    style .vsf file, go to the Colors section and then to the Window color. }
  DToolBack = $2B2B2B;       { VCL Style 'Windows11 Dark 1.0' }
  DSelBack = $1D4F76;        { VSCode Modern Dark }
  //DSelInactiveBack = $4F5051;{ VSCode Modern Dark }
  DIntelliBack = $202020;    { VSCode Modern Dark }
  DWACOBack = $4A4A4A;       { VSCode Modern Dark }
  DSTOBACK = $333A40;        { VSCode Modern Dark }
  DMarginFore = $716F71;     { Monokai Pro }
  DMarginBack = DToolBack;
  DSplitterBack = DToolBack;
  DBraceBack = DWACOBack;
  DIndentGuideFore = $716F71;{ Monokai Pro }
  //Monokai Pro's dark control color: $221F22

  LFore = $3B3B3B;           { VSCode Modern Light }
  LBack = $FFFFFF;
  LToolBack = clBtnFace;
  LSelBack = $A7D6FD;        { VSCode Modern Light }
  //LSelInactiveBack = $E4EBF1;{ VSCode Modern Light }
  LIntelliBack = $F8F8F8;    { VSCode Modern Light }
  LWACOBack = $ECECEC;       { Inno Setup 5, 4 tints lightened using color-hex.com }
  LSTOBACK = $D3EAFE;        { VSCode Modern Light }
  LMarginFore = $868686;     { VSCode Modern Light, tabset }
  LMarginBack = $F8F8F8;     { VSCode Modern Light, tabset }
  LSplitterBack = LToolBack;
  LBraceBack = LWACOBack;
  LIndentGuideFore = $C0C0C0;

  CFore = $000000;
  CBack = $FFFFFF;
  CToolBack = clBtnFace;
  CSelBack = LSelBack;
  CIntelliBack = LIntelliBack;
  CWACOBack = LWACOBack;
  CSTOBACK = LSTOBack;
  CMarginFore = clWindowText;
  CMarginBack = CToolBack;
  CSplitterBack = CToolBack;
  CBraceBack = CWACOBack;
  CIndentGuideFore = $C0C0C0;

  { The colors below might differ slightly from the listed source: the contrast with LBack and
    DBack has been increased to at least 4.5 using https://webaim.org/resources/contrastchecker }

  LRed = $D24152;            { Azure DevOps }
  LGreen = $2D862D;          { Azure DevOps }
  LBlue = $0078D4;           { Azure DevOps }
  LOrange = $C55420;         { Azure DevOps }
  LPurple = $9262A8;         { Azure DevOps }
  LYellow = $F2CB1D;         { Azure DevOps }
  LTeal = $2A8472;           { Visual Studio 2017 }
  LGray = $707070;           { Inno Setup 5 }

  { All sources same as L* }

  DRed = $D95E6C;
  DGreen = $339933;
  DBlue = $0088F0;
  DOrange = $E5885E;
  DPurple = $A278B5;
  DTeal = $4EC9B0;
  DGray = $878787;

  CRed = $FF0000;
  CGreen = $008000;
  CBlue = $0000FF;
  COrange = $808000;
  CReallyOrange = $FFA500;
  CPurple = $8000C0;         { Inno Setup 5 }
  CTeal = $008080;
  CGray = $707070;           { Inno Setup 5 }

  Colors: array [TThemeType, TThemeColor] of TColor = (
    (LFore, LBack, LToolBack, LSelBack, LIntelliBack, LWACOBack, LSTOBack, LMarginFore, LMarginBack, LSplitterBack, LBraceBack, LIndentGuideFore, LRed, LGreen, LBlue, LOrange, LOrange, LPurple, LTeal, LGray),
    (DFore, DBack, DToolBack, DSelBack, DIntelliBack, DWACOBack, DSTOBack, DMarginFore, DMarginBack, DSplitterBack, DBraceBack, DIndentGuideFore, DRed, DGreen, DBlue, DOrange, DOrange, DPurple, DTeal, DGray),
    (CFore, CBack, CToolBack, CSelBack, CIntelliBack, CWACOBack, CSTOBack, CMarginFore, CMarginBack, CSplitterBack, CBraceBack, CIndentGuideFore, CRed, CGreen, CBlue, COrange, CReallyOrange, CPurple, CTeal, CGray)
  );
  
begin
  Result := Colors[FType, Color];
  if Result > 0 then begin { Same check as ColorToRGB }
    { Not a system color so change RGB to BGR as Delphi requires }
    Result := RGB(GetBValue(Result), GetGValue(Result), GetRValue(Result));
  end;
end;

function TTheme.FGetDark: Boolean;
begin
  Result := FType = ttModernDark;
end;

function TTheme.FGetModern: Boolean;
begin
  Result := FType <> ttClassic;
end;

end.
