unit SetupLdrAndSetup.XPTheme;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Enables themes. Used only by the Setup and SetupLdr projects.

  Note: XPTheme must be included as the first unit in the program's "uses"
  clause so that its code runs before any VCL initialization code.
}

interface

implementation

{$R SetupLdrAndSetup.XPTheme.res}

uses
  Windows;

{ Avoid including Variants (via CommCtrl) in SetupLdr (SetupLdr uses XPTheme), saving 26 KB. }
procedure InitCommonControls; external comctl32 name 'InitCommonControls';

initialization
  InitCommonControls;
end.
