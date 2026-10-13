unit SetupLdrAndSetup.XPTheme;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Enables themes. Used only by the Setup, SetupCustomStyle and SetupLdr
  projects.

  Note: XPTheme must be included directly after SafeDLLPath in the program's
  "uses" clause so that its code runs before any VCL initialization code.
}

interface

implementation

{$IFDEF SETUPLDRPROJ}
  {$IFNDEF WIN64}
  {$R SetupLdr.XPTheme-x86.res}
  {$ELSE}
  {$R SetupLdr.XPTheme-x64.res}
  {$ENDIF}
{$ELSE}
  {$IFNDEF WIN64}
  {$R Setup.XPTheme-x86.res}
  {$ELSE}
  {$R Setup.XPTheme-x64.res}
  {$ENDIF}
{$ENDIF}

uses
  Windows;

{ Avoid including Variants (via CommCtrl) in SetupLdr (SetupLdr uses XPTheme), saving 26 KB. }
procedure InitCommonControls; external comctl32 name 'InitCommonControls';

initialization
  InitCommonControls;
end.
