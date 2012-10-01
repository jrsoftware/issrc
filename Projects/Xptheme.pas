unit XPTheme;

{
  Inno Setup
  Copyright (C) 1997-2007 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Enables themes on Windows XP/Vista, and disables DPI scaling on Vista.
  Used only by the Setup and SetupLdr projects.

  Note: XPTheme must be included as the first unit in the program's "uses"
  clause so that its code runs before any VCL initialization code.
}

interface

implementation

{$R XPTheme.res}

uses
  Windows;

{ Avoid including Variants (via CommCtrl) in SetupLdr (SetupLdr uses XPTheme), saving 26 KB. }
procedure InitCommonControls; external comctl32 name 'InitCommonControls';

initialization
  { Work around bug in Windows XP Gold & SP1: If the application manifest
    specifies COMCTL32.DLL version 6.0 (to enable visual styles), we must
    call InitCommonControls() to ensure that we actually link to
    COMCTL32.DLL, otherwise calls to MessageBox() fail. (XP SP2 appears
    to fix this.)
    Programs that don't statically link to COMCTL32, like SetupLdr, need this.
    (Actually, that's not completely true -- SetupLdr uses RedirFunc, which
    loads SHELL32.DLL, which in turn loads COMCTL32.DLL. But let's not rely on
    that undocumented behavior.) }
  InitCommonControls;
end.
