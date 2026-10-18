unit SetupLdrAndSetup.XPTheme;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Links the manifest. Also enables themes, except in SetupLdr. Used only by
  the Setup, SetupCustomStyle and SetupLdr projects.

  Note: XPTheme must be included directly after SafeDLLPath in the program's
  "uses" clause so that its code runs before any VCL initialization code.
}

interface

implementation

{ SetupLdr's manifest differs from Setup's in three ways:
  - It has a loadFrom entry for comctl32.dll in place of the Common-Controls
    version 6 dependency. This prevents comctl32.dll sideloading, because
    SetupLdr may run from an untrusted directory. As a result, SetupLdr gets
    comctl32.dll version 5, so it has no themes and cannot use task dialogs.
  - It has no loadFrom entries for winspool.drv and wtsapi32.dll. Only VCL
    imports these, and SetupLdr doesn't use VCL.
  - It has no loadFrom entry for sspicli.dll. Setup loads it through
    winhttp.dll, but SetupLdr doesn't load it. }

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
