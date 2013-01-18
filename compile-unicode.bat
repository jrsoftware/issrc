@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2012 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile all projects with Unicode support

setlocal

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo compilesettings.bat is missing or incomplete. It needs to be created
echo with the following lines, adjusted for your system:
echo.
echo   set DELPHI2009ROOT=C:\Program Files\CodeGear\RAD Studio\6.0   [Path to Delphi 2009 (or later)]
echo   set UNIROPSPATH=c:\rops\source                                [Path to ROPS]
goto failed2

:compilesettingsfound
set DELPHI2009ROOT=
set UNIROPSPATH=
call .\compilesettings.bat
if "%DELPHI2009ROOT%"=="" goto compilesettingserror
if "%UNIROPSPATH%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

rem  Compile each project separately because it seems Delphi
rem  carries some settings (e.g. $APPTYPE) between projects
rem  if multiple projects are specified on the command line.

cd Projects
if errorlevel 1 goto exit

cd ISPP
if errorlevel 1 goto failed

echo - ISPPCC.dpr
"%DELPHI2009ROOT%\bin\dcc32.exe" --no-config --peflags:1 --string-checks:off -Q -B -H -W %1 -U"%DELPHI2009ROOT%\lib" -E..\..\Files ISPPCC.dpr
if errorlevel 1 goto failed

echo - ISPP.dpr
"%DELPHI2009ROOT%\bin\dcc32.exe" --no-config --string-checks:off -Q -B -H -W %1 -U"%DELPHI2009ROOT%\lib" -E..\..\Files ISPP.dpr
if errorlevel 1 goto failed

cd ..

echo - Compil32.dpr
"%DELPHI2009ROOT%\bin\dcc32.exe" --no-config --peflags:1 --string-checks:off -Q -B -H -W %1 -U"%DELPHI2009ROOT%\lib;..\Components;%UNIROPSPATH%" -E..\Files -DPS_MINIVCL;PS_NOGRAPHCONST;PS_PANSICHAR;PS_NOINTERFACEGUIDBRACKETS Compil32.dpr
if errorlevel 1 goto failed

echo - ISCC.dpr
"%DELPHI2009ROOT%\bin\dcc32.exe" --no-config --peflags:1 --string-checks:off -Q -B -H -W %1 -U"%DELPHI2009ROOT%\lib;..\Components;%UNIROPSPATH%" -E..\Files -DPS_MINIVCL;PS_NOGRAPHCONST;PS_PANSICHAR;PS_NOINTERFACEGUIDBRACKETS ISCC.dpr
if errorlevel 1 goto failed

echo - ISCmplr.dpr
"%DELPHI2009ROOT%\bin\dcc32.exe" --no-config --string-checks:off -Q -B -H -W %1 -U"%DELPHI2009ROOT%\lib;..\Components;%UNIROPSPATH%" -E..\Files -DPS_MINIVCL;PS_NOGRAPHCONST;PS_PANSICHAR;PS_NOINTERFACEGUIDBRACKETS ISCmplr.dpr
if errorlevel 1 goto failed

echo - SetupLdr.dpr
"%DELPHI2009ROOT%\bin\dcc32.exe" --no-config --peflags:1 --string-checks:off -Q -B -H -W %1 -U"%DELPHI2009ROOT%\lib;..\Components" -E..\Files SetupLdr.dpr
if errorlevel 1 goto failed

echo - Setup.dpr
"%DELPHI2009ROOT%\bin\dcc32.exe" --no-config --peflags:1 --string-checks:off -Q -B -H -W %1 -U"%DELPHI2009ROOT%\lib;..\Components;%UNIROPSPATH%" -E..\Files -DPS_MINIVCL;PS_NOGRAPHCONST;PS_PANSICHAR;PS_NOINTERFACEGUIDBRACKETS Setup.dpr
if errorlevel 1 goto failed

echo - Renaming files
cd ..\Files
if errorlevel 1 goto failed
move SetupLdr.exe SetupLdr.e32
if errorlevel 1 goto failed
move Setup.exe Setup.e32
if errorlevel 1 goto failed

echo Success!
cd ..
goto exit

:failed
echo *** FAILED ***
cd ..
:failed2
exit /b 1

:exit
