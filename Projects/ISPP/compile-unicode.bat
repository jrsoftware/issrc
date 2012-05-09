@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2010 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile all ISPP projects with Unicode support
rem
rem  $jrsoftware: ispp/compile-unicode.bat,v 1.7 2010/12/27 12:10:38 mlaan Exp $

setlocal

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo compilesettings.bat is missing or incomplete. It needs to be created
echo with the following lines, adjusted for your system:
echo.
echo   set DELPHI2009ROOT=C:\Program Files\CodeGear\RAD Studio\6.0   [Path to Delphi 2009 (or later)]
goto exit

:compilesettingsfound
set DELPHI2009ROOT=
call .\compilesettings.bat
if "%DELPHI2009ROOT%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

rem  Compile each project separately because it seems Delphi
rem  carries some settings (e.g. $APPTYPE) between projects
rem  if multiple projects are specified on the command line.

echo - ISPPCC.dpr
"%DELPHI2009ROOT%\bin\dcc32.exe" --no-config --peflags:1 --string-checks:off -Q -B -H -W %1 -U"%DELPHI2009ROOT%\lib" -EFiles ISPPCC.dpr
if errorlevel 1 goto failed

echo - ISPP.dpr
"%DELPHI2009ROOT%\bin\dcc32.exe" --no-config --string-checks:off -Q -B -H -W %1 -U"%DELPHI2009ROOT%\lib" -EFiles ISPP.dpr
if errorlevel 1 goto failed

echo - Renaming files
cd Files
if errorlevel 1 goto failed
rem move x y
rem if errorlevel 1 goto failed

echo Success!
cd ..
goto exit

:failed
echo *** FAILED ***
cd ..
exit /b 1

:exit
