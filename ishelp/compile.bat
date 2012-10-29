@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2010 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile the help file
rem
rem  $jrsoftware: ishelp/compile.bat,v 1.5 2010/06/07 22:15:42 jr Exp $

setlocal

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo ishelp\compilesettings.bat is missing or incomplete. It needs to be created
echo with the following lines, adjusted for your system:
echo.
echo   set DELPHI7ROOT=%%ProgramFiles%%\delphi 7                [Path to Delphi 7 (or later)]
echo   set HHCEXE=%%ProgramFiles%%\HTML Help Workshop\hhc.exe   [Path to help compiler]
goto failed2

:compilesettingsfound
set DELPHI7ROOT=
set HHCEXE=
call .\compilesettings.bat
if "%DELPHI7ROOT%"=="" goto compilesettingserror
if "%HHCEXE%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

echo Compiling ISHelpGen.dpr:
cd ISHelpGen
if errorlevel 1 goto failed
"%DELPHI7ROOT%\bin\dcc32.exe" -Q -B -H -W %1 -U"%DELPHI7ROOT%\lib" ISHelpGen.dpr
if errorlevel 1 goto failed
cd ..
if errorlevel 1 goto failed

echo.
echo Generating help files:
echo.
ISHelpGen\ISHelpGen.exe .
if errorlevel 1 goto failed

echo.
echo Running help compiler:
echo.
del Staging\isetup.chm
if exist Staging\isetup.chm goto failed
"%HHCEXE%" Staging\hh_project.hhp
if %errorlevel% neq 1 goto failed
if not exist Staging\isetup.chm goto failed

rem  HHC leaves behind a temporary file each time it runs...
if exist "%TEMP%\~hh*.tmp" del /q "%TEMP%\~hh*.tmp"

echo Success!
exit /b 0

:failed
echo *** FAILED ***
:failed2
exit /b 1
