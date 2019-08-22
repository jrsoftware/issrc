@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2012 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile ISHelpGen

setlocal

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo ishelp\compilesettings.bat is missing or incomplete. It needs to be created
echo with the following lines, adjusted for your system:
echo.
echo   set DELPHI7ROOT=%%ProgramFiles%%\delphi 7                [Path to Delphi 7 (or later)]
goto failed2

:compilesettingsfound
set DELPHI7ROOT=
call .\compilesettings.bat
if "%DELPHI7ROOT%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

echo Compiling ISHelpGen.dpr:
"%DELPHI7ROOT%\bin\dcc32.exe" -Q -B -H -W %1 -U"%DELPHI7ROOT%\lib" ISHelpGen.dpr
if errorlevel 1 goto failed

echo Success!
exit /b 0

:failed
echo *** FAILED ***
:failed2
exit /b 1
