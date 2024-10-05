@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2024 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile IS7zDec.c using Embarcadero's free
rem  C++ compiler from https://www.embarcadero.com/free-tools/ccompiler
rem  with source debugging turned on

setlocal

cd /d %~dp0

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo compilesettings.bat is missing or incomplete. It needs to be created
echo with the following line, adjusted for your system:
echo.
echo   set BCCROOT=C:\BCC102
goto failed2

:compilesettingsfound
set BCCROOT=
call .\compilesettings.bat
if "%BCCROOT%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

echo - Compiling IS7zDec.c
"%BCCROOT%\bin\bcc32c.exe" -c -O2 -v IS7zDec.c
if errorlevel 1 goto failed

echo Success!
goto exit

:failed
echo *** FAILED ***
:failed2
exit /b 1

:exit
