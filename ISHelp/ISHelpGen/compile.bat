@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2025 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile ISHelpGen

setlocal

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo ishelp\ishelpgen\compilesettings.bat is missing or incomplete. It needs to be created
echo with the following lines, adjusted for your system:
echo.
echo   set DELPHIXEROOT=C:\Program Files\Embarcadero\RAD Studio\20.0 [Path to Delphi 10.4 Sydney (or later)]
goto failed2

:compilesettingsfound
set DELPHIXEROOT=
call .\compilesettings.bat
if "%DELPHIXEROOT%"=="" goto compilesettingserror

set DELPHIXELIB_WIN64=%DELPHIXEROOT%\lib\win64\release

rem -------------------------------------------------------------------------

set DELPHIXEDISABLEDWARNINGS=-W-SYMBOL_DEPRECATED -W-SYMBOL_PLATFORM -W-UNSAFE_CAST -W-EXPLICIT_STRING_CAST -W-EXPLICIT_STRING_CAST_LOSS -W-IMPLICIT_INTEGER_CAST_LOSS -W-IMPLICIT_CONVERSION_LOSS

set FLAGS=--no-config -Q -B -$L- -$C- -H -W -$T+ %DELPHIXEDISABLEDWARNINGS% %1
set FLAGSCONSOLE=%FLAGS% -CC
set NAMESPACES=System;System.Win;Winapi
set DCUDIR_WIN64=Dcu\x64\Release

echo Compiling ISHelpGen.exe:
mkdir %DCUDIR_WIN64%\ISHelpGen.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc64.exe" %FLAGSCONSOLE% -NS%NAMESPACES% -U"%DELPHIXELIB_WIN64%" -NU%DCUDIR_WIN64%\ISHelpGen.dpr ISHelpGen.dpr
if errorlevel 1 goto failed

echo Success!
exit /b 0

:failed
echo *** FAILED ***
:failed2
exit /b 1
