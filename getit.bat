@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2026 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to get GetIt dependencies

setlocal

cd /d %~dp0

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo compilesettings.bat is missing or incomplete. It needs to contain
echo the following line, adjusted for your system:
echo.
echo   set DELPHIXEROOT=C:\Program Files\Embarcadero\RAD Studio\20.0 [Path to Delphi 10.4 Sydney (or later)]
goto failed2

:compilesettingsfound
set DELPHIXEROOT=
call .\compilesettings.bat
if "%DELPHIXEROOT%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

call "%DELPHIXEROOT%\bin\rsvars.bat"
if errorlevel 1 goto failed

rem Delphi 10.4 note: Delphi 10.4 does not display these styles when using the
rem GetIt Package Manager from the IDE, but its GetItCmd.exe can still retrieve
rem them. When it does, it obtains the same files as Delphi 12.3.

GetItCmd.exe --accepteulas --donotsetenvfile -i=VCLStyle-Windows11Light-1.0;VCLStyle-PolarLight-1.0;VCLStyle-PolarDark-1.0;VCLStyle-Stellar-2.0;VCLStyle-Zircon-2.0
if errorlevel 1 goto failed

rem VCLStyle-WindowsModernDarkSE is the same as VCLStyle-Windows11Dark, except
rem for its style name, its file name, and an empty toolbar separator image.
rem Delphi 12.3's repository contains VCLStyle-Windows11Dark but not
rem VCLStyle-WindowsModernDarkSE, and for Delphi 13.0 it's the other way
rem around. So we try to get both, and fail only if neither style file
rem exists afterwards. The exit codes can't be used for this because
rem GetItCmd.exe of Delphi 12.3 returns 0 for a package it can't find.

echo Next command will fail on Delphi 13.0 and newer, that's okay
GetItCmd.exe --accepteulas --donotsetenvfile -i=VCLStyle-Windows11Dark-1.0

echo Next command will fail on Delphi 12.3 and older, that's okay
GetItCmd.exe --accepteulas --donotsetenvfile -i=VCLStyle-WindowsModernDarkSE

if not exist "%BDSCOMMONDIR%\Styles\Windows11_Modern_Dark.vsf" if not exist "%BDSCOMMONDIR%\Styles\Windows_Modern_Dark_SE.vsf" goto failed

echo Success!

exit /b 0

:failed
echo *** FAILED ***
:failed2
exit /b 1
