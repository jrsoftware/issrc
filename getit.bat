@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2025 Jordan Russell
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

GetItCmd.exe --accepteulas --donotsetenvfile -i=VCLStyle-Windows11Light-1.0;VCLStyle-Windows11Dark-1.0;VCLStyle-PolarLight-1.0;VCLStyle-PolarDark-1.0;VCLStyle-Zircon-2.0
if errorlevel 1 goto failed

echo Success!

goto exit

:failed
echo *** FAILED ***
cd ..
:failed2
exit /b 1

:exit
