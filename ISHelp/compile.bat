@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2024 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile the help file

setlocal

cd /d %~dp0

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo ISHelp\compilesettings.bat is missing or incomplete. It needs to be created
echo with the following lines, adjusted for your system:
echo.
echo   set HHCEXE=%%ProgramFiles%%\HTML Help Workshop\hhc.exe   [Path to help compiler]
goto failed2

:compilesettingsfound
set HHCEXE=
call .\compilesettings.bat
if "%HHCEXE%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

echo Synching dark files:
echo.
call synch-darkfiles.bat nopause
if errorlevel 1 goto failed

call :generate_help
if errorlevel 1 goto failed
call :generate_help -dark
if errorlevel 1 goto failed

del /q Staging-dark\topic_*.htm

echo Success!
exit /b 0

:generate_help
echo Generating help files using ISHelpGen:
echo.
ISHelpGen\ISHelpGen.exe . %1
if errorlevel 1 exit /b 1

echo.
echo Running HTML Help Compiler (hhc.exe):
echo.
if exist Staging%1\isetup.chm del Staging%1\isetup.chm
if exist Staging%1\isetup.chm exit /b 1
"%HHCEXE%" Staging%1\hh_project.hhp
if %errorlevel% neq 1 exit /b 1
if not exist Staging%1\isetup.chm exit /b 1

rem  HHC leaves behind a temporary file each time it runs...
if exist "%TEMP%\~hh*.tmp" del /q "%TEMP%\~hh*.tmp"

copy Staging%1\isetup.chm ..\Files\ISetup%1.chm
if not exist ..\Files\ISetup%1.chm exit /b 1

exit /b 0

:failed
echo *** FAILED ***
:failed2
exit /b 1
