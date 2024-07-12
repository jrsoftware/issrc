@echo off

rem  Inno Setup Preprocessor
rem  Copyright (C) 2001-2004 Alex Yackimoff
rem  Portions by Martijn Laan
rem
rem  Batch file to compile the help file

setlocal

cd /d %~dp0

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo ISPPHelp\compilesettings.bat is missing or incomplete. It needs
echo to be created with the following lines, adjusted for your system:
echo.
echo   set HHCEXE=%%ProgramFiles%%\HTML Help Workshop\hhc.exe   [Path to help compiler]
goto failed2

:compilesettingsfound
set HHCEXE=
call .\compilesettings.bat
if "%HHCEXE%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

echo Synching IS files:
echo.
call synch-isfiles.bat nopause
if errorlevel 1 goto failed

echo Synching dark files:
echo.
call synch-darkfiles.bat
if errorlevel 1 goto failed

call :generate_help
if errorlevel 1 goto failed
call :generate_help -dark
if errorlevel 1 goto failed

echo Success!
exit /b 0

:generate_help
echo Generating help files using ISHelpGen:
echo.
..\ISHelp\ISHelpGen\ISHelpGen.exe . %1
if errorlevel 1 exit /b 1

echo.
echo Running HTML Help Compiler (hhc.exe):
echo.
del Staging%1\ispp%1.chm
if exist Staging%1\ispp%1.chm exit /b 1
"%HHCEXE%" Staging%1\hh_project.hhp
if %errorlevel% neq 1 exit /b 1
if not exist Staging%1\ispp%1.chm exit /b 1

rem  HHC leaves behind a temporary file each time it runs...
if exist "%TEMP%\~hh*.tmp" del /q "%TEMP%\~hh*.tmp"

copy Staging%1\ispp%1.chm ..\Files\ISPP%1.chm
if not exist ..\Files\ISPP%1.chm exit /b 1

exit /b 0

:failed
echo *** FAILED ***
:failed2
exit /b 1
