@echo off

rem  Inno Setup Preprocessor
rem  Copyright (C) 2001-2004 Alex Yackimoff
rem  Portions by Martijn Laan
rem
rem  Batch file to compile the help file

setlocal

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo Projects\ISPP\Help\compilesettings.bat is missing or incomplete. It needs
echo to be created with the following lines, adjusted for your system:
echo.
echo   set HHCEXE=%%ProgramFiles%%\HTML Help Workshop\hhc.exe   [Path to help compiler]
goto failed2

:compilesettingsfound
set HHCEXE=
call .\compilesettings.bat
if "%HHCEXE%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

echo Generating help files:
echo.
..\..\..\ishelp\ISHelpGen\ISHelpGen.exe .
if errorlevel 1 goto failed

echo.
echo Running help compiler:
echo.
del Staging\ispp.chm
if exist Staging\ispp.chm goto failed
"%HHCEXE%" Staging\hh_project.hhp
if %errorlevel% neq 1 goto failed
if not exist Staging\ispp.chm goto failed

rem  HHC leaves behind a temporary file each time it runs...
if exist "%TEMP%\~hh*.tmp" del /q "%TEMP%\~hh*.tmp"

copy Staging\ispp.chm ..\..\..\Files\ISPP.chm
if not exist ..\..\..\Files\ISPP.chm goto failed

echo Success!
exit /b 0

:failed
echo *** FAILED ***
:failed2
exit /b 1
