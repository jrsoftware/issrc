@echo off

rem  Inno Setup Preprocesso
rem  Copyright (C) 2001-2004 Alex Yackimoff
rem  Portions by Martijn Laan
rem
rem  Batch file to compile the help file
rem
rem  $jrsoftware: ispp/Help/compile.bat,v 1.2 2006/09/28 09:55:27 mlaan Exp $

setlocal

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo compilesettings.bat is missing or incomplete. It needs to be created
echo with the following lines, adjusted for your system:
echo.
echo   set HHCEXE=%%ProgramFiles%%\HTML Help Workshop\hhc.exe   [Path to help compiler]
echo   set ISHELPGENEXE=ishelpgen.exe   [Path to ISHelpGen]
goto failed2

:compilesettingsfound
set HHCEXE=
set ISHELPGENEXE=
call .\compilesettings.bat
if "%HHCEXE%"=="" goto compilesettingserror
if "%ISHELPGENEXE%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

echo Generating help files:
echo.
"%ISHELPGENEXE%" .
if errorlevel 1 goto failed

echo.
echo Running help compiler:
echo.
del Staging\ispp.chm
if exist Staging\ispp.chm goto failed
"%HHCEXE%" Staging\hh_project.hhp
if %errorlevel% neq 1 goto failed
if not exist Staging\ispp.chm goto failed

echo Success!
exit /b 0

:failed
echo *** FAILED ***
:failed2
exit /b 1
