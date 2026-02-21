@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2026 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile islzma

setlocal

cd /d %~dp0

if "%1"=="x86" goto archfound
if "%1"=="x64" goto archfound
if "%1"=="arm64" goto archfound
echo Architecture parameter is missing or invalid. Must be "x86" or "x64" or "arm64".
goto failed2
:archfound

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo compilesettings.bat is missing or incomplete. It needs to be created
echo with the following line, adjusted for your system:
echo.
echo   set VSBUILDROOT=c:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build
goto failed2

:compilesettingsfound
set VSBUILDROOT=
call .\compilesettings.bat
if "%VSBUILDROOT%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

set vsarch=%1
if "%1"=="x86" set vsarch=amd64_x86
if "%1"=="arm64" set vsarch=amd64_arm64

set __VSCMD_ARG_NO_LOGO=1
set VSCMD_SKIP_SENDTELEMETRY=1

echo - Calling vcvarsall.bat %vsarch%
call "%VSBUILDROOT%\vcvarsall.bat" %vsarch%
if errorlevel 1 goto failed
echo.

set platform=%1
if "%1"=="x86" set platform=Win32
if "%1"=="arm64" set platform=Arm64EC

echo - Compiling islzma
msbuild.exe islzma.sln /t:Clean;Build /p:Configuration=Release;Platform=%platform% /nologo
if errorlevel 1 goto failed

echo Success!
goto exit

:failed
echo *** FAILED ***
:failed2
exit /b 1

:exit
