@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2025 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile LzmaDecodeInno.c

setlocal

cd /d %~dp0

if "%1"=="x86" goto archfound
if "%1"=="x64" goto archfound
echo Architecture parameter is missing or invalid. Must be "x86" or "x64".
goto failed2
:archfound

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo compilesettings.bat is missing or incomplete. It needs to be created
echo with the following line, adjusted for your system:
echo.
echo   set VSTOOLSROOT=C:\Program Files\Microsoft Visual Studio\2022\Community\Common7\Tools
goto failed2

:compilesettingsfound
set VSTOOLSROOT=
call .\compilesettings.bat
if "%VSTOOLSROOT%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

set __VSCMD_ARG_NO_LOGO=1
set VSCMD_SKIP_SENDTELEMETRY=1

echo - Calling VsDevCmd.bat -arch=%1
call "%VSTOOLSROOT%\VsDevCmd.bat" -arch=%1
if errorlevel 1 goto exit
echo.

echo - Compiling LzmaDecodeInno.c
cl.exe /c /O2 /D_LZMA_OUT_READ /D_LZMA_IN_CB LzmaDecodeInno.c /FoLzmaDecodeInno-%1.obj
if errorlevel 1 goto failed

echo Success!
goto exit

:failed
echo *** FAILED ***
:failed2
exit /b 1

:exit
