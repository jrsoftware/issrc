@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2026 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Builds and runs roundtrip.c, a compress->decompress check for the
rem  islzma release binaries. Build them first (compile.bat x86 and x64).
rem
rem    test.bat x86|x64

setlocal

cd /d %~dp0

if "%1"=="x86" goto archfound
if "%1"=="x64" goto archfound
echo Architecture parameter is missing or invalid. Must be "x86" or "x64"
goto failed2
:archfound
set arch=%1

if exist ..\compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo compilesettings.bat is missing or incomplete. It needs to be created
echo with the following line, adjusted for your system:
echo.
echo   set VSBUILDROOT=c:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build
goto failed2

:compilesettingsfound
set VSBUILDROOT=
call ..\compilesettings.bat
if "%VSBUILDROOT%"=="" goto compilesettingserror

set vsarch=%arch%
if "%arch%"=="x86" set vsarch=amd64_x86

set __VSCMD_ARG_NO_LOGO=1
set VSCMD_SKIP_SENDTELEMETRY=1

echo - Calling vcvarsall.bat %vsarch%
call "%VSBUILDROOT%\vcvarsall.bat" %vsarch%
if errorlevel 1 goto failed
echo.

echo - Copying release binaries next to the test
if "%arch%"=="x64" (
	copy /y ..\x64\Release\islzma-x64.dll islzma.dll >nul
) else (
	copy /y ..\Win32\Release\islzma.dll islzma.dll >nul
)
if errorlevel 1 goto failed
copy /y ..\Win32\Release\islzma32.exe islzma32.exe >nul
if errorlevel 1 goto failed
copy /y ..\x64\Release\islzma64.exe islzma64.exe >nul
if errorlevel 1 goto failed

echo - Compiling roundtrip.c
cl /nologo /W3 roundtrip.c ..\..\..\Compression.LZMADecompressor\Lzma2Decode\ISLzmaDec-%arch%.obj /Fe:roundtrip.exe
if errorlevel 1 goto failed
echo.

echo - Running test
rem  Explicit .\ because vcvarsall sets NoDefaultCurrentDirectoryInExePath.
.\roundtrip.exe
if errorlevel 1 goto failed

echo.
echo Success!
exit /b 0

:failed
echo *** FAILED ***
:failed2
exit /b 1
