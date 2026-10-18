@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2026 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Runs build-verify.sh with Git Bash in the build-verify subfolder, on copies
rem  of build.bat's VER line and the installers. Must be started elevated.

setlocal

cd /d "%~dp0"
if errorlevel 1 goto failed
if not exist build-verify\Output\ mkdir build-verify\Output
if errorlevel 1 goto failed
cd build-verify
if errorlevel 1 goto failed

findstr /b /c:"set VER=" ..\..\..\build.bat >build.bat
if errorlevel 1 goto failed
set VER=
for /f "tokens=2 delims==" %%v in (build.bat) do set VER=%%v
if "%VER%"=="" goto failed
rem  /j copies unbuffered, so no cache flush to Output shows up in the trace
xcopy /y /j ..\..\..\Output\innosetup-%VER%-*.exe Output\
if errorlevel 1 goto failed

rem  An existing PML makes Procmon prompt before it starts capturing
for %%a in (x86 x64) do del trace\%%a\procmon.pml trace\%%a\procmon.csv trace\%%a\filtered-setupldr.csv trace\%%a\filtered-setup.csv 2>nul
for %%a in (x86 x64) do for %%f in (procmon.pml procmon.csv filtered-setupldr.csv filtered-setup.csv) do if exist trace\%%a\%%f goto failed

"%ProgramFiles%\Git\bin\bash.exe" --noprofile --norc ../build-verify.sh
if errorlevel 1 goto failed

echo Success!
goto exit

:failed
echo *** FAILED ***
:exit
pause
