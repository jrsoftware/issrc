@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2026 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile and run ISTestTool unit tests

setlocal

cd /d %~dp0

if "%1"=="" (
  set "Archs=x86 x64"
) else if "%1"=="x86" (
  set "Archs=x86"
) else if "%1"=="x64" (
  set "Archs=x64"
) else (
  echo Architecture parameter is invalid. Must be omitted or be "x86" or "x64".
  goto failed
)

for %%a in (%Archs%) do (
  call :test %%a
  if errorlevel 1 goto failed
)

echo Success!

exit /b 0

:failed
echo *** FAILED ***
exit /b 1

:test
echo Cleaning output of previous build
if exist Files\ISTestTool.exe del Files\ISTestTool.exe

call .\compile.bat %~1 ISTestTool
if errorlevel 1 exit /b 1
echo Compiling %~1 Inno Setup Test Tool done

Files\ISTestTool.exe Projects\Bin\Script.Test.iss
if errorlevel 1 exit /b 1
echo Testing %~1 Inno Setup done

exit /b