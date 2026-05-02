@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2026 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile and run ISTestTool unit tests

setlocal

cd /d %~dp0

call :test x86
if errorlevel 1 goto failed

echo Cleaning output of previous build
del Files\ISTestTool.exe

call :test x64
if errorlevel 1 goto failed

echo Success!

exit /b 0

:failed
echo *** FAILED ***
exit /b 1

:test
call .\compile.bat %~1 istesttool
if errorlevel 1 exit /b 1
echo Compiling %~1 Inno Setup Test Tool done

Files\ISTestTool.exe Projects\Bin\Script.Test.iss
if errorlevel 1 exit /b 1
echo Testing %~1 Inno Setup done

exit /b