@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2024 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to prepare a release
rem
rem  Calls setup-sign.bat if it exists, else creates setup.exe without signing
rem
rem  This batch files does the following things:
rem  -Compile ISHelpGen
rem  -Compile ISetup*.chm
rem  -Compile Inno Setup
rem  -Create Inno Setup installer
rem
rem  Once done the installer can be found in Output

setlocal

set VER=6.4.1

echo Building Inno Setup %VER%...
echo.

cd /d %~dp0

if "%1"=="setup" goto setup
if not "%1"=="" goto failed

cd ishelp\ishelpgen
if errorlevel 1 goto failed
call .\compile.bat
if errorlevel 1 goto failed
cd ..\..
if errorlevel 1 goto failed
echo Compiling ISHelpGen done
pause

cd ishelp
if errorlevel 1 goto failed
call .\compile.bat
if errorlevel 1 goto failed
cd ..
if errorlevel 1 goto failed
echo Compiling ISetup*.chm done
pause

call .\compile.bat
if errorlevel 1 goto failed
echo Compiling Inno Setup done
pause

:setup
echo - Setup.exe
if exist .\setup-sign.bat (
  call .\setup-sign.bat
) else (
  files\iscc setup.iss
)
if errorlevel 1 goto failed
echo - Renaming files
cd output
if errorlevel 1 goto failed
move /y mysetup.exe innosetup-%VER%.exe
if errorlevel 1 goto failed
cd ..
if errorlevel 1 goto failed
echo Creating Inno Setup installer done
powershell.exe -Command "Write-Host -NoNewline 'SHA256 hash: '; (Get-FileHash -Algorithm SHA256 -Path output\innosetup-%VER%.exe).Hash.ToLower()"

echo All done!
pause
exit /b 0

:failed
echo *** FAILED ***
pause
exit /b 1
