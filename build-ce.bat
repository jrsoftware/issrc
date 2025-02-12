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
rem  -Ask the user to compile Inno Setup and ISHelpGen after clearing output first
rem  -Compile ISetup*.chm
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

call :deletefile files\compil32.exe
call :deletefile files\iscc.exe
call :deletefile files\iscmplr.dll
call :deletefile files\ispp.dll
call :deletefile files\setup.e32
call :deletefile files\setupldr.e32
call :deletefile ishelp\ishelpgen\ishelpgen.exe

echo.
echo Clearing compilation output done
echo Now open Projects\Projects.groupproj and build all projects in Release mode

echo Waiting for files...
call :waitforfile files\compil32.exe
call :waitforfile files\iscc.exe
call :waitforfile files\iscmplr.dll
call :waitforfile files\ispp.dll
call :waitforfile files\setup.e32
call :waitforfile files\setupldr.e32
call :waitforfile ishelp\ishelpgen\ishelpgen.exe

echo Found all, waiting 2 seconds more...
timeout /t 2 /nobreak >nul

cd ishelp
if errorlevel 1 goto failed
call .\compile.bat
if errorlevel 1 goto failed
cd ..
if errorlevel 1 goto failed
echo Compiling ISetup*.chm done
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

:deletefile
if exist "%~1" (
    del "%~1"
    if exist "%~1" goto failed
    echo Cleared %~1
) 
exit /b

:waitforfile
if not exist "%~1" (
    timeout /t 1 /nobreak >nul
    goto waitforfile
)
echo Found %~1
exit /b
