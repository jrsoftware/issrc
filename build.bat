@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2012 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to prepare a release
rem
rem  Calls setup-sign.bat if it exists, else creates setup.exe without signing
rem
rem  This batch files does the following things:
rem  -Compile ISPP.chm
rem  -Compile ISetup.chm
rem  -Compile ANSI Inno Setup
rem  -Create ANSI Inno Setup installer
rem  -Compile Unicode Inno Setup
rem  -Create Unicode Inno Setup installer
rem
rem  Once done the 2 installers can be found in Output

setlocal

set VER=5.6.1

echo Building Inno Setup %VER%...
echo.

cd /d %~dp0

cd projects\ispp\help
if errorlevel 1 goto failed
call .\compile.bat
if errorlevel 1 goto failed
cd ..\..\..
if errorlevel 1 goto failed
echo ISPP help done
pause

cd ishelp
if errorlevel 1 goto failed
call .\compile.bat
if errorlevel 1 goto failed
cd ..
if errorlevel 1 goto failed
echo IS help done
pause

call .\compile.bat
if errorlevel 1 goto failed
echo ANSI compile done
pause
echo - Setup.exe
if exist .\setup-sign.bat (
  call .\setup-sign.bat
) else (
  files\iscc setup.iss /qp /DNOSIGNTOOL
)
if errorlevel 1 goto failed
echo - Renaming files
cd output
if errorlevel 1 goto failed
move /y mysetup.exe innosetup-%VER%.exe
if errorlevel 1 goto failed
cd ..
if errorlevel 1 goto failed
echo ANSI setup done
pause

call .\compile-unicode.bat
if errorlevel 1 goto failed
echo Unicode compile done
pause
echo - Setup.exe
if exist .\setup-sign.bat (
  call .\setup-sign.bat
) else (
  files\iscc setup.iss /q /DNOSIGNTOOL
)
if errorlevel 1 goto failed
echo - Renaming files
cd output
if errorlevel 1 goto failed
move /y mysetup.exe innosetup-%VER%-unicode.exe
if errorlevel 1 goto failed
cd ..
if errorlevel 1 goto failed
echo Unicode setup done

echo All done!
pause
exit /b 0

:failed
echo *** FAILED ***
pause
exit /b 1