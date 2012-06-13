@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2012 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to prepare a release
rem
rem  Edit the desired version tag and number below before running
rem
rem  Calls setup-sign.bat if it exists, else creates setup.exe without signing
rem
rem  This batch files does the following things:
rem  -Compile ISPP.chm
rem  -Compile ISetup.chm
rem  -Compile ANSI Inno Setup
rem  -Create ANSI Inno Setup setup.exe
rem  -Compile Unicode Inno Setup
rem  -Create Unicode Inno Setup setup.exe
rem  -Zip source code
rem
rem  Once done the 2 setup files and the source code zip can be found in Output
rem  and additionally 2 ISCC files are stored there for later use while
rem  preparing an ISPack release

setlocal

set VER=5_5_1
set VER2=5.5.1

echo %VER2%?
pause

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
echo ANSI compile done
pause
if errorlevel 1 goto failed
echo - Setup.exe
if exist .\setup-sign.bat (
  call .\setup-sign.bat
) else (
  files\isppcc setup.iss /q /DNOSIGNTOOL
)
if errorlevel 1 goto failed
echo - Copying ISCC for ISPack
copy files\ISCC.exe output\ISCC-%VER2%.exe
if errorlevel 1 goto failed
echo - Renaming files
cd output
if errorlevel 1 goto failed
move /y setup.exe isetup-%VER2%.exe
cd ..
if errorlevel 1 goto failed
echo ANSI setup done
pause

call .\compile-unicode.bat
echo Unicode compile done
pause
if errorlevel 1 goto failed
echo - Setup.exe
if exist .\setup-sign.bat (
  call .\setup-sign.bat
) else (
  files\isppcc setup.iss /q /DNOSIGNTOOL
)
if errorlevel 1 goto failed
echo - Copying ISCC for ISPack
copy files\ISCC.exe output\ISCC-%VER2%-unicode.exe
if errorlevel 1 goto failed
echo - Renaming files
cd output
if errorlevel 1 goto failed
move /y setup.exe isetup-%VER2%-unicode.exe
cd ..
if errorlevel 1 goto failed
echo Unicode setup done
pause

echo Do src.bat?
pause

call .\src.bat %VER% %VER2%
if errorlevel 1 goto failed

echo Src done
echo All done!
pause
exit /b 0

:failed
echo *** FAILED ***
pause
exit /b 1