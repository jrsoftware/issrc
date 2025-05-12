@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2025 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to prepare a release
rem
rem  Calls setup-sign.bat if it exists to create a signed build, otherwise creates setup.exe without signing
rem  Signed build also require a setup-presign.bat to exist which should sign all files passed to it
rem
rem  This batch files does the following things:
rem  -Compile ISHelpGen
rem  -Compile ISetup*.chm
rem  -Compile Inno Setup including ISSigTool
rem  -Create Inno Setup installer
rem
rem  Once done the installer can be found in Output

setlocal

set VER=6.5.0-dev

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

if not exist files\issigtool.exe (
  echo Missing ISSigTool
  call .\compile.bat issigtool
  if errorlevel 1 goto failed
  echo Compiling ISSigTool done
)

rem  Verify precompiled binaries which are used during compilation
rem  Note: Other precompiled binaries are verified by Setup.iss
call .\issig.bat verify --key-file=def01.ispublickey ^
  Projects\Src\Setup.HelperEXEs.res ^
  Projects\Src\Compression.LZMADecompressor\Lzma2Decode\ISLzmaDec.obj ^
  Projects\Src\Compression.LZMA1SmallDecompressor\LzmaDecode\LzmaDecodeInno.obj ^
  Projects\Src\Compression.SevenZipDecoder\7zDecode\IS7zDec.obj
if errorlevel 1 goto failed
echo ISSigTool verify done

rem  Embed user's public key into sources
call .\issig.bat embed
if errorlevel 1 goto failed
echo ISSigTool embed done

call .\compile.bat
if errorlevel 1 goto failed
echo Compiling Inno Setup done

if exist .\setup-presign.bat (
  echo - Presigning
  call .\setup-presign.bat Files\ISCC.exe Files\ISCmplr.dll Files\ISPP.dll
  echo Presign done
) 

rem  Sign using user's private key
call .\issig.bat sign Files\ISCmplr.dll Files\ISPP.dll
if errorlevel 1 goto failed
echo ISSigTool sign done
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
call .\issig.bat sign output\innosetup-%VER%.exe
if errorlevel 1 goto failed
powershell.exe -NoProfile -Command "Write-Host -NoNewline 'SHA256 hash: '; (Get-FileHash -Algorithm SHA256 -Path output\innosetup-%VER%.exe).Hash.ToLower()"
if errorlevel 1 goto failed

echo All done!
pause
exit /b 0

:failed
echo *** FAILED ***
pause
exit /b 1
