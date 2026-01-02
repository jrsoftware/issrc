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
rem  -Create 32-bit and 64-bit Inno Setup installers
rem
rem  Once done the installer can be found in Output

setlocal

set VER=7.0.0-dev

echo Building Inno Setup %VER%...
echo.

cd /d %~dp0

call .\compile.bat x64 issigtool
if errorlevel 1 goto failed
echo Compiling ISSigTool done

rem  Verify precompiled binaries which are used during compilation
rem  Note: Other precompiled binaries are verified by Setup.iss
call .\issig.bat verify --key-file=def01.ispublickey ^
  Projects\Src\Setup.HelperEXEs.res ^
  Projects\Src\Compression.LZMADecompressor\Lzma2Decode\ISLzmaDec-x86.obj ^
  Projects\Src\Compression.LZMADecompressor\Lzma2Decode\ISLzmaDec-x64.obj ^
  Projects\Src\Compression.LZMA1SmallDecompressor\LzmaDecode\LzmaDecodeInno-x86.obj ^
  Projects\Src\Compression.LZMA1SmallDecompressor\LzmaDecode\LzmaDecodeInno-x64.obj ^
  Projects\Src\Compression.SevenZipDecoder\7zDecode\IS7zDec-x86.obj ^
  Projects\Src\Compression.SevenZipDecoder\7zDecode\IS7zDec-x64.obj
if errorlevel 1 goto failed
echo ISSigTool verify done

rem  Embed user's public key into sources
call .\issig.bat embed
if errorlevel 1 goto failed
echo ISSigTool embed done

call .\compile.bat x64 ishelpgen
if errorlevel 1 goto failed
echo Compiling ISHelpGen done

cd ishelp
if errorlevel 1 goto failed
call .\compile.bat
if errorlevel 1 goto failed
cd ..
if errorlevel 1 goto failed
echo Compiling ISetup*.chm done
pause

call :build x86

echo Cleaning output of previous build
del Files\ISCmplr.dll Files\ISPP.dll Files\Setup.e32 Files\Setup.e64 Files\SetupCustomStyle.e32 Files\SetupCustomStyle.e64 Files\SetupLdr.e32 Files\SetupLdr.e64
if errorlevel 1 goto failed
del Files\ISIDE.exe Files\ISCC.exe Files\ISSigTool.exe
if errorlevel 1 goto failed

call :build x64

echo All done!
pause
exit /b 0

:failed
echo *** FAILED ***
pause
exit /b 1

:build
call .\compile.bat %~1
if errorlevel 1 goto failed
echo Compiling %~1 Inno Setup done

if exist .\setup-presign.bat (
  echo - Presigning
  call .\setup-presign.bat Files\ISCC.exe Files\ISCmplr.dll Files\ISPP.dll
  if errorlevel 1 goto failed
  echo Presign done
) 

rem  Sign using user's private key - also see compile.bat
call .\issig.bat sign Files\ISCmplr.dll Files\ISPP.dll Files\Setup.e32 Files\Setup.e64 Files\SetupCustomStyle.e32 Files\SetupCustomStyle.e64 Files\SetupLdr.e32 Files\SetupLdr.e64
if errorlevel 1 goto failed
echo ISSigTool sign %~1 done

:setup
echo - %~1 Setup.exe
if exist .\setup-sign.bat (
  call .\setup-sign.bat /D%~1
) else (
  files\iscc setup.iss /D%~1
)
if errorlevel 1 goto failed
echo - Renaming %~1 files
cd output
if errorlevel 1 goto failed
move /y mysetup.exe innosetup-%VER%-%~1.exe
if errorlevel 1 goto failed
cd ..
if errorlevel 1 goto failed
echo Creating %~1 Inno Setup installer done
call .\issig.bat sign output\innosetup-%VER%-%~1.exe
if errorlevel 1 goto failed

exit /b