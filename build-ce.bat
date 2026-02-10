@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2026 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to prepare a release of 64-bit Inno Setup
rem
rem  Calls setup-sign.bat if it exists to create a signed build, otherwise creates setup.exe without signing
rem  Signed builds also require a setup-presign.bat to exist which should sign all files passed to it
rem
rem  This batch files does the following things:
rem  -Ask the user to compile Inno Setup including ISSigTool and ISHelpGen after clearing output first
rem  -Compile ISetup*.chm
rem  -Create 64-bit Inno Setup installer
rem
rem  Once done the installer can be found in Output

setlocal

set VER=7.0.0-preview-1

echo Building Inno Setup %VER%...
echo.

cd /d %~dp0

if /I "%1"=="setup" goto setup
if not "%1"=="" goto failed

if not exist files\issigtool.exe (
  echo Missing ISSigTool
  echo Now open Projects\Projects.groupproj and build the ISSigTool project and its Win64 target in Release mode

  echo - Waiting for file...
  call :waitforfile files\issigtool.exe
  echo Compiling ISSigTool done
)

rem  Verify precompiled binaries which are used during compilation
rem  Note: Other precompiled binaries are verified by Setup.iss
call .\issig.bat verify --key-file=def01.ispublickey ^
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

if not exist files\ishelpgen.exe (
  echo Missing ISHelpGen
  echo Now open Projects\Projects.groupproj and build the ISHelpGen project and its Win64 target in Release mode

  echo - Waiting for file...
  call :waitforfile files\ishelpgen.exe
  echo Compiling ISHelpGen done
)

cd ishelp
if errorlevel 1 goto failed
call .\compile.bat
if errorlevel 1 goto failed
cd ..
if errorlevel 1 goto failed
echo Compiling ISetup*.chm done
pause

echo.
call :deletefile files\iside.exe
call :deletefile files\iscc.exe
call :deletefile files\iscmplr.dll
call :deletefile files\ispp.dll
call :deletefile files\setup.e32
call :deletefile files\setup.e64
call :deletefile files\setupcustomstyle.e32
call :deletefile files\setupcustomstyle.e64
call :deletefile files\setupldr.e32
call :deletefile files\setupldr.e64
call :deletefile files\issigtool.exe
call :deletefile ishelp\ishelpgen\ishelpgen.exe

echo Clearing compilation output done
echo Now open Projects\Projects.groupproj and build the Release64 build group
echo You can open the Build Groups pane from the Projects tool window

echo - Waiting for files...
call :waitforfile files\iside.exe
call :waitforfile files\iscc.exe
call :waitforfile files\iscmplr.dll
call :waitforfile files\ispp.dll
call :waitforfile files\setup.e32
call :waitforfile files\setup.e64
call :waitforfile files\setupcustomstyle.e32
call :waitforfile files\setupcustomstyle.e64
call :waitforfile files\setupldr.e32
call :waitforfile files\setupldr.e64
call :waitforfile files\issigtool.exe
call :waitforfile ishelp\ishelpgen\ishelpgen.exe

echo Found all, waiting 2 seconds more...
timeout /t 2 /nobreak >nul
echo Compiling Inno Setup done

if exist .\setup-presign.bat (
  echo - Presigning
  call .\setup-presign.bat Files\ISCC.exe Files\ISCmplr.dll Files\ISPP.dll
  if errorlevel 1 goto failed
  echo Presign done
)

rem  Sign using user's private key - also see compile.bat
call .\issig.bat sign Files\ISCmplr.dll Files\ISPP.dll Files\Setup.e32 Files\Setup.e64 Files\SetupCustomStyle.e32 Files\SetupCustomStyle.e64 Files\SetupLdr.e32 Files\SetupLdr.e64
if errorlevel 1 goto failed
echo ISSigTool sign done

:setup
echo - Setup.exe
if exist .\setup-sign.bat (
  call .\setup-sign.bat /Dx64
) else (
  files\iscc setup.iss /Dx64
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