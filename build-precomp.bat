@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2026 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to build Inno Setup's precompiled binaries,
rem  except the MyDll and MyProg example files.
rem 
rem  Once done the output can be found in Files. Output is
rem  also synched to Projects\Bin by synch-isfiles.bat.
rem
rem  Does NOT update .issig files.

setlocal

cd /d %~dp0

if exist buildsettings.bat goto buildsettingsfound
:buildsettingserror
echo buildsettings.bat is missing or incomplete. It needs to be created
echo with the following lines, adjusted for your system:
echo.
echo   set IS7ZROOT=C:\Is7z
echo   set ISCOMPRESSROOT=C:\Iscompress
echo   set ISSCINTROOT=C:\Isscint
goto failed2

:buildsettingsfound
set IS7ZROOT=
set ISCOMPRESSROOT=
set ISSCINTROOT=
call .\buildsettings.bat
if "%IS7ZROOT%"=="" goto buildsettingserror
if "%ISCOMPRESSROOT%"=="" goto buildsettingserror
if "%ISSCINTROOT%"=="" goto buildsettingserror
  
call :build x86
if errorlevel 1 goto failed
call :build x64
if errorlevel 1 goto failed
call :compile-islzma arm64
if errorlevel 1 goto failed
call :copy-islzma
if errorlevel 1 goto failed
call "%IS7ZROOT%\build.bat" nosynch
if errorlevel 1 goto failed
call "%ISCOMPRESSROOT%\build.bat" nosynch
if errorlevel 1 goto failed
call "%ISSCINTROOT%\build.bat" nosynch
if errorlevel 1 goto failed
call Projects\Bin\synch-isfiles.bat nopause
if errorlevel 1 goto failed

echo All done!
pause
exit /b 0

:failed
echo *** FAILED ***
:failed2
pause
exit /b 1

:build
call :build-LzmaDecodeInno %~1
if errorlevel 1 exit /b 1
call :build-ISLzmaDec %~1
if errorlevel 1 exit /b 1
call :build-IS7zDec %~1
if errorlevel 1 exit /b 1
call :compile-islzma %~1
if errorlevel 1 exit /b 1
exit /b

:build-LzmaDecodeInno
echo Compiling LzmaDecodeInno-%~1.obj
set R=Projects\Src\Compression.LZMA1SmallDecompressor\LzmaDecode
call %R%\compile.bat %~1
if errorlevel 1 exit /b 1
copy %R%\LzmaDecodeInno-%~1.obj Files
if errorlevel 1 exit /b 1
exit /b

:build-ISLzmaDec
echo Compiling and copying ISLzmaDec-%~1.obj
set R=Projects\Src\Compression.LZMADecompressor\Lzma2Decode
call %R%\compile.bat %~1
if errorlevel 1 exit /b 1
copy %R%\ISLzmaDec-%~1.obj Files
if errorlevel 1 exit /b 1
exit /b

:build-IS7zDec
echo Compiling and copying IS7zDec-%~1.obj
set R=Projects\Src\Compression.SevenZipDecoder\7zDecode
call %R%\compile.bat %~1
if errorlevel 1 exit /b 1
copy %R%\IS7zDec-%~1.obj Files
if errorlevel 1 exit /b 1
exit /b

:compile-islzma
echo Compiling islzma %~1
set R=Projects\Src\Compression.LZMACompressor\islzma
call %R%\compile.bat %~1
if errorlevel 1 exit /b 1
exit /b

:copy-islzma
echo Copying islzma files
set R=Projects\Src\Compression.LZMACompressor\islzma
copy %R%\Win32\Release\islzma.dll Files
if errorlevel 1 exit /b 1
copy %R%\Win32\Release\islzma32.exe Files
if errorlevel 1 exit /b 1
copy %R%\x64\Release\islzma-x64.dll Files
if errorlevel 1 exit /b 1
copy %R%\x64\Release\islzma64.exe Files
if errorlevel 1 exit /b 1
copy %R%\Arm64EC\Release\islzma-ARM64EC.dll Files
if errorlevel 1 exit /b 1
exit /b