@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2025 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile Inno Setup

setlocal

cd /d %~dp0

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo compilesettings.bat is missing or incomplete. It needs to contain
echo the following line, adjusted for your system:
echo.
echo   set DELPHIXEROOT=C:\Program Files\Embarcadero\RAD Studio\20.0 [Path to Delphi 10.4 Sydney (or later)]
goto failed2

:compilesettingsfound
set DELPHIXEROOT=
call .\compilesettings.bat
if "%DELPHIXEROOT%"=="" goto compilesettingserror

set DELPHIXELIB_WIN32=%DELPHIXEROOT%\lib\win32\release
set DELPHIXELIB_WIN64=%DELPHIXEROOT%\lib\win64\release

rem -------------------------------------------------------------------------

rem  Compile each project separately because it seems Delphi
rem  carries some settings (e.g. $APPTYPE) between projects
rem  if multiple projects are specified on the command line.

set DELPHIXEDISABLEDWARNINGS=-W-SYMBOL_DEPRECATED -W-SYMBOL_PLATFORM -W-UNSAFE_CAST -W-EXPLICIT_STRING_CAST -W-EXPLICIT_STRING_CAST_LOSS

set FLAGS=--no-config -Q -B -$L- -$C- -H -W -$T+ %DELPHIXEDISABLEDWARNINGS% %1 -E..\Files
set FLAGSCONSOLE=%FLAGS% -CC
set FLAGSE32=%FLAGS% -TX.e32
set FLAGSE64=%FLAGS% -TX.e64
set NAMESPACES=System;System.Win;Winapi
set DCUDIR_WIN32=Dcu\Win32\Release
set DCUDIR_WIN64=Dcu\Win64\Release

set ROPSSRC=..\Components\UniPS\Source
set ROPSDEF=PS_MINIVCL;PS_NOGRAPHCONST;PS_PANSICHAR;PS_NOINTERFACEGUIDBRACKETS

call "%DELPHIXEROOT%\bin\rsvars.bat"
if errorlevel 1 goto failed

cd Projects
if errorlevel 1 goto failed

if "%1"=="issigtool" goto issigtool
if not "%1"=="" goto failed

echo - ISPP.dll
mkdir %DCUDIR_WIN32%\ISPP.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGSCONSOLE% -NS%NAMESPACES%  -U"%DELPHIXELIB_WIN32%"  -NU%DCUDIR_WIN32%\ISPP.dpr ISPP.dpr
if errorlevel 1 goto failed

echo - Compil32.exe
msbuild.exe Compil32.dproj /t:BuildVersionResource /p:Config=Release;Platform=Win32 /nologo /v:q
if errorlevel 1 goto failed
mkdir %DCUDIR_WIN32%\Compil32.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGS% -W-IMPLICIT_INTEGER_CAST_LOSS -W-IMPLICIT_CONVERSION_LOSS -NS%NAMESPACES%;Vcl;Vcl.Imaging -U"%DELPHIXELIB_WIN32%;%ROPSSRC%" -NU%DCUDIR_WIN32%\Compil32.dpr -DCOMPIL32PROJ;VCLSTYLES;%ROPSDEF% Compil32.dpr
if errorlevel 1 goto failed

echo - ISCC.exe
mkdir %DCUDIR_WIN32%\ISCC.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGS% -NS%NAMESPACES% -U"%DELPHIXELIB_WIN32%;%ROPSSRC%" -NU%DCUDIR_WIN32%\ISCC.dpr -D%ROPSDEF% ISCC.dpr
if errorlevel 1 goto failed

echo - ISCmplr.dll
mkdir %DCUDIR_WIN32%\ISCmplr.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGS% -W-IMPLICIT_INTEGER_CAST_LOSS -W-IMPLICIT_CONVERSION_LOSS -NS%NAMESPACES% -U"%DELPHIXELIB_WIN32%;%ROPSSRC%" -NU%DCUDIR_WIN32%\ISCmplr.dpr -D%ROPSDEF% ISCmplr.dpr
if errorlevel 1 goto failed

echo - SetupLdr.e32
mkdir %DCUDIR_WIN32%\SetupLdr.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGSE32% -NS%NAMESPACES% -U"%DELPHIXELIB_WIN32%" -NU%DCUDIR_WIN32%\SetupLdr.dpr -DSETUPLDRPROJ SetupLdr.dpr
if errorlevel 1 goto failed

echo - SetupLdr.e64
mkdir %DCUDIR_WIN64%\SetupLdr.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc64.exe" %FLAGSE64% -NS%NAMESPACES% -U"%DELPHIXELIB_WIN64%" -NU%DCUDIR_WIN64%\SetupLdr.dpr -DSETUPLDRPROJ SetupLdr.dpr
if errorlevel 1 goto failed

echo - Setup.e32
mkdir %DCUDIR_WIN32%\Setup.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGSE32% -W-IMPLICIT_INTEGER_CAST_LOSS -W-IMPLICIT_CONVERSION_LOSS -NS%NAMESPACES%;Vcl -U"%DELPHIXELIB_WIN32%;%ROPSSRC%" -NU%DCUDIR_WIN32%\Setup.dpr -DSETUPPROJ;%ROPSDEF% Setup.dpr
if errorlevel 1 goto failed

echo - SetupCustomStyle.e32
msbuild.exe SetupCustomStyle.dproj /t:BuildVersionResource /p:Config=Release;Platform=Win32 /nologo /v:q
if errorlevel 1 goto failed
mkdir %DCUDIR_WIN32%\SetupCustomStyle.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGSE32% -W-IMPLICIT_INTEGER_CAST_LOSS -W-IMPLICIT_CONVERSION_LOSS -NS%NAMESPACES%;Vcl -U"%DELPHIXELIB_WIN32%;%ROPSSRC%" -NU%DCUDIR_WIN32%\SetupCustomStyle.dpr -DSETUPPROJ;VCLSTYLES;%ROPSDEF% SetupCustomStyle.dpr
if errorlevel 1 goto failed

:issigtool
echo - ISSigTool.exe
mkdir %DCUDIR_WIN32%\ISSigTool.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGSCONSOLE% -NS%NAMESPACES% -U"%DELPHIXELIB_WIN32%" -NU%DCUDIR_WIN32%\ISSigTool.dpr ISSigTool.dpr
if errorlevel 1 goto failed

cd ..
if errorlevel 1 goto failed

echo Success!

if "%1"=="issigtool" goto exit
rem  Sign using user's private key - will be overwritten if called by build.bat
call .\issig.bat sign Files\ISCmplr.dll Files\ISPP.dll Files\Setup.e32 Files\SetupCustomStyle.e32 Files\SetupLdr.e32 Files\SetupLdr.e64
if errorlevel 1 goto failed
echo ISSigTool sign done

goto exit

:failed
echo *** FAILED ***
cd ..
:failed2
exit /b 1

:exit
