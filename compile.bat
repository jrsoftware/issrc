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

set DELPHIXELIB=%DELPHIXEROOT%\lib\win32\release

rem -------------------------------------------------------------------------

rem  Compile each project separately because it seems Delphi
rem  carries some settings (e.g. $APPTYPE) between projects
rem  if multiple projects are specified on the command line.

set DELPHIXEDISABLEDWARNINGS=-W-SYMBOL_DEPRECATED -W-SYMBOL_PLATFORM -W-UNSAFE_CAST -W-EXPLICIT_STRING_CAST -W-EXPLICIT_STRING_CAST_LOSS -W-IMPLICIT_INTEGER_CAST_LOSS -W-IMPLICIT_CONVERSION_LOSS

set FLAGS=--no-config -Q -B -$L- -$C- -H -W %DELPHIXEDISABLEDWARNINGS% %1 -E..\Files
set FLAGSCONSOLE=%FLAGS% -CC
set FLAGSE32=%FLAGS% -TX.e32
set NAMESPACES=System;System.Win;Winapi
set DCUDIR=Dcu\Release

set ROPSSRC=..\Components\UniPS\Source
set ROPSDEF=PS_MINIVCL;PS_NOGRAPHCONST;PS_PANSICHAR;PS_NOINTERFACEGUIDBRACKETS

cd Projects
if errorlevel 1 goto failed

if "%1"=="issigtool" goto issigtool
if not "%1"=="" goto failed

echo - ISPP.dpr
mkdir %DCUDIR%\ISPP.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGSCONSOLE% -NS%NAMESPACES%  -U"%DELPHIXELIB%"  -NU%DCUDIR%\ISPP.dpr ISPP.dpr
if errorlevel 1 goto failed

echo - Compil32.dpr
mkdir %DCUDIR%\Compil32.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGS% -NS%NAMESPACES%;Vcl;Vcl.Imaging -U"%DELPHIXELIB%;%ROPSSRC%" -NU%DCUDIR%\Compil32.dpr -DCOMPIL32PROJ;%ROPSDEF% Compil32.dpr
if errorlevel 1 goto failed

echo - ISCC.dpr
mkdir %DCUDIR%\ISCC.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGS% -NS%NAMESPACES% -U"%DELPHIXELIB%;%ROPSSRC%" -NU%DCUDIR%\ISCC.dpr -D%ROPSDEF% ISCC.dpr
if errorlevel 1 goto failed

echo - ISCmplr.dpr
mkdir %DCUDIR%\ISCmplr.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGS% -NS%NAMESPACES% -U"%DELPHIXELIB%;%ROPSSRC%" -NU%DCUDIR%\ISCmplr.dpr -D%ROPSDEF% ISCmplr.dpr
if errorlevel 1 goto failed

echo - SetupLdr.dpr
mkdir %DCUDIR%\SetupLdr.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGSE32% -NS%NAMESPACES% -U"%DELPHIXELIB%" -NU%DCUDIR%\SetupLdr.dpr -DSETUPLDRPROJ SetupLdr.dpr
if errorlevel 1 goto failed

echo - Setup.dpr
mkdir %DCUDIR%\Setup.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGSE32% -NS%NAMESPACES%;Vcl -U"%DELPHIXELIB%;%ROPSSRC%" -NU%DCUDIR%\Setup.dpr -DSETUPPROJ;%ROPSDEF% Setup.dpr
if errorlevel 1 goto failed

:issigtool
echo - ISSigTool.dpr
mkdir %DCUDIR%\ISSigTool.dpr 2>nul
"%DELPHIXEROOT%\bin\dcc32.exe" %FLAGSCONSOLE% -NS%NAMESPACES% -U"%DELPHIXELIB%" -NU%DCUDIR%\ISSigTool.dpr ISSigTool.dpr
if errorlevel 1 goto failed

cd ..
if errorlevel 1 goto failed

echo Success!
goto exit

:failed
echo *** FAILED ***
cd ..
:failed2
exit /b 1

:exit
