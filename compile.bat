@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2025 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile Inno Setup

setlocal

cd /d %~dp0

if "%1"=="x86" goto archfound
if "%1"=="x64" goto archfound
echo Architecture parameter is missing or invalid. Must be "x86" or "x64".
goto failed2
:archfound

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

rem -------------------------------------------------------------------------

call "%DELPHIXEROOT%\bin\rsvars.bat"
if errorlevel 1 goto failed

cd Projects
if errorlevel 1 goto failed

set EnvOptionsWarn=false

if "%1"=="x64" ( set Bits=64 ) else ( set Bits=32 )

if /I "%2"=="ishelpgen" (
  echo - ISHelpGen.exe
  msbuild.exe ..\ISHelp\ISHelpGen\ISHelpGen.dproj /t:Build /p:Config=Release;Platform=Win64 /nologo
) else if /I "%2"=="issigtool" (
  echo - ISSigTool.exe
  msbuild.exe ISSigTool.dproj /t:Build /p:Config=Release;Platform=Win%Bits% /nologo
) else (
  echo - Projects.groupproj - Release build group
  rem This emits warning MSB4056, but that's ok since the build doesn't use COM. Modern MSBuild supports
  rem /noWarn:MSB4056, but the version targeted by Delphi 12.3's rsvars.bat does not. Additionally Delphi's
  rem implementation of build groups does not seem to pass through additional parameters, so even with a
  rem modern MSBuild you cannot suppress the warning. Likewise, using /nologo or /v:q has no effect.
  msbuild.exe Projects.groupproj /t:Build /p:BuildGroup=Release%Bits%
)
if errorlevel 1 goto failed

cd ..
if errorlevel 1 goto failed

echo Success!

if not "%2"=="" goto exit
rem  Sign using user's private key - will be overwritten if called by build.bat
call .\issig.bat sign Files\ISCmplr.dll Files\ISPP.dll Files\Setup.e32 Files\Setup.e64 Files\SetupCustomStyle.e32 Files\SetupCustomStyle.e64 Files\SetupLdr.e32 Files\SetupLdr.e64
if errorlevel 1 goto failed
echo ISSigTool sign done

goto exit

:failed
echo *** FAILED ***
cd ..
:failed2
exit /b 1

:exit
