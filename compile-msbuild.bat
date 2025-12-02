@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2025 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile Inno Setup using MSBuild

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

rem -------------------------------------------------------------------------

call "%DELPHIXEROOT%\bin\rsvars.bat"
if errorlevel 1 goto failed

cd Projects
if errorlevel 1 goto failed

if "%1"=="issigtool" (
  echo - ISSigTool.exe
  msbuild.exe ISSigTool.dproj /t:Build /p:Config=Release;Platform=Win32;EnvOptionsWarn=false /nologo
) else (
  echo - Projects.groupproj
  rem This emits warning MSB4056, but that's ok since the build doesn't use COM
  rem Modern MSBuild supports /noWarn:MSB4056, but the version targeted by Delphi 12.3's rsvars.bat does not
  msbuild.exe Projects.groupproj /t:Build /p:BuildGroup=Release;EnvOptionsWarn=false /nologo
)
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
