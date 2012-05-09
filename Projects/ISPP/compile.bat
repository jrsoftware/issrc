@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2009 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile all ISPP projects without Unicode support
rem
rem  $jrsoftware: ispp/compile.bat,v 1.6 2010/12/29 13:23:31 mlaan Exp $

setlocal

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo compilesettings.bat is missing or incomplete. It needs to be created
echo with the following lines, adjusted for your system:
echo.
echo   set DELPHI7ROOT=c:\delphi7             [Path to Delphi 7 (or later)]
goto exit

:compilesettingsfound
set DELPHI7ROOT=
call .\compilesettings.bat
if "%DELPHI7ROOT%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

rem  Compile each project separately because it seems Delphi
rem  carries some settings (e.g. $APPTYPE) between projects
rem  if multiple projects are specified on the command line.

echo - ISPPCC.dpr
"%DELPHI7ROOT%\bin\dcc32.exe" -Q -B -H -W %1 -U"%DELPHI7ROOT%\lib" -EFiles ISPPCC.dpr -DIS_ALLOWD7
if errorlevel 1 goto failed

echo - ISPP.dpr
"%DELPHI7ROOT%\bin\dcc32.exe" -Q -B -H -W %1 -U"%DELPHI7ROOT%\lib" -EFiles ISPP.dpr -DIS_ALLOWD7
if errorlevel 1 goto failed

echo - Renaming files
cd Files
if errorlevel 1 goto failed
rem move x y
rem if errorlevel 1 goto failed

echo - StripReloc'ing
stripreloc /b- ISPPCC.exe
if errorlevel 1 goto failed

echo Success!
cd ..
goto exit

:failed
echo *** FAILED ***
cd ..
exit /b 1

:exit
