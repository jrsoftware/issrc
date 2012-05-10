@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2012 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to compile all projects without Unicode support

setlocal

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo compilesettings.bat is missing or incomplete. It needs to be created
echo with the following lines, adjusted for your system:
echo.
echo   set DELPHIROOT=c:\delphi2              [Path to Delphi 2 (or later)]
echo   set DELPHI3ROOT=c:\delphi3             [Path to Delphi 3 (or later)]
echo   set DELPHI7ROOT=c:\delphi7             [Path to Delphi 7 (or later)]
echo   set ROPSPATH=c:\rops\source            [Path to ROPS]
goto failed2

:compilesettingsfound
set DELPHIROOT=
set DELPHI3ROOT=
set DELPHI7ROOT=
set ROPSPATH=
call .\compilesettings.bat
if "%DELPHIROOT%"=="" goto compilesettingserror
if "%DELPHI3ROOT%"=="" goto compilesettingserror
if "%DELPHI7ROOT%"=="" goto compilesettingserror
if "%ROPSPATH%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

rem  Compile each project separately because it seems Delphi
rem  carries some settings (e.g. $APPTYPE) between projects
rem  if multiple projects are specified on the command line.

cd Projects
if errorlevel 1 goto exit

cd ISPP
if errorlevel 1 goto failed

echo - ISPPCC.dpr
"%DELPHI7ROOT%\bin\dcc32.exe" -Q -B -H -W %1 -U"%DELPHI7ROOT%\lib" -E..\..\Files ISPPCC.dpr -DIS_ALLOWD7
if errorlevel 1 goto failed

echo - ISPP.dpr
"%DELPHI7ROOT%\bin\dcc32.exe" -Q -B -H -W %1 -U"%DELPHI7ROOT%\lib" -E..\..\Files ISPP.dpr -DIS_ALLOWD7
if errorlevel 1 goto failed

cd ..

echo - Compil32.dpr
"%DELPHI3ROOT%\bin\dcc32.exe" -Q -B -H -W %1 -U"%DELPHI3ROOT%\lib;..\Components;%ROPSPATH%" -E..\Files -DPS_MINIVCL;PS_NOWIDESTRING;PS_NOINT64;PS_NOGRAPHCONST Compil32.dpr
if errorlevel 1 goto failed

echo - ISCC.dpr
"%DELPHIROOT%\bin\dcc32.exe" -Q -B -H -W %1 -U"%DELPHIROOT%\lib;..\Components;%ROPSPATH%" -E..\Files -DPS_MINIVCL;PS_NOWIDESTRING;PS_NOINT64;PS_NOGRAPHCONST ISCC.dpr
if errorlevel 1 goto failed

echo - ISCmplr.dpr
"%DELPHIROOT%\bin\dcc32.exe" -Q -B -H -W %1 -U"%DELPHIROOT%\lib;..\Components;%ROPSPATH%" -E..\Files -DPS_MINIVCL;PS_NOWIDESTRING;PS_NOINT64;PS_NOGRAPHCONST ISCmplr.dpr
if errorlevel 1 goto failed

echo - SetupLdr.dpr
"%DELPHIROOT%\bin\dcc32.exe" -Q -B -H -W %1 -U"%DELPHIROOT%\lib;..\Components" -E..\Files SetupLdr.dpr
if errorlevel 1 goto failed

echo - Setup.dpr
"%DELPHIROOT%\bin\dcc32.exe" -Q -B -H -W %1 -U"%DELPHIROOT%\lib;..\Components;%ROPSPATH%" -E..\Files -DPS_MINIVCL;PS_NOWIDESTRING;PS_NOINT64;PS_NOGRAPHCONST Setup.dpr
if errorlevel 1 goto failed

echo - Renaming files
cd ..\Files
if errorlevel 1 goto failed
move SetupLdr.exe SetupLdr.e32
if errorlevel 1 goto failed
move Setup.exe Setup.e32
if errorlevel 1 goto failed

echo - StripReloc'ing
stripreloc /b- Compil32.exe ISCC.exe SetupLdr.e32 Setup.e32 ISPPCC.exe
if errorlevel 1 goto failed

echo Success!
cd ..
goto exit

:failed
echo *** FAILED ***
cd ..
:failed2
exit /b 1

:exit
