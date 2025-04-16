@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2024 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to create .issig files required by Inno Setup (and delete any unwanted ones)

setlocal

cd /d %~dp0

if not "%ISSIGTOOL_KEY_FILE%"=="" goto keyfilefound
:compilesettingserror
echo ISSIGTOOL_KEY_FILE is missing or incomplete. It needs to be created
echo with the following line, adjusted for your system:
echo.
echo   set ISSIGTOOL_KEY_FILE=x:\path\MyKey.isprivatekey
echo.
echo To create this file use the following command:
echo.
echo   Files\ISSigTool.exe --key-file=x:\path\MyKey.isprivatekey generate-private-key
echo.
echo Do not share the file with others!
goto failed2

:keyfilefound

rem -------------------------------------------------------------------------

cd Files
if errorlevel 1 goto failed
del *.issig
if errorlevel 1 goto failed
ISSigTool sign isbzip.dll ISCmplr.dll islzma.dll ISPP.dll isscint.dll iszlib.dll
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
