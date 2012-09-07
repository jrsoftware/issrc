@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2012 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to zip the source code from Github
rem
rem  First parameter should be the version tag, second parameter the version number
rem
rem  Example usage: src.bat 5_4_3 5.4.3

cd output

if "%1%"=="" goto failed
if "%2%"=="" goto failed

del issrc-%2%.zip
if errorlevel 1 goto failed
rd /s /q issrc
if errorlevel 1 goto failed
rem note: github doesn't allow git archive at this moment
call git clone git://github.com/jrsoftware/issrc.git issrc
if errorlevel 1 goto failed
cd issrc
if errorlevel 1 goto failed
call git checkout is-%1%
if errorlevel 1 goto failed
zip -9RDX "..\issrc-%2%.zip" *
if errorlevel 1 goto failed
cd ..
if errorlevel 1 goto failed
rd /s /q issrc
if errorlevel 1 goto failed

echo Success!
cd ..
pause
exit /b 0

:failed
echo *** FAILED ***
cd ..
pause
exit /b 1