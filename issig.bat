@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2025 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to embed the public key in TrustFunc.AllowedPublicKeys.inc (before compilation) or to create ISCmplr.dll.issig and ISPP.dll.issig (after compilation)

setlocal

cd /d %~dp0

if not "%ISSIGTOOL_KEY_FILE%"=="" goto keyfilefound
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

if "%1"=="embed" goto embed
if "%1"=="sign" goto sign
if not "%1"=="" goto failed

:embed
cd Files
if errorlevel 1 goto failed
set targetfile=..\Components\TrustFunc.AllowedPublicKeys.inc
if not exist "%targetfile%" goto failed
set publickeyfile=_temp.ispublickey
ISSigTool export-public-key "%publickeyfile%"
if errorlevel 1 goto failed
if not exist "%publickeyfile%" goto failed
powershell -NoProfile -Command "& { $filePath = '%targetfile%'; $replacementFilePath = '%publickeyfile%'; $startMarker = \"AllowedPublicKey2Text := '''\"; $endMarker = \"''';//end\"; try { $content = Get-Content -Raw -Path $filePath; $replacementText = Get-Content -Raw -Path $replacementFilePath; [string] $pattern = '(?s)' + [regex]::Escape($startMarker) + '.*?' + [regex]::Escape($endMarker); if ($content -match $pattern) { $replacement = $startMarker + \"`r`n\" + $replacementText + \"`r`n\" + $endMarker; $newContent = $content -replace $pattern, $replacement; $utf8NoBomEncoding = New-Object System.Text.UTF8Encoding($false); [System.IO.File]::WriteAllText($filePath, $newContent, $utf8NoBomEncoding); Write-Host 'Embedded key.'; } else { Write-Host 'Markers not found.'; exit 1; } } catch { Write-Error ('Error: ' + $_.Exception.Message); exit 1; } }"
if errorlevel 1 goto failed
del "%publickeyfile%"
if errorlevel 1 goto failed
cd ..
if errorlevel 1 goto failed

echo Success!
goto exit

:sign
cd Files
if errorlevel 1 goto failed
ISSigTool sign ISCmplr.dll ISPP.dll
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
