@echo off

rem  Inno Setup
rem  Copyright (C) 1997-2025 Jordan Russell
rem  Portions by Martijn Laan
rem  For conditions of distribution and use, see LICENSE.TXT.
rem
rem  Batch file to embed the user's public key from compilesettings.bat in
rem  TrustFunc.AllowedPublicKeys.inc and setup.allowedpublickeys.iss (before
rem  compilation) or to sign files using it (after compilation)
rem
rem  If the key is missing it will be generated
rem
rem  Also used by build(-ce).bat to verify some precompiled files

setlocal

cd /d %~dp0

if exist compilesettings.bat goto compilesettingsfound
:compilesettingserror
echo compilesettings.bat is missing or incomplete. It needs to contain
echo the following line, adjusted for your system:
echo.
echo   set ISSIGTOOL_KEY_FILE=x:\path\MyKey.isprivatekey
goto failed2

:compilesettingsfound
set ISSIGTOOL_KEY_FILE=
call .\compilesettings.bat
if "%ISSIGTOOL_KEY_FILE%"=="" goto compilesettingserror

rem -------------------------------------------------------------------------

if not exist "%ISSIGTOOL_KEY_FILE%" (
  echo Missing key file
  Files\ISSigTool.exe generate-private-key
  if errorlevel 1 goto failed
  if not exist "%ISSIGTOOL_KEY_FILE%" goto failed
  echo Generating key file done - do not share with others!
)

if "%1"=="embed" goto embed
if "%1"=="sign" goto signorverify
if "%1"=="verify" goto signorverify
if not "%1"=="" goto failed

:embed
set publickeyfile=_temp.ispublickey
Files\ISSigTool.exe export-public-key "%publickeyfile%"
if errorlevel 1 goto failed
if not exist "%publickeyfile%" goto failed
set targetfile=Components\TrustFunc.AllowedPublicKeys.inc
if not exist "%targetfile%" goto failed
powershell.exe -NoProfile -Command "$filePath = '%targetfile%'; $replacementFilePath = '%publickeyfile%'; $startMarker = 'AllowedPublicKey2Text :='; $endMarker = ';'; try { $content = Get-Content -Raw -Path $filePath; $replacementText = Get-Content -Raw -Path $replacementFilePath; $replacementText = $replacementText -replace \"`r`n\", \"' + #13#10 +`r`n'\"; $replacementText = \"'\" + $replacementText + \"'\"; $replacementText = $replacementText -replace \" \+`r`n''\", \"\"; [string] $pattern = '(?s)' + [regex]::Escape($startMarker) + '.*?' + [regex]::Escape($endMarker); if ($content -match $pattern) { $replacement = $startMarker + \"`r`n\" + $replacementText  + $endMarker; $newContent = $content -replace $pattern, $replacement; $utf8NoBomEncoding = New-Object System.Text.UTF8Encoding($false); [System.IO.File]::WriteAllText($filePath, $newContent, $utf8NoBomEncoding); Write-Host \"Embedded public key in $filePath.\"; } else { Write-Host \"Pattern not found in $filePath.\"; exit 1; } } catch { Write-Error (\"Error: $_.Exception.Message\"); exit 1; }"
if errorlevel 1 goto failed
set targetfile=setup.allowedpublickeys.iss
if not exist "%targetfile%" goto failed
powershell.exe -NoProfile -Command "$filePath = '%targetfile%'; $replacementFilePath = '%publickeyfile%'; $startMarker = 'Name: mykey2; '; try { $content = Get-Content -Raw -Path $filePath; $replacementText = Get-Content -Raw -Path $replacementFilePath; $replacementText = $replacementText -replace \"`r`n\", \"; \"; $replacementText = $replacementText.Substring(0, $replacementText.Length - 2); $replacementText = $replacementText -replace 'format issig-public-key; key-id', 'KeyID:'; $replacementText = $replacementText -replace 'public-x', 'PublicX:'; $replacementText = $replacementText -replace 'public-y', 'PublicY:'; [string] $pattern = [regex]::Escape($startMarker) + '.*?$'; if ($content -match $pattern) { $replacement = $startMarker + $replacementText; $newContent = $content -replace $pattern, $replacement; $utf8NoBomEncoding = New-Object System.Text.UTF8Encoding($false); [System.IO.File]::WriteAllText($filePath, $newContent, $utf8NoBomEncoding); Write-Host \"Embedded public key in $filePath.\"; } else { Write-Host \"Pattern not found in $filePath.\"; exit 1; } } catch { Write-Error (\"Error: $_.Exception.Message\"); exit 1; }"
if errorlevel 1 goto failed
del "%publickeyfile%"
if errorlevel 1 goto failed

echo Success!
goto exit

:signorverify
Files\ISSigTool.exe %*
if errorlevel 1 goto failed

echo Success!
goto exit

:failed
echo *** FAILED ***
:failed2
exit /b 1

:exit
