@echo off

cd /d %~dp0

echo - Synching files from Files to Projects\Bin

copy ..\..\Files\Default.isl
copy ..\..\Files\ISPPBuiltins.iss
copy ..\..\Files\is*zip.dll
copy ..\..\Files\is*zlib.dll
copy ..\..\Files\isscint.dll
copy ..\..\Files\islzma.dll
copy ..\..\Files\islzma*.exe
copy ..\..\Files\ISetup.chm
copy ..\..\Files\ISetup-dark.chm
copy ..\..\Files\WizClassicSmallImage.bmp
copy ..\..\whatsnew.htm

echo - Synching files done

if "%1"=="nopause" goto :eof

pause