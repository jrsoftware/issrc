@echo off

cd /d %~dp0

echo - Synching files from Files to Projects\Bin

copy ..\..\Files\Default.isl
copy ..\..\Files\ISPPBuiltins.iss
copy ..\..\Files\is7z*.dll
copy ..\..\Files\is7z*.dll.issig
copy ..\..\Files\is*zip.dll
copy ..\..\Files\isbzip.dll.issig
copy ..\..\Files\is*zlib.dll
copy ..\..\Files\iszlib.dll.issig
copy ..\..\Files\isscint.dll
copy ..\..\Files\isscint.dll.issig
copy ..\..\Files\islzma.dll
copy ..\..\Files\islzma.dll.issig
copy ..\..\Files\islzma*.exe
copy ..\..\Files\ISetup.chm
copy ..\..\Files\ISetup-dark.chm
copy ..\..\Files\WizClassicSmallImage.bmp
copy ..\..\whatsnew.htm

del *.exp
del *.lib
del *.pdb
del *.bsc

echo - Synching files done

if "%1"=="nopause" goto :eof

pause