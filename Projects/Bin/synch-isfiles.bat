@echo off

cd /d %~dp0

echo - Synching files from Files to Projects\Bin

copy ..\..\Files\is7z*.dll
copy ..\..\Files\is7z*.dll.issig
copy ..\..\Files\is*zip.dll
copy ..\..\Files\is*zip.dll.issig
copy ..\..\Files\is*zlib.dll
copy ..\..\Files\is*zlib.dll.issig
copy ..\..\Files\isscint.dll
copy ..\..\Files\isscint.dll.issig
copy ..\..\Files\islzma.dll
copy ..\..\Files\islzma.dll.issig
copy ..\..\Files\islzma*.exe
copy ..\..\Files\islzma*.exe.issig
copy ..\..\Files\WizClassicSmallImage.bmp

if exist Default.isl (del Default.isl)
if exist ISPPBuiltins.iss (del ISPPBuiltins.iss)
if exist whatsnew.htm (del whatsnew.htm)
if exist ISetup.chm (del ISetup.chm)
if exist ISetup-dark.chm (del ISetup-dark.chm)

if exist *.exp (del *.exp)
if exist *.lib (del *.lib)
if exist *.pdb (del *.pdb)
if exist *.bsc (del *.bsc)

echo - Synching files done

if "%1"=="nopause" goto :eof

pause