@echo off

cd /d %~dp0

echo - Synching files from ISHelp to ISPPHelp

copy ..\ISHelp\isetup.dtd
call :synch_files
call :synch_files -dark

echo - Synching files done

if "%1"=="nopause" goto :eof

pause

exit /b 0

:synch_files

copy ..\ISHelp\Staging%1\contents.css Staging%1
copy ..\ISHelp\Staging%1\contents.js Staging%1
copy ..\ISHelp\Staging%1\styles.css Staging%1
copy ..\ISHelp\Staging%1\topic.js Staging%1
copy ..\ISHelp\Staging%1\images\contentsheadclosed.png Staging%1\images
copy ..\ISHelp\Staging%1\images\contentsheadopen.png Staging%1\images
copy ..\ISHelp\Staging%1\images\contentstopic.png Staging%1\images
copy ..\ISHelp\Staging%1\images\extlink.png Staging%1\images

exit /b 0