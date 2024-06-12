cd /d %~dp0

echo - Synching files from ISHelp to Projects\ISPP\Help

copy ..\..\..\ISHelp\isetup.dtd
copy ..\..\..\ISHelp\Staging\contents.css Staging
copy ..\..\..\ISHelp\Staging\contents.js Staging
copy ..\..\..\ISHelp\Staging\styles.css Staging
copy ..\..\..\ISHelp\Staging\topic.js Staging
copy ..\..\..\ISHelp\Staging\images\contentsheadclosed.png Staging\images
copy ..\..\..\ISHelp\Staging\images\contentsheadopen.png Staging\images
copy ..\..\..\ISHelp\Staging\images\contentstopic.png Staging\images
copy ..\..\..\ISHelp\Staging\images\extlink.png Staging\images

echo - Synching files done

if "%1"=="nopause" goto :eof

pause