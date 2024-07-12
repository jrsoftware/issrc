@echo off

cd /d %~dp0

echo - Synching files from Staging to Staging-dark

copy Staging\contents.js Staging-dark
copy Staging\contents-template.htm Staging-dark
copy Staging\index.htm Staging-dark
copy Staging\index.php Staging-dark
copy Staging\stoplist.stp Staging-dark
copy Staging\topic.js Staging-dark

echo - Synching files done

if "%1"=="nopause" goto :eof

pause