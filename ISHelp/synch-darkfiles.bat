@echo off

cd /d %~dp0

echo - Synching files from Staging to Staging-dark

copy Staging\hh_project.hhp Staging-dark
copy Staging\stoplist.stp Staging-dark
copy Staging\topic.js Staging-dark

echo - Synching files done

if "%1"=="nopause" goto :eof

pause