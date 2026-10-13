# Verifies the x86 installer in Output for DLL sideloading. Run it elevated from
# the repository root, or from a folder with the same build.bat and Output,
# like build-verify.bat does. Procmon uses its saved settings from HKCU:
# "Drop Filtered Events" must be off there.

set -exo pipefail
fltmc >/dev/null 2>&1 || { echo "Must be run as administrator" >&2; exit 1; }

mt=$(ls -t /c/Program\ Files*/Windows\ Kits/10/bin/*/x64/mt.exe)
test -n "$mt"
PATH="${mt%%/mt.exe*}:$PATH"

ver="$(sed -n 's/^set VER=//p' <build.bat)"
mt '-inputresource:Output\innosetup-'$ver-x86.exe -out:innosetup-$ver-x86.manifest
cat innosetup-$ver-x86.manifest
mkdir -p Output/innosetup-$ver-x86.exe.Local
cp -R "$(cygpath -au "$SYSTEMROOT")"/WinSxS/x86_microsoft.windows.common-controls_* Output/innosetup-$ver-x86.exe.Local/
mkdir -p Output/innosetup-$ver-x86.en-US
mkdir -p Output/innosetup-$ver-x86.en
mkdir -p Output/innosetup-$ver-x86.ENU
mkdir -p trace
curl -LO https://download.sysinternals.com/files/ProcessMonitor.zip
unzip -o ProcessMonitor.zip
# Need to start the background process via PowerShell because it would
# block for some reason if started as a Bash background process.
powershell -command 'start-process -NoNewWindow -FilePath .\Procmon.exe -ArgumentList "-AcceptEula -Quiet -BackingFile trace/procmon.pml -RunTime 60"'
test $? = 0
ps -W
./Procmon.exe -AcceptEula -WaitForIdle
./Output/innosetup-$ver-x86.exe //verysilent //suppressmsgboxes //dir=InnoSetup //noicons \
  //tasks= //portable=1 //currentuser
test -x InnoSetup/ISCC.exe
./Procmon.exe -Terminate -Quiet
powershell -command 'start-process -NoNewWindow -Wait -FilePath .\Procmon.exe -ArgumentList "-OpenLog trace\procmon.pml -SaveAs trace\procmon.csv"'

curdir="$(cygpath -aw Output | sed 's/\\/&&/g')"
ver="$(sed 's/\./\\&/g' <<<"$ver")"
sed -ne '/"'$curdir'\\innosetup-'$ver'-x86\.\(exe\|exe\.Config\|en-US\|en\|ENU\|EN\)"/d' \
  -e '/"System","[0-9]*","\(CreateFileMapping\|FASTIO_[A-Z_]*\)"/d' -e '/"'$curdir'\\/p' \
  trace/procmon.csv >trace/filtered.csv
if test -s trace/filtered.csv
then
  echo ":error:Unexpected filesystem access" >&2
  cat trace/filtered.csv >&2
  exit 1
fi
