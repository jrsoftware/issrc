# Verifies the x86 and x64 installers in Output for DLL sideloading. Run it
# elevated from the repository root, or from a folder with the same build.bat
# and Output, like build-verify.bat does. Procmon uses its saved settings from
# HKCU: "Drop Filtered Events" must be off there.

set -exo pipefail
fltmc >/dev/null 2>&1 || { echo "Must be run as administrator" >&2; exit 1; }

ver="$(sed -n 's/^set VER=//p' <build.bat)"

prepare_installer() {
  local arch=$1
  local winsxs_arch=x86
  if test $arch = x64
  then
    winsxs_arch=amd64
  fi

  mkdir -p Output/innosetup-$ver-$arch.exe.Local
  cp -R "$(cygpath -au "$SYSTEMROOT")"/WinSxS/${winsxs_arch}_microsoft.windows.common-controls_* Output/innosetup-$ver-$arch.exe.Local/
  mkdir -p Output/innosetup-$ver-$arch.en-US
  mkdir -p Output/innosetup-$ver-$arch.en
  mkdir -p Output/innosetup-$ver-$arch.ENU
  mkdir -p trace/$arch
}

verify_installer() {
  local arch=$1
  local previous_arch=$2
  # Need to start the background process via PowerShell because it would
  # block for some reason if started as a Bash background process.
  powershell -command 'start-process -NoNewWindow -FilePath .\Procmon.exe -ArgumentList "-AcceptEula -Quiet -BackingFile trace/'$arch'/procmon.pml -RunTime 60"'
  ./Procmon.exe -AcceptEula -WaitForIdle
  ./Output/innosetup-$ver-$arch.exe //verysilent //suppressmsgboxes //dir=InnoSetup-$arch //noicons \
    //tasks= //portable=1 //currentuser
  test -x InnoSetup-$arch/ISCC.exe
  ./Procmon.exe -Terminate -Quiet
  powershell -command 'start-process -NoNewWindow -Wait -FilePath .\Procmon.exe -ArgumentList "-OpenLog trace\'$arch'\procmon.pml -SaveAs trace\'$arch'\procmon.csv"'

  local curdir
  curdir="$(cygpath -aw Output | sed 's/\\/&&/g')"
  local escaped_ver
  escaped_ver="$(sed 's/\./\\&/g' <<<"$ver")"
  local ignore_previous_installer=()
  if test -n "$previous_arch"
  then
    # Other processes can still access the previous installer
    ignore_previous_installer=(-e '/"'$curdir'\\innosetup-'$escaped_ver'-'$previous_arch'\.exe"/d')
  fi
  sed -ne '/"'$curdir'\\innosetup-'$escaped_ver'-'$arch'\.\(exe\|exe\.Config\|en-US\|en\|ENU\|EN\|nl-NL\|nl\|NLD\|NL\)"/d' \
    -e '/"System","[0-9]*","\(CreateFileMapping\|FASTIO_[A-Z_]*\|WriteFile\|SetEndOfFileInformationFile\|IRP_MJ_CLOSE\)"/d' \
    "${ignore_previous_installer[@]}" -e '/"'$curdir'\\/p' \
    trace/$arch/procmon.csv >trace/$arch/filtered.csv
  if test -s trace/$arch/filtered.csv
  then
    echo ":error:Unexpected filesystem access during the $arch installer check" >&2
    cat trace/$arch/filtered.csv >&2
    exit 1
  fi
}

for arch in x86 x64
do
  prepare_installer $arch
done

# The download gives Windows time to write the comctl32.dll copies to disk
# before the first capture starts. The filter above also tries to ignore
# these writes.
curl -LO https://download.sysinternals.com/files/ProcessMonitor.zip
unzip -o ProcessMonitor.zip

previous_arch=
for arch in x86 x64
do
  verify_installer $arch $previous_arch
  previous_arch=$arch
done
