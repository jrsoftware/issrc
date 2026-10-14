# Verifies the x86 and x64 installers in Output for DLL sideloading. Run it
# elevated from the repository root, or from a folder with the same build.bat
# and Output, like build-verify.bat does. Procmon uses its saved settings from
# HKCU: "Drop Filtered Events" must be off there.

# Stop at the first failing command, also inside a pipeline, and log each
# command before it runs.
set -exo pipefail

# Stop unless running as administrator, which Procmon needs
fltmc >/dev/null 2>&1 || { echo "Must be run as administrator" >&2; exit 1; }

# Get the version from build.bat
ver="$(sed -n 's/^set VER=//p' <build.bat)"

prepare_installer() {
  local arch=$1
  local winsxs_arch=x86
  if test $arch = x64
  then
    winsxs_arch=amd64
  fi

  # cygpath is the Git Bash tool that converts between Windows paths and Git Bash paths
  mkdir -p Output/innosetup-$ver-$arch.exe.Local
  cp -R "$(cygpath -au "$SYSTEMROOT")"/WinSxS/${winsxs_arch}_microsoft.windows.common-controls_* Output/innosetup-$ver-$arch.exe.Local/
  mkdir -p Output/innosetup-$ver-$arch.en-US
  mkdir -p Output/innosetup-$ver-$arch.en
  mkdir -p Output/innosetup-$ver-$arch.ENU

  # Create the folder for the Procmon output
  mkdir -p trace/$arch
}

verify_installer() {
  local arch=$1
  local previous_arch=$2
  # Need to start the background process via PowerShell because it would
  # block for some reason if started as a Bash background process.
  powershell -command 'start-process -NoNewWindow -FilePath .\Procmon.exe -ArgumentList "-AcceptEula -Quiet -BackingFile trace/'$arch'/procmon.pml -RunTime 60"'
  # Wait until Procmon captures
  ./Procmon.exe -AcceptEula -WaitForIdle
  # Perform the installation
  ./Output/innosetup-$ver-$arch.exe //verysilent //suppressmsgboxes //dir=InnoSetup-$arch //noicons //tasks= //portable=1 //currentuser
  # Make sure the installer actually installed something
  test -x InnoSetup-$arch/ISCC.exe
  # Stop the capture
  ./Procmon.exe -Terminate -Quiet
  # Convert the trace to CSV for the filter
  powershell -command 'start-process -NoNewWindow -Wait -FilePath .\Procmon.exe -ArgumentList "-OpenLog trace\'$arch'\procmon.pml -SaveAs trace\'$arch'\procmon.csv"'
  # Make sure the conversion actually produced the CSV
  test -s trace/$arch/procmon.csv
  # Escape the version for use in the patterns
  local escaped_ver
  escaped_ver="$(sed 's/\./\\&/g' <<<"$ver")"

  # Verify SetupLdr

  # Escape the Output path for use in the patterns
  local escaped_output_path
  escaped_output_path="$(cygpath -aw Output | sed 's/\\/&&/g')"
  # Make sure the trace shows the installer opening itself, so the check can't
  # pass on a trace without the installer's events
  if ! grep -q '"innosetup-'$escaped_ver'-'$arch'\.exe","[0-9]*","CreateFile","'$escaped_output_path'\\innosetup-'$escaped_ver'-'$arch'\.exe"' trace/$arch/procmon.csv
  then
    echo "::error::The trace doesn't show the $arch installer opening itself"
    exit 1
  fi
  local ignore_previous_installer=()
  if test -n "$previous_arch"
  then
    # Other processes can still access the previous installer
    ignore_previous_installer=(-e '/"'$escaped_output_path'\\innosetup-'$escaped_ver'-'$previous_arch'\.exe"/d')
  fi
  # Copy all lines from the Procmon trace in procmon.csv to
  # filtered-setupldr.csv, except these:
  # 1. Access to the installer file itself
  # 2. Access to its .exe.Config file, which Windows looks for
  # 3. Access to its resource modules, which Delphi looks for
  # 4. Access to the previous installer, if any
  # 5. Select file operations by the System process, such as flushing our earlier copies
  # 6. Access outside Output
  # Exceptions 1 to 4 only apply when the complete path matches.
  sed -ne '/"'$escaped_output_path'\\innosetup-'$escaped_ver'-'$arch'\.\(exe\|exe\.Config\|en-US\|en\|ENU\|EN\|nl-NL\|nl\|NLD\|NL\)"/d' \
    "${ignore_previous_installer[@]}" \
    -e '/"System","[0-9]*","\(CreateFileMapping\|FASTIO_[A-Z_]*\|WriteFile\|SetEndOfFileInformationFile\|IRP_MJ_CLOSE\)"/d' \
    -e '/"'$escaped_output_path'\\/p' \
    trace/$arch/procmon.csv >trace/$arch/filtered-setupldr.csv
  # Fail if the filter left any lines, and show them
  if test -s trace/$arch/filtered-setupldr.csv
  then
    echo "::error::Unexpected filesystem access in SetupLdr's folder during the $arch installer check"
    cat trace/$arch/filtered-setupldr.csv
    exit 1
  fi

  # Verify Setup

  # Get Setup's folder from the event of Setup loading itself, and escape it
  # for use in the patterns. Unlike the event of SetupLdr starting Setup, this
  # event shows the folder in the same form as the file events: the long form,
  # also when TEMP is an 8.3 path. This script runs elevated, so Setup doesn't
  # respawn itself and the trace must have exactly one such event.
  local escaped_setup_path
  escaped_setup_path="$(sed -n 's/^.*"innosetup-'$escaped_ver'-'$arch'\.tmp","[0-9]*","Load Image","\([^"]*\)\\innosetup-'$escaped_ver'-'$arch'\.tmp",.*/\1/p' trace/$arch/procmon.csv | sed 's/\\/&&/g')"
  if test "$(grep -c . <<<"$escaped_setup_path")" != 1
  then
    echo "::error::The trace doesn't show the $arch Setup loading itself exactly once"
    exit 1
  fi
  # Copy all lines from the Procmon trace in procmon.csv to filtered-setup.csv,
  # except these:
  # 1. Access to Setup's .tmp file itself
  # 2. Access to its .tmp.Config file and culture folders, which Windows looks for
  # 3. Access to its resource modules, which Delphi looks for
  # 4. Access outside Setup's folder
  # Exceptions 1 to 3 only apply when the complete path matches.
  sed -ne '/"'$escaped_setup_path'\\innosetup-'$escaped_ver'-'$arch'\.\(tmp\|tmp\.Config\|en-US\|en\|ENU\|EN\|nl-NL\|nl\|NLD\|NL\)"/d' \
    -e '/"'$escaped_setup_path'\\\(en-US\|en\|nl-NL\|nl\)"/d' \
    -e '/"'$escaped_setup_path'\\/p' \
    trace/$arch/procmon.csv >trace/$arch/filtered-setup.csv
  # Fail if the filter left any lines, and show them
  if test -s trace/$arch/filtered-setup.csv
  then
    echo "::error::Unexpected filesystem access in Setup's folder during the $arch installer check"
    cat trace/$arch/filtered-setup.csv
    exit 1
  fi
}

for arch in x86 x64
do
  prepare_installer $arch
done

# The download gives Windows time to flush the comctl32.dll copies before the
# first capture starts. The filter also ignores the flushing, but only for the
# operations it lists.
curl -LO https://download.sysinternals.com/files/ProcessMonitor.zip
unzip -o ProcessMonitor.zip

previous_arch=
for arch in x86 x64
do
  verify_installer $arch $previous_arch
  previous_arch=$arch
done
