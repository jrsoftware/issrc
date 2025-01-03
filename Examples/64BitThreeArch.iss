; -- 64BitThreeArch.iss --
; Demonstrates how to install a program built for three different
; architectures (x86, x64, Arm64) using a single installer.

; SEE THE DOCUMENTATION FOR DETAILS ON CREATING .ISS SCRIPT FILES!

[Setup]
AppName=My Program
AppVersion=1.5
WizardStyle=modern
DefaultDirName={autopf}\My Program
DefaultGroupName=My Program
UninstallDisplayIcon={app}\MyProg.exe
Compression=lzma2
SolidCompression=yes
OutputDir=userdocs:Inno Setup Examples Output
; "ArchitecturesInstallIn64BitMode=x64compatible or arm64" instructs
; Setup to use "64-bit install mode" on x64-compatible systems and
; Arm64 systems, meaning Setup should use the native 64-bit Program
; Files directory and the 64-bit view of the registry. On all other
; OS architectures (e.g., 32-bit x86), Setup will use "32-bit
; install mode".
ArchitecturesInstallIn64BitMode=x64compatible or arm64

[Files]
; In order of preference, we want to install:
; - Arm64 binaries on Arm64 systems
; - else, x64 binaries on x64-compatible systems
; - else, x86 binaries

; Place all Arm64-specific files here, using 'Check: PreferArm64Files' on each entry.
Source: "MyProg-Arm64.exe"; DestDir: "{app}"; DestName: "MyProg.exe"; Check: PreferArm64Files

; Place all x64-specific files here, using 'Check: PreferX64Files' on each entry.
; Only the first entry should include the 'solidbreak' flag.
Source: "MyProg-x64.exe"; DestDir: "{app}"; DestName: "MyProg.exe"; Check: PreferX64Files; Flags: solidbreak

; Place all x86-specific files here, using 'Check: PreferX86Files' on each entry.
; Only the first entry should include the 'solidbreak' flag.
Source: "MyProg.exe"; DestDir: "{app}"; Check: PreferX86Files; Flags: solidbreak

; Place all common files here.
; Only the first entry should include the 'solidbreak' flag.
Source: "MyProg.chm"; DestDir: "{app}"; Flags: solidbreak
Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme

[Icons]
Name: "{group}\My Program"; Filename: "{app}\MyProg.exe"

[Code]
function PreferArm64Files: Boolean;
begin
  Result := IsArm64;
end;

function PreferX64Files: Boolean;
begin
  Result := not PreferArm64Files and IsX64Compatible;
end;

function PreferX86Files: Boolean;
begin
  Result := not PreferArm64Files and not PreferX64Files;
end;
