; -- 64BitThreeArch.iss --
; Demonstrates how to install a program built for three different
; architectures (x86, x64, Itanium) using a single installer.

; SEE THE DOCUMENTATION FOR DETAILS ON CREATING .ISS SCRIPT FILES!

[Setup]
AppName=My Program
AppVersion=1.5
DefaultDirName={pf}\My Program
DefaultGroupName=My Program
UninstallDisplayIcon={app}\MyProg.exe
Compression=lzma2
SolidCompression=yes
OutputDir=userdocs:Inno Setup Examples Output
; "ArchitecturesInstallIn64BitMode=x64 ia64" requests that the install
; be done in "64-bit mode" on x64 & Itanium, meaning it should use the
; native 64-bit Program Files directory and the 64-bit view of the
; registry. On all other architectures it will install in "32-bit mode".
ArchitecturesInstallIn64BitMode=x64 ia64

[Files]
; Install MyProg-x64.exe if running on x64, MyProg-IA64.exe if
; running on Itanium, MyProg.exe otherwise.
; Place all x64 files here
Source: "MyProg-x64.exe"; DestDir: "{app}"; DestName: "MyProg.exe"; Check: IsX64
; Place all IA64 files here, first one should be marked 'solidbreak'
Source: "MyProg-IA64.exe"; DestDir: "{app}"; DestName: "MyProg.exe"; Check: IsIA64; Flags: solidbreak
; Place all x86 files here, first one should be marked 'solidbreak'
Source: "MyProg.exe"; DestDir: "{app}"; Check: IsOtherArch; Flags: solidbreak
; Place all common files here, first one should be marked 'solidbreak'
Source: "MyProg.chm"; DestDir: "{app}"; Flags: solidbreak
Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme

[Icons]
Name: "{group}\My Program"; Filename: "{app}\MyProg.exe"

[Code]
function IsX64: Boolean;
begin
  Result := Is64BitInstallMode and (ProcessorArchitecture = paX64);
end;

function IsIA64: Boolean;
begin
  Result := Is64BitInstallMode and (ProcessorArchitecture = paIA64);
end;

function IsOtherArch: Boolean;
begin
  Result := not IsX64 and not IsIA64;
end;
