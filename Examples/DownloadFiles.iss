; -- DownloadFiles.iss --
;
; This script demonstrates how to download files and archives, extract
; downloaded archives, and verify the integrity of downloaded files and
; archives, all without using [Code].
;
; To verify the downloaded files and archives, this script shows two methods:
; -For ISSigTool-latest.exe and MyProg-ExtraReadmes.7z: using Inno Setup
;  Signature Tool, the [ISSigKeys] section, and .issig signature files which
;  are automatically downloaded before the main file is downloaded
; -For iscrypt.dll: using a simple SHA-256 hash check
; Using the Inno Setup Signature Tool has the benefit that the script does not
; need to be changed when the downloaded file or archive changes, so any
; installers built will also keep working (they are "evergreen")

[Setup]
AppName=My Program
AppVersion=1.5
WizardStyle=modern dynamic
DefaultDirName={autopf}\My Program
DefaultGroupName=My Program
UninstallDisplayIcon={app}\MyProg.exe
OutputDir=userdocs:Inno Setup Examples Output
ArchiveExtraction=auto

[ISSigKeys]
; Use Inno Setup Signature Tool (ISSigTool.exe) to create your own keys and
; .issig signature files
Name: mykey; RuntimeID: def01; \
  KeyID:   "def0147c3bbc17ab99bf7b7a9c2de1390283f38972152418d7c2a4a7d7131a38"; \
  PublicX: "e3e943066aff8f28d2219fd71c9ffff4c8d1aa26bc4225434be67180ab5e242d"; \
  PublicY: "e419041c3f54551e86a1c47f387005cd535dfc9d64339b30d37f9a4f7866b650"

[Files]
; Place any regular files here
Source: "MyProg.exe"; DestDir: "{app}"
Source: "MyProg.chm"; DestDir: "{app}"
Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme
; These files will be downloaded and verified during the actual installation
Source: "https://files.jrsoftware.org/is/misc/ISSigTool.exe"; DestName: "ISSigTool-latest.exe"; DestDir: "{app}"; \
  ExternalSize: 1_000_000; Flags: external download ignoreversion issigverify
Source: "https://jrsoftware.org/download.php/iscrypt.dll?dontcount=1"; DestName: "ISCrypt.dll"; DestDir: "{app}"; \
  Hash: "2f6294f9aa09f59a574b5dcd33be54e16b39377984f3d5658cda44950fa0f8fc"; \
  ExternalSize: 2560; Flags: external download ignoreversion
; This archive will be downloaded and verified at the start of the Preparing to Install Step
; It is then extracted normally during the actual installation process
Source: "https://files.jrsoftware.org/is/misc/myprog-extrareadmes-01.7z"; DestName: "MyProg-ExtraReadmes.7z"; DestDir: "{app}"; \
  ExternalSize: 88; Flags: external download extractarchive recursesubdirs ignoreversion issigverify
; This archive will be downloaded and extracted without verification
Source: "https://github.com/jrsoftware/issrc/archive/refs/heads/main.zip"; DestName: "issrc-main.zip"; DestDir: "{app}"; \
  ExternalSize: 22_000_000; Flags: external download extractarchive recursesubdirs ignoreversion

[Icons]
Name: "{group}\My Program"; Filename: "{app}\MyProg.exe"