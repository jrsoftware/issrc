; -- DownloadFiles.iss --
;
; This script demonstrates how to download files and archives, extract
; downloaded archives, and verify the integrity of downloaded files and
; archives, all without using [Code].
;
; To verify the downloaded files and archives, this script shows two methods:
; -For innosetup-latest.exe and MyProg-ExtraReadmes.7z: using Inno Setup
;  Signature Tool, the [ISSigKeys] section, and .issig sginature files which
;  are automatically downloaded before the main file is downloaded
; -For iscrypt.dll: using a simple SHA-256 hash check
; Using the Inno Setup Signature Tool has the benefit that the script does not
; need to be changed when the downloaded file or archive changes, so any
; installers built will also keep working (they are "evergreen")

[Setup]
AppName=My Program
AppVersion=1.5
WizardStyle=modern
DefaultDirName={autopf}\My Program
DefaultGroupName=My Program
UninstallDisplayIcon={app}\MyProg.exe
OutputDir=userdocs:Inno Setup Examples Output
ArchiveExtraction=full
; Use "ArchiveExtraction=enhanced" if all your archives are .7z files
; Use "ArchiveExtraction=enhanced/nopassword" if all your archives are not password-protected

[ISSigKeys]
; Use Inno Setup Signature Tool (ISSigTool.exe) to create your own keys and
; .issig signature files
Name: mykey; RuntimeID: def02; \
  KeyID:   "def020edee3c4835fd54d85eff8b66d4d899b22a777353ca4a114b652e5e7a28"; \
  PublicX: "515dc7d6c16d4a46272ceb3d158c5630a96466ab4d948e72c2029d737c823097"; \
  PublicY: "f3c21f6b5156c52a35f6f28016ee3e31a3ded60c325b81fb7b1f88c221081a61"

[Files]
; Place any regular files here
Source: "MyProg.exe"; DestDir: "{app}"
Source: "MyProg.chm"; DestDir: "{app}"
Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme
; These files will be downloaded and verified during the actual installation
Source: "https://jrsoftware.org/download.php/is.exe?dontcount=1"; DestName: "innosetup-latest.exe"; DestDir: "{app}"; \
  ExternalSize: 7_000_000; Flags: external download ignoreversion issigverify
Source: "https://jrsoftware.org/download.php/iscrypt.dll?dontcount=1"; DestName: "ISCrypt.dll"; DestDir: "{app}"; \
  Hash: "2f6294f9aa09f59a574b5dcd33be54e16b39377984f3d5658cda44950fa0f8fc"; \
  ExternalSize: 2560; Flags: external download ignoreversion
; This archive will be downloaded and verified at the start of the Preparing to Install Step
; It is then extracted normally during the actual installation process
Source: "https://jrsoftware.org/download.php/myprog-extrareadmes.7z"; DestName: "MyProg-ExtraReadmes.7z"; DestDir: "{app}"; \
  ExternalSize: 275; Flags: external download extractarchive recursesubdirs ignoreversion issigverify
; This archive will be downloaded and extracted without verification
Source: "https://github.com/jrsoftware/issrc/archive/refs/heads/main.zip"; DestName: "issrc-main.zip"; DestDir: "{app}"; \
  ExternalSize: 15_000_000; Flags: external download extractarchive recursesubdirs ignoreversion

[Icons]
Name: "{group}\My Program"; Filename: "{app}\MyProg.exe"