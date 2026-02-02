; -- Setup.iss --
; Inno Setup's own Setup script

; Inno Setup
; Copyright (C) 1997-2025 Jordan Russell. All rights reserved.
; Portions Copyright (C) 2000-2025 Martijn Laan. All rights reserved.
; For conditions of distribution and use, see LICENSE.TXT.

; #define x64
//
#ifdef x64
  #define arch "x64"
  #define dasharch "-" + arch
  #define spacebit ""
#else
  #define arch "x86"
  #define dasharch ""
  #define spacebit " (32-bit)"
#endif
//
#define CheckArch(str Filename) \
 Local[0] = Is64BitPEImage(AddBackslash(SourcePath) + Filename) != 0, \
 Local[1] = (arch == "x64") != 0, \
 (Local[0] != Local[1]) ? Error(ExtractFilename(Filename) + " has incorrect architecture. " + arch + " required.") : Filename
//
#define AppId "Inno Setup 7"
//
#include "isdonateandmail.iss"

#include "isportable.iss"

[Code]
function GetExtendedAppId(ForceExtension: String): String;
begin
  Result := '{#AppId}';
  if '{#spacebit}' <> '' then begin
    { DefaultDirName: extend if installing to userpf or autodesktop, since those have no distinct 32-bit/64-bit versions
      DefaultGroupName: always extend to not mess up side-by-side installation } 
    if (ForceExtension = '1') or not IsAdminInstallMode or PortableCheck then
      Result := Result + '{#spacebit}';
  end;
end;

[Setup]
AppName=Inno Setup
AppId={#AppId}
AppVersion=7.0.0-dev{#spacebit}
AppPublisher=jrsoftware.org
AppPublisherURL=https://www.innosetup.com/
AppSupportURL=https://www.innosetup.com/
AppUpdatesURL=https://www.innosetup.com/
VersionInfoCopyright=Copyright (C) 1997-2026 Jordan Russell. Portions Copyright (C) 2000-2026 Martijn Laan.
AppMutex=InnoSetupCompilerAppMutex7{#dasharch},Global\InnoSetupCompilerAppMutex7{#dasharch}
SetupMutex=InnoSetupCompilerSetupMutex7{#dasharch},Global\InnoSetupCompilerSetupMutex7{#dasharch}
WizardStyle=modern dynamic
DefaultDirName={code:GetDefaultDirName|{code:GetExtendedAppId|0}}
DefaultGroupName={code:GetExtendedAppId|1}
PrivilegesRequiredOverridesAllowed=dialog
AllowNoIcons=yes
Compression=lzma2/max
SolidCompression=yes
UninstallDisplayIcon={app}\ISIDE.exe
UsePreviousLanguage=no
LicenseFile=license.txt
TimeStampsInUTC=yes
TouchDate=none
TouchTime=00:00
#ifdef SIGNTOOL
SignTool=issigntool256
SignedUninstaller=yes
#endif
SetupArchitecture={#arch}

#expr EmitLanguagesSection

[Messages]
HelpTextNote=/PORTABLE=1%nEnable portable mode.
; Two "Setup" on the same line looks weird, so put a line break in between
english.WelcomeLabel1=Welcome to the Inno Setup%nSetup Wizard

[Tasks]
Name: desktopicon; Description: "{cm:CreateDesktopIcon}"; Flags: unchecked; Check: not PortableCheck
Name: fileassoc; Description: "{cm:AssocFileExtension,Inno Setup,.iss}"; Check: not PortableCheck

[InstallDelete]
; Remove desktop icon if needed
Type: files; Name: {autodesktop}\Inno Setup Compiler.lnk; Tasks: not desktopicon; Check: not PortableCheck
; Remove translations in case any got demoted
Type: files; Name: "{app}\Languages\*.isl"

#include "setup.allowedpublickeys.iss"

#ifdef SIGNTOOL
  #define signcheck "signcheck"
#else
  #define signcheck
#endif

[Files]
Source: "license.txt"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "{#CheckArch("files\ISIDE.exe")}"; DestDir: "{app}"; Flags: ignoreversion signonce touch
Source: "files\isscint{#dasharch}.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\isscint{#dasharch}.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "{#CheckArch("files\ISCC.exe")}"; DestDir: "{app}"; Flags: ignoreversion {#signcheck} touch
Source: "{#CheckArch("files\ISCmplr.dll")}"; DestDir: "{app}"; Flags: ignoreversion issigverify {#signcheck} touch
Source: "files\ISCmplr.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "{#CheckArch("files\ISPP.dll")}"; DestDir: "{app}"; Flags: ignoreversion issigverify {#signcheck} touch
Source: "files\ISPP.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\ISPPBuiltins.iss"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\iszlib{#dasharch}.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\iszlib{#dasharch}.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\isbzip{#dasharch}.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\isbzip{#dasharch}.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\islzma{#dasharch}.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\islzma{#dasharch}.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
#ifdef x64
Source: "files\islzma-Arm64EC.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\islzma-Arm64EC.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
#else
Source: "files\islzma32.exe"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\islzma32.exe.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\islzma64.exe"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\islzma64.exe.issig"; DestDir: "{app}"; Flags: ignoreversion touch
#endif
Source: "{#CheckArch("files\ISSigTool.exe")}"; DestDir: "{app}"; Flags: ignoreversion signonce touch
Source: "files\Setup.e32"; DestDir: "{app}"; Flags: ignoreversion issigverify touch
Source: "files\Setup.e32.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\Setup.e64"; DestDir: "{app}"; Flags: ignoreversion issigverify touch
Source: "files\Setup.e64.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\SetupCustomStyle.e32"; DestDir: "{app}"; Flags: ignoreversion issigverify touch
Source: "files\SetupCustomStyle.e32.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\SetupCustomStyle.e64"; DestDir: "{app}"; Flags: ignoreversion issigverify touch
Source: "files\SetupCustomStyle.e64.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\SetupLdr.e32"; DestDir: "{app}"; Flags: ignoreversion issigverify touch
Source: "files\SetupLdr.e32.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\SetupLdr.e64"; DestDir: "{app}"; Flags: ignoreversion issigverify touch
Source: "files\SetupLdr.e64.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\is7z.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\is7z.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\is7z-x64.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\is7z-x64.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\is7zxa.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\is7zxa.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\is7zxa-x64.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\is7zxa-x64.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\is7zxr.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\is7zxr.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\is7zxr-x64.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\is7zxr-x64.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\isunzlib.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\isunzlib.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\isunzlib-x64.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\isunzlib-x64.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\isbunzip.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\isbunzip.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\isbunzip-x64.dll"; DestDir: "{app}"; Flags: ignoreversion issigverify signcheck touch
Source: "files\isbunzip-x64.dll.issig"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\ISetup.chm"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\ISetup-dark.chm"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "whatsnew.htm"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\Default.isl"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\Languages\*.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch
Source: "files\SetupClassicIcon.ico"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\WizClassicImage.bmp"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\WizClassicImage-IS.bmp"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\WizClassicSmallImage.bmp"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\WizClassicSmallImage-IS.bmp"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "Examples\64Bit.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\64BitTwoArch.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\64BitThreeArch.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\AllPagesExample.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodeAutomation.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodeAutomation2.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodeClasses.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodeDlg.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodeDll.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodeDownloadFiles.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodeExample1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodePrepareToInstall.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Components.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\DownloadFiles.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Example1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Example2.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Example3.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\ISPPExample1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\ISPPExample1License.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\License.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Languages.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\MyDll.dll"; DestDir: "{app}\Examples"; Flags: ignoreversion issigverify signcheck touch
Source: "Examples\MyDll-x64.dll"; DestDir: "{app}\Examples"; Flags: ignoreversion issigverify signcheck touch
Source: "Examples\MyProg.chm"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\MyProg.exe"; DestDir: "{app}\Examples"; Flags: ignoreversion issigverify signcheck touch
Source: "Examples\MyProg-Arm64.exe"; DestDir: "{app}\Examples"; Flags: ignoreversion issigverify signcheck touch
Source: "Examples\MyProg-x64.exe"; DestDir: "{app}\Examples"; Flags: ignoreversion issigverify signcheck touch
Source: "Examples\PowerShell.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Readme.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Readme-Dutch.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Readme-German.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\UnicodeExample1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\UninstallCodeExample1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\MyDll\C\MyDll.c"; DestDir: "{app}\Examples\MyDll\C"; Flags: ignoreversion touch
Source: "Examples\MyDll\C\MyDll.def"; DestDir: "{app}\Examples\MyDll\C"; Flags: ignoreversion touch
Source: "Examples\MyDll\C\MyDll.vcxproj"; DestDir: "{app}\Examples\MyDll\C"; Flags: ignoreversion touch
Source: "Examples\MyDll\C\MyDll.sln"; DestDir: "{app}\Examples\MyDll\C"; Flags: ignoreversion touch
Source: "Examples\MyDll\Delphi\MyDll.dpr"; DestDir: "{app}\Examples\MyDll\Delphi"; Flags: ignoreversion touch
Source: "Examples\MyDll\Delphi\MyDll.dproj"; DestDir: "{app}\Examples\MyDll\Delphi"; Flags: ignoreversion touch

[INI]
Filename: "{app}\isfaq.url"; Section: "InternetShortcut"; Key: "URL"; String: "https://jrsoftware.org/isfaq.php" 

[UninstallDelete]
Type: files; Name: "{app}\isfaq.url"

[Icons]
; All these will be automatically skipped on portable mode, either because of NoIconsCheck being checked, or because of the desktopicon task being removed
Name: "{group}\Inno Setup Compiler"; Filename: "{app}\ISIDE.exe"; WorkingDir: "{app}"; AppUserModelID: "JR.InnoSetup.IDE.7{#dasharch}"
Name: "{group}\Inno Setup Documentation"; Filename: "{app}\ISetup.chm"
Name: "{group}\Inno Setup Documentation (Dark)"; Filename: "{app}\ISetup-dark.chm"
Name: "{group}\Inno Setup Example Scripts"; Filename: "{app}\Examples\"
Name: "{group}\Inno Setup FAQ"; Filename: "{app}\isfaq.url"
Name: "{group}\Inno Setup Revision History"; Filename: "{app}\whatsnew.htm"
Name: "{autodesktop}\Inno Setup Compiler"; Filename: "{app}\ISIDE.exe"; WorkingDir: "{app}"; AppUserModelID: "JR.InnoSetup.IDE.7{#dasharch}"; Tasks: desktopicon

[Run]
; The /ASSOC line will be automatically skipped on portable mode, because of the fileassoc task being removed
Filename: "{app}\ISIDE.exe"; Parameters: "/ASSOC"; StatusMsg: "{cm:AssocingFileExtension,Inno Setup,.iss}"; Tasks: fileassoc
Filename: "{app}\ISIDE.exe"; WorkingDir: "{app}"; Description: "{cm:LaunchProgram,Inno Setup}"; Flags: nowait postinstall skipifsilent

[UninstallRun]
; The /UNASSOC line will be automatically skipped on portable mode, because of Uninstallable being set to no
Filename: "{app}\ISIDE.exe"; Parameters: "/UNASSOC"; RunOnceId: "RemoveISSAssoc"