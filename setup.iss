; -- Setup.iss --
; Inno Setup's own Setup script

; Inno Setup
; Copyright (C) 1997-2025 Jordan Russell. All rights reserved.
; Portions Copyright (C) 2000-2025 Martijn Laan. All rights reserved.
; For conditions of distribution and use, see LICENSE.TXT.

#include "isdonateandmail.iss"

#include "isportable.iss"

[Setup]
AppName=Inno Setup
AppId={code:GetAppId|Inno Setup 6}
AppVersion=6.4.1
AppPublisher=jrsoftware.org
AppPublisherURL=https://www.innosetup.com/
AppSupportURL=https://www.innosetup.com/
AppUpdatesURL=https://www.innosetup.com/
VersionInfoCopyright=Copyright (C) 1997-2025 Jordan Russell. Portions Copyright (C) 2000-2025 Martijn Laan.
AppMutex=InnoSetupCompilerAppMutex,Global\InnoSetupCompilerAppMutex
SetupMutex=InnoSetupCompilerSetupMutex,Global\InnoSetupCompilerSetupMutex
WizardStyle=modern
DefaultDirName={code:GetDefaultDirName|Inno Setup 6}
DefaultGroupName=Inno Setup 6
PrivilegesRequiredOverridesAllowed=commandline
AllowNoIcons=yes
Compression=lzma2/max
SolidCompression=yes
Uninstallable=not PortableCheck
UninstallDisplayIcon={app}\Compil32.exe
UsePreviousLanguage=no
LicenseFile=license.txt
TimeStampsInUTC=yes
TouchDate=none
TouchTime=00:00
#ifdef SIGNTOOL
SignTool=issigntool256
SignedUninstaller=yes
#endif

#sub ProcessFoundLanguagesFile
  #define FileName FindGetFileName(FindHandle)
  #define Name LowerCase(RemoveFileExt(FileName))
  #define MessagesFile FindPathName + FileName
  //#define CustomMessagesFile FindPathName + 'Setup\' + Name + '.isl';
  //#if FileExists(CustomMessagesFile)
  //  #pragma message "Generating [Languages] entry with name " + Name + ": " + MessagesFile + ' & ' + CustomMessagesFile
  //  Name: {#Name}; MessagesFile: "{#MessagesFile},{#CustomMessagesFile}"
  //#else
    #pragma message "Generating [Languages] entry with name " + Name + ": " + MessagesFile
Name: {#Name}; MessagesFile: "{#MessagesFile}"
  //#endif
#endsub
//
#define FindPathName
#define FindHandle
#define FindResult
//
#sub DoFindFiles
  #for {FindHandle = FindResult = FindFirst(FindPathName + "*.isl", 0); FindResult; FindResult = FindNext(FindHandle)} ProcessFoundLanguagesFile
  #if FindHandle
    #expr FindClose(FindHandle)
  #endif
#endsub
//
#define FindFiles(str PathName) \
  FindPathName = PathName, \
  DoFindFiles
//
[Languages]
Name: english; MessagesFile: "files\Default.isl"
// Generate [Languages] entries for all official translations
#expr FindFiles("files\Languages\")

[Messages]
HelpTextNote=/PORTABLE=1%nEnable portable mode.
; Two "Setup" on the same line looks weird, so put a line break in between
english.WelcomeLabel1=Welcome to the Inno Setup%nSetup Wizard

[Tasks]
Name: desktopicon; Description: "{cm:CreateDesktopIcon}"; Flags: unchecked; Check: not PortableCheck
Name: fileassoc; Description: "{cm:AssocFileExtension,Inno Setup,.iss}"; Check: not PortableCheck

[InstallDelete]
; Remove old ISPP files
Type: files; Name: "{app}\ISCmplr.dls"
Type: files; Name: "{app}\Builtins.iss"
Type: files; Name: "{app}\ISPP.chm"
; Remove desktop icon if needed
Type: files; Name: {autodesktop}\Inno Setup Compiler.lnk; Tasks: not desktopicon; Check: not PortableCheck
; Remove old FAQ file
Type: files; Name: "{app}\isfaq.htm"
; Remove old .islu files
Type: files; Name: "{app}\Languages\*.islu"
; Remove translations in case any got demoted
Type: files; Name: "{app}\Languages\*.isl"
; Remove old ispack files
Type: files; Name: "{app}\Ispack-setup.exe"
Type: files; Name: "{app}\Examples\Setup.iss"
Type: files; Name: "{app}\Examples\Setup.ico"
Type: files; Name: "{app}\Examples\IsDonateAndMail.iss"
Type: files; Name: "{app}\Examples\IsDonate.bmp"
Type: files; Name: "{app}\Examples\IsMail.bmp"
Type: files; Name: "{app}\Examples\IsPortable.iss"
; Removed old/renamed wizard images
Type: files; Name: "{app}\WizModernImage.bmp"
Type: files; Name: "{app}\WizModernImage-IS.bmp"
Type: files; Name: "{app}\WizModernSmallImage.bmp"
Type: files; Name: "{app}\WizModernSmallImage-IS.bmp"
; Remove old ISCrypt.dll
Type: files; Name: "{app}\ISCrypt.dll"

[Files]
Source: "license.txt"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\ISetup.chm"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\ISetup-dark.chm"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\Compil32.exe"; DestDir: "{app}"; Flags: ignoreversion signonce touch
Source: "files\isscint.dll"; DestDir: "{app}"; Flags: ignoreversion signonce touch
#ifndef isccexe
  #define isccexe "ISCC.exe"
#endif
Source: "files\{#isccexe}"; DestName: "ISCC.exe"; DestDir: "{app}"; Flags: ignoreversion signonce touch
#ifndef iscmplrdll
  #define iscmplrdll "ISCmplr.dll"
#endif
Source: "files\{#iscmplrdll}"; DestName: "ISCmplr.dll"; DestDir: "{app}"; Flags: ignoreversion signonce touch
Source: "files\Setup.e32"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\SetupLdr.e32"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\Default.isl"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\Languages\*.isl"; DestDir: "{app}\Languages"; Flags: ignoreversion touch
Source: "files\SetupClassicIcon.ico"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\WizClassicImage.bmp"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\WizClassicImage-IS.bmp"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\WizClassicSmallImage.bmp"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\WizClassicSmallImage-IS.bmp"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\iszlib.dll"; DestDir: "{app}"; Flags: ignoreversion signonce touch
Source: "files\isunzlib.dll"; DestDir: "{app}"; Flags: ignoreversion signonce touch
Source: "files\isbzip.dll"; DestDir: "{app}"; Flags: ignoreversion signonce touch
Source: "files\isbunzip.dll"; DestDir: "{app}"; Flags: ignoreversion signonce touch
#ifndef islzmadll
  #define islzmadll "islzma.dll"
#endif
Source: "files\{#islzmadll}"; DestName: "islzma.dll"; DestDir: "{app}"; Flags: ignoreversion signonce touch
Source: "files\islzma32.exe"; DestDir: "{app}"; Flags: ignoreversion signonce touch
Source: "files\islzma64.exe"; DestDir: "{app}"; Flags: ignoreversion signonce touch
Source: "whatsnew.htm"; DestDir: "{app}"; Flags: ignoreversion touch
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
Source: "Examples\Example1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Example2.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Example3.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\ISPPExample1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\ISPPExample1License.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\License.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Languages.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\MyDll.dll"; DestDir: "{app}\Examples"; Flags: ignoreversion signonce touch
Source: "Examples\MyProg.chm"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\MyProg.exe"; DestDir: "{app}\Examples"; Flags: ignoreversion signonce touch
Source: "Examples\MyProg-Arm64.exe"; DestDir: "{app}\Examples"; Flags: ignoreversion signonce touch
Source: "Examples\MyProg-x64.exe"; DestDir: "{app}\Examples"; Flags: ignoreversion signonce touch
Source: "Examples\PowerShell.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Readme.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Readme-Dutch.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Readme-German.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\UnicodeExample1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\UninstallCodeExample1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\MyDll\C\MyDll.c"; DestDir: "{app}\Examples\MyDll\C"; Flags: ignoreversion touch
Source: "Examples\MyDll\C\MyDll.def"; DestDir: "{app}\Examples\MyDll\C"; Flags: ignoreversion touch
Source: "Examples\MyDll\C\MyDll.dsp"; DestDir: "{app}\Examples\MyDll\C"; Flags: ignoreversion touch
Source: "Examples\MyDll\C#\MyDll.cs"; DestDir: "{app}\Examples\MyDll\C#"; Flags: ignoreversion touch
Source: "Examples\MyDll\C#\MyDll.csproj"; DestDir: "{app}\Examples\MyDll\C#"; Flags: ignoreversion touch
Source: "Examples\MyDll\C#\MyDll.sln"; DestDir: "{app}\Examples\MyDll\C#"; Flags: ignoreversion touch
Source: "Examples\MyDll\C#\packages.config"; DestDir: "{app}\Examples\MyDll\C#"; Flags: ignoreversion touch
Source: "Examples\MyDll\C#\Properties\AssemblyInfo.cs"; DestDir: "{app}\Examples\MyDll\C#\Properties"; Flags: ignoreversion touch
Source: "Examples\MyDll\Delphi\MyDll.dpr"; DestDir: "{app}\Examples\MyDll\Delphi"; Flags: ignoreversion touch
#ifndef isppdll
  #define isppdll "ispp.dll"
#endif
Source: "files\{#isppdll}"; DestName: "ISPP.dll"; DestDir: "{app}"; Flags: ignoreversion signonce touch
Source: "files\ISPPBuiltins.iss"; DestDir: "{app}"; Flags: ignoreversion touch

[INI]
Filename: "{app}\isfaq.url"; Section: "InternetShortcut"; Key: "URL"; String: "https://jrsoftware.org/isfaq.php" 

[UninstallDelete]
Type: files; Name: "{app}\isfaq.url"

[Icons]
; All these will be automatically skipped on portable mode, either because of NoIconsCheck being checked, or because of the desktopicon task being removed
Name: "{group}\Inno Setup Compiler"; Filename: "{app}\Compil32.exe"; WorkingDir: "{app}"; AppUserModelID: "JR.InnoSetup.IDE.6"
Name: "{group}\Inno Setup Documentation"; Filename: "{app}\ISetup.chm"
Name: "{group}\Inno Setup Documentation (Dark)"; Filename: "{app}\ISetup-dark.chm"
Name: "{group}\Inno Setup Example Scripts"; Filename: "{app}\Examples\"
Name: "{group}\Inno Setup FAQ"; Filename: "{app}\isfaq.url"
Name: "{group}\Inno Setup Revision History"; Filename: "{app}\whatsnew.htm"
Name: "{autodesktop}\Inno Setup Compiler"; Filename: "{app}\Compil32.exe"; WorkingDir: "{app}"; AppUserModelID: "JR.InnoSetup.IDE.6"; Tasks: desktopicon

[Run]
; The /ASSOC line will be automatically skipped on portable mode, because of the fileassoc task being removed
Filename: "{app}\Compil32.exe"; Parameters: "/ASSOC"; StatusMsg: "{cm:AssocingFileExtension,Inno Setup,.iss}"; Tasks: fileassoc
Filename: "{app}\Compil32.exe"; WorkingDir: "{app}"; Description: "{cm:LaunchProgram,Inno Setup}"; Flags: nowait postinstall skipifsilent

[UninstallRun]
; The /UNASSOC line will be automatically skipped on portable mode, because of Uninstallable being set to no
Filename: "{app}\Compil32.exe"; Parameters: "/UNASSOC"; RunOnceId: "RemoveISSAssoc"