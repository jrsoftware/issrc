; Inno Setup
; Copyright (C) 1997-2020 Jordan Russell. All rights reserved.
; Portions Copyright (C) 2000-2020 Martijn Laan. All rights reserved.
; For conditions of distribution and use, see LICENSE.TXT.
;
; Setup script

#include "donate.iss"

[Setup]
AppName=Inno Setup
AppId=Inno Setup 6
AppVersion=6.0.6-dev
AppPublisher=jrsoftware.org
AppPublisherURL=https://www.innosetup.com/
AppSupportURL=https://www.innosetup.com/
AppUpdatesURL=https://www.innosetup.com/
VersionInfoCopyright=Copyright (C) 1997-2020 Jordan Russell. Portions Copyright (C) 2000-2020 Martijn Laan.
AppMutex=InnoSetupCompilerAppMutex,Global\InnoSetupCompilerAppMutex
SetupMutex=InnoSetupCompilerSetupMutex,Global\InnoSetupCompilerSetupMutex
WizardStyle=modern
DefaultDirName={autopf}\Inno Setup 6
DefaultGroupName=Inno Setup 6
PrivilegesRequiredOverridesAllowed=dialog
AllowNoIcons=yes
Compression=lzma2/max
SolidCompression=yes
Uninstallable=not PortableCheck
UninstallDisplayIcon={app}\Compil32.exe
LicenseFile=license.txt
TimeStampsInUTC=yes
TouchDate=none
TouchTime=00:00
WizardImageFile=compiler:WizModernImage-IS.bmp
WizardSmallImageFile=compiler:WizModernSmallImage-IS.bmp
#ifdef SIGNTOOL
SignTool=issigntool
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

#define FindPathName
#define FindHandle
#define FindResult

#sub DoFindFiles
  #for {FindHandle = FindResult = FindFirst(FindPathName + "*.isl", 0); FindResult; FindResult = FindNext(FindHandle)} ProcessFoundLanguagesFile
  #if FindHandle
    #expr FindClose(FindHandle)
  #endif
#endsub

#define FindFiles(str PathName) \
  FindPathName = PathName, \
  DoFindFiles

[Languages]
Name: english; MessagesFile: "files\Default.isl"
; Generate [Languages] entries for all official translations
#expr FindFiles("files\Languages\")

[Messages]
HelpTextNote=/PORTABLE=1%nEnable portable mode.
; Two "Setup" on the same line looks weird, so put a line break in between
english.WelcomeLabel1=Welcome to the Inno Setup%nSetup Wizard

[Tasks]
Name: desktopicon; Description: "{cm:CreateDesktopIcon}"; Flags: unchecked
Name: fileassoc; Description: "{cm:AssocFileExtension,Inno Setup,.iss}"

[InstallDelete]
; Remove old ISPP files
Type: files; Name: "{app}\ISCmplr.dls"
Type: files; Name: "{app}\Builtins.iss"
; Remove desktop icon if needed
Type: files; Name: {autodesktop}\Inno Setup Compiler.lnk; Tasks: not desktopicon
; Remove old FAQ file
Type: files; Name: "{app}\isfaq.htm"
; Remove old .islu files
Type: files; Name: "{app}\Languages\*.islu"
; Remove translations in case any got demoted
Type: files; Name: "{app}\Languages\*.isl"

[Files]
Source: "license.txt"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\ISetup.chm"; DestDir: "{app}"; Flags: ignoreversion touch
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
Source: "files\WizModernImage.bmp"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\WizModernImage-IS.bmp"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\WizModernSmallImage.bmp"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "files\WizModernSmallImage-IS.bmp"; DestDir: "{app}"; Flags: ignoreversion touch
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
Source: "Examples\MyProg-ARM64.exe"; DestDir: "{app}\Examples"; Flags: ignoreversion signonce touch
Source: "Examples\MyProg-x64.exe"; DestDir: "{app}\Examples"; Flags: ignoreversion signonce touch
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
Source: "files\ISPP.chm"; DestDir: "{app}"; Flags: ignoreversion touch
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
Name: "{group}\Inno Setup Compiler"; Filename: "{app}\Compil32.exe"; WorkingDir: "{app}"; AppUserModelID: "JR.InnoSetup.IDE.6"
Name: "{group}\Inno Setup Documentation"; Filename: "{app}\ISetup.chm"
Name: "{group}\Inno Setup Example Scripts"; Filename: "{app}\Examples\"
Name: "{group}\Inno Setup FAQ"; Filename: "{app}\isfaq.url"
Name: "{group}\Inno Setup Revision History"; Filename: "{app}\whatsnew.htm"
Name: "{autodesktop}\Inno Setup Compiler"; Filename: "{app}\Compil32.exe"; WorkingDir: "{app}"; AppUserModelID: "JR.InnoSetup.IDE.6"; Tasks: desktopicon

[Run]
Filename: "{app}\Compil32.exe"; Parameters: "/ASSOC"; StatusMsg: "{cm:AssocingFileExtension,Inno Setup,.iss}"; Tasks: fileassoc
Filename: "{app}\Compil32.exe"; WorkingDir: "{app}"; Description: "{cm:LaunchProgram,Inno Setup}"; Flags: nowait postinstall skipifsilent

[UninstallRun]
Filename: "{app}\Compil32.exe"; Parameters: "/UNASSOC"; RunOnceId: "RemoveISSAssoc"

[Code]
function PortableCheck: Boolean;
begin
  Result := ExpandConstant('{param:portable|0}') = '1';
end;