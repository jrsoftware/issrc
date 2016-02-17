; Inno Setup
; Copyright (C) 1997-2012 Jordan Russell. All rights reserved.
; Portions by Martijn Laan
; For conditions of distribution and use, see LICENSE.TXT.
;
; Setup script

[Setup]
AppName=Inno Setup
AppId=Inno Setup 5
AppVersion=5.5.9-dev
AppPublisher=jrsoftware.org
AppPublisherURL=http://www.innosetup.com/
AppSupportURL=http://www.innosetup.com/
AppUpdatesURL=http://www.innosetup.com/
VersionInfoCopyright=Copyright (C) 1997-2012 Jordan Russell. Portions Copyright (C) 2000-2012 Martijn Laan.
AppMutex=InnoSetupCompilerAppMutex,Global\InnoSetupCompilerAppMutex
SetupMutex=InnoSetupCompilerSetupMutex,Global\InnoSetupCompilerSetupMutex
MinVersion=0,5.0
DefaultDirName={pf}\Inno Setup 5
DefaultGroupName=Inno Setup 5
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
#ifndef NOSIGNTOOL
SignTool=issigntool
SignTool=issigntool256
SignedUninstaller=yes
#endif

[Languages]
Name: english; MessagesFile: "files\Default.isl"

#sub ProcessFoundFile
  #define FileName FindGetFileName(FindHandle)
  #define Name LowerCase(RemoveFileExt(FileName))
  #define MessagesFile PathName + FileName
  #pragma message "Generating [Languages] entry with name " + Name
  Name: {#Name}; MessagesFile: {#MessagesFile}
#endsub

#define PathName "files\Languages\"
#define FindHandle
#define FindResult

#for {FindHandle = FindResult = FindFirst(PathName + "*.isl", 0); FindResult; FindResult = FindNext(FindHandle)} ProcessFoundFile
#if FindHandle
  #expr FindClose(FindHandle)
#endif
#ifdef UNICODE
  #for {FindHandle = FindResult = FindFirst(PathName + "*.islu", 0); FindResult; FindResult = FindNext(FindHandle)} ProcessFoundFile
  #if FindHandle
    #expr FindClose(FindHandle)
  #endif
#endif

[Messages]
; two "Setup" on the same line looks weird, so put a line break in between
english.WelcomeLabel1=Welcome to the Inno Setup%nSetup Wizard

[Tasks]
Name: desktopicon; Description: "{cm:CreateDesktopIcon}"; Flags: unchecked
Name: fileassoc; Description: "{cm:AssocFileExtension,Inno Setup,.iss}"

[InstallDelete]
; Remove Unicode-only files if needed
#ifndef UNICODE
Type: files; Name: "{app}\Languages\*.islu"
#endif
; Remove ISPP files if needed (leave ISPP.chm)
Type: files; Name: "{app}\ISPP.dll"; Check: not ISPPCheck
Type: files; Name: "{app}\ISPPBuiltins.iss"; Check: not ISPPCheck
; Remove old ISPP files
Type: files; Name: "{app}\ISCmplr.dls"
Type: files; Name: "{app}\Builtins.iss"
; Older versions created the desktop icon under {userdesktop}
Type: files; Name: "{userdesktop}\Inno Setup Compiler.lnk"

[Files]
; Files used by [Code] first so these can be quickly decompressed despite solid compression
Source: "files\ISPP.ico"; Flags: dontcopy
; Other files
Source: "license.txt"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "ishelp\Staging\ISetup.chm"; DestDir: "{app}"; Flags: ignoreversion touch
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
#ifdef UNICODE
Source: "files\Languages\*.islu"; DestDir: "{app}\Languages"; Flags: ignoreversion touch
#endif
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
Source: "ishelp\isfaq.htm"; DestDir: "{app}"; Flags: ignoreversion touch
Source: "Examples\Example1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Example2.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Example3.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\64Bit.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\64BitTwoArch.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Components.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Languages.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\MyProg.exe"; DestDir: "{app}\Examples"; Flags: ignoreversion signonce touch
Source: "Examples\MyProg-x64.exe"; DestDir: "{app}\Examples"; Flags: ignoreversion signonce touch
Source: "Examples\MyProg.chm"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Readme.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Readme-Dutch.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\Readme-German.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodeExample1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodeDlg.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodeClasses.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodeDll.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodeAutomation.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodeAutomation2.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\CodePrepareToInstall.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\UninstallCodeExample1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\MyDll.dll"; DestDir: "{app}\Examples"; Flags: ignoreversion signonce touch
Source: "Examples\MyDll\C\MyDll.c"; DestDir: "{app}\Examples\MyDll\C"; Flags: ignoreversion touch
Source: "Examples\MyDll\C\MyDll.def"; DestDir: "{app}\Examples\MyDll\C"; Flags: ignoreversion touch
Source: "Examples\MyDll\C\MyDll.dsp"; DestDir: "{app}\Examples\MyDll\C"; Flags: ignoreversion touch
Source: "Examples\MyDll\C#\MyDll.cs"; DestDir: "{app}\Examples\MyDll\C#"; Flags: ignoreversion touch
Source: "Examples\MyDll\C#\MyDll.csproj"; DestDir: "{app}\Examples\MyDll\C#"; Flags: ignoreversion touch
Source: "Examples\MyDll\C#\MyDll.sln"; DestDir: "{app}\Examples\MyDll\C#"; Flags: ignoreversion touch
Source: "Examples\MyDll\C#\packages.config"; DestDir: "{app}\Examples\MyDll\C#"; Flags: ignoreversion touch
Source: "Examples\MyDll\C#\Properties\AssemblyInfo.cs"; DestDir: "{app}\Examples\MyDll\C#\Properties"; Flags: ignoreversion touch
Source: "Examples\MyDll\Delphi\MyDll.dpr"; DestDir: "{app}\Examples\MyDll\Delphi"; Flags: ignoreversion touch
Source: "Examples\ISPPExample1.iss"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
Source: "Examples\ISPPExample1License.txt"; DestDir: "{app}\Examples"; Flags: ignoreversion touch
; ISPP files
Source: "Projects\ISPP\Help\Staging\ISPP.chm"; DestDir: "{app}"; Flags: ignoreversion touch
#ifndef isppdll
  #define isppdll "ispp.dll"
#endif
Source: "files\{#isppdll}"; DestName: "ISPP.dll"; DestDir: "{app}"; Flags: ignoreversion signonce touch; Check: ISPPCheck
Source: "files\ISPPBuiltins.iss"; DestDir: "{app}"; Flags: ignoreversion touch; Check: ISPPCheck

[Icons]
Name: "{group}\Inno Setup Compiler"; Filename: "{app}\Compil32.exe"; WorkingDir: "{app}"; AppUserModelID: "JR.InnoSetup.IDE.5"
Name: "{group}\Inno Setup Documentation"; Filename: "{app}\ISetup.chm"
Name: "{group}\Inno Setup Example Scripts"; Filename: "{app}\Examples\"
Name: "{group}\Inno Setup FAQ"; Filename: "{app}\isfaq.htm"
Name: "{group}\Inno Setup Revision History"; Filename: "{app}\whatsnew.htm"
Name: "{commondesktop}\Inno Setup Compiler"; Filename: "{app}\Compil32.exe"; WorkingDir: "{app}"; AppUserModelID: "JR.InnoSetup.IDE.5"; Tasks: desktopicon

[Run]
Filename: "{app}\Compil32.exe"; Parameters: "/ASSOC"; StatusMsg: "{cm:AssocingFileExtension,Inno Setup,.iss}"; Tasks: fileassoc
Filename: "{app}\Compil32.exe"; WorkingDir: "{app}"; Description: "{cm:LaunchProgram,Inno Setup}"; Flags: nowait postinstall skipifsilent

[UninstallRun]
Filename: "{app}\Compil32.exe"; Parameters: "/UNASSOC"; RunOnceId: "RemoveISSAssoc"

[CustomMessages]
ISPPTitle=Inno Setup Preprocessor
ISPPSubtitle=Would you like to install Inno Setup Preprocessor?
ISPPText=Inno Setup Preprocessor (ISPP) is an official add-on for Inno Setup. ISPP allows you to conditionally compile parts of scripts, to use compile time variables in your scripts and to use built-in functions which for example can read from the registry or INI files at compile time.%n%nISPP also contains a special version of the ISCC command line compiler which can take variable definitions as command line parameters and use them during compilation.
ISPPText2=Select whether you would like to install ISPP, then click Next.
ISPPCheck=&Install Inno Setup Preprocessor

[Code]
var
  ISPPPage: TWizardPage;
  ISPPCheckBox: TCheckBox;
  
function GetModuleHandle(lpModuleName: LongInt): LongInt;
external 'GetModuleHandleA@kernel32.dll stdcall';
function ExtractIcon(hInst: LongInt; lpszExeFileName: AnsiString; nIconIndex: LongInt): LongInt;
external 'ExtractIconA@shell32.dll stdcall';
function DrawIconEx(hdc: LongInt; xLeft, yTop: Integer; hIcon: LongInt; cxWidth, cyWidth: Integer; istepIfAniCur: LongInt; hbrFlickerFreeDraw, diFlags: LongInt): LongInt;
external 'DrawIconEx@user32.dll stdcall';
function DestroyIcon(hIcon: LongInt): LongInt;
external 'DestroyIcon@user32.dll stdcall';

const
  DI_NORMAL = 3;
  
function CreateCustomOptionPage(AAfterId: Integer; ACaption, ASubCaption, AIconFileName, ALabel1Caption, ALabel2Caption,
  ACheckCaption: String; var CheckBox: TCheckBox): TWizardPage;
var
  Page: TWizardPage;
  Rect: TRect;
  hIcon: LongInt;
  Label1, Label2: TNewStaticText;
begin
  Page := CreateCustomPage(AAfterID, ACaption, ASubCaption);
  
  try
    AIconFileName := ExpandConstant('{tmp}\' + AIconFileName);
    if not FileExists(AIconFileName) then
      ExtractTemporaryFile(ExtractFileName(AIconFileName));

    Rect.Left := 0;
    Rect.Top := 0;
    Rect.Right := 32;
    Rect.Bottom := 32;

    hIcon := ExtractIcon(GetModuleHandle(0), AIconFileName, 0);
    try
      with TBitmapImage.Create(Page) do begin
        with Bitmap do begin
          Width := 32;
          Height := 32;
          Canvas.Brush.Color := WizardForm.Color;
          Canvas.FillRect(Rect);
          DrawIconEx(Canvas.Handle, 0, 0, hIcon, 32, 32, 0, 0, DI_NORMAL);
        end;
        Parent := Page.Surface;
      end;
    finally
      DestroyIcon(hIcon);
    end;
  except
  end;

  Label1 := TNewStaticText.Create(Page);
  with Label1 do begin
    AutoSize := False;
    Left := WizardForm.SelectDirLabel.Left;
    Width := Page.SurfaceWidth - Left;
    WordWrap := True;
    Caption := ALabel1Caption;
    Parent := Page.Surface;
  end;
  WizardForm.AdjustLabelHeight(Label1);

  Label2 := TNewStaticText.Create(Page);
  with Label2 do begin
    Top := Label1.Top + Label1.Height + ScaleY(12);
    Caption := ALabel2Caption;
    Parent := Page.Surface;
  end;
  WizardForm.AdjustLabelHeight(Label2);

  CheckBox := TCheckBox.Create(Page);
  with CheckBox do begin
    Top := Label2.Top + Label2.Height + ScaleY(12);
    Width := Page.SurfaceWidth;
    Caption := ACheckCaption;
    Parent := Page.Surface;
  end;
  
  Result := Page;
end;

procedure CreateCustomPages;
var
  Caption, SubCaption1, IconFileName, Label1Caption, Label2Caption, CheckCaption: String;
begin
  Caption := CustomMessage('ISPPTitle');
  SubCaption1 := CustomMessage('ISPPSubtitle');
  IconFileName := 'ISPP.ico';
  Label1Caption := CustomMessage('ISPPText');
  Label2Caption := CustomMessage('ISPPText2');
  CheckCaption := CustomMessage('ISPPCheck');

  ISPPPage := CreateCustomOptionPage(wpSelectProgramGroup, Caption, SubCaption1, IconFileName, Label1Caption, Label2Caption, CheckCaption, ISPPCheckBox);
end;

procedure InitializeWizard;
begin
  CreateCustomPages;
  
  ISPPCheckBox.Checked := (GetPreviousData('ISPP', '1') = '1') or (ExpandConstant('{param:ispp|0}') = '1');
end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
begin
  SetPreviousData(PreviousDataKey, 'ISPP', IntToStr(Ord(ISPPCheckBox.Checked)));
end;

function ISPPCheck: Boolean;
begin
  Result := ISPPCheckBox.Checked;
end;

function PortableCheck: Boolean;
begin
  Result := ExpandConstant('{param:portable|0}') = '1';
end;