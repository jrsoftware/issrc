; -- AllPagesExample.iss --
; Same as Example1.iss, but shows all the wizard pages Setup may potentially display

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

DisableWelcomePage=no
LicenseFile=license.txt
#define Password 'password'
Password={#Password}
InfoBeforeFile=readme.txt
UserInfoPage=yes
PrivilegesRequired=lowest
DisableDirPage=no
DisableProgramGroupPage=no
InfoAfterFile=readme.txt

[Files]
Source: "MyProg.exe"; DestDir: "{app}"
Source: "MyProg.chm"; DestDir: "{app}"
Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme

[Icons]
Name: "{group}\My Program"; Filename: "{app}\MyProg.exe"

[Components]
Name: "component"; Description: "Component";

[Tasks]
Name: "task"; Description: "Task";

[Code]
procedure InitializeWizard;
begin
  WizardForm.PasswordEdit.Text := '{#Password}';
  
  CreateCustomPage(wpSelectTasks, 'Empty Custom Page', 'This is an empty custom page.'); 
end;

function PrepareToInstall(var NeedsRestart: Boolean): String;
begin
  if SuppressibleMsgBox('Do you want to stop Setup at the Preparing To Install wizard page?', mbConfirmation, MB_YESNO, IDNO) = IDYES then
    Result := 'Stopped by user';
end;
