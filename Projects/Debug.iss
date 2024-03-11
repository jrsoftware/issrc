[Setup]
AppName=My Program
AppVerName=My Program version 1.5
DefaultDirName={autopf}\My Program
UseSetupLdr=0
OutputDir=..\Projects
AppVersion=1.2.3        
OutputBaseFilename=setup
PrivilegesRequired=lowest

[Languages]
Name: en; MessagesFile: "compiler:Default.isl"
Name: nl; MessagesFile: "..\Files\Languages\Dutch.isl"

[Files]
Source: "..\Examples\MyProg.exe"; DestDir: "{app}"
Source: "..\Examples\MyProg.chm"; DestDir: "{app}"
Source: "..\Examples\Readme.txt"; DestDir: "{app}"; Flags: isreadme

[Code]
function InitializeSetup: Boolean;
begin
  Result := True;
end;