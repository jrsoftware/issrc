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
Source: "..\Examples\Readme.txt"; DestDir: "{app}"

[Components]
Name: "main"; Description: "Main Files"; Types: full compact custom; Flags: fixed
Name: "help"; Description: "Help Files"; Types: full
Name: "help\english"; Description: "English"; Types: full
Name: "help\dutch"; Description: "Dutch"; Types: full
Name: "help\x"; Description: "X"; Types: full

[Code]
function InitializeSetup: Boolean;
begin
  Result := True;
end;