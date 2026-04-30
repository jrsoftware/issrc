[Setup]
AppName=Script.Test
AppVersion=1.0
DefaultDirName={autopf}\Script.Test
OutputDir=.
OutputBaseFilename=Script.Test-Setup
SetupArchitecture={#arch}
PrivilegesRequired=lowest

[Code]
function InitializeSetup: Boolean;
begin
  SaveStringToFile(ExpandConstant('{src}\Script.Test-Result.txt'), 'OK', False);
  Result := False;
end;