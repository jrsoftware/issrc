#ifndef arch
  #define arch "x64"
#endif
//
[Setup]
AppName=Script.Test
AppVersion=1.0
DefaultDirName={autopf}\Script.Test
OutputDir=.
OutputBaseFilename=Script.Test-Setup
SetupArchitecture={#arch}
PrivilegesRequired=lowest

#include "Script.ISPP.Test.iss"
#include "Script.ROPS.Test.iss"

[Code]
function InitializeSetup: Boolean;
var
  ResultText: String;
begin
  try
    ROPS_RunAllTests;
    ResultText := 'OK';
  except
    ResultText := GetExceptionMessage;
  end;
  Log(ResultText);
  SaveStringToFile(ExpandConstant('{src}\Script.Test-Result.txt'), ResultText, False);
  Result := False;
end;
