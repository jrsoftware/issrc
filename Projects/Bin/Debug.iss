; -- Debug.iss --
; Opened when you run the Compil32 project in Debug mode from the Delphi IDE
; Same for ISCmplr, ISCC, and ISPP and for any of the islzma projects
; Use it to test the compiler or Setup or the uninstaller

#pragma message "ɯɐɹƃoɹd ʎɯ"

#include "..\..\setup.allowedpublickeys.iss"

#include "Debug2.iss"

[Setup]
AppName=ɯɐɹƃoɹd ʎɯ
AppVerName=My Program version 1.5
DefaultDirName={autopf}\My Program
AppVersion=1.2.3
; The following four lines make the output debuggable from the Setup project
; If you put them in any example script you can debug that example as well
UseSetupLdr=0
OutputDir={#CompilerPath}
OutputBaseFilename=Setup
PrivilegesRequired=lowest

[Languages]
Name: en; MessagesFile: "..\..\Files\Default.isl"
Name: nl; MessagesFile: "..\..\Files\Languages\Dutch.isl"

[Files]
Source: "..\..\Examples\MyProg.exe"; DestDir: "{app}"
Source: "..\..\Examples\MyProg.chm"; DestDir: "{app}"
Source: "..\..\Examples\Readme.txt"; DestDir: "{app}"; Flags: isreadme

[Code]
function InitializeSetup: Boolean;
begin
  Result := True;
end;