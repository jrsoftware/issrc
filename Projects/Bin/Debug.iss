; -- Debug.iss --
; Opened when you run the Compil32 project in Debug mode from the Delphi IDE
; Same for ISCmplr, ISCC, and ISPP and for any of the islzma projects
; Use it to test the compiler or Setup or the uninstaller
; The Setup project enables logging to Setup.log when you run it

#pragma message "ɯɐɹƃoɹd ʎɯ"

#include "..\..\setup.allowedpublickeys.iss"

#include "Debug2.iss"

[Setup]
AppName=ɯɐɹƃoɹd ʎɯ
AppVerName=My Program version 1.5
DefaultDirName={autopf}\My Program
AppVersion=1.2.3
WizardStyle=modern dynamic

; This enables debugging
UseSetupLdr=no
OutputDir={#CompilerPath}
OutputBaseFilename=Setup
PrivilegesRequired=lowest

; This makes a task dialog show at startup
;UsePreviousPrivileges=no
;PrivilegesRequiredOverridesAllowed=dialog

; This enables RTL and scaling for testing
;[LangOptions]
;RightToLeft=true
;DialogFontSize=12

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