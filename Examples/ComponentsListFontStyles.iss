; -- ComponentsListFontStyles.iss --
;
; This script shows how to use new ItemFontStyle and SubItemFontStyle
; propertys for TNewCheckListBox component.
;
; Allows you to set the font style to any item in the TNewCheckListBox.
; Supported Font styles: [fsBold], [fsItalic], [fsUnderline], [fsStrikeOut]

[Setup]
AppName=My Application
AppVersion=1.5
DefaultDirName={autopf}\My Application
SolidCompression=yes
Compression=lzma/ultra
PrivilegesRequired=none
OutputDir=userdocs:Inno Setup Examples Output
DisableWelcomePage=yes

[Types]
Name: "full"; Description: "Full installation"
Name: "compact"; Description: "Compact installation"
Name: "custom"; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "program"; Description: "Program Files"; Types: full compact custom; Flags: fixed
Name: "help"; Description: "Help File (Select your Language)"; Types: full compact
Name: "help\help1"; Description: "Help File 1 (Test Bold)"; Flags: exclusive
Name: "help\help2"; Description: "Help File 2 (Test Italic)"; Flags: exclusive
Name: "readme"; Description: "Readme (Test Underline)"; Types: full
Name: "readme\readme1"; Description: "Readme 1 (Test StrikeOut)"; Types: full compact
Name: "readme\readme2"; Description: "Readme 2"; Types: full

[Files]
Source: "MyProg.exe"; DestDir: "{app}"; Components: program
Source: "MyProg.chm"; DestDir: "{app}"; DestName: "MyProg1.chm"; Components: help\help1
Source: "MyProg.chm"; DestDir: "{app}"; DestName: "MyProg2.chm"; Components: help\help2
Source: "Readme.txt"; DestDir: "{app}"; DestName: "Readme1.txt"; Components: readme\readme1; Flags: isreadme
Source: "Readme.txt"; DestDir: "{app}"; DestName: "Readme2.txt"; Components: readme\readme2

[Icons]
Name: "{group}\My Program"; Filename: "{app}\MyProg.exe"

[Code]
procedure InitializeWizard();
begin
//////////////////////////////////////////////////////////////////////////////
// If you need to make an emphasis on each item, this is a very good function.
  WizardForm.ComponentsList.ItemFontStyle[2] := [fsBold];
  WizardForm.ComponentsList.ItemFontStyle[3] := [fsItalic];
  WizardForm.ComponentsList.ItemFontStyle[4] := [fsUnderline];
  WizardForm.ComponentsList.ItemFontStyle[5] := [fsStrikeOut];
  WizardForm.ComponentsList.SubItemFontStyle[5] := [fsBold];
  WizardForm.ComponentsList.SubItemFontStyle[4] := [fsItalic];
  WizardForm.ComponentsList.SubItemFontStyle[3] := [fsUnderline];
  WizardForm.ComponentsList.SubItemFontStyle[2] := [fsStrikeOut];
end;

