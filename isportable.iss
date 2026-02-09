// -- IsPortable.iss --
// Include file with support functions for portable mode
//
[Setup]
UsePreviousAppDir=not PortableCheck
Uninstallable=not PortableCheck

[Code]
function PortableCheck: Boolean;
begin
  Result := ExpandConstant('{param:portable|0}') = '1';
end;

function GetDefaultDirName(Param: String): String;
begin
  if PortableCheck then
    Result := '{autodesktop}'
  else
    Result := '{autopf}';
  Result := ExpandConstant(AddBackslash(Result) + Param);
end;

<event('InitializeWizard')>
procedure IsPortableInitializeWizard;
begin
  if PortableCheck then
    WizardForm.NoIconsCheck.Checked := True;
end;

<event('ShouldSkipPage')>
function IsPortableShouldSkipPage(PageID: Integer): Boolean;
begin
  Result := (PageID = wpSelectProgramGroup) and PortableCheck;
end;
[/Code]