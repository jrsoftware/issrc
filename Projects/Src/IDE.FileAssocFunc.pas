unit IDE.FileAssocFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE's functions for registering/unregistering the .iss file association
}

interface

function RegisterISSFileAssociation(const AllowInteractive: Boolean; var AllUsers: Boolean): Boolean;
procedure UnregisterISSFileAssociation;

implementation

uses
  Windows, SysUtils, PathFunc, ShlObj, Shared.CommonFunc.Vcl, Shared.CommonFunc;
  
function GetRootkey: HKEY;
begin
  if IsAdminLoggedOn then
    Result := HKEY_LOCAL_MACHINE
  else
    Result := HKEY_CURRENT_USER;
end;

procedure UnregisterISSFileAssociationDo(const Rootkey: HKEY); forward;

function RegisterISSFileAssociation(const AllowInteractive: Boolean; var AllUsers: Boolean): Boolean;

  procedure SetKeyValue(const Rootkey: HKEY; const Subkey, ValueName: PChar; const Data: String);

    procedure Check(const Res: Longint);
    begin
      if Res <> ERROR_SUCCESS then
        raise Exception.CreateFmt('Error creating file association:'#13#10'%d - %s',
          [Res, Win32ErrorString(Res)]);
    end;

  var
    K: HKEY;
    Disp: DWORD;
  begin
    Check(RegCreateKeyExView(rvDefault, Rootkey, Subkey, 0, nil, 0, KEY_SET_VALUE,
      nil, K, @Disp));
    try
      Check(RegSetValueEx(K, ValueName, 0, REG_SZ, PChar(Data), (Length(Data)+1)*SizeOf(Data[1])));
    finally
      RegCloseKey(K);
    end;
  end;

var
  SelfName: String;
  Rootkey: HKEY;
begin
  Rootkey := GetRootkey;
  AllUsers := Rootkey = HKEY_LOCAL_MACHINE;
  
  Result := AllUsers or not AllowInteractive or
            (MsgBox('Unable to associate for all users without administrative privileges. Do you want to associate only for yourself instead?',
              'Associate', mbConfirmation, MB_YESNO) = IDYES);
  if not Result then
    Exit;

  SelfName := NewParamStr(0);

  SetKeyValue(Rootkey, 'Software\Classes\.iss', nil, 'InnoSetupScriptFile');
  SetKeyValue(Rootkey, 'Software\Classes\.iss', 'Content Type', 'text/plain');

  SetKeyValue(Rootkey, 'Software\Classes\InnoSetupScriptFile', nil, 'Inno Setup Script');
  SetKeyValue(Rootkey, 'Software\Classes\InnoSetupScriptFile\DefaultIcon', nil, SelfName + ',1');
  SetKeyValue(Rootkey, 'Software\Classes\InnoSetupScriptFile\shell\open\command', nil,
    '"' + SelfName + '" "%1"');
  SetKeyValue(Rootkey, 'Software\Classes\InnoSetupScriptFile\shell\OpenWithInnoSetup', nil,
    'Open with &Inno Setup');
  SetKeyValue(Rootkey, 'Software\Classes\InnoSetupScriptFile\shell\OpenWithInnoSetup\command', nil,
    '"' + SelfName + '" "%1"');
  SetKeyValue(Rootkey, 'Software\Classes\InnoSetupScriptFile\shell\Compile', nil, 'Compi&le');
  SetKeyValue(Rootkey, 'Software\Classes\InnoSetupScriptFile\shell\Compile\command', nil,
    '"' + SelfName + '" /cc "%1"');

  { If we just associated for all users, remove our existing association for the current user if it exists. }
  if AllUsers then
    UnregisterISSFileAssociationDo(HKEY_CURRENT_USER);

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

procedure UnregisterISSFileAssociationDo(const Rootkey: HKEY);
begin
  { Remove 'InnoSetupScriptFile' entirely. We own it. }
  RegDeleteKeyIncludingSubkeys(rvDefault, Rootkey,
    'Software\Classes\InnoSetupScriptFile');

  { Leave '.iss' as-is. Other apps may have added their own OpenWithProgids
    values there, and Microsoft docs recommend against trying to delete the
    key's default value (which points to a ProgID). See:
    https://learn.microsoft.com/en-us/windows/win32/shell/fa-file-types
  }

  { Remove unnecessary key set by previous versions }
  RegDeleteKeyIncludingSubkeys(rvDefault, Rootkey,
    'Software\Classes\Applications\Compil32.exe');
end;

procedure UnregisterISSFileAssociation;
begin
  UnregisterISSFileAssociationDo(HKEY_CURRENT_USER);
  if IsAdminLoggedOn then
    UnregisterISSFileAssociationDo(HKEY_LOCAL_MACHINE);

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

end.
