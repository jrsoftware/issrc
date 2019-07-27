unit CompFileAssoc;

{
  Inno Setup
  Copyright (C) 1997-2018 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Functions for registering/unregistering the .iss file association
}

interface

function RegisterISSFileAssociation(const AllowInteractive: Boolean; var AllUsers: Boolean): Boolean;
procedure UnregisterISSFileAssociation;

implementation

uses
  Windows, SysUtils, PathFunc, ShlObj, CmnFunc, CmnFunc2;
  
function GetRootkey: HKEY;
begin
  if IsAdminLoggedOn then
    Result := HKEY_LOCAL_MACHINE
  else
    Result := HKEY_CURRENT_USER;
end;

procedure UnregisterISSFileAssociationDo(const Rootkey: HKEY; const ChangeNotify: Boolean); forward;

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
    UnregisterISSFileAssociationDo(HKEY_CURRENT_USER, False);

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

procedure UnregisterISSFileAssociationDo(const Rootkey: HKEY; const ChangeNotify: Boolean);

  function KeyValueEquals(const Rootkey: HKEY; const Subkey: PChar; const Data: String): Boolean;
  var
    K: HKEY;
    S: String;
  begin
    Result := False;
    if RegOpenKeyExView(rvDefault, Rootkey, Subkey, 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      if RegQueryStringValue(K, nil, S) and (PathCompare(Data, S) = 0) then
        Result := True;
      RegCloseKey(K);
    end;
  end;

  function KeyExists(const Rootkey: HKEY; const Subkey: PChar): Boolean;
  var
    K: HKEY;
  begin
    Result := (RegOpenKeyExView(rvDefault, Rootkey, Subkey, 0, KEY_QUERY_VALUE,
      K) = ERROR_SUCCESS);
    if Result then
      RegCloseKey(K);
  end;

  function GetKeyNumSubkeysValues(const Rootkey: HKEY; const Subkey: PChar;
    var NumSubkeys, NumValues: DWORD): Boolean;
  var
    K: HKEY;
  begin
    Result := False;
    if RegOpenKeyExView(rvDefault, Rootkey, Subkey, 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      Result := RegQueryInfoKey(K, nil, nil, nil, @NumSubkeys, nil, nil,
        @NumValues, nil, nil, nil, nil) = ERROR_SUCCESS;
      RegCloseKey(K);
    end;
  end;

  procedure DeleteValue(const Rootkey: HKEY; const Subkey, ValueName: PChar);
  var
    K: HKEY;
  begin
    if RegOpenKeyExView(rvDefault, Rootkey, Subkey, 0, KEY_SET_VALUE, K) = ERROR_SUCCESS then begin
      RegDeleteValue(K, ValueName);
      RegCloseKey(K);
    end;
  end;

var
  SelfName: String;
  NumSubkeys, NumValues: DWORD;
begin
  if not KeyExists(Rootkey, 'Software\Classes\InnoSetupScriptFile') and not KeyExists(Rootkey, 'Software\Classes\.iss') then
    Exit;

  SelfName := NewParamStr(0);

  { NOTE: We can't just blindly delete the entire .iss & InnoSetupScriptFile
    keys, otherwise we'd remove the association even if we weren't the one who
    registered it in the first place. }

  { Clean up 'InnoSetupScriptFile' }
  if KeyValueEquals(Rootkey, 'Software\Classes\InnoSetupScriptFile\DefaultIcon', SelfName + ',1') then
    RegDeleteKeyIncludingSubkeys(rvDefault, Rootkey, 'Software\Classes\InnoSetupScriptFile\DefaultIcon');
  if KeyValueEquals(Rootkey, 'Software\Classes\InnoSetupScriptFile\shell\open\command', '"' + SelfName + '" "%1"') then
    RegDeleteKeyIncludingSubkeys(rvDefault, Rootkey, 'Software\Classes\InnoSetupScriptFile\shell\open');
  if KeyValueEquals(Rootkey, 'Software\Classes\InnoSetupScriptFile\shell\OpenWithInnoSetup\command', '"' + SelfName + '" "%1"') then
    RegDeleteKeyIncludingSubkeys(rvDefault, Rootkey, 'Software\Classes\InnoSetupScriptFile\shell\OpenWithInnoSetup');
  if KeyValueEquals(Rootkey, 'Software\Classes\InnoSetupScriptFile\shell\Compile\command', '"' + SelfName + '" /cc "%1"') then
    RegDeleteKeyIncludingSubkeys(rvDefault, Rootkey, 'Software\Classes\InnoSetupScriptFile\shell\Compile');
  RegDeleteKeyIfEmpty(rvDefault, Rootkey, 'Software\Classes\InnoSetupScriptFile\shell');
  if KeyValueEquals(Rootkey, 'Software\Classes\InnoSetupScriptFile', 'Inno Setup Script') and
     GetKeyNumSubkeysValues(Rootkey, 'Software\Classes\InnoSetupScriptFile', NumSubkeys, NumValues) and
     (NumSubkeys = 0) and (NumValues <= 1) then
    RegDeleteKey(Rootkey, 'Software\Classes\InnoSetupScriptFile');

  { Clean up '.iss' }
  if not KeyExists(Rootkey, 'Software\Classes\InnoSetupScriptFile') and
     KeyValueEquals(Rootkey, 'Software\Classes\.iss', 'InnoSetupScriptFile') then begin
    DeleteValue(Rootkey, 'Software\Classes\.iss', nil);
    DeleteValue(Rootkey, 'Software\Classes\.iss', 'Content Type');
  end;
  RegDeleteKeyIfEmpty(rvDefault, RootKey, 'Software\Classes\.iss');

  if ChangeNotify then
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

procedure UnregisterISSFileAssociation;
begin
  UnregisterISSFileAssociationDo(GetRootkey, True);
end;

end.
