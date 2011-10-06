unit CompFileAssoc;

{
  Inno Setup
  Copyright (C) 1997-2005 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Functions for registering/unregistering the .iss file association

  $jrsoftware: issrc/Projects/CompFileAssoc.pas,v 1.13 2009/04/21 13:46:04 mlaan Exp $
}

interface

procedure RegisterISSFileAssociation;
procedure UnregisterISSFileAssociation;

implementation

uses
  Windows, SysUtils, PathFunc, ShlObj, CmnFunc2;

procedure RegisterISSFileAssociation;

  procedure SetKeyValue(const Subkey, ValueName: PChar; const Data: String);

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
    Check(RegCreateKeyExView(rvDefault, HKEY_CLASSES_ROOT, Subkey, 0, nil, 0, KEY_SET_VALUE,
      nil, K, @Disp));
    try
      Check(RegSetValueEx(K, ValueName, 0, REG_SZ, PChar(Data), (Length(Data)+1)*SizeOf(Data[1])));
    finally
      RegCloseKey(K);
    end;
  end;

var
  SelfName: String;
begin
  SelfName := NewParamStr(0);

  SetKeyValue('.iss', nil, 'InnoSetupScriptFile');
  SetKeyValue('.iss', 'Content Type', 'text/plain');

  SetKeyValue('InnoSetupScriptFile', nil, 'Inno Setup Script');
  SetKeyValue('InnoSetupScriptFile\DefaultIcon', nil, SelfName + ',1');
  SetKeyValue('InnoSetupScriptFile\shell\open\command', nil,
    '"' + SelfName + '" "%1"');
  SetKeyValue('InnoSetupScriptFile\shell\OpenWithInnoSetup', nil,
    'Open with &Inno Setup');
  SetKeyValue('InnoSetupScriptFile\shell\OpenWithInnoSetup\command', nil,
    '"' + SelfName + '" "%1"');
  SetKeyValue('InnoSetupScriptFile\shell\Compile', nil, 'Compi&le');
  SetKeyValue('InnoSetupScriptFile\shell\Compile\command', nil,
    '"' + SelfName + '" /cc "%1"');

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

procedure UnregisterISSFileAssociation;

  function KeyValueEquals(const Subkey: PChar; const Data: String): Boolean;
  var
    K: HKEY;
    S: String;
  begin
    Result := False;
    if RegOpenKeyExView(rvDefault, HKEY_CLASSES_ROOT, Subkey, 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      if RegQueryStringValue(K, nil, S) and (PathCompare(Data, S) = 0) then
        Result := True;
      RegCloseKey(K);
    end;
  end;

  function KeyExists(const Subkey: PChar): Boolean;
  var
    K: HKEY;
  begin
    Result := (RegOpenKeyExView(rvDefault, HKEY_CLASSES_ROOT, Subkey, 0, KEY_QUERY_VALUE,
      K) = ERROR_SUCCESS);
    if Result then
      RegCloseKey(K);
  end;

  function GetKeyNumSubkeysValues(const Subkey: PChar;
    var NumSubkeys, NumValues: DWORD): Boolean;
  var
    K: HKEY;
  begin
    Result := False;
    if RegOpenKeyExView(rvDefault, HKEY_CLASSES_ROOT, Subkey, 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      Result := RegQueryInfoKey(K, nil, nil, nil, @NumSubkeys, nil, nil,
        @NumValues, nil, nil, nil, nil) = ERROR_SUCCESS;
      RegCloseKey(K);
    end;
  end;

  procedure DeleteValue(const Subkey, ValueName: PChar);
  var
    K: HKEY;
  begin
    if RegOpenKeyExView(rvDefault, HKEY_CLASSES_ROOT, Subkey, 0, KEY_SET_VALUE, K) = ERROR_SUCCESS then begin
      RegDeleteValue(K, ValueName);
      RegCloseKey(K);
    end;
  end;

var
  SelfName: String;
  NumSubkeys, NumValues: DWORD;
begin
  if not KeyExists('InnoSetupScriptFile') and not KeyExists('.iss') then
    Exit;

  SelfName := NewParamStr(0);

  { NOTE: We can't just blindly delete the entire .iss & InnoSetupScriptFile
    keys, otherwise we'd remove the association even if we weren't the one who
    registered it in the first place. }

  { Clean up 'InnoSetupScriptFile' }
  if KeyValueEquals('InnoSetupScriptFile\DefaultIcon', SelfName + ',1') then
    RegDeleteKeyIncludingSubkeys(rvDefault, HKEY_CLASSES_ROOT, 'InnoSetupScriptFile\DefaultIcon');
  if KeyValueEquals('InnoSetupScriptFile\shell\open\command', '"' + SelfName + '" "%1"') then
    RegDeleteKeyIncludingSubkeys(rvDefault, HKEY_CLASSES_ROOT, 'InnoSetupScriptFile\shell\open');
  if KeyValueEquals('InnoSetupScriptFile\shell\OpenWithInnoSetup\command', '"' + SelfName + '" "%1"') then
    RegDeleteKeyIncludingSubkeys(rvDefault, HKEY_CLASSES_ROOT, 'InnoSetupScriptFile\shell\OpenWithInnoSetup');
  if KeyValueEquals('InnoSetupScriptFile\shell\Compile\command', '"' + SelfName + '" /cc "%1"') then
    RegDeleteKeyIncludingSubkeys(rvDefault, HKEY_CLASSES_ROOT, 'InnoSetupScriptFile\shell\Compile');
  RegDeleteKeyIfEmpty(rvDefault, HKEY_CLASSES_ROOT, 'InnoSetupScriptFile\shell');
  if KeyValueEquals('InnoSetupScriptFile', 'Inno Setup Script') and
     GetKeyNumSubkeysValues('InnoSetupScriptFile', NumSubkeys, NumValues) and
     (NumSubkeys = 0) and (NumValues <= 1) then
    RegDeleteKey(HKEY_CLASSES_ROOT, 'InnoSetupScriptFile');

  { Clean up '.iss' }
  if not KeyExists('InnoSetupScriptFile') and
     KeyValueEquals('.iss', 'InnoSetupScriptFile') then begin
    DeleteValue('.iss', nil);
    DeleteValue('.iss', 'Content Type');
  end;
  RegDeleteKeyIfEmpty(rvDefault, HKEY_CLASSES_ROOT, '.iss');

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

end.
