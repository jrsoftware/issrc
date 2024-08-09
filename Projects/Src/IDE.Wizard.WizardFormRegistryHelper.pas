unit IDE.Wizard.WizardFormRegistryHelper;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Helper to avoid duplicate code between WizardForm and RegistryDesignerForm
}

interface

uses
  Forms, StdCtrls, ExtCtrls;

type
  TPrivilegesRequired = (prAdmin, prLowest, prDynamic);

  TWizardFormRegistryHelper = class
    private
      FForm: TForm;
      FFileEdit: TEdit;
      FUninsDeleteKeyCheck, FUninsDeleteKeyIfEmptyCheck,
      FUninsDeleteValueCheck, FMinVerCheck: TCheckBox;
      FMinVerEdit: TEdit;
      FMinVerDocImage: TImage;
      FPrivilegesRequired: TPrivilegesRequired;
      procedure SetPrivilegesRequired(const Value: TPrivilegesRequired);
      procedure UpdateImages;
      procedure AfterMonitorDpiChanged(Sender: TObject; OldDPI: Integer; NewDPI: Integer);
      procedure FileButtonClick(Sender: TObject);
      procedure UninsDeleteKeyIfEmptyCheckClick(Sender: TObject);
      procedure MinVerCheckClick(Sender: TObject);
      procedure MinVerDocImageClick(Sender: TObject);
    public
      constructor Create(const Form: TForm; const FileEdit: TEdit;
        const FileButton: TButton; const UninsDeleteKeyCheck,
        UninsDeleteKeyIfEmptyCheck, UninsDeleteValueCheck, MinVerCheck: TCheckBox;
        const MinVerEdit: TEdit; const MinVerDocImage: TImage);
      procedure AddScript(var Registry: String; const AllowException: Boolean);
      property PrivilegesRequired: TPrivilegesRequired write SetPrivilegesRequired;
    end;

implementation

uses
  Windows, Classes, SysUtils, StrUtils, TypInfo, Graphics, UITypes,
  ComCtrls, BrowseFunc,
  IDE.MainForm, IDE.ImagesModule, IDE.HelperFunc, IDE.Messages, Shared.CommonFunc, IDE.HtmlHelpFunc;

{ TWizardFormRegistryHelper }

procedure TWizardFormRegistryHelper.SetPrivilegesRequired(
  const Value: TPrivilegesRequired);
begin
  FPrivilegesRequired := Value;
end;

procedure TWizardFormRegistryHelper.UpdateImages;

  function GetImage(const Button: TToolButton; const WH: Integer): TWICImage;
  begin
    Result := ImagesModule.LightToolBarImageCollection.GetSourceImage(Button.ImageIndex, WH, WH)
  end;

begin
 { After a DPI change the button's Width and Height isn't yet updated, so calculate it ourselves }
  var WH := MulDiv(16, FForm.CurrentPPI, 96);
  FMinVerDocImage.Picture.Graphic:= GetImage(MainForm.HelpButton, WH);
end;

constructor TWizardFormRegistryHelper.Create(const Form: TForm;
  const FileEdit: TEdit; const FileButton: TButton; const UninsDeleteKeyCheck,
  UninsDeleteKeyIfEmptyCheck, UninsDeleteValueCheck, MinVerCheck: TCheckBox;
  const MinVerEdit: TEdit; const MinVerDocImage: TImage);
begin
  FForm := Form;
  FFileEdit := FileEdit;
  FUninsDeleteKeyCheck := UninsDeleteKeyCheck;
  FUninsDeleteKeyIfEmptyCheck := UninsDeleteKeyIfEmptyCheck;
  FUninsDeleteValueCheck := UninsDeleteValueCheck;
  FMinVerCheck := MinVerCheck;
  FMinVerEdit := MinVerEdit;
  FMinVerDocImage := MinVerDocImage;

  FileButton.OnClick := FileButtonClick;
  UninsDeleteKeyIfEmptyCheck.OnClick := UninsDeleteKeyIfEmptyCheckClick;
  MinVerCheck.OnClick := MinVerCheckClick;
  MinVerCheck.OnClick(nil);
  MinVerDocImage.OnClick := MinVerDocImageClick;
  MinVerDocImage.Cursor := crHandPoint;

  TryEnableAutoCompleteFileSystem(FileEdit.Handle);

  Form.OnAfterMonitorDpiChanged := AfterMonitorDpiChanged;
  UpdateImages;
end;

procedure TWizardFormRegistryHelper.AfterMonitorDpiChanged(Sender: TObject; OldDPI: Integer; NewDPI: Integer);
begin
  UpdateImages;
end;

procedure TWizardFormRegistryHelper.FileButtonClick(Sender: TObject);
begin
  var FileName: String := FFileEdit.Text;
  if NewGetOpenFileName('', FileName, '', SWizardAppRegFilter, SWizardAppRegDefaultExt, FForm.Handle) then
    FFileEdit.Text := FileName;
end;

procedure TWizardFormRegistryHelper.UninsDeleteKeyIfEmptyCheckClick(Sender: TObject);
begin
  FUninsDeleteKeyCheck.Enabled := FUninsDeleteKeyIfEmptyCheck.Checked;
  if not FUninsDeleteKeyCheck.Enabled then
    FUninsDeleteKeyCheck.Checked := False;
end;

procedure TWizardFormRegistryHelper.MinVerCheckClick(Sender: TObject);
begin
  FMinVerEdit.Enabled := FMinVerCheck.Checked;
  FMinVerDocImage.Visible := FMinVerCheck.Checked;
  if FMinVerEdit.Enabled then
    FForm.ActiveControl := FMinVerEdit;
end;

procedure TWizardFormRegistryHelper.MinVerDocImageClick(Sender: TObject);
begin
  if Assigned(HtmlHelp) then
    HtmlHelp(GetDesktopWindow, PChar(GetHelpFile), HH_DISPLAY_TOPIC, Cardinal(PChar('topic_winvernotes.htm')));
end;

procedure TWizardFormRegistryHelper.AddScript(var Registry: String;
  const AllowException: Boolean);

  function NextLine(const Lines: TStrings; var LineIndex: Integer): String;
  begin
    Inc(LineIndex);
    if LineIndex < Lines.Count then
      Result := Lines[LineIndex]
    else
      Result := ''; { Official .reg files must end with a blank line so should never get here but we support ones without }
  end;

  function CutStrBeginEnd(S: String; CharCount: Integer): String;
  begin
    Result := Copy(S, CharCount + 1, S.Length - 2 * CharCount);
  end;

  function StrRootRename(S: String): String;
  type
    TStrings = (HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE, HKEY_CLASSES_ROOT, HKEY_USERS, HKEY_CURRENT_CONFIG);
  begin
    var ARoot := TStrings(GetEnumValue(TypeInfo(TStrings), S));
    case ARoot of
      HKEY_CURRENT_USER:   Result := 'HKCU';
      HKEY_LOCAL_MACHINE:  Result := 'HKLM';
      HKEY_CLASSES_ROOT:   Result := 'HKCR';
      HKEY_USERS:          Result := 'HKU';
      HKEY_CURRENT_CONFIG: Result := 'HKCC';
    else
      raise Exception.CreateFmt('Unknown root %s', [S]);
    end;
  end;

  function UTF16LEHexStrToStr(HexStr: String): String;
  begin
    if HexStr.Length mod 4 <> 0 then
      HexStr := HexStr + '00'; { RegEdit does this as well on import }
    var UTF16LEBytes: TBytes;
    SetLength(UTF16LEBytes, HexStr.Length div 2);
    var i := 1;
    var idx := 0;
    while i <= HexStr.Length do
    begin
      UTF16LEBytes[idx] := StrToInt('$' + HexStr[i] + HexStr[i + 1]);
      i := i + 2;
      idx := idx + 1;
    end;
    Result := TEncoding.Unicode.GetString(UTF16LEBytes);
  end;

  type
    TValueType = (vtSz, vtSzAsList, vtExpandSz, vtMultiSz, vtBinary, vtDWord, vtDWordAsList, vtQWord, vtNone, vtDelete, vtUnsupported);

  function GetValueType(AStr: String): TValueType;
  { See https://en.wikipedia.org/wiki/Windows_Registry#.REG_files

    Value formats: (we don't support I/K/L and just ignore those)

    "Value A"="<REG_SZ String value data with escape characters>"
    "Value B"=hex:<REG_BINARY Binary data (as comma-delimited list of hexadecimal values)>
    "Value C"=dword:<REG_DWORD DWORD value integer>
    "Value D"=hex(0):<REG_NONE (as comma-delimited list of hexadecimal values)>
    "Value E"=hex(1):<REG_SZ (as comma-delimited list of hexadecimal values representing a UTF-16LE NUL-terminated string)>
    "Value F"=hex(2):<REG_EXPAND_SZ Expandable string value data  (as comma-delimited list of hexadecimal values representing a UTF-16LE NUL-terminated string)>
    "Value G"=hex(3):<REG_BINARY Binary data (as comma-delimited list of hexadecimal values)> ; equal to "Value B"
    "Value H"=hex(4):<REG_DWORD DWORD value (as comma-delimited list of 4 hexadecimal values, in little endian byte order)>
    "Value I"=hex(5):<REG_DWORD_BIG_ENDIAN DWORD value (as comma-delimited list of 4 hexadecimal values, in big endian byte order)>
    "Value J"=hex(7):<RED_MULTISZ Multi-string value data (as comma-delimited list of hexadecimal values representing UTF-16LE NUL-terminated strings)>
    "Value K"=hex(8):<REG_RESOURCE_LIST (as comma-delimited list of hexadecimal values)>
    "Value L"=hex(a):<REG_RESOURCE_REQUIREMENTS_LIST (as comma-delimited list of hexadecimal values)>
    "Value M"=hex(b):<REG_QWORD QWORD value (as comma-delimited list of 8 hexadecimal values, in little endian byte order)>

    Other notes from the article:
    To remove a key (and all subkeys, values and data), the key name must be preceded by a minus sign ("-")
    To remove a value (and its data), the values to be removed must have a minus sign ("-") after the equal sign ("=")
    The Default Value of a key can be edited by using "@" instead of "Value Name"
    Lines beginning with a semicolon are considered comments

    BTW: Missing from the article is a note about multiline lists, these use "\" to continue }
  begin
    if Pos('"', AStr) <> 0 then
      Result := vtSz //Value A
    else if (Pos('hex:', AStr) <> 0) or
            (Pos('hex(3):', AStr) <> 0) then
      Result := vtBinary //Value B or G
    else if Pos('dword:', AStr) <> 0 then
      Result := vtDWord //Value C
    else if Pos('hex(0):', AStr) <> 0 then
      Result := vtNone //Value D
    else if Pos('hex(1):', AStr) <> 0 then
      Result := vtSzAsList //Value E
    else if Pos('hex(2):', AStr) <> 0 then
      Result := vtExpandSz //Value F
    else if Pos('hex(4):', AStr) <> 0 then
      Result := vtDWordAsList //Value H
    else if Pos('hex(7):', AStr) <> 0 then
      Result := vtMultiSz //Value J
    else if Pos('hex(b):', AStr) <> 0 then
      Result := vtQWord //Value M
    else if AStr.StartsWith('-') then
      Result := vtDelete
    else
      Result := vtUnsupported;
  end;

  type
    TRegistryEntry = record
      Root, Subkey, ValueName, ValueData, ValueType: String;
    end;

  function RequiresAdminInstallMode(AEntry: TRegistryEntry): Boolean;
  begin
    Result := (AEntry.Root = 'HKLM') or (AEntry.Root = 'HKCC') or
              ((AEntry.Root = 'HKU') and SameText(AEntry.Subkey, '.Default'));
  end;

   function RequiresNotAdminInstallMode(AEntry: TRegistryEntry): Boolean;
  begin
    Result := (AEntry.Root = 'HKCU');
  end;

  function TextCommon(AEntry: TRegistryEntry): String;
  begin
    Result := '';
    if FMinVerCheck.Checked and (FMinVerEdit.Text <> '') then
      Result := Result + '; MinVersion: ' + FMinVerEdit.Text;
    if (FPrivilegesRequired <> prAdmin) and RequiresAdminInstallMode(AEntry) then
      Result := Result + '; Check: IsAdminInstallMode'
    else if (FPrivilegesRequired <> prLowest) and RequiresNotAdminInstallMode(AEntry) then
      Result := Result + '; Check: not IsAdminInstallMode';
  end;

  function TextKeyEntry(AEntry: TRegistryEntry; ADeleteKey: Boolean): String;
  begin
    Result := 'Root: ' + AEntry.Root +
              '; Subkey: ' + AEntry.Subkey;
    if ADeleteKey then
      Result := Result + '; ValueType: none' +
                         '; Flags: deletekey'
    else begin
      if FUninsDeleteKeyCheck.Checked then
        Result := Result + '; Flags: uninsdeletekey'
      else if FUninsDeleteKeyIfEmptyCheck.Checked then
        Result := Result + '; Flags: uninsdeletekeyifempty';
    end;
    Result := Result + TextCommon(AEntry);
  end;

  function TextValueEntry(AEntry: TRegistryEntry; AValueType: TValueType): String;
  begin
    Result := 'Root: ' + AEntry.Root +
              '; Subkey: ' + AEntry.Subkey +
              '; ValueType: ' + AEntry.ValueType +
              '; ValueName: ' + AEntry.ValueName;
    if AValueType = vtDelete then
      Result := Result + '; Flags: deletevalue'
    else begin
      if AValueType <> vtNone then
        Result := Result + '; ValueData: ' + AEntry.ValueData;
      if FUninsDeleteValueCheck.Checked then
        Result := Result + '; Flags: uninsdeletevalue';
    end;
    Result := Result + TextCommon(AEntry);
  end;

  function TextHeader: String;
  begin
    Result := ';Registry data from file ' + ExtractFileName(FFileEdit.Text);
  end;

  function TextBadHeader: String;
  begin
    Result := ';COULD NOT IMPORT ' + ExtractFileName(FFileEdit.Text);
  end;

  function TextFooter(const HadFilteredKeys, HadUnsupportedValueTypes: Boolean): String;
  begin
    Result := ';End of registry data from file ' + ExtractFileName(FFileEdit.Text);
    if HadFilteredKeys then
      Result := Result + SNewLine + ';SOME KEYS FILTERED DUE TO PRIVILEGESREQUIRED SETTINGS!';
    if HadUnsupportedValueTypes then
      Result := Result + SNewLine + ';SOME VALUES WITH UNSUPPORTED TYPES SKIPPED!'
  end;

begin
  if FFileEdit.Text = '' then
    Exit;

  var Lines := TStringList.Create;
  var OutLines := TStringList.Create;
  try
    Lines.LoadFromFile(FFileEdit.Text);

    { Official .reg files must have blank lines as second and last lines but we
      don't require that so we just check for the header on the first line }
    const Header = 'Windows Registry Editor Version 5.00'; { don't localize }
    if (Lines.Count = 0) or (Lines[0] <> Header) then begin
      if AllowException then
        raise Exception.Create('Invalid file format.')
      else begin
        Registry := Registry + TextBadHeader + SNewLine;
        Exit;
      end;
    end;

    var LineIndex := 1;
    var HadFilteredKeys := False;
    var HadUnsupportedValueTypes := False;
    while LineIndex <= Lines.Count-1 do
    begin
      var Line := Lines[LineIndex];
      if (Length(Line) > 2) and (Line[1] = '[') and (Line[Line.Length] = ']') then
      begin
        { Got a new section, first handle the key }
        Line := CutStrBeginEnd(Line, 1);
        var DeleteKey := Line.StartsWith('-');
        if DeleteKey then
          Delete(Line, 1, 1);
        var P := Pos('\', Line);

        var Entry: TRegistryEntry;
        Entry.Root := StrRootRename(Copy(Line, 1, P - 1));
        Entry.Subkey := Copy(Line, P + 1, MaxInt);
        if Entry.Root = 'HKCR' then begin
          Entry.Root := 'HKA';
          Entry.Subkey := 'Software\Classes\' + Entry.Subkey;
        end;
        Entry.Subkey := Entry.Subkey.Replace('\WOW6432Node', '')
                                            .Replace('{', '{{')
                                            .QuotedString('"');

        var FilterKey := ((FPrivilegesRequired = prAdmin) and RequiresNotAdminInstallMode(Entry)) or
                         ((FPrivilegesRequired = prLowest) and RequiresAdminInstallMode(Entry));

        if not FilterKey then
          OutLines.Add(TextKeyEntry(Entry, DeleteKey))
        else
          HadFilteredKeys := True;

        { Key done, handle values }
        Line := NextLine(Lines, LineIndex);

        while Line <> '' do begin
          if not FilterKey and not DeleteKey and (Line[1] <> ';') then begin
            P := Pos('=', Line);
            if (P = 2) and (Line[1] = '@') then
              Entry.ValueName := '""'
            else begin
              Entry.ValueName := CutStrBeginEnd(Copy(Line, 1, P - 1), 1);
              Entry.ValueName := Entry.ValueName.Replace('\\', '\')
                                                .Replace('{', '{{')
                                                .QuotedString('"');
            end;
            var ValueTypeAndData := Copy(Line, P + 1, MaxInt);
            var ValueType := GetValueType(ValueTypeAndData);
            case ValueType of
              vtSz:
                begin
                  Entry.ValueData := CutStrBeginEnd(ValueTypeAndData, 1);
                  Entry.ValueData := Entry.ValueData.Replace('\\', '\')
                                                    .Replace('{', '{{')
                                                    .QuotedString('"');
                  Entry.ValueType := 'string';
                end;
              vtSzAsList, vtExpandSz, vtMultiSz, vtBinary:
                begin
                  P := Pos(':', ValueTypeAndData);
                  var ValueData := Copy(ValueTypeAndData, P + 1, MaxInt);

                  var HasMoreLines := ValueData[ValueData.Length] = '\';
                  if HasMoreLines then
                    Delete(ValueData, ValueData.Length, 1);
                  Entry.ValueData := ValueData;

                  while HasMoreLines do
                  begin
                    ValueData := NextLine(Lines, LineIndex).TrimLeft;
                    HasMoreLines := ValueData[ValueData.Length] = '\';
                    if HasMoreLines then
                      Delete(ValueData, ValueData.Length, 1);
                    Entry.ValueData := Entry.ValueData + ValueData;
                  end;

                  Entry.ValueData := Entry.ValueData.Replace(',', ' ');
                  if ValueType <> vtBinary then
                  begin
                    Entry.ValueData := Entry.ValueData.Replace(' ', '');
                    Entry.ValueData := UTF16LEHexStrToStr(Entry.ValueData);
                  end;

                  if ValueType in [vtSzAsList, vtExpandSz] then
                  begin
                    Entry.ValueData := Entry.ValueData.Replace(#0, '');
                    Entry.ValueType := IfThen(ValueType = vtSzAsList, 'string', 'expandsz');
                  end else if ValueType = vtMultiSz then
                  begin
                    Entry.ValueData := Entry.ValueData.Replace(#0, '{break}');
                    Entry.ValueType := 'multisz';
                  end else
                    Entry.ValueType := 'binary';

                  Entry.ValueData := Entry.ValueData.QuotedString('"');
               end;
              vtDWord, vtDWordAsList, vtQWord:
                begin
                  P := Pos(':', ValueTypeAndData);
                  Entry.ValueData := Copy(ValueTypeAndData, P + 1, MaxInt);

                  if ValueType in [vtDWordAsList, vtQWord] then
                  begin
                    { ValueData is in reverse order, fix this }
                    var ReverseValueData := Entry.ValueData.Replace(',', '');
                    Entry.ValueData := '';
                    for var I := 0 to ReverseValueData.Length div 2 do
                      Entry.ValueData := Copy(ReverseValueData, (I * 2) + 1, 2) + Entry.ValueData;

                    Entry.ValueType := IfThen(ValueType = vtDWordAsList, 'dword', 'qword');
                  end else
                    Entry.ValueType := 'dword';

                  Entry.ValueData := '$' + Entry.ValueData;
                end;
              vtNone, vtDelete:
                begin
                  Entry.ValueType := 'none';
                  Entry.ValueData := ''; { value doesn't matter }
                end;
            end;

            if ValueType <> vtUnsupported then
              OutLines.Add(TextValueEntry(Entry, ValueType))
            else
              HadUnsupportedValueTypes := True;
          end;

          Line := NextLine(Lines, LineIndex); { Go to the next line - should be the next value or a comment }
        end; { Out of values }
      end;
      Inc(LineIndex); { Go to the next line - should be the next key section or a comment }
    end;
    OutLines.Insert(0, TextHeader);
    OutLines.Add(TextFooter(HadFilteredKeys, HadUnsupportedValueTypes));
    Registry := Registry + OutLines.Text;
  finally
    Lines.Free;
    OutLines.Free;
  end;
end;

end.
