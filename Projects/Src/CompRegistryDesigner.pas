unit CompRegistryDesigner;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Registry Designer form

  Originally contributed by leserg73
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, StdCtrls, TypInfo, UITypes, ExtCtrls;

type
  TRegistryDesignerForm = class(TForm)
    pnl_OKCancel: TPanel;
    Bevel1: TBevel;
    btn_Insert: TButton;
    btn_Cancel: TButton;
    st_Text1: TStaticText;
    edt_PathFileReg: TEdit;
    btn_Browse: TButton;
    gb_Settings: TGroupBox;
    cb_FlagUnInsDelKey: TCheckBox;
    st_uninsdelkey: TStaticText;
    cb_FlagUnInsDelKeyIfEmpty: TCheckBox;
    st_uninsdelkeyifempty: TStaticText;
    cb_FlagDelValue: TCheckBox;
    st_uninsdelvalue: TStaticText;
    cb_CheksIs64bit: TCheckBox;
    rb_Is64BitInstMod: TRadioButton;
    rb_NotIs64BitInstMod: TRadioButton;
    cb_MinVer: TCheckBox;
    st_MinVersion: TStaticText;
    edt_MinVer: TEdit;
    procedure btn_BrowseClick(Sender: TObject);
    procedure btn_InsertClick(Sender: TObject);
    procedure cb_FlagUnInsDelKeyClick(Sender: TObject);
    procedure cb_FlagUnInsDelKeyIfEmptyClick(Sender: TObject);
    procedure cb_CheksIs64bitClick(Sender: TObject);
    procedure cb_MinVerClick(Sender: TObject);
  private
    function GetText: String;
  public
    property Text: string read GetText;
  end;

implementation

uses
  CompMsgs, BrowseFunc, CmnFunc;

{$R *.dfm}

function TRegistryDesignerForm.GetText: String;
type
  TInnoRegData = record
    Root, Subkey, ValueName, ValueData, ValueType: String;
  end;

var
  OutLines: TStringList;
  hexb, hex2, hex7, hex0: Boolean;
  DeleteValue: Boolean;

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
      HKEY_CLASSES_ROOT:   Result := 'HKA'; // HKCR
      HKEY_USERS:          Result := 'HKU';
      HKEY_CURRENT_CONFIG: Result := 'HKCC';
    end;
  end;

  function HexStrToStr(const HexStr: String): String;
  begin
    var StrAsBytes: TBytes;
    SetLength(StrAsBytes, HexStr.Length div 2);
    var i := 1;
    var idx := 0;
    while i <= HexStr.Length do
    begin
      StrAsBytes[idx] := StrToInt('$' + HexStr[i] + HexStr[i + 1]);
      i := i + 2;
      idx := idx + 1;
    end;
    Result := TEncoding.Unicode.GetString(StrAsBytes);
  end;

  function GetValueType(AStr: String): Integer;
  (* "Value A"="<String value data with escape characters>"
     "Value B"=hex:<Binary data (as comma-delimited list of hexadecimal values)>
     "Value C"=dword:<DWORD value integer>
     "Value D"=hex(0):<REG_NONE (as comma-delimited list of hexadecimal values)>
     "Value E"=hex(1):<REG_SZ (as comma-delimited list of hexadecimal values representing a UTF-16LE NUL-terminated string)>
     "Value F"=hex(2):<Expandable string value data (as comma-delimited list of hexadecimal values representing a UTF-16LE NUL-terminated string)>
     "Value G"=hex(3):<Binary data (as comma-delimited list of hexadecimal values)> ; equal to "Value B"
     "Value H"=hex(4):<DWORD value (as comma-delimited list of 4 hexadecimal values, in little endian byte order)>
     "Value I"=hex(5):<DWORD value (as comma-delimited list of 4 hexadecimal values, in big endian byte order)>
     "Value J"=hex(7):<Multi-string value data (as comma-delimited list of hexadecimal values representing UTF-16LE NUL-terminated strings)>
     "Value K"=hex(8):<REG_RESOURCE_LIST (as comma-delimited list of hexadecimal values)>
     "Value L"=hex(a):<REG_RESOURCE_REQUIREMENTS_LIST (as comma-delimited list of hexadecimal values)>
     "Value M"=hex(b):<QWORD value (as comma-delimited list of 8 hexadecimal values, in little endian byte order)> *)
  begin
    Result := 1; // to string data handling - REG_SZ (Default)
    hexb := False;
    hex0 := False;
    hex2 := False;
    hex7 := False;
    DeleteValue := False;
    if Pos('"', AStr) = 0 then
      begin
        // REG_EXPAND_SZ
        if Pos('hex(2):', AStr) <> 0 then
          begin
            hex2 := True;
            Result := 2; // to binary data handling
          end;
        // REG_MULTI_SZ
        if Pos('hex(7):', AStr) <> 0 then
          begin
            hex7 := True;
            Result := 2; // to binary data handling
          end;
        // REG_BINARY
        if Pos('hex:', AStr) <> 0 then
          Result := 2; // to binary data handling
        // REG_QWORD
        if Pos('hex(b):', AStr) <> 0 then
          begin
            hexb := True;
            Result := 3; // to DWORD/QWORD value handling
          end;
        // REG_DWORD
        if Pos('dword:', AStr) <> 0 then
          Result := 3; // to DWORD/DWORD value handling
        // REG_NONE
        if Pos('hex(0):', AStr) <> 0 then
          begin
            hex0 := True;
            Result := 4; // to NONE data handling
          end;
        // REG_NONE AND DELETE Value
        if AStr.StartsWith('-') then
          begin
            DeleteValue := True;
            Result := 4; // to none data handling
          end;
      end;
  end;

  function AddParamToStr(const AStr: String): String;
  begin
    var AParam := AStr;
    if cb_MinVer.Checked then
      AParam := AParam + '; MinVersion: ' + edt_MinVer.Text;
    if cb_CheksIs64bit.Checked then
    begin
      if rb_Is64BitInstMod.Checked then
        AParam := AParam + '; Check: Is64BitInstallMode';
      if rb_NotIs64BitInstMod.Checked then
        AParam := AParam + '; Check: not Is64BitInstallMode';
    end;
    Result := AParam;
  end;

  procedure SubkeyRecord(AReg: TInnoRegData; ADeleteKey: Boolean);
  begin
    var ALine := 'Root: ' + AReg.Root +
                 '; Subkey: ' + AReg.Subkey;
    if ADeleteKey then
      ALine := ALine + '; ValueType: none' +
                       '; Flags: deletekey'
    else begin
      if cb_FlagUnInsDelKey.Checked then
        ALine := ALine + '; Flags: uninsdeletekey';
      if cb_FlagUnInsDelKeyIfEmpty.Checked then
        ALine := ALine + '; Flags: uninsdeletekeyifempty';
    end;
    ALine := AddParamToStr(ALine);
    OutLines.Add(ALine);
  end;

  procedure SubkeyParamRecord(AReg: TInnoRegData; ADeleteValue: Boolean);
  begin
    var ALine := 'Root: ' + AReg.Root +
                 '; Subkey: ' + AReg.Subkey +
                 '; ValueType: ' + AReg.ValueType +
                 '; ValueName: ' + AReg.ValueName;
    if ADeleteValue then
      ALine := ALine + '; Flags: deletevalue'
    else begin
      ALine := ALine + '; ValueData: ' + AReg.ValueData;
      if hex0 then
        ALine := ALine.Replace('; ValueData: ""', '');
      if cb_FlagDelValue.Checked then
        ALine := ALine + '; Flags: uninsdeletevalue';
    end;
    ALine := AddParamToStr(ALine);
    OutLines.Add(ALine);
  end;

begin
  var Lines := TStringList.Create;
  OutLines := TStringList.Create;
  try
    Lines.LoadFromFile(edt_PathFileReg.Text);
    OutLines.Add('; [ BEGIN ] Registry data from file ' + ExtractFileName(edt_PathFileReg.Text));
    var LineIndex := 0;
    while LineIndex <= Lines.Count-1 do
    begin
      var Line := Lines[LineIndex];
      if (Length(Line) > 2) and (Line[1] = '[') and (Line[Line.Length] = ']') then
      begin
        { Got a new root, handle the entire section }
        
        { First set the root and subkey of the new entry }
        Line := CutStrBeginEnd(Line, 1);
        var DeleteKey := Line.StartsWith('-');
        if DeleteKey then
          Line.Remove(1, 1);
        var BackslashPosition := Pos('\', Line);

        var ISRegData: TInnoRegData;
        ISRegData.Root := Copy(Line, 1, BackslashPosition - 1);
        ISRegData.Root := StrRootRename(ISRegData.Root);
        if ISRegData.Root.Contains('HKA') then
          ISRegData.Subkey := 'Software\Classes\'
        else
          ISRegData.Subkey := '';
        ISRegData.Subkey := ISRegData.Subkey + Copy(Line, BackslashPosition + 1, Line.Length);
        ISRegData.Subkey := ISRegData.Subkey.Replace('\WOW6432Node', '')
                                            .Replace('{', '{{')
                                            .QuotedString('"');

        { Go to the first line }
        Inc(LineIndex);
        Line := Lines[LineIndex];

        if (Line = '') and not DeleteKey then
          SubkeyRecord(ISRegData, DeleteKey);
        if (Line = '') or (Line <> '') and DeleteKey then
          SubkeyRecord(ISRegData, DeleteKey);
        
        { Handle first line and next line values - this reuses ISRegData each time }
        while (Line <> '') and not DeleteKey do
        begin
          BackslashPosition := Pos('=', Line);
          ISRegData.ValueName := CutStrBeginEnd(Copy(Line, 1, BackslashPosition - 1), 1);
          ISRegData.ValueName := ISRegData.ValueName.Replace('\\', '\')
                                                    .Replace('{', '{{')
                                                    .QuotedString('"');
          Line := Copy(Line, BackslashPosition + 1, Line.Length);
          case GetValueType(Line) of
            1: begin
                 // REG_SZ
                 ISRegData.ValueData := CutStrBeginEnd(Line, 1);
                 ISRegData.ValueData := ISRegData.ValueData.Replace('\\', '\')
                                                           .Replace('{', '{{')
                                                           .QuotedString('"');
                 ISRegData.ValueType := 'string';
               end;
            2: begin
                 // REG_BINARY
                 BackslashPosition := Pos(':', Line);
                 Line := Copy(Line, BackslashPosition + 1, Line.Length);
                 ISRegData.ValueData := Line;
                 // Multiline binary
                 if Line[Line.Length] = '\' then
                   ISRegData.ValueData := Copy(ISRegData.ValueData, 1, ISRegData.ValueData.Length - 1);
                 while Line[Line.Length] = '\' do
                 begin
                   Inc(LineIndex);
                   Line := Lines[LineIndex];
                   if Line[Line.Length] = '\' then
                     ISRegData.ValueData := ISRegData.ValueData + Copy(Line, 1, Line.Length - 1).TrimLeft
                   else
                     ISRegData.ValueData := ISRegData.ValueData + Copy(Line, 1, Line.Length).TrimLeft;
                 end;
                 ISRegData.ValueData := ISRegData.ValueData.Replace(',', ' ');
                 if hex2 or hex7 then
                   begin
                     ISRegData.ValueData := ISRegData.ValueData.Replace(' ', '');
                     ISRegData.ValueData := HexStrToStr(ISRegData.ValueData);
                   end;
                 if hex2 then
                   // REG_EXPAND_SZ
                   begin
                     ISRegData.ValueData := ISRegData.ValueData.Replace(#0, '');
                     ISRegData.ValueType := 'expandsz';
                   end
                 else if hex7 then
                   // REG_MULTI_SZ
                   begin
                     ISRegData.ValueData := ISRegData.ValueData.Replace(#0, '{break}');
                     ISRegData.ValueType := 'multisz';
                   end
                 else
                   // REG_BINARY
                   ISRegData.ValueType := 'binary';
                 ISRegData.ValueData := ISRegData.ValueData.QuotedString('"');
               end;
            3: begin
                 BackslashPosition := Pos(':', Line);
                 ISRegData.ValueData := Copy(Line, BackslashPosition + 1, Line.Length);
                 if hexb then
                   // REG_QWORD
                   begin
                     Line := ISRegData.ValueData.Replace(',', '');
                     ISRegData.ValueData := '';
                     for var j := 0 to Line.Length div 2 do
                       ISRegData.ValueData := Copy(Line, (j * 2) + 1, 2) + ISRegData.ValueData;
                     ISRegData.ValueType := 'qword';
                   end
                 else
                   // REG_DWORD
                   ISRegData.ValueType := 'dword';
                 ISRegData.ValueData := '$' + ISRegData.ValueData;
               end;
            4: begin
                 // REG_NONE
                 ISRegData.ValueType := 'none';
                 ISRegData.ValueData := '""';
               end;
          end;
          SubkeyParamRecord(ISRegData, DeleteValue);
          
          { Go to the next line }
          Inc(LineIndex);
          Line := Lines[LineIndex];
        end;
      end;
      Inc(LineIndex);
    end;
    OutLines.Add('; [ END ]');
    Result := OutLines.Text;
  finally
    Lines.Free;
    OutLines.Free;
  end;
end;

procedure TRegistryDesignerForm.btn_BrowseClick(Sender: TObject);
begin
  var FilePath := '';
  if NewGetOpenFileName('', FilePath, '', SWizardAppRegFilter, SWizardAppRegDefaultExt, Handle) then begin
    var FileCheck := TStringList.Create;
    try
      FileCheck.LoadFromFile(FilePath);
      const HeaderReg = 'Windows Registry Editor Version 5.00'; { don't localize }
      if (FileCheck[0] = HeaderReg) and (FileCheck[FileCheck.Count - 1] = '') then
        begin
          edt_PathFileReg.Text := FilePath;
        end
      else
        MsgBox('Invalid file format, select another one.', SCompilerFormCaption, mbError, MB_OK);
    finally
       FileCheck.Free;
    end;
  end;
end;

procedure TRegistryDesignerForm.btn_InsertClick(Sender: TObject);
begin
  if not FileExists(edt_PathFileReg.Text) then
    ModalResult := mrCancel;
end;

procedure TRegistryDesignerForm.cb_FlagUnInsDelKeyClick(Sender: TObject);
begin
  if cb_FlagUnInsDelKey.Checked then
    cb_FlagUnInsDelKeyIfEmpty.Checked := False;
end;

procedure TRegistryDesignerForm.cb_FlagUnInsDelKeyIfEmptyClick(Sender: TObject);
begin
  if cb_FlagUnInsDelKeyIfEmpty.Checked then
    cb_FlagUnInsDelKey.Checked := False;
end;

procedure TRegistryDesignerForm.cb_CheksIs64bitClick(Sender: TObject);
begin
  if cb_CheksIs64bit.Checked then
    begin
      rb_Is64BitInstMod.Enabled := True;
      rb_NotIs64BitInstMod.Enabled := True;
    end
  else
    begin
      rb_Is64BitInstMod.Enabled := False;
      rb_NotIs64BitInstMod.Enabled := False;
      rb_Is64BitInstMod.Checked := False;
      rb_NotIs64BitInstMod.Checked := False;
    end
end;

procedure TRegistryDesignerForm.cb_MinVerClick(Sender: TObject);
begin
  if cb_MinVer.Checked then
    begin
      st_MinVersion.Enabled := True;
      edt_MinVer.Enabled := True;
    end
  else
    begin
      st_MinVersion.Enabled := False;
      edt_MinVer.Enabled := False;
    end
end;

end.
