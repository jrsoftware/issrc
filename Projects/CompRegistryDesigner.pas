unit CompRegistryDesigner;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, TypInfo, UITypes, Vcl.ExtCtrls;

type
  TRegistryDesignerForm = class(TForm)
    OpenDialog1: TOpenDialog;
    pnl_OKCancel: TPanel;
    Bevel1: TBevel;
    btn_Insert: TButton;
    btn_Cancel: TButton;
    st_Text1: TStaticText;
    edt_PathFileReg: TEdit;
    btn_Browse: TButton;
    gb_Settings: TGroupBox;
    cb_FlagSubKey: TCheckBox;
    cb_FlagDelValue: TCheckBox;
    cb_CheksOnOff: TCheckBox;
    rb_IsWin64: TRadioButton;
    rb_NotIsWin64: TRadioButton;
    rb_IsAdmMod: TRadioButton;
    rb_NotIsAdmMod: TRadioButton;
    cb_MinVer: TCheckBox;
    edt_MinVer: TEdit;
    Panel1: TPanel;
    rb_FlagDelKey: TRadioButton;
    rb_FlagDelKeyIfEmpty: TRadioButton;
    st_uninsdeletevalue: TStaticText;
    st_MinVersion: TStaticText;
    procedure btn_BrowseClick(Sender: TObject);
    procedure btn_InsertClick(Sender: TObject);
    procedure cb_FlagDelValueClick(Sender: TObject);
    procedure cb_FlagSubKeyClick(Sender: TObject);
    procedure cb_CheksOnOffClick(Sender: TObject);
    procedure cb_MinVerClick(Sender: TObject);
    procedure rb_FlagDelKeyClick(Sender: TObject);
    procedure rb_FlagDelKeyIfEmptyClick(Sender: TObject);
    procedure rb_IsWin64Click(Sender: TObject);
    procedure rb_NotIsWin64Click(Sender: TObject);
    procedure rb_IsAdmModClick(Sender: TObject);
    procedure rb_NotIsAdmModClick(Sender: TObject);
  private
    function GetText: String;
  public
    property Text: string read GetText;
  end;

var
  AddRegDataForm: TRegistryDesignerForm;

implementation

uses
  CompMsgs;

{$R *.dfm}

function TRegistryDesignerForm.GetText: String;
type
  TInnoRegData = record
    Root, SubKey, ValueName, ValueData, ValueType: String;
  end;

const
  RemarkaLine = '; [ BEGIN ] Registry data from file ';
  RemarkaEND = '; [ END ]';

var
  InBuffer, OutBuffer: TStringList;
  InBufStr: String;
  i, j, poschar: Integer;
  ISRegData: TInnoRegData;
  hexb, hex2, hex7, hex0: Boolean;
  delkey, delval: Boolean;

  function StrIsRoot(S: String): Boolean;
  begin
    Result := False;
    if S <> '' then
      Result := (S[1] = '[') and (S[S.Length] = ']')
  end;

  function CutStrBeginEnd(S: String; CharCount: Integer): String;
  begin
    Result := Copy(S, CharCount + 1, S.Length);
    Result := Copy(Result, 1, S.Length - CharCount - 1);
  end;

  function DelSpaceChar(S: String): String;
  var
    i: Integer;
  begin
    Result := '';
    for i := 1 to S.Length do
      if S[i] <> ' ' then
        Result := Result + S[i]
  end;

  function StrRootRename(S: String): String;
  type
    TStrings = (HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE, HKEY_CLASSES_ROOT, HKEY_USERS, HKEY_CURRENT_CONFIG);
  var
    ARoot: TStrings;
  begin
    ARoot := TStrings(GetEnumValue(TypeInfo(TStrings), S));
    case ARoot of
      HKEY_CURRENT_USER:   Result := 'HKCU';
      HKEY_LOCAL_MACHINE:  Result := 'HKLM';
      HKEY_CLASSES_ROOT:   Result := 'HKCR';
      HKEY_USERS:          Result := 'HKU';
      HKEY_CURRENT_CONFIG: Result := 'HKCC';
    end;
  end;

  function HexStrToStr(const HexStr: String): String;
  var
    i, idx: Cardinal;
    StrAsBytes: TBytes;
  begin
    SetLength(StrAsBytes, HexStr.Length div 2);
    i := 1;
    idx := 0;
    while i <= HexStr.Length do
    begin
      StrAsBytes[idx] := StrToInt('$' + HexStr[i] + HexStr[i + 1]);
      i := i + 2;
      idx := idx + 1;
    end;
    Result := TEncoding.Unicode.GetString(StrAsBytes);
  end;

  function getValueType(AStr: String): Integer;
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
    delval := False;
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
            delval := True;
            Result := 4; // to none data handling
          end;
      end;
  end;

  function AddParamToStr(const AStr: String): String;
  var
   AParam: String;
  begin
    AParam := AStr;
    if cb_MinVer.Checked then
      AParam := AParam + '; MinVersion: ' + edt_MinVer.Text;
    if cb_CheksOnOff.Checked then
      begin
        if rb_IsWin64.Checked then
          AParam := AParam + '; Check: IsWin64';
        if rb_NotIsWin64.Checked then
          AParam := AParam + '; Check: not IsWin64';
        if rb_IsAdmMod.Checked then
          AParam := AParam + '; Check: IsAdminInstallMode';
        if rb_NotIsAdmMod.Checked then
          AParam := AParam + '; Check: not IsAdminInstallMode';
      end;
    Result := AParam;
  end;

  procedure SubKeyRecord(AReg: TInnoRegData; ADelKey: Boolean = False);
  var
    ALine: String;
  begin
    ALine := 'Root: ' + AReg.Root +
             '; SubKey: ' + AReg.SubKey;
    if ADelKey then
      ALine := ALine + '; ValueType: none' +
                       '; Flags: deletekey';
    if cb_FlagSubKey.Checked and not ADelKey then
      begin
        if rb_FlagDelKeyIfEmpty.Checked then
          ALine := ALine + '; Flags: uninsdeletekeyifempty';
        if rb_FlagDelKey.Checked then
          ALine := ALine + '; Flags: uninsdeletekey';
      end;
(*    if cb_FlagSubKey.Checked and ADelKey then
      begin
        if rb_FlagDelKeyIfEmpty.Checked then
          ALine := ALine + ' uninsdeletekeyifempty';
        if rb_FlagDelKey.Checked then
          ALine := ALine + ' uninsdeletekey';
      end;  *)
    ALine := AddParamToStr(ALine);
    OutBuffer.Add(ALine);
  end;

  procedure SubKeyParamRecord(AReg: TInnoRegData; ADelParam: Boolean = False);
  var
    ALine: String;
  begin
    ALine := 'Root: ' + AReg.Root +
             '; SubKey: ' + AReg.SubKey +
             '; ValueType: ' + AReg.ValueType +
             '; ValueName: ' + AReg.ValueName;
    if not ADelParam then
      ALine := ALine + '; ValueData: ' + AReg.ValueData;
    if ADelParam then
      ALine := ALine + '; Flags: deletevalue';
    if hex0 and not ADelParam then
      ALine := ALine.Replace('; ValueData: ""', '');
    if cb_FlagDelValue.Checked and not ADelParam then
      ALine := ALine + '; Flags: uninsdeletevalue';
    //if cb_FlagDelValue.Checked and ADelParam then
    //  ALine := ALine + ' uninsdeletevalue';
    ALine := AddParamToStr(ALine);
    OutBuffer.Add(ALine);
  end;

begin
  InBuffer := TStringList.Create;
  OutBuffer := TStringList.Create;
  try
    OutBuffer.Clear;
    InBuffer.LoadFromFile(edt_PathFileReg.Text);
    OutBuffer.Add(SNewLine + RemarkaLine + ExtractFileName(edt_PathFileReg.Text));
    i := 0;
    while i <= InBuffer.Count-1 do
      begin
        InBufStr := InBuffer.Strings[i];
        if StrIsRoot(InBufStr) then
          begin
            delkey := False;
            InBufStr := CutStrBeginEnd(InBufStr, 1);
            if InBufStr.StartsWith('-') then
              begin
                delkey := True;
                InBufStr := Copy(InBufStr, 2, InBufStr.Length);
              end;
            poschar := Pos('\', InBufStr);
            ISRegData.Root := Copy(InBufStr, 1, poschar - 1);
            ISRegData.Root := StrRootRename(ISRegData.Root);
            ISRegData.SubKey := Copy(InBufStr, poschar + 1, InBufStr.Length);
            ISRegData.SubKey := ISRegData.SubKey.Replace('{', '{{');
            ISRegData.SubKey := ISRegData.SubKey.QuotedString('"');
            if ISRegData.SubKey.Contains('\WOW6432Node') then
              begin
                //ISRegData.Root := ISRegData.Root + '32'; // 32-bit on Windows 64 bit
                ISRegData.SubKey := ISRegData.SubKey.Replace('\WOW6432Node', '');
              end;
            Inc(i);
            InBufStr := InBuffer.Strings[i];
            if (InBufStr = '') and not delkey then
              SubKeyRecord(ISRegData);
            if (InBufStr = '') or (InBufStr <> '') and delkey then
              SubKeyRecord(ISRegData, delkey);
            while (InBufStr <> '') and not delkey do
            begin
              poschar := Pos('=', InBufStr);
              ISRegData.ValueName := CutStrBeginEnd(Copy(InBufStr, 1, poschar - 1), 1);
              ISRegData.ValueName := ISRegData.ValueName.Replace('\\', '\');
              ISRegData.ValueName := ISRegData.ValueName.Replace('{', '{{');
              ISRegData.ValueName := ISRegData.ValueName.QuotedString('"');
              InBufStr := Copy(InBufStr, poschar + 1, InBufStr.Length);
              case getValueType(InBufStr) of
                1: begin
                     // REG_SZ
                     ISRegData.ValueData := CutStrBeginEnd(InBufStr, 1);
                     ISRegData.ValueData := ISRegData.ValueData.Replace('\\', '\');
                     ISRegData.ValueData := ISRegData.ValueData.Replace('{', '{{');
                     ISRegData.ValueData := ISRegData.ValueData.QuotedString('"');
                     ISRegData.ValueType := 'string';
                   end;
                2: begin
                     // REG_BINARY
                     poschar := Pos(':', InBufStr);
                     InBufStr := Copy(InBufStr, poschar + 1, InBufStr.Length);
                     ISRegData.ValueData := InBufStr;
                     // Multiline binary
                     if InBufStr[InBufStr.Length] = '\' then
                       ISRegData.ValueData := Copy(ISRegData.ValueData, 1, ISRegData.ValueData.Length - 1);
                     while InBufStr[InBufStr.Length] = '\' do
                     begin
                       Inc(i);
                       InBufStr := InBuffer.Strings[i];
                       if InBufStr[InBufStr.Length] = '\' then
                         ISRegData.ValueData := ISRegData.ValueData + DelSpaceChar(Copy(InBufStr, 1, InBufStr.Length - 1))
                       else
                         ISRegData.ValueData := ISRegData.ValueData + DelSpaceChar(Copy(InBufStr, 1, InBufStr.Length))
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
                     poschar := Pos(':', InBufStr);
                     ISRegData.ValueData := Copy(InBufStr, poschar + 1, InBufStr.Length);
                     if hexb then
                       // REG_QWORD
                       begin
                         InBufStr := ISRegData.ValueData.Replace(',', '');
                         ISRegData.ValueData := '';
                         for j := 0 to (InBufStr.Length div 2) do
                         ISRegData.ValueData := Copy(InBufStr, (j * 2) + 1, 2) + ISRegData.ValueData;
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
              if not delval then
                SubKeyParamRecord(ISRegData)
              else
                SubKeyParamRecord(ISRegData, delval);
              Inc(i);
              InBufStr := InBuffer.Strings[i];
            end;
          end;
        Inc(i);
      end;
    OutBuffer.Add(RemarkaEND + SNewLine);
    Result := OutBuffer.Text;
  finally
    InBuffer.Free;
    OutBuffer.Free;
  end;
end;

procedure TRegistryDesignerForm.btn_BrowseClick(Sender: TObject);
const
  HeaderReg = 'Windows Registry Editor Version 5.00';
var
  FileCheck: TStringList;
  FilePath: String;
begin
  if OpenDialog1.Execute then
  begin
    FilePath := OpenDialog1.FileName;
      FileCheck := TStringList.Create;
      try
        FileCheck.LoadFromFile(FilePath);
        if (FileCheck[0] = HeaderReg) and (FileCheck[FileCheck.Count - 1] = '') then
          begin
            edt_PathFileReg.Text := FilePath;
          end
        else
          MessageDlg('Invalid file format, select another one.', mtWarning, [mbOK], 0);
      finally
         FileCheck.Free;
      end;
  end;
end;

procedure TRegistryDesignerForm.btn_InsertClick(Sender: TObject);
begin
  if FileExists(edt_PathFileReg.Text) then
    ModalResult := mrOK
  else
    MessageDlg('No file selected for processing!', mtWarning, [mbOK], 0);
end;

procedure TRegistryDesignerForm.cb_CheksOnOffClick(Sender: TObject);
begin
  if cb_CheksOnOff.Checked then
    begin
      rb_IsWin64.Enabled := True;
      rb_NotIsWin64.Enabled := True;
      rb_IsAdmMod.Enabled := True;
      rb_NotIsAdmMod.Enabled := True;
    end
  else
    begin
      rb_IsWin64.Enabled := False;
      rb_NotIsWin64.Enabled := False;
      rb_IsAdmMod.Enabled := False;
      rb_NotIsAdmMod.Enabled := False;
      rb_IsWin64.Checked := False;
      rb_NotIsWin64.Checked := False;
      rb_IsAdmMod.Checked := False;
      rb_NotIsAdmMod.Checked := False;
      rb_IsWin64.Font.Style := [];
      rb_NotIsWin64.Font.Style := [];
      rb_IsAdmMod.Font.Style := [];
      rb_NotIsAdmMod.Font.Style := [];
    end
end;

procedure TRegistryDesignerForm.cb_FlagDelValueClick(Sender: TObject);
begin
  if cb_FlagDelValue.Checked then
    begin
      st_uninsdeletevalue.Font.Style := [fsBold];
      st_uninsdeletevalue.Enabled := True;
    end
  else
    begin
      st_uninsdeletevalue.Font.Style := [];
      st_uninsdeletevalue.Enabled := False;
    end;
end;

procedure TRegistryDesignerForm.cb_FlagSubKeyClick(Sender: TObject);
begin
  if cb_FlagSubKey.Checked then
    begin
      rb_FlagDelKey.Enabled := True;
      rb_FlagDelKeyIfEmpty.Enabled := True;
      rb_FlagDelKeyIfEmpty.Checked := True;
    end
  else
    begin
      rb_FlagDelKey.Enabled := False;
      rb_FlagDelKeyIfEmpty.Enabled := False;
      rb_FlagDelKey.Checked := False;
      rb_FlagDelKeyIfEmpty.Checked := False;
      rb_FlagDelKeyIfEmpty.Font.Style := [];
      rb_FlagDelKey.Font.Style := [];
    end
end;

procedure TRegistryDesignerForm.cb_MinVerClick(Sender: TObject);
begin
  if cb_MinVer.Checked then
    begin
      st_MinVersion.Font.Style := [fsBold];
      st_MinVersion.Enabled := True;
      edt_MinVer.Enabled := True;
    end
  else
    begin
      st_MinVersion.Font.Style := [];
      st_MinVersion.Enabled := False;
      edt_MinVer.Enabled := False;
    end
end;

procedure TRegistryDesignerForm.rb_FlagDelKeyClick(Sender: TObject);
begin
  if rb_FlagDelKey.Checked then
    begin
      rb_FlagDelKey.Font.Style := [fsBold];
      rb_FlagDelKeyIfEmpty.Font.Style := [];
    end;
end;

procedure TRegistryDesignerForm.rb_FlagDelKeyIfEmptyClick(Sender: TObject);
begin
  if rb_FlagDelKeyIfEmpty.Checked then
    begin
      rb_FlagDelKeyIfEmpty.Font.Style := [fsBold];
      rb_FlagDelKey.Font.Style := [];
    end;
end;

procedure TRegistryDesignerForm.rb_IsAdmModClick(Sender: TObject);
begin
  if rb_IsAdmMod.Checked then
    begin
      rb_IsAdmMod.Font.Style := [fsBold];
      rb_NotIsWin64.Font.Style := [];
      rb_IsWin64.Font.Style := [];
      rb_NotIsAdmMod.Font.Style := [];
    end;
end;

procedure TRegistryDesignerForm.rb_IsWin64Click(Sender: TObject);
begin
  if rb_IsWin64.Checked then
    begin
      rb_IsWin64.Font.Style := [fsBold];
      rb_NotIsWin64.Font.Style := [];
      rb_IsAdmMod.Font.Style := [];
      rb_NotIsAdmMod.Font.Style := [];
    end;
end;

procedure TRegistryDesignerForm.rb_NotIsAdmModClick(Sender: TObject);
begin
  if rb_NotIsAdmMod.Checked then
    begin
      rb_NotIsAdmMod.Font.Style := [fsBold];
      rb_NotIsWin64.Font.Style := [];
      rb_IsWin64.Font.Style := [];
      rb_IsAdmMod.Font.Style := [];
    end;
end;

procedure TRegistryDesignerForm.rb_NotIsWin64Click(Sender: TObject);
begin
  if rb_NotIsWin64.Checked then
    begin
      rb_NotIsWin64.Font.Style := [fsBold];
      rb_IsWin64.Font.Style := [];
      rb_IsAdmMod.Font.Style := [];
      rb_NotIsAdmMod.Font.Style := [];
    end;
end;

end.
