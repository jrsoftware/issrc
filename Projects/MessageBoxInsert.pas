unit MessageBoxInsert;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UIStateForm, StdCtrls, ExtCtrls, NewStaticText, Vcl.ComCtrls,
  Vcl.Imaging.pngimage;

type
  TMBDForm = class(TUIStateForm)
    IMGmbInformation: TImage;
    IMGmbConfirmation: TImage;
    IMGmbError: TImage;
    IMGmbCriticalError: TImage;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    MSGText: TMemo;
    rb_mbInformation: TRadioButton;
    rb_mbConfirmation: TRadioButton;
    rb_mbError: TRadioButton;
    rb_mbCriticalError: TRadioButton;
    GroupBox3: TGroupBox;
    MBDButtonOK: TButton;
    MBDButtonCancel: TButton;
    MBDButtonPreview: TButton;
    Bevel1: TBevel;
    rbMB_OK: TRadioButton;
    rbMB_OKCANCEL: TRadioButton;
    rbMB_YESNO: TRadioButton;
    rbMB_YESNOCANCEL: TRadioButton;
    rbMB_RETRYCANCEL: TRadioButton;
    rbMB_ABORTRETRYIGNORE: TRadioButton;
    GroupBox4: TGroupBox;
    cb_IDOK: TCheckBox;
    cb_IDCANCEL: TCheckBox;
    cb_IDYES: TCheckBox;
    cb_IDNO: TCheckBox;
    cb_IDABORT: TCheckBox;
    cb_IDRETRY: TCheckBox;
    cb_IDIGNORE: TCheckBox;
    GroupBox5: TGroupBox;
    cb_MB_SETFOREGROUND: TCheckBox;
    NewStaticText1: TNewStaticText;
    NewEdit1: TEdit;
    UpDown1: TUpDown;
    GroupBox6: TGroupBox;
    cb_MsgBox: TRadioButton;
    cb_TaskDialogMsgBox: TRadioButton;
    rb_IDOK: TCheckBox;
    rb_IDCANCEL: TCheckBox;
    rb_IDYES: TCheckBox;
    rb_IDNO: TCheckBox;
    rb_IDABORT: TCheckBox;
    rb_IDRETRY: TCheckBox;
    rb_IDIGNORE: TCheckBox;
    TaskInstructionLabel: TNewStaticText;
    TaskMesssageLabel: TNewStaticText;
    TaskInstructionText: TEdit;
    TaskMesssageText: TEdit;
    Button1Text: TEdit;
    Button2Text: TEdit;
    Button1Label: TNewStaticText;
    Button2Label: TNewStaticText;
    procedure FormCreate(Sender: TObject);
    procedure UpDown1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure rbMB_OKClick(Sender: TObject);
    procedure rbMB_OKCANCELClick(Sender: TObject);
    procedure rbMB_YESNOClick(Sender: TObject);
    procedure rbMB_RETRYCANCELClick(Sender: TObject);
    procedure rbMB_YESNOCANCELClick(Sender: TObject);
    procedure rbMB_ABORTRETRYIGNOREClick(Sender: TObject);
    procedure MBDButtonPreviewClick(Sender: TObject);
    procedure cb_TaskDialogMsgBoxClick(Sender: TObject);
    procedure cb_MsgBoxClick(Sender: TObject);
    procedure rb_IDOKClick(Sender: TObject);
    procedure rb_IDCANCELClick(Sender: TObject);
    procedure rb_IDNOClick(Sender: TObject);
    procedure rb_IDYESClick(Sender: TObject);
    procedure rb_IDRETRYClick(Sender: TObject);
    procedure rb_IDIGNOREClick(Sender: TObject);
    procedure rb_IDABORTClick(Sender: TObject);
    procedure MBDButtonOKClick(Sender: TObject);
  private
    procedure CreateScriptMSG;
  public
    { Public declarations }
    //fdefaultStyleName: String;
  end;

implementation

uses
  CmnFunc, CmnFunc2, CompForm, TaskDialog, Msgs;

{$R *.DFM}

procedure TMBDForm.FormCreate(Sender: TObject);
begin
  rb_mbInformation.Checked := True;
  rbMB_OK.Checked := True;
  MSGText.Lines.Clear;
  MSGText.Lines[MSGText.CaretPos.Y] := '<Enter your text here...>';
  MSGText.SelStart := MSGText.Perform(EM_LINEINDEX, 0, 0);
  MSGText.SelLength := Length(MSGText.Lines[0]);
  cb_IDOK.Checked := False;
  cb_IDCANCEL.Checked := False;
  cb_IDABORT.Checked := False;
  cb_IDRETRY.Checked := False;
  cb_IDIGNORE.Checked := False;
  cb_IDYES.Checked := False;
  cb_IDNO.Checked := False;
  cb_IDOK.Enabled := False;
  cb_IDCANCEL.Enabled := False;
  cb_IDABORT.Enabled := False;
  cb_IDRETRY.Enabled := False;
  cb_IDIGNORE.Enabled := False;
  cb_IDYES.Enabled := False;
  cb_IDNO.Enabled := False;
  NewStaticText1.Enabled := False;
  NewEdit1.Enabled := False;
  UpDown1.Enabled := False;

  TaskInstructionLabel.Visible := False;
  TaskMesssageLabel.Visible := False;
  TaskInstructionText.Visible := False;
  TaskMesssageText.Visible := False;
  Button1Text.Visible := False;
  Button2Text.Visible := False;
  Button1Label.Visible := False;
  Button2Label.Visible := False;
  TaskInstructionText.Text := '';
  TaskMesssageText.Text := '';
  Button1Text.Text := '';
  Button2Text.Text := '';
end;

procedure TMBDForm.rbMB_OKClick(Sender: TObject);
begin
  cb_IDOK.Checked := False;
  cb_IDCANCEL.Checked := False;
  cb_IDABORT.Checked := False;
  cb_IDRETRY.Checked := False;
  cb_IDIGNORE.Checked := False;
  cb_IDYES.Checked := False;
  cb_IDNO.Checked := False;
  cb_IDOK.Enabled := False;
  cb_IDCANCEL.Enabled := False;
  cb_IDABORT.Enabled := False;
  cb_IDRETRY.Enabled := False;
  cb_IDIGNORE.Enabled := False;
  cb_IDYES.Enabled := False;
  cb_IDNO.Enabled := False;
  NewStaticText1.Enabled := False;
  NewEdit1.Enabled := False;
  NewEdit1.Text := '1';
  UpDown1.Enabled := False;
  if cb_TaskDialogMsgBox.Checked then begin
     rb_IDOK.Enabled := True;
     rb_IDCANCEL.Enabled := False;
     rb_IDABORT.Enabled := False;
     rb_IDRETRY.Enabled := False;
     rb_IDIGNORE.Enabled := False;
     rb_IDYES.Enabled := False;
     rb_IDNO.Enabled := False;
     rb_IDOK.Checked := False;
     rb_IDCANCEL.Checked := False;
     rb_IDABORT.Checked := False;
     rb_IDRETRY.Checked := False;
     rb_IDIGNORE.Checked := False;
     rb_IDYES.Checked := False;
     rb_IDNO.Checked := False;
     Button2Text.Enabled := False;
     Button2Label.Enabled := False;
     Button1Text.Enabled := True;
     Button1Label.Enabled := True;
     Button1Text.Text := 'OK';
     Button2Text.Text := '';
  end;

end;

procedure TMBDForm.rbMB_YESNOClick(Sender: TObject);
begin
  cb_IDOK.Checked := False;
  cb_IDCANCEL.Checked := False;
  cb_IDABORT.Checked := False;
  cb_IDRETRY.Checked := False;
  cb_IDIGNORE.Checked := False;
  cb_IDYES.Checked := False;
  cb_IDNO.Checked := False;
  cb_IDOK.Enabled := False;
  cb_IDCANCEL.Enabled := False;
  cb_IDABORT.Enabled := False;
  cb_IDRETRY.Enabled := False;
  cb_IDIGNORE.Enabled := False;
  cb_IDYES.Enabled := True;
  cb_IDNO.Enabled := True;
  if not cb_TaskDialogMsgBox.Checked then begin
     NewStaticText1.Enabled := True;
     NewEdit1.Enabled := True;
     NewEdit1.Text := '1';
     UpDown1.Max := 2;
     UpDown1.Enabled := True;
  end;
  if cb_TaskDialogMsgBox.Checked then begin
     rb_IDOK.Enabled := False;
     rb_IDCANCEL.Enabled := False;
     rb_IDABORT.Enabled := False;
     rb_IDRETRY.Enabled := False;
     rb_IDIGNORE.Enabled := False;
     rb_IDYES.Enabled := True;
     rb_IDNO.Enabled := True;
     rb_IDOK.Checked := False;
     rb_IDCANCEL.Checked := False;
     rb_IDABORT.Checked := False;
     rb_IDRETRY.Checked := False;
     rb_IDIGNORE.Checked := False;
     rb_IDYES.Checked := False;
     rb_IDNO.Checked := False;
     Button2Text.Enabled := True;
     Button2Label.Enabled := True;
     Button1Text.Enabled := True;
     Button1Label.Enabled := True;
     Button1Text.Text := 'Yes';
     Button2Text.Text := 'No';
  end;
end;

procedure TMBDForm.rbMB_OKCANCELClick(Sender: TObject);
begin
  cb_IDOK.Checked := False;
  cb_IDCANCEL.Checked := False;
  cb_IDABORT.Checked := False;
  cb_IDRETRY.Checked := False;
  cb_IDIGNORE.Checked := False;
  cb_IDYES.Checked := False;
  cb_IDNO.Checked := False;
  cb_IDOK.Enabled := True;
  cb_IDCANCEL.Enabled := True;
  cb_IDABORT.Enabled := False;
  cb_IDRETRY.Enabled := False;
  cb_IDIGNORE.Enabled := False;
  cb_IDYES.Enabled := False;
  cb_IDNO.Enabled := False;
  if not cb_TaskDialogMsgBox.Checked then begin
     NewStaticText1.Enabled := True;
     NewEdit1.Enabled := True;
     NewEdit1.Text := '1';
     UpDown1.Max := 2;
     UpDown1.Enabled := True;
  end;
  if cb_TaskDialogMsgBox.Checked then begin
     rb_IDOK.Enabled := True;
     rb_IDCANCEL.Enabled := True;
     rb_IDABORT.Enabled := False;
     rb_IDRETRY.Enabled := False;
     rb_IDIGNORE.Enabled := False;
     rb_IDYES.Enabled := False;
     rb_IDNO.Enabled := False;
     rb_IDOK.Checked := False;
     rb_IDCANCEL.Checked := False;
     rb_IDABORT.Checked := False;
     rb_IDRETRY.Checked := False;
     rb_IDIGNORE.Checked := False;
     rb_IDYES.Checked := False;
     rb_IDNO.Checked := False;
     Button2Text.Enabled := False;
     Button2Label.Enabled := False;
     Button1Text.Enabled := True;
     Button1Label.Enabled := True;
     Button1Text.Text := 'OK';
     Button2Text.Text := '';
  end;
end;

procedure TMBDForm.rbMB_RETRYCANCELClick(Sender: TObject);
begin
  cb_IDOK.Checked := False;
  cb_IDCANCEL.Checked := False;
  cb_IDABORT.Checked := False;
  cb_IDRETRY.Checked := False;
  cb_IDIGNORE.Checked := False;
  cb_IDYES.Checked := False;
  cb_IDNO.Checked := False;
  cb_IDOK.Enabled := False;
  cb_IDCANCEL.Enabled := True;
  cb_IDABORT.Enabled := False;
  cb_IDRETRY.Enabled := True;
  cb_IDIGNORE.Enabled := False;
  cb_IDYES.Enabled := False;
  cb_IDNO.Enabled := False;
  if not cb_TaskDialogMsgBox.Checked then begin
     NewStaticText1.Enabled := True;
     NewEdit1.Enabled := True;
     NewEdit1.Text := '1';
     UpDown1.Max := 2;
     UpDown1.Enabled := True;
  end;
  if cb_TaskDialogMsgBox.Checked then begin
     rb_IDOK.Enabled := False;
     rb_IDCANCEL.Enabled := True;
     rb_IDABORT.Enabled := False;
     rb_IDRETRY.Enabled := True;
     rb_IDIGNORE.Enabled := False;
     rb_IDYES.Enabled := False;
     rb_IDNO.Enabled := False;
     rb_IDOK.Checked := False;
     rb_IDCANCEL.Checked := False;
     rb_IDABORT.Checked := False;
     rb_IDRETRY.Checked := False;
     rb_IDIGNORE.Checked := False;
     rb_IDYES.Checked := False;
     rb_IDNO.Checked := False;
     Button2Text.Enabled := False;
     Button2Label.Enabled := False;
     Button1Text.Enabled := True;
     Button1Label.Enabled := True;
     Button1Text.Text := 'Retry';
     Button2Text.Text := '';
  end;
end;

procedure TMBDForm.rbMB_YESNOCANCELClick(Sender: TObject);
begin
  cb_IDOK.Checked := False;
  cb_IDCANCEL.Checked := False;
  cb_IDABORT.Checked := False;
  cb_IDRETRY.Checked := False;
  cb_IDIGNORE.Checked := False;
  cb_IDYES.Checked := False;
  cb_IDNO.Checked := False;
  cb_IDOK.Enabled := False;
  cb_IDCANCEL.Enabled := True;
  cb_IDABORT.Enabled := False;
  cb_IDRETRY.Enabled := False;
  cb_IDIGNORE.Enabled := False;
  cb_IDYES.Enabled := True;
  cb_IDNO.Enabled := True;
  if not cb_TaskDialogMsgBox.Checked then begin
     NewStaticText1.Enabled := True;
     NewEdit1.Enabled := True;
     NewEdit1.Text := '1';
     UpDown1.Max := 3;
     UpDown1.Enabled := True;
  end;
  if cb_TaskDialogMsgBox.Checked then begin
     rb_IDOK.Enabled := False;
     rb_IDCANCEL.Enabled := True;
     rb_IDABORT.Enabled := False;
     rb_IDRETRY.Enabled := False;
     rb_IDIGNORE.Enabled := False;
     rb_IDYES.Enabled := True;
     rb_IDNO.Enabled := True;
     rb_IDOK.Checked := False;
     rb_IDCANCEL.Checked := False;
     rb_IDABORT.Checked := False;
     rb_IDRETRY.Checked := False;
     rb_IDIGNORE.Checked := False;
     rb_IDYES.Checked := False;
     rb_IDNO.Checked := False;
     Button2Text.Enabled := True;
     Button2Label.Enabled := True;
     Button1Text.Enabled := True;
     Button1Label.Enabled := True;
     Button1Text.Text := 'Yes';
     Button2Text.Text := 'No';
  end;
end;

procedure TMBDForm.rbMB_ABORTRETRYIGNOREClick(Sender: TObject);
begin
  cb_IDOK.Checked := False;
  cb_IDCANCEL.Checked := False;
  cb_IDABORT.Checked := False;
  cb_IDRETRY.Checked := False;
  cb_IDIGNORE.Checked := False;
  cb_IDYES.Checked := False;
  cb_IDNO.Checked := False;
  cb_IDOK.Enabled := False;
  cb_IDCANCEL.Enabled := False;
  cb_IDABORT.Enabled := True;
  cb_IDRETRY.Enabled := True;
  cb_IDIGNORE.Enabled := True;
  cb_IDYES.Enabled := False;
  cb_IDNO.Enabled := False;
  if not cb_TaskDialogMsgBox.Checked then begin
     NewStaticText1.Enabled := True;
     NewEdit1.Enabled := True;
     NewEdit1.Text := '1';
     UpDown1.Max := 3;
     UpDown1.Enabled := True;
  end;
  if cb_TaskDialogMsgBox.Checked then begin
     rb_IDOK.Enabled := False;
     rb_IDCANCEL.Enabled := False;
     rb_IDABORT.Enabled := True;
     rb_IDRETRY.Enabled := True;
     rb_IDIGNORE.Enabled := True;
     rb_IDYES.Enabled := False;
     rb_IDNO.Enabled := False;
     rb_IDOK.Checked := False;
     rb_IDCANCEL.Checked := False;
     rb_IDABORT.Checked := False;
     rb_IDRETRY.Checked := False;
     rb_IDIGNORE.Checked := False;
     rb_IDYES.Checked := False;
     rb_IDNO.Checked := False;
     Button2Text.Enabled := False;
     Button2Label.Enabled := False;
     Button1Text.Enabled := False;
     Button1Label.Enabled := False;
     Button1Text.Text := '';
     Button2Text.Text := '';
  end;
end;

procedure TMBDForm.UpDown1Changing(Sender: TObject; var AllowChange: Boolean);
begin
   AllowChange := True;
end;

procedure TMBDForm.cb_MsgBoxClick(Sender: TObject);
begin
   IMGmbConfirmation.Visible := True;
   cb_MB_SETFOREGROUND.Enabled := True;
   cb_MB_SETFOREGROUND.Checked := False;
   GroupBox1.Visible := True;
   GroupBox4.Caption := ' Return value ';
   cb_IDOK.Checked := False;
   cb_IDCANCEL.Checked := False;
   cb_IDABORT.Checked := False;
   cb_IDRETRY.Checked := False;
   cb_IDIGNORE.Checked := False;
   cb_IDYES.Checked := False;
   cb_IDNO.Checked := False;
   rb_IDOK.Visible := False;
   rb_IDCANCEL.Visible := False;
   rb_IDABORT.Visible := False;
   rb_IDRETRY.Visible := False;
   rb_IDIGNORE.Visible := False;
   rb_IDYES.Visible := False;
   rb_IDNO.Visible := False;
   rb_mbInformation.Checked := True;
   TaskInstructionLabel.Visible := False;
   TaskMesssageLabel.Visible := False;
   TaskInstructionText.Visible := False;
   TaskMesssageText.Visible := False;
   Button1Text.Visible := False;
   Button2Text.Visible := False;
   Button1Label.Visible := False;
   Button2Label.Visible := False;
   rbMB_OK.Checked := True;
   rbMB_OKClick(Self);
end;

procedure TMBDForm.cb_TaskDialogMsgBoxClick(Sender: TObject);
begin
   IMGmbConfirmation.Visible := False;
   cb_MB_SETFOREGROUND.Enabled := False;
   cb_MB_SETFOREGROUND.Checked := False;
   GroupBox1.Visible := False;
   GroupBox4.Caption := ' Return value --- Shield ';
   cb_IDOK.Checked := False;
   cb_IDCANCEL.Checked := False;
   cb_IDABORT.Checked := False;
   cb_IDRETRY.Checked := False;
   cb_IDIGNORE.Checked := False;
   cb_IDYES.Checked := False;
   cb_IDNO.Checked := False;
   rb_IDOK.Checked := False;
   rb_IDCANCEL.Checked := False;
   rb_IDABORT.Checked := False;
   rb_IDRETRY.Checked := False;
   rb_IDIGNORE.Checked := False;
   rb_IDYES.Checked := False;
   rb_IDNO.Checked := False;
   rb_IDOK.Visible := True;
   rb_IDCANCEL.Visible := True;
   rb_IDABORT.Visible := True;
   rb_IDRETRY.Visible := True;
   rb_IDIGNORE.Visible := True;
   rb_IDYES.Visible := True;
   rb_IDNO.Visible := True;
   rb_mbInformation.Checked := True;
   TaskInstructionLabel.Visible := True;
   TaskMesssageLabel.Visible := True;
   TaskInstructionText.Visible := True;
   TaskMesssageText.Visible := True;
   Button1Text.Visible := True;
   Button2Text.Visible := True;
   Button1Label.Visible := True;
   Button2Label.Visible := True;
   TaskInstructionText.Text := 'Instruction Text';
   TaskMesssageText.Text := 'Messsage Text';
  // Button1Text.Text := '';
  // Button2Text.Text := '';
   rbMB_OK.Checked := True;
   rbMB_OKClick(Self);
end;

procedure TMBDForm.rb_IDOKClick(Sender: TObject);
begin
   if rb_IDOK.Checked then
      rb_IDCANCEL.Checked := False;
end;

procedure TMBDForm.rb_IDCANCELClick(Sender: TObject);
begin
   if rb_IDCANCEL.Checked then begin
      rb_IDOK.Checked := False;
      rb_IDNO.Checked := False;
      rb_IDYES.Checked := False;
      rb_IDRETRY.Checked := False;
   end;
end;

procedure TMBDForm.rb_IDYESClick(Sender: TObject);
begin
   if rb_IDYES.Checked then begin
      rb_IDNO.Checked := False;
      rb_IDCANCEL.Checked := False;
   end;
end;

procedure TMBDForm.rb_IDNOClick(Sender: TObject);
begin
   if rb_IDNO.Checked then begin
      rb_IDYES.Checked := False;
      rb_IDCANCEL.Checked := False;
   end;
end;

procedure TMBDForm.rb_IDRETRYClick(Sender: TObject);
begin
   if rb_IDRETRY.Checked then begin
      rb_IDCANCEL.Checked := False;
      rb_IDABORT.Checked := False;
      rb_IDIGNORE.Checked := False;
   end;
end;

procedure TMBDForm.rb_IDIGNOREClick(Sender: TObject);
begin
   if rb_IDIGNORE.Checked then begin
      rb_IDABORT.Checked := False;
      rb_IDRETRY.Checked := False;
   end;
end;

procedure TMBDForm.rb_IDABORTClick(Sender: TObject);
begin
   if rb_IDABORT.Checked then begin
      rb_IDIGNORE.Checked := False;
      rb_IDRETRY.Checked := False;
   end;
end;

procedure TMBDForm.MBDButtonOKClick(Sender: TObject);
begin
  CreateScriptMSG;
end;

procedure TMBDForm.MBDButtonPreviewClick(Sender: TObject);
var
  ButtonsBtn : Cardinal;
  TypeIcon : TMsgBoxType;
  CaptionMsg : String;
  ShieldFlag : Integer;
  ButtonLabelsArray: TArray<string>;
begin
  {default value}
  ButtonsBtn := MB_OK;
  TypeIcon := mbInformation;
  ShieldFlag := 0;

  {icon and caption set}
  if rb_mbInformation.Checked then begin
     CaptionMsg := 'Info';
     TypeIcon := mbInformation;
  end;
  if rb_mbConfirmation.Checked then begin
     CaptionMsg := 'Confirm';
     TypeIcon := mbConfirmation;
  end;
  if rb_mbError.Checked then begin
     CaptionMsg := 'Error';
     TypeIcon := mbError;
  end;
  if rb_mbCriticalError.Checked then begin
     CaptionMsg := 'Fatal Error';
     TypeIcon := mbCriticalError;
  end;
  {button type set}
  if rbMB_OK.Checked then ButtonsBtn := MB_OK;
  if rbMB_OKCANCEL.Checked then ButtonsBtn := MB_OKCANCEL;
  if rbMB_YESNO.Checked then ButtonsBtn := MB_YESNO;
  if rbMB_YESNOCANCEL.Checked then ButtonsBtn := MB_YESNOCANCEL;
  if rbMB_RETRYCANCEL.Checked then ButtonsBtn := MB_RETRYCANCEL;
  if rbMB_ABORTRETRYIGNORE.Checked then ButtonsBtn := MB_ABORTRETRYIGNORE;

  if cb_MsgBox.Checked then begin
  {MsgBox(Text, Caption, Typ, Buttons);}
    if MSGText.GetTextLen = 0 then
       MSGText.Lines.Add('Your message text.');
    {MessageBox with DefButton}
    if NewEdit1.Text = '1' then
       MsgBox(MSGText.Lines.GetText, CaptionMsg, TypeIcon, ButtonsBtn);
    if NewEdit1.Text = '2' then
       MsgBox(MSGText.Lines.GetText, CaptionMsg, TypeIcon, ButtonsBtn or MB_DEFBUTTON2);
    if NewEdit1.Text = '3' then
       MsgBox(MSGText.Lines.GetText, CaptionMsg, TypeIcon, ButtonsBtn or MB_DEFBUTTON3);
    {MessageBox with DefButton and Flag MB_SETFOREGROUND}
    if (NewEdit1.Text = '1') and (cb_MB_SETFOREGROUND.Checked) then
       MsgBox(MSGText.Lines.GetText, CaptionMsg, TypeIcon, ButtonsBtn or MB_SETFOREGROUND);
    if (NewEdit1.Text = '2') and (cb_MB_SETFOREGROUND.Checked) then
       MsgBox(MSGText.Lines.GetText, CaptionMsg, TypeIcon, ButtonsBtn or MB_DEFBUTTON2 or MB_SETFOREGROUND);
    if (NewEdit1.Text = '3') and (cb_MB_SETFOREGROUND.Checked) then
       MsgBox(MSGText.Lines.GetText, CaptionMsg, TypeIcon, ButtonsBtn or MB_DEFBUTTON3 or MB_SETFOREGROUND);
  end;

  if cb_TaskDialogMsgBox.Checked then begin
     {create ButtonLabels array}
     if rbMB_YESNO.Checked or rbMB_YESNOCANCEL.Checked then
        ButtonLabelsArray := TArray<string>.Create(Button1Text.Text, Button2Text.Text)
     else if rbMB_ABORTRETRYIGNORE.Checked then
        ButtonLabelsArray := TArray<string>.Create('Retry', 'Ignore', 'Abort')
     else
        ButtonLabelsArray := TArray<string>.Create(Button1Text.Text);

     {get Shield Flag value}
     if rbMB_OK.Checked and rb_IDOK.Checked then ShieldFlag := IDOK;
     if rbMB_OKCANCEL.Checked and rb_IDOK.Checked then ShieldFlag := IDOK;
     if rbMB_OKCANCEL.Checked and rb_IDCANCEL.Checked then ShieldFlag := IDCANCEL;
     if rbMB_YESNO.Checked and rb_IDYES.Checked then ShieldFlag := IDYES;
     if rbMB_YESNO.Checked and rb_IDNO.Checked then ShieldFlag := IDNO;
     if rbMB_YESNOCANCEL.Checked and rb_IDYES.Checked then ShieldFlag := IDYES;
     if rbMB_YESNOCANCEL.Checked and rb_IDNO.Checked then ShieldFlag := IDNO;
     if rbMB_YESNOCANCEL.Checked and rb_IDCANCEL.Checked then ShieldFlag := IDCANCEL;
     if rbMB_RETRYCANCEL.Checked and rb_IDRETRY.Checked then ShieldFlag := IDRETRY;
     if rbMB_RETRYCANCEL.Checked and rb_IDCANCEL.Checked then ShieldFlag := IDCANCEL;
     if rbMB_ABORTRETRYIGNORE.Checked and rb_IDRETRY.Checked then ShieldFlag := IDRETRY;
     if rbMB_ABORTRETRYIGNORE.Checked and rb_IDABORT.Checked then ShieldFlag := IDABORT;
     if rbMB_ABORTRETRYIGNORE.Checked and rb_IDIGNORE.Checked then ShieldFlag := IDIGNORE;

  {TaskDialogMsgBox(Icon, Instruction, Text, Caption, Typ, Buttons, ButtonLabels, ShieldButton)}
     TaskDialogMsgBox('',
                      TaskInstructionText.Text,
                      TaskMesssageText.Text,
                      CaptionMsg,
                      TypeIcon,
                      ButtonsBtn,
                      ButtonLabelsArray,
                      ShieldFlag);
  end;

end;

procedure TMBDForm.CreateScriptMSG;

   { MsgBox }
   function TextMsg(M: Integer; a, b, c, d, e, f: String): String;
    { (M) - TypeMsg
      (a) - TextMsgIf
      (b) - IconTypes
      (c) - ButtonBtn
      (d) - IDButton
      (e) - IDButton2
      (f) - IDButton3 }
   var
      SMsg : String;
   begin
     SMsg := 'MsgBox(''' + a + ''', ' + b + ', ' + c + ')';
     case M of
        0: Result := SMsg + ';';
        1: Result := 'if ' + SMsg + ' = ' + d + ' then' + SNewLine + 'begin' + SNewLine + #9 + '// user clicked ' + StringReplace(d, 'ID', '', []) + SNewLine + 'end;';
        2: Result := 'case ' + SMsg + ' of ' + SNewLine + #9 + d +': { user clicked ' + StringReplace(d, 'ID', '', []) + ' };' + SNewLine + #9 + e +': { user clicked ' + StringReplace(e, 'ID', '', []) + ' };' + SNewLine + 'end;';
        3: Result := 'case ' + SMsg + ' of ' + SNewLine + #9 + d +': { user clicked ' + StringReplace(d, 'ID', '', []) + ' };' + SNewLine + #9 + e +': { user clicked ' + StringReplace(e, 'ID', '', []) + ' };' + SNewLine + #9 + 'else { user clicked ' + StringReplace(f, 'ID', '', []) + ' };' + SNewLine + 'end;';
     end;
   end;

   { TaskDialogMsgBox }
   function TextTask(N: Integer; a, b, c, r, s, d, e, f: String): String;
    { (N) - TypeMsg
      (a) - TextMsgIf (Button1Text.Text and Button2Text.Text)
      (b) - IconTypes
      (c) - ButtonBtn
      (r) - BtnTextArr
      (s) - ShieldFlg
      (d) - IDButton
      (e) - IDButton2
      (f) - IDButton3 }
   var
      STsg : String;
   begin
     STsg := 'TaskDialogMsgBox(''' + a + ''', ' + b + ', ' + c + ', [''' + r + '''], ' + s + ')';
     case N of
        0: Result := STsg + ';';
        1: Result := 'if ' + STsg + ' = ' + d + ' then' + SNewLine + 'begin' + SNewLine + #9 + '// user clicked ' + StringReplace(d, 'ID', '', []) + SNewLine + 'end;';
        2: Result := 'case ' + STsg + ' of ' + SNewLine + #9 + d +': { user clicked ' + StringReplace(d, 'ID', '', []) + ' };' + SNewLine + #9 + e +': { user clicked ' + StringReplace(e, 'ID', '', []) + ' };' + SNewLine + 'end;';
        3: Result := 'case ' + STsg + ' of ' + SNewLine + #9 + d +': { user clicked ' + StringReplace(d, 'ID', '', []) + ' };' + SNewLine + #9 + e +': { user clicked ' + StringReplace(e, 'ID', '', []) + ' };' + SNewLine + #9 + 'else { user clicked ' + StringReplace(f, 'ID', '', []) + ' };' + SNewLine + 'end;';
     end;
   end;

const
  remarka = '// Display a message box';
 // SNewLine = #13#10;  { line break }
var
  ButtonBtn, TextMsgIf, IconTypes : String;
  //IconTypes : TMsgBoxType;
  {MSGEdit, TaskInstrEdit, TaskMsgEdit, BtnEdit1, BtnEdit2, }IDButton, IDButton2, IDButton3 : String;
  TypeMsg, ModeMsg : Integer;
  BtnTextArr, ShieldFlg : String;
  //I : Integer;
begin
  {default value}
  ButtonBtn := 'MB_OK';
  IconTypes := 'mbInformation';
  ShieldFlg := '0';
  TypeMsg := 0;
  ModeMsg := 0;

  MSGTextInsert.Clear;
  MSGTextInsert.Add(remarka);

  {icon and caption set}
  if rb_mbInformation.Checked then begin
     IconTypes := 'mbInformation';
  end;
  if rb_mbConfirmation.Checked then begin
     IconTypes := 'mbConfirmation';
  end;
  if rb_mbError.Checked then begin
     IconTypes := 'mbError';
  end;
  if rb_mbCriticalError.Checked then begin
     IconTypes := 'mbCriticalError';
  end;
  {button type set}
  if rbMB_OK.Checked then ButtonBtn := 'MB_OK';
  if rbMB_OKCANCEL.Checked then ButtonBtn := 'MB_OKCANCEL';
  if rbMB_YESNO.Checked then ButtonBtn := 'MB_YESNO';
  if rbMB_YESNOCANCEL.Checked then ButtonBtn := 'MB_YESNOCANCEL';
  if rbMB_RETRYCANCEL.Checked then ButtonBtn := 'MB_RETRYCANCEL';
  if rbMB_ABORTRETRYIGNORE.Checked then ButtonBtn := 'MB_ABORTRETRYIGNORE';

  if cb_MsgBox.Checked then begin
     {MsgBox(Text, Typ, Buttons);}
     ModeMsg := 0;
     // rbMB_OK.Checked
     {MessageBox with DefButton}
     {MessageBox with DefButton and Flag MB_SETFOREGROUND}
     if (rbMB_OK.Checked) and (cb_MB_SETFOREGROUND.Checked) then
        ButtonBtn := 'MB_OK or MB_SETFOREGROUND';

     // rbMB_OKCANCEL
     {MessageBox with DefButton}
     if (rbMB_OKCANCEL.Checked) and (NewEdit1.Text = '2') then
        ButtonBtn := 'MB_OKCANCEL or MB_DEFBUTTON2';
     {MessageBox with DefButton and Flag MB_SETFOREGROUND}
     if (rbMB_OKCANCEL.Checked) and (cb_MB_SETFOREGROUND.Checked) then
        ButtonBtn := 'MB_OKCANCEL or MB_SETFOREGROUND';
     if (rbMB_OKCANCEL.Checked) and (NewEdit1.Text = '2') and (cb_MB_SETFOREGROUND.Checked) then
        ButtonBtn := 'MB_OKCANCEL or MB_DEFBUTTON2 or MB_SETFOREGROUND';

     // rbMB_YESNO
     {MessageBox with DefButton}
     if (rbMB_YESNO.Checked) and (NewEdit1.Text = '2') then
        ButtonBtn := 'MB_YESNO or MB_DEFBUTTON2';
     {MessageBox with DefButton and Flag MB_SETFOREGROUND}
     if (rbMB_YESNO.Checked) and (cb_MB_SETFOREGROUND.Checked) then
        ButtonBtn := 'MB_YESNO or MB_SETFOREGROUND';
     if (rbMB_YESNO.Checked) and (NewEdit1.Text = '2') and (cb_MB_SETFOREGROUND.Checked) then
        ButtonBtn := 'MB_YESNO or MB_DEFBUTTON2 or MB_SETFOREGROUND';

     // rbMB_RETRYCANCEL
     {MessageBox with DefButton}
     if (rbMB_RETRYCANCEL.Checked) and (NewEdit1.Text = '2') then
        ButtonBtn := 'MB_RETRYCANCEL or MB_DEFBUTTON2';
     {MessageBox with DefButton and Flag MB_SETFOREGROUND}
     if (rbMB_RETRYCANCEL.Checked) and (cb_MB_SETFOREGROUND.Checked) then
        ButtonBtn := 'MB_RETRYCANCEL or MB_SETFOREGROUND';
     if (rbMB_RETRYCANCEL.Checked) and (NewEdit1.Text = '2') and (cb_MB_SETFOREGROUND.Checked) then
        ButtonBtn := 'MB_RETRYCANCEL or MB_DEFBUTTON2 or MB_SETFOREGROUND';

     // rbMB_YESNOCANCEL
     {MessageBox with DefButton}
     if (rbMB_YESNOCANCEL.Checked) and (NewEdit1.Text = '2') then
        ButtonBtn := 'MB_YESNOCANCEL or MB_DEFBUTTON2';
     if (rbMB_YESNOCANCEL.Checked) and (NewEdit1.Text = '3') then
        ButtonBtn := 'MB_YESNOCANCEL or MB_DEFBUTTON3';
     {MessageBox with DefButton and Flag MB_SETFOREGROUND}
     if (rbMB_YESNOCANCEL.Checked) and (cb_MB_SETFOREGROUND.Checked) then
        ButtonBtn := 'MB_YESNOCANCEL or MB_SETFOREGROUND';
     if (rbMB_YESNOCANCEL.Checked) and (NewEdit1.Text = '2') and (cb_MB_SETFOREGROUND.Checked) then
        ButtonBtn := 'MB_YESNOCANCEL or MB_DEFBUTTON2 or MB_SETFOREGROUND';
     if (rbMB_YESNOCANCEL.Checked) and (NewEdit1.Text = '3') and (cb_MB_SETFOREGROUND.Checked) then
        ButtonBtn := 'MB_YESNOCANCEL or MB_DEFBUTTON3 or MB_SETFOREGROUND';

     // rbMB_ABORTRETRYIGNORE
     {MessageBox with DefButton}
     if (rbMB_ABORTRETRYIGNORE.Checked) and (NewEdit1.Text = '2') then
        ButtonBtn := 'MB_ABORTRETRYIGNORE or MB_DEFBUTTON2';
     if (rbMB_ABORTRETRYIGNORE.Checked) and (NewEdit1.Text = '3') then
        ButtonBtn := 'MB_ABORTRETRYIGNORE or MB_DEFBUTTON3';
     {MessageBox with DefButton and Flag MB_SETFOREGROUND}
     if (rbMB_ABORTRETRYIGNORE.Checked) and (cb_MB_SETFOREGROUND.Checked) then
        ButtonBtn := 'MB_ABORTRETRYIGNORE or MB_SETFOREGROUND';
     if (rbMB_ABORTRETRYIGNORE.Checked) and (NewEdit1.Text = '2') and (cb_MB_SETFOREGROUND.Checked) then
        ButtonBtn := 'MB_ABORTRETRYIGNORE or MB_DEFBUTTON2 or MB_SETFOREGROUND';
     if (rbMB_ABORTRETRYIGNORE.Checked) and (NewEdit1.Text = '3') and (cb_MB_SETFOREGROUND.Checked) then
        ButtonBtn := 'MB_ABORTRETRYIGNORE or MB_DEFBUTTON3 or MB_SETFOREGROUND';

     {replace in a message string escape /r/n}
     TextMsgIf := StringReplace(MSGText.Lines.GetText, SNewLine, '''#13#10''', [rfReplaceAll]);
  end;

  if cb_TaskDialogMsgBox.Checked then begin
     {TaskDialogMsgBox(TaskInstructionText.Text, TaskMesssageText.Text, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg)}
     ModeMsg := 1;
     {create ButtonLabels array}
     if rbMB_YESNO.Checked or rbMB_YESNOCANCEL.Checked then
        BtnTextArr :=  Button1Text.Text + ''', ''' + Button2Text.Text
     else if rbMB_ABORTRETRYIGNORE.Checked then
        BtnTextArr := 'Retry'', ''Ignore'', ''Abort'
     else
        BtnTextArr := Button1Text.Text;

     {get Shield Flag value}
     if rbMB_OK.Checked and rb_IDOK.Checked then ShieldFlg := 'IDOK';
     if rbMB_OKCANCEL.Checked and rb_IDOK.Checked then ShieldFlg := 'IDOK';
     if rbMB_OKCANCEL.Checked and rb_IDCANCEL.Checked then ShieldFlg := 'IDCANCEL';
     if rbMB_YESNO.Checked and rb_IDYES.Checked then ShieldFlg := 'IDYES';
     if rbMB_YESNO.Checked and rb_IDNO.Checked then ShieldFlg := 'IDNO';
     if rbMB_YESNOCANCEL.Checked and rb_IDYES.Checked then ShieldFlg := 'IDYES';
     if rbMB_YESNOCANCEL.Checked and rb_IDNO.Checked then ShieldFlg := 'IDNO';
     if rbMB_YESNOCANCEL.Checked and rb_IDCANCEL.Checked then ShieldFlg := 'IDCANCEL';
     if rbMB_RETRYCANCEL.Checked and rb_IDRETRY.Checked then ShieldFlg := 'IDRETRY';
     if rbMB_RETRYCANCEL.Checked and rb_IDCANCEL.Checked then ShieldFlg := 'IDCANCEL';
     if rbMB_ABORTRETRYIGNORE.Checked and rb_IDRETRY.Checked then ShieldFlg := 'IDRETRY';
     if rbMB_ABORTRETRYIGNORE.Checked and rb_IDABORT.Checked then ShieldFlg := 'IDABORT';
     if rbMB_ABORTRETRYIGNORE.Checked and rb_IDIGNORE.Checked then ShieldFlg := 'IDIGNORE';

     TextMsgIf := TaskInstructionText.Text + ''', ''' + TaskMesssageText.Text;
  end;

  {selected button OK}
  if (cb_IDOK.Checked and not cb_IDCANCEL.Checked) then begin
     IDButton := 'IDOK';
     TypeMsg := 1;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, '', '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, '', '');
     end;
  end

  {selected button CANCEL}
  else if (cb_IDCANCEL.Checked and not cb_IDOK.Checked and not cb_IDRETRY.Checked and not cb_IDYES.Checked and not cb_IDNO.Checked and not cb_IDABORT.Checked and not cb_IDIGNORE.Checked) then begin
     IDButton := 'IDCANCEL';
     TypeMsg := 1;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, '', '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, '', '');
     end;
  end

  {selected button OK and CANCEL}
  else if (cb_IDCANCEL.Checked and cb_IDOK.Checked and not cb_IDRETRY.Checked and not cb_IDYES.Checked and not cb_IDNO.Checked) then begin
     IDButton := 'IDOK';
     IDButton2 := 'IDCANCEL';
     TypeMsg := 2;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, IDButton2, '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, IDButton2, '');
     end;
  end

  {selected button YES}
  else if (cb_IDYES.Checked and not cb_IDNO.Checked and not cb_IDCANCEL.Checked) then begin
     IDButton := 'IDYES';
     TypeMsg := 1;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, '', '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, '', '');
     end;
  end

  {selected button NO}
  else if (cb_IDNO.Checked and not cb_IDYES.Checked and not cb_IDCANCEL.Checked) then begin
     IDButton := 'IDNO';
     TypeMsg := 1;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, '', '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, '', '');
     end;
  end

  {selected button YES and NO}
  else if (cb_IDYES.Checked and cb_IDNO.Checked and not cb_IDCANCEL.Checked) then begin
     IDButton := 'IDYES';
     IDButton2 := 'IDNO';
     TypeMsg := 2;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, IDButton2, '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, IDButton2, '');
     end;
  end

  {selected button YES and CANCEL}
  else if (cb_IDYES.Checked and not cb_IDNO.Checked and cb_IDCANCEL.Checked) then begin
     IDButton := 'IDYES';
     IDButton2 := 'IDCANCEL';
     TypeMsg := 2;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, IDButton2, '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, IDButton2, '');
     end;
  end

  {selected button NO and CANCEL}
  else if (cb_IDNO.Checked and not cb_IDYES.Checked and cb_IDCANCEL.Checked) then begin
     IDButton := 'IDNO';
     IDButton2 := 'IDCANCEL';
     TypeMsg := 2;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, IDButton2, '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, IDButton2, '');
     end;
  end

  {selected button YES, NO and CANCEL}
  else if (cb_IDYES.Checked and cb_IDNO.Checked and cb_IDCANCEL.Checked) then begin
     IDButton := 'IDYES';
     IDButton2 := 'IDNO';
     IDButton3 := 'IDCANCEL';
     TypeMsg := 3;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, IDButton2, IDButton3);
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, IDButton2, IDButton3);
     end;
  end

  {selected button RETRY}
  else if (cb_IDRETRY.Checked and not cb_IDCANCEL.Checked and not cb_IDABORT.Checked and not cb_IDIGNORE.Checked) then begin
     IDButton := 'IDRETRY';
     TypeMsg := 1;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, '', '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, '', '');
     end;
  end

  {selected button RETRY and CANCEL}
  else if (cb_IDRETRY.Checked and cb_IDCANCEL.Checked and not cb_IDABORT.Checked and not cb_IDIGNORE.Checked) then begin
     IDButton := 'IDRETRY';
     IDButton2 := 'IDCANCEL';
     TypeMsg := 2;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, IDButton2, '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, IDButton2, '');
     end;
  end

  {selected button IGNORE}
  else if (cb_IDIGNORE.Checked and not cb_IDCANCEL.Checked and not cb_IDABORT.Checked and not cb_IDRETRY.Checked) then begin
     IDButton := 'IDIGNORE';
     TypeMsg := 1;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, '', '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, '', '');
     end;
  end

  {selected button ABORT}
  else if (cb_IDABORT.Checked and not cb_IDCANCEL.Checked and not cb_IDRETRY.Checked and not cb_IDIGNORE.Checked) then begin
     IDButton := 'IDABORT';
     TypeMsg := 1;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, '', '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, '', '');
     end;
  end

  {selected button RETRY and IGNORE}
  else if (cb_IDRETRY.Checked and not cb_IDCANCEL.Checked and not cb_IDABORT.Checked and cb_IDIGNORE.Checked) then begin
     IDButton := 'IDRETRY';
     IDButton2 := 'IDIGNORE';
     TypeMsg := 2;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, IDButton2, '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, IDButton2, '');
     end;
  end

  {selected button RETRY and ABORT}
  else if (cb_IDRETRY.Checked and not cb_IDCANCEL.Checked and cb_IDABORT.Checked and not cb_IDIGNORE.Checked) then begin
     IDButton := 'IDRETRY';
     IDButton2 := 'IDABORT';
     TypeMsg := 2;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, IDButton2, '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, IDButton2, '');
     end;
  end

  {selected button IGNORE and ABORT}
  else if (not cb_IDRETRY.Checked and not cb_IDCANCEL.Checked and cb_IDABORT.Checked and cb_IDIGNORE.Checked) then begin
     IDButton := 'IDIGNORE';
     IDButton2 := 'IDABORT';
     TypeMsg := 2;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, IDButton2, '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, IDButton2, '');
     end;
  end

  {selected button RETRY, IGNORE and ABORT}
  else if (cb_IDRETRY.Checked and not cb_IDCANCEL.Checked and cb_IDABORT.Checked and cb_IDIGNORE.Checked) then begin
     IDButton := 'IDRETRY';
     IDButton2 := 'IDIGNORE';
     IDButton3 := 'IDABORT';
     TypeMsg := 3;
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, IDButton, IDButton2, IDButton3);
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, IDButton, IDButton2, IDButton3);
     end;
  end

  {no selected buttons}
  else begin
     case ModeMsg of
        0: TextMsgIf := TextMsg(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, '', '', '');
        1: TextMsgIf := TextTask(TypeMsg, TextMsgIf, IconTypes, ButtonBtn, BtnTextArr, ShieldFlg, '', '', '');
     end;
  end;

  MSGTextInsert.Add(TextMsgIf);
end;

end.
