unit IDE.MsgBoxDesignerForm;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  MessageBox Designer form
  
  Originally contributed by leserg73
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UIStateForm, StdCtrls, ExtCtrls, NewStaticText, ComCtrls, pngimage;

type
  TMsgBoxDesignerForm = class(TUIStateForm)
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
    TaskMessageLabel: TNewStaticText;
    TaskInstructionText: TEdit;
    TaskMessageText: TEdit;
    Button1Text: TEdit;
    Button2Text: TEdit;
    Button1Label: TNewStaticText;
    Button2Label: TNewStaticText;
    cb_Suppressible: TCheckBox;
    cb_DefIDOK: TRadioButton;
    cb_DefIDCANCEL: TRadioButton;
    cb_DefIDYES: TRadioButton;
    cb_DefIDNO: TRadioButton;
    cb_DefIDABORT: TRadioButton;
    cb_DefIDRETRY: TRadioButton;
    cb_DefIDIGNORE: TRadioButton;
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
    procedure cb_SuppressibleClick(Sender: TObject);
    procedure MSGTextKeyPress(Sender: TObject; var Key: Char);
  public
    function GetText(TabWidth: Integer; UseTabCharacter: Boolean): String;
  end;

implementation

uses
  Shared.CommonFunc.Vcl, Shared.CommonFunc, IDE.HelperFunc, Shared.TaskDialogFunc, IDE.Messages;

{$R *.DFM}

procedure TMsgBoxDesignerForm.FormCreate(Sender: TObject);
begin
  InitFormFont(Self);

  cb_Suppressible.Checked := True;
  MSGText.Lines[MSGText.CaretPos.Y] := '<Enter your text here...>';
  MSGText.SelStart := MSGText.Perform(EM_LINEINDEX, 0, 0);
  MSGText.SelLength := Length(MSGText.Lines[0]);
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
  TaskMessageLabel.Visible := False;
  TaskInstructionText.Visible := False;
  TaskMessageText.Visible := False;
  Button1Text.Visible := False;
  Button2Text.Visible := False;
  Button1Label.Visible := False;
  Button2Label.Visible := False;
end;

procedure TMsgBoxDesignerForm.rbMB_OKClick(Sender: TObject);
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
  if cb_Suppressible.Checked then begin
     cb_DefIDOK.Checked := True;
     cb_DefIDCANCEL.Checked := False;
     cb_DefIDYES.Checked := False;
     cb_DefIDNO.Checked := False;
     cb_DefIDABORT.Checked := False;
     cb_DefIDRETRY.Checked := False;
     cb_DefIDIGNORE.Checked := False;
     cb_DefIDOK.Enabled := True;
     cb_DefIDCANCEL.Enabled := False;
     cb_DefIDYES.Enabled := False;
     cb_DefIDNO.Enabled := False;
     cb_DefIDABORT.Enabled := False;
     cb_DefIDRETRY.Enabled := False;
     cb_DefIDIGNORE.Enabled := False;
  end;
end;

procedure TMsgBoxDesignerForm.rbMB_YESNOClick(Sender: TObject);
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
  if cb_Suppressible.Checked then begin
     cb_DefIDOK.Checked := False;
     cb_DefIDCANCEL.Checked := False;
     cb_DefIDYES.Checked := True;
     cb_DefIDNO.Checked := False;
     cb_DefIDABORT.Checked := False;
     cb_DefIDRETRY.Checked := False;
     cb_DefIDIGNORE.Checked := False;
     cb_DefIDOK.Enabled := False;
     cb_DefIDCANCEL.Enabled := False;
     cb_DefIDYES.Enabled := True;
     cb_DefIDNO.Enabled := True;
     cb_DefIDABORT.Enabled := False;
     cb_DefIDRETRY.Enabled := False;
     cb_DefIDIGNORE.Enabled := False;
  end;
end;

procedure TMsgBoxDesignerForm.rbMB_OKCANCELClick(Sender: TObject);
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
  if cb_Suppressible.Checked then begin
     cb_DefIDOK.Checked := True;
     cb_DefIDCANCEL.Checked := False;
     cb_DefIDYES.Checked := False;
     cb_DefIDNO.Checked := False;
     cb_DefIDABORT.Checked := False;
     cb_DefIDRETRY.Checked := False;
     cb_DefIDIGNORE.Checked := False;
     cb_DefIDOK.Enabled := True;
     cb_DefIDCANCEL.Enabled := True;
     cb_DefIDYES.Enabled := False;
     cb_DefIDNO.Enabled := False;
     cb_DefIDABORT.Enabled := False;
     cb_DefIDRETRY.Enabled := False;
     cb_DefIDIGNORE.Enabled := False;
  end;
end;

procedure TMsgBoxDesignerForm.rbMB_RETRYCANCELClick(Sender: TObject);
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
  if cb_Suppressible.Checked then begin
     cb_DefIDOK.Checked := False;
     cb_DefIDCANCEL.Checked := True;
     cb_DefIDYES.Checked := False;
     cb_DefIDNO.Checked := False;
     cb_DefIDABORT.Checked := False;
     cb_DefIDRETRY.Checked := False;
     cb_DefIDIGNORE.Checked := False;
     cb_DefIDOK.Enabled := False;
     cb_DefIDCANCEL.Enabled := True;
     cb_DefIDYES.Enabled := False;
     cb_DefIDNO.Enabled := False;
     cb_DefIDABORT.Enabled := False;
     cb_DefIDRETRY.Enabled := True;
     cb_DefIDIGNORE.Enabled := False;
  end;
end;

procedure TMsgBoxDesignerForm.rbMB_YESNOCANCELClick(Sender: TObject);
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
  if cb_Suppressible.Checked then begin
     cb_DefIDOK.Checked := False;
     cb_DefIDCANCEL.Checked := False;
     cb_DefIDYES.Checked := True;
     cb_DefIDNO.Checked := False;
     cb_DefIDABORT.Checked := False;
     cb_DefIDRETRY.Checked := False;
     cb_DefIDIGNORE.Checked := False;
     cb_DefIDOK.Enabled := False;
     cb_DefIDCANCEL.Enabled := True;
     cb_DefIDYES.Enabled := True;
     cb_DefIDNO.Enabled := True;
     cb_DefIDABORT.Enabled := False;
     cb_DefIDRETRY.Enabled := False;
     cb_DefIDIGNORE.Enabled := False;
  end;
end;

procedure TMsgBoxDesignerForm.rbMB_ABORTRETRYIGNOREClick(Sender: TObject);
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
  if cb_Suppressible.Checked then begin
     cb_DefIDOK.Checked := False;
     cb_DefIDCANCEL.Checked := False;
     cb_DefIDYES.Checked := False;
     cb_DefIDNO.Checked := False;
     cb_DefIDABORT.Checked := False;
     cb_DefIDRETRY.Checked := False;
     cb_DefIDIGNORE.Checked := True;
     cb_DefIDOK.Enabled := False;
     cb_DefIDCANCEL.Enabled := False;
     cb_DefIDYES.Enabled := False;
     cb_DefIDNO.Enabled := False;
     cb_DefIDABORT.Enabled := True;
     cb_DefIDRETRY.Enabled := True;
     cb_DefIDIGNORE.Enabled := True;
  end;
end;

procedure TMsgBoxDesignerForm.UpDown1Changing(Sender: TObject; var AllowChange: Boolean);
begin
   AllowChange := True;
end;

procedure TMsgBoxDesignerForm.cb_MsgBoxClick(Sender: TObject);
begin
   IMGmbConfirmation.Visible := True;
   cb_MB_SETFOREGROUND.Enabled := True;
   cb_MB_SETFOREGROUND.Checked := False;
   GroupBox1.Visible := True;
   if not cb_Suppressible.Checked then begin
      GroupBox4.Caption := ' Return values ';
      cb_DefIDOK.Visible := False;
      cb_DefIDCANCEL.Visible := False;
      cb_DefIDYES.Visible := False;
      cb_DefIDNO.Visible := False;
      cb_DefIDABORT.Visible := False;
      cb_DefIDRETRY.Visible := False;
      cb_DefIDIGNORE.Visible := False;
   end
   else begin
     GroupBox4.Caption := ' Return values /  -------- / Default ';
     cb_DefIDOK.Visible := True;
     cb_DefIDCANCEL.Visible := True;
     cb_DefIDYES.Visible := True;
     cb_DefIDNO.Visible := True;
     cb_DefIDABORT.Visible := True;
     cb_DefIDRETRY.Visible := True;
     cb_DefIDIGNORE.Visible := True;
   end;
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
   TaskMessageLabel.Visible := False;
   TaskInstructionText.Visible := False;
   TaskMessageText.Visible := False;
   Button1Text.Visible := False;
   Button2Text.Visible := False;
   Button1Label.Visible := False;
   Button2Label.Visible := False;
   rbMB_OK.Checked := True;
   rbMB_OKClick(Self);
end;

procedure TMsgBoxDesignerForm.cb_SuppressibleClick(Sender: TObject);
begin
   if cb_Suppressible.Checked then begin
     cb_DefIDOK.Visible := True;
     cb_DefIDCANCEL.Visible := True;
     cb_DefIDYES.Visible := True;
     cb_DefIDNO.Visible := True;
     cb_DefIDABORT.Visible := True;
     cb_DefIDRETRY.Visible := True;
     cb_DefIDIGNORE.Visible := True;
     if cb_MsgBox.Checked then
        GroupBox4.Caption := ' Return values /  -------- / Default ';
     if cb_TaskDialogMsgBox.Checked then
        GroupBox4.Caption := ' Return values /  Shield  / Default ';
   end
   else begin
     cb_DefIDOK.Checked := False;
     cb_DefIDCANCEL.Checked := False;
     cb_DefIDYES.Checked := False;
     cb_DefIDNO.Checked := False;
     cb_DefIDABORT.Checked := False;
     cb_DefIDRETRY.Checked := False;
     cb_DefIDIGNORE.Checked := False;
     cb_DefIDOK.Visible := False;
     cb_DefIDCANCEL.Visible := False;
     cb_DefIDYES.Visible := False;
     cb_DefIDNO.Visible := False;
     cb_DefIDABORT.Visible := False;
     cb_DefIDRETRY.Visible := False;
     cb_DefIDIGNORE.Visible := False;
     if cb_MsgBox.Checked then
        GroupBox4.Caption := ' Return values ';
     if cb_TaskDialogMsgBox.Checked then
        GroupBox4.Caption := ' Return values /  Shield ';
   end;
   if rbMB_OK.Checked then rbMB_OKClick(Self);
   if rbMB_OKCANCEL.Checked then rbMB_OKCANCELClick(Self);
   if rbMB_YESNO.Checked then rbMB_YESNOClick(Self);
   if rbMB_YESNOCANCEL.Checked then rbMB_YESNOCANCELClick(Self);
   if rbMB_RETRYCANCEL.Checked then rbMB_RETRYCANCELClick(Self);
   if rbMB_ABORTRETRYIGNORE.Checked then rbMB_ABORTRETRYIGNOREClick(Self);
end;

procedure TMsgBoxDesignerForm.cb_TaskDialogMsgBoxClick(Sender: TObject);
begin
   IMGmbConfirmation.Visible := False;
   cb_MB_SETFOREGROUND.Enabled := False;
   cb_MB_SETFOREGROUND.Checked := False;
   GroupBox1.Visible := False;
   if not cb_Suppressible.Checked then begin
     GroupBox4.Caption := ' Return values /  Shield ';
      cb_DefIDOK.Visible := False;
      cb_DefIDCANCEL.Visible := False;
      cb_DefIDYES.Visible := False;
      cb_DefIDNO.Visible := False;
      cb_DefIDABORT.Visible := False;
      cb_DefIDRETRY.Visible := False;
      cb_DefIDIGNORE.Visible := False;
   end
   else begin
     GroupBox4.Caption := ' Return values /  Shield  / Default ';
     cb_DefIDOK.Visible := True;
     cb_DefIDCANCEL.Visible := True;
     cb_DefIDYES.Visible := True;
     cb_DefIDNO.Visible := True;
     cb_DefIDABORT.Visible := True;
     cb_DefIDRETRY.Visible := True;
     cb_DefIDIGNORE.Visible := True;
   end;
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
   TaskMessageLabel.Visible := True;
   TaskInstructionText.Visible := True;
   TaskMessageText.Visible := True;
   Button1Text.Visible := True;
   Button2Text.Visible := True;
   Button1Label.Visible := True;
   Button2Label.Visible := True;
   TaskInstructionText.Text := 'Instruction Text';
   TaskMessageText.Text := 'Message Text';
   rbMB_OK.Checked := True;
   rbMB_OKClick(Self);
end;

procedure TMsgBoxDesignerForm.rb_IDOKClick(Sender: TObject);
begin
   if rb_IDOK.Checked then
      rb_IDCANCEL.Checked := False;
end;

procedure TMsgBoxDesignerForm.rb_IDCANCELClick(Sender: TObject);
begin
   if rb_IDCANCEL.Checked then begin
      rb_IDOK.Checked := False;
      rb_IDNO.Checked := False;
      rb_IDYES.Checked := False;
      rb_IDRETRY.Checked := False;
   end;
end;

procedure TMsgBoxDesignerForm.rb_IDYESClick(Sender: TObject);
begin
   if rb_IDYES.Checked then begin
      rb_IDNO.Checked := False;
      rb_IDCANCEL.Checked := False;
   end;
end;

procedure TMsgBoxDesignerForm.rb_IDNOClick(Sender: TObject);
begin
   if rb_IDNO.Checked then begin
      rb_IDYES.Checked := False;
      rb_IDCANCEL.Checked := False;
   end;
end;

procedure TMsgBoxDesignerForm.rb_IDRETRYClick(Sender: TObject);
begin
   if rb_IDRETRY.Checked then begin
      rb_IDCANCEL.Checked := False;
      rb_IDABORT.Checked := False;
      rb_IDIGNORE.Checked := False;
   end;
end;

procedure TMsgBoxDesignerForm.rb_IDIGNOREClick(Sender: TObject);
begin
   if rb_IDIGNORE.Checked then begin
      rb_IDABORT.Checked := False;
      rb_IDRETRY.Checked := False;
   end;
end;

procedure TMsgBoxDesignerForm.rb_IDABORTClick(Sender: TObject);
begin
   if rb_IDABORT.Checked then begin
      rb_IDIGNORE.Checked := False;
      rb_IDRETRY.Checked := False;
   end;
end;

procedure TMsgBoxDesignerForm.MBDButtonPreviewClick(Sender: TObject);
begin
  { default value }
  var Buttons := MB_OK;
  var Typ := mbInformation;

  { icon and caption set }
  var Caption: String;
  if rb_mbInformation.Checked then begin
     Caption := 'Info';
     Typ := mbInformation;
  end;
  if rb_mbConfirmation.Checked then begin
     Caption := 'Confirm';
     Typ := mbConfirmation;
  end;
  if rb_mbError.Checked then begin
     Caption := 'Error';
     Typ := mbError;
  end;
  if rb_mbCriticalError.Checked then begin
     Caption := 'Fatal Error';
     Typ := mbCriticalError;
  end;

  { button type set }
  if rbMB_OK.Checked then Buttons := MB_OK;
  if rbMB_OKCANCEL.Checked then Buttons := MB_OKCANCEL;
  if rbMB_YESNO.Checked then Buttons := MB_YESNO;
  if rbMB_YESNOCANCEL.Checked then Buttons := MB_YESNOCANCEL;
  if rbMB_RETRYCANCEL.Checked then Buttons := MB_RETRYCANCEL;
  if rbMB_ABORTRETRYIGNORE.Checked then Buttons := MB_ABORTRETRYIGNORE;

  if cb_MsgBox.Checked then begin
    if MSGText.GetTextLen = 0 then
       MSGText.Lines.Add('Your message text.');
    { MessageBox with DefButton }
    if NewEdit1.Text = '1' then
       MsgBox(MSGText.Lines.GetText, Caption, Typ, Buttons);
    if NewEdit1.Text = '2' then
       MsgBox(MSGText.Lines.GetText, Caption, Typ, Buttons or MB_DEFBUTTON2);
    if NewEdit1.Text = '3' then
       MsgBox(MSGText.Lines.GetText, Caption, Typ, Buttons or MB_DEFBUTTON3);
    { MessageBox with DefButton and Flag MB_SETFOREGROUND }
    if (NewEdit1.Text = '1') and (cb_MB_SETFOREGROUND.Checked) then
       MsgBox(MSGText.Lines.GetText, Caption, Typ, Buttons or MB_SETFOREGROUND);
    if (NewEdit1.Text = '2') and (cb_MB_SETFOREGROUND.Checked) then
       MsgBox(MSGText.Lines.GetText, Caption, Typ, Buttons or MB_DEFBUTTON2 or MB_SETFOREGROUND);
    if (NewEdit1.Text = '3') and (cb_MB_SETFOREGROUND.Checked) then
       MsgBox(MSGText.Lines.GetText, Caption, Typ, Buttons or MB_DEFBUTTON3 or MB_SETFOREGROUND);
  end else if cb_TaskDialogMsgBox.Checked then begin
     { create ButtonLabels array }
     var ButtonLabels: TArray<string>;
     if rbMB_YESNO.Checked or rbMB_YESNOCANCEL.Checked then
        ButtonLabels := TArray<string>.Create(Button1Text.Text, Button2Text.Text)
     else if rbMB_ABORTRETRYIGNORE.Checked then
        ButtonLabels := TArray<string>.Create('Retry', 'Ignore', 'Abort')
     else
        ButtonLabels := TArray<string>.Create(Button1Text.Text);

     { get Shield Flag value }
     var ShieldButton := 0;
     if rbMB_OK.Checked and rb_IDOK.Checked then ShieldButton := IDOK;
     if rbMB_OKCANCEL.Checked and rb_IDOK.Checked then ShieldButton := IDOK;
     if rbMB_OKCANCEL.Checked and rb_IDCANCEL.Checked then ShieldButton := IDCANCEL;
     if rbMB_YESNO.Checked and rb_IDYES.Checked then ShieldButton := IDYES;
     if rbMB_YESNO.Checked and rb_IDNO.Checked then ShieldButton := IDNO;
     if rbMB_YESNOCANCEL.Checked and rb_IDYES.Checked then ShieldButton := IDYES;
     if rbMB_YESNOCANCEL.Checked and rb_IDNO.Checked then ShieldButton := IDNO;
     if rbMB_YESNOCANCEL.Checked and rb_IDCANCEL.Checked then ShieldButton := IDCANCEL;
     if rbMB_RETRYCANCEL.Checked and rb_IDRETRY.Checked then ShieldButton := IDRETRY;
     if rbMB_RETRYCANCEL.Checked and rb_IDCANCEL.Checked then ShieldButton := IDCANCEL;
     if rbMB_ABORTRETRYIGNORE.Checked and rb_IDRETRY.Checked then ShieldButton := IDRETRY;
     if rbMB_ABORTRETRYIGNORE.Checked and rb_IDABORT.Checked then ShieldButton := IDABORT;
     if rbMB_ABORTRETRYIGNORE.Checked and rb_IDIGNORE.Checked then ShieldButton := IDIGNORE;

     { TaskDialogMsgBox(Icon, Instruction, Text, Caption, Typ, Buttons, ButtonLabels, ShieldButton) }
     TaskDialogMsgBox('', TaskInstructionText.Text, TaskMessageText.Text, Caption,
                      Typ, Buttons, ButtonLabels, ShieldButton);
  end;
end;

procedure TMsgBoxDesignerForm.MSGTextKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then begin
    MBDButtonCancel.Click;
    Key := #0;
  end;
end;

function TMsgBoxDesignerForm.GetText(TabWidth: Integer; UseTabCharacter: Boolean): String;

  function TextTab: String;
  begin     
    if UseTabCharacter then
      Result := #9
    else
      Result := Format('%*s', [TabWidth, '']);
  end;

  function TextUserClicked(IDButton: String): String;
  begin
    Result := 'user clicked ' + StringReplace(IDButton, 'ID', '', [])
  end;

  function TextCase(IDButton: String): String;
  begin
    Result := TextTab + IDButton +': { ' + TextUserClicked(IDButton) + ' };';
  end;

  function TextCall(ButtonCount: Integer; SCall, IDButton, IDButton2, IDButton3: String): String;
  begin
    case ButtonCount of
      0: Result := SCall + ';';
      1: Result := 'if ' + SCall + ' = ' + IDButton + ' then' + SNewLine +
                   'begin' + SNewLine +
                   TextTab + '// ' + TextUserClicked(IDButton) + SNewLine +
                   'end;';
      2, 3:
        begin
          Result := 'case ' + SCall + ' of ' + SNewLine +
                    TextCase(IDButton) + SNewLine +
                    TextCase(IDButton2) + SNewLine;
          if ButtonCount = 3 then
            Result := Result + TextCase(IDButton3) + SNewLine;
          Result := Result + 'end;';
        end;
    end;
  end;

   { MsgBox / SuppressibleMsgBox }
  function TextMsgBox(ButtonCount: Integer; Caption, Typ, Buttons,
    IDButton, IDButton2, IDButton3: String): String;
  begin
    var SMsgBox: String;
    if not cb_Suppressible.Checked then
      SMsgBox := 'MsgBox'
    else
      SMsgBox := 'SuppressibleMsgBox';
    //Buttons also contains Default if suppressible, pre-separated by a comma (but ButtonCount does *not* include this)
    SMsgBox := SMsgBox + '(''' + Caption + ''', ' + Typ + ', ' + Buttons + ')';
    Result := TextCall(ButtonCount, SMsgBox, IDButton, IDButton2, IDButton3);
  end;

  { TaskDialogMsgBox / SuppressibleTaskDialogMsgBox }
  function TextTaskDialog(ButtonCount: Integer; InstructionAndText, Typ, Buttons,
    ButtonLabels, ShieldButton, IDButton, IDButton2, IDButton3: String): String;
  begin
    var STaskDialog: String;
    if not cb_Suppressible.Checked then
      STaskDialog := 'TaskDialogMsgBox'
    else
      STaskDialog := 'SuppressibleTaskDialogMsgBox';
    //InstructionAndText contains both Instruction and Text, pre-separated by a quote, a comma and another quote
    //ButtonLabels contains all labels, also pre-separated by the above
    //ShieldButton also contains Default if suppressible, pre-separated by a comma (but ButtonCount does *not* include this)
    STaskDialog := STaskDialog+ '(''' + InstructionAndText + ''', ' + Typ + ', ' + Buttons + ', [''' + ButtonLabels + '''], ' + ShieldButton + ')';
    Result := TextCall(ButtonCount, STaskDialog, IDButton, IDButton2, IDButton3);
  end;

begin
  { default value }
  var ButtonCount := 0;
  var Buttons := 'MB_OK';
  var Typ := 'mbInformation';
  var ShieldButton: String := '0';
  var SuppressibleDefault := '';

  { make a string with Default parameter for Suppressible* calls }
  if cb_Suppressible.Checked then begin
    if cb_DefIDOK.Checked then SuppressibleDefault := ', IDOK';
    if cb_DefIDCANCEL.Checked then SuppressibleDefault := ', IDCANCEL';
    if cb_DefIDYES.Checked then SuppressibleDefault := ', IDYES';
    if cb_DefIDNO.Checked then SuppressibleDefault := ', IDNO';
    if cb_DefIDABORT.Checked then SuppressibleDefault := ', IDABORT';
    if cb_DefIDRETRY.Checked then SuppressibleDefault := ', IDRETRY';
    if cb_DefIDIGNORE.Checked then SuppressibleDefault := ', IDIGNORE';
  end;

  { icon and caption set }
  if rb_mbInformation.Checked then begin
     Typ := 'mbInformation';
  end;
  if rb_mbConfirmation.Checked then begin
     Typ := 'mbConfirmation';
  end;
  if rb_mbError.Checked then begin
     Typ := 'mbError';
  end;
  if rb_mbCriticalError.Checked then begin
     Typ := 'mbCriticalError';
  end;

  { button type set }
  if rbMB_OK.Checked then Buttons := 'MB_OK';
  if rbMB_OKCANCEL.Checked then Buttons := 'MB_OKCANCEL';
  if rbMB_YESNO.Checked then Buttons := 'MB_YESNO';
  if rbMB_YESNOCANCEL.Checked then Buttons := 'MB_YESNOCANCEL';
  if rbMB_RETRYCANCEL.Checked then Buttons := 'MB_RETRYCANCEL';
  if rbMB_ABORTRETRYIGNORE.Checked then Buttons := 'MB_ABORTRETRYIGNORE';

  var ModeMsg: Integer;
  var CaptionOrInstructionAndText: String;
  var ButtonLabels: String;

  if cb_MsgBox.Checked then begin
     { MsgBox(Text, Typ, Buttons); }
     ModeMsg := 0;

     { MessageBox with DefButton and Flag MB_SETFOREGROUND }
     if (rbMB_OK.Checked) and (cb_MB_SETFOREGROUND.Checked) then
        Buttons := 'MB_OK or MB_SETFOREGROUND';

     if (rbMB_OKCANCEL.Checked) and (NewEdit1.Text = '2') then
        Buttons := 'MB_OKCANCEL or MB_DEFBUTTON2';
     { MessageBox with DefButton and Flag MB_SETFOREGROUND }
     if (rbMB_OKCANCEL.Checked) and (cb_MB_SETFOREGROUND.Checked) then
        Buttons := 'MB_OKCANCEL or MB_SETFOREGROUND';
     if (rbMB_OKCANCEL.Checked) and (NewEdit1.Text = '2') and (cb_MB_SETFOREGROUND.Checked) then
        Buttons := 'MB_OKCANCEL or MB_DEFBUTTON2 or MB_SETFOREGROUND';

     if (rbMB_YESNO.Checked) and (NewEdit1.Text = '2') then
        Buttons := 'MB_YESNO or MB_DEFBUTTON2';
     { MessageBox with DefButton and Flag MB_SETFOREGROUND }
     if (rbMB_YESNO.Checked) and (cb_MB_SETFOREGROUND.Checked) then
        Buttons := 'MB_YESNO or MB_SETFOREGROUND';
     if (rbMB_YESNO.Checked) and (NewEdit1.Text = '2') and (cb_MB_SETFOREGROUND.Checked) then
        Buttons := 'MB_YESNO or MB_DEFBUTTON2 or MB_SETFOREGROUND';

     if (rbMB_RETRYCANCEL.Checked) and (NewEdit1.Text = '2') then
        Buttons := 'MB_RETRYCANCEL or MB_DEFBUTTON2';
     { MessageBox with DefButton and Flag MB_SETFOREGROUND }
     if (rbMB_RETRYCANCEL.Checked) and (cb_MB_SETFOREGROUND.Checked) then
        Buttons := 'MB_RETRYCANCEL or MB_SETFOREGROUND';
     if (rbMB_RETRYCANCEL.Checked) and (NewEdit1.Text = '2') and (cb_MB_SETFOREGROUND.Checked) then
        Buttons := 'MB_RETRYCANCEL or MB_DEFBUTTON2 or MB_SETFOREGROUND';

     if (rbMB_YESNOCANCEL.Checked) and (NewEdit1.Text = '2') then
        Buttons := 'MB_YESNOCANCEL or MB_DEFBUTTON2';
     if (rbMB_YESNOCANCEL.Checked) and (NewEdit1.Text = '3') then
        Buttons := 'MB_YESNOCANCEL or MB_DEFBUTTON3';
     { MessageBox with DefButton and Flag MB_SETFOREGROUND }
     if (rbMB_YESNOCANCEL.Checked) and (cb_MB_SETFOREGROUND.Checked) then
        Buttons := 'MB_YESNOCANCEL or MB_SETFOREGROUND';
     if (rbMB_YESNOCANCEL.Checked) and (NewEdit1.Text = '2') and (cb_MB_SETFOREGROUND.Checked) then
        Buttons := 'MB_YESNOCANCEL or MB_DEFBUTTON2 or MB_SETFOREGROUND';
     if (rbMB_YESNOCANCEL.Checked) and (NewEdit1.Text = '3') and (cb_MB_SETFOREGROUND.Checked) then
        Buttons := 'MB_YESNOCANCEL or MB_DEFBUTTON3 or MB_SETFOREGROUND';

     if (rbMB_ABORTRETRYIGNORE.Checked) and (NewEdit1.Text = '2') then
        Buttons := 'MB_ABORTRETRYIGNORE or MB_DEFBUTTON2';
     if (rbMB_ABORTRETRYIGNORE.Checked) and (NewEdit1.Text = '3') then
        Buttons := 'MB_ABORTRETRYIGNORE or MB_DEFBUTTON3';
     { MessageBox with DefButton and Flag MB_SETFOREGROUND }
     if (rbMB_ABORTRETRYIGNORE.Checked) and (cb_MB_SETFOREGROUND.Checked) then
        Buttons := 'MB_ABORTRETRYIGNORE or MB_SETFOREGROUND';
     if (rbMB_ABORTRETRYIGNORE.Checked) and (NewEdit1.Text = '2') and (cb_MB_SETFOREGROUND.Checked) then
        Buttons := 'MB_ABORTRETRYIGNORE or MB_DEFBUTTON2 or MB_SETFOREGROUND';
     if (rbMB_ABORTRETRYIGNORE.Checked) and (NewEdit1.Text = '3') and (cb_MB_SETFOREGROUND.Checked) then
        Buttons := 'MB_ABORTRETRYIGNORE or MB_DEFBUTTON3 or MB_SETFOREGROUND';

     { Suppressible msg }
     if cb_Suppressible.Checked then Buttons := Buttons + SuppressibleDefault;

     { replace in a message string escape /r/n }
     CaptionOrInstructionAndText := StringReplace(MSGText.Lines.GetText, SNewLine, '''#13#10''', [rfReplaceAll]);
  end else begin
     { TaskDialogMsgBox(TaskInstructionText.Text, TaskMessageText.Text, Typ, Buttons, ButtonLabels, ShieldButton) }
     ModeMsg := 1;

     { create ButtonLabels array }
     if rbMB_YESNO.Checked or rbMB_YESNOCANCEL.Checked then
        ButtonLabels :=  Button1Text.Text + ''', ''' + Button2Text.Text
     else if rbMB_ABORTRETRYIGNORE.Checked then
        ButtonLabels := 'Retry'', ''Ignore'', ''Abort'
     else
        ButtonLabels := Button1Text.Text;

     { get Shield Flag value }
     if rbMB_OK.Checked and rb_IDOK.Checked then ShieldButton := 'IDOK';
     if rbMB_OKCANCEL.Checked and rb_IDOK.Checked then ShieldButton := 'IDOK';
     if rbMB_OKCANCEL.Checked and rb_IDCANCEL.Checked then ShieldButton := 'IDCANCEL';
     if rbMB_YESNO.Checked and rb_IDYES.Checked then ShieldButton := 'IDYES';
     if rbMB_YESNO.Checked and rb_IDNO.Checked then ShieldButton := 'IDNO';
     if rbMB_YESNOCANCEL.Checked and rb_IDYES.Checked then ShieldButton := 'IDYES';
     if rbMB_YESNOCANCEL.Checked and rb_IDNO.Checked then ShieldButton := 'IDNO';
     if rbMB_YESNOCANCEL.Checked and rb_IDCANCEL.Checked then ShieldButton := 'IDCANCEL';
     if rbMB_RETRYCANCEL.Checked and rb_IDRETRY.Checked then ShieldButton := 'IDRETRY';
     if rbMB_RETRYCANCEL.Checked and rb_IDCANCEL.Checked then ShieldButton := 'IDCANCEL';
     if rbMB_ABORTRETRYIGNORE.Checked and rb_IDRETRY.Checked then ShieldButton := 'IDRETRY';
     if rbMB_ABORTRETRYIGNORE.Checked and rb_IDABORT.Checked then ShieldButton := 'IDABORT';
     if rbMB_ABORTRETRYIGNORE.Checked and rb_IDIGNORE.Checked then ShieldButton := 'IDIGNORE';

     { Suppressible msg }
     if cb_Suppressible.Checked then ShieldButton := ShieldButton + SuppressibleDefault;

     CaptionOrInstructionAndText := TaskInstructionText.Text + ''', ''' + TaskMessageText.Text;
  end;

  var IDButton, IDButton2, IDButton3: String;
  var Text: String;

  { selected button OK }
  if (cb_IDOK.Checked and not cb_IDCANCEL.Checked) then begin
     IDButton := 'IDOK';
     ButtonCount := 1;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, '', '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, '', '');
     end;
  end

  { selected button CANCEL }
  else if (cb_IDCANCEL.Checked and not cb_IDOK.Checked and not cb_IDRETRY.Checked and not cb_IDYES.Checked and not cb_IDNO.Checked and not cb_IDABORT.Checked and not cb_IDIGNORE.Checked) then begin
     IDButton := 'IDCANCEL';
     ButtonCount := 1;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, '', '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, '', '');
     end;
  end

  { selected button OK and CANCEL }
  else if (cb_IDCANCEL.Checked and cb_IDOK.Checked and not cb_IDRETRY.Checked and not cb_IDYES.Checked and not cb_IDNO.Checked) then begin
     IDButton := 'IDOK';
     IDButton2 := 'IDCANCEL';
     ButtonCount := 2;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, IDButton2, '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, IDButton2, '');
     end;
  end

  { selected button YES }
  else if (cb_IDYES.Checked and not cb_IDNO.Checked and not cb_IDCANCEL.Checked) then begin
     IDButton := 'IDYES';
     ButtonCount := 1;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, '', '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, '', '');
     end;
  end

  { selected button NO }
  else if (cb_IDNO.Checked and not cb_IDYES.Checked and not cb_IDCANCEL.Checked) then begin
     IDButton := 'IDNO';
     ButtonCount := 1;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, '', '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, '', '');
     end;
  end

  { selected button YES and NO }
  else if (cb_IDYES.Checked and cb_IDNO.Checked and not cb_IDCANCEL.Checked) then begin
     IDButton := 'IDYES';
     IDButton2 := 'IDNO';
     ButtonCount := 2;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, IDButton2, '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, IDButton2, '');
     end;
  end

  { selected button YES and CANCEL }
  else if (cb_IDYES.Checked and not cb_IDNO.Checked and cb_IDCANCEL.Checked) then begin
     IDButton := 'IDYES';
     IDButton2 := 'IDCANCEL';
     ButtonCount := 2;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, IDButton2, '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, IDButton2, '');
     end;
  end

  { selected button NO and CANCEL }
  else if (cb_IDNO.Checked and not cb_IDYES.Checked and cb_IDCANCEL.Checked) then begin
     IDButton := 'IDNO';
     IDButton2 := 'IDCANCEL';
     ButtonCount := 2;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, IDButton2, '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, IDButton2, '');
     end;
  end

  { selected button YES, NO and CANCEL }
  else if (cb_IDYES.Checked and cb_IDNO.Checked and cb_IDCANCEL.Checked) then begin
     IDButton := 'IDYES';
     IDButton2 := 'IDNO';
     IDButton3 := 'IDCANCEL';
     ButtonCount := 3;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, IDButton2, IDButton3);
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, IDButton2, IDButton3);
     end;
  end

  { selected button RETRY }
  else if (cb_IDRETRY.Checked and not cb_IDCANCEL.Checked and not cb_IDABORT.Checked and not cb_IDIGNORE.Checked) then begin
     IDButton := 'IDRETRY';
     ButtonCount := 1;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, '', '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, '', '');
     end;
  end

  { selected button RETRY and CANCEL }
  else if (cb_IDRETRY.Checked and cb_IDCANCEL.Checked and not cb_IDABORT.Checked and not cb_IDIGNORE.Checked) then begin
     IDButton := 'IDRETRY';
     IDButton2 := 'IDCANCEL';
     ButtonCount := 2;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, IDButton2, '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, IDButton2, '');
     end;
  end

  { selected button IGNORE }
  else if (cb_IDIGNORE.Checked and not cb_IDCANCEL.Checked and not cb_IDABORT.Checked and not cb_IDRETRY.Checked) then begin
     IDButton := 'IDIGNORE';
     ButtonCount := 1;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, '', '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, '', '');
     end;
  end

  { selected button ABORT }
  else if (cb_IDABORT.Checked and not cb_IDCANCEL.Checked and not cb_IDRETRY.Checked and not cb_IDIGNORE.Checked) then begin
     IDButton := 'IDABORT';
     ButtonCount := 1;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, '', '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, '', '');
     end;
  end

  { selected button RETRY and IGNORE }
  else if (cb_IDRETRY.Checked and not cb_IDCANCEL.Checked and not cb_IDABORT.Checked and cb_IDIGNORE.Checked) then begin
     IDButton := 'IDRETRY';
     IDButton2 := 'IDIGNORE';
     ButtonCount := 2;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, IDButton2, '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, IDButton2, '');
     end;
  end

  { selected button RETRY and ABORT }
  else if (cb_IDRETRY.Checked and not cb_IDCANCEL.Checked and cb_IDABORT.Checked and not cb_IDIGNORE.Checked) then begin
     IDButton := 'IDRETRY';
     IDButton2 := 'IDABORT';
     ButtonCount := 2;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, IDButton2, '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, IDButton2, '');
     end;
  end

  { selected button IGNORE and ABORT }
  else if (not cb_IDRETRY.Checked and not cb_IDCANCEL.Checked and cb_IDABORT.Checked and cb_IDIGNORE.Checked) then begin
     IDButton := 'IDIGNORE';
     IDButton2 := 'IDABORT';
     ButtonCount := 2;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, IDButton2, '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, IDButton2, '');
     end;
  end

  { selected button RETRY, IGNORE and ABORT }
  else if (cb_IDRETRY.Checked and not cb_IDCANCEL.Checked and cb_IDABORT.Checked and cb_IDIGNORE.Checked) then begin
     IDButton := 'IDRETRY';
     IDButton2 := 'IDIGNORE';
     IDButton3 := 'IDABORT';
     ButtonCount := 3;
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, IDButton, IDButton2, IDButton3);
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, IDButton, IDButton2, IDButton3);
     end;
  end

  { no selected buttons }
  else begin
     case ModeMsg of
        0: Text := TextMsgBox(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, '', '', '');
        1: Text := TextTaskDialog(ButtonCount, CaptionOrInstructionAndText, Typ, Buttons, ButtonLabels, ShieldButton, '', '', '');
     end;
  end;

  var SL := TStringList.Create;
  try
    SL.Text := Text;
    SL.Insert(0, '// Display a message box');
    for var I := 0 to SL.Count-1 do
      SL[I] := TextTab + SL[I];
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

end.
