; -- CodeClasses.iss --
;
; This script shows how to use the WizardForm object and the various VCL classes.

[Setup]
AppName=My Program
AppVersion=1.5
CreateAppDir=no
DisableProgramGroupPage=yes
DefaultGroupName=My Program
UninstallDisplayIcon={app}\MyProg.exe
WindowVisible=yes
OutputDir=userdocs:Inno Setup Examples Output

[Files]
Source: compiler:WizModernSmallImage.bmp; Flags: dontcopy

[Code]
procedure ButtonOnClick(Sender: TObject);
begin
  MsgBox('You clicked the button!', mbInformation, mb_Ok);
end;

procedure BitmapImageOnClick(Sender: TObject);
begin
  MsgBox('You clicked the image!', mbInformation, mb_Ok);
end;

procedure FormButtonOnClick(Sender: TObject);
var
  Form: TSetupForm;
  Edit: TNewEdit;
  OKButton, CancelButton: TNewButton;
begin
  Form := CreateCustomForm();
  try
    Form.ClientWidth := ScaleX(256);
    Form.ClientHeight := ScaleY(128);
    Form.Caption := 'TSetupForm';
    Form.CenterInsideControl(WizardForm, False);

    Edit := TNewEdit.Create(Form);
    Edit.Top := ScaleY(10);
    Edit.Left := ScaleX(10);
    Edit.Width := Form.ClientWidth - ScaleX(2 * 10);
    Edit.Height := ScaleY(23);
    Edit.Text := 'TNewEdit';
    Edit.Parent := Form;

    OKButton := TNewButton.Create(Form);
    OKButton.Parent := Form;
    OKButton.Width := ScaleX(75);
    OKButton.Height := ScaleY(23);
    OKButton.Left := Form.ClientWidth - ScaleX(75 + 6 + 75 + 10);
    OKButton.Top := Form.ClientHeight - ScaleY(23 + 10);
    OKButton.Caption := 'OK';
    OKButton.ModalResult := mrOk;
    OKButton.Default := True;

    CancelButton := TNewButton.Create(Form);
    CancelButton.Parent := Form;
    CancelButton.Width := ScaleX(75);
    CancelButton.Height := ScaleY(23);
    CancelButton.Left := Form.ClientWidth - ScaleX(75 + 10);
    CancelButton.Top := Form.ClientHeight - ScaleY(23 + 10);
    CancelButton.Caption := 'Cancel';
    CancelButton.ModalResult := mrCancel;
    CancelButton.Cancel := True;

    Form.ActiveControl := Edit;

    if Form.ShowModal() = mrOk then
      MsgBox('You clicked OK.', mbInformation, MB_OK);
  finally
    Form.Free();
  end;
end;

procedure CreateTheWizardPages;
var
  Page: TWizardPage;
  Button, FormButton: TNewButton;
  Panel: TPanel;
  CheckBox: TNewCheckBox;
  Edit: TNewEdit;
  PasswordEdit: TPasswordEdit;
  Memo: TNewMemo;
  ComboBox: TNewComboBox;
  ListBox: TNewListBox;
  StaticText, ProgressBarLabel: TNewStaticText;
  ProgressBar, ProgressBar2, ProgressBar3: TNewProgressBar;
  CheckListBox, CheckListBox2: TNewCheckListBox;
  FolderTreeView: TFolderTreeView;
  BitmapImage, BitmapImage2, BitmapImage3: TBitmapImage;
  BitmapFileName: String;
  RichEditViewer: TRichEditViewer;
begin
  { TButton and others }

  Page := CreateCustomPage(wpWelcome, 'Custom wizard page controls', 'TButton and others');

  Button := TNewButton.Create(Page);
  Button.Width := ScaleX(75);
  Button.Height := ScaleY(23);
  Button.Caption := 'TNewButton';
  Button.OnClick := @ButtonOnClick;
  Button.Parent := Page.Surface;

  Panel := TPanel.Create(Page);
  Panel.Width := Page.SurfaceWidth div 2 - ScaleX(8);
  Panel.Left :=  Page.SurfaceWidth - Panel.Width;
  Panel.Height := Button.Height * 2;
  Panel.Caption := 'TPanel';
  Panel.Color := clWindow;
  Panel.ParentBackground := False;
  Panel.Parent := Page.Surface;

  CheckBox := TNewCheckBox.Create(Page);
  CheckBox.Top := Button.Top + Button.Height + ScaleY(8);
  CheckBox.Width := Page.SurfaceWidth div 2;
  CheckBox.Height := ScaleY(17);
  CheckBox.Caption := 'TNewCheckBox';
  CheckBox.Checked := True;
  CheckBox.Parent := Page.Surface;

  Edit := TNewEdit.Create(Page);
  Edit.Top := CheckBox.Top + CheckBox.Height + ScaleY(8);
  Edit.Width := Page.SurfaceWidth div 2 - ScaleX(8);
  Edit.Text := 'TNewEdit';
  Edit.Parent := Page.Surface;

  PasswordEdit := TPasswordEdit.Create(Page);
  PasswordEdit.Left := Page.SurfaceWidth - Edit.Width;
  PasswordEdit.Top := CheckBox.Top + CheckBox.Height + ScaleY(8);
  PasswordEdit.Width := Edit.Width;
  PasswordEdit.Text := 'TPasswordEdit';
  PasswordEdit.Parent := Page.Surface;

  Memo := TNewMemo.Create(Page);
  Memo.Top := Edit.Top + Edit.Height + ScaleY(8);
  Memo.Width := Page.SurfaceWidth;
  Memo.Height := ScaleY(89);
  Memo.ScrollBars := ssVertical;
  Memo.Text := 'TNewMemo';
  Memo.Parent := Page.Surface;

  FormButton := TNewButton.Create(Page);
  FormButton.Top := Memo.Top + Memo.Height + ScaleY(8);
  FormButton.Width := ScaleX(75);
  FormButton.Height := ScaleY(23);
  FormButton.Caption := 'TSetupForm';
  FormButton.OnClick := @FormButtonOnClick;
  FormButton.Parent := Page.Surface;

  { TComboBox and others }

  Page := CreateCustomPage(Page.ID, 'Custom wizard page controls', 'TComboBox and others');

  ComboBox := TNewComboBox.Create(Page);
  ComboBox.Width := Page.SurfaceWidth;
  ComboBox.Parent := Page.Surface;
  ComboBox.Style := csDropDownList;
  ComboBox.Items.Add('TComboBox');
  ComboBox.ItemIndex := 0;

  ListBox := TNewListBox.Create(Page);
  ListBox.Top := ComboBox.Top + ComboBox.Height + ScaleY(8);
  ListBox.Width := Page.SurfaceWidth;
  ListBox.Height := ScaleY(97);
  ListBox.Parent := Page.Surface;
  ListBox.Items.Add('TListBox');
  ListBox.ItemIndex := 0;

  StaticText := TNewStaticText.Create(Page);
  StaticText.Top := ListBox.Top + ListBox.Height + ScaleY(8);
  StaticText.Caption := 'TNewStaticText';
  StaticText.AutoSize := True;
  StaticText.Parent := Page.Surface;

  ProgressBarLabel := TNewStaticText.Create(Page);
  ProgressBarLabel.Top := StaticText.Top + StaticText.Height + ScaleY(8);
  ProgressBarLabel.Caption := 'TNewProgressBar';
  ProgressBarLabel.AutoSize := True;
  ProgressBarLabel.Parent := Page.Surface;

  ProgressBar := TNewProgressBar.Create(Page);
  ProgressBar.Left := ProgressBarLabel.Width + ScaleX(8);
  ProgressBar.Top := ProgressBarLabel.Top;
  ProgressBar.Width := Page.SurfaceWidth - ProgressBar.Left;
  ProgressBar.Height := ProgressBarLabel.Height + ScaleY(8);
  ProgressBar.Parent := Page.Surface;
  ProgressBar.Position := 25;

  ProgressBar2 := TNewProgressBar.Create(Page);
  ProgressBar2.Left := ProgressBarLabel.Width + ScaleX(8);
  ProgressBar2.Top := ProgressBar.Top + ProgressBar.Height + ScaleY(4);
  ProgressBar2.Width := Page.SurfaceWidth - ProgressBar.Left;
  ProgressBar2.Height := ProgressBarLabel.Height + ScaleY(8);
  ProgressBar2.Parent := Page.Surface;
  ProgressBar2.Position := 50;
  { Note: TNewProgressBar.State property only has an effect on Windows Vista and newer }
  ProgressBar2.State := npbsError;

  ProgressBar3 := TNewProgressBar.Create(Page);
  ProgressBar3.Left := ProgressBarLabel.Width + ScaleX(8);
  ProgressBar3.Top := ProgressBar2.Top + ProgressBar2.Height + ScaleY(4);
  ProgressBar3.Width := Page.SurfaceWidth - ProgressBar.Left;
  ProgressBar3.Height := ProgressBarLabel.Height + ScaleY(8);
  ProgressBar3.Parent := Page.Surface;
  { Note: TNewProgressBar.Style property only has an effect on Windows XP and newer }
  ProgressBar3.Style := npbstMarquee;
  
  { TNewCheckListBox }

  Page := CreateCustomPage(Page.ID, 'Custom wizard page controls', 'TNewCheckListBox');

  CheckListBox := TNewCheckListBox.Create(Page);
  CheckListBox.Width := Page.SurfaceWidth;
  CheckListBox.Height := ScaleY(97);
  CheckListBox.Flat := True;
  CheckListBox.Parent := Page.Surface;
  CheckListBox.AddCheckBox('TNewCheckListBox', '', 0, True, True, False, True, nil);
  CheckListBox.AddRadioButton('TNewCheckListBox', '', 1, True, True, nil);
  CheckListBox.AddRadioButton('TNewCheckListBox', '', 1, False, True, nil);
  CheckListBox.AddCheckBox('TNewCheckListBox', '', 0, True, True, False, True, nil);

  CheckListBox2 := TNewCheckListBox.Create(Page);
  CheckListBox2.Top := CheckListBox.Top + CheckListBox.Height + ScaleY(8);
  CheckListBox2.Width := Page.SurfaceWidth;
  CheckListBox2.Height := ScaleY(97);
  CheckListBox2.BorderStyle := bsNone;
  CheckListBox2.ParentColor := True;
  CheckListBox2.MinItemHeight := WizardForm.TasksList.MinItemHeight;
  CheckListBox2.ShowLines := False;
  CheckListBox2.WantTabs := True;
  CheckListBox2.Parent := Page.Surface;
  CheckListBox2.AddGroup('TNewCheckListBox', '', 0, nil);
  CheckListBox2.AddRadioButton('TNewCheckListBox', '', 0, True, True, nil);
  CheckListBox2.AddRadioButton('TNewCheckListBox', '', 0, False, True, nil);

  { TFolderTreeView }

  Page := CreateCustomPage(Page.ID, 'Custom wizard page controls', 'TFolderTreeView');

  FolderTreeView := TFolderTreeView.Create(Page);
  FolderTreeView.Width := Page.SurfaceWidth;
  FolderTreeView.Height := Page.SurfaceHeight;
  FolderTreeView.Parent := Page.Surface;
  FolderTreeView.Directory := ExpandConstant('{src}');

  { TBitmapImage }

  Page := CreateCustomPage(Page.ID, 'Custom wizard page controls', 'TBitmapImage');

  BitmapFileName := ExpandConstant('{tmp}\WizModernSmallImage.bmp');
  ExtractTemporaryFile(ExtractFileName(BitmapFileName));

  BitmapImage := TBitmapImage.Create(Page);
  BitmapImage.AutoSize := True;
  BitmapImage.Bitmap.LoadFromFile(BitmapFileName);
  BitmapImage.Cursor := crHand;
  BitmapImage.OnClick := @BitmapImageOnClick;
  BitmapImage.Parent := Page.Surface;

  BitmapImage2 := TBitmapImage.Create(Page);
  BitmapImage2.BackColor := $400000;
  BitmapImage2.Bitmap := BitmapImage.Bitmap;
  BitmapImage2.Center := True;
  BitmapImage2.Left := BitmapImage.Width + 10;
  BitmapImage2.Height := 2*BitmapImage.Height;
  BitmapImage2.Width := 2*BitmapImage.Width;
  BitmapImage2.Cursor := crHand;
  BitmapImage2.OnClick := @BitmapImageOnClick;
  BitmapImage2.Parent := Page.Surface;

  BitmapImage3 := TBitmapImage.Create(Page);
  BitmapImage3.Bitmap := BitmapImage.Bitmap;
  BitmapImage3.Stretch := True;
  BitmapImage3.Left := 3*BitmapImage.Width + 20;
  BitmapImage3.Height := 4*BitmapImage.Height;
  BitmapImage3.Width := 4*BitmapImage.Width;
  BitmapImage3.Cursor := crHand;
  BitmapImage3.OnClick := @BitmapImageOnClick;
  BitmapImage3.Parent := Page.Surface;

  { TRichViewer }

  Page := CreateCustomPage(Page.ID, 'Custom wizard page controls', 'TRichViewer');

  RichEditViewer := TRichEditViewer.Create(Page);
  RichEditViewer.Width := Page.SurfaceWidth;
  RichEditViewer.Height := Page.SurfaceHeight;
  RichEditViewer.Parent := Page.Surface;
  RichEditViewer.ScrollBars := ssVertical;
  RichEditViewer.UseRichEdit := True;
  RichEditViewer.RTFText := '{\rtf1\ansi\ansicpg1252\deff0\deflang1043{\fonttbl{\f0\fswiss\fcharset0 Arial;}}{\colortbl ;\red255\green0\blue0;\red0\green128\blue0;\red0\green0\blue128;}\viewkind4\uc1\pard\f0\fs20 T\cf1 Rich\cf2 Edit\cf3 Viewer\cf0\par}';
  RichEditViewer.ReadOnly := True;
end;

procedure AboutButtonOnClick(Sender: TObject);
begin
  MsgBox('This demo shows some features of the various form objects and control classes.', mbInformation, mb_Ok);
end;

procedure URLLabelOnClick(Sender: TObject);
var
  ErrorCode: Integer;
begin
  ShellExecAsOriginalUser('open', 'http://www.innosetup.com/', '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;

procedure CreateAboutButtonAndURLLabel(ParentForm: TSetupForm; CancelButton: TNewButton);
var
  AboutButton: TNewButton;
  URLLabel: TNewStaticText;
begin
  AboutButton := TNewButton.Create(ParentForm);
  AboutButton.Left := ParentForm.ClientWidth - CancelButton.Left - CancelButton.Width;
  AboutButton.Top := CancelButton.Top;
  AboutButton.Width := CancelButton.Width;
  AboutButton.Height := CancelButton.Height;
  AboutButton.Caption := '&About...';
  AboutButton.OnClick := @AboutButtonOnClick;
  AboutButton.Parent := ParentForm;

  URLLabel := TNewStaticText.Create(ParentForm);
  URLLabel.Caption := 'www.innosetup.com';
  URLLabel.Cursor := crHand;
  URLLabel.OnClick := @URLLabelOnClick;
  URLLabel.Parent := ParentForm;
  { Alter Font *after* setting Parent so the correct defaults are inherited first }
  URLLabel.Font.Style := URLLabel.Font.Style + [fsUnderline];
  if GetWindowsVersion >= $040A0000 then   { Windows 98 or later? }
    URLLabel.Font.Color := clHotLight
  else
    URLLabel.Font.Color := clBlue;
  URLLabel.Top := AboutButton.Top + AboutButton.Height - URLLabel.Height - 2;
  URLLabel.Left := AboutButton.Left + AboutButton.Width + ScaleX(20);
end;

procedure InitializeWizard();
var
  BackgroundBitmapImage: TBitmapImage;
  BackgroundBitmapText: TNewStaticText;
begin
  { Custom wizard pages }

  CreateTheWizardPages;
  
  { Custom controls }

  CreateAboutButtonAndURLLabel(WizardForm, WizardForm.CancelButton);

  BackgroundBitmapImage := TBitmapImage.Create(MainForm);
  BackgroundBitmapImage.Left := 50;
  BackgroundBitmapImage.Top := 90;
  BackgroundBitmapImage.AutoSize := True;
  BackgroundBitmapImage.Bitmap := WizardForm.WizardBitmapImage.Bitmap;
  BackgroundBitmapImage.Parent := MainForm;
  
  BackgroundBitmapText := TNewStaticText.Create(MainForm);
  BackgroundBitmapText.Left := BackgroundBitmapImage.Left;
  BackgroundBitmapText.Top := BackgroundBitmapImage.Top + BackgroundBitmapImage.Height + ScaleY(8);
  BackgroundBitmapText.Caption := 'TBitmapImage';
  BackgroundBitmapText.Font.Color := clWhite;
  BackgroundBitmapText.Parent := MainForm;

  { Custom beveled label }

  WizardForm.BeveledLabel.Caption := ' BeveledLabel ';
end;

procedure InitializeUninstallProgressForm();
begin
  { Custom controls }

  CreateAboutButtonAndURLLabel(UninstallProgressForm, UninstallProgressForm.CancelButton);
end;

