; -- CodeClasses.iss --
;
; This script shows how to use the WizardForm object and the various VCL classes.

[Setup]
AppName=My Program
AppVersion=1.5
WizardStyle=modern dynamic
CreateAppDir=no
Uninstallable=no
DisableProgramGroupPage=yes
DefaultGroupName=My Program
OutputDir=userdocs:Inno Setup Examples Output
PrivilegesRequired=lowest

; Uncomment the following three lines to test the layout for RTL and scaling
;[LangOptions]
;RightToLeft=yes
;DialogFontSize=12

[Files]
Source: compiler:WizClassicSmallImage.bmp; Flags: dontcopy

[Code]
procedure ButtonOnClick(Sender: TObject);
begin
  MsgBox('You clicked the button!', mbInformation, mb_Ok);
end;

procedure FormButtonOnClick(Sender: TObject);
var
  Form: TSetupForm;
  Edit: TNewEdit;
  OKButton, CancelButton: TNewButton;
  W: Integer;
begin
  Form := CreateCustomForm();
  try
    Form.ClientWidth := ScaleX(256);
    Form.ClientHeight := ScaleY(128);
    Form.Caption := 'TSetupForm';

    Edit := TNewEdit.Create(Form);
    Edit.Top := ScaleY(10);
    Edit.Left := ScaleX(10);
    Edit.Width := Form.ClientWidth - ScaleX(2 * 10);
    Edit.Height := ScaleY(23);
    Edit.Anchors := [akLeft, akTop, akRight];
    Edit.Text := 'TNewEdit';
    Edit.Parent := Form;

    OKButton := TNewButton.Create(Form);
    OKButton.Parent := Form;
    OKButton.Caption := 'OK';
    OKButton.Left := Form.ClientWidth - ScaleX(75 + 6 + 75 + 10);
    OKButton.Top := Form.ClientHeight - ScaleY(23 + 10);
    OKButton.Height := ScaleY(23);
    OKButton.Anchors := [akRight, akBottom]
    OKButton.ModalResult := mrOk;
    OKButton.Default := True;

    CancelButton := TNewButton.Create(Form);
    CancelButton.Parent := Form;
    CancelButton.Caption := 'Cancel';
    CancelButton.Left := Form.ClientWidth - ScaleX(75 + 10);
    CancelButton.Top := Form.ClientHeight - ScaleY(23 + 10);
    CancelButton.Height := ScaleY(23);
    CancelButton.Anchors := [akRight, akBottom]
    CancelButton.ModalResult := mrCancel;
    CancelButton.Cancel := True;

    W := Form.CalculateButtonWidth([OKButton.Caption, CancelButton.Caption]);
    OKButton.Width := W;
    CancelButton.Width := W;

    Form.ActiveControl := Edit;
    { Keep the form from sizing vertically since we don't have any controls which can size vertically }
    Form.KeepSizeY := True;
    { Center on WizardForm. Without this call it will still automatically center, but on the screen }
    Form.FlipSizeAndCenterIfNeeded(True, WizardForm, False);

    if Form.ShowModal() = mrOk then
      MsgBox('You clicked OK.', mbInformation, MB_OK);
  finally
    Form.Free();
  end;
end;

procedure TaskDialogButtonOnClick(Sender: TObject);
begin
  { TaskDialogMsgBox isn't a class but showing it anyway since it fits with the theme }

  case TaskDialogMsgBox('Choose A or B',
                        'You can choose A or B.',   
                        mbInformation,
                        MB_YESNOCANCEL, ['I choose &A'#13#10'A will be chosen.', 'I choose &B'#13#10'B will be chosen.'],
                        IDYES) of
    IDYES: MsgBox('You chose A.', mbInformation, MB_OK);
    IDNO: MsgBox('You chose B.', mbInformation, MB_OK);
  end;
end;

procedure LinkLabelOnLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
var
  ErrorCode: Integer;
begin
  if (LinkType = sltID) and (Link = 'jrsoftware') then
    ShellExecAsOriginalUser('open', 'https://jrsoftware.org', '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode)
  else if LinkType = sltURL then  
    ShellExecAsOriginalUser('open', Link, '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;

procedure CreateTheWizardPages;
var
  Page: TWizardPage;
  Button, FormButton, TaskDialogButton, CommandLinkButton: TNewButton;
  Panel: TPanel;
  CheckBox: TNewCheckBox;
  Edit: TNewEdit;
  PasswordEdit: TPasswordEdit;
  Memo: TNewMemo;
  ComboBox: TNewComboBox;
  ListBox: TNewListBox;
  StaticText: array [0..2] of TNewStaticText;
  ProgressBarLabel: TNewStaticText;
  LinkLabel: TNewLinkLabel;
  ProgressBar: array [0..2] of TNewProgressBar;
  CheckListBox: array [0..1] of TNewCheckListBox;
  FolderTreeView: TFolderTreeView;
  BitmapImage: array [0..5] of TBitmapImage;
  Siids: array of Integer;
  SiidBitmapImage: TBitmapImage;
  I: Integer;
  BitmapButton: array [0..2] of TBitmapButton;
  BitmapFileName: String;
  RichEditViewer: TRichEditViewer;
begin
  { TButton and others }

  Page := CreateCustomPage(wpWelcome, 'Custom wizard page controls', 'TButton and others');

  Button := TNewButton.Create(Page);
  Button.Caption := 'TNewButton';
  Button.Width := WizardForm.CalculateButtonWidth([Button.Caption]);
  Button.Height := ScaleY(23);
  Button.OnClick := @ButtonOnClick;
  Button.Parent := Page.Surface;

  Panel := TPanel.Create(Page);
  Panel.Width := Page.SurfaceWidth div 2 - ScaleX(8);
  Panel.Left :=  Page.SurfaceWidth - Panel.Width;
  Panel.Height := Button.Height * 2;
  Panel.Anchors := [akLeft, akTop, akRight];
  Panel.Caption := 'TPanel';
  Panel.Color := clWindow;
  Panel.BevelKind := bkFlat;
  Panel.BevelOuter := bvNone;
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
  PasswordEdit.Anchors := [akLeft, akTop, akRight];
  PasswordEdit.Text := 'TPasswordEdit';
  PasswordEdit.Parent := Page.Surface;

  Memo := TNewMemo.Create(Page);
  Memo.Top := Edit.Top + Edit.Height + ScaleY(8);
  Memo.Width := Page.SurfaceWidth;
  Memo.Height := ScaleY(70);
  Memo.Anchors := [akLeft, akTop, akRight, akBottom];
  Memo.ScrollBars := ssVertical;
  Memo.Text := 'TNewMemo';
  Memo.Parent := Page.Surface;

  FormButton := TNewButton.Create(Page);
  FormButton.Caption := 'TSetupForm';
  FormButton.Top := Memo.Top + Memo.Height + ScaleY(8);
  FormButton.Width := WizardForm.CalculateButtonWidth([FormButton.Caption]);
  FormButton.Height := ScaleY(23);
  FormButton.Anchors := [akLeft, akBottom];
  FormButton.OnClick := @FormButtonOnClick;
  FormButton.Parent := Page.Surface;

  TaskDialogButton := TNewButton.Create(Page);
  TaskDialogButton.Caption := 'TaskDialogMsgBox';
  TaskDialogButton.Top := FormButton.Top + FormButton.Height + ScaleY(8);
  TaskDialogButton.Left := FormButton.Left;
  TaskDialogButton.Width := WizardForm.CalculateButtonWidth([TaskDialogButton.Caption]);
  TaskDialogButton.Height := ScaleY(23);
  TaskDialogButton.Anchors := [akLeft, akBottom];
  TaskDialogButton.OnClick := @TaskDialogButtonOnClick;
  TaskDialogButton.Parent := Page.Surface;
  
  CommandLinkButton := TNewButton.Create(Page);
  CommandLinkButton.Style := bsCommandLink;
  CommandLinkButton.Caption := 'TNewButton bsCommandLink';
  CommandLinkButton.CommandLinkHint := 'A note';
  //CommandLinkButton.ElevationRequired := True;
  CommandLinkButton.Font.Size := MulDiv(CommandLinkButton.Font.Size, 12, 9);
  CommandLinkButton.Top := FormButton.Top;
  CommandLinkButton.Left := TaskDialogButton.Left + TaskDialogButton.Width + ScaleX(8);
  CommandLinkButton.Width := Page.Surface.Width - CommandLinkButton.Left;
  CommandLinkButton.Anchors := [akLeft, akRight, akBottom];
  CommandLinkButton.OnClick := @ButtonOnClick;
  CommandLinkButton.Parent := Page.Surface;
  CommandLinkButton.AdjustHeightIfCommandLink;

  { TComboBox and others }

  Page := CreateCustomPage(Page.ID, 'Custom wizard page controls', 'TComboBox and others');

  ComboBox := TNewComboBox.Create(Page);
  ComboBox.Width := Page.SurfaceWidth;
  ComboBox.Anchors := [akLeft, akTop, akRight];
  ComboBox.Parent := Page.Surface;
  ComboBox.Style := csDropDownList;
  ComboBox.Items.Add('TComboBox');
  ComboBox.ItemIndex := 0;

  ListBox := TNewListBox.Create(Page);
  ListBox.Top := ComboBox.Top + ComboBox.Height + ScaleY(8);
  ListBox.Width := Page.SurfaceWidth;
  ListBox.Height := ScaleY(97);
  ListBox.Anchors := [akLeft, akTop, akRight, akBottom];
  ListBox.Parent := Page.Surface;
  ListBox.Items.Add('TListBox');
  ListBox.ItemIndex := 0;

  StaticText[0] := TNewStaticText.Create(Page);
  StaticText[0].Top := ListBox.Top + ListBox.Height + ScaleY(8);
  StaticText[0].Anchors := [akLeft, akRight, akBottom];
  StaticText[0].Caption := 'TNewStaticText';
  StaticText[0].Parent := Page.Surface;

  StaticText[1] := TNewStaticText.Create(Page);
  StaticText[1].AutoSize := False;
  StaticText[1].Left := StaticText[0].Width + ScaleX(32);
  StaticText[1].Top := StaticText[0].Top;
  StaticText[1].Anchors := [akLeft, akRight, akBottom];
  StaticText[1].WordWrap := True;
  StaticText[1].Caption := 'TNewStaticText with more text and an adjusted label height so it''s multi-line.';
  StaticText[1].Width := 2 * StaticText[0].Width;
  StaticText[1].Parent := Page.Surface;
  StaticText[1].AdjustHeight;

  StaticText[2] := TNewStaticText.Create(Page);
  StaticText[2].Top := StaticText[0].Top + StaticText[0].Height + ScaleY(8);
  StaticText[2].Anchors := [akLeft, akRight, akBottom];
  StaticText[2].Caption := 'TNewStaticText';
  StaticText[2].Parent := Page.Surface;
  StaticText[2].StyleElements := StaticText[2].StyleElements - [seFont];
  if IsDarkInstallMode then
    StaticText[2].Font.Color := StrToColor('#D95E6C')
  else
    StaticText[2].Font.Color := StrToColor('#D24152');

  LinkLabel := TNewLinkLabel.Create(Page);
  LinkLabel.AutoSize := False;
  LinkLabel.Left := StaticText[1].Left;
  LinkLabel.Top := StaticText[1].Top + StaticText[1].Height + ScaleY(8);
  LinkLabel.Anchors := [akLeft, akRight, akBottom];
  LinkLabel.Caption := 'TNew<a id="jrsoftware">Link</a>Label with more text and an adjusted label height so it''s multi-line with a second <a id="jrsoftware">link</a> on the second line.';
  LinkLabel.Width := StaticText[1].Width;
  LinkLabel.UseVisualStyle := HighContrastActive;
  LinkLabel.OnLinkClick := @LinkLabelOnLinkClick;
  LinkLabel.Parent := Page.Surface;
  LinkLabel.AdjustHeight;

  { TNewProgressBar }

  Page := CreateCustomPage(Page.ID, 'Custom wizard page controls', 'TNewProgressBar');

  ProgressBarLabel := TNewStaticText.Create(Page);
  ProgressBarLabel.Anchors := [akLeft, akTop];
  ProgressBarLabel.Caption := 'TNewProgressBar';
  ProgressBarLabel.Parent := Page.Surface;

  ProgressBar[0] := TNewProgressBar.Create(Page);
  ProgressBar[0].Left := ProgressBarLabel.Width + ScaleX(8);
  ProgressBar[0].Top := ProgressBarLabel.Top;
  ProgressBar[0].Width := Page.SurfaceWidth - ProgressBar[0].Left;
  ProgressBar[0].Height := ProgressBarLabel.Height + ScaleY(8);
  ProgressBar[0].Anchors := [akLeft, akRight, akTop];
  ProgressBar[0].Parent := Page.Surface;
  ProgressBar[0].Position := 25;

  ProgressBar[1] := TNewProgressBar.Create(Page);
  ProgressBar[1].Left := ProgressBarLabel.Width + ScaleX(8);
  ProgressBar[1].Top := ProgressBar[0].Top + ProgressBar[0].Height + ScaleY(4);
  ProgressBar[1].Width := Page.SurfaceWidth - ProgressBar[0].Left;
  ProgressBar[1].Height := ProgressBarLabel.Height + ScaleY(8);
  ProgressBar[1].Anchors := [akLeft, akRight, akTop];
  ProgressBar[1].Parent := Page.Surface;
  ProgressBar[1].Position := 50;
  ProgressBar[1].State := npbsError;

  ProgressBar[2] := TNewProgressBar.Create(Page);
  ProgressBar[2].Left := ProgressBarLabel.Width + ScaleX(8);
  ProgressBar[2].Top := ProgressBar[1].Top + ProgressBar[1].Height + ScaleY(4);
  ProgressBar[2].Width := Page.SurfaceWidth - ProgressBar[0].Left;
  ProgressBar[2].Height := ProgressBarLabel.Height + ScaleY(8);
  ProgressBar[2].Anchors := [akLeft, akRight, akTop];
  ProgressBar[2].Parent := Page.Surface;
  ProgressBar[2].Style := npbstMarquee;
  
  { TNewCheckListBox }

  Page := CreateCustomPage(Page.ID, 'Custom wizard page controls', 'TNewCheckListBox');

  CheckListBox[0] := TNewCheckListBox.Create(Page);
  CheckListBox[0].Width := Page.SurfaceWidth;
  CheckListBox[0].Height := ScaleY(97);
  CheckListBox[0].Anchors := [akLeft, akTop, akRight, akBottom];
  CheckListBox[0].Flat := True;
  CheckListBox[0].Parent := Page.Surface;
  CheckListBox[0].AddCheckBox('TNewCheckListBox', '', 0, True, True, False, True, nil);
  CheckListBox[0].AddRadioButton('TNewCheckListBox', '', 1, True, True, nil);
  CheckListBox[0].AddRadioButton('TNewCheckListBox', '', 1, False, True, nil);
  CheckListBox[0].AddCheckBox('TNewCheckListBox', '', 0, True, True, False, True, nil);
  CheckListBox[0].AddCheckBox('TNewCheckListBox', '', 1, True, True, False, True, nil);
  CheckListBox[0].AddCheckBox('TNewCheckListBox', '123', 2, True, True, False, True, nil);
  CheckListBox[0].AddCheckBox('TNewCheckListBox', '456', 2, False, True, False, True, nil);
  CheckListBox[0].AddCheckBox('TNewCheckListBox', '', 1, False, True, False, True, nil);
  CheckListBox[0].ItemFontStyle[5] := [fsBold];
  CheckListBox[0].SubItemFontStyle[5] := [fsBold];
  CheckListBox[0].ItemFontStyle[6] := [fsBold, fsItalic];
  CheckListBox[0].SubItemFontStyle[6] := [fsBold, fsUnderline];

  CheckListBox[1] := TNewCheckListBox.Create(Page);
  CheckListBox[1].Top := CheckListBox[0].Top + CheckListBox[0].Height + ScaleY(8);
  CheckListBox[1].Width := Page.SurfaceWidth;
  CheckListBox[1].Height := ScaleY(97);
  CheckListBox[1].Anchors := [akLeft, akRight, akBottom];
  CheckListBox[1].BorderStyle := bsNone;
  CheckListBox[1].ParentColor := True;
  CheckListBox[1].MinItemHeight := WizardForm.TasksList.MinItemHeight;
  CheckListBox[1].ShowLines := False;
  CheckListBox[1].WantTabs := True;
  CheckListBox[1].Parent := Page.Surface;
  CheckListBox[1].AddGroup('TNewCheckListBox', '', 0, nil);
  CheckListBox[1].AddRadioButton('TNewCheckListBox', '', 0, True, True, nil);
  CheckListBox[1].AddRadioButton('TNewCheckListBox', '', 0, False, True, nil);

  { TFolderTreeView }

  Page := CreateCustomPage(Page.ID, 'Custom wizard page controls', 'TFolderTreeView');

  FolderTreeView := TFolderTreeView.Create(Page);
  FolderTreeView.Width := Page.SurfaceWidth;
  FolderTreeView.Height := Page.SurfaceHeight;
  FolderTreeView.Anchors := [akLeft, akTop, akRight, akBottom];
  FolderTreeView.Parent := Page.Surface;
  FolderTreeView.Directory := ExpandConstant('{src}');

  { TBitmapImage }

  Page := CreateCustomPage(Page.ID, 'Custom wizard page controls', 'TBitmapImage');

  BitmapFileName := ExpandConstant('{tmp}\WizClassicSmallImage.bmp');
  ExtractTemporaryFile(ExtractFileName(BitmapFileName));
  
  BitmapImage[0] := TBitmapImage.Create(Page);
  BitmapImage[0].AutoSize := True;
  { Use BitmapImage[0].PngImage.LoadFromFile to load .png files }
  BitmapImage[0].Bitmap.LoadFromFile(BitmapFileName);
  BitmapImage[0].Parent := Page.Surface;
  
  BitmapImage[1] := TBitmapImage.Create(Page);
  BitmapImage[1].BackColor := clNone;
  BitmapImage[1].Bitmap := BitmapImage[0].Bitmap;
  BitmapImage[1].Center := True;
  BitmapImage[1].Left := BitmapImage[0].Width + 10;
  BitmapImage[1].Width := 2*BitmapImage[0].Width;
  BitmapImage[1].Height := 2*BitmapImage[0].Height;
  BitmapImage[1].Parent := Page.Surface;

  BitmapImage[2] := TBitmapImage.Create(Page);
  BitmapImage[2].Bitmap := BitmapImage[0].Bitmap;
  BitmapImage[2].Stretch := True;
  BitmapImage[2].Left := 3*BitmapImage[0].Width + 20;
  BitmapImage[2].Width := 4*BitmapImage[0].Width;
  BitmapImage[2].Height := 4*BitmapImage[0].Height;
  BitmapImage[2].Anchors := [akLeft, akTop, akRight, akBottom];
  BitmapImage[2].Parent := Page.Surface;
  
  Page := CreateCustomPage(Page.ID, 'Custom wizard page controls', 'TBitmapImage (stock icons)');

  BitmapImage[3] := TBitmapImage.Create(Page);
  BitmapImage[3].Width := ScaleX(16);
  BitmapImage[3].Height := BitmapImage[3].Width;
  InitializeBitmapImageFromStockIcon(BitmapImage[3], SIID_ERROR, clNone, [16, 24, 32]);
  BitmapImage[3].Parent := Page.Surface;

  BitmapImage[4] := TBitmapImage.Create(Page);
  BitmapImage[4].Width := ScaleX(32);
  BitmapImage[4].Height := BitmapImage[4].Width;
  InitializeBitmapImageFromStockIcon(BitmapImage[4], SIID_ERROR, clNone, [32, 48, 64]);
  BitmapImage[4].Left := BitmapImage[3].Left + BitmapImage[3].Width + 10; 
  BitmapImage[4].Parent := Page.Surface;

  BitmapImage[5] := TBitmapImage.Create(Page);
  BitmapImage[5].Width := 256;
  BitmapImage[5].Height := BitmapImage[5].Width;
  InitializeBitmapImageFromStockIcon(BitmapImage[5], SIID_ERROR, clNone, []);
  BitmapImage[5].Top := BitmapImage[4].Top + BitmapImage[4].Height + 10; 
  BitmapImage[5].Parent := Page.Surface;
 
  { See https://learn.microsoft.com/en-us/windows/win32/api/shellapi/ne-shellapi-shstockiconid for all available icons }
  Siids := [
    SIID_INFO, SIID_HELP, SIID_WARNING, SIID_LOCK, SIID_KEY,
    SIID_DOCNOASSOC, SIID_DOCASSOC, SIID_AUDIOFILES, SIID_IMAGEFILES, SIID_VIDEOFILES,
    SIID_APPLICATION, SIID_SOFTWARE, SIID_FOLDER, SIID_ZIPFILE, SIID_SHIELD,
    SIID_SERVER, SIID_MYNETWORK, SIID_DEVICECELLPHONE, SIID_DRIVEREMOVE, SIID_PRINTER,
    SIID_FIND, SIID_MEDIACDAUDIO, SIID_MEDIABLANKCD, SIID_RENAME, SIID_DELETE];

  for I := 0 to High(Siids) do begin
    SiidBitmapImage := TBitmapImage.Create(Page);
    SiidBitmapImage.Width := ScaleX(32);
    SiidBitmapImage.Height := SiidBitmapImage.Width;
    InitializeBitmapImageFromStockIcon(SiidBitmapImage, Siids[I], clNone, [32, 48, 64]);
    SiidBitmapImage.Left := BitmapImage[5].Left + BitmapImage[5].Width + 20 + (I mod 5) * ScaleX(42); 
    SiidBitmapImage.Top := (I div 5) * ScaleY(42); 
    SiidBitmapImage.Parent := Page.Surface;
  end;

  { TBitmapButton - Always has a 2 pixel margin around the image, used to
    display a focus rectangle. Other changes compared to TBitmapImage are:
    • Has a Caption property which should always be set
    • Center defaults to True
    • BackColor defaults to clNone }

  Page := CreateCustomPage(Page.ID, 'Custom wizard page controls', 'TBitmapButton (Press Alt to see focus rectangle)');
  
  BitmapButton[0] := TBitmapButton.Create(Page);
  BitmapButton[0].AutoSize := True;
  BitmapButton[0].Bitmap := BitmapImage[0].Bitmap;
  BitmapButton[0].Caption := 'Show Message'; { For accessibility }
  BitmapButton[0].Hint := 'TBitmapButton is an accessible version of TBitmapImage';
  BitmapButton[0].ShowHint := True;
  BitmapButton[0].Width := 2*BitmapButton[0].Width;
  BitmapButton[0].Cursor := crHand;
  BitmapButton[0].OnClick := @ButtonOnClick;
  BitmapButton[0].Parent := Page.Surface;

  BitmapButton[1] := TBitmapButton.Create(Page);
  BitmapButton[1].BackColor := $400000;
  BitmapButton[1].Bitmap := BitmapImage[0].Bitmap;
  BitmapButton[1].Caption := BitmapButton[0].Caption;
  BitmapButton[1].Hint := BitmapButton[0].Hint;
  BitmapButton[1].ShowHint := True;
  BitmapButton[1].Left := BitmapButton[0].Width + 10;
  BitmapButton[1].Width := 2*BitmapButton[0].Width;
  BitmapButton[1].Height := 2*BitmapButton[0].Height;
  BitmapButton[1].Cursor := crHand;
  BitmapButton[1].OnClick := @ButtonOnClick;
  BitmapButton[1].Parent := Page.Surface;
  
  BitmapButton[2] := TBitmapButton.Create(Page);
  BitmapButton[2].Width := ScaleX(24);
  BitmapButton[2].Height := ScaleY(24);
  InitializeBitmapButtonFromStockIcon(BitmapButton[2], SIID_HELP, clNone, [24, 36, 38]);
  BitmapButton[2].Caption := BitmapButton[0].Caption;
  BitmapButton[2].Hint := BitmapButton[0].Hint;
  BitmapButton[2].ShowHint := True;
  BitmapButton[2].Left := BitmapButton[1].Left + BitmapButton[1].Width + 10;
  BitmapButton[2].Cursor := crHand;
  BitmapButton[2].OnClick := @ButtonOnClick;  
  BitmapButton[2].Parent := Page.Surface;

  { TRichViewer }

  Page := CreateCustomPage(Page.ID, 'Custom wizard page controls', 'TRichViewer');

  RichEditViewer := TRichEditViewer.Create(Page);
  RichEditViewer.Width := Page.SurfaceWidth;
  RichEditViewer.Height := Page.SurfaceHeight;
  RichEditViewer.Anchors := [akLeft, akTop, akRight, akBottom];
  RichEditViewer.BevelKind := bkFlat;
  RichEditViewer.BorderStyle := bsNone;
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

procedure CreateAboutButtonAndURLLabel(ParentForm: TSetupForm; CancelButton: TNewButton);
var
  AboutButton: TNewButton;
  URLLabel: TNewLinkLabel;
begin
  AboutButton := TNewButton.Create(ParentForm);
  AboutButton.Left := ParentForm.ClientWidth - CancelButton.Left - CancelButton.Width;
  AboutButton.Top := CancelButton.Top;
  AboutButton.Width := CancelButton.Width;
  AboutButton.Height := CancelButton.Height;
  AboutButton.Anchors := [akLeft, akBottom];
  AboutButton.Caption := '&About...';
  AboutButton.OnClick := @AboutButtonOnClick;
  AboutButton.Parent := ParentForm;

  URLLabel := TNewLinkLabel.Create(ParentForm);
  URLLabel.Left := AboutButton.Left + AboutButton.Width + ScaleX(20);
  URLLabel.Top := AboutButton.Top + (AboutButton.Height - URLLabel.Height) div 2;
  URLLabel.Anchors := [akLeft, akBottom];
  URLLabel.Caption := '<a href="https://jrsoftware.org">jrsoftware.org</a>';
  URLLabel.OnLinkClick := @LinkLabelOnLinkClick;
  URLLabel.UseVisualStyle := True;
  URLLabel.Parent := ParentForm;
end;

procedure InitializeWizard();
begin
  { Custom wizard pages }

  CreateTheWizardPages;
  
  { Custom controls }

  CreateAboutButtonAndURLLabel(WizardForm, WizardForm.CancelButton);

  { Custom beveled label }

  WizardForm.BeveledLabel.Caption := ' Bevel ';
end;

procedure InitializeUninstallProgressForm();
begin
  { Custom controls }

  CreateAboutButtonAndURLLabel(UninstallProgressForm, UninstallProgressForm.CancelButton);
end;

