; -- IsDonateAndMail.iss --
; Include file which adds donate and subscribe buttons to Setup

[Files]
Source: "isdonate.bmp"; Flags: dontcopy
Source: "ismail.bmp"; Flags: dontcopy

[Code]
procedure DonateImageOnClick(Sender: TObject);
var
  ErrorCode: Integer;
begin
  ShellExecAsOriginalUser('open', 'https://jrsoftware.org/isdonate.php', '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;

procedure MailImageOnClick(Sender: TObject);
var
  ErrorCode: Integer;
begin
  ShellExecAsOriginalUser('open', 'https://jrsoftware.org/ismail.php', '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;

<event('InitializeWizard')>
procedure DonateAndMailImagesInitializeWizard;
var
  ImageFileName: String;
  DonateImage, MailImage: TBitmapImage;
  BevelTop: Integer;
begin
  ImageFileName := ExpandConstant('{tmp}\isdonate.bmp');
  ExtractTemporaryFile(ExtractFileName(ImageFileName));

  DonateImage := TBitmapImage.Create(WizardForm);
  DonateImage.AutoSize := True;
  DonateImage.Bitmap.LoadFromFile(ImageFileName);
  DonateImage.Anchors := [akLeft, akBottom];
  BevelTop := WizardForm.Bevel.Top;
  DonateImage.Top := BevelTop + (WizardForm.ClientHeight - BevelTop - DonateImage.Bitmap.Height) div 2;
  DonateImage.Left := DonateImage.Top - BevelTop;
  DonateImage.Cursor := crHand;
  DonateImage.OnClick := @DonateImageOnClick;
  DonateImage.Parent := WizardForm;

  ImageFileName := ExpandConstant('{tmp}\ismail.bmp');
  ExtractTemporaryFile(ExtractFileName(ImageFileName));

  MailImage := TBitmapImage.Create(WizardForm);
  MailImage.AutoSize := True;
  MailImage.Bitmap.LoadFromFile(ImageFileName);
  MailImage.Anchors := [akLeft, akBottom];
  BevelTop := WizardForm.Bevel.Top;
  MailImage.Top := DonateImage.Top
  MailImage.Left := DonateImage.Left + DonateImage.Width + ScaleX(8);
  MailImage.Cursor := crHand;
  MailImage.OnClick := @MailImageOnClick;
  MailImage.Parent := WizardForm;
end;