// -- IsDonateAndMail.iss --
// Include file which adds donate and subscribe buttons to Setup
//
[Files]
Source: "isdonate.bmp"; Flags: dontcopy noencryption
Source: "ismail.bmp"; Flags: dontcopy noencryption

[CustomMessages]
; No need to localize: The IS website is in English only
IsDonateAndMailDonateHint=Support Inno Setup - Thank you!
IsDonateAndMailMailHint=Be notified by e-mail of new Inno Setup releases

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
procedure IsDonateAndMailInitializeWizard;
var
  ImageFileName: String;
  DonateImage, MailImage: TBitmapImage;
  BevelTop: Integer;
begin
  if WizardSilent then
    Exit;

  ImageFileName := ExpandConstant('{tmp}\isdonate.bmp');
  ExtractTemporaryFile(ExtractFileName(ImageFileName));

  DonateImage := TBitmapImage.Create(WizardForm);
  DonateImage.AutoSize := True;
  DonateImage.Bitmap.LoadFromFile(ImageFileName);
  DonateImage.Hint := CustomMessage('IsDonateAndMailDonateHint');
  DonateImage.ShowHint := True;
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
  MailImage.Hint := CustomMessage('IsDonateAndMailMailHint');
  MailImage.ShowHint := True;
  MailImage.Anchors := [akLeft, akBottom];
  MailImage.Top := DonateImage.Top
  MailImage.Left := DonateImage.Left + DonateImage.Width + ScaleX(8);
  MailImage.Cursor := crHand;
  MailImage.OnClick := @MailImageOnClick;
  MailImage.Parent := WizardForm;
end;