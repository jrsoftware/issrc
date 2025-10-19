// -- IsDonateAndMail.iss --
// Include file which adds donate and subscribe buttons to Setup
//
[Files]
Source: "{#__DIR__}\isdonate.bmp"; Flags: dontcopy noencryption
Source: "{#__DIR__}\ismail.bmp"; Flags: dontcopy noencryption

[CustomMessages]
; No need to localize: The IS website is in English only
IsDonateAndMailDonateCaption=Donate
IsDonateAndMailDonateHint=Support Inno Setup - Thank you!
IsDonateAndMailMailCaption=Subscribe
IsDonateAndMailMailHint=Be notified by e-mail of new Inno Setup releases

[Code]
procedure DonateBitmapButtonOnClick(Sender: TObject);
var
  ErrorCode: Integer;
begin
  ShellExecAsOriginalUser('open', 'https://jrsoftware.org/isdonate.php', '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;

procedure MailBitmapButtonOnClick(Sender: TObject);
var
  ErrorCode: Integer;
begin
  ShellExecAsOriginalUser('open', 'https://jrsoftware.org/ismail.php', '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;

<event('InitializeWizard')>
procedure IsDonateAndMailInitializeWizard;
var
  ImageFileName: String;
  DonateBitmapButton, MailBitmapButton: TBitmapButton;
  BevelTop: Integer;
begin
  if WizardSilent then
    Exit;

  ImageFileName := ExpandConstant('{tmp}\isdonate.bmp');
  ExtractTemporaryFile(ExtractFileName(ImageFileName));

  DonateBitmapButton := TBitmapButton.Create(WizardForm);
  DonateBitmapButton.AutoSize := True;
  DonateBitmapButton.Bitmap.LoadFromFile(ImageFileName);
  DonateBitmapButton.Caption := CustomMessage('IsDonateAndMailDonateCaption');
  DonateBitmapButton.Hint := CustomMessage('IsDonateAndMailDonateHint');
  DonateBitmapButton.ShowHint := True;
  DonateBitmapButton.Anchors := [akLeft, akBottom];
  BevelTop := WizardForm.Bevel.Top;
  DonateBitmapButton.Top := BevelTop + (WizardForm.ClientHeight - BevelTop - DonateBitmapButton.Bitmap.Height) div 2;
  DonateBitmapButton.Left := DonateBitmapButton.Top - BevelTop;
  DonateBitmapButton.Cursor := crHand;
  DonateBitmapButton.OnClick := @DonateBitmapButtonOnClick;
  DonateBitmapButton.Parent := WizardForm;

  ImageFileName := ExpandConstant('{tmp}\ismail.bmp');
  ExtractTemporaryFile(ExtractFileName(ImageFileName));

  MailBitmapButton := TBitmapButton.Create(WizardForm);
  MailBitmapButton.AutoSize := True;
  MailBitmapButton.Bitmap.LoadFromFile(ImageFileName);
  MailBitmapButton.Caption := CustomMessage('IsDonateAndMailMailCaption');
  MailBitmapButton.Hint := CustomMessage('IsDonateAndMailMailHint');
  MailBitmapButton.ShowHint := True;
  MailBitmapButton.Anchors := [akLeft, akBottom];
  MailBitmapButton.Top := DonateBitmapButton.Top
  MailBitmapButton.Left := DonateBitmapButton.Left + DonateBitmapButton.Width + ScaleX(4);
  MailBitmapButton.Cursor := crHand;
  MailBitmapButton.OnClick := @MailBitmapButtonOnClick;
  MailBitmapButton.Parent := WizardForm;
end;