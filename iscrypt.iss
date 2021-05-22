// -- IsCrypt.iss --
// Include file with support functions to download encryption support
// Must be included before adding [Files] entries
//
#if FileExists('iscrypt-custom.ico')
  #define iscryptico      'iscrypt-custom.ico'
  #define iscrypticosizes '[32, 48, 64]'
#else
  #define iscryptico      'iscrypt.ico'
  #define iscrypticosizes '[32]'
#endif
//
[Files]
Source: "{#iscryptico}"; DestName: "iscrypt.ico"; Flags: dontcopy
Source: "{tmp}\ISCrypt.dll"; DestDir: "{app}"; Flags: ignoreversion external skipifsourcedoesntexist touch

[Code]
const
  ISCryptHash = '2f6294f9aa09f59a574b5dcd33be54e16b39377984f3d5658cda44950fa0f8fc';

var
  ISCryptPage: TWizardPage;
  ISCryptCheckBox: TCheckBox;

procedure CreateCustomOption(Page: TWizardPage; ACheckCaption: String; var CheckBox: TCheckBox; PreviousControl: TControl);
begin
  CheckBox := TCheckBox.Create(Page);
  with CheckBox do begin
    Top := PreviousControl.Top + PreviousControl.Height + ScaleY(12);
    Width := Page.SurfaceWidth;
    Height := ScaleY(Height);
    Anchors := [akLeft, akTop, akRight];
    Caption := ACheckCaption;
    Parent := Page.Surface;
  end;
end;

function CreateCustomOptionPage(AAfterId: Integer; ACaption, ASubCaption, AIconFileName, ALabel1Caption, ALabel2Caption,
  ACheckCaption: String; var CheckBox: TCheckBox): TWizardPage;
var
  Page: TWizardPage;
  BitmapImage: TBitmapImage;
  Label1, Label2: TNewStaticText;
begin
  Page := CreateCustomPage(AAfterID, ACaption, ASubCaption);
  
  AIconFileName := ExpandConstant('{tmp}\' + AIconFileName);
  if not FileExists(AIconFileName) then
    ExtractTemporaryFile(ExtractFileName(AIconFileName));

  BitmapImage := TBitmapImage.Create(Page);
  with BitmapImage do begin
    Width := ScaleX(32);
    Height := ScaleY(32);
    Parent := Page.Surface;
  end;
  
  InitializeBitmapImageFromIcon(BitmapImage, AIconFileName, Page.SurfaceColor, {#iscrypticosizes});

  Label1 := TNewStaticText.Create(Page);
  with Label1 do begin
    AutoSize := False;
    Left := WizardForm.SelectDirLabel.Left;
    Width := Page.SurfaceWidth - Left;
    Anchors := [akLeft, akTop, akRight];
    WordWrap := True;
    Caption := ALabel1Caption;
    Parent := Page.Surface;
  end;
  WizardForm.AdjustLabelHeight(Label1);

  Label2 := TNewStaticText.Create(Page);
  with Label2 do begin
    Top := Label1.Top + Label1.Height + ScaleY(12);
    Width := Page.SurfaceWidth;
    Anchors := [akLeft, akTop, akRight];
    WordWrap := True;
    Caption := ALabel2Caption;
    Parent := Page.Surface;
  end;
  WizardForm.AdjustLabelHeight(Label2);
  
  CreateCustomOption(Page, ACheckCaption, CheckBox, Label2);

  Result := Page;
end;

<event('InitializeWizard')>
procedure IsCryptInitializeWizard;
var
  ExistingFileName, Caption, SubCaption1, IconFileName, Label1Caption, Label2Caption, CheckCaption: String;
begin
  if WizardForm.PrevAppDir <> '' then begin
    ExistingFileName := AddBackslash(WizardForm.PrevAppDir) + 'ISCrypt.dll';
    try
      if GetSHA256OfFile(ExistingFileName) = ISCryptHash then
        Exit;
    except
    end;
  end;

  Caption := 'Encryption Support';
  SubCaption1 := 'Would you like to download encryption support?';
  IconFileName := 'iscrypt.ico';
  Label1Caption :=
    'Inno Setup supports encryption. However, because of encryption import/export laws in some countries, encryption support is not included in the main' +
    ' Inno Setup installer. Instead, it can be downloaded from a server located in the Netherlands now.';
  Label2Caption := 'Select whether you would like to download and install encryption support, then click Next.';
  CheckCaption := '&Download and install encryption support';

  ISCryptPage := CreateCustomOptionPage(wpSelectProgramGroup, Caption, SubCaption1, IconFileName, Label1Caption, Label2Caption, CheckCaption, ISCryptCheckBox);
  
  ISCryptCheckBox.Checked := ExpandConstant('{param:downloadiscrypt|0}') = '1';
end;

<event('NextButtonClick')>
function IsCryptNextButtonClick(CurPageID: Integer): Boolean;
var
  DownloadPage: TDownloadWizardPage;
begin
  Result := True;
  if (CurPageID = wpReady) and (ISCryptCheckBox <> nil) and ISCryptCheckBox.Checked then begin
    DownloadPage := CreateDownloadPage(SetupMessage(msgWizardPreparing), SetupMessage(msgPreparingDesc), nil);
    DownloadPage.Clear;
    DownloadPage.Add('https://jrsoftware.org/download.php/iscrypt.dll', 'ISCrypt.dll', ISCryptHash);
    DownloadPage.Show;
    try
      try
        DownloadPage.Download;
      except
        if DownloadPage.AbortedByUser then
          Log('Aborted by user.')
        else
          SuppressibleMsgBox(AddPeriod(GetExceptionMessage), mbCriticalError, MB_OK, IDOK);
      end;
    finally
      DownloadPage.Hide;
    end;
  end;
end;