unit IDE.Wizard.WizardForm;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE Script Wizard form
}

interface

uses
  Windows, Forms, Classes, Graphics, StdCtrls, ExtCtrls, Controls, Dialogs, pngimage,
  Vcl.BaseImageCollection, Vcl.ImageCollection,
  NewStaticText, DropListBox, NewCheckListBox, NewNotebook, BitmapButton, BitmapImage, 
  IDE.Wizard.WizardFormFilesHelper, IDE.Wizard.WizardFormRegistryHelper,
  IDE.IDEForm;

type
  TWizardPage = (wpWelcome, wpAppInfo, wpAppDir, wpAppFiles, wpAppAssoc, wpAppIcons,
                 wpAppDocs, wpPrivilegesRequired, wpAppRegistry, wpLanguages, wpCompiler,
                 wpWizardStyle, wpISPP, wpFinished);

  TWizardFormResult = (wrNone, wrEmpty, wrComplete);

  TWizardForm = class(TIDEForm)
    CancelButton: TButton;
    NextButton: TButton;
    BackButton: TButton;
    OuterNotebook: TNewNotebook;
    InnerNotebook: TNewNotebook;
    WelcomePage: TNewNotebookPage;
    MainPage: TNewNotebookPage;
    AppInfoPage: TNewNotebookPage;
    AppDirPage: TNewNotebookPage;
    AppFilesPage: TNewNotebookPage;
    AppIconsPage: TNewNotebookPage;
    AppDocsPage: TNewNotebookPage;
    PrivilegesRequiredPage: TNewNotebookPage;
    AppRegistryPage: TNewNotebookPage;
    LanguagesPage: TNewNotebookPage;
    CompilerPage: TNewNotebookPage;
    ISPPPage: TNewNotebookPage;
    FinishedPage: TNewNotebookPage;
    Bevel: TBevel;
    WelcomeImage: TBitmapImage;
    WelcomeLabel1: TNewStaticText;
    PnlMain: TPanel;
    Bevel1: TBevel;
    PageNameLabel: TNewStaticText;
    PageDescriptionLabel: TNewStaticText;
    InnerImage: TBitmapImage;
    FinishedLabel: TNewStaticText;
    FinishedImage: TBitmapImage;
    WelcomeLabel2: TNewStaticText;
    EmptyCheck: TCheckBox;
    AppNameLabel: TNewStaticText;
    AppNameEdit: TEdit;
    AppVersionLabel: TNewStaticText;
    AppVersionEdit: TEdit;
    AppDirNameLabel: TNewStaticText;
    AppRootDirComboBox: TComboBox;
    AppRootDirEdit: TEdit;
    AppDirNameEdit: TEdit;
    NotDisableDirPageCheck: TCheckBox;
    AppRootDirLabel: TNewStaticText;
    AppPublisherLabel: TNewStaticText;
    AppPublisherEdit: TEdit;
    OtherLabel: TNewStaticText;
    NotCreateAppDirCheck: TCheckBox;
    AppFilesLabel: TNewStaticText;
    AppFilesListBox: TDropListBox;
    AppFilesAddButton: TButton;
    AppFilesEditButton: TButton;
    AppFilesRemoveButton: TButton;
    AppURLLabel: TNewStaticText;
    AppURLEdit: TEdit;
    AppExeLabel: TNewStaticText;
    AppExeEdit: TEdit;
    AppExeRunCheck: TCheckBox;
    AppExeButton: TButton;
    AppGroupNameLabel: TNewStaticText;
    AppGroupNameEdit: TEdit;
    NotDisableProgramGroupPageCheck: TCheckBox;
    AllowNoIconsCheck: TCheckBox;
    AppExeIconsLabel: TNewStaticText;
    DesktopIconCheck: TCheckBox;
    CreateUninstallIconCheck: TCheckBox;
    CreateURLIconCheck: TCheckBox;
    AppLicenseFileLabel: TNewStaticText;
    AppLicenseFileEdit: TEdit;
    AppLicenseFileButton: TButton;
    AppInfoBeforeFileLabel: TNewStaticText;
    AppInfoBeforeFileEdit: TEdit;
    AppInfoBeforeFileButton: TButton;
    AppInfoAfterFileLabel: TNewStaticText;
    AppInfoAfterFileEdit: TEdit;
    AppInfoAfterFileButton: TButton;
    RequiredLabel1: TNewStaticText;
    RequiredLabel2: TNewStaticText;
    AppFilesAddDirButton: TButton;
    ISPPCheck: TCheckBox;
    ISPPLabel: TLabel;
    OutputDirLabel: TNewStaticText;
    OutputDirEdit: TEdit;
    OutputBaseFileNameLabel: TNewStaticText;
    OutputBaseFileNameEdit: TEdit;
    SetupIconFileLabel: TNewStaticText;
    SetupIconFileEdit: TEdit;
    PasswordLabel: TNewStaticText;
    PasswordEdit: TEdit;
    SetupIconFileButton: TButton;
    EncryptionCheck: TCheckBox;
    OutputDirButton: TButton;
    LanguagesLabel: TNewStaticText;
    LanguagesList: TNewCheckListBox;
    AllLanguagesButton: TButton;
    NoLanguagesButton: TButton;
    NoAppExeCheck: TCheckBox;
    UseAutoProgramsCheck: TCheckBox;
    PrivilegesRequiredLabel: TNewStaticText;
    PrivilegesRequiredAdminRadioButton: TRadioButton;
    PrivilegesRequiredLowestRadioButton: TRadioButton;
    PrivilegesRequiredOverridesAllowedCommandLineCheckbox: TCheckBox;
    PrivilegesRequiredOverridesAllowedDialogCheckbox: TCheckBox;
    AppAssocPage: TNewNotebookPage;
    AppAssocNameEdit: TEdit;
    AppAssocNameLabel: TNewStaticText;
    CreateAssocCheck: TCheckBox;
    AppAssocExtLabel: TNewStaticText;
    AppAssocExtEdit: TEdit;
    AppRegistryFileLabel: TNewStaticText;
    AppRegistryFileEdit: TEdit;
    AppRegistryFileButton: TButton;
    AppRegistrySettingsLabel: TNewStaticText;
    AppRegistryUninsDeleteKeyCheck: TCheckBox;
    AppRegistryUninsDeleteKeyIfEmptyCheck: TCheckBox;
    AppRegistryUninsDeleteValueCheck: TCheckBox;
    AppRegistryMinVerCheck: TCheckBox;
    AppRegistryMinVerEdit: TEdit;
    AppRegistryMinVerDocBitBtn: TBitmapButton;
    WelcomeImageDark: TBitmapImage;
    InnerImageDark: TBitmapImage;
    AppFilesAddDownloadButton: TButton;
    WizardStylePage: TNewNotebookPage;
    WizardStyleLabel: TNewStaticText;
    WizardStyleMainComboBox: TComboBox;
    WizardStyleDarkComboBox: TComboBox;
    WizardStyleSubStyleComboBox: TComboBox;
    WizardStyleImageCollection: TImageCollection;
    WizardStyleImage: TBitmapButton;
    WizardStyleImage2: TBitmapImage;
    WizardStyleImageTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure FileButtonClick(Sender: TObject);
    procedure AppRootDirComboBoxChange(Sender: TObject);
    procedure NotCreateAppDirCheckClick(Sender: TObject);
    procedure AppExeButtonClick(Sender: TObject);
    procedure NotDisableProgramGroupPageCheckClick(Sender: TObject);
    procedure PasswordEditChange(Sender: TObject);
    procedure OutputDirButtonClick(Sender: TObject);
    procedure AllLanguagesButtonClick(Sender: TObject);
    procedure NoLanguagesButtonClick(Sender: TObject);
    procedure NoAppExeCheckClick(Sender: TObject);
    procedure UseAutoProgramsCheckClick(Sender: TObject);
    procedure PrivilegesRequiredOverridesAllowedDialogCheckboxClick(Sender: TObject);
    procedure CreateAssocCheckClick(Sender: TObject);
    procedure WizardStyleComboBoxChange(Sender: TObject);
    procedure WizardStyleImageTimerTimer(Sender: TObject);
    procedure WizardStyleImageClick(Sender: TObject);
  private
    FCurPage: TWizardPage;
    FWizardName: String;
    FFilesHelper: TWizardFormFilesHelper;
    FRegistryHelper: TWizardFormRegistryHelper;
    FLanguages: TStringList;
    FResult: TWizardFormResult;
    FResultScript: String;
    function FixLabel(const S: String): String;
    procedure SetWizardName(const WizardName: String);
    procedure CurPageChanged;
    function SkipCurPage: Boolean;
    procedure UpdateAppExeControls;
    procedure UpdateAppAssocControls;
    procedure UpdateAppIconsControls;
    procedure UpdateWizardStyleImages;
    procedure WizardStyleImagePreviewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WizardStyleImagePreviewImageClick(Sender: TObject);
    function GetWizardStyle: String;
    procedure GenerateScript;
  public
    property WizardName: String write SetWizardName;
    property Result: TWizardFormResult read FResult;
    property ResultScript: String read FResultScript;
  end;

implementation

{$R *.DFM}

uses
  SysUtils, ShlObj, ActiveX, UITypes,
  PathFunc, BrowseFunc,
  Shared.CommonFunc.Vcl, Shared.CommonFunc, Shared.FileClass, Shared.LicenseFunc,
  IDE.HelperFunc, IDE.Messages, IDE.LocalizeFunc, IDE.Wizard.WizardFileForm;

type
  TConstant = record
    Constant, Description: String;
  end;

const
  NotebookPages: array[TWizardPage, 0..1] of Integer =
    ((0, -1), (1, 0), (1, 1), (1, 2),
     (1, 3), (1, 4), (1, 5), (1, 6),
     (1, 7), (1, 8), (1, 9), (1, 10), (1, 11), (2, -1));

  PageCaptions: array[TWizardPage] of String =
    (SWizardWelcome, SWizardAppInfo, SWizardAppDir, SWizardAppFiles, SWizardAppAssoc,
     SWizardAppIcons, SWizardAppDocs, SWizardPrivilegesRequired, SWizardAppRegistry,
     SWizardLanguages, SWizardCompiler, SWizardWizardStyle, SWizardISPP, SWizardFinished);

  PageDescriptions: array[TWizardPage] of String =
    ('', SWizardAppInfo2, SWizardAppDir2, SWizardAppFiles2, SWizardAppAssoc2,
         SWizardAppIcons2, SWizardAppDocs2, SWizardPrivilegesRequired2, SWizardAppRegistry2,
         SWizardLanguages2, SWizardCompiler2, SWizardWizardStyle2, SWizardISPP2, '');

  RequiredLabelVisibles: array[TWizardPage] of Boolean =
    (False, True, True, True, True, True, False, True, False, True, False, False, False, False);

  AppRootDirs: array[0..0] of TConstant =
  (
    ( Constant: '{autopf}'; Description: SWizardDirProgramFiles)
  );

  LanguagesDefaultIsl = 'Default.isl';
  LanguagesDefaultIslDescription = 'English'; { do not localize }

  EnabledColors: array[Boolean] of TColor = (clBtnFace, clWindow);

function TWizardForm.FixLabel(const S: String): String;
begin
  Result := S;
  {don't localize these}
  StringChange(Result, '[name]', FWizardName);
end;

procedure TWizardForm.SetWizardName(const WizardName: String);
begin
  FWizardName := WizardName;
end;

{ --- }

procedure TWizardForm.FormCreate(Sender: TObject);

  procedure AddLanguages(const Extension: String);
  var
    SearchRec: TSearchRec;
  begin
    if FindFirst(PathExtractPath(NewParamStr(0)) + 'Languages\*.' + Extension, faAnyFile, SearchRec) = 0 then begin
      repeat
        FLanguages.Add(SearchRec.Name);
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
  end;

  procedure MakeBold(const Ctl: TNewStaticText);
  begin
    Ctl.Font.Style := [fsBold];
  end;

  function SpaceLanguageName(const LanguageName: String): String;
  var
    I: Integer;
  begin
    Result := '';
    for I := 1 to Length(LanguageName) do begin
      if (I <> 1) and CharInSet(LanguageName[I], ['A'..'Z']) then
        Result := Result + ' ';
      Result := Result + LanguageName[I];
    end;
  end;

  procedure SetWidestNextCaption;
  begin
    { CurPageChanged will override this again }
    const FinishCaption = LFmtMessage(SWizardFinishButton);
    const NextCaption = LFmtMessage(SWizardNextButton);
    if CalculateButtonWidth([FinishCaption]) > CalculateButtonWidth([NextCaption]) then
      NextButton.Caption := FinishCaption
    else
      NextButton.Caption := NextCaption;
  end;

var
  I: Integer;
begin
  { Finish localization - also done per page below }
  WelcomeLabel1.Caption := LFmtMessage(WelcomeLabel1.Caption, ['[name]']);
  FinishedLabel.Caption := LFmtMessage(FinishedLabel.Caption, ['[name]']);
  SetWidestNextCaption;
  var W := SizeBottomButtons(NextButton, CancelButton, [BackButton]);
  BackButton.Width := W;
  BackButton.Left := NextButton.Left - W;
  { Size all browse buttons and resize all page-wide edits and comboboxes to keep a
    consistent right edge }
  const OldW = AppRegistryFileButton.Width;
  W := SizeSideButtons([AppExeButton, AppLicenseFileButton, AppInfoBeforeFileButton,
    AppInfoAfterFileButton, AppRegistryFileButton, SetupIconFileButton, OutputDirButton],
    [AppNameEdit, AppVersionEdit, AppPublisherEdit, AppURLEdit, AppRootDirComboBox,
    AppRootDirEdit, AppDirNameEdit, AppExeEdit, AppAssocNameEdit, AppAssocExtEdit,
    AppGroupNameEdit, AppLicenseFileEdit, AppInfoBeforeFileEdit, AppInfoAfterFileEdit,
    AppRegistryFileEdit, AppRegistryMinVerEdit, OutputDirEdit, OutputBaseFileNameEdit,
    SetupIconFileEdit, PasswordEdit]);
  const Diff = W - OldW;
  AppRegistryMinVerDocBitBtn.Left := AppRegistryMinVerDocBitBtn.Left - Diff;
  { These are not set in the .dfm because that would duplicate a message,
    one with and one without the accel char }
  AppInfoBeforeFileButton.Caption := RemoveAccelChar(AppLicenseFileButton.Caption);
  AppInfoAfterFileButton.Caption := RemoveAccelChar(AppLicenseFileButton.Caption);
  SetupIconFileButton.Caption := RemoveAccelChar(OutputDirButton.Caption);

  FResult := wrNone;

  FWizardName := LFmtMessage(SWizardDefaultName);
  FFilesHelper := TWizardFormFilesHelper.Create(Self,
    NotCreateAppDirCheck, AppFilesListBox, AppFilesAddButton, AppFilesAddDirButton,
    AppFilesAddDownloadButton, AppFilesEditButton, AppFilesRemoveButton);
  FRegistryHelper := TWizardFormRegistryHelper.Create(Self, AppRegistryFileEdit,
    AppRegistryFileButton, AppRegistryUninsDeleteKeyCheck,
    AppRegistryUninsDeleteKeyIfEmptyCheck, AppRegistryUninsDeleteValueCheck,
    AppRegistryMinVerCheck, AppRegistryMinVerEdit, AppRegistryMinVerDocBitBtn);

  FLanguages := TStringList.Create;
  FLanguages.Sorted := True;
  AddLanguages('isl');
  FLanguages.Sorted := False;
  FLanguages.Insert(0, LanguagesDefaultIsl);

  if not FormThemeActive then
    OuterNotebook.Color := InitFormThemeGetBkColor(True);

  if FontExists('Segoe UI') then begin
    WelcomeLabel1.Font.Name := 'Segoe UI';
    WelcomeLabel1.Font.Size := 14;
  end;

  MakeBold(PageNameLabel);
  MakeBold(RequiredLabel1);
  MakeBold(AppNameLabel);
  MakeBold(AppVersionLabel);
  MakeBold(AppRootDirLabel);
  MakeBold(AppDirNameLabel);
  MakeBold(AppExeLabel);
  MakeBold(AppAssocNameLabel);
  MakeBold(AppAssocExtLabel);
  MakeBold(AppGroupNameLabel);
  MakeBold(PrivilegesRequiredLabel);
  MakeBold(LanguagesLabel);

  if InitFormThemeIsDark then begin
    WelcomeImage.Bitmap := WelcomeImageDark.Bitmap;
    InnerImage.Bitmap := InnerImageDark.Bitmap;
  end;
  FinishedImage.Bitmap := WelcomeImage.Bitmap;

  RequiredLabel2.Left := RequiredLabel1.Left + RequiredLabel1.Width;

  { See Setup.WizardForm }
  if IsCustomStyleActive then
    BackButton.Left := BackButton.Left - 2;

  { AppInfo }
  AppNameEdit.Text := LFmtMessage(SWizardDefaultAppName);
  AppVersionEdit.Text := '1.5';
  AppPublisherEdit.Text := LFmtMessage(SWizardDefaultAppPublisher);
  AppURLEdit.Text := 'https://www.example.com/';

  { AppDir }
  for I := Low(AppRootDirs) to High(AppRootDirs) do
    AppRootDirComboBox.Items.Add(LFmtMessage(AppRootDirs[I].Description));
  AppRootDirComboBox.Items.Add(LFmtMessage(SWizardDirCustom));
  AppRootDirComboBox.ItemIndex := 0;
  AppRootDirEdit.Enabled := False;
  AppRootDirEdit.Color := clBtnFace;
  NotDisableDirPageCheck.Checked := True;

  { AppFiles }
  SizeSideButtons([AppFilesAddButton, AppFilesAddDirButton,
    AppFilesAddDownloadButton, AppFilesEditButton, AppFilesRemoveButton], AppFilesListBox);
  AppExeEdit.Text := PathExtractPath(NewParamStr(0)) + 'Examples\MyProg-x64.exe';
  AppExeRunCheck.Checked := True;

  { AppAssoc }
  CreateAssocCheck.Checked := True;
  AppAssocExtEdit.Text := '.myp';

  { AppIcons }
  UseAutoProgramsCheck.Checked := True;
  NotDisableProgramGroupPageCheck.Checked := True;
  DesktopIconCheck.Checked := True;

  { PrivilegesRequired }
  PrivilegesRequiredAdminRadioButton.Checked := True;
  
  { AppRegistry }
  AppRegistryFileLabel.Caption := LFmtMessage(AppRegistryFileLabel.Caption, [SLitRegExt]);

  { Languages }
  SizeSideButtons([AllLanguagesButton, NoLanguagesButton], LanguagesList);
  for I := 0 to FLanguages.Count-1 do begin
    if FLanguages[I] <> LanguagesDefaultIsl then
      LanguagesList.AddCheckBox(SpaceLanguageName(PathChangeExt(FLanguages[I], '')), '', 0, False, True, False, True, TObject(I))
    else
      LanguagesList.AddCheckBox(LanguagesDefaultIslDescription, '', 0, True, True, False, True, TObject(I));
  end;

  { Compiler }
  OutputBaseFileNameEdit.Text := 'mysetup';
  EncryptionCheck.Checked := True;
  EncryptionCheck.Enabled := False;

  { WizardStyle }
  WizardStyleMainComboBox.Items.AddStrings(['classic', 'modern']);
  WizardStyleMainComboBox.ItemIndex := 1;
  WizardStyleDarkComboBox.Items.AddStrings(['light', 'dark', 'dynamic']);
  WizardStyleDarkComboBox.ItemIndex := 2;
  WizardStyleSubStyleComboBox.Items.AddStrings(['default', 'polar', 'slate', 'stellar', 'windows11', 'zircon']);
  WizardStyleSubStyleComboBox.ItemIndex := 0;

  { ISPP }
  ISPPLabel.Caption := FixLabel(LFmtMessage(SWizardISPPLabel, ['[name]', '#define']));
  ISPPCheck.Caption := LFmtMessage(SWizardISPPCheck, ['#define']);
  ISPPCheck.Checked := ISPPInstalled;

  FCurPage := Low(TWizardPage);
  CurPageChanged;
end;

procedure TWizardForm.FormShow(Sender: TObject);
begin
  Caption := FWizardName;
  WelcomeLabel1.Caption := FixLabel(WelcomeLabel1.Caption);
  FinishedLabel.Caption := FixLabel(FinishedLabel.Caption);
end;

procedure TWizardForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrCancel then
    CanClose := MsgBox(FixLabel(LFmtMessage(SWizardCancelMessage, ['[name]'])), FWizardName, mbConfirmation, MB_YESNO) = idYes;
end;

procedure TWizardForm.FormDestroy(Sender: TObject);
begin
  FLanguages.Free;
  FRegistryHelper.Free;
  FFilesHelper.Free;
end;

{ --- }

procedure TWizardForm.CurPageChanged;
{ Call this whenever the current page is changed }
begin
  OuterNotebook.ActivePage := OuterNotebook.Pages[NotebookPages[FCurPage, 0]];
  if NotebookPages[FCurPage, 1] <> -1 then
    InnerNotebook.ActivePage := InnerNotebook.Pages[NotebookPages[FCurPage, 1]];

  { Set button visibility and captions }
  BackButton.Visible := not (FCurPage = wpWelcome);
  if FCurPage = wpFinished then
    NextButton.Caption := LFmtMessage(SWizardFinishButton)
  else
    NextButton.Caption := LFmtMessage(SWizardNextButton);

  RequiredLabel1.Visible := RequiredLabelVisibles[FCurPage];
  RequiredLabel2.Visible := RequiredLabel1.Visible;

  { Set the Caption to match the current page's title }
  PageNameLabel.Caption := LFmtMessage(PageCaptions[FCurPage]);
  PageDescriptionLabel.Caption := LFmtMessage(PageDescriptions[FCurPage], True);

  { Adjust focus }
  case FCurPage of
    wpAppInfo: ActiveControl := AppNameEdit;
    wpAppDir:
      begin
        if AppRootDirComboBox.Enabled then
          ActiveControl := AppRootDirComboBox
        else
          ActiveControl := NotCreateAppDirCheck;
      end;
    wpAppFiles:
      begin
        if AppExeEdit.Enabled then
          ActiveControl := AppExeEdit
        else
          ActiveControl := AppFilesListBox;
      end;
    wpAppAssoc: ActiveControl := CreateAssocCheck;
    wpAppIcons:
      begin
        if UseAutoProgramsCheck.Enabled then
          ActiveControl := UseAutoProgramsCheck
        else
          ActiveControl := AppGroupNameEdit;
      end;
    wpAppDocs: ActiveControl := AppLicenseFileEdit;
    wpPrivilegesRequired:
      begin
        if PrivilegesRequiredAdminRadioButton.Checked then
          ActiveControl := PrivilegesRequiredAdminRadioButton
        else
          ActiveControl := PrivilegesRequiredLowestRadioButton;
      end;
    wpAppRegistry: ActiveControl := AppRegistryFileEdit;
    wpLanguages: ActiveControl := LanguagesList;
    wpCompiler: ActiveControl := OutputDirEdit;
    wpWizardStyle:
      begin
        ActiveControl := WizardStyleMainComboBox;
        UpdateWizardStyleImages;
      end;
    wpISPP: ActiveControl := ISPPCheck;
  end;
end;

function TWizardForm.SkipCurPage: Boolean;
begin
  if ((FCurPage = wpAppAssoc) and not CreateAssocCheck.Enabled) or
     ((FCurPage = wpAppIcons) and NotCreateAppDirCheck.Checked) or
     ((FCurPage = wpLanguages) and not (FLanguages.Count > 1)) or
     ((FCurPage = wpISPP) and not ISPPInstalled) or
     (not (FCurPage in [wpWelcome, wpFinished]) and EmptyCheck.Checked) then
    Result := True
  else
    Result := False;
end;

procedure TWizardForm.NextButtonClick(Sender: TObject);

  function CheckAppInfoPage: Boolean;
  begin
    Result := False;

    if AppNameEdit.Text = '' then begin
      MsgBox(LFmtMessage(SWizardAppNameError), '',  mbError, MB_OK);
      ActiveControl := AppNameEdit;
    end else if AppVersionEdit.Text = '' then begin
      MsgBox(LFmtMessage(SWizardAppVersionError), '',  mbError, MB_OK);
      ActiveControl := AppVersionEdit;
    end else
      Result := True;
  end;

  function CheckAppDirPage: Boolean;
  begin
    Result := False;

    if not NotCreateAppDirCheck.Checked and
       (AppRootDirComboBox.ItemIndex = AppRootDirComboBox.Items.Count-1) and
       (AppRootDirEdit.Text = '') then begin
      MsgBox(LFmtMessage(SWizardAppRootDirError), '',  mbError, MB_OK);
      ActiveControl := AppRootDirEdit;
    end else if not NotCreateAppDirCheck.Checked and (AppDirNameEdit.Text = '') then begin
      MsgBox(LFmtMessage(SWizardAppDirNameError), '', mbError, MB_OK);
      ActiveControl := AppDirNameEdit;
    end else
      Result := True;
  end;

  function CheckAppFilesPage: Boolean;
  begin
    Result := False;

    if AppExeEdit.Enabled and (AppExeEdit.Text = '') then begin
      MsgBox(LFmtMessage(SWizardAppExeError), '', mbError, MB_OK);
      ActiveControl := AppExeEdit;
    end else
      Result := True;
  end;

  function CheckAppIconsPage: Boolean;
  begin
    Result := False;

    if AppGroupNameEdit.Text = '' then begin
      MsgBox(LFmtMessage(SWizardAppGroupNameError), '', mbError, MB_OK);
      ActiveControl := AppGroupNameEdit;
    end else
      Result := True;
  end;

  function CheckLanguagesPage: Boolean;
  var
    I: Integer;
  begin
    Result := False;

    for I := 0 to LanguagesList.Items.Count-1 do begin
      if LanguagesList.Checked[I] then begin
        Result := True;
        Exit;
       end;
    end;

    MsgBox(LFmtMessage(SWizardLanguagesSelError), '', mbError, MB_OK);
    ActiveControl := LanguagesList;
  end;

begin
  case FCurPage of
    wpAppInfo: if not CheckAppInfoPage then Exit;
    wpAppDir: if not CheckAppDirPage then Exit;
    wpAppFiles: if not CheckAppFilesPage then Exit;
    wpAppIcons: if not CheckAppIconsPage then Exit;
    wpLanguages: if not CheckLanguagesPage then Exit;
  end;

  repeat
    if FCurPage = wpAppAssoc then begin
      if (AppAssocExtEdit.Text <> '') and (AppAssocExtEdit.Text[1] <> '.') then
        AppAssocExtEdit.Text := '.' + AppAssocExtEdit.Text;
    end else if FCurPage = wpPrivilegesRequired then begin
      if not PrivilegesRequiredOverridesAllowedCommandLineCheckbox.Checked then begin
        if PrivilegesRequiredAdminRadioButton.Checked then
          FRegistryHelper.PrivilegesRequired := prAdmin
        else
          FRegistryHelper.PrivilegesRequired := prLowest
      end else
        FRegistryHelper.PrivilegesRequired := prDynamic;
    end else if FCurPage = wpFinished then begin
      GenerateScript;
      ModalResult := mrOk;
      Exit;
    end;
    Inc(FCurPage);

    { Even if we're skipping a page, we should still update it }
    case FCurPage of
      wpAppDir: if AppDirNameEdit.Text = '' then AppDirNameEdit.Text := AppNameEdit.Text;
      wpAppAssoc: if AppAssocNameEdit.Text = '' then AppAssocNameEdit.Text := LFmtMessage(SWizardAppAssocDefaultName, [AppNameEdit.Text]);
      wpAppIcons: if AppGroupNameEdit.Text = '' then AppGroupNameEdit.Text := AppNameEdit.Text;
    end;
  until not SkipCurPage;

  CurPageChanged;
end;

procedure TWizardForm.BackButtonClick(Sender: TObject);
begin
  if FCurPage = Low(TWizardPage) then Exit;

  { Go to the previous page }
  Dec(FCurPage);
  while SkipCurPage do
    Dec(FCurPage);
  CurPageChanged;
end;

{---}

procedure TWizardForm.UpdateAppExeControls;
var
  Enabled: Boolean;
begin
  Enabled := not NotCreateAppDirCheck.Checked;
  NoAppExeCheck.Enabled := Enabled;

  Enabled := Enabled and not NoAppExeCheck.Checked;
  AppExeLabel.Enabled := Enabled;
  AppExeEdit.Enabled := Enabled;
  AppExeEdit.Color := EnabledColors[Enabled];
  AppExeButton.Enabled := Enabled;
  AppExeRunCheck.Enabled := Enabled;

  AppExeIconsLabel.Enabled := Enabled;
  DesktopIconCheck.Enabled := Enabled;

  if Enabled then
    AppExeLabel.Font.Style := AppExeLabel.Font.Style + [fsBold]
  else
    AppExeLabel.Font.Style := AppExeLabel.Font.Style - [fsBold];
end;

procedure TWizardForm.UpdateAppAssocControls;
var
  Enabled: Boolean;
begin
  Enabled := NoAppExeCheck.Enabled and not NoAppExeCheck.Checked;
  CreateAssocCheck.Enabled := Enabled;

  Enabled := Enabled and CreateAssocCheck.Checked;
  AppAssocNameLabel.Enabled := Enabled;
  AppAssocNameEdit.Enabled := Enabled;
  AppAssocExtLabel.Enabled := Enabled;
  AppAssocExtEdit.Enabled := Enabled;

  if Enabled then begin
    AppAssocNameLabel.Font.Style := AppAssocNameLabel.Font.Style + [fsBold];
    AppAssocExtLabel.Font.Style := AppAssocExtLabel.Font.Style + [fsBold];
  end else begin
    AppAssocNameLabel.Font.Style := AppAssocNameLabel.Font.Style - [fsBold];
    AppAssocExtLabel.Font.Style := AppAssocExtLabel.Font.Style - [fsBold];
  end;
end;

procedure TWizardForm.UpdateAppIconsControls;
var
  Enabled: Boolean;
begin
  UseAutoProgramsCheck.Enabled := NoAppExeCheck.Enabled and not NoAppExeCheck.Checked;

  Enabled := not (UseAutoProgramsCheck.Enabled and UseAutoProgramsCheck.Checked);

  AppGroupNameLabel.Enabled := Enabled;
  AppGroupNameEdit.Enabled := Enabled;
  AppGroupNameEdit.Color := EnabledColors[Enabled];
  NotDisableProgramGroupPageCheck.Enabled := Enabled;
  AllowNoIconsCheck.Enabled := Enabled and NotDisableProgramGroupPageCheck.Checked;
  CreateURLIconCheck.Enabled := Enabled and (AppURLEdit.Text <> '');
  CreateUninstallIconCheck.Enabled := Enabled;

  if Enabled then
    AppGroupNameLabel.Font.Style := AppGroupNameLabel.Font.Style + [fsBold]
  else
    AppGroupNameLabel.Font.Style := AppGroupNameLabel.Font.Style - [fsBold];
end;

procedure TWizardForm.UpdateWizardStyleImages;

  procedure UpdateWizardStyleImage(const WizardStylePngImage: TPngImage; ImageName: String);
  begin
    ImageName := ImageName.Replace('dark windows11', 'dark');
    const ImageIndex = WizardStyleImageCollection.GetIndexByName(ImageName);
    if ImageIndex = -1 then
      raise Exception.CreateFmt('Image name ''%s'' not found', [ImageName]);
    WizardStylePngImage.Assign(WizardStyleImageCollection.GetSourceImage(ImageIndex, 0, 0));
  end;

begin
  var WizardStyle := GetWizardStyle;
  const Dynamic = WizardStyle.Contains('dynamic');
  if Dynamic then begin
    WizardStyle := WizardStyle.Replace('dynamic', 'dark');
    UpdateWizardStyleImage(WizardStyleImage2.PngImage, WizardStyle); { This image is always invisible }
    WizardStyle := WizardStyle.Replace(' dark', '');
  end;
  UpdateWizardStyleImage(WizardStyleImage.PngImage, WizardStyle);

  { To keep things simple this timer is always running but here we do reset it so the new images
    will never be swapped too quickly }
  WizardStyleImageTimer.Enabled := False;
  WizardStyleImageTimer.Enabled := True;
end;

{---}

procedure TWizardForm.AppRootDirComboBoxChange(Sender: TObject);
begin
  if AppRootDirComboBox.ItemIndex = AppRootDirComboBox.Items.Count-1 then begin
    AppRootDirEdit.Enabled := True;
    AppRootDirEdit.Color := clWindow;
    ActiveControl := AppRootDirEdit;
  end else begin
    AppRootDirEdit.Enabled := False;
    AppRootDirEdit.Color := clBtnFace;
  end;
end;

procedure TWizardForm.NotCreateAppDirCheckClick(Sender: TObject);
var
  Enabled: Boolean;
begin
  Enabled := not NotCreateAppDirCheck.Checked;

  { AppDir }
  AppRootDirLabel.Enabled := Enabled;
  AppRootDirComboBox.Enabled := Enabled;
  AppRootDirComboBox.Color := EnabledColors[Enabled];
  AppRootDirEdit.Enabled := Enabled and (AppRootDirComboBox.ItemIndex = AppRootDirComboBox.Items.Count-1);
  AppRootDirEdit.Color := EnabledColors[AppRootDirEdit.Enabled];
  AppDirNameLabel.Enabled := Enabled;
  AppDirNameEdit.Enabled := Enabled;
  AppDirNameEdit.Color := EnabledColors[Enabled];
  NotDisableDirPageCheck.Enabled := Enabled;

  if Enabled then begin
    AppRootDirLabel.Font.Style := AppRootDirLabel.Font.Style + [fsBold];
    AppDirNameLabel.Font.Style := AppDirNameLabel.Font.Style + [fsBold];
  end else begin
    AppRootDirLabel.Font.Style := AppRootDirLabel.Font.Style - [fsBold];
    AppDirNameLabel.Font.Style := AppDirNameLabel.Font.Style - [fsBold];
  end;

  { AppFiles }
  UpdateAppExeControls;

  { AppAssoc }
  UpdateAppAssocControls;
end;

procedure TWizardForm.AppExeButtonClick(Sender: TObject);
var
  FileName: String;
begin
  FileName := AppExeEdit.Text;
  if NewGetOpenFileName('', FileName, PathExtractPath(FileName),
       Format(SLitExtAndAllFilter, [LFmtMessage(SExeFiles), SLitExeExt, LFmtMessage(SAllFiles)]),
       SLitExeExt, Handle) then
    AppExeEdit.Text := FileName;
end;

procedure TWizardForm.NoAppExeCheckClick(Sender: TObject);
begin
  UpdateAppExeControls;
  UpdateAppAssocControls;
  UpdateAppIconsControls;
end;

procedure TWizardForm.CreateAssocCheckClick(Sender: TObject);
begin
  UpdateAppAssocControls;
end;

procedure TWizardForm.UseAutoProgramsCheckClick(Sender: TObject);
begin
  UpdateAppIconsControls;
end;

procedure TWizardForm.NotDisableProgramGroupPageCheckClick(
  Sender: TObject);
begin
  UpdateAppIconsControls;
end;

procedure TWizardForm.FileButtonClick(Sender: TObject);
var
  Edit: TEdit;
  Filter, DefaultExt, FileName: String;
begin
  if Sender = AppLicenseFileButton then
    Edit := AppLicenseFileEdit
  else if Sender = AppInfoBeforeFileButton then
    Edit := AppInfoBeforeFileEdit
  else if Sender = AppInfoAfterFileButton then
    Edit := AppInfoAfterFileEdit
  else
    Edit := SetupIconFileEdit;

  if Sender <> SetupIconFileButton then begin
    Filter := Format(SLitDocsAndAllFilter, [LFmtMessage(SDocFiles), LFmtMessage(SAllFiles)]);
    DefaultExt := SLitRtfExt;
  end else begin
    Filter := Format(SLitExtAndAllFilter, [LFmtMessage(SIcoFiles), SLitIcoExt, LFmtMessage(SAllFiles)]);
    DefaultExt := SLitIcoExt;
  end;

  FileName := Edit.Text;
  if NewGetOpenFileName('', FileName, PathExtractPath(FileName), Filter, DefaultExt, Handle) then
    Edit.Text := FileName;
end;

procedure TWizardForm.OutputDirButtonClick(Sender: TObject);
var
  Path: String;
begin
  Path := OutputDirEdit.Text;
  if PathDrivePartLength(Path) = 0 then
    Path := '';  { don't pass in a relative path to BrowseForFolder }
  if BrowseForFolder(LFmtMessage(SWizardCompilerOutputDir), Path, Handle) then
    OutputDirEdit.Text := Path;
end;

procedure TWizardForm.PasswordEditChange(Sender: TObject);
begin
  EncryptionCheck.Enabled := PasswordEdit.Text <> '';
end;

procedure TWizardForm.AllLanguagesButtonClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to LanguagesList.Items.Count-1 do
    LanguagesList.Checked[I] := True;
end;

procedure TWizardForm.NoLanguagesButtonClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to LanguagesList.Items.Count-1 do
    LanguagesList.Checked[I] := False;
end;

procedure TWizardForm.PrivilegesRequiredOverridesAllowedDialogCheckboxClick(
  Sender: TObject);
begin
  PrivilegesRequiredOverridesAllowedCommandLineCheckbox.Enabled := not PrivilegesRequiredOverridesAllowedDialogCheckbox.Checked;
  if PrivilegesRequiredOverridesAllowedDialogCheckbox.Checked then
    PrivilegesRequiredOverridesAllowedCommandLineCheckbox.Checked := True;
end;

procedure TWizardForm.WizardStyleComboBoxChange(Sender: TObject);
begin
  if (WizardStyleDarkComboBox.Text <> 'light') and ((WizardStyleSubStyleComboBox.Text = 'slate') or (WizardStyleSubStyleComboBox.Text = 'stellar') or (WizardStyleSubStyleComboBox.Text = 'zircon')) then
    WizardStyleDarkComboBox.ItemIndex := WizardStyleDarkComboBox.Items.IndexOf('light');
  UpdateWizardStyleImages;
end;

procedure TWizardForm.WizardStyleImagePreviewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    (Sender as TForm).ModalResult := mrCancel;
end;

procedure TWizardForm.WizardStyleImagePreviewImageClick(Sender: TObject);
begin
  const F = GetParentForm(Sender as TControl);
  if F <> nil then
    F.ModalResult := mrOk;
end;

procedure TWizardForm.WizardStyleImageClick(Sender: TObject);
begin
  const PreviewForm = TForm.CreateNew(nil);
  try
    PreviewForm.AutoSize := True;
    PreviewForm.BorderStyle := bsNone;
    PreviewForm.BorderIcons := [];
    PreviewForm.Color := clWindow;
    PreviewForm.KeyPreview := True;
    PreviewForm.OnKeyDown := WizardStyleImagePreviewKeyDown;

    PreviewForm.Position := poDesigned;
    const R = BoundsRect;
    PreviewForm.Left := R.Left + MulDiv(32, CurrentPPI, 96);
    PreviewForm.Top := R.Top + MulDiv(32, CurrentPPI, 96);

    const PreviewImage = TBitmapImage.Create(PreviewForm);
    PreviewImage.AutoSize := True;
    PreviewImage.BackColor := clNone;
    PreviewImage.Bitmap.Assign(WizardStyleImage.Bitmap);
    PreviewImage.Cursor := crHandPoint;
    PreviewImage.OnClick := WizardStyleImagePreviewImageClick;
    PreviewImage.Parent := PreviewForm;

    PreviewForm.ShowModal;
  finally
    PreviewForm.Free;
  end;
end;

procedure TWizardForm.WizardStyleImageTimerTimer(Sender: TObject);
begin
  if (FCurPage = wpWizardStyle) and (WizardStyleDarkComboBox.Text = 'dynamic') then begin
    const SaveBitmap = TBitmap.Create;
    try
      SaveBitmap.Assign(WizardStyleImage.Bitmap);
      WizardStyleImage.Bitmap.Assign(WizardStyleImage2.Bitmap);
      WizardStyleImage2.Bitmap.Assign(SaveBitmap);
    finally
      SaveBitmap.Free;
    end;
  end;
end;

{ --- }

function TWizardForm.GetWizardStyle: String;
begin
  Result := WizardStyleMainComboBox.Text;
  if WizardStyleDarkComboBox.ItemIndex <> 0 then
    Result := Result + ' ' + WizardStyleDarkComboBox.Text;
  if WizardStyleSubStyleComboBox.ItemIndex <> 0 then
    Result := Result + ' ' + WizardStyleSubStyleComboBox.Text;
end;

procedure TWizardForm.GenerateScript;

  function EscapeConstArgument(const S: String): String;
  begin
    Result := S;
    StringChange(Result, '%', '%25');
    StringChange(Result, ',', '%2c');
    StringChange(Result, '}', '%7d');
  end;

var
  Script, ISPP, Setup, Languages, Tasks, Files, Registry, INI, Icons, Run, UninstallDelete: String;
  I: Integer;
  AppExeName, AppName, AppAmpEscapedName, AppConstEscapedName, AppConstEscapedVersion, AppAssocKey, LanguageName, LanguageMessagesFile: String;
begin
  Script := '';

  AppExeName := PathExtractName(AppExeEdit.Text);
  AppName := AppNameEdit.Text;
  AppAmpEscapedName := DoubleAmp(AppName);
  AppConstEscapedName := EscapeConstArgument(AppName);
  AppConstEscapedVersion := EscapeConstArgument(AppVersionEdit.Text);

  if ISPPCheck.Checked then begin
    { Setup ISPP usage. Change the edits to reflect ISPP usage. A bit ugly but for now it works. }

    ISPP := '#define MyAppName "' + AppNameEdit.Text + '"' + SNewLine +
            '#define MyAppVersion "' + AppVersionEdit.Text + '"' + SNewLine;
    if AppDirNameEdit.Text = AppNameEdit.Text then
      AppDirNameEdit.Text := '{#MyAppName}';
    if AppGroupNameEdit.Text = AppNameEdit.Text then
      AppGroupNameEdit.Text := '{#MyAppName}';
    AppNameEdit.Text := '{#MyAppName}';
    AppAmpEscapedName := '{#DoubleAmp(MyAppName)}';
    AppConstEscapedName := '{#EscapeConstArgument(MyAppName)}';
    AppVersionEdit.Text := '{#MyAppVersion}';
    AppConstEscapedVersion := '{#EscapeConstArgument(MyAppVersion)}';

    if AppPublisherEdit.Text <> '' then begin
      ISPP := ISPP + '#define MyAppPublisher "' + AppPublisherEdit.Text + '"' + SNewLine;
      AppPublisherEdit.Text := '{#MyAppPublisher}';
    end;

    if AppURLEdit.Text <> '' then begin
      ISPP := ISPP + '#define MyAppURL "' + AppURLEdit.Text + '"' + SNewLine;
      AppURLEdit.Text := '{#MyAppURL}';
    end;

    { Special ones }
    if NoAppExeCheck.Enabled and not NoAppExeCheck.Checked then begin
      ISPP := ISPP + '#define MyAppExeName "' + AppExeName + '"' + SNewLine;
      AppExeName := '{#MyAppExeName}';
    end;

    if CreateAssocCheck.Enabled and CreateAssocCheck.Checked then begin
      if Pos(AppName, AppAssocNameEdit.Text) = 1 then
        ISPP := ISPP + '#define MyAppAssocName MyAppName + "' + Copy(AppAssocNameEdit.Text, Length(AppName)+1, MaxInt) + '"' + SNewLine
      else
        ISPP := ISPP + '#define MyAppAssocName "' + AppAssocNameEdit.Text + '"' + SNewLine;
      AppAssocNameEdit.Text := '{#MyAppAssocName}';
      ISPP := ISPP + '#define MyAppAssocExt "' + AppAssocExtEdit.Text + '"' + SNewLine;
      AppAssocExtEdit.Text := '{#MyAppAssocExt}';
      ISPP := ISPP + '#define MyAppAssocKey StringChange(MyAppAssocName, " ", "") + MyAppAssocExt' + SNewLine;
      AppAssocKey := '{#MyAppAssocKey}';
    end;

    if not NotCreateAppDirCheck.Checked and not NoAppExeCheck.Checked and AppExeRunCheck.Checked then
      ISPP := ISPP + '#define DoubleAmp(Value) StringChange(Value, "&", "&&")' + SNewLine;

    ISPP := ISPP + '#define EscapeConstArgument(Value) StringChange(StringChange(StringChange(Value, "%", "%25"), ",", "%2c"), "}", "%7d")' + SNewLine;
  end else begin
    ISPP := '';
    AppAssocKey := StringReplace(AppAssocNameEdit.Text, ' ', '', [rfReplaceAll]) + AppAssocExtEdit.Text;
  end;

  Setup := '[Setup]' + SNewLine;
  Languages := '[Languages]' + SNewLine;
  Tasks := '[Tasks]' + SNewLine;
  Files := '[Files]' + SNewLine;
  Registry := '[Registry]' + SNewLine;
  INI := '[INI]' + SNewLine;
  Icons := '[Icons]' + SNewLine;
  Run := '[Run]' + SNewLine;
  UninstallDelete := '[UninstallDelete]' + SNewLine;

  if not EmptyCheck.Checked then begin
    Setup := Setup + SLitComment + LFmtMessage(SWizardScriptCommentUniqueAppId, ['AppId']) + SNewLine +
      SLitComment + LFmtMessage(SWizardScriptCommentGenerateGuid) + SNewLine;
    Setup := Setup + 'AppId={' + GenerateGuid + SNewLine;
    { AppInfo }
    Setup := Setup + 'AppName=' + AppNameEdit.Text + SNewLine;
    Setup := Setup + 'AppVersion=' + AppVersionEdit.Text + SNewLine;
    Setup := Setup + ';AppVerName={cm:NameAndVersion,' + AppConstEscapedName + ',' + AppConstEscapedVersion + '}' + SNewLine;
    if AppPublisherEdit.Text <> '' then
      Setup := Setup + 'AppPublisher=' + AppPublisherEdit.Text + SNewLine;
    if AppURLEdit.Text <> '' then begin
      Setup := Setup + 'AppPublisherURL=' + AppURLEdit.Text + SNewLine;
      Setup := Setup + 'AppSupportURL=' + AppURLEdit.Text + SNewLine;
      Setup := Setup + 'AppUpdatesURL=' + AppURLEdit.Text + SNewLine;
    end;

    { AppDir }
    if not NotCreateAppDirCheck.Checked then begin
      if AppRootDirComboBox.ItemIndex = AppRootDirComboBox.Items.Count-1 then
        Setup := Setup + 'DefaultDirName=' + AddBackslash(AppRootDirEdit.Text) + AppDirNameEdit.Text + SNewLine
      else
        Setup := Setup + 'DefaultDirName=' + AddBackslash(AppRootDirs[AppRootDirComboBox.ItemIndex].Constant) + AppDirNameEdit.Text + SNewLine;
      if not NotDisableDirPageCheck.Checked then
        Setup := Setup + 'DisableDirPage=yes' + SNewLine;
    end else begin
      Setup := Setup + 'CreateAppDir=no' + SNewLine;
    end;

    { AppFiles }
    if not NotCreateAppDirCheck.Checked and not NoAppExeCheck.Checked then begin
      Files := Files + 'Source: "' + PathExtractPath(AppExeEdit.Text) + AppExeName + '"; DestDir: "{app}"; Flags: ignoreversion' + SNewLine;
      var AppExeIsReallyExe := SameText(PathExtractExt(AppExeEdit.Text), '.exe');
      if AppExeRunCheck.Checked then begin
        if AppExeIsReallyExe then
          Run := Run + 'Filename: "{app}\' + AppExeName + '"; Description: "{cm:LaunchProgram,' + AppAmpEscapedName + '}"; Flags: nowait postinstall skipifsilent' + SNewLine
        else
          Run := Run + 'Filename: "{app}\' + AppExeName + '"; Description: "{cm:LaunchProgram,' + AppAmpEscapedName + '}"; Flags: shellexec postinstall skipifsilent' + SNewLine;
      end;
      if AppExeIsReallyExe then
        Setup := Setup + 'UninstallDisplayIcon={app}\' + AppExeName + SNewLine;
      if Is64BitPEImage(AppExeEdit.Text) then begin
        Setup := Setup + SLitComment + LFmtMessage(SWizardScriptCommentArchitecturesAllowed, ['ArchitecturesAllowed=x64compatible']) + SNewLine;
        Setup := Setup + 'ArchitecturesAllowed=x64compatible' + SNewLine;
        Setup := Setup + SLitComment + LFmtMessage(SWizardScriptCommentArchitecturesInstallIn64BitMode1, ['ArchitecturesInstallIn64BitMode=x64compatible']) + SNewLine;
        Setup := Setup + SLitComment + LFmtMessage(SWizardScriptCommentArchitecturesInstallIn64BitMode2) + SNewLine;
        Setup := Setup + 'ArchitecturesInstallIn64BitMode=x64compatible' + SNewLine;
        Setup := Setup + SLitComment + LFmtMessage(SWizardScriptCommentChangeTo64BitInstaller) + SNewLine;
        Setup := Setup + ';SetupArchitecture=x64' + SNewLine;
      end;
    end;

    var HasExtractArchive: Boolean;
    FFilesHelper.AddScript(Files, HasExtractArchive);
    if HasExtractArchive then begin
      Setup := Setup + 'ArchiveExtraction=full' + SNewLine;
      Setup := Setup + SLitComment + LFmtMessage(SWizardScriptCommentArchiveExtractionEnhanced, ['ArchiveExtraction=enhanced', SLit7zExt]) + SNewLine;
      Setup := Setup + SLitComment + LFmtMessage(SWizardScriptCommentArchiveExtractionEnhancedNoPassword, ['ArchiveExtraction=enhanced/nopassword']) + SNewLine;
    end;

    { AppAssocation }
    if CreateAssocCheck.Enabled and CreateAssocCheck.Checked then begin
      Setup := Setup + 'ChangesAssociations=yes' + SNewLine;
      Registry := Registry + 'Root: HKA; Subkey: "Software\Classes\' + AppAssocExtEdit.Text + '\OpenWithProgids"; ValueType: string; ValueName: "' + AppAssocKey + '"; ValueData: ""; Flags: uninsdeletevalue' + SNewLine;
      Registry := Registry + 'Root: HKA; Subkey: "Software\Classes\' + AppAssocKey + '"; ValueType: string; ValueName: ""; ValueData: "' + AppAssocNameEdit.Text + '"; Flags: uninsdeletekey' + SNewLine;
      Registry := Registry + 'Root: HKA; Subkey: "Software\Classes\' + AppAssocKey + '\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\' + AppExeName + ',0"' + SNewLine;
      Registry := Registry + 'Root: HKA; Subkey: "Software\Classes\' + AppAssocKey + '\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\' + AppExeName + '"" ""%1"""' + SNewLine;
    end;

    { AppGroup }
    if not NotCreateAppDirCheck.Checked then begin
      if UseAutoProgramsCheck.Enabled and UseAutoProgramsCheck.Checked then begin
        Setup := Setup + 'DisableProgramGroupPage=yes' + SNewLine;
        Icons := Icons + 'Name: "{autoprograms}\' + AppNameEdit.Text + '"; Filename: "{app}\' + AppExeName + '"' + SNewLine;
      end else begin
        Setup := Setup + 'DefaultGroupName=' + AppGroupNameEdit.Text + SNewLine;
        if not NoAppExeCheck.Checked then
          Icons := Icons + 'Name: "{group}\' + AppNameEdit.Text + '"; Filename: "{app}\' + AppExeName + '"' + SNewLine;
        if not NotDisableProgramGroupPageCheck.Checked then
          Setup := Setup + 'DisableProgramGroupPage=yes' + SNewLine;
        if AllowNoIconsCheck.Checked and NotDisableProgramGroupPageCheck.Checked then
          Setup := Setup + 'AllowNoIcons=yes' + SNewLine;
        if CreateURLIconCheck.Enabled and CreateURLIconCheck.Checked then
          Icons := Icons + 'Name: "{group}\{cm:ProgramOnTheWeb,' + AppNameEdit.Text + '}"; Filename: "' + AppURLEdit.Text + '"' + SNewLine;
        if CreateUninstallIconCheck.Checked then
          Icons := Icons + 'Name: "{group}\{cm:UninstallProgram,' + AppNameEdit.Text + '}"; Filename: "{uninstallexe}"' + SNewLine;
      end;
      if DesktopIconCheck.Enabled and DesktopIconCheck.Checked then begin
        Tasks := Tasks + 'Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked' + SNewLine;
        Icons := Icons + 'Name: "{autodesktop}\' + AppNameEdit.Text + '"; Filename: "{app}\' + AppExeName + '"; Tasks: desktopicon' + SNewLine;
      end;
    end;

    { AppDocs }
    if AppLicenseFileEdit.Text <> '' then
      Setup := Setup + 'LicenseFile=' + AppLicenseFileEdit.Text + SNewLine;
    if AppInfoBeforeFileEdit.Text <> '' then
      Setup := Setup + 'InfoBeforeFile=' + AppInfoBeforeFileEdit.Text + SNewLine;
    if AppInfoAfterFileEdit.Text <> '' then
      Setup := Setup + 'InfoAfterFile=' + AppInfoAfterFileEdit.Text + SNewLine;

    { PrivilegesRequired }
    if PrivilegesRequiredAdminRadioButton.Checked then
      Setup := Setup + SLitComment + LFmtMessage(SWizardScriptCommentChangeToLowest) + SNewLine + ';'
    else
      Setup := Setup + SLitComment + LFmtMessage(SWizardScriptCommentChangeToAdmin) + SNewLine;
    Setup := Setup + 'PrivilegesRequired=lowest' + SNewLine; { Note how previous made sure this is outputted as comment if needed. }
    if PrivilegesRequiredOverridesAllowedDialogCheckbox.Checked then
      Setup := Setup + 'PrivilegesRequiredOverridesAllowed=dialog' + SNewLine
    else if PrivilegesRequiredOverridesAllowedCommandLineCheckbox.Checked then
      Setup := Setup + 'PrivilegesRequiredOverridesAllowed=commandline' + SNewLine;
      
    { AppRegistry }
    FRegistryHelper.AddScript(Registry, False);

    { Languages }
    if FLanguages.Count > 1 then begin
      var UsingISPPEmitLanguagesSection: Boolean;

      if ISPPCheck.Checked then begin
        UsingISPPEmitLanguagesSection := True;
        for I := 0 to LanguagesList.Items.Count-1 do begin
          if not LanguagesList.Checked[I] then begin
            UsingISPPEmitLanguagesSection := False;
            Break;
          end;
        end;
      end else
        UsingISPPEmitLanguagesSection := False;

      if UsingISPPEmitLanguagesSection then
        ISPP := ISPP + '#call EmitLanguagesSection' + SNewLine
      else begin
        for I := 0 to LanguagesList.Items.Count-1 do begin
          if LanguagesList.Checked[I] then begin
            LanguageMessagesFile := FLanguages[Integer(LanguagesList.ItemObject[I])];
            if LanguageMessagesFile <> LanguagesDefaultIsl then begin
              LanguageName := LanguagesList.Items[I];
              LanguageMessagesFile := 'Languages\' + LanguageMessagesFile;
            end else
              LanguageName := LanguagesDefaultIslDescription;
            StringChange(LanguageName, ' ', '');
            LanguageName := LowerCase(LanguageName);
            Languages := Languages + 'Name: "' + LanguageName + '"; MessagesFile: "compiler:' + LanguageMessagesFile + '"' + SNewLine;
          end;
        end;
      end;
    end;

    { Compiler }
    if OutputDirEdit.Text <> '' then
      Setup := Setup + 'OutputDir=' + OutputDirEdit.Text + SNewLine;
    if OutputBaseFileNameEdit.Text <> '' then
      Setup := Setup + 'OutputBaseFilename=' + OutputBaseFileNameEdit.Text + SNewLine;
    if SetupIconFileEdit.Text <> '' then
      Setup := Setup + 'SetupIconFile=' + SetupIconFileEdit.Text + SNewLine;
    if PasswordEdit.Text <> '' then begin
      Setup := Setup + 'Password=' + PasswordEdit.Text + SNewLine;
      if EncryptionCheck.Checked then
        Setup := Setup + 'Encryption=yes' + SNewLine;
    end;

    { Other }
    Setup := Setup + 'SolidCompression=yes' + SNewLine;
    Setup := Setup + 'WizardStyle=' + GetWizardStyle + SNewLine;

    { Build script }
    if ISPP <> '' then
      Script := Script + ISPP + SNewLine;
    Script := Script + Setup + SNewLine;
    if Length(Languages) > Length('[Languages]')+2 then
      Script := Script + Languages + SNewLine;
    if Length(Tasks) > Length('[Tasks]')+2 then
      Script := Script + Tasks + SNewLine;
    if Length(Files) > Length('[Files]')+2 then
      Script := Script + Files + SLitComment + LFmtMessage(SWizardScriptCommentSharedSystemFiles, ['Flags: ignoreversion']) + SNewLine2;
    if Length(Registry) > Length('[Registry]')+2 then
      Script := Script + Registry + SNewLine;
    if Length(INI) > Length('[INI]')+2 then
      Script := Script + INI + SNewLine;
    if Length(Icons) > Length('[Icons]')+2 then
      Script := Script + Icons + SNewLine;
    if Length(Run) > Length('[Run]')+2 then
      Script := Script + Run + SNewLine;
    if Length(UninstallDelete) > Length('[UninstallDelete]')+2 then
      Script := Script + UninstallDelete + SNewLine;

    FResult := wrComplete;
  end else begin
    Script := Script + Setup;
    FResult := wrEmpty;
  end;

  FResultScript := SLitComment + FixLabel(LFmtMessage(SWizardScriptHeader1, ['[name]'])) + SNewLine +
    SLitComment + LFmtMessage(SWizardScriptHeader2) + SNewLine;
  if (FResult = wrComplete) and not IsLicensed then
    FResultScript := FResultScript + SLitComment + AddPeriod(GetLicenseeDescription) + SNewLine;
  FResultScript := FResultScript + SNewLine + Script;
end;

{ --- }

end.
