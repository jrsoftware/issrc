unit IDE.Wizard.WizardForm;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE Script Wizard form
}

interface

uses
  Windows, Forms, Classes, Graphics, StdCtrls, ExtCtrls, Controls, Dialogs, pngimage,
  UIStateForm, NewStaticText, DropListBox, NewCheckListBox, NewNotebook,
  IDE.Wizard.WizardFormFilesHelper, IDE.Wizard.WizardFormRegistryHelper;

type
  TWizardPage = (wpWelcome, wpAppInfo, wpAppDir, wpAppFiles, wpAppAssoc, wpAppIcons,
                 wpAppDocs, wpPrivilegesRequired, wpAppRegistry, wpLanguages, wpCompiler,
                 wpISPP, wpFinished);

  TWizardFormResult = (wrNone, wrEmpty, wrComplete);

  TWizardForm = class(TUIStateForm)
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
    WelcomeImage: TImage;
    WelcomeLabel1: TNewStaticText;
    PnlMain: TPanel;
    Bevel1: TBevel;
    PageNameLabel: TNewStaticText;
    PageDescriptionLabel: TNewStaticText;
    InnerImage: TImage;
    FinishedLabel: TNewStaticText;
    FinishedImage: TImage;
    WelcomeLabel2: TNewStaticText;
    EmptyCheck: TCheckBox;
    WelcomeLabel3: TNewStaticText;
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
    AppRegistryMinVerDocImage: TImage;
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
  private
    CurPage: TWizardPage;
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
    procedure GenerateScript;
  public
    property WizardName: String write SetWizardName;
    property Result: TWizardFormResult read FResult;
    property ResultScript: String read FResultScript;
  end;

implementation

{$R *.DFM}

uses
  SysUtils, ShlObj, ActiveX, UITypes, Shared.FileClass,
  PathFunc, Shared.CommonFunc.Vcl, Shared.CommonFunc, IDE.HelperFunc, BrowseFunc,
  IDE.Messages, IDE.Wizard.WizardFileForm;

type
  TConstant = record
    Constant, Description: String;
  end;

const
  NotebookPages: array[TWizardPage, 0..1] of Integer =
    ((0, -1), (1, 0), (1, 1), (1, 2),
     (1, 3), (1, 4), (1, 5), (1, 6),
     (1, 7), (1, 8), (1, 9), (1, 10), (2, -1));

  PageCaptions: array[TWizardPage] of String =
    (SWizardWelcome, SWizardAppInfo, SWizardAppDir, SWizardAppFiles, SWizardAppAssoc,
     SWizardAppIcons, SWizardAppDocs, SWizardPrivilegesRequired, SWizardAppRegistry,
     SWizardLanguages, SWizardCompiler, SWizardISPP, SWizardFinished);

  PageDescriptions: array[TWizardPage] of String =
    ('', SWizardAppInfo2, SWizardAppDir2, SWizardAppFiles2, SWizardAppAssoc2,
         SWizardAppIcons2, SWizardAppDocs2, SWizardPrivilegesRequired2, SWizardAppRegistry2,
         SWizardLanguages2, SWizardCompiler2, SWizardISPP2, '');

  RequiredLabelVisibles: array[TWizardPage] of Boolean =
    (False, True, True, True, True, True, False, True, False, True, False, False, False);

  AppRootDirs: array[0..0] of TConstant =
  (
    ( Constant: '{autopf}'; Description: 'Program Files folder')
  );

  LanguagesDefaultIsl = 'Default.isl';
  LanguagesDefaultIslDescription = 'English';

  EnabledColors: array[Boolean] of TColor = (clBtnFace, clWindow);

function EscapeAmpersands(const S: String): String;
begin
  Result := S;
  StringChangeEx(Result, '&', '&&', True);
end;

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

var
  I: Integer;
begin
  FResult := wrNone;

  FWizardName := SWizardDefaultName;
  FFilesHelper := TWizardFormFilesHelper.Create(Self,
    NotCreateAppDirCheck, AppFilesListBox, AppFilesAddButton, AppFilesAddDirButton,
    AppFilesEditButton, AppFilesRemoveButton);
  FRegistryHelper := TWizardFormRegistryHelper.Create(Self, AppRegistryFileEdit,
    AppRegistryFileButton, AppRegistryUninsDeleteKeyCheck,
    AppRegistryUninsDeleteKeyIfEmptyCheck, AppRegistryUninsDeleteValueCheck,
    AppRegistryMinVerCheck, AppRegistryMinVerEdit, AppRegistryMinVerDocImage);

  FLanguages := TStringList.Create;
  FLanguages.Sorted := True;
  FLanguages.Duplicates := dupIgnore; { Some systems also return .islu files when searching for *.isl }
  AddLanguages('isl');
  AddLanguages('islu');
  FLanguages.Sorted := False;
  FLanguages.Insert(0, LanguagesDefaultIsl);

  InitFormFont(Self);
  if Font.Name = 'Segoe UI' then begin
    { See Setup.WizardForm.pas }
    for I := 0 to OuterNotebook.PageCount-1 do
      OuterNotebook.Pages[I].HandleNeeded;
    for I := 0 to InnerNotebook.PageCount-1 do
      InnerNotebook.Pages[I].HandleNeeded;
    ClientWidth := MulDiv(ClientWidth, 105, 100);
  end;
  if FontExists('Verdana') then
    WelcomeLabel1.Font.Name := 'Verdana';

  OuterNotebook.Color := clWindow;

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

  FinishedImage.Picture := WelcomeImage.Picture;

  RequiredLabel2.Left := RequiredLabel1.Left + RequiredLabel1.Width;

  { AppInfo }
  AppNameEdit.Text := 'My Program';
  AppVersionEdit.Text := '1.5';
  AppPublisherEdit.Text := 'My Company, Inc.';
  AppURLEdit.Text := 'https://www.example.com/';

  { AppDir }
  for I := Low(AppRootDirs) to High(AppRootDirs) do
    AppRootDirComboBox.Items.Add(AppRootDirs[I].Description);
  AppRootDirComboBox.Items.Add('(Custom)');
  AppRootDirComboBox.ItemIndex := 0;
  AppRootDirEdit.Enabled := False;
  AppRootDirEdit.Color := clBtnFace;
  NotDisableDirPageCheck.Checked := True;

  { AppFiles }
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

  { Languages }
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

  { ISPP }
  ISPPLabel.Caption := FixLabel(SWizardISPPLabel);
  ISPPCheck.Caption := SWizardISPPCheck;
  ISPPCheck.Checked := ISPPInstalled;

  CurPage := Low(TWizardPage);
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
    CanClose := MsgBox(FixLabel(SWizardCancelMessage), FWizardName, mbConfirmation, MB_YESNO) = idYes;
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
  OuterNotebook.ActivePage := OuterNotebook.Pages[NotebookPages[CurPage, 0]];
  if NotebookPages[CurPage, 1] <> -1 then
    InnerNotebook.ActivePage := InnerNotebook.Pages[NotebookPages[CurPage, 1]];

  { Set button visibility and captions }
  BackButton.Visible := not (CurPage = wpWelcome);
  if CurPage = wpFinished then
    NextButton.Caption := SWizardFinishButton
  else
    NextButton.Caption := SWizardNextButton;

  RequiredLabel1.Visible := RequiredLabelVisibles[CurPage];
  RequiredLabel2.Visible := RequiredLabel1.Visible;

  { Set the Caption to match the current page's title }
  PageNameLabel.Caption := PageCaptions[CurPage];
  PageDescriptionLabel.Caption := PageDescriptions[CurPage];

  { Adjust focus }
  case CurPage of
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
    wpISPP: ActiveControl := ISPPCheck;
  end;
end;

function TWizardForm.SkipCurPage: Boolean;
begin
  if ((CurPage = wpAppAssoc) and not CreateAssocCheck.Enabled) or
     ((CurPage = wpAppIcons) and NotCreateAppDirCheck.Checked) or
     ((CurPage = wpLanguages) and not (FLanguages.Count > 1)) or
     ((CurPage = wpISPP) and not ISPPInstalled) or
     (not (CurPage in [wpWelcome, wpFinished]) and EmptyCheck.Checked) then
    Result := True
  else
    Result := False;
end;

procedure TWizardForm.NextButtonClick(Sender: TObject);

  function CheckAppInfoPage: Boolean;
  begin
    Result := False;

    if AppNameEdit.Text = '' then begin
      MsgBox(SWizardAppNameError, '',  mbError, MB_OK);
      ActiveControl := AppNameEdit;
    end else if AppVersionEdit.Text = '' then begin
      MsgBox(SWizardAppVersionError, '',  mbError, MB_OK);
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
      MsgBox(SWizardAppRootDirError, '',  mbError, MB_OK);
      ActiveControl := AppRootDirEdit;
    end else if not NotCreateAppDirCheck.Checked and (AppDirNameEdit.Text = '') then begin
      MsgBox(SWizardAppDirNameError, '', mbError, MB_OK);
      ActiveControl := AppDirNameEdit;
    end else
      Result := True;
  end;

  function CheckAppFilesPage: Boolean;
  begin
    Result := False;

    if AppExeEdit.Enabled and (AppExeEdit.Text = '') then begin
      MsgBox(SWizardAppExeError, '', mbError, MB_OK);
      ActiveControl := AppExeEdit;
    end else
      Result := True;
  end;

  function CheckAppIconsPage: Boolean;
  begin
    Result := False;

    if AppGroupNameEdit.Text = '' then begin
      MsgBox(SWizardAppGroupNameError, '', mbError, MB_OK);
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

    MsgBox(SWizardLanguagesSelError, '', mbError, MB_OK);
    ActiveControl := LanguagesList;
  end;

begin
  case CurPage of
    wpAppInfo: if not CheckAppInfoPage then Exit;
    wpAppDir: if not CheckAppDirPage then Exit;
    wpAppFiles: if not CheckAppFilesPage then Exit;
    wpAppIcons: if not CheckAppIconsPage then Exit;
    wpLanguages: if not CheckLanguagesPage then Exit;
  end;

  repeat
    if CurPage = wpAppAssoc then begin
      if (AppAssocExtEdit.Text <> '') and (AppAssocExtEdit.Text[1] <> '.') then
        AppAssocExtEdit.Text := '.' + AppAssocExtEdit.Text;
    end else if CurPage = wpPrivilegesRequired then begin
      if not PrivilegesRequiredOverridesAllowedCommandLineCheckbox.Checked then begin
        if PrivilegesRequiredAdminRadioButton.Checked then
          FRegistryHelper.PrivilegesRequired := prAdmin
        else
          FRegistryHelper.PrivilegesRequired := prLowest
      end else
        FRegistryHelper.PrivilegesRequired := prDynamic;
    end else if CurPage = wpFinished then begin
      GenerateScript;
      ModalResult := mrOk;
      Exit;
    end;
    Inc(CurPage);

    { Even if we're skipping a page, we should still update it }
    case CurPage of
      wpAppDir: if AppDirNameEdit.Text = '' then AppDirNameEdit.Text := AppNameEdit.Text;
      wpAppAssoc: if AppAssocNameEdit.Text = '' then AppAssocNameEdit.Text := AppNameEdit.Text + ' File';
      wpAppIcons: if AppGroupNameEdit.Text = '' then AppGroupNameEdit.Text := AppNameEdit.Text;
    end;
  until not SkipCurPage;

  CurPageChanged;
end;

procedure TWizardForm.BackButtonClick(Sender: TObject);
begin
  if CurPage = Low(TWizardPage) then Exit;

  { Go to the previous page }
  Dec(CurPage);
  while SkipCurPage do
    Dec(CurPage);
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
  Enabled := not NoAppExeCheck.Checked;
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
    AppDirNameLabel.Font.Style := AppRootDirLabel.Font.Style + [fsBold];
  end else begin
    AppRootDirLabel.Font.Style := AppRootDirLabel.Font.Style - [fsBold];
    AppDirNameLabel.Font.Style := AppRootDirLabel.Font.Style - [fsBold];
  end;

  { AppFiles }
  UpdateAppExeControls;
end;

procedure TWizardForm.AppExeButtonClick(Sender: TObject);
var
  FileName: String;
begin
  FileName := AppExeEdit.Text;
  if NewGetOpenFileName('', FileName, PathExtractPath(FileName), SWizardAppExeFilter, SWizardAppExeDefaultExt, Handle) then
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
    Filter := SWizardAppDocsFilter;
    DefaultExt := SWizardAppDocsDefaultExt;
  end else begin
    Filter := SWizardCompilerSetupIconFileFilter;
    DefaultExt := SWizardCompilerSetupIconFileDefaultExt;
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
  if BrowseForFolder(SWizardCompilerOutputDir, Path, Handle, True) then
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

{ --- }

procedure TWizardForm.GenerateScript;

  function Is64BitPEImage(const Filename: String): Boolean;
  { Returns True if the specified file is a non-32-bit PE image, False
    otherwise. }
  var
    F: TFile;
    DosHeader: packed record
      Sig: array[0..1] of AnsiChar;
      Other: array[0..57] of Byte;
      PEHeaderOffset: LongWord;
    end;
    PESigAndHeader: packed record
      Sig: DWORD;
      Header: TImageFileHeader;
      OptHeaderMagic: Word;
    end;
  begin
    Result := False;
    F := TFile.Create(Filename, fdOpenExisting, faRead, fsRead);
    try
      if F.Read(DosHeader, SizeOf(DosHeader)) = SizeOf(DosHeader) then begin
        if (DosHeader.Sig[0] = 'M') and (DosHeader.Sig[1] = 'Z') and
           (DosHeader.PEHeaderOffset <> 0) then begin
          F.Seek(DosHeader.PEHeaderOffset);
          if F.Read(PESigAndHeader, SizeOf(PESigAndHeader)) = SizeOf(PESigAndHeader) then begin
            if (PESigAndHeader.Sig = IMAGE_NT_SIGNATURE) and
               (PESigAndHeader.OptHeaderMagic <> IMAGE_NT_OPTIONAL_HDR32_MAGIC) then
              Result := True;
          end;
        end;
      end;
    finally
      F.Free;
    end;
  end;

var
  Script, ISPP, Setup, Languages, Tasks, Files, Registry, INI, Icons, Run, UninstallDelete: String;
  I: Integer;
  AppExeName, AppName, AppAmpEscapedName, AppAssocKey, LanguageName, LanguageMessagesFile: String;
begin
  Script := '';

  AppExeName := PathExtractName(AppExeEdit.Text);
  AppName := AppNameEdit.Text;
  AppAmpEscapedName := EscapeAmpersands(AppName);

  if ISPPCheck.Checked then begin
    { Setup ISPP usage. Change the edits to reflect ISPP usage. A bit ugly but for now it works. }

    ISPP := '#define MyAppName "' + AppNameEdit.Text + '"' + SNewLine +
            '#define MyAppVersion "' + AppVersionEdit.Text + '"' + SNewLine;
    if AppDirNameEdit.Text = AppNameEdit.Text then
      AppDirNameEdit.Text := '{#MyAppName}';
    if AppGroupNameEdit.Text = AppNameEdit.Text then
      AppGroupNameEdit.Text := '{#MyAppName}';
    AppNameEdit.Text := '{#MyAppName}';
    AppAmpEscapedName := '{#StringChange(MyAppName, ''&'', ''&&'')}';
    AppVersionEdit.Text := '{#MyAppVersion}';

    if AppPublisherEdit.Text <> '' then begin
      ISPP := ISPP + '#define MyAppPublisher "' + AppPublisherEdit.Text + '"' + SNewLine;
      AppPublisherEdit.Text := '{#MyAppPublisher}';
    end;

    if AppURLEdit.Text <> '' then begin
      ISPP := ISPP + '#define MyAppURL "' + AppURLEdit.Text + '"' + SNewLine;
      AppURLEdit.Text := '{#MyAppURL}';
    end;

    { Special ones }
    if not NoAppExeCheck.Checked then begin
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
    Setup := Setup + (
      '; NOTE: The value of AppId uniquely identifies this application. Do not use the same AppId value in installers for other applications.' + SNewLine +
      '; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)' + SNewLine);
    Setup := Setup + 'AppId={' + GenerateGuid + SNewLine;
    { AppInfo }
    Setup := Setup + 'AppName=' + AppNameEdit.Text + SNewLine;
    Setup := Setup + 'AppVersion=' + AppVersionEdit.Text + SNewLine;
    Setup := Setup + ';AppVerName=' + AppNameEdit.Text + ' ' + AppVersionEdit.Text + SNewLine;
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
        Setup := Setup + '; "ArchitecturesAllowed=x64compatible" specifies that Setup cannot run' + SNewLine;
        Setup := Setup + '; on anything but x64 and Windows 11 on Arm.' + SNewLine;
        Setup := Setup + 'ArchitecturesAllowed=x64compatible' + SNewLine;
        Setup := Setup + '; "ArchitecturesInstallIn64BitMode=x64compatible" requests that the' + SNewLine;
        Setup := Setup + '; install be done in "64-bit mode" on x64 or Windows 11 on Arm,' + SNewLine;
        Setup := Setup + '; meaning it should use the native 64-bit Program Files directory and' + SNewLine;
        Setup := Setup + '; the 64-bit view of the registry.' + SNewLine;
        Setup := Setup + 'ArchitecturesInstallIn64BitMode=x64compatible' + SNewLine;
      end;
    end;

    { AppAssocation }
    if CreateAssocCheck.Enabled and CreateAssocCheck.Checked then begin
      Setup := Setup + 'ChangesAssociations=yes' + SNewLine;
      Registry := Registry + 'Root: HKA; Subkey: "Software\Classes\' + AppAssocExtEdit.Text + '\OpenWithProgids"; ValueType: string; ValueName: "' + AppAssocKey + '"; ValueData: ""; Flags: uninsdeletevalue' + SNewLine;
      Registry := Registry + 'Root: HKA; Subkey: "Software\Classes\' + AppAssocKey + '"; ValueType: string; ValueName: ""; ValueData: "' + AppAssocNameEdit.Text + '"; Flags: uninsdeletekey' + SNewLine;
      Registry := Registry + 'Root: HKA; Subkey: "Software\Classes\' + AppAssocKey + '\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\' + AppExeName + ',0"' + SNewLine;
      Registry := Registry + 'Root: HKA; Subkey: "Software\Classes\' + AppAssocKey + '\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\' + AppExeName + '"" ""%1"""' + SNewLine;
    end;

    FFilesHelper.AddScript(Files);

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
      Setup := Setup + '; Uncomment the following line to run in non administrative install mode (install for current user only).' + SNewLine + ';'
    else
      Setup := Setup + '; Remove the following line to run in administrative install mode (install for all users).' + SNewLine;
    Setup := Setup + 'PrivilegesRequired=lowest' + SNewLine; { Note how previous made sure this is outputted as comment if needed. }
    if PrivilegesRequiredOverridesAllowedDialogCheckbox.Checked then
      Setup := Setup + 'PrivilegesRequiredOverridesAllowed=dialog' + SNewLine
    else if PrivilegesRequiredOverridesAllowedCommandLineCheckbox.Checked then
      Setup := Setup + 'PrivilegesRequiredOverridesAllowed=commandline' + SNewLine;
      
    { AppRegistry }
    FRegistryHelper.AddScript(Registry, False);

    { Languages }
    if FLanguages.Count > 1 then begin
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
    Setup := Setup + 'WizardStyle=modern' + SNewLine;

    { Build script }
    if ISPP <> '' then
      Script := Script + ISPP + SNewLine;
    Script := Script + Setup + SNewLine;
    if Length(Languages) > Length('[Languages]')+2 then
      Script := Script + Languages + SNewLine;
    if Length(Tasks) > Length('[Tasks]')+2 then
      Script := Script + Tasks + SNewLine;
    if Length(Files) > Length('[Files]')+2 then
      Script := Script + Files +
        '; NOTE: Don''t use "Flags: ignoreversion" on any shared system files' +
        SNewLine2;
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

  FResultScript := FixLabel(SWizardScriptHeader) + SNewLine2 + Script;
end;

{ --- }

end.
