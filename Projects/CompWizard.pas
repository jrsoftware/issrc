unit CompWizard;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler Script Wizard form

  $jrsoftware: issrc/Projects/CompWizard.pas,v 1.68 2011/04/16 05:51:19 jr Exp $
}

interface

{$I VERSION.INC}

uses
  Windows, Forms, Classes, Graphics, StdCtrls, ExtCtrls, Controls, Dialogs,
  UIStateForm, NewStaticText, DropListBox, NewCheckListBox;

type
  TWizardPage = (wpWelcome, wpAppInfo, wpAppDir, wpAppFiles, wpAppIcons,
                 wpAppDocs, wpLanguages, wpCompiler, wpISPP, wpFinished);

  TWizardFormResult = (wrNone, wrEmpty, wrComplete);

  TWizardForm = class(TUIStateForm)
    CancelButton: TButton;
    NextButton: TButton;
    BackButton: TButton;
    Notebook1: TNotebook;
    Notebook2: TNotebook;
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
    QuickLaunchIconCheck: TCheckBox;
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
    procedure AppFilesListBoxClick(Sender: TObject);
    procedure AppFilesListBoxDblClick(Sender: TObject);
    procedure AppFilesAddButtonClick(Sender: TObject);
    procedure NotDisableProgramGroupPageCheckClick(Sender: TObject);
    procedure AppFilesEditButtonClick(Sender: TObject);
    procedure AppFilesRemoveButtonClick(Sender: TObject);
    procedure AppFilesAddDirButtonClick(Sender: TObject);
    procedure AppFilesListBoxDropFile(Sender: TDropListBox;
      const FileName: String);
    procedure PasswordEditChange(Sender: TObject);
    procedure OutputDirButtonClick(Sender: TObject);
    procedure AllLanguagesButtonClick(Sender: TObject);
    procedure NoLanguagesButtonClick(Sender: TObject);
    procedure NoAppExeCheckClick(Sender: TObject);
  private
    CurPage: TWizardPage;
    FWizardName: String;
    FWizardFiles: TList;
    FLanguages: TStringList;
    FResult: TWizardFormResult;
    FResultScript: String;
    function FixLabel(const S: String): String;
    procedure SetWizardName(const WizardName: String);
    function ISPPInstalled: Boolean;
    function ISCryptInstalled: Boolean;
    procedure CurPageChanged;
    function SkipCurPage: Boolean;
    procedure AddWizardFile(const Source: String; const RecurseSubDirs, CreateAllSubDirs: Boolean);
    procedure UpdateWizardFiles;
    procedure UpdateWizardFilesButtons;
    procedure UpdateAppExeControls;
    procedure GenerateScript;
  public
    property WizardName: String write SetWizardName;
    property Result: TWizardFormResult read FResult;
    property ResultScript: String read FResultScript;
  end;

implementation

{$R *.DFM}

uses
  SysUtils, ShlObj, {$IFNDEF Delphi3orHigher} Ole2, {$ELSE} ActiveX, {$ENDIF}
  PathFunc, CmnFunc, CmnFunc2, VerInfo, BrowseFunc,
  CompMsgs, CompWizardFile, CompForm;

type
  TConstant = record
    Constant, Description: String;
  end;

const
  NotebookPages: array[TWizardPage, 0..1] of Integer =
    ((0, -1), (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7), (2, -1));

  PageCaptions: array[TWizardPage] of String =
    (SWizardWelcome, SWizardAppInfo, SWizardAppDir, SWizardAppFiles,
     SWizardAppIcons, SWizardAppDocs, SWizardLanguages,
     SWizardCompiler, SWizardISPP, SWizardFinished);

  PageDescriptions: array[TWizardPage] of String =
    ('', SWizardAppInfo2, SWizardAppDir2, SWizardAppFiles2,
         SWizardAppIcons2, SWizardAppDocs2, SWizardLanguages2,
         SWizardCompiler2, SWizardISPP2, '');

  RequiredLabelVisibles: array[TWizardPage] of Boolean =
    (False, True, True, True, True, False, True, False, False, False);

  AppRootDirs: array[0..0] of TConstant =
  (
    ( Constant: '{pf}'; Description: 'Program Files folder')
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

function TWizardForm.ISPPInstalled(): Boolean;
begin
  Result := NewFileExists(PathExtractPath(NewParamStr(0)) + 'ISPP.dll');
end;

function TWizardForm.ISCryptInstalled(): Boolean;
begin
  Result := NewFileExists(PathExtractPath(NewParamStr(0)) + 'iscrypt.dll');
end;

{ --- }

{$IFDEF IS_D7}
type
  TNotebookAccess = class(TNotebook);
{$ENDIF}

procedure TWizardForm.FormCreate(Sender: TObject);

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
  SearchRec: TSearchRec;
  I: Integer;
begin
  FResult := wrNone;

  FWizardName := SWizardDefaultName;
  FWizardFiles := TList.Create;

  FLanguages := TStringList.Create;
  if FindFirst(PathExtractPath(NewParamStr(0)) + 'Languages\*.isl', faAnyFile, SearchRec) = 0 then begin
    repeat
      FLanguages.Add(SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  FLanguages.Sort;
  FLanguages.Insert(0, LanguagesDefaultIsl);

  InitFormFont(Self);
  if FontExists('Verdana') then
    WelcomeLabel1.Font.Name := 'Verdana';

{$IFDEF IS_D7}
  TNotebookAccess(Notebook1).ParentBackground := False;
  PnlMain.ParentBackground := False;
{$ENDIF}

  MakeBold(PageNameLabel);
  MakeBold(RequiredLabel1);
  MakeBold(AppNameLabel);
  MakeBold(AppVersionLabel);
  MakeBold(AppRootDirLabel);
  MakeBold(AppDirNameLabel);
  MakeBold(AppExeLabel);
  MakeBold(AppGroupNameLabel);
  MakeBold(LanguagesLabel);

  FinishedImage.Picture := WelcomeImage.Picture;

  RequiredLabel2.Left := RequiredLabel1.Left + RequiredLabel1.Width;

  { AppInfo }
  AppNameEdit.Text := 'My Program';
  AppVersionEdit.Text := '1.5';
  AppPublisherEdit.Text := 'My Company, Inc.';
  AppURLEdit.Text := 'http://www.example.com/';

  { AppDir }
  for I := Low(AppRootDirs) to High(AppRootDirs) do
    AppRootDirComboBox.Items.Add(AppRootDirs[I].Description);
  AppRootDirComboBox.Items.Add('(Custom)');
  AppRootDirComboBox.ItemIndex := 0;
  AppRootDirEdit.Enabled := False;
  AppRootDirEdit.Color := clBtnFace;
  NotDisableDirPageCheck.Checked := True;

  { AppFiles }
  AppExeEdit.Text := PathExtractPath(NewParamStr(0)) + 'Examples\MyProg.exe';
  AppExeRunCheck.Checked := True;
  UpdateWizardFilesButtons;

  { AppIcons }
  NotDisableProgramGroupPageCheck.Checked := True;
  DesktopIconCheck.Checked := True;

  { Languages }
  for I := 0 to FLanguages.Count-1 do begin
    if FLanguages[I] <> LanguagesDefaultIsl then
      LanguagesList.AddCheckBox(SpaceLanguageName(PathChangeExt(FLanguages[I], '')), '', 0, False, True, False, True, TObject(I))
    else
      LanguagesList.AddCheckBox(LanguagesDefaultIslDescription, '', 0, True, True, False, True, TObject(I));
  end;

  { Compiler }
  OutputBaseFileNameEdit.Text := 'setup';
  EncryptionCheck.Visible := ISCryptInstalled;
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
var
  I: Integer;
begin
  FLanguages.Free;
  for I := 0 to FWizardFiles.Count-1 do
    Dispose(FWizardFiles[i]);
  FWizardFiles.Free;
end;

{ --- }

procedure TWizardForm.CurPageChanged;
{ Call this whenever the current page is changed }
begin
  Notebook1.PageIndex := NotebookPages[CurPage, 0];
  if NotebookPages[CurPage, 1] <> -1 then
    Notebook2.PageIndex := NotebookPages[CurPage, 1];

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
  if CurPage in [wpWelcome, wpFinished] then
    Notebook1.Color := clWindow
  else
    Notebook1.Color := clBtnFace;

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
    wpAppIcons: ActiveControl := AppGroupNameEdit;
    wpAppDocs: ActiveControl := AppLicenseFileEdit;
    wpLanguages: ActiveControl := LanguagesList;
    wpCompiler: ActiveControl := OutputDirEdit;
    wpISPP: ActiveControl := ISPPCheck;
  end;
end;

function TWizardForm.SkipCurPage: Boolean;
begin
  if ((CurPage = wpAppIcons) and NotCreateAppDirCheck.Checked) or
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
    if CurPage = wpFinished then begin
      GenerateScript;
      ModalResult := mrOk;
      Exit;
    end;
    Inc(CurPage);

    { Even if we're skipping a page, we should still update it }
    case CurPage of
      wpAppDir: if AppDirNameEdit.Text = '' then AppDirNameEdit.Text := AppNameEdit.Text;
      wpAppIcons:
        begin
          if AppGroupNameEdit.Text = '' then AppGroupNameEdit.Text := AppNameEdit.Text;
          CreateURLIconCheck.Enabled := AppURLEdit.Text <> '';
        end;
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

procedure TWizardForm.AddWizardFile(const Source: String; const RecurseSubDirs, CreateAllSubDirs: Boolean);
var
  WizardFile: PWizardFile;
begin
  New(WizardFile);
  WizardFile.Source := Source;
  WizardFile.RecurseSubDirs := RecurseSubDirs;
  WizardFile.CreateAllSubDirs := CreateAllSubDirs;
  WizardFile.DestRootDirIsConstant := True;
  if not NotCreateAppDirCheck.Checked then
    WizardFile.DestRootDir := '{app}'
  else
    WizardFile.DestRootDir := '{win}';
  WizardFile.DestSubDir := '';
  FWizardFiles.Add(WizardFile);
end;

procedure TWizardForm.UpdateWizardFiles;
var
  WizardFile: PWizardFile;
  I: Integer;
begin
  AppFilesListBox.Items.BeginUpdate;
  AppFilesListBox.Items.Clear;
  for I := 0 to FWizardFiles.Count-1 do begin
    WizardFile := FWizardFiles[i];
    AppFilesListBox.Items.Add(WizardFile.Source);
  end;
  AppFilesListBox.Items.EndUpdate;
  UpdateHorizontalExtent(AppFilesListBox);
end;

procedure TWizardForm.UpdateWizardFilesButtons;
var
  Enabled: Boolean;
begin
  Enabled := AppFilesListBox.ItemIndex >= 0;
  AppFilesEditButton.Enabled := Enabled;
  AppFilesRemoveButton.Enabled := Enabled;
end;

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
  QuickLaunchIconCheck.Enabled := Enabled;

  if Enabled then
    AppExeLabel.Font.Style := AppExeLabel.Font.Style + [fsBold]
  else
    AppExeLabel.Font.Style := AppExeLabel.Font.Style - [fsBold];
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
end;

procedure TWizardForm.AppFilesListBoxClick(Sender: TObject);
begin
  UpdateWizardFilesButtons;
end;

procedure TWizardForm.AppFilesListBoxDblClick(Sender: TObject);
begin
  if AppFilesEditButton.Enabled then
    AppFilesEditButton.Click;
end;

procedure TWizardForm.AppFilesAddButtonClick(Sender: TObject);
var
  FileList: TStringList;
  I: Integer;
begin
  FileList := TStringList.Create;
  try
    if NewGetOpenFileNameMulti('', FileList, '', SWizardAllFilesFilter, '', Handle) then begin
      FileList.Sort;
      for I := 0 to FileList.Count-1 do
        AddWizardFile(FileList[I], False, False);
      UpdateWizardFiles;
    end;
  finally
    FileList.Free;
  end;
end;

procedure TWizardForm.AppFilesAddDirButtonClick(Sender: TObject);
var
  Path: String;
  Recurse: Boolean;
begin
  Path := '';
  if BrowseForFolder(SWizardAppFiles3, Path, Handle, False) then begin
    case MsgBox(Format(SWizardAppFilesSubDirsMessage, [Path]), '', mbConfirmation, MB_YESNOCANCEL) of
      IDYES: Recurse := True;
      IDNO: Recurse := False;
    else
      Exit;
    end;
    AddWizardFile(AddBackslash(Path) + '*', Recurse, Recurse);
    UpdateWizardFiles;
  end;
end;

procedure TWizardForm.AppFilesListBoxDropFile(Sender: TDropListBox;
  const FileName: String);
begin
  if DirExists(FileName) then
    AddWizardFile(AddBackslash(FileName) + '*', True, True)
  else
    AddWizardFile(FileName, False, False);
  UpdateWizardFiles;
  UpdateWizardFilesButtons;
end;

procedure TWizardForm.AppFilesEditButtonClick(Sender: TObject);
var
  WizardFileForm: TWizardFileForm;
  Index: Integer;
begin
  WizardFileForm := TWizardFileForm.Create(Application);
  try
    Index := AppFilesListBox.ItemIndex;
    WizardFileForm.AllowAppDestRootDir := not NotCreateAppDirCheck.Checked;
    WizardFileForm.WizardFile := FWizardFiles[Index];
    if WizardFileForm.ShowModal = mrOK then begin
      UpdateWizardFiles;
      AppFilesListBox.ItemIndex := Index;
      AppFilesListBox.TopIndex := Index;
      UpdateWizardFilesButtons;
    end;
  finally
    WizardFileForm.Free;
  end;
end;

procedure TWizardForm.AppFilesRemoveButtonClick(Sender: TObject);
var
  I: Integer;
begin
  I := AppFilesListBox.ItemIndex;
  Dispose(FWizardFiles[I]);
  FWizardFiles.Delete(I);
  UpdateWizardFiles;
  UpdateWizardFilesButtons;
end;

procedure TWizardForm.NotDisableProgramGroupPageCheckClick(
  Sender: TObject);
begin
  AllowNoIconsCheck.Enabled := NotDisableProgramGroupPageCheck.Checked;
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

{ --- }

procedure TWizardForm.GenerateScript;
var
  Script, ISPP, Setup, Languages, Tasks, Files, INI, Icons, Run, UninstallDelete: String;
  WizardFile: PWizardFile;
  I: Integer;
  AppExeName, AppAmpEscapedName, LanguageName, LanguageMessagesFile: String;
begin
  Script := '';

  AppExeName := PathExtractName(AppExeEdit.Text);
  AppAmpEscapedName := EscapeAmpersands(AppNameEdit.Text);

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
  end else
    ISPP := '';

  Setup := '[Setup]' + SNewLine;
  Languages := '[Languages]' + SNewLine;
  Tasks := '[Tasks]' + SNewLine;
  Files := '[Files]' + SNewLine;
  INI := '[INI]' + SNewLine;
  Icons := '[Icons]' + SNewLine;
  Run := '[Run]' + SNewLine;
  UninstallDelete := '[UninstallDelete]' + SNewLine;

  if not EmptyCheck.Checked then begin
    Setup := Setup + (
      '; NOTE: The value of AppId uniquely identifies this application.' + SNewLine +
      '; Do not use the same AppId value in installers for other applications.' + SNewLine +
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
      Files := Files + 'Source: "' + AppExeEdit.Text + '"; DestDir: "{app}"; Flags: ignoreversion' + SNewLine;
      if AppExeRunCheck.Checked then begin
        if CompareText(PathExtractExt(AppExeEdit.Text), '.exe') = 0 then
          Run := Run + 'Filename: "{app}\' + AppExeName + '"; Description: "{cm:LaunchProgram,' + AppAmpEscapedName + '}"; Flags: nowait postinstall skipifsilent' + SNewLine
        else
          Run := Run + 'Filename: "{app}\' + AppExeName + '"; Description: "{cm:LaunchProgram,' + AppAmpEscapedName + '}"; Flags: shellexec postinstall skipifsilent' + SNewLine;
      end;
    end;

    for I := 0 to FWizardFiles.Count-1 do begin
      WizardFile := FWizardFiles[I];
      Files := Files + 'Source: "' + WizardFile.Source + '"; DestDir: "' + RemoveBackslashUnlessRoot(AddBackslash(WizardFile.DestRootDir) + WizardFile.DestSubDir) + '"; Flags: ignoreversion';
      if WizardFile.RecurseSubDirs then
        Files := Files + ' recursesubdirs';
      if WizardFile.CreateAllSubDirs then
        Files := Files + ' createallsubdirs';
      Files := Files + SNewLine;
    end;

    { AppGroup }
    if not NotCreateAppDirCheck.Checked then begin
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
      if DesktopIconCheck.Enabled and DesktopIconCheck.Checked then begin
        Tasks := Tasks + 'Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked' + SNewLine;
        Icons := Icons + 'Name: "{commondesktop}\' + AppNameEdit.Text + '"; Filename: "{app}\' + AppExeName + '"; Tasks: desktopicon' + SNewLine;
      end;
      if QuickLaunchIconCheck.Enabled and QuickLaunchIconCheck.Checked then begin
        Tasks := Tasks + 'Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 0,6.1' + SNewLine;
        Icons := Icons + 'Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\' + AppNameEdit.Text + '"; Filename: "{app}\' + AppExeName + '"; Tasks: quicklaunchicon' + SNewLine;
      end;
    end;

    { AppDocs }
    if AppLicenseFileEdit.Text <> '' then
      Setup := Setup + 'LicenseFile=' + AppLicenseFileEdit.Text + SNewLine;
    if AppInfoBeforeFileEdit.Text <> '' then
      Setup := Setup + 'InfoBeforeFile=' + AppInfoBeforeFileEdit.Text + SNewLine;
    if AppInfoAfterFileEdit.Text <> '' then
      Setup := Setup + 'InfoAfterFile=' + AppInfoAfterFileEdit.Text + SNewLine;

    { Languages}
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
      if ISCryptInstalled and EncryptionCheck.Checked then
        Setup := Setup + 'Encryption=yes' + SNewLine;
    end;

    { Other }
    Setup := Setup + 'Compression=lzma' + SNewLine;
    Setup := Setup + 'SolidCompression=yes' + SNewLine;

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
    Script := Script + Setup + SNewLine;
    FResult := wrEmpty;
  end;

  FResultScript := FixLabel(SWizardScriptHeader) + SNewLine2 + Script;
end;

{ --- }

end.
