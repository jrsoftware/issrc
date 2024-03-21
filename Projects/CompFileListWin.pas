unit CompFileListWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs,ExtCtrls, StdCtrls,
  NewStaticText, DropListBox, CompWizard, CompWizardFilesHelper;

type
  TCFLWForm = class(TForm)
    Panel1: TPanel;
    CFLWButtonOK: TButton;
    CFLWButtonCancel: TButton;
    AppFilesEditButton: TButton;
    AppFilesRemoveButton: TButton;
    AppFilesAddDirButton: TButton;
    AppFilesAddButton: TButton;
    AppFilesListBox: TDropListBox;
    AppFilesLabel: TNewStaticText;
    NotCreateAppDirCheck: TCheckBox;
    procedure AppFilesEditButtonClick(Sender: TObject);
    procedure AppFilesRemoveButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AppFilesListBoxClick(Sender: TObject);
    procedure AppFilesListBoxDblClick(Sender: TObject);
    procedure AppFilesListBoxDropFile(Sender: TDropListBox;
      const FileName: string);
    procedure FormDestroy(Sender: TObject);
  private
    FWizardFiles: TList; //todo: remove?
    FFilesHelper: TWizardFormFilesHelper;
    function GetText: String;
    //todo remove!!!
    procedure AddWizardFile(const Source: String;
      const RecurseSubDirs, CreateAllSubDirs: Boolean);
    procedure UpdateWizardFilesButtons;
    procedure UpdateWizardFiles;
  public
    property Text: string read GetText;
  end;


var
  CFLWForm: TCFLWForm;

implementation

uses
  CompMsgs, BrowseFunc, CmnFunc, CmnFunc2, CompWizardFile, PathFunc;

{$R *.dfm}

procedure TCFLWForm.AddWizardFile(const Source: String;
  const RecurseSubDirs, CreateAllSubDirs: Boolean);
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

procedure TCFLWForm.UpdateWizardFiles;
var
  WizardFile: PWizardFile;
  I: Integer;
begin
  AppFilesListBox.Items.BeginUpdate;
  AppFilesListBox.Items.Clear;
  for I := 0 to FWizardFiles.Count - 1 do
  begin
    WizardFile := FWizardFiles[I];
    AppFilesListBox.Items.Add(WizardFile.Source);
  end;
  AppFilesListBox.Items.EndUpdate;
  UpdateHorizontalExtent(AppFilesListBox);
end;

procedure TCFLWForm.AppFilesEditButtonClick(Sender: TObject);
var
  WizardFileForm: TWizardFileForm;
  Index: Integer;
begin
  WizardFileForm := TWizardFileForm.Create(Application);
  try
    Index := AppFilesListBox.ItemIndex;
    WizardFileForm.AllowAppDestRootDir := not NotCreateAppDirCheck.Checked;
    WizardFileForm.WizardFile := FWizardFiles[Index];
    if WizardFileForm.ShowModal = mrOk then
    begin
      UpdateWizardFiles;
      AppFilesListBox.ItemIndex := Index;
      AppFilesListBox.TopIndex := Index;
      UpdateWizardFilesButtons;
    end;
  finally
    WizardFileForm.Free;
  end;

end;

procedure TCFLWForm.AppFilesListBoxClick(Sender: TObject);
begin
  UpdateWizardFilesButtons;
end;

procedure TCFLWForm.AppFilesListBoxDblClick(Sender: TObject);
begin
  if AppFilesEditButton.Enabled then
    AppFilesEditButton.Click;
end;

procedure TCFLWForm.AppFilesListBoxDropFile(Sender: TDropListBox;
  const FileName: string);
begin
  if DirExists(FileName) then
    AddWizardFile(AddBackslash(FileName) + '*', True, True)
  else
    AddWizardFile(FileName, False, False);
  UpdateWizardFiles;
  UpdateWizardFilesButtons;
end;

procedure TCFLWForm.AppFilesRemoveButtonClick(Sender: TObject);
var
  I: Integer;
begin
  I := AppFilesListBox.ItemIndex;
  Dispose(FWizardFiles[I]);
  FWizardFiles.Delete(I);
  UpdateWizardFiles;
  UpdateWizardFilesButtons;
end;

procedure TCFLWForm.FormCreate(Sender: TObject);
begin
  FFilesHelper := TWizardFormFilesHelper.Create(Handle, FWizardFiles,
    NotCreateAppDirCheck, AppFilesAddButton, AppFilesAddDirButton, AppFilesListBox);

  FWizardFiles := FFilesHelper.WizardFiles;
end;

procedure TCFLWForm.FormDestroy(Sender: TObject);
begin
  FFilesHelper.Free;
end;

procedure TCFLWForm.UpdateWizardFilesButtons;
var
  Enabled: Boolean;
begin
  Enabled := AppFilesListBox.ItemIndex >= 0;
  AppFilesEditButton.Enabled := Enabled;
  AppFilesRemoveButton.Enabled := Enabled;
end;

function TCFLWForm.GetText: String;
var
  Files: String;
  WizardFile: PWizardFile;
  I: Integer;
begin
  for I := 0 to FWizardFiles.Count - 1 do
    begin
      WizardFile := FWizardFiles[I];
      Files := Files + 'Source: "' + WizardFile.Source + '"; DestDir: "' +
        RemoveBackslashUnlessRoot(AddBackslash(WizardFile.DestRootDir) +
        WizardFile.DestSubDir) + '"; Flags: ignoreversion';
      if WizardFile.RecurseSubDirs then
        Files := Files + ' recursesubdirs';
      if WizardFile.CreateAllSubDirs then
        Files := Files + ' createallsubdirs';
      Files := Files + SNewLine;
    end;
    Files := Files +
        '; NOTE: Don''t use "Flags: ignoreversion" on any shared system files' +
        SNewLine2;
    Result := Files;
end;

end.
