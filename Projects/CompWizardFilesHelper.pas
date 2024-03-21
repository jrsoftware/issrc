unit CompWizardFilesHelper;

interface

uses
  Windows, Classes, StdCtrls,
  DropListBox;

type
  TWizardFormFilesHelper = class
    private
      FWizardFiles: TList;
      FHandle: HWND;
      FNotCreateAppDirCheck: TCheckBox;
      FAppFilesListBox: TDropListBox;
      procedure AddWizardFile(const Source: String; const RecurseSubDirs, CreateAllSubDirs: Boolean);
      procedure UpdateWizardFiles;
      procedure AddButtonClick(Sender: TObject);
    public
      property WizardFiles: TList read FWizardFiles;
      constructor Create(const Handle: HWND; const WizardFiles: TList;
        const NotCreateAppDirCheck: TCheckBox; const AppFilesAddButton: TButton;
        const AppFilesListBox: TDropListBox);
      destructor Destroy;
  end;

implementation

uses
  CmnFunc, BrowseFunc,
  CompMsgs, CompWizardFile;

constructor TWizardFormFilesHelper.Create(const Handle: HWND;
  const WizardFiles: TList; const NotCreateAppDirCheck: TCheckBox;
  const AppFilesAddButton: TButton; const AppFilesListBox: TDropListBox);
begin
  inherited Create;

  FWizardFiles := TList.Create;

  FHandle := Handle;
  FNotCreateAppDirCheck := NotCreateAppDirCheck;
  FAppFilesListBox := AppFilesListBox;

  AppFilesAddButton.OnClick := AddButtonClick;
end;

destructor TWizardFormFilesHelper.Destroy;
begin
  for var I := 0 to FWizardFiles.Count-1 do
    Dispose(FWizardFiles[i]);
  FWizardFiles.Free;
end;

procedure TWizardFormFilesHelper.AddWizardFile(const Source: String; const RecurseSubDirs, CreateAllSubDirs: Boolean);
var
  WizardFile: PWizardFile;
begin
  New(WizardFile);
  WizardFile.Source := Source;
  WizardFile.RecurseSubDirs := RecurseSubDirs;
  WizardFile.CreateAllSubDirs := CreateAllSubDirs;
  WizardFile.DestRootDirIsConstant := True;
  if not FNotCreateAppDirCheck.Checked then
    WizardFile.DestRootDir := '{app}'
  else
    WizardFile.DestRootDir := '{win}';
  WizardFile.DestSubDir := '';
  FWizardFiles.Add(WizardFile);
end;

procedure TWizardFormFilesHelper.UpdateWizardFiles;
var
  WizardFile: PWizardFile;
  I: Integer;
begin
  FAppFilesListBox.Items.BeginUpdate;
  FAppFilesListBox.Items.Clear;
  for I := 0 to FWizardFiles.Count-1 do begin
    WizardFile := FWizardFiles[i];
    FAppFilesListBox.Items.Add(WizardFile.Source);
  end;
  FAppFilesListBox.Items.EndUpdate;
  UpdateHorizontalExtent(FAppFilesListBox);
end;

procedure TWizardFormFilesHelper.AddButtonClick(Sender: TObject);
var
  FileList: TStringList;
  I: Integer;
begin
  FileList := TStringList.Create;
  try
    if NewGetOpenFileNameMulti('', FileList, '', SWizardAllFilesFilter, '', FHandle) then begin
      FileList.Sort;
      for I := 0 to FileList.Count-1 do
        AddWizardFile(FileList[I], False, False);
      UpdateWizardFiles;
    end;
  finally
    FileList.Free;
  end
end;

end.
