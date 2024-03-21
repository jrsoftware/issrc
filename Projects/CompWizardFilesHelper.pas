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
      FFilesListBox: TDropListBox;
      procedure AddWizardFile(const Source: String; const RecurseSubDirs, CreateAllSubDirs: Boolean);
      procedure UpdateWizardFiles;
      procedure AddButtonClick(Sender: TObject);
      procedure AddDirButtonClick(Sender: TObject);
    public
      property WizardFiles: TList read FWizardFiles;
      constructor Create(const Handle: HWND; const WizardFiles: TList;
        const NotCreateAppDirCheck: TCheckBox; const AddButton, AddDirButton: TButton;
        const FilesListBox: TDropListBox);
      destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  CmnFunc, BrowseFunc, PathFunc,
  CompMsgs, CompWizardFile;

constructor TWizardFormFilesHelper.Create(const Handle: HWND;
  const WizardFiles: TList; const NotCreateAppDirCheck: TCheckBox;
  const AddButton, AddDirButton: TButton; const FilesListBox: TDropListBox);
begin
  inherited Create;

  FWizardFiles := TList.Create;

  FHandle := Handle;
  FNotCreateAppDirCheck := NotCreateAppDirCheck;
  FFilesListBox :=FilesListBox;

  AddButton.OnClick := AddButtonClick;
  AddDirButton.OnClick := AddDirButtonClick;
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
  FFilesListBox.Items.BeginUpdate;
  FFilesListBox.Items.Clear;
  for I := 0 to FWizardFiles.Count-1 do begin
    WizardFile := FWizardFiles[i];
    FFilesListBox.Items.Add(WizardFile.Source);
  end;
  FFilesListBox.Items.EndUpdate;
  UpdateHorizontalExtent(FFilesListBox);
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

procedure TWizardFormFilesHelper.AddDirButtonClick(Sender: TObject);
var
  Path: String;
  Recurse: Boolean;
begin
  Path := '';
  if BrowseForFolder(SWizardAppFiles3, Path, FHandle, False) then begin
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

end.
