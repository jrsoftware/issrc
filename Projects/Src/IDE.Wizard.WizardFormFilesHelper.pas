unit IDE.Wizard.WizardFormFilesHelper;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Helper to avoid duplicate code between WizardForm and FilesDesignerForm
}

interface

uses
  Windows, Classes, Forms, StdCtrls,
  DropListBox;

type
  TWizardFormFilesHelper = class
    private
      FWizardFiles: TList;
      FForm: TForm;
      FNotCreateAppDirCheck: TCheckBox;
      FFilesListBox: TDropListBox;
      FEditButton: TButton;
      FRemoveButton: TButton;
      procedure AddWizardFile(const Source: String; const RecurseSubDirs, CreateAllSubDirs: Boolean);
      function GetWizardFilesCount: Integer;
      procedure UpdateWizardFiles;
      procedure UpdateWizardFilesButtons;
      procedure FilesListBoxClick(Sender: TObject);
      procedure FilesListBoxDblClick(Sender: TObject);
      procedure FilesListBoxDropFile(Sender: TDropListBox; const FileName: String);
      procedure AddButtonClick(Sender: TObject);
      procedure AddDirButtonClick(Sender: TObject);
      procedure EditButtonClick(Sender: TObject);
      procedure RemoveButtonClick(Sender: TObject);
    public
      constructor Create(const Form: TForm;
        const NotCreateAppDirCheck: TCheckBox; const FilesListBox: TDropListBox;
        const AddButton, AddDirButton, EditButton, RemoveButton: TButton);
      destructor Destroy; override;
      procedure AddScript(var Files: String);
      property FilesCount: Integer read GetWizardFilesCount;
  end;

implementation

uses
  SysUtils, UITypes,
  Shared.CommonFunc.Vcl, Shared.CommonFunc, BrowseFunc, PathFunc,
  IDE.Messages, IDE.Wizard.WizardFileForm;

constructor TWizardFormFilesHelper.Create(const Form: TForm;
  const NotCreateAppDirCheck: TCheckBox; const FilesListBox: TDropListBox;
  const AddButton, AddDirButton, EditButton, RemoveButton: TButton);
begin
  inherited Create;

  FWizardFiles := TList.Create;

  FForm := Form;
  FNotCreateAppDirCheck := NotCreateAppDirCheck;
  FFilesListBox := FilesListBox;
  FEditButton := EditButton;
  FRemoveButton := RemoveButton;

  FilesListBox.OnClick := FilesListBoxClick;
  FilesListBox.OnDblClick := FilesListBoxDblClick;
  FilesListBox.OnDropFile :=   FilesListBoxDropFile;
  AddButton.OnClick := AddButtonClick;
  AddDirButton.OnClick := AddDirButtonClick;
  EditButton.OnClick := EditButtonClick;
  RemoveButton.OnClick := RemoveButtonClick;

  UpdateWizardFilesButtons;
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

function TWizardFormFilesHelper.GetWizardFilesCount: Integer;
begin
  Result := FWizardFiles.Count;
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

procedure TWizardFormFilesHelper.UpdateWizardFilesButtons;
var
  Enabled: Boolean;
begin
  Enabled := FFilesListBox.ItemIndex >= 0;
  FEditButton.Enabled := Enabled;
  FRemoveButton.Enabled := Enabled;
end;

procedure TWizardFormFilesHelper.FilesListBoxClick(Sender: TObject);
begin
  UpdateWizardFilesButtons;
end;

procedure TWizardFormFilesHelper.FilesListBoxDblClick(Sender: TObject);
begin
  if FEditButton.Enabled then
    FEditButton.Click;
end;

procedure TWizardFormFilesHelper.FilesListBoxDropFile(Sender: TDropListBox;
  const FileName: String);
begin
  if DirExists(FileName) then
    AddWizardFile(AddBackslash(FileName) + '*', True, True)
  else
    AddWizardFile(FileName, False, False);
  UpdateWizardFiles;
  UpdateWizardFilesButtons;
end;

procedure TWizardFormFilesHelper.AddButtonClick(Sender: TObject);
var
  FileList: TStringList;
  I: Integer;
begin
  FileList := TStringList.Create;
  try
    if NewGetOpenFileNameMulti('', FileList, '', SWizardAllFilesFilter, '', FForm.Handle) then begin
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
  if BrowseForFolder(SWizardAppFiles3, Path, FForm.Handle, False) then begin
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

procedure TWizardFormFilesHelper.EditButtonClick(Sender: TObject);
var
  WizardFileForm: TWizardFileForm;
  Index: Integer;
begin
  WizardFileForm := TWizardFileForm.Create(Application);
  try
    Index := FFilesListBox.ItemIndex;
    WizardFileForm.AllowAppDestRootDir := not FNotCreateAppDirCheck.Checked;
    WizardFileForm.WizardFile := FWizardFiles[Index];
    if WizardFileForm.ShowModal = mrOk then begin
      UpdateWizardFiles;
      FFilesListBox.ItemIndex := Index;
      FFilesListBox.TopIndex := Index;
      UpdateWizardFilesButtons;
    end;
  finally
    WizardFileForm.Free;
  end;
end;

procedure TWizardFormFilesHelper.RemoveButtonClick(Sender: TObject);
var
  I: Integer;
begin
  I := FFilesListBox.ItemIndex;
    Dispose(FWizardFiles[I]);
  FWizardFiles.Delete(I);
  UpdateWizardFiles;
  UpdateWizardFilesButtons;
end;

procedure TWizardFormFilesHelper.AddScript(var Files: String);
var
  WizardFile: PWizardFile;
  I: Integer;
begin
  for I := 0 to FWizardFiles.Count-1 do begin
    WizardFile := FWizardFiles[I];
    Files := Files + 'Source: "' + WizardFile.Source + '"; DestDir: "' + RemoveBackslashUnlessRoot(AddBackslash(WizardFile.DestRootDir) + WizardFile.DestSubDir) + '"; Flags: ignoreversion';
    if WizardFile.RecurseSubDirs then
      Files := Files + ' recursesubdirs';
    if WizardFile.CreateAllSubDirs then
      Files := Files + ' createallsubdirs';
    Files := Files + SNewLine;
  end;
end;

end.
