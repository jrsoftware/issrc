unit IDE.Wizard.WizardFormFilesHelper;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Helper to avoid duplicate code between WizardForm and FilesDesignerForm
}

interface

uses
  Windows, Classes, Forms, StdCtrls,
  DropListBox, IDE.Wizard.WizardFileForm;

type
  TWizardFormFilesHelper = class
    private
      FWizardFiles: TList;
      FForm: TForm;
      FNotCreateAppDirCheck: TCheckBox;
      FFilesListBox: TDropListBox;
      FEditButton: TButton;
      FRemoveButton: TButton;
      function AddWizardFile(const Source: String; const Options: TWizardFileOptions): PWizardFile;
      function GetWizardFilesCount: Integer;
      procedure UpdateWizardFiles;
      procedure UpdateWizardFilesButtons;
      procedure FilesListBoxClick(Sender: TObject);
      procedure FilesListBoxDblClick(Sender: TObject);
      procedure FilesListBoxDropFile(Sender: TDropListBox; const FileName: String);
      procedure AddButtonClick(Sender: TObject);
      procedure AddDirButtonClick(Sender: TObject);
      procedure AddDownloadButtonClick(Sender: TObject);
      procedure EditButtonClick(Sender: TObject);
      procedure RemoveButtonClick(Sender: TObject);
    public
      constructor Create(const Form: TForm;
        const NotCreateAppDirCheck: TCheckBox; const FilesListBox: TDropListBox;
        const AddButton, AddDirButton, AddDownloadButton, EditButton, RemoveButton: TButton);
      destructor Destroy; override;
      procedure AddScript(var Files: String); overload;
      procedure AddScript(var Files: String; out HasExtractArchive: Boolean); overload;
      property FilesCount: Integer read GetWizardFilesCount;
  end;

implementation

uses
  SysUtils, UITypes, Dialogs,
  Shared.CommonFunc.Vcl, Shared.CommonFunc, BrowseFunc, PathFunc,
  IDE.Messages;

constructor TWizardFormFilesHelper.Create(const Form: TForm;
  const NotCreateAppDirCheck: TCheckBox; const FilesListBox: TDropListBox;
  const AddButton, AddDirButton, AddDownloadButton, EditButton, RemoveButton: TButton);
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
  AddDownloadButton.OnClick := AddDownloadButtonClick;
  EditButton.OnClick := EditButtonClick;
  RemoveButton.OnClick := RemoveButtonClick;

  UpdateWizardFilesButtons;
end;

destructor TWizardFormFilesHelper.Destroy;
begin
  for var I := 0 to FWizardFiles.Count-1 do
    Dispose(PWizardFile(FWizardFiles[i]));
  FWizardFiles.Free;
end;

function TWizardFormFilesHelper.AddWizardFile(const Source: String; const Options: TWizardFileOptions): PWizardFile;
var
  WizardFile: PWizardFile;
begin
  New(WizardFile);
  WizardFile.Source := Source;
  WizardFile.Options := Options;
  WizardFile.DestRootDirIsConstant := True;
  if not FNotCreateAppDirCheck.Checked then
    WizardFile.DestRootDir := '{app}'
  else
    WizardFile.DestRootDir := '{win}';
  WizardFile.DestSubDir := '';
  WizardFile.DestName := '';
  WizardFile.ExternalSize := 0;
  FWizardFiles.Add(WizardFile);
  Result := WizardFile;
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
    AddWizardFile(AddBackslash(FileName) + '*', [foRecurseSubDirs, foCreateAllSubDirs])
  else
    AddWizardFile(FileName, []);
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
        AddWizardFile(FileList[I], []);
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
    var Options: TWizardFileOptions;
    if Recurse then
      Options := [foRecurseSubDirs, foCreateAllSubDirs]
    else
      Options := [];
    AddWizardFile(AddBackslash(Path) + '*', Options);
    UpdateWizardFiles;
  end;
end;

procedure TWizardFormFilesHelper.AddDownloadButtonClick(Sender: TObject);
const
  DestNamePrompts: array [Boolean] of string = (SWizardAppFilesDownloadDestNamePrompt, SWizardAppFilesDownloadArchiveDestNamePrompt);
begin
  var Source := 'https://www.example.com/MyProg.7z';
  repeat
    if not InputQuery(FForm.Caption, SWizardAppFilesDownloadSourcePrompt, Source)  then
      Exit;
  until Source <> '';
  const ExtractArchive = MsgBox(SWizardAppFilesDownloadExtractArchiveMessage, '', mbConfirmation, MB_YESNO or MB_DEFBUTTON2) = IDYES;
  var DestName := 'MyProg.7z';
  repeat
    if not InputQuery(FForm.Caption, DestNamePrompts[ExtractArchive], DestName)  then
      Exit;
  until DestName <> '';
  var ExternalSizeAsString := '';
  var ExternalSize: Extended;
  repeat
    if not InputQuery(FForm.Caption, SWizardAppFilesDownloadExternalSizePrompt, ExternalSizeAsString)  then
      Exit;
  until TryStrToFloat(ExternalSizeAsString, ExternalSize);

  var Options: TWizardFileOptions := [foDownload];
  if ExtractArchive then
    Options := Options + [foExtractArchive, foRecurseSubDirs, foCreateAllSubDirs];
  const WizardFile = AddWizardFile(Source, Options);
  WizardFile.DestName := DestName;
  WizardFile.ExternalSize := Round(ExternalSize*1024*1024);
  UpdateWizardFiles;
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
  Dispose(PWizardFile(FWizardFiles[I]));
  FWizardFiles.Delete(I);
  UpdateWizardFiles;
  UpdateWizardFilesButtons;
end;

procedure TWizardFormFilesHelper.AddScript(var Files: String; out HasExtractArchive: Boolean);
var
  WizardFile: PWizardFile;
  I: Integer;
begin
  var AddedVerificationNote := False;

  for I := 0 to FWizardFiles.Count-1 do begin
    WizardFile := FWizardFiles[I];

    if (foDownload in WizardFile.Options) and not AddedVerificationNote then begin
      Files := Files + '; NOTE: Use the "issigverify" flag or the "Hash" parameter to verify downloads' + SNewLine;
      AddedVerificationNote := True;
    end;

    if foExtractArchive in WizardFile.Options then
      HasExtractArchive := True;

    Files := Files + 'Source: "' + WizardFile.Source + '"; DestDir: "' + RemoveBackslashUnlessRoot(AddBackslash(WizardFile.DestRootDir) + WizardFile.DestSubDir) + '"';
    if WizardFile.DestName <> '' then
      Files := Files + '; DestName: "' + WizardFile.DestName + '"';
    if WizardFile.ExternalSize <> 0 then
      Files := Files + '; ExternalSize: "' + WizardFile.ExternalSize.ToString + '"';
    Files := Files + '; Flags: ignoreversion';
    if WizardFile.Options * [foDownload, foExtractArchive] <> [] then
      Files := Files + ' external';
    if foDownload in WizardFile.Options then
      Files := Files + ' download';
    if foExtractArchive in WizardFile.Options then
      Files := Files + ' extractarchive';
    if foRecurseSubDirs in WizardFile.Options then
      Files := Files + ' recursesubdirs';
    if foCreateAllSubDirs in WizardFile.Options then
      Files := Files + ' createallsubdirs';
    Files := Files + SNewLine;
  end;
end;

procedure TWizardFormFilesHelper.AddScript(var Files: String);
begin
  var HasExtractArchive: Boolean;
  AddScript(Files, HasExtractArchive);
end;

end.
