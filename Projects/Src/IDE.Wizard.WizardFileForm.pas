unit IDE.Wizard.WizardFileForm;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE Script Wizard File form
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UIStateForm, StdCtrls, ExtCtrls, NewStaticText;

type
  TWizardFileOption = (foDownload, foExtractArchive, foRecurseSubDirs,  foCreateAllSubDirs);
  TWizardFileOptions = set of TWizardFileOption;

  PWizardFile = ^TWizardFile;
  TWizardFile = record
    Source: String;
    Options: TWizardFileOptions;
    DestRootDir: String;
    DestRootDirIsConstant: Boolean;
    DestSubDir: String;
    DestName: String;
    ExternalSize: Int64;
  end;

  TWizardFileForm = class(TUIStateForm)
    OKButton: TButton;
    CancelButton: TButton;
    GroupBox2: TGroupBox;
    DestRootDirComboBox: TComboBox;
    DestRootDirEdit: TEdit;
    DestRootDirLabel: TNewStaticText;
    DestSubDirEdit: TEdit;
    DestSubDirLabel: TNewStaticText;
    RequiredLabel1: TNewStaticText;
    RequiredLabel2: TNewStaticText;
    GroupBox1: TGroupBox;
    SourceLabel: TNewStaticText;
    SourceEdit: TEdit;
    RecurseSubDirsCheck: TCheckBox;
    CreateAllSubDirsCheck: TCheckBox;
    ExtractArchiveCheck: TCheckBox;
    DestNameLabel: TNewStaticText;
    DestNameEdit: TEdit;
    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DestRootDirComboBoxChange(Sender: TObject);
    procedure CheckClick(Sender: TObject);
  private
    FAllowAppDestRootDir: Boolean;
    FWizardFile: PWizardFile;
    procedure SetWizardFile(WizardFile: PWizardFile);
    procedure UpdateUI;
  public
    property AllowAppDestRootDir: Boolean write FAllowAppDestRootDir;
    property WizardFile: PWizardFile write SetWizardFile;
  end;

implementation

uses
  IDE.Messages, Shared.CommonFunc.Vcl, Shared.CommonFunc, IDE.HelperFunc;

{$R *.DFM}

type
  TConstant = record
    Constant, Description: String;
  end;

const
  DestRootDirs: array[0..6] of TConstant =
  (
    ( Constant: '{app}'; Description: 'Application directory'),
    ( Constant: '{autopf}'; Description: 'Program Files directory'),
    ( Constant: '{autocf}'; Description: 'Common Files directory'),
    ( Constant: '{win}'; Description: 'Windows directory'),
    ( Constant: '{sys}'; Description: 'Windows system directory'),
    ( Constant: '{src}'; Description: 'Setup source directory'),
    ( Constant: '{sd}'; Description: 'System drive root directory')
  );

procedure MakeBold(const Ctl: TNewStaticText);
begin
  Ctl.Font.Style := [fsBold];
end;

procedure TWizardFileForm.SetWizardFile(WizardFile: PWizardFile);
var
  I: Integer;
begin
  FWizardFile := WizardFile;

  if foDownload in WizardFile.Options then begin
    SourceLabel.Caption := '&Source URL:';
    SourceEdit.Text := Format('%s (~%.0f MB)', [WizardFile.Source, WizardFile.ExternalSize/(1024*1024)]);
    MakeBold(DestNameLabel);
  end else begin
    SourceEdit.Text := WizardFile.Source;
    if NewFileExists(WizardFile.Source) then
      RecurseSubDirsCheck.Enabled := False;
    ExtractArchiveCheck.Visible := False;
    if IsWildcard(WizardFile.Source) then begin
      DestNameLabel.Enabled := False;
      DestNameEdit.Color := clBtnFace;
      DestNameEdit.ReadOnly := True;
    end;
  end;

  ExtractArchiveCheck.Checked := foExtractArchive in WizardFile.Options;
  RecurseSubDirsCheck.Checked := foRecurseSubDirs in WizardFile.Options;
  CreateAllSubDirsCheck.Checked := foCreateAllSubDirs in WizardFile.Options;
  if WizardFile.DestRootDirIsConstant then begin
    for I := Low(DestRootDirs) to High(DestRootDirs) do begin
      if DestRootDirs[I].Constant = WizardFile.DestRootDir then begin
        DestRootDirComboBox.ItemIndex := I;
        Break;
      end;
    end;
  end else begin
    DestRootDirComboBox.ItemIndex := DestRootDirComboBox.Items.Count-1;
    DestRootDirEdit.Text := WizardFile.DestRootDir;
  end;
  DestSubDirEdit.Text := WizardFile.DestSubDir;
  DestNameEdit.Text := WizardFile.DestName;

  UpdateUI;
end;

{ --- }

procedure TWizardFileForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  InitFormFont(Self);
  InitFormTheme(Self);

  MakeBold(SourceLabel);
  MakeBold(DestRootDirLabel);
  MakeBold(RequiredLabel1);
  RequiredLabel2.Left := RequiredLabel1.Left + RequiredLabel1.Width;

  for I := Low(DestRootDirs) to High(DestRootDirs) do
    DestRootDirComboBox.Items.Add(DestRootDirs[I].Description);
  DestRootDirComboBox.Items.Add('(Custom)');
  DestRootDirComboBox.ItemIndex := 0;
end;

{ --- }

procedure TWizardFileForm.UpdateUI;
begin
  if foDownload in FWizardFile.Options then
    RecurseSubDirsCheck.Enabled := ExtractArchiveCheck.Checked;
  CreateAllSubDirsCheck.Enabled := RecurseSubDirsCheck.Enabled and RecurseSubDirsCheck.Checked;

  if DestRootDirComboBox.ItemIndex = DestRootDirComboBox.Items.Count-1 then begin
    DestRootDirEdit.Enabled := True;
    DestRootDirEdit.Color := clWindow;
  end else begin
    DestRootDirEdit.Enabled := False;
    DestRootDirEdit.Color := clBtnFace;
  end;
end;

{ --- }

procedure TWizardFileForm.CheckClick(Sender: TObject);
begin
  if (Sender = ExtractArchiveCheck) and ExtractArchiveCheck.Checked then begin
    RecurseSubDirsCheck.Checked := True;
    CreateAllSubDirsCheck.Checked := True;
  end;
  UpdateUI;
end;

procedure TWizardFileForm.DestRootDirComboBoxChange(Sender: TObject);
begin
  UpdateUI;
  if DestRootDirEdit.Enabled then
    ActiveControl := DestRootDirEdit;
end;

procedure TWizardFileForm.OKButtonClick(Sender: TObject);
var
  DestRootDirIndex: Integer;
begin
  ModalResult := mrNone;

  DestRootDirIndex := DestRootDirComboBox.ItemIndex;

  if (DestRootDirIndex = DestRootDirComboBox.Items.Count-1) and (DestRootDirEdit.Text = '') then begin
    MsgBox(SWizardFileDestRootDirError, '',  mbError, MB_OK);
    ActiveControl := DestRootDirEdit;
  end else if (DestRootDirs[DestRootDirIndex].Constant = '{app}') and not FAllowAppDestRootDir then begin
    MsgBox(SWizardFileAppDestRootDirError, '',  mbError, MB_OK);
    ActiveControl := DestRootDirComboBox;
  end else
    ModalResult := mrOk;

  if ModalResult = mrOk then begin
    FWizardFile.Options := FWizardFile.Options * [foDownload];
    if ExtractArchiveCheck.Checked then
      Include(FWizardFile.Options, foExtractArchive);
    if RecurseSubDirsCheck.Checked then
      Include(FWizardFile.Options, foRecurseSubDirs);
    if CreateAllSubDirsCheck.Checked then
      Include(FWizardFile.Options, foCreateAllSubDirs);
    if DestRootDirIndex = DestRootDirComboBox.Items.Count-1 then begin
      FWizardFile.DestRootDir := DestRootDirEdit.Text;
      FWizardFile.DestRootDirIsConstant := False;
    end else begin
      FWizardFile.DestRootDir := DestRootDirs[DestRootDirIndex].Constant;
      FWizardFile.DestRootDirIsConstant := True;
    end;
    FWizardFile.DestSubDir := DestSubDirEdit.Text;
    FWizardFile.DestName := DestNameEdit.Text;
  end;
end;

end.
