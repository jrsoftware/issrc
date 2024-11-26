unit IDE.Wizard.WizardFileForm;

{
  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE Script Wizard File form
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UIStateForm, StdCtrls, ExtCtrls, NewStaticText;

type

  PWizardFile = ^TWizardFile;
  TWizardFile = record
    Source: String;
    RecurseSubDirs: Boolean;
    CreateAllSubDirs: Boolean;
    DestRootDir: String;
    DestRootDirIsConstant: Boolean;
    DestSubDir: String;
  end;

  TWizardFileForm = class(TUIStateForm)
    OKButton: TButton;
    CancelButton: TButton;
    GroupBox2: TGroupBox;
    DestRootDirComboBox: TComboBox;
    DestRootDirEdit: TEdit;
    DestRootDirLabel: TNewStaticText;
    DestSubDirEdit: TEdit;
    SubDirLabel: TNewStaticText;
    RequiredLabel1: TNewStaticText;
    RequiredLabel2: TNewStaticText;
    GroupBox1: TGroupBox;
    SourceLabel: TNewStaticText;
    SourceEdit: TEdit;
    RecurseSubDirsCheck: TCheckBox;
    CreateAllSubDirsCheck: TCheckBox;
    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DestRootDirComboBoxChange(Sender: TObject);
    procedure RecurseSubDirsCheckClick(Sender: TObject);
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

procedure TWizardFileForm.SetWizardFile(WizardFile: PWizardFile);
var
  I: Integer;
begin
  FWizardFile := WizardFile;

  SourceEdit.Text := WizardFile.Source;
  RecurseSubDirsCheck.Checked := WizardFile.RecurseSubDirs;
  CreateAllSubDirsCheck.Checked := WizardFile.CreateAllSubDirs;
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

  UpdateUI;
end;

{ --- }

procedure TWizardFileForm.FormCreate(Sender: TObject);

  procedure MakeBold(const Ctl: TNewStaticText);
  begin
    Ctl.Font.Style := [fsBold];
  end;

var
  I: Integer;
begin
  InitFormFont(Self);

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
  CreateAllSubDirsCheck.Enabled := RecurseSubDirsCheck.Checked;

  if DestRootDirComboBox.ItemIndex = DestRootDirComboBox.Items.Count-1 then begin
    DestRootDirEdit.Enabled := True;
    DestRootDirEdit.Color := clWindow;
  end else begin
    DestRootDirEdit.Enabled := False;
    DestRootDirEdit.Color := clBtnFace;
  end;
end;

{ --- }

procedure TWizardFileForm.RecurseSubDirsCheckClick(Sender: TObject);
begin
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
    FWizardFile.RecurseSubDirs := RecurseSubDirsCheck.Checked;
    FWizardFile.CreateAllSubDirs := CreateAllSubDirsCheck.Checked;
    if DestRootDirIndex = DestRootDirComboBox.Items.Count-1 then begin
      FWizardFile.DestRootDir := DestRootDirEdit.Text;
      FWizardFile.DestRootDirIsConstant := False;
    end else begin
      FWizardFile.DestRootDir := DestRootDirs[DestRootDirIndex].Constant;
      FWizardFile.DestRootDirIsConstant := True;
    end;
    FWizardFile.DestSubDir := DestSubDirEdit.Text;
  end;
end;

end.
