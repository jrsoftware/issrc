unit IDE.FilesDesignerForm;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Files Designer form
}

interface

uses
  Classes, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  UIStateForm, NewStaticText, DropListBox, IDE.Wizard.WizardFormFilesHelper;

type
  TFilesDesignerForm = class(TUIStateForm)
    Panel1: TPanel;
    InsertButton: TButton;
    CancelButton: TButton;
    AppFilesEditButton: TButton;
    AppFilesRemoveButton: TButton;
    AppFilesAddDirButton: TButton;
    AppFilesAddButton: TButton;
    AppFilesListBox: TDropListBox;
    AppFilesLabel: TNewStaticText;
    NotCreateAppDirCheck: TCheckBox;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InsertButtonClick(Sender: TObject);
  private
    FFilesHelper: TWizardFormFilesHelper;
    function GetText: String;
    procedure SetCreateAppDir(const Value: Boolean);
  public
    property CreateAppDir: Boolean write SetCreateAppDir;
    property Text: string read GetText;
  end;

implementation

{$R *.dfm}

uses
  IDE.HelperFunc;

procedure TFilesDesignerForm.FormCreate(Sender: TObject);
begin
  InitFormFont(Self);

  FFilesHelper := TWizardFormFilesHelper.Create(Self,
    NotCreateAppDirCheck, AppFilesListBox, AppFilesAddButton, AppFilesAddDirButton,
    AppFilesEditButton, AppFilesRemoveButton);
end;

procedure TFilesDesignerForm.FormDestroy(Sender: TObject);
begin
  FFilesHelper.Free;
end;

procedure TFilesDesignerForm.SetCreateAppDir(const Value: Boolean);
begin
  NotCreateAppDirCheck.Checked := not Value;
end;

function TFilesDesignerForm.GetText: String;
begin
  Result := '';
  FFilesHelper.AddScript(Result);
end;

procedure TFilesDesignerForm.InsertButtonClick(Sender: TObject);
begin
  if FFilesHelper.FilesCount = 0 then
    ModalResult := mrCancel;
end;

end.
