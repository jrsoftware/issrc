unit CompRegistryDesigner;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Registry Designer form

  Originally contributed by leserg73
}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls,
  CompWizardRegistryHelper, NewStaticText;

type
  TRegistryDesignerForm = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    InsertButton: TButton;
    CancelButton: TButton;
    AppRegistryFileLabel: TNewStaticText;
    AppRegistryFileEdit: TEdit;
    AppRegistryFileButton: TButton;
    st_Settings: TNewStaticText;
    AppRegistryUninsDeleteKeyCheck: TCheckBox;
    AppRegistryUninsDeleteKeyCheckIfEmpty: TCheckBox;
    AppRegistryUninsDeleteValueCheck: TCheckBox;
    AppRegistryMinVerCheck: TCheckBox;
    AppRegistryMinVerEdit: TEdit;
    PriviligesRequiredLabel: TNewStaticText;
    procedure InsertButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FRegistryHelper: TWizardFormRegistryHelper;
    procedure SetPriviligesRequired(const Value: TPriviligesRequired);
    function GetText: String;
  public
    property PriviligesRequired: TPriviligesRequired write SetPriviligesRequired;
    property Text: string read GetText;
  end;

implementation

{$R *.dfm}

procedure TRegistryDesignerForm.SetPriviligesRequired(
  const Value: TPriviligesRequired);
begin
  if Value = prAdmin then
    PriviligesRequiredLabel.Caption := 'Script has PriviligesRequired=admin'
  else if Value = prLowest then
    PriviligesRequiredLabel.Caption := 'Script has PriviligesRequired=lowest'
  else
    PriviligesRequiredLabel.Caption := 'Script has PrivilegesRequiredOverridesAllowed set';
end;

procedure TRegistryDesignerForm.FormCreate(Sender: TObject);
begin
  FRegistryHelper := TWizardFormRegistryHelper.Create(Self, AppRegistryFileEdit,
    AppRegistryFileButton, AppRegistryUninsDeleteKeyCheck,
    AppRegistryUninsDeleteKeyCheckIfEmpty, AppRegistryUninsDeleteValueCheck,
    AppRegistryMinVerCheck, AppRegistryMinVerEdit);
end;

procedure TRegistryDesignerForm.FormDestroy(Sender: TObject);
begin
  FRegistryHelper.Free;
end;

function TRegistryDesignerForm.GetText: String;
begin
  Result := '';
  FRegistryHelper.AddScript(Result);
end;

procedure TRegistryDesignerForm.InsertButtonClick(Sender: TObject);
begin
  if not FileExists(AppRegistryFileEdit.Text) then
    ModalResult := mrCancel;
end;

end.
