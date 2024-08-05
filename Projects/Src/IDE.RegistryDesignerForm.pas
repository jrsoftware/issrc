unit IDE.RegistryDesignerForm;

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
  IDE.Wizard.WizardFormRegistryHelper, NewStaticText;

type
  TRegistryDesignerForm = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    InsertButton: TButton;
    CancelButton: TButton;
    AppRegistryFileLabel: TNewStaticText;
    AppRegistryFileEdit: TEdit;
    AppRegistryFileButton: TButton;
    AppRegistrySettingsLabel: TNewStaticText;
    AppRegistryUninsDeleteKeyCheck: TCheckBox;
    AppRegistryUninsDeleteKeyIfEmptyCheck: TCheckBox;
    AppRegistryUninsDeleteValueCheck: TCheckBox;
    AppRegistryMinVerCheck: TCheckBox;
    AppRegistryMinVerEdit: TEdit;
    PrivilegesRequiredLabel: TNewStaticText;
    AppRegistryMinVerDocImage: TImage;
    procedure InsertButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FRegistryHelper: TWizardFormRegistryHelper;
    procedure SetPrivilegesRequired(const Value: TPrivilegesRequired);
    function GetText: String;
  public
    property PrivilegesRequired: TPrivilegesRequired write SetPrivilegesRequired;
    property Text: string read GetText;
  end;

implementation

{$R *.dfm}

uses
  IDE.HelperFunc;

procedure TRegistryDesignerForm.SetPrivilegesRequired(
  const Value: TPrivilegesRequired);
begin
  if Value = prAdmin then
    PrivilegesRequiredLabel.Caption := 'Script has PrivilegesRequired=admin'
  else if Value = prLowest then
    PrivilegesRequiredLabel.Caption := 'Script has PrivilegesRequired=lowest'
  else
    PrivilegesRequiredLabel.Caption := 'Script has PrivilegesRequiredOverridesAllowed set';
  FRegistryHelper.PrivilegesRequired := Value;
end;

procedure TRegistryDesignerForm.FormCreate(Sender: TObject);
begin
  InitFormFont(Self);

  FRegistryHelper := TWizardFormRegistryHelper.Create(Self, AppRegistryFileEdit,
    AppRegistryFileButton, AppRegistryUninsDeleteKeyCheck,
    AppRegistryUninsDeleteKeyIfEmptyCheck, AppRegistryUninsDeleteValueCheck,
    AppRegistryMinVerCheck, AppRegistryMinVerEdit, AppRegistryMinVerDocImage);
end;

procedure TRegistryDesignerForm.FormDestroy(Sender: TObject);
begin
  FRegistryHelper.Free;
end;

function TRegistryDesignerForm.GetText: String;
begin
  Result := '';
  FRegistryHelper.AddScript(Result, True);
end;

procedure TRegistryDesignerForm.InsertButtonClick(Sender: TObject);
begin
  if not FileExists(AppRegistryFileEdit.Text) then
    ModalResult := mrCancel;
end;

end.
