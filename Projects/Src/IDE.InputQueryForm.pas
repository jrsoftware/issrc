unit IDE.InputQueryForm;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  InputQuery with either a TEdit or a TComboBox as the value control

  Unlike InputQuery it doesn't limit the value to 255 characters
}

interface

uses
  Classes, Controls, StdCtrls,
  IDE.IDEForm;

type
  TInputQueryForm = class(TIDEForm)
    OKButton: TButton;
    CancelButton: TButton;
    PromptLabel: TLabel;
    ValueComboBox: TComboBox;
    ValueEdit: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    FValueControl: TWinControl;
    procedure SelectValueControl(const AUseComboBox: Boolean);
    function GetValue: String;
    procedure SetPrompt(const APrompt: String);
    procedure SetValue(const AValue: String);
    procedure SetValues(const AValues: TStringList);
  public
    property Prompt: String write SetPrompt;
    property Value: String read GetValue write SetValue;
    property Values: TStringList write SetValues;
  end;

function InputQueryEdit(const ACaption, APrompt: String; var AValue: String): Boolean;
function InputQueryCombo(const ACaption, APrompt: String; var AValue: String; const AValues: TStringList): Boolean;

implementation

uses
  Forms;

{$R *.DFM}

function DoInputQuery(const ACaption, APrompt: String; var AValue: String;
  const AValues: TStringList): Boolean;
begin
  with TInputQueryForm.Create(Application) do try
    SelectValueControl(AValues <> nil);
    Caption := ACaption;
    Prompt := APrompt;
    Value := AValue;
    if AValues <> nil then
      Values := AValues;
    if ShowModal = mrOk then begin
      AValue := Value;
      Result := True;
    end else
      Result := False;
  finally
    Free;
  end;
end;

function InputQueryEdit(const ACaption, APrompt: String; var AValue: String): Boolean;
begin
  Result := DoInputQuery(ACaption, APrompt, AValue, nil);
end;

function InputQueryCombo(const ACaption, APrompt: String; var AValue: String;
  const AValues: TStringList): Boolean;
begin
  Result := DoInputQuery(ACaption, APrompt, AValue, AValues);
end;

procedure TInputQueryForm.FormCreate(Sender: TObject);
begin
  { Finish localization }
  SizeBottomButtons(OKButton, CancelButton);
end;

procedure TInputQueryForm.SelectValueControl(const AUseComboBox: Boolean);
begin
  if AUseComboBox then
    FValueControl := ValueComboBox
  else
    FValueControl := ValueEdit;
  ValueComboBox.Visible := AUseComboBox;
  ValueEdit.Visible := not AUseComboBox;
  PromptLabel.FocusControl := FValueControl;
end;

function TInputQueryForm.GetValue: String;
begin
  if FValueControl = ValueComboBox then
    Result := ValueComboBox.Text
  else
    Result := ValueEdit.Text;
end;

procedure TInputQueryForm.SetPrompt(const APrompt: String);
begin
  PromptLabel.Caption := APrompt;
  const MoveX = PromptLabel.Left + PromptLabel.Width + CancelButton.Left - (OkButton.Left + OkButton.Width) - FValueControl.Left;
  FValueControl.Left := FValueControl.Left + MoveX;
  FValueControl.Width := FValueControl.Width - MoveX;
end;

procedure TInputQueryForm.SetValue(const AValue: String);
begin
  if FValueControl = ValueComboBox then
    ValueComboBox.Text := AValue
  else begin
    ValueEdit.Text := AValue;
    ValueEdit.SelectAll;
  end;
end;

procedure TInputQueryForm.SetValues(const AValues: TStringList);
begin
  ValueComboBox.Items := AValues;
end;

end.
