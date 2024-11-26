unit IDE.InputQueryComboForm;

{
  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  InputQuery with a TComboBox instead of a TEdit

  Unlike InputQuery it doesn't limit the value to 255 characters
}

interface

uses
  Classes, Controls, StdCtrls, UIStateForm;

type
  TInputQueryComboForm = class(TUIStateForm)
    OKButton: TButton;
    CancelButton: TButton;
    PromptLabel: TLabel;
    ValueControl: TComboBox;
    procedure FormCreate(Sender: TObject);
  private
    function GetValue: String;
    procedure SetPrompt(const APrompt: String);
    procedure SetValue(const AValue: String);
    procedure SetValues(const AValues: TStringList);
  public
    property Prompt: String write SetPrompt;
    property Value: String read GetValue write SetValue;
    property Values: TStringList write SetValues;
  end;

function InputQueryCombo(const ACaption, APrompt: String; var AValue: String; const AValues: TStringList): Boolean;

implementation

uses
  Windows, Messages, IDE.HelperFunc, Forms;

{$R *.DFM}

function InputQueryCombo(const ACaption, APrompt: String; var AValue: String; const AValues: TStringList): Boolean;
begin
  with TInputQueryComboForm.Create(Application) do try
    Caption := ACaption;
    Prompt := APrompt;
    Value := AValue;
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

procedure TInputQueryComboForm.FormCreate(Sender: TObject);
begin
  InitFormFont(Self);
end;

function TInputQueryComboForm.GetValue: String;
begin
  Result := ValueControl.Text;
end;

procedure TInputQueryComboForm.SetPrompt(const APrompt: String);
begin
  PromptLabel.Caption := APrompt;
  var MoveX := PromptLabel.Left + PromptLabel.Width + CancelButton.Left - (OkButton.Left + OkButton.Width) - ValueControl.Left;
  ValueControl.Left := ValueControl.Left + MoveX;
  ValueControl.Width := ValueControl.Width - MoveX;
end;

procedure TInputQueryComboForm.SetValue(const AValue: String);
begin
  ValueControl.Text := AValue;
end;

procedure TInputQueryComboForm.SetValues(const AValues: TStringList);
begin
  ValueControl.Items := AValues;
end;

end.
