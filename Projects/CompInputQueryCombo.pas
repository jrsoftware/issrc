unit CompInputQueryCombo;

{
  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  InputQuery with a TComboBox instead of a TEdit
}

interface

uses
  Classes, Controls, StdCtrls, UIStateForm;

type
  TInputQueryCombo = class(TUIStateForm)
    OKButton: TButton;
    CancelButton: TButton;
    PromptLabel: TLabel;
    ValueComboBox: TComboBox;
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
  Windows, Messages, CompFunc, Forms;

{$R *.DFM}

function InputQueryCombo(const ACaption, APrompt: String; var AValue: String; const AValues: TStringList): Boolean;
begin
  with TInputQueryCombo.Create(Application) do try
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

procedure TInputQueryCombo.FormCreate(Sender: TObject);
begin
  InitFormFont(Self);
end;

function TInputQueryCombo.GetValue: String;
begin
  Result := ValueComboBox.Text;
end;

procedure TInputQueryCombo.SetPrompt(const APrompt: String);
begin
  PromptLabel.Caption := APrompt;
end;

procedure TInputQueryCombo.SetValue(const AValue: String);
begin
  ValueComboBox.Text := AValue;
end;

procedure TInputQueryCombo.SetValues(const AValues: TStringList);
begin
  ValueComboBox.Items := AValues;
end;

end.
