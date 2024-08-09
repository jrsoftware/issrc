unit IDE.InputQueryMemoForm;

{
  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  InputQuery with a TMemo instead of a TEdit

  Unlike InputQuery it doesn't limit the value to 255 characters
}

interface

uses
  Classes, Controls, StdCtrls, UIStateForm;

type
  TInputQueryMemoForm = class(TUIStateForm)
    OKButton: TButton;
    CancelButton: TButton;
    PromptLabel: TLabel;
    ValueControl: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ValueControlKeyPress(Sender: TObject; var Key: Char);
    procedure ValueControlChange(Sender: TObject);
    procedure ValueControlKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FSingleLine: Boolean;
    function GetValue: String;
    procedure SetPrompt(const APrompt: String);
    procedure SetValue(const AValue: String);
  public
    property SingleLine: Boolean write FSingleLine;
    property Prompt: String write SetPrompt;
    property Value: String read GetValue write SetValue;
  end;

function InputQueryMemo(const ACaption, APrompt: String; var AValue: String;
  const ASingleLine: Boolean = False): Boolean;

implementation

uses
  Windows, Messages, IDE.HelperFunc, Forms;

{$R *.DFM}

function InputQueryMemo(const ACaption, APrompt: String; var AValue: String;
  const ASingleLine: Boolean): Boolean;
begin
  with TInputQueryMemoForm.Create(Application) do try
    Caption := ACaption;
    Prompt := APrompt;
    Value := AValue;
    SingleLine := ASingleLine;
    if ShowModal = mrOk then begin
      AValue := Value;
      Result := True;
    end else
      Result := False;
  finally
    Free;
  end;
end;

procedure TInputQueryMemoForm.FormCreate(Sender: TObject);
begin
  InitFormFont(Self);
end;

function TInputQueryMemoForm.GetValue: String;
begin
  Result := ValueControl.Text;
end;

procedure TInputQueryMemoForm.SetPrompt(const APrompt: String);
begin
  PromptLabel.Caption := APrompt;
  var MoveX := PromptLabel.Left + PromptLabel.Width + CancelButton.Left - (OkButton.Left + OkButton.Width) - ValueControl.Left;
  ValueControl.Left := ValueControl.Left + MoveX;
  ValueControl.Width := ValueControl.Width - MoveX;
end;

procedure TInputQueryMemoForm.SetValue(const AValue: String);
begin
  ValueControl.Text := AValue;
  ValueControl.SelectAll;
end;

procedure TInputQueryMemoForm.ValueControlChange(Sender: TObject);
begin
  { We don't allow Enter to be added but it could still be pasted so must check.
    Checking with Lines.Count doesn't work, for example if one pastes 3 lines and
    then removes two it still returns 3 for Lines.Count. }
  if FSingleLine then begin
    var Text := ValueControl.Text;
    OKButton.Enabled := Pos(#10, Text) = 0;
  end else
    OKButton.Enabled := True;
end;

procedure TInputQueryMemoForm.ValueControlKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
   if Key = VK_ESCAPE then
    CancelButton.Click;
end;

procedure TInputQueryMemoForm.ValueControlKeyPress(Sender: TObject;
  var Key: Char);
begin
  { #10 = Ctrl+Enter, #13 = Enter or Shift+Enter }
  if FSingleLine and ((Key = #10) or (Key = #13)) then
    Key := #0;
end;

end.
