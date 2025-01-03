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
  Classes, Controls, StdCtrls, UIStateForm, Vcl.ExtCtrls;

type
  TInputQueryMemoForm = class(TUIStateForm)
    OKButton: TButton;
    CancelButton: TButton;
    PromptLabel: TLabel;
    ValueControl: TMemo;
    DocImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ValueControlKeyPress(Sender: TObject; var Key: Char);
    procedure ValueControlChange(Sender: TObject);
    procedure ValueControlKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
  private
    FSingleLine: Boolean;
    function GetValue: String;
    procedure SetPrompt(const APrompt: String);
    procedure SetValue(const AValue: String);
    procedure UpdateImages;
    procedure SetDocImageClick(const Value: TNotifyEvent);
  public
    property DocImageClick: TNotifyEvent write SetDocImageClick;
    property Prompt: String write SetPrompt;
    property SingleLine: Boolean write FSingleLine;
    property Value: String read GetValue write SetValue;
  end;

function InputQueryMemo(const ACaption, APrompt: String; var AValue: String;
  const ASingleLine: Boolean = False; const ADocImageClick: TNotifyEvent = nil): Boolean;

implementation

uses
  Windows, Messages, Forms, Graphics, ComCtrls,
  IDE.HelperFunc, IDE.ImagesModule, IDE.MainForm;

{$R *.DFM}

function InputQueryMemo(const ACaption, APrompt: String; var AValue: String;
  const ASingleLine: Boolean; const ADocImageClick: TNotifyEvent): Boolean;
begin
  with TInputQueryMemoForm.Create(Application) do try
    Caption := ACaption;
    Prompt := APrompt;
    Value := AValue;
    SingleLine := ASingleLine;
    DocImageClick := ADocImageClick;
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
  UpdateImages;
end;

procedure TInputQueryMemoForm.FormAfterMonitorDpiChanged(Sender: TObject;
  OldDPI, NewDPI: Integer);
begin
  UpdateImages;
end;

function TInputQueryMemoForm.GetValue: String;
begin
  Result := ValueControl.Text;
end;

procedure TInputQueryMemoForm.SetDocImageClick(const Value: TNotifyEvent);
begin
  DocImage.OnClick := Value;
  DocImage.Visible := Assigned(DocImage.OnClick);
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

procedure TInputQueryMemoForm.UpdateImages;

  function GetImage(const Button: TToolButton; const WH: Integer): TWICImage;
  begin
    Result := ImagesModule.LightToolBarImageCollection.GetSourceImage(Button.ImageIndex, WH, WH)
  end;

begin
 { After a DPI change the button's Width and Height isn't yet updated, so calculate it ourselves }
  var WH := MulDiv(16, CurrentPPI, 96);
  DocImage.Picture.Graphic:= GetImage(MainForm.HelpButton, WH);
end;

end.
