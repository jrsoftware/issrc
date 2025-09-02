unit Setup.TaskDialogForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, WinXPanels, ExtCtrls, StdCtrls,
  BidiCtrls, BitmapImage, NewStaticText,
  Setup.SetupForm;

type
  TTaskDialogForm = class(TSetupForm)
    BottomPanel: TPanel;
    MainPanel: TPanel;
    LeftPanel: TPanel;
    BitmapImage: TBitmapImage;
    MainStackPanel: TStackPanel;
    InstructionText: TNewStaticText;
    TextText: TNewStaticText;
    MainButton1: TButton;
    MainButton2: TButton;
    MainButton3: TButton;
    BottomStackPanel: TStackPanel;
    OkButton: TNewButton;
    YesButton: TNewButton;
    NoButton: TNewButton;
    RetryButton: TNewButton;
    CancelButton: TNewButton;
  private
    FPadX, FPadY: Integer;
    procedure UpdateHeight;
  public
    constructor Create(AOwner: TComponent); override; 
  end;

function TaskDialogForm(const Instruction, Text, Caption, Icon: String; const CommonButtons: Cardinal; const ButtonLabels: array of String; const ButtonIDs: array of Integer; const ShieldButton: Integer; const VerificationText: PWideChar; const pfVerificationFlagChecked: PBOOL): Integer;

implementation

uses
  CommCtrl, Setup.WizardForm;

{$R *.dfm}

function TaskDialogForm(const Instruction, Text, Caption, Icon: String; const CommonButtons: Cardinal; const ButtonLabels: array of String; const ButtonIDs: array of Integer; const ShieldButton: Integer; const VerificationText: PWideChar; const pfVerificationFlagChecked: PBOOL): Integer;

  procedure UpdateMainButton(const Button: TButton; const Index: Integer);
  begin
    Button.Visible := Index < Length(ButtonLabels);
    if Button.Visible then begin
      var Caption := ButtonLabels[Index];
      var Hint: String;
      const P = Pos(#10, Caption);
      if P <> 0 then begin
        Hint := Copy(Caption, P+1, MaxInt);
        Delete(Caption, P, MaxInt);
      end else
        Hint := '';
      Button.Caption := Caption;
      Button.CommandLinkHint := Hint;
      Button.ModalResult := ButtonIDs[Index];
      Button.ElevationRequired := Button.ModalResult = ShieldButton;
    end;
  end;

  procedure UpdateCommonButton(const Button: TButton; const CommonButtonFlag: Cardinal; var MadeVisible: Boolean);
  begin
    Button.Visible := (CommonButtons and CommonButtonFlag) <> 0;
    if Button.Visible then
      MadeVisible := True;
  end;

begin
  const Form = TTaskDialogForm.Create(nil);
  try
    Form.InstructionText.Caption := Instruction;
    Form.InstructionText.Font.Size := MulDiv(Form.Font.Size, 13, 9);
    Form.TextText.Caption := Text;
    Form.Caption := Caption;

    UpdateMainButton(Form.MainButton1, 0);
    UpdateMainButton(Form.MainButton2, 1);
    UpdateMainButton(Form.MainButton3, 2);

    var HasCommonButtons := False;
    UpdateCommonButton(Form.OkButton, TDCBF_OK_BUTTON, HasCommonButtons);
    UpdateCommonButton(Form.YesButton, TDCBF_YES_BUTTON, HasCommonButtons);
    UpdateCommonButton(Form.NoButton, TDCBF_NO_BUTTON, HasCommonButtons);
    UpdateCommonButton(Form.RetryButton, TDCBF_RETRY_BUTTON, HasCommonButtons);
    UpdateCommonButton(Form.CancelButton, TDCBF_CANCEL_BUTTON, HasCommonButtons);
    Form.BottomPanel.Visible := HasCommonButtons;

    Form.UpdateHeight;

    Result := Form.ShowModal;
  finally
    Form.Free;
  end;
end;

{ TTaskDialogForm }

constructor TTaskDialogForm.Create(AOwner: TComponent);
begin
  inherited;

  InitializeFont;

  KeepSizeY := True; { We will autosize height later }
  FlipSizeAndCenterIfNeeded(Assigned(WizardForm), WizardForm, False);

  const Pad = 10;
  FPadX := ScalePixelsX(Pad);
  FPadY := ScalePixelsY(Pad);

  MainPanel.Padding.Left := FPadX;
  MainPanel.Padding.Top := FPadY;
  MainPanel.Padding.Right := FPadX;
  MainPanel.Padding.Bottom := FPadY;
  { Similar to WizardForm: without this UpdateHeight will see wrong BottomMainButton.Top }
  MainStackPanel.HandleNeeded;
  MainStackPanel.Spacing := FPadY;
  BottomStackPanel.Spacing := FPadX;
  BottomStackPanel.Padding.Right := FPadX;
end;

procedure TTaskDialogForm.UpdateHeight;
begin
  var BottomMainButton := MainButton1;
  if MainButton3.Visible then
    BottomMainButton := MainButton3
  else if MainButton2.Visible then
    BottomMainButton := MainButton2;

  var NewClientHeight := FPadY + MainStackPanel.Top + BottomMainButton.Top + BottomMainButton.Height;
  if BottomPanel.Visible then
    NewClientHeight := NewClientHeight + BottomPanel.Height;

  ClientHeight := NewClientHeight;
end;

end.
