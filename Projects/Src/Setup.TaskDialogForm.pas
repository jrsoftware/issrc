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
    MainButton1: TNewButton;
    MainButton2: TNewButton;
    MainButton3: TNewButton;
    BottomStackPanel: TStackPanel;
    OkButton: TNewButton;
    YesButton: TNewButton;
    NoButton: TNewButton;
    RetryButton: TNewButton;
    CancelButton: TNewButton;
  private
    FCommonButtons: array of TNewButton;
    FCommonButtonFlags: array of Cardinal;
    FMainButtons: array of TNewButton;
    FPadX, FPadY: Integer;
    procedure Finish;
    procedure UpdateCommonButtons(const CommonButtons: Cardinal);
    procedure UpdateIcon(const Icon: PChar);
    procedure UpdateHeight;
    procedure UpdateMainButtons(const ButtonLabels: array of String; const ButtonIDs: array of Integer; const ShieldButton: Integer);
  public
    constructor Create(AOwner: TComponent); override; 
  end;

function TaskDialogForm(const Instruction, Text, Caption: String; const Icon: PChar;
  const CommonButtons: Cardinal; const ButtonLabels: array of String; const ButtonIDs: array of Integer;
  const ShieldButton: Integer; const TriggerMessageBoxCallbackFuncFlags: LongInt;
  const VerificationText: String; const pfVerificationFlagChecked: PBOOL): Integer;

implementation

uses
  CommCtrl,
  Shared.SetupMessageIDs, Shared.CommonFunc.Vcl, SetupLdrAndSetup.Messages, Setup.WizardForm,
  Setup.MainFunc;

{$R *.dfm}

function TaskDialogForm(const Instruction, Text, Caption: String; const Icon: PChar;
  const CommonButtons: Cardinal; const ButtonLabels: array of String; const ButtonIDs: array of Integer;
  const ShieldButton: Integer; const TriggerMessageBoxCallbackFuncFlags: LongInt;
  const VerificationText: String; const pfVerificationFlagChecked: PBOOL): Integer;
begin
  const Form = TTaskDialogForm.Create(nil);
  try
    Form.Caption := Caption;
    Form.InstructionText.Caption := Instruction;
    Form.InstructionText.Font.Size := MulDiv(Form.Font.Size, 13, 9);
    Form.TextText.Caption := Text;
    Form.UpdateIcon(Icon);
    Form.UpdateMainButtons(ButtonLabels, ButtonIDs, ShieldButton);
    Form.UpdateCommonButtons(CommonButtons);
    Form.UpdateHeight;
    if (Pos(':\', Text) <> 0) or (Pos('\\', Text) <> 0) then
      Form.Width := MulDiv(Form.Width, 125, 100);
    Form.InstructionText.AdjustHeight;
    Form.TextText.AdjustHeight;
    Form.Finish;

    TriggerMessageBoxCallbackFunc(TriggerMessageBoxCallbackFuncFlags, False);
    try
      Result := Form.ShowModal;
    finally
      TriggerMessageBoxCallbackFunc(TriggerMessageBoxCallbackFuncFlags, True);
    end;
  finally
    Form.Free;
  end;
end;

{ TTaskDialogForm }

constructor TTaskDialogForm.Create(AOwner: TComponent);
begin
  inherited;

  FCommonButtons := [OkButton, YesButton, NoButton, RetryButton, CancelButton];
  FCommonButtonFlags := [TDCBF_OK_BUTTON, TDCBF_YES_BUTTON, TDCBF_NO_BUTTON, TDCBF_RETRY_BUTTON, TDCBF_CANCEL_BUTTON];
  FMainButtons := [MainButton1, MainButton2, MainButton3];

  InitializeFont;

  const Pad = 10;
  FPadX := ScalePixelsX(Pad);
  FPadY := ScalePixelsY(Pad);

  MainPanel.Padding.Left := FPadX;
  MainPanel.Padding.Top := FPadY;
  MainPanel.Padding.Right := FPadX;
  MainPanel.Padding.Bottom := FPadY;
  { Similar to WizardForm: without this UpdateHeight will see wrong BottomMainButton.Top }
  MainStackPanel.HandleNeeded;
  MainStackPanel.Padding.Left := FPadX; { Also see below }
  MainStackPanel.Spacing := FPadY;
  BottomStackPanel.Spacing := FPadX;
  BottomStackPanel.Padding.Right := FPadX; { Also see below }

  OkButton.Caption := SetupMessages[msgButtonOK];
  YesButton.Caption := SetupMessages[msgButtonYes];
  NoButton.Caption := SetupMessages[msgButtonNo];
  RetryButton.Caption := SetupMessages[msgAbortRetryIgnoreRetry];
  CancelButton.Caption := SetupMessages[msgButtonCancel];
end;

procedure TTaskDialogForm.Finish;
begin
  if RightToLeft then begin
    { FlipSizeAndCenterIfNeeded does not update Align or Padding }
    if LeftPanel.Visible then
      LeftPanel.Align := alRight;
    MainStackPanel.Padding.Right := MainStackPanel.Padding.Left;
    MainStackPanel.Padding.Left := 0;
    if BottomPanel.Visible then begin
      BottomStackPanel.Align := alLeft;
      BottomStackPanel.Padding.Left := BottomStackPanel.Padding.Right;
      BottomStackPanel.Padding.Right := 0;
    end;
  end;

  KeepSizeX := True; { Already bit wider than regular task dialogs }
  KeepSizeY := True; { UpdateHeight already set height }
  FlipSizeAndCenterIfNeeded(Assigned(WizardForm), WizardForm, False);
end;

procedure TTaskDialogForm.UpdateCommonButtons(const CommonButtons: Cardinal);
begin
  var VisibleCaptions: array of String;
  var NVisibleCaptions := 0;
  for var I := 0 to Length(FCommonButtons)-1 do begin
    const CommonButton = FCommonButtons[I];
    const CommonButtonFlag = FCommonButtonFlags[I];
    CommonButton.Visible := CommonButtons and CommonButtonFlag <> 0;
    if CommonButton.Visible then begin
      Inc(NVisibleCaptions);
      SetLength(VisibleCaptions, NVisibleCaptions);
      VisibleCaptions[NVisibleCaptions-1] := CommonButton.Caption;
    end;
  end;

  BottomPanel.Visible := NVisibleCaptions > 0;
  if BottomPanel.Visible then begin
    const W = CalculateButtonWidth(VisibleCaptions);
    for var CommonButton in FCommonButtons do
      if CommonButton.Visible then
        CommonButton.Width := W;
  end;
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

procedure TTaskDialogForm.UpdateIcon(const Icon: PChar);
begin
  var ResourceName := '';
  if Icon = TD_INFORMATION_ICON then
    ResourceName := 'Z_TASKFORM_INFOICON' + WizardIconsPostfix
  else if Icon = TD_WARNING_ICON then
    ResourceName := 'Z_TASKFORM_WARNICON' + WizardIconsPostfix
  else if Icon = TD_ERROR_ICON then
    ResourceName := 'Z_TASKFORM_ERRORICON' + WizardIconsPostfix
  else if Icon <> nil then
    ResourceName := Icon;

  if ResourceName <> '' then
    BitmapImage.InitializeFromIcon(HInstance, PChar(ResourceName), clNone, [32, 48, 64])
  else
    LeftPanel.Visible := False;
end;

procedure TTaskDialogForm.UpdateMainButtons(const ButtonLabels: array of String; const ButtonIDs: array of Integer; const ShieldButton: Integer);
begin
  for var I := 0 to Length(FMainButtons)-1 do begin
    const MainButton = FMainButtons[I];
    MainButton.Visible := I < Length(ButtonLabels);
    if MainButton.Visible then begin
      var Caption := ButtonLabels[I];
      var Hint: String;
      const P = Pos(#10, Caption);
      if P <> 0 then begin
        Hint := Copy(Caption, P+1, MaxInt);
        Delete(Caption, P, MaxInt);
      end else
        Hint := '';
      MainButton.Caption := Caption;
      MainButton.CommandLinkHint := Hint;
      MainButton.ModalResult := ButtonIDs[I];
      if MainButton.ModalResult = IDCANCEL then
        MainButton.Cancel := True;
      MainButton.ElevationRequired := MainButton.ModalResult = ShieldButton;
    end;
  end;
end;

end.
