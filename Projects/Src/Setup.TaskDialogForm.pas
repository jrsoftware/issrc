unit Setup.TaskDialogForm;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Task Dialog form which can be styled

  Supports up to three command links
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, WinXPanels, ExtCtrls, StdCtrls,
  BidiCtrls, BitmapImage, NewStaticText,
  Setup.SetupForm;

const
  TD_TASKFORM_HELP_ICON = MAKEINTRESOURCEW(Word(-100));

type
  TCopyFormat = (cfTaskDialog, cfMessageBox);

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
    BottomPanel2: TPanel;
    VerificationCheck: TNewCheckBox;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FCommonButtons: array of TNewButton;
    FCommonButtonFlags: array of Cardinal;
    FMainButtons: array of TNewButton;
    FCopyFormat: TCopyFormat;
    procedure Finish(const DefCommonButton: Integer);
    procedure UpdateCommonButtons(const CommonButtons: Cardinal);
    procedure UpdateIcon(const Icon: PChar);
    procedure UpdateInstructionAndText(const Instruction, Text: String);
    procedure UpdateHeight;
    procedure UpdateMainButtonsAndBorderIcons(const CommonButtons: Cardinal;
      const ButtonLabels: array of String; const ButtonIDs: array of Integer; const ShieldButton: Integer);
    procedure UpdateVerificationText(const VerificationText: String; const pfVerificationFlagChecked: PBOOL);
  public
    constructor Create(AOwner: TComponent; const ACopyFormat: TCopyFormat; const ASetForeground: Boolean); reintroduce;
  end;

function TaskDialogForm(const Instruction, Text, Caption: String; const Icon: PChar;
  const CommonButtons: Cardinal; const ButtonLabels: array of String; const ButtonIDs: array of Integer;
  const DefCommonButton, ShieldButton: Integer; const TriggerMessageBoxCallbackFuncFlags: LongInt;
  const VerificationText: String; const pfVerificationFlagChecked: PBOOL; const CopyFormat: TCopyFormat;
  const SetForeground: Boolean): Integer;

implementation

uses
  CommCtrl, Clipbrd, Themes,
  Shared.SetupMessageIDs, Shared.CommonFunc, Shared.CommonFunc.Vcl,
  SetupLdrAndSetup.Messages, Setup.WizardForm, Setup.MainFunc;

{$R *.dfm}

function TaskDialogForm(const Instruction, Text, Caption: String; const Icon: PChar;
  const CommonButtons: Cardinal; const ButtonLabels: array of String; const ButtonIDs: array of Integer;
  const DefCommonButton, ShieldButton: Integer; const TriggerMessageBoxCallbackFuncFlags: LongInt;
  const VerificationText: String; const pfVerificationFlagChecked: PBOOL; const CopyFormat: TCopyFormat;
  const SetForeground: Boolean): Integer;
begin
  const Form = TTaskDialogForm.Create(nil, CopyFormat, SetForeground);
  try
    Form.Caption := Caption;
    Form.UpdateInstructionAndText(Instruction, Text);
    Form.UpdateIcon(Icon);
    Form.UpdateCommonButtons(CommonButtons);
    Form.UpdateVerificationText(VerificationText, pfVerificationFlagChecked);

    if (Pos(':\', Text) <> 0) or (Pos('\\', Text) <> 0) then
      Form.Width := MulDiv(Form.Width, 125, 100);

    if Form.InstructionText.Visible then
      Form.InstructionText.AdjustHeight;
    if Form.TextText.Visible then
      Form.TextText.AdjustHeight;
    Form.UpdateMainButtonsAndBorderIcons(CommonButtons, ButtonLabels, ButtonIDs, ShieldButton);
    Form.UpdateHeight;

    Form.Finish(DefCommonButton);

    TriggerMessageBoxCallbackFunc(TriggerMessageBoxCallbackFuncFlags, False);
    try
      Result := Form.ShowModal;
      if pfVerificationFlagChecked <> nil then
        pfVerificationFlagChecked^ := Form.VerificationCheck.Checked;
    finally
      TriggerMessageBoxCallbackFunc(TriggerMessageBoxCallbackFuncFlags, True);
    end;
  finally
    Form.Free;
  end;
end;

{ TTaskDialogForm }

constructor TTaskDialogForm.Create(AOwner: TComponent; const ACopyFormat: TCopyFormat; const ASetForeground: Boolean);
begin
  inherited Create(AOwner);

  FCommonButtons := [OkButton, YesButton, NoButton, RetryButton, CancelButton];
  FCommonButtonFlags := [TDCBF_OK_BUTTON, TDCBF_YES_BUTTON, TDCBF_NO_BUTTON, TDCBF_RETRY_BUTTON, TDCBF_CANCEL_BUTTON];
  FMainButtons := [MainButton1, MainButton2, MainButton3];
  FCopyFormat := ACopyFormat;
  SetForeground := ASetForeground;

  InitializeFont('Segoe UI', 8);

  var LStyle := StyleServices(Self);
  if not LStyle.Enabled or LStyle.IsSystemStyle then
    LStyle := nil;
  if LStyle <> nil then begin
    { Make MainPanel look the same as WizardForm's main area }
    MainPanel.StyleElements := [];
    MainPanel.Color := LStyle.GetStyleColor(scWindow);
  end;

  const Pad = 10;
  const PadX = ScalePixelsX(Pad);
  const PadY = ScalePixelsY(Pad);

  MainPanel.Padding.Left := PadX;
  MainPanel.Padding.Top := PadY;
  MainPanel.Padding.Right := PadX;
  MainPanel.Padding.Bottom := PadY;
  { Similar to WizardForm: without this UpdateHeight will see wrong BottomMainButton.Top }
  MainStackPanel.HandleNeeded;
  MainStackPanel.Padding.Left := PadX; { Also see Finish }
  MainStackPanel.Spacing := PadY;
  BottomStackPanel.Spacing := PadX;
  BottomStackPanel.Padding.Right := PadX; { Also see Finish }
  VerificationCheck.Left := PadX;

  OkButton.Caption := SetupMessages[msgButtonOK];
  YesButton.Caption := SetupMessages[msgButtonYes];
  NoButton.Caption := SetupMessages[msgButtonNo];
  RetryButton.Caption := SetupMessages[msgAbortRetryIgnoreRetry];
  CancelButton.Caption := SetupMessages[msgButtonCancel];
end;

procedure TTaskDialogForm.Finish(const DefCommonButton: Integer);
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

  if DefCommonButton > 0 then begin
    var I := DefCommonButton;
    for var CommonButton in FCommonButtons do begin
      if CommonButton.Visible then begin
        Dec(I);
        if I = 0 then begin
          ActiveControl := CommonButton;
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TTaskDialogForm.UpdateCommonButtons(const CommonButtons: Cardinal);
var
  VisibleCaptions: array of String;
begin
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
  var BottomControl: TControl := nil;

  if MainButton3.Visible then
    BottomControl := MainButton3
  else if MainButton2.Visible then
    BottomControl := MainButton2
  else if MainButton1.Visible then
    BottomControl := MainButton1
  else if TextText.Visible then
    BottomControl := TextText
  else if InstructionText.Visible then
    BottomControl := InstructionText;

  var NewClientHeight := MainPanel.Padding.Top + MainStackPanel.Top;
  if BottomControl <> nil then
    NewClientHeight := NewClientHeight + BottomControl.Top + BottomControl.Height;

  if LeftPanel.Visible then begin
    { Make sure the height is enough to fit the icon }
    const MinimumClientHeight = MainPanel.Padding.Top + LeftPanel.Top + BitmapImage.Top + BitmapImage.Height;
    if MinimumClientHeight > NewClientHeight then
      NewClientHeight := MinimumClientHeight;
  end;

  if BottomPanel.Visible then
    NewClientHeight := NewClientHeight + BottomPanel.Height;
  if BottomPanel2.Visible then
    NewClientHeight := NewClientHeight + BottomPanel2.Height;

  ClientHeight := NewClientHeight;
end;

procedure TTaskDialogForm.UpdateIcon(const Icon: PChar);
begin
  var ResourceName := '';
  if Icon = TD_ERROR_ICON then
    ResourceName := 'Z_TASKFORM_ERRORICON' + WizardIconsPostfix
  else if Icon = TD_TASKFORM_HELP_ICON then
    ResourceName := 'Z_TASKFORM_HELPICON' + WizardIconsPostfix
  else if Icon = TD_INFORMATION_ICON then
    ResourceName := 'Z_TASKFORM_INFOICON' + WizardIconsPostfix
  else if Icon = TD_WARNING_ICON then
    ResourceName := 'Z_TASKFORM_WARNICON' + WizardIconsPostfix
  else if Icon <> nil then
    ResourceName := Icon;

  if ResourceName <> '' then
    BitmapImage.InitializeFromIcon(HInstance, PChar(ResourceName), clNone, [32, 48, 64])
  else
    LeftPanel.Visible := False;
end;

procedure TTaskDialogForm.UpdateInstructionAndText(const Instruction, Text: String);
begin
  InstructionText.Visible := Instruction <> '';
  if InstructionText.Visible then begin
    InstructionText.Caption := Instruction;
    InstructionText.Font.Height := MulDiv(InstructionText.Font.Height, 12, 8);
  end;
  TextText.Visible := Text <> '';
  if TextText.Visible then
    TextText.Caption := Text;
end;

procedure TTaskDialogForm.UpdateMainButtonsAndBorderIcons(const CommonButtons: Cardinal;
  const ButtonLabels: array of String; const ButtonIDs: array of Integer; const ShieldButton: Integer);
begin
  var HaveCancel := False;
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
      MainButton.Font.Height := MulDiv(MainButton.Font.Height, 12, 8);
      MainButton.CommandLinkHint := Hint;
      MainButton.ModalResult := ButtonIDs[I];
      if MainButton.ModalResult = IDCANCEL then begin
        MainButton.Cancel := True;
        HaveCancel := True;
      end;
      MainButton.ElevationRequired := MainButton.ModalResult = ShieldButton;
      MainButton.AdjustHeightIfCommandLink;
    end;
  end;

  if not HaveCancel and (CommonButtons and TDCBF_CANCEL_BUTTON = 0) then begin
    const SystemMenu = GetSystemMenu(Handle, False);
    if SystemMenu <> 0 then
      EnableMenuItem(SystemMenu, SC_CLOSE, MF_BYCOMMAND or MF_GRAYED);
  end;
end;

procedure TTaskDialogForm.UpdateVerificationText(const VerificationText: String;
  const pfVerificationFlagChecked: PBOOL);
begin
  if VerificationText <> '' then begin
    VerificationCheck.Caption := VerificationText;
    if pfVerificationFlagChecked <> nil then
      VerificationCheck.Checked := pfVerificationFlagChecked^;
  end else
    BottomPanel2.Visible := False;
end;

procedure TTaskDialogForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  procedure AddButtonCaption(const SB: TStringBuilder; const MessageBoxFormat: Boolean;
    const Button: TButton);
  begin
    const Caption = RemoveAccelChar(Button.Caption);
    if MessageBoxFormat then
      SB.Append(Format('%s   ', [Caption]))
    else
      SB.Append(Format('[%s] ', [Caption]));
  end;

begin
  if (Shift = [ssCtrl]) and (Key = Ord('C')) then begin
    Key := 0;
    const SB = TStringBuilder.Create;
    try
      { Do not localize }

      const SLine = '---------------------------';
      const SLineAndNewLine = SLine + SNewLine;

      const MessageBoxFormat = FCopyFormat = cfMessageBox;

      if MessageBoxFormat then
        SB.Append(SLine)
      else
        SB.Append('[Window Title]');
      SB.Append(SNewLine);
      SB.Append(Caption);
      SB.Append(SNewLine);
      if MessageBoxFormat then
        SB.Append(SLineAndNewLine)
      else
        SB.Append(SNewLine);

      if InstructionText.Visible then begin
        if not MessageBoxFormat then begin
          SB.Append('[Main Instruction]');
          SB.Append(SNewLine);
        end;
        SB.Append(InstructionText.Caption);
        SB.Append(SNewLine);
        if not MessageBoxFormat or TextText.Visible then
          SB.Append(SNewLine);
      end;

      if TextText.Visible then begin
        if not MessageBoxFormat then begin
          SB.Append('[Content]');
          SB.Append(SNewLine);
        end;
        SB.Append(TextText.Caption);
        SB.Append(SNewLine);
        if not MessageBoxFormat then
          SB.Append(SNewLine);
      end;

      if MessageBoxFormat then
        SB.Append(SLineAndNewLine);

      for var MainButton in FMainButtons do
        if MainButton.Visible then
          AddButtonCaption(SB, MessageBoxFormat, MainButton);

      for var CommonButton in FCommonButtons do
        if CommonButton.Visible then
          AddButtonCaption(SB, MessageBoxFormat, CommonButton);

      if MessageBoxFormat then begin
        SB.Append(SNewLine);
        SB.Append(SLine); { Causes the spaces after the last button caption not to be trimmed, but this is same as with native MessageBox }
      end;

      Clipboard.AsText := SB.ToString.Trim;
    finally
      SB.Free;
    end;
  end;
end;

end.
