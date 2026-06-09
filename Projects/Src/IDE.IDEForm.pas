unit IDE.IDEForm;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TIDEForm, a TUIStateForm descendant which localizes the form
  and initializes its font and theme.

  Not used by IDE.MainForm!
}

interface

uses
  Classes, Controls, StdCtrls,
  UIStateForm;

type
  TIDEForm = class(TUIStateForm)
  private
    FFormThemeActive: Boolean;
    function ScalePixelsX(const N: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    function CalculateButtonWidth(const ButtonCaptions: array of String): Integer;
    function SizeBottomButtons(const LeftBottomButton, RightBottomButton: TButton;
      const ResizeControl: TControl = nil): Integer; overload;
    function SizeBottomButtons(const LeftBottomButton, RightBottomButton: TButton;
      const OtherButtons: array of TButton; const ResizeControl: TControl = nil): Integer; overload;
    function SizeSideButtons(const Buttons: array of TButton;
      const ResizeControl: TControl): Integer; overload;
    function SizeSideButtons(const Buttons: array of TButton;
      const ResizeControls: array of TControl): Integer; overload;
    property FormThemeActive: Boolean read FFormThemeActive;
  end;

implementation

uses
  Windows,
  Shared.CommonFunc,
  IDE.HelperFunc, IDE.LocalizeFunc;

constructor TIDEForm.Create(AOwner: TComponent);
begin
  inherited; { This does not trigger the OnCreate event of the form }
  LocalizeComponent(Self);
  InitFormFont(Self);
  FFormThemeActive := InitFormTheme(Self);
end;

function TIDEForm.ScalePixelsX(const N: Integer): Integer;
begin
  { Unlike Setup, the IDE's forms have Scaled set to True, so scale using
    CurrentPPI instead of font base units }
  Result := MulDiv(N, CurrentPPI, 96);
end;

function TIDEForm.CalculateButtonWidth(const ButtonCaptions: array of String): Integer;
begin
  { Same code as TSetupForm.CalculateButtonWidth }
  Result := ScalePixelsX(75);
  { Increase the button size if there are unusually long button captions }
  const DC = GetDC(0);
  try
    SelectObject(DC, Font.Handle);
    for var I := Low(ButtonCaptions) to High(ButtonCaptions) do begin
      const W = GetTextWidth(DC, ButtonCaptions[I], True) + ScalePixelsX(20);
      if Result < W then
        Result := W;
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

function TIDEForm.SizeBottomButtons(const LeftBottomButton,
  RightBottomButton: TButton; const ResizeControl: TControl): Integer;
begin
  Result := SizeBottomButtons(LeftBottomButton, RightBottomButton, [],
    ResizeControl);
end;

function TIDEForm.SizeBottomButtons(const LeftBottomButton, RightBottomButton: TButton;
  const OtherButtons: array of TButton; const ResizeControl: TControl): Integer;
begin
  var Captions: array of String := [LeftBottomButton.Caption, RightBottomButton.Caption];
  for var Button in OtherButtons do
    Captions := Captions + [Button.Caption];
  Result := CalculateButtonWidth(Captions);
  const Gap = RightBottomButton.Left - LeftBottomButton.Left - LeftBottomButton.Width;
  const Diff = Result - RightBottomButton.Width;
  RightBottomButton.Width := Result;
  RightBottomButton.Left := RightBottomButton.Left - Diff;
  LeftBottomButton.Width := Result;
  const OldLeft = LeftBottomButton.Left;
  LeftBottomButton.Left := RightBottomButton.Left - Gap - Result;
  if ResizeControl <> nil then
    ResizeControl.Width := ResizeControl.Width - (OldLeft - LeftBottomButton.Left);
end;

function TIDEForm.SizeSideButtons(const Buttons: array of TButton;
  const ResizeControl: TControl): Integer;
begin
  Result := SizeSideButtons(Buttons, [ResizeControl])
end;

function TIDEForm.SizeSideButtons(const Buttons: array of TButton;
  const ResizeControls: array of TControl): Integer;
begin
  var Captions: array of String;
  for var Button in Buttons do
    Captions := Captions + [Button.Caption];
  Result := CalculateButtonWidth(Captions);
  const Diff = Result - Buttons[0].Width;
  for var Button in Buttons do begin
    Button.Width := Result;
    Button.Left := Button.Left - Diff;
  end;
  for var ResizeControl in ResizeControls do
    if ResizeControl <> nil then
      ResizeControl.Width := ResizeControl.Width - Diff;
end;

end.
