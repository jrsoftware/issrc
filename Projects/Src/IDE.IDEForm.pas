unit IDE.IDEForm;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TIDEForm, a TUIStateForm descendant which localizes the form
  and initializes its font and theme.

  IDE.MainForm itself is not a TIDEForm!
}

interface

uses
  Messages, Classes, Forms, Controls, StdCtrls,
  UIStateForm;

type
  TIDEForm = class(TUIStateForm)
  private
    FFormThemeActive: Boolean;
    FUncloakPending: Boolean;
    FStartupCloakingUsed, FStartupClientAreaFilled: Boolean;
    function ScalePixelsX(const N: Integer): Integer;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    function CalculateButtonWidth(const ButtonCaptions: array of String): Integer;
    class procedure HandleWMWindowPosChanged(const Form: TForm;
      const Message: TWMWindowPosChanged; var StartupClientAreaFilled: Boolean;
      const StartupCloakingUsed, FormThemeIsDark: Boolean); static;
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
  Themes,
  UnsignedFunc,
  Shared.CommonFunc, Shared.CommonFunc.Vcl,
  IDE.HelperFunc, IDE.LocalizeFunc;

constructor TIDEForm.Create(AOwner: TComponent);
begin
  inherited; { This does not trigger the OnCreate event of the form }
  LocalizeComponent(Self);
  InitFormFont(Self);
  FFormThemeActive := InitFormTheme(Self);
end;

procedure TIDEForm.CreateWnd;
begin
  inherited;
  if (TStyleManager.FormBorderStyle = fbsSystemStyle) or not (seBorder in StyleElements) then
    SetDarkTitleBar(Self, InitFormThemeIsDark);
  if not ClientAreaAnimationsActive then begin
    { Prevents flicker, especially in dark mode, but even in light mode for a heavy form }
    FUncloakPending := SetWindowCloaked(Handle, True);
    if FUncloakPending then
      FStartupCloakingUsed := True;
  end;
end;

class procedure TIDEForm.HandleWMWindowPosChanged(const Form: TForm;
  const Message: TWMWindowPosChanged; var StartupClientAreaFilled: Boolean;
  const StartupCloakingUsed, FormThemeIsDark: Boolean);
{ When the window is shown without a cloaking (see CreateWnd) it appears
  white until the first paints complete, which causes bad flicker in dark
  mode. Avoid that by filling the client area with the dark background
  color as soon as possible. WM_WINDOWPOSCHANGED was the earliest working
  moment in many tests. At WM_WINDOWPOSCHANGING and WM_NCPAINT the code
  has no effect, and a class background brush was also not used by the
  system. With WM_WINDOWPOSCHANGED the paint happens before any control
  is visible, and it fixes all per-control flicker at once. }

  procedure FillClientAreaWithBackgroundColor;
  { Uses the window DC because child windows do not clip it, so it also
    covers the areas of controls that have not painted yet. }
  begin
    const DC = GetWindowDC(Form.Handle);
    if DC <> 0 then begin
      try
        var WindowRect: TRect;
        GetWindowRect(Form.Handle, WindowRect);
        const Origin = Form.ClientOrigin;
        var R := TRect.Create(0, 0, Form.ClientWidth, Form.ClientHeight);
        OffsetRect(R, Origin.X - WindowRect.Left, Origin.Y - WindowRect.Top);
        const Brush = CreateSolidBrush(UColorToRGB(Form.Color)); { Color set by InitFormTheme }
        FillRect(DC, R, Brush);
        DeleteObject(Brush);
      finally
        ReleaseDC(Form.Handle, DC);
      end;
    end;
  end;

begin
  if (Message.WindowPos.flags and SWP_SHOWWINDOW <> 0) and
     not StartupClientAreaFilled and not StartupCloakingUsed and
     FormThemeIsDark then begin
    { Update callers' variable tracking this has been done }
    StartupClientAreaFilled := True;
    { Fill the client area }
    FillClientAreaWithBackgroundColor;
    { Make sure the fill has reached the surface }
    GdiFlush;
    { ValidateRect so the BeginPaint below has nothing to erase }
    ValidateRect(Form.Handle, nil);
    { The fill did reach the surface, but the system keeps showing white until
      the window completes a BeginPaint/EndPaint cycle, so do an empty one;
      This bit is the essential trick to get our color to appear early. }
    var PS: TPaintStruct;
    BeginPaint(Form.Handle, PS);
    EndPaint(Form.Handle, PS);
    { Two problems remain: the empty cycle removed the pending paint of the
      form itself, which is what draws windowless controls, and the fill
      overwrote what controls already drew when the system sent them
      WM_NCPAINT and WM_ERASEBKGND while showing the window, such as the
      borders and backgrounds of list boxes. So queue a repaint of
      everything. }
    RedrawWindow(Form.Handle, nil, 0,
      RDW_INVALIDATE or RDW_ERASE or RDW_FRAME or RDW_ALLCHILDREN);
  end;
end;

procedure TIDEForm.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  { See MainForm, and above }
  HandleWMWindowPosChanged(Self, Message, FStartupClientAreaFilled,
    FStartupCloakingUsed, InitFormThemeIsDark);
  inherited;
end;

procedure TIDEForm.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if FUncloakPending then begin
    FUncloakPending := False;
    RedrawWindow(Handle, nil, 0, RDW_UPDATENOW or RDW_ALLCHILDREN);
    { Everything is now painted, show the window }
    SetWindowCloaked(Handle, False);
  end;
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
var
  Captions: array of String;
begin
  Captions := [LeftBottomButton.Caption, RightBottomButton.Caption];
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
var
  Captions: array of String;
begin
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
