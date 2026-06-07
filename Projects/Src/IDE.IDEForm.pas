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
  Classes,
  UIStateForm;

type
  TIDEForm = class(TUIStateForm)
  private
    FFormThemeActive: Boolean;
    function ScalePixelsX(const N: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    function CalculateButtonWidth(const ButtonCaptions: array of String): Integer;
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

end.
