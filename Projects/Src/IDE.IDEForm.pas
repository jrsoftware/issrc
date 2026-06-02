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
    FThemeStyled: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    property ThemeStyled: Boolean read FThemeStyled;
  end;

implementation

uses
  IDE.HelperFunc, IDE.LocalizeFunc;

constructor TIDEForm.Create(AOwner: TComponent);
begin
  inherited;
  LocalizeComponent(Self);
  InitFormFont(Self);
  FThemeStyled := InitFormTheme(Self);
end;

end.
