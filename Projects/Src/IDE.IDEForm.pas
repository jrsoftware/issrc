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
  public
    constructor Create(AOwner: TComponent); override;
    property FormThemeActive: Boolean read FFormThemeActive;
  end;

implementation

uses
  IDE.HelperFunc, IDE.LocalizeFunc;

constructor TIDEForm.Create(AOwner: TComponent);
begin
  inherited; { This does not trigger the OnCreate event of the form }
  LocalizeComponent(Self);
  InitFormFont(Self);
  FFormThemeActive := InitFormTheme(Self);
end;

end.
