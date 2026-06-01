unit IDE.LocalizeFunc;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE localization functions
}

interface

uses
  Classes;

function LStr(const Str: String; const AllowEmpty: Boolean = False): String;
function LStrFmt(const Str: String; const Args: array of const;
  const AllowEmpty: Boolean = False): String;
procedure LocalizeComponent(const Component: TComponent);

implementation

uses
  SysUtils, Controls, StdCtrls, Menus,
  NewTabSet;

function LStr(const Str: String; const AllowEmpty: Boolean): String;
begin
  if Str = '' then begin
    if AllowEmpty then
      Exit('')
    else
      raise Exception.Create('Internal error: LStr called with empty string');
  end;
  Result := Str; { Temporary }
end;

function LStrFmt(const Str: String; const Args: array of const;
  const AllowEmpty: Boolean): String;
begin
  Result := Format(LStr(Str, AllowEmpty), Args);
end;

type
  TControlAccess = class(TControl);

procedure LocalizeComponent(const Component: TComponent);

  procedure LocalizeStrings(const Strings: TStrings);
  begin
    Strings.BeginUpdate;
    try
      for var I := 0 to Strings.Count-1 do
        if Strings[I] <> '' then
          Strings[I] := LStr(Strings[I]);
    finally
      Strings.EndUpdate;
    end;
  end;

begin
  if Component is TControl then begin
    const Control = TControl(Component);
    if Control.Hint <> '' then
      Control.Hint := LStr(Control.Hint);

    const ControlAccess = TControlAccess(Control);
    if ControlAccess.Text <> '' then { This is both Caption and Text }
      ControlAccess.Text := LStr(ControlAccess.Text);

    if Component is TComboBox then
      LocalizeStrings(TComboBox(Component).Items)
    else if Component is TNewTabSet then begin
      LocalizeStrings(TNewTabSet(Component).Tabs);
      LocalizeStrings(TNewTabSet(Component).Hints);
    end else if Component is TCustomMemo then
      LocalizeStrings(TCustomMemo(Component).Lines);
  end else if Component is TMenuItem then begin
    const MenuItem = TMenuItem(Component);
    if MenuItem.Caption <> '' then
      MenuItem.Caption := LStr(MenuItem.Caption);
    if MenuItem.Hint <> '' then
      MenuItem.Hint := LStr(MenuItem.Hint);
  end;

  for var I := 0 to Component.ComponentCount-1 do
    LocalizeComponent(Component.Components[I]);
end;

end.
