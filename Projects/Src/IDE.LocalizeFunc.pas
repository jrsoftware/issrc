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

function LFmtMessage(const Str: String; const AllowEmpty: Boolean = False): String; overload;
function LFmtMessage(const Str: String; const Args: array of const;
  const AllowEmpty: Boolean = False): String; overload;

procedure LocalizeComponent(const Component: TComponent);

implementation

uses
  SysUtils, Controls, StdCtrls, Menus,
  NewTabSet;

function LFmtMessage(const Str: String; const AllowEmpty: Boolean): String;
begin
  Result := LFmtMessage(Str, [], AllowEmpty);
end;

function LFmtMessage(const Str: String; const Args: array of const;
  const AllowEmpty: Boolean): String;
begin
  if Str = '' then begin
    if AllowEmpty then
      Exit('')
    else
      raise Exception.Create('Internal error: LFmtMessage called with empty string');
  end;
  Result := Str; { Temporary }
  Result := Format(StringReplace(Result, '%n', #13#10, [rfReplaceAll]), Args);
end;

type
  TControlAccess = class(TControl);

procedure LocalizeComponent(const Component: TComponent);

  function LocalizeStrings(const Strings: TStrings): Boolean;
  begin
    const NewStrings = TStringList.Create;
    try
      NewStrings.Assign(Strings);
      Result := False;
      for var I := 0 to NewStrings.Count-1 do begin
        const LocalizedString = LFmtMessage(NewStrings[I], True);
        if LocalizedString <> NewStrings[I] then begin
          NewStrings[I] := LocalizedString;
          Result := True;
        end;
      end;
      if Result then
        Strings.Assign(NewStrings);
    finally
      NewStrings.Free;
    end;
  end;

  procedure LocalizeComboBox(const ComboBox: TComboBox);
  begin
    const ItemIndex = ComboBox.ItemIndex;
    if LocalizeStrings(ComboBox.Items) then
      ComboBox.ItemIndex := ItemIndex;
  end;

begin
  if Component is TControl then begin
    const Control = TControl(Component);
    if Control.Hint <> '' then
      Control.Hint := LFmtMessage(Control.Hint);

    const ControlAccess = TControlAccess(Control);
    if ControlAccess.Text <> '' then { This is both Caption and Text }
      ControlAccess.Text := LFmtMessage(ControlAccess.Text);

    if Component is TComboBox then
      LocalizeComboBox(TComboBox(Component))
    else if Component is TNewTabSet then begin
      LocalizeStrings(TNewTabSet(Component).Tabs);
      LocalizeStrings(TNewTabSet(Component).Hints);
    end else if Component is TCustomMemo then
      LocalizeStrings(TCustomMemo(Component).Lines);
  end else if Component is TMenuItem then begin
    const MenuItem = TMenuItem(Component);
    if MenuItem.Caption <> '' then
      MenuItem.Caption := LFmtMessage(MenuItem.Caption);
    if MenuItem.Hint <> '' then
      MenuItem.Hint := LFmtMessage(MenuItem.Hint);
  end;

  for var I := 0 to Component.ComponentCount-1 do
    LocalizeComponent(Component.Components[I]);
end;

end.
