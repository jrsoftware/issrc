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

type
  TTranslationPair = record
    English, Localized: String;
  end;

  TIDELanguage = (ilEnglish, ilDutch, ilGerman, ilJapanese);

procedure InitLocalization(const Lang: TIDELanguage);

function LFmtMessage(const Str: String; const AllowEmpty: Boolean = False): String; overload;
function LFmtMessage(const Str: String; const Args: array of const;
  const AllowEmpty: Boolean = False): String; overload;

procedure LocalizeComponent(const Component: TComponent);

implementation

uses
  SysUtils, Controls, StdCtrls, Menus, Generics.Collections,
  NewTabSet,
  IDE.LocalizeFunc.Dutch, IDE.LocalizeFunc.German, IDE.LocalizeFunc.Japanese;

var
  TranslationDictionary: TDictionary<String, String>;

procedure InitLocalization(const Lang: TIDELanguage);

  procedure AddTranslations(const Translations: array of TTranslationPair);
  begin
    for var I := Low(Translations) to High(Translations) do
      TranslationDictionary.AddOrSetValue(Translations[I].English, Translations[I].Localized);
  end;

begin
  TranslationDictionary.Clear;
  case Lang of
    ilDutch: AddTranslations(DutchIDETranslations);
    ilGerman: AddTranslations(GermanIDETranslations);
    ilJapanese: AddTranslations(JapaneseIDETranslations);
  end;
end;

function FmtIDEMessage(S: PChar; const Args: array of const): String;
{ Same as Setup's FmtMessage, but takes an array of const and replaces %n.
  Important property: %1, %2, etc. with no matching argument stay
  unchanged, so it is possible to first call FmtIDEMessage on a string
  without supplying the arguments, and then later call it a second
  time. This is used for .dfm strings having %1, etc. }

  function ArgToStr(const Arg: TVarRec): String;
  begin
    case Arg.VType of
      vtInteger: Result := IntToStr(Arg.VInteger);
      vtInt64: Result := IntToStr(Arg.VInt64^);
      vtChar: Result := Char(Arg.VChar);
      vtWideChar: Result := Arg.VWideChar;
      vtString: Result := String(Arg.VString^);
      vtAnsiString: Result := String(AnsiString(Arg.VAnsiString));
      vtWideString: Result := WideString(Arg.VWideString);
      vtUnicodeString: Result := UnicodeString(Arg.VUnicodeString);
    else
      raise Exception.Create('Internal error: Unexpected Arg.VType');
    end;
  end;

begin
  Result := '';
  if S = nil then Exit;
  while True do begin
    var P := StrScan(S, '%');
    if P = nil then begin
      Result := Result + S;
      Break;
    end;
    if P <> S then begin
      var Z: String;
      SetString(Z, S, P - S);
      Result := Result + Z;
      S := P;
    end;
    Inc(P);
    if CharInSet(P^, ['1'..'9']) and (Ord(P^) - Ord('1') <= High(Args)) then begin
      Result := Result + ArgToStr(Args[Ord(P^) - Ord('1')]);
      Inc(S, 2);
    end else if P^ = 'n' then begin
      Result := Result + #13#10;
      Inc(S, 2);
    end else begin
      Result := Result + '%';
      Inc(S);
      if P^ = '%' then
        Inc(S);
    end;
  end;
end;

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
  if not Assigned(TranslationDictionary) or
     not TranslationDictionary.TryGetValue(Str, Result) then
    Result := Str;
  Result := FmtIDEMessage(PChar(Result), Args);
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

  procedure LocalizeComboBox(const ComboBox: TCustomComboBox);
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

    { Of the following, only TNewTabSet.Tabs is currently actually
      prefilled in the .dfm files }
    if Component is TCustomComboBox then
      LocalizeComboBox(TCustomComboBox(Component))
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

initialization
  TranslationDictionary := TDictionary<String, String>.Create;
finalization
  TranslationDictionary.Free;
end.
