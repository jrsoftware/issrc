unit SelLangForm;

{
  Inno Setup
  Copyright (C) 1997-2006 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  "Select Language" form

  $jrsoftware: issrc/Projects/SelLangForm.pas,v 1.18 2010/01/13 17:48:52 mlaan Exp $
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SetupForm, StdCtrls, ExtCtrls, NewStaticText, BitmapImage, BidiCtrls;

type
  TSelectLanguageForm = class(TSetupForm)
    SelectLabel: TNewStaticText;
    LangCombo: TNewComboBox;
    OKButton: TNewButton;
    CancelButton: TNewButton;
    IconBitmapImage: TBitmapImage;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

function AskForLanguage: Boolean;

implementation

uses
  Struct, Msgs, MsgIDs, Main;

{$R *.DFM}

var
  DefComboWndProcW, PrevComboWndProc: Pointer;

function NewComboWndProc(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
stdcall;
begin
  case Msg of
    { CB_ADDSTRING must pass to the default Unicode combo box window procedure
      since PrevWndProc is an ANSI window procedure and calling it would result
      in Unicode->ANSI conversion. Do the same for CB_GETLBTEXT(LEN) so that
      MSAA sees Unicode strings. }
    CB_ADDSTRING, CB_GETLBTEXT, CB_GETLBTEXTLEN:
      Result := CallWindowProcW(DefComboWndProcW, Wnd, Msg, wParam, lParam)
  else
    Result := CallWindowProcW(PrevComboWndProc, Wnd, Msg, wParam, lParam);
  end;
end;

function AskForLanguage: Boolean;
{ Creates and shows the "Select Language" dialog. Returns True and activates
  the selected language if the user clicks OK, or False otherwise. }
var
  LangForm: TSelectLanguageForm;
  I, J: Integer;
  LangEntry: PSetupLanguageEntry;
{$IFNDEF UNICODE}
  ClassInfo: TWndClassW;
  N: String;
{$ENDIF}
  PrevLang: String;
begin
  LangForm := TSelectLanguageForm.Create(Application);
  try
{$IFNDEF UNICODE}
    { On NT, make it possible to add Unicode strings to our ANSI combo box by
      installing a window procedure with special CB_ADDSTRING handling.
      Yeah, this is a hack; it's too hard to create a native Unicode control
      in Delphi. }
    if Win32Platform = VER_PLATFORM_WIN32_NT then begin
      if GetClassInfoW(0, 'COMBOBOX', ClassInfo) then begin
        DefComboWndProcW := ClassInfo.lpfnWndProc;
        Longint(PrevComboWndProc) := SetWindowLongW(LangForm.LangCombo.Handle,
          GWL_WNDPROC, Longint(@NewComboWndProc));
      end;
    end;
{$ENDIF}

    for I := 0 to Entries[seLanguage].Count-1 do begin
      LangEntry := Entries[seLanguage][I];
{$IFDEF UNICODE}
      J := LangForm.LangCombo.Items.Add(LangEntry.LanguageName);
      LangForm.LangCombo.Items.Objects[J] := TObject(I);
{$ELSE}
      if (I = ActiveLanguage) or (LangEntry.LanguageCodePage = 0) or
         (LangEntry.LanguageCodePage = GetACP) or
         (shShowUndisplayableLanguages in SetupHeader.Options) then begin
        { Note: LanguageName is Unicode }
        N := LangEntry.LanguageName + #0#0;  { need wide null! }
        if Win32Platform = VER_PLATFORM_WIN32_NT then
          J := SendMessageW(LangForm.LangCombo.Handle, CB_ADDSTRING, 0,
            Longint(PWideChar(Pointer(N))))
        else
          J := LangForm.LangCombo.Items.Add(WideCharToString(PWideChar(Pointer(N))));
        if J >= 0 then
          LangForm.LangCombo.Items.Objects[J] := TObject(I);
      end;
{$ENDIF}
    end;

   { If there's multiple languages, select the previous language, if available }
    if (shUsePreviousLanguage in SetupHeader.Options) and
       (LangForm.LangCombo.Items.Count > 1) then begin
      { do not localize or change the following string }
      PrevLang := GetPreviousData(ExpandConst(SetupHeader.AppId), 'Inno Setup: Language', '');

      if PrevLang <> '' then begin
        for I := 0 to Entries[seLanguage].Count-1 do begin
          if CompareText(PrevLang, PSetupLanguageEntry(Entries[seLanguage][I]).Name) = 0 then begin
            LangForm.LangCombo.ItemIndex := LangForm.LangCombo.Items.IndexOfObject(TObject(I));
            Break;
          end;
        end;
      end;
    end;

    { Select the active language if no previous language was selected }
    if LangForm.LangCombo.ItemIndex = -1 then
      LangForm.LangCombo.ItemIndex := LangForm.LangCombo.Items.IndexOfObject(TObject(ActiveLanguage));

    if LangForm.LangCombo.Items.Count > 1 then begin
      Result := (LangForm.ShowModal = mrOK);
      if Result then begin
        I := LangForm.LangCombo.ItemIndex;
        if I >= 0 then
          SetActiveLanguage(Integer(LangForm.LangCombo.Items.Objects[I]));
      end;
    end
    else begin
      { Don't show language dialog if there aren't multiple languages to choose
        from, which can happen if only one language matches the user's code
        page. }
      Result := True;
    end;
  finally
    LangForm.Free;
  end;
end;

{ TSelectLanguageForm }

constructor TSelectLanguageForm.Create(AOwner: TComponent);
begin
  inherited;

  InitializeFont;
  Center;

  Caption := SetupMessages[msgSelectLanguageTitle];
  SelectLabel.Caption := SetupMessages[msgSelectLanguageLabel];
  OKButton.Caption := SetupMessages[msgButtonOK];
  CancelButton.Caption := SetupMessages[msgButtonCancel];

  IconBitmapImage.Bitmap.Canvas.Brush.Color := Color;
  IconBitmapImage.Bitmap.Width := Application.Icon.Width;
  IconBitmapImage.Bitmap.Height := Application.Icon.Height;
  IconBitmapImage.Bitmap.Canvas.Draw(0, 0, Application.Icon);
  IconBitmapImage.Width := IconBitmapImage.Bitmap.Width;
  IconBitmapImage.Height := IconBitmapImage.Bitmap.Height;
end;

end.
