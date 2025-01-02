unit Setup.SelectLanguageForm;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  "Select Language" form
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Setup.SetupForm, StdCtrls, ExtCtrls, NewStaticText, BitmapImage, BidiCtrls;

type
  TSelectLanguageForm = class(TSetupForm)
    SelectLabel: TNewStaticText;
    LangCombo: TNewComboBox;
    OKButton: TNewButton;
    CancelButton: TNewButton;
    IconBitmapImage: TBitmapImage;
    MainPanel: TPanel;
    Bevel: TBevel;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

function AskForLanguage: Boolean;

implementation

uses
  Shared.Struct, SetupLdrAndSetup.Messages, Shared.SetupMessageIDs,
  Setup.MainFunc, Shared.CommonFunc.Vcl;

{$R *.DFM}

function AskForLanguage: Boolean;
{ Creates and shows the "Select Language" dialog. Returns True and activates
  the selected language if the user clicks OK, or False otherwise. }
var
  LangForm: TSelectLanguageForm;
  I, J: Integer;
  LangEntry: PSetupLanguageEntry;
begin
  LangForm := AppCreateForm(TSelectLanguageForm) as TSelectLanguageForm;
  try
    for I := 0 to Entries[seLanguage].Count-1 do begin
      LangEntry := Entries[seLanguage][I];
      J := LangForm.LangCombo.Items.Add(LangEntry.LanguageName);
      LangForm.LangCombo.Items.Objects[J] := TObject(I);
    end;

   { If there's multiple languages, select the previous language, if available }
    if (shUsePreviousLanguage in SetupHeader.Options) and
       (LangForm.LangCombo.Items.Count > 1) then begin
      { Note: if UsePreviousLanguage is set to "yes" then the compiler does not
        allow AppId to include constants but we should still call ExpandConst
        to handle any '{{'. }
      I := GetPreviousLanguage(ExpandConst(SetupHeader.AppId));
      if I <> -1 then
        LangForm.LangCombo.ItemIndex := LangForm.LangCombo.Items.IndexOfObject(TObject(I));
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

  MainPanel.ParentBackground := False;

  InitializeFont;

  Caption := SetupMessages[msgSelectLanguageTitle];
  SelectLabel.Caption := SetupMessages[msgSelectLanguageLabel];
  OKButton.Caption := SetupMessages[msgButtonOK];
  CancelButton.Caption := SetupMessages[msgButtonCancel];

  IconBitmapImage.InitializeFromIcon(HInstance, 'MAINICON', MainPanel.Color, [32, 48, 64]);

  KeepSizeY := True;
end;

end.
