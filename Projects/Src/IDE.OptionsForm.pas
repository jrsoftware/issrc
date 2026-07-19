unit IDE.OptionsForm;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE Options form
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  NewGroupBox, NewStaticText,
  IDE.IDEForm;

type
  TOptionsFormDropDown = (odNone, odMemoKeyMapping, odLanguage);

  TOptionsForm = class(TIDEForm)
    OKButton: TButton;
    CancelButton: TButton;
    GroupBox1: TNewGroupBox;
    BackupCheck: TCheckBox;
    AssocButton: TButton;
    StartupCheck: TCheckBox;
    WizardCheck: TCheckBox;
    GroupBox3: TNewGroupBox;
    ChangeFontButton: TButton;
    FontPanel: TPanel;
    Label1: TNewStaticText;
    FontDialog: TFontDialog;
    UseSynHighCheck: TCheckBox;
    FullPathCheck: TCheckBox;
    CursorPastEOLCheck: TCheckBox;
    UndoAfterSaveCheck: TCheckBox;
    TabWidthEdit: TEdit;
    Label2: TNewStaticText;
    PauseOnDebuggerExceptionsCheck: TCheckBox;
    RunAsDifferentUserCheck: TCheckBox;
    AutosaveCheck: TCheckBox;
    UseFoldingCheck: TCheckBox;
    AutoIndentCheck: TCheckBox;
    IndentationGuidesCheck: TCheckBox;
    UseTabCharacterCheck: TCheckBox;
    AutoAutoCompleteCheck: TCheckBox;
    UnderlineErrorsCheck: TCheckBox;
    GutterLineNumbersCheck: TCheckBox;
    ColorizeCompilerOutputCheck: TCheckBox;
    Label3: TNewStaticText;
    KeyMappingComboBox: TComboBox;
    Label4: TNewStaticText;
    ThemeComboBox: TComboBox;
    OpenIncludedFilesCheck: TCheckBox;
    ShowPreprocessorOutputCheck: TCheckBox;
    HighlightWordAtCursorOccurrencesCheck: TCheckBox;
    HighlightSelTextOccurrencesCheck: TCheckBox;
    Label5: TNewStaticText;
    MemoKeyMappingComboBox: TComboBox;
    ShowWhiteSpaceCheck: TCheckBox;
    AutoreloadCheck: TCheckBox;
    UndoAfterReloadCheck: TCheckBox;
    AutoHideNewIncludedFilesCheck: TCheckBox;
    SmartHomeCheck: TCheckBox;
    Label6: TNewStaticText;
    LanguageComboBox: TComboBox;
    procedure AssocButtonClick(Sender: TObject);
    procedure ChangeFontButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TabWidthEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    class var
      FDropDownOnNextShow: TOptionsFormDropDown;
    var
      {}
  public
    class property DropDownOnNextShow: TOptionsFormDropDown write FDropDownOnNextShow;
  end;

implementation

uses
  Shared.CommonFunc.Vcl, Shared.CommonFunc, IDE.HelperFunc, IDE.FileAssocFunc, IDE.Messages, IDE.LocalizeFunc;

{$R *.DFM}

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  { Finish localization }
  OpenIncludedFilesCheck.Caption := LFmtMessage(OpenIncludedFilesCheck.Caption, ['#include']);
  AutoHideNewIncludedFilesCheck.Caption := LFmtMessage(AutoHideNewIncludedFilesCheck.Caption, ['#include']);
  AssocButton.Caption := LFmtMessage(AssocButton.Caption, [SLitIssExt]);
  AssocButton.Width := CalculateButtonWidth([AssocButton.Caption]);
  const W = SizeBottomButtons(OKButton, CancelButton, [ChangeFontButton]);
  const Diff = W - ChangeFontButton.Width;
  ChangeFontButton.Left := ChangeFontButton.Left - Diff;
  ChangeFontButton.Width := W;
  FontPanel.Width := FontPanel.Width - Diff;

  { Order must match IDE.HelperFunc.TKeyMappingType }
  KeyMappingComboBox.Items.Add(LFmtMessage(SOptionsKeyMappingDelphi));
  KeyMappingComboBox.Items.Add(LFmtMessage(SOptionsKeyMappingVisualStudio));

  { Order must match TIDEScintKeyMappingType }
  MemoKeyMappingComboBox.Items.Add(LFmtMessage(SOptionsMemoKeyMappingDefault));
  MemoKeyMappingComboBox.Items.Add(LFmtMessage(SOptionsMemoKeyMappingVSCode));

  { Order must match TThemeType }
  ThemeComboBox.Items.Add(LFmtMessage(SOptionsThemeLight));
  ThemeComboBox.Items.Add(LFmtMessage(SOptionsThemeDark));
  ThemeComboBox.Items.Add(LFmtMessage(SOptionsThemeClassic));

  { Order must match TIDELanguage }
  LanguageComboBox.Items.Add('English');
  LanguageComboBox.Items.Add(#$010C'e'#$0161'tina');
  LanguageComboBox.Items.Add('Nederlands');
  LanguageComboBox.Items.Add('Deutsch');
  LanguageComboBox.Items.Add(#$65E5#$672C#$8A9E);
end;

procedure TOptionsForm.FormShow(Sender: TObject);
begin
  case FDropDownOnNextShow of
    odMemoKeyMapping: begin
      ActiveControl := MemoKeyMappingComboBox;
      MemoKeyMappingComboBox.DroppedDown := True;
    end;
    odLanguage: begin
      ActiveControl := LanguageComboBox;
      LanguageComboBox.DroppedDown := True;
    end;
  end;
  FDropDownOnNextShow := odNone;
end;

procedure TOptionsForm.AssocButtonClick(Sender: TObject);
const
  SuccessMessages: array [Boolean] of String = (SAssocSuccessCurrentUser, SAssocSuccessAllUsers);
var
  AllUsers: Boolean;
begin
  if RegisterISSFileAssociation(True, AllUsers) then
    MsgBox(LFmtMessage(SuccessMessages[AllUsers], [SLitIssExt, NewParamStr(0)]),
      LFmtMessage(SAssocTitle), mbInformation, MB_OK);
end;

procedure TOptionsForm.ChangeFontButtonClick(Sender: TObject);
begin
  FontDialog.Font.Assign(FontPanel.Font);
  if FontDialog.Execute then
    FontPanel.Font.Assign(FontDialog.Font);
end;

procedure TOptionsForm.TabWidthEditChange(Sender: TObject);
begin
  OKButton.Enabled := StrToIntDef(TabWidthEdit.Text, 0) > 0;
end;

end.
