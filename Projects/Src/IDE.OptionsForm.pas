unit IDE.OptionsForm;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE Options form
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UIStateForm, StdCtrls, ExtCtrls, NewStaticText;

type
  TOptionsForm = class(TUIStateForm)
    OKButton: TButton;
    CancelButton: TButton;
    GroupBox1: TGroupBox;
    BackupCheck: TCheckBox;
    GroupBox2: TGroupBox;
    AssocButton: TButton;
    StartupCheck: TCheckBox;
    WizardCheck: TCheckBox;
    GroupBox3: TGroupBox;
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
    procedure AssocButtonClick(Sender: TObject);
    procedure ChangeFontButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TabWidthEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    class var
      FDropDownMemoKeyMappingComboBoxOnNextShow: Boolean;
    var
      {}
  public
    class property DropDownMemoKeyMappingComboBoxOnNextShow: Boolean write FDropDownMemoKeyMappingComboBoxOnNextShow;
  end;

implementation

uses
  Shared.CommonFunc.Vcl, Shared.CommonFunc, IDE.HelperFunc, IDE.FileAssocFunc;

{$R *.DFM}

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  InitFormFont(Self);

  { Order must match CompFunc.TKeyMappingType }
  KeyMappingComboBox.Items.Add('Classic');
  KeyMappingComboBox.Items.Add('Visual Studio / Visual Studio Code');

  { Order must match TIDEScintKeyMappingType }
  MemoKeyMappingComboBox.Items.Add('Classic / Visual Studio');
  MemoKeyMappingComboBox.Items.Add('Visual Studio Code');

  { Order must match TThemeType }
  ThemeComboBox.Items.Add('Light');
  ThemeComboBox.Items.Add('Dark');
  ThemeComboBox.Items.Add('Classic');
end;

procedure TOptionsForm.FormShow(Sender: TObject);
begin
  if FDropDownMemoKeyMappingComboBoxOnNextShow then begin
    ActiveControl := MemoKeyMappingComboBox;
    MemoKeyMappingComboBox.DroppedDown := True;
    FDropDownMemoKeyMappingComboBoxOnNextShow := False;
  end;
end;

procedure TOptionsForm.AssocButtonClick(Sender: TObject);
const
  UserStrings: array [Boolean] of String = ('the current user', 'all users');
var
  AllUsers: Boolean;
begin
  if RegisterISSFileAssociation(True, AllUsers) then
    MsgBox('The .iss extension was successfully associated for ' + UserStrings[AllUsers] + ' with:'#13#10 + NewParamStr(0),
      'Associate', mbInformation, MB_OK);
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
