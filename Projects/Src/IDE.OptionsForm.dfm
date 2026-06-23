object OptionsForm: TOptionsForm
  Left = 207
  Top = 173
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 453
  ClientWidth = 669
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    669
    453)
  TextHeight = 13
  object GroupBox1: TNewGroupBox
    Left = 8
    Top = 8
    Width = 321
    Height = 392
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Miscellaneous '
    TabOrder = 0
    object StartupCheck: TCheckBox
      Left = 8
      Top = 16
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show &Welcome dialog at startup'
      TabOrder = 0
    end
    object WizardCheck: TCheckBox
      Left = 8
      Top = 36
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Use the Inno Setup &Script Wizard'
      TabOrder = 1
    end
    object AutosaveCheck: TCheckBox
      Left = 8
      Top = 116
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Automatically sa&ve before compiling'
      TabOrder = 5
    end
    object BackupCheck: TCheckBox
      Left = 8
      Top = 136
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Create &backups when saving'
      TabOrder = 6
    end
    object UndoAfterSaveCheck: TCheckBox
      Left = 8
      Top = 156
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Allow &Undo after save'
      TabOrder = 7
    end
    object FullPathCheck: TCheckBox
      Left = 8
      Top = 214
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Display &full path in title bar'
      TabOrder = 10
    end
    object PauseOnDebuggerExceptionsCheck: TCheckBox
      Left = 8
      Top = 234
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Pause on exceptions'
      TabOrder = 11
    end
    object RunAsDifferentUserCheck: TCheckBox
      Left = 8
      Top = 254
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Launch Setup/Uninstall as administrator'
      TabOrder = 12
    end
    object ColorizeCompilerOutputCheck: TCheckBox
      Left = 8
      Top = 274
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Colori&ze "Compiler Output" view'
      TabOrder = 13
    end
    object OpenIncludedFilesCheck: TCheckBox
      Left = 8
      Top = 76
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Automatically open %1 files'
      TabOrder = 3
    end
    object ShowPreprocessorOutputCheck: TCheckBox
      Left = 8
      Top = 56
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Automatically show preprocessor output'
      TabOrder = 2
    end
    object Label3: TNewStaticText
      Left = 8
      Top = 301
      Width = 56
      Height = 14
      Caption = 'Menu &keys:'
      FocusControl = KeyMappingComboBox
      TabOrder = 14
    end
    object KeyMappingComboBox: TComboBox
      Left = 104
      Top = 297
      Width = 209
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 15
    end
    object AutoreloadCheck: TCheckBox
      Left = 8
      Top = 176
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Automatically reload files'
      TabOrder = 8
    end
    object UndoAfterReloadCheck: TCheckBox
      Left = 8
      Top = 196
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Allow Undo after reload'
      TabOrder = 9
    end
    object AutoHideNewIncludedFilesCheck: TCheckBox
      Left = 8
      Top = 96
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Keep new %1 files closed'
      TabOrder = 4
    end
    object Label6: TNewStaticText
      Left = 8
      Top = 330
      Width = 52
      Height = 14
      Caption = 'Language:'
      FocusControl = LanguageComboBox
      TabOrder = 16
    end
    object LanguageComboBox: TComboBox
      Left = 104
      Top = 324
      Width = 209
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 17
    end
  end
  object GroupBox3: TNewGroupBox
    Left = 340
    Top = 8
    Width = 321
    Height = 401
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Editor '
    TabOrder = 1
    object AutoAutoCompleteCheck: TCheckBox
      Left = 8
      Top = 16
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Invoke autoco&mplete automatically'
      TabOrder = 0
    end
    object UseSynHighCheck: TCheckBox
      Left = 8
      Top = 36
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Use syntax &highlighting'
      TabOrder = 1
    end
    object UnderlineErrorsCheck: TCheckBox
      Left = 8
      Top = 56
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'U&nderline syntax errors'
      TabOrder = 2
    end
    object CursorPastEOLCheck: TCheckBox
      Left = 8
      Top = 76
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Allow cursor to move beyond &end of lines'
      TabOrder = 3
    end
    object UseFoldingCheck: TCheckBox
      Left = 8
      Top = 96
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Enable section f&olding'
      TabOrder = 4
    end
    object UseTabCharacterCheck: TCheckBox
      Left = 8
      Top = 116
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Use tab cha&racter'
      TabOrder = 5
    end
    object AutoIndentCheck: TCheckBox
      Left = 8
      Top = 136
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Auto &indent mode'
      TabOrder = 6
    end
    object IndentationGuidesCheck: TCheckBox
      Left = 8
      Top = 176
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show indentation &guides'
      TabOrder = 8
    end
    object Label1: TNewStaticText
      Left = 8
      Top = 335
      Width = 25
      Height = 14
      Anchors = [akLeft, akBottom]
      Caption = 'Font:'
      TabOrder = 16
    end
    object FontPanel: TPanel
      Left = 104
      Top = 326
      Width = 129
      Height = 32
      Anchors = [akLeft, akRight, akBottom]
      BevelKind = bkFlat
      BevelOuter = bvNone
      Caption = 'AaBbXxZz'
      TabOrder = 17
    end
    object ChangeFontButton: TButton
      Left = 240
      Top = 331
      Width = 73
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = '&Change...'
      TabOrder = 18
      OnClick = ChangeFontButtonClick
    end
    object Label2: TNewStaticText
      Left = 8
      Top = 369
      Width = 54
      Height = 14
      Anchors = [akLeft, akBottom]
      Caption = '&Tab Width:'
      FocusControl = TabWidthEdit
      TabOrder = 19
    end
    object TabWidthEdit: TEdit
      Left = 104
      Top = 366
      Width = 50
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 20
      OnChange = TabWidthEditChange
    end
    object GutterLineNumbersCheck: TCheckBox
      Left = 8
      Top = 196
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show &line numbers in gutter'
      TabOrder = 9
    end
    object Label4: TNewStaticText
      Left = 8
      Top = 301
      Width = 37
      Height = 14
      Anchors = [akLeft, akBottom]
      Caption = 'T&heme:'
      FocusControl = ThemeComboBox
      TabOrder = 14
    end
    object ThemeComboBox: TComboBox
      Left = 104
      Top = 297
      Width = 209
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 15
    end
    object HighlightSelTextOccurrencesCheck: TCheckBox
      Left = 8
      Top = 216
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Highlight occurrences of current selection'
      TabOrder = 10
    end
    object HighlightWordAtCursorOccurrencesCheck: TCheckBox
      Left = 8
      Top = 236
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Highlight occurrences of current wor&d'
      TabOrder = 11
    end
    object Label5: TNewStaticText
      Left = 8
      Top = 272
      Width = 27
      Height = 14
      Anchors = [akLeft, akBottom]
      Caption = 'Ke&ys:'
      FocusControl = MemoKeyMappingComboBox
      TabOrder = 12
    end
    object MemoKeyMappingComboBox: TComboBox
      Left = 104
      Top = 268
      Width = 209
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 13
    end
    object ShowWhiteSpaceCheck: TCheckBox
      Left = 160
      Top = 368
      Width = 153
      Height = 17
      Anchors = [akRight, akBottom]
      Caption = 'Show whitespace'
      TabOrder = 21
    end
    object SmartHomeCheck: TCheckBox
      Left = 8
      Top = 156
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Smart Home key'
      TabOrder = 7
    end
  end
  object OKButton: TButton
    Left = 500
    Top = 421
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 580
    Top = 421
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object AssocButton: TButton
    Left = 16
    Top = 421
    Width = 201
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '&Associate .%1 files with this compiler'
    TabOrder = 4
    OnClick = AssocButtonClick
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdForceFontExist]
    Left = 8
    Top = 284
  end
end
