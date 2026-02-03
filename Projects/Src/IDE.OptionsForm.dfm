object OptionsForm: TOptionsForm
  Left = 207
  Top = 173
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 424
  ClientWidth = 589
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
    589
    424)
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 281
    Height = 334
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Miscellaneous '
    TabOrder = 0
    object StartupCheck: TCheckBox
      Left = 8
      Top = 16
      Width = 265
      Height = 17
      Caption = 'Show &Welcome dialog at startup'
      TabOrder = 0
    end
    object WizardCheck: TCheckBox
      Left = 8
      Top = 36
      Width = 265
      Height = 17
      Caption = 'Use the Inno Setup &Script Wizard'
      TabOrder = 1
    end
    object AutosaveCheck: TCheckBox
      Left = 8
      Top = 116
      Width = 265
      Height = 17
      Caption = 'Automatically sa&ve before compiling'
      TabOrder = 5
    end
    object BackupCheck: TCheckBox
      Left = 8
      Top = 136
      Width = 265
      Height = 17
      Caption = 'Create &backups when saving'
      TabOrder = 6
    end
    object UndoAfterSaveCheck: TCheckBox
      Left = 8
      Top = 156
      Width = 265
      Height = 17
      Caption = 'Allow &Undo after save'
      TabOrder = 7
    end
    object FullPathCheck: TCheckBox
      Left = 8
      Top = 214
      Width = 265
      Height = 17
      Caption = 'Display &full path in title bar'
      TabOrder = 10
    end
    object PauseOnDebuggerExceptionsCheck: TCheckBox
      Left = 8
      Top = 234
      Width = 265
      Height = 17
      Caption = '&Pause on exceptions'
      TabOrder = 11
    end
    object RunAsDifferentUserCheck: TCheckBox
      Left = 8
      Top = 254
      Width = 265
      Height = 17
      Caption = 'Always &launch Setup/Uninstall as administrator'
      TabOrder = 12
    end
    object ColorizeCompilerOutputCheck: TCheckBox
      Left = 8
      Top = 274
      Width = 265
      Height = 17
      Caption = 'Colori&ze "Compiler Output" view'
      TabOrder = 13
    end
    object OpenIncludedFilesCheck: TCheckBox
      Left = 8
      Top = 76
      Width = 265
      Height = 17
      Caption = 'Automatically open #include files'
      TabOrder = 3
    end
    object ShowPreprocessorOutputCheck: TCheckBox
      Left = 8
      Top = 56
      Width = 265
      Height = 17
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
      Left = 72
      Top = 297
      Width = 201
      Height = 21
      Style = csDropDownList
      TabOrder = 15
    end
    object AutoreloadCheck: TCheckBox
      Left = 8
      Top = 176
      Width = 265
      Height = 17
      Caption = 'Automatically reload files'
      TabOrder = 8
    end
    object UndoAfterReloadCheck: TCheckBox
      Left = 8
      Top = 196
      Width = 265
      Height = 17
      Caption = 'Allow Undo after reload'
      TabOrder = 9
    end
    object AutoHideNewIncludedFilesCheck: TCheckBox
      Left = 8
      Top = 96
      Width = 265
      Height = 17
      Caption = 'Keep new #include files closed'
      TabOrder = 4
    end
  end
  object GroupBox3: TGroupBox
    Left = 300
    Top = 8
    Width = 281
    Height = 372
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Editor '
    TabOrder = 1
    object AutoAutoCompleteCheck: TCheckBox
      Left = 8
      Top = 16
      Width = 265
      Height = 17
      Caption = 'Invoke autoco&mplete automatically'
      TabOrder = 0
    end
    object UseSynHighCheck: TCheckBox
      Left = 8
      Top = 36
      Width = 265
      Height = 17
      Caption = 'Use syntax &highlighting'
      TabOrder = 1
    end
    object UnderlineErrorsCheck: TCheckBox
      Left = 8
      Top = 56
      Width = 265
      Height = 17
      Caption = 'U&nderline syntax errors'
      TabOrder = 2
    end
    object CursorPastEOLCheck: TCheckBox
      Left = 8
      Top = 76
      Width = 265
      Height = 17
      Caption = 'Allow cursor to move beyond &end of lines'
      TabOrder = 3
    end
    object UseFoldingCheck: TCheckBox
      Left = 8
      Top = 96
      Width = 265
      Height = 17
      Caption = 'Enable section f&olding'
      TabOrder = 4
    end
    object UseTabCharacterCheck: TCheckBox
      Left = 8
      Top = 116
      Width = 265
      Height = 17
      Caption = 'Use tab cha&racter'
      TabOrder = 5
    end
    object AutoIndentCheck: TCheckBox
      Left = 8
      Top = 136
      Width = 130
      Height = 17
      Caption = 'Auto &indent mode'
      TabOrder = 6
    end
    object IndentationGuidesCheck: TCheckBox
      Left = 8
      Top = 156
      Width = 265
      Height = 17
      Caption = 'Show indentation &guides'
      TabOrder = 8
    end
    object Label1: TNewStaticText
      Left = 8
      Top = 306
      Width = 25
      Height = 14
      Caption = 'Font:'
      TabOrder = 16
    end
    object FontPanel: TPanel
      Left = 72
      Top = 297
      Width = 121
      Height = 32
      BevelKind = bkFlat
      BevelOuter = bvNone
      Caption = 'AaBbXxZz'
      TabOrder = 17
    end
    object ChangeFontButton: TButton
      Left = 200
      Top = 302
      Width = 73
      Height = 23
      Caption = '&Change...'
      TabOrder = 18
      OnClick = ChangeFontButtonClick
    end
    object Label2: TNewStaticText
      Left = 8
      Top = 340
      Width = 54
      Height = 14
      Caption = '&Tab Width:'
      FocusControl = TabWidthEdit
      TabOrder = 19
    end
    object TabWidthEdit: TEdit
      Left = 72
      Top = 337
      Width = 41
      Height = 21
      TabOrder = 20
      OnChange = TabWidthEditChange
    end
    object GutterLineNumbersCheck: TCheckBox
      Left = 8
      Top = 176
      Width = 265
      Height = 17
      Caption = 'Show &line numbers in gutter'
      TabOrder = 9
    end
    object Label4: TNewStaticText
      Left = 8
      Top = 272
      Width = 37
      Height = 14
      Caption = 'T&heme:'
      FocusControl = ThemeComboBox
      TabOrder = 14
    end
    object ThemeComboBox: TComboBox
      Left = 72
      Top = 268
      Width = 201
      Height = 21
      Style = csDropDownList
      TabOrder = 15
    end
    object HighlightSelTextOccurrencesCheck: TCheckBox
      Left = 8
      Top = 196
      Width = 265
      Height = 17
      Caption = 'Highlight occurrences of current selection'
      TabOrder = 10
    end
    object HighlightWordAtCursorOccurrencesCheck: TCheckBox
      Left = 8
      Top = 216
      Width = 265
      Height = 17
      Caption = 'Highlight occurrences of current wor&d'
      TabOrder = 11
    end
    object Label5: TNewStaticText
      Left = 8
      Top = 243
      Width = 27
      Height = 14
      Caption = 'Ke&ys:'
      FocusControl = MemoKeyMappingComboBox
      TabOrder = 12
    end
    object MemoKeyMappingComboBox: TComboBox
      Left = 72
      Top = 239
      Width = 201
      Height = 21
      Style = csDropDownList
      TabOrder = 13
    end
    object ShowWhiteSpaceCheck: TCheckBox
      Left = 120
      Top = 339
      Width = 120
      Height = 17
      Caption = 'Show whitespace'
      TabOrder = 21
    end
    object SmartHomeCheck: TCheckBox
      Left = 143
      Top = 136
      Width = 130
      Height = 17
      Caption = 'Smart Home key'
      TabOrder = 7
    end
  end
  object OKButton: TButton
    Left = 420
    Top = 392
    Width = 73
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 500
    Top = 392
    Width = 73
    Height = 23
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object AssocButton: TButton
    Left = 16
    Top = 392
    Width = 201
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '&Associate .iss files with this compiler'
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
