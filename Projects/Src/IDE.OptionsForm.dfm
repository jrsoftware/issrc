object OptionsForm: TOptionsForm
  Left = 207
  Top = 173
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 528
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
    528)
  TextHeight = 13
  object GroupBox1: TNewGroupBox
    Left = 8
    Top = 8
    Width = 321
    Height = 360
    Caption = ' Miscellaneous '
    TabOrder = 0
    object StartupCheck: TCheckBox
      Left = 8
      Top = 20
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show &Welcome dialog at startup'
      TabOrder = 0
    end
    object WizardCheck: TCheckBox
      Left = 8
      Top = 40
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Use the Inno Setup &Script Wizard'
      TabOrder = 1
    end
    object AutosaveCheck: TCheckBox
      Left = 8
      Top = 120
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Automatically sa&ve before compiling'
      TabOrder = 5
    end
    object BackupCheck: TCheckBox
      Left = 8
      Top = 140
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Create &backups when saving'
      TabOrder = 6
    end
    object UndoAfterSaveCheck: TCheckBox
      Left = 8
      Top = 160
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Allow &Undo after save'
      TabOrder = 7
    end
    object FullPathCheck: TCheckBox
      Left = 8
      Top = 218
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Display &full path in title bar'
      TabOrder = 10
    end
    object PauseOnDebuggerExceptionsCheck: TCheckBox
      Left = 8
      Top = 238
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Pause on exceptions'
      TabOrder = 11
    end
    object RunAsDifferentUserCheck: TCheckBox
      Left = 8
      Top = 258
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Launch Setup/Uninstall as administrator'
      TabOrder = 12
    end
    object ColorizeCompilerOutputCheck: TCheckBox
      Left = 8
      Top = 278
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Colori&ze "Compiler Output" view'
      TabOrder = 13
    end
    object OpenIncludedFilesCheck: TCheckBox
      Left = 8
      Top = 80
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Automatically open %1 files'
      TabOrder = 3
    end
    object ShowPreprocessorOutputCheck: TCheckBox
      Left = 8
      Top = 60
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Automatically show preprocessor output'
      TabOrder = 2
    end
    object Label3: TNewStaticText
      Left = 8
      Top = 305
      Width = 56
      Height = 14
      Caption = 'Menu &keys:'
      FocusControl = KeyMappingComboBox
      TabOrder = 14
    end
    object KeyMappingComboBox: TComboBox
      Left = 104
      Top = 301
      Width = 209
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      TabOrder = 15
    end
    object AutoreloadCheck: TCheckBox
      Left = 8
      Top = 180
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Automatically reload files'
      TabOrder = 8
    end
    object UndoAfterReloadCheck: TCheckBox
      Left = 8
      Top = 200
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Allow Undo after reload'
      TabOrder = 9
    end
    object AutoHideNewIncludedFilesCheck: TCheckBox
      Left = 8
      Top = 100
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Keep new %1 files closed'
      TabOrder = 4
    end
    object Label6: TNewStaticText
      Left = 8
      Top = 334
      Width = 52
      Height = 14
      Caption = 'Language:'
      FocusControl = LanguageComboBox
      TabOrder = 16
    end
    object LanguageComboBox: TComboBox
      Left = 104
      Top = 328
      Width = 209
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      DropDownCount = 16
      TabOrder = 17
    end
  end
  object GroupBox2: TNewGroupBox
    Left = 8
    Top = 376
    Width = 321
    Height = 108
    Caption = ' Inspector '
    TabOrder = 1
    object InspectorShowAllKnownDirectivesCheck: TCheckBox
      Left = 8
      Top = 20
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show all known directives'
      TabOrder = 0
    end
    object InspectorFollowCaretCheck: TCheckBox
      Left = 8
      Top = 40
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Follow cursor'
      TabOrder = 1
    end
    object InspectorQuoteNewDirectiveValuesCheck: TCheckBox
      Left = 8
      Top = 60
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Quote new directive values'
      TabOrder = 2
    end
    object InspectorQuoteNewParameterValuesCheck: TCheckBox
      Left = 8
      Top = 80
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Quote new parameter values'
      TabOrder = 3
    end
  end
  object GroupBox3: TNewGroupBox
    Left = 340
    Top = 8
    Width = 321
    Height = 405
    Caption = ' Editor '
    TabOrder = 2
    object AutoAutoCompleteCheck: TCheckBox
      Left = 8
      Top = 20
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Invoke autoco&mplete automatically'
      TabOrder = 0
    end
    object UseSynHighCheck: TCheckBox
      Left = 8
      Top = 40
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Use syntax &highlighting'
      TabOrder = 1
    end
    object UnderlineErrorsCheck: TCheckBox
      Left = 8
      Top = 60
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'U&nderline syntax errors'
      TabOrder = 2
    end
    object CursorPastEOLCheck: TCheckBox
      Left = 8
      Top = 80
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Allow cursor to move beyond &end of lines'
      TabOrder = 3
    end
    object UseFoldingCheck: TCheckBox
      Left = 8
      Top = 100
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Enable section f&olding'
      TabOrder = 4
    end
    object UseTabCharacterCheck: TCheckBox
      Left = 8
      Top = 120
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Use tab cha&racter'
      TabOrder = 5
    end
    object AutoIndentCheck: TCheckBox
      Left = 8
      Top = 140
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Auto &indent mode'
      TabOrder = 6
    end
    object IndentationGuidesCheck: TCheckBox
      Left = 8
      Top = 180
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show indentation &guides'
      TabOrder = 8
    end
    object Label1: TNewStaticText
      Left = 8
      Top = 339
      Width = 25
      Height = 14
      Caption = 'Font:'
      TabOrder = 16
    end
    object FontPanel: TPanel
      Left = 104
      Top = 330
      Width = 129
      Height = 32
      Anchors = [akTop, akRight]
      BevelKind = bkFlat
      BevelOuter = bvNone
      Caption = 'AaBbXxZz'
      TabOrder = 17
    end
    object ChangeFontButton: TButton
      Left = 240
      Top = 335
      Width = 73
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Change...'
      TabOrder = 18
      OnClick = ChangeFontButtonClick
    end
    object Label2: TNewStaticText
      Left = 8
      Top = 373
      Width = 54
      Height = 14
      Caption = '&Tab Width:'
      FocusControl = TabWidthEdit
      TabOrder = 19
    end
    object TabWidthEdit: TEdit
      Left = 104
      Top = 370
      Width = 30
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 20
      OnChange = TabWidthEditChange
    end
    object GutterLineNumbersCheck: TCheckBox
      Left = 8
      Top = 200
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show &line numbers in gutter'
      TabOrder = 9
    end
    object Label4: TNewStaticText
      Left = 8
      Top = 305
      Width = 37
      Height = 14
      Caption = 'T&heme:'
      FocusControl = ThemeComboBox
      TabOrder = 14
    end
    object ThemeComboBox: TComboBox
      Left = 104
      Top = 301
      Width = 209
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      TabOrder = 15
    end
    object HighlightSelTextOccurrencesCheck: TCheckBox
      Left = 8
      Top = 220
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Highlight occurrences of current selection'
      TabOrder = 10
    end
    object HighlightWordAtCursorOccurrencesCheck: TCheckBox
      Left = 8
      Top = 240
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Highlight occurrences of current wor&d'
      TabOrder = 11
    end
    object Label5: TNewStaticText
      Left = 8
      Top = 276
      Width = 27
      Height = 14
      Caption = 'Ke&ys:'
      FocusControl = MemoKeyMappingComboBox
      TabOrder = 12
    end
    object MemoKeyMappingComboBox: TComboBox
      Left = 104
      Top = 272
      Width = 209
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      TabOrder = 13
    end
    object ShowWhiteSpaceCheck: TCheckBox
      Left = 140
      Top = 372
      Width = 173
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Show whitespace'
      TabOrder = 21
    end
    object SmartHomeCheck: TCheckBox
      Left = 8
      Top = 160
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Smart Home key'
      TabOrder = 7
    end
  end
  object OKButton: TButton
    Left = 500
    Top = 496
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object CancelButton: TButton
    Left = 580
    Top = 496
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object AssocButton: TButton
    Left = 16
    Top = 496
    Width = 201
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '&Associate .%1 files with this compiler'
    TabOrder = 5
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
