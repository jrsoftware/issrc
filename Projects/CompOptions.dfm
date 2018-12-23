object OptionsForm: TOptionsForm
  Left = 207
  Top = 173
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 337
  ClientWidth = 589
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    589
    337)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 281
    Height = 221
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
      Top = 56
      Width = 265
      Height = 17
      Caption = 'Automatically sa&ve before compiling'
      TabOrder = 2
    end
    object BackupCheck: TCheckBox
      Left = 8
      Top = 76
      Width = 265
      Height = 17
      Caption = 'Create &backups when saving'
      TabOrder = 3
    end
    object UndoAfterSaveCheck: TCheckBox
      Left = 8
      Top = 96
      Width = 265
      Height = 17
      Caption = 'Allow &Undo after save'
      TabOrder = 4
    end
    object FullPathCheck: TCheckBox
      Left = 8
      Top = 116
      Width = 265
      Height = 17
      Caption = 'Display &full path in title bar'
      TabOrder = 5
    end
    object PauseOnDebuggerExceptionsCheck: TCheckBox
      Left = 8
      Top = 136
      Width = 265
      Height = 17
      Caption = '&Pause on exceptions'
      TabOrder = 6
    end
    object RunAsDifferentUserCheck: TCheckBox
      Left = 8
      Top = 156
      Width = 265
      Height = 17
      Caption = '*'
      TabOrder = 7
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 236
    Width = 281
    Height = 57
    Anchors = [akLeft, akBottom]
    Caption = ' File Associations '
    TabOrder = 1
    ExplicitTop = 216
    object AssocButton: TButton
      Left = 40
      Top = 20
      Width = 201
      Height = 23
      Caption = '&Associate .iss files with this compiler'
      TabOrder = 0
      OnClick = AssocButtonClick
    end
  end
  object GroupBox3: TGroupBox
    Left = 300
    Top = 8
    Width = 281
    Height = 285
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Editor '
    TabOrder = 2
    object AutoCompleteCheck: TCheckBox
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
      Top = 76
      Width = 265
      Height = 17
      Caption = 'U&nderline syntax errors'
      TabOrder = 3
    end
    object CursorPastEOLCheck: TCheckBox
      Left = 8
      Top = 96
      Width = 265
      Height = 17
      Caption = 'Allow cursor to move beyond &end of lines'
      TabOrder = 4
    end
    object WordWrapCheck: TCheckBox
      Left = 8
      Top = 116
      Width = 265
      Height = 17
      Caption = 'W&ord wrap'
      TabOrder = 5
    end
    object UseTabCharacterCheck: TCheckBox
      Left = 8
      Top = 136
      Width = 265
      Height = 17
      Caption = 'Use tab cha&racter'
      TabOrder = 6
    end
    object AutoIndentCheck: TCheckBox
      Left = 8
      Top = 156
      Width = 265
      Height = 17
      Caption = 'Auto &indent mode'
      TabOrder = 7
    end
    object IndentationGuidesCheck: TCheckBox
      Left = 8
      Top = 176
      Width = 265
      Height = 17
      Caption = 'Show indentation &guides'
      TabOrder = 8
    end
    object Label1: TNewStaticText
      Left = 8
      Top = 225
      Width = 55
      Height = 14
      Caption = 'Editor Font:'
      TabOrder = 10
    end
    object FontPanel: TPanel
      Left = 72
      Top = 216
      Width = 121
      Height = 32
      BevelKind = bkFlat
      BevelOuter = bvNone
      Caption = 'AaBbXxZz'
      TabOrder = 11
    end
    object ChangeFontButton: TButton
      Left = 200
      Top = 221
      Width = 73
      Height = 23
      Caption = '&Change...'
      TabOrder = 12
      OnClick = ChangeFontButtonClick
    end
    object Label2: TNewStaticText
      Left = 8
      Top = 259
      Width = 54
      Height = 14
      Caption = '&Tab Width:'
      FocusControl = TabWidthEdit
      TabOrder = 13
    end
    object TabWidthEdit: TEdit
      Left = 72
      Top = 256
      Width = 41
      Height = 21
      TabOrder = 14
      OnChange = TabWidthEditChange
    end
    object GutterLineNumbersCheck: TCheckBox
      Left = 8
      Top = 196
      Width = 265
      Height = 17
      Caption = 'Show &line numbers in gutter'
      TabOrder = 9
    end
    object ColorizeCompilerOutputCheck: TCheckBox
      Left = 8
      Top = 56
      Width = 265
      Height = 17
      Caption = 'Colori&ze "Compiler Output" view'
      TabOrder = 2
    end
  end
  object OKButton: TButton
    Left = 428
    Top = 305
    Width = 73
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    ExplicitTop = 285
  end
  object CancelButton: TButton
    Left = 508
    Top = 305
    Width = 73
    Height = 23
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    ExplicitTop = 285
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
