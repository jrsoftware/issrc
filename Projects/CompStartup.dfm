object StartupForm: TStartupForm
  Left = 328
  Top = 174
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Welcome'
  ClientHeight = 329
  ClientWidth = 389
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 228
    Top = 297
    Width = 73
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 308
    Top = 297
    Width = 73
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 373
    Height = 61
    Caption = ' New file '
    TabOrder = 0
    object NewImage: TImage
      Left = 12
      Top = 16
      Width = 16
      Height = 16
      AutoSize = True
      Transparent = True
    end
    object EmptyRadioButton: TRadioButton
      Left = 40
      Top = 16
      Width = 321
      Height = 17
      Caption = 'Create a new &empty script file'
      TabOrder = 0
      OnClick = RadioButtonClick
      OnDblClick = DblClick_
    end
    object WizardRadioButton: TRadioButton
      Left = 40
      Top = 36
      Width = 321
      Height = 17
      Caption = 'Create a new script file using the &Script Wizard'
      TabOrder = 1
      OnClick = RadioButtonClick
      OnDblClick = DblClick_
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 76
    Width = 373
    Height = 213
    Caption = ' Open file '
    TabOrder = 1
    object OpenImage: TImage
      Left = 12
      Top = 19
      Width = 16
      Height = 16
      AutoSize = True
      Transparent = True
    end
    object OpenRadioButton: TRadioButton
      Left = 40
      Top = 20
      Width = 321
      Height = 17
      Caption = 'Open an e&xisting script file'
      TabOrder = 0
      OnClick = RadioButtonClick
      OnDblClick = DblClick_
    end
    object OpenListBox: TListBox
      Left = 44
      Top = 44
      Width = 317
      Height = 157
      ItemHeight = 13
      TabOrder = 1
      OnClick = OpenListBoxClick
      OnDblClick = DblClick_
    end
  end
  object StartupCheck: TCheckBox
    Left = 8
    Top = 300
    Width = 189
    Height = 17
    TabStop = False
    Caption = '&Don'#39't show this dialog again'
    TabOrder = 4
  end
end
