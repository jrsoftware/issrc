object SignToolsForm: TSignToolsForm
  Left = 330
  Top = 188
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Sign Tools'
  ClientHeight = 247
  ClientWidth = 377
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 361
    Height = 201
    Caption = ' Sign Tools '
    TabOrder = 0
    object SignToolsListBox: TListBox
      Left = 8
      Top = 16
      Width = 265
      Height = 177
      ItemHeight = 13
      TabOrder = 0
      OnClick = SignToolsListBoxClick
    end
    object AddButton: TButton
      Left = 280
      Top = 18
      Width = 73
      Height = 23
      Caption = '&Add...'
      Default = True
      TabOrder = 1
      OnClick = AddButtonClick
    end
    object RemoveButton: TButton
      Left = 280
      Top = 74
      Width = 73
      Height = 23
      Caption = 'Remo&ve'
      TabOrder = 3
      OnClick = RemoveButtonClick
    end
    object EditButton: TButton
      Left = 280
      Top = 46
      Width = 73
      Height = 23
      Caption = '&Edit...'
      TabOrder = 2
      OnClick = EditButtonClick
    end
  end
  object OKButton: TButton
    Left = 216
    Top = 217
    Width = 73
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 296
    Top = 217
    Width = 73
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
