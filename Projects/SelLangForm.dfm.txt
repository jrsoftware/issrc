object SelectLanguageForm: TSelectLanguageForm
  Left = 200
  Top = 108
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'SelectLanguageForm'
  ClientHeight = 125
  ClientWidth = 297
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object IconBitmapImage: TBitmapImage
    Left = 8
    Top = 8
    Width = 32
    Height = 32
  end
  object CancelButton: TNewButton
    Left = 214
    Top = 93
    Width = 75
    Height = 23
    Cancel = True
    Caption = '*'
    ModalResult = 2
    TabOrder = 3
  end
  object OKButton: TNewButton
    Left = 133
    Top = 93
    Width = 75
    Height = 23
    Caption = '*'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object LangCombo: TNewComboBox
    Left = 56
    Top = 56
    Width = 233
    Height = 21
    Style = csDropDownList
    DropDownCount = 16
    ItemHeight = 13
    Sorted = True
    TabOrder = 1
  end
  object SelectLabel: TNewStaticText
    Left = 56
    Top = 8
    Width = 233
    Height = 39
    AutoSize = False
    Caption = '*'
    TabOrder = 0
    WordWrap = True
  end
end
