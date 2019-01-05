object SelectLanguageForm: TSelectLanguageForm
  Left = 200
  Top = 108
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'SelectLanguageForm'
  ClientHeight = 125
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  DesignSize = (
    297
    125)
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
    Anchors = [akRight, akBottom]
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
    Anchors = [akRight, akBottom]
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
    Anchors = [akTop, akRight]
    DropDownCount = 16
    Sorted = True
    TabOrder = 1
  end
  object SelectLabel: TNewStaticText
    Left = 56
    Top = 8
    Width = 233
    Height = 39
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '*'
    TabOrder = 0
    WordWrap = True
  end
end
