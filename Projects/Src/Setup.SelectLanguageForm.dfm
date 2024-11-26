object SelectLanguageForm: TSelectLanguageForm
  Left = 200
  Top = 108
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'SelectLanguageForm'
  ClientHeight = 140
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
    140)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TBevel
    Left = 0
    Top = 97
    Width = 297
    Height = 1
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object CancelButton: TNewButton
    Left = 214
    Top = 108
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '*'
    ModalResult = 2
    TabOrder = 2
  end
  object OKButton: TNewButton
    Left = 133
    Top = 108
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '*'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 297
    Height = 97
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 0
    DesignSize = (
      297
      97)
    object IconBitmapImage: TBitmapImage
      Left = 8
      Top = 8
      Width = 34
      Height = 34
    end
    object LangCombo: TNewComboBox
      Left = 56
      Top = 56
      Width = 233
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
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
end
