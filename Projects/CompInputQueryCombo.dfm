object InputQueryCombo: TInputQueryCombo
  Left = 330
  Top = 188
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = '...'
  ClientHeight = 73
  ClientWidth = 582
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
    582
    73)
  PixelsPerInch = 96
  TextHeight = 13
  object PromptLabel: TLabel
    Left = 8
    Top = 11
    Width = 265
    Height = 13
    AutoSize = False
    Caption = '...'
    FocusControl = ValueComboBox
  end
  object OKButton: TButton
    Left = 421
    Top = 43
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelButton: TButton
    Left = 501
    Top = 43
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ValueComboBox: TComboBox
    Left = 279
    Top = 8
    Width = 295
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
end
