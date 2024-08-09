object SignToolsForm: TSignToolsForm
  Left = 330
  Top = 188
  BorderIcons = [biSystemMenu]
  Caption = 'Configure Sign Tools'
  ClientHeight = 247
  ClientWidth = 577
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    577
    247)
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 561
    Height = 201
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Sign Tools '
    TabOrder = 0
    DesignSize = (
      561
      201)
    object SignToolsListBox: TListBox
      Left = 8
      Top = 16
      Width = 465
      Height = 177
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
      OnClick = SignToolsListBoxClick
      OnDblClick = SignToolsListBoxDblClick
    end
    object AddButton: TButton
      Left = 480
      Top = 18
      Width = 73
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Add...'
      Default = True
      TabOrder = 1
      OnClick = AddButtonClick
    end
    object RemoveButton: TButton
      Left = 480
      Top = 74
      Width = 73
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Remo&ve'
      TabOrder = 3
      OnClick = RemoveButtonClick
    end
    object EditButton: TButton
      Left = 480
      Top = 46
      Width = 73
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Edit...'
      TabOrder = 2
      OnClick = EditButtonClick
    end
  end
  object OKButton: TButton
    Left = 416
    Top = 217
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 496
    Top = 217
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
