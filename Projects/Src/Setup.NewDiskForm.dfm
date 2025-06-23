object NewDiskForm: TNewDiskForm
  Left = 226
  Top = 162
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = '*'
  ClientHeight = 169
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PopupMode = pmAuto
  Scaled = False
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    377
    169)
  TextHeight = 13
  object DiskBitmapImage: TBitmapImage
    Left = 8
    Top = 10
    Width = 48
    Height = 48
  end
  object CancelButton: TNewButton
    Left = 296
    Top = 137
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '*'
    ModalResult = 2
    TabOrder = 5
  end
  object OKButton: TNewButton
    Left = 216
    Top = 137
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '*'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object BrowseButton: TNewButton
    Left = 296
    Top = 95
    Width = 73
    Height = 23
    Anchors = [akTop, akRight]
    Caption = '*'
    TabOrder = 3
    OnClick = BrowseButtonClick
  end
  object PathEdit: TEdit
    Left = 8
    Top = 96
    Width = 281
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object PathLabel: TNewStaticText
    Left = 8
    Top = 80
    Width = 5
    Height = 14
    Caption = '*'
    FocusControl = PathEdit
    TabOrder = 1
  end
  object SelectDiskLabel: TNewStaticText
    Left = 72
    Top = 8
    Width = 297
    Height = 72
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '*'
    ShowAccelChar = False
    TabOrder = 0
    WordWrap = True
  end
end
