object UninstSharedFileForm: TUninstSharedFileForm
  Left = 200
  Top = 108
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'UninstSharedFileForm'
  ClientHeight = 225
  ClientWidth = 397
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object NoToAllButton: TNewButton
    Left = 283
    Top = 189
    Width = 75
    Height = 23
    Caption = '*'
    ModalResult = 9
    TabOrder = 3
  end
  object NoButton: TNewButton
    Left = 202
    Top = 189
    Width = 75
    Height = 23
    Caption = '*'
    ModalResult = 7
    TabOrder = 2
  end
  object YesToAllButton: TNewButton
    Left = 121
    Top = 189
    Width = 75
    Height = 23
    Caption = '*'
    ModalResult = 10
    TabOrder = 1
  end
  object YesButton: TNewButton
    Left = 40
    Top = 189
    Width = 75
    Height = 23
    Caption = '*'
    Default = True
    ModalResult = 6
    TabOrder = 0
  end
  object LocationEdit: TEdit
    Left = 88
    Top = 148
    Width = 297
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 8
  end
  object LocationLabel: TNewStaticText
    Left = 12
    Top = 151
    Width = 5
    Height = 14
    Caption = '*'
    TabOrder = 7
  end
  object FilenameEdit: TEdit
    Left = 88
    Top = 116
    Width = 297
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 6
  end
  object FilenameLabel: TNewStaticText
    Left = 12
    Top = 119
    Width = 5
    Height = 14
    Caption = '*'
    TabOrder = 5
  end
  object BodyLabel: TNewStaticText
    Left = 12
    Top = 12
    Width = 373
    Height = 97
    AutoSize = False
    Caption = '*'
    ShowAccelChar = False
    TabOrder = 4
    WordWrap = True
  end
end
