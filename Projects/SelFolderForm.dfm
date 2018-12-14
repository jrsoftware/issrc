object SelectFolderForm: TSelectFolderForm
  Left = 200
  Top = 108
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'SelectFolderForm'
  ClientHeight = 337
  ClientWidth = 349
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object CancelButton: TNewButton
    Left = 320
    Top = 305
    Width = 17
    Height = 23
    Cancel = True
    Caption = '*'
    ModalResult = 2
    TabOrder = 4
  end
  object OKButton: TNewButton
    Left = 304
    Top = 305
    Width = 16
    Height = 23
    Caption = '*'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 3
  end
  object NewFolderButton: TNewButton
    Left = 12
    Top = 305
    Width = 17
    Height = 23
    Cancel = True
    Caption = '*'
    Enabled = False
    TabOrder = 2
    OnClick = NewFolderButtonClick
  end
  object PathEdit: TEdit
    Left = 16
    Top = 36
    Width = 317
    Height = 21
    TabOrder = 1
    OnChange = PathEditChange
  end
  object BrowseLabel: TNewStaticText
    Left = 12
    Top = 12
    Width = 325
    Height = 14
    AutoSize = False
    Caption = '*'
    TabOrder = 0
    WordWrap = True
  end
end
