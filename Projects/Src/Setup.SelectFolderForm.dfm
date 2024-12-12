object SelectFolderForm: TSelectFolderForm
  Left = 200
  Top = 108
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'SelectFolderForm'
  ClientHeight = 337
  ClientWidth = 349
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PopupMode = pmAuto
  Scaled = False
  DesignSize = (
    349
    337)
  TextHeight = 13
  object CancelButton: TNewButton
    Left = 320
    Top = 305
    Width = 17
    Height = 23
    Anchors = [akRight, akBottom]
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
    Anchors = [akRight, akBottom]
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
    Anchors = [akLeft, akBottom]
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
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = PathEditChange
  end
  object BrowseLabel: TNewStaticText
    Left = 12
    Top = 12
    Width = 325
    Height = 14
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '*'
    TabOrder = 0
    WordWrap = True
  end
end
