object GotoFileForm: TGotoFileForm
  Left = 330
  Top = 188
  BorderIcons = [biSystemMenu]
  Caption = 'Goto File'
  ClientHeight = 497
  ClientWidth = 577
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    577
    497)
  TextHeight = 13
  object OKButton: TButton
    Left = 416
    Top = 467
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 496
    Top = 467
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object GotoFileListBox: TListBox
    Left = 8
    Top = 37
    Width = 561
    Height = 422
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 3
    OnDblClick = GotoFileListBoxDblClick
    OnKeyDown = GotoFileEditOrListBoxKeyDown
  end
  object GotoFileEdit: TEdit
    Left = 8
    Top = 8
    Width = 561
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = GotoFileEditChange
    OnKeyDown = GotoFileEditOrListBoxKeyDown
  end
end
