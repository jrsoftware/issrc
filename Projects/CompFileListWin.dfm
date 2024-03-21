object CFLWForm: TCFLWForm
  Left = 624
  Top = 375
  BorderStyle = bsDialog
  Caption = 'Insert File List Field'
  ClientHeight = 458
  ClientWidth = 636
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    636
    458)
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 408
    Width = 636
    Height = 50
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 399
    ExplicitWidth = 630
    DesignSize = (
      636
      50)
    object CFLWButtonOK: TButton
      Left = 408
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Insert'
      ModalResult = 1
      TabOrder = 0
    end
    object CFLWButtonCancel: TButton
      Left = 503
      Top = 16
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 497
    end
  end
  object AppFilesEditButton: TButton
    Left = 514
    Top = 152
    Width = 89
    Height = 23
    Anchors = [akTop, akRight]
    Caption = '&Edit...'
    TabOrder = 1
    OnClick = AppFilesEditButtonClick
  end
  object AppFilesRemoveButton: TButton
    Left = 514
    Top = 180
    Width = 89
    Height = 23
    Anchors = [akTop, akRight]
    Caption = 'Remo&ve'
    TabOrder = 2
    OnClick = AppFilesRemoveButtonClick
  end
  object AppFilesAddDirButton: TButton
    Left = 514
    Top = 124
    Width = 89
    Height = 23
    Anchors = [akTop, akRight]
    Caption = 'Add fol&der...'
    TabOrder = 3
  end
  object AppFilesAddButton: TButton
    Left = 514
    Top = 96
    Width = 89
    Height = 23
    Anchors = [akTop, akRight]
    Caption = '&Add file(s)...'
    TabOrder = 4
  end
  object AppFilesListBox: TDropListBox
    Left = 20
    Top = 96
    Width = 457
    Height = 297
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 15
    TabOrder = 5
    OnClick = AppFilesListBoxClick
    OnDblClick = AppFilesListBoxDblClick
    OnDropFile = AppFilesListBoxDropFile
  end
  object AppFilesLabel: TNewStaticText
    Left = 20
    Top = 62
    Width = 415
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Other application &files:'
    FocusControl = AppFilesListBox
    TabOrder = 6
    WordWrap = True
  end
  object NotCreateAppDirCheck: TCheckBox
    Left = 20
    Top = 24
    Width = 257
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'The application doe&sn'#39't need a folder'
    TabOrder = 7
  end
end
