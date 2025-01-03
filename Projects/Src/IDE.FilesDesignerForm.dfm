object FilesDesignerForm: TFilesDesignerForm
  Left = 624
  Top = 375
  BorderStyle = bsDialog
  Caption = '[Files] Entries Designer'
  ClientHeight = 403
  ClientWidth = 575
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 361
    Width = 575
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 360
    ExplicitWidth = 571
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 575
      Height = 3
      Align = alTop
      Shape = bsBottomLine
    end
    object InsertButton: TButton
      Left = 406
      Top = 11
      Width = 75
      Height = 25
      Caption = 'Insert'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = InsertButtonClick
    end
    object CancelButton: TButton
      Left = 490
      Top = 11
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object AppFilesEditButton: TButton
    Left = 476
    Top = 107
    Width = 89
    Height = 23
    Caption = '&Parameters...'
    TabOrder = 1
  end
  object AppFilesRemoveButton: TButton
    Left = 476
    Top = 135
    Width = 89
    Height = 23
    Caption = 'Remo&ve'
    TabOrder = 2
  end
  object AppFilesAddDirButton: TButton
    Left = 476
    Top = 79
    Width = 89
    Height = 23
    Caption = 'Add fol&der...'
    TabOrder = 3
  end
  object AppFilesAddButton: TButton
    Left = 476
    Top = 51
    Width = 89
    Height = 23
    Caption = '&Add file(s)...'
    TabOrder = 4
  end
  object AppFilesListBox: TDropListBox
    Left = 8
    Top = 52
    Width = 453
    Height = 297
    ItemHeight = 15
    TabOrder = 5
  end
  object AppFilesLabel: TNewStaticText
    Left = 8
    Top = 32
    Width = 411
    Height = 16
    AutoSize = False
    Caption = '&Files:'
    FocusControl = AppFilesListBox
    TabOrder = 6
    WordWrap = True
  end
  object NotCreateAppDirCheck: TCheckBox
    Left = 8
    Top = 8
    Width = 253
    Height = 17
    Caption = '&Script has CreateAppDir=no'
    TabOrder = 7
  end
end
