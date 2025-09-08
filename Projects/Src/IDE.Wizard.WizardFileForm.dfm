object WizardFileForm: TWizardFileForm
  Left = 284
  Top = 219
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Script Wizard File'
  ClientHeight = 345
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    420
    345)
  TextHeight = 13
  object RequiredLabel1: TNewStaticText
    Left = 8
    Top = 319
    Width = 21
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'bold'
    Enabled = False
    TabOrder = 4
  end
  object RequiredLabel2: TNewStaticText
    Left = 36
    Top = 319
    Width = 51
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = ' = required'
    Enabled = False
    TabOrder = 5
  end
  object OKButton: TButton
    Left = 258
    Top = 313
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
    Left = 338
    Top = 313
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 108
    Width = 403
    Height = 197
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Destination '
    TabOrder = 0
    DesignSize = (
      403
      197)
    object DestRootDirLabel: TNewStaticText
      Left = 12
      Top = 20
      Width = 112
      Height = 14
      Caption = 'Destination &base folder:'
      FocusControl = DestRootDirComboBox
      TabOrder = 0
    end
    object DestRootDirComboBox: TComboBox
      Left = 12
      Top = 40
      Width = 379
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 12
      TabOrder = 1
      OnChange = DestRootDirComboBoxChange
    end
    object DestRootDirEdit: TEdit
      Left = 12
      Top = 68
      Width = 379
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object DestSubDirLabel: TNewStaticText
      Left = 12
      Top = 96
      Width = 103
      Height = 14
      Caption = 'Destination &subfolder:'
      FocusControl = DestSubDirEdit
      TabOrder = 3
    end
    object DestSubDirEdit: TEdit
      Left = 12
      Top = 116
      Width = 379
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
    end
    object DestNameEdit: TEdit
      Left = 12
      Top = 164
      Width = 379
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
    end
    object DestNameLabel: TNewStaticText
      Left = 12
      Top = 144
      Width = 86
      Height = 14
      Caption = 'Destination &name:'
      FocusControl = DestSubDirEdit
      TabOrder = 6
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 403
    Height = 93
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Source '
    TabOrder = 3
    DesignSize = (
      403
      93)
    object SourceLabel: TNewStaticText
      Left = 12
      Top = 20
      Width = 95
      Height = 14
      Caption = '&Source file or folder:'
      Enabled = False
      TabOrder = 0
    end
    object SourceEdit: TEdit
      Left = 12
      Top = 40
      Width = 379
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
    object RecurseSubDirsCheck: TCheckBox
      Left = 12
      Top = 68
      Width = 125
      Height = 17
      Caption = '&Recurse subfolders'
      TabOrder = 2
      OnClick = CheckClick
    end
    object CreateAllSubDirsCheck: TCheckBox
      Left = 138
      Top = 68
      Width = 151
      Height = 17
      Caption = '&Include empty subfolders'
      TabOrder = 3
    end
    object ExtractArchiveCheck: TCheckBox
      Left = 290
      Top = 68
      Width = 111
      Height = 17
      Caption = '&Extract archive'
      TabOrder = 4
      OnClick = CheckClick
    end
  end
end
