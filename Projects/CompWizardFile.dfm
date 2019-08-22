object WizardFileForm: TWizardFileForm
  Left = 284
  Top = 219
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Script Wizard File'
  ClientHeight = 297
  ClientWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RequiredLabel1: TNewStaticText
    Left = 8
    Top = 271
    Width = 21
    Height = 14
    Caption = 'bold'
    Enabled = False
    TabOrder = 4
  end
  object RequiredLabel2: TNewStaticText
    Left = 36
    Top = 271
    Width = 51
    Height = 14
    Caption = ' = required'
    Enabled = False
    TabOrder = 5
  end
  object OKButton: TButton
    Left = 188
    Top = 265
    Width = 73
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 268
    Top = 265
    Width = 73
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 108
    Width = 333
    Height = 149
    Caption = ' Destination '
    TabOrder = 0
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
      Width = 309
      Height = 21
      Style = csDropDownList
      DropDownCount = 12
      ItemHeight = 13
      TabOrder = 1
      OnChange = DestRootDirComboBoxChange
    end
    object DestRootDirEdit: TEdit
      Left = 12
      Top = 68
      Width = 309
      Height = 21
      TabOrder = 2
    end
    object SubDirLabel: TNewStaticText
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
      Width = 309
      Height = 21
      TabOrder = 4
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 333
    Height = 93
    Caption = ' Source '
    TabOrder = 3
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
      Width = 309
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
    object RecurseSubDirsCheck: TCheckBox
      Left = 12
      Top = 68
      Width = 141
      Height = 17
      Caption = '&Recurse subfolders'
      TabOrder = 2
      OnClick = RecurseSubDirsCheckClick
    end
    object CreateAllSubDirsCheck: TCheckBox
      Left = 156
      Top = 68
      Width = 165
      Height = 17
      Caption = '&Include empty subfolders'
      TabOrder = 3
    end
  end
end
