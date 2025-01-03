object RegistryDesignerForm: TRegistryDesignerForm
  Left = 298
  Top = 273
  BorderStyle = bsDialog
  Caption = '[Registry] Entries Designer'
  ClientHeight = 347
  ClientWidth = 496
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
    496
    347)
  TextHeight = 13
  object AppRegistryMinVerDocImage: TImage
    Left = 406
    Top = 170
    Width = 16
    Height = 16
    Anchors = [akTop, akRight]
    AutoSize = True
    Transparent = True
    ExplicitLeft = 414
  end
  object Panel1: TPanel
    Left = 0
    Top = 305
    Width = 496
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 9
    ExplicitTop = 320
    ExplicitWidth = 500
    DesignSize = (
      496
      42)
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 496
      Height = 3
      Align = alTop
      Shape = bsBottomLine
      ExplicitWidth = 500
    end
    object InsertButton: TButton
      Left = 322
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Insert'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = InsertButtonClick
      ExplicitLeft = 330
    end
    object CancelButton: TButton
      Left = 406
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 410
    end
    object PrivilegesRequiredLabel: TNewStaticText
      Left = 8
      Top = 15
      Width = 5
      Height = 14
      Caption = '*'
      Enabled = False
      TabOrder = 2
    end
  end
  object AppRegistryFileLabel: TNewStaticText
    Left = 8
    Top = 18
    Width = 170
    Height = 14
    Caption = '&Windows registry file (.reg) to import:'
    FocusControl = AppRegistryFileEdit
    TabOrder = 0
  end
  object AppRegistryFileEdit: TEdit
    Left = 8
    Top = 38
    Width = 384
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    ExplicitWidth = 392
  end
  object AppRegistryFileButton: TButton
    Left = 406
    Top = 36
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Browse...'
    TabOrder = 2
    ExplicitLeft = 414
  end
  object AppRegistrySettingsLabel: TNewStaticText
    Left = 8
    Top = 69
    Width = 156
    Height = 14
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Settings (for all keys and values):'
    TabOrder = 3
  end
  object AppRegistryUninsDeleteKeyCheck: TCheckBox
    Left = 16
    Top = 109
    Width = 225
    Height = 17
    Caption = 'Also delete keys which are not empty'
    TabOrder = 5
  end
  object AppRegistryUninsDeleteKeyIfEmptyCheck: TCheckBox
    Left = 8
    Top = 89
    Width = 225
    Height = 17
    Caption = 'Delete keys which are empty on uninstall'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object AppRegistryUninsDeleteValueCheck: TCheckBox
    Left = 8
    Top = 139
    Width = 225
    Height = 17
    Caption = 'Delete values on uninstall'
    Checked = True
    State = cbChecked
    TabOrder = 6
    WordWrap = True
  end
  object AppRegistryMinVerCheck: TCheckBox
    Left = 8
    Top = 169
    Width = 245
    Height = 17
    Caption = 'Create only if Windows'#39' version is at least:'
    TabOrder = 7
  end
  object AppRegistryMinVerEdit: TEdit
    Left = 259
    Top = 168
    Width = 132
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    ExplicitWidth = 140
  end
end
