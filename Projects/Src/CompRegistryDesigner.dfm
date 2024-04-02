object RegistryDesignerForm: TRegistryDesignerForm
  Left = 298
  Top = 273
  BorderStyle = bsDialog
  Caption = '[Registry] Entries Designer'
  ClientHeight = 348
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object pnl_OKCancel: TPanel
    Left = 0
    Top = 306
    Width = 505
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 505
      Height = 3
      Align = alTop
      Shape = bsBottomLine
    end
    object btn_Insert: TButton
      Left = 335
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Insert'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btn_InsertClick
    end
    object btn_Cancel: TButton
      Left = 419
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object st_Text1: TStaticText
    Left = 13
    Top = 18
    Width = 397
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Select a Windows registry file (*.reg) to insert entries into th' +
      'e script:'
    TabOrder = 1
  end
  object edt_PathFileReg: TEdit
    Left = 13
    Top = 38
    Width = 397
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 2
  end
  object btn_Browse: TButton
    Left = 419
    Top = 36
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Browse...'
    TabOrder = 3
    OnClick = btn_BrowseClick
  end
  object gb_Settings: TGroupBox
    Left = 13
    Top = 69
    Width = 397
    Height = 230
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Settings (common for all values and subkeys) '
    TabOrder = 4
    object cb_FlagUnInsDelKey: TCheckBox
      Left = 16
      Top = 20
      Width = 150
      Height = 17
      Caption = 'uninsdeletekey'
      TabOrder = 0
      OnClick = cb_FlagUnInsDelKeyClick
    end
    object st_uninsdelkey: TStaticText
      Left = 36
      Top = 38
      Width = 350
      Height = 28
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Adds the Flag for delete the entire key, including all values an' +
        'd subkeys in it when the program is uninstalled (use only on key' +
        's private to your App).'
      TabOrder = 9
    end
    object cb_FlagUnInsDelKeyIfEmpty: TCheckBox
      Left = 16
      Top = 68
      Width = 150
      Height = 17
      Caption = 'uninsdeletekeyifempty'
      TabOrder = 1
      OnClick = cb_FlagUnInsDelKeyIfEmptyClick
    end
    object st_uninsdelkeyifempty: TStaticText
      Left = 36
      Top = 86
      Width = 350
      Height = 28
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Adds the Flag for delete the key if it has no values or subkeys ' +
        'left in it when the program is uninstalled.'
      TabOrder = 10
    end
    object cb_FlagDelValue: TCheckBox
      Left = 16
      Top = 116
      Width = 150
      Height = 17
      Caption = 'uninsdeletevalue'
      TabOrder = 2
      WordWrap = True
    end
    object st_uninsdelvalue: TStaticText
      Left = 36
      Top = 134
      Width = 350
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Adds the Flag for delete the value when the program is uninstall' +
        'ed.'
      TabOrder = 11
    end
    object cb_CheksIs64bit: TCheckBox
      Left = 16
      Top = 153
      Width = 370
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Add the Check if Setup/Uninstall is running in 64-bit install mo' +
        'de or not:'
      TabOrder = 3
      OnClick = cb_CheksIs64bitClick
    end
    object rb_Is64BitInstMod: TRadioButton
      Tag = 20
      Left = 50
      Top = 175
      Width = 150
      Height = 17
      Caption = 'Is64BitInstallMode'
      Enabled = False
      TabOrder = 4
    end
    object rb_NotIs64BitInstMod: TRadioButton
      Tag = 25
      Left = 212
      Top = 174
      Width = 160
      Height = 17
      Caption = 'not Is64BitInstallMode'
      Enabled = False
      TabOrder = 5
    end
    object cb_MinVer: TCheckBox
      Left = 16
      Top = 201
      Width = 185
      Height = 17
      Caption = 'Set a minimum Windows version:'
      TabOrder = 6
      OnClick = cb_MinVerClick
    end
    object st_MinVersion: TStaticText
      Left = 234
      Top = 202
      Width = 68
      Height = 14
      AutoSize = False
      Caption = 'MinVersion'
      Enabled = False
      TabOrder = 8
    end
    object edt_MinVer: TEdit
      Left = 306
      Top = 199
      Width = 75
      Height = 21
      Enabled = False
      TabOrder = 7
      Text = '6.2'
    end
  end
end
