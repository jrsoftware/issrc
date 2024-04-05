object RegistryDesignerForm: TRegistryDesignerForm
  Left = 298
  Top = 273
  BorderStyle = bsDialog
  Caption = '[Registry] Entries Designer'
  ClientHeight = 348
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object pnl_OKCancel: TPanel
    Left = 0
    Top = 320
    Width = 500
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 500
      Height = 3
      Align = alTop
      Shape = bsBottomLine
    end
    object btn_Insert: TButton
      Left = 330
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
      Left = 414
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
    object st_PriviligesRequired: TStaticText
      Left = 8
      Top = 15
      Width = 8
      Height = 17
      Caption = '*'
      Enabled = False
      TabOrder = 2
    end
  end
  object st_Text1: TStaticText
    Left = 8
    Top = 18
    Width = 316
    Height = 17
    Caption = 
      'Select a Windows registry file (*.reg) to insert entries into th' +
      'e script:'
    TabOrder = 4
  end
  object edt_PathFileReg: TEdit
    Left = 8
    Top = 38
    Width = 392
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btn_Browse: TButton
    Left = 414
    Top = 36
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Browse...'
    TabOrder = 1
    OnClick = btn_BrowseClick
  end
  object gb_Settings: TGroupBox
    Left = 8
    Top = 69
    Width = 392
    Height = 230
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Settings (common for all values and subkeys) '
    TabOrder = 2
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
      Width = 345
      Height = 28
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Adds the Flag for delete the entire key, including all values an' +
        'd subkeys in it when the program is uninstalled (use only on key' +
        's private to your App).'
      TabOrder = 6
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
      Width = 345
      Height = 28
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Adds the Flag for delete the key if it has no values or subkeys ' +
        'left in it when the program is uninstalled.'
      TabOrder = 7
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
      Width = 345
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Adds the Flag for delete the value when the program is uninstall' +
        'ed.'
      TabOrder = 8
    end
    object cb_MinVer: TCheckBox
      Left = 16
      Top = 201
      Width = 185
      Height = 17
      Caption = 'Set a minimum Windows version:'
      TabOrder = 3
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
      TabOrder = 5
    end
    object edt_MinVer: TEdit
      Left = 306
      Top = 199
      Width = 75
      Height = 21
      Enabled = False
      TabOrder = 4
      Text = '6.2'
    end
  end
end
