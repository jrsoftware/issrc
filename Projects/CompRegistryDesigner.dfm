object RegistryDesignerForm: TRegistryDesignerForm
  Left = 298
  Top = 273
  BorderStyle = bsDialog
  Caption = '[Registry] Entries Designer'
  ClientHeight = 311
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnl_OKCancel: TPanel
    Left = 0
    Top = 269
    Width = 548
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 548
      Height = 3
      Align = alTop
      Shape = bsBottomLine
    end
    object btn_Insert: TButton
      Left = 379
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Insert'
      TabOrder = 0
      OnClick = btn_InsertClick
    end
    object btn_Cancel: TButton
      Left = 463
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
    Left = 23
    Top = 23
    Width = 269
    Height = 17
    Caption = 'Select a registry file (*.reg) to insert entries into the script:'
    TabOrder = 1
  end
  object edt_PathFileReg: TEdit
    Left = 23
    Top = 43
    Width = 431
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 2
  end
  object btn_Browse: TButton
    Left = 463
    Top = 41
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse...'
    TabOrder = 3
    OnClick = btn_BrowseClick
  end
  object gb_Settings: TGroupBox
    Left = 23
    Top = 79
    Width = 431
    Height = 181
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Settings (common for all parameters and subsections) '
    TabOrder = 4
    object cb_FlagSubKey: TCheckBox
      Left = 23
      Top = 26
      Width = 234
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Set for all subkeys (if exist) the Flag:'
      TabOrder = 0
      OnClick = cb_FlagSubKeyClick
    end
    object cb_FlagDelValue: TCheckBox
      Left = 23
      Top = 75
      Width = 234
      Height = 17
      Caption = 'Set for all parameters the Flag:'
      TabOrder = 1
      OnClick = cb_FlagDelValueClick
    end
    object cb_CheksOnOff: TCheckBox
      Left = 23
      Top = 102
      Width = 126
      Height = 17
      Caption = 'Add the Check:'
      TabOrder = 2
      OnClick = cb_CheksOnOffClick
    end
    object rb_IsWin64: TRadioButton
      Tag = 20
      Left = 157
      Top = 102
      Width = 100
      Height = 17
      Caption = 'IsWin64'
      Enabled = False
      TabOrder = 3
      OnClick = rb_IsWin64Click
    end
    object rb_NotIsWin64: TRadioButton
      Tag = 25
      Left = 157
      Top = 125
      Width = 100
      Height = 17
      Caption = 'not IsWin64'
      Enabled = False
      TabOrder = 4
      OnClick = rb_NotIsWin64Click
    end
    object rb_IsAdmMod: TRadioButton
      Tag = 30
      Left = 265
      Top = 102
      Width = 149
      Height = 17
      Caption = 'IsAdminInstallMode'
      Enabled = False
      TabOrder = 5
      OnClick = rb_IsAdmModClick
    end
    object rb_NotIsAdmMod: TRadioButton
      Tag = 35
      Left = 265
      Top = 125
      Width = 149
      Height = 17
      Caption = 'not IsAdminInstallMode'
      Enabled = False
      TabOrder = 6
      OnClick = rb_NotIsAdmModClick
    end
    object cb_MinVer: TCheckBox
      Left = 23
      Top = 148
      Width = 245
      Height = 17
      Caption = 'Set a minimum Windows version:'
      TabOrder = 7
      OnClick = cb_MinVerClick
    end
    object edt_MinVer: TEdit
      Left = 342
      Top = 146
      Width = 75
      Height = 21
      Enabled = False
      TabOrder = 8
      Text = '6.2'
    end
    object Panel1: TPanel
      Left = 257
      Top = 24
      Width = 160
      Height = 45
      BevelOuter = bvNone
      TabOrder = 9
      object rb_FlagDelKey: TRadioButton
        Tag = 10
        Left = 8
        Top = 2
        Width = 149
        Height = 17
        Caption = 'uninsdeletekey'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = rb_FlagDelKeyClick
      end
      object rb_FlagDelKeyIfEmpty: TRadioButton
        Tag = 11
        Left = 8
        Top = 25
        Width = 149
        Height = 17
        Caption = 'uninsdeletekeyifempty'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = rb_FlagDelKeyIfEmptyClick
      end
    end
    object st_uninsdeletevalue: TStaticText
      Left = 272
      Top = 77
      Width = 130
      Height = 14
      AutoSize = False
      Caption = 'uninsdeletevalue'
      Enabled = False
      TabOrder = 10
    end
    object st_MinVersion: TStaticText
      Left = 272
      Top = 150
      Width = 66
      Height = 14
      AutoSize = False
      Caption = 'MinVersion'
      Enabled = False
      TabOrder = 11
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Registry File|*.reg'
    Title = 'Select registry file (*.reg)'
    Left = 488
    Top = 214
  end
end
