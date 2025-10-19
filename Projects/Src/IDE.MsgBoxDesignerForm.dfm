object MsgBoxDesignerForm: TMsgBoxDesignerForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'MsgBox/TaskDialogMsgBox Call Designer'
  ClientHeight = 380
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    562
    380)
  TextHeight = 13
  object TaskInstructionLabel: TNewStaticText
    Left = 18
    Top = 14
    Width = 50
    Height = 14
    Caption = 'Instruction'
    TabOrder = 12
  end
  object TaskInstructionText: TEdit
    Left = 89
    Top = 10
    Width = 463
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object TaskMessageLabel: TNewStaticText
    Left = 18
    Top = 40
    Width = 44
    Height = 14
    Caption = 'Message'
    TabOrder = 13
  end
  object TaskMessageText: TEdit
    Left = 89
    Top = 36
    Width = 463
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object Button1Label: TNewStaticText
    Left = 18
    Top = 66
    Width = 62
    Height = 14
    Caption = 'Text Button1'
    TabOrder = 14
  end
  object Button1Text: TEdit
    Left = 89
    Top = 62
    Width = 112
    Height = 21
    TabOrder = 3
  end
  object Button2Label: TNewStaticText
    Left = 208
    Top = 66
    Width = 38
    Height = 14
    Caption = 'Button2'
    TabOrder = 15
  end
  object Button2Text: TEdit
    Left = 259
    Top = 62
    Width = 112
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
  end
  object Button3Label: TNewStaticText
    Left = 373
    Top = 66
    Width = 38
    Height = 14
    Caption = 'Button3'
    TabOrder = 16
  end
  object Button3Text: TEdit
    Left = 424
    Top = 62
    Width = 126
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 546
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Message '
    TabOrder = 0
    DesignSize = (
      546
      81)
    object MSGText: TMemo
      Left = 7
      Top = 20
      Width = 533
      Height = 53
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssVertical
      TabOrder = 0
      OnKeyPress = MSGTextKeyPress
    end
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 93
    Width = 240
    Height = 60
    Caption = ' Type'
    TabOrder = 6
    object cb_Suppressible: TCheckBox
      Left = 14
      Top = 24
      Width = 86
      Height = 17
      Caption = 'Suppressible'
      TabOrder = 0
      OnClick = cb_SuppressibleClick
    end
    object cb_MsgBox: TRadioButton
      Left = 115
      Top = 13
      Width = 115
      Height = 17
      Caption = 'MsgBox'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = cb_MsgBoxClick
    end
    object cb_TaskDialogMsgBox: TRadioButton
      Left = 115
      Top = 36
      Width = 122
      Height = 17
      Caption = 'TaskDialogMsgBox'
      TabOrder = 2
      TabStop = True
      OnClick = cb_TaskDialogMsgBoxClick
    end
  end
  object GroupBox5: TGroupBox
    Left = 254
    Top = 93
    Width = 300
    Height = 60
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Flags '
    TabOrder = 7
    object NewStaticText1: TNewStaticText
      Left = 14
      Top = 26
      Width = 68
      Height = 14
      Caption = 'Default button'
      TabOrder = 3
    end
    object NewEdit1: TEdit
      Left = 97
      Top = 22
      Width = 21
      Height = 21
      Alignment = taRightJustify
      NumbersOnly = True
      ReadOnly = True
      TabOrder = 0
      Text = '1'
    end
    object UpDown1: TUpDown
      Left = 118
      Top = 22
      Width = 16
      Height = 21
      Associate = NewEdit1
      Min = 1
      Max = 3
      Position = 1
      TabOrder = 1
      OnChanging = UpDown1Changing
    end
    object cb_MB_SETFOREGROUND: TCheckBox
      Left = 140
      Top = 24
      Width = 157
      Height = 17
      Caption = 'MB_SETFOREGROUND'
      TabOrder = 2
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 157
    Width = 165
    Height = 174
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Icon '
    TabOrder = 8
    DesignSize = (
      165
      174)
    object IMGmbInformation: TBitmapImage
      Left = 10
      Top = 23
      Width = 32
      Height = 32
    end
    object IMGmbConfirmation: TBitmapImage
      Left = 10
      Top = 60
      Width = 32
      Height = 32
    end
    object IMGmbError: TBitmapImage
      Left = 10
      Top = 97
      Width = 32
      Height = 32
    end
    object IMGmbCriticalError: TBitmapImage
      Left = 10
      Top = 135
      Width = 32
      Height = 32
    end
    object rb_mbInformation: TRadioButton
      Left = 53
      Top = 31
      Width = 110
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'mbInformation'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rb_mbConfirmation: TRadioButton
      Left = 53
      Top = 68
      Width = 110
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'mbConfirmation'
      TabOrder = 1
      TabStop = True
    end
    object rb_mbError: TRadioButton
      Left = 53
      Top = 105
      Width = 110
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'mbError'
      TabOrder = 2
      TabStop = True
    end
    object rb_mbCriticalError: TRadioButton
      Left = 53
      Top = 142
      Width = 110
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'mbCriticalError'
      TabOrder = 3
      TabStop = True
    end
  end
  object GroupBox3: TGroupBox
    Left = 179
    Top = 157
    Width = 182
    Height = 174
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Buttons '
    TabOrder = 9
    DesignSize = (
      182
      174)
    object rbMB_OK: TRadioButton
      Left = 14
      Top = 23
      Width = 160
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'MB_OK'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbMB_OKClick
    end
    object rbMB_OKCANCEL: TRadioButton
      Left = 14
      Top = 46
      Width = 160
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'MB_OKCANCEL'
      TabOrder = 1
      TabStop = True
      OnClick = rbMB_OKCANCELClick
    end
    object rbMB_YESNO: TRadioButton
      Left = 14
      Top = 70
      Width = 160
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'MB_YESNO'
      TabOrder = 2
      TabStop = True
      OnClick = rbMB_YESNOClick
    end
    object rbMB_YESNOCANCEL: TRadioButton
      Left = 14
      Top = 94
      Width = 160
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'MB_YESNOCANCEL'
      TabOrder = 3
      TabStop = True
      OnClick = rbMB_YESNOCANCELClick
    end
    object rbMB_RETRYCANCEL: TRadioButton
      Left = 14
      Top = 118
      Width = 160
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'MB_RETRYCANCEL'
      TabOrder = 4
      TabStop = True
      OnClick = rbMB_RETRYCANCELClick
    end
    object rbMB_ABORTRETRYIGNORE: TRadioButton
      Left = 14
      Top = 142
      Width = 160
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'MB_ABORTRETRYIGNORE'
      TabOrder = 5
      TabStop = True
      OnClick = rbMB_ABORTRETRYIGNOREClick
    end
  end
  object GroupBox4: TGroupBox
    Left = 367
    Top = 158
    Width = 187
    Height = 174
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Return values '
    TabOrder = 10
    object cb_IDOK: TCheckBox
      Left = 16
      Top = 23
      Width = 80
      Height = 17
      Caption = 'IDOK'
      TabOrder = 0
    end
    object cb_IDCANCEL: TCheckBox
      Left = 16
      Top = 42
      Width = 80
      Height = 17
      Caption = 'IDCANCEL'
      TabOrder = 1
    end
    object cb_IDYES: TCheckBox
      Left = 16
      Top = 62
      Width = 80
      Height = 17
      Caption = 'IDYES'
      TabOrder = 2
    end
    object cb_IDNO: TCheckBox
      Left = 16
      Top = 82
      Width = 80
      Height = 17
      Caption = 'IDNO'
      TabOrder = 3
    end
    object cb_IDABORT: TCheckBox
      Left = 16
      Top = 102
      Width = 80
      Height = 17
      Caption = 'IDABORT'
      TabOrder = 4
    end
    object cb_IDRETRY: TCheckBox
      Left = 16
      Top = 122
      Width = 80
      Height = 17
      Caption = 'IDRETRY'
      TabOrder = 5
    end
    object cb_IDIGNORE: TCheckBox
      Left = 16
      Top = 142
      Width = 80
      Height = 17
      Caption = 'IDIGNORE'
      TabOrder = 6
    end
    object rb_IDOK: TCheckBox
      Left = 100
      Top = 23
      Width = 16
      Height = 17
      TabOrder = 7
      Visible = False
      OnClick = rb_IDOKClick
    end
    object rb_IDCANCEL: TCheckBox
      Left = 100
      Top = 42
      Width = 16
      Height = 17
      TabOrder = 8
      Visible = False
      OnClick = rb_IDCANCELClick
    end
    object rb_IDYES: TCheckBox
      Left = 100
      Top = 62
      Width = 16
      Height = 17
      TabOrder = 9
      Visible = False
      OnClick = rb_IDYESClick
    end
    object rb_IDNO: TCheckBox
      Left = 100
      Top = 82
      Width = 16
      Height = 17
      TabOrder = 10
      Visible = False
      OnClick = rb_IDNOClick
    end
    object rb_IDABORT: TCheckBox
      Left = 100
      Top = 102
      Width = 16
      Height = 17
      TabOrder = 11
      Visible = False
      OnClick = rb_IDABORTClick
    end
    object rb_IDRETRY: TCheckBox
      Left = 100
      Top = 122
      Width = 16
      Height = 17
      TabOrder = 12
      Visible = False
      OnClick = rb_IDRETRYClick
    end
    object rb_IDIGNORE: TCheckBox
      Left = 100
      Top = 142
      Width = 16
      Height = 17
      TabOrder = 13
      Visible = False
      OnClick = rb_IDIGNOREClick
    end
    object cb_DefIDOK: TRadioButton
      Left = 147
      Top = 23
      Width = 17
      Height = 17
      TabOrder = 14
      TabStop = True
      Visible = False
    end
    object cb_DefIDCANCEL: TRadioButton
      Left = 147
      Top = 42
      Width = 17
      Height = 17
      TabOrder = 15
      TabStop = True
      Visible = False
    end
    object cb_DefIDYES: TRadioButton
      Left = 147
      Top = 62
      Width = 17
      Height = 17
      TabOrder = 16
      TabStop = True
      Visible = False
    end
    object cb_DefIDNO: TRadioButton
      Left = 147
      Top = 82
      Width = 17
      Height = 17
      TabOrder = 17
      TabStop = True
      Visible = False
    end
    object cb_DefIDABORT: TRadioButton
      Left = 147
      Top = 102
      Width = 17
      Height = 17
      TabOrder = 18
      TabStop = True
      Visible = False
    end
    object cb_DefIDRETRY: TRadioButton
      Left = 147
      Top = 122
      Width = 17
      Height = 17
      TabOrder = 19
      TabStop = True
      Visible = False
    end
    object cb_DefIDIGNORE: TRadioButton
      Left = 147
      Top = 142
      Width = 17
      Height = 17
      TabOrder = 20
      TabStop = True
      Visible = False
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 338
    Width = 562
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 11
    DesignSize = (
      562
      42)
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 562
      Height = 1
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = 168
      ExplicitWidth = 50
    end
    object MBDButtonPreview: TButton
      Left = 10
      Top = 11
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Preview'
      TabOrder = 0
      OnClick = MBDButtonPreviewClick
    end
    object MBDButtonOK: TButton
      Left = 393
      Top = 11
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Insert'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object MBDButtonCancel: TButton
      Left = 477
      Top = 11
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
  end
end
