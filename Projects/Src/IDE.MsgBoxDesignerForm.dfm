object MsgBoxDesignerForm: TMsgBoxDesignerForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = '%1 Call Designer'
  ClientHeight = 414
  ClientWidth = 596
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    596
    414)
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
    Left = 112
    Top = 10
    Width = 476
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
    Left = 112
    Top = 36
    Width = 476
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object Button1Label: TNewStaticText
    Left = 18
    Top = 68
    Width = 37
    Height = 14
    Caption = 'Buttons'
    TabOrder = 14
  end
  object Button1Text: TEdit
    Left = 112
    Top = 64
    Width = 144
    Height = 21
    TabOrder = 3
  end
  object Button2Label: TNewStaticText
    Left = 262
    Top = 68
    Width = 7
    Height = 14
    Caption = '2'
    TabOrder = 15
  end
  object Button2Text: TEdit
    Left = 278
    Top = 64
    Width = 144
    Height = 21
    TabOrder = 4
  end
  object Button3Label: TNewStaticText
    Left = 428
    Top = 68
    Width = 7
    Height = 14
    Caption = '3'
    TabOrder = 16
  end
  object Button3Text: TEdit
    Left = 444
    Top = 64
    Width = 144
    Height = 21
    TabOrder = 5
  end
  object GroupBox1: TNewGroupBox
    Left = 8
    Top = 8
    Width = 580
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = '*'
    TabOrder = 0
    DesignSize = (
      580
      81)
    object MSGText: TMemo
      Left = 7
      Top = 20
      Width = 567
      Height = 53
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssVertical
      TabOrder = 0
      OnKeyPress = MSGTextKeyPress
    end
  end
  object GroupBox6: TNewGroupBox
    Left = 8
    Top = 91
    Width = 240
    Height = 94
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Type '
    TabOrder = 6
    object cb_Suppressible: TCheckBox
      Left = 14
      Top = 70
      Width = 211
      Height = 17
      Caption = 'Suppressible'
      TabOrder = 2
      OnClick = cb_SuppressibleClick
    end
    object cb_MsgBox: TRadioButton
      Left = 14
      Top = 24
      Width = 211
      Height = 17
      Caption = '*'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = cb_MsgBoxClick
    end
    object cb_TaskDialogMsgBox: TRadioButton
      Left = 14
      Top = 47
      Width = 211
      Height = 17
      Caption = '*'
      TabOrder = 1
      TabStop = True
      OnClick = cb_TaskDialogMsgBoxClick
    end
  end
  object GroupBox5: TNewGroupBox
    Left = 254
    Top = 91
    Width = 334
    Height = 94
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Flags '
    TabOrder = 7
    object DefaultButtonLabel: TNewStaticText
      Left = 14
      Top = 26
      Width = 68
      Height = 14
      Caption = 'Default button'
      TabOrder = 3
    end
    object DefaultButtonEdit: TEdit
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
    object DefaultButtonUpDown: TUpDown
      Left = 118
      Top = 22
      Width = 16
      Height = 21
      Associate = DefaultButtonEdit
      Min = 1
      Max = 3
      Position = 1
      TabOrder = 1
    end
    object cb_MB_SETFOREGROUND: TCheckBox
      Left = 14
      Top = 47
      Width = 275
      Height = 17
      Caption = '*'
      TabOrder = 2
    end
  end
  object GroupBox2: TNewGroupBox
    Left = 8
    Top = 191
    Width = 165
    Height = 174
    Anchors = [akLeft, akBottom]
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
      Caption = '*'
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
      Caption = '*'
      TabOrder = 1
      TabStop = True
    end
    object rb_mbError: TRadioButton
      Left = 53
      Top = 105
      Width = 110
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '*'
      TabOrder = 2
      TabStop = True
    end
    object rb_mbCriticalError: TRadioButton
      Left = 53
      Top = 142
      Width = 110
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '*'
      TabOrder = 3
      TabStop = True
    end
  end
  object GroupBox3: TNewGroupBox
    Left = 179
    Top = 191
    Width = 182
    Height = 174
    Anchors = [akLeft, akBottom]
    Caption = '*'
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
      Caption = '*'
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
      Caption = '*'
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
      Caption = '*'
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
      Caption = '*'
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
      Caption = '*'
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
      Caption = '*'
      TabOrder = 5
      TabStop = True
      OnClick = rbMB_ABORTRETRYIGNOREClick
    end
  end
  object GroupBox4: TNewGroupBox
    Left = 367
    Top = 191
    Width = 221
    Height = 174
    Anchors = [akLeft, akRight, akBottom]
    Caption = ' Return values '
    TabOrder = 10
    object cb_IDOK: TCheckBox
      Left = 16
      Top = 23
      Width = 80
      Height = 17
      Caption = '*'
      TabOrder = 0
    end
    object cb_IDCANCEL: TCheckBox
      Left = 16
      Top = 42
      Width = 80
      Height = 17
      Caption = '*'
      TabOrder = 1
    end
    object cb_IDYES: TCheckBox
      Left = 16
      Top = 62
      Width = 80
      Height = 17
      Caption = '*'
      TabOrder = 2
    end
    object cb_IDNO: TCheckBox
      Left = 16
      Top = 82
      Width = 80
      Height = 17
      Caption = '*'
      TabOrder = 3
    end
    object cb_IDABORT: TCheckBox
      Left = 16
      Top = 102
      Width = 80
      Height = 17
      Caption = '*'
      TabOrder = 4
    end
    object cb_IDRETRY: TCheckBox
      Left = 16
      Top = 122
      Width = 80
      Height = 17
      Caption = '*'
      TabOrder = 5
    end
    object cb_IDIGNORE: TCheckBox
      Left = 16
      Top = 142
      Width = 80
      Height = 17
      Caption = '*'
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
    Top = 372
    Width = 596
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 11
    DesignSize = (
      596
      42)
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 596
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
      Left = 427
      Top = 11
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Insert'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object MBDButtonCancel: TButton
      Left = 511
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
