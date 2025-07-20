object LicenseKeyForm: TLicenseKeyForm
  Left = 330
  Top = 188
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Enter Commercial License Key'
  ClientHeight = 200
  ClientWidth = 303
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    303
    200)
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 287
    Height = 154
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Paste your license key below'
    ShowFrame = False
    TabOrder = 0
    DesignSize = (
      287
      154)
    object LicenseKeyMemo: TMemo
      Left = 8
      Top = 16
      Width = 270
      Height = 130
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      OnChange = LicenseKeyMemoChange
    end
  end
  object CancelButton: TButton
    Left = 213
    Top = 167
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 1
  end
end
