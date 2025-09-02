object TaskDialogForm: TTaskDialogForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'TaskDialogForm'
  ClientHeight = 371
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PopupMode = pmAuto
  Scaled = False
  TextHeight = 13
  object BottomPanel: TPanel
    Left = 0
    Top = 322
    Width = 418
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object BottomStackPanel: TStackPanel
      Left = 35
      Top = 0
      Width = 383
      Height = 49
      Align = alRight
      AutoSize = True
      BevelOuter = bvNone
      ControlCollection = <
        item
          Control = OkButton
        end
        item
          Control = YesButton
        end
        item
          Control = NoButton
        end
        item
          Control = RetryButton
        end
        item
          Control = CancelButton
        end>
      Orientation = spoHorizontal
      TabOrder = 0
      object OkButton: TNewButton
        Left = 0
        Top = 12
        Width = 75
        Height = 25
        Caption = 'OkButton'
        ModalResult = 1
        TabOrder = 0
      end
      object YesButton: TNewButton
        Left = 77
        Top = 12
        Width = 75
        Height = 25
        Caption = 'YesButton'
        ModalResult = 6
        TabOrder = 1
      end
      object NoButton: TNewButton
        Left = 154
        Top = 12
        Width = 75
        Height = 25
        Caption = 'NoButton'
        ModalResult = 7
        TabOrder = 2
      end
      object RetryButton: TNewButton
        Left = 231
        Top = 12
        Width = 75
        Height = 25
        Caption = 'RetryButton'
        ModalResult = 4
        TabOrder = 3
      end
      object CancelButton: TNewButton
        Left = 308
        Top = 12
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'CancelButton'
        ModalResult = 2
        TabOrder = 4
      end
    end
  end
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 418
    Height = 322
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    object LeftPanel: TPanel
      Left = 0
      Top = 0
      Width = 32
      Height = 322
      Align = alLeft
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
      object BitmapImage: TBitmapImage
        Left = 0
        Top = 0
        Width = 32
        Height = 32
        BackColor = clNone
      end
    end
    object MainStackPanel: TStackPanel
      Left = 32
      Top = 0
      Width = 386
      Height = 322
      Align = alClient
      BevelOuter = bvNone
      ControlCollection = <
        item
          Control = InstructionText
        end
        item
          Control = TextText
        end
        item
          Control = MainButton1
        end
        item
          Control = MainButton2
        end
        item
          Control = MainButton3
        end>
      HorizontalPositioning = sphpFill
      ParentColor = True
      TabOrder = 1
      object InstructionText: TNewStaticText
        Left = 0
        Top = 0
        Width = 386
        Height = 14
        Caption = 'Instruction text'
        TabOrder = 0
      end
      object TextText: TNewStaticText
        Left = 0
        Top = 16
        Width = 386
        Height = 14
        Caption = 'Text text'
        TabOrder = 1
        WordWrap = True
      end
      object MainButton1: TButton
        Left = 0
        Top = 32
        Width = 386
        Height = 77
        Caption = 'MainButton1'
        CommandLinkHint = 'Note'
        ElevationRequired = True
        Style = bsCommandLink
        TabOrder = 2
      end
      object MainButton2: TButton
        Left = 0
        Top = 111
        Width = 386
        Height = 77
        Caption = 'MainButton2'
        Style = bsCommandLink
        TabOrder = 3
      end
      object MainButton3: TButton
        Left = 0
        Top = 190
        Width = 386
        Height = 77
        Caption = 'MainButton3'
        Style = bsCommandLink
        TabOrder = 4
      end
    end
  end
end
