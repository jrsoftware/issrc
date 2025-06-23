object UninstallProgressForm: TUninstallProgressForm
  Left = 191
  Top = 139
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'UninstallProgressForm'
  ClientHeight = 360
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDesigned
  Scaled = False
  DesignSize = (
    497
    360)
  TextHeight = 13
  object FBevel: TBevel
    Left = 0
    Top = 313
    Width = 497
    Height = 2
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object FCancelButton: TNewButton
    Left = 410
    Top = 327
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '*'
    Enabled = False
    TabOrder = 2
  end
  object FOuterNotebook: TNewNotebook
    Left = 0
    Top = 0
    Width = 497
    Height = 313
    ActivePage = FInnerPage
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ParentColor = False
    TabOrder = 0
    object FInnerPage: TNewNotebookPage
      DesignSize = (
        497
        313)
      object FBevel1: TBevel
        Left = 0
        Top = 58
        Width = 499
        Height = 3
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object FInnerNotebook: TNewNotebook
        Left = 4
        Top = 64
        Width = 489
        Height = 245
        ActivePage = FInstallingPage
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 1
        object FInstallingPage: TNewNotebookPage
          DesignSize = (
            489
            245)
          object FProgressBar: TNewProgressBar
            Left = 36
            Top = 56
            Width = 417
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Min = 0
            Max = 400
            Style = npbstNormal
          end
          object FStatusLabel: TNewStaticText
            Left = 36
            Top = 12
            Width = 417
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 0
          end
        end
      end
      object FMainPanel: TPanel
        Left = 0
        Top = 0
        Width = 497
        Height = 58
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        Color = clWindow
        TabOrder = 0
        DesignSize = (
          497
          58)
        object FWizardSmallBitmapImage: TBitmapImage
          Left = 451
          Top = 13
          Width = 32
          Height = 32
          Anchors = [akTop, akRight]
        end
        object FPageDescriptionLabel: TNewStaticText
          Left = 40
          Top = 26
          Width = 389
          Height = 29
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = '*'
          ShowAccelChar = False
          TabOrder = 1
          WordWrap = True
        end
        object FPageNameLabel: TNewStaticText
          Left = 24
          Top = 10
          Width = 405
          Height = 14
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = '*'
          TabOrder = 0
        end
      end
    end
  end
  object FBeveledLabel: TNewStaticText
    Left = 0
    Top = 306
    Width = 5
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = '*'
    Enabled = False
    TabOrder = 1
    Visible = False
  end
end
