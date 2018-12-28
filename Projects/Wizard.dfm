object WizardForm: TWizardForm
  Left = 191
  Top = 139
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'WizardForm'
  ClientHeight = 360
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnClose = FormClose
  OnResize = FormResize
  DesignSize = (
    497
    360)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TBevel
    Left = 0
    Top = 313
    Width = 497
    Height = 1
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object CancelButton: TNewButton
    Left = 464
    Top = 327
    Width = 17
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '*'
    TabOrder = 4
    OnClick = CancelButtonClick
  end
  object NextButton: TNewButton
    Left = 440
    Top = 327
    Width = 17
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '*'
    Default = True
    TabOrder = 3
    OnClick = NextButtonClick
  end
  object BackButton: TNewButton
    Left = 424
    Top = 327
    Width = 16
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '*'
    TabOrder = 2
    OnClick = BackButtonClick
  end
  object OuterNotebook: TNewNotebook
    Left = 0
    Top = 0
    Width = 497
    Height = 313
    ActivePage = InnerPage
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ParentColor = False
    TabOrder = 0
    object WelcomePage: TNewNotebookPage
      Color = clWindow
      ParentColor = False
      DesignSize = (
        497
        313)
      object WizardBitmapImage: TBitmapImage
        Left = 0
        Top = 0
        Width = 164
        Height = 314
        Anchors = [akLeft, akTop, akBottom]
        BackColor = clWindow
      end
      object WelcomeLabel2: TNewStaticText
        Left = 176
        Top = 76
        Width = 301
        Height = 234
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '*'
        ShowAccelChar = False
        TabOrder = 1
        WordWrap = True
      end
      object WelcomeLabel1: TNewStaticText
        Left = 176
        Top = 16
        Width = 301
        Height = 54
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '*'
        ShowAccelChar = False
        TabOrder = 0
        WordWrap = True
      end
    end
    object InnerPage: TNewNotebookPage
      DesignSize = (
        497
        313)
      object Bevel1: TBevel
        Left = 0
        Top = 58
        Width = 499
        Height = 1
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object InnerNotebook: TNewNotebook
        Left = 40
        Top = 72
        Width = 417
        Height = 237
        ActivePage = LicensePage
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 1
        object LicensePage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object LicenseNotAcceptedRadio: TNewRadioButton
            Left = 0
            Top = 216
            Width = 417
            Height = 17
            Anchors = [akLeft, akRight, akBottom]
            Caption = '*'
            Checked = True
            TabOrder = 3
            TabStop = True
            OnClick = LicenseNotAcceptedRadioClick
          end
          object LicenseAcceptedRadio: TNewRadioButton
            Left = 0
            Top = 196
            Width = 417
            Height = 17
            Anchors = [akLeft, akRight, akBottom]
            Caption = '*'
            TabOrder = 2
            OnClick = LicenseAcceptedRadioClick
          end
          object LicenseMemo: TRichEditViewer
            Left = 0
            Top = 24
            Width = 417
            Height = 161
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelKind = bkFlat
            BorderStyle = bsNone
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 1
            WantReturns = False
            UseRichEdit = False
          end
          object LicenseLabel1: TNewStaticText
            Left = 0
            Top = 0
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 0
            WordWrap = True
          end
        end
        object PasswordPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object PasswordEdit: TPasswordEdit
            Left = 0
            Top = 40
            Width = 265
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
          end
          object PasswordEditLabel: TNewStaticText
            Left = 0
            Top = 24
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            FocusControl = PasswordEdit
            TabOrder = 1
            WordWrap = True
          end
          object PasswordLabel: TNewStaticText
            Left = 0
            Top = 0
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 0
            WordWrap = True
          end
        end
        object InfoBeforePage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object InfoBeforeMemo: TRichEditViewer
            Left = 0
            Top = 24
            Width = 417
            Height = 205
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelKind = bkFlat
            BorderStyle = bsNone
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 1
            WantReturns = False
            UseRichEdit = False
          end
          object InfoBeforeClickLabel: TNewStaticText
            Left = 0
            Top = 0
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 0
            WordWrap = True
          end
        end
        object UserInfoPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object UserInfoSerialEdit: TNewEdit
            Left = 0
            Top = 120
            Width = 417
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 255
            TabOrder = 5
            OnChange = UserInfoEditChange
          end
          object UserInfoSerialLabel: TNewStaticText
            Left = 0
            Top = 104
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            FocusControl = UserInfoSerialEdit
            TabOrder = 4
            WordWrap = True
          end
          object UserInfoOrgEdit: TNewEdit
            Left = 0
            Top = 68
            Width = 417
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 255
            TabOrder = 3
            OnChange = UserInfoEditChange
          end
          object UserInfoOrgLabel: TNewStaticText
            Left = 0
            Top = 52
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            FocusControl = UserInfoOrgEdit
            TabOrder = 2
            WordWrap = True
          end
          object UserInfoNameEdit: TNewEdit
            Left = 0
            Top = 16
            Width = 417
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 255
            TabOrder = 1
            OnChange = UserInfoEditChange
          end
          object UserInfoNameLabel: TNewStaticText
            Left = 0
            Top = 0
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            FocusControl = UserInfoNameEdit
            TabOrder = 0
            WordWrap = True
          end
        end
        object SelectDirPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object SelectDirBitmapImage: TBitmapImage
            Left = 0
            Top = 0
            Width = 32
            Height = 32
          end
          object DiskSpaceLabel: TNewStaticText
            Left = 0
            Top = 216
            Width = 417
            Height = 14
            Anchors = [akLeft, akRight, akBottom]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 4
            WordWrap = True
          end
          object DirBrowseButton: TNewButton
            Left = 400
            Top = 67
            Width = 17
            Height = 23
            Anchors = [akTop, akRight]
            Caption = '*'
            TabOrder = 3
            OnClick = DirBrowseButtonClick
          end
          object DirEdit: TEdit
            Left = 0
            Top = 68
            Width = 397
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            Text = 'DirEdit'
          end
          object SelectDirBrowseLabel: TNewStaticText
            Left = 0
            Top = 44
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 1
            WordWrap = True
          end
          object SelectDirLabel: TNewStaticText
            Left = 44
            Top = 0
            Width = 373
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 0
            WordWrap = True
          end
        end
        object SelectComponentsPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object ComponentsDiskSpaceLabel: TNewStaticText
            Left = 0
            Top = 216
            Width = 417
            Height = 14
            Anchors = [akLeft, akRight, akBottom]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 3
          end
          object ComponentsList: TNewCheckListBox
            Left = 0
            Top = 48
            Width = 417
            Height = 157
            Anchors = [akLeft, akTop, akRight, akBottom]
            Offset = 2
            OnClickCheck = ComponentsListClickCheck
            RequireRadioSelection = True
            TabOrder = 2
          end
          object TypesCombo: TNewComboBox
            Left = 0
            Top = 24
            Width = 417
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
            OnChange = TypesComboChange
          end
          object SelectComponentsLabel: TNewStaticText
            Left = 0
            Top = 0
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 0
            WordWrap = True
          end
        end
        object SelectProgramGroupPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object SelectGroupBitmapImage: TBitmapImage
            Left = 0
            Top = 0
            Width = 32
            Height = 32
          end
          object NoIconsCheck: TNewCheckBox
            Left = 0
            Top = 215
            Width = 417
            Height = 17
            Anchors = [akLeft, akRight, akBottom]
            Caption = '*'
            TabOrder = 4
            OnClick = NoIconsCheckClick
          end
          object GroupBrowseButton: TNewButton
            Left = 400
            Top = 67
            Width = 17
            Height = 23
            Anchors = [akTop, akRight]
            Caption = '*'
            TabOrder = 3
            OnClick = GroupBrowseButtonClick
          end
          object GroupEdit: TNewEdit
            Left = 0
            Top = 68
            Width = 397
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            Text = 'GroupEdit'
          end
          object SelectStartMenuFolderBrowseLabel: TNewStaticText
            Left = 0
            Top = 44
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 1
            WordWrap = True
          end
          object SelectStartMenuFolderLabel: TNewStaticText
            Left = 44
            Top = 0
            Width = 373
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 0
            WordWrap = True
          end
        end
        object SelectTasksPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object TasksList: TNewCheckListBox
            Left = 0
            Top = 24
            Width = 417
            Height = 205
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clBtnFace
            RequireRadioSelection = True
            ShowLines = False
            TabOrder = 1
            WantTabs = True
          end
          object SelectTasksLabel: TNewStaticText
            Left = 0
            Top = 0
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 0
            WordWrap = True
          end
        end
        object ReadyPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object ReadyMemo: TNewMemo
            Left = 0
            Top = 24
            Width = 417
            Height = 205
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clBtnFace
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 1
            WantReturns = False
            WordWrap = False
          end
          object ReadyLabel: TNewStaticText
            Left = 0
            Top = 0
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 0
            WordWrap = True
          end
        end
        object PreparingPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object PreparingErrorBitmapImage: TBitmapImage
            Left = 0
            Top = 0
            Width = 16
            Height = 16
            Visible = False
          end
          object PreparingLabel: TNewStaticText
            Left = 24
            Top = 0
            Width = 393
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 0
            Visible = False
            WordWrap = True
          end
          object PreparingYesRadio: TNewRadioButton
            Left = 24
            Top = 28
            Width = 393
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '*'
            Checked = True
            TabOrder = 2
            TabStop = True
            Visible = False
          end
          object PreparingNoRadio: TNewRadioButton
            Left = 24
            Top = 56
            Width = 393
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '*'
            TabOrder = 3
            Visible = False
          end
          object PreparingMemo: TNewMemo
            Left = 24
            Top = 88
            Width = 393
            Height = 145
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clBtnFace
            Lines.Strings = (
              'PreparingMemo')
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 1
            WantReturns = False
            WordWrap = False
          end
        end
        object InstallingPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object FilenameLabel: TNewStaticText
            Left = 0
            Top = 16
            Width = 417
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            ForceLTRReading = True
            ShowAccelChar = False
            TabOrder = 1
          end
          object StatusLabel: TNewStaticText
            Left = 0
            Top = 0
            Width = 417
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            ShowAccelChar = False
            TabOrder = 0
          end
          object ProgressGauge: TNewProgressBar
            Left = 0
            Top = 42
            Width = 417
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Min = 0
            Max = 100
            Style = npbstNormal
          end
        end
        object InfoAfterPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object InfoAfterMemo: TRichEditViewer
            Left = 0
            Top = 24
            Width = 417
            Height = 205
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelKind = bkFlat
            BorderStyle = bsNone
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 1
            WantReturns = False
            UseRichEdit = False
          end
          object InfoAfterClickLabel: TNewStaticText
            Left = 0
            Top = 0
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            ShowAccelChar = False
            TabOrder = 0
            WordWrap = True
          end
        end
      end
      object MainPanel: TPanel
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
        object WizardSmallBitmapImage: TBitmapImage
          Left = 440
          Top = 0
          Width = 55
          Height = 58
          Anchors = [akTop, akRight]
          BackColor = clWindow
        end
        object PageDescriptionLabel: TNewStaticText
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
        object PageNameLabel: TNewStaticText
          Left = 24
          Top = 10
          Width = 405
          Height = 14
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = '*'
          ShowAccelChar = False
          TabOrder = 0
        end
      end
    end
    object FinishedPage: TNewNotebookPage
      Color = clWindow
      ParentColor = False
      DesignSize = (
        497
        313)
      object WizardBitmapImage2: TBitmapImage
        Left = 0
        Top = 0
        Width = 164
        Height = 314
        Anchors = [akLeft, akTop, akBottom]
        BackColor = clWindow
      end
      object RunList: TNewCheckListBox
        Left = 176
        Top = 156
        Width = 301
        Height = 149
        Anchors = [akLeft, akTop, akRight, akBottom]
        BorderStyle = bsNone
        ParentColor = True
        TabOrder = 2
        Visible = False
        WantTabs = True
      end
      object NoRadio: TNewRadioButton
        Left = 176
        Top = 184
        Width = 301
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = '*'
        TabOrder = 4
        Visible = False
      end
      object YesRadio: TNewRadioButton
        Left = 176
        Top = 156
        Width = 301
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = '*'
        Checked = True
        TabOrder = 3
        TabStop = True
        Visible = False
      end
      object FinishedLabel: TNewStaticText
        Left = 176
        Top = 76
        Width = 301
        Height = 53
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '*'
        ShowAccelChar = False
        TabOrder = 1
        WordWrap = True
      end
      object FinishedHeadingLabel: TNewStaticText
        Left = 176
        Top = 16
        Width = 301
        Height = 54
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '*'
        ShowAccelChar = False
        TabOrder = 0
        WordWrap = True
      end
    end
  end
  object BeveledLabel: TNewStaticText
    Left = 0
    Top = 306
    Width = 5
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = '*'
    Enabled = False
    ShowAccelChar = False
    TabOrder = 1
  end
end
