object WizardForm: TWizardForm
  Left = 191
  Top = 139
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'WizardForm'
  ClientHeight = 360
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Scaled = False
  OnClose = FormClose
  OnResize = FormResize
  DesignSize = (
    497
    360)
  TextHeight = 13
  object FBevel: TBevel
    Left = 0
    Top = 313
    Width = 497
    Height = 1
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object FCancelButton: TNewButton
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
  object FNextButton: TNewButton
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
  object FBackButton: TNewButton
    Left = 424
    Top = 327
    Width = 16
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '*'
    TabOrder = 2
    OnClick = BackButtonClick
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
    object FWelcomePage: TNewNotebookPage
      Color = clWindow
      ParentColor = False
      DesignSize = (
        497
        313)
      object FWizardBitmapImage: TBitmapImage
        Left = 0
        Top = 0
        Width = 164
        Height = 314
        Anchors = [akLeft, akTop, akBottom]
        BackColor = clWindow
      end
      object FWelcomeLabel2: TNewStaticText
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
      object FWelcomeLabel1: TNewStaticText
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
    object FInnerPage: TNewNotebookPage
      DesignSize = (
        497
        313)
      object FBevel1: TBevel
        Left = 0
        Top = 58
        Width = 499
        Height = 1
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object FInnerNotebook: TNewNotebook
        Left = 40
        Top = 72
        Width = 417
        Height = 237
        ActivePage = FLicensePage
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 1
        object FLicensePage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object FLicenseNotAcceptedRadio: TNewRadioButton
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
          object FLicenseAcceptedRadio: TNewRadioButton
            Left = 0
            Top = 196
            Width = 417
            Height = 17
            Anchors = [akLeft, akRight, akBottom]
            Caption = '*'
            TabOrder = 2
            OnClick = LicenseAcceptedRadioClick
          end
          object FLicenseMemo: TRichEditViewer
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
          object FLicenseLabel1: TNewStaticText
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
        object FPasswordPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object FPasswordEdit: TPasswordEdit
            Left = 0
            Top = 40
            Width = 265
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
          end
          object FPasswordEditLabel: TNewStaticText
            Left = 0
            Top = 24
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            FocusControl = FPasswordEdit
            TabOrder = 1
            WordWrap = True
          end
          object FPasswordLabel: TNewStaticText
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
        object FInfoBeforePage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object FInfoBeforeMemo: TRichEditViewer
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
          object FInfoBeforeClickLabel: TNewStaticText
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
        object FUserInfoPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object FUserInfoSerialEdit: TNewEdit
            Left = 0
            Top = 120
            Width = 417
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 255
            TabOrder = 5
            OnChange = UserInfoEditChange
          end
          object FUserInfoSerialLabel: TNewStaticText
            Left = 0
            Top = 104
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            FocusControl = FUserInfoSerialEdit
            TabOrder = 4
            WordWrap = True
          end
          object FUserInfoOrgEdit: TNewEdit
            Left = 0
            Top = 68
            Width = 417
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 255
            TabOrder = 3
            OnChange = UserInfoEditChange
          end
          object FUserInfoOrgLabel: TNewStaticText
            Left = 0
            Top = 52
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            FocusControl = FUserInfoOrgEdit
            TabOrder = 2
            WordWrap = True
          end
          object FUserInfoNameEdit: TNewEdit
            Left = 0
            Top = 16
            Width = 417
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 255
            TabOrder = 1
            OnChange = UserInfoEditChange
          end
          object FUserInfoNameLabel: TNewStaticText
            Left = 0
            Top = 0
            Width = 417
            Height = 14
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '*'
            FocusControl = FUserInfoNameEdit
            TabOrder = 0
            WordWrap = True
          end
        end
        object FSelectDirPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object FSelectDirBitmapImage: TBitmapImage
            Left = 0
            Top = 0
            Width = 34
            Height = 34
          end
          object FDiskSpaceLabel: TNewStaticText
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
          object FDirBrowseButton: TNewButton
            Left = 400
            Top = 67
            Width = 17
            Height = 23
            Anchors = [akTop, akRight]
            Caption = '*'
            TabOrder = 3
            OnClick = DirBrowseButtonClick
          end
          object FDirEdit: TEdit
            Left = 0
            Top = 68
            Width = 397
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            Text = 'DirEdit'
          end
          object FSelectDirBrowseLabel: TNewStaticText
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
          object FSelectDirLabel: TNewStaticText
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
        object FSelectComponentsPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object FComponentsDiskSpaceLabel: TNewStaticText
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
          object FComponentsList: TNewCheckListBox
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
          object FTypesCombo: TNewComboBox
            Left = 0
            Top = 24
            Width = 417
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
            OnChange = TypesComboChange
          end
          object FSelectComponentsLabel: TNewStaticText
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
        object FSelectProgramGroupPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object FSelectGroupBitmapImage: TBitmapImage
            Left = 0
            Top = 0
            Width = 34
            Height = 34
          end
          object FNoIconsCheck: TNewCheckBox
            Left = 0
            Top = 215
            Width = 417
            Height = 17
            Anchors = [akLeft, akRight, akBottom]
            Caption = '*'
            TabOrder = 4
            OnClick = NoIconsCheckClick
          end
          object FGroupBrowseButton: TNewButton
            Left = 400
            Top = 67
            Width = 17
            Height = 23
            Anchors = [akTop, akRight]
            Caption = '*'
            TabOrder = 3
            OnClick = GroupBrowseButtonClick
          end
          object FGroupEdit: TNewEdit
            Left = 0
            Top = 68
            Width = 397
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            Text = 'GroupEdit'
          end
          object FSelectStartMenuFolderBrowseLabel: TNewStaticText
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
          object FSelectStartMenuFolderLabel: TNewStaticText
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
        object FSelectTasksPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object FTasksList: TNewCheckListBox
            Left = 0
            Top = 24
            Width = 417
            Height = 205
            Anchors = [akLeft, akTop, akRight, akBottom]
            ParentColor = True
            RequireRadioSelection = True
            ShowLines = False
            TabOrder = 1
            WantTabs = True
          end
          object FSelectTasksLabel: TNewStaticText
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
        object FReadyPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object FReadyMemo: TNewMemo
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
          object FReadyLabel: TNewStaticText
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
        object FPreparingPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object FPreparingErrorBitmapImage: TBitmapImage
            Left = 0
            Top = 0
            Width = 17
            Height = 17
            Visible = False
          end
          object FPreparingLabel: TNewStaticText
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
          object FPreparingYesRadio: TNewRadioButton
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
          object FPreparingNoRadio: TNewRadioButton
            Left = 24
            Top = 56
            Width = 393
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '*'
            TabOrder = 3
            Visible = False
          end
          object FPreparingMemo: TNewMemo
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
        object FInstallingPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object FFilenameLabel: TNewStaticText
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
          object FStatusLabel: TNewStaticText
            Left = 0
            Top = 0
            Width = 417
            Height = 16
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            ShowAccelChar = False
            TabOrder = 0
          end
          object FProgressGauge: TNewProgressBar
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
        object FInfoAfterPage: TNewNotebookPage
          DesignSize = (
            417
            237)
          object FInfoAfterMemo: TRichEditViewer
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
          object FInfoAfterClickLabel: TNewStaticText
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
          Left = 439
          Top = 0
          Width = 58
          Height = 58
          Anchors = [akTop, akRight]
          BackColor = clWindow
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
          ShowAccelChar = False
          TabOrder = 0
        end
      end
    end
    object FFinishedPage: TNewNotebookPage
      Color = clWindow
      ParentColor = False
      DesignSize = (
        497
        313)
      object FWizardBitmapImage2: TBitmapImage
        Left = 0
        Top = 0
        Width = 164
        Height = 314
        Anchors = [akLeft, akTop, akBottom]
        BackColor = clWindow
      end
      object FRunList: TNewCheckListBox
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
      object FNoRadio: TNewRadioButton
        Left = 176
        Top = 184
        Width = 301
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = '*'
        TabOrder = 4
        Visible = False
      end
      object FYesRadio: TNewRadioButton
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
      object FFinishedLabel: TNewStaticText
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
      object FFinishedHeadingLabel: TNewStaticText
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
  object FBeveledLabel: TNewStaticText
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
