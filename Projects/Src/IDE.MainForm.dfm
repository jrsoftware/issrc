object MainForm: TMainForm
  Left = 206
  Top = 97
  BorderStyle = bsNone
  Caption = '*'
  ClientHeight = 306
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  Position = poDefault
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 29
    Width = 361
    Height = 1
    Align = alTop
    Shape = bsTopLine
  end
  object BodyPanel: TPanel
    Left = 0
    Top = 92
    Width = 361
    Height = 194
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 3
    object SplitPanel: TPanel
      Left = 0
      Top = 86
      Width = 361
      Height = 4
      Cursor = crSizeNS
      Align = alBottom
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 1
      Visible = False
      OnMouseMove = SplitPanelMouseMove
    end
    object StatusPanel: TPanel
      Left = 0
      Top = 90
      Width = 361
      Height = 104
      Align = alBottom
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      Visible = False
      object FindResultsList: TListBox
        Left = 0
        Top = 0
        Width = 361
        Height = 83
        Style = lbOwnerDrawFixed
        Align = alClient
        BorderStyle = bsNone
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 3
        Visible = False
        OnDblClick = FindResultsListDblClick
        OnDrawItem = FindResultsListDrawItem
        OnKeyDown = OutputListKeyDown
      end
      object DebugCallStackList: TListBox
        Left = 0
        Top = 0
        Width = 361
        Height = 83
        Style = lbOwnerDrawFixed
        Align = alClient
        BorderStyle = bsNone
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 2
        Visible = False
        OnDrawItem = DebugCallStackListDrawItem
        OnKeyDown = OutputListKeyDown
      end
      object DebugOutputList: TListBox
        Left = 0
        Top = 0
        Width = 361
        Height = 83
        Style = lbOwnerDrawFixed
        Align = alClient
        BorderStyle = bsNone
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 1
        Visible = False
        OnDrawItem = DebugOutputListDrawItem
        OnKeyDown = OutputListKeyDown
      end
      object CompilerOutputList: TListBox
        Left = 0
        Top = 0
        Width = 361
        Height = 83
        Style = lbOwnerDrawFixed
        Align = alClient
        BorderStyle = bsNone
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 0
        OnDrawItem = CompilerOutputListDrawItem
        OnKeyDown = OutputListKeyDown
      end
      object OutputTabSet: TNewTabSet
        Left = 0
        Top = 83
        Width = 361
        Height = 21
        Align = alBottom
        TabIndex = 0
        Tabs.Strings = (
          'Compiler Output'
          'Debug Output'
          'Debug Call Stack'
          'Find Results')
        OnClick = OutputTabSetClick
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 286
    Width = 361
    Height = 20
    Panels = <
      item
        Alignment = taCenter
        Bevel = pbNone
        Text = '   1:   1'
        Width = 64
      end
      item
        Alignment = taCenter
        Bevel = pbNone
        Width = 64
      end
      item
        Alignment = taCenter
        Bevel = pbNone
        Text = 'Insert'
        Width = 64
      end
      item
        Alignment = taCenter
        Bevel = pbNone
        Text = '.*'
        Width = 23
      end
      item
        Bevel = pbNone
        Style = psOwnerDraw
        Width = 110
      end
      item
        Bevel = pbNone
        Style = psOwnerDraw
        Width = 23
      end
      item
        Bevel = pbNone
        Style = psOwnerDraw
        Width = 128
      end
      item
        Bevel = pbNone
        Width = 50
      end>
    OnClick = StatusBarClick
    OnDrawPanel = StatusBarDrawPanel
    OnResize = StatusBarResize
  end
  object ToolBarPanel: TPanel
    Left = 0
    Top = 0
    Width = 361
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object ToolBar: TToolBar
      AlignWithMargins = True
      Left = 7
      Top = 4
      Width = 351
      Height = 25
      Margins.Left = 7
      Margins.Top = 4
      Margins.Bottom = 0
      Images = ThemedToolbarVirtualImageList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Transparent = True
      object BackNavButton: TToolButton
        Left = 0
        Top = 0
        ImageIndex = 54
        ImageName = 'button-arrow-left'
        OnClick = BackNavButtonClick
      end
      object ForwardNavButton: TToolButton
        Left = 23
        Top = 0
        ImageIndex = 55
        ImageName = 'button-arrow-right'
        OnClick = ForwardNavButtonClick
      end
      object ToolButton1: TToolButton
        Left = 46
        Top = 0
        Width = 8
        Style = tbsSeparator
      end
      object NewMainFileButton: TToolButton
        Left = 54
        Top = 0
        Hint = 'New Main Script (Ctrl+N)'
        ImageIndex = 0
        ImageName = 'document-new'
        OnClick = FNewMainFileClick
      end
      object OpenMainFileButton: TToolButton
        Left = 77
        Top = 0
        Hint = 'Open Main Script (Ctrl+O)'
        ImageIndex = 1
        ImageName = 'folder-open-filled-arrow-down-right'
        OnClick = FOpenMainFileClick
      end
      object SaveButton: TToolButton
        Left = 100
        Top = 0
        Hint = 'Save (Ctrl+S)'
        ImageIndex = 2
        ImageName = 'save-filled'
        OnClick = FSaveClick
      end
      object ToolButton2: TToolButton
        Left = 123
        Top = 0
        Width = 8
        Style = tbsSeparator
      end
      object CompileButton: TToolButton
        Left = 131
        Top = 0
        ImageIndex = 3
        ImageName = 'build'
        OnClick = BCompileClick
      end
      object StopCompileButton: TToolButton
        Left = 154
        Top = 0
        Hint = 'Stop Compile (Esc)'
        Enabled = False
        ImageIndex = 4
        ImageName = 'build-cancel-2'
        OnClick = BStopCompileClick
      end
      object ToolButton3: TToolButton
        Left = 177
        Top = 0
        Width = 8
        Style = tbsSeparator
      end
      object RunButton: TToolButton
        Left = 185
        Top = 0
        ImageIndex = 5
        ImageName = 'debug-start-filled'
        OnClick = RRunClick
      end
      object PauseButton: TToolButton
        Left = 208
        Top = 0
        Hint = 'Pause'
        Enabled = False
        ImageIndex = 6
        ImageName = 'debug-break-all-filled'
        OnClick = RPauseClick
      end
      object TerminateButton: TToolButton
        Left = 231
        Top = 0
        Enabled = False
        ImageIndex = 10
        ImageName = 'debug-stop-filled'
        OnClick = RTerminateClick
      end
      object ToolButton4: TToolButton
        Left = 254
        Top = 0
        Width = 8
        Style = tbsSeparator
      end
      object TargetSetupButton: TToolButton
        Left = 262
        Top = 0
        Hint = 'Target Setup (Ctrl+Q)'
        Grouped = True
        ImageIndex = 7
        ImageName = 'install'
        Style = tbsCheck
        OnClick = RTargetClick
      end
      object TargetUninstallButton: TToolButton
        Left = 285
        Top = 0
        Hint = 'Target Uninstall (Ctrl+W)'
        Grouped = True
        ImageIndex = 8
        ImageName = 'uninstall'
        Style = tbsCheck
        OnClick = RTargetClick
      end
      object ToolButton5: TToolButton
        Left = 308
        Top = 0
        Width = 8
        Style = tbsSeparator
      end
      object HelpButton: TToolButton
        Left = 316
        Top = 0
        Hint = 'Help (F1)'
        ImageIndex = 9
        ImageName = 'button-help'
        OnClick = HDocClick
      end
    end
  end
  object MemosTabSet: TNewTabSet
    Left = 0
    Top = 71
    Width = 361
    Height = 21
    Align = alTop
    TabIndex = 0
    Tabs.Strings = (
      'Main Script')
    TabPosition = tpTop
    OnClick = MemosTabSetClick
    OnCloseButtonClick = MemosTabSetOnCloseButtonClick
  end
  object UpdatePanel: TPanel
    Left = 0
    Top = 30
    Width = 361
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object UpdatePanelClosePaintBox: TPaintBox
      AlignWithMargins = True
      Left = 330
      Top = 10
      Width = 21
      Height = 21
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alRight
      OnClick = UpdatePanelClosePaintBoxClick
      OnPaint = UpdatePanelClosePaintBoxPaint
    end
    object UpdatePanelDonateImage: TImage
      AlignWithMargins = True
      Left = 303
      Top = 10
      Width = 21
      Height = 21
      Cursor = crHandPoint
      Margins.Top = 10
      Margins.Bottom = 10
      Align = alRight
      Center = True
      ParentShowHint = False
      ShowHint = True
      OnClick = UpdatePanelDonateImageClick
    end
    object UpdateLinkLabel: TLinkLabel
      Left = 13
      Top = 13
      Width = 303
      Height = 17
      Caption = 
        'Your version of Inno Setup has been updated! <a id="hwhatsnew">S' +
        'ee what'#39's new</a>.'
      TabOrder = 0
      OnLinkClick = UpdateLinkLabelLinkClick
    end
  end
  object MainMenu1: TMainMenu
    AutoHotkeys = maManual
    AutoLineReduction = maManual
    Left = 48
    Top = 64
    object FMenu: TMenuItem
      Caption = '&File'
      OnClick = FMenuClick
      object FNewMainFile: TMenuItem
        Caption = '&New'
        ShortCut = 16462
        OnClick = FNewMainFileClick
      end
      object FOpenMainFile: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = FOpenMainFileClick
      end
      object FRecent: TMenuItem
        Caption = 'Open &Recent'
        object N6: TMenuItem
          Caption = '-'
        end
        object FClearRecent: TMenuItem
          Caption = '&Clear Recently Opened'
          OnClick = FClearRecentClick
        end
      end
      object N19: TMenuItem
        Caption = '-'
      end
      object FSave: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
        OnClick = FSaveClick
      end
      object FSaveMainFileAs: TMenuItem
        Caption = 'Save &As...'
        OnClick = FSaveClick
      end
      object FSaveEncoding: TMenuItem
        Caption = 'Save &Encoding'
        object FSaveEncodingAuto: TMenuItem
          Caption = '&Auto (ANSI or UTF-8)'
          RadioItem = True
          OnClick = FSaveEncodingItemClick
        end
        object FSaveEncodingUTF8WithoutBOM: TMenuItem
          Caption = '&UTF-8'
          RadioItem = True
          OnClick = FSaveEncodingItemClick
        end
        object FSaveEncodingUTF8WithBOM: TMenuItem
          Caption = 'UTF-8 with &BOM'
          RadioItem = True
          OnClick = FSaveEncodingItemClick
        end
      end
      object FSaveAll: TMenuItem
        Caption = 'Sa&ve All'
        ShortCut = 24659
        OnClick = FSaveAllClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object FPrint: TMenuItem
        Caption = '&Print...'
        ShortCut = 16464
        OnClick = FPrintClick
      end
      object N22: TMenuItem
        Caption = '-'
      end
      object FExit: TMenuItem
        Caption = 'E&xit'
        OnClick = FExitClick
      end
    end
    object EMenu: TMenuItem
      Caption = '&Edit'
      OnClick = EMenuClick
      object EUndo: TMenuItem
        Caption = '&Undo'
        OnClick = EUndoClick
      end
      object ERedo: TMenuItem
        Caption = '&Redo'
        OnClick = ERedoClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object ECut: TMenuItem
        Caption = 'Cu&t'
        OnClick = ECutClick
      end
      object ECopy: TMenuItem
        Caption = '&Copy'
        OnClick = ECopyClick
      end
      object EPaste: TMenuItem
        Caption = '&Paste'
        OnClick = EPasteClick
      end
      object EDelete: TMenuItem
        Caption = 'De&lete'
        OnClick = EDeleteClick
      end
      object N25: TMenuItem
        Caption = '-'
      end
      object ESelectAll: TMenuItem
        Caption = 'Select &All'
        OnClick = ESelectAllClick
      end
      object ESelectNextOccurrence: TMenuItem
        Caption = 'Add Next &Occurrence'
        OnClick = ESelectNextOccurrenceClick
      end
      object ESelectAllOccurrences: TMenuItem
        Caption = '&Select All Occurrences'
        OnClick = ESelectAllOccurrencesClick
      end
      object ESelectAllFindMatches: TMenuItem
        Caption = 'Select All Find &Matches'
        OnClick = ESelectAllFindMatchesClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object EFind: TMenuItem
        Caption = '&Find...'
        ShortCut = 16454
        OnClick = EFindClick
      end
      object EFindInFiles: TMenuItem
        Caption = 'F&ind in Files...'
        ShortCut = 24646
        OnClick = EFindInFilesClick
      end
      object EFindNext: TMenuItem
        Caption = 'Find &Next'
        ShortCut = 114
        OnClick = EFindNextOrPreviousClick
      end
      object EFindPrevious: TMenuItem
        Caption = 'Find Pre&vious'
        ShortCut = 8306
        OnClick = EFindNextOrPreviousClick
      end
      object EReplace: TMenuItem
        Caption = 'R&eplace...'
        ShortCut = 16456
        OnClick = EReplaceClick
      end
      object EFindRegEx: TMenuItem
        Caption = 'Use Regular E&xpressions'
        OnClick = EFindRegExClick
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object EFoldLine: TMenuItem
        Caption = 'Fol&d Section'
        OnClick = EFoldOrUnfoldLineClick
      end
      object EUnfoldLine: TMenuItem
        Caption = '&Unfold Section'
        OnClick = EFoldOrUnfoldLineClick
      end
      object EGoto: TMenuItem
        Caption = '&Go to Line...'
        ShortCut = 16455
        OnClick = EGotoClick
      end
      object N18: TMenuItem
        Caption = '-'
      end
      object EToggleLinesComment: TMenuItem
        Caption = 'Toggle Line Comment'
        OnClick = EToggleLinesCommentClick
      end
      object EBraceMatch: TMenuItem
        Caption = 'Go To Matching Brace'
        OnClick = EBraceMatchClick
      end
    end
    object VMenu: TMenuItem
      Caption = '&View'
      OnClick = VMenuClick
      object VZoom: TMenuItem
        Caption = '&Zoom'
        object VZoomIn: TMenuItem
          Caption = 'Zoom &In'
          OnClick = VZoomInClick
        end
        object VZoomOut: TMenuItem
          Caption = 'Zoom &Out'
          OnClick = VZoomOutClick
        end
        object N9: TMenuItem
          Caption = '-'
        end
        object VZoomReset: TMenuItem
          Caption = '&Reset'
          OnClick = VZoomResetClick
        end
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object VToolbar: TMenuItem
        Caption = '&Toolbar'
        OnClick = VToolbarClick
      end
      object VStatusBar: TMenuItem
        Caption = 'St&atus Bar'
        OnClick = VStatusBarClick
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object VNextTab: TMenuItem
        Caption = '&Next Tab'
        ShortCut = 16393
        OnClick = VNextTabClick
      end
      object VPreviousTab: TMenuItem
        Caption = '&Previous Tab'
        ShortCut = 24585
        OnClick = VPreviousTabClick
      end
      object VCloseCurrentTab: TMenuItem
        Caption = 'C&lose Tab'
        ShortCut = 16499
        OnClick = VCloseCurrentTabClick
      end
      object VReopenTab: TMenuItem
        Caption = 'Re&open Tab'
      end
      object VReopenTabs: TMenuItem
        Caption = 'Reopen &All Tabs'
        OnClick = VReopenTabsClick
      end
      object MemosTabSetPopupMenu: TMenuItem
        Caption = 'MemosTabSetPopupMenu'
        Visible = False
        OnClick = MemosTabSetPopupMenuClick
        object VCloseCurrentTab2: TMenuItem
          Caption = 'C&lose Current Tab'
          OnClick = VCloseCurrentTabClick
        end
        object VReopenTab2: TMenuItem
          Caption = 'Re&open Tab'
        end
        object VReopenTabs2: TMenuItem
          Caption = 'Reopen &All Tabs'
          OnClick = VReopenTabsClick
        end
      end
      object NavPopupMenu: TMenuItem
        Caption = 'NavPopupMenu'
        Visible = False
        OnClick = NavPopupMenuClick
      end
      object N20: TMenuItem
        Caption = '-'
      end
      object VCompilerOutput: TMenuItem
        Caption = '&Compiler Output'
        RadioItem = True
        OnClick = VCompilerOutputClick
      end
      object VDebugOutput: TMenuItem
        Caption = '&Debug Output'
        RadioItem = True
        OnClick = VDebugOutputClick
      end
      object VDebugCallStack: TMenuItem
        Caption = 'D&ebug Call Stack'
        RadioItem = True
        OnClick = VDebugCallStackClick
      end
      object VFindResults: TMenuItem
        Caption = '&Find Results'
        RadioItem = True
        OnClick = VFindResultsClick
      end
      object VHide: TMenuItem
        Caption = '&Hide Bottom Pane'
        RadioItem = True
        OnClick = VHideClick
      end
      object N24: TMenuItem
        Caption = '-'
      end
      object VWordWrap: TMenuItem
        Caption = '&Word Wrap'
        ShortCut = 32858
        OnClick = VWordWrapClick
      end
      object OutputListPopupMenu: TMenuItem
        Caption = 'OutputListPopupMenu'
        Visible = False
        OnClick = SimpleMenuClick
        object POutputListCopy: TMenuItem
          Caption = '&Copy'
          OnClick = POutputListCopyClick
        end
        object POutputListSelectAll: TMenuItem
          Caption = 'Select &All'
          OnClick = POutputListSelectAllClick
        end
      end
    end
    object BMenu: TMenuItem
      Caption = '&Build'
      OnClick = BMenuClick
      object BCompile: TMenuItem
        Caption = '&Compile'
        OnClick = BCompileClick
      end
      object BStopCompile: TMenuItem
        Caption = 'S&top Compile'
        Enabled = False
        OnClick = BStopCompileClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object BLowPriority: TMenuItem
        Caption = '&Low Priority During Compile'
        OnClick = BLowPriorityClick
      end
      object N17: TMenuItem
        Caption = '-'
      end
      object BOpenOutputFolder: TMenuItem
        Caption = '&Open Output Folder'
        Enabled = False
        OnClick = BOpenOutputFolderClick
      end
    end
    object RMenu: TMenuItem
      Caption = '&Run'
      OnClick = RMenuClick
      object RRun: TMenuItem
        Caption = '&Run'
        OnClick = RRunClick
      end
      object RParameters: TMenuItem
        Caption = '&Parameters...'
        OnClick = RParametersClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object RRunToCursor: TMenuItem
        Caption = 'Run to &Cursor'
        OnClick = RRunToCursorClick
      end
      object RStepInto: TMenuItem
        Caption = 'Step &Into'
        OnClick = RStepIntoClick
      end
      object RStepOver: TMenuItem
        Caption = 'Step &Over'
        OnClick = RStepOverClick
      end
      object RStepOut: TMenuItem
        Caption = 'Step Out'
        OnClick = RStepOutClick
      end
      object RToggleBreakPoint: TMenuItem
        Caption = 'Toggle &Breakpoint'
        OnClick = RToggleBreakPointClick
      end
      object RDeleteBreakPoints: TMenuItem
        Caption = '&Delete All Breakpoints'
        OnClick = RDeleteBreakPointsClick
      end
      object BreakPointsPopupMenu: TMenuItem
        Caption = 'BreakPointsPopupMenu'
        Visible = False
        OnClick = BreakPointsPopupMenuClick
        object RToggleBreakPoint2: TMenuItem
          Caption = 'Toggle &Breakpoint'
          OnClick = RToggleBreakPointClick
        end
        object RDeleteBreakPoints2: TMenuItem
          Caption = '&Delete All Breakpoints'
          OnClick = RDeleteBreakPointsClick
        end
      end
      object RPause: TMenuItem
        Caption = 'P&ause'
        Enabled = False
        OnClick = RPauseClick
      end
      object RTerminate: TMenuItem
        Caption = '&Terminate'
        Enabled = False
        OnClick = RTerminateClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object REvaluate: TMenuItem
        Caption = '&Evaluate Constant...'
        Enabled = False
        OnClick = REvaluateClick
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object RTargetSetup: TMenuItem
        Caption = 'Target &Setup'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16465
        OnClick = RTargetClick
      end
      object RTargetUninstall: TMenuItem
        Caption = 'Target &Uninstall'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16471
        OnClick = RTargetClick
      end
    end
    object TMenu: TMenuItem
      Caption = '&Tools'
      OnClick = TMenuClick
      object TAddRemovePrograms: TMenuItem
        Caption = '&Add/Remove Programs'
        OnClick = TAddRemoveProgramsClick
      end
      object N23: TMenuItem
        Caption = '-'
      end
      object TGenerateGUID: TMenuItem
        Caption = 'Generate &GUID'
        ShortCut = 24647
        OnClick = TGenerateGUIDClick
      end
      object TFilesDesigner: TMenuItem
        Caption = 'Generate [F&iles] Entries...'
        ShortCut = 24649
        OnClick = TFilesDesignerClick
      end
      object TRegistryDesigner: TMenuItem
        Caption = 'Generate [&Registry] Entries...'
        ImageIndex = 66
        ShortCut = 24658
        OnClick = TRegistryDesignerClick
      end
      object TMsgBoxDesigner: TMenuItem
        Caption = 'Generate &MsgBox/TaskDialogMsgBox Call...'
        ShortCut = 24653
        OnClick = TMsgBoxDesignerClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object TSignTools: TMenuItem
        Caption = '&Configure Sign Tools...'
        OnClick = TSignToolsClick
      end
      object N16: TMenuItem
        Caption = '-'
      end
      object TOptions: TMenuItem
        Caption = '&Options...'
        OnClick = TOptionsClick
      end
    end
    object HMenu: TMenuItem
      Caption = '&Help'
      OnClick = SimpleMenuClick
      object HDonate: TMenuItem
        Caption = '&Support Inno Setup - Thank you!'
        OnClick = HDonateClick
      end
      object N21: TMenuItem
        Caption = '-'
      end
      object HShortcutsDoc: TMenuItem
        Caption = '&Keyboard And Mouse Commands'
        OnClick = HShortcutsDocClick
      end
      object HRegexDoc: TMenuItem
        Caption = 'Regular E&xpressions'
        OnClick = HRegexDocClick
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object HDoc: TMenuItem
        Caption = 'Inno Setup &Documentation'
        OnClick = HDocClick
      end
      object HExamples: TMenuItem
        Caption = 'Inno Setup &Example Scripts'
        OnClick = HExamplesClick
      end
      object HFaq: TMenuItem
        Caption = 'Inno Setup &FAQ'
        OnClick = HFaqClick
      end
      object HMailingList: TMenuItem
        Caption = 'Inno Setup &Mailing List'
        OnClick = HMailingListClick
      end
      object HWhatsNew: TMenuItem
        Caption = 'Inno Setup &Revision History'
        OnClick = HWhatsNewClick
      end
      object HWebsite: TMenuItem
        Caption = 'Inno Setup &Web Site'
        OnClick = HWebsiteClick
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object HISPPDoc: TMenuItem
        Caption = 'Inno Setup &Preprocessor Documentation'
        OnClick = HISPPDocClick
      end
      object HISPPSep: TMenuItem
        Caption = '-'
      end
      object HAbout: TMenuItem
        Caption = '&About Inno Setup'
        OnClick = HAboutClick
      end
    end
  end
  object FindDialog: TFindDialog
    OnFind = FindDialogFind
    Left = 48
    Top = 120
  end
  object ReplaceDialog: TReplaceDialog
    OnFind = FindDialogFind
    OnReplace = ReplaceDialogReplace
    Left = 160
    Top = 120
  end
  object CheckIfRunningTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = CheckIfRunningTimerTimer
    Left = 160
    Top = 64
  end
  object ThemedToolbarVirtualImageList: TVirtualImageList
    AutoFill = True
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'document-new'
        Name = 'document-new'
      end
      item
        CollectionIndex = 1
        CollectionName = 'folder-open-filled-arrow-down-right'
        Name = 'folder-open-filled-arrow-down-right'
      end
      item
        CollectionIndex = 2
        CollectionName = 'save-filled'
        Name = 'save-filled'
      end
      item
        CollectionIndex = 3
        CollectionName = 'build'
        Name = 'build'
      end
      item
        CollectionIndex = 4
        CollectionName = 'build-cancel-2'
        Name = 'build-cancel-2'
      end
      item
        CollectionIndex = 5
        CollectionName = 'debug-start-filled'
        Name = 'debug-start-filled'
      end
      item
        CollectionIndex = 6
        CollectionName = 'debug-break-all-filled'
        Name = 'debug-break-all-filled'
      end
      item
        CollectionIndex = 7
        CollectionName = 'install'
        Name = 'install'
      end
      item
        CollectionIndex = 8
        CollectionName = 'uninstall'
        Name = 'uninstall'
      end
      item
        CollectionIndex = 9
        CollectionName = 'button-help'
        Name = 'button-help'
      end
      item
        CollectionIndex = 10
        CollectionName = 'debug-stop-filled'
        Name = 'debug-stop-filled'
      end
      item
        CollectionIndex = 11
        CollectionName = 'save-as-filled'
        Name = 'save-as-filled'
      end
      item
        CollectionIndex = 12
        CollectionName = 'save-all-filled'
        Name = 'save-all-filled'
      end
      item
        CollectionIndex = 13
        CollectionName = 'printer'
        Name = 'printer'
      end
      item
        CollectionIndex = 14
        CollectionName = 'command-undo-1'
        Name = 'command-undo-1'
      end
      item
        CollectionIndex = 15
        CollectionName = 'command-redo-1'
        Name = 'command-redo-1'
      end
      item
        CollectionIndex = 16
        CollectionName = 'clipboard-cut'
        Name = 'clipboard-cut'
      end
      item
        CollectionIndex = 17
        CollectionName = 'clipboard-copy'
        Name = 'clipboard-copy'
      end
      item
        CollectionIndex = 18
        CollectionName = 'clipboard-paste'
        Name = 'clipboard-paste'
      end
      item
        CollectionIndex = 19
        CollectionName = 'select-all'
        Name = 'select-all'
      end
      item
        CollectionIndex = 20
        CollectionName = 'find'
        Name = 'find'
      end
      item
        CollectionIndex = 21
        CollectionName = 'replace'
        Name = 'replace'
      end
      item
        CollectionIndex = 22
        CollectionName = 'control-edit'
        Name = 'control-edit'
      end
      item
        CollectionIndex = 23
        CollectionName = 'debug-step-into'
        Name = 'debug-step-into'
      end
      item
        CollectionIndex = 24
        CollectionName = 'debug-step-over'
        Name = 'debug-step-over'
      end
      item
        CollectionIndex = 25
        CollectionName = 'debug-step-out'
        Name = 'debug-step-out'
      end
      item
        CollectionIndex = 26
        CollectionName = 'debug-breakpoint-filled'
        Name = 'debug-breakpoint-filled'
      end
      item
        CollectionIndex = 27
        CollectionName = 'variables'
        Name = 'variables'
      end
      item
        CollectionIndex = 28
        CollectionName = 'heart-filled'
        Name = 'heart-filled'
      end
      item
        CollectionIndex = 29
        CollectionName = 'alert-filled'
        Name = 'alert-filled'
      end
      item
        CollectionIndex = 30
        CollectionName = 'home'
        Name = 'home'
      end
      item
        CollectionIndex = 31
        CollectionName = 'button-info'
        Name = 'button-info'
      end
      item
        CollectionIndex = 32
        CollectionName = 'application'
        Name = 'application'
      end
      item
        CollectionIndex = 33
        CollectionName = 'folder-open-filled-find'
        Name = 'folder-open-filled-find'
      end
      item
        CollectionIndex = 34
        CollectionName = 'gear-filled'
        Name = 'gear-filled'
      end
      item
        CollectionIndex = 35
        CollectionName = 'key-filled'
        Name = 'key-filled'
      end
      item
        CollectionIndex = 36
        CollectionName = 'unused\letter-a-arrow-right-2'
        Name = 'unused\letter-a-arrow-right-2'
      end
      item
        CollectionIndex = 37
        CollectionName = 'symbol-cancel'
        Name = 'symbol-cancel'
      end
      item
        CollectionIndex = 38
        CollectionName = 'comment-text-script-filled'
        Name = 'comment-text-script-filled'
      end
      item
        CollectionIndex = 39
        CollectionName = 'control-tree-script-filled'
        Name = 'control-tree-script-filled'
      end
      item
        CollectionIndex = 40
        CollectionName = 'documents-script-filled'
        Name = 'documents-script-filled'
      end
      item
        CollectionIndex = 41
        CollectionName = 'tag-script-filled'
        Name = 'tag-script-filled'
      end
      item
        CollectionIndex = 42
        CollectionName = 'control-tab-filled-arrow-left-2'
        Name = 'control-tab-filled-arrow-left-2'
      end
      item
        CollectionIndex = 43
        CollectionName = 'control-tab-filled-arrow-right-2'
        Name = 'control-tab-filled-arrow-right-2'
      end
      item
        CollectionIndex = 44
        CollectionName = 'unused\control-tab-filled-cancel-2'
        Name = 'unused\control-tab-filled-cancel-2'
      end
      item
        CollectionIndex = 45
        CollectionName = 'control-tab-filled-redo-1'
        Name = 'control-tab-filled-redo-1'
      end
      item
        CollectionIndex = 46
        CollectionName = 'unused\find-arrow-left-2'
        Name = 'unused\find-arrow-left-2'
      end
      item
        CollectionIndex = 47
        CollectionName = 'unused\find-arrow-right-2'
        Name = 'unused\find-arrow-right-2'
      end
      item
        CollectionIndex = 48
        CollectionName = 'announcement'
        Name = 'announcement'
      end
      item
        CollectionIndex = 49
        CollectionName = 'debug-start-filled-arrow-right-2'
        Name = 'debug-start-filled-arrow-right-2'
      end
      item
        CollectionIndex = 50
        CollectionName = 'zoom-in'
        Name = 'zoom-in'
      end
      item
        CollectionIndex = 51
        CollectionName = 'zoom-out'
        Name = 'zoom-out'
      end
      item
        CollectionIndex = 52
        CollectionName = 'unused\debug-breakpoint-filled-eraser'
        Name = 'unused\debug-breakpoint-filled-eraser'
      end
      item
        CollectionIndex = 53
        CollectionName = 'debug-breakpoints-filled-eraser'
        Name = 'debug-breakpoints-filled-eraser'
      end
      item
        CollectionIndex = 54
        CollectionName = 'button-arrow-left'
        Name = 'button-arrow-left'
      end
      item
        CollectionIndex = 55
        CollectionName = 'button-arrow-right'
        Name = 'button-arrow-right'
      end
      item
        CollectionIndex = 56
        CollectionName = 'folder-open-filled'
        Name = 'folder-open-filled'
      end
      item
        CollectionIndex = 57
        CollectionName = 'eraser'
        Name = 'eraser'
      end
      item
        CollectionIndex = 58
        CollectionName = 'symbol-add'
        Name = 'symbol-add'
      end
      item
        CollectionIndex = 59
        CollectionName = 'symbol-remove'
        Name = 'symbol-remove'
      end>
    ImageCollection = ImagesModule.LightToolBarImageCollection
    Left = 80
    Top = 176
  end
  object LightToolbarVirtualImageList: TVirtualImageList
    Images = <>
    ImageCollection = ImagesModule.LightToolBarImageCollection
    Left = 191
    Top = 197
  end
  object FindInFilesDialog: TFindDialog
    Options = [frDown, frHideUpDown]
    OnFind = FindInFilesDialogFind
    Left = 272
    Top = 120
  end
  object PrintDialog: TPrintDialog
    Left = 272
    Top = 64
  end
  object ThemedMarkersAndACVirtualImageList: TVirtualImageList
    Images = <>
    Width = 12
    Height = 12
    Left = 320
    Top = 176
  end
end
