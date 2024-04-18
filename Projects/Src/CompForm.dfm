object CompileForm: TCompileForm
  Left = 206
  Top = 97
  Caption = '*'
  ClientHeight = 265
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poDefault
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
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
    Top = 51
    Width = 361
    Height = 194
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
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
        PopupMenu = ListPopupMenu
        TabOrder = 3
        Visible = False
        OnDblClick = FindResultsListDblClick
        OnDrawItem = FindResultsListDrawItem
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
        PopupMenu = ListPopupMenu
        TabOrder = 2
        Visible = False
        OnDrawItem = DebugCallStackListDrawItem
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
        PopupMenu = ListPopupMenu
        TabOrder = 1
        Visible = False
        OnDrawItem = DebugOutputListDrawItem
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
        PopupMenu = ListPopupMenu
        TabOrder = 0
        OnDrawItem = CompilerOutputListDrawItem
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
    Top = 245
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
        Bevel = pbNone
        Width = 100
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
    TabOrder = 2
    object ToolBar: TToolBar
      AlignWithMargins = True
      Left = 7
      Top = 4
      Width = 351
      Height = 25
      Margins.Left = 7
      Margins.Top = 4
      Margins.Bottom = 0
      Images = ThemedVirtualImageList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Transparent = True
      object NewMainFileButton: TToolButton
        Left = 0
        Top = 0
        Hint = 'New Main Script (Ctrl+N)'
        ImageIndex = 0
        ImageName = 'document-new'
        OnClick = FNewMainFileClick
      end
      object OpenMainFileButton: TToolButton
        Left = 23
        Top = 0
        Hint = 'Open Main Script (Ctrl+O)'
        ImageIndex = 1
        ImageName = 'folder-open-filled-arrow-down-right'
        OnClick = FOpenMainFileClick
      end
      object SaveButton: TToolButton
        Left = 46
        Top = 0
        Hint = 'Save (Ctrl+S)'
        ImageIndex = 2
        ImageName = 'save-filled'
        OnClick = FSaveClick
      end
      object ToolButton4: TToolButton
        Left = 69
        Top = 0
        Width = 8
        Caption = 'ToolButton4'
        Style = tbsSeparator
      end
      object CompileButton: TToolButton
        Left = 77
        Top = 0
        Hint = 'Compile (Ctrl+F9)'
        ImageIndex = 3
        ImageName = 'build'
        OnClick = BCompileClick
      end
      object StopCompileButton: TToolButton
        Left = 100
        Top = 0
        Hint = 'Stop Compile (Esc)'
        Enabled = False
        ImageIndex = 4
        ImageName = 'build-cancel-2'
        OnClick = BStopCompileClick
      end
      object ToolButton7: TToolButton
        Left = 123
        Top = 0
        Width = 8
        Caption = 'ToolButton7'
        Style = tbsSeparator
      end
      object RunButton: TToolButton
        Left = 131
        Top = 0
        Hint = 'Run (F9)'
        ImageIndex = 5
        ImageName = 'debug-start-filled'
        OnClick = RRunClick
      end
      object PauseButton: TToolButton
        Left = 154
        Top = 0
        Hint = 'Pause'
        Enabled = False
        ImageIndex = 6
        ImageName = 'debug-break-all-filled'
        OnClick = RPauseClick
      end
      object TerminateButton: TToolButton
        Left = 177
        Top = 0
        Hint = 'Terminate (Ctrl+F2)'
        Enabled = False
        ImageIndex = 10
        ImageName = 'debug-stop-filled'
        OnClick = RTerminateClick
      end
      object ToolButton10: TToolButton
        Left = 200
        Top = 0
        Width = 8
        Caption = 'ToolButton10'
        Style = tbsSeparator
      end
      object TargetSetupButton: TToolButton
        Left = 208
        Top = 0
        Hint = 'Target Setup (Ctrl+Q)'
        Grouped = True
        ImageIndex = 7
        ImageName = 'install'
        Style = tbsCheck
        OnClick = RTargetClick
      end
      object TargetUninstallButton: TToolButton
        Left = 231
        Top = 0
        Hint = 'Target Uninstall (Ctrl+W)'
        Grouped = True
        ImageIndex = 8
        ImageName = 'uninstall'
        Style = tbsCheck
        OnClick = RTargetClick
      end
      object ToolButton13: TToolButton
        Left = 254
        Top = 0
        Width = 8
        Caption = 'ToolButton13'
        Style = tbsSeparator
      end
      object HelpButton: TToolButton
        Left = 262
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
    Top = 30
    Width = 361
    Height = 21
    Align = alTop
    OnCloseButtonClick = MemosTabSetOnCloseButtonClick
    TabIndex = 0
    Tabs.Strings = (
      'Main Script')
    TabPosition = tpTop
    PopupMenu = MemosTabSetPopupMenu
    OnClick = MemosTabSetClick
  end
  object MainMenu1: TMainMenu
    AutoHotkeys = maManual
    AutoLineReduction = maManual
    Left = 8
    Top = 48
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
      object FMRUMainFilesSep: TMenuItem
        Caption = '-'
        Visible = False
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
      object ESelectAll: TMenuItem
        Caption = 'Select &All'
        OnClick = ESelectAllClick
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
      object N13: TMenuItem
        Caption = '-'
      end
      object EGoto: TMenuItem
        Caption = '&Go to Line...'
        ShortCut = 16455
        OnClick = EGotoClick
      end
      object N18: TMenuItem
        Caption = '-'
      end
      object ECompleteWord: TMenuItem
        Caption = 'Complete &Word'
        OnClick = ECompleteWordClick
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
    end
    object BMenu: TMenuItem
      Caption = '&Build'
      OnClick = BMenuClick
      object BCompile: TMenuItem
        Caption = '&Compile'
        ShortCut = 16504
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
        ShortCut = 120
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
        ShortCut = 115
        OnClick = RRunToCursorClick
      end
      object RStepInto: TMenuItem
        Caption = 'Step &Into'
        ShortCut = 118
        OnClick = RStepIntoClick
      end
      object RStepOver: TMenuItem
        Caption = 'Step &Over'
        ShortCut = 119
        OnClick = RStepOverClick
      end
      object RStepOut: TMenuItem
        Caption = 'Step Out'
        ShortCut = 8311
        OnClick = RStepOutClick
      end
      object RToggleBreakPoint: TMenuItem
        Caption = 'Toggle &Breakpoint'
        ShortCut = 116
        OnClick = RToggleBreakPointClick
      end
      object RPause: TMenuItem
        Caption = 'P&ause'
        Enabled = False
        OnClick = RPauseClick
      end
      object RTerminate: TMenuItem
        Caption = '&Terminate'
        Enabled = False
        ShortCut = 16497
        OnClick = RTerminateClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object REvaluate: TMenuItem
        Caption = '&Evaluate Constant...'
        Enabled = False
        ShortCut = 16499
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
      OnClick = HMenuClick
      object HDonate: TMenuItem
        Caption = 'D&onate - Thank you!'
        OnClick = HDonateClick
      end
      object N21: TMenuItem
        Caption = '-'
      end
      object HShortcutsDoc: TMenuItem
        Caption = '&Keyboard Commands'
        OnClick = HShortcutsDocClick
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
      object HPSWebsite: TMenuItem
        Caption = 'RemObjects Pascal Script Web Site'
        OnClick = HPSWebsiteClick
      end
      object N6: TMenuItem
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
    Left = 136
    Top = 48
  end
  object ReplaceDialog: TReplaceDialog
    OnFind = FindDialogFind
    OnReplace = ReplaceDialogReplace
    Left = 168
    Top = 48
  end
  object CheckIfRunningTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = CheckIfRunningTimerTimer
    Left = 200
    Top = 48
  end
  object ListPopupMenu: TPopupMenu
    Left = 8
    Top = 168
    object PListCopy: TMenuItem
      Caption = '&Copy'
      ShortCut = 16451
      OnClick = PListCopyClick
    end
    object PListSelectAll: TMenuItem
      Caption = 'Select &All'
      ShortCut = 16449
      OnClick = PListSelectAllClick
    end
  end
  object BuildImageList: TImageList
    ColorDepth = cd32Bit
    Height = 17
    Left = 312
    Top = 48
    Bitmap = {
      494C010104002400040010001100FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002200000001002000000000000022
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FF404040FF404040FF4040
      40FF404040FF404040FF404040FF404040FF404040FF404040FF404040FF4040
      40FF404040FF404040FF404040FFF6F6F6FFF6F6F6FF404040FF404040FF4040
      40FF404040FF404040FF404040FF404040FF404040FF404040FF404040FF4040
      40FF404040FF404040FF404040FFF6F6F6FFF6F6F6FF404040FF404040FF4040
      40FF404040FF404040FF404040FF404040FF404040FF404040FF404040FF4040
      40FF404040FF404040FF404040FFF6F6F6FFF6F6F6FF404040FF404040FF4040
      40FF404040FF404040FF404040FF404040FF404040FF404040FF404040FF4040
      40FF404040FF404040FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FF4040
      40FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF4040
      40FF404040FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FF4040
      40FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF4040
      40FF404040FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FF4040
      40FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF4040
      40FF404040FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FF4040
      40FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF4040
      40FF404040FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FF4040
      40FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF4040
      40FF404040FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FF4040
      40FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF4040
      40FF404040FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FF4040
      40FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF4040
      40FF404040FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FF4040
      40FF404040FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FF4040
      40FF404040FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFD4D4D4FF626262FF626262FFD4D4D4FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FF868686FF404040FF404040FF404040FF404040FF404040FF404040FF8686
      86FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FF626262FFD4D4D4FFF6F6F6FF404040FF404040FFF6F6F6FFD4D4D4FF6262
      62FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FF404040FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFB3B3B3FF4B4B4BFF404040FF404040FF4B4B4BFFB3B3B3FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FF404040FF404040FF9C9C9CFF404040FF404040FF9C9C9CFF404040FF4040
      40FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FF000000000000000000000000F6F6
      F6FF868686FF404040FF404040FF404040FF404040FF404040FF404040FF8686
      86FFF6F6F6FF000000000000000000000000000000000000000000000000F6F6
      F6FF626262FFD4D4D4FFF6F6F6FF404040FF404040FFF6F6F6FFD4D4D4FF6262
      62FFF6F6F6FF000000000000000000000000000000000000000000000000F6F6
      F6FFF6F6F6FF00000000F6F6F6FF404040FF404040FFF6F6F6FF00000000F6F6
      F6FFF6F6F6FF0000000000000000000000000000000000000000000000000000
      0000F6F6F6FFF6F6F6FFD4D4D4FF626262FF626262FFD4D4D4FFF6F6F6FFF6F6
      F6FF00000000000000000000000000000000000000000000000000000000F6F6
      F6FF404040FF404040FF9C9C9CFF404040FF404040FF9C9C9CFF404040FF4040
      40FFF6F6F6FF000000000000000000000000000000000000000000000000F6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F6F6
      F6FFF6F6F6FFB3B3B3FF4B4B4BFF404040FF404040FF4B4B4BFFB3B3B3FFF6F6
      F6FFF6F6F6FF000000000000000000000000000000000000000000000000F6F6
      F6FF626262FFD4D4D4FFF6F6F6FF404040FF404040FFF6F6F6FFD4D4D4FF6262
      62FFF6F6F6FF000000000000000000000000000000000000000000000000F6F6
      F6FFF6F6F6FF00000000F6F6F6FF404040FF404040FFF6F6F6FF00000000F6F6
      F6FFF6F6F6FF0000000000000000000000000000000000000000000000000000
      0000F6F6F6FFF6F6F6FFD4D4D4FF626262FF626262FFD4D4D4FFF6F6F6FFF6F6
      F6FF00000000000000000000000000000000000000000000000000000000F6F6
      F6FF868686FF404040FF404040FF404040FF404040FF404040FF404040FF8686
      86FFF6F6F6FF000000000000000000000000000000000000000000000000F6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F6F6
      F6FFF6F6F6FFB3B3B3FF4B4B4BFF404040FF404040FF4B4B4BFFB3B3B3FFF6F6
      F6FFF6F6F6FF000000000000000000000000000000000000000000000000F6F6
      F6FF404040FF404040FF9C9C9CFF404040FF404040FF9C9C9CFF404040FF4040
      40FFF6F6F6FF000000000000000000000000000000000000000000000000F6F6
      F6FFF6F6F6FF00000000F6F6F6FF404040FF404040FFF6F6F6FF00000000F6F6
      F6FFF6F6F6FF0000000000000000000000000000000000000000000000000000
      0000F6F6F6FFF6F6F6FFD4D4D4FF626262FF626262FFD4D4D4FFF6F6F6FFF6F6
      F6FF00000000000000000000000000000000000000000000000000000000F6F6
      F6FF868686FF404040FF404040FF404040FF404040FF404040FF404040FF8686
      86FFF6F6F6FF000000000000000000000000000000000000000000000000F6F6
      F6FF626262FFD4D4D4FFF6F6F6FF404040FF404040FFF6F6F6FFD4D4D4FF6262
      62FFF6F6F6FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F6F6
      F6FFF6F6F6FFB3B3B3FF4B4B4BFF404040FF404040FF4B4B4BFFB3B3B3FFF6F6
      F6FFF6F6F6FF000000000000000000000000000000000000000000000000F6F6
      F6FF404040FF404040FF9C9C9CFF404040FF404040FF9C9C9CFF404040FF4040
      40FFF6F6F6FF000000000000000000000000000000000000000000000000F6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FF404040FF404040FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FF000000000000000000000000424D3E000000000000003E000000
      2800000040000000220000000100010000000000100100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000}
  end
  object LightToolBarImageCollection: TImageCollection
    Images = <
      item
        Name = 'document-new'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000020B49444154388DCD91CF4B54
              7114C53FF7CD9B3733BE79F69CE78F4C0BC71C0C9A689115CECB492A086D1184
              82306E82A236D2AA76ED63FE8636B610C59590B5A901235E045181B41A378364
              04F5C2163354D3F7DBC2514C18A45D6775EFE59EC339F78A15116EE5DA01C80F
              388C0E386A75A366CCBF0DB16216CF3E25F9AD34F57A9D4AA582528ADD909991
              0EEE5FEE1E03AA082B82288D36D09CFFA568B91724D6D6BEB1AC94BABDB9B959
              2A97CB68AD77040C041AE40520DF98E71116A206D5F14BA3A16559E568343AE7
              BA6E3E93C920223B0291BB170ED297B22A086F8005416C34E3085388ACC463B1
              DAD1CCE0DCD8D48DC92008664CD37C61DBF67A18865B0E46071C25224A909220
              9D0022D2B9D5A37AAB1F947F48E1FB3EC56231198BC51EBBAE7B269D4E0360AE
              6ED48C6C771CE01CB028225D5AEBCFC024222FAD7E9FF5482F372726240C4301
              0E004F1289443B8031FFEE2B808FB008141AD10A088B68FCF8F075EFF8D599E5
              A5A5A5C120087410045A44DAB66F6026AD08A05B95D2D3224649B63294B456D3
              91647BABB4786DA6696601FBAFF7350E693E7CF505CF4B3DFDF8D3F6F23D8A8B
              8715D1F430F1D385F7466B971B491D790EF4D004E6F71F8AD95585E7598F8686
              4E5EB14E7418D1BEB358FDB959200BF436230398DB85D67AE4DA9D07AF938EB3
              3D3AB6D7F67E02B6E338A7F623EC85F1AF84FF4FC0DCDDE4723969B6D80C7F00
              DBA09E1C89B68BCF0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000002E749444154388DEDD14D685C
              6514C6F1FF79EF9D7B939989E9DCA4C60849C6CCD8069B49522921DD4C1C2994
              16A12BEB4289D636BB801B3F40C48D202206044553AA5DA81111945017E2C726
              D8115763B05D25A6ED68EA544B487BEF4D66DACCDCE342694313A1D5ADCFEA3D
              8BF7C7E1390230DC1D27DDE67061F93AEF1EEEA13BE5444061FC93F2AC63434D
              1DCEFA49541500DFF7595E5E66ABC870779C99A3BDD896E9AF477AB6114153CC
              44A085F05A349B70CC8E6A9DF3CF9D76BD0BBE1C57D52F55F5F8E2E2222B2B2B
              9B40936E73B12DD30F52B28D3CEDC604000512AE7910F8DEB53836D0DBF17B2C
              163B2D226F1B63C6B3D92CA9546A1368251CC3E1DDA93F8CF09B0853C0922087
              507E423881F08588BC4CDF4167E96A7D6F2291B81804C18BC0AF9EE7CD55AB55
              6AB5DA4DF0D4789678CC605BF2235001A6043120FB103E038E8244ADEA377A9A
              ABC57D4FBD70B57F60F0C962B1F80870CEF3BC331B51FBEF036C2E57B040C680
              3180E4EA2FE4E24D05EEED60F79E61EAF5BA999C9C3C09AC6732994FE7E7E7F1
              7D1F1B2828FA57693024F006221680AA9E043E4440C4A66EDCB98F3F9A1EFAB9
              7C1180E6E6666B6D6DED0363CC956432F9B5EFFB580B976BE587EFBFABEC58B2
              0DE104302322038A4E233C8E300BE654CBA36F5E4A149ED9D330CEA0EBBAFB3B
              3B3BC9E572542A151386E1621004B3411060BBB690704C16F806E52BE008F004
              F01E4A5184F7557555AF573FB7E3AD07F2F9D167F3F9D11BD5944A252A95CA8D
              D9AE47806A59D15714DE11310D01048844A654A3C0F27A7EB0B667EE060E6CDD
              B7DC0467CE5C616F6FEB7ABDC97B6B2914F7B11D51A3CF53109B9643AFBABA7E
              6DDAEED8B9DD6A4B7F0BECDA0ADC18BB11C16BDFAD92C9DC93074646EFB35EEF
              8B4505719373B1F4C888696AD90F1CBC1D0CC0DEF07EA8ABAB2BFDC0B19746B7
              F57623963D442C3E083C7F3BD05620EDEDED637DBB72637702DC1AF35F3EFF0F
              02B71C250C434AA5D21D016118FE33B8B0B020131313FF763900FE04B0B205D3
              D9779BBF0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000003C4494441544889B5944F6C14
              751CC53F6F667676BA4377B750FB07502991EE2E56AC111241EB4168BC183035
              5E0C170F6A22260D318D86A496100F48D2440F9A186D35861B9A182F880A84C4
              8BA85B0D54570D1A25055B62C952D242DBDDAF87EE9A6D77117AE09DBEBFF94E
              BEEFF7DEFBCE082011386C6C0900F8F3F21C3FF465905404F61FCBE50F0C1CBD
              40D30A8F59A78E4BF321006606C0F8F838B3B3B3DC085E3C70F8F8D9F56C5A5D
              E720ECD789EB56F942BA39E0C49E76024FEEE5EB2AF47D1D5D313EAD97802933
              7B271E8F5B2E97A35028D42470EE6DAD63D3EA3A47D290D0A1F6A640A51B9A99
              7157D2A72EE2A4248DD4FB6C4E37BA575DD73D070C4A7A2B0C43A5D3695CD7AD
              4D008030E01F492F4B1C42489210205288E388951187AB0F3FD8C1BA75EB9292
              1CE04560F0FF48DCA2C1631BEA5915BA5F2212427B314249DB302E02FD2C5075
              E3B8B999D42EFDFE777E3EEA478753E974666C6CEC6920E6FBFE57894482C9C9
              C9FFF20170CFF577D0187AA5197C8148027B4BFD0E601AB103B39FC148E67F21
              139D18DFB27DE7C59DBB5F78727E7EBE7D7474741BE0F9BE7F321E8F2F227124
              154B1B53905410EA05163C9204340B8D4A4E11A3E0CD4D155BBD2BAFB535B8D4
              D7D7D3DBDB4B4F4F0FC03EA03F0C4352A9148EB3E0BE07EC2FCB29B16E009EA9
              B071CCCC864A399571EACCD99F38F7DB61CC8C6432492291209FCF0F00736118
              1E0C8280E9E969BC63B9FC817453C09D0D3E827B80E3C08499352126803588A8
              19FBE438F8F7EDC25DD5863B19F0F9074774FEFCF945A14A7ADDCC0E96CFDEC0
              D10B9CD8D3BE305C9CC0F0801D92CE18F62EB052E8556460B62FD2B655D14CF7
              C0E66BD73A0F6FDFB5D9F7FDB27A1B1E1ED6D0D0D02242AFB93E4210910B1CC1
              8800DD60A32C845E047ACD4C925E31F4BDCD5FFB04F82808823E33F3ABF67209
              BCD9820114307BBE4871C6D059570E565E03C9CC8ABD667CE3B56EFCD46BC9AC
              30B3F700BF7CF3CA0C2B5714C0FBEEAF69DE3C39C12399A66F47277DFEB822ED
              7DA0600DC18204BFE309DCB5F71BC5E2616FCDA6D06D5CFF99A447CBC34B9B56
              AE91B488C40378E3E4258E4FDD8DA4E7849D9E9829FED810D87EE0947BC706EA
              3A9F1A003E34B3F7CBC36F15DE92F3DBCD2DAD0F45B63E4E6C6DFC008E4BA46D
              1B40277016882E6778158199395D5D5D8399EEDDF9682C56F97C4B79B82D31F9
              A619541E24291E8F77C562B1A5DE56D537E85565E02C57F272B1D4225B4E5DA3
              5765D1220595F22B7E7635EB1BF4A818514D703B70DB2DAADAA26C36BB6859CC
              8C5A75ADDEC8C848ED2FB9F236D96C966C367B33E5B78C7F01F173C274C1A858
              1C0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000003F5494441545885ED965F6C53
              6518C69FF7F43BEDE97AD69D759D2D033AB7B9B90956633A03052F4006732C66
              11412F703113D1CCC4ABADCC3F89374BC415883101630826C4E88D37B838B830
              4282A35C6CBB60BA18F7D7866060133CDBDAB5D8F5BCDEACCDFEB4C5C4C9559F
              AB93E7FBF23DBFEFFDDEF39D0364D1A9968D98EEF67EF1D5E1F26C53D64552EA
              C15528E0B499D203A5AA00804A80D29EA3C004972A2011D64D22153E7CAC6E33
              18F3C1CB77F41357A6A19A4D00C12D2F31BDB3C3890FF7B98B2589D4C3DFFD7D
              333CCB6B16636624120984C36130AF1DCF0A903418609E2792CE059E77775639
              2D930E9B90C1706C289471FAE5CD38F894B605441F2FAAEED6B21A47E3DCF854
              5F06802880668BC572757474F45F41A48BD9B1EB110476BBAA40D40BC651108F
              02F42B333B89A805C049520AF7AA874E472457DD75218467F5627EBF9F144589
              C462B1665DD77F1A1B1B7B2044BA074E5C99C6859F672718781B843E80DA4150
              88E82300DF0078DD54B367422EDBBA53087109C0B700C2AB170C068336ABD5FA
              BDA669CF555757832877C3D0A9968D285565D81509253661AD765A8A89E85D22
              0AA42631730F337F76BFC0FD97DDB325464A11249B03E6DABD8A70D5F403783A
              55815028640C0D0D51474747341E8FEFD775BD3F572568BADBFB25002F089504
              D272E20260B00EC6248061B9AEA1CDDED2D30DA06B3900001A1C1C4467676764
              09E25A3608E987DFE6DA6EDC5AF04DFD79DF31134DD80CC3D8C4CCC75784321F
              370C635342566DA2CCEB90AB76FA2CDE17DBACCFBCA20068CC04EAF3F9100C06
              554551FA344DDB91ED385638DD4D1B7074BBD30FA28B00CE1051D7124C3BC04D
              F01E0A95EC7FFF0080DD001C009E0590BEA9965720E50D0C0C20100844E2F178
              93AEEBA1D5954837E16B3E07DEF4975680E82C989B99F9CCD2EECF80B919A0B3
              3471B962F1EEEFFD005E007070797836D5D7D7A3A7A747B5582C7D9AA66DAFA8
              A8585B01478109BF74D51509493A07C6B1AB93F313E5C566DBA325CA7C52560B
              A5DAC6288DFF58C50BF73E31B96ADF286A3DBF8D8479CD3DE0F7FB73B63C33CF
              45A3D1E2919191B427004036114C4476663EF2F5D03D3DD07B0B17DF7A2C5A0E
              2CC8259E6851D37B2F25EFBE7A2DD2FBC191E4DC6D3B27133324CC6B0242A150
              CE97DEEFF7DB577B020066228B68F87CFC66D2608CDC8E0300EECC2700E6B0A4
              D84144BB84B33258D47ABE9D93896932179CCC15944BAB1B510080C1C0F01FB1
              1503B18401005364D5004003504EC2DC9769E7FF455226D3E3F16024EEA259FB
              E3CD9627F61180ADEB9ABA4C2293A9280A6ECCAA970A0E7C7ADDEC766F03F0E4
              43050080582CD6E076BBF7FC5FC129653C8287A93C401E200F9007C803E401B2
              7E0D8107FFE3AD87FE01B79474641F6C52820000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000527494441545885EDD75B6C14
              551CC7F1EF7F76F6527ADBD6C250A04093B1A20802162AD5122ED550452E4502
              88110209545951A310A3C107438CA634E80B26C50A8888C19020022654208104
              95C68DE225601B45081A4AE96EEC52285BE6EF432F16B2BB058A8487FE924D66
              72CECCF9E4CCCCF99F15BAC9D93523BC827C7E2AD4323DBFE24477DD6F798C44
              8DD9692602198005729B4857C7EC38983BDA4F616E32AD8EF2CED7F59C8BB462
              1A82227E117C5D7DC3FA79297B380B11F8EAF8050ED435271C44558946A33D03
              16E626336F4CE612817D93EF4E3B3D7FF31FA4F95C08F851BCE9DEB6C99E3A2C
              95F5730693E2311E53217ACEB00E8652DDC381AC583020ACAA3FD6D5D5110A85
              6E1ED8EA2802D5205F0C4CF72CDBBBCCAED97CF43C801FF0A6795DBC3AA91F2B
              275B88C82B4089F4C9281934B6C8639CD9BFCB719CA1D7DE5C44001C60A96DDB
              1B6F06D9F9E0FAA698EC7BCE6660BA671CB0072853D51D22320F58A7AAD90249
              885402E370278D4F79724DA30E29ACF4F97C8B63DD3C180C1208044444AE388E
              B3545537DD28B2F323391769E5992D2789B45C390A2C17E13311790DC840F08A
              480E228780C781E99E7BA6347AF2262EF4F97CF3BB1B64D5AA558661189522B2
              C8B66D3232326E0C989D6692E377939EE462EBF78DA8EA76557D4B44DE06DE10
              48018E022355758EAA9E087B2C9C48C366BDDC9CAACE95146028B01268BD7690
              1933666817E4C21B419AEDEB5C269049DB92D2F16B52B4414406008860A96A8D
              88DC0BF4EF13AC0A85835521C468C4ED0B19A9D6DFC9935F5EE7B18B7281E763
              205155A3BCBC7C83E3386ADBF6C7D7F3B84D417622E4A26420F80571C7EB2C22
              6381B11DE78A5E429D46A2CD21E7FCEF3B5BEA0EADF6D8453FC5BB7EE6CC991D
              C80F01B56D7B4B7748F354A8A564409A1BD325A0E0A0A92899880C12D82122FD
              3A41AA8781E5AADA288611C2F45D14771F8CA4748CF401F8464C039896684666
              CD9A056094979757B5233F498434BB96AF1CBF9B67C76634AD986045042A04FC
              AAFA9D8814A8EA61112952D529C0FB170B563070C2821C71B907D0B614F50566
              034F2402762055D558BB76ED47EDC8ADF19066D7931CBF87E58F58B47F1C1315
              9D0A723F50000454B518E15D81DAB4D0CF7B11230FD80DC47D2DE2A5B4B41455
              352A2A2A36B6233F8D85EC04DA591EAAE60FC174C922A0549542E037110601A8
              12025D27481DC88668DDA1A92DC776EDF73E30E34511637D224C30188C59C873
              7373292E2E36AAABAB378A88DAB6BDADA6A626363050D497CC64D704E06994F1
              F5916863D5B70DBCFE6876184044C2D1C297E8D370ECCB68DDE1D35C89AEBFB0
              BFE229337B78A569E52D06F2E301038140C29D868898AABA4944B65DDB66FED7
              090097A2D38ED75FBA3C6FD34986DEE546212CD08AE9895885F306E15A9077E9
              879D079A0FBE3757A3CDFD9C48FD59ACBC73B1061E33660C478E1CD14438E8AC
              38667BED8E0DDCFDCB3F9C0E450FB63A4AE537E769BEEC90EC15A4ADD887C5ED
              03D3D35F44F6F84697AE700F1CB9C189D4FFE51E9C3F0A98D41DE27AD25EBB63
              03AB4F34517DA2E9AAC6965605340C12166F0A22E207DC22C607A695B7A47DE6
              2601BE5B018C153351637D53148546810346AA056D15A62371DFB95B99B83BEA
              ACAC2C46E51718DBFFCC6C6ECE2E284B7A682140D1ED40754DC219045EF8D5BC
              2F7976C9EA331ECB7A1028BB1DA8AEE90E98AEAA6F5A96755B30B192F04FD39D
              905E604FD30BEC697A813D4D2FB0A7E905F634BDC09EA617D8D3DCF1C0EE36AC
              44221182C1E0FF8AA8ADAD8DDBD62DB0B6B6560281C02D05DD48FE05EA45F53D
              FB9464BD0000000049454E44AE426082}
          end>
      end
      item
        Name = 'folder-open-filled-arrow-down-right'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000024849444154388D95924F4893
              7118C73FBF77EFF6E652D486C2F6AE256684216221D2BA1889819075A820120A
              822868045D721278E9601074280F75AACBBA4420AD43055E22D03C7428681058
              B9F7D5A5B9D4B639DD9FDFAF4313641AE9179EDBEFF3F93D0FCF232A5ABBA83B
              3BE8D69CC600701E8189621A8828591C4ABEB8974D8F3FE75F1181DB6FDD9AD3
              35DADBECE90805BD046A0CE28BAB0C8F2588C692134ACA63898757B2B9F8A74D
              05BAE674854F96E0E4724135792AD8E972D0DFE917404734961C3877EAF8E051
              EBD706786A31870EF45DFB0B138A4E8A07BD7BE57B2B250042412FD158B2CF67
              FA07BB77545BC8A26FBDE0E3EC8A434708335063D0E4A9E0FE8946AE4727B5E6
              7A37877C95046A0C10C2940D41BC37AECE0ADDE55D83676E1E10001A4A4DC717
              57F991CA3161A7D95F57C1BBEFBF05407C7115949AAE7616C1E1AC2F1FA1A1DA
              890E4486C712B7C29D7E00DACD2ADACD2AD5E1AF64782C8153C848DBD4D39644
              FF25B35C506568B6AEA4BC138D25BB810D5B78194B4EF498F343C1D6C063C3B8
              ACD6C385B9399646462C5D7B7D77D9DF7DA1EBD5E77C381A4BF6ADDD81539391
              1E737E28DCFA3564188E33E5BFCB741AC0D61BAD373C5AF8D2F9CD7FF8D987DD
              5D833F0B6E6A5D7982750B2D0777A59EB81CEAF466FB97990C80A54BA970A97C
              735BED52F448CB4C029805EA01DF666099C0D6A7530500BBD492B754FF4DE9BD
              A5255205145825E396532C75A0A572924C4EDAC56D0A6426030A5B008C5EDCE3
              D8E7317242882D0B94522CAD145D3AC0B89D2D660BCAB15D81B594E70F1680FF
              8B96F41D350000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000034D49444154388D9D935F4C9B
              5518C67FEFF77D954007D88C16045AC65C8C6E02924CB300EA9C5133252E26CB
              60890B46C3328D5E189DBBD15DBA44A631C4191375F1CA2CD10BB9984D3647C2
              A2DBAC6036892423388654FE08B45D5B285DBFEF7BBD901171C5C9DEE4DC9CF7
              39BFF33CEFC911CB1F22D0F13E9EF5D5952272086845A84289036755E9C2B5FB
              E77A8E928E7CC3ADCAAC78E5333C65C11611E9DB52EE7DF4B5A6CA3B3B1ACBCD
              476A4BBD2E6C1E8D675F448C44E1BDCD91ECEFBF62C7A2FF09949A23172A45F8
              E5F9C680EFF08E10A6212B043D43731C0C8FE238DA6A4F8F8439FEC2AAB0ACAD
              5822BCB5A5DCEB3BBC23C4EB27AFD0B2A144F7D4F9F5DAA2CDA73F4D199D0F56
              E8BEC6807C3130FD6ED05F12FEB2F39EA085B3310F2F717E7CE19201B4B6D597
              611A42734D09EF9C1A93AF0667249575E4931FA7249575646F831F1169C89985
              21DB176A017AF3AC0F002C44AA42A50500B4D5FB0178FBD498BCBCEDAEE5AB83
              4B7DDBE3AD2ADE7D74DA5F7B9FFED35AEAF43152A78F0160A09A88656C00AE2D
              DA34D794D0F950051F9D9F5C1E667CA9EFB1AC44813FB46EB519FABD26167036
              3C1CDFBD6BF37A3EEF9FE6E30B93F26F6178388EAA4E8672D1CBB9DE336DA942
              EFCAC7F82D02AA1BEEF6DDF1A6A54AD7772389E77A86E6CC97B696B3A7AE6C65
              9CAC43F7B9092CD1AEA7035771C77ADB3376EE26774649498D934C7658B876BF
              2BD61B07C3A31FEE6B0CB0B7C14FB0B48078C6263C1CA7FBDC04A945FBEBED15
              B1EE9D75DA595CF8D4A67C7117221132172F8E8B6FDB2E82CFBECABC14ED74C4
              3C2222F53744AA3A6989763D5C1EEB3E5477A5A9BA28FBAD0845F980E9DE5EB2
              2323C72DEFD0494E345FF50D16DDFF439F6FFB03839E4DA164CEAAF2181ADFB8
              6E61F899EA199EAC9CDD5FEC71DE5B0D06E0CECF03442D8075EE82AF69A13FF2
              C46315271C7FF2FBEB6AFE698A060A4DA7DD14DA45C81B330F70DC9ABFEE02FC
              816AA961C881028F1E28C2BED5F915A5AACB4023BEE8329FD3ACC2CCD2E69A4B
              3319705D51881A0093E91C40D44DA76F0BB86444456F005336C0F8ED3A74D369
              5435019AB600A6FE0646ED89093203036B06DAB3B30051052C80F1640EE0673B
              16EB73E2F135035515E052346923008FD77AD9BFD50780C84D5FF97F432FCF66
              F90BFAC07407AAE3B5840000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000045C494441544889A5955B6814
              7714C67F6776CC6E1263B68D6E4C93D5041B25EB858614298835A4155A2D4A31
              E283A5F820B41404A194A4162B15447CB07DB308B6D060449F4AA12DB64562A0
              50916C551A3522B1D1DDDC36C624CDECEC75E6F421BB6BB43111FA3D0CDF70CE
              9CCB77CE7FFE228BBC54ECFE9CD2F52D00EB44E400D0825003C4517A513A15ED
              50279B1AFFFE04F1F08F3C2F64E9DE6394AE6B1144DA44E48BB222C3DC5C5BCE
              0ABF9778DA213C64713B961055BDA14AABAADB3FD67988C4CDCBCF9760E5F12B
              88D00E1CDB19AAE0704B10BFCFD4D94E5DF7A6A4EDE2008FEC4C44958DCEF444
              6CF0CBDD68D27A9E047FAC15C3F87367C38BE6C96D75DC8CD98402256A880880
              AAAA88C8AD98CD9E737DD869E73CE8DEADE123548E5D9B37F8DDF134A6881C28
              2B32CCC32D2BB819B369EDECE3FDC6009F36D7B82242FBC5010138F1761DFB9A
              029CBA32DC2A4AFBD67A7FE4B5152FB48988375708B99AF2BCFBB77EABDB04DE
              D85C5B8EDFE7D1725F09EF352E936F7A46C41068DB52A391A99448AE931D0D15
              72EACAB0A9C2965465E8AC0CF7B56A26D998AF58F5B1B2AA7A14E83611A90EFA
              BD001822F25973500139D3336A00AA8088202212F47B55440C55AAB3759B58BE
              FFE0A8515C960FA82222A00C7E122A746382DA898CEB2D0C45440E35075145CF
              F48C8A698836552F0620917124E766FB8221C4B7B8F0DD5C300D30517AC383D3
              9BF3551CFA6540EE4FA650C02368C671411555D59EA825AAAA264E6F45EC06E8
              9BF58A51D0451F6B24AA4A736D29264AE7AD58E2F5AE7B53B4ACF20382E46479
              B5A60C54690894E02872FAEA08C0839764E2F7AADBE71B867E3D58234F6D5B3E
              97881C51D57DA6A21D281FB55D1CD8F0DDEED51C7FABF6096755554791A3971E
              706DC8C2146DDF56F3305359ECFB50024D0828CC3C66F3F49D3BEA5856D45427
              9BC263B63EB23397F69CEB0BEE6B0AB0A3A142827EAFDA6947C283969CBE3AC2
              B5210B038E6F5C3A7961D79AC90D25A56BF6E762CD89747FBF0051296DDACEF2
              5D6D38B22800F215422B60CEEE0078608AB66F5C3A79E1E3B57F57AE29B72F8B
              50FF74A705EEBA3AD1D1219A4E9F34E3E19FF876D3187F95353E0CFBD6EFBD5E
              D4D03E45C91657677E768B0CB7B7BA24F9FBB6EAB1CCBB2B47375415A72F8850
              3FDFF6682A2564B3A210350196B9E3EC485EEED8BEE4EECF13A14D3F444AEBCE
              3E4A15E13194AAE224ABCAEC508537F38147747FFEE4CE07D7B641158188497E
              2AAA4BBC6EAA63F5CB4B92AB19BF0D8C025EA01EA82E5437FBB83EF59EE7AE35
              B3CEA0331DC4E259167B3D51371E07F001AFCCA9ED93ABF84C9B1B8F23A0AECA
              A00130389D0588E6B45B488105E15A160A09D08746BE0320A28EA36E2251A82E
              DFF25C7C3E5B4E8951206300DC9FCC004445445C6BE612911C9EC5E7B3E51244
              010C80E1E92C2891BC7EFF0F4AAEC8284061C88A4651D4EAEA82AE2E99F5C5B3
              F842B6E83F29F7710220091C2D783F793BFD87CF67CB8DA43B666767B27A3DC2
              D7EF54E5232F28C282C81D8DEB2349FE05DAE174E80E66F7B30000000049454E
              44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000458494441545885C5965D6C53
              651CC67FEFCE29ED58DBAD6B3730906E8C6D31736C0C3A32D111500943C0E80D
              94392EFC48BC10F442638C261AD444424854D00B839A98B93916BD91108D1A35
              11920EAB0B8C90A00EA4ACFBA2A1B33DEB3E7ACE79BD6837362964D9174FF2DE
              FCCF7BCEF37B9FF37FCF790569D97DBB706DDFAF2839B97EA011A84590070C21
              09022DE6F8685BF4878F8DF8E9369092F9900070ED3880B3FEC9320127EABCCE
              EAA6B505D4AE74E0CA56898EE804C3719A3BAF1308C5CE017EADF3BB3F23ED07
              E707C2EEDB45D1A14059E9E18E81963FFA0D5DD78D4CD275DD68E9EC374A0F77
              0C141F0A94E73EF4D4DCCD0165D9B3C7942C8BEDFBB7B71697F8AB0B00A41042
              FC7FA21082CA653914D82D4B7FEA1EAAB77AD77CA2FD7E4ACAB1C49C00B294A5
              4E7F9DD759BDA7CA8394523EFDF55FA2E35AFCB637EC59E3A1CEEBACCEB258FD
              F6F58FCEC91C200B4463534D0142088410E2B90DCB79E1643767AFC52530394A
              8F0405A974D8575300D0682D5E3B1F00F86A5738260B755E271FEC2CE1C0C96E
              71F63649F852F37DF6BC7C2A0BAD2CB72BB3065085102E57B63AAD58E775F27E
              1AE2C3C756CBDA958E69D75DD92A08E12A732FE178535179D2307B83BDA3DA70
              72E6BBE2EAD038077FB98E2AA51C8A8EE86E4F8E65DA84FBBD4EDEDB51C2FE6F
              521053AF4547749072C8E92AC056B8A9497477BCB6D1ABDCD2B877D2F9815105
              40057E0B86B5868672D72D933616DD84985A0F863580E0BD2545E46F395A212C
              5648F5CA8CD4FB4AC5E4F35490ADCD9D830DDBCAF298D87DE986CB282925CD9D
              830868D95AEEB6098B75CB4C8D334935C747DB0221F1D289AE48B5BFAA0080BF
              5FF6DD6E35A2BD2B4220143B5769B9DEB6DE7ECF33909B3B1BE3E23C0B9F3DBE
              02E178702FF93B5F2CB728E2D7B71E29F2EC5EE321C377082925ED5D11DEF8F1
              6A245B24EB8F593FBFB13A74E63CA6B16C3600A987620884C0B3FB4DEC350DE5
              405B9DD759BDAFA610DF0AFB947F814673E7208150EC9CC3A2FBDF5D77E9EAE6
              C2C829A1289B67EB6D6A1AD1D6D65E152979A2FB28DB56755DF9CABEBDF66CE8
              BEBD8150AC11F0217021890241816C5DE78E7DF96AE5657745DEF0B742289B66
              BD72C04C2400C22A408E0A1BC62F7C545FBCE4E2E555E39FFE3CE0F9E2522C87
              615DC5A618943A126C5E7EC351931F7B5ECDE275C03D1773007378F826402461
              007244B5598F54B913EF54B94367808B400C700015C00340F65C8D3302F4C593
              00E174D1063C9C1E0BA609802C803E4D07E84917174569AF9EA900E1BB00904A
              203A629234CCBB00205300007D713D6C2612C8793A6CDE49D2343135CD18D3E5
              959B009A3E22753D2AC7C6161CC04C2490A6D9FFCF50D29804185CC43E90E9F7
              DF17D7999A002CD24E98D8017DDA5480F8E2256026261248DE92C0E2000CA7FE
              03D31218980048CCED9C3F33805402FD197A202C17AF07C2039ACEE471B85FD3
              9152F6247B7AC48DE3C7171C42A69B7012C094703A94B890675314329C88E6D7
              5D923424FF8E990BEB3313FD07BEC21C88B07F6C2F0000000049454E44AE4260
              82}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000608494441545885CDD85F6C5B
              571DC0F1EFB9BE8EFF257692DA59D33589F23F6559BB069A2E85411A5A243481
              4A816E452ACDD4874A081E5631D0B622FA0769436205318100ADA80F7B604C19
              5BD792B69AD0908A1234B5D5EAA41BD52A12B0E7344EE3C6F6BD8E63FBFE78B0
              EB266BA3A09090FCA4FB707DEECFFAE877CEEFDC632BEE8466C3FB857D946F7F
              0A652F69554A3D05F4A06801DC400C6108F8B3889CB2A693B762E77E45F2EF6F
              B29CA10054898BAADE9FE1ACEFF02AA54EA054EFA6B56EB5A3A99C66BF0BB7DD
              C6ED5496E09841FFF518A1F88C81C831414E18EFBF634DBC7604ACDC3201351B
              0F1CF825AEC64FD702E71BD7B85A8EEFAC636B4DD97D137296D03734C10BEF86
              88A7B3A781279357CEA5275E3BB22C409BB77B3F655BBEEA55F0D76D75DE9657
              F7B4525FE99C3741538A871EF0F0A5E60ADEB971BB3599CE35DBD736F6656E85
              C88CDD5872A056DED38B82138D6B5C2DBFD9D584A7C48688C842890D954E5ED9
              DD8CD3AEED514AEDABF8F27741B32D3D50E98E56A554EFF19D7545DC77DEBAA1
              FE188C2E98DC167073B0B31AE098EEF5EBAEB66DCB0054AA77E35A8FBAB3E694
              52AABBC1C78F2E8CF27A700240665F7D4313FCE42FFF2A7EC1FE8E2A1CBA56AB
              94DAE16AEA5C72A00E7C7147936FCE874F6C0C0070F8C2080AF8C6C3FEE25838
              3EC307D154F1DEE7D4D9B2BE948B23F11E7BA0EEDCD20315CDCD7ED73D034F6C
              0C2002CF5F18514A215F6FF7DF273D1FCD7E171747E22D35154EF6765592B384
              3F04E344CDFF7DEBD115CAE3B1DF7F713FB9298000CF9D1F51303FD263B781C2
              5DE3B373A8CBBF0124F7FD6DFEEB378D1C596B71B09397639CBC1C431791582C
              959DB73C7B370510119E3B9FAFE4FD9E89A5B220DC2EABACC2EE69EFCC86877F
              AF145497698BD3C151AF433B06F935180C8E19DB1F6FAB9CF7E96F3D528500CF
              9E1B516D0137A58EB9150FDE3400861AEBEB59D3F5EB666B3A2156626251B2A9
              D32F928D7C58BCD781FEFEEBB1EDCF7C7E3D364D1507FA862608C767E6243754
              3A191E37D5D69AB262254763D30C8D990A38DBD558855656FE98CD1B80AA8645
              0135D7DC37982E22A74253E91FF70D4F78F63C1C280E7C306ECEE956804AB79D
              AD6EBB6C08DC6DAA97073EC612195CE72DB9F468ADB75629D5B528D93CA15BA9
              C42D9BDB7BEC8577433FFDCC836534145E73877B6A174C3EFBE1246F5E9BCC29
              E4D05EC7559CD3EB9EA624B0E885373B0E7454F0CD877CA8D2CE5DACD9FD434D
              A1DEA8F6967CE5E4EE665A03EEFF0AF74CFF3F4967AD1F74BA422FBD98FA5DBB
              3D3AFA1E50B2144000044B01F8F71EA774D34E0770CA69D7F61CECAC667F4715
              3EA77E4FCE486C9A97073EE6AD6B9339117976832FF9D22F36BFEF5D9B19BF28
              99CCA796CA960987495DB932AE003C0E9DAF7DEF79AE55743169F3ED0375DCA1
              6B355BD697D1E277E2B6DB88A5B2046F1A0C8D99CA12195470E8D1406CF0E823
              1FF9D6B9A74F2BA53EBB543880D4D5AB9883835714C07AAFCEDF0ED457244ACA
              1D6F573E3EF6A7926E7D3CEDDC09F4002D28DC0831604821676B3CD397BEDD18
              6657EDF866976EBD0AB42E250EC01818603A183CA303140E57765F363E7CB0FD
              E6CF7B1FBCF4DBCBB1F2FE4B93DEFEB0E964C6D2706A16359E145BFC536CAE4C
              B4B974EB69603FF9AD6AC9C3320C80900E3096CC02441171690EC711AF530E77
              57C706BAAB63EF01A38009788126E073403B859F0BCB1592078675809C40D4C8
              4A55A93D6219461DF9AA3C56B856240A150C17F7AC712307102A0CAC68880896
              692A66030BD31C5E15C054EA4E63DC05465611B068100915815163750145640A
              481681A178065609303735057003E0EE1427F21594745A4936BB32B242584612
              60F4DFF10C739B4424046099E64AD900B00C1320149A0D8C9A59042208D64A4F
              F39D3D303495BD0B8CA70573C6CA0832BE5A8091E42C20AC8EAD462C0B49A514
              AB1568996671938E1A9F04E63B39B4924D2286018288483892F8047035BCEE2C
              D344904950A979A758561258681080093337F7B019496400095BA6A9E267CE2C
              F81FE17284954800840B6FB6B9C0A89103D4B058D6D16C24F2FFD7913F6A0157
              23C93C70CEA9D8EBD038D051B102AC7BE3A3C9346FFF23B9D28C85E33FA8ECC1
              EF1FA5F11D0000000049454E44AE426082}
          end>
      end
      item
        Name = 'save-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000000BA49444154388D639C1730F7
              1F03A9E03FC37F06460646060606061606060606FB2CABFF0C0C0C0C07A71D63
              8C5F9BF01F9FDE85C10B18EDB311EA9948B61D0D8C1A008D0564B030780123D9
              06C0A2930800B784FA5E3838ED185E2FD86759FD43760186010C0C0C0CB85223
              B6F0C1E985AAEA6A460606068261C2C4F0FFFFBD27179F115287DB80FF0C0C8E
              778EDCBFFBF8C2531489B6D6D6FF0C487EC50518C3B44319DC55DCA5191818F6
              323232AA11B412292B3330303000004F87325F04D828D90000000049454E44AE
              426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000014449444154388DED94CF4A02
              511487BF3B33B970244D66A14830E2B695968B0A5AD6B24D0FE0A6A097909E23
              0B021F4217EE0A0277E1569CB2680299FEE9461A6F9B14D2D1AE18ADFAEDEE39
              E7F77138F79E2BCEF7CF062C2289442086470320B365CB8865D2EDF4685E3962
              F7644FAAB0AAC58AC86CDB442C530EBD0640C43289A5A2A3C2C45A42B9C171AF
              A6EC54D43F7071194141B7E1FE2EB05AAC88A0F8DCC0582ACACEF1A6D2A39EA6
              BF99A1536F2B99ED8D55097C1B4F20F0B6DE162B765C86CC5020A8DFEBF3EC78
              E20BF8738700F9427EEA4EBB0D976AB112989B39C35AADC669A934AB240028F1
              DF9EDEE732CD044A64A1757DE7DFDF3C4E24D3E934D96C762EA03118F8654DD3
              FDE665EBC2EF7FE8D1E432009EE311B7E38443E189CDF11C0F809787D7D10D77
              3B3D00442E99E368FD105DD30F80B2106249B99DB1EF1FE01334FE6334D2A0D3
              3A0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000001E3494441544889ED943D6F13
              4110869FD7BA0042E2EC2E964D08BD854C014850F017884414F117A8A12175F8
              099450818453D35A42082A0A1FC5552050145DF035E86C890F01F250DC9DEF23
              673BB11069986277666E7776F699B9D5938DC713003333493AAA5EF90D8150EA
              37337300EA2DD71A6D179045C1882818D1DDBA6C491840157AD1F67A03EA6D97
              46BB6E6044C1D8A260846366D668BBAC5F59B3242DC507747381168BD71B288E
              733E89B3AF2818E1C4578CB390241202C745148B48D1A4716AC7C9721971CCCC
              62967146984DB3CD675AA5976C81657632FD4774F2889C5957F37A5E1A46A00A
              BDCA3E2C331179BDC1DC8DD572044417AF5D60FDEADA726F51ACFFDB2E3A8136
              4DE4E5A337A99AAF62B9A20290C4CDBBD78B2D3CB34DC978AE769AD6ECACE6E2
              5AE90C63E88784FE5040A11E0BDB14A0D969D2DDEACE2D32BBEF14FAC399316A
              6544E935F3FE07DBDBF4FBFDC29A0C876543DE9F4C3549FAF5FD3729A234C304
              1D994E614DE65336E4FDD336357B71E00FF5D90FE7FF924B8A63B0C9C49EBF7F
              F5F1964D8CD6A5E621440F777626E986F263574694B9932E1AFF18FF74CFB877
              CCECD987D79F6E7FFDF28D53675790A4A11F1ABB9E167511A0BDB7FB532C9811
              1D8C0150EB5C8BFB37EE513FEDAE203D0536FF069A54FE00364CA597608847B3
              0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000016A494441545885ED973D4FC2
              4018C7FFD7DCE82013AC9D5C1870D184A5AB3138F1017C1951BF806C1AFC021A
              171D60F165704457498C8389539BE058A386C000A7426262DB73681A8A42B570
              E596FEA64B9EE4797ED7EB937B0E900C018095B91CD4841A5D15EEAFE6D2EAB6
              706E5C8002809A509149659CE80C00707090BE82C94C05802BE0A115B29E2B6A
              477764F5728D4300957C99689B83B9BDB522A2C024C402B1402C100B4817A041
              C14ABE4C82E2910AF8EF05410CDD8CF4238805A40B047681877F80088356C83A
              18F1F385120080B0D3D17F5B58FA118416D8291609FA73EEF40544E30A70009C
              F7D8EB9B1C01F6C90020A757EBBDCE0B9BBEC0A97E06BD69D49C2F7BD9A83E76
              DBCFA325F64B258E3F5A2BB480E55838B83F84DE346E6DCB5E32AEEAEFEDA78E
              A81A810CEC842A14DB0B5B4827D38B00AE0921B31365FFF11CF3633253D9BDD9
              FB1DA40AC5C6FC3A9233293738EEC71EF220F5D3F868E0F8E164CCE402F9067D
              DD6A7E7E719D050000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000264494441545885ED98414FD3
              6018809F0E261D1B4E81CC813161714462381813B97810A3261C3D7A07FF817A
              24911BFA17E40778B39C484485444E1EC04443B791495C22B36BA6C5E286B27D
              1E06650C581D5DC90E7D6EDFD7F7EBFBA4F9DAAFEF0B2D8E0410094618EC899F
              7E76516BB2CF572343C6C8D00E30D81367FCFAF82D60F4D4E4604F5000528DE0
              82A22A8B96E02EA36D67DA2643BD416BA2F86B9B7249D01DEB76C52FFF258FAF
              4D42EEEAB0E64C7D8BD29F12C022704090506F906BF787ADF1FA870CC5DF7F19
              9B1A7345706E720EB9D3CFC08D4BD6DCCAAB4F18DF36ADB1CF95CC4DC413748A
              27E8144FD0299EA0533C41A7B4BC60BB5D80A999ACBC5C7125B9A999C803E7EB
              C6D41594BB3A68F7FB587F9F6EAA9895DCEF3BF0AB75644CBD8BD1A108D1A148
              53A51AA5E5F7A027E8144FD029B6DFC16A760B1AC93EF278C2FD674575616647
              4382009747E3227EFB6445FEDABB35F4A4D6D09A8605439110D1E168A3CB00C8
              7ECE362CE8680FA6D3690A8582935BD8E248F0C5CC8CB4B1B1D12C972369F9B7
              B822280021D8D9DE4108517FC52953FD04674D7DEB47E2ED1AA2DC3A923E8054
              3E8551349625B8F75DCDE5D537A9FF928CC562420E04DC17D4B6724C2F3DC728
              6E2E83B8AB2573FAEA7C92B28DE4C38909FAFBFADC1704C89A59A6979EF1B368
              7C1470474BE9B9D5D749CAA5B2AB02761C3AB6A2A1284F6E3E262C87AF02F3C1
              EECE0BFE801FA83417E57080502474A264A66652340A5437490FC5541A98538A
              AA3C55D4D9C382FB928F08779CBB82C403497274FCEE737CCBB796054555168F
              15DC931CB9380240B3FCEA35CD6B51F504093DD1A4C42EF20F4D0CCD4F9BFA90
              E40000000049454E44AE426082}
          end>
      end
      item
        Name = 'build'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000015B49444154388DAD92BF4B42
              5114C7BFF7BDF07736242E4A3C1B4A1BFA45701BA2ECBD700D091A8468E85F70
              52C95968A9B1B1298806877E40280D3508D91448D020813C028750978C7B6F43
              44BEF296619FE91CCE39DF73CEBD0790600B8411CA95B2A15C29EB8C2CC8D2A0
              48233DD2B7C040A7E39E8AC1BB985059A3CE1A57879F5D1C1EF83777A07A7D4A
              F3FA88B76E4F2402D331D883E15300FBF660F858080010185E4D427178B60018
              985F4B48055EEB8F80401204E7EAA04F00E263820D00DB4208FDA556B1AC40A2
              D128E2F138344D038882F6C81C2EABCDC98B87E7B336E35508A83655092C85BC
              2BCBA343F7EE5A19E00CA669229FCF039452CEBA706736D9CCEE0D9BDD2BB3CA
              53AB5B0AA39472CB0A9D44FC2E1CAC8F0300C67CCEDE7EE12B137ED74F61AB40
              A15044A1582486AE0B00BFDA86A1BF17CADEA01728A5FCDB25A6D219924A6708
              0021B32D05FF3EC15F219452DE8FC01B38AA13D8832AC4950000000049454E44
              AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000026049444154388DCD923D4C53
              5118869F73DB6A5BFA732FDAA48D34708B31C1041D04AE5244ADD218F087451D
              DC2C26C4C481C5410D83A8838931C1280CE2E664D4105D34110D091053065D64
              68E00E1DD0D040221030947B1C4809D062312CBEDB7BCEFB3DE7CBF71D4111A9
              A7DAD09AAE3E07981D7D7725F3B2EBAF79A518F05FF5FF03ED1B0FB4E6EBF81B
              2FDB81ECF4DB477905DEE825769DED50007E8DBCB6A6FB1FFCBDC392EA18085E
              21E82A3DD741C9C1A6D53BD7BEC3949EE910402FF0D85BDB5ABC43696541F214
              C11B24D21128EF944890A0784A11826EE00210974B0B79409BDBED4655550281
              00AAAAE29EFF8976A0713C2BC5A8254537821D801F00218E2A42241C8A68F6EF
              B47DE1C3433C72015555713A9D2B9196969663EDEDEDC4E371ECF6958697962D
              BE4DCEF33E3573FAC5D7A91BBFB3D60C60733914DFC5EADD776295EAE7437B3C
              B81C36002CCB627070909E9E1EEC994CE653301894391880C3A65053E6A5A6CC
              4BB4DCC7B5FE710DA0B7752F0D15FE4EA073DD2214059FCF473A9D167933DCA8
              E3119527E72B41424385BF583C7F29857422A26E25960F1C1A1A667171915028
              88AEEB727878440054EDAF920063DFC704407DFD11699AA6989CFC81D3E9241A
              AD2F0C4CA7D3CCCECD61B7DB0987C34C982600E17019C0AAAFABAB656A2AC384
              69E2F578D6B7681886954C2697B7AB6432B96C1886557086376FDD1600276331
              A9EBBA7CD6D7A700B4251296699AE2E3C08000B87FEFAE05884D6798535B2221
              01346DE5C3E67C281444D354745D9785EA36054622FA5A2BD678E172B9D0346D
              D51704A652A9CD1EDCB2720C611886B56DDA1AFD013E19F1F112C6B2CE000000
              0049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000317494441544889B5953F6C53
              5714C67FE7BD1BC7244EE21781636080F784AA444434312274E9801D65A92AC4
              D6485086A40506E6F2A74AD516981811282291182A95481D8AB2B0242C4C1D1C
              555513B54506AA0E10AAC42DB2EC24AE4F8717270FFF69ECAA3DD3D1BDDF39DF
              3DDF39E73DA141DB77F11EA1FDBDF32242692D977C3E916C28CE6A94E0DF5AD3
              04AAFAFF12884853785333494B2B2D31178062F625A5DC6ACD60ABDDC1447B00
              D8587E8A6EAC354060D9C4CEDE64D7A163166015FF582EBE98BCB0755D96C874
              EF27FEF16D4CB427A4B05E7892E6D5579F502ABC7E335DD5EB4D2BE143C740B8
              0D7C63BA62ADF17377B023DDFEBD086287CAC93B8147A25C0A7B8318676FF57B
              6BD60EA02C20F21E22F74D572C6477C5B62BB00D26DAD30ECC0209841FEBA5A9
              22D08D02F9A5C7283A097A11D5F781AF815040A65DC0B7C03BAA7A5A5567F33F
              3D667DF9691581F4F5F5D1DBDB4B7F7F3F9D9D9DFEA96D308993FCF06A8D5F7E
              2F9CFFEDCFF55B8A3E40890396C2AA850CC73B5A3EF4BAC3F78FC4DBB0BF9F45
              370A00E4F379161717595A5AC2C4E3F1CF464646747474948E8E8E6D1980DC7A
              8967D9020F7F5E5D9CFCEEC5A992AA228211E4FCF1BDE9A4D7F5D601A775221A
              36F0EE85AD11CEE7F3CCCCCCB0B2B2222697CB4DD8B64D2412D1CA256A0F591C
              8EB57138D6464F24A45FCCFD0AC0E7C307F48323BB07808137E4DD8C0F87C314
              8B45C966B318F1691560D34755B5D23F33B8DDE4D1B7F700482D5CC007EA2C5A
              3D3B3DB0479B5DE5E63F154DE24D50F84C26C3DDA96900F13C97F1B1B1D2D4F4
              B495C9F8E3F7D1F81880DE9D9A1600CF732580111F338EEB1EDCDEF8600F1CC7
              6138950220EA44111149241278AE47F91ED80113F52BADD503C77148A592C186
              49627030D83C44445229FF67B359BD1C4D242A9B5C5BA2A07FF9CAD5B22B37AE
              5F2BCDCDCFCBDCDCBC6C56A0C9E409BD72F553ABFCDAEBD7BEA42257B544C131
              2BCB503EF75C4F24E597EDBAAE8808414C65FC8E635A2983E7B9B8EEC12D1900
              49264FD494256875256AC4DF01E7138888A4D3E9608FCACDFC477F27DCC2C282
              5FE6D0D0D05FF564FA2FEC6FBC7875AD0EEC1A9F0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000027F494441545885ED96CF6B13
              4114C7BF33BB9B6C129B349B5C4A52C15F6B4A41E2C543DC1E84620A8D3D3545
              0505FF01ED596B91A29EC5B322F1A4F518C436172FA1520F454A50ABA78A454A
              D3D264BBC9D264673C5883629B2CDB602E79C7E1FDF8F09DF7DE0C8143535293
              90425158D5328AB3334ED340741A281F8BF7B9A3036BF5F24604C00FA779A8D3
              C0765917A00BD00568BE074417C2E35390C247A9F1618E95F32F5A26F49D1D81
              FFFC656AE945B6F1F21EB8B9D3D4BFA9029E53E770247E517347638B4A6A32EC
              1FBADAB27878625A714763EFBC03DA05DF99E196C04D012C7D0B9CB32510E800
              72CAE84DC53F7465FFE2F124C2E9E9202134071013C07B4B2F1E0E60F7FB4714
              67672A9CB14B007600CC07476FF5F6687F43F8E2498427A60384D2790026C047
              B75E3F36AA9FF22D01484B0FEC499BBEDB43A8300740E09C2761D5BC5472AFB1
              7A2DC209312815E60170C6F94829F7A45C7AFBD44E6A90482482C1C1418C8D8D
              A1BFBFFF4047DE3780C54DEA7FB3B29DDB36EB96CF456F546AFCB34724B14A8D
              3DF3CBA2307C2290D4A27249F8B674609EF5F57564B3592C2F2F637575154455
              558442A14A3E9FAFDB212E9975E1FAAB2FF26E9DD5BE6E9AAE932179D72D52E9
              795A3503B268D9C9A1699AA8EBBAB75028FC1A43C6980C80DB090EC82232E32A
              AECDAE4800E01288941957119045D94EFC5EBDC6D53B5A44BD1E1199B48A544C
              E199F469F47A1C7F2B9C7F4814AF8447A9E38E0BFFB68EAFE28E03EC7B05B7EF
              4C359AE4E183FBEC8F737AC8F37FF64EC71520AAAA22180CB28585055B63D80E
              4B2412C4300C5A28143AAF40CB316C433F347D6F3AAE40B7073A0ED068C24422
              61EB73D27600D3346118C67F57A25AAD02007E02A7E2EC3215D0234700000000
              49454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C0864880000041C494441545885ED975B6C14
              5518C77F672F9D9DAE6D7769BADD6DEDF6AA88B656B78DA0314A1BA2D66D9468
              1A2D262852303186187CD507F0CD18A4BE084221124242423424B4C687F6C178
              0112760D118DA4711B4BD996067AD3BD75778E0FB69BDDE296A91DD392F4FF74
              CE7CF3FFE637F9CE77E68CC000594B2A29DABC3D3DBFD5FB295A64CA88D4988C
              48622E28A6A0B9FDD982E6F68182E6F65293926F445AC020C039B981CD80CDC0
              9C8602FE2F5A035CAED60097AB35C0E56AD5035AF4DE68BB7F13366F3D5A3CC2
              F4F7A7414BE97E88C853297CF25584C94CF4B71F890F5F311650A97A98D2373E
              46982CAF006795CA86D8F8A9F775418A3C95D2370F62AB6AF40275854F6D1BB8
              DEBD9DE4CD6BBA007595D8525C8130591E12429C04BEB4D7B728259D1F82C9AC
              17EE5EA01FC10993926F3317B974C1E9064CDE1C466AC92B52CA2E21C433C019
              7B436BDE6290224FA574C7416C558D65403F8875485ED4E2D1586AEA86B180F1
              A1CB8C9FFA00A9A5BE9052BE85A00D386D6F68B59674EEBF0D52E4A9B8767C82
              ADBAD1030C002E90CF6989E8A5B163EFEA2E2FC0E235CAD0EC8D2166C742E4D7
              B70405A6314C622FF0A0D555FD9552B14133DB1D8F0821B64A29BB95DAA629A5
              7C7DA9106200F0484DB6C944E2E2D8F13DC4872EEB8603B0582C161445415555
              F2F3F31142E4BE7BE677ECE78F636DD97DF8CF59CD9A42740B38697555BD36EF
              12405E49A54B08D12F04E56621DA0A54F3F9E8378770CB29A8ACCC995ED33422
              91089148844422412A95C2E2F57AA9A9A959DFD9D959D4D1D191B4DBED777CAB
              9978920BC3333F9CFDE5D689AFAF4EBCAE9844B2B658BDFAEB789406B7DDF7F3
              E85FFB6C56F3031D0DC5EFB5D63A624DE5F7F8D4778EDD31EFC4C4043D3D3DD6
              BEBEBE3F060707C3232323FF6C333333339FA552A9A7F5C001142816B6D439D9
              52E7E4F38BA3F2A36FAF6D9B8CA524C07864F68CCD6AE6E8CB756CAC283CA02B
              E19C9C4E27A15088582CB61FD8074BD8A87369F7636E929A9407BE1B110093D1
              24475EBA8F8D1585CB4D0D180008F0F6260F494DCAC317C21CDA5AC7E35E63E0
              C02040803D4F94F1C2867554390DFD2531F6B060341CDC05A799550F98730D9E
              EBED251C1E4DCF7775ED94F3E3EBE130BDBD7DE91DDDE77B94269F4F6678C512
              BD4B070C8747098542999F95F44362D15856ACA6BA5A1AE5D50DE8F1B8B31267
              CAA6DAA8CE48EC703A0CF3EA066CF7FB175E4ABF7599C7C3AEAE9D39E3FFC19B
              53776F932CD491A33DE9B1C7E3A6DDEF4F97E95220200281603AEEF73F2FCB3C
              9E4CAF58E0351E30D7A207989C98CC8AC7A2B1ACF8625EC3003317F65C13A4E5
              703AB2E23635FB8BB298D730C0C59AA2C9E75BB897659D7A97D2140BB5EA9B64
              D503A64B1C0804569223AD70389C359F07FC29180C120C066F77AC8C86E60796
              6834CAF4F4F4DE1584F957C5E37100FE0626A2761A39F1AEED0000000049454E
              44AE426082}
          end>
      end
      item
        Name = 'build-cancel-2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA8640000019E49444154384F637C6D
              6D399F818161EDB9CF9FB7C45CBFC930455599C14950D0072816BC5B5039B150
              D880E1F7DB270CEFB64C000A61029001BE407A0910477DFBFB772B1733B33790
              BD0C8863448F1EDF0CA4F102A6739FBF8014C500F132A0E666100DE2DFF9FE9D
              A0661060DEF1EE3D439284F82D5626265E4646C62AA058DFDFFFFFA7FB5DBECA
              F0EECF1F882A34C0C8CECDC0F0F73798CD345545990168B327909DF3FFFFFF6E
              10CDCCC8E8090A0B5C80555806CA021AE02424E801A45700713CD0E672100DE2
              1BF1F282C4B182BF5FDE415990405CF09F8F6F034770C88647B2720C0A6F5E33
              FC5CB33AF0EFEB377E62478E2642D5A1808D1B37323C7FFE1C4C33AC32336568
              8989D6FEFDEBD7B3BF7FFFFE0182673FAF5FD7FEB0761D2390BD1824860B9B9B
              9BFF65545353631014147C7EECD83151A80520F00A181E7B81811A09E5630556
              56568C4C50B61894860131429A61006600185455D730823090F99F18361033C0
              BCF017E805B0002900E40516281B0EF6EEDDC7B077DF3E46672727B081D8D8CE
              CE4E202E1850EC0294302007C05D00E593081818001688C55420DA1B6E000000
              0049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA864000002F449444154384FA5935F
              48537114C7CFEFEE8FA36D7A85166938B791BD148553B310B29290F6604982DA
              5B28F618955A59F447CCCAF5E243487B084C4A28B4B4822826B2122571D253A4
              A0457FA649DE3B41A679B75FE7F7BB9BA84D5AF481C3EF7BFEDCB3DF39F78ECC
              14ECBD0C008B00F4D6C5894FE0094C41933D134EA6A7E9004807E6EEF5A6385E
              9D333A500204FBEFC3D2CC67AEE3C11A9E27845CA7945EA0145A06E682509092
              AC23401E61FE105A896560B08F5727808076139B5D2200370881BAB5CD144A13
              6EC6D0CC290A1C1053DF0201051B36E3A54B319E8B768435ABF9380663A1102F
              4E04E12EEE6C2018040AD48D237FC0F17760FC01DEDAFBF8C70C3CFB39AB56AE
              4392756754A9084DB64CB6332D8ED989BE1D1BB5A3AEC6C6672A3659A0266DB3
              5AB90E86AD7951A522D46C49D3E2C3ACD961B4A36827F0B65731E6463BDD64B7
              B1BAF5A1E1A850D1D45B33DAF12C01412825C9C9AF87040D6418927C746909D7
              499AF1657D777FF9EA57CBFF245DBF08292623180C06EEB3CFA6F8975E0FBACA
              E3FD6245E51E8DD108916030B4D0F9F0DDE293EE5341DD86A7B6E73DB64824C2
              1F100401985E7BFA7C3E686B6B03E8C8CD81DAE262FD8B9E9EDE7038ACC44C99
              9F6F080D0DC2D4C8FB54F41B57E6E2D9F0F0B0929F9F1F16AECC4AF04696EF7C
              9B9E76F12B442106C3357DDEEEC68DBBB6BF44B7418DFE1DF66133D4FFD56A70
              7DD0802F26477513431B3D39922481DF3FCAB5D3994D254926939393DC2F2A3A
              4847FC7E224B3288A922E4389DD4EBED633F0A76BB9DD7306237E48416166002
              1B30635A92A5659F110804B8662723966375CB381C0EC0657A3D1E4FDC65276A
              B197B26AE418381A747575F371EA6ACF46D868FED151228A22ADAFABA52DEEDB
              449665E2CCCEA66565C7F83331E236DC969505D55555946993C9048585FB70A7
              4EAAD5AAE515E5E5A0280A359B4DDC5FC9AA1DC6309BCDB80A3B379D4E472C16
              0BD7566B064B1376329FC5D742D80E31E175B95C8568D1F0BF333E3E0EADADAD
              64B921C6F6ABA9FF01E0373FDA89B0088D4B5B0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA8640000038949444154484BB5544B
              4F5351109E730B161216D4A4581E625A45108C41440435461EBA104970E30241
              8C14E2E30F281836023161C1828D48792426C41A21260AC644588058E2031479
              044D0AA2BC8298A8207DDCF63A737A5B5EB6D0855F32F77C6766CECC9C39E75C
              367F2CED1C00E48024954CD96C62EED0304C58AC9017A6869A3DBB41602C15ED
              3750F28D21BB16CB03C29002D8E6C661F1ED13CE7D81123431C62E4A92D40220
              5DFA66B5399A6767A1343ADA1DBC831C9DC0D277F4BEFA40DC1F08204131067F
              803C0FF335452AB729CA5CC18FA0AE9DFB00643B98E07770823065B38A3816A2
              187127F90C9330064771FE8CEC2867B008D3D39FBF91FA0FA60D0A82B6FDF180
              950760F0FB98E43CEEC881B62594B3C87B1F7F5F80EB5FA6C16E25957F10C62D
              16689E99C3F603EDA4560EAE40792981D4679324B833F915441C3743E8A92B32
              5B8190A75643E9AE9D4859327EE85AFC4179813BC9C61D19B631A6684BD807DA
              2025AA7D23782F1DDB5A0835313A6A7432F69D7A4E95E760ADD49A4774BB50EA
              23954AE1617C1C5FE02FF0B2088780310A4E25D27BE8119D4E3BF20B98A415C7
              42B4D747060874E03EB134D429B315D03B68650059A054E62862E3BAC5A424E8
              C1879625DAC1DE670A94A6A75BC0E93C6B11024F44F574BF91D7FD13D5D5D57C
              5C5E5E86919111181D1DE509421601626D27D3DF455CBD961BACD11CE05E78D8
              8ECF9FEE2E1B0CBFC48F8329C331C9BDC76BABCAB96D135002A3D1C805EE1D4C
              84A2B454282928B82C8AA215C5BE4A5EDB6666B65B0607617262E6B0283AE6D7
              D9BD4A5D5D9D989292220A150B3F60C829E5BE1F1BBB8B07AA40C18E61EF5C48
              12D4EAE7810909A723A2C23AD0A292F53E21AFA781BF5402B5C5DB2126A2B453
              703CF435AF61F5749DC9834D6F860C5739080FD92202E8E34E4E55747675B1CE
              CE2E1E272B3353CAC848976E9696B9E3B2AACA0AE72A1FE6F6292DBBC58BA5D6
              5456DCF6C4E40948490AEA9F4EAB0396E98AA7D56ADD89F89C20FBB0D53E8835
              3E04D2F1311A7FCDE1E1E1E598A0DC64327125ED84AFF2C109DE6CC41B1B1B99
              C160D8D8224E1066B319EA0D0D44994EA7057D5191D3D0D02098CDE3DC5EACD7
              A3AFE4F601F461E8E359EF0EB5A14534A7442A95CAB3ED505528B725E12BA716
              1254A823ACF7214EEB65FAFF5BC44F1EE7347003612B9CE0CD26731AD6EE40AF
              D7732519DD857AE3045F7E030303D0DFDFEF4AA0D168E827B6A51F997F00F80B
              5E991FD08C99E7830000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA86400000338494441545847ED965D
              48145114C7CFCCEEB8E5E7CE6E242106666EDA2AAEF694D98B8259D4BA961545
              A414512F86519065F464411F2FDA8314AB41A465A464BB21448FB2045AEAFAB1
              8AF61041B9D5965FE3EA7ECC74EEECB8AD211536B812FEE0EECCB9E7CEFECF3D
              F7DC3B437DD9B1BD0904C13AE2763715F70F82D3EB855F5151143C484B853C56
              6D42B3746C6E6E7F46D75B21E0FD376841105E0045356C8E5C7BA4353D0DD633
              4AC9152044BC08CD47D8DA27FD7E515CB3B702E24B6FC3BA435789B924285B56
              26A444461EC37B330F7CD90837F3F8C0800333E193C4B7A0386B447F33B68A31
              8FE76E46E71BF2AC2C28ACAE6F50C0AAED1A86794F01D5A06594A3056A75FFCB
              F109A8D3A510F17D38EE09B67344DCD43700DF7D3EF16139A0C84F3CC3C0B374
              3DE0321C47F39EC0F3A51E3FDFAC6294449CCCFCBCD3E3A92B42F177B3B3E411
              D950901F8EE7C1EA726126D85ECCC4078AA2CC4A9A62B1366EA2FB02CEBC8ECC
              5C6E718298817936AA54F03ADB000C4DD76010E558A0F7B1FB64416F1F744F4F
              0706C90C2D5DC582BB959C44C4F7A0790AC59BF07A145BC98D4D4910AB109325
              3BE2BF4648D59ECFB244FC29D6C04510A012B3F011EDFA0DAA88E19D717183CF
              BFBA604E9065FB0751885B2D15B79A86DD8DEBD18233AF1CF7F9EE9C700C8121
              36A6076BE2138E33631043188443EE20148D5B53215FA32EC42DD82A087CE5A4
              D7537B7060086CD31C58510C0BB31B8318C3B124080709E2A1F373E06919A0F3
              D4EC2EACC51601844BB8BF6B4B0687A1676646749263D9D48FD5EF76D7A3598E
              AD313B26BA5874CA048D2B701A40A89AF0796B0EE3CC7BB880F83C2408B2FF47
              DD6E339A67B19D111D7261DF964D776619C0101D25F52C0E39AC5E656680C554
              16DC39FF05E241949090007ABD1E8C46232426268A8EC5105451D00549B1D585
              C99352D75FD1D6D626DD05703A9D60B158C06EB70702D0E974A0D56AB98E8E0E
              F9DE327F2037375739353515155C4F9EE7D7E08514C2B23449EFE7511C2E5603
              087B000BBF4043B85C7525F8AD70FD5A352FDD92FE60D04BEC5FF00D12F60C04
              CF019665FD369B4DDE97FD6FC8C9C9A1388E53ACDC1A0845867A58B0EEA1ACD6
              40D833B0B29640EC5946388E53FC00D75742D5905358F00000000049454E44AE
              426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA86400000451494441545847ED975F
              4C5B551CC77FF7DEF6027A2F6DA13047E61F0412E34C96D2F160A69850D8CB4C
              D8E6C312E3FBD46421B81835C19AF8AA2F3CF0409084C7F9A24C11352B7FDC83
              F165F8D239022BEDC4D0D2B29556606D697BFCFD4E6FEBBDB0553B6859C8BEC9
              37E7FECEEF9CDE0FE7DF3D089153AFBA81818531767938B8029F06FE84626AAD
              A986ABAF1C8746593E8DE16560ECAD4B8BB737AE44D6720DF65922C2590551E8
              477F79B1E9287CFEFC735A6AB7086E3C07D783E1B7E87AB4997224C579A66053
              FD31AD766F923E7CF6D8CF585A0441E8670C7FBB56B9A68802CCC6E2B9169AF2
              23774496BB31BC8ABE85EE69F8F5B7A8776B8B9A402AB85870F6BEB1FFA34AAA
              952470AA2A415A4551EC67004F9F54D46B16096026F6376F941F391DDC3CFAF4
              E2D6FDE86828444DCA2669663D060479525508D2260A62BF00EC29A7A27A5451
              82E554323F722ECC13DC02BA07E1EE9DF5DE84CD6C16C3F209C709474A8374AA
              CA4F18D609B991AC712A8AE79DC606B0CA7217D67F8736C085B7B7312CAF3820
              6907643D4D376459B55992180802C12DA209EE6EA5E048054012415A68BA6B95
              1F31B48B124202BC8DA635D75D693892A895058DADAE4222B7AEBEC1232883BB
              DB84CF33E8BBE36B6B1585231946307F94584CE63730FC1E4D6B6E0221DFC7D2
              7CCA62998EA73370636303C3CAA800A83B4A086E02D7DF127E5DBA0580AF313E
              42E72496A62E9B75A692901C90C31D47B82AB913C30974208B70A1CDCDC8F8BD
              289C509449AC7B4683942A0929E5A6F56584AB7A1DE31F00588065B3AE70723B
              726E7E01E81B6B3199F09C5409F2A8062922E46C2520A5EB8E1338AD7938B843
              2317D94E44CE79E7C1974CF246D3EBEBB8BB0952A13604F9019602417EB1FC17
              6F532E898D66F9352C695A112EEB0AA592E15EEF02DCD6E0F21A0804603818C4
              F31BDEC3B53982906E7CFE8C27CB281104F804CB6506AC7B35950A9FF7DE025F
              2291CBEED080FF0E0CAFAC6053F62E427E8555977299F209AF5BEC02BEAC2B9C
              4CAD9EBFF907C219476EA7386430849070312D881D5A75F974A1C1CEFD425595
              56F3FF74F6C597E0CD8FAE685189C24BC8A1119EC3787DA9AB83D6D656E8ECEC
              84DEDE5E686A6AE2C9626A6969D19E4A97CFE7D39E762B81EBDFE3F1C0E4E424
              6FC701ED763BBDF0E38E8E8E81C1C1C1CA7E6C1FA0B1B131D3C8C8C8F5582C76
              467F5990D3E9743596EA3E58D1F941F9A2CE643235B8718965F76D669F443393
              F79E542EC07DD313C0BDEAB107A4EBFC43B5B4E407BFDFAF4500EDED0E66B3D9
              B408606A6ABAB009AC362B38DBDBE932C1857D8512FB6A91514501E90553D3FF
              FE507373B3F1253B72FA97ECA5AF5E8FFD14F3BF42FB92B81D0E877B68688827
              0E52A3A3A3E45FE2F1B8EB70EDE21B7373B4B80B46D1A6E08E46A34C9FA30DA6
              CF3FA4EF7FAA24C0B9B9DFF9E2CE5BABE68A46D70D39FD0E2615EB5B4C4F36C9
              A3E8F06E928390618ADBDADADC7D7D7D3C7190A2EB3E9A4FB101101FCBFE8F78
              099A8DC7E3AE7F0019E3052D81E4242D0000000049454E44AE426082}
          end>
      end
      item
        Name = 'debug-start-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000015F49444154388D95D1BD4AC3
              5018C6F12739D112100C68112DA48208221D0423E850D0A2732747EB8D08DA2B
              F00EBC0217318E290EED26C58F745369441053B145C829C6E61C87DA34364D3F
              DE25D3FF97F7E510B2B60F2995052664F04F0B00C73843A45416A2AA5D1375C3
              22CBDB55DEFA1E0B2264290D51499CC5153947D96486A85A751CA80D4C278E8F
              0EB61057E4E47B9D1E5236B943546DA48D7C604F5BC4FCCC1436571730ABC849
              BB0BBD0C827C60773D09001004A117CA5116CB446D14023A1384FE4ECBF53B2D
              12E807F59C6689732B5571E83B05B1EE570020408A411A1631C671F75443A16C
              A1D6A0450079EE3A05AFA2A365EAD14030B41BCD22C0F3709D8267EA685574E0
              87024018608CE3FEB906A36CC1AE37FFFDD1332FFDB03352445802789EBB8EE1
              9957F02AE130049C9EDF04426A78A63E30EC021CE09C97EC3A3DE1AE63B457D5
              87863EC0BEDE808FC7347BBD85F7703172D8995F52ABF61A24DECCD800000000
              49454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000001F549444154388DA5D1CF4FD2
              6100C7F1F7F37CA9742B5468A072902F12856DE2A179779DAC0E9D3AB435CFBA
              F557885BFD09FD014DB453AB9BFE03321D6B8B2D98185F37B083832F4993185F
              9E0E5FF9A108017E4ECF7379EDF37C1EA1859E22BC61D4B98995FC0AE7263789
              A6CD3D477BB0F4567AC3452DFCACC0C83D54210BB5CA70A09C5944BA03EF40BC
              17529B959E50F226B0A6CD2C22EFEB2B737EB77E77F456C42C57578594B3D213
              FA3E0C6C836E7D6521E8F1BF5A7A843EE594C5B34AC42C57D75AF0728111675F
              70130C4C8FFB03D3E3B89CA33C7938893EE59485DF0DB8FF293AC0466CD88B7F
              724C16CB958859FEBB2AA42368C3CB05EE5CDFB82B082084C0DD6C3C268B6795
              791B6E34EE9CA227D81ED76538D2065FDAD8D173E16B12F44D10F44D70982B6A
              3BFBC69BECAFD26BE1B8FDD1F1F8C58674E9877250B015811020EC8BE4E23870
              C34CCE64E7C0207B52B2406D021B0295B68C38B5C476FF602667B27B60F0D386
              62401445DA3A8E534B6CD91FD34FC34CDE6477DFE0E8A4540736812890AE1B7B
              D412DB4DA891AE60266F373ACA9B751031601DA5D2D6711CABADD1D574802DA8
              5407ECA7A15296D11BEA00F3A7653E7CF97601A91888288AD4FF1A75800A504A
              91CC9ED6812D601D4859C6DE40D0D5869F8135E047BF4FEB16A1CDBF44F81650
              D53FD4139F86861AF907BFEE1CED30F1748A0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000002E1494441544889AD934D6B1B
              5714869F739526B29288422C59C6942A6E2D09140743BC2C91B113BA2B85FE86
              46DDF59FD40D24FD07DD14DC4DD73194765108719C4936A18D4ADD52591E7F69
              8C251979EEE9623E621B8D1A195F109C41C379CE79EE3B623E98C714AA2860FF
              FA0DDD7ACD459E54AA729FD4EC677366A2DC37338B5D3351410F5A70B87D3100
              333587C997EBC0F7222623D9C20B33B3D8937C19F55AD0D9B908406501F8D418
              A9A9EA5722644CB6E098F252D7E42BA8B7796E50CA4CCD919AA82C00B5AFBF98
              A773D4BFE2B6BB775579207055AE171C535AEC9A7C3954371A2865A6E6907C79
              01A87DFEC9C73A3B9DA35ACC49A7D74FBBFBDDBB0A0F442423D70BEBA6B4D433
              F9D248A0531BDC9F2F8A88C8B5B1F7F4F64779B975739C4EAF9F76DBDD0545EA
              228C4976D231A550DDC1E6FF82823B780B88FF084097999D1ED76A3106D554A9
              0B64243BE9989948DD5662EA4E29BA77E7431DF45200CA71EB664E3A47B1BABA
              0861EA967A922F0DDC68A0A26883B3F5D5F4097547FD7418867A90BA49C7CCDC
              0BBE23AF19A76EA8A2A4FADAD8656E4FE7A8166FD0393A8ED47D29422648DD52
              D74C9451AFF96E8A924EA4AE5A1CE7B0779CDEDEEFD6E2D4650BEB8CBDDF33E1
              54F174C3149DACCF3C13B700E1ED8F4BA34C7CF6B4F63AB2BAB6C1CB3F5DB196
              7D411F010F51DD51AF89FD7DF57C80D6EE214F82C6A8550F78047C0BEC586F93
              E3F51FB06F7E05F503806AA05EA322A18E276EB858C523987819D55DF536E9BF
              58C1BEF919D4C6C35C82E00E54354E8BAAEAC9BAB5D791D5E71BBC6AB8F89636
              F0389A58DB4D7C6705FF8FD38D4F011255EC1DB2BAB6214EC3156B89542C83EE
              A8D70A55FC02EA27F618A8682B9CD869B8048DF531F08D2ABBEA35F107A8180A
              8814458D5F365C7CAB1E48A462DB862A6C828AA100C2EB7CB8F24C4E345E06DD
              7E571543011A100E7C1BA908723C8A8AE11BA03F297C27886BDBFFE23B3F8EAC
              221160F7FE868DA76B00F69FE7D8D74FCEA522E9FC073A4EC07838C88C540000
              000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000381494441545885EDCFDF6F53
              651CC7F1CF734EEDBA8EEDAC3D838E76A1B06EB6A90E319060844591300217C6
              1BFF00034A9625DE7AE93D2A228AC6E81FE044E33F60B9232644DBB55841B1DD
              DA95B6D0D3F6B4E7B4A73F9FAF175032A276607BBCD177722E9EE439F9BC1EE0
              BF9EB8F520780E80ED5E0035AA40BBFEEF032C87DF8065E1D5B7C5E0A918EC72
              874A29D3218F0044DF228449CF0F60C21961CAC7C5E0E918B3CB6D5EDA300DF2
              6780E479F7F597FD6335A3BD54A9B7CF0A533E66099E8E32BBB3CDD52CD0D2CD
              07BC76740E47163CF04E4B6365AD7142ADB5DE14649F280496A24CF2B448CD00
              4DCD3CC0E2FE19D8AC16C813A338E47761DF6EC9AEEACDE36AADF596E0F45AC4
              8790CD81217F0B18B15A00008C3138274671C83F8D59F7A45DD59BAF94F5D639
              C1E9B50A81935126799A8340B6056CCD396EC341FF34E63C8E51556F1E2BEBCD
              7382D36B1702276370EC35A89A030CD53C402FC7B80D079F76617EC661D3EAAD
              974A5A6359989CD921064E4421CF1AA86601A36C1EA0D7E40E1B9E9F77C1BF47
              1ED18CD662B1DA581624B724FA97626CCA57A7CAF6908100BDA4B1111C98DB85
              C01EA75537DA4794AAB1CC24B7430C2C4599ECABF5830C05D06BE20124E895AD
              B546FBC542C55861925B16034B3141F6E954CD03F59279805EE3762B9EF3EDC2
              337BE5A76A8DCE0B05D55861927BA7307FFC065C418D7405A815CC036C85ECF7
              EDC4B3FBA62C46B373B8503156D8B8CB25CE1F8BF1665D83F21B8481571E23C6
              EE7F6060B8FF688159460000833FB34FF9520D57C369DC48169A9CE84B229C27
              3593E946AF8027AF9907C8157584C269FCBCAE3488E80BE2FC3CAFDCC976D7BE
              01AD5F03400FEF0E1590557484C229C4378A06117D0EE03DAE24F29DE8B7A0D4
              F54786870AC8143484C269DC4C15EB44F41980F7B992B8D78DAC826FFED4F7DF
              81009BF73484C229DC4A97F407C31F702551E8445641DB0C0F0448DFAD22144E
              E1D7744923E032112E702551E46BDBBF7820C046BE8250388DDB9BE52A813E26
              A28BBCF07BA9BB76059479B2E12702ACE72A088553B89D512B005D027091E7E2
              6A27B20ACAC7FFD1F06301925915DF875348DC51CB003E22C225CAC72BC318EE
              0B4864555CBF994732A796007C08D0273C17AF76225F0F6DB82FE0ABABB71410
              5D20E0723717D7F9DAF087FF12400410D13B207CDACD446A9DD877C0DD5F4C19
              EEC5B61E84D9A360921B3CFD23A8983475F8FF7AFD016953CD2B1C3677C50000
              000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000004A1494441545885EDD8DB4F14
              571CC0F1EF394B2BC8020BBBDCD6B502A9B80BB4FAA262DBD4B469144CDB87FE
              0135B54DBCBCF52F68026DFF88BEF609426D6CD2C63682B135366A645101AB16
              965D407697BD827B0176E6D707AAA15E229765491ABF8F2799994FCEE49C3919
              78D9FF3CF5E440D1E12F512536CCD804C6ED1F2193D80AD7E32C4F0E141D388E
              76347EA76B9A872D9ECE04C5E5486C0272D92DE03D036869FB08BDCD7A0E38A3
              B4A551D7348F583CC7E214976D09F4E9196CFD10B5ADF4AB9606BB5ECAC9BEEC
              A2714669DDA46B9A478B3C9D31555C8E5940E87381EFEDDBC927EF365351FAAA
              0EC5D37B179672A7D196DDBAA679D4E2E9884A690D929884C5D4D6005B1BECB8
              AACB705597D1DEE2A4C25AAC43F1D49B9985DC69A52C6EED68BAA3DD1D11CA6A
              91F8242C3E2C3CD0E9B002A0B5C2555DC6A156279565252A144FB765168D534A
              EB165DD5F097C573745659EB30E381BC4357057C94568A1D0E2BEDADF5549517
              AB703CDD9A5D304EA1749BB637DEB5783A42AAAC16C923744DC09550A7C3CAA1
              1627F68A12154EA45B32D9DC4994DAABED8DF7B4A72348791D920C41365978E0
              A39452D4DBADB4B73871D84A54389176A7177227516ABFB2ED1AD3EE23D3AAB2
              01493E80ECFA36FC0D019F05ADAD2C2592CCEC4E65735F28A5DA95CD356E711F
              9952F6062431BD66685E802BA1B555A51CF4D4536FB71249665E4F65964EA078
              5B57B87CDA7D6452D91B90E4F4AA3FA17905AE84D6546EE7A0A79E1D0E2BD1B9
              6CD37C66E904F08EB6B9FC16F751BFAA5A1D7453802BA1D5B6ED1C70D7E1AA2E
              233A97699C4F2F7D061CD6365740BB8F4EE817403715F82474BFBB8E9D35E5C4
              E6B30D73E9C5E3C0FBCAE69AB4EC39EA53F626CC74145291C20357421D1525EC
              DF53C7AEDA72E2F30BAF25538B9F021FA80AE7B465F77B63D4B6210FC3909A05
              40E755B00668F3CE2A4E7DBC97CF8FBD218DF5156F69ADCF2BA5AF589C6D9DAF
              74764169F5D6015742B55668F5F8DC5CA414452BCFD1455B011311C61E24E8BF
              11C0174C2A11B90A7423F28B3917C4B8D907E968E18122C2FDE904FD37FCF843
              734A44FE14912EE057732E8879B30F73EC7710F3F13505018A08F7A6E2F4DFF0
              1308CF2B11B90C7489695E30933318B77E40C6FFF80FAC204011E16E20C685C1
              0053B3F34A442E01DDC08019F5911BEA4302D79E09DB54A08870C71FA37FD0CF
              74E4A112918B4017C22533E6C3F0F66206AEADEA5E79058A08A31351FA07FD3C
              88A410A41FE812E1B2AC119657A08830EC8B30301860269642447E13A11BB822
              311F396FEFF2AB5C471B029A220C8F47E81FF4138AA71191F3FFAECAAB667402
              63A807095CDFC823D607344DE1D6F82C038301C2893422F233D02D22D7CDC818
              C6501F32B931D8BA80A629DC1C0B33E00D104E6440E42750DDC0A0393342CEDB
              830447F2025B13D03085A1BFC35CF40688243222C839A01B61C80C0E6F0A6C55
              40C334F1DE5F8645931911380B748B70DB0C0E63787B370DF642E0F23E16203A
              9731813E11BE061991E0C8F2AADC64D87381C2F259E2B62F6282F488C837C01D
              636604C3DB0BA1C2C09E0B444044BE07BE1591BBC6D410C6ADB3101A2D28EC51
              4FCF603A0AC871333681E1ED59FEE5F6B2976D5EFF00ECC66E7AF964DF280000
              000049454E44AE426082}
          end>
      end
      item
        Name = 'debug-break-all-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000007F49444154388D6360A01030
              C218CC06A10C4CA22A2892FF3FBD60F8FFF30B56F13F27E73330303030B0C004
              9944551898E54CFF212BFCF7E62ED3FF6FEFB08AC3F5A13BA923CDEE7F479ADD
              7F62C5310C20158C1A306A000303525286818A598718B129C4250E37E0FFA717
              28699C818181E1FF87270CFF7F7CC22A4E3500004F6534CA97F4020E00000000
              49454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000011149444154388DED94BD4A03
              4114464F36519760946063AAFC145A5858A4B25110DB3C43ACECEC6D447C8D54
              D9670876418CFD22828D084B16C40DB198848C9A0593B1485C749C4020967BAA
              61BE3B67E61673E19F4944AB944DB2B8672C52EF02F539C45ACB19F3D1F31D7C
              F4269AC86C6758DA3FCD03C75A7D7B1C3C386AD025B97558050A5A5E5757E7FE
              58174E290017A5DCBA0210832142862DC099E6D5ECEACA41366303E005FD0470
              03F851A3A6164E2ABB00345D9FA6EBFFCACADB9B1C95F3009CD56EFF9CB54CC2
              458885B13016CE83F12FD71AF7C06438E8B88F1DBC97DEDCC23670E9057D7DEF
              1B47C8B02564382BFF316097D3A4762AC65B95ECA2C237AC8DA2311F3D5DA3E4
              EBCC572FC417A295550677ECC58C0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000118494441544889ED953F4EC3
              3014C6BF8F7681A543777201A6D21BB4ACBD007729EAC4A1126E907086A64888
              15217568957C0CAEC176EC6629EA409F64E9D9EFE7F7E773A4007F6CF4765743
              B352A6D6AC3EA6D9FD6C3D72387DC4E06E91BCDBBE95D0E77B2FB3CF9F7F7B0E
              01924F001A922DC9D6F1972E13C4ACBF0CF325679DDF67329E50549B28F330CD
              0050009097EB28D3292099BCB3C9ADEC595ED61D86A46693CC61D6B4778F1620
              6913F0904CD67719980F442429FFDCB3CE1B9CDA9212C99957C1EC56A2207691
              C867FE8F44973738FF1B9C4FA2E275C34336A4242AAA1A7042318992FF8397AA
              EE741B988AAA8E263D5A40D20AC0CA1D371CDD32612C26915FFE660C5E8FD2ED
              ECB650B3EF67BE3ED2F153DB37E87DE8C33E42D9440000000049454E44AE4260
              82}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000008A4944415458856360180503
              0C18B109321B84323089AAE0D4F4FFD30B863F27E753AC8781818181059B6226
              5115066639D37FB80CFBF7E62E1335F4E074000C74A4D9FD4717AB9875086BA8
              91AB07ABABE809461D30EA8051078C3A60D401A30E1875C0A803461D30EA00BC
              AD62422D606AE8C1EA80FF9F5EE06CC733303030FCFFF0842A7A46C1A000004E
              7831806B0C343A0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C0864880000010B494441545885EDD8BF0A82
              5014C7F1DFB5B47F084110D414B537064DD16ECFD0D66B84436B0FD06BE41E4D
              ED824B50630D11B48820862D0515DEAE75A008CE075CBC9C7BBFA08B028CFD96
              902D688D2EF4CE30D526E1628AF8B07EBC59AC20674D52CD479E83B3E724AE65
              654342CF4398D53180B162FFBEC8E8CBF8795ED320CC6A0FC042316F0BA364CB
              16A58137B54A291E745B896B33C7953E817B23ABFDDC0F0098AF36D81FFD97B3
              CAC0829145B35E4ED321259B2F18CAE3A1914EFE020EA4E2402A0EA4E2402A0E
              A4E2402A0EA4E2402A0EA4E2402A0EA4E2402AE5A77D1046D8EE4EA44364F341
              1829679581FBA32F668EFB7ED59DB4FF7092A802EDEBF5A925FEE03562EC950B
              28AD319027FAC2200000000049454E44AE426082}
          end>
      end
      item
        Name = 'install'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000019149444154388DDD92B14B5B
              5118C5CF35AFC1BCC4679E97A7E2122825832274BB8B66788805057570B35051
              D0557088FE035954500A058762A76E2DE828E85415070775101D0C1849A10635
              C9EBD334DFBDE950A3A6F1A1E0E66FBADC73F871868FC183A60FB3D05B3B55E1
              F4A026FD71D8AB861ACFE489BC008176F7F2C3EC1E07F3EBC8FDF85A51F2D571
              18F6085052B85C5D44E9DAA91684DEBE437D6CA80FC0EB406BC77CC9CDFE9BA8
              1B681AFB04BF15990070AC9CF395ECFA52B540FD71016007C0CC2BC3F2C1B0E6
              0040335B006012C018005B15DCCA75D168149C731894835EB8C8AB3AEBBBAA35
              16000419635D00AE008CFA9C333BB4BF9C0E2437C1790338E7E09C43334D53DD
              EA7EEEFE6E6FAC1DC844DEDB47976AEDE677F84D58B35BD21B6D8727DB877AB8
              3E58314108A1E43D8AC5622E99C9C7FABFEC3547125BDF7A3EEF361FFDCAF712
              9123FF4308A1AA04524A4944D9D4B9DBB9B8914232E30C12D155398B4F4DABF8
              D4B4925292A7A02C21A20411151ECACB0B3478C0180B01887BE5659E7D894C08
              A11EAF79F317F2EDEDFEF8A49D9B0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000022C49444154388DED934B4854
              511CC6BFCF191D1F73CD71E696D3C399910C221015E52CA47C81D18041CB36E1
              C65C1604D53634A5BD18A4B6A81645109539D44697165786422AE24A16456338
              668AA3E9DC3BA7458E7AC79A2186767DABF3F8CEEFFCBFF320D2C8EEF2E2C0E5
              878D00C601E4CC5C12E9EC00809C8C8EBFD47F60F6B2A70E3802350080F8D7F7
              BF5D602B566177EF878CAF63FDF3EBF4C092B62EEC6AEE280450149FFB3017BD
              DF6335ABE528EB1C804D71FB007C9C7FD08765EDB1C56389ECF055816437C9F1
              5CD55FB6FBCC3548290100524AEC393B005BB1BB81E42B92ED0E5FD58E0A2D40
              73791E00AE00F84E72CCEEF4782D152A9EA30443006E49C8910DBF45ACA8A8D8
              EAE41520BFFE14567D424938D551002A806E9277A494CD00460079D3B6F8E57C
              C1CC0456261F0109D30A14428CA56CB2E0F41FE95AA83EFDE31B955100B5248B
              A494311237F625E62FE43DBF5D1F9B9DE94B4D08FCBA94A6603028BDDEAD741E
              8FA7D27DA8B2A9F7C562F0DDDCEA13008D04AE1FF3175F3C575DD630B5F7C4E8
              DADA9AB21D148944100A8508214442D334335586614C4E45964A8F0FBD2C0CF4
              4E7476DC7B834F0B2BAD86612C253DBA3E6DEAFAB4198D464D4DD34C214462C7
              3BDC3C0BB2E6B05AF8ACFFE4C1B6B7B3B1C1FA72A55D75E6DD25999FF40C0D0F
              13005A5B5AA4CB55B219F98F22591B28CD7FEA77390649F693CCDD3EDF7BB547
              26DBE170383370035A47B22E932FA97FF397755DCF1A9464500891C89AB64D3F
              01EA1CC9F28598C0460000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000002CD494441544889ED944B4894
              5114C77F6766BE71464DCDD728E64C9182500A117D4A44D0F4D8886944D80312
              66D5222A21A3368A3D4082B2072D9C4D4F6AD1225AD8C6B4458846A0122D22A4
              08324A6A7A0D3D66E69BDB62FA6C667CCC582E3B9B7338F79CF33FFFFFE55E21
              0DCBAF3F40EEFA3D1D4AA97611B14E5C682134F12C9D562C6955FD83A50DA094
              4AF00B0E2022097EC101FED6FE4B94D26CC989457A13391B7DA02034F992F737
              8E01D325B2BB5752B8F33862B5A1423F98BCDC4A2430310D208181252B8FFC6D
              47D0728B37DBF28AF76556EA14FBBA918CCC0489323CD5B87CE7D01697BAB4DC
              E2B3F622B7A3A0A92D3503B1580101D82BB04B89680E4FCD453CD5090CF21B0E
              01169788F4011EE01A566D2C254034F41D2318C09A9DEF13118740372216A538
              9F44BCE4F77037D0A0941A9B491E0071BBDD0909ADC8837D75233F4B576A5167
              DE7560077018C801DA8172A00FA10CC3A8B77D7933687F3D4670F0161891E900
              B5B5B51D26751189492196B7464995FF6BCD765B28ABE80A22CD28350E5402CF
              0197856843F6BBA783CE2777AB08BE6F36FB9367D9800E00115131278052CB1D
              DF6BD6D638F6DF0DD85B5E7E0C2911D9AD6217E1D22C52BF6559F6D012A7B1AA
              F7F1E77B617099FDF1B30085AEEB86DFEF372233D8D760F0CCC31701F1FAC7AC
              9EAEA1AB4BBB863F549C1ED6DB7AC7791D08AE0987C39333F5452291484F4F8F
              A1EBBA613357E6CFFA28A5948848A6D379B0AEDC11EDDCE46EEBEC7BE59BF814
              5AB67545C178EBBAB2F58559DA1D20D7ECB9DFDF8FC408E0F56E50A65CD31E5A
              F21D89D05A579E635C6AAC38FAF15B78BCA2D0B9252F53BB2D22D92AEE631A18
              78802991D7BB612A6F8B2F9A2D16E1F0F242A752CAF148446E0276F3DCF4A74E
              9EC054C2BC688039254A8ADB80A88858E39798A30798DF6727F3AC8F314847A2
              D9E21475310011919191917886530F65AE3855DDE8E8688CB6AEEBC67C69CFC7
              7E0178B47F0A1D19B6C70000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000002B9494441545885ED975D4853
              6118C7FFCFBB1DCF769C1FCB2F9044A6A16E063288B0A17DF895D9A62C49F242
              C8A48BAE828808F32E15EA260D82902248D41034D30A2FEA4EC4AB6242A382BE
              D82893C00A8F3B676B3B5D64528667EECCF066FFDBF33CCFFFF7BEBCE73DFF43
              D0A8B4EA53E0F36C087DF163E9619FD631D06B6DE477DA20582B24D9FFD2A0D9
              1D00D3DA4800089444F1B8C703B0554A00240012000900D5AB38C37D1182B542
              90FDDE95C5E12EE04730FA4462C868EE84505C2EC83EEFCAE2DD0BAAE5AA3B60
              B4561875A9991EC1B6BF2AE7641F884F5637D771C86AED45CA1E67952E35D323
              94561AA3F1AA0204FDDE008076004386027B4D76473FC8B001849E4366DB1518
              771FA801300405EDB2CF1B880B6071B80B81577333009A89D1A021AFB42EA7E3
              FABF107A0ED96D57915CB2AF8E311A04D02CFBBC330BB7CE46F307E5E6E6C264
              326D5CA04F02677741B41E2E57F486FB00DA25DF8B698443305AEC91E0C21B16
              FAFA194271793D4077281C720BAF9FCE859F4F22222DAB9A4B92042A2A2A82D9
              6C8EA815F23C3FB5F7F89996E95061D97230F200400780C74414511485016800
              709BD751D311D347CFB391BE5159965CD1562F8A225B03989D9D55D48A83A1D0
              D4CCFBEF2DE7A73F947D93C293004E13D1A4A2288D00068C1C355DAECEF3384B
              3246394E1FD5DCE17090288A6CD3F74012C7B90E169AC7AE3558E653799D13C0
              C0EAA301A39E5C3DB5F9F38DB6ACB1CD98FFA998221963ACA1D29236DEEFB41C
              3BF7E8DDD125297C2F996327BA6BF3BD4EEB8E71C658FDFA9ECE4B5D7F85A6DE
              9EEE087E05AAD8015621EA2B2DE91337DDBBDC37E63ED9DAECD9DCA182F40922
              AA8B7516006CFA0CAC97A2284F14456925A21122AA89D5F8F719D09C8A574DDF
              12518AD619409C1FA378CDE306D80A2500B61D60ED2D70381CF1FE6569039024
              09A2286ECB4E040251E3C2FFD74FE40CD1EADE2FCA890000000049454E44AE42
              6082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000003D6494441545885ED975F6C53
              551CC7BFBF736FFFDD76ED5A77BBB52CD03F0BCDE604DACC4008DAB131D414E7
              8B801331B225F8FFDFCC24223C2CD499C5840E713C9A108C994882B12B1B09CE
              40464C3476C47FF14D468C6DC06DDA65CCC9DAE383EB70BAAD77EB35E3A19F97
              FBEFFCBEE793736FCEB987A0024CB2C0167A69F6FAD78F8FA811FB77B62A213A
              09A69AD03A534DA8DF54135223F276B68A5956026D57310F808A82049A73540B
              3547F07FA120982F05C17C2908E64B41305F0A82F922E66CC10494EC3C0CBD6B
              3D327F4EE2C6076FE0D68DE12577A473AD47C9CEC3202660E2DB018C9D3BAEA8
              2EE7088A163B4CFE074DA2D5715E5BEAB9B76CFF09689DBE25C9E92B36A2B439
              02CD5DE5FB459BA3DB127C5C71ADD2573C01601884F382D1B6D1DEDC0DED9A7B
              94C9DD5D0B79EF3B601AE91910BD078E2FC115FBE5169C1E4B2035D8C3013C0D
              8ED360D4CF8CC64DF67D5DD0AD59B768ADA1BA16F6A63048A3791E0CEF82F316
              002747A35DEA0902C0686F177E1FECE11C7816C0878CA85FD04B9BEDCD116817
              903454D7426E0A8344F145C658041C4F013835128D2075F923C582A22CCB282A
              2ACADDF2874F61D0DDE27FF81A5E489BCB3204EA633A2954D6DC3538F249E79C
              A652F556C84D4700417C85883AC1F993E2E8D51EEDF77D6089AF207B3C8AE426
              2727411E8F07B22C1F05B061B1C69C7310D1359BABF2B991C01337AF73D33100
              FB00ECE099E94BC4C420117DCE39673C3D0D12C45610DE06C7DE559991D3862F
              DEF78C5FFFB91B804E911D7025954AB566A7990D7EBF3F1808047256399DCED5
              0E9F2FD47E79ECE56F9237D30062C4C48781DB9F3E09621B803038F6DCEF329F
              69AB7178BF733F7A717C7CDCA1C42C1E8F63686808C03FE6C140208096961625
              F5C1743A1D3D566C697CAD6FB835FECB441A4014C0D199E70700B403786C9BD7
              7C36DCB0BAA2C4A4FFACCABD4B915C96ACE0B256124110B696171B7A2321B731
              E034B671CE4F10D1A199C7ED9CF3DDDBBC96B31DDB5D6B4B4CFA01222A5F4E3F
              8092956401186341A759178B84DC3B5A7B7F3AF07562220DC2ABC4B1ABA1A238
              DAF180CB6795341788C8395FFDC1370FFD67FBD7F1563803CCDD16E6B51633C6
              EE5B65D1C78E3FE22DDAE2321F6444FEC62A5BB4F32177A555D20C2C24B71496
              3D825988688BDDA43DD7DDE80D5DFB6DEA47B74D5FA517D905222A5DACAEBEAE
              4ED17A92B7E08CE466A356E8ABB44BAF03384344F65C35F5F575F346FDFB862A
              820040449B005C522B2FCB1DFF3F5810CC978260BEDCF182B3D34C2C16433C1E
              5F4997591289C4EC7956F0643299BC984C2657C6687EAE020059AD564892B4C2
              2EF3333535B5D20AB9F90B8C7DFF181E5BB7B00000000049454E44AE426082}
          end>
      end
      item
        Name = 'uninstall'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000019749444154388DA5924F4B1B
              5114C57F6F66164D86B14E561A629C243520D988D03C423E4436D915A4D055B7
              16A41A30B6821F40A4BB0A225976518A3B839FC0A5B810FC5310AD69A3327591
              64DEC48D4A1C47257856EF9D7BDFB987731F84E0637C9846B1E0348A85AD46B1
              E07C7146C3DA00D083C4743CCE7CCA49037521C404507A3B60FD8AE9FAF9D6C5
              25DD40BFD67BA98E26F89C4C64803AB074432F01F50FC34399E54CEAFE835E07
              A5588CC5746A4C68DA26B0087C1742548112E00A215673A6B971D1E934B7FF5F
              DD0918B707CBD0019240F5B4DD5E6B7A1E39D3E477AB45DBF757DF44220A480E
              E8C65EAF833B819FFF9A8C47A3F557BAC6B7E313BEA61D72A6C9B9E7F16E6797
              4F23893580F5B33342055CA5A81C1E053305E04FA7C3CCFE41682D9849DF30B2
              D9EC7D454DC3B22C5E4723A03C4CDB269FCFE3BA2E4AA98702B66DFB41B256AB
              2D8C74BBB8EFA7BA93CB2BF33F06072997CB0BA116A494BE7A047F8B05A594F2
              1EAB4B29FDBE33989DAB303B57016E3EE5530EBC56CB7BCE81F1D4346118E239
              472F5EA390523ED8423FB806B64DDAC05EBD899A0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000020F49444154388DAD943D6853
              5114C77FE7F9621293169F3631814AB5444A9A4220D53E71110C5A78437410EA
              A27340829B3807B43AE9E6A04E0E22A21491584BD28214A91992A95408269B59
              4C6A533B4869AE431B3F9AAFA2F94F97FB3FE777EFB9DC738436F2DA6CCC8C85
              3871C0390EDC05A63ED66AD5A9E51536EAF57669685D6027813911890269B3BF
              FFF0F3501097D632AD3530E0B0930E8D12703A268039A0B8636D01990977DFC0
              EC6810AFAE7707061C0E5E8D05F1391DA745E41DF002B8B9639F077E88C8FC48
              9FDBFB3214C463B375064E1F3F866FBFE38C68DA2CF04CA1E240E3C1BE011710
              3644D332232ED7915B83839D81FB048030F014D4F56C6D5DFDE92FD56A6BA026
              812FC0902E5D4A7E53A922F010A512AFBF5654A2F01991ED2C11E1CAF20A4B6B
              EB35949ADC522AFB7675B509D8E28CDF3A6AB7933F357E165800B481C50F9DC2
              9B6FD80BF51CA80F0F0FB737AB15500A444029DC6E375EAFB733D0E3F1CCEFDE
              141162B1D8BD443CFEA9FEE0BE6F339356F6ABD7864A339749269397B2D9ECC5
              7640314DB36E5996F2FBFD7F199665E1F7FBD9CCE7F97E2381F17E510192CBE5
              C8E7F34DA072B94C2A9512BD911C89443A96D2502412F9155B2C6E77A56118AA
              542A492A95A27543EE518F1E3F01201A3DC721C300E80ED4C3617530B3D0D29B
              BE73BBB1945C2EB737A0689AD0615CED56EFFF2140A150F86F508321A669B69F
              E7FFA09F102C9A28B7781A490000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000002D8494441544889BD94BD4F93
              5114C67FE7B6252082C644D20A62D5442321212E6F40349ABA49357E30F93DC8
              E0E0A083C6303008E81FE0026E9A542202434D5C84E86034C680898A51E3570C
              837C24D880054A7B1CFAB634B4F4ADC6F82CEFB9F73EE73EF73CF79E5770C039
              9F97EB5B36031C005A80E3130B0BD1236F46791F8D3AA563F22DB6F8BC746CF6
              0304057A8143C0C0FAA2A25503B5356C2B29711470ADB47065838F56BF1F63E4
              20700FF0D807F203F5A5C6F405D7AD8BBD8A44F81E8B155E8100ED9BAAB854BD
              11410FDB9B0F03376DCA5981BD88842B3C9ED250CD76769797152620C035FF26
              5A2A2B1191A362CC5DE02549FF7FDAB490C219A0518C844BDC9ED577766C674F
              79B9B34085C7438BD78B20C7C49810F002680222993C55ED014E21D2688C7950
              EA72975DACDC509845762501E019D0B4A81A199DFD8588A4AB7C3D3B0BAABDC0
              71602BB031E7EECB0566E271C616E6012EA0DA144B2466CE7FF8C8E39FD319EA
              C2E5CF5F18989C4255FB50DD018CAEF464DD9983D94482236FDFB177ED9A04F0
              EB53748EA791083BCB56A3AA297B584C28E73F7C64687A9A229199854482DE89
              496701806FF3F3DCFE319E45141154356D551CE8199F58C99934F236DABF40C1
              029916FD09DCD5D5D57909D1681434F97A94E4578CC1292F2DE0F3F9DA52274B
              F9ABAA1863A8AAAA7ADCDCDCFCA41E454221D5A9495C7575DCEEBCC1C3E7CF09
              87C36D31FB37B13C3F75676EA00D4044520744445455A5A1A1E1743018FC5A2C
              42746CCC3FD7775F4BAFB60ED678BD54D6D6D2DFDFBF2F332747AC589615EFEE
              EE8E2F3A60E656F7E2C4AE7A275A1A5D5D5D71CBB2E26E49D6A5B6B2D8256A56
              6C0FEDE92CDEA347838A24E7F70702295E761FE4859D940B834343E9787F20B0
              74C99AF1EEF2C5C5A7CF507CE264EA9966F13A3BDA97CF27050AB6C8E5428D49
              CFAFC85B8A81FFD0C9055B942B76E025054444868787332BCCD934CB6327DEC8
              C80800625956FC6FCB2F04BF01F6EFB00456BAA9BD0000000049454E44AE4260
              82}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000002A5494441545885ED963F4C13
              6118C69FEF2B69697AB59C912257DA52286570C3C8D0485C5C5CA443717130BA
              41A20B1328D020D48415273584907464318638502221C1DDB6C4A15D88696A62
              6CB1B9DE9F5EEF1C308DD548EFAED5268667BCBCEFFBFCF2DCFB7D771698D483
              CB7D98F57A11E8B6E1FDB7B2D931B098699AE6FA111F0AF88376FBF6F51ED73E
              63B19CBC2B9DFC1B8069AE1FCB81C141007B8490310093D72E385F33164BC90C
              043552FCE8D47C08C01E80B51F8FD700ECCD78B8C053BFCFD840184860C93780
              D9818161426912C033002F08214B006E03A80078759561DEF86CD6E2DB62099A
              CEB95D7A8A627E2FA63D5C9012BA0B60B920CB1B5F1505571C0E1C4B1264557D
              19B4DB6B84D2E4549FFB262134FB309B83AA6376D3C462833ECC783C214A6812
              40AC20CB1B9154069F24090050541444521964056103408C129A9C72F786D647
              8675BD8E336B16FD3ECC70DC28216417C04241923723A90C72A2D850F7B95A3D
              85A8089B00160821BB77DCEED1E723C1D600A6DCBDDD84901D00730549DA8AA4
              7F376F80486790AD54B6A0610EC04ED47DA9BB25809C20889AA64E1444313199
              3EFAA37923C411B2954A4253B5899CD0A4014D96F0EED1470CDB6CF97CB58A2F
              8AD26C561DE2562A0D9FD59A3F96E5A6F56702F0AA8A0F82A0CBF867956A3594
              74F619BD37DAAE73808E0374711C0786617437504AE1743AE172D801458183BD
              88F1F17194CB65D46A3543E6A228A28B6118B02CABE7DAAE2B9148C4BC9A86F2
              FD7B185B5F5FDC76F5201A8DC60CB903E0799E925028049665D5C3C343BD1FB0
              BA8A372608BB7FA00220467BC3E130E1799E767E07FEB6C1FCE3270DE9C45757
              1A12FBFF1388AFAEFCBA5B0D89743C819600D8FD030D264E40DB00DAA173808E
              03D48F61381C6E69994C0388A2089EE73B928460E277AFEDFA0E2515F10611D7
              329C0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C0864880000037A494441545885EDD64F4C9B
              751CC7F1F7F76981F884875278DAB1C364C68D6E4B48D63A43969860A86E8A6B
              8B8947C741D4EDE2416FD39D883A7751E369C1648A31BA9D3C2817952E8BC603
              8732B3D24060C245E94A44C65C1FFA14F979584BC095D07F5A12F99C9E3FBFDF
              F7F77A9E5F9EE7F713AA100770AEFD61BC7575CC58161FFDFA5B35CA02E0ACB4
              8003F8D8D741C86C750367405DF4D6D7ABF3B37315E3F2F5AB816B01BE1591D3
              C0BE638DC6372EA7534597966A07DC806B05BE031E11918780FD08BE6386F175
              35905A399D7411863A0E12325B4DE07B40012FE76E3F0D8481CB67F6B66917F6
              B757344D2503754DE3CA211FA75ADC9E1C2E9B432DE69AC480A780E710197EA9
              6D8FE3D28147CB469604D4358D2F8FF8E86A767935876314C8002752B6FDC7C6
              7629DBBE91439ED434EDB3B0D7745CEA385016B268A0AE695C3D7C88E34DAE3D
              9AA645817BC089E9B4B5D43F39858800202244E213DCB6ED9F812022414DB4CF
              23A6E91C2A03591450D734AE1E394C97ABA94D44A2C01D5027A7D3E93B7DF109
              52767653FB196B85BEFBC89B4010785244BE0899A673C877B024E4B6C00691FB
              B82663AF885C03169552CF4CA7ADE5483C412A9B2DD86F0372422982C0132272
              25D46AD60DF93AAA07F4D6D7D3E56A3272B89452EAD919CBBA1B892758D802B7
              09797382949D4928A57A80E3220C87CC96EA01579502B88B52EFA154EF74DAFA
              B318DC3A726585BE7882DBB63DA994EA41A94F5655D1BEED97BA79DBE6DCAD5F
              38D5DCFCE9BDB5BF787D768E85EC6AF123E490CFC7135C686F9F720A5397530B
              45F7959246DA22FB1A1A187FFCB16EE01AA0993FFE548DB240992BC97F995D60
              A5D905569A1D0F747A3C1E0CC3A8ACC8E2EF9BCE1B1B1BF17ABD15D504B02C0B
              A76118783C9EF781A3A5161011C2E1F0C5D7CE9E9D5CFBF083B66C749486174F
              B7CF7EF5028383837D636363910A7C37969797DFC8AF2447FD7E7F77201028B9
              4A6F6F6FB7EE72910D47C84647D15F79F5960ED2DFDF4F67676759B2582CC6F8
              F838B061A90B04020C0C0C9455B050028100E53C703E79E0CEFF486A35F09B6F
              9D7F601FF0EE3B6FAFF18FFDC1EE1BDC2AC19E9EA27685B503067B0A5D7E60DA
              77FC14570D58E7F7E3BEFE83A24A9BE07CFE3F6FF0DFCA2EB0D2EC78E0FA7F70
              646484582C564BCB7AE6E7E7D78FF3C0E16432793D994CD64654387300E276BB
              D175BDC696C2C96432B5266C9FBF0133981FE63A1D38F80000000049454E44AE
              426082}
          end>
      end
      item
        Name = 'button-help'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000002E349444154388D6D935D6853
              7718C67FFF73F2614FBE1AEC1756251B6D196EEA36B2D19B8E750C192B5E0E32
              CBA8422FDDAE76E30643866086DE0D418637636D163AD8C53E9C53EB70131A4B
              D6981645AB81349DD08FCC9CC6D8B4CD39E7DD456B68589FCBE7799FE7FFBE2F
              FF57B1057FF428E1F74FEABA118C811A04A228C20825200D32EA6CAC25CD6BDF
              D8E55B491001400184073E21D477AC0748F6EE0F1EFEE8B556A29D01C24D2E4A
              558BF4E3A77C975926552867815825F3DB6C71ECCBCD10FF1B47897C95EAE93A
              777B7134B3605B9665EF04CBB2ECD1CC82DD75EEF662249EEA09BD730200BD7D
              F86B5D73EFBA7AE648E4C5D8A1560019B9B3AC3EFE3947FCE63C97EF3F61B7E1
              A2BBC5E095761F6D3EB7319E33FB76ED3F78A9F2F7AF42249E1A8C25EED55F9E
              2CACD8472E65EDBB0B4FEDB58D9A3D9137EDDE0B53F664C1AC77124BDCB323F1
              D460B07F08BDF9DDE1F8A9B7F77675B71800EC097AF8E0600B1D010FBAA6E80C
              79B11C617A6195B75E08A194C2E7D1B8FCA0D4248E9DD08068B433C0767874F5
              7CC10A504B951A7E8F56D7A39D015044755F331A4A85C34DAE8600A51422224F
              566BF2D9EF79B9FEC8E4C3C3AD757DB35E85013444CC52D562270CFD308BE1D6
              F865E8006D7E4F9D2F552D1031114103D2E9C795FF999552727FB9AA3EEFDF27
              016F63875BF569677D154D84C4486609D9FA59DB331E7E1A15A594DA4E8A0823
              992504126BF93B6852AB26270AE5ECD84CB1C17D2367D27D3EAD6EE4CC067E6C
              A6C844A19CC5B193CFA6AEA08BB5E134F5F4FE7133BF126BF3B98D97DB0C9452
              04BC3A6FEE0B70A8C387E1D11111C6668A7C717DAEE80803E5BFBE5F5E9DBE86
              BE3E7F17D7EEBDFFBADABB7E1ACF997D93FF543A7C1E8D3D412F2FB51AAC590E
              7FE657383D3ECFB7538B594718A8CEA6668B3F9E05C7D93C269446A87F8850FF
              715D737B63C00ED7C8A83876B27C2B6997AE5E04ABB669DD3E9F1E6AC7FFFA7B
              7823AFA2FB9BB764C1A956589F9BA692B982559C6FD8C97F38F86521099944DE
              0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000003DE49444154388D75945D6C53
              651CC67FFF734EBB76A5EBC6BE815164EB3A24D1391D44B83030378C269AF881
              5E80CCC4A8371A4362D444A33212BC504248F4C2C4044112D10CFC080C22C998
              713A8C62748331C818631FD9B276EDBAAE3D5D4FCFEBC5B686A5F3B97CDFE779
              F2BECFFF9F475882A6E37B642FBE9D2F210E679D20AD40134200C8471101FA80
              734AA963B6190F47CE7F46FCF2F7DC0D0110A79BB2D64F706F6CF0811C46645F
              7D85479A6A0A0994B8F0387422498BDEC9393A06228CCEA4E68003A00EC7FFB9
              68874E7D087666D150D3297FF928EEEA07FD282E5417BB036DCD7EB656795909
              195BD1DE17E250D72831D3FA09783EFE77472A74EA230074DF8E7D781B9FF401
              5DDBFC0581AF7707B967B56B49AC6E8492321C4D916708F90E1D4D84CDE51E5A
              02455C1C8C06E3A94CC05919684F8747484F0C22EBDBBAD01C795F5617BB5BCF
              ECD984C7A903D03D1CE3DD0BB7198FCDCB52368FD516A9832D7E7C2E038081A9
              044F9FECC74CDBAD562C7462F4E3A7D0C4700645E4C5B6667FD66C2C96E295D3
              3769AE29E4B7D7EE53D7DE6C50275F08AA9BE124FBCFDECA7E3F589ACFAB5B2A
              010EE805C586BB6E3B9A88B4DE5FE1D1EECE6C6D411EC77707796F471565AB9C
              380D8D2DEBBC1C7962235D4331B91335B3DC7D0D65E419DA7A411E75D734A201
              4D4D358539E137ACF1DCBD0902C886C56C43092BCBF3B90C1AD779019A1C657E
              34446A0325AE1C4311C939FBF6DF295C86A89AD5CBF98112178804D00C0C20DF
              E3D073C44B504AA9B1D83C073B47E81C8C72B0C54FC1E25096B0A0571E010C94
              8A449256C94A6622C26C2AC33327FBD9B8DAC5E93D9BD85CEEC9E14592162822
              4A2934A0AF6F72EE7F5F383095209CB0E4AB676BD54A66008BFA5EEC0C1A70AE
              632042C6562B922BBD4EDED8B64639F4DC4C01862326BD1309013AE62707D194
              52C746665273ED57432B0A4CCB666B959750C25AD1F1E8EFE3D84AF528A5FE4C
              F677A3D9E66C183870E8D22843D3668EE08B3F26D8736A407E199AC9B93B7B7D
              9A1FAE4D6780FDE9F028E6AD2BE8B6394BFEA6ED3D294B3DF0F36034B8DD5F40
              89C7911515B917F6ECA1B5AB28741BCBCCDEEA18C252EA6D50DF4D7DF3015678
              047D7EEC3A46E906E5ACA83E134F6502ED57C39B3336D495BA71191A6B0AF2B8
              B72C3F6B763B62D2D6798723DDE319CB56EF0087A3974E10BF7C0658EC43349D
              92E7DEC753BF0B11D90BB4E5195A55E33A6F4E1FF64D24C456AA07D8AFA067E6
              D271A2E73FCFBE7C59D09EFA5D143EFE3A86B7D81091666027508B641BBB9785
              C6FE2B1D1E65FAC74F316FF42CCB357772860377EDC3B86B1A7194FA41D71104
              8542A5E7494F0D93BCFE2BE6E01550768EFC3F3CF88C34236EAC3F0000000049
              454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000050C4944415448897D955B6C54
              551885BFFF9C99332D33537AB337675AA188564B0C5A6CD53E7803D348490443
              7C203E888009488C86E88B241AC1441F342A112F892FDE88F8608AF142141214
              8482552EED80B4A0D8D232B4C3B4D333F7F3FBD04EEFB89ED6397BEDB5F75EE7
              3FFF16A6403CF3285CF10CFEA6D58861E68BC82AA01568400800161041090107
              55F54BA02B1B1B6268DFDBD87FEE67262447DC653751F6E41BB84B822622EB81
              ED225255EE73EBD22A1F81F916966910896708856D4EF7DB92CA3A59A00DD8A6
              AADDA37FFCC0D5BD3B209B9EBE80BB6C01151BDFC3F09514097C063CD2545DC0
              E6A64A1A837E358459B86A6764CFC9301FB7F73392CCC680A781AFECAE5FB8F2
              E94B90CD00608AC74BC5A65D9845154522FC9CE7329A5F5D5EC3CB0F06A92ECC
              4300190753B8D7325916F0E963B797CA9901DBEA1D49AD06FA5C37043B0C8F97
              C4B9DFC616287EF459E6DD72AF29C2DE3CD368FEE0B145DAB2B84873868EC2D1
              4B23F2FDB908EDFFC664D0CE50E11F8B0BC0EF71C9CA5B8BE9BC12978B91448B
              20873CC1DBFE4E9C3F46367A05A9D9F10B62BA3602EFEF7CE426D62E29D55C0C
              97A229D9DAD6CDA9FED169F194CC73F3DA8A1A962F2A9CD08EA61C567FD625DD
              83F10BA8D6C77B4E24063EDA828161E603DB9BAA0B58BBA494DCCE931995F55F
              9FA37738C55B2B17F2C7D6A59C79EE2EFDFC895BA92EF4B0B5AD9B3F2F8FCA64
              6406AF3E5C8D882C446463DE823B7195043044A45544AA36375532EE3D5655A6
              B0E9EE4A76ADAA65555D097E8F0B8F4BA431E8E793C76FA638DFCD07C7FAA79D
              AC31E8A7E1469F021B3044F2EB9A3180D6729F5B1B837ECD01C010744D7D892E
              0BF85055C65FA3AAF82C937BAAFD74856DA6CE0174555D31C06D20B556452D06
              D0B0B4CA87217357CB5C1C4486E219BC96396BAC21E01FAB7FA1C1F416612012
              08CCB76617FA24A6EE1047953D27C3FAEBDFC3B26251E12CF1A49704005C8095
              2BB9A9465379EE39148EF3CA4FFFE8F1DE18CD3505BAA9B1729A4C5575DC4B50
              B5C61650BD762D9E299D3CFE98702A07E8194AB0F68B107ECB64E78A1AD6D497
              4EC43A754EC49E6813D7407101A150D86EFEBF8C44849FBAA3D8A92CFB9FAAD7
              0ABF95339DA53D1BB673348482011C3C3560CBA09D99A3E34C2299711081729F
              FBFF641CE88902D8AADA9E890E60A8EA9E54C6C9EE39199E567233F9967B2AF5
              AF171AE61CCBF1E164866F3A0761ACC38E242E7460009D40DB47ED030CC4D2D7
              2DCDDD47FBE5DDC37DD2D137F9F7CED4ED3A72994822EB28FAA6934E103FFB1B
              46363684C2B6916426F6FCB73D2433CE9C47DF7DF432EF1CEEA3A32F76BD68E4
              93130382EA874047ECF7EFD0E4286666388CB7FE81888874F78EA456775E89CB
              43B585B8CDE99FC46B99726F4D017707FD9479DD3AD37C6B5B372947DB8175CE
              68341DFEF445349DC44C0FF4E02E0D6255D676825CBE1849B6EC3F7FCD5C5C9A
              2F81F99E8918EEA8F4B1B4CAABE57E6B22A2916456DEFAB54F761CB824A9ACB6
              03ADAA4E34FCE576D27D7F01B92BD37453B6EE75F2EB9A11B81FF85844162EBB
              D1A7AD75C53404FCB92B5323F18C84C236077BA27CD33948249175C663D9A6EA
              D8835FEF24767CDFC4E92673305D14B56CA1E0BEB580E4216C126403503767E8
              828DD2A6F0266887331AE5EADE1DC4BB0ECD90CD8055BD84C296CDE4D72C01C3
              14515D8448031040B05022400838A668CC4927889DF88EE88FBB71ECE139F671
              1DB84A82E4D7DD87555E8BE99BDDD4D283BD24FF394DFCEC1134393A87C318FE
              03D1B45DC94CDA63250000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000631494441545885CD975B6C14
              D719C77F67D67BF57D0D9BF53AF8B2C6C40623B06BF2509C0454A1AA24A618DA
              8A26ED1B6A13B5859762A12A7968A8541290AAD236B402554DA216A9B5B01353
              A25ED204993A20438CB9D80A5E8C5DF0AE6DECF57AD7DE5DEFECCCE9C3DA86F5
              6DD77DEA5F9A8799F39DFFFF3FDF7CE73B73046942C9B2632DFF12A6A24A32EC
              2E84C1383F26758DF8840FD5D747A4AF132D389A2E2D225580B5723B39F507B0
              94D716096168045E00AA0117021B9230E0056E039740B644076E0E053F6B267C
              F39F20E5FF66C0F85419058D47B1946EA901DE1042346C586355EA4B72A87258
              29CC3661352A44541D5F2846EF6884CB8341EE8E457429651B706CC6D7D7357E
              FE38B10777566720B3F6450A1A9B2C8AD1FCB622C46B0D5576E5E036271B1DB6
              5409A36734CCD9CE61DA7AFDBA2EE5695DD79A021FBD130DB6FF313D0339CFBD
              4CFEEE432E21B8E0B65BB69CD85DC6D6C2AC94C20B71C337C5918BF7E9F747BB
              A5E4A560FB9FBC13174F2D8A333C799359FB22057B9B5C42D0BEC39D57F9EE37
              37509C675952603AA6118C6A180D421A14B1E8459CD926F657AFA1F751C43918
              88EE33176F6E969A1A9A19E84E8A9B9F6874945178E80F1625C3DCB1C39DB7E5
              B77BD7633424F38E4DAB9CE91CE6A32F26F0866202C020909B9ECAE41B9B0BF8
              56F55A99614836A36A92575B3D7CDA1FE896BAF6E5E1333F8CCEDCEF5A6CC0F9
              EAEFB0946E39E5B65B7FD0FADD2A324D49C9A1DB37CDF75AFA783AD7CC77B63A
              D8ECB461331A78341DA36330C47B5DA314E59838B3AF827C6BC6A26CED7DBF87
              7E7FF437B1478387BCBF7805F4F8E3006BE5764A8F5FA971BF7555BDFE70525B
              88D15054ABFBD575EDA7FFB8AFA9F1F8A2714DD334FFF48CB6FFFDDBDA2BE77A
              B4787C71D0F587939AFBADAB6AE9F12B3559CF7E7D5E5B01C8A93F00F07A4395
              5D59AAE002D138BBD6E7F1939DEB58E27303906BC9E0D77BCAE9F24EF3EFC1E0
              A2A0AD85593454D915E0F5ECED071E1B50B2F2B1B86B5D42883D07B73997242F
              B75B39B6AB04450831FBD996BC1C5926B1B33C974FFA2797E439B8CD8910628F
              C951EA323ACB1306ACE57508C5D058B1C6A2A45AE752A6686BC0BA5C332353EA
              92631B1D362A0A2C8A10A2D15AF16CC280A9A81260477D49EE8AC46299D42FC4
              585825D7625876BCBE341760C7AC2E4A86DD05B069A3C39A96C04A59988CC6E5
              C79E00DB8B73969D3FABB32923DF9530200C4610B80AB34D29C557CA823FACCA
              D75A3D14E799F9EA86FC654DCEEAB8444662375566A96D56A392D2C01C9ECC82
              AAE9F2DDCF47E4AEDFDF469770665F054B75C639588D0A08315F6C19B38C9168
              5C4FABE10B2198D31F9C88CAEFB77A0846358EBEB08EFDD5055249512C115507
              2923F306A4AE010C7983B167D231F004E4E10BFD94DB2D9CF85A19B644E74C59
              A9BE500C6088842E4A7CC20B70A77734B2C2B46408210844E2DC1E098B23CF3F
              3D279E166675EEA87E6FC280EAF3005CBA3C184C9B046046D301B0ADA2760066
              752EA9C39E8481C8BD6B4849CBDDB1B0DE3B1A4E9B28BDAE908CDED13077C7C2
              3AD012E9EB4C18D00223CCFCE7D690945C38DB399C369923CB84E7C775D29195
              7AF9CEE16CE7305272419DF00EC51EF6240C00043BFE0CF0E687BD7EBDDB3795
              36E16AD0ED9BE2C35EBF0EBC19EAF8CBFC7305207CF363667C7D5DBA94A78F5C
              1C603AA6A5243CD5E1A5E2E43571AAC39B32763AA671E4E200BA94A7E393235D
              A12BE7930D2075C65B8EA3EB5AD33D7FA4FB705B3FAA9672DF490BAA2639DCD6
              CF3D7FA45B4AD934FEC149A43A333F9E544BD9CFBF8C7DF78F5C02D1BED39D57
              FACB06F7A23FA3D5603AA671B8AD9F4FFA030352CAE7262F9FF306FE9AFC639A
              C41E1BBC8562CE0C994BAA9B072666BEF2774FC0B9D969C399C63EB110377C53
              1C3CEFE173EF54B744EE9ABEF137AFBFF504909CD945AF17EDBB8A8CAB218BBB
              F6BD89A896DB7C6BBC6E60222A8AF3CCACCD342E0C5F849ED1303FFFF4013FFB
              D703DD1F56DF91527E7BF2F2B9097FEBDB4B9E92965DCEE6B21A0AF61DC5B4B6
              A406780341C3336B6C294F465F8C8575246DC0B1F8E448D7F8072789F4B42F6B
              78E57EA218C8AA7B899CFA03181D654502D2381BD2A2FA8786829F353375E57C
              52C1ADDEC013301656605D5FF7F874AC1813B325203554BF1775D843C4736DC5
              B3E0FF1DFE0B6C26DD22171F283E0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000008A8494441545885DD987B70D4
              D515C73FE7B79BDDCD269BC726212424841888011A14D4220F83B6A588A368A7
              58198B743A8E32857670E81FCED887ED8CD36AADC3D4226305ADD2A12868AD1A
              948EB4D6A005411E12A821C62018F2DA64F3DA6776F777FAC726016137D944FF
              EA7766777EF3DB7BCFF9ECBDF79C7BEF112622C3826D4A15F6929958F34A301C
              2E101040897F99815E22DD2D84CF9D24D2F6C984DC40DC66CA72545E4FE6DC65
              38672F416C4EB7C012600E301DC84170A0F8010FD0081C51F88F19E80D054EBD
              CBC0A1D719FCFCD4570FE8A89C4FEECDEBB115CDB08BC8DD083F106431601802
              53B26C9AE3B062B71A0422261E7F048F3F22008A06516A81E754F51FA1E6A3F4
              ECDD9232E8A88086331BF7771F2263568D21226B819F89487185DBA1B7CD74B3
              B02C8BD9939CD8ADC6657DFBC3518E9EF75377A68F3DA7BD7407A2A2AAC78007
              55755FFF07AFD2FBE61FD1C1E0C4006DC55514AC790C6B4EE10C811D2272EDF5
              A52EFDF18222E697BA10497D754462CA1B1F77F3D4C136CEF68651D5BFAAEABA
              C18EE601CFF607897A5BC607689F7615853FDC846173AE40F84B9E332DF357DF
              9ACAF2CADC71815DAAC1A8C99F0EB5B3F9602BD19836A9725BCCD7DDD8F1CC3A
              229EB3A901DA8AAB98BC760B86DDB90661DBDCA24CCBE615154C76D9260C76A9
              8E9CF7B1FEB5263CFE4817B02CDADF7DBCEDA97B89F5B58F0E6838B329DAB09D
              B4ACC21518BC7CC3B46CCB96DB2B70A659123A8A99AA875B06E468AB8FF68108
              1153C9735A9959E0E48669596439AC4921CFF68458B3BB9196BE70872A8BC36D
              A79BDBB7DC07D1C1E48005F73C8A73F68D950287E71667666EFF5E6552B83D0D
              5E7E57D7C2F9FE41112037DDAA6916C11B88123155EC56D1BBE614B071D11432
              ED896D347B43DCB5B3016F207242D105FDEFBD18EAA9FD436240C78CEB29BC77
              9321C8C13C67DA35AFAF9995705A55551F7DB7459EFDB043AA0B9DBA767E118B
              CBB24620A2A672B2DDCFAEFA2E5E39D9C5D41C3B2FDC7925C559899748DD993E
              EE7DE513315537692CFAD3D627D710E9681EF97DE4AF15ACFE0D5657FE5A11B9
              EFB1E5D3985B9C99D0E08EE31ED9F47EABAC99374937AFA8A0323F1DDB4569C6
              1061B2CBC637A7E7B060AA8B5DF55DFCFB4C1F2BABF3D5625C1E6165B90EBA03
              114EB4FBAF13C3F8BB252BBF337062DF057BC3A3672B9E61077E3EBFD4A5CB2B
              7313C299AABAABBE8B9AF22CFDC54DA5588CD123FADA12174FDC524E8327283B
              3FF2246DBC71F114721C560BF0B073560DD6FCB22F0266CC5D8620778B48F14F
              1614254D258688BCFCFD2A1E5F5E9E72BAB9A9228779C519FAF2C9AEA46DB21D
              56D6CC9B840877805464CC5D7611A0612163F612807B2ADC0E9D5FEA1AD5A1CD
              62E04EB7427CFDA6F4B9615A360D9D4142515393D95D755501868888B03A63CE
              372E00DAA65461D8D3DD2252736B95FB4B25E264CA735A51105F3896D47861A6
              8DAF97B800EE482B28C3C874C701ED253301A9018C456559293914115435E968
              5CAAEE40140175D92DA3F65954960522D582B8ED255571406B5E09C01C436076
              A133559F294B55F55F9FF63273523A76EBE85135A7280389C745B535AF340E68
              A4BB00A61767D934D1A9640CDF638EE2CE8F3CD4770464F5D593C634382DD73E
              FC5861A4C7D35C7C2F12727247D9961249446434BE70D4D4A73F6863F3C1366E
              2CCFD695D5F9CA18C7BB8B187265A8A975A887C336BED11B865455452E8A2C55
              D5BD8D3D3C5E779E737D61F9CEEC3C7D646919460AD1373283228EE177716425
              108A98E306E49211E90E44F481DA660E9C1B902BF3D375FB9D95BA30C5C00308
              0C33A8FA47008726C9E3F147260238644FD554B8FF6F4D34760579646999DE99
              646B1B4D5D1718BA347EFDC230FDBD008D9DFE880C84A3E3861B9EB9A3AD3E3E
              6AF7CBAF974E65D555058C170EE0536F68F8B1D1F4F7C50123DD9F031C893BF1
              27EC9882B4A52F7E8EBB764AE243462A3ADAEA43D130503FC485113E770AE080
              AA06EBCEF44DC8B088883914D11318B811D59DE903E5A0A2C1C1381746A4ED13
              6281FE2050BBA7C14B3496F206F195EAB42740832728C0EEC19606CC902F0E08
              1038F90EAAFAE7AE40545E6FE89E90839AF26C76DC75A51664A44DA8FF73473A
              50D4AFAA2F064EBD3BF2DE0018F8B01684BDAA7AECA9036D0C46C79F720A32D2
              985FEA4A78471E4BCDDE10AFFDD70BCA5635635EDFB1B7467EB300C4FA3A715C
              318FB4DCE2E6BE70EC1EAB4518EBD875A98EB7FA38DB1B261C35713B531F4553
              950DB5CD9CED09F580AEF21D7B2BE03F7A092040A4F32C99D7AD6806661C39EF
              AB5E38358BA224F788445AB5B381178E764A2062B27446E21379226D3DDCC14B
              27BA04D8604607DFF3EC78081D5A7F3034C500839F9FA2FF835751747D24A64D
              EB5E6BE26C6F2891CDAF4C6F37F5F0C4FE16547597AA6EEB7D7B1BB19E2FDE8D
              BF9013C496CEE4755BB11556548AC8FE926C7BFEF32B2B29773B184BC75B7D84
              634ABED34A455E7A4A700FBCD14C286A1E54D5A5C1A64381CEE73682C6920302
              58DC5328FAD133585D7957036FBA9D69854FDC524E4D79F6984E5391A9CAB387
              3BF8FDFE16A2A61E046E1DECFCACA7FDE9FB3103FD97B5BFEC46ADC101020DEF
              E3ACBEA95D6CE9AF86A2E68D6F7CEC9DDC1D8830AF3813C704A27458CDDE201B
              6A9B79F1449798AABB557565C4F3D940C733EB19DA72C7060430037DF8EBFF89
              BD625E8FC595F702E03CD1E1BFEEA5135D46286252EE7690694B5C2D48A486CE
              008FD7B5F0CB7DE786A2950D8A3E146C3A14E97C7E63523818AB80694D23E7E6
              F5642F5C8918D66AE06144EEB00832BFD4C5A2B22CAA2767509E6BE7D20266B3
              37C891F37EEA3EEBE3B427281A3F426D057E6B46C29E9E7DDB18D8BF03CCD173
              6E4A1B675AE115E47C7B2DCE593588C8746035707BFC8243D23957D5307000D8
              0DBCA4B1A8D7777C2FBDFB2E8FD62F05382C6B411999D7DC8273D612D2269521
              481EF0352ED4A8D3517C4017F11A75BDA2C1C1D64602F5EFE03BB28758BF673C
              2EC70778B18C4CF7852A7FFCE275A1CA0F98FE0B55FE8B13EFFF9DFE07F072AA
              54BE6418950000000049454E44AE426082}
          end>
      end
      item
        Name = 'debug-stop-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000006C49444154388D6360A01030
              32303030E4898A3218707292A4F1C1AF5F0C4D2F5E30B03030303018707232B8
              F0F1FE23C5804BDFBF33313030400C8001FEAEEEFFC468FE5856CA0863339162
              2B36306AC0A8010C0C68291139859164C0835FBFE0699B5870E7E74F52EDC20E
              00445918C6387FF1D80000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000000A949444154388DEDD3310E82
              401040D18F18B2D9A807584DECEDA9A5F32A7A098FA0A51C072A3D0125E516D4
              60A6D9C44649483461C1925FCD14F3BA813F17006CA38858EB5150214221C21C
              20D69ACB66BD0792815E76ADAABC05DF2528750E8DF1929CB5200290031D90D0
              1816C7931758A7375C59B6FBCCEBBA4713388113D8A7CE2F3B6BA9D39B17E0AC
              FD096688741EDDA3EC3304003BA538AC9643A0B67BD3F0689EA38CAFBD001DA0
              2EB1B85F6FFE0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000000E7494441544889ED95C16EC2
              301044DF9A4845E22FC8A5557FA3BDF1F7957A62FD1B806432BD34C84E1CA06A
              A21ECA9CC63BD68E76F6B0B030AC279B10666D7CE83ABADE6013029F2FCFB31A
              EC3CF2713CD2E44533EB0024C9CCEC1ABFA249D2AAFF531800ACB65B356D0BA0
              AC3CC58B7772D739C6422C0C24A9695B9EDEDE874DEE83B0E45E948ACDE6E3DB
              37A6784DC3206B31365802A3887EC2476FC1407E44F41F227AECE0EF77B07844
              C37B60C95D08BBDC3A419557B4141D332B4C86F740E71849EE972C2555F92DAD
              6A905FA262F2097E4BA31F3000AFEBF548FC0DF6A713878AE1ECF80260AAD1E5
              DDCEE2550000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000000A14944415458856360180523
              1D30A20BE4898A32187072D2C4B207BF7E3134BD788122C682AEC8809393C185
              8FF71F2D1C70E9FB772674310C07C0004F7EC17F465E5EAA58FCFFF367862F13
              276084365E0730F2F23230F1F151C501F882132348E80D461D30EA8051078C3A
              60D401A30E1875C0A80370B688FE7FFE8CB725430AF8FFF933E90EC0D586A336
              C070C0835FBFB0B65EA901EEFCFC490B6347C1100700ED8B2416F7249E410000
              000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000000CF494441545885EDD7310E82
              401085E1A7586D25E10270031A6A4F61F01C1CC4D633EC4968A9AC48E0005082
              4202B1A130013626C35298F795339BCCDF2E40444687A5E1CD3D23526AD790BC
              EBF1A8AAD9FCB4F438520A57D7BD03086D874DB2B469929F0327E1D1F32E8E1F
              D8CB02309405C6BA5EDD9B02E1F801541C6F1EF5ADD5DA1878B47A7D030C9462
              A01403A51828C54029064A31508A81520C9462A01403A58CFFE2A12CD06A6D35
              60280BE3DE14988D756DFC546F285B5B2C06E65D8FB469127B3D73CFD77BCF73
              44FFE303277B327A1838E2BF0000000049454E44AE426082}
          end>
      end
      item
        Name = 'save-as-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000026449444154388D9D935F4853
              511CC7BFF7AE392B69DAAC81A621390812D20711CCB4C0DB9DD34C31CB2D4819
              45E0930F4EDF0B7AD64DC65E1A37B1D6EE76ABE5DF02F34F33B4497F885E224B
              D8DC068D9CD3CAB5794F0FB1316B45F885F372F87DBFE77BCE8743599B6E89D8
              8196C3CBF4F5E91BD80500359D9564C6FC9C6A173AFE1976BB85A313B3893D7A
              27A70300C80E0362B198FBEBFE8D920F5B4B282A2AFA7585FF95344B3A737FE4
              51DBC4FB27DCE68FCD8ADCDCDC6BBF37A0FEB6A29BD1C9D0E19076F4D5B853D3
              A03963B55AE5144559B635E0AFD81321DB2493CB6683CAE0E5876E9743B64756
              A9D7EBC1711C08219E64404D67655A733C1E9F16865DDA51D738DF70AEE18442
              A1804EA7C3DADADABC288AADA90D0800A4229266499FF9F67975136F1FF3F58D
              F555DDDDDDE8EBEB432412592084D4050281F5B414DA850ED236A47DFAB93074
              7EE4E5D83DB5A6AECA6030A0BFBF1F82535828CC2C682ACD381EF1FBFDE93146
              BF47A7EC77EC3AD79C8BD79CD59C34180C301A8D109CC28B8AE2F266B5523D94
              97910700A041C847DF1B7FD22CCD92BA2D372D5AEE2E676718A6BAA7A7072693
              094E87D373AABCBAF9AA563F48C7E8DAC43C4D80D34BEE4F4BDED72B20846C84
              0F7C6911A6043BC330D5BDBDBD30994CE0EDBCA72CBFB4F9127B91DB9D91599B
              DA96BA70AC15EC11361F1426257B257363AB63A3F243D90EB3D98C818101D86C
              B6C5425941237B901D946C49988431F999F8770EC4B6622B25CA92A3AB9130BC
              DF7CC3C5652A747575C1E3F12C124258793C3BEC0D79D954C881F500FEE0AE52
              A9909393330F2002C02A8AE283603018F5F97CE9DE1A00F013C303037CAF6863
              AA0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000038849444154388DA5935D4C5B
              6518C7FF6FE9E9697048D7AEB546712448D64483EEC20EF0CE8C7634A53DCE44
              BCDD35913195420D3707AEB8B2D1AC4EC9867A6132A34BDA15C6A11037377152
              CA046D028365061CA5A11FD4D2C229E7F4F5828F140853B7E7EAFDC8F37BFFCF
              F3FC5F7285BB5CC0D307EF9BF1F1BE193F940050F566255D8B6791176598CF99
              E9BF6527FF4C22D43FAE78CDF90A9DBFF310D9446EF74E0900478E3D03292F03
              B94D185F35FE67599A17CAA16495A014A0DB3214FFAFB28341089108416C67FF
              54404992C3B251324DAE4F5ECA9467603299B64A7E92604A99F04F377F6E1888
              0C9E8DAE2E4F50D085B2B2B2779F4821FB2C1B128F8B961FEE5E6B5E4EC4FA5A
              5A5A34F5F5F5358490AB7B14AEAFAE63FEC7F9C73E52AA2B0D3D883FB0062203
              EFA5C4556F575717B1D96C2084606C6CECF82E505DC682504AFEB8367D28AC40
              0B930975B2F1C6FDA1E65822E655A95464616101434343F07ABD28140AAE5DA0
              D164204693E171B0F0DDF1098B6FD4FF4E6A23F5B9DBED267ABD1E2E970B9224
              4192A4F729A5978B4BDE35F42DEF182986A98EA87ECB576C587D53D7CFA63652
              97DC6E37B1DBED100401B22C4396E5564AA9371A8D1E3EE537CE99A9B6524B36
              32EBE3C255C1E6BBE3E7D29BE92F3A3B3B89DD6EC7F0F0307A7A7A4064725EC7
              E82EA613692C2D2D1DEE436DA516E515E5137F898FACC18511677A33FD654747
              07696A6A4230184477773718C2B499F5E66F9C7A87D5003D806D63537AF0FB6E
              64D643E17B618BE733CFDBD158B4CFE5721187C3816030089EE7A156B16DF6BA
              C6AF6B5F340B2C616B77F214A034347FFB21F2B97C71CFA685EF049BE7530F17
              5DDE82399D4E8C8C8C80E779B00C7B817BCBF195A3C13EC894306600C076D715
              14B06493B95FA77C1188D93C0088CA2A45A3FFDE757B3416ED6B6F6F5714C318
              301F584F36F45B4F35DC6014CABAFD9529E71273E96A6DF5996C323738F9FD74
              9DE6A5F281D0A389B59498BAD87ABE55C1711C464747C1F33C20E3C393DAD7AF
              54AC560811FFCC2900588B67F7023FF9C5830BB56D7F57EB5E3E23AE89ADF723
              7381A9C4EFCD9AA39A528EE3100804D0DBDB0B49923ED229B59E13CC094B3EBD
              29E4D39B4211E7E6CE8200005BC2E274D569949012C84765C40BF1DB86E70CF5
              8542012B2B2B22808F29A51E26A3C431BA354DB2C7A9C04C7C16B3F159EC3B06
              6A6A6AA056AB03009E07D04F29FD96529A8CC562585C5C3CE086FDF10FABD099
              7F997B0B7E0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000004D2494441544889AD95DF4F53
              7718C69FEFE90FDA9E967628A3A2842520432AD52053674C4C14D090FD209BBA
              252EDE2FB870B1FD05ECD2B5EA8DBBD0CD5D2C9899CD6834241B2532C41FE836
              10AC2DF4D043A92D146DE9E998D8D273DE5DD0036D04E6C5BE57CFF99EE4F3BE
              EF93F739877DDFF69D020044448C31F6BA7A9D7744449ACEBEAF119242D00280
              B5BC986C9B8B117AF814650E3BD91D6504B065CCEA7AE979C61B43CC3BC3DEDA
              5D81642445523485FCA32522B26D2E466563054D3E08C3EE28A31DC7771200CA
              27ADA1812BC388796750D95841A0304B46A482D7DCD2584B5DE53453B53AFE6A
              7AC52AC6B002409E834B05F03F1E4EC35E6AF4DC650281714BC5B44444EAD439
              4D2B1A584FAB3700A010F9B2966CDB18C6277C093FEC5BEDB02C58A0CDD942EA
              F8B9695F7B8B00C67426DDD8D0DD4787FA267F370C4F8DF4C98ADC08E06783C1
              F0B9768D69D96B680060A60D461F36A3E5FAFD9B0631257A78335F75E0C00174
              77779F946579558B0080E53BB196566465246DCDB4DC8E0E1405A5A0C76C3157
              9D3D7B16B5B5B50400376EDCF8F8158BFCDD7E126F8BF9DDD26ADAF08661CCBA
              DB7AE4975B570DA3C1D15E8594AAF6F6766CDBB60D7EBF9FF5F7F703C05FF916
              B1CA772A72E08270BDC297B3B22F1C7BDAF2D3C52B7AE1B9E0E1CD7C95C562C1
              F9F3E7A1D3E970EEDC3948922466B3D993051655366EF9EF7031F80342B06960
              F80E177816F0F066BEDAED76A3A4A404EDEDEDE8ECEC04114D1251138050BE45
              05DBD2FFED3D4ED5EABDDEAC0F186AF4CD3D815E8D7F7ACCC39BF9EA3367CEC0
              E170607C7C1C0B0B0B001022A2838AA28462B118D6DA220050BF4B0018D2F32F
              1FFB867DADDD03DD08A6C41E9EE7B7AAF04020808E8E0E2493C94946AC89D7F0
              A1502C844824B27E92ED0E3B761CDF09E731A7575BAD6BB99F1C548292E83199
              4C356EB71B75757594079FB2E96CCDBBAC0DC9B6D20F41A92557575953E46902
              117947EE3D3ADC75AD4BF3587CECE1CD7C8DDBED86C3E1204110980A7FB3B8F4
              E0DED2BD2FCA33E5BD90A941E56819636C71219B97E415DFD3F319EFD0EDA196
              AE6B5DDCE083C11EA3C958E372B9B07DFB762CC3E79253F6527B53DBFEF7E737
              A537FD161D9E71E6E79103D1CDA8778645BD330529D59BF5826FF849EB858B17
              30F870D0633419DF76B95CA8AFAF872008AA2D61FBC6B2A663CD1F25F7EF7AF7
              579D4EE7CC35B8528080A3A4D035A15F4464645AED5EB1D6595A6F4DF4654727
              467B8DC66538A9F0B9B9B9F0067EC3C10FF6BD27EDA96BEC61C49C202A800380
              36F552CA141B8A3F25C28FC28078F49FC40B98CBF8DE403C208829F1BAD164AC
              75B95C703A9D989898601D1D1D482412619BCED6B4A764F7FCC664A967EA61A4
              1E44484653608C1514D19EBEEBC257FBBECC588BAC270844D34F624729A45C78
              F0FC8FF24579F1F0896327E0743A110C0691833FE5C035571BAAE25B942D9EB8
              90A88F2301227AE56703005CF4EF284EDFF906523AB948449F109166ECD9F8D5
              8814FD4CA3D1E81A1A1A70E9D2259C3A750AF1783C02E050112B0A541BAAE720
              6317813444A401A0215AD6CB67B9A4B5C80A9BC1068E6328DF5A8E456487388E
              DBC1711C65B35919C02D005F10D1787C360E3929AF17214CCF4F23236756922C
              A5254869091CC76123958231F6A7A228BCA2289789E80700A2A228989D9D4538
              1C5E179E7FFE058917D084AB1DDA590000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000004DC494441545885ED976D4C53
              6714C7FFCF6D8598552DB208FB02A97E98894A48946EBC08EA78DBAC20906CC9
              321435EE0370D148231A220395BDB82CEB2638D986E860CCBD98A05D5F302142
              2460D8661CABC332905A1BE8A5A52D08D486FBB20F8E4AA51565CA97ED2437B9
              E7E63CCFEFFF9CE73CE7DE4BB6BDAC802C448685B2E1F1619C377CEFF5C5B210
              19A2C3A3F98512607299A899BE78FA26293FCE2BA2ED5407B5F342DEBF869DCB
              3E2B2415C40933E77D3466D68385B6FF05FC070508BEEE8209E0797E840BE63E
              1DC5284243434151D48209201CC7F5B7FDD6BEA5C9D6C477DDFFE5FCAA55ABB6
              AC5EBD1A2291E8611F785EC672DC5F577E6DCD696CFEA12E353D757D545414AA
              ABABB300BC151919D9F47C058860BAA4D364ABAF6BCE64E564ADA7691A8410C8
              6432F19E3D7B3E5FBC7871D373DB028FDBD3CF840D6F6FEABA54BB3D7B7BCC34
              1C00A4522900BC0020F0169CCB393B7FBA08B7AD2B982CFD4D7D0D1113B95C2E
              F7C2474646A0542A01E0E8D4D4948F00F2CF85A4FCB879B3398EEBD55E6ECED4
              74686A3373325F4D4B4B434949092A2A2AB072E54AD0348D8181813296653FB3
              582CCFB608399EEB6DEFEECCB8785DFD75467646EC8103074008416565254A4B
              4B21914860369BDF6359B6D26834627272F2D91D438EE77BDB6F7466D46BBEFD
              2A353D2DBEB8B8D89BF688880848241258EE5ACA8385E0E346A311131313007C
              6B40C0AC3EF584708EEB6DBFD191F98DAEF1CB94B49478A552294CC39D4E2768
              9A26F6417B79E2B2C4CB223785AB1357BD631FBB05FEDEDF8F1A1191FEA1A543
              59CD7D974F27A7266F3C78F0A0B7CB399D4E141616C2CED82A7253DED6926E51
              8BC96392CE1C3F670DECBC9017F06B89655963C3C9866DDA9F7435A96FA4261D
              3A74C8074ED334EC8CEDD82E45AE46BE26467FF5F76B4B1F94F9439B770DB02C
              7B4BAFD72B1A340DA793D393371F3E7CD80B77B95CA0691A36EBF0F15D8ADC9F
              E56B62F4144585F89BE7A9049CCB394BCD806F53A9545F2426266E292D2DF507
              AFDCADD8A17E656D60F8530B980157A854AA53090909C9FEE0CC20F3FEEEAD3B
              D4F2351BF4840486030025088020086EE75DD79C70B288F4D59FACDFAA52A9AA
              E3E3E3538E1C390291480400181D1D4551511106EF0C7EF05AF8E6E698B51B74
              E4312BF70A18BDEF02800C83B6E7BEC3EC0C18C873BC652C722CBDEEC7BAAAD8
              D8D8B4B2B2321F384DD3B0DBED1FEECBDFA70EF7BCA47D5CDA7D0434FEF11D0C
              8CA185637985417B6B72E48E7F110EA7A3F64A5FEBBB718971AF979797CF5AB9
              D56AFD48A9545E52BCA9D041C09227810300C5F22C4E7655C1C0185A798EDF7A
              53776B6264C0E1132408027FADBDB3D16AB5E6161414CC820FF4DE3EB1F79DBD
              17376DDAA4A3286AD993C2013C3C95624A8C427921D6AD581B07022D2164A937
              2A086DAD536D9F04850635D5D4D400785070FBF7EF47CF9F3D1F6F0C49D0AC5B
              B24E0B0192B9802697893ADA7ACCEB7B1B11CBB3A8EAAAC2AEE8BC8EF025E152
              02F2409E008C89EFA16FBCEF42495E09E9EEEE865AAD464B4BCB94DBED3EB108
              E2B2204F104C53A6594DC69F0DDD1BF29F814026168B111D1DFD22455196B0B0
              B0450CC3F40038230842BDDBEDB699CD668C8D8DCD4D0E34FF5C0152A9141445
              6502686018A69665D94E87C301BBDD8EF1F1F17983A76DCE0C2C5FBE1C111111
              C4E3F108369B0D0E87033CFFEC7EA6FF06C5B12C782C2520110000000049454E
              44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000006D7494441545885EDD87B5013
              F90107F0EF6FB3D998581407A730383D1508826805EA8BF3C0A1F780AB41C6B6
              3EA6D3A9336A6FDADA2AF5813878E75CB9F3C52B6C9C69A7D33AA3ADA71DBD96
              6BE40E2BCF20243CAA771C5CAF3CDAABD631100844F2D84D36F9F50F0A350610
              3454FFB8EF4C2693FDFD7EBB9FFDEDFEF6F7CB92D4C5A95087A9F1BCE4DCAD73
              7EBF5975981A2FBDB0A10440E2B321F9E59B01C0FF7E27CE8F9CB73174D1FCF1
              02F3E7FD087D610189488808AAC0DE6FA7BD753D64F19AAFD1B16DC203017D7F
              B79089EA8F0111BA683E963CD468F89E8D44244420717B703BD6DC61466F5DCF
              A3C79A14C804F5E8B3902F814F9B2F814F9BE70A48290DD8F65C000943EC60E9
              6587D781909010C864B2F132768A76B3EE02403C9264FADBFDCFB7D439EAD577
              87FFBD233E3EBE5E92A4FB5D5D5DB0DBEDCFB6073D92A7C970FBA6E6DCF50B7C
              8FADB79E9BC75D04D0C5B2ECA665CB9641A5523D3BA0288A46C3ED9B9B2FDFB8
              F26B8552F1DDF3E7CFA3A2A202BB77EF5601B82A93C9D64546463E23208BBFFE
              59FF61D6E51B577FA5502AB6E8743A4445458110823D7BF6203D3D9D0390C3B2
              ECFF1F4838D2D617D6FFFA879F56FE929BC37D47A7D361C992257E7542434301
              40357A2E53A4A7B607E64E73D070F2AFB06D77E7DCFD565557954EF489DB72DE
              C8A14B972EF5AB633018A0D7EB4129FD1DA5340038BEA28888FB2A8411316838
              C12E180DAD4D9BEBEF1B78D53CD5F6CCB44CCAF33CA2A2A2B072E54A00404343
              038E1D3B0649928E01B86AB55AA706062B92576A34B4346619074C3A65887207
              CFF3888E8E8652A9C4810307C0F33C060707919F9F0F8FC7F32680937D7D7DB0
              582CB3FF1C94BC52A3A9A34573C5F0479E5130DFE3791E313131008083070FC2
              ED7663FFFEFD1045111E8FE72D0027CC6633EEDCB90360966792515CABE662E5
              1FB494A1DFE7791E6AF5FFFEFF300C83B4B434D8ED76E2717B8ECB207BF761DC
              44401AAC8F2449374D9FB66CBAF8D1A5121FF1FD40ABD5D2D8D858BF3A8D8D8D
              343F3F1F1CE18EC729E3CAB2C3B2FD7040E0280E98AD850702CC932CC7278B5C
              C536773BBA35976BAE1679E1DDC96B791A1717E757A7A9A909478F1E859C61DF
              5E1BBE56ABF6A92B19CA6C78745F8FBD07851111FF6ABD4BC2132202971A13D6
              77993EEBEFDC547BAFFE0CE4D855565686F8F8F8005C5E5E1EE432B660F3064D
              F1DA456BF45D55BD2934B07FA63F48327F91F958A0DBED6EB8F29B2B59B5B7EA
              CE5039F69469CBB07CF972BF3A46A3710CF74E76AAA62833E555BDD322A40100
              2181172A6883C4E3F118AAABABB3DEAB78EF1495E30DAD568B8484844970F277
              B35335859929AFEA394E9136D57E9F0868FDA715956F558E9FAEC7E331545555
              6D3E7BF6EC0941107E545A5A8A152B56F8B531994CC8CBCB03CBB027B253379D
              990E0E78C2F5A0DBE1465FA79900A00FE1DE71B95C3F292D2D1D9F19C6D2DCDC
              8C23478E8CE14E67A6BCA6E738EEB1B85120A5132EB5A713C121186A0DB5593A
              9DAE4010849F96949460D5AA5501B8DCDC5C303EE664D68BAFCF08370A1CCD03
              4BCF002213C2C1A9B8693564E630A60BC517B2CA4DE56FBB5CAE7D252525484C
              F47F0BD1D2D232863BB576E19AA2E4F0E4BF701CB76EBA380060044904801F3A
              ACCE8E4F3EE8246EA7FBB18DE421F2E6A188E1CC4BD72F1D77B95C39C5C5C548
              4A4A9A10C771DCE96DD9DB4EAD5BBCEE239F40D7CF040700CCFB9FBD8FEEC11E
              0B80971D5667FBC7E51D44744C8DB449B6376B7B6A7F4CE5F440616121929393
              FDCA5B5B5B919B9B0B9665CFECDCB9F3D4AE9FEDAA50CD9B3BA39E1B078A5E11
              A5C652740F760F0078D939E4BAFD49790711ED132FB57C3EDF174693B1C1DC6F
              DEB777EF5EAC5EBDDAAFBCADAD0D870F1F8657F216EED8BEE3E4D6AD5B2B140A
              45C00C31DDB000308AD4E2E7EB73AC316131AF38875DD73F2EEF5CAD4E5B4A9D
              432E00A36FA500C0E976FEBEAEA6EE358EE3C23332320270870E1D82DBE92E7A
              296AC3E98DF11B2B86BA875E0446473E242F86EFD90210F601C7A440BF47B742
              A640CEFA1CA8C362E613422A0921FE9785523A143A1C57DEF3C189D48DA95B0A
              0A0AC68BC6EE39A7C3591CAB529F4C5F987E8305FB8D1974160030BBCA77FB6D
              F07B0E8A5E115A931639EBF7DB62C3625328A8DFF423F84418FF615A287AC42C
              8D46435C2E17AAABAB71EDDA35B4B7B7139FCF57C410E648BC2A0E322A5BF368
              FB27C98C5AABD56A2C58B0601FC771A5191919A8A9A981C3E118027011C06F29
              A5ED56AB15BDBDBD4F857A2220CBB2484A4A0221E41621E4EB94D21A4AE93900
              7FA2948A56AB15168B0523232341C3013398EA944A2508218B00E829A5DFA694
              7EE1703860B15860B55AE1F57A830A1BCBB47B70EEDCB9888E8E06A514369B0D
              030303703A9DB3827A38FF01696E2A5C0C71FD750000000049454E44AE426082}
          end>
      end
      item
        Name = 'save-all-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000010649444154388DC592B14E02
              411086BFB9EC03486762B5D168614349682E56345A116DB1C6C0034007C147B0
              8607F011483031F6F646298CDB924843626E2CEE4EEF60D1BB58F8373B9BD9F9
              BF9DDD1132EAD63A5477AB11256400CE8E4EB1158BDDB1004189FAC800D88A2D
              4D4E41A65BEBFC4ED664956F721A98426401144DA21CC80084EDBADEDD3C48EB
              F6523DE54C9A6309AFE233DE3EFEA2FF3730D9CDA439DEE8B1B041D8AE7B1F30
              23AF79EE06BE574ECCA3420600EB5FB9ADADD5C70AB7744180EAF3EBE35B2ED9
              EBF7E3D1F9416EE918CC86188593A7FB97A9AAEEA7C9EBD1289DBAAD64F7EE00
              908BE3731A078D3D602A22875E9CA2486C385FCC83C16CF895FA04BA9D53019D
              11C5FB0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000001B549444154388DCD94BF4B23
              4118869FD91D1371178DBAA8491AF14795030F151B0BB5B1B5106B4F03E77F61
              AF8D7FC2DD82ADE0C20956C71D2888A0A828C44222A2A810827A89A0DE662C4C
              E226BA24976B7C61E063E6E5F9DEE11B4650A6C5F105AC062B57BE5FAD24409B
              D1466F6B0F00413D08305603EB33B024017A5B7B88F7C74780D11A83FD02F68A
              09F3EA04466A4C56802217C7170AD7B4F3CB5FCA538B62F5D36B91FF34000128
              14A288D3CA2D12A07BB8536552591E1F5C8666867C79DBDFB60904754CCB5027
              9BA7E23D8F04302D83BF8F2EDC3FD1F1A9C317183002D437D4615A86AFE74DE4
              FFD5C7077ADF21E9649AF5F9755F733A9926126BAF0E188A3456EC1E89B557F4
              BD02A34D9896412695F5359B96810C4A6E2E6E2B030132A92CFBCED1BBEF0BA0
              6F22A642D1A6EA127A35BDF24595EFD993DF7D1BE5F51BD03414EEDDF59F37A7
              3FD6D6D8D9DDADC078959370985D8DA329D44C72EBCC3D3FB82C3184C3619A43
              A16A5836F99F0640E672EEB2A6E9EEC946D2B6BA5A7480ABC32BA28108DCBFD4
              051506E61D9C93706C8044EA180031101E606EF02BBAA64F01CB42883ADF2CA5
              3F0D8036BB1A2FB13C03B6FD761F45892E950000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000253494441544889C595B16E13
              411086BF490ED9149CDD25B265853E0514A4800290434741910851414D0D0F80
              C3334003054820A188923A42888A8822C555203B5612FBDCD917241020FF14E7
              3BAFCFB9C441484CB3BF76E77676BF9D99333256F00A3CBDF904331B02489299
              59561FB7966849F2DC8D018AF3C564EA5136F829ED3A70CD734FED9AA40D4773
              943E6E4D92A501123B09CB8C882469DECC908457F00A7F8745CE98868CB1B86E
              9E8B66162CA94E06D3588FB0B87EDE69B04C20B2A3B1249BA788004A155FE5AA
              4F7B7B9F85E5452D2E2F00E61C75528741482F08EDFC4A8D7E27D2A013E592F4
              24A95CF559BA54D3EEA73D169717B870FBA272BF00D894F582504B2B35B1BD67
              FD83412ECAB9F88AA35B67748223AB473E892329B58C06983BF6A4FFC03C4969
              1A4C6B72B48CB1638AE3BF209AA864803008615336AE9E691D06E1EC88B213BD
              20B45E10BAF599A7670BE072BF7AFFF2D059D30CFAC437F0464C0531DFFEC180
              FEC100929E264DE972C5A75C2DC50F3EE29E546FB2F94425BBD6EF44EC6EEFE1
              74CD69442B3595ABA5D3239224E286C5DDB7F78EC4F572ED05A071EAE6236A00
              0DCFCCECD7F7DF2922C66966CD669366ABC58DD5D5B809A5CDCDD25F621EA267
              9F9FD33DEC3287F4AE1384D60D7A53D9D16AB5D8DADA9A0945D6BA875DDA8336
              9E609DA1DE7CF9D0BCA5A1E2871CE1AAD7EBD4EBF5E4EA4A31E6236A000D3790
              17FD887EFA45FF8EA4D75F3FB6D6CE968B02D8D9DC495207F78DE34C8B04FB86
              44BF13A588122C00DD6FF1689573151E5E7940A9E09FC1EC15B0FE574C607EE3
              FD63DA83F6C4E41F6AA6C773D025EA490000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000001E5494441545885ED973D4BC3
              4018C7FF776D15AC4333283A886412AC8582E0D0A55F40072D080EA273D52FA0
              9BD27E01A58B22EDE622B8894BB10E4505B72ABE80AD2846EA4B23AD454C9338
              686B84E44CDFCCD2FF7677CF3DF7BBE7F2BF24040C8D0D8C82E77856485DCA16
              B2B0B302788E87B7C7AB340B2023662813A02C7FD0A7024022922433DBB36AAD
              0BC60251A2CD0500B4D6648D520BC072003BCB6ABC8B475B87835E1D5C83DA28
              5C7D2E1C6D1CA2B3ABD3547262A7E81DEA05D7CF193AC9FE97D53E8A12EE530F
              95B6782BFEBDB20A150404003C131ECAF50F1B86566CD850ABCDFDE47A7B2E32
              E34DDD03F5E8E9F211B140D43A8076A703FEA04FDB45B48DA6BBA0DDD9C61CB7
              DC862D00CB01745D100B44895EFFBF00942FA43A657A03961F410B80791597BF
              DBCCC81FF429D0397BE9BD543B0000987933B25C93BB139188246B07A857F9D7
              028482A07BD4425E300FB0B8B444C2A1906E9959120A0296F7570CC729BE0BAC
              C84DFBFF60EAAB34AABA73B6776109048DA7E3906469F229FDB27DBA7B0EA5A4
              0F110E855454597E5300A9EC29568FD74A922C4D3DDFE4B652BBE7904B72A3D7
              31546547EE6E371646E66D0E9B639310325D7526CD97B056193143590FE1AF09
              EE2E372606C70110906A8A5DBE2974E6087901EB271B86533F01FB1CA0F44538
              BE410000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000339494441545885ED97CB4F13
              5114877F77A674C632BC1A1EA50D812811021A9BD89A605C10880909C6C7025C
              ABFF00F80768A23B498C2BB7AE5D18352EC49810126C540C58541288A0D45A4A
              01E91BA68599EB02DA506867A60FA00BBEDDCC3D73E6CB9D7BCF3D43A081FEF6
              3E085C9996D082B214F642A725D066B1A1DA503D72D0427B70CEACCEDCD524B8
              43675D4B0DE5CB79882111BED91572AEDF4A0B6112598E607E748E34DA1B2800
              043C4104174300A06D0613985A6B5169A940C013846F7605D69BD642F861E9C7
              12E647E7D0646F00002C004941A6206F38408E05F3A5E805356F128625A3D296
              4C29A590250AAE8C23B1708C72655C3EEFB702A85414EC6FEF83CD6253CC52C5
              5781304C97DBE9C19ACB8F8D9008BD410FC75307841A216B2BB68441FDD97A98
              AD9611009D8A8202579628C28A81D2A684A02784A02794BC17F685B55B515010
              90C465F45F94315B2DAA8F253F715D4B0D35B5D60200A65E4F13FBAD0BD4D864
              D42EA0C0BBFBC3E4D4A52608D5A534B21AC5BC6381C4D737214B321856791B24
              05F9721E95968AE480B1C908D3195341040140A82E4DC91FF145F0FDC53750BA
              7D92A80A1E36B1B0883F9F5D90E21200A0C25C9E36EEC80479418FB6CBA7C196
              B0BB6F93BD71475607D91276AF5C5A8ABE501F0BE6CBB160BE642C33E3CFC6A1
              2FD51FA64B5AD20A26FE0D0A45A3BD81F239763D690513FF06C540D1AFC163C1
              7C516D16C49088A5D995AC13579ACB53DAAB74504A41C8BEFE200575C1700CAE
              2F6E52D76ED2BCB3D77EAF6D4B2A08C6A271F8DD4130EC7E4131246A174CD0F3
              A0476B2886EF0DABC66C0436303D3C03794B568C3BB27E50DA9401A04B2D2E6B
              C189C94978BD5E5CE9EDCDC52B85DBAFEEA8C664BD8B037E3FBC5E6F4E42B9C0
              805280D29485594C246670C837B312FBF5C9054A0B7A0CE78D6ECC35069BF9FC
              5B4EC75F734F785E52999E38D9D198B13E757777A3FB1005999F6B7378FCF109
              625BE27B0A7AD5FDD5B33EEF58289A99D401C0DC8EE460C7C008AFE37BFF4E2D
              BE8947E382C16848AE4DE773A7E6A491E508C412060B69C6B25DEB29DFB1D9D8
              8CC18E01702C7F91103C246AE75026764F7EE60C5D5ACACCBEC79BAB9A71A3ED
              FAF6608E7E1A05F1E8C3906AAAFF2EC716D789819C920000000049454E44AE42
              6082}
          end>
      end
      item
        Name = 'printer'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000000CC49444154388DA592310E83
              300C45BF692416160E8127C64A9672AE4E65A613E7CAC4C8D673446269D2A104
              416920156F8B1C7F5BFF9B30C1CC28CBD221016B6D360C0300407D178D317EAF
              596B4DCB779632718FD3028A99010045512437E5798ED047229264DC12EFBD27
              220226138F8C8BE968AD69F6A0EA7AAABA9E00FC2536C7F8BC5D432345FEEE0B
              00DB8C6318635C1874FA90360200706F9A9F9B3CDA769E1CD8C498B2C12A466B
              6D067C8E4329E5523670CE5DC6715C17991922E25E078888ABEB3AEE416A1281
              372726657EA727BB7B0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000001AF49444154388DBD94316BDB
              5010C77F2FD8C21E8AE3C105093AD8C55EBAD4A620D052B22AFD164A279B184A
              BF4730F5AAAFD0A9D2D8E2C5E373966A7007E3A1D15428B5504A0D791D1CA556
              F2089287FEB77B77F7E3EEBD7B27B8956118F4FBFDD7C017CAE9248AA2D966B3
              01A0A28B984EA7AA0869341A89FB675AE060302851605E470767FE2F60A5D3E9
              ECC84787B195525896C576BBDD015BADD6E73DFF31C070382CC3BC68341A3F85
              D8BD8FB06DFBC6755D659AE6C1150A2188E398300C4505C0755D9E3E7FC1F75F
              7F00B09F3D51C08391784C524AC230FC37361FBFFEE0C3FC4A007C7BFFAAD01C
              EA74073C772CCE1D2B0395AA4E0B04582E974C26935280F1784CAFD7D3039324
              61B15808CFF30AB5ECFBBE48922417ABFD7A9EE77115C7FCBEBED6826AF53A96
              69E2FBFE039F1608100401ABD54A7B97ED765BBD3D3BD3E609DBB66FEE1FCEE7
              F3422D3B8E2394524A64537D5BE1C95ECC4BE002E0531010C7B116649A266F4E
              4F33F39D52EA326356A2289A0154AB55BADD6E2EA95EAB6981C7CDE6BE79B95E
              AF67699AEE80D9A6350CE32E424A89009AF9C49CA49400082148D3944737B66E
              1317D55F0BFF899155DA3ABF0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000200494441544889AD56BD8E9B
              4010FE2642B229DC4322219147400A969CE24E3C8A5D275DDA9CCE8F7025023F
              8A7529EC12F75664AAA4A5A2B2B949C1EEDE780DE4CE775FE3197667BE9DBF5D
              1304822080EFFB77CCFC938808009899BBE49EB5FBAAAA96FBFDDEF874D083C5
              62C142ED938D9E6559A79F0B02E6D67E3E9FDB8E06916519695B890FF6079D01
              52E893A50E80A4ED20C17BA337452CE2ED932DBD33454E100446994C26202230
              33ACEE18EC22E9D8755D489F8EEFFB77FAE4CAF60600F23CD74E8C4FCBBFD105
              C1ED683422CFF39E0F1AC7717311D77B623A9D3EA569DA9C4EA753A3F012F97F
              FBD2346DE2386ECE8AFCB0FDAB63A7EF5F3F3154FBBD058EEC8A87CD1FB3F06D
              F6115053FAC22EB2E59640558D01E0F78F2F66E3957791945B023BA4A2285014
              4567E7F4759196A328421445E729B20976BB1DF23C87F0246B61D7C5E8FAD417
              04760E99990060B3D93C598EBA64A3CF663343A47E5B02590371878188E87038
              E0509683C7FF1C8608C3D07C5393DD5D0366965389B22CB15EAF3194224A9233
              027197B5847D93BCDD6E5FF51EE814D97088E85E33AAB06E99F9E6CA14FD02F0
              08C0DC454E55554BBDEABA2EC6E331415D7857A4E8F1783C2EEBBA7E8E403ED0
              4110C0F33CA8883849122449A21DCB83778100A0AE6B0C3EFA3AB4D56AA5DB0E
              6298600D9AD9AF6D6DF4BE687DFF1286D0F5A2FD03F6F0ABB8A5751860000000
              0049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000124494441545885ED96319284
              2010451B972A121253E32D4E3091E79A688C35DA73116D6868E41DBAE80826D2
              B2565B71C575037F26ADE5E3039F16C0A8280AD05A73E55D2222E8FB7EB126B9
              8FB4D690E7B94F0180881957630106596BC3919F976529D6EA2CD95FE906B801
              6E00C9255EAA140400504A813166364E44208C31C9128F530821082166818488
              D9988447132F8663FA3024E4F57BE0E7C0E7D7F76855F77C780058CDF2A3E21C
              387B3936014E9DF554B325E89E8FE9EC4F07B97C135E0EB0DA116D7533B1B2D6
              B2A7E96EC9361D18F4AAAA5DCBD1D47554886D5E46299660F53222A2C5BE5D29
              0552CA11EC8803DEFB0F229ABDE39CE32D1A9C49E10022666DDB2ED62FDF03D1
              004D5DEF75220AF8FF1FC35469B81B803B1DBF91738EADBD01697E6EBE560366
              2B0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000224494441545885ED983F8ADB
              4014877FA3484EE56041548515ACB14913C8A2140BAE16CF1152E4006ADD6C2E
              12086A55E618319BC65D86F511DCA8512123835023DE36B260BDD278F4CFEB05
              7F2010D21BCD276986F7661824E8BA0EDBB66521AD098200699A563BC81A6B9A
              06CBB2BE02F8D5B558CE7D1886EBC68239230077AEEB52675A007CDF67F9B3A5
              A80802005CD76D257488EFFB4A715AA7BDF6C045B02D17C1B65C04DB72F682BA
              AEEBD0B472CFC160D06BE74404C3302AFB2122E8B66DEFF36D55DAB901002144
              1F8E3793C9048CB1B27BDB388ED76C3C1EC3B2AC2580BB3E0C641011B10A3B00
              0F711CCF8B5CECBA2E759D6F15795184F8BE5FE4EAF39F2465177FAF82E2FCD3
              8701BE7FF97832A143AA0419F24F7F7B357C55C1B3FFC53241961FAF4AE92FFE
              F3E37331B386EFDF9DCEA68452C1DBABE1A93D2A79D363F02C38BAAA134260B1
              58F432593CCF23C771A431CACB4ECFF33A5D17ABBEB4B2E0B137ED0B65C13D51
              14218AA2469D99A609D3346BB5A92D2884C0DFE5B2D198E4F33971CE6BB551AE
              0757AB55A76370369BB13AF5E03DE41575B1BBF55F88C6D5B5E338F8F67C2CFF
              24A2C7AA8A1A00F420081086E1BA2CC2300C4CA7D367D7CCD108E3EBEB4682E6
              E8C53778DC6C36FF9224298DCFB20C7A9AA6951B88658B99BA835CD696318624
              49B0DBED2ADB9CFD24A92DC83907E7BCD30923E3EDE7E23DAA3BA25DA322B805
              F0D093E0F6588054908810C7F11AC0BC33A503B22C93DE7F026C6FAA38C6EE11
              110000000049454E44AE426082}
          end>
      end
      item
        Name = 'command-undo-1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000020649444154388D8D924D4854
              6114869F33F78ECDAF5A3A48894C38983F248154D4C24D449B46AA5D4CAD4234
              305A14E1B64D1105499B885AB4956855A48582162EA44C910ADA642D4C472619
              739A99EBCCBDF7B449B38C66DEE5E13BCF77CEC311362578B68D403CD68E70D4
              9ECF0CA42F8F532A9EDFCDAD04E2B15684119426F199259B3700FE332DF8E3B1
              3DC028CA532D3A7D99FBB365014CFFF14602F1584C905160D4719CDECCC09416
              675365012432D8B51B18073E809E53CB769CC5ECD6970A0A59B5EC7C61368535
              F205CDD94864B0EBA5887496F51DB8AA3A0DDCB457ACC7995BAF3155B51B1803
              66143D4FC17535676F1DB5AA42406A4538093C34AB7D072AAF1CEC9750DF3E7C
              9D0DCD828C01CFDC5CB1E7FBB549B53FADFC4500B3693B81440BDEE69AFD1E91
              7185D346616A09A32EB86C462B8781EBE23562DB0EEF1A2ABEFF869BB6FE9C7F
              D9626D6201B36DC782110954099C32000A53498CBA60CA8C56BE006E88D768F0
              EEAD1DB69E7FFE97059CAF3F081C89025CF4AC1BCEDC9DC19A987F071C03E690
              FFA8B4361C89B1B95E789304483A8BD9C9DCE0C72D2BAC27D4DD8E591FEEF9A5
              A6FC48C44F30D186EFD0CE0E1179A568A2E4C11BF521C2173A429EB037ECA9F5
              D7809C10E807EE69CE7E5212201506DEC6EA4B02571514D519A0DBCDDB8F56EF
              BCA524401DC5495BB7057DA0B0AA6B4EB638BD446E680E3795E727EDA2CC389A
              80E6C00000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000002AF49444154388D95D45B888D
              5114C0F1FFDADFF79DFB3083F170087318D749DED444E4ED28924B2999310F42
              4AE3010F8A5C9E4694924B6E9912A228C9C3C82D22BC285333193494EB0C2373
              C639F37D672F0F66DC9A33E3AC97BD6BEDF56BB5F76A0BFF44BC7606B1742A0C
              6C41B4A1E74A5B6FE65CCBBFC70A86F90BAB994E2C9D0A215C42D889CA3C531A
              FE6F0CC0EDDFC46AA6115D98F2800BA8A441D7014D7ECBE7E2C1C8CAC944D329
              4790B3088B55ED46E064E64C33D95B6F8A03A3E90AE24B261BA051449659B5F5
              AA7AB4FB522BB9EBAF8AC200A4FCFC22039C1291D5AABA55D1FDE42D41EB9782
              45DABF66F3F8CFBF90BBF91AFB35F70B3C26226B55D5075E02880CDE85FE143B
              81FBC009DB9B7FDE7DB185DCD597B856ED53471C4500E531104061517F6A2E30
              5660930AF51272F696AC9ABEC7443D5CE0B0AAF5107340B10E50A7D9209F7BF8
              6EE03B8AB9C88828DE8461A8917241F619915DAA44E24B2BB7BB99C666123555
              0705758D9806550D08BB7536E3DB4C6373C14E657898D8AAA99FC273C7AE3198
              EF22B20DE5B213B475617B7C42B3CA1F00BD22B21918E755965D95B887FFF4D3
              C0622E8FFFF8039484F02696DE16618340990310B475A1199FD0ACD1F700ED43
              935E65E935EDCD0FFAE24173079105E37D13716702739C5F89DFE85DC011A11E
              E441D0997DE13F795F10C4829B4CE05594CE4698E7FE99FBDE37C889DAAA1DAA
              DC00BD6BDF7E2B8CF5853775044012A573C0F90857277192096C578EEC8DF641
              B1487A0289DAAA9888B4A35C1F6284076BCB105D3489D8D24A8C6B0E89B0DEAA
              56BB4357FE8ED8F229882798F2286ED5284C4978A4116910913A556D001E1505
              C6574C1909BA0FC50063806A044F557783EECADE794351201006E62358A003E5
              08CA71455BB34DED749F7E561C98FFD8F31648F5FF37EAE7F15B3E936D6A2778
              F515801F787911D5A3976D1C0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000380494441544889A5955F485E
              7518C73FDFF31E7DFFF8373751D485CA1AAD0941268DD14514A260CB51AD60FD
              13EAA28B88DD74330A1A41D4BAE8224888888617335CBB88362816C25A5A6E52
              910C692E29DB34F7AAAFA6EFDFF39EA78B57DD5CA6F6EE7B739E737EBFF3FDFC
              9E2F87E7887554DCD544B8BD01E01964E72C6B13B13707F07E9D5B6FFB86726E
              7D50D4B587507B3D8897109F82FAE488C0F6F0FF3607706F350FB73520F43CD0
              8DF137F0AA01787E5E80D50E222FDC43B8AD1E8943888F8145A0C3B0A164FF04
              A90B53F977107C62672E73E920E8138CA46FF698C460AA7F82C58F7E06CBCB1F
              37DC5E4FD1937703744AEAC1F00C3B00F66D7C788AF86D9803A8B2773F400770
              12C8028F9BD9D708BC912896CE6E6860CB704BFB78E3315283D7F0AF27D600DA
              10A73042C028F0FDEAA27206D20DB395FA2673032681F386F593B574E2DC044B
              3D9720EEA1EDBD8F9E166ACF3F8435FAC3E0086627BCA925E6DF1A44DB4E7414
              3B724E030F0217C15E04A51108616668F9D837D72BF78685408D824EE0694921
              DFEC03E070E64ACC5471BC1D27E8960A9D91B4CF37BF0F78169F4CBC7794EC4C
              629D83E614A88EA03BC2049BAB70CA83004D487D825DC011C3DE95DBB48DB2D7
              5A50D02D13FA0A68013E037B2EFB573C1B3B3A803F9BDC3818D721B4BF91C8C1
              5D484E9D230D238ACD674FC09F4EE08DCD117AA02625579F831E015A418D4E71
              E117C1966A4B0D4D6209EFBF01BEE18DCEE24513049BAB16108B8E9C03402200
              E04F27C88CC508EEAD492AE09C025A25B521EE748A0ABF0C36575BF28749486E
              0001FCDF17D08E120AEA4AAE200E0325ABA322331265FED81096CECE006D66F6
              8B5017D0ED54457077576C1CD3B2D267C601E6806B888635D3741592F2A240AB
              998D00635B725E962D65960B0C43FF1AD7999128F3EF5DC0D2D969137B0D8EF9
              D371BCD1D92D01420FEF002847D402E3EE7A9B32235162470770EB4BE300E91F
              A7B1CDBE24C0DD59B632EE9F120AFAD8596DFAD61655787F35452FDF4BA0A8A0
              56D230506A58D3BA1D6CA6406D312A0CE05404712A2314ECABA1F0AE0A84764B
              9C042A8137307ECB0B50F2CA7D143496D76116012B003518D6293824296C66DD
              60EF64A7E3E40500C0EC38F010E45256EEFAA799BD6E588F2D665878FFE26D00
              A46F309B4278C0558CEFC0CE9A2995B93CC7E2873F919D5ACA0FE05F4FE085DC
              B785DDF8D9F98677798EE4F9AB642ECD809F5BF90794647E416C343D02000000
              0049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000429494441545885BD96DB6F14
              551CC73FBF99DDD95DDAEDFD4EC8D696602C769BDA5688410DA0606C132EF141
              23F82421349A60A23E69E2BF80461E14132F0D89C8E5C19A00A56F9AD09A7095
              7B286DB194B6A4B7D9D9DD6167E6F8C052525C6DC36EF93E9E9C73BE9F73FD7D
              6191D22261B44878B1DD172D7D319D825BEA09EF6DA90FBE163154D2B19C1BD3
              3903D016EA10EAA823FC4E43AD2E5AAF2EDA5EBD3A3F67E60B0284DAEBC8DFB1
              7A05422FD0A73CF559EAE2BDA703106C7F86BC9D0D35402F8AF34AA977CDFD67
              DDFB7DA33905F0656A3436AC206F47439520BDC035E5796F9BFBCF39F6EF2339
              35CF081058B79CF0FB4DE522720A18544ABD65FEF0D7FDA53087C78E20B06E39
              E1CEE652D1A407B80B6CB7BA2ED9F689A12531079047E635843B5F284E9BC780
              37633F5D8A277E1B583273481F816F6D15F97B9A0B4493E34052A13AACAECB4B
              6E0E20FEC6320A3E7D315FFCFA09811285DA46CA9B71FF8E3DF1A40AE5A19855
              B69B708767B1CF8C91BA30012A0340DEAE46966DACFD52443EC862219920148A
              61A047A10EB813567FECC72BA4FEBC3B1F20B07E05E1DD4D8582F40021603BAE
              8A79E6FD2736D78A0C012906A2C016846D28BA156A4FE2C4AD71EBFB4B73BB21
              90FEF1763614839C024CA554BBF9F5592B9BA7A79587309A2A08BE1EC117295C
              29F02D50076A73E2D4D0D5D8818B40BA183937A65009276934551C01768B265B
              8DD6AA5FDC31CB7187CD270250710767608664EF10DEB43D693C5FD625BA1605
              F9DC575774D0B913B3DCDBE6A36A988648A4213A45A4DD68AD3C9C0DC4DCDC03
              33A4AE4FAAC04B35BF8AAE75086CF0AD2A39943C7E6B7E39766E4CA16C37AE37
              961ED5443E149137FC2D958753A3A6EB65F12A00BC8904EE645205DAAAFE00F6
              49D077D21D3147FE95079CEB5310D42DFFB325C704F9084D36065AAA8E3803D3
              AE3716CF0AC21D9AC51F2D9FD44B438D223428C7EBCE18489C8BF720A8C78C55
              25C704F94474EDD5C0DAEAA3A981692F5B08653B04D754FB4176E1D3F6FD6739
              8E775D21DE7D7304D80044C5D00F167EDCE6F337966505903A3781820B884424
              E40BFD6F20B1BA2E13EFBE791BD888A255FCFA178135355901A8B8833B6DCFC8
              832F209C310F3C0E010C2EEBA85F0FCCBAA3D95D460C0DBD2050A8500A301704
              7808619F1F1F84F4FDC8C63F5A8E68D2886218DB4D2C0A2017C60F15DC540BB0
              15E87186673347B2A592D1528911AD58096C53A857EC33630BC7F25C498F1410
              EE6CD605BE01BA3D2BD5679FBEF37476C068AD24BCA759D7F2FCDF01F50AD566
              FD7C1592EE1202183AFE6819A14DB518D18A95E995D7039BEDFED171FBE4839C
              993380FCF756E37FAEB41A408A02A215068A459379790068B3FB47C7CDAFCECE
              8DCB19805E9587BFAE682E40CC4B444ABDAC62A97EEBD035922707E78DCB1980
              174BE14E2597A7CD3D14B3D84EC21936B1CFDCC53E3D0A49375776B9D33F46F7
              C39A915C04300000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C0864880000057D494441545885CDD75B6C14
              7514C7F1EF999D99DDEE765BE8D5B610C94A45B9D8966A8ACA8B2684808946C2
              83318821102F898235C6A88FBE68628CC6C4C4680C5051130D62E45220CA5D05
              12099552451190626DA177BA7477E7727C60A9208552DD6DFD25FBB0D9FF7FE6
              9333333BE7C0FF3C32AAD58690FBD25D98B30A0D3CF5E3EF1CC639D89E25DAC5
              046E78A521E4AD9A4DB0F6A6224302BB0DC3E830F282C7927BCE649107E6A870
              73CA0B80ED289520BD5995A53332F012AEAE3C1FD80A4C031E54CFDF9DD87E2A
              CBBC918086105D59835D5716051A5166028BD4F7BFE97FE710A9037F8E2F30FC
              6C15C139E511413603B3155D8CA75BC70A775D6064E90C42732685053602758A
              3EE2AB6E1A7877EC70D7044696CE20BC301602BE04E6024B50365C68384AEABB
              B1C30118D7C0D9C07A84FB80650A9F0D341C25D1786A4C71F08F0A46964E27BC
              3066019F23CC4759A1F0717C6D3389C693638E83CB2A98F3D8EDE42C8899C0A7
              883C80F234C29AF8DA6606C70907E90A861E8C91B3301610918F44E461CFF757
              021F3807DA717FEFC79A5E98D193FA7107AF6D001C7FC4B562DF5346FECA5A03
              648D882C51D57DC0870022A37B555F2F8A8292003A80664D799DCEB16E06F7B5
              5E7CF0AE8195D0E24AA28BA7CD1331B632DAE6E1DFC757D5C34083AAFFA1D795
              880F341C1DB6F110FBEE32F256D62222CF8BC81BAABA137802F0325A41558010
              3009B813784844EA54F59CC20BA0EB06B79D24BEB6057CFD1B08105A3085DCC7
              6722C88B22BCA64A03B03C79A0CD1F58D79211A0911FC488DA98B74CC0AE29C5
              8CE5837097206F8AC85C555DADE893C9FD7FBAE7DFFE61685F00C03DDE8BC61D
              ECEAE26F413C11A947A8302BA29B0052DFB7A117DCFFF4F1BB1378ED719C962E
              123B4E936CEAC02C8FB61945396B0045E4398119818AE87A15D46DE9FA1B7825
              B2642F8088D403A566E584CD12B6709ACE65A49297A2DD4992BB5A51CFC79A59
              B45BA0277DCE8039AD6067EAF059B4277965C37A19723760A537149A5327344A
              2480D3D499512480FB73377E6F027B76E94160B2C033186C340A72DA53DFFE71
              75477D09695617EF000D1B62D403F9E6D489DBFCB081FB63169027FA90B08555
              39710FB04290DB0265914F127BCF0CDFF2BBC77B515BB06E2DF85A903C11A9F7
              D1B05539E16B4D7AB8BFF4641CE9B474119C3B292911CB36842714F9C8EF49F4
              5E7326718F742111136BEAC4ED40A12146BD2076F08E921D1A77708F67B8E3F7
              155525585D724A449E034EABE3EFBFEED0E43475221113B3B2A011284DDF9362
              5797ECCA06D2EB88137E20D68F188F0236229F8F38D5394DE72EDD1F5B808AF4
              5F906B5515EFF5FB53B8BF651099F2B16A4B0914E4CC03A6E8A0FBFE55FDE070
              89371CE5C296130A3CA5AA6B04795590A78273CA32874BC76B1B40A10F210A37
              3A76A691801F5E185BA16807B0094F47D835FA18110B8150BAB1B871E010D257
              DFAE2B7B59132E03AB9B330EB4A61500C4805674944080F8BA16E2197A3FFF33
              56553112B1A208B350B6F87DC9AB6792F14C68C11404592488056C717EEDF9FF
              00ED9A128255A506F0BCAA362BBA3F75A863F497381B099447C87DBA1A117956
              4466A9EA62B7EB024E4BD7F85730501E21FFA53A0279C17B81D754F52B55FD62
              70C371F0757C2B18BC6F32B98FCFC40899F703EB8193C0B2D4B12E923B5A817F
              F114FFD718C539D835A584E6DF8C59110D0BF20AF022F013B0C0EB49F49C7FEB
              D050DB9F35A0911F246F55EDD077C90F62442D242F680BD4000F01CB45285265
              35B0CA3B1B8FF7BD7E00ED4B0EEDCB5E056D037B46511DF0295C363409458298
              AAEA009B7D5F5F070E260F7530F05E137A3E75C561B27D897B815DC0C5F14C49
              A074287A04D8A56877AAA593C18D27700E9F1DF600599B83256A93BB64FAB0BF
              F9F114EEEFFD38473AF1BB13D9228C4DFE02439E763BD854C399000000004945
              4E44AE426082}
          end>
      end
      item
        Name = 'command-redo-1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000022549444154388D8D924F4893
              711CC63FDFDFFBEE9D7B7D9D3567FE49AD83211E0AB50C820A2D2B0F29451274
              0C04A30E21857511C20E151D0A4B29A18B456097224C023DD5A115CB0AA1700E
              F312AE692D4A9D4EE7AF832894917BEE9F0F0F0F8F9042721A6FE329DED10C0C
              4CBD7B3E34F9A80DF422002A1581B26C802D08FD4E796DA9FF782B880060A422
              488C8F90BE6D7F9F98560970D99557DC6BACCFFF16FFF4223541F2E704F17010
              BBACF69932CCAD825CB2F28A9F2AB71D130071B9712AEBB14B77A3EC4C0F9029
              207F8B0C5F3E86C76B00F78012A04A4C7F2139276FE0CA2A68005A44A8004969
              1BADF54BC93FD78395BDE91A701AB80EFA89D64C2E4EC7F4F2D2CB516E1BB16C
              05DC01CAB5D6D5A6955D741838A3B5AE4A8C8783B1BE766647076131F9076CFA
              8BC86DEA14D3B2BB80ED40753C14183141CE021DF391D160E4EE29F4DCF4AAAA
              A6BF90DCA64E4CAFBF03A843B36F26F46A387ABF05855001F4C706BAFE090364
              35B46266F8DB411AD01C8887021FA3DD1760611E855E5A5BCFCDACB5D928E883
              F15060E86B770B2C240030D6D534D6001ED3B77160EA6D1FA057917363EF11D3
              0ACC868391C9C757576000D97C25508FF010CDDE9991D783DF7B6FB210FDBC56
              9B95184E65DDB04A737C08B75C59052A63D7B158C6CE238653768844742C91FC
              11F9AF40451F5C24391D3B0F34024795A80FAECC0D5FDC05A5CDCAE3ACD94000
              94E3C3BBE7C4D295D39C7411F102BF267ADAA666C36FFE2BF80D0C1BB78BB790
              356F0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000002F549444154388D95D47F68D4
              751CC7F1E7EBF3FDEE7B3F76677373336A6B5B6BB464AC1FB342246ACA20C919
              56208244880582515ED62893020BC222DA1F21F54FB02866442BD090A8F0C764
              08915829613015BCC41DBBE5ADDDDCEDEEDEFD31B245DBB5FBFCF3F9E3F3E2C1
              8B37BC3FA28C13BB6F23CB1EEB0D809780B7277FF97E3AF5E91E2816AE675C39
              A017AF017810E935C4E795ED6B82DACD7BC179FF64CA01E50754DED53D222929
              94003A2AEA9ABEA8A86B2A66CF1C05B3F2C07CFA123357CE136DEF3A051A9553
              02B8A3A2AE79D0ABA92F4E9D39521E0830337A81FC589248FB433F808D3BB984
              416BE8C6962FBD68DCF477D08563C43A1F2174EB3DB8481C955281A0BE0D0511
              845E90B4CFCC3E06B6FA009577AFA37A43022F126F05B601AB11CB014F256833
              9BBDA120E94933CBF9B1953DD43CFE0A427B10AF621481618C6120FFBF55C1C7
              6C139299D969BFFAD15D487A53D2CB66D66FF02266A9DCE57314AEA62866AFCE
              AB446E5F8517AB7666D62FE77C334B00EFFBAE225829A9D7CC3EB0A26D9FFA6D
              98F457EF924F5F5AB0D292D59B8875AE77601F39E7369B59AF99F5658606F041
              CF9AD91F66B62B7BF628A94F7683154A62D53D3B85EC43A12D66B6DBE09DCC89
              038C1FEAC3211E000E5B3E373936B8AF2416EDE86669CF4E01FB85B69AD9EBC0
              5B99A101C60FBE07CCAEDE3220397DF134C5C974E9E9D73622E896F4B4616F00
              7BE762003EC618E2A6A0BE0DF90196CF2D08E6531700BE31B32EE05866E800E9
              39D82C08C731D6B9F09268D5C3DBB3E307FB1604B33F7DCB2810D4351D2B4C8C
              317172F03F19EF86B5DB9272DA21581A6A68FFDA45AB98BEF83314E66F3A7365
              846B233F924BFE3AEFBB57B5E6A9A4E4A2929E076E0935AC38115FF5C454E8E6
              36420D2B08377690FBFD5CC951FC0B4422DCD2F91D20891DA0E79C1F7405CB9B
              D7861B3B36445A3A8F4F9C1C9C2A5EFB7371E0F4F95380116EBAF388C97D2628
              02AD401BA259A83F333430B158F0FAA6FAB58DC4EFDF48E4B67B5128C2DC4FE1
              F2FE672864528B02FF02ACCF225CB468FFA50000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000003A4494441544889A59561485D
              6518C77FFF73EEF55CEF756D2EA7B43535B362B4AD8D5C200D466BD256EB8B68
              128DD5860CA2F62182A03E8C3EE9872056838445B4C51A48590912451BC61AD2
              56429BA25894CE9CD5E6EDCE5DAF7ABDDEF3F4E16A6EE174DEFE1F5ECEFBF27F
              FFBFE779389C23B290BBBC8835AF9CC40945EE466CC3F878BCF72C574EBC0EE9
              D44D5E272B40241F7911804F41C710F5B9EB1EA5704F03B8C1FF0FB0B92A0F62
              16079A84F6E6AEDB4AE1730DE006E68AC906E02762909E2254BE65587006A845
              D408FD1A2828EECAB9EB3EC6BBBF05F3B3030024072E403A45E8DE8A21A043A8
              0EA806FA02AB8A7BDCE5AB99E839933D6016A2602E5EE98641E09CA46724D500
              5DDE9AF23E377C07BAF18293B792C886ED78C5EB714279FF9E8B052407EFFE47
              905C2455213EC770815AA02D73D70D905F75806595353839B939488F015B11AB
              314252C66666CC3EDFB8B7CC66F6B80228074D82554B5E98C217DE2654FA1092
              9E051A80E2EC07372733FB2A7067F51B844A3749B2C3920E1A4C62F61158ABC1
              6F429340A6D25B766060606639923E98E9E22C5017886C7C1C89D7402F9B591F
              46AD61DDFE449C89BE0E5223BF938EFD81A5A7E7ADD20945C87FF2208E170E0A
              9D90B4C5CC3ACCEC69337F2C00940287801133ABC26C287EFE0BAE7DDD843F11
              5F7004F2C214ED3F8CBCB00B1C47D498D90FC053985D8FB6341290744052AEEF
              FBAF0243D7DA8F31FACDD145E79B097F8750C90617E943CCEA109D183BCDD2A3
              D19646129D6D384095614949CDC9CB3F337AEAFDC5C3737229DA7738130E4705
              7B809F307699F9B1684B23639D6D991122DD83316C66B17847F38DAFDB2D1528
              28C12BD908D024699F4137B0D3CC8F465B1A19FBB16DCE8B21C8AC7E32B168F8
              7FF48B615D185566FED5684BC34DE1990EB07EA435A0157915BB6F2B757AE412
              C9C18B18F6165069F857E60B077057ECA82F93B40DE80F161477A6AE0E90FAAB
              7F61427A9A44D769A6637F32DE7B3615EFF884F1EEF679AD2A69FCBE4CA21BB8
              8ED9C37E6AEAF2DF5F1E61ECFC67E0FBB7D5D1427257ECA88FCD7C4E7623EDC2
              0DB4871FA81C09AFDF8E138AE0E4E5E30443B8CB56928E47970C50D181F70895
              6D76848E487AD1B0097C4E225A0DEB174A491AF79389A14B87B62F19E08C34BF
              C97474C837EC25C39EC71841EC075A852E02BD66766CC9C933722D992071E114
              C182B50457955C146A02CE0183C000D023E9BCA553DF8DB61F5F32E0A67F49CE
              DA07C9DBFC045EE9269CA037E31002FCA94986DFDDBB64C03FB2A9818B7C5B64
              5E0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000482494441545885C597596C54
              5518C77FDFBD77F669A18BB4B56524529696B660580A2ADAC54AA258C4206FE0
              42C50483316820F1814445C38B228D122309C6BA44118A5608553106455A08D8
              80E962292074003B74DF66A69DB9C787224228AD745AFC3F9E7BBEFC7E3927F9
              CE777546397AEC448C98244C7F2798E6F0FB47136EBB7B36096BB64747CF5B3A
              C512EFF1F5561F04D49035DA680A58E252D00DE3294DD77E72CF2A9819BF7C23
              C8D088513D01658670CD7AF8A866B17980CDD6A4D4FD465CF2E5DE9A83B747C0
              EC6E23D0701C57567EB918D65460933529759F1E9BDCECAFF985C1AE63540500
              C29D97F1371CC39599BF4F0CEB0C84D7AC89A965BA3BAED55FF7EBD80BFC2311
              F0D6E0CACAFF56746396886CB4A5A47D2336677BE0D491B1170008B75D24E8AD
              55CEACBC32D18C7908AFDA3D997BC4EAECB85662CC040042AD17087A6B952B33
              FF6BD1F5FB81F5F6BB324BC5EAE80A9C3A3AF60200A1960BF4796B4D5766DE1E
              D1F43C609DCD93B90BDDD21D3C7D0CB959A135251D67FA426C9E0C34BBDB86C8
              7889A06F183177228E689B085F006E50792D7BB7FA6E10B0A6A4115BF832B689
              3332446435B00861B20CD7516E314AA9D280B7769971EDA23B7B29B185EBA234
              DDF2AE202B14AA1C780B459589D9A27A3B4C150E8D08A8D9DD88C56E053E0526
              807A31D858CD5501D7BC25C42DDD902CF01D1054A8B9FDED4D27BA2A4BF1D71D
              A2BFE90CA8A1FBFACD624D4927B1A8D8102B5F02492872BAABCA2FB695BD3320
              6049984CDC63EB5C02FB813F156A59E7CF9F07DA7FF810D51F1C11F45F781A09
              45C5BA66777F86E21E20A7BBAADCDBBCF30D50E68040CCE2B56816DB6600857A
              B2A56C4BA0FBF0CE88C057E1AB8A75DDEE2E01B281DCEEAAF2F3CD3B5F0735F0
              541B46BC07C794EC49C06A20A7AB62977F34E096A4694C7876ABA63BA276000B
              9552B93D27BE3F7B2D1CC070A6DD87204F0347423D1D156DFBB7450C37E23D24
              3CF3B6188EE8EDC0434AA9DC9EFA8AD3CD5F6DBA0E0E605893A7031400BB7B8E
              ED45F5F92386273EBF4D8CA8F80F8047805C7F7D657D73C90608F7DFB85FB339
              01A60327FD7F1C8E0C1E9742E2EAF731A2E2DF43781C455E6F7D459DEF93F583
              C20134101089524A75F4379F8F48C03E790E46F41D6B80E5280AFCF595D5BE92
              0D101A1C7E450050AA4B44C61931891109845A1A51A6F911F080BFBEF26453C9
              7A08F50D59635CB9F33A20CB3E75FE81E0B9DF472C10387D9C4BDB8AFCBA3BB6
              D6DF7074583880D6F75703C0016049D49C42D02D231600E8F3D6E0AF3BF49FE0
              00BA19E8212AFB8973226CD6ECCE1F15CA1B3CF35B4412B712DDEC6EC53E7541
              BB65FC84449017EC93667EDCE73B170AF9CEDE1E01807EDF59DCB31F3D28A215
              89688B5C1939A52AE80F051BAB6F8F40B8C3870A87FBEDA973CB04D68A682B1D
              53E71F71A43FD8A4427D03DF237C946E96EB06927105CF313E7F55B4C0161159
              A1942A07762BA80A77B5B4983D6D26E110E19EF6A6A61D2F0DFFE377AB02008E
              690B88297C054B6C728608834E44A1CECBC98D6F2EBE34260200683A8EE9F7E2
              4C7F109B67C60D3361B8ABB5E962F1CA513981FF3D7F038509BC328A6C643900
              00000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000005CD494441545885CDD86B7094
              6715C0F1FF792F7B4D20906C9216C96582D54A9348476A63C7114D412E5ADAA1
              56DB6AABD112418102D5564766C0D80FF2013AADA4531861DAA9A340653A75DA
              CA94183B825053269706622D974DA44036096968F6CDEE66DF3D7E48406E85E8
              EC929E4FEF9733E737679EF7799EF3C0C73C24D305ACBC2242DFFF0D6676AEE1
              7ED8978A6CFA2EEEE09931E71B19B40110A89C8D9D13BADD32CD366F4E7E5961
              EDB3985993C79C9F71A000229CC5200FA1C10E159714D6D68F19997160B46D0F
              EEE099C3C01C8420D060874AA6162CAEC7180332E36B10C00E1553B8B81E333B
              7706B007E803662522E193A7372F2575953599F10E020CF774727AF352DCC133
              CDC05C2004ECB143C505F98F6CC2F04F1C5FE079E4734B709DC126559D0F4C11
              913DDEFC9250FEC3EB118FFF8A79E6F50202A49C0162E11682E5D5270CCBB30F
              E147822CB0720A77FA4A670C45DB1AC04D8E1F10C01DE8267EBC856045759798
              F65BC0328479564EE10E5FE98CD8A5C8EB0E3C878C1D6F2658517D5C6CFB6D60
              1930DB1C41C62F448E0BF03C32DC42B0A2FAA898760BB05CE02BE6C4821D9ED2
              CA84D3D600A9E4D8B7192B548499950B0292C6DDC95B52C9A4D98B51586888EC
              50D8AF299D3FD4B1D789FCEE89AB57F24DFB3C59B7CEC37FF31D98FEEC094039
              702342209D48553DF7B95044EE56D506E0EB670FEC8A5DB18A7DC33426DFFD53
              7C45E51E411E44F89E88DC0E5869535D1B5DEB1C7D7BCB650583B7CE27F79EC7
              11DB3357904D2252AAAAEDAAFA24F016D0054445D2D9410005A813916FABEA33
              AABA6538D2797147B2AA16917BD76388481D223F43F5B0A6F4AB8ABE911C88E0
              B437120FB7E23A1FE0F69F4E0B2E70CB9799BC6039C0BA51DCB3282B9CF646FA
              FFB4E1BF406FD9E7C8FDDA4A446483883CAAAA9B55599EEC3F99E87FBD1EE750
              23A4DCB4A02EC44D9AB714E01722B24655B780FE38DADE48CFEFD740CA1D018A
              C74FDEBD6B10D3AA19C56D546575F4F09BF46D5F8B2686D20A3B870B3D508718
              D61322B24E55B7013F8CBED3A8E77030BAE8B3AB16614DCABF11D8A8AA6F28FA
              D860F36EFA76D681A6B76B00FE5B6611BABF0E31ACD522F2A4AABE083C126DFF
              CB45B8F3C00955F721C8CF011BA84D9CE8D0BE973283F37D661679F7D721A6B5
              4244D6A734F507941AA7BD3175290EC0F24C9D8E95931F041E065E543719EEDD
              F9ABB4AF37006F4905F9DF5C8B88B514910DAAFA12CA43CEB166B767FBDA2BD6
              B47C259500734424984AE90BD18EBD0C771FCB00AE92829AA7303CBEC5883C8D
              EACB280FC6C22D6EEFF3AB2199B8629E65878A01AA54D5013D103DF85A067015
              14D46CC4F0F86B44A45ED15715FD56BCB33519D9B6EAAA3FA185690294014780
              64EC68535A719EA9D3473B17784890E7547537F08D78B86DB87BEBCA6BEE1086
              8C9CFED9C0803B1049FB96E2BFA90AC3135820C26F156D0016C5C2AD89EEAD8F
              8EA9D6C8955F89037EC39F9D561C00A924C05E559E01EE89855B6363C58D0047
              0EC22E9052F1F8B16F989656DFD97DDB71DEDD3F307CE6D42AE7DDFD43FF0B0E
              C01A7D863828C212904F072B66FFF3835347D206D4C410916D2BFFEF7C23D6D5
              0EB05BC105EECB9E79D7474E58E31146EC4813A9E1F8FBA8FE1958620473B272
              EEFCC178BBCE87899BC4CA2FC35358F69E61C80A20E02D2ADF9DE83E4632121E
              6FDFC8D034DCD349F66D0B4F8A614C1661194887FF535F383CDCDB35EE481320
              15ED07DB8BAFE4B38DC097109689691F0B9657B71BDE00F1CE772E1BA8AF2B10
              207EBC196F71B96BE54ED925708721B20A98E22D2AFF7BD6CC8543466002AE33
              70D5879E4CC445838578FC84BEF36BFC9FBCCD165887B01A25063C0FEC52F887
              C61DE7D22BFFC05F5F60E85F07320F04C030C99953CBC42F3E00A679B3208F03
              F78A484021856A2FE05C3034FDA467C72FFF3878F0D58C002F7F59502576A409
              E75023E29BD06BE77DE26531ADA7817D40077002E17D41C24018F89B73E8CD7F
              274EBD9711E0356747F1F8F14D9B89B768FA47BE2C7CD8F40AF1706B46801FFB
              F80F396473F282DA75610000000049454E44AE426082}
          end>
      end
      item
        Name = 'clipboard-cut'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000002B249444154388D75914B6854
              7714C67FE7DE9B3899C95C136F9A514862C6CC085285224A176237A5508A2E4A
              C185A015DCF8086844B0D0852E8AE003D245C54004C14DA5F6015DB534B89141
              69172A2A8D0C661CC68866728DF3F04EC6DCFF3D2E1AA331F65B1D0EDFF9C1F7
              1D9B7965B3593299CC5ED7756FFABECFFB34303040369BDDD3D1D171AB5C2E03
              60BDE3194E26939BFAFAFA961CA752293CCFDB007CFFF67E01104511AA7A1CF8
              39954A75799EB7604A2412F4F6F626819F80EFA2285A0A28954A8461380C5C07
              7E4CA7D3763C1EC7711C32990C96658D02E3C698B3C562710160BF1E8C310441
              80E7797F88C82111E9735D77CC755D1289C420F0A5AA7E512C169B954A652900
              A0D96CA2AA73AEEB8E89C888E338F9582CD60E5C04B6F9BEFF7072727251378B
              0000F57A9D783CFEACADADEDDFF9C36BC05810047FE6F379547591FFDD2F0050
              2E9751D5AB400D38A1AABF4D4D4DF17679FF0B701C87FEFE7E44E40250001E8A
              C8C59E9E1E62B1D8128000B474A771B7EEC46EEF246EEA744D5C1DB2C2D9A3C0
              26559D1591BFD5691D9D59FDC9E96A6B275150A57AE3575E96EE212DDD69561D
              BCD06A2D4B9C0276834A5B2197687D7C67DB8BD2FDBF66671BAC58BBF1E370E5
              87D782355BEA884428972233F7CDD3D1C197E27DF52DC9CDDB8781B4A2FB8048
              907362E6CAC1C4CD0361CD27F9D167E7B11C4F5507015B44468007C1FDDC11E9
              FEFA0CF1755BA75575FDB3DFCF3E09679ED0BDFB4CA7585631AC94979BDA34CB
              7AD6555475B5FFCBC999A851E5835DA7568A70B7F968BCCB99AF4100C2AA8F79
              F11CD4BCF741A63E43D4A84064C0B20145BC1D27486EFCFC4D04134662B7FC00
              F88DC2EDFDA63E4DFB864F4780151A99FF2258F679A0D0C8FF3364D56E5C414D
              780C2808724FEC9671E0A9AA0ED57297A9E5AEA091390C4C89658F8B65DF0126
              143D56CD5DE6156A53268C3B049DCB0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000003CB49444154388D7D945F6895
              7518C73FCFFB9E73B6B3E9D9697F8EEB381ADB294E9AC5DA56374D0453D49560
              99944E84A02E0205A1CB502A2FBA3088A02E86D182A4B259510A251383AC4036
              5D94FB83DB6CAECD6DA7FDF39CBDE7EC3DE77DDFA70B8F4337D773F583E7FBFD
              C0F3E7F708F9080402D4D5D53D05D8994CE6CF9E9E1E3CCF63A588C7E38442A1
              C74424D8D7D7D7954AA5003096E88A80D3C160301C8BC5568445A3514A4A4A56
              89483B507C776E29F01A10063E0F87C3128D4697C142A1106BD7AE0568052279
              CF72A0E338D8B63DAEAA2DC0761139128D460985428B62BFDF4F2C164344DE00
              5E060EB8AE3B6EDBF6A2C6BCF350552CCBA2BCBCFC3AE088C83B4057381C1E98
              9999C1755DE2F138C160B00138A5AAC755B575787898F9F9F945A02C2D291289
              505D5D2D22F20DB049559FB62C6BC8B22C22914858442E037FABEAB64422E1DE
              B871E31EFF3220406D6D2D6565652111B9042CA8EA33404644BE031A817ACBB2
              12BDBDBDA8EA3DDEA5430160787898743A9D047603B522D20A34013B54F515C7
              7112030303CB602B023DCF63707010C7717A81D7807D2252076C017E1D1A1A22
              9BCDDECF7A7F2040369BBD631A051CE038A08EE3904EA757B2DDBF8700353535
              545454AC01BA8071E016B01E684C2693E3FDFDFDFF0F34569552585B8FE12F20
              682A0F9929D374ED0E6003D0A8AA6911E90446D50C6CBEA9A1DC5C0EF05C1686
              AEE026FF05C00750F4C416CAF71CC1F017AC03B6899B3572DDA7E2E648E74635
              CC662F971DC9E57214048BF708DE2FD9CAF5AD81FABD572BFC410F38E7794EDF
              EC8F1F93BAF825E27FF011A207DB10D377149123A8F6039EC086C0D81F3F9BA9
              C496A9EE0B649233546DDE8B5B5C7E26177DBC59E12A6088C8A39EEABBA0C726
              3F7B13DFAA8666C43077006FA1FA82A2675140D86A573DF93DAEFB225ED1B7BE
              D99B641EDEB41BC37C164FB72BDA91EFD64E81D32AD2B9BAF1F99F0C23B81A44
              5A8076553D3BD57E8CC94F0EE1D9990E544F6218FBFD6B6A0954C6408CFDA89E
              54CFE998FAEA6D26DB0EE3E5EC3340BB408B5914C690DB732905C69CD43456F7
              391686BAB047FB00C680D2BB76A114187566C7B1FE3ACFC2B54B6427AFDFD629
              6500BEFCAE77012FF942E547D7BCFA81EDA4A6288CD5FB815DC0792F9B41B319
              804E6097AFACEABDC881F773DE428AC2AA7505C0CE7C85C8EA8DFB287DEE5099
              2097817F800F010F3808C4818664E79909676E8207B6BE5E297005E8073EE2F6
              C7380C54A9D290FCFDEB6963BEF3079CE9B16960631E78026803A654B5C95BB0
              266E5DF894D46FA770E71213F943310DB4219C0046146DF2D273D3C98B5FF01F
              71BEB27629F292A90000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000004F649444154488985955D6C1C
              F515C57F677676BC5E4FBC5E9BC85ECBDB38649D384E2822E52B01551002A208
              8544F9100409A254C5AA1A9040BC55BC547D40F000B412082954456A2B543E02
              4429AA4A005135904489888D136CE7CBB8368B705833F6EE26FB31B70FBBA480
              6373DEFE33E79E33F7DEA3FF88EF209D4E934AA5B601FF2C954AC1D0D010E572
              99852089DEDE5E7CDFF725DD99CBE55E1B1D1DBDF4DEB94C4D1FB0C7F33C6532
              19242D68904EA7F17D1F49CFD76BBF87CB1914802DC0E3BEEF934EA7E7156F6D
              6DA5BDBD1D49BF02EE078A3F6A6066CF0303C0EF810DEDEDEDB4B5B5CD118FC5
              6274777703AC019E054E002F2C6870FEFC792A954ADECCB601B392FE22E927DD
              DDDDC4E3F1FF17390ECB962D2312892425BD025480ED66363B353535BF41A150
              607C7C1CE09499ED04DA80BF3B8ED390C964884422002C59B284783C8EA43DC0
              95C0AFCDEC64369B2597CB7DCF20F2C3960A8502AEEBE2FBFE88A408709FA48E
              4824B2AFB1B111D775E9ECEC44D263C023F5B13C353333C3D9B367E78C728E01
              401004241209A2D1E8BF255D2B69BB994DC662B1632D2D2D0037497A19386A66
              3BCAE572756464846AB53A476BDE0C46A351FAFAFAF03C2F29E90890026E31B3
              73F57393995D0B8C0D0F0F1304C165752E175300CAE53267CE9C01C8015B8110
              7815680666CCEC41606C6262625E71986744DFA2542A1186218944220B4C020F
              48EA33B38DC0F1E9E969C6C6C6169298BF834B04C7C1CC007AEA8F6E93F43840
              18863F56BE7007894482A54B9722E91EE08FC010300CDC2BE9443C1E3F51A954
              C8E7F3F36AE88747798D00785E94DECC953478D11EE0106066B61698917488DA
              2ED686C6C9E153A799CDD76E092B5F000BE71A34FDEC6E5A6EDD89DBD605329C
              5291A6A17D4D8D670FFEC711ABCD6CAB99BD59A7AF937400E9F485CEABD7CD5E
              B529A8C69380A8E4BE60E6C85B04EFBF0C66B51D2436FC92C5DB7E4BF48AAEB5
              12FB85BEB186787061D9CF072A8B3357551B9A9FAC3634BF190401939393840D
              8B0E561A938F95AFC8AC2CACB8E378186F0D244D0BF6BBC98E1B9377F4D3BAFD
              099070A31D195AD6EF02B405E9AF987D09FC1923AC34A7364EDFFC1B22B9F153
              2A17F9F2FD57B8509C20B6AE1F226E3E4C746228045EA2F6B19B24E703B370C7
              A26B7EF14661E03D9458BF9396DBFB939246A9DD88779BD9B7C16E92B4577083
              85B6223F78205B9A1AA365FDAE0E49C3C02133DB0CE401243503FB112B0D7AF2
              C7DEC9394E6C1192EE0292608F86955230F5EAEFC8BED84F580CF2060F1BF838
              BA27DAD183975A8EA44D806FC6C3562DE7BFFADB1364F7ECA65AF826001EC548
              0AEE72623E0EB53F561AC08C4F0B431F923FF60E17CF0D307DE04F60368C54C1
              48A34BB1482395C186678FECA330F82E174F1F25F8E875CC6CB0DE7D17808301
              F0DF5AA6B4AA71F98D78E955B8C94E9AD76E056985208AEA9C1AC6059ED0F2A6
              9F6EC0EB5A85DBDA89BFE64E24ADAEE5B5C657E2F6876859BFAB5562E4BB3BB0
              5211798D4D92F6023798D98AE26707B3E5A9CF69BEF9DE547D071F99B1996AB9
              6080E3BACDA0FDC04A33EBC90FBC9B738AA38741F635D08FB81E1894F49CD310
              7F46D200700BD86E205B18F998C2A9C3005F00BB815B2506E5469F71DCE873A0
              41E03AA01FC815470E13A94E67715BBBF0523D27C10E08F5021B116B80018C87
              0CDE2E4D7CC6D76F3D4DF5AB31BC5486E8E2EEE3820F9156D5F95703470DDB85
              F1AF8BE73E21F78F3FF03F4393F6FE330C08D80000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000551494441545885B5965B6C14
              7514C67F67666767DBDD76CBDAB51776BB5B88240854848A1A7DC068E22D3118
              121F1035A2D18AC84550628C98A8D107311850108809262289C1788946B91863
              7C4109374D4D2CE5B240972DB48BECADDDEECCFC7DA045DAD2ED96EA79988799
              EFFBCE97EF7FCECCE814A9BABA3A6A6A6A48A55228A58A418795883069D2247C
              3E1FA9546A449C564CC4E7F3110804E685C3E13135874BE6ABABAB1FACACAC2C
              8A2B6A404410910DC160B0B2A2A2A2E4E6A669525757570E6C180D5BD4407FEC
              3522F276341A45444A32108944D0757D0D10FA2F0CAC075ACACACA6E9F3871E2
              A8CD038100555555D38115C0078EE35CBB81783C8E52EA75E014B0A5B6B6D628
              2B2B1B11AFEB3A0D0D0D026C02124AA935F178FCDA0DE472391289445629B518
              B851D3B4D5D16874447C2814C2ED763F252277004B93C964E6E2C58B450DE845
              9F02994C864020D06E18C614E019B7DBBDD3B2ACEE6C363B08E7F57A8946A341
              11F90AD86559D61B478F1E655C4700E0380EB1580CA5D48B4056443E0A87C362
              18C6205CFF90AE050CA5D4D23367CE50281446931F3D01807C3E8FC7E3C99697
              979F1791654087699A0793C92400B5B5B50483C1BB80B5C0EA4C26B327168B95
              224D697B05B85C2E66CC988161187B819B9552D3DADBDB13994C86A6A62653D7
              F5C340DA719CDB5B5B5BED9E9E9E9274473D8281B22C8BD3A74F03B4001E1179
              3F1289D0D8D888AEEBAB81C94AA996CECECE929B4389473050B95C0E9FCF9734
              4DD3D134ED055DD70F98A6991391CF818DF97C7EDBB163C7C6F4DD28F90806CA
              344DA64D9B66B85CAEFD409552EA2611D9A6947AACADAD6DD4B51B5A634A00C0
              B66D00C7EFF71F04968B885729B5A8BBBBBB2F91488C55AEF419B8B22CCB4229
              35C07D1EB8A5B7B7F75AA4C66EC0300CC2E1B021225B802470524436D7D7D7BB
              3C1ECFFF6F201289E072B95E1291E9C02AA045449A344D5B59EC353D528D6906
              AAAAAA08854237003B809F6DDB5E5528144EE8BADE082C76BBDD9F170A85642E
              972B59B3E404745D271289006C0614F05C3C1EE7F8F1E328A556016911D9D4D0
              D0C0D0D774B1725DEDA6181EBCB31EC03379369A610250DED385FBF42F8BC4B1
              E62AA55ECD66B3ED894402A5145D5D5DDDC160F04511F95433DC4F841F5AF249
              C677E9DF41D905F2B1DF49EFFF06D59B1DDE6B98A3EA066A9E780F57303C4F60
              39D00C88388503950776DC68761C8A2B47CDFEB3F58FC2C01751D77566DC3413
              C3F47CDF573BBD39D5FC68ABD2DDCDFDFA0780F556BA7BE7B94F5EA2EFCC9F23
              1B10C3A47ED9A718D5E1B5C0B35CFA1BDADD1FF93DC072E3C2A96FEC7C6EE1C9
              8F57E2A4BB2FF342CF7E885911D864F9EB175E8D076CB373179776AC5B709907
              4386B062CE3C7CB3EE9F0FBCA95073FBCEB66FEFDEF9562CFDDBD731D784BA9F
              8C40FD774EB9FF1DE5AB3EA5E08FDEA3BF5DE679E73C3CDFF154BC36120FE17D
              CDF00CE20D33E09FFB38EEEB1BB702DBFB3ADA769CDDF834D6F918F6DF09B287
              7EC01D9ADA69548735E049CD5DB635FDEB97E3E2C1902D70F96B406806765FF8
              6123587DFF3E540EC96FD701EC1291999AB7CA355EDE300320A05000EA4A9101
              2DDB42A94B20FA2FE3E30D3160A7CE01EC07EE9F706F0BE883F779C27D8B11E1
              3EA538E8E452F67879306406747F90B229B75D00DE75F96BF79A93667714BA62
              68A68FEB1E7E195FD3DDB3802DC02BD9237B5A7BDAF68D8BD79FDD15417ABCD4
              AFF80CC35FF336B00CD8C8E0755A026C76F2D9951DEB1660FFDD392EDEB004B0
              0AE44F1CA17CEA9D3F6A66F961601EC2328447FACDBE6CF7A4379CDFFE2A85F8
              5FE3E70D4DE0CA242A6F9D8F67CAAD681E2F00AAAF87DE134748EFFB023B75FE
              6AB46BE2FD03F7107FB738AEFB300000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000008A6494441545885AD987B7094
              E515C67FCF7E7BC9662F61B3BB81901B414CBD21D6225ED07AA197B1ED60D1D2
              5A7BF502322A15956AE950AD62ADD516AA850A8E565BB51D74462B1DC7CBB415
              B1C0A823280A85404902844012423624D9DD6C764FFF48700889B8099C999DD9
              F9E69CE73CDF79CF39EF778E431E525E5E4E717131A9548ADEDEDE7C4C3E55DC
              6E37555555442211DADBDB3F53DF950F68341A251E8F4FACAEAE3E2E72001515
              15C462B15362B1585EFA79110490B422180CBA4A4A4A464C2E180C128BC590F4
              98A4134EF03C4937959595E1F178864D4E12E3C68D03F891A48BF3B5CB9BA099
              F500F7BBDDEEF2AAAAAA61131C33660C7EBF3F2AE9A17EAC134B10F8ADA4B0A4
              A5914884482492B7A1CFE7A3ACAC0C49BF911407169B595EB64E3E4A2E978B70
              38BC0E9829E90260733018DCD2DADA4A3E8E264C9880DFEFBF08F83D50676657
              B7B4B4F49EB02A6E6A6AA2BBBB3B05CCB13E468F78BDDE51E5E5E59F695B5C5C
              4C51519107784C7D957173269349EEDEBD3B1FD7F91F717D7D3DB95C6E35F094
              A452490F969494100C063FD5C6711C2A2B2B01E64B3ACDCC569AD9EBBB76ED22
              9BCDE6E537AF2306C86432388E4330187C1BF8317011F0662814DAD5D2D232A4
              4D454505E170B85AD2DF804E339B9E4824BAF6ECD993AFDB6115098D8D8DA4D3
              E983C06D925C9296171414F84A4B4B07E9060201E2F1389296492A0416E472B9
              FD0D0D0DC371997F0401CC8C542A45341AFD1838A7BF6072814060F5C1830707
              5C83353535F87CBE99921698D93A33BBA5B1B131AFEBED48195604011289046D
              6D6D98D9CD66D609DCE572B94EED6FC2008C1E3D9A402010069698598F99CD49
              2693D6D4D4345C77C32708D0D0D040369B6D00EE96E493F4782814523C1EC7E3
              F1D05FDDF74B1A0B2C063EAEAFAF1F89ABE11DF161C9E5726432192291C87BC0
              D7249D07EC0B8542EF078341FC7EFF6449CBCDACFE70CF6B6E6E1E11C1114510
              A0B5B5958E8E8EAC99CD36B30CF0A0E33863C3E1B0032CEFC71E56CF3BA104E1
              93DEF881992D915424E951200B2446D2F38692FCBE798E21A5A5A5545454F881
              4D924E32B31966B615684F2412FB6B6B6B8F0BFFB822087DD7602A954A0277F4
              3FFA03D094C96486DDF38692E326E8F57A71BBDD009703482A97F480C7E339FC
              FCB8E4B809565757E338CE8592669959A799ED953447D2F9276244382E82D168
              947038EC95B4A21FEB1EE096FEFF2BFC7EBF67A86B703832A23E087DD3594D4D
              0D8EE32C94F42D33DB6066B380FF0213FB3FEB7B42A1D0DB070E1C1871258FB8
              8AABABAB89C7E3A7021B00C7CCCE4BA7D31BBABABA8846A36381CD80CFCCCE3A
              74E850EDD6AD5B47E46744471C0E8789C562025648F2D157B91BEAEAEA686868
              2093C9EC0516482A90B422140A29DF31F36839660455102430F132BC15A723A7
              BF22CD2849D4124C34CC7689E566D6606667B4B6B676D5D5D5017DB9397EFC78
              495A2369AA19B39281D14FEE8D9E0987C74D337A1AB7D2F5D1BFC975B60D9F60
              60D257895C311FA7305425F83E301908020DDE96EDFF19B5F6B1C5B26C91994D
              CF6432AF6CDAB469409ED5D4D450346AD46982F77139DDED53AEBBB567ECC48B
              812AA09BBED478369749EF6CFFE71374BCF5CC903C862C92E0946F129BB910C7
              EB5B287801E932A0B7FF777E2E18BB265D36C9E774B5ADEA0DC4163536ECA4F3
              60EB008CAE8C5134695A4BAEA834D231F90797F6969C7C2550092480D1485701
              37CB717BFD274F592DAF9FD4F6773F3B82EE683965F39E431EDF4392E69BD9B3
              C002C31A2D9D44DE42494C0796CACC4FA6FB82E4BE9DB5FBFE386B004EF4CA05
              84BE7079B51CCF7A433960AEC14B96EECEC9EB07A954B048D27566B6CCB0B9FB
              FF348F54ED3BC78EE0A82FDF40C1B84917223D8ED952C36E4C6E5B7FA8F9CF3F
              E5E0ABCBE8D9BB0D5FD599DB9C82E08BB8743D8EF742275CF25472C77B6413FB
              0170058B897FFB1EE4785E009519363597EE5EDBF6F2C3D6B2F25E3ADF7909F9
              029DDEB2CFAD020A25DD6AD83A2718DDD9B5F1D5017C065571C1847300E661B6
              0FE3AECE0DAFD1FCD4EDF4B634402E4B72CB1A9A965E4B6FA2B901E36E495325
              4DF1F7D9F5618CFF3C729C8992A601F7593AB963DFF23974BEF70FE8ED21DBD1
              42DB8BBFA6FD8D15182C34B35D42B7F9C79F0DAE81311B44D01D2905B8045895
              4D1D4AB5ADFADDD12AE43ADB687BE551307B1E30E0E27EBB233074098019CF27
              D63C47A669FB209CC4EA67C8ECFB5F06F83BF0451C474E51FCD804E5785D9222
              4063BAFE432CD539081820B9752D06ED867503B123B3598E0F8C984116B12FB9
              75ED9018E4B224B7AD0768941400FC47AB0C6ED4B95CCECCDA80725FE519E0F6
              0E895D70D2648062A142A085233720B92C88168103565A30FEECA10902BEBED4
              2837B34E8C24476D520611EC6DDB03F02670852B50E42FFEC6BC41A0F2FA29FE
              FA4F90F41DFA3AC19BD98EE6A1304CE8EAA2CBAEC51D1FBC110B9E3B03DFD81A
              2F3003584D2E6BD9A39AF62082A9FA0F31588214175A1C3A7706D199F7E00A16
              03E0AD389DD29B9EC013AB3809B8CFCCDE02DE4FEDDC7814866D36B337805FB8
              0A42A78CB96119FED32FFDE405477D6536D1E9771CDE7895034BD2BB3743EFC0
              CDDCA03EE8299D40E9DCA771B9DCF74BFAB999BD60D8CF2C9BADCB7634E38E8C
              750457018FF4BFE0F9E9BDB53B9B1EFDE1009CD8F77E4560E2B44AC17AC063C6
              6DC856660F1DE875790B91AFB052F080A46BCC6C891977343F7327C92D6B8E4D
              10203CED7A225FBA0189F9C8752FE0C36C07D0014C9034CACC3602DFCDA5BB6B
              9B96DF38A84A5DC1624AE7FE054F383A1EE9AF92A6985902D80104259DDC3F0D
              2E027BA073C3EBB43EFFCB415C86BCEAD2751BB14C1ADFB8B3D6C9E53C0DEC07
              5C881E60AD992D02EEEC3DD8D4DAFCF4ED64F60E1E8CAC274972CB5BF8AA261E
              748A624F02EF0249C08DD883F1AC99CD065EE958BB92B6971F8621768DC7FC9A
              71058B099D331D5FE519C857F889726FFB7E92DBDFA5EBA37F0DCA99A1C47FCA
              540A4FFB22EE5839EA47C965D2F434D6D2F5C16B649AEB3ED5F6FFB62A920205
              9FFAA20000000049454E44AE426082}
          end>
      end
      item
        Name = 'clipboard-copy'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000010E49444154388DAD93316A84
              401885DF2C8338D915B510146C9D03D80803C909929B790AAF60D2671BBD80DD
              54222208828D8D4226CDAEB06A1285BCF2CDF0CDFFFD30248AA22F6C44293500
              78EBFBFE534A09A5D4D6355000C8B26C752A8438EBBA9E0278755DF7DA34CDCF
              805B0800D4757D4AD314BEEFA36D5B0080E338300C034A294CD384B22CE789E8
              9298E739922499B50821608C8131366B699A366BAD00E338628F561004572925
              4E9B62BF248EE327C6586A59D60BE7FC38200C4315C7F19931F66E9AE6F34AE1
              AF0821EE8F5E007C1C022CF72284B81C5658E6FF015DD71D0250001042907BE1
              79DE31C0300C0F535455054AF7EF961645F15070CE61DBF66E00591637C0E617
              DFCA37F1436332AB8F7DBC0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000019649444154388DD5D43D6BC2
              401807F0FF734A06271D441027B9805B314BB6BE408B93DFA16D960E078EBAB6
              AB43D7BE7E874E190B5D1C8F4CE2DBA2284ED5944CBE90EBD248304144A7FEA7
              CB73C7EF9EBBC091699A3E764429E500B8F23CEFBBDBEDC2F7772E0799A6E9D7
              6A35A5EB7A64D2B66DD8B64D7FE8A5EBBAB35EAFB7134C0280AEEB300C23DCD5
              FD62B180E338C8E57298CD66582E97A7A954EA43D3B4F03AAC56AB2818741B0C
              46A3D143B3D93C198FC769D77537C7D434EDAC5C2E432905003F4A2967301860
              3E9FC7829B4C261348291F019C6F76A3CD7EC1D80770C7397F0BA3B1E07ABD06
              005896A52CCB8ACC4B292184608CB127DFF715E7FC3D40591CB86F1A8D0631C6
              9E89E896738E4C26731C58AD5655BD5E27C6D80B11DD70CE8F03A59454281450
              A95488885E88E83AF60EF78D1082429F0900AF07818661A0D56AA9AD6E218448
              1C75E4B8FC5370381C1E0C2681C8DF42A9543A0ABCD82E763A9DC3C176BBFDB5
              5DCCE7F348A7D387819EE7458AC11B379D4E21A5DC0BEAF7FB00426F6038C562
              11D96CF613A1E76BDFFC02B98AA7327E4FAEFA0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000214494441544889B595BD8EDA
              401485CF0D14E0291020B45494915B2C0165F63568112582CD926C19296953A5
              8D6CD7BC09120D1512E2058805C2852504857D52802D2F607E02B9D51DCFB5CF
              B9DF8C67A4D1680400409222224939C95F245FD7EB35A7D3297CDFC735910680
              6AB54AC3300080B1B928B72C0B22D20540A5545FD7F5AB45D2246918065AAD16
              938A4CD344A954C262B1E80108344DFBAA691A3DCFBB2CB047410012C7E2FB7E
              CA711C8C4623D4EB754C26136432196C361B88089E9E9E90CFE741EE7C91C47C
              3E3FEA2A9DA4EC380E7ABDDEA7E572F9ECFB3E822078375F2814E2C33F247FE7
              72B92374698616F6CEC3DCF33CB8AEFBBCDD6EBF5DE4B06BFFA3A6695F0ED727
              8E080788A2F687C3616420AC09C7B66D8B655942F24544A0947A27F2E11A77D7
              44B3D98488BC00F8A994125DD7914AA59211C5E35CCD7EC84EA7439232180C3E
              030894526FBAAE27238A7FE4D40F181B8725EC76BB0080C160D007204AA9B7C4
              5D746B589625FB3540B158C46AB57A25C987202249CBB24EBDDABF1B51BBDD46
              BBDD3E3ABF6CDB16D3341FB78B92E2BF0B3C620D4ECE8569E21AC4BF73619B26
              DD213B810337517ECBA5722ED224699A264CD344D80900D46AB548E05E44DFC3
              07615B24311E8FA323FA2E44AEEBFE38D55A369B452693B911C871A467B3D9C9
              894AA58272B91C398DBB8ED75D44744E5D444012B66D87AD2346E808EB21E28B
              02A18BFD06F8A7F80B2D1DCC37E37502640000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000190494441545885ED963D6F82
              401C877F6730FAC7B3428C893A9AC0E2A28E97B47E82F693011FA6B5733BC927
              70D1C9B791916830E97551ABC417502169C23341C25D9E3B1E72B066B309CE39
              E222A5C466B3C1743A859432F6F81D0AE71CBAAEFFDC20E003782B140A5FE3F1
              F866096577311C0E63CD20842811D13B8057C330BE2793C94D12B9D8230EB06D
              5B5555F543D3B4BE6118608CA52BD0E9746059964A44034DD3FAF57A3DF61C4A
              E8FEEC1296CB65CE755D044100CFF3D06EB7E1380EF2F93CAAD52A168B056AB5
              1ACAE5F2D1B86BB18605CEE2BA2E1CC7398A75341AFD99330622021185052EC6
              1A59200802008F8FF5AE06A2B2EDE464ACA90874BB5D69DBF63ED643895404B6
              12873BF1629A261863E9090040AFD783655925221A542A95E756AB153DC27B10
              428417CA017C168BC5A7C405CE7D3542080EA4D8C029526FE014994026900964
              0291053CCF4B44607F1608212EFED2361A8D6404D6EB357CDFBFBA13F3F91C8A
              F2F8B34B99CD66911E344D13BAAE3F5CE0FF449814B15FEAB5581313881A6B1C
              56AB157E01E09796F80E06D5370000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000002BC494441545885EDD83F4FDB
              401CC6F1E7629BED7284CAD16541458278A3D8231282A1AFA0EAD8566D59993A
              B1F50520558A05EAD066C88B0812B403DD10435C65636B11121E524C1046018B
              BB0E49DA04E5AFED34A8CA77C992FBE9934B7C724C745D07A514519352A256AB
              E1ECEC2CF2ACD6544A29745DFF006029EC102925009401BCD3344D9C9C9CC4E5
              83DA785D324D73D5B2AC50434AA5121CC7599352A638E76F00C4866C02615916
              D6D7D7430F725D57FABEFFA25AAD4ACEF95BC4844C449ED088730EDBB6C1187B
              0920CF394FCCCECE469E1B1B1000B2D92C72B91C1863AF007CE69C2752A954A4
              996AFFB7B427A52C5C5F5F17AEAEAE2084C0EDED2D6AB51A18632897CB505515
              2B2B2BD8DBDB431004C9E9E9E90B4551BACE3B3F3F8710626020E9071442FCDC
              DADAFA717878F85A0801292582208094124747472084200802DCDDDD8110B296
              4EA7914EA73B7D5001E0A3AEEB95E3E3E3AEC8303B88D3D3D3C7979797EF4DD3
              94C3AE6FE6380E91523EA3943E350CE35737642860E3DCC3CECE4E581F969797
              C1185BAC56AB5F7B2163BD48866D73731386612C02F842297D641806128976D2
              5881C96412B66DC3308C2700F629A533F79163050275642E9743369B5D427D27
              DB9063070200630CB66D3791FB94D25413F92080C05FE4C2C28289FA4EA6E6E6
              E686BF8AE36C6363A3E3B94B08B1A4949F344D7B3E36E0F6F676D73374777717
              C562710608710EC655AF5B3BC7710000849087F31BBC1F21F56FFFC1029B4D80
              519B00A33601466D028CDA0418B5A181BEEF23088251583AF6E76EA6582CA254
              2AF55DC01843A5521929AAB526B0E0BAEE37D775FB2E5014E5A0D79380B8533D
              CFC3CDCD4D61D0058AA22093C9AC8E12D59AEA791E3CCF1B78C1D4D4143299CC
              0849EDFD7F57F1BF2ED27F927C3E1F97A3ADD6D3242CF002C0C1A8808DBE0303
              3C0FBC9FA669989F9F8F9FD321DFF7F11BCACA036191C1B07D0000000049454E
              44AE426082}
          end>
      end
      item
        Name = 'clipboard-paste'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000018C49444154388D9D933D6FDA
              5018468F0DA5D8418248490618408A200BBBA51BA90B2D5952A93F2043F7B079
              EBD685252EED5A21F2133A14B2A00EA5837F40592259192052A534519212B9A2
              6EECDB2541B6F808CDD9EE7DF51E3D8F74AF428872B98CAEEBFBC01B40052CCF
              F3DEF5FB7D7CDF6716CAE1AB1CE76395EE59929B27ABDBD96CF65BA3D120168B
              619A26C3E17047FB3BFAF27CDD25A7070CAE3DDE7E3D9F08E22F365301C0A7AB
              0C0A50A954643E9F07A05AADD26AB5BAE3449AD7E540A228CAF7B3B11A89706A
              6E05EEB1E51B8611F873300C23708F0F6E4FCDADE0682F1FD98F870F42086566
              D1054C04B66D4706F57A5DED743A00ACA69FF2E1A34D7AB4867331A250283018
              0C905246138429954A0001C0D5AF3F7CB67F0020A5E66E6C68BB8944A2E738CE
              7C81E779F7C964F85E08B1924C26DBC0CB62B1D853672D3F8465592B9AA6B533
              99CCB3B9091651ABD514200574FE5B10AE2484483DAA4298B902E9BB4B092615
              C6CEFBC8E0F6E7C9F282CB6673EA05FEBE5E5B4E30F539EE702E4680FEE0F3FE
              07F14E9DA96448B2630000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000026649444154388DA5D14F4893
              711CC7F1F7EFD93F9B6E616D82C4320C3759194ED070C176C98B5E2428760CBC
              E9B143DEC47374B4E358072F056187196460460882ECE9B26C0E4269E1217A9C
              D336E7F63CBF0ED670CE9E14BFA7E7C7E7E1C5EFF3FD098E8CC562A1A7A707BB
              DD7E077802DC04BE008F0DC3584CA7D3944A25CC46DCBFE14697902B5AD8562E
              D2EA69BB0E7C1A19197146A351969797999B9B2B03FDBB3BF9F485D20FAE36EB
              D89443E065BA500F7E7B1498AA18F03AE7E055AE89B2CE607777F7503C1E4708
              01C0F8F838AAAA2E59159646AFECCB7BBEB270580098F63DCDD4815660AAC973
              499257B03B9BB8D1D5C5C0C0400D038844220821A2D96C366A73DB707A9BD135
              4D00D3C72B5B019C838338946DBA1C0E6666661AF6128BC588C5624C4C4CE0E8
              28E3BCDDCA6E3279E20EAD470FAAAA8A70386CB272B8D5119466790D1C1E1EA6
              FFEE785DB8B6B696989D9D7D9ECFE701B05915B60BFB7CDDD4D015173BF9221E
              8F074DD3300CA31E6C6F6F17BECB7D75603018DC5C5959D9585D5D7D0850A91A
              BCF9B0C1BB8F6018162AD596486767CB33AFD7FB3393C96018467DE586EB5BAD
              148BC56BC05428146AA8AAAAAA0039EA72B98602818096C964CC412925521E3A
              273D56381CC6ED76F7160A8585BFA862069E66262727F1FBFDBDC05B97CBD57A
              6E30914820A54408D1072C9856FEDF8C8D8DD5F6EAF7FB6532990C9D17AC7DA7
              5229E6E7E7C5B92B1F1F73D038404AFD4C60ADB25EF88C51CAD585D5AA0559D9
              3B3B585E5FA7BAB5D51056A582B1A79D197C7F90CD9E185614EB4669AF1910A4
              52295328FBC7102F1EF8FEF9D38154887F6F8BFED295C5D3DEF037C8AAE66B06
              AF2D7B0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000345494441544889B595CB4F1B
              5714C67F67C6A5D8A328A9301256AB14A43618A1586250E81EB1A8DA051B366C
              B158B7A5552B4595FA9092D02EBA6367B3EA3F512A2F2DA591F02CD287050D0A
              0105B5A1C4B43636D8734F1763BB636327B0E8B7FAEED53DAFEF9C7BAFD085C1
              C141262727B12C6B4444EE00EF0116B001DC56D5BDEDED6D4AA552B7694F88F3
              8A0060546828BC71FD4D868787AF020FA2D1E85BB3B3B3D8B64D2E97A352A9EC
              A9EAAD6AB5FAACF8DBAF44046CD1B6B36A4331DA15E0C9CA0D4078588AF0C3C1
              000FFE8A6090CF6CDBBEBBB6B6462A955280ADAD2D595E5EA65EAFDF03FDFCE6
              B506EF8E9C716BA88E15E4C8FBDFEFF2F0CFD38E0096881811CCCD6B75FFA7A3
              01A36219E0EEE8E828A9542AC84244C6C7C799989840446E8B58E6E7E301F34E
              BCEEDB961811F1FB4914018824121A492420BFC3D4D494BAAE4B2291404450FD
              AFE6858505666666B45028E0799E44A7A7691C1C68E3E0A06F0F22AAAA914482
              A8EBAAEA235CD76569694981B6736D92B9B9B9D65A3CCFD3A8EB6AB55090FAD3
              A7FD03888808684B8A6C364B269321D80E9CF7E24DA804761D959E93A80949A7
              D33D0F6D6C6C7CB9BFBFDFE164241EE3F1EE11C30A3AF02A7EED14B1EC73C1E4
              C9CA0D3F3A3D4DD475197CFBA39E0172B99CBDBABA4AA552A1592957AF0CF07A
              3CC66B769DEA7199E7273EBB158BBFCB27148B457C3FE8BB25A19AA5896E3E36
              3686887C01F822620073FCCF99F965E7B99FFFBD620ACFC4EC946DDF57F9CE71
              1C492693D8B67D4EA2BEA8D56AED8CD2E974B7D80A90C964003E00D4719C4F92
              C9A4168BC5608A5A1621DAC1C3684D583732998CC4E3710E0F0F3F048CE3389F
              269349BD9044614808AD352000F3F3F32C2E2E02AC00DF3A8E23D6CBE4B90C3C
              CF23168B3134348488AC00DF5CA807178188E0799E789E07415F04F8F8D23DE8
              DE6FADF3F97CCB3100D96C966C367BFE26B78CC2BC2B53093B7FC18D07828FE4
              7FC5852452BF46ABFA7E12F5E04180B044FED1FDA0C4E0116B73BFBC076A80CB
              4BD431458DA3FBEDE8AD03AACAD9E37DD4F4FD535E8888AAEAC9E626279B9B10
              9A8230AF5DB98E1A734E861EB2F494E8AB5E59877969EF0F8C8902B0BEBE1E96
              A8AF4DF33E10F9F151F9EB9795B9756AE16B10A0F9A85D18FF0219AAD90497F7
              286A0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000028F494441545885ED97416F12
              4114C7FF6F972E2CB2054A4D6A6C4282B8BD986C433898B99968F4D078323121
              3D78A01E31EA27F003B0245CFB09B446A34631E9C598062EEDC5239C48438A29
              B1A464C30AC2786859C122D0A5A51EF81FDFCC9BF9CD9B376F66084314080410
              0A85E6892809E0FEB1F93DE7FCF9EEEE6EA55C2E0F1B62A084418D4484603038
              43449BB1586C3593C9CC663299D9582CB64A449B8B8B8B330E87632C007A7233
              006DC16519F64D019FF72494EA22445184A2280FA2D1E8CB743A0D22020070CE
              914824B0BDBDFDD0308CD7CD6613979D6DDCBBF21357DD6D6BAC62B581175FF6
              070238B40517EE5CF3B4BB8D6F0E7CF0FF6182A669BC337927329AA6616767E7
              95C7E30100FC02F0E8C6014757C76FDFCD811106002B7E736B6BDCB26E6D5036
              9BE57D3D8E158FC7118FC7AD3E8C319A7BBCC681A3F97FACAFD33F9DBB3494F0
              BC7522835CD79F12B001C6D8482BE8F57D466641E7008DECDB3785B3D9EC69E7
              EE88000CDCBABFF5DF6DC148A12B954A422E9743A3D1E8B1F3968176AD8CD661
              19F5EA3CF2951A545505E71CCD6613C562119CF706C85615C9E572D075BD3DBC
              A70CBF5F06E7FC10C08A24495B8542A107C2164067E5C38E6A478CB159A7D3F9
              C9E7F3ADA8AAFA359FCF5B1013CB816432794996E58F5EAFF756381C86200893
              058844225CD775B7DBEDFEE0F57A6F2F2D2D4114C5C99E82E5E565A452295951
              94778AA2DC0D8542F672C08E1863DD8B9501BC9524C93D11807EC9CA18730117
              5C8888E8E22BE114600A3005B005C05BC699019CA884662135D4A95DDB3B3300
              5B11681D8EF71DEB96158151DFF10050AFCE9F1D40B1DA18E907D3AD7CA586A3
              0B6D7C9DFAED0F00AAAAC2EFF78FF0261C2CC330045BD7B1699A300C63EC235C
              AFD7F11B0EA9D36E4E04F8970000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000003DF494441545885ED984F6823
              551CC7BFBF64663A99644D3709A1949E0CD926DBA2B05D745794966DEB160AA5
              82DD4B0F2B144F6DA1EBA5D0832C5E3C897AA8A7E422F4A215C143AC86C2824A
              557687449496E6E061A5A12EAD49669B699299791ED2C4240DECA4939215FAB9
              0CFCDE9BDF7CDEBF79BC473089C3E140281402CFF3D701DC07F0C649D10F00EE
              EBBAFE707777178AA2984D690A325B7160600092244D10D1D7A3A3A3C2F8F838
              00201E8F637373B3C8187BAB542A6D24934930C6DA26C85D16ED700AF59E3A23
              E435C2B101300688A2084992BA8828323B3B2B2C2C2C54EB0E0F0FA3A7A74758
              5B5B8B088210F07ABD855C2E0702D06507248E81A37AE182C6F024AF9B12A48F
              6EF7E0CEA0FB6500DD9560A648F8FC4F077E7CC2C300557AE4259EE73FD9D8D8
              8024497549F2F93C262626502A959600FC464400185EF194F0CE8B2AFC629D60
              66EB713E79E78BC7A604B993E7C700462AC16E81E1A06883713203CA1F04FC7E
              3F6B9403004992E0F7FBB1B7B7F7694DDB71A411F38BAC711A3D0070CB945D8D
              201CD7AE31C7D0D07F051F3EC0DCDB53989B9B3395687D7D1D00AA5D158D46F1
              F0A76FE07977A41A531F3D822ACB66DD0000B6966A7700AE69D07303C4C9905B
              6C6D2DB22C83B817C0796E403BDC620099FE633C5BD07B93AE5CFD05A954CA92
              E495AB438CF3DEA4B2E0D9682A08004B4B4B67CD59CB997AAD96FFE71C6C05C3
              30EE298A925055B5E90E621433D0B20E18EADF283CD5A16862E6AF82019FCF57
              ADA3280A0A858229C19687445194C4E2E222F6F7F7479A6E71CC00633AC07440
              37A0E95D28E95DD38180A7D2A0AF8AC5E2EF3B3B3B383E3E7EA660CBA8AA5A91
              7B3F180CB6F46E2A9582A228F38220BC190E8713DBDBDBA7242D0B32C6C01843
              3018C4EAEA6A4BEFCECFCF23954A79154589F33C7F3B1C0ECB8D921D5F243333
              33989C9CF40088F33C7F3D140A4114C56A79C7056D361B565656303535D50DE0
              7BBBDDFE6A281482D3E92C977756AF8CCD66C3F2F232A6A7A7DD76BBFD3B9EE7
              5FEBEFEF87CBE5EABC603A9D862CCB482412181B1BC3E0E0E02522FA96E3B8D7
              FBFAFAAC2F12ABC462318AC562753122BAC418FB00C0AD8E0A9EACFA533FCF68
              348A482402E0399983CD202210D1F32B58E142D02A178256B910B4CA85A0552C
              0B32A6A1C96ED5369AEEC5DAC196E9047A36533E6F9C13CD050F7F369D40CF16
              CB87A273C2F210EB9903C030DAE1D2946A0FAAB24CADDE3C01C053C907433FFF
              21BE879A0BCC56C8EEFF93D034D748DB8C1AE03EFBF5105FFE914D9E3581DD79
              195AB70BE9741AD168B42D52B51756962F777C3E1F0281C05D0077ADE66A2091
              CBE5DEB32CE876BBD1DBDBDB0EA1531C1D1DE15F9457707117DD22AB00000000
              49454E44AE426082}
          end>
      end
      item
        Name = 'select-all'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000006B49444154388D63E4485EF7
              8F0107F8F7E62ED3FF6FEF1898E54C71AA6161606060E848B3FB8F2E5131EB10
              23321F971A265C26130B8681018C14C7C2BF377771BAE2FF87270CFF7F7C62C0
              A786E5FFB777389DF7FFC72786FF3FBF30E053C382CF79A309894E06509C9000
              5F814AAA6AB123A20000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000012F49444154388DE594B14A03
              311CC6BFD82037B4D8D2E1F010841B32573239598A93F816BE411FC107F0519C
              3A16E9D629D0A15386B8DD9142E9D5137AD3C5E17AA297901627C56F0AFF7C7C
              09F9FF7F21C1C37309BF6665BA1C997C85161BBD0018FACC1400EEAF6313F5DB
              D6A6901A42EA6F35CE42C359687993F53B267345280044FD36E2A86B9954BAB5
              6ABD4EE0F4D63AF15DFF27FA878114A8BAE96A804A32676DEA08DAE4C567E0AC
              391A0D2DBEAE55BA751E5E8B9CDE3DFAC20000E5FA156697A1757175D04B4DBE
              3A6832BB0C28DE708C97EE71F26941D2E5788FDE1380813710C090B3D0F43A81
              B5A992ACF95E83F8FCECC645CA262F20A4AED0E32C74E234858D5F1C7571CB2F
              9D870BA9FFC060FFFE400A549FA34B354ECD9A0BC93A8302C064AEC8B1371052
              131FAA1F804D7B98DDEF51D30000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000168494441544889ED943D6A02
              511485BF272F90CE3A3603F66982902A4408D65944C81E4CA5A4C91E5C4436E0
              146AABB3838C30169322084E0A53E949E11B7F88F323A4F454E7C23DEFCD39F7
              CD35974FEF6B004932C6981CDE5DCDC6AF4A62ECF56307E8146924C902D4AFAA
              AAD7AA801100E880FBC18C636835BC4CCD344E14C60BAC24D56B551E6E3C1D3D
              05E84FA2835A12C618E569A4C884F102BBB1B5B9B920A22D5C6980CC8870924A
              D617FC17AC246DF273373B64715763CCC675669FA3E788CA45E40733FA93E8E4
              88DABD51914616E896743BDCE30352DB053017AD9792E7C3FAEB0396732ADE6D
              698D5512976E6639473FDF9CA2B16E71A58323870F56B3F1D02DBB7BA059A491
              84053A40FA2A4C0E17BB3934814E198D95A456C34B97DDFEE0B6BCDD1B1DD84E
              9FE9DBF35DA6C60F22FC203AFFC9E7657784BBFAB488A671222932A92D24F679
              56447E1091A5997E263B0761BC208C17794EFF3800E407D1F1EE3DFC02322F9F
              0EC40A0FC70000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000009149444154588563601860C0
              C8C0C0C0C06C10CAC024AA4292C6FF9F5E30FC39399F8152FD2C0C0C0C0C4CA2
              2A0CCC72A6FF4831E0DF9BBB4C303625FA5990053BD2ECFE13A3B962D621466C
              E2E4E867C2A7901E60D401A30E1875C0A803504A425C251CB1801CFD2C0C0C90
              8A01B96C2706FCFFF004C1A650FFC806A3ED81D1F6C0A803461D30EA8051078C
              B607061C0000DA6762CF8066A1DD0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000000F8494441545885ED973D0E82
              4010463F408D0536DA101B6DA92CED940B780D3BAFE041BC81B517C00B989068
              AB5660B326261484085A680C1A7FB2CC1A28E6953BD997D7ED0E50723400D01A
              1674CB268952B1C3E5B8876A5F050074CB46753019027072FADCF36ABE3CDF03
              55FA2A9943A75E33A6ED962965F24588284E0060F93252E2CB06A2DD32311EF5
              A484B385876D707A3B53E1D3A56E17000752E1402A1C488503A9702095A7B7D8
              1721660B4F4AE08BF0EB8CEACB06BA519C7C7CF87FE0BE3B53E1BB7D589B5D18
              9D7E1ED1833458233D6CF00F1F43817712AA8F7712DE498A8603A97020150EA4
              C281544A1FC83B09EF24457305F358C215ACC4C2430000000049454E44AE4260
              82}
          end>
      end
      item
        Name = 'find'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000001EE49444154388D95924F6892
              611CC73FCFBBE98BF876D0B666E16E795147B865170B8240A46187885A1AC40E
              A3EDD2A120068308BA740A3AC55A1434ECB04510B1CB82B5A03F906C054EE90F
              04E59257A10C713175EEE9A0522F6AB2EFF1797E9FCFEFF9FD78042DA2F9C3D8
              C39754C56C990022083C004892C043B955B99D7F76A75478318B68820F86D97D
              6ACA296021E8B2794787FA1870580148E81BDC5FC9B2F839BF060CE717A7D70D
              0245B3E1BCF25855CC96B79347FBBD637E47AB073213D7B9B19C5E93DBD5438A
              A1FBE07114B36522E8B2B58501C6FC0E822E9B57285DE30681DAEF06888C0EF5
              B5851BA9D7440D02D1AD82C0D398F97F19705841088FD27CD5B4D7F6916010C8
              EA1648994CE81B1DD95A8D4C1A04E5CC4780D8BD956C4741BD26D6F5EF61F557
              965D8191F75F7E6E9EB49894DEC17D1A42184792523213D779B09A4B48E4F85F
              8150B09FB88CBA777F0038FFEA6BE15B2AF7BB778F66C26EE9A65295AC668A5C
              5F4A33FB2E9700860B2FE7F2A201F79CBE8AE60B1D01E6811129E51B21C40520
              0AB811802405C4E4B69C2EBC9E2BE5176E215AC1A5F5D4F28F2737B11E3886EA
              74234C6A7DC915CA994F14E34F297FFF50EBDD73E61A9A2F74187884E06C299D
              7AAEDFBD88DC2C765C2480A2F94201601E4164A730D4FEC139205A4AA796760A
              03FC010F39B70CB07E86710000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000029A49444154388D9DD34F4854
              5114C7F1EF7D33CEC3D748A3235AA0396048D3C269A1B5A836AEDCA4119492A4
              6590A6D5D6200A6AE5225068E3BFADA141980A691B11AA8D192904865A1A920A
              3ACFC9714667E6BDB92DA6C87AA3A8BFC55D1D3EF71EEE39821DA26887C9BCFC
              10EDC4593B42540055088A00171040320E7483ECDDFAFED958E97984B9B684D8
              093B52DF8E23CB5300F438559BAFDCEBE6746E1A19A976F44D83B18520FD537E
              3622E624506904FDD3CB6DB7928359354FD1BCE70A80B725F9AECCE6520F6E2D
              C552E70FC7B83F3CCFC8B7C02A92F391C5E9690BA8E61572F476BB1DC45849BE
              CBD776F1383625E9BD00987149FDAB5946BE062641162BFF17388BCB0051E154
              6DBEE652CFAE18804D1134977A70AA361F884A0BA8E67801AACABDEEA46D268B
              5B4BA1DCEB06B86A0185AA01149DC94DDB13F627897A616D590008E14A4FB5EF
              0B4C4FB583C0650165E208E89BC6BE407DD300290356301601E4F8D842705FE0
              EFFA710B185D9A01E8EE9FF2E30FC7F684F9C331FAA7FC00DD1630F4691829E9
              D98898134D43F31871B92B66C4254D43F36C44CC09093D1630BE15048109BC1C
              9DFB29EAFA66580D257FE96A28465DDF2CA3738115A032A62F9AFF4CAD9A5748
              F6CD561455BB01B423790C5C3AE4B09D2AF366587679604A2714352780CA7824
              3CB3DCD1F07797D5BC42B26B5B5154ED3A820E240D52D205D22684B802540145
              085C48D6808F40B794F285A12F9A2BCF1F10FDF12501AA1E1FD9B52D280EAD06
              213A91B211E8D45F3FC35C5FC5597C0135E76462E8052013D3105D9C2634F186
              E0870130A200886D2FAB46882E908D483AF5C116D6DFF7EEFA21C9228E3D1941
              7168D508BA803B40873ED8C2FABBFD63008AE2D0AE21441792BB40873E70700C
              40013CC4E3F780767DE0606D6ECF2F28E60B04457AFF610000000049454E44AE
              426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000039F494441544889AD955F685B
              551CC73FBFDCDCB6697AA9A459FA92F82265464110C4D2AE15C18A4FABE24461
              2B2885FAB081CE69ADE88BFA502B1DB82A08B6D0B70C143AECBAB74ED0EA5AEA
              1F10C145290A9A4E636862D3B499C9BDC9CF87245DD6A459D1FDE0C2EFDCF3FB
              7DBF87EFF99E73845B44CBE15EFC4FBD816175B8041E058E03BD0821009418B0
              0C44142E6B2E5B4C5D3AC7F63717019046E056EFD3F88E9E0671850566801EBF
              D7A43B64116A6F062096CEB11ACBB0B16303AC288C8046B7AE7CC2DF0BEFED4F
              D072B897CEE7CE22E27A18F8D4EF35ADD1FE2083611FA6215A5D6B17542E4653
              4C7EB9CEC68E9D019E50F4F3D4C2B9FA046236137CF5022EAB232CB07277A0D5
              9A3DD645C06BAA88ECF6A8EAEE585535B16DCBF085357E4A6433408F3A76D455
              8FA0ED81A3B84A9ACFF8BDA6357BAC8BCEB6A6466A02D0693531FB64177EAF69
              0133B8DDAEBA04DEFB1F436000E819ED0F12F09AAAAA5A596925AAC7953CD066
              EA687F10A04790815A02C3A4A9F32E80137EAFC960D8879403D8CDF78EABF3C1
              B00FBFD704385E43208689347B007ABA4356CD861E244C43B43B6401F4BAF7AD
              1242152B5624D89B379A2BF786F62110AA8F48B5531AB9A83AAFC0D4DDE47277
              2C96CEED3B7DAB88A573A0C46A098A05D4CE03ACACC632D805AD714A2317A9AA
              DA056535960158A921502787B3F11B40249975988F26EBBAA6918BE6A3294966
              1D80485D89767EB80CB0A8AACB934BD78867F20796269EC933B9B48EAA2E832E
              D625D8FEF612AAA8C2F3C9AC9D1F9E5BE3CF0390C4337986E7D64866ED2D60A4
              086AD4541926879E7913F35048041903FA92593B3F7F3565B4B7B8A5ABC38321
              DC64D57C4165EEC7A4BCB0F02BB1746E0B785C55BFDBFA22B2E7B2334C0243E3
              78C27D88C8A4206754750A982E7F477CAD260F06DBB8F38ED219F97D33C7D7EB
              DBA4B236C0156044557FBE1EFD8A44E4F52A02C34D606882D6701FC0BB2232AA
              CA14E899B24B44441EE1C68313044059A7F4E09C073ED36251D34B113617A7A1
              6097090C93C0D03B78C247449009E015604AE1E5423AA11B1FBF85E7DE87F0DE
              378061F9EAEA5FFC679BECD525B696CE63C77FD9FD2F18268113E378EEE94760
              029151D00F505E7236FFD2F8CC299CE47AA9DA6560B47762B45A3783E7AFE3A4
              FE808253432C8167CF5664191791D7B4047EDA4927343E7DF206F87F0CB7A70C
              0E8C95C0B504FED1499CD4B5FF050EE012785B44C6800FA16AE5B7011C4A97DD
              F75A2CBE8FEA8BCE6642E3D3A77092B7071CE05F79A5D585682DC09A00000000
              49454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000049A494441545885BD975B6C14
              551880BFB3B39776BB2DBBED42D7528A184B6969B92526586E62A42D8D468CDC
              4490B73EC8C504A33E289848108DD118E385C8251A4CC0D4824A221428B754AA
              2F9AA2764B05A176D1B6BBED6EB7DD6DA7BB33C7876DA982B00BDDF23FCECCFF
              7FDF3967E69C7F04A30883CD416A412996DC228CF609800024005AD0877AAD99
              B0BB1EBDB7EB9635C4DD808D59B9D897549156F2A85328C6D540393003413608
              8994ED4023704CEADAC190BB3E10A8DD45B4F3CAE805D24B57E058BAD16A3059
              B6A6180D1B2B0B32AD8F3D68A738DBCA789B0924748422FCD21EE2F8EF016A5B
              FC7D839AFE9E8C46DEF29FF8540D9EDD7FF7028ECA4D642C58932F84F87A5951
              D6B45716E532C166BE6DCE5F41951DA7DB38D6E26F049E0A36D4B4767FF3CEF5
              FB4AE2235F85BDAC6AAA49319CDB5176FFE42D0B724933C74F4FB718A92C7030
              2EC5E8AABF1A7CDA9C5B78586A911EF56A63E20246E724B2D7BE6D138A726A67
              F994BC1533C627EA0D801082593936322C4AC6B92B3D8FA43C30E7B3B0FB7B4D
              EFED4A4C206BF956CCAE296F2E2F763EFEC2BC897704FF77CCBC2F8D165FBFEB
              72B7AA1933279E0DFD7C1443BC24655C36D6825297D5A43CFFF2A2DCBB86436C
              265E5D3C09B362D8929AFF90DDE8CC8B2F905A381FA128EB9E2CCAB464594DA3
              1200C8C9B0503ED561138855D692C5F10552F28A012ACAF21DA3860F4759BE1D
              60A925675A024BE070816056892B2D690243B5661A1DAEF80206538AD96C3038
              1CA9C6A40964A79940E04288F802C4DE1D99343A0C6D7FB13D30AE80D4A2AA1A
              D57B7A06A249E37B431190B21329E30B688176800BBFB68793263054EB82D6E7
              8F2FA0B63501D49EB8E44F9AC050AD5AD5F35B7C81F0C5F348E4FEC34D5D9140
              FFE897A1B36F90EF2EFAC3120EF4379F8F2F10F5B632F0C74F9E3E55DBFD6EFD
              B5510BEC3CE3418DEA1F0C7ADC5D831E77425F0181DA5D485D7BED4063E79523
              EE5B7737F1E260A3976FDD5D6E29E50EFFB18F81044F432DE8C33A7DA1AAA43B
              C3A72EF73C31D96EA160BCF58EE05F5EF0B2ED646B87D429EFFDE150476F4375
              8202C28073E536AC53E72E44F089A6CB176B5BFCD33B4211C79C1C1BA9A6DB4F
              A22F1461DBC9563E6AF8BB594ACAC32D0D977CD5DB41EAB1F289C06DB32B1600
              D5C0EA68A0E38C326E428610627B9A59A95A5694651E6EC932AD26A4947485A3
              B196EC528023EEEE81FE88F6219237823FD684BA8FBC0F5A640491007CFE10FC
              19B5ADE94CFBDECD9873F2C9ACDC8C7962618E10AC23D69496180D220B90515D
              76116B4A8F025FA8D72E76FA6B7731D0D270332621B8608DDAD674BA7DCF66E4
              40DFF5C7CC93A6632D9C8F25AF1825C38921C56604A43ED0A769411FAAA789FE
              E6F30CB75FFF8BBAF98AC0B9F2756C732AE621F90A7856F5349DBA119EACF8AF
              C0C8C84B811AA45CAB7ADC75ED7BC7060E3072C6DE00979ABE36D2F1675DE7E7
              2F8D197C4440089C2BB7629B5DF13050A3A3AF537DAD75DE7D1BD0FBBAC70C1E
              13108621F8D2B9C02129792EE26B3DE9DDB371CCE100C6F4D215C3F0C3C0FA41
              EFD5131DBB37DDF68732A902068B15C02591EB23DED6E39DBB37DC3338801029
              36EC4BAA108A42A06EDF3D8503FC034BC3E24B553F518D0000000049454E44AE
              426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000639494441545885C5D87B7054
              E519C7F1EF7BF672760FBBC92631AC29988D9064D0426813A74A407410D351DB
              99945E68A90583973F8AB55AC7561D3B4EDBA976C60B71AC95126E29ED14DA29
              9D86A18C721944A8B6694094A263361A828009EC6677936C36BB7BCED33F9250
              9824653726E4F7F7D9F7FDEC73CEFB9CF73D8A098EA3A80CD7AC4AEC0533D174
              0305080202921E2075AE83818EE3244FBF0F9675D9F1D444A094CB43CE826FE0
              BD7119365FA1A150B7020B805214798342BA810F817F0872D08C74257A8EEEA6
              E7F09FB07AC39307F47CA996BC3BD660737BCB80C780E52EBBE6A9289AC6EC7C
              173EB71D11E8EE4F130CF5F3DEA77192A61505FE20C2F356327E32BAA781D8E1
              ED20232B3A6EA072BAB9EA5B4F63CCBDC5A9503F071EB961A6D7BEAA723A4B66
              FB70D9B5517F174F9ABC1E8CB0A5A593F73E8D0F803C2B22CF2482CD66D7D69F
              20C9FECF0E544E37FED5F5E8818AAB95524D7E8FB3EA674B8BB9BD2C2FE33144
              849D1F84F9C5FE5384E3A9378165C933C1F0D975F75F821C17B070E5F318D72F
              2A5270B06A8667D6ABB5A514188EF10CC5D99E240FEC68E54457FC3FC0ADFD6D
              2DE1CE8D0F81650260CB76406FF57272AA97BB9462EF0D33BDD7377EB39C1C97
              7D5C3800AF6EE32BD7E5F3E6C7B1E9E7FA520B1CF99FDB2AE9940CB4BF03C0E8
              0FCA18D13CF9F86EBF0FA5F14C91D7F9C575B5B3713BB2FE8F2392A3DB59BFAC
              8C7CC37133F064EE923A6C0533B207E62C5A81E6F67C5E291EFC654D0979EEF1
              DDD6D152E475F2D325D7003CA11CCE40CEA2155902351B9EAABB50A8C7AB8B73
              6CB7CCCA9D30DC70BE3A279F79574FD315EA516FE51D6077660ED40315D83C3E
              1FF0F5BA2AFF84E3009452DC53E507C5DD4A77EBAEF21BB30016CF45295593EB
              B2EB375F3BF1D51B4E4DA90FA74DCB05B5D815F842E640C7F4004075D50C0F76
              6D42DE90A3C670DA98E7375050ED282CCE1C68F71501949716B8260D379CD202
              3740B9A61BD9AD62202FCF3DFE9E9769F207E7C847A92C80837755A989D9005D
              7E2E855264D36604804877223D39A88B12E94F83D02D229903CD9E1040EB47A1
              C4E4C986D2164E00B44A3A9939301D3A0DF0D6BF4FF7605A32593612698B77CF
              F601BC9D0A7D923930D17E0C1179AD3B914EBDD5119B34E0BE608444DAEA1591
              0303EDC732070EB4BF8324FB43084D5B8E744D0A4E44683CD209B04D2C339E08
              36670E945482BE637B009E3DF0515426A38A7B82115A4EF7A580E7FA4F1CC48A
              47B3EB83D137B62266EAA888343CF95A3BB1095CD1E7FB523CBDB70390174508
              46F66F06B2DCB05AFD3134D734F4C0BC03B184597BBC2B5E78D79C7C6C9FF1D5
              174F9ADCBBA395B6507FB320AB62CD7F33FB9A9BB2070220169ECA3B530AE5FD
              249A5C7AF44C2FB7CDF6A18F7148BA5C42F114F7FEA595A3677A83887C3975EE
              64ECFCEF9F00339D3D500F54E0AF7B01CDEEAC43F122C253A7A203457FFFB0BB
              B0FC2A37D7F8F4AC70FBDA22DCBF23485BB8BF45446ACCDE7067E7A687B1067B
              2E90C5A1490F54E05F5D8FA61BF7A0588FF07D113680184AA9E794520F2C2EC9
              D15655F95918C81973C793322DDEF838CAE6962EDEEE88A5819780A7925DEDC9
              AEDFFD98F4F98E4BAECF08A897CCC7BF7A2D9AD35885A201610DD010DEFD0AF6
              DCE9786FFA1A4AB3CF071E076A735D7667E50CCFA807F723677AE94D9A09843F
              8BC8AF800F7A5B76116E7A61C4993823E045955B89521B40D620348477AE1DFC
              1AC0E0F718DF6DF7615CB71065B3E70135C04D401990C7E09B3CCCF0A70F91BD
              88198BBF7F98E8814692A74E8C39FFFF05EA254338A7B112C506E041607D78E7
              5A6287B68FB85E337271CF59885E3C174761009BB700E5700220A924664F8874
              E8148993EF92686DC68C9DBB5C7DC6065EC0E9C6F7406D44E407287E1B6EFA5F
              E5AE4446ED0D176EABD3B81BD4462CEB21B8F2388011DB63BDA4027F5D3D9AD3
              F82E8A4D92B67E08B22EB2AFE18AE3460007FB5C3D9A6EAC40A9CD96653E8CC8
              ABB183DB89EEDF74C57197002F544E37BE83525B2C311F11F84DF4F03662AFBF
              3C2538187A062F6A25DF46D128223F52A25E891EFA23B1DD538703B03B8ACA86
              17C4108E4745E4D7B143DB89ED9A5A1C80E62C2A43D38DB9A01A11794C89BC1C
              3BB48DC8AEFAA9B601601F3A0C1D176431C23F07712F4DB5EB421480317F298E
              C212CC5888DE7FFD75AA4D97E4BF2BE28652B5758AE10000000049454E44AE42
              6082}
          end>
      end
      item
        Name = 'replace'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000002DA49444154388D6D924D685C
              6514869FF3CD77E7EFCE6D3A33666C6ED284109B981642845A5C584504112C82
              42518B0C58D112170ABA50D052115D1531DA850B292EBA7063C16DBBF06727A8
              318468179550A39D19C998E0FCDDDCDCB9F7B868A738E0BB7D5F1ECE034700CA
              E5323333334F016F475174747575954AF53CF9C3C71300550D80ABAA5AAD7D52
              6D47F5EB0C62002A950AC02BC011C7711E28954A7706493F321A471520129117
              D2E3F7F2DF987C3E8FE77973C061E01CB0741B3820A071DF00392042CC10C0DE
              1E2F011755F53311B95E2814EE0AF2F9268049E76E69A0B12A6753396FF88272
              B9EC02551179C718F3B7889444E47456FA7714E2BD20A3CA5B02EF3BE5896140
              2A953A057C134591E976BB46558F02676CAB61060A49D0568184FF89059E002E
              D46A351A8D060B0B0B2BD96CF6A6E96D2F0E14249DEDA3ACA09CEEB7B6860161
              183E0D245B5BB78A7ABD8EEFFB8F84ED9D38FCE35783080AA0CAEEC60AFF7C77
              690820B3B3B3ECEEEEB2B9B93954F8BE8FEBBAC471CCC6C606AE31BC717082C7
              4A453392B2E309BA7DADD7EDDA62B1988461688220A0D3E9100401D65A7CDFDF
              678CF95D55EF6B369B373E3F38CE43FBF79F04968118D8E767321F1B00C7718A
              D3D3D3AFCDCFCFE3BA2EA3A3A31863AA404644961E9D9AE2F8C8C802B0ACAA27
              76A26852552715BD6C813EE001CF596BA7E6E6E65E4F9264F01B6780F3F95CEE
              1C22CF039F7EDF6AFDFCCC2FD73894CFB50EA4D36B56553F1291DF44C402C7AC
              B53FA96A03D853D54B2272B2233CDB833117562FD6FF225065ADDB63ADDBC3B6
              DBED370B85C2BBC057404355BF10912F817B44E44FA0B0AD8CD554AF1C1239F2
              E2D801AEEEEC70CCF398CC6610C771585C5CBC5B445E55D5B3C08488FCA8AAF7
              F7FBFD3D6B2D22F2ED9346DE7B39653E1478A917C7577229332BCAC3A9244988
              E3B81B86E1D7BD5E4F5DD77D5C44EAAD56EBF2FAFA7AC718D3F13CCF3415EF41
              23175C910FD2C62C839C40E4877F01383631C983AA9D4E0000000049454E44AE
              426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000003D649444154388D6D944B6CD4
              551487BFDF9D7FE75506E7D199A1C55A6849694D4B114DBA102D05454D080413
              2401435C2842353E56BAD040941089B8302E1488180C31C485F1111726A6A021
              3E178406058229CA986128B456994E3B63E77F5C509BE27892BB38C977BFDC73
              72EE1133D1D3D34328147A1B586866EB878686A8469334BFF8711F701CC0B02A
              C630B0CFCC0E5F7EEB312AF9F3CC0D07904824080683B7005B81878045994CE6
              26D0CCFA311E008E201D14F4845AEFE4BFE10032990C92B6017F013F497A329D
              4E23692EFB55397F7ED0CCF66296072D57305223F4C2E130F3E7CF07D8091C02
              F2C09E4020B03B954A95E7B07DA1A6A51E703FB0C0C4B74C576A85332F590DB4
              99D921E04F49FB804D0D0D0D4727664049C7E7949F03AED76517D596DCD0D000
              B053529D73EE77E7DC7549714903A100E04FFF2B71BEEF3BDFFC36A02078A52E
              D1582BF43C6F21B0DECC769859FFCCD904F4BAE9CA1DAEF4C72C5C3A77128C8B
              C03740538D0DF0243D01FC62660773B91C939393B4B7B7236950F80381E2C851
              7F5E1A49BBEA3BEF01B80D690B663BCCFE4768662549AF572A150A8502006363
              632493C937657E9FAB94C68113401F60C035CCB6021F4D9E3D59235477773792
              1819199915C66231162F5E0C728C2FB98F4AAA75F68201F67799D299418A3F7E
              562B6C6DBD01E7F379A6A6A66E6EB073B4B4B4CCE6972E5DA25AAD52EF1CDB9B
              1A599B889309060198A85639552CE2A5D3E941E0F9AB57AF9EFEAF30954A914E
              A76F05DE33B38D131313C5896BD7F8625937EDD148B3A497817E2004FCD011AD
              7FD503569959BCA3A3C303A60B8502B95C0E806C360BB043D21AE0D14C26F3CE
              E66884F6FA488BD077C045603F30096C947844BDBDBDBE99F5038F03A3C07385
              4281B1B1313A3B3B83927E03AE035366B6EC0D41679DF7013796C89A0BA5D274
              AE526175220E863CA0626659490780CF0197CD669F49A55248DA0484CD6CB5A4
              D3C0AA4AC07D0DAC03B69D2B95A61F1C3A43C9F7690C06F124F3CCEC8073EED8
              9CD63D0D143CCFDB0B3C05BC2FE92CF0A9A481029C5A2ECD33B35FDFBD5CA0E4
              FB005CAEDCF8D7CECC9E35B31566B6C1CCAE98D959E030B042522FD031932F00
              365C30E266360534AE4D266AC626303E3E4E3C1E2F040281D780A899AD01AE48
              DA03D4033FCFACB151205B06B7D2A9144677B586C3C79684C374CD8BF2E1ED9D
              BCD0DC5C2780AEAE2E2291C852601CB8022425E5CC6C4BB95CFEA4582C128BC5
              088542030178697B40EBD6397702F8123802F8C0C340A300C2E130C964120049
              343535DD2D69BF99AD1C1E1EAE8E8E8E924C26696B6B8B493AD92536EF0AB860
              54DA0BDC0B08F8DE60F73FB5858645A2BB47CD0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000052549444154488975946F6CDD
              551DC63FCFEFDEDE3FBDBD7FDADEDDEB4DEF6A6919ED56B2280424442133D18D
              880E5E2CCE083209662C4C309A9457B089864034E2340E5F2044168D46261188
              9390A610D148F6C2C53FD5FD716E266D735B5CBBF6B677EDFDF3F8E2DED6C55D
              CF8B93F3FBE53CDFE739CF79CE57B4463E9FA7BFBF3F2C691298B0BDFFECD9B3
              2C2C2CD0F3A94749DFF1F943B69F942483B1E7810960CCF63FA7BFBF8FEAF469
              FE7704EB8B5C2E87A49D928680CF49CAE4F3F96B00B60F633F05FC02D8019C00
              229D5B3F7ACD5E8030402A9522168B011CB0BD0824812FA452A9EFB5FE637B1D
              F3547DE9DF045D3D481A077E8EF4E1A02BFB5E3B8260DD1E4983C04EE039E0F7
              C07E20C8E5720048DA00D5CB97D649BB2481A9B5950F84239108E9749A56C19A
              ED1F493A27E918B0239BCD8EAF04C1D5984391C216014560AFED93C0295F596A
              4B1064B35924C5810780D78029DBAFD89E030E844221BABBBBAFB6E849E009E0
              8B401C302211BFE1D6F6042D0BF6009B24ED0982A021A9222907DC0D6C8EB8BA
              6191A41010B29D323C887493E0ABA164B63D41474707920E00E76D1FB67D18F8
              BAEDA79BF5F45068F97D68343640E553BFA1B6B2B08CFD63EC33C08D6DAB0361
              493703B7008FDA3EBAB8B8882492C924926E041E0AAD5CFAA6AA151CE9C4365D
              1FDA8571A7D06EE006E038B83D01700058027E522E97397DFA3492D8BE7D3BD1
              68F4287042D52BF7AABA02914E2435008470F3624EDA7CB75129B727B0FDBCA4
              B76D5F2E954A4033F3737373F4F5F58D0363B8FEAE6A6B255A328DC15C01FE64
              3C0EAE5E1E7FA92D8146474791846D26272737D2120E87191E1E4612B5780F0B
              433B7034D91470D55C2F2FB074F235D6FEF5E7F604FDFDFD00542A15E6E6E6DA
              6E4A26936432998DEFD9D95956575701880701F7E5737CBC3BCD40340634D356
              AAAEF187C5CB840B85C221E0EDF9F9F977FE1F415F5F1FC964728BA43DB69F09
              82A071F1E2453E188DF2B36D230C75C6257417CD581780D92162AFDF9E4AFD3A
              542C16276C5F90F44E2C162393C9B0B2B242A315CB783CCEE6CD9B91F42CF035
              E0BD582C766E7676969F8E0C339A4824241D070E0143AD23DC162878104804AD
              24108944C8E572F7E672B960EBD6AD442211A0D9A7805E602F80A44742A110F7
              5C37C0CDC92E241D013E89FD65E3BCED9B8C3F607BBFED1F06FA6F17BB4BD22B
              928E44A3D160646484783C4E6F6F2F92F60131E038CD863834D8DD836000B81F
              78CE70F4EF4BE5FA97CE9C61627EA1BE54ABBD50AED7CF87818AED01E01BC0B3
              921E078268347A707474D49202E061E0B7B61F93F41949FB2B62AC2EDD191661
              DB2FCDACAEB17BF26FCCD7EAFCEAFD4B1BF717D87E51D203926AC0E32DC71E96
              F49DD6E1760183C00F8019E097C0BE45E85C8682CD1AD6F4B15289F95AFD9A80
              846D7F057857D230906BA9AD006FAC7B0E5C011292F6DA9E96D47B193E3B6F97
              D352D822B12DD1D9B65F6B606080743A4D2412C94A7A0BB8DEF63DC0B8A42DC0
              5F2585016C5B926C3B027F7C2CA4833B82E077867D75FBD8D83FCEF37269969D
              DDDD7CA2BBF96E04303838486F6FEF2D925E05EEB73DD152FF6DE011DB1F93B4
              7895B0DDC033B7893BC6C2A12351C8DBBE13383F57ADB2A9A303601B1008A050
              28502C160112C072A3D120088284A40BC09BB6EF2B954A2C2F2FD3D3D3432693
              E99274210D270E86F4ADDB83E0AD56CA5E05A668BE874F03A7C200333333ACAE
              AE1204C13240474707C562B1009C039EAFD7EB4C4D4D51AFD7A9542A64329932
              F0C2226C3AD5F05F6E953F12969E007609B28669E045C3D3FF017ED213B3A1CE
              0BFE0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000777494441545885CD966B705C
              6519C77FEFB9EC39D9FB49F6926D96A4AD691A5B6DA802D6695194322A4C1964
              B00C23D84E9D019B192F0CA82378E1362AA3CC6881423F884EED5805FCA02054
              E4938882D8564A13B43753B7D96437DD64DB64377BC99EC70FDD846EB325F18B
              E37FE6CC99F3BEEFF33CFFF779CFF33E7F45134422111CC74129C5F1E3C7A9D5
              6A0DF3E1ABB7612557CD7D4BFDED960B94FEF577A6F63F0F33D566AEE7C16836
              D8D1D1A12CCB7A0BB8BBADAD6D5F369B6D98B792ABF0AEBAB206A87304C445C8
              036FF82FFDC4F702976FFAE3E8AE7EA45A5A908076E140381CC6B2AC8DC032A0
              3F1E8F5FD4D8AD4C6BE5536F6B3367C64C415602BB81BD5672D57ADFE59B160C
              0E4D32108BC500FA81AF03DFB26D7B692010189A9C9C9C675CCD0E917E742B80
              6881B65CDBA6BBF6FAD67CDC06BE6C75F4BE3AB508020D19B02C8B50287409B0
              5E449E02762BA5EE78B72CCCC29DCC31F1875D88C800D0A95A828B087F018158
              2C8652EA76604FB95C2E8AC893C0D670386C99A6F9AE8E94EDA7F5BAAFA094FA
              083028A5C5ECFFBC23D0348D4824E201B6011F4BA552241289E37EBFFFA0A669
              9F89C5627B8687871B8CCDB624F12D3F44F38531133D11CDF4DC02DC0D7CB47C
              F2D07F47A0B5B515C3306E040E572A9523F97C1E4DD3F0FBFD3B816F44A3D13D
              E9741A119933D65A02AE77D5950D0E45E4272272C22D15164560EE08E2F1384A
              A97EE00980152B56108D461191178084699A6BC3E17083F16C1554B227B55AB9
              D82222EB80254AA95FB45E7F27684DAB7C7E067C3E1F5EAFF7FD4AA9F5C006CB
              B2B02CAB6163C0F6783C7EFBC4C4C4DCE07955009A5EF6ADFDE45F23377CED06
              655A0386CFF9A019EBDA5F1D3DBE7006EABBDF2E22DFAE542A5AA15068785CD7
              6D076E0C040261DBB69B7B726B14F6FF8EA983FBAAC00160B9D2179101C33070
              1C27086C1691D5C78E1DE3C29AEFE9E9190B87C32F2AA5B6C462B11F972FE2CC
              5AB616EFEAAB74602DF0B0B8EEC204A2D128BAAEDF06BC542A9532CD2E9C6C36
              4B2814DAA9947A2A1289EC18D1758177AA40D97EF460D432DB3ADE07DC031C75
              4B8583D5B1A18509F8FD7E80DB80AF663299A68BF2F93C954AE52F9665150DC3
              B8C2B6EDD7A1B10A0429231C037E29F0C8C4EF772EAA2119A55209E09A5AAD36
              99CBE52EBA309BCD924C26AFAA56AB93E5EC49D05B74007403CD1B52325311B7
              78966AE604936FFC96F2D09B0B06FFBF80B22C0BC330A8D56AD4B3F13F85D1D5
              D585E338DF29140AF71F3E7C785146A669B274E9520072B91CE3E3E34DD7253D
              1E6E8A45E9F3F9303535373E5828F2CCD8698E4E4FA37A7A7A701CC72D140ADA
              62092C59B28464327917F0E162B1785333BB6DED711E58B6D46769DA1781CD40
              2FE704CC51E0E99A2B3F7AF4D4A9A9866EA89422180C52AF8CA6504A118BC534
              603B708DD7EBED0C04020D6B6E8BC778F83DCB97599A7600580DF4975CB7B538
              530B236C03BA754DBD747334D22848DADBDBE9E8E8D8A0941A4CA552E3A3A3A3
              F308388E83C7E3B916C802BF01EE88C7E3F7CEDE1F615DE7FEAE4E4F7D6EC744
              B5FAF88327FFCDD3D9316AC0A79CF0DFEE5FB6746BD2B65ADF2A161B09E8BA8E
              886CD4346D476767E73540EE42127571D20FEC1491D79452AF84C3E1074CD32C
              57AB556E88440818E656E0E4E4CCCCE3D7BD7598A3D3EFFCDCCF8D4FF072FE0C
              41431FCF5767D0440411A95996A59D3973866AB57A1FF002F0726767675B7B7B
              FB9C714B4B0B7EBFBF1BB854449E2997CBC780039AA66D8E46A300AC0B054071
              33C2138FA44E35049FC5B4EB92A954298BCCB5E303BAAE7FA1BBBB5BABF7FB6F
              02CFD74944EA3A715631F5034FE5F3F9723A9D06D8495DBC2AA5F0EB3A409F20
              AFFF2A3B362FF885D0CE9E3D8B887C0ED8629A66C5EBF5BA4A29572975AF52AA
              0FD8110A85D0759D4824E2056E057665B359C6C7C7A956AB2F0251D3342F731C
              67D66F68AC5299383D33B330814C26C3F0F0F03FA7A6A63E5428148C52A9A489
              8826220F89C821E04B854281482482AEEB9F055E0152EDEDED74777723222EE7
              44CCF9127EB2CDE309F9B479AA7F3E0180743ACDE0E020478E1CC13DD7421F02
              3601578F8C8C9C4EA7D3733F9F52EAD34A29371C0EBB8EE3B89665B94AA91F00
              37FBFDFE362C1BE090AED465D747DA16476016B1580CCBB2EE03AE05368E8C8C
              E452A914C16010DBB637009E5AAD364FB0542A150DD8AB94FAFC31CB03F02C70
              FB3D9D49E24DD4B44FD3E8F3F948783CA8F327128904894462BDAEEB6F8F8E8E
              8EA75229E09C3E741C672FF0A76C36FBF8D0D05083C36030486F6FEF5AE0D77E
              A4FB67866EB528F526220F9E989EFEF99DC74FF0E7B3E7EE890DC100DF5DBE8C
              F7FA7C579E999979A58180528AD6D6565CD76556FB793C1EFAFAFA124AA94322
              B27C606060B2582CCEDBD59A356BB06DFB55E0FBB788FBDCADA6D98B621FF022
              F064AA541A345066BBE5E9AB57D215E2BA1B1A8E4044C8E5729C2F3C5B5A5A00
              D601BBA7A6A69A0607A85F588F01EB1E1D3BCD4F4747FF01B216380DECBDC4B6
              A712B635A6947A0C1810E1037B32D931D5D4DB79B06D9BAEAE2E0CC360787898
              7C3EDF749DAEEBAC5CB912804C26432E97635DD0CF2DB118BD5E1F7A3D52D515
              068A459ECD8EF1DAE424FF01AF16FF8123B9E1F80000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000A0C494441545885ED98696C5C
              D51580BFF3E6CDF266F12C9EE771122F8903C410C05052A56C71A16C95202C2D
              7B80469425A66A49372AB5A0364A415D54B56C01C452045243515BCA22356189
              4C15A06A03A4842684C024B163E2C493B13D1ECF78EC79A73F662638B18778DA
              FEEC91469A37F7DEF3BE39F79C73CFB94215314D93E6E666004484643289AA4E
              99E79DDB41E8F34BA7D5A180934D93DFB999DCB637C029567B5D5531AB0DD8B6
              8D6DDBEDC0CF812B868787F303030353E6B9EB9B082DBAF07AE06B9F8229C018
              CA1E85EEBA25D73E5BE8FB30BFEFF1DB298E1CA809D0A836D0D0D000F00D11B9
              10B8BCFC5C4D5A81B94037D08DD28DF20EE017F8B520EF7A671F3337BEECEE9A
              E0A08A05C3E1301E8F27082C53D5ED4057201078CAEFF7333A3A5A4DD7AEF103
              7D3F19D9F412AE500C23588FEFA845181E7F9D88FC1178D237B7A3D3B45B99D8
              BFEBBF036C6868404496013955BDDC308C7755F5738944E2ED64325955D944FA
              13065F79F4E0B37883D857DC39EC5FD879BB886C56A5D5AC9FB3AB16C0295BEC
              F57A894422002B80C780F754F55511E9AAAFAFC7E572CD58B98E8D905EF710AA
              BAB5FC53B3F823335E3F2DA06DDB88C8992272ACAA3E528EDC35C0552212B56D
              BBA617844EFD2AC0D180087CC244E13F071411E2F1384097AABE04ECEEE9E941
              559F070E88C80D8944A2AA32717B31A3B3F0CC3A1AFFF167D170C32F097DE12B
              6E11F999AABEA5AA1F1553BD35011EE283B1580CD3341B814B81A5994C86FEFE
              7EE2F178D1EFF73F2222B77A3C9EDF84C3611D1A1A3A5C57D8D7727C67F30F9E
              ABE44B0FB000B819A807BE3491EAA5B0675B4D8087583091486018C6D7815DC0
              CBFBF6ED0360FFFEFD008F027345E49C2A29E7246003B041443688C83A11B957
              448E0736AB6A464D1FAEBADA5CE420A0DFEF271008B8809B8087008D46A3B4B5
              B5110E8701F6027F02BACA69E8705DDD657D86AA1A8EE3B81C555B5597021111
              79CB1D8ECF899C7F4B4D8007B7B8EC5B178948B3AA5E0D5C54F64780CAB6CD12
              91F922D2DCD0D0D0D3DB7BA83FE53EDAC4DE47BAC070E1AAB3D5DB746CAA6EC9
              352FFA5A4E580F748BC82AFF719D37A6585D1BA0699AD4D7D723225DAAFA3AA5
              6D3A64A288A0AAA8EA3211B9C5B6ED1FEDD9B3677AAD4E91E2E05E4607F732FA
              AFD749DC786FC19A7FCAC3C02AC30ACE18EE20603C1EC7308C6380B381C52323
              239BA6090262B1189665ED03EE72BBDDABA2D168E18849C32932B67333D6FC53
              F280BB26BA0A6039B7AD00FEAEAA9B7A7B7B191E1E9E323997CB317FFEFCA745
              E41EE0B24422B1B6E7B3B41B2E02279D4FDD926B012E00DEA6585B4563D6D5D5
              E1F3F9FCC0F5C0CA7C3E3F2D1C403A9DA6502864BC5EEFD3405730185CEBF7F9
              0F8EBB1BE612BFFC4EC4F4600463781AE7E30A44EA806F8BC835AA7AEED8EEF7
              6B038CC56288C8D54051557F5F492DD389AA323030C09C3973D600FF044E8844
              22EF8D97863BCC50FD6BA145175602CA0022080B500EA8EA9540F7F0C6B5B501
              9683A107F8A1E338D3D67C9365FFFEFDCC9E3DFB7D11F929502CAFAFA4984F45
              006510650BCAEBC57C663CFDD27D8C6ED95013A0D4D5D5D1D8D88861180C0F0F
              D3D7D777C4452D2D2D589645B158242521B4F59443C65DD146009CEC10CED828
              E3FD1F93DBFE165AC8D504F77FF95F88B4B5B561DB7602C8F7F6F60E554DBE9F
              21814000CBB200C866B3E47247DECA9869B23814226C9A257F2DCBEE7C9E4D99
              11C6CA0D5AE5A8FB1D2547FF49CD74405B5B1B96655D027C92C964FEB675EBD6
              AA739BBD5E56CF6BE5BC6814D3301602A70209A0007C88EA86A189E2D0437D7D
              DCB7A7AF7A5737530987C3F87CBE00F038B025180C2EA9D6BB2C0E857866E1B1
              045CAED3815F89C82255DD05F4025E60012266D86DAEB9A3A5F9C7A78742D929
              15B5611844A35162B1186EF7914FA672FF722DA53473BA88744C578E1D65F958
              BBB09D80CB7523A5B2EC7D759C7645DB5285F125F96271B1AADAA82E072E0356
              357BBD532DD8D4D4846DDBB66118DE7C3EDFBB6DDB36C6C7C7A7859BD4BF7401
              0F028B81AE783C7E4B4F4F0FC549C7DAEAD656022E5727C81A545728FAD8FA74
              9A5FECEE6573368B57842FC7A2E377B6B63CDB6CF9D623381FE6F35301CB565B
              2522E7589675767B7B7B4F35C8B2F5CE149185AA7A31B009F8AD88DC61DBF6E0
              DEBD7B0198EBF372762C2A82DC07FA98A28FADDEB99B7B27E5DC31559E4B1DE0
              E5F42017C6634300AFA5074BD9BF5C46619A26854201C771BE43A9AADE605956
              4B7B7BFB94ED16914A91719BAABEA8AABB54F5CF405A44964FDEE6F3A2514464
              09C802555DF542EAC021709325EB383CB36F8067F60DB07F7C02A37C6EE68160
              2C1623954A31343434AAAA1701C93264EBE190E5167496885CAAAA0F643219F2
              F9FC04F030B0C2E3F148B912A7DD6F019C07FA26F0C93DBB764F0B379D548264
              3D70BD699A8B172E5C2891480411C9014B818F80D72A9086515A52B6D0CDAAFA
              31F06A7F7F3FFDFDFD50EA5D9A0DC338BFD2019A6240A981DABC353BCA8E7C7E
              E680C3C3C3A8EAFDC03AC330DE300CA3E876BB1D117144242B22E788C83CE02F
              966599A6691208040804026E11B90978707C7C5CD3E934A9548A62B1D80FFC01
              B82D1C0EE3F57A2BEF0AA33AB8259B9D311C80914AA51818189850D5EB1DC799
              AFAA5F04CE52D5CAE76555CD032B5575425549241288C8A5401DF0642E97231E
              8F138D462BF9EF01E002119937C917F3885811776DA9D704482693F4F5F5E1F1
              7876023B63B158A5895A0D9C015CAAAAEB76ECD80194FC0FB80DD803AC0C87C3
              957433F90E3107ACB06DFBFBC648064A4177D469E1305E918347D9112D58F932
              3636462693C1E572556E17EE0656029754E0D2E934F1781C11395144CEA0D48A
              768A4827D079D8F7EDC072D334AD422804B011E80C1A86F7AA8699F7C653EC1D
              8BC500EE02BE095CACAAAF54E0E0607074A96AB7AA9E3D323232F55F1B067EBF
              DF12911EE0AA770DE389EB94E7111E40E4863B5B9B1FD938345C35583A020102
              2E17C9E9123580AAAE53D58DC0AB93E1A2D1281E8F27025C0B2CCF66B3542B0C
              3A3A3A725EAFF709E0B66DC813BB54B3AD227703F7D499EEEEE78E3FEE8315DB
              77F0D749FD4F9DCBC5F79A9BB875F62C41A4B5BF50D829872BF6F97C34353521
              220C0C0C1C840358B06001E170F85BC0778179C96472A27C2D324566CF9ECD9C
              3973DA44E403555D728E216FAE340D43906781D350BD45E1F94D9911360E0DD1
              ECF5726E3442D0345B44E47EA05D1DE7D82980D5C4344D4E3EF96411916DC053
              131313ABDF79E79D692FD6A174649E78E289B85CAE178041C771AEBB697890CB
              E2711391D594FCFB23A01BD57D8884800E903341FF01BA3C373EF1C18C6F234D
              D3A4B1B1D12F224DC0FDFDFDFD23D335F715711C07CBB2F0FBFD9533EDAD47DF
              DB82DB10675128F88A213C0932011C85C831408852A77897AADEF5612E975AB6
              6D3B33B6A06118343535E1F797FAE06432C9D8D8D867AEF1FBFDB4B4B400A54A
              BBA7A7D4E6377B3D5CD9D0C049C100C1C36E6CB78E8EF2FAE010EB0FA42902FF
              06ED1005A4A02270E30000000049454E44AE426082}
          end>
      end
      item
        Name = 'control-edit'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000000BE49444154388DC592310E82
              3018855F0D17F86781CD0E12599B30D0E095D453A8897A209A327008DC046726
              56EB8409291A6207DED23479EFCBFFBF16985B8C73EE04F088E8E5040080B22C
              CD3FE1244998D75F9AFA6119FC20345A172C570A9CAF5055776CB30C52A6C309
              00E0D9D46C0C20658A38DEE0783AB3C37E678868E0594C19B50F1191B5EA24C0
              2F4D02B46DDB9FD69A9F0E967E30FA125A17C8954214ADCDE57AFB5EA21F8463
              7926653A0858062184DB47EABACEB9C879F50692B0341C9F0AA3AA0000000049
              454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000012B49444154388DD592BF4FC2
              4014C73F6D2F9D28A1896DD4C49A9414128D031172249818075CD4FF1667DC8F
              41C22C24E0AFB00A9DDAE26060A0542176D0CF76F75E3EF7BE2F077F1DCD34CD
              5C85A256AB5DE62A04BA790A9152264AA9F8B728A56229652296E2288A88A328
              F5A0A6EB98A6C9D37048B7FBC0B1E761DB3683C180A01270D16AA52203F0FEF6
              CACBF3445B175AC5E2E2E4F40CAB50C0F38E504A310F439ACD26CE9E931A40A4
              6E32705D97EB761B80D170C4DDEDCDC63E7D5BE1124337D00D23B3BEB3F02776
              16C6494C12C799F5AD77389D4E79ECF7E9A91EF330A4D3B9270802AAD5CA66E1
              FEC1218EE32ED6459AFE15E26336633C9ED0683456DFA66497B285420884C81E
              B8ECFB947D7F75AED7CF37F66952CAE4FBB0BB2180AB3C859A655979FAFE019F
              01A18E586BD072150000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000001B3494441544889ED94C14E1A
              511486BFD30CB0F04A358A090B864565485A1977C35AFA1AD6BE49AD69FB0C6D
              67D3F63DA40FA04B5337536908AB5969443684F1B88021C354048CECFC57FFDC
              FB9FFBDFF39FCC8567CC80D8B6BD5403AB582C1E02A82A22C2BC7C1E9DAA62A9
              EA8764C1227C2E5DBD5EBFF57D3F1A0C0683688479F82C9DEFFB91E77991B568
              A6DDEE0D27A7271863D8755DC230D456EB9F6C6C6E885BABFDA7B734D1D7F99F
              B3984A42230085C2169B852DAEAE2E69367FA3AA1C1F37A5D7EBA1AA54AB0EB5
              9D9DF159F1B1960C27A300DDEB6B196DEA687DCCF3F9970A502A95F8F2F91341
              10F0E3E72FB6B75FF16E7F9F4C2693AE01E0C5A211018808954A4501CAE532D9
              6C76AA7622A245398020E3B5946E68908C281D4B8A4F68521DC93D35C023234A
              DE56D107758F32505582E0AF00B4DB6DFAFDFE54EDC40C56F3F9E47526782E97
              03A0D3E9F0F5DB775415630C17172D0E3F1E51AD3AF2FEE0E0E119BC7E538B37
              EF9B01AACADADA3A6F1B0DCCAA61D775350C43463F5A7A86438374EBB3B8312B
              341A7BE36FDBB6E317595555D235E2795E3435C02780252247B1E3329E6B711C
              67990D3C6336EE00B01E6DBEA24776AB0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000000DE494441545885ED95310EC2
              201885DFDF7464F9D7EE3AE8014CDB63A9AB56573551EF648D57A88760226512
              1771D026A80334916F212490F741C8038844FE1DCAB20C428820E15A6BA44208
              30F32D8480522A49EDA4AE6BE333BC280A0280C4676817A97BC9672C96158808
              C3E1004D73853106EB556500901781F5AA8294129BED8EE6B3A96166B8C23B05
              2EE79373D32B93BCBC01A0472898D979724BF0371005A2401478EB81495EFEF2
              2710004829F1189F9DF0B5C0AFD82A1E8F47667F3886A9E20E9C6DD89F3760FF
              67EF025A6B28A582DC44DBB6216223919E71077B7E4042EE4430150000000049
              454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000175494441545885EDD7BF4AC3
              4000C7F1DFA509248BB5D8401D7AD06C6DA54726111771B16F50F73C81CFA0CE
              3E411EC1CE7511172BFE5932A758EC032425CB0D259C4B2BA504A489E582DC67
              0907C9E53BE4B81CA0288AB25344D775689A26BB239310023AA514B66D3300FB
              B28336CC932409F4E5E00EC099C4982C4F00CE5781F03C4F789E272F678DEFFB
              F07D1F0050CE8F6F8DFEFB2DF97CCD66988413989609D6EB89300C4914C5A8D7
              0FC018931F388F637C4EA788E308A3D103310C03878D06168B4539021963608C
              81732EAE6F6EB5CBC14074BB9DADE7C90C7C7D792645E2DA9D23B157AD02002C
              CB22CB6BAEB94ABF485460512AB0281558940A2C2A7327393E39157FF502CEB9
              004038E7B99EDFD95E1C0401DEDE3F104711A9542AE27E38C4783C46B3D944BF
              7F213FB056ABC169B56075DAE86DFC6E6D6367819452504A5743E2BA6EAE794A
              BF4888E338B06DFB11253C342549F27368BA42098F9D00404CD3846118B26332
              A5692A3B415194FFEF1BEC896421A69682900000000049454E44AE426082}
          end>
      end
      item
        Name = 'debug-step-into'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000001D549444154388D9593316853
              511486BFFB729F2F92A72FA94D8744A890FADC544474B2608788A8A353141441
              BAE8A0081D74510441C1A1A28B501C14ED96C1AA4B1637B574D3499124A54843
              1AD246F3C8EB3D0ED590DAD7142F1CB81C7EFEF39FF39FA3B2D75F61A7872F02
              1328955360B1C50B9B8BD9CADDD30B00DA4E0F5F006E809C33C1AFD9B03E6F58
              0DFB12ACB61ADDBF062680F3CB9F5E7FAC17EF239D804C2683EBBA7D493CDFFF
              43A054CE043F67EBC507482700C0755D52A994D9AA1500AD2016D6E78D74DADD
              64BBDDA6D56A7567E1380E5AEB680200CC5AB1F8C81112FBC7682C9629BF7FD1
              05F9BEBFA9222D606203590B4B9BC1B337E3DA1B7A02DC8AEDD8555D9A99ECC5
              46BA6321F2D57212879227C61163DAC03250F2460BBB53A7AEF4624D5458C03D
              A578961C2D1CD65EDA02AEA2780794BC63856C323FDEAB6043E8B0599BD23B07
              41A9E7CAD239059680A00091A7DBFDA327F95EFAAB60630B0B8F2FD1FC509C0A
              AA5FF6858D1F5A442C442641BE019783EAE7BE0A542F5BE6DA4BB60DED7988E2
              0CC2F195B93795DAF41DFCBD239BBBB04E4EDC75800184B195B9B795DAF46D10
              E9EBC23A058903791207F3746A6596661E81AC15EDB7072A2AF9EF2DB8AE8B6D
              DB515022F7F37F6EE137F602A172A4E0AE6F0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000025949444154388D95944F4854
              511487BFF3DE9B199B722C9B315BE49B78BB208C36220845449B5AE4CA1681A3
              9B8C0C0437B9125DB588146C618BA86002CD22CA5AB4080A321DA2853BDBD428
              946834D28C8399F33C2D6694196BFEF883BBB89C7BBE7BB8BF7B8ED4778DE2B3
              4F844418005A45A40E102A57C78FC7830F573FBD02C0F2851B83021F80347043
              55E70115A9983997BFB1041904D2AADA9C59595C4B4D4FB0B1F415D9CCE0F178
              2A22BACB71BC5E6F16085C04FA322B8B6BDF4722E85A1280603088E33836102E
              4B0C34000D39A0C82154E753D34FB661798A00FD159599932159037463E9CBFF
              E20380B19B650188089AF993BDC15F83A7DE21B59921168B15D073CFD00EB417
              ABD00250550C7F0DE2DDC3E1AB77F184C2B6C242F2FD98AEBC1CDE991307DE15
              052AAC0355BEA32759FF3687150A57033181A781964BDDC04E681C785BBC42D5
              59E05CA0A9F5F5C6721CDC4C0AD3EA15E1018A065ADABA01F2A0114A1825F6CD
              9956813155ED041EB1E922A605C26590FBA88EAAEAF5E4D438E64C14C7718AB1
              7215C233A04F0CB9873288692D009A0DEB2F11B906B88196B69EF44C14B28614
              3725F1FC16072EF4DC16D39C10E43C421D8AA00822C71475818F7939714A9822
              00E6FE7AF61E3F83E1F3E36F3C8B276423C81D842B281D0AD1C4E410BECF6FCA
              764FC1043003751CE97B01C20842174A07683431394C726A7CEB1FF653C214A3
              806E9820EC039A503A816D589E4A768F957FD25DFDC9EFF8EC6A55B8B1197013
              93433B6150C6947F869E585ECCEA831828A41305B1DADA5A6CDB3E059CAE18B8
              A55D8DAF3C5965E2117639BEFE02A80ECD81E5B2E5B90000000049454E44AE42
              6082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000309494441544889AD954D685C
              7514C57FE7BD17A7699AA626C1898B69DD741389AE42D408460B5594B66E3A01
              BB93B6B86A41A22E94C445850A455DE8AA20BA28A89B50C722883404FC009516
              6C16BA505B5A17A6C9048798BCCC4CE6B8C89B6426997412EB5DDDFFC73DF79E
              F3FFB8EA7EE10C6D7D07403C2534827804D3214900B6DDC8DF64ED93D2ED1BC7
              6E9DCB56B710B5F51D40D2ABC05BC04DCC05602609609B36B57E22423CB902EE
              4F6D8E03F1F27C9EF2F4755C5CDC6E02CA85DBF5098446809B86E3CB0B85383F
              7E9685A909002491C964B6952005B4EDDD5BC7E051CC054C3C3B7E96C504BC9A
              A0A7A707496300B6A995AD76DCC8B74D04740033CBF3F93AF05AB33D5A0BBA6E
              ED8E7E24246397A7AF6F4A5B5278370C56060D0EB452A93035B5E162108621BD
              BDBD00639B569558E42A97B0A55A2E5DCFBF466BEFE300FCF3F3D7CCE5DEAB0B
              0A82A02AC168738956A828BAEF0100763E38C4AE8123089D02AEED1E1C9E1001
              F9DC3B754992B8B0A944C605A03BEA4893DAF750C2445DC0EB40ABD091F6C1A3
              1360F2B977D72BD05C22CCF7C073E057BAB26FC6F33F8C8399453C037C055C14
              3AD43E989D34622E61B2558902C339A48CE07C4BE7FD3BEE7DFA25A89401AE02
              0711312227F4C4EEC78ED279E86590AA728492C2E4966DF08130022E63BF8174
              463088C24BC0CC8AD0007C0B1C063E9774B87D303BB970F5CBAD4BB474E31AA9
              7D7D6F837F4C7ED363C96FBA465542A2DDF6096092B5B730BA1970D582BF3E3C
              CDDFDF7D46A5185FB6FDACED4E43683BB42B2DC0796C6C5FB4FDA28B3195C542
              55E3649FC3463E10AE96A2D44E5ABA3304A95D74674709F7A403C107C049440E
              335C29C54BD31F8D50FCE30AFDFDFDD81EDBD24B06F0D202C53F7FA5EDE18344
              1D6901EF4B3A697CC966D8A57869FAE311E2DF7E2208822D4B14359C157B8021
              DB5F005917D7C0570B5A9368FD5C9DBF214169F6162E17E714DD3304142AA58D
              E0B0FA929B4AD4B027EED83F40EBFE0100167FF986F8F72B75EB411054CF60F9
              3F25A8ADB2514793443A9DBE53E8AA353E831AA0FFA3A335B5BBE968FF02916A
              D2F578D4740F0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000030F494441545885ED964D481C
              6718C77FEFECECACAEE3C2BA4342D45D7B690F065A0A4DF1905B0239190F4D3C
              049B801E4A9B5B2FC54A5CE2A1901CCDADADEDCD9098433E109234D183A0A114
              DA924DA9162FDA9284ACBABB6676867566DE1C566D96F563475773487E303033
              EFF3F1E7F9CFFB32F0B6235E7F50634DD4B79D42359A1180285DDE16D7CA91BE
              3EE02B475DBB097F780CA3B33FACA8A15EA00B415C20143FC59CDC8B26E0A96F
              016AAC09A3331956546D0CB0816E2999715F2E7848AFE262EEF2E2733FCDD705
              D4B79D4251B55EC0969E7B3CF3E0472737791D699B7EEBF9460508C69A01BA80
              EECC8321273BF6F39E372E11801020445C4A6F3A37796DD3E05028444B4B4B55
              1ADBB6CDDCDC5C5140F18B4771CDACDC6AECAAAA128D46934072B7024CD354E0
              B55D0080E76C99E4BA2EA6695E042EEE56806559940BD806DBB649A55225EF1A
              1B1BD175DDB780959595CA04E8474E128AB762FDF32BF9C763E5EBBA4E341AAD
              7CAFAE5262812C5E52A98F090241708BEA14BD81D867DF360B18D03F693F9F56
              8396F9FBBD9242B66DAF17F3C39A05AB8912A47C06E2BDBA8F8EFF1FE579E079
              0B404228815B46677F6DDDC7274A0A398E43A150F07D9558E09819801101DF34
              B47FDDE1BECC60CF4CE1E533A447062CA3B3FFA45094DB82C04DE374B203296C
              F38FBBC0EE2D10005AE23087BE1A8A0A9802FE94907416E6A79DF4BCC4F3D0E2
              AD04F48630823B481CE9791D2F86FBEC7C6A9C4422412412F1DB1FCBB2989D9D
              2D4EA030F784CCC3A1A5E8B19EA3C02501BF694622AC1989F24C01284A5FCDFB
              9F5EC8A7C6D72DF04BD92EC8FEF20385FFFE4E478E9EE9091ACD5F2084215627
              A4D44610AA560B5C03B2C07785F9BF802A59B015351FB471F0F3CB3522A8DD2A
              0A96ED8BA383F9DCC45580EA58B0194A6D8403672FD70875AD39ED8BA357D69B
              03D5B3602384AAA1A89A013C93F0E5D2E8603E37315C125395836833DCE5344B
              F7BFFF37146F3D67CD3C6279EA4659CC6E0FA26D8FE2ECD84F5BAEEFA90595B0
              A71654C29E5BB01DEF2CD8570B36FBFBD9370B763AEE8DD891053B1DF746AC59
              F0C679058B8D97CCD7AFE6DA0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000488494441545885ED985F685B
              551CC73FBFDCDB26ED92AEB959BA56A55BB331C13F542AC35587BA4D50B02A42
              411161B39332063E5406A282B00705116D755261886E8AE0C017AD821B329C52
              441161A8C5873953485DD6348DBB77BB6DBADC1C1F6ED2A62E6BD3DC54F7E017
              02879BDFFD9D0FE7FCCEF97D13F85FDE2457FB420BB71602AE1A52919493C331
              5355BFBF78769F46D3DD4FD074E7E36821A351446E4258E30172F6F2F49FDF27
              5E7DB46A407D6154CFFABE411A62B737032F23EC11A4A1EACCAEE240CC4B8279
              C0F003FB69E8E88A02DF0011142FE5459D10852952F50AE6BCC0CD03FA8206A1
              EE5E10791708A2D4D6FCDCCCB8397A8C99333FE06492554FA01C6F8C3A40A0A3
              0BF1E9B788488F52AAD799BD389E7CA79FCB937F784A5E0BF900A4AE1E11EE51
              4AD90AF5D9F4E743D7041C1456B0506311200538F6CF27977D31180CB279F3E6
              55034B26932493C992532C222845DE9A46CDCD2C9B4044F0FBFDBB81F75781EF
              A0A66907A1E4148B3B2B2A37B792445F023B6A8AE62A5E1CE84B042D29DBB619
              1B1B3B0F9C2F7D5E575787CFE7AB1EAD243F7800741C07CBB2AE781E8BC58846
              A39D4073D574104F2412E3B66D570E18EAEEA5F1D69D90CF91FEF47572A9F1A5
              C207817B3D001E2C7C1600154AA10A26417CA0F2F3D15A5314E3910322F01A70
              A4B57FF897E4E1FD4B41EEF400B748BA0B07284C208C42029BBAD4EC991F17A2
              341D600DB01D78520F45EEBB1AE4E4E424A6697A065B5C83EEE27DE713590BEC
              08F70C9C4C0E3F3D7FDD38569A6CFCF4C5C0C6CEFB118EA3F84A0F4576B5F60F
              FF7AFEF07E2E9740B6B4B4108D4677031B3DF07D9D48244ED9B6EDDE2EFABA0D
              5C7FE06304F9167715B7679367FECA7CF126B3677F82BC83D437B0BE6F884047
              E75AE0048A0DC0CE9C951E2B5DC9C2217903B8CD03E0D144227174626262C1E8
              B5F40DD2B0A57B93C02870017851893A8EE358CE05D7704A9D1F2D6800341756
              B21DD895B3D263E70EEDC13153B4B5B5D1DCECE500BB4AA5524C4D4D2D1C92F4
              27AFD0F6CC91DFF550E40E6058448E0922683E7CC675E5B30828A54E6821E346
              3D72C325C74C91CBE5C866B39E011DC7014A4EB163A63877680F46EF0BE38D5B
              B63DA8F22A26225DFF74D44AA9021ACF8B10039E03B954ACD7502854932DCE66
              B3473399CCE27BD03153A4DE1B405FB781FAF69BCF8A70B6142EB8F561FC1B3B
              11781B9198423D05EAA3E99121E6267E2B4D751AB74CAA55BC38A8D82A87EE7A
              8C48CF00088710D987527D0A3ECC8C0C628E1E9B8F0B87C33436367A6073659A
              26966555D649B4A62891870600DE42D887527B290307EE161B86E119B0D84A2B
              0214F7A20E02DB50EC053E982E0307A0EB3A7EBFFF241E5BDD15766B2939569A
              59F7A2EE069CF4C8205619B8120DE0D12C140715D7A0E8F568A1C8B23FC483C1
              208140C0039B2BDBB659919B51B939729973CBC6155ADDBF6FB756A8DADBAD1A
              AB6676CBDB3F4365F49FD560A5AAB5DD5AAD2DEEC45B2F8E03A760156A706666
              06D3349FF59AA7E888AA066C6F6F47D7CBBFBE2A766BA5320CA3D8D25643E5ED
              56153A55139C2B152F0EAABE665A5B5BD134AD2634E554B45BD7BCFE06F4E9D2
              F5100E7B350000000049454E44AE426082}
          end>
      end
      item
        Name = 'debug-step-over'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000001CD49444154388D8592316853
              511486BF73F3DA9736AF3409A643324488BCCD0E0E3A3819042737DD142A3848
              4745E8A083EDD882430517C1A9A01D840C55171D1C1C34B582A28214314DC421
              3E49D2B42FC9EBBD0E6A486BDEF39FEE39F7F09DFFE75EC95D7BC848267F0998
              43A420A01850A7FA517D5B9E214CD648263F035C0773417776D703AFA6C7B48F
              ED7B48D025F1E32B8EEB86038039E062ABBCF6DA2B2D627A1D5CD7652295D27F
              8712A9540440A4A03B3BEB5E6909D3EB00E0FB3EED76BB1FC5B66D2CCB1A0E10
              88055E4D9B9EDF6F562A957D43AEEB921A70743002EC05A11607A4863587FB1A
              AE08070794CD66711CA75FFF39473890DF77F123C7494C1749363F33D1AA0EDD
              F80FC0808EA5730A65E943E76FC4ADC9A9BBDD46EDA6FFBEA4F6BEBCF93F0063
              36959D38963C73A56CB4F6815630997BDE38395B6CE895EACFB5E548402C79FA
              725B84C578FEE82B658F7F17518F115C60219E9F7E84B25AFE663914A08266FD
              3EB084C88AC4ACAE88689059440AC0BD31F744B483F6BB67C8E8F85B8CB983D1
              F3CA4EDC02D20819E0DCCE8717CDDD4F2F4301325864AF3E6074EAF06D84B318
              4E6D6F3CD9AAAF2E80097F907DFF40C51D1B4863286E6F3CDDAAAFCE83319111
              7E01A34E9DC55C7BCC090000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000024D49444154388DADD4CD4B94
              5114C7F1EFB93AEFF8D2F4A446A50F8C50249AB40971E1F462D6A6ACBF2082A0
              1641D2A25A94142D5A066E244CB46D6E5AD9A2859BB256A611B4A9718434B1B0
              1975C6675E9ED3429286669C093ACBDFE57EB88773EF95A6ABC3F85A3AF78870
              1FB820220D80F07799D8AD6345E2C2AAF6D9472C8137C006705B55E3800A8A64
              D248DE41F2398C93C4B2ACF2A0200F800D55EDCAAD2EA5D7A69F935D8E5117F0
              B0BF3ED023F94C1475917C2EBA2B12290F02FDC09DDCEA527A71E8129A4E0210
              B22CAAFC111BE829AB1480228DA8C6D7A627B631806C364B32991C07C67F671E
              8F87402050AE654441B3CB5F0A161289048944A220B32C8B482432080C963E21
              2022682E53695763C0D48EE03F960D44FF3758725025C170384C73737341668C
              81AD218D17DB5314F41D68C37FB08B90BB8E772336283B0CA024A8AA54D53761
              8275345E19C27883E7CD66622A3B3D32E6F9B93055EC1D9604155D530807DB4F
              B0199B41BCC11A60D8F5D7CDFFECBED6278B1FE33F261EE23AA98A40B11FBD7D
              01E4D5D58BEB3393843A4E623CBE768457289F153DE3CCCF26974707D04CBA2C
              6814EE21F48991273547CFB648B557800F402F42AB20937EBBB3B6F1F2E38A4E
              68D449CDA29C06BAC5989831262F22AE20EF41768B4817F0CC677754067E1BB9
              8EF37DE1B5AAB6A9EA6145A3AA7A5C55FB4157145D01EEBA4EF976E18F8FD4BB
              EF10265043F8DC4D3C0D76ADC04B845694536E2635B7FCF4064E7CAE2CB87D0F
              335F3F515DBF176F831D02260BB0D1CA300053244B01EF507AB7B0019CF9CA30
              805F62E1E5B4CBC9F03A0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000002E8494441544889A5954F6854
              5718C57FE7BE97193226BE90647408D1A860C1C6A49B89A4CB34E9A60BFF76A1
              6E548C75D3A51474A30645D75D2888500A15EC4A5782A5B594DA2EBA680B2DA6
              CDA69110C1A871944C669CCC9BAF8B37D1313398377A5687CB77CFB9DFE17EF7
              AAFBE039D60C8C82F848E80462182390240033B315DC9BFDF210A5D97F88037F
              CDC02892BE00CE033318D780C75531AACA8045BC52C14938E7E21920462271FB
              D68C71A0182ECC539E9BC6968A742B8F977F82CA45542EE10A39D2BD01F464E3
              19089D00660CC6C3C5E7C5F91B1759FCFB47009C73F465B348AAAC8C0BA9517C
              AF7133331FF121C6358CE2931B172954C557E04CACE3368C0802E071B8305F27
              6E66E472398089DA75490441F0B2A6B67E25F785649895E7A6EBDCCD8CA9A9A9
              BA75E71C4343439859B87A44D51359A9D06CF70067572BF0ADB6AF2650DD36B1
              7A44CBB7A24928BA45F1227A131A0D94E779CBF4DD2272CE91CD361EA8778AA8
              65FD16927D833819E811B2B07ED0681C4B2D7F199199E1DABB22F1F42632C72F
              E3A582345679FEE2B7AF5E241FFC7546BCD55DC037580212FEBACD28D14A4BCF
              7BB854D009FC84DCE442FFCE03E5F6CC844DFD4CF9E983E60D309B06065D4B92
              8E8F8F539ABD07664F91EE0247C3B6F4F5FCFB9FEC2F6F1C2E3DBCF2394B73FF
              3565E0758C8DAF070E0377921BFA67BCB569FCF62E70EE96A0073800EA57A2F5
              666AFB4858F8F7572AF95C7C8360ECE8EF429F4A3A2629E707EBEE23B728C980
              5B885E4907816D2E91BA991A18A92CFC793BF6E43B2A956706A3C01FC025A487
              924220044A18472CFA71F622AE7A6D9DF86BBB6377E03FBA7E9AAE7DA7665DA2
              754CD20E6098E8855DC607A0DD6079E06B9A1C7C01781D19DA867692ECD90A40
              A2771B7E7B77242EBEC34801BB0DFBA130F90B73DF9C847029BE412D927D8364
              3EBB843C3F128714C62EC3EE1426EF56C5CBB13BA87B68FC8E0C727E27701B94
              C2D80354C54F35250ED18F560713F3820B98DD33F8FE9578BC586AF13F639874
              DD30F2BD4A0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000002B6494441545885ED954F4814
              511CC73F6F765C77C7D5589D88765524E88F0A6504E1C1EAA0D125E9620A6217
              A34BD74E1269041EECD839E912FE431091C08318941841104117A32E6A68A88B
              4E8E3BECCECEEBE0FA275757DD598BA02F0CCCCC7BBFF97E78DFF79B077F5962
              FB835A1426BFBA01552F4600E2F7E11425A2068B034F5C01A81B37DAF95AF4C6
              764D5173DB8016042502A1A42BB68D853030E71A402D0AA33776688AEA1D072C
              A0554ABE2456971CA4B36771E267E4871BF34D80FCEA0614D5DB0658D249D42D
              8F3DB78DC901A465BAFDFEC100728A8A015A80D6E5B16E7B65FCC5AE9343A110
              8140206BE6966525F7801020448994CE9431D9BF6741201020180CEE9DC92165
              9AA6A202C91D8F92305764BA65B72C0BD334D36ECCC3281A8D6E7501008E9DB6
              607A7A7AD7F79946138FC7770064A84CA3D98CC0AD328D2635820C65DB36B158
              ECD075FF23F81FC1BF1741BA1FCE1F89E0C8CE8283EAE8CF827DB4D759E04607
              0210BE00C79B1EE329D08531D127CD8FA359035000E4FA2595FC228127276592
              AFAC0AADBCA63E377C6E54BFDDEECFBB7823BB00EBF6721E4459DE85BA944989
              D508D271C6008F509461BDB1DD9F57951D080F40EEE9CBE486CE960AB8E93B75
              A92F36F7157B69760BC058C08E7CB7B5CAAB834211CD02A549ABB836684766ED
              F8FC37570002C05B5AC9C9FBDD4101EF804F123AECA599297B7146E26C759DB7
              A4024FA050433082C4968E736BA1E7A1B5F6F9B53B008063D7EF11ACBDAB035D
              40931042DBAF584AD969BC1F7A1419EA720F00E0AFB842414D33397AB18A10BA
              D831AEF80B10AAD70FF4032B40FDE2606774F5C3487600D2C977A69A13779EFA
              448E77185041D6475E3D5B33DEF6666C7E6000351822F4A0CFA7A81BE624CD7B
              5C99C3661BEE33492B4051BDE5404C66D11C926DB89F125103B5303C2F13F15E
              E3CDCBB831E16ED9B7EB176FDA2BA99AF6082C0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000003BD494441545885D5985D685B
              6518C77FCFC9B1696D92AE495336C444287AD3EE620E6765CA205E0C8529CEC1
              C481DB147516BF2D88F54204BD98085A95098254D15E08435027289BBA5D8830
              87182CDB8589A6505C6DED57BEBAD3247DBC681B3A48D734E7BC43FF702ECEC7
              F39CDF79DFFFFB9C8717FEE3925A17ED8E384DB16E44406A3F52538B4E91E2C8
              0F9EC101D8AB4F7CA128917D03B4DCD86B81EC12613B426B9D90B3A5E9BF068D
              01FA4251B63C35841DECD80E0C89488FA25328B93A07710C18F4946E3560E4FE
              8115B8D340525577A8E839162B94E726EB4A56999BF09A6F09D0EE88D37253AF
              050C014994C4C2786A61FAEB412EFDF10B2C563C7FF18600FDB16E963C273DAA
              BAC3F93BBD30FEFEA3E8C2FC9A81E17018CBB28CC2954AA595291644B85955A7
              143D3773E2ED2BC201C46231FC7EFF2B06F932D96CF6631B400410694535072C
              4D6B7DDA65080EA00D562D12591EC6F2F4C5BA3C97CFE7711C2761109042A170
              791DDC8852A9D49AF70281009D9D9D8DA6AE6A9507BD55737333D168F42070D0
              459A5FB3D9ECF34600979501CEB88C373382C56291B1B1B133B803C4711CA353
              EC3A4F3E9F37036859D64A8D7453274F3B8E9330E9C18F58FAAF37AA5930E4C1
              5C2E473A9D1E0546DDE431566682C1205D5D5D71E006176966B3D96CD2E4141F
              C2A5078184C976E455C0727124C08507D76BB7D2E974A3A9AB72E5C1ABDA6EB9
              D0D56BB736AAFF75BBE5A5360CE80B8409F4EE05A0903C4979D2552D5E575540
              0550455A02570C883EF406CDB19E6D402974EB7D23173FE8330A6955E1D03260
              5BFE005620BC66802F1801780D38E50B467A363F760C3B1A370B088A2A7F22B2
              052112BA7DFF9A01A5890CC0832C3594A7EC60A4DB24A4055099CF017C836A51
              90FEB63B0ED0BAEDEE9A0193C3035CCA24E780DD08A3C07726210540EC26AE7B
              F904764BE805118EAAF28CC2312793D4F9D4592A33E3970735B5D07E571F56D3
              B59B10BE45890177967353E7C73DF664755B2870CB3D44F6BE84881C45A41FD5
              1184932839911ABB47AACBDE6593883CA9AAFF2824E67F3F7B61E2C3A73D03AC
              AEE2FCCF5F62B76FA62D71F84594CF05FA507623B4D6FE3401D5958FB4800E81
              AD58BE0B9ED1516303B3E9FA6E423B1F587703D36EEB442D0B41DE03791CF410
              303CF5C59BE47E3A6E0EB01E8576EEA77DCF7308BC2B224754F530F0E9F4576F
              91FDF133CFE0A05A66EA57F0B67DB4EF791681774438A2AA0F9B826B08B0756B
              02415E47E409551E013E3105070DFC8BB55206388EEA7985E1198370D08007AF
              89C609DFDB8F583E0ABF7DEFE982A8A57F01D4FC617D782F2EAF000000004945
              4E44AE426082}
          end>
      end
      item
        Name = 'debug-step-out'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000001D449444154388D8592416B13
              4118869FD99D74E376CD668BB1252DCD21BAC72ADE2D185011FD035221D68378
              F124857AF0E4C58322D8A247F1504110A1425B89E0C1ABD1120F160B554903C5
              D636D558C99264C68325B47193BCA7F986779EF9BE7746B04743375F46A57BF8
              1170EBE7DB9952797E8A6E32F616BA51AF0215E08D3B3A36E49DBFDE15608A03
              07714F65714F5EC4B46398B6BB80C0076E4753232F3064A5BA926F0B1083379E
              1149A4C6814984480B3034E87F2DE95C505A3AB7363DDE7E844822751998007D
              492BD5A3B536D07A0AF417E06A50FAD4358349205BC9CFBD6B6C7F57C07D0417
              D0647E2F2EAC6ECDDEEB0890089156C19FF75BB377199C786E017D666523637F
              CEADF67C7C4DDFD1239D0102CC7A794DE95A95F2DC83A0F7F8996C62A380BDB9
              ACECB8DBF13080D4A04CB7DFC0906AA79063A790233A3C8C88C59A4F6C591652
              CA362368BD6258BD27E267AFE5B75F3D04AD28168BFB4CBEEFE3799E0A07C01D
              2178121F1DCB3AC74E7FA8AD7F5534EA4D436DB304CBF3D0F2E99A80FAAF1F8F
              65EC10083123BD8174C41BD8670C4A4BC62E20B40361C6FB713357B0923E0881
              6831D4D6BFE12D3EC5F3BCD00C5AFDA1EA96C17F4A2693388ED3AC77D7E11984
              6D3A8ED3F6C656FD05F54396C36A90A8FD0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000024049444154388D9593CD4B54
              6114C67FE7DEEB95C646651C6D16A2D6B8280C5D454481D0220317E13F90AE0C
              6D11B50872132191E0A29DA06D44DC848B8C367DB848A10F2C88202C88E80E14
              3433D7019BD16AE6CE3D2D6602351DAF0F1C5E0EE7F0BCCF799FF7089B60D636
              D23CF24804C681692FBBFAE1C7D4305E3A4150185B32D302A801CE000B56B8E1
              786C7002ABB175FF8446A80EB16C0A292707F4203865D28ED8E004550149C58A
              3413E9BBCE81F61388C851A00910847AE01E8A0F9CF5B2AB2BDF6EF7EE4968C5
              8626B1C20DA781291139B6B9A8A02288AADE31C3910B814636C3914EE009F042
              7DFFB0EFFBA6AA1AAADA099A56D565E0622115CC18439051E099AA0E6E7C5E4E
              A89757A003580012404F21EDAC25A786831102DDC0CC6FE73D99F9718CAAEA30
              F0144850F47A34FD652D337D15339FC3B6ED3DC312915A55CDACBF7B8CE63750
              BF9815C3BC6CE473CF6BDFCED6D9C94F5DB1F616A0259042AB74085EE63BC55C
              86F4EC0D421DDDF3357F32D8C98F57046E0662DA34B2828A152DFDB38D9525DC
              B951D65FDE47E056B9277058AA9A025AC227FBC8BD7908C50200AEEBE2BAEE96
              DBA3D128F178BC1FE8AF34F203E09A7DE8C85CECD2E4AF9F4B3314925F512FBF
              437B01C00116772394D6B1D78D02AF801C7097D2575111D9DEBB981DEF251E8F
              B7026DBB2A2CA49C745553DBA9F27B8D894869F5FEC7BFBD1FA082516284EAA8
              3F3FC4C1AE7318D52190D222EF84B2C2DDB84A8415ABDB10D4941D61180696B5
              B55CCE1D2A99B2879A8A06EC4B611903EC7353FE02AE2DC6221A83AFFF000000
              0049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000031B494441544889AD944F685C
              5514C67FE7BD379DC99F3269D326419DA6B6146430B6D144852C6A8A0B15DA5D
              AB452928C4850B4589B850128A2275A188E84A10222EA4BA31DAA21222851A53
              140A4DC8A24148B434EDD4496B12CDBC99CCFB5C24195FA6E3CC0B7857DF3DE7
              BDF37DDFB9F71EA36CD5DFDBCBCE67DEC6B0178109A11F167FFC82F9AFDF2BFF
              34D2726E8BB831C09A81D781AF0CEBDDDA7394ED875FFE7F0882E5051059E031
              C05F2339B8B5E718DB0EBFB2690217C062091AF63F4AE3FD8F136BBE0BAFA905
              27D1780D18C1388E71C2B0B1782A3DEBD62759BEFC5364028BB777D07CFC4D62
              C956C3EC41838781A6D52C000780238825E088E0DCD50F4E50B87A391281D7FA
              ECFB3889863B814F81473664B52EC30035027D48E7CC2CB203C712F5496004E8
              045E406A95E4022E28067C6CAB5E86259E53C1A7F8F7426402CFB07E8CBD0A74
              0838BF92FD1D6FDB1DC8751DC33E02FA848681A7B492CB6786FA29DE9C8BEE00
              388AF81638FFD7C428B7468730C733C33E049E07CE483C1914727E66A89FDCAF
              BF442E0EE0617637D2970445E687DFA56E6F17184D402FF08DE32F1EDB726DCA
              2F4E7E4F4B3E03BB766D8EC02026C8E73333044BF314B257D04AFEA6795B0EA2
              60A161EAAC5F373B3E680A505B1BE1039654DA57C292F024092058CC0290BF32
              C5F5A157A9DBF750C641ECBC751182E2804245C32BBCAF84BD3526F35A769792
              B9E90BE4A62FE0380ED6D58599B9E52AA33B400BC00E2FD94ABCFD3EFCD94BA5
              024110303939795B5F5DD7259D4E030CD638026CF7A9F1B3C03D92D285F9B9DC
              8DCF5EA330375DF527C771E8EEEE4652B196036B3F357EC8E03BA4CF057D28C8
              F9339728DC9841F9E58A044B63A739B02FDA6DF28051E00DCCDE32E8C1DC3389
              3D9D7F24F674FEAB62A3B293CB1323EBB9C19A67E0CF4E106FEF7807F4F3DAAB
              7E1A91AC326F4E86F0402D07CEF54F5EE2CFB1D304F9DCA8A427246D17B892DC
              F59954864B2A2BE43660C02DC9B4783DB11DA9F55D555541F6371ED8DF11A945
              D1E76ED8F6266E9157AD9099914AA52AC6432D2AC52BBEE45A046DABF367B05C
              E57FA9DE948350A181102ECF55C5FF00E6A6B2A4BFE5F1E80000000049454E44
              AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000002E3494441545885ED564D4814
              61187EBE6F776657765456F36F35A82C03E98730431022EAA2A5505122149EA4
              0E1EEAD04D8B3C74B19B54D8A5BA989256D4C12229282DAC63172B0A4AFC4134
              D359C79DE6E77B3B6C2EFEA5BB3B535E7A6060069E799F87F7E1FDDE0F586778
              E22506F6562375DF51906DC19A1AF9B706782088BCB36DB9FE8DC55702BB0EBD
              3086076DEBFBB02B0678DC4C220D841226C95DD9752D724A51992B06E2EA0099
              3AC49C66F8B6957631CECF308FE74860E7C1FBC6F0A070DA09B6F043CA29446A
              790DE4DC2D605C52185B6CD09BBD1990E4340EF618C057B28C9AF13B174CFDF3
              BBA40D78E75F027B2A9179B2299D734F3380D38CB18C35FEDD0DAF7C29B0E3C0
              45C706A4AC4DD870A2318D71CF2B00C3002A85F1F3831D51ED8564EE57C065BF
              0CA01344A964A3C59C184A5A3C6640293B06E6F136011813C2AE9EEEB926D481
              6EC0326344B9A018B9F5AD1218EE1221551055AA2FDBC3EAEB4EE706A48C1000
              D412706AFA599B50FB3B1691983F809CFA5689FB957B206413898AE9FE7635DC
              7BDD9138101B4306301602D197F09BEE652426F9C17D8110001244156A5F871A
              EE712E0EFCEE008B3E5CCCCD1019916524313B85F0C0836FBE82E2E3739F0630
              D37BD315F198817990B056661161EAD155D744FF68602D844221288AE29AB8AE
              EB8919501405C16050B86540D3349E90015DD7A1695AFCFB630D442291C43A30
              34B4F2A1936C34A669460D50F4115CC964F04A8B0EA078906C340B2220806814
              8C152A2555A3B36F1F265428D9686211D8EA24007432A031A3EADC611151C5DC
              FBE77117B22C0B866124AA0FD334A3EB58CEDF8EBC86DB698CF33E4497D1654B
              9DFC24C293AB16B0B56975FCD6792A2A2A72168131F2113F9EDE5083950DFB19
              63CD009E48E9591948CF5AB5803533910F60CC49048B2E24BEADA5482BAF8D5E
              483C520A965C5896C29E9D8A8CB5D651B253A0EBFAEA02F1C271044EE1780A9C
              C2C914B862C08583C8195C9B82B5F07F1DFF8D75BCEEF805F0B1513316615434
              0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C0864880000043E494441545885CD984D8C13
              6518809FB77F5396361BBADB5AD6A525342A201283269E00170F48D444C570E0
              C21145C1F89728D1701049E44A000514D90B1A63F4A25108FFE84913081149D8
              A44096E56765BB1DB65DFA33F37AE86E65B705DBCE34F1394D66FABEF374BE77
              E67BBF0FFEE7785B09F2F73C82BF3B81968B6871DC6DA72948B301339F7C81EE
              D59B01A2E5BFAF0C5FFFFC35ACB111F7CD26F0341B104C2E46E01D11F9C31F4D
              A6E2EB77E10D45DAE106B4208855466DFA5146118EFA6373E7C5D7EF6E9B64D3
              8299C37B290EA5876DCB7A46952C70CC1F4BB64DB26E0D06928B317A17E09911
              AEFB03CFCC08A1A75E048FC43CC81144C2A8F695862FA7DDAEC929F7F7459374
              ADFE9060725140449E039E00C288C87451559D3C8C89C81A55AE80F6E5CF9F4E
              DFEC7FD735415FF5A0AB97F8FACFF086662D17D88F904449031914B42654983C
              ABAA651199A3962E934030ED9ADDDD825D2F6FC6179AB514F80538A6CA4A542F
              DAF96CDD6F9D37D485F803009F88C812BB6CBD81CA8142FA8C9B7E1541FF03F3
              08A696F8807DC029557DBE30F89735F2C3768A572FD404752CEA23BA762B085B
              41DEB76C7B23E8EEECC983648FEC735F30D03B1F813E1179C8567DA578F58275
              63EF06B478A72660C6C26544D76E453CDE8F05F9C0567B93A2BB467FFB86DB87
              76B82A571594CABBF2B8AA0EA37A6EE4E79D75E5003A162E453CDE8D22B2D9B6
              F54D60A7F9EB416EFFE8BE5C55908A65074A0EA030F0FB3D03D4B650D56F55C9
              2ADA9F3DFD35D936C94D11140404CA996BF70D183DB4075FA8EBBA043AFAEF5C
              3E4BF6F09EB6C94D116C147B6C849BFDEFB5C3A52E4D0B4E128944F0789A9FCA
              9BA1542AB52E984824300C638B9B42D3B8649AE68196052758EE8A4A7D3AC1C1
              108F8D8D51281456B8E7534B2E976B5D706060E09ED742A110B158ACD5D455A6
              D4A04E74041E177ABA603048341A5D07AC7390E68C699A6FFFFB04953C30537C
              01FC0FCEA754670E6E924BC00987F1952738D1DB9D1591A8A28F45566D3877E3
              CBB7C0B65ACA9CCFE7191C1C3CE1509042A150998403731E65F6EB5FF804FE04
              AEA8EAB3F9F327AC5BDF6FC76EA13B8E4422241209276E40E545AC36CAB337F5
              63F43CBC14E430E831854D58E58BC56B171B5EFB5AB76F317CF023BABBBB49A5
              525B0027DFC9E3A669AEA8D6E0ADEFB6117F75CF29F107560AEC17E1025E5FDA
              E85D90AB6DF86B5165A89C195A75D7A9AF80E30E044761DA9AC4482EA66BCD16
              FC919EBBD7244683EBFB4C3933B46DF0D397300C837038ECC0AD42A954AA7367
              5F808E457DD5555DCDBFB80F566E94CC4F3B26873809CC75E0376A9AE6D9A6B7
              3E1AC1CD1A6CA7A0E33CA66936BF79D4086ED6A0D36EA62EE17098542AB51C78
              DA411A57DAADFB311767ED5827B4B03FD850E6CE4E7A7A7A1CE7C9E572AD0B26
              12097CBE760E008C8F8F3B5B93188671D44DA1694C6BB75AC351B7F21F5C0207
              35188FC7F17A5BDA836F9842A1D0D6FCAEF00F07938F00F25867720000000049
              454E44AE426082}
          end>
      end
      item
        Name = 'debug-breakpoint-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000020649444154388D8D933D4C53
              5118869F73AEBD9496D24B6202570642B449D9844598DC9848EA821D1C8C38E0
              00ACAEEEC6C5BA38C0A691412721441385C1D481D08568804411944B1313A1B7
              7FB7877B8F036DF849A07DA733BCEF7372BEF3BD8273EA374D52F13803E136AE
              080140A061C3ABB25828F0ADEA9DF18BC6212A254F7ABA19B72C5B0831058C09
              480068D802DE6BAD5F7C705DE7F19EC33FDF3F0144A5E0755F1F372391B40885
              5E9AC323B1D0D02046770F007E7E1FB596A3F635EB6AA5267F78DEFCF8CF6DFE
              FAFE31E0E9359BBB5D5D696959AFA2130F8561DBE75F760C721C4A73B33A3838
              B8B7ECBAF3F7777611FDA6C9A71BD76DC334BF774CCFC42E0A9F861433CFDDA0
              564BA6B77FEDCB54BC1329C494393CD2340C60D836E6F0484C08317D27DE894C
              86C30063A1A1C1A6E186EADEB164388C0C1D7F55A231B05654F7264242205B4E
              5D2019680DB0E5E7F32D87EADE2D5F83DCF03C8005955B6B1950F72E6C7A55E4
              52C1456B9DA965B345DF719ADFEE38D4B259576B324B0517B95EADF2D12D3A5A
              A9C9D2DCACBE0CD25824ADD4A35CA5EC7C2E163100BE944A8CC662EBD6D1D1A6
              5A5D1DD5A5729B884610ED1108027C670F6F7985CABBB7AE2E9727F24ABD79B0
              B3CB61109C94E9AA61F0ACB797DB1DD14BCB94AB549C99DF7FD8550A38D5C686
              6E45DA49C52D06C26DD477045FC3A6E7B1583864A5582238E5FF0F6C0AEAF274
              C9E3050000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000002C049444154388D9D94C16B1B
              471487BF99D5A81BED7AD7C431D871B587E8961C4A948B827D488FB12128E740
              6F2D248142A9A1D0FF20E0404F39943681D4F79840E59E43844A207BE82D3886
              DA692F712475ED5D59F1EECEF41039D8954854FF8EF3E67DCCBCF77B4F304216
              50731C6A4E89D98242006610EBE639AD24A19924BC35662857FCF7A0EE7B7C3B
              3D4D502CFA085107E6816010DE069A18B3F63ACBA27B6FDEF0B0D3251F05B480
              BB7367B9EEFB0A21BE0396A5EB4E142A15C4D41400A6DD26DBDC44C7F11EB082
              31779E2649FAE5AB57F4B4390EFC61EE2CD727272781C7D275E7EDC52554B58A
              B0AC633F30794E1A86F41BBFA2E3B8095C6B25C93F37FEDC221F3C8CBAEFF1CD
              F4B44288DFACD9D979F7D66D0AE7CE21A41CAE9194587373A88B17C936360213
              C70B65555C3D305A3FEBED6359C08FE54FF10B85EFA5EB7EE1DEBA8DF4FD51BD
              3A0EB66DD4F90BA4611898F420FDCCB69FAC76BBC89A5322287EE201CBF6E2D2
              58B04349DFC75E5C025876AD8277D5F390B5920382BA74DD0955AD8E0D3B94AA
              5691AE3B81A07ED929216795025828542A430D1847C2B228542A000B334A7158
              F5B21C58E3241AE49681F7C021839F40E228705BB73B27260D72B7016427CF00
              9AD9E64B4C9E7F286FA44C9E936DBE046876B31CF97BD203C39A8EE3BD340CFF
              37300DC3C3515C6B2509F26992D0CEB20858E9371AE8281A1BA6A3887EA301B0
              92691DADEFEE62E5406A0C575CB765D283CFB38D8D409DBF80B0ED8FC2929F7F
              4277DA4DE0AB079D8E5EDFDB7B37CB7FECEF73A9744A07C5E22313C70B691806
              C27190333343F36CF29CF4F9737ABF3C44773A4DE0DA8B7EBFF7F55F7F9372C4
              2E2529B85F0EA8392525C65E5FDC79F1B69FDED8DA62277BD7D063FEB3809B67
              CE7073EA34AE657D70C1F6B58E56BB5DEEBEDEA17764738F34B42725573D8FCB
              4E8919A5DE5F324037CB69F512D6A35D7646D8EC5F5A3A279F79497429000000
              0049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000389494441544889A5964F481C
              7714C73FEF37AEFB67B675AD9A3528AED02E011B0A6B487BC841C8B9A13D28F4
              52B587DA436A4E52E8A150E8B9345402C58B7A2BE83125879E84069A43DD43AD
              85D842C6348D663746D78C3B9BDD99D783BBCB5A7737D27E2FF3DEFC86F77DF3
              9DC7FB8ED0067163B862DB8C4422C48C41AAF71528ABB2E979DC3B3A2257A9B4
              AC21CD6E0E8642DCE8EDE1BD448288180BE1229001CE571F790C6451362A1AF8
              3F1E1E722B9FE757AFF47282C9EE6E3EEF3F474C8C8DC82C3003A45A34E8000B
              A0F3BEAAFB5DFE295F3FC9E1B722F8EC5C1FD77B7B41640C581663863AD2690D
              6546B1865398AE8402121CECE33F702867D7A96C6D8906C1363085B276BB70C0
              8DBF1ED549EA0493DDDD7C75BE1F443E0016AD64B2333A3E81954AA9880880AA
              D6E35AEE3B8E145757F077775F001F817EBFF8748F2F777601B06A9A2F0C0D12
              32660C58ED78FD8DCEF8CC27989E1E6D214D1D2691A0F3D225F1B7B7ADE0D9DE
              35909FDE8A469D9F5D9747E50A0660B6B787A8181B58B692C94E7B7A1A8944A8
              752B5534C627F248047B7A1A2B99EC04962DB0E7FAFA8E1B881BC3FB890482CC
              8A3143D1F10908875FDAF92984C31A1D9F408C1902F9F4B26D73211CC65CB163
              848D58081F77A4D36AA5525AD357555BC6CDCEAC544A3BD269459811C1BA1A8F
              63462251042E02C3A1CC685B595A4AD4108732A300C3206F8E44229898318064
              00ACE156E37E7634D4188D19434735E907305D89DADB520FDAC4CDCE4C57028E
              C7BF1FA14E508388C889796F15B73AFBF77498EA7507203838389B0E6DD05063
              07C0943500C802F8CE83FF4DD050235B0E14B3E99540D9009CF27A9666E377D6
              3155552DAF67011C948D4DCFC3DC735DCAAA3EB050D9BA2FBEE3FCE731F51D47
              2A5BF7055850D4BFEBBA989CEFB3F6FC10605E8360BBB8BA02A552539F688B52
              498AAB2B5437EBFC1FA512D962F1F823DFCCE5F1555D60CADFDD7DE12E2DA19E
              D756AE13B9E7E12E2DD536EA942AEECD5C9E80EA367D52A91016E1ED58CC41E4
              CFE0D9DEB5CAE66F963530202691682B91EF3872B4B488FFF061755DF3C39D42
              816F7239A0C10F2CE0DBC101DE7DB50B600C696338FBFBF84E33C3D1B55F8A45
              3E74B67183E024418DE48B6492C99ED730882DC2992C53611E55F74EA1C0DCDF
              8FEBC54F11D4F04E2CC65C5F2F976D1B41EAA62F55D3D706D357D4FFDDF3B895
              CF73BB7078AA56DB69B9100E73F59578DBDF96BBCF5DB2C522418B1AFF00C284
              05A7A231DC810000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000413494441545885ED974D4C1B
              471886DF5976BDC31ACBAD4D137E5C0E08F16705293D814BE0E05EA256AA54D4
              0BA80AADC2BDAAD41B39A5D73655D41E5AAAFEA4B139855354F550722010E7E7
              94142B4E8520550504CCE25D51AFD7EBD99D1E0C8E171283A939B5EF7177E67B
              9FD9FDE69B6F80FFBA4835833B650F228A173D94A251145DB33566E3B169E2AE
              6160D1346B07200078DFEFC7878D4174CB721F216404C020802E00C1DD612A80
              2700E639E737FEB4AC473FA8DB886B1A2CCE8F0FD02DCBB8D2DA825E4A8740C8
              6784D237A5701862470784D34D10BC5E008093CDC2D97806B6B4844232096E9A
              0BE07C72C5B2E63E5D5DC3835CAE7A80F33E1FBE0CB5D65341B842289DA0D1B7
              E019180091E58A2BE2F93CAC4402E6EC6FE0A6396573FEF1A5B57533A6694707
              38EFF3E1EBD743AFD61172536C6FEF5746C720F8FD158DF7CBD17518F118D8F2
              F25DCEF93B97D6D7333F670E42D4ED7FD02DCBB8D6D6562F09C2AF525F5FBFF7
              C2380445A9CA1C0008A590CE9E8593DE0C399B9B43C3DE86E90786C1FE2A145E
              0E2000B8D6D68666D9F395D8DEFEAEF7C2388828566D5E82100448E130EC95E5
              1034EDB588A2DC8C653260FB3C4B7ACFEF474F3D1D22944E28A363FFCABC0421
              8A5046C740289D68F648431F0502AEF72E808BC10008219769345AF53FAF24C1
              EF078D464108B93C1E0CA07C5925804ED9831E4ACF104A073D03919A99EFC933
              1001A174F0B4289EE9F73ECFA91240BFE2050819917AC3876EB5E388C832A4DE
              3040C8C8C06EFD70018429058073624747CDCDF7B41BFB5C6FD1CB0DD0584CB8
              2EA1A9E9C400766377359625F7F3242C96A4A050F6796A0E508C1D2CAF7EAE5D
              50D5D1784CEDF770017060DBC9664FCCDCC966C181ED170268CC068094B3F1EC
              E4008AB15399A2971BE049B18998674B4B2706B01B7BFE8FB286A50470C73000
              CE670AC924783E5F73739ECFA3904C029CCFDC318C83008BA689A796F5909BE6
              829548D41CC04A24C04D7341B3ED87B7CBF2CC9584DFAB2A38E793E6AD5938BA
              5E337347D761DE9A053826AF6F675C6D9A0B605AD3F1D4B2E6782E3765C463E0
              8C1D0856AD386330E231F05C6E4A6585B96F55D5F5DED50FD80092391323AFF8
              67A1695127BD1992C26110C1C5599DF9741C2C95BAC7391FFD6475952DEECBAF
              031DD11A63D8628C457DBE19676363D85E590E899D9D2065F5FB2872741DC64F
              3F82A552F700BC7D35BDB573A4960C007E374DA88C99C30D0D7168DA29EBFEFD
              378820A0AEB9F9D02685E7F3B0E6E761C4AEC349A7BFE39C8F5E4D6FED7C914E
              BF707CC5EA1B51147CDEDA8216493A565BAEDAF6DCE4DA1A7ED9F9FBA51E8796
              FF7A42703110C007C1004E49621F41E58B09C06F68CC7E14DBCEE01B5585EE38
              15E31FF9FC1101F47BBD88280A7AEA8B57B3BDC91C806EDB789C2B5ECD6E67B3
              87DE88FED79EFE017C61B86C57AF05870000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000572494441545885ED98DD6F1C
              D519879FF7CCAC67776777ED682D4C6C76491C611069219652010DF9AC6884A5
              A04AF155CB4563A496AAB755CD9FD014A15E50D15C1057AD5AB517984AA455D5
              883491AB7089AD1455C28218BCF636C6AC0BEB78F66B665E2EFC817737764C6C
              6F6FF8DDCC68E69CF33E7366E63DBFF3C2D7DA9E642706E9B42CA2C6D45DF355
              B9E5FBDB1EFB9E00FB9C360652298EB82E07A351E2C6C445643F905A695254D5
              A98AAAF77EB9CCF5258F7F148B4C94CBBB0B78CA75F9496727DF4AC48D20DF01
              068113C00111A99B42550D810F816BC01B8A5EF94FA91C5EFCB4C068B1B8B380
              F7DB36BFE8DECB8944C2127801916111D96FBABAD4EEEBC37A208349A7915874
              19AE54262C14086672F89393847373A2AA53A89E57B8F86EA914BC349B67B25A
              DD3EE013F118BFC964485BD621447E2BC67A2CD27F08E7E831AC9E9EAD3C1FC1
              EC2C957F8D511B9F40C3E006AAE796C270E2E7F93C7F2D2E6EDAD7DAECE6B3C9
              24AF6733B8C69C139137ED6CB6C73D3784F3D4B731A9D4665DEB64522922DFF8
              2691830709F3B35D61B1F8C38848FED9546A623108192F95BE3AE013F138AF67
              33448CF99931E655E7C8D376FCF9E731EDED5B066B024D26891C3E0CA5B21DCE
              CC3C27E01D4FB8EF4C55AABC5FA96C1DF07EDBE6CF0F6649D8F6391179D5F9EE
              6962030348432AB9178931441E7904C410DCFCF01955CD9D4A2426FEB9789BF9
              20D81AE0AF7BBA7934163B84C89BCE91A7EDD8C0C0B6C11A65F7F6A24B1E412E
              77DA16F9DBE158ECD69F3EFB0C6D68D73425275D9713C9A485C8889DCD3AD133
              67761C6E55D13367B0B359079191876351EBFB1D1D4D6D9A007FDC9906181263
              1E8F9D1D44AC4DFFA36D492C8BD8D941C498C741865EE84C37BDD23AC0BEB636
              9E725D2322C391FE7EACEEEE5D835B95D5DD4DA4BF1F1186F7B7B599E3AEBB31
              E0407B0AE09488F43A478FED3ADCAA9CA3C710915EE0D40AC39D019F745D1019
              34F775E95693F04EC8EAE9C1DCD7A5880C3EB9D90C3E168D029CB4FBFA5A06B7
              AA9598273391087BD6A5B3B5B34ECB5A7625C8012B936939E0724C39A0107FD0
              719A01A3C62022FB108C49A75B0E68D269443022B22F2A5F5A84C6349302D65C
              492B25B1581DC3AA1A01A5EEF0FF515DF046C022806EE22E764BEB627EBEFEFA
              1AA0AFCAB2A9240C0B8556B20110160AA084AAFA91BF6E455E03BCE5FB54C2D0
              53F46630936B3960309343D19B80F751E54BA75DF78A3F58F66457FDC9C9D6D2
              012B31AF7EEEFB7CBACE76D5018E2D2D013A1ACECD4990CFB70C2EC8E709E7E6
              04181D5BF2EAEED5015E2E2E82724555A72A63632D03AC8C8D2D7FFF70E5F262
              FD8EAF0E70BC5CE6BD523950D5F3B589715A318B413E4F6D621C553D3F57AD05
              97176F6F0C0830B2FC075FD420B8511A7D03BD830DDF296910B012E30670F18F
              FF5BA0A2F59EBA0970B458E45DCF0B5475C89F9EAE942F5DDA35C0F2A54BF8D3
              D315551DCA55ABC185C242539B3BDAE571AFC4D98E8E5B6DC6E4C399DC7388C1
              EEEDDD59B8B7DFA67AED2AC08B81EADF7F3A33CB54ADB635C04210F071B5C640
              2A390178C1CD0F9ED1250FFBA187B6BDB3D320A0FCD65B54AF5D45558755F5B5
              5F7E32CF5F3628876CB8E198AC54280621C713EE3B20B920377D3A989CB4AD4C
              06934CDE135C90CFE3FDFE77D4DEFB77057851555F1B5958E095F9F90DFBDCD5
              157CAF3DC5F9BD7B891AD38FC8C80E953E867CD5F1973F99E7C25D96D52DD996
              471D875FF574F37034BA23C5A3996A3518CEFF97EB9E7797C85FC15759C00FF6
              74F0A3749A071CC708AC95DF040ED0507E4335D4BAF21B57E66BB5F00F0B0B5C
              2834A7936D03AE073D9E701948B573C48DB337124190384243019329506FC1F7
              B9EE2D17302F2FDEA6BA45B07B066CD41ECB625F5B1B8ED40FE5ABF271B57AC7
              7ACBD76AA5BE003C5F53CC0D4A57DE0000000049454E44AE426082}
          end>
      end
      item
        Name = 'variables'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000018949444154388DA5924F4B54
              6114C67FE7DCD72B084A05DA66486F211488428BFECC3417DDD5A29D1F603E40
              106E5CF659EE57D08D81BB861931046BC0DA082374AB95F80F5AD4BCEF69E160
              DEE13613F4EC5ECEF3FC0EEFC311AE499267C42B6BB340065213C15122337A60
              2DA0513044C95380ECC5E324AD2F568854AD0CE043889A9D3CDDDAED665A98A8
              03A4D60F97652F17A9525FAC00522BB8444004372C7C1D22821BED1C21277753
              A2B9278846E8F43C9313B16EB40EB93939FEC7A41ACDDC987870E7F6D49B782C
              5A2D00E2E5D77340264215C45DFCF8C9CEC1B7813DD653D1F6C3F999C6CBDA3D
              C6C7DC15C401D9F347493D5D1ADEFABB8F797D7BEF28BB3F7BEBD54232BDEA43
              3033BC8254FBE1BFFE3352255DAA108CEAE1D7D3CF3E046B7672C05AAEDF7AE9
              E64108E03E1D1DFB9D83EFBEF490FE4567E71784932F311608F987F2531D263B
              CDF9B5B97EF5FEEF3B5033BC0F61A4B1EFE99915EB52B056B393330CE243A0D9
              C931B33603000734B676BBD9DBF7DD2A48792746CFB036D008F97E61F41BDA5C
              905768E2EF9D0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000022F49444154388DAD94CD4B54
              6114C67FE7BDF78E334DCC0C730D478D686A9391082D26218A920A5C451004AD
              DA196DEA0F3004EB1FB055B6932010AC55907D40E0467291D0C2C2226D61343A
              A38D9A736B9C7B5A34E1C71DCDC17E9B7771DEF7E17938EF39C226CCA1533867
              6E19A01BB82E220D806CBEB70E55D52C701FB86B6FAE5AAD1701BAF7849D9ECE
              4C1A371E4140B75403F28562C3B3B1A99E15AF444050425180AECE4C9A4C4BE3
              36C6D638DC9400E0F1C864970996051149B9F1C88EC4FEE2C62388482A2028B2
              FED83995FB52C5E1EEB0B11CA4F93812AA4474C244C3CED999DC32B1681DC6AC
              F7AA3896498643F605DB32D744241470EA5CBA87249AEA05B98CD02095B0B665
              B0AD8DC18D08758E953D988A0F9D6EDB7F20958C3E171117E0F3D7EF3C78FA0E
              DB249ADB107999D85BE72663E1ED3F1C30BFE831FE69B6777EB178FE4AC7911B
              C95864706364A1EF58DA75AF9E3B8A65FEDD8AB2AF3C7AF5DE9D98CEF54D4CE7
              3B4EB636CF89C8BED5B2AFAA940C487BA6A57147620096114EB4A4F095F62FD9
              45BFECEBA4EFAB8E7DF806E8A82D42C8B1CC9693500DDB3288109AC92DF3F0C5
              4429BBF08385252F0FDC0C4C4A2D2C2C79CC2F7903AA3AA8AA43AC7AF95D092A
              507A3B38802A5AF2F0A74783B35C2BE5F10D4DE6BF4F8A01B4A68EACA15AE5A5
              51D5D95CA15893D25CA188AACE5613B451FA87DF4CDD06A8FFB34CB7B604E40A
              4586C7A640E9D7D55F5504A177E567892723935D9575BF2D9575DF8F70C7FFF8
              3A50FF0DDE11C0F71182A7300000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000307494441544889AD95CD6E1B
              6514869F33FF535A871A47AD908A301562538B864D5D40A40822B6FC09D87003
              A8E5069A05ADD20B40D970032CBA692F2005918555AF88902B2124140B2859B4
              CE4F07D2F178667C58CCB819C79EA48D7857EFF9BE3373BEF3CEF79E11A6C038
              3B8F3DFF35C045E0064853044F5555446494578C475C953E681BB8AACA5D997C
              BD607FF61DC6F1DA45E04EB5E27B8D7A0DC73601058A8F14E38C0FE2944EB7C7
              56D0EF83BE3F59C0F270BEFC1E117E78A1E25FBAFCF11CBE63E9B44ECB100E12
              59BEB5C65610FE6494A749F35CBD86EF582A3900A480625CE4BE6369A35E03A4
              39B5800888E067B21C0D7626A9679525A8AA66BA8E3813FCC0BD9C9616C8DA16
              DDE3E3B7667FBC9F93A76505C400D3C97BF300706CE39B7438248C123C77EF1C
              7B8DA92B22AFA9EA82881C67FC7A3DE196F1EA7B186F7C8171ECE4E8E4000C53
              BD7E6FBDC746EF5F1C6BFC5B88806B9BCC3E7F8C575E9C3975BAFADCB269189F
              8CA93792C87EFB2B80B71096446882B8AAAAC950E5E1A390073B8FA7CA001A09
              D29E9DF117E7E7CE7CFEFAD9D99B96697E5A904800B580371159A9565CB7519F
              C5B1CD5C03C98F339D0FE2D4E9741FBEF3E051B8F2E3CF7F2E544F78575E3E3D
              F381889C28766B21DCA8563CF7CA4773F8EEB319EAD2F933B27C7BCDDD0CC2A5
              5FFFD87CF7A553953B067C0810C7A902910172A151AFE1BBD30D55C645447C37
              3394AA5CD8E8ED12C5E96F2222E120A1D3ED01B42D11BC4C96A3C1B14D447037
              FF096975FE8EE264A89D6E8FEDA0DF07AE96FAE05911EC0E68DDDB208C926834
              4D81BBD6D338F62027E76B24A992A4C36BAA5C030555D2DF57B10E736C192F16
              1311549578F55B74E77EB6F7781BC2EDF2517114E8CE7D74737D6CED7F93A80C
              462E1170F8D52CF2A254053A5940956810A7A509876110A7A832286BC2006D77
              BA3DC22811CD019914077155D570904866286D8F64DE0F0BD5C5ADA0BFB27C7B
              CD6DD46B62DB6636099FCCB4E93CCE7FEEDB413F4275111DA2FD604A01A4850E
              17B6827069F597BF9A80F3B4EA006D5417415AE97A0B767B1349FF01B306CC19
              CA4B45CF0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000263494441545885ED97316813
              611886DFEFFEBB4BD22454A98106AD5A4B9454A143105B1CBAB8985A87EA5A0B
              CEDA4DD4C1C52EBA6A2747E7462891421B0709D4E05010419A80A7484BDB41A5
              C5A4E75DEEEE73482891923F6A92BAE4DD8E7BF9BEE7FFEEEEBDFF2734104562
              10F131400F80A891BBB19801B836DC4206BCF11ED2921489414BCE0449684F01
              5C27A250F3005C0430C7EC4D97338F8AAACC2CE29741427B123FD13395BC700A
              7E5D70B3003F6D37B8F0F6D3D4EA976F1067AFDC944E40BD741FE2F8F99D0737
              8643A180DE6CEF3D154D1B33CF7345EFABD1ADC88C048008E156360780504007
              118509042980FC0D6981080D000E401D800E800A54E376700CD07E8F5B251243
              B84B57E6973FE270D8F7C745FD9AF01D3D12BED8DB137C2C14252105A0C86968
              C9872112DA2C806B4414AC35FCD8B591FBB0F1378B023397FCBA9A1A1D3A961C
              1DEA5B10A23E842A069320A1CD26CEF44E8E8F0C20E0539B8E5BD372BAD23963
              F2F5BB35F447BBEFF6470FBDAAE755A0050060A2DABCD9DE0080804FC5F8C800
              ACB23BF179736759E6552A714BA15635AF8520A2A0693B9614A0ED712B7BA05C
              FD0A0E5AA6E530339718DCFE1C289AE57DCD5FE60C009863D76EFF04F26BDFA3
              F79E65F7AE99B904200560DACB2FB51F60B754DAE4EDF5CAA4196030D829C32B
              2CC233B2ED07E0ED75D8F377EADEFFEFFF820E4007409146652BD4A0BEC2A884
              8369392DED6B5A0E98516AB43E05AE0D00A974CE40AB204CCB41FA8D01805F54
              EBD795EA16321027876FAF14B6B052D8DAB723FA17D5C4ED2D777551EA250050
              FA1210E7AE56F684A0A64F44CC5C3982E797E01959A9F717A553D6E53B4F9FB6
              0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000357494441545885ED984D6C1B
              451480BF37BB4B6C3789D724544E6335495520CD4FA1082184A21421115AB8F5
              C681134242C0A10724AE51C509904002718213124772E000022181D3032A0224
              534A200928B55B85A6C109C58D9DF5EEE360474951C09BDAC639F893ECBDBCD1
              7CFB3CF3DE8C85B03831CCF014269E0201093D303C5AFD0E561609E6BF00DF0B
              394FD4C5397D0E89F71F16E12C302688DD504B05453780B4A2EFE8F25CC1FBEC
              D57053588F3C8F35FCF83181D944572431988C6399C6E7B0E4F9CCE7F21437CB
              DFAB72B2FCED87053BCC4073680C81D787FAE289679F1CC7B1AD86CB6D91BF51
              E4ED99EF4E1436BC17ADBED1D74C98412216208F4E8CA79A2A0790E88A70FFD1
              830093188B50822088108B75844A78DDC43A1C801848384169C696AD39A72042
              D80CB68EB660BDB405EB65DF0B862E6CB665BEDC2CFBAAAAC81EEB8EAA422519
              032232B077C1A88BFDC0D3C8A1E395E62C726B933ED08365E4B1D94C8E9FB379
              6EA70DDB96A1A73BC2509F3BD1D31D7DD718190B2778470CE7F4398CDB3F06BC
              014C8A48E49F8125CF67F1EA3A8B57D7F76E07A094117EEC8D47A79F78707062
              74A8F77C1849DBBA770AE3F60F005F8D0CF6BA0F0D27E9702CBD3D8B7FC70FD4
              FA25973F7E3E93FBE8930BBF9DE975A32FF4F574A66B0A4A3C0570F6F0C16EF7
              99A9114C13FBDADDA90400B399ECF44F4BAB2792771E58AAB5264D75B18D1E4D
              B94D95DBE29E54025446AEE56F022CD58A370208625BE6FFA938961110ECCD72
              0010D48AAF58B5E0B402FC67B952AD7CF665A1F60355D032B45050D1AD027E0B
              81AA2E5CC9035C040DDF491ACDFA5F25E62EFF11E970B6AF1025CFD76FE696C9
              FEFE671E782B28ACB64EF0FAFA061F7C7E291904DB5954D52290065E56AF78D9
              CFCCB44EB0BA8B8FEC90A3F28B2BC1F545FCAFDF83B56CEB04018AEF9FA919B3
              2F77F14EDA82F5D216AC97B660BD18057669894D27EC9CD50C6AB1E4F9CDB3D9
              41C9F351A51836DE545F25FDC3AF2BDC2C7ACD330302552ECC2D033ABBDB4966
              37C41C3B85F3F0739D08E94457E4BEF12377B1F384D128FC206021B746F6DA8D
              35454F062B0B17BD8F5FA92D8871704E4D6392C39DC04BC02410D9EBE5BC16AA
              5A062E016FAA575CF23E9D4657E6430802D811ACD1A7B62FEE957F541B2D5879
              1656F13333E85A36D4B8BF015749217AD69784520000000049454E44AE426082}
          end>
      end
      item
        Name = 'certificate-license'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000029149444154388D75924D4854
              511886DF73CFDCE94E13E928A18E4A42C56850E926661C245D18B4AA8D10D828
              0551A012E22A08DC1481622D046D17686ED2557F10413A05D24274C4BA8B3422
              7F90BAA3771C729AB9E7AFCD38360D3EAB7338DFF3F27DE71C827F6838E2C558
              6D00E5EE43ED20B80DA0217BB400A59ED89C4DDCFABA82E9C44ECE217B8B2AB7
              1B330DE76891CB35412B2ADA8C48075CF5F500001E8B213D3E06B1B9399996B2
              FDD2E292F89C4A218FE19327608543FD766744B0842DFE87256C61774684150E
              F54F9EAE43013F82E7BD56B8319931CD02798F8C690A2BDC98FCD518F41EA51A
              00400380325DC761AA05697595D7150814A667710502A0D5555E4210AC313CFB
              01D9EB28213EDF8172AECAE703404AF6F61A0030A5006055AE6F404979A0ACA4
              845CDF008055AEE47EC036E78833674E6E6FADB168F4C000168D426E6FAD3129
              E7BEA733F9234CFDB40480BED4D020B86916C8DC34911A1A0480BE57F12DF127
              DB69EE1F14BB28DE9D3D831AC3B84F74FDAEDEDC023DFB0F582C0636330DC5D8
              C33863F75A1797B0EE38F91DA48404250420A4D1E8E804F5FB911A1D416A7404
              D4EF8711E90008095142F05B8AC2F97A2AFDB0C2A12B76674470C7113C9914F1
              0B4DBBF1E6A65D9E4C0AEE38C2EEB826AC70E8F2839AE3394F038063BA0BBD55
              956E00039EAE6E104AC1E6E7A138FFA018FFC816E6412885A7AB1B0006AE5794
              EBA73C06008002C0CDB232B496F8EE104DBBEA4C4F136E7E81B22C88E5E55100
              DF88E1B99879F902E967E3844859AA119210527D7A9FD8810B0012826333E33C
              2604C3A542E86A76F62921A40DC05BA51471DEBC264AA9E742A91B96C318A0B8
              CD79FE2BECD1525C84B1BA5ADDD0B4473B9CF7D88CA1C6E319164AF5762FAFF0
              292B9E57FF17E9CB503E4EDA5E4A0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000036F49444154388D8DD34D4C54
              5714C0F1FF99371F3C669C19308040F9B020426B4BDA2E4C4C5AACAE58D536A5
              4D9BB229EDA6B515374D255D191D0CBB264DBB2A954D63CA18BB336C40899A38
              A421065D0C10E382AA40112DF3F9DE9B395DCC303A4E8DDED57DF79CFBCBB9EF
              DE233C353ACD2A26BAF7D2659AB5204711DE07F616C371940BA03FDDCD661F7C
              B9B8446C2B51B65F9EFC68F179B9F8FA6B3478BD8780DF2510A8F31E7C17A3AB
              0B80DCE222D6A51934915847F5D3443E3F7D64E1163792C9FF07A3AFF4D05713
              DE0F4C7B0E1CA8F29F18C1150A9755907FF490E46804FBDAB50C70682995BAFE
              F6FC0D72C5B86B3BB1D7EFE79D70C800C63DBDBD558153910A0CC0150A133815
              C1D3DB5B058C779AD5467F6DEDE3F8F6A42F1C4444FA45A4C71C3E8EB8DD1558
              E9586E37E6F07144A44784FEBE70B012EC34AB010EBBDADAD4DDD1F94C6C7BB8
              3B3A71B5B52970784F757525581CCDAEA6A6E762A5CD85DCE6B2B5A77292A452
              2F0C1673934F2E95C07F730EC0BC138FA32F806A2A85138F03CC3F729C4A7021
              99047452D3E96C263AF95C30139D44D3E92CE8E4CDE4E3024AE0C58D4D12B673
              0F18CB4C9CC5BA7AE5999875F50A9989B300639AD77BD1F5F552CCD89E6455B1
              5539180ECD92CB75D93333FBF21B1BB81A1A90701854C9DDBE4DFAB77132BFFC
              8C3ACE3985E15FEFAF6AF49F8D1258D629000B6FBDC9AE2AEF80787DE724B003
              DD7C206CBF49C7416A6A55135BA8657D9C769C68F7DC5FA4F2F9CA23031CD9B9
              93069FD70772C637F011A1E879FCA747556D5BD5B6D51F19D550F43CBE0F0700
              CE5419866FA87157594125D027C20FEDAD88C831576D6DBBF9D960A15B0A37B8
              0524709C42970C0EE2AAA9D92D22DF1E6B6EA2CEE3AE04BF6A6AA4D5E7AB0746
              CCCF8710BF1F007BEE3AC00C3063C76285FFE40F600E7D01301274BBEB4EB4B6
              968341C3E09BA64644E4A48804ADD9CB244723683A45119902A6ECB9189A4A91
              8C9CC69ABD8C888444E4E427F5F5B4FB7C00B801428681DF30BC28F7151DB563
              3103F820B7B4D4A16B6BA04C292ABABA2A5B47BF5667796919B800E400CB103C
              8D5EAF7D279B2DDC72D030F8BEE52502861B04F60777F0B269B60097005B55BB
              511097C48B45F4AD5AD6CAF4E64350C86A9E1F57FE66C5B22A9F0D40BDC7C39F
              FB5E654FB5B91BD58155DB1EBB9BC9F04630F81DF0C79A65DD79EFE62D96D399
              8ABDFF0100DE52FF238236110000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000048549444154488985954F6C14
              6518C67FEFCC74A75BB745282DA5A594E5AF48AB62F853F440917AE120C642D4
              46F04068D7005E4C0C78F140E285180F728102510C2A52148331A12992426220
              85C4526B69CB9F9692545840DBD0DD9D76675E0F5DD6D97609CFE97D67BE799E
              EF7DBEF77B47C882D24080A6258B58959F8FC28B02DB11790D982780423FAABF
              29340974F4C662447AFBE88CC5A7704936F25395CB0807ED20C8E7C076310CD3
              282F57736E0500EEED01BCC14151CF73516D023E7A984CC6377775737574F4C9
              022670B2F2795E9956902BC8AFC0DA9CD5D5041B1B31172C54FF5AF7C675891F
              38C0F8A58B006D8A6E18883B89757F74F0C8F33238D378B3B0901D65A588C87E
              11A9CBADAFE799DD7BD42C9C29222200928231A390406DADE238E276FD19068A
              9EB5CC5F1C4FF97D6424CD69F805B694CC026439B0CDAA5EA3C18608C814177D
              F50BC1C6085675B502DB40966F2929CE589221B0A62004420322463012011105
              D014FCF1E31C110D463E001103A161762040851D982AB02037174B0C80F56638
              8C159ECF645BFCB13FB7C2F331C36180F522B0242F2F7B05800D949AF3E63DD9
              962720F54D298AED7F6EF9139D705C304CD216A46CC91667E4862180E8A4CECC
              A840840410F50607B3DAE28F27E7DEE01D80A88824B20A0C3A0E4CECA5CDEDEB
              C51D1A7AAA2D8FE10E0DE1F6F502B4A92A779CB1A90263AA74C54641F5B07A1E
              89238798DC39FED89F278E1C423D0F540F8FBA2E3DB158768B8EDF8DA2701E38
              39D6D2224EF389A75AE4349F90B19616014E2A9CFF311AC5F57166DCE4DE588C
              BAA2220A4CB315D890BCDC3ECB8B46B19E5B8AA45AEF31B1F7E001F1FD5F8AF3
              ED3151D54E54378DBA5E6267DF75FE71FF9798724D57E7E7F3D3B2A55886512E
              227D408ED8B658552FA859911A760303243BAF8A3A8E4EB8AB8B511DDC75FD06
              C7A3F733F8ACC9021E8A651888C856318C9CDC8646929D5735D9D1C1F8E5F689
              5D8542582B56A8555945A2E96000CFDBA2F05932B383A75A24C0C1C58B9863DB
              B311F92EB0B62690B7EB43B56B5F17A3B898F1F36D008476EF21AFA111ABAA4A
              DD9B37C51DE85F057CF55228F4E8E8DD7BF885320E79E3CC425616E483C85EB1
              ED50B031927E97BC7205E001F0707C220620D818416C3B24227BE7D8363B4A4B
              322A480BE41AC2A71573017919D86AD76DC2282D9D6845CFD3943DAD40EB787B
              3BEA790A609495A9FD561DC0FB02CB77CE29A32C9065D8ED28994D996D8B08FB
              44C414CBC2696E161D1911F7D64DD168548033C0198DDE13B7FF96E8C8B038CD
              2744722C103111D9976798F2F1DCF2B440FA905F2DC80765A31852A3AA24BE39
              2A0063AD2D9AB37215A8BA2867551054DDF173E78CF14B17717B7A263A515545
              641DF046CDB4693F4F114835EC29CFF3F252EB4D11599FECEE3EEE5EBB662B74
              297A070544FE4A1CFDBA525513C03BAA7A1670551501C77F0669814FFA6FB334
              EF3E802340ED8CE96C2E9E791AA41ED5EF53F6E0B82EB9A679466109500F9CBE
              F0EF30C7EEDE9B2884D45CCBD8771618C0170B17F06E7111026F2BFCDD9F48B4
              DD8EC7593B637A0D48B1AAFE70617898F7BA7B88FB7EF47EFC07D8D70A197932
              D0EF0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000005BF494441545885A5975D6C14
              D71DC57F77767677069BB5B1F1DA29AEC14EB0A9B0212D0EB0D92681A6D82241
              4A442223252DF48182025495A254AA203C444A9454555B51A90A01A9A4B44990
              DA874A1093753E487029C1310BC12424D8710A724DD6BBC61FBBDE8FD999B97D
              F01AD5B0EB0CE63C8DEEFFEA9C7375E7FECFBD82DB802A040F95F858E9F3B150
              F352EC720190B46C063219BAE371DE1F192523A5634EE154787B55153BAABFE3
              F27B3C8F008F03CDC05DB929D7806EE09F63A6D97EF0BF83D61F07AF91B2ED3B
              3750E3F570F87BF52C2D2A6E05B14F14172FF63CF0006AD33214BF1F007B6808
              B3E7024667273291E805F9CB2BE95468F3A5CB7C9E4C39596361F19EE6154483
              8117636B1EB412070F58667CDC2A04333E6E250E1EB0626B1EB4A2C1D52FF6AD
              BA8FC639736627EE11820FEF6D221A0CBC34BCBED5CA9C3F5F50F86664CE9FB7
              86D7B75AD160E0A5F08AEF53AABA0AEA14ACFC62C1029EF0573C2ADCEE3FCDFD
              DD1F509B9A1C9B572A2B51972DC7E808FDD0A728678B14A5F7BDD1D1FC73F30D
              7A8560E782BB54609FB6790B6A63A363F129A88D8D689BB700ECFB6955A53ADF
              AD3A37B0765E29A56EF7634A4949ADB669D36D8B4F41DBB409A5A4A4D6A3288F
              6D282F776E20E8F3016C74AF598BD0F4591B109A8E7BCD5A808D394E6706166A
              1AC04A75D9F2598B4F21C7B1B246F33A37A04C768705AEAACA3B36E0AAAA9AE4
              12F95B4E5E03376ACA4C6587985C4D41A2991422762C76C7FA398E48A174C86B
              60C8C80284CDCF2EDEB1811C47383AC9E9CCC0B9781CA43C6A9C388134CD598B
              4BD3C4387102A43C1A8EC79D1B387E7D84AC9447EC482466B4BF3D6B0346FBDB
              D891484CC291A3B161E70662A6C991C85012D89DDAFF2AD6C0C06D8B5B0303A4
              F6BF0AB0FBBDEB23C92FD3E9BCF30A66413831C153FE8A739A6535981F9F6E74
              DF1F44993BD799F8E02089E79EC51E1E3E92B6ACE7B77C719991025B59D040D2
              B6B99048F07879F931259168C87684968AF9F371D5D6210A9C6969DB18EF7630
              F1FC1EEC58ECEFD2B67FB6ABEF2BEBDFE3F9F71F1C5C481E2D2BE3D0927A2184
              E81542D42A350BF1B4B4A03635A1F8271B953D14C1ECE9C1E8E8C0BE7A052965
              BF94B2FED7FD5FCB3F7F1399913F7F44FD1F16EB1A428827455151ADEFB50364
              4F9FC6E83C49FAAF872193995C80D72B5DF5F578376CC0BD3AC0F8F66D752427
              9EA8D7F57F7C1BFF8CA870BBE95F7D9F271A0CF4255E7F7DDAA5C3B872C58A06
              03563418B08CAB57A7D512870E59D160A07730B0CAB358D766D498B1D7FEEABB
              D5CC75A9BB94CACA5AADAD6D5A2DDBFD09C067C02573F2FB06B4B63614BFBFCE
              AD283BF72EAC999D81065D6373556519B047DFBA0DE19D9E6666D719801010CA
              76754DAB094D43DFBA0D60CFFAF2B279F7FB0A9F9E82065E58B40897107BD586
              8652CFBA75D36A329B251B0E037400A16CF8EC2D1DD3D3D2825ADF300FC4DE17
              162D2A289477FCA1121F0F9795DE8310CFE83B76216E4A45F3E245643299B2A4
              EC4C98E649393191BE393784A2A0EFD809423CB3BCB8E8EE8D150E6F444A6EF5
              205E51FC7EB77D6D90CCF1768C53A790B9174FF6932E808F3E4D24D2A7C6C6D3
              C04933B70D524A8C53FF2273BC1DFB9B6B287EBF4708F1F2EE9A1AF43CF17ECB
              31AC70BB595A34A70EA8B62391F0C42B2FAB395F75DAD33F99A36FDB3EB5FF1D
              1F8C8C12310C5ACBCB42D9AEAE166DEBCF49BDB69FF49B6F24817EC0064CA0A6
              5AD36A17EBFAD7172626663690B26D0E4786FA81D53ED5852E14AABD5E961617
              AD4ABFF1B7E3726CACC4EAEB1352CAD007A3A3448D2C52CA90D57BF9F7C9DFFE
              46668E1D1B03D6F7255367BE4AA5C848C968EEFFB89EBD35921DBD0DBD42F097
              250DFC685E69B310E21D203E66666B979CE9C604CEFCE05EEA74FD3F40B194B2
              F55C3C71F6C9CF2F11B7AC6FE57674E7CA48C9962FBEE4FDEBC3DD52CA75C09B
              9D23634CFDF71F4E3E3ADE027E7C7A74CCB1F86D4301F6DD7337179A57D05631
              FFC6F8DAD2127A9A5770784903DE02415508FF0380B7A5319332FE6300000000
              49454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000007AE494441545885B5987B7055
              D51587BF75EEB9CFDC3C4D6E08CFA020991225D0D488C1B62A71D06A874E2DC3
              88D53A56A8D0075A1975B41D46705A0BB63E66CA48A78A8E9D5A474746B036B5
              1388AD686223D1A08D015A211A425E24B909F775CE59FD23090D86DCDC1BD2DF
              BFFBB7D7F9F6DA67AFFD10CE4305A649455616F3037E6679BD982267DA5A6331
              0E9F8ED0100ED31A8F4FFA1B32B165ACAECCCA64C3CC197C2D3B1B53A41491AF
              012540FEB0A50B6846B51638F46E38CCD39FB7B1B7E7D4FF17B0C8E3E637175D
              C8D5B9B9A6C0F710F989882C0490CC4C95DC5C00F4D429341C160055FD08D527
              14763584C3D6DD478ED21C894E3DE06599419E29B998428FA70CE43911B9C455
              52A2DE6FDC88595181ABB0F02CBF7DF224565D1DB1D7F76037378BAA3681DE36
              68DB8D77B51CE68D9EDEA9035C941160F725A5045DC64D20CF1BA1426F60E3DD
              B82B2B11491E425549BCFD36A71FFF354E47470CF456CBE1E55BFFD5CC9BBD13
              434E0898679AEC2BBB84E95EEF4A305E722F5AE4CAD8FA084676762A633B23A7
              AF8FC1871E24F1C10736E8AA01DBDA7D55E3877C1A8D25ED674C1478EBDC62A6
              7BBD0B409E374B4B5DC16DDBD3860330B2B3096EDB8E595AEA029E0FBA5C0B9E
              9A376FE27EC91A170602DC54900FC84E23272723F8F016C4E74B1B6E44E2F311
              7C780B464E4E06C8CE8AAC4C560C2FAC4901DE316D1AC0F52272A57FED3A8CFC
              FC64F69464E4E7E35FBB0E11B912E4FA3B8AA64D1EF0FAFC3C4464BD8442EA59
              71DD79C38DC8B3E23A24145211D62FCBC926CBE54A1FB03410E002B799095479
              965721A6396580629A7896570154994266657656FA80C57E1F20978988DB5D5E
              3E65702372979723226E90CBE626F9AFC7050C0EA5FD620057F1DC29071C15F3
              E28CC94CF1B0720164126565228D8A9974194F04680160DBE74FF445FD2FA695
              CC363EA0027012C0E9EA9A1AA8511A15F36432DFB8807D9635741201EC964FA6
              8E6C582331150EF559E327715CC0964804E0A0AAF624DE3930D57C24DE3980AA
              F6A0DA7878E85BE9011E8D46698BC56C9497E2FB6B71523879A42AA7B797F8FE
              5A505E8A398E5DDF1F4E1F10E0D5CE2E147D9C68C48E3EB76BCA00A3CFED4263
              115BD1C7FFDADDC3A0E34C0E70C789762CC76951D811DBFD2A89F71BCE1B2EF1
              7E03B1DDAF82B2036879F2F3B6A4FEF12B2430E838B80D83A559996FA1BAD23A
              70A0C0ACB81C232F6F5270D6D1A30CDEB7098D463F56D5D52F7776259E399974
              1127070478AF3FCCB5793989428FE7CF1A8B7F3B515393E59A7711AE9933D382
              4BD4BDCBE0FDF7E384FB3F03AD3A118F75DDD6DC4224C9F4428A47FE90DBCD9E
              855FE2C28CC06C60B7889479565CA7BEDB6FC735AD28695FBBFD04D1679F25FE
              973744551B81955DB1D8F11B3FFA9823295C9E52BE3485DC6EEA969411345D2B
              415E11C30011CCF2AFE0AEA8C0356F1E46EED0D43BA77AB08F1C21515787F5CF
              F74015751C50BEA5E86B5F6DFC80E6D3E39796D14AF90CB5301020E872B940B6
              BAE6CC21B8ED31E27BF710AFA921F2D493E71CA83163A6FAD6DC82E7861B18B8
              F7A7D8C78F3F82F27A4566A69D2A604A72016F955D4A67E5D2755DCBAE70226F
              D5DAA395686FB707763E6D77562E753A2B973A03BFDB6927DADBCFF244F6EFB7
              BB965DE174562E5D7BA8FCCB490FA9670D3215D3EA5088928C4026B0D92C5BAC
              EE2B2ACF0E52503052C8FB807EEDEDC5282838CBE35EB60C735199029B431E77
              70C38CE4FF6ECA8001C3E08139B310E43E318C42FFFA0D63EEC2AA8A555F0750
              03EC4BBC573F268E88E05FBF01318C692272DFFAE9D329F2B8CF1FF0C7338A28
              74BB67011B3D55D7AAB960C1188FD3DA8AD3DE2E403550EDB4B589FDD967637C
              6649099EE5CB15B8DBE732663E307BD6F90116793C6C983E0344B68AD7E7F77F
              FFCE73FA8633A6A8560F436A6228A363E4BB732DE2F5FA41B6AC0E855818084C
              1EF081D9B3F0B95C4B04D6F856ADC2F8C2FBCB19C0FA7A54B545E198AAFE4755
              8F58F563A719C055380DEF775621F05D1159BC756EF1E4004B030156870A40D8
              2E7979E2BB79CD397D1A8F63351E04A8EE482468080F0054271A0FA289C439FB
              F8D7DC82E4E60AB0AD32278BAADC9C7101C7AD839B8B6723C88D22F2754474E0
              E70F8108B84CCC4B2FC577F31A4404ABE943884605A5BAA6A797B6788CF2ACCC
              6A22911F5A4D4DEA5EB2045525FA8717B09A9AC0B640150C0311B91AE586CDC5
              C57BDF3CD5981EE05C9F1F846B5475BF7677E37477BB19DA79B2AD77DF59A8FD
              FDEAFFC15D23D31B55B4B6B6AF97D6589C7B66CDDC0FC412F5751E73F162223B
              7E4BEC4F2F8AAA1E02FA19BA508CA4F79AF97EDFDEB433D83030406B3CB61186
              B639AF61602214793D004FC45EFCE38FB02D4D343400FC0338BDAFB78F3ECB62
              D0B60783A6F9B6555F7755241E27F6CACBA2AA4FAAEAC68E7882983A58AA9C48
              E16938ED27E02DC5735837BD0811D92E22F700A8EAA683FDFD8F5DDBF41100BF
              5F309F6FE65FB049C47814401DE731854D7BBABB59FBC961D2B923A6B4938CD6
              CF3E3DC6D39FB7E1A873AFE338BF6288B0BAA6AFEF8CA7F6542F3A546E50477F
              A9AA9B5EEBEC4A1B0E52380F9E4BFBFAFAF0A8727976F6DF801E85BD8F1E3B7E
              E635BFDBB25857547412E8017EB1ABBD9D8D47FF9D36DCA40101FEDE1FA6CFB6
              98EF0FD47724E26C3DD63A7C95867EDBA6322B0B90FA173A3A78F0D36367DAD2
              D57F012B5C4217FD6E09E90000000049454E44AE426082}
          end>
      end
      item
        Name = 'gears'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000002C549444154388D8D924D6863
              6514869FEFBB37496F734B084DD38E29133AA80CD64D4349A55D18DC44518214
              4A0591E22C54041792755775256E84220CB401B1304468DD440425BB6CCA08DD
              1833916E94FE841BF247D29FC47BEF7163717E16CEBB3EE779CF8147F11C999A
              9A626E6EEE4DE015DFF71F68ADDF01068EE33CD0CF03304D13E0B652EA2BC330
              4E9552F7813BA66962FCDF723299647A7AFA05ADF5D7C0CF22B226224A29F5FE
              D8D8D8BEBA190C068344A3510CC3A0DD6EE3BA2EF1789CC9C9492CCB6A28A5E2
              22B2D4ED761F4622913B5AEB63C05300B1588C643269198691079A83C1E0BEE7
              794422917BC06DE05BE000A889C8174AA9CF80B744E43513B869FE3593C9DC3D
              3C3C1C02AF025896F5E1F2F2B2552E973F00BE04BED15ABF0788886C763A9D86
              F9D8BBB5F9F9F9BB994C26D4ED763FBD01379B4DCAE5F26FC05F40DEF7FDEF94
              52EF0257AEEB6286C3612626264CE045CBB2C866B3341A0D00666666D8DFDF07
              488AC82F5757573F398E836559BB2282E338180B0B0B6B5AEB4F52A9D4DBF97C
              9EEDED6D3637379BC562F1F2F2F232BCB1B1C1D1D1D174A3D1980C040261C330
              7E3F3E3EA6D7EBE1BA2E6A6969C95D5F5F572B2B2BCCCECEB2BABAEAF8BEFF32
              80D6FA8F838383F8C9C909954A8562B1E85F5C5C04AAD5EA7F8E00CAB66D42A1
              109EE721221EF0B78820229EE7798442216CDB4629A5782A5A443EDAD9D9D92B
              140A24120972B9DC2DA554556B5DCDE572B7128904854281DDDDDD3D11F95844
              9E2444A35152A954309D4E3F2A954A9EEBBA5EBD5EF7EAF5BAE7BAAE572A95BC
              743AFD68717131F8AF544F6ADEE97488C562A36834FAF0F4F4F4A54AA542BFDF
              07E0FCFC9CB3B33380A3E170386AB55ACFA8FEB807AFB75A2DB6B6B6DC7EBFFF
              3D806DDB6BD96C3600BC61188602E4698001303E3E8E6DDB7BF57A7D62341AFD
              787D7DFDF97038FC0118D56AB53F81F55EAF3768B7DBCF5CF00FB88B2FA32CDF
              5C260000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000040F49444154388D95934D6854
              6714869F73EF9DC99DCCE43A7F9208896D92458B36450413D3450281741137A5
              50DBBA9012082D56B3110A22B869215974A85D1A855608B68CE9222A9448A449
              29EDA66A694A16D5D1C11966243F4A66A676A6D7EF9E2EB4C5120BFAEECFC339
              E7E5119E33BDBDBD02BC0F148320B86459D616E063E09362B1B8E23C2FF071C6
              80172CCBAA0002B403674464C57A56422412A1BDBD1DC003524018F819F84B44
              04684F24123CF3866D6D6DA4D3E941E0A0886C57D50155FD4155239665FD024C
              343737B7C99343F1781CCFF3B02C8B5AADC6DADA1A9EE7914AA588C562442291
              691139A0AA06705757574D2291C0719CAF44E46DC00880E338747777E3795E08
              F84044EA41109CCEE572747676E238CE7B405255BF169111119952D58F543503
              BC22228BC0F7C0110148A7D3747575ED04CEC762B1976AB59A01DE00FE7CFCAB
              D996969650B55ABDA5AAEF8AC8E722D2AFAA55200A1855EDA9542ABF3F594AAF
              6DDB2F9F3D7B96FEFE7E5B442E5996754544BEEDEBEB0B4D4F4F130A85BA80D7
              80EDAA7A1D38079C179190886CAF56AB8F4A111154F59C31E6D38B172FC68F1F
              3F4E10049A4EA7595B5BC3B66DB2D92CBEEF6F88C869E03B55350F1F3E5C761C
              C756D519E08AAA62B5B4B4B075EB5644E480EBBAF181810152A914E17098A5A5
              25C2E130C96492C1C1415CD7DD02BC1304C152B55A5DCEE7F3DCB973C7148BC5
              6FF2F9BC96CB65EC5DBB76ED0C87C37DC0E4E8E8687C787898B9B9390E1F3ECC
              ECECACCCCCCCB06DDB36F6EEDD4B10045CBB76ED5511B9D5D4D4148AC7E3AB37
              6EDCA056ABF1E0C103002CE027D7752F44A3D117474646D8D8D8607272927ABD
              7ECC18D35CAFD78F4D4E4E52A954D8B76F1FD168B4D375DD0BC08F96B5D90B4B
              4462994C46E7E7E7B5B5B5957C3E4FBD5E47554F1A63EAAA7AB2D16870FBF66D
              5A5B5B999F9FD74C26A322127D24C87FE3A86AFDC489134D4D4D4D4C4D4DD1D1
              D1412814C2F7FD83A150E80C70D0711C3A3A3AB877EF1E636363341A0D54B5F1
              34A32C60787D7D7DB4542A952F5FBE4C3299647C7C1CDBB64F0155DBB64F8D8F
              8F934C26999B9BA3542A95D7D7D74755F5F520083613D3E9343D3D3DECD9B3E7
              C8D0D090C9E572C618630A8582595858308542C118634C2E9733434343A6B7B7
              777CF7EEDDA4D3693CCFDBC4B3FF69271E8F5FF77DFF43D775233B76ECA0B9B9
              99EEEE6E1CC7A1D16890CD66B97AF5EA86AABEB3B2B262EEDEBD4BA3B1F96A07
              20080244E44DDBB693FBF7EFD7898909161717C5F33CAD542A323030A0478F1E
              259BCD6EF17DFF80E3385F3EED7FFF021FA7688C593B74E850AA542AA1AAA3F7
              EFDFFF0370171616BEB879F3A6F8BEBFA1AA85FF8301D800AA4A229128589675
              A65AAD8679A4D167A5526939168BFD2A22A652A92C036F0541F05BB95CA65EAF
              3F15F8373355D31371A47ECF0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000005284944415448899D945F6C53
              F715C73FE75E1CFFE19A7163C70B2841C18E36E60C278A8BDA87AD935F10A54C
              1B8BB4754C7B98C4A4A0457DD8C326F2824C79803DAC938A1012685A261EC603
              8CAD8D911A958198BAA9A893A6AA77EEA238900D62E4262106FBFADACE3D7B88
              41A31993CA57BA2FF777CEEFA3F33BDF7384E7D4C0C000894422044CA86AC1F7
              7DC7300C44E4C74009786F7E7E9E4DCF0BE868AF889C02BE6B9AE62C50030E01
              55554D01EE7303C2E130AAFA01E002D9CEF75805C00D87C3189FF7E29E9E1E76
              EFDE8D655988C82F4424025C52D59754750CF8373006E4128904F27901FDFDFD
              F4F6F6BE2C22AF03AF002B40AADD6E7B00A6697E4B442E039F007F7FEA894CD3
              2491486059168661D06EB7999F9FC7F77DFAFAFAB02C8B60308888EC040E76D2
              CAAAEA3D78F08070384C241259E8FCFF3230F804108BC5E8EFEF27100898C021
              11315475CAF33C1A8D06DBB66D43440E0161E032F015E0F74006C8C562B13F01
              86884C00A8EA0911991200C330C866B388C84BC05BC0E87A8CBE0CBC0F202259
              E02F22B249553F52D5D745240EFC0E58EBC47D514476A9EA3C90ADD7EBAB9F6D
              F21F6DDB1E3D76EC183D3D3D2222974564414416807762B19879FCF8718DC7E3
              5F15916955FD10680201E01BC02E5505F887AAAE562A950D7330E379DE6BD96C
              96F1F1712E5DBAD4934AA500989B9B636C6C8C9191111A8D06C00CD007ACAAEA
              CF45E40F401D7807B080B0EFFBEB73D0D5D5F5A402D775BF7FE5CA153D7CF830
              FBF7EF47555544A4F3AE7AEEDC39A9D56A025C04FEACAABB00B75EAFB7BABABA
              304DF355604555B5D96C225BB76E25168BD1DDDD1D1111676060A0FFECD9B36C
              D9B2455BAD96388E03403A9D26100868B55A95F1F1716EDFBEFD2F20DD6C36EB
              B55A8DE5E5659ACD26C16010805AAD86EBBA98994CC68A44224911F991881C3C
              79F2243B76ECD062B128478E1CE1E2C58B140A05B97AF52AC3C3C3D2D7D7C7E0
              E0208542612B50374DB3120A851AB66D37171717A956ABD4EB75DAED360006F0
              371171803752A9946632195AAD964C4E4E522E974BAA7AD0F7FD6F97CBE5D2E4
              E424CD6693E1E16192C9A4026F888823221F3E76E367B549440673B99CE67239
              B66FDF8E88E0380EF7EFDF17E0A7C0DBBEEF639AA696CBE52B8EE3303232C2D1
              A347595C5CE4DAB56B7AFDFAF554C73D1B01AAAAE17098EEEE6E2CCB4255B5DD
              6E8BAE6778AA4A27D90368B7DBA8AA46A351F13CEFF1D2A3E3838D00A05C2814
              B64D4F4F6B3A9DE6FCF9F3323434846DDBACACAC9C320CE341A7F453B66D3334
              340420F97C1EC771E8386CF1591518C00BAA9A03DE2C168B323B3B4B2814D27C
              3E4F341ACDB03EBD7F8D46A3997C3E4F381C6676769662B128C0AF5435A7AA7B
              3A36DE0090DEDE5E6CDBC6B2AC2F88C83FD3E974FCF4E9D31A0A8578F8F0A1DC
              BA750B803D7BF6108D46B5D168C8C4C4048EE37CAAAA5F6A341AAB954A8556AB
              C5D2D2D20680F9E8D1235CD7251E8F7B40796969E93B814080D1D151090683EC
              DCB95393C9A43CF6F7D4D494CCCCCC88AAFE04F8E0CE9D3B542A155CD77D660F
              705D171141445E0C0402ECDDBB971B376E303D3D4D3299148052A9C481030764
              DFBE7D5CB870413DCFFBBAAAFE766D6DED7F5EFC14E0BFF403DBB6B55AAD72E6
              CC19161616EA376FDE7C72582A9522274E9C201E8F73F7EEDD1F02E3A669FE5F
              C253EB1AF89E88FCD2308CDE8E3DBF09BCD789FD1AF0AE6118E2FBFEA7AAFA33
              E0377373732C2F2F3F1360C27AF743A1109148E463E0D7800FDC04CEADACACB4
              575757D7366FDE3C2F227E67451F02DEAFD56ADCBB770FDFF79F09F80F6F3A51
              AAB073DEA50000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000085A494441545885AD576D4C54
              D9197EEE9D3BCC9D7B1966066606E4434001F99062AD5CA83580517613096A54
              6A75C7824A3F88513729B66BB6354DB44DA3D66C6CACD9AA35F24769AA896091
              AD5F0D5AAD21464D5094BA02F2E10CC39D8B083377E6CE99D31F9489B36B7671
              9727B93FCE7DDFF3BECF39E7B9EF7B2E835904C771484E4E86C16040381C465F
              5F1F0821B0582CB0DBED00004551303A3A1A99A39BADE456AB15797979FAB8B8
              B80F8D46E30541107A6262627A28A5C8CCCC644551EC301A8D9BAD56EB43B3D9
              ECF67ABDA094CE1E81A4A424984CA67F969696FED4E9748AB76EDD920441B025
              242454E974BA9AC58B17AFDEB2654BE6C3870F7F02E0DF5EAFB757D33470B345
              80520A00DD191919CBD7AC5983E1E1E1797ABDFED78220E0C58B17A8A9A98128
              8A387AF46800C07F672B6F04696969902469C3AA55AB482010206FC399336788
              2449778B8B8B599EE701CC82060C0603789E477A7ABAC0715CDBA14387625352
              520000AAAA626C6C0C82200000F2F3F3F1E0C1836497CBE56259F65E3018FC76
              0462636351585898EA70383E6059B6AEBCBCBCCCE97402005A5B5BB173E74EAD
              A9A969ECFEFDFB427979F93451B4B6B61608821072381C49DF8A802008484848
              F8A8B0B0F0F76565658BABABAB91949404AFD78B5DBB764DA8AA5A04E06397CB
              359765D9454B962C81DD6E87DFEFB7E4E6E6AE7AF2E4C90F6743846C595919A6
              570E006EB71B9AA63D0B87C39F8F8E8E223131F1CAC0C0401D00300C839D3B77
              0200CE9F3FAF7BEB0ED86C36E4E4E4306969690B6C369B2CCB32C2E170D4CA17
              2E5CA8B7DBEDF3198699DBD5D55579EEDC39F03C8FFCFC7C98CD66B4B7B727FA
              7CBE0951148D0CC3EC773A9D89D9D9D9080402D8B06103CE9C39035555E52802
              8220203B3B1B0E876329C771E7799EFF1DC3303D3A9DAE6B6C6C2CE297959505
              A3D1789CE7F9BF1242C643A1509DAAAA6D1E8FE7476BD7AE05C77158BA7429A3
              28CA7B66B3F9C74EA73371F5EAD560180657AE5C415B5BDB759FCF57AA69DA21
              E64D0279797930994CA7121313EB76ECD80187C3818686861784903C55557996
              658D945285E7F9029BCDF69FA6A626B6A5A5054D4D4DAADFEF6F0450535F5F5F
              BE7DFBF6B79ED5CB972F515F5F1FF67ABD6583838377868787C1BEE9C0300C03
              60D5FEFDFB51595989A2A222545454CC6559D6E57038E4D4D4D441B3D93CC130
              CC8D868606363E3E1E757575A8ADADE5012C07F0878B172F22140A451D19A514
              9452DCB871035EAFF716A5F4CEC8C8C897196664644092A49FEFD8B183844221
              4208211E8F870C0D0D45C6C16090747777134DD30821848C8E8E92152B56A892
              24E54A9274B9B9B999A8AA4A6A6A6AC8C68D1BC9D6AD5BC9CA952B497B7B3BF1
              F97CA4AAAA8A4892543A5D1BA2BE02455160B7DBCF767676FEF1F1E3C77C4141
              01E2E3E3A348EA743AE4E4E444C6CDCDCD78FDFAF5BF003C0170EFD4A9532B1E
              3C78A0EBEFEFFF0CC02F29A502C3308E63C78E5DBC76ED1A3C1ECF734A694450
              1111320C83ECEC6CE8F5FAD3D5D5D58BD6AD5B87A913F96A646565E1E6CD9BF3
              5FBD7AD543293D1A08042EF6F6F61A29A5BF191C1CEC7EFEFCF990C562E90904
              02A4BFBFBF3D1C0ED7CAB2EC96651994524432C4C7C763FEFCF995191919ED4D
              4D4D8889898924090402E8ECEC84D7EBC5BC79F35050501045EEE9D3A7D8B66D
              9B871092F5F2E5CB8970380C4A295C2E1728A510040156AB159452BC7AF50A93
              939391B95C565616188681D16804C3301F6EDDBA352A79777737F6EEDD4BDD6E
              F70D00FD007E5052529273E0C001C4C6C60200162C58808A8A0AFBF5EBD73F30
              9BCD9F06834184C361188D46F87CBEC8F336B0F1F1F1D7AC56AB9BE779774C4C
              CC7BE5E5E511E3E4E424F6ECD91376BBDDEB82C160E5E8E8683D21A4E0EEDDBB
              7F3978F06054A0CACA4A0038228AA2DB6AB5BA131212F658ADD6AF3D42949494
              84655926B22C134551A2DA674B4B0B9124E96F454545D0EBF500005114B164C9
              12636969A922CB72C437180C92E938972E5D2292245D98EE8A5F050E002C16CB
              5B8D2E970B001ECBB20C4DD322BBF2FAF56BBFD96CEE1B1919F9CEF45C9D4E17
              89238AE28C043C4D803E7BF60C00A0D7EB919E9E1E31A6A6A602C0F7121212A6
              1B0C445184C9648AE3382E6BCE9C3911DF40208081818108F1FFDF90BE9E00A5
              F4EC962D5B160200C771B9972F5FD64F8BABA2A202292929AB8686867E565050
              F0E9F8F838AC56AB9165D953D5D5D582C9648A04EAE8E8C0BE7DFB6400C30028
              80B6999060BD5EAF53519445AAAA2E0A854297AE5EBD1A311A0C061C397204D9
              D9D97F8E8989E9B2D96CFFE038AEAFAAAA6ADDEEDDBBA302B5B7B703C02F2627
              2717298AF25D59964F2A8A32A35D003055078A8B8BBF5F5D5D4DC6C7C7A3C4A8
              691A79F4E811B979F326191C1CFCD25DAFB3B393949494F41617171B0C06C3CC
              93E28D4AA8AA2AAC56EB60301874F4F5F5152F5FBE1C2C3BD5AB188681CD6643
              5A5A1ADEDC7660AAC33536368627262636CBB2FCF4CD9F8E9920D20D29A5D322
              FA55474787BFA7A76746015A5A5AE0F178DA017C362DC26F440000E2E2E20060
              FBB265CB8CF9F9F900A656D8D5D505BFDF0F4A291445C19D3B77400801006CDE
              BC19168BE57D0085332A3C5F40543734994C2C808F6B6B6B23EF0E1F3E8CDBB7
              6FF70298C3300C4F29F5020836363626AD5FBF1E2693099B366DD21D3F7EFCB7
              8220AC7F5702EC17C6610017F6EEDD4BDBDADA70EFDE3DDCBE7DFB734A699EDF
              EF175555D5A9AA6A07F0FEC99327C9C8C8084E9C3881D3A74FFB005C7ED7E400
              1055AE4C26133233336130188A1986F944AFD7976A9AB6D1E3F1FCBDB7B737E2
              979F9F8FD8D8D83FE9F5FA064DD3CE524A3F9A9C9C1CEAEDED85DFEFFFE60480
              29C527262622393999E1382E3D140AF575757521180C467C4451446E6EAE8E65
              D9945028F462606000EFAAFE69FC0FB2BCF4ACEF9596080000000049454E44AE
              426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000AC1494441545885BD987D5054
              E7BDC73FCFD9759765D9857D4179115D1603CAAC356602494B91B626B557AF6D
              AC095E69C7114DB93126B9D5CE3433B6192665DA8976FA4226AD2F1D9AA04123
              C46ABC6971BCBEE436D32026D5A015040C2F9A5D02E17565818573F6E91FB25B
              086AC8A8FDFE779EF33DBFF37DCEF33CBFDFF77704FF06CC9C3913B3D90CC0F0
              F0309F7CF209004208525252D0E97400F4F6F632303030E959DDBD1466B55A49
              4F4FC7E974DACC667389D96CCEB15AADEF5CBF7E9DD1D151929393494C4C4C30
              9BCD7F349BCD0687C3F10F8BC5C2E0E0209AA6DD98C4BD149895958510E207C0
              CF67CF9EEDF0F97C8442A1A59AA6BDA3AA2A06830121C4BE84848482FEFE7E46
              46466A81FF1E1818B8D8D8D80880722F05021660E7EAD5AB1DFBF7EF67F9F2E5
              0007753A9DCF6834F609212E0B210A4A4A4A3870E0002929290F01C542FCEBBB
              E9EFA53A29E575214455737373BE5EAFA7A8A808ABD5EA4C4F4FC76432D1D9D9
              69D5340D8FC7437373333E9F0F60B794F2DF2370747494A8A8A823172F5E5CD3
              D0D02033333379F6D9676FCADDBF7F3F9AA6F980BF8E8E8E46C6EFD9123B9D4E
              8C46631CF0EBD5AB57CB050B16DC96BF69D326E2E3E393801287C3C18C193380
              BB7C8AF57A3D6EB79BF8F8781C0E077ABDFEC7C9C9C9FFB963C70EF4FA1B8BD5
              D1D141797939D5D5D5F8FD7EE6CD9B87A22898CD669292923871E2C483C01F63
              6363076D36DBDD5DE2B8B838EC76FB02E015C00864AE5AB50A83C100407B7B3B
              454545F8FDFE0F80FA63C78E7DFBECD9B371252525082158B26409898989868E
              8E8E93D1D1D1BDC0F97BB107BFEE7038BE969F9F0FC0A38F3E1AB9515E5E8EDF
              EF3F25A55C068400CFC993273F2C2828109999990821D8B2650BADADAD193E9F
              8FA3478F7EF95E0814717171AC5BB76ECA0DAFD70BF08EA669A1F6F676DC6EF7
              3F804FBD5EEFCCCCCC4C00727373C9CDCDE5FCF9F31C3D7A54DC56A0DD6EC7E1
              70A0280A6363635CBB768DB1B1B1293C87C381C3E1C068340284BABABA282E2E
              06A0B0B01097CB0540464606172E5C785CA7D395BADDEE41E03F144599999E9E
              1E89555959C9A54B97E8EBEB434A19BAA9C0E8E868E6CC9983D56A3502FF03E4
              004F03DE969696495CA3D188CBE542A7D36D0316017FB87EFDFAEF8E1F3F6E00
              BE959C9C9C52545404C0FAF5EBA9ADADFDD2D5AB573F02DA84100FAC5BB78EB9
              73E702A0AA2AE5E5E5F4F6F61E03AE0117A794BAD9B36793909080A228AB805F
              DAEDF6548BC5427B7BFB1B52CAEFD5D7D7130C0601D0348DB4B4346C365B8E4E
              A7FBEBFDF7DFCFB973E734608F94B25808B1D666B395BEF1C61B58AD56E08659
              3875EA143D3D3D2C5EBC98850B1746DE7DF8F06176ECD8E1075C7EBF7F20180C
              4EADC50F3EF8208AA21C9E3163C6B7F3F3F359BF7E3D6D6D6D141515C95028F4
              15E03C308B1B49BE07080821CE2E5FBE7CF10B2FBCC0993367282D2DA5ADADAD
              4F4AB94A08F1FB9C9C9CCC975E7A29926A6E86C6C646366FDE4C2010F8512814
              FA6D5D5D1D6363635313B5104201F2366EDCC833CF3C434C4C0C1E8F87A54B97
              0A21C4314551061545B9AA284A8BA228FD42089FD96C5EBC69D326001E7EF861
              76EEDC495454944D0891016C7BEFBDF7447373F32DC5015455551108043E065E
              FEF4D34F237B7DCA94C6C6C6420683617B5555D52F9E78E209A2A3A3811B993E
              35353576E1C28524242448BD5E4F5F5F1F4D4D4DF1168B05A7D31989B177EF5E
              4646465A807DC0919C9C1CB960C102DADADA78F9E59799508B9152F2FCF3CFB3
              61C3064E9C38313B180C3E1E151555198E3545A0D7EBC5E57295F6F4F43CF5FA
              EBAFCF096FF0A4A424366CD830899B989848383D84D1D6D646555515C056C000
              3C1A1313C3E0E020BB76EDA2A6A6E6724D4D8D0F88063A8115B9B9B97AA7D389
              C3E1C0E7F3AD1142DC5AE078AA300081BABABA5B2EC9AD70E1C205545595C0D0
              B89B79ECD8B163BFAEA9A949F3FBFD12F8BEAAAAE7C37E505194D2175F7CF199
              402080A66947A4945B27BA9949B5D864329196968610A27CEEDCB979A5A5A561
              C1D346464606C3C3C3E2E2C58BCB81D781734288DDC160B00F3822A5FC735353
              13D7AE5D03C062B1D48C8E8E9A42A1D04F81ED7EBF7FA0BDBD1D555581CF38EA
              D4D454E2E3E3BFA9D7EBABCBCACA989840C35055952B57AE30323282CBE5222E
              2E6E0A47D3348A8A8AA8AFAFAF9052AEEBECEC8CF41D137B1245514849494151
              6E9CD5FEFE7EFAFAFA26C51276BB9DB0831D4FB8A756AE5C99B76DDBB6494429
              25870E1DA2ACAC8C81810121A5943A9D8EA54B97B275EB5662636327F1CF9D3B
              C7E6CD9B43C0BCEEEEEEF6703334363686DFEFFF9C759820302B2BEB7E206B5C
              649410E237656565E2B3FE6DF7EEDDBCF6DA6B43C036296505705D08910BFCCA
              ED767F69CF9E3D91CE2D3CA1828202DADADA0E00FF3F3EDC2BA5FC537373B3EC
              EFEF9F9640BD10E2D4AC59B362C37BCD62B1307FFEFC49A4969616F6EEDD0BB0
              464AF917AFD7CBD0D010494949A7626262BED6D2D2F2F77DFBF6A53EF5D453FF
              9AB9102C5BB68CEAEAEAB5C05A009FCF87AAAADFD1EBF56F4FF303C2430F3D14
              7AFFFDF7B5DB61D7AE5D5A7676F699ACAC2C1C0E47E4594551F0783C6467676F
              7EECB1C76E1B43D3346DE5CA955A7676F6F726E6CCCFC3B42C7F67672740C3E8
              E8283D3D3D91F1502814B9D7D9D92926A6875B6162C776D704DA6C3680B90683
              215259C2183FC5736D369B9CCECBA7338989D04B29875F79E595A8B0DBB0582C
              9494944C22E5E5E5515151B104C84E4F4F3FFBD1471F313434444A4A0A717171
              06E0D9BCBCBC29C1DF7AEB2D4E9F3E1DB91E3F18235F4820F0784343C357C7AF
              A384103FDCB87163C46402783C1E56AC5821DE7EFBEDA30683E107F3E7CFFF33
              101242A401BF8B8F8F5F5458583825F8912347B87CF9F209E083F1A15E29E5FF
              7E91AF285C2E57645F389D4E84101FAC5DBB76F173CF3D3789A8AA2A3B77EEA4
              B2B212555507B861B3923C1E0FC5C5C52427274FE23736365258588894726120
              10A81F1A1A026EF4CA1D1D1D8442A1E9099C7891919181D56AFD2F93C9545151
              51415252D294077A7B7BA9ABAB23180CE276BBB9EFBEFBA66C7C29255BB66CA1
              B6B6F62F52CA95F5F5F50402816909FA2C26D5625555B1DBED97344D5BD2D0D0
              E05AB66C59A44485613299484D4D65DEBC79381C8E9B9ECA83070F72E8D0A161
              29E5EAC1C1C19EF15F1A772E30180C62369B31994CFFD7D5D595DFDADA6A7DE4
              9147BE50C093274FB27DFB7629A5DC20A57CE7CA952B376DB4A68B2969A6ABAB
              0B29E52740CFCD8CC0E7C1E97422A5444AD93C3C3C4C78EFDD35818989890821
              0A4C26D3A2279F7C3232DEDFDF4F7575351F7FFC31AAAA22A5A4AFAF8FDADA5A
              3EFCF0C3086FD1A245E1F6E037D1D1D1D8EDF63B1238C5B05A2C1601FC6CCD9A
              35936CFCABAFBE4A6565A51CEF59229052869C4EA73878F06024893FFDF4D3BC
              FBEEBB5F0E0683DF8D8A8AFAD39D08BC5525D18E1F3FCEE9D3A79152E2F57A39
              7CF830C06A29A55B4AB9444AF90D29E52220BEBBBBFBEAB899A0ABAB8B3D7BF6
              30FE0B6D7AB9E4369872043D1E0F2693C92A84F809F0DC030F3C6030180C9C39
              73E66F52CA255EAF979191118410A8AACA9C397330994CF946A3F1407E7E3E6F
              BEF926C3C3C31F005BA494EFB5B6B6D2DDDD7DF704EA743A12131399356B168A
              A2A409217E097C0558110804FE7EE9D2A549FCD8D858D2D3D311421C061603C5
              52CABD232323F2EAD5AB53FEDADFB1C0308C4623292929D86C3684108442219A
              9A9A6EEA86DD6E77C486699A86D7EB0DBB9C3BC63F0196ECA7E1AB50EE180000
              000049454E44AE426082}
          end>
      end
      item
        Name = 'heart-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000023C49444154388D95924D4854
              6114869FEFDE3B8D3936062E021D176235EA2CDC48CB82947E374250500BA532
              6861B470D7225CB82B681349A5189830062E5A284AB42F28B541622CB321E7CE
              983F8313CD9DFBF77D2D34431C09DFD5C777DEE7E59CC311A74221FA6B23070E
              EA5A8F407400D5C002F0E497E73D7594A2CA306E21C46DA01E3051BC70957CD0
              639A8E988D468DC3863E1E88C55ACBCE9C45ABAAC2374D8A13E3788B8BA30046
              5DDDE5B2F317D0ABAB916B6B14A72671E7E6DED8525E14A9585377201A7D547E
              FD0642D3F82BE579FC7EFE0C80D0CD2E8461FCAB4949616000773E79D7003A83
              AD6D3B60006118945FBDB6FDDE51D334826D6DB8F3C94E0341831E89504A5A38
              5CF21F608B69D050F8F8FE9EC63DB5C9F81A30EDA552FBE6B798690D883BEFDF
              ED3B608B896BAE54436E2291D94F175E2A859B4864500C6923B9F58292F28E15
              8FA3ECE27F616517B1E27194947726F21B05FD63C1A23D5CF9B9C22E1E91D9E5
              964073334288D2B094148687F152DFFBF39EFFB0EBC712BAAD143396457B65E5
              94585D3DA172B9A34653D3AE102525D6681CF7D3ECA4AF5447F7525ACE168BE8
              0019CFE39BE3C873E143632A9369913F97EB03B1D8F67129CFA330F212776666
              0AD4A5DE6CD619DBC8036C06007CB16D52B6E3B556845E899595E3FEC2D798D1
              D888721C0A830378C9E4A82FE595BEECB23DB89EDBEE6CD7B02743211ED74644
              58D7EF69E1F07D0099CFF7DA52F6F5A44DF53A9FDFE12FB9AD63C120FD3535D4
              97054F03988EFBB63B9DE68365EDF2FE013DE40F5D62A27AF40000000049454E
              44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000031649444154388DA5D4416C93
              651CC7F1EFF3F6EDDBF232B2CDC5ACA15933D4643A589A90148C07E4E0C1841A
              0F9381E0C19B265CB6839E3C71E4628C17D93451221162E26242899986A00726
              78A88354C14AA0659BACB5EBF676EDDBF67DDFE7EF81319919A8E3777A92E7F7
              FF3CF95F1EF5A419E2F344828168B44729F52EF02A1007EE02191139B1E8FBF3
              75ADE9B3ACED4AA9778034D00BCC035F2372E20FCFAB1C2D1451933BFAD96DDB
              43C037A1DEDE98B5F7798C9E1EF45295F6952B0473734BC041EEE5CB503CDE65
              EDD983D1D58DAE54685FFE916061E12EF0F29D56FB9A2A0C0E76A0B8164E2613
              F6E1D751A6C9FD88D63433195A3F7CDF0088EC7BD18E1E3880328CBF3BBE4FE3
              CC1778333345842113C531A3BB3B618F1C5A870128C3209A4EA397976C80683A
              8D526A7DC734B1470E512B1613BA5A3D6602C3562A85B22C368A520AFBB5836B
              E70D3B9685954AD19C9A1A36510C84E2F10D8B6B03D1E823EF01568D01430911
              FEB1EAA6629A28881802B774A9FCD89E2E9511B8650017FDEBBF3E36B86A5C34
              104EF9F9BC0A4AA54D6341A9849FCF2BE09421C8B4687DA1793EB369B0793E83
              687D0161DA98726A88C89897CB79ED6CF67F63ED6C162F97F344642CD774095D
              755D8E7475964CA5DAFEEFF997C23B77627474FC272C5858A0F1D9A7E079EF21
              32F9F6EC2CA165ADA90701FBB76DBB44100CF9D76F3C174E265191C82331ED38
              D4C7C7915A6D1218FDA452E1CCB24308E0E766937ECBE2D968F41CAEBBDFFFED
              465F78D7AE87A2DA71A84F8CA3CBE54B02C3971B0D6F746E1E817B20C0B7B51A
              2F6CB5BDB8657D252B2BFBFC5F727DE1C141D4962DEBB1C545EA273F222895A6
              81F4CD5673E5C8ED224D1178101420E338ECB5EDD676D33C2BAE9BF2AECE3C15
              DAD18FD1D905805F2C509F984057ABDF89C82B375BADDAE142816AA0D71E0C3D
              F8BA27706ED9E19948C47B3A12394BBB1DF3B2D9DD6A6B07C1EC2CEEE9D388EB
              7E2C226FFCD46834DF2CDEE1CF2058B7C186DF4708381E8B71F4896E80B79452
              EF0388C8187072CA71189D9BA7B1BAE6BF82F733D2D9C9F1588CA86124017C64
              E6C352990F2A9587CEFC05DECF63D77E7B9F630000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000040F494441544889AD955B6C14
              7514C67F6766D8EDA6B795DE560BDB106928C85BF1C13486180524A0A641F101
              1362A0411E8CD117A36FA889F2A03131410DD108492172D1074894222144A24D
              A06281DA1A8AE5D2764B939696BD4C7776E67F7CE8B6027617827E4FDF3FE7CB
              F99FF3CDCC376201EFC7626C7A280A228F0ABC8EC86A817A8549E01755FD1238
              39EC794C06014B231180A744641BF08440546108D50EE033E0F2C96492ED8343
              C8BB75B56CABAE02652B229F4A2814711A1BB1AAAAD0741AFFD225F4561245DB
              157D4D00902F04D92415E5388D8D486929666C6C5AEB792EF006E8571D9349E4
              F2B2A53822AF00DFCC6B6A92C88B2F211515CA0C7C9FECA953923DDE811AF313
              8058D633E155AB09AF5CA938CEAC546FDD12F7D041727D7D0A6C56D576B9BA6C
              591D42AFB3787165E9D636C4B6515515110166B977F62CEE816F0124B2F1650D
              AD5831A74E8380F4EEDDF897FB2751965A085BC4B2A391D656C4B62984507333
              A19616422D2D1A6A6E2EA813DB26D2DA8A585614618B03ACB31BE26AD5D4AAEA
              3FCEDC7E98E125EBD6038882922FCFA5B36A6BB11B1AF00706D659084D767DFD
              F4ED7914E4B68D38CEBD7540BE6793252A65122E29B8F28342C2250894398A8E
              9B54B2B6D0BAC578B19A49255118B78033C1952BD3B7DEC7EA721B8AE9F23DCF
              58C031333A2A269190FFC199E9E9130931A3A3021CB35475BFAAA6B3A77F46F3
              9859B7182F56CBF74AA9EA7E4B601CD895EBEAC22412FFD922934848AEAB0BE0
              7360DCEACAB8A0BA538D19710F1F4283E081ADD120C03D7C08356604D59D3772
              3EF68097654365A52B70856472A306FE7480E5A79A996C2E7EF779EAC71FC4EF
              EE46555F45F5B7F7466E600FE57C1EB16D964722BD0835E6EAD5C7ADEA6AB162
              B13B5EC77B2177EE9C648F1E456117F049672ACD07A3A3D8009DAECBB3E5E5CC
              779C13408BFF67DF22BB212E7655F57D3D03BFFF92B8FBDAD12038096C9EF0FD
              60F3F5EB4C1A337D81A7CAE9749A17A2954158AC2318B3C6BF7831662F8C63CD
              9F5FD422BFBF5F327BF6A09ED7ADB0DE379A6E1B1CA4279B0560363E6F0601BF
              675C9EAFAC987244BE230856F9E7CFC7EC876362D5D4CC6997FF478F64F6EE45
              3DAF1B58A3AA636F278639964CCE6AEEC8E7C15C8EDEEC146BCBCB33161CC498
              27FD0B17164A5999D80B16CC5A0288D7D929EE8103A8EFFFAAAA6B81B11D2323
              ECBB3971C710FFFA01FCE579F4B82E4F9795BA61CB3E88EAF2A0AF6F89663238
              8D8D82314C1D3942F67887A83147810D9E31933B8613EC9998B8BB1D05E3E1B1
              9212DAE371A2F36C4B908F80B79C785C00FC6BD714F858E11D2F08CCF6C1214E
              A45273F6299A3FF5F31CBE8EC759120E03B48AC86E00556D03BEBFE1FBB45DBB
              CEF9A9A9823DEE1970A596C58775753C17AD44601180C240672ACD9B896146FC
              E25FFEDFB4813F6F667570110000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000055E494441545885C5565B6C54
              55145DE7DC3B736F673A6D4379B5A5054AD1D242A20484406285422441112124
              3C7EAD8D89510C1FFE69628821D1A026C6184403410B1208A0B6699162C514AB
              586C8985F298B6406728336D67A6F3BAAF73B71F2D55DA0E6911EAFABCF7ECBD
              D6B9FBEEBD17F03F838D7C20035897918175191E94A8AA9CCEF92C009249E4EF
              308CE4B9580CC7C2118484B82FCEC339366466608DC7832245495318CB052092
              B6DD7D4DD7ADDA8128AA0706A011A516F094AA626F5E2E0A15650D63EC4DC6F9
              6A9695A532CE6187C3822CAB09C0BE9810551F0402F681FE100060636626DE99
              3183673BE4ED005E65B2BC9C67654964DBA0705823DB3E4B449FDC31CD336FFB
              FD38174F8C165096EEC6BEFC7C97CAF917D2B4695B95F23570949682A92A0080
              2C0BD6F5EBD01B7E82D5D1718188B69D08473A7B2D0B1553B3E732C60ECB8585
              4B95E756419E3F1F4C9607E3340D665B1BF4FA3310C1E0114154B1CBE74F9C88
              44FE1130DBE140CDBC4267BA24D53A972C294BDBB419CCE118B3664404A3B111
              DAF7DF05C8B6CB0180715EAFBEB861BA73E54A3036AAAA8371A689E4F1E3309A
              FFF8D9245AB7B9A3D368D5344800B077561E8A55F57DC7C245DB5DDB778049D2
              984900803106B9A0003C3BDB6DB6FDF50218DBE1DABA2D5759B62C2539003049
              825C5A0AFB4ECF1C0403EAC234F54C55280C69AED389F766CE9CCD5CAEAFDD15
              151257949449FE0D292707A4EB99F2EC39996A59D9B8621863908B8A60FCFEDB
              D2E9C0A1A6783C226DC9CAC4B31ECF5BCA8A15AB9C8B168D2BD13DC885F32017
              1581713EEE18E67482623149DCBC391012A2812F54D30060ADA3A47442E400C0
              6479F8679B0886B8D62E4A53C1D307D517F39C9C09277A580C7115BB39070703
              189071AFDD26034C55073901700020200EC39834013074101027005CB76D00B8
              21828149E317C12000DCD06D1BBC433700E0BCF07A274FC020D7F90EC3006F4A
              2400C209A3A5053462513C0E10118C96160038D1144F8037C5E3E8135683F0F9
              AE8BAECEC72E40747542F87CD775DB6EA88F46C12D0055FD2102B047ABAB7BAC
              5F8188A0D5D501C09E539108456C7BB00B0EF4F72326C421CBEBBD64B6B63E36
              01666B2B2CAFF79269D3A1CF7BFB000CB561AF10F83410144454A99D3A29EC68
              F49193DBD128B4532705112A0FF5F709EF50DB0FAFBD9664126B3C1EDF34222E
              7CFE32C7E2C50FDC6E1301D93612070F42F4F4ECF69946D56BDD3E9843A51E16
              6003B8104F605356E62F7238B41CA631CFF1C4938F448056FD03CC3F2F9E366D
              AAACB8759BBA4C73F8DD7D8BBF4F08DC320C5A9F9151236EDEDCC0DCEEA97241
              C17F22D71B1BA1D5D5B683B07EF7DD9E64F588F28E721ED774032068CBDDAE6A
              EBDAD52D7C4AB647CACD7D2872A3B919C9E3C7FC44545E150AF57C18EC1D7566
              4CEBD39448205796230BD5B41AEBCAE52D3C7B8A5BCA999808E362339247BF0D
              8068F5D981A877A7DF8FB11A3CA5F7AA8FC550E074F42D50946AB3ADED65969E
              EE91F3F3C745AEFF7A1EC963C7FC202A6F88C6AE567677C34A7136A50002F063
              348A3C59EE2D559593567BFB3A08912D1515A5369E44D06B6BA1D5D45C03505E
              3F3070A3B2BB1BC603865B6AF73924E2742C06052CBCD4E53A627575AEB0EFDE
              2D7014178F7242A4EB481CAE82D1D4D408E0F9A3FDA13B6FF8FC296F3E2E01F7
              D09848E0AE652557B9DDDF5020906B5DB9F2B43CAF08DCED0600884000F12FF7
              C3F27ABF22A2AD7B03C1D8EE4060CC9A8FC48426CD72970B9FE5CF42B62CBFC2
              14E5E3B49736A60140F2D4C924E9FACE9810FB77F9FCA89DC0249DF0A8CB9365
              7C342B0FCFB8DD250C380C00046C6BD7B4CBAFDFEEC68DC970561CC0CE6953D1
              51B240ED2A59A0BE3B73069C8F686C4F3AFE06E5A57A09FC89FA630000000049
              454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000729494441545885CD986B6C14
              D715C77F7766F6C1AE6D6C03EB07A6B43C4C426D48422305480B49086D93162A
              B5525B48498ADAA4216AAB28AA422BF503AA404D2BA4A8899AD0B828A8808402
              2A8D20044231342949310E0487976A60FD003FD6D878BD3B3B3BAFD30F361604
              C7C690D8FD7F9BB9F7FCCF6FEEDEBD67E6C0FFB9D4608393830116E4E4303518
              22AA69FDF7939EC7D96C9643A914ADAE3B6882025D67414E9499E13085BAD17F
              3F2B3EE7B2361F98694E59D9E101CE8F44782E368139918886520B8085C03420
              0A74021F036F2372AABA27C5FA44823ACBBAFEE102019E8FC5589C974B406933
              517C03A8000A8134500F1C44E4D019CBF25F4A74B0ABA76770401D585354C463
              E30A95523C01EAB74AD727EB93BE805654840A0611338DD7DC8CDFDEAE44E4DF
              88FCC68577377474B0BE3D8107FCB8A080D5C5458495FA2A4AAD534ACDD76231
              D1CBCA50912862DBF8ED6D788D8D88E73580FC4E84D7F77627E5D9964B98BEDC
              08A8032F9795F1CDBCDC7CA5D456150A7D3DB46021C179F3D0A2D11B9ECC6B6B
              237BB01AA7B61611F99388FC6A4F32E9B6382E2BC7151A4AA93F2AA57E119833
              87D0C207D08B8A6EF0F0D369ECC387C91E3A8864B37B4564D951337365456343
              3F643FE0F3B118ABC68FCB45A96ABDB8F8EEC8E34FA08F1F3FD00EB84E6E7D3D
              E696CDF8A9D46E84EF818052DBB59C9C4723CB1FC398366D480FAFA30373D3EB
              78ADADC7401E78F34AB2E7E7172FF62F1C33C321D64F9C88A6D4163D167B30FA
              F42AF4FCFC218D01B4C2428C8A0ADC8FEBCA259B9D82524BB5B163BF1B5DF50C
              4659D9CD7944220466CFC63D75AA44D266F98C70F88D13990C176CBB17706D49
              09E5E1F01265186BA23F7D12BDB0F0A68CAF4D604C9D86535B5BA974BD32E7C9
              A7D08B8B87E5A182418C2953706A6AEEC4F78F4F0F05CF6EE9BA825EA0EBFCBE
              B4144DA9CDA1B9F34A82F7DE3B2CE37EC8BC3C54248231630681995FBE358FDC
              5C2495C26B6A2C8F19C66BFB923D68F747A3184A5528A5E604E7CFBF25E3AB0A
              DE3797E07D736FCF63FE7C94527350AA62416E0E5AE59830C0222D16137DC284
              DB32574AA1D4A067FF90D2274C408BC5045854191E83D677BACFD26F72438F84
              FA5866151A3A57EB57B1CA1B3B8A48D7AB8FA51840EBFB410CA56B9F1E31C252
              BD75DF5080D65754BAC5CC8C22D2F5924C06E08A009A2B0250EF25DA4715EA5A
              F5B19C73C5478BDB5980235E5313E279A34B0688E7E13535011C89676DB4E319
              0B11398065396E7DFD68F3E1D6D7836539221C386E59681F663298BEDF2DF096
              73B466B4F9708ED620B05B21DDEFA5D268B6086F2593007F76EAEAF03B3B470D
              CEEFECC4A9AB0378E5FD749A56D7ED3D073775762122FBC5F36AACFDEF8C1AA0
              B5FF1DC4F36A4464FFA6CE2E805EC03ACBE283741A84D54E6D2D6EEF261D51B9
              4D4D38B5B500ABE3B6CDBEBED7FFFED3796D5B3B2272507C7F5B66C7F611FD47
              8BE791D9B11DF1FD6D2272F085B676AE66D7AF4E6A775D8A0C838AC8987F494F
              CFE3CAF7A3C6F4E9230298DDFB36CE8913098125EFA652E61F1289FEB1EBEADB
              BAF6765A6D3B81C8CA6C75B538A74F7DEE70CEE95364ABAB0558697A5E62754B
              EB75E3FAB517B608A72D8BA5F9F9FFD5C070CF9EFD5AA0A262C08FA6CF425E22
              417AE346C471D621F2975F5FBAC47F3E5172F54F0635390E8E2FDC1FCD3984EB
              DCE39D395B1EB8EB2E5430F899C2F9A914E6860DF8C9EE5DC0D39B3BBBE4E5CB
              976F98770320C0D14C86A9C1A09487426F4A26B3D83B7FBE34307B36CA30069A
              3E6C8965615655E1B5B61C15916F1F3533F633CDCDC80073070404F8672AC5DC
              68C4290D04FE21C9E4B7BC787C7C60F62C947E7B90626749FF75236E43FC8C88
              2C3E97CD76FFA8B1115306C21B04D003F6257B5894936B8E0B183BFD2B5D4BBD
              78BC30505979CB2B2996457AE346DC0BE7CF010FB53B4EDBF278036D831C699F
              0A086089B02799E4C1DCDC9E42C3D8E177753DE2D5D78F372A2A86BD27FD741A
              B3AA0AB7217EA617CEBDF4C38606E28E3368DCA08000E655C8684E4F81A1BF21
              C9E443EEC99325C61D77A245223705E75DBE8CB9E155BC96966322F270C271DA
              7ED0D0C039DB1E3276484000D3EF7DA1B82732C62C0D04B68A69CE713FFA68AA
              3EF98B68437420DC781CB3EA35FCAEAE7D22F2E879DBBEB2BCA1910B43ACDCB0
              00A17725777527B9231CB2BF140A6EC3B663CEB10FBFA28DCD439F3871C018BB
              E608E6E6BF2196B5419015C74CD35AD1D048CB103DC55B02047080DDC924F9BA
              EECF1E3366372209E7E4C987FD9EA46E4C9F8ED27BEDC471C8ECFC3BD6DEBD0E
              BEFF4B44D6EC4926FDA79A9BE9F6FDE1A41CBCC33A98BE9F9FCFDAD21202300F
              A5B6EA25259322CB9603606EDD82D7D2D2042C1391C32F263A78F19AFA3A2280
              007787C3BC34A98C49C14001A82A150C7E07406C7B27F0934ED7ED7AAEF92207
              D2E95BCE717B7D0A2057D378A1B49447F272514AFD0C408457DF37D33CDB7C71
              C81EF6E70E7855CBF2F35932B6B73BF15E3AC52B1D9719FD6FC411D0FF009587
              5B3E70DC08820000000049454E44AE426082}
          end>
      end
      item
        Name = 'alert-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000026E49444154388D8D934D4894
              5114869F7BBF9FF966FCD471C61274B4D4691C1505CB1C15114C120AB21F5A45
              8B7646615010B40FDAB48EC07451EB20C4FEDCE4D6221206179A0B13B3214D1C
              FC19673E9DEFB608354BAD777539E77D1FCE3970057BE86E6B90DE58B00BC100
              600237DF4CAD3CEF194AFCE5D5FE2C5CADCFE7C6C90261197238EFECD932ABAE
              CEE74C4C7495E6E90F97D6B32AFE3DB33FE041E7616EB716D65B861CD683C12A
              6F63239A6D93999EB66426DD79AA3C67A4D0A725DF4DA7B63362EBD171D4C7D3
              4BA1B0D0B4F739EDED7E331C46885F6DA51499F171D64647BF2AD76DBCF6626E
              61E44B6AF704F7DA0A3916F4F4F962B106ABA6663B0C2084402F2A42391B79D9
              F979CB94E2EDABA95500E49629946F6842CA335634BAD75D01B06AAA01BA4BF3
              8DEDDA36A024570F088FC712A6B92F40E6DA08218A2B0A0CA1CBDF0035873C04
              BD7A58DAF6BE61002135A4DFAFDBA676BCB3DCDE01F436054070D908850E0400
              782A2B41D073A725802E403B5F6573AB3958210D63C0EEE8300E5A01400B0470
              2627EB83A61ADC74D5BC3674A5D43234F932A7B9F9C8FF4C20741D6959726366
              A63516F23D9396A13DF684C38D9EDADA7F86B76446227822917A4D8A7E1D6871
              D7D6488F8DA1F9FD48DB4678BD08D344E83A002A9B45390E2A9DC65D5D259B4C
              E2A6520027F4ACAB1A48244E6F241231A04A41D9F286ACCDB10C4B57594091C9
              8252EEA225D52C300B7C564A7D005E8B8057D254E2251C3029CD37F0D9B90CCE
              173CAA6E68BE7EF142374BC9247D7DFD14AB85589BFDE3E3CABAC3DCF226938B
              193E7D4BEFFC852DD9B64D341ACD9352F601E78045E0BEE3384FE2F138AEEBEE
              F2FF04BBF8B998BCE474B90000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000033D49444154388D8DD44B689C
              5514C0F1FFB9F77ECF99CC4C273349E791B474E2678C89CD6342D362B4633585
              B8B08F8DB8A9D485DDA95BDD28082EB40815C18D6EC48554ED42A475E163D945
              050543506AA4262D2438659266924E32339F8BD0D43AA99D037771EFB9F7C7E1
              C03DC27D221733FC706A2FAEA5CE006F0216F07E48F8F6E90B37F8EECFEA8EEF
              F44E87BE25BCFE788AFDBBDDA22875C13F783062EFDBE7D617164A84E1CF9D9E
              FEEDD2D555369B6D80435D0E5F3DDFC344DEEF41E45D934C3E142D9530A9149B
              8B8B345756927BE2F6C51303B1F5CB0B6B2C551BF7BC57FFDEE46386CF4EE6C8
              7458CF213263D2E9697764643BEF04013A91984298C944CDC4A727F2E463E6FE
              159E9DEA66B0DB9B00BEF18B4537522A6192C9EDBC492671FAFB09D7D7238D72
              F9A467D4F97C87A97CFDFB6A6B85315BF14C210270CE0902CB1B1D45445A7A24
              5AE34F4E6265320911DE79BA102566AB5670A0CB41441E1691A2373CDC02DD83
              8AE06EDD39A6159181B4DD0A462C41441E11DB0E7522F1BF208049A711115B44
              0A31F76EE7B6C1A0D301D8258EF3400C401C07940A81CC44DE6F058FF6450186
              743CDE1E288249A7014AC7FB63385AEE8213798F918CAB80E326976B0B04B00B
              0580539DBEF64E8F6EB549F996F0DE541722F2A25856AF13046D834E10A07CBF
              0B78E5D50349F6C40DFAC367338C65BD1E44BEF4C7C73D3B9F6F1B14AD518EC3
              C6B56B878C92F36359EFA63ADA177511F9C2CA6677B943436D6377C20E02ECDE
              5E4F443E7FACDB8D2811F948777414A3478E204A3D58F86F9522440E1F46C7E3
              FB45E463034CA135B5D959742A858EC751D128A2771C44DB11369B34AB559ACB
              CBD4CBE53BF7270D70A851A9BCBC76E5CA53C0A322E203886D87E2BA88318865
              6D21F5FAD6AAD5086FD70442C230AC01B3C08FC007F2C613299EDC1BA12F69A3
              058548AEDEA4F7FB45E7851B75FFCCB1E929546303803FFEBACE4FBFFC5A9DCE
              DE7E29EB359604990FC3701ED89CAF6C7279618DEDDF6F6BA12766D8DD61C8A5
              12FCED64B3AB0D3D3332321C1B2F16A9542A5CBCF42DD5D55BE77ABCCDD7C2A5
              AB2CD71A5C5FA933BF5CE7D6C6D6B46D1D2780D69AC1C1416CDB1E1391B3C001
              E026F04918866F95CBE5FADCDCDC8EBDFD0737FED379FD9563A8000000004945
              4E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000043D4944415448899D964B6C54
              5518C77FFF73EFDC7BA77766FA9C3E297DD002A950892D012196F0AAC122EACE
              C480C485B030B8209A800B4CDCB121D1283E48247161A26C5C91B0C485914449
              0951A984F228AF52028CA5D3F6CECC71515A5BE8F0F0BFFA7FF7DEF33DFEE73B
              DFB9E20938DC57C72B8B13AEA48F8177059E85EFADB57BAF6472631B8F5E6422
              6F8BAE378F735E9770E85D1422699FA4FDFEA24595DED2A54919B34BD2A78D29
              9775CD258F4DD029F6625777395F6EADC3778D9174D46B6A2A4D6CDA64BDA626
              ECC404B9E1E1E780839B5B1385285FE0B7EBE34F5781111CEEABE5A397D29478
              4E9FA4BF243599540A0049326188245FD2CF259EE9DEDF93E6B32D35E8692AD8
              B3AA821D2BCA003E30D2376E656565B8762D4147074848925B55854926C9DFBE
              DD602727B703E79654F97FE62DFC7A353BC7DF9CA035A1C3C9775A88C7CC1BC0
              8F7E5B9BC29E1E8B33BF92766242A3274E10DDB8316E2D6BC6A3427FCFB783DC
              BC9F9F5FA2D79624095CF9C021B7BA5AE1BA75E038E801A6259A81EFDB446F2F
              260C03894341CCD0D79E2CBE072BEA02805EA031DED5858A643EC74110107476
              02F408BB786543503C40E01A24AD96E310ABAFC75A6B01EC03CCE6B36DAFB111
              40A055BE3BB76FDCD9C6825217A0C6C4E3D3D9CB5A6BA7E599CD67DB260CA71E
              889A8EB43F7F05614C2CAEF001AAE4CFFDE889705DCCD49AD6BAA4CBD24AEFD1
              005BDA931821E005A7A2625E598A490458B7B616603360DEEA2C9B1BC0087677
              9783F4A2A4865843C3BC9D339B3F6C7B6D6D48B4825E7E73798AC654ECBF00EF
              AD2CA77DAAAC0FE5FB78CDCDCF2611E03537639229240E788EF4C9FA34029CEE
              FA8083BDB53846AF1A9903F1AE2EDCFAFA399B598CCFB1256B8240D1A54B0BAC
              B5D75ACABDDF47C672A87F770BE5F1D842C429379D4EA7B66D03638ACFDF27E0
              9FE3C7150D0D65B076E564C19E37657137441C33BE9F4E6CD83073B89E750FA6
              79D8D383098214D2B19851CA48FA42C67487EBD7E39496FEDFC467E024122436
              6E448ED329E9880B6C95E7D9DCCD9B20E1545460E2712CCCC834DD920FF387ED
              42364BFECE1D72C3C3C8F3ACCD6637B8C0EB85F1F103D9D3A7D7489A1A24B118
              260C31F138261E47B118C462C87D70F073396C2E878D220AE3E3D86C96C2E828
              368A84B5586B23E014B04FE7DF6FC77384B5B64C5227B00C68019AEEE7D4742F
              32DDE5A14FE080F21300B28E6733639358EC9D945B3867C43070191804CE5A6B
              FB815B004A7A86B50BE32CAB0EE8A8F66948BAA44397A4EFF2D3553FF1C3E5F8
              D0F39DCB936FEFD86EA73BF2DCC080BEFAFA08ED8968CFCE96ECE7EDC91CD9A8
              C0C8589EA14C8E3F6E8D333032C1C94B63F3DE7208284F96D0B6A4032BED35C6
              1C5CD8D8487B7B1B77EFDEA3FFCC1945517456D8D563A399B10B7F0F10152C93
              F9477D151DF8D9C9889230C4F7FD5F808BF73299D60B838315D7AE5F1F29140A
              DF596B775AC85CBC7C85CCFD2CC5FE5CFE057F3A8F93FC1A10AD000000004945
              4E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000054D494441545885B5575B6C14
              5518FECE99999DCBCE6C77DABDB6DB6E5B6BD8B45B608B2D18AC051B5A082008
              6D30F860104D242131C60442139FD060D0E88351131FF48118D1840886101303
              112989C4A8B43E006DCA36B4B0BDD165BBDDEE6D76C6875EB46C177769F9DE66
              E6FCFFF7FD73BE73CE7F081E033E9B0907022AEC120300486806CEDC98C2C5DB
              B1827391420336948938B5B74CE459FA0180FD004C00CE1B06DE3ED93D3EF6D9
              EFE1272BE0C22B15F03B85337C4DCD6E69FD7A806591E8E941FCFAF5DE54466F
              DCF065509B8867F2CE470B212F1129EA1CBC8FCAF26E734B0BA8D90CCAF3101B
              1BC19596AE3631747BBD932FA8A0BC04983982AE661BAE1EAC960821AFB22525
              200CB3F09D1002D6660380D64FB7BBE9E1A662B07996C6FCDF802A2B876F3B3D
              68AF51F699587AC15459D92EF8FD602C96C5030941666AAA894BCCECD95821FD
              F5BC57BC7B2918432C6D3C32FF233DE0303338BFBF022E853BC958ADEFC89B37
              83B5DB738E370C03E96010B1EEEEB41E8F1FECBB9FFCE6A5D3C388A6F49C318F
              FC035FEC2885DF211C611DF6772D3B776657FD70358480515598BC5E263538F8
              A2CA1ABFD9CCCCED9F07722FCF9C33D55C21A2A552AAA5027F5C696B03E5F337
              1763B5426E6D6508255F75D659CC35C55CE1023AEB8A4008E9120201969ACD79
              93CF8373BBC157579752425EDFE72F2A5CC0B31E912794EEE257AD2A987C1EBC
              CF0700FB9E29150B17E054D83A4655A5427EFDC3609D4E104A1B3C162EA7D796
              14A00A14007150497A6C7200202C0BC2F39C55A06A4102AC02030390C1E5364F
              DE224C26F02C2929B7B0F90BA873F02080F771CCF73098E2620024B0B972E95C
              4B0A68AD3603C0FAB9ED7559E03C1E00D8B5A776E93D244B40114FB1AD469108
              A5ED73C1CB82A9BA1A84657735B845C7064FF66AC81270B8A91812470F705EAF
              4CC5DCCB275F509E07EFF3F1203872F4B9922CC245CFB576135E6B501550D225
              0602CB269F87B8762D08C71D5AE716AB5EF62F9E8A0501024BF0C9561738867C
              24F87CAE9598FF05124982D8D0208090CFBB9AED2855FE5D110B1BC4C7ED2E34
              7BCD1D8CC57242D9B265D179BF12601D0EA487869EE252F1D17A27FFC70F37A6
              A01B73020E35AA78639DEA272C774ED9B6CDC428CA8A9203B32725E77623D9D7
              F7429999B9A00A74E4D2E00C9816AF840FDB5C2594D28BF2A64D4ED30A383F17
              A82080161571A96070EB6A177FFAFE4C26C6BCD9A8628D4BFC5E0C049AC4FAFA
              27463E0F56554108B16AA1D05A5DC7A979136ADAC80892FDFDD06385F7F6F942
              4F24901C1840FADE3D00982604602F0FCEA0B3D6D28950A8231D0A750068A692
              646354158CA280CA32A82C830822A82880F03C08C781701C40E7F4EB3A0C4D83
              914EC348A66024E2D01309E8B118F4E969E8D12832E13032D3D353308C6E18F8
              2E6318A72F0563B33DA14B66B0FD69051B2B24AC71F2B099590F21F001A40A80
              178027AD13FB509C692BB75B299349826849105D9BED292963E80C8FC84C0A26
              A4FBCCAC31046002C05D0077000401DC9C4E65067A4692C6B5E1199CBB1945F0
              417AE9A654315154A91C3C160E650A8762892263B6E34AD872BBEBD851AF7D89
              C6D4300C1C7FFF04DCDAA86F0D37D2FF20AE61624643685AC3DD290D772269DC
              8B6A59714B9E91D1948EDED1247A47930BEFBC5E194EA7E5EC2F977F7DABB363
              6F56CCF59E5E4C4E4EFE3DA691FEAFFF0CE5704136F2DE6D32990C6C36DBB5A1
              E1E11D93E1B0C3566283280A884422B8D27D1567CFFD18D1757DEFD8D8582812
              89E42DA0A0BBA1D3E9447979B94C293D06A013B3FE9800F01380F7A2D168F0D6
              AD5BD0F5DCF78065090000599661B7DB21FDA75D4BA7D30887C3181F1F2F341D
              FE011CE188F3BF71FA720000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C0864880000070A494441545885C5987D6CD5
              5719C73FCFEFFCDEEEDBEFF6B6BDB7EFEFD0162854283048C7D0C290910CB738
              7C05829BC368086631D3FD61B63F966DC6680C3159625C74892C24C6A9318B2F
              A001711AB5DB100C0C6A97A5B4584A4B4BDB6B7B7BDF8E7F5CBA314B6FE98BE3
              9BDC7F7EBF737ECFE73CF739CFF39C232C813AEA026CAF0FE098927BA0E1FCE0
              14AF9C1B652AA317F56D59CC642570E481527637852C44BE00EC046CE02FC08B
              EF8E246FECFB791F9747D37707F0D0C6084FB6178744E4B8E1BA1BEDFA7A508A
              544F0F99B1B13EADF5B6B35713DDBB8FF5DE1DC0BF3E5E4779C87A5185C35FF2
              1E7C10C3EF0740A7D3C44F9C20D9DBFB77ADF5E6878E5DE6CCD5A905D930160A
              571D36290B9A0EB0CFBF61E37B7000629AF8DBDB11917B0459D350E82CD4CCFC
              014B8326DFFD7809270FD402DC272201152D9E314E791E62DB1AA1E5F9ED319E
              DE1AC573E6EF8F79CD7860598093076AD8B32ABCCE52C61F0CC3F8AD5952A2C5
              346F3B5E725EFDB16B1A2F7D715DA4E4C4FE1A3655BAF302BCE3187C746D01CF
              7C348A08DF108C67ADFA3AE56B6BC38C44669DA3B526D5D7C7646727E9A1A1EB
              5AEBBDE98C3E7EE8D7FDFCA6FB3F4B07D8511BE0479F28C350C611B1AC4381AD
              5B71EAEBEF6C6580CE66997CE30D26CF9E4DEB6CF6F38954F6678FFCB48F7F5E
              9B7BE3A8B906788EC1B13D95046C75582CF3E9D0AE5DD85555770C0720225815
              1588691AE92B57769B86F1BB8D95BE7FBF726E94B9D2F89C31F8950D0514FAD4
              32846F05DADBB14A4BE70577AB7CADAD38CB973B22BCBCACD0B61E59E5CD3927
              2FA012F8744B0122F28C555EEED88D8D0B869B967FF3660CC7592122FB0E7CA4
              6071801BCA7D14F9CC88C01EB7B5159145E5F59C41D7C5696E0638B832EA501A
              C81F65790157C66C806DE238965551B168B869D9757508AC078A57C6F2A79DBC
              80614781D0AA8A8A1063C145678654713118862122AB0B7D8BF06071CEFDD15B
              CBD852480C03715D0DC40A7DF9179EF76DC03200FCB3558AC5482C0BC0ABF4AC
              BCE3F2023614DA003171E7579EEE44462804B06C53A52FFFB8D95EF82DA1B9D8
              016836BCB9F3D57C65C66200F735153BD48467F7E2AC801D75011CD3A811A4EA
              E6C79654767535C00641AA3FB33A3C7FC0CFB684011E364241549E8660A152D1
              282A121184C7F7AE09CFDA8ADDF6695B99CBBD357E010EDACB972F4982FE5F89
              086E4B0BC097C3AE0A1F6CBBBD1366002A816F6E8D02F22931CD2677E5CA2587
              9B96D3D888F2BC08F0E4C1B6026AC233B3C50CC0C7D615D056EE064478C16D69
              61A973E0AD12A5F0AD5F0FF084631A0DCF77CC8CF50FA4F1D5318723BBCA300D
              E388F2BC6DC18E0E44CDD9912D4A2A12213D306066C7C757541758478727329C
              1D78BF4F7CCF7AD4AF38FAC94A8AFCE6C322F2EDE08E1DA8F0ECBB6BA9242298
              25254C7575D593C90EDD53E5EF3CF14E9CEB9399F70195C04B0F55D012739A41
              5EF3AF5FEF384BD05ADDA90CD7455C97F4E5CB1FB30C5EDB52ED1F78F5C238C9
              8CCEC5E073DB626CAEF41780FCD2AEAD0DB96BD77E6870D3729A9BB11B1A7C88
              BC5A1BB10B7FB8BB1425A0762D0FF2D496622522BF300B0B378476EEFCBFC7DD
              ED2422585555A47A7B237A727273A5671D4393319A8A6C44E46B86EBDE1FDCB1
              63BA88DF15896511DAB9132310D822C8D7CB3D0BE3E6C14EEB6C96A9AE2E3223
              2368BDB81BA9852A3336C6D4A54BE8540A840901CCFE780AADF98E9E9ABA31F9
              D65B871367CEAC129F4F9BD128AAB010150E63844218C1204620B0E8C6556B8D
              4E24C8C6E364C7C7C98C8E92191E263D3848767C5CB4D6FF02BE07FA07FDF174
              CE7DFBD784796C5D01B5111B11A903B6006DC02AA051A00C118508E238DAF0F9
              109F0FB1EDDCCFB272713B1DBBD92C3A93C9792295243B9544271239B0440232
              999B7F9B1E06BA80F3C09BC069ADF58591890C2F9FBDC1F7FF36FCC1837B43C4
              6253959F353187C6229B6545369EA3002C11A902AA35C42E8E99ADE747CDA7B6
              B76FC4268DA427914C1AD1B97B402D0AAD2CB4E9F0E7CE7F48859B3ED6E4A5DF
              5430883008BA57430F9A78260BEF8C24E91E4E72A63F41E79549CE0D2498BEF7
              9CB30BF05B42A56751E559447C8AB2B043AF5D5DDC9F30079EF8EA61AAAB673F
              C40F0F0FF3EC732F48999B5ED56A0FBE3D3A1E676432C3702243FF789ABEB114
              57E399BCF6E7ECE527529AAEEB49BAAE27732B12A1ADAD66C830F4EB7F3CFDA7
              7BF7EDFDDCAC734F9E3A8DD6FAC29509E3EDE39D434C4C4CCC656E861694F082
              C1208EE35CE8BF7A757F3299540D0DF518B76C9E743ACDF113BFE7E4A9531960
              6F2A957AB7B77761B7AC0B6AF47C3E1FCDCDCD28A5EE370CE3279EE745573437
              E1791E63E3E35CBC7889D1D1D111E051ADF5AFBABBBB191919F9F00021E7C586
              86066CDB0E027B45640B500C5C075ED75A1FCD66B3633D3D3D0C0D0D2DD4CC22
              6FF995A2A8A88870388CBAA53C66B359E2F138D7AE5D239D5EF80D3FC07F01B7
              D8182086C9EE420000000049454E44AE426082}
          end>
      end
      item
        Name = 'home'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000001CF49444154388DA5904B6813
              511885CF7D643A9318F322B5A15A853626B4544AA9126629D8468DA008BA2876
              23149A859BEE0AC55DA169215DB9EC42A1A5B87413C8424149C020B8F0819222
              DA6C4434C46992693377AEAB3EACED34E0D9DDFB9FFF3B3F87E01029DD318427
              3270F94E4D03B045BD96FDB1320373FDCD3F5E76F0C33D3486CE898CCADDBEE5
              932A9FEE6074B44595A8673899B3CD86B5BDF1EE0800A1F027D308A61E9CA68C
              E77A43DAD8933B31DCBD1046F19B31586D8AABDAF944CE15EAAE353E15015BEC
              0188EA41E7BD799C18B9A61390FCE55E7F74F976145D5E05018DE3567F08E59F
              66E44BD51C5722D19216D3BF363F1721CD3A180F9F45D7E423A83D03F729A16B
              5389886F6EF41C54D7DE710AA7B81E0F80009E5265739C794335CF50F2F5D6C6
              0790330FF39C69DE2C11ADF4D2CDB84CC58387F5BAAB7CB98AF4D3F7C4E61D8F
              A510539469DE5562B752FE17591CB70C0057FA02F03F5F04B1B61284B1352AA5
              5C502A6F2FB96A95639777C48DEFD0D65F2624304BDC176FA087D5E19375BB50
              28C87600BAAE9386CD68799382364ACF60FF6A3F7D47B2F91BCD8FAFC09D52F6
              BF8FBA8E3AA50821A865598E1EC7A1699A300CC3C9E20C6847FF0DF8ABC483C5
              B533FB035DA297A57C648CE00000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000002A149444154388DAD925F4893
              5118C69FF7FBE63E67730EB726869BA4A294096D996462A4F6EFC2A228A1B042
              705EB5DB122ABA8B88A23F17120683A48BBAB0AE6A17491161F647DC9488AC51
              51A99F994EA773EAF66D6F3722CEB9A9D47377DEF33CBFF370CE212491AEAA01
              FABD760882EA0CC00A335A27BBDA31FEF416108D2C9B11971B925A03C3D10BC8
              A83CAE1648BC43441789A816C086544BF13329B72432D3DF050ECFC567E34ED0
              AD87A9E13AD4D9855944D44E44158EF26C66065ADECAC4CC6F001C0BFB86E491
              FBCD08CBDEC40DA58D5664D96F436DB49412D1F3756AB1F8666D1E4E5A4D28B7
              E85064D4E0E5777F4E38C227444D7A97D67A604099184678F86B3C505B7A10A6
              FACB1052B5F544786CD64B996D7585D861D12D980B0C1AD414E8F1FAC764FAC4
              5CE4148929C369C5BB3DA44EC3ECB71E801922041199479AA1DFD32490205E15
              88AE959B75AAB6BA229833A4B83B32A4A5E0F06603FA4782E24F7FE810009394
              5BD22159B644673E7781B29A5A909ABF4D4FC00322DA7FDA6AE2F35566560971
              D71BA3489471A373905ADF0F13337732739DE2937F0BA979B64D04BCA36864DF
              CED007BE546359110600A24038BB2B872B677B9822A10A22EA4E31646F1788E8
              0A316BB57D8F9E6807BA7945D2126986FA58EB7ED84E512504D03981396AA7E9
              B132F5487F0F11AD5C6D898888A43F5F3E8AE3BFCA18B0AB7C1D7747753C0331
              3846800558E66FAE2461768A94574EDFA8224135F5E21E24A311949FBF56CEA2
              964078E013A60707A14A6674BBDDF0783C3133ABD50A9BCD96309314E8F178E0
              743AFD007AE7475B1B1B1B3392018564C079F52A8A521D0A85AA1781FF098860
              3008BFDFBF1AEBEA806BD17F07C63C4A201080DBED5E58CBB21C17906539C613
              08041203BD5E2F391C8EA40D5C2E17B95CAE84FB7F0145D3F385BA1374FB0000
              000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000036E494441544889B5545D681C
              65143DE79B99DDD96477936CD2AC89609AB54D05A549435A49021AB03658C416
              5AADA8F8105ADF84A2F82E566911C152059FD6824545919287FA83A2066B2C25
              64C222422BB5924D1B5BB58949379B64333BD70767747676936C1E3C3070EFCC
              B9DFB97FDF10EB40D536A0E9E9D710497583C01020B608DE5BB93989DFCFBC04
              FBD6D49AF1DA5A1F8D96AD480E9D44B8759B41F24D00AF926A1F8006555BF74D
              74C7A05398FE19F6CCF58D0BD46CDF8DE6675F871E6B6C24390CE0505B838978
              58E3DC52F17E92FD34C29F453BF7E4A56863793253AD0051B7FB301AF7BD08A5
              873A497E45B2FB81F63A9C3ED08127B76FC2A53F16919D2BA4003C0EA5CE47B6
              ECFCCDD87417162F5F001C7B75011589A3E9A96388EDDA0F9207410C934C0EF5
              247162B01D35869288A1F1D17B1258B61D4C4C2FD4037C8644D648A67E8C6CEB
              C3D29531388BB7CB05F4442B9A874EC26CEF52245F0670CAD455F8F8E0661CD9
              798728FA1221D0DF16C7E684C9F3BFCE1B2B82FD04625AACF1DBDAEE479CC2F5
              4BB067A6FF13303B7A913CFC168CFA649CE487008EB4C4427CF7E0560CA4EA84
              2E00803E743445642055CFEF27E738BF54EC25D94B3DF445B46B4FDE292CA230
              F51318EB3F84C4DEE701A56D217996E47D3B5A6BE5EDC7EE46321A82888877B8
              DFF6FB33F9151C3D77153F646F5344AE8AC80140320B99AFC1B6E31740F26100
              1F00483C74A786534F744958F3F5A40AD88EE0858FC7F9F93540801C04CF09E4
              2345F228804F01C4A3994FB02B6F4958A390849778253BE8EB8AD2B99091D8D8
              1910A251F17D92C714800880D9D0CDCB7B6B7EF96E43595782393526A1ECF883
              224E16C0B42EE29C80C8E9F0B5F11B2EC7EBB717C355ECB26F5E6C64F2E278A1
              E5DE2EC730E7F5B98B6785E2DC68FE33E36D8800FF6C8B2BB4EE907D36482274
              EB0A725FBE33BF0C0DFA5FC36F402905F6F400DCD860578553C4C2F839E4F379
              E8D5F0FD6D09B4685D9408882FDA6FF7F5F579E6BF2592C4E8E86830A62C8112
              01B79FABCD60444446DCD703EE5371067E91AA5AE462C4B6ED578AC5224CD3A4
              2BB02E94DF09B6C8F33D3397CB617676D6CB502AF0CA5A5422E05FC10A3F383F
              0F70E711E081814D2C11F83F50D51649A06E1181772137BC459665955C58F730
              067800C0743A0D3F6F626262ED2D1211B12C0B9665052B2DAB0080A4D3E920AF
              0C7F036F5BAA4BF2085DF20000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000449494441545885ED945D4CDB
              5518C69F73FEE7DFF25DCA4781526819A5A51D5FDD3040A5032611199A297388
              2C5126CC65C912B9D10B2F98C96276B198C5A8247ECC65C9124CD44D26612A54
              6770CCC5B94C5BB4830DC6F81C1B303E0A1DD01E2F4C365008A583EC86E7FE79
              9FDF3979DE5780AF0A8901CBD90FBA691BF8BD3EC035EED318E29329760BC4FC
              1A8148038F0298E373B36FCFB77EE8F6DCBCB0EA59C26A0D34B51462EE817022
              4A1B92E3C3CAC382FDCDA3CEB96CAAC9690293CEF001DB3A010812B0FC1A30E3
              8E744A694B8129DEB42B4F07935681798F27F1D6F0E46EAA309C270AFD6D4FEF
              EF807B760D018214108B0F4150A6964924ACA17CBB21D29CA2C4C5F6010C8E4C
              A1C0148FC8507FF9B5BED1577850D40DAA31B7F341BB57BD581180C4A4405254
              4B6948D4BB6121FEC7AA4B52454DB40C675A3B3172771C7747A7707D68129634
              15366BC2C5CEBEB1175DF00BA4DA829FF8C410E7F77A7D07A0C66721E6BF114A
              44BFAFB4B1F2CAAA92543026E0C4391B9223FDF0E5C12CFE6AAE1A3FB70F91C6
              CBBDC8D44723DBA8C4E088D3EB5E2C0D404530CB410869CF1B08212D96345556
              59413286C7A6F179930DFBB7C5F3F7CA53216102248CA274AB12D3AE797CD0F4
              17498C95232F3DCEEB5EFC1F20201C62512D049569A744A48DBBF3F431791971
              B8DA398C86D60E7C5299C1F75A3420E4E1061342F0A42E1C9B95C13872C64624
              1286FC0CEF7AB10880441920161F2254A6AC0D0D96D655ED489326A9E468FAB5
              0B1D378771B6C6CCCD49E1CB7EA7362A08CF65C4A0EE7B07E9B933054BAA0AC6
              157AF1E01954FF345876553011D8C98418D9CE3D85460802417D8B03F1328693
              FB32B93C50B26CF8424DCECCA1FAC415621F9AC69E4223044A506F75A0A36FAC
              99735EE1B67D33EABE7C0A0087002A42C8D907D1F4929650DA9C6D545A2A9E32
              607CFA3E3E6BFC13A5A6287CBC770B0F9432AFC201402A0AD895198B09E77DF2
              D1770E6863E5D896AE5AB21702331F00D3173EC3183DF7822529AE70AB1A8E5B
              23F8C2FA378E96A5F09AA22450B2FA8B4D0881451F81E4A8401C396D23FE7E22
              F2D2E316F72222B19DC23F042078DD94A4087D22391AD62B3DB0FED6856F6B72
              F8CBD971AB0EFEAF4A3262607D2B97FFE118C0D90BD791921001833A3C00E0C5
              607E20549D05B6FDCD4881D2AB094A59F450673B6C75559E6899DF23872FD4F8
              F41CF4AFBD4FFDD5E9B83DEAEC0447E6DCF96353D4D37309EE6BD63B1E0FAFEC
              EDEE39BEA9BB9EAF753800C80244687ABEE613FD5D870154CC77FE38E5E9FE05
              0C00DC978E830445348779FA9B0578AA01F03527004009218AFE96776E1035DC
              173F05807F01E09EC5FC0F8721D5E900B97C3DB21F4832DA01B7FDF443A8754D
              F3421B00DE9FB705329BCD4B5EA6B6B6B65597D727000070BBDDD4E5720100A4
              522918631E5FE6F80CE072B960B7DB01003A9D0E721FB7E7B17760036003E0B1
              032CB986CB1D9A95E48B6F1180CBE582D3E9F4EA576666661ED90700FF00A805
              9F195826B0270000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000005EF494441545885ED957D4C55
              E71DC73FBF73CEBD977BB9E0E5720179973715F0057022EA3AA6386DD7A4AEE9
              56D4455C5D5717BB17B62C76D9B276FB6BD996AC66E96CB6A089B6C6A69D71AB
              0A96B611525B17ABDEA688655C518C22081729D78B70B997F3EC0F3BB356F085
              8BA97FF8494E72729E73BEE793E7F99EE7E84C1589B9E80BD6A16595C3E83508
              F64D49AC4C4948EE4358966E422CF6C78051659A8722FFDE81D9D61075B61E9D
              9986FE95F518E5DF13D12DCF8BC83611598B68612DA3EC883893312F7A41995F
              82A0CD89B17C0B7A7E659C88EC89B11A9BBEBBA248E6E478E43F1706AAC64C35
              47DC331AB48CD251F3E249080F4FEA35935B62572696AAE7D0E2A7E723B22FC9
              E528AA59594C728203809E2B43BCD2D88A7F70A415C5E36A78A023DCF467544F
              EB5DBFEAAE6750B22BB0AEFC15E24858252287666725666E7C642E2EA78D0FDB
              7AE81D18A2202381B282647AAE0C25FB0323EBC562FF58CBAB3CA3468228FF99
              7B24281A7AE993188B7F80E8962D9AC8F665A559F6272A67A26BC2810F3A38EC
              3DCFE9CE7EAE85221466255252908C32554CE7E5C0A47B79674B6CC4607CFD67
              68990B1C225267B5E8D5DFA99CC5BCBC248646C2EC7EE73481ABC3ECDEB45045
              4C45CDDF8F8B2BDEC1BA1585C4C6586839DBC71B4DED84C2637B95521B95BF23
              187EF70F70AD7F0A04A7A561A9FA259A2B3D1BD8E78EB7CFAF5959446AA293EE
              FE20BB1A4F93E5B2F1DAE672959578BD83E7FDD758F3F231B9F0E908EBBF514C
              9AC739E95EDE728925A30CEBAAE79158F73281C6FCF4849CEF3F3A1777BC9D96
              B37DEC7CAB9587E724F3FAB3E5CA1367BBF19CCB61615D45069F7405E4D5E633
              24C6C5909F3EB95E4E28A8CF7F0263E966C4B0FC4413D9F5D0BC0C67F5B2D958
              0D8DB74F9CE7C0071DFC66F56CF5C7EA39588D9B632C86C6EAB254EC169D970E
              B54968748CD993E8E5CDC9460C7A652D7AD1376D22526731F4E7BE5D3953AB2C
              C924141E63CFBB6DB475FA79F5870BD586AF662332714B44848A7C37E5B909FC
              ED1D9FB4777D4A61969BC2EC4452121C77B45F7E3E3D6E3A96AA2D883B3B5D60
              AFCB19B370FDCA223292E2F00F0EB3ABB19578ABF0FAE6456A566ADC8462E371
              AE6F8835DB8EC9A5C02835AB8A99EE8EBDA35EDE9841499D8BF5E1DFA2C5252D
              15783B2775DACCA71F9D4792CB81EFE2003B1A5A5834631AFFFCE91295E1B6DF
              951C4042AC95758B333975615076379FC133CD4E5E9AEBB6BDD401B4E2C7B07C
              EDC788C5F60CF0DAE2A2B4F8B55585D8AC3A475ABA78A3A99D1F55E5F2F28652
              E5B04EFEEF6835341E5F9086A10B2F35B44978CC6456969B92FC897B29FAFC27
              D1CBAA2D22FCC5D0B567562FCDA7BC309570C464DF7BED7CD2E9E7AF35A5AA7A
              51C6A4C5C6A3F1D4659EAA3B2129894ED62C2FC46E336EDA2FCD73EF0775BD60
              199A27E7D722F28B8D8FCC655E5E1283432176349C622838C29BB54BD48AE2E4
              299503C84B76F2ADB254F61EBB20873FEE223FDD456E9A8BACE4784EFA2E1703
              5719BE7A44174F1E9252F811506D189A2B36C642DD81160A92EC1CFCF912333F
              C539E572FFC31D6B656D45A6FAE8DC15D9F3DE59925C0E8EB676E11F1CF62AC5
              D3E645EF98AEFA7C68D915218989FFF052FFD08693BECB9AA3E738754FCD370B
              664CEDB28E87CDD0C8317AD5FE37FFC5D1DE58FC83C3432856A940775FE4F09F
              D088848834BF8832C78E2AA55EB0062F3D9B79E92D34650AD7B7A17B7E08484A
              DF51891B6C5F0D6C526ACC17697A1122231800EA4A2791E3AFA02FACF9BDA7FF
              0422B2ED9E4FDD17101149E97DFFE000F198A70FA2FA3B00AE0B0298ADFB315B
              F7E32C29019B6DC2A07B496CA897C83F367FEE9AF6A598DC050F04A3E5BE1734
              6E7FCBADD9BA752B3E9F6FDCB18282026A6B6BA3CA8F5AD0E7F3E1F57A9B81E6
              2F0C557E764445D4829FD11C08047E1708040070381CB8DDEE17B88F04090402
              74757501E0F17870BBDD53927BDF7F240F04A3E58160B43C108C9609F7C1FAFA
              7ABC5EEF6D03BABBBB6F39B67DFBF6A83226126CAAAFAFBF6DF0FFD139DEB59E
              9E9EE63B11BC15370906834142A1D0F2BB0D0A854237CEC3E13081406027B033
              2A3BE0BF06FE7892D10020D50000000049454E44AE426082}
          end>
      end
      item
        Name = 'button-info'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000026549444154388D9593DD4B93
              611887AFE77D67CE4D9D33B649A6CC183BD13E168B24F040A1A8C4F36148059D
              04E27F5044041576D649440491B3B1830E8248348D4A70CA72E9EAC418F8097E
              AC36D7727EBCEFFB74300D4DA1BC0E9FFBFEFD78EE2FC126C5FE16EC17DB55D5
              521A007109F023B0234901519041637D3594EE7BAC67064320250002C0DEDC81
              ADA1D50B84EAAB4B8FB7F91CF82B4BB0179948E534A2733F791E5B22329D1903
              02D9D89B8964F876DEA4F8540BEEFB11AFA7737821189BD7354DD3F742D3343D
              189BD73D9DC30BEE7B11AFADE92A00AAEBDA43552930F7DE39E73E1238E64008
              01C04022CDD9A75F449DCB424DB9192104752E2B4E6B81A53F916E30571F7D92
              FDF45AAAE517DA5BEBAB4BAFDF68AAFA23DEC25E64E27455BE942D6A9D164666
              B3157359FD9BBEB21C5780D6369F6397B8A6DC4CC79943D4949B77BC0B2168F3
              39005ACD353E14C0EFAF2CE16F0612693C0FA2622091DE15F3579680C0AF5ACB
              5010C2BEFD8BFF433E5FD80114A44CA772DABE0C52390DA44C23250A108DCE65
              F765B0991F35D65650A4A4BB2BB688DCDCAC7F21A5A42BB68884EED5C9CF2872
              23171A9ACE8C85E3C9FF3208C7930C4D67C630F4D0AFD11E54A9AD1B45DEFA77
              EF2797034E6B81A5D6694108B16B8C524AC2F12437DF4E250D4973E6E38BA595
              F13ED4B599AF980E1EFE6E72795EF527D20D23B3D90AEB018532B3894293C28F
              158D0F93CBDCEA9FE1D9E8C2982169CE4D4426922FEF8261E48F09A1606BBC8C
              ADF18AAA141406803DAE91A034F4506630A4A77A1F81B691976EAF4FB5B9283E
              799E42F709D4E2B2CDB0C4C865599B1A271BEB414BCEECE8C96FC533130A98BB
              6A390000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000034549444154388DA5944D4C5C
              6514869FF3DD3BC30C57187E064AB5756CE132288962ED682C0BD3223675D198
              26E2C22034317155174D8C6E8C0A8B26468921D1E8C2A451171243EA4FECD4D0
              44DAD84A4DAD31D016AC2356908065B86007E60E33733F17202919BA68FB2EBF
              73DEE79CEF2C5EE17F2983D013ED84F61C447CFE06413A8116041B2846E30023
              C071ADF551CF4D259D13EF933AF725374A00C41FA4BAF31D82DB7784407A10E9
              68AAB1A4A5AE0C3B1CC0F21938E91CC3338BC4C71C2617328B4017E89ED4AF27
              BDD9BE37C1CBAF0295C1A6177B09D63E1241F35D6D65D0EE6E8DF0D8D6123652
              DED3F48FCC72E4D424FFBAB96F80E752BFC433B37D6F01608476775012DB1F02
              4EED8A94DA9FB545D956115803249269AE245D8A4C85E5375022346EB278CA2E
              E764623E9ACAE46DFF66BB3F9B9C203B9D4085761F044D4F6D45D0FEF0993A2C
              BFB16EA38F7E9AE6F9BE31393DBEB0EE7D7B45808F0FD8047CAA0D682FDF7708
              948112D31F159117BA5B230530809DF7DCC581C64A1D292B2AA845AB8A79E9D1
              CD005D4669A5196C68468948E7433596BAD9CDDA1EACE2ED7DDBD8B965E37AC7
              8E6A8A4C75AF204F06EB6228A0A5A5AE6CC36680812B0EBD67A7B8F4CFD286F5
              50C024B632ACC5571D4121526F87031B36030CFC3E4FEFD929B97C1320801D0E
              80888D325140B1E52BBCDDAD68C5AF2D01145A3B4E3A774740279D038DA3B546
              012323338B77045CF50FE3E551C0F1F89843DED3B705BBEAB80C4F2F09105F9E
              49A0B4D6472716328BFD17676F0BD8FBE3149ED6435AEBF3E9CB67509E7B3D09
              741D199C647CCEBD25D8B7A3737C75692E0F1CCE262771FFB880E1B9D729BEBF
              792893D30F0F24E6A3CD9152C2966FCDD46A97F3F2AEBB79A0BAB800F64A7C9C
              9CD6AF82FEE2DAE76F904B4E602CFF3D8A59759FF6D7D41E4B65F276FFC56463
              DE8386AA200153156CF5A7E3D2FDFD5FBC77662A9FF3F46B40CFFCE0A7A4CE1D
              0356F31065107EF675ACA6BD88483BD05D64AAADB12D2505793832BD249ED643
              C0610D430B839F307FE283B58172E374AB692F654F1FC22CA93445A415D803D4
              236B893DCC4A62FF9C4D4E32F7F5BBB8BF0DADFBC13A2000A68F60FDE304EB62
              F8AA226018088246A3B3CB64AF5D253DFA036EE20268AFC0FE1F96DB42665B4A
              7FA20000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000433494441544889A5954D6C54
              6514869F73EFCC6D87CE94FE51DA3253A4256831846086B4912E1405D2D49280
              91B8306E1030415D9890B8B10B1234118D0425F803BA51031117A418A2244282
              0252A08A5806858A282D4329D369A777663A3FC7457BE9DFB486F0ACDEE49CBC
              DF3DEF77EEBDC238246F1645AB5FC6D7B01E314C8F88AC055A8020821FB08008
              4A0838A1AA0780CB99D85DEE1ED985FDEB3126238E70973F44F98BEFE02E0D98
              886C045A45A46AAED7ADCBAABCF8675B58A641249E26D46B73E9962DC3996C06
              6803B6A9EAB5A15FBEE3CEA11D90494D3CC05DBE808ACD1F62784B8B05BE04D6
              345417B2B5A192FA804F0D610A77ECB41CBCD8CBBEF65B0C263331E025E06BFB
              F28FDCFEE20DC8A4013025AF808A2D7B308B2B8A45F821DF65346E5F359F3757
              06A82ECA4700198571BAC03259EEF7EABA47CBE4F7B06DDD1C1C5E0F74BBE604
              3A8CBC02127F9C1939A0A4F955663DFCB829C2A17CD368FC78DD426D5A54AC8E
              A163BAF74C0FA76E0C88651A54165AF726F1E5B9E499474AE8BC1D97EB914493
              2027F3028BFF4E5C3D4B267A1BC357BF1E6023CA9AD6A7AA75C5FC4205D0511C
              BDF7E71E769FEAE642776C4ACD32457735D7686DA9C705EC1724BF68F516000C
              0CD303B4365417B261491993A370F472BF9786808F4A9F354D6406DB9FAE4644
              6A10D99CBFE0315CA57E5C22D22222555B1B2A755C2A53D8FFEC225475427493
              A90FF808CEF36AFBBF839B30E4034F5DA31A40CB5CAF5BEB033E9D3CFA4C3A57
              0DD0B57525008B416AAD8A5A0C20B8ACCA8B21B9B7C5D14B7777C8C277CFC967
              E7C232535FD0EF1B5D0C8266413106227EFFECB1AD7850C6BCC40FE0022CCB34
              70C6751A736801D0FFE91BF512542D0003D5FEFE789A5CE38ED78EC9742F9EA3
              23F1B4D3DB0F8A018442BDF6030633C69531AF100A0670E2B7B02D7D767AFA1D
              BD0F8E7745016C556D4F47C318AA7A70389DCD1CBCD83BE36A3A069AA3E6E881
              649AC39D7D30F2851D4CFCD5810174026D9FB68709C7520F74077B4EF7104964
              B28AEECCA612C4AF9CC1C8C4EEA2B06D30998EBDFE6D17C97436E7E823C63346
              239F9F0F0BAA9F001DB10B47D1E410326BE92AE63CBF1D11790EE1AB276B8A8C
              5DCD355A6019F793BBBCD6768D783ADB8EB23233D46FDF7C6F03597B003315EE
              C25D16C0AAACED04E9B91E49361DBBDA6F2E2AF3887F765ECEB81C06931979FF
              A76ED971FC1F19CE683BD0A29A8DF61E6825D5FDE7BDC9C17453FEC2DB78EA1A
              117802D8272235CBE779B5A5AE84A0DFE7FC3235124F4BA8D7E6445794C39D7D
              441299EC682CDB54B376DF376F113B776442B423982E8A9B5EA170C50640F211
              B608B209A89BE6526C9436859DA01DD9A128770EED207EF9E494BB9B8055BD84
              A2A6AD78E62F01C314515D884810F02358281120049C5534964D25889D3F4AF4
              FB8FC8DA03399E631A5CA5013C752BB0E6D6627A8BA6D4537D3749DEB844FCCA
              693439349D0DFF0164E91202BC32FEEE0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000506494441545885CD975D4C93
              5718C77FE72D2D2D5FD682B5968D8F028AA86130D4C4B1CCC579831F011713B7
              65D92EBC98C9E29DC41B6F74C93631BB20992E8B376ED9CC1C032746E332E70C
              063598212A340AB232470B886D69A12D2D7DCF2E8A88A25050C9FE77EF7B9E73
              FEBFF37D1E419C52524C18F25E4797594882C98AD06827CAA41A65CCE322E2EA
              24D8D942D437106FB38899020C856F9056BE037D5E69A6109A2AE02D60256045
              9084240038815BC045900D21C78D5EDFE53A02377E0729E706A05D9C4B7AD55E
              F439C525C03E21C496A51906A53C3B8DE566034B527518B40AC1888ACB1FC63E
              10E4528F8F3B8341554AD9081C187575B63EA8FF82F0BDF6D90124976E22BDAA
              5AAF68130F2A42ECDAB2DCA4EC5C6DA1C89C34D380D13110E0684B1F8D76B7AA
              4A794455A3D5DEB38743BEA61FE203487BF37D1656ECB60AC1699B495F5C5391
              CB6B4B5266347E52D75DC3EC39F337DDEE509B946CF635FDE8F49CA99D12A799
              FC915CBA89F4CA6AAB1034ADB7190B8F6D5F4A96513F6B73004BAA8E77576660
              BF1FB4F47843DB12B356D5C968C43FEA687B3A80D69C8BF9E31ABDA24938BFDE
              662CFCA6321FBD569993F943E9340A15CB4CDCEA0F187BBCA10D7A5BE9B15077
              EB58D4DB3715C0FCE197684D995FD94C862DC7B62F9DD1BCB6D9C9073FDD1600
              6B5F4D7D669C4611BC936FE45CA7C7E20DA9C6C4EC5567FD574F8254015020B6
              D5F439C5258A10BB6A2A7248D6699ED9E05C94ACD35053918B22C42EDDA2EC92
              94B24D8F0001D22BABD19A32BFDE5A94BEFCA3D2C57135BAF6D55476AFB34EDB
              FBC9B2A4EA707842E2F6FDA059637AE5C4F0955F0050949485E86DA55621C4D6
              9DAB2DB3EDDCACB473B50521C4569D39C7AAB5E4C5000C79650845535590A157
              E2D9E70F55DBEC24FFD03551DBEC8CBB4E9139898274BD2284A83214AC8901E8
              320B01D697672F984D67E6ACF29C0500EBC77D51124C5680154566C3BC008CFB
              AC4858688D01088D1604D625A9BA790118F7B18A84D86D3ABED94592E1390F9D
              7865D02A20C4C4628BB94A190C8DA9F302108CA82065700240AA51805EA72F3C
              2F002E7F18A097982FCA98C709D06E1F084E53EDC569DCA73DE28E6D5F25E2EA
              02B878A9C7372F00E33E17237D5D3180E0DD6B4849C39DC1806A1F08BC5473FB
              40803B830115680876B6C400A2DE7E46FFB9D92B25A78FB6F44DDFC273EA684B
              1F52723AE271F686FFED880100F89A4F00EC3F6577AB6DAEE19762DEE61AE694
              DDAD02FBFDCD3F4FFC57000237CE33EAEA6C55A53CB2E78C839170F4859A8F84
              A3EC39E34095F2C8D8507FABFF4AFD44D9C49B5097B502CB27DFEA1545D3FCB6
              CD587CA4321FAD66C657FB8C8A4425BB4E7671A1DBDB26A55C37F07D7528D8D1
              34513EF1F2880EDD478D84C60C056B1A1D9ED16DEDFD01E3867C233ACDDC4FC8
              9170944F4FDDE542B7D721A5DC3874E9B86778D2F03F060010EEB9899298EC4F
              CC5E59E7F08C6EF8ADCB6B596549C232877BE2BA6B989DF55DFCE51C6E93C88D
              23D7CF39DD276B80C71395296FAF50E755E458C4AFB7957EE7094517D4DD7C50
              E6F0844496319145C9DA27C3A7A86320C0E77FDEE3B33FEEA9EE40E4B094F2BD
              A14BC73DEE93079F9A253D739213734B48DFB617DDA2EC12601F822DCB329266
              CC8C6E0F0654248DC081B1A1FED607BF1E62F29CC70D0080A221A56C3369E53B
              D09A733305C4911BD21071F7F6FA2ED7317CA51E19199DD622EE65AE5D528021
              BFEC5176AC6863B52520A344DC4E227D5D04BBAE4D9B0BFEEFF41F1ED60A2C02
              4B58310000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000006FF494441545885C5985B6C14
              D719C77F67BCDE9B776DEF1A636CEC18624C0DC450D3B42681DA694543A88050
              055588A6546DD5224122221E4A9546257D8AAA3EB4528225122B2141A1A1AE14
              11CCA5A9AB14572DB7DA2008C576C0C4066CCCDABBEBCB5E6776BE3E2CC62CD7
              B5EBE2FFC3ECE8CC39737E3A3BE77FBEEF534C445A06D69915D88AE761C92B46
              B3BB41810284E4C50C07D107AE12EBFE1CBDF78B094D03C977A62DFBDC25B8AA
              56E05C508BB23ABD0A6A8185C01C2017851D2104F8800EA045E05F6638180D9F
              3FCAF0C94F885F393FF980F6B9D5789EDB82B5B0DCA694DA80E2470AB50CD034
              0533B3AD926BB760B3688475135F48C717D2158020118446E05D11F94BB4B395
              C091BAB4411F08A83973F0BEF02A59F36B34A5D426E0574AA9A232AF5D56CFF3
              F27469360BA63BB159B4BBC60EC50C5AAF8568BE3CC8C1763F03614389C86960
              BB88340D9DF898E0A1379178646280D6A20AF237FE164B6E41B9820F95524F2E
              2971CB4B4F15525DE246A9F4BF0E3D211CB830C0CEE3BD74056388C85E11D91C
              EFEB1CF67DB01DC37F757C80B6598B28F8F1EFD1ACCE3528F6E439335DAF2F7F
              8C95733DE302BB5371C364D7C9EBBC75BC0723211745589D1819E8E87B7B33BA
              AF2B3D406B51053336D5A1D99C1B51D45715BA32DE5A53C60CB775C26077AAE5
              DA085BF65FC417D2FB8115C6D0C099DE9D3F253178FDC1809A3387C2AD1F9099
              5DB0068D3F7F73564E46DDF365383333260D6E545D81281B1B3AB83A18EB1361
              59ACB7BDF37ADDCFC088A7F44B9979DAFADF602B7962AE521CAE2A72D9EB5F28
              4F1BEE74CF08DDC11831C3C4EBCC7C68FF5C8785DAD9391C6C0FB8A286F94C86
              3BEF7DCDEA30A21D27EE0D682F5F8267C5264DA10EE5393367EDF9FE57F0381E
              3ED1A8D6FFB18DDDAD37545837F94EB927AD311E87858A7C07072EF867004EDB
              CC799F86CFFF1D3314B8D5E7963F78566E46A17EAE947AF2F5E58F4DEA37F720
              D5CCCE61C357F305785965582A739FDD94F2DC02C9D5B31695DB80D7AA4BDCB2
              726E7A2B70BBFEB0EA71E20991694ECBB8C76E5B3693C60BFE8C60D4D8E19C5F
              B3CE32AD14A33FB9AB3580ACAA1528D406A554D1CB4F154EC84AAA8A5C5497B8
              29CB738C7B6C8EDDC2C6C5D3518AB5A0CAB2AA56DC7AA6A16590B5A016E08765
              5EBB5497B8C73D012437C9892BC35C1A78F0C9703FAD5F948FA694528A17B316
              7E7B0CD03AB302CDE6F02AA56A565578276CC4AF3476F2837DED6AD7C9BBBD2C
              1D15B8AC7CA3D80DB03633BF14CDE54D02DA8AE701AA06D09696664FE8E593A5
              A5A5D9A054A542796DC51549404B5E31C0424DC18202E794022E2CCC4225F745
              A525AF04004D73B801E614655BE55E51C9A3D42C8F6DF4B64C73B880511F54E4
              7AECE3B787C9D66D0C1E75F314D66E6E09BB758A570F188B2B95B28FB6255B84
              705437A704EA768547194442A36D9A247F7DBE903E154C29EA1F63E89764FA85
              668682001D3742BA1A8E19538496D4257F74F4B6C30C0D02A0E90357005A005A
              7B42F71CF8A8D4DA33822031E0DC4D2EB458F7798063221269BE3C38957C345F
              1E04E1B8209178920B4DEFFD8244782802341E6CF36324644AE0DA7D61DA7C11
              0534C4AFB6614647928000E1CF3F4344DEEB0F1BEA93B68129017CB7A50F4142
              22F251F8FCD15BED1AC0F0BF1B417144444EEF3CD64BDC78B496D3E98FB2FF3F
              7E10DE1133E11F397D381530DE7D8E68672BC0F6AE608C5DA72616914C44A608
              3B9ABAD0136600E48DD0992324827DA9800081C37588489388ECDD79AC87D66B
              238F04B0FE541FC7BA8715F04BD388FB824DF529CF53823FCFDA5F90BDE47BD9
              0A752A3F2B73CEBE0D1594E6DAF97FE9AF1703BCB4FF1286290D22B23E70A48E
              A1A37B52FAA41CC0C1436F12BF7E69484456FB427AFFC63F757079CC3C271DEE
              95039D18A61C17919F442E9E64A879EF5DFD5293DE8441A4E304598B960F6836
              67D3502CB1B6B13DE0AAC87750EA999C953445A83FD5C76B9F76114FC8716095
              EEEB1ABEB17B1BA2DFBD187765E5121926DCF64F9C95DFBAAEAC8E8FA386F9CC
              810BFE1903619DC5452EECFF43D4D3E98FB0B5B1938FCEF62B53A44144D6E9BE
              2F87FBDEDEC2CD23F7E18000667890D0B9BF612B5B1CC870E7BD0F38CFF685BE
              BEEF6CBF16D54D667BEDB8ACE99743DA6E84F95DF3557EDDD44D57201A00B60A
              F26AE4E249FDC6EE6DF785838715302D99E43EB7859CA7D7A1344B25B003A5D6
              66285475899BA5A5D954CEC862B6C7C69D05CC4E7F84966B219ABF1CA4DD1751
              920CA1DE01DE30F5982FD054CFF03F3E04F3C19E9B560A9759F038B9CF6EC239
              BF06A5D41CE045E0F96482C37DFF73118901C78006609F240CFFC89923049BEA
              4904D2F3DA71E59896FC525C5FFB2ECEF9B5644E2F45A1F2802718AB513B1046
              807E9235EA738244E23D1D84CF7DC648CB411243BEF14C393EC0DBA5B9BC6355
              FE64E23556E507CCD058955FA28FC6F4A744FF054019EC15B10EC7C800000000
              49454E44AE426082}
          end>
      end>
    Left = 272
    Top = 48
  end
  object ThemedVirtualImageList: TVirtualImageList
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
        CollectionName = 'certificate-license'
        Name = 'certificate-license'
      end
      item
        CollectionIndex = 29
        CollectionName = 'gears'
        Name = 'gears'
      end
      item
        CollectionIndex = 30
        CollectionName = 'heart-filled'
        Name = 'heart-filled'
      end
      item
        CollectionIndex = 31
        CollectionName = 'alert-filled'
        Name = 'alert-filled'
      end
      item
        CollectionIndex = 32
        CollectionName = 'home'
        Name = 'home'
      end
      item
        CollectionIndex = 33
        CollectionName = 'button-info'
        Name = 'button-info'
      end>
    ImageCollection = LightToolBarImageCollection
    Left = 272
    Top = 100
  end
  object LightVirtualImageList: TVirtualImageList
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
        CollectionName = 'certificate-license'
        Name = 'certificate-license'
      end
      item
        CollectionIndex = 29
        CollectionName = 'gears'
        Name = 'gears'
      end
      item
        CollectionIndex = 30
        CollectionName = 'heart-filled'
        Name = 'heart-filled'
      end
      item
        CollectionIndex = 31
        CollectionName = 'alert-filled'
        Name = 'alert-filled'
      end
      item
        CollectionIndex = 32
        CollectionName = 'home'
        Name = 'home'
      end
      item
        CollectionIndex = 33
        CollectionName = 'button-info'
        Name = 'button-info'
      end>
    ImageCollection = LightToolBarImageCollection
    Left = 272
    Top = 140
  end
  object DarkToolBarImageCollection: TImageCollection
    Images = <
      item
        Name = 'document-new'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000021049444154388DCD904F4893
              7118C73FCFBB9739FFCC326D36F5D0C228618742E850736997A8EC9610E8B93A
              B463E0A5C3D0D3AE5E0BCC832842A7ECCFA881A16412164D59500A210E8D92F5
              42EE6DBEFE9E0E994828E1ADEFE9791EF87C78F88ADF27DC3C5B0740BC39487B
              73D064F3456B64768D704303A18E1B2082E3382493495CD7656724D17698BB17
              C39780758409418CA216CA79B5EC8AB578F293A93936AEAAB7A6A6A632894482
              52A9B42DB010D8824781F8D63D8E308AF1D6E76767D63CCFFBA8AAC3B1582C3E
              303080DFEFDF16F8EE5C38C2D143FECF086F8051412A512E235C07263CCF2B2E
              E757869F4DCF75B5B4B4242291C8CB6834BA944EA7D9DCDCC46A6F0E1A113182
              64040901884848908C25628E9B4553969F2197CB31383858E579DEA3582C76A6
              AFAF0F003B9B2F5AD1700020068C8948BDAAAE025D0A93D942392B650DF4F6DE
              96603028C001557D1C8944EA00EC91B7DFE80F379E431843E9069E03DD5BFBB5
              F1E5E087851FDF871EBEEF3FA1AA0A904AA56A447E976757F97D8056ABD22322
              1900858C1AD3E36CD8D5CE86AFC61813052AD925F6BD575FE9BCDAF9A4E95447
              6D60799AF2A549E60A019EE6ABDE154AF6C155D77E0134EE0603D8CE4F432ED0
              4AA8FEF4D0C2C2E295E2970A6BBE1020E7543C00A240D35E3080BD636EBB9FCE
              BE76DDDA3FFBC9BDDEDE55605956A5EBBAADFF02FE8EB55FE0FF13EC2C91542A
              25FB15FC02382BCAE4583E4BE50000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000002F649444154388DEDD2D16B9B
              551CC6F1EFEFBC27EFEBDBCECDA45431D41AC3745087A94E46B560DB113AA695
              2148BD503AB659E81F205E78E18D77B620A2CCCEE92E94818ADA502FC4EA4DC1
              C86C8A562751EA52F76EEDB6BACE2CD5744D96F71C2F6A746315B75DFBDC1ECE
              87C3791E01D8DEDA40A2C9E5C4F92A6FF4DF496BD43540CFE07BC1A4ABA1297E
              172D0F3F493DD3D3D38C8D8DB15EF4F6D60632FB9368476DAD19FB4368EA4796
              579E68A1D155F71827F24BA97B5BACB6F18E83D6DA4FFBFAFA0E96CB65262626
              AE0255A2C9433B6A2BC8375AC93E2F227F71D0E8A90780AFC4549E2D2DCC2E86
              61F8A588BCAEB51E1C1E1EA6B7B7F72AD0697415FDF7477F55C2691146817941
              7663F91EE110C227585E3C5A8CBA6A53FCA14AA5B2E0FBFE0B8EE39C4AA7D333
              854281B9B9B97FC0F1C1CD344414DA916F8133C0A8200A248DF011B01F301BC2
              52E8FF7132FB7EF678E94470724F5B5B5B9F526A2E9D4E1FBB1C55AD51D7DC14
              51461023C89B826800111C410604A9291113778AA6B9F45D57E9FC2253535364
              3219051CD65AF78F8C8CD0D9D9B9560AD063B16B9F06ED0223883800D6DAC3C0
              BB0006A18A3BD3D5B3A33D765B0B00D56AD5F13CEF1DADF585542A3591CD6671
              7E3EB71AECB87B63E03A720BC221202322F759EC1184A711260D8CBFFA53F3D9
              0F834D0F5689A4C230DC592C160982806834AA7CDF2FE472B9C95C2E87F6B4D0
              E8AACDC0E7583E03F602CF006F61C98AF0B640D955E6E3724D76E5F3F9E7F2F9
              FCDF2524934962B118226BEBD03503581B58E125E080880AEBBB31D68E62EDEF
              8BABEED18595C8ADC0AE75D77CF9B033C72EF0D8B6C4A5EEC79F7AAD7673DCDB
              303B1E468AC7310807669BBC883247E657BCE6B3AB912F807BFF130C0D8CFF96
              A063CBEE47ACB51D2BA7A65EF6960A3D176B32F3E3B2DF7131543B8147AF0583
              B596EBE95E5A5A4A7CF0F5A5AE73676E27B4D25E3192029EBF16683D90E5E5E5
              8160FEF40038D7635C1175C337FF07EBB9A214DFF7492693D705F8BEFFEF603C
              1E97A1A1A11B7D1C007F02ADC1176D8B93AE910000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000003DB494441544889B5944F6C54
              6514C57FE7CD9B768657A648A745515A9C96022321443191120D2A25266EDCE8
              4217C485C688092186689A60097141489AC84213756A8C3161A126A61BD22228
              095AFF60C0D00EF8072348695AB0740A9D7198CEBB2E660666CAD45813CEEABB
              DF7D39E79E7BEFFB04501F7288DF1902E0DC951C2777AE46920FECEE3F93DAD3
              7DF0224D752EF7B4C6597EFFA394E3C081035CB87081B9E046420E9F3E1F63ED
              D2B083B05FC6B356FEC1AA25218E6C6B27E42AE087C2F9D4A627EBF20B1A5F01
              AE9AD93B1B376EB4AD5BB7924AA5AA0A38F7DD1566EDD2B023A957685F7B5348
              005600CD8B6A08079D95924E38B9E9F55CFEED9A999D057A24ED6F6B6B532291
              2012895417004018F097A45725F621244908102B1187118BF167AE9DFFF51463
              63638B2439C0CB927AE2F1B87A7B7BA9AFAFBF4520E01B3CB662210D5EE010A2
              5E68078627A9036314D84541AAD337CE1C9B5AAA7074D94C2E37F3C1C8C8C8EA
              6834FA34B0201A8D7EB161C306060606C866B33705CEEE5A43D4738B1C0C2016
              013B8AF935401AB119B3D38EC4BD81711AAF26C7BE1DFE63F4E0B1934FB9AEDB
              DEDCDCDC21C96D6C6CFCB2A3A383FEFEFE1B228E24BFB831794979A1ED40A147
              92802542C392E30379CFB9EE47B2A36F68F23C994C86BEBE3E06070701BA24ED
              8AC7E32412093CCF2B6C11B0BB64C7CC005600CF96B571C4CC7A8B732AE1E8B2
              96E56C5A11C7711CD2E934E9741ACFF3BACD2C178FC7F7B6B4B4904C2671FBCF
              A4F6AC6A0AB1EC8E1A046DC06160DCCC9A10E3C0DD885A33BA0C3836E6319A71
              996E0CD3F1C4036A68686056816F027B4B776EF7C18B1CD9D65E201747305C60
              B3A45386BD0B2C167A1D19F8740D4DD6EAF884D71DBCFCE7BAC1E19EF5F97CBE
              446E5BB66C51676767C516B94B1606090515003EC108029D60C31486EE03DBCD
              4C925EF3B11F838E7D067C94CBE5769A594D614C73C3BD9E37803C662FFA5846
              728624072BFAF5C1307FBB19DF9D9BAEFDFCDC744D9D99BD0FD4942A2F279C15
              E21E3F9F66FF579778E999C77F9889C699A96F56E4C47B16C84E09E09BF105FC
              3C15B4007CFCFBB55A6F3413EC93F44889BCB8693784245588B8006F7D33C573
              FBBB90F402E67FEF87A33F05B253BB81A323E9205F5F8A74031F9A59A244FE5F
              E1CE8ADF9EB832F9D0D18B0DFC3D36B1C737189A0C01AC038680DAF990DF2260
              664E3299EC39747A2295CDD695671E2C9157E9F9BFCFA03C90A44C26F370369B
              ADE86DB573B55C91A342C4999FE1F963768B6E382EB73ED7798EB842A0C2C1CD
              F7ADE2B1AB7AAE962BC6730BDC0EDCF616DDB245ADADAD159B321FC462B1EA7F
              727935B1588C582CF67FF84B1C15F13F3DB6D7BB23CD95E90000000049454E44
              AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000412494441545885ED965D4C5B
              6518C7FFCF694FE968F96A81968F51BA75D99C0C66D22CC248E6A66C8848888E
              1B8386CC8971863B29267A4964B61B5E10314A1C24C60B2EB8590612343381C2
              1233A3C31075A163E3C38C5281420B5D4F7B1E2F80CA571946B6ABFEAF4EDEE7
              CDFFF9BDCF73DEE71C208A9A2BB3E06ECCFFF29B6A53B42D7B2261EDC190A044
              AA461109A46995007000A0C89A2E5E0183560981B06752AE251F6E78663F188B
              8E9BD3F3577E7443AB520004A3B8CAF4FEC9547C7CCE982208A4FD21E58D8979
              246E6BE876BBD1D4D4846030B87B80B0CC00F32291F0B5ED4563FDC1D4B87B3A
              8D520443979120E2F3F3FB515590FC2C889A3C52DC5B19878E973E976DEADE6C
              C6CC7E00E5999999FD757575BB828814F383D3E9B09D311C04D175306A417C17
              A0DF993995882A015CF587E86CF31F06DF8325F52D599673369BD9ED760A0683
              3E5114CB9D4EE7C06E20224D1FBAEFC7A134F5DC6183FA3722DC00280CC22902
              89003E0370DEE9D6FCDAEFD69632230C600C801640F29A47494909B5B5B58905
              05055566B379302F2F6FBCAFAF0FE170383A40736516AAAD7AD49CD0C1A453ED
              D3C72B96894841441F114824A21700343373AF9242A1A3C9C1E1A349819E03DA
              60972F247CB52029CA0018D7003A3B3B797C7C5C959F9F5F65369B9D8F8310AA
              ADFA6BE78E24DE2E346B670FA7EFF30B82304944B60D7D22B2098230694A80DF
              AA5F9E3D95EEBFFD6AD6C2B55732BD0100BD9B4D5D2E17DADBDB359224DD282E
              2E3ED9D2D202954AB53DC0F77F2E5CB833B5641DF33CD2CDF8258D2CCBD9CC7C
              79FD2666BE2CCB72F66C8035A30B2ADD9D39B575C0ADB970733A410DA0743B63
              97CB858E8E0EAD2449DD3B416CB8D18D6519A82D4C2D02510F805622FA7015E6
              12C065DD139AA1AEA9D4D7019C01A0037002406452D9ED76AAAFAF978928E26B
              B158505353E31345B1CCE9740E6D7E312383E84DAB0EEF14A59941D406E67266
              6E5D3D7D2B98CB016A2B3604CC06B5E404F03280AAF5C9A3697474747D250A1B
              1B1B37C495C0CA84FBB422338900071815FDF77C2E538A4A93AB57635112E606
              A6E3278B0DCB15492AD9516BF1BCDD34627C2FC4B4650E0080C3E110B65B0790
              C8CC3DB9B9B9295B004405414194C8CC17BFFD7976DE767D0A3DEF5AFC2660C9
              1314FD5D53BAD706FF0E0DD65A3C17F571A14405F14C88B7CE639BCDC63B55C3
              6EB727AEEBCEBF0033BE104ABE189D08CB8C91870100C0F4A204303FF0870400
              74FA6140747C3262BCA420763F92E9EA4E89FE8B9400203330FCD7F286C0B224
              03C0982F24002BC3C61462EADEEEE4FF1B60B31A1A1A70EC781ADDBFDB5BFE93
              472600797B9AF57100393939501F29FAEECA8D5F6E79BDDEE7011C7BAA000010
              171757E2F57A5F7A5289D714EDCA3C35C5006200318018400C200610F56B08AC
              FCE53E69807F00871795EAD359DBF60000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000551494441545885ED976B6C93
              5518809FF7EBB776A373ED06DB60306403116274F3322FE88848208068604200
              355E20D17AFF312046A33F8C311A4A887F3019A22021DE4298F76453BC60749D
              78198EC419441C88D9C67A61C5D2F5EBF7FA636319D8B590E1E28F3DC997B479
              4FCF79F29EEF9CF7AD90818EE72F7709F26E7B287EFB351BDB320DBFE018E982
              13F24C04F281629011523A13F3F48715577A9955E6C6B295173FEDA42B6A611A
              82225E11B207FBCD2872E1BB711C22F0653B341DD3B48BC4E371BABBBB872738
              ABCCCDCAAB0AD60834DC7249DE9155DB7F272FDB818017C5E571F5257BC18C8B
              D8BC7C32B94E63BE0A89C56B1EF83C5E36F73260DCD993AB2A40D8B2AC9675EB
              D6D1D0D070DE82035B6CD98A4023C87B133DCEAA8F1F9CC6C29979005EC095E7
              72B0764E11DBEF9A42AECB518BC8FA1309C7D781F698D3B6EDF7813D673F22B2
              4744F699A679BFDFEF67FEFCF9E72D38B07185B9260D0F4D63A2C7792DF011E0
              53D55D22B212D8A4AA13047210A903AE3D95941BB61C1C1B6C8D7AEA1289C4EA
              5493979797E3F3F9445593C00396656D5BBB762D8D8D8DE72C3890C1AEA8C5DD
              3B0E138D279B814744784B449E04F2115C22528AC857C022E0F6EF8339C19F42
              39F7261289559916D9B56B9701D499A679DFF966D280BED35AEACDC293E360E7
              F74154F51D557D4E445E009E16C8059A812B5475B9AAB6B505054F5672BBCBB0
              2F1234179802AC03ACB3170904023A48F2DE0D1B36306FDEBC731234FBEFB902
              A080BE2BE5F4D3A3E87111290110A15855BF139199C0F8D5979E0CC1C990AD04
              7B6D0975C71D7FBDFD8777536B644C19F0F0D90B3537372322464D4DCD16D334
              D5EFF7BF712EDB6D0A528F5086928FE015246BA8C1225205549DFEAEE8290382
              D9A686263A12F595F9B1675A23637E1EEAF7814000C0A8A9A979B55F72472649
              A33D145F6859F60CD062545DB6DA1EDBB6CB6CD56A55ED1C3C5855F7AAEA15B6
              6D4F4AAABAE3491913E935261D3B695EBE3F94F3CC37C7DD008BD365241008B0
              7BF76E03D86A9AE6DD99DE497370F92AF566714F557ECFE3B38BA3021B05BCAA
              1A1091EB5475AF8854ABEA5CE0E5CD07DCB4440B4A932A25F45D4585C01DC0AD
              E904019A9A9A008CA54B97BED69FC99DB5B5B529337946A92BF53A79E4A662FA
              0FC7CD8A2E0076F6871F55D55A8497445874FD781B55A6035FD0772D6D036ECB
              243758B2BEBEDE005E77381C77FAFDFE940767A0924C1BE764EBAA8B311D721F
              50A3CA2CE057112601A81202DD24C841902D15F9B10537169EFC6C6F97FB0990
              CDE964A64E9D9AB290777474D0D2D262545656BEDE9FC9372B2A2A520B3E5A5D
              4881DB311BB813E586CE6822B8B5E9384FCD9B10065008D7FD92475591F54145
              7EEC4896C1E615178796FD1E75D61D8D395703D70C25E8F3F932751A26B0CD34
              CD3753050090BE291C8A2EFEA5F354EFCA6D879932360B85B08095B089FE78C2
              33695F44A6571745F72C9F1C5E91EDB08BBC4EABE368CCD9956AD543870EB17E
              FDFAF49D040315C7ECAFDDA9053F3C708223A1C4E796ADD47DDBCDDFBD366E97
              201056D570AF6D9050190FF2D1979DB98FFFD6E3DAE275268FB59D7055027332
              499C0B22FF4EF48060635B0F8D6D3D6704E396021A0609C792028817C80279E5
              68CCB9E6688CAE7EB9EC0B21980A335DB0B327814250604FA8D784BE0A739A21
              DFB90BC9901DF592254BF861FF01A37BDAB2BF5B6385BE4F8EE501548F84D460
              D2661078ECE0982BDD6F1FFEF5CF48247235E01B09A9C1A41514118F883C1B89
              4446CAE75FA4FDD3F47F605470B88C0A0E9751C1E1322A385C460587CBA8E070
              19151C2EFF7BC14C1D35393939949797FFA71225252543C6320A96949488CF37
              E29DFE00FF0026770FC5B2318B760000000049454E44AE426082}
          end>
      end
      item
        Name = 'folder-open-filled-arrow-down-right'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000025D49444154388D9D914F4893
              7118C73FBF77AF7BE7E6BF9ACED4B983666DA8446A7A3184209A51A7A0400F81
              D52108A14390041EA2D043D7E8961D320FE141A4D243188142925614E6212DDC
              EB9F0DA7733AFF6C7BDF5F8734448DAC2F3C97073E9FE77978446AD5599C571F
              D89514AD0568408802A49C063AA569B42D3EBBBBB632F0943F45B81F8DD915AB
              F63A4B1FA9768DF7638DCD13776413F2FA89B82B86A5699C0AB65F5A8B4F7CD8
              53A02A56ED76963E52EDFADA47524B975A740E25B1CEA12F3D02A88EB82B5A2E
              DF6A6EADDF78B30B9E0884518146D7783F492D9DA99A26E179F7D88CE5940800
              D7781F11774563C64167ABBFD81BB05844FE76C1E8986E5111A2C01A9BC7B61C
              C433DCC1544D9392BAA4630F7FC71A9B07210A46BFAFD2D039144C1A326F0BEE
              BE572B0014A49C8E3BB249D83289E594901A9D6125D72700E28E6C9072DA66AE
              9134A46BE709C5854E54A033E43D7327EF730F00F6F024F6F0A474CC7F23E4F5
              A392E8BC523A5376BEBEB660A72023CDA6ABD234DA23EECAD3C0AE2F2CB98F0F
              9F307BDBECC1B68E81D955B91D4E77B8A92CBD1EB0E4A8AB094FF9B1AE85348F
              0C15D579423EBF235C743290CC703EAC325F5EBB68DEBF6193D1E69DD333D30A
              7139CB87D5A3E141BA7C47EA86824BCF5FCCC45B17CD03A4B188CF1C2C2B66F4
              490A890B7BFD5FD3320102AA6198688AE93B2C3EF59E335ECD0241C005E4EF05
              FE165833017435301701D06DBF1A799BF5D7D8AC590001450F2E0104368DFBCE
              D6064A746583E5D886FE3F0229D105C0C7EE9B166F516E5C88FD0B24924874CD
              AA02BC7D3F69ACAE272CFF26801FD30BFC040A78F121EC8D98DF000000004945
              4E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000034E49444154388D9D945D4C9B
              6514C77FCFDBA77DF9E8606DC767A53036C7D07DB031BD50990B449726DC982C
              0C97E0E2856657266646BDF0C21B679418CD4CBCD1EC4AA3895C986936B38445
              700C6370C9303303C660E98BAC1FAC94D11668DF1E2F3A8CCCE264E7F6FF3FBF
              E7FC4F721EA5AB1BA978ED7374557DAD42BD0574A2941F8823328848AFE4EC91
              3B5FBD4B72E06B1E54AAE6543FBAB2E119A5F8AE386179BC53C3B89231B2A69B
              057F0B0B35BB6D819322F269F4E39759BAF6F37F03035FDCA845A951DFCD4B9E
              DAD13E94C81A43BCAE15EB400F2274DAB727CEEBCF8EAD0B5B5ACEA051EACDE2
              79CB533BDA47E889E3B8C3D7C5333D2CB6B384585387B165BC5F7C93832AB6ED
              D953F5F5B5E72FFCF0769DD390C602BCF981919B5735D0E99DBE8C12C11D1963
              665FB74284D2E838D1A6E79577EA32DEA921E6B61FDA9BB68D8015CF3CBDD5A7
              BF2C00FC09E8D028E57725E700F04E0F0330B3BF5B558E5DF8DBB9AA2733CAFF
              D1B737C2B3566CCD5EBADA031C6D0F006020326F9BA500D8CE62DC91312A26FA
              893407D56A83EDCAEBB96C763E329772AFB7C36A9F1B0D0C26FCFB8E6C0EFD46
              F4D176A23B0FABFB8D097F0B2232BBA36461ECC543D5475796B36BF4C7B79603
              34EC68A8784323D2BB50B3FB85785DABA362E222DEE9E135716C5D44B8398843
              32BD07F5F7B43DB6D8ED70E8FB9E9C23BD94AD2F32BDC7B5E4EC111CFAA475A0
              E713DFE420DEA9215CC918B6CB4DC2DF42B83948CE69F6EDC95D3CDD943CF3CA
              EFE3F1ED85E236D61D26507330A43C1DC7A8EB7987BBB6336863BCAF94DAB36A
              1291590799DE5DB981D35DB9F79EDA82754E41492160F3B62EAA7C7BCFE84D7F
              9CA37FD77ECF2F89B2A1B391F2962BA9CA408A32BF834CBC4626C79FCC9DA555
              7E7CB598BB1FAE070328729503581AC06DAC789EF3C67EF5853FF8A6331BBE94
              C11531C8569AA4BB0DEC6E050563FEB3CC3C30A41753CB00334A19E5887DC224
              75C224F5A0FE02C032809071279126995E591691E8BDB1375C2EA71BC3D00AB0
              0C80997002C032CDCD0F05BC175744541E6885170042E6434E68BACA11917990
              450D3013C94FE8296BA4C1DFB161E0A6523F80058206B8F5671CE04A6971D580
              FB91EA0D0325FF875EBD351B470104DB76F2FA4B6D7955FDEB94FF0F11806B93
              B7F90B52CE4E46DCEC4BF20000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000045A494441544889A5954F4C1C
              7514C73F6F669859D8C052960A84C5541A20B5DAA2B49A482AB56D48B40713A5
              31CAC5344D6C8C4D1A2FC5A4D158A3C61AE3C1683C78C2566DB456E39FC4830D
              283DB452120B86368162D9A12EB034E5CFEE02CBCEF3B07FD856A04D7CA76FE6
              FDE6BDF7FDFCDE64446C1F65FBDFA768FB93A03C202287107681848018AA03A8
              9E54E824955CB8D17994D8B9D3DC6D48F0E047146D7B4A443882C89B66326115
              8F5FC68E4549590EF1602DF3816A51F853D1365487A39FBC42A2EF97BB6B50F3
              D91022D201BCBD2EDC4BD5A56F3117E674F984305BB959DCA676966C7F18D547
              5233D189EB475BD1F8CC5D35D82C86D1B76EF40F2BD4FB3989D21085375D1510
              005555119144A09AE196C37866C157A8B63FED7E4AF5CCE09AC507AF4E6089C8
              213399B0AA2E9D26511A6278E7AB04877EA3B2FF8C2780DBD42E00357D5F503E
              D4C544436B9B081D7B773E18DE19283F22224E661044843CDDFD53F760B705EC
              2E1EBF8CB910D3C2C538C1E1DF255AF7848052D5FF9D26FDC19C93D2F0459968
              68B5405AAE4492271EF37B6DB6653C949D587599ACAA1E03BA2D44AAED5834CD
              0BA4AAFF8C023255BFDB10D034294544C48E4555440C45AB7B47E29CFBFAC278
              7C21952DA892B1F0CD5BCD39371610F72CDBC95D4ABA098868B46E97A029F54F
              8D00E0594E8601F191B1594A1752B9F7560ACB34B0501D88056B772082AAEA58
              D30BB2E82F0711504F55CCDC84B160ADA8AA5AE20D34567A5CBB469DE72D73C9
              B8001055A5B5B91E0B38391F083D3E5BB99992C85FCBED55F14F5D05C0373D06
              862993F57B0046ABCCA99EE71B973635ED6D0E65B1E423CAE83754F5454B553B
              1179D96D6ADF725FCFC784FABEFCCF610C53AE6F6D235EB60153173BB67B3F24
              8B16BB0EFE3D5698069637565654ADDFA68E1D702D52C905B50ADA966CFFAFC3
              2D876BCA87BA290DF78A1D8BAA6739120BD6CA64FD1EE2C10D88A6DE6DD0F3A7
              1E4D766E199F1C3BA0F96B735B5404B70AE08ABFF9592AF6BF430AF31E553E44
              A40DB06E7100A3A62E7634E8F953CF78C72B6AB8DC2550B70216C9AC8AEED8F6
              BA9886FD8100F49F3DCEC5A57B8D73D3E5DE85B9F535D35E518B27460888999A
              1C28D7D19EEDDE8FC9663DBDA58C7F4E09D4ADC25D000A2C3FCD0FBF268A1CB6
              002AEC18CFAD1BE9DCE5F4FCDC9B1CFCFECA9C796296200629CAB84E950EDD5F
              42F42543BD0322ACB99A008E5D8C8801AA612BFB50A0A4D0A4B370E6EC7C230C
              02E380437ADAEABCE5BA857BFE3D64B563978AAAAAA2AE0510999CA5C4EF731D
              3B8088F854B5718DF593FC822BE532751465CC0008476E02B80556911846C19D
              08DC317C76005512AA440D80487416202C62A85D509C9B2E6B7925BD56CEB103
              6410270D80ABEE0D005744C4974E2299584DAF95739C00A02E8001E046A65108
              E7F1CBD95D4DAF95CB387041B17288545D4574D3C67D6CDAB82FBFD26AFA4E39
              777A6E3EDB60069079D063B74FB4C29F2A5761B55CE64ABA23D1D974579F6371
              F2BDF64C61FE77643F95DE01977F01FD8551C543F4F04E0000000049454E44AE
              426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000045E494441545885C5965F6C53
              551CC73FA7B76BCBDAADED6EE54F0673AE2B0112324C98B0C0A6C40748F4C110
              C0B990181D2FFE892F06457C455FE041487C930089E05802A2D1684234718825
              8C900E14235BA194DE41BB96B5DDEDC6D6DD1E1FD66E2C9404BBAD7E93FB7272
              CEF97E7EDF73CE2F579097BD7527AE1DFB14C5E16A073A806621844B4A9904AE
              002773E3A35DA9735F1823E78F81CC311F1200AE5D9F50BD758F0F38ED880F34
              A9C11E2A13B7304FE84C5AEC8CAA5E12DE36744F631FD09EF19FBB9938BA775E
              2014FBE69DB8777CE41332F7FBF24077C3B26B67B08D44518C0904A01813D846
              A2B8C297B13C4C2D1D59BAA6BD62C5EAEFA56124C66F5E9E3380C9B5739F029C
              AE0D747B6A427F00C86213055013F2531BE8F6005DD5AFBEAB28AE257307501C
              CE76477CA0C91DF223418636BD23744FE31317B8437E1CF181265385B5DDBE69
              FBDC014074A8C11E0420403CF3CF79C22FBC85AE36C847757DFB1121A5940250
              833D001D565FF37C00B0BE32716B7AC0111FA0EEF231C21B3A45C6E32DBAA832
              110458EF58BC94E757D752BBB8BA64003342B8CDE3FAACC12988E38437748ABA
              4B47A57DCA7066D178068470AF7A6E31DF9E7A7F65366B0CFAFBEEE8FAE8C453
              1B07EF26D87BE807CC489934AC76F571887EEA7A8F13DED829EA2E7D35EB621A
              563B4899D49369AEDE1CDEBDB6C1B5FFC566AFF82F955FBD1151A61280DE8CEA
              DDE61CEC7B6C9263A89FBADE138437EE99B57946F5025C19D5C21CFCF9EF3559
              43C2135E4F319D39B0797A3F33702AE16DDB563DD84761F4FAF6234FAC460209
              6F2B4879D2A15DB3650DB9E5698D8BC99C1B1FEDD23D8D1F0ED7B734D584FC00
              AC3DFB41F16A84100FEA5BD03DBE3EAFB8DFF5EC64A4330ECE528CBD2B54CE1E
              7E1351B5B513D7AEFD2B4D3277A136D0ED7187FC142B5F02C3F52D68EB76C56D
              22D37A78C9C9076DCBDCD714C5547237921243204CA89D07B1B7BCB612E872C4
              FB9BD4E005EC8920CA7806C36A27A37A49785BD13DBEBE4532DDFE766EEF9D75
              FCF6A394C64BA59A5B2DD5B4ACFB78D08CCCB13BFB1DAF34BA6F9F8836345FA4
              E10DDDE3EB00D60B21DC52CA61E08A2077AA51F67EF3BAF1995AC75F3F49682B
              D51CC05AE104D0CC000E5B056DD5F7BF5C3E72FA464F72E868802D5F47C42AC6
              84032B632C93FD34E57EADF272F53D05E353409D8B7921816980584207C4586E
              523F542F0307EA095C447203480355C01A6013B068AEC6450122B1248066B138
              016CC0CBF96FC194F7D24C005A340D10C953954579AF481E2085446AE5069085
              0412C951B25943B35A4AEA2925C9667182945300005A345DB60404266C56B7F1
              7062F2F60C402C356612E661B3B972C1012C962A40DCBF7537614C030C0EA501
              CA9242E1096AD114330085975051368088164BCF0044A22980B25CC4421B8E3C
              9A80162B00FC4F47706FFA0E9421817C179C0590EF86654DE0DED008E6698058
              0A2989D4387D62CB86CF1714404A0910D162A919805C4EF2CBA5FE3F6B9C950A
              82A27F45F30700D94983E1F4D802BA3CA5FE056F7FDB6D946B336F0000000049
              454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000603494441545885CD987F685B
              D715C73FE7E9C77BB2644996652F756C7921891597E2C6FD91A434654DDA8CB6
              A4830EDAA6EDCC52280B2374D014C6C60A5BB3C118A3E9463BE8C858B3B5A52B
              A36909EBD2422104B2386B0886A41E78AE996DF4A458B6AC9F8E2CEBC7DD1F92
              55CB75E6C593A77EE1FDF1DEBDF7BDCF3BE7DE73CEBDC2A2340BEE070FE1DE7F
              18CDA60711791AD80BF4884893522A0E7C0AFC552975A2742D154BBCFB4BE6CE
              BECD7A4A00446FA2ED7BC731B6ED74831C13E4A0233E21EEC8158C5404AD90A3
              6077926D0990DCB89D05A76F0EC55105C7AE7DF29752ECF8112815D70950B3D0
              FEFC1F31B6ED0A001F1999684FC7D03BB8663E5B718012211ED849A4EF118A56
              E31470606EF0FD5CEC77CFAF0BA0C5FDD07771EE7ED42D22675DD3A33D9BCEBD
              8A9199FE0F7F048E64084FF832E98EBE60D1666CB57506DF2D4427C88746EA0E
              A8B9F71F46448E1999684FF785E3580A3914A8D506EA9928DDE75F432BE51F13
              9101EFA33F00CD527F40B1EB41110E760CBD53859BDCF58CCC76EF5A75B02315
              A16DE46380A3164F9BD5D1B7671D0091838ED949599C7302D27C7518B3FF00B3
              DD3B51CB341BD841B8EF9BD517F8C7CEA2950A0111B9DFB8F9EEBA035A81FBDC
              912B350F7DE3830098FD0740295A26FE5E6DCB37F998F76CACDE5BF2599C3363
              A4DB837B6D376DFE703D00B71AA9C8171A7CE38328C0BCED0901D452C8E5D253
              11D2EDC19EEEAE36BE73E83E0AC512AFBF7791A958A60E80224EAD905BB1B175
              7C10905521B5420E84A6EE0E2F2F7CEDFE5EA0F8E3C3FBFE198EA62814575D6F
              2BEA95B7CEF1EA5B7FC38A52F182DDE9BF5EC7D6F1F320154B2AB5E2D78ABA13
              1489743CCD9899D9B1A5B3F9F782D0F915EF9AE08017BDCD8EA35076F1956C4B
              608FD71CBA6EEFD67F9D0704F3F627C54886B114E66BDAB3DE00C0A72933C4CF
              CFFC63ABD3B02AAFCB0622374CF6F4439BD87493AB7A6F054E27376EDFB361F8
              14B2C440F1C00E169CAD3583F5F414F32D5DE29C1EAD76CC39FD645B0202EA03
              5B788444267F4F2293C79CC9DE301CC0DC7C6DCAD4945227169A7C73F1C0CE9A
              86ACB79339FF969ACBBA3087737A541949B3DA2FDAFB000A2ED8AEC52F39A747
              03C05D6B22BB8EACA56C2A6669F21C8DF43DF20B676C0CBD92E63A2E9F5C7570
              62633F89AE3B8B823AB2DF7E8598433D97C8A0D503ECD9A7EE66E01BB723CE7B
              9FC037F0334DE0A43D9B78F8ABE75F63A5B0B3125CE88E6F51D2ACDFDF6E1B7E
              E9E5CE73B774FA7C17017B3D002B2A0940EBA15FE3DCB15F074E68A5FC636D23
              1FE31F3B8B25FFC5799473FA89F63E40A2EBCEA2821F06D4F04B8739E20E18B9
              7316CD7E73BDC85A3C5BE8EEB8372A002E9783C75F798321AD9799823100FC54
              2B15BA9C3363E8957AB0A897EBC1AC37200A2E08A523DB4A8317064A3FF2F831
              4F0175CD735D1B76B339F0E09015C0E7D6F94DFF68CBECC298FE8770FB1B7F8A
              05DF8E6BEDFBD2EDC1BDE9F6600F42138A4A45AD3E68531397F6955EE72E75B2
              5F67FE4D20584F3800DDEE0130AD8B0F9452369FAD34FCF5FCAF5EEE2D4EFE76
              84DB4E7F26779C9E914E0A4AC7AEB2B431494FE9133673699BCEFC73C0B72987
              AABA4BB7BB01425600732A098A69D1C45128667F62A8E40BB77266F05675E622
              30015C03DCC0166037700B95EDC27AA90258B660B1A8988AA5D506BF3BA2DB3D
              DD94AD724FE56A88165D5C8D595767D20021DDE66E14D31209765BB3B014301C
              4D019815F286CA6E73A195B70F9F0386A6920066C5F70DD52283522AB4DCC55F
              1A40A54802992AE06424015F12173B0C3FA0C680CF13BB5971B1CDDA2422EB12
              DAFE6B1965234D8C87679700469328A5422242A3DD5CF16268329260D91C9408
              506A3C6039484F849700A6323932D95C5E29156D3C6039489B53C9DAE2D2AC86
              9A462E140DBBCD25AC0CB818AC1B6741DDEE42440330AFC6D22B5A30A4DB1A67
              C18AF7944299E1A9D432C068E3B3896E73A314B328C99AD1EBCEC146BAD80328
              13602A96A92D36CD680A14A66E77CBF6DE67D67666F13FCAD05B00CC89C82CB0
              AC1ABE3A93066118E4456FF32600640DA7036BD5929395CB95055B5B157B5C06
              CF3E55D9FBFC1FC16A54811C199FE6CF1F5D6E0CC38DE8DFCBC7759309A8C222
              0000000049454E44AE426082}
          end>
      end
      item
        Name = 'save-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000000B749444154388DED93310E82
              401045DF27DE024B63AB07B0586B3B6A0A1B3B4A13136F400D47B0B0E206062E
              61A9F1248C8D1288024A2C7DEDEEFCBF7F6656B7E3B9E47B0C10C008A0480B03
              70915374D85957651AC6CA93DC24E122276F807B83BFC0630A75D230D66081E7
              38FB905499FC3E828B5C67843CC9CBFA0B5E0400DAB6F15D7F5A2304412033EB
              ED896766577FE6F7DD6B17009693C5E4329E8F1B075996593D6B1BDA875B36AB
              B50F9C244D3F30ADBE32C01D156D2FBAE364A45F0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000014749444154388DED94BF4B02
              6118C73F6FBE06C2211A82D835A89C834BA3D070E30DFD01FD014D6EF9873415
              2D8D81434B53638BE0E4D0D8A4740969C4499D702291D7DB101E88A7DC6134F5
              6CCF8FEF8787E77D9F473C5D3F7CB1992940CC1D09D06BF79437F2D0721A8669
              88B3BB4B1585D4B0EAA2DBEE32194DD45C2B01BC91873B7083C2EEEB63E4F63C
              C7633C1C07FE56646544FB076E6E322C58C9977F17D8B0EA222C1E1BE80E5C5A
              17AD489F7A95FDCD0C8BB56224B1DDB1951062613CAB80E2F96DA8A69FD35050
              2A99626F6757D81D7B693CA140809BFBDB953B5DC9976958F5D0DCDA1956AB55
              4CD35C571206547E3A9F8E255A0B548AE3D241C9D7F7F5A5A4E338F4FBFD5840
              39F3674D9990BE611A57725B26C62F3FB74DCF16E01DF858DE1C3D5B0020A367
              8247D6721A00E2B066717E728A4CC823014D8448C66868E1FC037C0345245E38
              BD388F800000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000001F7494441544889ED94BD6E13
              511085BF632D91508A8D9520479B14969D8D5CC494D0E1C6294C832C52F00E3C
              02AF414B47A434EE208D9BB4A4238886289123D98808A47528904291A1B0EFB2
              3FDE380A12349C6A66EEDCB9336766AE067B1FAF00CCCC24E9A672C119809CDD
              CCCC03188FC6160D2324D9D2DA127EE0B37FD4B7C94590C8C959BDD36C130D23
              2E3E5F9899515E2F9B1FF8786666D1306270383080EAC3AAFCC0E7EDFB7E1C68
              1E24D169B6351E8DE33808F9814F491338478984EC6A9E2127754089C7348D0F
              40E9A659DE169E4D004C1A6558DC34E754243B3D4E3A716ED356FDA7E8DF53E4
              1595F6F87E3BBE93302BE396D57328A4A8D36C5F7BB108598ABC69B50693F2CE
              0ECF18BC1BDCF62F92FB83FEDA14CD7D2091704A9EA5CF42AE07EEA0F5BC15C7
              49C6CCBEEF84839707E9712EEA817B48928ECF4FECD397535CA2D3EF3986196C
              AED6082B7501E97ECC1B5380E3F353F68FFA739ABCADB0522F8C51CA8DA91BB3
              DF66BADD2E8D4623E5336BBB53F793142DDC5DC8ADFB745793D4A5684C0676E7
              B3282A99D99B602B50B015CC1F895BC0C3D841EC85ADF08924461F46398A7ABD
              DE5536EB249214C53647D1D7EFDF7EDEF3579E01BB1B8F369E2E2E2F72F9E312
              49DA5CAD99B47D6D6561A58624551F54E3BD308CF25A190085EB75765FBC62C5
              5FBE23E935B0F3078CE4F00B6CC779C66400B6F80000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000016749444154588563601860C0
              C8C0C0C0901B94C1A0AFAC4B578B1FBE7CC4D0BCA8938185818181415F5997C1
              C5D8E11F8DEDFCCF00F530030303C3E57B5799181818200E808103530EFC87B1
              1D721C18739694FF67A0029812D3C9B87FF2FEFF8C8C8C70B361724CD4B08012
              30EA8051078C3A60D40103EE00167C9253623A19F1C9D3D401C8F502350023AC
              224003031E05A30E187007E0CD053080DC802005EC9FBCFF1FAEC44792031818
              1818486D1D119B85073C0A487640505010E3FFFFFFA956460C9210F8FF9FE1FF
              FFFF5FF9A5F907C6012F3FBC66606060F0D1F3D1FB2A2023407F07342E6C6738
              78F1C8412616262F5D1FDD2F82B2823835AC5BB7EE3FA1AC45B2037EFDFEC590
              D69BC770F0E291234CCC4C1E3ADE3A9F84E484A865075E80E213361636865925
              9318ECF56DCC191818B6333232521A1F28DD316470F9DE5526DFAA304C493616
              3686AE8C6606254905062A863406B8F3F41E43E1D40A9A994F34000031285620
              092A8DFB0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000272494441545885ED98BF4F13
              511CC03F6D5A4BB833FD75B451624008DA2EA681200903A296184D1C982C9B2C
              264EFC03EA208993830E2E2E8C2C261A571D205D6A40A189694A0C3F1AB529B9
              8652A84949D17368B9B46A4B8F6B6D87FB4C772FDFEFBD4FDEF7BD77F70EDA1C
              13408FF71CC3BEC156BB5410DB8A134BAC610118F60DF2F4FE932BC0780B5C14
              4A0355C6C2B3572F1655C112E38707878F72E99CDAD071BA835F1D0ADF33C9A6
              98753BCF62CAC3C1FE81DA264A22169B056011A8102497CEB1FA7A55BDEFBDDC
              4BA1E727CFDFBD6C8AE0CCC43D2C5B66124B09B52D3019C0D1ED50EFCD4DE9B9
              8118827A3104F56208EAC510D48B21A897B617B41C17E0129CDCBA146C4AE72E
              C1C91ED99A313505F37B791C5907A3EEE1868AA96421BF9FAF19525330154F91
              8AA71AEAA495B69F8386A05E0C41BD1CBB0F96533AD0FC7944D444E65B4689BE
              89D61DAF491020B2BEAC7CD8F8A8350D8091BE212EDAFA35E56816DCF991E1CB
              F686D6340006BC7D60D396A36B0E4A9284D56AD5F38863D12538363666B2DBED
              8D72F9276DBF8ACD008A028AA26039A5794A369DF2117C2B4842C617F4FDFDAF
              A985980196D73E21EFA6578009AFCFBBE30FFAEB9294655929140ACD174C6C7F
              656A761A399B5E4121E8B9E049FB27FC984CB52DC3E130D96CED0FCE860802AC
              2737093D9E46DE4D4781EB9E018FECBFE1C7646E6DBD2B56C5467293D0EC34F3
              0FE73E7B1CD2B5AEFEAEF7C21DC15BC817CB284A2223F6A1E2867B025C8213D1
              2A12980C548D1125B1BAE091E4D4EC5DE61FCCC53C8EAEAB9DAECE5079A9DD36
              176ED17522C1124A1DEFF385A38BAA817D67CE737BF46631A805558EC49688C4
              96FE7FC75AF90D937EA1AB9C3603B70000000049454E44AE426082}
          end>
      end
      item
        Name = 'build'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000016A49444154388DA5923F2C03
              511CC73FEF5A8D568E33906028B66B94C4A283A984C972233159D9A423916E1D
              2516038359C792480C42BA19847767510B1A9DE42222F49EC1DFEA5D11DFE9FB
              DEEFBDCFFBFDBE7910A0487C80F8C6C5727CE362393A940E3A861658F9A5FE0D
              087F5DC446A6689D980B55EF6EABEEDEE6E72B519D8E85754246A7E6EE6F79F7
              47DBFE8096912922BDC902B01EE94D6E2B00A5689F59418BE973C0989E9E9D0E
              043C954B44875804764346A702F5DA414C9F05969452E9C7D249CD08229BCD52
              2C16A9542A20349A862771BB92836EF7E08EA7852F8190F0AA3D7AF96C5CBF39
              3D7F3E2E40F519C33048A5520829A597C964D4F7701EDABA298DCE03D07FB04A
              B35BAE0B3097CB8970DDEE9BA277D7F41DAE01F85EF6CDA01E72D5A85C0B304D
              13D3348594520124128986DE719C5A806DDBD8B6FD9185E3383F7AF0F9899665
              09CBB284524A0579DF11DE95CFE71580104204F9861DFC55424AE9FD07F002EF
              469B357AD7D9020000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000025E49444154388DCD933D4C53
              5118869FEFFEB489F5F6CF7B13A806120B0B24BABA902EC6902E26240C6E06D8
              1C8C8B89030C3AE9C860620C9D347121C689110B89607020C634E1928261A8D2
              56688369B5947B1C9486524A61F39DCE7BEFFB3DE73BE7E4830E0ADDBE4F6F6A
              23D59BDA485D1A7BD6298ED631714EFDFF40E3F887F0E82382C3130650DF7DF3
              A4A5C0BA7997C89D490D606FFEB5B7FB6AAAE9BF1C2F883D4D63D857DE019F81
              C9FDEF597CDD7D29807A293FA6851C117801ECA3D4BDAD89BED33B54DE01C073
              84B78032BBE3530A050AF49003300D8C02B7BCDF959613E85D5D5D1886818860
              9A262AE772E15A228B6E7E12D1A6117C2842FFF24302E3E2D593FAAF9F1FF766
              1E20E56D4CD3241008100E8791A5A5A5C4DCDC1CABABAB789E0780273AD5480F
              E5CBD78777AE0E3DF4346317D0B583FD60E4EB87C7C16F5FDE077E6CA079F546
              678383832493498C5028345F2A95D4210C40530704763609EC6C7231EFB27563
              3C02D0BBFC122BBF360534BF0450AD56711C475AEEF0B882DB197A966740C0CA
              AF758AB73E4A3BE859D5048CC7E3F87C3E4AA512854241F5F7F70B402E975300
              B1584C00D6D7D795E338120E87A9D56A64B3D906A36952A2D128B66D635916BA
              AE63DB36B66DE3F7FBF1FBFD0DAFEB3A966561DB36D168B47D872B2B2B47AD2C
              2E2E36D600F97CBEE15DD7C575DDD38F7CA891911101C86432AA5028A84422A1
              01A4D369CF711C1918181080D9D9594F449AA6ED44E0C2C28202A8542AD46AB5
              862F97CB54AB558AC5A23AA9AE2DB0582C1EB572C44BBD5EA752F93B72C7BB6B
              0063B158BB0DCFAC4386643219AF43F65CFA036899D6177A35E0F70000000049
              454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000313494441544889B5934F6C1B
              5510C67F63AFBD0DA69170ECAA2E3248095215AB82880B08E24482DE108744BD
              2021C411550A47E054CEDC01092E1C532112517AE46F9C8038853F227AA6024A
              9C40807DA4A56DD61B3BEC7070766B3B1BD541F09D667766BE6FDE37EF0903E2
              E4854B64EF3FF389881006B79ED838FFE0407DA94105FE2D8E2CA0AAFFAF8088
              1CA9DE4924C91C2373EA0100F6B6B7086FFE99D89C3A3E82932F01D0FEF507B4
              1D0C20904A537CF16D8E551E4B01A9BD6BBFEDFDF1DA33713AB2C8299639F1D2
              3C4EBE9455D5D6EEF75F62DF384FE8DFE8A54B9ADEAD3C0EC89B20EF39F794DC
              132FCF931E1EE9E44510271B910F039F8AC82BEEE947708AF71D9C37F1EC1D7C
              85C853C045275FCAA6F7AD5055486770F2A51C7019918781B5C3480E0868AB49
              F0CDC728BC85EA1CF034300F64BB6C1A02DE071ED5307C56552F07DF7E46EB97
              2B0704646E6E8ECDCD4D1A8D06BEEF777E3A19EE7AF2799A853176874B2FB4EF
              2EBCAEE8259493404AE19A889CCDEC6C3FE7DEFCFDE2D0F655828FDE21DCEDF4
              67B359CAE532E57219595D5D7D756565456BB51A4110DCB64184D071D9CD15B9
              71EFC439EFF4D9338AA88840184AF1CA87ABC7B7BEFBC0BDE5916EFBA01A5FE1
              4C2643B55A657272521CD7752F846148B3D9EC7D41AAA4DA0143D73718BABE81
              13FCA55B0F9D030D39F5F5BB9ABFFAF90430D1DBD2A168B55AA4D369C9E57238
              D28102C8FE08AAAAFD71E1A7E59868E4E72F404492EAA238AA4D7C688761E4C7
              9A1EF529275ED36E8E7E3E19B02E82A31D00502814A856AB0062AD656969299C
              9E9E4E150A05006AB51A804E4D4D0980B556BA6A0460797919CFF36E0B74EFC0
              F77DEAF53AAA8AEFFB8888ACAFAF63AD25CA03D4EB7500767676E21ACFF31091
              B82616E8FEF07D1F634CBC30119146A3A1FDCE1863A258F76B7A967CA845DDDB
              9F99998909171616C24AA522E3E3E302608C51638CCECECEC63B5C5C5CECE94F
              B4A8FB9A453668E70189B5568C318808D6DA48A87FD1835FD3C886E8E8D65A3C
              CF8B6D101131C624DA32904583C477CAC5168D8D8DDD7192A36274741400595B
              5BFBFBBF24EEC73FAA829AD70FCA68D30000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000282494441545885ED954F6892
              611CC7BFCFF3BEFE1DE674A1C4A87450044D8240DCC190825A302A50A8E1ADED
              E261A76097B173909D82EC1263D031508A3AE4824E42652D9A30E93204D78A86
              2375F96AEFBFA743CE24A68E77961EF63DBD3CFC7EDFDFE7F9FD9EE7790934CA
              36390FDE711C6AA588AD8559AD36E0B5261A4E7A8F185C9E0DB9F86D18C057AD
              3E546B62B7740070007000D0FE1DE0F5189ABA03DE394285374FD4ED978B1D0D
              CD63D760B93845D5D2A65A78780BACBADD36BE6D074CA7FD18F05DF51B5CA36F
              6D93F3872DE3D31D8B0F4DDFB51B5CA3AF4D672E9C1FF04E74046E0BA0940B60
              AAF201C0368025DBF539BBE5D2EE10F5E23642B92500350069A5B4B93F003197
              C1D6C2ACC0987205043F0024076FCC0DFE0D512F6E25944BD68B4F7C7F7CBB52
              5D79D51180748CF853C04228F70200C7181B872C9AA9DEB8A1CAE23003A9508E
              4F02602A532F979FDE2B979FDDDF8B3548241281C3E1403A9D46A1506819A83B
              E185303276A87CF4EC92A2332B54AADD647AD32722564FA93AC32227D538CB97
              95F181CF2B2529D37AE756AB153E9F0FB22C231A8D82F7783C080402C2F2F2B2
              EC743A5BA396F3B07FCC43C93EE772FE1923A37CE6A7DE047DAD98211549E74E
              C56A9C28AC0300DAF9000887C37C369B3503F56B482935124258DBACBA78A90A
              772A86DCB9191D001055D1B9530FC04B5523C89E260A4A2921F5584D0F112F09
              70A762B0AEBF67EE540CBC2468B1F9EDA53951ACE0D8BB479A0BEFA8E74F71CF
              01761D4130186C9CA6783CAEEE7C874221BA9FF5C6C96B527F7620914834AE64
              3375B7D69BD59F1D68D67ECF43AB9DEFA8FF3BF02FE6DEAC9E77A0E7008D1144
              A3D1BDFDCABA0D90CFE7B1BABAFADF3BB1B6B60600F805E3DEF7FBA1320DE900
              00000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000432494441545885ED965F6C53
              551CC73FE7F6DEB6DE15EF2DD2D19941670B99013230ABD107367DE9C6882889
              4B4C7C312424F8A2E87C3762427C53F740CC0291680C8986487C3321ECA10BCE
              2C2EB20289C8026B718ED9C1DA8D35FD7F7C7034ED5CC705AE32927D939B9C93
              DFFD7DEFE7DEDFF99D7B043648F50731F6BE5D99CF7EF709E53BB37658A3D861
              E2307C7876F7767B76F70E7A76F76E142EDD0E5BC026C045F9819701B78D9EB6
              02FE275A037C58AD013EACD6001F56AB1E50B57AA37B4727CEE02E642EC3FCD9
              93502E597E8870E9AC8B1C00C541F65294FCB50BF602BAB6B6E37BF738C2A1BE
              01FCE00AEECACE0C1CB604295C3ABEF74FE2DE1ADE0C6C29771F1CBC79E4158A
              7F252C015A2AB1DAD88270A8DB816F80EFF5708F6BC3A17E501C56E19A8173C0
              D78A5B773BBC7E4B7096018BD313C852F132705008BA80D37AB8C7B9126415DC
              D3C03904EB81D7CAD94CB6347BD35EC0DCF82833038791E5E257120E21440FF0
              ED13E13DDA5387FA114B2015978EEFBD2F716F696F0206113422D953CE2D8C26
              3F3F60B9BC002BD7A84AC5A9710A7F8EA3B777FF2A84322D84E803B6694D5BCE
              389F692B3BD6ADDF2584D82FA5EC77B6BE9876B5ECD8288418049AA42CF7C842
              7624F9D9017257472DC301384CD3C4300C344D435114545545D3B4652F9271B8
              F507EED61746A5A2DC168AA30F7856F1986784106D4288FD48D9AF78BC2E4551
              0681664AC5BD8EDC9DE1F9531F52BAF2735D6F4DD3703A9D783C1E4CD3440841
              3E9F471C3D7A94AEAEAED668346A9C3F7FBE98CBE5EEF95625D5C5C286ADA436
              3FFFCE5CF3736F512A9C72CF4FFF9EF56EFAC87D7BE2F5ACB9E9882817B77927
              863F7872EAD2907EEB1A4AA9704FDF868606229188160E8713274E9C983A76EC
              D83FDB8CAEEB5FA8AAFA523E9F4708714F23B594C798BE8C317D9964EA86BCB9
              E3D5374BAE060950741BA745B948CB4F037866C63FAD2459F0CD6432F8FD7E9C
              4EE7C7C01158DC07AD40D593EFEA39A4A2C8E9EDFB0440C9D940CBF0009E99F1
              07F6ACE6B1FC2759498D57CE228543265B2304868FE3495EB5C316B0091060E3
              6F3F62DEF805D7C28C5D9680CD8705BBE1E03138CDAC7AC0BA6BB0ADAD0DC330
              2AF368342AEF8E0DC360E7CE9D95568BC7E3C4E37159952B4CD3B49C9B48D4FF
              F5D505340C039FCF57BDFF541EA2695A4D2C994CCAEA5CD3341F38D732603A9D
              AE31AE56A150A831CE643235F1542AF5C0B9960163B158CD5C54ED9E7373730C
              0D0DD58D5FBC78F1BE7257D2E3DB244BD5D1D15119A7522962B158A54C814040
              0402814A7C6C6C4C2E2E11003A3B3B2B5F309D4EFFAB3AB600D65BF400BAAED7
              C4354DAB89AF946B1B60F5C25E6C828A32994C4DBC50A83D5A55C7AABFACAD80
              2B3545229158BA975587EFAB29966AD537C9AA07AC9438180C1289441E250B00
              5EAFB7667E17F042281422140AFDFF44CB6BE2EE40BD7EFD3A2323237D8F1066
              594D4E4E02F037CEF08396D4C25C2E0000000049454E44AE426082}
          end>
      end
      item
        Name = 'build-cancel-2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA864000001A049444154384F9D90CD
              4B026110C6E7752DB2A43622A193E9CDED188B069108097D4356441145C7A0B3
              1D3AD6C9AE79F15244449740A8C432FA03B4AE2AD8C553204407B78FC3AE6F33
              DB6AC46A563F18E699619EE17D8729B27C000067778A72B1502840CCED866057
              D714F6E6927DD2FA46FF30A8A5223C9FEE62CB0C2D98C67C8CB1FCA269971D82
              3089FA0463C59EC99C63FE11CBBDA2D0D00AC6099A7728535D787F6F6A262CF3
              F8EC574DA3E17DC6D836658DF3F345EC3782D9EC86C20531970BDA05611CF526
              E77C8FB2C0D838F51B61ED751A0A178C8AE218E6538CB50AE75B94A91EB4DBA9
              5F974AF9C9509F473C2C0B423C69B3C5AF551546AC5698787B9BEDAE54663AD3
              E97563EE1BB22C83288AE0F3F9803DACAEC2E3D0D040249148E1171CC079C9A9
              AA41B7C6B3B76DAD470C60C9F099884422CC72E5F5428FDF7FA39B09C61CC596
              96543373158B913FCD5F387E6326AA0B7442A110A3C0D7F0DF68F2B068340A81
              40400B87C37AE32FD00DAC86AEE1F1782858369BD5174A9264D2F97C9E4A1DD3
              825C2E47517B0D0ED7D555BEDDE03FD46E60D47F04E003C0B6AF9C90ACFEFB00
              00000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA864000002D249444154384FA5945D
              48935118C79FF3EE4BB7E91A382782B224886157B5411042178936E863D7DD14
              74951762107D1092C5CC8F6E8DAE8A88E883120BCA5518E1EA6622080B0AC5C8
              799320D8E65EB6C9F69EFECFBBBDE2C291D50F1ECEF39CF39CFF7BCE73CE7B44
              2618EC27A2BC9472F8E2F232DD5E59A1A19616EAF17A2D42880718BB3BDEE47F
              DBE7D9079728F5FA0E157E7CD3FDED60C14B822822892EC34662E93475D4D5B1
              D8538C77C28E3B6766DEEBD93B40810D41E82A446FC22E74D4D7578815A4DCB1
              18634A158B74A4BEFE23095180E020FAC2B000EC048B9D5E5CA4AFB91CE7EE08
              650C358BADAFB33F8A957EC1EAB8580F61538F575769626D8DC7AA62DDB3BFEC
              9550F800503333FC47B0DD24E57D889E857FFE5443039D6B6CE4BCAAD4F80F95
              BD124A4F539319022C76147612AB3C8313BF86ED8FA2BF6FB8B5554FAC8A562C
              3B25144CBC8736A4A1766921DE8DAB2AA942DC403C80FE5BA82DAFB62A854F4F
              A8B6B6965C2E1779BD5EFDDA74E56D367AE9747E7865321DCC09414E4DCB1E53
              D5785736DB9BB239277A5D765F797E55DADBDB29140A91F9FBDEBD6FB4CE4E6B
              7261E1596E6E2EC4831945A1E70E47FF67AB75F0674D9D5BCA8D11ECE4803EB3
              0AD96C963C1E8F505EF8FD640E06C776793CBA98C186A20C246CB6EB49B911FD
              93D856F862336DE5762BD0A12B7F23C618823A76BB9DFC58311B0A2D1B706D8C
              18272F5B71E2EC73CBB131C679061582168B451F64639F3F60C40C9F24FBDC32
              C618E719F085DE24954A512C162B4724D2782892C9642900894442F7198EB7E4
              525B5BA96A158206BCA54020C035A4C9C9490DDB123E9F4FA8AA2AA3D1A8ECEE
              EE160E87432C2D2DC9D9D9597D8EC1B6822BF8BFA7A7A7F1D3E0A1CCE7697E7E
              9E572A8B784898783C4E269349E6B679342A6A68C022AB7818D8344D13994C46
              F7D7F050F056B9E598FB7F6773856EB77BB30EFF427373B3DE8A482442E17078
              0AFE61BDE7BF20FA05EFF603679D4BB27A0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA8640000038749444154484BA5555D
              481451143E77767756C80A73234225DBA87525A23F2BC21F0291A80493282A2B
              0DECBD07C197A487DE7BA828A98888C4A895A82C021FD27E51247A51A31F52B2
              073583DC76737667A6EFDCE6AEEBDFDAD207C77BEEB9E77CDFB9676647112E2A
              DA4F4495B66D9F1A368CF89EF7EFE9F3E4241DF3F9E8527E3E6942ECC07923AC
              E6F6F275E1C6CC5570898C6F1FE8D78BBBD24F0516B82184380E811658ED57C3
              30AF8D8C50536E2EB9FE923FE6448BC4AE253DDDEFD84F071AD9763D885BE11F
              81D08D5C5D779D05393ADF8E58BBCC21DA1BD75C699333341E0BD613B03B10A8
              6111D84E41F484CF617BD0C4EB071361B8E943F8BD5E6A0F0428D7EB7583F416
              C80FE24626CE7EC1F6C17F191A1FA7FA6F2314FB9DBE88C60FF4FAE828A14BBE
              C90587DC057B6113BD89D9369D1B1EA638D685B0B4EAB4E34D413B969D4D6772
              72089D6FC5FE216E11C1DA81FD5EF8D73C42B81EE186FE0CAF2C48858CF5258E
              3705EDE2EAD53C6826E799BBD067256EC3A3B90791E3B0AB39BAAEDD5FBB5616
              A4065A9A010DE45B40C2E4DC22933FC75862F08F422484F5048BE469F281A744
              A49769A6837F074C526E0851F9C9EDEEEA76BBE9E9C404D52C5E4C9B0CC3B3C2
              B25A5CB851D4E529F5BD79D5F3B76C6E048341B9EABA4E797979D25820D35CB4
              28F02A2BABB7D5B2AABE9BE60674CEED9AF9F1F89543E1F0CF402CB6EDD9CAC0
              CB9BE68F26C9B0003C1E0F9594945071713189EEC38769D98103F4C5E33979BE
              B9F932CE13A3C044DF6659D6EE6CD31CFF9AE92B8A1A3F1F23968506167CA52A
              2A2A447979396977F1AB3582C1AA956BD65CC1ACF17510F249C95588CDE39AF6
              F4A3AE57287275960A9CA3A0BADD009BEF216E84B5ABCE935B4FDECE384A604E
              D2194D30D4ADA6BD88C97973D448B8F98F12E72E0A0B0B45414181CCEEEFEFB7
              FBFAFAECEAEA6A552D42A190C5397863382638870D398966DBDADA1CCFB98152
              C72A46F1D940010D0C0CD0D8D8188704FB6C1CE73DC7D59E7D2E55356CC99835
              2214C8CE9D6229CEBE43603B022A663B392C286BD892316B44D2017CF88FC6EF
              3220093B3B3BADB2B2328DE38CAEAE2E995B5A5A2AF7C8112A968C592362B01F
              89441263191C1C9467BCAA31F0B9CA6153390C49E660CEB788C1C56A0C434343
              4A20318668342A9272586056F70C29903C22C6BFF88CF9CED857100D0D0D5457
              57D784785347478713FE7FF8FD7E6952A0B6B6963F62FFF4214B0F447F00550B
              FC40B67A17750000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA86400000347494441545847ED966D
              4853611480CFBDD72DE7DCEE54120B3241A7E627E4C43FC112123F22F3A31F25
              D99744FBA3148533EA77E0824043A4D2EA4FA6D5EC032AFC1DFD114CAD36754E
              2182B4106DA1CE7DDCADF3BEBB4CCB65A217F5870F9CDDF7BC1FF79CF79CF3BE
              77309B9BDB31ABD355F5A7A7435C5818846207C3C08BE46432B70CE5F9687636
              230EAD1BD60FF01A18E6BE56A138F12635759913C478A7560B8778FE28AA8F51
              DE3A04019701441DBF0E3BEBEE414CCD4DA2AE09A62F230392158A93B8A536BF
              DF7FC6E67476968C8CC077AF3768BC80E74B716E17CAC549B7FB4ED2E0205D2C
              05DCAB991928E6F98F3132D917062311CD71F642B5FA738FC301ED8989C4F811
              9CF704E512315E343C0CD38240174B01CD659C4C066F535200D3700AD5BB8220
              9CF6084257B85C4E8C939D5F9E70BB5B8BD1B8DDE5224B2483233FB33E1FBC0C
              44621023F19565D9368E65A33022261CBE823B6F2DC6B4486D9CF04735EF95CB
              61203313642CDB84C66BB1261E60778DDE6A85BEB9B9C0248961C5272DB8A684
              0462BC04D5F368BC039F5528C76EC5C7839AA3C1921CFA565AED494950A0D110
              E3CF7C8260C4A3D98051F886216ADF25978F1C54A9ACDD9826979F9E40C9E096
              182F46DDECF3F91AA65DAEDBD5E3E3B05FA91C8896C926B0BF6DB75C3EAC57A9
              86A476827B4ACEB94653843BEDC6B037FCF4789ACBC6C6E03DE65C2CCC7E2CCC
              499C4B9C18C2480C3D9C9A0AAC5E058C4205E0758BDA72583CE78568DC8CEDAB
              0EAFB7B9CC6E877EA7930E4E7A3C40AA7F7461A11DD55A9447399191E5747095
              28730F8BADD0B098E70BE0F75FC39D3795DA6CF0617E5E1C0A409DC0F38F4EB4
              A15A8762A003AB4470FC105BFFC09695C57EC2EB38272242EC090DB9ACDEA5A5
              414FF9B9E0C9D970647BF6417CFB98565425815E44068301626363A1B7B717A6
              56283026420D9E03D5EA811B677F895DFF45A7D389AD45789E87BCBC3CF0E207
              8F3AD0D2D2027ABD7ECE68347AE98C0DA0B1B131CC6AB52A83F9C4FB3F1C0B52
              B95122DA5BBC8A378B6D0736DD81D07F83918A8A0A7A420866B3D92736A1B2B2
              32E8F45AFAB1F082EF256C7A0482F7407E7EBE505F5F2FEDC77E054C261363B1
              58B8AD5B034B596F3DFC9DF7A56CD7C0A647606BA580F66C20168B85FB0D72CD
              3E07E24AEC5C0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              00097048597300000EC300000EC301C76FA86400000458494441545847ED985F
              4C5B551CC77FF7DEDED2960A8C3F031E8048B23132973469C107DD144BC97459
              42C2DB7CF1CD7F314117754B0C3ED168A23E986942986FFA6062FC836CD3C8DC
              F4AD854792C1286E0B50A00418301885B5F5FB3B3DBDB40ADDDAD982CB3EC98F
              73CF9F5B3E39F777CE3DAD72A7A9A98B888A29163B7D6E7A9ACE4C4DA1BA3307
              2D16BA74E81055EA7A1BAAA7715FC7AB376EDCF9667E3E3EE03F464594288AF2
              36CA4FDEACAEA68F6A6A44C77624C97950FD015186D0B98F297CA6C308D3FE3A
              D9FA7028CB2E17A98AF219243B63B1D8A7B168F4DD73B3B3747672520E899324
              D78AEA4F886B088F7D707091FB738556A4AAD46CB7FF8A6B3193314C445361E1
              6F4F280A5D5E59118358EE22E4AAB6E446106DD7EFDE5DEC09857848CED02E2F
              2F53B1A611A458729FAAAAFCB86DA80FD863319ABC774FCC1CE4DC6867B9EB08
              0FE4168E8F8CD06A348A6AEED0F8CFC096E42FA896B2241EB7D565B30DBC5251
              41FB74FD05B4F72184DC28E45E1C1DA510E4738D106458B20492CD71C9323193
              8A62316B5A0C659F423486769EB97921B7B929EECB358620939849485E42B59C
              2521760AD79C73AD2C773C8F720C6F33297C353747EBC83DF03D2282D933A1BC
              8298FF6E6121AF724CCA0C8AADA4A1814A4CA6E750FD19C139D78FD5FD064AFD
              5851D1EF4B91080DAEAEA29A1F0CC1845CA5D9CC72FD58247F21785BF9165129
              377393A7B8F84A3E2585E001295765361F4395E56E462291D6A9B5B5B91F9796
              C861B35D447B95DC273596BC9D2749CD90D3F5A3C8B70B78B7DE84A07B3A1C9E
              3B1908D0D7C83BB170EC7696AC9633A942F26A3E6652F31D3ECC337714FFF802
              EAB7B0EDB6CE40EE04E4021B1B6290B14FDAED3C8625DF41A9B0A43718146332
              C55CEFA0C8E28CACED8C8A997B165B493FAE6FF1CCCD6E6C84845C381C1F2179
              7F6282BE9C99E1E5FD3AC6F542924F411F8ACE2CB03EC5D9747F543CD6B32827
              7841CC6E6E865EC23E37B6BE1EEFFD072CF9455CF2358C3F8FF22DD19105D1F0
              03A6C68ACB655F713AF7078E1CA1030505B2353D1FE3488673A472BBF9E92765
              53E6A8293BDCCE9C2A2DA597CBCAA8FE01E51274D437D2492FBF15F72805079B
              A9E67CA05A567306D6070E766D6DD4D9D949C3C3C3E4F3F968015BCB7D315B69
              ADE179F27DFE9E6CC88C96961679958AAEEBE47038C8E974D204725E08B6B7B7
              537777F799B1B1B10F7A7B7BF3FBB2DD06B7DB6DC2A4FD3934347422F9B08093
              9566C1F681C3F4EE063CAC282D2C6508A2415EED0D123EFF3A6EED351E0B3E2C
              FF6FC1F2F2726A6C6C34C26AB5E215BC45725F6D6D2D4E6A5B647AEF4EA415AC
              C0574E7C8092089BCD267BE224F7D5D5A5FED42105B3BA37993DFF888D3789D7
              EBED1A1F1FEFEAE9E9111DBB89C7E3E1F8C3EFF7BB1FAD55CCC98C9C3142E6BB
              801741721FE7A0EC126C77AFFCD8B46424C8C98C0F37925B360B781124F7B160
              32E9EE4DC7E345920D8FEE22D90D521E713018ECEAEBE3DF29771797CBC5211E
              718A202EB3FE229E03AEFAFD7EF7DFF2A0E60338368DDC0000000049454E44AE
              426082}
          end>
      end
      item
        Name = 'debug-start-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000019C49444154388D95D0CB4A42
              411CC7F1DF6852D90D513BAE1ADBB439D10D2CE2A0A451549C5A1A2EAC4D2F10
              D1AAA07C01A1563D41940FD0A6B4631454ABA288A2C20B29490686D70A9A165E
              BA68A9FFE5C0F7F39F19F9E8EC12CCB605D4D637227C7F01C63E50CDC8CDB605
              F08228F18218E81FB3FBDF5E5308DF550EC940080821A6683CB5A7D251C9BAB8
              31B4BC758DC1A939C8E48A0A80DC38774EB07D7C658CC6536E958EEE17A0C9FF
              2179DF880DAD6D1D2B7B173E3CC69238BE0BE13991A65C4BE3AC46AD31F38218
              348CD9FDEF7F3CED0BB8F4150EB3D003A28934D5B534CCA8D55A4BA720060C25
              FEA824F0130A219A48532E0B9979410C18C6EDFEF74C16AA29F7498C0167FE08
              CE0311F450CE64E1DBDD1A8E1E4C2F6E387A2D5649560EF88D010C00080388A2
              4E59FE06044037E560E9D443DBA43C04E04827621EAF6B1D5ED7DADF40A93093
              0B25D71A32C91700280608802ECA61F8FBC664CCE3DDCE6ECC87F9A9290A793D
              B4CDCAA3DC46B7E42A1D1601F3130355855F0063608C1D699B95ABD58405E029
              748BE0F5A9F1E674179E4D67C5617E3E01E986E2A2E66C8D660000000049454E
              44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000022049444154388DA5D2C16B92
              711CC7F1CFCFE7593DE633F63CD694F090CFE33C39671D02A159117689A05AB5
              4330061D82B9CABF6097CD75EB507FC0EA12AD3CED560441AD4B87116AD666EA
              26B4951A8FDA908D60FB76F079166A9AAE0F3CD717EFDF9787F92FDE84EA3D85
              9FDA77BC893EC4A696C7FF8C3B7DED364E5E18BFA30E0D97864726354BDF61AC
              A763F8B555DD17680200065C610CC99E83C2E333A361F7D4F30C2EDFBD8F5EAB
              7D3F200318C3A7AF3FB8B562650CD0E1EBE181A968F730E70D5C82C37D7C3C96
              2B38A3EF3F63AD58315945B34FB6082113C7BB9C1EFFC7C0C8A42676788A3D30
              5B283BB385324AD56D2CAD7EC36AB162B2F69A7DD22121C4F1BCEB98C79FECE4
              C64DA0B152751B4BD90DE48A15932C9A7D924598E0787EC0E9F127036DE09660
              6D0C9A5EAC9F62481685098ED38BAF36C3FF00FFACEE144631C7BB9C83B51B1B
              30DFF6C27F59265F42265F82CB2E7341AF32A6F4F7DDE839203C393B1A9E75B8
              7DE9AEC1BA51EDA3DAFFCC00745FA8DA249CF72A506CD20E119E12300B209558
              5CC08BB9E9CE41D52621E855A0F64B3B04CC1321C280545C8736D2317454B807
              D9A45DBD286214BD9C9BC6BA0E196B093640F3449801432AF1B6BEA8714DA06A
              93101CD421FD690056128BEDA126F0A82CE2D6B91350ED7A111061C04ABCC5D3
              5A82040211C1E338B24BC03322CC1845DD407585045A0058881196E3EF3A7B5A
              4B309F5BC6970FAF1F6C6D96F1EAD1BDAE8B1AF71BB7E82664061D363B000000
              0049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000326494441544889AD925F4C5B
              6514C07FE7B2CDDABB509C5054ACB90D4B46C285EC45DACE041023C640F4D5C7
              6936D98B92F8BC3D9098B8071D4CC5F926912DF1C91817E71898986526689871
              5088868284B66A7ADBA5632C30EA383EDCB68C85F24F4EF225E7E67E39BF737E
              E793FA173A397CB419805B3F7ECDDCE4287B1965C75E3BC18B6FBC77D4B223B9
              50C7F125CB8E90F96B966C2AB127000340E175605AC4E8A96B6A7FA2FBC275BA
              3EFC0EAB3EBC170041DCDCA7CA191166417AEA42AF1CEAFEFC06A7CE7DFFBF40
              0600E2223EBEFA8B8ECFA7CA55390D4C033D479E7FF9D0BB17AE73EAA32B04ED
              C88E0165754DED5876B81568F966EC0F9D984F319570C4F41CF0F87D66B3C0DB
              22E2ADACA9FD2DD4F1E6B2551F26939C25EB6C6F4765754DED041B8EB5022D3F
              4CCE8988C8DDE5158DC61D89C61D4CCF7E8FDF67B61AD025228F573E7B783CDC
              F9D652B021423A39B3E563C803222E203A57FC212272EF7E8EF1F9944E251DBC
              8F1DF054959B2D227401DECA9ADAF170C7F1A5E016AF6E9DA291893F75A34B8B
              CB2B44E75344138EE4276A167722EF9335B5B7429DA5D56DA8A830C1C33922B2
              F888BAAA72B3D910BA40BC45758DEBD56DAAA8547EEF7E8E89B8C364C2C1EBD9
              EFF1BBEA4E22782B9F5953978A4FB30F40D535A385649BF9DFD9452EDD88F274
              C541DA6CABC20EF8CF08BC23627C72A4A9BDEFDFDC4A765FBE2B54B5D8A9AAEA
              56F9C3DF0AA0F901411404F7B813EC369EAA38286DF5160D01BF184256E15385
              F3A0192711E3E72B03BB03F8CB4D5EB22D1A027E0C61215FB80FC8A493335C1B
              789F9BC35FB1FA20B7B31D143B7ECE8FC0026EE15E546FA71331AE0D7EC0CDA1
              4BACAE3E2836B3AD1D140ADB812A0C43EEA0F4037DA8AB62E4E259C6AE5E5C57
              781DA0B40A2F6DB6258D816A29A840E945F22ABE5853512A365454ED33A5CD0E
              D2B8A6A25FE11CAAB79D448CE10D546C0A2828AAF69945C786C802E45540DA49
              4C3332585AC5A6804274BF1A923257453FD00BA433C9198606B656B10940DD05
              C35D438A2A324E32C6F097DB575112A080B27A5995CF44C449C7630C0F9E656C
              68672A4A02FE999B22FAD3B7BF02FC3E3AC4E8E5815DA92815FF01CF2FBACC02
              AD8AC60000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000003D7494441545885EDD45B6C53
              7500C7F15FD7D1F57E59DB53286D6DD96827EB122D11A1931856302131633121
              E28B31918E2C4B7CC2F86262B2E025300151668C66531F8C893C10A3C050D039
              1D2AD2B59D6BD7D1B2F5E2767A5BD7AEEDBA02FDFB30AB1B11DC588F2FFA7BFB
              27E79CEFE79C8703FCD7C75E7A68D8F604365B7621978EA390CBFCFB9A836F9C
              C589C15B2F747F93E7ED3FDC03994AC77873D917B0EC390095CE748555C57E5E
              DBB0B5F4D8531D6E09A5B9391D18412197660450B5FCC802009CF9C9AB0A2533
              DDEC6A4EC0DA6A3FFCF2A73EC1FE177BA0D0D4551C70C717780694CEF8CAD9AB
              3EFC301E41309116C885BC3D3221DFAE355AD8CD6DED2EA5CE588C4E7A914B27
              99030C8E8550BC751B33D9027EB9318DC9789A2F13F26CB52261BBBAAEA9BAB9
              ED908BD2198BF4A417B97482394079A95C01D726684CC466F93201AFA556C43F
              A4AE6BE22C424C0BD135405604580A714CD00844533CA980BB4BBE08E15BDBDA
              DDEAFAA6F978C48FB924CD1CA0BCD9FC02862769F8E91457CCAB795C211674A8
              F40F0AADAD7697C6F8F07C3C3C8ECC0A21F705282F9D5F803318856F2A5923E6
              71772AC5820E4A6792ECD867776B4C967C3C741D99E4347380F232F345B88251
              8CFD96E488789CE63F2032EBBE832EAD696B2E161EBF2BA42280F2E60A45B842
              317822098E90CBB152624127A533CAADAD76B7D664C9C6237E641253CC01CACB
              168A708762188DC4D7096B38DB2989A093D21995DB9F7C6E64D3433BE7527410
              293AC41CE02FC84D8C8463F8351CAFE6D7AC7B542515752AD406D5237B9F75CF
              6767E7829E9FEFFC1533334208082100010B60B1592C561587CB070054331956
              49046869D4A349472DB0403E20841CA527BD918B1FBD0AE7A5CF9803AC970A60
              6B34C0AC551600F23E21B78F4E07BD535F7DF81A9C97CF8090D29FD75614B041
              2A84CDAC47A346390FE03D00C7C23E07FDF5C7AF6364F0F365E18A0236CA4468
              31EBB165A3220FE05D00DD61DFB5587F6F174687BEBCE7BD6B02686A45B0990D
              6850CBB38B61F26678CC11BFD0D705CF3F84D704D0CAC5B099F5306D90CF01E4
              3421A5E3619F23D9DFD705CFD0B9553D6B5580071462D8CC066C5E5F9B01F036
              21A593A1B1AB3317FB8EC073E5FCAAC2AB02E89512D8CC06D4AB646916708A00
              27FDC303B3177ABB10700EDC577845000325C56EB3019B28690AC05B0039757D
              7820DDDFDB05BFF3BB3585EF09A853C9B0AD5E0D83523A03E00421E49D807320
              B3F8C69509DF054000004FEFD89200291D27A474DA3FFC6DB6BFEF48C5C37F0F
              2000087909203D9E1FCFE72E7D720C379CDF33122E8FB5F460D97D004AAD11A3
              435F20E273301AFE7FE5FD0E65E9CED9ED4EA24B0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000004DD494441545885EDD6DF4F53
              671CC7F1CF732852FA1B5A686D819E0A65682B744B8C06588CE08D3713B9F052
              37B320B265FE07DBD2EA1697EC6A37CBB24836CD96658B89BA2D80E864320589
              523210A5AD58E9E14729B4A595CE259CEF2EB0CC886EA0FD916CBE6F4ECE73CE
              79CE2B4F724E1EE065FFF1729E1C38F0E169D437B7C154510DC13B843F130FB2
              E15A6915F08DB64F60B23ABEE06DDB87EB9BDB2272B52EABD055C09DFB8F42A6
              2C380BA02D47926BE16D3B46EA9BDF09CBD5DAAC40B92707D8A3E3A810924417
              1FBE0986D1DC3C69FBCEFD47ADEF7FEFC3BEF73E85B2509F31E05357305FA9F9
              E0F22D3FCE0CDCC642E22167D0286BF273254772722456B36DFBADBAE6D639ED
              461ED3E32348C422D901DE0ACC4208C721CCC7D037164024F190336814D5B2BC
              DC23399CA4CA54E918AD6B6A0D69375A30353E82442C9C79E054240E00200093
              E118AE7904841FFCC10C1AA55D2ECD6BE5386E8BA9D271BBBEE9C86CA1313DD0
              3501932D43E3E8F308988F27985EA3B0C9F336B4328EB397586BEED435B5CE68
              8D9B303D3E82C51441D7057C1C3A1589E39A2780B95882E9D5F22D7269DE61C6
              713526AB63AC7E5FEBB4AEA41CA1800FF17030F3C0C79B8EC4D1E715105A5864
              7AB5BC4A21DD7098316E9BB1DCEEABDDDB22182BB622E81F432C3C931DE00A34
              FA007D1E0133D1388A5472AB4A267D9B31B6436FDE7CB7B6A92560ACA846F0FE
              1DC4E6D7074D193059706111FD5E015391188A54F20A954C7A0860757A73D578
              EDDEC313266B0D66FC6B87A61C986C766111FDDE4908E118742AD926B54C7A08
              40BDDE5CE5AF6B6AF19BAC8E3541D3064C168A2DE2BA6F1281F905E894328B5A
              267D0BC04EBDB9EA7E5D53CB3D93D581E03F40D30EFC1B9AC0806F1213735168
              95325E23971E04D06030574DD4EE6D192FA974201A9A4478E67E7680C9E6E209
              0CDC9D823F1445A122BF4C23CF3F00607751A955D8B6E7A0AFDCF13AE6A7FC2B
              D0559B854CE5999EC7E7DD37F1E5A541BA371BADE5384907C7E55CB5BED6B0E7
              DDCF2EA2C050965D603291082251F254C21893B0C7AE4BB260024028D717A2D1
              CE6353710123A27E22D145849F43821717BEFE18D159213B40ABA1008D760BF8
              220D2312AF89B4E404A133145886DDB8F00DC4A5A595FB3306ACDC588846BB05
              669D9A11512F91E82451EC0E06C6D07DEA046E767D0B515C5AF55CDA81AF18B5
              D86DE751AA553322EA212217804B8267085D5F1DC3F095F34F85A51DB8D9A443
              A38D478956C588E817227282D02378DDE838E9C470EFB935CD9372E096121D1A
              6D16980A9520A28B44E404A857F00CAD0B9672A0BDB4080D361EC6022588A84B
              24D1C508579757CCB56E584A800C80BDB4188D761E068D0222891DA2B8E404D0
              2F78DCE86C7761B8F7FC8BBCE2F9800C407559311AEC3CF46A0588E827227281
              686062EC06BADA8F63E4B71F5F08F65C40C6809A323D1A6C3C8AD57280E8DCA3
              AFF2A677B0071D279DF0B97B52025B17906380C36CC02E1B8F22958C88E8EC32
              8CDCDEC11E749E74C2EBFE35A5B0350139C6F02AAFC72E1B0F9D4246043A4324
              BA08F8DDB7B262E981FD2BB0CAA44383DD02AD3C5F24883F10C4634418F10D5E
              4647BB2BEDB0670293FB8AAD254522117D27D2D27100A3DEC1CBE86C77C1E7BE
              9211D83381000144A708F88840776E5FEF44F7E913B8EBEECD282CD92A602428
              0084838277F9CF3FE91DCA86EB65FF9FFE0233327E3237657698000000004945
              4E44AE426082}
          end>
      end
      item
        Name = 'debug-break-all-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000008049444154388D6360A01030
              C2186E09D50C729A6628926F9EDC61F8F6F91D56F10D938B1918181818586082
              729A660C3AD63EFF90153EBE7196E9E3DBE758C5616C26063450B17CDFFF8AE5
              FBFE132B8E6100A960D480510318189092320C74443A316253884B1C6EC09B27
              7750D238030303C3CB87D719BE7C7883559C6A00003CF33564079C1BA9000000
              0049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000011549444154388DED94314BC3
              4018405F45C4A307F540ADA0601371535404D74AE8E42A08BAC4C94DFC010EE2
              E60F7072D1FC094110694705E922B8884DA636CB05355A9D74B0414C4F28D431
              6F3AEEDDBD9BEE837F26972C86449EC5D575E3A1671DF2F11633363563F4F7D7
              17BCE81080C164531646D9DA3F9D06B653E7FD877AD5D3CD809535D7054A297F
              76BCEB045DC10E25E0E0318C3E01547E1825450DF03ADE8DE276397A7D07C02E
              AA1C50058224900E0270725507A032675199B77EB9DB468BCBBB0600479B4ED7
              DD0153B01FB26016CC82BD60FCCB3BCE12F03D1CD22C5B13D8E3233D077DE0D0
              2EAAF45E82A7A4A82929FEF23F0356C802E58D3DE3ABBAE9D38E9F989C5D30FA
              9B730FDD0A8CAE6FBE0050FE465723616F550000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000139494441544889ED953D4EC3
              401085DF03930E2121451434716AFB0E488EBB48B49C809E8A2E28456EC135A8
              70CD05020D124442A2224D240A2B213B14F69AF57A17374129C854B33BDFFCBD
              B564E08F8DE661FFA08320E87861B5FE8212D5CAAC9679750ECCE0F07282B38B
              2B6FF2D3C31D3EDE9E5B99DBEBF3EABC6703246F00AC492A92CAF0472663C5B4
              3FB2EB05F685B6FBE94C0A4F90C67D27933DCE00500820897A4EA6D140A4A89B
              4D5F45DF0DA2D0C548560E411249D4A3CEFDB50149880848B22C26DAAF31C507
              528B591800C71B6CDABC1289B1AF58BB6B8974AC9C7C275183F92712EDDE60FB
              6FB03D89D2B85FEEFB2397C900601A87307F8A2E89BCFF83411C36A6B54C9228
              AC8ABAA6773610913180B19960276BC68EB99AD4763AEA9EE2F0F8C4390900E4
              9F0BAC96792B337F7FF1C6376EDF28BEE958E39A4EAA0000000049454E44AE42
              6082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000008A4944415458856360180503
              0C18B109BA255433C8699AE1D4F4E6C91D860D938B29D6C3C0C0C0C0824DB19C
              A619838EB5CF3F5C863DBE7196891A7A703A00062A96EFFB8F2ED611E98435D4
              C8D583D555F404A30E1875C0A803461D30EA8051078C3A60D401A30E187500DE
              5631A1163035F46075C09B277770B6E3191818185E3EBC4E153DA360500000C1
              BD2FA209C5F9A00000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C0864880000011249444154588563601805A3
              606001232E097D876006FFEC2EA20C59581FC5F0F0DA4914317E516986FC6987
              88D27F70D5448683AB27619563C1A5899D939B414852A19E8181A19E80F98EAC
              EC1C07D10599995918842415EC191818F613D0DFC8C92BD0884B12A70361E0D9
              FBCFFFB79CBB8D552ECDD908670C2083597BCFFDC726EE63A4CA2025C88B572F
              4107FEF8F587E1DEAB0FC4B80327C0A5FFC7AF3F04F5325164331DC0A8032905
              A30EA4148C3A905230EA404AC1A8032905A30EA4148C3A905230EA404AC1A803
              2905A30EA4148C3A905230EA404A01C191050E3616062531018A2CC1A59F838D
              A0F5841D2825C8CB98E66C44BAAB9000B16338D8002107364231B9E020C31048
              46A36014E00300651D2BF54B5EA53F0000000049454E44AE426082}
          end>
      end
      item
        Name = 'install'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000019549444154388DDD923F485B
              5114C67FC73C63CC13A3C1685034E82448E95029B814D4C145ABBC3A1784D2C5
              2242E0B9752E298243DF5451A1207448827F26C1C5D15270280F2934ED622868
              31469F29A8EF3A1893488C48BBF94DF79EF37DBF7BE01EA8A0D0D447220B2937
              FC76A5920580AA3BBBF7D0030068C5939706238AD4E86437E66FBE1208513FF2
              06711599E42C2A775C0ED0FB46A91F7AF51CE8AA7D3C30A79CC3ABB01EA0D95C
              A63ADC352D90BA383E58CDAE7D2807A8BF0EC057E0BDD6D8E2A1B16516406B6A
              078802AF8181BCAF08B02CAB70398F9CEC25F7FDFDA9D3AA4DC0932F9BC044AB
              CFED1F0F9FA5FDC653D4586F2123B66DBB85299472767EA6C7DEFDF0D94775AD
              9B22D2AD94DAD5B3E9C1A9B6A39E673D9D4911D14B2710DBB65DD3345549CD39
              F307877FF5BEFC9E0B462C5F666FB2E3CBD2139F73F019A82D0DC76231D12897
              AE397FD63AB6178733E1472F02BFBF8DD7E40E3F21E205300C4300E2F1B80BDC
              0A4044EABCB9CC7A28B565894854A4E84B24122AEF1128DD835B20223253A97F
              ADFFDEC41BBFF02FBA043D947F1A542F8A580000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000022C49444154388DED934D4894
              5114869FE38C3A394EA339639F63218E988B68917ED14252A310FA116AEDAE4D
              8B5A4492D132425A04AD822221A33FA2C016FE502E74E12E86C8A0AF881A9130
              B53167469A69FAFC392D6A6CA62925A45DEFEADE735E9EFB9ECBBDC22A729455
              B2E5D268333002E44D1C0BAE6607206F4DC75FEA3F70FD72FE5A28DCB61B5016
              26DFFC3E41C966F2CBABD0451B3BFC7C75A0F7E869361E3E5184AADB9E7A1789
              F69CCD361B41FC67EEE22CF157814CCCDD3C4762F441F68159E96A1B10B82022
              2305153586EFE43554150055A5BCF31E4EAFBF5190318136576D43EE04999BA5
              7804E03C10139161A7D75F9195D0EBDF23793208DC02ED5B8ACFE600A5ABABEB
              27DDE526B9E300431187672A2503809FEF89EFA8EA5EA00FB8515DB47C6A7F99
              8DE3593FBAB4980DB42C6B38B3A0AAD1173389E397C70B5333140F00F522E256
              D504AAD7ABF55347479DECAA29735F14919C57E2045A42A19046A3D195623C1E
              AF0D4CC65A6275470E7EF556F603CDA85E2D9E7ED9B9E9ED60E3ABB9E0C0B8CB
              E5C904959696629AA6380142A110E17038B3BF5D6168EBFCEDD6F70DED87521E
              A3DDF3F17577E5D8C37DF697D8A3D1D9493780CFE70320994C621806A669E6BE
              C395BB809D1BE63F3CA97ADAD39A2C36BADD73E1B68285E47D1171A53D4D4D4D
              02605996DAB6BD32F21F2522F58589D9C7059F23DD22724544F233FBBDBDBD9A
              5E0783C1B5813FA0A688986BF9D2FA377F391008AC1B9466886559CBEBA665E8
              1BCE09B969546E20410000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000002BF494441544889ED545D4853
              61187EDEB373B6A99B3ACD8EA6B8DC0CA684A151048114D8A0D28B2ECA94FE90
              8288A20407118150F78A0405411A05DD08769504BB96B22EDCCD59514BC432AC
              2986A3E26C3B7BBBD8CEF46CFE4CD965CFCD77F8BEF77DDEF779DEF37D400E70
              9CB903E7F0747FED932F9A73781A66E7DE5CD2000042CE91DB44CE0598D9B0E6
              BD001119D6BC17D82EFE5BB429C4CC0DDB912E14B7DF000044BF7FC6E2C36B00
              B22D92DCCD28BF3208413483A37F111EBC8478F86B56018302C15E06C7B97B10
              1DF2319343BE5AD0781815B746400545068BCCF5FBB1B37704D28E1A5974C803
              92BCDB5A76FEFEE60A4830E9441708E8029164A96F796071371B1438CEDE0504
              4126C00FC009E0194C5260D30209F50FB4E54508F6F21E22B282304820818121
              6323422592E4B5003A9839100FCFAEADC0E7F31936249EC252E5D1D8DB2553F7
              62949E031820A2B4950CEC4A755E2D104EB80A131307ED2A1255117006170050
              3018ECD7A5EB3E6BCCF3534B78FC684614E754F129883AC11C02B007C02700B2
              C0898E43C5EAC4E53AF654150A9DC0CA2FAC7331334400FD2BCD81004000B826
              FAB3A97146B9BE606BB9A8DA6526A26E4E0E4286163FB9633EF0A67139DC5CE1
              691D2722397546195C4C8AA2687EBF1F7EBF3FEB8A4A66F3D0AF1267DFB77DA7
              05D52E0F13513BB4D871C7ECFB77951FC60F486AE4153397ADE5BDD7EBA5B6B6
              36889404A724524A221311C563B19B45E150A23A30EA9B6B3AD513B396D695CE
              4D85E48FAF5B4535F29281123DA7A1A1214D1E0C06D3CD665DB4CC1901DC6B5B
              0869CEC991DB31D11AB2447E78A5446C94886CBCEA61F2783C2022626663014E
              421F4EFA800DAF1AF7597F87D9C23C49442F009899C1ABE3C6C6C674EF0DD8D0
              A28C6F1F800411995637B15E8E1EB395C78EB6189F54908B45EB5BB7F11990B2
              C8ED76A7E5E50B2E970B00408AA268F924CEC43F3C674ADE073B7C8500000000
              49454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000002B3494441545885ED965D4853
              6118C7FFCFBB8FE3D6F25B8B99D52E148CD6B2A8BCB042D331CC29CE1875538C
              088CA40B03EFBAF326C1BB28314243340BB6B422BAE9A290CC4CF24C8E51A6AB
              91564E0CA2DA97DBE922153536DD4EE4CDFE77EF799FE7F9FFDE17DEE73C8438
              9562AE875267C0FCCC477CEB698AB70CE4F1262A7506A8F71EF3F93F8C26C5ED
              0E80494906A094982F1940B212000980044002202A40DAE926E4B43C5767D6B7
              02F275363D6248B75D414ECBC09F3C29006A43A94A96BA855717969766377480
              9234D1ABC995C83C7F159AE213A5B2D46C5E5D58AE9204E07739BD20D840E8E2
              F20F94655DBA05526D8E689E71E13A5485E56520748160F3BB78AF2480D9D68B
              F08E3EEB87885A22D6C9E90CC6EC868EBF21E44A64D5B742BDE7A89118EB8488
              5AFF24DF3FD372662D7F505D5D1DF47A7DE40005078FAE18DD53F2226F88EE01
              B0F95DFC63CC0790947F301C987AC7E6E73E43B5FB880940BB82C41A6B4EE8C5
              F6E99708FFFA1ED5DCED7643AED7EB515252128E161808F81E78DE08D6874A43
              7558A1EAE37486B3001E0180429B078536AF02C04D0A05AB0FFF7CCD5BF715F4
              71B9FBCD6B9D5E1004B634903436368A51622B89E88E366DC83A5D64AB0A739A
              FB00CE2DEE0168A3A0AF6AEBAB2E7EF6CBE8DDCB4FC44A00D1EAA1B9B9998018
              FA80288AE614CF5BFBB6C17627F3FFA804D0B6B0D546419F593BDCED4C9F1AB1
              03E29A275FAE984632994C5691EC1977E40EB65B3E1DB21D0F719A1E16F49DD4
              0E778FA54E8F38988C9956E7582C165ABEB6DBED61225AFA16F34CC818999267
              C77B770CDCA8992930EDCA78FF5491FC75AC97888CB1D68A0B000088C8B869CE
              D5B7B3FFDA2922BA4D446591621D0E87B82A77C58DC43D152F984E125184CEB4
              3E49FA194935970CF02F9400D87080A557B0D81AFF3B80DBED8620081B721313
              13131B61BB52BF016889CCEB77059F240000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000003CD494441545885ED966D6C53
              5518C7FFCFB9B76F6B3B69E9CA4BEB88ED062921065788938C0C67262443C35E
              8226462218419D4636030B463F1058143E603042628C9489D9166421618C0D8A
              188824A6BC043E986B62D6F15664969775D3B1CDDE7BFC40DBF852E8DD7ACDF6
              A1BF4FF79E739EFFF9E5DCE43997A001CC62836DF596D4FB9D7D9BB5887D90AD
              450819F26029AB7BD25256D76329ABD32232852682096C009ED7300F80B682FF
              0B39C16CC909664B4E305B7282D99213CC96292F28665CC1044C5FB71386E245
              504687717B6F3DE2B7C2E3DEC850BC08F6753B414CC0F0F96E0C7CFB89AABA8C
              2728D866C2FCCC2A8BE8701FD7BBE62E9ED1D40AFD9C05E392332E588A828600
              74CE39EB45877B4FFE8A3754D7AAFDC47F807015C071C1EA78BAE0FDAFA12BF2
              AB932B590E47FD17607AD39B207C0E20A4DA4E8DA07C2782C1608003D800C241
              10F5B0BCFC52E7C67DD0679034FA97C3B1613748A7AF07A3CFC0F13A082DF7DA
              B76927080003EDDB317822C0C1F95B005A89A88799CC4B0A1A020F9534F9573C
              9013C57789D1A7E0780DC0817B6DDB3014DCAF5A50ACADAD45494989BAD5B306
              F9B101EB3BD7EF338540DD82D15CE56C08FC70F7C047E9E5047123801DE05833
              CFA2B43F67E887BCAC1858D6AC6ABBBEBE3E50737333AAABAB77015898A98073
              7E4DBA157B7B47D838DCCF1EDB0DC25A70ACE472FC0C096239117DCF39675C8E
              8304B111A08F01FEAA97DF3DB8C5C73DAE69E63D446450771AB8140A851A936D
              66616F6F6F79389CB97D44A3D1C2D9376E57C5E6D7BD3732FD0919401709E20B
              0078720D09E22600DBC19557CCBFFE74C8FECB11EF8568D169292F6F961A338F
              C703AFD70BE06F7D301C0E23180CAAA92F9765B9D335F0E58B374BD736DE2F28
              9201EA04B02B31DF04602BE7FC656BE4D2E199E7BE291AE17F7E77F66CBF2A39
              00A8ACAC4C094EE8261104E159D3D8D05177A8C56C8AF66EE29CEF25A20F13D3
              5BB9A2BC648D5C3EECBAD03AD7A88C9D2222F744F601D4DC240F8131566E1889
              753D1E6A597963F19AA66167B10CA0019CAFCEBF79B9D375B16D9E4E1E3D498C
              CD4E575F535343FF1EEBE8E85088E81FE359DDC58CB1A586D15857E1B9FD564B
              BFF401387F6ADAF5F39DEE8B6D3E5D7CE41411A5951B0F133EC1244454A61B1D
              3A56F8E357556366C7CF86DF7F9BCF94F849229AF1A83A4992F8A3E635134C48
              2E11E4B16E632CB299880E119133538D2449E972FEF3D935114C849712D119AD
              F2924CF9FFC19C60B6E404B365CA0BA6DA8CDFEF87C7E3994C9714369B2DF59C
              146CB1DBEDA7ED76FBE418A5E70A00504545057C3EDF24BBA42712894CB64266
              FE02FFD521FCAB87425A0000000049454E44AE426082}
          end>
      end
      item
        Name = 'uninstall'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000001A549444154388DA59231481B
              5118C77FEFDD817A4825CB45093982E22295B81CCE216B41384A87CEA163A183
              E952AAA0A4256321E0A2E2D8A1E9503A083DEA2E64BCC192A13A340A3D318DC3
              C5DCBD0E5A392F6721F89FDEFB7FEFFDDF8FEF7B90A297D92C3DDB2EF46CFB7B
              CFB60BB57C3EED18005AD258CD66D9B0AC59C015422C012BCB93935F32529EBB
              DD2E2A715EC6379BB91C6F72B939C0056A37760D705F98E6DC56A170F7429CC0
              C964786F59F352CA6FC006B02D84580356803F52CA9DC786F1F56230F00F2F2F
              6F03F47F8B294D438005ACFDEAF7F77E87218B86C1CF20A0AFD4CEFCF87828C0
              7AA4EB3FE204B7019F7C9F858909774C4A3E743ABCB32C160D83F330E4E9D111
              AF6766F604B07B76466A40378A583D3949F61480CED515AF8E8F536BC99E8C2C
              BDD168DC319452B4DB6DA2FD7D00069A46A552A1582C22E5F07B7AA9548A9266
              ABD55A3FC8E779E2FBEAF3F4F4DB20082897CBEB6904C2F3BCA85AAD26FF0700
              1F4F4F79669A4A0821D2EAF57A5D8CDC03C771701C07A59482D814D2F4DC3487
              C89ACDE635FA0DD57F03C2FBD8637AF01885E779435318457F0176F080143E13
              D5DD0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000020A49444154388DAD94CF6B13
              4118869F6FC9CF363D04C31A534821311B9B8B252E6215E241B4471B027AF00F
              5010AF1EF514D08367FF801C3C08B120D85A4D05C12A21E7ADE4B0BD7929B86A
              DA4042DDF16022B1D92445F39E8679BFF79961BE99114648F7FBD9C86631C2E1
              73C023E0E6C756EBEB6AB3C981EB8E8AA18D82ADFF8699C0A6885C01DE2ECFCD
              9D786118CC6A9E316FE0E96090F78681110A9D073601BB67FD046ACB9148AC96
              C9A0FB7C9381996090F56C9644287441445E03CF81FB3DFB2AD001B6729188FE
              2A9BF584FE057C924C722A10B8A869DA06F04CC11DA07F60DF806B2272A0695A
              ED4C387CF2C1FCFC78A04F04E02C50514ADDFDD46AA9417FBBD5FA8E522BC017
              60A1573F1AB8E638A0D45394BAB7E638EAF6EE2ED20B8908C56693EDFDFD1F28
              B572E8BAF5978E33041C5E6240C940809DA5A5CBC03B409BADD7C7950FEF701A
              9A3AD0572E97479A6F2A15500A444029F2F93CA552693CB0582C6E1D9D745D97
              7ABDFE585F5CFCFC616F2F7EA9D351D59999854C3C4E2E975B350CE3BA787418
              402CCB721B8D86728E74ACD168E0380EB96E97878EC30D5D572222A9548A743A
              3D048A46A398A629BE7ED8B6EDA1222FD9B6FDA736168B01D06EB7553C1E17D3
              34F17E90C754A15000C0B22CBADD2EC064E08EDFAF6EE9BAA757AD56FB4349A5
              52C7032A113964C20B18D0F4EF21402291F86F509F2196658DFECFFF41BF00A9
              44A611921BD8140000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000002C3494441544889B5934F4893
              7118C73FCFEBFB8A381D746813661A6A3A6F4278300F56885456447F0884FE88
              048974E820D2C96B52C7E8DA2534CA4CB03CB88AA2439411046D2D4929E85433
              50E7D2ADEDE9B0BD633ADD66D8F7F4F03EDFDFF3FD3EDFDFFB13F2E0B2CBC5CD
              EA6A8023C025A0EB472CF6BB3318E4D3CA4ABEE318B99ABD2E1737AAAA008E02
              0F80E3C0239765953EF17AF19694FCBBC035B79BA1AA2A44E418701FB052AD83
              C0F84ED32C9DA8AF675F69E9D60404B8EEF130E0F180EA89D4F0F7C0AD14E522
              D02622132ED3748CEED9435B79796102020C5556D25751812172D2308C11E01D
              C9FC1752B461E002D06A18C644996595DDAFABA3ADAC2CBF80DBB2E875BB1191
              5386610C036F814E603193A7AAF7807322D26A883C761415950F783CF9053270
              10780D74FE515DFC18892022E92D3F442290BCF42EA016D8B5C99CB502E1789C
              EFD128C015543B638944B8676E8EE78B190B8870F5DB3746E7E751D587A83602
              816052340BE61A814482C39F3F73C0E94C00912FABABBC5A5A62AFC381AADAF1
              1053A5676E8EA70B0B5822E1982A23F3F3F90500BE46A3DC0985B2882282AAA6
              A38A037737199A899C0F6D3B50B04066445B81D9DFDF9F93303B3B0BD3D308A0
              24FFA2E2E262FAFBFA0A13E8EEEE1EB49DD9F9DA2E43A1D08BC9C9C997E19F3F
              F9158DEA8E4482806551D9D444434303CDCDCD83A66966DD8F5DAB2A263068A7
              903208A02222C160F0FCCCCCCC57713A79B3B4B4FB5024A2B79DCE67B1A222A6
              A6A6686969D92FF6D475E753B58ADFEF8FFB7C3E7C3E5FCE70CF2C2F733A1CE6
              ACDB5D50341D1D1DD2DEDE8E29492880ED465575A33AE56C435E6363637A8340
              2090369BF50E7222AD930DAFD79BCE3D1008A4BF9B9A04B623BBB1BE1E733818
              773836E58D8D8DA537CC44C1112580F8DAB8368D32D3C07F7FC90547B4519DAF
              07A9886A6B6BD7ACBE1DA8A9A90140FC7E7F7C3B07AFC75F6E43554735801807
              0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000029B494441545885ED95DD4B53
              7118C7BFCF39D38D31BC71EC8DE99CF972751022E84AC2D6CD240A84D0BA96F2
              26A31BC50B752429EC3FC84244F0720B2276214EEAA25BA18BC159DB40176C41
              E454ACF332CF7E5D4CC784DACE59E52AFCDE9D1FCFF37C3FCF739EF33B3C1AD4
              7D8703D31E0FBACD66BC3B3A6AB40CF84692269D4E847D3E5FAFC512B9D6D6F6
              D6C6F307F1C3C3F30198743AB1D8D9D905608B882E03B87DD5667B65E3F9FD46
              203823C18FCBE6DD00B600844F8EC300B6265D2EFF92D76BAC200C4C60C1E3C1
              B4DB7D89E3B838802500CB44340FE016806F005E5CB15A5F77B5B41462070760
              3AEB9AF4042D7ABD78E872F510D126802779555DF9A26910AC56EC2A0A54C69E
              F75A2C1ACFF3F17B0EC70D8EE3D20F767650D251BBEEC49E96CDFB88280E2094
              57D595A028E2A3A200000A9A86A0282225CB2B004244141FB3DBFB9EF9FDBA5E
              47CD9805AF178FDCEEFE93CE67F3AABA1A1445A44FCC4FF5A9582C4348D22A80
              5922DABC6BB7F72FFBFDBF0630D6DE6E011003309353D5B51F999F814826F141
              92D61863330062A3E5FCC60152B22C33C606F38AB23E5CC3BC1A623899444A92
              D6196383695996EB01D45CC23BA9147ACCE65CAE58C4E7E3E37AB52A10D74511
              BED6D6DCAEAAD68DAF09F0B554C27B49D2655CAD7D4DC3BECE3CA3F7C66FD705
              40D3014C1313131004417702630C994C06A58D0D00C031CF637C7C1C030303E0
              3863FD64B359980441C0D0D0909E6BBBA2EDEDEDD09B8E0EDCDCDBC34BB77B4E
              511404028190217700894482AB7C865353537A7F6000300F5339356D32CD51A1
              4006F3110E8709F81B76E04F1B8C8C8C50F5732412291151E5ECFF9F40341A3D
              B31BD5DD03FFFA04461D0E4600D58FFCB99A3E810B80A6035496F0F46A3C7780
              6C368B4422D1944964329966D89ED5777993F4DB3658C1F10000000049454E44
              AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000378494441545885ED98CB6F1B
              451CC73FBFB51D94D82EF6C6C445AA09593746CE2972A44A692B0525A4AD8BD4
              DAF1012EBD55202171810337CE5495FA0720040E45A807420E70C2A116072E56
              848A108DD7511C9A2A28AE5AA069831F9B6639F8A1B44D895FC291F0E7B6BB33
              DFF9EC3C34A311DA8005F8F0C811BC361B997C9E2B1B1BED8805C0DA6A800598
              F5FB89A8AA1B781BB8E4B5D9CC0F6EDF6E59AE9ADF0E3915F84E442E00BE630E
              C7B7CF5B2CE6C2E666E70477C9F503096048447A819781578E391CDFB4435269
              A6529F08714D23A2AA1E600130818B95CFD3C039E0D377BC5EE5B2CFD7D23035
              2C685714BED234CEB95C2F54E48C8AD41F95223F01AF01AF8BC8EC5B1E8FE593
              C1C1A6251B12B42B0AF3478F72D2E51A5014E57BA0089CCA19C69FBBCBE54AA5
              1B15C9D38AC5F279CCE3B1C487869A92AC5BD0AE28CC0F0F73FCD021AFA228D7
              45640B38A5E7F37FBDB9BC8C8800202284759D9C61FC0C4C89C89422F245B4BF
              DF1AD7B48625EB12B42B0AF38100E34EE76144AE03F74DD33CADE7F3F7C3E934
              39C378AC7CA650209C4E932B957E01A6107915912F23AA6A8DFBFD0D49EE2BF8
              9C08F38100C79DCE1745244979AE9DC9140A9BE1749A3BDBDB7BD6CB140A9CD1
              7572A5D2AFC014705244AE45DD6E5BDCEF6F9FA0D76663DCE1700249E08E699A
              613D9F7FF06F7255960B05C2BACE46A974D334CD49601C91D988DBDD3EC16DD3
              047880697E84699ECDE4F30FEB91AB521B6EC3485396FCACBE9A65F6DDEA7E37
              0CDEBF758BF32E57FCE1A347BCBBB656B75C95E56291B3E934977D3EDD0AFAC7
              F7EED55D571A6AE919BCD4D3C3D2E8E804E569A0D853A976C4024DEE24FF255D
              C156E90AB6CA8117B4C6623142A1504B2189AB571F7B0E8542C462B196320156
              5757B1864221A2D1E81560B4D1809D9D1D52A9D4A5816030FDE3DDBB874F148B
              7CDDD73738ECF532323212090402E7ABA79C26B8914AA5DEABEE24A32B2B2B13
              D96CB6E194C5C5C589BF0D8385DE5E4E148B5CB3DB57646B4B92C9246B6B6B4D
              99699A86BF72A0A86D75D96C964422D154E05E64B3599AF96180E9E9E99AE0C1
              5F249D6A786666E6A9C9393737B7234F4CDA6E0F3E8BA5A525B39E729D147CEA
              DD93C30BFFA721BED9D3C31B0303A6B4E9105CE5C0F76057B055BA82AD525BC5
              636363689AD649971AEE5D370F55C15955557F28DFE41E187E0390C9C94982C1
              60875DF6667D7DBDD30AFBF30FC7D220BD25A2B3F10000000049454E44AE4260
              82}
          end>
      end
      item
        Name = 'button-help'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000002E249444154388D6D93CD6F1B
              5514C57F6F9EBFD271634F9CAAB69AD050C791425AA591924514198458B0C802
              51B59125162CD865D315A8F017C0028976C7A2AD5824329B102A24242A844485
              5CA5A9C02D1228246D684B908BC7763C197BC69E99C72256D440CEF29E7BCFD5
              B91F821EF4FC259217AF48194F1440BC034C0B210CA5541D5807B514B8EDE2EE
              EA67BE75FB26A8000001905CF890FE37DF1B03518C57FF984C6DDD4137B790AE
              8D1FD5B15359CC6C9EBDC15C1954C12E7DBD615E7F1F5480D4F30B18173F1813
              4ADD19FAE5CB3399072BC4AC0A9ADF41009ADF216655483E5923E2ECA6ADF444
              213C3C7E4BF9BEE96EAC214E5DBD2F653C796FE8E7E2E4C07609A59432B3AF8A
              6AEE0DBA7D09A2568593BF7D4B62A70C406D649667538572D07567FEBEF2BA1F
              92F164215EDD9C34B64B00B4526744EDE5394E973E27663DC74E8DF074E65D42
              4E13BDF61863BB4463787A726F70B4706CEEC2924CBC75F9E3CCC3D5D1985501
              20D26E30F0E75DC24E138122D2AA83266919A739FEFCF77D5B9ECBEED0541FBE
              B71C02A675738B1721020F40082100E8C6FA95ECB60FF85EFEB48C1B680861C8
              8ECD11505E44574FA70AAA9939C7C0E39F0E08E9DA208481000DA51A7E443F4A
              8047738B689E4BEEFB4F083BCD83B81FD541A9068006ACDBA9ECFFDB2BA59CC4
              299179B0A2A4E71CE27AF9EB41DB4643B16C66F3A8FF080821C4B9AF2E2BD13B
              B60361C0CCE601969D8D7B6841A755DC1BCC95EB23B387049AE9091E5EB8269A
              E98943F1FAC82C7B83B9B20AFCA27D7715A9BA9DA0EF6CFE072BFD4A21EC348F
              C51ACFF657D575D0AB9BF4D59F20FD0EAA57FCD7F9852A42CC5BDFDDF8A7BDF6
              0DB2F3A84CE8C44B667878FC56337336DF3A319AD63C9748BB466C7707150A63
              9D1C67E7FC25AAD9D7CA0831DFFEF5C78DDA171F41E0F7FC098DFEF945FAE717
              A51689168023BE912515F845EBF64DBFB1F229789DFDD217FDC9810CFAECDB44
              C76690C78D1EAD085A16EEE67DECD22A5E65FBD04CFE059C363EDBB1259C4700
              00000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000003DA49444154388D75945D6C54
              451CC57FFFB9FB71F7DEB26D690BA5A54B119716084588A6F88988247E25A4D1
              68623436BE186342421B238F06DF4479822801236A8C12839A60687C81104C6C
              B5069136A6DA562AB40BDB6EB7BBEC767B77EFDDF1614BD3BAE53CCE9C73E63F
              673247B80365107EFA0DC2CFBD85F883AD22D209EC41880A6269AD93C000704E
              6B7DAA389B4EA4CE1C2673F12B164300246851B7FF04666B7B25C811445EB3A6
              AF49383680998EA15C07376093AB8E906ABC8FBCB5320B1C027D24FBCB0FC5C4
              896E287AF386CA6055F7E798AD3BD7013F9A9978B4E1F2692AA686590E5A8464
              A49D585B079ECF3C0BBC94FDF97B2771B21B0023FCEC9B543CFA622570B162F2
              AFE8FA4B4731B3932531A2E7C20D92B76B519E8BF2F208104ADDA072E20AB71B
              DA5A3CBF190D34B59E29C4C728DC1842D67E34880A989F98995B9D1BCE7F88E1
              3900DCAEDBC8F88E9729D82BA5E4AE098FFFAED75EFE1AA39003602EBC86E1DD
              DD1495BFD39D897F31F1F62318551D5D2DA2E478A4EF53B933593E54CDE8E35D
              545DEF675DEF49560F9E65457C8864733B99552D545FEF07C0E764D0A2C8D645
              7748D03A96FF77B0A810E90C4D8FA9C599057249D65F3A4AC31FDFE29F4BA38A
              1E766284C8AF9F91A9DF2C8E55B3C0AD1DB9882ABA11117932B4F96114B0271C
              1B280BDF4A8C965E6D118299D20D5C73C502CF28E4B0A74600F6F8EB37A0808D
              663A56662822656BD3CD0F226E5E9BE99B4BD683257D14C3C08788A55CA74C7C
              075A6B5DB06B98D8DA41BA7E0B8D974F63B8734B38CA7340B0017C689D740376
              EDDD0C3DBFC9F0AE03043271EEBDF001A1D478392760032401143090AB6EBAEB
              8473E1065C332CEB7F3AA6973303C855454073557B1E0A38976ADC8E5E263300
              7F2EC9AA3F7BB4CC7FADFFC3B16BC9554704E8298C0FA1B4D6A7F2D6CA6C32D2
              BEACA0A8FCD8937FE306572C7B627CD35368E8D55AF7E7AE5C401567D309E050
              ACAD03A7A2AE4C30D9B2977F1EDB2F99D59BCAF6661AB733D3F480077415E263
              38437D187A364D68DBEE5EADFCDB6F37B4B554C487F0399905912F9FC59E1AC6
              4A8CE22BCC2E31BB71FF2B6851EF68F437D3270EE0DEBA86911F1BC0577F8F0E
              AC6DF9CEF39BD16473FB162D8A506A1C557409E4928452E30B668E5D4B6CDBF3
              C4373FE3695107D1FA48BAE738D9F95E2CE5A20C6A5E7F1F6BE73E44E455E03D
              55749BECA91182E918CA73F0E6FB305715110DBD4097D6F4A67B3E2675E6F0C2
              E44B82B676EEA3EA85831855753E11D90B3C016C44C4A2D4D8572935F66F85F8
              18335FBECBDCE0A525B996BF9C2F4068EB2E829B1E22B0660318C63C4DA3F30E
              859BA3E4AE9CC719EA5B68E9C5F80FA83D99DB14F13D1A0000000049454E44AE
              426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000005124944415448897D945B6C1B
              7516C67F67663C7162277692C6696387A66D48B8048AA06CC5AA826EB9AF9415
              CB9617C445889B40082A7151E18127DE601F110B08A19596DD22760B28BB4294
              EB02ED92564D68B9D46D1242EF25B6E33871EDB13D338787C426D7FD9E8EF4FF
              FEDFF9CEF73F33C23C483044E48F4F12DE7A27625AF522F207A01FD824220905
              1B348B92043E577417CA116F3ACDD4AE1728ECFF0FA02CD0AC165647376D8FFD
              8540ACCB44E47EE079810ECBC96943661CBB3089F8153C3B8C1389538C768A6F
              981E3000FAB42A6385C1F7C9BCF92CB8E5850DAC8E6EDA9FFA1B66A4AD19780B
              B8399C1E2196DC43283DA2F83E8BE1069B24DBF55B523DDBF0AC601E7840D177
              9C439F927AF951F05C004C0986697FE62DACD67833F0A9E157B6C4BF79873587
              775357C8802A3207A0569B5E9986F4A8369F38204EF3057639D472BB206702AB
              D70D4B308CF3DD17B30DA277ECA4E1B2AD26F04FC3AF6C59FBBFD735726A58AB
              821806E7DB2E945CFC0ACEAFEA16B72E4CA03885E17BB3025E4922A70EE2443A
              A5D4D8762BC897F6FA8DC79DEFF7E265CF229DAF26112BF010F04A627817CDE3
              FB6AAF540EAD92139BEFA318ED5C108F55CE131F7A9BA633876A5C3F10646CEB
              93E234B68FABD2E724F739A997EEC68CDCB6A35E44FE154E8F34AE39BC9BAA73
              356DF9F1BA277083111243BB480CFD9DF6E4871A4E1D9562B4934CF7561A2792
              623BB9D9CC3C97E0CC39995ABBB905D194D59A183C3F38802122FD021DB1E49E
              5F570A10DFA5EDD8C7AC1D7C83E8A98398AE83E157249C1E65DDDE57B04A7952
              3D372C982C941EA52133A6200F6218527FC5360CA0DF72721A4A8F681500A8AF
              CD3F7DADA1F4D8920D325D8770EA18C5689C857754A3270F025C22B0C18EF760
              009B1A32E3E0FBCB6ECB723522E2D68531DDD292B3798636198D2D188824ECC2
              E41297552C700828C264D7359A8FF54AD399C34BF8F3B4120016608BEFD6C4E6
              0B2F6EE444E29CDEB85D0BADEB094F24B5EDE8470B68AAAA735A8260CF35D029
              CF0EADAA8D5F25CEAB014A8DED8C5EB703B3E2101FFA07CDC70741FD25772A76
              A8DA6F6AB6819274221D5B56CC680ED3ABFBF04D9BDE3D2F68C0C9CD8A2EC373
              221DD52C930006F079317A81B8C12659865F836F0610C07272FFD7C8CCEA4B01
              0AA81EF026CF62A8EADBBE617AD9AE6B163CE8E23A76E403EDDBFDB8A2BA22CF
              B3824C755E0D30A030533A3A8801FC000CA47AAEC7AD8FAEB89AA9DE9B64E292
              DF4BA1759D2C3EABD61317DD8C5B17F255F545BFE250FCF60B0C6F3A0DF0B467
              05F327AFBE07DFB0961D3DD57B233F5F740B859675CB47B3A64FD2DDBF13545F
              03860BFBDE459D3CA6973D47FD55B7644564AC1C6AB9DD8924A4E9DCF7C8DCFF
              BC0AC32B4B78E218A1F4285631A78BC54FFCE63ED4300F80DEE5E7B395F4CB8F
              A01507B3727A84407B178178EF0F829C2D8563B7CE745C6E0667CE895DCCD662
              68C81EA72133AE7669BA16911FA8979FFBFAE5ECE57F1235CC03A8F6AB6A2EF3
              FA0E2A278F006002140F7F465DE7C504DAD70F217CE5D685AF9D5ABBB9251FEB
              41CD80185E05C32B23805B179642EB7A321BAEE5F45577928FF5FAC0ABA8DEA5
              AAB9C9BF3E4761FFBF6BD3FDBA9AA64574FB4E1A6FB8174482020F23F22070F1
              B2A10B059401557D1118F6F35926DFDC49F1D0278B688B606FB892C8F6670876
              5F8918A628740B6C424880D8A86681A4C27ED0BC968BE4F7BEC7F4BB7FC63F3F
              B58C8F1560C5BA086EDC869DE8C1686C41E6A83AF7FDBA1327288D0DE37CF75F
              B4985F49865F00BFAD74368268F0F10000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000627494441545885CD976D4C5B
              E715C77FCFB5EFF5B5B1310E2F217678C91A1242925116365252AD5569A6AE5A
              AA054D9A26A5DB585569D5A4A99F224DCAA22AA934A993F661D21AAD7BE98688
              A64D51D32A54D5D60C352AA22D252584049692740163DE0218DB60FC76EFB30F
              268C1783ED7DDAFFDBF539CFF9FFCFF1F31C9D23C8114A6131FA8166B4AA4358
              4B2A10562B200089344D520FFC24C7EF101BEAC6084EE51A1691CD41AF7F8AC2
              A7DBB0D536F9846239093C011C4208AF00878428524E00B7806BC0E5F8485F20
              D2D541F4D377419AFF9B00ABB786E2EFBF8AADA6B101F83982137A685271CDFC
              0B3D14408D06518C04A64523E9F01073FB8894D51273EF32915C01CE27FCC3FD
              F3ED67487C71233F0105CDAD789E3FAF2B9AFE9A40BE54E4BFAE948C74610F05
              B2158C65B78FD99AA758A838624AC405691AA7172EBD168BFCFDF7B909707DE3
              053CDFFD991744A76D71A6BEA2AF034770342BF146443D55F81B4F1177960D20
              E5B7C2FFF8C3C4C2DF7EB1C9CFB2F6C3D1DC4AF1F3E7BC203E2C9C1EAADDD3FD
              3AB6E85C4602C362C3D4EC083325057253226A2C8467B4977891AF3CEE2C6BB5
              ED6DB864A61291C448DF3ABFD583566F0DE567DFD115D5D653383D545FF5D1EF
              101B2E50CAE6E4414D0BA1DD0D24ED1E8110601AD2BEE0C733FA093BEE7F2485
              34D789914261F4B11709EFAC1B90A6D13CFDCB53B1C4E7BD9B2B50F2930BA8A5
              95BFB22DCE9CD8D3FD1B1433B5A1A495DCFBFACB98AA4EE99DABEC1CEAA4F4F3
              AB1405FA11D260765F0B216FBD289CBC896224D76428714D0C12F63D5A6ED85C
              45B6BD5F796FF1DA5F565F8705D24FCDFDEC8F1B04F28DEA9E37842D3ABF3E73
              CDC9BD275EA6C8DF4765EF9FB0870358134B585231D4588882B92FF0DCFF9850
              C511C2DE2F5334F6A9146BAAAB480347708C60F5D146C5B9A333159C9A4A8EDE
              4ADB005C4FB7019C29F25F57325DB894E6C03D71935D836F23909BEC00D6E432
              959FFC91E88E6A16CBF66FBA138EE0286EFF750538B3C29716A01416A3D73679
              81E74A46BA3206D71767F0DDF82B8A406C072D1E118553B789941FCC18A774A4
              0B84784EF5EEF5AABBF7A705E8B5CD08C572520F4F2AD9DEB9943273FA6BA02D
              CD91D4DD196DF650003D34A10838A9D73D9E16A0551F0278D23973275BEC9C90
              D25D5892D12DEDAE34CF932BBC28D6920A8083F6D0784E04DB5521A5DA65B8FC
              30DB25A387C6418883D6D24A00ACC26A0581578D067312B025B9562047BFF623
              B4A559DC8101C9166D5E8D0641E215166B5A40DA4F38D6BEDD6C90524A218400
              308545CEEF39C6F48167B085A7A8EEF92D993AE343284602048E87DF5690805C
              362DAA3367052B881714CBFB475FC4501D940FBE8D67B437635B5E0BD3A20172
              99957FD22A4D132481A4DDB33F1F7229A51CFBEA0FB145A6D9DDD781C548400E
              F345D2EE014940AE744225F5C00F703B56E4CB871F4373B0ECA914E5B7AE3C24
              CF092B3CB757785192E37700AE45CA6AF312602AE94BA418F1BCCEADF05C5BE1
              45890DF7005C8EB97799CB85DEBC82E58B65B7373D31C1E5D850775A80313F41
              FCEE6701249DB3FB5A720EA6C6C21C7EEBA7528D85733E335BD30292CED4DC78
              20F1EF9B69010091AE7680730B1547CCA8A72AE780F920EAA962A1E288099C8B
              5C6D5FFD5D0188F6BE4BC23FDC2F1117FC8DA7302C5AD680D307BEC960EBAFC5
              74ED33597D0D8B86BFF114127121353FD9BFF8C1C5F5029026F3ED67304DE374
              DC5936E06F6A430A25DF2433420A057F531B7167D98094F2F4FCC5579089D8AA
              7D7522328253C8C4724AAF7BFC4AC259D61AF35416B9260651A49131B073F62E
              3B87DFC3397B774B72C3A23176F405C23BEBEE4BE4F1C8FB6F0617FFF9E7753E
              EB86D2C4BDCF50F48288F648C3A584B3AC25E27BB4DC1E1C438D85F2CE3CEAA9
              62F4D84B2CEDA81E90521E8F7EFCCE44B0E32C6C18682C1B0FC66E77238D6444
              DFDFD46ED85CEE60F5D1C684B354684B73A8F14856E265B78FA9C3DF66B2BED5
              4C69CED7257C2FFCFE9BC160C7D98C5BD296AD53DBD744F10F5E452DFF527A33
              429CD0C313D937A342AF09F20A703E353FD93F7FF1156237AE6E2978FBDE6DB1
              5270EC3BB88EB7A1791FF181C861379497930FFC81C5AE0E163FB8B8EEC2E52F
              600DD48A3AF4038FA1556FDC8E419AC67FB7E3E19E6D77C1FF3BFC073EE5A590
              0532F8440000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C0864880000088E494441545885DD986B709C
              5519C77FCFBB6F7637BB9BCDFDD2943697B6B1250D125AB085D20A12AB33B4D4
              A9E20C82A00CC308CC203A03233A835F10C4191910701C2A1D11115A40AA2017
              51190A6DB8C446C0A60D49DAB40DDDECA6BB49F67E7B1F3F6C529A7637B7FAC9
              FFA733FB9E739EDF3EE739CF79CE11E623C386BDB10D7B431B666D0386CB0B80
              003AD1C50A0749FB07490D74933EDA332F339373CE5ACED64B71AFDD42717B07
              86D35D016C00CE03962294813841A32801A017E802F664C3C1447CDFDF88ECDE
              416AA0FB7F0FE85C7929655BEFA468F10A8720D700D723B24EC0402D8A624135
              5331249BC632ED641C5E324EAF2082A2719497802754F5B5E4C177197DFE17B3
              069D16D0F0945371FDCF286EEF3044E466E0C72252EF18F769E9B12E3CFE8314
              8F1EC3B032678CCD9A4E62954D846BCF65F49C0BC83A4B4455F781DEA5CA1B91
              379F6674E7FD6832363FC0A28656AA6FFB0D66C58265C01F4464B53BD0AB353D
              AFE21EE99B536CA8188C2E5A8D7FF946529E6A54F56955BD253DD41B0E3C7233
              D9C0D1B9013A96AEA2FA8EED180ED766447E6FA6A29EFAEE1D940E75CF2D684F
              9365D808B474E05FBE1115A30FD54DD9F140EFF003DF22E31B981D6051432BB5
              77FE11C3E9FEB6886C2B0E1EB63574FE96A2C4D859A04D55B4A289236B6E24ED
              2819013666C7FCDDBE7BB7923DF1E9F48086A79CDA7BFE425145DD6611E339CF
              708FADA1731B46369DD790221AAD5A2AB1CA26D2C5A5A8616226C338478728F1
              1FC0968E17844CBAAB38B4EE3652AEF26154D7258FF60C0CDFBB1532A9C28095
              B7FE1AD7055F6E1178DF151AF434EFFE5541B8D185EDF8566E26EDAE14D4C296
              8CAA6896ACC3831AA64836A51587F650BBFF656C99647E484F35FD1BEE206377
              7FA8E8DAF0EBDB13A3CFDE9B1FD0D9BA9EEA3B9E3044A4D34C45572DFBFBCFF3
              2EAB82FA566E919196CBA53834A8D507DFC0E33F701242C5205EB68860E35A42
              0D5FC01E1DA1E9ED47B1C747F342866B9673F892EF89C2839ACDFCF0F84FAF24
              F3E92727BFDB261B55B73C82595673B388DC744ED753B8438379270C36AF93E1
              D62BA5B2EF4D5DFCDE769C611F86953DE51F2B458931BCBE8FF1043E21D87831
              91BA73293FDCA9829E11F38EE808194709F18A860B458C176DE535FED87B2F9F
              FC6E00385AD753B4788503F8893BD0ABA543F993A882061BD7E2F1EDD7051FBE
              80A8E6ED3729F78901167DF02489D285126CBEA46002A8DBFF12662A6A03EE29
              6EEFC0AC6B9E0AE85E7B15825C2322F5353DAF164C2502B2E49FBF6451D753B3
              4E375EDF7E5CC1431A6A5853B08F2D1DA7B2FF2D44D882CA12D79AAB4E01346C
              B8DA3B00AE738CFBD43DD237AD4143B398C9083207950C1F20515A8F6598055D
              5E71E81D501511AE75AFFEEA67F6EC8D6D88D35D21C2FAD2635D6795880BC94C
              46400CC91615173EB912E3B8037D005BCC054B30BC5513800D2B11580F6278FC
              07676D547586003C4519A707D4525B3A3EED188FFF2022D22650616F6CCB019A
              B58D00E7A116C5A3C7660D385B29E8785D2BCEB1210C2B33ED02B97299C300DA
              8A725C1813C5E6D2A25850F35525050D4F68A67EC1C68B8997374865FFEE19E7
              B44702B986C812A33857049BB91F283353D3973DA74B44A6F5866598EA6FB902
              FFF28D94F8FEA3E583EF2A339477B65474B2593ED9D39C30E7940247DA749AF4
              E0A9B00A3A56FF797CAD9B49B92BA5ECC87BBA70DFB3E44BD2A7CBB04E323827
              1BE684A59865B3CF19F0742F66EC1E1DBCF07AA2352DE21C1BD2A6B71F554FA0
              77D6F3593607133C5126A267C28304324EEF9C0173732980220687D6DE44C2BB
              8085FF7A46CB0FEFCD7BB44DA78CB364B2393219DC86150E02F4669C5EC99ACE
              BC03A7D3A413A3954DC42B1A6561F74E2A0EEF99D5929EAE6449ED64B3D78A84
              728069FF20401722C42A9BE60C08B9584CBB2A00709DE89FD71C902B645535A9
              F05126C7853171BBDAAB100FD7AE98D7C422223AB1ED662A20A653A4F65C804E
              D078B27F5F0E307DB4072B361E47F5A5B1732E40C598B781B351C2BB804459BD
              003B53873F46E3E11C2040EC835700B6679C5E195DB46A5E063CC33D34BDF5B0
              9A89F1798D1F597A19AA44157D26BEEFF593BF1B0091779E47D15755759F7FF9
              462CC35670A2422A4A86F18CF4E5BD23CFA4A4A786D0E2D5008F93CD06A37B5F
              9C0A98EAEB2279F05D80BB529E1A022D57CCD948ACBC8148D552129E9A398D53
              60E8FC6FA0620B01F7453B774DB9DD9D7455FAF800EEF5570F00CB62554BDA3C
              FEDE82F7887CEADFF07D4E2CBB4C2CD341E9F18F663D6E64D9E5849AD70970BB
              954EBC3DF2D86D27E30F263C08901AE826FCE6D328DCAA62F41D5973234977D5
              AC0DCD47E30BDAF0B56E425577A8EAB6B15D0F933D3134A5CF94642A0E173577
              3F877D614B8B88ECB6C782554DEF3C8663B2CA9846B1F2062C5B1166621C67C4
              3F2BB82317DD8065989DAADA91E8D9130B3CF81D38E50276062080AD7A31753F
              DA81ADB4FA7CE0AF662A5ABBE8FD2729F11F98D1E86CA4E496D5D7BA0915A313
              B8327DBC3F347CFFD558913343EA8CEDAAB131E2FFFE07C5ABBEE213A7EB4F6A
              B37F716CF1EABA8CA30477F0D0BC76E9A4929E1A8E5C7403A1E675A2C84E55FD
              7ADA3710F63F700D1347EECC8000562444ACEB151C9F5B13B279AB7F87882B5E
              BEF8C250D3C58665B3E388F80BBE16E453DC5BCF70EB2686DABF49CA5D15026E
              57E5EE44CF9EF4C843DF2D0807333D609A76CAB6DE49C997AE436C661B708F88
              6C412D71073EC1E3EFC5151AC41E09604B4531AC3496CD41C6E925595243B4B2
              9948ED0A12A50B4555A3C0E3A0F759A96460ECCF0F137E6DDB19313737C00915
              D52FA3F46B3FA0B8BD031196825C0B5C25226D9C92094E974212D5BDC04EE059
              CD6682D1CE5D8CED7AE88CDD7A56809332EB9A715FB215577B07E68225085402
              2BF9EC8DBA1834823242EE8DFA238578FAC87E621FBC4274CF0B6443BEB9989C
              FF35D8F056616F6CC3ACC9BDF2E7CAC2DC3BBF6A2E8E33FE4192FDFBA624DEFF
              3BFD1763CAA148740D013C0000000049454E44AE426082}
          end>
      end
      item
        Name = 'debug-stop-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000006D49444154388D6360A01030
              32303030543A3A3218CBC890A4F1DEDBB70C65DBB631B03030303018CBC83078
              6B68FC23C580734F9F32313030400C808183333AFE13A3D93EA38211C66622C5
              566C60D48051031818D05222720A23C9807B6FDFC2D336B1E0E6EBD7A4DA851D
              0000AEB517E3B433AB3A0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000000B449444154388DEDD3310A82
              6014C0F1BF21E8E0100A220D098D0EB9BBE4057272F2304DDDA2BD03B40B5A27
              C80B7CA322B8391434B4942014F859A3FFE9BDE1FDB6077F4E0158992681EBFE
              041565495155A80081EB7288E30D108EF4B27D9AE61DF82A7CDC6FBBB6A9A524
              C3B251351D20077A206D53733D1DA5403F4A982F96DD3E93BA1ED0044EE0040E
              A9F7CB8665E34789146058F657305335BDF7E81265EF4101583B0E5BCF1B0375
              9D85E022C44FC6C79ED33225363BFEAFED0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000000F5494441544889ED55310E82
              40109C514365E2B560447FE003FC9BCFF0459626B6966A228536545A90381682
              E1E040A3100B9D6A7687EC1CB3C5022D8319E97B5EA383CF4982AB7437E87B1E
              8EF379A306B3C502EB28422FDF247905004922C93A5EA3495237FBC632008038
              DA2B3EEC0042F7297073876686639920B4E6590692141F76D8AE96C21B98901C
              F823ABD7C9172499AD9D29AAB84B4BEB6A8336508A284B5892ACBE8357D496C1
              3FA21F88E8BF83EFEFA0F5888AF7806638D6A4F89F2FC20421485A26C57B2013
              8418F8A34796929CFC99E634C85FA2FC2BAAF8330D488F7E87C4D4F74BE227D8
              9C4EB82449A3339DB8012F5BCA4966FC6CC60000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000009D4944415458856360180523
              1D30A20B543A3A3218CBC8D0C4B27B6FDF32946DDB8622C682AEC8584686C15B
              43E31F2D1C70EEE9532674310C07C0C099D5F3FEFFFEFE8D2A16B37272319884
              266184365E07FCFEFE8DE1D7B72F5471003E801124F406A30E1875C0A803461D
              30EA8051078C3A60D401385B44AC9C5C54B3049F59381D80AB0D476D80E1807B
              6FDF626DBD5203DC7CFD9A16C68E82210E00056A20C3510F519F000000004945
              4E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000000D2494441545885EDD7A10A83
              5014C6F16F63C574B1190C36AB685B19669F61B067F03DB6B7591EDC2770D162
              12146E18A6D515C340BD0C8ED730BE5F3CE7C2F9D70B1091D56E6E78C9321CA3
              68D390DA185CB59ECC0F738F8F5184739ADE0024AEC346956E9AF2E7C051F21E
              5EA7A16FDD65015041084FF98B7B5B2086BE45FDB8AF1EF52DCE0B6BE0DEE9F5
              1530508A81520C9462A01403A51828C54029064A31508A81520C94B2FE8B5510
              22CE0BA7012A08AD7B5B60E529DFFAA95E51B5B4980DAC8D816E9AD25DCFD4B3
              EBB63C47F43F3E8EA025D049DCF9FB0000000049454E44AE426082}
          end>
      end
      item
        Name = 'save-as-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000025F49444154388D9D915D4853
              6118C7FFEF719F376508954D44D82187045A64E1CE99D361599C1DD202DBA422
              68D0455E795150D7EB2262372E082218E6851F79264437C170D5826D47B2350F
              75A173590A91A9DB74E56C7BBB10F313097F972FCFFB3CFF0F92EC550AD803F1
              84C28877DBA00280A037481B3A1AC8AD67B7775DF6E8EA03666D76ED8DD9CB75
              00A094EE6D01D522F47D7EF658694D39DC6EF7AA85FF6529BBF4DAD856E9D0D7
              16FB8AB4AAD32D152D37372920BBA0516B029F0A93CE12F3E1E7D1F7F2D9AEAE
              AEFD001E6F52E0BE780F0008B690CE66DE4C3233D7D8939503CB2B3973201080
              CD66030099247B9542D01BA420DB3F0240414383EC6593B3A4AEB43F1C0DF399
              4C063CCF43A7D38529A5E7D615505000D858D15236FBF6672ED57EA0F6607F44
              8EF04343431045117ABD3E92CFE7CFFB7CBECC8E2176F4DCA15AB566B85D7438
              4D5CD540448EF27EBF1FA22882337311922AB44C7D184F7B3C9E9D6B54ABD4C3
              56DEDA6E3C71B43F3A225BFC7E3FEC763BCC75E6E8C2D86C6B51E24F4F7E7E05
              00C0504A13866AC386AAB2A12BA2D369B1D5F78D8E8ED64B9204BBDD0ECECCC9
              33A1A9D6CF4FE3DD5A95A68921AB4E19008D2CCF4E94D5948112BAF86361EE92
              E954555F2C16AB972409822080E778393B916EFD26257C45BF48D346B5AA272F
              7D5F5DC2F54623670C1C396E78F7BB0296E4D417EBE0E020044180C5621921A9
              C285E2457DB7ED46E399AD7655F77B1E6239979BB656732683A91CBA43FB5EA4
              3EA6E072B9C0B2EC08A5B439191B5FC8CFE59A09596F7A7C3AB13D3CAFD70B45
              51C28AA2BC5214C5118FC7B59D9D9D3BE5FC8FBF1B93FCC056357B3F00000000
              49454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000037E49444154388DA5945D4C9B
              551CC69F73DEB785B62FB51F60C16E3343482F74A5D0A2639991C5AC40ADFBC2
              8C7609895C1913C5F91163BCDB951770C10DE824BA2189517C8B711D55B822D9
              4DE9DB2E44B325CDD8C63A6C6123D82E851678798F1783D249F0039FAB939C9C
              DF799EFFFF7F0E99FDEE8682FFAF0B7D62FF853E71003C00CC5C9B6142B900D9
              A2408C5E61FF747A9FF119BCD178824EFF38CD6A8ED640A8100A7B3C006417B3
              E04B78AC1937706BE1CEBFB695FE3D0D794D061803DBB441FF5BB29D620A9315
              C6169E70B867989AC5920F173AD48784BBE7BA3BE13C7978EFC05C2E173BD05A
              7D5CEF349F41A52A2A40956830379CDD53E46C7E599AC925DC9663FB3A3416DD
              E0D8D898211E8FDB0921DF3FE150AF2943534DE3DF5EB2944D4BA5666D8BF990
              C547B4DCC0C8C80889C56200009BCDF66C01987F9487216B20DE03C77785293C
              BBBEA849B7E99F3777084F950DC8B24C2A2A2A505F5F0F8FC703C6D8C705E07C
              7C9ECCC7E777856DF04AACF2A8D5BDDFFB5C3BD5719F8BA24832990CBABABAC0
              711C14457917C057DB91190A03DDFC4E332986AD2CAF4C27171EB4188E3C7D86
              EAB82F445124D168140E87039452288AD2CD181B181E1EDEBDCB816890CDFD91
              22BA124DE498EB154FA57BFF29A2E12E16C37C3E1F0823EF913CFA43C1107A7A
              7A761FECFB4B4924D3A9E8868096D2DAB29344C37D298A22912409757575F0F9
              7C60EBCA792591FFA6E4B6D23277E31E80AD97427602B5251AE960F541B7C7FB
              DA6983C9381808040A30BFDF0F39279F4F4EDE1B5ABBBD32CE317A78EB1C658C
              49B52FD742A55115D7ECD7571B9B3DDE13AF9F32994D83814080442291823339
              B7FEFE6CE8D6E5C44F7742DC3A7D1100C8A6290AC0AD3569A71CA71D506BD560
              60AB73B3A9368BDDEA359A8C83A3A3A3341289C06EB73FAED9063E580CA72E2D
              4EA67E56C97CD363D876445E8A5FCF34DA1A5AB5466DC879D6D9F4E0FEC3315A
              5B9A251AAE3F180CD2A9A929D8ED76F8FD7E50D00F596AF56B8BDA345EEE75BE
              040042F9F6D70500FC9B9FBD85CB9F5C7CE4B235B4AA75EAEECA17AAAEAE5693
              8EE5E5656D381C86CBE5427B7B3B28A51F911CEB2BCDF06EAA578F438FF12267
              935B0BB2D90074B57582E7789C7BBB134295FE5A2693394208815EAF5F05F029
              80BE5F7E0821F1DB5D14D76C4BE19B12C237A59DFD9D989880D56ABD0AA00AC0
              25C6D8B78CB1A5A1A121F4F6F6EE1C87BFE84FC06D604F22175C5E0000000049
              454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000004BB494441544889AD554B6F13
              67143DDFCC30786C53DBA913DB634302B6134026A91A1E52694B218E2954912A
              153555A958B4E2B1E10FB45B36ADCA2F288EA2B40458B4021522B571FA58B009
              2AA8282910E3F8199428013B8D8DC713F3DD2E1C3B4E48A00BEEEADCFB49E7CE
              3DF7312C71799C03001111638CFD5FFC82372222B1E7CB8F3116FF071200E4A6
              7294CD64B175DF564C4CC7283A132380D568D6C615DFEFF0A2D5E96589D1046C
              1E1B59DD56D49B444494CD6491184D50CBDE1644676274E3EF610240F54CEB60
              7CD0C1E0776C43623441606016D5B22281C02A0600A8808AC3EA1ED6C275522D
              BF81A14EC14A02BC422B2F9635ADA85D2222288A019224552422AA545D0115A7
              167C01AE0F914CF772DAC28736458FF16619FD5706907934F56A24D2B4D203CF
              81E62E47976751DE69FE0316310F81F5BBDD6E455AABD4FA515C0F57FD5C7EFE
              5EA6F028E478CF6390EC1B2345ADE81DBB3D86CECECE1382203C2F51A586559D
              5AC74441BCFB986743D236E3C62AF9850B17904EA70900F6ECD9F391B4542D55
              BFE8DDB6B7A8B3E50DA06EF8D7C20B858507B7A6EFBEDFB67787A1718B63040C
              DEA1A121643219783C1E16080400E0F6B2440C2C319A00004620624B9C04C26A
              4C06DC336E3685BA0F0765439329A2699AB7582CE2C891232897CBE8E9E981A2
              2871CEF9898A444BBB131F8DBF74B99E89FCBE756743B0797F83A03499235A49
              F385C361E4F3799C397306BDBDBD20A204E73C08205991088C405871570E9E3D
              282CF7A5127F5A2846D393996E53874D54B66C8A14B5A22F1C0E239D4E435555
              C8B20C0049223AC4394F0E0E0E62CD29AA5A742646D19949000C46D930E6737B
              8FBABA9B21DAE561ADA4F9ABE42E970B274F9E84D1684C10A7205B447270F022
              BEFEF69B972598C4D0DD081863E31DFE5DA1967D7E41B4CB235A496B0D87C348
              A552A4AA2A5B224FA1C8BBC51CE536E4801B3F5E070008ABC7B4EAD485C7776C
              DB1E7A73FF6ED1BEB9A9469E4C266BE4269329557A523C549C582848D33422E8
              CB532E31C698ACC854BB5775BA1B6565BCDD1F08ED7E7BAFD0DAD63A5C2A95AA
              5F0E97CB55232F4CE783539178DEC2CCBFDADB2DEDF5D74420A2EB6A40656A40
              5DB15C4F0BC5873E8FF7687728047FAB3FA2EB7A5B5F5F1F52A9149C4E2796C8
              D385E98560FCE789DCDCCDE95F048DB5AF965900700C0C57FD07FC70B7BB2B05
              30E2C9FBA9A3AF791BCA4D9B1D23BAAEB785C361241209AA929BCDE6F4E2BC7E
              E8D16FA9F97FEF3C19969E49ED04C2EA5B28CDCECFE98D16FB27007EF0BDE33B
              666A30213B9B1D911DCA43D12E5F2BE9A5ED7D7D7D482693703A9DAC4A8E220F
              52BA946F521A22D64ED32E02C1E6B68131B6228974FCDC17B8F85558B75B5F3F
              CEC0480DA8C72C4FADDFE9CD82CA4476F8E69F37914C26E1703870EAD42998CD
              E60C08DD628E1E6F2A2B119B6FD3AEA5BE3DF7B30100219A89E1D3739F6336F7
              7891887A39E7E20697E127B9D1F019116D88C562E8EAEAC2E9D3A761369BA788
              A80B658A4A3964052674129148442200B10ED7AC96B2D16A87C3D6044531A0FF
              F20020B23B003A38E7248AE23322FA9D88CE32C62606BFBF886B57AEAEB53A35
              7B3835094DD796176D363787D9DC1C8C4623480018F017009320089738E7FD00
              E29C730C0C0CE0FCF9F3CF35733DFB0FA22B9561ADD017890000000049454E44
              AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000004D8494441545885ED966D4C53
              5718C7FFE796A2D042A1B7526499C2408DDB0A3802D920EA40F69616B06ED94C
              0C3131D8BB64C14FCBB2F0C1E9D0216C864C265337900123BC358002328C22C6
              51C3CBA02A6D29D0E1AC4B8A236B07A5026DEF3E106A412AF8C697EDFFE99EFB
              3CF7FCFEE79EE73CF792CC5D1F232A5C8295D26DD31FC82ECD758DBDA2C22548
              8E79D3B952066E1AFA29F7B1D7DC455B419BCB4462662295F9F3E74F0D2BD873
              8C6D2B6863DDE75D98F3D08D95D6FF06FE7B06D805E3153340283266B3DDCF17
              040740269361F5EAD52B648080102F322C8A0F4E9AD9C4718AE25EA8CCCDCD4D
              2A2929019FCF7FD0079E1B9F4306D724AC7D7FDD071167BBD53D31232323904A
              A5728944F251565656FD7335E0703846C2766EDC25DC2E2EFEB5B323A6B1B111
              00603299BC3233334F444444D43FB72D58C5F11EFEDD7977A76847485147972A
              760E0E0056AB150078003C6F41C19E634F0C77D81D0683D32817BD1A7CDA0147
              9C5EAF77C5F87C3EF6EDDB07005F5A2C1637030484104200E0CAC92B4F0C0721
              FAD094F0B4B56FAF2B5275AA5EEFEDEDC5DEBD7B5151510193C9048661101414
              74707C7CFCDBFCFCFC675C84047A615C50AA3031F8C7EB5DD7DF686868000094
              9797233D3D1D369B0D2291E88BF1F1F1A3191919D06834CFF01812E8E938716A
              E8EE0D3F74A9BB13EAEBEB5DA1D1D1D159382D3A3469B61EC9C8C8C0AD5BB700
              B8D7000B9665D9858D6A796C8AE8E93871DAFADD1BCE74DFE849A8ABAB734DC5
              E7F3C1300CA1FDE8435EB79DAD7FDE1871C1E71B58448B7DBF17CA6EB70F8FD9
              2C729F2D82533D377FDBAA542A5D311E8F07854201A14FE0E13B9583CD9BA223
              2E112702DC9F5FB2063E29FBCCE3DF120106D2925353E293124E77F5766FAFAD
              AD9D07671806B4AF30DB50AA6D32ABC75A36BFB6D17FB6CC1FE8696A4017151D
              2DDBB235E654775F4FA207F81143A9B6D1AC1E6B2184042E36C9631938999E37
              97AF8B8E8E4E91CBE5DF6B349AA49A9A1ACCEDB9AFAF2F140A05685FE15143A9
              F6BCF98667F8631B7083CBE47279A156AB4DAEAEAE9E076718066BF8A2AF0C65
              BAF366F5580B8167F8AC019605CBB2B6C0171F99070098B24F0FA5ED4891CAE5
              F2933A9DEEADAAAAAA87E0625A9CC30E4EFD62518F5D78D4CA5D064CE67B0090
              2A9149EE07AEF39C4F408C5ADBD0BBF13B12BE1B181878A7B2B2F2A1D7EEE7E7
              77ACF95CE3797F8E6F3380A55704803AFC530EDAD5D72E511C4A26914A2685EB
              858B26F278BCA2C08D418A7E9DE63D0FF05CA55279EE6AE7B50B1445F92D070E
              00D4F4CC3414C70FA05D7DED0AC5A1A412A9C44A87D2F392589675BEB2F9E50A
              9AA6D39B9A9AE074CE9E4C1F1F1F28140A88E9A0BCAB17DB1BFAFBFB2F00102C
              170E00AE53E9EDE58D339F9EC0F6A8847880341342FCE76253F6A9F6BFC4D6E3
              E6E97FEA0B0B0B5D2BDFBF7F3F42D6867CCD313A9A56FD4D355314C55F0A78D3
              D04FA5647DE81ABB1AD1B47D1A8A6F0E208FC9EE7829243480B8758C30493802
              8385CA8BCA4B242C2C0CB1B1B1888C8C9CF1F6F6CE9BB2DE3F78473D04E2843F
              C1822EB38886EE1A167F039E24140A71F9F2651197CB355A2C16AE4020D00228
              0650363C3C7C2F2727072A956A49B0272DD98AB76DDB062E979B06A05C201014
              4D4C4CA85A5B5BA1542AD1D7D7F7C4E0651B989999C1E8E868B1D1682CAAABAB
              434B4B0B2627279F1A3CA77F017EC9E65036311E9F0000000049454E44AE4260
              82}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000006BF494441545885EDD87B4C5B
              D71D07F0EFB9F7FA7529AE81F01A4B13C03C9378318124CDD634DB923E16EC34
              6469A3C90A2662CB364D59352D5BA5AD8BAAB60152DAA4A9B2061B232486B23F
              88906A594D4C026A2AD2D23C90CA984AAA6008DC05A76B404D0C7EDC7BCFFE20
              781803312979FCD1EF3F96EF3DBF733FF75C9F7B8E4C5EFAF10E94E417E151C9
              1FDFFF4BC477AE24BF083F7FFA857700AC7E38A488FC240A78E773F5E8F0E8D3
              63C258F8445A411A8627AE932FBDFD8B2A488C4BA0EBB3D7104F97874E1D536B
              D5482F4827B3B59F02624C18C3C06703E1225D868E7C79B31F1FF69C5954A03E
              250BEBB3D7CCBCD69C406651AF7E1FF21DF0DBE63BE0B7CD2305A494461D7B24
              8054A6B74381D03F650E282E2E467C7C7CF8DCC3031210C2108651309FA996F1
              79E3E9F2DF03396457636363BADBED86D1687CC840008425E793D6A7967EFFC5
              ECA34CBAE2A3DB415F3321E48A56ABDD6AB3D9B062C58AFFAF240F3AAC8AFB24
              B124D9BCB42CDB3621FBB7DB8FD8E1F57AB165CB167EF3E6CD2D3CCF6FAAACAC
              EC7A2823180A8997F43BF24C4BCBB28FFB6960BBCD6683D7EB0500B4B5B5A1A7
              A74709E0659D4EF7E01F712814BC3844469E7F7C43F2FB7E1AD8515757871B37
              6E44B4F1F97C00C01342E67FC4EBB2D62027356BD1703EBFEFE257DCD8CF74F9
              4BDE234AE6C5D31F9CA6337185858558BB762D003489A238034810DE518C7C31
              028D5603C5220DB2224EF149F2AA1473568EFAA83F1478E9D2A54BD46432C1EB
              F5627070308CB3582C6018E6AF94D296B367CF4E0312801012067AFBBC8B0203
              0030A433F54719266D66DA7B7E31B0CB66B361646404C160107BF6EC81DD6E47
              7C7C3C2C160B58967D95525AD5D4D48413274E3C8059CCA033A128B9347DEB13
              47FD08FC620A0700ADADADE0380E959595502A956059F66F94D2834D4D4DA8A9
              A9B9537E5F71A433A128A574D94EFD11512959EC763BAE5FBF1ED1A4B7B7171A
              8D86B02C7B0032DE9C8E03A6EDA841013ADB62788F210CE94C2C4A2E7D62A7FE
              B0A89476DB6C362A0842449B828202582C16021107D831FA2E771311B848E024
              320AA88E5723AD206DD6EDF85CF1DFF677C909A434E385AC5A492597DB6DF628
              5C7E7E3E76EFDE0D04E86BC17EDF11ADA8394508F9E1CCBEEEFA1B546BD5C85C
              9749AE8C5C8D6974350AF5A7A9BAA4AD8A5CFE509011F7D86C360C0F0F47B4C9
              CBCB4379793968407E5D700EBC2D79FCCE555B573D39DB038C7992BCDB567757
              2021E4E3E7373E6B7AEAC98D878254ACB4DBEDF3E1DE109C9E5A6FBBE0D47D4F
              B7F14E7D549F8B3949CEAD5CB9D254F45471B508E957F5F5F5181A1A9A158720
              7D53707ADEF276084E506C9CAFD37B026624A4E3F75BF64EBFDD730683C1BC6D
              DBB6832A95EAD7F5F5F5B876ED5A444D6E6EEE14EEE0B0D373C8DB213821CF8F
              BB6720AFD420372D7B0A78CE603098CD66F31B6AB5FAB70E8723BC324CC759AD
              D6295CCD8D187100C0513AFB563B962858EE5C7E6181C96C36BFAE56AB7FE770
              3830303010D12627270756AB15ACCC5409A7168603000E936F966F52F429107A
              0484264231158E07273EDDF9CC0E536691FE35B55ABDCFE170C0E3F144E12A2A
              2AC050522D5F0BD4D261D10D19EB62C5010033EE1F07805FF289FCBF8C6546A2
              E015772DBAE5BFDDD51F1A7ACEB061F5019EE75F6E68689813170A856ABA3ABB
              AABFF9CFE8872A5EB97E213800606A4E1CC6C52F2E7F05E0A77C02FFB971BB91
              2879E5BC45B292BEAAD527FD4662E43F343434A0BF3FF20F26BD5E0FABD58A50
              2874E8CC9933D51F9C76BAC603130B1AB930703C3081F2EABDB8D077F9BF7790
              DDC6322351C6CD891CC8C9CDFD383129719FCBE5C2D5AB57A3701515151045F1
              AD8E8E8EAAF3E7CFBB24498A5A21620D0700E3810958ABF6A2F195BA9B25F945
              9B353ACD696399B1F8CA4757685C421C008437AE0CC7FEC3F803E3339224A576
              77774774969D9D8D8A8A0AB0606A7D43BE9A31CFD7AEAC25CB360093339FD512
              E832745188C7963C362730E2D5CDAB34687CE5388AF38A1E27849C2284CC782C
              948EC85FE76B0A75077BFFDDBBBDB9B9397C666AB6722CF736332A57A904B4B1
              845D13FB584DDEFFF25D2BE60686917F3E8E9282C9BEA72F3F3243115C8A2592
              960CD7D7D72B06070761301850525282E5CB971300B554A67F527928381F89AA
              8F257705CE9763C78E61D3A64DFB44513CDCDDDD0D83C100954A350AA0198083
              52FAB9DBEDC6FEFDFB2149D2826073256660525212DADBDBC171DC6542888152
              DA4E296D00D01A0C06036EB71B274F9EC4850B17160536959877339999996059
              3603805396E5320003BDBDBD686D6D85CBE5C2AD5BB71615B660602010802008
              822449073A3B3BD1D2D282BEBEBEFB829A9EFF0185E7B74428D64A2000000000
              49454E44AE426082}
          end>
      end
      item
        Name = 'save-all-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000010349444154388DC592B14EC3
              301445CF8BFC1719533151656065482436D42543A70C2C480CE94685943FE8DC
              74EBC8C0943FA8927F40DDA28825DF50B1E4B160D4428292327017DB7AF63DEF
              DA168EB45D6EB8B90A5A46C8002CA2077C6F8AEF5D023823CEB706C0F7A6A3C9
              1664B6CBCD59643B91B797FD50B202F2096AED6800CAACD4200924797ED2AE93
              59BC92625D68B808A533C75FF4FF06E67891C5AB1F19071B9459D979815622D2
              697ED24190049D9B8A75D10E3200F8FE947DB10EEF07AAA6761C55AD5DDF3D29
              465124AAFA6BA4AAA999A5730C104EAE273B11F16C31CF73ED6BD992ABA60640
              D2F891FBDB3B17D889C8450FD07E635EEBBD334BE75F850F217052486849E8F2
              0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000001C249444154388DCD943F4B5B
              511C869F73EF91801C82918480B78DB14410841094080EA20E0E0E1DA4080E5D
              BA140AC58EFD0685D242C53F1F209031BAB83988528782149DBA18A50E16A377
              B8D66BA49AEBEDD0A4C6C45363BAF8C281F3E7E539EF8FDFE1086AB431B7C2A3
              887555BBDFA8244067F431E99E3E005A03AD00A34DB052C0270990EEE9E3E3AB
              77C3C04893C1D680EDBF09CB8A03C34D26AB40911B732B953233E5715FAD562F
              64130DF001519E1BB5871220FF39EFABB0A214BD62F1EBB296F4ACFF29664170
              669FF989A184B8CD23015CDB45062497ED1E3B853D2DF0FCF21CF9D3C0B55DAD
              A72EF2FFEAE103ABDF2156A88337632FB5662BD441E1FB616340E7C001A00553
              6B3EDA2FE01C3808716B83EB81EEB18B8A28ADF9F4F814EFC2A3CD6ABB1B08A0
              228AD4444A7BFDD6D2967FF2E3440BAB0356F43AFBD6AFDD9B7FFE5E5FE71FAD
              038601BE178C06EB4E93C924B158EC0EC6B566720BC4A77A317C9F175D835D9E
              95B46E181CC7A1582C36C2CA50FE690064C92B65A529BDC4502263EFDA264077
              F409FC8280192454955E4514420854F8BA7133B9850CC0976F9B0088F1813166
              A73F204D3929208B102DFF4853FDD30018F1A9DE1B86DFD56A7706D8E95D4C00
              00000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000246494441544889BD943D4F14
              5114869F032341287637EEB0C92E09ECB293509850299DD8501163213116FE03
              2B1B63B471EDACD5D25828893FC0D858D06F69C81488CB1640309A2C6B4362C1
              6B313BE39D81FDC0104F73DF39F7E3DCF3DC73C6C8D8F4E4145B6F9B98D90980
              24999965F5A0B9584B92E71E0C30D51B8167D9E0E7B49BC08AE7DEDA3549CF1D
              CD597AD09C244B02C4360CCB88882469DCCC9084373D3975E1585C87E7A21905
              CB1044160788FDDE79B00C409460890F4F10011CED1FA9B3D7A1BA5CE5EBF76F
              DA3E6C6186A2C59CD241A946505AB076B34D61B6A07C25DF979927499DBD0EED
              665BF3D7E7D93E6CF1E9CB67F5DD01AC2DAD5A7DA6A676B32D0CCB95737D518E
              59640038B2A7A3AFAC4E8D18CEFE9406181B74D38B304F91C5692559CAC9F70C
              6DB14F08673FCE3AE03F204A75324050AAB1B6B46A8EEB940E4AB591F0F409B0
              60F599DAF03E1858674E00F70D365F6E9E38731A410F7D03AF87571071CD57F2
              E42A390C4B0EC8EACE5E87EE4137CAA8F70671F7C687A73AD9B57C25CFDCB539
              062192A4EE41F7FC8822250378F0EED199B85EDD7F91AC1D82A801343C33B389
              CB134AAA8FBF255B2C16F17D9F300C939BC73FB7E4BB0FA287AF1FB3B3DF624C
              D2C7F2D5B295AF96DD7204C0F77D161717474291B59DFD165BBB211E621DE343
              B012DC76FEC44852188684619846921DD3881A40C30DE4FDF8F5F3B79F2BDE03
              36EA37EA778E8F8E05649B2DB15EA5A9BA5C35210A95428228C612670060C1EC
              021B4FDF50CC5DB96466EF81F57F6202E3B79EDC656B374C39FF00EE51B3DA97
              510DF10000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000001ED4944415458856364C00372
              833218F49575F129A1083C7CF98881059F027D655D061763877FB472C0E57B57
              99F03A00060E4C39F09F818181C121C781316749F97F722D9C12D3C9886C1603
              03030313B986510B8C3A60C01DC0822FABE92BEB30FCFCF29349C55685E1DFDF
              7F0C2F5FBE620831F16378F7F53D5186FFFDF797E1D68BBB0C2F3EBDC2999358
              88C96A32FA3270B6B8B8183176FF6760606064606060D879651FD3968BBB702A
              8467436A66B5FD93F7FF6764646470C8716014E0E4C7AB9EA8728012A024A0C0
              3039BA63E01CF0EFED1F86032B0E2004182151030334CF05BFBEFEC22B3FE0D9
              70D40103EE00ACB9604A4C27233671BA38005620510218191989F6C08047C1A8
              03F016C5B0761B3160FFE4FDFFB0C53D2B072BF90E6060606020A666C4976B04
              6405181CB21DC87700A5E03FEB7F86CBF7AE628DEA3B4FEF11EF80A0A020C6B5
              6BD7620D667CE0CED37B0CBE556138E599208D1706064626BA953D680EF8CFC0
              F0FFFFFF0DDA1EDA03E208A645BB9633FCFCFD334C585178AD8E970E03233376
              47AC5BB7EE3FA9C14F94030E5D3ACA90D69BF7E7E7EF9F9142F2422B74BD7519
              9898E9573CC07D64A767CD30AB7812333B1BFB3C4646C65832CC82B78491C1E5
              7B5799F02542140DB67A560C25E1790C8C0C8C0CD40AED3B4FEF31144EADC029
              0F00070590859FA321DF0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000335494441545885ED984F4893
              611CC7BFCFEBD6FEBC737BDF57C3D1DC9ACE04116AE290210526696154441182
              87EA929740280FE2B12E42A76ED2C96387BA14186158B04B8860ECD0B4A92C68
              F94AD166BAF9AA9B4F17375D73DBBBEDD5EDB0CF6DDBEFF93D1F9EFD9EDFF3BC
              2F810C46078621187939A18AB2145C864A4E609FBB17F5272DD3472DF41F5F3E
              7F9D79244B708F2ED12752695D82B65A0B738B994C7AA7A8122602CBC3ED7091
              C04C80020067E1C059380090B78209C47911E160189C8583B9C58C49EF0725FC
              70A6AE116E870B81990000C0DE614F0A328ACC708454048BA5EC05656F92782C
              FE8951311400084310894688FE848E46B7378B99DF0980CB2A383A308C3E776F
              D62C66A10E3446BBAD4E2B84D30274461D36B7240CB86FE34F2494B7D54E3C86
              0571110BE2E23421A40B20990505239F68C25D59B35601BC95076FDD3F516AF9
              9A7CBC280E98F02CC77C5B5DCA3928F9178B3E918AF32200C079D3495ECDBEA1
              C1D04A3E021919EA19247E8F1F91DF116AA835A0E94213D1A9B5208480D2ECBD
              3E2928AD4B0807C3C91F82A115F85797151104808D5F1B58FBB996FC5C631070
              B9F522086120B099CFF9BC4E12253112033A4C6D50ABD50090B238072999E0F6
              DF6DF8DEFBB01BDBDDFF92A4EF9692F5C1D84E2C552E0365DFA82B82C552112C
              968C6DE656FB356CEE1475115084430513CF066A54293249E07B804AEB524163
              330916E3A328655F8315C162C97959D87B48CF3B71E84728E57A5528B9058D5A
              D83BECC4BFBA24FB2D82853F054A6956410DAB016FE541E3E969B5D55AF98209
              9E4FBD901B8AA19E0750E5A81E3DA747EB9556A84E645728D97D50A551014077
              CEB87C13DB6C36701C07AFD75B88570AF6FED69C3179EF6296656132990A122A
              0406940294A6146639C10000059E995BEAB61ADC0DA5F6498379F9F135A252F4
              1DA5B86173D9361B3B1BB30EF0F97CF0783CC7A40730B30B73B83B368888149D
              02C5756B9B35EA38EF3836815CA8006076610EF7C6063131323ECD6AD8ABF5E7
              EADF6A588D211A8A266BB3EFEC25D94905960763212024FD9D4BBEB59E92C1D5
              DC86899171E8B5FA4E42C85372D80CCAD12DA7CDA409B437B7E1F19D8720E4F0
              155092FE27F773C6FC03AEBDE8EF996E9C160000000049454E44AE426082}
          end>
      end
      item
        Name = 'printer'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000000D249444154388DA593B10D83
              301045DF115A5660030FC10A9658231D0CE13292C7806522537A0D3A9C023B89
              42208EF89D65DFBFEFFFEF84086B2D4DD32C64C03957B46D0B40F979D9F77D38
              2A36C6C8FBB9C8E97884D304A5B51600A55476515DD7A43A99A629CBB80F0440
              209AF8CBB83D12638C3C3DB8EB9BDCF54D42087F913D6354E3752D1491DDD747
              04B0CD780F5DD72D121B9D1EA40D0180D6FAAB92611816A2FB099B183315BC62
              74CE15B00E4755554B8E82799E2FDEFBF50B69ABE23602308EE35715C938EF3D
              BBDB989B44C203EEB95089513AFECF0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000001A649444154388DBD943D6FDA
              5014869F5B99C16470A840082F91EC295284182C75AC183C7441FC032F0C5EB2
              E43F2026D4953FD18585C1281562C443C4E058AA60A52A03F6C0402D850C0487
              24578961C8BBDD7BCEFBE89CFB71044FD2759DE170F81DF8CD71AA3B8E339A4C
              260028B28C5EAFB7CD42725D57BCDE9302E7F3F91105BED497939D9F0554DAED
              3600F97CFE6448ABD5A2D96C02208220B83D889D03B5D96C96E9524CD314C01D
              10EDF74410040FBEEF6F57ABD5C91502140A052CCB120A80EFFB847F23FEE7BF
              0270B6FCB31542BC7912EFC9300C2CCB7A7E36AB8B6FFCBBFC2100AE7E5D676A
              59A61458BE1F50BE1FEC404756270502542A151A8DC651807EBFCF62B1900355
              55C5344DE1795EA6966DDB16AAAABEC8957E3DCFF3D0348D5C2E27052549421C
              C7D8B6FD2626050254AB554AA592F42C97CBE5763C1E4B7D0AC8A7C693E1C3D6
              5DD70548FD0A503F88D7809FFB0A354D9342E238663A9DEE9737EC7ECB0EE838
              CE08A0582CD2ED765353144524492205AED7EBC3E55DA7D3198561B803EE27AD
              AEEB69866118006C361B295051943407200C43DE9DD8B233CDAA4775E781ABE0
              D3AE910000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000001D3494441544889A5563B72E2
              40107D6F8B2320672448019B6C468C8329D5EE2D2026D86808ED3219A771E640
              A24AF619544C06F916BAC26E6F80662C8D34D8C22FEAAEE9EF9B1EB58806B4D6
              58AD568F22F24092002022D22707CE9E8AA2D8AED76B17738400F23C97DA1124
              3BB2AF2BA57AE37412885CFCB32C938E750024A194A2F5BD9A80A4AD6C1045D6
              D7C7B7CF56792B821449A3DF906CF5BAF27E8AB4D64E99CFE75FA62849123463
              D218F3682BAF6D1722B2D8EFF79F6501C0658A48BE0178B3B144043C1C0E7F07
              451A881149E6792E5996F55231F0A139394D532AA5DA97FCE7FB2F2BF2CEBCB4
              82DDDC815C704930FBE90EEECC8B9B988FA6287406D414D9E7FEE3F9B733FC2A
              452E819F713A9D228EE3263521B973763C1E713A9DDA1DF809E238B62337B803
              11E92668DE412D1000B4D6FF1A76129001404862B7DB75E801BC3BF05E25C7E3
              31A228F29B6CE17C3EA3AA2AA79364EF1DF44D64144598CD66BD74595D445A09
              FC58C197BCD96C06ED034B918F11C9A7BA1A9BF95E4416B750547F8B5E9BDFA2
              5151145B6B9C2409269309012C809B287AADAA6A5B96E57B07CD05ADB5C672B9
              74CEC618186380EB53643BA788A02C4B5C5DFAB6B5344D077F87FA0625B8D142
              7F09D7D0B7D1FE03767C81D703C53B4D0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000136494441545885ED57BB7184
              30107D3A93D2822AB08AA00566E881C4CEA00720F38CCA80663C0AA5404D901D
              9718D97368F958DC1D012F432B468FB7BB4F0B03813CCF2184A0C29B60AD4555
              55DE5844BD2484409224D73D0828A52E548C2430A22CCB21E4F0BAAED95C9C64
              F62C9C044E0227818872BCBD5C100038E790524ED6ADB56052CADD1C6F060380
              892129A52ECE09431D6F250987D1215F5F03F70BDFE99793EABDFDB832C666BD
              3C145E05863BB99E4E80790AE65198A440749FBF5FFF60F9810314E1CB09CC4E
              444BD3CC5A14454176D339922D2A30224DD34DE968DB76958939024B52755DB7
              29157F0F6F9A8681F096C85AEB9DDB39E788E3D8DD92210AF47DFF668C99ECD1
              5A23A2FE587EAE69F71CA2803106599679F71DA7069610A2C01C8EDF867BB9E1
              66025477FC075A6B327603E6036AB38C85496E0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C0864880000022A494441545885ED98B16E9B
              4018C7FF34A803CA402406CB8351D9989A478822DD90D16FC19A4A3C83273752
              27061E829D25E98897783148A98CECADB52B5942BD4874A08B21957B87CFE670
              6DC9BF0971DF1D3FE04EDF77A7A0065DD7E1BA6E5D48637CDF479AA6DC76B5AE
              B3A669E8F7FB1F013CC8165B731F04C1786FC1353A809B300C0B695A000821CA
              7AEC5A4404010061183612DA84102214F74EEA535BE02CD894B36053CE824D39
              7A4155D775689AC66CEC743AAD0B1886816EB7CB6CCBF31CAAEBBA65BEE5A59D
              6B00B02CAB0DBFEBE170C86B5B4551342E53DD03809BBA911CC751248A957C06
              C01BF711C06D958BC3302C64E75B41FE2942082155AE3EFE45C2BAF9DDBEABAE
              DFFFFA89AB797430A14D98823FEC3BA5587FFACBC5CB7F153CCD5F0C00CADBEA
              925A49EF0A53F0C3D72F95D4C5EFD7C3D930600A5E2EBF1DDA83CBD1CFC1A317
              DCBAABB32CABAD3407CFF38AE9745A1B23BCEDF43C4FEA6A167D6961C16D6FDA
              16C282259AA671EBC76D504A4129DDA9CFCE82A669C2B6EDBDE6E46432299224
              D9A94F25480851448E23E238461CC752E6A3E33800BF1E04F026788FFA8ABA3A
              DDEAF57A304D732FA1D96C86F97CFEF7AD4F009E39E12B00507DDF4710046356
              846118D82CC929A5582E977B0932E6DFF3603078E2FDF62CCBA0A669CA3D4064
              6D6628A5582C16B20491240946A311B7CFE92C1251642E12114E3F1797889E88
              CA46447005E0B125C1D5B6805AC13CCF1145D118C0AD34A50DB22CAB6DFF0394
              D6B359F1F32E9D0000000049454E44AE426082}
          end>
      end
      item
        Name = 'command-redo-1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000022149444154388D8D904F48D3
              7118879FF7F7C7FDF9B166969BC1B02CC23C88640416583AFA03515A60E4A143
              9050E4C104B3439720A4C4539910A14104424160511DD2D34A2CB0281CAEA175
              292551B74A6573BA7D3B8882196E9FFBF3F0BCAF90C13C4D8FB017ED6F04FAE6
              067A86A6BBAE804A01A0652210BB05B013E8B5F69D2CCA39DF06B284EA990816
              BE8770EE3DF64A4C5B2170C3F415BED0737DD3F14F7D990992BF268887067096
              55BF14DD2C1691EB59BE5DCF35BB151500C9B263959FC151E247B7DC0E44DC80
              FC2B3236FBD0AC6C1DE8020A810A31BC05E45E7E80E9C9AF019A114A65F9C034
              534ABD912D2DAF31F376B4225C42D106AA07984AFD8928A592AB00CD6E21364B
              03EE01BB9552958699B7FD38504F4A55247E7C198C3EB9C97CF83D241757E77B
              0BF05EED16DD66DD07F60095F16060C40069003A1263E1C189D65A546C764DAA
              E12DC0DBDC8DEEF6740027007F2C18084FDEBD8886500AF4FE7E76FBBF3040CE
              B95BE8D99E3B0835C0E15830303CD97E01161368A8A56FA7E273E97EF60D3812
              0B0686966100DD5DDD700870189EFCBEB9FEA7805A43CE8F7E00C37C371F1AF8
              1979786D050690FCCED12A44BA411D8805DF7E8C3E6E61717C245DCDCA74ABFC
              745873B8721069373D5B3597FF6CD43A58AB5B65552C8C7F4D2423E3EB0AB4A9
              8E7A923391261475C029443E9B1BF3C6B2B615376A4E57DA0201D05C9B701DAD
              C359E24773BA2C44360033D39D4DB3F1E1FE75057F01D9E8B4CB2247034A0000
              000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000002E249444154388D95D36D68D5
              6518C7F1EFEF7FFEE7ECF89F3BCD313147B8B63A989406B99252B32809490AB2
              076631448A286AC5F2811ED688264605E18B3023584C900AC9DE44B545889212
              991284100D21E5549B7BD2531EB79D737EBD582D3577D8B9DEDC2FEE1F1FAEEB
              E2BE4519357B5533352D9D09600BF0D6B9235F8C0DEE7A0E8A85A94C500E18BB
              A21660155207B0376A5A93A87D720704B1FF32E5808A57102D5B7B420A32126D
              C092B02EFD697CFE35C5DCD11EB0CB03F3032799F8AD8F68E93DC7503020A90D
              5814AFBB765F6C6E7DF1FCD1AFCA0301F2BFF7913F7D8AE44DAB8F0023818236
              703A71D5C2CF8228654D8D33AB8ACAE5EB482E5C4610A5404225E078FD0D04C9
              08D00B92DEB4BD1BB13104886E7D8039EBDB8945A934F038B01C9827A9E404B6
              FF3D0B925A6C8F87952B1EA266C37624B5835E011781C3C061DB79A9549F0084
              C02393AE7F0C6B1EED40D236492FDAEE366CC63E3D71EA3885D17E8AD991CB2A
              C9C5B7134BCD0D806E49A1ED36E0DD50896493C456BBB8CBF653E77F3AC8F09E
              D7280CFC3A6D4B55776FA072C5BA00D325A9D9F656DB3BB2BD5D84A0676D466D
              36E58EF530B8B3F5A2977F396C4E73BB30EF4B7ACCF6CB86B7B35F7FC8E8C7DB
              088195C097CE8FFD35BCFBD592D8AC9BEFA5BAB95DC04E491B6D77006F647BBB
              18FDA8138000A916C88CFDF203C5B34325B71F9FD78860B5A4278C3B81D7CF5E
              800184D843405DE2EAC51026203F3E2D38D17F02430FF69D8803976293201C04
              D604512AAA7E70CBB94B031756EEFBCF199288CF6F3C503833C89FFBF7FC2FA3
              051FF4DDA2203864FB3DDBCF64BFE9E6CCBE77702E5B72FCE92A965AFB740605
              91A4E7810589861BBFADBAAB2517AFBF9E8A862554A497327EF278C9555CD461
              EABE56AAEF6F15A803E9256002FB1090018A489B329B570E17863233EB70ECE7
              EF3050916EDA2F059F0045200D5C87D40074677BBBB2335DC1D4470DAF6C64F6
              1DEB492EBA8D2059F9CFEDE475FFF687298CFC3123F06FF8F41FB39EA8739D00
              00000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000003A2494441544889A5956D6895
              6518C77FFFE7393BE7393B7393A5D3658A3BCDC2A21A5313C9B024C1911AC9A6
              04951411580E3329CA0F4144F5C12FD2DB20AC6D95BD9851831506665922686A
              E98631CC66E328B617E7E671E76CE79CE7EAC3D9700BD9DCE9FFE97EB9EEFFEF
              BA2EEE9B5BE420B7B894D2D7F6E278053701CB805D89133FD0F5DE26480F8D89
              7572021414E37811803D480DC053DE5DCB99B6F11D0804FF3FC032292C3BACC5
              EC32A24EE8F170C530C40D5C4D2617807FB907D243846EBBE7BCE067A006A95A
              702630A3AC256FF67C12C7BF07DFCF0D003078FA28645284E62F89611C12AC07
              D6026D819965A7DCE259247FDB973B6004A250986079650770588EB34E500DB4
              04E7CC6F730BA6A2D1079CC269E42FAC2218ADC0094FC96E4AD7F2BE2A3978B7
              2F05D7456805F035D9D6D700CDD9D36E1E450F6F61CAF2C77042F941E07E6029
              7023E069186266681470646ED9C9C8F242A01C91C4582B7911A66FFE90D0BC05
              487A04780398936BDB46CBCCF6068A37BC8E376F81907648AA35B324F0115813
              E82F2079BD15985950D207C3551C04D6072277AF027811D864666D408D99B5FA
              03FD245A7E22FDCF5932DD312C93BA66964EB880A9352FE1780579923E91B4C8
              CC0E19AC36F3E301602EE815A0DBCC5680C5E2073EE7D257DBB181FE715B202F
              42C9967A142E70311A816A33FBD5B007F1ADBFB7711B01D0D392C266FE5661B1
              BEE677E9FB66C784FD9517A1E4F906423757BA18F564DFC1318395F87EDFC5C6
              6D5C39B8070758616683A02F063B4ED1D7F4D6C4E6A17CA63F574F305AE902EF
              4B7A14F81DA8C2F77B47CC011CA432E0BC99F5C6F73582F9130202256584CA2B
              91A893F48461ADC04A33BF67B479168009B217C14F5C9ED0FC3F3A6D580BC603
              6699AE8B0D2F73E5E09763021C8C766096A4A9917BD75D976BBAB39DA133C701
              B6034BCCFCCE8B0D63331F915BF4D0E6A8A465407B6066D9B1F48533A4CE9D1E
              9F90493170F43BD23D311227F6A7E2FB3F26716CEF3543357BE79F5149AD403F
              B0C01F4A9EBBB4FB4DE2073E033F735D158D27B7684D6DAF240356015572033F
              7A77DCD79D5FB9122752885378034E308C53340DBFAF6BD20095BCB00BEFD6C5
              0ED2DB92369A5902F8146832AC5D288534E027E3B1D833774E1AE0F4ECDC4AAA
              F36FDFCC9E35B30D4037F024D0247412F803B386493B0FCBB5649C8123CDE4CD
              984BA0347A52A80E380C740067815348472C3DF44BFFB77593068CF94D82D10A
              228BD710BA65110A7AD9EDE1081B4C70E1D5D59306FC0BF4A579797E0C1AE800
              00000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000045F494441545885C596696C54
              5518869F73EF9DB9B3C0B4749962BAD0861A2B0C652B56544ADB40084D001311
              0D44D468EA16D4D408C644028A86841F908889064121D860C2524B886890A5C1
              8CF043D28EC844AC2D08AD433B74BA31FB3DFE68202538543A537D7FDE9CEF3E
              4FBE7C39E753497154E724B4CC3C8CC15E30E2A9FEFDDDA3973C4CEE675E47DE
              8E8BAECCDA6D2094116B463E710FD19C935035ED5945514ED8CB974CCF7C61CB
              88126A2A0588C7B0972F3D2B4C7A0182CDE6BC926FB5EC82AEE0B963801C7B01
              A3CF4FC8EBC636A7E6A8D0F462609339BFE4889A9DDF9D4822B51D00E2011FA1
              0B6E6C65354784499F0A6C34E59534AA6959D7432D27C65EE0A644B8DD83ADAC
              E6B050B5194288F57AE1B46F84C51E089D3F3DF60200F1EE3F89B47BA4AD6C71
              A350B5878077F5E2598784C5DE3B5C62CC0400625D9789B4B5485BD9E206A169
              8F016BF5E2590785C5DE7F53624C056E49B47B0CDBECC58784AA550375E6C933
              F7A39A06C25E372251A1B9B014EBCC85E89367A058C7EB08914E12F78696958F
              6273E808B10F180754F7ECDB74ED0E0173E13426ACDA805E34DD8510B5081681
              982C527C6949290F86DA3DCBB5E11FEDF35792B16AFD78A19AB6217806C95124
              1F813C27917E632060C878745440C5EA40E81633B0178913783DD2D6CC2D017B
              C5D364ACFE2057C0774018C99CD8F58EE68193F5049B8F13BD7A11A4312AB8B9
              A814E75BBB3581E56B24F7019503EE868EC0571B8766C094FB0039EF1DB22B26
              DD0D5C96522EEFFF7E67A8B7612B32121A15F43678DD6E55B139EA813940E5A0
              BBE1B27FE7DB208DA10EA43FF50E8A49DF3C54229FECA97F3F34707C4F526018
              1AE4EC21F81EA01CA81A0E07D0B49C22AC53E71502B54065FFF1BDC154C04D05
              53C8AAFB52516D8E5DC03C2965D58DB387DB86C30134EBF46A403C079C890FF4
              B803FBB7240DD7728A70BEF1B950ED8E1DC00289ACBAE139D5EADFB5EE8E39D2
              CC935C000B810383A7F723C38349C373D6D50B35CDF929500354853C4DBFF93F
              790562913BCF0B8B1DA004644BD0733239787601CEB5F5A869CEEDC0E34075D0
              D3E4EDDAFEF23FC20114210408315E426FCCD7969480E5C147D0D29DAF022B10
              2C0CFED274BE6BFB4B09E1008A448294FD0291A666E4262510BD760969C4BF00
              2A829EA696AE8FEF0E07D0643804E0054AADAE8A6391D69F472D10F6BAF17DB8
              3CA83A322F047FFD7144388012BDE20538062CB357AC00CD3C6A0180487B0BC1
              9613FF0A0EA01AC13EC655AEBC2484D8AC58C6FD00F24AD8FB535212F712D5E8
              F36371CD0F6819132702AFE9F797ED8E76B6C6621D17FF1B018068C7EFD81F7D
              E29410CA8B42288B6CB3171D94E11BB1C81FCD245AA7532A10EFE944C66351EB
              94B98D20D608A1ACB6BAE69DB1CE58E093B108F19EBF927E9412E5B685C4B1EC
              4DD297AE71005B6FED0370003817EFEBF61B7DDD868CC730FAAFFBAE6D7D7E74
              6FF3DD04002CD3E63361D506B4AC7C9748B011C502BEDCAB75733BC744000045
              C35A5A997027347ABB7D9D1B97A4A403FF7BFE06A401B37D23E7691000000000
              49454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000005A4494441545885CDD85B6C14
              551CC7F1EF7F6676B79D96167AB11742022D25E225A214A262D03411B16A00AF
              0411435510B5560A788B3E34E8033E0062BC2012BC1034048D260AD6DA780584
              B6409050A58004AC205DA82D30DB6977E6EF435BA058A59A5DEA2F99645EFE39
              9F9CC93973FE07FEE791780F60650D2363FE7B98299986D7DAE41F5D3819BF35
              DCE77A238E3600ECB1B71218947DB5699A3B8369D9F9594FBD8F9192D1E7FAB8
              034140A415910C90EA404EDED0AC27D7F4191977A053F3297E4BD36E60024212
              4A7520277F485F917107468FECE7F797A6E1B536ED4499803010A80EE4E4E7F6
              0579013E711772D134BCD6A6ED2813814CE04B2B3B2F2B73FE6A8CA481FD0BEC
              81744ED4A86A31305844BE0CE60ECFCC2C5D8E84ECFE0576238FBEFC00BE7B6A
              33702B305444BE081514A65D347755AF48F3420201FCE38771F7D4608F293E28
              56700B500ADC6CA5E7AE0D8D18DBE6D4AC07AFA3FF8000DEF1C3B4EDD98A3DB6
              F817B182B508A5C08D5D48F76C64BF00BB91EE9EADD8638AF78919DC81F03850
              64A6E7AE0D1614B6476AD68317EDFBAFCECACEC34CCD8C39345450C8C0297351
              659288AC0536ABEF1747767EE5845F9DF3CFC0844BAF23E99A29245E518461A7
              A4009703B922D2FB92FB8F51D5EED74922325955AB81DB4E7CBDA6AD576060C8
              4806DDB79050DEA8A008F782CC44B85A102B96B0F3A067477EFA61C55F06B4AF
              BD9DB4192F600442138157456498AAEE427951D12D0807514E89C4EE20A4AADD
              E7AA85824C57D5571456741CDE470F6052D174D2A65520C2421179465577ABFA
              37A952E5351FC1A9DB80DB50877FB29968F8D798E0ECC29B1974F73300155DB8
              D715CA9CDA0DFCB1A6E20C3034F25AD2A63E8F088B45E409557D13783C1A6E6C
              6F5EB788485D25F85E4C50DD491C3D9181772C00784E90E7557505CA6391BA0D
              1C5B5E06BED70994904DFACC4588699574E196A8EABCC8F62A8EBD350F759D98
              C2BA7119B35F464CEB6911A950D555C0C34EDD060D77E1804E6072D174CCF49C
              5C6089AA5601F34F6DFE84E3AB9E8CF9AC9D839B27222FAAEA6AE021A7B627EE
              347040D10C0479160828CC6E3FB053E3854BB87202E9B39622A65526222FF9EA
              7F805212D9F6B97F2E0EC008E68DC24ACB4902EE0756E3450F1C5B191F5CA860
              3419B31623A6F988882C56D5752833223F6FF5C26F96F73AA6152A2804982022
              49EAFBEF3A3BAA89FED6107BDCF0D164CE5D85114C9C2522CB54F56355BDD76D
              A8F58E2D7B10A2EDBDD65981EC3C806B1475147E70367D181F5CF9DB18097689
              C06BAAFA992A53DDBD75D1A6A525FFB8080D4C0B201F652F106DABDF1C535C30
              6F54E7CC85EC1902CB55A904EE72F7D676342D9979DE1DA2F3C02A0C005ABCE6
              2331DF52122F1B8F9190740BB052A11AB8C36DA86DEF0BEE0C105C20D1B05362
              8A03503F0AF03DF00ACA14B7A1B6ED681F716780CA41609811B2090C191953E0
              89AAB789FCF84D4B347CA83CB2EB9BC8BFC101585E4B13409D88CC51B8D81E73
              CB4F2D87EA630654D7A16969C97FAE37DAF7EF00A854F080BB93C7DFF3B71D56
              7FC468DBBD11ED701B51FD1C648E31202D397552597FBB4EC7C4EBC0CA194E60
              F08806C3A00CB083F957567634EE217A786F7FFB3A9BA68E23FB19307EEA6F18
              669A88942AD4275C76C3EEE8EFBFF43BD204F04F1C4782898446147E055C2F50
              6A0482FBEDC2E25D46C8C6DDB7AD47AF7AC18100EECF5B08E65FE5052E1AF211
              C8381129070607F3AFDA943CFE9E88919C8A7FB2F95F5D3EC6223D1A0B09D964
              3CFA0689978E0B0015C03CA00D7807F80874ABEF3A8E7FB2192FDC78BAAE65FD
              1BB4EDFA36FE40000C93D429E5A4DCF40098D648419E02B953045BC1070DA338
              67354D0BC22B177C786A63EC0F19D0DBCD822A6EFD269C6D9518766A389035F4
              6331CD65C046A01EF855441A81035DCF77CEF6AA431D31DCDCCFCE797B4709D9
              245C328E60DEA8D3370BE7169DFC6E2D6E436D3C7CFFFFFC099F6B6FE33D1419
              7B0000000049454E44AE426082}
          end>
      end
      item
        Name = 'command-undo-1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000021049444154388D8D934B4854
              011486BF73EF34633AE3288AE1A387988652A4124241BB72D3A2841665D922C2
              A220491083160546448BDA4404AD2308573DB448F29158112911421198624CD9
              34893A77669C7BE79E16818C498EFFFA9CEFFCE7E71C214DC10B75045A6A7600
              FB921373B77E1E7E422619FF3457032F804A23DB93B179091068DB89BFA5BA0A
              E8031E6B32756EB6EBED9A001EFFF16A02276A2A04E903FA5CDB391DE978A58B
              AFBFAF092065A3C7B60003C0B8A2273566A79CA985FFD55BAEE5C417474244BB
              BFA0511B291B6D1E1431F6AE691CEAAA32AAE80D271CEBFEDD3E8494BC3F5A69
              88D10F8CA9EA19165D57A3C995560BB3042814E410D001DC717EC53B25BF6B37
              3907CAB781F4034FDD85646BF8EC4BB5C723CB090678B71792DB568BAFB66897
              880C801E311383DF304B03116F557E2F704D7C6645F6FE4D3D897733B8E1789A
              7B48CDC4883D9BC4575F14F294F883204D260A89C169CCD240D85B95FF1CB82E
              3E73A3AF6143AFF5E0F3CA18528A3D3987BF692BC0F9BF87E4C2ECE511AC9EAF
              1F8146600259254ACB598AC64CB798189806E5873D35FF66FEF687E52BA429EF
              5203EBCA83ADC06A7356CA28CE26AFAD8EF58D9BEB05194269CE78F09EF25C0A
              AEEEF14BD017308B730A808382740277DD68F25146806479F0D614B4835C4155
              15C68053AE653F8C5C1C26F3CB392EA970ECA622F740E7DDB86325864344EF7F
              2215B2F803FC07CFD8A90415200000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000002C649444154388D95D45F6854
              4714C7F1EF997BF74FCC6677EB9A4A54AC563011DC608D588882BE49110AE2DB
              4AC1944A23018D98B60F0525DAA7350AA2548BFF405010220A5AFA50411125D0
              1A9B6869D51851118B244B9B98DD64EFDE9DE34362D590C4ECEF7166CE8733C3
              61847189B5D45191AA0901DF80A6074FFDE50D1EEA1A7F6CD29877B09D7554A4
              6A82403B22BB41D6984478DAD83B6074E7274452D501E09C887C866AA3C2AF5E
              575F49A00B10694A1249D53822724684CFADB54D082706DA3AC95DEC2D0D8CA4
              AA897D9934C06911365AD566557B74F0C81D86CEDE2F09039079B73719E0A488
              7CA1AADF2ABA5F7D4BA1BB7FEA4A059B2BE0DDED277BB1179B19F91FFC4944B6
              285A407934BA3A8D56940C7013386E47FC9E81A3DD644FDFC3B56ABB8D383A76
              EC77C09729445585D1B79F076C039A25E4FC10DFBE7CAF290FE2023FA2362062
              0E58B58E080D36E71787AF3C9DF88D2201CCEC32828B6782239522B2CF18D3AA
              AAE1E8574BBF7707DA3A89B7AC3808EA1A63D2AAEA4B99DB507CE9D981B6CE49
              3B35893015DB97F545D62FDC0C6618F84E840B4EE1CF0CFAD223B4AAAA03F044
              D801CC0F25675D9248907CC73F135F7DD8277FF519261622B834714D44B6827C
              E000786368B8BEEA06888AC80E604EA836F1B3CD17996AB8F3B75E10D9B0A860
              66046A81D5CEEB0DEF6E063B8A5E071C1169063AFC17B9DE916BCF2605292AEE
              8218C125333F05D6B86FEFBD1EE478CB8A5DAA7A05B8EE3F1E9C1C1B4B685925
              C01C2033E17C94ADFB88C08228C5FE61B2E71F4E8995A7AA89B7D4CD10E409F0
              CB744678E2040C159B9710DD92445CE7B088345A6BEBDDF757BE49F4EB241274
              3055E504577E881B0F27C498B48834A86A1AF8AD2430D6589B50651FA3DFDE5C
              A05E20A0AA7B145A73971F5112088410D60216A51F38A2E83194FBD9F61EFE4B
              DF2A0DF49F0F3D073E560005F58AE4FFE823DBFE80C2DFFF02F00AF12110E43A
              1D18760000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000393494441544889A5956D48DD
              6518C67FF7DF733CE7E8994EE63696274323AD65D17B11D584260E06B9822DB7
              B508B6C22F85D0CB87514104417D8820C22F11C58A08D7A0B1152DF7D29624A2
              496EC18AE6983AE76B0EF51CCF39FEFFFFAB0F4737E75EB4D3F5E5B96F9EFBB9
              AEFBBA3FDC8FB11006CB5F7F80687D25C076E0B83CBF6F78E74FCC748F5E55BE
              189C85E485AFDD4FB4BE026017669F03CD3846CE9AFCFF4C7E954086BC12B0E7
              319A902681570098F1B31208CC755EF0EA7D44EB2B31631BF02930056C146A4F
              7CD743F2685FF602F9BBD6929F99F966B0CF80A47CFF29B05F13FB7B187FB70D
              94153F81687D05850DF700AA33B33D802BB449E244E2F8392EFE0F72008BFDB6
              1D6023B017F040CF481CC220D53E8892DE92889474499F1E67FAD039BC81F815
              02B518FB1061E034D076F9964CF736C7322FBE9C0B7101F845D2513CA5A70E9C
              61E2C32E34358395746E3D68E66CC876040BD02B6937E86BB777929186C3D84D
              1D5BA38EE31C041E033A24ED34B33406862109B34CDBF3E3B95C286C500E5607
              3C6B6661F9FEC782C6D41F63B235AD9B7122C102C3BE37B3477DDF6FC6780E4F
              33139FFC8E3798B86EBB4E6994C0AA08E1C763E4144700AACCAC19A890D80D7A
              DF721F5A4DF147EBB050A0D0CC7E041E04BE0176B87D93DEC84B2D7843D71701
              20D7217FC7ED1436DC8D394ECCCC3A8128E8CE1CEF7C9C74F72891F5B7A4083A
              DF1A3C09D400E55618DA1FA98E69FA481F8ACF5C5FC013335D23B88371224FC4
              26CC61CACCD9044CE700780371D2A74689AC2F4D5AC0D907D498592D50EA1484
              0E84D7C5347DA417C5DD1B1A71FFBA48F0B64202E54567804660D9A55D946A1F
              62ACF1679474C7805A4927CDEC05A029108B12BA77D58DC7348BC92FFF04691C
              1800CAAE5876A9F641461B8FE127DD51A046D229E0EF2531CF4293E94B2160CE
              C2828C9363F8497718F488D0076EFF14A9AEE12509E43D7D2BC072A004381BB8
              5651AA7D8891175BC8AD2C4A00245B07F087A717250F56AD20BAA51233DB6266
              21F97E8B2DFA6A890857C7287AE7619C65A112C33A81024955D774B018026505
              5838404E71849C927CC2B53713BA6B2566CE1D86ED0556026F013D593958FDD5
              0672D7AE8849E4490A9A5999A43A33DB666611494D422F7BFD537E560E0024BE
              00AAE776D3ECD92FE94D497BFC8934636F9C206B01330E4B0C022E701E68156A
              41A4D2DD23FCF3761B5EDF647602EE853896177C0FE67D769E4FFAE418891FCE
              92EA18063F73F32F883985AB9650E6A60000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000445494441545885BD965B6C54
              551486BF75CE30574A3B1D2E6DA1404AADD8048CE5525A9511C180F8C0556322
              26BE1863A2466A7C42ABF0A0312A158D3E7879821023371F8846A7AD09093545
              295223422AA4B60C20B49DCE746E9D39E76C1F86564BD1369D19FEB773B2CEF9
              BFBDB2F75E3F4C52B6AA226C5545932D9FB4F4C914153C538DEF9DFB1779B655
              DAAD583A96FEB53F6700DA4405D3772CA6F0A5FB168AA6B7689AFEF2B4053372
              663E21C0F41D8B296A58560EB400EDCA54AF0DB75FBB33009E1D7753D8505376
              D3FCACB2D453A137DACC444B6FFE015C5B2A28DC595322480B704199D693A1C6
              3623FE4D774ECD016CB7BE706F5C8877D7AA5922D20C742BA5B687DEFD39950F
              73B8A503AE8D0BF1EEAEF7892601E09A426D0D37750CC7BFEACA8B3980FCDBBC
              7877BD577409005160E3E0DED3F1E881F379331F0570AC9B87EFED076668BA1E
              00D20AB521DCD4118DEECFAF3980386A4BF07DE09FAE39F4EF408A95525B485B
              61E36278CA3F55280B45C44A9A09A32B44E24490E1F6AB60DD06A070D70A0AB6
              557D28222F64B190DB4128143D404029F5B971357A2AFC7E07C91F2E8F05706F
              AEC0DBB8AA509000E002B66258516B7078CAE6E27389085E6029B009D8021C57
              A8E7A35F5EB81E7EEFF46837046E5EB70D355E419A812165A9C7428D6DB16C8E
              9E5EE6C1595F8A67FB5DD8AB8A2B81CF800A855A1F3DDC753EFCD64F993A8054
              671F2A964E3AEBCA8E00CF89C866A7BFFC90111C328CAEC12901A8A134E97303
              C48EFE81D9971870AC2C3D20366DA920AFDBAB8B0F1ADD91987131FCCF311CE9
              4451C3B259402B704599D6A681C61F93896FA7DE8911D997CF61E6476B74CDA1
              078001A33FBEFDDAA35F8F1DC7A9CE3EAC8411B7AF9C7D54447B5134D9E0F4CF
              3D9CEA1E34CD4B91AC00CC2B31CC1B71E57AA8FC24B04FDCB6EF8D4B91E0B83C
              903ADB072E3DE6B877D63111D989C85AD7EA794752E7064CF372342B88F48510
              8EBAD201DB1CCF1241AA495BC76F1B4852ED7F211E5BD49E8178556C9ADFFDC8
              FCA3A9DFFAAD6C2154C2C0BD6EFE34E059ECDABEFF1CC791A63344F79F0B020F
              034BC5AE1F9CB9D76F73D4966405903C790594EA146181E6B6B9FE3790849BCE
              30B4FFF75E602DB05C1CFA9BAE75F3B30250D134467F220C220805E3C6F17888
              0E80EE82A7EF5903448C3FB3DB8C3875F4626721999B726842801188445BB01B
              32FB232BFFDA5244D39628458F9534139302C885F188DC8F57026C0602E9AED0
              F844944F39FDF370D5975592990DAB93278213C7F25CC956E5C5BBA74E17E453
              E0B81949B5279A7BEE4C079CFEB978F7D4EB7A81FD0B6091526A45E4E35F5071
              237F00E2D471D496E079A20A575D692599952F52A8F589D6DEEBB143999C9933
              80A25796E1A8995D0A20339DA2FB5C5ED1646C1E506A45BCB5F77A68D7C9D1EF
              7206602B2FC05EED0B8E3C8F4944A807ADC8F0A9C8279DC46E49D83903B022C3
              9837E2736F9A8F66C2745788E4892089E61E54DCC8955DEEF437B114C52A1B3E
              D5BE0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000005C2494441545885CDD77B6C54
              651AC7F1EFF34EE7CCA59D4E9D29D4B65010A314D722521129AD46058D9A788B
              316E973FDCE82226DEAA0D0663A2D964538C349ACDFEB5AB91888AD1788972B3
              0101A982C401625117B32B541098D2DBD076EEE73CFED10261054BD799E22F99
              644E72DE399F79DE73729E077EE791319DED12C27FBF0ECFD565466D757A977D
              416AF3A13CD1462E79EE670AE1967A7CD74D2A1563B68A315157C8BB2FBE667F
              1E795070AEB8504B3DBE055521A00DB84484FEBCCA46323AD025845AE6E35F50
              15043600D381DB35EB6C1D7CE7FB7CFB4601BA84504B1DBE055302C07AE072E0
              6ECD3A9B7A97B593DC74F0FC02837FBB06DF8D530A05D602B3817B34EB6CE85D
              D64E621C70BF0A0C36D752B870AA5F443E06E6AAEA7D38CE9ADE67B78F1BEEAC
              C060732D81C66A2FF021423DCA22E083BED65D24DA3AC70D774660F0A95A028D
              D32DE03DE07A94FB157D27B62242FCEDFC3F14FF1B733A6E36457F9AEE067917
              919B81BF80BE195B116170F5BE71C79D060C34CDA2A8B1BA4090D508B7A1FA30
              B0B27F4584C1B7CE0F0E46B6B8F0FE19041AAB5D08AB44E42EC7B11F03FE95DC
              7488CCBE7E3CB56539B894A223DF9C8134D903C721ED8CBA4A7C3755115EDE60
              809522B24855DB81570144C6F6AA1E95A7248128B0575376776A4F17F1B5FB89
              B7759E152B458B2F27B864E642236603636D1EFE7FAEA3CA1EE075557D351B1D
              1A8AB546486EFA65E321DE9BAA08B7D423224F8AC88BAABA19580CD839ADA02A
              0A5E8149C055C01D88CC45F51868B3C21B836FEF23D6BA0B6C3DB94E000AFF78
              2925CD731061A988B4A8EAEBA00FC4371E74622FEDCA09D084BDB82EF0E09E11
              C2DB3009EBB2100273106915917A557D4D551F8A6FECCCF63DFDF9C9752E80CC
              DE1E9CC134BEBAF2CF19AE5C1350E99E56BC0684445B273A98F94D1FA72B41F6
              C701D2912EE21FFC87E41787715F143CEC2AF7AF0454842740FEE09E167C0FD0
              74A4EB141020D3D1833390C65B57B10D60045966CD0CAF354516A9ED477252C9
              1371BA12C43FFA01B20E9E39655B15E933421388CB336BC2E664FB4F38DDC9D3
              1BD6F429E456C03D820C5B35E1F51270E71C0990DE7D0CBB3B81AFA17227C864
              8147103E3617FA8F26D677FEB2A33E81F4CCBBF05355F51B639A80A0BB26FC09
              852ED23BA2394766BEEBC504DC5835133E43781091EA82C981B7E2EBF69FB9E5
              4F77F4A01E83E78A891B45A458449A50F5BB6BC21B356193F9BA3BE7C854244A
              E1AD17A54CB165212C1658651F4BF49F752649EF8C220137D6CCD236206C8C69
              1231966F5EC5A7CE409A74474F6E85B6A228BEBA8A0322E609E0474DD93B7E75
              684A6D3F8A297263D594AE07CA46EE49F1D4556CD13C20ED8303142D9A711C23
              8D8085C8BBA34E75A9ED473001379E9913D6019523C8ACB7AE7C9BDD9722F34D
              EE909AB2F15D5B896BA27F2130558732FF34A3AE0262ADBB1878F3DF0ABA4455
              578AF0579025BE055539C39D48E6C071148D010138D7B11388B5464070028DD5
              0FAA1205D6608FDE8D8C352660218817488E0908105B1181ACE3786F9CBC4CE3
              59FA5FF82AE7406BD6448069C0C1310301622FEF26F6F2EE1CB386635D538E09
              B803400DB0CEEE4E724EF7E078A5A8F15240EE161137B02EDDD1FDFB017A1A2A
              F0CDAF34223CA9AA7B157624B7FD34F62DCE475C538A093D3F0F117954446A54
              F51EFBE810A948F4FC57B0606A31E17F5C8F29F1CE075A54F523557DFFF82B7B
              C1D6F35B41FF9D1753D25C8BF8DD37C8F01CBE1FF8736A4F17F10FFF3BFC07C6
              1BE52AF7E36D9844E1BD97604D0BFA419E1158AAF09DAADE62F724FA7A97B683
              A3F9059AB097F0F28653C7210FA6C483B9C0630972257007F08020A58ABEA6AA
              8F670F0F0DF53CBA19A72779725DDE8062B9F05E553617580DC34313E045A454
              A0405533C05A479DE5C0CEC4B643F43DB7038DA54FFB9D7C6F713FB06558CCF0
              5CAC1A55E800B6A0DA9BFA2ACAC0AA6F49B59FB95BCFDB1C6C4A3C9434CD3E79
              AC00C355C419C890F9BE8FD49747B0BB12F9228C4F7E064DE981F23BAE774700
              00000049454E44AE426082}
          end>
      end
      item
        Name = 'clipboard-cut'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000002D749444154388D6D914F689B
              751CC63FDFF74D53FF3435C606635AA62B4C5A70AD87420A562F1E2AB5871694
              D64244D1C328193A0B46F1500F1654A4B360B7D64BCBC6E2920C3C1461B2300B
              430F1EEC70A6AE54E76A14D23FB66BDFD024EBFBBE5F2F6D5C369FD38FE7F73C
              0F3F3E3FD8D7E4E424D96CF68D442281D7EBE5FF343E3E4E369B7D2D954A553C
              E3AECCC9F6F6F68E783C7E4F391A8DD2DDDD7D14F8FC4EBF32502C160146810B
              8383830D7D7D7D95505B5B1B2323233E20057C542A952A777270686C6C24954A
              E1F7FBBF021ACAE5F20B4343434E3E9F279D4E130E87CF03B5A552A93F1A8DB2
              B8B8088079306059164B4B4BF4F4F45C340CE32D8FC773A8ABAB2BD3D9D9496B
              6B6B0CE8077AC6C6C6CAF3F3F3951754060072B91CB66DEF4522918C884CD5D7
              D72F373535D5013340EFDCDCDCCD8989892A365503000B0B0BB4B4B46C363737
              FFBA5FBC0264969797BF8DC562D8B65D95BFFB17505592C924AA7A19B0800F55
              F5EB6432C99DF0EE8178A04020403A9D26140A9D071E030A40D1B2AC97060606
              585959A9CA7B006AC247F075BF89E90B1079F210BE60E804F0ACAA76002511F9
              B1E601DFBBCF8DCE7E7AF1EA6F38855B14E6CF71FBC655C4133E42E8830B5EE3
              BEBA4F80574590171F751EECF43BBDB985EF2FFD95FB938EDE57223FED9857BE
              59350BAEE202675C7BEFBDB5CFA2B725F0FAC7D475BD7C12388CEA31155C4126
              BD06EB56F687E1BDAD55FCCFF49FB6954740630AA62053C0EFC59FBF7B4782C7
              BFE4FEA79FDF50E5A9ADC468DEFEE76F82B1E987C53457ECCDFC43CEF63AB587
              DBB6D5751FDF9C7D7FCBDDDDA661F85448447E29DFBCD660A8FC07D3B9B5866B
              6D81EBEE5BD58C9D9D0D9C9D0D50B752F2B8BB16C01911A682C3A78EA963BBE2
              A9F902D573F6464E9DED756A9F389A1091E9E0F1E918608A619E06CE3ABB1646
              E1F259D4DE8B037F2092154FCD756055D113D6A519ACCC2CEA3A6F036B6298D7
              C530AF0137508D5B9919FE05A6DA30EAEC4338270000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000003E849444154388D85947F4CD4
              7518C75FCFF70BC2A90987097AE4A8D6CEAC3497C79A3B9A5B682AE934697343
              732BDC58B746D91FAD68EA566DE994A5ABFE405CFCA16324F4833057C2D616D7
              0F87B541F7056BD55CC271807827C771FCB8FB3EFDC1C554603DFF7D3ECFFBFD
              DAF37C9E671F4885CBE5C2B2AC42CBB2D65EBC781187C3C17C619A26B5B5B558
              96F5A865599EC2C2C2999C71977621D0545050905D5D5D3D2FB0A2A2820D1B36
              2C061A8145B7E7EE06FE01640367376EDC28151515B3605EAF179FCF8788D400
              B929CF6C60381C666868A85F55F7025B81433E9F0FAFD73B23CECBCBE3D8B163
              88C8CB22B24755F78F8D8DF5F7F5F5CDDD8AC7E3A1B3B313CBB2DEB22C2B1108
              044AFC7E3F2E970BD334A9AFAFC7B2ACF5DDDDDD71CBB2DE0B0402ECDCB9F30E
              8679FB21180C323A3A4A5151D10F22B24E440E6666667EE6F178C26EB79BE2E2
              E26C116903BA80F2A6A626ADA9A9B9032873557AF4E851B66FDFBE44442E03E3
              AAEA05E222F205E0019EE8E9E9192C2B2B637272F2FF810E8783FAFA7ADC6EF7
              23C04FC057AA7A5A44DA54B5381A8DFA4B4B4B090683B3BC774F1980783C4E65
              6525232323DDC0011129139175C026C05F555535276C5E20C0C0C000A15008A0
              575513C071402391085D5D5DF3D9E6071E397204B7DB9D079C073A8176E053A7
              D3B9E2C4891398A639A76FE6D658722F8EC79F66C1FD6B28DCBC03DF0B7BCC05
              A63403AE54AB0D4039B0256779FEB9C145F9F675C9263DDF8D1DBB858E8F02A9
              A12C2C7C969CF2E318E919AB812D190646E58353AB8A97D92F4D26B564321E6B
              8D4422E4AEC8F7A41BF2BDFFA6D170F2AFB4402C2936F0AD26133DE1A6E38C5E
              3A83A4AF5CCDF2435F2266DA61440EA17A15B04578CCEB4C7EE7CAD44D571A3E
              A6F7EF3F2979F303421346CBE58851A24A0030447858957714DE1D3A7580B445
              DEDD88696E03DE46F53955BD30FD08B2D91F4E6B36D4DE1DCB5AF3F9D44A27CD
              21B354458A51DDAA4A6B4AB703A409B463B1B7F41BC3589805C85EA051552FDC
              FCE40D06ABF7638FC75A513D6723FBD25D0FB120DF8D22FB50CE6932D93A5CFB
              3A43275FC49E9A68016D14D86BDCE39C99720ED097BC7583D8CFCD4CF4FCC8E4
              B5DF00FAA67333FB9F03F4266EF432D6F135E38176A67A7FFF4FB71485B494F0
              0AF0BC99B5EC70EEC1BA89C4AD4132563D990EEC02DAEC8931EC8931800E6057
              5A6EC1FBCB2A4F4FD9F128190FACCD007630FD37228B9F29C7B9A76AA9C02FC0
              75E0146003AF00AB80F5D1F6C65062388873D7ABCB815F81ABC047A93D7E0DB8
              4FD1F5D1B6B3C346ACFD3C89816BC3C053D340A905EA801BAA5A648FC742232D
              1F32DA5647E2667F48152F300CD421520BFCA3506447C3C3D14B67F817C3D7A3
              BCAA75F79F0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000005134944415448898D946D4C95
              E719C77FFFE7BC3587E3A18C97690624B6413D16DD6CD61799596CB67E684B29
              46BAE0DEED4275539BD0484235D36866D62E23AC59B169B489093A579AACACF5
              F88576A91DCA185B9C471F08B6EB6AC72C874C40E0000738CFB50F5087156CAF
              6FF7735FD7EF9FEB7F5DF703F3A2AEAE0ED7759F745D377AE6CC19F2F3F3F9BC
              0804029C3C7912D77523AEEB56353535DD74EF2C50B31A389A9797A7C6C64602
              81C06D05EAEAEA58B3660DC0E1B9DACF1518073603BBD7AD5B477D7DFDA2F0F2
              F272B66CD982A41AE07BC0C41711380C2424FD02F876757535151515B7242D5F
              BE9C7DFBF6E138CEBD927E0374032FDF56201E8F934EA753C0936636061C078A
              F7EFDF4F2C16BB91170E87696868201C0EE700BF37B31933FB8E998DB5B6B62E
              2ED0DDDDCDA1438730B30FCCECC7402ED0120A85420D0D0D44A35100F6EEDDCB
              8A152B907414B80BF829D0D3DCDC4C5B5BDB4D02BECFB6D4D3D3436E6E2EA5A5
              A59725F9802D9296666767BF158BC5884422D4D4D420E959E099395B7E75FEFC
              79F6ECD9432693B989A75BCC0542A110C78E1D63EDDAB53EE08F921EF13C6F1B
              70D4CC90F40DC771DE36B37F001B070707D35555552493C95B580B0A00141414
              D0D2D2425E5E5E8EA42E6019B0D1CC3E9A3B6799D9D7CDECCAF6EDDB397BF6EC
              829C85B60880818101EAEBEBF13C6F08A8023CE075200A8C023F02AE3435352D
              0A870566303FFAFAFA989C9CA4ACACAC1FB80AFC50D26A33AB002E9C3B778E83
              070F62668B3216EDE0D30806839F024AE63E7D4BD26E80E1E1613CCFBB6DFD6D
              3BD8B06103070E1CC0719C2724FD1670815E49D54077494949F7E8E828894462
              51863E7B54280CC0B2A55FA6E577C7C9B933BB04E8040C58CFACFF9D40D4CCD6
              4FCF647AB6D63CCD85C425006C6A02CCBB55206B4315D1C776E02F280620E233
              B616CF643D529039EB73546A665566D60A20A94CD23B9ED93FDBAF396547AF04
              46925342C0CCB5FF30FADE6B8CC60F83D9EC0CA215BBC8DDFA3C8182E2F582B8
              E07ACA73464E25FD0977D459F3DF34CF0F4ED1FA97CE4E8E1C39C2D014E7FA27
              EDD94B234EECB5AB810B03D31A110C03715FEE571EBCB3B2969C9FFC1AE4E00F
              14AE22FBF15D8036239DC02C091CC3CCFB685C15CFF504B93BEC7D90E5333A4E
              B471FDCA65120FFC0C4FBED4C713226378C0ABCC2E4CA5A4770DBE1B79B0F20F
              135D71142DDF41F6A6DA1CA1F799FD23969BD9C8AC156481DE9078C0CC56A6BA
              4EF7CF7CF221D1C7772E751CF59AD189D92683D49C7551200EC480925447EB90
              E384A3083D0AE400B5369D1E197C7537C917AAC98C5D4F01BBCC88809E0816AE
              2450BC1A49956644CCD8653353A96BAF3CC340C30FC88C0D8F00B573AC47155E
              72E31D140198D9A5F1F36F93EA6865EAFDBF71FDD44B98592FD20C6645F376A2
              08691AAC77ACFD75C6BB4E93EEE960F44FCD98D9C5B99C42F8FF43EB9B5BAA7B
              EE28FD26C1BBBE862FAF88250F7D1FD04A4100A96FDE3EFF5B1044AC08DFF718
              C1E56BF1E7171129DB8444E90DA681A295B56497EFF892A4CBF36760E971140A
              67497A036667309178B77F26F92F963CFCD432895EA0C3CC3691991E07903F10
              05C5819899958C779D1E7226DD768041601BE87EE0A2A4179D3BB21A2525808D
              C04E89FEC94B7F66C26D07EC136027F090A48BF2071BE50FBE08BA88741FD836
              6068C26DC79719BC8A3FBF8840E1AA1E897780554005D2BD4002B3A70DDE9CFA
              D865A8F9E764921F122C5C897FD9DD1704EF21DD83A800BE0AFCDDE0298CB6F4
              E5BF32DCF24BFE0720A817D19EE7E1430000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000059C494441545885AD976D4C95
              E719C77FD7E1BCEE1C108403392BB0B6C68CA139ED14AD32196D60D9443F0026
              66C69226AD51525AE54CB159082C8CAE8B6BB30D5D9A754E5B8CCBA6A9332267
              6C84CC31B625A5C1E823B7C1D4C0D83A741E05E5FD705EEE7DB062E4CD83ECFF
              E1F9F05CD7FF7FFD9FEBBEAFFB791E58007BF6ECA1A1A18184848485D2E684D9
              6CE6D0A1431C3C7870C13CD34241AFD74B616161715555D5A20DECDAB58BAD5B
              B76EC9C9C97972035FE048494949C2FAF5EB632E9E9191C1EEDDBBBF041C795C
              6E2C06D244E49DBABA3AAC566B4C066A6B6BB1D96CB540FA920C04834180C322
              529E9999B9B1BCBCFCB1C58B8A8AC8CDCD5D0DF8805F7CA1F164C8CECEC6300C
              A752AA572975E5D2A54B96952B57CE9BEF72B9686F6F17A5548752AABFBBBBDB
              959F9FBF608D053B70F5EA554E9C3831A6B57E5D44B22D16CB5BF5F5F5984C73
              D37C3E1F292929AF89C83780BD6D6D6DA3EDEDED8F7FD28560B7DB696D6D4529
              75522935AE94FAEACE9D3B67E579BD5E0CC3702BA56E2BA57EDFD9D989DBED5E
              5AF107C8CBCB432995AA940A28A52E7476768AC7E3998E9B4C26CE9C398352AA
              5129754F2995BE63C78E98B46399023A3A3AF0FBFDB7802A11C9773A9DAF5557
              574FC7CBCACAC8CACA7A49445E066A0CC3F8FCD4A95331199098B280A4A4249A
              9B9B494C4C6C03BE0EACAAACACBC6918067EBFDFE670382E0123914864E3F6ED
              DB233D3D3D31E9C6C56A60727292C1C1410A0A0AFE21226F02CFAE59B3E663AF
              D7CB8A152BAA8162A0B8B1B171A0A9A92956D9D80D00F4F4F4B076EDDAC1F4F4
              F4A888BCE9743ABB323333C781D3C0FB0303031FF97C3EC2E170CC9A312FC103
              64646470EEDC398BCD66FB1448D45A3F27221F69ADCB2A2A2A163D768BEA00C0
              F0F030D16834BA61C3868B22520938B5D6AFB6B4B44C1D3D7A74B172B14DC14C
              8C8C8C0098B4D6001522B2AEAFAFEF49A416BF046EB71BBFDF6F713A9D5D401A
              300C8C8642A175A5A5A5E1DEDEDE45E92DBA03353535389DCE2A11590D1C00CA
              45C46BB158F6D7D5D5CD7B4CCF8745ED818282022A2A2A5602BF05DA2726260E
              040281BEF8F8F86780D73D1ECFE9DBB76F0F2AA562D68C79095C2E17E7CF9F27
              3535F5CFC00BC0730D0D0DD72F5FBECCB163C79245440157C6C6C6BEB565CB16
              0281404CBAE639EF5AED38379662FF5A2E62B122C00BCFA6B22C25F555117951
              6B5D7DEDDAB5EBC78F1F271C0E73F6ECD93BA5A5A5DF1391935687F3956FD737
              36FE517D8E003A122678BD8BD18ED3E88991C777C09CF60CEE7DBFC69CF674B1
              4025900388D544976F45287BD3F2C840341259FB4AD9CB21C33000484848A0A9
              F90F2C4B4A6AF964282EE767BD163511B9CF03BA80C3917B818F03477633D567
              CCDF01B1DA71571EC392FA95F7803DC061E007809E8A5278E833CBF3675D6623
              323112EAEEBF39CD1B999CE28DCE20C94FD9FED9372E9B80BF3FE00185C0F1B8
              65EE6FBA2B3FDC7BA37633D17BB7A6B98F6C4257FE77716D2CDE06D46B7831F4
              EF9EDFDCF9F0ADFED1BF9EEA37273F75C19C92EEBF33253F1ED2B67F69CD95E0
              D5BF4DF34CEB4BB6DD0D49CD7C3CE0E726ABFD111ECC18437B562EDCFF96FB69
              A85F5DBCF976099357FE42F0DA27DC7AAF8C09E38201FC04F039566D5A326F96
              81B8640F88E400AD77CFBC0BE1A987411D65E8773F02F81322CF9BE293CC4BE5
              CD32807E78D5E1D95FB33A1202ADE591ECA5F0661A88DCFD2F68FD29B07959C9
              7E303FFA1F90B8ED00887C07AD2F46C7EE4596CA83199B302E310DC7EABC21E0
              5D73F297DB6C591BFE13BAD18B385C2C2F7B1BE7BAA235C0AF80EF8F7736ABC9
              EEF625F160C639208E783C3F6CC1BCDCF30EB00F781F68E5E138BD017C109D18
              DD7FA3763391C18125F1661900B03CED2575DF51E212528A78701089085ADF3F
              50C6879BEEFC722F93AAE3FFC29BF35D208E78E25FDA897D551E26BB0B04A2C1
              71829F75317AE12491A19B73D19E88F73F0EA297E25DABB7AD0000000049454E
              44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000008FE494441545885BD987B70D4
              D515C73FDFDD64972424248009288F26951668A104313C022254EA9002D19212
              C1DA8AA0E20BA1600942892C8F06452216143A30E00C82E064045119A823E02B
              A8A09412A580D08457786503913C77F7F48F04246EC03C989EBF76E79EF33DDF
              7BEE39E79EDF75520F9932650A292929141414E0F57AEB63724D898E8E66E6CC
              990C1E3C980F3EF8A049585764DBB66DECDFBFBFDBDAB56B713A9D4DC29A376F
              1E7979799DF3F2F2EAA5EFA82FB0A4E5DDBB77778C1A35AAD1E47AF5EA456A6A
              2AC0ABF5B5A917414948EA23E9F1C99327131717D760722E978BCCCC4C24FD49
              D2C01B4AD0CC30B34A606E444444BB1933663498E0D8B163898F8F6F053C5F83
              75E308D6C8424951C092C183073364C8907A1BB66FDF9E471F7D14490B24DD04
              2C32B31B4770FDFAF598D95C333B286984A4B48C8C0C222323EBE564D6AC59B8
              DDEE01C0836676049893939373E308AE5EBD9A03070E940313000316C7C5C545
              4F9A34E9476D870E1D4ADFBE7D438157250978A2A8A8A86CE1C285378EA0DFEF
              C7E3F1E0F3F97698D92A496D2565A5A7A7939898784DBBC8C848323232903455
              5257335B6F665B172C58404949C98D2308B06FDF3EDE78E30D8067CCECB4A4F1
              9206783C1E5C2E579D3693264DA255AB56F1C00C33F3029373737379E79D77EA
              EBB64145C2E2C58B292C2CF402936B6C97C5C7C7BBC78F1F1FA4DBBD7B77468D
              1A85A4A592C281E9E5E5E5A73D1E4F435CD2A06BA1AAAA8AFCFC7C525252F603
              B74BEA0704121313776CDDBA95E2E2E26A50A793254B96101B1BFB7B49D3CDEC
              53337BF295575E61FBF6ED0D22D8A00802ECDCB9936DDBB661664F98D977C0B4
              D0D0D02EB367CFBEA2F3C0030FD0B973E72820BBA6E74D387AF4A8AD5CB9B2A1
              EE1A16C1CBB27BF76ED2D2D22EB8DDEE3249BF057AB46DDB76F5D9B367397FFE
              3CD9D9D9B85CAE1724FD1A78C1CCD64D9C389193274FFE7F08969595E1F57A19
              3468D017408AA43E4061CF9E3DF774EBD68D8484845E929699D97F81FB727272
              7CEBD6AD6B8C2BD428AB1A59B56A154949493D80CF8052E0173515FE99A44433
              4B292A2ADA9A929252EFB6F24369700E5E2D1E8F878A8A8ABD40B6A416C0CB92
              FCC085C6F4BCBAA449C35D7171310E8783A4A4A44F80FB2425037B81D7CC6C53
              6E6EEEA5175F7CB1292E9A76C450DD52366FDE4CC78E1D4748DA6866C7CDEC97
              5EAFF7E29831633876EC5893F09B74C40037DF7CF3E5F9702880A47692E64745
              45111313D354F8A64770C58A15F4E9D3A7BFC3E1D86166A5C045496DCC6CC0C1
              830773D3D2D2F0FBFD8DC66F5204535353E9DBB7AF4BD2F21AAC4CE0C99ADFCB
              3B75EA143A6EDCB8A6B8687C91444747B374E952C2C2C2664A4A33B32F818781
              6F806E35637D656262E247575F830D95464770DAB469C4C4C4740132001FF048
              7E7EBEFFBDF7DEC3CC269AD9056086CBE5FA5966666663DD348E60727232C387
              0F17B05C92DBCCFE6E665F7A3C1EE6CD9B477171F14960BAA466929627252569
              E4C8918D221872BD45854512DE6B28AE841EC8F9BDEABDC36EC7D0C30EA9BF99
              E503B3366DDAC4AE5DBB00C8CACA222B2B6B3970BFA48101B387521F7B66E58E
              1649D8E5BA34A8CCFF37A5BBB710B878EEDA1CAEB510DE7B0431F73F87233CAA
              A3A43F00BD80E640FEAF5A043E9EDBB96A51A8831601B311C55EEFBBC3860DAB
              9567CB962DA3FF80015D057BFC46E99C83A14F7FEE750E34E848F5B5F825B0C6
              AA2A8E146F5A4CC996E575F2A8B3489A0F1C4DCBB10B70B89ACD94F4A6C460AA
              F3CC07F43D53E918F34991C31DEB0EBC5D58AE39AFBEFC12FBF77C5E0B63EF81
              C3B41F32FA6C41992366E1B7AE417925CEDF1974002E00714823253D81C3E90A
              EB9ABC43CDC229CFFBF8C7231812DB91B69E2D28D4FDBCA4A966B606986ED809
              2B2F45EE70491A012C718AB008A7F53BF39FBD07CFCC4FAB85D3F2C1F9B41C98
              1EEF337203460078CAB0B7ACBC3420773892DA0273243D64664BCDECA933D963
              A9C8FBA8164E509144DEF5200A75F79734A53AF9F963E9FE0F4F9C9AF11B8E3F
              95C8B9A513CC57746A13D0DF6FF82EFAB4D29DD003D7ADB77D0F1AD59A887E23
              A90CF08F8011626677F8CB2FE514AD7E3670FCE95E9C9C9A4CC987EB4F99D978
              337B41D2E3A0BBA2EE7E38288241049B754D0698646685984DBBB46B23E7B2C7
              E22B3C02013F655FBD4FE19C7BF0159DCA0766494A96945463578DD1B937723A
              BBD50CAC1EAB283D7C262B9D4B1F6D005F25FEE2D3785F7B960B6F2DC2CC669A
              5981C4E4669D7B83A376D6051174B6BA05A43B81B7036525E5DED79F0BDA55E0
              E239BC1BFE86C106AABF930786B4BEA53606BAB3E6EF8692AD2BA83AF64D10CE
              C52DCBA93A71A80AD88874074EA79C316DAE4F90905087200638517168375656
              F72C57BE6F3B98151B560AB4BE3A9D15E206AC35E0070ACBFE758D77C0809FF2
              FD3B014E082240613F5409261808040C2B02DAB9127A4048DDDFBCEE2EFD005A
              82C281B357AF59C007E82CD55DA2ADFBE7BDEB26F83D4E3B83EFC0CA7E94A0FF
              EC3130B603A9CEC8966131A3670681CA1D4ECCA80C24A5AB3A74DBFDDEC2DA18
              B09DEAE3BF2F6AF89384B44908C2693E7034AE0E5D5DC0BD98ED20E037FF855A
              7B0D2658716837563DC2DF042C6A3E700C2DC72DC411D51A00577C0FE266E410
              1AF7939F021E33DB09EC293FB0EB87187966B60DF8AB233CAA73EC33AF13D6F3
              EE2B1B6C71CF9F89B9FFB9CB2F5EED80EC8A6FF782AFF6CB5C501F0C6DDF8536
              7FDD889C2173253D6B666F1A6498DF7734E02DC4D9EA16A7A491C0E29A0DF6AD
              2CF8FA48E1ECE1B5705A3DB684F0DB867690C80542319B6C68BDFFE2599FC31D
              8EDC111D24E64B1A6366D96636E5DCD209947DF5FEF50902440E9F4874EA4424
              4D05CD96709BD961E02270ABA46833FBCA60B4955F3A783A2B3DA84A1D51AD69
              93B99990E8D80460ADA4A49A09E730D01CA993A0CACCE618CCBF94BB91A21553
              82B8D479D5551EFC0CAA2A7077EAF5A99CCED5C069C08154097C82D91CE02FFE
              F327CE9D79693C5505C10FE256514AD9DEF771DDDAD31B121DB712F81C280385
              201DC76C8D993D22F16EC93F57E35D9309753C6A5E77E47744B5A6F91DE9B813
              7A2077C4156D7FD129CAF23EA6F48B778372A62E69D67D10E189430889ED08AA
              06B1CA722A0BBEE6D2AE4DF84E1EBAA6EDFF000F99BE9165A151790000000049
              454E44AE426082}
          end>
      end
      item
        Name = 'clipboard-copy'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000011C49444154388DAD92318A83
              4018859F412C3611B4B411ECD25AD8CD2E886DB6B0F018B61EC1420B0BB1133D
              4476FB0D7805ED3C8158A6D081CC1621B21A93DD817DE5FB876FFE6F18A1699A
              0B56C2183B0378AFAAEACBF77D8CE3B8760C42D334972008D872104591300CC3
              5992A4439224A73CCF5701E244120401005455DD589685AEEBA0280A00C0755D
              98A60900E8BA0E61184E1B894BE27EBF87E338332DC3306018C6A4A569DAA475
              0710C56BF5406B3B0CC391107248D3F4E4FB3E36AB624F5296E50BA5F4480879
              4BD3941FD0B62D2B8A624B29FD2084BCDE29FC96388E6F97EE18639F5C80E5BB
              4451B4E35658E6FF01B22C730144E0FA6D6F45DFF77C80BAAE675BE8BACE07F0
              3C6F56645906DBB6FF0E7834F8A9F52CDFCC226CF5D991FCA60000000049454E
              44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000001A149444154388DD5943F8B1A
              4118C69F77F7ADC422E2142258B86E9F665AF30762AF9FC0DC21CC7E00B54F5A
              07AEBD5C926FA0102C776D02560ACB555B6E236CA527692C449C3467D8E82946
              AB3CDD3CEFCB6FDE3FC35014455B9C9031E611402D0CC385520AABD5EA543A28
              8AA2ED70383449921C04A5949052D233F4C3783C7E524A9D043200244982388E
              D3FE276646B95CC672B944369B0533BF715DF747B158FC93B45EAF319FCF0F81
              440422A29D2984F8DC68345E0B215E6532195896050028140A6F47A3D16E14BF
              369BCD63BBDD4610047F03F795CFE7E1BAEE1D8077C75A23A22D337B5AEB6F9D
              4E07BEEF1F07DAB60D000882C0A46FDFC9711C789E671963EE99D9F47ABDEF00
              E0FB3EAC63159CA3C1604000BE30F3ADD61AB55AED3AE0643231FD7E9F003CD8
              B67DA3B5BE0E58A95468B158200C4322A20766FEF8E20CCF95E779943ADA44F4
              F522601CC7E876BB26ED3D2FCABEAAE597F49F028510170319009452E96D6136
              9B5D057CBF6F964AA5CB81CD66F3E7BED96AB550AD562F034EA7D303B35EAF03
              0072B91C1CC7390BB4FB274F3E6C29254929FFA9C2DFFDDB8679EB261DC90000
              000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000022A494441544889B555BB8EDA
              40143DD7581408197F00502088B0FF80C20A856D893FA0443C2A8A6C9665E922
              256DAA6D23F317742E2828E98D84709F16BB02096E8A186708385E96CD693C8F
              E3FB3877662E799E770400666622A2A43133BF30F3E36AB5E27EBF8FED768BD7
              803CCF3BFABECFBEEF23B209663E1BDBB64D91AF170023CFF3B8DBED220CC354
              073233B3EFFB705D9793489665210802288AF209C051D3B471BD5EE7E57299EE
              807E83A36F2C8B244919555551ABD5B05EAF512E97B1DFEF91CD66414468B7DB
              68369BB1A1C3E180E9747A219D9CE45955550C06838F8AA234254982244967FB
              AD560BCC71D23F99F947A3D1B8A88FCC024B1CE77239E4F3F9A62CCB5FAE0520
              50010044F441D7F527C771B8D7EB2108823F1201E088244A14177A3299C4019C
              38A7B96DDB64591631F30300E8BAFE349D4EE34CCEF3BE038BC502001E007CD7
              759D1CC741A150489648441A879979369B3100320CE33380A3AEEBCF8EE3244B
              241AB87601455E741179369B01000CC31801204DD39E134FD1ADB02C8B8808BB
              DD0E61184251944700FC6E125996F5F71A008CEE96C8755DB8AE7BF17ED9B64D
              A669BEDF294AC27F7770770DD2FEBFBB06493DE4C4910423A241148BC5B3B5B7
              426666364D13A669E29409006C361B6432993852316AD1C06B24FA1A6D9E75B1
              4AA512CFEF91489ECFE7DFAEA556AD56512A952E9EE55B210F87C3AB1BE3F118
              9D4E278E548C5AE4A54AF42FEF442436FD9B50A954D21D9C028A0EC09BF00BCC
              7D954A4B4B09710000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000197494441545885ED96B16E82
              501885CF35370E2A4D8C9B83CAC6C2E0EC6D0D0F50A22FE0ECA0AB4F604CBC8E
              C415DFA1DAB97D094731BA1A27D34188D2C1D8A01605141213CE04E43FE403BE
              1B2E69369B90651941B25AADD0EBF5609A66A03E00505996A128CA3E40F70780
              9ACFE7BF5AAD5660087A3CE8743AB69F22E73CBDDD6E3F2A95CABBA669DFED76
              3B1044C277C3115DD753A6698E1963554DD3904C26A30598CFE747880963ACDA
              68347CDF833A4F0821C46D309BCD26244902A514822060B158405555EC763B6C
              361BE47239D4EB7594CBE58BEE3559E9C515974892845AAD76226BB1583C9911
              4511A2289E57AFCAEA1980D2C3E8A365BDCB01AFB9266B24008661D84E599D10
              9100008715331A8D5296658D19636F4788C80000C0300CE8BA9EB62C6BC2187B
              ED76BBDE25BC2783C1E0FC4133B66D7F964AA597D001DC560DE73C430889F613
              FC9718200688016280E7011004211480BF7F01E7DC753F0800EBF53A1C80E572
              89E9747AF34D140A857000FAFDBEA7C1E1700845511E0EF03C128615DF1B925B
              B28606E055563F99CD66F805CC31AB9EA2C072B50000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000002BF494441545885ED97BD4FDB
              4018877F674E801311294E2616AA58828D2617462450D5B09378ACADB64B3277
              62ABD82043D7623B1D32F31F2425A26BA6B66260496807CCD22A2CD81252B80E
              C1111F0D496C87209467F172EF4F8FDEFBF01DC9E572608C21084E4E4E502A95
              02C972A18C316C6E6E7E0290F413C439FF09E0433C1EBFDAD9D909C60E00BDFE
              261B8DC65AB3D9F41492482420CBF23AE73CAAAAEA3B008149BA8268369BA856
              AB9E4232990CA2D1289F999979130A85B8AAAAEF39E757BBBBBBBE0505DF09D7
              B45A2D98A609C77154005F344D13B6B6B67CE7062608009665C1300C388EA301
              28A9AA2A6C6C6CF8CAA4FD87DCA33C3D3D5D164511822080520A4A292E2E2EB0
              B0B08076BB8DA3A323A45229504A23ABABABE7A150A86758A552816DDB830912
              42483F3B42C8EF6C36FB6B6969E9AD20742680520A4208161717C139EF4A1342
              D6154581A228F77238E757003EE772B93FF97CBEA7E4D01D248420168BBD0887
              C31F1B8D061FB6DE4596650220CB187BADEBFADF5E925EA6186EE7745DF7EA87
              62B108DBB6974551FCFA9064A09B6458F6F7F7717A7ABA0CA0CA188BE9BA8EBB
              EB75AC828EE3C0344D5896F51240853126E9BA0E5114BB63C62A0874240DC380
              655949743A291986D1EDE4D805815B9D4CA2D3C9A83BDD4F4210006CDB86699A
              383B3B4BA1D3C9E8F6F6B6B75D1C148542A1D7B9CB38E7663C1E57C626B8B7B7
              D7F30C5D5959413A9D960821E3EBE043573B5996E1FED49ECC1AECC544D02F13
              41BF4C04FD3211F4CB44D02FCF4F70767616535353A370F92FDDDB4C3A9D4622
              91E85B60DB362291C848A56EE20A962549FA264952DF8276BB7DE83E3B1F037A
              707000CBB2CA8316CCCDCD41D3B4B5514ADD84D66A35D46AB5810BE6E7E7A169
              DA08956EF3FC76F163E3EB4D92C96482F2B8C5CDD3C4ABE03980C351095EF31D
              F020787979897ABDFE03C0ABC095EE707C7C8C7F62B2E7F4F9D2EA2700000000
              49454E44AE426082}
          end>
      end
      item
        Name = 'clipboard-paste'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000019949444154388D9D9331481B
              511C87BF771C113515D1542153A09468E9628748DBA345D07488BA045C0B9902
              9AB117EAE2D6C0A183640ED93A852E090D9480E8A22E4D33B49B93724B225952
              E4EEE49E83355CE82546BFEDF1E37DFC7F7FDE13FC4351144AA512D1687413D8
              061460B7D56AED2512093A9D0E7E886FFB1F71C428F6D83CAF5E2FBF6DB7DB47
              854201D77549A5528442A10FBF7F1DD7DCCB3A01D9E1ECFC924FBB95AE404DBC
              9F7701CE022B00341A0DD96C3601A8D7EBC4E3F11F2F17DEF0CC6E4940FCFC73
              A1782750000E4EB72580AEEBB25AAD76C35AAD86AEEB12E0E0E4B3F4ABA07A0F
              866108DFA203E80AB2D96C4F904C2695582C06C0B5D564762E87E94CF124F29C
              9D9D6972B91CB66DF74EE0C5344D0017401D798A3AB3CE1520A4FCBBB131B71A
              0E870F33994C7F81AADE46773BF0D41CB72CABAC69DA5A3E9F3F547C6FDF43B1
              581C771CA7AC69DABBBE130C229D4E0B2028A5AC3C58E0AD641846F05115BCF4
              158C8D584309BA153657BFF704819917C30B9616BFFCF7024D678AAB2104E2F8
              EB966F301159424CDE7EB441DC00A9CB89078ED361AD0000000049454E44AE42
              6082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000025F49444154388DA591CF4B93
              6100C73FCFEBF6DAF25D69B9D0B543BA26A33AE8EB42A26817BDB6A891A7A00E
              F5FA1FD44DBC48AC9B8760D0A1E898A730124798B563030F0B626CE2C4BD10A5
              2BACB95FBE4F0771385D33F17B7A1EBE5F3E3CDFEF23D825A7D3C9CCCC0C9D9D
              9D5781A7C025E02BF0A8582CCE8F8E8E924EA7692671F7C6201241058D8B8341
              6E87EF7881C54422713C994CE2F7FB191A1A2A0197E39F3E7E79FFEE35AADC40
              6001F0EA4DA21E585A7C322E11E495F3FC6CF161D1722597CB8D4C4D4DD54263
              6363F4F4F42C08AC85F6AD8CECB05242D9064EB4F63FAE03DA80F1DF7F72B2D8
              EAA2202A986696542A55174A26934829836EB73BE890EBFC29ADE26C730B6062
              6F651B403AFB16D961F1A3EA221A8DEEDB251E8F138FC7310C83B22DC1B7FC2C
              03171E34DCD0B6FBE2F57A4524126918DCD1AF2CB2995F037E4E2448CFD6BFCE
              E3F1BC0806832F354D03405A2514FB29EC9A9F8D72055BEB1942A110B1588C42
              A1B01B28C8E7F3626969A90EB8B2B292EDEBEB5BF6F97CF7B6632A5AD72D9021
              BE2B36242DD72727AF3D0B87C36B8661502814EA2BEF956559A8AA7A0E18CF64
              32FBAA7ABD5E819437755D1F8946A3EB8661340702288A02D0F0B32291089B9B
              9BFD0E8723B603550E021EA4E9E9694CD3EC07E6745DEF383270787818210442
              081D881D58B99962B1586D57D33465201018382AB076EEEDED251008882357DE
              ABA640D5564551AC43016B95FD9E55CE9E5EAB33CB550DED58F9F0C02E974EFB
              89FC3E734B0A68530F0DFCD0EDD21B9A95ADEAB2693F4995EDD19BC9ED760320
              E69E3FFC67C8AE3AE81EBC1FB4843AFFBF2FFC0B1C84D8644406A83D00000000
              49454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000349494441544889AD95BB6F5B
              7514C73FE75EC7AE711CBBB2108FA0925CB75431280A03951858AA9B3B7461C8
              D635C99604545090AA4A854AB48281892112CEC2963F80255622555104991A65
              2048C171249C540E8F1447E451DBBFC3605FE327B815DFE9FC7EF73CBFE79CDF
              155A303434C4D2D2129148E46511F91CB80158C032705B557F999B9B637575B5
              D5B42302FD2F04015004B0989E9E261289C444E461A954BABCB5B5853186D1D1
              D19BA150E83D1179677676F6D78D1FBEAF5999BAB393B312C6685300397BF400
              104EAD047F5AC3FC252F81589FA8EAFD85850572B99C020C0E0ECACCCC0CB66D
              3F40F54E587F276672444C016A41DEBDF9358FB6F79B02582262443061F35BE5
              C47ED588651BE07EA150606F6FAF9A85881C1C1C90CFE71191DB6259E6CC7ED1
              44CCE38A881A11A974A508E0A8B8AB4F8ABBE0BC4F369BD56C36CBD1D1519BF2
              FAFA3A3B3B3BEA380EC96452F6F657880F387A71C0E9DE0355D527C55D72F915
              BD34AC64B35932994C1391AAAA009B9B9B00789E278EE3682EBFA2C3AF89C4A3
              C3DD03888800EA53E1791EE3E3E3D4EE5155ED24D74C7C3B6A3974A6080410C9
              64321D95C6C6C63E4D2412F8BE5595F2D93E17A22328F0B474425F5F8470384C
              2010A05C2E3753E41F5AA8A9CB878787F7262626088542F54C2D3BCCC5CB7731
              52A6104C801DE59B6F6FF0E3F64F4C4E4E522C16AB7AFF945CA5C83F37CA8542
              01E02E50A94E9D183B98307DFD6F55A4FF6DF3B4EF9239977805B1BE4AA552B2
              B8B8482C166BA4E8DF110C06B12CABAD4A55454414C0755D44E40355D5919191
              8FD3E9B44E4D4D3553D44D6EC4F2F272DBBD88E0BAAE148B45A2D1E887226252
              A9D47C3A9DD69E286A7126AD7A54A7848D8D0DD6D6D6006E015FA65229B17AA1
              A857388EC3F9F939C7C7C788C82DE08B9E7AD02B92C9A42493499F5E013E7AE6
              1EB4DEFBE7F9F97968186DCFF3705DB77D937DA346B9D161635FBA6D79A3CDFF
              DA834EE889A2505F09BFFA6E1475FA062D8FDDB5AB3B7EF97E93003496B0B1AA
              FBF4CC14D5A74844B8F6C6CF1DCBBC107F13EB39C90CA8AA0E0D5E6768F0BA9F
              793D415F787CFC07A762A33C1F459FD53E363DC78DF2C0C0554E6B3BE9795EDB
              767782E354FF7281EF1E6EDFFB2FE557AEC489BD5E0DE0BA6E2FFEEBF81B2698
              A3B62FB70F630000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000285494441545885ED97C16BD3
              501CC7BFAFD6CED5C462293BAC54262DAB8391D3A8976E122B7868EDF62FECE0
              4977B4C5FD01B6A6DE66D8A1B4B0AB08A22EBBF450081ED48387C2F316029BEC
              3008C2104613E8F3501BBA5592ACAB55A15F08845F7EBFF7FBBCDFEFBDC70BE0
              A27C3E8F56AB15A194EE504ABFFF7A7628A591F5F575B7705711A78F814000AA
              AA5EE679FEB3AAAA42B3D9040088A288959595966559A94C266319863134807F
              F3E15D2C2DC66C8345A671ECBB099370E0791E3CCFAF6A9A26288A62FB288A82
              68342AC4E3F15559965F1B86013F3B41A8A323C07ED87EDA8181272F769D0196
              1663C8DE59E8F41BB5C083534EBAAEB3B381BAAE239148BC1204C1B6C5CD770C
              7D55FDF2F59BCFB502BD97E6A74D3BC98DE53C29140A0349FBD56834D068346C
              1F499248F3E35306D2CD2FDE7EE6D8DE9E5C09FFB4FC670DF26E963C5FEECEE8
              BC83BDDCCD92C739858110CFB1030000502C16CF9B1B0040BA891D5B7756FF5A
              0BBC952E1C0EFB92C924FCFED3E1C1A93658388648E8120EAD30F8B979C8F202
              00E0E8E808A55209A6693A0178533299C4DADA5AC7C9E704DDF28AE22D30C68E
              01E46667673F6C6C6C9C82180AA03773B7ADDA932449D74CD3DC4BA7D3B9ADAD
              2DB51F626C6BA05EAF5F354D5349A7D3E2F6F636388E1B2F80A669AC5EAF07DB
              EDF6FB542A75AF5AAD82E3B8F1EE025DD751ABD5A62DCB7A2B08C2FD72B93CDC
              1A1846954AA57FB2D38CB137333333C1B100FC6EB14A92748510F2F70FA209C0
              046002F07F0204A7DA23031838091FE5F65C83D8F598AB8F570D558148687427
              B83D92D77B3C001C5A619C8C0A403B303CFDC1F48B9F9B1FD9EA3DF7DD1F0064
              5986288A8E77422FA294FA866AE6FEFE3E28A5172E82A669F8097DD7DE1FF48B
              4F920000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000003E4494441545885ED985D4C5B
              6518809FEFB4F48782FD65711D21EE086823228D1D46B309811C345C2C78E1AD
              72A17061D2B85B2F4CB38B25C40BF5B24AA48B97D49848823F64C9C84202EDC0
              C22421D50801AD1A3A4BE380AD5B7BBC1894AEABA17F0B6A786E4EF27EDF79F3
              9CEF7DBF73723E4191343737130804B05AAD1EC0079CDB1BBA06F8B6B7B7AF7B
              BD5E6667678B4D5914A2D889C1601097CBF52AF0E5D2D2922E128900D0D1D141
              7B7B7B0A786D7373F39BBEBE3E52A954D504B5764B2D75B5FA07822A820C5A32
              6801686A6AC2E572E981D1E9E969DDE4E46476EEF2F232894442D7D5D535DAD0
              D0F0647F7FFF9D50280480441A89BB08D407F2DFBE73973F6EDE2A4A507C7AF1
              75DE38FFFC7380653F780F1D3735CF704B3A054242555580F6743AFD91CFE77B
              6885743A1D3E9F0F8D46F32EB024840055A536F31B8EF43235ECE64EDF9ABEFE
              F362DF5B9F1425A8DDBB7E08741F0453DC134610D2FDA710F73B21994CAA85CA
              974AA5482693D8EDF68F0F1E5D90113AB586DDFC36BA0AF414659723C8EA2F57
              D4B55FAF64074E3C3BC6B5F0F74C4D4D15956864640438A8A5A2289C3B63267A
              E3BD6CEC8953BD9C6EEC2DD60D00A9A4D94780B65030146DA1ABD9882CDB5014
              A5ACC4B22CF3D7EE2EA1680B9D2D5135DB27D5100C475BC5E34F6D71F2A41159
              96CB120458F929A186A3ADA2B325AA1E3EBB0441808989897273E652D6AAE5F2
              DFECC112B960341A237ABDBEE0A0D9B4C356EA691ACCBB64346676D2DAADFA06
              99818181FDF72B0B0B0B6C6C6C1423587A231B8DC6C8D0D010168BA5BBD03E10
              42452BA59124508586DFA51ACCCD9A814B97CEEF0B7E118FC77F181C1C647575
              F530C1D2D1EBF5FB72EFC762B192EE753A9D180C86771C0E475F2010881492AC
              46891142108BC5F0FBFD25DD373C3C8CD3E9B41B0C862987C3F14A201058C897
              3CF24D323333C3FCFCBC0D9872381C9EB1B131DADADAB2E3472E98C964181F1F
              271C0E5B80EFEC76FB0BA3A3A3B8DD6EE05F2008A0AA2AC16090B9B939B32449
              DFD6D5D5BDE4F7FB71BBDD472F68B3D99065195996595C5C647D7DBD5E08F1B5
              C9643AEBF57AABB3492AC1E3F1088FC7931FAE5755F522D073A4827BBBFEA1EF
              B4A228288A8210E2E84B7C18C78295722C5829C78295722C5829FF7F418D2653
              0D8F7FA4E0B7F84CEB8F452730D49E40233D3AC982829D25086634B71FA960C5
              257ECC642EF754A328B22B78BAB157947AF204F0E74E9284545355A95CF6052F
              907380590AB5A6A6484268BBAB669487F683CFAEF2F957F38BE52678F16C0F6F
              BEFD3256ABB5EC93B07C720FACB4D1B54DA26B9B6527B335B683106B369B6DBA
              5A827B44A00A3FEEF1789C50287419B85CB1521E2B2B2BFC0DC906346CD5F3CD
              750000000049454E44AE426082}
          end>
      end
      item
        Name = 'select-all'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000006D49444154388D639C70E4EF
              3F061CE0F18DB34C1FDF3E67D0B1F6C1A98685818181A162F9BEFFE8121D914E
              8CC87C5C6A9870994C2C18060630521C0B8F6F9CC5E98A970FAF337CF9F08601
              9F1A968F6F9FE374DE970F6F18BE7D7EC7804F0DE55E6060184D48941A40712C
              0000D8054F17A8004BC20000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000013A49444154388DE554314BC3
              40187D273714EA902C2937E68682D0A170206E162908427F837FC0C91F520AFE
              14D75A32740DADE0A425A1835C2DB689A5812E350E314573C7A53829BEE978DF
              E3DDC77DDF3BD21B6EDF6186371979674B39C5F1C5E50040CB24A60070EB3FA6
              325E2B45E13208CEBE717E20533F948A965987E8883AA10020E3358279AC88B8
              632B5C946CB4DA1C07A6F67F827F6848816C9ABA0170C7D272ED86ABF076B5B2
              33F48AAB51C0F8EB99D76CF09A7A790E727573673203003C3FDD63B598E1E8E4
              BC544B97725A2A5A2D6648DE5EB18F96F486DB4189663C1979D79FD1EB02681A
              3B04D0F2039946C9462972C72ABE573378894E7549B1AB1508CEB2E8F9A1D4C6
              A9DD70950104F318FD87507BB9E0EC0F2CF6EF37A440F639EA90C7A9C8E92299
              7B5000E8883AD9B703C1193145F5038E23644374A717690000000049454E44AE
              426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000174494441544889ED96B16AC2
              501486BF5BF20071C97E7535AFD05A357B57F73E41A76E914E76F32D3A171CA5
              28E8D2B58182600507A166B042577B3A24316A734D848E9EE9BFE4FEF7DCFF3F
              E71EA2BAA3CD0F808888524A1DC1ED60DC7B08E713AE5B773EE0E77144442C80
              8FCF954C972B400900C81EF6DC3259D10F66464EC52989766C2C1191E97245FF
              6D2699A700CDAADE5B8B08801CE32817A51D1B2B921565CEB128252B8580028C
              16459FE1C27483FF0A2BCA16299558FB311CAF212699F745F06C51318B3CB74C
              B3AA4FB208509D56FD28277968ED826A873B78B0BD624EA8DBC7E782E7C3FCFD
              9575B8C0BDBC29CCB1C2F9A4F0E675B8E0FB2BE4148EEA8E363E44854BAC36E0
              4130EE0DE3617705D4F238228205F871B2B4790F3071CF93D6A116F372399688
              483F9825C36EB7705BDC69D5F764276D7AFFF462E478AEA651D5E7977C1E7619
              385EC32916559C922817F5B78D236C1A769EAB3171CA4E2955A01D1BEDD84699
              26058D839F81ACF8052CCE94E253BC63DD0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000009049444154588563601860C0
              C8C0C0C0E09650CD20A7694692C6374FEE306C985CCC40A97E1606060606394D
              33061D6B9F7FA418F0F8C65926189B12FD2CC88215CBF7FD27467347A4132336
              7172F433E153480F30EA8051078C3A60D401282521AE128E58408E7E16060648
              C5805CB613035E3EBC0E6753AA7F6483D1F6C0687B60D401A30E1875C0A80346
              DB03030E00DD245F1367329F0F0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000000F7494441545885ED97B10A82
              5014867F4305C1450842DCAC35A35928F105DA7B005F22087A8D867A8DA6D027
              10E75CC3D145086A6988C2C20A3D177438DF780FF7E3DBEE3D40C79100A06F0D
              613B2E49743E2538A70944FB6400B01D17CBD57E0EC06BE80B0FBB4DF40C14E9
              934B87DEE57A5B677951CB641A3A34550180E86324C4570E449617D81EE35AC2
              C09FC21E18953311BE5EADDB2DC0815438900A0752E1402A1C48E5ED2D360D1D
              813FAD25300DFDE78CEA2B07869AAA7C7DF8FF10569D89F04900608D2618CF16
              4D442FD238421A3F7E5CA27D0C05DE49A83EDE497827691B0EA4C2815438900A
              0752E97C20EF24BC93B4CD1D07D794D1D4097CC80000000049454E44AE426082}
          end>
      end
      item
        Name = 'find'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000001DD49444154388D9592BD6B53
              5118879F73B55E9A8049B9E97045021533686EE85689DA21760C687090361115
              2C6847FF0097822848DAD176EC902244D00EE2D424E0A0B8351F76B812300817
              9A40F3410837A61E8724C2C584D0DF78CEFB3CEFE1C7118C887BF11E332BCF55
              4575AD017104410024256057F67EBF697CD8B49B9FB611A360EDD1CB8BC0478F
              55307C6696E97A05808ED74F2D10A1A1878A40B4FE7EE39743A09CD7B8F02AA7
              2AAAEB9B5EDC3366CDCCA807520DDCC232EE14E59F9305C5B1FDFA5D14D5B5E6
              B1F2636180593383C72A184239F3D42150E7E601E23E3337161EC6676601120E
              81983A07109C3EAE4C140C7A092A236FFFAB767C1C02D9EB01943A5EFF447030
              537208BA951240AA1A884C14D4FA332987A0FD750F29D96AEAA1C36A60692C5C
              0D2CD1D0430529D93AFBEF54287863CF10826B80CF326E1FB6B54B577C3FB20C
              4BEDCCF8A95D8ED0D48D02106DEDEFD862086B8F5FE30EC7168134B02CA5FC22
              84780224105C4502F01D484929B75BFB3B76FDED0B0442415B1DC092348265BB
              7C903BDE5DC7B510459D9B474CA92025F2A447B752A2FD394DF767B1BF5B5B4D
              E20EC76E227807ACD8E583EC51F221B2D39A582480E20EC76E20480371BB9C3F
              150CFD7F701F48D8E57CE628F9E05430C05FD393AFCAA0620D26000000004945
              4E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000028A49444154388DA5D4CD4B54
              6114C7F1EFB9739B6BDD21A668D26024216D91D4043341426EEA3F083271E815
              DCF462D4CA6D14F4F20714EA806D34155AD4B65D6DA46648235B6404EA04C94C
              38A9379A3BF7765A64AFD711077FEBC387739EE73C8F502546641BDBCFDF6173
              E2A829C849200DA44424AAAA25200B0CA932EA7E78E515FBAEE07FFE8854C3EA
              7B47D9B46BCF5E6024E49513D1B92C76611AD375F0C2364EAC8552630ADFB426
              814EFF4BE1DDFCED8ED5C11D3DFD6C491CDB0B3CDFFA696A473C378CE92E07EA
              BC70847CB28BC55DAD4594767776EA5D00B45A92ECEC1D33055E6CFD3495D83D
              3E80A8563B19548499C3DD2C36B44EAAEA21E3FF02FB48070227435E3911CF0D
              AF8901882AF1DC3021AF9C1091CE00186EDA0F908ECEBD5C75CCD562BACB44E7
              B2005D01D0A8B3015276E1FDBAB05FB10BD300C1911140246ABA4E4DA0E93A88
              4834082AA05AF2C2764DA017B651D55200D44A1920EBC45A6A0257EAB301D09D
              7D0B30546A4CE15991F57567452835A6008602A033FE1855467CD39AC827D3A8
              044FE5EFA818E493697CD39A5018098EFC7511C0071E2D35B4CA4C5B77D54E2B
              568499B66E16EBF715804EAF90F7FF7929567392D8D5418C3AFB1C481FE875E0
              78C82B1F8CE673D885F784DC65FC700427D64C299EC437AD09A0F3FB37677AFE
              6EFACF5BB69A93C4AE0D6258F659907ED00B8A66504222D2C1CA6F834814D505
              20070CA13A5629E6FDE2FD4B5466DEFC04AD9695CE2CFB0CC800A217818185D1
              5BF85F0A44DA4F106E3AF06BE97F6E56E51B95D9B738E34F587E360A9E0B80FC
              EEACCE3E0D928115ECE10D969E3E58F342568BC4EFBDC6B0ECD34006E112D0BF
              F0F0264B4F076BC6000CC3B24F219241B8BC510CC0009A801EA06FA318C00FD5
              4704E278DE75380000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000003AF494441544889AD95414CDC
              451487BFB7FBDF6561D9426449084A4A8CC434555B1331503421A17A3069F5E2
              A1B4897AC04835166B1B3C78B2A6018931082A014E26C8410F4D4D8C4D5BD344
              29178C7A204D83A60A4D2114648185EDEEFE979F8765E9CAAE2BC6FE4E6F326F
              DE37F3DE9B19E35F1478AC998A97BBF096557A80678056E08099D508409A01AE
              0223C0A58DF8FAC6D2E819D6BEFF1210562878A8E525CA8FBC8B99670F300434
              FAE251820B53F8D7160148042B8886EB708B4A01C6813649D7A2973F6769F4CC
              3F03028F3653796208334F3370CE8947435593E7299F99C052AEB27DE5752C52
              F304737B0FE31695AE022F085D591A7D3F3FC07C01AABBAFE02DABDC038C17AF
              DC0AD58E0DE0C42232B3AD3592B6C692E41697DBEF4DAF11DB55BD0A342A99B8
              E6C907083EFD6226E7434E7C35543B3680EFCE72A16C02E0BBB34CEDD8004E3C
              1A0286CCE7F3E407341C06380834564D7E8D138B4892323BCD287B9CB19D5844
              5593E7011AC10EE6021C3FBEFB1F0638EA8B47D339DF14B0656F1F67DBE53313
              38F128406B0EC0BC3EACA804A031B8309553D09DC852AE4A17A6000E38F93D0C
              CC6AFC6B0B648E9E99CAB60BCDF9D716C1A8C90FB84BDAE4DDED94425D946D67
              7CF2161904D24C225851985F4089600588995CC0860BC904C078345C87BC4E4E
              A714EA224992D7211AAE0318CF0128192739FB1BC0881B0811A9A9CFDB3585BA
              2852536F6E200430923745EB13DF20B8887475EE91432403653B4E4D3250C6DC
              DE43205D15BA981710FDE12B1012BCEAFA4B13379ADA77044906CAB8D1D48E5B
              54BA22D126216F8E97E327DCF6214ED58366D0093CE516851291DDF55E2719B3
              A29539D8486D7BEC7C16D9DD60D30DAF9028A958019E077E5CFD7668DB63E7F8
              A96CEF27B0BF05831E333B29A9176910B341A0C949AC115CF895CC1D4904C3AC
              851FC2F50701C690DA04D763BF7CC7C2676F6401BC3E2A5FFF84E27D2D00DD66
              765AA817E9A48448D7B085CD0F07B307D2DBD74DD21FCE17C0656D6C68E5C230
              CBE73E0237B10970FC541EEFA7787F8B21BA805318BD126FA79666B5387C8AE2
              C79F25F8E47378768521FBE0121828B6CAFA4F9758BD304CF2E6F5AD69C3F113
              3EDE4FC9BE1630BA0C3B2DD407BCE52EDED27CCF31DCF93FD2DE1E2FDEFBAAF1
              04FF5E70C5D7716F4F43CACD29A985DF1C4C0787B366F68E501FA2C35D9AD5FC
              07ADB8F3D3398BFE8B9CCD9C9F053A85FA243A524BB39AEF3E827B7BE67F0507
              700CDEC3AC13F429D091FA7356F33D47EF4970483F763F031F234EB899E0999C
              DF03FD055BB4D89CD961E1630000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000484494441545885BDD66B6C53
              5500C0F1FFB9BD6B77CBD6AE9B735D9983491004F67289C20083A00E124C86C6
              89C29806A389E183686224513FF88A668C1825C1C4C4181F40826C620C82BA31
              F8A2C425B365061264630CF6E8186BBBD1E7BDF7F861C02486B5ECC1F9DA7BCE
              EF7FCE4D738F600A4371E4A0153F82B5A8044BB61B8100402231027EE2E73B88
              785B3083FE5BAE212603ABB98538376CC75EB1EE2EA1A66D04AA801221449E04
              8994FD801738224D637FF8AFE640B069177AEFD9A90764ACD942D6D36FDA9534
              DBDB8AA96F735E6CB73BFA7C68C33DA8D11008D0D39D845D85843C25043D65A3
              52B1EC927AE2A3C0A14F6223873F9F7C4056CD0E321FDF3A5F08F183ABA76DA1
              BBE31069D1D08473E25A167D254F12F4947A810D23ADDF750F7FF3CE8DDF2D29
              EFFCD1E771566FBF4F419E2868DF3FC77DFA30163D96749E458FE2BCD48E9A88
              B847F3163E659D5BD2240D3D183BFB67EA016A5E11B9DBF66408C5D252D0BEAF
              30BBFB64AADDC0D831DB87BBB124228E91BCFB57D9163CF455C4D76298C1C1D4
              02B25FF818EBECF91FBA2E9C5CEF3E7DE4B6F0FF0E6DB89B98D3E38E39F20D35
              B7F078F8F7269464932CD91EB492556EC588BF927FEAC749E3307612F9BE4684
              69BCA62DAACC52F38A920768A5AB1116B5D6D5D36653E3A3530A00B04602387B
              BD19209ED12AD6260FB0CD2B0758EBE8F54D19BF3E1CBD5E8075B6B94B527805
              391E80326DB87BDA02ECC317004A2D39B3930708AB66554CC3A5C6C3D316A046
              8320845B2092070002A49C361DE0FA6A82E401D2D063A6A206F5346DDA7C3DDD
              0152FAA594C9038CA14B00BE4856E1B405445CF700F88CD0E5E401F12E1FC0D1
              90A778DA0242F9250047135DBEE401115F2B12BE09143E98D0D3EC53C613E90E
              8205E56109FBC2BED6E4017AFF396267FEB868A8B62F0616AF9F72405F7135A6
              A27E1A3F7F6A28713E851300083436204DE3ADA1A2E55D818207268D5F995B49
              A0A0E2B494F283C0C17A20C5AFA11118402B7F2CA63A73C3239EE227ACA39749
              0FF5DD26BE8C4B65350308AA465BF70E8C367F9D628050C8D95A8FB664E5C3C0
              1E2994D783B34B172734A7CB7EA50BC5484C383D61CBA4B7BC06FF82AA3312AA
              A21D27FE19FAF20D308DB1E593E22FD6336B69F54AE000B051BFD2D7AA66BB1D
              20DEB3E8B197B27ADAAC8E5E1F5AA007357E150918B68C6B57B25202051551D3
              92B65B22DF1D3DB6F7EAF0FEF7418F8F13C9763EABB27AC535FCD958A7B7D5DF
              5087B5701159353BB0CE59E21142D43276292D16D2CC01A414CA106397D29F81
              6F63DD7FFB838D0D443B8EFF9F99105F56BD02C101E0B958A7F798BFA10E1919
              B9F198F5DE32B4D2D5D8E695A338EF46B167AA08A40C8F187AC04FBCCB4BD4D7
              4AEC6CDBADF73901BE1CF81E2136C53ABD2DFE862D37E1D3356E0E18C72B8183
              2037C7BA4E35CF140EA0DE0A97A6B159EF3BD77CF9B397670C87EB7FC3717C19
              D0684A6AE3FD9DCD833B6B31438333868F058CE34B812629E516BDAFF3B73B81
              03A8196BEAC671A88BF777FE3AB873136670E6710055499F05E046CA3ABDBFF3
              97C1FACD770C071042CBC459FD2A425109FEB4FB8EE200FF020E9DE3A9D35360
              330000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000622494441545885C5D86B7054
              671DC7F1EF73CEEE399B3DBB49767361A18194648342915296410933904E115E
              38CE60D1A9540A327594C1DEA8AD53677CA38EC541A64C2D450A2909DADA0BED
              8C621DA78651264CC49689A52D62851882A5E4C6267BCFDECE797CB14B1AA65C
              7661437E6FCFB3FFF39967CF73159438F69973D13FF745ECB5F5088731E18944
              665264FA7B49F59E20D3F72158E675EB8952A044991BF73D1B70AD5887EAF539
              05A205580AF811C203522219054E037F97D0698EF42763C77E4FEC703B56E4E2
              E4018D15EBF07CFD8728CEF226E049E03EC5CABACA46CEE1880DA2A6E22020AB
              B94895FB18AB9C85A5A861E06529D96125E3E722879E257AB8FD8A3D7AC340A1
              3BF17E6707CE45AB35013F05B61AC1B3B6AAFF1EA1BCFF248A95BDE2EF2C5523
              3C6301417F0B89CA9929609B84A793A7BACC8BBB362353899B070ADD49EDD636
              347FC0278438644F4602B79D789DF2FE0F0BAE2181705D800B77AE25AB194781
              7BD39F9C1E19FCF9DACB903704AC7E782FCE85F74C073A8D91B30DF5C75AB1A5
              6337528A8CA392BEE6EF325671DBBF8096E47FDE1919DAF1C0F8DFAD165BD0BD
              7223EE951B1D4288C346B077DEECAEDDA8D9E40DE100D46C92CA8FBB89F9E6D5
              661DE54BD5EABADF924DCBD499E3C50395F26A6AB6EC46D11CDBED63A1350D47
              9FBB29DC785D2B4B79FF4942F54BEAA5AA995AE3A2CEF8BB6F21136194620AB9
              573D8870BAEF10F050DD7BAF624BC76F1A7729F6649819EFBF09F023A1E9F5EE
              D50FE6F0055750545CCBD622104F19C3A755F7E0BF4B86BB948AF3DD38431FEB
              02F103D7D2AF814D2B1CA8FB17A3BABD95C0DAEA9EBF951C07B9115BD5730460
              BDD00DDD317F79E140AD712108B14ACD8CE9EEC18F260508507EE10314CBAC10
              82E57AD3E2C281765F0340B311EC45486BD280AA99A62CF43F8066BBAFA170A0
              ADBA0E608E1E1D982CDB78F4C800C01CC56114378A018F2D55BA917BB5E4277D
              2F42140D14B9456A922301915BE50A06E659215333AEDDB004317583DCF64C16
              0EB4C2C3006792EE699327CB27FF8E33329D2A1C981D3A07702C51D5802CCD3E
              F78AB1141B639E5900FFC80C9D2B1C98EAE9464AF976563732B1DA3993068C4C
              FF0296628F493892EEE92E0278FA383295082239146C6C993460D0BF02E05569
              6613C9535D450C92F418F177DF02D816F5CD93B1EAA692E3C2331610F7CECE00
              BF1C3BD181151B2D6E9A89FEF905A4997D4FC2BEF38175983647C97019DDCD85
              85DF0078464AD913F9E3F34091FB412B1E4671B8D0FC812396E65C93F4CCAAA9
              38FF4F84BCB9B9D15235FA966D26E59A765C4AB93176F47533DEF95AF1400029
              2D8CE67B3302DC1957CDCA8477F6350F49D74B5673D1B76C3309EFED3D48B93A
              33D01B09EEDE0266A678A0EE0F50F3682B8A4DDB04E219903F4E1B55D3C3758B
              6A1C917EB444B0285CC477077DCDDF23E9F6754B295799918B83C33B37618587
              C6DB140CD4FD016A1E6F437118DF06B117E41689FC15920396E6F486662D5994
              F0DE2E6CE9385A6204719525510A95E8F4F97C72D77D0C7DFECB59CBE6D80972
              7D76A03732BC7313D981DECBDA1734E3EA4D016AB6B6A1E8C64610FB407E1FD8
              177A633BAAC787ABE57E506D770A780A58A366C63423D88B1E1D444D273E3DB8
              BBA791A86AC0B4E949E0A094F217C047F1AE3719FDDD4F3E73262E0838A1E736
              8068BD841B7DE567443BDA81DC7D4CF9571FA16CE1DD28AADD03AC02BE043401
              9E7C778E2073571F200F4BD38C8C7DF057C27FDA43E6ECFB577DFF3581E338DD
              D88010AD201F02F64EC44D8CE2F250B6E06EB4C6BBB0FB1A502A6A50EC7A6E0F
              9449618686C90CF591EEE92679AA0B7374F07AFD7375A0D614A0F6B1361487F1
              0042BC083C0CBC90C3B55DB770A97245A0EECF7F730E633DB01F780421F6DC6A
              1C80ED1AB86F01FBB1CC4711624FF8D0AE5B8EFB0C7002EE7E8468B32CF331A4
              FC75ECED56C27F78F696E32E036A9FE2D681689796DC2A11BB231DED44DFD83E
              2538C84FD45AD3626A73B86F22C46F90D6E348F97CA4A39DE8C1A7A70C07A0DA
              67CE65DA132FE5711C909227805D91BFEC2732C538009B7DE65C1487311FC101
              A47C12C973E18E362207B74DB50D005BFE02FB24965C2EE19D68C77E22AF4D7D
              CF5D8A00702EF90A365F2366689878E72B536DBA2CFF0731BE752EB83AB67A00
              00000049454E44AE426082}
          end>
      end
      item
        Name = 'replace'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000030349444154388D6D914B685C
              7518C57FDF3F93DEDCA477663A988CE96B36C5CA348F4AAAB8B20ED924188231
              340FB1810A79595070A1815A1A7C1082881189A1183719A8AB12B2120D685682
              8D8F260B654609ED68D24492299D64663477EEFD5CB43E063CDBEFF0E39CEF08
              406767271313135DC0A56C367BA6B5B595D0C834D5A75B7D00552D025F000377
              C63B76DD5F7FE26F1980BEBE3E809780539148E4C9B6B6B67F0C7EC9355A72EB
              0017B87020D6C07F65E2F1384D4D4D278138700518EDEDEDFDD7E1B9A8E71AC0
              065C44CA0081FEFE7E446414F844553F16919F9B9B9B1F7AF4DEDDED0C60ACEA
              FB35C043F5B2A9099527686F6FAF010644E40D63CC8E884444E4C51347A30068
              C935DE7ED142750C78BBB22E560EB06DFB79E0AB5C2E6752A99451D533C070CC
              F60D807A2E7EFE9E023EFF23033C037C34333343777737994CE67B60BDCED2D3
              3CA85079285A007A415F2BDDDD2AFFC1E6E6E673BEEFFBF3F3F3F8BE4F329964
              707030F1FBC61F9E7B6BDF3CD81105FE4C7D43EEB3AB6500999E9E2693C93039
              395976181919A1B1B191DDDD5DC6C6C6A83186B1C387690F874DA8A2E288AF9A
              FDB158CC07128984BFB1B161D6D6D6585959219D4E130E87191A1A0A5A96751B
              786C6161E1D6A59D1D9E0E85CE01538007048F58D60706A0BEBEFED0F8F8F82B
              737373343434D0D3D38365590380058CBE75FE3C6783C126604A553BB2AE7B5C
              558FABEAF5005052554744FA1DC789CDCECEBE5A2C1601468161E0DDD8B16357
              C49817509DF97A6FEF876753294EDA76EEE1406035A0AAEF8BC82F2212009E70
              1CE73BC77136817D554D8AC839FFE0C13EDFB6EB4DA170F3EAD61645556E160A
              F7675C5D5D7D1D08A9EAA2AA26814F818BC00911F90D78CA0B872F96EAEA6E03
              A786A3516C634804835CA8AD456A6B6B595A5A8A022FABEA65E0A8887CABAA8F
              6F6F6FEF87C3612A2B2B979CC5C53723D7AEBDA7AA8305CFFBBCC6984710395B
              512814C8E7F3F9F5F5F52FD3E9B4C6E3F136E0CEF2F2F2F5AEAEAEBDAAAAAABD
              969616E345224EF58D1B1F56148BEF1C30660A910E60F92F0B9B4349BBBA2035
              0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000040749444154388D6DD05F4CD5
              651CC7F1F7F7777EA71FE728726A264750C329D2B06561CE799372B0920D5022
              3130BCA8B5145BE54DB6866BD32CEA74931B2BFF4C863A92AED0D9D6ACC31FDD
              6A82B5C950AAB9084B4037060B010F7FCEA70B8D54FADC7DF77CF67A9EE76B00
              3E9F8FB367CF120E87BF00D22515E5E7E7D33B9A6041F4FC5AA0993B9992F43B
              F009E268DFBE8D4CF474726F1C80482442381C4E01B69A59BE99656CD9B2E5BE
              A2A45C492F007566760858E165ADE6C13800A5A5A500DB80BF255D06DE282929
              C1F3BC7BBBADF16B579A247D24A917F494E3056682191919AC59B30660077018
              A8015E4D4E4EF60A0A0AFE6B1A6BBD45D97966560D84053F6A223E132C2B2BC3
              CC22C012498725D5037E33DB5C5252728F67CD66F69D99BD0BF499D9B07FFED2
              9960515111C00E33F33B8EF397E338C36616022A17A6CEC567FA77874E229170
              12D212A01FD8EBCE5D30139C33674E3A502469FBDDC5E74ADA6C66AB937C3CFD
              E8439A2E8F753481D40DFC00A4CDD00017781DB80A1C8A46A374777773E0C001
              5CD76D728DCAF4249DE88F83997D105C9107B0082807B6FF1FE800A340B4BFBF
              9FE3C78FD3DADA4A2C1643D2E70643C9AE868016602DF02C301BD80AD48D5D8A
              CD7C614F4FCFA7AEEB525F5FCFD4D414000D0D0D2C5FBEFC4C023BD37B7980DB
              438A00E8CE32D1449CD19FBE65E4FCD73340DBBF7F3F00478E1CA1BBBBFBBEC3
              4020405555D5F45C5D5DCDF0F030B31C879DA9A96C088548F5FB0118999AE2E7
              5BB7708B8B8B9B805D8D8D8D971E040B0B0BD9B469D302A05652716767E7ADD3
              0D0D9CCBCE262B105808EC0172010F68CB0E04F639C03A49A1DADA5AB7B3B393
              DDBB774F8365656500DBCD2CCFCC5E292F2F674F5A1ACB92921E03DA802780CF
              802AC08759A93BFD77B35A60A0A2A2E21D80582C466666E643C06B92AE02958B
              172FFE32272707EBEBFB18F84D52DEAF636393D7C6C7792E25E51892B9C03890
              0A1C34B36F00A7A2A2E2ADC2C242CC6C33902429E238CE2549EBBC949473F4F5
              1500DBBAC6C62673BBBA18492448F3FB71CDE4483A686627CDEC1C906C666F9A
              D9FBA15008602770CCCCBA249D36B3CAC979F392CD6C36F0C7C19B3719492400
              E89D98E0DAF8388EA4B725E548DA28E986A42EE028906366AB81C7EFCE616063
              3C232324E936307FC39D4BEF8BAFA5A5854824D21F0C06AB81A0A43CE086997D
              08CC02AE9819C000902ACF7382EDEDA34E3CFECC52CF3BB92C29892703014E65
              65519596E63780C6C646323333B3240D01378047CCEC4FA0FCFAF5EBA7DADBDB
              59B56A15E9E9E9959648543D7CE244417273730BD2F7401D90005E04E6FB002E
              5CB8C0E0E0E0405B5BDB484747072B57AECC015600EF45A351D5D4D4303838C8
              FAF5EB7FC1715E4E78DE57C18B17EB6C72F2796017F012302C69EF3F0C1EAA8B
              27570A430000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000005724944415448897595616CD5
              6719C57FE7DFDBDB726F29154A6B6F564220571620318E1589619699305A9840
              4D5A90149984A425B562FC4013CD06AB6699C43948266CB5624D30A138283A63
              4D4CD3C91A3441BA31B77E201653128AD469D7DD96DB727BEFF1C3BDAD8B94F7
              EBFB7F9EF33CE79CF7FC45EEECDFBF9FB6B6B690A421A0DF76D3912347E8EBEB
              E3330DDFA3B8E6D031DB2F481260DBE340BFE1A8CC3FEEB6EF2275FB03FEFF04
              004110B06FDF3E246D93B41AF8BAA492C6C6C6870A6C1FB7DD0EFC1A785AD06B
              1C5EF4F9AF3CF4ED3CC0A64D9BA8ACAC04386CFB1320027C63C3860DAC5AB56A
              AEF15C4D7B7AE2A3176D1F069A8138E80B794B4A1F0D909B7E95A46DC0ABC055
              A0290882A0BEBE1E802C3380209DF8686E9F224908CF2ED81D0862B1189B376F
              0668B23D6BFBE7C0EB921E97F4F4EEDDBB8944239FAE39167EECF1E3923A41AF
              D9BE66782F73FF9385011A1A1A0885428B8003C06F813BB6DFB4FD2FE0F0E2C5
              8BD9BA75EBFF28322F00CF03DF04160196142D5CF7D4C20075757500F5C07249
              F54110642425259501CF4AAA7CACB4649E224979409E71B1E120D213C077F34A
              962F0CB06CD932241D066ED93E0E1C075EB4FD5296710EC50A3204CC8BCCD45F
              7E433A313E85FD4BEC9BC0FA47691002360055C0B781D357AE5CA1A0A080AAAA
              2A24AD070E9515F887913C48CC66DD14DDB40BE388D02EE073C0C54702E4A64F
              00BFBA71E306CDCDCD8442217A7B7B89C562A781DE6888BAA29049CC0A491900
              219C15E69AED937E84C821DB6724BD6D7BE2DCB97300CCCECE72E9D2255A5A5A
              FA241DCD8381C2807B90E52927F834F0BE4D1F3835F1D66B0B02A8A7A787FCFC
              7C92C9248D8D8DCCCCCC00505656466767274110303231C3AB83FF6622A5F9C2
              39576512FF61F29D0B3C181E5C7883AB57AF02303C3C3CDF1C606C6C8C9D3B77
              02B071E346EAAAABA1207B77E1C20546464600880401074B4B79261E67654101
              7323FCF3C103FE3C39898686868E016FF7F7F7FFA9A5A565C129BABABAA8AAAA
              8A93B5F3CBDDDDDD99F6F6765686C3F4AC5943BCB050402DF02C50018C61BF65
              F8BD86868632B68F8F8E8EB60F0C0C90C964E8E8E8E0DEBD7B00C4E3712E5FBE
              0CF033E020B0636262E20F3535355C8EC5A82A2A8A4A3A0F6C07C681DB40A5A4
              A5B65F09724EA0A2A282868686BA3D7BF6045D5D5D94979703D918B7BD0CD89B
              5BA8A5B8B898137BF752555484A453C033B65B6D97DB7EC2F059DB4DB65F0F72
              F98EA45A496F4A3A55595919747575118FC7A9ADAD45D273920A818BB9405CBD
              EDC92701560AF6930DC8D3435353E903C3C3F47DFC713A914A754ECECEDE0A01
              4960A5ED1F003F92D406042B56ACF8567777B7C3E1700034DB7EC7F611493B25
              35391A3D4A5E5E357608FB177753296A6EDE643C9DE6D2F8F8BC7E81EDB3C001
              49B3405B8EB166E027E170184935C02AE0A7C05DDB9780E7D2D168C4914885ED
              07C0E8D9B131C6D3E9876D6AFB3BC080A4354099A466DB49E07739F65A244DDB
              8E4ADA0B8C4A5A9659B2644F7AC992C9BCC9C990EDE8FA4824B19003F362B198
              972E5DFA61341AFD50D2F78162DBBB25F50171E01549059276035F93F4250087
              C395E1E1E1CE823B770E017F8B1716BE3F964AF1EEFDFB6C2F29A1B5BC9C9A92
              92ECBB3871E2043B76ECA8027A80FDB6FB73C2FF1868B1FD94A44F87CD2EE0E5
              4583835F5EFEC61BA73433536EBB1AB835964A51969F0FD25AEC2004D9576CFB
              1AB006989A9E9EA6B0B0304AF6277411F8EBF9F3E7B97EFD3ADBB76F67CB962D
              6724B5CDC4E3CDC975EB0E460607FF28E95DA0A73C1CBE03AC06BE8AF45E08A0
              A3A383DBB76F138D46A732990C656565B4B6B656007F07CE2493494E9E3C4922
              9160646484EAEAEA49499D99A2A2E5C9B56B3F5874E3C617954E3F0FD400A5C0
              2870D6F64BFF05BFC554F467937FDA0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000007C3494441545885CD976B5094
              E715809FF7DB0B2CEC2222AC20E282608DF5BAB618EBAEE93A2A3A6A07271BE5
              62AC9ACE9848AB9DD62833B14DBCC52655D3E82842751C2512D4EAA40E4CBC8E
              3AB13141131C95CF182FD86A55E4A2A8EBB20BECF7F607175DC1A07F3A3933FB
              E77DCF79CF33E7ECB97C824E64EAD4A98C1F3F1E80DCDC5C3C1E4FD07DB75FFD
              0E63D2D0A7AC245AC3237C974FF3E85F7BA0B9B1B3A73B8878FA4051140E1F3E
              2C626363CF036FAF5AB5EA40515151904ECC82CD840D1B1B78C25E9352D603A7
              81BFF8AF9DFDA2FAAFD9C8465F9700CAD30763C68C213636761C9004E4646565
              3DD358F33728FE7F9F579AEE551924F4070A81E290A4A18EF0D1D3BB74DE2940
              464606400E900BBC9C9898983862C4884E8D9B6E5FA16A793AB7168E9237FF38
              B2CEFBCDE7C5C09F80DF1B6D835F1CC066B3316AD4A804C021A5DC0A140A21DE
              CCCECEEEF221ED7E0DF59F7D8C94A8401F11DEEDC501B2B2B21042CC0576DCBA
              75CB0BE403B35D2E5788D56AFDC18784C942F7CC2508C12BC005E97DF86200A1
              A1A1A4A7A71B813780FC356BD6A0AAEA55E08CC16098367D7AC79C1AAC366216
              6CA6E73B7BE8FDB7B268D3E05FCE07DE063EF45FFDF6C500264D9A444444C4AB
              40457575F5A5A3478FB273E74E803C609EDBED46AFD7071B87456861C3C66AA1
              29C335C5185A2D8458079448292BB586E0D2ED12A035CF39C0264DD358B76E1D
              53A64C01F81C888B8989B18F1D3B36C8B8AD0A1A6F572A01BFD724A51C09F412
              427CDA3DEBCFA00B06EE4CF40043860C61C0800183018710C2191717475C5CDC
              937A129897999939F7E0C183ED876D5500804EEF0F1F997E2A6AE68AA9C218A2
              EA2D513F33C4257FDBF4DFEFBB8E406BADCF03DEADAEAE5654550DFA699A160B
              BC9A9A9A1A999C9CDCF94B81661E7DB997475FFDB3094939D0179DA1EB084446
              463271E2C40860BA9472606E6E2E656565414A050505354EA773BF1062566666
              E6BABF3FE30F1EF293119886A7E9003BF0215AA06B00B7DB8DD1689C091CAAAC
              ACBCF3B47380DDBB77E3743AF380ADE9E9E9EB8B3EBB2C258FAB40319951BAC7
              86E863FA0C12F00E7059F379CE34DDBEDA258062B7DB0166029B8A8B8B3B553A
              76EC185555555F01DEF0F0F0118989892DC66D55D0FF65CD60B5D50BC136A05C
              4AF95AFD9ED5CF3590F4D7AF5F474A39DEEBF53E2C2929E95449D33476EDDAC5
              82050B5CF7EFDF7F78EFAA8A3FB4560720747A84B9BB904D8D527AEB69BC7919
              CF17BB69BC7CBA4BE73F0A11090909582C167C3E1F959595FF7F808D1B373266
              CC98F754555D366DDAB4E7328A898961E9D2A54829292929E1C9DEF0A424188D
              644445610F0FC7A0B4F63C29A9686860575D1DDFFB7CB4B5AAF78065CF4B9D91
              9181CBE55A08FCA277EFDEAF750630D76A6555424278A8A2CC07A6032FD1B2C0
              5C9ED4BDFBEE8571711F7F74FBB627681A1A8D46468F1ECDF0E1C39FE95CAFD7
              E376BB155A1AD7F89494943EA9A9A9413A73A2A3F9C8664B0A559472602090E3
              0B04A21E05029148F90690A213E250568F1EC1E338272787BCBC3C67616161D4
              AC59B33A05484B4BC36AB54E02AA812D42883767CC98D17E1FA9D3F17E428211
              D807ACBFDBDC3C73FEB56B5FC79F39E38B2F2FF7CFB872E59BEB7EFF6CA49C52
              E1F5B6A7004551309BCD00E38410EB172F5E3C1EA8DBBE7D7B10406BDBCEA165
              4A7E0D9C70B95CCB7BF6ECE9BF73E70EEEA8282C7AFD6CE03F0F03818DE3BEFB
              8E4BBEC7BBE1BEFA7A0E3D7840379DEEEEBDE6E6F608046C369B72E2C409EEDE
              BDBB9496097864F1E2C53D9E8C44FFFEFDB1DBED29C0B04020F08F1B376E5C01
              CA0D06C3F4D6550E87D98C800C60D307B76E05396F93064DA3AAA909BF942D00
              52CA7293C9F4D6CA952B95070F1E40CB5E57DA0A11DDB6986667672384C801B6
              9E3C79D29F9F9F4F6B2472DAF605B34E0730544A59F6696D6DA7697C529453A7
              4E01FC1A98151515D5989C9CAC09213421C41221C45060BDD3E9C46C363379F2
              E430E075A0A0A8A88803070E505F5FBF1F88898E8EFE795A5A1A0801D0ADBAA9
              E95E4D73739700BAF3E7CFA3695A9DC964DA525B5BBBBCB1B17199C5625906E8
              804820ABA4A4C4DBAF5F3F5C2ED76CC0086C8E8888202D2D8DE8E86819161666
              00265BADD67DBAE3C7E9171AFA0793A2146CA8AAF2354AF98300411F26F1F1F1
              141414909494B412980C8CDBB66D5BDDDAB56B292D2DC566B39D698D4A079152
              364829FB3CCACDAD1B585D7D1C78FFAD6BD70EEFE8220D41653867CE1C6C36DB
              5260529BF3D5AB57E37038B0D96C4EC0E8F3F93A2C2CB5B5B50A502C84F84D6C
              5A1AC01E60EEBBBD7A116BE8B894981585616161F47AFA6EE1C2859C3B77CE51
              515111B568D1A2F6F3BCBC3C54552D5655F5B72B56ACE8F0A0C3E1405555BBAA
              AA9517CACA94070E87C9939A7AC9939A3AF3ECA041382D9676DD57CC664E0D1C
              88273575F44DBB3D380566B399091326E0F178DAFB7B7C7C3CFBF7EF8FD3E974
              E780BE6EB7FBE1C58B1783C3A8286D29FA12F840FFC92725F1478EBC84100780
              FD40FE75BFFF821E0C7146E3D0D64A1AA149E90C4A81C7E361EFDEBD41C32525
              25059D4E3712283C7BF66C07E7F0785F00360023778586B2B9A6E6A294D20ED4
              02C57D42423CBD42426A84101B00554A39BCB0A6A6A6C3D7F1D3D2B76F5F962C
              5982C562213F3F9FA3478F76AA67369BD9B2650B003B76ECA0B4B494516633AF
              F7E8C14FC3C2D0B594274D5252E1F5B2B3AE8E931E0FFF0332BD0FAC38B0654D
              0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000AC8494441545885ED987F5494
              E795C73F77DE6146108611868420033AD650A5626DBA615388A829463C9D8821
              8A424D4212EB4AF6F4AC9BB0BBA77B92AC9E98D8D4D5762B499B6DD6F4444962
              8F565B7AA2209A31EBAE1B0B6AFC51090AA1A2120521A030E03B73F78F19880A
              24D2F6CF7EFF99F779DFE7B9EF67EEFB3CF7B9F7114690D3E9A4ACAC6CB0BD7A
              F56AFAFBFB87F4B34FFE26D1F72F1E725FC3BFC1EE2BF81B7E8FFFA3FD100C8C
              F4BA11651DE941414101F9F9F95F055E0116D7D6D6FA77ECD831D4C01DA94467
              3FF228F0F88D780A7D28E7015FCCBCE5BFBAFEC753FE4B1B4B0876B58D0AD032
              DC4DC330282C2C04F87B11F90EB028DC1E49A9C004C007F854F1A11C01A2801F
              0B1CB5A54C9DE02A2D1F151C8CE0C1ACAC2C929292A281EFAAEAC740697A7AFA
              5BD3A64DE3F8F1E323D96A36DB5A565F3DB81D8BC38511EBC23E250B8B3DCA21
              223B805FDABF724F8E35D183D9DAF8E7011615152122DF057A814522725455BF
              51585858F70580986D2D7CB6EB27836D898C21FEC957BAA266CCFD07113906A4
              5AEF486D1E0DE0904FEC76BBC9CECE065809BCA1AAC755B546444AE7CF9F8FD3
              E9BC6DE3DADB4DE78E0DA8EA1F0014DC3276DC6D8F1F1670F1E2C588C8FD2232
              0578DD344D80D7802576BB7D5C7E7EFEA85E103D6719C0644044F5A25EEFFBD3
              016D361B05050500A5AAFA3B55FDE3FAF5EB017E035C011E5BBA74E988C624C2
              8E113F9E08F71422EF9947C2F7FF939859451122F243553DA4703678B9795480
              37CDC1BCBC3C1C0E4722B01078A8AEAE8E2D5BB650585818F0783CAF037F979C
              9CFC939C9C1CF5F97CB7DA8AB54F9A9193FCA30F5055001B90067C0F88071E30
              2F35D3DF7C625480377930BC389E029A55B5BAA2A2028077DF7D1755FD05A150
              F2EDC58B870666E0EBC07E60BF88EC17913D22F21F22F235E098AA76631B8331
              2EF14F034C4F4F273D3DDD0096033F13119D3D7B366BD7AEE5DE7BEF0568057E
              0D94E6E4E490949474AB2D5FD89E45552DAA4143551354F521C0292287ACE312
              C73B16AE1A15E0E0270E7BCF0BB881A580D7EBF50E760C7FB6BB804922E22E2C
              2C3CB771E3C69B8CF94F1FE2D3578AC062608C4B54DB8469ED310F3E55699FF4
              F52A417C026BC6CEC87DB2837FBE6D400B84F6DDBCBC3C8052E000F03B11F185
              BDE2037CE1F63B4033B0A2A0A0009BCD36BCD5608040FB797A6B7773695D217D
              F51FF6033F077225D271DB7010F6607E7E3E369BED6E1199A3AA99C78E1DAB3D
              78F0E090CEF3E6CDC3E3F15C029E773A9D6B727373FBDFEFFC92370403F47D7C
              983169997E2062547403804B962C414456AAEA6155AD2D2F2F6738C0C6C646D6
              AF5FBF057859441E2E2E2E7EE7FDF2ED235BB71844FDED02621E7C0A601E5037
              DA8CC69A95954572727214F028B0AAB1B1715838809A9A1AAE5CB9D21D1717B7
              0528CDC8C878273DFD349F869F47DC3589F8275E41226C581C2E22C6A7618989
              7308FC232245AA9ADBD778747480B9B9B988C85220006CDBB66DDB889DFBFBFB
              D9BE7D3BCB972F7F4D443E02A66567671FDF6E0230DD884DD8179DFDC8C082B2
              004E9034D02BA81602BEEEEACDA303340C03553D2722FFDAD7D7E7DFB56BD717
              0ED8B66D1B4F3CF1C449C330D60201AB61803918626E5527A227543910ECE9BA
              DEB9ED657A6B778F0E70F7EEDD24242454D9ED760E1D3A447777F7170EB870E1
              02151515A4A5A53D7FEDDA35F6EDFE6F3A8D3B0F105AFD0018F1E301085EED40
              FBAED1DFF231FE1307D0BE9E51C1FD557F09C9DAB56B59B870E19D80BFBCBCFC
              B3F2F2D1A7E51919194C9C381180D3A74F535F5FFFA563E20D83FBA2A389B5DE
              9C3337F7F773F8EA55FA420B6D70AB7B9BD08EB17AD474C0BA75EB484D4DCD07
              2E1E3D7AF4FF8A8B8B47EC9B62B3F1C39414E6399D44403A22F7017702FD4003
              AAFB3B0381CFCA5B5BD9D0DA3A725577BB9A3973262929296381FF024E4C9F3E
              7DE6D4A9533975EAD490BEF74547F3EBBBEF26DA30B2800D22F24D556D065A00
              3B908688D569B5BEF683E4E47FCB8A89B9362434444646E2F57AF17ABDC4C7C7
              7F296061612122520C5844240B983E5C0538D96E1F807B92505A765255BFAAAA
              9EB6EBD767FA03814C42D94F09F030B026C56E1FEAC1175E7881F9F3E727582C
              167B5353534B4949096D6DC3D7B26EB79B59B3664128C97855553345A4D4EBF5
              AED8B871239D9D9F6FD4EBDC6EA22D961C42E5C34A54DF78AFB393972E5CE048
              4F0F7611BEE3745E5F9D9CFCAB0963C654896AB0C1EF1F1A5C5D2E1722B24644
              F67B3C1EF7E6CD9B71B95C237A0FB85F44D209652BAF024576BBDD7963EDE2B1
              DB99EB740A223F255488BDF17C4B0B8BCE9CE1484F2836F6A9B2BDA383CC9327
              59D1D8F8D98AA6A6EE954D4D9F03AA2A4EA793CB972F130C069F219456EDF778
              3C29C341DE50BF3CADAA95AADAACAABB800EA0A4A8A808C3300098171B8B88CC
              144853D5353B3B3AD8D0DA3AEC9FBE160CB2B5BD9DADEDED5C32CD10A0AAFA81
              E8BCBC3C2A2A2A3870E0408FAA7A81A63064EAAD900B162C202626E62E60A1AA
              96D7D5D5D1D4D464863DB932292949B2B2B200981A19093057E17F818B6B5A5A
              86851B4E031EAC021E753A9D996FBFFDB6CC98310311E9051E02CE02FB0620A3
              A2A280C1C5F13DA011A8D9BA752B15151503B58B5B441E2C2A2A02C02A02A102
              EAD8A9DE5E1AFA6EBFF4B47CF8E187009B803D168BE57F2C164B202E2E2E2822
              4111B92622DF169189C06E8FC763753A9D6464643065CA940842F5CBAB6D6D6D
              BA77EF5E2A2B2BE9E9E9F914D80E3C9D9D9D8DDBED861060ACAA767ED433BAFD
              D8525959C9CE9D3B4D557D34180C4E52D559C06C559DADAAB381EAF01458659A
              A6D9D7D7C7B265CB20549A3A805F9E397306AFD7CB030F3C405353134039A104
              75E2801701BF8844C686E7E5ED4A062E264C98404242020073E7CE256CF84560
              15F0B0699A7BCACACAA8ADAD65DFBE7D58AD561F7007A13A0509796920170478
              06F8595757D73FFDE1F1C759EC706C0292AE06020FA71E3932B8957DA907072E
              3EF9E4130E1F3E8CDBED66D1A245002F85E1F24DD3DCF3ECB3CF525555454141
              0156AB3503C8065A4524474472801CE0C6EB8F811287C311F937F3E7031C0472
              C61A86BD7884B0359C8604EABCBC3C0CC3781EF83EB0C034CDBD656565545757
              631806E1A38F52C0A7AA73EAEAEA08046EAE33A2A2A2484F4F8F04CE014BC6CC
              9CB9593FF8E03722522EF0D8EAF1E35FFFA0BB9B06BF7F58A8195151441B0667
              FDFEA18081400055DD031C344DB3A6ACAC8CAAAA2A00E6CC994342428253448A
              55B5A4A1A161603EDE24C330A8AAAAEA4D4C4CDC0C3CDD3F79F2E6EB4949D76C
              172FBE04BCEC8C88F0BD979656FF546323EFDF9020C75A2CFC60FC789E4E4C14
              20B5B5BFFF932133F6DCB973B8DDEEF32D2D2D4D1B366CA0BABA7AF0D973CF3D
              477272F20AE01BC0CA4D9B36054F9E3C390450551933660C9999996745E44585
              BD6AB7B744D5D51D42E45E81E7C61A464391CB559F1B1B8BC76EE731978B1FA7
              A692E570A488C85BC0AA288BE55519627D04C5C7C7E3F3F944444E036F757575
              BD98939333ECC1FA40FF9A9A1A2222227E0B749AA6B9ECC8C2853C121F6FE5F3
              C57796509A77098801A603F703BF074A7A4CB37ED833EAE164B7DB119148A012
              F8C5CE9D3B478403686F6F1FF0FE7AA0CE300C9E6C6CE4DF2F5C30AF0783FF02
              7C05D84CE8E4EB5BC0A43058AEAA7EABBEB7B7FEC1FAFADBCF073B3A3AD8BA75
              6BCFE4C9939F011838F9FA22BDF9E69BB85C2E1FE03B71E20401E085F3E779E3
              F2658AE2E2CEDD131DFDA3E85BE2E2A9DE5EF67775F15E672701E0FF01A78240
              03C9D1A07C0000000049454E44AE426082}
          end>
      end
      item
        Name = 'control-edit'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000000B649444154388DC592210E84
              301045FF6C50A08AE106D0832038510F81EF5DB84809190D025314955B149B90
              B29B662BF8AEC9FF2F337F0A3C2DD25AA701C6717CA7003200504AF97FC27DDF
              53763E841081C15AEB9BA6212925D67545555598A609CC7C9D0000CAB2A43B00
              33639E67745D47C33078E7DCC5F38A19F50CEDFB1EAC1A05F8A528409EE70080
              A22882353F1D586B6F2F51D735A4945896C5B76DFBBDC46DDB82301111335F02
              8127F9231963928B7C56075E4A49AE08AC17480000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000011649444154388DD592B18AC2
              401445CF84102B11C2541241ADACECAC65FFC64FCADF6C932FB0984A2236C240
              120803092932B395293609EA6E8ADDD3CDCCE5F0DE65E0AF2396CBE5B442A5D4
              714AA10F7C4E2D248E6397A6E9AF44DBED96D3E924FCC785E779789ED70B3AE7
              68DB162925BBDD8E3CCFA9AA8A288AD05A73B95CFA13022C160BC23014DF8575
              5DBBFBFD4ED3341445C166B3210802D234C51833BCF22B1863504A0120A5E47C
              3E0FE6FA3B3EC13987736EF4FD6DE133DE160A2110A25775C7CB1DCEE77356AB
              15EBF59A2008D8EFF768ADD15A0F0BCBB2C418D32BE7D1D76C36230C43AED76B
              F76DAAAA1A175A6BB1D68E4E986519499274E7DBED3698134AA971CB0FF0818F
              2985E270384CE9FB077C011C296BFB6CE0EC0D0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000184494441544889ED94BD8AC2
              501085BF5917821601C128D859E615AC2CC4D2B7B15EF6217C0E7BADC527F0A7
              F7072E4A0415E54264B658136274CDEE825BEDA94E8639393373E75EF84706A4
              D3E93CD7603299BC01A82A22C277F977F25415198FC7E76776F02A2232180CB4
              DFEFAB5C4A50D54C9E95D76AB5A4D96CF2FAD38A1CC7A156AB61AD653E9FE3BA
              AE7A9E2787C34196CBE56D07FA0900AAD56A1497448E00ECF77B76BB1D854201
              DFF701F07D5F1CC701C018C362B1D0BB23121105C8E7F35F8EE5743A29C076BB
              A5D7EB51A954A8D7EBACD76B46A31161185E692283979F8E2882314601369B0D
              E7F3D77B7235A2A473164FADABA6F3E20E2E23E2228A3FD23C1224E257B1B426
              367830852CDC547B0FBF362897CB02502A95C8E572A40A8F717506D1A6DCA950
              C33004A0582CD2683400B0D6E2791EED761B638C0C87C3C76BBA5AAD3EFFF6E0
              F61E8F47A6D36974D1D4755D2E178DA426368848B2C547DC5ACB6C368B634110
              48100451C772A3F98BC7EE1D9EF85C77BBDD6736F08F6C7C00AAF00F3D205664
              420000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000000DE494441545885ED95B10D84
              201885DF6FAC700007B06608130B3BD7700287701AED5C844E1A120B079052AE
              C2C23341AF0093E3EB4820EF83C0038844FE1D6ADB169CF320E14A29A49C7394
              65B98710104224A91D745D677C86F77D4F0090F80CBD22754FB947D33430C660
              5D57E4790E22C2300C8688C88BC0388E608CA1AE6B9AA6C968ADE10ABF14288A
              C2B9E8CC3CCF3B1191D61A00B06D9B73E796E077200A448128F0D50352CAC77F
              827DF38C310040966547273C16F8155BC5CBB298AAAAC254F1993B6DF89E3B60
              FF67EF024A290821829C849432446C24F2323E723648EF2326A72E0000000049
              454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C0864880000015A494441545885EDD7B18AC2
              300080E13F6A96662952E8E2525C855B7C80E320A3838F7283CF710FD4E5E66E
              37383A35EA224151442AA5B7281C7705D19EC43BF22D2590A6FF12D280E779DE
              5D89300C0982C07547ADA228E84C2613C6E3F11310BA0EFA669D65D947E73478
              039E1DC6D479075ECE81A4695AA569EA2EE70BAD355A6B005A8E5B2EEA5C9E72
              9B6EB74B1CC71445419EE7551CC74229C56EB7C318E33E300802A2284229C560
              30106559B2D96C68B7DB8F11688CC1188394B21A8D46AD2CCBAAE57279F53AB5
              81FD7E5F34899BCFE7D5E17000E0783C8AD3F3A6B51E7E93F8C0A67C60533EB0
              291FD854ED49329BCDAADFFA8094B2028494F2A6F7EF7616F77A3D9224412925
              CAB2AC86C321EBF51A6B2DD3E9D47DE07EBF67B55AB1582C7EFC6E5DE36E81D6
              5AACB5E7A1C8F3FCA675FECE26D15A8BF33DE0919C035F79C06B278048928428
              8A5CC7D4DA6EB7AE133CCFFBFF3E01745B740D950080A10000000049454E44AE
              426082}
          end>
      end
      item
        Name = 'debug-step-into'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000001D149444154388D8D934F6813
              4118C57FB39962ACC6861035A2ECA13908E25E4410118478D18282E09F93520B
              2A3979100B2A580A52B55E3C94E8C5520AF65045F02E7810C48354410978A9D1
              68AC514C166224B1D9F93C6896D56E9A7EF0C1CCF0DE9B376FF8D4A6B1C7F4A4
              FA87808B28955660D1A55A6E7973E9FCEE0500DD93EA3F050C03274CF3E79CF7
              F58311AFB5AC80A955FCB5B227E7DF02277F3C7BF8A27A6F04F9D5209BCDE238
              4E37230068944A9B467DAE4D06701C874C26635624A020E27D2B9A3619A0582C
              92CFE7FD2C6CDB26168B75700088F9735974DB1E7A770E70FB5581DAF8311F94
              CBE53A3AD22026B2DEB6B0B4490CDD8CEA44EA0E702512DFF0C9BD7F3D880DFD
              1D4B84792BBA7647DF910B205E03A8014FD6ED3FBD257EFC52106BC25A033794
              62BAEFC0994131DE4BE01C30F15724238BCDD2720E74CB2D4FE9F846801915D1
              6905968000207277F5F6BD03506A3B58FA84F2D8516A4F67A79AEFDF6C6D5516
              B48858884C20F20E38DB2CBC0E66B0A42DAFF299EAF465CA570F238D3AC02DE0
              20B0AFFEFCD1C7EACC68D70CFC52BDB15540A24DFE3E390CE23B0FCF20B87167
              AF35D7EC3A34B8F8A580FB603C48EE98810A3BFC7F161CC721994C8641FF7510
              24AC74167E039089BC5FFE11B3080000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000026049444154388D9D944D48D3
              6118C07FCFFE1BB335E766D9D20E16A207CF03F1D6414388E8DFB122DCBA74C8
              28A8601E22351B04D1A50E5D4423478B903A042159DAA1208AA0F2108630BFFA
              A0D4D91C7EFE793A4C6D5A9BB307DEC3C3FBBEBFF779F9F13CE20FC77056064A
              405A812322B20B10F28FD0CF8E8B77522F7B00B03B2B033B415E012920ACAA23
              082AF9333F652676903620A5AAB5D6E4F85CF279374B13436C7318F87CBEBC88
              C553C3149595AD023181666B727CEE6B9B89A6120034982691487B39B037DF52
              D340113FAA23C96777D7601911042E6F056893B4005DFAF2F95FFBAD806D2BCB
              0E2022E8D242FA05B70FC79E2A9E0C8EF1A8BA7A1DDD344D22914823D098FDCB
              80AA6273FB10A70B7FF83E8ED28A7260F4D7D34E4DC4DA37DE89032FB2021516
              800267550D8BF18F384A2B0A81D7408FA73ED8046C84C68181EC15AABE070EB8
              F71FEB5DFE368C5A56520CE33CD005A29EBA609300D37FA0417288B22B5C1391
              98CD30DE151F6F89AA5A005140043A5544DDF5A1330A303F086951ADB92A7C08
              3423D201B489CD3E8AA2000A33889C1655CB53173CC7E30B9016925DCA74B405
              EFD14B37C4B03F103888B2DACB02540B580A6F32EEC4C92565B6BF9BB90FFDB8
              020D6336E7F6DBAE9A4338FCFB40E416223B54350444A76257C1BD061CC8FE65
              C09A9C20D9DB81E1DB8DE7F059809B20A7500D297427EE5D61B6AF0B4C133691
              625B9F1920B8811AD093ABB0645F57E6A99CDD63CF3C69CDFC6071E8EDACB332
              500B5889D85F30D84CCABA6C7991EFD74F60149558AE0227853A4FE1CA5802F0
              7ABDB08994AC5374A56FFF637CE58E205B1C5FBF01E6C7DB7067A152EB000000
              0049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000317494441544889A5954F685C
              5514C67F67EE7B21F3EA983609680445A81BEDA2680B55545A9BA0126905214A
              196CA3C4D09504AC2EAC2460EB422814A2AE8A1B43B42E6B8D8A8B942C546422
              51544468855688DAA426C6B6F35EA6799F8BF993F997CCD49CD5B9E7BCFB7DE7
              7CF7DD7BACF3F028C99DBD60B6D7E008C683883633330049AAE7AF913B9DFBF3
              B7F4ECEB3DC54FF0923B7B31B3D780B780DF11E318F342324A58CDDA4FD5010F
              B3C70AE01F03034261FCCF1572B3E75174FD66095859F8AB626D77BD7F6102B8
              17B86FE5EA62F8F70747C97EF70500BEEF33343474D324551DF010625C520578
              91A0BFBF1F331B019044D91154ACEBF992F0C0DA40F3F1D27C0578B9491A2E07
              ADCAADEB7B062650EE8FF36BB669666E031D905F44D91AE0300CE9EBEBAB8927
              9349C6C6C60046D6ACAA609E0ABD98F38BE5D2FEFC3192DBF702309BF98CC5D3
              C72B360541509460B8B144F956CC756DCD57F7C0136CDA7D008397811F533D2F
              9C334BB0F0D1316015A0B0CF3594486809E8F4DABB68B96707E67C0C3A303B8A
              9434E3E954CFA1730216AB489A9208F10DF014D8AB1D0327C36B53E308AE98F4
              24F025C619C4BE54F7C1293358F8F0CD72091A4A94109C00EE3438E577DED1DA
              F6CC2B10DF0098011E4784C05933DB7D4BF721361F1806B3A21CCECC5CE12FAB
              F101E7214D62F606C671B087CDDC04C67C99125F61B61FF8C4A4FDB7761F9C5A
              9AF9BC7989962FCCD0B2F5FEB731CB145ED374E135AD68D520257809982251BA
              0BC37551CB2C71F9643FFF4E8EA1E8FAA4A45EC56A079C2417C7F2815348483A
              2378315E0E89AF2E16899D2427C9D5F301572AC55A37E1DD763789648A8E8113
              785B6E4F80BD070C026781E7E2E56C34373A88BBF83DD3D3D3481A69EA260328
              BC46EEE2CF04BBF6E1B67419F0AE990D4A9AC88387D1DCE820D12F5F130441D3
              127975A3C666C41EA14F05CF2ACA4673EFE4C14B05AD4A541DABF06B086E5CBE
              04B9E505BC963D8825E56AC1A174931B4A547726B66E7B94D66D8F0090FD6192
              E8D76F2BF2411014CF60E57F11146DAD89E6FB3EE9747ABDADAB9DAE970C8280
              4C26B3D189D6D83632D1FE03EAA4DCA5777B15600000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000301494441545885ED964F6C8B
              611CC73FCFDB77ED746989AE76A0B39DA99BDB76A8888BD84E73902544252638
              488889B0CC61B24CE2FFC161823818078A840812443858B2C87A20D9D008D6A8
              F65DB7BDAFE99EC761366AEDF49D9603DFD3FBBECFEFCF37BFCFFBCBFBC2BF2E
              F1E38DC35F8D675533FAA21A8410F972F24A8EA64874EFB195A34F5DB857AEC5
              17EE728B32D73EA059081100343BC532A9A1C5C07BDB061CFE6A7C5B8EB885EE
              BC0F58C066A5E44B399C904AC9828B49E3E3909DE6D3063CAB9A11BA731F6029
              39B1DA881CCFA4EF5D409969BBF56C4B07D017D50034039B8DC889CCF0CDD325
              6F9C65400881102220A57C91BE773E6F702010A0B5B5B5288D63B1189D9D9DDF
              5F424053A32935DBD83D1E0FA150A80D68FB5D03D1685403B20CA026BECC9A64
              5916D168B41D68FF5D03030303CC30F02B0D0E0ED2D4D494F5ACA5A585603068
              DB4032992CCC4045FD7A5CB52B30A38F317B6FCD380F06838442A1C277F59B72
              21509AD72FD09D90190740F3FAF06D3ABC04385451BF7E7BA2DB698E3D8D6415
              8AC562D3C5EC280B8142A194FA2084A871AF5CFB6EECC9D5C9282951522684A6
              550BCD11F185BB1A812C138661108FC7EDF6CF463091FE047005D8BB70C3C146
              99FE84D5FF00399224D1BDDBF485BB1A84E6B82E34C7355FF84823604D99280A
              82D10797F0D435B5034F34B7B7C7BFEB6C5B26FEFAC5C4D06BA5A4448E24C71C
              DECA06E086D0B4882FDCD5A8BE7CB6CCDEDBC541303ED88771FD64727EC3CE3A
              A053C0336755AD9BAADADCD99A637FF9B2BA0366EFEDE220003022C7F8FCA6FF
              A3774D38AC572DDD2A109588C9CFB556B100A13BE7013D80A1A063FCD573A0B8
              5B80D57717ABEF2E4006F80050BEBC1EFF8E33E5C03960584143AAA7C31C7D74
              1928D216E493E69E4FE58E33E5A2CC15017405EB523D1D63E93BDDD331454390
              53652E4499AB92C9696CFBB9391419C1CF92461CE3DAD1B7AEDA151BCDFE878C
              DCBF3823A6A40800866F9C9AF5BCB4080A50491114A29223F895FE23F8A308F2
              FDFDFC3104731D772ECD09C15CC79D4B5308FEBABE0253148C42570F46C30000
              000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000004A7494441545885ED986B685B
              6518C77FEF49D6745DDB245BB31633AA6ECCCE31374DAB9BD26DEE42272868D8
              18DE463757D08F4E68655EC646AD0C654EA6F593D03675758283E0A775B4C50A
              55D64E50983350EC056C686C356DB295B6E6F4F1439ADE96DE7212DD07FF70E0
              E5CD7B9EF33BEFE579FE27F0BF8C49CDF783698D33394F98D0D183FD09DF3E1B
              50339175A08CACFD4731591D194AA9CD2856A9F9DF63318D46067FBFD657B12B
              6140F3742B8DB5276A48DFB4C30654014795522B138E1C550FB0DE48802940DB
              A10AD237ED7000DF016B8053227215082995F00C468CC04D016AD93964ED3D02
              F0399089C8A3136323BDE1A61A466FB61119EC4BFC0913BA71404BC16328CDB4
              4529F58C881CD247C2BD81B38789F83B0D054F8634006D453A28B55B444604F9
              66E852E55D0107B13D18DD636B8001047DE4C72B8BDEB875EB56CE9D3B973230
              8FC7437D7DFDF421514A2911612234888C8D2C1AC062B1E0743A4B819A14F09D
              C9CECE3E0333D34C141222E3CB097405D89344B0987A620DF302831694CFE7A3
              B4B434000466F6DBED7632323212479B111F0C0086C3613A3A3AEEE8AFAAAAC2
              ED766F036C09D3414F757575AFCFE75B3A60E6DE2364143D05BACE5F174F13E9
              EF5A68F879E04903806726AF69400141046DF53DA099662558933D8FD52F9D56
              C087406D6E45C38DC0072F2E04B9D700DC2C99A7F12404D8016529D82E63BF7E
              3F3D4A3301AC028A81974D56C7FEB5150D37FE880379F9F2E5B84BBF5CCDDA83
              2220F083A69415D8637FFE9D96C0FB87A6D28D3E3CC068E7F55BE91B8B0E008D
              4093D9EAD8975BD1F0CBDC993C78F0206EB7BB14B8CF00DFB7D5D5D5AD3E9F2F
              5A49C6BB7E02916B22D2065C485B5760CB7DEB6B2C0F3E119DBDC83803E78F31
              DA797D183800F4A254B366756CCEAD68C09C778761D906EC36704DBDDC944DC9
              79BD868C87766D40D186300CBC2D4823BA1E9E188A6612B5C282969D036043D1
              88900FEC8B0C0FDC0C543E871EECA7ACAC8CE2E26203931795D7EBC5EBF54E1F
              9260ED492CA7BCBF99AC39DB417DA694FA4AA114660D2D675DFC280A44E4AAC9
              EA2830AFBDF7B61EEC27180CD2D767C0FD4C2A140A01334EB13ED44F7FE5B3D8
              4BCFF6AEDCB2F3691159AF9472CD75D4223289C649A266F44DE0F6C4E86D005C
              2E176EB7FB23E061037C757EBFBFAEA5A565761ED48301063F3E8639F77ED236
              3CD2A5A08B19663573E7612C1B8B003E554AAD179163C0C5A14BEFF177EF8D99
              A17E06860D00F6C41A71137524D04D24D03DAB2F6BFF51D2A2709FA0D4AB22F2
              0AF045F0CB4AC24DB553E39A9B9BF1FBFD7506E000686F6F9F1F70AE4CF63C6C
              2FBC8B820BA05E43E4B840FDD01C3880C2C2424A4A4A8CF2110A85E8E8E85862
              A98B26EA4C6007C871C0130F0EC066B3E1743A5B3058EAE2DAADF9A40F0F30DE
              79FD96E581A2C7017DEEB2C6D1090C9A855863E99F6BE6344C56C7A21FE22E97
              8BFCFC7C036C51F97C3E96E566888CA3FFB9787E9B2C75FFBEDD5AA6926FB792
              ACA4D9AD84FF32984FFFDD1E5CA2926DB752B5C4DB30568B7B805648C11EECEE
              EEA6BDBDFD0DA371628E2861C0F2F2726CB6F8992425766BB92A29298995B454
              28BEDD4A40AD49C1B9533DB146C2801E8F8758414F856276EBAED73F5FECCC34
              31B9D4940000000049454E44AE426082}
          end>
      end
      item
        Name = 'debug-step-over'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000001BB49444154388D8D913F6853
              5118C57FDF7DAFF4D924140391BEE509A6589166695D8A20BC0E828374D055B4
              5B70715171B16055245A703174B1B8E82228BA28FE412729D4D6162552844A8D
              0E5129D6D6C424F4DDEB605A52E97BE64CF7BBF79EDF3D9723EEE567B475ED1A
              06CE2192165034A9B6F84E954687089378130B2780B3C0715DFF3D137CFBA47B
              77BAF4A4E23816EC087E125B9C8A04CC03C77EBDBAF7FAC7ED114CBD4A3E9FC7
              F77D1DEA6A928D485A57CB33EB668062B148A150D8F88AE779241289AD010256
              F0BDA8D7CD00B95C6ED3A5A844368009D65A49ABB6DAB45B7136149EE05F65B3
              593299CCC6DC58472490BF67CEDEFD74EC3B44CFC101FC3D6E8B2D60B495F214
              CAD6C9E1AB8E9DEC1A7F58D3E7B72FD555ACF4FEFF006358504EBCAFF3C8E969
              4C5005563F56D48B910FCEE0CA93375F96EF5E8904589D43A7CA225C73BAFBA7
              C4899544D423603770B1BDBBEF3E96BD5A9B9F0C05A8B5E5AFB78031E08E5876
              5D4434222711490337B7F51E884E50997E8C38B1398CB981D1A3CA895F009240
              0A385A997DBE527DFB321420CD837BE9296D6EFA3A7018F0CB930F3E2F4D9C01
              135EC8A66EA523D1DE787DB01533C01F9CEC9A6BFEDE0FC50000000049454E44
              AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000025749444154388DADD34F4893
              711CC7F1F7F799DB742E419D5A0ED4C62C2883443D68900B7256971E3BD7413B
              760A12B428B183B7F21611482E3B74AC931D2A3CAC423128EB50A6E5C08AA6D2
              1F9BB68DEDDBC194469B3E415FF8C1C3E7C7F3E2FBFB7D9F472A7AEEE0AC6D2C
              03E9073A44A41C10FE2E23D2E5CB1267569EB3B6D103F20488013DAA1A415003
              70DB20DFA6D8058A1D4ABD696E094AD5D0EC35A045559B534BF3ABCB8F6E93FC
              304DA0B989F3674EB73A0D0236C06EACAD2D3B044CA037B534BFFAE9B289C6BE
              0290EF2FC1E3A00668DD9AF91314A94035B2FC70640303585C5C646262220484
              D6338FC783CFB7F93DE6098882263FBECDD80887C384C3E18CCC344D060606FA
              80BEDC1D02228226E3564F350C8C6D0AFE63D50081FF0DE61C544E30180CD2DD
              DD9D91B95C2E581B5228DB3B5941876F3F05750771D7EFA6B2D2DB27927B0039
              4155C556EAC57017537E6E04C3E93AFECEA163D33F92C3BBDC3A26D97EC45CA0
              A2CBAA9414341C25FE661CC3E9DA065CFF9290B90BAFEDEDDE4434F22DD48BC6
              572C815235347B0F48A9EA89D8D3BB14361D43ECCE7DC0036016E5487C66F27B
              74B0D3126AA872096817911BEE968E6AB13B057809B4017E8451476D6351D9D9
              9B963A34341E7B0104810322F25E445222921691E7889422D22C702BDFDF600D
              8C5E3945E2F3DC6345F7AAEA1E550DA8EA215535515D407501B898B67A87EB0F
              8EEA3AC45544C9C97EECDB771681DC07FCC0E1F4CFD85474B08BC4CCE496E0C6
              779888BCC256EA256F87AF1018457F63F1D85474B093C4CC336B47CE92ADA08C
              036DE9786C6AE1AA750CE017CFEEDA3393477CD70000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000002F4494441544889A5944D6854
              6714869FF7DE9BF9F92C991827496B9A161B111AC13F82D89A2E6C0BA52EAA0D
              08BA282A15DA1A41107F166E34206E5CB94817AD2D4A43ED4E284A2B4D48A19B
              92949A525A04ED4F8211ED98B441994C66EE1C1789659219CD1DF3AE0E87EF3B
              CF77DE7BCE55FAC3B324DBB782F4BAE0306213464A9200CCCCE6C5FEED93EF30
              FDF7AF4451906CDF8AA4A3C0296014A3179131CC84904018001278325C3241E0
              5C24805ACEDDDC22B80A7C05EC336CAA38798FFCD80DFC428EF7DEDACCB371C3
              F990F08D740C5E7486370B5D10F0C2B99B97819781B6F0FEBF53E3178E93FDE9
              1B009C730C0E0E22A938DFAEC7D8372736330B10AF60F49AD99CE2F37422D273
              2B2800A5C032C5C94C59F1300C19181800E89E732908E8E8E878D4C5FFF94A71
              209081E56FDF28A3E77239BABABACAF2CE3986868630B370618B0049582E5B7D
              FF7072A1038195F6558566AF752F6C51C954542349D12D7A925C85852AC92DCE
              A2477B50498BB2A8A67915F1D60D2412310A266A3C16B168B334AFB66186F85C
              2B8DC7BEC45F52D7E061933F4E1472AFD6174F784FF5A52000F2402C58BE12C5
              92C45ADAF097D4D503DF17D1EF9F8D04BB46B261F7F313D70933A3D503CCEC2F
              608D174B50DB7998FC9FC3184C087E00DE1F9BD2C5F323C1CEE264D3F4DD3387
              C88F952FE493E4A7B61D6C42DA83597FFCA575A3C1D226FCBA4624EF0AB01CD8
              25B1DA4B242FB9F6B7C3EC2F0314EF8F470678667606B33F247D2DE98378EBFA
              B43C1F2034F808F85C5227A8D7AB4DD7341EF9022FD5101D4031FC0FEC0DE067
              A007B8038440289806F61A6640A7E053BF368D5FD7181910DCFBE4104B779FBE
              E525DD9B421B814D40AAE4CC5AD076C403CCCECF0C53F4911280BFAC99675EDB
              414D4B1B00F1156BF0676C580B5C45388CED067DD9E17E323D5D50988E0E2855
              6CE5069A8E5D449E3F531C1CB0CDA07FEA5A1FFFF4EC87B010B983B27F51B0AC
              197CBF1EF42DE0307B17E8CF0EF791F9F84055C52B020030C6919DC6F8CDE0BB
              ECB5D9E2116D29D543B10F6FF057C75E8B0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000002B5494441545885ED974B4854
              5118C77FDF9DEBBCC447383315E534B5885E53160842B4D0A48D994B215C6981
              8B8A362DA4A24D05622D0A8222ECE1C60C5B0CB510CD205AD4A6458B810C049B
              8274D49C71F081DEB9A745E5239D199D198BC0FFEA9E7BBFFFF97E9CFF3DF770
              E11F4B160E2C6E2F791575E81E1F2292C833277322C268EB858C00F4DF17CED2
              2A8A1A5A9C92636B02EA44A418D092998DC8D016E05BC60016B797A253379CA2
              5B5F01D340BD52E627737CD454CA4C6836A3234399349F03C8ABA843746B1330
              ADCC78653470CB88F5B6A1A66299CEBF3200DDE303A803EAA381DBC6F88B3BCB
              16373636E2F7FBB3D63C140AFD04101144A4D834CDBE58EFE38406BFDF4F7979
              79E24C56A96030A8E90BC69A9A88A864CB1E0A85080683495FCCD5A8BFBF9F85
              00A8F86C52437373F3B2F7D38D666C6C6C3140BA4A379A3F23485BE946B32482
              74158D460987C3ABF6AD47B01EC1FF1741B20FCE5F8960ADCF82945AF3B32095
              129D0599684500E2C8C375FA2696028F8CF73C5493EF02590358B89C4ACB770B
              BA7549917D67298E0347ABAD3E7F5751438BC35956935D008542293528223E67
              69D592A2787404659A2F018B68964036212C00B63D87B179F77A81E3F65D654F
              66BE7CC4087F9E07880C610C870CC7A1CA4E11EDA48856EB3858D9690C878CD9
              AF7D190108807547099B2E3EDB00BC053E28B8628407FAE243034A99F3BBCEBA
              7D3F967C9713E1390A4399F19A91BBE7A6A7DE776506005050739E8213675D40
              33502B22CE5466A5D4B5D8EBF6CB636D97320700B09754927FAC017DE3365D10
              17B2F8B9965B88E85607D0014415547F7FD43435F1E669760092C9BEEF08EE33
              F7EC92630B00BA82EA48C7F5C958776BDACD570C60716D65F3D56EBB96E5E690
              E2DF6FAE28B7102DC7B61B98218BCDE1D7364C25732282EEF10E129F691FEFBA
              3F1BEB799095E6003F003D0C2E3CFA59D5E30000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000003D5494441545885D5985F685B
              7514C73FE7DED8DA644DD6AEB5954275DD632B6C29EC41981D71B6BE08AD1DC2
              2ADA4E05A74E044D270487136BC70485A9557CDA6456A788F8E043C54E36640F
              0B5D3130BA1465AEB0CBAC4B6D676D69D73FC787DB864C6E679A7BEFD02F0472
              F3CB39F9FCEEEF9C734F0EFCC7254E1F06AA3653B4659BBD288E5F7194CECD30
              7B7EC023B41596DC0BB3AC8AB2CEC394343C60004D22D2881012E77DFC53538B
              992B477D0334CBAAA93EF80D66A4B2113826220DAA3A81329D1F1F5780A39ED2
              E50296771D5E853B0DA45475BBAA0EB1BCC4D2D4785ECE16FFB8EA359F0D18A8
              DACC9DF53B0CE0980D476CC11ABD3179B287F9D173B0BCE4F90FAF0B7025219A
              568E75FB82357A63BC77373A3FBBA661737333C160D057B84C266303AE646B54
              5527547568F264CF2DE100BABBBBA9A9A979DD47BECBC964F2133B06454008A1
              4C03F6B1E6A7269FE000229093248208024B13565E31974AA5B02C2BE62320E9
              74FAE63AB81EC5E3F135D7A2D128EDEDED85BACE2A1B835EABB6B696B6B6B64E
              A0D3859B9F92C9E4CBBE00AEE83270C6A5BD3F77309D4ED3D7D7770677805896
              E5DF11B7B6B6BAF6934AA5FC010C0683AB35D24D9D3C6D5956CCCF183C8EFD5C
              2F5453E0530C0E0F0F934824C68031377E7C2B33D16894DEDEDE7B807B5DB899
              4A2693293F8FB80B973108C40C4F509CF50660B878C5C0450CFE5BBB9548240A
              759D95AB18BCBDED56E1BA7DEDD67AF5BF6EB7BCD4BA018D7005A53B3B009849
              7ECBE26F973C87CAD54D80AA8A9494DED2A0F2C58F29AEDBBA0D58D8B0B3E3C2
              F8DB1DBE4266EBA0A28B40404A4A31C2156B1A98F65A0F3068462A1BAA0E7C46
              A0BACE5F405545955F45E46E4436953EB4774D8385AB97003AB01BCA41335259
              EF27A401B03CFB27A003AA3A2B100FB73C43E8FE471D0D321FBDC0FCCF43D781
              16600CE19419A9ACBFCB27487BEA1228A2E6DD7398A1F02B0247145E42F5C3B9
              5FCEEBDCC859FB9F5EEEAE8A8344761FC0280E6E04BE036A810717AF5F1BF9DD
              E398CC8E85423B1EA3BCB317113922227155BD007C0F4C8BC3084E5557DF6E44
              D88F925188CD8D9CBD78ED9D273D03CC66F1CC8F5F12D85443F891FDAFAAEAD7
              02CF032D0821C79D89AC428A20860A15A27A9F18E645CFE8701860DE51B795F0
              AE4E8AB644B3034CA7E99B59560D6280C807883C8B6A9742FF64FF21FEFAE184
              7F80F968C3AE2ECAF71C04781FD807EC55F874EAF337991E3CEE191CE4D4C1BC
              E1624F50BEE73580F710F6014FA1FEC0410180C1C68701794BE03994A7811393
              5FF4F80207053C8BD51E2C7DA53002F44FFA74E756B5EE180C54D751FEF82130
              4D6687063C4D0827FD0DDA1E61D6BA53E5590000000049454E44AE426082}
          end>
      end
      item
        Name = 'debug-step-out'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000001DA49444154388D8D913D6813
              7118C67FFFCBA589C624F51693436E68C4A170A045E8265C05ADD28220745188
              8A4A045769759062518A0E0E25938452B083A022741015742AA5F831580445A2
              95B67E514DB5B624D7E45E876AB0E612FB4CEFC7F33E3CFFE70F7FC1BC3A1EB6
              72F9612B97DFDADC738EF5405BD355568AC022F028B6EFC4BA44026A639CD881
              D344F71E478BC4096CDA7C0FD80E0C84B6B5DD21A02F965E4DD41550C94B0F09
              265A8E017D289552A0090800220FDCE9A9FD9F060ED67F4230D17214380B1C41
              A4494434448610790B9C2ABD7BF1DF0CFA80F4CFF1DB4FCA5FE73CE01AD00574
              2C4DDC9D298CF63714D0512AE515979E156E5C2039F838041866583A7A92EE4C
              D88D203B871A0B280854E6673D718B2CDCBC5C8AB477A70FED69A53361782476
              373C5E7580789A616A68BAB73C39C6F2E418F31B7A79E9EEAA7EB1655944A351
              5F0165E5F2AF4538FCE3FEF5A7DF6F5D01F16A48D96C16C7716A17AB0E18548A
              9178E7C974A4BDEB7979EE8D2795729550FEF2FE4FA9F90A94173E0FEBCD5B00
              4675C34C050D730DB1343DA5C12C80AF0315304C62DD6768B25A514AD510563E
              E6B9B82384E3387EF7E8956F1F288C9CF75D5691CDD675A0FB0D33990CB66D57
              FBDFB57F067E43DBB6EBA6FE2F7E018B7E9C103B4EBF6E0000000049454E44AE
              426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000024249444154388D95934B4894
              5118869F6FFE5F2D53678446270D4B83B00CBBD8A21B0442B86C562D5CA86D5A
              5810085A2D42C450B1A055414511D1856CD16513A590526644D0A6820A650612
              9D460646F332FDE37C2D9A0195F9C7DF17CEE29CF39DE7BCE7BCE7084B6414FA
              28BD322CC065E0EE6234FC25D45B4F7C720CA7722DEF19001B8023C080E1F6EE
              2A6A7B88E9AB583BD095578864E5604D8CFD01EA80003060BABD55C56B808AE9
              2DA3B0E112EB761E42904AA00810C003DC4224A1AAB58968F8DB78CB81558166
              F1853E0CB7F730705344762C9D54504004BA8D828DC71D1DD9707BAB115E01C3
              AA5AAEAA86AABA54B51AD530AA1F8106CB61302EA013A55F554FCD7F7D17542B
              A64015300004813A6B62341AEAAD77043481A3C0C9D8CF4F44EE5FA4A4E74D3E
              C86B5483A6685DFE5C381A79D08A2F2F1BF24A56078A4881AA46663F3C4717E6
              60313E83CB3C5D90A583ADDB2C778DC7BDDB55FBC891BB944300167F07494C4F
              3175FD0CEBF71E7BB6AF7233FB6BF69C15A1DD318DFF77A88018C97736FFB99F
              C89D3678DF87081DC91AC74DCA6E8F4E00E762BFBEDF0B75FA21FED77677BFDF
              4F57575723D098E9C84F8196ECD2ED4F8ACF3F9E9F7E79036BFC076AC5ECD604
              80215BA0AAB68BC888888CE494575FF5365F0B22A8202B6B8778D192020EDA02
              E39363E12C5FC541443A801E11497DBD954AFDFB26B00FCA15EA3EC1CCDBBE70
              6261B6197493AA1AA4BFF094320695CE89AD9C869256B9B9B9783C9E6563C97E
              800CA1D83A4CBAD9026CB5AB5993C3A49AC810403AFD032152CB1ACAA565B200
              00000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000031D494441544889A5954F685C
              5514C67FDFBC37E3CC1B43C734B18E6250138A54D0D61A91A6505B458A962C84
              D856218A9011BA902C0A22DA0C5537822056838B22A24144709362A4421BA969
              B399A8B4B67661835AB591FC214C1AE29B97C93B2EDAA6C3389D3F7856DFBDF7
              BCF37DDF39977745592436EFA465DF208297819FCCF876F1F827CC7FFE2660E5
              E9352352BE21278A602DD26BC0B0C4F6A6C79F27B5F700A0FF4F102EE53198C3
              6C2750400C03DB9A1EEBE596670F344CE0002896C0EB7C8AE496A7715BDB709B
              D344124D7F03C780BD40AFA4F1D83D1B7F8F2453F867BFAB9B40B18E07599B79
              0FB7392D490F038F00A9929C8D48DDC02266DD989D987AA39BE58B3FD745E0DE
              DAFF319144F20ED0A7C0A395D30C8C9B813E8313A8FE5944144FAE011D033601
              FB80755C699D6310050EEB4AC523062F5AE0132EE6EB267025ED476A270C7718
              9C2C4EFF86DBDA86229188D020D0676647803D16FC13CCBC9F6165EECFFA1D00
              3D981D3538B9347194FCC887107104FA00C80023C0EE30F00B33873214CE8FD7
              5D1CC045BA1BB32F095798FF2C4B7CC31610298CED88AF52AE3DF3502A2CA4A7
              CE10ECEA825D5D8D1108A206C1F2A55F082FCF519CBE08CBC13C6E6C9BB085DE
              3B8B85275AC3ACDBF100B6F57E543260335B5D57C266866B660610E66701087E
              3DCDCCA10CF1FBB64EC7635176BCB20747366076BD686994AE2B61F72A939C74
              FBEAA17F6E0CFFDC189EE771D3ABBB91E494ABACDF01B600B4B8CD69621D9B09
              2E7C7F9DC8F7E9E9E9F94F5F13890443434300D95A3350DB47935F03F79AB1A1
              38FB973F3BF812CB7F9CAFFA91E7794C4C4C60662B7538E01DC13782C3D196DB
              FB6E1B18F60B933F50BC7401F3972A12AC8C7F71ADC74ECD1960368AF43AE22D
              5097E48CC4D777CEB2BE13A14ACA0EE6CF1EBFA6305BD34130F923B1F64D6F23
              E504FB11CF61ACD18DFF37074BF040D55E0291E9775FE0F2E81056581A35B327
              2DB4E6ABD61D33732AE0D21654CB73006755A6E249DC757701AAF96E4517A6C8
              9D1AABAB458DBF8134788BAA2A8D46E9EFEFAFB85FEF2DAAEAC0F33C72B91C92
              B2E52A6FA4BA21072585064A70F95955FC2F1444B8AA3F71B1C5000000004945
              4E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000002E2494441545885ED964F4814
              6118C69F6F766674D7DDCD34FFA459886410AE1DF212882814A8D1A1C80A0BCA
              43E12510BC049AE4A956C2F0E0C1CC435922690941581E2C8C4EDDC2AD883674
              09AD4DDA991DD73FBBB3F37610977535DDDD19F1D2739A199E79DF1FF37CF3BD
              1FB0C332C56B4C2BAF83B5B21E08AB50BDD3DBC9B45E9C3D13050FBEE5EEEF73
              7715F47C16534B2A8CAB1DAF910101101D6582389475BDC73088B822A0E54568
              01399872B87C081C778D994C272D6535CF82531F35D5EBD105C0A26F84FC43B0
              1EBF0C31BF188C17ACB1807CDE413021C5CE80176098A2D0F2B9DF5D57434B9F
              DE270DC0AF5E588E9D46668373173399DA015C628C656CF1EE11F0296D96B2DA
              9BBA01F8BD45C8BC72DBCE4CA609003F00D484434B5F282087A3CDCC6C079792
              2A026C109A6623D23A423FBF27DD3C0260ADAC07E3855600B314564F49C31D9A
              32DE0FA8C188512C2C4576F34301481D20221B11D528A3F71565AC4F3F80B0A7
              00002E00B8288D746AB14599D986ECE6470267B13D05211B44D5F2589F5F797E
              57577360F5376400632C8F40EEF9F1FE7526269AC1CCB63C108888AAFDAF7AFD
              CAD01DDDCD81C8226400C0D1BC44B4BCB0CEA4F9E7A0BC79322D16969E599C9C
              8032D26948F32880155138B4B18B34488FDB0C6BFA4F80ADD4D8D80887C36158
              738FC7931880C3E140555595661480CBE5E21202F0783C70B95C71CF8FADE476
              BB13FB024EA773C3E7C946E3F3F9D600689C3D8B8117D76C40F128D968A22220
              10D10C63AC28ADFCEC4CE0ED404285928D2612812A7901601040CBEEF32DB514
              90B5850F2FE32E24CB32BC5E6FA2FDE1F3F9567620E14009725B47EC8CE3DE61
              6518DD5265EF574DDABCA8A6FCF17BEF35507777B7BE0842D39390863BFCE975
              372A18433B8051213D2703E9399B1650A55FF9006675470000CAEB5E043D9F64
              DB898626715F711378C1CC620E2CB10AFBE716010322D02BDD11E8952111E891
              9E080C01306023D2273D1124B408FF8FE3ED18C73BAEBFA06354C67B80230700
              00000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000460494441545885CD987F6854
              051CC03FDFDBED4EDFF036B639B5E56D4E850A5779EB8F8872B9681AF5C7440B
              12C40403835428A725484479619262B42258CC29F4C31043FA47AEA622045E05
              AE451ECCBC659E597BCDDBA6936D77EFDB1F37E7DADDE6EEDE1BF88183E3DD7D
              BFEF73F7BEF7BDEFF7C15D4E5E2E41F9154BC89FBB001D1E4407079C76B247C1
              E3CF33BFF902F33FBB307BDEEE102E5FE9B49ECF956D8077F12308F2BA203FE7
              CFAB5A3867C717D32A99B5205612D43A846A1CA42D7F5E55D5744A662D183FB6
              8FA1CB916EB5ACA750ED054EA6243F9F1649C974D0B3A8064F65352EC38748FA
              5B5CB34A28A87D1144CA5C22DF23CC02960FFFF57BF4EF3D6BB1FACCE91174CF
              ADA278C31EBC0B977A44E459A0069159409AA602A8029489C80B8A5E42597EF3
              97B668F781971D13748F3E29AB60CE8EAFC8F315D7022D2252A1AA51D06B2029
              A1B48F2723B29A00990FD632F116441DB31B2B58BC3E489EAFF809E004705255
              5728DA69F5C733F6BABCC25224DF0BC86E41025632F12AD03AD8F9A3937E2941
              F73D8BF1DEF7A81B6806CEA8EA73435D1DC99EC3BB18FEE3D7B420A36625259B
              0E00BC2BF046D2B236A37CD27FA299DE63FB9D17F4565623B05C44165BAA6B86
              BA3A92FFEC5D8B0EDE4C0B98B9F4694A361D40F2DCEF80BC6959D616858FFBBE
              3B48FFD1F71D951B151CE16155ED06ED881FDD9B51EE96A0B8DC9B05D96959D6
              56549BAE875AE83F12745CEEB6A0080806CA0D80C1F33F4C18A0C9040A5FAB6A
              AFC2A1FE500BBDD324775B1010040492666CD280DE63FB7015CEBE2A5EE3D060
              E74FF47FE36CCD4D283855AC3E937F3F74AECFDD89AC056F515F5F8F61184EBA
              A4619A66EE828D8D8D949797BFE5A4D038BAC2E1706BCE8223D43AA2929942B0
              7189DBDBDB89C56275CEF9A41389447217DCB66DDB84AF05020156AF5E9D6BEA
              51D26A505571F94A6C27F6FBFDAC5AB56A3DB0DE469A73E170F835F78819AA3A
              0014E0F6925FB124E37F70967401A76DC6FFEF1B6C1791D90AD5456BB67774EF
              DF901AEF73201289D0D4D474DAA620B1582C25387CF522409BAA760AEC9B71FF
              632B4B5FF928D97378574ED3B1DFEFA7A1A1C18E1B90FA21BA01862E9E63E8D2
              6F098FFF818D404844BE3502F55B663E54D739FCE7F929EFBEC9DE6ECC4FB762
              18C6AD1E69A74F9E8AC56275A397B8A77527653BBE3CE3F2CC58416AA28E90E7
              8E7A163C7823E3E2320E852B09F3F233630E1D044ED9108CC3B89DC4B3A88692
              8D1FE02EBD77CC4E827782DD6A3CD712E6E5E095EDCBF0FBFD0402011B6E294C
              D3CC7066B707A366259E05D5B88CC2AC125AD7AF113FF21E0D0D0D0483C10AA0
              D2865F3C1C0EB7A737EAC41003678F3370F6B88DDC00BC84CD1A04EAB2BFB330
              75DE26756320D7471D4CB1B8B2C5C91AB43BCD64241008100C066B81276DA471
              64DC9A8C4AEC8D63F6C6ADC9304D937038DC0AB4DAC913894472AFC1C6C6468A
              8A8AEC9CFF8E44A3D1DC0543A110E5E5E56D4E0A8D63CCB8953BB6A6953BD005
              36DACCBA75EBF0F97C8ED96422169B7C47BF2BF80F1B1E93066AAEBEEA000000
              0049454E44AE426082}
          end>
      end
      item
        Name = 'debug-breakpoint-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000023349444154388D8D93BF4F13
              7118C63FDF3BDA22858452E80162D28103A134278D89134E68C20F4394A44C24
              860563F03F711217C24848B08B68804107771640EC816D881A489B964A0DA5D7
              00B9FB3AF09B01FA4CEFF03C9FE17DDF47704DAD7E3F51C320AC69B82A2A404A
              6C293133193EC6E37C4FA7AFF8C5D950ED76F3766080D148A4490831010C2284
              0E80944960414AF97E616323FD7A7E9EBF967501A876BB591A1BE3614BCB88B4
              EDA994B9529349C429E67701F0FA1AD0DA423477761784AA8E2773B90F4FA6A7
              C9168BA800EF8686E86B6F1F392C1666573FCF7AB2893847D60148095272641D
              90DFFE45EE77C2E30FEA2F02B5BE9FA1C6C6F8DCDA1A4AABDFCF68777793B4ED
              A9F5A598B0F672D7D7722E6B2FC7FA524C48DB9EEAD5F5C69E6010256A180821
              2652E64ACD4DE1CB9094B95223E04DD43050C29A06309849C46F0D9FE9D43BD8
              A569282E5505D0ADD38595232BBF0B42E82E554501400864D971AE7815DB7140
              CAA4D7D75036C0EB6B0029938E942866360BB018D03BCB066827DE45339341F9
              649A38524EDE0D450EAAEAEA6F0D57D5D5D31C8A14A49493F3A689B29A4AB1B8
              B99916AA3A1EEE8FCA9B205575F584FBA352A8EAABE5EDEDF49744E2E413BF6D
              6DF1ACA3E347A0D69768BA6F3C7555DEF11C974A1C1F96108A42B55FE3DE8347
              B43FEE2B54782AC7D2FBFB73CF6766F8572A5D9429E0F5323D3C4CAFAEDF58A6
              E59D9DF4CB588C3FF93C70A98D67EA0906891A065D9A864B5541081CC739AFF3
              D76412475E1CF23FB16AF0850E8027A80000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C086488000002D549444154388D9D954D6B13
              5B18809F7362E2A435D3A48BBA084D9D807111A52E0B6942A572EF42912AE207
              B871E30F904237E2E2EE84BA15112FDDA80B458C827017722D81D08020824C37
              219DB65831D1686A9A49CC383377D12636A6686E9FE539BC0FE7E5FD38821DF0
              48494AD3486A1A61550521362F5C97B26992310C5E160A7CFBFEBD2B56FC7C70
              617494EB93931C181C1C00A680044244B684AB4016487FA856D76F6632DCCEE5
              B05DB75BE891923B67CE707174D48B1033C0B4D5A8072A6B2B34BE560050D420
              C1F0085EC55F056671DD1BFF160AD6F9070FA8359B9DC2BFCF9EE5E2D1A341E0
              99D530134BB9794A791DD7713A539292A18371A263137895BE2C702A63189513
              7373D88E83A795E6B5C9492FF04FEDF3C7C49BF47DBE7E7807DB5269E3BAD4CA
              254AF94542C35AC4E7EF1F1F0985EE59B6ED649797911E21B87EFC38C08CD530
              136F9F3FA4696EEC54AB0E9AE6066F9F3FC46A980960E66A32C980A22053D128
              07422115985ECACDF724DB2E5DCACD034C077C3E752A1E4726350D60CA6AD403
              A5BCDEB3AC4529AF6335EA0184984A691A32ACAA00E395B595AE02F482EB3854
              D65600C6C3AA8ADC3A1F6EB5C66ED88A1D4688B6B0ABC1778100DAC255450DEE
              DAB415BB8AEB22CBA609900D86471042FE3A72A7670949303C02902D9B263263
              18B890F62AFEEA502CFEBF8543B1385EC55F75219D310CE4CB42818FB5DA3A30
              1B1D9BC0D7B7AF6799AF6F1FD1B1098059CBB6D79FE83A1EDB71B06C9B3F62B1
              05CF1EEFB1D0B016292FE7B1ADE66F65474E9C430904B3C0955B0B0BCED3C5C5
              CD597EFDFE3D639188A30D0E3EF1F9FBC7F71F3A1CB11A75CC2F9FBAE65948C9
              FED861E27F9E6EC94EE9C5A279F9D1232CDBFED12EFD3E1F8F2F5D22A569BDAF
              2FB8A1178BD6C9B9398A1B9B23DBD17F1E29994E26B99A4A11D8BBF7C7828508
              42742CD8BA65ADDF7DF58ABF5EBC68EFC22E618B0145E1743CDEFD0500E55A8D
              8C6190D6F5F6ABB6F31FFF13336BA952D3540000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000003E1494441544889A5965F4C53
              7714C73FE796DE0295824477DB744085D82CD640F405F627028B083CF8A4D99F
              CCC4F83079D91E97F8B6B998986DC95E78F38D273783C66421B22562AA3C98F8
              C0C2DACC48A4D20E4B3158062DB3D7DEFEF6D05B2CB30575DF977B7EF7FC72BE
              E79CDF37BFF313B681C7E5A2AFBD9D4E9F0FB7AE8348D1A114A665319B4C32FD
              E811A94CA66A0CA9F4B3ADA989737D7D7CDCD585CBE974081C040E21E2B30992
              C08C8248DEB2AC89FBF7F9211C66E6F1E39D0946BABBB9303848BDAEBB05BE04
              CE026D55125C002E01A356A190FDF1F66DBEBD79134BA9CA04E70706F8EAC811
              10E905C680D67422A6527351D6528BE432EB0A10D7AE063C861F637F88DD2DFB
              048803A7512A3C1E8970E6CA954D124779E6E78F1D43443E01C63756579AA393
              D748FC7E57659F3E917CEE19AA500094E473CFC83E7DC2F25C54A5FF7A84C7EB
              6F74D6D67F8AC8C303861169AAABE3B7B9B917046D4D4DFC7CEA14BAC3D10B8C
              AF3E8EEBB3BFFCC43F7FA7153B20975963F941543C86DF51DBD0781C983EECF7
              2FDC89C588AFAEA2019CEBEFA7CEE97403631BE9153D3A7915EBB98948513662
              A3DC2E5F5BCF4DA29357D948AFE8C098A669EEAF070600D03C2E171F7576621F
              68EB83F00DF2666EC7CCFF8BBC99530FC237005A05BE78AFB5959061A0F57674
              50EB743A80CFD389985A4B2D2A0065A39A5DC9B7965A54E9444C016711710C06
              83685D5E2FB6CE03A9B928D8CAAAD4966A2D2AD980D8310202A12E9F0FCDADEB
              0087EC0C5EB7332FA12CC661B7AE53632FBC00B9CC7AA95A368D6DEC4ABE5C66
              BDD4052FB04950DA222282524A95CAAE6657F76DD587667F97005CBB3CAFD888
              EA288BB104A09996053003E031FCFF9BA02CC68C695968B34B4B2888000BC6FE
              10D835BE894C0165C7585010994D26D1A66331F2966529B8B4BB25201EC3FFC6
              32F5187ED9DD121055BC61AD5BF3F368A94C865F8B17D328483CD83B4C8DEEAA
              3827B6438DEE9260EF30207150A37F2E2F732F91281EF2C5A9290A854216385D
              DFBCC70C0D9DC0E1D479D516399C3AA1A113D437EF3181D328B217A7A6282855
              BC4D97D6D7A9ADA9E1FD4060017858DBD0787C4F20E8C8AE2C8B995DAFDA2211
              A1D1FBB61C1C3A49C35B3E1338034C5C8F46B930350594CD833BB118C1BD7B39
              60181160DA5957FFA1F79DCEA6922AACFC730AF93C229AD43634D2DCDA417B4F
              BFECEBEE15675D7D0238094CDC8DC7F9ECF2656C756E9D680E11BE1B1E66A4A7
              0787A6B9798D91A9207B3D1261E4DA3532A6B9B9A1E2617E1008F0CDD1A3BCDB
              D606222F863EF810D932F405AC3F9249BE0F87B91A89BC146B5BB5840C83C160
              90AED2B305B009369F2DB7E6E7B9974850509547C8BF440105177121B1010000
              000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000450494441545885ED975D4C5A
              6718C7FFEF81735A101594C1F123B59010B5A8139BA5CA1497D82E5D96B826D4
              34714BEF5CB6344D9A5D7BD726BD74BBE88D1FE9C592F54A36ED4D9754CD2C6A
              5AA29D5DA9757E0071625181BA025AE0F0EE42B40544E60757DBFFEA9CBCCFFB
              FC7F6F4EDEE73C0FF05F17394870A54A854FB45A54F13CD4321940E2DB29852F
              14C2738F078F1C0E3C75BB8F0F8021045F190CB8D6D0802A9EAF218498013402
              2807218571002F805900564A69FFA2CFF7ECCEC404EEDA6C782B088707D0ABD5
              E86D6B430DCF9B08704B88843FF63AE7E05F7621E45B43786B1300C09D94405A
              F001142565283CAD8388E5C628D0B9E0F58E7E63B160DCE53A38C025BD1E7D6D
              6D128958DC2544C21DAEC971B8ED53884523FB9E8811B328D6D7A1ECAC112296
              EB89C66237BEBB7F7FABD766DB335E94CEFCC72B57149C48F4606365E98BE9C1
              7BF0FFE5008DC5F63507001A8BE16FCF323CB3CF91AB2A3A2BCD93B75CACA8F8
              793D10D89A5C5ECE0CA057AB3170F5AA84138B1FAC2FCED6DB7FED87107E9BD1
              385942248CD5393B7214CAD21C85D27441A7BB37EE74465DAF5F27C431092F84
              A0EFF2659C64D9AE8D95A5FA99A1C17F75EA74A2B118668606B1B1B2542F6298
              AE6EB31912964D0FD05E5B8B6A9E37099170C7CCC3A39927403C1C8410097794
              E6E79BAE3534A407B86E34821072D335398E70287064F31D854301B826C74108
              B9F96D7D3DC4CC3BDBDDA74A950A553C5F2D44C28D6EFBD4B199EFC86D9F8210
              0937F2B9B9D54D1A4D2A8049A30121C4EC75CE65BC6A87512C1A81D739074288
              D9B417404D51110034F997D3178DA32A9EBBE9C36DAF4400B54C0600E541DF5A
              D600E2B9CB55DB5E8900200420A430122FAFD950646B1320A470F72786A45B00
              00A0346B007BE566121629F5B11269D6FC598914A0D4F73EC82E802F14028097
              390A65D600E2B95FFA37DF7DE65D00FBEA2A0058E525A7B30610CF6DB57B3CA9
              00A38B8B0060516A7460C46CF2DE238B11B3506A740060F96DDB2B11E0A9DB8D
              05AF775AC47263C57AC3B10314EB0D10B1DC982F149A1E9E9F4F0500803B1313
              A0947696D519C1496529490E2B4E2A43599D1194D2CEBEA4362D01E0AECD8605
              AF7754C49DE8A93CDF0AC2A4DED2838A300C2ACFB742C49DE8590F0647BFB75A
              13D6131A1281524CAFACE04B83614892276FC951284BD79D7F1EBA3610864165
              4B2B0A4E691F534ADB3B2C96E8EF491D734A47B4B4B181B54020FA59458545AA
              5036CB8B4F95FA971C1022E10399735219AA2E9A51704AFB18C0E7B74746DE74
              3F799212B7674F38E576632D18DCBAA0D3FD24C993AB8ACED4D6D11845C0BB9A
              B14961C42C4AAA3FC2994F2F41925FD04B296DBF3D32F2E6D6F0F09EF1FBB6E5
              CD1A0DBACD6694CAE5876ACBD782C1D11B0303F8E5C58BB41E19071309CBE2BA
              D188AFCF9D43515E5E0D01F61F4C807E7F28F4ACD766C30F562BDEAF7A8702D8
              91986160D268D0ACD5A29AE7A14A1ACDFC9B9BF8E3D52B3C7238303C3F9F7122
              FA5F3BFA07EE5BCBEE56050BB30000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000005B9494441545885EDD85D4C9B
              F715C7F1EF796C30C1C621B8C604488331B8CA4C5E1AA5536912345221D69B36
              4CA8AA842A95E4A65A276DD226E5B2D2EE72D39B74BD59D36A17EB54B5A9D8B2
              8D4A194D3325A98048E5A5354B82312A1E06139BF21ABCD8CFD90D20C0404900
              5FF577E7E7F93F3E1F5BF2DFE77FE0C76C2FB2136F52EC709067B5AEBA96324D
              46A7A7B7FDDE4F043C545C4C534D0DF53E1F474A4A70E4E6E68B8817112700AA
              D3AA1A5E48A5E683B118D74321AE0683744722BB0B6CF4FBF96D5D1D272B2A0C
              81178166E06722E2038C35CB4D550D015F029F2A74F446A3E6BBB76EF1514FCF
              CE024B9D4EDE3D7B96C6EA6A0B22E7052E8888772E31A19323616626C678383D
              492A9904C06AB3B1C7B98F027709FB0E78B117B94555C30A1751BDDC3932927E
              ABAD8D81586CFBC0535E2F7F7EED359EB2DB8F097C88EA91F1C12091DE2EE6E2
              3F5C00C0EE2AA6FCE84FF154FD0444FA145A6793C99E5FB6B571A5BF7FD3672D
              9BDD3C1B08F0494B0B0E9BAD55443E9B9988967DD3FE29D1E0D73C7A38B7251C
              C0A38773C4C3F778307C9F027789C7E670BE916BB18C3605023D53C924DD2323
              8F0F3CEDF5F2494B0B3916CBEF0C914BA3FD77AC03FFFA2BFF9B9FDD326C3DE8
              F8DD7E72726DD6BD9EB29781F986AAAADBA1789C6FC7C7B70E2C753A696F6DC5
              9197D76A885C0A77FD9B70D70D507D62DC7254498C0CA1A6C9BEF28A06551D69
              7CE6999EF6BB7789CD667EF875817F7AF5550EEFDF7F4C0CE3B3D1FE3BD670D7
              8DEDC3D6642A3A428E2D0F674959A355E41FCF1F3C38F6E19D3BE89A2F61EDD6
              40A3DF4FA3DF6F11910F6662A3B6D0575FEC386E29A1AFBE6026366A13910F6A
              3C1ECBF9132732D664007F73EA14889C43F5E8BD1B9FA3A6B96B40354DEEDDF8
              1C548F2272EE57274F6231569356BD3AE47653575969085C880D06B7BC8D6C27
              73F118B1C12002177C2E97D15055B531B0E9F061503D23229591DEEE5DC72D25
              D2DB8D8854A27AA6A9A66663609DD78B8834CF251EE86C7CFD9FFD6E64363ECE
              5CE2818A48739DD7BB31F078692940FD64249C35DC52166BD63F5D5848D19E3D
              CBD797811E8703BBCD962F22BE995834EBC0995894C5A623DFE77265026D562B
              021580F1707A32EBC0C59A868854E4E5E464020196FAB9A5AE249B59AEB9D453
              2E66ED3EB8D8DDECC05FDA6367B9E6AA0E6B3550751AC09A9B9715D2CA2CD754
              9D5A797D1998324D14C28099B7B730AB3880C59AA6C2702A9DCE048E4E4FB3F0
              E8D1BCAA0E15B8F7671D58E0DE8FAA0EA13A1F4A24328100FF999800B85E74C0
              4BB6B358F3FAE4C2C2AAB66B15B0637010E08ABDC82D765771D670765731F622
              B700573AEEDF5F756F15F06A30884287AA86CB8F3C973560F991E750D530D071
              75606063607724425F349A56B8E8A90E6077B9771D6777B9F1540750B8383633
              93DE140870E9D62D50BD8C489FBFEE25C4C858B26311C3C05FF71288F4A17AF9
              8F9D9D2453A9CD811FF5F4D0F9DD7769553D57E0294D56D69ED9356065ED190A
              3CA5498573C39393E9776EDECC58B3EE99A42B12A1E5D967C7722D96D1BD9EB2
              97D534998A6E7C347C923C7DFC050E1EAF05D53753E974FBEB1F7F4C281EDF1A
              F0C1DC1CE1C949CE06023DC07C61794583D596C7F7FF1DDEF6C94E0C03DF0B2F
              72F0782DAA7A4155DF7BFBDA35FED2DBBBEEFA0DCFC5C1588CA985051AAAAB6F
              0B8C388B4B1B8B0E545A6726A28F75685F19BBAB9840E32F78CAEB4F026FAAEA
              7B7FB87D9BDF77746CF8CCA69385EE48845022C1CFFDFE1E8B61FCD3E62878BE
              F4D0314FDEDE7D2C4C7FBF65A8DD554C656D3DD5A71AB0390AFA54F5959469B6
              BF7DEDDAA638D8E2F0E8704909EF37375353526201B6373C82CBC38944FAADB6
              36BE1C1AFAC1DA5B1EBF590C83F3274EF0EBD3A7A9282C3444E4F1C66FAA1D63
              B3B3E6FB9D9DBC73F366C676B26DE04A684375354D8100F53E1F654E27C0BA03
              4C603E3E3FCFF55088BF0D0CF0F781812DC39E18B83645F9F9F88A8A58D9A603
              A4D2694289C4BAF3961F93CDFC1F0A996D2C5D35499E0000000049454E44AE42
              6082}
          end>
      end
      item
        Name = 'variables'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000018449444154388DA592CD4A1B
              6114869FF3CD47165D160A8532C4125BA3604D2D454818415A219BAE12288294
              2C5CF61A8257D06B180ABD031766AB24109080AB0C94FED04068D1402B5804E7
              9BE3420919C98FE0BB3B9CF7075E5E61042FDFBCE7C3EE972C108A4809B08C47
              ACAA4DA0962214362A00E1FEF1B7F5C3A847A2AAE3D446C40BF2FE7A7925179A
              D18767338848E9463C211C12550EA31E22524A19200260A789474D006B661167
              C1BE7ABBC58B8D0A9E67C92EBEE6DFFF0BF36EF5197FCF2F0050E0D239EFE4EC
              7CB13738ABC789565306DBF5CF730821628A72D37A69C14FA528C4CEB956E7C7
              EFDA5EE72B97493234B10861E3F87B7030BBF56073F96918F54F3F76FB83AA11
              51146704533CB863EB9E31C5F9C70FBB464483BC8FA24D8B602725DF3601ECD2
              9347AEF8DC77638774173CF01CBFA2A34C9238A27663E25427E2CFCF884F3B6B
              C3FBDE3B308033D70B9C4EBCE6C44ABA2EA3AACD20EF33CDC48810E47D8016B7
              FAB6A0B5F24A2E2C17E687431A8358555BA0B56EBB917A5C013A0DA5B5D3A28F
              640000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000022D49444154388DA5944D4B54
              6114809F73BD8DDDE14EDACCE8988252B3B08162C010A4689160E8B2685181D0
              2AA34DD0AA8525597FC05D6E041302C3D683B409A20F6B655444690489E0D8C4
              C4D88CF3F59E164AE55C19477CB6E7BCCF7BDE8F73840ABAFA2E317867CA0286
              816B221201A432EF3F5455578007C07DBB32DA7BF126C070AE501A49CC2F92CA
              6401B48A9050C01F198847471C9F8D47E8040E020C25E61779BBB85CCDF397AF
              C93400E7BB3B87ACCAA0088848CB66653593CA641191168F70F3BAAADD593564
              1BE1DEB0ED7DF5C44E0EB0DF0980083EC7656DBD70E650A34B3A9BC7987FEFA1
              6A2819135C2F96CF968DB902E2F394786BEA1DCDED47C362C905908888605429
              950D6563B6241BA3E48AC5956FC9F4CCF34FDFDB9399EC2C100238DCDCC8D5DE
              2EEC48472C8EC8D35FD97CE8E75A6EC723055D8713475A4743AED3F7F8F5C7EB
              E95C617ACB9111C63E2CAD861EBD788FA9FADB36B0042E9F3A168AB585C7626D
              4DBD2FBF2CAD8A48936D598A52B404ABE7CDC2724D3200A330B7B04C9D583D1D
              4D0DA6CEB23E8BA0DDD156147D6523F84AA656DD06256340F0B5050F3078FA78
              B1A5C1A5C15F9F026E783A6537045D87A0EB4C0A4CABEA4C21F73BB527A12542
              62E2EE24AAE4736BCC3F7BE2EDE5DD323B31BA7593BD0A2BB1D86134554155BD
              4B2D554D8603FE5D99C2013FAA9A641BA10D3ADE1F8FDE06F851C3C80A07FCF4
              C7A3808E17F3DE7C5B61D4F1D571AEBB736873DC576563DCEBB8C2BDB9C4434F
              FC0F164CCD98E0C9E9590000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000031D494441544889AD95CF6B5C
              5514C73FE7CEFB953793B4A371029129AE2A2DCECE90699562C190E24A505050
              D085DBEC858ED0DA14DCD75D375D74A3FE038654A88BE240A90B23868208E988
              38D336439ACE64DE9B99775CBC99E625332F4D83DFD5F7DC77DEFDDEF3BDF7DC
              2B8CC19B8B9FF249E506C019E0AA8894014F5555446498978C13BCA3AA55E022
              F08BEC9F5CC4F0D50F7F929F397106915BCDA73BDE5AAD41D88B000592BF24E3
              983B96A1542C90CF4D74507D7744C09DC8F1CD4A13447EDA6C75DEF9F6C7BBB4
              C3AE8EAB340DBE63CBD28539F259EFB6199B212022E5DF6B0DDA61576580B8C2
              5D24E3246F875D5DAB3510A13C5E202E7B22B6E568E8F62254F1ACB40455D5D8
              D72167841FFC2DA6A90271D9A2BB7CEFA9D91FEFE7C3CDB7004CC6C2763C005C
              3F0740D0ED5DCA08F8AE4D27EC0D4577D7A6EA8AF03AE802484E64F77825B935
              FFDEE75CF8E212C7A6670179368931F2F51BC502B3F949C25E7F4F75912A41B7
              C7C3ED367FD59B33F5ADD6B5BEF2C1D096586560D1C75F5E07780B6119316501
              5755D5CE64A4702CCB2B53FE781B2088A27EF5E156BBF2F3FAC647BF3D687C17
              C187098B04500B388B98D5666BC71D34D46073074B48E18E659C52B1706EE678
              6EF5FCE9D716369FEE2C3D78BCBD883099ACD642B8DA6CEDB8D756EED20E5EAC
              A16EFFB1214B8B73EEF494BF7CEAD5E9F3B5CDED5B91EAFB0076C6281018C1CC
              AFD51AB483F10D95C64544DA41DC5046CCFCEC4B933876E6BE88C88463532A16
              00AA168217DB723484BD3E08EECB399FB74F1603DBCA68A958209FF53AC0C5D4
              3E78514CF92E674F16F15D3B48DEA6D6613AF6A04E1E8C611983E598CB914697
              51508DB8B77213EB791D9BC693622282AA72F3CA67D437D60178F2E81F9E3CFE
              37FDAA380AEA1BEBFC7DFFD73D63FF9B456930038B80E71FCD244F5A95A0A302
              40E05829CFC221E05819803061C45E01D5A85A2A16F05D5B740088AD3888ABAA
              FA8E2DA56201D5A81A4F37AA60A952C9E7BCD5A5C53977ADD690EEC8E33E9EDB
              C3C73DEB05AA5A51EDD3DA7A342A00DCD14817F25977F9DCA9136514E750DE08
              21AA5555AD8870E7DEEAF734EBB591B4FF000BF2C6D49521A341000000004945
              4E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000270494441545885ED964D4814
              6118C77FEF3AB33BEE47081298109B90597B88DA482113B1A3E5A175A92E2248
              A7DA844E11748F0E5DD28B7D5C3DAD910A1E2A8308152125E8E0928165F46160
              983A39BB633E1D3291CA7795DDA50EFE6FC3FCE7FDFFE661E6791E45168523D5
              D4C513F8FC3B500A40657B442F11DCCC12237D7779F57C507F5A3852CDC58E27
              01D36B750071A55430B77410641121B9B2F2BDFDDED5D8A2A133D7C62E607AAD
              5BA90FB3AD032F5EE36496255700CB6B041A0FED6DDD5F5E4AFD99F6362D803F
              588282787274023BEDE69A0DC0829321393AC1B5D8F1E6E260499B47EB568052
              A17C85FF929D7651A89042A107C8F583DB84B200145EDB00DB0006403852435D
              3C81E50FB1DA6F01081F38CAD76F8EA7295AC99CED6CEA40011CD7F5BDFFB250
              3B336FDF10E18816201CA921D1311834BC5627D0AC940AFC6EAAADDABD959742
              446CC75DEE793AF1B6F1596A7A60450361D4C513185EAB737CEA534BFFF8248E
              9B87766B1AFEA668654B43640F539FE7AE4CCFCE3FDEC8EBB1FC2180D86A78AE
              D90038EE32FDE393788DA258C5CE92219DD7835228A582F90A5F0FA1940A14FB
              CCB41EA0C0F2287D3BD74EC342C9328B44446C410A5F8180CFFC23FCD4E17D00
              4937BD54F80A549597EEBA7EEEC4DAB588D8400FD03EDC7BBBF000A6B81FDFA5
              5EAE555A4470330EC30FBA187BD45D78809937296E9EAFDEF0FE3F9F05DB00DB
              00FF01800822625B667EFF48CB3440C4FEB9A26800DC8C03D0D314AD245F1096
              69D014AD44E0BE9BD66F52C648DF1D0ED69FBE14AD28235A51F6D78D68AB5AD7
              6E1343BD5D5AAF02881C3B49C3D9CBF8FC215496F1B929000437ED30DCDBC5D8
              C36EADF707A0E9DB55A107EB7C0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000371494441545885ED975D6C53
              6518C77FCF39A71FEBC63E5A5DB745DA84D5517540A6402296819810C5181213
              B933DEA08982B88B796942BC74263384488CD11BC32D775C70313F364D0C71C1
              3899CCA0A31DE8BED83AEDB652DA3E5E74B544819EAE1DDDC57E17E7E29CF7C9
              FBCB73CEFBBEFF23D8C45DD7C09EC3AFD31C0823220862B7D4169ABFAA327165
              98EFCF7F4E3A75CBDE2C9BBC7E8E9D1AC01FDC1A00E9013A45C4AAACA082B20C
              3AA8CAE9F191EF163FE97DD19EE09177CFF0F44B471F436428BE946CBA36B340
              36AB95F403C0619974B47A7159E625D07DE73F7D6FD15617424F3E0B227DD766
              169A3EFBEA47D2D96CC5E5F234D6BA79FBE0CE2E8FCB71ECD1AEFD1F18768A4C
              D34284FD4363B1359503882F26B9149D02E8364C07B6044504104F32955E53B9
              3CC9541A113C22D813AC0EB9E5B18E05736C0896CB8660B9AC7B41DBE7E9ED74
              E66B8769ACEA7C5355C835232822C1526A2DC885814347DFA763E701405636E6
              020D0F3F422AAB07F6860374B4FAD0123515E57626C35C6299F19978642E91FC
              18E8BC67C11DD35BEEBA068E9F1AA03918EE043E14A11BC4FDDF9A1A20D4E225
              D4E22DCDAE403A9BCD5E9EFD7BE9E4859F7E8FFC7263F65BBD9F645E70CFE137
              680E868302DF8CDE986DBCF8DB1FA4D2998A471543C4EC68F56E8F8403E75ED8
              D1FEF2CDC4F25B530B8B834505FD8130404FECE65F8D5F0C8D50F91055E0EAD4
              3C009170E064B8CDD735194F448B7D93C6CAFB7EE2EAD4FC9ACAE5F9F5CF390C
              91C79BEB6B01A2C5C61B925B14D65A04D0BB91CDAD30CB69990045B35BD5F641
              91FFEF1677635D6ED486A0AA9A56D52A76F0DE8F34E4F702FAB3A2F64F924AD3
              E071B1B5D5E74E6532FFDE739AA6EE6A6F63B3AF7E5ED18FE2D3D7AB27F8D026
              0FAFEEDDD6621A8597A8AA496010B437B5B418FBF26C5FF5045D0E0B604BC10E
              50458189B11F38D7FF0E93E3A3D51304E8899845C7ACCB557C271B82E5B22158
              2E1B82E592137C30496B5558AA8AA249A7F5609A99CB819AB4FBE765ACB46F70
              5BC04F8D736D0F160176B7B7A1CA90DD3F436B626C985D875E3BDDE8711D39F1
              FCEE1D23B16952E94CF1CA12310C21E46F62B3AF3E0EF4C6AE0CDBAA13CBE9E2
              CDFE0B6CD9FE4C1DC871A05B04F77D13DB2A50D534300AF4DF5A4E44CFF41C24
              3A7AB1B82080B3A6967DAF9CA0E3A9E740B015C54B170450E2D3D71938DBC7E4
              F8655B75FF001FEA1FB20BDFFFD70000000049454E44AE426082}
          end>
      end
      item
        Name = 'gears'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000002E049444154388D8D924D4863
              6714869FEF9AE4728D919B045AA781A9C451C610B248051BA911F42E5AAC5890
              982EECA25D74A603AD8B5929B4A47440285D1766D145E94050A4B81067D1F803
              6695D41FEA9D06F1672169093318254A9BE6E6F3EBA6B4B5B3E89CFDF3F09E73
              5EC10B4C2A952293C9BC09441CC7C9BADDEEB781CB858585ACF62202BFDF0F70
              5308F195C7E3F94508F11008FBFD7EC4FFC1994C86F1F1F1573C1ECF63605729
              F58510E213605C4AF9C6DF825028C4C8C8085EAF979595156AB51AE9749AB1B1
              313A3B3B2B4288979452FDC562B1D8D7D717D634ED10900260626282999919C3
              308CFBC033DBB61FD66A350606063E006E02DF02DF0325E001F031F09694F275
              17C0F0F0308661FC68DBF6ED9E9E9E3FA2D16814C0719CF74BA592118BC5DE03
              BE14427C0DBC0B28A5D4A71B1B1B15D7BFD62D9D9C9CDCDEDBDBD3DBDADAEE01
              5C5E5ED2DEDE4E2C16B38113A5D47DA5D47742887780DF4F4F4F7145A351E2F1
              B80BB8D56834D8DDDDC5344D00CECFCF49241200AF2AA57E383C3C7CBCB8B848
              381CFE464A49369BA5657D7D3DA5EBFADDE3E3E3D1A5A525464747999A9A7A36
              3838F89BAEEBDEB5B535BABABA5E0E0402C16030E8F5F97C3F4F4F4FB3B9B9C9
              D9D9191A90CDE7F31FE572394CD324994C3E554A754B29BB93C9E453D334595D
              5D259FCFDF134264DD6EF7B5376B420851AFD769369B689A0620AFAEAE9C66B3
              E90052D3341CC7A15EAF03CFF746534A7D6859D623CBB2A856AB140A851B2D2D
              2D4F745D7F5228146E54AB552CCBC2B2AC474AA93B52CA6B026159167373739E
              D6D6D69FE6E7E7BBB7B7B7E9E8E800A052A9108FC749A7D30752CAD8ECEC6C63
              7F7F9F8383837F12E47239B6B6B61A4031180C12894408854284422122910881
              400060A75C2E37969797AFC1002E80BF620DF97C3E2627279B86612C00D4EBF5
              D4CECE8E1B18360C4300EABF3770011C1D1D313434F45A7F7FFFE7C0AFE572F9
              C1C5C505BDBDBD7B8944E216F0996DDBCFC1007F02646428E792A4AC7B000000
              0049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000041F49444154388D95D37F68D4
              0518C7F1F773DFFB7EEFA7DBEE9C5C8AEC720A0EDC881B6CAC8DD51FDA68E11F
              6327B41425C46890D17F89101331C93F4A5628F8634CFF084BB7091B078930A2
              B60AE4E655ECC26B58C129E29A70B79DEEBEDEF7EEE99F8C6C06FAFCF5FCF3BC
              7878F83CC233D6ECECAC88C8DBC02DDBB6131E8FA71A38027C78F2E4C979F7B3
              6086613C6ADF02A21E8F671110603D306418C6D3830D0D0D747777232255C06A
              C00292C00B2222AABABEABABEBE7A70677EFDE4D4F4FCFCBC01E11A953D59754
              751AF0013F021FD5D7D73F27FF1EDAB66D1B2D2D2DF8FD7E52A914972F5FA6AD
              AD8DEDDBB7D3DCDC4C341AFD5C4476026555F54E4C4C94BBBBBB314DF30B1179
              1D280B406D6D2DC78E1DA3ADADCD14917EA0E838CED903070E70E8D021AAAAAA
              DE04C2C097C06B22724655DF57D54F804611F906F8167857007A7A7A387AF4E8
              1660A4582C6EF67ABD6555ED019645C402C68BC5A2E9F57A7F53D53744E45311
              79515597442400942B954AD3B56BD77E7501A82A40ABAA360C0E0E92C9640C97
              CB9510914911F96A6E6ECE3C7EFC38954AA51E6807EA5435055C50D511C01491
              BA6432891BC0E3F1A0AA1744E4E3D6D6D69A4B972EE172B974717191AAAA2A2A
              950A1D1D1DB85CAEBCAA9E05BE06CA8B8B8BBFAC5AB5CA00465575B25C2EE36E
              6969A1B7B71711D9592A956AD2E9348542019FCF47341A657E7E9EE5E565D2E9
              349D9D9DD5A669F6398E73EEFAF5EB5CBC789175EBD6953D1ECFD8BD7BF7181B
              1B43D2E9F4166003F0D9D5AB57A3939393C46231E2F1389665C9C3870F756C6C
              8C542AC5D6AD5BE9EAEAFA03780FF8DDB6ED747373F363F172013F944AA509DB
              B69F4F2693F87C3EE2F138A6691EAC542A7ED3340FC6E3717C3E1F333333D8B6
              BDA1542A4D00DF5B96B522AF2E11090E0F0FEBC0C080E6F3792291089665A1AA
              83B95CAEA8AA839665118944C8E5720C0C0CE8F0F0B08A48404456806EA0B86B
              D72E4FA954E2C489132C2C2CE0380E8661EC09854243C01EC77158585820180C
              B27FFF7E4CD34455ED277D944B555F0906837B43A1D09D582C46A150209148A0
              AAA7456449554F2712090A8502B1588C50287427180CEE05BA1CC759011AB76F
              DFCE6EDAB4E9A79A9A9A525D5DDDABE9749A4C26C3CCCC0C376FDE34AF5CB9C2
              DCDC1C6BD6ACA1AFAF0FC33006969696CE1C3E7C383B323242369B7D1CCC6432
              B8DD6EDADBDB53A669BEE3388E2F9BCD62DB3677EFDEA55C2E6359169D9D9D6C
              DCB8310FF48D8E8E9687868656608F6E482E9743447A55353C3D3DAD3B76ECA0
              B1B1511E3C78A07EBF5F666767757C7C9C8E8E8E6AC33076060281F34FBADF3F
              E0DF754B4416FAFBFB5787C36180BD8140E03EE06D6A6A3AB776ED5A310C23AF
              AA2BD7FA2F78E3C60DF2F9FC77D5D5D59BC3E1F007C07DE0FCA953A7D8B76F1F
              6EB7BBAEB6B636021CB16DFBCFA9A9A9FF05FF0271E1C5B82894B9E600000000
              49454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000005344944415448899D956B6893
              5718C77F4F6EB5B192F4122FAD891D6DA7A53360AB383F6C2A8AB02E1FBCD0CD
              B905411C2A938203D9978105FD30F1C336441406837929DE70D36D2296517041
              47472646BC4D9AC410E66B9DEDDA267DDBF44D9E7D682AF30AF30FE7CB399CE7
              779EF3FC9F738457D4AE5DBB686B6B9B2222DB81F3A669DE2C2929C166B37D0C
              C4815F3A3A3A70BC2A0040445689C85E557DAFB4B4F42E9005360043401D60BE
              3260EEDCB9A86A0F60022DC531A9F3AA6A36343420FF37705B5B1B9B366D2210
              08001C16918F54F5B4AAEE13911AE06B6036B03297CB75FFEF0C6A6B6B090402
              6F03EDC03BAAFA1710364D73CCE572FD6EB7DB5544CE00075C2ED7B527001E8F
              8770384C63632353A64CA1BFBF9F8E8E0EB2D92C3B76EC20180CE2F7FB015E03
              D614B719AA3AD6DDDD4D53531373E6CC4915E7E702F58F01A150889D3B775259
              5969073688880DF8CE300C92C9249B376F8689029602678046E07B202822CB5B
              5B5BBB015BD155007B80EF04C0ED76D3D3D38388BC09EC079A45440B85C2DBC0
              6500116901AE88884355AFAB6ABB885489C87120AFAA97811922324F5513404B
              6F6FEFA0EDA92B3E97CD669B8F1F3FCEE0E0A088C819114989480AF82993C9D8
              8F1D3BA6C3C3C36F88C8CFAA1A55D59CAA3A81A5C03C5505B8A5AA83A74E9D7A
              A60FBA9C4EE7FADEDE5E2E5CB8C092254B7C8661003073E64CAE5CB9423C1EC7
              E974027431E19641E033E02C3002FC04948948E9E8E8E8441F4C9F3EFD710625
              25251F2C5EBC58BBBABA8846A3A8AA8AC8A49D75D5AA55525A5A2AAA7A028814
              4F6D261289719FCF475959D9BBC0403E9FD7BEBE3E1C4B972E25140A01B881BD
              0F1E3CD0482482AAAAC3E19040202000A9540ACBB2884422048341F5F97C5F00
              E71E3D7A34148BC5B878F1220F1F3EA4A6A6A61FE0FAF5EBDCB97307B971E346
              5931D5B5C09E83070F6A229150BFDF2FE17018AFD78B88C8C0C0801E39728474
              3A4D6D6D2DDBB66D13E073E08CAAA6814C281422994C3E71E736E00F11B909EC
              360C4393C9240E876332785C55D7140A85D55EAF371E0E87B1DBED2493490CC3
              5060B788DC1491E8A41B9F964344EA63B198C66231060606000804029497978B
              AA7E0AFC6859164EA753BD5EEF0F8140804422C1E9D3A7292F2F27180C6A3018
              AC7B26F26406AAAAB95C8E4C2683699AA8AADA6C36744263966531323282AA8E
              014CAE99A64926932197CB51B4E673E5008C850B17CE6A6969D1743ACDFEFDFB
              25954A91CD66993A75EA5EA7D3F98FC7E301D89BCD6649A5528888AC5FBF1EBF
              DF8F8888AADE7F11C4A6AA0B557539F0654D4D8D54575793CBE5B4B3B313D334
              834C74EF6FA669063B3B3B191F1FA7BABA9AD9B3670BF095AA2E57D54500E3E3
              E3CF0064E3C68DAC5CB992050B167844E4CF743A5D75E8D021CDE572B8DD6E69
              686800E0EEDDBB8C8C8CA8CBE5922D5BB6E0F7FBFF56D5D7EFDDBB3778F2E449
              868787397BF62CF97CFE0980FDDAB56BC4E371D6AD5B3706185EAF776D3E9F27
              91488865591886A17D7D7D625916002B56AC90E6E66651D54F809E7DFBF671E2
              C4096EDDBAF5DC5A38264F0720228B2DCBE2EAD5AB343535B168D122EEDFBF2F
              00B366CDA2A7A747A2D128CB962D5387C3F196AA1EEEEFEF7F61811F03FEA30F
              33998CBADD6E5A5B5BA9AAAA1A696C6C7CBC3863C60CF7D1A347191A1AA2B2B2
              32ACAA5BA74D9B96E7257AE23555D56D1E8FC7D8BE7D3B3E9F0FE07DA0AA3856
              575454D0DEDE2E1515158F0A85C256E0A5C101EC00F97C9EFAFA7AEAEAEA6E00
              DF8A4801F85555BFB974E992158944F2F3E7CF4F14E7A34C7C3C976FDFBECD81
              0307181D1D7D21E05FA78A77A9CD0662B80000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000843494441545885AD977D4CD3
              771EC75FBF3E20D2528A520745D9ACAE309E2443C7E64591A270E20C3E9C0EBD
              8D6CBA44F798733BB35B6ED924D94517A32E6166D3B9DB19F0B6380E1B23C882
              F2A81658481AECAF54EE0658E4498A32C3438BB4BFFB03DBB3D3DC34FAF9ABE9
              F7D3CFFBFDFB7CDFEFCFE75781C718B367CF66E7CE9DE8F57A262626F8F4D34F
              191919213B3B9B0D1B3600505D5DCDE9D3A7F1F97C8F131AB2B3B3696E6E568A
              A2F89E288AFDA228AEDDBB772F269389C6C64699288A8DA228568BA2987AF2E4
              49341A0D00C2E322F0C9279FB079F3E6731D1D1D59972F5F66E3C68D9DC03F81
              3040DBD9D9B9CD6AB5929797E70D0D0DFDFDA64D9B6AEC763B8AC745E04E4BDB
              AF5FBF9ED5D2D2C2AC59B30C5EAFF7238FC7834EA7E3E2C58BB8DD6ED6AE5DEB
              9124E9DFFE2B786C0426272701EA172D5AF466454505555555F7E4646666A254
              2AED9224F54E4C4C00207F54E0279F7C92F9F3E7F3E1871F8629148ACA6FBFFD
              567DF3E64D00944A252A95CA4F8E9E9E1E162C58A0D76AB5035AADB6B5AFAFEF
              D134909696C6891327E6026B81349BCDF67A494909004B962C213F3FFFB652A9
              BCD5D9D939FBF8F1E3B8DD6EE2E2E278EBADB7BA8103408FEC510844444400BC
              79F5EAD52F2C16CBEB8D8D8D00A8542AD6AF5F3FAA50289224497AC26030FC63
              C58A1500389D4E1A1A1A9EB2582CC540F9231110040140268A2266B399EEEE6E
              0022232391CBE5FFF1783C3F9F3C795202AAA3A2A202BFABA8A8C06C3603C8EF
              11A14C26233F3F9F77DF7D57D0E97446A7D379E595575E6178783890939898C8
              912347949191914F01AE55AB5691999949757535168B85FEFE7E4646461669B5
              DAF7366FDE6C05FED2DEDE0E8042A160F7EEDD28954A00579008131313397CF8
              302FBDF4D252954AF52FAFD7FBB759B36675848787DBEAEAEA0279070F1EC460
              307CE9F57AFF2E93C96EC9E5F2574342422AB55A6D417373333E9F0F87C32184
              8787E7B8DDEEC2BABABA275A5A5A8069DD2C5EBCB806787E7878787F90084F9C
              38415A5ADA37BFFCF2CBAB9595958C8C8CF0C61B6F38BD5EEF334EA733342C2C
              6CE6F8F8F8CDF9F3E7278D8E8E361D3A7448F6DC73CF919595E50E0909F933B0
              A9BABA3AF3DCB973F7BDB2C8C848DE7EFB6D9F5AAD5E5E5C5C6CF9EAABAF08D2
              8042A11080BCD2D252AC562BDDDDDD5CBE7C394E2E970FE8F5FAE1B0B0B06B71
              7171A3406D6565A56C7474949A9A1A6A6B6B43812C605F4646063299CCAF8FA0
              48494941AD565F9024C9525656368D797782DD6E979293938B7273730F7FFDF5
              D70098CD66CE9C39133E323202802008424C4C8CAAAFAF0F98567C6666E66DE0
              63E0506D6D2D00EFBFFF3E9224E1F178888A8AE2D4A9535CBA748965CB962DD7
              6834CFCF9933A7C9E5720577A0AEAE0E4992BE5BB870A17BDEBC79008C8E8EE2
              07079024093F38C0B265CB080D0DAD031C40EBCA952BBD5BB66C41A7D3FD3867
              CE9C45F3E6CD7B61E6CC99F97979796CDDBA158D46D309040A0608F8D52908C2
              973FFDF453684F4FCF7DEFF1D7D1D0D080CBE55A05144892F4914AA55A9C9A9A
              7A1CD8555C5C6C33994C2D5D5D5D67B45AEDC74949491FF87CBE24B3D9ECE8EC
              EC9CEEA8BFD09A356BF8ECB3CF56B95CAEAACF3FFF9CA9A9A9008842A1E0E9A7
              9F46AD5633383888D3E90C22111B1BCB3BEFBC33042C3C76ECD8E8E4E4241E8F
              87929212262727494C4CC43F882E5CB8405B5BDBFF6A1717172393C9301A8D08
              82F0A7F3E7CF0781CF9D3B97C2C242292222A216B80AFCAEA3A3C3585A5A8AC7
              E301A0B7B7179BCDA64B4949F9E3F2E5CB8FF4F7F7E3F17830180C381C0EEC76
              3B76BBFDBE1D94994CA6F359595983B1B1B18353535339369B2D703863C60C5E
              7BED355F4444C40697CBB5AAA2A2E2F589898924A3D17874E3C68D4185AC562B
              82201C4C4848185CB162C5606E6EEE6E93C9F49B57A8100421ABA8A84882E99D
              7EFBF6EDC0617272326AB5FAD4C0C0C0E9828202868686484E4EF6959696BE97
              9A9A5A60369B35E3E3E30088A24851515128101A1F1F4F4141C10BF7B3E23D1D
              00181B1B636C6C0CFF8EF64764642480BDA2A282A1A121006C361BADADAD1382
              2074DF5946C0B43BFC75DC6EF76F02DF4D408A8E8E263A3A1A9D4E1774E872B9
              00D2D7AC5913384B4E4E263D3D5DE3F3F916FAF73E4C0BD55FE70EF1070A8524
              49DFEDDAB52B19C0E7F325ECD9B347E91797CD66E3C68D1B79D1D1D13BCACACA
              8E343737633299662A95CA6F9A9A9AC2EE7ED2A4A424B66EDD3A0CF44D3744AA
              9424E937090877BB40AFD797959797AF6F6E6E0E24444545F1F2CB2F131313D3
              CEB40B16B7B6B64695979707B965DBB66DC4C7C7BF7AE5CA9512BF0B8E1E3D8A
              C3E1F8FF04FC1F5E7CF145F6EDDBF7C2AD5BB72E1C38702060317FC4C6C6121E
              1ECEC0C040D0640430180CECD8B1C3294952C2EAD5AB3D0F3AC4E0AE49585555
              85D3E9B44444447C595050804C16FCAED2DBDB8BC3E1B8075CABD5B265CB169F
              24493BCF9E3DFB50E04104A6A6A6D8BB772FC007898989137ABDFE810A646464
              A0D168AA7C3EDF8FFBF7EF7F28F02002004B972E05D8DEDEDE3EF3DAB56BC0B4
              15E3E2E2FC6F30A8542AE2E3E3031DAAAFAF676C6C2C572E97A76467673F3481
              A075FCECB3CFCA80BFD6D4D404BE5BB76E1D0909095D400C100ADC0026CD6673
              B4C562C1ED76D3D0D0205FBD7AF51EA3D1183C1E1F20823AE09BFEBB525E5858
              28A5A7A7B360C1021212127EF67ABDCF747575A9AE5FBF2E773A9D3A20372727
              C7ABD168C8C9C961E5CA95E3C0D9877EFC5F47464606959595D86CB625A2285E
              6C6B6BF38AA2F8873BDA98662C93F1FDF7DF238A62715B5BDB94288A25369B2D
              F6871F7EC068343E3207424242D8BE7D3B4D4D4D82288A4F5DBA7489989898A0
              9CD4D454AC56AB5C14C5B8FAFA7AD6AD5B778F6B1E34FE0BF3AD8C28E5BE06F9
              0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000AF3494441545885B5987D5054
              E77AC07FEF9EB37CB8B2D905960D02C14541BD382B108DA80414C61A13BCE646
              EB8DDC26632BD738B5B1B19A6986B6A669276608686E4C8DE32D3326BD458D4D
              BC6A031A4188D750A6462428AB3BC6AC04E45316705D765D817DFB47640B421C
              F3C1F3DF79CF739EF33BCFF3BECFC7114CB0288A425E5E1E3367CE04E0EBAFBF
              E6830F3E0020343494CD9B37A3D7EB013875EA1467CE9C9968A4FF97F4F474CA
              CBCBB1D96C469BCD5664B3D9FEB9B1B19179F3E601F0F2CB2FD3D8D8F8A8CD66
              3B6CB3D97E63B3D944494909717171011B6222011B1A1A5055F5B7C09B4EA733
              223C3C1C20C7E3F17CDED7D787D96C4655D53FF4F5F5E5E9743AB45AEDFF022F
              D5D6D65ECACFCF07403391808AA2840921F6D6D6D6461417175357578710E223
              9D4ED7161313D3ABAAAA1DC82B2D2DA5B8B818A7D3391F785D5194800D752201
              8786866E0B21FE2B3A3A7A8DDFEFE7B3CF3EC3E3F144B6B5B5E1F3F930180C7A
              4551686E6E263A3A9A7B1EDE37343434F1808AA2D0D9D9494C4CCC518BC5F2EB
              B8B838D9D2D2425959D9B8FA9999990821DAA4947FEAE8E808AC4F5888737373
              8989893100BB6A6A6A644B4BCB03F54F9C3881CBE59A2284F8D7DCDC5CA2A2A2
              809FF990180C06B66FDF8ED168C462B1603299B6F7F4F4BC5E5C5CCC70D88C46
              230B162C40AFD7E37038F8F2CB2F915202909C9CCC8B2FBE78574A9970FDFAF5
              F6EEEEEE9F37C45959592C5BB66C16F06F40B010E217B5B5B5013893C9C4A64D
              9B080D0D3D0F5C4E4D4DFD656262A2A1B4B414009BCD466F6F6F90D1683C9D90
              90D0939090503F117B7089DBED5EFCC5175F00F0D5575F056E646767131A1A5A
              25A55C06F881D956ABF5AB3367CE881B376E0070FCF871CC66F38CF0F070E6CF
              9FBFE067DD83420800E176BBA9AEAEA6BABA1A97CB15B81F111101F0797F7FBF
              BFA0A000A011B819191919D0B97CF932D5D5D5D4D7D70388077A70F9F2E5AC5C
              B992E0E0603A3A3AD8B97327DDDDDD63F4727373870F0580DF6030B076ED5A00
              2A2B2BB979F32600ADADADC4C7C7AFD6E974EFEED8B1C30D2C07A25A5B5B03B6
              162D5AC4638F3DC6E4C9939152FAC7054C4E4EE6B5D75E232D2D2D18F85B6011
              F0D7AAAAB6BEFAEAABA374E3E2E278E38D370809092900E600FF1E1212B22725
              25250878CAE974C69D3A750A80D3A74F939494648D8C8CFC066802D2AAAAAA02
              1FA0D168C8CECE262C2CECA494B205B834E614BFF2CA2BAC5FBF1E8D46F32BA0
              C8ED765BBC5E2F2693E99094F237797979747474A0280A4EA793A2A222727272
              160921FEE4703898366DDA9094F2F752CAD785106BFBFBFBDF2D2A2AC2EBF502
              A0D56AB15AAD8153FCEDB7DF06DE9D9E9ECE73CF3DE792524E3D77EEDCADB6B6
              B6B169A6B1B11121C41F8786867E5953534365652566B3994D9B364960A194B2
              5E08619652AA4208A794B25F0871AEAEAE2EF5F0E1C3242525B162C50AA2A2A2
              7A815F01EFDBEDF65F7CF8E187F8FDFEF10206C0942953D8B87123C1C1C15B7D
              3EDFEF962E5D8AD3E91C9BA8A5941A20ABA2A282B2B2327C3E1FCDCDCD5CBC78
              510027351A8D5B08D1ACD1681C42883E21449BCFE74B3D71E2040057AF5E65EF
              DEBD0C0C0C18811940C1CC9933457474F4F7C201646464101C1C7C434AB9FBF8
              F1E3389D4E609C52E7743AFD2693A930232363474D4D0D77EFDE05A0BCBC9CCE
              CECE479A9A9AE8EDED957EBF1F9D4E474C4C8CC9EBF572FBF6ED808DECEC6CB4
              5AAD434AF90721C4D12B57AEC8D6D6564C26132B56AC60442D4608C1912347A8
              A8A860CE9C39B1AAAAAE8E8D8D3D3C6C4BB91FD0E7F3F1E4934FD6050707BF20
              A57CE49B6FBE01E0CE9D3B381C0E7A7A7AF07ABDDCB9730797CB456B6B2B9D9D
              9D81E7A3A2A258B3660D4288BF1442B408217EDFD2D222AE5DBBC6B3CF3ECBAC
              59B3EC168BE5E2F4E9D33B636363EB626363A7DFB87143E3F7FB99356B16A1A1
              A16A7B7BFBE163C78E8DEF41B3D90C1004F45B2C960786653C898F8F47511429
              A5F400B7A594CFA6A6A6EE9A3163C6B449932649E02FFAFBFBEB47F483EF3EFF
              FCF37F1312128210E228F07723BB99511E9C3E7D3A6FBDF5168AA27C78F3E6CD
              AC9292120607077F10605B5B1B414141223E3EFE6929E57F0217807D414141BD
              C0512965D9860D1B282C2C44ABD53277EEDC5AAD561B2A84F8472965615D5DDD
              AD1D3B76D0D3D303DCE7C10D1B36A0AAEA9FF9FDFE3F2F2D2D0DA48691A2D168
              888E8E46ABD5D2D5D585C7E319A3535E5E4E424242446C6C6CA194F2C503070E
              DCD5E974BF03020D02404949097ABDFED6A44993B6009C3D7B9693274F8EB2A5
              E6E6E6A2AADF71E6E4E400BC565757477B7BFB98172F5CB890A54B97A2D3E904
              20FD7E3F0D0D0D1C3B766C14A89492B2B232366EDCB816F827A3D1F86D4D4D0D
              4020290378BD5EDE7CF3CD07C40384CD664B01E6DDBB0E1142BCB37BF7EE40F1
              1E9665CB96919D9DED010A805229E56D21C493C0CECECE4EEB9E3D7BF0F97CA3
              9ED9BA752B5151510781E151AD474A7964F3E6CDB2AAAAEA8160C3A20255B76E
              DD7A64606020F055F7C399CD66962C5902F0EBC1C1C1F27DFBF661B7DB79E9A5
              97AA66CF9EBDD86C36D72D59B2C4727F78EAEBEB79FCF1C7D7026B01C2C3C3D1
              68342BF57AFDA70F4507A84208C3A14387A4C3E1F85E25ABD58A10E24B2965F9
              F6EDDB194E01B5B5B51C3C78F0566262E23B292929BBEF07ACAAAA62A4A70A0A
              0A30180C8F3C2C1C3C64CB6F341A01AE74777707E0E03B6F1F387000E08AC160
              989011F6A100EF5589F8C8C84892939347DDCBCACA028877BBDDF267A7035429
              A5F799679E09194E291E8F67D82B01B1D96C2C5EBC385308F1C4FBEFBF7F6EDB
              B66DD8ED76B66CD9C2E2C58B8380971B1B1BC7187FE28927B05AAD816B9D4E87
              94F2CE0F020456C7C6C666DCBB0E1142BC525151312A1D34373773FEFC793177
              EEDCE3919191BFDDBF7F7F19E017424C03F6B85CAE39959595638CA7A7A71313
              1353099CBFB7D403FCF70F49FEEAC71F7F7C4255D513002B57AE04C89A3F7F7E
              EAA79F8E3E689F7CF2091E8FC79491917154A3D1DC02FA8510539A9A9A3874E8
              106EB77B94FE942953863BEC2D76BBFDB2DD6E07BECB83A74F9F7E68C0511B7B
              FFFEFDCC9B37EFF9C1C1C1D29D3B77D2DBDB3BE681C993273375EA54828282E8
              E8E8A0ADAD6D5CC3F9F9F9242626964B2957E4E5E571F1E2C587861A29A36AB1
              CBE5E2E9A79FB6298A9219171737F5C2850B81997558EEDEBD4B575717EDEDED
              A35AAC91929191C182050BBC52CA550D0D0DCEBD7BF7FE28B831804D4D4D2427
              273375EAD40A83C1B0E6D1471FD5373434FC208356AB9555AB564921C45F0D0E
              0E7EBE75EB56BABABA7E34E09834F3D1471F21A5EC009CF7EFAB871197CB353C
              7E7E7DFDFA752E5DBAF4A3E1C605CCCFCF47089137303030A7A2A222B0AED3E9
              484B4B232222028D4613584B4A4A6264DFD8D4D44443438300DE494C4C64F9F2
              E53F09704CC39A969626807F397BF6ECA83D969393434646861CFE2829E5B0A7
              FC2E974BBCFDF6DBA3C683E4E4E405AAAA3E67B1588EFC14C0F1862680A19494
              1466CF9E0D7C57E4D3D3D39152AE925226482933816C29E51C29A5292C2CACF9
              5E33815EAFE7A9A79E1A6EE1BE7F8C7B4819E3C16BD7AEC9A4A4A4B9E1E1E1FF
              F0C20B2F6C76381C41838383288A5223A53CF6DE7BEFD1DADADAA4280A7D7D7D
              6CDBB68D848484BFCFCCCC3CA8D16858B87021414141E7812DC0FF8CFCD7F763
              644C810F0B0B63FDFAF5AC5BB70E5555A709218A8085C03357AF5EAD5BBD7A35
              236786458B16B16FDF3E84107F045281D7A594FFD1DCDC2C0B0B0B27EEAF7D5C
              5C1CBB77EFA6B1B1119BCDC6850B17484F4F1F5777D7AE5D01BDDADA5AD6AD5B
              C7C8FFCC3F45FE0F2BFBA52AF71ABE4C0000000049454E44AE426082}
          end>
      end
      item
        Name = 'certificate-license'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000029A49444154388D75924F6855
              4718C57F3377EE7D97777DF9A7F9E3036D931817C64555445328A6226E5C540B
              01215AA868A98A8804172EEA5650D08548B32BB448B4620A7655A48BEAC68558
              7061A090179A2C62307FCCCB7D2FF77566EEB878EF25C6C6B3FA66E69C3367E6
              FB04EF614F1431D2DDCDE6201814427C0FECAA1DFDED9C1B5E30E6EEB785027F
              168B2B1A512FB60401CF7A7BBD46A5EEBEF1BC81DFA28857BE0FC00EAD39562A
              D16AED83244D070F8E8DD997E532005EDDE0C6D6ADECDDB0E1EABFBE7FEE87E6
              66FE0902622989A564C2F77912867CA675EF26E75C6726F3D7C8DC1C00B26E70
              B4A92902868673394A72657B05252919CEE50086FA73B9A8A1C691001DBE4FE4
              79FBA7958A266AB1D7C384EF33AD542485D8DF15866B1320444B719D9B3F4451
              4A10A2A5BE9600DA3980C90E6B11D57A5D08E7E8B01660B2A6A91ACC19C3ACD6
              CF1BAD9DDA57A97CD4605FA542A3B553FFA5E9F3428DB792796476D602436796
              96D8A6F5FFC4DBB4E6CCD212C0D0A38505BB9CA6C07B6D7C512AF17573F3589B
              E705FD49F245DE181A9CA3CB188E94CB7C13C7649CBB36ABF59D93E3E314AB4F
              594D504ED3FA547DFE308A98F13C4EC4318371CC8CE7319ACD02F44921886BE2
              3506E7DBDBF9240C8F4E2A7560349BE5F76C16DFB924702E7994CD321A454C2A
              D5DFA2D45757F2F9B506AD4A71399F0F80EB3FE7723821E8D51ACFB9279E734F
              776A4D2A04BF5407E9FAE9B6367F7B26B3FA07675B5B39DCD474514879BC2F49
              448FD66C4C533E35E64760BC22C4E143CBCB1C2B958427C4464F88B7169E3D5E
              5C4401BCB596D75ADF026E6F52CADF5DA9FC84100338F78703F16592089CFBD5
              3A776A5A6B0D980563AAB3F161BB0E353470AFA7C70FA5BCB968CC857963E80C
              C3DBD6B94BDF150AE6FEFCFC1AFE3BDE810A68A4A321790000000049454E44AE
              426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000035649444154388D8D944D685C
              5514C77FE7DD9979339337F3C6E0A44D5A63924226016BC5484B45B04D57296E
              D406412AB82D48FD58B82874938D10A92B4144CCB234AD10C18204A11137D58D
              29A58B891A956088213624934CE67DCCBBD7455E9E19C712CFEABDFB3FE7C7BD
              E7FECF15FE1583D92CD70706A8E4F39DC05B22F2325089E50563CC0CF0F17210
              ACBFB9B8C8F7F57A4BBDECFFE9CD64981B1EA62B9D1E1591EB0DCB2ADFB56D7E
              4BA500E86F3639EDFBE4B45E33C6BC5ED7FACE58B5CAFCCECE7F03BF1A1CE4AC
              EB9E02EEFC68DBD94F8A45B62CAB650705ADB954ABF1ACEF7BC0E842A3F1C3C9
              070F88623DC97E2697E34CB1A880A96A2693BDE6BA6D30802DCBE29AEB52CD64
              B2C054259B552F954A899E549C2D1601C610199E721C229136D85E44224C390E
              880C1B183B5328B4032BB91C22726E5929B3944E3F12B6174BE934CB4A191139
              57C9E7DB81711C5955EA40D85EC4B947F6AFFD03DC3D623D67CCFF06C6B92DBE
              49809BCD26C0FC4018626B7D20CCD69A81300498DF8CA2643D01DEDFD901636E
              D9C6F8E71B8D0381E71B0D6C637CE0D6FD7D3E4C80B73736D88AA21560F242BD
              CE88EF3F1236E2FB5CD89D9049ADF5CAF4C3878996DC806F0C21305A2C7E6719
              33F87C103CF55814F19752D44410A0378A18DFDEE662BD8EA5F50D03EF7CB6B6
              66A6D7D713609BD97E3E7E9C6EDB1E0F2DEBC68E082563A4196B296043C47418
              83D2FA354FEB2FFAEEDDA3BEAFE72D1E79B5B3938BE5B22D2233B73B3A4A93A5
              128BE9342F781E16F061A9C4E7850236301486CFA5443EDD8AA2E8EEF6767B0F
              6D11268E1E4544DEAE5956DF97F93C9108A95D6B6C01DBCA18221166F2796A96
              D52F2297DFEBEEA62B7E3C5A80970F1FA6D7B6BB802BD38E43239EE313410030
              07CCC5DF342C8B9B8E0370C555AA7CB5A7A715E82AC5BB870E21308148F194E7
              71A956C3D67A0F380BCC9E0802ECF8B539E97920E28AC8C41BE53203B69DF419
              57291CCBCA007F62CC074F0781025EE96B368F3DAE35DA985940CA5ACBC4C686
              79320C7F01668008089448BA2793097FF5FDDD5B7695E26A4F0F4E3CC7A70B05
              8E65B34F00DF0221C60C19404416E24DBCB81A047F7CB3B90980670C1FADACB0
              1404EDB601E84AA5F87A68884A2ED76F8C195F0DC3C9E52060C471DE076EAE86
              E1EF63D52A3F795E5BEDDF4DA54DCAA0E41E8A0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C086488000004584944415448898D944F6C54
              5514C67F5FDFCC9BD29969B17F6C292596D4D82E903FBA10041461217481510C
              12129048C095A2D19D4B96C4C418121784C49850948444055122A82598B822F2
              CF40885869894DA09296E9BC7933EFBDE3E28DF54DA710CFEA9C9B7BCF77CF77
              BFFB893962613ACDA78B17B3B2B919CC9649DA83B41EE8AD6E19C1EC07333B84
              74F1BAE7F1C6CD9B5CF2BCBA5E9ABDD0934E736A6080C599CC3CC187487B4C72
              FE721CBBED38F105C2900561289985981D3278EF5E187A9BAF5FE7D762F1C100
              0E70B2BF9F35F97CA3A453C0F317331986B259FE4CA5CC12871E0B026D9F9E66
              99EF030C9BD9E088EF97565DBD4A218A6A7ACEC496D656F6757521E9A0A42D5F
              67B37C92CFDBA4E30849009284A449C7E17C26632E68200816031D8F38CE493F
              8A385F28CCF46C4802EC6A6B035801ECBEE0BA76349BC578701870349BE582EB
              1AB01B58B1ABBDBD664F0DC0EA7C1EA4BD480D477239A2B807568D64FE6F1D81
              1DC9E5406A00F676BB2EBDAE5B0FF0782643AAA10160C3A8E330964AC5745469
              999D27EBB1548A51C70169031203F3E6CD3D812003748FA5520F2166EEA89EE9
              166492CAA9E9642081A22A1533EB0FC89375148B4B364B99B5134825E04E7718
              CE494B329F5D778721C09D6A8F7A80D17299EA65867B83808EF8C0FF8A8E30A4
              370800860DB8552ED703F8665CF13CCCECB0CCD85A28C02CE524F319AACC6C6B
              A180CC30B3C3D361C8B58465D4503474F72EC039E0F85ADFD726CF931E429180
              4D9EA7B5BE2FE03870EED8C404C9D96B7EF275CF636B5B1B2DA9D419607079A5
              D2D91A45FC9E4A518A253C23DDF961C8CE42412F178BC2EC32F0EA741495F6DE
              BCC9BD04BD7566F76C2EC737FDFDA4A445926E00E98AA46BE9B4DDAECA776110
              3050A9281DD35436B327301B7D736484A189899A7E0DB30142331C33043B4D4A
              0FE5725C765DEB0B0236168B6C2C16E90B022EB9AE0DE57298E40A7620115ABD
              B16876F1FDC0002B73B90548D77E696CCC7DD4DC6C485A5D2AF1D6E424023E6E
              69E1E7C64630B377A7A6F44CA95400FA477D7FFCE92B572826DCB466822DADAD
              AC8CFD687F45CA25CDEEC9587A13067F2FA9CAD080A16C968A9403F6F7B82E6F
              7776CE4D51A3C4FE9E1E0C9E02767EDBD4C4B8E31880CC6C69DCF40C706669B9
              8CAA321D771CFBAEA909E07560C53B5D5DF4A4D3F500FB3A3BE9715D090E2039
              0130E879CA9B695118AA358A049C064EB745917AC250CD661AF43C05009223E9
              40D671F4C1C2853300335EF45C3E0FF092A47566C6966251006B4A25BBE8BAC8
              2C8CE02C661284AB4AA586E5E5327D4120883FA0A41780CD1B5A5ABEAA032096
              F797511435C5049B83B4A1AF52F9A22F08320657311BABEEFEED956271096625
              836D989D3508CD0C243FF9063300EF8F8EB224F6711FE0C59616B6B5B79F00B6
              CBEC738BE9A11445CC739CD332EB37D80E9CF8716A8ACF6217C0805BFE7F1875
              1F2DF938077B7BD9D1D181E035331BFFC3F787474A25D6CF9FBF0E78D4CC8EFD
              74FF3E5B6FDCC04B483319FF001D5210F1BFC44B060000000049454E44AE4260
              82}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C08648800000592494441545885A5965D6C14
              551BC77F67A633B31FDDEEAE4B4B4D0BB4B0528CC54654C40434BC018D1A8BDF
              9F89516FD46824D12B016362B47A65E4424DDE9BF7C2AF4DDE37BE18406D3544
              810B544AD5887C6C438DC648A14BBBEDCEEEECECCC395E6C6BB076965DF9DF6C
              B267E6F9FF9E67CE799E2368404D42F0AF588C75B1185D9645B3A6015094925F
              5D976F0A0586F279CA4AD51D53D46BFC745B1BCFB6B7EB8B4DF356E00EE01AE0
              D2D9477E070E03BBF29EF7C9DBE3E3FE1BA74F5392F2E20196992699749AD5D1
              E8CDC0CEA2A65DF6AD6571C230C8CD562025253D950AD796CB44A4CC025BC74A
              A5C107464739EA38FF1C609969F2C5E59773A969BEE20BF1C2AE4884DD9108CE
              ACF17C85A4E4F662913B8A4574A55ECB7BDE8E5B8E1FE7875229D0430F5AB084
              60EFAA55AC08855E2D68DA0B03890407C2613C11CCEC09C14FA6C98F86C1B5AE
              BBA145086B733CBEEF8389099C807D1108F05C7B3BF72D5A749B27C45BAF2693
              644D33D078BE72BACE31D3E406C7599FD4F5E1A8A66587F2F9059F5DB0969610
              6C6D6F6F02767E148D92358CBACDE794350C3E8A4601763ED6DADAD4DAD4543F
              C0A6789C84616C29685AF79E48A461F339ED894428685AB7A9695BFA93C9FA01
              36C46200771DB22CDC1ADFFC427285E0906501DC754335667D005DD597D61E6B
              E0BB076936C6DA65D598F50168D5AC3B26028E5B233A5B8DD1A1075432D04180
              262FA2FC735242206AF8042E28184FFAFE45035CE2FB28186F08E04CA5027064
              65F5F7A274D96CAC71D7AD1FE070A1004AEDBEBE5C466B60B2FD2DB8525C5F2E
              03EC1EB6EDFA01F64E4DE12A9549F9FEC4C60B0C935ADAE838A47C7F422995F9
              FFE464FD00673D8FF727268AC0B6870B05167B5EC3E68B3D8F870B05806D8353
              53C5E3018904CE82C3B6CD238B168DB4685ACF1AD7ED3D6259D8751ECB36DF67
              C7D4140929332529773C383ACAB9800D1D08509492EF6C9B7B92C93D71E8D9E0
              38579CD3757ED57508389E4229D697CB3C9FCF9394F2BF52CA479F181BF30F56
              2BB1F03B17CAA63F91E0FD745A0821B208D1FD9BAE733014E2B86190D3ABFC29
              DF6755A5C27AC7A1C3F741A9534AA995CFFFF28BFAF7993335E32F3CA2CED3CA
              701821C43D254DEBDE9E4C7295EBB2B65CE64EDBC69A4DA00C6ACC30D8170E33
              629A0C4C4E2E0FF9FEDD2B43A1FF5D287ECD0AB43635F1C395579A315DFF29D3
              DCDCBDAB3A5E0168F73CDECCE500D89A4A317EDEB8BDD3B6B9BF5038E54A79C5
              BAA347DD93354E52CD5DB5ADA38398AE3F93D3F5EEBDF3C6F2EA6A63390A1C5B
              3DAF61ED8D4438A7EBCB4D4D7BFAE5CECE5A16C100AB42211E6F6DBD04D89E89
              46A9CCDB787D55804160B0AFDA6CFE942B04996AB5B6DF964824D73737370E30
              D0D9892EC48B63869138180AFD654D578ADE6AD643C0606FA5823EAF631E0885
              18338CA410E2C581A54B038D16FC7F634B0B9B138934423CF56E73336A5EF63D
              950A21294BBE5207663C6F7F584A67FEDC5042F05E35F3A7D644A32BEE4DA5EA
              03D08081254B1042BC9ED334A3CDF7B9B15462CD79659E2DFF5723B6ED1C9899
              7180FD7D73C34629AE2E97B9B154A2D5F7C9E9BA09BCF6524707E105FAC7DF8E
              619B61D01B0E2F073A53BE7FE4C9E9E9A659AEE51F47A3910FA3D13980A1CFF3
              794E572ADC9A4C0EF6B9EE4D19A578C8B6E9B7ED22700A9080072C5D629ADD3D
              E1F0D877C5626D80A294FCE7ECD95308B12EAEEB8485608965B13A12B96E8B6D
              7F1A9332DEE579422935F8453ECF19CF43C16097E7BDF1C4CC8CDA582AE5815B
              B2A5D2D759C7A1AC1493D5E6446E819952D795C712824C3ACDA678FC1A21C467
              C04CDEF3BA978D8CE001DFF7F6B2221CFE1968564ADD3C6CDBC3FD274F325DC7
              85A6AEE952568A074647199A9C3CAC94DA0C7CF0E5F43473F9EC9B9E06F810D8
              74309F1FEE3F71A22EF386A501EF747591EDEBE3A1F376F5A69616B27D7D64D2
              69AC06EF917F00EF7E3F4B76D11AEB0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C08648800000740494441545885B5986D8C54
              6715C77FE7CEBD77E7E5EECE02CBBEB208D2B084855D8A0D481B4A2D2118DA1A
              2ABED45015F503866AA0DA4A8CA96922A0C54ADAC686862646516B836D028652
              D7228241AB22BA0ADBC24AB75D9097E565BB33CC303377E6DEE38759C8167667
              6667D7FFC79973CFF93DCFB9F77FCF738531A8D63459E438B484424CB56D4C91
              1BFFF5BA2EDDA9147F4B243893CD965D438A87DCAA258EC3868606EEA9AAC214
              9923224B805940CD60C865E084AA1E028EBF9948F0FC850BEC1918F8FF02365A
              163F9E368D65D1A829B00691F522D20A9010D198610010F57D1C550150D52E54
              9F55F8E9914422F7C87BEFF1763A3DFE801F8D44F8C58C19D4D9F63C11F99988
              CC3D659A7A2014A2D3B6B912087C207E92E731CF75B93795E2B65C4E54F598AA
              7E31E9799D5FE9E9E1B5586CFC006F0F87797DD62C1CC3F814223BAF0402153F
              A9ACE4A86D831449A1CA475C972F5FBDCA24CFCBA0FA859CEA2B0F9D3AC56F4B
              802C0A38D1347973F66C9A2A2A5602BB4ED876E0E96894C4603B4B95E3FB3C16
              8B31CB753DE03309CFDB7D6757173D994CC1EB8A56D9DADC4C5345450BB0B3DB
              B2025BAAAB470D0790300CB65457D36D590160A71308B4BC307D7AD1EB0A569A
              1B0AF1D9499300765C358CC8B66814B7584B0BC815615B34CA55C388003B1639
              0EF745A3E503AEADAD456085882C7EC97118B8E94128470381002F390E22B258
              60C5DABABAF201EF9F300144D65D360C3D140C8E19EEBA0E05835C360C4564DD
              DD5555440B2C7C44C0B9A110932CAB1258763818C41F436B6F962FC2E1FC8297
              05442A1757568E1E70463088C00211B18ED9F6B8C15DD731DB46442C81051F2E
              D09D110123F9277526C09971B8F76ED6909C339D02AE50CC2F260065D94A310D
              C939A1505CB1CAB95282CAD1909CB912E386551FC044CF1B33D0CD1A92B34F0B
              C48D0838E0792874014CCF155C6459BA9E53558FC70AE41F11B03B9D06D57FAA
              6AFF7CD71D77C0F9AE8BAAF6039D2753A9D103FE279DE66C26E301BB16A6D354
              FAFEB8C155FA3E0BF333E1AEB4EF7B7F4926470F08F04A7F3FAAFA4C50D55B55
              20C968B52A9924089EAA3EF3FAC000C9028B2F08F8DCC58B647DBF5B61FBF254
              8AD6716875ABEBB23C954255B703DDDBCE9F2F185FD08193BE8F6D18DCE5387F
              14587987EB4EEEACA82056A62F4ECDE5F8F6C000B6EA5BAAFAD0CB57AE645FBC
              74A97C4080BF26127C3C1ACDD6DBF63E5B75D59D994C55AF69D2679AA3826BCF
              64D8188B11F1FDFF02CBCEBAEEE5CFBDF30EA922F776491340AD65F1BB96166E
              0B85A602BB1199772818D45F47225C2EF21AACF13C3E9D4CB2249D16543B8195
              975CF7F4F29327F34E5144258F28B5A6C9BFDADA7002819502AFAA080AFCDBB6
              E9B46D7A4D93A1A7BA0FE572CC735DDA5C17014415557D50E1370BBBBA78BB80
              B50C55C97D6A0B87A9348C00B0E99C69B2A5BA9AA5A9148B3219BE94480CBBD0
              F38180EE0987F97D28C477060668C8E536A3FADA22C7F14A052C690703C09F5B
              5B690D87D78AC8F6ADD1A81EADA8B8F1FF04CF63592AC527934901783512D1FD
              A110EF0F69FF1D990C8FC762A2AA5FBDE0BA3BE61F3B46BC046F2DE9717CB8A6
              86D9A15025F0649765E9D19BE6C3F70301AAF2C56240BC4AF50370007FB76DDE
              B22C059EACB32C677D7D7D29A58B03460C83279A9A10918D2A52F773C719F62C
              DC9EF7C803C01FDA873B4A8A30786DBD886C5CDFD040A3658D1DF0D1FA7AEA6D
              BB19D8703818D4778749DA90CB51EBFB0274001D75BE2FF5C30C003D96C5E18A
              0A051E0D8A4CF96E53D3D8001B2D8BC1566C7245422F4722C3C60DEE9EAA6A87
              AA7600DA3EC25BE7578E43562484C8F756D7D43037142A1FF089C6468286311F
              91D5FBC2E15BBEBF0C0554D56EA0177857554F8D04782510605F388CC0E745E4
              F6A7A64E2D0F706E28C4EAC9931191A7E322B23B1C1E36CE5465761EA6A32F9B
              E5487EA8E868CD663175F8517477384CCC3004F8E1DD55552C2F70781FD10737
              373763883C2022F7F822FACD580C15C1034E58167BC261106156364B1044A1E3
              8D789C73990C0B1CA723A8FAB5966C56BB6C1B545979ED1A2DD92C01064D5B04
              11B95755EFDF3265CADE8E113E248D083823EF734B55F560B5E751ED791679DF
              8CCE77DD5647557F19895C6F6F5A550F1D8CC5E8755DBED5D87810C8B4BBAEDD
              65593C9C48F0402A25AA7A1C88030A6407F777E9CC5068EFA877F04832C969D7
              DD8008B5A649D0303081C6BC073EFB896BD7BE1E50D539F9F61E06AEBD118F13
              CBE548785EB2D234FFD4EEBA1FB3545991877B0ED50D7DD92C69DF27079CCB66
              41954267921101D7F4F40CFBFB53CDCDACABAB5B0F64EF4BA5BE417E3B3AFE91
              48D03F682DFBE3711E9C38B1635A2E77EFB45C0EF5FD1F293CBEBBBF9F353D3D
              8CE60836EAC16EE399333C7FE102EAFB8FF9BEBF1540553BF6C7E337620EC462
              90B71B7CDFFF81AF5A161C94300F0EA7FDF138962A775555ED07FA81BD9BCE9E
              E5F4A0B5F4E772ACABABEB03FA05BEFFE2C58B3CD2DB3B6AB8316B5D5D1D5D6D
              6D1C9D33E79695EE6B69A1ABAD8DCDCDCD63AAF13F7E861D26B3F4F682000000
              0049454E44AE426082}
          end>
      end
      item
        Name = 'heart-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000026149444154388D95905F4853
              6118C69FEF9CB5A66ED333CF36DB263677CAB53085ACAE0229BB48A38250A737
              932EFA73117451377A1181DD157413455746B0F08889D0922C82BAD0C44471A1
              459A6E6EF96F9A2E6D733BE77C5D68E59F09F95CBDBCDFF37BBFF779499920E0
              596DAD3A5DADBE41000F08B180D2510A3CFC198F3F5E912418B5DA4B00AE8210
              0728FD0EE0494292EE5E696B4B90507DBDCA909EFE722E307272BCF73D628B0B
              D0F226E41F2B853EC7260240742A54F5ADE71D9622D348CBE4B0F7C87164E709
              6FE2C96405F9D5D878ED4768ECBEDF2762BD08C3E2D0996A00C0E08B665045DE
              F05E585105CE66BFAE025017E8EBC2665145C6D0EBF6BFF56605FABAC0D9EC75
              2A10E25C9A9DDA620080646C39651F009666A700429C0C289509C36C6BDC4E84
              61004A650640BF3EC7BAE3016B4C3F03A0798FB368C703D698662629CB4D7C7E
              C1A4CE6CF96F5867B680CF2F9804A54DAC49AB4D1EB65AC7B32CB955D35F3EA5
              BCF87AB1BBD4282CAF846AB7E662FBD0D020DB130CA2BAA868383B8B3367188C
              25B3A39FB7A70981EBD479E8CDD6470BB1D83DB7D70B362E49E89D9880BBB8B8
              536FE08F6A7499C25C6024255C505A0EA3C3F94A56148F4714958FE130580008
              47A318894494732ED7731D6F2EC9E0784764FC2B40E92ACBB070959D85497075
              52E0C24D9F2FE11D18588DF4E783E199198CCDCF4BA79DCE165DB6713F67CB3B
              3817185DCD5C51092ED72E4A8A52DDD0D1B1F2A0BBFBDF629B372D13043C75BB
              49A646D390882DDF0200755AC6EDB824DDB9DCDA4A5BFCFE8DC952DDEA80C904
              6F4D0DF6F1FC0900082F2EBEF588223E04835BBCBF016F4EE069A2631FBA0000
              000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000033149444154388DA5D44F4CDB
              7518C7F1F7F7D7D2D252A0F4C79F8AB485821D10600B619B87455DFC130EAD8B
              2126332E7171261C7691839EBCED605CA25EBC9898856C8765716C318CE8C1B4
              4DCCA889CE3858B063195D080C584B18B6B4B43FDAC7031B8A1B64CAE7FA7C9F
              D7F37CBF87AF6A7038B876F2249D6EB7AEE063E0184A3D8FC8223026703695C9
              DC5F2B14F0D5D4342AA53E028228D580C87DE03B11393BBFBABA1C1C1E464506
              0739ECF174033F641F2EBB17A67E27B7BA42796535EEF61E1CB50D0F81B7D9CC
              B799D49273313EC17A66155B550DCF751EC0EED41781FE7B2B2B932A7BE68C03
              A5269377E3DE7878142995D88A52F85F3C4A53CFC12CC0DCC42FF6999F2320F2
              F7114DA3FDD51075FEF65944BACD28753A9FF9D37B3B32B61D0310612616C6EA
              A8B403CCC4C2FC3B522A713B3C46557DA3D7EAA83A6D060616E313948A1B4F1C
              7E9CE9E8F73BD6004AC50D16E313F8FA8E0C98516A5F3AB5B46B43D128EC5A07
              48A79640A97D1A60955DB67BD63C32AC1A22099B53DF336873EA2092D080A8EE
              6DDD33A8FB5A01A21A70BEC6D3AC6C4ED71EB67351D3D4AC80F39A400C54D87F
              F8E8FF06377B55188869A3535388C890DEDC66D4B575FC67ACAEAD03BDB9CD10
              91A19B0B0B987E9B9FE7FDBEBE07664D2BB83CFED796137730D673CF84D99D3A
              5DFD03284DFB44E0EA898B1731ADE472A4F379DE0804C63593B9DBE56BED48DE
              FD83A261EC8A59EC15F484DEC162ABB80A7CF8D5F5EB0CDFB88109E0D7B9395A
              759D2EB7FB5A99B5FC1597D7EF4925A677442DF60A7A82C7B155BBC605067E4A
              248C53232388C8260830168FF3724B8BE1753AAF94D9EC2FD5B6043CA97B7728
              16F2DB30ABA38A036FBE8BCDE98A01C1E96432131C1E26F768F81628225CB975
              8B233E5FBEA9BAFA5259B9ED60C30B9DFED585390A6B69002AEB1BD91F3A8ED5
              51F5A38884A693C974FFB9732C67B35B034DFF9C6E148B8C4C4ED25E5F6F046A
              6B2F99CA2CEE864057AF91CB5259E7A6E3F563982DD66F44E4C4F8ECECFA5B17
              2EF02093D97603F5B43732691A5F04837C70E810C0A0822F01048680AF47A7A6
              3875F9326B85273F8DA7828FF35E6F2F9F8742949BCDFB01364AA59B9F45227C
              1A8DEED8F31722A93B42438363210000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C08648800000430494441544889AD947D4CD5
              6514C73FE7F95D51B90817AF206F0A28BB801788365F66C5D4B218C1665AE6CA
              A6366DB5B5D6E6969BFF6A7FE41F35B79AD99A5B7363E0B49AA9F8B21A4D5B8E
              A16E24C30905285C5E1C977B2F172FF7E2BD9CFE109A182FCEFAFEF57D9EF37D
              CE73BEE77976C48870B8AA8ADDAB5783C872543F12915710C9443500FCAEF00D
              AAF55D8100FE5088E2F474800D22F23E226B0107AA1E854BA87E89EA5F97DADA
              78BBB616F9B4BC9CBD6565007B80C363D1E87C5F772723433EE6CC8B27392B87
              B9F60454B55AE1030180A322B23D727F189FA793072321E62725939C9583B16C
              23C0C7C0B1332D2DC8D08103D88C7907F86EB0AB5D5A7F3D4FE47E50198765B3
              91F5CC1AC959F90288FC0C80EAC6CE6BBFD1DDD4A0B1687442CA5CFB0271ADAF
              60E192650AEC54A896D0C1838B11B9E5EFB99B74F3EC0954C7505515117998EB
              215FEC2A227F432580DCAE3FA7FDADCD53EA440CC555DB70642C0D0085464576
              A3EA68BB7211D531A6437F6B339EE6EB789AAF6B7F6BF3B43AD531DAAE5C0255
              87C26E9B40E5507F8F867C5E9D2C547D9CB75FAD0790A9628FF2906F80A1FE1E
              12D3322B0D2205C1813E00641CD3F1B158141D8BCDAA03080EF4814881114888
              8D46A6B5FCB418CF996053D5C1B8787BEA747667E233C5E2E2EDA03A6880C6C4
              B42C44E489ACCB23982146625A1640A3012EC63B9C6277A6CAFFD51EBB3355E2
              1D4E012E1A55AD01EE6716AF44C7316177263E532CB37825C0B0AAD618440681
              2369AE22129CA9FFB94509CE54497315017C0D0C9A86BB77010E21D2E75A5781
              18F3D4AD116370ADAB00913EE0504F3088F5A7D7CBF6D2D21181CE790B12DF14
              63E1EBEE64A2AA89CAA6E28FAF73D7AC97D4BC4254F55DE0C6BEBA3AAC2EBF9F
              254949946664DC5248494ACB5A150EF864D87B6FD2779C0D8B5D6E59BEF64514
              8E005F5C6E6F67FF850B5800973B3AA82A286091DDFE8B883CEFCCCECB1DBAD7
              2B9160E089DE20392B57566CDC8418538FEA4E6F2814DB7CFC38FE701803108C
              4478ABA6067F383CAAB0552CABC95DBE054766F6AC953B32B371976F412CAB49
              61EB83586C74C78913740502000F1D00784321AE7577B3B5A4246C19F383B1AC
              97539617A4857C03F2F8209CC0A25C97B8CBB760D96C4D0AE5AAEAFDF0F4697E
              6A69F947633D7AE08EDFCFCDDE5E5E73BB4396C84963ACB29465F94B1E844332
              3CD03FA94519EE67257F7D25C6B2AEAA6A8582F79373E738D6D838A988491700
              B479BDFCD1D3C3AB858523736DB6932252E4CCCECB9F336F3E7ECF1D1111F29E
              7B899C5565222267517D7D34160BECABABE36843C3BF5C4E3B1E4AD3D339B36B
              170BED7623F019B03778AF570016A4A62BF0B9AAEE8F44A363DB6B6B397FFBF6
              9479669C3F4B1D0EBEDFB183152929009B45E45B00557D0FF8B12718645B7535
              373C9E6973CC3AE012E2E2F86AD326DE282901C81DDFEEB8D2D1C19E53A7F00C
              0DCD78FE6FAE780D74F756021D0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000057E494441545885CD975D4C93
              5718C7FFE77DA9054A29F403E82BC55A28DF4370411D2A8AE046F02B1B4145B2
              0B97A9C9BC709733D98D8959DC2EF6713313D9B898824CFC9A5121030CC28628
              3207012C146C550A58CA37A59F6FCF2EE89C8A25E080ED7FF99EE779FEBF9373
              729EE705FE6391573FF8310CF62426624F521252944A3FB15018098075F17CBF
              7E68C856DDD383D2070F303C3DFD525EB05088BD2929C88B8F479C4211E02F10
              7000F869A7B3AFD36C76FFD2D1812B1D1DB0B95CBE01D22323519C9F0FAD5C9E
              430839064AB739A626FC29A510068979C2B04D143833E970949DA8AEF69C6E6A
              0200EC5BB3065FE5E5317291E800010E530FBFC13135C9128640280AB683905B
              94D2EF4CE3E3359F5CBD8A9A9E9ED900395A2D2E141505FAFBF915DBC647F63F
              F9A31116831EBCCB3913C8B0088D544395BA1E12A5AA99525A78BEB5D5609E9A
              C2B18D1B571342CE8F0F3C4D7FFAE75D8CF619413D3C008015AC807CB516516B
              3310209196F3941E3A74F1E274796BEB3F001AA914778E1E5D21160AAB9E75B7
              6FE9AEAF02E5799FE7C625BF8DE88C6C3321241B0028A5B5BD8DB561FDED2DBE
              CF9A65119B998BF0D8E4DB2E9ECFDD76E68CB3C564020B00C5F9F9488E88F8C2
              62E83EA0ABBD0650EAB310004C9A07601F1F15C935713B011475DDBACE0DEADA
              E6CC01A51836EA219285A9C552857F2AC7D594DCBF0F364626C3D7BB76ADE29D
              8E736D377E663D6EF7DC85BCB28E0C8115AC904C3C3349FADA9AE7950300A3A6
              C7E01253D35786869EAD3718C699BCF878003838A06B13B8EDB679170200C3BD
              DB30DCBBBDA01CB7DD86015D9B00C0C1BCB83830691C0700DB878DFA05150200
              EAF1807A3C0BCEF37A6D4FE3383062A11000E2AD23430B2EF4A6F27AC5070985
              6040084048B0DBE9583600B7D30110120C42C0805280522BEB27583600AF9715
              0018FBCCADEF090C912D1B4060880CA0B4C7E17281D15B2C00D028E1A2960DC0
              EBD5A8B758C034180C0070252C2601733F3F8B230A202C260100AED41B8D60EA
              0D060C59AD7541F270BD242272C901241191089287EBED6E775DA54E07C6EDF1
              A0A4B9990238B57A5DE69203783D4E5D686DA5A3361B1800387DE70E261D8EB3
              12A5AA4D119DB064E68AE8044894AA3627CF9FFDA6A101006600CC562BBEACAB
              E329702466530E2F08085C74734140206236E5F094D223C577EFF25D33977FA6
              1B0240735F1FF2E2E24C2B43A58C5811B1E599BE63F1DC0941726E3E44A18A93
              4FC6C6CA8ACACBE1F4B6FBE7001E4AD16834E2405A5A832454B6811508A247FB
              8C8BE2AF79270B613149BFBA78FEC8DED252DA3B32F27C8D7D31D0323D0DC3C8
              08FD2039F96670F8CADD2EBB4D3E3934F0AFCCB9A4B550A767EA00EC385E5969
              BBD4DEFED23AFB6AC243B3199452FB668DE6864CA529B04D8C89DFB451856993
              109B99DB4F09C92E696E1E3C5153332B6616000034188D504924E36B38EEA642
              AD2DB04D8C89160A11A64D427CD60E3308D956D5D5D5FB5145C56B1FBAD70200
              406557173452E9F05B4AE50DB95AFBBED3362D9E1A1A9C97B932310D715B72FB
              414876757777D7FEB232B87DCC0D3E012880EB0F1F4215126249E1B8ABB255D1
              B984656563A6C7739AABD76542B37E6B372524BB52A7EB292C2B83638E01D727
              C08B1042961DCB50ABCB4394AA0C91541135FCA477D624C4F8099090BD1BCAC4
              D4DF29F0DE4F2D2D03072B2A7CEE7C5E007FABEED1230C4E4CD8DE8D8D2D0D92
              2A38993A266DCCF4186EC7CC0C1920912265E73E847051251E4AF79FACAD9D3A
              5E5535AFE636EBD76C2E6D56AB71AEB0100A91E8638FDBF5ADFEB7EA0000D06E
              DA6E63FC049F4ED8ED3F1CBE7C19D73A3BE75D73410000A09248F063410136AA
              D58904380F0014286C1F1CECFCB0BC1CDDDE2776C900008021049F6DDD8AE359
              59FE0C21F8BEA9C9FE7955D59C97ED7FABBF00FC7649981F485BDD0000000049
              454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C0864880000076F494441545885DDD86B5054
              E71DC7F1EF3967775976B9B3C0C206502E4B44C0A268741A635430460CD09818
              EB4C9B4E92769A49A79DCEB433693369DADA71A6ED643AD3E9C5A6B918A72663
              14C144AD8A2146EC784B836358C40D0B0272935DD96561D9FBD31720D5217231
              513AFDBDDBF39CF37F3EE739FB9CCB03FFE391A66ACC4A48A02C371773521251
              1ACDC476D7E82896FE7EEA5A5BE9191A9AB283449D8ED29C1C16A5A692A8D74F
              6CF7068358070668686FE7625FDFEC800F6765F14A6929CBD2D365499256010F
              0339801EB80E34014704341FBD7C996D1F7E48634FCFA493FB65591995F9F9A8
              15251F580F140009C008D00A9C10F0B1A5AF2FFCDB1327A86E6A9A1AA8C832AF
              9697F3DD65CB244992BE03BC2CC2E14CF7B55E4606ED84837ED45A1D510623BA
              F84409F897809F0743A1863F9C3AC5B6E3C70909C1F32B56F09B75EBD0AAD52B
              25D80E7CDD33E810C3F63E025E0F8A4A832EDE4074722A922C7700DB84103BDF
              6F6E16CF555733E2F74F062AB2CCAECD9BA92A288893E09D7030F048D785B374
              5B3E25E81D9D7466BAB844D28B9793622E40C01F11E2A7B5164BB07B68881756
              AC502149BF97E087FDD626BA1ACFE0713A26D5506923312D5C4CFAD71E4056A9
              8F0AD87ABAA3C359F9F6DB13C809E0AFCBCAF8C9AA55D1C0479E417B71D3916A
              BC43CE2FFA07DC92B8B40C169455A2D6EA0E014F200448D2BE80D7537EA9EE00
              CE9ECE696B6863E22858BF095DBCA11158BDF7E245F7D3EFBD37367000454623
              AF6DDA842CCBBB479DD7D75C38B01BBF6778DAC2005EB70B7BFBE72465E79915
              B5260B49AAF47B86375DA8DDCDB0BD7F4635823E2FD75A5B30CCCB4D556B23CD
              F9C9C97B1B7B7A68753890015E5AB30695A2548870F8714B5D0D41DFE44B3A25
              726890CF0EEF251C0A6E0D87825B3F3BBC17EFD0E0AC6A047DA358EA6A10E1F0
              E3922455FCA2B414002551A7E34F9595C8B2FC8F5ECBA7A9FDD6C9336926098C
              8E10F07A19EC6AE77AA7ED0E6B78D044EA884E4E331BA3A3FFFE417333F2EAEC
              6C548A52002CE96EFAF71D15BE91DEE6467A9B1BBF548D71C312A0A0CC6C465E
              6C3201947A9C0E31EA9ADD65B91B19750DE2713A04505A9C96869CA8D301140D
              0FDCFE6E7EAF336E2932E874C848124892D137E29E6BD744C62D4624696C1603
              2A110ECF21E9D688701824490520230408E1524568E7DA3511558416847002C8
              C1B1916BD5C525CEADEAA68C5B6CC15008D9E670009C1B7F70CFAD0C906499E8
              E4548073368703F97C571702EA154D44202E2D73AE7DC4A565A268220202EACF
              5FBD8A7CEEEA55467C3E17421C36DE5F38D73E8CF7178210870057BDCD86EC0B
              06A9B15800FE62C8CA23222A66CE7011513118B2F200FEDAD0D646CFD0D0D86D
              66C7993308382E49F2F9CC9207E70C9859F22092249F17707CC7D9B30063C0C6
              9E1E1ADADA1042BC68CC2B202AC978CF715149468C790508215E6CB3DBF9E0D2
              A5FF02017E76E408C00990F698573D7A4F67B424CB98573D0A487B80132F1F3B
              4668FCC1A1DCD8A9CFED262D26866293E9A446A77F5A9615BDB3BBE39E00E72F
              7D88A4ACBC01A0A2DE66F3BC525737D176CB30BD74F4285D2ED7808067D28B97
              8B848CECBB8E4BC8C826BD78B910423CE3F6F9065EA8ADBDA55DB9F9872F18A4
              A9AF8F2D45459FCBB2AC4AC8CC7AC8DE6E25E8F3DE155C646C3C85E54F222BAA
              ED48D26B3FA8ADE5D4952BB707025C191CC41F0AB13A27E76345512D4ECCC836
              5F6BBD443818F84A716AAD8E45155BD1E8A20E0A219E7FFDDC39F1BB932727ED
              37090870BAB3933C8341E42727BFAFD646AE8B3365A40DB45E4284435F094E51
              6B28DCB8197D42D2274288C74E7776FABFB5670F4288990101FE79F9322BE7CF
              0FA4C7C61E88D0476F8C4D4D370CD85AF8B2AF65B24A4D61F96662524C2D4288
              7556BBDDF5D8CE9D786EFA589F1130140E73A8A585F2050B3C897A7DAD362AA6
              3236F5BE047BDBE53B1E4945ADA170C393C4A6A6DB10626DFFF0707FF95B6FD1
              3BC5FACE6D8100A3810035160B1BF2F2DC06BDBE5A1B1DBB21FEBE79067BBB95
              7030382B9C4A1B49D1C6A7884931B5006BFB86877BD6BFF106E36F5377060418
              F1FBA9B558586F36BB1374BABD5A7DF45AC3FCDCD4EB1D36827EDF8C70DAE858
              16557C137D4272A310A2ACDFEDEE5FFFE69B58EDF6698F9D167803B9DF62E181
              8C0C4F7A6CEC3B6AAD6E49724E7EF6505F37D37DCBC4A49828DAB88588A89863
              428872ABDDEE2CDFB973DA919B1510C0E3F7B3BFA989852929FEDCA4A43D8A5A
              9D9C625E58E21B7133E2B8F685C7A4E415B2705D158A26E26F02BE7DB6ABCB5B
              B56B17DD2ED74CBB9D391020100AB1DF62213E32325C62321D926479C0302FB7
              4CA38F529C573B10626C86CB8A8A9C9565CC5BBA322049F28F8410BFAAB158C2
              5BDF7D17E7E8EC9655660504104270CC6AA57B688847CCE64F6449AA8F4E4A2D
              4B9C678E75F576A18ED45158FE140919595D42880A0135DBEBEBF9F1C1830442
              B39FFD532E014F97A52613BBB66C21233E3E5E82D7C3A1601580ACA86A053CE7
              181919FC5E753547ACD63BEEE34B010162B55AFE5C55C5370A0A90E0FB000276
              9C6C6BE3D97DFBA65DC3BEEBC01B79B6A484278A8A00F8C866E3D586868977BA
              FFEBFC0735750DE14B1817CD0000000049454E44AE426082}
          end>
      end
      item
        Name = 'alert-filled'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000026E49444154388D8D934D4894
              5114869F7BBF9FF966FCD471C61274B4D4691C1505CB1C15114C120AB21F5A45
              8B7646615010B40FDAB48EC07451EB20C4FEDCE4D6221206179A0B13B3214D1C
              FC19673E9DEFB608354BAD777539E77D1FCE3970057BE86E6B90DE58B00BC100
              600237DF4CAD3CEF194AFCE5D5FE2C5CADCFE7C6C90261197238EFECD932ABAE
              CEE74C4C7495E6E90F97D6B32AFE3DB33FE041E7616EB716D65B861CD683C12A
              6F63239A6D93999EB66426DD79AA3C67A4D0A725DF4DA7B63362EBD171D4C7D3
              4BA1B0D0B4F739EDED7E331C46885F6DA51499F171D64647BF2AD76DBCF6626E
              61E44B6AF704F7DA0A3916F4F4F962B106ABA6663B0C2084402F2A42391B79D9
              F979CB94E2EDABA95500E49629946F6842CA335634BAD75D01B06AAA01BA4BF3
              8DEDDA36A024570F088FC712A6B92F40E6DA08218A2B0A0CA1CBDF0035873C04
              BD7A58DAF6BE61002135A4DFAFDBA676BCB3DCDE01F436054070D908850E0400
              782A2B41D073A725802E403B5F6573AB3958210D63C0EEE8300E5A01400B0470
              2627EB83A61ADC74D5BC3674A5D43234F932A7B9F9C8FF4C20741D6959726366
              A63516F23D9396A13DF684C38D9EDADA7F86B76446227822917A4D8A7E1D6871
              D7D6488F8DA1F9FD48DB4678BD08D344E83A002A9B45390E2A9DC65D5D259B4C
              E2A6520027F4ACAB1A48244E6F241231A04A41D9F286ACCDB10C4B57594091C9
              8252EEA225D52C300B7C564A7D005E8B8057D254E2251C3029CD37F0D9B90CCE
              173CAA6E68BE7EF142374BC9247D7DFD14AB85589BFDE3E3CABAC3DCF226938B
              193E7D4BEFFC852DD9B64D341ACD9352F601E78045E0BEE3384FE2F138AEEBEE
              F2FF04BBF8B998BCE474B90000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000033D49444154388D8DD44B689C
              5514C0F1FFB9F77ECF99CC4C273349E791B474E2678C89CD6342D362B4633585
              B8B08F8DB8A9D485DDA95BDD28082EB40815C18D6EC48554ED42A475E163D945
              050543506AA4262D2438659266924E32339F8BD0D43AA99D037771EFB9F7C7E1
              C03DC27D221733FC706A2FAEA5CE006F0216F07E48F8F6E90B37F8EECFEA8EEF
              F44E87BE25BCFE788AFDBBDDA22875C13F783062EFDBE7D617164A84E1CF9D9E
              FEEDD2D555369B6D80435D0E5F3DDFC344DEEF41E45D934C3E142D9530A9149B
              8B8B345756927BE2F6C51303B1F5CB0B6B2C551BF7BC57FFDEE46386CF4EE6C8
              7458CF213263D2E9697764643BEF04013A91984298C944CDC4A727F2E463E6FE
              159E9DEA66B0DB9B00BEF18B4537522A6192C9EDBC492671FAFB09D7D7238D72
              F9A467D4F97C87A97CFDFB6A6B85315BF14C210270CE0902CB1B1D45445A7A24
              5AE34F4E6265320911DE79BA102566AB5670A0CB41441E1691A2373CDC02DD83
              8AE06EDD39A6159181B4DD0A462C41441E11DB0E7522F1BF208049A711115B44
              0A31F76EE7B6C1A0D301D8258EF3400C401C07940A81CC44DE6F058FF6450186
              743CDE1E288249A7014AC7FB63385AEE8213798F918CAB80E326976B0B04B00B
              0580539DBEF64E8F6EB549F996F0DE541722F2A25856AF13046D834E10A07CBF
              0B78E5D50349F6C40DFAC367338C65BD1E44BEF4C7C73D3B9F6F1B14AD518EC3
              C6B56B878C92F36359EFA63ADA177511F9C2CA6677B943436D6377C20E02ECDE
              5E4F443E7FACDB8D2811F948777414A3478E204A3D58F86F9522440E1F46C7E3
              FB45E463034CA135B5D959742A858EC751D128A2771C44DB11369B34AB559ACB
              CBD4CBE53BF7270D70A851A9BCBC76E5CA53C0A322E203886D87E2BA88318865
              6D21F5FAD6AAD5086FD70442C230AC01B3C08FC007F2C613299EDC1BA12F69A3
              058548AEDEA4F7FB45E7851B75FFCCB1E929546303803FFEBACE4FBFFC5A9DCE
              DE7E29EB359604990FC3701ED89CAF6C7279618DEDDF6F6BA12766D8DD61C8A5
              12FCED64B3AB0D3D3332321C1B2F16A9542A5CBCF42DD5D55BE77ABCCDD7C2A5
              AB2CD71A5C5FA933BF5CE7D6C6D6B46D1D2780D69AC1C1416CDB1E1391B3C001
              E026F04918866F95CBE5FADCDCDC8EBDFD0737FED379FD9563A8000000004945
              4E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000043D4944415448899D964B6C54
              5518C77FFF73EFDC7BA77766FA9C3E297DD002A950892D012196F0AAC122EACE
              C480C485B030B8209A800B4CDCB121D1283E48247161A26C5C91B0C485914449
              0951A984F228AF52028CA5D3F6CECC71515A5BE8F0F0BFFA7FF7DEF33DFEE73B
              DFB9E20938DC57C72B8B13AEA48F8177059E85EFADB57BAF6472631B8F5E6422
              6F8BAE378F735E9770E85D1422699FA4FDFEA24595DED2A54919B34BD2A78D29
              9775CD258F4DD029F6625777395F6EADC3778D9174D46B6A2A4D6CDA64BDA626
              ECC404B9E1E1E780839B5B1385285FE0B7EBE34F5781111CEEABE5A397D29478
              4E9FA4BF243599540A0049326188245FD2CF259EE9DEDF93E6B32D35E8692AD8
              B3AA821D2BCA003E30D2376E656565B8762D4147074848925B55854926C9DFBE
              DD602727B703E79654F97FE62DFC7A353BC7DF9CA035A1C3C9775A88C7CC1BC0
              8F7E5B9BC29E1E8B33BF92766242A3274E10DDB8316E2D6BC6A3427FCFB783DC
              BC9F9F5FA2D79624095CF9C021B7BA5AE1BA75E038E801A6259A81EFDB446F2F
              260C03894341CCD0D79E2CBE072BEA02805EA031DED5858A643EC74110107476
              02F408BB786543503C40E01A24AD96E310ABAFC75A6B01EC03CCE6B36DAFB111
              40A055BE3BB76FDCD9C6825217A0C6C4E3D3D9CB5A6BA7E599CD67DB260CA71E
              889A8EB43F7F05614C2CAEF001AAE4CFFDE889705DCCD49AD6BAA4CBD24AEFD1
              005BDA931821E005A7A2625E598A490458B7B616603360DEEA2C9B1BC0087677
              9783F4A2A4865843C3BC9D339B3F6C7B6D6D48B4825E7E73798AC654ECBF00EF
              AD2CA77DAAAC0FE5FB78CDCDCF2611E03537639229240E788EF4C9FA34029CEE
              FA8083BDB53846AF1A9903F1AE2EDCFAFA399B598CCFB1256B8240D1A54B0BAC
              B5D75ACABDDF47C672A87F770BE5F1D842C429379D4EA7B66D03638ACFDF27E0
              9FE3C7150D0D65B076E564C19E37657137441C33BE9F4E6CD83073B89E750FA6
              79D8D383098214D2B19851CA48FA42C67487EBD7E39496FEDFC467E024122436
              6E448ED329E9880B6C95E7D9DCCD9B20E1545460E2712CCCC834DD920FF387ED
              42364BFECE1D72C3C3C8F3ACCD6637B8C0EB85F1F103D9D3A7D7489A1A24B118
              260C31F138261E47B118C462C87D70F073396C2E878D220AE3E3D86C96C2E828
              368A84B5586B23E014B04FE7DF6FC77384B5B64C5227B00C68019AEEE7D4742F
              32DDE5A14FE080F21300B28E6733639358EC9D945B3867C43070191804CE5A6B
              FB815B004A7A86B50BE32CAB0EE8A8F66948BAA44397A4EFF2D3553FF1C3E5F8
              D0F39DCB936FEFD86EA73BF2DCC080BEFAFA08ED8968CFCE96ECE7EDC91CD9A8
              C0C8589EA14C8E3F6E8D333032C1C94B63F3DE7208284F96D0B6A4032BED35C6
              1C5CD8D8487B7B1B77EFDEA3FFCC1945517456D8D563A399B10B7F0F10152C93
              F9477D151DF8D9C9889230C4F7FD5F808BF73299D60B838315D7AE5F1F29140A
              DF596B775AC85CBC7C85CCFD2CC5FE5CFE057F3A8F93FC1A10AD000000004945
              4E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000054D494441545885B5575B6C14
              5518FECE99999DCBCE6C77DABDB6DB6E5B6BD8B45B608B2D18AC051B5A082008
              6D30F860104D242131C60442139FD060D0E88351131FF48118D1840886101303
              112989C4A8B43E006DCA36B4B0BDD165BBDDEE6D76C6875EB46C177769F9DE66
              E6FCFFF7FD73BE73CE7F081E033E9B0907022AEC120300486806CEDC98C2C5DB
              B1827391420336948938B5B74CE459FA0180FD004C00CE1B06DE3ED93D3EF6D9
              EFE1272BE0C22B15F03B85337C4DCD6E69FD7A806591E8E941FCFAF5DE54466F
              DCF065509B8867F2CE470B212F1129EA1CBC8FCAF26E734B0BA8D90CCAF3101B
              1BC19596AE3631747BBD932FA8A0BC04983982AE661BAE1EAC960821AFB22525
              200CB3F09D1002D6660380D64FB7BBE9E1A662B07996C6FCDF802A2B876F3B3D
              68AF51F699587AC15459D92EF8FD602C96C5030941666AAA894BCCECD95821FD
              F5BC57BC7B2918432C6D3C32FF233DE0303338BFBF022E853BC958ADEFC89B37
              83B5DB738E370C03E96010B1EEEEB41E8F1FECBB9FFCE6A5D3C388A6F49C318F
              FC035FEC2885DF211C611DF6772D3B776657FD70358480515598BC5E263538F8
              A2CA1ABFD9CCCCED9F07722FCF9C33D55C21A2A552AAA5027F5C696B03E5F337
              1763B5426E6D6508255F75D659CC35C55CE1023AEB8A4008E9120201969ACD79
              93CF8373BBC157579752425EDFE72F2A5CC0B31E912794EEE257AD2A987C1EBC
              CF0700FB9E29150B17E054D83A4655A5427EFDC3609D4E104A1B3C162EA7D796
              14A00A14007150497A6C7200202C0BC2F39C55A06A4102AC02030390C1E5364F
              DE224C26F02C2929B7B0F90BA873F02080F771CCF73098E2620024B0B972E95C
              4B0A68AD3603C0FAB9ED7559E03C1E00D8B5A776E93D244B40114FB1AD469108
              A5ED73C1CB82A9BA1A84657735B845C7064FF66AC81270B8A91812470F705EAF
              4CC5DCCB275F509E07EFF3F1203872F4B9922CC245CFB576135E6B501550D225
              0602CB269F87B8762D08C71D5AE716AB5EF62F9E8A0501024BF0C9561738867C
              24F87CAE9598FF05124982D8D0208090CFBB9AED2855FE5D110B1BC4C7ED2E34
              7BCD1D8CC57242D9B265D179BF12601D0EA487869EE252F1D17A27FFC70F37A6
              A01B73020E35AA78639DEA272C774ED9B6CDC428CA8A9203B32725E77623D9D7
              F7429999B9A00A74E4D2E00C9816AF840FDB5C2594D28BF2A64D4ED30A383F17
              A82080161571A96070EB6A177FFAFE4C26C6BCD9A8628D4BFC5E0C049AC4FAFA
              27463E0F56554108B16AA1D05A5DC7A979136ADAC80892FDFDD06385F7F6F942
              4F24901C1840FADE3D00982604602F0FCEA0B3D6D28950A8231D0A750068A692
              646354158CA280CA32A82C830822A82880F03C08C781701C40E7F4EB3A0C4D83
              914EC348A66024E2D01309E8B118F4E969E8D12832E13032D3D353308C6E18F8
              2E6318A72F0563B33DA14B66B0FD69051B2B24AC71F2B099590F21F001A40A80
              178027AD13FB509C692BB75B299349826849105D9BED292963E80C8FC84C0A26
              A4FBCCAC31046002C05D0077000401DC9C4E65067A4692C6B5E1199CBB1945F0
              417AE9A654315154A91C3C160E650A8762892263B6E34AD872BBEBD851AF7D89
              C6D4300C1C7FFF04DCDAA86F0D37D2FF20AE61624643685AC3DD290D772269DC
              8B6A59714B9E91D1948EDED1247A47930BEFBC5E194EA7E5EC2F977F7DABB363
              6F56CCF59E5E4C4E4EFE3DA691FEAFFF0CE5704136F2DE6D32990C6C36DBB5A1
              E1E11D93E1B0C3566283280A884422B8D27D1567CFFD18D1757DEFD8D8582812
              89E42DA0A0BBA1D3E9447979B94C293D06A013B3FE9800F01380F7A2D168F0D6
              AD5BD0F5DCF78065090000599661B7DB21FDA75D4BA7D30887C3181F1F2F341D
              FE011CE188F3BF71FA720000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C0864880000070A494441545885C5987D6CD5
              5719C73FCFEFFCDEEEDBEFF6B6BDB7EFEFD0162854283048C7D0C290910CB738
              7C05829BC368086631D3FD61B63F966DC6680C3159625C74892C24C6A9318B2F
              A001711AB5DB100C0C6A97A5B4584A4B4BDB6B7B7BDF8E7F5CBA314B6FE98BE3
              9BDC7F7EBF737ECFE73CF739CFF39C232C813AEA026CAF0FE098927BA0E1FCE0
              14AF9C1B652AA317F56D59CC642570E481527637852C44BE00EC046CE02FC08B
              EF8E246FECFB791F9747D37707F0D0C6084FB6178744E4B8E1BA1BEDFA7A508A
              544F0F99B1B13EADF5B6B35713DDBB8FF5DE1DC0BF3E5E4779C87A5185C35FF2
              1E7C10C3EF0740A7D3C44F9C20D9DBFB77ADF5E6878E5DE6CCD5A905D930160A
              571D36290B9A0EB0CFBF61E37B7000629AF8DBDB11917B0459D350E82CD4CCFC
              014B8326DFFD7809270FD402DC272201152D9E314E791E62DB1AA1E5F9ED319E
              DE1AC573E6EF8F79CD7860598093076AD8B32ABCCE52C61F0CC3F8AD5952A2C5
              346F3B5E725EFDB16B1A2F7D715DA4E4C4FE1A3655BAF302BCE3187C746D01CF
              7C348A08DF108C67ADFA3AE56B6BC38C44669DA3B526D5D7C7646727E9A1A1EB
              5AEBBDE98C3E7EE8D7FDFCA6FB3F4B07D8511BE0479F28C350C611B1AC4381AD
              5B71EAEBEF6C6580CE66997CE30D26CF9E4DEB6CF6F38954F6678FFCB48F7F5E
              9B7BE3A8B906788EC1B13D95046C75582CF3E9D0AE5DD85555770C0720225815
              1588691AE92B57769B86F1BB8D95BE7FBF726E94B9D2F89C31F8950D0514FAD4
              32846F05DADBB14A4BE70577AB7CADAD38CB973B22BCBCACD0B61E59E5CD3927
              2FA012F8744B0122F28C555EEED88D8D0B869B967FF3660CC7592122FB0E7CA4
              6071801BCA7D14F9CC88C01EB7B5159145E5F59C41D7C5696E0638B832EA501A
              C81F65790157C66C806DE238965551B168B869D9757508AC078A57C6F2A79DBC
              80614781D0AA8A8A1063C145678654713118862122AB0B7D8BF06071CEFDD15B
              CBD852480C03715D0DC40A7DF9179EF76DC03200FCB3558AC5482C0BC0ABF4AC
              BCE3F2023614DA003171E7579EEE44462804B06C53A52FFFB8D95EF82DA1B9D8
              016836BCB9F3D57C65C66200F735153BD48467F7E2AC801D75011CD3A811A4EA
              E6C79654767535C00641AA3FB33A3C7FC0CFB684011E364241549E8660A152D1
              282A121184C7F7AE09CFDA8ADDF6695B99CBBD357E010EDACB972F4982FE5F89
              086E4B0BC097C3AE0A1F6CBBBD1366002A816F6E8D02F22931CD2677E5CA2587
              9B96D3D888F2BC08F0E4C1B6026AC233B3C50CC0C7D615D056EE064478C16D69
              61A973E0AD12A5F0AD5F0FF084631A0DCF77CC8CF50FA4F1D5318723BBCA300D
              E388F2BC6DC18E0E44CDD9912D4A2A12213D306066C7C757541758478727329C
              1D78BF4F7CCF7AD4AF38FAC94A8AFCE6C322F2EDE08E1DA8F0ECBB6BA9242298
              25254C7575D593C90EDD53E5EF3CF14E9CEB9399F70195C04B0F55D012739A41
              5EF3AF5FEF384BD05ADDA90CD7455C97F4E5CB1FB30C5EDB52ED1F78F5C238C9
              8CCEC5E073DB626CAEF41780FCD2AEAD0DB96BD77E6870D3729A9BB11B1A7C88
              BC5A1BB10B7FB8BB1425A0762D0FF2D496622522BF300B0B378476EEFCBFC7DD
              ED2422585555A47A7B237A727273A5671D4393319A8A6C44E46B86EBDE1FDCB1
              63BA88DF15896511DAB9132310D822C8D7CB3D0BE3E6C14EEB6C96A9AE2E3223
              2368BDB81BA9852A3336C6D4A54BE8540A840901CCFE780AADF98E9E9ABA31F9
              D65B871367CEAC129F4F9BD128AAB010150E63844218C1204620B0E8C6556B8D
              4E24C8C6E364C7C7C98C8E92191E263D3848767C5CB4D6FF02BE07FA07FDF174
              CE7DFBD784796C5D01B5111B11A903B6006DC02AA051A00C118508E238DAF0F9
              109F0FB1EDDCCFB272713B1DBBD92C3A93C9792295243B9544271239B0440232
              999B7F9B1E06BA80F3C09BC069ADF58591890C2F9FBDC1F7FF36FCC1837B43C4
              6253959F353187C6229B6545369EA3002C11A902AA35C42E8E99ADE747CDA7B6
              B76FC4268DA427914C1AD1B97B402D0AAD2CB4E9F0E7CE7F48859B3ED6E4A5DF
              5430883008BA57430F9A78260BEF8C24E91E4E72A63F41E79549CE0D2498BEF7
              9CB30BF05B42A56751E559447C8AB2B043AF5D5DDC9F30079EF8EA61AAAB673F
              C40F0F0FF3EC732F48999B5ED56A0FBE3D3A1E676432C3702243FF789ABEB114
              57E399BCF6E7ECE527529AAEEB49BAAE27732B12A1ADAD66C830F4EB7F3CFDA7
              7BF7EDFDDCAC734F9E3A8DD6FAC29509E3EDE39D434C4C4CCC656E861694F082
              C1208EE35CE8BF7A757F3299540D0DF518B76C9E743ACDF113BFE7E4A9531960
              6F2A957AB7B77761B7AC0B6AF47C3E1FCDCDCD28A5EE370CE3279EE745573437
              E1791E63E3E35CBC7889D1D1D111E051ADF5AFBABBBB191919F9F00021E7C586
              86066CDB0E027B45640B500C5C075ED75A1FCD66B3633D3D3D0C0D0D2DD4CC22
              6FF995A2A8A88870388CBAA53C66B359E2F138D7AE5D239D5EF80D3FC07F01B7
              D8182086C9EE420000000049454E44AE426082}
          end>
      end
      item
        Name = 'home'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C086488000001CF49444154388DA5904D6B13
              5118859F7B6724D38C494A682706AB85D6E2A2092E941215BB5444211BA10B71
              25852A76559885E21F88482514D48D684134E00F0808A222166A88B6521117B1
              62FDC0544B9A3493C6998C0B69D19A4E033EBB7BDF73CE7BEF1134615B778CCE
              0B3750C3D131A0E15496C617AF8FB2FA76EA1FADB2F1C29F48D2397A5353B6B7
              DF527E5A63D2758EA205FAF483C9AC5BABD8F5C2AB4D02842474CA243C74B14B
              286A56AB7C3BD6F36C82F0FC73568CBD71470B1CD7628359D5E82E59B38FA1E1
              FCB60188B6001D2369DA6283878007C1AF73915D2F2651EC1A008EEAE3E38133
              2C47E3456068F5C3DC93C589119C1F9F51D41DBD44CCBBF87AF69D159031DE3D
              0CEDCCDF4736ECF5C7C9864368218F1042AF76EC39ADB41B253D919CAECFCF20
              BAD22F55A907C73585F3C6D46D37B4906FD6EB3ACBD138C523C3A26A33E93AF6
              3929F5E03D9FE4E4D5FE3A5B9901825F5E73ADBF8E5F21211435235DDC2B87C3
              CE40AFEE6E695E63B7DFE544C44EB870597EBF73697AFFCA9B22205A051003F6
              FB52313D3C2BAB4F33C8A54F2D6F5F43D6CAD4661EA16E2648A552E2CFB3699A
              4DFF28BDB6589625CBE5B2A7C673582814C8E5725E12EF8056F8EF80BF4ADC58
              5C2BB35F63B49BBE54864B360000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000029149444154388DAD925F4893
              5118C69FF77CDBB77D73737D33E75A596D9F64C31B1B4B05EB465A5E24D64545
              110585D0A83088F0AA5BA9E82A0C2382C02022B0A23F372A82C44C42226B854B
              9D41FE6F6BE834B3FD3B5D54D6989B5A3D77E73DCFFB3B0FEF7B0859945B730A
              C6DAB32041751A401C9CDF8874DEC6F4BD46209958B24758AA481A1D4CC72FC1
              E03E211263D789E80211D500B08AB6D2766D9133F1D5D7051E5D48EF4D7B41B6
              20BFFE26D4858E02226A25A0D2EC6FE39C8060713571A007C0FE5868742274CD
              83D8487FE6849AE272E49F6B81BA60B38B883A8544B4A4B0B705791FBCD00707
              A18D8C63CE52B28133D5614197FB3CA762EF683C3C8ED8E8FB7460CECE8358EB
              698220198E80F0409CFF6CB2799BA10F0D2D9AB5B353C89DF061AEC0614888BA
              A3A412272567F52B1275F8E6EF0138870026C074AC11C6DA7A464CB80CA22BFA
              4F832A5B773334F3E1B419A9A273903FF662C1B85E8819CCB500CC9A22678746
              7126177C5D20F3F93BD03A2AD600B84B40755EE019B7F81E72CAB0C5DFD36798
              2CD943A12D6EE28017E007E2C19129A6DD5AEE00F042CDB07B9F30C4D7BD6E5D
              1E06003C09CBDB27FC906A804B022A01EA55E56FDCCE40749111F42737C59FEE
              CA8BF2E549A9DA618AF333B658ABC81005D0C038789D594C96398D899758E21B
              AD40B4CD987CA7E892651CA863D38FAE86AC03ED6316ED0F18AD520020AB4145
              81B670F856C30C9B7DDC04BCE900FB9B6CBF221280B17E7CE9BE0F5536A3DD6E
              87A22829B5402080E1E1E18C3D59818AA2C0ED76CF00E8FB592A0560CC0664D9
              803FD5178944AA82C160D51FE07F02C2EFF7C3EBF5AEC4BA32E06AF4DF81294B
              91240976BB7DF12CCB725A832CCB291E49923203AD562B793C9EAC095C2E17B9
              5CAE8CF7DF018D03D27ED75B65CD0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000035E4944415448899D935D685B
              6518C77FCF39F95AD2A61F69B2A28DCE53D99C1FB3085B28535AB0EB85D875E0
              FC400487A04E10D422052F44512FEC95A8880A52C7C4156FA43713C1E2BA8D0D
              751F30628B1F65D576D6766B71EDBAA44DCEC9E3C5922EC9D2A6D91F0E3C1FEF
              FBFE9FE7FF3C472803231822B4FF637C5B62083C07D8A81E4C4D8D31FBC94BD8
              33E36BDE37D74ABAA35B89F41CC07BFB3D6E810F80F710E906EACC60E8477FAC
              2B939E18C1BE345939817FC7A334BCFC39664D382430083CE9BD3A8B2B9514C7
              E38F013BC5E3FB2E10EB4AA86393FAF3F47A098460F72BD43DFD1686DB73BFC0
              0F22F240F5C5DFD874E253EAFF3AC9526D13E9AAB085F038621EF7DDBDF35F77
              A3C552FC2838F6EA0446A086D08B1F52D5F61422B2171814D8D8307684A63387
              30EC653533B6D45E384BC67491ACB76A81670426DCB76E8E6FD8D6CED2E80932
              89F91B09CC7094C86B07F06EDE6E88C8DBC04746C6F6369D1DA0E18F2145F57A
              25AA545DFC1DEFE225B9D2B8D5AD86B94744AA8D60F888BF754F26F5F708CEEC
              E47502DF7DED447A0EE20ADD12149101E07977F2B2DC71F233AAA74754B20090
              3C78E7A73438332A8B1BEF12C7ED6F159156717BBF0FC476277439416AFC1C52
              D5B18FBA27DE404CD79DC0B72272AF7FEEBCDEF6733FEEA5055455738FE7DBF9
              BEED0930B1631F57235B4455CFABEA63A0E712A70E23D12FC6106417C22111EA
              A3B3BF527DBC5F8D4CE1B0CA41C564F9A167652CDC822A8BA02FA8F28D2122AF
              221C0682FB37D9ECF58CAB38E915C155B5A47D839FB1B5CB3BA9AF37A7310453
              C4F85A44DE35800D02FFB5D4641EE96E742AAABA141E0E67B4ADDE6913744261
              CAA5AAEF8BF0657BC899CE16942F73815390289D13556557C439F3CB65A365D1
              66C1B57074407D1ED7F483DBBB731B52C0B29E21E7D98808DB828AEF58FFC285
              7F6670CD7FF52669BF9F40CF6EA0A0C09B8629CAF24F835C191DC5A8E4629142
              EB822BDFD16B58B173F1BEBEBE158EFCF3BDBDBDA5366B758232331856D5E16C
              B83DFB15CC20D7653E490141190CCFCDCDBD934C268946A39225288B8219144B
              947372663C1E6768680855A52897FF331610542251FE39B8368FB21255B44537
              83756DD12A9BA26BE44A1388883437379792A4A4449D9D9D058F5996B5F616A9
              AA5A9685655914C74B75D0D1D141318A3BF81F0696B64904E0E26D0000000049
              454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C0864880000044F494441545885ED966B4CDB
              6518C5CFBF170AB45D0BA5C86530C80A81205260B88D81026306490889A3B82D
              3037C52D9B89D14521591430BA986032BC44330D68B2CD2019996036E684719D
              0C106CC96869072D50ACA52BA145A0A5375E3F4D2103810EC317CEF7F39C5FDE
              E7E4C94B879B12864440F2CE97483C741493634398333D746B0EE58E297A5F16
              0ACBAED2BD38BC4F00CA615F983F5FF371914BD6726DC3B3E81B351C2C28C1CB
              255F0B3C58DE0D2AFDF411D3FC42B23F9FB74F9C7EB8D1C3D3DB3ADCDF0A806C
              3E0093E585C2B22B48CD7B238E46D19BDB14E3F1D77B95908D1BC0A4D377EF12
              F225E1B1C96D614FEF3728BA1AE1B02F6C1E806F6018CE56DE46446246BED345
              1A6AEF29845DC37F20396227827C38681BD2C2F897C5272AC8EFB8FF4E915A9C
              9E271F96B6ADAB176B028812D270A6F2679A2028FC8269DE5659D52A658E4FCD
              E0A5A42824090976FB78C04FE08F4ED504143A2333325090271008D9495985AD
              533A35991C55B80FF09CE44D14945EE6B3BC38756A83F94475AB0C4E17C1C9E7
              C510CCAA20ABFB88E8069A10218AA2E262E2D0AFD1A37B4487201FEEBA7BB122
              0083C9C291F355C838F66E348D466BBEAB9AD87BAD5B017F1E1B45E962CC0DD4
              1355D337208B2E9045270CCA5FB1C39381F4D44C4A6D30A35DA95D772F1E03E0
              098371E6E22D44EFCDCA752D921B75BDCAC00EA516E2B0A770F45911D48D9544
              37F0CB6383CC130AD8A646F142660E356F276857AEAF17CB00C29F3980B39FDE
              A6842191A53356FB57DFB60FB08627A7912D16216D171BB21F4A895937B4EACA
              2C263DA61E74E3404A26E5E3EB87BBCAB57BF10F4072EE291C2FFF9EEBC5E1D7
              8C19674E57B74A298BDD81C2D45884123DA4B565C4363BB56AF823391666A11F
              6C4554640C151B1D8BFE513D7AFEA31774069385C3E7BE40D6C9F745343AA3A9
              6744975AD32507CF9B85D733E2E11A6981FCE6E764D1695F33FC9188CB09C350
              27F86C169596721023932674ACD20B7A7EF125ECCF792DCB4570ABBEEF41488B
              7C0C51C17E7825251A13772E91F1DE7A6CE4B22D95493B08C7B416873273A859
              9B0B1DCA8965BD088DDA23A7B1797E00A853D2B149FE6FEA3F911113867C7110
              EED7BE47F4F236B78297CA38DC8BDFAF1693EC881DC84988C0E084110ADD9437
              80173DBC39A0625373F1EA853A21A128D9E843734042B0277EBA787AD13E6F7E
              E2F0A562B0BC2129A9A2C98D803F8F3D4C40F65CF9A0608E76BFB301F76E541B
              2990133101ECEA24AE966C763800386D16C4737444E44BFF1020C77A6E7E3727
              6DAE0503007EFCEC6DF806843665268637D128145114E5DED2D71045810A7069
              CADBEF0CE07AE55B00001A00386C565C3A970DC360CBFF91BB4C9649152E9717
              C261B3FC0BB095DA0660B863AAA8A858F12F595C5CBCE1F2BA05000056AB95A6
              D1680000A1A1A1E072B98BEECC717B051A8D061289041289047D7D7DEE8ED9FA
              0E6C036C036C39C08A7760B543B396DCF12D03D06AB590CBE5EB7A15B55AFDC4
              3E00F81B8380F845BE7D89F90000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C0864880000061D494441545885ED986B5054
              E71D879FF71C585C5845AE7295007211C1EB924A1AF0B2626BC60B1926A19691
              66681227CD4C67E224CE449B4026B5994CDB4C9396D4CE4833D2A66A1B4CAA63
              4D4C4721E2A510054470895C45EEBB2C0B2CCB2EBB7BFAC168D318E5B2E8F8C1
              67E67C39EF39BFF3CC797FE7FFE1C8CC101109CB78E2F9374949DF8275641053
              4FFB8CE48A99085991B995A75E7E1F95B766B300BBD3E1F8F493DFEFA0E2F01F
              DDCE96DDB959926536BEF0169B5FFCB5F05079BD2E10EF83D82A49F27852DA86
              0AFF9028F4959FE1723AA7FD8C69BF416F5F7FB615FC9504EDBAD94248FB6D0E
              E79643E71A9085E0A9950B5179C8A5A0E477E82F8C14EFCAC6DCDF79FF0443A2
              93C8FF55298111B10B04D2C786616B52C9E94BF40F8D0230CFD7876DE9290468
              D4F5A03C3964EC692E29CCA5B9E68B293F6BCA5BBC78F5933CF7F611E60484FC
              40207DDAD86D8CFCA0AC9621AB0D6D4C28C1737C68EE3571B1B587503F4D70E0
              6CEF6D5EDE9A4B2BD6E7368D0E0F70EDCA97F7465048323FCC7F9DEC97DEC343
              E5B51344715943BBFA70A51E97A2B071791CEB93A3581419C42C9527FA2E03B5
              6DBD4892342B3A68EEB47B39A92D56A97DC82BFC90A4B427BC8590F68D3B5D39
              FF38DF405D473FDE2A4F7EFCF822E66B246A3F7E5B1192C4E2AC9DE2DA9093BF
              9DB9CCA8DD417244D0B47B39E11B0C9E1FCFCFDEFD9CE894C7A284904E982C63
              BA7DA7AA69E91B24C4D787E774CB986DEFE7C281D79491FE36C6CC7DF4EACF10
              9B942AB40B1368E91BA4A56F902B9D06E242FD93BC559E9B7D03434F2C5FF723
              5387FEC284F3F2AE8249691BD8FECE717C03C3D708214E34F79AA28B4FD560B2
              D8488E08E227AB166369ADA4E6A33DCAB875E8D67D0E9B85EEBA93F80747888C
              4757621CB1D2D23738AD5EDE513033EF557276EEC55335EBE74288928AC60ECD
              DFCF3730EE74919912CDE61571B4557CA87CF5EF7D28AEDBFBA4B89CF4359E03
              A78D8CF47542E5E14163B771CABDBCAD832AB50F5B5F2D66E99A6C2F107B1D4E
              57DEE12A3DD56DBD7879C83C9D96447CA037978FFC5631344FEE8BF47F64298B
              B35E11AD83760E9CADC73A855EFE9F6040780CF97B4A098B490E478852F3A82D
              F52FA7EBE8340D13A05193979182C661A6A6F497CAA8716A83573D771E4BB277
              0BBB3A9892D375F49A2D939A97B7B6386EC55A5E78E738FEA151DF4788CFDBFA
              CDF1C5A7AA318E585910E2C74F572FC5D9D340F5A142C5366C9C921C8063CC42
              F7E593048444898CD447310C8FD23A895ECA00AB735E2277D79F51A935CF8338
              78BEA973CE81B3F5D81C4E1E4F88E4E9950BE9BAF04FEAFFF59EE272D8A72C77
              13C5E5A44F7F06E172B02A5D273C6499AFBA8DD4B6DFB997F2FA677EC1C6ED7B
              3C852C17395D4AC1275F36CA27EBDB912589ECEF25921E1742C3B177956B5547
              A62DF66D06AF5F61A8FB2AA969EBC4FC7901E8BB8C34761BE91DB488C4B0009D
              2C49C9E1714B8E07452CB0CBA91BF288885FBA1BC4CB1F94D572B9A39F396A15
              F96B96F08846E1E2A10265A0B566C6E46E623575D3DB7896F8E4956279621C4D
              3D03B4F69BB96630A38D095B040C5BCC860A39326139314B326A8410394E976B
              AEC536CEB36B97E1696EE7E281D75C5653F78CCBDDC431364277DD2925283C5A
              A46B53E91F1AE5B1F8080234EA6A05E559FD7F3E730AD52C1F76EC3B4F705462
              9A4094BB14459EAF1EA2BCE40D576B4BF33D93FB26313131AC7F66B768B606A2
              28581414ADE17AD3D5DFE46B91EC63164A0A73713AC6CF815230D7D3F6E2229F
              5E048AE0C618BA2F47AC7A40047A8C6C0165BBCBE1B85A52988BDD6A4102E86A
              BAC4B13FED42515C6F457A99F602E27E7343D2706CA0ABE5E091A257B8DE7811
              E0862040D9C1DFB123C30B61D4DFBBBD9C005F4F3B6FE624F0C5477FB8754EBA
              CBF50F040F05DDE58117F4703760D3A64D8485857DE75A575717478F1E752BDF
              6DC1B0B030626363CB81F26F2DADFAFA700BB705BFA6BCB2B2F28DAAAA2A0012
              1313D1E974053C408254555551545404405656163A9D6E46721FF88FE4A1A0BB
              3C1474978782EE72C739A8D56A898D8D9D30C0CFCFEFAE6B9999996E65DC49B0
              4CABD54E18FC0DDABEEB9CBFBF7FF96404EFC66D82B5B5B5747676AE9D6A5067
              E7FF7E85180C062A2B2BF703FBDDB203FE0BE373A895251012DB000000004945
              4E44AE426082}
          end>
      end
      item
        Name = 'button-info'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000000473424954080808087C0864880000025449444154388D9593DF4B53
              6118C73FEF39CEB33CCB76DCCC8919D272600A7A6181C4E8B28B7593A80CBBEC
              CEFB7EFE0F41745FD285B2BC109340C88B402F1666E02A10968B1115A83B3977
              769C9B9EF376E1307F44E8E7F279BFDFEFFB3CEF0F41153D3A887FE0A1AAFACE
              C541DC017A851086947203580439E6964B89CDA9A78E353B0AD2054000F8871E
              517FF36E0444C297FBDA1DC8CCA39B19D4B28DA3E9D8813066384A31D89E0219
              B793AFD3E6F37B205D543D3A8431703F22A49CBFB0F4EA52F3A749BCD62A8A53
              41008A53C16BADE2FFBE40EDF666C80A75C63DAD1DD3D271CC727A01C53FF040
              05122D4B13C1866C72AF25A010EAE473FF33510875EEB7DA904DD2B234110412
              F5B74654D5DF84A2FAFC715F6EA5DBC8263988565CE3FCF28CD48A6B87EA4636
              892FB7D2AD78B478DDF57E1460389099DBDFF96FC03A4DCB3368C5F543750104
              327300C3DEC83514A05737331CE5E80807A9EA7B559F818210865AB18F89FE87
              5AB6410803010A52E69D5AFD54018EA6839479000558B403E1530554F58B6EC9
              4641326E86A3C8139A256086A300E3DBE90F286E652B510CB6A736DAFA4E14B0
              D1D64731D89E92AE93B0DF4FA1CA9D8A7BA62BFACE0A5D897BB60B75DEFC0F04
              C7AF5156CD3F7B86720811B3DEBE582F2DBC41AD7C4B51D378D1F4B4764C179A
              BBA25B8D9743CA6E999A8A8D707670341F565307BF7A06C9856FA4102256FA32
              97FEFDF231B84EF5FD0885FAD808F5B11155A9D5E2C03F7E2363D27512D6ECA8
              939F7C02BB953DEBC1F9D48666F4BEDB6891ABA8678DEAB2C4DDB228AF7CC44E
              4EB1BB9A3D74267F00893CFA04AD2F828B0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
              0D0000000473424954080808087C0864880000033C49444154388DA5945D689B
              6514C77FE779F3F1E68324ADEDB6A6369B8E2CCD64AD05A545519975609D3086
              3A6F9C0E6F440603D70B7B29F5CE41AFBC9874174318A252149415419432B0AD
              3846694522B636D01A9B364D93254DDF246F1E2F52CB4ADA8B6DFFCBE79CF37B
              CE73CEC35FF85FCA2030F01E81572F224E77A7885C00FA11A28278B5D659600E
              B8A9B5BE5EDBCC67726357284C7CC1BD1200717B69BD348AD9D91B041941E41D
              EFFAA204527398F914AA6A5175F9283545C8B53F49D9DB5C0486418F147FFDBE
              96191D849ABD0D540607063FC7ECEC3B0CFC6016D2D1F09D2FF1AFFDC55ED222
              6423BDA4BACE623BCCEF80378B93DF5A996B83001881D3EFE37FEE5C1098F0AF
              FE197DECD6A798C5D51DC096FF005B8136C4AE60D86504F0E49608FE33C3DD70
              57CC769A515747E758259DA4B29440054E5F0418310B2BD1C393D7306C6B5747
              ABB153FCFDFC25291C8CEF3A77175639F2CB5554AD720E381F7A63089481524E
              774C84B7C377BE6A8001F8320B8492D3DA555C6B8899F914AD891F01868D60AB
              C3EC3E8942E482673DA9F69B59F3E2241DB76FE0CB2CEC196F999F40D5AA1111
              79C973FC5914D01F48CDED990C906B3BC14A7C8052B07DCFB85129E15B9B07E8
              771E3A8A028E99F9D4BEC07CB88B747C40B6F60102B8EBF5510C03858857551B
              67773F52B605820F40A175B6EAF23D14D0AED767EB40982B35753C14B0148A80
              6656DB360AB8996BEF418B3C10CCF2B5506A8A08305E594EA0B4D6D7CBDEE662
              36D2FB40C074FC65344C69AD7F2BCDFC8CAA6DE633C070AAEB2C96BFF5BE601B
              ED3D6C743C6D03972BE92456621A436FE6F1749F9CD2CAD97337DC15F3A71338
              ACC24E513035CBC13FC6F1E4961B604B4FBD8516F5A1467FBD3EFA01D595458C
              72720EC7A1C7B5EBD1D837B6D38C668FF43EA145E1C92DA36AD586AE2C5F0BA9
              EED7481F7FC5D6A286D07A243FFE19C56D5FAC6F42193CF2EE2778FBCE2022E7
              818F55ADDAE15B9BC79D4FA16C0B7BDB0F4BA1886898022E6BCD547EFC2AB9B1
              2B3B17EE5AADB7EF0CA1D7873042AD0E113905BC081C43C44BDDB167A93BF6ED
              4A3AC9C68D8FD8FAFDD6AE1734FE15870BCF891770C79FC1D576140C633B4DA3
              CB16957F1728CDFC849598DE71E97BF51F0D26438E26459ACA0000000049454E
              44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000000473424954080808087C0864880000043E494441544889A5965B6C54
              7510C67F73CED9D36D77CBF6926E61DB42699B1614C160C14B08C16A8324D604
              8517623406C1C4F8A051086AC2934F121313638212832462505430D517120151
              20D060B953D356948BB574B7BB6DB7BBDBEE9E333EB45B4B6937A8DFD397CCE4
              9B996F26FFFC854910AF8FC0DAD7F1AFDA809856BE883C05B4008D2252A96083
              46513A80A38AEE43B9E20C8689ED7B87C4E9EF01E536CD2CB1427594BDB2134F
              B0DA446423B05D2064A506B42072153BD18FB8691CDB4F2A5041B2A84A5CC374
              8056D02DAA74274E7D4B64F79B9019BDBD8015AAA3FC8DCF300365C5C05E60B5
              3FDC49B0E310BE70A7E2BA4C45C63B4BA2D58FD057DF846379E3C08B8AEE4F9D
              3B4CDF872F839301C014AF9FF2AD7BB14A2B8A81C3869B5E5171763F73CE7F43
              5E2202AAC83880096E3AA31484BBB4F85A9BA48AE7DAA3BE92A705F9D3337B7E
              BB78FDA42E1E1B2B50B47E1B05F7AD3281AF0C37BD62DEC95D1AB8D1AE59C1AC
              E8ADFA6686CB1B44DC0C7632363189E98C48E0C61952812A19292C5B03F2935D
              B3E48FD4A5E338D11E8CC2551B003602AB43E7BE567F6F8702E838B2BCAFA199
              DE054F9028997F474C9CB456B57DAADEC15E0BF8040C6F60ED6B001898563EB0
              DD1FEEA4F8F7934CB522CB0B22BFE10F77E149C6A6B5CC48A7089DFD12811A11
              DDEC6D780833588D25222D02A160C7219DF0641ACC3FB173ACDB49D64D852FDC
              4541A45B874B6B3761C807F9F737A901B458A901F5853B75EAE8B9F8743154B5
              E8FA19807B046AED8A7A2CA0B12072155C77C29EC99D66F9A5967771AC3CE65C
              384859D71166CAF385BBB303351A85255D06229576A23F8739FF0E93B42A012C
              C0163733D151363A0D971CB17F261AD312041BC0008D39B60FB8F37226F3C95D
              E6CACBD8BE6C6E6CAC80D2910A84FEA3217762426BEC41C4008E268BE64AC63B
              2BD795DE358666DF0B9040B5CDE9EFC150D52F5CC374A2D50FE73CCDC92233E5
              39969758D532805685A1915F4F61009781D6BEFAC7C8E417FDAF1DDC5AB09A4C
              9ECF55D51D6E3A45F2C2310C67300CB0C5B1BCF1EBCB9EC335AC19C7CFE5E1D0
              9C4512AE7B5450FD18684F9C3880A6E248C1F22729DDFC3E22B21EE1F3593D97
              8CAAB63D6AA45377E57956FCDAF217704D4F1B689333144DF4BCDD8C3B1CC34C
              DFECC4535E8DA7A2E1B2203D23FEE09AA1D062D33BF497D8C9E8B47665E17AF2
              A577518BF42C7E46D430DB506D51D581C8AE57495FBF028009903C7F84BCAA85
              78CA6B7E41F83993E75F199BF760493C588F9A1E319C3486338A00993CBF244A
              6B88D4AEE4E6031B88071B5CE023549F55D581FE3D6F9138FDDD34B69A1645EB
              B651F8F8F320E2157809914DC0C2191692406955D51D40BB1B8FD2BF7B1BC973
              3FE4DE9B5DBB94C0BAAD78EB962286290A75028D08952036AA51A043E134685C
              4793C48F1F64F0C07BB8C3B1A972331F8615ACC6BBA409BBB21EA3B004194FD5
              F16F49E6D63546BADB495DFC114DC66792E16FE31334F4F92501190000000049
              454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000000473424954080808087C086488000004ED494441545885CD975D4C14
              5718869F33FB37C0C2B20556DCED02FEA00BDA20950445D31AA94D4DAAA9C4A4
              69826D8C6952D2C4CB2626AD69D0A48917BD68AA5CD8C648304D8C0969D49856
              A5D2126D2D8AD60A25582D2E0BF2BB2C8BCBB0ECCCE9C502FE5465A195F4BD3B
              73CECCFB7C73724EBE5790A094B40CD48232ACB92B31677A116633200089340C
              62FD7E26BADAD15A9BD083F712FD2C62A6056AD146D25EDB89CD57EA118A691B
              F02AB01221DC0292254490B21BF81D6804EAC73B9A03E1863A22BF9E0669CC0D
              C0ECCE27E3DDFDD8F24B8A814F106C51433D4A6ADF1FA8A100964810458F6298
              AC4C243BD11C1EC22E1F9A63A181E424B02FEA6F6B19AAFD98E8ED6BB3034829
              ABC0B9639FAA58D503025995EEBFA264763490140ACCF4C318737818C8DFC8B0
              77B5211135D2D03F1A3E71400B7FF7556200A9AFEFC2F9F61E378853B6D1BE22
              6F731DC9C1CE198D1F57C4998BBFA49271BBEB3A52BE39F2FDD7DDC3C73FFBC7
              3AD3C383E4B20A327654BB41FC94D6DBEA5BD474085B6470D6E600162D84B3F3
              32E3E99EEC71BBABC2B6B4F884118B86A31DCD4F0630BBF3C9DA7D581526F3F9
              B4DE565FEEA5C328466C4EE65352A48EA3AB05CD99933E6E7795ABBE3547B5F6
              5F62FAE083AD9C06C8FCB0064B56CEE7B6D1BE2D8B9A0ECE68DE5BB0993BAFEC
              1648897DE0D653D70924A9DD3718F1ACCAD66DA9E9B6A52F9F196DFC66FA7428
              103F6A6A7E49B14056799BEB30E9D17F55F9E332E951BCCD75086495257B7171
              CAFAED0FE6009C95D558B2BC079DFE2B0599B77F4CE8A3F6815B2C683BF3CCEA
              1F96450B316ECF129AC3ED3267E51E1FFDA10E004549CB40F595BA81AD991D0D
              B3AC6D76CAEA680021B65ADC4BDD961797C701545F1942316D53477A9444CEF9
              947A0B3673A3E20BD1EB7B23E177924201D450B722609B5AB83E0E60CD5B09B0
              C1DED73E9B62E6ACD4B8CF86495F1473A617604552A86B5E00D4501708B1C29C
              95130710663308DC9648705E002C912048DCC2648E03C46F6391ACE813F302A0
              E85110244F8F410272CC3059E605C03059013986947100691820094C2439E705
              6022C90992809CBA0963FD7E809B5ABA675E00267D6E4EFAA24C74B50334865D
              BE790198F4699CF445D1DA2E02D46B8E85C6589AFBB99A8F39DCF18E09EAB5D6
              A638803ED4CDF8ADAB0124A70696953F578081FC72909C8A0D7605A2777E8B03
              00841B6A01AA87BDAB8D8833F7B998479CB90C7B571B4075F85CEDF473052072
              F934517F5B8B44D4F84B2AD14DD6FFD45C3759F19754221135B1A19E96D10BC7
              A6E7A67B42EBE255B8F61C5715C57431ADB7B528F7D261C40C2D75229242A173
              EDFB8C2C28BC2EA52CEBFFF2034DBB766E7A7EBA23D283F790D1B1985AB8FE64
              D4EEAAD09C39E9A9DD3750A43E6773DD64E5EE9A5D8C2C28FC4B223785CF1E09
              8E9E3FFAC89A479AD2E89F5751D494B07549F189A8DD551EF6ACCA4E0ADEC5A2
              85666D1E71E6D2B9AE8AFB2FE45D97526E8AFCFC6D77B06E2FF19BF7290000DA
              CD26A43E11569797D6EAB65447306F4D49D49E25ACF707B18C8767341E7378B8
              F7D25BF414551831ABFD90847746CE1E0906EBF63E31253D351959979592F1DE
              7E2CD98BE3C908B1451DE99E3919A5B90D9027817DB1A19E96A1639FF2F09E27
              0C0080C94CCABAEDA46EDA89D5BDC40322816C28EB27FAFD81D1863A462F1C43
              46B5675ACC184EA764F116A216ACC59AF7783A0669E80FD271DBC56766C1FF9D
              FE06ECC805DE6154382E0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000280000002808060000008CFEB8
              6D0000000473424954080808087C086488000006F6494441545885DD987B6C95
              7719C73FCF7BDE73E9B9F5026D691DF442B9AD1CB28E0E9932C6D4068C323A99
              7161C311BC2CF12E89CCE81F2626BA65FCA1A87F6CD9E6B2A8A8204E16223089
              5191D2294D5186B55D2914E82CE71C7A7A7AEED7C73F4E5B28D7B6024DFCFEF5
              7BDFF777F9E4F73ECFEFF93D8F301D19166CB53E6C353ECCCA1A0CA717000174
              B44B3E3244C6DF4FBAEF0499F35DD35A666CCE49CBD1F810AE075B296A6AC170
              B8CA80878165400342098803348612007A800EA02D17194A263AFF40F4C86ED2
              7D276E3FA063E943946CDC8E75DE12BB209B80A711592560A079ACF12135D371
              2497216FDAC8DABD641D5E4104451328FB819FAAEAA154F7DB0CEFDD3169D09B
              021AEE52CA9EFE3E454D2D86883C037C5B44AAED23835A7CA103B7BF9BA2E10B
              18F9EC356373A683F8AC3A2295F7327CCFFDE41C1E51D54ED06755391CFDD32E
              86F73C8FA6E2D303B4D63452FEA59730CBAA1600BF10916657A0472BBA0EE20A
              F64EC936540C86E736E35FBC96B4BB1C55DDA5AA5FC80CF444023F79865CE0FC
              D400ED0DCB29FFFA6B1876E7A388FCCC4CC7DCD52776533C70626A467B95F286
              85C0C216FC8BD7A262F4A2BA3E3712E8B9F8C2936407FB260768AD69A472FB2F
              311CAE4F8BC82B4543672D35EDAF624D86FF07B4898A95D5716EE567C8D83D41
              606D2EEC3F31F8BD8DE42EBD774D5FCB950F86BB948AEDBB303DA58F8A183F77
              5FECB2D4B5BD8899B9B99D4C55B6C430DEF7FE49A46A9933672D6A35ECCE37EC
              8B57866247F7423E7763C0B2CFFD007BFD7D0B0539E00CF53BEADA5EC4C86526
              B568BCB48694BB9CBC6162A663B7EC6F66E2782E9E223C77B93B6FDAD6588A67
              BF6ED89DD9E4A923D7077434AEA6E4B16D8688FCDE4CC76AEB8FFC784A3B77FA
              E1AF7169C1239237ED14FFE7E4A4C698E9388EF000E1790FCC0171DA6A7D6FC5
              3B0E918F0C8DF731C61A258F7F0311F9BC8834579FD87D5B6DEE66F2F8FF4D59
              DF5F15912F8BC5F4957C62DB84EF16007BE36ABCEB3E6B1764AF3BF8AE7BCE3B
              6F4ED95B9D436729397F1CCFC5AE49FDE22BE51A3A43A8EE0346DE62AD34ABEA
              77C7FFB69F7C34048CEEA0EBC10D08B24944AA2BBA0E4EEB287186FA71077B71
              44FD531E6BC9249875FA2F88D08ACA7CE7CA0DE3DF0C0C0BCEA61680CDF69141
              75057BA781577092E8EC0692EE8A698D2F3B7314544584A75CCD1FBD0C68ABF5
              210E579908AB8B2F744CFB203EB7620B67567F45028B5AA635DE9A1CC115E805
              6835ABE66378678F02D62C45603588E1F6774F13EFF6C8EDEF46447C0265B65A
              5F01D0ACAC055886E6291ABE30937C3843FD50F00B9FB5C085317AD96CB0C687
              F47AB792BB295B34506888CC378A0A97E0C239289498E9DB1BCEA623CBE5E3A9
              74CC19460F6A71C82443DA9D94911F67708CBF0340359EB7D8660069A2F2167B
              A1A11A430BD9CD58A80B641DDE99A1BA42598767AC191C4BBE8CD1C0DC937578
              25673AAE3BF06E29E5A91C6BF68C87BA8CBF1FA00311E2B3EA6608ADA058591D
              AA9A5238992D70618C6657C7141291CA25330A18ADBC17A01D34913ADD098091
              39DF453E3E9240757FF89EFB51316E3AC99D52D25B45B2A45A803DE9B3EFA089
              480110207EFC00C06B59875786E72E9F11C060C323A81253F45789CEB7C6DF1B
              00D1A37B51F4A0AA76FA17AF256F586E38D19D50CA5D41685E33C0CBE47243B1
              63BF9B0898EEED20D5FD36C0B369770581851FB96B700A0CDCF749542C21E0B9
              58FBBE09D9DDB8C10DFF66078A1E56D55DFEC5EB8895DD1D8F0E2EF810B18A45
              027C339F4906C2FB764EF83EE1FA57B2F9BB78D63CE915F8BB351569A8FFF30F
              B1C782770C6EA4CA47FFFBB7A262EC51D52786F7EE2072E0A5097D26B86C78F7
              F3A42F748FA8EAFA8CDD133CB3EA8BA4DCE5770CEEDC8A2DA818EDAABA35D9D5
              46E4D02BD7F49BE80DB90CC9534770ADF8D825C3E13A9CB316B586E72E773BC2
              03B76D2795C26F1D687A02352CEDC0C7B3837D91C0CEAD682A710B4040E36112
              FFF82345CBD70D8AC3F9865A6C6BC2F39AE764ED1E5C4367AE5BC99AAC52EE0A
              CEADD842A87E9528B247551FCF0CF645FC2F6C9A900BDF1410201F0D11EF3880
              7DD1CA90C55BFE3A22CE44E9BC070AA9A10D7BD48F259B9A3458C25BCDC5C6F5
              0C347D8AB46B7608F8AA2ADF4A76B565823BB7DE100E6E55C0346D946CDC8EE7
              C39B118BE903BE2322AD685E5C817771FB7B7086FAB1450358D2318C7C86BCC5
              4ED6E125E5A92036AB9E68E51292C5EF13558D012F833E974FA702E1377F54B0
              B9AB6A3153031C95B57A01C58F6DA3A8A905111A409E023688888FAB1CED4A29
              A4503D06EC017EADB9EC50AC7D1FE17D3BC95D1A98CCD253CB32CD39F5B83EB8
              1167530B66D57C0466014BB95CA32E028DA20429D4A84F2A2432E7FE45FCF801
              626DBF25171A9CCA92D3AF471ADED9D86A7D9815852ABFC8D8748A6AC18EB3FE
              7E52A73BC703FFFFA5FE0B421BCF7FB59D72060000000049454E44AE426082}
          end>
      end>
    Left = 152
    Top = 96
  end
  object FindInFilesDialog: TFindDialog
    Options = [frDown, frHideUpDown]
    OnFind = FindInFilesDialogFind
    Left = 136
    Top = 152
  end
  object PrintDialog: TPrintDialog
    Left = 224
    Top = 149
  end
  object MemosTabSetPopupMenu: TPopupMenu
    OnPopup = MemosTabSetPopup
    Left = 48
    Top = 51
    object VCloseCurrentTab2: TMenuItem
      Caption = 'C&lose Current Tab'
      ShortCut = 16499
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
end
