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
