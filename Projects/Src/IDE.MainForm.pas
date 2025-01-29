unit IDE.MainForm;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler form
}

{x$DEFINE STATICCOMPILER}
{ For debugging purposes, remove the 'x' to have it link the compiler code into
  this program and not depend on ISCmplr.dll. You will also need to add the
  ..\Components and Src folders to the Delphi Compiler Search path in the project
  options. Also see ISCC's STATICCOMPILER and Compiler.Compile's STATICPREPROC. }

{$IFDEF STATICCOMPILER}
{$R ..\Res\ISCmplr.images.res}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls, Forms, Dialogs, CommDlg,
  Generics.Collections, UIStateForm, StdCtrls, ExtCtrls, Menus, Buttons, ComCtrls, CommCtrl,
  ScintInt, ScintEdit, IDE.ScintStylerInnoSetup, NewTabSet, ModernColors, IDE.IDEScintEdit,
  Shared.DebugStruct, Shared.CompilerInt, NewUxTheme, ImageList, ImgList, ToolWin, IDE.HelperFunc,
  VirtualImageList, BaseImageCollection;

const
  WM_StartCommandLineCompile = WM_USER + $1000;
  WM_StartCommandLineWizard = WM_USER + $1001;
  WM_StartNormally = WM_USER + $1002;

type
  PDebugEntryArray = ^TDebugEntryArray;
  TDebugEntryArray = array[0..0] of TDebugEntry;
  PVariableDebugEntryArray = ^TVariableDebugEntryArray;
  TVariableDebugEntryArray = array[0..0] of TVariableDebugEntry;
  TStepMode = (smRun, smStepInto, smStepOver, smStepOut, smRunToCursor);
  TDebugTarget = (dtSetup, dtUninstall);

const
  DebugTargetStrings: array[TDebugTarget] of String = ('Setup', 'Uninstall');

type
  TStatusMessageKind = (smkStartEnd, smkNormal, smkWarning, smkError);

  TIncludedFile = class
    Filename: String;
    CompilerFileIndex: Integer;
    LastWriteTime: TFileTime;
    HasLastWriteTime: Boolean;
    Memo: TIDEScintFileEdit;
  end;

  TIncludedFiles = TObjectList<TIncludedFile>;

  TFindResult = class
    Filename: String;
    Line, LineStartPos: Integer;
    Range: TScintRange;
    PrefixStringLength: Integer;
  end;

  TFindResults = TObjectList<TFindResult>;

  TMenuBitmaps = TDictionary<TMenuItem, HBITMAP>;
  TKeyMappedMenus = TDictionary<TShortCut, TToolButton>;

  TCallTipState = record
    StartCallTipWord: Integer;
    FunctionDefinition: AnsiString;
    BraceCount: Integer;
    LastPosCallTip: Integer;
    ClassOrRecordMember: Boolean;
    CurrentCallTipWord: String;
    CurrentCallTip: Integer;
    MaxCallTips: Integer;
  end;

  TUpdatePanelMessage = class
    Msg, ConfigIdent: String;
    ConfigValue: Integer;
    Color: TColor;
    constructor Create(const AMsg, AConfigIdent: String; const AConfigValue: Integer; const AColor: TColor);
  end;

  TUpdatePanelMessages = TObjectList<TUpdatePanelMessage>;

  TMainForm = class(TUIStateForm)
    MainMenu1: TMainMenu;
    FMenu: TMenuItem;
    FNewMainFile: TMenuItem;
    FOpenMainFile: TMenuItem;
    FSave: TMenuItem;
    FSaveMainFileAs: TMenuItem;
    N1: TMenuItem;
    BCompile: TMenuItem;
    N2: TMenuItem;
    FExit: TMenuItem;
    EMenu: TMenuItem;
    EUndo: TMenuItem;
    N3: TMenuItem;
    ECut: TMenuItem;
    ECopy: TMenuItem;
    EPaste: TMenuItem;
    EDelete: TMenuItem;
    N4: TMenuItem;
    ESelectAll: TMenuItem;
    VMenu: TMenuItem;
    EFind: TMenuItem;
    EFindNext: TMenuItem;
    EReplace: TMenuItem;
    HMenu: TMenuItem;
    HDoc: TMenuItem;
    HAbout: TMenuItem;
    FRecent: TMenuItem;
    FClearRecent: TMenuItem;
    N6: TMenuItem;
    VCompilerOutput: TMenuItem;
    FindDialog: TFindDialog;
    ReplaceDialog: TReplaceDialog;
    StatusPanel: TPanel;
    CompilerOutputList: TListBox;
    SplitPanel: TPanel;
    HWebsite: TMenuItem;
    VToolbar: TMenuItem;
    N7: TMenuItem;
    TOptions: TMenuItem;
    HFaq: TMenuItem;
    StatusBar: TStatusBar;
    BodyPanel: TPanel;
    VStatusBar: TMenuItem;
    ERedo: TMenuItem;
    RMenu: TMenuItem;
    RStepInto: TMenuItem;
    RStepOver: TMenuItem;
    N5: TMenuItem;
    RRun: TMenuItem;
    RRunToCursor: TMenuItem;
    N10: TMenuItem;
    REvaluate: TMenuItem;
    CheckIfRunningTimer: TTimer;
    RPause: TMenuItem;
    RParameters: TMenuItem;
    OutputListPopupMenu: TMenuItem;
    POutputListCopy: TMenuItem;
    HISPPSep: TMenuItem;
    N12: TMenuItem;
    BStopCompile: TMenuItem;
    HISPPDoc: TMenuItem;
    N13: TMenuItem;
    EGoto: TMenuItem;
    RTerminate: TMenuItem;
    BMenu: TMenuItem;
    BLowPriority: TMenuItem;
    HDonate: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    RTargetSetup: TMenuItem;
    RTargetUninstall: TMenuItem;
    OutputTabSet: TNewTabSet;
    DebugOutputList: TListBox;
    VDebugOutput: TMenuItem;
    VHide: TMenuItem;
    N11: TMenuItem;
    TMenu: TMenuItem;
    TAddRemovePrograms: TMenuItem;
    RToggleBreakPoint: TMenuItem;
    RDeleteBreakPoints: TMenuItem;
    HWhatsNew: TMenuItem;
    TGenerateGUID: TMenuItem;
    TSignTools: TMenuItem;
    N16: TMenuItem;
    HExamples: TMenuItem;
    N17: TMenuItem;
    BOpenOutputFolder: TMenuItem;
    N8: TMenuItem;
    VZoom: TMenuItem;
    VZoomIn: TMenuItem;
    VZoomOut: TMenuItem;
    N9: TMenuItem;
    VZoomReset: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    FSaveEncoding: TMenuItem;
    FSaveEncodingAuto: TMenuItem;
    FSaveEncodingUTF8WithBOM: TMenuItem;
    ToolBar: TToolBar;
    BackNavButton: TToolButton;
    ForwardNavButton: TToolButton;
    ToolButton1: TToolButton;
    NewMainFileButton: TToolButton;
    OpenMainFileButton: TToolButton;
    SaveButton: TToolButton;
    ToolButton2: TToolButton;
    CompileButton: TToolButton;
    StopCompileButton: TToolButton;
    ToolButton3: TToolButton;
    RunButton: TToolButton;
    PauseButton: TToolButton;
    ToolButton4: TToolButton;
    TargetSetupButton: TToolButton;
    TargetUninstallButton: TToolButton;
    ToolButton5: TToolButton;
    HelpButton: TToolButton;
    Bevel1: TBevel;
    TerminateButton: TToolButton;
    ThemedToolbarVirtualImageList: TVirtualImageList;
    LightToolbarVirtualImageList: TVirtualImageList;
    POutputListSelectAll: TMenuItem;
    DebugCallStackList: TListBox;
    VDebugCallStack: TMenuItem;
    TMsgBoxDesigner: TMenuItem;
    TRegistryDesigner: TMenuItem;
    ToolBarPanel: TPanel;
    HMailingList: TMenuItem;
    MemosTabSet: TNewTabSet; { First tab is the main memo, last tab is the preprocessor output memo }
    FSaveAll: TMenuItem;
    RStepOut: TMenuItem;
    VNextTab: TMenuItem;
    VPreviousTab: TMenuItem;
    N20: TMenuItem;
    HShortcutsDoc: TMenuItem;
    HRegExDoc: TMenuItem;
    N21: TMenuItem;
    EFindPrevious: TMenuItem;
    FindResultsList: TListBox;
    VFindResults: TMenuItem;
    EFindInFiles: TMenuItem;
    FindInFilesDialog: TFindDialog;
    FPrint: TMenuItem;
    N22: TMenuItem;
    PrintDialog: TPrintDialog;
    FSaveEncodingUTF8WithoutBOM: TMenuItem;
    TFilesDesigner: TMenuItem;
    VCloseCurrentTab: TMenuItem;
    VReopenTab: TMenuItem;
    VReopenTabs: TMenuItem;
    MemosTabSetPopupMenu: TMenuItem;
    VCloseCurrentTab2: TMenuItem;
    VReopenTab2: TMenuItem;
    VReopenTabs2: TMenuItem;
    NavPopupMenu: TMenuItem;
    N23: TMenuItem;
    ThemedMarkersAndACVirtualImageList: TVirtualImageList;
    ESelectNextOccurrence: TMenuItem;
    ESelectAllOccurrences: TMenuItem;
    BreakPointsPopupMenu: TMenuItem;
    RToggleBreakPoint2: TMenuItem;
    RDeleteBreakPoints2: TMenuItem;
    N24: TMenuItem;
    VWordWrap: TMenuItem;
    N25: TMenuItem;
    ESelectAllFindMatches: TMenuItem;
    EToggleLinesComment: TMenuItem;
    EBraceMatch: TMenuItem;
    EFoldLine: TMenuItem;
    EUnfoldLine: TMenuItem;
    EFindRegEx: TMenuItem;
    UpdatePanel: TPanel;
    UpdateLinkLabel: TLinkLabel;
    UpdatePanelClosePaintBox: TPaintBox;
    UpdatePanelDonateImage: TImage;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FExitClick(Sender: TObject);
    procedure FOpenMainFileClick(Sender: TObject);
    procedure EUndoClick(Sender: TObject);
    procedure EMenuClick(Sender: TObject);
    procedure ECutClick(Sender: TObject);
    procedure ECopyClick(Sender: TObject);
    procedure EPasteClick(Sender: TObject);
    procedure EDeleteClick(Sender: TObject);
    procedure FSaveClick(Sender: TObject);
    procedure ESelectAllClick(Sender: TObject);
    procedure FNewMainFileClick(Sender: TObject);
    procedure FNewMainFileUserWizardClick(Sender: TObject);
    procedure HDocClick(Sender: TObject);
    procedure BCompileClick(Sender: TObject);
    procedure FMenuClick(Sender: TObject);
    procedure FMRUClick(Sender: TObject);
    procedure VCompilerOutputClick(Sender: TObject);
    procedure HAboutClick(Sender: TObject);
    procedure EFindClick(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure EReplaceClick(Sender: TObject);
    procedure ReplaceDialogReplace(Sender: TObject);
    procedure EFindNextOrPreviousClick(Sender: TObject);
    procedure SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure VMenuClick(Sender: TObject);
    procedure HWebsiteClick(Sender: TObject);
    procedure VToolbarClick(Sender: TObject);
    procedure TOptionsClick(Sender: TObject);
    procedure HFaqClick(Sender: TObject);
    procedure HISPPDocClick(Sender: TObject);
    procedure VStatusBarClick(Sender: TObject);
    procedure ERedoClick(Sender: TObject);
    procedure StatusBarResize(Sender: TObject);
    procedure RStepIntoClick(Sender: TObject);
    procedure RStepOverClick(Sender: TObject);
    procedure RRunToCursorClick(Sender: TObject);
    procedure RRunClick(Sender: TObject);
    procedure REvaluateClick(Sender: TObject);
    procedure CheckIfRunningTimerTimer(Sender: TObject);
    procedure RPauseClick(Sender: TObject);
    procedure RParametersClick(Sender: TObject);
    procedure POutputListCopyClick(Sender: TObject);
    procedure BStopCompileClick(Sender: TObject);
    procedure EGotoClick(Sender: TObject);
    procedure RTerminateClick(Sender: TObject);
    procedure BMenuClick(Sender: TObject);
    procedure BLowPriorityClick(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure HDonateClick(Sender: TObject);
    procedure RTargetClick(Sender: TObject);
    procedure DebugOutputListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure OutputTabSetClick(Sender: TObject);
    procedure VHideClick(Sender: TObject);
    procedure VDebugOutputClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TAddRemoveProgramsClick(Sender: TObject);
    procedure RToggleBreakPointClick(Sender: TObject);
    procedure RDeleteBreakPointsClick(Sender: TObject);
    procedure HWhatsNewClick(Sender: TObject);
    procedure TGenerateGUIDClick(Sender: TObject);
    procedure TSignToolsClick(Sender: TObject);
    procedure HExamplesClick(Sender: TObject);
    procedure BOpenOutputFolderClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure VZoomInClick(Sender: TObject);
    procedure VZoomOutClick(Sender: TObject);
    procedure VZoomResetClick(Sender: TObject);
    procedure FSaveEncodingItemClick(Sender: TObject);
    procedure CompilerOutputListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure POutputListSelectAllClick(Sender: TObject);
    procedure DebugCallStackListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure VDebugCallStackClick(Sender: TObject);
    procedure HMailingListClick(Sender: TObject);
    procedure TMsgBoxDesignerClick(Sender: TObject);
    procedure TRegistryDesignerClick(Sender: TObject);
    procedure MemosTabSetClick(Sender: TObject);
    procedure FSaveAllClick(Sender: TObject);
    procedure RStepOutClick(Sender: TObject);
    procedure TMenuClick(Sender: TObject);
    procedure VNextTabClick(Sender: TObject);
    procedure VPreviousTabClick(Sender: TObject);
    procedure HShortcutsDocClick(Sender: TObject);
    procedure HRegExDocClick(Sender: TObject);
    procedure VFindResultsClick(Sender: TObject);
    procedure EFindInFilesClick(Sender: TObject);
    procedure FindInFilesDialogFind(Sender: TObject);
    procedure FindResultsListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure FindResultsListDblClick(Sender: TObject);
    procedure FPrintClick(Sender: TObject);
    procedure TFilesDesignerClick(Sender: TObject);
    procedure VCloseCurrentTabClick(Sender: TObject);
    procedure VReopenTabsClick(Sender: TObject);
    procedure MemosTabSetPopupMenuClick(Sender: TObject);
    procedure MemosTabSetOnCloseButtonClick(Sender: TObject; Index: Integer);
    procedure StatusBarClick(Sender: TObject);
    procedure SimpleMenuClick(Sender: TObject);
    procedure OutputListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RMenuClick(Sender: TObject);
    procedure BackNavButtonClick(Sender: TObject);
    procedure ForwardNavButtonClick(Sender: TObject);
    procedure NavPopupMenuClick(Sender: TObject);
    procedure ESelectNextOccurrenceClick(Sender: TObject);
    procedure ESelectAllOccurrencesClick(Sender: TObject);
    procedure BreakPointsPopupMenuClick(Sender: TObject);
    procedure FClearRecentClick(Sender: TObject);
    procedure VWordWrapClick(Sender: TObject);
    procedure ESelectAllFindMatchesClick(Sender: TObject);
    procedure EToggleLinesCommentClick(Sender: TObject);
    procedure EBraceMatchClick(Sender: TObject);
    procedure EFoldOrUnfoldLineClick(Sender: TObject);
    procedure EFindRegExClick(Sender: TObject);
    procedure UpdateLinkLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure UpdatePanelClosePaintBoxPaint(Sender: TObject);
    procedure UpdatePanelClosePaintBoxClick(Sender: TObject);
    procedure UpdatePanelDonateImageClick(Sender: TObject);
  private
    { Private declarations }
    FMemos: TList<TIDEScintEdit>;                      { FMemos[0] is the main memo and FMemos[1] the preprocessor output memo - also see MemosTabSet comment above }
    FMainMemo: TIDEScintFileEdit;                      { Doesn't change }
    FPreprocessorOutputMemo: TIDEScintEdit;            { Doesn't change and is the only memo which isnt a TIDEScint*File*Edit}
    FFileMemos: TList<TIDEScintFileEdit>;              { All memos except FPreprocessorOutputMemo, including those without a tab }
    FHiddenFiles: TStringList;                          { List of files which *do* use a memo but are hidden by the user and have no tab }
    FActiveMemo: TIDEScintEdit;                        { Changes depending on user input }
    FErrorMemo, FStepMemo: TIDEScintFileEdit;          { These change depending on user input }
    FMemosStyler: TInnoSetupStyler;                     { Single styler for all memos }
    FCompilerVersion: PCompilerVersionInfo;
    FMRUMainFilesMenuItems: array[0..MRUListMaxCount-1] of TMenuItem;
    FMRUMainFilesList: TStringList;
    FMRUParametersList: TStringList;
    FOptions: record
      ShowStartupForm: Boolean;
      UseWizard: Boolean;
      Autosave: Boolean;
      MakeBackups: Boolean;
      FullPathInTitleBar: Boolean;
      UndoAfterSave: Boolean;
      PauseOnDebuggerExceptions: Boolean;
      RunAsDifferentUser: Boolean;
      AutoAutoComplete: Boolean;
      AutoCallTips: Boolean;
      UseSyntaxHighlighting: Boolean;
      ColorizeCompilerOutput: Boolean;
      UnderlineErrors: Boolean;
      HighlightWordAtCursorOccurrences: Boolean;
      HighlightSelTextOccurrences: Boolean;
      CursorPastEOL: Boolean;
      TabWidth: Integer;
      UseTabCharacter: Boolean;
      ShowWhiteSpace: Boolean;
      UseFolding: Boolean;
      FindRegEx: Boolean;
      WordWrap: Boolean;
      AutoIndent: Boolean;
      IndentationGuides: Boolean;
      LowPriorityDuringCompile: Boolean;
      GutterLineNumbers: Boolean;
      KeyMappingType: TKeyMappingType;
      MemoKeyMappingType: TIDEScintKeyMappingType;
      ThemeType: TThemeType;
      ShowPreprocessorOutput: Boolean;
      OpenIncludedFiles: Boolean;
      ShowCaretPosition: Boolean;
    end;
    FOptionsLoaded: Boolean;
    FTheme: TTheme;
    FSignTools: TStringList;
    FFindResults: TFindResults;
    FCompiling: Boolean;
    FCompileWantAbort: Boolean;
    FBecameIdle: Boolean;
    FModifiedAnySinceLastCompile, FModifiedAnySinceLastCompileAndGo: Boolean;
    FDebugEntries: PDebugEntryArray;
    FDebugEntriesCount: Integer;
    FVariableDebugEntries: PVariableDebugEntryArray;
    FVariableDebugEntriesCount: Integer;
    FCompiledCodeText: AnsiString;
    FCompiledCodeDebugInfo: AnsiString;
    FDebugClientWnd: HWND;
    FProcessHandle, FDebugClientProcessHandle: THandle;
    FDebugTarget: TDebugTarget;
    FCompiledExe, FUninstExe, FTempDir: String;
    FPreprocessorOutput: String;
    FIncludedFiles: TIncludedFiles;
    FDebugging: Boolean;
    FStepMode: TStepMode;
    FPaused, FPausedAtCodeLine: Boolean;
    FRunToCursorPoint: TDebugEntry;
    FReplyString: String;
    FDebuggerException: String;
    FRunParameters: String;
    FLastFindOptions: TFindOptions;
    FLastFindRegEx: Boolean;
    FLastFindText: String;
    FLastReplaceText: String;
    FLastEvaluateConstantText: String;
    FSavePriorityClass: DWORD;
    FBuildAnimationFrame: Cardinal;
    FLastAnimationTick: DWORD;
    FProgress, FProgressMax: Cardinal;
    FTaskbarProgressValue: Cardinal;
    FProgressThemeData: HTHEME;
    FMenuThemeData: HTHEME;
    FToolbarThemeData: HTHEME;
    FMenuDarkBackgroundBrush: TBrush;
    FMenuDarkHotOrSelectedBrush: TBrush;
    FDebugLogListTimestampsWidth: Integer;
    FOnPendingSquiggly: Boolean;
    FPendingSquigglyCaretPos: Integer;
    FCallStackCount: Cardinal;
    FDevMode, FDevNames: HGLOBAL;
    FMenuImageList: TVirtualImageList;
    FMenuBitmaps: TMenuBitmaps;
    FMenuBitmapsSize: TSize;
    FMenuBitmapsSourceImageCollection: TCustomImageCollection;
    FSynchingZoom: Boolean;
    FNavStacks: TIDEScintEditNavStacks;
    FCurrentNavItem: TIDEScintEditNavItem;
    FKeyMappedMenus: TKeyMappedMenus;
    FBackNavButtonShortCut, FForwardNavButtonShortCut: TShortCut;
    FBackNavButtonShortCut2, FForwardNavButtonShortCut2: TShortCut;
    FIgnoreTabSetClick: Boolean;
    FFirstTabSelectShortCut, FLastTabSelectShortCut: TShortCut;
    FCompileShortCut2: TShortCut;
    FCallTipState: TCallTipState;
    FUpdatePanelMessages: TUpdatePanelMessages;
    function AnyMemoHasBreakPoint: Boolean;
    class procedure AppOnException(Sender: TObject; E: Exception);
    procedure AppOnActivate(Sender: TObject);
    class procedure AppOnGetActiveFormHandle(var AHandle: HWND);
    procedure AppOnIdle(Sender: TObject; var Done: Boolean);
    function AskToDetachDebugger: Boolean;
    procedure BringToForeground;
    procedure BuildAndSaveBreakPointLines(const AMemo: TIDEScintFileEdit);
    procedure BuildAndSaveKnownIncludedAndHiddenFiles;
    procedure CheckIfTerminated;
    procedure ClearMRUMainFilesList;
    procedure CloseTab(const TabIndex: Integer);
    procedure CompileFile(AFilename: String; const ReadFromFile: Boolean);
    procedure CompileIfNecessary;
    function ConfirmCloseFile(const PromptToSave: Boolean): Boolean;
    procedure DebuggingStopped(const WaitForTermination: Boolean);
    procedure DebugLogMessage(const S: String);
    procedure DebugShowCallStack(const CallStack: String; const CallStackCount: Cardinal);
    function DestroyLineState(const AMemo: TIDEScintFileEdit): Boolean;
    procedure DestroyDebugInfo;
    procedure DetachDebugger;
    function EvaluateConstant(const S: String; out Output: String): Integer;
    function EvaluateVariableEntry(const DebugEntry: PVariableDebugEntry;
      out Output: String): Integer;
    procedure FindNext(const ReverseDirection: Boolean);
    function FindSetupDirectiveValue(const DirectiveName,
      DefaultValue: String): String; overload;
    function FindSetupDirectiveValue(const DirectiveName: String;
      DefaultValue: Boolean): Boolean; overload;
    function FromCurrentPPI(const XY: Integer): Integer;
    function GetBorderStyle: TFormBorderStyle;
    procedure Go(AStepMode: TStepMode);
    procedure HideError;
    procedure InitializeFindText(Dlg: TFindDialog);
    function InitializeFileMemo(const Memo: TIDEScintFileEdit; const PopupMenu: TPopupMenu): TIDEScintFileEdit;
    function InitializeMainMemo(const Memo: TIDEScintFileEdit; const PopupMenu: TPopupMenu): TIDEScintFileEdit;
    function InitializeMemoBase(const Memo: TIDEScintEdit; const PopupMenu: TPopupMenu): TIDEScintEdit;
    function InitializeNonFileMemo(const Memo: TIDEScintEdit; const PopupMenu: TPopupMenu): TIDEScintEdit;
    function InitiateAutoCompleteOrCallTipAllowedAtPos(const AMemo: TIDEScintEdit;
      const WordStartLinePos, PositionBeforeWordStartPos: Integer): Boolean;
    procedure InitiateAutoComplete(const Key: AnsiChar);
    procedure UpdateCallTipFunctionDefinition(const Pos: Integer = -1);
    procedure InitiateCallTip(const Key: AnsiChar);
    procedure ContinueCallTip;
    procedure InvalidateStatusPanel(const Index: Integer);
    procedure LoadBreakPointLinesAndUpdateLineMarkers(const AMemo: TIDEScintFileEdit);
    procedure LoadKnownIncludedAndHiddenFilesAndUpdateMemos(const AFilename: String);
    procedure MemoCallTipArrowClick(Sender: TObject; const Up: Boolean);
    procedure MemoChange(Sender: TObject; const Info: TScintEditChangeInfo);
    procedure MemoCharAdded(Sender: TObject; Ch: AnsiChar);
    procedure MainMemoDropFiles(Sender: TObject; X, Y: Integer; AFiles: TStrings);
    procedure MemoHintShow(Sender: TObject; var Info: TScintHintInfo);
    procedure MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MemoKeyPress(Sender: TObject; var Key: Char);
    procedure MemoLinesDeleted(Memo: TIDEScintFileEdit; FirstLine, Count, FirstAffectedLine: Integer);
    procedure MemoLinesInserted(Memo: TIDEScintFileEdit; FirstLine, Count: integer);
    procedure MemoMarginClick(Sender: TObject; MarginNumber: Integer;
      Line: Integer);
    procedure MemoMarginRightClick(Sender: TObject; MarginNumber: Integer;
      Line: Integer);
    procedure MemoModifiedChange(Sender: TObject);
    function MemoToTabIndex(const AMemo: TIDEScintEdit): Integer;
    procedure MemoUpdateUI(Sender: TObject; Updated: TScintEditUpdates);
    procedure MemoZoom(Sender: TObject);
    function MultipleSelectionPasteFromClipboard(const AMemo: TIDESCintEdit): Boolean;
    procedure UpdateReopenTabMenu(const Menu: TMenuItem);
    procedure ModifyMRUMainFilesList(const AFilename: String; const AddNewItem: Boolean);
    procedure ModifyMRUParametersList(const AParameter: String; const AddNewItem: Boolean);
    procedure MoveCaretAndActivateMemo(AMemo: TIDEScintEdit; const LineNumberOrPosition: Integer;
      const AlwaysResetColumnEvenIfOnRequestedLineAlready: Boolean;
      const IsPosition: Boolean = False; const PositionVirtualSpace: Integer = 0);
    procedure NavItemClick(Sender: TObject);
    procedure NewMainFile;
    procedure NewMainFileUsingWizard;
    procedure OpenFile(AMemo: TIDEScintFileEdit; AFilename: String; const MainMemoAddToRecentDocs: Boolean);
    procedure OpenMRUMainFile(const AFilename: String);
    procedure ParseDebugInfo(DebugInfo: Pointer);
    procedure ReadMRUMainFilesList;
    procedure ReadMRUParametersList;
    procedure RemoveMemoFromNav(const AMemo: TIDEScintEdit);
    procedure RemoveMemoBadLinesFromNav(const AMemo: TIDEScintEdit);
    procedure ReopenTabClick(Sender: TObject);
    procedure ReopenTabOrTabs(const HiddenFileIndex: Integer; const Activate: Boolean);
    procedure ResetAllMemosLineState;
    procedure StartProcess;
    function SaveFile(const AMemo: TIDEScintFileEdit; const SaveAs: Boolean): Boolean;
    procedure SetBorderStyle(Value: TFormBorderStyle);
    procedure SetErrorLine(const AMemo: TIDEScintFileEdit; const ALine: Integer);
    procedure SetStatusPanelVisible(const AVisible: Boolean);
    procedure SetStepLine(const AMemo: TIDEScintFileEdit; ALine: Integer);
    procedure ShowOpenMainFileDialog(const Examples: Boolean);
    procedure StatusMessage(const Kind: TStatusMessageKind; const S: String);
    function StoreAndTestLastFindOptions(Sender: TObject): Boolean;
    function TestLastFindOptions: Boolean;
    procedure SyncEditorOptions;
    function TabIndexToMemo(const ATabIndex, AMaxTabIndex: Integer): TIDEScintEdit;
    function ToCurrentPPI(const XY: Integer): Integer;
    procedure ToggleBreakPoint(Line: Integer);
    procedure UpdateAllMemoLineMarkers(const AMemo: TIDEScintFileEdit);
    procedure UpdateAllMemosLineMarkers;
    procedure UpdateBevel1Visibility;
    procedure UpdateCaption;
    procedure UpdateCaretPosPanelAndBackNavStack;
    procedure UpdateCompileStatusPanels(const AProgress, AProgressMax: Cardinal;
      const ASecondsRemaining: Integer; const ABytesCompressedPerSecond: Cardinal);
    procedure UpdateEditModePanel;
    procedure UpdateFindRegExPanel;
    procedure UpdatePreprocMemos;
    procedure UpdateLineMarkers(const AMemo: TIDEScintFileEdit; const Line: Integer);
    procedure UpdateImages;
    procedure UpdateMarginsAndAutoCompleteIcons;
    procedure UpdateMarginsAndSquigglyAndCaretWidths;
    procedure UpdateMemosTabSetVisibility;
    procedure UpdateMenuBitmapsIfNeeded;
    procedure UpdateModifiedPanel;
    procedure UpdateNavButtons;
    procedure UpdateNewMainFileButtons;
    procedure UpdateOccurrenceIndicators(const AMemo: TIDEScintEdit);
    procedure UpdateOutputTabSetListsItemHeightAndDebugTimeWidth;
    procedure UpdateRunMenu;
    procedure UpdateSaveMenuItemAndButton;
    procedure UpdateTargetMenu;
    procedure UpdateUpdatePanel;
    procedure UpdateKeyMapping;
    procedure UpdateTheme;
    procedure UpdateThemeData(const Open: Boolean);
    procedure ApplyMenuBitmaps(const ParentMenuItem: TMenuItem);
    procedure UpdateStatusPanelHeight(H: Integer);
    procedure WMAppCommand(var Message: TMessage); message WM_APPCOMMAND;
    procedure WMCopyData(var Message: TWMCopyData); message WM_COPYDATA;
    procedure WMDebuggerHello(var Message: TMessage); message WM_Debugger_Hello;
    procedure WMDebuggerGoodbye(var Message: TMessage); message WM_Debugger_Goodbye;
    procedure WMDebuggerQueryVersion(var Message: TMessage); message WM_Debugger_QueryVersion;
    procedure GetMemoAndDebugEntryFromMessage(Kind, Index: Integer; var Memo: TIDEScintFileEdit;
      var DebugEntry: PDebugEntry);
    procedure DebuggerStepped(var Message: TMessage; const Intermediate: Boolean);
    procedure WMDebuggerStepped(var Message: TMessage); message WM_Debugger_Stepped;
    procedure WMDebuggerSteppedIntermediate(var Message: TMessage); message WM_Debugger_SteppedIntermediate;
    procedure WMDebuggerException(var Message: TMessage); message WM_Debugger_Exception;
    procedure WMDebuggerSetForegroundWindow(var Message: TMessage); message WM_Debugger_SetForegroundWindow;
    procedure WMDebuggerCallStackCount(var Message: TMessage); message WM_Debugger_CallStackCount;
    procedure WMStartCommandLineCompile(var Message: TMessage); message WM_StartCommandLineCompile;
    procedure WMStartCommandLineWizard(var Message: TMessage); message WM_StartCommandLineWizard;
    procedure WMStartNormally(var Message: TMessage); message WM_StartNormally;
    procedure WMDPIChanged(var Message: TMessage); message WM_DPICHANGED;
    procedure WMSettingChange(var Message: TMessage); message WM_SETTINGCHANGE;
    procedure WMSysColorChange(var Message: TMessage); message WM_SYSCOLORCHANGE;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    procedure WMUAHDrawMenu(var Message: TMessage); message WM_UAHDRAWMENU;
    procedure WMUAHDrawMenuItem(var Message: TMessage); message WM_UAHDRAWMENUITEM;
    procedure UAHDrawMenuBottomLine;
    procedure WMNCActivate(var Message: TMessage); message WM_NCACTIVATE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsShortCut(var Message: TWMKey): Boolean; override;
  published
    property BorderStyle: TFormBorderStyle read GetBorderStyle write SetBorderStyle;
  end;

var
  MainForm: TMainForm;
  CommandLineFilename, CommandLineWizardName: String;
  CommandLineCompile: Boolean;
  CommandLineWizard: Boolean;

implementation

uses
  ActiveX, Clipbrd, ShellApi, ShlObj, IniFiles, Registry, Consts, Types, UITypes,
  Math, StrUtils, WideStrUtils,
  PathFunc, Shared.CommonFunc.Vcl, Shared.CommonFunc, Shared.FileClass, IDE.Messages, NewUxTheme.TmSchema, BrowseFunc,
  IDE.HtmlHelpFunc, TaskbarProgressFunc, IDE.ImagesModule,
  {$IFDEF STATICCOMPILER} Compiler.Compile, {$ENDIF}
  IDE.OptionsForm, IDE.StartupForm, IDE.Wizard.WizardForm, IDE.SignToolsForm,
  Shared.ConfigIniFile, Shared.SignToolsFunc, IDE.InputQueryComboForm, IDE.MsgBoxDesignerForm,
  IDE.FilesDesignerForm, IDE.RegistryDesignerForm, IDE.Wizard.WizardFormRegistryHelper;

{$R *.DFM}

const
  { Memos }
  MaxMemos = 22; { Includes the main and preprocessor output memos }
  FirstIncludedFilesMemoIndex = 1; { This is an index into FFileMemos }

  { Status bar panel indexes }
  spCaretPos = 0;
  spModified = 1;
  spEditMode = 2;
  spFindRegEx = 3;
  spHiddenFilesCount = 4;
  spCompileIcon = 5;
  spCompileProgress = 6;
  spExtraStatus = 7;

  { Output tab set indexes }
  tiCompilerOutput = 0;
  tiDebugOutput = 1;
  tiDebugCallStack = 2;
  tiFindResults = 3;

  LineStateGrowAmount = 4000;

{ TUpdatePanelMessage }

constructor TUpdatePanelMessage.Create(const AMsg, AConfigIdent: String;
  const AConfigValue: Integer; const AColor: TColor);
begin
  Msg := AMsg;
  ConfigIdent := AConfigIdent;
  ConfigValue := AConfigValue;
  Color := AColor;
end;

{ TMainFormPopupMenu }

type
  TMainFormPopupMenu = class(TPopupMenu)
  private
    FParentMenuItem: TMenuItem;
  public
    constructor Create(const AOwner: TComponent; const ParentMenuItem: TMenuItem); reintroduce; virtual;
    procedure Popup(X, Y: Integer); override;
  end;

constructor TMainFormPopupMenu.Create(const AOwner: TComponent; const ParentMenuItem: TMenuItem);
begin
  inherited Create(AOwner);
  FParentMenuItem := ParentMenuItem;
end;

procedure TMainFormPopupMenu.Popup(X, Y: Integer);
var
  Form: TMainForm;
begin
  { Show the existing main menu's submenu }
  Form := Owner as TMainForm;
  var OldVisible := FParentMenuItem.Visible; { See ApplyMenuBitmaps }
  FParentMenuItem.Visible := True;
  try
    TrackPopupMenu(FParentMenuItem.Handle, TPM_RIGHTBUTTON, X, Y, 0, Form.Handle, nil);
  finally
    FParentMenuItem.Visible := OldVisible;
  end;
end;

{ TMainForm }

function TMainForm.InitializeMemoBase(const Memo: TIDEScintEdit; const PopupMenu: TPopupMenu): TIDEScintEdit;
begin
  Memo.Align := alClient;
  Memo.Font.Name := GetPreferredMemoFont; { Default font only, see ReadConfig }
  Memo.Font.Size := 10;
  Memo.ShowHint := True;
  Memo.Styler := FMemosStyler;
  Memo.PopupMenu := PopupMenu;
  Memo.OnCallTipArrowClick := MemoCallTipArrowClick;
  Memo.OnChange := MemoChange;
  Memo.OnCharAdded := MemoCharAdded;
  Memo.OnHintShow := MemoHintShow;
  Memo.OnKeyDown := MemoKeyDown;
  Memo.OnKeyPress := MemoKeyPress;
  Memo.OnMarginClick := MemoMarginClick;
  Memo.OnMarginRightClick := MemoMarginRightClick;
  Memo.OnModifiedChange := MemoModifiedChange;
  Memo.OnUpdateUI := MemoUpdateUI;
  Memo.OnZoom := MemoZoom;
  Memo.Parent := BodyPanel;
  Memo.SetAutoCompleteSeparators(InnoSetupStylerWordListSeparator, InnoSetupStylerWordListTypeSeparator);
  Memo.SetWordChars(Memo.GetDefaultWordChars+'#{}[]');
  Memo.Theme := FTheme;
  Memo.Visible := False;
  Result := Memo;
end;

function TMainForm.InitializeFileMemo(const Memo: TIDEScintFileEdit; const PopupMenu: TPopupMenu): TIDEScintFileEdit;
begin
  InitializeMemoBase(Memo, PopupMenu);
  Memo.ChangeHistory := schMarkers;
  Memo.CompilerFileIndex := UnknownCompilerFileIndex;
  Memo.ErrorLine := -1;
  Memo.StepLine := -1;
  Result := Memo;
end;

function TMainForm.InitializeMainMemo(const Memo: TIDEScintFileEdit; const PopupMenu: TPopupMenu): TIDEScintFileEdit;
begin
  InitializeFileMemo(Memo, PopupMenu);
  Memo.AcceptDroppedFiles := True;
  Memo.CompilerFileIndex := -1;
  Memo.OnDropFiles := MainMemoDropFiles;
  Memo.Used := True;
  Result := Memo;
end;

function TMainForm.InitializeNonFileMemo(const Memo: TIDEScintEdit; const PopupMenu: TPopupMenu): TIDEScintEdit;
begin
  InitializeMemoBase(Memo, PopupMenu);
  Memo.ReadOnly := True;
  Result := Memo;
end;

constructor TMainForm.Create(AOwner: TComponent);

  procedure CheckUpdatePanelMessage(const Ini: TConfigIniFile; const ConfigIdent: String;
    const ConfigValueDefault, ConfigValueMinimum: Integer; const Msg: String; const Color: TColor);
  begin
    var ConfigValue := Ini.ReadInteger('UpdatePanel', ConfigIdent, ConfigValueDefault);
    if ConfigValue < ConfigValueMinimum then
      FUpdatePanelMessages.Add(TUpdatePanelMessage.Create(Msg, ConfigIdent, ConfigValueMinimum, Color));
  end;

  procedure ReadConfig;
  var
    Ini: TConfigIniFile;
    WindowPlacement: TWindowPlacement;
    I: Integer;
    Memo: TIDEScintEdit;
  begin
    Ini := TConfigIniFile.Create;
    try
      { Menu check boxes state }
      ToolbarPanel.Visible := Ini.ReadBool('Options', 'ShowToolbar', True);
      StatusBar.Visible := Ini.ReadBool('Options', 'ShowStatusBar', True);
      FOptions.LowPriorityDuringCompile := Ini.ReadBool('Options', 'LowPriorityDuringCompile', False);

      { Configuration options }
      FOptions.ShowStartupForm := Ini.ReadBool('Options', 'ShowStartupForm', True);
      FOptions.UseWizard := Ini.ReadBool('Options', 'UseWizard', True);
      FOptions.Autosave := Ini.ReadBool('Options', 'Autosave', False);
      FOptions.MakeBackups := Ini.ReadBool('Options', 'MakeBackups', False);
      FOptions.FullPathInTitleBar := Ini.ReadBool('Options', 'FullPathInTitleBar', False);
      FOptions.UndoAfterSave := Ini.ReadBool('Options', 'UndoAfterSave', True);
      FOptions.PauseOnDebuggerExceptions := Ini.ReadBool('Options', 'PauseOnDebuggerExceptions', True);
      FOptions.RunAsDifferentUser := Ini.ReadBool('Options', 'RunAsDifferentUser', False);
      FOptions.AutoAutoComplete := Ini.ReadBool('Options', 'AutoComplete', True);
      FOptions.AutoCallTips := Ini.ReadBool('Options', 'AutoCallTips', True);
      FOptions.UseSyntaxHighlighting := Ini.ReadBool('Options', 'UseSynHigh', True);
      FOptions.ColorizeCompilerOutput := Ini.ReadBool('Options', 'ColorizeCompilerOutput', True);
      FOptions.UnderlineErrors := Ini.ReadBool('Options', 'UnderlineErrors', True);
      FOptions.HighlightWordAtCursorOccurrences := Ini.ReadBool('Options', 'HighlightWordAtCursorOccurrences', False);
      FOptions.HighlightSelTextOccurrences := Ini.ReadBool('Options', 'HighlightSelTextOccurrences', True);
      FOptions.CursorPastEOL := Ini.ReadBool('Options', 'EditorCursorPastEOL', False);
      FOptions.TabWidth := Ini.ReadInteger('Options', 'TabWidth', 2);
      FOptions.UseTabCharacter := Ini.ReadBool('Options', 'UseTabCharacter', False);
      FOptions.ShowWhiteSpace := Ini.ReadBool('Options', 'ShowWhiteSpace', False);
      FOptions.UseFolding := Ini.ReadBool('Options', 'UseFolding', True);
      FOptions.FindRegEx := Ini.ReadBool('Options', 'FindRegEx', False);
      FOptions.WordWrap := Ini.ReadBool('Options', 'WordWrap', False);
      FOptions.AutoIndent := Ini.ReadBool('Options', 'AutoIndent', True);
      FOptions.IndentationGuides := Ini.ReadBool('Options', 'IndentationGuides', True);
      FOptions.GutterLineNumbers := Ini.ReadBool('Options', 'GutterLineNumbers', False);
      FOptions.ShowPreprocessorOutput := Ini.ReadBool('Options', 'ShowPreprocessorOutput', True);
      FOptions.OpenIncludedFiles := Ini.ReadBool('Options', 'OpenIncludedFiles', True);
      I := Ini.ReadInteger('Options', 'KeyMappingType', Ord(GetDefaultKeyMappingType));
      if (I >= 0) and (I <= Ord(High(TKeyMappingType))) then
        FOptions.KeyMappingType := TKeyMappingType(I);
      I := Ini.ReadInteger('Options', 'MemoKeyMappingType', Ord(GetDefaultMemoKeyMappingType));
      if (I >= 0) and (I <= Ord(High(TIDEScintKeyMappingType))) then
        FOptions.MemoKeyMappingType := TIDEScintKeyMappingType(I);
      I := Ini.ReadInteger('Options', 'ThemeType', Ord(GetDefaultThemeType));
      if (I >= 0) and (I <= Ord(High(TThemeType))) then
        FOptions.ThemeType := TThemeType(I);
      FMainMemo.Font.Name := Ini.ReadString('Options', 'EditorFontName', FMainMemo.Font.Name);
      FMainMemo.Font.Size := Ini.ReadInteger('Options', 'EditorFontSize', 10);
      FMainMemo.Font.Charset := Ini.ReadInteger('Options', 'EditorFontCharset', FMainMemo.Font.Charset);
      FMainMemo.Zoom := Ini.ReadInteger('Options', 'Zoom', 0); { MemoZoom will zoom the other memos }
      for Memo in FMemos do
        if Memo <> FMainMemo then
          Memo.Font := FMainMemo.Font;

      { UpdatePanel visibility }
      CheckUpdatePanelMessage(Ini, 'KnownVersion', 0, Integer(FCompilerVersion.BinVersion),
        'Your version of Inno Setup has been updated! <a id="hwhatsnew">See what''s new</a>.',
        $ABE3AB); //MGreen with HSL lightness changed from 40% to 78%
      CheckUpdatePanelMessage(Ini, 'VSCodeMemoKeyMap', 0, 1,
        'VS Code-style editor shortcuts added! Use the <a id="toptions-vscode">Editor Keys option</a> in Options dialog.',
        $FFD399); //MBlue with HSL lightness changed from 42% to 80%
      UpdateUpdatePanel;

      { Debug options }
      FOptions.ShowCaretPosition := Ini.ReadBool('Options', 'ShowCaretPosition', False);
      if FOptions.ShowCaretPosition then
        StatusBar.Panels[spCaretPos].Width := StatusBar.Panels[spCaretPos].Width * 2;

      SyncEditorOptions;
      UpdateNewMainFileButtons;
      UpdateKeyMapping;
      UpdateTheme;
      UpdateFindRegExPanel;

      { Window state }
      WindowPlacement.length := SizeOf(WindowPlacement);
      GetWindowPlacement(Handle, @WindowPlacement);
      WindowPlacement.showCmd := SW_HIDE;  { the form isn't Visible yet }
      WindowPlacement.rcNormalPosition.Left := Ini.ReadInteger('State',
        'WindowLeft', WindowPlacement.rcNormalPosition.Left);
      WindowPlacement.rcNormalPosition.Top := Ini.ReadInteger('State',
        'WindowTop', WindowPlacement.rcNormalPosition.Top);
      WindowPlacement.rcNormalPosition.Right := Ini.ReadInteger('State',
        'WindowRight', WindowPlacement.rcNormalPosition.Left + Width);
      WindowPlacement.rcNormalPosition.Bottom := Ini.ReadInteger('State',
        'WindowBottom', WindowPlacement.rcNormalPosition.Top + Height);
      SetWindowPlacement(Handle, @WindowPlacement);
      { Note: Must set WindowState *after* calling SetWindowPlacement, since
        TCustomForm.WMSize resets WindowState }
      if Ini.ReadBool('State', 'WindowMaximized', False) then
        WindowState := wsMaximized;
      { Note: Don't call UpdateStatusPanelHeight here since it clips to the
        current form height, which hasn't been finalized yet }

      { StatusPanel height }
      StatusPanel.Height := ToCurrentPPI(Ini.ReadInteger('State', 'StatusPanelHeight',
        (10 * FromCurrentPPI(DebugOutputList.ItemHeight) + 4) + FromCurrentPPI(OutputTabSet.Height)));
    finally
      Ini.Free;
    end;
    FOptionsLoaded := True;
  end;

var
  I: Integer;
  NewItem: TMenuItem;
  PopupMenu: TPopupMenu;
  Memo: TIDEScintEdit;
begin
  inherited;

  {$IFNDEF STATICCOMPILER}
  FCompilerVersion := ISDllGetVersion;
  {$ELSE}
  FCompilerVersion := ISGetVersion;
  {$ENDIF}

  FModifiedAnySinceLastCompile := True;

  InitFormFont(Self);

  { For some reason, if AutoScroll=False is set on the form Delphi ignores the
    'poDefault' Position setting }
  AutoScroll := False;

  { Append the shortcut key text to the Edit items. Don't actually set the
    ShortCut property because we don't want the key combinations having an
    effect when Memo doesn't have the focus. }
  SetFakeShortCut(EUndo, Ord('Z'), [ssCtrl]);
  SetFakeShortCut(ERedo, Ord('Y'), [ssCtrl]);
  SetFakeShortCut(ECut, Ord('X'), [ssCtrl]);
  SetFakeShortCut(ECopy, Ord('C'), [ssCtrl]);
  SetFakeShortCut(EPaste, Ord('V'), [ssCtrl]);
  SetFakeShortCut(ESelectAll, Ord('A'), [ssCtrl]);
  SetFakeShortCut(EDelete, VK_DELETE, []);
  SetFakeShortCutText(VZoomIn, SmkcCtrl + 'Num +');    { These zoom shortcuts are handled by Scintilla and only support the active memo, unlike the menu items which work on all memos }
  SetFakeShortCutText(VZoomOut, SmkcCtrl + 'Num -');
  SetFakeShortCutText(VZoomReset, SmkcCtrl + 'Num /');
  { Use fake Esc shortcut for Stop Compile so it doesn't conflict with the
    editor's autocompletion list }
  SetFakeShortCut(BStopCompile, VK_ESCAPE, []);
  { Use fake Ctrl+F4 shortcut for VCloseCurrentTab2 because VCloseCurrentTab
    already has the real one }
  SetFakeShortCut(VCloseCurrentTab2, VK_F4, [ssCtrl]);
  { Use fake Ctrl+C and Ctrl+A shortcuts for OutputListPopupMenu's items so they
    don't conflict with the editor which also uses fake shortcuts for these }
  SetFakeShortCut(POutputListCopy, Ord('C'), [ssCtrl]);
  SetFakeShortCut(POutputListSelectAll, Ord('A'), [ssCtrl]);
  { Set real shortcut on TOptions which can't be set at design time }
  TOptions.ShortCut := ShortCut(VK_OEM_COMMA, [ssCtrl]);

  PopupMenu := TMainFormPopupMenu.Create(Self, EMenu);

  FMemosStyler := TInnoSetupStyler.Create(Self);
  FMemosStyler.ISPPInstalled := ISPPInstalled;

  FTheme := TTheme.Create;
  InitFormThemeInit(FTheme);

  ToolBarPanel.ParentBackground := False;
  UpdatePanel.ParentBackground := False;
  UpdatePanelDonateImage.Hint := RemoveAccelChar(HDonate.Caption);

  UpdateImages;

  FMemos := TList<TIDEScintEdit>.Create;
  FMainMemo := InitializeMainMemo(TIDEScintFileEdit.Create(Self), PopupMenu);
  FMemos.Add(FMainMemo);
  FPreprocessorOutputMemo := InitializeNonFileMemo(TIDEScintEdit.Create(Self), PopupMenu);
  FMemos.Add(FPreprocessorOutputMemo);
  for I := FMemos.Count to MaxMemos-1 do
    FMemos.Add(InitializeFileMemo(TIDEScintFileEdit.Create(Self), PopupMenu));
  FFileMemos := TList<TIDEScintFileEdit>.Create;
  for Memo in FMemos do
    if Memo is TIDEScintFileEdit then
      FFileMemos.Add(TIDEScintFileEdit(Memo));
  FHiddenFiles := TStringList.Create(dupError, True, True);
  FActiveMemo := FMainMemo;
  FActiveMemo.Visible := True;
  FErrorMemo := FMainMemo;
  FStepMemo := FMainMemo;
  UpdateMarginsAndSquigglyAndCaretWidths;

  FMemosStyler.Theme := FTheme;

  MemosTabSet.PopupMenu := TMainFormPopupMenu.Create(Self, MemosTabSetPopupMenu);
  FFirstTabSelectShortCut := ShortCut(Ord('1'), [ssCtrl]);
  FLastTabSelectShortCut := ShortCut(Ord('9'), [ssCtrl]);

  FNavStacks := TIDEScintEditNavStacks.Create;
  UpdateNavButtons;
  FCurrentNavItem.Invalidate;

  BackNavButton.Style := tbsDropDown;
  BackNavButton.DropdownMenu := TMainFormPopupMenu.Create(Self, NavPopupMenu);

  PopupMenu := TMainFormPopupMenu.Create(Self, OutputListPopupMenu);

  CompilerOutputList.PopupMenu := PopupMenu;
  DebugOutputList.PopupMenu := PopupMenu;
  DebugCallStackList.PopupMenu := PopupMenu;
  FindResultsList.PopupMenu := PopupMenu;

  UpdateOutputTabSetListsItemHeightAndDebugTimeWidth;

  Application.HintShortPause := 0;
  Application.OnException := AppOnException;
  Application.OnActivate := AppOnActivate;
  Application.OnIdle := AppOnIdle;

  FMRUMainFilesList := TStringList.Create;
  for I := 0 to High(FMRUMainFilesMenuItems) do begin
    NewItem := TMenuItem.Create(Self);
    NewItem.OnClick := FMRUClick;
    FRecent.Insert(I, NewItem);
    FMRUMainFilesMenuItems[I] := NewItem;
  end;
  FMRUParametersList := TStringList.Create;

  FSignTools := TStringList.Create;
  FFindResults := TFindResults.Create;

  FIncludedFiles := TIncludedFiles.Create;
  UpdatePreprocMemos;

  FDebugTarget := dtSetup;
  UpdateTargetMenu;

  UpdateCaption;

  FMenuDarkBackgroundBrush := TBrush.Create;
  FMenuDarkHotOrSelectedBrush := TBrush.Create;

  LightToolbarVirtualImageList.AutoFill := True;
  ThemedMarkersAndACVirtualImageList.AutoFill := True;

  UpdateThemeData(True);

  FMenuBitmaps := TMenuBitmaps.Create;
  FMenuBitmapsSize.cx := 0;
  FMenuBitmapsSize.cy := 0;

  FKeyMappedMenus := TKeyMappedMenus.Create;

  FCallTipState.MaxCallTips := 1; { Just like SciTE 5.50 }

  FUpdatePanelMessages := TUpdatePanelMessages.Create;

  if CommandLineCompile then begin
    ReadSignTools(FSignTools);
    PostMessage(Handle, WM_StartCommandLineCompile, 0, 0)
  end else if CommandLineWizard then begin
    { Stop Delphi from showing the compiler form }
    Application.ShowMainForm := False;
    { Show wizard form later }
    PostMessage(Handle, WM_StartCommandLineWizard, 0, 0);
  end else begin
    ReadConfig; { Calls UpdateTheme }
    ReadSignTools(FSignTools);
    PostMessage(Handle, WM_StartNormally, 0, 0);
  end;
end;

destructor TMainForm.Destroy;

  procedure SaveConfig;
  var
    Ini: TConfigIniFile;
    WindowPlacement: TWindowPlacement;
  begin
    Ini := TConfigIniFile.Create;
    try
      { Theme state - can change without opening the options }
      Ini.WriteInteger('Options', 'ThemeType', Ord(FOptions.ThemeType));  { Also see TOptionsClick }

      { Menu check boxes state }
      Ini.WriteBool('Options', 'ShowToolbar', ToolbarPanel.Visible);
      Ini.WriteBool('Options', 'ShowStatusBar', StatusBar.Visible);
      Ini.WriteBool('Options', 'LowPriorityDuringCompile', FOptions.LowPriorityDuringCompile);

      { Window state }
      WindowPlacement.length := SizeOf(WindowPlacement);
      GetWindowPlacement(Handle, @WindowPlacement);
      Ini.WriteInteger('State', 'WindowLeft', WindowPlacement.rcNormalPosition.Left);
      Ini.WriteInteger('State', 'WindowTop', WindowPlacement.rcNormalPosition.Top);
      Ini.WriteInteger('State', 'WindowRight', WindowPlacement.rcNormalPosition.Right);
      Ini.WriteInteger('State', 'WindowBottom', WindowPlacement.rcNormalPosition.Bottom);
      { The GetWindowPlacement docs claim that "flags" is always zero.
        Fortunately, that's wrong. WPF_RESTORETOMAXIMIZED is set when the
        window is either currently maximized, or currently minimized from a
        previous maximized state. }
      Ini.WriteBool('State', 'WindowMaximized', WindowPlacement.flags and WPF_RESTORETOMAXIMIZED <> 0);
      Ini.WriteInteger('State', 'StatusPanelHeight', FromCurrentPPI(StatusPanel.Height));

      { Zoom state }
      Ini.WriteInteger('Options', 'Zoom', FMainMemo.Zoom); { Only saves the main memo's zoom }
    finally
      Ini.Free;
    end;
  end;

begin
  UpdateThemeData(False);

  Application.OnActivate := nil;
  Application.OnIdle := nil;

  if FOptionsLoaded and not (CommandLineCompile or CommandLineWizard) then
    SaveConfig;

  if FDevMode <> 0 then
    GlobalFree(FDevMode);
  if FDevNames <> 0 then
    GlobalFree(FDevNames);

  FUpdatePanelMessages.Free;
  FNavStacks.Free;
  FKeyMappedMenus.Free;
  FMenuBitmaps.Free;
  FMenuDarkBackgroundBrush.Free;
  FMenuDarkHotOrSelectedBrush.Free;
  FTheme.Free;
  DestroyDebugInfo;
  FIncludedFiles.Free;
  FFindResults.Free;
  FSignTools.Free;
  FMRUParametersList.Free;
  FMRUMainFilesList.Free;
  FFileMemos.Free;
  FHiddenFiles.Free;
  FMemos.Free;

  inherited;
end;

function TMainForm.GetBorderStyle: TFormBorderStyle;
begin
  Result := inherited BorderStyle;
end;

procedure TMainForm.SetBorderStyle(Value: TFormBorderStyle);
begin
  { Hack: To stop the Delphi IDE from adding Explicit* properties to the .dfm
    file every time the unit is saved, we set BorderStyle=bsNone on the form.
    At run-time, ignore that setting so that BorderStyle stays at the default
    value, bsSizeable.
    It would be simpler to change BorderStyle from bsNone to bsSizeable in the
    form's constructor, but it doesn't quite work: when a form's handle is
    created while BorderStyle=bsNone, Position=poDefault behaves like
    poDefaultPosOnly (see TCustomForm.CreateParams). }

  if Value <> bsNone then
    inherited BorderStyle := Value;
end;

class procedure TMainForm.AppOnException(Sender: TObject; E: Exception);
begin
  AppMessageBox(PChar(AddPeriod(E.Message)), SCompilerFormCaption,
    MB_OK or MB_ICONSTOP);
end;

class procedure TMainForm.AppOnGetActiveFormHandle(var AHandle: HWND);
begin
  { As of Delphi 11.3, the default code in TApplication.GetActiveFormHandle
    (which runs after this handler) calls GetActiveWindow, and if that returns
    0, it calls GetLastActivePopup(Application.Handle).
    The problem is that when the application isn't in the foreground,
    GetActiveWindow returns 0, and when MainFormOnTaskBar=True, the
    GetLastActivePopup call normally just returns Application.Handle (since
    there are no popups owned by the application window).
    So if the application calls Application.MessageBox while it isn't in the
    foreground, that message box will be owned by Application.Handle, not by
    the last-active window as it should be. That can lead to the message box
    falling behind the main form in z-order.
    To rectify that, when no window is active and MainFormOnTaskBar=True, we
    fall back to returning the handle of the main form's last active popup,
    which is the window that would be activated if the main form's taskbar
    button were clicked. (If Application.Handle is active, we treat that the
    same as no active window because Application.Handle shouldn't be the owner
    of any windows when MainFormOnTaskBar=True.)
    If there is no assigned main form or if MainFormOnTaskBar=False, then we
    fall back to the default handling. }

  if Application.MainFormOnTaskBar then begin
    AHandle := GetActiveWindow;
    if ((AHandle = 0) or (AHandle = Application.Handle)) and
       Assigned(Application.MainForm) and
       Application.MainForm.HandleAllocated then
      AHandle := GetLastActivePopup(Application.MainFormHandle);
  end;
end;

procedure TMainForm.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  UpdateImages;
  UpdateMarginsAndAutoCompleteIcons;
  UpdateMarginsAndSquigglyAndCaretWidths;
  UpdateOutputTabSetListsItemHeightAndDebugTimeWidth;
  UpdateStatusPanelHeight(StatusPanel.Height);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if IsWindowEnabled(Handle) then
    CanClose := ConfirmCloseFile(True)
  else
    { CloseQuery is also called by the VCL when a WM_QUERYENDSESSION message
      is received. Don't display message box if a modal dialog is already
      displayed. }
    CanClose := False;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  var AShortCut := ShortCut(Key, Shift);
  if (AShortCut = VK_ESCAPE) and BStopCompile.Enabled then begin
    Key := 0; { Intentionally only done when BStopCompile is enabled to allow the memo to process it instead }
    BStopCompileClick(Self)
  end else if (AShortCut = FBackNavButtonShortCut) or
              ((FBackNavButtonShortCut2 <> 0) and (AShortCut = FBackNavButtonShortCut2)) then begin
    Key := 0;
    if BackNavButton.Enabled then
      BackNavButtonClick(Self);
  end else if (AShortCut = FForwardNavButtonShortCut) or
              ((FForwardNavButtonShortCut2 <> 0) and (AShortCut = FForwardNavButtonShortCut2)) then begin
    Key := 0;
    if ForwardNavButton.Enabled then
      ForwardNavButtonClick(Self);
  end else if (AShortCut >= FFirstTabSelectShortCut) and (AShortCut <= FLastTabSelectShortCut) then begin
    Key := 0;
    if MemosTabSet.Visible then begin
      var TabIndex := AShortCut - FFirstTabSelectShortCut;
      if TabIndex < 8 then begin
        if TabIndex < MemosTabSet.Tabs.Count then
          MemosTabSet.TabIndex := TabIndex;
      end else { Ctrl+9 = Select last tab }
        MemosTabSet.TabIndex := MemosTabSet.Tabs.Count-1;
    end;
  end else if AShortCut = FCompileShortCut2 then begin
    Key := 0;
    if BCompile.Enabled then
      BCompileClick(Self);
  end else if (Key = VK_F6) and not (ssAlt in Shift) then begin
    { Toggle focus between the active memo and the active bottom pane }
    Key := 0;
    if ActiveControl <> FActiveMemo then
      ActiveControl := FActiveMemo
    else if StatusPanel.Visible then begin
      case OutputTabSet.TabIndex of
        tiCompilerOutput: ActiveControl := CompilerOutputList;
        tiDebugOutput: ActiveControl := DebugOutputList;
        tiDebugCallStack: ActiveControl := DebugCallStackList;
        tiFindResults: ActiveControl := FindResultsList;
      end;
    end;
  end;
end;

procedure TMainForm.MemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  procedure SimplifySelection(const AMemo: TIDEScintEdit);
  begin
    { The built in Esc (SCI_CANCEL) simply drops all additional selections
      and does not empty the main selection, It doesn't matter if Esc is
      pressed once or twice. Implement our own behaviour, same as VSCode.
      Also see https://github.com/microsoft/vscode/issues/118835. }
    if AMemo.SelectionCount > 1 then
      AMemo.RemoveAdditionalSelections
    else if not AMemo.SelEmpty then
     AMemo.SetEmptySelection;
    AMemo.ScrollCaretIntoView;
  end;

  procedure AddCursor(const AMemo: TIDEScintEdit; const Up: Boolean);
  begin
    { Does not try to keep the main selection. }

    var Selections: TScintCaretAndAnchorList := nil;
    var VirtualSpaces: TScintCaretAndAnchorList := nil;
    try
      Selections := TScintCaretAndAnchorList.Create;
      VirtualSpaces := TScintCaretAndAnchorList.Create;
      { Get all the virtual spaces as well before we start doing modifications }
      AMemo.GetSelections(Selections, VirtualSpaces);
      for var I := 0 to Selections.Count-1 do begin
        var Selection := Selections[I];
        var LineCaret := AMemo.GetLineFromPosition(Selection.CaretPos);
        var LineAnchor := AMemo.GetLineFromPosition(Selection.AnchorPos);
        if LineCaret = LineAnchor then begin
          { Add selection with same caret and anchor offsets one line up or down. }
          var OtherLine := LineCaret + IfThen(Up, -1, 1);;
          if (OtherLine < 0) or (OtherLine >= AMemo.Lines.Count) then
            Continue { Already at the top or bottom, can't add }
          else begin
            var LineStartPos := AMemo.GetPositionFromLine(LineCaret);
            var CaretCharacterCount := AMemo.GetCharacterCount(LineStartPos, Selection.CaretPos) + VirtualSpaces[I].CaretPos;
            var AnchorCharacterCount := AMemo.GetCharacterCount(LineStartPos, Selection.AnchorPos) + VirtualSpaces[I].AnchorPos;
            var OtherLineStart := AMemo.GetPositionFromLine(OtherLine);
            var MaxCharacterCount := AMemo.GetCharacterCount(OtherLineStart, AMemo.GetLineEndPosition(OtherLine));
            var NewCaretCharacterCount := CaretCharacterCount;
            var NewCaretVirtualSpace := 0;
            var NewAnchorCharacterCount := AnchorCharacterCount;
            var NewAnchorVirtualSpace := 0;
            if NewCaretCharacterCount > MaxCharacterCount then begin
              NewCaretVirtualSpace := NewCaretCharacterCount - MaxCharacterCount;
              NewCaretCharacterCount := MaxCharacterCount;
            end;
            if NewAnchorCharacterCount > MaxCharacterCount then begin
              NewAnchorVirtualSpace := NewAnchorCharacterCount - MaxCharacterCount;
              NewAnchorCharacterCount := MaxCharacterCount;
            end;
            var NewSelection: TScintCaretAndAnchor;
            NewSelection.CaretPos := AMemo.GetPositionRelative(OtherLineStart, NewCaretCharacterCount);
            NewSelection.AnchorPos := AMemo.GetPositionRelative(OtherLineStart, NewAnchorCharacterCount);
            { AddSelection trims selections except for the main selection so
              we need to check that ourselves unfortunately. Not doing a check
              gives a problem when you AddCursor two times starting with an
              empty single selection. The result will be 4 cursors, with 2 of
              them in the same place. The check below fixes this but not
              other cases when there's only partial overlap and Scintilla still
              behaves weird. The check also doesn't handle virtual space which
              is why we ultimately don't set virtual space: it leads to duplicate
              selections. }
            var MainSelection := AMemo.Selection;
            if not NewSelection.Range.Within(AMemo.Selection) then begin
              AMemo.AddSelection(NewSelection.CaretPos, NewSelection.AnchorPos);
              { if svsUserAccessible in FActiveMemo.VirtualSpaceOptions then begin
                var MainSel := AMemo.MainSelection;
                AMemo.SelectionCaretVirtualSpace[MainSel] := NewCaretVirtualSpace;
                AMemo.SelectionAnchorVirtualSpace[MainSel] := NewAnchorVirtualSpace;
              end; }
            end;
          end;
        end else begin
          { Extend multiline selection up or down. This is not the same as
            LineExtendUp/Down because those can shrink instead of extend. }
          var CaretBeforeAnchor := Selection.CaretPos < Selection.AnchorPos;
          var Down := not Up;
          var LineStartOrEnd, StartOrEndPos, VirtualSpace: Integer;
          { Does it start (when going up) or end (when going down) at the caret or the anchor? }
          if (Up and CaretBeforeAnchor) or (Down and not CaretBeforeAnchor) then begin
            LineStartOrEnd := LineCaret;
            StartOrEndPos := Selection.CaretPos;
            VirtualSpace := VirtualSpaces[I].CaretPos;
          end else begin
            LineStartOrEnd := LineAnchor;
            StartOrEndPos := Selection.AnchorPos;
            VirtualSpace := VirtualSpaces[I].AnchorPos;
          end;
          var NewStartOrEndPos: Integer;
          var NewVirtualSpace := 0;
          { Go up or down one line or to the start or end of the document }
          if (Up and (LineStartOrEnd > 0)) or (Down and  (LineStartOrEnd < AMemo.Lines.Count-1))  then begin
            var CharacterCount := AMemo.GetCharacterCount(AMemo.GetPositionFromLine(LineStartOrEnd), StartOrEndPos) + VirtualSpace;
            var OtherLine := LineStartOrEnd + IfThen(Up, -1, 1);
            var OtherLineStart := AMemo.GetPositionFromLine(OtherLine);
            var MaxCharacterCount := AMemo.GetCharacterCount(OtherLineStart, AMemo.GetLineEndPosition(OtherLine));
            var NewCharacterCount := CharacterCount;
            if NewCharacterCount > MaxCharacterCount then begin
              NewVirtualSpace := NewCharacterCount - MaxCharacterCount;
              NewCharacterCount := MaxCharacterCount;
            end;
            NewStartOrEndPos := AMemo.GetPositionRelative(OtherLineStart, NewCharacterCount);
          end else
            NewStartOrEndPos := IfThen(Up, 0, AMemo.GetPositionFromLine(AMemo.Lines.Count));
          { Move the caret or the anchor up or down to extend the selection }
          if (Up and CaretBeforeAnchor) or (Down and not CaretBeforeAnchor) then begin
            AMemo.SelectionCaretPosition[I] := NewStartOrEndPos;
            if svsUserAccessible in FActiveMemo.VirtualSpaceOptions then
              AMemo.SelectionCaretVirtualSpace[I] := NewVirtualSpace;
          end else begin
            AMemo.SelectionAnchorPosition[I] := NewStartOrEndPos;
            if svsUserAccessible in FActiveMemo.VirtualSpaceOptions then
              AMemo.SelectionAnchorVirtualSpace[I] := NewVirtualSpace;
          end;
        end;
      end;
    finally
      VirtualSpaces.Free;
      Selections.Free;
    end;
  end;

  procedure AddCursorsToLineEnds(const AMemo: TIDEScintEdit);
  begin
    { Does not try to keep the main selection. Otherwise behaves the same as
      observed in Visual Studio Code, see comments. }

    var Selections: TScintCaretAndAnchorList := nil;
    var VirtualSpaces: TScintCaretAndAnchorList := nil;
    try
      Selections := TScintCaretAndAnchorList.Create;
      VirtualSpaces := TScintCaretAndAnchorList.Create;
      AMemo.GetSelections(Selections, VirtualSpaces);

      { First remove all empty selections }
      for var I := Selections.Count-1 downto 0 do begin
        var Selection := Selections[I];
        var VirtualSpace := VirtualSpaces[I];
        if (Selection.CaretPos + VirtualSpace.CaretPos) =
           (Selection.AnchorPos + VirtualSpace.AnchorPos) then begin
          Selections.Delete(I);
          VirtualSpaces.Delete(I);
        end;
      end;

      { If all selections were empty do nothing }
      if Selections.Count = 0 then
        Exit;

      { Handle non empty selections }
      for var I := Selections.Count-1 downto 0 do begin
        var Selection := Selections[I];
        var Line1 := AMemo.GetLineFromPosition(Selection.CaretPos);
        var Line2 := AMemo.GetLineFromPosition(Selection.AnchorPos);
        var SelSingleLine := Line1 = Line2;
        if SelSingleLine then begin
          { Single line selections are updated into empty selection at end of selection }
          var VirtualSpace := VirtualSpaces[I];
          if Selection.CaretPos + VirtualSpace.CaretPos > Selection.AnchorPos + VirtualSpace.AnchorPos then begin
            Selection.AnchorPos := Selection.CaretPos;
            VirtualSpace.AnchorPos := VirtualSpace.CaretPos;
          end else begin
            Selection.CaretPos := Selection.AnchorPos;
            VirtualSpace.CaretPos := VirtualSpace.AnchorPos;
          end;
          Selections[I] := Selection;
          VirtualSpaces[I] := VirtualSpace;
        end else begin
          { Multiline selections are replaced by empty selections at each end of line }
          if Line1 > Line2 then begin
            var TmpLine := Line1;
            Line1 := Line2;
            Line2 := TmpLine;
          end;
          { Ignore last line if the selection doesn't really select anything on that line }
          if Selection.Range.EndPos = AMemo.GetPositionFromLine(Line2) then
            Dec(Line2);
          for var Line := Line1 to Line2 do begin
            Selection.CaretPos := AMemo.GetLineEndPosition(Line);
            Selection.AnchorPos := Selection.CaretPos;
            Selections.Add(Selection);
            VirtualSpaces.Add(TScintCaretAndAnchor.Create(0, 0));
          end;
          Selections.Delete(I);
          VirtualSpaces.Delete(I);
        end;
      end;

      { Send updated selections to memo }
      for var I := 0 to Selections.Count-1 do begin
        var Selection := Selections[I];
        var VirtualSpace := VirtualSpaces[I];
        if I = 0 then
          AMemo.SetSingleSelection(Selection.CaretPos, Selection.AnchorPos)
        else
          AMemo.AddSelection(Selection.CaretPos, Selection.AnchorPos);
        AMemo.SelectionCaretVirtualSpace[I] := VirtualSpaces[I].CaretPos;
        AMemo.SelectionAnchorVirtualSpace[I] := VirtualSpaces[I].AnchorPos;
      end;
    finally
      VirtualSpaces.Free;
      Selections.Free;
    end;
  end;

begin
  if (Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END]) then begin
    var Memo := Sender as TIDEScintEdit;
     { Make sure we don't break the special rectangular select shortcuts }
    if Shift * [ssShift, ssAlt, ssCtrl] <> Memo.GetRectExtendShiftState(True) then begin
      if Memo.SelectionMode in [ssmRectangular, ssmThinRectangular] then begin
         { Allow left/right/etc. navigation with rectangular selection, see
           https://sourceforge.net/p/scintilla/feature-requests/1275/ and
           https://sourceforge.net/p/scintilla/bugs/2412/#cb37
           Notepad++ calls this "Enable Column Selection to Multi-editing" which
           is on by default and in VSCode and VS it's also on by default. }
        Memo.SelectionMode := ssmStream;
      end;
    end;
    { Key is not cleared to allow Scintilla to do the actual handling }
  end;

  if Key = VK_F1 then begin
    Key := 0;
    var HelpFile := GetHelpFile;
    if Assigned(HtmlHelp) then begin
      HtmlHelp(GetDesktopWindow, PChar(HelpFile), HH_DISPLAY_TOPIC, 0);
      var S := FActiveMemo.WordAtCaret;
      if S <> '' then begin
        var KLink: THH_AKLINK;
        FillChar(KLink, SizeOf(KLink), 0);
        KLink.cbStruct := SizeOf(KLink);
        KLink.pszKeywords := PChar(S);
        KLink.fIndexOnFail := True;
        HtmlHelp(GetDesktopWindow, PChar(HelpFile), HH_KEYWORD_LOOKUP, DWORD(@KLink));
      end;
    end;
  end else if ((Key = Ord('V')) or (Key = VK_INSERT)) and (Shift * [ssShift, ssAlt, ssCtrl] = [ssCtrl]) then begin
    if FActiveMemo.CanPaste then
      if MultipleSelectionPasteFromClipboard(FActiveMemo) then
        Key := 0;
  end else if (Key = VK_SPACE) and (Shift * [ssShift, ssAlt, ssCtrl] = [ssShift, ssCtrl]) then begin
    Key := 0;
    { Based on SciTE 5.50's SciTEBase::MenuCommand IDM_SHOWCALLTIP }
    if FActiveMemo.CallTipActive then begin
      FCallTipState.CurrentCallTip := IfThen(FCallTipState.CurrentCallTip + 1 = FCallTipState.MaxCallTips, 0, FCallTipState.CurrentCallTip + 1);
      UpdateCallTipFunctionDefinition;
    end else begin
      FCallTipState.BraceCount := 1; { Missing in SciTE, see https://sourceforge.net/p/scintilla/bugs/2446/ }
      InitiateCallTip(#0);
    end;
  end else begin
    var AShortCut := ShortCut(Key, Shift);
    { Check if the memo keymap wants us to handle the shortcut but first check
      the menu keymap didn't already claim the same shortcut. Other shortcuts
      (which are always same and not set by the menu keymap) are assumed to
      never conflict. }
    if not FKeyMappedMenus.ContainsKey(AShortCut) then begin
      var ComplexCommand := FActiveMemo.GetComplexCommand(AShortCut);
      if ComplexCommand <> ccNone then begin
        if Key <> VK_ESCAPE then { Allow Scintilla to see Esc }
          Key := 0;
        case ComplexCommand of
          ccSelectNextOccurrence:
            ESelectNextOccurrenceClick(Self);
          ccSelectAllOccurrences:
            ESelectAllOccurrencesClick(Self);
          ccSelectAllFindMatches:
            ESelectAllFindMatchesClick(Self);
          ccFoldLine:
            EFoldOrUnfoldLineClick(EFoldLine);
          ccUnfoldLine:
            EFoldOrUnfoldLineClick(EUnfoldLine);
          ccSimplifySelection:
            SimplifySelection(FActiveMemo);
          ccToggleLinesComment:
            EToggleLinesCommentClick(Self); //GetCompexCommand already checked ReadOnly for us
          ccAddCursorUp, ccAddCursorDown:
            AddCursor(FActiveMemo, ComplexCommand = ccAddCursorUp);
          ccBraceMatch:
            EBraceMatchClick(Self);
          ccAddCursorsToLineEnds:
            AddCursorsToLineEnds(FActiveMemo);
          else
            raise Exception.Create('Unknown ComplexCommand');
        end;
      end;
    end;
  end;
end;

procedure TMainForm.MemoKeyPress(Sender: TObject; var Key: Char);
begin
  if ((Key = #9) or (Key = ' ')) and (GetKeyState(VK_CONTROL) < 0) then begin
    { About #9, as Wikipedia explains: "The most known and common tab is a
      horizontal tabulation <..> and may be referred to as Ctrl+I." Ctrl+I is
      (just like in Visual Studio Code) our alternative code completion character
      because Ctrl+Space is used by the Chinese IME and Alt+Right is used for the
      forward button. So that's why we handle #9 here. Doesn't mean Ctrl+Tab
      doesn't work: it doesnt trigger KeyPress, even if it wasn't a menu
      shortcut for Next Tab (which it is). }
    InitiateAutoComplete(#0);
    Key := #0;
  end else if (Key <= #31) or (Key = #127) then begin
    { Prevent "control characters" from being entered in text. Don't need to be
      concerned about #9 or #10 or #13 etc here. Based on Notepad++'s WM_CHAR
      handling in ScintillaEditView.cpp.
      Also don't need to be concerned about shortcuts like Ctrl+Shift+- which
      equals #31. }
    Key := #0
  end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  { Make sure the status panel's height is decreased if necessary in response
    to the form's height decreasing }
  if StatusPanel.Visible then
    UpdateStatusPanelHeight(StatusPanel.Height);
end;

procedure TMainForm.WndProc(var Message: TMessage);
begin
  { Without this, the status bar's owner drawn panels sometimes get corrupted and show
    menu items instead. See:
    http://groups.google.com/group/borland.public.delphi.vcl.components.using/browse_thread/thread/e4cb6c3444c70714 }
  with Message do
    case Msg of
      WM_DRAWITEM:
        with PDrawItemStruct(Message.LParam)^ do
          if (CtlType = ODT_MENU) and not IsMenu(hwndItem) then
            CtlType := ODT_STATIC;
    end;
  inherited
end;

function TMainForm.IsShortCut(var Message: TWMKey): Boolean;
begin
  { Key messages are forwarded by the VCL to the main form for ShortCut
    processing. In Delphi 5+, however, this happens even when a TFindDialog
    is active, causing Ctrl+V/Esc/etc. to be intercepted by the main form.
    Work around this by always returning False when not Active. }
  if Active then
    Result := inherited IsShortCut(Message)
  else
    Result := False;
end;

procedure TMainForm.UpdateCaption;
var
  NewCaption: String;
begin
  if FMainMemo.Filename = '' then
    NewCaption := GetFileTitle(FMainMemo.Filename)
  else begin
    if FOptions.FullPathInTitleBar then
      NewCaption := FMainMemo.Filename
    else
      NewCaption := GetDisplayFilename(FMainMemo.Filename);
  end;
  NewCaption := NewCaption + ' - ' + SCompilerFormCaption + ' ' +
    String(FCompilerVersion.Version);
  if FCompiling then
    NewCaption := NewCaption + '  [Compiling]'
  else if FDebugging then begin
    if not FPaused then
      NewCaption := NewCaption + '  [Running]'
    else
      NewCaption := NewCaption + '  [Paused]';
  end;
  Caption := NewCaption;
  if not CommandLineWizard then
    Application.Title := NewCaption;
end;

procedure TMainForm.UpdateNewMainFileButtons;
begin
  if FOptions.UseWizard then begin
    FNewMainFile.Caption := '&New...';
    FNewMainFile.OnClick := FNewMainFileUserWizardClick;
    NewMainFileButton.OnClick := FNewMainFileUserWizardClick;
  end else begin
    FNewMainFile.Caption := '&New';
    FNewMainFile.OnClick := FNewMainFileClick;
    NewMainFileButton.OnClick := FNewMainFileClick;
  end;
end;

procedure TMainForm.NewMainFile;
var
  Memo: TIDEScintFileEdit;
begin
  HideError;
  FUninstExe := '';
  if FDebugTarget <> dtSetup then begin
    FDebugTarget := dtSetup;
    UpdateTargetMenu;
  end;
  FHiddenFiles.Clear;
  InvalidateStatusPanel(spHiddenFilesCount);
  for Memo in FFileMemos do
    if Memo.Used then
      Memo.BreakPoints.Clear;
  DestroyDebugInfo;

  FMainMemo.Filename := '';
  UpdateCaption;
  FMainMemo.SaveEncoding := seUTF8WithoutBOM;
  FMainMemo.Lines.Clear;
  FModifiedAnySinceLastCompile := True;
  FPreprocessorOutput := '';
  FIncludedFiles.Clear;
  UpdatePreprocMemos;
  FMainMemo.ClearUndo;

  FNavStacks.Clear;
  UpdateNavButtons;
  FCurrentNavItem.Invalidate;
end;

{ Breakpoints are preserved on a per-file basis }
procedure TMainForm.LoadBreakPointLinesAndUpdateLineMarkers(const AMemo: TIDEScintFileEdit);
begin
  if AMemo.BreakPoints.Count <> 0 then
    raise Exception.Create('AMemo.BreakPoints.Count <> 0'); { NewMainFile or OpenFile should have cleared these }

  try
    var HadSkippedBreakPoint := False;
    var Strings := TStringList.Create;
    try
      LoadBreakPointLines(AMemo.FileName, Strings);
      for var LineAsString in Strings do begin
        var Line := LineAsString.ToInteger;
        if Line < AMemo.Lines.Count then
          AMemo.BreakPoints.Add(Line)
        else
          HadSkippedBreakPoint := True;
      end;
    finally
      Strings.Free;
    end;
    for var Line in AMemo.BreakPoints do
      UpdateLineMarkers(AMemo, Line);

    { If there were breakpoints beyond the end of file get rid of them so they
      don't magically reappear on a reload of an externally edited and grown
      file }
    if HadSkippedBreakPoint then
      BuildAndSaveBreakPointLines(AMemo);
  except
    { Ignore any exceptions }
  end;
end;

procedure TMainForm.BuildAndSaveBreakPointLines(const AMemo: TIDEScintFileEdit);
begin
  try
    if AMemo.FileName <> '' then begin
      var Strings := TStringList.Create;
      try
        for var Line in AMemo.BreakPoints do
          Strings.Add(Line.ToString);
        SaveBreakPointLines(AMemo.FileName, Strings);
      finally
        Strings.Free;
      end;
    end;
  except
    { Handle exceptions locally; failure to save the breakpoint lines list should not be
      a fatal error }
    Application.HandleException(Self);
  end;
end;

{ Known included and hidden files are preserved on a per-main-file basis }
procedure TMainForm.LoadKnownIncludedAndHiddenFilesAndUpdateMemos;
begin
  if FIncludedFiles.Count <> 0 then
    raise Exception.Create('FIncludedFiles.Count <> 0'); { NewMainFile should have cleared these }

  try
    if AFilename <> '' then begin
      var Strings := TStringList.Create;
      try
        LoadKnownIncludedAndHiddenFiles(FMainMemo.FileName, Strings, FHiddenFiles);
        if Strings.Count > 0 then begin
          try
            for var Filename in Strings do begin
              var IncludedFile := TIncludedFile.Create;
              IncludedFile.Filename := Filename;
              IncludedFile.CompilerFileIndex := UnknownCompilerFileIndex;
              IncludedFile.HasLastWriteTime := GetLastWriteTimeOfFile(IncludedFile.Filename,
                @IncludedFile.LastWriteTime);
              FIncludedFiles.Add(IncludedFile);
            end;
          finally
            UpdatePreprocMemos;
          end;
        end;
      finally
        Strings.Free;
      end;
    end;
  except
    { Ignore any exceptions }
  end;
end;

procedure TMainForm.BuildAndSaveKnownIncludedAndHiddenFiles;
begin
  try
    if FMainMemo.FileName <> '' then begin
      var Strings := TStringList.Create;
      try
        for var IncludedFile in FIncludedFiles do
          Strings.Add(IncludedFile.Filename);
        SaveKnownIncludedAndHiddenFiles(FMainMemo.FileName, Strings, FHiddenFiles);
      finally
        Strings.Free;
      end;
    end;
  except
    { Handle exceptions locally; failure to save the includes list should not be
      a fatal error }
    Application.HandleException(Self);
  end;
end;

procedure TMainForm.NewMainFileUsingWizard;
var
  WizardForm: TWizardForm;
  SaveEnabled: Boolean;
begin
  WizardForm := TWizardForm.Create(Application);
  try
    SaveEnabled := Enabled;
    if CommandLineWizard then begin
      WizardForm.WizardName := CommandLineWizardName;
      { Must disable MainForm even though it isn't shown, otherwise
        menu keyboard shortcuts (such as Ctrl+O) still work }
      Enabled := False;
    end;
    try
      if WizardForm.ShowModal <> mrOk then
        Exit;
    finally
      Enabled := SaveEnabled;
    end;

    if CommandLineWizard then begin
      SaveTextToFile(CommandLineFileName, WizardForm.ResultScript, seUTF8WithoutBOM);
    end else begin
      NewMainFile;
      FMainMemo.Lines.Text := WizardForm.ResultScript;
      FMainMemo.ClearUndo;
      if WizardForm.Result = wrComplete then begin
        FMainMemo.ForceModifiedState;
        if MsgBox('Would you like to compile the new script now?', SCompilerFormCaption, mbConfirmation, MB_YESNO) = IDYES then
          BCompileClick(Self);
      end;
    end;
  finally
    WizardForm.Free;
  end;
end;

procedure TMainForm.OpenFile(AMemo: TIDEScintFileEdit; AFilename: String;
  const MainMemoAddToRecentDocs: Boolean);

  function GetStreamSaveEncoding(const Stream: TStream): TSaveEncoding;
  var
    Buf: array[0..2] of Byte;
  begin
    Result := seAuto;
    var StreamSize := Stream.Size;
    var CappedSize: Integer;
    if StreamSize > High(Integer) then
      CappedSize := High(Integer)
    else
      CappedSize := Integer(StreamSize);
    if (CappedSize >= SizeOf(Buf)) and (Stream.Read(Buf, SizeOf(Buf)) = SizeOf(Buf)) and
       (Buf[0] = $EF) and (Buf[1] = $BB) and (Buf[2] = $BF) then
      Result := seUTF8WithBOM
    else begin
      Stream.Seek(0, soFromBeginning);
      var S: AnsiString;
      SetLength(S, CappedSize);
      SetLength(S, Stream.Read(S[1], CappedSize));
      if DetectUTF8Encoding(S) in [etUSASCII, etUTF8] then
        Result := seUTF8WithoutBOM;
    end;
  end;

  function GetEncoding(const SaveEncoding: TSaveEncoding): TEncoding;
  begin
    if SaveEncoding in [seUTF8WithBOM, seUTF8WithoutBOM] then
      Result := TEncoding.UTF8
    else
      Result := nil;
  end;

var
  Stream: TFileStream;
begin
  AMemo.OpeningFile := True;
  try
    AFilename := PathExpand(AFilename);
    var NameChange := PathCompare(AMemo.Filename, AFilename) <> 0;

    Stream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
    try
      if AMemo = FMainMemo then
        NewMainFile
      else begin
        AMemo.BreakPoints.Clear;
        if DestroyLineState(AMemo) then
          UpdateAllMemoLineMarkers(AMemo);
        if NameChange then  { Also see below the other case which needs to be done after load }
          RemoveMemoFromNav(AMemo);
      end;
      GetFileTime(Stream.Handle, nil, nil, @AMemo.FileLastWriteTime);
      AMemo.SaveEncoding := GetStreamSaveEncoding(Stream);
      Stream.Seek(0, soFromBeginning);
      AMemo.Lines.LoadFromStream(Stream, GetEncoding(AMemo.SaveEncoding));
      if (AMemo <> FMainMemo) and not NameChange then
        RemoveMemoBadLinesFromNav(AMemo);
    finally
      Stream.Free;
    end;
    AMemo.ClearUndo;
    if AMemo = FMainMemo then begin
      AMemo.Filename := AFilename;
      UpdateCaption;
      ModifyMRUMainFilesList(AFilename, True);
      if MainMemoAddToRecentDocs then
        AddFileToRecentDocs(AFilename);
      LoadKnownIncludedAndHiddenFilesAndUpdateMemos(AFilename);
      InvalidateStatusPanel(spHiddenFilesCount);
    end;
    LoadBreakPointLinesAndUpdateLineMarkers(AMemo);
  finally
    AMemo.OpeningFile := False;
  end;
end;

procedure TMainForm.OpenMRUMainFile(const AFilename: String);
{ Same as OpenFile, but offers to remove the file from the MRU list if it
  cannot be opened }
begin
  try
    OpenFile(FMainMemo, AFilename, True);
  except
    Application.HandleException(Self);
    if MsgBoxFmt('There was an error opening the file. Remove it from the list?',
       [AFilename], SCompilerFormCaption, mbError, MB_YESNO) = IDYES then begin
      ModifyMRUMainFilesList(AFilename, False);
      DeleteBreakPointLines(AFilename);
      DeleteKnownIncludedAndHiddenFiles(AFilename);
    end;
  end;
end;

function TMainForm.SaveFile(const AMemo: TIDEScintFileEdit; const SaveAs: Boolean): Boolean;

  procedure SaveMemoTo(const FN: String);
  var
    TempFN, BackupFN: String;
    Buf: array[0..4095] of Char;
  begin
    { Save to a temporary file; don't overwrite existing files in place. This
      way, if the system crashes or the disk runs out of space during the save,
      the existing file will still be intact. }
    if GetTempFileName(PChar(PathExtractDir(FN)), 'iss', 0, Buf) = 0 then
      raise Exception.CreateFmt('Error creating file (code %d). Could not save file',
        [GetLastError]);
    TempFN := Buf;
    try
      SaveTextToFile(TempFN, AMemo.Lines.Text, AMemo.SaveEncoding);

      { Back up existing file if needed }
      if FOptions.MakeBackups and NewFileExists(FN) then begin
        BackupFN := PathChangeExt(FN, '.~is');
        DeleteFile(BackupFN);
        if not RenameFile(FN, BackupFN) then
          raise Exception.Create('Error creating backup file. Could not save file');
      end;

      { Delete existing file }
      if not DeleteFile(FN) and (GetLastError <> ERROR_FILE_NOT_FOUND) then
        raise Exception.CreateFmt('Error removing existing file (code %d). Could not save file',
          [GetLastError]);
    except
      DeleteFile(TempFN);
      raise;
    end;
    { Rename temporary file.
      Note: This is outside the try..except because we already deleted the
      existing file, and don't want the temp file also deleted in the unlikely
      event that the rename fails. }
    if not RenameFile(TempFN, FN) then
      raise Exception.CreateFmt('Error renaming temporary file (code %d). Could not save file',
        [GetLastError]);
    GetLastWriteTimeOfFile(FN, @AMemo.FileLastWriteTime);
  end;

var
  FN: String;
begin
  Result := False;
  var OldName := AMemo.Filename;
  if SaveAs or (AMemo.Filename = '') then begin
    if AMemo <> FMainMemo then
      raise Exception.Create('Internal error: AMemo <> FMainMemo');
    FN := AMemo.Filename;
    if not NewGetSaveFileName('', FN, '', SCompilerOpenFilter, 'iss', Handle) then Exit;
    FN := PathExpand(FN);
    SaveMemoTo(FN);
    AMemo.Filename := FN;
    UpdateCaption;
  end else
    SaveMemoTo(AMemo.Filename);
  AMemo.SetSavePoint;
  if not FOptions.UndoAfterSave then
    AMemo.ClearUndo(False);
  Result := True;
  if AMemo = FMainMemo then begin
    ModifyMRUMainFilesList(AMemo.Filename, True);
    if PathCompare(AMemo.Filename, OldName) <> 0 then begin
      if OldName <> '' then begin
        DeleteBreakPointLines(OldName);
        DeleteKnownIncludedAndHiddenFiles(OldName);
      end;
      BuildAndSaveBreakPointLines(AMemo);
      BuildAndSaveKnownIncludedAndHiddenFiles;
    end;
  end;
end;

function TMainForm.ConfirmCloseFile(const PromptToSave: Boolean): Boolean;

  function PromptToSaveMemo(const AMemo: TIDEScintFileEdit): Boolean;
  var
    FileTitle: String;
  begin
    Result := True;
    if AMemo.Modified then begin
      FileTitle := GetFileTitle(AMemo.Filename);
      case MsgBox('The text in the ' + FileTitle + ' file has changed.'#13#10#13#10 +
         'Do you want to save the changes?', SCompilerFormCaption, mbError,
         MB_YESNOCANCEL) of
        IDYES: Result := SaveFile(AMemo, False);
        IDNO: ;
      else
        Result := False;
      end;
    end;
  end;

var
  Memo: TIDEScintFileEdit;
begin
  if FCompiling then begin
    MsgBox('Please stop the compile process before performing this command.',
      SCompilerFormCaption, mbError, MB_OK);
    Result := False;
    Exit;
  end;
  if FDebugging and not AskToDetachDebugger then begin
    Result := False;
    Exit;
  end;
  Result := True;
  if PromptToSave then begin
    for Memo in FFileMemos do begin
      if Memo.Used then begin
        Result := PromptToSaveMemo(Memo);
        if not Result then
          Exit;
      end;
    end;
  end;
end;

procedure TMainForm.ClearMRUMainFilesList;
begin
  try
    ClearMRUList(FMRUMainFilesList, 'ScriptFileHistoryNew');
  except
    { Ignore any exceptions. }
  end;
end;

procedure TMainForm.ReadMRUMainFilesList;
begin
  try
    ReadMRUList(FMRUMainFilesList, 'ScriptFileHistoryNew', 'History');
  except
    { Ignore any exceptions. }
  end;
end;

procedure TMainForm.ModifyMRUMainFilesList(const AFilename: String;
  const AddNewItem: Boolean);
begin
  { Load most recent items first, just in case they've changed }
  try
    ReadMRUMainFilesList;
  except
    { Ignore any exceptions. }
  end;
  try
    ModifyMRUList(FMRUMainFilesList, 'ScriptFileHistoryNew', 'History', AFileName, AddNewItem, @PathCompare);
  except
    { Handle exceptions locally; failure to save the MRU list should not be
      a fatal error. }
    Application.HandleException(Self);
  end;
end;

procedure TMainForm.ReadMRUParametersList;
begin
  try
    ReadMRUList(FMRUParametersList, 'ParametersHistory', 'History');
  except
    { Ignore any exceptions. }
  end;
end;

procedure TMainForm.ModifyMRUParametersList(const AParameter: String;
  const AddNewItem: Boolean);
begin
  { Load most recent items first, just in case they've changed }
  try
    ReadMRUParametersList;
  except
    { Ignore any exceptions. }
  end;
  try
    ModifyMRUList(FMRUParametersList, 'ParametersHistory', 'History', AParameter, AddNewItem, @CompareText);
  except
    { Handle exceptions locally; failure to save the MRU list should not be
      a fatal error. }
    Application.HandleException(Self);
  end;
end;

procedure TMainForm.StatusMessage(const Kind: TStatusMessageKind; const S: String);
begin
  AddLines(CompilerOutputList, S, TObject(Kind), False, alpNone, 0);
  CompilerOutputList.Update;
end;

procedure TMainForm.DebugLogMessage(const S: String);
begin
  AddLines(DebugOutputList, S, nil, True, alpTimestamp, FDebugLogListTimestampsWidth);
  DebugOutputList.Update;
end;

procedure TMainForm.DebugShowCallStack(const CallStack: String; const CallStackCount: Cardinal);
begin
  DebugCallStackList.Clear;
  AddLines(DebugCallStackList, CallStack, nil, True, alpCountdown, FCallStackCount-1);
  DebugCallStackList.Items.Insert(0, '*** [Code] Call Stack');
  DebugCallStackList.Update;
end;

type
  PAppData = ^TAppData;
  TAppData = record
    Form: TMainForm;
    Filename: String;
    Lines: TStringList;
    CurLineNumber: Integer;
    CurLine: String;
    OutputExe: String;
    DebugInfo: Pointer;
    ErrorMsg: String;
    ErrorFilename: String;
    ErrorLine: Integer;
    Aborted: Boolean;
  end;

function CompilerCallbackProc(Code: Integer; var Data: TCompilerCallbackData;
  AppData: Longint): Integer; stdcall;

  procedure DecodeIncludedFilenames(P: PChar; const IncludedFiles: TIncludedFiles);
  var
    IncludedFile: TIncludedFile;
    I: Integer;
  begin
    IncludedFiles.Clear;
    if P = nil then
      Exit;
    I := 0;
    while P^ <> #0 do begin
      if not IsISPPBuiltins(P) then begin
        IncludedFile := TIncludedFile.Create;
        IncludedFile.Filename := GetCleanFileNameOfFile(P);
        IncludedFile.CompilerFileIndex := I;
        IncludedFile.HasLastWriteTime := GetLastWriteTimeOfFile(IncludedFile.Filename,
          @IncludedFile.LastWriteTime);
        IncludedFiles.Add(IncludedFile);
      end;
      Inc(P, StrLen(P) + 1);
      Inc(I);
    end;
  end;

  procedure CleanHiddenFiles(const IncludedFiles: TIncludedFiles; const HiddenFiles: TStringList);
  var
    HiddenFileIncluded: array of Boolean;
  begin
    if HiddenFiles.Count > 0 then begin
      { Clean previously hidden files which are no longer included }
      if IncludedFiles.Count > 0 then begin
        SetLength(HiddenFileIncluded, HiddenFiles.Count);
        for var I := 0 to HiddenFiles.Count-1 do
          HiddenFileIncluded[I] := False;
        for var I := 0 to IncludedFiles.Count-1 do begin
          var IncludedFile := IncludedFiles[I];
          var HiddenFileIndex := HiddenFiles.IndexOf(IncludedFile.Filename);
          if HiddenFileIndex <> -1 then
            HiddenFileIncluded[HiddenFileIndex] := True;
        end;
        for var I := HiddenFiles.Count-1 downto 0 do
          if not HiddenFileIncluded[I] then
            HiddenFiles.Delete(I);
      end else
        HiddenFiles.Clear;
    end;
  end;

begin
  Result := iscrSuccess;
  with PAppData(AppData)^ do
    case Code of
      iscbReadScript:
        begin
          if Data.Reset then
            CurLineNumber := 0;
          if CurLineNumber < Lines.Count then begin
            CurLine := Lines[CurLineNumber];
            Data.LineRead := PChar(CurLine);
            Inc(CurLineNumber);
          end;
        end;
      iscbNotifyStatus:
        if Data.Warning then
          Form.StatusMessage(smkWarning, Data.StatusMsg)
        else
          Form.StatusMessage(smkNormal, Data.StatusMsg);
      iscbNotifyIdle:
        begin
          Form.UpdateCompileStatusPanels(Data.CompressProgress,
            Data.CompressProgressMax, Data.SecondsRemaining,
            Data.BytesCompressedPerSecond);
          { We have to use HandleMessage instead of ProcessMessages so that
            Application.Idle is called. Otherwise, Flat TSpeedButton's don't
            react to the mouse being moved over them.
            Unfortunately, HandleMessage by default calls WaitMessage. To avoid
            this we have an Application.OnIdle handler which sets Done to False
            while compiling is in progress - see AppOnIdle.
            The GetQueueStatus check below is just an optimization; calling
            HandleMessage when there are no messages to process wastes CPU. }
          if GetQueueStatus(QS_ALLINPUT) <> 0 then begin
            Form.FBecameIdle := False;
            repeat
              Application.HandleMessage;
              { AppOnIdle sets FBecameIdle to True when it's called, which
                indicates HandleMessage didn't find any message to process }
            until Form.FBecameIdle;
          end;
          if Form.FCompileWantAbort then
            Result := iscrRequestAbort;
        end;
      iscbNotifyPreproc:
        begin
          Form.FPreprocessorOutput := TrimRight(Data.PreprocessedScript);
          DecodeIncludedFilenames(Data.IncludedFilenames, Form.FIncludedFiles); { Also stores last write time }
          CleanHiddenFiles(Form.FIncludedFiles, Form.FHiddenFiles);
          Form.InvalidateStatusPanel(spHiddenFilesCount);
          Form.BuildAndSaveKnownIncludedAndHiddenFiles;
        end;
      iscbNotifySuccess:
        begin
          OutputExe := Data.OutputExeFilename;
          if Form.FCompilerVersion.BinVersion >= $3000001 then begin
            DebugInfo := AllocMem(Data.DebugInfoSize);
            Move(Data.DebugInfo^, DebugInfo^, Data.DebugInfoSize);
          end else
            DebugInfo := nil;
        end;
      iscbNotifyError:
        begin
          if Assigned(Data.ErrorMsg) then
            ErrorMsg := Data.ErrorMsg
          else
            Aborted := True;
          ErrorFilename := Data.ErrorFilename;
          ErrorLine := Data.ErrorLine;
        end;
    end;
end;

procedure TMainForm.CompileFile(AFilename: String; const ReadFromFile: Boolean);

  function GetMemoFromErrorFilename(const ErrorFilename: String): TIDEScintFileEdit;
  var
    Memo: TIDEScintFileEdit;
  begin
    if ErrorFilename = '' then
      Result := FMainMemo
    else begin
      if FOptions.OpenIncludedFiles then begin
        for Memo in FFileMemos do begin
          if Memo.Used and (PathCompare(Memo.Filename, ErrorFilename) = 0) then begin
            Result := Memo;
            Exit;
          end;
        end;
      end;
      Result := nil;
    end;
  end;

var
  SourcePath, S, Options: String;
  Params: TCompileScriptParamsEx;
  AppData: TAppData;
  StartTime, ElapsedTime, ElapsedSeconds: DWORD;
  I: Integer;
  Memo: TIDEScintFileEdit;
  OldActiveMemo: TIDEScintEdit;
begin
  if FCompiling then begin
    { Shouldn't get here, but just in case... }
    MsgBox('A compile is already in progress.', SCompilerFormCaption, mbError, MB_OK);
    Abort;
  end;

  if not ReadFromFile then begin
    if FOptions.OpenIncludedFiles then begin
      { Included files must always be saved since they're not read from the editor by the compiler }
      for Memo in FFileMemos do begin
        if (Memo <> FMainMemo) and Memo.Used and Memo.Modified then begin
          if FOptions.Autosave then begin
            if not SaveFile(Memo, False) then
              Abort;
          end else begin
            case MsgBox('The text in the ' + Memo.Filename + ' file has changed and must be saved before compiling.'#13#10#13#10 +
               'Save the changes and continue?', SCompilerFormCaption, mbError,
               MB_YESNO) of
              IDYES:
                if not SaveFile(Memo, False) then
                  Abort;
            else
              Abort;
            end;
          end;
        end;
      end;
    end;
    { Save main file if requested }
    if FOptions.Autosave and FMainMemo.Modified then begin
      if not SaveFile(FMainMemo, False) then
        Abort;
    end else if FMainMemo.Filename = '' then begin
      case MsgBox('Would you like to save the script before compiling?' +
         SNewLine2 + 'If you answer No, the compiled installation will be ' +
         'placed under your My Documents folder by default.',
         SCompilerFormCaption, mbConfirmation, MB_YESNOCANCEL) of
        IDYES:
          if not SaveFile(FMainMemo, False) then
            Abort;
        IDNO: ;
      else
        Abort;
      end;
    end;
    AFilename := FMainMemo.Filename;
  end; {else: Command line compile, AFilename already set. }

  DestroyDebugInfo;
  OldActiveMemo := FActiveMemo;
  AppData.Lines := TStringList.Create;
  try
    FBuildAnimationFrame := 0;
    FProgress := 0;
    FProgressMax := 0;
    FTaskbarProgressValue := 0;

    FActiveMemo.CancelAutoCompleteAndCallTip;
    FActiveMemo.Cursor := crAppStart;
    FActiveMemo.SetCursorID(999);  { hack to keep it from overriding Cursor }
    CompilerOutputList.Cursor := crAppStart;
    for Memo in FFileMemos do
      Memo.ReadOnly := True;
    UpdateEditModePanel;
    HideError;
    CompilerOutputList.Clear;
    SendMessage(CompilerOutputList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
    DebugOutputList.Clear;
    SendMessage(DebugOutputList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
    DebugCallStackList.Clear;
    SendMessage(DebugCallStackList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
    OutputTabSet.TabIndex := tiCompilerOutput;
    SetStatusPanelVisible(True);

    SourcePath := GetSourcePath(AFilename);

    FillChar(Params, SizeOf(Params), 0);
    Params.Size := SizeOf(Params);
    Params.CompilerPath := nil;
    Params.SourcePath := PChar(SourcePath);
    Params.CallbackProc := CompilerCallbackProc;
    Pointer(Params.AppData) := @AppData;
    Options := '';
    for I := 0 to FSignTools.Count-1 do
      Options := Options + AddSignToolParam(FSignTools[I]);
    Params.Options := PChar(Options);

    AppData.Form := Self;
    AppData.CurLineNumber := 0;
    AppData.Aborted := False;
    I := ReadScriptLines(AppData.Lines, ReadFromFile, AFilename, FMainMemo);
    if I <> -1 then begin
      if not ReadFromFile then begin
        MoveCaretAndActivateMemo(FMainMemo, I, False);
        SetErrorLine(FMainMemo, I);
      end;
      raise Exception.CreateFmt(SCompilerIllegalNullChar, [I + 1]);
    end;

    StartTime := GetTickCount;
    StatusMessage(smkStartEnd, Format(SCompilerStatusStarting, [TimeToStr(Time)]));
    StatusMessage(smkStartEnd, '');
    FCompiling := True;
    FCompileWantAbort := False;
    UpdateRunMenu;
    UpdateCaption;
    SetLowPriority(FOptions.LowPriorityDuringCompile, FSavePriorityClass);

    AppData.Filename := AFilename;

    {$IFNDEF STATICCOMPILER}
    if ISDllCompileScript(Params) <> isceNoError then begin
    {$ELSE}
    if ISCompileScript(Params, False) <> isceNoError then begin
    {$ENDIF}
      StatusMessage(smkError, SCompilerStatusErrorAborted);
      if not ReadFromFile and (AppData.ErrorLine > 0) then begin
        Memo := GetMemoFromErrorFilename(AppData.ErrorFilename);
        if Memo <> nil then begin
          { Move the caret to the line number the error occurred on }
          MoveCaretAndActivateMemo(Memo, AppData.ErrorLine - 1, False);
          SetErrorLine(Memo, AppData.ErrorLine - 1);
        end;
      end;
      if not AppData.Aborted then begin
        S := '';
        if AppData.ErrorFilename <> '' then
          S := 'File: ' + AppData.ErrorFilename + SNewLine2;
        if AppData.ErrorLine > 0 then
          S := S + Format('Line %d:' + SNewLine, [AppData.ErrorLine]);
        S := S + AppData.ErrorMsg;
        SetAppTaskbarProgressState(tpsError);
        MsgBox(S, 'Compiler Error', mbCriticalError, MB_OK)
      end;
      Abort;
    end;
    ElapsedTime := GetTickCount - StartTime;
    ElapsedSeconds := ElapsedTime div 1000;
    StatusMessage(smkStartEnd, Format(SCompilerStatusFinished, [TimeToStr(Time),
      Format('%.2u%s%.2u%s%.3u', [ElapsedSeconds div 60, FormatSettings.TimeSeparator,
        ElapsedSeconds mod 60, FormatSettings.DecimalSeparator, ElapsedTime mod 1000])]));
  finally
    AppData.Lines.Free;
    FCompiling := False;
    SetLowPriority(False, FSavePriorityClass);
    OldActiveMemo.Cursor := crDefault;
    OldActiveMemo.SetCursorID(SC_CURSORNORMAL);
    CompilerOutputList.Cursor := crDefault;
    for Memo in FFileMemos do
      Memo.ReadOnly := False;
    UpdateEditModePanel;
    UpdateRunMenu;
    UpdateCaption;
    UpdatePreprocMemos;
    if AppData.DebugInfo <> nil then begin
      ParseDebugInfo(AppData.DebugInfo); { Must be called after UpdateIncludedFilesMemos }
      FreeMem(AppData.DebugInfo);
    end;
    InvalidateStatusPanel(spCompileIcon);
    InvalidateStatusPanel(spCompileProgress);
    SetAppTaskbarProgressState(tpsNoProgress);
    StatusBar.Panels[spExtraStatus].Text := '';
  end;
  FCompiledExe := AppData.OutputExe;
  FModifiedAnySinceLastCompile := False;
  FModifiedAnySinceLastCompileAndGo := False;
end;

procedure TMainForm.SyncEditorOptions;
const
  SquigglyStyles: array[Boolean] of Integer = (INDIC_HIDDEN, INDIC_SQUIGGLE);
  WhiteSpaceStyles: array[Boolean] of Integer = (SCWS_INVISIBLE, SCWS_VISIBLEALWAYS);
var
  Memo: TIDEScintEdit;
begin
  for Memo in FMemos do begin
    Memo.UseStyleAttributes := FOptions.UseSyntaxHighlighting;
    Memo.Call(SCI_INDICSETSTYLE, minSquiggly, SquigglyStyles[FOptions.UnderlineErrors]);
    Memo.Call(SCI_SETVIEWWS, WhiteSpaceStyles[FOptions.ShowWhiteSpace], 0);

    if FOptions.CursorPastEOL then
      Memo.VirtualSpaceOptions := [svsRectangularSelection, svsUserAccessible, svsNoWrapLineStart]
    else
      Memo.VirtualSpaceOptions := [];
    Memo.FillSelectionToEdge := FOptions.CursorPastEOL;

    Memo.TabWidth := FOptions.TabWidth;
    Memo.UseTabCharacter := FOptions.UseTabCharacter;

    Memo.KeyMappingType := FOptions.MemoKeyMappingType;
    if Memo = FMainMemo then begin
      SetFakeShortCut(ESelectNextOccurrence,  FMainMemo.GetComplexCommandShortCut(ccSelectNextOccurrence));
      SetFakeShortCut(ESelectAllOccurrences, FMainMemo.GetComplexCommandShortCut(ccSelectAllOccurrences));
      SetFakeShortCut(ESelectAllFindMatches, FMainMemo.GetComplexCommandShortCut(ccSelectAllFindMatches));
      SetFakeShortCut(EFoldLine, FMainMemo.GetComplexCommandShortCut(ccFoldLine));
      SetFakeShortCut(EUnfoldLine, FMainMemo.GetComplexCommandShortCut(ccUnfoldLine));
      SetFakeShortCut(EToggleLinesComment, FMainMemo.GetComplexCommandShortCut(ccToggleLinesComment));
      SetFakeShortCut(EBraceMatch, FMainMemo.GetComplexCommandShortCut(ccBraceMatch));
    end;

    Memo.UseFolding := FOptions.UseFolding;
    Memo.WordWrap := FOptions.WordWrap;

    if FOptions.IndentationGuides then
      Memo.IndentationGuides := sigLookBoth
    else
      Memo.IndentationGuides := sigNone;

    Memo.LineNumbers := FOptions.GutterLineNumbers;
  end;
end;

procedure TMainForm.FMenuClick(Sender: TObject);
var
  I: Integer;
begin
  FSaveMainFileAs.Enabled := FActiveMemo = FMainMemo;
  FSaveEncoding.Enabled := FSave.Enabled; { FSave.Enabled is kept up-to-date by UpdateSaveMenuItemAndButton }
  FSaveEncodingAuto.Checked := FSaveEncoding.Enabled and ((FActiveMemo as TIDEScintFileEdit).SaveEncoding = seAuto);
  FSaveEncodingUTF8WithBOM.Checked := FSaveEncoding.Enabled and ((FActiveMemo as TIDEScintFileEdit).SaveEncoding = seUTF8WithBOM);
  FSaveEncodingUTF8WithoutBOM.Checked := FSaveEncoding.Enabled and ((FActiveMemo as TIDEScintFileEdit).SaveEncoding = seUTF8WithoutBOM);
  FSaveAll.Visible := FOptions.OpenIncludedFiles;
  ReadMRUMainFilesList;
  FRecent.Visible := FMRUMainFilesList.Count <> 0;
  for I := 0 to High(FMRUMainFilesMenuItems) do
    with FMRUMainFilesMenuItems[I] do begin
      if I < FMRUMainFilesList.Count then begin
        Visible := True;
        Caption := '&' + IntToStr((I+1) mod 10) + ' ' + DoubleAmp(FMRUMainFilesList[I]);
      end
      else
        Visible := False;
    end;

  ApplyMenuBitmaps(Sender as TMenuItem);
end;

procedure TMainForm.FNewMainFileClick(Sender: TObject);
begin
  if ConfirmCloseFile(True) then
    NewMainFile;
end;

procedure TMainForm.FNewMainFileUserWizardClick(Sender: TObject);
begin
  if ConfirmCloseFile(True) then
    NewMainFileUsingWizard;
end;

procedure TMainForm.ShowOpenMainFileDialog(const Examples: Boolean);
var
  InitialDir, FileName: String;
begin
  if Examples then begin
    InitialDir := PathExtractPath(NewParamStr(0)) + 'Examples';
    Filename := PathExtractPath(NewParamStr(0)) + 'Examples\Example1.iss';
  end
  else begin
    InitialDir := PathExtractDir(FMainMemo.Filename);
    Filename := '';
  end;
  if ConfirmCloseFile(True) then
    if NewGetOpenFileName('', FileName, InitialDir, SCompilerOpenFilter, 'iss', Handle) then
      OpenFile(FMainMemo, Filename, False);
end;

procedure TMainForm.FOpenMainFileClick(Sender: TObject);
begin
  ShowOpenMainFileDialog(False);
end;

procedure TMainForm.FSaveClick(Sender: TObject);
begin
  SaveFile((FActiveMemo as TIDEScintFileEdit), Sender = FSaveMainFileAs);
end;

procedure TMainForm.FSaveEncodingItemClick(Sender: TObject);
begin
  var Memo := (FActiveMemo as TIDEScintFileEdit);

  var OldSaveEncoding := Memo.SaveEncoding;

  if Sender = FSaveEncodingUTF8WithBOM  then
    Memo.SaveEncoding := seUTF8WithBOM
  else if Sender = FSaveEncodingUTF8WithoutBOM  then
    Memo.SaveEncoding := seUTF8WithoutBOM
  else
    Memo.SaveEncoding := seAuto;

  if Memo.SaveEncoding <> OldSaveEncoding then
    Memo.ForceModifiedState;
end;

procedure TMainForm.FSaveAllClick(Sender: TObject);
var
  Memo: TIDEScintFileEdit;
begin
  for Memo in FFileMemos do
    if Memo.Used and Memo.Modified then
      SaveFile(Memo, False);
end;

procedure TMainForm.FPrintClick(Sender: TObject);

  procedure SetupNonDarkPrintStyler(var PrintStyler: TInnoSetupStyler; var PrintTheme: TTheme;
    var OldStyler: TScintCustomStyler; var OldTheme: TTheme);
  begin
    { Not the most pretty code, would ideally make a copy of FActiveMemo and print that instead or
      somehow convince Scintilla to use different print styles but don't know of a good way to do
      either. Using SC_PRINT_COLOURONWHITE doesn't help, this gives white on white in dark mode. }
    PrintStyler := TInnoSetupStyler.Create(nil);
    PrintTheme := TTheme.Create;
    PrintStyler.ISPPInstalled := ISPPInstalled;
    PrintStyler.Theme := PrintTheme;
    if not FTheme.Dark then
      PrintTheme.Typ := FTheme.Typ
    else
      PrintTheme.Typ := ttModernLight;
    OldStyler := FActiveMemo.Styler;
    OldTheme := FActiveMemo.Theme;
    FActiveMemo.Styler := PrintStyler;
    FActiveMemo.Theme := PrintTheme;
    FActiveMemo.UpdateThemeColorsAndStyleAttributes;
  end;

  procedure DeinitPrintStyler(const PrintStyler: TInnoSetupStyler; const PrintTheme: TTheme;
    const OldStyler: TScintCustomStyler; const OldTheme: TTheme);
  begin
    if (OldStyler <> nil) or (OldTheme <> nil) then begin
      if OldStyler <> nil then
        FActiveMemo.Styler := OldStyler;
      if OldTheme <> nil then
        FActiveMemo.Theme := OldTheme;
      FActiveMemo.UpdateThemeColorsAndStyleAttributes;
    end;
    if PrintTheme <> FTheme then
      PrintTheme.Free;
    PrintStyler.Free;
  end;

var
  PrintStyler: TInnoSetupStyler;
  OldStyler: TScintCustomStyler;
  PrintTheme, OldTheme: TTheme;
  PrintMemo: TIDEScintEdit;
  HeaderMemo: TIDEScintFileEdit;
  FileTitle, S: String;
  pdlg: TPrintDlg;
  hdc: Windows.HDC;
  rectMargins, rectPhysMargins, rectSetup, rcw: TRect;
  ptPage, ptDpi: TPoint;
  headerLineHeight, footerLineHeight: Integer;
  fontHeader, fontFooter: HFONT;
  tm: TTextMetric;
  di: TDocInfo;
  lengthDoc, lengthDocMax, lengthPrinted: Integer;
  frPrint: TScintRangeToFormat;
  pageNum: Integer;
  printPage: Boolean;
  ta: UINT;
  sHeader, sFooter: String;
  pen, penOld: HPEN;
begin
  if FActiveMemo is TIDEScintFileEdit then
    HeaderMemo := TIDEScintFileEdit(FActiveMemo)
  else
    HeaderMemo := FMainMemo;
  sHeader := HeaderMemo.Filename;
  FileTitle := GetFileTitle(HeaderMemo.Filename);
  if HeaderMemo <> FActiveMemo then begin
    S := ' - ' + MemosTabSet.Tabs[MemoToTabIndex(FActiveMemo)];
    sHeader := Format('%s  %s', [sHeader, S]);
    FileTitle := Format('%s %s', [FileTitle, S]);
  end;
  sHeader := Format('%s - %s', [sHeader, DateTimeToStr(Now())]);

  { Based on SciTE 5.50's SciTEWin::Print }
  
  ZeroMemory(@pdlg, SizeOf(pdlg));
  pdlg.lStructSize := SizeOf(pdlg);
  pdlg.hwndOwner := Handle;
  pdlg.hInstance := hInstance;
  pdlg.Flags := PD_USEDEVMODECOPIES or PD_ALLPAGES or PD_RETURNDC;
  pdlg.nFromPage := 1;
  pdlg.nToPage := 1;
  pdlg.nMinPage := 1;
  pdlg.nMaxPage := $ffff; // We do not know how many pages in the document until the printer is selected and the paper size is known.
  pdlg.nCopies := 1;
  pdlg.hDC := 0;
  pdlg.hDevMode := FDevMode;
  pdlg.hDevNames := FDevNames;

  // See if a range has been selected
  var rangeSelection := FActiveMemo.Selection;
  if rangeSelection.StartPos = rangeSelection.EndPos then
    pdlg.Flags := pdlg.Flags or PD_NOSELECTION
  else
    pdlg.Flags := pdlg.Flags or PD_SELECTION;

  if not PrintDlg(pdlg) then
    Exit;

  PrintStyler := nil;
  PrintTheme := nil;
  OldStyler := nil;
  OldTheme := nil;
  try
    if FTheme.Dark then
      SetupNonDarkPrintStyler(PrintStyler, PrintTheme, OldStyler, OldTheme)
    else
      PrintTheme := FTheme;

    FDevMode := pdlg.hDevMode;
    FDevNames := pdlg.hDevNames;

    hdc := pdlg.hDC;

    // Get printer resolution
    ptDpi.x := GetDeviceCaps(hdc, LOGPIXELSX);    // dpi in X direction
    ptDpi.y := GetDeviceCaps(hdc, LOGPIXELSY);    // dpi in Y direction

    // Start by getting the physical page size (in device units).
    ptPage.x := GetDeviceCaps(hdc, PHYSICALWIDTH);   // device units
    ptPage.y := GetDeviceCaps(hdc, PHYSICALHEIGHT);  // device units

    // Get the dimensions of the unprintable
    // part of the page (in device units).
    rectPhysMargins.left := GetDeviceCaps(hdc, PHYSICALOFFSETX);
    rectPhysMargins.top := GetDeviceCaps(hdc, PHYSICALOFFSETY);

    // To get the right and lower unprintable area,
    // we take the entire width and height of the paper and
    // subtract everything else.
    rectPhysMargins.right := ptPage.x                       // total paper width
                             - GetDeviceCaps(hdc, HORZRES)  // printable width
                             - rectPhysMargins.left;        // left unprintable margin

    rectPhysMargins.bottom := ptPage.y                      // total paper height
                              - GetDeviceCaps(hdc, VERTRES) // printable height
                              - rectPhysMargins.top;        // right unprintable margin

    // At this point, rectPhysMargins contains the widths of the
    // unprintable regions on all four sides of the page in device units.

  (*
    // Take in account the page setup given by the user (if one value is not null)
    if (pagesetupMargin.left != 0 || pagesetupMargin.right != 0 ||
            pagesetupMargin.top != 0 || pagesetupMargin.bottom != 0) {
      GUI::Rectangle rectSetup;

      // Convert the hundredths of millimeters (HiMetric) or
      // thousandths of inches (HiEnglish) margin values
      // from the Page Setup dialog to device units.
      // (There are 2540 hundredths of a mm in an inch.)

      TCHAR localeInfo[3];
      GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IMEASURE, localeInfo, 3);

      if (localeInfo[0] == '0') { // Metric system. '1' is US System   *)
        rectSetup.left := MulDiv(500 {pagesetupMargin.left}, ptDpi.x, 2540);
        rectSetup.top := MulDiv(500 {pagesetupMargin.top}, ptDpi.y, 2540);
        rectSetup.right := MulDiv(500 {pagesetupMargin.right}, ptDpi.x, 2540);
        rectSetup.bottom  := MulDiv(500 {pagesetupMargin.bottom}, ptDpi.y, 2540);
      (* } else {
        rectSetup.left  = MulDiv(pagesetupMargin.left, ptDpi.x, 1000);
        rectSetup.top = MulDiv(pagesetupMargin.top, ptDpi.y, 1000);
        rectSetup.right = MulDiv(pagesetupMargin.right, ptDpi.x, 1000);
        rectSetup.bottom  = MulDiv(pagesetupMargin.bottom, ptDpi.y, 1000);
      } *)

      // Don't reduce margins below the minimum printable area
      rectMargins.left := Max(rectPhysMargins.left, rectSetup.left);
      rectMargins.top := Max(rectPhysMargins.top, rectSetup.top);
      rectMargins.right := Max(rectPhysMargins.right, rectSetup.right);
      rectMargins.bottom := Max(rectPhysMargins.bottom, rectSetup.bottom);
  (*
    } else {
      rectMargins := rectPhysMargins;
    }
  *)

    // rectMargins now contains the values used to shrink the printable
    // area of the page.

    // Convert device coordinates into logical coordinates
    DPtoLP(hdc, rectMargins, 2);
    DPtoLP(hdc, rectPhysMargins, 2);

    // Convert page size to logical units and we're done!
    DPtoLP(hdc, ptPage, 1);

    headerLineHeight := MulDiv(9, ptDpi.y, 72);
    fontHeader := CreateFont(headerLineHeight, 0, 0, 0, FW_REGULAR, 1, 0, 0, 0, 0, 0, 0, 0, PChar(FActiveMemo.Font.Name));
    SelectObject(hdc, fontHeader);
    GetTextMetrics(hdc, &tm);
    headerLineHeight := tm.tmHeight + tm.tmExternalLeading;

    footerLineHeight := MulDiv(9, ptDpi.y, 72);
    fontFooter := CreateFont(footerLineHeight, 0, 0, 0, FW_REGULAR, 0, 0, 0, 0, 0, 0, 0, 0, PChar(FActiveMemo.Font.Name));
    SelectObject(hdc, fontFooter);
    GetTextMetrics(hdc, &tm);
    footerLineHeight := tm.tmHeight + tm.tmExternalLeading;

    ZeroMemory(@di, SizeOf(di));
    di.cbSize := SizeOf(di);
    di.lpszDocName  := PChar(FileTitle);
    di.lpszOutput := nil;
    di.lpszDatatype := nil;
    di.fwType := 0;

    if StartDoc(hdc, &di) < 0 then begin
      DeleteDC(hdc);
      DeleteObject(fontHeader);
      DeleteObject(fontFooter);
      MsgBox('Can not start printer document.', SCompilerFormCaption, mbError, MB_OK);
      Exit;
    end;

    lengthDocMax := FActiveMemo.GetRawTextLength;
    // PD_SELECTION -> requested to print selection.
    lengthDoc := IfThen((pdlg.Flags and PD_SELECTION) <> 0, rangeSelection.EndPos, lengthDocMax);
    lengthPrinted := IfThen((pdlg.Flags and PD_SELECTION) <> 0, rangeSelection.StartPos, 0);

    // We must subtract the physical margins from the printable area
    frPrint.hdc := hdc;
    frPrint.hdcTarget := hdc;
    frPrint.rc.left := rectMargins.left - rectPhysMargins.left;
    frPrint.rc.top := rectMargins.top - rectPhysMargins.top;
    frPrint.rc.right := ptPage.x - rectMargins.right - rectPhysMargins.left;
    frPrint.rc.bottom := ptPage.y - rectMargins.bottom - rectPhysMargins.top;
    frPrint.rcPage.left := 0;
    frPrint.rcPage.top := 0;
    frPrint.rcPage.right := ptPage.x - rectPhysMargins.left - rectPhysMargins.right - 1;
    frPrint.rcPage.bottom := ptPage.y - rectPhysMargins.top - rectPhysMargins.bottom - 1;
    frPrint.rc.top := frPrint.rc.top + headerLineHeight + headerLineHeight div 2;
    frPrint.rc.bottom := frPrint.rc.bottom - (footerLineHeight + footerLineHeight div 2);

    // Print each page
    pageNum := 1;

    while lengthPrinted < lengthDoc do begin
      printPage := ((pdlg.Flags and PD_PAGENUMS) = 0) or
                   ((pageNum >= pdlg.nFromPage) and (pageNum <= pdlg.nToPage));

      sFooter := Format('- %d -', [pageNum]);

      if printPage then begin
        StartPage(hdc);

        SetTextColor(hdc, PrintTheme.Colors[tcFore]);
        SetBkColor(hdc, PrintTheme.Colors[tcBack]);
        SelectObject(hdc, fontHeader);
        ta := SetTextAlign(hdc, TA_BOTTOM);
        rcw := Rect(frPrint.rc.left, frPrint.rc.top - headerLineHeight - headerLineHeight div 2,
                    frPrint.rc.right, frPrint.rc.top - headerLineHeight div 2);
        rcw.bottom := rcw.top + headerLineHeight;
        ExtTextOut(hdc, frPrint.rc.left + 5, frPrint.rc.top - headerLineHeight div 2,
                   ETO_OPAQUE, rcw, sHeader, Length(sHeader), nil);
        SetTextAlign(hdc, ta);
        pen := CreatePen(0, 1, GetTextColor(hdc));
        penOld := SelectObject(hdc, pen);
        MoveToEx(hdc, frPrint.rc.left, frPrint.rc.top - headerLineHeight div 4, nil);
        LineTo(hdc, frPrint.rc.right, frPrint.rc.top - headerLineHeight div 4);
        SelectObject(hdc, penOld);
        DeleteObject(pen);
      end;

      frPrint.chrg.StartPos := lengthPrinted;
      frPrint.chrg.EndPos := lengthDoc;

      lengthPrinted := FActiveMemo.FormatRange(printPage, @frPrint);

      if printPage then begin
        SetTextColor(hdc, PrintTheme.Colors[tcFore]);
        SetBkColor(hdc, PrintTheme.Colors[tcBack]);
        SelectObject(hdc, fontFooter);
        ta := SetTextAlign(hdc, TA_TOP);
        rcw := Rect(frPrint.rc.left, frPrint.rc.bottom + footerLineHeight div 2,
                    frPrint.rc.right, frPrint.rc.bottom + footerLineHeight + footerLineHeight div 2);
        ExtTextOut(hdc, frPrint.rc.left + 5, frPrint.rc.bottom + footerLineHeight div 2,
                   ETO_OPAQUE, rcw, sFooter, Length(sFooter), nil);
        SetTextAlign(hdc, ta);
        pen := CreatePen(0, 1, GetTextColor(hdc));
        penOld := SelectObject(hdc, pen);
        MoveToEx(hdc, frPrint.rc.left, frPrint.rc.bottom + footerLineHeight div 4, nil);
        LineTo(hdc, frPrint.rc.right, frPrint.rc.bottom + footerLineHeight div 4);
        SelectObject(hdc, penOld);
        DeleteObject(pen);

        EndPage(hdc);
      end;
      Inc(pageNum);

      if ((pdlg.Flags and PD_PAGENUMS) <> 0) and (pageNum > pdlg.nToPage) then
        Break;
    end;

    FActiveMemo.FormatRange(False, nil);

    EndDoc(hdc);
    DeleteDC(hdc);
    DeleteObject(fontHeader);
    DeleteObject(fontFooter);
  finally
    DeinitPrintStyler(PrintStyler, PrintTheme, OldStyler, OldTheme);
  end;
end;

procedure TMainForm.FClearRecentClick(Sender: TObject);
begin
  if MsgBox('Are you sure you want to clear the list of recently opened files?',
    SCompilerFormCaption, mbConfirmation, MB_YESNO or MB_DEFBUTTON2) <> IDNO then
      ClearMRUMainFilesList;
end;

procedure TMainForm.FMRUClick(Sender: TObject);
var
  I: Integer;
begin
  if ConfirmCloseFile(True) then
    for I := 0 to High(FMRUMainFilesMenuItems) do
      if FMRUMainFilesMenuItems[I] = Sender then begin
        OpenMRUMainFile(FMRUMainFilesList[I]);
        Break;
      end;
end;

procedure TMainForm.FExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.EMenuClick(Sender: TObject);
var
  MemoHasFocus, MemoIsReadOnly: Boolean;
begin
  MemoHasFocus := FActiveMemo.Focused;
  MemoIsReadOnly := FActiveMemo.ReadOnly;
  EUndo.Enabled := MemoHasFocus and FActiveMemo.CanUndo;
  ERedo.Enabled := MemoHasFocus and FActiveMemo.CanRedo;
  ECut.Enabled := MemoHasFocus and not MemoIsReadOnly and not FActiveMemo.SelEmpty;
  ECopy.Enabled := MemoHasFocus and not FActiveMemo.SelEmpty;
  EPaste.Enabled := MemoHasFocus and FActiveMemo.CanPaste;
  EDelete.Enabled := MemoHasFocus and not FActiveMemo.SelEmpty;
  ESelectAll.Enabled := MemoHasFocus;
  ESelectNextOccurrence.Enabled := MemoHasFocus;
  ESelectAllOccurrences.Enabled := MemoHasFocus;
  ESelectAllFindMatches.Enabled := MemoHasFocus and (FLastFindText <> '');
  EFind.Enabled := MemoHasFocus;
  EFindNext.Enabled := MemoHasFocus;
  EFindPrevious.Enabled := MemoHasFocus;
  EReplace.Enabled := MemoHasFocus and not MemoIsReadOnly;
  EFindRegEx.Checked := FOptions.FindRegEx;
  EFoldLine.Visible := FOptions.UseFolding;
  EFoldLine.Enabled := MemoHasFocus;
  EUnfoldLine.Visible := EFoldLine.Visible;
  EUnfoldLine.Enabled := EFoldLine.Enabled;
  EGoto.Enabled := MemoHasFocus;
  EToggleLinesComment.Enabled := not MemoIsReadOnly;
  EBraceMatch.Enabled := MemoHasFocus;

  ApplyMenuBitmaps(Sender as TMenuItem);
end;

procedure TMainForm.EUndoClick(Sender: TObject);
begin
  FActiveMemo.Undo;
end;

procedure TMainForm.ERedoClick(Sender: TObject);
begin
  FActiveMemo.Redo;
end;

procedure TMainForm.ECutClick(Sender: TObject);
begin
  FActiveMemo.CutToClipboard;
end;

procedure TMainForm.ECopyClick(Sender: TObject);
begin
  FActiveMemo.CopyToClipboard;
end;

function TMainForm.MultipleSelectionPasteFromClipboard(const AMemo: TIDEScintEdit): Boolean;
begin
  { Scintilla doesn't yet properly support multiple selection paste. Handle it
    here, just like VS and VSCode do: if there's multiple selections and the paste
    text has the same amount of lines then paste 1 line per selection. Do this even
    if the paste text is marked as rectangular. Otherwise (so no match between
    the selection count and the line count) paste all lines into each selection.
    For the latter we don't need handling here: this is Scintilla's default
    behaviour if SC_MULTIPASTE_EACH is on. }
  Result := False;
  var SelectionCount := AMemo.SelectionCount;
  if SelectionCount > 1 then begin
    var PasteLines := Clipboard.AsText.Replace(#13#10, #13).Split([#13, #10]);
    if SelectionCount = Length(PasteLines) then begin
      AMemo.BeginUndoAction;
      try
        for var I := 0 to SelectionCount-1 do begin
          var StartPos := AMemo.SelectionStartPosition[I]; { Can't use AMemo.GetSelections because each paste can update other selections }
          var EndPos := AMemo.SelectionEndPosition[I];
          AMemo.ReplaceTextRange(StartPos, EndPos, PasteLines[I], srmMinimal);
          { Update the selection to an empty selection at the end of the inserted
            text, just like ReplaceMainSelText }
          var Pos := AMemo.Target.EndPos; { ReplaceTextRange updates the target }
          AMemo.SelectionCaretPosition[I] := Pos;
          AMemo.SelectionAnchorPosition[I] := Pos;
        end;
        { Be like SCI_PASTE }
        AMemo.ChooseCaretX;
        AMemo.ScrollCaretIntoView;
      finally
        AMemo.EndUndoAction;
      end;
      Result := True;
    end;
  end;
end;

procedure TMainForm.EPasteClick(Sender: TObject);
begin
  if not MultipleSelectionPasteFromClipboard(FActiveMemo) then
    FActiveMemo.PasteFromClipboard;
end;

procedure TMainForm.EDeleteClick(Sender: TObject);
begin
  FActiveMemo.ClearSelection;
end;

procedure TMainForm.ESelectAllClick(Sender: TObject);
begin
  FActiveMemo.SelectAll;
end;

procedure TMainForm.ESelectAllOccurrencesClick(Sender: TObject);
begin
  { Might be called even if ESelectAllOccurrences.Enabled would be False in EMenuClick }
  if FActiveMemo.SelEmpty then begin
    { If the selection is empty then SelectAllOccurrences will actually just select
      the word at caret which is not what we want, so preselect this word ourselves }
    var Range := FActiveMemo.WordAtCaretRange;
    if Range.StartPos <> Range.EndPos then
      FActiveMemo.SetSingleSelection(Range.EndPos, Range.StartPos);
  end;
  FActiveMemo.SelectAllOccurrences([sfoMatchCase]);
end;

procedure TMainForm.ESelectNextOccurrenceClick(Sender: TObject);
begin
  { Might be called even if ESelectNextOccurrence.Enabled would be False in EMenuClick }
  FActiveMemo.SelectNextOccurrence([sfoMatchCase]);
end;

procedure TMainForm.EToggleLinesCommentClick(Sender: TObject);
begin
  var AMemo := FActiveMemo;

  { Based on SciTE 5.50's SciTEBase::StartBlockComment - only toggles comments
    for the main selection }

  var Selection := AMemo.Selection;
  var CaretPosition := AMemo.CaretPosition;
  // checking if caret is located in _beginning_ of selected block
  var MoveCaret := CaretPosition < Selection.EndPos;
  var SelStartLine := AMemo.GetLineFromPosition(Selection.StartPos);
  var SelEndLine := AMemo.GetLineFromPosition(Selection.EndPos);
  var Lines := SelEndLine - SelStartLine;
  var FirstSelLineStart := AMemo.GetPositionFromLine(SelStartLine);
  // "caret return" is part of the last selected line
  if (Lines > 0) and (Selection.EndPos = AMemo.GetPositionFromLine(SelEndLine)) then
    Dec(SelEndLine);
  { We rely on the styler to identify [Code] section lines, but we
    may be searching into areas that haven't been styled yet }
  AMemo.StyleNeeded(Selection.EndPos);
  AMemo.BeginUndoAction;
  try
    var LastLongCommentLength := 0;
    for var I := SelStartLine to SelEndLine do begin
      var LineIndent := AMemo.GetLineIndentPosition(I);
      var LineEnd := AMemo.GetLineEndPosition(I);
      var LineBuf := AMemo.GetTextRange(LineIndent, LineEnd);
      // empty lines are not commented
      if LineBuf = '' then
        Continue;
      var Comment: String;
      if LineBuf.StartsWith('//') or
         (FMemosStyler.GetSectionFromLineState(AMemo.Lines.State[I]) = scCode) then
        Comment := '//'
      else
        Comment := ';';
      var LongComment := Comment + ' ';
      LastLongCommentLength := Length(LongComment);
      if LineBuf.StartsWith(Comment) then begin
        var CommentLength := Length(Comment);
        if LineBuf.StartsWith(LongComment) then begin
          // Removing comment with space after it.
          CommentLength := Length(LongComment);
        end;
        AMemo.Selection := TScintRange.Create(LineIndent, LineIndent + CommentLength);
        AMemo.SelText := '';
        if I = SelStartLine then // is this the first selected line?
          Dec(Selection.StartPos, CommentLength);
        Dec(Selection.EndPos, CommentLength); // every iteration
        Continue;
      end;
      if I = SelStartLine then // is this the first selected line?
        Inc(Selection.StartPos, Length(LongComment));
      Inc(Selection.EndPos, Length(LongComment)); // every iteration
      AMemo.Call(SCI_INSERTTEXT, LineIndent, AMemo.ConvertStringToRawString(LongComment));
    end;
    // after uncommenting selection may promote itself to the lines
    // before the first initially selected line;
    // another problem - if only comment symbol was selected;
    if Selection.StartPos < FirstSelLineStart then begin
      if Selection.StartPos >= Selection.EndPos - (LastLongCommentLength - 1) then
        Selection.EndPos := FirstSelLineStart;
      Selection.StartPos := FirstSelLineStart;
    end;
    if MoveCaret then begin
      // moving caret to the beginning of selected block
      AMemo.CaretPosition := Selection.EndPos;
      AMemo.CaretPositionWithSelectFromAnchor := Selection.StartPos;
    end else
      AMemo.Selection := Selection;
  finally
    AMemo.EndUndoAction;
  end;
end;

procedure TMainForm.EBraceMatchClick(Sender: TObject);
begin
  var AMemo := FActiveMemo;

  var Selections: TScintCaretAndAnchorList := nil;
  var VirtualSpaces: TScintCaretAndAnchorList := nil;
  try
    Selections := TScintCaretAndAnchorList.Create;
    VirtualSpaces := TScintCaretAndAnchorList.Create;
    AMemo.GetSelections(Selections, VirtualSpaces);
    for var I := 0 to Selections.Count-1 do begin
      if VirtualSpaces[I].CaretPos = 0 then begin
        var Pos := Selections[I].CaretPos;
        var MatchPos := AMemo.GetPositionOfMatchingBrace(Pos);
        if MatchPos = -1 then begin
          Pos := AMemo.GetPositionBefore(Pos);
          MatchPos := AMemo.GetPositionOfMatchingBrace(Pos)
        end;
        if MatchPos <> -1 then begin
          AMemo.SelectionCaretPosition[I] := MatchPos;
          AMemo.SelectionAnchorPosition[I] := MatchPos;
          if I = 0 then
            AMemo.ScrollCaretIntoView;
        end;
      end;
    end;
  finally
    VirtualSpaces.Free;
    Selections.Free;
  end;
end;

procedure TMainForm.ESelectAllFindMatchesClick(Sender: TObject);
begin
  { Might be called even if ESelectAllFindMatches.Enabled would be False in EMenuClick }
  if FLastFindText <> ''  then begin
    var StartPos := 0;
    var EndPos := FActiveMemo.RawTextLength;
    var FoundRange: TScintRange;
    var ClosestSelection := -1;
    var ClosestSelectionDistance := 0; { Silence compiler }
    var CaretPos := FActiveMemo.CaretPosition;

    while (StartPos < EndPos) and
          FActiveMemo.FindText(StartPos, EndPos, FLastFindText,
            FindOptionsToSearchOptions(FLastFindOptions, FLastFindRegEx), FoundRange) do begin
      if StartPos = 0 then
        FActiveMemo.SetSingleSelection(FoundRange.EndPos, FoundRange.StartPos)
      else
        FActiveMemo.AddSelection(FoundRange.EndPos, FoundRange.StartPos);

      var Distance := Abs(CaretPos-FoundRange.EndPos);
      if (ClosestSelection = -1) or (Distance < ClosestSelectionDistance) then begin
        ClosestSelection := FActiveMemo.SelectionCount-1;
        ClosestSelectionDistance := Distance;
      end;

      StartPos := FoundRange.EndPos;
    end;
    if ClosestSelection <> -1 then begin
      FActiveMemo.MainSelection := ClosestSelection;
      FActiveMemo.ScrollCaretIntoView;
    end;
  end;
end;

procedure TMainForm.VMenuClick(Sender: TObject);
begin
  VZoomIn.Enabled := (FActiveMemo.Zoom < 20);
  VZoomOut.Enabled := (FActiveMemo.Zoom > -10);
  VZoomReset.Enabled := (FActiveMemo.Zoom <> 0);
  VToolbar.Checked := ToolbarPanel.Visible;
  VStatusBar.Checked := StatusBar.Visible;
  VNextTab.Enabled := MemosTabSet.Visible and (MemosTabSet.Tabs.Count > 1);
  VPreviousTab.Enabled := VNextTab.Enabled;
  VCloseCurrentTab.Enabled := MemosTabSet.Visible and (FActiveMemo <> FMainMemo) and (FActiveMemo <> FPreprocessorOutputMemo);
  VReopenTab.Visible := MemosTabSet.Visible and (FHiddenFiles.Count > 0);
  if VReopenTab.Visible then
    UpdateReopenTabMenu(VReopenTab);
  VReopenTabs.Visible := VReopenTab.Visible;
  VHide.Checked := not StatusPanel.Visible;
  VCompilerOutput.Checked := StatusPanel.Visible and (OutputTabSet.TabIndex = tiCompilerOutput);
  VDebugOutput.Checked := StatusPanel.Visible and (OutputTabSet.TabIndex = tiDebugOutput);
  VDebugCallStack.Checked := StatusPanel.Visible and (OutputTabSet.TabIndex = tiDebugCallStack);
  VFindResults.Checked := StatusPanel.Visible and (OutputTabSet.TabIndex = tiFindResults);
  VWordWrap.Checked := FOptions.WordWrap;

  ApplyMenuBitmaps(Sender as TMenuItem);
end;

procedure TMainForm.VNextTabClick(Sender: TObject);
var
  NewTabIndex: Integer;
begin
  NewTabIndex := MemosTabSet.TabIndex+1;
  if NewTabIndex >= MemosTabSet.Tabs.Count then
    NewTabIndex := 0;
  MemosTabSet.TabIndex := NewTabIndex;
end;

procedure TMainForm.VPreviousTabClick(Sender: TObject);
var
  NewTabIndex: Integer;
begin
  NewTabIndex := MemosTabSet.TabIndex-1;
  if NewTabIndex < 0 then
    NewTabIndex := MemosTabSet.Tabs.Count-1;
  MemosTabSet.TabIndex := NewTabIndex;
end;

procedure TMainForm.CloseTab(const TabIndex: Integer);
begin
  var Memo := TabIndexToMemo(TabIndex, MemosTabSet.Tabs.Count-1);
  var MemoWasActiveMemo := Memo = FActiveMemo;

  MemosTabSet.Tabs.Delete(TabIndex); { This will not change MemosTabset.TabIndex }
  MemosTabSet.Hints.Delete(TabIndex);
  MemosTabSet.CloseButtons.Delete(TabIndex);

  FHiddenFiles.Add((Memo as TIDEScintFileEdit).Filename);
  InvalidateStatusPanel(spHiddenFilesCount);
  BuildAndSaveKnownIncludedAndHiddenFiles;

  { Because MemosTabSet.Tabs and FHiddenFiles have both been updated now,
    hereafter setting TabIndex will not select the memo we're closing
    even if it's not hidden yet because TabIndexToMemo as called by
    MemosTabSetClick will skip it }

  if MemoWasActiveMemo then begin
    { Select next tab, except when we're already at the end. Avoiding flicker by
      doing this before hiding old active memo. We do this in a dirty way by
      clicking two tabs while making sure TabSetClick doesn't see the first
      'fake' one. }
    FIgnoreTabSetClick := True;
    try
      VNextTabClick(Self);
    finally
      FIgnoreTabSetClick := False;
    end;
    VPreviousTabClick(Self);
    Memo.CancelAutoCompleteAndCallTip;
    Memo.Visible := False;
  end else if TabIndex < MemosTabset.TabIndex then
    MemosTabSet.TabIndex := MemosTabset.TabIndex-1; { Reselect old selected tab }
end;

procedure TMainForm.VCloseCurrentTabClick(Sender: TObject);
begin
  CloseTab(MemosTabSet.TabIndex);
end;

procedure TMainForm.ReopenTabOrTabs(const HiddenFileIndex: Integer;
  const Activate: Boolean);
begin
  var ReopenFilename: String;
  if HiddenFileIndex >= 0 then begin
    ReopenFilename := FHiddenFiles[HiddenFileIndex];
    FHiddenFiles.Delete(HiddenFileIndex);
  end else begin
    ReopenFilename := FHiddenFiles[0];
    FHiddenFiles.Clear;
  end;
  InvalidateStatusPanel(spHiddenFilesCount);

  UpdatePreprocMemos;
  BuildAndSaveKnownIncludedAndHiddenFiles;

  { Activate the memo if requested }
  if Activate then begin
    for var Memo in FFileMemos do begin
      if Memo.Used and (PathCompare(Memo.Filename, ReopenFilename) = 0) then begin
        MemosTabSet.TabIndex := MemoToTabIndex(memo);
        Break;
      end;
    end
  end;
end;

procedure TMainForm.ReopenTabClick(Sender: TObject);
begin
  ReopenTabOrTabs((Sender as TMenuItem).Tag, True);
end;

procedure TMainForm.VReopenTabsClick(Sender: TObject);
begin
  ReopenTabOrTabs(-1, True);
end;

procedure TMainForm.VZoomInClick(Sender: TObject);
begin
  FActiveMemo.ZoomIn; { MemoZoom will zoom the other memos }
end;

procedure TMainForm.VZoomOutClick(Sender: TObject);
begin
  FActiveMemo.ZoomOut;
end;

procedure TMainForm.VZoomResetClick(Sender: TObject);
begin
  FActiveMemo.Zoom := 0;
end;

procedure TMainForm.VToolbarClick(Sender: TObject);
begin
  ToolbarPanel.Visible := not ToolbarPanel.Visible;
end;

procedure TMainForm.VStatusBarClick(Sender: TObject);
begin
  StatusBar.Visible := not StatusBar.Visible;
end;

procedure TMainForm.VWordWrapClick(Sender: TObject);
begin
  FOptions.WordWrap := not FOptions.WordWrap;
  SyncEditorOptions;
  var Ini := TConfigIniFile.Create;
  try
    Ini.WriteBool('Options', 'WordWrap', FOptions.WordWrap);
  finally
    Ini.Free;
  end;
end;

procedure TMainForm.SetStatusPanelVisible(const AVisible: Boolean);
var
  CaretWasInView: Boolean;
begin
  if StatusPanel.Visible <> AVisible then begin
    CaretWasInView := FActiveMemo.IsPositionInViewVertically(FActiveMemo.CaretPosition);
    if AVisible then begin
      { Ensure the status panel height isn't out of range before showing }
      UpdateStatusPanelHeight(StatusPanel.Height);
      SplitPanel.Top := ClientHeight;
      StatusPanel.Top := ClientHeight;
    end
    else begin
      if StatusPanel.ContainsControl(ActiveControl) then
        ActiveControl := FActiveMemo;
    end;
    SplitPanel.Visible := AVisible;
    StatusPanel.Visible := AVisible;
    if AVisible and CaretWasInView then begin
      { If the caret was in view, make sure it still is }
      FActiveMemo.ScrollCaretIntoView;
    end;
  end;
end;

procedure TMainForm.VHideClick(Sender: TObject);
begin
  SetStatusPanelVisible(False);
end;

procedure TMainForm.VCompilerOutputClick(Sender: TObject);
begin
  OutputTabSet.TabIndex := tiCompilerOutput;
  SetStatusPanelVisible(True);
end;

procedure TMainForm.VDebugOutputClick(Sender: TObject);
begin
  OutputTabSet.TabIndex := tiDebugOutput;
  SetStatusPanelVisible(True);
end;

procedure TMainForm.VDebugCallStackClick(Sender: TObject);
begin
  OutputTabSet.TabIndex := tiDebugCallStack;
  SetStatusPanelVisible(True);
end;

procedure TMainForm.VFindResultsClick(Sender: TObject);
begin
  OutputTabSet.TabIndex := tiFindResults;
  SetStatusPanelVisible(True);
end;

procedure TMainForm.BMenuClick(Sender: TObject);
begin
  BLowPriority.Checked := FOptions.LowPriorityDuringCompile;
  BOpenOutputFolder.Enabled := (FCompiledExe <> '');

  ApplyMenuBitmaps(Sender as TMenuItem);
end;

procedure TMainForm.BCompileClick(Sender: TObject);
begin
  CompileFile('', False);
end;

procedure TMainForm.BStopCompileClick(Sender: TObject);
begin
  SetAppTaskbarProgressState(tpsPaused);
  try
    if MsgBox('Are you sure you want to abort the compile?', SCompilerFormCaption,
       mbConfirmation, MB_YESNO or MB_DEFBUTTON2) <> IDNO then
      FCompileWantAbort := True;
  finally
    SetAppTaskbarProgressState(tpsNormal);
  end;
end;

procedure TMainForm.BLowPriorityClick(Sender: TObject);
begin
  FOptions.LowPriorityDuringCompile := not FOptions.LowPriorityDuringCompile;
  { If a compile is already in progress, change the priority now }
  if FCompiling then
    SetLowPriority(FOptions.LowPriorityDuringCompile, FSavePriorityClass);
end;

procedure TMainForm.BOpenOutputFolderClick(Sender: TObject);
begin
  LaunchFileOrURL(AddBackslash(GetSystemWinDir) + 'explorer.exe',
    Format('/select,"%s"', [FCompiledExe]));
end;

procedure TMainForm.HShortcutsDocClick(Sender: TObject);
begin
  if Assigned(HtmlHelp) then
    HtmlHelp(GetDesktopWindow, PChar(GetHelpFile), HH_DISPLAY_TOPIC, Cardinal(PChar('topic_compformshortcuts.htm')));
end;

procedure TMainForm.HRegExDocClick(Sender: TObject);
begin
  if Assigned(HtmlHelp) then
    HtmlHelp(GetDesktopWindow, PChar(GetHelpFile), HH_DISPLAY_TOPIC, Cardinal(PChar('topic_compformregex.htm')));
end;

procedure TMainForm.HDocClick(Sender: TObject);
begin
  if Assigned(HtmlHelp) then
    HtmlHelp(GetDesktopWindow, PChar(GetHelpFile), HH_DISPLAY_TOPIC, 0);
end;

procedure TMainForm.HExamplesClick(Sender: TObject);
begin
  LaunchFileOrURL(PathExtractPath(NewParamStr(0)) + 'Examples');
end;

procedure TMainForm.HFaqClick(Sender: TObject);
begin
  LaunchFileOrURL(PathExtractPath(NewParamStr(0)) + 'isfaq.url');
end;

procedure TMainForm.HWhatsNewClick(Sender: TObject);
begin
  LaunchFileOrURL(PathExtractPath(NewParamStr(0)) + 'whatsnew.htm');
end;

procedure TMainForm.HWebsiteClick(Sender: TObject);
begin
  LaunchFileOrURL('https://jrsoftware.org/isinfo.php');
end;

procedure TMainForm.HMailingListClick(Sender: TObject);
begin
  OpenMailingListSite;
end;

procedure TMainForm.HISPPDocClick(Sender: TObject);
begin
  if Assigned(HtmlHelp) then
    HtmlHelp(GetDesktopWindow, PChar(GetHelpFile), HH_DISPLAY_TOPIC, Cardinal(PChar('topic_isppoverview.htm')));
end;

procedure TMainForm.HDonateClick(Sender: TObject);
begin
  OpenDonateSite;
end;

procedure TMainForm.HAboutClick(Sender: TObject);
var
  S: String;
begin
  { Removing the About box or modifying any existing text inside it is a
    violation of the Inno Setup license agreement; see LICENSE.TXT.
    However, adding additional lines to the About box is permitted, as long as
    they are placed below the original copyright notice. }
  S := FCompilerVersion.Title + ' Compiler version ' +
    String(FCompilerVersion.Version) + SNewLine;
  if FCompilerVersion.Title <> 'Inno Setup' then
    S := S + (SNewLine + 'Based on Inno Setup' + SNewLine);
  S := S + ('Copyright (C) 1997-2025 Jordan Russell' + SNewLine +
    'Portions Copyright (C) 2000-2025 Martijn Laan' + SNewLine +
    'All rights reserved.' + SNewLine2 +
    'Inno Setup home page:' + SNewLine +
    'https://www.innosetup.com/' + SNewLine2 +
    'RemObjects Pascal Script home page:' + SNewLine +
    'https://www.remobjects.com/ps' + SNewLine2 +
    'Refer to LICENSE.TXT for conditions of distribution and use.');
  MsgBox(S, 'About ' + FCompilerVersion.Title, mbInformation, MB_OK);
end;

procedure TMainForm.WMStartCommandLineCompile(var Message: TMessage);
var
  Code: Integer;
begin
  UpdateStatusPanelHeight(ClientHeight);
  Code := 0;
  try
    try
      CompileFile(CommandLineFilename, True);
    except
      Code := 2;
      Application.HandleException(Self);
    end;
  finally
    Halt(Code);
  end;
end;

procedure TMainForm.WMStartCommandLineWizard(var Message: TMessage);
var
  Code: Integer;
begin
  Code := 0;
  try
    try
      NewMainFileUsingWizard;
    except
      Code := 2;
      Application.HandleException(Self);
    end;
  finally
    Halt(Code);
  end;
end;

procedure TMainForm.WMStartNormally(var Message: TMessage);

  procedure ShowStartupForm;
  var
    StartupForm: TStartupForm;
    Ini: TConfigIniFile;
  begin
    ReadMRUMainFilesList;
    StartupForm := TStartupForm.Create(Application);
    try
      StartupForm.MRUFilesList := FMRUMainFilesList;
      StartupForm.StartupCheck.Checked := not FOptions.ShowStartupForm;
      if StartupForm.ShowModal = mrOK then begin
        if FOptions.ShowStartupForm <> not StartupForm.StartupCheck.Checked then begin
          FOptions.ShowStartupForm := not StartupForm.StartupCheck.Checked;
          Ini := TConfigIniFile.Create;
          try
            Ini.WriteBool('Options', 'ShowStartupForm', FOptions.ShowStartupForm);
          finally
            Ini.Free;
          end;
        end;
        case StartupForm.Result of
          srEmpty:
            FNewMainFileClick(Self);
          srWizard:
            FNewMainFileUserWizardClick(Self);
          srOpenFile:
            if ConfirmCloseFile(True) then
              OpenMRUMainFile(StartupForm.ResultMainFileName);
          srOpenDialog:
            ShowOpenMainFileDialog(False);
          srOpenDialogExamples:
            ShowOpenMainFileDialog(True);
        end;
      end;
    finally
      StartupForm.Free;
    end;
  end;

begin
  if CommandLineFilename = '' then begin
    if FOptions.ShowStartupForm then
      ShowStartupForm;
  end else
    OpenFile(FMainMemo, CommandLineFilename, False);
end;

procedure TMainForm.WMSysColorChange(var Message: TMessage);
begin
  inherited;
  for var Memo in FMemos do
    Memo.SysColorChange(Message);
end;

procedure TMainForm.UpdateReopenTabMenu(const Menu: TMenuItem);
begin
  Menu.Clear;
  for var I := 0 to FHiddenFiles.Count-1 do begin
    var MenuItem := TMenuItem.Create(Menu);
    MenuItem.Caption := '&' + IntToStr((I+1) mod 10) + ' ' + DoubleAmp(PathExtractName(FHiddenFiles[I]));
    MenuItem.Tag := I;
    MenuItem.OnClick := ReopenTabClick;
    Menu.Add(MenuItem);
  end;
end;

procedure TMainForm.MemosTabSetPopupMenuClick(Sender: TObject);
begin
  { Main and preprocessor memos can't be hidden }
  VCloseCurrentTab2.Enabled := (FActiveMemo <> FMainMemo) and (FActiveMemo <> FPreprocessorOutputMemo);

  VReopenTab2.Visible := FHiddenFiles.Count > 0;
  if VReopenTab2.Visible then
    UpdateReopenTabMenu(VReopenTab2);
  VReopenTabs2.Visible := VReopenTab2.Visible;

  ApplyMenuBitmaps(Sender as TMenuItem)
end;

procedure TMainForm.MemosTabSetClick(Sender: TObject);
begin
  if FIgnoreTabSetClick then
    Exit;

  var NewActiveMemo := TabIndexToMemo(MemosTabSet.TabIndex, MemosTabSet.Tabs.Count-1);
  if NewActiveMemo <> FActiveMemo then begin
    { Avoiding flicker by showing new before hiding old }
    NewActiveMemo.Visible := True;
    var OldActiveMemo := FActiveMemo;
    FActiveMemo := NewActiveMemo;
    ActiveControl := NewActiveMemo;
    OldActiveMemo.CancelAutoCompleteAndCallTip;
    OldActiveMemo.Visible := False;

    UpdateSaveMenuItemAndButton;
    UpdateRunMenu;
    UpdateCaretPosPanelAndBackNavStack;
    UpdateEditModePanel;
    UpdateModifiedPanel;
  end;
end;

procedure TMainForm.MemosTabSetOnCloseButtonClick(Sender: TObject; Index: Integer);
begin
  CloseTab(Index);
end;

procedure TMainForm.InitializeFindText(Dlg: TFindDialog);
var
  S: String;
begin
  S := FActiveMemo.MainSelText;
  if (S <> '') and (Pos(#13, S) = 0) and (Pos(#10, S) = 0) then
    Dlg.FindText := S
  else
    Dlg.FindText := FLastFindText;
end;

const
  OldFindReplaceWndProcProp = 'OldFindReplaceWndProc';

function FindReplaceWndProc(Wnd: HWND; Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT; stdcall;

  function CallDefWndProc: LRESULT;
  begin
    Result := CallWindowProc(Pointer(GetProp(Wnd, OldFindReplaceWndProcProp)), Wnd,
      Msg, WParam, LParam);
  end;

begin
  case Msg of
    WM_MENUCHAR:
      if LoWord(wParam) = VK_RETURN then begin
        var hwndCtl := GetDlgItem(Wnd, idOk);
        if (hWndCtl <> 0) and IsWindowEnabled(hWndCtl) then
          PostMessage(Wnd, WM_COMMAND, MakeWParam(idOk, BN_CLICKED), Windows.LPARAM(hWndCtl));
      end;
    WM_NCDESTROY:
      begin
        Result := CallDefWndProc;
        RemoveProp(Wnd, OldFindReplaceWndProcProp);
        Exit;
      end;
   end;
   Result := CallDefWndProc;
end;

procedure ExecuteFindDialogAllowingAltEnter(const FindDialog: TFindDialog);
begin
  var DoHook := FindDialog.Handle = 0;
  FindDialog.Execute;
  if DoHook then begin
    SetProp(FindDialog.Handle, OldFindReplaceWndProcProp, GetWindowLong(FindDialog.Handle, GWL_WNDPROC));
    SetWindowLong(FindDialog.Handle, GWL_WNDPROC, IntPtr(@FindReplaceWndProc));
  end;
end;

procedure TMainForm.EFindClick(Sender: TObject);
begin
  ReplaceDialog.CloseDialog;
  if FindDialog.Handle = 0 then
    InitializeFindText(FindDialog);
  if (Sender = EFind) or (Sender = EFindNext) then
    FindDialog.Options := FindDialog.Options + [frDown]
  else
    FindDialog.Options := FindDialog.Options - [frDown];
  ExecuteFindDialogAllowingAltEnter(FindDialog);
end;

procedure TMainForm.EFindInFilesClick(Sender: TObject);
begin
  InitializeFindText(FindInFilesDialog);
  FindInFilesDialog.Execute;
end;

procedure TMainForm.EFindNextOrPreviousClick(Sender: TObject);
begin
  if FLastFindText = '' then
    EFindClick(Sender)
  else begin
    if Sender = EFindNext then
      FLastFindOptions := FLastFindOptions + [frDown]
    else
      FLastFindOptions := FLastFindOptions - [frDown];
    FLastFindRegEx := FOptions.FindRegEx;
    if not TestLastFindOptions then
      Exit;
    FindNext(False);
  end;
end;

procedure TMainForm.FindNext(const ReverseDirection: Boolean);
var
  StartPos, EndPos: Integer;
  Range: TScintRange;
begin
  var Down := frDown in FLastFindOptions;
  if ReverseDirection then
    Down := not Down;
  if Down then begin
    StartPos := FActiveMemo.Selection.EndPos;
    EndPos := FActiveMemo.RawTextLength;
  end
  else begin
    StartPos := FActiveMemo.Selection.StartPos;
    EndPos := 0;
  end;
  if FActiveMemo.FindText(StartPos, EndPos, FLastFindText,
     FindOptionsToSearchOptions(FLastFindOptions, FLastFindRegEx), Range) then
    FActiveMemo.SelectAndEnsureVisible(Range)
  else
    MsgBoxFmt('Cannot find "%s"', [FLastFindText], SCompilerFormCaption,
      mbInformation, MB_OK);
end;

function TMainForm.StoreAndTestLastFindOptions(Sender: TObject): Boolean;
begin
  if Sender is TFindDialog then begin
    with Sender as TFindDialog do begin
      FLastFindOptions := Options;
      FLastFindText := FindText;
    end;
  end else begin
    with Sender as TReplaceDialog do begin
      FLastFindOptions := Options;
      FLastFindText := FindText;
    end;
  end;
  FLastFindRegEx := FOptions.FindRegEx;

  Result := TestLastFindOptions;
end;

function TMainForm.TestLastFindOptions;
begin
  if FLastFindRegEx then begin
    Result := FActiveMemo.TestRegularExpression(FLastFindText);
    if not Result then
      MsgBoxFmt('Invalid regular expression "%s"', [FLastFindText], SCompilerFormCaption,
        mbError, MB_OK);
  end else
    Result := True;
end;

procedure TMainForm.FindDialogFind(Sender: TObject);
begin
  { This event handler is shared between FindDialog & ReplaceDialog }

  if not StoreAndTestLastFindOptions(Sender) then
    Exit;

  if GetKeyState(VK_MENU) < 0 then begin
    { Alt+Enter was used to close the dialog }
    (Sender as TFindDialog).CloseDialog;
    ESelectAllFindMatchesClick(Self); { Uses the copy made above }
  end else
    FindNext(GetKeyState(VK_SHIFT) < 0);
end;

procedure TMainForm.FindInFilesDialogFind(Sender: TObject);
begin
  if not StoreAndTestLastFindOptions(Sender) then
    Exit;

  FindResultsList.Clear;
  SendMessage(FindResultsList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
  FFindResults.Clear;

  var Hits := 0;
  var Files := 0;

  for var Memo in FFileMemos do begin
    if Memo.Used then begin
      var StartPos := 0;
      var EndPos := Memo.RawTextLength;
      var FileHits := 0;
      var Range: TScintRange;
      while (StartPos < EndPos) and
            Memo.FindText(StartPos, EndPos, FLastFindText,
              FindOptionsToSearchOptions(FLastFindOptions, FLastFindRegEx), Range) do begin
        var Line := Memo.GetLineFromPosition(Range.StartPos);
        var Prefix := Format('  Line %d: ', [Line+1]);
        var FindResult := TFindResult.Create;
        FindResult.Filename := Memo.Filename;
        FindResult.Line := Line;
        FindResult.LineStartPos := Memo.GetPositionFromLine(Line);
        FindResult.Range := Range;
        FindResult.PrefixStringLength := Length(Prefix);
        FFindResults.Add(FindResult);
        FindResultsList.Items.AddObject(Prefix + Memo.Lines[Line], FindResult);
        Inc(FileHits);
        StartPos := Range.EndPos;
      end;
      Inc(Files);
      if FileHits > 0 then begin
        Inc(Hits, FileHits);
        FindResultsList.Items.Insert(FindResultsList.Count-FileHits, Format('%s (%d hits):', [Memo.Filename, FileHits]));
      end;
    end;
  end;

  FindResultsList.Items.Insert(0, Format('Find "%s" (%d hits in %d files)', [FindInFilesDialog.FindText, Hits, Files]));

  FindInFilesDialog.CloseDialog;

  OutputTabSet.TabIndex := tiFindResults;
  SetStatusPanelVisible(True);
end;

function TMainForm.FindSetupDirectiveValue(const DirectiveName,
  DefaultValue: String): String;
begin
  Result := DefaultValue;

  var Memo := FMainMemo; { This function only searches the main file }
  var StartPos := 0;
  var EndPos := Memo.RawTextLength;
  var Range: TScintRange;

  { We rely on the styler to identify [Setup] section lines, but we
    may be searching into areas that haven't been styled yet }
  Memo.StyleNeeded(EndPos);

  while (StartPos < EndPos) and
        Memo.FindText(StartPos, EndPos, DirectiveName, [sfoWholeWord], Range) do begin
    var Line := Memo.GetLineFromPosition(Range.StartPos);
    if FMemosStyler.GetSectionFromLineState(Memo.Lines.State[Line]) = scSetup then begin
      var LineValue := Memo.Lines[Line].Trim; { LineValue can't be empty }
      if LineValue[1] <> ';' then begin
        var LineParts := LineValue.Split(['=']);
        if (Length(LineParts) = 2) and SameText(LineParts[0].Trim, DirectiveName) then begin
          Result := LineParts[1].Trim;
          { If Result is surrounded in quotes, remove them, just like TSetupCompiler.SeparateDirective }
          if (Length(Result) >= 2) and
             (Result[1] = '"') and (Result[Length(Result)] = '"') then
            Result := Copy(Result, 2, Length(Result)-2);
          Exit; { Compiler doesn't allow a directive to be specified twice so we can exit now }
        end;
      end;
    end;
    StartPos := Range.EndPos;
  end;
end;

function TMainForm.FindSetupDirectiveValue(const DirectiveName: String;
  DefaultValue: Boolean): Boolean;
begin
  var Value := FindSetupDirectiveValue(DirectiveName, IfThen(DefaultValue, '1', '0'));
  if not TryStrToBoolean(Value, Result) then
    Result := DefaultValue;
end;

procedure TMainForm.EReplaceClick(Sender: TObject);
begin
  FindDialog.CloseDialog;
  if ReplaceDialog.Handle = 0 then begin
    InitializeFindText(ReplaceDialog);
    ReplaceDialog.ReplaceText := FLastReplaceText;
  end;
  ExecuteFindDialogAllowingAltEnter(ReplaceDialog);
end;

procedure TMainForm.ReplaceDialogReplace(Sender: TObject);
begin
  if not StoreAndTestLastFindOptions(Sender) then
    Exit;

  FLastReplaceText := ReplaceDialog.ReplaceText;
  var ReplaceMode := RegExToReplaceMode(FLastFindRegEx);

  if frReplaceAll in FLastFindOptions then begin
    var ReplaceCount := 0;
    FActiveMemo.BeginUndoAction;
    try
      var Pos := 0;
      var Range: TScintRange;
      while FActiveMemo.FindText(Pos, FActiveMemo.RawTextLength, FLastFindText,
         FindOptionsToSearchOptions(FLastFindOptions, FLastFindRegEx), Range) do begin
        var NewRange := FActiveMemo.ReplaceTextRange(Range.StartPos, Range.EndPos, FLastReplaceText, ReplaceMode);
        Pos := NewRange.EndPos;
        Inc(ReplaceCount);
      end;
    finally
      FActiveMemo.EndUndoAction;
    end;
    if ReplaceCount = 0 then
      MsgBoxFmt('Cannot find "%s"', [FLastFindText], SCompilerFormCaption,
        mbInformation, MB_OK)
    else
      MsgBoxFmt('%d occurrence(s) replaced.', [ReplaceCount], SCompilerFormCaption,
        mbInformation, MB_OK);
  end
  else begin
    if FActiveMemo.MainSelTextEquals(FLastFindText, FindOptionsToSearchOptions(frMatchCase in FLastFindOptions, FLastFindRegEx)) then begin
      { Note: the MainSelTextEquals above performs a search so the replacement
        below is safe even if the user just enabled regex }
      FActiveMemo.ReplaceMainSelText(FLastReplaceText, ReplaceMode);
    end;
    FindNext(GetKeyState(VK_SHIFT) < 0);
  end;
end;

procedure TMainForm.EFindRegExClick(Sender: TObject);
begin
  { If EFindRegEx uses Alt+R as the shortcut just like VSCode then also handle it like VSCode:
    when the memo does not have the focus open the Run menu (also Alt+R) instead }
  if not FActiveMemo.Focused and (EFindRegEx.ShortCut = ShortCut(Ord('R'), [ssAlt])) then
    SendMessage(Handle, WM_SYSCOMMAND, SC_KEYMENU, Ord('r'))
  else begin
    FOptions.FindRegEx := not FOptions.FindRegEx;
    UpdateFindRegExPanel;
    var Ini := TConfigIniFile.Create;
    try
      Ini.WriteBool('Options', 'FindRegEx', FOptions.FindRegEx);
    finally
      Ini.Free;
    end;
  end;
end;

procedure TMainForm.EFoldOrUnfoldLineClick(Sender: TObject);
begin
  FActiveMemo.FoldLine(FActiveMemo.CaretLine, Sender = EFoldLine);
end;

procedure TMainForm.UpdateStatusPanelHeight(H: Integer);
var
  MinHeight, MaxHeight: Integer;
begin
  MinHeight := (3 * DebugOutputList.ItemHeight + ToCurrentPPI(4)) + OutputTabSet.Height;
  MaxHeight := BodyPanel.ClientHeight - ToCurrentPPI(48) - SplitPanel.Height;
  if H > MaxHeight then H := MaxHeight;
  if H < MinHeight then H := MinHeight;
  StatusPanel.Height := H;
end;

procedure TMainForm.UpdateOccurrenceIndicators(const AMemo: TIDEScintEdit);

  procedure FindTextAndAddRanges(const AMemo: TIDEScintEdit;
    const TextToFind: TScintRawString; const Options: TScintFindOptions;
    const Selections, IndicatorRanges: TScintRangeList);
  begin
    if TScintEdit.RawStringIsBlank(TextToFind) then
      Exit;

    var StartPos := 0;
    var EndPos := AMemo.RawTextLength;
    var FoundRange: TScintRange;

    while (StartPos < EndPos) and
          AMemo.FindRawText(StartPos, EndPos, TextToFind, Options, FoundRange) do begin
      StartPos := FoundRange.EndPos;

      { Don't add indicators on lines which have a line marker }
      var Line := AMemo.GetLineFromPosition(FoundRange.StartPos);
      var Markers := AMemo.GetMarkers(Line);
      if Markers * [mlmError, mlmBreakpointBad, mlmStep] <> [] then
        Continue;

      { Add indicator while making sure it does not overlap any regular selection
        styling for either the main selection or any additional selection. Does
        not account for an indicator overlapping more than 1 selection. }
      var OverlappingSelection: TScintRange;
      if Selections.Overlaps(FoundRange, OverlappingSelection) then begin
        if FoundRange.StartPos < OverlappingSelection.StartPos then
          IndicatorRanges.Add(TScintRange.Create(FoundRange.StartPos, OverlappingSelection.StartPos));
        if FoundRange.EndPos > OverlappingSelection.EndPos then
          IndicatorRanges.Add(TScintRange.Create(OverlappingSelection.EndPos, FoundRange.EndPos));
      end else
        IndicatorRanges.Add(FoundRange);
    end;
  end;

begin
  { Add occurrence indicators for the word at cursor if there's any and the
    main selection is within this word. On top of those add occurrence indicators
    for the main selected text if there's any. Don't do anything if the main
    selection is not single line. All of these things are just like VSCode. }

  var MainSelection: TScintRange;
  var MainSelNotEmpty := AMemo.SelNotEmpty(MainSelection);
  var MainSelSingleLine := AMemo.GetLineFromPosition(MainSelection.StartPos) =
                           AMemo.GetLineFromPosition(MainSelection.EndPos);

  var IndicatorRanges: TScintRangeList := nil;
  var Selections: TScintRangeList := nil;
  try
    IndicatorRanges := TScintRangeList.Create;
    Selections := TScintRangeList.Create;

    if FOptions.HighlightWordAtCursorOccurrences and (AMemo.CaretVirtualSpace = 0) and MainSelSingleLine then begin
      var Word := AMemo.WordAtCaretRange;
      if (Word.StartPos <> Word.EndPos) and MainSelection.Within(Word) then begin
        var TextToIndicate := AMemo.GetRawTextRange(Word.StartPos, Word.EndPos);
        AMemo.GetSelections(Selections); { Gets any additional selections as well }
        FindTextAndAddRanges(AMemo, TextToIndicate, [sfoMatchCase, sfoWholeWord], Selections, IndicatorRanges);
      end;
    end;
    AMemo.UpdateIndicators(IndicatorRanges, minWordAtCursorOccurrence);

    IndicatorRanges.Clear;
    if FOptions.HighlightSelTextOccurrences and MainSelNotEmpty and MainSelSingleLine then begin
      var TextToIndicate := AMemo.RawMainSelText;
      if Selections.Count = 0 then { If 0 then we didn't already call GetSelections above}
        AMemo.GetSelections(Selections);
      FindTextAndAddRanges(AMemo, TextToIndicate, [], Selections, IndicatorRanges);
    end;
    AMemo.UpdateIndicators(IndicatorRanges, minSelTextOccurrence);
  finally
    Selections.Free;
    IndicatorRanges.Free;
  end;
end;

procedure TMainForm.UpdateImages;
{ Should be called at startup and after DPI changes }
begin
  var WH := MulDiv(16, CurrentPPI, 96);
  var Images := ImagesModule.LightToolBarImageCollection;

  var Image := Images.GetSourceImage(Images.GetIndexByName('heart-filled'), WH, WH);
  UpdatePanelDonateImage.Picture.Graphic:= Image;
end;

procedure TMainForm.UpdateOutputTabSetListsItemHeightAndDebugTimeWidth;
{ Should be called at startup and after DPI changes }
begin
  CompilerOutputList.Canvas.Font.Assign(CompilerOutputList.Font);
  CompilerOutputList.ItemHeight := CompilerOutputList.Canvas.TextHeight('0') + 1;

  DebugOutputList.Canvas.Font.Assign(DebugOutputList.Font);
  FDebugLogListTimestampsWidth := DebugOutputList.Canvas.TextWidth(Format('[00%s00%s00%s000]   ', [FormatSettings.TimeSeparator, FormatSettings.TimeSeparator, FormatSettings.DecimalSeparator]));
  DebugOutputList.ItemHeight := DebugOutputList.Canvas.TextHeight('0') + 1;

  DebugCallStackList.Canvas.Font.Assign(DebugCallStackList.Font);
  DebugCallStackList.ItemHeight := DebugCallStackList.Canvas.TextHeight('0') + 1;

  FindResultsList.Canvas.Font.Assign(FindResultsList.Font);
  FindResultsList.ItemHeight := FindResultsList.Canvas.TextHeight('0') + 1;
end;

type
  TBitmapWithBits = class
    Handle: HBITMAP;
    pvBits: Pointer;
    destructor Destroy; override;
  end;

destructor TBitmapWithBits.Destroy;
begin
  if Handle <> 0 then
    DeleteObject(Handle);
  inherited;
end;

procedure TMainForm.UpdateMarginsAndAutoCompleteIcons;
{ Should be called at startup and after theme and DPI changes }

type
  TMarkerOrACBitmaps = TObjectDictionary<Integer, TBitmapWithBits>;

  procedure SwapRedBlue(const pvBits: PByte; Width, Height: Integer);
  begin
    var pvPixel := pvBits;
    var pvMax := pvBits + 4*Width*Height;
    while pvPixel < pvMax do begin
      var Tmp := PByte(pvPixel)^;
      PByte(pvPixel)^ := PByte(pvPixel + 2)^;
      PByte(pvPixel + 2)^ := Tmp;
      Inc(pvPixel, 4);
    end;
  end;

  procedure AddMarkerOrACBitmap(const MarkerOrACBitmaps: TMarkerOrACBitmaps; const DC: HDC; const BitmapInfo: TBitmapInfo;
    const MarkerNumberOrACType: Integer; const BkBrush: TBrush; const ImageList: TVirtualImageList; const ImageName: String);
  begin
    { Prepare a bitmap and select it }
    var pvBits: Pointer;
    var Bitmap := CreateDIBSection(DC, BitmapInfo, DIB_RGB_COLORS, pvBits, 0, 0);
    var OldBitmap := SelectObject(DC, Bitmap);

    { Fill the entire bitmap to avoid any alpha so we don't have to worry about
      whether will be premultiplied or not (it was in tests) when Scintilla wants
      it without premultiplication }
    var Width := BitmapInfo.bmiHeader.biWidth;
    var Height := Abs(BitmapInfo.bmiHeader.biHeight);
    var Rect := TRect.Create(0, 0, Width, Height);
    FillRect(DC, Rect, BkBrush.Handle);

    { Draw the image - the result will be in pvBits }
    if ImageList_Draw(ImageList.Handle, ImageList.GetIndexByName(ImageName), DC, 0, 0, ILD_TRANSPARENT) then begin
      SwapRedBlue(pvBits, Width, Height); { Change pvBits from BGRA to RGBA like Scintilla wants }
      var Bitmap2 := TBitmapWithBits.Create;
      Bitmap2.Handle := Bitmap;
      Bitmap2.pvBits := pvBits;
      MarkerOrACBitmaps.Add(MarkerNumberOrACType, Bitmap2);
    end else begin
      SelectObject(DC, OldBitmap);
      DeleteObject(Bitmap);
    end;
  end;

type
  TMarkerNumberOrACType = TPair<Integer, String>;

  function NNT(const MarkerNumberOrACType: Integer; const Name: String): TMarkerNumberOrACType;
  begin
    Result := TMarkerNumberOrACType.Create(MarkerNumberOrACType, Name); { This is a record so no need to free }
  end;

begin
  var ImageList := ThemedMarkersAndACVirtualImageList;

  var DC := CreateCompatibleDC(0);
  if DC <> 0 then begin
    try
      var MarkerBitmaps: TMarkerOrACBitmaps := nil;
      var MarkerBkBrush: TBrush := nil;
      var AutoCompleteBitmaps: TMarkerOrACBitmaps := nil;
      var AutoCompleteBkBrush: TBrush := nil;
      try
        var BitmapInfo := CreateBitmapInfo(ImageList.Width, -ImageList.Height, 32); { This is a record so no need to free }

        MarkerBitmaps := TMarkerOrACBitmaps.Create([doOwnsValues]);
        MarkerBkBrush := TBrush.Create;
        MarkerBkBrush.Color := FTheme.Colors[tcMarginBack];

        var NamedMarkers := [
          NNT(mmiHasEntry, 'markers\debug-stop-filled'),
          NNT(mmiEntryProcessed, 'markers\debug-stop-filled_2'),
          NNT(mmiBreakpoint, 'markers\debug-breakpoint-filled'),
          NNT(mmiBreakpointBad, 'markers\debug-breakpoint-filled-cancel-2'),
          NNT(mmiBreakpointGood, 'markers\debug-breakpoint-filled-ok-2'),
          NNT(mmiStep, 'markers\symbol-arrow-right'),
          NNT(mmiBreakpointStep, 'markers\debug-breakpoint-filled-ok2-symbol-arrow-right'),
          NNT(SC_MARKNUM_FOLDER, 'markers\symbol-add'),
          NNT(SC_MARKNUM_FOLDEROPEN, 'markers\symbol-remove')];

        for var NamedMarker in NamedMarkers do
          AddMarkerOrAcBitmap(MarkerBitmaps, DC, BitmapInfo, NamedMarker.Key, MarkerBkBrush, ImageList, NamedMarker.Value);

        AutoCompleteBitmaps := TMarkerOrACBitmaps.Create([doOwnsValues]);
        AutoCompleteBkBrush := TBrush.Create;
        AutoCompleteBkBrush.Color := FTheme.Colors[tcIntelliBack];

        var NamedTypes := [
          NNT(awtSection, 'ac\structure-filled'),
          NNT(awtParameter, 'ac\xml-filled'),
          NNT(awtDirective, 'ac\xml-filled'),
          NNT(awtFlag, 'ac\values'),
          NNT(awtPreprocessorDirective, 'ac\symbol-hashtag'),
          NNT(awtConstant, 'ac\constant-filled_2'),
          NNT(awtScriptFunction, 'ac\method-filled'),
          NNT(awtScriptType, 'ac\types'),
          NNT(awtScriptVariable, 'ac\variables'),
          NNT(awtScriptConstant, 'ac\constant-filled'),
          NNT(awtScriptInterface, 'ac\interface-filled'),
          NNT(awtScriptProperty, 'ac\properties-filled'),
          NNT(awtScriptEvent, 'ac\event-filled'),
          NNT(awtScriptKeyword, 'ac\list'),
          NNT(awtScriptEnumValue, 'ac\constant-filled')];

        for var NamedType in NamedTypes do
          AddMarkerOrAcBitmap(AutoCompleteBitmaps, DC, BitmapInfo, NamedType.Key, AutoCompleteBkBrush, ImageList, NamedType.Value);

        for var Memo in FMemos do begin
          Memo.Call(SCI_RGBAIMAGESETWIDTH, ImageList.Width, 0);
          Memo.Call(SCI_RGBAIMAGESETHEIGHT, ImageList.Height, 0);
          for var MarkerBitmap in MarkerBitmaps do
            Memo.Call(SCI_MARKERDEFINERGBAIMAGE, MarkerBitmap.Key, LPARAM(MarkerBitmap.Value.pvBits));
          for var AutoCompleteBitmap in AutoCompleteBitmaps do
            Memo.Call(SCI_REGISTERRGBAIMAGE, AutoCompleteBitmap.Key, LPARAM(AutoCompleteBitmap.Value.pvBits));
        end;
      finally
        AutoCompleteBkBrush.Free;
        AutoCompleteBitmaps.Free;
        MarkerBkBrush.Free;
        MarkerBitmaps.Free;
      end;
    finally
      DeleteDC(DC);
    end;
  end;
end;

procedure TMainForm.UpdateMarginsAndSquigglyAndCaretWidths;
{ Update the width of our two margins. Note: the width of the line numbers
  margin is fully handled by TScintEdit. Should be called at startup and after
  DPI change. }
begin
  var IconMarkersWidth := ToCurrentPPI(18); { 3 pixel margin on both sides of the icon }
  var BaseChangeHistoryWidth := ToCurrentPPI(6); { 6 = 2 pixel bar with 2 pixel margin on both sides because: "SC_MARK_BAR ... takes ... 1/3 of the margin width" }
  var FolderMarkersWidth := ToCurrentPPI(14); { 1 pixel margin on boths side of the icon }
  var LeftBlankMarginWidth := ToCurrentPPI(2); { 2 pixel margin between gutter and the main text }
  var SquigglyWidth := ToCurrentPPI(100); { 100 = 1 pixel }
  var CaretWidth := ToCurrentPPI(2);
  var WhiteSpaceSize := CaretWidth;

  for var Memo in FMemos do
    Memo.UpdateWidthsAndSizes(IconMarkersWidth, BaseChangeHistoryWidth, FolderMarkersWidth,
      LeftBlankMarginWidth, 0, SquigglyWidth, CaretWidth, WhiteSpaceSize);
end;

procedure TMainForm.SplitPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and StatusPanel.Visible then begin
    UpdateStatusPanelHeight(BodyPanel.ClientToScreen(Point(0, 0)).Y -
      SplitPanel.ClientToScreen(Point(0, Y)).Y +
      BodyPanel.ClientHeight - (SplitPanel.Height div 2));
  end;
end;

procedure TMainForm.SimpleMenuClick(Sender: TObject);
begin
  ApplyMenuBitmaps(Sender as TMenuItem);
end;

procedure TMainForm.TMenuClick(Sender: TObject);
var
  MemoIsReadOnly: Boolean;
begin
  MemoIsReadOnly := FActiveMemo.ReadOnly;
  TGenerateGUID.Enabled := not MemoIsReadOnly;
  TMsgBoxDesigner.Enabled := not MemoIsReadOnly;
  TFilesDesigner.Enabled := not MemoIsReadOnly;
  TRegistryDesigner.Enabled := not MemoIsReadOnly;

  ApplyMenuBitmaps(Sender as TMenuItem);
end;

procedure TMainForm.TAddRemoveProgramsClick(Sender: TObject);
begin
  StartAddRemovePrograms;
end;

procedure TMainForm.TGenerateGUIDClick(Sender: TObject);
begin
  if MsgBox('The generated GUID will be inserted into the editor at the cursor position. Continue?',
     SCompilerFormCaption, mbConfirmation, MB_YESNO) = IDYES then
    FActiveMemo.MainSelText := GenerateGuid;
end;

procedure TMainForm.TMsgBoxDesignerClick(Sender: TObject);
begin
  if (FMemosStyler.GetSectionFromLineState(FActiveMemo.Lines.State[FActiveMemo.CaretLine]) <> scCode) and
     (MsgBox('The generated Pascal script will be inserted into the editor at the cursor position, but the cursor is not in the [Code] section. Continue anyway?',
      SCompilerFormCaption, mbConfirmation, MB_YESNO) = IDNO) then
    Exit;

  var MsgBoxForm := TMsgBoxDesignerForm.Create(Application);
  try
    if MsgBoxForm.ShowModal = mrOk then
      FActiveMemo.MainSelText := MsgBoxForm.GetText(FOptions.TabWidth, FOptions.UseTabCharacter);
  finally
    MsgBoxForm.Free;
  end;
end;

procedure TMainForm.TRegistryDesignerClick(Sender: TObject);
begin
  var RegistryDesignerForm := TRegistryDesignerForm.Create(Application);
  try
    var PrivilegesRequired := FindSetupDirectiveValue('PrivilegesRequired', 'admin');
    var PrivilegesRequiredOverridesAllowed := FindSetupDirectiveValue('PrivilegesRequiredOverridesAllowed', '');
    if PrivilegesRequiredOverridesAllowed = '' then begin
      if SameText(PrivilegesRequired, 'admin') then
        RegistryDesignerForm.PrivilegesRequired := prAdmin
      else
        RegistryDesignerForm.PrivilegesRequired := prLowest
    end else
      RegistryDesignerForm.PrivilegesRequired := prDynamic;
    if RegistryDesignerForm.ShowModal = mrOk then
    begin
      FActiveMemo.CaretColumn := 0;
      var Text := RegistryDesignerForm.Text;
      if FMemosStyler.GetSectionFromLineState(FActiveMemo.Lines.State[FActiveMemo.CaretLine]) <> scRegistry then
        Text := '[Registry]' + SNewLine + Text;
      FActiveMemo.MainSelText := Text;
    end;
  finally
    RegistryDesignerForm.Free;
  end;
end;

procedure TMainForm.TFilesDesignerClick(Sender: TObject);
begin
  var FilesDesignerForm := TFilesDesignerForm.Create(Application);
  try
    FilesDesignerForm.CreateAppDir := FindSetupDirectiveValue('CreateAppDir', True);
    if FilesDesignerForm.ShowModal = mrOk then begin
      FActiveMemo.CaretColumn := 0;
      var Text := FilesDesignerForm.Text;
      if FMemosStyler.GetSectionFromLineState(FActiveMemo.Lines.State[FActiveMemo.CaretLine]) <> scFiles then
        Text := '[Files]' + SNewLine + Text;
      FActiveMemo.MainSelText := Text;
    end;
  finally
    FilesDesignerForm.Free;
  end;
end;

procedure TMainForm.TSignToolsClick(Sender: TObject);
var
  SignToolsForm: TSignToolsForm;
  Ini: TConfigIniFile;
  I: Integer;
begin
  SignToolsForm := TSignToolsForm.Create(Application);
  try
    SignToolsForm.SignTools := FSignTools;

    if SignToolsForm.ShowModal <> mrOK then
      Exit;

    FSignTools.Assign(SignToolsForm.SignTools);

    { Save new options }
    Ini := TConfigIniFile.Create;
    try
      Ini.EraseSection('SignTools');
      for I := 0 to FSignTools.Count-1 do
        Ini.WriteString('SignTools', 'SignTool' + IntToStr(I), FSignTools[I]);
    finally
      Ini.Free;
    end;
  finally
    SignToolsForm.Free;
  end;
end;

procedure TMainForm.TOptionsClick(Sender: TObject);
var
  OptionsForm: TOptionsForm;
  Ini: TConfigIniFile;
  Memo: TIDEScintEdit;
begin
  OptionsForm := TOptionsForm.Create(Application);
  try
    OptionsForm.StartupCheck.Checked := FOptions.ShowStartupForm;
    OptionsForm.WizardCheck.Checked := FOptions.UseWizard;
    OptionsForm.AutosaveCheck.Checked := FOptions.Autosave;
    OptionsForm.BackupCheck.Checked := FOptions.MakeBackups;
    OptionsForm.FullPathCheck.Checked := FOptions.FullPathInTitleBar;
    OptionsForm.UndoAfterSaveCheck.Checked := FOptions.UndoAfterSave;
    OptionsForm.PauseOnDebuggerExceptionsCheck.Checked := FOptions.PauseOnDebuggerExceptions;
    OptionsForm.RunAsDifferentUserCheck.Checked := FOptions.RunAsDifferentUser;
    OptionsForm.AutoAutoCompleteCheck.Checked := FOptions.AutoAutoComplete;
    OptionsForm.UseSynHighCheck.Checked := FOptions.UseSyntaxHighlighting;
    OptionsForm.ColorizeCompilerOutputCheck.Checked := FOptions.ColorizeCompilerOutput;
    OptionsForm.UnderlineErrorsCheck.Checked := FOptions.UnderlineErrors;
    OptionsForm.CursorPastEOLCheck.Checked := FOptions.CursorPastEOL;
    OptionsForm.TabWidthEdit.Text := IntToStr(FOptions.TabWidth);
    OptionsForm.UseTabCharacterCheck.Checked := FOptions.UseTabCharacter;
    OptionsForm.ShowWhiteSpaceCheck.Checked := FOptions.ShowWhiteSpace;
    OptionsForm.UseFoldingCheck.Checked := FOptions.UseFolding;
    OptionsForm.AutoIndentCheck.Checked := FOptions.AutoIndent;
    OptionsForm.IndentationGuidesCheck.Checked := FOptions.IndentationGuides;
    OptionsForm.GutterLineNumbersCheck.Checked := FOptions.GutterLineNumbers;
    OptionsForm.ShowPreprocessorOutputCheck.Checked := FOptions.ShowPreprocessorOutput;
    OptionsForm.OpenIncludedFilesCheck.Checked := FOptions.OpenIncludedFiles;
    OptionsForm.KeyMappingComboBox.ItemIndex := Ord(FOptions.KeyMappingType);
    OptionsForm.MemoKeyMappingComboBox.ItemIndex := Ord(FOptions.MemoKeyMappingType);
    OptionsForm.ThemeComboBox.ItemIndex := Ord(FOptions.ThemeType);
    OptionsForm.FontPanel.Font.Assign(FMainMemo.Font);
    OptionsForm.FontPanel.ParentBackground := False;
    OptionsForm.FontPanel.Color := FMainMemo.Color;
    OptionsForm.HighlightWordAtCursorOccurrencesCheck.Checked := FOptions.HighlightWordAtCursorOccurrences;
    OptionsForm.HighlightSelTextOccurrencesCheck.Checked := FOptions.HighlightSelTextOccurrences;

    if OptionsForm.ShowModal <> mrOK then
      Exit;

    FOptions.ShowStartupForm := OptionsForm.StartupCheck.Checked;
    FOptions.UseWizard := OptionsForm.WizardCheck.Checked;
    FOptions.Autosave := OptionsForm.AutosaveCheck.Checked;
    FOptions.MakeBackups := OptionsForm.BackupCheck.Checked;
    FOptions.FullPathInTitleBar := OptionsForm.FullPathCheck.Checked;
    FOptions.UndoAfterSave := OptionsForm.UndoAfterSaveCheck.Checked;
    FOptions.PauseOnDebuggerExceptions := OptionsForm.PauseOnDebuggerExceptionsCheck.Checked;
    FOptions.RunAsDifferentUser := OptionsForm.RunAsDifferentUserCheck.Checked;
    FOptions.AutoAutoComplete := OptionsForm.AutoAutoCompleteCheck.Checked;
    FOptions.UseSyntaxHighlighting := OptionsForm.UseSynHighCheck.Checked;
    FOptions.ColorizeCompilerOutput := OptionsForm.ColorizeCompilerOutputCheck.Checked;
    FOptions.UnderlineErrors := OptionsForm.UnderlineErrorsCheck.Checked;
    FOptions.CursorPastEOL := OptionsForm.CursorPastEOLCheck.Checked;
    FOptions.TabWidth := StrToInt(OptionsForm.TabWidthEdit.Text);
    FOptions.UseTabCharacter := OptionsForm.UseTabCharacterCheck.Checked;
    FOptions.ShowWhiteSpace := OptionsForm.ShowWhiteSpaceCheck.Checked;
    FOptions.UseFolding := OptionsForm.UseFoldingCheck.Checked;
    FOptions.AutoIndent := OptionsForm.AutoIndentCheck.Checked;
    FOptions.IndentationGuides := OptionsForm.IndentationGuidesCheck.Checked;
    FOptions.GutterLineNumbers := OptionsForm.GutterLineNumbersCheck.Checked;
    FOptions.ShowPreprocessorOutput := OptionsForm.ShowPreprocessorOutputCheck.Checked;
    FOptions.OpenIncludedFiles := OptionsForm.OpenIncludedFilesCheck.Checked;
    FOptions.KeyMappingType := TKeyMappingType(OptionsForm.KeyMappingComboBox.ItemIndex);
    FOptions.MemoKeyMappingType := TIDEScintKeyMappingType(OptionsForm.MemoKeyMappingComboBox.ItemIndex);
    FOptions.ThemeType := TThemeType(OptionsForm.ThemeComboBox.ItemIndex);
    FOptions.HighlightWordAtCursorOccurrences := OptionsForm.HighlightWordAtCursorOccurrencesCheck.Checked;
    FOptions.HighlightSelTextOccurrences := OptionsForm.HighlightSelTextOccurrencesCheck.Checked;

    UpdateCaption;
    UpdatePreprocMemos;
    InvalidateStatusPanel(spHiddenFilesCount);
    for Memo in FMemos do begin
      { Move caret to start of line to ensure it doesn't end up in the middle
        of a double-byte character if the code page changes from SBCS to DBCS }
      Memo.CaretLine := Memo.CaretLine;
      Memo.Font.Assign(OptionsForm.FontPanel.Font);
    end;
    SyncEditorOptions;
    UpdateMarginsAndSquigglyAndCaretWidths;
    UpdateNewMainFileButtons;
    UpdateOccurrenceIndicators(FActiveMemo);
    UpdateKeyMapping;
    UpdateTheme;

    { Save new options }
    Ini := TConfigIniFile.Create;
    try
      Ini.WriteBool('Options', 'ShowStartupForm', FOptions.ShowStartupForm);
      Ini.WriteBool('Options', 'UseWizard', FOptions.UseWizard);
      Ini.WriteBool('Options', 'Autosave', FOptions.Autosave);
      Ini.WriteBool('Options', 'MakeBackups', FOptions.MakeBackups);
      Ini.WriteBool('Options', 'FullPathInTitleBar', FOptions.FullPathInTitleBar);
      Ini.WriteBool('Options', 'UndoAfterSave', FOptions.UndoAfterSave);
      Ini.WriteBool('Options', 'PauseOnDebuggerExceptions', FOptions.PauseOnDebuggerExceptions);
      Ini.WriteBool('Options', 'RunAsDifferentUser', FOptions.RunAsDifferentUser);
      Ini.WriteBool('Options', 'AutoComplete', FOptions.AutoAutoComplete);
      Ini.WriteBool('Options', 'AutoCallTips', FOptions.AutoCallTips);
      Ini.WriteBool('Options', 'UseSynHigh', FOptions.UseSyntaxHighlighting);
      Ini.WriteBool('Options', 'ColorizeCompilerOutput', FOptions.ColorizeCompilerOutput);
      Ini.WriteBool('Options', 'UnderlineErrors', FOptions.UnderlineErrors);
      Ini.WriteBool('Options', 'HighlightWordAtCursorOccurrences', FOptions.HighlightWordAtCursorOccurrences);
      Ini.WriteBool('Options', 'HighlightSelTextOccurrences', FOptions.HighlightSelTextOccurrences);
      Ini.WriteBool('Options', 'EditorCursorPastEOL', FOptions.CursorPastEOL);
      Ini.WriteInteger('Options', 'TabWidth', FOptions.TabWidth);
      Ini.WriteBool('Options', 'UseTabCharacter', FOptions.UseTabCharacter);
      Ini.WriteBool('Options', 'ShowWhiteSpace', FOptions.ShowWhiteSpace);
      Ini.WriteBool('Options', 'UseFolding', FOptions.UseFolding);
      Ini.WriteBool('Options', 'AutoIndent', FOptions.AutoIndent);
      Ini.WriteBool('Options', 'IndentationGuides', FOptions.IndentationGuides);
      Ini.WriteBool('Options', 'GutterLineNumbers', FOptions.GutterLineNumbers);
      Ini.WriteBool('Options', 'ShowPreprocessorOutput', FOptions.ShowPreprocessorOutput);
      Ini.WriteBool('Options', 'OpenIncludedFiles', FOptions.OpenIncludedFiles);
      Ini.WriteInteger('Options', 'KeyMappingType', Ord(FOptions.KeyMappingType));
      Ini.WriteInteger('Options', 'MemoKeyMappingType', Ord(FOptions.MemoKeyMappingType));
      Ini.WriteInteger('Options', 'ThemeType', Ord(FOptions.ThemeType)); { Also see Destroy }
      Ini.WriteString('Options', 'EditorFontName', FMainMemo.Font.Name);
      Ini.WriteInteger('Options', 'EditorFontSize', FMainMemo.Font.Size);
      Ini.WriteInteger('Options', 'EditorFontCharset', FMainMemo.Font.Charset);
    finally
      Ini.Free;
    end;
  finally
    OptionsForm.Free;
  end;
end;

{ Also see TabIndexToMemoIndex }
function TMainForm.MemoToTabIndex(const AMemo: TIDEScintEdit): Integer;
begin
  if AMemo = FMainMemo then
    Result := 0 { First tab displays the main memo  }
  else if AMemo = FPreprocessorOutputMemo then begin
    if not FPreprocessorOutputMemo.Used then
      raise Exception.Create('not FPreprocessorOutputMemo.Used');
    Result := MemosTabSet.Tabs.Count-1 { Last tab displays the preprocessor output memo }
  end else begin
    Result := FFileMemos.IndexOf(AMemo as TIDEScintFileEdit); { Other tabs display include files which start second tab }

   { Filter memos explicitly hidden by the user }
    for var MemoIndex := Result-1 downto 0 do
      if FHiddenFiles.IndexOf(FFileMemos[MemoIndex].Filename) <> -1 then
        Dec(Result);
  end;
end;

{ Also see MemoToTabIndex }
function TMainForm.TabIndexToMemo(const ATabIndex, AMaxTabIndex: Integer): TIDEScintEdit;
begin
  if ATabIndex = 0 then
    Result := FMemos[0] { First tab displays the main memo which is FMemos[0] }
  else if FPreprocessorOutputMemo.Used and (ATabIndex = AMaxTabIndex) then
    Result := FMemos[1] { Last tab displays the preprocessor output memo which is FMemos[1] }
  else begin
    { Only count memos not explicitly hidden by the user }
    var TabIndex := 0;
    for var MemoIndex := FirstIncludedFilesMemoIndex to FFileMemos.Count-1 do begin
      if FHiddenFiles.IndexOf(FFileMemos[MemoIndex].Filename) = -1 then begin
        Inc(TabIndex);
        if TabIndex = ATabIndex then begin
          Result := FMemos[MemoIndex + 1];   { Other tabs display include files which start at second tab but at FMemos[2] }
          Exit;
        end;
      end;
    end;

    raise Exception.Create('TabIndexToMemo failed');
  end;
end;

procedure TMainForm.MoveCaretAndActivateMemo(AMemo: TIDEScintEdit; const LineNumberOrPosition: Integer;
  const AlwaysResetColumnEvenIfOnRequestedLineAlready: Boolean; const IsPosition: Boolean;
  const PositionVirtualSpace: Integer);
var
  Pos: Integer;
begin
  { Reopen tab if needed }
  if AMemo is TIDEScintFileEdit then begin
    var FileName := (AMemo as TIDEScintFileEdit).Filename;
    var HiddenFileIndex := FHiddenFiles.IndexOf(Filename);
    if HiddenFileIndex <> -1 then begin
      ReopenTabOrTabs(HiddenFileIndex, False);
      { The above call to ReopenTabOrTabs will currently lead to a call to UpdateIncludedFilesMemos which
        sets up all the memos. Currently it will keep same memo for the reopened file but in case it no
        longer does at some point: look it up again }
      AMemo := nil;
      for var Memo in FFileMemos do begin
        if Memo.Used and (PathCompare(Memo.Filename, Filename) = 0) then begin
          AMemo := Memo;
          Break;
        end;
      end;
      if AMemo = nil then
        raise Exception.Create('AMemo MIA');
    end;
  end;

  { Move caret }
  if IsPosition then
    Pos := LineNumberOrPosition
  else if AlwaysResetColumnEvenIfOnRequestedLineAlready or (AMemo.CaretLine <> LineNumberOrPosition) then
    Pos := AMemo.GetPositionFromLine(LineNumberOrPosition)
  else
    Pos := AMemo.CaretPosition; { Not actually moving caret - it's already were we want it}

  { If the line is in a contracted section, expand it }
  AMemo.EnsureLineVisible(AMemo.GetLineFromPosition(Pos));

  { If the line isn't in view, scroll so that it's in the center }
  if not AMemo.IsPositionInViewVertically(Pos) then
    AMemo.TopLine := AMemo.GetVisibleLineFromDocLine(AMemo.GetLineFromPosition(Pos)) -
      (AMemo.LinesInWindow div 2);

  AMemo.CaretPosition := Pos;
  if IsPosition then
    AMemo.CaretVirtualSpace := PositionVirtualSpace;

  { Activate memo }
  MemosTabSet.TabIndex := MemoToTabIndex(AMemo); { This causes MemosTabSetClick to show the memo }
end;

procedure TMainForm.SetErrorLine(const AMemo: TIDEScintFileEdit; const ALine: Integer);
var
  OldLine: Integer;
begin
  if AMemo <> FErrorMemo then begin
    SetErrorLine(FErrorMemo, -1);
    FErrorMemo := AMemo;
  end;

  if FErrorMemo.ErrorLine <> ALine then begin
    OldLine := FErrorMemo.ErrorLine;
    FErrorMemo.ErrorLine := ALine;
    if OldLine >= 0 then
      UpdateLineMarkers(FErrorMemo, OldLine);
    if FErrorMemo.ErrorLine >= 0 then begin
      FErrorMemo.ErrorCaretPosition := FErrorMemo.CaretPosition;
      UpdateLineMarkers(FErrorMemo, FErrorMemo.ErrorLine);
    end;
  end;
end;

procedure TMainForm.SetStepLine(const AMemo: TIDEScintFileEdit; ALine: Integer);
var
  OldLine: Integer;
begin
  if AMemo <> FStepMemo then begin
    SetStepLine(FStepMemo, -1);
    FStepMemo := AMemo;
  end;

  if FStepMemo.StepLine <> ALine then begin
    OldLine := FStepMemo.StepLine;
    FStepMemo.StepLine := ALine;
    if OldLine >= 0 then
      UpdateLineMarkers(FStepMemo, OldLine);
    if FStepMemo.StepLine >= 0 then
      UpdateLineMarkers(FStepMemo, FStepMemo.StepLine);
  end;
end;

procedure TMainForm.HideError;
begin
  SetErrorLine(FErrorMemo, -1);
  if not FCompiling then
    StatusBar.Panels[spExtraStatus].Text := '';
end;

procedure TMainForm.RemoveMemoFromNav(const AMemo: TIDEScintEdit);
begin
  if FNavStacks.RemoveMemo(AMemo) then
    UpdateNavButtons;
  if FCurrentNavItem.Memo = AMemo then
    FCurrentNavItem.Invalidate;
end;

procedure TMainForm.RemoveMemoBadLinesFromNav(const AMemo: TIDEScintEdit);
begin
  if FNavStacks.RemoveMemoBadLines(AMemo) then
    UpdateNavButtons;
  { We do NOT update FCurrentNav here so it might point to a line that's
    deleted until next UpdateCaretPosPanelAndBackStack by UpdateMemoUI }
end;

procedure TMainForm.UpdateNavButtons;
begin
  ForwardNavButton.Enabled := FNavStacks.Forward.Count > 0;
  BackNavButton.Enabled := (FNavStacks.Back.Count > 0) or
                           ForwardNavButton.Enabled; { for the dropdown }
end;

procedure TMainForm.BackNavButtonClick(Sender: TObject);
begin
  { Delphi does not support BTNS_WHOLEDROPDOWN so we can't be like VS which
    can have a disabled back nav button with an enabled dropdown. To avoid
    always showing two dropdowns we keep the back button enabled when we need
    the dropdown. So we need to check for this. }
  if FNavStacks.Back.Count = 0 then begin
    Beep;
    Exit;
  end;

  FNavStacks.Forward.Add(FCurrentNavItem);
  var NewNavItem := FNavStacks.Back.ExtractAt(FNavStacks.Back.Count-1);
  UpdateNavButtons;
  FCurrentNavItem := NewNavItem; { Must be done *before* moving }
  MoveCaretAndActivateMemo(NewNavItem.Memo,
    NewNavItem.Memo.GetPositionFromLineColumn(NewNavItem.Line, NewNavItem.Column), False, True, NewNavItem.VirtualSpace);
end;

procedure TMainForm.ForwardNavButtonClick(Sender: TObject);
begin
  FNavStacks.Back.Add(FCurrentNavItem);
  var NewNavItem := FNavStacks.Forward.ExtractAt(FNavStacks.Forward.Count-1);
  UpdateNavButtons;
  FCurrentNavItem := NewNavItem; { Must be done *before* moving }
  MoveCaretAndActivateMemo(NewNavItem.Memo,
    NewNavItem.Memo.GetPositionFromLineColumn(NewNavItem.Line, NewNavItem.Column), False, True, NewNavItem.VirtualSpace);
end;

procedure TMainForm.WMAppCommand(var Message: TMessage);
begin
  var Command := GET_APPCOMMAND_LPARAM(Message.LParam);

  if Command = APPCOMMAND_BROWSER_BACKWARD then begin
    if BackNavButton.Enabled then
      BackNavButton.Click;
    Message.Result := 1;
  end else if Command = APPCOMMAND_BROWSER_FORWARD then begin
    if ForwardNavButton.Enabled then
      ForwardNavButton.Click;
    Message.Result := 1;
  end;
end;

procedure TMainForm.NavItemClick(Sender: TObject);
begin
  var MenuItem := Sender as TMenuItem;
  var Clicks := Abs(MenuItem.Tag);
  if Clicks > 0 then begin
    var ButtonToClick: TToolButton;
    if MenuItem.Tag > 0 then
      ButtonToClick := ForwardNavButton
    else
      ButtonToClick := BackNavButton;
    while Clicks > 0 do begin
      if not ButtonToClick.Enabled then
        raise Exception.Create('not ButtonToClick.Enabled');
      ButtonToClick.Click;
      Dec(Clicks);
    end;
  end;
end;

procedure TMainForm.NavPopupMenuClick(Sender: TObject);

  procedure AddNavItemToMenu(const NavItem: TIDEScintEditNavItem; const Checked: Boolean;
    const ClicksNeeded: Integer; const Menu: TMenuItem);
  begin
    if NavItem.Line >= NavItem.Memo.Lines.Count then
      raise Exception.Create('NavItem.Line >= NavItem.Memo.Lines.Count');
    var LineInfo :=  NavItem.Memo.Lines[NavItem.Line];
    if LineInfo.Trim = '' then
      LineInfo := Format('Line %d', [NavItem.Line+1]);

    var Caption: String;
    if MemosTabSet.Visible then
      Caption := Format('%s: %s', [MemosTabSet.Tabs[MemoToTabIndex(NavItem.Memo)], LineInfo])
    else
      Caption := LineInfo;

    var MenuItem := TMenuItem.Create(Menu);
    MenuItem.Caption := DoubleAmp(Caption);
    MenuItem.Checked := Checked;
    MenuItem.RadioItem := True;
    MenuItem.Tag := ClicksNeeded;
    MenuItem.OnClick := NavItemClick;
    Menu.Add(MenuItem);
  end;

begin
  var Menu := Sender as TMenuItem;

  Menu.Clear;

  { Setup dropdown. The result should end up being just like Visual Studio 2022
    which means from top to bottom:
    - Furthest (=oldest) forward item
    - ...
    - Closest (=next) forward item
    - Current position in the active memo, checked
    - Closest (=next) back item
    - ...
    - Furthest (=oldest) back item
    The Tag parameter should be set to the amount of clicks needed to get to
    the item, positive for forward and negative for back }

  for var I := 0 to FNavStacks.Forward.Count-1 do
    AddNavItemToMenu(FNavStacks.Forward[I], False, FNavStacks.Forward.Count-I, Menu);
  AddNavItemToMenu(FCurrentNavItem, True, 0, Menu);
  for var I := FNavStacks.Back.Count-1 downto 0 do
    AddNavItemToMenu(FNavStacks.Back[I], False, -(FNavStacks.Back.Count-I), Menu);
end;

procedure TMainForm.UpdateCaretPosPanelAndBackNavStack;
begin
  { Update panel }
  var Text := Format('%4d:%4d', [FActiveMemo.CaretLine + 1,
    FActiveMemo.CaretColumnExpandedForTabs + 1]);
  if FOptions.ShowCaretPosition then begin
    var CaretPos := FActiveMemo.CaretPosition;
    Text := Format('%d@%d+%d:%s', [FActiveMemo.GetStyleAtPosition(CaretPos), CaretPos, FActiveMemo.CaretVirtualSpace, Text]);
  end;
  StatusBar.Panels[spCaretPos].Text := Text;

  { Update NavStacks.Back if needed and remember new position }
  var NewNavItem := TIDEScintEditNavItem.Create(FActiveMemo); { This is a record so no need to free }
  if FCurrentNavItem.Valid and FNavStacks.AddNewBackForJump(FCurrentNavItem, NewNavItem) then
    UpdateNavButtons;
  FCurrentNavItem := NewNavItem;
end;

procedure TMainForm.UpdateEditModePanel;
const
  InsertText: array[Boolean] of String = ('Overwrite', 'Insert');
begin
  if FActiveMemo.ReadOnly then
    StatusBar.Panels[spEditMode].Text := 'Read only'
  else
    StatusBar.Panels[spEditMode].Text := InsertText[FActiveMemo.InsertMode];
end;

procedure TMainForm.UpdateFindRegExPanel;
const
  FindRegExText: array[Boolean] of String = ('', '.*');
begin
  StatusBar.Panels[spFindRegEx].Text := FindRegExText[FOptions.FindRegEx];
end;

procedure TMainForm.UpdateMemosTabSetVisibility;
begin
  MemosTabSet.Visible := FPreprocessorOutputMemo.Used or FFileMemos[FirstIncludedFilesMemoIndex].Used;
  if not MemosTabSet.Visible then
    MemosTabSet.TabIndex := 0; { For next time }
end;

procedure TMainForm.UpdateModifiedPanel;
begin
  if FActiveMemo.Modified then
    StatusBar.Panels[spModified].Text := 'Modified'
  else
    StatusBar.Panels[spModified].Text := '';
end;

procedure TMainForm.UpdatePreprocMemos;

  procedure UpdatePreprocessorOutputMemo(const NewTabs, NewHints: TStringList;
    const NewCloseButtons: TBoolList);
  begin
    if FOptions.ShowPreprocessorOutput and (FPreprocessorOutput <> '') and
       not SameStr(TrimRight(FMainMemo.Lines.Text), FPreprocessorOutput) then begin
      NewTabs.Add('Preprocessor Output');
      NewHints.Add('');
      NewCloseButtons.Add(False);
      FPreprocessorOutputMemo.ReadOnly := False;
      try
        FPreprocessorOutputMemo.Lines.Text := FPreprocessorOutput;
        FPreprocessorOutputMemo.ClearUndo;
      finally
        FPreprocessorOutputMemo.ReadOnly := True;
      end;
      FPreprocessorOutputMemo.Used := True;
    end else begin
      if FPreprocessorOutputMemo.Used then
        RemoveMemoFromNav(FPreprocessorOutputMemo);
      FPreprocessorOutputMemo.Used := False;
      FPreprocessorOutputMemo.Visible := False;
    end;
  end;

  procedure UpdateIncludedFilesMemos(const NewTabs, NewHints: TStringList;
    const NewCloseButtons: TBoolList);
  var
    IncludedFile: TIncludedFile;
    I: Integer;
  begin
    if FOptions.OpenIncludedFiles and (FIncludedFiles.Count > 0) then begin
      var NextMemoIndex := FirstIncludedFilesMemoIndex;
      var NextTabIndex := 1; { First tab displays the main memo  }
      for IncludedFile in FIncludedFiles do begin
        IncludedFile.Memo := FFileMemos[NextMemoIndex];
        try
          if not IncludedFile.Memo.Used or
            ((PathCompare(IncludedFile.Memo.Filename, IncludedFile.Filename) <> 0) or
              not IncludedFile.HasLastWriteTime or
              (CompareFileTime(IncludedFile.Memo.FileLastWriteTime, IncludedFile.LastWriteTime) <> 0)) then begin
            IncludedFile.Memo.Filename := IncludedFile.Filename;
            IncludedFile.Memo.CompilerFileIndex := IncludedFile.CompilerFileIndex;
            OpenFile(IncludedFile.Memo, IncludedFile.Filename, False); { Also updates FileLastWriteTime }
            IncludedFile.Memo.Used := True;
          end else begin
            { The memo assigned to the included file already has that file loaded
              and is up-to-date so no call to OpenFile is needed. However, it could be
              that CompilerFileIndex is not set yet. This happens if the initial
              load was from the history loaded by LoadKnownIncludedAndHiddenFiles
              and is followed by the user doing a compile. }
            if IncludedFile.Memo.CompilerFileIndex = UnknownCompilerFileIndex then
              IncludedFile.Memo.CompilerFileIndex := IncludedFile.CompilerFileIndex;
          end;

          if FHiddenFiles.IndexOf(IncludedFile.Filename) = -1 then begin
            NewTabs.Insert(NextTabIndex, GetDisplayFilename(IncludedFile.Filename));
            NewHints.Insert(NextTabIndex, GetFileTitle(IncludedFile.Filename));
            NewCloseButtons.Insert(NextTabIndex, True);
            Inc(NextTabIndex);
          end;

          Inc(NextMemoIndex);
          if NextMemoIndex = FFileMemos.Count then
            Break; { We're out of memos :( }
        except on E: Exception do
          begin
            StatusMessage(smkWarning, 'Failed to open included file: ' + E.Message);
            IncludedFile.Memo := nil;
          end;
        end;
      end;
      { Hide any remaining memos }
      for I := NextMemoIndex to FFileMemos.Count-1 do begin
        FFileMemos[I].BreakPoints.Clear;
        if FFileMemos[I].Used then
          RemoveMemoFromNav(FFileMemos[I]);
        FFileMemos[I].Used := False;
        FFileMemos[I].Visible := False;
      end;
    end else begin
      for I := FirstIncludedFilesMemoIndex to FFileMemos.Count-1 do begin
        FFileMemos[I].BreakPoints.Clear;
        if FFileMemos[I].Used then
          RemoveMemoFromNav(FFileMemos[I]);
        FFileMemos[I].Used := False;
        FFileMemos[I].Visible := False;
      end;
      for IncludedFile in FIncludedFiles do
        IncludedFile.Memo := nil;
    end;
  end;

var
  NewTabs, NewHints: TStringList;
  NewCloseButtons: TBoolList;
  I, SaveTabIndex: Integer;
  SaveTabName: String;
begin
  NewTabs := nil;
  NewHints := nil;
  NewCloseButtons := nil;
  try
    NewTabs := TStringList.Create;
    NewTabs.Add(MemosTabSet.Tabs[0]); { 'Main Script' }
    NewHints := TStringList.Create;
    NewHints.Add(GetFileTitle(FMainMemo.Filename));
    NewCloseButtons := TBoolList.Create;
    NewCloseButtons.Add(False);

    UpdatePreprocessorOutputMemo(NewTabs, NewHints, NewCloseButtons);
    UpdateIncludedFilesMemos(NewTabs, NewHints, NewCloseButtons);

    { Set new tabs, try keep same file open }
    SaveTabIndex := MemosTabSet.TabIndex;
    SaveTabName := MemosTabSet.Tabs[MemosTabSet.TabIndex];
    MemosTabSet.Tabs := NewTabs;
    MemosTabSet.Hints := NewHints;
    MemosTabSet.CloseButtons := NewCloseButtons;
    I := MemosTabSet.Tabs.IndexOf(SaveTabName);
    if I <> -1 then
       MemosTabSet.TabIndex := I;
    if MemosTabSet.TabIndex = SaveTabIndex then begin
      { If TabIndex stayed the same then the tabset won't perform a Click but we need this to make
       sure the right memo is visible - so trigger it ourselves }
      MemosTabSetClick(MemosTabSet);
   end;
  finally
    NewCloseButtons.Free;
    NewHints.Free;
    NewTabs.Free;
  end;

  UpdateMemosTabSetVisibility;
  UpdateBevel1Visibility;
end;

procedure TMainForm.MemoUpdateUI(Sender: TObject; Updated: TScintEditUpdates);

  procedure UpdatePendingSquiggly(const AMemo: TIDEScintEdit);
  var
    Pos: Integer;
    Value: Boolean;
  begin
    { Check for the inPendingSquiggly indicator on either side of the caret }
    Pos := AMemo.CaretPosition;
    Value := False;
    if AMemo.CaretVirtualSpace = 0 then begin
      Value := AMemo.GetIndicatorAtPosition(minPendingSquiggly, Pos);
      if not Value and (Pos > 0) then
        Value := AMemo.GetIndicatorAtPosition(minPendingSquiggly, Pos-1);
    end;
    if FOnPendingSquiggly <> Value then begin
      FOnPendingSquiggly := Value;
      { If caret has left a pending squiggly, force restyle of the line }
      if not Value then begin
        { Stop reporting the caret position to the styler (until the next
          Change event) so the token doesn't re-enter pending-squiggly state
          if the caret comes back and something restyles the line }
        AMemo.ReportCaretPositionToStyler := False;
        AMemo.RestyleLine(AMemo.GetLineFromPosition(FPendingSquigglyCaretPos));
      end;
    end;
    FPendingSquigglyCaretPos := Pos;
  end;

  procedure UpdateBraceHighlighting(const AMemo: TIDEScintEdit);
  const
    OpeningBraces: TSysCharSet = ['(', '[', '{', '<'];
    ClosingBraces: TSysCharSet = [')', ']', '}', '>'];

    function HighlightPos(const AMemo: TIDEScintEdit; const CaretPos: Integer;
      const Before: Boolean; const Braces: TSysCharSet): Boolean;
    begin
      var Pos := CaretPos;
      if Before then begin
        if Pos > 0 then
          Pos := AMemo.GetPositionBefore(Pos)
        else
          Exit(False);
      end;
      var C := AMemo.GetByteAtPosition(Pos);
      Result := C in Braces;
      if Result then begin
        var MatchPos := AMemo.GetPositionOfMatchingBrace(Pos);
        if MatchPos >= 0 then
          AMemo.SetBraceHighlighting(Pos, MatchPos)
        else begin
          { Found an unmatched brace: highlight it as bad unless it's an opening
            brace and the caret is at the end of the line }
          var CaretLineEndPos := AMemo.GetLineEndPosition(AMemo.CaretLine);
          if (C in ClosingBraces) or (CaretPos <> CaretLineEndPos) then
            AMemo.SetBraceBadHighlighting(Pos)
          else
            AMemo.SetBraceHighlighting(-1, -1);
        end;
      end;
    end;

  begin
    var Highlighted := False;
    var Section := FMemosStyler.GetSectionFromLineState(AMemo.Lines.State[AMemo.CaretLine]);
    if (Section <> scNone) and (AMemo.CaretVirtualSpace = 0) then begin
      var Pos := AMemo.CaretPosition;
      Highlighted := Highlighted or HighlightPos(AMemo, Pos, False, OpeningBraces);
      Highlighted := Highlighted or HighlightPos(AMemo, Pos, False, ClosingBraces);
      Highlighted := Highlighted or HighlightPos(AMemo, Pos, True, ClosingBraces);
      Highlighted := Highlighted or HighlightPos(AMemo, Pos, True, OpeningBraces);
    end;
    if not Highlighted then
      AMemo.SetBraceHighlighting(-1, -1);
  end;

begin
  if Updated * [suContent, suSelection] = [] then
    Exit;

  var Memo := Sender as TIDEScintEdit;

  if (Memo = FErrorMemo) and ((FErrorMemo.ErrorLine < 0) or (FErrorMemo.CaretPosition <> FErrorMemo.ErrorCaretPosition)) then
    HideError;

  if Memo = FActiveMemo then begin
    UpdateCaretPosPanelAndBackNavStack;
    UpdateEditModePanel;
  end;

  UpdatePendingSquiggly(Memo);
  UpdateBraceHighlighting(Memo);
  UpdateOccurrenceIndicators(Memo);
end;

procedure TMainForm.MemoModifiedChange(Sender: TObject);
begin
  if Sender = FActiveMemo then
    UpdateModifiedPanel;
end;

procedure TMainForm.MemoCallTipArrowClick(Sender: TObject;
  const Up: Boolean);
begin
  { Based on SciTE 5.50's SciTEBase::Notify SA::Notification::CallTipClick }
  if Up and (FCallTipState.CurrentCallTip > 0) then begin
    Dec(FCallTipState.CurrentCallTip);
    UpdateCallTipFunctionDefinition;
  end else if not Up and (FCallTipState.CurrentCallTip + 1 < FCallTipState.MaxCallTips) then begin
    Inc(FCallTipState.CurrentCallTip);
    UpdateCallTipFunctionDefinition;
  end;
end;

procedure TMainForm.MemoChange(Sender: TObject; const Info: TScintEditChangeInfo);

  procedure MemoLinesInsertedOrDeleted(Memo: TIDEScintFileEdit);
  var
    FirstAffectedLine, Line, LinePos: Integer;
  begin
    Line := Memo.GetLineFromPosition(Info.StartPos);
    LinePos := Memo.GetPositionFromLine(Line);
    FirstAffectedLine := Line;
    { If the deletion/insertion does not start on the first character of Line,
      then we consider the first deleted/inserted line to be the following
      line (Line+1). This way, if you press Del at the end of line 1, the dot
      on line 2 is removed, while line 1's dot stays intact. }
    if Info.StartPos > LinePos then
      Inc(Line);
    if Info.LinesDelta > 0 then
      MemoLinesInserted(Memo, Line, Info.LinesDelta)
    else
      MemoLinesDeleted(Memo, Line, -Info.LinesDelta, FirstAffectedLine);
  end;

var
  Memo: TIDEScintFileEdit;
begin
  if not (Sender is TIDEScintFileEdit) then
    Exit;

  Memo := TIDEScintFileEdit(Sender);

  if Memo.OpeningFile then
    Exit;

  FModifiedAnySinceLastCompile := True;
  if FDebugging then
    FModifiedAnySinceLastCompileAndGo := True
  else begin
    { Modified while not debugging or opening a file; free the debug info and clear the dots }
    DestroyDebugInfo;
  end;

  if Info.LinesDelta <> 0 then
    MemoLinesInsertedOrDeleted(Memo);

  if Memo = FErrorMemo then begin
    { When the Delete key is pressed, the caret doesn't move, so reset
      FErrorCaretPosition to ensure that OnUpdateUI calls HideError }
    FErrorMemo.ErrorCaretPosition := -1;
  end;

  { The change should trigger restyling. Allow the styler to see the current
    caret position in case it wants to set a pending squiggly indicator. }
  Memo.ReportCaretPositionToStyler := True;
end;

function TMainForm.InitiateAutoCompleteOrCallTipAllowedAtPos(const AMemo: TIDEScintEdit;
  const WordStartLinePos, PositionBeforeWordStartPos: Integer): Boolean;
begin
  Result := (PositionBeforeWordStartPos < WordStartLinePos) or
            not FMemosStyler.IsCommentOrPascalStringStyle(AMemo.GetStyleAtPosition(PositionBeforeWordStartPos));
end;

procedure TMainForm.InitiateAutoComplete(const Key: AnsiChar);

  function OnlyWhiteSpaceBeforeWord(const Memo: TIDEScintEdit; const LinePos, WordStartPos: Integer): Boolean;
  var
    I: Integer;
    C: AnsiChar;
  begin
    { Only allow autocompletion if no non-whitespace characters exist before the current word on the line }
    I := WordStartPos;
    Result := False;
    while I > LinePos do begin
      I := FActiveMemo.GetPositionBefore(I);
      if I < LinePos then
        Exit;  { shouldn't get here }
      C := FActiveMemo.GetByteAtPosition(I);
      if C > ' ' then
        Exit;
    end;
    Result := True;
  end;

var
  CaretPos, Line, LinePos, WordStartPos, WordEndPos, CharsBefore,
    LangNamePos: Integer;
  Section: TInnoSetupStylerSection;
  IsParamSection: Boolean;
  WordList: AnsiString;
  FoundSemicolon, FoundFlagsOrType, FoundDot: Boolean;
  C: AnsiChar;
begin
  if FActiveMemo.AutoCompleteActive or FActiveMemo.ReadOnly then
    Exit;

  if Key = #0 then begin
    { If a character is typed then Scintilla will handle selections but
      otherwise we should empty them and also make sure the caret is visible
      before we start autocompletion }
    FActiveMemo.SetEmptySelections;
    FActiveMemo.ScrollCaretIntoView;
  end;

  CaretPos := FActiveMemo.CaretPosition;
  Line := FActiveMemo.GetLineFromPosition(CaretPos);
  LinePos := FActiveMemo.GetPositionFromLine(Line);

  WordStartPos := FActiveMemo.GetWordStartPosition(CaretPos, True);
  WordEndPos := FActiveMemo.GetWordEndPosition(CaretPos, True);
  CharsBefore := CaretPos - WordStartPos;

  { Don't auto start autocompletion after a character is typed if there are any
    word characters adjacent to the character }
  if Key <> #0 then begin
    if CharsBefore > 1 then
      Exit;
    if WordEndPos > CaretPos then
      Exit;
  end;

  case FActiveMemo.GetByteAtPosition(WordStartPos) of
    '#':
      begin
        if not OnlyWhiteSpaceBeforeWord(FActiveMemo, LinePos, WordStartPos) then
          Exit;
        WordList := FMemosStyler.ISPPDirectivesWordList;
        FActiveMemo.SetAutoCompleteFillupChars(' ');
      end;
    '{':
      begin
        WordList := FMemosStyler.ConstantsWordList;
        FActiveMemo.SetAutoCompleteFillupChars('\:');
      end;
    '[':
      begin
        if not OnlyWhiteSpaceBeforeWord(FActiveMemo, LinePos, WordStartPos) then
          Exit;
        WordList := FMemosStyler.SectionsWordList;
        FActiveMemo.SetAutoCompleteFillupChars('');
      end;
    else
      begin
        Section := FMemosStyler.GetSectionFromLineState(FActiveMemo.Lines.State[Line]);
        if Section = scCode then begin
          { Space can only initiate autocompletion after non whitespace }
          if (Key = ' ') and OnlyWhiteSpaceBeforeWord(FActiveMemo, LinePos, WordStartPos) then
            Exit;

          var PositionBeforeWordStartPos := FActiveMemo.GetPositionBefore(WordStartPos);
          if Key <> #0 then begin
            FActiveMemo.StyleNeeded(PositionBeforeWordStartPos); { Make sure the typed character has been styled }
            if not InitiateAutoCompleteOrCallTipAllowedAtPos(FActiveMemo, LinePos, PositionBeforeWordStartPos) then
              Exit;
          end;

          WordList := '';

          { Autocomplete event functions if the current word on the line has
            exactly 1 space before it which has the word 'function' or
            'procedure' before it which has only whitespace before it }
          if (PositionBeforeWordStartPos >= LinePos) and (FActiveMemo.GetByteAtPosition(PositionBeforeWordStartPos) <= ' ') then begin
            var FunctionWordEndPos := PositionBeforeWordStartPos;
            var FunctionWordStartPos := FActiveMemo.GetWordStartPosition(FunctionWordEndPos, True);
            if OnlyWhiteSpaceBeforeWord(FActiveMemo, LinePos, FunctionWordStartPos) then begin
              var FunctionWord := FActiveMemo.GetTextRange(FunctionWordStartPos, FunctionWordEndPos);
              if SameText(FunctionWord, 'procedure') then
                WordList := FMemosStyler.EventFunctionsWordList[True]
              else if SameText(FunctionWord, 'function') then
                WordList := FMemosStyler.EventFunctionsWordList[False];
              if WordList <> '' then
                FActiveMemo.SetAutoCompleteFillupChars('');
            end;
          end;

          { If no event function was found then autocomplete script functions,
            types, etc if the current word has no dot before it }
          if WordList = '' then begin
            var ClassOrRecordMember := (PositionBeforeWordStartPos >= LinePos) and (FActiveMemo.GetByteAtPosition(PositionBeforeWordStartPos) = '.');
            WordList := FMemosStyler.ScriptWordList[ClassOrRecordMember];
            FActiveMemo.SetAutoCompleteFillupChars('');
          end;

          if WordList = '' then
            Exit;
        end else begin
          IsParamSection := FMemosStyler.IsParamSection(Section);

          { Autocomplete if the current word on the line has only whitespace
            before it, or else also: after the last ';' or after 'Flags:' or
            'Type:' in parameterized sections }
          FoundSemicolon := False;
          FoundFlagsOrType := False;
          FoundDot := False;
          var I := WordStartPos;
          while I > LinePos do begin
            I := FActiveMemo.GetPositionBefore(I);
            if I < LinePos then
              Exit;  { shouldn't get here }
            C := FActiveMemo.GetByteAtPosition(I);

            if IsParamSection and (C in [';', ':']) and
               FMemosStyler.IsSymbolStyle(FActiveMemo.GetStyleAtPosition(I)) then begin { Make sure it's an stSymbol ';' or ':' and not one inside a quoted string }
              FoundSemicolon := C = ';';
              if not FoundSemicolon then begin
                var ParameterWordEndPos := I;
                var ParameterWordStartPos := FActiveMemo.GetWordStartPosition(ParameterWordEndPos, True);
                var ParameterWord := FActiveMemo.GetTextRange(ParameterWordStartPos, ParameterWordEndPos);
                FoundFlagsOrType := SameText(ParameterWord, 'Flags') or
                                    ((Section in [scInstallDelete, scUninstallDelete]) and SameText(ParameterWord, 'Type'));
              end else
                FoundFlagsOrType := False;
              if FoundSemicolon or FoundFlagsOrType then
                Break;
            end;
            if (Section = scLangOptions) and (C = '.') and not FoundDot then begin
              { Verify that a word (language name) precedes the '.', then check for
                any non-whitespace characters before the word }
              LangNamePos := FActiveMemo.GetWordStartPosition(I, True);
              if LangNamePos >= I then
                Exit;
              I := LangNamePos;
              FoundDot := True;
            end else if C > ' ' then begin
              if IsParamSection and not (Section in [scInstallDelete, scUninstallDelete]) and
                 (FMemosStyler.FlagsWordList[Section] <> '') then begin
                { Verify word before the current word (or before that when we get here again) is
                  a valid flag and if so, continue looking before it instead of stopping }
                var FlagEndPos := FActiveMemo.GetWordEndPosition(I, True);
                var FlagStartPos := FActiveMemo.GetWordStartPosition(I, True);
                var FlagWord := FActiveMemo.GetTextRange(FlagStartPos, FlagEndPos);
                if FMemosStyler.SectionHasFlag(Section, FlagWord) then
                  I := FlagStartPos
                else
                  Exit;
              end else
                Exit;
            end;
          end;
          { Space can only initiate autocompletion after ';' or 'Flags:' or 'Type:' in parameterized sections }
          if (Key = ' ') and not (FoundSemicolon or FoundFlagsOrType) then
            Exit;

          if FoundFlagsOrType then begin
            WordList := FMemosStyler.FlagsWordList[Section];
            if WordList = '' then
              Exit;
            FActiveMemo.SetAutoCompleteFillupChars(' ');
          end else begin
            WordList := FMemosStyler.KeywordsWordList[Section];
            if WordList = '' then { CustomMessages }
              Exit;
            if IsParamSection then
              FActiveMemo.SetAutoCompleteFillupChars(':')
            else
              FActiveMemo.SetAutoCompleteFillupChars('=');
          end;
        end;
      end;
  end;
  FActiveMemo.ShowAutoComplete(CharsBefore, WordList);
end;

procedure TMainForm.UpdateCallTipFunctionDefinition(const Pos: Integer { = -1 });
begin
  { Based on SciTE 5.50's SciTEBase::FillFunctionDefinition }
    
  if Pos > 0 then
    FCallTipState.LastPosCallTip := Pos;

  // Should get current api definition
  var FunctionDefinition := FMemosStyler.GetScriptFunctionDefinition(FCallTipState.ClassOrRecordMember, FCallTipState.CurrentCallTipWord, FCallTipState.CurrentCallTip, FCallTipState.MaxCallTips);
  if ((FCallTipState.MaxCallTips = 1) and FunctionDefinition.HasParams) or //if there's a single definition then only show if it has a parameter
     (FCallTipState.MaxCallTips > 1) then begin                            //if there's multiple then show always just like MemoHintShow, so even the one without parameters if it exists
    FCallTipState.FunctionDefinition := FunctionDefinition.ScriptFuncWithoutHeader;
    if FCallTipState.MaxCallTips > 1 then
      FCallTipState.FunctionDefinition := AnsiString(Format(#1'%d of %d'#2'%s', [FCallTipState.CurrentCallTip+1, FCallTipState.MaxCallTips, FCallTipState.FunctionDefinition]));

    FActiveMemo.ShowCallTip(FCallTipState.LastPosCallTip - Length(FCallTipState.CurrentCallTipWord), FCallTipState.FunctionDefinition);
    ContinueCallTip;
  end;
end;

procedure TMainForm.InitiateCallTip(const Key: AnsiChar);
begin
  var Pos := FActiveMemo.CaretPosition;

  if (FMemosStyler.GetSectionFromLineState(FActiveMemo.Lines.State[FActiveMemo.GetLineFromPosition(Pos)]) <> scCode) or
     ((Key <> #0) and not InitiateAutoCompleteOrCallTipAllowedAtPos(FActiveMemo,
       FActiveMemo.GetPositionFromLine(FActiveMemo.GetLineFromPosition(Pos)),
       FActiveMemo.GetPositionBefore(Pos))) then
    Exit;

  { Based on SciTE 5.50's SciTEBase::StartAutoComplete }

  FCallTipState.CurrentCallTip := 0;
  FCallTipState.CurrentCallTipWord := '';
  var Line := FActiveMemo.CaretLineText;
  var Current := FActiveMemo.CaretPositionInLine;
  var CallTipWordCharacters := FActiveMemo.WordCharsAsSet;

  {$ZEROBASEDSTRINGS ON}
  repeat
    var Braces := 0;
		while ((Current > 0) and ((Braces <> 0) or not (Line[Current-1] = '('))) do begin
			if Line[Current-1] = '(' then
			  Dec(Braces)
			else if Line[Current-1] = ')' then
				Inc(Braces);
			Dec(Current);
			Dec(Pos);
    end;
    if Current > 0 then begin
      Dec(Current);
      Dec(Pos);
    end else
      Break;
    while (Current > 0) and (Line[Current-1] <= ' ') do begin
      Dec(Current);
      Dec(Pos);
    end
  until not ((Current > 0) and not CharInSet(Line[Current-1], CallTipWordCharacters));
  {$ZEROBASEDSTRINGS OFF}
  if Current <= 0 then
    Exit;

	FCallTipState.StartCallTipWord := Current - 1;
  {$ZEROBASEDSTRINGS ON}
	while (FCallTipState.StartCallTipWord > 0) and CharInSet(Line[FCallTipState.StartCallTipWord-1], CallTipWordCharacters) do
    Dec(FCallTipState.StartCallTipWord);
  FCallTipState.ClassOrRecordMember := (FCallTipState.StartCallTipWord > 0) and (Line[FCallTipState.StartCallTipWord-1] = '.');
  {$ZEROBASEDSTRINGS OFF}

  SetLength(Line, Current);
  FCallTipState.CurrentCallTipWord := Line.Substring(FCallTipState.StartCallTipWord); { Substring is zero-based }

  FCallTipState.FunctionDefinition := '';
  UpdateCallTipFunctionDefinition(Pos);
end;

procedure TMainForm.ContinueCallTip;
begin
  { Based on SciTE 5.50's SciTEBase::ContinueCallTip }

	var Line := FActiveMemo.CaretLineText;
	var Current := FActiveMemo.CaretPositionInLine;

	var Braces := 0;
	var Commas := 0;
	for var I := FCallTipState.StartCallTipWord to Current-1 do begin
    {$ZEROBASEDSTRINGS ON}
		if CharInSet(Line[I], ['(', '[']) then
      Inc(Braces)
		else if CharInSet(Line[I], [')', ']']) and (Braces > 0) then
			Dec(Braces)
		else if (Braces = 1) and (Line[I] = ',') then
			Inc(Commas);
    {$ZEROBASEDSTRINGS OFF}
	end;

  {$ZEROBASEDSTRINGS ON}
	var StartHighlight := 0;
  var FunctionDefinition := FCallTipState.FunctionDefinition;
  var FunctionDefinitionLength := Length(FunctionDefinition);
	while (StartHighlight < FunctionDefinitionLength) and not (FunctionDefinition[StartHighlight] = '(') do
		Inc(StartHighlight);
	if (StartHighlight < FunctionDefinitionLength) and (FunctionDefinition[StartHighlight] = '(') then
		Inc(StartHighlight);
	while (StartHighlight < FunctionDefinitionLength) and (Commas > 0) do begin
		if FunctionDefinition[StartHighlight] in [',', ';'] then
			Dec(Commas);
		// If it reached the end of the argument list it means that the user typed in more
		// arguments than the ones listed in the calltip
		if FunctionDefinition[StartHighlight] = ')' then
			Commas := 0
		else
			Inc(StartHighlight);
	end;
	if (StartHighlight < FunctionDefinitionLength) and (FunctionDefinition[StartHighlight] in [',', ';']) then
		Inc(StartHighlight);
	var EndHighlight := StartHighlight;
	while (EndHighlight < FunctionDefinitionLength) and not (FunctionDefinition[EndHighlight] in [',', ';']) and not (FunctionDefinition[EndHighlight] = ')') do
		Inc(EndHighlight);
  {$ZEROBASEDSTRINGS OFF}

	FActiveMemo.SetCallTipHighlight(StartHighlight, EndHighlight);
end;

procedure TMainForm.MemoCharAdded(Sender: TObject; Ch: AnsiChar);

  function LineIsBlank(const Line: Integer): Boolean;
  begin
    var S := FActiveMemo.Lines.RawLines[Line];
    Result := TScintEdit.RawStringIsBlank(S);
  end;

var
  NewLine, PreviousLine, NewIndent, PreviousIndent: Integer;
begin
  if FOptions.AutoIndent and (Ch = FActiveMemo.LineEndingString[Length(FActiveMemo.LineEndingString)]) then begin
    { Add to the new line any (remaining) indentation from the previous line }
    NewLine := FActiveMemo.CaretLine;
    PreviousLine := NewLine-1;
    if PreviousLine >= 0 then begin
      NewIndent := FActiveMemo.GetLineIndentation(NewLine);
      { If no indentation was moved from the previous line to the new line
        (i.e., there are no spaces/tabs directly to the right of the new
        caret position), and the previous line is completely empty (0 length),
        then use the indentation from the last line containing non-space
        characters. }
      if (NewIndent = 0) and (FActiveMemo.Lines.RawLineLengths[PreviousLine] = 0) then begin
        Dec(PreviousLine);
        while (PreviousLine >= 0) and LineIsBlank(PreviousLine) do
          Dec(PreviousLine);
      end;
      if PreviousLine >= 0 then begin
        PreviousIndent := FActiveMemo.GetLineIndentation(PreviousLine);
        FActiveMemo.SetLineIndentation(NewLine, NewIndent + PreviousIndent);
        FActiveMemo.CaretPosition := FActiveMemo.GetPositionFromLineExpandedColumn(NewLine,
          PreviousIndent);
      end;
    end;
  end;
  
  { Based on SciTE 5.50's SciTEBase::CharAdded but with an altered interaction
    between calltips and autocomplete }

  var DoAutoComplete := False;

  if FActiveMemo.CallTipActive then begin
    if Ch = ')' then begin
      Dec(FCallTipState.BraceCount);
      if FCallTipState.BraceCount < 1 then
        FActiveMemo.CancelCallTip
      else if FOptions.AutoCallTips then
        InitiateCallTip(Ch);
    end else if Ch = '(' then begin
      Inc(FCallTipState.BraceCount);
      if FOptions.AutoCallTips then
        InitiateCallTip(Ch);
    end else
      ContinueCallTip;
  end else if FActiveMemo.AutoCompleteActive then begin
    if Ch = '(' then begin
      Inc(FCallTipState.BraceCount);
      if FOptions.AutoCallTips then begin
        InitiateCallTip(Ch);
        if not FActiveMemo.CallTipActive then begin
          { Normally the calltip activation means any active autocompletion gets
            cancelled by Scintilla but if the current word has no call tip then
            we should make sure ourselves that the added brace still cancels
            the currently active autocompletion }
          DoAutoComplete := True;
        end;
      end;
    end else if Ch = ')' then
      Dec(FCallTipState.BraceCount)
    else
      DoAutoComplete := True;
  end else if Ch = '(' then begin
    FCallTipState.BraceCount := 1;
    if FOptions.AutoCallTips then
      InitiateCallTip(Ch);
  end else
    DoAutoComplete := True;

  if DoAutoComplete then begin
    case Ch of
      'A'..'Z', 'a'..'z', '_', '#', '{', '[', '<', '0'..'9':
        if not FActiveMemo.AutoCompleteActive and FOptions.AutoAutoComplete and not (Ch in ['0'..'9']) then
          InitiateAutoComplete(Ch);
    else
      var RestartAutoComplete := (Ch in [' ', '.']) and
        (FOptions.AutoAutoComplete or FActiveMemo.AutoCompleteActive);
      FActiveMemo.CancelAutoComplete;
      if RestartAutoComplete then
        InitiateAutoComplete(Ch);
    end;
  end;
end;

procedure TMainForm.MemoHintShow(Sender: TObject; var Info: TScintHintInfo);

  function GetCodeVariableDebugEntryFromFileLineCol(FileIndex, Line, Col: Integer; out DebugEntry: PVariableDebugEntry): Boolean;
  var
    I: Integer;
  begin
    { FVariableDebugEntries uses 1-based line and column numbers }
    Inc(Line);
    Inc(Col);
    Result := False;
    for I := 0 to FVariableDebugEntriesCount-1 do begin
      if (FVariableDebugEntries[I].FileIndex = FileIndex) and
         (FVariableDebugEntries[I].LineNumber = Line) and
         (FVariableDebugEntries[I].Col = Col) then begin
        DebugEntry := @FVariableDebugEntries[I];
        Result := True;
        Break;
      end;
    end;
  end;

  function GetCodeColumnFromPosition(const Pos: Integer): Integer;
  var
    LinePos: Integer;
    S: TScintRawString;
    U: String;
  begin
    { [Code] lines get converted from the editor's UTF-8 to UTF-16 Strings when
      passed to the compiler. This can lead to column number discrepancies
      between Scintilla and ROPS. This code simulates the conversion to try to
      find out where ROPS thinks a Pos resides. }
    LinePos := FActiveMemo.GetPositionFromLine(FActiveMemo.GetLineFromPosition(Pos));
    S := FActiveMemo.GetRawTextRange(LinePos, Pos);
    U := FActiveMemo.ConvertRawStringToString(S);
    Result := Length(U);
  end;

  function FindVarOrFuncRange(const Pos: Integer): TScintRange;
  begin
    { Note: The GetPositionAfter is needed so that when the mouse is over a '.'
      between two words, it won't match the word to the left of the '.' }
    FActiveMemo.SetDefaultWordChars;
    Result.StartPos := FActiveMemo.GetWordStartPosition(FActiveMemo.GetPositionAfter(Pos), True);
    Result.EndPos := FActiveMemo.GetWordEndPosition(Pos, True);
  end;

  function FindConstRange(const Pos: Integer): TScintRange;
  var
    BraceLevel, ConstStartPos, Line, LineEndPos, I: Integer;
    C: AnsiChar;
  begin
    Result.StartPos := 0;
    Result.EndPos := 0;
    BraceLevel := 0;
    ConstStartPos := -1;
    Line := FActiveMemo.GetLineFromPosition(Pos);
    LineEndPos := FActiveMemo.GetLineEndPosition(Line);
    I := FActiveMemo.GetPositionFromLine(Line);
    while I < LineEndPos do begin
      if (I > Pos) and (BraceLevel = 0) then
        Break;
      C := FActiveMemo.GetByteAtPosition(I);
      if C = '{' then begin
        if FActiveMemo.GetByteAtPosition(I + 1) = '{' then
          Inc(I)
        else begin
          if BraceLevel = 0 then
            ConstStartPos := I;
          Inc(BraceLevel);
        end;
      end
      else if (C = '}') and (BraceLevel > 0) then begin
        Dec(BraceLevel);
        if (BraceLevel = 0) and (ConstStartPos <> -1) then begin
          if (Pos >= ConstStartPos) and (Pos <= I) then begin
            Result.StartPos := ConstStartPos;
            Result.EndPos := I + 1;
            Exit;
          end;
          ConstStartPos := -1;
        end;
      end;
      I := FActiveMemo.GetPositionAfter(I);
    end;
  end;

  procedure UpdateInfo(var Info: TScintHintInfo; const HintStr: String; const Range: TScintRange; const Memo: TIDEScintEdit);
  begin
    Info.HintStr := HintStr;
    Info.CursorRect.TopLeft := Memo.GetPointFromPosition(Range.StartPos);
    Info.CursorRect.BottomRight := Memo.GetPointFromPosition(Range.EndPos);
    Info.CursorRect.Bottom := Info.CursorRect.Top + Memo.LineHeight;
    Info.HideTimeout := High(Integer); { infinite }
  end;

begin
  var Pos := FActiveMemo.GetPositionFromPoint(Info.CursorPos, True, True);
  if Pos < 0 then
    Exit;
  var Line := FActiveMemo.GetLineFromPosition(Pos);

  { Check if cursor is over a [Code] variable or function }
  if FMemosStyler.GetSectionFromLineState(FActiveMemo.Lines.State[Line]) = scCode then begin
    var VarOrFuncRange := FindVarOrFuncRange(Pos);
    if VarOrFuncRange.EndPos > VarOrFuncRange.StartPos then begin
      var HintStr := '';
      var DebugEntry: PVariableDebugEntry;
      if (FActiveMemo is TIDEScintFileEdit) and (FDebugClientWnd <> 0) and
         GetCodeVariableDebugEntryFromFileLineCol((FActiveMemo as TIDEScintFileEdit).CompilerFileIndex,
           Line, GetCodeColumnFromPosition(VarOrFuncRange.StartPos), DebugEntry) then begin
        var Output: String;
        case EvaluateVariableEntry(DebugEntry, Output) of
          1: HintStr := Output;
          2: HintStr := Output;
        else
          HintStr := 'Unknown error';
        end;
      end else begin
        var ClassMember := False;
        var Name := FActiveMemo.GetTextRange(VarOrFuncRange.StartPos, VarOrFuncRange.EndPos);
        var Index := 0;
        var Count: Integer;
        var FunctionDefinition := FMemosStyler.GetScriptFunctionDefinition(ClassMember, Name, Index, Count);
        if Count = 0 then begin
          ClassMember := not ClassMember;
          FunctionDefinition := FMemosStyler.GetScriptFunctionDefinition(ClassMember, Name, Index, Count);
        end;
        while Index < Count do begin
          if Index <> 0 then
            FunctionDefinition := FMemosStyler.GetScriptFunctionDefinition(ClassMember, Name, Index);
          if HintStr <> '' then
            HintStr := HintStr + #13;
          if FunctionDefinition.WasFunction then
            HintStr := HintStr + 'function '
          else
            HintStr := HintStr + 'procedure ';
          HintStr := HintStr + String(FunctionDefinition.ScriptFuncWithoutHeader);
          Inc(Index);
        end;
      end;

      if HintStr <> '' then begin
        UpdateInfo(Info, HintStr, VarOrFuncRange, FActiveMemo);
        Exit;
      end;
    end;
  end;

  if FDebugClientWnd <> 0 then begin
    { Check if cursor is over a constant }
    var ConstRange := FindConstRange(Pos);
    if ConstRange.EndPos > ConstRange.StartPos then begin
      var HintStr := FActiveMemo.GetTextRange(ConstRange.StartPos, ConstRange.EndPos);
      var Output: String;
      case EvaluateConstant(Info.HintStr, Output) of
        1: HintStr := HintStr + ' = "' + Output + '"';
        2: HintStr := HintStr + ' = Exception: ' + Output;
      else
        HintStr := HintStr + ' = Unknown error';
      end;
      UpdateInfo(Info, HintStr, ConstRange, FActiveMemo);
    end;
  end;
end;

procedure TMainForm.MainMemoDropFiles(Sender: TObject; X, Y: Integer;
  AFiles: TStrings);
begin
  if (AFiles.Count > 0) and ConfirmCloseFile(True) then
    OpenFile(FMainMemo, AFiles[0], True);
end;

procedure TMainForm.MemoZoom(Sender: TObject);
begin
  if not FSynchingZoom then begin
    FSynchingZoom := True;
    try
      for var Memo in FMemos do
        if Memo <> Sender then
          Memo.Zoom := (Sender as TScintEdit).Zoom;
    finally
      FSynchingZoom := False;
    end;
  end;
end;

procedure TMainForm.StatusBarResize(Sender: TObject);
begin
  { Without this, on Windows XP with themes, the status bar's size grip gets
    corrupted as the form is resized }
  if StatusBar.HandleAllocated then
    InvalidateRect(StatusBar.Handle, nil, True);
end;

procedure TMainForm.WMDebuggerQueryVersion(var Message: TMessage);
begin
  Message.Result := FCompilerVersion.BinVersion;
end;

procedure TMainForm.WMDebuggerHello(var Message: TMessage);
var
  PID: DWORD;
  WantCodeText: Boolean;
begin
  FDebugClientWnd := HWND(Message.WParam);

  { Save debug client process handle }
  if FDebugClientProcessHandle <> 0 then begin
    { Shouldn't get here, but just in case, don't leak a handle }
    CloseHandle(FDebugClientProcessHandle);
    FDebugClientProcessHandle := 0;
  end;
  PID := 0;
  if GetWindowThreadProcessId(FDebugClientWnd, @PID) <> 0 then
    FDebugClientProcessHandle := OpenProcess(SYNCHRONIZE or PROCESS_TERMINATE,
      False, PID);

  WantCodeText := Bool(Message.LParam);
  if WantCodeText then
    SendCopyDataMessageStr(FDebugClientWnd, Handle, CD_DebugClient_CompiledCodeTextA, FCompiledCodeText);
  SendCopyDataMessageStr(FDebugClientWnd, Handle, CD_DebugClient_CompiledCodeDebugInfoA, FCompiledCodeDebugInfo);

  UpdateRunMenu;
end;

procedure TMainForm.WMDebuggerGoodbye(var Message: TMessage);
begin
  ReplyMessage(0);
  DebuggingStopped(True);
end;

procedure TMainForm.GetMemoAndDebugEntryFromMessage(Kind, Index: Integer; var Memo: TIDEScintFileEdit; var DebugEntry: PDebugEntry);

  function GetMemoFromDebugEntryFileIndex(const FileIndex: Integer): TIDEScintFileEdit;
  var
    Memo: TIDEScintFileEdit;
  begin
    Result := nil;
    if FOptions.OpenIncludedFiles then begin
      for Memo in FFileMemos do begin
        if Memo.Used and (Memo.CompilerFileIndex = FileIndex) then begin
          Result := Memo;
          Exit;
        end;
      end;
    end else if FMainMemo.CompilerFileIndex = FileIndex then
      Result := FMainMemo;
  end;

var
  I: Integer;
begin
  for I := 0 to FDebugEntriesCount-1 do begin
    if (FDebugEntries[I].Kind = Kind) and (FDebugEntries[I].Index = Index) then begin
      Memo := GetMemoFromDebugEntryFileIndex(FDebugEntries[I].FileIndex);
      DebugEntry := @FDebugEntries[I];
      Exit;
    end;
  end;
  Memo := nil;
  DebugEntry := nil;
end;

procedure TMainForm.BringToForeground;
{ Brings our top window to the foreground. Called when pausing while
  debugging. }
var
  TopWindow: HWND;
begin
  TopWindow := GetThreadTopWindow;
  if TopWindow <> 0 then begin
    { First ask the debug client to call SetForegroundWindow() on our window.
      If we don't do this then Windows (98/2000+) will prevent our window from
      becoming activated if the debug client is currently in the foreground. }
    SendMessage(FDebugClientWnd, WM_DebugClient_SetForegroundWindow,
      WPARAM(TopWindow), 0);
    { Now call SetForegroundWindow() ourself. Why? When a remote thread calls
      SetForegroundWindow(), the request is queued; the window doesn't actually
      become active until the next time the window's thread checks the message
      queue. This call causes the window to become active immediately. }
    SetForegroundWindow(TopWindow);
  end;
end;

procedure TMainForm.DebuggerStepped(var Message: TMessage; const Intermediate: Boolean);
var
  Memo: TIDEScintFileEdit;
  DebugEntry: PDebugEntry;
  LineNumber: Integer;
begin
  GetMemoAndDebugEntryFromMessage(Message.WParam, Message.LParam, Memo, DebugEntry);
  if (Memo = nil) or (DebugEntry = nil) then
    Exit;

  LineNumber := DebugEntry.LineNumber;

  if LineNumber < 0 then { UninstExe has a DebugEntry but not a line number }
    Exit;

  if (LineNumber < Memo.LineStateCount) and
     (Memo.LineState[LineNumber] <> lnEntryProcessed) then begin
    Memo.LineState[LineNumber] := lnEntryProcessed;
    UpdateLineMarkers(Memo, LineNumber);
  end;

  if (FStepMode = smStepOut) and DebugEntry.StepOutMarker then
    FStepMode := smStepInto { Pause on next line }
  else if (FStepMode = smStepInto) or
          ((FStepMode = smStepOver) and not Intermediate) or
          ((FStepMode = smRunToCursor) and
          (FRunToCursorPoint.Kind = Integer(Message.WParam)) and
          (FRunToCursorPoint.Index = Message.LParam)) or
          (Memo.BreakPoints.IndexOf(LineNumber) <> -1) then begin
    MoveCaretAndActivateMemo(Memo, LineNumber, True);
    HideError;
    SetStepLine(Memo, LineNumber);
    BringToForeground;
    { Tell Setup to pause }
    Message.Result := 1;
    FPaused := True;
    FPausedAtCodeLine := DebugEntry.Kind = Ord(deCodeLine);
    UpdateRunMenu;
    UpdateCaption;
  end;
end;

procedure TMainForm.WMDebuggerStepped(var Message: TMessage);
begin
  DebuggerStepped(Message, False);
end;

procedure TMainForm.WMDebuggerSteppedIntermediate(var Message: TMessage);
begin
  DebuggerStepped(Message, True);
end;

procedure TMainForm.WMDPIChanged(var Message: TMessage);
begin
  inherited;
  for var Memo in FMemos do
    Memo.DPIChanged(Message);
end;

procedure TMainForm.WMDebuggerException(var Message: TMessage);
var
  Memo: TIDEScintFileEdit;
  DebugEntry: PDebugEntry;
  LineNumber: Integer;
  S: String;
begin
  if FOptions.PauseOnDebuggerExceptions then begin
    GetMemoAndDebugEntryFromMessage(Message.WParam, Message.LParam, Memo, DebugEntry);

    if DebugEntry <> nil then
      LineNumber := DebugEntry.LineNumber
    else
      LineNumber := -1;

    if (Memo <> nil) and (LineNumber >= 0) then begin
      MoveCaretAndActivateMemo(Memo, LineNumber, True);
      SetStepLine(Memo, -1);
      SetErrorLine(Memo, LineNumber);
    end;

    BringToForeground;
    { Tell Setup to pause }
    Message.Result := 1;
    FPaused := True;
    FPausedAtCodeLine := (DebugEntry <> nil) and (DebugEntry.Kind = Ord(deCodeLine));
    UpdateRunMenu;
    UpdateCaption;

    ReplyMessage(Message.Result);  { so that Setup enters a paused state now }
    if LineNumber >= 0 then begin
      S := Format('Line %d:' + SNewLine + '%s.', [LineNumber + 1, FDebuggerException]);
      if (Memo <> nil) and (Memo.Filename <> '') then
        S := Memo.Filename + SNewLine2 + S;
      MsgBox(S, 'Runtime Error', mbCriticalError, mb_Ok)
    end else
      MsgBox(FDebuggerException + '.', 'Runtime Error', mbCriticalError, mb_Ok);
  end;
end;

procedure TMainForm.WMDebuggerSetForegroundWindow(var Message: TMessage);
begin
  SetForegroundWindow(HWND(Message.WParam));
end;

procedure TMainForm.WMDebuggerCallStackCount(var Message: TMessage);
begin
  FCallStackCount := Message.WParam;
end;

procedure TMainForm.WMCopyData(var Message: TWMCopyData);
var
  S: String;
begin
  case Message.CopyDataStruct.dwData of
    CD_Debugger_ReplyW: begin
        FReplyString := '';
        SetString(FReplyString, PChar(Message.CopyDataStruct.lpData),
          Message.CopyDataStruct.cbData div SizeOf(Char));
        Message.Result := 1;
      end;
    CD_Debugger_ExceptionW: begin
        SetString(FDebuggerException, PChar(Message.CopyDataStruct.lpData),
          Message.CopyDataStruct.cbData div SizeOf(Char));
        Message.Result := 1;
      end;
    CD_Debugger_UninstExeW: begin
        SetString(FUninstExe, PChar(Message.CopyDataStruct.lpData),
          Message.CopyDataStruct.cbData div sizeOf(Char));
        Message.Result := 1;
      end;
    CD_Debugger_LogMessageW: begin
        SetString(S, PChar(Message.CopyDataStruct.lpData),
          Message.CopyDataStruct.cbData div SizeOf(Char));
        DebugLogMessage(S);
        Message.Result := 1;
      end;
    CD_Debugger_TempDirW: begin
        { Paranoia: Store it in a local variable first. That way, if there's
          a problem reading the string FTempDir will be left unmodified.
          Gotta be extra careful when storing a path we'll be deleting. }
        SetString(S, PChar(Message.CopyDataStruct.lpData),
          Message.CopyDataStruct.cbData div SizeOf(Char));
        { Extreme paranoia: If there are any embedded nulls, discard it. }
        if Pos(#0, S) <> 0 then
          S := '';
        FTempDir := S;
        Message.Result := 1;
      end;
    CD_Debugger_CallStackW: begin
        SetString(S, PChar(Message.CopyDataStruct.lpData),
          Message.CopyDataStruct.cbData div SizeOf(Char));
        DebugShowCallStack(S, FCallStackCount);
      end;
  end;
end;

function TMainForm.DestroyLineState(const AMemo: TIDEScintFileEdit): Boolean;
begin
  if Assigned(AMemo.LineState) then begin
    AMemo.LineStateCapacity := 0;
    AMemo.LineStateCount := 0;
    FreeMem(AMemo.LineState);
    AMemo.LineState := nil;
    Result := True;
  end else
    Result := False;
end;

procedure TMainForm.DestroyDebugInfo;
var
  HadDebugInfo: Boolean;
  Memo: TIDEScintFileEdit;
begin
  HadDebugInfo := False;
  for Memo in FFileMemos do
    if DestroyLineState(Memo) then
      HadDebugInfo := True;

  FDebugEntriesCount := 0;
  FreeMem(FDebugEntries);
  FDebugEntries := nil;

  FVariableDebugEntriesCount := 0;
  FreeMem(FVariableDebugEntries);
  FVariableDebugEntries := nil;

  FCompiledCodeText := '';
  FCompiledCodeDebugInfo := '';

  { Clear all dots and reset breakpoint icons (unless exiting; no point) }
  if HadDebugInfo and not(csDestroying in ComponentState) then
    UpdateAllMemosLineMarkers;
end;

var
  PrevCompilerFileIndex: Integer;
  PrevMemo: TIDEScintFileEdit;

procedure TMainForm.ParseDebugInfo(DebugInfo: Pointer);

  function GetMemoFromCompilerFileIndex(const CompilerFileIndex: Integer): TIDEScintFileEdit;
  var
    Memo: TIDEScintFileEdit;
  begin
    if (PrevCompilerFileIndex <> CompilerFileIndex) then begin
      PrevMemo := nil;
      for Memo in FFileMemos do begin
        if Memo.Used and (Memo.CompilerFileIndex = CompilerFileIndex) then begin
          PrevMemo := Memo;
          Break;
        end;
      end;
      PrevCompilerFileIndex := CompilerFileIndex;
    end;
    Result := PrevMemo;
  end;

{ This creates and fills the DebugEntries and Memo LineState arrays }
var
  Header: PDebugInfoHeader;
  Memo: TIDEScintFileEdit;
  Size: Cardinal;
  I: Integer;
begin
  DestroyDebugInfo;

  Header := DebugInfo;
  if (Header.ID <> DebugInfoHeaderID) or
     (Header.Version <> DebugInfoHeaderVersion) then
    raise Exception.Create('Unrecognized debug info format');

  try
    for Memo in FFileMemos do begin
      if Memo.Used then begin
        I := Memo.Lines.Count;
        Memo.LineState := AllocMem(SizeOf(TLineState) * (I + LineStateGrowAmount));
        Memo.LineStateCapacity := I + LineStateGrowAmount;
        Memo.LineStateCount := I;
      end;
    end;

    Inc(Cardinal(DebugInfo), SizeOf(Header^));

    FDebugEntriesCount := Header.DebugEntryCount;
    Size := FDebugEntriesCount * SizeOf(TDebugEntry);
    GetMem(FDebugEntries, Size);
    Move(DebugInfo^, FDebugEntries^, Size);
    for I := 0 to FDebugEntriesCount-1 do
      Dec(FDebugEntries[I].LineNumber);
    Inc(Cardinal(DebugInfo), Size);

    FVariableDebugEntriesCount := Header.VariableDebugEntryCount;
    Size := FVariableDebugEntriesCount * SizeOf(TVariableDebugEntry);
    GetMem(FVariableDebugEntries, Size);
    Move(DebugInfo^, FVariableDebugEntries^, Size);
    Inc(Cardinal(DebugInfo), Size);

    SetString(FCompiledCodeText, PAnsiChar(DebugInfo), Header.CompiledCodeTextLength);
    Inc(Cardinal(DebugInfo), Header.CompiledCodeTextLength);

    SetString(FCompiledCodeDebugInfo, PAnsiChar(DebugInfo), Header.CompiledCodeDebugInfoLength);

    PrevCompilerFileIndex := UnknownCompilerFileIndex;

    for I := 0 to FDebugEntriesCount-1 do begin
      if FDebugEntries[I].LineNumber >= 0 then begin
        Memo := GetMemoFromCompilerFileIndex(FDebugEntries[I].FileIndex);
        if (Memo <> nil) and (FDebugEntries[I].LineNumber < Memo.LineStateCount) then begin
          if Memo.LineState[FDebugEntries[I].LineNumber] = lnUnknown then
            Memo.LineState[FDebugEntries[I].LineNumber] := lnHasEntry;
        end;
      end;
    end;
    UpdateAllMemosLineMarkers;
  except
    DestroyDebugInfo;
    raise;
  end;
end;

procedure TMainForm.ResetAllMemosLineState;
{ Changes green dots back to grey dots }
var
  Memo: TIDEScintFileEdit;
  I: Integer;
begin
  for Memo in FFileMemos do begin
    if Memo.Used and Assigned(Memo.LineState) then begin
      for I := 0 to Memo.LineStateCount-1 do begin
        if Memo.LineState[I] = lnEntryProcessed then begin
          Memo.LineState[I] := lnHasEntry;
          UpdateLineMarkers(Memo, I);
        end;
      end;
    end;
  end;
end;

procedure TMainForm.CheckIfTerminated;
var
  H: THandle;
begin
  if FDebugging then begin
    { Check if the process hosting the debug client (e.g. Setup or the
      uninstaller second phase) has terminated. If the debug client hasn't
      connected yet, check the initial process (e.g. SetupLdr or the
      uninstaller first phase) instead. }
    if FDebugClientWnd <> 0 then
      H := FDebugClientProcessHandle
    else
      H := FProcessHandle;
    if WaitForSingleObject(H, 0) <> WAIT_TIMEOUT then
      DebuggingStopped(True);
  end;
end;

procedure TMainForm.DebuggingStopped(const WaitForTermination: Boolean);

  function GetExitCodeText: String;
  var
    ExitCode: DWORD;
  begin
    { Note: When debugging an uninstall, this will get the exit code off of
      the first phase process, since that's the exit code users will see when
      running the uninstaller outside the debugger. }
    case WaitForSingleObject(FProcessHandle, 0) of
      WAIT_OBJECT_0:
        begin
          if GetExitCodeProcess(FProcessHandle, ExitCode) then begin
            { If the high bit is set, the process was killed uncleanly (e.g.
              by a debugger). Show the exit code as hex in that case. }
            if ExitCode and $80000000 <> 0 then
              Result := Format(DebugTargetStrings[FDebugTarget] + ' exit code: 0x%.8x', [ExitCode])
            else
              Result := Format(DebugTargetStrings[FDebugTarget] + ' exit code: %u', [ExitCode]);
          end
          else
            Result := 'Unable to get ' + DebugTargetStrings[FDebugTarget] + ' exit code (GetExitCodeProcess failed)';
        end;
      WAIT_TIMEOUT:
        Result := DebugTargetStrings[FDebugTarget] + ' is still running; can''t get exit code';
    else
      Result := 'Unable to get ' + DebugTargetStrings[FDebugTarget] +  ' exit code (WaitForSingleObject failed)';
    end;
  end;

var
  ExitCodeText: String;
begin
  if WaitForTermination then begin
    { Give the initial process time to fully terminate so we can successfully
      get its exit code }  
    WaitForSingleObject(FProcessHandle, 5000);
  end;
  FDebugging := False;
  FDebugClientWnd := 0;
  ExitCodeText := GetExitCodeText;
  if FDebugClientProcessHandle <> 0 then begin
    CloseHandle(FDebugClientProcessHandle);
    FDebugClientProcessHandle := 0;
  end;
  CloseHandle(FProcessHandle);
  FProcessHandle := 0;
  FTempDir := '';
  CheckIfRunningTimer.Enabled := False;
  HideError;
  SetStepLine(FStepMemo, -1);
  UpdateRunMenu;
  UpdateCaption;
  DebugLogMessage('*** ' + ExitCodeText);
  StatusBar.Panels[spExtraStatus].Text := ' ' + ExitCodeText;
end;

procedure TMainForm.DetachDebugger;
begin
  CheckIfTerminated;
  if not FDebugging then Exit;
  SendNotifyMessage(FDebugClientWnd, WM_DebugClient_Detach, 0, 0);
  DebuggingStopped(False);
end;

function TMainForm.AskToDetachDebugger: Boolean;
begin
  if FDebugClientWnd = 0 then begin
    MsgBox('Please stop the running ' + DebugTargetStrings[FDebugTarget] +  ' process before performing this command.',
      SCompilerFormCaption, mbError, MB_OK);
    Result := False;
  end else if MsgBox('This command will detach the debugger from the running ' + DebugTargetStrings[FDebugTarget] + ' process. Continue?',
     SCompilerFormCaption, mbError, MB_OKCANCEL) = IDOK then begin
    DetachDebugger;
    Result := True;
  end else
    Result := False;
end;

function TMainForm.AnyMemoHasBreakPoint: Boolean;
begin
  { Also see RDeleteBreakPointsClick }
  for var Memo in FFileMemos do
    if Memo.Used and (Memo.BreakPoints.Count > 0) then
      Exit(True);
  Result := False;
end;

procedure TMainForm.RMenuClick(Sender: TObject);
begin
  RDeleteBreakPoints.Enabled := AnyMemoHasBreakPoint;
  { See UpdateRunMenu for other menu items }

  ApplyMenuBitmaps(RMenu);
end;

procedure TMainForm.BreakPointsPopupMenuClick(Sender: TObject);
begin
  RToggleBreakPoint2.Enabled := FActiveMemo is TIDEScintFileEdit;
  RDeleteBreakPoints2.Enabled := AnyMemoHasBreakPoint;
  { Also see UpdateRunMenu }

  ApplyMenuBitmaps(Sender as TMenuItem);
end;

{ Should always be called when one of the Enabled states would change because
  other code depends on the states being correct always even if the user never
  clicks the Run menu. This is unlike the other menus. Note: also updates
  BCompile and BStopCompile from the Build menu. }
procedure TMainForm.UpdateRunMenu;
begin
  CheckIfTerminated;
  BCompile.Enabled := not FCompiling and not FDebugging;
  CompileButton.Enabled := BCompile.Enabled;
  BStopCompile.Enabled := FCompiling;
  StopCompileButton.Enabled := BStopCompile.Enabled;
  RRun.Enabled := not FCompiling and (not FDebugging or FPaused);
  RunButton.Enabled := RRun.Enabled;
  RPause.Enabled := FDebugging and not FPaused;
  PauseButton.Enabled := RPause.Enabled;
  RRunToCursor.Enabled := RRun.Enabled and (FActiveMemo is TIDEScintFileEdit);
  RStepInto.Enabled := RRun.Enabled;
  RStepOver.Enabled := RRun.Enabled;
  RStepOut.Enabled := FPaused;
  RToggleBreakPoint.Enabled := FActiveMemo is TIDEScintFileEdit;
  RTerminate.Enabled := FDebugging and (FDebugClientWnd <> 0);
  TerminateButton.Enabled := RTerminate.Enabled;
  REvaluate.Enabled := FDebugging and (FDebugClientWnd <> 0);
  { See RMenuClick for other menu items and also see BreakPointsPopupMenuClick }
end;

procedure TMainForm.UpdateSaveMenuItemAndButton;
begin
  FSave.Enabled := FActiveMemo is TIDEScintFileEdit;
  SaveButton.Enabled := FSave.Enabled;
end;

procedure TMainForm.UpdateTargetMenu;
begin
  if FDebugTarget = dtSetup then begin
    RTargetSetup.Checked := True;
    TargetSetupButton.Down := True;
  end else begin
    RTargetUninstall.Checked := True;
    TargetUninstallButton.Down := True;
  end;
end;

procedure TMainForm.UpdateKeyMapping;

type
  TKeyMappedMenu = TPair<TMenuItem, TPair<TShortcut, TToolButton>>;

  function KMM(const MenuItem: TMenuItem; const DelphiKey: Word; const DelphiShift: TShiftState;
    const VisualStudioKey: Word; const VisualStudioShift: TShiftState;
    const ToolButton: TToolButton = nil): TKeyMappedMenu;
  begin
    var AShortCut: TShortCut;
    case FOptions.KeyMappingType of
      kmtDelphi: AShortCut := ShortCut(DelphiKey, DelphiShift);
      kmtVisualStudio: AShortCut := ShortCut(VisualStudioKey, VisualStudioShift);
    else
      raise Exception.Create('Unknown FOptions.KeyMappingType');
    end;

    Result := TKeyMappedMenu.Create(MenuItem, TPair<TShortcut, TToolButton>.Create(AShortcut, ToolButton)); { These are records so no need to free }
  end;

begin
  var KeyMappedMenus := [
    KMM(EFindRegEx, Ord('R'), [ssCtrl, ssAlt], Ord('R'), [ssAlt]),
    KMM(BCompile, VK_F9, [ssCtrl], Ord('B'), [ssCtrl], CompileButton), { Also FCompileShortCut2 below }
    KMM(RRun, VK_F9, [], VK_F5, [], RunButton),
    KMM(RRunToCursor, VK_F4, [], VK_F10, [ssCtrl]),
    KMM(RStepInto, VK_F7, [], VK_F11, []),
    KMM(RStepOver, VK_F8, [], VK_F10, []),
    KMM(RStepOut, VK_F8, [ssShift], VK_F11, [ssShift]),
    KMM(RToggleBreakPoint, VK_F5, [], VK_F9, []),
    KMM(RDeleteBreakPoints, VK_F5, [ssShift, ssCtrl], VK_F9, [ssShift, ssCtrl]),
    KMM(RTerminate, VK_F2, [ssCtrl], VK_F5, [ssShift], TerminateButton),
    KMM(REvaluate, VK_F7, [ssCtrl], VK_F9, [ssShift])];

  FKeyMappedMenus.Clear;

  for var KeyMappedMenu in KeyMappedMenus do begin
    var ShortCut := KeyMappedMenu.Value.Key;
    var ToolButton := KeyMappedMenu.Value.Value;
    KeyMappedMenu.Key.ShortCut := ShortCut;
    if ToolButton <> nil then begin
      var MenuItem := KeyMappedMenu.Key;
      ToolButton.Hint := Format('%s (%s)', [RemoveAccelChar(MenuItem.Caption), NewShortCutToText(ShortCut)]);
    end;
    FKeyMappedMenus.Add(ShortCut, ToolButton);
  end;

  { Set fake shortcuts on any duplicates of the above in popup menus }

  SetFakeShortCut(RToggleBreakPoint2, RToggleBreakPoint.ShortCut);
  SetFakeShortCut(RDeleteBreakPoints2, RDeleteBreakPoints.ShortCut);

  { Handle two special cases:
    -The Nav buttons have no corresponding menu item and also no ShortCut property
     so they need special handling
    -Visual Studio and Delphi have separate Compile and Build shortcuts and the
     Compile shortcut is displayed by the menu and is set above but we want to
     allow the Build shortcuts as well for our single Build/Compile command }

  FBackNavButtonShortCut := ShortCut(VK_LEFT, [ssAlt]);
  FForwardNavButtonShortCut := ShortCut(VK_RIGHT, [ssAlt]);

  case FOptions.KeyMappingType of
    kmtDelphi:
      begin
        FBackNavButtonShortCut2 := 0;
        FForwardNavButtonShortCut2 := 0;
        FCompileShortCut2 := ShortCut(VK_F9, [ssShift]);
      end;
    kmtVisualStudio:
      begin
        FBackNavButtonShortCut2 := ShortCut(VK_OEM_MINUS, [ssCtrl]);
        FForwardNavButtonShortCut2 := ShortCut(VK_OEM_MINUS, [ssCtrl, ssShift]);
        FCompileShortCut2 := ShortCut(VK_F7, []);
      end;
  else
    raise Exception.Create('Unknown FOptions.KeyMappingType');
  end;

  BackNavButton.Hint := Format('Back (%s)', [NewShortCutToText(FBackNavButtonShortCut)]);
  FKeyMappedMenus.Add(FBackNavButtonShortCut, nil);
  ForwardNavButton.Hint := Format('Forward (%s)', [NewShortCutToText(FForwardNavButtonShortCut)]);
  FKeyMappedMenus.Add(FForwardNavButtonShortCut, nil);
end;

procedure TMainForm.UpdateTheme;
begin
  FTheme.Typ := FOptions.ThemeType;

  SetHelpFileDark(FTheme.Dark);

  for var Memo in FMemos do begin
    Memo.UpdateThemeColorsAndStyleAttributes;
    SetControlWindowTheme(Memo, FTheme.Dark);
  end;

  InitFormTheme(Self);
  ToolbarPanel.Color := FTheme.Colors[tcToolBack];

  if FTheme.Dark then begin
    ThemedToolbarVirtualImageList.ImageCollection := ImagesModule.DarkToolBarImageCollection;
    ThemedMarkersAndACVirtualImageList.ImageCollection := ImagesModule.DarkMarkersAndACImageCollection;
  end else begin
    ThemedToolbarVirtualImageList.ImageCollection := ImagesModule.LightToolBarImageCollection;
    ThemedMarkersAndACVirtualImageList.ImageCollection := ImagesModule.LightMarkersAndACImageCollection;
  end;

  UpdateBevel1Visibility;
  UpdateMarginsAndAutoCompleteIcons;

  SplitPanel.ParentBackground := False;
  SplitPanel.Color := FTheme.Colors[tcSplitterBack];

  if FTheme.Dark then begin
    MemosTabSet.Theme := FTheme;
    OutputTabSet.Theme := FTheme;
  end else begin
    MemosTabSet.Theme := nil;
    OutputTabSet.Theme := nil;
  end;

  FMenuDarkBackgroundBrush.Color := FTheme.Colors[tcToolBack];
  FMenuDarkHotOrSelectedBrush.Color := $2C2C2C; { Same as themed menu drawn by Windows 11, which is close to Colors[tcBack] }

  DrawMenuBar(Handle);

 { SetPreferredAppMode doesn't work without FlushMenuThemes here: it would have
   to be called before the form is created to have an effect without
   FlushMenuThemes. So don't call SetPreferredAppMode if FlushMenuThemes is
   missing. }
  if Assigned(SetPreferredAppMode) and Assigned(FlushMenuThemes) then begin
    FMenuImageList := ThemedToolbarVirtualImageList;
    if FTheme.Dark then
      SetPreferredAppMode(PAM_FORCEDARK)
    else
      SetPreferredAppMode(PAM_FORCELIGHT);
    FlushMenuThemes;
  end else
    FMenuImageList := LightToolbarVirtualImageList;
end;

procedure TMainForm.UpdateThemeData(const Open: Boolean);

  procedure CloseThemeDataIfNeeded(var ThemeData: HTHEME);
  begin
    if ThemeData <> 0 then begin
      CloseThemeData(ThemeData);
      ThemeData := 0;
    end;
  end;

begin
  CloseThemeDataIfNeeded(FProgressThemeData);
  CloseThemeDataIfNeeded(FMenuThemeData);
  CloseThemeDataIfNeeded(FToolbarThemeData);

  if Open and UseThemes then begin
    FProgressThemeData := OpenThemeData(Handle, 'Progress');
    FMenuThemeData := OpenThemeData(Handle, 'Menu');
    FToolbarThemeData := OpenThemeData(Handle, 'Toolbar');
  end;
end;

procedure TMainForm.UpdateUpdatePanel;
begin
  UpdatePanel.Visible := FUpdatePanelMessages.Count > 0;
  if UpdatePanel.Visible then begin
    var MessageToShowIndex := FUpdatePanelMessages.Count-1;
    UpdateLinkLabel.Tag := MessageToShowIndex;
    UpdateLinkLabel.Caption := FUpdatePanelMessages[MessageToShowIndex].Msg;
    UpdatePanel.Color := FUpdatePanelMessages[MessageToShowIndex].Color;
  end;
  UpdateBevel1Visibility;
end;

procedure TMainForm.UpdateMenuBitmapsIfNeeded;

  procedure AddMenuBitmap(const MenuBitmaps: TMenuBitmaps; const DC: HDC; const BitmapInfo: TBitmapInfo;
    const MenuItem: TMenuItem; const ImageList: TVirtualImageList; const ImageIndex: Integer); overload;
  begin
    var pvBits: Pointer;
    var Bitmap := CreateDIBSection(DC, bitmapInfo, DIB_RGB_COLORS, pvBits, 0, 0);
    var OldBitmap := SelectObject(DC, Bitmap);
    if ImageList_Draw(ImageList.Handle, ImageIndex, DC, 0, 0, ILD_TRANSPARENT) then
      MenuBitmaps.Add(MenuItem, Bitmap)
    else begin
      SelectObject(DC, OldBitmap);
      DeleteObject(Bitmap);
    end;
  end;

  procedure AddMenuBitmap(const MenuBitmaps: TMenuBitmaps; const DC: HDC; const BitmapInfo: TBitmapInfo;
    const MenuItem: TMenuItem; const ImageList: TVirtualImageList; const ImageName: String); overload;
  begin
    AddMenuBitmap(MenuBitmaps, DC, BitmapInfo, MenuItem, ImageList, ImageList.GetIndexByName(ImageName));
  end;

type
  TButtonedMenu = TPair<TMenuItem, TToolButton>;
  TNamedMenu = TPair<TMenuItem, String>;

  function BM(const MenuItem: TMenuItem; const ToolButton: TToolButton): TButtonedMenu;
  begin
    Result := TButtonedMenu.Create(MenuItem, ToolButton); { This is a record so no need to free }
  end;

  function NM(const MenuItem: TMenuItem; const Name: String): TNamedMenu;
  begin
    Result := TNamedMenu.Create(MenuItem, Name); { This is a record so no need to free }
  end;

begin
  { This will create bitmaps for the current DPI using ImageList_Draw.

    These draw perfectly even on Windows 7. Other techniques don't work because
    they loose transparency or only look good on Windows 8 and later. Or they do
    work but cause lots more VCL code to be run than just our simple CreateDIB+Draw
    combo.

    ApplyBitmaps will apply them to menu items using SetMenuItemInfo. The menu item
    does not copy the bitmap so they should still be alive after ApplyBitmaps is done.

    Depends on FMenuImageList to pick the best size icons for the current DPI
    from the collection. }

  var ImageList := FMenuImageList;

  var NewSize: TSize;
  NewSize.cx := ImageList.Width;
  NewSize.cy := ImageList.Height;
  if (NewSize.cx <> FMenuBitmapsSize.cx) or (NewSize.cy <> FMenuBitmapsSize.cy) or
     (ImageList.ImageCollection <> FMenuBitmapsSourceImageCollection) then begin

    { Cleanup previous }

    for var Bitmap in FMenuBitmaps.Values do
      DeleteObject(Bitmap);
    FMenuBitmaps.Clear;

    { Create }

    var DC := CreateCompatibleDC(0);
    if DC <> 0 then begin
      try
        var BitmapInfo := CreateBitmapInfo(NewSize.cx, NewSize.cy, 32);

        var ButtonedMenus := [
          BM(FNewMainFile, NewMainFileButton),
          BM(FOpenMainFile, OpenMainFileButton),
          BM(FSave, SaveButton),
          BM(BCompile, CompileButton),
          BM(BStopCompile, StopCompileButton),
          BM(RRun, RunButton),
          BM(RPause, PauseButton),
          BM(RTerminate, TerminateButton),
          BM(HDoc, HelpButton)];

        for var ButtonedMenu in ButtonedMenus do
          AddMenuBitmap(FMenuBitmaps, DC, BitmapInfo, ButtonedMenu.Key, ImageList, ButtonedMenu.Value.ImageIndex);

        var NamedMenus := [
          NM(FClearRecent, 'eraser'),
          NM(FSaveMainFileAs, 'save-as-filled'),
          NM(FSaveAll, 'save-all-filled'),
          NM(FPrint, 'printer'),
          NM(EUndo, 'command-undo-1'),
          NM(ERedo, 'command-redo-1'),
          NM(ECut, 'clipboard-cut'),
          NM(ECopy, 'clipboard-copy'),
          NM(POutputListCopy, 'clipboard-copy'),
          NM(EPaste, 'clipboard-paste'),
          NM(EDelete, 'symbol-cancel'),
          NM(ESelectAll, 'select-all'),
          NM(POutputListSelectAll, 'select-all'),
          NM(EFind, 'find'),
          NM(EFindInFiles, 'folder-open-filled-find'),
          //NM(EFindNext, 'unused\find-arrow-right-2'),
          //NM(EFindPrevious, 'unused\find-arrow-left-2'),
          NM(EReplace, 'replace'),
          NM(EFoldLine, 'symbol-remove'),
          NM(EUnfoldLine, 'symbol-add'),
          NM(VZoomIn, 'zoom-in'),
          NM(VZoomOut, 'zoom-out'),
          NM(VNextTab, 'control-tab-filled-arrow-right-2'),
          NM(VPreviousTab, 'control-tab-filled-arrow-left-2'),
          //NM(VCloseCurrentTab, 'unused\control-tab-filled-cancel-2'),
          NM(VReopenTabs, 'control-tab-filled-redo-1'),
          NM(VReopenTabs2, 'control-tab-filled-redo-1'),
          NM(BOpenOutputFolder, 'folder-open-filled'),
          NM(RParameters, 'control-edit'),
          NM(RRunToCursor, 'debug-start-filled-arrow-right-2'),
          NM(RStepInto, 'debug-step-into'),
          NM(RStepOver, 'debug-step-over'),
          NM(RStepOut, 'debug-step-out'),
          NM(RToggleBreakPoint, 'debug-breakpoint-filled'),
          NM(RToggleBreakPoint2, 'debug-breakpoint-filled'),
          NM(RDeleteBreakPoints, 'debug-breakpoints-filled-eraser'),
          NM(RDeleteBreakPoints2, 'debug-breakpoints-filled-eraser'),
          NM(REvaluate, 'variables'),
          NM(TAddRemovePrograms, 'application'),
          NM(TGenerateGUID, 'tag-script-filled'),
          NM(TFilesDesigner, 'documents-script-filled'),
          NM(TRegistryDesigner, 'control-tree-script-filled'),
          NM(TMsgBoxDesigner, 'comment-text-script-filled'),
          NM(TSignTools, 'key-filled'),
          NM(TOptions, 'gear-filled'),
          NM(HDonate, 'heart-filled'),
          NM(HMailingList, 'alert-filled'),
          NM(HWhatsNew, 'announcement'),
          NM(HWebsite, 'home'),
          NM(HAbout, 'button-info')];

        for var NamedMenu in NamedMenus do
          AddMenuBitmap(FMenuBitmaps, DC, BitmapInfo, NamedMenu.Key, ImageList, NamedMenu.Value);
      finally
        DeleteDC(DC);
      end;
    end;

    FMenuBitmapsSize := NewSize;
    FMenuBitmapsSourceImageCollection := FMenuImageList.ImageCollection;
  end;
end;

procedure TMainForm.ApplyMenuBitmaps(const ParentMenuItem: TMenuItem);
begin
  UpdateMenuBitmapsIfNeeded;

  { Setting MainMenu1.ImageList or a menu item's .Bitmap to make a menu item
    show a bitmap is not OK: it causes the entire menu to become owner drawn
    which makes it looks different from native menus and additionally the trick
    SetFakeShortCut uses doesn't work with owner drawn menus.

    Instead UpdateMenuBitmapsIfNeeded has prepared images which can be applied
    to native menu items using SetMenuItemInfo and MIIM_BITMAP - which is what we
    do below.

    A problem with this is that Delphi's TMenu likes to constantly recreate the
    underlying native menu items, for example when updating the caption. Sometimes
    it will even destroy and repopulate an entire menu because of a simple change
    like setting the caption of a single item!

    This means the result of our SetMenuItemInfo call (which Delphi doesn't know
    about) will quickly become lost when Delphi recreates the menu item.

    Fixing this in the OnChange event is not possible, this is event is more
    than useless.

    The solution is shown by TMenu.DispatchPopup: in reaction to WM_INITMENUPOPUP
    it calls our Click events right before the menu is shown, giving us the
    opportunity to call SetMenuItemInfo for the menu's items.

    This works unless Delphi decides to destroy and repopulate the menu after
    calling Click. Most amazingly it can do that indeed: it does this if the DPI
    changed since the last popup or if a automatic hotkey change or line reduction
    happens due to the menu's AutoHotkeys or AutoLineReduction properties. To make
    things even worse: for the Run menu it does this each and every time it is
    opened: this menu currently has a 'Step Out' item which has no shortcut but
    also all its letters are taken by another item already. This confuses the
    AutoHotkeys code, making it destroy and repopulate the entire menu over and
    over because it erroneously thinks a hotkey changed.

    To avoid this MainMenu1.AutoHotkeys was set to maManual since we have always
    managed the hotkeys ourselves anyway and .AutoLineReduction was also set to
    maManual and we now manage that ourselves as well.

    This just leave an issue with the icons not appearing on the first popup after
    a DPI change and this seems like a minor issue only.
    
    For TPopupMenu: calling ApplyMenuBitmaps(PopupMenu.Items) does work but makes
    the popup only show icons without text. This seems to be a limitiation of menus
    created by CreatePopupMenu instead of CreateMenu. This is why our popups with
    icons are all menu items popped using TMainFormPopupMenu. These menu items
    are hidden in the main menu and temporarily shown on popup. Popping an always
    hidden menu item (or a visible one as a child of a hidden parent) doesnt work.  }

  var mmi: TMenuItemInfo;
  mmi.cbSize := SizeOf(mmi);
  mmi.fMask := MIIM_BITMAP;

  for var I := 0 to ParentMenuItem.Count-1 do begin
    var MenuItem := ParentMenuItem.Items[I];
    if MenuItem.Visible then begin
      if FMenuBitmaps.TryGetValue(MenuItem, mmi.hbmpItem) then
        SetMenuItemInfo(ParentMenuItem.Handle, MenuItem.Command, False, mmi);
      if MenuItem.Count > 0 then
        ApplyMenuBitmaps(MenuItem);
    end;
  end;
end;

procedure TMainForm.StartProcess;
var
  RunFilename, RunParameters, WorkingDir: String;
  Info: TShellExecuteInfo;
  SaveFocusWindow: HWND;
  WindowList: Pointer;
  ShellExecuteResult: BOOL;
  ErrorCode: DWORD;
begin
  if FDebugTarget = dtUninstall then begin
    if FUninstExe = '' then
      raise Exception.Create(SCompilerNeedUninstExe);
    RunFilename := FUninstExe;
  end else begin
    if FCompiledExe = '' then
      raise Exception.Create(SCompilerNeedCompiledExe);
    RunFilename := FCompiledExe;
  end;
  RunParameters := Format('/DEBUGWND=$%x ', [Handle]) + FRunParameters;

  ResetAllMemosLineState;
  DebugOutputList.Clear;
  SendMessage(DebugOutputList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
  DebugCallStackList.Clear;
  SendMessage(DebugCallStackList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
  if not (OutputTabSet.TabIndex in [tiDebugOutput, tiDebugCallStack]) then
    OutputTabSet.TabIndex := tiDebugOutput;
  SetStatusPanelVisible(True);

  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(Info);
  Info.fMask := SEE_MASK_FLAG_NO_UI or SEE_MASK_FLAG_DDEWAIT or
    SEE_MASK_NOCLOSEPROCESS or SEE_MASK_NOZONECHECKS;
  Info.Wnd := Handle;
  if FOptions.RunAsDifferentUser then
    Info.lpVerb := 'runas'
  else
    Info.lpVerb := 'open';
  Info.lpFile := PChar(RunFilename);
  Info.lpParameters := PChar(RunParameters);
  WorkingDir := PathExtractDir(RunFilename);
  Info.lpDirectory := PChar(WorkingDir);
  Info.nShow := SW_SHOWNORMAL;
  { When the RunAsDifferentUser option is enabled, it's this process that
    waits on the UAC dialog, not Setup(Ldr), so we need to disable windows to
    prevent the user from clicking other things before the UAC dialog is
    dismissed (which is definitely a possibility if the "Switch to the secure
    desktop when prompting for elevation" setting is disabled in Group
    Policy). }
  SaveFocusWindow := GetFocus;
  WindowList := DisableTaskWindows(Handle);
  try
    { Also temporarily remove the focus since a disabled window's children can
      still receive keystrokes. This is needed if Windows doesn't switch to
      the secure desktop immediately and instead shows a flashing taskbar
      button that the user must click (which happened on Windows Vista; I'm
      unable to reproduce it on Windows 11). }
    Windows.SetFocus(0);
    ShellExecuteResult := ShellExecuteEx(@Info);
    ErrorCode := GetLastError;
  finally
    EnableTaskWindows(WindowList);
    Windows.SetFocus(SaveFocusWindow);
  end;
  if not ShellExecuteResult then begin
    { Don't display error message if user clicked Cancel at UAC dialog }
    if ErrorCode = ERROR_CANCELLED then
      Abort;
    raise Exception.CreateFmt(SCompilerExecuteSetupError2, [RunFilename,
      ErrorCode, Win32ErrorString(ErrorCode)]);
  end;
  FDebugging := True;
  FPaused := False;
  FProcessHandle := Info.hProcess;
  CheckIfRunningTimer.Enabled := True;
  UpdateRunMenu;
  UpdateCaption;
  DebugLogMessage('*** ' + DebugTargetStrings[FDebugTarget] + ' started');
end;

procedure TMainForm.CompileIfNecessary;

  function UnopenedIncludedFileModifiedSinceLastCompile: Boolean;
  var
    IncludedFile: TIncludedFile;
    NewTime: TFileTime;
  begin
    Result := False;
    for IncludedFile in FIncludedFiles do begin
      if (IncludedFile.Memo = nil) and IncludedFile.HasLastWriteTime and
         GetLastWriteTimeOfFile(IncludedFile.Filename, @NewTime) and
         (CompareFileTime(IncludedFile.LastWriteTime, NewTime) <> 0) then begin
        Result := True;
        Exit;
      end;
    end;
  end;

begin
  CheckIfTerminated;

  { Display warning if the user modified the script while running - does not support unopened included files  }
  if FDebugging and FModifiedAnySinceLastCompileAndGo then begin
    if MsgBox('The changes you made will not take effect until you ' +
       're-compile.' + SNewLine2 + 'Continue running anyway?',
       SCompilerFormCaption, mbError, MB_YESNO) <> IDYES then
      Abort;
    FModifiedAnySinceLastCompileAndGo := False;
    { The process may have terminated while the message box was up; check,
      and if it has, we want to recompile below }
    CheckIfTerminated;
  end;

  if not FDebugging and (FModifiedAnySinceLastCompile or UnopenedIncludedFileModifiedSinceLastCompile) then
    CompileFile('', False);
end;

procedure TMainForm.Go(AStepMode: TStepMode);
begin
  CompileIfNecessary;
  FStepMode := AStepMode;
  HideError;
  SetStepLine(FStepMemo, -1);
  if FDebugging then begin
    if FPaused then begin
      FPaused := False;
      UpdateRunMenu;
      UpdateCaption;
      if DebugCallStackList.Items.Count > 0 then begin
        DebugCallStackList.Clear;
        SendMessage(DebugCallStackList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
        DebugCallStackList.Update;
      end;
      { Tell it to continue }
      SendNotifyMessage(FDebugClientWnd, WM_DebugClient_Continue,
        Ord(AStepMode = smStepOver), 0);
    end;
  end
  else
    StartProcess;
end;

function TMainForm.EvaluateConstant(const S: String;
  out Output: String): Integer;
begin
  { This is about evaluating constants like 'app' and not [Code] variables }
  FReplyString := '';
  Result := SendCopyDataMessageStr(FDebugClientWnd, Handle,
    CD_DebugClient_EvaluateConstantW, S);
  if Result > 0 then
    Output := FReplyString;
end;

function TMainForm.EvaluateVariableEntry(const DebugEntry: PVariableDebugEntry;
  out Output: String): Integer;
begin
  FReplyString := '';
  Result := SendCopyDataMessage(FDebugClientWnd, Handle, CD_DebugClient_EvaluateVariableEntry,
    DebugEntry, SizeOf(DebugEntry^));
  if Result > 0 then
    Output := FReplyString;
end;

procedure TMainForm.RRunClick(Sender: TObject);
begin
  Go(smRun);
end;

procedure TMainForm.RParametersClick(Sender: TObject);
begin
  ReadMRUParametersList;
  InputQueryCombo('Run Parameters', 'Command line parameters for ' + DebugTargetStrings[dtSetup] +
    ' and ' + DebugTargetStrings[dtUninstall] + ':', FRunParameters, FMRUParametersList);
  if FRunParameters <> '' then
    ModifyMRUParametersList(FRunParameters, True);
end;

procedure TMainForm.RPauseClick(Sender: TObject);
begin
  if FDebugging and not FPaused then begin
    if FStepMode <> smStepInto then begin
      FStepMode := smStepInto;
      UpdateCaption;
    end
    else
      MsgBox('A pause is already pending.', SCompilerFormCaption, mbError,
        MB_OK);
  end;
end;

procedure TMainForm.RRunToCursorClick(Sender: TObject);

  function GetDebugEntryFromMemoAndLineNumber(Memo: TIDEScintFileEdit; LineNumber: Integer;
    var DebugEntry: TDebugEntry): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to FDebugEntriesCount-1 do begin
      if (FDebugEntries[I].FileIndex = Memo.CompilerFileIndex) and
         (FDebugEntries[I].LineNumber = LineNumber) then begin
        DebugEntry := FDebugEntries[I];
        Result := True;
        Break;
      end;
    end;
  end;

begin
  CompileIfNecessary;
  if not GetDebugEntryFromMemoAndLineNumber((FActiveMemo as TIDEScintFileEdit), FActiveMemo.CaretLine, FRunToCursorPoint) then begin
    MsgBox('No code was generated for the current line.', SCompilerFormCaption,
      mbError, MB_OK);
    Exit;
  end;
  Go(smRunToCursor);
end;

procedure TMainForm.RStepIntoClick(Sender: TObject);
begin
  Go(smStepInto);
end;

procedure TMainForm.RStepOutClick(Sender: TObject);
begin
  if FPausedAtCodeLine then
    Go(smStepOut)
  else
    Go(smStepInto);
end;

procedure TMainForm.RStepOverClick(Sender: TObject);
begin
  Go(smStepOver);
end;

procedure TMainForm.RTerminateClick(Sender: TObject);
var
  S, Dir: String;
begin
  S := 'This will unconditionally terminate the running ' +
       DebugTargetStrings[FDebugTarget] + ' process. Continue?';

  if FDebugTarget = dtSetup then
    S := S + #13#10#13#10'Note that if ' + DebugTargetStrings[FDebugTarget] + ' ' +
         'is currently in the installation phase, any changes made to the ' +
         'system thus far will not be undone, nor will uninstall data be written.';

  if MsgBox(S, 'Terminate', mbConfirmation, MB_YESNO or MB_DEFBUTTON2) <> IDYES then
    Exit;
  CheckIfTerminated;
  if FDebugging then begin
    DebugLogMessage('*** Terminating process');
    Win32Check(TerminateProcess(FDebugClientProcessHandle, 6));
    if (WaitForSingleObject(FDebugClientProcessHandle, 5000) <> WAIT_TIMEOUT) and
       (FTempDir <> '') then begin
      Dir := FTempDir;
      FTempDir := '';
      DebugLogMessage('*** Removing left-over temporary directory: ' + Dir);
      { Sleep for a bit to allow files to be unlocked by Windows,
        otherwise it fails intermittently (with Hyper-Threading, at least) }
      Sleep(50);
      if not DeleteDirTree(Dir) and DirExists(Dir) then
        DebugLogMessage('*** Failed to remove temporary directory');
    end;
    DebuggingStopped(True);
  end;
end;

procedure TMainForm.REvaluateClick(Sender: TObject);
var
  Output: String;
begin
  if InputQuery('Evaluate', 'Constant to evaluate (e.g., "{app}"):',
     FLastEvaluateConstantText) then begin
    case EvaluateConstant(FLastEvaluateConstantText, Output) of
      1: MsgBox(Output, 'Evaluate Result', mbInformation, MB_OK);
      2: MsgBox(Output, 'Evaluate Error', mbError, MB_OK);
    else
      MsgBox('An unknown error occurred.', 'Evaluate Error', mbError, MB_OK);
    end;
  end;
end;

procedure TMainForm.CheckIfRunningTimerTimer(Sender: TObject);
begin
  { In cases of normal Setup termination, we receive a WM_Debugger_Goodbye
    message. But in case we don't get that, use a timer to periodically check
    if the process is no longer running. }
  CheckIfTerminated;
end;

procedure TMainForm.POutputListCopyClick(Sender: TObject);
var
  ListBox: TListBox;
  Text: String;
  I: Integer;
begin
  if CompilerOutputList.Visible then
    ListBox := CompilerOutputList
  else if DebugOutputList.Visible then
    ListBox := DebugOutputList
  else if DebugCallStackList.Visible then
    ListBox := DebugCallStackList
  else
    ListBox := FindResultsList;
  Text := '';
  if ListBox.SelCount > 0 then begin
    for I := 0 to ListBox.Items.Count-1 do begin
      if ListBox.Selected[I] then begin
        if Text <> '' then
          Text := Text + SNewLine;
        Text := Text + ListBox.Items[I];
      end;
    end;
  end;
  Clipboard.AsText := Text;
end;

procedure TMainForm.POutputListSelectAllClick(Sender: TObject);
var
  ListBox: TListBox;
  I: Integer;
begin
  if CompilerOutputList.Visible then
    ListBox := CompilerOutputList
  else if DebugOutputList.Visible then
    ListBox := DebugOutputList
  else if DebugCallStackList.Visible then
    ListBox := DebugCallStackList
  else
    ListBox := FindResultsList;
  ListBox.Items.BeginUpdate;
  try
    for I := 0 to ListBox.Items.Count-1 do
      ListBox.Selected[I] := True;
  finally
    ListBox.Items.EndUpdate;
  end;
end;

procedure TMainForm.OutputListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [ssCtrl] then begin
    if Key = Ord('C') then
      POutputListCopyClick(Sender)
    else if Key = Ord('A') then
      POutputListSelectAllClick(Sender);
  end;
end;

procedure TMainForm.AppOnIdle(Sender: TObject; var Done: Boolean);
begin
  { For an explanation of this, see the comment where HandleMessage is called }
  if FCompiling then
    Done := False;

  FBecameIdle := True;
end;

procedure TMainForm.EGotoClick(Sender: TObject);
var
  S: String;
  L: Integer;
begin
  S := IntToStr(FActiveMemo.CaretLine + 1);
  if InputQuery('Go to Line', 'Line number:', S) then begin
    L := StrToIntDef(S, Low(L));
    if L <> Low(L) then
      FActiveMemo.CaretLine := L - 1;
  end;
end;

procedure TMainForm.StatusBarClick(Sender: TObject);
begin
  if MemosTabSet.Visible and (FHiddenFiles.Count > 0) then begin
    var Point := SmallPointToPoint(TSmallPoint(GetMessagePos()));
    var X := StatusBar.ScreenToClient(Point).X;
    var W := 0;
    for var I := 0 to StatusBar.Panels.Count-1 do begin
      Inc(W, StatusBar.Panels[I].Width);
      if X < W then begin
        if I = spHiddenFilesCount then
          (MemosTabSet.PopupMenu as TMainFormPopupMenu).Popup(Point.X, Point.Y);
        Break;
      end else if I = spHiddenFilesCount then
        Break;
    end;
  end;
end;

procedure TMainForm.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
const
  TP_DROPDOWNBUTTONGLYPH = 7;
  TS_NORMAL = 1;
begin
  case Panel.Index of
    spHiddenFilesCount:
      if MemosTabSet.Visible and (FHiddenFiles.Count > 0) then begin
        var RText := Rect;
        if FToolbarThemeData <> 0 then begin
          Dec(RText.Right, RText.Bottom - RText.Top);
          var RGlyph := Rect;
          RGlyph.Left := RText.Right; { RGlyph is now a square }
          DrawThemeBackground(FToolbarThemeData, StatusBar.Canvas.Handle, TP_DROPDOWNBUTTONGLYPH, TS_NORMAL, RGlyph, nil);
        end;
        var S := Format('Tabs closed: %d', [FHiddenFiles.Count]);
        StatusBar.Canvas.TextRect(RText, S, [tfCenter]);
      end;
    spCompileIcon:
      if FCompiling then begin
        var BuildImageList := ImagesModule.BuildImageList;
        ImageList_Draw(BuildImageList.Handle, FBuildAnimationFrame, StatusBar.Canvas.Handle,
          Rect.Left + ((Rect.Right - Rect.Left) - BuildImageList.Width) div 2,
          Rect.Top + ((Rect.Bottom - Rect.Top) - BuildImageList.Height) div 2, ILD_NORMAL);
      end;
    spCompileProgress:
      if FCompiling and (FProgressMax > 0) then begin
        var R := Rect;
        InflateRect(R, -2, -2);
        if FProgressThemeData = 0 then begin
          { Border }
          StatusBar.Canvas.Pen.Color := clBtnShadow;
          StatusBar.Canvas.Brush.Style := bsClear;
          StatusBar.Canvas.Rectangle(R);
          InflateRect(R, -1, -1);
          { Filled part }
          var SaveRight := R.Right;
          R.Right := R.Left + MulDiv(FProgress, R.Right - R.Left,
            FProgressMax);
          StatusBar.Canvas.Brush.Color := clHighlight;
          StatusBar.Canvas.FillRect(R);
          { Unfilled part }
          R.Left := R.Right;
          R.Right := SaveRight;
          StatusBar.Canvas.Brush.Color := clBtnFace;
          StatusBar.Canvas.FillRect(R);
        end else begin
          DrawThemeBackground(FProgressThemeData, StatusBar.Canvas.Handle,
            PP_BAR, 0, R, nil);
          { PP_FILL drawing on Windows 11 (and probably 10) is bugged: when
            the width of the green bar is less than ~25 pixels, the bar is
            drawn over the left border. The same thing happens with
            TProgressBar, so I don't think the API is being used incorrectly.
            Work around the bug by passing a clipping rectangle that excludes
            the left edge when running on Windows 10/11 only. (I don't know if
            earlier versions need it, or if later versions will fix it.) }
          var CR := R;
          if (Win32MajorVersion = 10) and (Win32MinorVersion = 0) then
            Inc(CR.Left);  { does this need to be DPI-scaled? }
          R.Right := R.Left + MulDiv(FProgress, R.Right - R.Left,
            FProgressMax);
          DrawThemeBackground(FProgressThemeData, StatusBar.Canvas.Handle,
            PP_FILL, PBFS_NORMAL, R, @CR);
        end;
      end;
  end;
end;

procedure TMainForm.InvalidateStatusPanel(const Index: Integer);
var
  R: TRect;
begin
  { For some reason, the VCL doesn't offer a method for this... }
  if SendMessage(StatusBar.Handle, SB_GETRECT, Index, LPARAM(@R)) <> 0 then begin
    InflateRect(R, -1, -1);
    InvalidateRect(StatusBar.Handle, @R, True);
  end;
end;

procedure TMainForm.UpdateCompileStatusPanels(const AProgress,
  AProgressMax: Cardinal; const ASecondsRemaining: Integer;
  const ABytesCompressedPerSecond: Cardinal);
begin
  var CurTick := GetTickCount;
  var LastTick := FLastAnimationTick;
  FLastAnimationTick := CurTick;

  { Icon and text panels - updated every 500ms }
  if CurTick div 500 <> LastTick div 500 then begin
    InvalidateStatusPanel(spCompileIcon);
    FBuildAnimationFrame := (FBuildAnimationFrame + 1) mod 4;
    if ASecondsRemaining >= 0 then
      StatusBar.Panels[spExtraStatus].Text := Format(
        ' Estimated time remaining: %.2d%s%.2d%s%.2d     Average KB/sec: %.0n',
        [(ASecondsRemaining div 60) div 60, FormatSettings.TimeSeparator,
         (ASecondsRemaining div 60) mod 60, FormatSettings.TimeSeparator,
         ASecondsRemaining mod 60, ABytesCompressedPerSecond / 1024])
    else
      StatusBar.Panels[spExtraStatus].Text := '';
  end;

  { Progress panel and taskbar progress bar - updated every 100ms }
  if (CurTick div 100 <> LastTick div 100) and
     ((FProgress <> AProgress) or (FProgressMax <> AProgressMax)) then begin
    FProgress := AProgress;
    FProgressMax := AProgressMax;
    InvalidateStatusPanel(spCompileProgress);

    { The taskbar progress updates are slow (on Windows 11). Limiting the
      range to 64 instead of 1024 improved compression KB/sec by about 4%
      (9000 to 9400) when the rate limit above is disabled. }
    var NewValue: Cardinal := 1;  { must be at least 1 for progress bar to show }
    if AProgressMax > 0 then begin
      { Not using MulDiv here to avoid rounding up }
      NewValue := (AProgress * 64) div AProgressMax;
      if NewValue = 0 then
        NewValue := 1;
    end;
    { Don't call the function if the value hasn't changed, just in case there's
      a performance penalty. (There doesn't appear to be on Windows 11.) }
    if FTaskbarProgressValue <> NewValue then begin
      FTaskbarProgressValue := NewValue;
      SetAppTaskbarProgressValue(NewValue, 64);
    end;
  end;
end;

procedure TMainForm.WMSettingChange(var Message: TMessage);
begin
  inherited;
  if (FTheme.Typ <> ttClassic) and IsWindows10 and (Message.LParam <> 0) and (StrIComp(PChar(Message.LParam), 'ImmersiveColorSet') = 0) then begin
    FOptions.ThemeType := GetDefaultThemeType;
    UpdateTheme;
  end;
  for var Memo in FMemos do
    Memo.SettingChange(Message);
end;

procedure TMainForm.WMThemeChanged(var Message: TMessage);
begin
  { Don't Run to Cursor into this function, it will interrupt up the theme change }
  UpdateThemeData(True);
  inherited;
end;

procedure TMainForm.WMUAHDrawMenu(var Message: TMessage);
begin
  if FTheme.Dark then begin
    var MenuBarInfo: TMenuBarInfo;
    MenuBarInfo.cbSize := SizeOf(MenuBarInfo);
    GetMenuBarInfo(Handle, Integer(OBJID_MENU), 0, MenuBarInfo);

    var WindowRect: TRect;
    GetWindowRect(Handle, WindowRect);

    var Rect := MenuBarInfo.rcBar;
    OffsetRect(Rect, -WindowRect.Left, -WindowRect.Top);

    var UAHMenu := PUAHMenu(Message.lParam);
    FillRect(UAHMenu.hdc, Rect, FMenuDarkBackgroundBrush.Handle);
  end else
    inherited;
end;

procedure TMainForm.WMUAHDrawMenuItem(var Message: TMessage);
const
  ODS_NOACCEL = $100;
  DTT_TEXTCOLOR = 1;
  MENU_BARITEM = 8;
  MBI_NORMAL = 1;
var
  Buffer: array of Char;
begin
  if FTheme.Dark then begin
    var UAHDrawMenuItem := PUAHDrawMenuItem(Message.lParam);

    var MenuItemInfo: TMenuItemInfo;
    MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
    MenuItemInfo.fMask := MIIM_STRING;
    MenuItemInfo.dwTypeData := nil;
    GetMenuItemInfo(UAHDrawMenuItem.um.hmenu, UAHDrawMenuItem.umi.iPosition, True, MenuItemInfo);
    Inc(MenuItemInfo.cch);
    SetLength(Buffer, MenuItemInfo.cch);
    MenuItemInfo.dwTypeData := @Buffer[0];
    GetMenuItemInfo(UAHDrawMenuItem.um.hmenu, UAHDrawMenuItem.umi.iPosition, True, MenuItemInfo);

    var dwFlags: DWORD := DT_CENTER or DT_SINGLELINE or DT_VCENTER;
    if (UAHDrawMenuItem.dis.itemState and ODS_NOACCEL) <> 0 then
      dwFlags := dwFlags or DT_HIDEPREFIX;

    var Inactive := (UAHDrawMenuItem.dis.itemState and ODS_INACTIVE) <> 0;

    var TextColor: TThemeColor;
    if Inactive then
      TextColor := tcMarginFore
    else
      TextColor := tcFore;

    var opts: TDTTOpts;
    opts.dwSize := SizeOf(opts);
    opts.dwFlags := DTT_TEXTCOLOR;
    opts.crText := FTheme.Colors[TextColor];

    var Brush: HBrush;
    { ODS_HOTLIGHT can be set when the menu is inactive so we check Inactive as well. }
    if not Inactive and ((UAHDrawMenuItem.dis.itemState and (ODS_HOTLIGHT or ODS_SELECTED)) <> 0) then
      Brush := FMenuDarkHotOrSelectedBrush.Handle
    else
      Brush := FMenuDarkBackgroundBrush.Handle;

    FillRect(UAHDrawMenuItem.um.hdc, UAHDrawMenuItem.dis.rcItem, Brush);
    DrawThemeTextEx(FMenuThemeData, UAHDrawMenuItem.um.hdc, MENU_BARITEM, MBI_NORMAL, MenuItemInfo.dwTypeData, MenuItemInfo.cch, dwFlags, @UAHDrawMenuItem.dis.rcItem, opts);
  end else
    inherited;
end;

{ Should be removed if the main menu ever gets removed }
procedure TMainForm.UAHDrawMenuBottomLine;
begin
  if FTheme.Dark then begin
    var ClientRect: TRect;
    Windows.GetClientRect(Handle, ClientRect);
		MapWindowPoints(Handle, 0, ClientRect, 2);

    var WindowRect: TRect;
    GetWindowRect(Handle, WindowRect);

    var Rect := ClientRect;
    OffsetRect(Rect, -WindowRect.Left, -WindowRect.Top);

    Rect.Bottom := Rect.Top;
    Dec(Rect.Top);

    var DC := GetWindowDC(Handle);
  	FillRect(DC, Rect, FMenuDarkBackgroundBrush.Handle);
		ReleaseDC(Handle, DC);
  end;
end;

procedure TMainForm.WMNCActivate(var Message: TMessage);
begin
  inherited;
  UAHDrawMenuBottomLine;
end;

procedure TMainForm.WMNCPaint(var Message: TMessage);
begin
  inherited;
  UAHDrawMenuBottomLine;
end;

procedure TMainForm.RTargetClick(Sender: TObject);
var
  NewTarget: TDebugTarget;
begin
  if (Sender = RTargetSetup) or (Sender = TargetSetupButton) then
    NewTarget := dtSetup
  else
    NewTarget := dtUninstall;
  if (FDebugTarget <> NewTarget) and (not FDebugging or AskToDetachDebugger) then
    FDebugTarget := NewTarget;

  { Update always even if the user decided not to switch so the states are restored }
  UpdateTargetMenu;
end;

procedure TMainForm.AppOnActivate(Sender: TObject);
const
  ReloadMessages: array[Boolean] of String = (
    'The %s file has been modified outside of the source editor.' + SNewLine2 +
      'Do you want to reload the file?',
    'The %s file has been modified outside of the source editor. Changes have ' +
      'also been made in the source editor.' + SNewLine2 + 'Do you want to ' +
      'reload the file and lose the changes made in the source editor?');
var
  Memo: TIDEScintFileEdit;
  NewTime: TFileTime;
  Changed: Boolean;
begin
  for Memo in FFileMemos do begin
    if (Memo.Filename = '') or not Memo.Used then
      Continue;

    { See if the file has been modified outside the editor }
    Changed := False;
    if GetLastWriteTimeOfFile(Memo.Filename, @NewTime) then begin
      if CompareFileTime(Memo.FileLastWriteTime, NewTime) <> 0 then begin
        Memo.FileLastWriteTime := NewTime;
        Changed := True;
      end;
    end;

    { If it has been, offer to reload it }
    if Changed then begin
      if IsWindowEnabled(Handle) then begin
        if MsgBox(Format(ReloadMessages[Memo.Modified], [Memo.Filename]),
           SCompilerFormCaption, mbConfirmation, MB_YESNO) = IDYES then
          if ConfirmCloseFile(False) then begin
            OpenFile(Memo, Memo.Filename, False);
            if Memo = FMainMemo then
              Break; { Reloading the main script will also reload all include files }
          end;
      end
      else begin
        { When a modal dialog is up, don't offer to reload the file. Probably
          not a good idea since the dialog might be manipulating the file. }
        MsgBox('The ' + Memo.Filename + ' file has been modified outside ' +
          'of the source editor. You might want to reload it.',
          SCompilerFormCaption, mbInformation, MB_OK);
      end;
    end;
  end;
end;

procedure TMainForm.CompilerOutputListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  ThemeColors: array [TStatusMessageKind] of TThemeColor = (tcGreen, tcFore, tcOrange, tcRed);
var
  Canvas: TCanvas;
  S: String;
  StatusMessageKind: TStatusMessageKind;
begin
  Canvas := CompilerOutputList.Canvas;
  S := CompilerOutputList.Items[Index];

  Canvas.FillRect(Rect);
  Inc(Rect.Left, 2);
  if FOptions.ColorizeCompilerOutput and not (odSelected in State) then begin
    StatusMessageKind := TStatusMessageKind(CompilerOutputList.Items.Objects[Index]);
    Canvas.Font.Color := FTheme.Colors[ThemeColors[StatusMessageKind]];
  end;
  Canvas.TextOut(Rect.Left, Rect.Top, S);
end;

procedure TMainForm.DebugOutputListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Canvas: TCanvas;
  S: String;
begin
  Canvas := DebugOutputList.Canvas;
  S := DebugOutputList.Items[Index];

  Canvas.FillRect(Rect);
  Inc(Rect.Left, 2);
  if (S <> '') and (S[1] = #9) then
    Canvas.TextOut(Rect.Left + FDebugLogListTimestampsWidth, Rect.Top, Copy(S, 2, Maxint))
  else begin
    if (Length(S) > 20) and (S[18] = '-') and (S[19] = '-') and (S[20] = ' ') then begin
      { Draw lines that begin with '-- ' (like '-- File entry --') in bold }
      Canvas.TextOut(Rect.Left, Rect.Top, Copy(S, 1, 17));
      Canvas.Font.Style := [fsBold];
      Canvas.TextOut(Rect.Left + FDebugLogListTimestampsWidth, Rect.Top, Copy(S, 18, Maxint));
    end else
      Canvas.TextOut(Rect.Left, Rect.Top, S);
  end;
end;

procedure TMainForm.DebugCallStackListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Canvas: TCanvas;
  S: String;
begin
  Canvas := DebugCallStackList.Canvas;
  S := DebugCallStackList.Items[Index];

  Canvas.FillRect(Rect);
  Inc(Rect.Left, 2);
  Canvas.TextOut(Rect.Left, Rect.Top, S);
end;

procedure TMainForm.FindResultsListDblClick(Sender: TObject);
var
  FindResult: TFindResult;
  Memo: TIDEScintFileEdit;
  I: Integer;
begin
  I := FindResultsList.ItemIndex;
  if I <> -1 then begin
    FindResult := FindResultsList.Items.Objects[I] as TFindResult;
    if FindResult <> nil then begin
      for Memo in FFileMemos do begin
        if Memo.Used and (PathCompare(Memo.Filename, FindResult.Filename) = 0) then begin
          MoveCaretAndActivateMemo(Memo, FindResult.Line, True);
          Memo.SelectAndEnsureVisible(FindResult.Range);
          ActiveControl := Memo;
          Exit;
        end;
      end;
      MsgBox('File not opened.', SCompilerFormCaption, mbError, MB_OK);
    end;
  end;
end;

procedure TMainForm.FindResultsListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Canvas: TCanvas;
  S, S2: String;
  FindResult: TFindResult;
  StartI, EndI: Integer;
  SaveColor: TColor;
begin
  Canvas := FindResultsList.Canvas;
  S := FindResultsList.Items[Index];
  FindResult := FindResultsList.Items.Objects[Index] as TFindResult;

  Canvas.FillRect(Rect);
  Inc(Rect.Left, 2);
  if FindResult = nil then begin
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(Rect.Left, Rect.Top, S);
  end else if not (odSelected in State) then begin
    StartI := FindResult.Range.StartPos - FindResult.LineStartPos + 1 + FindResult.PrefixStringLength;
    EndI := FindResult.Range.EndPos - FindResult.LineStartPos + 1 + FindResult.PrefixStringLength;
    if StartI > 1 then begin
      Canvas.TextOut(Rect.Left, Rect.Top, Copy(S, 1, StartI-1));
      Rect.Left := Canvas.PenPos.X;
    end;
    SaveColor := Canvas.Brush.Color;
    if FTheme.Dark then
      Canvas.Brush.Color := FTheme.Colors[tcRed]
    else
      Canvas.Brush.Color := FTheme.Colors[tcSelBack];
    S2 := Copy(S, StartI, EndI-StartI);
    Rect.Right := Rect.Left + Canvas.TextWidth(S2);
    Canvas.TextRect(Rect, Rect.Left, Rect.Top, S2); { TextRect instead of TextOut to avoid a margin around the text }
    if EndI <= Length(S) then begin
      Canvas.Brush.Color := SaveColor;
      S2 := Copy(S, EndI, MaxInt);
      Rect.Left := Rect.Right;
      Rect.Right := Rect.Left + Canvas.TextWidth(S2);
      Canvas.TextRect(Rect, Rect.Left, Rect.Top, S2);
    end;
  end else
    Canvas.TextOut(Rect.Left, Rect.Top, S)
end;

procedure TMainForm.OutputTabSetClick(Sender: TObject);
begin
  case OutputTabSet.TabIndex of
    tiCompilerOutput:
      begin
        CompilerOutputList.BringToFront;
        CompilerOutputList.Visible := True;
        DebugOutputList.Visible := False;
        DebugCallStackList.Visible := False;
        FindResultsList.Visible := False;
      end;
    tiDebugOutput:
      begin
        DebugOutputList.BringToFront;
        DebugOutputList.Visible := True;
        CompilerOutputList.Visible := False;
        DebugCallStackList.Visible := False;
        FindResultsList.Visible := False;
      end;
    tiDebugCallStack:
      begin
        DebugCallStackList.BringToFront;
        DebugCallStackList.Visible := True;
        CompilerOutputList.Visible := False;
        DebugOutputList.Visible := False;
        FindResultsList.Visible := False;
      end;
    tiFindResults:
      begin
        FindResultsList.BringToFront;
        FindResultsList.Visible := True;
        CompilerOutputList.Visible := False;
        DebugOutputList.Visible := False;
        DebugCallStackList.Visible := False;
      end;
  end;
end;

procedure TMainForm.ToggleBreakPoint(Line: Integer);
var
  Memo: TIDEScintFileEdit;
  I: Integer;
begin
  Memo := FActiveMemo as TIDEScintFileEdit;
  I := Memo.BreakPoints.IndexOf(Line);
  if I = -1 then
    Memo.BreakPoints.Add(Line)
  else
    Memo.BreakPoints.Delete(I);
  UpdateLineMarkers(Memo, Line);
  BuildAndSaveBreakPointLines(Memo);
end;

procedure TMainForm.MemoMarginClick(Sender: TObject; MarginNumber: Integer;
  Line: Integer);
begin
  if (MarginNumber = 1) and RToggleBreakPoint.Enabled then
    ToggleBreakPoint(Line);
end;

procedure TMainForm.MemoMarginRightClick(Sender: TObject; MarginNumber: Integer;
  Line: Integer);
begin
  if MarginNumber = 1 then begin
    var Point := SmallPointToPoint(TSmallPoint(GetMessagePos()));
    var PopupMenu := TMainFormPopupMenu.Create(Self, BreakPointsPopupMenu);
    try
      PopupMenu.Popup(Point.X, Point.Y);
    finally
      PopupMenu.Free;
    end;
  end;
end;

procedure TMainForm.RToggleBreakPointClick(Sender: TObject);
begin
  ToggleBreakPoint(FActiveMemo.CaretLine);
end;

procedure TMainForm.RDeleteBreakPointsClick(Sender: TObject);
begin
  { Also see AnyMemoHasBreakPoint }
  for var Memo in FFileMemos do begin
    if Memo.Used and (Memo.BreakPoints.Count > 0) then begin
      for var I := Memo.BreakPoints.Count-1 downto 0 do begin
        var Line := Memo.BreakPoints[I];
        Memo.BreakPoints.Delete(I);
        UpdateLineMarkers(Memo, Line);
      end;
      BuildAndSaveBreakPointLines(Memo);
    end;
  end;
end;

procedure TMainForm.MemoLinesInserted(Memo: TIDEScintFileEdit; FirstLine, Count: integer);
var
  I, Line: Integer;
begin
  for I := 0 to FDebugEntriesCount-1 do
    if (FDebugEntries[I].FileIndex = Memo.CompilerFileIndex) and
       (FDebugEntries[I].LineNumber >= FirstLine) then
      Inc(FDebugEntries[I].LineNumber, Count);

  if Assigned(Memo.LineState) and (FirstLine < Memo.LineStateCount) then begin
    { Grow FStateLine if necessary }
    I := (Memo.LineStateCount + Count) - Memo.LineStateCapacity;
    if I > 0 then begin
      if I < LineStateGrowAmount then
        I := LineStateGrowAmount;
      ReallocMem(Memo.LineState, SizeOf(TLineState) * (Memo.LineStateCapacity + I));
      Inc(Memo.LineStateCapacity, I);
    end;
    { Shift existing line states and clear the new ones }
    for I := Memo.LineStateCount-1 downto FirstLine do
      Memo.LineState[I + Count] := Memo.LineState[I];
    for I := FirstLine to FirstLine + Count - 1 do
      Memo.LineState[I] := lnUnknown;
    Inc(Memo.LineStateCount, Count);
  end;

  if Memo.StepLine >= FirstLine then
    Inc(Memo.StepLine, Count);
  if Memo.ErrorLine >= FirstLine then
    Inc(Memo.ErrorLine, Count);

  var BreakPointsChanged := False;
  for I := 0 to Memo.BreakPoints.Count-1 do begin
    Line := Memo.BreakPoints[I];
    if Line >= FirstLine then begin
      Memo.BreakPoints[I] := Line + Count;
      BreakPointsChanged := True;
    end;
  end;
  if BreakPointsChanged then
    BuildAndSaveBreakPointLines(Memo);

  FNavStacks.LinesInserted(Memo, FirstLine, Count);
end;

procedure TMainForm.MemoLinesDeleted(Memo: TIDEScintFileEdit; FirstLine, Count,
  FirstAffectedLine: Integer);
var
  I, Line: Integer;
  DebugEntry: PDebugEntry;
begin
  for I := 0 to FDebugEntriesCount-1 do begin
    DebugEntry := @FDebugEntries[I];
    if (DebugEntry.FileIndex = Memo.CompilerFileIndex) and
       (DebugEntry.LineNumber >= FirstLine) then begin
      if DebugEntry.LineNumber < FirstLine + Count then
        DebugEntry.LineNumber := -1
      else
        Dec(DebugEntry.LineNumber, Count);
    end;
  end;

  if Assigned(Memo.LineState) then begin
    { Shift existing line states }
    if FirstLine < Memo.LineStateCount - Count then begin
      for I := FirstLine to Memo.LineStateCount - Count - 1 do
        Memo.LineState[I] := Memo.LineState[I + Count];
      Dec(Memo.LineStateCount, Count);
    end
    else begin
      { There's nothing to shift because the last line(s) were deleted, or
        line(s) past FLineStateCount }
      if Memo.LineStateCount > FirstLine then
        Memo.LineStateCount := FirstLine;
    end;
  end;

  if Memo.StepLine >= FirstLine then begin
    if Memo.StepLine < FirstLine + Count then
      Memo.StepLine := -1
    else
      Dec(Memo.StepLine, Count);
  end;
  if Memo.ErrorLine >= FirstLine then begin
    if Memo.ErrorLine < FirstLine + Count then
      Memo.ErrorLine := -1
    else
      Dec(Memo.ErrorLine, Count);
  end;

  var BreakPointsChanged := False;
  for I := Memo.BreakPoints.Count-1 downto 0 do begin
    Line := Memo.BreakPoints[I];
    if Line >= FirstLine then begin
      if Line < FirstLine + Count then begin
        Memo.BreakPoints.Delete(I);
        BreakPointsChanged := True;
      end else begin
        Memo.BreakPoints[I] := Line - Count;
        BreakPointsChanged := True;
      end;
    end;
  end;
  if BreakPointsChanged then
    BuildAndSaveBreakPointLines(Memo);

  if FNavStacks.LinesDeleted(Memo, FirstLine, Count) then
    UpdateNavButtons;
  { We do NOT update FCurrentNavItem here so it might point to a line that's
    deleted until next UpdateCaretPosPanelAndBackStack by UpdateMemoUI }

  { When lines are deleted, Scintilla insists on moving all of the deleted
    lines' markers to the line on which the deletion started
    (FirstAffectedLine). This is bad for us as e.g. it can result in the line
    having two conflicting markers (or two of the same marker). There's no
    way to stop it from doing that, or to easily tell which markers came from
    which lines, so we simply delete and re-create all markers on the line. }
  UpdateLineMarkers(Memo, FirstAffectedLine);
end;

procedure TMainForm.UpdateLineMarkers(const AMemo: TIDEScintFileEdit; const Line: Integer);
var
  NewMarker: Integer;
begin
  if Line >= AMemo.Lines.Count then
    Exit;

  var StepLine := AMemo.StepLine = Line;

  NewMarker := -1;
  if AMemo.BreakPoints.IndexOf(Line) <> -1 then begin
    if AMemo.LineState = nil then
      NewMarker := mmiBreakpoint
    else if (Line < AMemo.LineStateCount) and (AMemo.LineState[Line] <> lnUnknown) then
      NewMarker := IfThen(StepLine, mmiBreakpointStep, mmiBreakpointGood)
    else
      NewMarker := mmiBreakpointBad;
  end else if StepLine then
    NewMarker := mmiStep
  else begin
    if Line < AMemo.LineStateCount then begin
      case AMemo.LineState[Line] of
        lnHasEntry: NewMarker := mmiHasEntry;
        lnEntryProcessed: NewMarker := mmiEntryProcessed;
      end;
    end;
  end;

  { Delete all markers on the line. To flush out any possible duplicates,
    even the markers we'll be adding next are deleted. }
  if AMemo.GetMarkers(Line) <> [] then
    AMemo.DeleteAllMarkersOnLine(Line);

  if NewMarker <> -1 then
    AMemo.AddMarker(Line, NewMarker);

  if StepLine then
    AMemo.AddMarker(Line, mlmStep)
  else if AMemo.ErrorLine = Line then
    AMemo.AddMarker(Line, mlmError)
  else if NewMarker = mmiBreakpointBad then
    AMemo.AddMarker(Line, mlmBreakpointBad);
end;

procedure TMainForm.UpdateLinkLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  var Handled := True;
  if (LinkType = sltID) and (Link = 'hwhatsnew') then
    HWhatsNew.Click
  else if (LinkType = sltID) and (Link = 'toptions-vscode') then begin
    TOptionsForm.DropDownMemoKeyMappingComboBoxOnNextShow := True;
    TOptions.Click
  end else
    Handled := False;
  if Handled then
    UpdatePanelClosePaintBoxClick(Sender);
end;

procedure TMainForm.UpdatePanelClosePaintBoxClick(Sender: TObject);
begin
  var MessageToHideIndex := UpdateLinkLabel.Tag;
  var Ini := TConfigIniFile.Create;
  try
    Ini.WriteInteger('UpdatePanel', FUpdatePanelMessages[MessageToHideIndex].ConfigIdent, FUpdatePanelMessages[MessageToHideIndex].ConfigValue);
  finally
    Ini.Free;
  end;
  FUpdatePanelMessages.Delete(MessageToHideIndex);
  UpdateUpdatePanel;
end;

procedure TMainForm.UpdatePanelDonateImageClick(Sender: TObject);
begin
  HDonate.Click;
end;

procedure TMainForm.UpdatePanelClosePaintBoxPaint(Sender: TObject);
const
  MENU_SYSTEMCLOSE = 17;
  MSYSC_NORMAL = 1;
begin
  var Canvas := UpdatePanelClosePaintBox.Canvas;
  var R := TRect.Create(0, 0, UpdatePanelClosePaintBox.Width, UpdatePanelClosePaintBox.Height);
  if FMenuThemeData <> 0 then begin
    var Offset := MulDiv(1, CurrentPPI, 96);
    Inc(R.Left, Offset);
    DrawThemeBackground(FMenuThemeData, Canvas.Handle, MENU_SYSTEMCLOSE, MSYSC_NORMAL, R, nil);
  end else begin
    InflateRect(R, -MulDiv(6, CurrentPPI, 96), -MulDiv(6, CurrentPPI, 96));
    Canvas.Pen.Color := Canvas.Font.Color;
    Canvas.MoveTo(R.Left, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.MoveTo(R.Left, R.Bottom-1);
    Canvas.LineTo(R.Right, R.Top-1);
  end;
end;

procedure TMainForm.UpdateAllMemoLineMarkers(const AMemo: TIDEScintFileEdit);
begin
  for var Line := 0 to AMemo.Lines.Count-1 do
    UpdateLineMarkers(AMemo, Line);
end;

procedure TMainForm.UpdateAllMemosLineMarkers;
begin
  for var Memo in FFileMemos do
    if Memo.Used then
      UpdateAllMemoLineMarkers(Memo);
end;

procedure TMainForm.UpdateBevel1Visibility;
begin
  { Bevel1 is the line between the toolbar and memos when there's nothing in
    between and the color of the toolbar and memo margins is the same }
  Bevel1.Visible := (ToolBarPanel.Color = FTheme.Colors[tcMarginBack]) and
                    not UpdatePanel.Visible and not MemosTabSet.Visible;
end;

function TMainForm.ToCurrentPPI(const XY: Integer): Integer;
begin
  Result := MulDiv(XY, CurrentPPI, 96);
end;

function TMainForm.FromCurrentPPI(const XY: Integer): Integer;
begin
  Result := MulDiv(XY, 96, CurrentPPI);
end;

initialization
  Application.OnGetActiveFormHandle := TMainForm.AppOnGetActiveFormHandle;
  InitThemeLibrary;
  InitHtmlHelpLibrary;
  { For ClearType support, try to make the default font Microsoft Sans Serif }
  if DefFontData.Name = 'MS Sans Serif' then
    DefFontData.Name := AnsiString(GetPreferredUIFont);
  CoInitialize(nil);
finalization
  CoUninitialize();
end.
