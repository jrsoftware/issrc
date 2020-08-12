unit CompForm;

{
  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler form
}

{x$DEFINE STATICCOMPILER}
{ For debugging purposes, remove the 'x' to have it link the compiler code
  into this program and not depend on ISCmplr.dll. }

{$I VERSION.INC}

{$IFDEF IS_D6}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls, Forms, Dialogs,
  Generics.Collections, UIStateForm, StdCtrls, ExtCtrls, Menus, Buttons, ComCtrls, CommCtrl,
  ScintInt, ScintEdit, ScintStylerInnoSetup, NewTabSet, ModernColors, CompScintEdit,
  DebugStruct, CompInt, UxTheme, ImageList, ImgList, ToolWin, CompFunc,
  VirtualImageList, BaseImageCollection, ImageCollection;

const
  WM_StartCommandLineCompile = WM_USER + $1000;
  WM_StartCommandLineWizard = WM_USER + $1001;
  WM_StartNormally = WM_USER + $1002;

type
  TLineState = (lnUnknown, lnHasEntry, lnEntryProcessed);
  PLineStateArray = ^TLineStateArray;
  TLineStateArray = array[0..0] of TLineState;
  PDebugEntryArray = ^TDebugEntryArray;
  TDebugEntryArray = array[0..0] of TDebugEntry;
  PVariableDebugEntryArray = ^TVariableDebugEntryArray;
  TVariableDebugEntryArray = array[0..0] of TVariableDebugEntry;
  TStepMode = (smRun, smStepInto, smStepOver, smRunToCursor);
  TDebugTarget = (dtSetup, dtUninstall);

const
  DebugTargetStrings: array[TDebugTarget] of String = ('Setup', 'Uninstall');

type
  TStatusMessageKind = (smkStartEnd, smkNormal, smkWarning, smkError);

  TCompMainScintEdit = class(TCompScintEdit)
  private
    ErrorLine, StepLine: Integer;
    ErrorCaretPosition: Integer;
    BreakPoints: TList;
    LineState: PLineStateArray;
    LineStateCapacity, LineStateCount: Integer;
  end;

  TIncludedFile = class
    Filename: String;
    LastWriteTime: TFileTime;
    HasLastWriteTime: Boolean;
    Memo: TCompScintEdit;
  end;

  TIncludedFiles = TObjectList<TIncludedFile>;

  TCompileForm = class(TUIStateForm)
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
    N6: TMenuItem;
    HAbout: TMenuItem;
    FMRUMainFilesSep: TMenuItem;
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
    ListPopupMenu: TPopupMenu;
    PListCopy: TMenuItem;
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
    HPSWebsite: TMenuItem;
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
    ECompleteWord: TMenuItem;
    N19: TMenuItem;
    FSaveEncoding: TMenuItem;
    FSaveEncodingAuto: TMenuItem;
    FSaveEncodingUTF8: TMenuItem;
    ToolBar: TToolBar;
    NewMainFileButton: TToolButton;
    OpenMainFileButton: TToolButton;
    SaveButton: TToolButton;
    ToolButton4: TToolButton;
    CompileButton: TToolButton;
    StopCompileButton: TToolButton;
    ToolButton7: TToolButton;
    RunButton: TToolButton;
    PauseButton: TToolButton;
    ToolButton10: TToolButton;
    TargetSetupButton: TToolButton;
    TargetUninstallButton: TToolButton;
    ToolButton13: TToolButton;
    HelpButton: TToolButton;
    Bevel1: TBevel;
    BuildImageList: TImageList;
    TerminateButton: TToolButton;
    LightToolBarImageCollection: TImageCollection;
    DarkToolBarImageCollection: TImageCollection;
    ToolBarVirtualImageList: TVirtualImageList;
    PListSelectAll: TMenuItem;
    DebugCallStackList: TListBox;
    VDebugCallStack: TMenuItem;
    TInsertMsgBox: TMenuItem;
    ToolBarPanel: TPanel;
    HMailingList: TMenuItem;
    MemosTabSet: TNewTabSet;
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
    procedure EFindNextClick(Sender: TObject);
    procedure SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure VMenuClick(Sender: TObject);
    procedure HWebsiteClick(Sender: TObject);
    procedure VToolbarClick(Sender: TObject);
    procedure TOptionsClick(Sender: TObject);
    procedure HFaqClick(Sender: TObject);
    procedure HPSWebsiteClick(Sender: TObject);
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
    procedure PListCopyClick(Sender: TObject);
    procedure BStopCompileClick(Sender: TObject);
    procedure HMenuClick(Sender: TObject);
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
    procedure ECompleteWordClick(Sender: TObject);
    procedure FSaveEncodingItemClick(Sender: TObject);
    procedure CompilerOutputListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure PListSelectAllClick(Sender: TObject);
    procedure DebugCallStackListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure VDebugCallStackClick(Sender: TObject);
    procedure HMailingListClick(Sender: TObject);
    procedure TInsertMsgBoxClick(Sender: TObject);
    procedure MemosTabSetClick(Sender: TObject);
  private
    { Private declarations }
    FMemos: TList<TCompScintEdit>; { FMemos[0] is always the main memo }
    FMainMemo: TCompMainScintEdit;
    FActiveMemo: TCompScintEdit;
    FMemosStyler: TInnoSetupStyler;
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
      AutoComplete: Boolean;
      UseSyntaxHighlighting: Boolean;
      ColorizeCompilerOutput: Boolean;
      UnderlineErrors: Boolean;
      CursorPastEOL: Boolean;
      TabWidth: Integer;
      UseTabCharacter: Boolean;
      WordWrap: Boolean;
      AutoIndent: Boolean;
      IndentationGuides: Boolean;
      LowPriorityDuringCompile: Boolean;
      GutterLineNumbers: Boolean;
      ThemeType: TThemeType;
      OpenIncludedFiles: Boolean;
    end;
    FOptionsLoaded: Boolean;
    FTheme: TTheme;
    FSignTools: TStringList;
    FCompiling: Boolean;
    FCompileWantAbort: Boolean;
    FBecameIdle: Boolean;
    FModifiedSinceLastCompile, FModifiedSinceLastCompileAndGo: Boolean;
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
    FIncludedFiles: TIncludedFiles;
    FLoadingIncludedFiles: Boolean;
    FDebugging: Boolean;
    FStepMode: TStepMode;
    FPaused: Boolean;
    FRunToCursorPoint: TDebugEntry;
    FReplyString: String;
    FDebuggerException: String;
    FRunParameters: String;
    FLastFindOptions: TFindOptions;
    FLastFindText: String;
    FLastReplaceText: String;
    FLastEvaluateConstantText: String;
    FSavePriorityClass: DWORD;
    FBuildAnimationFrame: Cardinal;
    FLastAnimationTick: DWORD;
    FProgress, FProgressMax: Cardinal;
    FProgressThemeData: HTHEME;
    FProgressChunkSize, FProgressSpaceSize: Integer;
    FDebugLogListTimestampsWidth: Integer;
    FOnPendingSquiggly: Boolean;
    FPendingSquigglyCaretPos: Integer;
    FCallStackCount: Cardinal;
    class procedure AppOnException(Sender: TObject; E: Exception);
    procedure AppOnActivate(Sender: TObject);
    procedure AppOnIdle(Sender: TObject; var Done: Boolean);
    function AskToDetachDebugger: Boolean;
    procedure BringToForeground;
    procedure CheckIfTerminated;
    procedure CompileFile(AFilename: String; const ReadFromFile: Boolean);
    procedure CompileIfNecessary;
    function ConfirmCloseFile(const PromptToSave: Boolean): Boolean;
    procedure DebuggingStopped(const WaitForTermination: Boolean);
    procedure DebugLogMessage(const S: String);
    procedure DebugShowCallStack(const CallStack: String; const CallStackCount: Cardinal);
    procedure DestroyDebugInfo;
    procedure DetachDebugger;
    function EvaluateConstant(const S: String; var Output: String): Integer;
    function EvaluateVariableEntry(const DebugEntry: PVariableDebugEntry;
      var Output: String): Integer;
    procedure FindNext;
    function FromCurrentPPI(const XY: Integer): Integer;
    procedure Go(AStepMode: TStepMode);
    procedure HideMainMemoError;
    procedure InitializeFindText(Dlg: TFindDialog);
    function InitializeMainMemo(const Memo: TCompMainScintEdit; const PopupMenu: TPopupMenu): TCompMainScintEdit;
    function InitializeMemo(const Memo: TCompScintEdit; const PopupMenu: TPopupMenu): TCompScintEdit;
    procedure InitiateAutoComplete(const Key: AnsiChar);
    procedure InvalidateStatusPanel(const Index: Integer);
    procedure MemoChange(Sender: TObject; const Info: TScintEditChangeInfo);
    procedure MemoCharAdded(Sender: TObject; Ch: AnsiChar);
    procedure MemoDropFiles(Sender: TObject; X, Y: Integer; AFiles: TStrings);
    procedure MainMemoHintShow(Sender: TObject; var Info: TScintHintInfo);
    procedure MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MemoKeyPress(Sender: TObject; var Key: Char);
    procedure MainMemoLinesDeleted(FirstLine, Count, FirstAffectedLine: Integer);
    procedure MainMemoLinesInserted(FirstLine, Count: integer);
    procedure MainMemoMarginClick(Sender: TObject; MarginNumber: Integer;
      Line: Integer);
    procedure MemoModifiedChange(Sender: TObject);
    procedure MemoUpdateUI(Sender: TObject);
    procedure ModifyMRUMainFilesList(const AFilename: String; const AddNewItem: Boolean);
    procedure ModifyMRUParametersList(const AParameter: String; const AddNewItem: Boolean);
    procedure MoveMainMemoCaret(const LineNumber: Integer; const AlwaysResetColumn: Boolean);
    procedure NewMainFile;
    procedure NewMainFileUsingWizard;
    procedure OpenFile(AMemo: TCompScintEdit; AFilename: String; const MainMemoAddToRecentDocs: Boolean);
    procedure OpenMRUMainFile(const AFilename: String);
    procedure ParseDebugInfo(DebugInfo: Pointer);
    procedure ReadMRUMainFilesList;
    procedure ReadMRUParametersList;
    procedure ResetMainMemoLineState;
    procedure StartProcess;
    function SaveFile(const AMemo: TCompScintEdit; const SaveAs: Boolean): Boolean;
    procedure SetMainMemoErrorLine(ALine: Integer);
    procedure SetStatusPanelVisible(const AVisible: Boolean);
    procedure SetMainMemoStepLine(ALine: Integer);
    procedure ShowOpenMainFileDialog(const Examples: Boolean);
    procedure StatusMessage(const Kind: TStatusMessageKind; const S: String);
    procedure SyncEditorOptions;
    procedure SyncZoom;
    function ToCurrentPPI(const XY: Integer): Integer;
    procedure MainMemoToggleBreakPoint(Line: Integer);
    procedure UpdateAllMainMemoLineMarkers;
    procedure UpdateBevel1;
    procedure UpdateCaption;
    procedure UpdateCaretPosPanel;
    procedure UpdateCompileStatusPanels(const AProgress, AProgressMax: Cardinal;
      const ASecondsRemaining: Integer; const ABytesCompressedPerSecond: Cardinal);
    procedure UpdateEditModePanel;
    procedure UpdateIncludedFiles;
    procedure UpdateMainMemoLineMarkers(const Line: Integer);
    procedure UpdateModifiedPanel;
    procedure UpdateNewMainFileButtons;
    procedure UpdateTabSetListsItemHeightAndDebugTimeWidth;
    procedure UpdateRunMenu;
    procedure UpdateTargetMenu;
    procedure UpdateTheme;
    procedure UpdateThemeData(const Close, Open: Boolean);
    procedure UpdateStatusPanelHeight(H: Integer);
    procedure WMCopyData(var Message: TWMCopyData); message WM_COPYDATA;
    procedure WMDebuggerHello(var Message: TMessage); message WM_Debugger_Hello;
    procedure WMDebuggerGoodbye(var Message: TMessage); message WM_Debugger_Goodbye;
    procedure WMDebuggerQueryVersion(var Message: TMessage); message WM_Debugger_QueryVersion;
    function GetLineNumberFromEntry(Kind, Index: Integer): Integer;
    procedure DebuggerStepped(var Message: TMessage; const Intermediate: Boolean);
    procedure WMDebuggerStepped(var Message: TMessage); message WM_Debugger_Stepped;
    procedure WMDebuggerSteppedIntermediate(var Message: TMessage); message WM_Debugger_SteppedIntermediate;
    procedure WMDebuggerException(var Message: TMessage); message WM_Debugger_Exception;
    procedure WMDebuggerSetForegroundWindow(var Message: TMessage); message WM_Debugger_SetForegroundWindow;
    procedure WMDebuggerCallStackCount(var Message: TMessage); message WM_Debugger_CallStackCount;
    procedure WMStartCommandLineCompile(var Message: TMessage); message WM_StartCommandLineCompile;
    procedure WMStartCommandLineWizard(var Message: TMessage); message WM_StartCommandLineWizard;
    procedure WMStartNormally(var Message: TMessage); message WM_StartNormally;
    procedure WMSettingChange(var Message: TMessage); message WM_SETTINGCHANGE;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
{$IFDEF IS_D4}
  protected
    procedure WndProc(var Message: TMessage); override;
{$ENDIF}
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFDEF IS_D5}
    function IsShortCut(var Message: TWMKey): Boolean; override;
{$ENDIF}
  end;

var
  CompileForm: TCompileForm;
  CommandLineFilename, CommandLineWizardName: String;
  CommandLineCompile: Boolean;
  CommandLineWizard: Boolean;

implementation

uses
  ActiveX, Clipbrd, ShellApi, ShlObj, IniFiles, Registry, Consts, Types,
  PathFunc, CmnFunc, CmnFunc2, FileClass, CompMsgs, TmSchema, BrowseFunc,
  HtmlHelpFunc, TaskbarProgressFunc,
  {$IFDEF STATICCOMPILER} Compile, {$ENDIF}
  CompOptions, CompStartup, CompWizard, CompSignTools, CompTypes, CompInputQueryCombo, CompMessageBoxDesigner;

{$R *.DFM}

const
  { Memos }
  MaxMemos = 11; { Includes the main memo }
  FirstIncludedFilesMemoIndex = 1;

  { Status bar panel indexes }
  spCaretPos = 0;
  spModified = 1;
  spEditMode = 2;
  spCompileIcon = 3;
  spCompileProgress = 4;
  spExtraStatus = 5;

  { Output tab set indexes }
  tiCompilerOutput = 0;
  tiDebugOutput = 1;
  tiDebugCallStack = 2;

  LineStateGrowAmount = 4000;

{ TCompileFormMemoPopupMenu }

type
  TCompileFormMemoPopupMenu = class(TPopupMenu)
  public
    procedure Popup(X, Y: Integer); override;
  end;

{ok}procedure TCompileFormMemoPopupMenu.Popup(X, Y: Integer);
var
  Form: TCompileForm;
begin
  { Show the existing Edit menu }
  Form := Owner as TCompileForm;
  TrackPopupMenu(Form.EMenu.Handle, TPM_RIGHTBUTTON, X, Y, 0, Form.Handle, nil);
end;

function TCompileForm.InitializeMemo(const Memo: TCompScintEdit; const PopupMenu: TPopupMenu): TCompScintEdit;
begin
  Memo.AcceptDroppedFiles := True;
  Memo.Align := alClient;
  Memo.AutoCompleteFontName := Font.Name;
  Memo.AutoCompleteFontSize := Font.Size;
  Memo.CodePage := CP_UTF8;
  Memo.Font.Name := 'Courier New';
  Memo.Font.Size := 10;
  Memo.Styler := FMemosStyler;
  Memo.PopupMenu := PopupMenu;
  Memo.OnChange := MemoChange;
  Memo.OnCharAdded := MemoCharAdded;
  Memo.OnDropFiles := MemoDropFiles;
  Memo.OnKeyDown := MemoKeyDown;
  Memo.OnKeyPress := MemoKeyPress;
  Memo.OnModifiedChange := MemoModifiedChange;
  Memo.OnUpdateUI := MemoUpdateUI;
  Memo.Parent := BodyPanel;
  Memo.Theme := FTheme;
  Memo.Visible := False;
  Result := Memo;
end;

function TCompileForm.InitializeMainMemo(const Memo: TCompMainScintEdit; const PopupMenu: TPopupMenu): TCompMainScintEdit;
begin
  InitializeMemo(Memo, PopupMenu);
  Memo.ShowHint := True;
  Memo.OnMarginClick := MainMemoMarginClick;
  Memo.OnHintShow := MainMemoHintShow;
  Result := Memo;
end;

{ TCompileForm }

{ok}constructor TCompileForm.Create(AOwner: TComponent);

  procedure ReadConfig;
  var
    Ini: TConfigIniFile;
    WindowPlacement: TWindowPlacement;
    I: Integer;
    Memo: TCompScintEdit;
  begin
    Ini := TConfigIniFile.Create;
    try
      { Menu check boxes state }
      Toolbar.Visible := Ini.ReadBool('Options', 'ShowToolbar', True);
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
      FOptions.AutoComplete := Ini.ReadBool('Options', 'AutoComplete', True);
      FOptions.UseSyntaxHighlighting := Ini.ReadBool('Options', 'UseSynHigh', True);
      FOptions.ColorizeCompilerOutput := Ini.ReadBool('Options', 'ColorizeCompilerOutput', True);
      FOptions.UnderlineErrors := Ini.ReadBool('Options', 'UnderlineErrors', True);
      FOptions.CursorPastEOL := Ini.ReadBool('Options', 'EditorCursorPastEOL', True);
      FOptions.TabWidth := Ini.ReadInteger('Options', 'TabWidth', 2);
      FOptions.UseTabCharacter := Ini.ReadBool('Options', 'UseTabCharacter', False);
      FOptions.WordWrap := Ini.ReadBool('Options', 'WordWrap', False);
      FOptions.AutoIndent := Ini.ReadBool('Options', 'AutoIndent', True);
      FOptions.IndentationGuides := Ini.ReadBool('Options', 'IndentationGuides', True);
      FOptions.GutterLineNumbers := Ini.ReadBool('Options', 'GutterLineNumbers', False);
      FOptions.OpenIncludedFiles := Ini.ReadBool('Options', 'OpenIncludedFiles', True);
      I := Ini.ReadInteger('Options', 'ThemeType', Ord(GetDefaultThemeType));
      if (I >= 0) and (I <= Ord(High(TThemeType))) then
        FOptions.ThemeType := TThemeType(I);
      if GetACP = 932 then begin
        { Default to MS Gothic font on CP 932 (Japanese), as Courier New is
          only capable of displaying Japanese characters on XP and later. }
        FMainMemo.Font.Name := 'MS Gothic';
        FMainMemo.Font.Size := 9;
        FMainMemo.Font.Charset := SHIFTJIS_CHARSET;
      end;
      FMainMemo.Font.Name := Ini.ReadString('Options', 'EditorFontName', FMainMemo.Font.Name);
      FMainMemo.Font.Size := Ini.ReadInteger('Options', 'EditorFontSize', FMainMemo.Font.Size);
      FMainMemo.Font.Charset := Ini.ReadInteger('Options', 'EditorFontCharset', FMainMemo.Font.Charset);
      FMainMemo.Zoom := Ini.ReadInteger('Options', 'Zoom', 0);
      for Memo in FMemos do begin
        if Memo <> FMainMemo then begin
          Memo.Font := FMainMemo.Font;
          Memo.Zoom := FMainMemo.Zoom;
        end;
      end;
      SyncEditorOptions;
      UpdateNewMainFileButtons;
      UpdateTheme;

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
begin
  inherited;

  {$IFNDEF STATICCOMPILER}
  FCompilerVersion := ISDllGetVersion;
  {$ELSE}
  FCompilerVersion := ISGetVersion;
  {$ENDIF}

  FModifiedSinceLastCompile := True;
  
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
  SetFakeShortCut(ECompleteWord, VK_RIGHT, [ssAlt]);
  SetFakeShortCutText(VZoomIn, SmkcCtrl + 'Num +');    { These zoom shortcuts are handled by Scintilla and only support the active memo, unlike the menu items which work on all memos }
  SetFakeShortCutText(VZoomOut, SmkcCtrl + 'Num -');
  SetFakeShortCutText(VZoomReset, SmkcCtrl + 'Num /');
  { Use fake Esc shortcut for Stop Compile so it doesn't conflict with the
    editor's autocompletion list }
  SetFakeShortCut(BStopCompile, VK_ESCAPE, []);

{$IFNDEF IS_D103RIO}
  { TStatusBar needs manual scaling before Delphi 10.3 Rio }
  StatusBar.Height := ToPPI(StatusBar.Height);
  for I := 0 to StatusBar.Panels.Count-1 do
    StatusBar.Panels[I].Width := ToPPI(StatusBar.Panels[I].Width);
{$ENDIF}

  PopupMenu := TCompileFormMemoPopupMenu.Create(Self);

  FMemosStyler := TInnoSetupStyler.Create(Self);
  FMemosStyler.IsppInstalled := IsppInstalled;
  FTheme := TTheme.Create;
  FMemos := TList<TCompScintEdit>.Create;
  FMainMemo := InitializeMainMemo(TCompMainScintEdit.Create(Self), PopupMenu);
  for I := 0 to MaxMemos-1 do begin
    if I = 0 then
      FMemos.Add(FMainMemo)
    else
      FMemos.Add(InitializeMemo(TCompScintEdit.Create(Self), PopupMenu));
  end;
  FActiveMemo := FMainMemo;
  FActiveMemo.Visible := True;
  FMemosStyler.Theme := FTheme;

  FMainMemo.ErrorLine := -1;
  FMainMemo.StepLine := -1;

  FMainMemo.BreakPoints := TList.Create;

  UpdateTabSetListsItemHeightAndDebugTimeWidth;

  Application.HintShortPause := 0;
  Application.OnException := AppOnException;
  Application.OnActivate := AppOnActivate;
  Application.OnIdle := AppOnIdle;

  FMRUMainFilesList := TStringList.Create;
  for I := 0 to High(FMRUMainFilesMenuItems) do begin
    NewItem := TMenuItem.Create(Self);
    NewItem.OnClick := FMRUClick;
    FMenu.Insert(FMenu.IndexOf(FMRUMainFilesSep), NewItem);
    FMRUMainFilesMenuItems[I] := NewItem;
  end;
  FMRUParametersList := TStringList.Create;

  FSignTools := TStringList.Create;

  FIncludedFiles := TIncludedFiles.Create;
  UpdateIncludedFiles;

  FDebugTarget := dtSetup;
  UpdateTargetMenu;

  UpdateCaption;

  UpdateThemeData(False, True);

  if CommandLineCompile then begin
    ReadSignTools(FSignTools);
    PostMessage(Handle, WM_StartCommandLineCompile, 0, 0)
  end else if CommandLineWizard then begin
    { Stop Delphi from showing the compiler form }
    Application.ShowMainForm := False;
    { Show wizard form later }
    PostMessage(Handle, WM_StartCommandLineWizard, 0, 0);
  end else begin
    ReadConfig;
    ReadSignTools(FSignTools);
    PostMessage(Handle, WM_StartNormally, 0, 0);
  end;
end;

{ok}destructor TCompileForm.Destroy;

  procedure SaveConfig;
  var
    Ini: TConfigIniFile;
    WindowPlacement: TWindowPlacement;
  begin
    Ini := TConfigIniFile.Create;
    try
      { Theme state }
      Ini.WriteInteger('Options', 'ThemeType', Ord(FOptions.ThemeType));  { Also see TOptionsClick }

      { Menu check boxes state }
      Ini.WriteBool('Options', 'ShowToolbar', Toolbar.Visible);
      Ini.WriteBool('Options', 'ShowStatusBar', StatusBar.Visible);
      Ini.WriteBool('Options', 'LowPriorityDuringCompile', FOptions.LowPriorityDuringCompile);

      { Window state }
      WindowPlacement.length := SizeOf(WindowPlacement);
      GetWindowPlacement(Handle, @WindowPlacement);
      Ini.WriteInteger('State', 'WindowLeft', WindowPlacement.rcNormalPosition.Left);
      Ini.WriteInteger('State', 'WindowTop', WindowPlacement.rcNormalPosition.Top);
      Ini.WriteInteger('State', 'WindowRight', WindowPlacement.rcNormalPosition.Right);
      Ini.WriteInteger('State', 'WindowBottom', WindowPlacement.rcNormalPosition.Bottom);
      Ini.WriteBool('State', 'WindowMaximized', WindowState = wsMaximized);
      Ini.WriteInteger('State', 'StatusPanelHeight', FromCurrentPPI(StatusPanel.Height));

      { Zoom state }
      Ini.WriteInteger('Options', 'Zoom', FMainMemo.Zoom); { Only saves the main memo's zoom }
    finally
      Ini.Free;
    end;
  end;

begin
  UpdateThemeData(True, False);

  Application.OnActivate := nil;
  Application.OnIdle := nil;

  if FOptionsLoaded and not (CommandLineCompile or CommandLineWizard) then
    SaveConfig;

  FTheme.Free;
  FMainMemo.BreakPoints.Free;
  DestroyDebugInfo;
  FIncludedFiles.Free;
  FSignTools.Free;
  FMRUParametersList.Free;
  FMRUMainFilesList.Free;

  inherited;
end;

class procedure TCompileForm.AppOnException(Sender: TObject; E: Exception);
begin
  AppMessageBox(PChar(AddPeriod(E.Message)), SCompilerFormCaption,
    MB_OK or MB_ICONSTOP);
end;

procedure TCompileForm.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  UpdateTabSetListsItemHeightAndDebugTimeWidth;
  UpdateStatusPanelHeight(StatusPanel.Height);
end;

procedure TCompileForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if IsWindowEnabled(Application.Handle) then
    CanClose := ConfirmCloseFile(True)
  else
    { CloseQuery is also called by the VCL when a WM_QUERYENDSESSION message
      is received. Don't display message box if a modal dialog is already
      displayed. }
    CanClose := False;
end;

{ok}procedure TCompileForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ShortCut(Key, Shift) = VK_ESCAPE then begin
    if BStopCompile.Enabled then
      BStopCompileClick(Self);
  end
  else if (Key = VK_F6) and not(ssAlt in Shift) then begin
    { Toggle focus between panes }
    Key := 0;
    if ActiveControl <> FActiveMemo then
      ActiveControl := FActiveMemo
    else if StatusPanel.Visible then begin
      case OutputTabSet.TabIndex of
        tiCompilerOutput: ActiveControl := CompilerOutputList;
        tiDebugOutput: ActiveControl := DebugOutputList;
        tiDebugCallStack: ActiveControl := DebugCallStackList;
      end;
    end;
  end;
end;

procedure TCompileForm.FormResize(Sender: TObject);
begin
  { Make sure the status panel's height is decreased if necessary in response
    to the form's height decreasing }
  if StatusPanel.Visible then
    UpdateStatusPanelHeight(StatusPanel.Height);
end;

{$IFDEF IS_D4}
procedure TCompileForm.WndProc(var Message: TMessage);
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
{$ENDIF}

{$IFDEF IS_D5}
function TCompileForm.IsShortCut(var Message: TWMKey): Boolean;
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
{$ENDIF}

procedure TCompileForm.UpdateCaption;
var
  NewCaption: String;
begin
  if FMainMemo.Filename = '' then
    NewCaption := 'Untitled'
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

{ok}procedure TCompileForm.UpdateNewMainFileButtons;
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

{ok}procedure TCompileForm.NewMainFile;
begin
  HideMainMemoError;
  FUninstExe := '';
  if FDebugTarget <> dtSetup then begin
    FDebugTarget := dtSetup;
    UpdateTargetMenu;
  end;
  FMainMemo.BreakPoints.Clear;
  DestroyDebugInfo;

  FMainMemo.Filename := '';
  UpdateCaption;
  FMainMemo.SaveInUTF8Encoding := False;
  FMainMemo.Lines.Clear;
  FModifiedSinceLastCompile := True;
  FIncludedFiles.Clear;
  UpdateIncludedFiles;
  FMainMemo.ClearUndo;
end;

{ok}procedure TCompileForm.NewMainFileUsingWizard;
var
  WizardForm: TWizardForm;
  SaveEnabled: Boolean;
begin
  WizardForm := TWizardForm.Create(Application);
  try
    SaveEnabled := Enabled;
    if CommandLineWizard then begin
      WizardForm.WizardName := CommandLineWizardName;
      { Must disable CompileForm even though it isn't shown, otherwise
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
      SaveTextToFile(CommandLineFileName, WizardForm.ResultScript, False);
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

{ok}procedure TCompileForm.OpenFile(AMemo: TCompScintEdit; AFilename: String;
  const MainMemoAddToRecentDocs: Boolean);

  function IsStreamUTF8Encoded(const Stream: TStream): Boolean;
  var
    Buf: array[0..2] of Byte;
  begin
    Result := False;
    if Stream.Read(Buf, SizeOf(Buf)) = SizeOf(Buf) then
      if (Buf[0] = $EF) and (Buf[1] = $BB) and (Buf[2] = $BF) then
        Result := True;
  end;

var
  Stream: TFileStream;
begin
  AFilename := PathExpand(AFilename);

  Stream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    if AMemo = FMainMemo then
      NewMainFile;
    GetFileTime(Stream.Handle, nil, nil, @AMemo.FileLastWriteTime);
    AMemo.SaveInUTF8Encoding := IsStreamUTF8Encoded(Stream);
    Stream.Seek(0, soFromBeginning);
    AMemo.Lines.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
  AMemo.ClearUndo;
  AMemo.Filename := AFilename;
  if AMemo = FMainMemo then begin
    UpdateCaption;
    ModifyMRUMainFilesList(AFilename, True);
    if MainMemoAddToRecentDocs then
      AddFileToRecentDocs(AFilename);
  end;
end;

{ok}procedure TCompileForm.OpenMRUMainFile(const AFilename: String);
{ Same as OpenFile, but offers to remove the file from the MRU list if it
  cannot be opened }
begin
  try
    OpenFile(FMainMemo, AFilename, True);
  except
    Application.HandleException(Self);
    if MsgBoxFmt('There was an error opening the file. Remove it from the list?',
       [AFilename], SCompilerFormCaption, mbError, MB_YESNO) = IDYES then
      ModifyMRUMainFilesList(AFilename, False);
  end;
end;

{ok}function TCompileForm.SaveFile(const AMemo: TCompScintEdit; const SaveAs: Boolean): Boolean;

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
      SaveTextToFile(TempFN, AMemo.Lines.Text, AMemo.SaveInUTF8Encoding);

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
    AMemo.ClearUndo;
  Result := True;
  if AMemo = FMainMemo then
    ModifyMRUMainFilesList(AMemo.Filename, True);
end;

{ok}function TCompileForm.ConfirmCloseFile(const PromptToSave: Boolean): Boolean;

  function PromptToSaveMemo(const AMemo: TCompScintEdit): Boolean;
var
  FileTitle: String;
  begin
    Result := True;
    if AMemo.Modified then begin
      FileTitle := AMemo.Filename;
      if FileTitle = '' then FileTitle := 'Untitled';
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
  Memo: TCompScintEdit;
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
    for Memo in FMemos do begin
      Result := PromptToSaveMemo(Memo);
      if not Result then
        Exit;
    end;
  end;
end;

{ok}procedure TCompileForm.ReadMRUMainFilesList;
begin
  try
    ReadMRUList(FMRUMainFilesList, 'ScriptFileHistoryNew', 'History');
  except
    { Ignore any exceptions. }
  end;
end;

{ok}procedure TCompileForm.ModifyMRUMainFilesList(const AFilename: String;
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

{ok}procedure TCompileForm.ReadMRUParametersList;
begin
  try
    ReadMRUList(FMRUParametersList, 'ParameterHistory', 'History');
  except
    { Ignore any exceptions. }
  end;
end;

{ok}procedure TCompileForm.ModifyMRUParametersList(const AParameter: String;
  const AddNewItem: Boolean);
begin
  { Load most recent items first, just in case they've changed }
  try
    ReadMRUParametersList;
  except
    { Ignore any exceptions. }
  end;
  try
    ModifyMRUList(FMRUParametersList, 'ParameterHistory', 'History', AParameter, AddNewItem, @CompareText);
  except
    { Handle exceptions locally; failure to save the MRU list should not be
      a fatal error. }
    Application.HandleException(Self);
  end;
end;

{ok}procedure TCompileForm.StatusMessage(const Kind: TStatusMessageKind; const S: String);
begin
  AddLines(CompilerOutputList, S, TObject(Kind), False, alpNone, 0);
  CompilerOutputList.Update;
end;

{ok}procedure TCompileForm.DebugLogMessage(const S: String);
begin
  AddLines(DebugOutputList, S, nil, True, alpTimestamp, FDebugLogListTimestampsWidth);
  DebugOutputList.Update;
end;

{ok}procedure TCompileForm.DebugShowCallStack(const CallStack: String; const CallStackCount: Cardinal);
begin
  DebugCallStackList.Clear;
  AddLines(DebugCallStackList, CallStack, nil, True, alpCountdown, FCallStackCount-1);
  DebugCallStackList.Items.Insert(0, '*** [Code] Call Stack');
  DebugCallStackList.Update;
end;

type
  PAppData = ^TAppData;
  TAppData = record
    Form: TCompileForm;
    Lines: TStringList;
    CurLineNumber: Integer;
    CurLine: String;
    OutputExe: String;
    IncludedFiles: TIncludedFiles;
    ErrorMsg: String;
    ErrorFilename: String;
    ErrorLine: Integer;
    Aborted: Boolean;
  end;

{ok}function CompilerCallbackProc(Code: Integer; var Data: TCompilerCallbackData;
  AppData: Longint): Integer; stdcall;

  procedure ParseIncludedFilenames(P: PChar; const IncludedFiles: TIncludedFiles);
  var
    IncludedFile: TIncludedFile;
  begin
    IncludedFiles.Clear;
    if P = nil then
      Exit;
    while P^ <> #0 do begin
      if not IsISPPBuiltins(P) then begin
        IncludedFile := TIncludedFile.Create;
        IncludedFile.Filename := P;
        IncludedFile.HasLastWriteTime := GetLastWriteTimeOfFile(IncludedFile.Filename,
          @IncludedFile.LastWriteTime);
        IncludedFiles.Add(IncludedFile);
      end;
      Inc(P, StrLen(P) + 1);
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
      iscbNotifySuccess:
        begin
          OutputExe := Data.OutputExeFilename;
          if Form.FCompilerVersion.BinVersion >= $3000001 then
            Form.ParseDebugInfo(Data.DebugInfo);
          ParseIncludedFilenames(Data.IncludedFilenames, IncludedFiles); { Also stores last write time }
        end;
      iscbNotifyError:
        begin
          if Assigned(Data.ErrorMsg) then
            ErrorMsg := Data.ErrorMsg
          else
            Aborted := True;
          ErrorFilename := Data.ErrorFilename;
          ErrorLine := Data.ErrorLine;
          ParseIncludedFilenames(Data.IncludedFilenamesSoFar, IncludedFiles); { Also stores last write time }
        end;
    end;
end;

procedure TCompileForm.CompileFile(AFilename: String; const ReadFromFile: Boolean);

  procedure ReadScriptLines(const ALines: TStringList);

    function ContainsNullChar(const S: String): Boolean;
    var
      I: Integer;
    begin
      Result := False;
      for I := 1 to Length(S) do
        if S[I] = #0 then begin
          Result := True;
          Break;
        end;
    end;

  var
    F: TTextFileReader;
    I: Integer;
  begin
    if ReadFromFile then begin
      F := TTextFileReader.Create(AFilename, fdOpenExisting, faRead, fsRead);
      try
        while not F.Eof do
          ALines.Add(F.ReadLine);
      finally
        F.Free;
      end;
    end
    else begin
      ALines.Capacity := FMainMemo.Lines.Count;
      ALines.Assign(FMainMemo.Lines);
    end;

    { Check for null characters }
    for I := 0 to ALines.Count-1 do begin
      if ContainsNullChar(ALines[I]) then begin
        if not ReadFromFile then begin
          MoveMainMemoCaret(I, False);
          SetMainMemoErrorLine(I);
        end;
        raise Exception.CreateFmt(SCompilerIllegalNullChar, [I + 1]);
      end;
    end;
  end;

var
  SourcePath, S, Options: String;
  Params: TCompileScriptParamsEx;
  AppData: TAppData;
  StartTime, ElapsedTime, ElapsedSeconds: DWORD;
  I: Integer;
  Memo: TCompScintEdit;
begin
  if FCompiling then begin
    { Shouldn't get here, but just in case... }
    MsgBox('A compile is already in progress.', SCompilerFormCaption, mbError, MB_OK);
    Abort;
  end;

  if not ReadFromFile then begin
    { Regular compile instead of a command line compile }
    if FOptions.Autosave and FMainMemo.Modified then begin
      if not SaveFile(FMainMemo, False) then Abort;
    end else if FMainMemo.Filename = '' then begin
      case MsgBox('Would you like to save the script before compiling?' +
         SNewLine2 + 'If you answer No, the compiled installation will be ' +
         'placed under your My Documents folder by default.',
         SCompilerFormCaption, mbConfirmation, MB_YESNOCANCEL) of
        IDYES: if not SaveFile(FMainMemo, False) then Abort;
        IDNO: ;
      else
        Abort;
      end;
    end;
    AFilename := FMainMemo.Filename;
  end;

  DestroyDebugInfo;
  AppData.Lines := TStringList.Create;
  AppData.IncludedFiles := FIncludedFiles;
  try
    FBuildAnimationFrame := 0;
    FProgress := 0;
    FProgressMax := 0;

    FMainMemo.CancelAutoComplete;
    FMainMemo.Cursor := crAppStart;
    FMainMemo.SetCursorID(999);  { hack to keep it from overriding Cursor }
    CompilerOutputList.Cursor := crAppStart;
    for Memo in FMemos do
      Memo.ReadOnly := True;
    UpdateEditModePanel;
    HideMainMemoError;
    CompilerOutputList.Clear;
    SendMessage(CompilerOutputList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
    DebugOutputList.Clear;
    SendMessage(DebugOutputList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
    DebugCallStackList.Clear;
    SendMessage(DebugCallStackList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
    OutputTabSet.TabIndex := tiCompilerOutput;
    SetStatusPanelVisible(True);

    if AFilename <> '' then
      SourcePath := PathExtractPath(AFilename)
    else begin
      { If the script was not saved, default to My Documents }
      SourcePath := GetShellFolderPath(CSIDL_PERSONAL);
      if SourcePath = '' then
        raise Exception.Create('GetShellFolderPath failed');
    end;
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
    ReadScriptLines(AppData.Lines);

    StartTime := GetTickCount;
    StatusMessage(smkStartEnd, Format(SCompilerStatusStarting, [TimeToStr(Time)]));
    StatusMessage(smkStartEnd, '');
    FCompiling := True;
    FCompileWantAbort := False;
    UpdateRunMenu;
    UpdateCaption;
    SetLowPriority(FOptions.LowPriorityDuringCompile, FSavePriorityClass);
    {$IFNDEF STATICCOMPILER}
    if ISDllCompileScript(Params) <> isceNoError then begin
    {$ELSE}
    if ISCompileScript(Params, False) <> isceNoError then begin
    {$ENDIF}
      StatusMessage(smkError, SCompilerStatusErrorAborted);
      if not ReadFromFile and (AppData.ErrorLine > 0) and
         (AppData.ErrorFilename = '') then begin
        { Move the caret to the line number the error occurred on }
        MoveMainMemoCaret(AppData.ErrorLine - 1, False);
        SetMainMemoErrorLine(AppData.ErrorLine - 1);
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
      Format('%.2u%s%.2u%s%.3u', [ElapsedSeconds div 60, {$IFDEF IS_DXE}FormatSettings.{$ENDIF}TimeSeparator,
        ElapsedSeconds mod 60, {$IFDEF IS_DXE}FormatSettings.{$ENDIF}DecimalSeparator, ElapsedTime mod 1000])]));
  finally
    AppData.Lines.Free;
    FCompiling := False;
    SetLowPriority(False, FSavePriorityClass);
    FMainMemo.Cursor := crDefault;
    FMainMemo.SetCursorID(SC_CURSORNORMAL);
    CompilerOutputList.Cursor := crDefault;
    for Memo in FMemos do
      Memo.ReadOnly := False;
    UpdateEditModePanel;
    UpdateRunMenu;
    UpdateCaption;
    UpdateIncludedFiles;
    InvalidateStatusPanel(spCompileIcon);
    InvalidateStatusPanel(spCompileProgress);
    SetAppTaskbarProgressState(tpsNoProgress);
    StatusBar.Panels[spExtraStatus].Text := '';
  end;
  FCompiledExe := AppData.OutputExe;
  FModifiedSinceLastCompile := False;
  FModifiedSinceLastCompileAndGo := False;
end;

{ok}procedure TCompileForm.SyncEditorOptions;
const
  SquigglyStyles: array[Boolean] of Integer = (INDIC_HIDDEN, INDIC_SQUIGGLE);
var
  Memo: TCompScintEdit;
begin
  for Memo in FMemos do begin
    Memo.UseStyleAttributes := FOptions.UseSyntaxHighlighting;
    Memo.Call(SCI_INDICSETSTYLE, inSquiggly, SquigglyStyles[FOptions.UnderlineErrors]);

    if FOptions.CursorPastEOL then
      Memo.VirtualSpaceOptions := [svsRectangularSelection, svsUserAccessible]
    else
      Memo.VirtualSpaceOptions := [];
    Memo.FillSelectionToEdge := FOptions.CursorPastEOL;

    Memo.TabWidth := FOptions.TabWidth;
    Memo.UseTabCharacter := FOptions.UseTabCharacter;

    Memo.WordWrap := FOptions.WordWrap;

    if FOptions.IndentationGuides then
      Memo.IndentationGuides := sigLookBoth
    else
      Memo.IndentationGuides := sigNone;

    Memo.LineNumbers := FOptions.GutterLineNumbers;
  end;
end;

{ok}procedure TCompileForm.FMenuClick(Sender: TObject);

  function DoubleAmp(const S: String): String;
  var
    I: Integer;
  begin
    Result := S;
    I := 1;
    while I <= Length(Result) do begin
      if Result[I] = '&' then begin
        Inc(I);
        Insert('&', Result, I);
        Inc(I);
      end
      else
        Inc(I, PathCharLength(S, I));
    end;
  end;

var
  I: Integer;
begin
  FSaveMainFileAs.Enabled := FActiveMemo = FMainMemo;
  FSaveEncodingAuto.Checked := not FActiveMemo.SaveInUTF8Encoding;
  FSaveEncodingUTF8.Checked := FActiveMemo.SaveInUTF8Encoding;
  ReadMRUMainFilesList;
  FMRUMainFilesSep.Visible := FMRUMainFilesList.Count <> 0;
  for I := 0 to High(FMRUMainFilesMenuItems) do
    with FMRUMainFilesMenuItems[I] do begin
      if I < FMRUMainFilesList.Count then begin
        Visible := True;
        Caption := '&' + IntToStr((I+1) mod 10) + ' ' + DoubleAmp(FMRUMainFilesList[I]);
      end
      else
        Visible := False;
    end;
end;

{ok}procedure TCompileForm.FNewMainFileClick(Sender: TObject);
begin
  if ConfirmCloseFile(True) then
    NewMainFile;
end;

{ok}procedure TCompileForm.FNewMainFileUserWizardClick(Sender: TObject);
begin
  if ConfirmCloseFile(True) then
    NewMainFileUsingWizard;
end;

{ok}procedure TCompileForm.ShowOpenMainFileDialog(const Examples: Boolean);
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

{ok}procedure TCompileForm.FOpenMainFileClick(Sender: TObject);
begin
  ShowOpenMainFileDialog(False);
end;

{ok}procedure TCompileForm.FSaveClick(Sender: TObject);
begin
  SaveFile(FActiveMemo, Sender = FSaveMainFileAs);
end;

{ok}procedure TCompileForm.FSaveEncodingItemClick(Sender: TObject);
begin
  FActiveMemo.SaveInUTF8Encoding := (Sender = FSaveEncodingUTF8);
end;

{ok}procedure TCompileForm.FMRUClick(Sender: TObject);
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

{ok}procedure TCompileForm.FExitClick(Sender: TObject);
begin
  Close;
end;

{ok}procedure TCompileForm.EMenuClick(Sender: TObject);
var
  MemoHasFocus: Boolean;
begin
  MemoHasFocus := FActiveMemo.Focused;
  EUndo.Enabled := MemoHasFocus and FActiveMemo.CanUndo;
  ERedo.Enabled := MemoHasFocus and FActiveMemo.CanRedo;
  ECut.Enabled := MemoHasFocus and FActiveMemo.SelAvail;
  ECopy.Enabled := MemoHasFocus and FActiveMemo.SelAvail;
  EPaste.Enabled := MemoHasFocus and Clipboard.HasFormat(CF_TEXT);
  EDelete.Enabled := MemoHasFocus and FActiveMemo.SelAvail;
  ESelectAll.Enabled := MemoHasFocus;
  EFind.Enabled := MemoHasFocus;
  EFindNext.Enabled := MemoHasFocus;
  EReplace.Enabled := MemoHasFocus;
  EGoto.Enabled := MemoHasFocus;
  ECompleteWord.Enabled := MemoHasFocus;
end;

{ok}procedure TCompileForm.EUndoClick(Sender: TObject);
begin
  FActiveMemo.Undo;
end;

{ok}procedure TCompileForm.ERedoClick(Sender: TObject);
begin
  FActiveMemo.Redo;
end;

{ok}procedure TCompileForm.ECutClick(Sender: TObject);
begin
  FActiveMemo.CutToClipboard;
end;

{ok}procedure TCompileForm.ECopyClick(Sender: TObject);
begin
  FActiveMemo.CopyToClipboard;
end;

{ok}procedure TCompileForm.EPasteClick(Sender: TObject);
begin
  FActiveMemo.PasteFromClipboard;
end;

{ok}
procedure TCompileForm.EDeleteClick(Sender: TObject);
begin
  FActiveMemo.ClearSelection;
end;

{ok}procedure TCompileForm.ESelectAllClick(Sender: TObject);
begin
  FActiveMemo.SelectAll;
end;

{ok}procedure TCompileForm.ECompleteWordClick(Sender: TObject);
begin
  InitiateAutoComplete(#0);
end;

{ok}procedure TCompileForm.VMenuClick(Sender: TObject);
begin
  VZoomIn.Enabled := (FActiveMemo.Zoom < 20);
  VZoomOut.Enabled := (FActiveMemo.Zoom > -10);
  VZoomReset.Enabled := (FActiveMemo.Zoom <> 0);
  VToolbar.Checked := Toolbar.Visible;
  VStatusBar.Checked := StatusBar.Visible;
  VHide.Checked := not StatusPanel.Visible;
  VCompilerOutput.Checked := StatusPanel.Visible and (OutputTabSet.TabIndex = tiCompilerOutput);
  VDebugOutput.Checked := StatusPanel.Visible and (OutputTabSet.TabIndex = tiDebugOutput);
  VDebugCallStack.Checked := StatusPanel.Visible and (OutputTabSet.TabIndex = tiDebugCallStack);
end;

{ok}procedure TCompileForm.SyncZoom;
var
  Memo: TCompScintEdit;
begin
  { The zoom shortcuts are handled by Scintilla and may cause different zoom levels per memo. This
    function sets the zoom of all memo's to the zoom of the active memo to make zoom in synch again. }
  for Memo in FMemos do
    if Memo <> FActiveMemo then
      Memo.Zoom := FActiveMemo.Zoom;
end;

{ok}procedure TCompileForm.VZoomInClick(Sender: TObject);
var
  Memo: TCompScintEdit;
begin
  SyncZoom;
  for Memo in FMemos do
    Memo.ZoomIn;
end;

{ok}procedure TCompileForm.VZoomOutClick(Sender: TObject);
var
  Memo: TCompScintEdit;
begin
  SyncZoom;
  for Memo in FMemos do
    Memo.ZoomOut;
end;

{ok}procedure TCompileForm.VZoomResetClick(Sender: TObject);
var
  Memo: TCompScintEdit;
begin
  for Memo in FMemos do
    Memo.Zoom := 0;
end;

{ok}procedure TCompileForm.VToolbarClick(Sender: TObject);
begin
  Toolbar.Visible := not Toolbar.Visible;
end;

{ok}procedure TCompileForm.VStatusBarClick(Sender: TObject);
begin
  StatusBar.Visible := not StatusBar.Visible;
end;

{ok}procedure TCompileForm.SetStatusPanelVisible(const AVisible: Boolean);
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

{ok}procedure TCompileForm.VHideClick(Sender: TObject);
begin
  SetStatusPanelVisible(False);
end;

{ok}procedure TCompileForm.VCompilerOutputClick(Sender: TObject);
begin
  OutputTabSet.TabIndex := tiCompilerOutput;
  SetStatusPanelVisible(True);
end;

{ok}procedure TCompileForm.VDebugOutputClick(Sender: TObject);
begin
  OutputTabSet.TabIndex := tiDebugOutput;
  SetStatusPanelVisible(True);
end;

{ok}procedure TCompileForm.VDebugCallStackClick(Sender: TObject);
begin
  OutputTabSet.TabIndex := tiDebugCallStack;
  SetStatusPanelVisible(True);
end;

{ok}procedure TCompileForm.BMenuClick(Sender: TObject);
begin
  BLowPriority.Checked := FOptions.LowPriorityDuringCompile;
  BOpenOutputFolder.Enabled := (FCompiledExe <> '');
end;

{ok}procedure TCompileForm.BCompileClick(Sender: TObject);
begin
  CompileFile('', False);
end;

{ok}procedure TCompileForm.BStopCompileClick(Sender: TObject);
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

{ok}procedure TCompileForm.BLowPriorityClick(Sender: TObject);
begin
  FOptions.LowPriorityDuringCompile := not FOptions.LowPriorityDuringCompile;
  { If a compile is already in progress, change the priority now }
  if FCompiling then
    SetLowPriority(FOptions.LowPriorityDuringCompile, FSavePriorityClass);
end;

{ok}procedure TCompileForm.BOpenOutputFolderClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := GetWinDir;
  ShellExecute(Application.Handle, 'open', PChar(AddBackslash(Dir) + 'explorer.exe'),
    PChar(Format('/select,"%s"', [FCompiledExe])), PChar(Dir), SW_SHOW);
end;

{ok}procedure TCompileForm.HMenuClick(Sender: TObject);
begin
  HISPPDoc.Visible := NewFileExists(PathExtractPath(NewParamStr(0)) + 'ispp.chm');
  HISPPSep.Visible := HISPPDoc.Visible;
end;

{ok}procedure TCompileForm.HDocClick(Sender: TObject);
var
  HelpFile: String;
begin
  HelpFile := GetHelpFile;
  if Assigned(HtmlHelp) then
    HtmlHelp(GetDesktopWindow, PChar(HelpFile), HH_DISPLAY_TOPIC, 0);
end;

{ok}procedure TCompileForm.MemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  S, HelpFile: String;
  KLink: THH_AKLINK;
begin
  if Key = VK_F1 then begin
    HelpFile := GetHelpFile;
    if Assigned(HtmlHelp) then begin
      HtmlHelp(GetDesktopWindow, PChar(HelpFile), HH_DISPLAY_TOPIC, 0);
      S := FMainMemo.WordAtCursor;
      if S <> '' then begin
        FillChar(KLink, SizeOf(KLink), 0);
        KLink.cbStruct := SizeOf(KLink);
        KLink.pszKeywords := PChar(S);
        KLink.fIndexOnFail := True;
        HtmlHelp(GetDesktopWindow, PChar(HelpFile), HH_KEYWORD_LOOKUP, DWORD(@KLink));
      end;
    end;
  end
  else if (Key = VK_RIGHT) and (Shift * [ssShift, ssAlt, ssCtrl] = [ssAlt]) then begin
    InitiateAutoComplete(#0);
    Key := 0;
  end;
end;

{ok}procedure TCompileForm.MemoKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = ' ') and (GetKeyState(VK_CONTROL) < 0) then begin
    InitiateAutoComplete(#0);
    Key := #0;
  end;
end;

{ok}procedure TCompileForm.HExamplesClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open',
    PChar(PathExtractPath(NewParamStr(0)) + 'Examples'), nil, nil, SW_SHOW);
end;

{ok}procedure TCompileForm.HFaqClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open',
    PChar(PathExtractPath(NewParamStr(0)) + 'isfaq.url'), nil, nil, SW_SHOW);
end;

{ok}procedure TCompileForm.HWhatsNewClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open',
    PChar(PathExtractPath(NewParamStr(0)) + 'whatsnew.htm'), nil, nil, SW_SHOW);
end;

{ok}procedure TCompileForm.HWebsiteClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', 'https://jrsoftware.org/isinfo.php', nil,
    nil, SW_SHOW);
end;

{ok}procedure TCompileForm.HMailingListClick(Sender: TObject);
begin
  OpenMailingListSite;
end;

{ok}procedure TCompileForm.HPSWebsiteClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', 'http://www.remobjects.com/ps', nil,
    nil, SW_SHOW);
end;

{ok}procedure TCompileForm.HISPPDocClick(Sender: TObject);
begin
  if Assigned(HtmlHelp) then
    HtmlHelp(GetDesktopWindow, PChar(GetHelpFile + '::/hh_isppredirect.xhtm'), HH_DISPLAY_TOPIC, 0);
end;

{ok}procedure TCompileForm.HDonateClick(Sender: TObject);
begin
  OpenDonateSite;
end;

{ok}procedure TCompileForm.HAboutClick(Sender: TObject);
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
  S := S + ('Copyright (C) 1997-2020 Jordan Russell' + SNewLine +
    'Portions Copyright (C) 2000-2020 Martijn Laan' + SNewLine +
    'All rights reserved.' + SNewLine2 +
    'Inno Setup home page:' + SNewLine +
    'https://www.innosetup.com/' + SNewLine2 +
    'RemObjects Pascal Script home page:' + SNewLine +
    'https://www.remobjects.com/ps' + SNewLine2 +
    'Refer to LICENSE.TXT for conditions of distribution and use.');
  MsgBox(S, 'About ' + FCompilerVersion.Title, mbInformation, MB_OK);
end;

{ok}procedure TCompileForm.WMStartCommandLineCompile(var Message: TMessage);
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

{ok}procedure TCompileForm.WMStartCommandLineWizard(var Message: TMessage);
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

{ok}procedure TCompileForm.WMStartNormally(var Message: TMessage);

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

{ok}procedure TCompileForm.MemosTabSetClick(Sender: TObject);
var
  Memo: TCompScintEdit;
  I: Integer;
begin
  for I := 0 to MemosTabSet.Tabs.Count-1 do begin
    Memo := FMemos[I];
    Memo.Visible := (I = MemosTabSet.TabIndex);
    if Memo.Visible then begin
      FActiveMemo := Memo;
      ActiveControl := Memo;
    end;
  end;
  RToggleBreakPoint.Enabled := FActiveMemo = FMainMemo;
  UpdateRunMenu;
  UpdateCaretPosPanel;
  UpdateEditModePanel;
  UpdateModifiedPanel;
end;

{ok}procedure TCompileForm.InitializeFindText(Dlg: TFindDialog);
var
  S: String;
begin
  S := FActiveMemo.SelText;
  if (S <> '') and (Pos(#13, S) = 0) and (Pos(#10, S) = 0) then
    Dlg.FindText := S
  else
    Dlg.FindText := FLastFindText;
end;

{ok}procedure TCompileForm.EFindClick(Sender: TObject);
begin
  ReplaceDialog.CloseDialog;
  if FindDialog.Handle = 0 then
    InitializeFindText(FindDialog);
  FindDialog.Execute;
end;

{ok}procedure TCompileForm.EFindNextClick(Sender: TObject);
begin
  if FLastFindText = '' then
    EFindClick(Sender)
  else
    FindNext;
end;

{ok}procedure TCompileForm.FindNext;
var
  StartPos, EndPos: Integer;
  Range: TScintRange;
begin
  if frDown in FLastFindOptions then begin
    StartPos := FActiveMemo.Selection.EndPos;
    EndPos := FActiveMemo.RawTextLength;
  end
  else begin
    StartPos := FActiveMemo.Selection.StartPos;
    EndPos := 0;
  end;
  if FActiveMemo.FindText(StartPos, EndPos, FLastFindText,
     FindOptionsToSearchOptions(FLastFindOptions), Range) then
    FActiveMemo.Selection := Range
  else
    MsgBoxFmt('Cannot find "%s"', [FLastFindText], SCompilerFormCaption,
      mbInformation, MB_OK);
end;

{ok}procedure TCompileForm.FindDialogFind(Sender: TObject);
begin
  { this event handler is shared between FindDialog & ReplaceDialog }
  with Sender as TFindDialog do begin
    { Save a copy of the current text so that InitializeFindText doesn't
      mess up the operation of Edit | Find Next }
    FLastFindOptions := Options;
    FLastFindText := FindText;
  end;
  FindNext;
end;

{ok}procedure TCompileForm.EReplaceClick(Sender: TObject);
begin
  FindDialog.CloseDialog;
  if ReplaceDialog.Handle = 0 then begin
    InitializeFindText(ReplaceDialog);
    ReplaceDialog.ReplaceText := FLastReplaceText;
  end;
  ReplaceDialog.Execute;
end;

{ok}procedure TCompileForm.ReplaceDialogReplace(Sender: TObject);
var
  ReplaceCount, Pos: Integer;
  Range, NewRange: TScintRange;
begin
  FLastFindOptions := ReplaceDialog.Options;
  FLastFindText := ReplaceDialog.FindText;
  FLastReplaceText := ReplaceDialog.ReplaceText;

  if frReplaceAll in FLastFindOptions then begin
    ReplaceCount := 0;
    FActiveMemo.BeginUndoAction;
    try
      Pos := 0;
      while FActiveMemo.FindText(Pos, FActiveMemo.RawTextLength, FLastFindText,
         FindOptionsToSearchOptions(FLastFindOptions), Range) do begin
        NewRange := FActiveMemo.ReplaceTextRange(Range.StartPos, Range.EndPos, FLastReplaceText);
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
    if FActiveMemo.SelTextEquals(FLastFindText, frMatchCase in FLastFindOptions) then
      FActiveMemo.SelText := FLastReplaceText;
    FindNext;
  end;
end;

{ok}procedure TCompileForm.UpdateStatusPanelHeight(H: Integer);
var
  MinHeight, MaxHeight: Integer;
begin
  MinHeight := (3 * DebugOutputList.ItemHeight + ToCurrentPPI(4)) + OutputTabSet.Height;
  MaxHeight := BodyPanel.ClientHeight - ToCurrentPPI(48) - SplitPanel.Height;
  if H > MaxHeight then H := MaxHeight;
  if H < MinHeight then H := MinHeight;
  StatusPanel.Height := H;
end;

{ok}procedure TCompileForm.UpdateTabSetListsItemHeightAndDebugTimeWidth;
begin
  CompilerOutputList.Canvas.Font.Assign(CompilerOutputList.Font);
  CompilerOutputList.ItemHeight := CompilerOutputList.Canvas.TextHeight('0');

  DebugOutputList.Canvas.Font.Assign(DebugOutputList.Font);
  FDebugLogListTimestampsWidth := DebugOutputList.Canvas.TextWidth(Format('[00%s00%s00%s000]   ', [FormatSettings.TimeSeparator, FormatSettings.TimeSeparator, FormatSettings.DecimalSeparator]));
  DebugOutputList.ItemHeight := DebugOutputList.Canvas.TextHeight('0');

  DebugCallStackList.Canvas.Font.Assign(DebugCallStackList.Font);
  DebugCallStackList.ItemHeight := DebugCallStackList.Canvas.TextHeight('0');
end;

{ok}procedure TCompileForm.SplitPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and StatusPanel.Visible then begin
    UpdateStatusPanelHeight(BodyPanel.ClientToScreen(Point(0, 0)).Y -
      SplitPanel.ClientToScreen(Point(0, Y)).Y +
      BodyPanel.ClientHeight - (SplitPanel.Height div 2));
  end;
end;

{ok}procedure TCompileForm.TAddRemoveProgramsClick(Sender: TObject);
begin
  StartAddRemovePrograms;
end;

{ok}procedure TCompileForm.TGenerateGUIDClick(Sender: TObject);
begin
  if MsgBox('The generated GUID will be inserted into the editor at the cursor position. Continue?',
     SCompilerFormCaption, mbConfirmation, MB_YESNO) = IDYES then
    FActiveMemo.SelText := GenerateGuid;
end;

{ok}procedure TCompileForm.TInsertMsgBoxClick(Sender: TObject);
var
  MsgBoxForm: TMBDForm;
begin
  MsgBoxForm := TMBDForm.Create(Application);
  try
    if MsgBoxForm.ShowModal = mrOk then
      FActiveMemo.SelText := MsgBoxForm.Text;
  finally
    MsgBoxForm.Free;
  end;
end;

{ok}procedure TCompileForm.TSignToolsClick(Sender: TObject);
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

procedure TCompileForm.TOptionsClick(Sender: TObject);
var
  OptionsForm: TOptionsForm;
  Ini: TConfigIniFile;
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
    OptionsForm.AutoCompleteCheck.Checked := FOptions.AutoComplete;
    OptionsForm.UseSynHighCheck.Checked := FOptions.UseSyntaxHighlighting;
    OptionsForm.ColorizeCompilerOutputCheck.Checked := FOptions.ColorizeCompilerOutput;
    OptionsForm.UnderlineErrorsCheck.Checked := FOptions.UnderlineErrors;
    OptionsForm.CursorPastEOLCheck.Checked := FOptions.CursorPastEOL;
    OptionsForm.TabWidthEdit.Text := IntToStr(FOptions.TabWidth);
    OptionsForm.UseTabCharacterCheck.Checked := FOptions.UseTabCharacter;
    OptionsForm.WordWrapCheck.Checked := FOptions.WordWrap;
    OptionsForm.AutoIndentCheck.Checked := FOptions.AutoIndent;
    OptionsForm.IndentationGuidesCheck.Checked := FOptions.IndentationGuides;
    OptionsForm.GutterLineNumbersCheck.Checked := FOptions.GutterLineNumbers;
    OptionsForm.OpenIncludedFilesCheck.Checked := FOptions.OpenIncludedFiles;
    OptionsForm.ThemeComboBox.ItemIndex := Ord(FOptions.ThemeType);
    OptionsForm.FontPanel.Font.Assign(FMainMemo.Font);
    OptionsForm.FontPanel.ParentBackground := False;
    OptionsForm.FontPanel.Color := FMainMemo.Color;

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
    FOptions.AutoComplete := OptionsForm.AutoCompleteCheck.Checked;
    FOptions.UseSyntaxHighlighting := OptionsForm.UseSynHighCheck.Checked;
    FOptions.ColorizeCompilerOutput := OptionsForm.ColorizeCompilerOutputCheck.Checked;
    FOptions.UnderlineErrors := OptionsForm.UnderlineErrorsCheck.Checked;
    FOptions.CursorPastEOL := OptionsForm.CursorPastEOLCheck.Checked;
    FOptions.TabWidth := StrToInt(OptionsForm.TabWidthEdit.Text);
    FOptions.UseTabCharacter := OptionsForm.UseTabCharacterCheck.Checked;
    FOptions.WordWrap := OptionsForm.WordWrapCheck.Checked;
    FOptions.AutoIndent := OptionsForm.AutoIndentCheck.Checked;
    FOptions.IndentationGuides := OptionsForm.IndentationGuidesCheck.Checked;
    FOptions.GutterLineNumbers := OptionsForm.GutterLineNumbersCheck.Checked;
    FOptions.OpenIncludedFiles := OptionsForm.OpenIncludedFilesCheck.Checked;
    FOptions.ThemeType := TThemeType(OptionsForm.ThemeComboBox.ItemIndex);
    UpdateCaption;
    UpdateIncludedFiles;
    { Move caret to start of line to ensure it doesn't end up in the middle
      of a double-byte character if the code page changes from SBCS to DBCS }
    FMainMemo.CaretLine := FMainMemo.CaretLine;
    FMainMemo.Font.Assign(OptionsForm.FontPanel.Font);
    SyncEditorOptions;
    UpdateNewMainFileButtons;
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
      Ini.WriteBool('Options', 'AutoComplete', FOptions.AutoComplete);
      Ini.WriteBool('Options', 'UseSynHigh', FOptions.UseSyntaxHighlighting);
      Ini.WriteBool('Options', 'ColorizeCompilerOutput', FOptions.ColorizeCompilerOutput);
      Ini.WriteBool('Options', 'UnderlineErrors', FOptions.UnderlineErrors);
      Ini.WriteBool('Options', 'EditorCursorPastEOL', FOptions.CursorPastEOL);
      Ini.WriteInteger('Options', 'TabWidth', FOptions.TabWidth);
      Ini.WriteBool('Options', 'UseTabCharacter', FOptions.UseTabCharacter);
      Ini.WriteBool('Options', 'WordWrap', FOptions.WordWrap);
      Ini.WriteBool('Options', 'AutoIndent', FOptions.AutoIndent);
      Ini.WriteBool('Options', 'IndentationGuides', FOptions.IndentationGuides);
      Ini.WriteBool('Options', 'GutterLineNumbers', FOptions.GutterLineNumbers);
      Ini.WriteBool('Options', 'OpenIncludedFiles', FOptions.OpenIncludedFiles);
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

{ok}procedure TCompileForm.MoveMainMemoCaret(const LineNumber: Integer;
  const AlwaysResetColumn: Boolean);
var
  Pos: Integer;
begin
  if AlwaysResetColumn or (FMainMemo.CaretLine <> LineNumber) then
    Pos := FMainMemo.GetPositionFromLine(LineNumber)
  else
    Pos := FMainMemo.CaretPosition;

  { If the line isn't in view, scroll so that it's in the center }
  if not FMainMemo.IsPositionInViewVertically(Pos) then
    FMainMemo.TopLine := FMainMemo.GetVisibleLineFromDocLine(LineNumber) -
      (FMainMemo.LinesInWindow div 2);

  FMainMemo.CaretPosition := Pos;
  MemosTabSet.TabIndex := FMemos.IndexOf(FMainMemo);
end;

{ok}procedure TCompileForm.SetMainMemoErrorLine(ALine: Integer);
var
  OldLine: Integer;
begin
  if FMainMemo.ErrorLine <> ALine then begin
    OldLine := FMainMemo.ErrorLine;
    FMainMemo.ErrorLine := ALine;
    if OldLine >= 0 then
      UpdateMainMemoLineMarkers(OldLine);
    if FMainMemo.ErrorLine >= 0 then begin
      FMainMemo.ErrorCaretPosition := FMainMemo.CaretPosition;
      UpdateMainMemoLineMarkers(FMainMemo.ErrorLine);
    end;
  end;
end;

{ok}procedure TCompileForm.SetMainMemoStepLine(ALine: Integer);
var
  OldLine: Integer;
begin
  if FMainMemo.StepLine <> ALine then begin
    OldLine := FMainMemo.StepLine;
    FMainMemo.StepLine := ALine;
    if OldLine >= 0 then
      UpdateMainMemoLineMarkers(OldLine);
    if FMainMemo.StepLine >= 0 then
      UpdateMainMemoLineMarkers(FMainMemo.StepLine);
  end;
end;

{ok}procedure TCompileForm.HideMainMemoError;
begin
  SetMainMemoErrorLine(-1);
  if not FCompiling then
    StatusBar.Panels[spExtraStatus].Text := '';
end;

{ok}procedure TCompileForm.UpdateCaretPosPanel;
begin
  StatusBar.Panels[spCaretPos].Text := Format('%4d:%4d', [FActiveMemo.CaretLine + 1,
    FActiveMemo.CaretColumnExpanded + 1]);
end;

{ok}procedure TCompileForm.UpdateEditModePanel;
const
  InsertText: array[Boolean] of String = ('Overwrite', 'Insert');
begin
  if FActiveMemo.ReadOnly then
    StatusBar.Panels[spEditMode].Text := 'Read only'
  else
    StatusBar.Panels[spEditMode].Text := InsertText[FActiveMemo.InsertMode];
end;

{ok}procedure TCompileForm.UpdateModifiedPanel;
begin
  if FActiveMemo.Modified then
    StatusBar.Panels[spModified].Text := 'Modified'
  else
    StatusBar.Panels[spModified].Text := '';
end;

procedure TCompileForm.UpdateIncludedFiles;
var
  NewTabs: TStringList;
  IncludedFile: TIncludedFile;
  I, NextMemoIndex: Integer;
  SaveTabName: String;
begin
  if FOptions.OpenIncludedFiles and (FIncludedFiles.Count > 0) then begin
    NewTabs := TStringList.Create;
    try
      NewTabs.Add(MemosTabSet.Tabs[0]); { 'Main Script' }
      NextMemoIndex := FirstIncludedFilesMemoIndex;
      FLoadingIncludedFiles := True;
      try
        for I := 0 to FIncludedFiles.Count-1 do begin
          IncludedFile := FIncludedFiles[I];
          NewTabs.Add(PathExtractName(IncludedFile.Filename));
          IncludedFile.Memo := FMemos[NextMemoIndex];
          OpenFile(IncludedFile.Memo, IncludedFile.Filename, False);
          Inc(NextMemoIndex);
          if NextMemoIndex = FMemos.Count then
            Break; { We're out of memos :( }
        end;
      finally
        FLoadingIncludedFiles := False;
      end;
      { Hide any remaining memos }
      for I := NextMemoIndex to FMemos.Count-1 do
        FMemos[I].Visible := False;
      { Set new tabs, try keep same file open }
      SaveTabName := MemosTabSet.Tabs[MemosTabSet.TabIndex];
      MemosTabSet.Tabs := NewTabs;
      I := MemosTabSet.Tabs.IndexOf(SaveTabName);
      if I <> -1 then
         MemosTabSet.TabIndex := I;
    finally
      NewTabs.Free;
    end;
    MemosTabSet.Visible := True;
  end else begin
    for I := FirstIncludedFilesMemoIndex to FMemos.Count-1 do
      FMemos[I].Visible := False;
    for I := 0 to FIncludedFiles.Count-1 do
      FIncludedFiles[I].Memo := nil;
    MemosTabSet.Visible := False;
    MemosTabSet.TabIndex := 0; { For next time }
  end;
  
  UpdateBevel1;
end;

procedure TCompileForm.MemoUpdateUI(Sender: TObject);

  procedure UpdatePendingSquiggly;
  var
    Pos: Integer;
    Value: Boolean;
  begin
    { Check for the inPendingSquiggly indicator on either side of the caret }
    Pos := FMainMemo.CaretPosition;
    Value := False;
    if FMainMemo.CaretVirtualSpace = 0 then begin
      Value := (inPendingSquiggly in FMainMemo.GetIndicatorsAtPosition(Pos));
      if not Value and (Pos > 0) then
        Value := (inPendingSquiggly in FMainMemo.GetIndicatorsAtPosition(Pos-1));
    end;
    if FOnPendingSquiggly <> Value then begin
      FOnPendingSquiggly := Value;
      { If caret has left a pending squiggly, force restyle of the line }
      if not Value then begin
        { Stop reporting the caret position to the styler (until the next
          Change event) so the token doesn't re-enter pending-squiggly state
          if the caret comes back and something restyles the line }
        FMainMemo.ReportCaretPositionToStyler := False;
        FMainMemo.RestyleLine(FMainMemo.GetLineFromPosition(FPendingSquigglyCaretPos));
      end;
    end;
    FPendingSquigglyCaretPos := Pos;
  end;

  procedure UpdateBraceHighlighting;
  var
    Section: TInnoSetupStylerSection;
    Pos, MatchPos: Integer;
    C: AnsiChar;
  begin
    Section := FMemosStyler.GetSectionFromLineState(FMainMemo.Lines.State[FMainMemo.CaretLine]);
    if (Section <> scNone) and (FMainMemo.CaretVirtualSpace = 0) then begin
      Pos := FMainMemo.CaretPosition;
      C := FMainMemo.GetCharAtPosition(Pos);
      if C in ['(', '[', '{'] then begin
        MatchPos := FMainMemo.GetPositionOfMatchingBrace(Pos);
        if MatchPos >= 0 then begin
          FMainMemo.SetBraceHighlighting(Pos, MatchPos);
          Exit;
        end;
      end;
      if Pos > 0 then begin
        Pos := FMainMemo.GetPositionBefore(Pos);
        C := FMainMemo.GetCharAtPosition(Pos);
        if C in [')', ']', '}'] then begin
          MatchPos := FMainMemo.GetPositionOfMatchingBrace(Pos);
          if MatchPos >= 0 then begin
            FMainMemo.SetBraceHighlighting(Pos, MatchPos);
            Exit;
          end;
        end;
      end;
    end;
    FMainMemo.SetBraceHighlighting(-1, -1);
  end;

begin
  if (FMainMemo.ErrorLine < 0) or (FMainMemo.CaretPosition <> FMainMemo.ErrorCaretPosition) then
    HideMainMemoError;
  UpdateCaretPosPanel;
  UpdatePendingSquiggly;
  UpdateBraceHighlighting;
  UpdateEditModePanel;
end;

{ok}procedure TCompileForm.MemoModifiedChange(Sender: TObject);
begin
  UpdateModifiedPanel;
end;

procedure TCompileForm.MemoChange(Sender: TObject; const Info: TScintEditChangeInfo);

  procedure MainMemoLinesInsertedOrDeleted;
  var
    FirstAffectedLine, Line, LinePos: Integer;
  begin
    Line := FMainMemo.GetLineFromPosition(Info.StartPos);
    LinePos := FMainMemo.GetPositionFromLine(Line);
    FirstAffectedLine := Line;
    { If the deletion/insertion does not start on the first character of Line,
      then we consider the first deleted/inserted line to be the following
      line (Line+1). This way, if you press Del at the end of line 1, the dot
      on line 2 is removed, while line 1's dot stays intact. }
    if Info.StartPos > LinePos then
      Inc(Line);
    if Info.LinesDelta > 0 then
      MainMemoLinesInserted(Line, Info.LinesDelta)
    else
      MainMemoLinesDeleted(Line, -Info.LinesDelta, FirstAffectedLine);
  end;

begin
  FModifiedSinceLastCompile := True;
  if FDebugging then
    FModifiedSinceLastCompileAndGo := True
  else if not FLoadingIncludedFiles then begin
    { Modified while not debugging or loading included files; free the debug info and clear the dots }
    DestroyDebugInfo;
  end;

  if FActiveMemo = FMainMemo then begin
    if Info.LinesDelta <> 0 then
      MainMemoLinesInsertedOrDeleted;

    { When the Delete key is pressed, the caret doesn't move, so reset
      FErrorCaretPosition to ensure that OnUpdateUI calls HideError }
    FMainMemo.ErrorCaretPosition := -1;
  end;

  { The change should trigger restyling. Allow the styler to see the current
    caret position in case it wants to set a pending squiggly indicator. }
  FActiveMemo.ReportCaretPositionToStyler := True;
end;

procedure TCompileForm.InitiateAutoComplete(const Key: AnsiChar);
var
  CaretPos, Line, LinePos, WordStartPos, WordEndPos, CharsBefore, I,
    LangNamePos: Integer;
  Section: TInnoSetupStylerSection;
  IsParamSection: Boolean;
  WordList: AnsiString;
  FoundSemicolon, FoundDot: Boolean;
  C: AnsiChar;
begin
  if FMainMemo.AutoCompleteActive or FMainMemo.ReadOnly then
    Exit;

  FMainMemo.CaretPosition := FMainMemo.CaretPosition;  { clear any selection }
  CaretPos := FMainMemo.CaretPosition;
  Line := FMainMemo.GetLineFromPosition(CaretPos);
  Section := FMemosStyler.GetSectionFromLineState(FMainMemo.Lines.State[Line]);

  WordList := FMemosStyler.KeywordList[Section];
  if WordList = '' then
    Exit;
  IsParamSection := FMemosStyler.IsParamSection(Section);

  LinePos := FMainMemo.GetPositionFromLine(Line);
  WordStartPos := FMainMemo.GetWordStartPosition(CaretPos, True);
  WordEndPos := FMainMemo.GetWordEndPosition(CaretPos, True);
  CharsBefore := CaretPos - WordStartPos;

  { Don't start autocompletion after a character is typed if there are any
    word characters adjacent to the character }
  if Key <> #0 then begin
    if CharsBefore > 1 then
      Exit;
    if WordEndPos > CaretPos then
      Exit;
  end;

  { Only allow autocompletion if no non-whitespace characters exist before
    the current word on the line, or after the last ';' in parameterized
    sections }
  FoundSemicolon := False;
  FoundDot := False;
  I := WordStartPos;
  while I > LinePos do begin
    I := FMainMemo.GetPositionBefore(I);
    if I < LinePos then
      Exit;  { shouldn't get here }
    C := FMainMemo.GetCharAtPosition(I);
    { Make sure it's an stSymbol ';' and not one inside a quoted string }
    if IsParamSection and (C = ';') and
       FMemosStyler.IsSymbolStyle(FMainMemo.GetStyleAtPosition(I)) then begin
      FoundSemicolon := True;
      Break;
    end;
    if (Section = scLangOptions) and (C = '.') and not FoundDot then begin
      { Verify that a word (language name) precedes the '.', then check for
        any non-whitespace characters before the word }
      LangNamePos := FMainMemo.GetWordStartPosition(I, True);
      if LangNamePos >= I then
        Exit;
      I := LangNamePos;
      FoundDot := True;
    end
    else begin
      if C > ' ' then
        Exit;
    end;
  end;
  { Space can only initiate autocompletion after a semicolon in a
    parameterized section }
  if (Key = ' ') and not FoundSemicolon then
    Exit;

  if IsParamSection then
    FMainMemo.SetAutoCompleteFillupChars(':')
  else
    FMainMemo.SetAutoCompleteFillupChars('=');
  FMainMemo.ShowAutoComplete(CharsBefore, WordList);
end;

procedure TCompileForm.MemoCharAdded(Sender: TObject; Ch: AnsiChar);

  function LineIsBlank(const Line: Integer): Boolean;
  var
    S: TScintRawString;
    I: Integer;
  begin
    S := FMainMemo.Lines.RawLines[Line];
    for I := 1 to Length(S) do
      if not(S[I] in [#9, ' ']) then begin
        Result := False;
        Exit;
      end;
    Result := True;
  end;

var
  NewLine, PreviousLine, NewIndent, PreviousIndent: Integer;
  RestartAutoComplete: Boolean;
begin
  if FOptions.AutoIndent and (Ch = FMainMemo.LineEndingString[Length(FMainMemo.LineEndingString)]) then begin
    { Add to the new line any (remaining) indentation from the previous line }
    NewLine := FMainMemo.CaretLine;
    PreviousLine := NewLine-1;
    if PreviousLine >= 0 then begin
      NewIndent := FMainMemo.GetLineIndentation(NewLine);
      { If no indentation was moved from the previous line to the new line
        (i.e., there are no spaces/tabs directly to the right of the new
        caret position), and the previous line is completely empty (0 length),
        then use the indentation from the last line containing non-space
        characters. }
      if (NewIndent = 0) and (FMainMemo.Lines.RawLineLengths[PreviousLine] = 0) then begin
        Dec(PreviousLine);
        while (PreviousLine >= 0) and LineIsBlank(PreviousLine) do
          Dec(PreviousLine);
      end;
      if PreviousLine >= 0 then begin
        PreviousIndent := FMainMemo.GetLineIndentation(PreviousLine);
        { If virtual space is enabled, and tabs are not being used for
          indentation (typing in virtual space doesn't create tabs), then we
          don't actually have to set any indentation if the new line is
          empty; we can just move the caret out into virtual space. }
        if (svsUserAccessible in FMainMemo.VirtualSpaceOptions) and
           not FMainMemo.UseTabCharacter and
           (FMainMemo.Lines.RawLineLengths[NewLine] = 0) then begin
          FMainMemo.CaretVirtualSpace := PreviousIndent;
        end
        else begin
          FMainMemo.SetLineIndentation(NewLine, NewIndent + PreviousIndent);
          FMainMemo.CaretPosition := FMainMemo.GetPositionFromLineExpandedColumn(NewLine,
            PreviousIndent);
        end;
      end;
    end;
  end;

  case Ch of
    'A'..'Z', 'a'..'z', '_':
      if FOptions.AutoComplete then
        InitiateAutoComplete(Ch);
  else
    RestartAutoComplete := (Ch in [' ', '.']) and
      (FOptions.AutoComplete or FMainMemo.AutoCompleteActive);
    FMainMemo.CancelAutoComplete;
    if RestartAutoComplete then
      InitiateAutoComplete(Ch);
  end;
end;

procedure TCompileForm.MainMemoHintShow(Sender: TObject; var Info: TScintHintInfo);

  function GetCodeVariableDebugEntryFromLineCol(Line, Col: Integer): PVariableDebugEntry;
  var
    I: Integer;
  begin
    { FVariableDebugEntries uses 1-based line and column numbers }
    Inc(Line);
    Inc(Col);
    Result := nil;
    for I := 0 to FVariableDebugEntriesCount-1 do begin
      if (FVariableDebugEntries[I].LineNumber = Line) and
         (FVariableDebugEntries[I].Col = Col) then begin
        Result := @FVariableDebugEntries[I];
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
    { On the Unicode build, [Code] lines get converted from the editor's
      UTF-8 to UTF-16 Strings when passed to the compiler. This can lead to
      column number discrepancies between Scintilla and ROPS. This code
      simulates the conversion to try to find out where ROPS thinks a Pos
      resides. }
    LinePos := FMainMemo.GetPositionFromLine(FMainMemo.GetLineFromPosition(Pos));
    S := FMainMemo.GetRawTextRange(LinePos, Pos);
    U := FMainMemo.ConvertRawStringToString(S);
    Result := Length(U);
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
    Line := FMainMemo.GetLineFromPosition(Pos);
    LineEndPos := FMainMemo.GetLineEndPosition(Line);
    I := FMainMemo.GetPositionFromLine(Line);
    while I < LineEndPos do begin
      if (I > Pos) and (BraceLevel = 0) then
        Break;
      C := FMainMemo.GetCharAtPosition(I);
      if C = '{' then begin
        if FMainMemo.GetCharAtPosition(I + 1) = '{' then
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
      I := FMainMemo.GetPositionAfter(I);
    end;
  end;

var
  Pos, Line, I, J: Integer;
  Output: String;
  DebugEntry: PVariableDebugEntry;
  ConstRange: TScintRange;
begin
  if FDebugClientWnd = 0 then
    Exit;
  Pos := FMainMemo.GetPositionFromPoint(Info.CursorPos, True, True);
  if Pos < 0 then
    Exit;
  Line := FMainMemo.GetLineFromPosition(Pos);

  { Check if cursor is over a [Code] variable }
  if FMemosStyler.GetSectionFromLineState(FMainMemo.Lines.State[Line]) = scCode then begin
    { Note: The '+ 1' is needed so that when the mouse is over a '.'
      between two words, it won't match the word to the left of the '.' }
    I := FMainMemo.GetWordStartPosition(Pos + 1, True);
    J := FMainMemo.GetWordEndPosition(Pos, True);
    if J > I then begin
      DebugEntry := GetCodeVariableDebugEntryFromLineCol(Line,
        GetCodeColumnFromPosition(I));
      if DebugEntry <> nil then begin
        case EvaluateVariableEntry(DebugEntry, Output) of
          1: Info.HintStr := Output;
          2: Info.HintStr := Output;
        else
          Info.HintStr := 'Unknown error';
        end;
        Info.CursorRect.TopLeft := FMainMemo.GetPointFromPosition(I);
        Info.CursorRect.BottomRight := FMainMemo.GetPointFromPosition(J);
        Info.CursorRect.Bottom := Info.CursorRect.Top + FMainMemo.LineHeight;
        Info.HideTimeout := High(Integer);  { infinite }
        Exit;
      end;
    end;
  end;

  { Check if cursor is over a constant }
  ConstRange := FindConstRange(Pos);
  if ConstRange.EndPos > ConstRange.StartPos then begin
    Info.HintStr := FMainMemo.GetTextRange(ConstRange.StartPos, ConstRange.EndPos);
    case EvaluateConstant(Info.HintStr, Output) of
      1: Info.HintStr := Info.HintStr + ' = "' + Output + '"';
      2: Info.HintStr := Info.HintStr + ' = Exception: ' + Output;
    else
      Info.HintStr := Info.HintStr + ' = Unknown error';
    end;
    Info.CursorRect.TopLeft := FMainMemo.GetPointFromPosition(ConstRange.StartPos);
    Info.CursorRect.BottomRight := FMainMemo.GetPointFromPosition(ConstRange.EndPos);
    Info.CursorRect.Bottom := Info.CursorRect.Top + FMainMemo.LineHeight;
    Info.HideTimeout := High(Integer);  { infinite }
  end;
end;

{ok}procedure TCompileForm.MemoDropFiles(Sender: TObject; X, Y: Integer;
  AFiles: TStrings);
begin
  if (AFiles.Count > 0) and ConfirmCloseFile(True) then
    OpenFile(FActiveMemo, AFiles[0], True);
end;

{ok}procedure TCompileForm.StatusBarResize(Sender: TObject);
begin
  { Without this, on Windows XP with themes, the status bar's size grip gets
    corrupted as the form is resized }
  if StatusBar.HandleAllocated then
    InvalidateRect(StatusBar.Handle, nil, True);
end;

{ok}procedure TCompileForm.WMDebuggerQueryVersion(var Message: TMessage);
begin
  Message.Result := FCompilerVersion.BinVersion;
end;

procedure TCompileForm.WMDebuggerHello(var Message: TMessage);
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

{ok}procedure TCompileForm.WMDebuggerGoodbye(var Message: TMessage);
begin
  ReplyMessage(0);
  DebuggingStopped(True);
end;

function TCompileForm.GetLineNumberFromEntry(Kind, Index: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FDebugEntriesCount-1 do begin
    if (FDebugEntries[I].Kind = Kind) and
       (FDebugEntries[I].Index = Index) then begin
      Result := FDebugEntries[I].LineNumber;
      Break;
    end;
  end;
end;

procedure TCompileForm.BringToForeground;
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

procedure TCompileForm.DebuggerStepped(var Message: TMessage; const Intermediate: Boolean);
var
  LineNumber: Integer;
begin
  LineNumber := GetLineNumberFromEntry(Message.WParam, Message.LParam);
  if LineNumber < 0 then
    Exit;

  if (LineNumber < FMainMemo.LineStateCount) and
     (FMainMemo.LineState[LineNumber] <> lnEntryProcessed) then begin
    FMainMemo.LineState[LineNumber] := lnEntryProcessed;
    UpdateMainMemoLineMarkers(LineNumber);
  end;

  if (FStepMode = smStepInto) or
     ((FStepMode = smStepOver) and not Intermediate) or
     ((FStepMode = smRunToCursor) and
      (FRunToCursorPoint.Kind = Integer(Message.WParam)) and
      (FRunToCursorPoint.Index = Message.LParam)) or
     (FMainMemo.BreakPoints.IndexOf(Pointer(LineNumber)) <> -1) then begin
    MoveMainMemoCaret(LineNumber, True);
    HideMainMemoError;
    SetMainMemoStepLine(LineNumber);
    BringToForeground;
    { Tell Setup to pause }
    Message.Result := 1;
    FPaused := True;
    UpdateRunMenu;
    UpdateCaption;
  end;
end;

procedure TCompileForm.WMDebuggerStepped(var Message: TMessage);
begin
  DebuggerStepped(Message, False);
end;

procedure TCompileForm.WMDebuggerSteppedIntermediate(var Message: TMessage);
begin
  DebuggerStepped(Message, True);
end;

procedure TCompileForm.WMDebuggerException(var Message: TMessage);
var
  LineNumber: Integer;
begin
  if FOptions.PauseOnDebuggerExceptions then begin
    LineNumber := GetLineNumberFromEntry(Message.WParam, Message.LParam);

    if (LineNumber >= 0) then begin
      MoveMainMemoCaret(LineNumber, True);
      SetMainMemoStepLine(-1);
      SetMainMemoErrorLine(LineNumber);
    end;

    BringToForeground;
    { Tell Setup to pause }
    Message.Result := 1;
    FPaused := True;
    UpdateRunMenu;
    UpdateCaption;

    ReplyMessage(Message.Result);  { so that Setup enters a paused state now }
    if LineNumber >= 0 then
      MsgBox(Format('Line %d:' + SNewLine + '%s.', [LineNumber + 1, FDebuggerException]), 'Runtime Error', mbCriticalError, mb_Ok)
    else
      MsgBox(FDebuggerException + '.', 'Runtime Error', mbCriticalError, mb_Ok);
  end;
end;

procedure TCompileForm.WMDebuggerSetForegroundWindow(var Message: TMessage);
begin
  SetForegroundWindow(HWND(Message.WParam));
end;

procedure TCompileForm.WMDebuggerCallStackCount(var Message: TMessage);
begin
  FCallStackCount := Message.WParam;
end;

procedure TCompileForm.WMCopyData(var Message: TWMCopyData);
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

procedure TCompileForm.DestroyDebugInfo;
var
  HadDebugInfo: Boolean;
begin
  HadDebugInfo := Assigned(FMainMemo.LineState);

  FMainMemo.LineStateCapacity := 0;
  FMainMemo.LineStateCount := 0;
  FreeMem(FMainMemo.LineState);
  FMainMemo.LineState := nil;

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
    UpdateAllMainMemoLineMarkers;
end;

procedure TCompileForm.ParseDebugInfo(DebugInfo: Pointer);
{ This creates and fills the DebugEntries and FLineState arrays }
var
  Header: PDebugInfoHeader;
  Size: Cardinal;
  I: Integer;
begin
  DestroyDebugInfo;

  Header := DebugInfo;
  if (Header.ID <> DebugInfoHeaderID) or
     (Header.Version <> DebugInfoHeaderVersion) then
    raise Exception.Create('Unrecognized debug info format');

  try
    I := FMainMemo.Lines.Count;
    FMainMemo.LineState := AllocMem(SizeOf(TLineState) * (I + LineStateGrowAmount));
    FMainMemo.LineStateCapacity := I + LineStateGrowAmount;
    FMainMemo.LineStateCount := I;

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

    for I := 0 to FDebugEntriesCount-1 do begin
      if (FDebugEntries[I].LineNumber >= 0) and
         (FDebugEntries[I].LineNumber < FMainMemo.LineStateCount) then begin
        if FMainMemo.LineState[FDebugEntries[I].LineNumber] = lnUnknown then
          FMainMemo.LineState[FDebugEntries[I].LineNumber] := lnHasEntry;
      end;
    end;
    UpdateAllMainMemoLineMarkers;
  except
    DestroyDebugInfo;
    raise;
  end;
end;

procedure TCompileForm.ResetMainMemoLineState;
{ Changes green dots back to grey dots }
var
  I: Integer;
begin
  for I := 0 to FMainMemo.LineStateCount-1 do
    if FMainMemo.LineState[I] = lnEntryProcessed then begin
      FMainMemo.LineState[I] := lnHasEntry;
      UpdateMainMemoLineMarkers(I);
    end;
end;

procedure TCompileForm.CheckIfTerminated;
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

procedure TCompileForm.DebuggingStopped(const WaitForTermination: Boolean);

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
  HideMainMemoError;
  SetMainMemoStepLine(-1);
  UpdateRunMenu;
  UpdateCaption;
  DebugLogMessage('*** ' + ExitCodeText);
  StatusBar.Panels[spExtraStatus].Text := ' ' + ExitCodeText;
end;

procedure TCompileForm.DetachDebugger;
begin
  CheckIfTerminated;
  if not FDebugging then Exit;
  SendNotifyMessage(FDebugClientWnd, WM_DebugClient_Detach, 0, 0);
  DebuggingStopped(False);
end;

function TCompileForm.AskToDetachDebugger: Boolean;
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

procedure TCompileForm.UpdateRunMenu;
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
  RRunToCursor.Enabled := (FActiveMemo = FMainMemo) and RRun.Enabled;
  RStepInto.Enabled := RRun.Enabled;
  RStepOver.Enabled := RRun.Enabled;
  RTerminate.Enabled := FDebugging and (FDebugClientWnd <> 0);
  TerminateButton.Enabled := RTerminate.Enabled;
  REvaluate.Enabled := FDebugging and (FDebugClientWnd <> 0);
end;

procedure TCompileForm.UpdateTargetMenu;
begin
  if FDebugTarget = dtSetup then begin
    RTargetSetup.Checked := True;
    TargetSetupButton.Down := True;
  end else begin
    RTargetUninstall.Checked := True;
    TargetUninstallButton.Down := True;
  end;
end;

{ok}procedure TCompileForm.UpdateTheme;

  procedure SetControlTheme(const WinControl: TWinControl);
  begin
    if UseThemes then begin
      if FTheme.Dark then
        SetWindowTheme(WinControl.Handle, 'DarkMode_Explorer', nil)
      else
        SetWindowTheme(WinControl.Handle, nil, nil);
    end;
  end;

var
  Memo: TCompScintEdit;
begin
  FTheme.Typ := FOptions.ThemeType;
  for Memo in FMemos do begin
    Memo.UpdateThemeColors;
    Memo.UpdateStyleAttributes;
    SetControlTheme(Memo);
  end;
  ToolBarPanel.ParentBackground := False;
  ToolBarPanel.Color := FTheme.Colors[tcToolBack];
  if FTheme.Dark then
    ToolBarVirtualImageList.ImageCollection := DarkToolBarImageCollection
  else
    ToolBarVirtualImageList.ImageCollection := LightToolBarImageCollection;
  UpdateBevel1;
  SplitPanel.ParentBackground := False;
  SplitPanel.Color := FTheme.Colors[tcSplitterBack];
  if FTheme.Dark then begin
    MemosTabSet.Theme := FTheme;
    OutputTabSet.Theme := FTheme;
  end else begin
    MemosTabSet.Theme := nil;
    OutputTabSet.Theme := nil;
  end;
  CompilerOutputList.Font.Color := FTheme.Colors[tcFore];
  CompilerOutputList.Color := FTheme.Colors[tcBack];
  CompilerOutputList.Invalidate;
  SetControlTheme(CompilerOutputList);
  DebugOutputList.Font.Color := FTheme.Colors[tcFore];
  DebugOutputList.Color := FTheme.Colors[tcBack];
  DebugOutputList.Invalidate;
  SetControlTheme(DebugOutputList);
  DebugCallStackList.Font.Color := FTheme.Colors[tcFore];
  DebugCallStackList.Color := FTheme.Colors[tcBack];
  DebugCallStackList.Invalidate;
  SetControlTheme(DebugCallStackList);
end;

{ok}procedure TCompileForm.UpdateThemeData(const Close, Open: Boolean);
begin
  if Close then begin
    if FProgressThemeData <> 0 then begin
      CloseThemeData(FProgressThemeData);
      FProgressThemeData := 0;
    end;
  end;

  if Open then begin
    if UseThemes then begin
      FProgressThemeData := OpenThemeData(Handle, 'Progress');
      if (GetThemeInt(FProgressThemeData, 0, 0, TMT_PROGRESSCHUNKSIZE, FProgressChunkSize) <> S_OK) or
         (FProgressChunkSize <= 0) then
        FProgressChunkSize := 6;
      if (GetThemeInt(FProgressThemeData, 0, 0, TMT_PROGRESSSPACESIZE, FProgressSpaceSize) <> S_OK) or
         (FProgressSpaceSize < 0) then  { ...since "OpusOS" theme returns a bogus -1 value }
        FProgressSpaceSize := 2;
    end else
      FProgressThemeData := 0;
  end;
end;

procedure TCompileForm.StartProcess;
const
  SEE_MASK_NOZONECHECKS = $00800000;
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

  ResetMainMemoLineState;
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
  Info.Wnd := Application.Handle;
  if FOptions.RunAsDifferentUser and (Win32MajorVersion >= 5) then
    Info.lpVerb := 'runas'
  else
    Info.lpVerb := 'open';
  Info.lpFile := PChar(RunFilename);
  Info.lpParameters := PChar(RunParameters);
  WorkingDir := PathExtractDir(RunFilename);
  Info.lpDirectory := PChar(WorkingDir);
  Info.nShow := SW_SHOWNORMAL;
  { Disable windows so that the user can't click other things while a "Run as"
    dialog is up on Windows 2000/XP (they aren't system modal like on Vista) }
  SaveFocusWindow := GetFocus;
  WindowList := DisableTaskWindows(0);
  try
    { Also temporarily remove the focus since a disabled window's children can
      still receive keystrokes. This is needed on Vista if the UAC dialog
      doesn't come to the foreground for some reason (e.g. if the following
      SetActiveWindow call is removed). }
    Windows.SetFocus(0);
    { On Vista, when disabling windows, we have to make the application window
      the active window, otherwise the UAC dialog doesn't come to the
      foreground automatically. Note: This isn't done on older versions simply
      to avoid unnecessary title bar flicker. }
    if Win32MajorVersion >= 6 then
      SetActiveWindow(Application.Handle);
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

procedure TCompileForm.CompileIfNecessary;

  function IncludedFileModifiedSinceLastCompile: Boolean;
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

  { Display warning if the user modified the script while running - does not support included files  }
  if FDebugging and FModifiedSinceLastCompileAndGo then begin
    if MsgBox('The changes you made will not take effect until you ' +
       're-compile.' + SNewLine2 + 'Continue running anyway?',
       SCompilerFormCaption, mbError, MB_YESNO) <> IDYES then
      Abort;
    FModifiedSinceLastCompileAndGo := False;
    { The process may have terminated while the message box was up; check,
      and if it has, we want to recompile below }
    CheckIfTerminated;
  end;

  if not FDebugging and (FModifiedSinceLastCompile or IncludedFileModifiedSinceLastCompile) then
    CompileFile('', False);
end;

procedure TCompileForm.Go(AStepMode: TStepMode);
begin
  CompileIfNecessary;
  FStepMode := AStepMode;
  HideMainMemoError;
  SetMainMemoStepLine(-1);
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

function TCompileForm.EvaluateConstant(const S: String;
  var Output: String): Integer;
begin
  FReplyString := '';
  Result := SendCopyDataMessageStr(FDebugClientWnd, Handle,
    CD_DebugClient_EvaluateConstantW, S);
  if Result > 0 then
    Output := FReplyString;
end;

function TCompileForm.EvaluateVariableEntry(const DebugEntry: PVariableDebugEntry;
  var Output: String): Integer;
begin
  FReplyString := '';
  Result := SendCopyDataMessage(FDebugClientWnd, Handle, CD_DebugClient_EvaluateVariableEntry,
    DebugEntry, SizeOf(DebugEntry^));
  if Result > 0 then
    Output := FReplyString;
end;

procedure TCompileForm.RRunClick(Sender: TObject);
begin
  Go(smRun);
end;

procedure TCompileForm.RParametersClick(Sender: TObject);
begin
  ReadMRUParametersList;
  InputQueryCombo('Run Parameters', 'Command line parameters for ' + DebugTargetStrings[dtSetup] +
    ' and ' + DebugTargetStrings[dtUninstall] + ':', FRunParameters, FMRUParametersList);
  if FRunParameters <> '' then
    ModifyMRUParametersList(FRunParameters, True);
end;

procedure TCompileForm.RPauseClick(Sender: TObject);
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

procedure TCompileForm.RRunToCursorClick(Sender: TObject);

  function GetDebugEntryFromLineNumber(LineNumber: Integer;
    var DebugEntry: TDebugEntry): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to FDebugEntriesCount-1 do begin
      if FDebugEntries[I].LineNumber = LineNumber then begin
        DebugEntry := FDebugEntries[I];
        Result := True;
        Break;
      end;
    end;
  end;

begin
  CompileIfNecessary;
  if not GetDebugEntryFromLineNumber(FMainMemo.CaretLine, FRunToCursorPoint) then begin
    MsgBox('No code was generated for the current line.', SCompilerFormCaption,
      mbError, MB_OK);
    Exit;
  end;
  Go(smRunToCursor);
end;

procedure TCompileForm.RStepIntoClick(Sender: TObject);
begin
  Go(smStepInto);
end;

procedure TCompileForm.RStepOverClick(Sender: TObject);
begin
  Go(smStepOver);
end;

procedure TCompileForm.RTerminateClick(Sender: TObject);
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

procedure TCompileForm.REvaluateClick(Sender: TObject);
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

procedure TCompileForm.CheckIfRunningTimerTimer(Sender: TObject);
begin
  { In cases of normal Setup termination, we receive a WM_Debugger_Goodbye
    message. But in case we don't get that, use a timer to periodically check
    if the process is no longer running. }
  CheckIfTerminated;
end;

procedure TCompileForm.PListCopyClick(Sender: TObject);
var
  ListBox: TListBox;
  Text: String;
  I: Integer;
begin
  if CompilerOutputList.Visible then
    ListBox := CompilerOutputList
  else if DebugOutputList.Visible then
    ListBox := DebugOutputList
  else
    ListBox := DebugCallStackList;
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

procedure TCompileForm.PListSelectAllClick(Sender: TObject);
var
  ListBox: TListBox;
  I: Integer;
begin
  if CompilerOutputList.Visible then
    ListBox := CompilerOutputList
  else if DebugOutputList.Visible then
    ListBox := DebugOutputList
  else
    ListBox := DebugCallStackList;
  ListBox.Items.BeginUpdate;
  try
    for I := 0 to ListBox.Items.Count-1 do
      ListBox.Selected[I] := True;
  finally
    ListBox.Items.EndUpdate;
  end;
end;

procedure TCompileForm.AppOnIdle(Sender: TObject; var Done: Boolean);
begin
  { For an explanation of this, see the comment where HandleMessage is called }
  if FCompiling then
    Done := False;

  FBecameIdle := True;
end;

procedure TCompileForm.EGotoClick(Sender: TObject);
var
  S: String;
  L: Integer;
begin
  S := IntToStr(FMainMemo.CaretLine + 1);
  if InputQuery('Go to Line', 'Line number:', S) then begin
    L := StrToIntDef(S, Low(L));
    if L <> Low(L) then
      FMainMemo.CaretLine := L - 1;
  end;
end;

procedure TCompileForm.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  R, BR: TRect;
  W, ChunkCount: Integer;
begin
  case Panel.Index of
    spCompileIcon:
      if FCompiling then begin
        ImageList_Draw(BuildImageList.Handle, FBuildAnimationFrame, StatusBar.Canvas.Handle,
          Rect.Left + ((Rect.Right - Rect.Left) - BuildImageList.Width) div 2,
          Rect.Top + ((Rect.Bottom - Rect.Top) - BuildImageList.Height) div 2, ILD_NORMAL);
      end;
    spCompileProgress:
      if FCompiling and (FProgressMax > 0) then begin
        R := Rect;
        InflateRect(R, -2, -2);
        if FProgressThemeData = 0 then begin
          R.Right := R.Left + MulDiv(FProgress, R.Right - R.Left,
            FProgressMax);
          StatusBar.Canvas.Brush.Color := clHighlight;
          StatusBar.Canvas.FillRect(R);
        end else begin
          DrawThemeBackground(FProgressThemeData, StatusBar.Canvas.Handle, PP_BAR, 0, R, nil);
          BR := R;
          GetThemeBackgroundContentRect(FProgressThemeData, StatusBar.Canvas.Handle, PP_BAR, 0, BR, @R);
          IntersectClipRect(StatusBar.Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
          W := MulDiv(FProgress, R.Right - R.Left, FProgressMax);
          ChunkCount := W div (FProgressChunkSize + FProgressSpaceSize);
          if W mod (FProgressChunkSize + FProgressSpaceSize) > 0 then
            Inc(ChunkCount);
          R.Right := R.Left + FProgressChunkSize;
          for W := 0 to ChunkCount - 1 do
          begin
            DrawThemeBackground(FProgressThemeData, StatusBar.Canvas.Handle, PP_CHUNK, 0, R, nil);
            OffsetRect(R, FProgressChunkSize + FProgressSpaceSize, 0);
          end;
        end;
      end;
  end;
end;

procedure TCompileForm.InvalidateStatusPanel(const Index: Integer);
var
  R: TRect;
begin
  { For some reason, the VCL doesn't offer a method for this... }
  if SendMessage(StatusBar.Handle, SB_GETRECT, Index, LPARAM(@R)) <> 0 then begin
    InflateRect(R, -1, -1);
    InvalidateRect(StatusBar.Handle, @R, True);
  end;
end;

procedure TCompileForm.UpdateCompileStatusPanels(const AProgress,
  AProgressMax: Cardinal; const ASecondsRemaining: Integer;
  const ABytesCompressedPerSecond: Cardinal);
var
  T: DWORD;
begin
  { Icon panel }
  T := GetTickCount;
  if Cardinal(T - FLastAnimationTick) >= Cardinal(500) then begin
    FLastAnimationTick := T;
    InvalidateStatusPanel(spCompileIcon);
    FBuildAnimationFrame := (FBuildAnimationFrame + 1) mod 4;
    { Also update the status text twice a second }
    if ASecondsRemaining >= 0 then
      StatusBar.Panels[spExtraStatus].Text := Format(
        ' Estimated time remaining: %.2d%s%.2d%s%.2d     Average KB/sec: %.0n',
        [(ASecondsRemaining div 60) div 60, {$IFDEF IS_DXE}FormatSettings.{$ENDIF}TimeSeparator,
         (ASecondsRemaining div 60) mod 60, {$IFDEF IS_DXE}FormatSettings.{$ENDIF}TimeSeparator,
         ASecondsRemaining mod 60, ABytesCompressedPerSecond / 1024])
    else
      StatusBar.Panels[spExtraStatus].Text := '';
  end;

  { Progress panel and taskbar progress bar }
  if (FProgress <> AProgress) or
     (FProgressMax <> AProgressMax) then begin
    FProgress := AProgress;
    FProgressMax := AProgressMax;
    InvalidateStatusPanel(spCompileProgress);
    SetAppTaskbarProgressValue(AProgress, AProgressMax);
  end;
end;

procedure TCompileForm.WMSettingChange(var Message: TMessage);
begin
  if (FTheme.Typ <> ttClassic) and (Win32MajorVersion >= 10) and (Message.LParam <> 0) and (StrIComp(PChar(Message.LParam), 'ImmersiveColorSet') = 0) then begin
    FOptions.ThemeType := GetDefaultThemeType;
    UpdateTheme;
  end;
end;

procedure TCompileForm.WMThemeChanged(var Message: TMessage);
begin
  { Don't Run to Cursor into this function, it will interrupt up the theme change }
  UpdateThemeData(True, True);
  inherited;
end;

procedure TCompileForm.RTargetClick(Sender: TObject);
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

procedure TCompileForm.AppOnActivate(Sender: TObject);
const
  ReloadMessages: array[Boolean] of String = (
    'The file has been modified outside of the source editor.' + SNewLine2 +
      'Do you want to reload the file?',
    'The file has been modified outside of the source editor. Changes have ' +
      'also been made in the source editor.' + SNewLine2 + 'Do you want to ' +
      'reload the file and lose the changes made in the source editor?');
var
  NewTime: TFileTime;
  Changed: Boolean;
begin
  if FMainMemo.Filename = '' then
    Exit;

  { See if the file has been modified outside the editor }
  Changed := False;
  if GetLastWriteTimeOfFile(FMainMemo.Filename, @NewTime) then begin
    if CompareFileTime(FMainMemo.FileLastWriteTime, NewTime) <> 0 then begin
      FMainMemo.FileLastWriteTime := NewTime;
      Changed := True;
    end;
  end;

  { If it has been, offer to reload it }
  if Changed then begin
    if IsWindowEnabled(Application.Handle) then begin
      if MsgBox(FMainMemo.Filename + SNewLine2 + ReloadMessages[FMainMemo.Modified],
         SCompilerFormCaption, mbConfirmation, MB_YESNO) = IDYES then
        if ConfirmCloseFile(False) then
          OpenFile(FMainMemo, FMainMemo.Filename, False);
    end
    else begin
      { When a modal dialog is up, don't offer to reload the file. Probably
        not a good idea since the dialog might be manipulating the file. }
      MsgBox(FMainMemo.Filename + SNewLine2 + 'The file has been modified outside ' +
        'of the source editor. You might want to reload it.',
        SCompilerFormCaption, mbInformation, MB_OK);
    end;
  end;
end;

procedure TCompileForm.CompilerOutputListDrawItem(Control: TWinControl;
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

procedure TCompileForm.DebugOutputListDrawItem(Control: TWinControl;
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

procedure TCompileForm.DebugCallStackListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
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

procedure TCompileForm.OutputTabSetClick(Sender: TObject);
begin
  case OutputTabSet.TabIndex of
    tiCompilerOutput:
      begin
        CompilerOutputList.BringToFront;
        CompilerOutputList.Visible := True;
        DebugOutputList.Visible := False;
        DebugCallStackList.Visible := False;
      end;
    tiDebugOutput:
      begin
        DebugOutputList.BringToFront;
        DebugOutputList.Visible := True;
        CompilerOutputList.Visible := False;
        DebugCallStackList.Visible := False;
      end;
    tiDebugCallStack:
      begin
        DebugCallStackList.BringToFront;
        DebugCallStackList.Visible := True;
        CompilerOutputList.Visible := False;
        DebugOutputList.Visible := False;
      end;
  end;
end;

{ok}procedure TCompileForm.MainMemoToggleBreakPoint(Line: Integer);
var
  I: Integer;
begin
  I := FMainMemo.BreakPoints.IndexOf(Pointer(Line));
  if I = -1 then
    FMainMemo.BreakPoints.Add(Pointer(Line))
  else
    FMainMemo.BreakPoints.Delete(I);
  UpdateMainMemoLineMarkers(Line);
end;

{ok}procedure TCompileForm.MainMemoMarginClick(Sender: TObject; MarginNumber: Integer;
  Line: Integer);
begin
  if MarginNumber = 1 then
    MainMemoToggleBreakPoint(Line);
end;

{ok}procedure TCompileForm.MainMemoLinesInserted(FirstLine, Count: integer);
var
  I, Line: Integer;
begin
  for I := 0 to FDebugEntriesCount-1 do
    if FDebugEntries[I].LineNumber >= FirstLine then
      Inc(FDebugEntries[I].LineNumber, Count);

  if Assigned(FMainMemo.LineState) and (FirstLine < FMainMemo.LineStateCount) then begin
    { Grow FStateLine if necessary }
    I := (FMainMemo.LineStateCount + Count) - FMainMemo.LineStateCapacity;
    if I > 0 then begin
      if I < LineStateGrowAmount then
        I := LineStateGrowAmount;
      ReallocMem(FMainMemo.LineState, SizeOf(TLineState) * (FMainMemo.LineStateCapacity + I));
      Inc(FMainMemo.LineStateCapacity, I);
    end;
    { Shift existing line states and clear the new ones }
    for I := FMainMemo.LineStateCount-1 downto FirstLine do
      FMainMemo.LineState[I + Count] := FMainMemo.LineState[I];
    for I := FirstLine to FirstLine + Count - 1 do
      FMainMemo.LineState[I] := lnUnknown;
    Inc(FMainMemo.LineStateCount, Count);
  end;

  if FMainMemo.StepLine >= FirstLine then
    Inc(FMainMemo.StepLine, Count);
  if FMainMemo.ErrorLine >= FirstLine then
    Inc(FMainMemo.ErrorLine, Count);

  for I := 0 to FMainMemo.BreakPoints.Count-1 do begin
    Line := Integer(FMainMemo.BreakPoints[I]);
    if Line >= FirstLine then
      FMainMemo.BreakPoints[I] := Pointer(Line + Count);
  end;
end;

{ok}procedure TCompileForm.MainMemoLinesDeleted(FirstLine, Count,
  FirstAffectedLine: Integer);
var
  I, Line: Integer;
  DebugEntry: PDebugEntry;
begin
  for I := 0 to FDebugEntriesCount-1 do begin
    DebugEntry := @FDebugEntries[I];
    if DebugEntry.LineNumber >= FirstLine then begin
      if DebugEntry.LineNumber < FirstLine + Count then
        DebugEntry.LineNumber := -1
      else
        Dec(DebugEntry.LineNumber, Count);
    end;
  end;

  if Assigned(FMainMemo.LineState) then begin
    { Shift existing line states }
    if FirstLine < FMainMemo.LineStateCount - Count then begin
      for I := FirstLine to FMainMemo.LineStateCount - Count - 1 do
        FMainMemo.LineState[I] := FMainMemo.LineState[I + Count];
      Dec(FMainMemo.LineStateCount, Count);
    end
    else begin
      { There's nothing to shift because the last line(s) were deleted, or
        line(s) past FLineStateCount }
      if FMainMemo.LineStateCount > FirstLine then
        FMainMemo.LineStateCount := FirstLine;
    end;
  end;

  if FMainMemo.StepLine >= FirstLine then begin
    if FMainMemo.StepLine < FirstLine + Count then
      FMainMemo.StepLine := -1
    else
      Dec(FMainMemo.StepLine, Count);
  end;
  if FMainMemo.ErrorLine >= FirstLine then begin
    if FMainMemo.ErrorLine < FirstLine + Count then
      FMainMemo.ErrorLine := -1
    else
      Dec(FMainMemo.ErrorLine, Count);
  end;

  for I := FMainMemo.BreakPoints.Count-1 downto 0 do begin
    Line := Integer(FMainMemo.BreakPoints[I]);
    if Line >= FirstLine then begin
      if Line < FirstLine + Count then begin
        FMainMemo.BreakPoints.Delete(I);
      end else begin
        Line := Line - Count;
        FMainMemo.BreakPoints[I] := Pointer(Line);
      end;
    end;
  end;

  { When lines are deleted, Scintilla insists on moving all of the deleted
    lines' markers to the line on which the deletion started
    (FirstAffectedLine). This is bad for us as e.g. it can result in the line
    having two conflicting markers (or two of the same marker). There's no
    way to stop it from doing that, or to easily tell which markers came from
    which lines, so we simply delete and re-create all markers on the line. }
  UpdateMainMemoLineMarkers(FirstAffectedLine);
end;

{ok}procedure TCompileForm.UpdateMainMemoLineMarkers(const Line: Integer);
var
  NewMarker: Integer;
begin
  if Line >= FMainMemo.Lines.Count then
    Exit;

  NewMarker := -1;
  if FMainMemo.BreakPoints.IndexOf(Pointer(Line)) <> -1 then begin
    if FMainMemo.LineState = nil then
      NewMarker := mmIconBreakpoint
    else if (Line < FMainMemo.LineStateCount) and (FMainMemo.LineState[Line] <> lnUnknown) then
      NewMarker := mmIconBreakpointGood
    else
      NewMarker := mmIconBreakpointBad;
  end
  else begin
    if Line < FMainMemo.LineStateCount then begin
      case FMainMemo.LineState[Line] of
        lnHasEntry: NewMarker := mmIconHasEntry;
        lnEntryProcessed: NewMarker := mmIconEntryProcessed;
      end;
    end;
  end;

  { Delete all markers on the line. To flush out any possible duplicates,
    even the markers we'll be adding next are deleted. }
  if FMainMemo.GetMarkers(Line) <> [] then
    FMainMemo.DeleteAllMarkersOnLine(Line);

  if NewMarker <> -1 then
    FMainMemo.AddMarker(Line, NewMarker);

  if FMainMemo.StepLine = Line then
    FMainMemo.AddMarker(Line, mmLineStep)
  else if FMainMemo.ErrorLine = Line then
    FMainMemo.AddMarker(Line, mmLineError)
  else if NewMarker in [mmIconBreakpoint, mmIconBreakpointGood] then
    FMainMemo.AddMarker(Line, mmLineBreakpoint)
  else if NewMarker = mmIconBreakpointBad then
    FMainMemo.AddMarker(Line, mmLineBreakpointBad);
end;

{ok}procedure TCompileForm.UpdateAllMainMemoLineMarkers;
var
  Line: Integer;
begin
  for Line := 0 to FMainMemo.Lines.Count-1 do
    UpdateMainMemoLineMarkers(Line);
end;

procedure TCompileForm.UpdateBevel1;
begin
  Bevel1.Visible := (FTheme.Colors[tcMarginBack] = ToolBarPanel.Color) and not MemosTabSet.Visible;
end;

procedure TCompileForm.RToggleBreakPointClick(Sender: TObject);
begin
  MainMemoToggleBreakPoint(FMainMemo.CaretLine);
end;

function TCompileForm.ToCurrentPPI(const XY: Integer): Integer;
begin
  Result := MulDiv(XY, CurrentPPI, 96);
end;

function TCompileForm.FromCurrentPPI(const XY: Integer): Integer;
begin
  Result := MulDiv(XY, 96, CurrentPPI);
end;

initialization
  InitThemeLibrary;
  InitHtmlHelpLibrary;
  { For ClearType support, try to make the default font Microsoft Sans Serif }
  if DefFontData.Name = 'MS Sans Serif' then
    DefFontData.Name := AnsiString(GetPreferredUIFont);
  CoInitialize(nil);
finalization
  CoUninitialize();
end.
