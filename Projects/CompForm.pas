unit CompForm;

{
  Inno Setup
  Copyright (C) 1997-2011 Jordan Russell
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UIStateForm, StdCtrls, ExtCtrls, Menus, Buttons, ComCtrls, CommCtrl,
  ScintInt, ScintEdit, ScintStylerInnoSetup, NewTabSet,
  DebugStruct, CompInt, UxThemeISX;

const
  WM_StartCommandLineCompile = WM_USER + $1000;
  WM_StartCommandLineWizard = WM_USER + $1001;
  WM_StartNormally = WM_USER + $1002;

  MRUListMaxCount = 10;

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
  TISScintEdit = class;

  TCompileForm = class(TUIStateForm)
    MainMenu1: TMainMenu;
    FMenu: TMenuItem;
    FNew: TMenuItem;
    FOpen: TMenuItem;
    FSave: TMenuItem;
    FSaveAs: TMenuItem;
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
    FMRUSep: TMenuItem;
    VCompilerOutput: TMenuItem;
    FindDialog: TFindDialog;
    ReplaceDialog: TReplaceDialog;
    StatusPanel: TPanel;
    CompilerOutputList: TListBox;
    SplitPanel: TPanel;
    ToolbarPanel: TPanel;
    NewButton: TSpeedButton;
    Bevel1: TBevel;
    OpenButton: TSpeedButton;
    SaveButton: TSpeedButton;
    CompileButton: TSpeedButton;
    HelpButton: TSpeedButton;
    HWebsite: TMenuItem;
    VToolbar: TMenuItem;
    N7: TMenuItem;
    TOptions: TMenuItem;
    HFaq: TMenuItem;
    Bevel2: TBevel;
    Bevel3: TBevel;
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
    RunButton: TSpeedButton;
    Bevel4: TBevel;
    PauseButton: TSpeedButton;
    RPause: TMenuItem;
    RParameters: TMenuItem;
    ListPopupMenu: TPopupMenu;
    PListCopy: TMenuItem;
    HISPPSep: TMenuItem;
    N12: TMenuItem;
    BStopCompile: TMenuItem;
    StopCompileButton: TSpeedButton;
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
    TargetSetupButton: TSpeedButton;
    TargetUninstallButton: TSpeedButton;
    Bevel5: TBevel;
    TabSet: TNewTabSet;
    DebugOutputList: TListBox;
    VDebugOutput: TMenuItem;
    VHide: TMenuItem;
    N11: TMenuItem;
    SpacerPaintBox: TPaintBox;
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FExitClick(Sender: TObject);
    procedure FOpenClick(Sender: TObject);
    procedure EUndoClick(Sender: TObject);
    procedure EMenuClick(Sender: TObject);
    procedure ECutClick(Sender: TObject);
    procedure ECopyClick(Sender: TObject);
    procedure EPasteClick(Sender: TObject);
    procedure EDeleteClick(Sender: TObject);
    procedure FSaveClick(Sender: TObject);
    procedure ESelectAllClick(Sender: TObject);
    procedure FNewClick(Sender: TObject);
    procedure FNewWizardClick(Sender: TObject);
    procedure FSaveAsClick(Sender: TObject);
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
    procedure TabSetClick(Sender: TObject);
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
  private
    { Private declarations }
    FCompilerVersion: PCompilerVersionInfo;
    FFilename: String;
    FFileLastWriteTime: TFileTime;
    FSaveInUTF8Encoding: Boolean;
    FMRUMenuItems: array[0..MRUListMaxCount-1] of TMenuItem;
    FMRUList: TStringList;
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
      UnderlineErrors: Boolean;
      CursorPastEOL: Boolean;
      TabWidth: Integer;
      UseTabCharacter: Boolean;
      WordWrap: Boolean;
      AutoIndent: Boolean;
      IndentationGuides: Boolean;
      LowPriorityDuringCompile: Boolean;
    end;
    FOptionsLoaded: Boolean;
    FSignTools: TStringList;
    FCompiling: Boolean;
    FCompileWantAbort: Boolean;
    FBecameIdle: Boolean;
    FErrorLine, FStepLine: Integer;
    FErrorCaretPosition: Integer;
    FModifiedSinceLastCompile, FModifiedSinceLastCompileAndGo: Boolean;
    FDebugEntries: PDebugEntryArray;
    FDebugEntriesCount: Integer;
    FVariableDebugEntries: PVariableDebugEntryArray;
    FVariableDebugEntriesCount: Integer;
    FCompiledCodeText: AnsiString;
    FCompiledCodeDebugInfo: AnsiString;
    FLineState: PLineStateArray;
    FLineStateCapacity, FLineStateCount: Integer;
    FDebugClientWnd: HWND;
    FProcessHandle, FDebugClientProcessHandle: THandle;
    FDebugTarget: TDebugTarget;
    FCompiledExe, FUninstExe, FTempDir: String;
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
    FBuildImageList: HIMAGELIST;
    FBuildAnimationFrame: Cardinal;
    FLastAnimationTick: DWORD;
    FProgress, FProgressMax: Cardinal;
    FProgressThemeData: HTHEME;
    FProgressChunkSize, FProgressSpaceSize: Integer;
    FDebugLogListTimeWidth: Integer;
    FBreakPoints: TList;
    FOnPendingSquiggly: Boolean;
    FPendingSquigglyCaretPos: Integer;
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
    procedure DestroyDebugInfo;
    procedure DetachDebugger;
    function EvaluateConstant(const S: String; var Output: String): Integer;
    function EvaluateVariableEntry(const DebugEntry: PVariableDebugEntry;
      var Output: String): Integer;
    procedure FindNext;
    procedure Go(AStepMode: TStepMode);
    procedure HideError;
    procedure InitializeFindText(Dlg: TFindDialog);
    procedure InitiateAutoComplete(const Key: AnsiChar);
    procedure InvalidateStatusPanel(const Index: Integer);
    procedure MemoChange(Sender: TObject; const Info: TScintEditChangeInfo);
    procedure MemoCharAdded(Sender: TObject; Ch: AnsiChar);
    procedure MemoDropFiles(Sender: TObject; X, Y: Integer; AFiles: TStrings);
    procedure MemoHintShow(Sender: TObject; var Info: TScintHintInfo);
    procedure MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MemoKeyPress(Sender: TObject; var Key: Char);
    procedure MemoLinesDeleted(FirstLine, Count, FirstAffectedLine: Integer);
    procedure MemoLinesInserted(FirstLine, Count: integer);
    procedure MemoMarginClick(Sender: TObject; MarginNumber: Integer;
      Line: Integer);
    procedure MemoModifiedChange(Sender: TObject);
    procedure MemoUpdateUI(Sender: TObject);
    procedure ModifyMRUList(const AFilename: String; const AddNewItem: Boolean);
    procedure MoveCaret(const LineNumber: Integer; const AlwaysResetColumn: Boolean);
    procedure NewFile;
    procedure NewWizardFile;
    procedure OpenFile(AFilename: String; const AddToRecentDocs: Boolean);
    procedure OpenMRUFile(const AFilename: String);
    procedure ParseDebugInfo(DebugInfo: Pointer);
    procedure ReadMRUList;
    procedure ResetLineState;
    procedure StartProcess;
    function SaveFile(const SaveAs: Boolean): Boolean;
    class procedure SaveTextToFile(const Filename: String; const S: String;
      const ForceUTF8Encoding: Boolean);
    procedure SetErrorLine(ALine: Integer);
    procedure SetLowPriority(ALowPriority: Boolean);
    procedure SetStatusPanelVisible(const AVisible: Boolean);
    procedure SetStepLine(ALine: Integer);
    procedure ShowOpenDialog(const Examples: Boolean);
    procedure StatusMessage(const S: String);
    procedure SyncEditorOptions;
    procedure ToggleBreakPoint(Line: Integer);
    procedure UpdateAllLineMarkers;
    procedure UpdateCaption;
    procedure UpdateCompileStatusPanels(const AProgress, AProgressMax: Cardinal;
      const ASecondsRemaining: Integer; const ABytesCompressedPerSecond: Cardinal);
    procedure UpdateEditModePanel;
    procedure UpdateLineMarkers(const Line: Integer);
    procedure UpdateNewButtons;
    procedure UpdateRunMenu;
    procedure UpdateTargetMenu;
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
    procedure WMStartCommandLineCompile(var Message: TMessage); message WM_StartCommandLineCompile;
    procedure WMStartCommandLineWizard(var Message: TMessage); message WM_StartCommandLineWizard;
    procedure WMStartNormally(var Message: TMessage); message WM_StartNormally;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
{$IFDEF IS_D4}
  protected
    procedure WndProc(var Message: TMessage); override;
{$ENDIF}
  public
    { Public declarations }
    Memo: TISScintEdit;
    MemoStyler: TInnoSetupStyler;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFDEF IS_D5}
    function IsShortCut(var Message: TWMKey): Boolean; override;
{$ENDIF}
  end;

  TISScintEdit = class(TScintEdit)
  protected
    procedure CreateWnd; override;
  end;

var
  CompileForm: TCompileForm;

  CommandLineFilename, CommandLineWizardName: String;
  CommandLineCompile: Boolean;
  CommandLineWizard: Boolean;

function GenerateGuid: String;
procedure InitFormFont(Form: TForm);

implementation

uses
  ActiveX, Clipbrd, ShellApi, ShlObj, IniFiles, Registry, CommDlg, Consts,
  PathFunc, CmnFunc, CmnFunc2, FileClass, CompMsgs, TmSchemaISX, BrowseFunc,
  HtmlHelpFunc, TaskbarProgressFunc,
  {$IFDEF STATICCOMPILER} Compile, {$ENDIF}
  CompOptions, CompStartup, CompWizard, CompSignTools;

{$R *.DFM}
{$R CompImages.res}

const
  { Status bar panel indexes }
  spCaretPos = 0;
  spModified = 1;
  spInsertMode = 2;
  spCompileIcon = 3;
  spCompileProgress = 4;
  spExtraStatus = 5;

  { Tab set indexes }
  tiCompilerOutput = 0;
  tiDebugOutput = 1;

  { Memo marker numbers }
  mmIconHasEntry = 0;        { grey dot }
  mmIconEntryProcessed = 1;  { green dot }
  mmIconBreakpoint = 2;      { stop sign }
  mmIconBreakpointGood = 3;  { stop sign + check }
  mmIconBreakpointBad = 4;   { stop sign + X }
  mmLineError = 10;          { red line highlight }
  mmLineBreakpoint = 11;     { red line highlight }
  mmLineBreakpointBad = 12;  { ugly olive line highlight }
  mmLineStep = 13;           { blue line highlight }

  { Memo indicator numbers (also in ScintStylerInnoSetup) }
  inSquiggly = 0;
  inPendingSquiggly = 1;

  LineStateGrowAmount = 4000;

procedure InitFormFont(Form: TForm);
var
  FontName: String;
  Metrics: TNonClientMetrics;
begin
{$IFNDEF UNICODE}
  if Win32MajorVersion < 5 then begin
    { On pre-2000 Windows, just use MS Sans Serif always, except on Japanese }
    if DefFontData.Charset = SHIFTJIS_CHARSET then begin
      { MS Sans Serif can't display Japanese characters, so revert to the
        default Japanese font (requires D3+) }
      Form.Font.Handle := 0;
      Exit;
    end;
    FontName := GetPreferredUIFont;
  end
  else
{$ENDIF}
  begin
    Metrics.cbSize := SizeOf(Metrics);  { <- won't work on Delphi 2010? }
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(Metrics),
       @Metrics, 0) then
      FontName := Metrics.lfMessageFont.lfFaceName;
    { Only allow fonts that we know will fit the text correctly }
    if CompareText(FontName, 'Microsoft Sans Serif') <> 0 then
      FontName := 'Tahoma';
  end;
  Form.Font.Name := FontName;
  Form.Font.Size := 8;
end;

function GetDisplayFilename(const Filename: String): String;
var
  Buf: array[0..MAX_PATH-1] of Char;
begin
  if GetFileTitle(PChar(Filename), Buf, SizeOf(Buf)) = 0 then
    Result := Buf
  else
    Result := Filename;
end;

function GetLastWriteTimeOfFile(const Filename: String;
  var LastWriteTime: TFileTime): Boolean;
var
  H: THandle;
begin
  H := CreateFile(PChar(Filename), 0, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, 0, 0);
  if H <> INVALID_HANDLE_VALUE then begin
    Result := GetFileTime(H, nil, nil, @LastWriteTime);
    CloseHandle(H);
  end
  else
    Result := False;
end;

procedure AddFileToRecentDocs(const Filename: String);
{ Notifies the shell that a document has been opened. On Windows 7, this will
  add the file to the Recent section of the app's Jump List.
  It is only necessary to call this function when the shell is unaware that
  a file is being opened. Files opened through Explorer or common dialogs get
  added to the Jump List automatically. }
begin
  SHAddToRecentDocs(
    {$IFDEF UNICODE} SHARD_PATHW {$ELSE} SHARD_PATHA {$ENDIF},
    PChar(Filename));
end;

function GenerateGuid: String;
var
  Guid: TGUID;
  P: PWideChar;
begin
  if CoCreateGuid(Guid) <> S_OK then
    raise Exception.Create('CoCreateGuid failed');
  if StringFromCLSID(Guid, P) <> S_OK then
    raise Exception.Create('StringFromCLSID failed');
  try
    Result := P;
  finally
    CoTaskMemFree(P);
  end;
end;

{ TConfigIniFile }

type
  TConfigIniFile = class(TRegIniFile)
  private
    FMutex: THandle;
    FAcquiredMutex: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TConfigIniFile.Create;
begin
  inherited Create('Software\Jordan Russell\Inno Setup');
  { Paranoia: Use a mutex to prevent multiple instances from reading/writing
    to the registry simultaneously }
  FMutex := CreateMutex(nil, False, 'Inno-Setup-IDE-Config-Mutex');
  if FMutex <> 0 then
    if WaitForSingleObject(FMutex, INFINITE) <> WAIT_FAILED then
      FAcquiredMutex := True;
end;

destructor TConfigIniFile.Destroy;
begin
  if FMutex <> 0 then begin
    if FAcquiredMutex then
      ReleaseMutex(FMutex);
    CloseHandle(FMutex);
  end;
  inherited;
end;

{ TISScintEdit }

procedure TISScintEdit.CreateWnd;
const
  PixmapHasEntry: array[0..8] of PAnsiChar = (
    '5 5 2 1',
    'o c #808080',
    '. c #c0c0c0',
    'ooooo',
    'o...o',
    'o...o',
    'o...o',
    'ooooo',
    nil);
  PixmapEntryProcessed: array[0..8] of PAnsiChar = (
    '5 5 2 1',
    'o c #008000',
    '. c #00ff00',
    'ooooo',
    'o...o',
    'o...o',
    'o...o',
    'ooooo',
    nil);
  PixmapBreakpoint: array[0..14] of PAnsiChar = (
    '9 10 3 1',
    '= c none',
    'o c #000000',
    '. c #ff0000',
    '=========',
    '==ooooo==',
    '=o.....o=',
    'o.......o',
    'o.......o',
    'o.......o',
    'o.......o',
    'o.......o',
    '=o.....o=',
    '==ooooo==',
    nil);
  PixmapBreakpointGood: array[0..15] of PAnsiChar = (
    '9 10 4 1',
    '= c none',
    'o c #000000',
    '. c #ff0000',
    '* c #00ff00',
    '======oo=',
    '==oooo**o',
    '=o....*o=',
    'o....**.o',
    'o....*..o',
    'o...**..o',
    'o**.*...o',
    'o.***...o',
    '=o.*...o=',
    '==ooooo==',
    nil);
  PixmapBreakpointBad: array[0..15] of PAnsiChar = (
    '9 10 4 1',
    '= c none',
    'o c #000000',
    '. c #ff0000',
    '* c #ffff00',
    '=========',
    '==ooooo==',
    '=o.....o=',
    'o.*...*.o',
    'o.**.**.o',
    'o..***..o',
    'o.**.**.o',
    'o.*...*.o',
    '=o.....o=',
    '==ooooo==',
    nil);
const
  SC_MARK_BACKFORE = 3030;  { new marker type added in my Scintilla build }
begin
  inherited;

  Call(SCI_SETCARETWIDTH, 2, 0);
  Call(SCI_AUTOCSETAUTOHIDE, 0, 0);
  Call(SCI_AUTOCSETCANCELATSTART, 0, 0);
  Call(SCI_AUTOCSETDROPRESTOFWORD, 1, 0);
  Call(SCI_AUTOCSETIGNORECASE, 1, 0);
  Call(SCI_AUTOCSETMAXHEIGHT, 7, 0);

  Call(SCI_ASSIGNCMDKEY, Ord('Z') or ((SCMOD_SHIFT or SCMOD_CTRL) shl 16), SCI_REDO);

  Call(SCI_SETSCROLLWIDTH, 1024 * CallStr(SCI_TEXTWIDTH, 0, 'X'), 0);

  Call(SCI_INDICSETSTYLE, inSquiggly, INDIC_SQUIGGLE);
  Call(SCI_INDICSETFORE, inSquiggly, clRed);
  Call(SCI_INDICSETSTYLE, inPendingSquiggly, INDIC_HIDDEN);

  Call(SCI_SETMARGINTYPEN, 1, SC_MARGIN_SYMBOL);
  Call(SCI_SETMARGINWIDTHN, 1, 21);
  Call(SCI_SETMARGINSENSITIVEN, 1, 1);
  Call(SCI_SETMARGINCURSORN, 1, SC_CURSORARROW);
  Call(SCI_SETMARGINTYPEN, 2, SC_MARGIN_BACK);
  Call(SCI_SETMARGINMASKN, 2, 0);
  Call(SCI_SETMARGINWIDTHN, 2, 1);
  Call(SCI_SETMARGINTYPEN, 3, SC_MARGIN_SYMBOL);
  Call(SCI_SETMARGINMASKN, 3, 0);
  Call(SCI_SETMARGINWIDTHN, 3, 1);
  Call(SCI_SETMARGINLEFT, 0, 2);

  Call(SCI_MARKERDEFINEPIXMAP, mmIconHasEntry, LPARAM(@PixmapHasEntry));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconEntryProcessed, LPARAM(@PixmapEntryProcessed));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconBreakpoint, LPARAM(@PixmapBreakpoint));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconBreakpointGood, LPARAM(@PixmapBreakpointGood));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconBreakpointBad, LPARAM(@PixmapBreakpointBad));
  Call(SCI_MARKERDEFINE, mmLineError, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineError, clWhite);
  Call(SCI_MARKERSETBACK, mmLineError, clMaroon);
  Call(SCI_MARKERDEFINE, mmLineBreakpoint, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineBreakpoint, clWhite);
  Call(SCI_MARKERSETBACK, mmLineBreakpoint, clRed);
  Call(SCI_MARKERDEFINE, mmLineBreakpointBad, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineBreakpointBad, clLime);
  Call(SCI_MARKERSETBACK, mmLineBreakpointBad, clOlive);
  Call(SCI_MARKERDEFINE, mmLineStep, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineStep, clWhite);
  Call(SCI_MARKERSETBACK, mmLineStep, clBlue);
end;

{ TCompileFormMemoPopupMenu }

type
  TCompileFormMemoPopupMenu = class(TPopupMenu)
  public
    procedure Popup(X, Y: Integer); override;
  end;

procedure TCompileFormMemoPopupMenu.Popup(X, Y: Integer);
var
  Form: TCompileForm;
begin
  { Show the existing Edit menu }
  Form := Owner as TCompileForm;
  TrackPopupMenu(Form.EMenu.Handle, TPM_RIGHTBUTTON, X, Y, 0, Form.Handle, nil);
end;

{ TCompileForm }

constructor TCompileForm.Create(AOwner: TComponent);

  procedure ReadConfig;
{$IFNDEF UNICODE}
  const
    { "MS Gothic" in Japanese (CP 932) }
    SMSGothicLocalized = #$82'l'#$82'r '#$83'S'#$83'V'#$83'b'#$83'N';
{$ENDIF}
  var
    Ini: TConfigIniFile;
    WindowPlacement: TWindowPlacement;
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
      FOptions.UndoAfterSave := Ini.ReadBool('Options', 'UndoAfterSave', False);
      FOptions.PauseOnDebuggerExceptions := Ini.ReadBool('Options', 'PauseOnDebuggerExceptions', True);
      FOptions.RunAsDifferentUser := Ini.ReadBool('Options', 'RunAsDifferentUser', False);
      FOptions.AutoComplete := Ini.ReadBool('Options', 'AutoComplete', True);
      FOptions.UseSyntaxHighlighting := Ini.ReadBool('Options', 'UseSynHigh', True);
      FOptions.UnderlineErrors := Ini.ReadBool('Options', 'UnderlineErrors', True);
      FOptions.CursorPastEOL := Ini.ReadBool('Options', 'EditorCursorPastEOL', True);
      FOptions.TabWidth := Ini.ReadInteger('Options', 'TabWidth', 2);
      FOptions.UseTabCharacter := Ini.ReadBool('Options', 'UseTabCharacter', False);
      FOptions.WordWrap := Ini.ReadBool('Options', 'WordWrap', False);
      FOptions.AutoIndent := Ini.ReadBool('Options', 'AutoIndent', True);
      FOptions.IndentationGuides := Ini.ReadBool('Options', 'IndentationGuides', False);
      if GetACP = 932 then begin
        { Default to MS Gothic font on CP 932 (Japanese), as Courier New is
          only capable of displaying Japanese characters on XP and later. }
{$IFNDEF UNICODE}
        { Use the English name if it's supported on this version of Windows
          (I believe it was first added in Windows 2000), because the CP 932
          localized Japanese name will no longer be valid if the user later
          switches out of CP 932. }
        if FontExists('MS Gothic') then
          Memo.Font.Name := 'MS Gothic'
        else
          Memo.Font.Name := SMSGothicLocalized;
{$ELSE}
        { UNICODE requires 2000+, so we can just use the English name }
        Memo.Font.Name := 'MS Gothic';
{$ENDIF}
        Memo.Font.Size := 9;
        Memo.Font.Charset := SHIFTJIS_CHARSET;
      end;
      Memo.Font.Name := Ini.ReadString('Options', 'EditorFontName', Memo.Font.Name);
      Memo.Font.Size := Ini.ReadInteger('Options', 'EditorFontSize', Memo.Font.Size);
      Memo.Font.Charset := Ini.ReadInteger('Options', 'EditorFontCharset', Memo.Font.Charset);
      Memo.Zoom := Ini.ReadInteger('Options', 'Zoom', 0);
      SyncEditorOptions;
      UpdateNewButtons;

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
      StatusPanel.Height := Ini.ReadInteger('State', 'StatusPanelHeight',
        (10 * DebugOutputList.ItemHeight + 4) + SpacerPaintBox.Height + TabSet.Height);
    finally
      Ini.Free;
    end;
    FOptionsLoaded := True;
  end;

  procedure ReadSignTools;
  var
    Ini: TConfigIniFile;
    I: Integer;
    S: String;
  begin
    Ini := TConfigIniFile.Create;
    try
      { Sign tools }
      FSignTools.Clear();
      I := 0;
      repeat
        S := Ini.ReadString('SignTools', 'SignTool' + IntToStr(I), '');
        if S <> '' then
          FSignTools.Add(S);
        Inc(I);
      until S = '';
    finally
      Ini.Free;
    end;
  end;

  procedure SetFakeShortCutText(const MenuItem: TMenuItem; const S: String);
  begin
    MenuItem.Caption := MenuItem.Caption + #9 + S;
  end;

  procedure SetFakeShortCut(const MenuItem: TMenuItem; const Key: Word;
    const Shift: TShiftState);
  begin
    SetFakeShortCutText(MenuItem, ShortCutToText(ShortCut(Key, Shift)));
  end;

var
  I: Integer;
  NewItem: TMenuItem;
begin
  inherited;

  {$IFNDEF STATICCOMPILER}
  FCompilerVersion := ISDllGetVersion;
  {$ELSE}
  FCompilerVersion := ISGetVersion;
  {$ENDIF}

  FErrorLine := -1;
  FStepLine := -1;
  FModifiedSinceLastCompile := True;

  InitFormFont(Self);

  FBuildImageList := ImageList_LoadBitmap(HInstance, 'BUILDIMAGES', 17, 0, clSilver);

  { For some reason, if AutoScroll=False is set on the form Delphi ignores the
    'poDefault' Position setting }
  AutoScroll := False;

{$IFNDEF UNICODE}
  FSaveEncoding.Visible := False;
{$ENDIF}

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
  SetFakeShortCutText(VZoomIn, SmkcCtrl + 'Num +');
  SetFakeShortCutText(VZoomOut, SmkcCtrl + 'Num -');
  SetFakeShortCutText(VZoomReset, SmkcCtrl + 'Num /');
  { Use fake Esc shortcut for Stop Compile so it doesn't conflict with the
    editor's autocompletion list } 
  SetFakeShortCut(BStopCompile, VK_ESCAPE, []);

  MemoStyler := TInnoSetupStyler.Create(Self);

  Memo := TISScintEdit.Create(Self);
  Memo.AcceptDroppedFiles := True;
  Memo.Align := alClient;
  Memo.AutoCompleteFontName := Font.Name;
  Memo.AutoCompleteFontSize := Font.Size;
{$IFDEF UNICODE}
  Memo.CodePage := CP_UTF8;
{$ENDIF}
  Memo.Font.Name := 'Courier New';
  Memo.Font.Size := 10;
  Memo.ShowHint := True;
  Memo.Styler := MemoStyler;
  Memo.PopupMenu := TCompileFormMemoPopupMenu.Create(Self);
  Memo.OnChange := MemoChange;
  Memo.OnCharAdded := MemoCharAdded;
  Memo.OnDropFiles := MemoDropFiles;
  Memo.OnHintShow := MemoHintShow;
  Memo.OnKeyDown := MemoKeyDown;
  Memo.OnKeyPress := MemoKeyPress;
  Memo.OnMarginClick := MemoMarginClick;
  Memo.OnModifiedChange := MemoModifiedChange;
  Memo.OnUpdateUI := MemoUpdateUI;
  Memo.Parent := BodyPanel;

  FBreakPoints := TList.Create;

  DebugOutputList.Canvas.Font.Assign(DebugOutputList.Font);
  FDebugLogListTimeWidth := DebugOutputList.Canvas.TextWidth(Format(
    '[00%s00%s00%s000]   ', [TimeSeparator, TimeSeparator, DecimalSeparator]));
  DebugOutputList.ItemHeight := DebugOutputList.Canvas.TextHeight('0');

  Application.HintShortPause := 0;
  Application.OnException := AppOnException;
  Application.OnActivate := AppOnActivate;
  Application.OnIdle := AppOnIdle;

  FMRUList := TStringList.Create;
  for I := 0 to High(FMRUMenuItems) do begin
    NewItem := TMenuItem.Create(Self);
    NewItem.OnClick := FMRUClick;
    FMenu.Insert(FMenu.IndexOf(FMRUSep), NewItem);
    FMRUMenuItems[I] := NewItem;
  end;

  FSignTools := TStringList.Create;

  FDebugTarget := dtSetup;
  UpdateTargetMenu;

  UpdateCaption;

  UpdateThemeData(False, True);

  if CommandLineCompile then begin
    ReadSignTools;
    PostMessage(Handle, WM_StartCommandLineCompile, 0, 0)
  end else if CommandLineWizard then begin
    { Stop Delphi from showing the compiler form }
    Application.ShowMainForm := False;
    { Show wizard form later }
    PostMessage(Handle, WM_StartCommandLineWizard, 0, 0);
  end else begin
    ReadConfig;
    ReadSignTools;
    PostMessage(Handle, WM_StartNormally, 0, 0);
  end;
end;

destructor TCompileForm.Destroy;

  procedure SaveConfig;
  var
    Ini: TConfigIniFile;
    WindowPlacement: TWindowPlacement;
  begin
    Ini := TConfigIniFile.Create;
    try
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
      Ini.WriteBool('State', 'WindowMaximized', WindowState = wsMaximized);
      Ini.WriteInteger('State', 'StatusPanelHeight', StatusPanel.Height);
      
      { Zoom state }
      Ini.WriteInteger('Options', 'Zoom', Memo.Zoom);
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

  FBreakPoints.Free;
  DestroyDebugInfo;
  FSignTools.Free;
  FMRUList.Free;
  if FBuildImageList <> 0 then begin
    ImageList_Destroy(FBuildImageList);
    FBuildImageList := 0;
  end;

  inherited;
end;

class procedure TCompileForm.AppOnException(Sender: TObject; E: Exception);
begin
  AppMessageBox(PChar(AddPeriod(E.Message)), SCompilerFormCaption,
    MB_OK or MB_ICONSTOP);
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

procedure TCompileForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ShortCut(Key, Shift) = VK_ESCAPE then begin
    if BStopCompile.Enabled then
      BStopCompileClick(Self);
  end
  else if (Key = VK_F6) and not(ssAlt in Shift) then begin
    { Toggle focus between panes }
    Key := 0;
    if ActiveControl <> Memo then
      ActiveControl := Memo
    else if StatusPanel.Visible then begin
      case TabSet.TabIndex of
        tiCompilerOutput: ActiveControl := CompilerOutputList;
        tiDebugOutput: ActiveControl := DebugOutputList;
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
  if FFilename = '' then
    NewCaption := 'Untitled'
  else begin
    if FOptions.FullPathInTitleBar then
      NewCaption := FFilename
    else
      NewCaption := GetDisplayFilename(FFilename);
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

procedure TCompileForm.UpdateNewButtons;
begin
  if FOptions.UseWizard then begin
    FNew.OnClick := FNewWizardClick;
    NewButton.OnClick := FNewWizardClick;
  end else begin
    FNew.OnClick := FNewClick;
    NewButton.OnClick := FNewClick;
  end;
end;

procedure TCompileForm.NewFile;
begin
  HideError;
  FUninstExe := '';
  if FDebugTarget <> dtSetup then begin
    FDebugTarget := dtSetup;
    UpdateTargetMenu;
  end;
  FBreakPoints.Clear;
  DestroyDebugInfo;

  FFilename := '';
  UpdateCaption;
  FSaveInUTF8Encoding := False;
  Memo.Lines.Clear;
  FModifiedSinceLastCompile := True;
  Memo.ClearUndo;
end;

procedure TCompileForm.NewWizardFile;
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
      NewFile;
      Memo.Lines.Text := WizardForm.ResultScript;
      Memo.ClearUndo;
      if WizardForm.Result = wrComplete then begin
        Memo.ForceModifiedState;
        if MsgBox('Would you like to compile the new script now?', SCompilerFormCaption, mbConfirmation, MB_YESNO) = IDYES then
          BCompileClick(Self);
      end;
    end;
  finally
    WizardForm.Free;
  end;
end;

procedure TCompileForm.OpenFile(AFilename: String;
  const AddToRecentDocs: Boolean);

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
    NewFile;
    GetFileTime(Stream.Handle, nil, nil, @FFileLastWriteTime);
    FSaveInUTF8Encoding := IsStreamUTF8Encoded(Stream);
    Stream.Seek(0, soFromBeginning);
    Memo.Lines.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
  Memo.ClearUndo;
  FFilename := AFilename;
  UpdateCaption;
  ModifyMRUList(AFilename, True);
  if AddToRecentDocs then
    AddFileToRecentDocs(AFilename);
end;

procedure TCompileForm.OpenMRUFile(const AFilename: String);
{ Same as OpenFile, but offers to remove the file from the MRU list if it
  cannot be opened }
begin
  try
    OpenFile(AFilename, True);
  except
    Application.HandleException(Self);
    if MsgBoxFmt('There was an error opening the file. Remove it from the list?',
       [AFilename], SCompilerFormCaption, mbError, MB_YESNO) = IDYES then
      ModifyMRUList(AFilename, False);
  end;
end;

class procedure TCompileForm.SaveTextToFile(const Filename: String;
  const S: String; const ForceUTF8Encoding: Boolean);
var
{$IFDEF UNICODE}
  AnsiMode: Boolean;
  AnsiStr: AnsiString;
{$ENDIF}
  F: TTextFileWriter;
begin
{$IFDEF UNICODE}
  AnsiMode := False;
  if not ForceUTF8Encoding then begin
    AnsiStr := AnsiString(S);
    if S = String(AnsiStr) then
      AnsiMode := True;
  end;
{$ENDIF}

  F := TTextFileWriter.Create(Filename, fdCreateAlways, faWrite, fsNone);
  try
{$IFDEF UNICODE}
    if AnsiMode then
      F.WriteAnsi(AnsiStr)
    else
{$ENDIF}
      F.Write(S);
  finally
    F.Free;
  end;
end;

function TCompileForm.SaveFile(const SaveAs: Boolean): Boolean;

  procedure SaveTo(const FN: String);
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
      SaveTextToFile(TempFN, Memo.Lines.Text, FSaveInUTF8Encoding);

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
    GetLastWriteTimeOfFile(FN, FFileLastWriteTime);
  end;

var
  FN: String;
begin
  Result := False;
  if SaveAs or (FFilename = '') then begin
    FN := FFilename;
    if not NewGetSaveFileName('', FN, '', SCompilerOpenFilter, 'iss', Handle) then Exit;
    FN := PathExpand(FN);
    SaveTo(FN);
    FFilename := FN;
    UpdateCaption;
  end
  else
    SaveTo(FFilename);
  Memo.SetSavePoint;
  if not FOptions.UndoAfterSave then
    Memo.ClearUndo;
  Result := True;
  ModifyMRUList(FFilename, True);
end;

function TCompileForm.ConfirmCloseFile(const PromptToSave: Boolean): Boolean;
var
  FileTitle: String;
begin
  Result := True;
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
  if PromptToSave and Memo.Modified then begin
    FileTitle := FFilename;
    if FileTitle = '' then FileTitle := 'Untitled';
    case MsgBox('The text in the ' + FileTitle + ' file has changed.'#13#10#13#10 +
       'Do you want to save the changes?', SCompilerFormCaption, mbError,
       MB_YESNOCANCEL) of
      IDYES: Result := SaveFile(False);
      IDNO: ;
    else
      Result := False;
    end;
  end;
end;

procedure TCompileForm.ReadMRUList;
{ Loads the list of MRU items from the registry }
var
  Ini: TConfigIniFile;
  I: Integer;
  S: String;
begin
  try
    Ini := TConfigIniFile.Create;
    try
      FMRUList.Clear;
      for I := 0 to High(FMRUMenuItems) do begin
        S := Ini.ReadString('ScriptFileHistoryNew', 'History' + IntToStr(I), '');
        if S <> '' then FMRUList.Add(S);
      end;
    finally
      Ini.Free;
    end;
  except
    { Ignore any exceptions; don't want to hold up the display of the
      File menu. }
  end;
end;

procedure TCompileForm.ModifyMRUList(const AFilename: String;
  const AddNewItem: Boolean);
var
  I: Integer;
  Ini: TConfigIniFile;
  S: String;
begin
  try
    { Load most recent items first, just in case they've changed }
    ReadMRUList;

    I := 0;
    while I < FMRUList.Count do begin
      if PathCompare(FMRUList[I], AFilename) = 0 then
        FMRUList.Delete(I)
      else
        Inc(I);
    end;
    if AddNewItem then
      FMRUList.Insert(0, AFilename);
    while FMRUList.Count > High(FMRUMenuItems)+1 do
      FMRUList.Delete(FMRUList.Count-1);

    { Save new MRU items }
    Ini := TConfigIniFile.Create;
    try
      { MRU list }
      for I := 0 to High(FMRUMenuItems) do begin
        if I < FMRUList.Count then
          S := FMRUList[I]
        else
          S := '';
        Ini.WriteString('ScriptFileHistoryNew', 'History' + IntToStr(I), S);
      end;
    finally
      Ini.Free;
    end;
  except
    { Handle exceptions locally; failure to save the MRU list should not be
      a fatal error. }
    Application.HandleException(Self);
  end;
end;

procedure TCompileForm.StatusMessage(const S: String);
var
  DC: HDC;
  Size: TSize;
begin
  with CompilerOutputList do begin
    try
      TopIndex := Items.Add(S);
    except
      on EOutOfResources do begin
        Clear;
        SendMessage(Handle, LB_SETHORIZONTALEXTENT, 0, 0);
        Items.Add(SCompilerStatusReset);
        TopIndex := Items.Add(S);
      end;
    end;
    DC := GetDC(0);
    try
      SelectObject(DC, Font.Handle);
      GetTextExtentPoint(DC, PChar(S), Length(S), Size);
    finally
      ReleaseDC(0, DC);
    end;
    Inc(Size.cx, 5);
    if Size.cx > SendMessage(Handle, LB_GETHORIZONTALEXTENT, 0, 0) then
      SendMessage(Handle, LB_SETHORIZONTALEXTENT, Size.cx, 0);
    Update;
  end;
end;

procedure TCompileForm.DebugLogMessage(const S: String);
var
  ST: TSystemTime;
  FirstLine: Boolean;

  procedure AddLine(S: String);
  var
    StartsWithTab: Boolean;
    DC: HDC;
    Size: TSize;
  begin
    if FirstLine then begin
      FirstLine := False;
      Insert(Format('[%.2u%s%.2u%s%.2u%s%.3u]   ', [ST.wHour, TimeSeparator,
        ST.wMinute, TimeSeparator, ST.wSecond, DecimalSeparator,
        ST.wMilliseconds]), S, 1);
      StartsWithTab := False;
    end
    else begin
      Insert(#9, S, 1);
      StartsWithTab := True;
    end;
    try
      DebugOutputList.TopIndex := DebugOutputList.Items.Add(S);
    except
      on EOutOfResources do begin
        DebugOutputList.Clear;
        SendMessage(DebugOutputList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
        DebugOutputList.Items.Add(SCompilerStatusReset);
        DebugOutputList.TopIndex := DebugOutputList.Items.Add(S);
      end;
    end;
    DC := GetDC(0);
    try
      SelectObject(DC, DebugOutputList.Font.Handle);
      if StartsWithTab then
        GetTextExtentPoint(DC, PChar(S)+1, Length(S)-1, Size)
      else
        GetTextExtentPoint(DC, PChar(S), Length(S), Size);
    finally
      ReleaseDC(0, DC);
    end;
    Inc(Size.cx, 5);
    if StartsWithTab then
      Inc(Size.cx, FDebugLogListTimeWidth);
    if Size.cx > SendMessage(DebugOutputList.Handle, LB_GETHORIZONTALEXTENT, 0, 0) then
      SendMessage(DebugOutputList.Handle, LB_SETHORIZONTALEXTENT, Size.cx, 0);
  end;

var
  LineStart, I: Integer;
  LastWasCR: Boolean;
begin
  GetLocalTime(ST);
  FirstLine := True;
  LineStart := 1;
  LastWasCR := False;
  { Call AddLine for each line. CR, LF, and CRLF line breaks are supported. }
  for I := 1 to Length(S) do begin
    if S[I] = #13 then begin
      AddLine(Copy(S, LineStart, I - LineStart));
      LineStart := I + 1;
      LastWasCR := True;
    end
    else begin
      if S[I] = #10 then begin
        if not LastWasCR then
          AddLine(Copy(S, LineStart, I - LineStart));
        LineStart := I + 1;
      end;
      LastWasCR := False;
    end;
  end;
  AddLine(Copy(S, LineStart, Maxint));
  DebugOutputList.Update;
end;

type
  PAppData = ^TAppData;
  TAppData = record
    Form: TCompileForm;
    Lines: TStringList;
    CurLineNumber: Integer;
    CurLine: String;
    OutputExe: String;
    ErrorMsg: String;
    ErrorFilename: String;
    ErrorLine: Integer;
    Aborted: Boolean;
  end;

function CompilerCallbackProc(Code: Integer; var Data: TCompilerCallbackData;
  AppData: Longint): Integer; stdcall;
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
        Form.StatusMessage(Data.StatusMsg);
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

procedure TCompileForm.CompileFile(AFilename: String;
  const ReadFromFile: Boolean);

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
      ALines.Capacity := Memo.Lines.Count;
      ALines.Assign(Memo.Lines);
    end;

    { Check for null characters }
    for I := 0 to ALines.Count-1 do begin
      if ContainsNullChar(ALines[I]) then begin
        if not ReadFromFile then begin
          MoveCaret(I, False);
          SetErrorLine(I);
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
begin
  if FCompiling then begin
    { Shouldn't get here, but just in case... }
    MsgBox('A compile is already in progress.', SCompilerFormCaption, mbError, MB_OK);
    Abort;
  end;

  if not ReadFromFile then begin
    if FOptions.Autosave and Memo.Modified then begin
      if not SaveFile(False) then Abort;
    end else if FFilename = '' then begin
      case MsgBox('Would you like to save the script before compiling?' +
         SNewLine2 + 'If you answer No, the compiled installation will be ' +
         'placed under your My Documents folder by default.',
         SCompilerFormCaption, mbConfirmation, MB_YESNOCANCEL) of
        IDYES: if not SaveFile(False) then Abort;
        IDNO: ;
      else
        Abort;
      end;
    end;
    AFilename := FFilename;
  end;

  DestroyDebugInfo;
  AppData.Lines := TStringList.Create;
  try
    FBuildAnimationFrame := 0;
    FProgress := 0;
    FProgressMax := 0;

    Memo.CancelAutoComplete;
    Memo.Cursor := crAppStart;
    Memo.SetCursorID(999);  { hack to keep it from overriding Cursor }
    CompilerOutputList.Cursor := crAppStart;
    Memo.ReadOnly := True;
    UpdateEditModePanel;
    HideError;
    CompilerOutputList.Clear;
    SendMessage(CompilerOutputList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
    DebugOutputList.Clear;
    SendMessage(DebugOutputList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
    TabSet.TabIndex := tiCompilerOutput;
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
      Options := Options + 'SignTool-' + FSignTools[I] + #0;
    Params.Options := PChar(Options);

    AppData.Form := Self;
    AppData.CurLineNumber := 0;
    AppData.Aborted := False;
    ReadScriptLines(AppData.Lines);

    StartTime := GetTickCount;
    StatusMessage(Format(SCompilerStatusStarting, [TimeToStr(Time)]));
    StatusMessage('');
    FCompiling := True;
    FCompileWantAbort := False;
    UpdateRunMenu;
    UpdateCaption;
    SetLowPriority(FOptions.LowPriorityDuringCompile);
    {$IFNDEF STATICCOMPILER}
    if ISDllCompileScript(Params) <> isceNoError then begin
    {$ELSE}
    if ISCompileScript(Params, False) <> isceNoError then begin
    {$ENDIF}
      StatusMessage(SCompilerStatusErrorAborted);
      if not ReadFromFile and (AppData.ErrorLine > 0) and
         (AppData.ErrorFilename = '') then begin
        { Move the caret to the line number the error occured on }
        MoveCaret(AppData.ErrorLine - 1, False);
        SetErrorLine(AppData.ErrorLine - 1);
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
    StatusMessage(Format(SCompilerStatusFinished, [TimeToStr(Time),
      Format('%.2u%s%.2u%s%.3u', [ElapsedSeconds div 60, TimeSeparator,
        ElapsedSeconds mod 60, DecimalSeparator, ElapsedTime mod 1000])]));
  finally
    AppData.Lines.Free;
    FCompiling := False;
    SetLowPriority(False);
    Memo.Cursor := crDefault;
    Memo.SetCursorID(SC_CURSORNORMAL);
    CompilerOutputList.Cursor := crDefault;
    Memo.ReadOnly := False;
    UpdateEditModePanel;
    UpdateRunMenu;
    UpdateCaption;
    InvalidateStatusPanel(spCompileIcon);
    InvalidateStatusPanel(spCompileProgress);
    SetAppTaskbarProgressState(tpsNoProgress);
    StatusBar.Panels[spExtraStatus].Text := '';
  end;
  FCompiledExe := AppData.OutputExe;
  FModifiedSinceLastCompile := False;
  FModifiedSinceLastCompileAndGo := False;
end;

procedure TCompileForm.SetLowPriority(ALowPriority: Boolean);
begin
  if ALowPriority then begin
    { Save current priority and change to 'low' }
    if FSavePriorityClass = 0 then
      FSavePriorityClass := GetPriorityClass(GetCurrentProcess);
    SetPriorityClass(GetCurrentProcess, IDLE_PRIORITY_CLASS);
  end
  else begin
    { Restore original priority }
    if FSavePriorityClass <> 0 then begin
      SetPriorityClass(GetCurrentProcess, FSavePriorityClass);
      FSavePriorityClass := 0;
    end;
  end;
end;

function TranslateCharsetInfo(lpSrc: PDWORD; var lpCs: TCharsetInfo;
  dwFlags: DWORD): BOOL; stdcall; external gdi32;

procedure TCompileForm.SyncEditorOptions;
const
  SquigglyStyles: array[Boolean] of Integer = (INDIC_HIDDEN, INDIC_SQUIGGLE);
{$IFNDEF UNICODE}
var
  CharsetInfo: TCharsetInfo;
{$ENDIF}
begin
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

{$IFNDEF UNICODE}
  { Try to set the editor's code page to match the font's character set }
  if (Memo.Font.Charset <> DEFAULT_CHARSET) and
     TranslateCharsetInfo(PDWORD(Memo.Font.Charset), CharsetInfo, TCI_SRCCHARSET) then
    Memo.CodePage := CharsetInfo.ciACP
  else
    Memo.CodePage := 0;
{$ENDIF}
end;

procedure TCompileForm.FMenuClick(Sender: TObject);

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
  FSaveEncodingAuto.Checked := not FSaveInUTF8Encoding;
  FSaveEncodingUTF8.Checked := FSaveInUTF8Encoding;
  ReadMRUList;
  FMRUSep.Visible := FMRUList.Count <> 0;
  for I := 0 to High(FMRUMenuItems) do
    with FMRUMenuItems[I] do begin
      if I < FMRUList.Count then begin
        Visible := True;
        Caption := '&' + IntToStr((I+1) mod 10) + ' ' + DoubleAmp(FMRUList[I]);
      end
      else
        Visible := False;
    end;
end;

procedure TCompileForm.FNewClick(Sender: TObject);
begin
  if ConfirmCloseFile(True) then
    NewFile;
end;

procedure TCompileForm.FNewWizardClick(Sender: TObject);
begin
  if ConfirmCloseFile(True) then
    NewWizardFile;
end;

procedure TCompileForm.ShowOpenDialog(const Examples: Boolean);
var
  InitialDir, FileName: String;
begin
  if Examples then begin
    InitialDir := PathExtractPath(NewParamStr(0)) + 'Examples';
    Filename := PathExtractPath(NewParamStr(0)) + 'Examples\Example1.iss';
  end
  else begin
    InitialDir := PathExtractDir(FFilename);
    Filename := '';
  end;
  if ConfirmCloseFile(True) then
    if NewGetOpenFileName('', FileName, InitialDir, SCompilerOpenFilter, 'iss', Handle) then
      OpenFile(Filename, False);
end;

procedure TCompileForm.FOpenClick(Sender: TObject);
begin
  ShowOpenDialog(False);
end;

procedure TCompileForm.FSaveClick(Sender: TObject);
begin
  SaveFile(False);
end;

procedure TCompileForm.FSaveAsClick(Sender: TObject);
begin
  SaveFile(True);
end;

procedure TCompileForm.FSaveEncodingItemClick(Sender: TObject);
begin
  FSaveInUTF8Encoding := (Sender = FSaveEncodingUTF8);
end;

procedure TCompileForm.FMRUClick(Sender: TObject);
var
  I: Integer;
begin
  if ConfirmCloseFile(True) then
    for I := 0 to High(FMRUMenuItems) do
      if FMRUMenuItems[I] = Sender then begin
        OpenMRUFile(FMRUList[I]);
        Break;
      end;
end;

procedure TCompileForm.FExitClick(Sender: TObject);
begin
  Close;
end;

procedure TCompileForm.EMenuClick(Sender: TObject);
var
  MemoHasFocus: Boolean;
begin
  MemoHasFocus := Memo.Focused;
  EUndo.Enabled := MemoHasFocus and Memo.CanUndo;
  ERedo.Enabled := MemoHasFocus and Memo.CanRedo;
  ECut.Enabled := MemoHasFocus and Memo.SelAvail;
  ECopy.Enabled := MemoHasFocus and Memo.SelAvail;
  EPaste.Enabled := MemoHasFocus and Clipboard.HasFormat(CF_TEXT);
  EDelete.Enabled := MemoHasFocus and Memo.SelAvail;
  ESelectAll.Enabled := MemoHasFocus;
  EFind.Enabled := MemoHasFocus;
  EFindNext.Enabled := MemoHasFocus;
  EReplace.Enabled := MemoHasFocus;
  EGoto.Enabled := MemoHasFocus;
  ECompleteWord.Enabled := MemoHasFocus;
end;

procedure TCompileForm.EUndoClick(Sender: TObject);
begin
  Memo.Undo;
end;

procedure TCompileForm.ERedoClick(Sender: TObject);
begin
  Memo.Redo;
end;

procedure TCompileForm.ECutClick(Sender: TObject);
begin
  Memo.CutToClipboard;
end;

procedure TCompileForm.ECopyClick(Sender: TObject);
begin
  Memo.CopyToClipboard;
end;

procedure TCompileForm.EPasteClick(Sender: TObject);
begin
  Memo.PasteFromClipboard;
end;

procedure TCompileForm.EDeleteClick(Sender: TObject);
begin
  Memo.ClearSelection;
end;

procedure TCompileForm.ESelectAllClick(Sender: TObject);
begin
  Memo.SelectAll;
end;

procedure TCompileForm.ECompleteWordClick(Sender: TObject);
begin
  InitiateAutoComplete(#0);
end;

procedure TCompileForm.VMenuClick(Sender: TObject);
begin
  VZoomIn.Enabled := (Memo.Zoom < 20);
  VZoomOut.Enabled := (Memo.Zoom > -10);
  VZoomReset.Enabled := (Memo.Zoom <> 0);
  VToolbar.Checked := ToolbarPanel.Visible;
  VStatusBar.Checked := StatusBar.Visible;
  VHide.Checked := not StatusPanel.Visible;
  VCompilerOutput.Checked := StatusPanel.Visible and (TabSet.TabIndex = tiCompilerOutput);
  VDebugOutput.Checked := StatusPanel.Visible and (TabSet.TabIndex = tiDebugOutput);
end;

procedure TCompileForm.VZoomInClick(Sender: TObject);
begin
  Memo.ZoomIn;
end;

procedure TCompileForm.VZoomOutClick(Sender: TObject);
begin
  Memo.ZoomOut;
end;

procedure TCompileForm.VZoomResetClick(Sender: TObject);
begin
  Memo.Zoom := 0;
end;

procedure TCompileForm.VToolbarClick(Sender: TObject);
begin
  ToolbarPanel.Visible := not ToolbarPanel.Visible;
end;

procedure TCompileForm.VStatusBarClick(Sender: TObject);
begin
  StatusBar.Visible := not StatusBar.Visible;
end;

procedure TCompileForm.SetStatusPanelVisible(const AVisible: Boolean);
var
  CaretWasInView: Boolean;
begin
  if StatusPanel.Visible <> AVisible then begin
    CaretWasInView := Memo.IsPositionInViewVertically(Memo.CaretPosition);
    if AVisible then begin
      { Ensure the status panel height isn't out of range before showing }
      UpdateStatusPanelHeight(StatusPanel.Height);
      SplitPanel.Top := ClientHeight;
      StatusPanel.Top := ClientHeight;
    end
    else begin
      if StatusPanel.ContainsControl(ActiveControl) then
        ActiveControl := Memo;
    end;
    SplitPanel.Visible := AVisible;
    StatusPanel.Visible := AVisible;
    if AVisible and CaretWasInView then begin
      { If the caret was in view, make sure it still is }
      Memo.ScrollCaretIntoView;
    end;
  end;
end;

procedure TCompileForm.VHideClick(Sender: TObject);
begin
  SetStatusPanelVisible(False);
end;

procedure TCompileForm.VCompilerOutputClick(Sender: TObject);
begin
  TabSet.TabIndex := tiCompilerOutput;
  SetStatusPanelVisible(True);
end;

procedure TCompileForm.VDebugOutputClick(Sender: TObject);
begin
  TabSet.TabIndex := tiDebugOutput;
  SetStatusPanelVisible(True);
end;

procedure TCompileForm.BMenuClick(Sender: TObject);
begin
  BLowPriority.Checked := FOptions.LowPriorityDuringCompile;
  BOpenOutputFolder.Enabled := (FCompiledExe <> '');
end;

procedure TCompileForm.BCompileClick(Sender: TObject);
begin
  CompileFile('', False);
end;

procedure TCompileForm.BStopCompileClick(Sender: TObject);
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

procedure TCompileForm.BLowPriorityClick(Sender: TObject);
begin
  FOptions.LowPriorityDuringCompile := not FOptions.LowPriorityDuringCompile;
  { If a compile is already in progress, change the priority now }
  if FCompiling then
    SetLowPriority(FOptions.LowPriorityDuringCompile);
end;

procedure TCompileForm.BOpenOutputFolderClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := GetWinDir;
  ShellExecute(Application.Handle, 'open', PChar(AddBackslash(Dir) + 'explorer.exe'),
    PChar(Format('/select,"%s"', [FCompiledExe])), PChar(Dir), SW_SHOW);
end;

procedure TCompileForm.HMenuClick(Sender: TObject);
begin
  HISPPDoc.Visible := NewFileExists(PathExtractPath(NewParamStr(0)) + 'ispp.chm');
  HISPPSep.Visible := HISPPDoc.Visible;
end;

function GetHelpFile: String;
begin
  Result := PathExtractPath(NewParamStr(0)) + 'isetup.chm';
end;

procedure TCompileForm.HDocClick(Sender: TObject);
var
  HelpFile: String;
begin
  HelpFile := GetHelpFile;
  if Assigned(HtmlHelp) then
    HtmlHelp(GetDesktopWindow, PChar(HelpFile), HH_DISPLAY_TOPIC, 0);
end;

procedure TCompileForm.MemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  S, HelpFile: String;
  KLink: THH_AKLINK;
begin
  if Key = VK_F1 then begin
    HelpFile := GetHelpFile;
    if Assigned(HtmlHelp) then begin
      HtmlHelp(GetDesktopWindow, PChar(HelpFile), HH_DISPLAY_TOPIC, 0);
      S := Memo.WordAtCursor;
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

procedure TCompileForm.MemoKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = ' ') and (GetKeyState(VK_CONTROL) < 0) then begin
    InitiateAutoComplete(#0);
    Key := #0;
  end;
end;

procedure TCompileForm.HExamplesClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open',
    PChar(PathExtractPath(NewParamStr(0)) + 'Examples'), nil, nil, SW_SHOW);
end;

procedure TCompileForm.HFaqClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open',
    PChar(PathExtractPath(NewParamStr(0)) + 'isfaq.htm'), nil, nil, SW_SHOW);
end;

procedure TCompileForm.HWhatsNewClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open',
    PChar(PathExtractPath(NewParamStr(0)) + 'whatsnew.htm'), nil, nil, SW_SHOW);
end;

procedure TCompileForm.HWebsiteClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', 'http://www.innosetup.com/', nil,
    nil, SW_SHOW);
end;

procedure TCompileForm.HPSWebsiteClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', 'http://www.remobjects.com/ps', nil,
    nil, SW_SHOW);
end;

procedure TCompileForm.HISPPDocClick(Sender: TObject);
begin
  if Assigned(HtmlHelp) then
    HtmlHelp(GetDesktopWindow, PChar(GetHelpFile + '::/hh_isppredirect.xhtm'), HH_DISPLAY_TOPIC, 0);
end;

procedure TCompileForm.HDonateClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', 'http://www.jrsoftware.org/isdonate.php', nil,
    nil, SW_SHOW);
end;

procedure TCompileForm.HAboutClick(Sender: TObject);
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
  S := S + ('Copyright (C) 1997-2012 Jordan Russell' + SNewLine +
    'Portions Copyright (C) 2000-2012 Martijn Laan' + SNewLine +
    'All rights reserved.' + SNewLine2 +
    'Inno Setup home page:' + SNewLine +
    'http://www.innosetup.com/' + SNewLine2 +
    'RemObjects Pascal Script home page:' + SNewLine +
    'http://www.remobjects.com/ps' + SNewLine2 +
    'Refer to LICENSE.TXT for conditions of distribution and use.');
  MsgBox(S, 'About ' + FCompilerVersion.Title, mbInformation, MB_OK);
end;

procedure TCompileForm.WMStartCommandLineCompile(var Message: TMessage);
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

procedure TCompileForm.WMStartCommandLineWizard(var Message: TMessage);
var
  Code: Integer;
begin
  Code := 0;
  try
    try
      NewWizardFile;
    except
      Code := 2;
      Application.HandleException(Self);
    end;
  finally
    Halt(Code);
  end;
end;

procedure TCompileForm.WMStartNormally(var Message: TMessage);

  procedure ShowStartupForm;
  var
    StartupForm: TStartupForm;
    Ini: TConfigIniFile;
  begin
    ReadMRUList;
    StartupForm := TStartupForm.Create(Application);
    try
      StartupForm.MRUList := FMRUList;
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
            FNewClick(Self);
          srWizard:
            FNewWizardClick(Self);
          srOpenFile:
            if ConfirmCloseFile(True) then
              OpenMRUFile(StartupForm.ResultFileName);
          srOpenDialog:
            ShowOpenDialog(False);
          srOpenDialogExamples:
            ShowOpenDialog(True);
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
    OpenFile(CommandLineFilename, False);
end;

procedure TCompileForm.InitializeFindText(Dlg: TFindDialog);
var
  S: String;
begin
  S := Memo.SelText;
  if (S <> '') and (Pos(#13, S) = 0) and (Pos(#10, S) = 0) then
    Dlg.FindText := S
  else
    Dlg.FindText := FLastFindText;
end;

procedure TCompileForm.EFindClick(Sender: TObject);
begin
  ReplaceDialog.CloseDialog;
  if FindDialog.Handle = 0 then
    InitializeFindText(FindDialog);
  FindDialog.Execute;
end;

procedure TCompileForm.EFindNextClick(Sender: TObject);
begin
  if FLastFindText = '' then
    EFindClick(Sender)
  else
    FindNext;
end;

function FindOptionsToSearchOptions(const FindOptions: TFindOptions): TScintFindOptions;
begin
  Result := [];
  if frMatchCase in FindOptions then
    Include(Result, sfoMatchCase);
  if frWholeWord in FindOptions then
    Include(Result, sfoWholeWord);
end;

procedure TCompileForm.FindNext;
var
  StartPos, EndPos: Integer;
  Range: TScintRange;
begin
  if frDown in FLastFindOptions then begin
    StartPos := Memo.Selection.EndPos;
    EndPos := Memo.RawTextLength;
  end
  else begin
    StartPos := Memo.Selection.StartPos;
    EndPos := 0;
  end;
  if Memo.FindText(StartPos, EndPos, FLastFindText,
     FindOptionsToSearchOptions(FLastFindOptions), Range) then
    Memo.Selection := Range
  else
    MsgBoxFmt('Cannot find "%s"', [FLastFindText], SCompilerFormCaption,
      mbInformation, MB_OK);
end;

procedure TCompileForm.FindDialogFind(Sender: TObject);
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

procedure TCompileForm.EReplaceClick(Sender: TObject);
begin
  FindDialog.CloseDialog;
  if ReplaceDialog.Handle = 0 then begin
    InitializeFindText(ReplaceDialog);
    ReplaceDialog.ReplaceText := FLastReplaceText;
  end;
  ReplaceDialog.Execute;
end;

procedure TCompileForm.ReplaceDialogReplace(Sender: TObject);
var
  ReplaceCount, Pos: Integer;
  Range, NewRange: TScintRange;
begin
  FLastFindOptions := ReplaceDialog.Options;
  FLastFindText := ReplaceDialog.FindText;
  FLastReplaceText := ReplaceDialog.ReplaceText;

  if frReplaceAll in FLastFindOptions then begin
    ReplaceCount := 0;
    Memo.BeginUndoAction;
    try
      Pos := 0;
      while Memo.FindText(Pos, Memo.RawTextLength, FLastFindText,
         FindOptionsToSearchOptions(FLastFindOptions), Range) do begin
        NewRange := Memo.ReplaceTextRange(Range.StartPos, Range.EndPos, FLastReplaceText);
        Pos := NewRange.EndPos;
        Inc(ReplaceCount);
      end;
    finally
      Memo.EndUndoAction;
    end;
    if ReplaceCount = 0 then
      MsgBoxFmt('Cannot find "%s"', [FLastFindText], SCompilerFormCaption,
        mbInformation, MB_OK)
    else
      MsgBoxFmt('%d occurrence(s) replaced.', [ReplaceCount], SCompilerFormCaption,
        mbInformation, MB_OK);
  end
  else begin
    if Memo.SelTextEquals(FLastFindText, frMatchCase in FLastFindOptions) then
      Memo.SelText := FLastReplaceText;
    FindNext;
  end;
end;

procedure TCompileForm.UpdateStatusPanelHeight(H: Integer);
var
  MinHeight, MaxHeight: Integer;
begin
  MinHeight := (3 * DebugOutputList.ItemHeight + 4) +
    SpacerPaintBox.Height + TabSet.Height;
  MaxHeight := BodyPanel.ClientHeight - 48 - SplitPanel.Height;
  if H > MaxHeight then H := MaxHeight;
  if H < MinHeight then H := MinHeight;
  StatusPanel.Height := H;
end;

procedure TCompileForm.SplitPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and StatusPanel.Visible then begin
    UpdateStatusPanelHeight(BodyPanel.ClientToScreen(Point(0, 0)).Y -
      SplitPanel.ClientToScreen(Point(0, Y)).Y +
      BodyPanel.ClientHeight - (SplitPanel.Height div 2));
  end;
end;

procedure TCompileForm.TAddRemoveProgramsClick(Sender: TObject);
var
  Dir: String;
  Wow64DisableWow64FsRedirectionFunc: function(var OldValue: Pointer): BOOL; stdcall;
  Wow64RevertWow64FsRedirectionFunc: function(OldValue: Pointer): BOOL; stdcall;
  RedirDisabled: Boolean;
  RedirOldValue: Pointer;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Dir := GetSystemDir
  else
    Dir := GetWinDir;

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  { Have to disable file system redirection because the 32-bit version of
    appwiz.cpl is buggy on XP x64 RC2 -- it doesn't show any Change/Remove
    buttons on 64-bit MSI entries, and it doesn't list non-MSI 64-bit apps
    at all. }
  Wow64DisableWow64FsRedirectionFunc := GetProcAddress(GetModuleHandle(kernel32),
    'Wow64DisableWow64FsRedirection');
  Wow64RevertWow64FsRedirectionFunc := GetProcAddress(GetModuleHandle(kernel32),
    'Wow64RevertWow64FsRedirection');
  RedirDisabled := Assigned(Wow64DisableWow64FsRedirectionFunc) and
    Assigned(Wow64RevertWow64FsRedirectionFunc) and
    Wow64DisableWow64FsRedirectionFunc(RedirOldValue);
  try
    Win32Check(CreateProcess(nil, PChar('"' + AddBackslash(Dir) + 'control.exe" appwiz.cpl'),
       nil, nil, False, 0, nil, PChar(Dir), StartupInfo, ProcessInfo));
  finally
    if RedirDisabled then
      Wow64RevertWow64FsRedirectionFunc(RedirOldValue);
  end;
  CloseHandle(ProcessInfo.hProcess);
  CloseHandle(ProcessInfo.hThread);
end;

procedure TCompileForm.TGenerateGUIDClick(Sender: TObject);
begin
  if MsgBox('The generated GUID will be inserted into the editor at the cursor position. Continue?',
     SCompilerFormCaption, mbConfirmation, MB_YESNO) = IDYES then
    Memo.SelText := GenerateGuid;
end;

procedure TCompileForm.TSignToolsClick(Sender: TObject);
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
    OptionsForm.UnderlineErrorsCheck.Checked := FOptions.UnderlineErrors;
    OptionsForm.CursorPastEOLCheck.Checked := FOptions.CursorPastEOL;
    OptionsForm.TabWidthEdit.Text := IntToStr(FOptions.TabWidth);
    OptionsForm.UseTabCharacterCheck.Checked := FOptions.UseTabCharacter;
    OptionsForm.WordWrapCheck.Checked := FOptions.WordWrap;
    OptionsForm.AutoIndentCheck.Checked := FOptions.AutoIndent;
    OptionsForm.IndentationGuidesCheck.Checked := FOptions.IndentationGuides;
    OptionsForm.FontPanel.Font.Assign(Memo.Font);

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
    FOptions.UnderlineErrors := OptionsForm.UnderlineErrorsCheck.Checked;
    FOptions.CursorPastEOL := OptionsForm.CursorPastEOLCheck.Checked;
    FOptions.TabWidth := StrToInt(OptionsForm.TabWidthEdit.Text);
    FOptions.UseTabCharacter := OptionsForm.UseTabCharacterCheck.Checked;
    FOptions.WordWrap := OptionsForm.WordWrapCheck.Checked;
    FOptions.AutoIndent := OptionsForm.AutoIndentCheck.Checked;
    FOptions.IndentationGuides := OptionsForm.IndentationGuidesCheck.Checked;
    UpdateCaption;
    { Move caret to start of line to ensure it doesn't end up in the middle
      of a double-byte character if the code page changes from SBCS to DBCS }
    Memo.CaretLine := Memo.CaretLine;
    Memo.Font.Assign(OptionsForm.FontPanel.Font);
    SyncEditorOptions;
    UpdateNewButtons;

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
      Ini.WriteBool('Options', 'UnderlineErrors', FOptions.UnderlineErrors);
      Ini.WriteBool('Options', 'EditorCursorPastEOL', FOptions.CursorPastEOL);
      Ini.WriteInteger('Options', 'TabWidth', FOptions.TabWidth);
      Ini.WriteBool('Options', 'UseTabCharacter', FOptions.UseTabCharacter);
      Ini.WriteBool('Options', 'WordWrap', FOptions.WordWrap);
      Ini.WriteBool('Options', 'AutoIndent', FOptions.AutoIndent);
      Ini.WriteBool('Options', 'IndentationGuides', FOptions.IndentationGuides);
      Ini.WriteString('Options', 'EditorFontName', Memo.Font.Name);
      Ini.WriteInteger('Options', 'EditorFontSize', Memo.Font.Size);
      Ini.WriteInteger('Options', 'EditorFontCharset', Memo.Font.Charset);
    finally
      Ini.Free;
    end;
  finally
    OptionsForm.Free;
  end;
end;

procedure TCompileForm.MoveCaret(const LineNumber: Integer;
  const AlwaysResetColumn: Boolean);
var
  Pos: Integer;
begin
  if AlwaysResetColumn or (Memo.CaretLine <> LineNumber) then
    Pos := Memo.GetPositionFromLine(LineNumber)
  else
    Pos := Memo.CaretPosition;

  { If the line isn't in view, scroll so that it's in the center }
  if not Memo.IsPositionInViewVertically(Pos) then
    Memo.TopLine := Memo.GetVisibleLineFromDocLine(LineNumber) -
      (Memo.LinesInWindow div 2);

  Memo.CaretPosition := Pos;
  ActiveControl := Memo;
end;

procedure TCompileForm.SetErrorLine(ALine: Integer);
var
  OldLine: Integer;
begin
  if FErrorLine <> ALine then begin
    OldLine := FErrorLine;
    FErrorLine := ALine;
    if OldLine >= 0 then
      UpdateLineMarkers(OldLine);
    if FErrorLine >= 0 then begin
      FErrorCaretPosition := Memo.CaretPosition;
      UpdateLineMarkers(FErrorLine);
    end;
  end;
end;

procedure TCompileForm.SetStepLine(ALine: Integer);
var
  OldLine: Integer;
begin
  if FStepLine <> ALine then begin
    OldLine := FStepLine;
    FStepLine := ALine;
    if OldLine >= 0 then
      UpdateLineMarkers(OldLine);
    if FStepLine >= 0 then
      UpdateLineMarkers(FStepLine);
  end;
end;

procedure TCompileForm.HideError;
begin
  SetErrorLine(-1);
  if not FCompiling then
    StatusBar.Panels[spExtraStatus].Text := '';
end;

procedure TCompileForm.UpdateEditModePanel;
const
  InsertText: array[Boolean] of String = ('Overwrite', 'Insert');
begin
  if Memo.ReadOnly then
    StatusBar.Panels[spInsertMode].Text := 'Read only'
  else
    StatusBar.Panels[spInsertMode].Text := InsertText[Memo.InsertMode];
end;

procedure TCompileForm.MemoUpdateUI(Sender: TObject);

  procedure UpdatePendingSquiggly;
  var
    Pos: Integer;
    Value: Boolean;
  begin
    { Check for the inPendingSquiggly indicator on either side of the caret }
    Pos := Memo.CaretPosition;
    Value := False;
    if Memo.CaretVirtualSpace = 0 then begin
      Value := (inPendingSquiggly in Memo.GetIndicatorsAtPosition(Pos));
      if not Value and (Pos > 0) then
        Value := (inPendingSquiggly in Memo.GetIndicatorsAtPosition(Pos-1));
    end;
    if FOnPendingSquiggly <> Value then begin
      FOnPendingSquiggly := Value;
      { If caret has left a pending squiggly, force restyle of the line }
      if not Value then begin
        { Stop reporting the caret position to the styler (until the next
          Change event) so the token doesn't re-enter pending-squiggly state
          if the caret comes back and something restyles the line }
        Memo.ReportCaretPositionToStyler := False;
        Memo.RestyleLine(Memo.GetLineFromPosition(FPendingSquigglyCaretPos));
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
    Section := MemoStyler.GetSectionFromLineState(Memo.Lines.State[Memo.CaretLine]);
    if (Section <> scNone) and (Memo.CaretVirtualSpace = 0) then begin
      Pos := Memo.CaretPosition;
      C := Memo.GetCharAtPosition(Pos);
      if C in ['(', '[', '{'] then begin
        MatchPos := Memo.GetPositionOfMatchingBrace(Pos);
        if MatchPos >= 0 then begin
          Memo.SetBraceHighlighting(Pos, MatchPos);
          Exit;
        end;
      end;
      if Pos > 0 then begin
        Pos := Memo.GetPositionBefore(Pos);
        C := Memo.GetCharAtPosition(Pos);
        if C in [')', ']', '}'] then begin
          MatchPos := Memo.GetPositionOfMatchingBrace(Pos);
          if MatchPos >= 0 then begin
            Memo.SetBraceHighlighting(Pos, MatchPos);
            Exit;
          end;
        end;
      end;
    end;
    Memo.SetBraceHighlighting(-1, -1);
  end;

begin
  if (FErrorLine < 0) or (Memo.CaretPosition <> FErrorCaretPosition) then
    HideError;
  StatusBar.Panels[spCaretPos].Text := Format('%4d:%4d', [Memo.CaretLine + 1,
    Memo.CaretColumnExpanded + 1]);
  UpdatePendingSquiggly;
  UpdateBraceHighlighting;
  UpdateEditModePanel;
end;

procedure TCompileForm.MemoModifiedChange(Sender: TObject);
begin
  if Memo.Modified then
    StatusBar.Panels[spModified].Text := 'Modified'
  else
    StatusBar.Panels[spModified].Text := '';
end;

procedure TCompileForm.MemoChange(Sender: TObject; const Info: TScintEditChangeInfo);

  procedure LinesInsertedOrDeleted;
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
      MemoLinesInserted(Line, Info.LinesDelta)
    else
      MemoLinesDeleted(Line, -Info.LinesDelta, FirstAffectedLine);
  end;

begin
  FModifiedSinceLastCompile := True;
  if FDebugging then
    FModifiedSinceLastCompileAndGo := True
  else begin
    { Modified while not debugging; free the debug info and clear the dots }
    DestroyDebugInfo;
  end;

  if Info.LinesDelta <> 0 then
    LinesInsertedOrDeleted;

  { When the Delete key is pressed, the caret doesn't move, so reset
    FErrorCaretPosition to ensure that OnUpdateUI calls HideError }
  FErrorCaretPosition := -1;

  { The change should trigger restyling. Allow the styler to see the current
    caret position in case it wants to set a pending squiggly indicator. }
  Memo.ReportCaretPositionToStyler := True;
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
  if Memo.AutoCompleteActive or Memo.ReadOnly then
    Exit;

  Memo.CaretPosition := Memo.CaretPosition;  { clear any selection }
  CaretPos := Memo.CaretPosition;
  Line := Memo.GetLineFromPosition(CaretPos);
  Section := MemoStyler.GetSectionFromLineState(Memo.Lines.State[Line]);

  WordList := MemoStyler.KeywordList[Section];
  if WordList = '' then
    Exit;
  IsParamSection := MemoStyler.IsParamSection(Section);

  LinePos := Memo.GetPositionFromLine(Line);
  WordStartPos := Memo.GetWordStartPosition(CaretPos, True);
  WordEndPos := Memo.GetWordEndPosition(CaretPos, True);
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
    I := Memo.GetPositionBefore(I);
    if I < LinePos then
      Exit;  { shouldn't get here }
    C := Memo.GetCharAtPosition(I);
    { Make sure it's an stSymbol ';' and not one inside a quoted string }
    if IsParamSection and (C = ';') and
       MemoStyler.IsSymbolStyle(Memo.GetStyleAtPosition(I)) then begin
      FoundSemicolon := True;
      Break;
    end;
    if (Section = scLangOptions) and (C = '.') and not FoundDot then begin
      { Verify that a word (language name) precedes the '.', then check for
        any non-whitespace characters before the word }
      LangNamePos := Memo.GetWordStartPosition(I, True);
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
    Memo.SetAutoCompleteFillupChars(':')
  else
    Memo.SetAutoCompleteFillupChars('=');
  Memo.ShowAutoComplete(CharsBefore, WordList);
end;

procedure TCompileForm.MemoCharAdded(Sender: TObject; Ch: AnsiChar);

  function LineIsBlank(const Line: Integer): Boolean;
  var
    S: TScintRawString;
    I: Integer;
  begin
    S := Memo.Lines.RawLines[Line];
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
  if FOptions.AutoIndent and (Ch = Memo.LineEndingString[Length(Memo.LineEndingString)]) then begin
    { Add to the new line any (remaining) indentation from the previous line }
    NewLine := Memo.CaretLine;
    PreviousLine := NewLine-1;
    if PreviousLine >= 0 then begin
      NewIndent := Memo.GetLineIndentation(NewLine);
      { If no indentation was moved from the previous line to the new line
        (i.e., there are no spaces/tabs directly to the right of the new
        caret position), and the previous line is completely empty (0 length),
        then use the indentation from the last line containing non-space
        characters. }
      if (NewIndent = 0) and (Memo.Lines.RawLineLengths[PreviousLine] = 0) then begin
        Dec(PreviousLine);
        while (PreviousLine >= 0) and LineIsBlank(PreviousLine) do
          Dec(PreviousLine);
      end;
      if PreviousLine >= 0 then begin
        PreviousIndent := Memo.GetLineIndentation(PreviousLine);
        { If virtual space is enabled, and tabs are not being used for
          indentation (typing in virtual space doesn't create tabs), then we
          don't actually have to set any indentation if the new line is
          empty; we can just move the caret out into virtual space. }
        if (svsUserAccessible in Memo.VirtualSpaceOptions) and
           not Memo.UseTabCharacter and
           (Memo.Lines.RawLineLengths[NewLine] = 0) then begin
          Memo.CaretVirtualSpace := PreviousIndent;
        end
        else begin
          Memo.SetLineIndentation(NewLine, NewIndent + PreviousIndent);
          Memo.CaretPosition := Memo.GetPositionFromLineExpandedColumn(NewLine,
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
      (FOptions.AutoComplete or Memo.AutoCompleteActive);
    Memo.CancelAutoComplete;
    if RestartAutoComplete then
      InitiateAutoComplete(Ch);
  end;
end;

procedure TCompileForm.MemoHintShow(Sender: TObject; var Info: TScintHintInfo);

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
{$IFDEF UNICODE}
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
    LinePos := Memo.GetPositionFromLine(Memo.GetLineFromPosition(Pos));
    S := Memo.GetRawTextRange(LinePos, Pos);
    U := Memo.ConvertRawStringToString(S);
    Result := Length(U);
  end;
{$ELSE}
  begin
    Result := Memo.GetColumnFromPosition(Pos);
  end;
{$ENDIF}

  function FindConstRange(const Pos: Integer): TScintRange;
  var
    BraceLevel, ConstStartPos, Line, LineEndPos, I: Integer;
    C: AnsiChar;
  begin
    Result.StartPos := 0;
    Result.EndPos := 0;
    BraceLevel := 0;
    ConstStartPos := -1;
    Line := Memo.GetLineFromPosition(Pos);
    LineEndPos := Memo.GetLineEndPosition(Line);
    I := Memo.GetPositionFromLine(Line);
    while I < LineEndPos do begin
      if (I > Pos) and (BraceLevel = 0) then
        Break;
      C := Memo.GetCharAtPosition(I);
      if C = '{' then begin
        if Memo.GetCharAtPosition(I + 1) = '{' then
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
      I := Memo.GetPositionAfter(I);
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
  Pos := Memo.GetPositionFromPoint(Info.CursorPos, True, True);
  if Pos < 0 then
    Exit;
  Line := Memo.GetLineFromPosition(Pos);

  { Check if cursor is over a [Code] variable }
  if MemoStyler.GetSectionFromLineState(Memo.Lines.State[Line]) = scCode then begin
    { Note: The '+ 1' is needed so that when the mouse is over a '.'
      between two words, it won't match the word to the left of the '.' }
    I := Memo.GetWordStartPosition(Pos + 1, True);
    J := Memo.GetWordEndPosition(Pos, True);
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
        Info.CursorRect.TopLeft := Memo.GetPointFromPosition(I);
        Info.CursorRect.BottomRight := Memo.GetPointFromPosition(J);
        Info.CursorRect.Bottom := Info.CursorRect.Top + Memo.LineHeight;
        Info.HideTimeout := High(Integer);  { infinite }
        Exit;
      end;
    end;
  end;

  { Check if cursor is over a constant }
  ConstRange := FindConstRange(Pos);
  if ConstRange.EndPos > ConstRange.StartPos then begin
    Info.HintStr := Memo.GetTextRange(ConstRange.StartPos, ConstRange.EndPos);
    case EvaluateConstant(Info.HintStr, Output) of
      1: Info.HintStr := Info.HintStr + ' = "' + Output + '"';
      2: Info.HintStr := Info.HintStr + ' = Exception: ' + Output;
    else
      Info.HintStr := Info.HintStr + ' = Unknown error';
    end;
    Info.CursorRect.TopLeft := Memo.GetPointFromPosition(ConstRange.StartPos);
    Info.CursorRect.BottomRight := Memo.GetPointFromPosition(ConstRange.EndPos);
    Info.CursorRect.Bottom := Info.CursorRect.Top + Memo.LineHeight;
    Info.HideTimeout := High(Integer);  { infinite }
  end;
end;

procedure TCompileForm.MemoDropFiles(Sender: TObject; X, Y: Integer;
  AFiles: TStrings);
begin
  if (AFiles.Count > 0) and ConfirmCloseFile(True) then
    OpenFile(AFiles[0], True);
end;

procedure TCompileForm.StatusBarResize(Sender: TObject);
begin
  { Without this, on Windows XP with themes, the status bar's size grip gets
    corrupted as the form is resized }
  if StatusBar.HandleAllocated then
    InvalidateRect(StatusBar.Handle, nil, True);
end;

procedure TCompileForm.WMDebuggerQueryVersion(var Message: TMessage);
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

procedure TCompileForm.WMDebuggerGoodbye(var Message: TMessage);
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

  if (LineNumber < FLineStateCount) and
     (FLineState[LineNumber] <> lnEntryProcessed) then begin
    FLineState[LineNumber] := lnEntryProcessed;
    UpdateLineMarkers(LineNumber);
  end;

  if (FStepMode = smStepInto) or
     ((FStepMode = smStepOver) and not Intermediate) or
     ((FStepMode = smRunToCursor) and
      (FRunToCursorPoint.Kind = Message.WParam) and
      (FRunToCursorPoint.Index = Message.LParam)) or
     (FBreakPoints.IndexOf(Pointer(LineNumber)) <> -1) then begin
    MoveCaret(LineNumber, True);
    HideError;
    SetStepLine(LineNumber);
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
      MoveCaret(LineNumber, True);
      SetStepLine(-1);
      SetErrorLine(LineNumber);
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
  end;
end;

procedure TCompileForm.DestroyDebugInfo;
var
  HadDebugInfo: Boolean;
begin
  HadDebugInfo := Assigned(FLineState);

  FLineStateCapacity := 0;
  FLineStateCount := 0;
  FreeMem(FLineState);
  FLineState := nil;

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
    UpdateAllLineMarkers;
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
    I := Memo.Lines.Count;
    FLineState := AllocMem(SizeOf(TLineState) * (I + LineStateGrowAmount));
    FLineStateCapacity := I + LineStateGrowAmount;
    FLineStateCount := I;

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
         (FDebugEntries[I].LineNumber < FLineStateCount) then begin
        if FLineState[FDebugEntries[I].LineNumber] = lnUnknown then
          FLineState[FDebugEntries[I].LineNumber] := lnHasEntry;
      end;
    end;
    UpdateAllLineMarkers;
  except
    DestroyDebugInfo;
    raise;
  end;
end;

procedure TCompileForm.ResetLineState;
{ Changes green dots back to grey dots }
var
  I: Integer;
begin
  for I := 0 to FLineStateCount-1 do
    if FLineState[I] = lnEntryProcessed then begin
      FLineState[I] := lnHasEntry;
      UpdateLineMarkers(I);
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
  HideError;
  SetStepLine(-1);
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
  RRunToCursor.Enabled := RRun.Enabled;
  RStepInto.Enabled := RRun.Enabled;
  RStepOver.Enabled := RRun.Enabled;
  RTerminate.Enabled := FDebugging and (FDebugClientWnd <> 0);
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

procedure TCompileForm.UpdateThemeData(const Close, Open: Boolean);
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
  end else
    RunFilename := FCompiledExe;
  RunParameters := Format('/DEBUGWND=$%x ', [Handle]) + FRunParameters;

  ResetLineState;
  DebugOutputList.Clear;
  SendMessage(DebugOutputList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
  TabSet.TabIndex := tiDebugOutput;
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
begin
  CheckIfTerminated;

  { Display warning if the user modified the script while running }
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

  if not FDebugging and FModifiedSinceLastCompile then
    CompileFile('', False);
end;

procedure TCompileForm.Go(AStepMode: TStepMode);
begin
  CompileIfNecessary;
  FStepMode := AStepMode;
  HideError;
  SetStepLine(-1);
  if FDebugging then begin
    if FPaused then begin
      FPaused := False;
      UpdateRunMenu;
      UpdateCaption;
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
  InputQuery('Run Parameters', 'Command line parameters for ' + DebugTargetStrings[dtSetup] +
    ' and ' + DebugTargetStrings[dtUninstall] + ':', FRunParameters);
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
  if not GetDebugEntryFromLineNumber(Memo.CaretLine, FRunToCursorPoint) then begin
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
begin
  Clipboard.AsText := (ListPopupMenu.PopupComponent as TListBox).Items.Text;
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
  S := IntToStr(Memo.CaretLine + 1);
  if InputQuery('Go to Line', 'Line number:', S) then begin
    L := StrToIntDef(S, Low(L));
    if L <> Low(L) then
      Memo.CaretLine := L - 1;
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
        ImageList_Draw(FBuildImageList, FBuildAnimationFrame, StatusBar.Canvas.Handle,
          Rect.Left + ((Rect.Right - Rect.Left) - 17) div 2,
          Rect.Top + ((Rect.Bottom - Rect.Top) - 15) div 2, ILD_NORMAL);
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
        [(ASecondsRemaining div 60) div 60, TimeSeparator,
         (ASecondsRemaining div 60) mod 60, TimeSeparator,
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
  if FFilename = '' then
    Exit;

  { See if the file has been modified outside the editor }
  Changed := False;
  if GetLastWriteTimeOfFile(FFilename, NewTime) then begin
    if CompareFileTime(FFileLastWriteTime, NewTime) <> 0 then begin
      FFileLastWriteTime := NewTime;
      Changed := True;
    end;
  end;

  { If it has been, offer to reload it }
  if Changed then begin
    if IsWindowEnabled(Application.Handle) then begin
      if MsgBox(FFilename + SNewLine2 + ReloadMessages[Memo.Modified],
         SCompilerFormCaption, mbConfirmation, MB_YESNO) = IDYES then
        if ConfirmCloseFile(False) then
          OpenFile(FFilename, False);
    end
    else begin
      { When a modal dialog is up, don't offer to reload the file. Probably
        not a good idea since the dialog might be manipulating the file. }
      MsgBox(FFilename + SNewLine2 + 'The file has been modified outside ' +
        'of the source editor. You might want to reload it.',
        SCompilerFormCaption, mbInformation, MB_OK);
    end;
  end;
end;

procedure TCompileForm.DebugOutputListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);

  function SafeGetItem(const ListBoxHandle: HWND; const Index: Integer): String;
  { Prior to Delphi 6, the VCL will incur a buffer overflow if you trying
    reading an item from a TListBox longer than 4096 characters. }
  var
    Len: Integer;
  begin
    Len := SendMessage(ListBoxHandle, LB_GETTEXTLEN, Index, 0);
    if Len <= 0 then
      Result := ''  { zero length or out of range? }
    else begin
      SetString(Result, nil, Len);
      Len := SendMessage(ListBoxHandle, LB_GETTEXT, Index, LPARAM(Result));
      if Len <= 0 then
        Result := ''  { failed? }
      else
        SetLength(Result, Len);  { since LB_GETTEXTLEN can overestimate }
    end;
  end;

var
  S: String;
begin
  { An owner drawn list box is used for precise tab expansion }  
  S := SafeGetItem(DebugOutputList.Handle, Index);
  DebugOutputList.Canvas.FillRect(Rect);
  Inc(Rect.Left, 2);
  if (S <> '') and (S[1] = #9) then
    DebugOutputList.Canvas.TextOut(Rect.Left + FDebugLogListTimeWidth,
      Rect.Top, Copy(S, 2, Maxint))
  else begin
    if (Length(S) > 16) and (S[14] = '-') and (S[15] = '-') and (S[16] = ' ') then begin
      { Draw lines that begin with '-- ' (like '-- File entry --') in bold }
      DebugOutputList.Canvas.TextOut(Rect.Left, Rect.Top, Copy(S, 1, 13));
      DebugOutputList.Canvas.Font.Style := [fsBold];
      DebugOutputList.Canvas.TextOut(Rect.Left + FDebugLogListTimeWidth,
        Rect.Top, Copy(S, 14, Maxint));
    end
    else
      DebugOutputList.Canvas.TextOut(Rect.Left, Rect.Top, S);
  end;
end;

procedure TCompileForm.TabSetClick(Sender: TObject);
begin
  case TabSet.TabIndex of
    tiCompilerOutput:
      begin
        CompilerOutputList.BringToFront;
        CompilerOutputList.Visible := True;
        DebugOutputList.Visible := False;
      end;
    tiDebugOutput:
      begin
        DebugOutputList.BringToFront;
        DebugOutputList.Visible := True;
        CompilerOutputList.Visible := False;
      end;
  end;
end;

procedure TCompileForm.ToggleBreakPoint(Line: Integer);
var
  I: Integer;
begin
  I := FBreakPoints.IndexOf(Pointer(Line));
  if I = -1 then
    FBreakPoints.Add(Pointer(Line))
  else
    FBreakPoints.Delete(I);
  UpdateLineMarkers(Line);
end;

procedure TCompileForm.MemoMarginClick(Sender: TObject; MarginNumber: Integer;
  Line: Integer);
begin
  if MarginNumber = 1 then
    ToggleBreakPoint(Line);
end;

procedure TCompileForm.MemoLinesInserted(FirstLine, Count: integer);
var
  I, Line: Integer;
begin
  for I := 0 to FDebugEntriesCount-1 do
    if FDebugEntries[I].LineNumber >= FirstLine then
      Inc(FDebugEntries[I].LineNumber, Count);

  if Assigned(FLineState) and (FirstLine < FLineStateCount) then begin
    { Grow FStateLine if necessary }
    I := (FLineStateCount + Count) - FLineStateCapacity;
    if I > 0 then begin
      if I < LineStateGrowAmount then
        I := LineStateGrowAmount;
      ReallocMem(FLineState, SizeOf(TLineState) * (FLineStateCapacity + I));
      Inc(FLineStateCapacity, I);
    end;
    { Shift existing line states and clear the new ones }
    for I := FLineStateCount-1 downto FirstLine do
      FLineState[I + Count] := FLineState[I];
    for I := FirstLine to FirstLine + Count - 1 do
      FLineState[I] := lnUnknown;
    Inc(FLineStateCount, Count);
  end;

  if FStepLine >= FirstLine then
    Inc(FStepLine, Count);
  if FErrorLine >= FirstLine then
    Inc(FErrorLine, Count);

  for I := 0 to FBreakPoints.Count-1 do begin
    Line := Integer(FBreakPoints[I]);
    if Line >= FirstLine then
      FBreakPoints[I] := Pointer(Line + Count);
  end;
end;

procedure TCompileForm.MemoLinesDeleted(FirstLine, Count,
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

  if Assigned(FLineState) then begin
    { Shift existing line states }
    if FirstLine < FLineStateCount - Count then begin
      for I := FirstLine to FLineStateCount - Count - 1 do
        FLineState[I] := FLineState[I + Count];
      Dec(FLineStateCount, Count);
    end
    else begin
      { There's nothing to shift because the last line(s) were deleted, or
        line(s) past FLineStateCount }
      if FLineStateCount > FirstLine then
        FLineStateCount := FirstLine;
    end;
  end;

  if FStepLine >= FirstLine then begin
    if FStepLine < FirstLine + Count then
      FStepLine := -1
    else
      Dec(FStepLine, Count);
  end;
  if FErrorLine >= FirstLine then begin
    if FErrorLine < FirstLine + Count then
      FErrorLine := -1
    else
      Dec(FErrorLine, Count);
  end;

  for I := FBreakPoints.Count-1 downto 0 do begin
    Line := Integer(FBreakPoints[I]);
    if Line >= FirstLine then begin
      if Line < FirstLine + Count then begin
        FBreakPoints.Delete(I);
      end else begin
        Line := Line - Count;
        FBreakPoints[I] := Pointer(Line);
      end;
    end;
  end;

  { When lines are deleted, Scintilla insists on moving all of the deleted
    lines' markers to the line on which the deletion started
    (FirstAffectedLine). This is bad for us as e.g. it can result in the line
    having two conflicting markers (or two of the same marker). There's no
    way to stop it from doing that, or to easily tell which markers came from
    which lines, so we simply delete and re-create all markers on the line. }
  UpdateLineMarkers(FirstAffectedLine);
end;

procedure TCompileForm.UpdateLineMarkers(const Line: Integer);
var
  NewMarker: Integer;
begin
  if Line >= Memo.Lines.Count then
    Exit;

  NewMarker := -1;
  if FBreakPoints.IndexOf(Pointer(Line)) <> -1 then begin
    if FLineState = nil then
      NewMarker := mmIconBreakpoint
    else if (Line < FLineStateCount) and (FLineState[Line] <> lnUnknown) then
      NewMarker := mmIconBreakpointGood
    else
      NewMarker := mmIconBreakpointBad;
  end
  else begin
    if Line < FLineStateCount then begin
      case FLineState[Line] of
        lnHasEntry: NewMarker := mmIconHasEntry;
        lnEntryProcessed: NewMarker := mmIconEntryProcessed;
      end;
    end;
  end;

  { Delete all markers on the line. To flush out any possible duplicates,
    even the markers we'll be adding next are deleted. }
  if Memo.GetMarkers(Line) <> [] then
    Memo.DeleteAllMarkersOnLine(Line);

  if NewMarker <> -1 then
    Memo.AddMarker(Line, NewMarker);

  if FStepLine = Line then
    Memo.AddMarker(Line, mmLineStep)
  else if FErrorLine = Line then
    Memo.AddMarker(Line, mmLineError)
  else if NewMarker in [mmIconBreakpoint, mmIconBreakpointGood] then
    Memo.AddMarker(Line, mmLineBreakpoint)
  else if NewMarker = mmIconBreakpointBad then
    Memo.AddMarker(Line, mmLineBreakpointBad);
end;

procedure TCompileForm.UpdateAllLineMarkers;
var
  Line: Integer;
begin
  for Line := 0 to Memo.Lines.Count-1 do
    UpdateLineMarkers(Line);
end;

procedure TCompileForm.RToggleBreakPointClick(Sender: TObject);
begin
  ToggleBreakPoint(Memo.CaretLine);
end;

{$IFNDEF UNICODE}
var
  Compil32LeadBytes: TLeadByteSet;
{$ENDIF}

initialization
{$IFNDEF UNICODE}
  GetLeadBytes(Compil32LeadBytes);
  ConstLeadBytes := @Compil32LeadBytes;
{$ENDIF}
  InitThemeLibrary;
  InitHtmlHelpLibrary;
  { For ClearType support, try to make the default font Microsoft Sans Serif }
  if DefFontData.Name = 'MS Sans Serif' then
    DefFontData.Name := AnsiString(GetPreferredUIFont);
  CoInitialize(nil);
finalization
  CoUninitialize();
end.
