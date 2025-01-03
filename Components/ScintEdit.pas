unit ScintEdit;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TScintEdit component: a VCL wrapper for Scintilla
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Generics.Collections, ScintInt;

const
  StyleNumbers = 32; { The syntax highlighting can use up to 32 styles }
  StyleNumberBits = 5; { 5 bits are needed to store 32 values }
  StyleNumberMask = StyleNumbers-1; { To get the 5 bits from a byte it needs to be AND-ed with $1F = 31 }
  StyleNumberUnusedBits = 8-StyleNumberBits; { 3 bits of a byte are unused }

type
  TScintChangeHistory = (schDisabled, schMarkers, schIndicators);
  TScintCommand = type NativeInt;
  TScintEditAutoCompleteSelectionEvent = TNotifyEvent;
  TScintEditCallTipArrowClick = procedure(Sender: TObject; const Up: Boolean) of object;
  TScintEditChangeInfo = record
    Inserting: Boolean;
    StartPos, Length, LinesDelta: Integer;
  end;
  TScintEditChangeEvent = procedure(Sender: TObject;
    const Info: TScintEditChangeInfo) of object;
  TScintEditCharAddedEvent = procedure(Sender: TObject; Ch: AnsiChar) of object;
  TScintEditDropFilesEvent = procedure(Sender: TObject; X, Y: Integer;
    AFiles: TStrings) of object;
  TScintHintInfo = Controls.THintInfo;
  TScintEditHintShowEvent = procedure(Sender: TObject;
    var Info: TScintHintInfo) of object;
  TScintEditMarginClickEvent = procedure(Sender: TObject; MarginNumber: Integer;
    Line: Integer) of object;
  TScintEditUpdate = (suContent, suSelection, suVScroll, suHScroll);
  TScintEditUpdates = set of TScintEditUpdate;
  TScintEditUpdateUIEvent = procedure(Sender: TObject; Updated: TScintEditUpdates) of object;
  TScintFindOption = (sfoMatchCase, sfoWholeWord, sfoRegEx);
  TScintFindOptions = set of TScintFindOption;
  TScintFoldFlag = (sffLineBeforeExpanded, sffLineBeforeContracted,
    sffLineAfterExpanded, sffLineAfterContracted, sffLevelNumbers, sffLineState);
  TScintFoldFlags = set of TScintFoldFlag;
  TScintIndentationGuides = (sigNone, sigReal, sigLookForward, sigLookBoth);
  TScintKeyCode = type Word;
  TScintKeyDefinition = type Cardinal;
  TScintReplaceMode = (srmNormal, srmMinimal, srmRegEx);
  TScintStyleByteIndicatorNumber = 0..1; { Could be increased to 0..StyleNumberUnusedBits-1 }
  TScintStyleByteIndicatorNumbers = set of TScintStyleByteIndicatorNumber;
  TScintIndicatorNumber = INDICATOR_CONTAINER..INDICATOR_MAX;
  TScintLineEndings = (sleCRLF, sleCR, sleLF);
  TScintLineState = type Integer;
  TScintMarkerNumber = 0..31;
  TScintMarkerNumbers = set of TScintMarkerNumber;
  TScintRange = record
    StartPos, EndPos: Integer;
    constructor Create(const AStartPos, AEndPos: Integer);
    function Empty: Boolean;
    function Overlaps(const ARange: TScintRange): Boolean;
    function Within(const ARange: TScintRange): Boolean;
  end;
  TScintRangeList = class(TList<TScintRange>)
    function Overlaps(const ARange: TScintRange;
      var AOverlappingRange: TScintRange): Boolean;
  end;
  TScintCaretAndAnchor = record
    CaretPos, AnchorPos: Integer;
    constructor Create(const ACaretPos, AAnchorPos: Integer);
    function Range: TScintRange;
  end;
  TScintCaretAndAnchorList = class(TList<TScintCaretAndAnchor>);
  TScintRawCharSet = set of AnsiChar;
  TScintRawString = type RawByteString;
  TScintRectangle = record
    Left, Top, Right, Bottom: Integer;
  end;
  TScintSelectionMode = (ssmStream, ssmRectangular, ssmLines, ssmThinRectangular);
  TScintStyleNumber = 0..StyleNumbers-1;
  TScintVirtualSpaceOption = (svsRectangularSelection, svsUserAccessible,
    svsNoWrapLineStart);
  TScintVirtualSpaceOptions = set of TScintVirtualSpaceOption;
  PScintRangeToFormat = ^TScintRangeToFormat;
  TScintRangeToFormat = record
    hdc, hdcTarget: UINT_PTR;
    rc, rcPage: TScintRectangle;
    chrg: TScintRange;
  end;

  TScintEditStrings = class;
  TScintCustomStyler = class;

  EScintEditError = class(Exception);

  TScintEdit = class(TWinControl)
  private
    FAcceptDroppedFiles: Boolean;
    FAutoCompleteFontName: String;
    FAutoCompleteFontSize: Integer;
    FAutoCompleteStyle: Integer;
    FChangeHistory: TScintChangeHistory;
    FCodePage: Integer;
    FDirectPtr: Pointer;
    FDirectStatusFunction: SciFnDirectStatus;
    FEffectiveCodePage: Integer;
    FEffectiveCodePageDBCS: Boolean;
    FFillSelectionToEdge: Boolean;
    FFoldLevelNumbersOrLineState: Boolean;
    FForceModified: Boolean;
    FIndentationGuides: TScintIndentationGuides;
    FLeadBytes: TScintRawCharSet;
    FLineNumbers: Boolean;
    FLines: TScintEditStrings;
    FOnAutoCompleteSelection: TScintEditAutoCompleteSelectionEvent;
    FOnCallTipArrowClick: TScintEditCallTipArrowClick;
    FOnChange: TScintEditChangeEvent;
    FOnCharAdded: TScintEditCharAddedEvent;
    FOnDropFiles: TScintEditDropFilesEvent;
    FOnHintShow: TScintEditHintShowEvent;
    FOnMarginClick: TScintEditMarginClickEvent;
    FOnMarginRightClick: TScintEditMarginClickEvent;
    FOnModifiedChange: TNotifyEvent;
    FOnUpdateUI: TScintEditUpdateUIEvent;
    FOnZoom: TNotifyEvent;
    FReportCaretPositionToStyler: Boolean;
    FStyler: TScintCustomStyler;
    FTabWidth: Integer;
    FUseStyleAttributes: Boolean;
    FUseTabCharacter: Boolean;
    FVirtualSpaceOptions: TScintVirtualSpaceOptions;
    FWordChars: AnsiString;
    FWordCharsAsSet: TSysCharSet;
    FWordWrap: Boolean;
    procedure ApplyOptions;
    procedure ForwardMessage(const Message: TMessage);
    function GetAutoCompleteActive: Boolean;
    function GetCallTipActive: Boolean;
    function GetCaretColumn: Integer;
    function GetCaretColumnExpandedForTabs: Integer;
    function GetCaretLine: Integer;
    function GetCaretLineText: String;
    function GetCaretPosition: Integer;
    function GetCaretPositionInLine: Integer;
    function GetCaretVirtualSpace: Integer;
    function GetInsertMode: Boolean;
    function GetLineEndings: TScintLineEndings;
    function GetLineEndingString: TScintRawString;
    function GetLineHeight: Integer;
    function GetLinesInWindow: Integer;
    function GetMainSelText: String;
    function GetModified: Boolean;
    function GetRawCaretLineText: TScintRawString;
    function GetRawMainSelText: TScintRawString;
    function GetRawSelText: TScintRawString;
    function GetRawText: TScintRawString;
    function GetReadOnly: Boolean;
    class function GetReplaceTargetMessage(const ReplaceMode: TScintReplaceMode): Cardinal;
    class function GetSearchFlags(const Options: TScintFindOptions): Integer;
    function GetSelection: TScintRange;
    function GetSelectionAnchorPosition(Selection: Integer): Integer;
    function GetSelectionAnchorVirtualSpace(Selection: Integer): Integer;
    function GetSelectionCaretPosition(Selection: Integer): Integer;
    function GetSelectionCaretVirtualSpace(Selection: Integer): Integer;
    function GetSelectionEndPosition(Selection: Integer): Integer;
    function GetSelectionCount: Integer;
    function GetSelectionMode: TScintSelectionMode;
    function GetSelectionStartPosition(Selection: Integer): Integer;
    function GetSelText: String;
    function GetTopLine: Integer;
    function GetZoom: Integer;
    procedure SetAcceptDroppedFiles(const Value: Boolean);
    procedure SetAutoCompleteFontName(const Value: String);
    procedure SetAutoCompleteFontSize(const Value: Integer);
    procedure SetCodePage(const Value: Integer);
    procedure SetCaretColumn(const Value: Integer);
    procedure SetCaretLine(const Value: Integer);
    procedure SetCaretPosition(const Value: Integer);
    procedure SetCaretPositionWithSelectFromAnchor(const Value: Integer);
    procedure SetCaretVirtualSpace(const Value: Integer);
    procedure SetChangeHistory(const Value: TScintChangeHistory);
    procedure SetFillSelectionToEdge(const Value: Boolean);
    procedure SetFoldFlags(const Value: TScintFoldFlags);
    procedure SetIndentationGuides(const Value: TScintIndentationGuides);
    procedure SetLineNumbers(const Value: Boolean);
    procedure SetMainSelection(const Value: Integer);
    procedure SetMainSelText(const Value: String);
    procedure SetRawMainSelText(const Value: TScintRawString);
    procedure SetRawSelText(const Value: TScintRawString);
    procedure SetRawText(const Value: TScintRawString);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetSelection(const Value: TScintRange);
    procedure SetSelectionAnchorPosition(Selection: Integer; const Value: Integer);
    procedure SetSelectionAnchorVirtualSpace(Selection: Integer;
      const Value: Integer);
    procedure SetSelectionCaretPosition(Selection: Integer; const Value: Integer);
    procedure SetSelectionCaretVirtualSpace(Selection: Integer;
      const Value: Integer);
    procedure SetSelectionMode(const Value: TScintSelectionMode);
    procedure SetSelText(const Value: String);
    procedure SetStyler(const Value: TScintCustomStyler);
    procedure SetTabWidth(const Value: Integer);
    procedure SetTopLine(const Value: Integer);
    procedure SetUseStyleAttributes(const Value: Boolean);
    procedure SetUseTabCharacter(const Value: Boolean);
    procedure SetVirtualSpaceOptions(const Value: TScintVirtualSpaceOptions);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetZoom(const Value: Integer);
    procedure UpdateCodePage;
    procedure UpdateLineNumbersWidth;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
  protected
    procedure Change(const AInserting: Boolean; const AStartPos, ALength,
      ALinesDelta: Integer); virtual;
    procedure CheckPosRange(const StartPos, EndPos: Integer);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    class function GetErrorException(const S: String): EScintEditError;
    class procedure Error(const S: String); overload;
    class procedure ErrorFmt(const S: String; const Args: array of const);
    function GetMainSelection: Integer;
    function GetTarget: TScintRange;
    procedure InitRawString(var S: TScintRawString; const Len: Integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Notify(const N: TSCNotification); virtual;
    procedure SetTarget(const StartPos, EndPos: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddMarker(const Line: Integer; const Marker: TScintMarkerNumber);
    procedure AddSelection(const CaretPos, AnchorPos: Integer);
    procedure AssignCmdKey(const Key: AnsiChar; const Shift: TShiftState;
      const Command: TScintCommand); overload;
    procedure AssignCmdKey(const KeyCode: TScintKeyCode; const Shift: TShiftState;
      const Command: TScintCommand); overload;
    procedure BeginUndoAction;
    function Call(Msg: Cardinal; WParam: Longint; LParam: Longint): Longint; overload;
    function Call(Msg: Cardinal; WParam: Longint; LParam: Longint; out WarnStatus: Integer): Longint; overload;
    function Call(Msg: Cardinal; WParam: Longint; const LParamStr: TScintRawString): Longint; overload;
    function Call(Msg: Cardinal; WParam: Longint; const LParamStr: TScintRawString; out WarnStatus: Integer): Longint; overload;
    procedure CancelAutoComplete;
    procedure CancelAutoCompleteAndCallTip;
    procedure CancelCallTip;
    function CanPaste: Boolean;
    function CanRedo: Boolean;
    function CanUndo: Boolean;
    procedure ChooseCaretX;
    procedure ClearAll;
    procedure ClearCmdKey(const Key: AnsiChar; const Shift: TShiftState); overload;
    procedure ClearCmdKey(const KeyCode: TScintKeyCode; const Shift: TShiftState); overload;
    procedure ClearIndicators(const IndicatorNumber: TScintIndicatorNumber);
    procedure ClearSelection;
    procedure ClearUndo(const ClearChangeHistory: Boolean = True);
    function ConvertRawStringToString(const S: TScintRawString): String;
    function ConvertPCharToRawString(const Text: PChar;
      const TextLen: Integer): TScintRawString;
    function ConvertStringToRawString(const S: String): TScintRawString;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure DeleteAllMarkersOnLine(const Line: Integer);
    procedure DeleteMarker(const Line: Integer; const Marker: TScintMarkerNumber);
    procedure DPIChanged(const Message: TMessage);
    procedure EndUndoAction;
    procedure EnsureLineVisible(const Line: Integer);
    function FindRawText(const StartPos, EndPos: Integer; const S: TScintRawString;
      const Options: TScintFindOptions; out MatchRange: TScintRange): Boolean;
    function FindText(const StartPos, EndPos: Integer; const S: String;
      const Options: TScintFindOptions; out MatchRange: TScintRange): Boolean;
    procedure FoldLine(const Line: Integer; const Fold: Boolean);
    function FormatRange(const Draw: Boolean;
      const RangeToFormat: PScintRangeToFormat): Integer;
    procedure ForceModifiedState;
    function GetByteAtPosition(const Pos: Integer): AnsiChar;
    function GetCharacterCount(const StartPos, EndPos: Integer): Integer;
    function GetColumnFromPosition(const Pos: Integer): Integer;
    function GetDefaultWordChars: AnsiString;
    function GetDocLineFromVisibleLine(const VisibleLine: Integer): Integer;
    function GetIndicatorAtPosition(const IndicatorNumber: TScintIndicatorNumber;
      const Pos: Integer): Boolean;
    function GetLineEndPosition(const Line: Integer): Integer;
    function GetLineFromPosition(const Pos: Integer): Integer;
    function GetLineIndentation(const Line: Integer): Integer;
    function GetLineIndentPosition(const Line: Integer): Integer;
    function GetMarkers(const Line: Integer): TScintMarkerNumbers;
    function GetPointFromPosition(const Pos: Integer): TPoint;
    function GetPositionAfter(const Pos: Integer): Integer;
    function GetPositionBefore(const Pos: Integer): Integer;
    function GetPositionFromLine(const Line: Integer): Integer;
    function GetPositionFromLineColumn(const Line, Column: Integer): Integer;
    function GetPositionFromLineExpandedColumn(const Line, ExpandedColumn: Integer): Integer;
    function GetPositionFromPoint(const P: TPoint;
      const CharPosition, CloseOnly: Boolean): Integer;
    function GetPositionOfMatchingBrace(const Pos: Integer): Integer;
    function GetPositionRelative(const Pos, CharacterCount: Integer): Integer;
    function GetRawTextLength: Integer;
    function GetRawTextRange(const StartPos, EndPos: Integer): TScintRawString;
    procedure GetSelections(const RangeList: TScintRangeList); overload;
    procedure GetSelections(const CaretAndAnchorList: TScintCaretAndAnchorList); overload;
    procedure GetSelections(const CaretAndAnchorList, VirtualSpacesList: TScintCaretAndAnchorList); overload;
    function GetStyleAtPosition(const Pos: Integer): TScintStyleNumber;
    function GetTextRange(const StartPos, EndPos: Integer): String;
    function GetVisibleLineFromDocLine(const DocLine: Integer): Integer;
    function GetWordEndPosition(const Pos: Integer; const OnlyWordChars: Boolean): Integer;
    function GetWordStartPosition(const Pos: Integer; const OnlyWordChars: Boolean): Integer;
    function IsPositionInViewVertically(const Pos: Integer): Boolean;
    class function KeyCodeAndShiftToKeyDefinition(const KeyCode: TScintKeyCode;
      Shift: TShiftState): TScintKeyDefinition;
    function MainSelTextEquals(const S: String;
      const Options: TScintFindOptions): Boolean;
    class function KeyToKeyCode(const Key: AnsiChar): TScintKeyCode;
    procedure PasteFromClipboard;
    function RawMainSelTextEquals(const S: TScintRawString;
      const Options: TScintFindOptions): Boolean;
    class function RawStringIsBlank(const S: TScintRawString): Boolean;
    procedure Redo;
    procedure RemoveAdditionalSelections;
    function ReplaceMainSelText(const S: String;
      const ReplaceMode: TScintReplaceMode = srmNormal): TScintRange;
    function ReplaceRawMainSelText(const S: TScintRawString;
      const ReplaceMode: TScintReplaceMode = srmNormal): TScintRange;
    function ReplaceRawTextRange(const StartPos, EndPos: Integer;
      const S: TScintRawString; const ReplaceMode: TScintReplaceMode = srmNormal): TScintRange;
    function ReplaceTextRange(const StartPos, EndPos: Integer; const S: String;
      const ReplaceMode: TScintReplaceMode = srmNormal): TScintRange;
    procedure RestyleLine(const Line: Integer);
    procedure ScrollCaretIntoView;
    procedure SelectAll;
    procedure SelectAllOccurrences(const Options: TScintFindOptions);
    procedure SelectAndEnsureVisible(const Range: TScintRange);
    procedure SelectNextOccurrence(const Options: TScintFindOptions);
    function SelEmpty: Boolean;
    function SelNotEmpty(out Sel: TScintRange): Boolean;
    procedure SetAutoCompleteFillupChars(const FillupChars: AnsiString);
    procedure SetAutoCompleteSeparators(const Separator, TypeSeparator: AnsiChar);
    procedure SetAutoCompleteSelectedItem(const S: TScintRawString);
    procedure SetAutoCompleteStopChars(const StopChars: AnsiString);
    procedure SetBraceBadHighlighting(const Pos: Integer);
    procedure SetBraceHighlighting(const Pos1, Pos2: Integer);
    procedure SetCursorID(const CursorID: Integer);
    procedure SetCallTipHighlight(HighlightStart, HighlightEnd: Integer);
    procedure SetDefaultWordChars;
    procedure SetEmptySelection;
    procedure SetEmptySelections;
    procedure SetIndicators(const StartPos, EndPos: Integer;
      const IndicatorNumber: TScintIndicatorNumber; const Value: Boolean);
    procedure SetLineIndentation(const Line, Indentation: Integer);
    procedure SetSavePoint;
    procedure SetSingleSelection(const CaretPos, AnchorPos: Integer);
    procedure SettingChange(const Message: TMessage);
    procedure SetWordChars(const S: AnsiString);
    procedure ShowAutoComplete(const CharsEntered: Integer; const WordList: AnsiString);
    procedure ShowCallTip(const Pos: Integer; const Definition: AnsiString);
    procedure StyleNeeded(const EndPos: Integer);
    procedure SysColorChange(const Message: TMessage);
    function TestRegularExpression(const S: String): Boolean;
    function TestRawRegularExpression(const S: TScintRawString): Boolean;
    procedure Undo;
    procedure UpdateStyleAttributes;
    function WordAtCaret: String;
    function WordAtCaretRange: TScintRange;
    procedure ZoomIn;
    procedure ZoomOut;
    property AutoCompleteActive: Boolean read GetAutoCompleteActive;
    property CallTipActive: Boolean read GetCallTipActive;
    property CaretColumn: Integer read GetCaretColumn write SetCaretColumn;
    property CaretColumnExpandedForTabs: Integer read GetCaretColumnExpandedForTabs;
    property CaretLine: Integer read GetCaretLine write SetCaretLine;
    property CaretLineText: String read GetCaretLineText;
    property CaretPosition: Integer read GetCaretPosition write SetCaretPosition;
    property CaretPositionInLine: Integer read GetCaretPositionInLine;
    property CaretPositionWithSelectFromAnchor: Integer write SetCaretPositionWithSelectFromAnchor;
    property CaretVirtualSpace: Integer read GetCaretVirtualSpace write SetCaretVirtualSpace;
    property EffectiveCodePage: Integer read FEffectiveCodePage;
    property FoldFlags: TScintFoldFlags write SetFoldFlags;
    property InsertMode: Boolean read GetInsertMode;
    property LineEndings: TScintLineEndings read GetLineEndings;
    property LineEndingString: TScintRawString read GetLineEndingString;
    property LineHeight: Integer read GetLineHeight;
    property Lines: TScintEditStrings read FLines;
    property LinesInWindow: Integer read GetLinesInWindow;
    property MainSelection: Integer read GetMainSelection write SetMainSelection;
    property MainSelText: String read GetMainSelText write SetMainSelText;
    property Modified: Boolean read GetModified;
    property RawCaretLineText: TScintRawString read GetRawCaretLineText;
    property RawMainSelText: TScintRawString read GetRawMainSelText write SetRawMainSelText;
    property RawSelText: TScintRawString read GetRawSelText write SetRawSelText;
    property RawText: TScintRawString read GetRawText write SetRawText;
    property RawTextLength: Integer read GetRawTextLength;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Selection: TScintRange read GetSelection write SetSelection;
    property SelectionAnchorPosition[Selection: Integer]: Integer read GetSelectionAnchorPosition write SetSelectionAnchorPosition;
    property SelectionAnchorVirtualSpace[Selection: Integer]: Integer read GetSelectionAnchorVirtualSpace write SetSelectionAnchorVirtualSpace;
    property SelectionCaretPosition[Selection: Integer]: Integer read GetSelectionCaretPosition write SetSelectionCaretPosition;
    property SelectionCaretVirtualSpace[Selection: Integer]: Integer read GetSelectionCaretVirtualSpace write SetSelectionCaretVirtualSpace;
    property SelectionCount: Integer read GetSelectionCount;
    property SelectionEndPosition[Selection: Integer]: Integer read GetSelectionEndPosition;
    property SelectionMode: TScintSelectionMode read GetSelectionMode write SetSelectionMode;
    property SelectionStartPosition[Selection: Integer]: Integer read GetSelectionStartPosition;
    property SelText: String read GetSelText write SetSelText;
    property Styler: TScintCustomStyler read FStyler write SetStyler;
    property Target: TScintRange read GetTarget;
    property TopLine: Integer read GetTopLine write SetTopLine;
    property WordChars: AnsiString read FWordChars;
    property WordCharsAsSet: TSysCharSet read FWordCharsAsSet;
  published
    property AcceptDroppedFiles: Boolean read FAcceptDroppedFiles write SetAcceptDroppedFiles
      default False;
    property AutoCompleteFontName: String read FAutoCompleteFontName
      write SetAutoCompleteFontName;
    property AutoCompleteFontSize: Integer read FAutoCompleteFontSize
      write SetAutoCompleteFontSize default 0;
    property ChangeHistory: TScintChangeHistory read FChangeHistory write SetChangeHistory default schDisabled;
    property CodePage: Integer read FCodePage write SetCodePage default CP_UTF8;
    property Color;
    property FillSelectionToEdge: Boolean read FFillSelectionToEdge write SetFillSelectionToEdge
      default False;
    property Font;
    property IndentationGuides: TScintIndentationGuides read FIndentationGuides
      write SetIndentationGuides default sigNone;
    property LineNumbers: Boolean read FLineNumbers write SetLineNumbers default False;
    property ParentFont;
    property PopupMenu;
    property ReportCaretPositionToStyler: Boolean read FReportCaretPositionToStyler
      write FReportCaretPositionToStyler;
    property TabStop default True;
    property TabWidth: Integer read FTabWidth write SetTabWidth default 8;
    property UseStyleAttributes: Boolean read FUseStyleAttributes write SetUseStyleAttributes
      default True;
    property UseTabCharacter: Boolean read FUseTabCharacter write SetUseTabCharacter
      default True;
    property VirtualSpaceOptions: TScintVirtualSpaceOptions read FVirtualSpaceOptions
      write SetVirtualSpaceOptions default [];
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property Zoom: Integer read GetZoom write SetZoom default 0;
    property OnAutoCompleteSelection: TScintEditAutoCompleteSelectionEvent read FOnAutoCompleteSelection write FOnAutoCompleteSelection;
    property OnCallTipArrowClick: TScintEditCallTipArrowClick read FOnCallTipArrowClick write FOnCallTipArrowClick;
    property OnChange: TScintEditChangeEvent read FOnChange write FOnChange;
    property OnCharAdded: TScintEditCharAddedEvent read FOnCharAdded write FOnCharAdded;
    property OnDropFiles: TScintEditDropFilesEvent read FOnDropFiles write FOnDropFiles;
    property OnHintShow: TScintEditHintShowEvent read FOnHintShow write FOnHintShow;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMarginClick: TScintEditMarginClickEvent read FOnMarginClick write FOnMarginClick;
    property OnMarginRightClick: TScintEditMarginClickEvent read FOnMarginRightClick write FOnMarginRightClick;
    property OnModifiedChange: TNotifyEvent read FOnModifiedChange write FOnModifiedChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnUpdateUI: TScintEditUpdateUIEvent read FOnUpdateUI write FOnUpdateUI;
    property OnZoom: TNotifyEvent read FOnZoom write FOnZoom;
  end;

  TScintEditStrings = class(TStrings)
  private
    FEdit: TScintEdit;
    function GetLineEndingLength(const Index: Integer): Integer;
    function GetRawLine(Index: Integer): TScintRawString;
    function GetRawLineWithEnding(Index: Integer): TScintRawString;
    function GetRawLineLength(Index: Integer): Integer;
    function GetRawLineLengthWithEnding(Index: Integer): Integer;
    function GetState(Index: Integer): TScintLineState;
    procedure PutRawLine(Index: Integer; const S: TScintRawString);
  protected
    procedure CheckIndexRange(const Index: Integer);
    procedure CheckIndexRangePlusOne(const Index: Integer);
    function Get(Index: Integer): String; override;
    function GetCount: Integer; override;
    function GetTextStr: String; override;
    procedure Put(Index: Integer; const S: String); override;
    procedure SetTextStr(const Value: String); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: String); override;
    procedure InsertRawLine(Index: Integer; const S: TScintRawString);
    procedure SetText(Text: PChar); override;
    property RawLineLengths[Index: Integer]: Integer read GetRawLineLength;
    property RawLineLengthsWithEnding[Index: Integer]: Integer read GetRawLineLengthWithEnding;
    property RawLines[Index: Integer]: TScintRawString read GetRawLine write PutRawLine;
    property RawLinesWithEnding[Index: Integer]: TScintRawString read GetRawLineWithEnding;
    property State[Index: Integer]: TScintLineState read GetState;
  end;

  TScintStyleAttributes = record
    FontName: String;
    FontSize: Integer;
    FontStyle: TFontStyles;
    FontCharset: TFontCharset;
    ForeColor: TColor;
    BackColor: TColor;
  end;

  TScintCustomStyler = class(TComponent)
  private
    FCaretIndex: Integer;
    FCurIndex: Integer;
    FLineState: TScintLineState;
    FStyleStartIndex: Integer;
    FStyleStr: AnsiString;
    FText: TScintRawString;
    FTextLen: Integer;
    function GetCurChar: AnsiChar;
    function GetEndOfLine: Boolean;
  protected
    procedure ApplyStyleByteIndicators(const Indicators: TScintStyleByteIndicatorNumbers;
      StartIndex, EndIndex: Integer);
    procedure ApplyStyle(const Style: TScintStyleNumber;
      StartIndex, EndIndex: Integer);
    procedure CommitStyle(const Style: TScintStyleNumber);
    function ConsumeAllRemaining: Boolean;
    function ConsumeChar(const C: AnsiChar): Boolean;
    function ConsumeCharIn(const Chars: TScintRawCharSet): Boolean;
    function ConsumeChars(const Chars: TScintRawCharSet): Boolean;
    function ConsumeCharsNot(const Chars: TScintRawCharSet): Boolean;
    function ConsumeString(const Chars: TScintRawCharSet): TScintRawString;
    function CurCharIn(const Chars: TScintRawCharSet): Boolean;
    function CurCharIs(const C: AnsiChar): Boolean;
    procedure GetFoldLevel(const LineState, PreviousLineState: TScintLineState;
      var Level: Integer; var Header, EnableHeaderOnPrevious: Boolean); virtual; abstract;
    procedure GetStyleAttributes(const Style: Integer;
      var Attributes: TScintStyleAttributes); virtual; abstract;
    function LineTextSpans(const S: TScintRawString): Boolean; virtual;
    function NextCharIs(const C: AnsiChar): Boolean;
    function PreviousCharIn(const Chars: TScintRawCharSet): Boolean;
    procedure ResetCurIndexTo(Index: Integer);
    procedure ReplaceText(StartIndex, EndIndex: Integer; const C: AnsiChar);
    procedure StyleNeeded; virtual; abstract;
    property CaretIndex: Integer read FCaretIndex;
    property CurChar: AnsiChar read GetCurChar;
    property CurIndex: Integer read FCurIndex;
    property EndOfLine: Boolean read GetEndOfLine;
    property LineState: TScintLineState read FLineState write FLineState;
    property StyleStartIndex: Integer read FStyleStartIndex;
    property Text: TScintRawString read FText;
    property TextLength: Integer read FTextLen;
  end;

  TScintPixmap = class
  private
    type
      TPixmap = array of AnsiString;
    class var
      ColorCodes: String;
    var
      FPixmap: TPixmap;
    class constructor Create;
    function GetPixmap: Pointer;
  public
    procedure InitializeFromBitmap(const ABitmap: TBitmap; const TransparentColor: TColorRef);
    property Pixmap: Pointer read GetPixmap;
  end;

implementation

uses
  ShellAPI, RTLConsts, UITypes, GraphUtil;

{ TScintEdit }

const
  AUTOCSETSEPARATOR = #9;

constructor TScintEdit.Create(AOwner: TComponent);
begin
  inherited;
  FCodePage := CP_UTF8;
  FLines := TScintEditStrings.Create;
  FLines.FEdit := Self;
  FTabWidth := 8;
  FUseStyleAttributes := True;
  FUseTabCharacter := True;
  SetBounds(0, 0, 257, 193);
  ParentColor := False;
  TabStop := True;
end;

destructor TScintEdit.Destroy;
begin
  SetStyler(nil);
  FLines.Free;
  FLines := nil;
  inherited;
end;

procedure TScintEdit.AddMarker(const Line: Integer;
  const Marker: TScintMarkerNumber);
begin
  FLines.CheckIndexRange(Line);
  Call(SCI_MARKERADD, Line, Marker);
end;

procedure TScintEdit.AddSelection(const CaretPos, AnchorPos: Integer);
{ Adds a new selection as the main selection retaining all other selections as
  additional selections without scrolling the caret into view. The first
  selection should be added with SetSingleSelection. }
begin
  Call(SCI_ADDSELECTION, CaretPos, AnchorPos);
end;

procedure TScintEdit.ApplyOptions;
const
  IndentationGuides: array [TScintIndentationGuides] of Integer = (SC_IV_NONE, SC_IV_REAL,
    SC_IV_LOOKFORWARD, SC_IV_LOOKBOTH);
var
  Flags: Integer;
begin
  if not HandleAllocated then
    Exit;
  Call(SCI_SETSELEOLFILLED, Ord(FFillSelectionToEdge), 0);
  Call(SCI_SETTABWIDTH, FTabWidth, 0);
  Call(SCI_SETUSETABS, Ord(FUseTabCharacter), 0);
  Flags := 0;
  if svsRectangularSelection in VirtualSpaceOptions then
    Flags := Flags or SCVS_RECTANGULARSELECTION;
  if svsUserAccessible in VirtualSpaceOptions then
    Flags := Flags or SCVS_USERACCESSIBLE;
  if svsNoWrapLineStart in VirtualSpaceOptions then
    Flags := Flags or SCVS_NOWRAPLINESTART;
  Call(SCI_SETVIRTUALSPACEOPTIONS, Flags, 0);
  Call(SCI_SETWRAPMODE, Ord(FWordWrap), 0);
  Call(SCI_SETINDENTATIONGUIDES, IndentationGuides[FIndentationGuides], 0);
  { If FChangeHistory is not schDisabled then next call to ClearUndo will enable
    change history and else we should disable it now }
  if FChangeHistory = schDisabled then
    Call(SCI_SETCHANGEHISTORY, SC_CHANGE_HISTORY_DISABLED, 0);
end;

procedure TScintEdit.AssignCmdKey(const Key: AnsiChar; const Shift: TShiftState;
  const Command: TScintCommand);
begin
  AssignCmdKey(KeyToKeyCode(Key), Shift, Command);
end;

procedure TScintEdit.AssignCmdKey(const KeyCode: TScintKeyCode;
  const Shift: TShiftState; const Command: TScintCommand);
begin
  Call(SCI_ASSIGNCMDKEY, KeyCodeAndShiftToKeyDefinition(KeyCode, Shift), Command);
end;

procedure TScintEdit.BeginUndoAction;
begin
  Call(SCI_BEGINUNDOACTION, 0, 0);
end;

function TScintEdit.Call(Msg: Cardinal; WParam: Longint; LParam: Longint): Longint;
begin
  var Dummy: Integer;
  Result := Call(Msg, WParam, LParam, Dummy);
end;

function TScintEdit.Call(Msg: Cardinal; WParam: Longint; LParam: Longint;
  out WarnStatus: Integer): Longint;
begin
  HandleNeeded;
  if FDirectPtr = nil then
    Error('Call: FDirectPtr is nil');
  if not Assigned(FDirectStatusFunction) then
    Error('Call: FDirectStatusFunction is nil');
  var ErrorStatus: Integer;
  Result := FDirectStatusFunction(FDirectPtr, Msg, WParam, LParam, ErrorStatus);

  if ErrorStatus <> 0 then begin
    var Dummy: Integer;
    FDirectStatusFunction(FDirectPtr, SCI_SETSTATUS, 0, 0, Dummy);
    if ErrorStatus < SC_STATUS_WARN_START then
      ErrorFmt('Error status %d returned after Call(%u, %d, %d) = %d',
        [ErrorStatus, Msg, WParam, LParam, Result]);
  end;

  WarnStatus := ErrorStatus;
end;

function TScintEdit.Call(Msg: Cardinal; WParam: Longint;
  const LParamStr: TScintRawString): Longint;
begin
  var Dummy: Integer;
  Result := Call(Msg, WParam, LParamStr, Dummy);
end;

function TScintEdit.Call(Msg: Cardinal; WParam: Longint;
  const LParamStr: TScintRawString; out WarnStatus: Integer): Longint;
begin
  Result := Call(Msg, WParam, LPARAM(PAnsiChar(LParamStr)), WarnStatus);
end;

procedure TScintEdit.CancelAutoComplete;
begin
  Call(SCI_AUTOCCANCEL, 0, 0);
end;

procedure TScintEdit.CancelAutoCompleteAndCallTip;
begin
  CancelAutoComplete;
  CancelCallTip;
end;

procedure TScintEdit.CancelCallTip;
begin
  Call(SCI_CALLTIPCANCEL, 0, 0);
end;

function TScintEdit.CanPaste: Boolean;
begin
  Result := Call(SCI_CANPASTE, 0, 0) <> 0;
end;

function TScintEdit.CanRedo: Boolean;
begin
  Result := Call(SCI_CANREDO, 0, 0) <> 0;
end;

function TScintEdit.CanUndo: Boolean;
begin
  Result := Call(SCI_CANUNDO, 0, 0) <> 0;
end;

procedure TScintEdit.Change(const AInserting: Boolean;
  const AStartPos, ALength, ALinesDelta: Integer);
var
  Info: TScintEditChangeInfo;
begin
  inherited Changed;
  if Assigned(FOnChange) then begin
    Info.Inserting := AInserting;
    Info.StartPos := AStartPos;
    Info.Length := ALength;
    Info.LinesDelta := ALinesDelta;
    FOnChange(Self, Info);
  end;
end;

procedure TScintEdit.CheckPosRange(const StartPos, EndPos: Integer);
begin
  if (StartPos < 0) or (StartPos > EndPos) or (EndPos > GetRawTextLength) then
    ErrorFmt('CheckPosRange: Invalid range (%d, %d)', [StartPos, EndPos]);
end;

procedure TScintEdit.ChooseCaretX;
begin
  Call(SCI_CHOOSECARETX, 0, 0);
end;

procedure TScintEdit.ClearAll;
begin
  Call(SCI_CLEARALL, 0, 0);
  ChooseCaretX;
end;

procedure TScintEdit.ClearCmdKey(const Key: AnsiChar; const Shift: TShiftState);
begin
  ClearCmdKey(KeyToKeyCode(Key), Shift);
end;

procedure TScintEdit.ClearCmdKey(const KeyCode: TScintKeyCode; const Shift: TShiftState);
begin
  Call(SCI_CLEARCMDKEY, KeyCodeAndShiftToKeyDefinition(KeyCode, Shift), 0);
end;

procedure TScintEdit.ClearIndicators(
  const IndicatorNumber: TScintIndicatorNumber);
begin
  Call(SCI_SETINDICATORCURRENT, IndicatorNumber, 0);
  Call(SCI_INDICATORCLEARRANGE, 0, RawTextLength);
end;

procedure TScintEdit.ClearSelection;
begin
  Call(SCI_CLEAR, 0, 0);
end;

procedure TScintEdit.ClearUndo(const ClearChangeHistory: Boolean);
begin
  { SCI_EMPTYUNDOBUFFER resets the save point but doesn't send a
    SCN_SAVEPOINTREACHED notification. Call SetSavePoint manually to get
    that. SetSavePoint additionally resets FForceModified. }
  SetSavePoint;
  Call(SCI_EMPTYUNDOBUFFER, 0, 0);

  if ClearChangeHistory and (FChangeHistory <> schDisabled) then begin
    Call(SCI_SETCHANGEHISTORY, SC_CHANGE_HISTORY_DISABLED, 0);
    var Flags := SC_CHANGE_HISTORY_ENABLED;
    if FChangeHistory = schMarkers then
      Flags := Flags or SC_CHANGE_HISTORY_MARKERS
    else
      Flags := Flags or SC_CHANGE_HISTORY_INDICATORS;
    Call(SCI_SETCHANGEHISTORY, Flags, 0);
  end;
end;

function TScintEdit.ConvertRawStringToString(const S: TScintRawString): String;
var
  SrcLen, DestLen: Integer;
  DestStr: UnicodeString;
begin
  SrcLen := Length(S);
  if SrcLen > 0 then begin
    DestLen := MultiByteToWideChar(FCodePage, 0, PAnsiChar(S), SrcLen, nil, 0);
    if DestLen <= 0 then
      Error('MultiByteToWideChar failed');
    SetString(DestStr, nil, DestLen);
    if MultiByteToWideChar(FCodePage, 0, PAnsiChar(S), SrcLen, @DestStr[1],
       Length(DestStr)) <> DestLen then
      Error('Unexpected result from MultiByteToWideChar');
  end;
  Result := DestStr;
end;

function TScintEdit.ConvertPCharToRawString(const Text: PChar;
  const TextLen: Integer): TScintRawString;
var
  DestLen: Integer;
  DestStr: TScintRawString;
begin
  if TextLen > 0 then begin
    DestLen := WideCharToMultiByte(FCodePage, 0, Text, TextLen, nil, 0, nil, nil);
    if DestLen <= 0 then
      Error('WideCharToMultiByte failed');
    InitRawString(DestStr, DestLen);
    if WideCharToMultiByte(FCodePage, 0, Text, TextLen, @DestStr[1], Length(DestStr),
       nil, nil) <> DestLen then
      Error('Unexpected result from WideCharToMultiByte');
  end;
  Result := DestStr;
end;

function TScintEdit.ConvertStringToRawString(const S: String): TScintRawString;
begin
  Result := ConvertPCharToRawString(PChar(S), Length(S));
end;

procedure TScintEdit.CopyToClipboard;
begin
  Call(SCI_COPY, 0, 0);
end;

procedure TScintEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  CreateSubClass(Params, 'Scintilla');
  //Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  Params.WindowClass.style := Params.WindowClass.style and
    not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TScintEdit.CreateWnd;
begin
  if IsscintLibary = 0 then
    Error('CreateWnd: IsscintLibary is 0');
  inherited;
  FDirectPtr := Pointer(SendMessage(Handle, SCI_GETDIRECTPOINTER, 0, 0));
  if FDirectPtr = nil then
    Error('CreateWnd: FDirectPtr is nil');
  FDirectStatusFunction := SciFnDirectStatus(SendMessage(Handle, SCI_GETDIRECTSTATUSFUNCTION, 0, 0));
  if not Assigned(FDirectStatusFunction) then
    Error('CreateWnd: FDirectStatusFunction is nil');
  UpdateCodePage;
  Call(SCI_SETCOMMANDEVENTS, 0, 0);
  Call(SCI_SETMODEVENTMASK, SC_MOD_INSERTTEXT or SC_MOD_DELETETEXT, 0);
  Call(SCI_SETCARETPERIOD, GetCaretBlinkTime, 0);
  Call(SCI_SETSCROLLWIDTHTRACKING, 1, 0);
  { The default popup menu conflicts with the VCL's PopupMenu }
  Call(SCI_USEPOPUP, 0, 0);
  SetDefaultWordChars;
  ApplyOptions;
  UpdateStyleAttributes;
  if FAcceptDroppedFiles then
    DragAcceptFiles(Handle, True);
end;

procedure TScintEdit.CutToClipboard;
begin
  Call(SCI_CUT, 0, 0);
end;

procedure TScintEdit.DeleteAllMarkersOnLine(const Line: Integer);
begin
  FLines.CheckIndexRange(Line);
  Call(SCI_MARKERDELETE, Line, -1);
end;

procedure TScintEdit.DeleteMarker(const Line: Integer;
  const Marker: TScintMarkerNumber);
begin
  FLines.CheckIndexRange(Line);
  Call(SCI_MARKERDELETE, Line, Marker);
end;

procedure TScintEdit.EndUndoAction;
begin
  Call(SCI_ENDUNDOACTION, 0, 0);
end;

procedure TScintEdit.EnsureLineVisible(const Line: Integer);
begin
  FLines.CheckIndexRange(Line);
  Call(SCI_ENSUREVISIBLE, Line, 0);
end;

class function TScintEdit.GetErrorException(const S: String): EScintEditError;
 { Can be used when just calling Error would cause a compiler warning because it doesn't realize Error always raises }
begin
  Result := EScintEditError.Create('TScintEdit error: ' + S);
end;

class procedure TScintEdit.Error(const S: String);
begin
  raise GetErrorException(S);
end;

class procedure TScintEdit.ErrorFmt(const S: String; const Args: array of const);
begin
  Error(Format(S, Args));
end;

function TScintEdit.FindRawText(const StartPos, EndPos: Integer;
  const S: TScintRawString; const Options: TScintFindOptions;
  out MatchRange: TScintRange): Boolean;
begin
  SetTarget(StartPos, EndPos);
  Call(SCI_SETSEARCHFLAGS, GetSearchFlags(Options), 0);
  Result := Call(SCI_SEARCHINTARGET, Length(S), S) >= 0;
  if Result then
    MatchRange := GetTarget;
end;

function TScintEdit.FindText(const StartPos, EndPos: Integer; const S: String;
  const Options: TScintFindOptions; out MatchRange: TScintRange): Boolean;
begin
  Result := FindRawText(StartPos, EndPos, ConvertStringToRawString(S),
    Options, MatchRange);
end;

procedure TScintEdit.FoldLine(const Line: Integer; const Fold: Boolean);
begin
  FLines.CheckIndexRange(Line);
  { If the line is not part of a fold the following will return False }
  var Folded := Call(SCI_GETFOLDEXPANDED, Line, 0) = 0;
  if Fold <> Folded then begin
    { If the line is not part of a fold the following will do nothing
      and else if the line is not the header Scintilla will lookup the
      header for us }
    Call(SCI_TOGGLEFOLD, Line, 0);
  end;
end;

procedure TScintEdit.ForceModifiedState;
begin
  if not FForceModified then begin
    FForceModified := True;
    if Assigned(FOnModifiedChange) then
      FOnModifiedChange(Self);
  end;
end;

function TScintEdit.FormatRange(const Draw: Boolean;
  const RangeToFormat: PScintRangeToFormat): Integer;
begin
  Result := Call(SCI_FORMATRANGE, Ord(Draw), LPARAM(RangeToFormat));
end;

procedure TScintEdit.ForwardMessage(const Message: TMessage);
begin
  if HandleAllocated then
    CallWindowProc(DefWndProc, Handle, Message.Msg, Message.WParam, Message.LParam);
end;

function TScintEdit.GetAutoCompleteActive: Boolean;
begin
  Result := Call(SCI_AUTOCACTIVE, 0, 0) <> 0;
end;

function TScintEdit.GetByteAtPosition(const Pos: Integer): AnsiChar;
begin
  Result := AnsiChar(Call(SCI_GETCHARAT, Pos, 0));
end;

function TScintEdit.GetCallTipActive: Boolean;
begin
  Result := Call(SCI_CALLTIPACTIVE, 0, 0) <> 0;
end;

function TScintEdit.GetCaretColumn: Integer;
begin
  Result := GetColumnFromPosition(GetCaretPosition);
end;

function TScintEdit.GetCaretColumnExpandedForTabs: Integer;
begin
  Result := Call(SCI_GETCOLUMN, GetCaretPosition, 0);
  Inc(Result, GetCaretVirtualSpace);
end;

function TScintEdit.GetCaretLine: Integer;
begin
  Result := GetLineFromPosition(GetCaretPosition);
end;

function TScintEdit.GetCaretLineText: String;
begin
  Result := ConvertRawStringToString(GetRawCaretLineText);
end;

function TScintEdit.GetCaretPosition: Integer;
begin
  Result := Call(SCI_GETCURRENTPOS, 0, 0);
end;

function TScintEdit.GetCaretPositionInLine: Integer;
begin
  var Caret := CaretPosition;
  var LineStart := GetPositionFromLine(GetLineFromPosition(Caret));
  Result := Caret - LineStart;
end;

function TScintEdit.GetCaretVirtualSpace: Integer;
begin
  Result := GetSelectionCaretVirtualSpace(GetMainSelection);
end;

function TScintEdit.GetCharacterCount(const StartPos, EndPos: Integer): Integer;
begin
  CheckPosRange(StartPos, EndPos);
  Result := Call(SCI_COUNTCHARACTERS, StartPos, EndPos);
end;

function TScintEdit.GetColumnFromPosition(const Pos: Integer): Integer;
var
  Line: Integer;
begin
  Line := GetLineFromPosition(Pos);
  Result := Pos - GetPositionFromLine(Line);
end;

function TScintEdit.GetDefaultWordChars: AnsiString;
begin
  Result := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_';
end;

function TScintEdit.GetDocLineFromVisibleLine(const VisibleLine: Integer): Integer;
begin
  Result := Call(SCI_DOCLINEFROMVISIBLE, VisibleLine, 0);
end;

function TScintEdit.GetIndicatorAtPosition(
  const IndicatorNumber: TScintIndicatorNumber; const Pos: Integer): Boolean;
begin
  Result := Call(SCI_INDICATORVALUEAT, IndicatorNumber, Pos) <> 0;
end;

function TScintEdit.GetInsertMode: Boolean;
begin
  Result := Call(SCI_GETOVERTYPE, 0, 0) = 0;
end;

function TScintEdit.GetLineEndings: TScintLineEndings;
begin
  case Call(SCI_GETEOLMODE, 0, 0) of
    SC_EOL_CR: Result := sleCR;
    SC_EOL_LF: Result := sleLF;
    SC_EOL_CRLF: Result := sleCRLF;
  else
    raise GetErrorException('Unexpected SCI_GETEOLMODE result');
  end;
end;

function TScintEdit.GetLineEndingString: TScintRawString;
const
  EndingStrs: array[TScintLineEndings] of TScintRawString =
    (#13#10, #13, #10);
begin
  Result := EndingStrs[LineEndings];
end;

function TScintEdit.GetLineEndPosition(const Line: Integer): Integer;
{ Returns the position at the end of the line, before any line end characters. }
begin
  FLines.CheckIndexRange(Line);
  Result := Call(SCI_GETLINEENDPOSITION, Line, 0);
end;

function TScintEdit.GetLineFromPosition(const Pos: Integer): Integer;
begin
  Result := Call(SCI_LINEFROMPOSITION, Pos, 0);
end;

function TScintEdit.GetLineHeight: Integer;
begin
  Result := Call(SCI_TEXTHEIGHT, 0, 0);
end;

function TScintEdit.GetLineIndentation(const Line: Integer): Integer;
begin
  FLines.CheckIndexRange(Line);
  Result := Call(SCI_GETLINEINDENTATION, Line, 0);
end;

function TScintEdit.GetLineIndentPosition(const Line: Integer): Integer;
begin
  FLines.CheckIndexRange(Line);
  Result := Call(SCI_GETLINEINDENTPOSITION, Line, 0);
end;

function TScintEdit.GetLinesInWindow: Integer;
begin
  Result := Call(SCI_LINESONSCREEN, 0, 0);
end;

function TScintEdit.GetMainSelection: Integer;
begin
  Result := Call(SCI_GETMAINSELECTION, 0, 0);
end;

function TScintEdit.GetMainSelText: String;
begin
  Result := ConvertRawStringToString(GetRawMainSelText);
end;

function TScintEdit.GetMarkers(const Line: Integer): TScintMarkerNumbers;
begin
  FLines.CheckIndexRange(Line);
  Integer(Result) := Call(SCI_MARKERGET, Line, 0);
end;

function TScintEdit.GetModified: Boolean;
begin
  Result := FForceModified or (Call(SCI_GETMODIFY, 0, 0) <> 0);
end;

function TScintEdit.GetPointFromPosition(const Pos: Integer): TPoint;
begin
  Result.X := Call(SCI_POINTXFROMPOSITION, 0, Pos);
  Result.Y := Call(SCI_POINTYFROMPOSITION, 0, Pos);
end;

function TScintEdit.GetPositionAfter(const Pos: Integer): Integer;
begin
  Result := Call(SCI_POSITIONAFTER, Pos, 0);
end;

function TScintEdit.GetPositionBefore(const Pos: Integer): Integer;
begin
  Result := Call(SCI_POSITIONBEFORE, Pos, 0);
end;

function TScintEdit.GetPositionFromLine(const Line: Integer): Integer;
begin
  FLines.CheckIndexRangePlusOne(Line);
  Result := Call(SCI_POSITIONFROMLINE, Line, 0);
end;

function TScintEdit.GetPositionFromLineColumn(const Line, Column: Integer): Integer;
var
  Col, Len: Integer;
begin
  Col := Column;
  Result := GetPositionFromLine(Line);
  Len := GetLineEndPosition(Line) - Result;
  if Col > Len then
    Col := Len;
  if Col > 0 then
    Inc(Result, Col);
end;

function TScintEdit.GetPositionFromLineExpandedColumn(const Line,
  ExpandedColumn: Integer): Integer;
begin
  FLines.CheckIndexRange(Line);
  Result := Call(SCI_FINDCOLUMN, Line, ExpandedColumn);
end;

function TScintEdit.GetPositionFromPoint(const P: TPoint;
  const CharPosition, CloseOnly: Boolean): Integer;
begin
  if CharPosition then begin
    if CloseOnly then
      Result := Call(SCI_CHARPOSITIONFROMPOINTCLOSE, P.X, P.Y)
    else
      Result := Call(SCI_CHARPOSITIONFROMPOINT, P.X, P.Y);
  end
  else begin
    if CloseOnly then
      Result := Call(SCI_POSITIONFROMPOINTCLOSE, P.X, P.Y)
    else
      Result := Call(SCI_POSITIONFROMPOINT, P.X, P.Y);
  end;
end;

function TScintEdit.GetPositionOfMatchingBrace(const Pos: Integer): Integer;
begin
  Result := Call(SCI_BRACEMATCH, Pos, 0);
end;

function TScintEdit.GetPositionRelative(const Pos,
  CharacterCount: Integer): Integer;
begin
  Result := Call(SCI_POSITIONRELATIVE, Pos, CharacterCount);
end;

function TScintEdit.GetRawCaretLineText: TScintRawString;
begin
  var Line := CaretLine;
  Result := GetRawTextRange(GetPositionFromLine(Line), GetPositionFromLine(Line+1));
end;

function TScintEdit.GetRawMainSelText: TScintRawString;
begin
  var MainSel := MainSelection;
  var CaretPos := SelectionCaretPosition[MainSel];
  var AnchorPos := SelectionAnchorPosition[MainSel];
  if AnchorPos < CaretPos then
    Result := GetRawTextRange(AnchorPos, CaretPos)
  else
    Result := GetRawTextRange(CaretPos, AnchorPos);
end;

function TScintEdit.GetRawSelText: TScintRawString;
{ Gets the combined text of *all* selections }
var
  Len: Integer;
  S: TScintRawString;
begin
  Len := Call(SCI_GETSELTEXT, 0, 0);
  if Len > 0 then begin
    InitRawString(S, Len);
    Call(SCI_GETSELTEXT, 0, LPARAM(PAnsiChar(@S[1])));
  end;
  Result := S;
end;

function TScintEdit.GetRawText: TScintRawString;
begin
  Result := GetRawTextRange(0, GetRawTextLength);
end;

function TScintEdit.GetRawTextLength: Integer;
begin
  Result := Call(SCI_GETLENGTH, 0, 0);
end;

function TScintEdit.GetRawTextRange(const StartPos, EndPos: Integer): TScintRawString;
var
  S: TScintRawString;
  Range: TSci_TextRange;
begin
  CheckPosRange(StartPos, EndPos);
  if EndPos > StartPos then begin
    InitRawString(S, EndPos - StartPos);
    Range.chrg.cpMin := StartPos;
    Range.chrg.cpMax := EndPos;
    Range.lpstrText := @S[1];
    if Call(SCI_GETTEXTRANGE, 0, LPARAM(@Range)) <> EndPos - StartPos then
      Error('Unexpected result from SCI_GETTEXTRANGE');
  end;
  Result := S;
end;

function TScintEdit.GetReadOnly: Boolean;
begin
  Result := Call(SCI_GETREADONLY, 0, 0) <> 0;
end;

class function TScintEdit.GetReplaceTargetMessage(
  const ReplaceMode: TScintReplaceMode): Cardinal;
begin
  case ReplaceMode of
    srmNormal: Result := SCI_REPLACETARGET;
    srmMinimal: Result := SCI_REPLACETARGETMINIMAL;
    srmRegEx: Result := SCI_REPLACETARGETRE;
  else
    raise GetErrorException('Unknown ReplaceMode');
  end;
end;

class function TScintEdit.GetSearchFlags(const Options: TScintFindOptions): Integer;
begin
  Result := 0;
  if sfoMatchCase in Options then
    Result := Result or SCFIND_MATCHCASE;
  if sfoWholeWord in Options then
    Result := Result or SCFIND_WHOLEWORD;
  if sfoRegEx in Options then
    Result := Result or (SCFIND_REGEXP or SCFIND_CXX11REGEX);
end;

function TScintEdit.GetSelection: TScintRange;
begin
  Result.StartPos := Call(SCI_GETSELECTIONSTART, 0, 0);
  Result.EndPos := Call(SCI_GETSELECTIONEND, 0, 0);
end;

procedure TScintEdit.GetSelections(const RangeList: TScintRangeList);
begin
  RangeList.Clear;
  for var I := 0 to SelectionCount-1 do begin
    var StartPos := GetSelectionStartPosition(I);
    var EndPos := GetSelectionEndPosition(I);
    RangeList.Add(TScintRange.Create(StartPos, EndPos));
  end;
end;

procedure TScintEdit.GetSelections(const CaretAndAnchorList: TScintCaretAndAnchorList);
begin
  CaretAndAnchorList.Clear;
  for var I := 0 to SelectionCount-1 do begin
    var CaretPos := GetSelectionCaretPosition(I);
    var AnchorPos := GetSelectionAnchorPosition(I);
    CaretAndAnchorList.Add(TScintCaretAndAnchor.Create(CaretPos, AnchorPos));
  end;
end;

procedure TScintEdit.GetSelections(const CaretAndAnchorList, VirtualSpacesList: TScintCaretAndAnchorList);
begin
  GetSelections(CaretAndAnchorList);
  VirtualSpacesList.Clear;
  for var I := 0 to SelectionCount-1 do begin
    var CaretPos := GetSelectionCaretVirtualSpace(I);
    var AnchorPos := GetSelectionAnchorVirtualSpace(I);
    VirtualSpacesList.Add(TScintCaretAndAnchor.Create(CaretPos, AnchorPos));
  end;
end;

function TScintEdit.GetSelectionAnchorPosition(Selection: Integer): Integer;
begin
  Result := Call(SCI_GETSELECTIONNANCHOR, Selection, 0);
end;

function TScintEdit.GetSelectionAnchorVirtualSpace(Selection: Integer): Integer;
begin
  Result := Call(SCI_GETSELECTIONNANCHORVIRTUALSPACE, Selection, 0);
end;

function TScintEdit.GetSelectionCaretPosition(Selection: Integer): Integer;
begin
  Result := Call(SCI_GETSELECTIONNCARET, Selection, 0);
end;

function TScintEdit.GetSelectionCaretVirtualSpace(Selection: Integer): Integer;
begin
  Result := Call(SCI_GETSELECTIONNCARETVIRTUALSPACE, Selection, 0);
end;

function TScintEdit.GetSelectionCount: Integer;
{ Returns the number of selections currently active. Rectangular selections are
  handled (and returned) as multiple selections, one for each line. }
begin
  Result := Call(SCI_GETSELECTIONS, 0, 0);
end;

function TScintEdit.GetSelectionEndPosition(Selection: Integer): Integer;
begin
  Result := Call(SCI_GETSELECTIONNEND, Selection, 0)
end;

function TScintEdit.GetSelectionMode: TScintSelectionMode;
begin
  case Call(SCI_GETSELECTIONMODE, 0, 0) of
    SC_SEL_STREAM: Result := ssmStream;
    SC_SEL_RECTANGLE: Result := ssmRectangular;
    SC_SEL_LINES: Result := ssmLines;
    SC_SEL_THIN: Result := ssmThinRectangular;
  else
    raise GetErrorException('Unexpected SCI_GETSELECTIONMODE result');
  end;
end;

function TScintEdit.GetSelectionStartPosition(Selection: Integer): Integer;
begin
  Result := Call(SCI_GETSELECTIONNSTART, Selection, 0);
end;

function TScintEdit.GetSelText: String;
begin
  Result := ConvertRawStringToString(GetRawSelText);
end;

function TScintEdit.GetStyleAtPosition(const Pos: Integer): TScintStyleNumber;
begin
  Result := Call(SCI_GETSTYLEAT, Pos, 0);
end;

function TScintEdit.GetTarget: TScintRange;
begin
  Result.StartPos := Call(SCI_GETTARGETSTART, 0, 0);
  Result.EndPos := Call(SCI_GETTARGETEND, 0, 0);
end;

function TScintEdit.GetTextRange(const StartPos, EndPos: Integer): String;
begin
  Result := ConvertRawStringToString(GetRawTextRange(StartPos, EndPos));
end;

function TScintEdit.GetTopLine: Integer;
begin
  Result := Call(SCI_GETFIRSTVISIBLELINE, 0, 0);
end;

function TScintEdit.GetVisibleLineFromDocLine(const DocLine: Integer): Integer;
begin
  FLines.CheckIndexRange(DocLine);
  Result := Call(SCI_VISIBLEFROMDOCLINE, DocLine, 0);
end;

function TScintEdit.GetWordEndPosition(const Pos: Integer;
  const OnlyWordChars: Boolean): Integer;
begin
  Result := Call(SCI_WORDENDPOSITION, Pos, Ord(OnlyWordChars));
end;

function TScintEdit.GetWordStartPosition(const Pos: Integer;
  const OnlyWordChars: Boolean): Integer;
begin
  Result := Call(SCI_WORDSTARTPOSITION, Pos, Ord(OnlyWordChars));
end;

function TScintEdit.GetZoom: Integer;
begin
  Result := Call(SCI_GETZOOM, 0, 0);
end;

procedure TScintEdit.InitRawString(var S: TScintRawString; const Len: Integer);
begin
  SetString(S, nil, Len);
  //experimental, dont need this ATM:
  if FCodePage <> 0 then
    System.SetCodePage(RawByteString(S), FCodePage, False);
end;

function TScintEdit.IsPositionInViewVertically(const Pos: Integer): Boolean;
var
  P: TPoint;
begin
  P := GetPointFromPosition(Pos);
  Result := (P.Y >= 0) and (P.Y + GetLineHeight <= ClientHeight);
end;

class function TScintEdit.KeyCodeAndShiftToKeyDefinition(
  const KeyCode: TScintKeyCode; Shift: TShiftState): TScintKeyDefinition;
begin
  Result := KeyCode;
  if ssShift in Shift then
    Result := Result or (SCMOD_SHIFT shl 16);
  if ssAlt in Shift then
    Result := Result or (SCMOD_ALT shl 16);
  if ssCtrl in Shift then
    Result := Result or (SCMOD_CTRL shl 16);
end;

class function TScintEdit.KeyToKeyCode(const Key: AnsiChar): TScintKeyCode;
begin
  Result := Ord(UpCase(Key));
end;

function TScintEdit.MainSelTextEquals(const S: String;
  const Options: TScintFindOptions): Boolean;
begin
  Result := RawMainSelTextEquals(ConvertStringToRawString(S), Options);
end;

procedure TScintEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FStyler then
      SetStyler(nil);
end;

procedure TScintEdit.Notify(const N: TSCNotification);
begin
  case N.nmhdr.code of
    SCN_AUTOCSELECTION:
      begin
        if Assigned(FOnAutoCompleteSelection) then
          FOnAutoCompleteSelection(Self);
      end;
    SCN_CALLTIPCLICK:
      begin
        if (N.position in [1, 2]) and Assigned(FOnCallTipArrowClick) then
          FOnCallTipArrowClick(Self, N.position = 1);
      end;
    SCN_CHARADDED:
      begin
        if Assigned(FOnCharAdded) then
          FOnCharAdded(Self, AnsiChar(N.ch));
      end;
    SCN_MARGINCLICK:
      begin
        if Assigned(FOnMarginClick) then
          FOnMarginClick(Self, N.margin, GetLineFromPosition(N.position));
      end;
    SCN_MARGINRIGHTCLICK:
      begin
        if Assigned(FOnMarginRightClick) then
          FOnMarginRightClick(Self, N.margin, GetLineFromPosition(N.position));
      end;
    SCN_MODIFIED:
      begin
        { CreateWnd limits SCN_MODIFIED to INSERTTEXT and DELETETEXT }
        if N.modificationType and SC_MOD_INSERTTEXT <> 0 then
          Change(True, N.position, N.length, N.linesAdded)
        else if N.modificationType and SC_MOD_DELETETEXT <> 0 then
          Change(False, N.position, N.length, N.linesAdded);

        if (N.linesAdded > 0) and FLineNumbers then
          UpdateLineNumbersWidth;
      end;
    SCN_SAVEPOINTLEFT,
    SCN_SAVEPOINTREACHED:
      begin
        if Assigned(FOnModifiedChange) then
          FOnModifiedChange(Self);
      end;
    SCN_STYLENEEDED: StyleNeeded(N.position);
    SCN_UPDATEUI:
      begin
        if Assigned(FOnUpdateUI) then
          FOnUpdateUI(Self, TScintEditUpdates(Byte(N.updated)));
      end;
    SCN_ZOOM:
      begin
        if Assigned(FOnZoom) then
          FOnZoom(Self);
        if FLineNumbers then
          UpdateLineNumbersWidth;
      end;
  end;
end;

procedure TScintEdit.PasteFromClipboard;
begin
  Call(SCI_PASTE, 0, 0);
end;

function TScintEdit.RawMainSelTextEquals(const S: TScintRawString;
  const Options: TScintFindOptions): Boolean;
begin
  Call(SCI_TARGETFROMSELECTION, 0, 0);
  Call(SCI_SETSEARCHFLAGS, GetSearchFlags(Options), 0);
  Result := False;
  if Call(SCI_SEARCHINTARGET, Length(S), S) >= 0 then begin
    var Target := GetTarget;
    var Sel := GetSelection;
    if (Target.StartPos = Sel.StartPos) and (Target.EndPos = Sel.EndPos) then
      Result := True;
  end;
end;

class function TScintEdit.RawStringIsBlank(const S: TScintRawString): Boolean;
begin
  for var I := 1 to Length(S) do
    if not(S[I] in [#9, ' ']) then
      Exit(False);
  Result := True;
end;

procedure TScintEdit.Redo;
begin
  Call(SCI_REDO, 0, 0);
end;

procedure TScintEdit.RemoveAdditionalSelections;
{ Removes additional selections without scrolling the caret into view }
begin
  var MainSel := MainSelection;
  var CaretPos := SelectionCaretPosition[MainSel];
  var AnchorPos := SelectionAnchorPosition[MainSel];
  SetSingleSelection(CaretPos, AnchorPos);
end;

function TScintEdit.ReplaceMainSelText(const S: String;
  const ReplaceMode: TScintReplaceMode): TScintRange;
begin
  ReplaceRawMainSelText(ConvertStringToRawString(S), ReplaceMode);
end;

function TScintEdit.ReplaceRawMainSelText(const S: TScintRawString;
  const ReplaceMode: TScintReplaceMode): TScintRange;
{ Replaces the main selection just like SetRawSelText/SCI_REPLACESEL but
  without removing additional selections }
begin
  { First replace the selection }
  Call(SCI_TARGETFROMSELECTION, 0, 0);
  Call(GetReplaceTargetMessage(ReplaceMode), Length(S), S);
  { Then make the main selection an empty selection at the end of the inserted
    text, just like SCI_REPLACESEL }
  var Pos := GetTarget.EndPos; { SCI_REPLACETARGET* updates the target }
  var MainSel := MainSelection;
  SetSelectionCaretPosition(MainSel, Pos);
  SetSelectionAnchorPosition(MainSel, Pos);
  { Finally call Editor::SetLastXChosen and scroll caret into view, also just
    like SCI_REPLACESEL }
  ChooseCaretX;
  ScrollCaretIntoView;
end;

function TScintEdit.ReplaceRawTextRange(const StartPos, EndPos: Integer;
  const S: TScintRawString; const ReplaceMode: TScintReplaceMode): TScintRange;
begin
  CheckPosRange(StartPos, EndPos);
  SetTarget(StartPos, EndPos);
  Call(GetReplaceTargetMessage(ReplaceMode), Length(S), S);
  Result := GetTarget;
end;

function TScintEdit.ReplaceTextRange(const StartPos, EndPos: Integer;
  const S: String; const ReplaceMode: TScintReplaceMode): TScintRange;
begin
  Result := ReplaceRawTextRange(StartPos, EndPos, ConvertStringToRawString(S), ReplaceMode);
end;

procedure TScintEdit.RestyleLine(const Line: Integer);
begin
  var StartPos := GetPositionFromLine(Line);
  var EndPos := GetPositionFromLine(Line + 1);
  { Back up the 'last styled position' if necessary using SCI_STARTSTYLINE
    (SCI_SETENDSTYLED would have been a clearer name because setting the
    'last styled position' is all it does) }
  if StartPos < Call(SCI_GETENDSTYLED, 0, 0) then
    Call(SCI_STARTSTYLING, StartPos, 0);
  StyleNeeded(EndPos);
end;

procedure TScintEdit.ScrollCaretIntoView;
begin
  Call(SCI_SCROLLCARET, 0, 0);
end;

procedure TScintEdit.SelectAllOccurrences(const Options: TScintFindOptions);
{ At the moment this does not automatically expand folds, unlike VSCode. Also
  see SelectNextOccurrence. }
begin
  Call(SCI_TARGETWHOLEDOCUMENT, 0, 0);
  Call(SCI_SETSEARCHFLAGS, GetSearchFlags(Options), 0);
  Call(SCI_MULTIPLESELECTADDEACH, 0, 0);
end;

procedure TScintEdit.SelectAndEnsureVisible(const Range: TScintRange);
begin
  CheckPosRange(Range.StartPos, Range.EndPos);

  { If the range is in a contracted section, expand it }
  var StartLine := GetLineFromPosition(Range.StartPos);
  var EndLine := GetLineFromPosition(Range.EndPos);
  for var Line := StartLine to EndLine do
    EnsureLineVisible(Line);

  { Select }
  Selection := Range;
end;

procedure TScintEdit.SelectNextOccurrence(const Options: TScintFindOptions);
{ At the moment this does not automatically expand folds, unlike VSCode. Also
  see SelectAllOccurrences. }
begin
  Call(SCI_TARGETWHOLEDOCUMENT, 0, 0);
  Call(SCI_SETSEARCHFLAGS, GetSearchFlags(Options), 0);
  Call(SCI_MULTIPLESELECTADDNEXT, 0, 0);
end;

function TScintEdit.SelEmpty: Boolean;
{ Returns True if the main selection is empty even if there are additional
  selections. }
begin
  var Sel: TScintRange;
  Result := not SelNotEmpty(Sel);
end;

function TScintEdit.SelNotEmpty(out Sel: TScintRange): Boolean;
begin
  Sel := GetSelection;
  Result := Sel.EndPos > Sel.StartPos;
end;

procedure TScintEdit.SelectAll;
begin
  Call(SCI_SELECTALL, 0, 0);
end;

procedure TScintEdit.SetAcceptDroppedFiles(const Value: Boolean);
begin
  if FAcceptDroppedFiles <> Value then begin
    FAcceptDroppedFiles := Value;
    if HandleAllocated then
      DragAcceptFiles(Handle, Value);
  end;
end;

procedure TScintEdit.SetAutoCompleteFillupChars(const FillupChars: AnsiString);
begin
  Call(SCI_AUTOCSETFILLUPS, 0, FillupChars);
end;

procedure TScintEdit.SetAutoCompleteFontName(const Value: String);
begin
  if FAutoCompleteFontName <> Value then begin
    FAutoCompleteFontName := Value;
    UpdateStyleAttributes;
  end;
end;

procedure TScintEdit.SetAutoCompleteFontSize(const Value: Integer);
begin
  if FAutoCompleteFontSize <> Value then begin
    FAutoCompleteFontSize := Value;
    UpdateStyleAttributes;
  end;
end;

procedure TScintEdit.SetAutoCompleteSelectedItem(const S: TScintRawString);
begin
  Call(SCI_AUTOCSELECT, 0, S);
end;

procedure TScintEdit.SetAutoCompleteSeparators(const Separator, TypeSeparator: AnsiChar);
begin
  Call(SCI_AUTOCSETSEPARATOR, WParam(Separator), 0);
  Call(SCI_AUTOCSETTYPESEPARATOR, WParam(TypeSeparator), 0);
end;

procedure TScintEdit.SetAutoCompleteStopChars(const StopChars: AnsiString);
begin
  Call(SCI_AUTOCSTOPS, 0, StopChars);
end;

procedure TScintEdit.SetBraceBadHighlighting(const Pos: Integer);
begin
  Call(SCI_BRACEBADLIGHT, Pos, 0);
end;

procedure TScintEdit.SetBraceHighlighting(const Pos1, Pos2: Integer);
begin
  Call(SCI_BRACEHIGHLIGHT, Pos1, Pos2);
end;

procedure TScintEdit.SetCallTipHighlight(HighlightStart, HighlightEnd: Integer);
begin
  Call(SCI_CALLTIPSETHLT, HighlightStart, HighlightEnd);
end;

procedure TScintEdit.SetCaretColumn(const Value: Integer);
begin
  SetCaretPosition(GetPositionFromLineColumn(GetCaretLine, Value));
end;

procedure TScintEdit.SetCaretLine(const Value: Integer);
begin
  Call(SCI_GOTOLINE, Value, 0);
  ChooseCaretX;
end;

procedure TScintEdit.SetCaretPosition(const Value: Integer);
begin
  Call(SCI_GOTOPOS, Value, 0);
  ChooseCaretX;
end;

procedure TScintEdit.SetCaretPositionWithSelectFromAnchor(const Value: Integer);
{ Sets the caret position and creates a selection between the anchor and the
  caret position without scrolling the caret into view. }
begin
  Call(SCI_SETCURRENTPOS, Value, 0);
end;

procedure TScintEdit.SetCaretVirtualSpace(const Value: Integer);
{ Also sets the anchor's virtual space! }
var
  Pos, LineEndPos, MainSel: Integer;
begin
  { Weird things happen if a non-zero virtual space is set when the caret
    isn't at the end of a line, so don't allow it }
  Pos := GetCaretPosition;
  LineEndPos := GetLineEndPosition(GetLineFromPosition(Pos));
  if (Pos = LineEndPos) or (Value = 0) then begin
    MainSel := GetMainSelection;
    SetSelectionAnchorVirtualSpace(MainSel, Value);
    SetSelectionCaretVirtualSpace(MainSel, Value);
    ChooseCaretX;
  end;
end;

procedure TScintEdit.SetChangeHistory(const Value: TScintChangeHistory);
begin
  if FChangeHistory <> Value then begin
    FChangeHistory := Value;
    ApplyOptions;
  end;
end;

procedure TScintEdit.SetCodePage(const Value: Integer);
begin
  if FCodePage <> Value then begin
    FCodePage := Value;
    UpdateCodePage;
  end;
end;

procedure TScintEdit.SetCursorID(const CursorID: Integer);
begin
  Call(SCI_SETCURSOR, CursorID, 0);
end;

procedure TScintEdit.SetDefaultWordChars;
begin
  SetWordChars(GetDefaultWordChars);
end;

procedure TScintEdit.SetEmptySelection;
{ Make the main selection empty and removes additional selections without
  scrolling the caret into view }
begin
  Call(SCI_SETEMPTYSELECTION, GetCaretPosition, 0);
end;

procedure TScintEdit.SetEmptySelections;
{ Makes all selections empty without scrolling the caret into view }
begin
  for var Selection := 0 to SelectionCount-1 do begin
    var Pos := SelectionCaretPosition[Selection];
    SelectionAnchorPosition[Selection] := Pos;
  end;
end;

procedure TScintEdit.SetFillSelectionToEdge(const Value: Boolean);
begin
  if FFillSelectionToEdge <> Value then begin
    FFillSelectionToEdge := Value;
    ApplyOptions;
  end;
end;

procedure TScintEdit.SetFoldFlags(const Value: TScintFoldFlags);
begin
  var Flags := 0;
  if sffLineBeforeExpanded in Value then
    Flags := Flags or SC_FOLDFLAG_LINEBEFORE_EXPANDED;
  if sffLineBeforeContracted in Value then
    Flags := Flags or SC_FOLDFLAG_LINEBEFORE_CONTRACTED;
  if sffLineAfterExpanded in Value then
    Flags := Flags or SC_FOLDFLAG_LINEAFTER_EXPANDED;
  if sffLineAfterContracted in Value then
    Flags := Flags or SC_FOLDFLAG_LINEAFTER_CONTRACTED;

  if sffLevelNumbers in Value then
    Flags := Flags or SC_FOLDFLAG_LEVELNUMBERS
  else if sffLineState in Value then
    Flags := Flags or SC_FOLDFLAG_LINESTATE;

  Call(SCI_SETFOLDFLAGS, Flags, 0);

  var FoldLevelNumbersOrLineState := Value * [sffLevelNumbers, sffLineState] <> [];
  if FoldLevelNumbersOrLineState <> FFoldLevelNumbersOrLineState then begin
    FFoldLevelNumbersOrLineState := FoldLevelNumbersOrLineState;
    UpdateLineNumbersWidth;
  end;
end;

procedure TScintEdit.SetIndicators(const StartPos, EndPos: Integer;
  const IndicatorNumber: TScintIndicatorNumber; const Value: Boolean);
begin
  CheckPosRange(StartPos, EndPos);
  Call(SCI_SETINDICATORCURRENT, IndicatorNumber, 0);
  if Value then begin
    Call(SCI_SETINDICATORVALUE, IndicatorNumber, 1);
    Call(SCI_INDICATORFILLRANGE, StartPos, EndPos - StartPos);
  end else
    Call(SCI_INDICATORCLEARRANGE, StartPos, EndPos - StartPos);
end;

procedure TScintEdit.SetLineIndentation(const Line, Indentation: Integer);
begin
  FLines.CheckIndexRange(Line);
  Call(SCI_SETLINEINDENTATION, Line, Indentation);
end;

procedure TScintEdit.SetIndentationGuides(const Value: TScintIndentationGuides);
begin
  if FIndentationGuides <> Value then begin
    FIndentationGuides := Value;
    ApplyOptions;
  end;
end;

procedure TScintEdit.SetLineNumbers(const Value: Boolean);
begin
  if FLineNumbers <> Value then begin
    FLineNumbers := Value;
    UpdateLineNumbersWidth;
  end;
end;

procedure TScintEdit.SetMainSelection(const Value: Integer);
begin
  Call(SCI_SETMAINSELECTION, Value, 0);
end;

procedure TScintEdit.SetMainSelText(const Value: String);
begin
  SetRawMainSelText(ConvertStringToRawString(Value));
end;

procedure TScintEdit.SetRawMainSelText(const Value: TScintRawString);
begin
  ReplaceRawMainSelText(Value, srmMinimal);
end;

procedure TScintEdit.SetRawSelText(const Value: TScintRawString);
{ Replaces the main selection's text and *clears* additional selections }
begin
  Call(SCI_REPLACESEL, 0, Value);
end;

procedure TScintEdit.SetRawText(const Value: TScintRawString);
begin
  { Workaround: Without this call, if the caret is on line 0 and out in
    virtual space, it'll remain in virtual space after the replacement }
  Call(SCI_CLEARSELECTIONS, 0, 0);
  { Using ReplaceRawTextRange instead of SCI_SETTEXT for embedded null support }
  ReplaceRawTextRange(0, GetRawTextLength, Value);
  ChooseCaretX;
end;

procedure TScintEdit.SetReadOnly(const Value: Boolean);
begin
  Call(SCI_SETREADONLY, Ord(Value), 0);
end;

procedure TScintEdit.SetSavePoint;
begin
  if FForceModified then begin
    FForceModified := False;
    if Assigned(FOnModifiedChange) then
      FOnModifiedChange(Self);
  end;
  Call(SCI_SETSAVEPOINT, 0, 0);
end;

procedure TScintEdit.SetSelection(const Value: TScintRange);
{ Sets the main selection and removes additional selections. Very similar
  to SetSingleSelection, not sure why both messages exist and are slightly
  different }
begin
  Call(SCI_SETSEL, Value.StartPos, Value.EndPos);
end;

procedure TScintEdit.SetSelectionAnchorPosition(Selection: Integer;
  const Value: Integer);
{ Also sets anchors's virtual space to 0 }
begin
  Call(SCI_SETSELECTIONNANCHOR, Selection, Value);
end;

procedure TScintEdit.SetSelectionAnchorVirtualSpace(Selection: Integer;
  const Value: Integer);
begin
  Call(SCI_SETSELECTIONNANCHORVIRTUALSPACE, Selection, Value);
end;

procedure TScintEdit.SetSelectionCaretPosition(Selection: Integer;
  const Value: Integer);
{ Also sets caret's virtual space to 0 }
begin
  Call(SCI_SETSELECTIONNCARET, Selection, Value);
end;

procedure TScintEdit.SetSelectionCaretVirtualSpace(Selection: Integer;
  const Value: Integer);
begin
  Call(SCI_SETSELECTIONNCARETVIRTUALSPACE, Selection, Value);
end;

procedure TScintEdit.SetSelectionMode(const Value: TScintSelectionMode);
begin
  var Mode: Integer;
  if Value = ssmStream then
    Mode := SC_SEL_STREAM
  else if Value = ssmRectangular then
    Mode := SC_SEL_RECTANGLE
  else if Value = ssmLines then
    Mode := SC_SEL_LINES
  else
    Mode := SC_SEL_THIN;
  { Note this uses *CHANGE* and not *SET* }
  Call(SCI_CHANGESELECTIONMODE, Mode, 0);
end;

procedure TScintEdit.SetSelText(const Value: String);
begin
  SetRawSelText(ConvertStringToRawString(Value));
end;

procedure TScintEdit.SetSingleSelection(const CaretPos, AnchorPos: Integer);
{ Sets the main selection and removes additional selections without scrolling
  the caret into view }
begin
  Call(SCI_SETSELECTION, CaretPos, AnchorPos);
end;

procedure TScintEdit.SetStyler(const Value: TScintCustomStyler);
begin
  if FStyler <> Value then begin
    if Assigned(Value) then
      Value.FreeNotification(Self);
    FStyler := Value;
    if HandleAllocated then begin
      Call(SCI_CLEARDOCUMENTSTYLE, 0, 0);
      Call(SCI_STARTSTYLING, 0, 0);
      UpdateStyleAttributes;
    end;
  end;
end;

procedure TScintEdit.SetTabWidth(const Value: Integer);
begin
  if (FTabWidth <> Value) and (Value > 0) and (Value < 100) then begin
    FTabWidth := Value;
    ApplyOptions;
  end;
end;

procedure TScintEdit.SetTarget(const StartPos, EndPos: Integer);
begin
  Call(SCI_SETTARGETSTART, StartPos, 0);
  Call(SCI_SETTARGETEND, EndPos, 0);
end;

procedure TScintEdit.SetTopLine(const Value: Integer);
begin
  Call(SCI_SETFIRSTVISIBLELINE, Value, 0);
end;

procedure TScintEdit.SetUseStyleAttributes(const Value: Boolean);
begin
  if FUseStyleAttributes <> Value then begin
    FUseStyleAttributes := Value;
    UpdateStyleAttributes;
  end;
end;

procedure TScintEdit.SetUseTabCharacter(const Value: Boolean);
begin
  if FUseTabCharacter <> Value then begin
    FUseTabCharacter := Value;
    ApplyOptions;
  end;
end;

procedure TScintEdit.SetVirtualSpaceOptions(const Value: TScintVirtualSpaceOptions);
begin
  if FVirtualSpaceOptions <> Value then begin
    FVirtualSpaceOptions := Value;
    ApplyOptions;
  end;
end;

procedure TScintEdit.SetWordChars(const S: AnsiString);
begin
  FWordChars := S;
  FWordCharsAsSet := [];
  for var C in S do
    Include(FWordCharsAsSet, C);
  Call(SCI_SETWORDCHARS, 0, S);
end;

procedure TScintEdit.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then begin
    FWordWrap := Value;
    ApplyOptions;
  end;
end;

procedure TScintEdit.SetZoom(const Value: Integer);
begin
  Call(SCI_SETZOOM, Value, 0);
end;

procedure TScintEdit.ShowAutoComplete(const CharsEntered: Integer;
  const WordList: AnsiString);
begin
  Call(SCI_AUTOCSHOW, CharsEntered, WordList);
end;

procedure TScintEdit.ShowCallTip(const Pos: Integer;
  const Definition: AnsiString);
begin
  Call(SCI_CALLTIPSHOW, Pos, Definition);
end;

procedure TScintEdit.StyleNeeded(const EndPos: Integer);

  function CalcCaretIndex(const FirstLine, LastLine: Integer): Integer;
  var
    CaretPos, StartPos, EndPos: Integer;
  begin
    Result := 0;
    if FReportCaretPositionToStyler then begin
      CaretPos := GetCaretPosition;
      StartPos := GetPositionFromLine(FirstLine);
      EndPos := GetLineEndPosition(LastLine);
      if (CaretPos >= StartPos) and (CaretPos <= EndPos) then
        Result := CaretPos - StartPos + 1;
    end;
  end;

  procedure MaskDoubleByteCharacters(var S: TScintRawString);
  var
    Len, I: Integer;
  begin
    { This replaces all lead and trail bytes in S with #$80 and #$81 to
      ensure that stylers do not mistake trail bytes for single-byte ASCII
      characters (e.g. #131'A' is a valid combination on CP 932). }
    if not FEffectiveCodePageDBCS then
      Exit;
    Len := Length(S);
    I := 1;
    while I <= Len do begin
      if S[I] in FLeadBytes then begin
        S[I] := #$80;
        if I < Len then begin
          Inc(I);
          S[I] := #$81;
        end;
      end;
      Inc(I);
    end;
  end;

  function LineSpans(const Line: Integer): Boolean;
  var
    S: TScintRawString;
  begin
    S := FLines.RawLines[Line];
    MaskDoubleByteCharacters(S);
    Result := FStyler.LineTextSpans(S);
  end;

  function StyleLine(const FirstLine: Integer; const StartStylingPos: Integer): Integer;
  begin
    { Find final line in series of spanned lines }
    var LastLine := FirstLine;
    while (LastLine < Lines.Count - 1) and LineSpans(LastLine) do
      Inc(LastLine);

    { We don't pass line endings to the styler, because when the style of a
      line ending changes, Scintilla assumes it must be a 'hanging' style and
      immediately repaints all subsequent lines. (To see this in the IS IDE,
      insert and remove a ';' character before a [Setup] directive, i.e.
      toggle comment styling.) }

    FStyler.FCaretIndex := CalcCaretIndex(FirstLine, LastLine);
    FStyler.FCurIndex := 1;
    FStyler.FStyleStartIndex := 1;
    FStyler.FLineState := 0;
    if FirstLine > 0 then
      FStyler.FLineState := FLines.GetState(FirstLine-1);
    FStyler.FText := GetRawTextRange(GetPositionFromLine(FirstLine),
      GetLineEndPosition(LastLine));
    MaskDoubleByteCharacters(FStyler.FText);
    FStyler.FTextLen := Length(FStyler.FText);
    FStyler.FStyleStr := StringOfChar(AnsiChar(0), FStyler.FTextLen +
      FLines.GetLineEndingLength(LastLine));

    var PreviousLineState := FStyler.LineState;

    FStyler.StyleNeeded;

    var N := Length(FStyler.FStyleStr);
    if N > 0 then begin
      var HadStyleByteIndicators := False;

      { Apply style byte indicators. Add first as INDICATOR_CONTAINER and so on. }
      for var Indicator := 0 to High(TScintStyleByteIndicatorNumber) do begin
        var PrevI := 1;
        var PrevValue := Indicator in TScintStyleByteIndicatorNumbers(Byte(Ord(FStyler.FStyleStr[1]) shr StyleNumberBits));
        for var CurI := 2 to N do begin
          var CurValue := Indicator in TScintStyleByteIndicatorNumbers(Byte(Ord(FStyler.FStyleStr[CurI]) shr StyleNumberBits));
          if CurValue <> PrevValue then begin
            SetIndicators(StartStylingPos+PrevI-1, StartStylingPos+CurI-1, Ord(Indicator)+INDICATOR_CONTAINER, PrevValue);
            HadStyleByteIndicators := HadStyleByteIndicators or PrevValue;
            PrevI := CurI;
            PrevValue := CurValue;
          end;
        end;
        SetIndicators(StartStylingPos+PrevI-1, StartStylingPos+N, Ord(Indicator)+INDICATOR_CONTAINER, PrevValue);
        HadStyleByteIndicators := HadStyleByteIndicators or PrevValue;
      end;

      { Apply styles after removing any style byte indicators }
      if HadStyleByteIndicators then
        for var I := 1 to N do
          FStyler.FStyleStr[I] := AnsiChar(Ord(FStyler.FStyleStr[I]) and StyleNumberMask);
      Call(SCI_SETSTYLINGEX, Length(FStyler.FStyleStr), FStyler.FStyleStr);

      FStyler.FStyleStr := '';
      FStyler.FText := '';
    end;

    { Get fold level }

    var LineState := FStyler.LineState;
    var FoldLevel: Integer;
    var FoldHeader, EnableFoldHeaderOnPrevious: Boolean;

    FStyler.GetFoldLevel(LineState, PreviousLineState, FoldLevel, FoldHeader, EnableFoldHeaderOnPrevious);

    Inc(FoldLevel, SC_FOLDLEVELBASE);
    if FoldHeader then
      FoldLevel := FoldLevel or SC_FOLDLEVELHEADERFLAG;

    { Apply line state and fold level }

    for var I := FirstLine to LastLine do begin
      var OldState := FLines.GetState(I);
      if FStyler.FLineState <> OldState then
        Call(SCI_SETLINESTATE, I, FStyler.FLineState);
      var OldLevel := Call(SCI_GETFOLDLEVEL, I, 0);
      var NewLevel := FoldLevel;
      { Setting SC_FOLDLEVELWHITEFLAG on empty lines causes a problem: when
        Scintilla auto expands a contracted section (for example after removing ']'
        from a section header) all the empty lines stay invisible, even any which
        are in the middle of the section. See https://sourceforge.net/p/scintilla/bugs/2442/ }
      //if FLines.GetRawLineLength(I) = 0 then
      //  NewLevel := NewLevel or SC_FOLDLEVELWHITEFLAG;
      if NewLevel <> OldLevel then
        Call(SCI_SETFOLDLEVEL, I, NewLevel);
    end;

    { Retroactively set header on previous line if requested to do so. Must be
      *after* the loop above. Not sure why. Problem reproduction: move code above
      the loop, run it, open Debug.iss, change [Setup] to [Set up] and notice
      styling of the [Languages] section below it is now broken. If you turn on
      sffLevelNumbers you will also see that the first entry in that section got
      a header flag. }

    if (FirstLine > 0) and EnableFoldHeaderOnPrevious then begin
      var PreviousLine := FirstLine-1;
      var OldLevel := Call(SCI_GETFOLDLEVEL, PreviousLine, 0);
      var NewLevel := OldLevel or SC_FOLDLEVELHEADERFLAG;
      if NewLevel <> OldLevel then
        Call(SCI_SETFOLDLEVEL, PreviousLine, NewLevel);
    end;

    Result := LastLine;
  end;

  procedure DefaultStyleLine(const Line: Integer);
  var
    StyleStr: AnsiString;
  begin
    { Note: Using SCI_SETSTYLINGEX because it only redraws the part of the
      range that changed, whereas SCI_SETSTYLING redraws the entire range. }
    StyleStr := StringOfChar(AnsiChar(0), FLines.GetRawLineLengthWithEnding(Line));
    Call(SCI_SETSTYLINGEX, Length(StyleStr), StyleStr);
  end;

var
  StartPos, StartLine, EndLine, Line: Integer;
begin
  StartPos := Call(SCI_GETENDSTYLED, 0, 0);
  StartLine := GetLineFromPosition(StartPos);
  { EndPos (always?) points to the position *after* the last character of the
    last line needing styling (usually an LF), so subtract 1 to avoid
    restyling one extra line unnecessarily.
    But don't do this if we're being asked to style all the way to the end.
    When the document's last line is empty, 'EndPos - 1' will point to the
    line preceding the last line, so StyleLine() will never be called on the
    last line, and it will never be assigned a LINESTATE. This causes IS's
    autocompletion to think the last line's section is scNone. }
  if EndPos < GetRawTextLength then
    EndLine := GetLineFromPosition(EndPos - 1)
  else
    EndLine := GetLineFromPosition(EndPos);

  //outputdebugstring('-----');
  //outputdebugstring(pchar(format('StyleNeeded poses: %d, %d', [StartPos, EndPos])));
  //outputdebugstring(pchar(format('StyleNeeded lines: %d, %d', [StartLine, EndLine])));

  { If StartLine is within a series of spanned lines, back up }
  if Assigned(FStyler) then
    while (StartLine > 0) and (LineSpans(StartLine - 1)) do
      Dec(StartLine);

  Line := StartLine;
  while Line <= EndLine do begin
    var StartStylingPos := GetPositionFromLine(Line);
    Call(SCI_STARTSTYLING, StartStylingPos, 0);
    if Assigned(FStyler) then
      Line := StyleLine(Line, StartStylingPos)
    else
      DefaultStyleLine(Line);
    Inc(Line);
  end;
end;

procedure TScintEdit.SysColorChange(const Message: TMessage);
begin
  ForwardMessage(Message);
end;

function TScintEdit.TestRawRegularExpression(const S: TScintRawString): Boolean;
{ Example invalid regular expression: ( }
begin
  Call(SCI_SETTARGETRANGE, 0, 0);
  Call(SCI_SETSEARCHFLAGS, GetSearchFlags([sfoRegEx]), 0);
  var WarnStatus: Integer;
  var Res := Call(SCI_SEARCHINTARGET, Length(S), S, WarnStatus);
  Result := not ((Res = -1) and (WarnStatus = SC_STATUS_WARN_REGEX));
end;

function TScintEdit.TestRegularExpression(const S: String): Boolean;
begin
  Result := TestRawRegularExpression(ConvertStringToRawString(S));
end;

procedure TScintEdit.Undo;
begin
  Call(SCI_UNDO, 0, 0);
end;

procedure TScintEdit.UpdateCodePage;

  procedure InitLeadBytes;
  var
    Info: TCPInfo;
    I: Integer;
    J: Byte;
  begin
    FLeadBytes := [];
    if FEffectiveCodePageDBCS and GetCPInfo(FEffectiveCodePage, Info) then begin
      I := 0;
      while (I < MAX_LEADBYTES) and ((Info.LeadByte[I] or Info.LeadByte[I+1]) <> 0) do begin
        for J := Info.LeadByte[I] to Info.LeadByte[I+1] do
          Include(FLeadBytes, AnsiChar(J));
        Inc(I, 2);
      end;
    end;
  end;

var
  CP: Integer;
begin
  if HandleAllocated then begin
    { To Scintilla, code page 0 does not mean the current ANSI code page, but
      an unspecified single byte code page. So that DBCS support is properly
      enabled when running on a DBCS ANSI code page, replace 0 with GetACP. }
    CP := FCodePage;
    if CP = 0 then
      CP := GetACP;
    Call(SCI_SETCODEPAGE, CP, 0);

    { Scintilla ignores attempts to set a code page it has no special support
      for. But the editor could currently be set for UTF-8 or DBCS, so get it
      out of that mode by setting the code page to 0 (a value it does
      recognize). }
    if Call(SCI_GETCODEPAGE, 0, 0) <> CP then
      Call(SCI_SETCODEPAGE, 0, 0);

    FEffectiveCodePage := Call(SCI_GETCODEPAGE, 0, 0);
    FEffectiveCodePageDBCS := (FEffectiveCodePage <> 0) and
      (FEffectiveCodePage <> SC_CP_UTF8);
    InitLeadBytes;
  end;
end;

procedure TScintEdit.UpdateLineNumbersWidth;
var
  LineCount, PixelWidth: Integer;
  Nines: String;
begin
  if FLineNumbers or FFoldLevelNumbersOrLineState then begin
    { Note: Based on SciTE's SciTEBase::SetLineNumberWidth. }

    if FFoldLevelNumbersOrLineState then
      Nines := StringOfChar('9', 12)
    else begin
      LineCount := Call(SCI_GETLINECOUNT, 0, 0);

      Nines := '9';
      while LineCount >= 10 do begin
        LineCount := LineCount div 10;
        Nines := Nines + '9';
      end;
    end;

    PixelWidth := 4 + Call(SCI_TEXTWIDTH, STYLE_LINENUMBER, AnsiString(Nines));
  end else
    PixelWidth := 0;
  
  Call(SCI_SETMARGINWIDTHN, 0, PixelWidth);
end;

procedure TScintEdit.UpdateStyleAttributes;
var
  DefaultAttr: TScintStyleAttributes; 

  procedure SetStyleAttr(const StyleNumber: Integer;
    const Attr: TScintStyleAttributes; const Force: Boolean);
  begin
    if Force or (Attr.FontName <> DefaultAttr.FontName) then
      Call(SCI_STYLESETFONT, StyleNumber, AnsiString(Attr.FontName));
    if Force or (Attr.FontSize <> DefaultAttr.FontSize) then
      { Note: Scintilla doesn't support negative point sizes like the VCL }
      Call(SCI_STYLESETSIZE, StyleNumber, Abs(Attr.FontSize));
    if Force or (Attr.FontCharset <> DefaultAttr.FontCharset) then
      Call(SCI_STYLESETCHARACTERSET, StyleNumber, Attr.FontCharset);
    if Force or (Attr.FontStyle <> DefaultAttr.FontStyle) then begin
      Call(SCI_STYLESETBOLD, StyleNumber, Ord(fsBold in Attr.FontStyle));
      Call(SCI_STYLESETITALIC, StyleNumber, Ord(fsItalic in Attr.FontStyle));
      Call(SCI_STYLESETUNDERLINE, StyleNumber, Ord(fsUnderline in Attr.FontStyle));
    end;
    if Force or (Attr.ForeColor <> DefaultAttr.ForeColor) then
      Call(SCI_STYLESETFORE, StyleNumber, ColorToRGB(Attr.ForeColor));
    if Force or (Attr.BackColor <> DefaultAttr.BackColor) then
      Call(SCI_STYLESETBACK, StyleNumber, ColorToRGB(Attr.BackColor));
  end;

  procedure SetStyleAttrFromStyler(const StyleNumber: Integer);
  var
    Attr: TScintStyleAttributes;
  begin
    Attr := DefaultAttr;
    FStyler.GetStyleAttributes(StyleNumber, Attr);
    SetStyleAttr(StyleNumber, Attr, False);
  end;

var
  I: Integer;
begin
  if not HandleAllocated then
    Exit;

  Call(SCI_SETCARETFORE, ColorToRGB(Font.Color), 0);

  DefaultAttr.FontName := Font.Name;
  DefaultAttr.FontSize := Font.Size;
  DefaultAttr.FontStyle := Font.Style;
  DefaultAttr.FontCharset := Font.Charset;
  DefaultAttr.ForeColor := Font.Color;
  DefaultAttr.BackColor := Color;

  Call(SCI_STYLERESETDEFAULT, 0, 0);
  SetStyleAttr(STYLE_DEFAULT, DefaultAttr, True);
  Call(SCI_STYLECLEARALL, 0, 0);

  if Assigned(FStyler) then begin
    if FUseStyleAttributes then begin
      for I := 0 to StyleNumbers-1 do
        SetStyleAttrFromStyler(I);
      SetStyleAttrFromStyler(STYLE_BRACEBAD);
      SetStyleAttrFromStyler(STYLE_BRACELIGHT);
      SetStyleAttrFromStyler(STYLE_INDENTGUIDE);
    end;
    SetStyleAttrFromStyler(STYLE_LINENUMBER);
  end;

  if (AutoCompleteFontName <> '') or (AutoCompleteFontSize > 0) then begin
    if AutoCompleteFontName <> '' then
      DefaultAttr.FontName := AutoCompleteFontName;
    if AutoCompleteFontSize > 0 then
    DefaultAttr.FontSize := AutoCompleteFontSize;
    DefaultAttr.FontStyle := [];
    { Note: Scintilla doesn't actually use the colors set here }
    DefaultAttr.ForeColor := clWindowText;
    DefaultAttr.BackColor := clWindow;
    if FAutoCompleteStyle = 0 then
      FAutoCompleteStyle := Call(SCI_ALLOCATEEXTENDEDSTYLES, 1, 0);
    SetStyleAttr(FAutoCompleteStyle, DefaultAttr, True);
    Call(SCI_AUTOCSETSTYLE, FAutoCompleteStyle, 0);
  end else
    Call(SCI_AUTOCSETSTYLE, 0, 0);
end;

function TScintEdit.WordAtCaret: String;
begin
  var Range := WordAtCaretRange;
  Result := GetTextRange(Range.StartPos, Range.EndPos);
end;

function TScintEdit.WordAtCaretRange: TScintRange;
begin
  var Pos := GetCaretPosition;
  Result.StartPos := GetWordStartPosition(Pos, True);
  Result.EndPos := GetWordEndPosition(Pos, True);
end;

procedure TScintEdit.ZoomIn;
begin
  Call(SCI_ZOOMIN, 0, 0);
end;

procedure TScintEdit.ZoomOut;
begin
  Call(SCI_ZOOMOUT, 0, 0);
end;

procedure TScintEdit.CMColorChanged(var Message: TMessage);
begin
  inherited;
  UpdateStyleAttributes;
end;

procedure TScintEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateStyleAttributes;
end;

procedure TScintEdit.CMHintShow(var Message: TCMHintShow);
begin
  inherited;
  if Assigned(FOnHintShow) then
    FOnHintShow(Self, Message.HintInfo^);
end;

procedure TScintEdit.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  UpdateStyleAttributes;
end;

procedure TScintEdit.CNNotify(var Message: TWMNotify);
begin
  Notify(PSCNotification(Message.NMHdr)^);
end;

procedure TScintEdit.WMDestroy(var Message: TWMDestroy);
begin
  FDirectPtr := nil;
  FDirectStatusFunction := nil;
  inherited;
end;

procedure TScintEdit.DpiChanged(const Message: TMessage);
begin
  ForwardMessage(Message);
end;

procedure TScintEdit.WMDropFiles(var Message: TWMDropFiles);
var
  FileList: TStringList;
  NumFiles, I: Integer;
  Filename: array[0..MAX_PATH-1] of Char;
  P: TPoint;
begin
  FileList := nil;
  try
    if FAcceptDroppedFiles and Assigned(FOnDropFiles) then begin
      FileList := TStringList.Create;
      NumFiles := DragQueryFile(Message.Drop, UINT(-1), nil, 0);
      for I := 0 to NumFiles-1 do
        if DragQueryFile(Message.Drop, I, Filename,
           SizeOf(Filename) div SizeOf(Filename[0])) <> 0 then
          FileList.Add(Filename);

      if FileList.Count > 0 then begin
        if not DragQueryPoint(Message.Drop, P) then begin
          P.X := -1;
          P.Y := -1;
        end;
        FOnDropFiles(Self, P.X, P.Y, FileList);
      end;
    end;
  finally
    FileList.Free;
    DragFinish(Message.Drop);
    Message.Drop := 0;
  end;
end;

procedure TScintEdit.WMEraseBkgnd(var Message: TMessage);
begin
  { Bypass the VCL's WM_ERASEBKGND handler; it causes flicker when selecting +
    scrolling downward using the mouse }
  Message.Result := CallWindowProc(DefWndProc, Handle, Message.Msg,
    Message.WParam, Message.LParam);
end;

procedure TScintEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or (DLGC_WANTARROWS or DLGC_WANTTAB);
end;

procedure TScintEdit.WMMouseWheel(var Message: TMessage);
begin
  { Bypass TControl's broken WM_MOUSEWHEEL handler: it translates WParamLo
    from a combination of MK_* values to a TShiftState -- which is only
    meaningful to the VCL -- but it doesn't restore the original value before
    passing an unhandled WM_MOUSEWHEEL message up to DefWndProc. This causes
    Scintilla to see Ctrl+wheel as Shift+wheel, breaking zoom. (Observed on
    Delphi 2009 and still needed in Delphi 11.3.) }
  Message.Result := CallWindowProc(DefWndProc, Handle, Message.Msg,
    Message.WParam, Message.LParam);
end;

procedure TScintEdit.SettingChange(const Message: TMessage);
begin
  ForwardMessage(Message);
end;

{ TScintEditStrings }

procedure TScintEditStrings.CheckIndexRange(const Index: Integer);
begin
  if (Index < 0) or (Index >= GetCount) then
    Error(@SListIndexError, Index);
end;

procedure TScintEditStrings.CheckIndexRangePlusOne(const Index: Integer);
begin
  if (Index < 0) or (Index > GetCount) then
    Error(@SListIndexError, Index);
end;

procedure TScintEditStrings.Clear;
begin
  FEdit.SetRawText('');
end;

procedure TScintEditStrings.Delete(Index: Integer);
var
  StartPos, EndPos: Integer;
begin
  CheckIndexRange(Index);
  StartPos := FEdit.GetPositionFromLine(Index);
  EndPos := FEdit.GetPositionFromLine(Index + 1);
  FEdit.ReplaceRawTextRange(StartPos, EndPos, '');
end;

function TScintEditStrings.Get(Index: Integer): String;
begin
  Result := FEdit.ConvertRawStringToString(GetRawLine(Index));
end;

function TScintEditStrings.GetCount: Integer;
begin
  Result := FEdit.Call(SCI_GETLINECOUNT, 0, 0);
end;

function TScintEditStrings.GetLineEndingLength(const Index: Integer): Integer;
var
  StartPos, EndPos: Integer;
begin
  CheckIndexRange(Index);
  StartPos := FEdit.GetLineEndPosition(Index);
  EndPos := FEdit.GetPositionFromLine(Index + 1);
  Result := EndPos - StartPos;
end;

function TScintEditStrings.GetRawLine(Index: Integer): TScintRawString;
var
  StartPos, EndPos: Integer;
begin
  CheckIndexRange(Index);
  StartPos := FEdit.GetPositionFromLine(Index);
  EndPos := FEdit.GetLineEndPosition(Index);
  Result := FEdit.GetRawTextRange(StartPos, EndPos);
end;

function TScintEditStrings.GetRawLineLength(Index: Integer): Integer;
var
  StartPos, EndPos: Integer;
begin
  CheckIndexRange(Index);
  StartPos := FEdit.GetPositionFromLine(Index);
  EndPos := FEdit.GetLineEndPosition(Index);
  Result := EndPos - StartPos;
end;

function TScintEditStrings.GetRawLineLengthWithEnding(Index: Integer): Integer;
var
  StartPos, EndPos: Integer;
begin
  CheckIndexRange(Index);
  StartPos := FEdit.GetPositionFromLine(Index);
  EndPos := FEdit.GetPositionFromLine(Index + 1);
  Result := EndPos - StartPos;
end;

function TScintEditStrings.GetRawLineWithEnding(Index: Integer): TScintRawString;
var
  StartPos, EndPos: Integer;
begin
  CheckIndexRange(Index);
  StartPos := FEdit.GetPositionFromLine(Index);
  EndPos := FEdit.GetPositionFromLine(Index + 1);
  Result := FEdit.GetRawTextRange(StartPos, EndPos);
end;

function TScintEditStrings.GetState(Index: Integer): TScintLineState;
begin
  CheckIndexRange(Index);
  Result := FEdit.Call(SCI_GETLINESTATE, Index, 0);
end;

function TScintEditStrings.GetTextStr: String;
begin
  Result := FEdit.ConvertRawStringToString(FEdit.GetRawText);
end;

procedure TScintEditStrings.Insert(Index: Integer; const S: String);
begin
  InsertRawLine(Index, FEdit.ConvertStringToRawString(S));
end;

procedure TScintEditStrings.InsertRawLine(Index: Integer; const S: TScintRawString);
var
  Pos: Integer;
  EndingStr, InsertStr: TScintRawString;
begin
  CheckIndexRangePlusOne(Index);
  EndingStr := FEdit.GetLineEndingString;
  Pos := FEdit.GetPositionFromLine(Index);
  if (Index = GetCount) and (Pos <> FEdit.GetPositionFromLine(Index - 1)) then
    InsertStr := EndingStr + S + EndingStr
  else
    InsertStr := S + EndingStr;
  { Using ReplaceRawTextRange instead of SCI_INSERTTEXT for embedded null support }
  FEdit.ReplaceRawTextRange(Pos, Pos, InsertStr);
end;

procedure TScintEditStrings.Put(Index: Integer; const S: String);
begin
  PutRawLine(Index, FEdit.ConvertStringToRawString(S));
end;

procedure TScintEditStrings.PutRawLine(Index: Integer; const S: TScintRawString);
var
  StartPos, EndPos: Integer;
begin
  CheckIndexRange(Index);
  StartPos := FEdit.GetPositionFromLine(Index);
  EndPos := FEdit.GetLineEndPosition(Index);
  FEdit.ReplaceRawTextRange(StartPos, EndPos, S, srmMinimal);
end;

procedure TScintEditStrings.SetText(Text: PChar);
begin
  FEdit.SetRawText(FEdit.ConvertPCharToRawString(Text, StrLen(Text)));
end;

procedure TScintEditStrings.SetTextStr(const Value: String);
begin
  FEdit.SetRawText(FEdit.ConvertStringToRawString(Value));
end;

{ TScintCustomStyler }

procedure TScintCustomStyler.ApplyStyleByteIndicators(const Indicators: TScintStyleByteIndicatorNumbers;
  StartIndex, EndIndex: Integer);
begin
  var IndByte := Byte(Indicators) shl StyleNumberBits;
  if IndByte <> 0 then begin
    if StartIndex < 1 then
      StartIndex := 1;
    if EndIndex > FTextLen then
      EndIndex := FTextLen;
    for var I := StartIndex to EndIndex do
      FStyleStr[I] := AnsiChar(Ord(FStyleStr[I]) or IndByte);
  end;
end;

procedure TScintCustomStyler.ApplyStyle(const Style: TScintStyleNumber;
  StartIndex, EndIndex: Integer);
begin
  if StartIndex < 1 then
    StartIndex := 1;
  if EndIndex > FTextLen then
    EndIndex := FTextLen;
  for var I := StartIndex to EndIndex do
    if Ord(FStyleStr[I]) and StyleNumberMask = 0 then
      FStyleStr[I] := AnsiChar(Style or (Ord(FStyleStr[I]) and not StyleNumberMask));
end;

procedure TScintCustomStyler.CommitStyle(const Style: TScintStyleNumber);
begin
  ApplyStyle(Style, FStyleStartIndex, FCurIndex - 1);
  FStyleStartIndex := FCurIndex;
end;

function TScintCustomStyler.ConsumeAllRemaining: Boolean;
begin
  Result := FCurIndex <= FTextLen;
  if Result then
    FCurIndex := FTextLen + 1;
end;

function TScintCustomStyler.ConsumeChar(const C: AnsiChar): Boolean;
begin
  Result := (FCurIndex <= FTextLen) and (FText[FCurIndex] = C);
  if Result then
    Inc(FCurIndex);
end;

function TScintCustomStyler.ConsumeCharIn(const Chars: TScintRawCharSet): Boolean;
begin
  Result := (FCurIndex <= FTextLen) and (FText[FCurIndex] in Chars);
  if Result then
    Inc(FCurIndex);
end;

function TScintCustomStyler.ConsumeChars(const Chars: TScintRawCharSet): Boolean;
begin
  Result := False;
  while FCurIndex <= FTextLen do begin
    if not(FText[FCurIndex] in Chars) then
      Break;
    Result := True;
    Inc(FCurIndex);
  end;
end;

function TScintCustomStyler.ConsumeCharsNot(const Chars: TScintRawCharSet): Boolean;
begin
  Result := False;
  while FCurIndex <= FTextLen do begin
    if FText[FCurIndex] in Chars then
      Break;
    Result := True;
    Inc(FCurIndex);
  end;
end;

function TScintCustomStyler.ConsumeString(const Chars: TScintRawCharSet): TScintRawString;
var
  StartIndex: Integer;
begin
  StartIndex := FCurIndex;
  ConsumeChars(Chars);
  Result := Copy(FText, StartIndex, FCurIndex - StartIndex);
end;

function TScintCustomStyler.CurCharIn(const Chars: TScintRawCharSet): Boolean;
begin
  Result := (FCurIndex <= FTextLen) and (FText[FCurIndex] in Chars);
end;

function TScintCustomStyler.CurCharIs(const C: AnsiChar): Boolean;
begin
  Result := (FCurIndex <= FTextLen) and (FText[FCurIndex] = C);
end;

function TScintCustomStyler.GetCurChar: AnsiChar;
begin
  Result := #0;
  if FCurIndex <= FTextLen then
    Result := FText[FCurIndex];
end;

function TScintCustomStyler.GetEndOfLine: Boolean;
begin
  Result := FCurIndex > FTextLen;
end;

function TScintCustomStyler.LineTextSpans(const S: TScintRawString): Boolean;
begin
  Result := False;
end;

function TScintCustomStyler.NextCharIs(const C: AnsiChar): Boolean;
begin
  Result := (FCurIndex < FTextLen) and (FText[FCurIndex+1] = C);
end;

function TScintCustomStyler.PreviousCharIn(const Chars: TScintRawCharSet): Boolean;
begin
  Result := (FCurIndex > 1) and (FCurIndex-1 <= FTextLen) and
    (FText[FCurIndex-1] in Chars);
end;

procedure TScintCustomStyler.ReplaceText(StartIndex, EndIndex: Integer;
  const C: AnsiChar);
var
  P: PAnsiChar;
  I: Integer;
begin
  if StartIndex < 1 then
    StartIndex := 1;
  if EndIndex > FTextLen then
    EndIndex := FTextLen;
  P := @FText[1];
  for I := StartIndex to EndIndex do
    P[I-1] := C;
end;

procedure TScintCustomStyler.ResetCurIndexTo(Index: Integer);
begin
  FCurIndex := Index;
  FStyleStartIndex := Index;
end;

{ TScintPixmap }

const
  XPMTransparentChar = ' ';
  XPMTerminatorChar = '"';

class constructor TScintPixmap.Create;
begin
  { Chars 128-255 are supported below but don't work in Scintilla }
  for var C := #1 to #127 do
    if (C <> XPMTransparentChar) and (C <> XPMTerminatorChar) then
      ColorCodes := ColorCodes + C;
end;

function TScintPixmap.GetPixmap: Pointer;
begin
  Result := FPixmap;
end;

type
  TRGBTripleArray = array[0..MaxInt div SizeOf(TRGBTriple) - 1] of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;

procedure TScintPixmap.InitializeFromBitmap(const ABitmap: TBitmap;
  const TransparentColor: TColorRef);

  procedure SetNextPixmapLine(const Pixmap: TPixmap; var Index: Integer; const Line: String);
  begin
    if Index > High(Pixmap) then
      TScintEdit.Error('SetNextPixmapLine: Index out of range');

    { Convert Line to an AnsiString, but copy the exact ordinal values;
      i.e. don't do any translation of 128-255 }
    var AnsiLine: AnsiString;
    SetLength(AnsiLine, Length(Line));
    for var I := 1 to Length(AnsiLine) do
      AnsiLine[I] := AnsiChar(Ord(Line[I]));
    Pixmap[Index] := AnsiLine;
    Inc(Index);
  end;

begin
  if ABitmap.PixelFormat <> pf24bit then
    TScintEdit.Error('Invalid PixelFormat');

  var Colors := TDictionary<Integer, TPair<Char, String>>.Create; { RGB -> Code & WebColor }
  try
    { Build colors list }
    for var Y := 0 to ABitmap.Height-1 do begin
      var Pixels: PRGBTripleArray := ABitmap.ScanLine[Y];
      for var X := 0 to ABitmap.Width-1 do begin
        var Color := RGB(Pixels[X].rgbtRed, Pixels[X].rgbtGreen, Pixels[X].rgbtBlue);
        if (Color <> TransparentColor) and not Colors.ContainsKey(Color) then begin
          var ColorCodeIndex := Colors.Count+1;
          if ColorCodeIndex > Length(ColorCodes) then
            TScintEdit.Error('Too many colors');
          Colors.Add(Color, TPair<Char, String>.Create(ColorCodes[ColorCodeIndex], RGBToWebColorStr(Color)))
        end;
      end;
    end;

    { Build pixmap }
    var Line: String;
    SetLength(FPixmap, 0); { Not really needed but makes things clearer while debugging }
    SetLength(FPixmap, 1 + Colors.Count + ABitmap.Height + 1);
    Line := Format('%d %d %d 1', [ABitmap.Width, ABitmap.Height, Colors.Count]);
    var Index := 0;
    SetNextPixmapLine(FPixmap, Index, Line);
    for var Color in Colors do begin
      Line := Format('%s c %s', [Color.Value.Key, Color.Value.Value]);
      SetNextPixmapLine(FPixmap, Index, Line);
    end;
    for var Y := 0 to ABitmap.Height-1 do begin
      Line := '';
      var Pixels: PRGBTripleArray := ABitmap.ScanLine[Y];
      for var X := 0 to ABitmap.Width-1 do begin
        var Color := RGB(Pixels[X].rgbtRed, Pixels[X].rgbtGreen, Pixels[X].rgbtBlue);
        if Color = TransparentColor then
          Line := Line + XPMTransparentChar
        else
          Line := Line + Colors[Color].Key;
      end;
      SetNextPixmapLine(FPixmap, Index, Line);
    end;

    { Add terminating nil pointer - Scintilla doesnt really need it but setting it anyway }
    SetNextPixmapLine(FPixmap, Index, '');
  finally
    Colors.Free;
  end;
end;

{ TScintRange }

constructor TScintRange.Create(const AStartPos, AEndPos: Integer);
begin
  StartPos := AStartPos;
  EndPos := AEndPos;
end;

function TScintRange.Overlaps(const ARange: TScintRange): Boolean;
begin
  Result := not ARange.Empty and (StartPos <= ARange.EndPos) and (EndPos >= ARange.StartPos);
end;

function TScintRange.Empty: Boolean;
begin
  Result := StartPos = EndPos;
end;

function TScintRange.Within(const ARange: TScintRange): Boolean;
begin
  Result := (StartPos >= ARange.StartPos) and (EndPos <= ARange.EndPos);
end;

{ TScintRangeList }

function TScintRangeList.Overlaps(const ARange: TScintRange;
  var AOverlappingRange: TScintRange): Boolean;
begin
  for var Item in Self do begin
    if Item.Overlaps(ARange) then begin
      AOverlappingRange := Item;
      Exit(True);
    end;
  end;
  Result := False;
end;

{ TScintCaretAndAnchor }

constructor TScintCaretAndAnchor.Create(const ACaretPos, AAnchorPos: Integer);
begin
  CaretPos := ACaretPos;
  AnchorPos := AAnchorPos;
end;

function TScintCaretAndAnchor.Range: TScintRange;
begin
  if CaretPos <= AnchorPos then begin
    Result.StartPos := CaretPos;
    Result.EndPos := AnchorPos;
  end else begin
    Result.StartPos := AnchorPos;
    Result.EndPos := CaretPos;
  end;
end;

end.
