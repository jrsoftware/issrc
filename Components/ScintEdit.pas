unit ScintEdit;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TScintEdit component: a VCL wrapper for Scintilla

  $jrsoftware: issrc/Components/ScintEdit.pas,v 1.22 2010/12/28 06:52:20 jr Exp $
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ScintInt;

type
  TScintEditChangeInfo = record
    Inserting: Boolean;
    StartPos, Length, LinesDelta: Integer;
  end;
  TScintEditChangeEvent = procedure(Sender: TObject;
    const Info: TScintEditChangeInfo) of object;
  TScintEditCharAddedEvent = procedure(Sender: TObject; Ch: AnsiChar) of object;
  TScintEditDropFilesEvent = procedure(Sender: TObject; X, Y: Integer;
    AFiles: TStrings) of object;
  TScintHintInfo = {$IFDEF UNICODE} Controls. {$ENDIF} THintInfo;
  TScintEditHintShowEvent = procedure(Sender: TObject;
    var Info: TScintHintInfo) of object;
  TScintEditMarginClickEvent = procedure(Sender: TObject; MarginNumber: Integer;
    Line: Integer) of object;
  TScintFindOption = (sfoMatchCase, sfoWholeWord);
  TScintFindOptions = set of TScintFindOption;
  TScintIndentationGuides = (sigNone, sigReal, sigLookForward, sigLookBoth);
  TScintIndicatorNumber = 0..2;
  TScintIndicatorNumbers = set of TScintIndicatorNumber;
  TScintLineEndings = (sleCRLF, sleCR, sleLF);
  TScintLineState = type Integer;
  TScintMarkerNumber = 0..31;
  TScintMarkerNumbers = set of TScintMarkerNumber;
  TScintRange = record
    StartPos, EndPos: Integer;
  end;
  TScintRawCharSet = set of AnsiChar;
  TScintRawString = type {$IFDEF UNICODE} RawByteString {$ELSE} AnsiString {$ENDIF};
  TScintStyleNumber = 0..31;
  TScintVirtualSpaceOption = (svsRectangularSelection, svsUserAccessible);
  TScintVirtualSpaceOptions = set of TScintVirtualSpaceOption;

  TScintEditStrings = class;
  TScintCustomStyler = class;

  TScintEdit = class(TWinControl)
  private
    FAcceptDroppedFiles: Boolean;
    FAutoCompleteFontName: String;
    FAutoCompleteFontSize: Integer;
    FCodePage: Integer;
    FDirectPtr: Pointer;
    FEffectiveCodePage: Integer;
    FEffectiveCodePageDBCS: Boolean;
    FFillSelectionToEdge: Boolean;
    FForceModified: Boolean;
    FIndentationGuides: TScintIndentationGuides;
    FLeadBytes: TScintRawCharSet;
    FLines: TScintEditStrings;
    FOnChange: TScintEditChangeEvent;
    FOnCharAdded: TScintEditCharAddedEvent;
    FOnDropFiles: TScintEditDropFilesEvent;
    FOnHintShow: TScintEditHintShowEvent;
    FOnMarginClick: TScintEditMarginClickEvent;
    FOnModifiedChange: TNotifyEvent;
    FOnUpdateUI: TNotifyEvent;
    FReportCaretPositionToStyler: Boolean;
    FStyler: TScintCustomStyler;
    FTabWidth: Integer;
    FUseStyleAttributes: Boolean;
    FUseTabCharacter: Boolean;
    FVirtualSpaceOptions: TScintVirtualSpaceOptions;
    FWordWrap: Boolean;
    procedure ApplyOptions;
    function GetAutoCompleteActive: Boolean;
    function GetCaretColumn: Integer;
    function GetCaretColumnExpanded: Integer;
    function GetCaretLine: Integer;
    function GetCaretPosition: Integer;
    function GetCaretVirtualSpace: Integer;
    function GetInsertMode: Boolean;
    function GetLineEndings: TScintLineEndings;
    function GetLineEndingString: TScintRawString;
    function GetLineHeight: Integer;
    function GetLinesInWindow: Integer;
    function GetModified: Boolean;
    function GetRawSelText: TScintRawString;
    function GetRawText: TScintRawString;
    function GetRawTextLength: Integer;
    function GetReadOnly: Boolean;
    function GetSelection: TScintRange;
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
    procedure SetCaretVirtualSpace(const Value: Integer);
    procedure SetFillSelectionToEdge(const Value: Boolean);
    procedure SetIndentationGuides(const Value: TScintIndentationGuides);
    procedure SetRawSelText(const Value: TScintRawString);
    procedure SetRawText(const Value: TScintRawString);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetSelection(const Value: TScintRange);
    procedure SetSelText(const Value: String);
    procedure SetStyler(const Value: TScintCustomStyler);
    procedure SetTabWidth(const Value: Integer);
    procedure SetTopLine(const Value: Integer);
    procedure SetUseStyleAttributes(const Value: Boolean);
    procedure SetUseTabCharacter(const Value: Boolean);
    procedure SetVirtualSpaceOptions(const Value: TScintVirtualSpaceOptions);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetZoom(const Value: Integer);
    procedure StyleNeeded(const EndPos: Integer);
    procedure UpdateCodePage;
    procedure UpdateStyleAttributes;
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
    class procedure Error(const S: String);
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
    procedure BeginUndoAction;
    function Call(Msg: Cardinal; WParam: Longint; LParam: Longint): Longint;
    function CallStr(Msg: Cardinal; WParam: Longint;
      const LParamStr: TScintRawString): Longint;
    procedure CancelAutoComplete;
    function CanRedo: Boolean;
    function CanUndo: Boolean;
    procedure ChooseCaretX;
    procedure ClearAll;
    procedure ClearSelection;
    procedure ClearUndo;
    function ConvertRawStringToString(const S: TScintRawString): String;
    function ConvertPCharToRawString(const Text: PChar;
      const TextLen: Integer): TScintRawString;
    function ConvertStringToRawString(const S: String): TScintRawString;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure DeleteAllMarkersOnLine(const Line: Integer);
    procedure DeleteMarker(const Line: Integer; const Marker: TScintMarkerNumber);
    procedure EndUndoAction;
    function FindRawText(const StartPos, EndPos: Integer; const S: TScintRawString;
      const Options: TScintFindOptions; out MatchRange: TScintRange): Boolean;
    function FindText(const StartPos, EndPos: Integer; const S: String;
      const Options: TScintFindOptions; out MatchRange: TScintRange): Boolean;
    procedure ForceModifiedState;
    function GetCharAtPosition(const Pos: Integer): AnsiChar;
    function GetColumnFromPosition(const Pos: Integer): Integer;
    function GetDocLineFromVisibleLine(const VisibleLine: Integer): Integer;
    function GetIndicatorsAtPosition(const Pos: Integer): TScintIndicatorNumbers;
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
    function GetRawTextRange(const StartPos, EndPos: Integer): TScintRawString;
    function GetStyleAtPosition(const Pos: Integer): TScintStyleNumber;
    function GetTextRange(const StartPos, EndPos: Integer): String;
    function GetVisibleLineFromDocLine(const DocLine: Integer): Integer;
    function GetWordEndPosition(const Pos: Integer; const OnlyWordChars: Boolean): Integer;
    function GetWordStartPosition(const Pos: Integer; const OnlyWordChars: Boolean): Integer;
    function IsPositionInViewVertically(const Pos: Integer): Boolean;
    procedure PasteFromClipboard;
    function RawSelTextEquals(const S: TScintRawString; const MatchCase: Boolean): Boolean;
    procedure Redo;
    function ReplaceRawTextRange(const StartPos, EndPos: Integer;
      const S: TScintRawString): TScintRange;
    function ReplaceTextRange(const StartPos, EndPos: Integer; const S: String): TScintRange;
    procedure RestyleLine(const Line: Integer);
    procedure ScrollCaretIntoView;
    function SelAvail: Boolean;
    procedure SelectAll;
    function SelTextEquals(const S: String; const MatchCase: Boolean): Boolean;
    procedure SetAutoCompleteFillupChars(const FillupChars: AnsiString);
    procedure SetAutoCompleteSelectedItem(const S: TScintRawString);
    procedure SetAutoCompleteStopChars(const StopChars: AnsiString);
    procedure SetBraceHighlighting(const Pos1, Pos2: Integer);
    procedure SetCursorID(const CursorID: Integer);
    procedure SetEmptySelection;
    procedure SetLineIndentation(const Line, Indentation: Integer);
    procedure SetSavePoint;
    procedure ShowAutoComplete(const CharsEntered: Integer; const WordList: AnsiString);
    procedure Undo;
    function WordAtCursor: String;
    procedure ZoomIn;
    procedure ZoomOut;
    property AutoCompleteActive: Boolean read GetAutoCompleteActive;
    property CaretColumn: Integer read GetCaretColumn write SetCaretColumn;
    property CaretColumnExpanded: Integer read GetCaretColumnExpanded;
    property CaretLine: Integer read GetCaretLine write SetCaretLine;
    property CaretPosition: Integer read GetCaretPosition write SetCaretPosition;
    property CaretVirtualSpace: Integer read GetCaretVirtualSpace write SetCaretVirtualSpace;
    property EffectiveCodePage: Integer read FEffectiveCodePage;
    property InsertMode: Boolean read GetInsertMode;
    property LineEndings: TScintLineEndings read GetLineEndings;
    property LineEndingString: TScintRawString read GetLineEndingString;
    property LineHeight: Integer read GetLineHeight;
    property Lines: TScintEditStrings read FLines;
    property LinesInWindow: Integer read GetLinesInWindow;
    property Modified: Boolean read GetModified;
    property RawSelText: TScintRawString read GetRawSelText write SetRawSelText;
    property RawText: TScintRawString read GetRawText write SetRawText;
    property RawTextLength: Integer read GetRawTextLength;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Selection: TScintRange read GetSelection write SetSelection;
    property SelText: String read GetSelText write SetSelText;
    property Styler: TScintCustomStyler read FStyler write SetStyler;
    property TopLine: Integer read GetTopLine write SetTopLine;
  published
    property AcceptDroppedFiles: Boolean read FAcceptDroppedFiles write SetAcceptDroppedFiles
      default False;
    property AutoCompleteFontName: String read FAutoCompleteFontName
      write SetAutoCompleteFontName;
    property AutoCompleteFontSize: Integer read FAutoCompleteFontSize
      write SetAutoCompleteFontSize default 0;
    property CodePage: Integer read FCodePage write SetCodePage default 0;
    property FillSelectionToEdge: Boolean read FFillSelectionToEdge write SetFillSelectionToEdge
      default False;
    property Font;
    property IndentationGuides: TScintIndentationGuides read FIndentationGuides
      write SetIndentationGuides default sigNone;
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
    property OnChange: TScintEditChangeEvent read FOnChange write FOnChange;
    property OnCharAdded: TScintEditCharAddedEvent read FOnCharAdded write FOnCharAdded;
    property OnDropFiles: TScintEditDropFilesEvent read FOnDropFiles write FOnDropFiles;
    property OnHintShow: TScintEditHintShowEvent read FOnHintShow write FOnHintShow;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMarginClick: TScintEditMarginClickEvent read FOnMarginClick write FOnMarginClick;
    property OnModifiedChange: TNotifyEvent read FOnModifiedChange write FOnModifiedChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnUpdateUI: TNotifyEvent read FOnUpdateUI write FOnUpdateUI;
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
{$IFNDEF UNICODE}
    class procedure Error(Msg: PResStringRec; Data: Integer);
{$ENDIF}
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
    procedure ApplyIndicators(const Indicators: TScintIndicatorNumbers;
      StartIndex, EndIndex: Integer);
    procedure ApplyStyle(const Style: TScintStyleNumber;
      StartIndex, EndIndex: Integer);
    procedure CommitStyle(const Style: TScintStyleNumber);
    function ConsumeAllRemaining: Boolean;
    function ConsumeChar(const C: AnsiChar): Boolean;
    function ConsumeChars(const Chars: TScintRawCharSet): Boolean;
    function ConsumeCharsNot(const Chars: TScintRawCharSet): Boolean;
    function ConsumeString(const Chars: TScintRawCharSet): TScintRawString;
    function CurCharIn(const Chars: TScintRawCharSet): Boolean;
    function CurCharIs(const C: AnsiChar): Boolean;
    procedure GetStyleAttributes(const Style: Integer;
      var Attributes: TScintStyleAttributes); virtual; abstract;
    function LineTextSpans(const S: TScintRawString): Boolean; virtual;
    function NextCharIs(const C: AnsiChar): Boolean;
    function PreviousCharIn(const Chars: TScintRawCharSet): Boolean;
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

  EScintEditError = class(Exception);

implementation

uses
  ShellAPI,
{$IFDEF UNICODE}
  RTLConsts;
{$ELSE}
  Consts;
{$ENDIF}

{ TScintEdit }

constructor TScintEdit.Create(AOwner: TComponent);
begin
  inherited;
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
  Call(SCI_SETVIRTUALSPACEOPTIONS, Flags, 0);
  Call(SCI_SETWRAPMODE, Ord(FWordWrap), 0);
  Call(SCI_SETINDENTATIONGUIDES, IndentationGuides[FIndentationGuides], 0);
end;

procedure TScintEdit.BeginUndoAction;
begin
  Call(SCI_BEGINUNDOACTION, 0, 0);
end;

function TScintEdit.Call(Msg: Cardinal; WParam: Longint; LParam: Longint): Longint;
var
  ErrorStatus: LRESULT;
begin
  HandleNeeded;
  if FDirectPtr = nil then
    Error('Call: FDirectPtr is nil');
  Result := Scintilla_DirectFunction(FDirectPtr, Msg, WParam, LParam);

  ErrorStatus := Scintilla_DirectFunction(FDirectPtr, SCI_GETSTATUS, 0, 0);
  if ErrorStatus <> 0 then begin
    Scintilla_DirectFunction(FDirectPtr, SCI_SETSTATUS, 0, 0);
    ErrorFmt('Error status %d returned after Call(%u, %d, %d) = %d',
      [ErrorStatus, Msg, WParam, LParam, Result]);
  end;
end;

function TScintEdit.CallStr(Msg: Cardinal; WParam: Longint;
  const LParamStr: TScintRawString): Longint;
begin
  Result := Call(Msg, WParam, LPARAM(PAnsiChar(LParamStr)));
end;

procedure TScintEdit.CancelAutoComplete;
begin
  Call(SCI_AUTOCCANCEL, 0, 0);
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

procedure TScintEdit.ClearSelection;
begin
  Call(SCI_CLEAR, 0, 0);
end;

procedure TScintEdit.ClearUndo;
begin
  { SCI_EMPTYUNDOBUFFER resets the save point but doesn't send a
    SCN_SAVEPOINTREACHED notification. Call SetSavePoint manually to get
    that. SetSavePoint additionally resets FForceModified. }
  SetSavePoint;
  Call(SCI_EMPTYUNDOBUFFER, 0, 0);
end;

function TScintEdit.ConvertRawStringToString(const S: TScintRawString): String;
{$IFDEF UNICODE}
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
{$ELSE}
begin
  Result := S;
end;
{$ENDIF}

function TScintEdit.ConvertPCharToRawString(const Text: PChar;
  const TextLen: Integer): TScintRawString;
var
{$IFDEF UNICODE}
  DestLen: Integer;
{$ENDIF}
  DestStr: TScintRawString;
begin
  if TextLen > 0 then begin
{$IFDEF UNICODE}
    DestLen := WideCharToMultiByte(FCodePage, 0, Text, TextLen, nil, 0, nil, nil);
    if DestLen <= 0 then
      Error('WideCharToMultiByte failed');
    InitRawString(DestStr, DestLen);
    if WideCharToMultiByte(FCodePage, 0, Text, TextLen, @DestStr[1], Length(DestStr),
       nil, nil) <> DestLen then
      Error('Unexpected result from WideCharToMultiByte');
{$ELSE}
    SetString(DestStr, Text, TextLen);
{$ENDIF}
  end;
  Result := DestStr;
end;

function TScintEdit.ConvertStringToRawString(const S: String): TScintRawString;
begin
{$IFDEF UNICODE}
  Result := ConvertPCharToRawString(PChar(S), Length(S));
{$ELSE}
  Result := S;
{$ENDIF}
end;

procedure TScintEdit.CopyToClipboard;
begin
  Call(SCI_COPY, 0, 0);
end;

procedure TScintEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  CreateSubClass(Params, 'Scintilla');
  Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  Params.WindowClass.style := Params.WindowClass.style and
    not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TScintEdit.CreateWnd;
begin
  inherited;
  FDirectPtr := Pointer(SendMessage(Handle, SCI_GETDIRECTPOINTER, 0, 0));
  if FDirectPtr = nil then
    Error('CreateWnd: FDirectPtr is nil');
  UpdateCodePage;
  Call(SCI_SETCARETPERIOD, GetCaretBlinkTime, 0);
  Call(SCI_SETSCROLLWIDTHTRACKING, 1, 0);
  { The default popup menu conflicts with the VCL's PopupMenu on Delphi 3 }
  Call(SCI_USEPOPUP, 0, 0);
{$IFNDEF UNICODE}
  { This hack is needed because non-Unicode VCL replaces the Scintilla's
    default Unicode window proc with an ANSI one }
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Call(SCI_SETKEYSUNICODE, 1, 0);
{$ENDIF}
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

class procedure TScintEdit.Error(const S: String);
begin
  raise EScintEditError.Create('TScintEdit error: ' + S);
end;

class procedure TScintEdit.ErrorFmt(const S: String; const Args: array of const);
begin
  Error(Format(S, Args));
end;

function TScintEdit.FindRawText(const StartPos, EndPos: Integer;
  const S: TScintRawString; const Options: TScintFindOptions;
  out MatchRange: TScintRange): Boolean;
var
  Flags: Integer;
begin
  Flags := 0;
  if sfoMatchCase in Options then
    Flags := Flags or SCFIND_MATCHCASE;
  if sfoWholeWord in Options then
    Flags := Flags or SCFIND_WHOLEWORD;

  SetTarget(StartPos, EndPos);
  Call(SCI_SETSEARCHFLAGS, Flags, 0);
  Result := Call(SCI_SEARCHINTARGET, Length(S), LPARAM(PAnsiChar(S))) >= 0;
  if Result then
    MatchRange := GetTarget;
end;

function TScintEdit.FindText(const StartPos, EndPos: Integer; const S: String;
  const Options: TScintFindOptions; out MatchRange: TScintRange): Boolean;
begin
  Result := FindRawText(StartPos, EndPos, ConvertStringToRawString(S),
    Options, MatchRange);
end;

procedure TScintEdit.ForceModifiedState;
begin
  if not FForceModified then begin
    FForceModified := True;
    if Assigned(FOnModifiedChange) then
      FOnModifiedChange(Self);
  end;
end;

function TScintEdit.GetAutoCompleteActive: Boolean;
begin
  Result := Call(SCI_AUTOCACTIVE, 0, 0) <> 0;
end;

function TScintEdit.GetCaretColumn: Integer;
begin
  Result := GetColumnFromPosition(GetCaretPosition);
end;

function TScintEdit.GetCaretColumnExpanded: Integer;
begin
  Result := Call(SCI_GETCOLUMN, GetCaretPosition, 0);
  Inc(Result, GetCaretVirtualSpace);
end;

function TScintEdit.GetCaretLine: Integer;
begin
  Result := GetLineFromPosition(GetCaretPosition);
end;

function TScintEdit.GetCaretPosition: Integer;
begin
  Result := Call(SCI_GETCURRENTPOS, 0, 0);
end;

function TScintEdit.GetCaretVirtualSpace: Integer;
begin
  Result := Call(SCI_GETSELECTIONNCARETVIRTUALSPACE, GetMainSelection, 0);
end;

function TScintEdit.GetCharAtPosition(const Pos: Integer): AnsiChar;
begin
  Result := AnsiChar(Call(SCI_GETCHARAT, Pos, 0));
end;

function TScintEdit.GetColumnFromPosition(const Pos: Integer): Integer;
var
  Line: Integer;
begin
  Line := GetLineFromPosition(Pos);
  Result := Pos - GetPositionFromLine(Line);
end;

function TScintEdit.GetDocLineFromVisibleLine(const VisibleLine: Integer): Integer;
begin
  Result := Call(SCI_DOCLINEFROMVISIBLE, VisibleLine, 0);
end;

function TScintEdit.GetIndicatorsAtPosition(const Pos: Integer): TScintIndicatorNumbers;
var
  Indic: Byte;
begin
  Indic := Byte(Call(SCI_GETSTYLEAT, Pos, 0)) shr 5;
  Result := TScintIndicatorNumbers(Indic);
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
  else
    Result := sleCRLF;
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

function TScintEdit.GetRawSelText: TScintRawString;
var
  Len: Integer;
  S: TScintRawString;
begin
  Len := Call(SCI_GETSELTEXT, 0, 0) - 1;
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

function TScintEdit.GetSelection: TScintRange;
begin
  Result.StartPos := Call(SCI_GETSELECTIONSTART, 0, 0);
  Result.EndPos := Call(SCI_GETSELECTIONEND, 0, 0);
end;

function TScintEdit.GetSelText: String;
begin
  Result := ConvertRawStringToString(GetRawSelText);
end;

function TScintEdit.GetStyleAtPosition(const Pos: Integer): TScintStyleNumber;
begin
  Result := Call(SCI_GETSTYLEAT, Pos, 0) and $1F;
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
{$IFDEF UNICODE}
  //experimental, dont need this ATM:
  if FCodePage <> 0 then
    System.SetCodePage(RawByteString(S), FCodePage, False);
{$ENDIF}
end;

function TScintEdit.IsPositionInViewVertically(const Pos: Integer): Boolean;
var
  P: TPoint;
begin
  P := GetPointFromPosition(Pos);
  Result := (P.Y >= 0) and (P.Y + GetLineHeight <= ClientHeight);
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
    SCN_MODIFIED:
      begin
        if N.modificationType and SC_MOD_INSERTTEXT <> 0 then
          Change(True, N.position, N.length, N.linesAdded)
        else if N.modificationType and SC_MOD_DELETETEXT <> 0 then
          Change(False, N.position, N.length, N.linesAdded);
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
          FOnUpdateUI(Self);
      end;
  end;
end;

procedure TScintEdit.PasteFromClipboard;
begin
  Call(SCI_PASTE, 0, 0);
end;

function TScintEdit.RawSelTextEquals(const S: TScintRawString;
  const MatchCase: Boolean): Boolean;
var
  Flags: Integer;
  Target, Sel: TScintRange;
begin
  Flags := 0;
  if MatchCase then
    Flags := Flags or SCFIND_MATCHCASE;

  Call(SCI_TARGETFROMSELECTION, 0, 0);
  Call(SCI_SETSEARCHFLAGS, Flags, 0);
  Result := False;
  if Call(SCI_SEARCHINTARGET, Length(S), LPARAM(PAnsiChar(S))) >= 0 then begin
    Target := GetTarget;
    Sel := GetSelection;
    if (Target.StartPos = Sel.StartPos) and (Target.EndPos = Sel.EndPos) then
      Result := True;
  end;
end;

procedure TScintEdit.Redo;
begin
  Call(SCI_REDO, 0, 0);
end;

function TScintEdit.ReplaceRawTextRange(const StartPos, EndPos: Integer;
  const S: TScintRawString): TScintRange;
begin
  CheckPosRange(StartPos, EndPos);
  SetTarget(StartPos, EndPos);
  Call(SCI_REPLACETARGET, Length(S), LPARAM(PAnsiChar(S)));
  Result := GetTarget;
end;

function TScintEdit.ReplaceTextRange(const StartPos, EndPos: Integer;
  const S: String): TScintRange;
begin
  Result := ReplaceRawTextRange(StartPos, EndPos, ConvertStringToRawString(S));
end;

procedure TScintEdit.RestyleLine(const Line: Integer);
var
  StartPos, EndPos, EndStyledPos: Integer;
begin
  StartPos := GetPositionFromLine(Line);
  EndPos := GetPositionFromLine(Line + 1);
  { Back up the 'last styled position' if necessary }
  EndStyledPos := Call(SCI_GETENDSTYLED, 0, 0);
  if StartPos < EndStyledPos then
    Call(SCI_STARTSTYLING, StartPos, 0);
  StyleNeeded(EndPos);
end;

procedure TScintEdit.ScrollCaretIntoView;
begin
  Call(SCI_SCROLLCARET, 0, 0);
end;

function TScintEdit.SelAvail: Boolean;
var
  Sel: TScintRange;
begin
  Sel := GetSelection;
  Result := (Sel.EndPos > Sel.StartPos);
end;

procedure TScintEdit.SelectAll;
begin
  Call(SCI_SELECTALL, 0, 0);
end;

function TScintEdit.SelTextEquals(const S: String;
  const MatchCase: Boolean): Boolean;
begin
  Result := RawSelTextEquals(ConvertStringToRawString(S), MatchCase);
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
  CallStr(SCI_AUTOCSETFILLUPS, 0, FillupChars);
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
  CallStr(SCI_AUTOCSELECT, 0, S);
end;

procedure TScintEdit.SetAutoCompleteStopChars(const StopChars: AnsiString);
begin
  CallStr(SCI_AUTOCSTOPS, 0, StopChars);
end;

procedure TScintEdit.SetBraceHighlighting(const Pos1, Pos2: Integer);
begin
  Call(SCI_BRACEHIGHLIGHT, Pos1, Pos2);
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

procedure TScintEdit.SetCaretVirtualSpace(const Value: Integer);
var
  Pos, LineEndPos, MainSel: Integer;
begin
  { Weird things happen if a non-zero virtual space is set when the caret
    isn't at the end of a line, so don't allow it }
  Pos := GetCaretPosition;
  LineEndPos := GetLineEndPosition(GetLineFromPosition(Pos));
  if (Pos = LineEndPos) or (Value = 0) then begin
    MainSel := GetMainSelection;
    Call(SCI_SETSELECTIONNANCHORVIRTUALSPACE, MainSel, Value);
    Call(SCI_SETSELECTIONNCARETVIRTUALSPACE, MainSel, Value);
    ChooseCaretX;
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

procedure TScintEdit.SetEmptySelection;
{ Clears all selections without scrolling the caret into view }
var
  Pos: Integer;
begin
  Pos := GetCaretPosition;
  Call(SCI_SETSELECTION, Pos, Pos);
end;

procedure TScintEdit.SetFillSelectionToEdge(const Value: Boolean);
begin
  if FFillSelectionToEdge <> Value then begin
    FFillSelectionToEdge := Value;
    ApplyOptions;
  end;
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

procedure TScintEdit.SetRawSelText(const Value: TScintRawString);
begin
  Call(SCI_REPLACESEL, 0, LPARAM(PAnsiChar(Value)));
  ChooseCaretX;
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
begin
  Call(SCI_SETSEL, Value.StartPos, Value.EndPos);
  ChooseCaretX;
end;

procedure TScintEdit.SetSelText(const Value: String);
begin
  SetRawSelText(ConvertStringToRawString(Value));
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
  Call(SCI_AUTOCSHOW, CharsEntered, LPARAM(PAnsiChar(WordList)));
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

  function StyleLine(const FirstLine: Integer): Integer;
  var
    LastLine, I: Integer;
    OldState: TScintLineState;
  begin
    { Find final line in series of spanned lines }
    LastLine := FirstLine;
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

    FStyler.StyleNeeded;
    Call(SCI_SETSTYLINGEX, Length(FStyler.FStyleStr), LPARAM(PAnsiChar(FStyler.FStyleStr)));

    FStyler.FStyleStr := '';
    FStyler.FText := '';

    for I := FirstLine to LastLine do begin
      OldState := FLines.GetState(I);
      if FStyler.FLineState <> OldState then
        Call(SCI_SETLINESTATE, I, FStyler.FLineState);
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
    Call(SCI_SETSTYLINGEX, Length(StyleStr), LPARAM(PAnsiChar(StyleStr)));
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
    Call(SCI_STARTSTYLING, GetPositionFromLine(Line), $FF);
    if Assigned(FStyler) then
      Line := StyleLine(Line)
    else
      DefaultStyleLine(Line);
    Inc(Line);
  end;
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

procedure TScintEdit.UpdateStyleAttributes;
var
  DefaultAttr: TScintStyleAttributes; 

  procedure SetStyleAttr(const StyleNumber: Integer;
    const Attr: TScintStyleAttributes; const Force: Boolean);
  begin
    if Force or (Attr.FontName <> DefaultAttr.FontName) then
      CallStr(SCI_STYLESETFONT, StyleNumber, AnsiString(Attr.FontName));
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

const
  { Note: This style is specific to our patched build }
  STYLE_AUTOCOMPLETION = 39;
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

  if Assigned(FStyler) and FUseStyleAttributes then begin
    for I := 0 to 31 do
      SetStyleAttrFromStyler(I);
    SetStyleAttrFromStyler(STYLE_BRACELIGHT);
    SetStyleAttrFromStyler(STYLE_INDENTGUIDE);
  end;

  if AutoCompleteFontName <> '' then
    DefaultAttr.FontName := AutoCompleteFontName;
  if AutoCompleteFontSize > 0 then
    DefaultAttr.FontSize := AutoCompleteFontSize;
  DefaultAttr.FontStyle := [];
  { Note: Scintilla doesn't actually use the colors set here }
  DefaultAttr.ForeColor := clWindowText;
  DefaultAttr.BackColor := clWindow;
  SetStyleAttr(STYLE_AUTOCOMPLETION, DefaultAttr, True);
end;

function TScintEdit.WordAtCursor: String;
var
  Pos, StartPos, EndPos: Integer;
begin
  Pos := GetCaretPosition;
  StartPos := GetWordStartPosition(Pos, True);
  EndPos := GetWordEndPosition(Pos, True);
  Result := GetTextRange(StartPos, EndPos);
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
  inherited;
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
    Delphi 2009.) }
  Message.Result := CallWindowProc(DefWndProc, Handle, Message.Msg,
    Message.WParam, Message.LParam);
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

{$IFNDEF UNICODE}
class procedure TScintEditStrings.Error(Msg: PResStringRec; Data: Integer);
begin
  TList.Error(LoadResString(Msg), Data);
end;
{$ENDIF}

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
  FEdit.ReplaceRawTextRange(StartPos, EndPos, S);
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

procedure TScintCustomStyler.ApplyIndicators(const Indicators: TScintIndicatorNumbers;
  StartIndex, EndIndex: Integer);
var
  IndByte: Byte;
  I: Integer;
  P: PAnsiChar;
begin
  IndByte := Byte(Indicators) shl 5;
  if IndByte <> 0 then begin
    if StartIndex < 1 then
      StartIndex := 1;
    if EndIndex > FTextLen then
      EndIndex := FTextLen;
    { Note: The PAnsiChar stuff is to avoid UniqueString() on every iteration }
    P := @FStyleStr[1];
    for I := StartIndex to EndIndex do
      P[I-1] := AnsiChar(Ord(P[I-1]) or IndByte);
  end;
end;

procedure TScintCustomStyler.ApplyStyle(const Style: TScintStyleNumber;
  StartIndex, EndIndex: Integer);
const
  StyleMask = $1F;
var
  P: PAnsiChar;
  I: Integer;
begin
  if StartIndex < 1 then
    StartIndex := 1;
  if EndIndex > FTextLen then
    EndIndex := FTextLen;
  { Note: The PAnsiChar stuff is to avoid UniqueString() on every iteration }
  P := @FStyleStr[1];
  for I := StartIndex to EndIndex do
    if Ord(P[I-1]) and StyleMask = 0 then
      P[I-1] := AnsiChar(Style or (Ord(P[I-1]) and not StyleMask));
end;

procedure TScintCustomStyler.CommitStyle(const Style: TScintStyleNumber);
begin
  ApplyStyle(Style, FStyleStartIndex, FCurIndex - 1);
  FStyleStartIndex := FCurIndex;
end;

function TScintCustomStyler.ConsumeAllRemaining: Boolean;
begin
  Result := (FCurIndex <= FTextLen);
  if Result then
    FCurIndex := FTextLen + 1;
end;

function TScintCustomStyler.ConsumeChar(const C: AnsiChar): Boolean;
begin
  Result := (FCurIndex <= FTextLen) and (FText[FCurIndex] = C);
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
  Result := (FCurIndex > FTextLen);
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

end.
