unit CompScintEdit;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE's TScintEdit
}

interface

uses
  Windows, Graphics, Classes, Generics.Collections, ScintEdit, ModernColors;

const
  { Memo marker numbers }
  mmIconHasEntry = 0;        { grey dot }
  mmIconEntryProcessed = 1;  { green dot }
  mmIconBreakpoint = 2;      { stop sign }
  mmIconBreakpointGood = 3;  { stop sign + check }
  mmIconBreakpointBad = 4;   { stop sign + X }
  mmLineError = 10;          { maroon line highlight }
  mmLineBreakpointBad = 11;  { ugly olive line highlight }
  mmLineStep = 12;           { blue line highlight }
  mmIconStep = 13;           { blue arrow }
  mmIconBreakpointStep = 14; { blue arrow on top of a stop sign + check }

  { Memo indicator numbers (also in ScintStylerInnoSetup) }
  inSquiggly = 0;
  inPendingSquiggly = 1;

  { Just some invalid value used to indicate an unknown/uninitialized compiler FileIndex value }
  UnknownCompilerFileIndex = -2;

type
  TLineState = (lnUnknown, lnHasEntry, lnEntryProcessed);
  PLineStateArray = ^TLineStateArray;
  TLineStateArray = array[0..0] of TLineState;
  TSaveEncoding = (seAuto, seUTF8WithBOM, seUTF8WithoutBOM);

  TCompScintEdit = class(TScintEdit)
  private
    FTheme: TTheme;
    FOpeningFile: Boolean;
    FUsed: Boolean; { The IDE only shows 1 memo at a time so can't use .Visible to check if a memo is used }
  protected
    procedure CreateWnd; override;
  public
    property Theme: TTheme read FTheme write FTheme;
    property OpeningFile: Boolean read FOpeningFile write FOpeningFile;
    property Used: Boolean read FUsed write FUsed;
    procedure UpdateMemoMarkerColumnWidth(const AWidth: Integer);
    procedure UpdateThemeColorsAndStyleAttributes;
  end;

  TCompScintFileEdit = class(TCompScintEdit)
  private
    FBreakPoints: TList<Integer>;
    FCompilerFileIndex: Integer;
    FFilename: String;
    FFileLastWriteTime: TFileTime;
    FSaveEncoding: TSaveEncoding;
  public
    ErrorLine, ErrorCaretPosition: Integer;
    StepLine: Integer;
    LineState: PLineStateArray;
    LineStateCapacity, LineStateCount: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BreakPoints: TList<Integer> read FBreakPoints;
    property Filename: String read FFileName write FFilename;
    property CompilerFileIndex: Integer read FCompilerFileIndex write FCompilerFileIndex;
    property FileLastWriteTime: TFileTime read FFileLastWriteTime write FFileLastWriteTime;
    property SaveEncoding: TSaveEncoding read FSaveEncoding write FSaveEncoding;
  end;

  TCompScintEditNavItem = record
    Memo: TCompScintEdit;
    Line, Column, VirtualSpace: Integer;
    constructor Create(const AMemo: TCompScintEdit);
    function EqualMemoAndLine(const ANavItem: TCompScintEditNavItem): Boolean;
    procedure Invalidate;
    function Valid: Boolean;
  end;

  { Not using TStack since it lacks a way the keep a maximum amount of items by discarding the oldest }
  TCompScintEditNavStack = class(TList<TCompScintEditNavItem>)
  public
    function LinesDeleted(const AMemo: TCompScintEdit; const FirstLine, LineCount: Integer): Boolean;
    procedure LinesInserted(const AMemo: TCompScintEdit; const FirstLine, LineCount: Integer);
    procedure Optimize;
    function RemoveMemo(const AMemo: TCompScintEdit): Boolean;
    function RemoveMemoBadLines(const AMemo: TCompScintEdit): Boolean;
  end;

  TCompScintEditNavStacks = class
  private
    FBackNavStack: TCompScintEditNavStack;
    FForwardNavStack: TCompScintEditNavStack;
  public
    constructor Create;
    destructor Destroy; override;
    function AddNewBackForJump(const OldNavItem, NewNavItem: TCompScintEditNavItem): Boolean;
    procedure Clear;
    procedure Limit;
    function LinesDeleted(const AMemo: TCompScintEdit; const FirstLine, LineCount: Integer): Boolean;
    procedure LinesInserted(const AMemo: TCompScintEdit; const FirstLine, LineCount: Integer);
    function RemoveMemo(const AMemo: TCompScintEdit): Boolean;
    function RemoveMemoBadLines(const AMemo: TCompScintEdit): Boolean;
    property Back: TCompScintEditNavStack read FBackNavStack;
    property Forward: TCompScintEditNavStack read FForwardNavStack;
  end;

implementation

uses
  ScintInt;
  
{ TCompScintEdit }

procedure TCompScintEdit.CreateWnd;
const
  SC_MARK_BACKFORE = 3030;  { new marker type added in Inno Setup's Scintilla build }
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
  Call(SCI_INDICSETFORE, inSquiggly, clRed); { May be overwritten by UpdateThemeColorsAndStyleAttributes }
  Call(SCI_INDICSETSTYLE, inPendingSquiggly, INDIC_HIDDEN);

  { Set up the gutter column with breakpoint etc symbols - note: column 0 is the
    line numbers column and its width is set up by TScintEdit.UpdateLineNumbersWidth }
  Call(SCI_SETMARGINTYPEN, 1, SC_MARGIN_SYMBOL);
  Call(SCI_SETMARGINSENSITIVEN, 1, 1); { Makes it react to mouse clicks }
  Call(SCI_SETMARGINCURSORN, 1, SC_CURSORARROW);

  { Set 2 pixel margin between gutter and the main text - note: the first
    parameter is unused so the value '0' doesn't mean anything below }
  Call(SCI_SETMARGINLEFT, 0, 2);

  Call(SCI_MARKERDEFINE, mmLineError, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineError, clWhite);
  Call(SCI_MARKERSETBACK, mmLineError, clMaroon);
  Call(SCI_MARKERDEFINE, mmLineBreakpointBad, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineBreakpointBad, clLime);
  Call(SCI_MARKERSETBACK, mmLineBreakpointBad, clOlive);
  Call(SCI_MARKERDEFINE, mmLineStep, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineStep, clWhite);
  Call(SCI_MARKERSETBACK, mmLineStep, clBlue); { May be overwritten by UpdateThemeColorsAndStyleAttributes }
end;

procedure TCompScintEdit.UpdateMemoMarkerColumnWidth(const AWidth: Integer);
begin
  Call(SCI_SETMARGINWIDTHN, 1, AWidth);
end;

procedure TCompScintEdit.UpdateThemeColorsAndStyleAttributes;
begin
  if FTheme <> nil then begin
    Font.Color := FTheme.Colors[tcFore];
    Color := FTheme.Colors[tcBack];
    Call(SCI_SETSELBACK, 1, FTheme.Colors[tcSelBack]);
    Call(SCI_INDICSETFORE, inSquiggly, FTheme.Colors[tcRed]);
    Call(SCI_MARKERSETBACK, mmLineStep, FTheme.Colors[tcBlue]);
  end;
  UpdateStyleAttributes;
end;

{ TCompScintFileEdit }

constructor TCompScintFileEdit.Create;
begin
  inherited;
  FBreakPoints := TList<Integer>.Create;
end;

destructor TCompScintFileEdit.Destroy;
begin
  FBreakPoints.Free;
  inherited;
end;

{ TCompScintEditNavItem }

constructor TCompScintEditNavItem.Create(const AMemo: TCompScintEdit);
begin
  Memo := AMemo;
  Line := AMemo.CaretLine;
  Column := AMemo.CaretColumn;
  VirtualSpace := AMemo.CaretVirtualSpace;
end;

function TCompScintEditNavItem.EqualMemoAndLine(
  const ANavItem: TCompScintEditNavItem): Boolean;
begin
  Result := (Memo = ANavItem.Memo) and (Line = ANavItem.Line);
end;

procedure TCompScintEditNavItem.Invalidate;
begin
  Memo := nil;
end;

function TCompScintEditNavItem.Valid: Boolean;
begin
  Result := (Memo <> nil) and (Line < Memo.Lines.Count); { Line check: see MemoLinesDeleted and RemoveMemoBadLinesFromNav }
end;

{ TCompScintEditNavStack }

function TCompScintEditNavStack.LinesDeleted(const AMemo: TCompScintEdit;
  const FirstLine, LineCount: Integer): Boolean;
begin
  Result := False;
  for var I := Count-1 downto 0 do begin
    var NavItem := Items[I];
    if NavItem.Memo = AMemo then begin
      var Line := NavItem.Line;
      if Line >= FirstLine then begin
        if Line < FirstLine + LineCount then begin
          Delete(I);
          Result := True;
        end else begin
          NavItem.Line := Line - LineCount;
          Items[I] := NavItem;
        end;
      end;
    end;
  end;
  if Result then
    Optimize;
end;

procedure TCompScintEditNavStack.LinesInserted(const AMemo: TCompScintEdit;
  const FirstLine, LineCount: Integer);
begin
  for var I := 0 to Count-1 do begin
    var NavItem := Items[I];
    if NavItem.Memo = AMemo then begin
      var Line := NavItem.Line;
      if Line >= FirstLine then begin
        NavItem.Line := Line + LineCount;
        Items[I] := NavItem;
      end;
    end;
  end;
end;

procedure TCompScintEditNavStack.Optimize;
begin
  { Turn two entries for the same memo and line which are next to each other
    into one entry, ignoring column differences (like Visual Studio 2022) }
  for var I := Count-1 downto 1 do
    if Items[I].EqualMemoAndLine(Items[I-1]) then
      Delete(I);
end;

function TCompScintEditNavStack.RemoveMemo(
  const AMemo: TCompScintEdit): Boolean;
begin
  Result := False;
  for var I := Count-1 downto 0 do begin
    if Items[I].Memo = AMemo then begin
      Delete(I);
      Result := True;
    end;
  end;
  if Result then
    Optimize;
end;

function TCompScintEditNavStack.RemoveMemoBadLines(
  const AMemo: TCompScintEdit): Boolean;
begin
  Result := False;
  var LastGoodLine := AMemo.Lines.Count-1;
  for var I := Count-1 downto 0 do begin
    if (Items[I].Memo = AMemo) and (Items[I].Line > LastGoodLine) then begin
      Delete(I);
      Result := True;
    end;
  end;
  if Result then
    Optimize;
end;

{ TCompScintEditNavStacks }

constructor TCompScintEditNavStacks.Create;
begin
  inherited;
  FBackNavStack := TCompScintEditNavStack.Create;
  FForwardNavStack := TCompScintEditNavStack.Create;
end;

destructor TCompScintEditNavStacks.Destroy;
begin
  FForwardNavStack.Free;
  FBackNavStack.Free;
  inherited;
end;

function TCompScintEditNavStacks.AddNewBackForJump(const OldNavItem,
  NewNavItem: TCompScintEditNavItem): Boolean;
begin
  { Want a new item when changing tabs or moving at least 11 lines at once,
    similar to Visual Studio 2022, see:
    https://learn.microsoft.com/en-us/archive/blogs/zainnab/navigate-backward-and-navigate-forward
    Note: not doing the other stuff listed in the article atm }
  Result := (OldNavItem.Memo <> NewNavItem.Memo) or
            (Abs(OldNavItem.Line - NewNavItem.Line) >= 11);
  if Result then begin
    FBackNavStack.Add(OldNavItem);
    Limit;
  end;
end;

procedure TCompScintEditNavStacks.Clear;
begin
  FBackNavStack.Clear;
  FForwardNavStack.Clear;
end;

procedure TCompScintEditNavStacks.Limit;
begin
  { The dropdown showing both stacks + the current nav item should show at most
    16 items just like Visual Studio 2022 }
  if FBackNavStack.Count + FForwardNavStack.Count >= 15 then
    FBackNavStack.Delete(0);
end;

function TCompScintEditNavStacks.LinesDeleted(const AMemo: TCompScintEdit;
  const FirstLine, LineCount: Integer): Boolean;
begin
  Result := FBackNavStack.LinesDeleted(AMemo, FirstLine, LineCount);
  Result := FForwardNavStack.LinesDeleted(AMemo, FirstLine, LineCount) or Result;
end;

procedure TCompScintEditNavStacks.LinesInserted(const AMemo: TCompScintEdit;
  const FirstLine, LineCount: Integer);
begin
  FBackNavStack.LinesInserted(AMemo, FirstLine, LineCount);
  FForwardNavStack.LinesInserted(AMemo, FirstLine, LineCount);
end;

function TCompScintEditNavStacks.RemoveMemo(
  const AMemo: TCompScintEdit): Boolean;
begin
  Result := FBackNavStack.RemoveMemo(AMemo);
  Result := FForwardNavStack.RemoveMemo(AMemo) or Result;
end;

function TCompScintEditNavStacks.RemoveMemoBadLines(
  const AMemo: TCompScintEdit): Boolean;
begin
  Result := FBackNavStack.RemoveMemoBadLines(AMemo);
  Result := FForwardNavStack.RemoveMemoBadLines(AMemo) or Result;
end;

end.