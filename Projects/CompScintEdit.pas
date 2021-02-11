unit CompScintEdit;

{
  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
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
  mmLineError = 10;          { red line highlight }
  mmLineBreakpoint = 11;     { red line highlight }
  mmLineBreakpointBad = 12;  { ugly olive line highlight }
  mmLineStep = 13;           { blue line highlight }

  { Memo indicator numbers (also in ScintStylerInnoSetup) }
  inSquiggly = 0;
  inPendingSquiggly = 1;

  { Just some invalid value used to indicate an unknown/uninitialized compiler FileIndex value }
  UnknownCompilerFileIndex = -2;

type
  TLineState = (lnUnknown, lnHasEntry, lnEntryProcessed);
  PLineStateArray = ^TLineStateArray;
  TLineStateArray = array[0..0] of TLineState;

  TCompScintEdit = class(TScintEdit)
  private
    FTheme: TTheme;
    FUsed: Boolean;
  protected
    procedure CreateWnd; override;
  public
    property Theme: TTheme read FTheme write FTheme;
    property Used: Boolean read FUsed write FUsed;
    procedure UpdateThemeColorsAndStyleAttributes;
  end;

  TCompScintFileEdit = class(TCompScintEdit)
  private
    FBreakPoints: TList<Integer>;
    FCompilerFileIndex: Integer;
    FFilename: String;
    FFileLastWriteTime: TFileTime;
    FSaveInUTF8Encoding: Boolean;
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
    property SaveInUTF8Encoding: Boolean read FSaveInUTF8Encoding write FSaveInUTF8Encoding;
  end;

implementation

uses
  ScintInt;
  
{ TCompScintEdit }

procedure TCompScintEdit.CreateWnd;
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
  Call(SCI_INDICSETFORE, inSquiggly, clRed); { May be overwritten by UpdateThemeColors }
  Call(SCI_INDICSETSTYLE, inPendingSquiggly, INDIC_HIDDEN);

  Call(SCI_SETMARGINTYPEN, 1, SC_MARGIN_SYMBOL);
  Call(SCI_SETMARGINWIDTHN, 1, 21);
  Call(SCI_SETMARGINSENSITIVEN, 1, 1);
  Call(SCI_SETMARGINCURSORN, 1, SC_CURSORARROW);
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

procedure TCompScintEdit.UpdateThemeColorsAndStyleAttributes;
begin
  if FTheme <> nil then begin
    Font.Color := FTheme.Colors[tcFore];
    Color := FTheme.Colors[tcBack];
    Call(SCI_SETSELBACK, 1, FTheme.Colors[tcSelBack]);
    Call(SCI_INDICSETFORE, inSquiggly, FTheme.Colors[tcRed]);
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

end.