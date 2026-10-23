unit IDE.Minimap;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE Minimap
}

interface

uses
  Messages,
  Classes, Graphics,
  IDE.IDEScintEdit;

type
  TMinimap = class(TIDEScintEdit)
  private
    FGapWidth: Integer;
    procedure SetGapWidth(const Value: Integer);
  protected
    procedure CreateWnd; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateFont(const AFont: TFont);
    property GapWidth: Integer read FGapWidth write SetGapWidth;
  end;

implementation

uses
  Windows, ActiveX,
  ScintInt;

const
  MinimapFontSize = 2;

{ TMinimap }

constructor TMinimap.Create(AOwner: TComponent);
begin
  inherited;
  ShowHint := False;
  TabStop := False;
end;

procedure TMinimap.CreateWnd;
begin
  inherited;

  { Scintilla registers every window as a drop target, and a drop would insert
    text into the document }
  RevokeDragDrop(Handle);

  Call(SCI_SETVSCROLLBAR, 0, 0);
  Call(SCI_SETHSCROLLBAR, 0, 0);

  { The gap uses margin 2: TScintEdit.UpdateLineNumbersWidth sets margin 0's
    width, and margin 3's mask draws the folding markers. SC_MARGIN_BACK gives it
    the default style's background instead of STYLE_LINENUMBER's. }
  for var Margin := 0 to Call(SCI_GETMARGINS, 0, 0)-1 do
    Call(SCI_SETMARGINWIDTHN, Margin, 0);
  Call(SCI_SETMARGINTYPEN, mmChangeHistory, SC_MARGIN_BACK);
  Call(SCI_SETMARGINWIDTHN, mmChangeHistory, FGapWidth);
  Call(SCI_SETMARGINLEFT, 0, 0);
  Call(SCI_SETMARGINRIGHT, 0, 0);

  Call(SCI_SETCARETSTYLE, CARETSTYLE_INVISIBLE, 0);
  SetCursorID(SC_CURSORARROW);
  Call(SCI_SETMODEVENTMASK, 0, 0);
  Call(SCI_SETUNDOSELECTIONHISTORY, SC_UNDO_SELECTION_HISTORY_DISABLED, 0);
end;

procedure TMinimap.SetGapWidth(const Value: Integer);
begin
  if FGapWidth <> Value then begin
    FGapWidth := Value;
    if HandleAllocated then
      Call(SCI_SETMARGINWIDTHN, mmChangeHistory, FGapWidth);
  end;
end;

procedure TMinimap.UpdateFont(const AFont: TFont);
begin
  Font.Assign(AFont);
  Font.Size := MinimapFontSize;
end;

procedure TMinimap.WndProc(var Message: TMessage);
begin
  { Scintilla would take the focus, change its own selection, or scroll and zoom
    itself }
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK, WM_LBUTTONUP,
    WM_RBUTTONDOWN, WM_RBUTTONDBLCLK, WM_RBUTTONUP,
    WM_MBUTTONDOWN, WM_MBUTTONDBLCLK, WM_MBUTTONUP,
    WM_MOUSEWHEEL, WM_MOUSEHWHEEL:
      ;
  else
    inherited;
  end;
end;

end.
