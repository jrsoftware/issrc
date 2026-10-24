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
  ScintInt,
  IDE.IDEScintEdit;

type
  TMinimap = class(TIDEScintEdit)
  private
    FGutterWidth: Integer;
    FMemo: TIDEScintEdit;
    procedure SetDocPointer;
    procedure SetGutterWidth(const Value: Integer);
  protected
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Notify(const N: TSCNotification); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetMemo(const AMemo: TIDEScintEdit);
    procedure UpdateFont(const AFont: TFont);
    procedure UpdateTheme;
    property GutterWidth: Integer read FGutterWidth write SetGutterWidth;
  end;

implementation

uses
  Windows, ActiveX;

const
  { VS Code's minimap with its default scale draws each character 1 device
    pixel wide below 200% scaling, and 2 at 200%. At 1 point, characters of
    this minimap have the same widths. See minimapScale and minimapCharWidth
    in EditorLayoutInfoComputer._computeMinimapLayout. }
  MinimapFontSize = 1;

{ TMinimap }

constructor TMinimap.Create(AOwner: TComponent);
begin
  inherited;
  ParentShowHint := False;
  ShowHint := False;
  TabStop := False;
end;

procedure TMinimap.CreateWnd;
begin
  inherited;

  { Scintilla registers every window as a drop target, and a drop would insert
    text into the document }
  RevokeDragDrop(Handle);

  { No blank margins, scrollbars or caret on the minimap }
  Call(SCI_SETMARGINLEFT, 0, 0);
  Call(SCI_SETMARGINRIGHT, 0, 0);
  Call(SCI_SETVSCROLLBAR, 0, 0);
  Call(SCI_SETHSCROLLBAR, 0, 0);
  Call(SCI_SETCARETSTYLE, CARETSTYLE_INVISIBLE, 0);

  { Regular cursor always }
  SetCursorID(SC_CURSORARROW);

  { Use the entire gutter for a single margin showing change history }
  for var Margin := 0 to Call(SCI_GETMARGINS, 0, 0)-1 do
    Call(SCI_SETMARGINWIDTHN, Margin, 0);
  Call(SCI_SETMARGINTYPEN, mmChangeHistory, SC_MARGIN_BACK);
  Call(SCI_SETMARGINWIDTHN, mmChangeHistory, FGutterWidth);

  { Ensure the icon markers stay invisible now that the minimap has no margin
    for them. Without this Scintilla would display them as line markers instead. }
  for var Marker := mmiFirst to mmiLast do
    Call(SCI_MARKERDEFINE, Marker, SC_MARK_EMPTY);

  { No change notifications or undo selection history, don't need them }
  Call(SCI_SETMODEVENTMASK, 0, 0);
  Call(SCI_SETUNDOSELECTIONHISTORY, SC_UNDO_SELECTION_HISTORY_DISABLED, 0);

  { Ensure document is shared }
  SetDocPointer;
end;

procedure TMinimap.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FMemo) then
    FMemo := nil;
end;

procedure TMinimap.Notify(const N: TSCNotification);
begin
  { Undo and redo make every view scroll its caret into view, also sideways.
    Detect and fix using SCN_UPDATEUI. Comes before the paint so doesn't
    cause flicker. }
  if (N.nmhdr.code = SCN_UPDATEUI) and (N.updated and SC_UPDATE_H_SCROLL <> 0) and
     (Call(SCI_GETXOFFSET, 0, 0) <> 0) then { Check because SCI_SETXOFFSET always repaints }
    Call(SCI_SETXOFFSET, 0, 0);
  inherited;
end;

procedure TMinimap.SetDocPointer;
begin
  if (FMemo <> nil) and HandleAllocated then begin
    { Not using FMemo.Call: its Integer result can't hold a pointer }
    const Document = SendMessage(FMemo.Handle, SCI_GETDOCPOINTER, 0, 0);
    Call(SCI_SETDOCPOINTER, 0, Document);
  end;
end;

procedure TMinimap.SetGutterWidth(const Value: Integer);
begin
  if FGutterWidth <> Value then begin
    FGutterWidth := Value;
    if HandleAllocated then
      Call(SCI_SETMARGINWIDTHN, mmChangeHistory, FGutterWidth);
  end;
end;

procedure TMinimap.SetMemo(const AMemo: TIDEScintEdit);
{ Always calls SetDocPointer, so resyncs always }
begin
  if FMemo <> AMemo then begin
    if FMemo <> nil then
      FMemo.RemoveFreeNotification(Self);
    FMemo := AMemo;
    if FMemo <> nil then
      FMemo.FreeNotification(Self);
  end;
  SetDocPointer;
end;

procedure TMinimap.UpdateFont(const AFont: TFont);
begin
  Font.Assign(AFont);
  Font.Size := MinimapFontSize;
end;

procedure TMinimap.UpdateTheme;
begin
  UpdateThemeColorsAndStyleAttributes;
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
