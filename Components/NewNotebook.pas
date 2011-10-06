unit NewNotebook;

{
  Inno Setup
  Copyright (C) 1997-2008 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TNewNotebook component

  $jrsoftware: issrc/Components/NewNotebook.pas,v 1.4 2008/10/08 23:23:02 jr Exp $
}

{$IFDEF VER90}
  {$DEFINE DELPHI2}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms;

type
  TNewNotebookPage = class;

  TNewNotebook = class(TWinControl)
  private
    FActivePage: TNewNotebookPage;
    FPages: TList;
    function GetPage(Index: Integer): TNewNotebookPage;
    function GetPageCount: Integer;
    procedure InsertPage(Page: TNewNotebookPage);
    procedure RemovePage(Page: TNewNotebookPage);
    procedure SetActivePage(Page: TNewNotebookPage);
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ShowControl(AControl: TControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindNextPage(CurPage: TNewNotebookPage; GoForward: Boolean): TNewNotebookPage;
    procedure GetChildren(Proc: TGetChildProc {$IFNDEF DELPHI2} ;
      Root: TComponent {$ENDIF}); override;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TNewNotebookPage read GetPage;
  published
    property ActivePage: TNewNotebookPage read FActivePage write SetActivePage;
    property Align;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TNewNotebookPage = class(TCustomControl)
  private
    FNotebook: TNewNotebook;
    function GetPageIndex: Integer;
    procedure SetNotebook(ANotebook: TNewNotebook);
    procedure SetPageIndex(Value: Integer);
  protected
    procedure Paint; override;
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Notebook: TNewNotebook read FNotebook write SetNotebook;
  published
    property Color nodefault;  { nodefault needed for Color=clWindow to persist }
    property DragMode;
    property Enabled;
    property Font;
    property Height stored False;
    property Left stored False;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Top stored False;
    property Visible stored False;
    property Width stored False;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

{ TNewNotebookPage }

constructor TNewNotebookPage.Create(AOwner: TComponent);
begin
  inherited;
  Align := alClient;
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
  Visible := False;
end;

destructor TNewNotebookPage.Destroy;
begin
  if Assigned(FNotebook) then
    FNotebook.RemovePage(Self);
  inherited;
end;

function TNewNotebookPage.GetPageIndex: Integer;
begin
  if Assigned(FNotebook) then
    Result := FNotebook.FPages.IndexOf(Self)
  else
    Result := -1;
end;

procedure TNewNotebookPage.Paint;
begin
  inherited;
  if csDesigning in ComponentState then begin
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

procedure TNewNotebookPage.ReadState(Reader: TReader);
begin
  inherited;
  if Reader.Parent is TNewNotebook then
    Notebook := TNewNotebook(Reader.Parent);
end;

procedure TNewNotebookPage.SetNotebook(ANotebook: TNewNotebook);
begin
  if FNotebook <> ANotebook then begin
    if Assigned(FNotebook) then
      FNotebook.RemovePage(Self);
    Parent := ANotebook;
    if Assigned(ANotebook) then
      ANotebook.InsertPage(Self);
  end;
end;

procedure TNewNotebookPage.SetPageIndex(Value: Integer);
begin
  if Assigned(FNotebook) then begin
    if Value >= FNotebook.FPages.Count then
      Value := FNotebook.FPages.Count-1;
    if Value < 0 then
      Value := 0;
    FNotebook.FPages.Move(PageIndex, Value);
  end;
end;

{ TNewNotebook }

constructor TNewNotebook.Create(AOwner: TComponent);
begin
  inherited;
  Width := 150;
  Height := 150;
  FPages := TList.Create;
end;

destructor TNewNotebook.Destroy;
var
  I: Integer;
begin
  if Assigned(FPages) then begin
    for I := 0 to FPages.Count-1 do
      TNewNotebookPage(FPages[I]).FNotebook := nil;
    FPages.Free;
  end;
  inherited;
end;

procedure TNewNotebook.AlignControls(AControl: TControl; var Rect: TRect);
var
  I: Integer;
  Ctl: TControl;
begin
  inherited;
  { The default AlignControls implementation in Delphi 2 and 3 doesn't set
    the size of invisible controls. Pages that aren't currently visible must
    have valid sizes for BidiUtils' FlipControls to work properly.
    Note: We loop through Controls and not FPages here because
    TNewNotebookPage.SetNotebook sets Parent (causing AlignControls to be
    called) before it calls InsertPage. }
  if not IsRectEmpty(Rect) then begin
    for I := 0 to ControlCount-1 do begin
      Ctl := Controls[I];
      if (Ctl is TNewNotebookPage) and not Ctl.Visible then
        Ctl.BoundsRect := Rect;
    end;
  end;
end;

procedure TNewNotebook.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

function TNewNotebook.FindNextPage(CurPage: TNewNotebookPage;
  GoForward: Boolean): TNewNotebookPage;
var
  I, StartIndex: Integer;
begin
  if FPages.Count > 0 then begin
    StartIndex := FPages.IndexOf(CurPage);
    if StartIndex = -1 then begin
      if GoForward then
        StartIndex := FPages.Count-1
      else
        StartIndex := 0;
    end;
    I := StartIndex;
    repeat
      if GoForward then begin
        Inc(I);
        if I = FPages.Count then
          I := 0;
      end
      else begin
        if I = 0 then
          I := FPages.Count;
        Dec(I);
      end;
      Result := FPages[I];
      Exit;
    until I = StartIndex;
  end;
  Result := nil;
end;

procedure TNewNotebook.GetChildren(Proc: TGetChildProc {$IFNDEF DELPHI2} ;
  Root: TComponent {$ENDIF});
var
  I: Integer;
begin
  for I := 0 to FPages.Count-1 do
    Proc(TNewNotebookPage(FPages[I]));
end;

function TNewNotebook.GetPage(Index: Integer): TNewNotebookPage;
begin
  Result := FPages[Index];
end;

function TNewNotebook.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TNewNotebook.InsertPage(Page: TNewNotebookPage);
begin
  FPages.Add(Page);
  Page.FNotebook := Self;
end;

procedure TNewNotebook.RemovePage(Page: TNewNotebookPage);
begin
  Page.FNotebook := nil;
  FPages.Remove(Page);
  if FActivePage = Page then
    SetActivePage(nil);
end;

procedure TNewNotebook.ShowControl(AControl: TControl);
begin
  if (AControl is TNewNotebookPage) and (TNewNotebookPage(AControl).FNotebook = Self) then
    SetActivePage(TNewNotebookPage(AControl));
  inherited;
end;

procedure TNewNotebook.SetActivePage(Page: TNewNotebookPage);
var
  ParentForm: {$IFDEF DELPHI2} TForm {$ELSE} TCustomForm {$ENDIF};
begin
  if Assigned(Page) and (Page.FNotebook <> Self) then
    Exit;
  if FActivePage <> Page then begin
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) and Assigned(FActivePage) and
       FActivePage.ContainsControl(ParentForm.ActiveControl) then
      ParentForm.ActiveControl := FActivePage;
    if Assigned(Page) then begin
      Page.BringToFront;
      Page.Visible := True;
      if Assigned(ParentForm) and Assigned(FActivePage) and
         (ParentForm.ActiveControl = FActivePage) then begin
        if Page.CanFocus then
          ParentForm.ActiveControl := Page
        else
          ParentForm.ActiveControl := Self;
      end;
    end;
    if Assigned(FActivePage) then
      FActivePage.Visible := False;
    FActivePage := Page;
    if Assigned(ParentForm) and Assigned(FActivePage) and
       (ParentForm.ActiveControl = FActivePage) then
      FActivePage.SelectFirst;
  end;
end;

end.
