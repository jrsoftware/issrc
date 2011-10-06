unit BitmapImage;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  A TImage-like component for bitmaps without the TPicture bloat

  $jrsoftware: issrc/Components/BitmapImage.pas,v 1.6 2009/03/23 14:57:40 mlaan Exp $
}

interface

uses
  Windows, Controls, Graphics, Classes;

type
  TBitmapImage = class(TGraphicControl)
  private
    FAutoSize: Boolean;
    FBackColor: TColor;
    FBitmap: TBitmap;
    FCenter: Boolean;
    FReplaceColor: TColor;
    FReplaceWithColor: TColor;
    FStretch: Boolean;
    FStretchedBitmap: TBitmap;
    FStretchedBitmapValid: Boolean;
    procedure BitmapChanged(Sender: TObject);
    procedure SetBackColor(Value: TColor);
    procedure SetBitmap(Value: TBitmap);
    procedure SetCenter(Value: Boolean);
    procedure SetReplaceColor(Value: TColor);
    procedure SetReplaceWithColor(Value: TColor);
    procedure SetStretch(Value: Boolean);
  protected
    function GetPalette: HPALETTE; override;
    procedure Paint; override;
    procedure SetAutoSize(Value: Boolean); {$IFDEF UNICODE}override;{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property BackColor: TColor read FBackColor write SetBackColor default clBtnFace;
    property Center: Boolean read FCenter write SetCenter default False;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property PopupMenu;
    property ShowHint;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property ReplaceColor: TColor read FReplaceColor write SetReplaceColor default clNone;
    property ReplaceWithColor: TColor read FReplaceWithColor write SetReplaceWithColor default clNone;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('JR', [TBitmapImage]);
end;

constructor TBitmapImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FBackColor := clBtnFace;
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := BitmapChanged;
  FReplaceColor := clNone;
  FReplaceWithColor := clNone;
  FStretchedBitmap := TBitmap.Create;
  Height := 105;
  Width := 105;
end;

destructor TBitmapImage.Destroy;
begin
  FStretchedBitmap.Free;
  FBitmap.Free;
  inherited Destroy;
end;

procedure TBitmapImage.BitmapChanged(Sender: TObject);
begin
  FStretchedBitmapValid := False;
  if FAutoSize and (FBitmap.Width > 0) and (FBitmap.Height > 0) then
    SetBounds(Left, Top, FBitmap.Width, FBitmap.Height);
  if (FBitmap.Width >= Width) and (FBitmap.Height >= Height) then
    ControlStyle := ControlStyle + [csOpaque]
  else
    ControlStyle := ControlStyle - [csOpaque];
  Invalidate;
end;

procedure TBitmapImage.SetAutoSize(Value: Boolean);
begin
  FAutoSize := Value;
  BitmapChanged(Self);
end;

procedure TBitmapImage.SetBackColor(Value: TColor);
begin
  if FBackColor <> Value then begin
    FBackColor := Value;
    BitmapChanged(Self);
  end;
end;

procedure TBitmapImage.SetBitmap(Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TBitmapImage.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then begin
    FCenter := Value;
    BitmapChanged(Self);
  end;
end;

procedure TBitmapImage.SetReplaceColor(Value: TColor);
begin
  if FReplaceColor <> Value then begin
    FReplaceColor := Value;
    BitmapChanged(Self);
  end;
end;

procedure TBitmapImage.SetReplaceWithColor(Value: TColor);
begin
  if FReplaceWithColor <> Value then begin
    FReplaceWithColor := Value;
    BitmapChanged(Self);
  end;
end;

procedure TBitmapImage.SetStretch(Value: Boolean);
begin
  if FStretch <> Value then begin
    FStretch := Value;
    FStretchedBitmap.Assign(nil);
    BitmapChanged(Self);
  end;
end;

function TBitmapImage.GetPalette: HPALETTE;
begin
  Result := FBitmap.Palette;
end;

procedure TBitmapImage.Paint;
var
  R: TRect;
  Bmp: TBitmap;
  X, Y: Integer;
begin
  with Canvas do begin
    R := ClientRect;

    if Stretch then begin
      if not FStretchedBitmapValid or (FStretchedBitmap.Width <> R.Right) or
         (FStretchedBitmap.Height <> R.Bottom) then begin
        FStretchedBitmapValid := True;
        if (FBitmap.Width = R.Right) and (FBitmap.Height = R.Bottom) then
          FStretchedBitmap.Assign(FBitmap)
        else begin
          FStretchedBitmap.Assign(nil);
          FStretchedBitmap.Palette := CopyPalette(FBitmap.Palette);
          FStretchedBitmap.Width := R.Right;
          FStretchedBitmap.Height := R.Bottom;
          FStretchedBitmap.Canvas.StretchDraw(R, FBitmap);
        end;
      end;
      Bmp := FStretchedBitmap;
    end
    else
      Bmp := FBitmap;

    if (FBackColor <> clNone) and (Bmp.Width < Width) or (Bmp.Height < Height) then begin
      Brush.Style := bsSolid;
      Brush.Color := FBackColor;
      FillRect(R);
    end;

    if csDesigning in ComponentState then begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

    if Center then begin
      X := R.Left + ((R.Right - R.Left) - Bmp.Width) div 2;
      if X < 0 then
        X := 0;
      Y := R.Top + ((R.Bottom - R.Top) - Bmp.Height) div 2;
      if Y < 0 then
        Y := 0;
    end else begin
      X := 0;
      Y := 0;
    end;

    if (FReplaceColor <> clNone) and (FReplaceWithColor <> clNone) then begin
      Brush.Color := FReplaceWithColor;
      BrushCopy(Rect(X, Y, X + Bmp.Width, Y + Bmp.Height), Bmp, Rect(0, 0, Bmp.Width, Bmp.Height), FReplaceColor);
    end else
      Draw(X, Y, Bmp);
  end;
end;

end.
