unit BitmapImage;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  A TImage-like component for bitmaps without the TPicture bloat

  Also see TBitmapButton which is the TWinControl version
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
    procedure SetAutoSize(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InitializeFromIcon(const Instance: HINST; const Name: PChar; const BkColor: TColor; const AscendingTrySizes: array of Integer): Boolean;
  published
    property Align;
    property Anchors;
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

uses
  Math, Resample;

procedure Register;
begin
  RegisterComponents('JR', [TBitmapImage]);
end;

function TBitmapImage.InitializeFromIcon(const Instance: HINST; const Name: PChar; const BkColor: TColor; const AscendingTrySizes: array of Integer): Boolean;
begin
  { Find the largest regular icon size smaller than the scaled image }
  var Size := 0;
  for var I := Length(AscendingTrySizes)-1 downto 0 do begin
    if (Width >= AscendingTrySizes[I]) and (Height >= AscendingTrySizes[I]) then begin
      Size := AscendingTrySizes[I];
      Break;
    end;
  end;
  if Size = 0 then
    Size := Min(Width, Height);

  { Load the desired icon }
  var Flags := LR_DEFAULTCOLOR;
  if Instance = 0 then
    Flags := Flags or LR_LOADFROMFILE;
  var Handle := LoadImage(Instance, Name, IMAGE_ICON, Size, Size, Flags);
  if Handle = 0 then
    Handle := LoadImage(Instance, Name, IMAGE_ICON, 0, 0, Flags);
  if Handle <> 0 then begin
    const Icon = TIcon.Create;
    try
      Icon.Handle := Handle;

      { Set sizes (overrides any scaling) }
      Width := Icon.Width;
      Height := Icon.Height;

      { Draw icon into bitmap }
      Bitmap.Canvas.Brush.Color := BkColor;
      Bitmap.Width := Width;
      Bitmap.Height := Height;
      Bitmap.Canvas.Draw(0, 0, Icon);

      Result := True;
    finally
      Icon.Free;
    end;
  end else
    Result := False;
end;

constructor TBitmapImage.Create(AOwner: TComponent);
begin
  inherited;
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
    ControlStyle := ControlStyle + [csOpaque] - [csParentBackground]
  else
    ControlStyle := ControlStyle - [csOpaque] + [csParentBackground];
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
begin
  var R := ClientRect;
  const Is32bit = (FBitmap.PixelFormat = pf32bit) and
    (FBitmap.AlphaFormat in [afDefined, afPremultiplied]);

  var W, H: Integer;
  var Bmp: TBitmap;
  if Stretch then begin
    W := R.Width;
    H := R.Height;
    Bmp := FStretchedBitmap;
    if not FStretchedBitmapValid or (FStretchedBitmap.Width <> W) or
        (FStretchedBitmap.Height <> H) then begin
      FStretchedBitmapValid := True;
      if (FBitmap.Width = W) and (FBitmap.Height = H) then
        FStretchedBitmap.Assign(FBitmap)
      else begin
        FStretchedBitmap.Assign(nil);
        if not StretchBmp(FBitmap, FStretchedBitmap, W, H, Is32bit) then begin
          if Is32bit then begin
            FStretchedBitmapValid := False;
            Bmp := FBitmap;
          end else begin
            FStretchedBitmap.Palette := CopyPalette(FBitmap.Palette);
            FStretchedBitmap.Width := W;
            FStretchedBitmap.Height := H;
            FStretchedBitmap.Canvas.StretchDraw(R, FBitmap);
          end;
        end;
      end;
    end;
  end else begin
    Bmp := FBitmap;
    W := Bmp.Width;
    H := Bmp.Height;
  end;

  if (FBackColor <> clNone) and (Is32Bit or (Bmp.Width < Width) or (Bmp.Height < Height)) then begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := FBackColor;
    Canvas.FillRect(R);
  end;

  if csDesigning in ComponentState then begin
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width, Height);
  end;

  var X := R.Left;
  var Y := R.Top;
  if Center then begin
    Inc(X, (R.Width - W) div 2);
    if X < 0 then
      X := 0;
    Inc(Y, (R.Height - H) div 2);
    if Y < 0 then
      Y := 0;
  end;

  if not Is32bit and (FReplaceColor <> clNone) and (FReplaceWithColor <> clNone) then begin
    Canvas.Brush.Color := FReplaceWithColor;
    Canvas.BrushCopy(Rect(X, Y, X + W, Y + H), Bmp, Rect(0, 0, Bmp.Width, Bmp.Height), FReplaceColor);
  end else
    Canvas.Draw(X, Y, Bmp);
end;

end.