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
  TPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; var ARect: TRect) of object;

  TBitmapImageImplementation = record
  private
    FControl: TControl;
  public
    AutoSize: Boolean;
    AutoSizeExtraWidth, AutoSizeExtraHeight: Integer;
    BackColor: TColor;
    Bitmap: TBitmap;
    Center: Boolean;
    ReplaceColor: TColor;
    ReplaceWithColor: TColor;
    Stretch: Boolean;
    StretchedBitmap: TBitmap;
    StretchedBitmapValid: Boolean;
    OnPaint: TPaintEvent;
    procedure Init(const AControl: TControl; const AAutoSizeExtraWidth: Integer = 0;
      const AAutoSizeExtraHeight: Integer = 0);
    procedure DeInit;
    function InitializeFromIcon(const Instance: HINST; const Name: PChar; const BkColor: TColor; const AscendingTrySizes: array of Integer): Boolean;
    procedure BitmapChanged(Sender: TObject);
    procedure SetAutoSize(Sender: TObject; Value: Boolean);
    procedure SetBackColor(Sender: TObject; Value: TColor);
    procedure SetBitmap(Value: TBitmap);
    procedure SetCenter(Sender: TObject; Value: Boolean);
    procedure SetReplaceColor(Sender: TObject; Value: TColor);
    procedure SetReplaceWithColor(Sender: TObject; Value: TColor);
    procedure SetStretch(Sender: TObject; Value: Boolean);
    function GetPalette: HPALETTE;
    procedure Paint(const Sender: TObject; const Canvas: TCanvas; var R: TRect);
  end;

  TBitmapImage = class(TGraphicControl)
  private
    FImpl: TBitmapImageImplementation;
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
    property AutoSize: Boolean read FImpl.AutoSize write SetAutoSize default False;
    property BackColor: TColor read FImpl.BackColor write SetBackColor default clBtnFace;
    property Center: Boolean read FImpl.Center write SetCenter default False;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property Bitmap: TBitmap read FImpl.Bitmap write SetBitmap;
    property PopupMenu;
    property ShowHint;
    property Stretch: Boolean read FImpl.Stretch write SetStretch default False;
    property ReplaceColor: TColor read FImpl.ReplaceColor write SetReplaceColor default clNone;
    property ReplaceWithColor: TColor read FImpl.ReplaceWithColor write SetReplaceWithColor default clNone;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TPaintEvent read FImpl.OnPaint write FImpl.OnPaint;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses
  SysUtils, Math, Resample;

procedure Register;
begin
  RegisterComponents('JR', [TBitmapImage]);
end;

{ TBitmapImageImplementation }

procedure TBitmapImageImplementation.Init(const AControl: TControl;
  const AAutoSizeExtraWidth, AAutoSizeExtraHeight: Integer);
begin
  FControl := AControl;
  AutoSizeExtraWidth := AAutoSizeExtraWidth;
  AutoSizeExtraHeight := AAutoSizeExtraHeight;
  Bitmap := TBitmap.Create;
  Bitmap.OnChange := BitmapChanged;
  BackColor := clNone;
  ReplaceColor := clNone;
  ReplaceWithColor := clNone;
  StretchedBitmap := TBitmap.Create;
end;

procedure TBitmapImageImplementation.DeInit;
begin
  FreeAndNil(StretchedBitmap);
  FreeAndNil(Bitmap);
end;

function TBitmapImageImplementation.InitializeFromIcon(const Instance: HINST; const Name: PChar; const BkColor: TColor; const AscendingTrySizes: array of Integer): Boolean;
begin
  { Find the largest regular icon size smaller than the scaled image }
  var Size := 0;
  for var I := Length(AscendingTrySizes)-1 downto 0 do begin
    if (FControl.Width >= AscendingTrySizes[I]) and (FControl.Height >= AscendingTrySizes[I]) then begin
      Size := AscendingTrySizes[I];
      Break;
    end;
  end;
  if Size = 0 then
    Size := Min(FControl.Width, FControl.Height);

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
      FControl.Width := Icon.Width;
      FControl.Height := Icon.Height;

      { Draw icon into bitmap }
      Bitmap.Canvas.Brush.Color := BkColor;
      Bitmap.Width := FControl.Width;
      Bitmap.Height := FControl.Height;
      Bitmap.Canvas.Draw(0, 0, Icon);

      Result := True;
    finally
      Icon.Free;
    end;
  end else
    Result := False;
end;

procedure TBitmapImageImplementation.BitmapChanged(Sender: TObject);
begin
  StretchedBitmapValid := False;
  if AutoSize and (Bitmap.Width > 0) and (Bitmap.Height > 0) then
    FControl.SetBounds(FControl.Left, FControl.Top, Bitmap.Width + AutoSizeExtraWidth,
      Bitmap.Height + AutoSizeExtraHeight);
  FControl.Invalidate;
end;

procedure TBitmapImageImplementation.SetAutoSize(Sender: TObject; Value: Boolean);
begin
  AutoSize := Value;
  BitmapChanged(Sender);
end;

procedure TBitmapImageImplementation.SetBackColor(Sender: TObject; Value: TColor);
begin
  if BackColor <> Value then begin
    BackColor := Value;
    BitmapChanged(Sender);
  end;
end;

procedure TBitmapImageImplementation.SetBitmap(Value: TBitmap);
begin
  Bitmap.Assign(Value);
end;

procedure TBitmapImageImplementation.SetCenter(Sender: TObject; Value: Boolean);
begin
  if Center <> Value then begin
    Center := Value;
    BitmapChanged(Sender);
  end;
end;

procedure TBitmapImageImplementation.SetReplaceColor(Sender: TObject; Value: TColor);
begin
  if ReplaceColor <> Value then begin
    ReplaceColor := Value;
    BitmapChanged(Sender);
  end;
end;

procedure TBitmapImageImplementation.SetReplaceWithColor(Sender: TObject; Value: TColor);
begin
  if ReplaceWithColor <> Value then begin
    ReplaceWithColor := Value;
    BitmapChanged(Sender);
  end;
end;

procedure TBitmapImageImplementation.SetStretch(Sender: TObject; Value: Boolean);
begin
  if Stretch <> Value then begin
    Stretch := Value;
    StretchedBitmap.Assign(nil);
    BitmapChanged(Sender);
  end;
end;

function TBitmapImageImplementation.GetPalette: HPALETTE;
begin
  Result := Bitmap.Palette;
end;

procedure TBitmapImageImplementation.Paint(const Sender: TObject; const Canvas: TCanvas; var R: TRect);
begin
  const Is32bit = Bitmap.SupportsPartialTransparency;

  var W, H: Integer;
  var Bmp: TBitmap;
  if Stretch then begin
    W := R.Width;
    H := R.Height;
    Bmp := StretchedBitmap;
    if not StretchedBitmapValid or (StretchedBitmap.Width <> W) or
        (StretchedBitmap.Height <> H) then begin
      StretchedBitmapValid := True;
      if (Bitmap.Width = W) and (Bitmap.Height = H) then
        StretchedBitmap.Assign(Bitmap)
      else begin
        StretchedBitmap.Assign(nil);
        if not StretchBmp(Bitmap, StretchedBitmap, W, H, Is32bit) then begin
          if Is32bit then begin
            StretchedBitmapValid := False;
            Bmp := Bitmap;
          end else begin
            StretchedBitmap.Palette := CopyPalette(Bitmap.Palette);
            StretchedBitmap.Width := W;
            StretchedBitmap.Height := H;
            StretchedBitmap.Canvas.StretchDraw(Rect(0, 0, W, H), Bitmap);
          end;
        end;
      end;
    end;
  end else begin
    Bmp := Bitmap;
    W := Bmp.Width;
    H := Bmp.Height;
  end;

  if (BackColor <> clNone) and (Is32Bit or (Bmp.Width < FControl.Width) or (Bmp.Height < FControl.Height)) then begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := BackColor;
    Canvas.FillRect(R);
  end;

  if csDesigning in FControl.ComponentState then begin
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, FControl.Width, FControl.Height);
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

  if not Is32bit and (ReplaceColor <> clNone) and (ReplaceWithColor <> clNone) then begin
    Canvas.Brush.Color := ReplaceWithColor;
    Canvas.BrushCopy(Rect(X, Y, X + W, Y + H), Bmp, Rect(0, 0, Bmp.Width, Bmp.Height), ReplaceColor);
  end else
    Canvas.Draw(X, Y, Bmp);

  if Assigned(OnPaint) then
    OnPaint(Sender, Canvas, R);
end;

{ TBitmapImage }

constructor TBitmapImage.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csParentBackground, csReplicatable];
  FImpl.Init(Self);
  FImpl.BackColor := clBtnFace;
  Width := 105;
  Height := 105;
end;

destructor TBitmapImage.Destroy;
begin
  FImpl.DeInit;
  inherited;
end;

function TBitmapImage.InitializeFromIcon(const Instance: HINST; const Name: PChar; const BkColor: TColor; const AscendingTrySizes: array of Integer): Boolean;
begin
  Result := FImpl.InitializeFromIcon(HInstance, Name, BkColor, AscendingTrySizes);
end;

procedure TBitmapImage.SetAutoSize(Value: Boolean);
begin
  FImpl.SetAutoSize(Self, Value);
end;

procedure TBitmapImage.SetBackColor(Value: TColor);
begin
  FImpl.SetBackColor(Self, Value);
end;

procedure TBitmapImage.SetBitmap(Value: TBitmap);
begin
  FImpl.SetBitmap(Value);
end;

procedure TBitmapImage.SetCenter(Value: Boolean);
begin
  FImpl.SetCenter(Self, Value);
end;

procedure TBitmapImage.SetReplaceColor(Value: TColor);
begin
  FImpl.SetReplaceColor(Self, Value);
end;

procedure TBitmapImage.SetReplaceWithColor(Value: TColor);
begin
  FImpl.SetReplaceWithColor(Self, Value);
end;

procedure TBitmapImage.SetStretch(Value: Boolean);
begin
  FImpl.SetStretch(Self, Value);
end;

function TBitmapImage.GetPalette: HPALETTE;
begin
  Result := FImpl.GetPalette;
end;

procedure TBitmapImage.Paint;
begin
  var R := ClientRect;
  FImpl.Paint(Self, Canvas, R);
end;

end.