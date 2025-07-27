unit BitmapButton;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  A TImage-like component for bitmaps without the TPicture bloat and
  which is actually a button with a focus rectangle when focused - in
  other words: an accessible TImage
  
  Make sure you set the Caption property even if it isn't visible

  Also see TBitmapImage which is the TGraphicControl version
}

interface

uses
  Windows, Messages, Controls, Graphics, Classes;

type
  TPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; var ARect: TRect) of object;

  TBitmapButton = class(TCustomControl)
  private
    FAutoSize: Boolean;
    FBackColor: TColor;
    FBitmap: TBitmap;
    FCenter: Boolean;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnPaint: TPaintEvent;
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
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
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
    property BackColor: TColor read FBackColor write SetBackColor default clNone;
    property Caption;
    property Center: Boolean read FCenter write SetCenter default False;
    property Enabled;
    property ParentShowHint;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property PopupMenu;
    property ShowHint;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property ReplaceColor: TColor read FReplaceColor write SetReplaceColor default clNone;
    property ReplaceWithColor: TColor read FReplaceWithColor write SetReplaceWithColor default clNone;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
  end;

procedure Register;

implementation

uses
  Math, Resample;

procedure Register;
begin
  RegisterComponents('JR', [TBitmapButton]);
end;

function TBitmapButton.InitializeFromIcon(const Instance: HINST; const Name: PChar; const BkColor: TColor; const AscendingTrySizes: array of Integer): Boolean;
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

constructor TBitmapButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable];
  FBackColor := clNone;
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := BitmapChanged;
  FReplaceColor := clNone;
  FReplaceWithColor := clNone;
  FStretchedBitmap := TBitmap.Create;
  TabStop := True;
  Height := 105;
  Width := 105;
end;

procedure TBitmapButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'BUTTON');
end;

destructor TBitmapButton.Destroy;
begin
  FStretchedBitmap.Free;
  FBitmap.Free;
  inherited Destroy;
end;

procedure TBitmapButton.BitmapChanged(Sender: TObject);
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

procedure TBitmapButton.SetAutoSize(Value: Boolean);
begin
  FAutoSize := Value;
  BitmapChanged(Self);
end;

procedure TBitmapButton.SetBackColor(Value: TColor);
begin
  if FBackColor <> Value then begin
    FBackColor := Value;
    BitmapChanged(Self);
  end;
end;

procedure TBitmapButton.SetBitmap(Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TBitmapButton.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then begin
    FCenter := Value;
    BitmapChanged(Self);
  end;
end;

procedure TBitmapButton.SetReplaceColor(Value: TColor);
begin
  if FReplaceColor <> Value then begin
    FReplaceColor := Value;
    BitmapChanged(Self);
  end;
end;

procedure TBitmapButton.SetReplaceWithColor(Value: TColor);
begin
  if FReplaceWithColor <> Value then begin
    FReplaceWithColor := Value;
    BitmapChanged(Self);
  end;
end;

procedure TBitmapButton.SetStretch(Value: Boolean);
begin
  if FStretch <> Value then begin
    FStretch := Value;
    FStretchedBitmap.Assign(nil);
    BitmapChanged(Self);
  end;
end;

function TBitmapButton.GetPalette: HPALETTE;
begin
  Result := FBitmap.Palette;
end;

procedure TBitmapButton.Paint;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  
  var R := ClientRect;

  if Focused then begin
    { See TBitBtn.DrawItem in Vcl.Buttons.pas }
    Canvas.Pen.Color := clWindowFrame;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.DrawFocusRect(R);
  end;

  { Note: On Windows 11 the focus rectangle border is always 2 pixels wide / high, even at 200% DPI }
  var FocusBorderWidth: UINT := 2;
  var FocusBorderHeight: UINT := 2;
  SystemParametersInfo(SPI_GETFOCUSBORDERWIDTH, 0, @FocusBorderWidth, 0);
  SystemParametersInfo(SPI_GETFOCUSBORDERHEIGHT, 0, @FocusBorderHeight, 0);

  InflateRect(R, -FocusBorderWidth, -FocusBorderHeight);

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

  if Assigned(FOnPaint) then
    FOnPaint(Self, Canvas, R);
end;

procedure TBitmapButton.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TBitmapButton.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TBitmapButton.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = BN_CLICKED) and Assigned(FOnClick) then
    FOnClick(Self)
  else if (Message.NotifyCode = BN_DBLCLK) and Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

end.