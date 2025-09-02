unit NewProgressBar;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TNewProgressBar component - a smooth 32 bit TProgressBar

  Note: themed animated progress bars don't immediately show changes.
  This applies both to Position and State. For example if you set State while the
  progress bar is still moving towards a new Position, the new State doesnt show until
  the moving animation has finished.

  Define VCLSTYLES for full VCL Styles support.
}

interface

uses
  Windows, Messages, Classes, Controls, ComCtrls,
  {$IFDEF VCLSTYLES} Vcl.Themes, {$ELSE} Themes, {$ENDIF}
  ExtCtrls, Types, Graphics;

type
  TNewProgressBarState = (npbsNormal, npbsError, npbsPaused);

  TNewProgressBarStyle = (npbstNormal, npbstMarquee);

  TNewProgressBar = class(TWinControl)
  private
    FMin: LongInt;
    FMax: LongInt;
    FPosition: LongInt;
    FState: TNewProgressBarState;
    FStyle: TNewProgressBarStyle;
    class constructor Create;
    class destructor Destroy;
    procedure SetMin(Value: LongInt);
    procedure SetMax(Value: LongInt);
    procedure SetPosition(Value: LongInt);
    procedure SetState(Value: TNewProgressBarState);
    procedure SetStyle(Value: TNewProgressBarStyle);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Anchors;
    property Min: LongInt read FMin write SetMin;
    property Max: LongInt read FMax write SetMax;
    property Position: LongInt read FPosition write SetPosition default 0;
    property State: TNewProgressBarState read FState write SetState default npbsNormal;
    property Style: TNewProgressBarStyle read FStyle write SetStyle default npbstMarquee;
    property Visible default True;
  end;

  TNewProgressBarStyleHook = class(TStyleHook)
{$IFDEF VCLSTYLES}
  strict private
    FMarqueeTimer: TTimer;
    FMarqueeStep: Integer;
    procedure MarqueeAction(Sender: TObject);
    function GetBarRect: TRect;
    function GetBorderWidth: Integer;
    function GetMax: Integer;
    function GetMin: Integer;
    function GetOrientation: TProgressBarOrientation;
    function GetPercent: Single;
    function GetPosition: Integer;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
  strict protected
    procedure PaintBar(Canvas: TCanvas); virtual;
    procedure PaintFrame(Canvas: TCanvas); virtual;
    procedure Paint(Canvas: TCanvas); override;
    property BarRect: TRect read GetBarRect;
    property BorderWidth: Integer read GetBorderWidth;
    property Max: Integer read GetMax;
    property Min: Integer read GetMin;
    property Orientation: TProgressBarOrientation read GetOrientation;
    property Position: Integer read GetPosition;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
{$ENDIF}
  end;

procedure Register;

implementation

uses
  CommCtrl, SysUtils, GraphUtil;

procedure Register;
begin
  RegisterComponents('JR', [TNewProgressBar]);
end;

{ TNewProgressBar }

class constructor TNewProgressBar.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TNewProgressBar, TNewProgressBarStyleHook);
end;

constructor TNewProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  Width := 150;
  Height := GetSystemMetrics(SM_CYVSCROLL);
  FMin := 0;
  FMax := 100;
end;

procedure TNewProgressBar.CreateParams(var Params: TCreateParams);
const
  PBS_SMOOTH = 1;
  PBS_MARQUEE = 8;
begin
  InitCommonControls;
  inherited;
  CreateSubClass(Params, PROGRESS_CLASS);
  Params.Style := Params.Style or PBS_SMOOTH;
  if Style = npbstMarquee then
    Params.Style := Params.Style or PBS_MARQUEE;
end;

procedure TNewProgressBar.CreateWnd;
const
  PBM_SETMARQUEE = WM_USER+10;
begin
  inherited CreateWnd;
  SendMessage(Handle, PBM_SETRANGE, 0, MAKELPARAM(0, 65535));
  SetPosition(FPosition);
  SetState(FState);
  SendMessage(Handle, PBM_SETMARQUEE, WPARAM(FStyle = npbstMarquee), 0);
end;

class destructor TNewProgressBar.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TNewProgressBar, TNewProgressBarStyleHook);
end;

procedure TNewProgressBar.SetMin(Value: LongInt);
begin
  FMin := Value;
  SetPosition(FPosition);
end;

procedure TNewProgressBar.SetMax(Value: LongInt);
begin
  FMax := Value;
  SetPosition(FPosition);
end;

procedure TNewProgressBar.SetPosition(Value: LongInt);
begin
  if Value < FMin then
    Value := FMin
  else if Value > FMax then
    Value := FMax;
  FPosition := Value;
  if HandleAllocated and (FStyle <> npbstMarquee) then
    SendMessage(Handle, PBM_SETPOS, MulDiv(Value - FMin, 65535, FMax - FMin), 0);
end;

procedure TNewProgressBar.SetState(Value: TNewProgressBarState);
const
  PBST_NORMAL = $0001;
  PBST_ERROR = $0002;
  PBST_PAUSED = $0003;
  PBM_SETSTATE = WM_USER+16;
  States: array[TNewProgressBarState] of UINT = (PBST_NORMAL, PBST_ERROR, PBST_PAUSED);
begin
  FState := Value;
  if HandleAllocated then
    SendMessage(Handle, PBM_SETSTATE, States[Value], 0);
end;

procedure TNewProgressBar.SetStyle(Value: TNewProgressBarStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    RecreateWnd;
  end;
end;

procedure TNewProgressBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  { Bypass TWinControl's default WM_ERASEBKGND handling.
    On Windows Vista with COMCTL32 v6, a WM_ERASEBKGND message is sent every
    time a progress bar's position changes. TWinControl.WMEraseBkgnd does a
    FillRect on the whole client area, which results in ugly flickering.
    Previous versions of Windows only sent a WM_ERASEBKGND message when a
    progress bar moved backwards, so flickering was rarely apparent. }
  DefaultHandler(Message);
end;

{$IFDEF VCLSTYLES}

{ TNewProgressBarStyleHook - same as Vcl.ComCtrls' TProgressBarStyleHook
  except that it accesses the Control property as a TNewProgressBar instead
  of a TProgressBar }

constructor TNewProgressBarStyleHook.Create(AControl: TWinControl);
const
  cDefaultMarqueeInterval = 10;
begin
  inherited;
  OverridePaint := True;
  DoubleBuffered := True;
  FMarqueeTimer := TTimer.Create(nil);
  FMarqueeTimer.Interval := cDefaultMarqueeInterval;
  FMarqueeTimer.OnTimer := MarqueeAction;
  FMarqueeTimer.Enabled := (TNewProgressBar(Control).Style = npbstMarquee) and
    not (csDesigning in Control.ComponentState);
end;

function TNewProgressBarStyleHook.GetPercent: Single;
var
  LMin, LMax, LPos: Integer;
begin
  LMin := Min;
  LMax := Max;
  LPos := Position;
  if (LPos >= LMin) and (LMax >= LPos) and (LMax - LMin <> 0) then
    Result := (LPos - LMin) / (LMax - LMin)
  else
    Result := 0;
end;

destructor TNewProgressBarStyleHook.Destroy;
begin
  FreeAndNil(FMarqueeTimer);
  inherited;
end;

function TNewProgressBarStyleHook.GetBarRect: TRect;
begin
  Result := TRect.Create(0, 0, Control.Width, Control.Height);
  InflateRect(Result, -BorderWidth, -BorderWidth);
end;

procedure TNewProgressBarStyleHook.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  Message.Result := 0;
  Handled := True;
end;

procedure TNewProgressBarStyleHook.PaintFrame(Canvas: TCanvas);
var
  R: TRect;
  Details: TThemedElementDetails;
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  if not LStyle.Available then
    Exit;

  R := BarRect;
  if Orientation = pbHorizontal then
    Details := LStyle.GetElementDetails(tpBar)
  else
    Details := LStyle.GetElementDetails(tpBarVert);
  LStyle.DrawElement(Canvas.Handle, Details, R);
end;

procedure TNewProgressBarStyleHook.MarqueeAction(Sender: TObject);
begin
  if StyleServices.Available and Control.Visible and (Control is TNewProgressBar) and
    (TNewProgressBar(Control).Style = npbstMarquee) and not (csDesigning in Control.ComponentState) then
    Invalidate
  else
    FMarqueeTimer.Enabled := False;
end;

{$IF CompilerVersion < 36.0}
{ From Delphi 12.3's GraphUtil - including the function name typo }
procedure SetPreMutipliedColor(ABitMap: TBitmap; Color: TColor);
var
  X, Y: Integer;
  R, G, B: Byte;
  LRGBQuad: PRGBQuad;
begin
  if ABitMap.PixelFormat <> pf32bit then
    Exit;

  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
  ABitmap.AlphaFormat := afIgnored;
  for Y := 0 to ABitMap.Height - 1 do
  begin
    LRGBQuad := ABitMap.ScanLine[Y];
    for X := 0 to ABitMap.Width - 1 do
    begin
      LRGBQuad.rgbRed := R;
      LRGBQuad.rgbGreen := G;
      LRGBQuad.rgbBlue := B;
      Inc(LRGBQuad);
    end;
  end;
  ABitmap.AlphaFormat := afPremultiplied;
end;
{$ENDIF}

{$IF CompilerVersion < 35.0}
{ From Delphi 12.3's GraphUtil }
type
  PRGBAArray = ^TRGBAArray;
  TRGBAArray = array[0..0] of TRGBQuad;

procedure InitAlpha(ABitmap: TBitmap; AAlpha: Byte);
var
  I: Integer;
  LLastLine: PRGBAArray;
begin
  LLastLine := ABitmap.ScanLine[ABitmap.Height - 1];
  {$IFOPT R+} {$DEFINE RANGECHECKS_ON} {$R-} {$ENDIF}
  for I := 0 to ABitmap.Width * ABitmap.Height - 1 do
    LLastLine[I].rgbReserved := AAlpha;
  {$IFDEF RANGECHECKS_ON} {$R+} {$UNDEF RANGECHECKS_ON} {$ENDIF}
end;
{$ENDIF}

procedure TNewProgressBarStyleHook.PaintBar(Canvas: TCanvas);
const
  cStateColorAlpha = 130;
  cStateErrorColor = clRed;
  cStatePausedColor = clYellow;
  cMarqueeSize = 125;
  cMarqueeSteps = 5;//cMarqueeSize div 3;
var
  FillR, R: TRect;
  W, Pos: Integer;
  Details: TThemedElementDetails;
  LStyle: TCustomStyleServices;
  LIsMarquee: Boolean;
  LBuffer: TBitmap;
begin
  LStyle := StyleServices;
  if not LStyle.Available then
    Exit;
  R := BarRect;
  InflateRect(R, -1, -1);
  if Orientation = pbHorizontal then
    W := R.Width
  else
    W := R.Height;

  LIsMarquee := (Control is TNewProgressBar) and
    (TNewProgressBar(Control).Style = npbstMarquee) and not (csDesigning in Control.ComponentState);
  if LIsMarquee then
    Pos := Control.ScaleValue(cMarqueeSize)
  else
    Pos := Round(W * GetPercent);
  FillR := R;
  if Orientation = pbHorizontal then
  begin
    FillR.Right := FillR.Left + Pos;
    Details := LStyle.GetElementDetails(tpChunk);
  end
  else
  begin
    FillR.Top := FillR.Bottom - Pos;
    Details := LStyle.GetElementDetails(tpChunkVert);
  end;

  if LIsMarquee then
  begin
    FillR.SetLocation(FMarqueeStep, FillR.Top);
    Inc(FMarqueeStep, cMarqueeSteps);
    if FMarqueeStep >= Control.Width then
      FMarqueeStep := -cMarqueeSize;
  end;

  LStyle.DrawElement(Canvas.Handle, Details, FillR);

  if not FillR.IsEmpty and not LIsMarquee and (Control is TNewProgressBar) and (TNewProgressBar(Control).State <> npbsNormal) then
  begin
    LBuffer := TBitmap.Create;
    try
      LBuffer.PixelFormat := pf32bit;
      LBuffer.SetSize(FillR.Width, FillR.Height);
      InitAlpha(LBuffer, 0);
      LStyle.DrawElement(LBuffer.Canvas.Handle, Details, TRect.Create(0, 0, LBuffer.Width, LBuffer.Height));
      case TNewProgressBar(Control).State of
        npbsError:
          SetPreMutipliedColor(LBuffer, cStateErrorColor);
        npbsPaused:
          SetPreMutipliedColor(LBuffer, cStatePausedColor);
      end;
      Canvas.Draw(FillR.Left, FillR.Top, LBuffer, cStateColorAlpha);
    finally
      LBuffer.Free;
    end;
  end
end;

procedure TNewProgressBarStyleHook.Paint(Canvas: TCanvas);
var
  Details: TThemedElementDetails;
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  if LStyle.Available then
  begin
    Details.Element := teProgress;
    if LStyle.HasTransparentParts(Details) then
      LStyle.DrawParentBackground(Handle, Canvas.Handle, Details, False);
  end;
  PaintFrame(Canvas);
  PaintBar(Canvas);
end;

function TNewProgressBarStyleHook.GetMax: Integer;
begin
  Result := SendMessage(Handle, PBM_GetRange, 0, 0);
end;

function TNewProgressBarStyleHook.GetMin: Integer;
begin
  Result := SendMessage(Handle, PBM_GetRange, 1, 0);
end;

function TNewProgressBarStyleHook.GetOrientation: TProgressBarOrientation;
begin
  Result := pbHorizontal;
  if (Handle <> 0) and (GetWindowLong(Handle, GWL_STYLE) and PBS_VERTICAL = PBS_VERTICAL) then
    Result := pbVertical;
end;

function TNewProgressBarStyleHook.GetPosition: Integer;
begin
  Result := SendMessage(Handle, PBM_GETPOS, 0, 0);
end;

function TNewProgressBarStyleHook.GetBorderWidth: Integer;
begin
  Result := 0;
end;

{$ENDIF}

end.
