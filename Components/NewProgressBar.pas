unit NewProgressBar;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TNewProgressBar component - a smooth 32 bit TProgressBar

  Note: themed animated progress bars and don't immediately show changes.
  This applies both to Position and State. For example if you set State while the
  progress bar is still moving towards a new Position, the new State doesnt show until
  the moving animation has finished.
}

interface

uses
  Messages, Classes, Controls, ComCtrls;

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

procedure Register;

implementation

uses
  Windows, CommCtrl;

procedure Register;
begin
  RegisterComponents('JR', [TNewProgressBar]);
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

end.
