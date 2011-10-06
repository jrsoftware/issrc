unit SetupForm;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TSetupForm

  $jrsoftware: issrc/Projects/SetupForm.pas,v 1.16 2010/02/02 06:59:38 jr Exp $
}

interface

{$I VERSION.INC}

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  UIStateForm, MsgIDs;

type
  TSetupForm = class(TUIStateForm)
  private
    FBaseUnitX, FBaseUnitY: Integer;
    FRightToLeft: Boolean;
    FFlipControlsOnShow: Boolean;
    FControlsFlipped: Boolean;
    procedure WMQueryEndSession(var Message: TWMQueryEndSession); message WM_QUERYENDSESSION;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure VisibleChanging; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    {$IFNDEF IS_D4}
    constructor CreateNew(AOwner: TComponent);
    {$ELSE}
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    {$ENDIF}
    function CalculateButtonWidth(const ButtonCaptions: array of TSetupMessageID): Integer;
    procedure Center;
    procedure CenterInsideControl(const Ctl: TWinControl;
      const InsideClientArea: Boolean);
    procedure CenterInsideRect(const InsideRect: TRect);
    procedure FlipControlsIfNeeded;
    procedure InitializeFont;
    function ScalePixelsX(const N: Integer): Integer;
    function ScalePixelsY(const N: Integer): Integer;
    property BaseUnitX: Integer read FBaseUnitX;
    property BaseUnitY: Integer read FBaseUnitY;
    property ControlsFlipped: Boolean read FControlsFlipped;
    property FlipControlsOnShow: Boolean read FFlipControlsOnShow write FFlipControlsOnShow;
    property RightToLeft: Boolean read FRightToLeft;
  end;

procedure CalculateBaseUnitsFromFont(const Font: TFont; var X, Y: Integer);
function GetRectOfPrimaryMonitor(const WorkArea: Boolean): TRect;
function SetFontNameSize(const AFont: TFont; const AName: String;
  const ASize: Integer; const AFallbackName: String;
  const AFallbackSize: Integer): Boolean;

const
  OrigBaseUnitX = 6;
  OrigBaseUnitY = 13;

implementation

uses
  CmnFunc2, Main, Msgs, BidiUtils;

var
  WM_QueryCancelAutoPlay: UINT;

function GetRectOfPrimaryMonitor(const WorkArea: Boolean): TRect;
begin
  if not WorkArea or
     not SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0) then
    Result := Rect(0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
end;

function GetRectOfMonitorContainingRect(const R: TRect): TRect;
{ Returns bounding rectangle of monitor containing or nearest to R }
type
  HMONITOR = type THandle;
  TMonitorInfo = record
    cbSize: DWORD;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFlags: DWORD;
  end;
const
  MONITOR_DEFAULTTONEAREST = $00000002;
var
  Module: HMODULE;
  MonitorFromRect: function(const lprc: TRect; dwFlags: DWORD): HMONITOR; stdcall;
  GetMonitorInfo: function(hMonitor: HMONITOR; var lpmi: TMonitorInfo): BOOL; stdcall;
  M: HMONITOR;
  Info: TMonitorInfo;
begin
  Module := GetModuleHandle(user32);
  MonitorFromRect := GetProcAddress(Module, 'MonitorFromRect');
  GetMonitorInfo := GetProcAddress(Module, 'GetMonitorInfoA');
  if Assigned(MonitorFromRect) and Assigned(GetMonitorInfo) then begin
    M := MonitorFromRect(R, MONITOR_DEFAULTTONEAREST);
    Info.cbSize := SizeOf(Info);
    if GetMonitorInfo(M, Info) then begin
      Result := Info.rcWork;
      Exit;
    end;
  end;
  Result := GetRectOfPrimaryMonitor(True);
end;

function SetFontNameSize(const AFont: TFont; const AName: String;
  const ASize: Integer; const AFallbackName: String;
  const AFallbackSize: Integer): Boolean;
{ Returns True if AName <> '' and it used AName as the font name,
  False otherwise. }

  function SizeToHeight(const S: Integer): Integer;
  begin
    Result := MulDiv(-S, Screen.PixelsPerInch, 72);
  end;

begin
  Result := False;
  if AName <> '' then begin
    if FontExists(AName) then begin
      AFont.Name := AName;
      AFont.Height := SizeToHeight(ASize);
      Result := True;
      Exit;
    end;
    { Note: AFallbackName is not used if the user specified an empty string for
      AName because in that case they want the default font used always }
    if (AFallbackName <> '') and FontExists(AFallbackName) then begin
      AFont.Name := AFallbackName;
      AFont.Height := SizeToHeight(AFallbackSize);
      Exit;
    end;
  end;
  AFont.Name := GetPreferredUIFont;
  AFont.Height := SizeToHeight(AFallbackSize);
end;

procedure CalculateBaseUnitsFromFont(const Font: TFont; var X, Y: Integer);
var
  DC: HDC;
  Size: TSize;
  TM: TTextMetric;
begin
  DC := GetDC(0);
  try
    SelectObject(DC, Font.Handle);
    { Based on code from Q145994: }
    GetTextExtentPoint(DC,
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz', 52, Size);
    X := (Size.cx div 26 + 1) div 2;
    GetTextMetrics(DC, TM);
    Y := TM.tmHeight;
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure NewChangeScale(const Ctl: TControl; const XM, XD, YM, YD: Integer);
var
  X, Y, W, H: Integer;
begin
  X := MulDiv(Ctl.Left, XM, XD);
  Y := MulDiv(Ctl.Top, YM, YD);
  if not(csFixedWidth in Ctl.ControlStyle) then
    W := MulDiv(Ctl.Width, XM, XD)
  else
    W := Ctl.Width;
  if not(csFixedHeight in Ctl.ControlStyle) then
    H := MulDiv(Ctl.Height, YM, YD)
  else
    H := Ctl.Height;
  Ctl.SetBounds(X, Y, W, H);
end;

procedure NewScaleControls(const Ctl: TWinControl; const XM, XD, YM, YD: Integer);
{ This is like TControl.ScaleControls, except it allows the width and height
  to be scaled independently }
var
  I: Integer;
  C: TControl;
begin
  for I := 0 to Ctl.ControlCount-1 do begin
    C := Ctl.Controls[I];
    if C is TWinControl then begin
      TWinControl(C).DisableAlign;
      try
        NewScaleControls(TWinControl(C), XM, XD, YM, YD);
        NewChangeScale(C, XM, XD, YM, YD);
      finally
        TWinControl(C).EnableAlign;
      end;
    end
    else
      NewChangeScale(C, XM, XD, YM, YD);
  end;
end;

function GetParentSetupForm(AControl: TControl): TSetupForm;
begin
  { Note: Unlike GetParentForm, this checks all levels, not just the top }
  repeat
    if AControl is TSetupForm then begin
      Result := TSetupForm(AControl);
      Exit;
    end;
    AControl := AControl.Parent;
  until AControl = nil;
  Result := nil;
end;

function IsParentSetupFormFlipped(AControl: TControl): Boolean;
var
  ParentForm: TSetupForm;
begin
  ParentForm := GetParentSetupForm(AControl);
  if Assigned(ParentForm) then
    Result := ParentForm.ControlsFlipped
  else
    Result := False;
end;

function IsParentSetupFormRightToLeft(AControl: TControl): Boolean;
var
  ParentForm: TSetupForm;
begin
  ParentForm := GetParentSetupForm(AControl);
  if Assigned(ParentForm) then
    Result := ParentForm.RightToLeft
  else
    Result := False;
end;

{ TSetupForm }

constructor TSetupForm.Create(AOwner: TComponent);
begin
  { Must initialize FRightToLeft here in addition to CreateNew because
    CreateNew isn't virtual on Delphi 2 and 3 }
  FRightToLeft := LangOptions.RightToLeft;
  FFlipControlsOnShow := FRightToLeft;
  inherited;
  { In Delphi 2005 and later, Position defaults to poDefaultPosOnly, but we
    don't want the form to be changing positions whenever its handle is
    recreated, so change it to the D7 and earlier default of poDesigned. }
  if Position = poDefaultPosOnly then
    Position := poDesigned;
end;

{$IFNDEF IS_D4}
constructor TSetupForm.CreateNew(AOwner: TComponent);
{$ELSE}
constructor TSetupForm.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
{$ENDIF}
begin
  { Note: On Delphi 2 and 3, CreateNew isn't virtual, so this is only reached
    when TSetupForm.CreateNew is called explicitly }
  FRightToLeft := LangOptions.RightToLeft;
  FFlipControlsOnShow := FRightToLeft;
  inherited;
end;

function TSetupForm.CalculateButtonWidth(const ButtonCaptions: array of TSetupMessageID): Integer;
var
  DC: HDC;
  I, W: Integer;
begin
  Result := ScalePixelsX(75);
  { Increase the button size if there are unusually long button captions }
  DC := GetDC(0);
  try
    SelectObject(DC, Font.Handle);
    for I := Low(ButtonCaptions) to High(ButtonCaptions) do begin
      W := GetTextWidth(DC, SetupMessages[ButtonCaptions[I]], True) + ScalePixelsX(20);
      if Result < W then
        Result := W;
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TSetupForm.CenterInsideControl(const Ctl: TWinControl;
  const InsideClientArea: Boolean);
var
  R: TRect;
begin
  if not InsideClientArea then begin
    if GetWindowRect(Ctl.Handle, R) then
      CenterInsideRect(R);
  end
  else begin
    R := Ctl.ClientRect;
    MapWindowPoints(Ctl.Handle, 0, R, 2);
    CenterInsideRect(R);
  end;
end;

procedure TSetupForm.CenterInsideRect(const InsideRect: TRect);
var
  R, MR: TRect;
begin
  R := Bounds(InsideRect.Left + ((InsideRect.Right - InsideRect.Left) - Width) div 2,
    InsideRect.Top + ((InsideRect.Bottom - InsideRect.Top) - Height) div 2,
    Width, Height);

  { Clip to nearest monitor }
  MR := GetRectOfMonitorContainingRect(R);
  if R.Right > MR.Right then
    OffsetRect(R, MR.Right - R.Right, 0);
  if R.Bottom > MR.Bottom then
    OffsetRect(R, 0, MR.Bottom - R.Bottom);
  if R.Left < MR.Left then
    OffsetRect(R, MR.Left - R.Left, 0);
  if R.Top < MR.Top then
    OffsetRect(R, 0, MR.Top - R.Top);

  BoundsRect := R;
end;

procedure TSetupForm.Center;
begin
  CenterInsideRect(GetRectOfPrimaryMonitor(True));
end;

procedure TSetupForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if FRightToLeft then
    Params.ExStyle := Params.ExStyle or (WS_EX_RTLREADING or WS_EX_LEFTSCROLLBAR or WS_EX_RIGHT);
end;

procedure TSetupForm.CreateWnd;
begin
  inherited;
  if WM_QueryCancelAutoPlay <> 0 then
    AddToWindowMessageFilterEx(Handle, WM_QueryCancelAutoPlay);
end;

procedure TSetupForm.FlipControlsIfNeeded;
begin
  if FFlipControlsOnShow then begin
    FFlipControlsOnShow := False;
    FControlsFlipped := not FControlsFlipped;
    FlipControls(Self);
  end;
end;

procedure TSetupForm.InitializeFont;
var
  W, H: Integer;
  R: TRect;
begin
  { Note: Must keep the following lines in synch with ScriptFunc_R's
    InitializeScaleBaseUnits }
  SetFontNameSize(Font, LangOptions.DialogFontName, LangOptions.DialogFontSize,
    '', 8);
  CalculateBaseUnitsFromFont(Font, FBaseUnitX, FBaseUnitY);

  if (FBaseUnitX <> OrigBaseUnitX) or (FBaseUnitY <> OrigBaseUnitY) then begin
    { Loosely based on scaling code from TForm.ReadState: }
    NewScaleControls(Self, BaseUnitX, OrigBaseUnitX, BaseUnitY, OrigBaseUnitY);
    R := ClientRect;
    W := MulDiv(R.Right, FBaseUnitX, OrigBaseUnitX);
    H := MulDiv(R.Bottom, FBaseUnitY, OrigBaseUnitY);
    SetBounds(Left, Top, W + (Width - R.Right), H + (Height - R.Bottom));
  end;
end;

function TSetupForm.ScalePixelsX(const N: Integer): Integer;
begin
  Result := MulDiv(N, BaseUnitX, OrigBaseUnitX);
end;

function TSetupForm.ScalePixelsY(const N: Integer): Integer;
begin
  Result := MulDiv(N, BaseUnitY, OrigBaseUnitY);
end;

procedure TSetupForm.VisibleChanging;
begin
  inherited;
  { Note: Unlike DoShow, any exceptions raised in VisibleChanging will be
    propagated out, which is what we want }
  if not Visible then
    FlipControlsIfNeeded;
end;

procedure TSetupForm.WMQueryEndSession(var Message: TWMQueryEndSession);
begin
  { TDummyClass.AntiShutdownHook in Setup.dpr already denies shutdown attempts
    but we also need to catch WM_QUERYENDSESSION here to suppress the VCL's
    default handling which calls CloseQuery. We do not want to let TMainForm &
    TNewDiskForm display any 'Exit Setup?' message boxes since we're already
    denying shutdown attempts, and also we can't allow them to potentially be
    displayed on top of another dialog box that's already displayed. }

  { Return zero, except if RestartInitiatedByThisProcess is set (which means
    we called RestartComputer previously) }
  if RestartInitiatedByThisProcess then
    Message.Result := 1;
end;

procedure TSetupForm.WndProc(var Message: TMessage);
begin
  { When we receive a 'QueryCancelAutoPlay' message as a result of a new CD
    being inserted, return 1 to prevent it from being 'autoplayed'.
    Note: According to the docs, this message is only sent on Shell version
    4.70 and later. }
  if (WM_QueryCancelAutoPlay <> 0) and (Message.Msg = WM_QueryCancelAutoPlay) then
    Message.Result := 1
  else
    inherited;
end;

initialization
  BidiUtils.IsParentFlippedFunc := IsParentSetupFormFlipped;
  BidiUtils.IsParentRightToLeftFunc := IsParentSetupFormRightToLeft;
  WM_QueryCancelAutoPlay := RegisterWindowMessage('QueryCancelAutoPlay');
end.
