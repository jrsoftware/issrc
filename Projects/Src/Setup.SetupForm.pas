unit Setup.SetupForm;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TSetupForm

  Also used by UninstallProgressForm and UninstallSharedFileForm!

  Requires following globals to be set:
  -LangOptions.RightToLeft
  -LangOptions.DialogFontName
  -LangOptions.DialogFontSize
  -LangOptions.DialogFontBaseScaleWidth
  -LangOptions.DialogFontBaseScaleHeight
  -shWizardBorderStyled in SetupHeader.Options
  -shWizardKeepAspectRatio in SetupHeader.Options
  Also requires following globals to be set, but 0 is allowed:
  -SetupHeader.WizardSizePercentX
  -SetupHeader.WizardSizePercentY
}

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  UIStateForm;

type
  TSetupForm = class(TUIStateForm)
  private
    FOrigBaseUnitX, FOrigBaseUnitY: Integer;
    FBaseUnitX, FBaseUnitY: Integer;
    FRightToLeft: Boolean;
    FFlipControlsOnShow: Boolean;
    FCenterOnShow: Boolean;
    FControlsFlipped: Boolean;
    FKeepSizeX, FKeepSizeY: Boolean;
    FOrgClientWidthAfterScale, FOrgClientHeightAfterScale: Integer;
    FSetForeground: Boolean;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMQueryEndSession(var Message: TWMQueryEndSession); message WM_QUERYENDSESSION;
  protected
    procedure Center;
    procedure CenterInsideControl(const Ctl: TWinControl;
      const InsideClientArea: Boolean);
    procedure CenterInsideRect(const InsideRect: TRect);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GetExtraClientWidth: Integer;
    function GetExtraClientHeight: Integer;
    procedure FlipControlsIfNeeded;
    procedure CenterIfNeeded(const ACenterInsideControl: Boolean;
      const CenterInsideControlCtl: TWinControl;
      const CenterInsideControlInsideClientArea: Boolean);
    procedure VisibleChanging; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    function CalculateButtonWidth(const ButtonCaptions: array of String): Integer;
    procedure InitializeFont(const KeepSizeX: Boolean = False; const KeepSizeY: Boolean = False);
    class function ScalePixelsX(const OrigBaseUnitX, BaseUnitX, N: Integer): Integer; overload;
    class function ScalePixelsY(const OrigBaseUnitY, BaseUnitY, N: Integer): Integer; overload;
    function ScalePixelsX(const N: Integer): Integer; overload;
    function ScalePixelsY(const N: Integer): Integer; overload;
    function ShouldSizeX: Boolean;
    function ShouldSizeY: Boolean;
    function ShowModal: Integer; override;
    procedure FlipAndCenterIfNeeded(const ACenterInsideControl: Boolean = False;
      const CenterInsideControlCtl: TWinControl = nil;
      const CenterInsideControlInsideClientArea: Boolean = False); virtual;
    property BaseUnitX: Integer read FBaseUnitX;
  published
    property CenterOnShow: Boolean read FCenterOnShow write FCenterOnShow;
    property ControlsFlipped: Boolean read FControlsFlipped;
    property ExtraClientWidth: Integer read GetExtraClientWidth;
    property ExtraClientHeight: Integer read GetExtraClientHeight;
    property FlipControlsOnShow: Boolean read FFlipControlsOnShow write FFlipControlsOnShow;
    property KeepSizeX: Boolean read FKeepSizeX;
    property KeepSizeY: Boolean read FKeepSizeY;
    property RightToLeft: Boolean read FRightToLeft;
    property SetForeground: Boolean read FSetForeground write FSetForeground;
  end;

procedure CalculateBaseUnitsFromFont(const Font: TFont; var X, Y: Integer);
function SetFontNameSize(const AFont: TFont; const AName: String;
  const ASize: Integer; const AFallbackName: String;
  const AFallbackSize: Integer): Boolean;

implementation

uses
  Generics.Collections, UITypes, StdCtrls,
  BidiUtils, NewNotebook, NewStaticText,
  Shared.Struct, Shared.CommonFunc, Shared.CommonFunc.Vcl, Setup.MainFunc;

var
  WM_QueryCancelAutoPlay: UINT;

function GetRectOfPrimaryMonitor(const WorkArea: Boolean): TRect;
begin
  if not WorkArea or
     not SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0) then
    Result := Rect(0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
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
  AFont.Name := 'Segoe UI';
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

function IsParentSetupFormFlipped(AControl: TControl): Boolean;

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

var
  ParentForm: TSetupForm;
begin
  ParentForm := GetParentSetupForm(AControl);
  if Assigned(ParentForm) then
    Result := ParentForm.ControlsFlipped
  else
    Result := False;
end;

function GetPPI(const Wnd: HWND): Integer;
begin
  { Based on TSysStyleHook.GetCurrentPPI }
  if CheckPerMonitorV2SupportForWindow(Wnd) then begin { Currently always False in Setup }
    { GetDPIForWindow requires Windows 10 version 1607. However, because it is delay-loaded and it's
      never executed on older versions of Windows, it does not cause entry point not found errors. }
    Result := GetDPIForWindow(Wnd)
  end else
    Result := Screen.PixelsPerInch;
end;

{ TSetupForm }

constructor TSetupForm.Create(AOwner: TComponent);
begin
  { Must initialize FRightToLeft here in addition to CreateNew because
    CreateNew isn't virtual on Delphi 2 and 3 }
  FRightToLeft := LangOptions.RightToLeft;
  FFlipControlsOnShow := FRightToLeft;
  FCenterOnShow := True;
  inherited;
   { Setting BidiMode before inherited causes an AV when TControl tries to
     send CM_BIDIMODECHANGED. This is why we have additonal RTL code in
     CreateParams below. }
  if FRightToLeft then
    BiDiMode := bdRightToLeft;
  { In Delphi 2005 and later, Position defaults to poDefaultPosOnly, but we
    don't want the form to be changing positions whenever its handle is
    recreated, so change it to the D7 and earlier default of poDesigned. }
  if Position = poDefaultPosOnly then
    Position := poDesigned;
end;

constructor TSetupForm.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
begin
  { Note: On Delphi 2 and 3, CreateNew isn't virtual, so this is only reached
    when TSetupForm.CreateNew is called explicitly }
  FRightToLeft := LangOptions.RightToLeft;
  FFlipControlsOnShow := FRightToLeft;
  FCenterOnShow := True;
  inherited;
  if FRightToLeft then
    BiDiMode := bdRightToLeft;
end;

function TSetupForm.CalculateButtonWidth(const ButtonCaptions: array of String): Integer;
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
      W := GetTextWidth(DC, ButtonCaptions[I], True) + ScalePixelsX(20);
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
  const CtlForm = GetParentForm(Ctl);
  if (CtlForm = nil) or not IsWindowVisible(CtlForm.Handle) or
     IsIconic(CtlForm.Handle) then begin
    Center;
    Exit;
  end;

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
  { With Application.MainFormOnTaskBar=True, by default, a form won't get a
    taskbar button if the main form hasn't been created yet (due to the owner
    being an invisible Application.Handle), or if the main form exists but
    isn't visible (e.g., because it's a silent install). Force it to have a
    taskbar button in those cases by specifying no owner for the window.
    (Another method is to set WS_EX_APPWINDOW and leave WndParent set to
    Application.Handle, but it doesn't quite work correctly: if the form
    displays a message box, and you activate another app's window, clicking on
    the form's taskbar button activates the message box again, but the taskbar
    button doesn't change to a "selected" state.) }
  if (Params.WndParent <> 0) and
     (Application.MainFormOnTaskBar or (Params.WndParent <> Application.Handle)) and
     not IsWindowOnTaskbar(Params.WndParent) then
    Params.WndParent := 0;

  { See comment in Create. Also: The following does not make the title bar RTL.
    Achieving this requires adding WS_EX_LAYOUTRTL, which VCL does not support. }
  if FRightToLeft then
    Params.ExStyle := Params.ExStyle or (WS_EX_RTLREADING or WS_EX_LEFTSCROLLBAR or WS_EX_RIGHT);
end;

procedure TSetupForm.CreateWnd;

  procedure SetControlsCurrentPPI(const Ctl: TWinControl; const PPI: Integer);
  begin
    for var I := 0 to Ctl.ControlCount-1 do begin
      const C = Ctl.Controls[I];
      if C is TWinControl then begin
        SetControlsCurrentPPI(TWinControl(C), PPI);
        C.SetCurrentPPI(PPI);
      end else
        C.SetCurrentPPI(PPI)
    end;
  end;

begin
  inherited;
  if WM_QueryCancelAutoPlay <> 0 then
    AddToWindowMessageFilterEx(Handle, WM_QueryCancelAutoPlay);
  if not (shWizardBorderStyled in SetupHeader.Options) then begin
    { SetDarkTitleBar also removes seBorder which disables styling of the titlebar and the border.
      Note that removing seBorder in Create causes a small bit of space to the right of bevels for
      some reason. Doing it here does not cause this problem. It's also here because SetDarkTitleBar
      requires the handle of the form. }
    SetDarkTitleBar(Self, IsDarkInstallMode);
    { SetDarkTitleBar is a noop on older versions of Windows }
    if seBorder in StyleElements then
      StyleElements := StyleElements - [seBorder];
  end;

  { We don't use the Scaled property for scaling and this means the CurrentPPI property will not be
    set correctly. This causes problems when VCL code inspects it, for example in THintWindow.CalcHintRect
    and FormStyleHook.GetBorderSize. So we should update it ourselves by directly writing to the
    FCurrentPPI private variable of the form and all controls on it, which we can do using a class
    helper. Note: Doing it later for the form causes issues with incorrect non-client vs. client size
    when styled title bars are enabled. }

  const PPI = GetPPI(Handle);
  SetCurrentPPI(PPI);
  SetControlsCurrentPPI(Self, PPI);
  
  { Now that CurrentPPI of the form is set you must make sure that any controls you later parent to
    the form already have the same CurrentPPI, otherwise VCL will scale the controls. Currently this
    is done in:
    -Setup.ScriptClasses's TControlParentW and TNewNotebookPageNotebook_W
    -Setup.ScriptDlg's SetCtlParent
    -TWizardForm.AddPage
    To debug/detect scaling add a breakpoint in Vcl.Controls' TWinControl.ChangeScale and set project
    option Building->Delphi Compiler->Compiling->Debugging->Use debug .dcus. }
end;

procedure TSetupForm.FlipControlsIfNeeded;
begin
  if FFlipControlsOnShow then begin
    FFlipControlsOnShow := False;
    FControlsFlipped := not FControlsFlipped;
    FlipControls(Self);
  end;
end;

procedure TSetupForm.CenterIfNeeded(const ACenterInsideControl: Boolean; const CenterInsideControlCtl: TWinControl; const CenterInsideControlInsideClientArea: Boolean);
begin
  if FCenterOnShow then begin
    FCenterOnShow := False;
    { Center }
    if ACenterInsideControl then
      CenterInsideControl(CenterInsideControlCtl, CenterInsideControlInsideClientArea)
    else
      Center;
  end;
end;

function TSetupForm.ShouldSizeX: Boolean;
begin
  Result := not FKeepSizeX and (SetupHeader.WizardSizePercentX > 100);
end;

function TSetupForm.ShouldSizeY: Boolean;
begin
  Result := not FKeepSizeY and (SetupHeader.WizardSizePercentY > 100);
end;

procedure TSetupForm.FlipAndCenterIfNeeded(const ACenterInsideControl: Boolean;
  const CenterInsideControlCtl: TWinControl; const CenterInsideControlInsideClientArea: Boolean);
begin
  FlipControlsIfNeeded;
  CenterIfNeeded(ACenterInsideControl, CenterInsideControlCtl, CenterInsideControlInsideClientArea);
end;

type
  TControlAccess = class(TControl);

procedure TSetupForm.InitializeFont(const KeepSizeX, KeepSizeY: Boolean);

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

  type
    TControlAnchorsList = TDictionary<TControl, TAnchors>;

  procedure StripAndStoreChildControlCustomAnchors(const ParentCtl: TControl; const AnchorsList: TControlAnchorsList);
  begin
    if ParentCtl is TWinControl then begin
      const ParentWinCtl = TWinControl(ParentCtl);
      for var I := 0 to ParentWinCtl.ControlCount-1 do begin
        const Ctl = ParentWinCtl.Controls[I];

        if Ctl.Anchors <> [akLeft, akTop] then begin
          AnchorsList.Add(Ctl, Ctl.Anchors);
          { Before we can set Anchors to [akLeft, akTop] (which has a special
            'no anchors' meaning to VCL), we first need to update the Explicit*
            properties so the control doesn't get moved back to an old position }
          TControlAccess(Ctl).UpdateExplicitBounds;
          Ctl.Anchors := [akLeft, akTop];
        end;

        StripAndStoreChildControlCustomAnchors(Ctl, AnchorsList);
      end;
    end;
  end;

  function GetHasChildControlCustomAnchors(const ParentCtl: TControl): Boolean;
  begin
    if ParentCtl is TWinControl then begin
      const ParentWinCtl = TWinControl(ParentCtl);
      for var I := 0 to ParentWinCtl.ControlCount-1 do begin
        const Ctl = ParentWinCtl.Controls[I];
        if (Ctl.Anchors <> [akLeft, akTop]) or GetHasChildControlCustomAnchors(Ctl) then
          Exit(True);
      end;
    end;

    Result := False;
  end;

  procedure RestoreAnchors(const AnchorsList: TControlAnchorsList);
  begin
    { The order in which we restore the anchors shouldn't matter, so just
      enumerate the list }
    for var Item in AnchorsList do
      Item.Key.Anchors := Item.Value;
  end;

  procedure ParentHandlesNeeded(const ParentCtl: TControl);
  begin
    if ParentCtl is TWinControl then begin
      const ParentWinCtl = TWinControl(ParentCtl);
      if ParentWinCtl.ControlCount > 0 then begin
        if not (ParentWinCtl is TNewNotebook) then { For notebooks: only need handles on pages }
          ParentWinCtl.HandleNeeded;
        for var I := 0 to ParentWinCtl.ControlCount-1 do
          ParentHandlesNeeded(ParentWinCtl.Controls[I]);
      end;
    end;
  end;

  function GetHasTopLevelAutoSizeAnchoredStaticText(const ParentCtl: TWinControl): Boolean;
  begin
    for var I := 0 to ParentCtl.ControlCount-1 do begin
      if ParentCtl.Controls[I] is TNewStaticText then begin
        const Ctl = TNewStaticText(ParentCtl.Controls[I]);
        if Ctl.AutoSize and (Ctl.Anchors * [akBottom, akRight] <> []) then
          Exit(True);
      end else if ParentCtl.Controls[I] is TStaticText then begin
        const Ctl = TStaticText(ParentCtl.Controls[I]);
        if Ctl.AutoSize and (Ctl.Anchors * [akBottom, akRight] <> []) then
          Exit(True);
      end;
    end;

    Result := False;
  end;

begin
  { T(New)StaticText's which have AutoSize set, and also akBottom or akRight, and have the form
    itself as the parent, need an early HandleNeeded call on the parent to ensure correct positioning.
    It's needed even if no sizing and no scaling is done, and it is somehow related to the
    SetFontNameSize call below and T(New)StaticText's AdjustBounds. Example control with this issue:
    WizardForm.BeveledLabel. }

  if GetHasTopLevelAutoSizeAnchoredStaticText(Self) then
    HandleNeeded; { Also see ShowModal, and below }

  { Set font. Note: Must keep the following lines in synch with Setup.ScriptFunc.pas's
    InitializeScaleBaseUnits }

  SetFontNameSize(Font, LangOptions.DialogFontName, LangOptions.DialogFontSize, '', 9);

  CalculateBaseUnitsFromFont(Font, FBaseUnitX, FBaseUnitY);

  FOrigBaseUnitX := LangOptions.DialogFontBaseScaleWidth;
  FOrigBaseUnitY := LangOptions.DialogFontBaseScaleHeight;

  if shWizardKeepAspectRatio in SetupHeader.Options then begin
    if FBaseUnitX / FOrigBaseUnitX > FBaseUnitY / FOrigBaseUnitY then begin
      FBaseUnitY := FBaseUnitX;
      FOrigBaseUnitY := FOrigBaseUnitX;
    end else begin
      FBaseUnitX := FBaseUnitY;
      FOrigBaseUnitX := FOrigBaseUnitY;
    end;
  end;

  { Scale }

  var HasCustomAnchors: Boolean;

  if (FBaseUnitX <> FOrigBaseUnitX) or (FBaseUnitY <> FOrigBaseUnitY) then begin
    const ControlAnchorsList = TControlAnchorsList.Create;
    try
      { Custom anchors interfere with our scaling code, so strip them and restore
        afterward }
      StripAndStoreChildControlCustomAnchors(Self, ControlAnchorsList);
      HasCustomAnchors := ControlAnchorsList.Count > 0;
      { Loosely based on scaling code from TForm.ReadState: }
      NewScaleControls(Self, FBaseUnitX, FOrigBaseUnitX, FBaseUnitY, FOrigBaseUnitY);
      const R = ClientRect;
      const W = MulDiv(R.Right, FBaseUnitX, FOrigBaseUnitX);
      const H = MulDiv(R.Bottom, FBaseUnitY, FOrigBaseUnitY);
      SetBounds(Left, Top, W + (Width - R.Right), H + (Height - R.Bottom));
    finally
      RestoreAnchors(ControlAnchorsList);
      ControlAnchorsList.Free;
    end;
  end else
    HasCustomAnchors := GetHasChildControlCustomAnchors(Self);

  { Size }

  FKeepSizeX := KeepSizeX;
  FKeepSizeY := KeepSizeY;
  FOrgClientWidthAfterScale := ClientWidth;
  FOrgClientHeightAfterScale := ClientHeight;

  const LShouldSizeX = ShouldSizeX;
  const LShouldSizeY = ShouldSizeY;

  if HasCustomAnchors and (LShouldSizeX or LShouldSizeY) then begin
    { Various things related to positioning and anchoring don't work without this:
      you get positions of child controls back as if there was no anchoring until
      handles are automatically created. For WizardForm it works if done after
      sizing but for UninstallProgressForm it must be done before sizing (for
      unknown reasons), so doing before sizing. }
    ParentHandlesNeeded(Self); { Also see ShowModal, and above }
  end;

  if LShouldSizeX then
    ClientWidth := MulDiv(ClientWidth, SetupHeader.WizardSizePercentX, 100);
  if LShouldSizeY then
    ClientHeight := MulDiv(ClientHeight, SetupHeader.WizardSizePercentY, 100);
end;

function TSetupForm.GetExtraClientWidth: Integer;
begin
  Result := ClientWidth - FOrgClientWidthAfterScale;
end;

function TSetupForm.GetExtraClientHeight: Integer;
begin
  Result := ClientHeight - FOrgClientHeightAfterScale;
end;

class function TSetupForm.ScalePixelsX(const OrigBaseUnitX, BaseUnitX, N: Integer): Integer;
begin
  Result := MulDiv(N, BaseUnitX, OrigBaseUnitX);
end;

function TSetupForm.ScalePixelsX(const N: Integer): Integer;
begin
  Result := ScalePixelsX(FOrigBaseUnitX, FBaseUnitX, N);
end;

class function TSetupForm.ScalePixelsY(const OrigBaseUnitY, BaseUnitY, N: Integer): Integer;
begin
  Result := MulDiv(N, BaseUnitY, OrigBaseUnitY);
end;

function TSetupForm.ScalePixelsY(const N: Integer): Integer;
begin
  Result := ScalePixelsY(FOrigBaseUnitY, FBaseUnitY, N);
end;

function TSetupForm.ShowModal: Integer;
begin
  { Work around VCL issue (Delphi 11.3): ShowModal calls DisableTaskWindows
    without ensuring the form's handle has been created first. If the handle
    is created after DisableTaskWindows, PopupMode=pmAuto breaks;
    TCustomForm.CreateParams finds that the active window is disabled, and
    doesn't use it as the owner. It then falls back to pmNone behavior, which
    is to use the main form or application window as the owner. }
  HandleNeeded; { Also see InitializeFont }
  Result := inherited;
end;

procedure TSetupForm.VisibleChanging;
begin
  inherited;
  { Note: Unlike DoShow, any exceptions raised in VisibleChanging will be
    propagated out, which is what we want }
  if not Visible then
    FlipAndCenterIfNeeded;
  end;

procedure TSetupForm.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  { This usually just makes the taskbar button flash }
  if FSetForeground and Showing then
    SetForegroundWindow(Handle);
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
  WM_QueryCancelAutoPlay := RegisterWindowMessage('QueryCancelAutoPlay');
end.
