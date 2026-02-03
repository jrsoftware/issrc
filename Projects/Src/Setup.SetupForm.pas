unit Setup.SetupForm;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TSetupForm

  Also used by UninstallProgressForm and UninstallSharedFileForm!

  Requires following globals to be set:
  -IsDarkInstallMode
  -LangOptions.RightToLeft
  -LangOptions.DialogFontName
  -LangOptions.DialogFontSize
  -LangOptions.DialogFontBaseScaleWidth
  -LangOptions.DialogFontBaseScaleHeight
  -SetupHeader.WizardLightControlStyling
  -shWizardKeepAspectRatio in SetupHeader.Options
  Also requires following globals to be set, but 0 is allowed:
  -SetupHeader.WizardSizePercentX
  -SetupHeader.WizardSizePercentY
}

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  UIStateForm,
  Setup.MainFunc;

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
    FOrigClientWidthAfterScale, FOrigClientHeightAfterScale: Integer;
    FSetForeground: Boolean;
    FDidDisableChildControlsStylesAsNeeded: Boolean;
    class function ShouldDisableControlStylesAsNeeded: Boolean;
    class procedure DisableControlStyleAsNeeded(const Ctl: TControl); static;
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
    class procedure SetCtlParent(const AControl: TControl; const AParent: TWinControl); static;
    function ScalePixelsX(const N: Integer): Integer; overload;
    function ScalePixelsY(const N: Integer): Integer; overload;
    procedure SetBackImage(const BackImages: TWizardImages; const Stretch, Center: Boolean; const Opacity: Byte; const Redraw: Boolean); overload;
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
  Generics.Collections, UITypes, WinXPanels, Themes, StdCtrls, ExtCtrls,
  BidiUtils, BitmapButton, BitmapImage, NewNotebook, NewStaticText, NewCheckListBox, FormBackgroundStyleHook,
  Shared.Struct, Shared.CommonFunc, Shared.CommonFunc.Vcl,
  Setup.InstFunc;

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
    Result := Integer(GetDPIForWindow(Wnd))
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
  W: Integer;
begin
  Result := ScalePixelsX(75);
  { Increase the button size if there are unusually long button captions }
  DC := GetDC(0);
  try
    SelectObject(DC, Font.Handle);
    for var I := Low(ButtonCaptions) to High(ButtonCaptions) do begin
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

class function TSetupForm.ShouldDisableControlStylesAsNeeded: Boolean;
begin
  Result := not IsDarkInstallMode and (SetupHeader.WizardLightControlStyling <> wcsAll);
  if Result then begin
    const LStyle = StyleServices;
    Result := LStyle.Enabled and not LStyle.IsSystemStyle;
  end;
end;

class procedure TSetupForm.DisableControlStyleAsNeeded(const Ctl: TControl);
{ Call ShouldDisableControlStylesAsNeeded first }
begin
  { SetupHeader.WizardLightControlStyling is either wcsAllButButtons or wcsOnlyRequired,
    so for buttons the style must always be disabled. }
  if Ctl is TCustomButton then
    Ctl.StyleName := TStyleManager.SystemStyleName
  else if SetupHeader.WizardLightControlStyling = wcsOnlyRequired then begin
    if (Ctl is TNewCheckListBox) and TNewCheckListBox(Ctl).TransparentIfStyled then begin
      { Requires VCL Styles for transparency, but can be told to use native checkboxes and radiobuttons }
      TNewCheckListBox(Ctl).DisableStyledButtons := True;
    end else begin
      const KeepStyle =
        (Ctl is TCustomPanel) or
        (Ctl is TBitmapButton) or (Ctl is TBitmapImage) or             { Don't use VCL Styles }
        (Ctl is TNewNotebook) or (Ctl is TNewNotebookPage) or          { Don't use VCL Styles }
        ((Ctl is TNewStaticText) and TNewStaticText(Ctl).Transparent); { Requires VCL Styles for transparency }
      if not KeepStyle then
        Ctl.StyleName := TStyleManager.SystemStyleName;
    end;
  end;
end;

procedure TSetupForm.CreateWnd;

  procedure DisableChildControlsStylesAsNeeded(const ParentCtl: TWinControl);
  { Call ShouldDisableControlStylesAsNeeded first }
  begin
    for var I := 0 to ParentCtl.ControlCount-1 do begin
      const Ctl = ParentCtl.Controls[I];

      if Ctl is TWinControl then begin
        const WinCtl = Ctl as TWinControl;
        { Sanity check that the control's handle isn't already allocated,
          because otherwise it would run TWinControl.UpdateStyleElements
          which does a RecreateWnd. Might work but isn't efficient. }
        if WinCtl.HandleAllocated then
          InternalError('Unexpected HandleAllocated');
        { Update children }
        DisableChildControlsStylesAsNeeded(WinCtl);
      end;

      { Update self }
      DisableControlStyleAsNeeded(Ctl);
    end;
  end;

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
  { DisableChildControlsStylesAsNeeded works both before and after
    calling inherited. But it does require the child controls to have
    no handle allocated, which is why it's in CreateWnd and not in
    Create: in Create it can't be before inherited since it wouldn't
    yet know about the children, and also not after since the
    handles might be allocated. }
  if ShouldDisableControlStylesAsNeeded and not FDidDisableChildControlsStylesAsNeeded then begin
    DisableChildControlsStylesAsNeeded(Self);
    { Don't need to disable again if the window is recreated }
    FDidDisableChildControlsStylesAsNeeded := True;
  end;

  inherited;

  if WM_QueryCancelAutoPlay <> 0 then
    AddToWindowMessageFilterEx(Handle, WM_QueryCancelAutoPlay);

  { SetDarkTitleBar requires Handle to be allocated, which is why we
    we call it after calling inherited. }
  if (TStyleManager.FormBorderStyle = fbsSystemStyle) or not (seBorder in StyleElements) then
    SetDarkTitleBar(Self, IsDarkInstallMode);

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
    -Setup.WizardForm.CustomPages's SetCtlParent
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
    if ACenterInsideControl and (CenterInsideControlCtl <> nil) then
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

  procedure RestoreAnchors(const AnchorsList: TControlAnchorsList);
  begin
    { The order in which we restore the anchors shouldn't matter, so just
      enumerate the list }
    for var Item in AnchorsList do
      Item.Key.Anchors := Item.Value;
  end;

  function ExcludeFromParentHandlesNeeded(const ParentCtl: TWinControl): Boolean;
  begin
    { Right-aligned TStackPanels are excluded. For example, calling
      HandleNeeded on TaskDialogForm's BottomStackPanel causes it to become
      left-aligned instead of right-aligned. This occurs regardless of the
      timing of the HandleNeeded call, such as before or after sizing. }
    Result := (ParentCtl is TStackPanel) and (TStackPanel(ParentCtl).Align = alRight);
  end;

  procedure ParentHandlesNeeded(const ParentCtl: TControl);
  begin
    if ParentCtl is TWinControl then begin
      const ParentWinCtl = TWinControl(ParentCtl);
      if (ParentWinCtl.ControlCount > 0) and not ExcludeFromParentHandlesNeeded(ParentWinCtl) then begin
        if not (ParentWinCtl is TNewNotebook) then { For notebooks: only need handles on pages }
          ParentWinCtl.HandleNeeded;
        for var I := 0 to ParentWinCtl.ControlCount-1 do
          ParentHandlesNeeded(ParentWinCtl.Controls[I]);
      end;
    end;
  end;

begin
  { Create parent handles.

    Various things related to positioning and anchoring don't work without this:
    you get positions of child controls back as if there was no anchoring until
    handles are automatically created.

    Initially we did this only when sizing the form (for WizardForm it worked if
    done after sizing but for UninstallProgressForm it had be done before sizing,
    for unknown reasons).

    For WizardForm's BeveledLabel though, it needs it before the font name/size
    change (again for unknown reasons), otherwise the label will end up in the
    wrong position, even if all we do is changing the font. Setting AutoSize to
    False also causes it to stay in the correct position. (To see the bad
    positioning for WizardForm.BeveledLabel you would first have to disable next
    ParentHandlesNeeded call and then also the automatic vertical recentering in
    WizardForm.)

    Doing it always, instead of only before or after sizing, also helps
    TaskDialogForm which does its own sizing (so KeepSizeX and KeepSizeY are
    both True), but still needs parent handles to be created to avoid the issue.

    Note: Caller should make sure the created handles do not get lost again. For
    example, setting StyleElements to [] on a parent would cause the handle to
    get deallocated again, and would reintroduce the issue. So this must be done
    before calling us, and not after. }

  ParentHandlesNeeded(Self); { Also see ShowModal }

  { Set font. Note: Must keep the following lines in synch with Setup.ScriptFunc.pas's
    InitializeScaleBaseUnits }

  SetFontNameSize(Font, LangOptions.DialogFontName, LangOptions.DialogFontSize, '', 9);

  CalculateBaseUnitsFromFont(Font, FBaseUnitX, FBaseUnitY);

  FOrigBaseUnitX := LangOptions.DialogFontBaseScaleWidth;
  FOrigBaseUnitY := LangOptions.DialogFontBaseScaleHeight;

  if shWizardKeepAspectRatio in SetupHeader.Options then begin
    if FBaseUnitX * FOrigBaseUnitY > FBaseUnitY * FOrigBaseUnitX then begin
      FBaseUnitY := FBaseUnitX;
      FOrigBaseUnitY := FOrigBaseUnitX;
    end else begin
      FBaseUnitX := FBaseUnitY;
      FOrigBaseUnitX := FOrigBaseUnitY;
    end;
  end;

  { Scale }

  if (FBaseUnitX <> FOrigBaseUnitX) or (FBaseUnitY <> FOrigBaseUnitY) then begin
    const ControlAnchorsList = TControlAnchorsList.Create;
    try
      { Custom anchors interfere with our scaling code, so strip them and restore
        afterward }
      StripAndStoreChildControlCustomAnchors(Self, ControlAnchorsList);
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
  end;

  { Size }

  FKeepSizeX := KeepSizeX;
  FKeepSizeY := KeepSizeY;
  FOrigClientWidthAfterScale := ClientWidth;
  FOrigClientHeightAfterScale := ClientHeight;

  const LShouldSizeX = ShouldSizeX;
  const LShouldSizeY = ShouldSizeY;

  if LShouldSizeX then
    ClientWidth := MulDiv(ClientWidth, SetupHeader.WizardSizePercentX, 100);
  if LShouldSizeY then
    ClientHeight := MulDiv(ClientHeight, SetupHeader.WizardSizePercentY, 100);
end;

function TSetupForm.GetExtraClientWidth: Integer;
begin
  Result := ClientWidth - FOrigClientWidthAfterScale;
end;

function TSetupForm.GetExtraClientHeight: Integer;
begin
  Result := ClientHeight - FOrigClientHeightAfterScale;
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

procedure TSetupForm.SetBackImage(const BackImages: TWizardImages; const Stretch, Center: Boolean;
  const Opacity: Byte; const Redraw: Boolean);
begin
  if not CustomWizardBackground then
    InternalError('Cannot set a background image at this time: custom wizard background not active');
  const Graphic = SelectBestImage(BackImages, ClientWidth, ClientHeight);
  TFormBackgroundStyleHook.Graphic := Graphic;
  TFormBackgroundStyleHook.GraphicTarget := Self;
  TFormBackgroundStyleHook.Stretch := Stretch;
  TFormBackgroundStyleHook.Center := Center;
  TFormBackgroundStyleHook.Opacity := Opacity;
  TNewCheckListBox.ComplexParentBackground := Graphic <> nil;
  if Redraw and HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_UPDATENOW or RDW_ALLCHILDREN);
end;

class procedure TSetupForm.SetCtlParent(const AControl: TControl; const AParent: TWinControl);
{ To be called when a control is added after the form has already been created }
begin
  { Disable style if needed }
  if ShouldDisableControlStylesAsNeeded then
    DisableControlStyleAsNeeded(AControl);

  { Set CurrentPPI of the control to be parented to the CurrentPPI of the parent, preventing VCL
    from scaling the control. Also see TSetupForm.CreateWnd.  }
  AControl.SetCurrentPPI(AParent.CurrentPPI);
  AControl.Parent := AParent;
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
