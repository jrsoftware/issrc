unit NewStaticText;

{
  TNewStaticText - similar to TStaticText but with multi-line AutoSize
  support and a WordWrap property, and without a Transparent property.

  Define VCLSTYLES for full VCL Styles support, and for transparency support.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  {$IFDEF VCLSTYLES} Vcl.Themes, {$ELSE} Themes, {$ENDIF}
  Graphics;

{$IFDEF VCLSTYLES}
  {$DEFINE TRANSPARENCYSUPPORT}
{$ENDIF}

type
  TNewStaticText = class(TWinControl)
  private
    FAutoSize: Boolean;
    FFocusControl: TWinControl;
    FForceLTRReading: Boolean;
    FLastAdjustBoundsRTL: Boolean;
    FShowAccelChar: Boolean;
    {$IFNDEF TRANSPARENCYSUPPORT}
    FTransparent: Boolean;
    {$ENDIF}
    FWordWrap: Boolean;
    class constructor Create;
    class destructor Destroy;
    {$IFDEF TRANSPARENCYSUPPORT}
    procedure CNCtlColorStatic(var Message: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    {$ENDIF}
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure AdjustBounds;
    function CalcBounds: TPoint;
    function GetDrawTextFlags: UINT;
    procedure SetFocusControl(Value: TWinControl);
    procedure SetForceLTRReading(Value: Boolean);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
    function GetTransparent: Boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAutoSize(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    function AdjustHeight: Integer;
  published
    property Align;
    property Anchors;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property Font;
    property ForceLTRReading: Boolean read FForceLTRReading write SetForceLTRReading
      default False;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar
      default True;
    property ShowHint;
    property StyleElements;
    property StyleName;
    property TabOrder;
    property TabStop;
    property Transparent: Boolean read GetTransparent write SetTransparent
      default True;
    property Visible;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
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

  TNewStaticTextStyleHook = class(TStyleHook)
{$IFDEF VCLSTYLES}
  strict protected
    procedure Paint(Canvas: TCanvas); override;
  public
    constructor Create(AControl: TWinControl); override;
{$ENDIF}
  end;

procedure Register;

implementation

uses
  StdCtrls, Types, BidiUtils;

procedure Register;
begin
  RegisterComponents('JR', [TNewStaticText]);
end;

{ TNewStaticText }

constructor TNewStaticText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption,
    csReplicatable, csDoubleClicks, csGestures {$IF CompilerVersion >= 35.0}, csNeedsDesignDisabledState{$ENDIF}];
  {$IFNDEF TRANSPARENCYSUPPORT}
  ControlStyle := ControlStyle + [csOpaque];
  FTransparent := True;  { Even though Transparent is ignored, we still want it to work as if it isn't at design time }
  {$ENDIF}
  Width := 65;
  Height := 17;
  FAutoSize := True;
  FShowAccelChar := True;
  AdjustBounds;
end;

class constructor TNewStaticText.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TNewStaticText, TNewStaticTextStyleHook);
end;

procedure TNewStaticText.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'STATIC');
  with Params do
  begin
    Style := Style or SS_NOTIFY;
    if ExStyle and WS_EX_RIGHT = 0 then begin
      { Quirk: No style is set for WordWrap=False in RTL mode; WS_EX_RIGHT
        overrides SS_LEFTNOWORDWRAP, and there is no SS_RIGHTNOWORDWRAP style.
        WordWrap=False still affects AdjustBounds, though. }
      if not FWordWrap then Style := Style or SS_LEFTNOWORDWRAP;
    end;
    if not FShowAccelChar then Style := Style or SS_NOPREFIX;
    if FForceLTRReading then ExStyle := ExStyle and not WS_EX_RTLREADING;
    WindowClass.style := WindowClass.style and not CS_VREDRAW;
  end;
end;

class destructor TNewStaticText.Destroy;
begin
  TCustomStyleEngine.UnregisterStyleHook(TNewStaticText, TNewStaticTextStyleHook);
end;

procedure TNewStaticText.CMDialogChar(var Message: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and
    IsAccel(Message.CharCode, Caption) then
    with FFocusControl do
      if CanFocus then
      begin
        SetFocus;
        Message.Result := 1;
      end;
end;

procedure TNewStaticText.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TNewStaticText.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  { What we're really trapping here is changes to Parent. Recalculate size
    if the new Parent's RTL setting is different. }
  if IsRightToLeft <> FLastAdjustBoundsRTL then
    AdjustBounds;
end;

procedure TNewStaticText.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
  AdjustBounds;
end;

procedure TNewStaticText.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

function TNewStaticText.GetDrawTextFlags: UINT;
begin
  Result := DT_EXPANDTABS or DT_NOCLIP;
  if FWordWrap then Result := Result or DT_WORDBREAK;
  if not FShowAccelChar then Result := Result or DT_NOPREFIX;
  if IsRightToLeft then begin
    { Note: DT_RTLREADING must be included even when just calculating the
      size, since on certain fonts it can affect the width of characters.
      (Consider the Hebrew string: 'a '#$F9' b'. On 2000 with Lucida Console
      as the font, the spaces aren't drawn as wide with RTLREADING.) }
    Result := Result or DT_RIGHT;
    if not FForceLTRReading then
      Result := Result or DT_RTLREADING;
  end;
end;

function TNewStaticText.CalcBounds: TPoint;
var
  R: TRect;
  S: String;
  DC: HDC;
begin
  { Note: The calculated width/height is actually one pixel wider/taller
    than the size of the text, so that when Enabled=False the white shadow
    does not get clipped }
  R := Rect(0, 0, Width, 0);
  if R.Right > 0 then Dec(R.Right);

  S := Caption;
  if (S = '') or (FShowAccelChar and (S[1] = '&') and (Length(S) = 1)) then
    S := S + ' ';

  DC := GetDC(0);
  try
    SelectObject(DC, Font.Handle);
    DrawText(DC, PChar(S), Length(S), R, DT_CALCRECT or GetDrawTextFlags);
  finally
    ReleaseDC(0, DC);
  end;

  Result.X := R.Right + 1;
  Result.Y := R.Bottom + 1;
end;

procedure TNewStaticText.AdjustBounds;
var
  NewBounds: TPoint;
  NewLeft, NewWidth: Integer;
begin
  if not (csLoading in ComponentState) and FAutoSize then
  begin
    FLastAdjustBoundsRTL := IsRightToLeft;

    NewBounds := CalcBounds;

    NewLeft := Left;
    NewWidth := Width;
    if not FWordWrap then begin
      NewWidth := NewBounds.X;
      if IsParentFlipped(Self) then
        Inc(NewLeft, Width - NewWidth);
    end;
    SetBounds(NewLeft, Top, NewWidth, NewBounds.Y);
  end;
end;

function TNewStaticText.AdjustHeight: Integer;
var
  OldHeight: Integer;
begin
  OldHeight := Height;
  Height := CalcBounds.Y;
  Result := Height - OldHeight;
end;

procedure TNewStaticText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TNewStaticText.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if Value then AdjustBounds;
  end;
end;

procedure TNewStaticText.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TNewStaticText.SetForceLTRReading(Value: Boolean);
begin
  if FForceLTRReading <> Value then begin
    FForceLTRReading := Value;
    RecreateWnd;
    AdjustBounds;
  end;
end;

procedure TNewStaticText.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    RecreateWnd;
    AdjustBounds;
  end;
end;

{$IFDEF TRANSPARENCYSUPPORT}
procedure TNewStaticText.CNCtlColorStatic(var Message: TWMCtlColorStatic);
begin
  { Vcl.StdCtrls' TCustomStaticText.CNCtlColorStatic uses StyleServices(Self).Enabled
    instead of IsCustomStyleActive, but that can return True even when no style is active,
    which is undesirable because when no style is active the inherited call is required to
    support the Font and Brush colors }
  if IsCustomStyleActive and Transparent then
  begin
    SetBkMode(Message.ChildDC, Windows.TRANSPARENT);
    StyleServices(Self).DrawParentBackground(Handle, Message.ChildDC, nil, False);
    { Return an empty brush to prevent Windows from overpainting what we just have created. }
    Message.Result := GetStockObject(NULL_BRUSH);
  end
  else
    inherited;
end;
{$ENDIF}

procedure TNewStaticText.SetTransparent(const Value: Boolean);
begin
{$IFDEF TRANSPARENCYSUPPORT}
  if Transparent <> Value then
  begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
{$ELSE}
  FTransparent := Value;
{$ENDIF}
end;

function TNewStaticText.GetTransparent: Boolean;
begin
{$IFDEF TRANSPARENCYSUPPORT}
  Result := not (csOpaque in ControlStyle);
{$ELSE}
  Result := FTransparent;
{$ENDIF}
end;

procedure TNewStaticText.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    RecreateWnd;
    AdjustBounds;
  end;
end;

{$IFDEF VCLSTYLES}

{ TNewStaticTextStyleHook - same as Vcl.StdCtrls' TStaticTextStyleHook
  except that it accesses the Control property as a TNewStaticText instead
  of a TCustomStaticText or TStaticText }

type
  TControlAccess = class(TControl);

constructor TNewStaticTextStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverridePaint := True;
  OverrideEraseBkgnd := True;
  PaintOnEraseBkgnd := True;
  DoubleBuffered := True;
end;

procedure TNewStaticTextStyleHook.Paint(Canvas: TCanvas);
const
  States: array[Boolean] of TThemedTextLabel = (ttlTextLabelDisabled, ttlTextLabelNormal);
var
  Details: TThemedElementDetails;
  R: TRect;
  S: String;
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;

  if LStyle.Available then begin
    R := Control.ClientRect;
    {$IFDEF TRANSPARENCYSUPPORT}
    if TNewStaticText(Control).Transparent then begin
      Details := LStyle.GetElementDetails(tbCheckBoxUncheckedNormal);
      LStyle.DrawParentBackground(Handle, Canvas.Handle, Details, False);
      Canvas.Brush.Style := bsClear;
    end else {$ENDIF} begin
      Canvas.Brush.Color := LStyle.GetStyleColor(scWindow);
      Canvas.FillRect(R);
    end;
    Details := LStyle.GetElementDetails(States[Control.Enabled]);
    S := TNewStaticText(Control).Caption;
    if (S = '') or (TNewStaticText(Control).FShowAccelChar and (S[1] = '&') and (S[2] = #0)) then
      S := S + ' ';
    if seFont in Control.StyleElements then
      DrawControlText(Canvas, Details, S, R, TNewStaticText(Control).GetDrawTextFlags)
    else begin
      Canvas.Font := TNewStaticText(Control).Font;
      DrawText(Canvas.Handle, S, Length(S), R, TNewStaticText(Control).GetDrawTextFlags);
    end;
  end;
end;

{$ENDIF}

end.
