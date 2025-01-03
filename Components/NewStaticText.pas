unit NewStaticText;

{
  TNewStaticText - similar to TStaticText but with multi-line AutoSize
  support and a WordWrap property
}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms;

type
  TNewStaticText = class(TWinControl)
  private
    FAutoSize: Boolean;
    FFocusControl: TWinControl;
    FForceLTRReading: Boolean;
    FLastAdjustBoundsRTL: Boolean;
    FShowAccelChar: Boolean;
    FWordWrap: Boolean;
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
    procedure SetWordWrap(Value: Boolean);
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
    property TabOrder;
    property TabStop;
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

procedure Register;

implementation

uses
  BidiUtils;

procedure Register;
begin
  RegisterComponents('JR', [TNewStaticText]);
end;

{ TNewStaticText }

constructor TNewStaticText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption,
    csOpaque, csReplicatable, csDoubleClicks];
  Width := 65;
  Height := 17;
  FAutoSize := True;
  FShowAccelChar := True;
  AdjustBounds;
end;

procedure TNewStaticText.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'STATIC');
  with Params do
  begin
    Style := Style or SS_NOTIFY;
    if not SetBiDiStyles(Self, Params) then begin
      { Quirk: No style is set for WordWrap=False in RTL mode; WS_EX_RIGHT
        overrides SS_LEFTNOWORDWRAP, and there is no SS_RIGHTNOWORDWRAP style.
        WordWrap=False still affects AdjustBounds, though. }
      if not FWordWrap then Style := Style or SS_LEFTNOWORDWRAP;
    end;
    if not FShowAccelChar then Style := Style or SS_NOPREFIX;
    if FForceLTRReading then ExStyle := ExStyle and not WS_EX_RTLREADING;
  end;
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
  if IsParentRightToLeft(Self) <> FLastAdjustBoundsRTL then
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
  if IsParentRightToLeft(Self) then begin
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
  if (S = '') or (FShowAccelChar and (S[1] = '&') and (S[2] = #0)) then
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
    FLastAdjustBoundsRTL := IsParentRightToLeft(Self);

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

procedure TNewStaticText.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    RecreateWnd;
    AdjustBounds;
  end;
end;

end.
