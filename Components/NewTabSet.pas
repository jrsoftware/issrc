unit NewTabSet;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TNewTabSet - modern VS-style tabs with theme support
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math, Generics.Collections,
  ModernColors, NewUxTheme;

type
  TTabPosition = (tpTop, tpBottom);

  TBoolList = TList<Boolean>;

  TCloseButtonClickEvent = procedure(Sender: TObject; Index: Integer) of object;

  TNewTabSet = class(TCustomControl)
  private
    FCloseButtons: TBoolList;
    FHints: TStrings;
    FMenuThemeData: HTHEME;
    FOnCloseButtonClick: TCloseButtonClickEvent;
    FTabs: TStrings;
    FTabIndex: Integer;
    FTabPosition: TTabPosition;
    FTabsOffset: Integer;
    FTheme: TTheme;
    FThemeDark: Boolean;
    FHotIndex: Integer;
    procedure EnsureCurrentTabIsFullyVisible;
    function GetTabRect(const Index: Integer; const ApplyTabsOffset: Boolean = True): TRect;
    function GetCloseButtonRect(const TabRect: TRect): TRect;
    procedure InvalidateTab(Index: Integer);
    procedure CloseButtonsListChanged(Sender: TObject; const Item: Boolean;
      Action: TCollectionNotification);
    procedure TabsListChanged(Sender: TObject);
    procedure HintsListChanged(Sender: TObject);
    procedure SetCloseButtons(Value: TBoolList);
    procedure SetTabs(Value: TStrings);
    procedure SetTabIndex(Value: Integer);
    procedure SetTabPosition(Value: TTabPosition);
    procedure SetTheme(Value: TTheme);
    procedure SetHints(const Value: TStrings);
    function ToCurrentPPI(const XY: Integer): Integer;
    procedure UpdateThemeData(const Open: Boolean);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure UpdateHotIndex(NewHotIndex: Integer);
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CloseButtons: TBoolList read FCloseButtons write SetCloseButtons;
    property Theme: TTheme read FTheme write SetTheme;
  published
    property Align;
    property AutoSize default True;
    property Font;
    property Hints: TStrings read FHints write SetHints;
    property ParentFont;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
    property Tabs: TStrings read FTabs write SetTabs;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition default tpBottom;
    property PopupMenu;
    property OnClick;
    property OnCloseButtonClick: TCloseButtonClickEvent read FOnCloseButtonClick write FOnCloseButtonClick;
  end;

procedure Register;

implementation

uses
  Types;

procedure Register;
begin
  RegisterComponents('JR', [TNewTabSet]);
end;

procedure RGBToHSV(const R, G, B: Integer; var H, S: Double; var V: Integer);
var
  Max, Min, C: Integer;
begin
  Max := R;
  if G > Max then Max := G;
  if B > Max then Max := B;
  Min := R;
  if G < Min then Min := G;
  if B < Min then Min := B;

  C := Max - Min;
  if C = 0 then begin
    H := 0;
    S := 0;
  end
  else begin
    if Max = R then
      H := (60 * (G - B)) / C
    else if Max = G then
      H := (60 * (B - R)) / C + 120
    else if Max = B then
      H := (60 * (R - G)) / C + 240;
    if H < 0 then
      H := H + 360;
    S := C / Max;
  end;
  V := Max;
end;

procedure HSVtoRGB(const H, S: Double; const V: Integer; var R, G, B: Integer);
var
  I, P, Q, T: Integer;
  F: Double;
begin
  I := Trunc(H / 60);
  F := Frac(H / 60);
  P := Round(V * (1.0 - S));
  Q := Round(V * (1.0 - S * F));
  T := Round(V * (1.0 - S * (1.0 - F)));
  case I of
    0: begin R := V; G := t; B := p; end;
    1: begin R := q; G := V; B := p; end;
    2: begin R := p; G := V; B := t; end;
    3: begin R := p; G := q; B := V; end;
    4: begin R := t; G := p; B := V; end;
    5: begin R := V; G := p; B := q; end;
  else
    { Should only get here with bogus input }
    R := 0; G := 0; B := 0;
  end;
end;

function LightenColor(const Color: TColorRef; const Amount: Integer): TColorRef;
var
  H, S: Double;
  V, R, G, B: Integer;
begin
  RGBtoHSV(Byte(Color), Byte(Color shr 8), Byte(Color shr 16), H, S, V);
  Inc(V, Amount);
  if V > 255 then
    V := 255;
  if V < 0 then
    V := 0;
  HSVtoRGB(H, S, V, R, G, B);
  Result := R or (G shl 8) or (B shl 16);
end;

{ TNewTabSet }

const
  TabSetMarginX = 4;
  TabPaddingX = 5;
  TabPaddingY = 3;
  CloseButtonSizeX = 12;

constructor TNewTabSet.Create(AOwner: TComponent);
begin
  inherited;
  FCloseButtons := TBoolList.Create;
  FCloseButtons.OnNotify := CloseButtonsListChanged;
  FTabs := TStringList.Create;
  TStringList(FTabs).OnChange := TabsListChanged;
  FTabPosition := tpBottom;
  FHints := TStringList.Create;
  TStringList(FHints).OnChange := HintsListChanged;
  FHotIndex := -1;
  ControlStyle := ControlStyle + [csOpaque];
  Width := 129;
  Height := 21;
  AutoSize := True;
end;

procedure TNewTabSet.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params.WindowClass do
    style := style and not CS_HREDRAW;
end;

procedure TNewTabSet.CreateWnd;
begin
  inherited;
  UpdateThemeData(True);
end;

destructor TNewTabSet.Destroy;
begin
  UpdateThemeData(False);
  FHints.Free;
  FTabs.Free;
  FCloseButtons.Free;
  inherited;
end;

procedure TNewTabSet.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if AutoSize then
    AdjustSize;
end;

procedure TNewTabSet.CMHintShow(var Message: TCMHintShow);
var
  I: Integer;
  R: TRect;
begin
  inherited;
  if Message.HintInfo.HintControl = Self then begin
    for I := 0 to FTabs.Count-1 do begin
      if I >= FHints.Count then
        Break;
      R := GetTabRect(I);
      if PtInRect(R, Message.HintInfo.CursorPos) then begin
        Message.HintInfo.HintStr := FHints[I];
        Message.HintInfo.CursorRect := R;
        Break;
      end;
    end;
  end;
end;

procedure TNewTabSet.WMMouseMove(var Message: TWMMouseMove);
begin
  var Pos := SmallPointToPoint(Message.Pos);
  var NewHotIndex := -1;

  for var I := 0 to FTabs.Count-1 do begin
    if I <> TabIndex then begin
      var R := GetTabRect(I);
      if PtInRect(R, TPoint.Create(Pos.X, Pos.Y)) then begin
        NewHotIndex := I;
        Break;
      end;
    end;
  end;

  UpdateHotIndex(NewHotIndex);
end;

procedure TNewTabSet.WMThemeChanged(var Message: TMessage);
begin
  { Don't Run to Cursor into this function, it will interrupt up the theme change }
  UpdateThemeData(True);
  inherited;
end;

procedure TNewTabSet.EnsureCurrentTabIsFullyVisible;
begin
  const AdjacentTabVisiblePixels = ToCurrentPPI(30);
  const CR = ClientRect;
  const R = GetTabRect(FTabIndex, False);
  var Offset := FTabsOffset;

  { If the tab is overflowing to the right, scroll right }
  var Overflow := R.Right - Offset - CR.Right + AdjacentTabVisiblePixels;
  if Overflow > 0 then
    Inc(Offset, Overflow);

  { If there's extra space after the last tab, scroll left if possible }
  const LastTabRight = GetTabRect(FTabs.Count-1, False).Right +
    ToCurrentPPI(TabSetMarginX);
  Offset := Min(Offset, Max(0, LastTabRight - CR.Right));

  { If the tab is overflowing to the left, scroll left }
  Overflow := Offset - R.Left + AdjacentTabVisiblePixels;
  if Overflow > 0 then
    Offset := Max(0, Offset - Overflow);

  if FTabsOffset <> Offset then begin
    FTabsOffset := Offset;
    Invalidate;
  end;
end;

function TNewTabSet.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  { We need to manage our own height for correct results with non-default PPI }
  Canvas.Font.Assign(Font);
  NewHeight := Canvas.TextHeight('0') + (ToCurrentPPI(TabPaddingY) * 2) +
    ToCurrentPPI(2);
  Result := True;
end;

function TNewTabSet.GetTabRect(const Index: Integer;
  const ApplyTabsOffset: Boolean = True): TRect;
var
  CR: TRect;
  I, SizeX, SizeY: Integer;
  Size: TSize;
begin
  CR := ClientRect;
  Canvas.Font.Assign(Font);
  if FTabPosition = tpBottom then
    Result.Top := 0;
  Result.Right := ToCurrentPPI(TabSetMarginX);
  if ApplyTabsOffset then
    Dec(Result.Right, FTabsOffset);
  for I := 0 to FTabs.Count-1 do begin
    Size := Canvas.TextExtent(FTabs[I]);
    SizeX := Size.cx + (ToCurrentPPI(TabPaddingX) * 2);
    if (I < FCloseButtons.Count) and FCloseButtons[I] then
      Inc(SizeX, ToCurrentPPI(CloseButtonSizeX));
    SizeY := Size.cy + (ToCurrentPPI(TabPaddingY) * 2);
    if FTabPosition = tpTop then
      Result.Top := CR.Bottom - SizeY;
    Result := Bounds(Result.Right, Result.Top, SizeX, SizeY);
    if Index = I then
      Exit;
  end;
  SetRectEmpty(Result);
end;

function TNewTabSet.GetCloseButtonRect(const TabRect: TRect): TRect;
begin
  Result := TRect.Create(TabRect.Right - ToCurrentPPI(CloseButtonSizeX) - ToCurrentPPI(TabPaddingX) div 2,
    TabRect.Top, TabRect.Right - ToCurrentPPI(TabPaddingX) div 2, TabRect.Bottom);
end;

procedure TNewTabSet.InvalidateTab(Index: Integer);
var
  R: TRect;
begin
  if HandleAllocated and (Index >= 0) and (Index < FTabs.Count) then begin
    R := GetTabRect(Index);
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TNewTabSet.CloseButtonsListChanged(Sender: TObject; const Item: Boolean;
  Action: TCollectionNotification);
begin
  FHotIndex := -1;
  Invalidate;
end;

procedure TNewTabSet.TabsListChanged(Sender: TObject);
begin
  FHotIndex := -1;
  Invalidate;
end;

procedure TNewTabSet.HintsListChanged(Sender: TObject);
begin
  ShowHint := FHints.Count > 0;
end;

procedure TNewTabSet.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  I: Integer;
  R: TRect;
begin
  if Button = mbLeft then begin
    for I := 0 to FTabs.Count-1 do begin
      R := GetTabRect(I);
      if (X >= R.Left) and (X < R.Right) then begin
        if ((I = TabIndex) or (I = FHotIndex)) and (I < FCloseButtons.Count) and FCloseButtons[I] then begin
          var R2 := GetCloseButtonRect(R);
          if PtInRect(R2, TPoint.Create(X, Y)) then begin
            if Assigned(OnCloseButtonClick) then
              OnCloseButtonClick(Self, I);
            Break;
          end;
        end;
        TabIndex := I;
        Break;
      end;
    end;
  end;
end;

procedure TNewTabSet.UpdateHotIndex(NewHotIndex: Integer);
begin
  var OldHotIndex := FHotIndex;
  if NewHotIndex <> OldHotIndex then begin
    FHotIndex := NewHotIndex;
    if OldHotIndex <> -1 then
      InvalidateTab(OldHotIndex);
    if NewHotIndex <> -1 then
      InvalidateTab(NewHotIndex);
  end;
end;

procedure TNewTabSet.CMMouseLeave(var Message: TMessage);
begin
  UpdateHotIndex(-1);
  inherited;
end;

procedure TNewTabSet.Paint;
var
  HighColorMode: Boolean;

  procedure DrawCloseButton(const TabRect: TRect; const TabIndex: Integer);
  const
    MENU_SYSTEMCLOSE = 17;
    MSYSC_NORMAL = 1;
  begin
   if (TabIndex < FCloseButtons.Count) and FCloseButtons[TabIndex] then begin
      var R := GetCloseButtonRect(TabRect);
      if FMenuThemeData <> 0 then begin
        var Offset := ToCurrentPPI(1);
        Inc(R.Left, Offset);
        Inc(R.Top, Offset);
        DrawThemeBackground(FMenuThemeData, Canvas.Handle, MENU_SYSTEMCLOSE, MSYSC_NORMAL, R, nil);
      end else begin
        InflateRect(R, -ToCurrentPPI(3), -ToCurrentPPI(6));
        Canvas.Pen.Color := Canvas.Font.Color;
        Canvas.MoveTo(R.Left, R.Top);
        Canvas.LineTo(R.Right, R.Bottom);
        Canvas.MoveTo(R.Left, R.Bottom-1);
        Canvas.LineTo(R.Right, R.Top-1);
      end;
    end;
  end;

  procedure DrawTabs(const SelectedTab: Boolean);
  var
    I: Integer;
    R: TRect;
  begin
    for I := 0 to FTabs.Count-1 do begin
      R := GetTabRect(I);
      if SelectedTab and (FTabIndex = I) then begin
        if FTheme <> nil then
          Canvas.Brush.Color := FTheme.Colors[tcBack]
        else
          Canvas.Brush.Color := clBtnFace;
        Canvas.FillRect(R);

        if FTheme <> nil then
          Canvas.Font.Color := FTheme.Colors[tcFore]
        else
          Canvas.Font.Color := clBtnText;
        Canvas.TextOut(R.Left + ToCurrentPPI(TabPaddingX), R.Top + ToCurrentPPI(TabPaddingY), FTabs[I]);
        DrawCloseButton(R, I);
        ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
        Break;
      end;
      if not SelectedTab and (FTabIndex <> I) then begin
        if FHotIndex = I then begin
          if FTheme <> nil then
            Canvas.Font.Color := FTheme.Colors[tcFore]
          else
            Canvas.Font.Color := clBtnText;
        end else if FTheme <> nil then
          Canvas.Font.Color := FTheme.Colors[tcMarginFore]
        else if HighColorMode and (ColorToRGB(clBtnFace) <> clBlack) then
          Canvas.Font.Color := LightenColor(ColorToRGB(clBtnShadow), -43)
        else begin
          { If the button face color is black, or if running in low color mode,
            use plain clBtnHighlight as the text color }
          Canvas.Font.Color := clBtnHighlight;
        end;
        Canvas.TextOut(R.Left + ToCurrentPPI(TabPaddingX), R.Top + ToCurrentPPI(TabPaddingY), FTabs[I]);
        if FHotIndex = I then
          DrawCloseButton(R, I);
      end;
    end;
  end;

var
  CR: TRect;
begin
  Canvas.Font.Assign(Font);

  HighColorMode := (GetDeviceCaps(Canvas.Handle, BITSPIXEL) *
    GetDeviceCaps(Canvas.Handle, PLANES)) >= 15;

  CR := ClientRect;

  { Work around an apparent NT 4.0/2000/??? bug. If the width of the DC is
    greater than the width of the screen, then any call to ExcludeClipRect
    inexplicably shrinks the DC's clipping rectangle to the screen width.
    Calling IntersectClipRect first with the entire client area as the
    rectangle solves this (don't ask me why). }
  IntersectClipRect(Canvas.Handle, CR.Left, CR.Top, CR.Right, CR.Bottom);

  { Selected tab }
  DrawTabs(True);

  { Top or bottom line }
  if FTheme <> nil then
    Canvas.Brush.Color := FTheme.Colors[tcBack]
  else
    Canvas.Brush.Color := clBtnFace;
  const LineRectHeight = ToCurrentPPI(1);
  var LineRect := CR;
  if FTabPosition = tpBottom then
    LineRect.Bottom := LineRect.Top + LineRectHeight
  else
    LineRect.Top := LineRect.Bottom - LineRectHeight;
  Canvas.FillRect(LineRect);

  { Background fill }
  if FTheme <> nil then
    Canvas.Brush.Color := FTheme.Colors[tcMarginBack]
  else if HighColorMode then
    Canvas.Brush.Color := LightenColor(ColorToRGB(clBtnFace), 35)
  else
    Canvas.Brush.Color := clBtnShadow;
  if FTabPosition = tpBottom then
    Inc(CR.Top, LineRectHeight)
  else
    Dec(CR.Bottom, LineRectHeight);
  Canvas.FillRect(CR);

  { Non-selected tabs }
  DrawTabs(False);
end;

procedure TNewTabSet.Resize;
begin
  EnsureCurrentTabIsFullyVisible;
  inherited;
end;

procedure TNewTabSet.SetCloseButtons(Value: TBoolList);
begin
  FCloseButtons.Clear;
  for var V in Value do
    FCloseButtons.Add(V);
end;

procedure TNewTabSet.SetHints(const Value: TStrings);
begin
  FHints.Assign(Value);
end;

procedure TNewTabSet.SetTabIndex(Value: Integer);
begin
  if FTabIndex <> Value then begin
    InvalidateTab(FTabIndex);
    FTabIndex := Value;
    InvalidateTab(Value);
    EnsureCurrentTabIsFullyVisible;
    Click;
  end;
end;

procedure TNewTabSet.SetTabPosition(Value: TTabPosition);
begin
  if FTabPosition <> Value then begin
    FTabPosition := Value;
    Invalidate;
  end;
end;

procedure TNewTabSet.SetTabs(Value: TStrings);
begin
  FTabs.Assign(Value);
  if FTabIndex >= FTabs.Count then
    SetTabIndex(FTabs.Count-1);
end;

procedure TNewTabSet.SetTheme(Value: TTheme);
begin
  if FTheme <> Value then begin
    FTheme := Value;
    var NewThemeDark := (FTheme <> nil) and FTheme.Dark;
    if FThemeDark <> NewThemeDark then
      UpdateThemeData(True);
    FThemeDark := NewThemeDark;
    Invalidate;
  end;
end;

function TNewTabSet.ToCurrentPPI(const XY: Integer): Integer;
begin
  Result := MulDiv(XY, CurrentPPI, 96);
end;

procedure TNewTabSet.UpdateThemeData(const Open: Boolean);
begin
  if FMenuThemeData <> 0 then begin
    CloseThemeData(FMenuThemeData);
    FMenuThemeData := 0;
  end;

  if Open and UseThemes then begin
    if (FTheme <> nil) and FTheme.Dark then
      FMenuThemeData := OpenThemeData(Handle, 'DarkMode::Menu');
    if FMenuThemeData = 0 then
      FMenuThemeData := OpenThemeData(Handle, 'Menu');
  end;
end;

end.
