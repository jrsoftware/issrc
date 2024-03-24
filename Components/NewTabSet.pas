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
  ModernColors, UxTheme;

type
  TTabPosition = (tpTop, tpBottom);

  TBoolList = TList<Boolean>;

  TNewTabSet = class(TCustomControl)
  private
    FCloseButtons: TBoolList;
    FHints: TStrings;
    FMenuThemeData: HTHEME;
    FOnCloseButtonClick: TNotifyEvent;
    FTabs: TStrings;
    FTabIndex: Integer;
    FTabPosition: TTabPosition;
    FTabsOffset: Integer;
    FTheme: TTheme;
    FThemeDark: Boolean;
    function GetTabRect(Index: Integer): TRect;
    function GetCloseButtonRect(const TabRect: TRect): TRect;
    procedure InvalidateTab(Index: Integer);
    procedure TabsListChanged(Sender: TObject);
    procedure SetCloseButtons(Value: TBoolList);
    procedure SetTabs(Value: TStrings);
    procedure SetTabIndex(Value: Integer);
    procedure SetTabPosition(Value: TTabPosition);
    procedure SetTheme(Value: TTheme);
    procedure SetHints(const Value: TStrings);
    procedure UpdateThemeData(const Open: Boolean);
    procedure EnsureCurrentTabIsFullyVisible;
  protected
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CloseButtons: TBoolList read FCloseButtons write SetCloseButtons;
    property Theme: TTheme read FTheme write SetTheme;
  published
    property Align;
    property Font;
    property Hints: TStrings read FHints write SetHints;
    property ParentFont;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
    property Tabs: TStrings read FTabs write SetTabs;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition default tpBottom;
    property PopupMenu;
    property OnClick;
    property OnCloseButtonClick: TNotifyEvent read FOnCloseButtonClick write FOnCloseButtonClick;
  end;

procedure Register;

implementation

uses
  WinApi.UxTheme;

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
  TabPaddingX = 5;
  TabPaddingY = 3;
  TabSpacing = 1;
  CloseButtonSizeX = 12;

constructor TNewTabSet.Create(AOwner: TComponent);
begin
  inherited;
  FCloseButtons := TBoolList.Create;
  FTabs := TStringList.Create;
  TStringList(FTabs).OnChange := TabsListChanged;
  FTabPosition := tpBottom;
  FHints := TStringList.Create;
  ControlStyle := ControlStyle + [csOpaque];
  Width := 129;
  Height := 21;
end;

procedure TNewTabSet.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TNewTabSet.CreateWnd;
begin
  inherited;
  UpdateThemeData(True);
end;

destructor TNewTabSet.Destroy;
begin
  UpdateThemeData(False);
  FTabs.Free;
  inherited;
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

procedure TNewTabSet.WMThemeChanged(var Message: TMessage);
begin
  { Don't Run to Cursor into this function, it will interrupt up the theme change }
  UpdateThemeData(True);
  inherited;
end;

function TNewTabSet.GetTabRect(Index: Integer): TRect;
var
  CR: TRect;
  I, SizeX, SizeY: Integer;
  Size: TSize;
begin
  CR := ClientRect;
  Canvas.Font.Assign(Font);
  if FTabPosition = tpBottom then
    Result.Top := 0;
  Result.Right := 4 - FTabsOffset;
  for I := 0 to FTabs.Count-1 do begin
    Size := Canvas.TextExtent(FTabs[I]);
    SizeX := Size.cx + (TabPaddingX * 2) + TabSpacing;
    if (I < FCloseButtons.Count) and FCloseButtons[I] then
      Inc(SizeX, MulDiv(CloseButtonSizeX, CurrentPPI, 96));
    SizeY := Size.cy + (TabPaddingY * 2);
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
  Result := TRect.Create(TabRect.Right - MulDiv(CloseButtonSizeX, CurrentPPI, 96) - TabPaddingX div 2,
    TabRect.Top, TabRect.Right - TabPaddingX div 2, TabRect.Bottom);
end;

procedure TNewTabSet.InvalidateTab(Index: Integer);
var
  R: TRect;
begin
  if HandleAllocated and (Index >= 0) and (Index < FTabs.Count) then begin
    R := GetTabRect(Index);
    { Inc R.Right since the trailing separator of a tab overwrites the first
      pixel of the next tab }
    Inc(R.Right);
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TNewTabSet.TabsListChanged(Sender: TObject);
begin
  Invalidate;
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
        if (I = TabIndex) and (I < FCloseButtons.Count) and FCloseButtons[I] then begin
          var R2 := GetCloseButtonRect(R);
          if PtInRect(R2, TPoint.Create(X, Y)) then begin
            if Assigned(OnCloseButtonClick) then
              OnCloseButtonClick(Self);
            Break;
          end;
        end;
        TabIndex := I;
        Break;
      end;
    end;
  end;
end;

procedure TNewTabSet.Paint;
var
  HighColorMode: Boolean;

  procedure DrawTabs(const SelectedTab: Boolean);
  var
    I: Integer;
    R: TRect;
  begin
    for I := 0 to FTabs.Count-1 do begin
      R := GetTabRect(I);
      if SelectedTab and (FTabIndex = I) then begin
        Dec(R.Right, TabSpacing);
        if FTheme <> nil then
          Canvas.Brush.Color := FTheme.Colors[tcBack]
        else
          Canvas.Brush.Color := clBtnFace;
        Canvas.FillRect(R);
        
        if FTheme <> nil then
          Canvas.Font.Color := FTheme.Colors[tcFore]
        else
          Canvas.Font.Color := clBtnText;
        Canvas.TextOut(R.Left + TabPaddingX, R.Top + TabPaddingY, FTabs[I]);

       if (I < FCloseButtons.Count) and FCloseButtons[I] then begin
          var R2 := GetCloseButtonRect(R);
          if FMenuThemeData <> 0 then begin
            var Offset := MulDiv(1, CurrentPPI, 96);
            Inc(R2.Left, Offset);
            Inc(R2.Top, Offset);
            DrawThemeBackground(FMenuThemeData, Canvas.Handle,  MENU_SYSTEMCLOSE, MSYSC_NORMAL, R2, nil);
          end else begin
            InflateRect(R2, -MulDiv(3, CurrentPPI, 96), -MulDiv(6, CurrentPPI, 96));
            Canvas.Pen.Color := Canvas.Font.Color;
            Canvas.MoveTo(R2.Left, R2.Top);
            Canvas.LineTo(R2.Right, R2.Bottom);
            Canvas.MoveTo(R2.Left, R2.Bottom-1);
            Canvas.LineTo(R2.Right, R2.Top-1);
          end;
        end;
        ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
        Break;
      end;
      if not SelectedTab and (FTabIndex <> I) then begin
        if FTheme <> nil then
          Canvas.Font.Color := FTheme.Colors[tcMarginFore]
        else if HighColorMode and (ColorToRGB(clBtnFace) <> clBlack) then
          Canvas.Font.Color := LightenColor(ColorToRGB(clBtnShadow), -43)
        else begin
          { If the button face color is black, or if running in low color mode,
            use plain clBtnHighlight as the text color }
          Canvas.Font.Color := clBtnHighlight;
        end;
        Canvas.TextOut(R.Left + TabPaddingX, R.Top + TabPaddingY, FTabs[I]);
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
    Canvas.Pen.Color := FTheme.Colors[tcBack]
  else
    Canvas.Pen.Color := clBtnFace;
  if FTabPosition = tpBottom then begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(CR.Right, 0);
  end else begin
    Canvas.MoveTo(0, CR.Bottom-1);
    Canvas.LineTo(CR.Right, CR.Bottom-1);
  end;

  { Background fill }
  if FTheme <> nil then
    Canvas.Brush.Color := FTheme.Colors[tcMarginBack]
  else if HighColorMode then
    Canvas.Brush.Color := LightenColor(ColorToRGB(clBtnFace), 35)
  else
    Canvas.Brush.Color := clBtnShadow;
  if FTabPosition = tpBottom then
    Inc(CR.Top)
  else
    Dec(CR.Bottom);
  Canvas.FillRect(CR);

  { Non-selected tabs }
  DrawTabs(False);
end;

procedure TNewTabSet.SetCloseButtons(Value: TBoolList);
begin
  FCloseButtons.Clear;
  for var V in Value do
    FCloseButtons.Add(V);
  Invalidate;
end;

procedure TNewTabSet.SetHints(const Value: TStrings);
begin
  FHints.Assign(Value);
  ShowHint := FHints.Count > 0;
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

procedure TNewTabSet.UpdateThemeData(const Open: Boolean);
begin
  if FMenuThemeData <> 0 then begin
    CloseThemeData(FMenuThemeData);
    FMenuThemeData := 0;
  end;

  if Open then begin
    if UseThemes then begin
      if (FTheme <> nil) and FTheme.Dark then
        FMenuThemeData := OpenThemeData(Handle, 'DarkMode::Menu')
      else
        FMenuThemeData := OpenThemeData(Handle, 'Menu');
    end else
      FMenuThemeData := 0;
  end;
end;

procedure TNewTabSet.EnsureCurrentTabIsFullyVisible;
var
  rcTab, rcCtl, rcLast: TRect;
  iExtra, iDelta, iNewOffset: Integer;
begin
  rcCtl := ClientRect;
  rcTab := GetTabRect(FTabIndex);

  { Check and modify tabs offset so everything fits }
  iExtra := Min(rcCtl.Width div 2, rcTab.Width * 4);  { arbitrary value, adjust as needed }
  iDelta := rcTab.Width div 2;                        { arbitrary value, adjust as needed }

  { Left side is easy, limit is always 0 }
  if rcTab.Left < rcCtl.Left + iDelta then begin
    FTabsOffset := Max(0, FTabsOffset - rcCtl.Left - rcTab.Left - iExtra);
    Invalidate;
  end;

  { Right side limit depends on last tab and total available space }
  if rcTab.Right > rcCtl.Right - iDelta then begin
    iNewOffset := FTabsOffset + (rcTab.Right - rcCtl.Right) + iExtra;
    FTabsOffset := 0; { We need the last tabs leftmost position w/o any offset }
    rcLast := GetTabRect(FTabs.Count-1);
    FTabsOffset := Max(0, Min(iNewOffset, rcLast.Right - rcCtl.Width + 10));
    Invalidate;
  end;
end;


end.
