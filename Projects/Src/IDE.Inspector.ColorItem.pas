unit IDE.Inspector.ColorItem;

interface

uses
  Windows, Controls, Graphics, Classes, SysUtils, Types, Dialogs, Math,
  JvInspector;

type
  TJvInspectorColorItem = class(TJvInspectorStringItem)
  private
    FBtnColorRect: TRect;
    function StringToColorDef(const S: string; Default: TColor): TColor;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    procedure DrawValue(const ACanvas: TCanvas); override;
  end;

implementation

uses
  IDE.HelperFunc;

function TJvInspectorColorItem.StringToColorDef(const S: string; Default: TColor): TColor;
var
  R, G, B: Integer;

  function ParseHexPair(Position: Integer): Integer;
  begin
    Result := StrToIntDef('$' + Copy(S, Position, 2), -1);
  end;
{ A value in the form of #rrggbb or $bbggrr, where bb, gg, and rr specify the
  two-digit intensities (in hexadecimal) for blue, green, and red respectively. }
begin
  if Length(S) >= 7 then begin
    if S[1] = '#' then begin
      R := ParseHexPair(2);
      G := ParseHexPair(4);
      B := ParseHexPair(6);
      if (R >= 0) and (G >= 0) and (B >= 0) then
        Exit(TColor(RGB(Byte(R), Byte(G), Byte(B))));
    end
    else if S[1] = '$' then begin
      B := ParseHexPair(2);
      G := ParseHexPair(4);
      R := ParseHexPair(6);
      if (R >= 0) and (G >= 0) and (B >= 0) then
        Exit(TColor(RGB(Byte(R), Byte(G), Byte(B))));
    end;
  end;
  try
    Result := StringToColor(S);
  except
    Result := Default;
  end;
end;

procedure TJvInspectorColorItem.DrawValue(const ACanvas: TCanvas);
var
  BtnColorSize: Integer;
  ColorValue: TColor;
begin
  inherited DrawValue(ACanvas);
  FBtnColorRect := Rects[iprValueButton];
  if not IsRectEmpty(FBtnColorRect) then begin
    BtnColorSize := Min(FBtnColorRect.Width, FBtnColorRect.Height) - 6;
    if BtnColorSize > 0 then begin
      FBtnColorRect.Left := FBtnColorRect.Left + (FBtnColorRect.Width - BtnColorSize) div 2;
      FBtnColorRect.Top := FBtnColorRect.Top + (FBtnColorRect.Height - BtnColorSize) div 2;
      FBtnColorRect.Right := FBtnColorRect.Left + BtnColorSize;
      FBtnColorRect.Bottom := FBtnColorRect.Top + BtnColorSize;
      ColorValue := StringToColorDef(DisplayValue, clGray);
      ACanvas.Brush.Color := ColorValue;
      ACanvas.Pen.Color := clBlack;
      if InitFormThemeIsDark then
        ACanvas.Pen.Color := $009A9A9A;
      ACanvas.Rectangle(FBtnColorRect);
    end;
  end;
end;

procedure TJvInspectorColorItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
  dlg: TColorDialog;
  RGBColor: TColorRef;
  ColorName: String;
begin
  Pt := Point(X, Y);
  if (Button = mbLeft) and PtInRect(Rects[iprValueButton], Pt) then begin
    dlg := TColorDialog.Create(nil);
    try
      dlg.Color := StringToColorDef(DisplayValue, clBlack);
      if dlg.Execute then begin
        ColorName := ColorToString(dlg.Color);
        if (Length(ColorName) > 0) and (ColorName[1] = '$') then begin
          RGBColor := TColorRef(ColorToRGB(dlg.Color));
          ColorName := (Format('#%.2x%.2x%.2x',
            [Byte(GetRValue(RGBColor)), Byte(GetGValue(RGBColor)), Byte(GetBValue(RGBColor))]));
        end;
        DisplayValue := ColorName;
        InvalidateItem;
      end;
    finally
      dlg.Free;
    end;
    Exit;
  end;
  inherited;
end;

end.