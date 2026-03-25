
unit uPSR_graphics;
{$I PascalScript.inc}
interface
uses
  uPSRuntime, uPSUtils;



procedure RIRegisterTGRAPHICSOBJECT(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFont(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTPEN(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTBRUSH(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCanvas(cl: TPSRuntimeClassImporter);
procedure RIRegisterTGraphic(CL: TPSRuntimeClassImporter);
procedure RIRegisterTBitmap(CL: TPSRuntimeClassImporter; Streams: Boolean);
procedure RIRegisterTPicture(CL: TPSRuntimeClassImporter);

procedure RIRegister_Graphics(Cl: TPSRuntimeClassImporter; Streams: Boolean);

implementation
{$IFNDEF FPC}
uses
  Classes{$IFDEF CLX}, QGraphics{$ELSE}, Windows, Graphics{$ENDIF} {$IFDEF DELPHI_TOKYO_UP}, UITypes {$ENDIF};
{$ELSE}
uses
  Classes, Graphics,LCLType;
{$ENDIF}

{$IFNDEF CLX}
procedure TFontHandleR(Self: TFont; var T: HFONT); begin T := Self.Handle; end;
procedure TFontHandleW(Self: TFont; T: HFONT); begin Self.Handle := T; end;
{$ENDIF}
procedure TFontPixelsPerInchR(Self: TFont; var T: Longint); begin T := Self.PixelsPerInch; end;
procedure TFontPixelsPerInchW(Self: TFont; T: Longint); begin {$IFNDEF FPC} Self.PixelsPerInch := T;{$ENDIF} end;
procedure TFontStyleR(Self: TFont; var T: TFontStyles); begin T := Self.Style; end;
procedure TFontStyleW(Self: TFont; T: TFontStyles); begin Self.Style:= T; end;

procedure RIRegisterTFont(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TFont) do
  begin
    RegisterConstructor(@TFont.Create, 'Create');
{$IFNDEF CLX}
    RegisterPropertyHelper(@TFontHandleR, @TFontHandleW, 'Handle');
{$ENDIF}
    RegisterPropertyHelper(@TFontPixelsPerInchR, @TFontPixelsPerInchW, 'PixelsPerInch');
    RegisterPropertyHelper(@TFontStyleR, @TFontStyleW, 'Style');
  end;
end;
{$IFNDEF CLX}
procedure TCanvasHandleR(Self: TCanvas; var T: HDC); begin T := Self.Handle; end;
procedure TCanvasHandleW(Self: TCanvas; T: HDC); begin Self.Handle:= T; end;
{$ENDIF}

procedure TCanvasPixelsR(Self: TCanvas; var T: Longint; X,Y: Longint); begin T := Self.Pixels[X,Y]; end;
procedure TCanvasPixelsW(Self: TCanvas; T, X, Y: Longint); begin Self.Pixels[X,Y]:= T; end;
{$IFDEF FPC}
procedure TCanvasArc(Self : TCanvas; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); begin Self.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4); end;
procedure TCanvasChord(Self : TCanvas; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); begin self.Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4); end;
procedure TCanvasRectangle(Self : TCanvas; X1,Y1,X2,Y2 : integer); begin self.Rectangle(x1,y1,x2,y2); end;
procedure TCanvasRoundRect(Self : TCanvas; X1, Y1, X2, Y2, X3, Y3 : integer); begin self.RoundRect(X1, Y1, X2, Y2, X3, Y3); end;
procedure TCanvasEllipse(Self : TCanvas;X1, Y1, X2, Y2: Integer); begin self.Ellipse(X1, Y1, X2, Y2); end;
procedure TCanvasFillRect(Self : TCanvas; const Rect: TRect); begin self.FillRect(rect); end;
procedure TCanvasFloodFill(Self : TCanvas; X, Y: Integer; Color: TColor; FillStyle: TFillStyle); begin self.FloodFill(x,y,color,fillstyle); end;
{$ENDIF}

procedure RIRegisterTCanvas(cl: TPSRuntimeClassImporter); // requires TPersistent
begin
  with Cl.Add(TCanvas) do
  begin
{$IFDEF FPC}
    RegisterMethod(@TCanvasArc, 'Arc');
    RegisterMethod(@TCanvasChord, 'Chord');
    RegisterMethod(@TCanvasRectangle, 'Rectangle');
    RegisterMethod(@TCanvasRoundRect, 'RoundRect');
    RegisterMethod(@TCanvasEllipse, 'Ellipse');
    RegisterMethod(@TCanvasFillRect, 'FillRect');
    RegisterMethod(@TCanvasFloodFill, 'FloodFill');
{$ELSE}  
    RegisterMethod(@TCanvas{$IFNDEF FPC}.{$ENDIF}Arc, 'Arc');
    RegisterMethod(@TCanvas{$IFNDEF FPC}.{$ENDIF}Chord, 'Chord');
    RegisterMethod(@TCanvas{$IFNDEF FPC}.{$ENDIF}Rectangle, 'Rectangle');
    RegisterMethod(@TCanvas{$IFNDEF FPC}.{$ENDIF}RoundRect, 'RoundRect');
    RegisterMethod(@TCanvas{$IFNDEF FPC}.{$ENDIF}Ellipse, 'Ellipse');
    RegisterMethod(@TCanvas{$IFNDEF FPC}.{$ENDIF}FillRect, 'FillRect');
{$IFNDEF CLX}
    RegisterMethod(@TCanvas{$IFNDEF FPC}.{$ENDIF}FloodFill, 'FloodFill');
{$ENDIF}
{$ENDIF}
    RegisterMethod(@TCanvas.Draw, 'Draw');

    RegisterMethod(@TCanvas.Lineto, 'LineTo');
    RegisterMethod(@TCanvas.Moveto, 'MoveTo');
    RegisterMethod(@TCanvas.Pie, 'Pie');
    RegisterMethod(@TCanvas.Refresh, 'Refresh');
    RegisterMethod(@TCanvas.TextHeight, 'TextHeight');
    RegisterMethod(@TCanvas.TextOut, 'TextOut');
    RegisterMethod(@TCanvas.TextWidth, 'TextWidth');
{$IFNDEF CLX}
    RegisterPropertyHelper(@TCanvasHandleR, @TCanvasHandleW, 'Handle');
{$ENDIF}
    RegisterPropertyHelper(@TCanvasPixelsR, @TCanvasPixelsW, 'Pixels');
  end;
end;


procedure TGRAPHICSOBJECTONCHANGE_W(Self: TGraphicsObject; T: TNotifyEvent); begin Self.OnChange := t; end;
procedure TGRAPHICSOBJECTONCHANGE_R(Self: TGraphicsObject; var T: TNotifyEvent); begin T :=Self.OnChange; end;


procedure RIRegisterTGRAPHICSOBJECT(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TGRAPHICSOBJECT) do
  begin
    RegisterPropertyHelper(@TGRAPHICSOBJECTONCHANGE_R, @TGRAPHICSOBJECTONCHANGE_W, 'OnChange');
  end;
end;

procedure RIRegisterTPEN(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TPEN) do
  begin
    RegisterConstructor(@TPEN.CREATE, 'Create');
  end;
end;

procedure RIRegisterTBRUSH(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TBRUSH) do
  begin
    RegisterConstructor(@TBRUSH.CREATE, 'Create');
  end;
end;

procedure TGraphicOnChange_W(Self: TGraphic; const T: TNotifyEvent); begin Self.OnChange := T; end;
procedure TGraphicOnChange_R(Self: TGraphic; var T: TNotifyEvent); begin T := Self.OnChange; end;
procedure TGraphicWidth_W(Self: TGraphic; const T: Integer); begin Self.Width := T; end;
procedure TGraphicWidth_R(Self: TGraphic; var T: Integer); begin T := Self.Width; end;
procedure TGraphicModified_W(Self: TGraphic; const T: Boolean); begin Self.Modified := T; end;
procedure TGraphicModified_R(Self: TGraphic; var T: Boolean); begin T := Self.Modified; end;
procedure TGraphicHeight_W(Self: TGraphic; const T: Integer); begin Self.Height := T; end;
procedure TGraphicHeight_R(Self: TGraphic; var T: Integer); begin T := Self.Height; end;
procedure TGraphicEmpty_R(Self: TGraphic; var T: Boolean); begin T := Self.Empty; end;

procedure RIRegisterTGraphic(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TGraphic) do
  begin
    RegisterVirtualConstructor(@TGraphic.Create, 'Create');
    RegisterVirtualMethod(@TGraphic.LoadFromFile, 'LoadFromFile');
    RegisterVirtualMethod(@TGraphic.SaveToFile, 'SaveToFile');
    RegisterPropertyHelper(@TGraphicEmpty_R,nil,'Empty');
    RegisterPropertyHelper(@TGraphicHeight_R,@TGraphicHeight_W,'Height');
    RegisterPropertyHelper(@TGraphicWidth_R,@TGraphicWidth_W,'Width');
    RegisterPropertyHelper(@TGraphicOnChange_R,@TGraphicOnChange_W,'OnChange');

    {$IFNDEF PS_MINIVCL}
    RegisterPropertyHelper(@TGraphicModified_R,@TGraphicModified_W,'Modified');
    {$ENDIF}
  end;
end;

procedure TBitmapTransparentColor_R(Self: TBitmap; var T: TColor); begin T := Self.TransparentColor; end;
{$IFNDEF CLX}
{$IFNDEF FPC}
procedure TBitmapIgnorePalette_W(Self: TBitmap; const T: Boolean); begin Self.IgnorePalette := T; end;
procedure TBitmapIgnorePalette_R(Self: TBitmap; var T: Boolean); begin T := Self.IgnorePalette; end;
{$ENDIF}
procedure TBitmapPalette_W(Self: TBitmap; const T: HPALETTE); begin Self.Palette := T; end;
procedure TBitmapPalette_R(Self: TBitmap; var T: HPALETTE); begin T := Self.Palette; end;
{$ENDIF}
procedure TBitmapMonochrome_W(Self: TBitmap; const T: Boolean); begin Self.Monochrome := T; end;
procedure TBitmapMonochrome_R(Self: TBitmap; var T: Boolean); begin T := Self.Monochrome; end;
{$IFNDEF CLX}
procedure TBitmapHandle_W(Self: TBitmap; const T: HBITMAP); begin Self.Handle := T; end;
procedure TBitmapHandle_R(Self: TBitmap; var T: HBITMAP); begin T := Self.Handle; end;
{$ENDIF}
procedure TBitmapCanvas_R(Self: TBitmap; var T: TCanvas); begin T := Self.Canvas; end;

procedure RIRegisterTBitmap(CL: TPSRuntimeClassImporter; Streams: Boolean);
begin
  with CL.Add(TBitmap) do
  begin
    if Streams then begin
      RegisterMethod(@TBitmap.LoadFromStream, 'LoadFromStream');
      RegisterMethod(@TBitmap.SaveToStream, 'SaveToStream');
    end;
    RegisterPropertyHelper(@TBitmapCanvas_R,nil,'Canvas');
{$IFNDEF CLX}
    RegisterPropertyHelper(@TBitmapHandle_R,@TBitmapHandle_W,'Handle');
{$ENDIF}

    {$IFNDEF PS_MINIVCL}
{$IFNDEF FPC}
    RegisterMethod(@TBitmap.Dormant, 'Dormant');
{$ENDIF}
    RegisterMethod(@TBitmap.FreeImage, 'FreeImage');
{$IFNDEF CLX}
    RegisterMethod(@TBitmap.LoadFromClipboardFormat, 'LoadFromClipboardFormat');
{$ENDIF}
    RegisterMethod(@TBitmap.LoadFromResourceName, 'LoadFromResourceName');
    RegisterMethod(@TBitmap.LoadFromResourceID, 'LoadFromResourceID');
{$IFNDEF CLX}
    RegisterMethod(@TBitmap.ReleaseHandle, 'ReleaseHandle');
    RegisterMethod(@TBitmap.ReleasePalette, 'ReleasePalette');
    RegisterMethod(@TBitmap.SaveToClipboardFormat, 'SaveToClipboardFormat');
    RegisterPropertyHelper(@TBitmapMonochrome_R,@TBitmapMonochrome_W,'Monochrome');
    RegisterPropertyHelper(@TBitmapPalette_R,@TBitmapPalette_W,'Palette');
{$IFNDEF FPC}
    RegisterPropertyHelper(@TBitmapIgnorePalette_R,@TBitmapIgnorePalette_W,'IgnorePalette');
{$ENDIF}
{$ENDIF}
    RegisterPropertyHelper(@TBitmapTransparentColor_R,nil,'TransparentColor');
    {$ENDIF}
  end;
end;

procedure TPictureBitmap_W(Self: TPicture; const T: TBitmap); begin Self.Bitmap := T; end;
procedure TPictureBitmap_R(Self: TPicture; var T: TBitmap); begin T := Self.Bitmap; end;
procedure RIRegisterTPicture(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TPicture) do
    registerPropertyHelper(@TPictureBitmap_R,@TPictureBitmap_W,'Bitmap');
end;

procedure RIRegister_Graphics(Cl: TPSRuntimeClassImporter; Streams: Boolean);
begin
  RIRegisterTGRAPHICSOBJECT(cl);
  RIRegisterTFont(Cl);
  RIRegisterTCanvas(cl);
  RIRegisterTPEN(cl);
  RIRegisterTBRUSH(cl);
  RIRegisterTGraphic(CL);
  RIRegisterTBitmap(CL, Streams);
  RIRegisterTPicture(CL);
end;

// PS_MINIVCL changes by Martijn Laan

end.





