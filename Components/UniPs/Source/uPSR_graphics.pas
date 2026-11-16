
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
uses
  Classes, Windows, Graphics, UITypes;

procedure TFontHandleR(Self: TFont; var T: HFONT); begin T := Self.Handle; end;
procedure TFontHandleW(Self: TFont; T: HFONT); begin Self.Handle := T; end;
procedure TFontPixelsPerInchR(Self: TFont; var T: Longint); begin T := Self.PixelsPerInch; end;
procedure TFontPixelsPerInchW(Self: TFont; T: Longint); begin Self.PixelsPerInch := T; end;
procedure TFontStyleR(Self: TFont; var T: TFontStyles); begin T := Self.Style; end;
procedure TFontStyleW(Self: TFont; T: TFontStyles); begin Self.Style:= T; end;

procedure RIRegisterTFont(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TFont) do
  begin
    RegisterConstructor(@TFont.Create, 'Create');
    RegisterPropertyHelper(@TFontHandleR, @TFontHandleW, 'Handle');
    RegisterPropertyHelper(@TFontPixelsPerInchR, @TFontPixelsPerInchW, 'PixelsPerInch');
    RegisterPropertyHelper(@TFontStyleR, @TFontStyleW, 'Style');
  end;
end;
procedure TCanvasHandleR(Self: TCanvas; var T: HDC); begin T := Self.Handle; end;
procedure TCanvasHandleW(Self: TCanvas; T: HDC); begin Self.Handle:= T; end;

procedure TCanvasPixelsR(Self: TCanvas; var T: Longint; X,Y: Longint); begin T := Self.Pixels[X,Y]; end;
procedure TCanvasPixelsW(Self: TCanvas; T, X, Y: Longint); begin Self.Pixels[X,Y]:= T; end;

procedure RIRegisterTCanvas(cl: TPSRuntimeClassImporter); // requires TPersistent
begin
  with Cl.Add(TCanvas) do
  begin
    RegisterMethod(@TCanvas.Arc, 'Arc');
    RegisterMethod(@TCanvas.Chord, 'Chord');
    RegisterMethod(@TCanvas.Rectangle, 'Rectangle');
    RegisterMethod(@TCanvas.RoundRect, 'RoundRect');
    RegisterMethod(@TCanvas.Ellipse, 'Ellipse');
    RegisterMethod(@TCanvas.FillRect, 'FillRect');
    RegisterMethod(@TCanvas.FloodFill, 'FloodFill');
    RegisterMethod(@TCanvas.Draw, 'Draw');

    RegisterMethod(@TCanvas.Lineto, 'LineTo');
    RegisterMethod(@TCanvas.Moveto, 'MoveTo');
    RegisterMethod(@TCanvas.Pie, 'Pie');
    RegisterMethod(@TCanvas.Refresh, 'Refresh');
    RegisterMethod(@TCanvas.TextHeight, 'TextHeight');
    RegisterMethod(@TCanvas.TextOut, 'TextOut');
    RegisterMethod(@TCanvas.TextWidth, 'TextWidth');
    RegisterPropertyHelper(@TCanvasHandleR, @TCanvasHandleW, 'Handle');
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
  end;
end;

procedure TBitmapHandle_W(Self: TBitmap; const T: HBITMAP); begin Self.Handle := T; end;
procedure TBitmapHandle_R(Self: TBitmap; var T: HBITMAP); begin T := Self.Handle; end;
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
    RegisterPropertyHelper(@TBitmapHandle_R,@TBitmapHandle_W,'Handle');
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

end.





