{ Compiletime Graphics support }
unit uPSC_graphics;

{$I PascalScript.inc}
interface
uses
  uPSCompiler, uPSUtils;



procedure SIRegister_Graphics_TypesAndConsts(Cl: TPSPascalCompiler);
procedure SIRegisterTGRAPHICSOBJECT(Cl: TPSPascalCompiler);
procedure SIRegisterTFont(Cl: TPSPascalCompiler);
procedure SIRegisterTPEN(Cl: TPSPascalCompiler);
procedure SIRegisterTBRUSH(Cl: TPSPascalCompiler);
procedure SIRegisterTCanvas(cl: TPSPascalCompiler);
procedure SIRegisterTGraphic(CL: TPSPascalCompiler);
procedure SIRegisterTBitmap(CL: TPSPascalCompiler; Streams: Boolean);
procedure SIRegisterTPicture(CL: TPSPascalCompiler);

procedure SIRegister_Graphics(Cl: TPSPascalCompiler; Streams: Boolean);

implementation
{$IFNDEF PS_NOGRAPHCONST}
uses
  {$IFDEF CLX}QGraphics{$ELSE}Graphics{$ENDIF};
{$ELSE}
{$IFNDEF CLX}
{$IFNDEF FPC}
uses
  Windows;
{$ENDIF}
{$ENDIF}
{$ENDIF}

procedure SIRegisterTGRAPHICSOBJECT(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TGraphicsObject') do
  begin
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
  end;
end;

procedure SIRegisterTFont(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TGraphicsObject'), 'TFont') do
  begin
    RegisterMethod('constructor Create;');
{$IFNDEF CLX}
    RegisterProperty('Handle', 'Integer', iptRW);
{$ENDIF}
    RegisterProperty('Color', 'TColor', iptRW);
    RegisterProperty('Height', 'Integer', iptRW);
    RegisterProperty('Name', 'string', iptRW);
    RegisterProperty('Pitch', 'Byte', iptRW);
    RegisterProperty('Size', 'Integer', iptRW);
    RegisterProperty('PixelsPerInch', 'Integer', iptRW);
    RegisterProperty('Style', 'TFontStyles', iptrw);
  end;
end;

procedure SIRegisterTCanvas(cl: TPSPascalCompiler); // requires TPersistent
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TCanvas') do
  begin
    RegisterMethod('procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);');
    RegisterMethod('procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);');
    RegisterMethod('procedure Draw(X, Y: Integer; Graphic: TGraphic);');
    RegisterMethod('procedure Ellipse(X1, Y1, X2, Y2: Integer);');
    RegisterMethod('procedure FillRect(const Rect: TRect);');
{$IFNDEF CLX}
    RegisterMethod('procedure FloodFill(X, Y: Integer; Color: TColor; FillStyle: Byte);');
{$ENDIF}
    RegisterMethod('procedure LineTo(X, Y: Integer);');
    RegisterMethod('procedure MoveTo(X, Y: Integer);');
    RegisterMethod('procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);');
    RegisterMethod('procedure Rectangle(X1, Y1, X2, Y2: Integer);');
    RegisterMethod('procedure Refresh;');
    RegisterMethod('procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);');
    RegisterMethod('function TextHeight(Text: string): Integer;');
    RegisterMethod('procedure TextOut(X, Y: Integer; Text: string);');
    RegisterMethod('function TextWidth(Text: string): Integer;');
{$IFNDEF CLX}
    RegisterProperty('Handle', 'Integer', iptRw);
{$ENDIF}
    RegisterProperty('Pixels', 'Integer Integer Integer', iptRW);
    RegisterProperty('Brush', 'TBrush', iptR);
    RegisterProperty('CopyMode', 'Byte', iptRw);
    RegisterProperty('Font', 'TFont', iptR);
    RegisterProperty('Pen', 'TPen', iptR);
  end;
end;

procedure SIRegisterTPEN(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TGraphicsObject'), 'TPen') do
  begin
    RegisterMethod('constructor Create');
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Mode', 'TPenMode', iptrw);
    RegisterProperty('Style', 'TPenStyle', iptrw);
    RegisterProperty('Width', 'Integer', iptrw);
  end;
end;

procedure SIRegisterTBRUSH(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TGraphicsObject'), 'TBrush') do
  begin
    RegisterMethod('constructor Create');
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Style', 'TBrushStyle', iptrw);
  end;
end;

procedure SIRegister_Graphics_TypesAndConsts(Cl: TPSPascalCompiler);
{$IFDEF PS_NOGRAPHCONST}
const
  clSystemColor = {$IFDEF DELPHI7UP} $FF000000 {$ELSE} $80000000 {$ENDIF};
{$ENDIF}
begin
{$IFNDEF PS_NOGRAPHCONST}
  cl.AddConstantN('clScrollBar', 'Integer').Value.ts32 := clScrollBar;
  cl.AddConstantN('clBackground', 'Integer').Value.ts32 := clBackground;
  cl.AddConstantN('clActiveCaption', 'Integer').Value.ts32 := clActiveCaption;
  cl.AddConstantN('clInactiveCaption', 'Integer').Value.ts32 := clInactiveCaption;
  cl.AddConstantN('clMenu', 'Integer').Value.ts32 := clMenu;
  cl.AddConstantN('clWindow', 'Integer').Value.ts32 := clWindow;
  cl.AddConstantN('clWindowFrame', 'Integer').Value.ts32 := clWindowFrame;
  cl.AddConstantN('clMenuText', 'Integer').Value.ts32 := clMenuText;
  cl.AddConstantN('clWindowText', 'Integer').Value.ts32 := clWindowText;
  cl.AddConstantN('clCaptionText', 'Integer').Value.ts32 := clCaptionText;
  cl.AddConstantN('clActiveBorder', 'Integer').Value.ts32 := clActiveBorder;
  cl.AddConstantN('clInactiveBorder', 'Integer').Value.ts32 := clInactiveCaption;
  cl.AddConstantN('clAppWorkSpace', 'Integer').Value.ts32 := clAppWorkSpace;
  cl.AddConstantN('clHighlight', 'Integer').Value.ts32 := clHighlight;
  cl.AddConstantN('clHighlightText', 'Integer').Value.ts32 := clHighlightText;
  cl.AddConstantN('clBtnFace', 'Integer').Value.ts32 := clBtnFace;
  cl.AddConstantN('clBtnShadow', 'Integer').Value.ts32 := clBtnShadow;
  cl.AddConstantN('clGrayText', 'Integer').Value.ts32 := clGrayText;
  cl.AddConstantN('clBtnText', 'Integer').Value.ts32 := clBtnText;
  cl.AddConstantN('clInactiveCaptionText', 'Integer').Value.ts32 := clInactiveCaptionText;
  cl.AddConstantN('clBtnHighlight', 'Integer').Value.ts32 := clBtnHighlight;
  cl.AddConstantN('cl3DDkShadow', 'Integer').Value.ts32 := cl3DDkShadow;
  cl.AddConstantN('cl3DLight', 'Integer').Value.ts32 := cl3DLight;
  cl.AddConstantN('clInfoText', 'Integer').Value.ts32 := clInfoText;
  cl.AddConstantN('clInfoBk', 'Integer').Value.ts32 := clInfoBk;
{$ELSE}
{$IFNDEF CLX}  // These are VCL-only; CLX uses different constant values
  cl.AddConstantN('clScrollBar', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_SCROLLBAR);
  cl.AddConstantN('clBackground', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_BACKGROUND);
  cl.AddConstantN('clActiveCaption', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_ACTIVECAPTION);
  cl.AddConstantN('clInactiveCaption', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_INACTIVECAPTION);
  cl.AddConstantN('clMenu', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_MENU);
  cl.AddConstantN('clWindow', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_WINDOW);
  cl.AddConstantN('clWindowFrame', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_WINDOWFRAME);
  cl.AddConstantN('clMenuText', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_MENUTEXT);
  cl.AddConstantN('clWindowText', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_WINDOWTEXT);
  cl.AddConstantN('clCaptionText', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_CAPTIONTEXT);
  cl.AddConstantN('clActiveBorder', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_ACTIVEBORDER);
  cl.AddConstantN('clInactiveBorder', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_INACTIVEBORDER);
  cl.AddConstantN('clAppWorkSpace', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_APPWORKSPACE);
  cl.AddConstantN('clHighlight', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_HIGHLIGHT);
  cl.AddConstantN('clHighlightText', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_HIGHLIGHTTEXT);
  cl.AddConstantN('clBtnFace', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_BTNFACE);
  cl.AddConstantN('clBtnShadow', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_BTNSHADOW);
  cl.AddConstantN('clGrayText', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_GRAYTEXT);
  cl.AddConstantN('clBtnText', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_BTNTEXT);
  cl.AddConstantN('clInactiveCaptionText', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_INACTIVECAPTIONTEXT);
  cl.AddConstantN('clBtnHighlight', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_BTNHIGHLIGHT);
  cl.AddConstantN('cl3DDkShadow', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_3DDKSHADOW);
  cl.AddConstantN('cl3DLight', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_3DLIGHT);
  cl.AddConstantN('clInfoText', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_INFOTEXT);
  cl.AddConstantN('clInfoBk', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_INFOBK);
{$ENDIF}
{$ENDIF}
  cl.AddConstantN('clBlack', 'Integer').Value.ts32 := $000000;
  cl.AddConstantN('clMaroon', 'Integer').Value.ts32 := $000080;
  cl.AddConstantN('clGreen', 'Integer').Value.ts32 := $008000;
  cl.AddConstantN('clOlive', 'Integer').Value.ts32 := $008080;
  cl.AddConstantN('clNavy', 'Integer').Value.ts32 := $800000;
  cl.AddConstantN('clPurple', 'Integer').Value.ts32 := $800080;
  cl.AddConstantN('clTeal', 'Integer').Value.ts32 := $808000;
  cl.AddConstantN('clGray', 'Integer').Value.ts32 := $808080;
  cl.AddConstantN('clSilver', 'Integer').Value.ts32 := $C0C0C0;
  cl.AddConstantN('clRed', 'Integer').Value.ts32 := $0000FF;
  cl.AddConstantN('clLime', 'Integer').Value.ts32 := $00FF00;
  cl.AddConstantN('clYellow', 'Integer').Value.ts32 := $00FFFF;
  cl.AddConstantN('clBlue', 'Integer').Value.ts32 := $FF0000;
  cl.AddConstantN('clFuchsia', 'Integer').Value.ts32 := $FF00FF;
  cl.AddConstantN('clAqua', 'Integer').Value.ts32 := $FFFF00;
  cl.AddConstantN('clLtGray', 'Integer').Value.ts32 := $C0C0C0;
  cl.AddConstantN('clDkGray', 'Integer').Value.ts32 := $808080;
  cl.AddConstantN('clWhite', 'Integer').Value.ts32 := $FFFFFF;
  cl.AddConstantN('clNone', 'Integer').Value.ts32 := $1FFFFFFF;
  cl.AddConstantN('clDefault', 'Integer').Value.ts32 := $20000000;

  Cl.addTypeS('TFontStyle', '(fsBold, fsItalic, fsUnderline, fsStrikeOut)');
  Cl.addTypeS('TFontStyles', 'set of TFontStyle');

  cl.AddTypeS('TFontPitch', '(fpDefault, fpVariable, fpFixed)');
  cl.AddTypeS('TPenStyle', '(psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear, psInsideFrame)');
  cl.AddTypeS('TPenMode', '(pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy, pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge, pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor)');
  cl.AddTypeS('TBrushStyle', '(bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross)');
  cl.addTypeS('TColor', 'Integer');

{$IFNDEF CLX}
  cl.addTypeS('HBITMAP', 'Integer');
  cl.addTypeS('HPALETTE', 'Integer');
{$ENDIF}
end;

procedure SIRegisterTGraphic(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TPersistent'),'TGraphic') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('procedure LoadFromFile(const FileName: string)');
    RegisterMethod('procedure SaveToFile(const FileName: string)');
    RegisterProperty('Empty', 'Boolean', iptr);
    RegisterProperty('Height', 'Integer', iptrw);
    RegisterProperty('Modified', 'Boolean', iptrw);
    RegisterProperty('Width', 'Integer', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
  end;
end;

procedure SIRegisterTBitmap(CL: TPSPascalCompiler; Streams: Boolean);
begin
  with CL.AddClassN(CL.FindClass('TGraphic'),'TBitmap') do
  begin
    if Streams then begin
      RegisterMethod('procedure LoadFromStream(Stream: TStream)');
      RegisterMethod('procedure SaveToStream(Stream: TStream)');
    end;
    RegisterProperty('Canvas', 'TCanvas', iptr);
{$IFNDEF CLX}
    RegisterProperty('Handle', 'HBITMAP', iptrw);
{$ENDIF}

    {$IFNDEF IFPS_MINIVCL}
    RegisterMethod('procedure Dormant');
    RegisterMethod('procedure FreeImage');
{$IFNDEF CLX}
    RegisterMethod('procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE)');
{$ENDIF}
    RegisterMethod('procedure LoadFromResourceName(Instance: THandle; const ResName: string)');
    RegisterMethod('procedure LoadFromResourceID(Instance: THandle; ResID: Integer)');
{$IFNDEF CLX}
    RegisterMethod('function ReleaseHandle: HBITMAP');
    RegisterMethod('function ReleasePalette: HPALETTE');
    RegisterMethod('procedure SaveToClipboardFormat(var Format: Word; var Data: THandle; var APalette: HPALETTE)');
    RegisterProperty('Monochrome', 'Boolean', iptrw);
    RegisterProperty('Palette', 'HPALETTE', iptrw);
    RegisterProperty('IgnorePalette', 'Boolean', iptrw);
{$ENDIF}
    RegisterProperty('TransparentColor', 'TColor', iptr);
    {$ENDIF}
  end;
end;

procedure SIRegisterTPicture(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TPersistent'),'TPicture') do
  begin
    RegisterProperty('Bitmap','TBitmap',iptrw);
  end;
end;

procedure SIRegister_Graphics(Cl: TPSPascalCompiler; Streams: Boolean);
begin
  SIRegister_Graphics_TypesAndConsts(Cl);
  SIRegisterTGRAPHICSOBJECT(Cl);
  SIRegisterTGraphic(Cl);
  SIRegisterTFont(Cl);
  SIRegisterTPEN(cl);
  SIRegisterTBRUSH(cl);
  SIRegisterTCanvas(cl);
  SIRegisterTBitmap(Cl, Streams);
  SIRegisterTPicture(cl);
end;

// PS_MINIVCL changes by Martijn Laan

End.
