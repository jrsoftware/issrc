unit Resample;
interface
uses
  Windows, Math, Graphics;

function StretchBmp(Canvas: TCanvas; SrcBitmap, DstBitmap: TBitmap;
  DstWidth, DstHeight: Integer; Is32bit: Boolean): Boolean;

implementation

const
  FixedBits = 16;
  FixedOne = 1 shl FixedBits;
  FixedOneHalf = FixedOne shr 1;
type
  TWeight = packed record
    Offset: Integer; //Byte offset to pixel data
    case Integer of
      0: (Weight: Integer); //Pixel weight in Q16.16 fixed point format
      1: (Temp: Single); //same thing in float format
  end;
  TWeightArray = array [0..MaxInt div SizeOf(TWeight) - 1] of TWeight;
  TPutPixelProc = procedure(const Weights: array of TWeight; Bits, Pixel: Pointer);

procedure ResampleBits(DstSize, SrcSize: Integer; SrcLine, DstLine: Pointer;
  PixelSize, LineCount, SrcLineSize, DstLineSize: Integer; PutPixelProc: TPutPixelProc);
var
  I, J, Count: Integer;
  Limit, Scale, X, Y, Center, Sup, Sum: Single;
  Weights: ^TWeightArray;
  Src, Dst: Pointer;
const
  FilterWidth = 2.0;
begin
  Scale := SrcSize / DstSize;
  if Scale < 1.0 then
    Limit := 1.0
  else
    Limit := 1.0 / Scale;
  Sup := FilterWidth / Limit;
  GetMem(Weights, Trunc(Sup * 2.0 + 2.0) * SizeOf(TWeight));
  try
    for I := 0 to DstSize - 1 do begin
      Count := 0;
      Sum := 0;
      Center := (I + 0.5) * Scale;
      for J := Floor(Center - Sup) to Ceil(Center + Sup) do begin
        X := Abs(J - Center + 0.5);
        if X > Sup then Continue;
        X := X * Limit;
        {Resampling filter}
        if X < 1.0 then //SPLINE16
          Y := Sqr(X) * (X - 9 / 5) - 1 / 5 * X + 1
        else
          Y := Sqr(X - 1) * (-1 / 3 * (X - 1) + 4 / 5) - 7 / 15 * (X - 1);
        {The code from above must be kept in sync with FilterWidth value}
        if (Y = 0) or (J < 0) or (J >= SrcSize) then Continue;
        with Weights[Count] do begin
          Temp := Y;
          Offset := J * PixelSize;
        end;
        Sum := Sum + Y;
        Inc(Count);
      end;
      if Sum <> 0 then begin
        Sum := FixedOne / Sum;
        for J := 0 to Count - 1 do
          with Weights[J] do
            Weight := Round(Temp * Sum);
      end else
        Count := 0;
      Src := SrcLine;
      Dst := DstLine;
      for J := 0 to LineCount - 1 do begin
        PutPixelProc(Slice(Weights^, Count), Src, Dst);
        Inc(PByte(Src), SrcLineSize);
        Inc(PByte(Dst), DstLineSize);
      end;
      Inc(PByte(DstLine), PixelSize);
    end;
  finally
    FreeMem(Weights);
  end;
end;

//Process pixel in BGR format
procedure PutPixel24(const Weights: array of TWeight; Bits, Pixel: Pointer);
type
  PRGBTriple = ^TRGBTriple;
var
  I, R, G, B: Integer;
begin
  R := FixedOneHalf;
  G := FixedOneHalf;
  B := FixedOneHalf;
  for I := 0 to High(Weights) do
    with Weights[I], PRGBTriple(PAnsiChar(Bits) + Offset)^ do begin
      Inc(R, rgbtRed * Weight);
      Inc(G, rgbtGreen * Weight);
      Inc(B, rgbtBlue * Weight);
    end;
  with PRGBTriple(Pixel)^ do begin
    //Clamps all channels to values between 0 and 255
    if R > 0 then if R < 255 shl FixedBits then rgbtRed   := R shr FixedBits else rgbtRed   := 255 else rgbtRed   := 0;
    if G > 0 then if G < 255 shl FixedBits then rgbtGreen := G shr FixedBits else rgbtGreen := 255 else rgbtGreen := 0;
    if B > 0 then if B < 255 shl FixedBits then rgbtBlue  := B shr FixedBits else rgbtBlue  := 255 else rgbtBlue  := 0;
  end;
end;

//Process pixel in BGRA premultiplied alpha format
procedure PutPixel32P(const Weights: array of TWeight; Bits, Pixel: Pointer);
var
  I, R, G, B, A: Integer;
  AByte: Byte;
begin
  R := FixedOneHalf;
  G := FixedOneHalf;
  B := FixedOneHalf;
  A := FixedOneHalf;
  for I := 0 to High(Weights) do
    with Weights[I], PRGBQuad(PAnsiChar(Bits) + Offset)^ do begin
      Inc(R, rgbRed * Weight);
      Inc(G, rgbGreen * Weight);
      Inc(B, rgbBlue * Weight);
      Inc(A, rgbReserved * Weight);
    end;
  //Clamps alpha channel to values between 0 and 255
  if A > 0 then if A < 255 shl FixedBits then AByte := A shr FixedBits else AByte := 255 else AByte := 0;
  with PRGBQuad(Pixel)^ do begin
    rgbReserved := AByte;
    I := AByte shl FixedBits;
    //Clamps other channels to values between 0 and Alpha
    if R > 0 then if R < I then rgbRed   := R shr FixedBits else rgbRed   := AByte else rgbRed   := 0;
    if G > 0 then if G < I then rgbGreen := G shr FixedBits else rgbGreen := AByte else rgbGreen := 0;
    if B > 0 then if B < I then rgbBlue  := B shr FixedBits else rgbBlue  := AByte else rgbBlue  := 0;
  end;
end;

function StretchBmp(Canvas: TCanvas; SrcBitmap, DstBitmap: TBitmap;
  DstWidth, DstHeight: Integer; Is32bit: Boolean): Boolean;
var
  SrcLineSize, DstLineSize, SrcWidth, SrcHeight, PixelSize: Integer;
  SrcBits, DstBits, tmpBits: Pointer;
  BI: TBitmapInfo;
  DIB: HBITMAP;
  Proc: TPutPixelProc;
const
  NULL = {$IFDEF VER90}nil{$ELSE}0{$ENDIF};
begin
  Result := False;
  try
    if (DstWidth <= 0) or (DstHeight <= 0) then Exit;
    //High quality resampling makes sense only
    //in True Color and High Color display modes.
    if GetDeviceCaps(Canvas.Handle, BITSPIXEL) <= 8 then Exit;
    SrcWidth  := SrcBitmap.Width;
    SrcHeight := SrcBitmap.Height;
    if (SrcWidth <= 0) or (SrcHeight <= 0) then Exit;
    FillChar(BI, SizeOf(BI), 0);
    BI.bmiHeader.biSize := SizeOf(BI.bmiHeader);
    BI.bmiHeader.biWidth := SrcWidth;
    BI.bmiHeader.biHeight := SrcHeight;
    BI.bmiHeader.biPlanes := 1;
    BI.bmiHeader.biCompression := BI_RGB;
    if Is32bit then begin
      BI.bmiHeader.biBitCount := 32;
      PixelSize := 4;
      Proc := PutPixel32P;
    end else begin
      BI.bmiHeader.biBitCount := 24;
      PixelSize := 3;
      Proc := PutPixel24;
    end;
    DstLineSize := (DstWidth * PixelSize + 3) and not 3;
    SrcLineSize := (SrcWidth * PixelSize + 3) and not 3;
    GetMem(tmpBits, SrcHeight * DstLineSize);
    try
      GetMem(SrcBits, SrcLineSize * SrcHeight);
      try
        if GetDIBits(Canvas.Handle, SrcBitmap.Handle,
          0, SrcHeight, SrcBits, BI, DIB_RGB_COLORS) = 0 then Exit;
        //Stretch horizontally
        ResampleBits(DstWidth, SrcWidth, SrcBits, tmpBits,
          PixelSize, SrcHeight, SrcLineSize, DstLineSize, Proc);
      finally
        FreeMem(SrcBits);
      end;
      BI.bmiHeader.biWidth := DstWidth;
      BI.bmiHeader.biHeight := DstHeight;
      DIB := CreateDIBSection(Canvas.Handle, BI, DIB_RGB_COLORS, DstBits, NULL, 0);
      if DIB = 0 then Exit;
      try
        //Stretch vertically
        ResampleBits(DstHeight, SrcHeight, tmpBits, DstBits,
          DstLineSize, DstWidth, PixelSize, PixelSize, Proc);
        DstBitmap.Handle := DIB;
        Result := True;
      except
        DeleteObject(DIB);
        raise;
      end;
    finally
      FreeMem(tmpBits);
    end;
  except
  end;
end;

end.