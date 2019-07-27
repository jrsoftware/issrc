unit Resample;
interface
uses
  Windows, Math, Graphics;

function StretchBmp(SrcBitmap, DstBitmap: TBitmap;
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


function StretchBmp(SrcBitmap, DstBitmap: TBitmap;
  DstWidth, DstHeight: Integer; Is32bit: Boolean): Boolean;
var
  SrcWidth, SrcHeight, SrcLineSize, DstLineSize, PixelSize: Integer;
  SrcBits, DstBits, TmpBits: Pointer;
  PixelFormat: TPixelFormat;
  Proc: TPutPixelProc;
begin
  Result := False;
  try
    if (DstWidth <= 0) or (DstHeight <= 0) then Exit;
    SrcWidth := SrcBitmap.Width;
    SrcHeight := SrcBitmap.Height;
    if (SrcWidth <= 0) or (SrcHeight <= 0) then Exit;
    if Is32bit then begin
      PixelFormat := pf32bit;
      PixelSize := 4;
      Proc := PutPixel32P;
    end else begin
      PixelFormat := pf24bit;
      PixelSize := 3;
      Proc := PutPixel24;
    end;
    //NOTE: Irreversible change of SrcBitmap pixel format
    SrcBitmap.PixelFormat := PixelFormat;
    SrcLineSize := WPARAM(SrcBitmap.ScanLine[0]) - WPARAM(SrcBitmap.ScanLine[1]);
    if SrcLineSize >= 0 then
      SrcBits := SrcBitmap.ScanLine[SrcHeight - 1]
    else begin
      SrcLineSize := -SrcLineSize;
      SrcBits := SrcBitmap.ScanLine[0];
    end;
    DstBitmap.PixelFormat := PixelFormat;
    DstBitmap.AlphaFormat := SrcBitmap.AlphaFormat;
    DstBitmap.Width := DstWidth;
    DstBitmap.Height := DstHeight;
    DstLineSize := WPARAM(DstBitmap.ScanLine[0]) - WPARAM(DstBitmap.ScanLine[1]);
    if DstLineSize >= 0 then
      DstBits := DstBitmap.ScanLine[DstHeight - 1]
    else begin
      DstLineSize := -DstLineSize;
      DstBits := DstBitmap.ScanLine[0];
    end;
    TmpBits := nil;
    try
      //Minimize temporary allocations by choosing right stretch order
      if DstWidth * SrcHeight < DstHeight * SrcWidth then begin
        GetMem(TmpBits, SrcHeight * DstLineSize);
        //Stretch horizontally
        ResampleBits(DstWidth, SrcWidth, SrcBits, TmpBits, PixelSize,
          SrcHeight, SrcLineSize, DstLineSize, Proc);
        //Stretch vertically
        ResampleBits(DstHeight, SrcHeight, TmpBits, DstBits, DstLineSize,
          DstWidth, PixelSize, PixelSize, Proc);
      end else begin
        GetMem(TmpBits, DstHeight * SrcLineSize);
        //Stretch vertically
        ResampleBits(DstHeight, SrcHeight, SrcBits, TmpBits, SrcLineSize,
          SrcWidth, PixelSize, PixelSize, Proc);
        //Stretch horizontally
        ResampleBits(DstWidth, SrcWidth, TmpBits, DstBits, PixelSize,
          DstHeight, SrcLineSize, DstLineSize, Proc);
      end;
      Result := True;
    finally
      FreeMem(TmpBits);
    end;
  except
  end;
end;

end.