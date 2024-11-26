unit Compression.Base;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Abstract (de)compression classes, and some generic (de)compression-related functions
}

interface

uses
  Windows, SysUtils, Shared.Int64Em, Shared.FileClass;

type
  ECompressError = class(Exception);
  ECompressDataError = class(ECompressError);
  ECompressInternalError = class(ECompressError);

  TCompressorProps = class
  end;

  TCompressorProgressProc = procedure(BytesProcessed: Cardinal) of object;
  TCompressorWriteProc = procedure(const Buffer; Count: Longint) of object;
  TCustomCompressorClass = class of TCustomCompressor;
  TCustomCompressor = class
  private
    FEntered: Integer;
    FProgressProc: TCompressorProgressProc;
    FWriteProc: TCompressorWriteProc;
  protected
    procedure DoCompress(const Buffer; Count: Longint); virtual; abstract;
    procedure DoFinish; virtual; abstract;
    property ProgressProc: TCompressorProgressProc read FProgressProc;
    property WriteProc: TCompressorWriteProc read FWriteProc;
  public
    constructor Create(AWriteProc: TCompressorWriteProc;
      AProgressProc: TCompressorProgressProc; CompressionLevel: Integer;
      ACompressorProps: TCompressorProps); virtual;
    procedure Compress(const Buffer; Count: Longint);
    procedure Finish;
  end;

  TDecompressorReadProc = function(var Buffer; Count: Longint): Longint of object;
  TCustomDecompressorClass = class of TCustomDecompressor;
  TCustomDecompressor = class
  private
    FReadProc: TDecompressorReadProc;
  protected
    property ReadProc: TDecompressorReadProc read FReadProc;
  public
    constructor Create(AReadProc: TDecompressorReadProc); virtual;
    procedure DecompressInto(var Buffer; Count: Longint); virtual; abstract;
    procedure Reset; virtual; abstract;
  end;

  { TStoredCompressor is a compressor which doesn't actually compress }
  TStoredCompressor = class(TCustomCompressor)
  protected
    procedure DoCompress(const Buffer; Count: Longint); override;
    procedure DoFinish; override;
  end;

  TStoredDecompressor = class(TCustomDecompressor)
  public
    procedure DecompressInto(var Buffer; Count: Longint); override;
    procedure Reset; override;
  end;

  TCompressedBlockWriter = class
  private
    FCompressor: TCustomCompressor;
    FFile: TFile;
    FStartPos: Integer64;
    FTotalBytesStored: Cardinal;
    FInBufferCount, FOutBufferCount: Cardinal;
    FInBuffer, FOutBuffer: array[0..4095] of Byte;
    procedure CompressorWriteProc(const Buffer; Count: Longint);
    procedure DoCompress(const Buf; var Count: Cardinal);
    procedure FlushOutputBuffer;
  public
    constructor Create(AFile: TFile; ACompressorClass: TCustomCompressorClass;
      CompressionLevel: Integer; ACompressorProps: TCompressorProps);
    destructor Destroy; override;
    procedure Finish;
    procedure Write(const Buffer; Count: Cardinal);
  end;

  TCompressedBlockReader = class
  private
    FDecompressor: TCustomDecompressor;
    FFile: TFile;
    FInBytesLeft: Cardinal;
    FInitialized: Boolean;
    FInBufferNext: Cardinal;
    FInBufferAvail: Cardinal;
    FInBuffer: array[0..4095] of Byte;
    function DecompressorReadProc(var Buffer; Count: Longint): Longint;
    procedure ReadChunk;
  public
    constructor Create(AFile: TFile; ADecompressorClass: TCustomDecompressorClass);
    destructor Destroy; override;
    procedure Read(var Buffer; Count: Cardinal);
  end;

function GetCRC32(const Buf; BufSize: Cardinal): Longint;
procedure TransformCallInstructions(var Buf; Size: Integer;
  const Encode: Boolean; const AddrOffset: LongWord);
function UpdateCRC32(CurCRC: Longint; const Buf; BufSize: Cardinal): Longint;

implementation

const
  SCompressorStateInvalid = 'Compressor state invalid';
  SStoredDataError = 'Unexpected end of stream';
  SCompressedBlockDataError = 'Compressed block is corrupted';

var
  CRC32TableInited: BOOL;
  CRC32Table: array[Byte] of Longint;

procedure InitCRC32Table;
var
  CRC: Longint;
  I, N: Integer;
begin
  for I := 0 to 255 do begin
    CRC := I;
    for N := 0 to 7 do begin
      if Odd(CRC) then
        CRC := (CRC shr 1) xor Longint($EDB88320)
      else
        CRC := CRC shr 1;
    end;
    Crc32Table[I] := CRC;
  end;
end;

function UpdateCRC32(CurCRC: Longint; const Buf; BufSize: Cardinal): Longint;
var
  P: ^Byte;
begin
  if not CRC32TableInited then begin
    InitCRC32Table;
    InterlockedExchange(Integer(CRC32TableInited), Ord(True));
  end;
  P := @Buf;
  while BufSize <> 0 do begin
    CurCRC := CRC32Table[Lo(CurCRC) xor P^] xor (CurCRC shr 8);
    Dec(BufSize);
    Inc(P);
  end;
  Result := CurCRC;
end;

function GetCRC32(const Buf; BufSize: Cardinal): Longint;
begin
  Result := UpdateCRC32(Longint($FFFFFFFF), Buf, BufSize) xor Longint($FFFFFFFF);
end;

procedure TransformCallInstructions(var Buf; Size: Integer;
  const Encode: Boolean; const AddrOffset: LongWord);
{ [Version 3] Converts relative addresses in x86/x64 CALL and JMP instructions
  to absolute addresses if Encode is True, or the inverse if Encode is False. }
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..$7FFFFFFE] of Byte;
var
  P: PByteArray;
  I: Integer;
  Addr, Rel: LongWord;
begin
  if Size < 5 then
    Exit;
  Dec(Size, 4);
  P := @Buf;
  I := 0;
  while I < Size do begin
    { Does it appear to be a CALL or JMP instruction with a relative 32-bit
      address? }
    if (P[I] = $E8) or (P[I] = $E9) then begin
      Inc(I);
      { Verify that the high byte of the address is $00 or $FF. If it isn't,
        then what we've encountered most likely isn't a CALL or JMP. }
      if (P[I+3] = $00) or (P[I+3] = $FF) then begin
        { Change the lower 3 bytes of the address to be relative to the
          beginning of the buffer, instead of to the next instruction. If
          decoding, do the opposite. }
        Addr := (AddrOffset + LongWord(I) + 4) and $FFFFFF;  { may wrap, but OK }
        Rel := P[I] or (P[I+1] shl 8) or (P[I+2] shl 16);
        if not Encode then
          Dec(Rel, Addr);
        { For a slightly higher compression ratio, we want the resulting high
          byte to be $00 for both forward and backward jumps. The high byte
          of the original relative address is likely to be the sign extension
          of bit 23, so if bit 23 is set, toggle all bits in the high byte. }
        if Rel and $800000 <> 0 then
          P[I+3] := not P[I+3];
        if Encode then
          Inc(Rel, Addr);
        P[I] := Byte(Rel);
        P[I+1] := Byte(Rel shr 8);
        P[I+2] := Byte(Rel shr 16);
      end;
      Inc(I, 4);
    end
    else
      Inc(I);
  end;
end;

{ TCustomCompressor }

constructor TCustomCompressor.Create(AWriteProc: TCompressorWriteProc;
  AProgressProc: TCompressorProgressProc; CompressionLevel: Integer;
  ACompressorProps: TCompressorProps);
begin
  inherited Create;
  FWriteProc := AWriteProc;
  FProgressProc := AProgressProc;
end;

procedure TCustomCompressor.Compress(const Buffer; Count: Longint);
begin
  if FEntered <> 0 then
    raise ECompressInternalError.Create(SCompressorStateInvalid);
  Inc(FEntered);
  DoCompress(Buffer, Count);
  Dec(FEntered);
end;

procedure TCustomCompressor.Finish;
begin
  if FEntered <> 0 then
    raise ECompressInternalError.Create(SCompressorStateInvalid);
  Inc(FEntered);
  DoFinish;
  Dec(FEntered);
end;

{ TCustomDecompressor }

constructor TCustomDecompressor.Create(AReadProc: TDecompressorReadProc);
begin
  inherited Create;
  FReadProc := AReadProc;
end;

{ TStoredCompressor }

procedure TStoredCompressor.DoCompress(const Buffer; Count: Longint);
begin
  WriteProc(Buffer, Count);
  if Assigned(ProgressProc) then
    ProgressProc(Count);
end;

procedure TStoredCompressor.DoFinish;
begin
end;

{ TStoredDecompressor }

procedure TStoredDecompressor.DecompressInto(var Buffer; Count: Longint);
var
  P: ^Byte;
  NumRead: Longint;
begin
  P := @Buffer;
  while Count > 0 do begin
    NumRead := ReadProc(P^, Count);
    if NumRead = 0 then
      raise ECompressDataError.Create(SStoredDataError);
    Inc(P, NumRead);
    Dec(Count, NumRead);
  end;
end;

procedure TStoredDecompressor.Reset;
begin
end;

{ TCompressedBlockWriter }

type
  TCompressedBlockHeader = packed record
    StoredSize: LongWord;   { Total bytes written, including the CRCs }
    Compressed: Boolean;    { True if data is compressed, False if not }
  end;

constructor TCompressedBlockWriter.Create(AFile: TFile;
  ACompressorClass: TCustomCompressorClass; CompressionLevel: Integer;
  ACompressorProps: TCompressorProps);
var
  HdrCRC: Longint;
  Hdr: TCompressedBlockHeader;
begin
  inherited Create;

  FFile := AFile;
  if Assigned(ACompressorClass) and (CompressionLevel <> 0) then
    FCompressor := ACompressorClass.Create(CompressorWriteProc, nil,
      CompressionLevel, ACompressorProps);
  FStartPos := AFile.Position;

  { Note: These will be overwritten by Finish }
  HdrCRC := 0;
  AFile.WriteBuffer(HdrCRC, SizeOf(HdrCRC));
  Hdr.StoredSize := 0;
  Hdr.Compressed := False;
  AFile.WriteBuffer(Hdr, SizeOf(Hdr));
end;

destructor TCompressedBlockWriter.Destroy;
begin
  FCompressor.Free;
  inherited;
end;

procedure TCompressedBlockWriter.FlushOutputBuffer;
{ Flushes contents of FOutBuffer into the file, with a preceding CRC }
var
  CRC: Longint;
begin
  CRC := GetCRC32(FOutBuffer, FOutBufferCount);
  FFile.WriteBuffer(CRC, SizeOf(CRC));
  Inc(FTotalBytesStored, SizeOf(CRC));
  FFile.WriteBuffer(FOutBuffer, FOutBufferCount);
  Inc(FTotalBytesStored, FOutBufferCount);
  FOutBufferCount := 0;
end;

procedure TCompressedBlockWriter.CompressorWriteProc(const Buffer; Count: Longint);
var
  P: ^Byte;
  Bytes: Cardinal;
begin
  P := @Buffer;
  while Count > 0 do begin
    Bytes := Count;
    if Bytes > SizeOf(FOutBuffer) - FOutBufferCount then
      Bytes := SizeOf(FOutBuffer) - FOutBufferCount;
    Move(P^, FOutBuffer[FOutBufferCount], Bytes);
    Inc(FOutBufferCount, Bytes);
    if FOutBufferCount = SizeOf(FOutBuffer) then
      FlushOutputBuffer;
    Dec(Count, Bytes);
    Inc(P, Bytes);
  end;
end;

procedure TCompressedBlockWriter.DoCompress(const Buf; var Count: Cardinal);
begin
  if Count > 0 then begin
    if Assigned(FCompressor) then
      FCompressor.Compress(Buf, Count)
    else
      CompressorWriteProc(Buf, Count);
  end;
  Count := 0;
end;

procedure TCompressedBlockWriter.Write(const Buffer; Count: Cardinal);
var
  P: ^Byte;
  Bytes: Cardinal;
begin
  { Writes are buffered strictly as an optimization, to avoid feeding tiny
    blocks to the compressor }
  P := @Buffer;
  while Count > 0 do begin
    Bytes := Count;
    if Bytes > SizeOf(FInBuffer) - FInBufferCount then
      Bytes := SizeOf(FInBuffer) - FInBufferCount;
    Move(P^, FInBuffer[FInBufferCount], Bytes);
    Inc(FInBufferCount, Bytes);
    if FInBufferCount = SizeOf(FInBuffer) then
      DoCompress(FInBuffer, FInBufferCount);
    Dec(Count, Bytes);
    Inc(P, Bytes);
  end;
end;

procedure TCompressedBlockWriter.Finish;
var
  Pos: Integer64;
  HdrCRC: Longint;
  Hdr: TCompressedBlockHeader;
begin
  DoCompress(FInBuffer, FInBufferCount);
  if Assigned(FCompressor) then
    FCompressor.Finish;
  if FOutBufferCount > 0 then
    FlushOutputBuffer;

  Pos := FFile.Position;
  FFile.Seek64(FStartPos);
  Hdr.StoredSize := FTotalBytesStored;
  Hdr.Compressed := Assigned(FCompressor);
  HdrCRC := GetCRC32(Hdr, SizeOf(Hdr));
  FFile.WriteBuffer(HdrCRC, SizeOf(HdrCRC));
  FFile.WriteBuffer(Hdr, SizeOf(Hdr));
  FFile.Seek64(Pos);
end;

{ TCompressedBlockReader }

constructor TCompressedBlockReader.Create(AFile: TFile;
  ADecompressorClass: TCustomDecompressorClass);
var
  HdrCRC: Longint;
  Hdr: TCompressedBlockHeader;
  P: Integer64;
begin
  inherited Create;

  FFile := AFile;

  if (AFile.Read(HdrCRC, SizeOf(HdrCRC)) <> SizeOf(HdrCRC)) or
     (AFile.Read(Hdr, SizeOf(Hdr)) <> SizeOf(Hdr)) then
    raise ECompressDataError.Create(SCompressedBlockDataError);
  if HdrCRC <> GetCRC32(Hdr, SizeOf(Hdr)) then
    raise ECompressDataError.Create(SCompressedBlockDataError);
  P := AFile.Position;
  Inc64(P, Hdr.StoredSize);
  if Compare64(P, AFile.Size) > 0 then
    raise ECompressDataError.Create(SCompressedBlockDataError);
  if Hdr.Compressed then
    FDecompressor := ADecompressorClass.Create(DecompressorReadProc);
  FInBytesLeft := Hdr.StoredSize;
  FInitialized := True;
end;

destructor TCompressedBlockReader.Destroy;
var
  P: Integer64;
begin
  FDecompressor.Free;
  if FInitialized then begin
    { Must seek ahead if the caller didn't read everything that was originally
      compressed, or if it did read everything but zlib is in a "CHECK" state
      (i.e. it didn't read and verify the trailing adler32 yet due to lack of
      input bytes). }
    P := FFile.Position;
    Inc64(P, FInBytesLeft);
    FFile.Seek64(P);
  end;
  inherited;
end;

procedure TCompressedBlockReader.ReadChunk;
var
  CRC: Longint;
  Len: Cardinal;
begin
  { Read chunk CRC }
  if FInBytesLeft < SizeOf(CRC) + 1 then
    raise ECompressDataError.Create(SCompressedBlockDataError);
  FFile.ReadBuffer(CRC, SizeOf(CRC));
  Dec(FInBytesLeft, SizeOf(CRC));

  { Read chunk data }
  Len := FInBytesLeft;
  if Len > SizeOf(FInBuffer) then
    Len := SizeOf(FInBuffer);
  FFile.ReadBuffer(FInBuffer, Len);
  Dec(FInBytesLeft, Len);
  FInBufferNext := 0;
  FInBufferAvail := Len;
  if CRC <> GetCRC32(FInBuffer, Len) then
    raise ECompressDataError.Create(SCompressedBlockDataError);
end;

function TCompressedBlockReader.DecompressorReadProc(var Buffer;
  Count: Longint): Longint;
var
  P: ^Byte;
  Bytes: Cardinal;
begin
  Result := 0;
  P := @Buffer;
  while Count > 0 do begin
    if FInBufferAvail = 0 then begin
      if FInBytesLeft = 0 then
        Break;
      ReadChunk;
    end;
    Bytes := Count;
    if Bytes > FInBufferAvail then
      Bytes := FInBufferAvail;
    Move(FInBuffer[FInBufferNext], P^, Bytes);
    Inc(FInBufferNext, Bytes);
    Dec(FInBufferAvail, Bytes);
    Inc(P, Bytes);
    Dec(Count, Bytes);
    Inc(Result, Bytes);
  end;
end;

procedure TCompressedBlockReader.Read(var Buffer; Count: Cardinal);
begin
  if Assigned(FDecompressor) then
    FDecompressor.DecompressInto(Buffer, Count)
  else begin
    { Not compressed -- call DecompressorReadProc directly }
    if Cardinal(DecompressorReadProc(Buffer, Count)) <> Count then
      raise ECompressDataError.Create(SCompressedBlockDataError);
  end;
end;

end.
