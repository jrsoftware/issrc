unit Compression.bzlib;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Declarations for some bzlib2 functions & structures
}

interface

uses
  Windows, SysUtils, Compression.Base;

function BZInitCompressFunctions(Module: HMODULE): Boolean;
function BZInitDecompressFunctions(Module: HMODULE): Boolean;

type
  TBZAlloc = function(AppData: Pointer; Items, Size: Cardinal): Pointer; stdcall;
  TBZFree = procedure(AppData, Block: Pointer); stdcall;
  TBZStreamRec = record
    next_in: Pointer;
    avail_in: Integer;
    total_in: Integer;
    total_in_hi: Integer;

    next_out: Pointer;
    avail_out: Integer;
    total_out: Integer;
    total_out_hi: Integer;

    State: Pointer;

    zalloc: TBZAlloc;
    zfree: TBZFree;
    AppData: Pointer;
  end;

  TBZCompressor = class(TCustomCompressor)
  private
    FCompressionLevel: Integer;
    FInitialized: Boolean;
    FStrm: TBZStreamRec;
    FBuffer: array[0..65535] of Byte;
    procedure EndCompress;
    procedure FlushBuffer;
    procedure InitCompress;
  protected
    procedure DoCompress(const Buffer; Count: Longint); override;
    procedure DoFinish; override;
  public
    constructor Create(AWriteProc: TCompressorWriteProc;
      AProgressProc: TCompressorProgressProc; CompressionLevel: Integer;
      ACompressorProps: TCompressorProps); override;
    destructor Destroy; override;
  end;

  TBZDecompressor = class(TCustomDecompressor)
  private
    FInitialized: Boolean;
    FStrm: TBZStreamRec;
    FReachedEnd: Boolean;
    FBuffer: array[0..65535] of Byte;
    FHeapBase, FHeapNextFree: Pointer;
    function Malloc(Bytes: Cardinal): Pointer;
  public
    constructor Create(AReadProc: TDecompressorReadProc); override;
    destructor Destroy; override;
    procedure DecompressInto(var Buffer; Count: Longint); override;
    procedure Reset; override;
  end;

implementation

var
  BZ2_bzCompressInit: function(var strm: TBZStreamRec;
    blockSize100k, verbosity, workFactor: Integer): Integer; stdcall;
  BZ2_bzCompress: function(var strm: TBZStreamRec;
    action: Integer): Integer; stdcall;
  BZ2_bzCompressEnd: function(var strm: TBZStreamRec): Integer; stdcall;
  BZ2_bzDecompressInit: function(var strm: TBZStreamRec;
    verbosity, small: Integer): Integer; stdcall;
  BZ2_bzDecompress: function(var strm: TBZStreamRec): Integer; stdcall;
  BZ2_bzDecompressEnd: function(var strm: TBZStreamRec): Integer; stdcall;

const
  BZ_RUN              = 0;
  BZ_FLUSH            = 1;
  BZ_FINISH           = 2;

  BZ_OK               = 0;
  BZ_RUN_OK           = 1;
  BZ_FLUSH_OK         = 2;
  BZ_FINISH_OK        = 3;
  BZ_STREAM_END       = 4;
  BZ_SEQUENCE_ERROR   = (-1);
  BZ_PARAM_ERROR      = (-2);
  BZ_MEM_ERROR        = (-3);
  BZ_DATA_ERROR       = (-4);
  BZ_DATA_ERROR_MAGIC = (-5);
  BZ_IO_ERROR         = (-6);
  BZ_UNEXPECTED_EOF   = (-7);
  BZ_OUTBUFF_FULL     = (-8);
  BZ_CONFIG_ERROR     = (-9);

  SBzlibDataError = 'bzlib: Compressed data is corrupted';
  SBzlibInternalError = 'bzlib: Internal error. Code %d';
  SBzlibAllocError = 'bzlib: Too much memory requested';

function BZInitCompressFunctions(Module: HMODULE): Boolean;
begin
  BZ2_bzCompressInit := GetProcAddress(Module, 'BZ2_bzCompressInit');
  BZ2_bzCompress := GetProcAddress(Module, 'BZ2_bzCompress');
  BZ2_bzCompressEnd := GetProcAddress(Module, 'BZ2_bzCompressEnd');
  Result := Assigned(BZ2_bzCompressInit) and Assigned(BZ2_bzCompress) and
    Assigned(BZ2_bzCompressEnd);
  if not Result then begin
    BZ2_bzCompressInit := nil;
    BZ2_bzCompress := nil;
    BZ2_bzCompressEnd := nil;
  end;
end;

function BZInitDecompressFunctions(Module: HMODULE): Boolean;
begin
  BZ2_bzDecompressInit := GetProcAddress(Module, 'BZ2_bzDecompressInit');
  BZ2_bzDecompress := GetProcAddress(Module, 'BZ2_bzDecompress');
  BZ2_bzDecompressEnd := GetProcAddress(Module, 'BZ2_bzDecompressEnd');
  Result := Assigned(BZ2_bzDecompressInit) and Assigned(BZ2_bzDecompress) and
    Assigned(BZ2_bzDecompressEnd);
  if not Result then begin
    BZ2_bzDecompressInit := nil;
    BZ2_bzDecompress := nil;
    BZ2_bzDecompressEnd := nil;
  end;
end;

function BZAllocMem(AppData: Pointer; Items, Size: Cardinal): Pointer; stdcall;
begin
  try
    GetMem(Result, Items * Size);
  except
    { trap any exception, because zlib expects a NULL result if it's out
      of memory }
    Result := nil;
  end;
end;

procedure BZFreeMem(AppData, Block: Pointer); stdcall;
begin
  FreeMem(Block);
end;

function Check(const Code: Integer; const ValidCodes: array of Integer): Integer;
var
  I: Integer;
begin
  if Code = BZ_MEM_ERROR then
    OutOfMemoryError;
  Result := Code;
  for I := Low(ValidCodes) to High(ValidCodes) do
    if ValidCodes[I] = Code then
      Exit;
  raise ECompressInternalError.CreateFmt(SBzlibInternalError, [Code]);
end;

procedure InitStream(var strm: TBZStreamRec);
begin
  FillChar(strm, SizeOf(strm), 0);
  with strm do begin
    zalloc := BZAllocMem;
    zfree := BZFreeMem;
  end;
end;

{ TBZCompressor }

constructor TBZCompressor.Create(AWriteProc: TCompressorWriteProc;
  AProgressProc: TCompressorProgressProc; CompressionLevel: Integer;
  ACompressorProps: TCompressorProps);
begin
  inherited;
  FCompressionLevel := CompressionLevel;
  InitCompress;
end;

destructor TBZCompressor.Destroy;
begin
  EndCompress;
  inherited;
end;

procedure TBZCompressor.InitCompress;
begin
  if not FInitialized then begin
    InitStream(FStrm);
    FStrm.next_out := @FBuffer;
    FStrm.avail_out := SizeOf(FBuffer);
    Check(BZ2_bzCompressInit(FStrm, FCompressionLevel, 0, 0), [BZ_OK]);
    FInitialized := True;
  end;
end;

procedure TBZCompressor.EndCompress;
begin
  if FInitialized then begin
    FInitialized := False;
    BZ2_bzCompressEnd(FStrm);
  end;
end;

procedure TBZCompressor.FlushBuffer;
begin
  if FStrm.avail_out < SizeOf(FBuffer) then begin
    WriteProc(FBuffer, SizeOf(FBuffer) - FStrm.avail_out);
    FStrm.next_out := @FBuffer;
    FStrm.avail_out := SizeOf(FBuffer);
  end;
end;

procedure TBZCompressor.DoCompress(const Buffer; Count: Longint);
begin
  InitCompress;
  FStrm.next_in := @Buffer;
  FStrm.avail_in := Count;
  while FStrm.avail_in > 0 do begin
    Check(BZ2_bzCompress(FStrm, BZ_RUN), [BZ_RUN_OK]);
    if FStrm.avail_out = 0 then
      FlushBuffer;
  end;
  if Assigned(ProgressProc) then
    ProgressProc(Count);
end;

procedure TBZCompressor.DoFinish;
begin
  InitCompress;
  FStrm.next_in := nil;
  FStrm.avail_in := 0;
  { Note: This assumes FStrm.avail_out > 0. This shouldn't be a problem since
    Compress always flushes when FStrm.avail_out reaches 0. }
  while Check(BZ2_bzCompress(FStrm, BZ_FINISH), [BZ_FINISH_OK, BZ_STREAM_END]) <> BZ_STREAM_END do
    FlushBuffer;
  FlushBuffer;
  EndCompress;
end;

{ TBZDecompressor }

{ Why does TBZDecompressor use VirtualAlloc instead of GetMem?
  It IS 4.0.1 it did use GetMem and allocate blocks on demand, but thanks to
  Delphi's flawed memory manager this resulted in crippling memory
  fragmentation when Reset was called repeatedly (e.g. when an installation
  contained thousands of files and solid decompression was disabled) while
  Setup was allocating other small blocks (e.g. FileLocationFilenames[]), and
  eventually caused Setup to run out of virtual address space.
  So, it was changed to allocate only one chunk of virtual address space for
  the entire lifetime of the TBZDecompressor instance. It divides this chunk
  into smaller amounts as requested by bzlib. As IS only creates one instance
  of TBZDecompressor, this change should completely eliminate the
  fragmentation issue. }

const
  DecompressorHeapSize = $600000;
  { 6 MB should be more than enough; the most I've seen bzlib 1.0.2's
    bzDecompress* allocate is 64116 + 3600000 bytes, when decompressing data
    compressed at level 9 }

function DecompressorAllocMem(AppData: Pointer; Items, Size: Cardinal): Pointer; stdcall;
begin
  Result := TBZDecompressor(AppData).Malloc(Items * Size);
end;

procedure DecompressorFreeMem(AppData, Block: Pointer); stdcall;
begin
  { Since bzlib doesn't repeatedly deallocate and allocate blocks during a
    decompression run, we don't have to handle frees. }
end;

constructor TBZDecompressor.Create(AReadProc: TDecompressorReadProc);
begin
  inherited Create(AReadProc);
  FHeapBase := VirtualAlloc(nil, DecompressorHeapSize, MEM_RESERVE, PAGE_NOACCESS);
  if FHeapBase = nil then
    OutOfMemoryError;
  FHeapNextFree := FHeapBase;
  FStrm.AppData := Self;
  FStrm.zalloc := DecompressorAllocMem;
  FStrm.zfree := DecompressorFreeMem;
  FStrm.next_in := @FBuffer;
  FStrm.avail_in := 0;
  Check(BZ2_bzDecompressInit(FStrm, 0, 0), [BZ_OK]);
  FInitialized := True;
end;

destructor TBZDecompressor.Destroy;
begin
  if FInitialized then
    BZ2_bzDecompressEnd(FStrm);
  if Assigned(FHeapBase) then
    VirtualFree(FHeapBase, 0, MEM_RELEASE);
  inherited Destroy;
end;

function TBZDecompressor.Malloc(Bytes: Cardinal): Pointer;
begin
  { Round up to dword boundary if necessary }
  if Bytes mod 4 <> 0 then
    Inc(Bytes, 4 - Bytes mod 4);

  { Did bzlib request more memory than we reserved? This shouldn't happen
    unless this unit is used with a different version of bzlib that allocates
    more memory. Note: The funky Cardinal casts are there to convince
    Delphi (2) to do an unsigned compare. }
  if Cardinal(Cardinal(FHeapNextFree) - Cardinal(FHeapBase) + Bytes) > Cardinal(DecompressorHeapSize) then
    raise ECompressInternalError.Create(SBzlibAllocError);

  if VirtualAlloc(FHeapNextFree, Bytes, MEM_COMMIT, PAGE_READWRITE) = nil then
    Result := nil
  else begin
    Result := FHeapNextFree;
    Inc(Cardinal(FHeapNextFree), Bytes);
  end;
end;

procedure TBZDecompressor.DecompressInto(var Buffer; Count: Longint);
begin
  FStrm.next_out := @Buffer;
  FStrm.avail_out := Count;
  while FStrm.avail_out > 0 do begin
    if FReachedEnd then  { unexpected EOF }
      raise ECompressDataError.Create(SBzlibDataError);
    if FStrm.avail_in = 0 then begin
      FStrm.next_in := @FBuffer;
      FStrm.avail_in := ReadProc(FBuffer, SizeOf(FBuffer));
      { Unlike zlib, bzlib does not return an error when avail_in is zero and
        it still needs input. To avoid an infinite loop, check for this and
        consider it a data error. }
      if FStrm.avail_in = 0 then
        raise ECompressDataError.Create(SBzlibDataError);
    end;
    case Check(BZ2_bzDecompress(FStrm), [BZ_OK, BZ_STREAM_END, BZ_DATA_ERROR, BZ_DATA_ERROR_MAGIC]) of
      BZ_STREAM_END: FReachedEnd := True;
      BZ_DATA_ERROR, BZ_DATA_ERROR_MAGIC: raise ECompressDataError.Create(SBzlibDataError);
    end;
  end;
end;

procedure TBZDecompressor.Reset;
begin
  FStrm.next_in := @FBuffer;
  FStrm.avail_in := 0;
  { bzlib doesn't offer an optimized 'Reset' function like zlib }
  BZ2_bzDecompressEnd(FStrm);
  FHeapNextFree := FHeapBase;  { discard previous allocations }
  Check(BZ2_bzDecompressInit(FStrm, 0, 0), [BZ_OK]);
  FReachedEnd := False;
end;

end.
