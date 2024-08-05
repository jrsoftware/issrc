unit Compression.Zlib;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Declarations for zlib functions & structures
}

interface

uses
  Windows, SysUtils, Compression.Base;

function ZlibInitCompressFunctions(Module: HMODULE): Boolean;
function ZlibInitDecompressFunctions(Module: HMODULE): Boolean;

type
  TZAlloc = function(AppData: Pointer; Items, Size: Cardinal): Pointer; stdcall;
  TZFree = procedure(AppData, Block: Pointer); stdcall;
  TZStreamRec = packed record
    next_in: Pointer;     { next input byte }
    avail_in: Cardinal;   { number of bytes available at next_in }
    total_in: Cardinal;   { total nb of input bytes read so far }

    next_out: Pointer;    { next output byte should be put here }
    avail_out: Cardinal;  { remaining free space at next_out }
    total_out: Cardinal;  { total nb of bytes output so far }

    msg: PAnsiChar;       { last error message, NULL if no error }
    internal: Pointer;    { not visible by applications }

    zalloc: TZAlloc;      { used to allocate the internal state }
    zfree: TZFree;        { used to free the internal state }
    AppData: Pointer;     { private data object passed to zalloc and zfree }

    data_type: Integer;   { best guess about the data type: ascii or binary }
    adler: Longint;       { adler32 value of the uncompressed data }
    reserved: Longint;    { reserved for future use }
  end;

  TZCompressor = class(TCustomCompressor)
  private
    FCompressionLevel: Integer;
    FInitialized: Boolean;
    FStrm: TZStreamRec;
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

  TZDecompressor = class(TCustomDecompressor)
  private
    FInitialized: Boolean;
    FStrm: TZStreamRec;
    FReachedEnd: Boolean;
    FBuffer: array[0..65535] of Byte;
  public
    constructor Create(AReadProc: TDecompressorReadProc); override;
    destructor Destroy; override;
    procedure DecompressInto(var Buffer; Count: Longint); override;
    procedure Reset; override;
  end;

implementation

const
  SZlibDataError = 'zlib: Compressed data is corrupted';
  SZlibInternalError = 'zlib: Internal error. Code %d';

  ZLIB_VERSION = '1.2.1';  { Do not change this! }

  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;

  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = -1;
  Z_STREAM_ERROR  = -2;
  Z_DATA_ERROR    = -3;
  Z_MEM_ERROR     = -4;
  Z_BUF_ERROR     = -5;
  Z_VERSION_ERROR = -6;

var
  deflateInit_: function(var strm: TZStreamRec; level: Integer; version: PAnsiChar;
    stream_size: Integer): Integer; stdcall;
  deflate: function(var strm: TZStreamRec; flush: Integer): Integer; stdcall;
  deflateEnd: function(var strm: TZStreamRec): Integer; stdcall;
  inflateInit_: function(var strm: TZStreamRec; version: PAnsiChar;
    stream_size: Integer): Integer; stdcall;
  inflate: function(var strm: TZStreamRec; flush: Integer): Integer; stdcall;
  inflateEnd: function(var strm: TZStreamRec): Integer; stdcall;
  inflateReset: function(var strm: TZStreamRec): Integer; stdcall;

function ZlibInitCompressFunctions(Module: HMODULE): Boolean;
begin
  deflateInit_ := GetProcAddress(Module, 'deflateInit_');
  deflate := GetProcAddress(Module, 'deflate');
  deflateEnd := GetProcAddress(Module, 'deflateEnd');
  Result := Assigned(deflateInit_) and Assigned(deflate) and
    Assigned(deflateEnd);
  if not Result then begin
    deflateInit_ := nil;
    deflate := nil;
    deflateEnd := nil;
  end;
end;

function ZlibInitDecompressFunctions(Module: HMODULE): Boolean;
begin
  inflateInit_ := GetProcAddress(Module, 'inflateInit_');
  inflate := GetProcAddress(Module, 'inflate');
  inflateEnd := GetProcAddress(Module, 'inflateEnd');
  inflateReset := GetProcAddress(Module, 'inflateReset');
  Result := Assigned(inflateInit_) and Assigned(inflate) and
    Assigned(inflateEnd) and Assigned(inflateReset);
  if not Result then begin
    inflateInit_ := nil;
    inflate := nil;
    inflateEnd := nil;
    inflateReset := nil;
  end;
end;

function zlibAllocMem(AppData: Pointer; Items, Size: Cardinal): Pointer; stdcall;
begin
  try
    GetMem(Result, Items * Size);
  except
    { trap any exception, because zlib expects a NULL result if it's out
      of memory }
    Result := nil;
  end;
end;

procedure zlibFreeMem(AppData, Block: Pointer); stdcall;
begin
  FreeMem(Block);
end;

function Check(const Code: Integer; const ValidCodes: array of Integer): Integer;
var
  I: Integer;
begin
  if Code = Z_MEM_ERROR then
    OutOfMemoryError;
  Result := Code;
  for I := Low(ValidCodes) to High(ValidCodes) do
    if ValidCodes[I] = Code then
      Exit;
  raise ECompressInternalError.CreateFmt(SZlibInternalError, [Code]);
end;

procedure InitStream(var strm: TZStreamRec);
begin
  FillChar(strm, SizeOf(strm), 0);
  with strm do begin
    zalloc := zlibAllocMem;
    zfree := zlibFreeMem;
  end;
end;

{ TZCompressor }

constructor TZCompressor.Create(AWriteProc: TCompressorWriteProc;
  AProgressProc: TCompressorProgressProc; CompressionLevel: Integer;
  ACompressorProps: TCompressorProps);
begin
  inherited;
  FCompressionLevel := CompressionLevel;
  InitCompress;
end;

destructor TZCompressor.Destroy;
begin
  EndCompress;
  inherited;
end;

procedure TZCompressor.InitCompress;
begin
  { Note: This really ought to use the more efficient deflateReset when
    starting a new stream, but our DLL doesn't currently export it. }
  if not FInitialized then begin
    InitStream(FStrm);
    FStrm.next_out := @FBuffer;
    FStrm.avail_out := SizeOf(FBuffer);
    Check(deflateInit_(FStrm, FCompressionLevel, zlib_version, SizeOf(FStrm)), [Z_OK]);
    FInitialized := True;
  end;
end;

procedure TZCompressor.EndCompress;
begin
  if FInitialized then begin
    FInitialized := False;
    deflateEnd(FStrm);
  end;
end;

procedure TZCompressor.FlushBuffer;
begin
  if FStrm.avail_out < SizeOf(FBuffer) then begin
    WriteProc(FBuffer, SizeOf(FBuffer) - FStrm.avail_out);
    FStrm.next_out := @FBuffer;
    FStrm.avail_out := SizeOf(FBuffer);
  end;
end;

procedure TZCompressor.DoCompress(const Buffer; Count: Longint);
begin
  InitCompress;
  FStrm.next_in := @Buffer;
  FStrm.avail_in := Count;
  while FStrm.avail_in > 0 do begin
    Check(deflate(FStrm, Z_NO_FLUSH), [Z_OK]);
    if FStrm.avail_out = 0 then
      FlushBuffer;
  end;
  if Assigned(ProgressProc) then
    ProgressProc(Count);
end;

procedure TZCompressor.DoFinish;
begin
  InitCompress;
  FStrm.next_in := nil;
  FStrm.avail_in := 0;
  { Note: This assumes FStrm.avail_out > 0. This shouldn't be a problem since
    Compress always flushes when FStrm.avail_out reaches 0. }
  while Check(deflate(FStrm, Z_FINISH), [Z_OK, Z_STREAM_END]) <> Z_STREAM_END do
    FlushBuffer;
  FlushBuffer;
  EndCompress;
end;

{ TZDecompressor }

constructor TZDecompressor.Create(AReadProc: TDecompressorReadProc);
begin
  inherited Create(AReadProc);
  InitStream(FStrm);
  FStrm.next_in := @FBuffer;
  FStrm.avail_in := 0;
  Check(inflateInit_(FStrm, zlib_version, SizeOf(FStrm)), [Z_OK]);
  FInitialized := True;
end;

destructor TZDecompressor.Destroy;
begin
  if FInitialized then
    inflateEnd(FStrm);
  inherited Destroy;
end;

procedure TZDecompressor.DecompressInto(var Buffer; Count: Longint);
begin
  FStrm.next_out := @Buffer;
  FStrm.avail_out := Count;
  while FStrm.avail_out > 0 do begin
    if FReachedEnd then  { unexpected EOF }
      raise ECompressDataError.Create(SZlibDataError);
    if FStrm.avail_in = 0 then begin
      FStrm.next_in := @FBuffer;
      FStrm.avail_in := ReadProc(FBuffer, SizeOf(FBuffer));
      { Note: If avail_in is zero while zlib still needs input, inflate() will
        return Z_BUF_ERROR. We interpret that as a data error (see below). }
    end;
    case Check(inflate(FStrm, Z_NO_FLUSH), [Z_OK, Z_STREAM_END, Z_DATA_ERROR, Z_BUF_ERROR]) of
      Z_STREAM_END: FReachedEnd := True;
      Z_DATA_ERROR, Z_BUF_ERROR: raise ECompressDataError.Create(SZlibDataError);
    end;
  end;
end;

procedure TZDecompressor.Reset;
begin
  FStrm.next_in := @FBuffer;
  FStrm.avail_in := 0;
  Check(inflateReset(FStrm), [Z_OK]);
  FReachedEnd := False;
end;

end.
