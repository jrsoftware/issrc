unit Compression.Zstd;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Originally contributed by Amyspark <amy@amyspark.me>

  Declarations for Zstandard functions & structures
}

interface

uses
  Windows, SysUtils, Compression.Base;

function ZstdInitCompressFunctions(Module: HMODULE): Boolean;
function ZstdInitDecompressFunctions(Module: HMODULE): Boolean;

type
  TZSTD_inBuffer = record
    src: Pointer;        { start of input buffer }
    size: UInt64;        { size of input buffer }
    pos: UInt64;         { position where reading stopped. Will be updated. Necessarily 0 <= pos <= size }
  end;

  TZSTD_outBuffer = record
    src: Pointer;        { start of output buffer }
    size: UInt64;        { size of output buffer }
    pos: UInt64;         { position where writing stopped. Will be updated. Necessarily 0 <= pos <= size }
  end;

  TZSTD_frameProgression = record
    ingested: UInt64;   { nb input bytes read and buffered }
    consumed: UInt64;   { nb input bytes actually compressed }
    produced: UInt64;   { nb of compressed bytes generated and buffered }
    flushed: UInt64;    { nb of compressed bytes flushed : not provided; can be tracked from caller side }
    currentJobID: Cardinal;         { MT only : latest started job nb }
    nbActiveWorkers: Cardinal;      { MT only : nb of workers actively compressing at probe time }
  end;

  TZstdCompressor = class(TCustomCompressor)
  private
    FCompressionLevel: Integer;
    FNumThreads: Integer;
    FInitialized: Boolean;
    FStrm: Pointer;
    FOut: TZSTD_outBuffer;
    { Zstd blocks are 128KB + overhead; give it 256KB to minimize roundtrip
      times. See Destroy as to why this is important }
    FBuffer: array[0..$40000] of Byte;
    { Workaround for Zstd not resetting the frame progression until compress2 
      is called. Let's keep a good local copy }
    FProgress: TZSTD_FrameProgression;
    procedure EndCompress;
    procedure FlushBuffer;
    procedure InitCompress;
  protected
    procedure DoCompress(const Buffer; Count: Cardinal); override;
    procedure DoFinish; override;
  public
    constructor Create(AWriteProc: TCompressorWriteProc;
      AProgressProc: TCompressorProgressProc; CompressionLevel: Integer;
      ACompressorProps: TCompressorProps); override;
    destructor Destroy; override;
  end;

  TZstdDecompressor = class(TCustomDecompressor)
  private
    FInitialized: Boolean;
    FStrm: Pointer;
    FIn: TZSTD_inBuffer;
    FReachedEnd: Boolean;
    { Cache a few blocks more than the compressor }
    FBuffer: array[0..$FFFFF] of Byte;
  public
    constructor Create(AReadProc: TDecompressorReadProc); override;
    destructor Destroy; override;
    procedure DecompressInto(var Buffer; Count: Cardinal); override;
    procedure Reset; override;
  end;

implementation

const
  SZlibDataError = 'zstd: Compressed data is corrupted';
  SZlibInternalError = 'zstd: Internal error. Code %d';

  ZSTD_e_continue = 0;
  ZSTD_e_flush    = 1;
  ZSTD_e_end      = 2;

  ZSTD_c_nbWorkers        = 400;

  ZSTD_reset_session_only = 1;

var
  ZSTD_createCStream: function (): Pointer; stdcall;
  ZSTD_initCStream: function (zcs: Pointer; compressionLevel: Integer): UInt64; stdcall;
  ZSTD_CCtx_setParameter: function(cctx: Pointer; param: Cardinal; value: Integer): UInt64; stdcall;
  ZSTD_compressStream2: function(cctx: Pointer; var output: TZSTD_outBuffer; var input: TZSTD_inBuffer; endOp: Cardinal): UInt64; stdcall;
  ZSTD_freeCStream: function(zcs: Pointer): UInt64; stdcall;

  ZSTD_createDStream: function(): Pointer; stdcall;
  ZSTD_initDStream: function(zds: Pointer): UInt64; stdcall;
  ZSTD_decompressStream: function(zds: Pointer; var output: TZSTD_outBuffer; var input: TZSTD_inBuffer): UInt64; stdcall;
  ZSTD_freeDStream: function(zds: Pointer): UInt64; stdcall;

  ZSTD_isError: function(res: UInt64): Cardinal; stdcall;
  ZSTD_CCtx_reset: function(cctx: Pointer; reset: Cardinal): UInt64; stdcall;
  ZSTD_getFrameProgression: function(cctx: Pointer): TZSTD_frameProgression; stdcall;

function ZstdInitCompressFunctions(Module: HMODULE): Boolean;
begin
  ZSTD_createCStream := GetProcAddress(Module, 'ZSTD_createCStream');
  ZSTD_CCtx_setParameter := GetProcAddress(Module, 'ZSTD_CCtx_setParameter');
  ZSTD_initCStream := GetProcAddress(Module, 'ZSTD_initCStream');
  ZSTD_compressStream2 := GetProcAddress(Module, 'ZSTD_compressStream2');
  ZSTD_freeCStream := GetProcAddress(Module, 'ZSTD_freeCStream');
  ZSTD_isError := GetProcAddress(Module, 'ZSTD_isError');
  ZSTD_CCtx_reset := GetProcAddress(Module, 'ZSTD_CCtx_reset');
  ZSTD_getFrameProgression := GetProcAddress(Module, 'ZSTD_getFrameProgression');
  Result := Assigned(ZSTD_createCStream) and Assigned(ZSTD_CCtx_setParameter) and Assigned(ZSTD_compressStream2) and
    Assigned(ZSTD_freeCStream) and Assigned(ZSTD_isError) and Assigned(ZSTD_CCtx_reset) and Assigned(ZSTD_getFrameProgression);
  if not Result then begin
    ZSTD_createCStream := nil;
    ZSTD_initCStream := nil;
    ZSTD_CCtx_setParameter := nil;
    ZSTD_compressStream2 := nil;
    ZSTD_freeCStream := nil;
    ZSTD_isError := nil;
    ZSTD_CCtx_reset := nil;
    ZSTD_getFrameProgression := nil;
  end;
end;

function ZstdInitDecompressFunctions(Module: HMODULE): Boolean;
begin
  ZSTD_createDStream := GetProcAddress(Module, 'ZSTD_createDStream');
  ZSTD_initDStream := GetProcAddress(Module, 'ZSTD_initDStream');
  ZSTD_decompressStream := GetProcAddress(Module, 'ZSTD_decompressStream');
  ZSTD_freeDStream := GetProcAddress(Module, 'ZSTD_freeDStream');
  ZSTD_isError := GetProcAddress(Module, 'ZSTD_isError');
  Result := Assigned(ZSTD_createDStream) and Assigned(ZSTD_initDStream) and
    Assigned(ZSTD_decompressStream) and Assigned(ZSTD_freeDStream) and Assigned(ZSTD_isError);
  if not Result then begin
    ZSTD_createDStream := nil;
    ZSTD_initDStream := nil;
    ZSTD_decompressStream := nil;
    ZSTD_freeDStream := nil;
    ZSTD_isError := nil;
  end;
end;

function Check(const Code: UInt64): Boolean;
begin
  if ZSTD_isError(Code) <> 0 then
    raise ECompressInternalError.CreateFmt(SZlibInternalError, [Code]);
  Result := True;
end;

{ TZstdCompressor }

constructor TZstdCompressor.Create(AWriteProc: TCompressorWriteProc;
  AProgressProc: TCompressorProgressProc; CompressionLevel: Integer;
  ACompressorProps: TCompressorProps);
var
  GetActiveProcessorCountFunc: function(GroupNumber: WORD): WORD; stdcall;
begin
  inherited;
  FCompressionLevel := CompressionLevel;
  FNumThreads := 0;
  { Let's make Zstd use automatically all physical processors }
  GetActiveProcessorCountFunc := GetProcAddress(GetModuleHandle(kernel32),
    'GetActiveProcessorCount');
  if Assigned(GetActiveProcessorCountFunc) then begin
    const ActiveProcessorCount = GetActiveProcessorCountFunc(65535);
    if ActiveProcessorCount > 1 then
      FNumThreads := ActiveProcessorCount div 2;
  end;
  InitCompress;
end;

destructor TZstdCompressor.Destroy;
var
  Code: UInt64;
  FUnusedIn: TZSTD_inBuffer;
  OldCount: UInt64;
  Actual: UInt64;
begin
  { Unlike other destructors e.g. Zlib, this library is backed by a runtime
    thread pool. For this reason the buffer needs to be as small as
    possible (1 block == 64KB), so as to trigger the flush ASAP, then every
    write afterwards is promptly discarded from memory.
    This helps a lot with returning the control to the event loop, minimizing
    the time the UI spends unresponsive. }
  FillChar(FUnusedIn, SizeOf(FUnusedIn), 0);
  FOut.pos := 0;
  Code := 1;
  OldCount := FProgress.consumed;
  if FProgress.consumed <> FProgress.ingested then begin
    While Code <> 0 do begin
      Code := ZSTD_compressStream2(FStrm, FOut, FUnusedIn, ZSTD_e_flush);
      if Assigned(ProgressProc) then begin
        FProgress := ZSTD_getFrameProgression(FStrm);
        Actual := FProgress.consumed - OldCount;
        ProgressProc(Actual);
        OldCount := FProgress.consumed;
      end;
      FOut.pos := 0;
    end;
  end;
  EndCompress;
  Check(ZSTD_freeCStream(FStrm));
  inherited;
end;

procedure TZstdCompressor.InitCompress;
var
  Code: UInt64;
begin
  { Decoupling initialization from compression context creation allows
    reusing the context for further compression operations. Also, in
    multithreaded mode, it's pretty easy to OOM Delphi by using Zstd together
    with SolidCompression=no, as Zstd allocates a thread pool per context }
  if FStrm = nil then begin
    FStrm := ZSTD_createCStream();
    Check(ZSTD_initCStream(FStrm, FCompressionLevel));
    if FNumThreads > 1 then begin
      Code := ZSTD_CCtx_setParameter(FStrm, ZSTD_c_nbWorkers, FNumThreads);
      if ZSTD_isError(Code) <> 0 then
        Check(ZSTD_CCtx_setParameter(FStrm, ZSTD_c_nbWorkers, 1));
    end;
  end;
  if not FInitialized then begin
    FillChar(FProgress, SizeOf(FProgress), 0);
    FillChar(FOut, SizeOf(FOut), 0);
    FOut.src := @FBuffer;
    FOut.size := SizeOf(FBuffer);
    FOut.pos := 0;
    FInitialized := True;
  end;
end;

procedure TZstdCompressor.EndCompress;
begin
  if FInitialized then begin
    FInitialized := False;
    { Only reset the compression state; the rest is reusable }
    Check(ZSTD_CCtx_reset(FStrm, ZSTD_reset_session_only));
  end;
end;

procedure TZstdCompressor.FlushBuffer;
begin
  if FOut.pos > 0 then begin
    WriteProc(FBuffer, FOut.pos);
    FOut.pos := 0;
  end;
end;

procedure TZstdCompressor.DoCompress(const Buffer; Count: Cardinal);
var
  FIn: TZSTD_inBuffer;
  OldCount: UInt64;
  Actual: UInt64;
begin
  InitCompress;
  FIn.src := @Buffer;
  FIn.size := Count;
  FIn.pos := 0;
  OldCount := FProgress.consumed;
  while FIn.pos < Count do begin
    Check(ZSTD_compressStream2(FStrm, FOut, FIn, ZSTD_e_continue));
    if FOut.pos > 0 then
      FlushBuffer;
    { Maximize responsiveness by tying ProgressProc to the actual data
      ingested; especially helpful with compression levels >= 19 }
    if Assigned(ProgressProc) then begin
      FProgress := ZSTD_getFrameProgression(FStrm);
      Actual := FProgress.consumed - OldCount;
      ProgressProc(Actual);
      OldCount := FProgress.consumed;
    end;
  end;
end;

procedure TZstdCompressor.DoFinish;
var
  FIn: TZSTD_inBuffer;
  FReachedEnd: Boolean;
  Code: UInt64;
  OldCount: UInt64;
  Actual: UInt64;
begin
  InitCompress;
  FReachedEnd := False;
  FillChar(FIn, SizeOf(FIn), 0);
  OldCount := FProgress.consumed;
  while not FReachedEnd do begin
    Code := ZSTD_compressStream2(FStrm, FOut, FIn, ZSTD_e_end);
    if ZSTD_IsError(Code) <> 0 then
      raise ECompressDataError.CreateFmt(SZlibInternalError, [Code]);
    FlushBuffer;
    if Code = 0 then
      FReachedEnd := True;
    if Assigned(ProgressProc) then begin
      FProgress := ZSTD_getFrameProgression(FStrm);
      Actual := FProgress.consumed - OldCount;
      ProgressProc(Actual);
      OldCount := FProgress.consumed;
    end;
  end;
  EndCompress;
end;

{ TZstdDecompressor }

constructor TZstdDecompressor.Create(AReadProc: TDecompressorReadProc);
begin
  inherited Create(AReadProc);
  FStrm := ZSTD_createDStream();
  Check(ZSTD_initDStream(FStrm));
  FillChar(FIn, SizeOf(FIn), 0);
  FInitialized := True;
end;

destructor TZstdDecompressor.Destroy;
begin
  if FInitialized then begin
    Check(ZSTD_freeDStream(FStrm));
    FInitialized := False;
  end;
  inherited Destroy;
end;

procedure TZstdDecompressor.DecompressInto(var Buffer; Count: Cardinal);
var
  FOut: TZSTD_outBuffer;
  Code: UInt64;
begin
  FOut.src := @Buffer;
  FOut.size := Count;
  FOut.pos := 0;
  while FOut.pos < Count do begin
    if FReachedEnd then  { unexpected EOF }
      raise ECompressDataError.Create(SZlibDataError);
    if FIn.pos = FIn.size then begin
      FIn.src := @FBuffer;
      FIn.size := ReadProc(FBuffer, SizeOf(FBuffer));
      FIn.pos := 0;
    end;
    Code := ZSTD_decompressStream(FStrm, FOut, FIn);
    if ZSTD_isError(Code) <> 0 then begin
      raise ECompressDataError.CreateFmt(SZlibInternalError, [Code]);
    end else if Code = 0 then begin
      FReachedEnd := True;
    end;
  end;
end;

procedure TZstdDecompressor.Reset;
begin
  FillChar(FIn, SizeOf(FIn), 0);
  Check(ZSTD_initDStream(FStrm));
  FReachedEnd := False;
end;

end.
