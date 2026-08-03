unit Compression.Zstd;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Originally contributed by Amyspark <amy@amyspark.me>

  Declarations for Zstandard functions & structures

  https://raw.githack.com/facebook/zstd/release/doc/zstd_manual.html
}

interface

uses
  Windows, SysUtils, Compression.Base;

function ZstdInitCompressFunctions(Module: HMODULE): Boolean;
function ZstdInitDecompressFunctions(Module: HMODULE): Boolean;

type
  TZSTD_inBuffer = record
    src: Pointer;      { start of input buffer }
    size: NativeUInt;  { size of input buffer }
    pos: NativeUInt;   { position where reading stopped. Will be updated. Necessarily 0 <= pos <= size }
  end;

  TZSTD_outBuffer = record
    dst: Pointer;      { start of output buffer }
    size: NativeUInt;  { size of output buffer }
    pos: NativeUInt;   { position where writing stopped. Will be updated. Necessarily 0 <= pos <= size }
  end;

  TZSTD_frameProgression = record
    ingested: UInt64;  { nb input bytes read and buffered }
    consumed: UInt64;  { nb input bytes actually compressed }
    produced: UInt64;  { nb of compressed bytes generated and buffered }
    flushed: UInt64;   { nb of compressed bytes flushed : not provided; can be tracked from caller side }
    currentJobID: Cardinal;     { MT only : latest started job nb }
    nbActiveWorkers: Cardinal;  { MT only : nb of workers actively compressing at probe time }
  end;

  TZstdCompressor = class(TCustomCompressor)
  private
    FCompressionLevel: Integer;
    FNumThreads: Integer;
    FInitialized: Boolean;
    FStrm: Pointer;
    FOut: TZSTD_outBuffer;
    FBuffer: array[0..$FFFFF] of Byte;
    { Workaround for Zstd not resetting the frame progression until compress2
      is called. Let's keep a good local copy. }
    FProgress: TZSTD_frameProgression;
    function EndCompress: NativeUInt;
    procedure FlushBuffer;
    procedure InitCompress;
    procedure ReportProgress;
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
    FStrm: Pointer;
    FIn: TZSTD_inBuffer;
    FReachedEnd: Boolean;
    FBuffer: array[0..$FFFFF] of Byte;
  public
    constructor Create(AReadProc: TDecompressorReadProc); override;
    destructor Destroy; override;
    procedure DecompressInto(var Buffer; Count: Cardinal); override;
    procedure Reset; override;
  end;

implementation

const
  SZstdDataError = 'zstd: Compressed data is corrupted';
  SZstdInternalError = 'zstd: Internal error. Code %d';

  ZSTD_error_no_error = 0;
  ZSTD_error_memory_allocation = 64;

  ZSTD_e_continue = 0;
  ZSTD_e_end = 2;

  ZSTD_c_nbWorkers = 400;
  ZSTD_c_jobSize = 401;

  ZSTD_reset_session_only = 1;

var
  ZSTD_createCStream: function: Pointer; stdcall;
  ZSTD_initCStream: function(zcs: Pointer; compressionLevel: Integer): NativeUInt; stdcall;
  ZSTD_CCtx_setParameter: function(cctx: Pointer; param: Cardinal; value: Integer): NativeUInt; stdcall;
  ZSTD_compressStream2: function(cctx: Pointer; var output: TZSTD_outBuffer; var input: TZSTD_inBuffer; endOp: Cardinal): NativeUInt; stdcall;
  ZSTD_freeCStream: function(zcs: Pointer): NativeUInt; stdcall;

  ZSTD_createDStream: function: Pointer; stdcall;
  ZSTD_initDStream: function(zds: Pointer): NativeUInt; stdcall;
  ZSTD_decompressStream: function(zds: Pointer; var output: TZSTD_outBuffer; var input: TZSTD_inBuffer): NativeUInt; stdcall;
  ZSTD_freeDStream: function(zds: Pointer): NativeUInt; stdcall;

  ZSTD_getErrorCode: function(functionResult: NativeUInt): Cardinal; stdcall;
  ZSTD_CCtx_reset: function(cctx: Pointer; reset: Cardinal): NativeUInt; stdcall;
  ZSTD_getFrameProgression: function(cctx: Pointer): TZSTD_frameProgression; stdcall;

function ZstdInitCompressFunctions(Module: HMODULE): Boolean;
begin
  ZSTD_createCStream := GetProcAddress(Module, 'ZSTD_createCStream');
  ZSTD_CCtx_setParameter := GetProcAddress(Module, 'ZSTD_CCtx_setParameter');
  ZSTD_initCStream := GetProcAddress(Module, 'ZSTD_initCStream');
  ZSTD_compressStream2 := GetProcAddress(Module, 'ZSTD_compressStream2');
  ZSTD_freeCStream := GetProcAddress(Module, 'ZSTD_freeCStream');
  ZSTD_getErrorCode := GetProcAddress(Module, 'ZSTD_getErrorCode');
  ZSTD_CCtx_reset := GetProcAddress(Module, 'ZSTD_CCtx_reset');
  ZSTD_getFrameProgression := GetProcAddress(Module, 'ZSTD_getFrameProgression');
  Result :=
    Assigned(ZSTD_createCStream) and Assigned(ZSTD_CCtx_setParameter) and
    Assigned(ZSTD_initCStream) and Assigned(ZSTD_compressStream2) and
    Assigned(ZSTD_freeCStream) and Assigned(ZSTD_getErrorCode) and
    Assigned(ZSTD_CCtx_reset) and Assigned(ZSTD_getFrameProgression);
  if not Result then begin
    ZSTD_createCStream := nil;
    ZSTD_initCStream := nil;
    ZSTD_CCtx_setParameter := nil;
    ZSTD_compressStream2 := nil;
    ZSTD_freeCStream := nil;
    ZSTD_getErrorCode := nil;
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
  ZSTD_getErrorCode := GetProcAddress(Module, 'ZSTD_getErrorCode');
  Result :=
    Assigned(ZSTD_createDStream) and Assigned(ZSTD_initDStream) and
    Assigned(ZSTD_decompressStream) and Assigned(ZSTD_freeDStream) and
    Assigned(ZSTD_getErrorCode);
  if not Result then begin
    ZSTD_createDStream := nil;
    ZSTD_initDStream := nil;
    ZSTD_decompressStream := nil;
    ZSTD_freeDStream := nil;
    ZSTD_getErrorCode := nil;
  end;
end;

procedure Check(const Code: NativeUInt);
begin
  const ErrorCode = ZSTD_getErrorCode(Code);
  if ErrorCode <> ZSTD_error_no_error then begin
    if ErrorCode = ZSTD_error_memory_allocation then
      OutOfMemoryError;
    raise ECompressInternalError.CreateFmt(SZstdInternalError, [Code]);
  end;
end;

{ TZstdCompressor }

constructor TZstdCompressor.Create(AWriteProc: TCompressorWriteProc;
  AProgressProc: TCompressorProgressProc; CompressionLevel: Integer;
  ACompressorProps: TCompressorProps);
begin
  inherited;
  FCompressionLevel := CompressionLevel;
  FNumThreads := 1;
  if ACompressorProps is TThreadedCompressorProps then begin
    const Props = (ACompressorProps as TThreadedCompressorProps);
    if Props.NumBlockThreads <> 0 then
      FNumThreads := Props.NumBlockThreads;
  end;
  InitCompress;
end;

destructor TZstdCompressor.Destroy;
begin
  EndCompress;
  ZSTD_freeCStream(FStrm);
  inherited;
end;

procedure TZstdCompressor.InitCompress;
begin
  { Decoupling initialization from compression context creation allows
    reusing the context for further compression operations. Also, in
    multithreaded mode, it's pretty easy to OOM Delphi by using Zstd together
    with SolidCompression=no, as Zstd allocates a thread pool per context. }
  if FStrm = nil then begin
    FStrm := ZSTD_createCStream;
    if FStrm = nil then
      OutOfMemoryError;
    Check(ZSTD_initCStream(FStrm, FCompressionLevel));
    if FNumThreads > 1 then begin
      Check(ZSTD_CCtx_setParameter(FStrm, ZSTD_c_nbWorkers, FNumThreads));
      if FCompressionLevel > 19 then
        Check(ZSTD_CCtx_setParameter(FStrm, ZSTD_c_jobSize, 32 * 1024 * 1024)); { Ensures aborting is quick }
    end;
  end;
  if not FInitialized then begin
    FProgress := Default(TZSTD_frameProgression);
    FOut := Default(TZSTD_outBuffer);
    FOut.dst := @FBuffer;
    FOut.size := SizeOf(FBuffer);
    FInitialized := True;
  end;
end;

function TZstdCompressor.EndCompress: NativeUInt;
begin
  if FInitialized then begin
    FInitialized := False;
    { Only reset the compression state; the rest is reusable }
    Result := ZSTD_CCtx_reset(FStrm, ZSTD_reset_session_only);
  end else
    Result := ZSTD_error_no_error;
end;

procedure TZstdCompressor.FlushBuffer;
begin
  if FOut.pos > 0 then begin
    WriteProc(FBuffer, Cardinal(FOut.pos));
    FOut.pos := 0;
  end;
end;

procedure TZstdCompressor.ReportProgress;
begin
  { Maximize responsiveness by tying ProgressProc to the actual data
    consumed; especially helpful with compression levels >= 19 }
  if Assigned(ProgressProc) then begin
    const OldConsumed = FProgress.consumed;
    FProgress := ZSTD_getFrameProgression(FStrm);
    ProgressProc(Int64(FProgress.consumed - OldConsumed));
  end;
end;

procedure TZstdCompressor.DoCompress(const Buffer; Count: Cardinal);
begin
  InitCompress;
  var LIn := Default(TZSTD_inBuffer);
  LIn.src := @Buffer;
  LIn.size := Count;
  while LIn.pos < Count do begin
    Check(ZSTD_compressStream2(FStrm, FOut, LIn, ZSTD_e_continue));
    if FOut.pos = FOut.size then
      FlushBuffer;
    ReportProgress;
  end;
end;

procedure TZstdCompressor.DoFinish;
begin
  InitCompress;
  var LIn := Default(TZSTD_inBuffer);
  var ReachedEnd := False;
  while not ReachedEnd do begin
    const Code = ZSTD_compressStream2(FStrm, FOut, LIn, ZSTD_e_end);
    Check(Code);
    FlushBuffer;
    if Code = 0 then
      ReachedEnd := True;
    ReportProgress;
  end;
  Check(EndCompress);
end;

{ TZstdDecompressor }

constructor TZstdDecompressor.Create(AReadProc: TDecompressorReadProc);
begin
  inherited Create(AReadProc);
  FStrm := ZSTD_createDStream;
  if FStrm = nil then
    OutOfMemoryError;
  Reset;
end;

destructor TZstdDecompressor.Destroy;
begin
  if FStrm <> nil then
    ZSTD_freeDStream(FStrm);
  inherited;
end;

procedure TZstdDecompressor.DecompressInto(var Buffer; Count: Cardinal);
begin
  var LOut := Default(TZSTD_outBuffer);
  LOut.dst := @Buffer;
  LOut.size := Count;
  while LOut.pos < Count do begin
    if FReachedEnd then  { unexpected EOF }
      raise ECompressDataError.Create(SZstdDataError);
    if FIn.pos = FIn.size then begin
      FIn.src := @FBuffer;
      FIn.size := ReadProc(FBuffer, SizeOf(FBuffer));
      FIn.pos := 0;
    end;
    const OldInPos = FIn.pos;
    const OldOutPos = LOut.pos;
    const Code = ZSTD_decompressStream(FStrm, LOut, FIn);
    const ErrorCode = ZSTD_getErrorCode(Code);
    if ErrorCode <> ZSTD_error_no_error then begin
      if ErrorCode = ZSTD_error_memory_allocation then
        OutOfMemoryError;
      raise ECompressDataError.Create(SZstdDataError);
    end else if (FIn.pos = OldInPos) and (LOut.pos = OldOutPos) then begin
      { Sanity check; no data consumed or decompressed at all }
      raise ECompressDataError.Create(SZstdDataError);
    end else if Code = 0 then
      FReachedEnd := True;
  end;
end;

procedure TZstdDecompressor.Reset;
begin
  FIn := Default(TZSTD_inBuffer);
  Check(ZSTD_initDStream(FStrm));
  FReachedEnd := False;
end;

end.
