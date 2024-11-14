unit Compression.LZMACompressor;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Interface to the islzma LZMA/LZMA2 compression DLL and EXEs in
  Compression.LZMACompressor\islzma, used by ISCmplr.
}

interface

uses
  Windows, SysUtils,
  Compression.Base;

function LZMAInitCompressFunctions(Module: HMODULE): Boolean;
function LZMAGetLevel(const Value: String; var Level: Integer): Boolean;

const
  clLZMAFast = 1;
  clLZMANormal = 2;
  clLZMAMax = 3;
  clLZMAUltra = 4;
  clLZMAUltra64 = 5;

type
  { Internally-used types }
  TLZMASRes = type Integer;
  TLZMACompressorCustomWorker = class;

  TLZMACompressorProps = class(TCompressorProps)
  public
    Algorithm: Integer;
    BlockSize: Integer;
    BTMode: Integer;
    DictionarySize: Cardinal;
    NumBlockThreads: Integer;
    NumFastBytes: Integer;
    NumThreads: Integer;
    WorkerProcessFilename: String;
    constructor Create;
  end;

  { Internally-used records }
  TLZMAEncoderProps = record
    Algorithm: Integer;
    BlockSize: Integer;
    BTMode: Integer;
    NumHashBytes: Integer;
    DictionarySize: Cardinal;
    NumBlockThreads: Integer;
    NumFastBytes: Integer;
    NumThreads: Integer;
  end;
  TLZMACompressorRingBuffer = record
    Count: Longint;         { updated by reader and writer using InterlockedExchangeAdd only }
    WriterOffset: Longint;  { accessed only by writer thread }
    ReaderOffset: Longint;  { accessed only by reader thread }
    Buf: array[0..$FFFFF] of Byte;
  end;
  PLZMACompressorSharedEvents = ^TLZMACompressorSharedEvents;
  TLZMACompressorSharedEvents = record
    TerminateWorkerEvent: THandle;
    StartEncodeEvent: THandle;
    EndWaitOnInputEvent: THandle;
    EndWaitOnOutputEvent: THandle;
    WorkerWaitingOnInputEvent: THandle;
    WorkerWaitingOnOutputEvent: THandle;
    WorkerEncodeFinishedEvent: THandle;
  end;
  PLZMACompressorSharedData = ^TLZMACompressorSharedData;
  TLZMACompressorSharedData = record
    ProgressBytes: Int64;
    NoMoreInput: BOOL;
    EncodeResult: TLZMASRes;
    InputBuffer: TLZMACompressorRingBuffer;
    OutputBuffer: TLZMACompressorRingBuffer;
  end;
  PLZMACompressorProcessData = ^TLZMACompressorProcessData;
  TLZMACompressorProcessData = record
    StructSize: LongWord;
    ParentProcess: THandle;
    LZMA2: BOOL;
    EncoderProps: TLZMAEncoderProps;
    Events: TLZMACompressorSharedEvents;
    SharedDataStructSize: LongWord;
    SharedDataMapping: THandle;
  end;

  TLZMACompressor = class(TCustomCompressor)
  private
    FUseLZMA2: Boolean;
    FEvents: TLZMACompressorSharedEvents;
    FShared: PLZMACompressorSharedData;
    FWorker: TLZMACompressorCustomWorker;
    FEncodeStarted: Boolean;
    FEncodeFinished: Boolean;
    FLastInputWriteCount: LongWord;
    FLastProgressBytes: Int64;
    FProgressTimer: THandle;
    FProgressTimerSignaled: Boolean;
    procedure FlushOutputBuffer(const OnlyOptimalSize: Boolean);
    procedure InitializeProps(const CompressionLevel: Integer;
      const ACompressorProps: TCompressorProps);
    class function IsObjectSignaled(const AObject: THandle): Boolean;
    class procedure SatisfyWorkerWait(const AWorkerEvent, AMainEvent: THandle);
    procedure SatisfyWorkerWaitOnInput;
    procedure SatisfyWorkerWaitOnOutput;
    procedure StartEncode;
    procedure UpdateProgress;
    procedure WaitForWorkerEvent;
  protected
    procedure DoCompress(const Buffer; Count: Longint); override;
    procedure DoFinish; override;
  public
    constructor Create(AWriteProc: TCompressorWriteProc;
      AProgressProc: TCompressorProgressProc; CompressionLevel: Integer;
      ACompressorProps: TCompressorProps); override;
    destructor Destroy; override;
  end;

  TLZMA2Compressor = class(TLZMACompressor)
  public
    constructor Create(AWriteProc: TCompressorWriteProc;
      AProgressProc: TCompressorProgressProc; CompressionLevel: Integer;
      ACompressorProps: TCompressorProps); override;
  end;

  { Internally-used classes }
  TLZMACompressorCustomWorker = class
  protected
    FEvents: PLZMACompressorSharedEvents;
    FShared: PLZMACompressorSharedData;
  public
    constructor Create(const AEvents: PLZMACompressorSharedEvents); virtual;
    function GetExitHandle: THandle; virtual; abstract;
    procedure SetProps(const LZMA2: Boolean; const EncProps: TLZMAEncoderProps);
      virtual; abstract;
    procedure UnexpectedTerminationError; virtual; abstract;
  end;

implementation

const
  ISLZMA_EXE_VERSION = 102;

type
  TLZMACompressorHandle = type Pointer;

  TLZMAWorkerThread = class(TLZMACompressorCustomWorker)
  private
    FThread: THandle;
    FLZMAHandle: TLZMACompressorHandle;
    FReadLock, FWriteLock, FProgressLock: Integer;
    function CheckTerminateWorkerEvent: HRESULT;
    function FillBuffer(const AWrite: Boolean; const Data: Pointer;
      Size: Cardinal; var ProcessedSize: Cardinal): HRESULT;
    function ProgressMade(const TotalBytesProcessed: UInt64): HRESULT;
    function Read(var Data; Size: Cardinal; var ProcessedSize: Cardinal): HRESULT;
    function WakeMainAndWaitUntil(const AWakeEvent, AWaitEvent: THandle): HRESULT;
    procedure WorkerThreadProc;
    function Write(const Data; Size: Cardinal; var ProcessedSize: Cardinal): HRESULT;
  public
    constructor Create(const AEvents: PLZMACompressorSharedEvents); override;
    destructor Destroy; override;
    function GetExitHandle: THandle; override;
    procedure SetProps(const LZMA2: Boolean; const EncProps: TLZMAEncoderProps);
      override;
    procedure UnexpectedTerminationError; override;
  end;

  TLZMAWorkerProcess = class(TLZMACompressorCustomWorker)
  private
    FProcess: THandle;
    FSharedMapping: THandle;
    FExeFilename: String;
  public
    constructor Create(const AEvents: PLZMACompressorSharedEvents); override;
    destructor Destroy; override;
    function GetExitHandle: THandle; override;
    procedure SetProps(const LZMA2: Boolean; const EncProps: TLZMAEncoderProps);
      override;
    procedure UnexpectedTerminationError; override;
    property ExeFilename: String read FExeFilename write FExeFilename;
  end;

  PLZMASeqInStream = ^TLZMASeqInStream;
  TLZMASeqInStream = record
    Read: function(p: PLZMASeqInStream; var buf; var size: Cardinal): TLZMASRes; stdcall;
    Instance: TLZMAWorkerThread;
  end;
  PLZMASeqOutStream = ^TLZMASeqOutStream;
  TLZMASeqOutStream = record
    Write: function(p: PLZMASeqOutStream; const buf; size: Cardinal): Cardinal; stdcall;
    Instance: TLZMAWorkerThread;
  end;
  PLZMACompressProgress = ^TLZMACompressProgress;
  TLZMACompressProgress = record
    Progress: function(p: PLZMACompressProgress; inSize, outSize: UInt64): TLZMASRes; stdcall;
    Instance: TLZMAWorkerThread;
  end;

var
  LZMADLLInitialized: Boolean;

  LZMA_Init: function(LZMA2: BOOL; var handle: TLZMACompressorHandle): TLZMASRes;
    stdcall;
  LZMA_SetProps: function(handle: TLZMACompressorHandle;
    const encProps: TLZMAEncoderProps; encPropsSize: Cardinal): TLZMASRes; stdcall;
  LZMA_Encode: function(handle: TLZMACompressorHandle;
    const inStream: TLZMASeqInStream; const outStream: TLZMASeqOutStream;
    const progress: TLZMACompressProgress): TLZMASRes; stdcall;
  LZMA_End: function(handle: TLZMACompressorHandle): TLZMASRes; stdcall;

const
  { SRes (TLZMASRes) }
  SZ_OK = 0;
  SZ_ERROR_MEM = 2;
  SZ_ERROR_READ = 8;
  SZ_ERROR_PROGRESS = 10;
  SZ_ERROR_FAIL = 11;

function LZMAInitCompressFunctions(Module: HMODULE): Boolean;
begin
  LZMADLLInitialized := False;
  LZMA_Init := GetProcAddress(Module, 'LZMA_Init3');
  LZMA_SetProps := GetProcAddress(Module, 'LZMA_SetProps3');
  LZMA_Encode := GetProcAddress(Module, 'LZMA_Encode3');
  LZMA_End := GetProcAddress(Module, 'LZMA_End3');
  Result := Assigned(LZMA_Init) and Assigned(LZMA_SetProps) and
    Assigned(LZMA_Encode) and Assigned(LZMA_End);
  if Result then
    LZMADLLInitialized := True
  else begin
    LZMA_Init := nil;
    LZMA_SetProps := nil;
    LZMA_Encode := nil;
    LZMA_End := nil;
  end;
end;

procedure LZMAInternalError(const Msg: String);
begin
  raise ECompressInternalError.Create('lzma: ' + Msg);
end;

procedure LZMAInternalErrorFmt(const Msg: String; const Args: array of const);
begin
  LZMAInternalError(Format(Msg, Args));
end;

procedure LZMAWin32Error(const FunctionName: String);
var
  LastError: DWORD;
begin
  LastError := GetLastError;
  LZMAInternalErrorFmt('%s failed (%u)', [FunctionName, LastError]);
end;

function LZMAGetLevel(const Value: String; var Level: Integer): Boolean;
begin
  Result := True;
  if CompareText(Value, 'fast') = 0 then
    Level := clLZMAFast
  else if CompareText(Value, 'normal') = 0 then
    Level := clLZMANormal
  else if CompareText(Value, 'max') = 0 then
    Level := clLZMAMax
  else if CompareText(Value, 'ultra') = 0 then
    Level := clLZMAUltra
  else if CompareText(Value, 'ultra64') = 0 then
    Level := clLZMAUltra64
  else
    Result := False;
end;

function LZMACreateEvent(const ManualReset: BOOL): THandle;
begin
  Result := CreateEvent(nil, ManualReset, False, nil);
  if Result = 0 then
    LZMAWin32Error('CreateEvent');
end;

function LZMASeqInStreamReadWrapper(p: PLZMASeqInStream; var buf;
  var size: Cardinal): TLZMASRes; stdcall;
begin
  if p.Instance.Read(buf, size, size) = S_OK then
    Result := SZ_OK
  else
    Result := SZ_ERROR_READ;
end;

function LZMASeqOutStreamWriteWrapper(p: PLZMASeqOutStream; const buf;
  size: Cardinal): Cardinal; stdcall;
begin
  if p.Instance.Write(buf, size, Result) <> S_OK then
    Result := 0;
end;

function LZMACompressProgressProgressWrapper(p: PLZMACompressProgress;
  inSize, outSize: UInt64): TLZMASRes; stdcall;
begin
  if p.Instance.ProgressMade(inSize) = S_OK then
    Result := SZ_OK
  else
    Result := SZ_ERROR_PROGRESS;
end;

{ TLZMACompressorRingBuffer:
  Designed to support concurrent, lock-free access by two threads in a
  pipe-like fashion: one thread may read from the buffer (FIFO) at the same
  time another thread is writing to it. Two threads, however, may NOT both
  read, or both write simultaneously. }

procedure RingBufferReset(var Ring: TLZMACompressorRingBuffer);
begin
  Ring.Count := 0;
  Ring.WriterOffset := 0;
  Ring.ReaderOffset := 0;
end;

function RingBufferInternalWriteOrRead(var Ring: TLZMACompressorRingBuffer;
  const AWrite: Boolean; var Offset: Longint; const Data: Pointer;
  Size: Longint): Longint;
var
  P: ^Byte;
  Bytes: Longint;
begin
  Result := 0;
  P := Data;
  while Size > 0 do begin
    if AWrite then
      Bytes := SizeOf(Ring.Buf) - Ring.Count
    else
      Bytes := Ring.Count;
    if Bytes = 0 then
      { Buffer is full (write) or empty (read) }
      Break;
    if Bytes > Size then
      Bytes := Size;
    if Bytes > SizeOf(Ring.Buf) - Offset then
      Bytes := SizeOf(Ring.Buf) - Offset;

    { On a weakly-ordered CPU, the read of Count above must happen before
      Buf content is read below (otherwise the content could be stale) }
    MemoryBarrier;

    if AWrite then begin
      Move(P^, Ring.Buf[Offset], Bytes);
      InterlockedExchangeAdd(Ring.Count, Bytes);  { full barrier }
    end
    else begin
      Move(Ring.Buf[Offset], P^, Bytes);
      InterlockedExchangeAdd(Ring.Count, -Bytes);  { full barrier }
    end;
    if Offset + Bytes = SizeOf(Ring.Buf) then
      Offset := 0
    else
      Inc(Offset, Bytes);

    Dec(Size, Bytes);
    Inc(Result, Bytes);
    Inc(P, Bytes);
  end;
end;

function RingBufferRead(var Ring: TLZMACompressorRingBuffer; var Buf;
  const Size: Longint): Longint;
begin
  Result := RingBufferInternalWriteOrRead(Ring, False, Ring.ReaderOffset,
    @Buf, Size);
end;

function RingBufferWrite(var Ring: TLZMACompressorRingBuffer; const Buf;
  const Size: Longint): Longint;
begin
  Result := RingBufferInternalWriteOrRead(Ring, True, Ring.WriterOffset,
    @Buf, Size);
end;

function RingBufferReadToCallback(var Ring: TLZMACompressorRingBuffer;
  const AWriteProc: TCompressorWriteProc; Size: Longint): Longint;
var
  Bytes: Longint;
begin
  Result := 0;
  while Size > 0 do begin
    Bytes := Ring.Count;
    if Bytes = 0 then
      Break;
    if Bytes > Size then
      Bytes := Size;
    if Bytes > SizeOf(Ring.Buf) - Ring.ReaderOffset then
      Bytes := SizeOf(Ring.Buf) - Ring.ReaderOffset;

    { On a weakly-ordered CPU, the read of Count above must happen before
      Buf content is read below (otherwise the content could be stale) }
    MemoryBarrier;

    AWriteProc(Ring.Buf[Ring.ReaderOffset], Bytes);
    InterlockedExchangeAdd(Ring.Count, -Bytes);  { full barrier }
    if Ring.ReaderOffset + Bytes = SizeOf(Ring.Buf) then
      Ring.ReaderOffset := 0
    else
      Inc(Ring.ReaderOffset, Bytes);

    Dec(Size, Bytes);
    Inc(Result, Bytes);
  end;
end;

{ TLZMACompressorProps }

constructor TLZMACompressorProps.Create;
begin
  inherited;
  Algorithm := -1;
  BTMode := -1;
end;

{ TLZMACompressorCustomWorker }

constructor TLZMACompressorCustomWorker.Create(const AEvents: PLZMACompressorSharedEvents);
begin
  inherited Create;
  FEvents := AEvents;
end;

{ TLZMAWorkerThread }

function WorkerThreadFunc(Parameter: Pointer): Integer;
begin
  try
    TLZMAWorkerThread(Parameter).WorkerThreadProc;
  except
  end;
  Result := 0;
end;

constructor TLZMAWorkerThread.Create(const AEvents: PLZMACompressorSharedEvents);
begin
  inherited;
  FShared := VirtualAlloc(nil, SizeOf(FShared^), MEM_COMMIT, PAGE_READWRITE);
  if FShared = nil then
    OutOfMemoryError;
end;

destructor TLZMAWorkerThread.Destroy;
begin
  if FThread <> 0 then begin
    SetEvent(FEvents.TerminateWorkerEvent);
    WaitForSingleObject(FThread, INFINITE);
    CloseHandle(FThread);
    FThread := 0;
  end;
  if Assigned(FLZMAHandle) then
    LZMA_End(FLZMAHandle);
  if Assigned(FShared) then
    VirtualFree(FShared, 0, MEM_RELEASE);
  inherited;
end;

function TLZMAWorkerThread.GetExitHandle: THandle;
begin
  Result := FThread;
end;

procedure TLZMAWorkerThread.SetProps(const LZMA2: Boolean;
  const EncProps: TLZMAEncoderProps);
var
  Res: TLZMASRes;
  ThreadID: DWORD;
begin
  Res := LZMA_Init(LZMA2, FLZMAHandle);
  if Res = SZ_ERROR_MEM then
    OutOfMemoryError;
  if Res <> SZ_OK then
    LZMAInternalErrorFmt('LZMA_Init failed with code %d', [Res]);

  if LZMA_SetProps(FLZMAHandle, EncProps, SizeOf(EncProps)) <> SZ_OK then
    LZMAInternalError('LZMA_SetProps failed');

  FThread := BeginThread(nil, 0, WorkerThreadFunc, Self, 0, ThreadID);
  if FThread = 0 then
    LZMAWin32Error('BeginThread');
end;

procedure TLZMAWorkerThread.UnexpectedTerminationError;
begin
  LZMAInternalError('Worker thread terminated unexpectedly');
end;

procedure TLZMAWorkerThread.WorkerThreadProc;
{ Worker thread main procedure }
var
  InStream: TLZMASeqInStream;
  OutStream: TLZMASeqOutStream;
  CompressProgress: TLZMACompressProgress;
  H: array[0..1] of THandle;
begin
  InStream.Read := LZMASeqInStreamReadWrapper;
  InStream.Instance := Self;
  OutStream.Write := LZMASeqOutStreamWriteWrapper;
  OutStream.Instance := Self;
  CompressProgress.Progress := LZMACompressProgressProgressWrapper;
  CompressProgress.Instance := Self;

  H[0] := FEvents.TerminateWorkerEvent;
  H[1] := FEvents.StartEncodeEvent;
  while WaitForMultipleObjects(2, @H, False, INFINITE) = WAIT_OBJECT_0 + 1 do begin
    FShared.EncodeResult := LZMA_Encode(FLZMAHandle, InStream, OutStream,
      CompressProgress);
    if not SetEvent(FEvents.WorkerEncodeFinishedEvent) then
      Break;
  end;
end;

function TLZMAWorkerThread.WakeMainAndWaitUntil(const AWakeEvent,
  AWaitEvent: THandle): HRESULT;
var
  H: array[0..1] of THandle;
begin
  if not SetEvent(AWakeEvent) then begin
    SetEvent(FEvents.TerminateWorkerEvent);
    Result := E_FAIL;
    Exit;
  end;
  H[0] := FEvents.TerminateWorkerEvent;
  H[1] := AWaitEvent;
  case WaitForMultipleObjects(2, @H, False, INFINITE) of
    WAIT_OBJECT_0 + 0: Result := E_ABORT;
    WAIT_OBJECT_0 + 1: Result := S_OK;
  else
    SetEvent(FEvents.TerminateWorkerEvent);
    Result := E_FAIL;
  end;
end;

function TLZMAWorkerThread.CheckTerminateWorkerEvent: HRESULT;
begin
  case WaitForSingleObject(FEvents.TerminateWorkerEvent, 0) of
    WAIT_OBJECT_0 + 0: Result := E_ABORT;
    WAIT_TIMEOUT: Result := S_OK;
  else
    SetEvent(FEvents.TerminateWorkerEvent);
    Result := E_FAIL;
  end;
end;

function TLZMAWorkerThread.FillBuffer(const AWrite: Boolean;
  const Data: Pointer; Size: Cardinal; var ProcessedSize: Cardinal): HRESULT;
{ Called from worker thread (or a thread spawned by the worker thread) }
var
  P: ^Byte;
  Bytes: Longint;
begin
  ProcessedSize := 0;
  P := Data;
  while Size <> 0 do begin
    var LimitedSize: LongInt;
    if Size > MaxLong then
      LimitedSize := MaxLong
    else
      LimitedSize := Size;
    if AWrite then
      Bytes := RingBufferWrite(FShared.OutputBuffer, P^, LimitedSize)
    else begin
      if FShared.NoMoreInput then begin
        { If NoMoreInput=True and *then* we see that the input buffer is
          empty (ordering matters!), we know that all input has been
          processed and that the input buffer will stay empty }
        MemoryBarrier;
        if FShared.InputBuffer.Count = 0 then
          Break;
      end;
      Bytes := RingBufferRead(FShared.InputBuffer, P^, LimitedSize);
    end;
    if Bytes = 0 then begin
      if AWrite then begin
        { Output buffer full; wait for the main thread to flush it }
        Result := WakeMainAndWaitUntil(FEvents.WorkerWaitingOnOutputEvent,
          FEvents.EndWaitOnOutputEvent);
        if Result <> S_OK then
          Exit;
      end
      else begin
        { Input buffer empty; wait for the main thread to fill it }
        Result := WakeMainAndWaitUntil(FEvents.WorkerWaitingOnInputEvent,
          FEvents.EndWaitOnInputEvent);
        if Result <> S_OK then
          Exit;
      end;
    end
    else begin
      Inc(ProcessedSize, Bytes);
      Dec(Size, Bytes);
      Inc(P, Bytes);
    end;
  end;
  Result := S_OK;
end;

function TLZMAWorkerThread.Read(var Data; Size: Cardinal;
  var ProcessedSize: Cardinal): HRESULT;
{ Called from worker thread (or a thread spawned by the worker thread) }
begin
  { Sanity check: Make sure we're the only thread inside Read }
  if InterlockedExchange(FReadLock, 1) <> 0 then begin
    Result := E_FAIL;
    Exit;
  end;
  Result := FillBuffer(False, @Data, Size, ProcessedSize);
  InterlockedExchange(FReadLock, 0);
end;

function TLZMAWorkerThread.Write(const Data; Size: Cardinal;
  var ProcessedSize: Cardinal): HRESULT;
{ Called from worker thread (or a thread spawned by the worker thread) }
begin
  { Sanity check: Make sure we're the only thread inside Write }
  if InterlockedExchange(FWriteLock, 1) <> 0 then begin
    Result := E_FAIL;
    Exit;
  end;
  Result := FillBuffer(True, @Data, Size, ProcessedSize);
  InterlockedExchange(FWriteLock, 0);
end;

function TLZMAWorkerThread.ProgressMade(const TotalBytesProcessed: UInt64): HRESULT;
{ Called from worker thread (or a thread spawned by the worker thread) }
begin
  { Sanity check: Make sure we're the only thread inside Progress }
  if InterlockedExchange(FProgressLock, 1) <> 0 then begin
    Result := E_FAIL;
    Exit;
  end;
  { An Interlocked function is used to ensure the 64-bit value is written
    atomically (not with two separate 32-bit writes).
    TLZMACompressor will ignore negative values. LZMA SDK's 7zTypes.h says
    "-1 for size means unknown value", though I don't see any place
    where LzmaEnc actually does call Progress with inSize = -1. }
  InterlockedExchange64(FShared.ProgressBytes, Int64(TotalBytesProcessed));
  Result := CheckTerminateWorkerEvent;
  InterlockedExchange(FProgressLock, 0);
end;

{ TLZMAWorkerProcess }

constructor TLZMAWorkerProcess.Create(const AEvents: PLZMACompressorSharedEvents);
begin
  inherited;
  FSharedMapping := CreateFileMapping(INVALID_HANDLE_VALUE, nil,
    PAGE_READWRITE, 0, SizeOf(FShared^), nil);
  if FSharedMapping = 0 then
    LZMAWin32Error('CreateFileMapping');
  FShared := MapViewOfFile(FSharedMapping, FILE_MAP_WRITE, 0, 0,
    SizeOf(FShared^));
  if FShared = nil then
    LZMAWin32Error('MapViewOfFile');
end;

destructor TLZMAWorkerProcess.Destroy;
begin
  if FProcess <> 0 then begin
    SetEvent(FEvents.TerminateWorkerEvent);
    WaitForSingleObject(FProcess, INFINITE);
    CloseHandle(FProcess);
    FProcess := 0;
  end;
  if Assigned(FShared) then
    UnmapViewOfFile(FShared);
  if FSharedMapping <> 0 then
    CloseHandle(FSharedMapping);
  inherited;
end;

function TLZMAWorkerProcess.GetExitHandle: THandle;
begin
  Result := FProcess;
end;

procedure TLZMAWorkerProcess.SetProps(const LZMA2: Boolean;
  const EncProps: TLZMAEncoderProps);

  function GetSystemDir: String;
  var
    Buf: array[0..MAX_PATH-1] of Char;
  begin
    GetSystemDirectory(Buf, SizeOf(Buf) div SizeOf(Buf[0]));
    Result := Buf;
  end;

  procedure DupeHandle(const SourceHandle: THandle; const DestProcess: THandle;
    var DestHandle: THandle; const DesiredAccess: DWORD);
  begin
    if not DuplicateHandle(GetCurrentProcess, SourceHandle, DestProcess,
       @DestHandle, DesiredAccess, False, 0) then
      LZMAWin32Error('DuplicateHandle');
  end;

  procedure DupeEventHandles(const Src: TLZMACompressorSharedEvents;
    const Process: THandle; var Dest: TLZMACompressorSharedEvents);

    procedure DupeEvent(const SourceHandle: THandle; var DestHandle: THandle);
    begin
      DupeHandle(SourceHandle, Process, DestHandle, SYNCHRONIZE or
        EVENT_MODIFY_STATE);
    end;

  begin
    DupeEvent(Src.TerminateWorkerEvent, Dest.TerminateWorkerEvent);
    DupeEvent(Src.StartEncodeEvent, Dest.StartEncodeEvent);
    DupeEvent(Src.EndWaitOnInputEvent, Dest.EndWaitOnInputEvent);
    DupeEvent(Src.EndWaitOnOutputEvent, Dest.EndWaitOnOutputEvent);
    DupeEvent(Src.WorkerWaitingOnInputEvent, Dest.WorkerWaitingOnInputEvent);
    DupeEvent(Src.WorkerWaitingOnOutputEvent, Dest.WorkerWaitingOnOutputEvent);
    DupeEvent(Src.WorkerEncodeFinishedEvent, Dest.WorkerEncodeFinishedEvent);
  end;

const
  InheritableSecurity: TSecurityAttributes = (
    nLength: SizeOf(InheritableSecurity); lpSecurityDescriptor: nil;
    bInheritHandle: True);
var
  ProcessDataMapping: THandle;
  ProcessData: PLZMACompressorProcessData;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  ProcessData := nil;
  ProcessDataMapping := CreateFileMapping(INVALID_HANDLE_VALUE,
    @InheritableSecurity, PAGE_READWRITE, 0, SizeOf(ProcessData^), nil);
  if ProcessDataMapping = 0 then
    LZMAWin32Error('CreateFileMapping');
  try
    ProcessData := MapViewOfFile(ProcessDataMapping, FILE_MAP_WRITE, 0, 0,
      SizeOf(ProcessData^));
    if ProcessData = nil then
      LZMAWin32Error('MapViewOfFile');

    ProcessData.StructSize := SizeOf(ProcessData^);
    ProcessData.LZMA2 := LZMA2;
    ProcessData.EncoderProps := EncProps;
    ProcessData.SharedDataStructSize := SizeOf(FShared^);

    FillChar(StartupInfo, SizeOf(StartupInfo), 0);
    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.dwFlags := STARTF_FORCEOFFFEEDBACK;
    if not CreateProcess(PChar(FExeFilename),
       PChar(Format('islzma_exe %d 0x%x', [ISLZMA_EXE_VERSION, ProcessDataMapping])),
       nil, nil, True, CREATE_DEFAULT_ERROR_MODE or CREATE_SUSPENDED, nil,
       PChar(GetSystemDir), StartupInfo, ProcessInfo) then
      LZMAWin32Error('CreateProcess');

    try
      { We duplicate the handles instead of using inheritable handles so that
        if something outside this unit calls CreateProcess() while compression
        is in progress, our handles won't be inadvertently passed on to that
        other process. }
      DupeHandle(GetCurrentProcess, ProcessInfo.hProcess,
        ProcessData.ParentProcess, SYNCHRONIZE);
      DupeHandle(FSharedMapping, ProcessInfo.hProcess,
        ProcessData.SharedDataMapping, FILE_MAP_WRITE);
      DupeEventHandles(FEvents^, ProcessInfo.hProcess, ProcessData.Events);
      if ResumeThread(ProcessInfo.hThread) = DWORD(-1) then
        LZMAWin32Error('ResumeThread');
    except
      CloseHandle(ProcessInfo.hThread);
      TerminateProcess(ProcessInfo.hProcess, 1);
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      CloseHandle(ProcessInfo.hProcess);
      raise;
    end;
    FProcess := ProcessInfo.hProcess;
    CloseHandle(ProcessInfo.hThread);
  finally
    if Assigned(ProcessData) then
      UnmapViewOfFile(ProcessData);
    CloseHandle(ProcessDataMapping);
  end;
end;

procedure TLZMAWorkerProcess.UnexpectedTerminationError;
var
  ProcessExitCode: DWORD;
begin
  if GetExitCodeProcess(FProcess, ProcessExitCode) then
    LZMAInternalErrorFmt('Worker process terminated unexpectedly (0x%x)',
      [ProcessExitCode])
  else
    LZMAInternalError('Worker process terminated unexpectedly ' +
      '(failed to get exit code)');
end;

{ TLZMACompressor }

constructor TLZMACompressor.Create(AWriteProc: TCompressorWriteProc;
  AProgressProc: TCompressorProgressProc; CompressionLevel: Integer;
  ACompressorProps: TCompressorProps);
begin
  inherited;
  FEvents.TerminateWorkerEvent := LZMACreateEvent(True);       { manual reset }
  FEvents.StartEncodeEvent := LZMACreateEvent(False);          { auto reset }
  FEvents.EndWaitOnInputEvent := LZMACreateEvent(False);       { auto reset }
  FEvents.EndWaitOnOutputEvent := LZMACreateEvent(False);      { auto reset }
  FEvents.WorkerWaitingOnInputEvent := LZMACreateEvent(True);  { manual reset }
  FEvents.WorkerWaitingOnOutputEvent := LZMACreateEvent(True); { manual reset }
  FEvents.WorkerEncodeFinishedEvent := LZMACreateEvent(True);  { manual reset }
  FProgressTimer := CreateWaitableTimer(nil, False, nil);      { auto reset }
  if FProgressTimer = 0 then
    LZMAWin32Error('CreateWaitableTimer');
  InitializeProps(CompressionLevel, ACompressorProps);
end;

destructor TLZMACompressor.Destroy;

  procedure DestroyEvent(const AEvent: THandle);
  begin
    if AEvent <> 0 then
      CloseHandle(AEvent);
  end;

begin
  FWorker.Free;
  DestroyEvent(FProgressTimer);
  DestroyEvent(FEvents.WorkerEncodeFinishedEvent);
  DestroyEvent(FEvents.WorkerWaitingOnOutputEvent);
  DestroyEvent(FEvents.WorkerWaitingOnInputEvent);
  DestroyEvent(FEvents.EndWaitOnOutputEvent);
  DestroyEvent(FEvents.EndWaitOnInputEvent);
  DestroyEvent(FEvents.StartEncodeEvent);
  DestroyEvent(FEvents.TerminateWorkerEvent);
  inherited;
end;

procedure TLZMACompressor.InitializeProps(const CompressionLevel: Integer;
  const ACompressorProps: TCompressorProps);
const
  algorithm: array [clLZMAFast..clLZMAUltra64] of Integer = (0, 1, 1, 1, 1);
  dicSize: array [clLZMAFast..clLZMAUltra64] of Cardinal = (32 shl 10, 2 shl 20, 8 shl 20, 32 shl 20, 64 shl 20);
  numFastBytes: array [clLZMAFast..clLZMAUltra64] of Integer = (32, 32, 64, 64, 64);
  btMode: array [clLZMAFast..clLZMAUltra64] of Integer = (0, 1, 1, 1, 1);
  numHashBytes: array [Boolean] of Integer = (5, 4);
var
  EncProps: TLZMAEncoderProps;
  Props: TLZMACompressorProps;
  WorkerProcessFilename: String;
begin
  if (CompressionLevel < Low(algorithm)) or (CompressionLevel > High(algorithm)) then
    LZMAInternalError('TLZMACompressor.Create got invalid CompressionLevel ' + IntToStr(CompressionLevel));

  FillChar(EncProps, SizeOf(EncProps), 0);
  EncProps.Algorithm := algorithm[CompressionLevel];
  EncProps.BTMode := btMode[CompressionLevel];
  EncProps.DictionarySize := dicSize[CompressionLevel];
  EncProps.NumBlockThreads := -1;
  EncProps.NumFastBytes := numFastBytes[CompressionLevel];
  EncProps.NumThreads := -1;

  if ACompressorProps is TLZMACompressorProps then begin
    Props := (ACompressorProps as TLZMACompressorProps);
    if Props.Algorithm <> -1 then
      EncProps.Algorithm := Props.Algorithm;
    EncProps.BlockSize := Props.BlockSize;
    if Props.BTMode <> -1 then
      EncProps.BTMode := Props.BTMode;
    if Props.DictionarySize <> 0 then
      EncProps.DictionarySize := Props.DictionarySize;
    if Props.NumBlockThreads <> 0 then
      EncProps.NumBlockThreads := Props.NumBlockThreads;
    if Props.NumFastBytes <> 0 then
      EncProps.NumFastBytes := Props.NumFastBytes;
    if Props.NumThreads <> 0 then
      EncProps.NumThreads := Props.NumThreads;
    WorkerProcessFilename := Props.WorkerProcessFilename;
  end;

  EncProps.NumHashBytes := numHashBytes[EncProps.BTMode = 1];

  if WorkerProcessFilename <> '' then begin
    FWorker := TLZMAWorkerProcess.Create(@FEvents);
    (FWorker as TLZMAWorkerProcess).ExeFilename := WorkerProcessFilename;
  end
  else begin
    if not LZMADLLInitialized then
      LZMAInternalError('LZMA DLL functions not initialized');
    FWorker := TLZMAWorkerThread.Create(@FEvents);
  end;
  FShared := FWorker.FShared;
  FWorker.SetProps(FUseLZMA2, EncProps);
end;

class function TLZMACompressor.IsObjectSignaled(const AObject: THandle): Boolean;
begin
  Result := False;
  case WaitForSingleObject(AObject, 0) of
    WAIT_OBJECT_0: Result := True;
    WAIT_TIMEOUT: ;
  else
    LZMAInternalError('IsObjectSignaled: WaitForSingleObject failed');
  end;
end;

class procedure TLZMACompressor.SatisfyWorkerWait(const AWorkerEvent,
  AMainEvent: THandle);
begin
  if IsObjectSignaled(AWorkerEvent) then begin
    if not ResetEvent(AWorkerEvent) then
      LZMAWin32Error('SatisfyWorkerWait: ResetEvent');
    if not SetEvent(AMainEvent) then
      LZMAWin32Error('SatisfyWorkerWait: SetEvent');
  end;
end;

procedure TLZMACompressor.SatisfyWorkerWaitOnInput;
begin
  SatisfyWorkerWait(FEvents.WorkerWaitingOnInputEvent, FEvents.EndWaitOnInputEvent);
end;

procedure TLZMACompressor.SatisfyWorkerWaitOnOutput;
begin
  SatisfyWorkerWait(FEvents.WorkerWaitingOnOutputEvent, FEvents.EndWaitOnOutputEvent);
end;

procedure TLZMACompressor.UpdateProgress;
const
  MaxBytesPerProgressProcCall = 1 shl 30;  { 1 GB }
var
  NewProgressBytes, Bytes: Int64;
  LimitedBytes: Cardinal;
begin
  { Check if the timer is signaled. Because it's an auto-reset timer, this
    also resets it to non-signaled. Note that WaitForWorkerEvent also waits
    on the timer and sets FProgressTimerSignaled. }
  if IsObjectSignaled(FProgressTimer) then
    FProgressTimerSignaled := True;

  if FProgressTimerSignaled then begin
    FProgressTimerSignaled := False;
    if Assigned(ProgressProc) then begin
      { An Interlocked function is used to ensure the 64-bit value is read
        atomically (not with two separate 32-bit reads). }
      NewProgressBytes := InterlockedExchangeAdd64(FShared.ProgressBytes, 0);

      { Make sure the new value isn't negative or going backwards. A call
        to ProgressProc is always made, even if the byte count is 0. }
      if NewProgressBytes > FLastProgressBytes then begin
        Bytes := NewProgressBytes - FLastProgressBytes;
        FLastProgressBytes := NewProgressBytes;
      end else
        Bytes := 0;

      repeat
        if Bytes >= MaxBytesPerProgressProcCall then
          LimitedBytes := MaxBytesPerProgressProcCall
        else
          LimitedBytes := Cardinal(Bytes);
        ProgressProc(LimitedBytes);
        Dec(Bytes, LimitedBytes);
      until Bytes = 0;
    end;
  end;
end;

procedure TLZMACompressor.FlushOutputBuffer(const OnlyOptimalSize: Boolean);
const
  { Calling WriteProc may be an expensive operation, so we prefer to wait
    until we've accumulated a reasonable number of bytes before flushing }
  OptimalFlushSize = $10000;  { can't exceed size of OutputBuffer.Buf }
var
  Bytes: Longint;
begin
  while True do begin
    Bytes := FShared.OutputBuffer.Count;
    if Bytes = 0 then
      Break;
    if Bytes > OptimalFlushSize then
      Bytes := OptimalFlushSize;
    if OnlyOptimalSize and (Bytes < OptimalFlushSize) then
      Break;
    RingBufferReadToCallback(FShared.OutputBuffer, WriteProc, Bytes);
    { Output buffer (partially?) flushed; unblock worker Write }
    SatisfyWorkerWaitOnOutput;
  end;
  { Must satisfy a waiting worker even if there was nothing to flush. (Needed
    to avoid deadlock in the event the main thread empties the output buffer
    after the worker's FillBuffer(AWrite=True) gets Bytes=0 but *before* it
    sets WorkerWaitingOnOutputEvent and waits on EndWaitOnOutputEvent.) }
  SatisfyWorkerWaitOnOutput;
end;

procedure TLZMACompressor.StartEncode;

  procedure StartProgressTimer;
  const
    { This interval was chosen because:
      - It's two system timer ticks, rounded up:
          (1000 / 64) * 2 = 31.25
      - The keyboard repeat rate is 30/s by default:
          1000 / 30 = 33.333
        So if an edit control is focused and the ProgressProc is processing
        messages, the caret should move at full speed when an arrow key is
        held down. }
    Interval = 32;
  begin
    FProgressTimerSignaled := False;
    var DueTime := Int64(-10000) * Interval;
    if not SetWaitableTimer(FProgressTimer, DueTime, Interval, nil, nil, False) then
      LZMAWin32Error('SetWaitableTimer');
  end;

begin
  if not FEncodeStarted then begin
    FShared.NoMoreInput := False;
    FShared.ProgressBytes := 0;
    FShared.EncodeResult := -1;
    RingBufferReset(FShared.InputBuffer);
    RingBufferReset(FShared.OutputBuffer);
    FLastInputWriteCount := 0;
    FLastProgressBytes := 0;
    FEncodeFinished := False;
    FEncodeStarted := True;
    if not ResetEvent(FEvents.WorkerEncodeFinishedEvent) then
      LZMAWin32Error('StartEncode: ResetEvent');
    StartProgressTimer;
    if not SetEvent(FEvents.StartEncodeEvent) then
      LZMAWin32Error('StartEncode: SetEvent');
  end;
end;

procedure TLZMACompressor.WaitForWorkerEvent;
var
  H: array[0..4] of THandle;
begin
  { Wait until the worker needs our attention. Separate, manual-reset events
    are used for progress/input/output because it allows us to see
    specifically what the worker is waiting for, which eases debugging and
    helps to avoid unnecessary wakeups.
    Note that the order of the handles in the array is significant: when more
    than one object is signaled, WaitForMultipleObjects returns the index of
    the array's first signaled object. The "worker unexpectedly terminated"
    object must be at the front to ensure it takes precedence over the Worker*
    events. }
  H[0] := FWorker.GetExitHandle;
  H[1] := FEvents.WorkerEncodeFinishedEvent;
  H[2] := FProgressTimer;
  H[3] := FEvents.WorkerWaitingOnInputEvent;
  H[4] := FEvents.WorkerWaitingOnOutputEvent;
  case WaitForMultipleObjects(5, @H, False, INFINITE) of
    WAIT_OBJECT_0 + 0: FWorker.UnexpectedTerminationError;
    WAIT_OBJECT_0 + 1: FEncodeFinished := True;
    WAIT_OBJECT_0 + 2: FProgressTimerSignaled := True;
    WAIT_OBJECT_0 + 3,
    WAIT_OBJECT_0 + 4: ;
  else
    LZMAInternalError('WaitForWorkerEvent: WaitForMultipleObjects failed');
  end;
end;

procedure TLZMACompressor.DoCompress(const Buffer; Count: Longint);
var
  P: ^Byte;
  BytesWritten: Longint;
  InputWriteCount: LongWord;
begin
  StartEncode;

  P := @Buffer;
  while Count > 0 do begin
    if FEncodeFinished then begin
      if FShared.EncodeResult = SZ_ERROR_MEM then
        OutOfMemoryError;
      LZMAInternalErrorFmt('Compress: LZMA_Encode failed with code %d',
        [FShared.EncodeResult]);
    end;
    UpdateProgress;
    { Note that the progress updates that come in every ~100 ms also serve to
      keep the output buffer flushed well before it fills up. }
    FlushOutputBuffer(True);
    BytesWritten := RingBufferWrite(FShared.InputBuffer, P^, Count);
    if BytesWritten = 0 then begin
      { Input buffer full; unblock worker Read }
      SatisfyWorkerWaitOnInput;
      { Wait until the worker wants more input, needs output to be flushed,
        and/or has progress to report. All combinations are possible, so we
        need to handle all three before waiting again. }
      WaitForWorkerEvent;
    end
    else begin
      Dec(Count, BytesWritten);
      Inc(P, BytesWritten);

      { Unblock the worker every 64 KB so it doesn't have to wait until the
        entire input buffer is filled to begin/continue compressing. }
      InputWriteCount := FLastInputWriteCount + LongWord(BytesWritten);
      if InputWriteCount shr 16 <> FLastInputWriteCount shr 16 then
        SatisfyWorkerWaitOnInput;
      FLastInputWriteCount := InputWriteCount;
    end;
  end;
end;

procedure TLZMACompressor.DoFinish;
begin
  StartEncode;

  { Ensure prior InputBuffer updates are made visible before setting
    NoMoreInput. (This isn't actually needed right now because there's
    already a full barrier inside RingBufferWrite. But that's an
    implementation detail.) }
  MemoryBarrier;
  FShared.NoMoreInput := True;
  while not FEncodeFinished do begin
    SatisfyWorkerWaitOnInput;
    UpdateProgress;
    FlushOutputBuffer(True);
    { Wait until the worker wants more input, needs output to be flushed,
      and/or has progress to report. All combinations are possible, so we
      need to handle all three before waiting again. }
    WaitForWorkerEvent;
  end;
  { Flush any remaining output in optimally-sized blocks, then flush whatever
    is left }
  FlushOutputBuffer(True);
  FlushOutputBuffer(False);
  case FShared.EncodeResult of
    SZ_OK: ;
    SZ_ERROR_MEM: OutOfMemoryError;
  else
    LZMAInternalErrorFmt('Finish: LZMA_Encode failed with code %d',
      [FShared.EncodeResult]);
  end;

  { Encoding was successful; verify that all input was consumed }
  if FShared.InputBuffer.Count <> 0 then
    LZMAInternalErrorFmt('Finish: Input buffer is not empty (%d)',
      [FShared.InputBuffer.Count]);

  FEncodeStarted := False;
  if not CancelWaitableTimer(FProgressTimer) then
    LZMAWin32Error('CancelWaitableTimer');
end;

{ TLZMA2Compressor }

constructor TLZMA2Compressor.Create(AWriteProc: TCompressorWriteProc;
  AProgressProc: TCompressorProgressProc; CompressionLevel: Integer;
  ACompressorProps: TCompressorProps);
begin
  FUseLZMA2 := True;
  inherited;
end;

end.
