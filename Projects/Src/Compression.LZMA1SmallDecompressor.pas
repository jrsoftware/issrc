unit Compression.LZMA1SmallDecompressor;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Interface to the older, size-optimized LZMA SDK 4.43 decompression OBJ in
  Compression.LZMA1SmallDecompressor\LzmaDecode, used by SetupLdr.
}

interface

uses
  Windows, SysUtils, Compression.Base, Shared.Int64Em;

type
  { Internally-used record }
  TLZMAInternalDecoderState = record
    { NOTE: None of these fields are ever accessed directly by this unit.
      They are exposed purely for debugging purposes. }
    opaque_Properties: record
      lc: Integer;
      lp: Integer;
      pb: Integer;
      DictionarySize: LongWord;
    end;
    opaque_Probs: Pointer;
    opaque_Buffer: Pointer;
    opaque_BufferLim: Pointer;
    opaque_Dictionary: Pointer;
    opaque_Range: LongWord;
    opaque_Code: LongWord;
    opaque_DictionaryPos: LongWord;
    opaque_GlobalPos: LongWord;
    opaque_DistanceLimit: LongWord;
    opaque_Reps: array[0..3] of LongWord;
    opaque_State: Integer;
    opaque_RemainLen: Integer;
    opaque_TempDictionary: array[0..3] of Byte;
  end;

  TLZMA1SmallDecompressor = class(TCustomDecompressor)
  private
    FReachedEnd: Boolean;
    FHeaderProcessed: Boolean;
    FDecoderState: TLZMAInternalDecoderState;
    FHeapBase: Pointer;
    FHeapSize: Cardinal;
    FBuffer: array[0..65535] of Byte;
    procedure DestroyHeap;
    procedure DoRead(var Buffer: Pointer; var BufferSize: Cardinal);
    procedure ProcessHeader;
  public
    destructor Destroy; override;
    procedure DecompressInto(var Buffer; Count: Longint); override;
    procedure Reset; override;
  end;

implementation

const
  SLZMADataError = 'lzma1smalldecompressor: Compressed data is corrupted (%d)';

procedure LZMADataError(const Id: Integer);
begin
  raise ECompressDataError.CreateFmt(SLZMADataError, [Id]);
end;

procedure LZMAInternalError(const Msg: String);
begin
  raise ECompressInternalError.CreateFmt('lzma1smalldecompressor: %s', [Msg]);
end;

procedure LZMAInternalErrorFmt(const Msg: String; const Args: array of const);
begin
  LZMAInternalError(Format(Msg, Args));
end;

{ TLZMA1SmallDecompressor }

{$L Src\Compression.LZMA1SmallDecompressor\LzmaDecode\LzmaDecodeInno.obj}

type
  TLzmaInCallback = record
    Read: function(obj: Pointer; var buffer: Pointer; var bufferSize: Cardinal): Integer; cdecl;
  end;

const
  LZMA_RESULT_OK = 0;
  LZMA_RESULT_DATA_ERROR = 1;

  LZMA_PROPERTIES_SIZE = 5;

function LzmaMyDecodeProperties(var vs: TLZMAInternalDecoderState;
  vsSize: Integer; const propsData; propsDataSize: Integer;
  var outPropsSize: LongWord; var outDictionarySize: LongWord): Integer; cdecl; external name '_LzmaMyDecodeProperties';
procedure LzmaMyDecoderInit(var vs: TLZMAInternalDecoderState;
  probsPtr: Pointer; dictionaryPtr: Pointer); cdecl; external name '_LzmaMyDecoderInit';
function LzmaDecode(var vs: TLZMAInternalDecoderState;
  var inCallback: TLzmaInCallback; var outStream; outSize: Cardinal;
  var outSizeProcessed: Cardinal): Integer; cdecl; external name '_LzmaDecode';

type
  TLZMADecompressorCallbackData = record
    Callback: TLzmaInCallback;
    Instance: TLZMA1SmallDecompressor;
  end;

function ReadFunc(obj: Pointer; var buffer: Pointer; var bufferSize: Cardinal): Integer; cdecl;
begin
  TLZMADecompressorCallbackData(obj^).Instance.DoRead(buffer, bufferSize);
  { Don't bother returning any sort of failure code, because if DoRead failed,
    it would've raised an exception }
  Result := LZMA_RESULT_OK;
end;

destructor TLZMA1SmallDecompressor.Destroy;
begin
  DestroyHeap;
  inherited;
end;

procedure TLZMA1SmallDecompressor.DestroyHeap;
begin
  FHeapSize := 0;
  if Assigned(FHeapBase) then begin
    VirtualFree(FHeapBase, 0, MEM_RELEASE);
    FHeapBase := nil;
  end;
end;

procedure TLZMA1SmallDecompressor.DoRead(var Buffer: Pointer; var BufferSize: Cardinal);
begin
  Buffer := @FBuffer;
  BufferSize := 0;
  if not FReachedEnd then begin
    BufferSize := ReadProc(FBuffer, SizeOf(FBuffer));
    if BufferSize = 0 then
      FReachedEnd := True;  { not really necessary, but for consistency }
  end;
end;

procedure TLZMA1SmallDecompressor.ProcessHeader;
var
  Props: array[0..LZMA_PROPERTIES_SIZE-1] of Byte;
  ProbsSize, DictionarySize: LongWord;
  NewHeapSize: Cardinal;
begin
  { Read header fields }
  if ReadProc(Props, SizeOf(Props)) <> SizeOf(Props) then
    LZMADataError(1);

  { Initialize the LZMA decoder state structure, and calculate the size of
    the Probs and Dictionary }
  FillChar(FDecoderState, SizeOf(FDecoderState), 0);
  if LzmaMyDecodeProperties(FDecoderState, SizeOf(FDecoderState), Props,
     SizeOf(Props), ProbsSize, DictionarySize) <> LZMA_RESULT_OK then
    LZMADataError(3);
  if DictionarySize > LongWord(64 shl 20) then
    { sanity check: we only use dictionary sizes <= 64 MB }
    LZMADataError(7);

  { Allocate memory for the Probs and Dictionary, and pass the pointers over }
  NewHeapSize := ProbsSize + DictionarySize;
  if FHeapSize <> NewHeapSize then begin
    DestroyHeap;
    FHeapBase := VirtualAlloc(nil, NewHeapSize, MEM_COMMIT, PAGE_READWRITE);
    if FHeapBase = nil then
      OutOfMemoryError;
    FHeapSize := NewHeapSize;
  end;
  LzmaMyDecoderInit(FDecoderState, FHeapBase, Pointer(Cardinal(FHeapBase) + ProbsSize));

  FHeaderProcessed := True;
end;

procedure TLZMA1SmallDecompressor.DecompressInto(var Buffer; Count: Longint);
var
  CallbackData: TLZMADecompressorCallbackData;
  Code: Integer;
  OutProcessed: Cardinal;
begin
  if not FHeaderProcessed then
    ProcessHeader;
  CallbackData.Callback.Read := ReadFunc;
  CallbackData.Instance := Self;
  Code := LzmaDecode(FDecoderState, CallbackData.Callback, Buffer, Count,
    OutProcessed);
  case Code of
    LZMA_RESULT_OK: ;
    LZMA_RESULT_DATA_ERROR: LZMADataError(5);
  else
    LZMAInternalErrorFmt('LzmaDecode failed (%d)', [Code]);
  end;
  if OutProcessed <> Cardinal(Count) then
    LZMADataError(6);
end;

procedure TLZMA1SmallDecompressor.Reset;
begin
  FHeaderProcessed := False;
  FReachedEnd := False;
end;

end.
