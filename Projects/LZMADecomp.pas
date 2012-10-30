unit LZMADecomp;

{
  Inno Setup
  Copyright (C) 1997-2012 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Interface to the "new" LZMA/LZMA2 SDK decompression OBJs in lzma2\Decoder
}

interface

{$I VERSION.INC}

uses
  Windows, SysUtils, Int64Em, Compress;

type
  TLZMACustomDecompressor = class(TCustomDecompressor)
  private
    FReachedEnd: Boolean;
    FHeaderProcessed: Boolean;
    FNextIn: ^Byte;
    FAvailIn: Cardinal;
    FBuffer: array[0..65535] of Byte;
  protected
    function DecodeToBuf(var Dest; var DestLen: Cardinal; const Src;
      var SrcLen: Cardinal; var Status: Integer): Integer; virtual; abstract;
    procedure FreeDecoder; virtual; abstract;
    procedure ProcessHeader; virtual; abstract;
  public
    destructor Destroy; override;
    procedure DecompressInto(var Buffer; Count: Longint); override;
    procedure Reset; override;
  end;

  { Internally-used records }
  TLZMA1InternalDecoderState = array[0..27] of LongWord;
  TLZMA2InternalDecoderState = array[0..34] of LongWord;

  TLZMA1Decompressor = class(TLZMACustomDecompressor)
  private
    FDecoderState: TLZMA1InternalDecoderState;
  protected
    function DecodeToBuf(var Dest; var DestLen: Cardinal; const Src;
      var SrcLen: Cardinal; var Status: Integer): Integer; override;
    procedure FreeDecoder; override;
    procedure ProcessHeader; override;
  end;

  TLZMA2Decompressor = class(TLZMACustomDecompressor)
  private
    FDecoderState: TLZMA2InternalDecoderState;
  protected
    function DecodeToBuf(var Dest; var DestLen: Cardinal; const Src;
      var SrcLen: Cardinal; var Status: Integer): Integer; override;
    procedure FreeDecoder; override;
    procedure ProcessHeader; override;
  end;

implementation

type
  TLZMASRes = type Integer;

  PLZMAISzAlloc = ^TLZMAISzAlloc;
  TLZMAISzAlloc = record
    Alloc: function(p: PLZMAISzAlloc; size: Cardinal): Pointer;
    Free: procedure(p: PLZMAISzAlloc; address: Pointer);
  end;

const
  { SRes (TLZMASRes) }
  SZ_OK = 0;
  SZ_ERROR_DATA = 1;
  SZ_ERROR_MEM = 2;

  { ELzmaFinishMode }
  LZMA_FINISH_ANY = 0;
  LZMA_FINISH_END = 1;

  { ELzmaStatus }
  LZMA_STATUS_NOT_SPECIFIED = 0;
  LZMA_STATUS_FINISHED_WITH_MARK = 1;
  LZMA_STATUS_NOT_FINISHED = 2;
  LZMA_STATUS_NEEDS_MORE_INPUT = 3;
  LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK = 4;

  SLZMADecompDataError = 'lzmadecomp: Compressed data is corrupted (%d)';

  { To ensure we don't allocate inordinate amounts of memory in the event a
    stream's header is corrupted, we limit the dictionary size to the maximum
    size the compiler currently allows. }
  MaxDictionarySize = 256 shl 20;  { 256 MB }

{$L lzma2\Decoder\ISLzmaDec.obj}
{$L lzma2\Decoder\ISLzma2Dec.obj}

function IS_LzmaDec_Init(var state: TLZMA1InternalDecoderState;
  stateSize: Cardinal; const props; propsSize: Cardinal;
  const alloc: TLZMAISzAlloc): TLZMASRes; external;
function LzmaDec_DecodeToBuf(var state: TLZMA1InternalDecoderState; var dest;
  var destLen: Cardinal; const src; var srcLen: Cardinal; finishMode: Integer;
  var status: Integer): TLZMASRes; external;
procedure LzmaDec_Free(var state: TLZMA1InternalDecoderState;
  const alloc: TLZMAISzAlloc); external;

function IS_Lzma2Dec_Init(var state: TLZMA2InternalDecoderState;
  stateSize: Cardinal; prop: Byte; const alloc: TLZMAISzAlloc): TLZMASRes;
  external;
function Lzma2Dec_DecodeToBuf(var state: TLZMA2InternalDecoderState; var dest;
  var destLen: Cardinal; const src; var srcLen: Cardinal; finishMode: Integer;
  var status: Integer): TLZMASRes; external;
procedure IS_Lzma2Dec_Free(var state: TLZMA2InternalDecoderState;
  const alloc: TLZMAISzAlloc); external;

procedure LzmaDec_Allocate; external;
procedure LzmaDec_AllocateProbs; external;
procedure LzmaDec_DecodeToDic; external;
procedure LzmaDec_FreeProbs; external;
procedure LzmaDec_Init; external;
procedure LzmaDec_InitDicAndState; external;
procedure LzmaProps_Decode; external;

procedure LZMADecompInternalError(const Msg: String);
begin
  raise ECompressInternalError.CreateFmt('lzmadecomp: %s', [Msg]);
end;

procedure LZMADecompDataError(const Id: Integer);
begin
  raise ECompressDataError.CreateFmt(SLZMADecompDataError, [Id]);
end;

function LZMAAllocFunc(p: PLZMAISzAlloc; size: Cardinal): Pointer;
begin
  if (size <> 0) and (size <= Cardinal(MaxDictionarySize)) then
    Result := VirtualAlloc(nil, size, MEM_COMMIT, PAGE_READWRITE)
  else
    Result := nil;
end;

procedure LZMAFreeFunc(p: PLZMAISzAlloc; address: Pointer);
begin
  if Assigned(address) then
    VirtualFree(address, 0, MEM_RELEASE);
end;

const
  LZMAAlloc: TLZMAISzAlloc = (Alloc: LZMAAllocFunc; Free: LZMAFreeFunc);

function _memcpy(dest, src: Pointer; n: Cardinal): Pointer; cdecl;
begin
  Move(src^, dest^, n);
  Result := dest;
end;

{ TLZMACustomDecompressor }

destructor TLZMACustomDecompressor.Destroy;
begin
  FreeDecoder;
  inherited;
end;

procedure TLZMACustomDecompressor.DecompressInto(var Buffer; Count: Longint);
var
  NextOut: ^Byte;
  AvailOut: Longint;
  OutBytes, InBytes: Cardinal;
  Code: TLZMASRes;
  DecodeStatus: Integer;
begin
  if not FHeaderProcessed then begin
    { Reset these following a reset }
    FNextIn := @FBuffer;
    FAvailIn := 0;
    ProcessHeader;
    FHeaderProcessed := True;
  end;
  NextOut := @Buffer;
  AvailOut := Count;
  while AvailOut > 0 do begin
    if (FAvailIn = 0) and not FReachedEnd then begin
      FNextIn := @FBuffer;
      FAvailIn := ReadProc(FBuffer, SizeOf(FBuffer));
      if FAvailIn = 0 then
        FReachedEnd := True;  { not really necessary, but for consistency }
    end;
    OutBytes := AvailOut;
    InBytes := FAvailIn;
    Code := DecodeToBuf(NextOut^, OutBytes, FNextIn^, InBytes, DecodeStatus);
    case Code of
      SZ_OK: ;
      SZ_ERROR_DATA: LZMADecompDataError(3);
    else
      LZMADecompInternalError(Format('DecodeToBuf failed (%d)', [Code]));
    end;
    { No bytes in or out indicates corruption somewhere. Possibilities:
      - The caller is asking for too many bytes.
      - It needs more input but there isn't any left.
      - It encountered an "end mark" previously and is now refusing to
        process any further input. }
    if (InBytes = 0) and (OutBytes = 0) then
      LZMADecompDataError(4);
    Dec(AvailOut, OutBytes);
    Inc(NextOut, OutBytes);
    Dec(FAvailIn, InBytes);
    Inc(FNextIn, InBytes);
  end;
end;

procedure TLZMACustomDecompressor.Reset;
begin
  FHeaderProcessed := False;
  FReachedEnd := False;
end;

{ TLZMA1Decompressor }

procedure TLZMA1Decompressor.FreeDecoder;
begin
  LzmaDec_Free(FDecoderState, LZMAAlloc);
end;

procedure TLZMA1Decompressor.ProcessHeader;
var
  Props: packed record  { size = LZMA_PROPS_SIZE (5) }
    Misc: Byte;
    DictionarySize: LongWord;  { little endian, unaligned }  
  end;
begin
  { Read header fields }
  if ReadProc(Props, SizeOf(Props)) <> SizeOf(Props) then
    LZMADecompDataError(1);

  if Props.DictionarySize > LongWord(MaxDictionarySize) then
    LZMADecompDataError(5);

  { Note: IS_LzmaDec_Init will re-use already-allocated memory if it can.
    FDecoderState is assumed to be initialized to zero on the first call. }
  case IS_LzmaDec_Init(FDecoderState, SizeOf(FDecoderState), Props,
     SizeOf(Props), LZMAAlloc) of
    SZ_OK: ;
    SZ_ERROR_MEM: OutOfMemoryError;
  else
    { SZ_ERROR_UNSUPPORTED and anything else }
    LZMADecompDataError(2);
  end;
end;

function TLZMA1Decompressor.DecodeToBuf(var Dest; var DestLen: Cardinal;
  const Src; var SrcLen: Cardinal; var Status: Integer): Integer;
begin
  Result := LzmaDec_DecodeToBuf(FDecoderState, Dest, DestLen, Src, SrcLen,
    LZMA_FINISH_ANY, Status);
end;

{ TLZMA2Decompressor }

procedure TLZMA2Decompressor.FreeDecoder;
begin
  IS_Lzma2Dec_Free(FDecoderState, LZMAAlloc);
end;

procedure TLZMA2Decompressor.ProcessHeader;

  { Macro from Lzma2Dec.c: }
  function LZMA2_DIC_SIZE_FROM_PROP(p: LongWord): LongWord;
  begin
    Result := (2 or (p and 1)) shl (p div 2 + 11);
  end;

var
  Prop: Byte;
begin
  { Read header fields }
  if ReadProc(Prop, SizeOf(Prop)) <> SizeOf(Prop) then
    LZMADecompDataError(1);

  if (Prop >= 40) or
     (LZMA2_DIC_SIZE_FROM_PROP(Prop) > LongWord(MaxDictionarySize)) then
    LZMADecompDataError(5);

  { Note: IS_Lzma2Dec_Init will re-use already-allocated memory if it can.
    FDecoderState is assumed to be initialized to zero on the first call. }
  case IS_Lzma2Dec_Init(FDecoderState, SizeOf(FDecoderState), Prop,
     LZMAAlloc) of
    SZ_OK: ;
    SZ_ERROR_MEM: OutOfMemoryError;
  else
    { SZ_ERROR_UNSUPPORTED and anything else }
    LZMADecompDataError(2);
  end;
end;

function TLZMA2Decompressor.DecodeToBuf(var Dest; var DestLen: Cardinal;
  const Src; var SrcLen: Cardinal; var Status: Integer): Integer;
begin
  Result := Lzma2Dec_DecodeToBuf(FDecoderState, Dest, DestLen, Src, SrcLen,
    LZMA_FINISH_ANY, Status);
end;

end.
