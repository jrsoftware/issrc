unit Compiler.CompressionHandler;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compression handler used by TSetupCompiler
}

interface

uses
  SHA256, ChaCha20, Shared.Struct, Shared.Int64Em, Shared.FileClass, Compression.Base,
  Compiler.StringLists, Compiler.SetupCompiler;

type
  TCompressionHandler = class
  private
    FCachedCompressors: TLowFragList;
    FCompiler: TSetupCompiler;
    FCompressor: TCustomCompressor;
    FChunkBytesRead: Integer64;
    FChunkBytesWritten: Integer64;
    FChunkEncrypted: Boolean;
    FChunkFirstSlice: Integer;
    FChunkStarted: Boolean;
    FChunkStartOffset: Longint;
    FCryptContext: TChaCha20Context;
    FCurSlice: Integer;
    FDestFile: TFile;
    FDestFileIsDiskSlice: Boolean;
    FInitialBytesCompressedSoFar: Integer64;
    FSliceBaseOffset: Cardinal;
    FSliceBytesLeft: Cardinal;
    procedure EndSlice;
    procedure NewSlice(const Filename: String);
  public
    constructor Create(ACompiler: TSetupCompiler; const InitialSliceFilename: String);
    destructor Destroy; override;
    procedure CompressFile(const SourceFile: TFile; Bytes: Integer64;
      const CallOptimize: Boolean; var SHA256Sum: TSHA256Digest);
    procedure EndChunk;
    procedure Finish;
    procedure NewChunk(const ACompressorClass: TCustomCompressorClass;
      const ACompressLevel: Integer; const ACompressorProps: TCompressorProps;
      const AUseEncryption: Boolean; const ACryptKey: TSetupEncryptionKey);
    procedure ProgressProc(BytesProcessed: Cardinal);
    function ReserveBytesOnSlice(const Bytes: Cardinal): Boolean;
    procedure WriteProc(const Buf; BufSize: Longint);
    property ChunkBytesRead: Integer64 read FChunkBytesRead;
    property ChunkBytesWritten: Integer64 read FChunkBytesWritten;
    property ChunkEncrypted: Boolean read FChunkEncrypted;
    property ChunkFirstSlice: Integer read FChunkFirstSlice;
    property ChunkStartOffset: Longint read FChunkStartOffset;
    property ChunkStarted: Boolean read FChunkStarted;
    property CurSlice: Integer read FCurSlice;
  end;

implementation

uses
  SysUtils, Compiler.Messages, Compiler.HelperFunc;

constructor TCompressionHandler.Create(ACompiler: TSetupCompiler;
  const InitialSliceFilename: String);
begin
  inherited Create;
  FCompiler := ACompiler;
  FCurSlice := -1;
  FCachedCompressors := TLowFragList.Create;
  NewSlice(InitialSliceFilename);
end;

destructor TCompressionHandler.Destroy;
var
  I: Integer;
begin
  if Assigned(FCachedCompressors) then begin
    for I := FCachedCompressors.Count-1 downto 0 do
      TCustomCompressor(FCachedCompressors[I]).Free;
    FreeAndNil(FCachedCompressors);
  end;
  FreeAndNil(FDestFile);
  inherited;
end;

procedure TCompressionHandler.Finish;
begin
  EndChunk;
  EndSlice;
end;

procedure TCompressionHandler.EndSlice;
var
  DiskSliceHeader: TDiskSliceHeader;
begin
  if Assigned(FDestFile) then begin
    if FDestFileIsDiskSlice then begin
      DiskSliceHeader.TotalSize := FDestFile.Size.Lo;
      FDestFile.Seek(SizeOf(DiskSliceID));
      FDestFile.WriteBuffer(DiskSliceHeader, SizeOf(DiskSliceHeader));
    end;
    FreeAndNil(FDestFile);
  end;
end;

procedure TCompressionHandler.NewSlice(const Filename: String);

  function GenerateSliceFilename(const Compiler: TSetupCompiler;
    const ASlice: Integer): String;
  begin
    var SlicesPerDisk := Compiler.GetSlicesPerDisk;
    var OutputBaseFilename := Compiler.GetOutputBaseFilename;
    var Major := ASlice div SlicesPerDisk + 1;
    var Minor := ASlice mod SlicesPerDisk;
    if SlicesPerDisk = 1 then
      Result := Format('%s-%d.bin', [OutputBaseFilename, Major])
    else
      Result := Format('%s-%d%s.bin', [OutputBaseFilename, Major,
        Chr(Ord('a') + Minor)]);
  end;

begin
  var DiskSliceSize := FCompiler.GetDiskSliceSize;
  EndSlice;
  Inc(FCurSlice);
  if (FCurSlice > 0) and not FCompiler.GetDiskSpanning then
    FCompiler.AbortCompileFmt(SCompilerMustUseDiskSpanning,
      [DiskSliceSize]);
  if Filename = '' then begin
    FDestFileIsDiskSlice := True;
    FDestFile := TFile.Create(FCompiler.GetOutputDir +
      GenerateSliceFilename(FCompiler, FCurSlice), fdCreateAlways, faReadWrite, fsNone);
    FDestFile.WriteBuffer(DiskSliceID, SizeOf(DiskSliceID));
    var DiskHeader: TDiskSliceHeader;
    DiskHeader.TotalSize := 0;
    FDestFile.WriteBuffer(DiskHeader, SizeOf(DiskHeader));
    FSliceBaseOffset := 0;
    FSliceBytesLeft := DiskSliceSize - (SizeOf(DiskSliceID) + SizeOf(DiskHeader));
  end
  else begin
    FDestFileIsDiskSlice := False;
    FDestFile := TFile.Create(Filename, fdOpenExisting, faReadWrite, fsNone);
    FDestFile.SeekToEnd;
    FSliceBaseOffset := FDestFile.Position.Lo;
    FSliceBytesLeft := Cardinal(DiskSliceSize) - FSliceBaseOffset;
  end;
end;

function TCompressionHandler.ReserveBytesOnSlice(const Bytes: Cardinal): Boolean;
begin
  if FSliceBytesLeft >= Bytes then begin
    Dec(FSliceBytesLeft, Bytes);
    Result := True;
  end
  else
    Result := False;
end;

procedure TCompressionHandler.NewChunk(const ACompressorClass: TCustomCompressorClass;
  const ACompressLevel: Integer; const ACompressorProps: TCompressorProps;
  const AUseEncryption: Boolean; const ACryptKey: TSetupEncryptionKey);

  procedure SelectCompressor;
  var
    I: Integer;
    C: TCustomCompressor;
  begin
    { No current compressor, or changing compressor classes? }
    if (FCompressor = nil) or (FCompressor.ClassType <> ACompressorClass) then begin
      FCompressor := nil;
      { Search cache for requested class }
      for I := FCachedCompressors.Count-1 downto 0 do begin
        C := FCachedCompressors[I];
        if C.ClassType = ACompressorClass then begin
          FCompressor := C;
          Break;
        end;
      end;
    end;
    if FCompressor = nil then begin
      FCachedCompressors.Expand;
      FCompressor := ACompressorClass.Create(WriteProc, ProgressProc,
        ACompressLevel, ACompressorProps);
      FCachedCompressors.Add(FCompressor);
    end;
  end;

  procedure InitEncryption;
  begin
    { Create a unique nonce from the base nonce }
    var Nonce := FCompiler.GetEncryptionBaseNonce;
    Nonce.RandomXorStartOffset := Nonce.RandomXorStartOffset xor FChunkStartOffset;
    Nonce.RandomXorFirstSlice := Nonce.RandomXorFirstSlice xor FChunkFirstSlice;

    XChaCha20Init(FCryptContext, ACryptKey[0], Length(ACryptKey), Nonce, SizeOf(Nonce), 0);
  end;

var
  MinBytesLeft: Cardinal;
begin
  EndChunk;

  { If there isn't enough room left to start a new chunk on the current slice,
    start a new slice }
  MinBytesLeft := SizeOf(ZLIBID);
  Inc(MinBytesLeft);  { for at least one byte of data }
  if FSliceBytesLeft < MinBytesLeft then
    NewSlice('');

  FChunkFirstSlice := FCurSlice;
  FChunkStartOffset := FDestFile.Position.Lo - FSliceBaseOffset;
  FDestFile.WriteBuffer(ZLIBID, SizeOf(ZLIBID));
  Dec(FSliceBytesLeft, SizeOf(ZLIBID));
  FChunkBytesRead.Hi := 0;
  FChunkBytesRead.Lo := 0;
  FChunkBytesWritten.Hi := 0;
  FChunkBytesWritten.Lo := 0;
  FInitialBytesCompressedSoFar := FCompiler.GetBytesCompressedSoFar;

  SelectCompressor;

  FChunkEncrypted := AUseEncryption;
  if AUseEncryption then
    InitEncryption;

  FChunkStarted := True;
end;

procedure TCompressionHandler.EndChunk;
begin
  if not FChunkStarted then
    Exit;
  FChunkStarted := False;

  FCompressor.Finish;
  { In case we didn't get a ProgressProc call after the final block: }
  FCompiler.SetBytesCompressedSoFar(FInitialBytesCompressedSoFar);
  FCompiler.AddBytesCompressedSoFar(FChunkBytesRead);
  FCompiler.CallIdleProc;
end;

procedure TCompressionHandler.CompressFile(const SourceFile: TFile;
  Bytes: Integer64; const CallOptimize: Boolean; var SHA256Sum: TSHA256Digest);
var
  Context: TSHA256Context;
  AddrOffset: LongWord;
  BufSize: Cardinal;
  Buf: array[0..65535] of Byte;
  { ^ *must* be the same buffer size used in Setup (TFileExtractor), otherwise
    the TransformCallInstructions call will break }
begin
  SHA256Init(Context);
  AddrOffset := 0;
  while True do begin
    BufSize := SizeOf(Buf);
    if (Bytes.Hi = 0) and (Bytes.Lo < BufSize) then
      BufSize := Bytes.Lo;
    if BufSize = 0 then
      Break;

    SourceFile.ReadBuffer(Buf, BufSize);
    Inc64(FChunkBytesRead, BufSize);
    Dec64(Bytes, BufSize);
    SHA256Update(Context, Buf, BufSize);
    if CallOptimize then begin
      TransformCallInstructions(Buf, BufSize, True, AddrOffset);
      Inc(AddrOffset, BufSize);  { may wrap, but OK }
    end;
    FCompressor.Compress(Buf, BufSize);
  end;
  SHA256Sum := SHA256Final(Context);
end;

procedure TCompressionHandler.WriteProc(const Buf; BufSize: Longint);
var
  P, P2: Pointer;
  S: Cardinal;
begin
  FCompiler.CallIdleProc;
  P := @Buf;
  while BufSize > 0 do begin
    S := BufSize;
    if FSliceBytesLeft = 0 then
      NewSlice('');
    if S > Cardinal(FSliceBytesLeft) then
      S := FSliceBytesLeft;

    if not FChunkEncrypted then
      FDestFile.WriteBuffer(P^, S)
    else begin
      { Using encryption. Can't modify Buf in place so allocate a new,
        temporary buffer. }
      GetMem(P2, S);
      try
        XChaCha20Crypt(FCryptContext, P^, P2^, S);
        FDestFile.WriteBuffer(P2^, S)
      finally
        FreeMem(P2);
      end;
    end;

    Inc64(FChunkBytesWritten, S);
    Inc(Cardinal(P), S);
    Dec(BufSize, S);
    Dec(FSliceBytesLeft, S);
  end;
end;

procedure TCompressionHandler.ProgressProc(BytesProcessed: Cardinal);
begin
  FCompiler.AddBytesCompressedSoFar(BytesProcessed);
  FCompiler.CallIdleProc;
end;

end.
