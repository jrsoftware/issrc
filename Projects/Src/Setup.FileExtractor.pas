unit Setup.FileExtractor;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TFileExtractor class to extract (=decrypt, decompress, and/or verify) Setup files
}

interface

uses
  Windows, SysUtils, Shared.Int64Em, Shared.FileClass, Compression.Base,
  Shared.Struct, ChaCha20;

type
  TExtractorProgressProc = procedure(Bytes: Cardinal);

  TFileExtractor = class
  private
    FDecompressor: array[Boolean] of TCustomDecompressor;
    FSourceF: TFile;
    FOpenedSlice, FChunkFirstSlice, FChunkLastSlice: Integer;
    FChunkStartOffset: Longint;
    FChunkBytesLeft, FChunkDecompressedBytesRead: Integer64;
    FNeedReset: Boolean;
    FChunkCompressed, FChunkEncrypted: Boolean;
    FCryptContext: TChaCha20Context;
    FCryptKey: TSetupEncryptionKey;
    FCryptKeySet: Boolean;
    FEntered: Integer;
    procedure DecompressBytes(var Buffer; Count: Cardinal);
    class function FindSliceFilename(const ASlice: Integer): String;
    procedure OpenSlice(const ASlice: Integer);
    function ReadProc(var Buf; Count: Longint): Longint;
    procedure SetCryptKey(const Value: TSetupEncryptionKey);
  public
    constructor Create(ADecompressorClass: TCustomDecompressorClass);
    destructor Destroy; override;
    procedure DecompressFile(const FL: TSetupFileLocationEntry; const DestF: TFile;
      const ProgressProc: TExtractorProgressProc; const VerifyChecksum: Boolean);
    procedure SeekTo(const FL: TSetupFileLocationEntry;
      const ProgressProc: TExtractorProgressProc);
    property CryptKey: TSetupEncryptionKey write SetCryptKey;
  end;

function FileExtractor: TFileExtractor;
procedure FreeFileExtractor;

implementation

uses
  PathFunc, Shared.CommonFunc, Setup.MainFunc, SetupLdrAndSetup.Messages,
  Shared.SetupMessageIDs, Setup.InstFunc, Compression.Zlib, Compression.bzlib,
  Compression.LZMADecompressor, SHA256, Setup.LoggingFunc, Setup.NewDiskForm;

var
  FFileExtractor: TFileExtractor;

function FileExtractor: TFileExtractor;
const
  DecompClasses: array[TSetupCompressMethod] of TCustomDecompressorClass =
    (TStoredDecompressor, TZDecompressor, TBZDecompressor, TLZMA1Decompressor,
     TLZMA2Decompressor);
begin
  if FFileExtractor = nil then
    FFileExtractor := TFileExtractor.Create(DecompClasses[SetupHeader.CompressMethod]);
  Result := FFileExtractor;
end;

procedure FreeFileExtractor;
begin
  FreeAndNil(FFileExtractor);
end;

procedure SourceIsCorrupted(const AReason: String);
begin
  Log('Source file corrupted: ' + AddPeriod(AReason));
  raise Exception.Create(SetupMessages[msgSourceIsCorrupted]);
end;

{ TFileExtractor }

constructor TFileExtractor.Create(ADecompressorClass: TCustomDecompressorClass);
begin
  inherited Create;
  FOpenedSlice := -1;
  FChunkFirstSlice := -1;
  { Create one 'decompressor' for use with uncompressed chunks, and another
    for use with compressed chunks }
  FDecompressor[False] := TStoredDecompressor.Create(ReadProc);
  FDecompressor[True] := ADecompressorClass.Create(ReadProc);
end;

destructor TFileExtractor.Destroy;
begin
  FSourceF.Free;
  FDecompressor[True].Free;
  FDecompressor[False].Free;
  inherited;
end;

procedure TFileExtractor.SetCryptKey(const Value: TSetupEncryptionKey);
begin
  FCryptKey := Value;
  FCryptKeySet := True;
end;

var
  LastSourceDir: String;

class function TFileExtractor.FindSliceFilename(const ASlice: Integer): String;
var
  Major, Minor: Integer;
  Prefix, F1, Path: String;
begin
  Prefix := PathChangeExt(PathExtractName(SetupLdrOriginalFilename), '');
  Major := ASlice div SetupHeader.SlicesPerDisk + 1;
  Minor := ASlice mod SetupHeader.SlicesPerDisk;
  if SetupHeader.SlicesPerDisk = 1 then
    F1 := Format('%s-%d.bin', [Prefix, Major])
  else
    F1 := Format('%s-%d%s.bin', [Prefix, Major, Chr(Ord('a') + Minor)]);
  if LastSourceDir <> '' then begin
    Result := AddBackslash(LastSourceDir) + F1;
    if NewFileExists(Result) then Exit;
  end;
  Result := AddBackslash(SourceDir) + F1;
  if NewFileExists(Result) then Exit;
  Path := SourceDir;
  LogFmt('Asking user for new disk containing "%s".', [F1]);
  if SelectDisk(Major, F1, Path) then begin
    LastSourceDir := Path;
    Result := AddBackslash(Path) + F1;
  end
  else
    Abort;
end;

procedure TFileExtractor.OpenSlice(const ASlice: Integer);
var
  Filename: String;
  TestDiskSliceID: TDiskSliceID;
  DiskSliceHeader: TDiskSliceHeader;
begin
  if FOpenedSlice = ASlice then
    Exit;

  FOpenedSlice := -1;
  FreeAndNil(FSourceF);

  if SetupLdrOffset1 = 0 then
    Filename := FindSliceFilename(ASlice)
  else
    Filename := SetupLdrOriginalFilename;
  FSourceF := TFile.Create(Filename, fdOpenExisting, faRead, fsRead);
  if SetupLdrOffset1 = 0 then begin
    if FSourceF.Read(TestDiskSliceID, SizeOf(TestDiskSliceID)) <> SizeOf(TestDiskSliceID) then
      SourceIsCorrupted('Invalid slice header (1)');
    if TestDiskSliceID <> DiskSliceID then
      SourceIsCorrupted('Invalid slice header (2)');
    if FSourceF.Read(DiskSliceHeader, SizeOf(DiskSliceHeader)) <> SizeOf(DiskSliceHeader) then
      SourceIsCorrupted('Invalid slice header (3)');
    if FSourceF.Size.Lo <> DiskSliceHeader.TotalSize then
      SourceIsCorrupted('Invalid slice header (4)');
  end;
  FOpenedSlice := ASlice;
end;

procedure TFileExtractor.DecompressBytes(var Buffer; Count: Cardinal);
begin
  try
    FDecompressor[FChunkCompressed].DecompressInto(Buffer, Count);
  except
    { If DecompressInto raises an exception, force a decompressor reset &
      re-seek the next time SeekTo is called by setting FNeedReset to True.
      We don't want to get stuck in an endless loop with the decompressor
      in e.g. a data error state. Also, we have no way of knowing if
      DecompressInto successfully decompressed some of the requested bytes
      before the exception was raised. }
    FNeedReset := True;
    raise;
  end;
  Inc64(FChunkDecompressedBytesRead, Count);
end;

procedure TFileExtractor.SeekTo(const FL: TSetupFileLocationEntry;
  const ProgressProc: TExtractorProgressProc);

  procedure InitDecryption;
  begin
    { Recreate the unique nonce from the base nonce }
    var Nonce := SetupHeader.EncryptionBaseNonce;
    Nonce.RandomXorStartOffset := Nonce.RandomXorStartOffset xor FChunkStartOffset;
    Nonce.RandomXorFirstSlice := Nonce.RandomXorFirstSlice xor FChunkFirstSlice;

    XChaCha20Init(FCryptContext, FCryptKey[0], Length(FCryptKey), Nonce, SizeOf(Nonce), 0);
  end;

  procedure Discard(Count: Integer64);
  var
    Buf: array[0..65535] of Byte;
    BufSize: Cardinal;
  begin
    try
      while True do begin
        BufSize := SizeOf(Buf);
        if (Count.Hi = 0) and (Count.Lo < BufSize) then
          BufSize := Count.Lo;
        if BufSize = 0 then
          Break;
        DecompressBytes(Buf, BufSize);
        Dec64(Count, BufSize);
        if Assigned(ProgressProc) then
          ProgressProc(0);
      end;
    except
      on E: ECompressDataError do
        SourceIsCorrupted(E.Message);
    end;
  end;

var
  TestCompID: TCompID;
  Diff: Integer64;
begin
  if FEntered <> 0 then
    InternalError('Cannot call file extractor recursively');
  Inc(FEntered);
  try
    if (foChunkEncrypted in FL.Flags) and not FCryptKeySet then
      InternalError('Cannot read an encrypted file before the key has been set');

    { Is the file in a different chunk than the current one?
      Or, is the file in a part of the current chunk that we've already passed?
      Or, did a previous decompression operation fail, necessitating a reset? }
    if (FChunkFirstSlice <> FL.FirstSlice) or
       (FChunkStartOffset <> FL.StartOffset) or
       (Compare64(FL.ChunkSuboffset, FChunkDecompressedBytesRead) < 0) or
       FNeedReset then begin
      FChunkFirstSlice := -1;
      FDecompressor[foChunkCompressed in FL.Flags].Reset;
      FNeedReset := False;

      OpenSlice(FL.FirstSlice);

      FSourceF.Seek(SetupLdrOffset1 + FL.StartOffset);
      if FSourceF.Read(TestCompID, SizeOf(TestCompID)) <> SizeOf(TestCompID) then
        SourceIsCorrupted('Failed to read CompID');
      if Longint(TestCompID) <> Longint(ZLIBID) then
        SourceIsCorrupted('Invalid CompID');

      FChunkFirstSlice := FL.FirstSlice;
      FChunkLastSlice := FL.LastSlice;
      FChunkStartOffset := FL.StartOffset;
      FChunkBytesLeft := FL.ChunkCompressedSize;
      FChunkDecompressedBytesRead.Hi := 0;
      FChunkDecompressedBytesRead.Lo := 0;
      FChunkCompressed := foChunkCompressed in FL.Flags;
      FChunkEncrypted := foChunkEncrypted in FL.Flags;

      if foChunkEncrypted in FL.Flags then
        InitDecryption;
    end;

    { Need to seek forward in the chunk? }
    if Compare64(FL.ChunkSuboffset, FChunkDecompressedBytesRead) > 0 then begin
      Diff := FL.ChunkSuboffset;
      Dec6464(Diff, FChunkDecompressedBytesRead);
      Discard(Diff);
    end;
  finally
    Dec(FEntered);
  end;
end;

function TFileExtractor.ReadProc(var Buf; Count: Longint): Longint;
var
  Buffer: Pointer;
  Left, Res: Cardinal;
begin
  Buffer := @Buf;
  Left := Count;
  if (FChunkBytesLeft.Hi = 0) and (FChunkBytesLeft.Lo < Left) then
    Left := FChunkBytesLeft.Lo;
  Result := Left;
  while Left <> 0 do begin
    Res := FSourceF.Read(Buffer^, Left);
    Dec64(FChunkBytesLeft, Res);

    { Decrypt the data after reading from the file }
    if FChunkEncrypted then
      XChaCha20Crypt(FCryptContext, Buffer^, Buffer^, Res);

    if Left = Res then
      Break
    else begin
      Dec(Left, Res);
      Inc(Longint(Buffer), Res);
      { Go to next disk }
      if FOpenedSlice >= FChunkLastSlice then
        { Already on the last slice, so the file must be corrupted... }
        SourceIsCorrupted('Already on last slice');
      OpenSlice(FOpenedSlice + 1);
    end;
  end;
end;

procedure TFileExtractor.DecompressFile(const FL: TSetupFileLocationEntry;
  const DestF: TFile; const ProgressProc: TExtractorProgressProc;
  const VerifyChecksum: Boolean);
var
  BytesLeft: Integer64;
  Context: TSHA256Context;
  AddrOffset: LongWord;
  BufSize: Cardinal;
  Buf: array[0..65535] of Byte;
  { ^ *must* be the same buffer size used by the compiler (TCompressionHandler),
    otherwise the TransformCallInstructions call will break }
begin
  if FEntered <> 0 then
    InternalError('Cannot call file extractor recursively');
  Inc(FEntered);
  try
    BytesLeft := FL.OriginalSize;

    { To avoid file system fragmentation, preallocate all of the bytes in the
      destination file }
    DestF.Seek64(BytesLeft);
    DestF.Truncate;
    DestF.Seek(0);

    SHA256Init(Context);

    try
      AddrOffset := 0;
      while True do begin
        BufSize := SizeOf(Buf);
        if (BytesLeft.Hi = 0) and (BytesLeft.Lo < BufSize) then
          BufSize := BytesLeft.Lo;
        if BufSize = 0 then
          Break;

        DecompressBytes(Buf, BufSize);
        if foCallInstructionOptimized in FL.Flags then begin
          TransformCallInstructions(Buf, BufSize, False, AddrOffset);
          Inc(AddrOffset, BufSize);  { may wrap, but OK }
        end;
        Dec64(BytesLeft, BufSize);
        SHA256Update(Context, Buf, BufSize);
        DestF.WriteBuffer(Buf, BufSize);

        if Assigned(ProgressProc) then
          ProgressProc(BufSize);
      end;
    except
      on E: ECompressDataError do
        SourceIsCorrupted(E.Message);
    end;

    if VerifyChecksum and not SHA256DigestsEqual(SHA256Final(Context), FL.SHA256Sum) then
      SourceIsCorrupted('SHA-256 hash mismatch');
  finally
    Dec(FEntered);
  end;
end;

end.
