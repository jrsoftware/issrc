unit Extract;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TFileExtractor class

  $jrsoftware: issrc/Projects/Extract.pas,v 1.30 2010/03/13 18:51:37 jr Exp $
}

interface

uses
  Windows, SysUtils, Int64Em, FileClass, Compress, Struct, ArcFour;

type
  TExtractorProgressProc = procedure(Bytes: Cardinal);
  TExtractorDownloadWebFileProc = procedure(const WebFilename, Description, DestFilename: String);

  TFileExtractor = class
  private
    FDecompressor: array[Boolean] of TCustomDecompressor;
    FSourceF: TFile;
    FOpenedSlice, FChunkFirstSlice, FChunkLastSlice: Integer;
    FPackageIndex: Smallint;
    FChunkStartOffset: Longint;
    FChunkBytesLeft, FChunkDecompressedBytesRead: Integer64;
    FNeedReset: Boolean;
    FChunkCompressed, FChunkEncrypted: Boolean;
    FCryptContext: TArcFourContext;
    FCryptKey: String;
    FEntered: Integer;
    FSetupPackageOffset: Longint;
    FDownloadWebFileProc: TExtractorDownloadWebFileProc;
    procedure DecompressBytes(var Buffer; Count: Cardinal);
    class function FindSliceFilename(const PackageFilename: String; const ASlice: Integer): String;
    procedure OpenSlice(const APackageIndex: Smallint; const ASlice: Integer);
    function ReadProc(var Buf; Count: Longint): Longint;
    function TestOpenPackageFile(const Filename: String; PackageEntry: PSetupPackageEntry;
      IsSlice: Boolean): TFile;
  public
    constructor Create(ADecompressorClass: TCustomDecompressorClass);
    destructor Destroy; override;
    procedure DecompressFile(const FL: TSetupFileLocationEntry; const DestF: TFile;
      const ProgressProc: TExtractorProgressProc; const VerifyChecksum: Boolean);
    procedure SeekTo(const FL: TSetupFileLocationEntry;
      const ProgressProc: TExtractorProgressProc;
      const DownloadWebFileProc: TExtractorDownloadWebFileProc);
    class function ReadPackageHeader(PackageEntry: PSetupPackageEntry; F: TFile;
      IsSlice, RaiseExceptions: Boolean): Boolean;
    property CryptKey: String write FCryptKey;
  end;

function FileExtractor: TFileExtractor;
procedure FreeFileExtractor;

implementation

uses
  PathFunc, CmnFunc, CmnFunc2, Main, Msgs, MsgIDs, InstFunc, CompressZlib, bzlib,
  LZMADecomp, SHA1, Logging, NewDisk;

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

var
  LastSourceDir: String;

class function TFileExtractor.FindSliceFilename(const PackageFilename: String;
  const ASlice: Integer): String;
var
  Major, Minor: Integer;
  Prefix, F1, F2, Path, Suffix: String;
begin
  if PackageFilename = '' then begin
    Prefix := PathChangeExt(PathExtractName(SetupLdrOriginalFilename), '');
    Suffix := '.bin';
  end
  else begin
    Prefix := PathChangeExt(PackageFilename, '');
    Suffix := PathExtractExt(PackageFilename);
  end;
  Major := ASlice div SetupHeader.SlicesPerDisk + 1;
  Minor := ASlice mod SetupHeader.SlicesPerDisk;
  if SetupHeader.SlicesPerDisk = 1 then
    F1 := Format('%s-%d%s', [Prefix, Major, Suffix])
  else
    F1 := Format('%s-%d%s%s', [Prefix, Major, Chr(Ord('a') + Minor), Suffix]);

  if (PackageFilename <> '') and IsWebPackage(PackageFilename) then begin
    { A web package must not exist at this time }
    Result := F1;
    Exit;
  end;

  F2 := Format('..\DISK%d\', [Major]) + F1;
  if LastSourceDir <> '' then begin
    Result := AddBackslash(LastSourceDir) + F1;
    if NewFileExists(Result) then Exit;
  end;
  Result := AddBackslash(SourceDir) + F1;
  if NewFileExists(Result) then Exit;
  if LastSourceDir <> '' then begin
    Result := PathExpand(AddBackslash(LastSourceDir) + F2);
    if NewFileExists(Result) then Exit;
  end;
  Result := PathExpand(AddBackslash(SourceDir) + F2);
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

{ ReadPackageHeader reads and validates the header of a local/web package. If it
  can read the header successfully, it returns the StartOffset of the compressed
  data. }
class function TFileExtractor.ReadPackageHeader(PackageEntry: PSetupPackageEntry; F: TFile;
  IsSlice, RaiseExceptions: Boolean): Boolean;

  procedure AbortPackageInit(const Msg: TSetupMessageID);
  begin
    if RaiseExceptions then
      LoggedMsgBox(SetupMessages[Msg], '', mbCriticalError, MB_OK, True, IDOK);
    Abort;
  end;

 var
  PackageHeader: TSetupPackageHeader;
  TestID: TSetupID;
  SliceID: TDiskSliceID;
begin
  Result := True;
  try
    if IsSlice then begin
      if F.Read(SliceID, SizeOf(SliceID)) <> SizeOf(SliceID) then
        AbortPackageInit(msgSetupFileCorruptOrWrongVer);
      if SliceID <> DiskSliceID then
        AbortPackageInit(msgSetupFileCorruptOrWrongVer);
    end;
    if F.Read(TestID, SizeOf(TestID)) <> SizeOf(TestID) then
      AbortPackageInit(msgSetupFileCorruptOrWrongVer);
    if TestID <> SetupPackageID then
      AbortPackageInit(msgSetupFileCorruptOrWrongVer);
    if F.Read(PackageHeader, SizeOf(PackageHeader)) <> SizeOf(PackageHeader) then
      AbortPackageInit(msgSetupFileCorrupt);;
    if not CompareMem(@PackageHeader.PackageGuid, @PackageEntry.PackageGuid, SizeOf(TGUID)) then
      AbortPackageInit(msgSetupFileCorruptOrWrongVer);
    if PackageHeader.TotalSize <> F.Size.Lo then
      AbortPackageInit(msgSetupFileCorrupt);;
  except
    if RaiseExceptions then
      raise;
    Result := False;
  end;
end;

function TFileExtractor.TestOpenPackageFile(const Filename: String;
  PackageEntry: PSetupPackageEntry; IsSlice: Boolean): TFile;
begin
  Result := nil;
  if NewFileExists(Filename) then begin
    try
      Result := TFile.Create(Filename, fdOpenExisting, faRead, fsRead);
      if not ReadPackageHeader(PackageEntry, Result, IsSlice, False) then
        FreeAndNil(Result);
    except
      Result.Free;
      Result := nil;
    end;
  end;
end;

procedure TFileExtractor.OpenSlice(const APackageIndex: Smallint; const ASlice: Integer);
var
  Filename, PkgFileName: String;
  IsSlice: Boolean;
  TestDiskSliceID: TDiskSliceID;
  DiskSliceHeader: TDiskSliceHeader;
  PackageEntry: PSetupPackageEntry;
begin
  if (FPackageIndex = APackageIndex) and (FOpenedSlice = ASlice) then
    Exit;

  FOpenedSlice := -1;
  FreeAndNil(FSourceF);

  if APackageIndex > 0 then begin
    FSetupPackageOffset := -1; // for sanity
    IsSlice := ASlice > 0;
    { Get package filename and adjust to installer directory }
    PackageEntry := PSetupPackageEntry(Entries[sePackage][APackageIndex - 1]);
    Filename := PackageEntry.SourceFilename;

    if IsSlice then
      Filename := FindSliceFilename(Filename, ASlice);

    if not IsWebPackage(Filename) then begin
      if not IsSlice then
        Filename := PathExpand(AddBackslash(SourceDir) + Filename);
    end
    else begin
      { The web package may have a local copy, so test for it }
      PkgFilename := Filename;

      Filename := PathExpand(AddBackslash(SourceDir) + ExtractWebFilename(PkgFilename));
      FSourceF := TestOpenPackageFile(Filename, PackageEntry, IsSlice);
      if FSourceF = nil then begin
        { The web package may have been already downloaded to <tmp> }
        Filename := PathExpand(AddBackslash(TempInstallDir) + ExtractWebFilename(PkgFilename));
        FSourceF := TestOpenPackageFile(Filename, PackageEntry, IsSlice);

        if (FSourceF = nil) and Assigned(FDownloadWebFileProc) then begin
          PkgFilename := PathExpand(AddBackslash(SourceDir) + ExtractWebFilename(PkgFilename)); // prefered directory is {src}
          { (Always) download to <tmp> }
          FDownloadWebFileProc(PackageEntry.SourceFilename, PackageEntry.Description, Filename);

          if poLocalCopy in PackageEntry.Options then begin
            { The file should be used as a local copy, so move it from <tmp> to <src> if possible }
            if MoveFile(PChar(Filename), PChar(PkgFilename)) then
              Filename := PkgFilename;
          end;
        end;
      end;
    end;

    if (FSourceF = nil) and not NewFileExists(Filename) then
      raise Exception.Create(FmtSetupMessage1(msgSetupFileMissing, ExtractWebFileName(PackageEntry.SourceFilename)));

    if FSourceF = nil then begin
      { Open the file and validate the header }
      FSourceF := TFile.Create(Filename, fdOpenExisting, faRead, fsRead);
      ReadPackageHeader(PackageEntry, FSourceF, IsSlice, True);
    end;
    FSetupPackageOffset := FSourceF.Position.Lo;
  end
  else begin
    if SetupLdrOffset1 = 0 then
      Filename := FindSliceFilename('', ASlice)
    else begin
      Filename := SetupLdrOriginalFilename;
    end;
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
  const ProgressProc: TExtractorProgressProc;
  const DownloadWebFileProc: TExtractorDownloadWebFileProc);

  procedure InitDecryption;
  var
    Salt: TSetupSalt;
    Context: TSHA1Context;
    Hash: TSHA1Digest;
  begin
    { Read the salt }
    if FSourceF.Read(Salt, SizeOf(Salt)) <> SizeOf(Salt) then
      SourceIsCorrupted('Failed to read salt');

    { Initialize the key, which is the SHA-1 hash of the salt plus FCryptKey }
    SHA1Init(Context);
    SHA1Update(Context, Salt, SizeOf(Salt));
    SHA1Update(Context, Pointer(FCryptKey)^, Length(FCryptKey)*SizeOf(FCryptKey[1]));
    Hash := SHA1Final(Context);
    ArcFourInit(FCryptContext, Hash, SizeOf(Hash));

    { The compiler discards the first 1000 bytes for extra security,
      so we must as well }
    ArcFourDiscard(FCryptContext, 1000);
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
    if (foChunkEncrypted in FL.Flags) and (FCryptKey = '') then
      InternalError('Cannot read an encrypted file before the key has been set');
    FDownloadWebFileProc := DownloadWebFileProc;

    { Is the file in a different chunk than the current one?
      Or, is the file in a part of the current chunk that we've already passed?
      Or, did a previous decompression operation fail, necessitating a reset? }
    if (FChunkFirstSlice <> FL.FirstSlice) or
       (FChunkStartOffset <> FL.StartOffset) or
       (FPackageIndex <> FL.PackageIndex) or
       (Compare64(FL.ChunkSuboffset, FChunkDecompressedBytesRead) < 0) or
       FNeedReset then begin
      FChunkFirstSlice := -1;
      FDecompressor[foChunkCompressed in FL.Flags].Reset;
      FNeedReset := False;

      OpenSlice(FL.PackageIndex, FL.FirstSlice);

      if FL.PackageIndex = 0 then
        FSourceF.Seek(SetupLdrOffset1 + FL.StartOffset)
      else
        FSourceF.Seek(FSetupPackageOffset + FL.StartOffset);

      if FSourceF.Read(TestCompID, SizeOf(TestCompID)) <> SizeOf(TestCompID) then
        SourceIsCorrupted('Failed to read CompID');
      if Longint(TestCompID) <> Longint(ZLIBID) then
        SourceIsCorrupted('Invalid CompID');
      if foChunkEncrypted in FL.Flags then
        InitDecryption;

      FChunkFirstSlice := FL.FirstSlice;
      FChunkLastSlice := FL.LastSlice;
      FChunkStartOffset := FL.StartOffset;
      FChunkBytesLeft := FL.ChunkCompressedSize;
      FChunkDecompressedBytesRead.Hi := 0;
      FChunkDecompressedBytesRead.Lo := 0;
      FChunkCompressed := foChunkCompressed in FL.Flags;
      FChunkEncrypted := foChunkEncrypted in FL.Flags;
      FPackageIndex := FL.PackageIndex;
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
      ArcFourCrypt(FCryptContext, Buffer^, Buffer^, Res);

    if Left = Res then
      Break
    else begin
      Dec(Left, Res);
      Inc(Longint(Buffer), Res);
      { Go to next disk }
      if FOpenedSlice >= FChunkLastSlice then
        { Already on the last slice, so the file must be corrupted... }
        SourceIsCorrupted('Already on last slice');
      OpenSlice(FPackageIndex, FOpenedSlice + 1);
    end;
  end;
end;

procedure TFileExtractor.DecompressFile(const FL: TSetupFileLocationEntry;
  const DestF: TFile; const ProgressProc: TExtractorProgressProc;
  const VerifyChecksum: Boolean);
var
  BytesLeft: Integer64;
  Context: TSHA1Context;
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

    SHA1Init(Context);

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
        SHA1Update(Context, Buf, BufSize);
        DestF.WriteBuffer(Buf, BufSize);

        if Assigned(ProgressProc) then
          ProgressProc(BufSize);
      end;
    except
      on E: ECompressDataError do
        SourceIsCorrupted(E.Message);
    end;

    if VerifyChecksum and not SHA1DigestsEqual(SHA1Final(Context), FL.SHA1Sum) then
      SourceIsCorrupted('SHA-1 hash mismatch');
  finally
    Dec(FEntered);
  end;
end;

end.
