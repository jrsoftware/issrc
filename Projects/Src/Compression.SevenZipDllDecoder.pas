unit Compression.SevenZipDllDecoder;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Interface to the 7-Zip 7z(xa).dll Decoder DLL's, used by Setup

  Based on the 7-zip source code and the 7-zip Delphi API by Henri Gourvest
  https://github.com/geoffsmith82/d7zip MPL 1.1 licensed
}

interface

uses
  Compression.SevenZipDecoder;

procedure InitSevenZipLibrary(const DllFilename: String);

procedure ExtractArchiveRedir(const DisableFsRedir: Boolean;
  const ArchiveFilename, DestDir, Password: String; const FullPaths: Boolean;
  const OnExtractionProgress: TOnExtractionProgress);

implementation

uses
  Classes, SysUtils, Forms,
  Windows, ActiveX,
  Compression.SevenZipDllDecoder.Interfaces, PathFunc,
  Shared.FileClass, Shared.Int64Em, Shared.SetupMessageIDs, Shared.CommonFunc,
  SetupLdrAndSetup.Messages, SetupLdrAndSetup.RedirFunc,
  Setup.LoggingFunc, Setup.MainFunc, Setup.InstFunc;

type
  TInStream = class(TInterfacedObject, IInStream)
  private
    FFile: TFile;
  protected
    function Read(data: Pointer; size: UInt32; processedSize: PUInt32): HRESULT; stdcall;
    function Seek(offset: Int64; seekOrigin: UInt32; newPosition: PUInt64): HRESULT; stdcall;
  public
    constructor Create(AFile: TFile);
    destructor Destroy; override;
  end;

  TSequentialOutStream = class(TInterfacedObject, ISequentialOutStream)
  private
    FFile: TFile;
  protected
    function Write(data: Pointer; size: UInt32; processedSize: PUInt32): HRESULT; stdcall;
  public
    constructor Create(AFile: TFile);
    destructor Destroy; override;
  end;

  TArchiveOpenCallback = class(TInterfacedObject, IArchiveOpenCallback,
    ICryptoGetTextPassword)
  private
    FPassword: String;
  protected
    { IArchiveOpenCallback }
    function SetTotal(files, bytes: PUInt64): HRESULT; stdcall;
    function SetCompleted(files, bytes: PUInt64): HRESULT; stdcall;
    { ICryptoGetTextPassword - queried for on openCallback }
    function CryptoGetTextPassword(out password: TBStr): HRESULT; stdcall;
  public
    constructor Create(const Password: String);
  end;

  TArchiveExtractCallback = class(TInterfacedObject, IArchiveExtractCallback,
    ICryptoGetTextPassword)
  private
    FInArchive: IInArchive;
    FDisableFsRedir: Boolean;
    FExpandedDestDir, FPassword: String;
    FFullPaths: Boolean;
    FExtractedArchiveName: String;
    FOnExtractionProgress: TOnExtractionProgress;
    FCurrentFilename, FLastReportedFilename: String;
    FProgress, FProgressMax, FLastReportedProgress, FLastReportedProgressMax: UInt64;
    FOpRes: TNOperationResult;
  protected
    { IProgress }
    function SetTotal(total: UInt64): HRESULT; stdcall;
    function SetCompleted(completeValue: PUInt64): HRESULT; stdcall;
    { IArchiveExtractCallback }
    function GetStream(index: UInt32; out outStream: ISequentialOutStream;
      askExtractMode: Int32): HRESULT; stdcall;
    function PrepareOperation(askExtractMode: Int32): HRESULT; stdcall;
    function SetOperationResult(opRes: TNOperationResult): HRESULT; stdcall;
    { ICryptoGetTextPassword - queried for on extractCallback }
    function CryptoGetTextPassword(out password: TBStr): HRESULT; stdcall;
  public
    constructor Create(const InArchive: IInArchive;
      const DisableFsRedir: Boolean; const ArchiveFileName, DestDir, Password: String;
      const FullPaths: Boolean; const OnExtractionProgress: TOnExtractionProgress);
    property OpRes: TNOperationResult read FOpRes;
  end;

function SevenZipSetPassword(const Password: String; out outPassword: TBStr): HRESULT;
begin
  try
    if Password = '' then Exit(S_FALSE);
    outPassword := SysAllocString(PChar(Password));
    if outPassword = nil then Exit(E_OUTOFMEMORY);
    Result := S_OK;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end;
end;

{ TInStream }

constructor TInStream.Create(AFile: TFile);
begin
  inherited Create;
  FFile := AFile;
end;

destructor TInStream.Destroy;
begin
  FFile.Free;
  inherited;
end;

function TInStream.Read(data: Pointer; size: UInt32;
  processedSize: PUInt32): HRESULT;
begin
  try
    var BytesRead := FFile.Read(data^, size);
    if processedSize <> nil then
      processedSize^ := BytesRead;
    Result := S_OK;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end;
end;

function TInStream.Seek(offset: Int64; seekOrigin: UInt32;
  newPosition: PUInt64): HRESULT;
begin
  try
    case seekOrigin of
      STREAM_SEEK_SET: FFile.Seek64(Integer64(offset));
      STREAM_SEEK_CUR: FFile.Seek64(Integer64(Int64(FFile.Position) + offset));
      STREAM_SEEK_END: FFile.Seek64(Integer64(Int64(FFile.Size) + offset));
    end;
    if newPosition <> nil then
      newPosition^ := UInt64(FFile.Position);
    Result := S_OK;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end;
end;

{ TSequentialOutStream }

constructor TSequentialOutStream.Create(AFile: TFile);
begin
  inherited Create;
  FFile := AFile;
end;

destructor TSequentialOutStream.Destroy;
begin
  FFile.Free;
  inherited;
end;

function TSequentialOutStream.Write(data: Pointer; size: UInt32;
  processedSize: PUInt32): HRESULT;
begin
  try
    FFile.WriteBuffer(data^, size);
    if processedSize <> nil then
      processedSize^ := size;
    Result := S_OK;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end;
end;

{ TArchiveOpenCallback }

constructor TArchiveOpenCallback.Create(const Password: String);
begin
  inherited Create;
  FPassword := Password;
end;

function TArchiveOpenCallback.SetCompleted(files,
  bytes: PUInt64): HRESULT;
begin
  Result := S_OK;
end;

function TArchiveOpenCallback.SetTotal(files,
  bytes: PUInt64): HRESULT;
begin
  Result := S_OK;
end;

function TArchiveOpenCallback.CryptoGetTextPassword(
  out password: TBStr): HRESULT;
begin
  { Note: have not yet seen 7-Zip actually call this, so maybe it's not really needed }
  Result := SevenZipSetPassword(FPassword, password);
end;

{ TArchiveExtractCallback }

constructor TArchiveExtractCallback.Create(const InArchive: IInArchive;
  const DisableFsRedir: Boolean; const ArchiveFileName, DestDir, Password: String;
  const FullPaths: Boolean; const OnExtractionProgress: TOnExtractionProgress);
begin
  inherited Create;
  FInArchive := InArchive;
  FDisableFsRedir := DisableFsRedir;
  FExpandedDestDir := AddBackslash(PathExpand(DestDir));
  FPassword := Password;
  FFullPaths := FullPaths;
  FExtractedArchiveName := PathExtractName(ArchiveFileName);
  FOnExtractionProgress := OnExtractionProgress;
end;

function TArchiveExtractCallback.SetTotal(total: UInt64): HRESULT;
begin
  { From IArchive.h: 7-Zip can call functions for IProgress or ICompressProgressInfo functions
    from another threads simultaneously with calls for IArchiveExtractCallback interface }
  InterlockedExchange64(Int64(FProgressMax), Int64(total));
  Result := S_OK;
end;

function TArchiveExtractCallback.SetCompleted(completeValue: PUInt64): HRESULT;
begin
  InterlockedExchange64(Int64(FProgress), Int64(completeValue^));
  Result := S_OK;
end;

function TArchiveExtractCallback.GetStream(index: UInt32;
  out outStream: ISequentialOutStream; askExtractMode: Int32): HRESULT;
begin
  try
    FCurrentFilename := '';
    if askExtractMode = kExtract then begin
      var ItemPath: OleVariant;
      var Res := FInArchive.GetProperty(index, kpidPath, ItemPath);
      if Res <> S_OK then Exit(Res);
      if ItemPath = '' then Exit(E_FAIL);
      var IsDir: OleVariant;
      Res := FInArchive.GetProperty(index, kpidIsDir, IsDir);
      if Res <> S_OK then Exit(Res);
      if IsDir then begin
        if FFullPaths then begin
          FCurrentFilename := ItemPath + '\';
          var ExpandedDir: String;
          if not ValidateAndCombinePath(FExpandedDestDir, ItemPath, ExpandedDir) then Exit(E_ACCESSDENIED);
          ForceDirectoriesRedir(FDisableFsRedir, ExpandedDir);
        end;
        outStream := nil;
      end else begin
        if not FFullPaths then
          ItemPath := PathExtractName(ItemPath);
        FCurrentFilename := ItemPath;
        var ExpandedFileName: String;
        if not ValidateAndCombinePath(FExpandedDestDir, ItemPath, ExpandedFileName) then Exit(E_ACCESSDENIED);
        ForceDirectoriesRedir(FDisableFsRedir, PathExtractPath(ExpandedFileName));
        { From IArchive.h: can also set outstream to nil to tell 7zip to skip the file }
        outstream := TSequentialOutStream.Create(TFileRedir.Create(FDisableFsRedir, ExpandedFileName, fdCreateAlways, faWrite, fsNone));
      end;
    end;
    Result := S_OK;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end;
end;

function TArchiveExtractCallback.PrepareOperation(askExtractMode: Int32): HRESULT;
begin
  { From Client7z.cpp: PrepareOperation is called *after* GetStream has been called
    From IArchive.h: 7-Zip doesn't call GetStream/PrepareOperation/SetOperationResult from different threads simultaneously }
  try
    var Abort := False;

    TThread.Synchronize(nil, procedure
      begin
        if FCurrentFilename <> '' then begin
          if FCurrentFilename <> FLastReportedFilename then begin
            LogFmt('- %s', [FCurrentFilename]); { Just like 7zMain.c }
            FLastReportedFilename := FCurrentFilename;
          end;

          if Assigned(FOnExtractionProgress) then begin
            { Make sure script isn't called crazy often because that would slow the extraction significantly. Only report:
              -At start or finish
              -Or if somehow Progress decreased or Max changed
              -Or if at least 512 KB progress was made since last report
            }
            if (FProgress = 0) or (FProgress = FProgressMax) or
               (FProgress < FLastReportedProgress) or (FProgressMax <> FLastReportedProgressMax) or
               ((FProgress - FLastReportedProgress) > 524288) then begin
              try
                if not FOnExtractionProgress(FExtractedArchiveName, FCurrentFilename, FProgress, FProgressMax) then
                  Abort := True;
              finally
                FLastReportedProgress := FProgress;
                FLastReportedProgressMax := FProgressMax;
              end;
            end;
          end;
        end;

        if not Abort and DownloadTemporaryFileOrExtract7ZipArchiveProcessMessages then
          Application.ProcessMessages;
      end);

    if Abort then
      SysUtils.Abort;

    Result := S_OK;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end;
end;

function TArchiveExtractCallback.SetOperationResult(opRes: TNOperationResult): HRESULT;

  function OperationResultToString(const opRes: TNOperationResult): String;
  begin
    case opRes of
      kOK: Result := 'OK';
      kUnsupportedMethod: Result := 'Unsupported method';
      kDataError: Result := 'Data error';
      kCRCError: Result := 'CRC error';
      kUnavailable: Result := 'Unavailable';
      kUnexpectedEnd: Result := 'Unexpected end';
      kDataAfterEnd: Result := 'Data after end';
      kIsNotArc: Result := 'Is not an archive';
      kHeadersError: Result := 'Headers error';
      kWrongPassword: Result := 'Wrong password';
    else
      Result := Format('Unknown result %d', [Ord(opRes)]);
    end;
  end;

begin
  { From IArchive.h: Can now can close the file, set attributes, timestamps and security information }
  if opRes <> kOK then begin
    FOpRes := opRes;
    LogFmt('ERROR: %s', [OperationResultToString(opRes)]);  { Just like 7zMain.c }
  end;
  Result := S_OK;
end;

function TArchiveExtractCallback.CryptoGetTextPassword(
  out password: TBStr): HRESULT;
begin
  Result := SevenZipSetPassword(FPassword, password);
end;

{---}

var
  SevenZipLibrary: THandle;
  CreateSevenZipObject: function(const clsid, iid: TGUID; var outObject): HRESULT; stdcall;

procedure FreeSevenZipLibrary;
begin
  if SevenZipLibrary <> 0 then begin
    FreeLibrary(SevenZipLibrary);
    SevenZipLibrary := 0;
    CreateSevenZipObject := nil;
  end;
end;

procedure InitSevenZipLibrary(const DllFilename: String);
begin
  if SevenZipLibrary = 0 then begin
    SevenZipLibrary := LoadLibrary(PChar(DllFilename));
    if SevenZipLibrary = 0 then
      Win32ErrorMsg('LoadLibrary');
    CreateSevenZipObject := GetProcAddress(SevenZipLibrary, 'CreateObject');
    if not Assigned(CreateSevenZipObject) then begin
      var LastError := GetLastError;
      FreeSevenZipLibrary;
      Win32ErrorMsgEx('GetProcAddress', LastError);
    end;
  end;
end;

procedure ExtractArchiveRedir(const DisableFsRedir: Boolean;
  const ArchiveFilename, DestDir, Password: String;
  const FullPaths: Boolean; const OnExtractionProgress: TOnExtractionProgress);

  function GetHandler(const Ext: String): TGUID;
  begin
    if SameText(Ext, '.zip') then
      Result := CLSID_HandlerZip
    else if SameText(Ext, '.7z') then
      Result := CLSID_Handler7z
    else if SameText(Ext, '.rar') then
      Result := CLSID_HandlerRar
    else if SameText(Ext, '.bzip2') then
      Result := CLSID_HandlerBZip2
    else if SameText(Ext, '.tar') then
      Result := CLSID_HandlerTar
    else if SameText(Ext, '.gzip') then
      Result := CLSID_HandlerGzip
    else if SameText(Ext, '.iso') then
      Result := CLSID_HandlerIso
    else if SameText(Ext, '.cab') then
      Result := CLSID_HandlerCab
    else if SameText(Ext, '.lzma') then
      Result := CLSID_HandlerLzma
    else if SameText(Ext, '.wim') then
      Result := CLSID_HandlerWim
    else if SameText(Ext, '.rpm') then
      Result := CLSID_HandlerRpm
    else if SameText(Ext, '.deb') then
      Result := CLSID_HandlerDeb
    else
      InternalError('ExtractArchive: Unknown ArchiveFileName extension');
  end;

begin
  if not Assigned(CreateSevenZipObject) then
    InternalError('ExtractArchive: 7z(xa).dll not loaded');
  if ArchiveFileName = '' then
    InternalError('ExtractArchive: Invalid ArchiveFileName value');
  const clsid = GetHandler(PathExtractExt(ArchiveFilename));
  if DestDir = '' then
    InternalError('ExtractArchive: Invalid DestDir value');

  LogFmt('Extracting archive %s to %s. Full paths? %s', [ArchiveFileName, DestDir, SYesNo[FullPaths]]);

  { CreateObject }
  var InArchive: IInArchive;
  if CreateSevenZipObject(clsid, IInArchive, InArchive) <> S_OK then
    raise Exception.Create(FmtSetupMessage(msgErrorExtractionFailed, ['Cannot get class object'])); { From Client7z.cpp }

  { Open }
  const InStream: IInStream =
    TInStream.Create(TFileRedir.Create(DisableFsRedir, ArchiveFilename, fdOpenExisting, faRead, fsRead));
  var ScanSize: Int64 := 1 shl 23; { From Client7z.cpp }
  const OpenCallback: IArchiveOpenCallback = TArchiveOpenCallback.Create(Password);
  if InArchive.Open(InStream, @ScanSize, OpenCallback) <> S_OK then
    raise Exception.Create(FmtSetupMessage(msgErrorExtractionFailed, ['Cannot open file as archive'])); { From Client7z.cpp }

  { Extract }
  const ExtractCallback: IArchiveExtractCallback =
    TArchiveExtractCallback.Create(InArchive, DisableFsRedir,
      ArchiveFilename, DestDir, Password, FullPaths, OnExtractionProgress);
  const Res = InArchive.Extract(nil, $FFFFFFFF, 0, ExtractCallback);
  if Res = E_ABORT then
    raise Exception.Create(SetupMessages[msgErrorExtractionAborted])
  else if Res <> S_OK then
    raise Exception.Create(FmtSetupMessage(msgErrorExtractionFailed, [Format('%s %s', [Win32ErrorString(Res), IntToHexStr8(Res)])]));
  var OpRes := (ExtractCallback as TArchiveExtractCallback).OpRes;
  if OpRes <> kOK then
    raise Exception.Create(FmtSetupMessage(msgErrorExtractionFailed, [Ord(OpRes).ToString]));

  Log('Everything is Ok'); { Just like 7zMain.c }
end;

initialization

finalization
  FreeSevenZipLibrary;

end.