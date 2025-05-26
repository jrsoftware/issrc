unit Compression.SevenZipDLLDecoder;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Interface to the 7-Zip Decoder DLLs, used by Setup

  Based on the 7-Zip source code and the 7-Zip Delphi API by Henri Gourvest
  https://github.com/geoffsmith82/d7zip MPL 1.1 licensed
}

interface

uses
  Compression.SevenZipDecoder;

function SevenZipDLLInit(const SevenZipLibrary: HMODULE): Boolean;

procedure ExtractArchiveRedir(const DisableFsRedir: Boolean;
  const ArchiveFilename, DestDir, Password: String; const FullPaths: Boolean;
  const OnExtractionProgress: TOnExtractionProgress);

implementation

uses
  Classes, SysUtils, Forms, Variants,
  Windows, ActiveX, ComObj,
  Compression.SevenZipDLLDecoder.Interfaces, PathFunc,
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
    function CryptoGetTextPassword(out password: WideString): HRESULT; stdcall;
  public
    constructor Create(const Password: String);
  end;

  TArchiveExtractCallback = class(TInterfacedObject, IArchiveExtractCallback,
    ICryptoGetTextPassword)
  private
    type
      TCurrent = record
        Path, ExpandedPath: String;
        HasAttrib: Boolean;
        Attrib: DWORD;
        procedure SetAttrib(const AAttrib: DWORD);
      end;
      TVarTypeSet = set of varEmpty..varUInt64; { Incomplete but don't need others }
    var
      FInArchive: IInArchive;
      FDisableFsRedir: Boolean;
      FExpandedDestDir, FPassword: String;
      FFullPaths: Boolean;
      FExtractedArchiveName: String;
      FOnExtractionProgress: TOnExtractionProgress;
      FCurrent: TCurrent;
      FLastReportedCurrentPath: String;
      FProgress, FProgressMax, FLastReportedProgress, FLastReportedProgressMax: UInt64;
      FOpRes: TNOperationResult;
    function GetProperty(const index: UInt32; const propID: PROPID;
      const allowedTypes: TVarTypeSet; out value: OleVariant): Boolean; overload;
    function GetProperty(index: UInt32; propID: PROPID; out value: String): Boolean; overload;
    function GetProperty(index: UInt32; propID: PROPID; out value: UInt32): Boolean; overload;
    function GetProperty(index: UInt32; propID: PROPID; out value: Boolean): Boolean; overload;
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
    function CryptoGetTextPassword(out password: WideString): HRESULT; stdcall;
  public
    constructor Create(const InArchive: IInArchive;
      const DisableFsRedir: Boolean; const ArchiveFileName, DestDir, Password: String;
      const FullPaths: Boolean; const OnExtractionProgress: TOnExtractionProgress);
    property OpRes: TNOperationResult read FOpRes;
  end;

function SevenZipSetPassword(const Password: String; out outPassword: WideString): HRESULT;
begin
  try
    if Password = '' then
      Exit(S_FALSE);
    outPassword := Password;
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
  out password: WideString): HRESULT;
begin
  { Note: have not yet seen 7-Zip actually call this, so maybe it's not really needed }
  Result := SevenZipSetPassword(FPassword, password);
end;

{ TArchiveExtractCallback }

procedure TArchiveExtractCallback.TCurrent.SetAttrib(const AAttrib: DWORD);
begin
  Attrib := AAttrib;
  HasAttrib := True;
end;

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
  FOpRes := kOK;
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

function TArchiveExtractCallback.GetProperty(const index: UInt32;
  const propID: PROPID; const allowedTypes: TVarTypeSet; out value: OleVariant): Boolean;
{ Raises an exception on error but otherwise always sets value, returning True if
  it's not empty }
begin
  var Res := FInArchive.GetProperty(index, propID, value);
  if Res <> S_OK then
    OleError(Res);
  Result := not VarIsEmpty(Value);
  if Result and not (VarType(value) in allowedTypes) then
    OleError(E_FAIL);
end;

function TArchiveExtractCallback.GetProperty(index: UInt32; propID: PROPID;
  out value: String): Boolean;
begin
  var varValue: OleVariant;
  Result := GetProperty(index, propID, [varOleStr], varValue);
  value := varValue;
end;

function TArchiveExtractCallback.GetProperty(index: UInt32; propID: PROPID;
  out value: Cardinal): Boolean;
begin
  var varValue: OleVariant;
  Result := GetProperty(index, propID, [varUInt32], varValue);
  value := varValue;
end;

function TArchiveExtractCallback.GetProperty(index: UInt32; propID: PROPID;
  out value: Boolean): Boolean;
begin
  var varValue: OleVariant;
  Result := GetProperty(index, propID, [varBoolean], varValue);
  value := varValue;
end;

function TArchiveExtractCallback.GetStream(index: UInt32;
  out outStream: ISequentialOutStream; askExtractMode: Int32): HRESULT;
begin
  try
    FCurrent := Default(TCurrent);
    if askExtractMode = kExtract then begin
      var Path: String;
      if not GetProperty(index, kpidPath, Path) then
        Path := PathChangeExt(FExtractedArchiveName, '');
      var IsDir: Boolean;
      GetProperty(index, kpidIsDir, IsDir);
      if IsDir then begin
        if FFullPaths then begin
          FCurrent.Path := Path + '\';
          if not ValidateAndCombinePath(FExpandedDestDir, Path, FCurrent.ExpandedPath) then
            OleError(E_ACCESSDENIED);
          ForceDirectories(FDisableFsRedir, FCurrent.ExpandedPath);
        end;
        outStream := nil;
      end else begin
        var Attrib: DWORD;
        if GetProperty(index, kpidAttrib, Attrib) then
          FCurrent.SetAttrib(Attrib);
        if not FFullPaths then
          Path := PathExtractName(Path);
        FCurrent.Path := Path;
        if not ValidateAndCombinePath(FExpandedDestDir, Path, FCurrent.ExpandedPath) then
          OleError(E_ACCESSDENIED);
        ForceDirectories(FDisableFsRedir, PathExtractPath(FCurrent.ExpandedPath));
        { From IArchive.h: can also set outstream to nil to tell 7zip to skip the file }
        outstream := TSequentialOutStream.Create(TFileRedir.Create(FDisableFsRedir, FCurrent.ExpandedPath, fdCreateAlways, faWrite, fsNone));
      end;
    end;
    Result := S_OK;
  except
    on E: EOleSysError do
      Result := E.ErrorCode;
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

  if GetCurrentThreadId <> MainThreadID then
    Exit(E_FAIL);

  try
    var Abort := False;

    if FCurrent.Path <> '' then begin
      if FCurrent.Path <> FLastReportedCurrentPath then begin
        LogFmt('- %s', [FCurrent.Path]); { Just like 7zMain.c }
        FLastReportedCurrentPath := FCurrent.Path;
      end;

      if Assigned(FOnExtractionProgress) then begin
        { Make sure script isn't called crazy often because that would slow the extraction significantly. Only report:
          -At start or finish
          -Or if somehow Progress decreased or Max changed
          -Or if at least 512 KB progress was made since last report
        }
        const Progress = UInt64(InterlockedExchangeAdd64(Int64(FProgress), 0));
        const ProgressMax = UInt64(InterlockedExchangeAdd64(Int64(FProgressMax), 0));
        if (Progress = 0) or (Progress = ProgressMax) or
           (Progress < FLastReportedProgress) or (ProgressMax <> FLastReportedProgressMax) or
           ((Progress - FLastReportedProgress) > 524288) then begin
          try
            if not FOnExtractionProgress(FExtractedArchiveName, FCurrent.Path, Progress, ProgressMax) then
              Abort := True;
          finally
            FLastReportedProgress := Progress;
            FLastReportedProgressMax := ProgressMax;
          end;
        end;
      end;
    end;

    if not Abort and DownloadTemporaryFileOrExtractArchiveProcessMessages then
      Application.ProcessMessages;

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
begin
  { From IArchive.h: Can now can close the file, set attributes, timestamps and security information }
  try
    if opRes <> kOK then begin
      FOpRes := opRes;
      Result := E_FAIL;
    end else begin
      if (FCurrent.ExpandedPath <> '') and FCurrent.HasAttrib and
         not SetFileAttributesRedir(FDisableFsRedir, FCurrent.ExpandedPath, FCurrent.Attrib) then
        Result := E_FAIL
      else
        Result := S_OK;
    end;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end;
end;

function TArchiveExtractCallback.CryptoGetTextPassword(
  out password: WideString): HRESULT;
begin
  Result := SevenZipSetPassword(FPassword, password);
end;

{---}

var
  CreateSevenZipObject: function(const clsid, iid: TGUID; var outObject): HRESULT; stdcall;

function SevenZipDLLInit(const SevenZipLibrary: HMODULE): Boolean;
begin
  CreateSevenZipObject := GetProcAddress(SevenZipLibrary, 'CreateObject');
  Result := Assigned(CreateSevenZipObject);
end;

procedure ExtractArchiveRedir(const DisableFsRedir: Boolean;
  const ArchiveFilename, DestDir, Password: String;
  const FullPaths: Boolean; const OnExtractionProgress: TOnExtractionProgress);

  function GetHandler(const Ext, NotFoundErrorMsg: String): TGUID;
  begin
    if SameText(Ext, '.7z') then
      Result := CLSID_Handler7z
    else if SameText(Ext, '.zip') then
      Result := CLSID_HandlerZip
    else if SameText(Ext, '.gz') then
      Result := CLSID_HandlerGzip
    else if SameText(Ext, '.bz2') then
      Result := CLSID_HandlerBZip2
    else if SameText(Ext, '.xz') then
      Result := CLSID_HandlerXz
    else if SameText(Ext, '.tar') then
      Result := CLSID_HandlerTar
    else if SameText(Ext, '.rar') then
      Result := CLSID_HandlerRar
    else if SameText(Ext, '.iso') then
      Result := CLSID_HandlerIso
    else if SameText(Ext, '.msi') then
      Result := CLSID_HandlerCompound
    else if SameText(Ext, '.cab') then
      Result := CLSID_HandlerCab
    else if SameText(Ext, '.rpm') then
      Result := CLSID_HandlerRpm
    else if SameText(Ext, '.vhd') then
      Result := CLSID_HandlerVhd
    else if SameText(Ext, '.vhdx') then
      Result := CLSID_HandlerVhdx
    else if SameText(Ext, '.vdi') then
      Result := CLSID_HandlerVDI
    else if SameText(Ext, '.vmdk') then
      Result := CLSID_HandlerVMDK
    else if SameText(Ext, '.wim') then
      Result := CLSID_HandlerWim
    else if SameText(Ext, '.dmg') then
      Result := CLSID_HandlerDmg
    else
      InternalError(NotFoundErrorMsg);
  end;

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
  if ArchiveFileName = '' then
    InternalError('ExtractArchive: Invalid ArchiveFileName value');
  const clsid = GetHandler(PathExtractExt(ArchiveFilename),
    'ExtractArchive: Unknown ArchiveFileName extension');
  if DestDir = '' then
    InternalError('ExtractArchive: Invalid DestDir value');

  LogFmt('Extracting archive %s to %s. Full paths? %s', [ArchiveFileName, DestDir, SYesNo[FullPaths]]);

  LogFmt('%s Decoder : Igor Pavlov', [SetupHeader.SevenZipLibraryName]); { Just like 7zMain.c }

  { CreateObject }
  var InArchive: IInArchive;
  if CreateSevenZipObject(clsid, IInArchive, InArchive) <> S_OK then begin
    Log('ERROR: Cannot get class object'); { Just like 7zMain.c and Client7z.cpp }
    raise Exception.Create(FmtSetupMessage(msgErrorExtractionFailed, ['-1']));
  end;

  { Open }
  const InStream: IInStream =
    TInStream.Create(TFileRedir.Create(DisableFsRedir, ArchiveFilename, fdOpenExisting, faRead, fsRead));
  var ScanSize: Int64 := 1 shl 23; { From Client7z.cpp }
  const OpenCallback: IArchiveOpenCallback = TArchiveOpenCallback.Create(Password);
  if InArchive.Open(InStream, @ScanSize, OpenCallback) <> S_OK then begin
    Log('ERROR: Cannot open file as archive'); { Just like 7zMain.c and Client7z.cpp }
    raise Exception.Create(FmtSetupMessage(msgErrorExtractionFailed, ['-2']));
  end;

  { Extract }
  const ExtractCallback: IArchiveExtractCallback =
    TArchiveExtractCallback.Create(InArchive, DisableFsRedir,
      ArchiveFilename, DestDir, Password, FullPaths, OnExtractionProgress);
  const Res = InArchive.Extract(nil, $FFFFFFFF, 0, ExtractCallback);
  if Res = E_ABORT then
    raise Exception.Create(SetupMessages[msgErrorExtractionAborted])
  else begin
    var OpRes := (ExtractCallback as TArchiveExtractCallback).OpRes;
    if OpRes <> kOK then begin
      LogFmt('ERROR: %s', [OperationResultToString(opRes)]); { Just like 7zMain.c }
      raise Exception.Create(FmtSetupMessage(msgErrorExtractionFailed, [Ord(OpRes).ToString]))
    end else if Res <> S_OK then
      raise Exception.Create(FmtSetupMessage(msgErrorExtractionFailed, [Format('%s %s', [Win32ErrorString(Res), IntToHexStr8(Res)])]));
  end;

  Log('Everything is Ok'); { Just like 7zMain.c }
end;

end.