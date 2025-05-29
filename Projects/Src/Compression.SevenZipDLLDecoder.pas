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
    const
      varFileTime = 64; { Delphi lacks proper VT_FILETIME support }
    type
      TCurrent = record
        Path, ExpandedPath: String;
        HasAttrib: Boolean;
        Attrib: DWORD;
        CTime, MTime: TFileTime;
        outStream: ISequentialOutStream;
        procedure SetAttrib(const AAttrib: DWORD);
      end;
      TProgress = record
        Current: TCurrent;
        Progress, ProgressMax: UInt64;
        Abort: Boolean;
      end;
      TResult = record
        SavedFatalException: TObject;
        Res: HRESULT;
        OpRes: TNOperationResult;
      end;
      TVarTypeSet = set of varEmpty..varFileTime; { Incomplete but don't need others }
    var
      FInArchive: IInArchive;
      FDisableFsRedir: Boolean;
      FExpandedDestDir, FPassword: String;
      FFullPaths: Boolean;
      FExtractedArchiveName: String;
      FOnExtractionProgress: TOnExtractionProgress;
      FProgressAndLogQueueLock: TObject;
      FProgress: TProgress;
      FLogQueue: TStrings;
      FResult: TResult;
    function GetProperty(const index: UInt32; const propID: PROPID;
      const allowedTypes: TVarTypeSet; out value: OleVariant): Boolean; overload;
    function GetProperty(index: UInt32; propID: PROPID; out value: String): Boolean; overload;
    function GetProperty(index: UInt32; propID: PROPID; out value: UInt32): Boolean; overload;
    function GetProperty(index: UInt32; propID: PROPID; out value: Boolean): Boolean; overload;
    function GetProperty(index: UInt32; propID: PROPID; out value: TFileTime): Boolean; overload;
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
    destructor Destroy; override;
  end;

  TFileTimeHelper = record helper for TFileTime
    procedure Clear;
    function HasTime: Boolean;
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
  FProgressAndLogQueueLock := TObject.Create;
  FLogQueue := TStringList.Create;
  FResult.OpRes := kOK;
end;

destructor TArchiveExtractCallback.Destroy;
begin
  FResult.SavedFatalException.Free;
  FLogQueue.Free;
  FProgressAndLogQueueLock.Free;
end;

function TArchiveExtractCallback.SetTotal(total: UInt64): HRESULT;
begin
  { From IArchive.h: 7-Zip can call functions for IProgress or ICompressProgressInfo functions
    from another threads simultaneously with calls for IArchiveExtractCallback interface }
  try
    System.TMonitor.Enter(FProgressAndLogQueueLock);
    try
      FProgress.ProgressMax := total;
    finally
      System.TMonitor.Exit(FProgressAndLogQueueLock);
    end;
    Result := S_OK;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end;
end;

function TArchiveExtractCallback.SetCompleted(completeValue: PUInt64): HRESULT;
begin
  try
    System.TMonitor.Enter(FProgressAndLogQueueLock);
    try
      if FProgress.Abort then
        SysUtils.Abort;
      FProgress.Progress := completeValue^;
    finally
      System.TMonitor.Exit(FProgressAndLogQueueLock);
    end;
    Result := S_OK;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end;
end;

function TArchiveExtractCallback.GetProperty(const index: UInt32;
  const propID: PROPID; const allowedTypes: TVarTypeSet; out value: OleVariant): Boolean;
{ Raises an EOleSysError exception on error but otherwise always sets value,
  returning True if it's not empty }
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

function TArchiveExtractCallback.GetProperty(index: UInt32; propID: PROPID;
  out value: TFileTime): Boolean;
begin
  var varValue: OleVariant;
  Result := GetProperty(index, propID, [varFileTime], varValue);
  if Result then
    value := TFileTime(TVarData(varValue).VInt64)
  else
    value.Clear;
end;

function TArchiveExtractCallback.GetStream(index: UInt32;
  out outStream: ISequentialOutStream; askExtractMode: Int32): HRESULT;
begin
  try
    var NewCurrent := Default(TCurrent);
    if askExtractMode = kExtract then begin
      var Path: String;
      if not GetProperty(index, kpidPath, Path) then
        Path := PathChangeExt(FExtractedArchiveName, '');
      var IsDir: Boolean;
      GetProperty(index, kpidIsDir, IsDir);
      if IsDir then begin
        if FFullPaths then begin
          NewCurrent.Path := Path + '\';
          if not ValidateAndCombinePath(FExpandedDestDir, Path, NewCurrent.ExpandedPath) then
            OleError(E_ACCESSDENIED);
          ForceDirectories(FDisableFsRedir, NewCurrent.ExpandedPath);
        end;
        outStream := nil;
      end else begin
        var Attrib: DWORD;
        if GetProperty(index, kpidAttrib, Attrib) then begin
          if Attrib and $F0000000 <> 0 then
            Attrib := Attrib and $3FFF; { "PosixHighDetect", just like FileDir.cpp and similar to 7zMain.c }
          NewCurrent.SetAttrib(Attrib);
        end;
        GetProperty(index, kpidCTime, NewCurrent.CTime);
        GetProperty(index, kpidMTime, NewCurrent.MTime);
        if not FFullPaths then
          Path := PathExtractName(Path);
        NewCurrent.Path := Path;
        if not ValidateAndCombinePath(FExpandedDestDir, Path, NewCurrent.ExpandedPath) then
          OleError(E_ACCESSDENIED);
        ForceDirectories(FDisableFsRedir, PathExtractPath(NewCurrent.ExpandedPath));
        const ExistingFileAttr = GetFileAttributesRedir(FDisableFsRedir, NewCurrent.ExpandedPath);
        if (ExistingFileAttr <> INVALID_FILE_ATTRIBUTES) and
           (ExistingFileAttr and FILE_ATTRIBUTE_READONLY <> 0) then
          SetFileAttributesRedir(FDisableFsRedir, NewCurrent.ExpandedPath, ExistingFileAttr and not FILE_ATTRIBUTE_READONLY);
        { From IArchive.h: can also set outstream to nil to tell 7zip to skip the file }
        outstream := TSequentialOutStream.Create(TFileRedir.Create(FDisableFsRedir, NewCurrent.ExpandedPath, fdCreateAlways, faWrite, fsNone));
        NewCurrent.outStream := outStream;
      end;
    end;
    System.TMonitor.Enter(FProgressAndLogQueueLock);
    try
      if FProgress.Abort then
        SysUtils.Abort;
      FProgress.Current := NewCurrent;
      if NewCurrent.Path <> '' then
        FLogQueue.Append(NewCurrent.Path)
    finally
      System.TMonitor.Exit(FProgressAndLogQueueLock);
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
  { From Client7z.cpp: PrepareOperation is called *after* GetStream has been called }
  Result := S_OK;
end;

function TArchiveExtractCallback.SetOperationResult(opRes: TNOperationResult): HRESULT;
begin
  { From IArchive.h: Can now can close the file, set attributes, timestamps and security information }
  try
    try
      if opRes <> kOK then begin
        FResult.OpRes := opRes;
        Result := E_FAIL; { Make sure it doesn't continue with the next file }
      end else begin
        { GetStream is the only writer to outStream and ExpandedPath and HasAttrib so we don't need a lock because of this note from
          IArchive.h: 7-Zip doesn't call GetStream/PrepareOperation/SetOperationResult from different threads simultaneously }
        if (FProgress.Current.outStream <> nil) and (FProgress.Current.CTime.HasTime or FProgress.Current.MTime.HasTime) then
          SetFileTime((FProgress.Current.outStream as TSequentialOutStream).FFile.Handle,
            @FProgress.Current.CTime, nil, @FProgress.Current.MTime);
        FProgress.Current.outStream := nil; { Like 7zMain.c close the file before setting attributes - note that 7-Zip has cleared its own reference as well already }
        if (FProgress.Current.ExpandedPath <> '') and FProgress.Current.HasAttrib then
          SetFileAttributesRedir(FDisableFsRedir, FProgress.Current.ExpandedPath, FProgress.Current.Attrib);
        Result := S_OK;
      end;
    finally
      FProgress.Current.outStream := nil;
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

procedure SevenZipError(const LogMessage, ExceptMessage: String);
{ Do not call from secondary thread. LogMessage may contain non-localized text
  ExceptMessage should not. }
begin
  LogFmt('ERROR: %s', [LogMessage]); { Just like 7zMain.c }
  raise Exception.Create(ExceptMessage);
end;

procedure SevenZipWin32Error(const FunctionName: String; LastError: DWORD = 0); overload;
begin
  if LastError = 0 then
    LastError := GetLastError;
  const Msg = Format('%s (%u)', [Win32ErrorString(LastError), LastError]);
  SevenZipError(Format('%s failed: %s', [FunctionName, Msg]), Msg);
end;

function ExtractThreadFunc(Parameter: Pointer): Integer;
begin
  const E = TArchiveExtractCallback(Parameter);
  try
    E.FResult.Res := E.FInArchive.Extract(nil, $FFFFFFFF, 0, E);
  except
    const Ex = AcquireExceptionObject;
    MemoryBarrier;
    E.FResult.SavedFatalException := Ex;
  end;
  { Be extra sure FSavedFatalException (and everything else) is made visible
    prior to thread termination. (Likely redundant, but you never know...) }
  MemoryBarrier;
  Result := 0;
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

  procedure HandleProgress(const E: TArchiveExtractCallback);
  begin
    var Progress: TArchiveExtractCallback.TProgress;

    System.TMonitor.Enter(E.FProgressAndLogQueueLock);
    try
      Progress := E.FProgress;
      for var S in E.FLogQueue do
        LogFmt('- %s', [S]); { Just like 7zMain.c }
      E.FLogQueue.Clear;
    finally
      System.TMonitor.Exit(E.FProgressAndLogQueueLock);
    end;

    if Progress.Abort then
      Exit;

    var Abort := False;

    if (Progress.Current.Path <> '') and Assigned(E.FOnExtractionProgress) then begin
      { Calls to HandleProgress are already throttled so here we don't have to worry
        about calling the script to often }
      if not E.FOnExtractionProgress(E.FExtractedArchiveName, Progress.Current.Path, Progress.Progress, Progress.ProgressMax) then
        Abort := True;
    end;

    if not Abort and DownloadTemporaryFileOrExtractArchiveProcessMessages then
      Application.ProcessMessages;

    if Abort then begin
      System.TMonitor.Enter(E.FProgressAndLogQueueLock);
      try
        E.FProgress.Abort := True;
      finally
        System.TMonitor.Exit(E.FProgressAndLogQueueLock);
      end;
    end;
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
      Result := Format('Unknown operation result: %d', [Ord(opRes)]);
    end;
  end;

  procedure HandleResult([Ref] const Result: TArchiveExtractCallback.TResult);
  begin
    if Assigned(Result.SavedFatalException) then begin
      var Msg: String;
      if Result.SavedFatalException is Exception then
        Msg := (Result.SavedFatalException as Exception).Message
      else
        Msg := Result.SavedFatalException.ClassName;
      SevenZipError(Format('Worker thread terminated unexpectedly with exception: %s', [Msg]), Msg);
    end else if Result.Res = E_ABORT then
      Abort
    else begin
      var OpRes := Result.OpRes;
      if OpRes <> kOK then
        SevenZipError(OperationResultToString(Result.OpRes), Ord(OpRes).ToString)
      else if Result.Res <> S_OK then
        SevenZipWin32Error('Extract', Result.Res);
    end;
  end;

  procedure Extract(const E: TArchiveExtractCallback);
  begin
    { We're calling 7-Zip's Extract in a separate thread. This is because packing
      our example MyProg.exe into a (tiny) .7z and extracting it caused a problem:
      GetStream and PrepareOperation and SetOperationResult were *all* called by
      7-Zip from a secondary thread. So we can't block our main thread as well
      because then we can't communicate progress to it. Having this extra thread
      has the added bonus of being able to communicate progress more often from
      SetCompleted. }

    var ThreadID: TThreadID; { Not used but BeginThread requires it }
    const ThreadHandle = BeginThread(nil, 0, ExtractThreadFunc, E, 0, ThreadID);
    if ThreadHandle = 0 then
      SevenZipWin32Error('BeginThread');

    try
      try
        while True do begin
          case WaitForSingleObject(ThreadHandle, 50) of
            WAIT_OBJECT_0: Break;
            WAIT_TIMEOUT: HandleProgress(E);
          else
            SevenZipWin32Error('WaitForSingleObject');
          end;
        end;
      except
        { If an exception was raised during the loop (most likely it would
          be from the user's OnExtractionProgress handler), request abort
          and make one more attempt to wait on the thread. If we don't get
          definitive confirmation that the thread terminated (WAIT_OBJECT_0),
          then bump the object's reference count to prevent it from being
          freed, because the thread could still be running and accessing the
          object. Leaking memory isn't ideal, but a use-after-free problem
          is worse. Realisitically, though, WaitForSingleObject should never
          fail if given a valid handle. }
        E.FProgress.Abort := True;
        if WaitForSingleObject(ThreadHandle, INFINITE) <> WAIT_OBJECT_0 then
          E._AddRef;
        raise;
      end;
    finally
      CloseHandle(ThreadHandle);
    end;

    HandleProgress(E);
    HandleResult(E.FResult);
  end;

begin
  LogArchiveExtractionModeOnce;

  if ArchiveFileName = '' then
    InternalError('ExtractArchive: Invalid ArchiveFileName value');
  const clsid = GetHandler(PathExtractExt(ArchiveFilename),
    'ExtractArchive: Unknown ArchiveFileName extension');
  if DestDir = '' then
    InternalError('ExtractArchive: Invalid DestDir value');

  LogFmt('Extracting archive %s to %s. Full paths? %s', [ArchiveFileName, DestDir, SYesNo[FullPaths]]);

  LogFmt('%s Decoder : Igor Pavlov', [SetupHeader.SevenZipLibraryName]); { Just like 7zMain.c }

  try
    { CreateObject }
    var InArchive: IInArchive;
    if CreateSevenZipObject(clsid, IInArchive, InArchive) <> S_OK then
      SevenZipError('Cannot get class object' { Just like Client7z.cpp }, '-1');

    { Open }
    var F: TFile := nil; { Set to nil to silence compiler }
    try
      F := TFileRedir.Create(DisableFsRedir, ArchiveFilename, fdOpenExisting, faRead, fsRead);
    except
      SevenZipWin32Error('CreateFile');
    end;
    const InStream: IInStream = TInStream.Create(F);
    var ScanSize: Int64 := 1 shl 23; { From Client7z.cpp }
    const OpenCallback: IArchiveOpenCallback = TArchiveOpenCallback.Create(Password);
    if InArchive.Open(InStream, @ScanSize, OpenCallback) <> S_OK then
      SevenZipError('Cannot open file as archive' { Just like Client7z.cpp }, '-2');

    { Extract }
    const ExtractCallback: IArchiveExtractCallback =
      TArchiveExtractCallback.Create(InArchive, DisableFsRedir,
        ArchiveFilename, DestDir, Password, FullPaths, OnExtractionProgress);
    Extract(ExtractCallback as TArchiveExtractCallback);

    Log('Everything is Ok'); { Just like 7zMain.c }
  except
    on E: EAbort do
      raise Exception.Create(SetupMessages[msgErrorExtractionAborted])
    else
      raise Exception.Create(FmtSetupMessage(msgErrorExtractionFailed, [GetExceptMessage]));
  end;
end;

{ TFileTimeHelper }

procedure TFileTimeHelper.Clear;
begin
  { SetFileTime regards a pointer to a FILETIME structure with both members
    set to 0 the same as a NULL pointer and we make use of that. Note that
    7-Zip may return a value with both members set to 0 as well. }
  dwLowDateTime := 0;
  dwHighDateTime := 0;
end;

function TFileTimeHelper.HasTime: Boolean;
begin
  Result := (dwLowDateTime <> 0) or (dwHighDateTime <> 0);
end;

end.