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
  Windows, Shared.VerInfoFunc, Compression.SevenZipDecoder;

function SevenZipDLLInit(const SevenZipLibrary: HMODULE;
  [ref] const VersionNumbers: TFileVersionNumbers): Boolean;

procedure ExtractArchiveRedir(const DisableFsRedir: Boolean;
  const ArchiveFilename, DestDir, Password: String; const FullPaths: Boolean;
  const OnExtractionProgress: TOnExtractionProgress);

{ These functions work similar to Windows' FindFirstFile, FindNextFile, and
  FindClose with the exception that recursion is built-in and that the
  resulting FindFileData.cFilename contains not just a filename but also the
  subdir }
type
  TArchiveFindHandle = type Cardinal;
function ArchiveFindFirstFileRedir(const DisableFsRedir: Boolean;
  const ArchiveFilename, DestDir, Password: String; const RecurseSubDirs: Boolean;
  out FindFileData: TWin32FindData): TArchiveFindHandle;
function ArchiveFindNextFile(const FindFile: TArchiveFindHandle; out FindFileData: TWin32FindData): Boolean;
function ArchiveFindClose(const FindFile: TArchiveFindHandle): Boolean;

type
  TFileTimeHelper = record helper for TFileTime
    procedure Clear;
    function HasTime: Boolean;
  end;

implementation

uses
  Classes, SysUtils, Forms, Variants,
  ActiveX, ComObj, Generics.Collections,
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

  TArchiveExtractCallbackBase = class(TInterfacedObject)
  private
    type
      TResult = record
        SavedFatalException: TObject;
        Res: HRESULT;
        OpRes: TNOperationResult;
      end;
    var
      FInArchive: IInArchive;
      FPassword: String;
      FExtractThreadFunc: TThreadFunc;
      FLock: TObject;
      FProgress, FProgressMax: UInt64;
      FAbort: Boolean;
      FResult: TResult;
  protected
    { IProgress }
    function SetTotal(total: UInt64): HRESULT; stdcall;
    function SetCompleted(completeValue: PUInt64): HRESULT; stdcall;
    { IArchiveExtractCallback }
    function PrepareOperation(askExtractMode: Int32): HRESULT; stdcall;
    function SetOperationResult(opRes: TNOperationResult): HRESULT; stdcall;
    { ICryptoGetTextPassword - queried for on extractCallback }
    function CryptoGetTextPassword(out password: WideString): HRESULT; stdcall;
    { Other }
    procedure Extract;
    procedure HandleProgress; virtual; abstract;
    procedure HandleResult;
  public
    constructor Create(const InArchive: IInArchive; const Password: String);
    destructor Destroy; override;
  end;

  TArchiveExtractCallback = class(TArchiveExtractCallbackBase, IArchiveExtractCallback,
    ICryptoGetTextPassword)
  private
    type
      TCurrent = record
        Path, ExpandedPath: String;
        HasAttrib: Boolean;
        Attrib: DWORD;
        CTime, MTime: TFileTime;
        outStream: ISequentialOutStream;
        procedure SetAttrib(const AAttrib: DWORD);
      end;
    var
      FDisableFsRedir: Boolean;
      FExpandedDestDir: String;
      FFullPaths: Boolean;
      FExtractedArchiveName: String;
      FOnExtractionProgress: TOnExtractionProgress;
      FCurrent: TCurrent;  { Protected by base's FLock }
      FLogQueue: TStrings; { Same }
  protected
    { IArchiveExtractCallback }
    function GetStream(index: UInt32; out outStream: ISequentialOutStream;
      askExtractMode: Int32): HRESULT; stdcall;
    function SetOperationResult(opRes: TNOperationResult): HRESULT; stdcall;
    { Other }
    procedure HandleProgress; override;
  public
    constructor Create(const InArchive: IInArchive;
      const DisableFsRedir: Boolean; const ArchiveFileName, DestDir, Password: String;
      const FullPaths: Boolean; const OnExtractionProgress: TOnExtractionProgress);
    destructor Destroy; override;
  end;

{ Helper functions }

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

const
  varFileTime = 64; { Delphi lacks proper VT_FILETIME support }
type
  TVarTypeSet = set of varEmpty..varFileTime; { Incomplete but don't need others }

function GetProperty(const InArchive: IInArchive; const index: UInt32;
  const propID: PROPID; const allowedTypes: TVarTypeSet; out value: OleVariant): Boolean; overload;
{ Raises an EOleSysError exception on error but otherwise always sets value,
  returning True if it's not empty }
begin
  var Res := InArchive.GetProperty(index, propID, value);
  if Res <> S_OK then
    OleError(Res);
  Result := not VarIsEmpty(Value);
  if Result and not (VarType(value) in allowedTypes) then
    OleError(E_FAIL);
end;

function GetProperty(const InArchive: IInArchive; index: UInt32; propID: PROPID;
  out value: String): Boolean; overload;
begin
  var varValue: OleVariant;
  Result := GetProperty(InArchive, index, propID, [varOleStr], varValue);
  value := varValue;
end;

function GetProperty(const InArchive: IInArchive; index: UInt32; propID: PROPID;
  out value: Cardinal): Boolean; overload;
begin
  var varValue: OleVariant;
  Result := GetProperty(InArchive, index, propID, [varUInt32], varValue);
  value := varValue;
end;

function GetProperty(const InArchive: IInArchive; index: UInt32; propID: PROPID;
  out value: Boolean): Boolean; overload;
begin
  var varValue: OleVariant;
  Result := GetProperty(InArchive, index, propID, [varBoolean], varValue);
  value := varValue;
end;

function GetProperty(const InArchive: IInArchive; index: UInt32; propID: PROPID;
  out value: Integer64): Boolean; overload;
begin
  var varValue: OleVariant;
  Result := GetProperty(InArchive, index, propID, [varUInt64], varValue);
  value := Integer64(UInt64(varValue));
end;

function GetProperty(const InArchive: IInArchive; index: UInt32; propID: PROPID;
  out value: TFileTime): Boolean; overload;
begin
  var varValue: OleVariant;
  Result := GetProperty(InArchive, index, propID, [varFileTime], varValue);
  if Result then
    value := TFileTime(TVarData(varValue).VInt64)
  else
    value.Clear;
end;

procedure PosixHighDetect(var Attrib: DWORD);
begin
  { "PosixHighDetect", just like FileDir.cpp and similar to 7zMain.c }
  if Attrib and $F0000000 <> 0 then
    Attrib := Attrib and $3FFF;
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

{ TArchiveExtractCallbackBase }

constructor TArchiveExtractCallbackBase.Create(const InArchive: IInArchive;
  const Password: String);
begin
  inherited Create;
  FInArchive := InArchive;
  FPassword := Password;
  FLock := TObject.Create;
  FResult.OpRes := kOK;
end;

destructor TArchiveExtractCallbackBase.Destroy;
begin
  FResult.SavedFatalException.Free;
  FLock.Free;
  inherited;
end;

function TArchiveExtractCallbackBase.SetTotal(total: UInt64): HRESULT;
begin
  { From IArchive.h: 7-Zip can call functions for IProgress or ICompressProgressInfo functions
    from another threads simultaneously with calls for IArchiveExtractCallback interface }
  try
    System.TMonitor.Enter(FLock);
    try
      FProgressMax := total;
    finally
      System.TMonitor.Exit(FLock);
    end;
    Result := S_OK;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end;
end;

function TArchiveExtractCallbackBase.SetCompleted(completeValue: PUInt64): HRESULT;
begin
  try
    if FAbort then
      SysUtils.Abort;

    System.TMonitor.Enter(FLock);
    try
      FProgress := completeValue^;
    finally
      System.TMonitor.Exit(FLock);
    end;
    Result := S_OK;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end;
end;

function TArchiveExtractCallbackBase.PrepareOperation(askExtractMode: Int32): HRESULT;
begin
  { From Client7z.cpp: PrepareOperation is called *after* GetStream has been called }
  Result := S_OK;
end;

function TArchiveExtractCallbackBase.SetOperationResult(
  opRes: TNOperationResult): HRESULT;
begin
  try
    if opRes <> kOK then begin
      FResult.OpRes := opRes;
      Result := E_FAIL; { Make sure it doesn't continue with the next file }
    end else
      Result := S_OK;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end;
end;

function TArchiveExtractCallbackBase.CryptoGetTextPassword(
  out password: WideString): HRESULT;
begin
  Result := SevenZipSetPassword(FPassword, password);
end;

procedure TArchiveExtractCallbackBase.Extract;
begin
  { We're calling 7-Zip's Extract in a separate thread. This is because packing
    our example MyProg.exe into a (tiny) .7z and extracting it caused a problem:
    GetStream and PrepareOperation and SetOperationResult were *all* called by
    7-Zip from a secondary thread. So we can't block our main thread as well
    because then we can't communicate progress to it. Having this extra thread
    has the added bonus of being able to communicate progress more often from
    SetCompleted. }

  var ThreadID: TThreadID; { Not used but BeginThread requires it }
  if not Assigned(FExtractThreadFunc) then
    InternalError('not Assigned(FExtractThreadFunc)');
  const ThreadHandle = BeginThread(nil, 0, FExtractThreadFunc, Self, 0, ThreadID);
  if ThreadHandle = 0 then
    SevenZipWin32Error('BeginThread');

  try
    try
      while True do begin
        case WaitForSingleObject(ThreadHandle, 50) of
          WAIT_OBJECT_0: Break;
          WAIT_TIMEOUT: HandleProgress; { This calls the user's OnExtractionProgress handler! }
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
      FAbort := True; { Atomic so no lock }
      if WaitForSingleObject(ThreadHandle, INFINITE) <> WAIT_OBJECT_0 then
        _AddRef;
      raise;
    end;
  finally
    CloseHandle(ThreadHandle);
  end;

  HandleProgress;
  HandleResult;
end;

procedure TArchiveExtractCallbackBase.HandleResult;

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

begin
  if Assigned(FResult.SavedFatalException) then begin
    var Msg: String;
    if FResult.SavedFatalException is Exception then
      Msg := (FResult.SavedFatalException as Exception).Message
    else
      Msg := FResult.SavedFatalException.ClassName;
    SevenZipError(Format('Worker thread terminated unexpectedly with exception: %s', [Msg]), Msg);
  end else if FResult.Res = E_ABORT then
    Abort
  else begin
    var OpRes := FResult.OpRes;
    if OpRes <> kOK then
      SevenZipError(OperationResultToString(FResult.OpRes), Ord(OpRes).ToString)
    else if FResult.Res <> S_OK then
      SevenZipWin32Error('Extract', FResult.Res);
  end;
end;

{ TArchiveExtractCallback }

procedure TArchiveExtractCallback.TCurrent.SetAttrib(const AAttrib: DWORD);
begin
  Attrib := AAttrib;
  HasAttrib := True;
end;

function ArchiveExtractCallbackExtractThreadFunc(Parameter: Pointer): Integer;
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

constructor TArchiveExtractCallback.Create(const InArchive: IInArchive;
  const DisableFsRedir: Boolean; const ArchiveFileName, DestDir, Password: String;
  const FullPaths: Boolean; const OnExtractionProgress: TOnExtractionProgress);
begin
  inherited Create(InArchive, Password);
  FExtractThreadFunc := ArchiveExtractCallbackExtractThreadFunc;
  FDisableFsRedir := DisableFsRedir;
  FExpandedDestDir := AddBackslash(PathExpand(DestDir));
  FFullPaths := FullPaths;
  FExtractedArchiveName := PathExtractName(ArchiveFileName);
  FOnExtractionProgress := OnExtractionProgress;
  FLogQueue := TStringList.Create;
end;

destructor TArchiveExtractCallback.Destroy;
begin
  FLogQueue.Free;
end;

function TArchiveExtractCallback.GetStream(index: UInt32;
  out outStream: ISequentialOutStream; askExtractMode: Int32): HRESULT;
begin
  try
    if FAbort then
      SysUtils.Abort;

    var NewCurrent := Default(TCurrent);
    if askExtractMode = kExtract then begin
      var Path: String;
      if not GetProperty(FInArchive, index, kpidPath, Path) then
        Path := PathChangeExt(FExtractedArchiveName, '');
      var IsDir: Boolean;
      GetProperty(FInArchive, index, kpidIsDir, IsDir);
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
        if GetProperty(FInArchive, index, kpidAttrib, Attrib) then begin
          PosixHighDetect(Attrib);
          NewCurrent.SetAttrib(Attrib);
        end;
        GetProperty(FInArchive, index, kpidCTime, NewCurrent.CTime);
        GetProperty(FInArchive, index, kpidMTime, NewCurrent.MTime);
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
        const DestF = TFileRedir.Create(FDisableFsRedir, NewCurrent.ExpandedPath, fdCreateAlways, faWrite, fsNone);
        var BytesLeft: Integer64;
        if GetProperty(FInArchive, index, kpidSize, BytesLeft) then begin
          { To avoid file system fragmentation, preallocate all of the bytes in the
            destination file }
          DestF.Seek64(BytesLeft);
          DestF.Truncate;
          DestF.Seek(0);
        end;
        { From IArchive.h: can also set outstream to nil to tell 7zip to skip the file }
        outstream := TSequentialOutStream.Create(DestF);
        NewCurrent.outStream := outStream;
      end;
    end;
    System.TMonitor.Enter(FLock);
    try
      FCurrent := NewCurrent;
      if NewCurrent.Path <> '' then
        FLogQueue.Append(NewCurrent.Path)
    finally
      System.TMonitor.Exit(FLock);
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

function TArchiveExtractCallback.SetOperationResult(opRes: TNOperationResult): HRESULT;
begin
  { From IArchive.h: Can now can close the file, set attributes, timestamps and security information }
  try
    try
      Result := inherited;
      if Result = S_OK then begin
        { GetStream is the only writer to outStream and ExpandedPath and HasAttrib so we don't need a lock because of this note from
          IArchive.h: 7-Zip doesn't call GetStream/PrepareOperation/SetOperationResult from different threads simultaneously }
        if (FCurrent.outStream <> nil) and (FCurrent.CTime.HasTime or FCurrent.MTime.HasTime) then
          SetFileTime((FCurrent.outStream as TSequentialOutStream).FFile.Handle,
            @FCurrent.CTime, nil, @FCurrent.MTime);
        FCurrent.outStream := nil; { Like 7zMain.c close the file before setting attributes - note that 7-Zip has cleared its own reference as well already }
        if (FCurrent.ExpandedPath <> '') and FCurrent.HasAttrib then
          SetFileAttributesRedir(FDisableFsRedir, FCurrent.ExpandedPath, FCurrent.Attrib);
      end;
    finally
      FCurrent.outStream := nil;
    end;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end;
end;

procedure TArchiveExtractCallback.HandleProgress;
begin
  var CurrentPath: String;
  var Progress, ProgressMax: UInt64;

  System.TMonitor.Enter(FLock);
  try
    CurrentPath := FCurrent.Path;
    Progress := FProgress;
    ProgressMax := FProgressMax;
    for var S in FLogQueue do
      LogFmt('- %s', [S]); { Just like 7zMain.c }
    FLogQueue.Clear;
  finally
    System.TMonitor.Exit(FLock);
  end;

  var Abort := FAbort;
  if Abort then
    Exit;

  if (CurrentPath <> '') and Assigned(FOnExtractionProgress) then begin
    { Calls to HandleProgress are already throttled so here we don't have to worry
      about calling the script to often }
    if not FOnExtractionProgress(FExtractedArchiveName, CurrentPath, Progress, ProgressMax) then
      Abort := True;
  end;

  if not Abort and DownloadTemporaryFileOrExtractArchiveProcessMessages then
    Application.ProcessMessages;

  if Abort then
    FAbort := Abort; { Atomic so no lock }
end;

{ Additional helper functions }

var
  CreateSevenZipObject: function(const clsid, iid: TGUID; var outObject): HRESULT; stdcall;
  VersionBanner: String;

function SevenZipDLLInit(const SevenZipLibrary: HMODULE;
  [ref] const VersionNumbers: TFileVersionNumbers): Boolean;
begin
  CreateSevenZipObject := GetProcAddress(SevenZipLibrary, 'CreateObject');
  Result := Assigned(CreateSevenZipObject);
  if (VersionNumbers.MS <> 0) or (VersionNumbers.LS <> 0) then
    VersionBanner := Format(' %u.%.2u', [(VersionNumbers.MS shr 16) and $FFFF, VersionNumbers.MS and $FFFF])
  else
    VersionBanner := '';
end;

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

function OpenArchiveRedir(const DisableFsRedir: Boolean;
  const ArchiveFilename, Password: String; const clsid: TGUID): IInArchive;
begin
  { CreateObject }
  if CreateSevenZipObject(clsid, IInArchive, Result) <> S_OK then
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
  if Result.Open(InStream, @ScanSize, OpenCallback) <> S_OK then
    SevenZipError('Cannot open file as archive' { Just like Client7z.cpp }, '-2');
end;

{ ExtractArchiveRedir }

procedure ExtractArchiveRedir(const DisableFsRedir: Boolean;
  const ArchiveFilename, DestDir, Password: String;
  const FullPaths: Boolean; const OnExtractionProgress: TOnExtractionProgress);
begin
  LogArchiveExtractionModeOnce;

  if ArchiveFileName = '' then
    InternalError('ExtractArchive: Invalid ArchiveFileName value');
  const clsid = GetHandler(PathExtractExt(ArchiveFilename),
    'ExtractArchive: Unknown ArchiveFileName extension');
  if DestDir = '' then
    InternalError('ExtractArchive: Invalid DestDir value');

  LogFmt('Extracting archive %s to %s. Full paths? %s', [ArchiveFileName, DestDir, SYesNo[FullPaths]]);

  LogFmt('%s Decoder%s : Igor Pavlov', [SetupHeader.SevenZipLibraryName, VersionBanner]); { Just like 7zMain.c }

  try
    { Open }
    const InArchive = OpenArchiveRedir(DisableFsRedir, ArchiveFilename, Password, clsid);

    { Extract }
    const ExtractCallback: IArchiveExtractCallback =
      TArchiveExtractCallback.Create(InArchive, DisableFsRedir,
        ArchiveFilename, DestDir, Password, FullPaths, OnExtractionProgress);
    (ExtractCallback as TArchiveExtractCallback).Extract;

    Log('Everything is Ok'); { Just like 7zMain.c }
  except
    on E: EAbort do
      raise Exception.Create(SetupMessages[msgErrorExtractionAborted])
    else
      raise Exception.Create(FmtSetupMessage(msgErrorExtractionFailed, [GetExceptMessage]));
  end;
end;

{ ArchiveFindFirstFileRedir & co }

type
  TArchiveFindState = record
    InArchive: IInArchive;
    ExpandedDestDir, ExtractedArchiveName: String;
    RecurseSubDirs: Boolean;
    currentIndex, numItems: UInt32;
    function GetInitialCurrentFindData(out FindData: TWin32FindData): Boolean;
    procedure FinishCurrentFindData(var FindData: TWin32FindData);
  end;

  TArchiveFindStates = TList<TArchiveFindState>;

var
  ArchiveFindStates: TArchiveFindStates;

function TArchiveFindState.GetInitialCurrentFindData(out FindData: TWin32FindData): Boolean;

  function SkipFile(const Path: String; const IsDir: Boolean): Boolean;
  begin
    Result := (not RecurseSubDirs and (IsDir or (PathPos('\', Path) <> 0))) or
              not ValidateAndCombinePath(ExpandedDestDir, Path);
  end;

begin
  var Path: String;
  if not GetProperty(InArchive, currentIndex, kpidPath, Path) then
    Path := PathChangeExt(ExtractedArchiveName, '');
  var IsDir: Boolean;
  GetProperty(InArchive, currentIndex, kpidIsDir, IsDir);

  Result := not SkipFile(Path, IsDir);
  if Result then begin
    FindData := Default(TWin32FindData);
    if Length(Path) >= MAX_PATH then
      InternalError('GetInitialCurrentFindData: Length(Path) >= MAX_PATH');
    StrPCopy(FindData.cFileName, Path);
    if IsDir then
      FindData.dwFileAttributes := FindData.dwFileAttributes or FILE_ATTRIBUTE_DIRECTORY;
  end;
end;

procedure TArchiveFindState.FinishCurrentFindData(var FindData: TWin32FindData);
begin
  if FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then begin
    var Attrib: DWORD;
    GetProperty(InArchive, currentIndex, kpidAttrib, Attrib);
    PosixHighDetect(Attrib);
    FindData.dwFileAttributes := FindData.dwFileAttributes or Attrib;
    GetProperty(InArchive, currentIndex, kpidCTime, FindData.ftCreationTime);
    GetProperty(InArchive, currentIndex, kpidMTime, FindData.ftLastWriteTime);
    var Size: Integer64;
    GetProperty(InArchive, currentIndex, kpidSize, Size);
    FindData.nFileSizeHigh := Size.Hi;
    FindData.nFileSizeLow := Size.Lo;
  end;
end;

function ArchiveFindFirstFileRedir(const DisableFsRedir: Boolean;
  const ArchiveFilename, DestDir, Password: String; const RecurseSubDirs: Boolean;
  out FindFileData: TWin32FindData): TArchiveFindHandle;
begin
  if ArchiveFileName = '' then
    InternalError('ArchiveFindFirstFile: Invalid ArchiveFileName value');
  const clsid = GetHandler(PathExtractExt(ArchiveFilename),
    'ArchiveFindFirstFile: Unknown ArchiveFileName extension');

  try
    { Open }
    var State := Default(TArchiveFindState);
    State.InArchive := OpenArchiveRedir(DisableFsRedir, ArchiveFilename, Password, clsid);
    if State.InArchive.GetNumberOfItems(State.numItems) <> S_OK then
      SevenZipError('Cannot get number of items', '-3');
    if DestDir <> '' then
      State.ExpandedDestDir := AddBackslash(PathExpand(DestDir));
    State.ExtractedArchiveName := PathExtractName(ArchiveFilename);
    State.RecurseSubDirs := RecurseSubDirs;

    for var currentIndex: UInt32 := 0 to State.numItems-1 do begin
      if State.GetInitialCurrentFindData(FindFileData) then begin
        { Finish state }
        State.currentIndex := currentIndex;

        { Save state }
        if ArchiveFindStates = nil then
          ArchiveFindStates := TArchiveFindStates.Create;
        ArchiveFindStates.Add(State);

        { Finish find data & exit }
        State.FinishCurrentFindData(FindFileData);
        Exit(ArchiveFindStates.Count-1);
      end;
    end;
    Result := INVALID_HANDLE_VALUE;
  except
    on E: EAbort do
      raise Exception.Create(SetupMessages[msgErrorExtractionAborted])
    else
      raise Exception.Create(FmtSetupMessage(msgErrorExtractionFailed, [GetExceptMessage]));
  end;
end;

function CheckFindFileHandle(const FindFile: TArchiveFindHandle): Integer;
begin
  Result := Integer(FindFile);
  if (Result < 0) or (Result >= ArchiveFindStates.Count) then
    InternalError('CheckFindFileHandle failed');
end;

function ArchiveFindNextFile(const FindFile: TArchiveFindHandle; out FindFileData: TWin32FindData): Boolean;
begin
  const I = CheckFindFileHandle(FindFile);
  var State := ArchiveFindStates[I];

  for var currentIndex := State.currentIndex+1 to State.numItems-1 do begin
    State.currentIndex := currentIndex;
    if State.GetInitialCurrentFindData(FindFileData) then begin
      { Update state }
      ArchiveFindStates[I] := State; { This just updates currentIndex }

      { Finish find data & exit }
      State.FinishCurrentFindData(FindFileData);
      Exit(True);
    end;
  end;
  Result := False;
end;

function ArchiveFindClose(const FindFile: TArchiveFindHandle): Boolean;
begin
  ArchiveFindStates.Delete(CheckFindFileHandle(FindFile));
  Result := True;
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

initialization

finalization
  if (ArchiveFindStates <> nil) and (ArchiveFindStates.Count > 0) then { Not allowed because it has references to 7-Zip which is probably already unloaded }
    InternalError('ArchiveFindStates.Count > 0');
  ArchiveFindStates.Free;

end.