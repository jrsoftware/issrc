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
  Windows, Shared.FileClass, Shared.VerInfoFunc, Compression.SevenZipDecoder;

function SevenZipDLLInit(const SevenZipLibrary: HMODULE;
  [ref] const VersionNumbers: TFileVersionNumbers): Boolean;
procedure SevenZipDLLDeInit;

procedure MapArchiveExtensions(const DestExt, SourceExt: String);

procedure ExtractArchiveRedir(const DisableFsRedir: Boolean;
  const ArchiveFilename, DestDir, Password: String; const FullPaths: Boolean;
  const OnExtractionProgress: TOnExtractionProgress);

{ These functions work similar to Windows' FindFirstFile, FindNextFile, and
  FindClose with the exception that recursion is built-in and that the
  resulting FindFileData.cFilename contains not just a filename but also the
  subdir. Also, ArchiveFindFirstFileRedir throws an exception for most errors:
  INVALID_HANDLE_VALUE is only used if the archive is ok but no suitable file
  was found. }
type
  TArchiveFindHandle = type NativeUInt;
  TOnExtractToHandleProgress = procedure(const Bytes, Param: Int64);
function ArchiveFindFirstFileRedir(const DisableFsRedir: Boolean;
  const ArchiveFilename, DestDir, Password: String;
  const RecurseSubDirs, ExtractIntent: Boolean;
  out FindFileData: TWin32FindData): TArchiveFindHandle;
function ArchiveFindNextFile(const FindFile: TArchiveFindHandle; out FindFileData: TWin32FindData): Boolean;
function ArchiveFindClose(const FindFile: TArchiveFindHandle): Boolean;
procedure ArchiveFindExtract(const FindFile: TArchiveFindHandle; const DestF: TFile;
  const OnExtractToHandleProgress: TOnExtractToHandleProgress; const OnExtractToHandleProgressParam: Int64);

type
  TFileTimeHelper = record helper for TFileTime
    procedure Clear;
    function HasTime: Boolean;
  end;

implementation

uses
  Classes, SysUtils, Forms, Variants, ActiveX, ComObj, Generics.Collections, Generics.Defaults,
  Compression.SevenZipDLLDecoder.Interfaces, PathFunc,
  Shared.SetupMessageIDs, Shared.CommonFunc,
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
    constructor Create(const AFile: TFile);
    destructor Destroy; override;
  end;

  TSequentialOutStream = class(TInterfacedObject, ISequentialOutStream)
  private
    FFile: TFile;
  protected
    function Write(data: Pointer; size: UInt32; processedSize: PUInt32): HRESULT; stdcall;
  public
    constructor Create(const AFileToBeDuplicated: TFile);
    destructor Destroy; override;
  end;

  TArchiveCallback = class(TInterfacedObject, ICryptoGetTextPassword)
  private
    FPassword: String;
  protected
    { ICryptoGetTextPassword - queried for by 7-Zip both on IArchiveOpenCallback
      and IArchiveExtractCallback instances - note: have not yet seen 7-Zip actually
      call it on an IArchiveOpenCallback instance }
    function CryptoGetTextPassword(out password: WideString): HRESULT; stdcall;
  public
    constructor Create(const Password: String);
  end;

  TArchiveOpenCallback = class(TArchiveCallback, IArchiveOpenCallback)
  protected
    { IArchiveOpenCallback }
    function SetTotal(files, bytes: PUInt64): HRESULT; stdcall;
    function SetCompleted(files, bytes: PUInt64): HRESULT; stdcall;
  end;

  TArchiveOpenCallbackWithStreamBackup = class(TArchiveOpenCallback)
  private
    FStreamBackup: IInStream;
  public
    constructor Create(const Password: String; const StreamToBackup: IInStream);
  end;

  TArchiveOpenFileCallback = class(TArchiveOpenCallback, IArchiveOpenVolumeCallback)
  private
    FDisableFsRedir: Boolean;
    FArchiveFilename: String;
  protected
    { IArchiveOpenVolumeCallback - queried for by 7-Zip on IArchiveOpenCallback }
    function GetProperty(propID: PROPID; var value: OleVariant): HRESULT; stdcall;
    function GetStream(const name: PChar; var inStream: IInStream): HRESULT; stdcall;
  public
    constructor Create(const DisableFsRedir: Boolean; const ArchiveFilename, Password: String);
  end;

  TArchiveExtractBaseCallback = class(TArchiveCallback, IArchiveExtractCallback)
  private
    type
      TResult = record
        SavedFatalException: TObject;
        Res: HRESULT;
        OpRes: TNOperationResult;
      end;
      TArrayOfUInt32 = array of UInt32;
    var
      FInArchive: IInArchive;
      FnumItems: UInt32;
      FLock: TObject;
      FProgress, FProgressMax: UInt64;
      FAbort: Boolean;
      FResult: TResult;
  protected
    { IProgress }
    function SetTotal(total: UInt64): HRESULT; stdcall;
    function SetCompleted(completeValue: PUInt64): HRESULT; stdcall;
    { IArchiveExtractCallback }
    function GetStream(index: UInt32; out outStream: ISequentialOutStream;
      askExtractMode: Int32): HRESULT; virtual; stdcall; abstract;
    function PrepareOperation(askExtractMode: Int32): HRESULT; stdcall;
    function SetOperationResult(opRes: TNOperationResult): HRESULT; virtual; stdcall;
    { Other }
    function GetIndices: TArrayOfUInt32; virtual; abstract;
    procedure Extract;
    procedure HandleProgress; virtual; abstract;
    procedure HandleResult;
  public
    constructor Create(const InArchive: IInArchive; const numItems: UInt32;
      const Password: String);
    destructor Destroy; override;
  end;

  TArchiveExtractAllCallback = class(TArchiveExtractBaseCallback)
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
      askExtractMode: Int32): HRESULT; override; stdcall;
    function SetOperationResult(opRes: TNOperationResult): HRESULT; override; stdcall;
    { Other }
    function GetIndices: TArchiveExtractBaseCallback.TArrayOfUInt32; override;
    procedure HandleProgress; override;
  public
    constructor Create(const InArchive: IInArchive; const numItems: UInt32;
      const DisableFsRedir: Boolean; const ArchiveFileName, DestDir, Password: String;
      const FullPaths: Boolean; const OnExtractionProgress: TOnExtractionProgress);
    destructor Destroy; override;
  end;

  TArchiveExtractToHandleCallback = class(TArchiveExtractBaseCallback)
  private
    FIndex: UInt32;
    FDestF: TFile;
    FOnExtractToHandleProgress: TOnExtractToHandleProgress;
    FOnExtractToHandleProgressParam: Int64;
    FPreviousProgress: UInt64;
  protected
    { IArchiveExtractCallback }
    function GetStream(index: UInt32; out outStream: ISequentialOutStream;
      askExtractMode: Int32): HRESULT; override; stdcall;
    { Other }
    function GetIndices: TArchiveExtractBaseCallback.TArrayOfUInt32; override;
    procedure HandleProgress; override;
  public
    constructor Create(const InArchive: IInArchive; const numItems: UInt32;
      const Password: String; const Index: UInt32; const DestF: TFile;
      const OnExtractToHandleProgress: TOnExtractToHandleProgress;
      const OnExtractToHandleProgressParam: Int64);
    destructor Destroy; override;
  end;

{ Helper functions }

procedure SevenZipWin32Error(const FunctionName: String; const ErrorCode: DWORD); overload;
begin
  const ExceptMessage = FmtSetupMessage(msgErrorFunctionFailedWithMessage,
    [FunctionName, IntToStr(ErrorCode), Win32ErrorString(ErrorCode)]);
  const LogMessage = Format('Function %s returned error code %d', [FunctionName, ErrorCode]);
  SevenZipError(ExceptMessage, LogMessage);
end;

procedure SevenZipWin32Error(const FunctionName: String); overload;
begin
  SevenZipWin32Error(FunctionName, GetLastError);
end;

function GetHandler(const Filename, NotFoundErrorMsg: String): TGUID; forward;

const
  varFileTime = 64; { Delphi lacks proper VT_FILETIME support }
type
  TVarTypeSet = set of varEmpty..varFileTime; { Incomplete but don't need others }

function GetProperty(const InArchive: IInArchive; const index: UInt32;
  const propID: PROPID; const allowedTypes: TVarTypeSet; out value: OleVariant): Boolean; overload;
{ Raises an EOleSysError exception on error but otherwise always sets value,
  returning True if it's not empty. Set index to $FFFF to query an archive property
  instead of an item propery }
begin
  var Res: HRESULT;
  if index = $FFFF then
    Res := InArchive.GetArchiveProperty(propID, value)
  else
    Res := InArchive.GetProperty(index, propID, value);
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
  out value: UInt64): Boolean; overload;
begin
  var varValue: OleVariant;
  Result := GetProperty(InArchive, index, propID, [varUInt64], varValue);
  value := varValue;
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

constructor TInStream.Create(const AFile: TFile);
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
      STREAM_SEEK_SET: FFile.Seek(offset);
      STREAM_SEEK_CUR: FFile.Seek(FFile.Position + offset);
      STREAM_SEEK_END: FFile.Seek(FFile.Size + offset);
    else
      Exit(E_INVALIDARG);
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

constructor TSequentialOutStream.Create(const AFileToBeDuplicated: TFile);
begin
  inherited Create;
  FFile := TFile.CreateDuplicate(AFileToBeDuplicated);
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

{ TArchiveCallback }

constructor TArchiveCallback.Create(const Password: String);
begin
  inherited Create;
  FPassword := Password;
end;

function TArchiveCallback.CryptoGetTextPassword(
  out password: WideString): HRESULT;
begin
  try
    password := FPassword;
    Result := S_OK;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end
end;

{ TArchiveOpenCallback }

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

{ TArchiveOpenCallbackWithStreamBackup }

constructor TArchiveOpenCallbackWithStreamBackup.Create(const Password: String;
  const StreamToBackup: IInStream);
begin
  inherited Create(Password);
  FStreamBackup := StreamToBackup;
end;

{ TArchiveOpenFileCallback }

constructor TArchiveOpenFileCallback.Create(const DisableFsRedir: Boolean; const ArchiveFilename,
  Password: String);
begin
  inherited Create(Password);
  FDisableFsRedir := DisableFsRedir;
  FArchiveFilename := ArchiveFilename;
end;

function TArchiveOpenFileCallback.GetProperty(propID: PROPID; var value: OleVariant): HRESULT;
begin
  { This is for multi-volume archives: when the archive is opened 7-Zip only receives a stream. It
    will then use this callback to find the name of the archive (like archive.7z.001) to figure out
    the name of other volumes (like archive.7z.002) }
  if propID = kpidName then
    value := FArchiveFilename
  else
    value := Unassigned; { Note sure if this is really needed }
  Result := S_OK;
end;

function TArchiveOpenFileCallback.GetStream(const name: PChar; var inStream: IInStream): HRESULT;
begin
  { This is for multi-volume archives: after 7-Zip figures out the name of other volumes (like
    archive.7z.002) it will then use this callback to open it. The callback must either return
    S_FALSE or set instream to nil when it tries to open a volume which doesn't exists (like
    archive.7z.003 when there's two volumes only). }
  try
    if NewFileExistsRedir(FDisableFsRedir, name) then begin
      const F = TFileRedir.Create(FDisableFsRedir, name, fdOpenExisting, faRead, fsRead);
      instream := TInStream.Create(F);
    end else
      instream := nil;
    Result := S_OK;
  except
    on E: EAbort do
      Result := E_ABORT
    else
      Result := E_FAIL;
  end;
end;

{ TArchiveExtractBaseCallback }

constructor TArchiveExtractBaseCallback.Create(const InArchive: IInArchive;
  const numItems: UInt32; const Password: String);
begin
  inherited Create(Password);
  FInArchive := InArchive;
  FnumItems := numItems;
  FLock := TObject.Create;
  FResult.OpRes := kOK;
end;

destructor TArchiveExtractBaseCallback.Destroy;
begin
  FResult.SavedFatalException.Free;
  FLock.Free;
  inherited;
end;

function TArchiveExtractBaseCallback.SetTotal(total: UInt64): HRESULT;
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

function TArchiveExtractBaseCallback.SetCompleted(completeValue: PUInt64): HRESULT;
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

function TArchiveExtractBaseCallback.PrepareOperation(askExtractMode: Int32): HRESULT;
begin
  { From Client7z.cpp: PrepareOperation is called *after* GetStream has been called }
  Result := S_OK;
end;

function TArchiveExtractBaseCallback.SetOperationResult(
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

function ExtractThreadFunc(Parameter: Pointer): Integer;
begin
  const E = TArchiveExtractBaseCallback(Parameter);
  try
    const Indices = E.GetIndices;
    const NIndices = Length(Indices);
    if NIndices > 0 then begin
       { From IArchive.h: indices must be sorted. Also: 7-Zip's code crashes if
         sent an invalid index. So we check them fully. }
      for var I := 0 to NIndices-1 do
        if (Indices[I] >= E.FnumItems) or ((I > 0) and (Indices[I-1] >= Indices[I])) then
          InternalError('NIndices invalid');
      E.FResult.Res := E.FInArchive.Extract(@Indices[0], NIndices, 0, E)
    end else
      E.FResult.Res := E.FInArchive.Extract(nil, $FFFFFFFF, 0, E)
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


procedure TArchiveExtractBaseCallback.Extract;
begin
  { We're calling 7-Zip's Extract in a separate thread. This is because packing
    our example MyProg.exe into a (tiny) .7z and extracting it caused a problem:
    GetStream and PrepareOperation and SetOperationResult were *all* called by
    7-Zip from a secondary thread. So we can't block our main thread as well
    because then we can't communicate progress to it. Having this extra thread
    has the added bonus of being able to communicate progress more often from
    SetCompleted. }

  var ThreadID: TThreadID; { Not used but BeginThread requires it }
  const ThreadHandle = BeginThread(nil, 0, ExtractThreadFunc, Self, 0, ThreadID);
  if ThreadHandle = 0 then
    SevenZipWin32Error('BeginThread');

  try
    try
      while True do begin
        case WaitForSingleObject(ThreadHandle, 50) of
          WAIT_OBJECT_0: Break;
          WAIT_TIMEOUT: HandleProgress; { This calls the user's OnExtractionProgress handler! }
          WAIT_FAILED: SevenZipWin32Error('WaitForSingleObject');
        else
          SevenZipError('WaitForSingleObject returned unknown value');
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

procedure TArchiveExtractBaseCallback.HandleResult;

  procedure BadOperationResultError(const opRes: TNOperationResult);
  begin
    var LogMessage: String;
    case opRes of
      kUnsupportedMethod: LogMessage := 'Unsupported method';
      kDataError: LogMessage := 'Data error';
      kCRCError: LogMessage := 'CRC error';
      kUnavailable: LogMessage := 'Unavailable data';
      kUnexpectedEnd: LogMessage := 'Unexpected end';
      kDataAfterEnd: LogMessage := 'Data after end';
      kIsNotArc: LogMessage := 'Is not an archive';
      kHeadersError: LogMessage := 'Headers error';
      kWrongPassword: LogMessage := 'Wrong password';
    else
      LogMessage := Format('Unknown operation result: %d', [Ord(opRes)]);
    end;

    case opRes of
      kUnsupportedMethod:
        SevenZipError(SetupMessages[msgArchiveUnsupportedFormat], LogMessage);
      kDataError, kCRCError, kUnavailable, kUnexpectedEnd, kDataAfterEnd, kIsNotArc, kHeadersError:
        SevenZipError(SetupMessages[msgArchiveIsCorrupted], LogMessage);
      kWrongPassword:
        SevenZipError(SetupMessages[msgArchiveIncorrectPassword], LogMessage);
    else
      SevenZipError(Ord(opRes).ToString, LogMessage);
    end;
  end;

  procedure BadResultError(const Res: HRESULT);
  begin
    if Res = E_OUTOFMEMORY then
      SevenZipError(Win32ErrorString(E_OUTOFMEMORY))
    else
      SevenZipWin32Error('Extract', FResult.Res);
  end;

begin
  if Assigned(FResult.SavedFatalException) then begin
    var Msg: String;
    if FResult.SavedFatalException is Exception then
      Msg := (FResult.SavedFatalException as Exception).Message
    else
      Msg := FResult.SavedFatalException.ClassName;
    InternalErrorFmt('Worker thread terminated unexpectedly with exception: %s', [Msg]);
  end else begin
    var OpRes := FResult.OpRes;
    if OpRes <> kOK then
      BadOperationResultError(OpRes)
    else if FResult.Res <> S_OK then
      BadResultError(FResult.Res);
  end;
end;

{ TArchiveExtractAllCallback }

procedure TArchiveExtractAllCallback.TCurrent.SetAttrib(const AAttrib: DWORD);
begin
  Attrib := AAttrib;
  HasAttrib := True;
end;

constructor TArchiveExtractAllCallback.Create(const InArchive: IInArchive;
  const numItems: UInt32; const DisableFsRedir: Boolean;
  const ArchiveFileName, DestDir, Password: String;
  const FullPaths: Boolean; const OnExtractionProgress: TOnExtractionProgress);
begin
  inherited Create(InArchive, numItems, Password);
  FDisableFsRedir := DisableFsRedir;
  FExpandedDestDir := AddBackslash(PathExpand(DestDir));
  FFullPaths := FullPaths;
  FExtractedArchiveName := PathExtractName(ArchiveFileName);
  FOnExtractionProgress := OnExtractionProgress;
  FLogQueue := TStringList.Create;
end;

destructor TArchiveExtractAllCallback.Destroy;
begin
  FLogQueue.Free;
end;

function TArchiveExtractAllCallback.GetIndices: TArchiveExtractBaseCallback.TArrayOfUInt32;
begin
  SetLength(Result, 0); { No indices = extract all }
end;

function TArchiveExtractAllCallback.GetStream(index: UInt32;
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
        try
          var BytesLeft: UInt64;
          if GetProperty(FInArchive, index, kpidSize, BytesLeft) then begin
            { To avoid file system fragmentation, preallocate all of the bytes in the
              destination file }
            DestF.Seek(Int64(BytesLeft));
            DestF.Truncate;
            DestF.Seek(0);
          end;
          { From IArchive.h: can also set outstream to nil to tell 7zip to skip the file }
          outstream := TSequentialOutStream.Create(DestF);
        finally
          { TSequentialOutStream duplicates the TFile, so DestF is no longer needed }
          DestF.Free;
        end;
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

function TArchiveExtractAllCallback.SetOperationResult(opRes: TNOperationResult): HRESULT;
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

procedure TArchiveExtractAllCallback.HandleProgress;
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

  if (CurrentPath <> '') and Assigned(FOnExtractionProgress) then
    if not FOnExtractionProgress(FExtractedArchiveName, CurrentPath, Progress, ProgressMax) then
      Abort;

  if DownloadTemporaryFileOrExtractArchiveProcessMessages then
    Application.ProcessMessages;
end;

{ TArchiveExtractToHandleCallback }

constructor TArchiveExtractToHandleCallback.Create(const InArchive: IInArchive;
  const numItems: UInt32; const Password: String; const Index: UInt32;
  const DestF: TFile; const OnExtractToHandleProgress: TOnExtractToHandleProgress;
  const OnExtractToHandleProgressParam: Int64);
begin
  inherited Create(InArchive, numItems, Password);
  FIndex := Index;
  FDestF := TFile.CreateDuplicate(DestF);
  FOnExtractToHandleProgress := OnExtractToHandleProgress;
  FOnExtractToHandleProgressParam := OnExtractToHandleProgressParam;
end;

destructor TArchiveExtractToHandleCallback.Destroy;
begin
  FDestF.Free;
  inherited;
end;

function TArchiveExtractToHandleCallback.GetIndices: TArchiveExtractBaseCallback.TArrayOfUInt32;
begin
  SetLength(Result, 1);
  Result[0] := FIndex;
end;

function TArchiveExtractToHandleCallback.GetStream(index: UInt32;
  out outStream: ISequentialOutStream; askExtractMode: Int32): HRESULT;
begin
  try
    if askExtractMode = kExtract then begin
      if index <> FIndex then
        OleError(E_INVALIDARG);
      var IsDir: Boolean;
      GetProperty(FInArchive, index, kpidIsDir, IsDir);
      if IsDir then
        OleError(E_INVALIDARG);
      var BytesLeft: UInt64;
      if GetProperty(FInArchive, index, kpidSize, BytesLeft) then begin
        { To avoid file system fragmentation, preallocate all of the bytes in the
          destination file }
        FDestF.Seek(Int64(BytesLeft));
        FDestF.Truncate;
        FDestF.Seek(0);
      end;
      outstream := TSequentialOutStream.Create(FDestF);
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

procedure TArchiveExtractToHandleCallback.HandleProgress;
begin
  if Assigned(FOnExtractToHandleProgress) then begin
    var Progress: UInt64;

    System.TMonitor.Enter(FLock);
    try
      Progress := FProgress;
    finally
      System.TMonitor.Exit(FLock);
    end;

    FOnExtractToHandleProgress(Progress-FPreviousProgress, FOnExtractToHandleProgressParam);
    FPreviousProgress := Progress;
  end;
end;

{ Additional helper functions }

type
  TSevenZipHandlers = TDictionary<String, TGUID>;

var
  CreateSevenZipObject: function(const clsid, iid: TGUID; var outObject): HRESULT; stdcall;
  VersionBanner: String;
  Handlers: TSevenZipHandlers;

function SevenZipDLLInit(const SevenZipLibrary: HMODULE;
  [ref] const VersionNumbers: TFileVersionNumbers): Boolean;
begin
  CreateSevenZipObject := GetProcAddress(SevenZipLibrary, 'CreateObject');
  Result := Assigned(CreateSevenZipObject);
  if (VersionNumbers.MS <> 0) or (VersionNumbers.LS <> 0) then
    VersionBanner := Format(' %u.%.2u', [(VersionNumbers.MS shr 16) and $FFFF, VersionNumbers.MS and $FFFF])
  else
    VersionBanner := '';

  Handlers := TSevenZipHandlers.Create(TIStringComparer.Ordinal);
  Handlers.Add('.7z', CLSID_Handler7z);
  Handlers.Add('.zip', CLSID_HandlerZip);
  Handlers.Add('.gz', CLSID_HandlerGzip);
  Handlers.Add('.bz2', CLSID_HandlerBZip2);
  Handlers.Add('.xz', CLSID_HandlerXz);
  Handlers.Add('.tar', CLSID_HandlerTar);
  Handlers.Add('.rar', CLSID_HandlerRar);
  Handlers.Add('.iso', CLSID_HandlerIso);
  Handlers.Add('.msi', CLSID_HandlerCompound);
  Handlers.Add('.cab', CLSID_HandlerCab);
  Handlers.Add('.rpm', CLSID_HandlerRpm);
  Handlers.Add('.vhd', CLSID_HandlerVhd);
  Handlers.Add('.vhdx', CLSID_HandlerVhdx);
  Handlers.Add('.vdi', CLSID_HandlerVDI);
  Handlers.Add('.vmdk', CLSID_HandlerVMDK);
  Handlers.Add('.wim', CLSID_HandlerWim);
  Handlers.Add('.dmg', CLSID_HandlerDmg);
  Handlers.Add('.001', CLSID_HandlerSplit);
end;

function GetHandlerForExt(const Ext, NotFoundErrorMsg: String): TGUID;
begin
  if not Handlers.TryGetValue(Ext, Result) then
    InternalError(NotFoundErrorMsg);
end;

function GetHandler(const Filename, NotFoundErrorMsg: String): TGUID;
begin;
  Result := GetHandlerForExt(PathExtractExt(Filename), NotFoundErrorMsg);
end;

procedure MapArchiveExtensions(const DestExt, SourceExt: String);
begin
  if (Length(DestExt) < 2) or (DestExt[1] <> '.') then
    InternalError('MapArchiveExtensions: Invalid DestExt');
  const clsid = GetHandlerForExt(SourceExt, 'MapArchiveExtensions: Invalid SourceExt');
  Handlers.AddOrSetValue(DestExt, clsid);
end;

var
  LoggedBanner: Boolean;

procedure LogBannerOnce;
begin
  if not LoggedBanner then begin
    LogFmt('%s Decoder%s : Igor Pavlov', [SetupHeader.SevenZipLibraryName, VersionBanner]); { Just like 7zMain.c }
    LoggedBanner := True;
  end;
end;

function OpenArchiveRedir(const DisableFsRedir: Boolean;
  const ArchiveFilename, Password: String; const clsid: TGUID; out numItems: UInt32): IInArchive;
const
  DefaultScanSize: Int64 = 1 shl 23; { From Client7z.cpp }
begin
  { CreateObject }
  if CreateSevenZipObject(clsid, IInArchive, Result) <> S_OK then
    SevenZipError(SetupMessages[msgArchiveUnsupportedFormat], 'Cannot get class object' { Just like Client7z.cpp });

  { Open }
  var F: TFile := nil; { Set to nil to silence compiler }
  try
    F := TFileRedir.Create(DisableFsRedir, ArchiveFilename, fdOpenExisting, faRead, fsRead);
  except
    on E: EFileError do
      SevenZipWin32Error('CreateFile', E.ErrorCode);
  end;
  const InStream: IInStream = TInStream.Create(F);
  var ScanSize := DefaultScanSize;
  const OpenCallback: IArchiveOpenCallback = TArchiveOpenFileCallback.Create(DisableFsRedir, ArchiveFileName, Password);
  if Result.Open(InStream, @ScanSize, OpenCallback) <> S_OK then
    SevenZipError(SetupMessages[msgArchiveIsCorrupted], 'Cannot open file as archive' { Just like Client7z.cpp });
  if Result.GetNumberOfItems(numItems) <> S_OK then
    SevenZipError(SetupMessages[msgArchiveIsCorrupted], 'Cannot get number of items');

  if numItems = 1 then begin
    { Get inner archive stream if it exists - See OpenArchive.cpp CArchiveLink::Open
      Give up trying to get or open it on any error }
    var MainSubFile: Cardinal;
    var SubSeqStream: ISequentialInStream;
    if not GetProperty(Result, $FFFF, kpidMainSubfile, MainSubFile) or
       (MainSubFile <> 0) or
       not Supports(Result, IInArchiveGetStream) or
       ((Result as IInArchiveGetStream).GetStream(MainSubFile, SubSeqStream) <> S_OK) or
       (SubSeqStream = nil) or
       not Supports(SubSeqStream, IInStream) then
      Exit;
    const SubStream = SubSeqStream as IInStream;

    { Open inner archive }
    var MainSubFilePath: String;
    if not GetProperty(Result, MainSubFile, kpidPath, MainSubFilePath) then
      Exit;
    if MainSubFilePath = '' then
      MainSubFilePath := PathChangeExt(ArchiveFilename, '');

    var SubClsid: TGUID;
    try
      SubClsid := GetHandler(MainSubFilePath, '');
    except
      Exit;
    end;
    var SubResult: IInArchive;
    if CreateSevenZipObject(SubClsid, IInArchive, SubResult) <> S_OK then
      Exit;

    var SubScanSize := DefaultScanSize;
    const SubOpenCallback: IArchiveOpenCallback =
      TArchiveOpenCallbackWithStreamBackup.Create(Password, InStream); { In tests the backup of InStream wasn't needed but better safe than sorry }
    var SubNumItems: UInt32;
    if (SubResult.Open(SubStream, @SubScanSize, SubOpenCallback) <> S_OK) or
       (SubResult.GetNumberOfItems(SubNumItems) <> S_OK) then
      Exit;

    Result := SubResult;
    numItems := SubNumItems;
  end;
end;

{ ExtractArchiveRedir }

procedure ExtractArchiveRedir(const DisableFsRedir: Boolean;
  const ArchiveFilename, DestDir, Password: String;
  const FullPaths: Boolean; const OnExtractionProgress: TOnExtractionProgress);
begin
  LogArchiveExtractionModeOnce;

  if ArchiveFileName = '' then
    InternalError('ExtractArchive: Invalid ArchiveFileName value');
  const clsid = GetHandler(ArchiveFilename,
    'ExtractArchive: Unknown ArchiveFileName extension');
  if DestDir = '' then
    InternalError('ExtractArchive: Invalid DestDir value');

  LogFmt('Extracting archive %s to %s. Full paths? %s', [ArchiveFileName,
    RemoveBackslashUnlessRoot(DestDir), SYesNo[FullPaths]]);

  LogBannerOnce;

  { Open }
  var numItems: UInt32;
  const InArchive = OpenArchiveRedir(DisableFsRedir, ArchiveFilename, Password,
    clsid, numItems);

  { Extract }
  const ExtractCallback: IArchiveExtractCallback =
    TArchiveExtractAllCallback.Create(InArchive, numItems, DisableFsRedir,
      ArchiveFilename, DestDir, Password, FullPaths, OnExtractionProgress);
  (ExtractCallback as TArchiveExtractAllCallback).Extract;

  Log('Everything is Ok'); { Just like 7zMain.c }
end;

{ ArchiveFindFirstFileRedir & co }

type
  TArchiveFindState = record
    InArchive: IInArchive;
    ExpandedDestDir, ExtractedArchiveName, Password: String;
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
    var Size: UInt64;
    GetProperty(InArchive, currentIndex, kpidSize, Size);
    FindData.nFileSizeHigh := Int64Rec(Size).Hi;
    FindData.nFileSizeLow := Int64Rec(Size).Lo;
  end;
end;

function ArchiveFindFirstFileRedir(const DisableFsRedir: Boolean;
  const ArchiveFilename, DestDir, Password: String; const RecurseSubDirs,
  ExtractIntent: Boolean; out FindFileData: TWin32FindData): TArchiveFindHandle;
begin
  LogArchiveExtractionModeOnce;

  if ArchiveFileName = '' then
    InternalError('ArchiveFindFirstFile: Invalid ArchiveFileName value');
  const clsid = GetHandler(ArchiveFilename,
    'ArchiveFindFirstFile: Unknown ArchiveFileName extension');

  LogBannerOnce;

  { Open }
  var State := Default(TArchiveFindState);
  State.InArchive := OpenArchiveRedir(DisableFsRedir, ArchiveFilename, Password, clsid, State.numItems);
  if DestDir <> '' then
    State.ExpandedDestDir := AddBackslash(PathExpand(DestDir));
  State.ExtractedArchiveName := PathExtractName(ArchiveFilename);
  State.Password := Password;
  State.RecurseSubDirs := RecurseSubDirs;

  { Log start of extraction }
  if ExtractIntent then begin
    LogFmt('Start extracting archive %s to %s. Recurse subdirs? %s', [ArchiveFilename,
      RemoveBackslashUnlessRoot(DestDir), SYesNo[RecurseSubDirs]]);
    var Solid: Boolean;
    if GetProperty(State.InArchive, $FFFF, kpidSolid, Solid) and Solid then
      Log('Archive is solid; extraction performance may degrade');
  end;

  if State.numItems > 0 then begin
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
  end;
  Result := INVALID_HANDLE_VALUE;
end;

function CheckFindFileHandle(const FindFile: TArchiveFindHandle): Integer;
begin
  Result := Integer(FindFile);
  if (Result < 0) or (Result >= ArchiveFindStates.Count) or
     (ArchiveFindStates[Result].InArchive = nil) then
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
  const I = CheckFindFileHandle(FindFile);
  var State := ArchiveFindStates[I];
  State.InArchive := nil;
  ArchiveFindStates[I] := State; { This just updates InArchive }
  Result := True;
end;

procedure ArchiveFindExtract(const FindFile: TArchiveFindHandle; const DestF: TFile;
  const OnExtractToHandleProgress: TOnExtractToHandleProgress;
  const OnExtractToHandleProgressParam: Int64);
begin
  const State = ArchiveFindStates[CheckFindFileHandle(FindFile)];

  var FindData: TWin32FindData;
  if not State.GetInitialCurrentFindData(FindData) or
     (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0) then
    InternalError('ArchiveFindExtract: Invalid current');

  const ExtractCallback: IArchiveExtractCallback =
    TArchiveExtractToHandleCallback.Create(State.InArchive, State.numItems,
      State.Password, State.currentIndex, DestF, OnExtractToHandleProgress,
      OnExtractToHandleProgressParam);
  (ExtractCallback as TArchiveExtractToHandleCallback).Extract;
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

{ SevenZipDLLDeInit }

procedure SevenZipDLLDeInit;
begin
  FreeAndNil(Handlers);
  { ArchiveFindStates has references to 7-Zip so must be cleared before the DLL is unloaded }
  FreeAndNil(ArchiveFindStates);
end;

end.