unit Setup.DownloadFileFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Installation procedures: downloading files
}

interface

uses
  Shared.FileClass, Shared.Struct;

type
  TOnDownloadProgress = function(const Url, BaseName: string; const Progress, ProgressMax: Int64): Boolean of object;
  TOnSimpleDownloadProgress = procedure(const Bytes, Param: Int64);

function DownloadFile(const Url, CustomUserName, CustomPassword: String;
  const DestF: TFile; [ref] const Verification: TSetupFileVerification; const ISSigSourceFilename: String;
  const OnSimpleDownloadProgress: TOnSimpleDownloadProgress;
  const OnSimpleDownloadProgressParam: Int64): Int64;

  function DownloadTemporaryFile(const Url, BaseName: String;
  [ref] const Verification: TSetupFileVerification; const OnDownloadProgress: TOnDownloadProgress): Int64; overload;
function DownloadTemporaryFile(const Url, BaseName: String;
  [ref] const Verification: TSetupFileVerification; const OnDownloadProgress: TOnDownloadProgress;
  out DestFile: String): Int64; overload;
function DownloadTemporaryFileSize(const Url: String): Int64;
function DownloadTemporaryFileDate(const Url: String): String;
procedure SetDownloadTemporaryFileCredentials(const User, Pass: String);

function GetISSigUrl(const Url, ISSigUrl: String): String;

implementation

uses
  Windows, Classes, Forms, SysUtils, Net.HttpClient, Net.URLClient, NetEncoding,
  ISSigFunc, PathFunc, SHA256,
  Shared.CommonFunc, Shared.SetupMessageIDs, Shared.SetupTypes,
  SetupLdrAndSetup.InstFunc, SetupLdrAndSetup.Messages,
  Setup.InstFunc, Setup.ISSigVerifyFunc, Setup.LoggingFunc, Setup.MainFunc;

type
  THTTPDataReceiver = class
  private
    type
      TResult = record
        SavedFatalException: TObject;
        HTTPStatusCode: Integer;
        HTTPStatusText: String;
        FileSize: Int64;
      end;
    var
      FBaseName, FCleanUrl: String;
      FHasCredentials: Boolean;
      FUser, FPass: String;
      FDestFile: TFile;
      FOnDownloadProgress: TOnDownloadProgress;
      FOnSimpleDownloadProgress: TOnSimpleDownloadProgress;
      FOnSimpleDownloadProgressParam: Int64;
      FLock: TObject;
      FProgress, FProgressMax: Int64;
      FProgressSet: Boolean;
      FLastReportedProgress: Int64;
      FAbort: Boolean;
      FResult: TResult;
  protected
    procedure DoDownload;
    procedure HandleProgress;
    procedure HandleResult(const UseSetupMessagesForErrors: Boolean);
  public
    constructor Create(const Url, CustomUser, CustomPass: String; const DestFile: TFile);
    destructor Destroy; override;
    property BaseName: String write FBaseName;
    property OnDownloadProgress: TOnDownloadProgress write FOnDownloadProgress;
    property OnSimpleDownloadProgress: TOnSimpleDownloadProgress write FOnSimpleDownloadProgress;
    property OnSimpleDownloadProgressParam: Int64 write FOnSimpleDownloadProgressParam;
    property Aborted: Boolean read FAbort;
    property Progress: Int64 read FProgress;
    property ProgressMax: Int64 read FProgressMax;
    procedure OnReceiveData(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
    function Download(const UseSetupMessagesForErrors: Boolean): Int64;
  end;

function GetCredentialsAndCleanUrl(const Url, CustomUser, CustomPass: String; var User, Pass, CleanUrl: String) : Boolean;
begin
  const Uri = TUri.Create(Url); { This is a record so no need to free }
  if CustomUser = '' then
    User := TNetEncoding.URL.Decode(Uri.Username)
  else
    User := CustomUser;
  if CustomPass = '' then
    Pass := TNetEncoding.URL.Decode(Uri.Password, [TURLEncoding.TDecodeOption.PlusAsSpaces])
  else
    Pass := CustomPass;
  Uri.Username := '';
  Uri.Password := '';
  CleanUrl := Uri.ToString;
  Result := (User <> '') or (Pass <> '');
  if Result then
    LogFmt('Download is using basic authentication: %s, ***', [User])
  else
    Log('Download is not using basic authentication');
end;

procedure SetUserAgentAndSecureProtocols(const AHTTPClient: THTTPClient);
begin
  AHTTPClient.UserAgent := SetupTitle + ' ' + SetupVersion;
  { TLS 1.2 isn't enabled by default on older versions of Windows }
  AHTTPClient.SecureProtocols := [THTTPSecureProtocol.TLS1, THTTPSecureProtocol.TLS11, THTTPSecureProtocol.TLS12];
end;

{ THTTPDataReceiver }

constructor THTTPDataReceiver.Create(const Url, CustomUser, CustomPass: String; const DestFile: TFile);
begin
  inherited Create;
  FDestFile := DestFile;
  FHasCredentials := GetCredentialsAndCleanUrl(Url, CustomUser, CustomPass, FUser, FPass, FCleanUrl);
  FLock := TObject.Create;
end;

destructor THTTPDataReceiver.Destroy;
begin
  FResult.SavedFatalException.Free;
  FLock.Free;
  inherited;
end;

procedure THTTPDataReceiver.OnReceiveData(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
begin
  if FAbort then
    Abort := True;

  System.TMonitor.Enter(FLock);
  try
    FProgress := AReadCount;
    FProgressMax := AContentLength;
    FProgressSet := True;
  finally
    System.TMonitor.Exit(FLock);
  end;
end;

procedure THTTPDataReceiver.DoDownload;
begin
  const HTTPClient = THTTPClient.Create; { http://docwiki.embarcadero.com/RADStudio/Rio/en/Using_an_HTTP_Client }
  try
    SetUserAgentAndSecureProtocols(HTTPClient);
    HTTPClient.OnReceiveData := OnReceiveData;

    const HandleStream = THandleStream.Create(FDestFile.Handle);
    try
      if FHasCredentials then begin
        const Base64 = TBase64Encoding.Create(0);
        try
          HTTPClient.CustomHeaders['Authorization'] := 'Basic ' + Base64.Encode(FUser + ':' + FPass);
        finally
          Base64.Free;
        end;
      end;
      
      const HTTPResponse = HTTPClient.Get(FCleanUrl, HandleStream);
      
      FResult.HTTPStatusCode := HTTPResponse.StatusCode;
      FResult.HTTPStatusText := HTTPResponse.StatusText;
      FResult.FileSize := HandleStream.Size;
    finally
      HandleStream.Free;
    end;
  finally
    HTTPClient.Free;
  end;
end;

procedure THTTPDataReceiver.HandleProgress;
begin
  var Progress, ProgressMax: Int64;
  var ProgressSet: Boolean;

  System.TMonitor.Enter(FLock);
  try
    Progress := FProgress;
    ProgressMax := FProgressMax;
    ProgressSet := FProgressSet;
  finally
    System.TMonitor.Exit(FLock);
  end;

  if ProgressSet then begin
    try
      if Assigned(FOnDownloadProgress) then begin
        if not FOnDownloadProgress(FCleanUrl, FBaseName, Progress, ProgressMax) then
          FAbort := True; { Atomic so no lock }
      end else if Assigned(FOnSimpleDownloadProgress) then begin
        try
          FOnSimpleDownloadProgress(Progress-FLastReportedProgress, FOnSimpleDownloadProgressParam);
        finally
          FLastReportedProgress := Progress;
        end;
      end;
    except
      if ExceptObject is EAbort then { FOnSimpleDownloadProgress always uses Abort to abort }
        FAbort := True { Atomic so no lock }
      else
        raise;
    end;
  end;

  if DownloadTemporaryFileOrExtractArchiveProcessMessages then
    Application.ProcessMessages;
end;

procedure THTTPDataReceiver.HandleResult(const UseSetupMessagesForErrors: Boolean);
begin
  if Assigned(FResult.SavedFatalException) then begin
    var Msg: String;
    if FResult.SavedFatalException is Exception then
      Msg := (FResult.SavedFatalException as Exception).Message
    else
      Msg := FResult.SavedFatalException.ClassName;
    InternalErrorFmt('Worker thread terminated unexpectedly with exception: %s', [Msg]);
  end else begin
    if Aborted then begin
      if UseSetupMessagesForErrors then
        raise Exception.Create(SetupMessages[msgErrorDownloadAborted])
      else
        Abort;
    end else if (FResult.HTTPStatusCode < 200) or (FResult.HTTPStatusCode > 299) then begin
      if UseSetupMessagesForErrors then
        raise Exception.Create(FmtSetupMessage(msgErrorDownloadFailed, [IntToStr(FResult.HTTPStatusCode), FResult.HTTPStatusText]))
      else
        raise Exception.Create(Format('%d %s', [FResult.HTTPStatusCode, FResult.HTTPStatusText]));
    end;
  end;
end;

function DownloadThreadFunc(Parameter: Pointer): Integer;
begin
  const D = THTTPDataReceiver(Parameter);
  try
    D.DoDownload;
  except
    const Ex = AcquireExceptionObject;
    MemoryBarrier;
    D.FResult.SavedFatalException := Ex;
  end;
  MemoryBarrier;
  Result := 0;
end;

function THTTPDataReceiver.Download(const UseSetupMessagesForErrors: Boolean): Int64;
begin
  var ThreadID: TThreadID;
  const ThreadHandle = BeginThread(nil, 0, DownloadThreadFunc, Self, 0, ThreadID);
  if ThreadHandle = 0 then
    raise Exception.Create('Failed to create download thread: ' + SysErrorMessage(GetLastError));

  try
    try
      while True do begin
        case WaitForSingleObject(ThreadHandle, 50) of
          WAIT_OBJECT_0: Break;
          WAIT_TIMEOUT: HandleProgress;
          WAIT_FAILED: raise Exception.Create('WaitForSingleObject failed: ' + SysErrorMessage(GetLastError));
        else
          raise Exception.Create('WaitForSingleObject returned unknown value');
        end;
      end;
    except
    	{ If an exception was raised during the loop (most likely it would
        be from the user's OnDownloadProgress handler), request abort
        and make one more attempt to wait on the thread. }
      FAbort := True; { Atomic so no lock }
      WaitForSingleObject(ThreadHandle, INFINITE);
      raise;
    end;
  finally
    CloseHandle(ThreadHandle);
  end;

  HandleProgress;
  HandleResult(UseSetupMessagesForErrors);

  Result := FResult.FileSize;
end;

function MaskPasswordInUrl(const Url: String): String;
var
  Uri: TUri;
begin
  Uri := TUri.Create(Url);
  if Uri.Password <> '' then begin
    Uri.Password := '***';
    Result := Uri.ToString;
  end else
    Result := URL;
end;

var
  DownloadTemporaryFileUser, DownloadTemporaryFilePass: String;

procedure SetDownloadTemporaryFileCredentials(const User, Pass: String);
begin
  DownloadTemporaryFileUser := User;
  DownloadTemporaryFilePass := Pass;
end;

function GetISSigUrl(const Url, ISSigUrl: String): String;
begin
  if ISSigUrl <> '' then
    Result := ISSigUrl
  else begin
    const Uri = TUri.Create(Url); { This is a record so no need to free }
    Uri.Path := Uri.Path + ISSigExt;
    Result := Uri.ToString;
  end;
end;

function DownloadFile(const Url, CustomUserName, CustomPassword: String;
  const DestF: TFile; [ref] const Verification: TSetupFileVerification; const ISSigSourceFilename: String;
  const OnSimpleDownloadProgress: TOnSimpleDownloadProgress;
  const OnSimpleDownloadProgressParam: Int64): Int64;
var
  HTTPDataReceiver: THTTPDataReceiver;
begin
  if Url = '' then
    InternalError('DownloadFile: Invalid Url value');

  LogFmt('Downloading file from %s', [MaskPasswordInURL(Url)]);

  HTTPDataReceiver := THTTPDataReceiver.Create(Url, CustomUserName, CustomPassword, DestF);
  try
    HTTPDataReceiver.OnSimpleDownloadProgress := OnSimpleDownloadProgress;
    HTTPDataReceiver.OnSimpleDownloadProgressParam := OnSimpleDownloadProgressParam;

    { Download to specified handle }
    Result := HTTPDataReceiver.Download(False);
    
    { Check verification if specified, otherwise check everything else we can check }
    if Verification.Typ <> fvNone then begin
      var ExpectedFileHash: TSHA256Digest;
      if Verification.Typ = fvHash then
        ExpectedFileHash := Verification.Hash
      else
        DoISSigVerify(DestF, nil, ISSigSourceFilename, False, Verification.ISSigAllowedKeys, ExpectedFileHash);
      const FileHash = GetSHA256OfFile(DestF);
      if not SHA256DigestsEqual(FileHash, ExpectedFileHash) then
        VerificationError(veFileHashIncorrect);
      Log(VerificationSuccessfulLogMessage);
    end else begin
      if HTTPDataReceiver.ProgressMax > 0 then begin
        if HTTPDataReceiver.Progress <> HTTPDataReceiver.ProgressMax then
          raise Exception.Create(FmtSetupMessage(msgErrorProgress, [IntToStr(HTTPDataReceiver.Progress), IntToStr(HTTPDataReceiver.ProgressMax)]))
        else if HTTPDataReceiver.ProgressMax <> Result then
          raise Exception.Create(FmtSetupMessage(msgErrorFileSize, [IntToStr(HTTPDataReceiver.ProgressMax), IntToStr(Result)]));
      end;
    end;
  finally
    HTTPDataReceiver.Free;
  end;
end;

function DownloadTemporaryFile(const Url, BaseName: String;
  [ref] const Verification: TSetupFileVerification; const OnDownloadProgress: TOnDownloadProgress;
  out DestFile: String): Int64;
var
  TempFile: String;
  TempF: TFile;
  TempFileLeftOver: Boolean;
  HTTPDataReceiver: THTTPDataReceiver;
  RetriesLeft: Integer;
  LastError: DWORD;
begin
  if Url = '' then
    InternalError('DownloadTemporaryFile: Invalid Url value');
  if BaseName = '' then
    InternalError('DownloadTemporaryFile: Invalid BaseName value');

  DestFile := AddBackslash(TempInstallDir) + BaseName;

  LogFmt('Downloading temporary file from %s: %s', [MaskPasswordInURL(Url), DestFile]);

  { Does not disable FS redirection, like everything else working on the temp dir }

  { Prepare directory }
  if NewFileExists(DestFile) then begin
    if Verification.Typ = fvHash then begin
      if SHA256DigestsEqual(GetSHA256OfFile(False, DestFile), Verification.Hash) then begin
        Log('  File already downloaded.');
        Result := 0;
        Exit;
      end;
    end else if Verification.Typ = fvISSig then begin
      var ExistingFileName: String;
      var ExistingFileSize: Int64;
      var ExistingFileHash: TSHA256Digest;
      if ISSigVerifySignature(DestFile, GetISSigAllowedKeys(ISSigAvailableKeys, Verification.ISSigAllowedKeys),
           ExistingFileName, ExistingFileSize, ExistingFileHash, nil, nil, nil) then begin
        const DestF = TFile.Create(DestFile, fdOpenExisting, faRead, fsReadWrite);
        try
          { Not checking ExistingFileName because we can't be sure what the original filename was }
          if (DestF.Size = ExistingFileSize) and
             (SHA256DigestsEqual(GetSHA256OfFile(DestF), ExistingFileHash)) then begin
            Log('  File already downloaded.');
            Result := 0;
            Exit;
          end;
        finally
          DestF.Free;
        end;
      end;
    end;

    SetFileAttributes(PChar(DestFile), GetFileAttributes(PChar(DestFile)) and not FILE_ATTRIBUTE_READONLY);
    DelayDeleteFile(False, DestFile, 13, 50, 250);
  end else
    ForceDirectories(False, PathExtractPath(DestFile));

  { Create temporary file }
  TempFile := GenerateUniqueName(False, PathExtractPath(DestFile), '.tmp');
  TempF := TFile.Create(TempFile, fdCreateAlways, faWrite, fsNone);
  TempFileLeftOver := True;

  HTTPDataReceiver := THTTPDataReceiver.Create(Url,
      DownloadTemporaryFileUser, DownloadTemporaryFilePass, TempF);
  try
    HTTPDataReceiver.BaseName := BaseName;
    HTTPDataReceiver.OnDownloadProgress := OnDownloadProgress;

    { To test redirects: https://jrsoftware.org/download.php/is.exe
      To test expired certificates: https://expired.badssl.com/
      To test self-signed certificates: https://self-signed.badssl.com/
      To test basic authentication: https://guest:guest@jigsaw.w3.org/HTTP/Basic/
      To test 100 MB file: https://speed.hetzner.de/100MB.bin
      To test 1 GB file: https://speed.hetzner.de/1GB.bin
      To test file without a content length: https://github.com/jrsoftware/issrc/archive/main.zip }

    { Download to temporary file}
    Result := HTTPDataReceiver.Download(True);

    { Check verification if specified, otherwise check everything else we can check }
    if Verification.Typ <> fvNone then begin
      var ExpectedFileHash: TSHA256Digest;
      if Verification.Typ = fvHash then
        ExpectedFileHash := Verification.Hash
      else
        DoISSigVerify(TempF, nil, DestFile, False, Verification.ISSigAllowedKeys, ExpectedFileHash);
        FreeAndNil(TempF);
        const FileHash = GetSHA256OfFile(False, TempFile);
      if not SHA256DigestsEqual(FileHash, ExpectedFileHash) then
        VerificationError(veFileHashIncorrect);
      Log(VerificationSuccessfulLogMessage);
    end else begin
        FreeAndNil(TempF);
      if HTTPDataReceiver.ProgressMax > 0 then begin
        if HTTPDataReceiver.Progress <> HTTPDataReceiver.ProgressMax then
          raise Exception.Create(FmtSetupMessage(msgErrorProgress, [IntToStr(HTTPDataReceiver.Progress), IntToStr(HTTPDataReceiver.ProgressMax)]))
        else if HTTPDataReceiver.ProgressMax <> Result then
          raise Exception.Create(FmtSetupMessage(msgErrorFileSize, [IntToStr(HTTPDataReceiver.ProgressMax), IntToStr(Result)]));
      end;
    end;

    { Rename the temporary file to the new name now, with retries if needed }
    RetriesLeft := 4;
    while not MoveFile(PChar(TempFile), PChar(DestFile)) do begin
      { Couldn't rename the temporary file... }
      LastError := GetLastError;
      { Does the error code indicate that it is possibly in use? }
      if LastErrorIndicatesPossiblyInUse(LastError, True) then begin
        LogFmt('  The existing file appears to be in use (%d). ' +
          'Retrying.', [LastError]);
        Dec(RetriesLeft);
        Sleep(1000);
        if RetriesLeft > 0 then
          Continue;
      end;
      { Some other error occurred, or we ran out of tries }
      SetLastError(LastError);
      Win32ErrorMsg('MoveFile'); { Throws an exception }
    end;
    TempFileLeftOver := False;
  finally
    TempF.Free;
    HTTPDataReceiver.Free;
    if TempFileLeftOver then
      DeleteFile(TempFile);
  end;
end;

function DownloadTemporaryFile(const Url, BaseName: String;
  [ref] const Verification: TSetupFileVerification; const OnDownloadProgress: TOnDownloadProgress): Int64;
begin
  var DestFile: String;
  Result := DownloadTemporaryFile(Url, BaseName, Verification, OnDownloadProgress, DestFile);
end;

procedure DownloadTemporaryFileSizeAndDate(const Url: String; var FileSize: Int64; var FileDate: String);
var
  HTTPClient: THTTPClient;
  HTTPResponse: IHTTPResponse;
  User, Pass, CleanUrl: string;
  HasCredentials : Boolean;
  Base64: TBase64Encoding;
begin
  HTTPClient := THTTPClient.Create;
  Base64 := nil;
  try
    HasCredentials := GetCredentialsAndCleanUrl(Url,
      DownloadTemporaryFileUser, DownloadTemporaryFilePass, User, Pass, CleanUrl);
    if HasCredentials then begin
      Base64 := TBase64Encoding.Create(0);
      HTTPClient.CustomHeaders['Authorization'] := 'Basic ' + Base64.Encode(User + ':' + Pass);
    end;
    SetUserAgentAndSecureProtocols(HTTPClient);
    HTTPResponse := HTTPClient.Head(CleanUrl);
    if (HTTPResponse.StatusCode < 200) or (HTTPResponse.StatusCode > 299) then
      raise Exception.Create(FmtSetupMessage(msgErrorDownloadSizeFailed, [IntToStr(HTTPResponse.StatusCode), HTTPResponse.StatusText]))
    else begin
      FileSize := HTTPResponse.ContentLength;
      FileDate := HTTPResponse.LastModified;
    end;
  finally
    Base64.Free;
    HTTPClient.Free;
  end;
end;

function DownloadTemporaryFileSize(const Url: String): Int64;
var
  FileSize: Int64;
  FileDate: String;
begin
  if Url = '' then
    InternalError('DownloadTemporaryFileSize: Invalid Url value');
  LogFmt('Getting size of %s.', [MaskPasswordInUrl(Url)]);
  DownloadTemporaryFileSizeAndDate(Url, FileSize, FileDate);
  Result := FileSize;
end;

function DownloadTemporaryFileDate(const Url: String): String;
var
  FileSize: Int64;
  FileDate: String;
begin
  if Url = '' then
    InternalError('DownloadTemporaryFileDate: Invalid Url value');
  LogFmt('Getting last modified date of %s.', [MaskPasswordInUrl(Url)]);
  DownloadTemporaryFileSizeAndDate(Url, FileSize, FileDate);
  Result := FileDate;
end;

end.
