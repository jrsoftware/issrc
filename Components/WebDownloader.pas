{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: WebDownloader.pas, released on 2007-08-19.

The Initial Developer of the Original Code is Andreas Hausladen
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.
----------------------------------------------------------------------------}
// $Id$

unit WebDownloader;

interface

uses
  Windows, Messages, SysUtils, Forms, Classes;

const
  WM_PROGRESS = WM_USER + 1;

type
  TWebDownloader = class;

  TProcessEventsProc = procedure;
  TWebDownloaderEvent = procedure(Sender: TWebDownloader);
  TWebDownloaderGrabberProc = procedure of object;

  TWebDownloaderStatus = (wdsInactive, wdsConnecting, wdsDisconnecting,
                          wdsDownloading, wdsFinished, wdsError);
  TWebDownloaderProxyMode = (pmNoProxy, pmSysConfig, pmManual);

  TWebDownloaderThread = class(TThread)
  private
    FGrabberProc: TWebDownloaderGrabberProc;
    FExceptionMessage: string;
  protected
    procedure Execute; override;
  public
    constructor Create(AGrabberProc: TWebDownloaderGrabberProc);
    property ExceptionMessage: string read FExceptionMessage;
  end;

  TWebDownloader = class(TObject)
  private
    FOnProcessEvents: TProcessEventsProc;
    FOnStatus: TWebDownloaderEvent;
    FOnProgress: TWebDownloaderEvent;
    FHandle: THandle;
    FUrl, FUserName, FPassword: string;
    FPort: Integer;
    FErrorText: string;
    FThread: TWebDownloaderThread;
    FStatus: TWebDownloaderStatus;
    FProxyMode: TWebDownloaderProxyMode;
    FProxyIgnoreList: string;
    FProxyAddresses: string;
    FAgent: string;
    FReferer: string;
    FProxyUserName: string;
    FProxyPassword: string;
    FStream: TStream;
    FTotalSize: Cardinal;
    FBytesRead: Cardinal;
    FFtpTextMode: Boolean;
    FFtpPassive: Boolean;
    FStartTime: TDateTime;
    FDownloadStartTime: TDateTime;
    FInProgress: Boolean;
    FTimeout: Cardinal;
    FProtocol: string;
  protected
    procedure ParseUrl(URL: string; Protocol: string;
      var Host: string; var FileName: string; var UserName: string;
      var Password: string; var Port: Cardinal);
    procedure WndProc(var Msg: TMessage);

    procedure Synchronize(Proc: TThreadMethod);
    procedure GrabHttp;
    procedure GrabFtp;

    procedure SetStatus(NewStatus: TWebDownloaderStatus);

    procedure TriggerStatus;
    procedure TriggerError;
    procedure TriggerProgress;

    property Thread: TWebDownloaderThread read FThread;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DownloadFile(const AUrl: string; AStream: TStream);

    function GetElapsedTime: TDateTime;
    function GetElapsedTimeStr: string;
    function GetRemainingTime: TDateTime;
    function GetRemainingTimeStr: string;

    property Url: string read FUrl;
    property Status: TWebDownloaderStatus read FStatus;
    property ErrorText: string read FErrorText;
    property BytesRead: Cardinal read FBytesRead;
    property TotalSize: Cardinal read FTotalSize;

    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Port: Integer read FPort write FPort;
    property Timeout: Cardinal read FTimeout write FTimeout;

    property Agent: string read FAgent write FAgent;
    property Referer: string read FReferer write FReferer;

    property ProxyMode: TWebDownloaderProxyMode read FProxyMode write FProxyMode default pmSysConfig;
    property ProxyAddresses: string read FProxyAddresses write FProxyAddresses;
    property ProxyIgnoreList: string read FProxyIgnoreList write FProxyIgnoreList;
    property ProxyUserName: string read FProxyUserName write FProxyUserName;
    property ProxyPassword: string read FProxyPassword write FProxyPassword;

    property FtpTextMode: Boolean read FFtpTextMode write FFtpTextMode;
    property FtpPassive: Boolean read FFtpPassive write FFtpPassive;

    property OnProcessEvents: TProcessEventsProc read FOnProcessEvents write FOnProcessEvents;
    property OnStatus: TWebDownloaderEvent read FOnStatus write FOnStatus;
    property OnProgress: TWebDownloaderEvent read FOnProgress write FOnProgress;
  end;

implementation

{ Copied from WinInet.pas }
const
  wininetdll = 'wininet.dll';

const
  INTERNET_OPEN_TYPE_PRECONFIG        = 0;  { use registry configuration }
  INTERNET_OPEN_TYPE_DIRECT           = 1;  { direct to net }
  INTERNET_OPEN_TYPE_PROXY            = 3;  { via named proxy }

  INTERNET_SERVICE_URL = 0;
  INTERNET_SERVICE_FTP = 1;
  INTERNET_SERVICE_GOPHER = 2;
  INTERNET_SERVICE_HTTP = 3;

  INTERNET_DEFAULT_HTTPS_PORT = 443;

  INTERNET_FLAG_RELOAD            = $80000000;  { retrieve the original item }
  INTERNET_FLAG_PRAGMA_NOCACHE    = $00000100;  { asking wininet to add "pragma: no-cache" }
  INTERNET_FLAG_NO_CACHE_WRITE    = $04000000;  { don't write this item to the cache }
  INTERNET_FLAG_DONT_CACHE        = INTERNET_FLAG_NO_CACHE_WRITE;
  INTERNET_FLAG_SECURE            = $00800000;  { use PCT/SSL if applicable (HTTP) }

  INTERNET_OPTION_CONNECT_TIMEOUT = 2;
  INTERNET_OPTION_PROXY_USERNAME  = 43;
  INTERNET_OPTION_PROXY_PASSWORD  = 44;

  HTTP_QUERY_CONTENT_LENGTH = 5;
  HTTP_QUERY_STATUS_CODE    = 19; { special: part of status line }

  INTERNET_FLAG_PASSIVE = $08000000;                { used for FTP connections }

  { flags for FTP }
  FTP_TRANSFER_TYPE_UNKNOWN   = $00000000;
  FTP_TRANSFER_TYPE_ASCII     = $00000001;
  FTP_TRANSFER_TYPE_BINARY    = $00000002;

{$IFDEF UNICODE}
  ExternalEncoding = 'W';
{$ELSE}
  ExternalEncoding = 'A';
{$ENDIF UNICODE}

type
  HINTERNET = Pointer;

  INTERNET_STATUS_CALLBACK = TFarProc;
  TFNInternetStatusCallback = INTERNET_STATUS_CALLBACK;
  PFNInternetStatusCallback = ^TFNInternetStatusCallback;

  INTERNET_PORT = Word;

function InternetGetLastResponseInfo(var lpdwError: DWORD; lpszBuffer: PChar;
  var lpdwBufferLength: DWORD): BOOL; stdcall;
  external wininetdll name 'InternetGetLastResponseInfo' + ExternalEncoding;

function InternetOpen(lpszAgent: PChar; dwAccessType: DWORD;
  lpszProxy, lpszProxyBypass: PChar; dwFlags: DWORD): HINTERNET; stdcall;
  external wininetdll name 'InternetOpen' + ExternalEncoding;

function InternetSetStatusCallback(hInet: HINTERNET;
  lpfnInternetCallback: PFNInternetStatusCallback): PFNInternetStatusCallback; stdcall;
  external wininetdll name 'InternetSetStatusCallback';

function InternetConnect(hInet: HINTERNET; lpszServerName: PChar;
  nServerPort: INTERNET_PORT; lpszUsername: PChar; lpszPassword: PChar;
  dwService: DWORD; dwFlags: DWORD; dwContext: DWORD): HINTERNET; stdcall;
  external wininetdll name 'InternetConnect' + ExternalEncoding;

function HttpOpenRequest(hConnect: HINTERNET; lpszVerb: PChar;
  lpszObjectName: PChar; lpszVersion: PChar; lpszReferrer: PChar;
  lplpszAcceptTypes: PLPSTR; dwFlags: DWORD;
  dwContext: DWORD): HINTERNET; stdcall;
  external wininetdll name 'HttpOpenRequest' + ExternalEncoding;

function InternetSetOption(hInet: HINTERNET; dwOption: DWORD;
  lpBuffer: Pointer; dwBufferLength: DWORD): BOOL; stdcall;
  external wininetdll name 'InternetSetOption' + ExternalEncoding;

function HttpSendRequest(hRequest: HINTERNET; lpszHeaders: PChar;
  dwHeadersLength: DWORD; lpOptional: Pointer;
  dwOptionalLength: DWORD): BOOL; stdcall;
  external wininetdll name 'HttpSendRequest' + ExternalEncoding;

function HttpQueryInfo(hRequest: HINTERNET; dwInfoLevel: DWORD;
  lpvBuffer: Pointer; var lpdwBufferLength: DWORD;
  var lpdwReserved: DWORD): BOOL; stdcall;
  external wininetdll name 'HttpQueryInfo' + ExternalEncoding;

function InternetReadFile(hFile: HINTERNET; lpBuffer: Pointer;
  dwNumberOfBytesToRead: DWORD; var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall;
  external wininetdll name 'InternetReadFile';

function InternetCloseHandle(hInet: HINTERNET): BOOL; stdcall;
  external wininetdll name 'InternetCloseHandle';

function FtpOpenFile(hConnect: HINTERNET; lpszFileName: PChar;
  dwAccess: DWORD; dwFlags: DWORD; dwContext: DWORD): HINTERNET; stdcall;
  external wininetdll name 'FtpOpenFile' + ExternalEncoding;

function FtpGetFileSize(hFile: HINTERNET; lpdwFileSizeHigh: LPDWORD): DWORD; stdcall;
  external wininetdll name 'FtpGetFileSize';

{---------------------------------------------------------------------------------}

type
  TBufferedStream = class(TObject)
  private
    FBuffer: array [0..65535] of Byte;
    FBufferPos: Integer;
    FStream: TStream;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;

    procedure Write(const Buf; BufSize: Integer);
    procedure Flush;
  end;

{ TBufferedStream }

constructor TBufferedStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

destructor TBufferedStream.Destroy;
begin
  Flush;
  inherited Destroy;
end;

procedure TBufferedStream.Flush;
begin
  if FBufferPos > 0 then
    FStream.Write(FBuffer, FBufferPos);
  FBufferPos := 0;
end;

procedure TBufferedStream.Write(const Buf; BufSize: Integer);
begin
  if BufSize < 0 then
    Exit;

  if BufSize >= SizeOf(FBuffer) then
  begin
    Flush;
    FStream.Write(Buf, BufSize);
  end
  else
  begin
    if SizeOf(FBuffer) - FBufferPos < BufSize then
      Flush;
    Move(Buf, FBuffer[FBufferPos], BufSize);
    Inc(FBufferPos, BufSize);
  end;
end;

function FormatDownloadTime(t: TDateTime): string;
var
  h, m, s, msec: Word;
  d: Integer;
begin
  Result := '';
  d := Trunc(t);
  DecodeTime(t, h, m, s, msec);
  if (d = 0) and (h > 0) then
    Result := Result + 'h:';
  Result := Result + 'nn:ss';
  if d > 0 then
    Result := IntToStr(d * 24 + h) + ':' + FormatDateTime(Result, t)
  else
    Result := FormatDateTime(Result, t);
end;

function GetLastInternetError: string;
var
  dwIndex: DWORD;
  dwBufLen: DWORD;
  Buffer: PChar;
begin
  dwIndex := 0;
  dwBufLen := 1024;
  GetMem(Buffer, dwBufLen * SizeOf(Char));
  try
    InternetGetLastResponseInfo(dwIndex, Buffer, dwBufLen);
    Result := StrPas(Buffer);
  finally
    FreeMem(Buffer);
  end;
end;

{ TWebDownloaderThread }

constructor TWebDownloaderThread.Create(AGrabberProc: TWebDownloaderGrabberProc);
begin
  inherited Create(False);
  FGrabberProc := AGrabberProc;
  Resume;
end;

procedure TWebDownloaderThread.Execute;
begin
  try
    FGrabberProc;
  except
    on EAbort do
      ;
    on E: Exception do
      FExceptionMessage := E.Message;
  end;
  Terminate;
end;

{ TWebDownloader }

constructor TWebDownloader.Create;
begin
  inherited Create;
  FHandle := AllocateHWnd(WndProc);
  FProxyMode := pmSysConfig;
end;

destructor TWebDownloader.Destroy;
begin
  DeallocateHWnd(FHandle);
  inherited Destroy;
end;

procedure TWebDownloader.DownloadFile(const AUrl: string; AStream: TStream);
var
  I: Integer;
  GrabberProc: TWebDownloaderGrabberProc;
begin
  Assert( FThread = nil );
  FStream := AStream;
  FStatus := wdsInactive;
  FUrl := AUrl;
  FErrorText := '';
  FTotalSize := 0;
  FBytesRead := 0;
  FStartTime := Now;
  FDownloadStartTime := 0.0;

  if Assigned(FOnProcessEvents) then
    FOnProcessEvents;

  I := Pos('://', AUrl);
  FProtocol := Copy(AUrl, 1, I - 1);
  if (CompareText(FProtocol, 'http') = 0) or (CompareText(FProtocol, 'https') = 0) then
    GrabberProc := GrabHttp
  else
  if CompareText(FProtocol, 'ftp') = 0 then
    GrabberProc := GrabFtp
  else
    // cannot be reached because the caller only allows http and ftp
    raise Exception.CreateFmt('Unsupported protocol "%s"', [FProtocol]);

  FThread := TWebDownloaderThread.Create(GrabberProc);
  try
    while not Thread.Terminated do
    begin
      if Assigned(FOnProcessEvents) then
        FOnProcessEvents
      else
        Application.ProcessMessages;
      SleepEx(10, True);
    end;
    if Thread.ExceptionMessage <> '' then
    begin
      SetStatus(wdsError);
      raise Exception.Create(Thread.ExceptionMessage);
    end;
  finally
    FThread.Free;
    FThread := nil;
  end;
  if FStatus <> wdsError then
    SetStatus(wdsInactive);
end;

procedure TWebDownloader.Synchronize(Proc: TThreadMethod);
begin
  if FThread <> nil then
    FThread.Synchronize(Proc)
  else
    Proc;
end;

// Code from JVCL JvUrlGrabbers
procedure TWebDownloader.ParseUrl(URL: string; Protocol: string;
  var Host: string; var FileName: string; var UserName: string;
  var Password: string; var Port: Cardinal);
var
  Err: Integer;
  Ps: Integer;
begin
  // Default return values
  Host := '';
  FileName := '';
  UserName := '';
  Password := '';
  Port := 0;

  // Remove the protocol part from the given Value
  if Pos(UpperCase(Protocol), UpperCase(URL)) <> 0 then
    URL := Copy(URL, Length(Protocol) + 1, Length(URL));

  // Get the filename, if any
  if Pos('/', URL) <> 0 then
  begin
    Ps := Pos('/', URL);
    Host := Copy(URL, 1, Ps - 1);
    FileName := Copy(URL, Ps + 1, Length(URL));
  end
  else
    Host := URL;

  // Get the username password couple
  Ps := Pos('@', Host);
  if Ps <> 0 then
  begin
    UserName := Copy(Host, 1, Ps - 1);
    Host := Copy(Host, Ps + 1, Length(Host));
    // now, figure out if there is a password
    Ps := Pos(':', UserName);
    if Ps <> 0 then
    begin
      Password := Copy(UserName, Ps + 1, Length(UserName));
      UserName := Copy(UserName, 1, Ps - 1);
    end;
  end;

  // Get the port
  Ps := Pos(':', Host);
  if Ps <> 0 then
  begin
    Val(Copy(Host, Ps + 1, Length(Host)), Port, Err);
    Host := Copy(Host, 1, Ps - 1);
    if Err <> 0 then
      Port := 0;
  end;
end;

procedure TWebDownloader.SetStatus(NewStatus: TWebDownloaderStatus);
begin
  if NewStatus <> FStatus then
  begin
    FStatus := NewStatus;
    if (FThread <> nil) and Assigned(FOnStatus) then
      FThread.Synchronize(TriggerStatus);
  end;
end;

procedure TWebDownloader.TriggerError;
begin
  FStatus := wdsError;
  TriggerStatus;
end;

procedure TWebDownloader.TriggerProgress;
begin
  try
    if Assigned(FOnProgress) then
      FOnProgress(Self);
  finally
    FInProgress := False;
  end;
end;

procedure TWebDownloader.TriggerStatus;
begin
  if Assigned(FOnStatus) then
    FOnStatus(Self);
end;

procedure TWebDownloader.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_PROGRESS then
  begin
    TriggerProgress;
    Msg.Result := 1;
  end
  else
  with Msg do
    Result := DefWindowProc(FHandle, Msg, WParam, LParam);
end;

function TWebDownloader.GetElapsedTime: TDateTime;
begin
  Result := Now - FStartTime;
end;

function TWebDownloader.GetElapsedTimeStr: string;
begin
  Result := FormatDownloadTime(GetElapsedTime);
end;

function TWebDownloader.GetRemainingTime: TDateTime;
const
  OneSecond = 1/SecsPerDay;
var
  Diff: TDateTime;
begin
  Result := 0;
  if (TotalSize > 0) and (BytesRead > 0) and (FDownloadStartTime > 0.0) then
  begin
    Diff := Now - FDownloadStartTime;
    if Diff > OneSecond then
      Result := (TotalSize - BytesRead) * Diff / BytesRead + OneSecond;
  end;
end;

function TWebDownloader.GetRemainingTimeStr: string;
var
  t: TDateTime;
begin
  t := GetRemainingTime;
  if t > 0 then
    Result := FormatDownloadTime(GetRemainingTime)
  else
    Result := '';
end;

procedure DownloadCallBack(Handle: HINTERNET; Context: DWORD;
  AStatus: DWORD; Info: Pointer; StatLen: DWORD); stdcall;
begin
  with TWebDownloader(Context) do
    Synchronize(TriggerStatus);
end;

procedure TWebDownloader.GrabHttp;
var
  hSession, hHostConnection, hDownload: HINTERNET;
  HostName, FileName, strUserName, strPassword: string;
  UserName, Password: PChar;
  Port: Cardinal;
  Buffer: PChar;
  dwBufLen, dwIndex, dwBytesRead, dwTotalBytes: DWORD;
  HasSize: Boolean;
  BufferedStream: TBufferedStream;
  Buf: array [0..32*1024 - 1] of Byte;
  Flags: DWORD;
begin
  Buffer := nil;

  hSession := nil;
  hHostConnection := nil;
  hDownload := nil;
  BufferedStream := nil;
  try
    try
      ParseUrl(FUrl, FProtocol + '://', HostName, FileName, strUserName, strPassword, Port);
      if strUserName = '' then
        strUserName := Self.UserName;
      if strPassword = '' then
        strPassword := Self.Password;
      if Port = 0 then
        Port := Self.Port;
      if (Port = 0) and (CompareText(FProtocol, 'https') = 0) then
        Port := INTERNET_DEFAULT_HTTPS_PORT;

      // Setup the PChars for the call to InternetConnect
      if strUserName = '' then
        UserName := nil
      else
        UserName := PChar(strUserName);
      if strPassword = '' then
        Password := nil
      else
        Password := PChar(strPassword);

      FErrorText := '';

      //Connect to the web
      SetStatus(wdsConnecting);
      case ProxyMode of
        pmNoProxy:
          hSession := InternetOpen(PChar(Agent), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
        pmSysConfig:
          hSession := InternetOpen(PChar(Agent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
        pmManual:
          hSession := InternetOpen(PChar(Agent), INTERNET_OPEN_TYPE_PROXY, PChar(ProxyAddresses), PChar(ProxyIgnoreList), 0);
      end;
      if hSession = nil then
      begin
        FErrorText := SysErrorMessage(GetLastError);
        Synchronize(TriggerError);
        Exit;
      end;
      InternetSetStatusCallback(hSession, PFNInternetStatusCallback(@DownloadCallBack));

      // Connect to the host
      hHostConnection := InternetConnect(hSession, PChar(HostName), Port,
        UserName, Password, INTERNET_SERVICE_HTTP, 0, DWORD(Self));

      if Thread.Terminated then
        Exit;

      if hHostConnection = nil then
      begin
        FErrorText := GetLastInternetError;
        Buffer := nil;
        Synchronize(TriggerError);
        Exit;
      end;

      //Request the file
      Flags := INTERNET_FLAG_RELOAD or INTERNET_FLAG_PRAGMA_NOCACHE;
      if CompareText(FProtocol, 'https') = 0 then
        Flags := Flags or INTERNET_FLAG_SECURE;
      hDownload := HttpOpenRequest(hHostConnection, 'GET', PChar(FileName), 'HTTP/1.0',
        PChar(Referer), nil, Flags, 0);

      if hDownload = nil then
      begin
        FErrorText := GetLastInternetError;
        Synchronize(TriggerError);
        Exit;
      end;

      if ProxyMode in [pmManual, pmSysConfig] then
      begin
        // if manual mode and valid session, we set proxy user name and password
        InternetSetOption(hDownload,
                          INTERNET_OPTION_PROXY_USERNAME,
                          PChar(ProxyUserName),
                          Length(ProxyUserName) + 1);
        InternetSetOption(hDownload,
                          INTERNET_OPTION_PROXY_PASSWORD,
                          PChar(ProxyPassword),
                          Length(ProxyPassword) + 1);
      end;
      if FTimeout > 0 then
        InternetSetOption(hDownload,
                          INTERNET_OPTION_CONNECT_TIMEOUT,
                          @FTimeout,
                          SizeOf(FTimeout));

      //Send the request
      HttpSendRequest(hDownload, nil, 0, nil, 0);

      if Thread.Terminated then
        Exit;

      dwIndex := 0;
      dwBufLen := 1024;
      GetMem(Buffer, dwBufLen * SizeOf(Char));

      if not HttpQueryInfo(hDownload, HTTP_QUERY_STATUS_CODE, Buffer, dwBufLen, dwIndex) or (StrPas(Buffer) <> '200') then
      begin
        FErrorText := GetLastInternetError;
        Synchronize(TriggerError);
        Exit;
      end;

      dwIndex := 0;
      dwBufLen := 1024;
      HasSize := HttpQueryInfo(hDownload, HTTP_QUERY_CONTENT_LENGTH, Buffer, dwBufLen, dwIndex);
      if Thread.Terminated then
        Exit;

      if HasSize then
        FTotalSize := StrToInt(StrPas(Buffer))
      else
        FTotalSize := 0;
      FStream.Size := TotalSize; // allocate memory/disk space
      FStream.Position := 0;

      BufferedStream := TBufferedStream.Create(FStream);

      dwTotalBytes := 0;
      SetStatus(wdsDownloading);
      if HasSize then
      begin
        dwBytesRead := 1;
        while (dwBytesRead > 0) and not Thread.Terminated do
        begin
          if not InternetReadFile(hDownload, @Buf, SizeOf(Buf), dwBytesRead) then
            dwBytesRead := 0
          else
          begin
            Inc(dwTotalBytes, dwBytesRead);
            FBytesRead := dwTotalBytes;
            BufferedStream.Write(Buf, dwBytesRead);
            if not FInProgress then
            begin
              FInProgress := True;
              PostMessage(FHandle, WM_PROGRESS, 0, 0);
            end;
          end;

          if FDownloadStartTime = 0.0 then
            FDownloadStartTime := Now;
        end;
        FStream.Size := FStream.Position; // truncate if the file we got is smaller than what was reported in the header
      end
      else
      begin
        while InternetReadFile(hDownload, @Buf, SizeOf(Buf), dwBytesRead) and not Thread.Terminated do
        begin
          if dwBytesRead = 0 then
            Break;
          Inc(dwTotalBytes, dwBytesRead);
          FBytesRead := dwTotalBytes;
          BufferedStream.Write(Buf, dwBytesRead);
          if not FInProgress then
          begin
            FInProgress := True;
            PostMessage(FHandle, WM_PROGRESS, 0, 0);
          end;

          if FDownloadStartTime = 0.0 then
            FDownloadStartTime := Now;
        end;
      end;
      if not Thread.Terminated then
        SetStatus(wdsFinished);
    except
    end;
    SetStatus(wdsDisconnecting);
  finally
    // Free all stuff's
    if Buffer <> nil then
      FreeMem(Buffer);
    BufferedStream.Free;

    // Release all handles
    if (hDownload <> nil) and not InternetCloseHandle(hDownload) then
    begin
      FErrorText := GetLastInternetError;
      Synchronize(TriggerError);
    end;
    if (hHostConnection <> nil) and not InternetCloseHandle(hHostConnection) then
    begin
      FErrorText := GetLastInternetError;
      Synchronize(TriggerError);
    end;
    if (hSession <> nil) and not InternetCloseHandle(hSession) then
    begin
      FErrorText := GetLastInternetError;
      Synchronize(TriggerError);
    end;
  end;
end;

// Code adapted from JVCL JvUrlGrabbers
procedure TWebDownloader.GrabFtp;
const
  cPassive: array [Boolean] of DWORD = (0, INTERNET_FLAG_PASSIVE);
var
  hSession, hHostConnection, hDownload: HINTERNET;
  HostName, FileName, strUserName, strPassword: string;
  UserName, Password: PChar;
  Port: Cardinal;
  LocalBytesRead, TotalBytes: DWORD;
  Buf: array [0..32*1024 - 1] of Byte;
  dwFileSizeHigh: DWORD;
  BufferedStream: TBufferedStream;
begin
  hSession := nil;
  hHostConnection := nil;
  hDownload := nil;
  BufferedStream := nil;
  try
    try
      FErrorText := '';
      ParseUrl(Url, 'ftp://', HostName, FileName, strUserName, strPassword, Port);
      if strUserName = '' then
        strUserName := Self.UserName;
      if strPassword = '' then
        strPassword := Self.Password;
      if Port = 0 then
        Port := Self.Port;

      // Setup the PChars for the call to InternetConnect
      if strUserName = '' then
        UserName := nil
      else
        UserName := PChar(strUserName);
      if strPassword = '' then
        Password := nil
      else
        Password := PChar(strPassword);

      // Connect to the web
      SetStatus(wdsConnecting);
      case ProxyMode of
        pmNoProxy:
          hSession := InternetOpen(PChar(Agent), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
        pmSysConfig:
          hSession := InternetOpen(PChar(Agent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
        pmManual:
          hSession := InternetOpen(PChar(Agent), INTERNET_OPEN_TYPE_PROXY, PChar(ProxyAddresses), PChar(ProxyIgnoreList), 0);
      end;
      if hSession = nil then
      begin
        FErrorText := GetLastInternetError;
        Synchronize(TriggerError);
        Exit;
      end;
      if ProxyMode = pmManual then
      begin
        // if manual mode and valid session, we set proxy user name and password
        InternetSetOption(hSession, INTERNET_OPTION_PROXY_USERNAME, PChar(ProxyUserName), Length(ProxyUserName) + 1);
        InternetSetOption(hSession, INTERNET_OPTION_PROXY_PASSWORD, PChar(ProxyPassword), Length(ProxyPassword) + 1);
      end;

//      InternetSetStatusCallback(hSession, PFNInternetStatusCallback(@DownloadCallBack));

      // Connect to the hostname
      hHostConnection := InternetConnect(hSession, PChar(HostName), Port,
        UserName, Password, INTERNET_SERVICE_FTP, cPassive[FtpPassive], 0);
      if hHostConnection = nil then
      begin
        FErrorText := GetLastInternetError;
        Synchronize(TriggerError);
        Exit;
      end;

      InternetSetStatusCallback(hHostConnection, PFNInternetStatusCallback(@DownloadCallBack));

      // Request the file
      if not FtpTextMode then
        hDownload := FtpOpenFile(hHostConnection, PChar(FileName), GENERIC_READ,
          FTP_TRANSFER_TYPE_BINARY or INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_RELOAD, 0)
      else
        hDownload := FtpOpenFile(hHostConnection, PChar(FileName), GENERIC_READ,
          FTP_TRANSFER_TYPE_ASCII or INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_RELOAD, 0);

      if Thread.Terminated then
        Exit;

      if hDownload = nil then
      begin
        FErrorText := GetLastInternetError;
        Synchronize(TriggerError);
        Exit;
      end;
      FTotalSize := FtpGetFileSize(hDownload, @dwFileSizeHigh);
      FStream.Size := TotalSize; // allocate memory / disk space
      FStream.Position := 0;

      if Thread.Terminated then
        Exit;

      TotalBytes := 0;
      LocalBytesRead := 1;
      SetStatus(wdsDownloading);
      while (LocalBytesRead <> 0) and not Thread.Terminated do
      begin
        if not InternetReadFile(hDownload, @Buf, SizeOf(Buf), LocalBytesRead) then
          LocalBytesRead := 0
        else
        begin
          Inc(TotalBytes, LocalBytesRead);
          FBytesRead := TotalBytes;
          BufferedStream.Write(Buf, LocalBytesRead);
          if not FInProgress then
          begin
            FInProgress := True;
            PostMessage(FHandle, WM_PROGRESS, 0, 0);
          end;
        end;

        // Be CPU friendly.
        SleepEx(0, True);
        if FDownloadStartTime = 0.0 then
          FDownloadStartTime := Now;
      end;

      if not Thread.Terminated then
        SetStatus(wdsFinished);
    except
    end;
    SetStatus(wdsDisconnecting);
  finally
    BufferedStream.Free;
    //Release all handles
    if (hDownload <> nil) and not InternetCloseHandle(hDownload) then
    begin
      FErrorText := GetLastInternetError;
      Synchronize(TriggerError);
    end;
    if (hHostConnection <> nil) and not InternetCloseHandle(hHostConnection) then
    begin
      FErrorText := GetLastInternetError;
      Synchronize(TriggerError);
    end;
    if (hSession <> nil) and not InternetCloseHandle(hSession) then
    begin
      FErrorText := GetLastInternetError;
      Synchronize(TriggerError);
    end;
  end;
end;

end.

