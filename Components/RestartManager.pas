unit RestartManager;

{
  Basic RestartManager API interface unit by Martijn Laan
}

interface

uses
  ActiveX, Windows;

procedure FreeRestartManagerLibrary;
function InitRestartManagerLibrary: Boolean;
function UseRestartManager: Boolean;

const
  RM_SESSION_KEY_LEN = SizeOf(TGUID);

  CCH_RM_SESSION_KEY = RM_SESSION_KEY_LEN*2;

  CCH_RM_MAX_APP_NAME = 255;

  CCH_RM_MAX_SVC_NAME = 63;

  RM_INVALID_TS_SESSION = -1;

  RM_INVALID_PROCESS = -1;

type
  RM_APP_TYPE = DWORD;

const
  RmUnknownApp = 0;
  RmMainWindow = 1;
  RmOtherWindow = 2;
  RmService = 3;
  RmExplorer = 4;
  RmConsole = 5;
  RmCritical = 1000;

type
  RM_SHUTDOWN_TYPE = DWORD;

const
  RmForceShutdown = $01;
  RmShutdownOnlyRegistered = $10;

  //RM_APP_STATUS

type
  RM_REBOOT_REASON = DWORD;

const
  RmRebootReasonNone = $0;
  RmRebootReasonPermissionDenied = $1;
  RmRebootReasonSessionMismatch = $2;
  RmRebootReasonCriticalProcess = $4;
  RmRebootReasonCriticalService = $8;
  RmRebootReasonDetectedSelf = $10;

type
  RM_UNIQUE_PROCESS = record
    dwProcessId: DWORD;
    ProcessStartTime: TFileTime;
  end;

  RM_PROCESS_INFO = record
    Process: RM_UNIQUE_PROCESS;
    strAppName: array[0..CCH_RM_MAX_APP_NAME] of WideChar;
    strServiceShortName: array[0..CCH_RM_MAX_SVC_NAME] of WideChar;
    ApplicationType: RM_APP_TYPE;
    AppStatus: ULONG;
    TSSessionId: DWORD;
    bRestartable: BOOL;
  end;

  //RM_FILTER_TRIGGER

  //RM_FILTER_ACTION

  //RM_FILTER_INFO

  //RM_WRITE_STATUS_CALLBACK

var
  RmStartSession: function (pSessionHandle: LPDWORD; dwSessionFlags: DWORD; strSessionKey: LPWSTR): DWORD; stdcall;

  RmRegisterResources: function (dwSessionHandle: DWORD; nFiles: UINT; rgsFilenames: Pointer; nApplications: UINT; rgApplications: Pointer; nServices: UINT; rgsServiceNames: Pointer): DWORD; stdcall;

  RmGetList: function (dwSessionHandle: DWORD; pnProcInfoNeeded, pnProcInfo: PUINT; rgAffectedApps: Pointer; lpdwRebootReasons: LPDWORD): DWORD; stdcall;

  RmShutdown: function (dwSessionHandle: DWORD; lActionFlags: ULONG; fnStatus: Pointer): DWORD; stdcall;
  
  RmRestart: function (dwSessionHandle: DWORD; dwRestartFlags: DWORD; fnStatus: Pointer): DWORD; stdcall;

  RmEndSession: function (dwSessionHandle: DWORD): DWORD; stdcall;

implementation

//----------------------------------------------------------------------------------------------------------------------

uses
  SysUtils, PathFunc;

const
  restartmanagerlib = 'Rstrtmgr.dll';

var
  RestartManagerLibrary: THandle;
  ReferenceCount: Integer;  // We have to keep track of several load/unload calls.

procedure FreeRestartManagerLibrary;
begin
  if ReferenceCount > 0 then
    Dec(ReferenceCount);

  if (RestartManagerLibrary <> 0) and (ReferenceCount = 0) then
  begin
    FreeLibrary(RestartManagerLibrary);
    RestartManagerLibrary := 0;

    RmStartSession := nil;
    RmRegisterResources := nil;
    RmGetList := nil;
    RmShutdown := nil;
    RmRestart := nil;
    RmEndSession := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function InitRestartManagerLibrary: Boolean;

  function GetSystemDir: String;
  var
    Buf: array[0..MAX_PATH-1] of Char;
  begin
    GetSystemDirectory(Buf, SizeOf(Buf) div SizeOf(Buf[0]));
    Result := StrPas(Buf);
  end;

begin
  Inc(ReferenceCount);

  if RestartManagerLibrary = 0 then
  begin
    RestartManagerLibrary := LoadLibrary(PChar(AddBackslash(GetSystemDir) + restartmanagerlib));
    if RestartManagerLibrary <> 0 then
    begin
      RmStartSession := GetProcAddress(RestartManagerLibrary, 'RmStartSession');
      RmRegisterResources := GetProcAddress(RestartManagerLibrary, 'RmRegisterResources');
      RmGetList := GetProcAddress(RestartManagerLibrary, 'RmGetList');
      RmShutdown := GetProcAddress(RestartManagerLibrary, 'RmShutdown');
      RmRestart := GetProcAddress(RestartManagerLibrary, 'RmRestart');
      RmEndSession := GetProcAddress(RestartManagerLibrary, 'RmEndSession');
    end;
  end;
  Result := RestartManagerLibrary <> 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function UseRestartManager: Boolean;

begin
  Result := RestartManagerLibrary <> 0;
end;

//----------------------------------------------------------------------------------------------------------------------


initialization
finalization
  while ReferenceCount > 0 do
    FreeRestartManagerLibrary;

end.
