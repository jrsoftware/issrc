unit Setup.SpawnCommon;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Constants and types shared by the SpawnServer and SpawnClient units
}

interface

uses
  Windows, Messages, SHA256;

const
  { Spawn client -> spawn server messages }
  WM_SpawnServer_ClientConnected = WM_USER + $155A;
  WM_SpawnServer_ProcessRequest  = WM_USER + $155B;
  WM_SpawnServer_ExitNow         = WM_USER + $155C;

  { Possible codes returned by WM_SpawnServer_ProcessRequest handler }
  SPAWN_MSGRESULT_BASE      = $6C8A5700;
  SPAWN_MSGRESULT_OK        = SPAWN_MSGRESULT_BASE + 1;
  SPAWN_MSGRESULT_NOT_READY = SPAWN_MSGRESULT_BASE + 2;

type
  PSpawnServerSharedMemory = ^TSpawnServerSharedMemory;
  TSpawnServerSharedMemory = record
  public const
    ExpectedVersionNumber = {$IFNDEF WIN64} - {$ENDIF} 100;
    ObjectNamePrefix = 'Local\InnoSetupSpawnSharedMemory-';
  public type
    TLockState = (smsFree, smsClientPreparing, smsHandOffToServer,
      smsServerProcessing, smsHandBackToClient, smsClientHandlingResult);
    TRequestResult = (smrUnknown, smrInvalidData, smrExecReturned,
      smrOutOfMemory, smrException);
  public
    StructSize: UInt32;
    VersionNumber: Int32;
    [volatile] ServerWnd, FirstProcessWnd: UInt32;  { HWND }
    [volatile] ClientConnected: LongBool;
    [volatile] ExitNowRequested: LongBool;
    [volatile] ExitNowExitCode: DWORD;
    [volatile] LockState: TLockState;
    LockedFields: record
      [volatile] RequestResult: TRequestResult;
      [volatile] ExecResult: Boolean;
      [volatile] ExecResultCode: DWORD;
      [volatile] DataSize: UInt32;
      [volatile] DataHash: TSHA256Digest;
      [volatile] Data: array[0..$3FFFF] of Byte;
    end;
    function TryChangeLockState(const AFromState, AToState: TLockState): TLockState;
  end;

implementation

{ TSpawnServerSharedMemory }

function TSpawnServerSharedMemory.TryChangeLockState(const AFromState,
  AToState: TLockState): TLockState;
begin
  Byte(Result) := AtomicCmpExchange(Byte(LockState), Byte(AToState),
    Byte(AFromState));
end;

end.
