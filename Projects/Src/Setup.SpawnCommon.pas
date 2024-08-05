unit Setup.SpawnCommon;

{
  Inno Setup
  Copyright (C) 1997-2007 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Constants and types shared by the SpawnServer and SpawnClient units
}

interface

uses
  Messages;

const
  { Spawn client -> spawn server messages }
  WM_SpawnServer_Query = WM_USER + $1550;

  { Spawn client -> spawn server WM_COPYDATA messages }
  CD_SpawnServer_Exec      = $4A73E9C0;
  CD_SpawnServer_ShellExec = $4A73E9C1;

  { Possible wParam values in a WM_SpawnServer_Query message } 
  SPAWN_QUERY_STATUS        = 1;
  SPAWN_QUERY_RESULTCODE_LO = 2;
  SPAWN_QUERY_RESULTCODE_HI = 3;

  { Bits set in high word of every response }
  SPAWN_MSGRESULT_SUCCESS_BITS = $6C830000;
  SPAWN_MSGRESULT_FAILURE_BITS = $6C840000;

  { Possible error codes returned by WM_COPYDATA handler }
  SPAWN_MSGRESULT_OUT_OF_MEMORY           = SPAWN_MSGRESULT_FAILURE_BITS or 1;
  SPAWN_MSGRESULT_UNEXPECTED_EXCEPTION    = SPAWN_MSGRESULT_FAILURE_BITS or 2;
  SPAWN_MSGRESULT_ALREADY_IN_CALL         = SPAWN_MSGRESULT_FAILURE_BITS or 3;
  SPAWN_MSGRESULT_INVALID_DATA            = SPAWN_MSGRESULT_FAILURE_BITS or 4;

  { Possible error codes returned by WM_SpawnServer_Query handler }
  SPAWN_MSGRESULT_INVALID_SEQUENCE_NUMBER = SPAWN_MSGRESULT_FAILURE_BITS or 5;
  SPAWN_MSGRESULT_INVALID_QUERY_OPERATION = SPAWN_MSGRESULT_FAILURE_BITS or 6;

  { Low word of response to SPAWN_QUERY_STATUS query }
  SPAWN_STATUS_EXCEPTION      = 1;
  SPAWN_STATUS_RUNNING        = 2;
  SPAWN_STATUS_RETURNED_TRUE  = 3;
  SPAWN_STATUS_RETURNED_FALSE = 4;

implementation

end.
