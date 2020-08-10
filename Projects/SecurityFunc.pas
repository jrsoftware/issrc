unit SecurityFunc;

{
  Inno Setup
  Copyright (C) 1997-2008 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Functions for altering ACLs on files & registry keys

  $jrsoftware: issrc/Projects/SecurityFunc.pas,v 1.6 2008/10/17 22:18:14 jr Exp $
}

interface

uses
  Windows, SysUtils, CmnFunc2, Struct;

function GrantPermissionOnFile(const DisableFsRedir: Boolean; Filename: String;
  const Entries: TGrantPermissionEntry; const EntryCount: Integer): Boolean;
function GrantPermissionOnKey(const RegView: TRegView; const RootKey: HKEY;
  const Subkey: String; const Entries: TGrantPermissionEntry;
  const EntryCount: Integer): Boolean;

implementation

uses
  PathFunc, Msgs, InstFunc, Logging, RedirFunc, Helper;

{$IFNDEF UNICODE}
function AllocWideCharStr(const S: String): PWideChar;
{ Allocates a null-terminated Unicode copy of S on the heap. Use FreeMem to
  free the returned pointer. } 
var
  SourceLen, DestLen: Integer;
begin
  SourceLen := Length(S);
  if SourceLen = 0 then
    DestLen := 0
  else begin
    DestLen := MultiByteToWideChar(CP_ACP, 0, PChar(S), SourceLen, nil, 0);
    if (DestLen <= 0) or (DestLen >= High(Integer) div SizeOf(WideChar)) then
      InternalError('AllocWideCharStr: MultiByteToWideChar failed');
  end;
  GetMem(Result, (DestLen + 1) * SizeOf(WideChar));
  try
    if DestLen <> 0 then
      if MultiByteToWideChar(CP_ACP, 0, PChar(S), SourceLen, Result, DestLen) <> DestLen then
        InternalError('AllocWideCharStr: MultiByteToWideChar failed');
    Result[DestLen] := #0;
  except
    FreeMem(Result);
    raise;
  end;
end;
{$ENDIF}

function InternalGrantPermission(const ObjectType: DWORD; const ObjectName: String;
  const Entries: TGrantPermissionEntry; const EntryCount: Integer;
  const Inheritance: DWORD): DWORD;
{ Grants the specified access to the specified object. Returns ERROR_SUCCESS if
  successful. Always fails on Windows 9x/Me and NT 4.0. }
type
  PPSID = ^PSID;
  PPACL = ^PACL;
  PTrusteeW = ^TTrusteeW;
  TTrusteeW = record
    pMultipleTrustee: PTrusteeW;
    MultipleTrusteeOperation: DWORD;  { MULTIPLE_TRUSTEE_OPERATION }
    TrusteeForm: DWORD;  { TRUSTEE_FORM }
    TrusteeType: DWORD;  { TRUSTEE_TYPE }
    ptstrName: PWideChar;
  end;
  TExplicitAccessW = record
    grfAccessPermissions: DWORD;
    grfAccessMode: DWORD;  { ACCESS_MODE }
    grfInheritance: DWORD;
    Trustee: TTrusteeW;
  end;
  PArrayOfExplicitAccessW = ^TArrayOfExplicitAccessW;
  TArrayOfExplicitAccessW = array[0..999999] of TExplicitAccessW;
const
  GRANT_ACCESS = 1;
  TRUSTEE_IS_SID = 0;
  TRUSTEE_IS_UNKNOWN = 0;
var
  AdvApiHandle: HMODULE;
  GetNamedSecurityInfoW: function(pObjectName: PWideChar; ObjectType: DWORD;
    SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PPSID;
    ppDacl, ppSacl: PPACL; var ppSecurityDescriptor: PSECURITY_DESCRIPTOR): DWORD;
    stdcall;
  SetNamedSecurityInfoW: function(pObjectName: PWideChar; ObjectType: DWORD;
    SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PSID;
    ppDacl, ppSacl: PACL): DWORD; stdcall;
  SetEntriesInAclW: function(cCountOfExplicitEntries: ULONG;
    const pListOfExplicitEntries: TExplicitAccessW; OldAcl: PACL;
    var NewAcl: PACL): DWORD; stdcall;
  {$IFNDEF UNICODE}
  WideObjectName: PWideChar;
  {$ENDIF}
  SD: PSECURITY_DESCRIPTOR;
  Dacl, NewDacl: PACL;
  ExplicitAccess: PArrayOfExplicitAccessW;
  E: ^TGrantPermissionEntry;
  I: Integer;
  Sid: PSID;
begin
  { Windows 9x/Me don't support ACLs, and GetNamedSecurityInfo and
    SetEntriesInACL are buggy on NT 4 }
  if (Win32Platform <> VER_PLATFORM_WIN32_NT) or (Lo(GetVersion) < 5) then begin
    Result := ERROR_INVALID_FUNCTION;
    Exit;
  end;

  AdvApiHandle := GetModuleHandle(advapi32);
  GetNamedSecurityInfoW := GetProcAddress(AdvApiHandle, PAnsiChar('GetNamedSecurityInfoW'));
  SetNamedSecurityInfoW := GetProcAddress(AdvApiHandle, PAnsiChar('SetNamedSecurityInfoW'));
  SetEntriesInAclW := GetProcAddress(AdvApiHandle, PAnsiChar('SetEntriesInAclW'));
  if (@GetNamedSecurityInfoW = nil) or (@SetNamedSecurityInfoW = nil) or
     (@SetEntriesInAclW = nil) then begin
    Result := ERROR_PROC_NOT_FOUND;
    Exit;
  end;

  {$IFNDEF UNICODE}
  WideObjectName := AllocWideCharStr(ObjectName);
  try
  {$ENDIF}
    ExplicitAccess := nil;
    Result := GetNamedSecurityInfoW(
      {$IFDEF UNICODE} PChar(ObjectName) {$ELSE} WideObjectName {$ENDIF},
      ObjectType, DACL_SECURITY_INFORMATION, nil, nil, @Dacl, nil, SD);
    if Result <> ERROR_SUCCESS then
      Exit;
    try
      { Note: Dacl will be nil if GetNamedSecurityInfo is called on a FAT partition.
        Be careful not to dereference a nil pointer. }
      ExplicitAccess := AllocMem(EntryCount * SizeOf(ExplicitAccess[0]));
      E := @Entries;
      for I := 0 to EntryCount-1 do begin
        if not AllocateAndInitializeSid(E.Sid.Authority, E.Sid.SubAuthCount,
           E.Sid.SubAuth[0], E.Sid.SubAuth[1], 0, 0, 0, 0, 0, 0, Sid) then begin
          Result := GetLastError;
          if Result = ERROR_SUCCESS then  { just in case... }
            Result := ERROR_INVALID_PARAMETER;
          Exit;
        end;
        ExplicitAccess[I].grfAccessPermissions := E.AccessMask;
        ExplicitAccess[I].grfAccessMode := GRANT_ACCESS;
        ExplicitAccess[I].grfInheritance := Inheritance;
        ExplicitAccess[I].Trustee.TrusteeForm := TRUSTEE_IS_SID;
        ExplicitAccess[I].Trustee.TrusteeType := TRUSTEE_IS_UNKNOWN;
        PSID(ExplicitAccess[I].Trustee.ptstrName) := Sid;
        Inc(E);
      end;
      Result := SetEntriesInAclW(EntryCount, ExplicitAccess[0], Dacl, NewDacl);
      if Result <> ERROR_SUCCESS then
        Exit;
      try
        Result := SetNamedSecurityInfoW(
          {$IFDEF UNICODE} PChar(ObjectName) {$ELSE} WideObjectName {$ENDIF},
          ObjectType, DACL_SECURITY_INFORMATION, nil, nil, NewDacl, nil);
      finally
        LocalFree(HLOCAL(NewDacl));
      end;
    finally
      if Assigned(ExplicitAccess) then begin
        for I := EntryCount-1 downto 0 do begin
          Sid := PSID(ExplicitAccess[I].Trustee.ptstrName);
          if Assigned(Sid) then
            FreeSid(Sid);
        end;
        FreeMem(ExplicitAccess);
      end;
      LocalFree(HLOCAL(SD));
    end;
  {$IFNDEF UNICODE}
  finally
    FreeMem(WideObjectName);
  end;
  {$ENDIF}
end;

function GrantPermission(const Use64BitHelper: Boolean; const ObjectType: DWORD;
  const ObjectName: String; const Entries: TGrantPermissionEntry;
  const EntryCount: Integer; const Inheritance: DWORD): DWORD;
{ Invokes either the internal GrantPermission function or the one inside the
  64-bit helper, depending on the setting of Use64BitHelper }
begin
  try
    if Use64BitHelper then
      Result := HelperGrantPermission(ObjectType, ObjectName, Entries,
        EntryCount, Inheritance)
    else
      Result := InternalGrantPermission(ObjectType, ObjectName, Entries,
        EntryCount, Inheritance);
  except
    { If the helper interface (or even InternalGrantPermission) raises an
      exception, don't propagate it. Just log it and return an error code, as
      that's what the caller is expecting on failure. }
    Log('Exception while setting permissions:' + SNewLine + GetExceptMessage);
    Result := ERROR_GEN_FAILURE;
  end;
end;

const
  OBJECT_INHERIT_ACE    = 1;
  CONTAINER_INHERIT_ACE = 2;

function GrantPermissionOnFile(const DisableFsRedir: Boolean; Filename: String;
  const Entries: TGrantPermissionEntry; const EntryCount: Integer): Boolean;
{ Grants the specified access to the specified file/directory. Returns True if
  successful. On failure, the thread's last error code is set. Always fails on
  Windows 9x/Me and NT 4.0. }
const
  SE_FILE_OBJECT = 1;
var
  Attr, Inheritance, ErrorCode: DWORD;
begin
  { Expand filename if needed because the 64-bit helper may not have the same
    current directory as us }
  Filename := PathExpand(Filename);
  Attr := GetFileAttributesRedir(DisableFsRedir, Filename);
  if Attr = $FFFFFFFF then begin
    Result := False;
    Exit;
  end;
  if Attr and FILE_ATTRIBUTE_DIRECTORY <> 0 then
    Inheritance := OBJECT_INHERIT_ACE or CONTAINER_INHERIT_ACE
  else
    Inheritance := 0;
  ErrorCode := GrantPermission(DisableFsRedir, SE_FILE_OBJECT, Filename, Entries,
    EntryCount, Inheritance);
  SetLastError(ErrorCode);
  Result := (ErrorCode = ERROR_SUCCESS);
end;

function GrantPermissionOnKey(const RegView: TRegView; const RootKey: HKEY;
  const Subkey: String; const Entries: TGrantPermissionEntry;
  const EntryCount: Integer): Boolean;
{ Grants the specified access to the specified registry key. Returns True if
  successful. On failure, the thread's last error code is set. Always fails on
  Windows 9x/Me and NT 4.0. }
const
  SE_REGISTRY_KEY = 4;
var
  ObjName: String;
  ErrorCode: DWORD;
begin
  case RootKey of
    HKEY_CLASSES_ROOT: ObjName := 'CLASSES_ROOT';
    HKEY_CURRENT_USER: ObjName := 'CURRENT_USER';
    HKEY_LOCAL_MACHINE: ObjName := 'MACHINE';
    HKEY_USERS: ObjName := 'USERS';
  else
    { Other root keys are not supported by Get/SetNamedSecurityInfo }
    SetLastError(ERROR_INVALID_PARAMETER);
    Result := False;
    Exit;
  end;
  ObjName := ObjName + '\' + Subkey;
  ErrorCode := GrantPermission(RegView = rv64Bit, SE_REGISTRY_KEY, ObjName,
    Entries, EntryCount, CONTAINER_INHERIT_ACE);
  SetLastError(ErrorCode);
  Result := (ErrorCode = ERROR_SUCCESS);
end;

end.
