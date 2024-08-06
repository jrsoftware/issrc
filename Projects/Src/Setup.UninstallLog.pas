unit Setup.UninstallLog;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Uninstallation functions
}

interface

uses
  Windows, SysUtils, Shared.Int64Em, Shared.FileClass, Shared.CommonFunc;

const
  HighestSupportedVersion = 1048;
  { Each time the format of the uninstall log changes (usually a new entry type
    is added), HighestSupportedVersion and the file version number of Setup
    are incremented to match (51.x). Do NOT do this yourself; doing so could cause
    incompatibilities with future Inno Setup releases. It's recommended that you
    use the "utUserDefined" log entry type if you wish to implement your own
    custom uninstall log entries; see below for more information. }

type
  TUninstallRecTyp = type Word;
const
  { Values for TUninstallRecTyp.
    If you wish to define your own custom uninstall entry type, you should use
    "utUserDefined". (Do NOT define your own ut* constants; this could cause
    incompatibilities with future Inno Setup releases.) The first field in a
    utUserDefined record must be a string which specifies a unique name for
    the record type. Example:
    UninstLog.Add(utUserDefined, ['MyRecordType', ... ], 0);
 }
  utUserDefined          = $01;
  utStartInstall         = $10;
  utEndInstall           = $11;
  utCompiledCode         = $20;
  utRun                  = $80;
  utDeleteDirOrFiles     = $81;
  utDeleteFile           = $82;
  utDeleteGroupOrItem    = $83;
  utIniDeleteEntry       = $84;
  utIniDeleteSection     = $85;
  utRegDeleteEntireKey   = $86;
  utRegClearValue        = $87;
  utRegDeleteKeyIfEmpty  = $88;
  utRegDeleteValue       = $89;
  utDecrementSharedCount = $8A;
  utRefreshFileAssoc     = $8B;
  utMutexCheck           = $8C;

  { Flags on ExtraData }
  utRun_NoWait = 1;
  utRun_WaitUntilIdle = 2;
  utRun_ShellExec = 4;
  utRun_RunMinimized = 8;
  utRun_RunMaximized = 16;
  utRun_SkipIfDoesntExist = 32;
  utRun_RunHidden = 64;
  utRun_ShellExecRespectWaitFlags = 128;
  utRun_DisableFsRedir = 256;
  utRun_DontLogParameters = 512;
  utRun_LogOutput = 1024;
  utDeleteFile_ExistedBeforeInstall = 1;
  utDeleteFile_Extra = 2;
  utDeleteFile_IsFont = 4;
  utDeleteFile_SharedFile = 8;
  utDeleteFile_RegisteredServer = 16;
  utDeleteFile_CallChangeNotify = 32;
  utDeleteFile_RegisteredTypeLib = 64;
  utDeleteFile_RestartDelete = 128;
  utDeleteFile_RemoveReadOnly = 256;
  utDeleteFile_NoSharedFilePrompt = 512;
  utDeleteFile_SharedFileIn64BitKey = 1024;
  utDeleteFile_DisableFsRedir = 2048;  { also determines whether file was registered as 64-bit }
  utDeleteFile_GacInstalled = 4096;
  utDeleteFile_PerUserFont = 8192;
  utDeleteDirOrFiles_Extra = 1;
  utDeleteDirOrFiles_IsDir = 2;
  utDeleteDirOrFiles_DeleteFiles = 4;
  utDeleteDirOrFiles_DeleteSubdirsAlso = 8;
  utDeleteDirOrFiles_CallChangeNotify = 16;
  utDeleteDirOrFiles_DisableFsRedir = 32;
  utIniDeleteSection_OnlyIfEmpty = 1;
  utReg_KeyHandleMask = $80FFFFFF; 
  utReg_64BitKey = $01000000;
  utDecrementSharedCount_64BitKey = 1;

type
  PUninstallRec = ^TUninstallRec;
  TUninstallRec = record
    Prev, Next: PUninstallRec;
    ExtraData: Longint;
    DataSize: Cardinal;
    Typ: TUninstallRecTyp;
    Data: array[0..$6FFFFFFF] of Byte;  { *must* be last field }
  end;

  TDeleteUninstallDataFilesProc = procedure;

  TUninstallLogFlags = set of (ufAdminInstalled, ufDontCheckRecCRCs,
    ufModernStyle, ufAlwaysRestart, ufChangesEnvironment, ufWin64,
    ufPowerUserInstalled, ufAdminInstallMode);

  TUninstallLog = class
  private
    FList, FLastList: PUninstallRec;
    FCount: Integer;
    class function AllocRec(const Typ: TUninstallRecTyp;
      const ExtraData: Longint; const DataSize: Integer): PUninstallRec;
    function Delete(const Rec: PUninstallRec): PUninstallRec;
    procedure InternalAdd(const NewRec: PUninstallRec);
  protected
    procedure HandleException; virtual; abstract;
    function ShouldRemoveSharedFile(const Filename: String): Boolean; virtual;
    procedure StatusUpdate(StartingCount, CurCount: Integer); virtual;
  public
    InstallMode64Bit: Boolean;
    AppId, AppName: String;
    NeedRestart: Boolean;
    Flags: TUninstallLogFlags;
    Version: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Typ: TUninstallRecTyp; const Data: array of String;
      const ExtraData: Longint);
    procedure AddReg(const Typ: TUninstallRecTyp; const RegView: TRegView;
      const RootKey: HKEY; const Data: array of String);
    function CanAppend(const Filename: String;
      var ExistingFlags: TUninstallLogFlags): Boolean;
    function CheckMutexes: Boolean;
    procedure Clear;
    class function ExtractRecData(const Rec: PUninstallRec;
      var Data: array of String): Integer;
    function ExtractLatestRecData(const Typ: TUninstallRecTyp;
      const ExtraData: Longint; var Data: array of String): Boolean;
    procedure Load(const F: TFile; const Filename: String);
    function PerformUninstall(const CallFromUninstaller: Boolean;
      const DeleteUninstallDataFilesProc: TDeleteUninstallDataFilesProc): Boolean;
    class function WriteSafeHeaderString(Dest: PAnsiChar; const Source: String;
     MaxDestBytes: Cardinal): Cardinal;
    class function ReadSafeHeaderString(const Source: AnsiString): String;
    procedure Save(const Filename: String;
      const Append, UpdateUninstallLogAppName: Boolean);
    property List: PUninstallRec read FList;
    property LastList: PUninstallRec read FLastList;
  end;

function ReadUninstallLogFlags(const F: TFile; const Filename: String): TUninstallLogFlags;

implementation

uses
  Messages, ShlObj, AnsiStrings,
  PathFunc, Shared.Struct, SetupLdrAndSetup.Messages, Shared.SetupMessageIDs, Setup.InstFunc,
  Setup.InstFunc.Ole, SetupLdrAndSetup.RedirFunc, Compression.Base,
  Setup.LoggingFunc, Setup.RegDLL, Setup.Helper, Setup.DotNetFunc;

type
  { Note: TUninstallLogHeader should stay <= 512 bytes in size, so that it
    fits into a single disk sector and can be written atomically }
  TUninstallLogHeader = packed record
    ID: TUninstallLogID;
    AppId: array[0..127] of AnsiChar;
    AppName: array[0..127] of AnsiChar;
    Version, NumRecs: Integer;
    EndOffset: LongWord;
    Flags: Longint;
    Reserved: array[0..26] of Longint;  { reserved for future use }
    CRC: Longint;
  end;
  TUninstallCrcHeader = packed record
    Size, NotSize: Cardinal;
    CRC: Longint;
  end;
  TUninstallFileRec = packed record
    Typ: TUninstallRecTyp;
    ExtraData: Longint;
    DataSize: Cardinal;
  end;

procedure ReadUninstallLogHeader(const F: TFile; const Filename: String;
  var Header: TUninstallLogHeader; var Header64Bit: Boolean);

  procedure Corrupt;
  begin
    raise Exception.Create(FmtSetupMessage1(msgUninstallDataCorrupted, Filename));
  end;

begin
  F.Seek(0);
  if F.Read(Header, SizeOf(Header)) <> SizeOf(Header) then
    Corrupt;
  if (Header.CRC <> $11111111) and
      { ^ for debugging purposes, you can change the CRC field in the file to
        $11111111 to disable CRC checking on the header}
     (Header.CRC <> GetCRC32(Header, SizeOf(Header)-SizeOf(Longint))) then
    Corrupt;
  if Header.ID = UninstallLogID[False] then
    Header64Bit := False
  else if Header.ID = UninstallLogID[True] then
    Header64Bit := True
  else
    Corrupt;
end;

function ReadUninstallLogFlags(const F: TFile; const Filename: String): TUninstallLogFlags;
{ Reads the flags from the header of the open file F. The Filename parameter
  is only used when generating exception error messages. }
var
  Header: TUninstallLogHeader;
  Header64Bit: Boolean;
begin
  ReadUninstallLogHeader(F, Filename, Header, Header64Bit);
  Result := TUninstallLogFlags((@Header.Flags)^);
end;

{ Misc. uninstallation functions }

function ListContainsPathOrSubdir(const List: TSimpleStringList;
  const Path: String): Boolean;
{ Returns True if List contains Path or a subdirectory of Path }
var
  SlashPath: String;
  SlashPathLen, I: Integer;
begin
  SlashPath := AddBackslash(Path);
  SlashPathLen := Length(SlashPath);
  if SlashPathLen > 0 then begin   { ...sanity check }
    for I := 0 to List.Count-1 do begin
      if List[I] = Path then begin
        Result := True;
        Exit;
      end;
      if (Length(List[I]) > SlashPathLen) and
         CompareMem(Pointer(List[I]), Pointer(SlashPath), SlashPathLen * SizeOf(SlashPath[1])) then begin
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

procedure LoggedRestartDeleteDir(const DisableFsRedir: Boolean; Dir: String);
begin
  Dir := PathExpand(Dir);
  if not DisableFsRedir then begin
    { Work around WOW64 bug present in the IA64 and x64 editions of Windows
      XP (3790) and Server 2003 prior to SP1 RC2: MoveFileEx writes filenames
      to the registry verbatim without mapping system32->syswow64. }
    Dir := ReplaceSystemDirWithSysWow64(Dir);
  end;
  if not MoveFileExRedir(DisableFsRedir, Dir, '', MOVEFILE_DELAY_UNTIL_REBOOT) then
    LogFmt('MoveFileEx failed (%d).', [GetLastError]);
end;

const
  drFalse = '0';
  drTrue = '1';

function LoggedDeleteDir(const DisableFsRedir: Boolean; const DirName: String;
  const DirsNotRemoved, RestartDeleteDirList: TSimpleStringList): Boolean;
const
  FILE_ATTRIBUTE_REPARSE_POINT = $00000400;
  DirsNotRemovedPrefix: array[Boolean] of Char = (drFalse, drTrue);
var
  Attribs, LastError: DWORD;
begin
  Attribs := GetFileAttributesRedir(DisableFsRedir, DirName);
  { Does the directory exist? }
  if (Attribs <> $FFFFFFFF) and
     (Attribs and FILE_ATTRIBUTE_DIRECTORY <> 0) then begin
    LogFmt('Deleting directory: %s', [DirName]);
    { If the directory has the read-only attribute, strip it first }
    if Attribs and FILE_ATTRIBUTE_READONLY <> 0 then begin
      if (Attribs and FILE_ATTRIBUTE_REPARSE_POINT <> 0) or
         IsDirEmpty(DisableFsRedir, DirName) then begin
        if SetFileAttributesRedir(DisableFsRedir, DirName, Attribs and not FILE_ATTRIBUTE_READONLY) then
          Log('Stripped read-only attribute.')
        else
          Log('Failed to strip read-only attribute.');
      end
      else
        Log('Not stripping read-only attribute because the directory ' +
          'does not appear to be empty.');
    end;
    Result := RemoveDirectoryRedir(DisableFsRedir, DirName);
    if not Result then begin
      LastError := GetLastError;
      if Assigned(DirsNotRemoved) then begin
        LogFmt('Failed to delete directory (%d). Will retry later.', [LastError]);
        DirsNotRemoved.AddIfDoesntExist(DirsNotRemovedPrefix[DisableFsRedir] + DirName);
      end
      else if Assigned(RestartDeleteDirList) and
         ListContainsPathOrSubdir(RestartDeleteDirList, DirName) then begin
        LogFmt('Failed to delete directory (%d). Will delete on restart (if empty).',
          [LastError]);
        LoggedRestartDeleteDir(DisableFsRedir, DirName);
      end
      else
        LogFmt('Failed to delete directory (%d).', [LastError]);
    end;
  end
  else
    Result := True;
end;

procedure CrackRegExtraData(const ExtraData: Longint; var RegView: TRegView;
  var RootKey: HKEY);
begin
  if ExtraData and utReg_64BitKey <> 0 then
    RegView := rv64Bit
  else
    RegView := rv32Bit;
  RootKey := ExtraData and utReg_KeyHandleMask;
end;

{ TUninstallLog }

constructor TUninstallLog.Create;
begin
  inherited Create;
  Clear;
end;

destructor TUninstallLog.Destroy;
begin
  Clear;
  inherited Destroy;
end;

class function TUninstallLog.AllocRec(const Typ: TUninstallRecTyp;
  const ExtraData: Longint; const DataSize: Integer): PUninstallRec;
{ Allocates a new PUninstallRec, but does not add it to the list. Returns nil
  if the value of the DataSize parameter is out of range. }
begin
  { Sanity check the size to protect against integer overflows. 128 MB should
    be way more than enough. }
  if (DataSize < 0) or (DataSize > $08000000) then begin
    Result := nil;
    Exit;
  end;
  Result := AllocMem(Integer(@PUninstallRec(nil).Data) + DataSize);
  Result.Typ := Typ;
  Result.ExtraData := ExtraData;
  Result.DataSize := DataSize;
end;

procedure TUninstallLog.InternalAdd(const NewRec: PUninstallRec);
{ Adds a new entry to the uninstall list }
begin
  if List = nil then begin
    FList := NewRec;
    FLastList := List;
  end
  else begin
    LastList^.Next := NewRec;
    NewRec^.Prev := LastList;
    FLastList := NewRec;
  end;
  Inc(FCount);
end;

procedure TUninstallLog.Add(const Typ: TUninstallRecTyp; const Data: array of String;
  const ExtraData: Longint);
var
  I, L: Integer;
  S, X: AnsiString;
  AData: AnsiString;
  NewRec: PUninstallRec;
begin
  for I := 0 to High(Data) do begin
    L := Length(Data[I])*SizeOf(Data[I][1]);

    SetLength(X, SizeOf(Byte) + SizeOf(Integer));
    X[1] := AnsiChar($FE);
    Integer((@X[2])^) := Integer(-L);
    S := S + X;

    SetString(AData, PAnsiChar(Pointer(Data[I])), L);
    S := S + AData;
  end;
  S := S + AnsiChar($FF);

  NewRec := AllocRec(Typ, ExtraData, Length(S)*SizeOf(S[1]));
  if NewRec = nil then
    InternalError('DataSize range exceeded');
  Move(Pointer(S)^, NewRec.Data, NewRec.DataSize);
  InternalAdd(NewRec);

  if Version < HighestSupportedVersion then
    Version := HighestSupportedVersion;
end;

procedure TUninstallLog.AddReg(const Typ: TUninstallRecTyp;
  const RegView: TRegView; const RootKey: HKEY; const Data: array of String);
{ Adds a new utReg* type entry }
var
  ExtraData: Longint;
begin
  { If RootKey isn't a predefined key, or has unrecognized garbage in the
    high byte (which we use for our own purposes), reject it }
  if RootKey shr 24 <> $80 then
    Exit;

  { ExtraData in a utReg* entry consists of a root key value (HKEY_*)
    OR'ed with flag bits in the high byte }
  HKEY(ExtraData) := RootKey;
  if RegView in RegViews64Bit then
    ExtraData := ExtraData or utReg_64BitKey;
  Add(Typ, Data, ExtraData);
end;

function TUninstallLog.Delete(const Rec: PUninstallRec): PUninstallRec;
{ Removes Rec from the linked list, then frees it. Returns (what was) the
  previous record, or nil if there is none. }
begin
  Result := Rec.Prev;
  if Assigned(Rec.Prev) then
    Rec.Prev.Next := Rec.Next;
  if Assigned(Rec.Next) then
    Rec.Next.Prev := Rec.Prev;
  if FList = Rec then
    FList := Rec.Next;
  if FLastList = Rec then
    FLastList := Rec.Prev;
  Dec(FCount);
  FreeMem(Rec);
end;

procedure TUninstallLog.Clear;
{ Frees all entries in the uninstall list and clears AppName/AppDir }
begin
  while FLastList <> nil do
    Delete(FLastList);
  FCount := 0;
  AppId := '';
  AppName := '';
  Flags := [];
end;

type
  PDeleteDirData = ^TDeleteDirData;
  TDeleteDirData = record
    DirsNotRemoved: TSimpleStringList;
  end;

function LoggedDeleteDirProc(const DisableFsRedir: Boolean; const DirName: String;
  const Param: Pointer): Boolean;
begin
  Result := LoggedDeleteDir(DisableFsRedir, DirName, PDeleteDirData(Param)^.DirsNotRemoved, nil);
end;

function LoggedDeleteFileProc(const DisableFsRedir: Boolean; const FileName: String;
  const Param: Pointer): Boolean;
begin
  LogFmt('Deleting file: %s', [FileName]);
  Result := DeleteFileRedir(DisableFsRedir, FileName);
  if not Result then
    LogFmt('Failed to delete the file; it may be in use (%d).', [GetLastError]);
end;

procedure ProcessMessagesProc; far;
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;

class function TUninstallLog.ExtractRecData(const Rec: PUninstallRec;
  var Data: array of String): Integer;
var
  I, L: Integer;
  X: ^Byte;
begin
  for I := 0 to High(Data) do
    Data[I] := '';
  I := 0;
  X := @Rec^.Data;
  while I <= High(Data) do begin
    case X^ of
      $00..$FC: begin
           L := X^;
           Inc(X);
         end;
      $FD: begin
           Inc(X);
           L := Word(Pointer(X)^);
           Inc(X, SizeOf(Word));
         end;
      $FE: begin
           Inc(X);
           L := Integer(Pointer(X)^);
           Inc(X, SizeOf(Integer));
         end;
      $FF: Break;
    end;
    if L < 0 then begin
      L := -L;
      SetString(Data[I], PChar(X), L div SizeOf(Char));
    end else
      SetString(Data[I], PAnsiChar(X), L);
    Inc(X, L);
    Inc(I);
  end;
  Result := I;
end;

function TUninstallLog.ExtractLatestRecData(const Typ: TUninstallRecTyp;
 const ExtraData: Longint; var Data: array of String): Boolean;
var
  CurRec: PUninstallRec;
begin
  CurRec := LastList;
  while CurRec <> nil do begin
    if (CurRec^.Typ = Typ) and (CurRec^.ExtraData = ExtraData) then begin
      ExtractRecData(CurRec, Data);
      Result := True;
      Exit;
    end;
    CurRec := CurRec^.Prev;
  end;
  Result := False;
end;

function TUninstallLog.CheckMutexes: Boolean;
var
  CurRec: PUninstallRec;
  Data: String;
begin
  Result := False;
  CurRec := LastList;
  while CurRec <> nil do begin
    if CurRec^.Typ = utMutexCheck then begin
      ExtractRecData(CurRec, Data);
      if CheckForMutexes(Data) then begin
        Result := True;
        Exit;
      end;
    end;
    CurRec := CurRec^.Prev;
  end;
end;

procedure RunExecLog(const S: String; const Error, FirstLine: Boolean; const Data: NativeInt);
begin
  if not Error and FirstLine then
    Log('Running Exec output:');
  Log(S);
end;

function TUninstallLog.PerformUninstall(const CallFromUninstaller: Boolean;
  const DeleteUninstallDataFilesProc: TDeleteUninstallDataFilesProc): Boolean;
{ Undoes all the changes in the uninstall list, in reverse order they were
  added. Deletes entries that were successfully undone.
  Returns True if all elements were successfully removed; False if some
  could not be removed. }

var
  RefreshFileAssoc: Boolean;
  ChangeNotifyList, RunOnceList: TSimpleStringList;
  UnregisteredServersList, RestartDeleteDirList: array[Boolean] of TSimpleStringList;
  DeleteDirData: TDeleteDirData;

  function LoggedFileDelete(const Filename: String; const DisableFsRedir,
    NotifyChange, RestartDelete, RemoveReadOnly: Boolean): Boolean;
  var
    ExistingAttr, LastError: DWORD;
  begin
    Result := True;

    { Automatically delete generated indexes associated with help files }
    if SameText(PathExtractExt(Filename), '.hlp') then begin
      LoggedFileDelete(PathChangeExt(Filename, '.gid'), DisableFsRedir, False, False, False);
      LoggedFileDelete(PathChangeExt(Filename, '.fts'), DisableFsRedir, False, False, False);
    end
    else if SameText(PathExtractExt(Filename), '.chm') then
      LoggedFileDelete(PathChangeExt(Filename, '.chw'), DisableFsRedir, False, False, False);

    { Automatically unpin shortcuts }
    if SameText(PathExtractExt(Filename), '.lnk') then
      UnpinShellLink(Filename);
      
    if NewFileExistsRedir(DisableFsRedir, Filename) then begin
      LogFmt('Deleting file: %s', [FileName]);
      if RemoveReadOnly then begin
        ExistingAttr := GetFileAttributesRedir(DisableFsRedir, Filename);
        if (ExistingAttr <> $FFFFFFFF) and
           (ExistingAttr and FILE_ATTRIBUTE_READONLY <> 0) then
          if SetFileAttributesRedir(DisableFsRedir, Filename,
             ExistingAttr and not FILE_ATTRIBUTE_READONLY) then
            Log('Stripped read-only attribute.')
          else
            Log('Failed to strip read-only attribute.');
      end;
      if not DeleteFileRedir(DisableFsRedir, Filename) then begin
        LastError := GetLastError;
        if RestartDelete and CallFromUninstaller and
           ((LastError = ERROR_ACCESS_DENIED) or (LastError = ERROR_SHARING_VIOLATION)) and
           (GetFileAttributesRedir(DisableFsRedir, Filename) and FILE_ATTRIBUTE_READONLY = 0) then begin
          LogFmt('The file appears to be in use (%d). Will delete on restart.',
            [LastError]);
          try
            RestartReplace(DisableFsRedir, Filename, '');
            NeedRestart := True;
            { Add the file's directory to the list of directories that should
              be restart-deleted later }
            RestartDeleteDirList[DisableFsRedir].AddIfDoesntExist(PathExtractDir(PathExpand(Filename)));
          except
            Log('Exception message:' + SNewLine + GetExceptMessage);
            Result := False;
          end;
        end
        else begin
          LogFmt('Failed to delete the file; it may be in use (%d).', [LastError]);
          Result := False;
        end;
      end
      else begin
        { Note: It is assumed that DisableFsRedir will be False when NotifyChange is True }
        if NotifyChange then begin
          SHChangeNotify(SHCNE_DELETE, SHCNF_PATH, PChar(Filename), nil);
          ChangeNotifyList.AddIfDoesntExist(PathExtractDir(Filename));
        end;
      end;
    end;
  end;

  function LoggedDecrementSharedCount(const Filename: String;
    const Key64Bit: Boolean): Boolean;
  const
    Bits: array[Boolean] of Integer = (32, 64);
  var
    RegView: TRegView;
  begin
    if Key64Bit then
      RegView := rv64Bit
    else
      RegView := rv32Bit;
    LogFmt('Decrementing shared count (%d-bit): %s', [Bits[Key64Bit], Filename]);
    Result := DecrementSharedCount(RegView, Filename);
    if Result then
      Log('Shared count reached zero.');
  end;

  procedure LoggedUnregisterServer(const Is64Bit: Boolean; const Filename: String);
  begin
    { Just as an optimization, make sure we aren't unregistering
      the same file again }
    if UnregisteredServersList[Is64Bit].IndexOf(Filename) = -1 then begin
      if Is64Bit then
        LogFmt('Unregistering 64-bit DLL/OCX: %s', [Filename])
      else
        LogFmt('Unregistering 32-bit DLL/OCX: %s', [Filename]);
      try
        RegisterServer(True, Is64Bit, Filename, True);
        UnregisteredServersList[Is64Bit].Add(Filename);
        Log('Unregistration successful.');
      except
        Log('Unregistration failed:' + SNewLine + GetExceptMessage);
      end;
    end
    else
      LogFmt('Not unregistering DLL/OCX again: %s', [Filename]);
  end;

  procedure LoggedUnregisterTypeLibrary(const Is64Bit: Boolean;
    const Filename: String);
  begin
    if Is64Bit then
      LogFmt('Unregistering 64-bit type library: %s', [Filename])
    else
      LogFmt('Unregistering 32-bit type library: %s', [Filename]);
    try
      if Is64Bit then
        HelperRegisterTypeLibrary(True, Filename)
      else
        UnregisterTypeLibrary(Filename);
      Log('Unregistration successful.');
    except
      Log('Unregistration failed:' + SNewLine + GetExceptMessage);
    end;
  end;

  procedure LoggedUninstallAssembly(const StrongAssemblyName: String);
  begin
    LogFmt('Uninstalling from GAC: %s', [StrongAssemblyName]);
    try
      with TAssemblyCacheInfo.Create(rvDefault) do try
        UninstallAssembly(StrongAssemblyName);
      finally
        Free;
      end;
    except
      Log('Uninstallation failed:' + SNewLine + GetExceptMessage);
    end;
  end;

  procedure LoggedProcessDirsNotRemoved;
  var
    I: Integer;
    S: String;
    DisableFsRedir: Boolean;
  begin
    for I := 0 to DeleteDirData.DirsNotRemoved.Count-1 do begin
      S := DeleteDirData.DirsNotRemoved[I];
      { The first character specifies the DisableFsRedir value
        (e.g. '0C:\Program Files\My Program') }
      DisableFsRedir := (S[1] = drTrue);
      System.Delete(S, 1, 1);
      LoggedDeleteDir(DisableFsRedir, S, nil, RestartDeleteDirList[DisableFsRedir]);
    end;
  end;
  
  function GetLogIniFilename(const Filename: String): String;
  begin
    if Filename <> '' then
      Result := Filename
    else
      Result := 'win.ini';
  end;

const
  GroupInfoChars: array[0..3] of Char = ('"', '"', ',', ',');
  NullChar: Char = #0;
var
  StartCount: Integer;
  CurRec: PUninstallRec;
  CurRecDataPChar: array[0..9] of PChar;
  CurRecData: array[0..9] of String;
  ShouldDeleteRec, IsTempFile, IsSharedFile, SharedCountDidReachZero: Boolean;
  Filename, Section, Key: String;
  Subkey, ValueName: PChar;
  P, ErrorCode: Integer;
  RegView: TRegView;
  RootKey, K: HKEY;
  Wait: TExecWait;
  ShowCmd: Integer;

  procedure SplitData(const Rec: PUninstallRec);
  var
    C, I: Integer;
  begin
    C := ExtractRecData(Rec, CurRecData);
    for I := 0 to 9 do begin
      if I < C then
        CurRecDataPChar[I] := PChar(CurRecData[I])
      else
        CurRecDataPChar[I] := nil;
    end;
  end;

begin
  Log('Starting the uninstallation process.');
  SetCurrentDir(GetSystemDir);
  Result := True;
  NeedRestart := False;

  RefreshFileAssoc := False;
  RunOnceList := nil;
  UnregisteredServersList[False] := nil;
  UnregisteredServersList[True] := nil;
  RestartDeleteDirList[False] := nil;
  RestartDeleteDirList[True] := nil;
  DeleteDirData.DirsNotRemoved := nil;
  ChangeNotifyList := TSimpleStringList.Create;
  try
    RunOnceList := TSimpleStringList.Create;
    UnregisteredServersList[False] := TSimpleStringList.Create;
    UnregisteredServersList[True] := TSimpleStringList.Create;
    RestartDeleteDirList[False] := TSimpleStringList.Create;
    RestartDeleteDirList[True] := TSimpleStringList.Create;
    if Assigned(DeleteUninstallDataFilesProc) then
      DeleteDirData.DirsNotRemoved := TSimpleStringList.Create;

    StartCount := FCount;
    StatusUpdate(StartCount, FCount);

    { Step 1 - Process all utRun entries }
    if CallFromUninstaller then begin
      CurRec := LastList;
      while CurRec <> nil do begin
        if CurRec^.Typ = utRun then begin
          try
            SplitData(CurRec);
            { Verify that a utRun entry with the same RunOnceId has not
              already been executed }
            if (CurRecData[3] = '') or (RunOnceList.IndexOf(CurRecData[3]) = -1) then begin
              Wait := ewWaitUntilTerminated;
              if CurRec^.ExtraData and utRun_NoWait <> 0 then
                Wait := ewNoWait
              else if CurRec^.ExtraData and utRun_WaitUntilIdle <> 0 then
                Wait := ewWaitUntilIdle;
              ShowCmd := SW_SHOWNORMAL;
              if CurRec^.ExtraData and utRun_RunMinimized <> 0 then
                ShowCmd := SW_SHOWMINNOACTIVE
              else if CurRec^.ExtraData and utRun_RunMaximized <> 0 then
                ShowCmd := SW_SHOWMAXIMIZED
              else if CurRec^.ExtraData and utRun_RunHidden <> 0 then
                ShowCmd := SW_HIDE;
              { Note: This code is similar to code in the ProcessRunEntry
                function of Main.pas }
              if CurRec^.ExtraData and utRun_ShellExec = 0 then begin
                Log('Running Exec filename: ' + CurRecData[0]);
                if (CurRec^.ExtraData and utRun_DontLogParameters = 0) and (CurRecData[1] <> '') then
                  Log('Running Exec parameters: ' + CurRecData[1]);
                if (CurRec^.ExtraData and utRun_SkipIfDoesntExist = 0) or
                   NewFileExistsRedir(CurRec^.ExtraData and utRun_DisableFsRedir <> 0, CurRecData[0]) then begin
                  var OutputReader: TCreateProcessOutputReader := nil;
                  try
                    if GetLogActive and (CurRec^.ExtraData and utRun_LogOutput <> 0) then
                      OutputReader := TCreateProcessOutputReader.Create(RunExecLog, 0);
                    if not InstExec(CurRec^.ExtraData and utRun_DisableFsRedir <> 0,
                       CurRecData[0], CurRecData[1], CurRecData[2], Wait,
                       ShowCmd, ProcessMessagesProc, OutputReader, ErrorCode) then begin
                      LogFmt('CreateProcess failed (%d).', [ErrorCode]);
                      Result := False;
                    end
                    else begin
                      if Wait = ewWaitUntilTerminated then
                        LogFmt('Process exit code: %u', [ErrorCode]);
                    end;
                  finally
                    OutputReader.Free;
                  end;
                end else
                  Log('File doesn''t exist. Skipping.');
              end
              else begin
                Log('Running ShellExec filename: ' + CurRecData[0]);
                if (CurRec^.ExtraData and utRun_DontLogParameters = 0) and (CurRecData[1] <> '') then
                  Log('Running ShellExec parameters: ' + CurRecData[1]);
                if (CurRec^.ExtraData and utRun_SkipIfDoesntExist = 0) or
                   FileOrDirExists(CurRecData[0]) then begin
                  if CurRec^.ExtraData and utRun_ShellExecRespectWaitFlags = 0 then
                    Wait := ewNoWait;
                  if not InstShellExec(CurRecData[4], CurRecData[0], CurRecData[1], CurRecData[2],
                     Wait, ShowCmd, ProcessMessagesProc, ErrorCode) then begin
                    LogFmt('ShellExecuteEx failed (%d).', [ErrorCode]);
                    Result := False;
                  end
                  else begin
                    if Wait = ewWaitUntilTerminated then
                      LogFmt('Process exit code: %u', [ErrorCode]);
                  end;
                end else
                  Log('File/directory doesn''t exist. Skipping.');
              end;
              if CurRecData[3] <> '' then
                RunOnceList.Add(CurRecData[3]);
            end else
              LogFmt('Skipping RunOnceId "%s" filename: %s', [CurRecData[3], CurRecData[0]]);
          except
            Result := False;
            if not(ExceptObject is EAbort) then
              HandleException;
          end;
          CurRec := Delete(CurRec);
          StatusUpdate(StartCount, FCount);
        end
        else
          CurRec := CurRec^.Prev;
      end;
    end;

    { Step 2 - Decrement shared file counts, unregister DLLs/TLBs/fonts, and uninstall from GAC }
    CurRec := LastList;
    while CurRec <> nil do begin
      ShouldDeleteRec := False;
      if CurRec^.Typ = utDeleteFile then begin
        { Default to deleting the record in case an exception is raised by
          DecrementSharedCount, the reference count doesn't reach zero, or the
          user opts not to delete the shared file. }
        ShouldDeleteRec := True;
        try
          SplitData(CurRec);
          { Note: Some of this code is duplicated in Step 3 }
          if CallFromUninstaller or (CurRec^.ExtraData and utDeleteFile_ExistedBeforeInstall = 0) then begin
            IsTempFile := not CallFromUninstaller and (CurRecData[1] <> '');

            { Decrement shared file count if necessary }
            IsSharedFile := CurRec^.ExtraData and utDeleteFile_SharedFile <> 0;
            if IsSharedFile then
              SharedCountDidReachZero := LoggedDecrementSharedCount(CurRecData[0],
                CurRec^.ExtraData and utDeleteFile_SharedFileIn64BitKey <> 0)
            else
              SharedCountDidReachZero := False; //silence compiler

            if not IsSharedFile or
               (SharedCountDidReachZero and
                (IsTempFile or
                 not NewFileExistsRedir(CurRec^.ExtraData and utDeleteFile_DisableFsRedir <> 0, CurRecData[0]) or
                 (CurRec^.ExtraData and utDeleteFile_NoSharedFilePrompt <> 0) or
                 ShouldRemoveSharedFile(CurRecData[0]))) then begin
              { The reference count reached zero and the user did not object
                to the file being deleted, so don't delete the record; allow
                the file to be deleted in the next step. }
              ShouldDeleteRec := False;
              { Unregister if necessary }
              if not IsTempFile then begin
                if CurRec^.ExtraData and utDeleteFile_RegisteredServer <> 0 then begin
                  LoggedUnregisterServer(CurRec^.ExtraData and utDeleteFile_DisableFsRedir <> 0,
                    CurRecData[0]);
                end;
                if CurRec^.ExtraData and utDeleteFile_RegisteredTypeLib <> 0 then begin
                  LoggedUnregisterTypeLibrary(CurRec^.ExtraData and utDeleteFile_DisableFsRedir <> 0,
                    CurRecData[0]);
                end;
              end;
              if CurRec^.ExtraData and utDeleteFile_IsFont <> 0 then begin
                LogFmt('Unregistering font: %s', [CurRecData[2]]);
                UnregisterFont(CurRecData[2], CurRecData[3], CurRec^.ExtraData and utDeleteFile_PerUserFont <> 0);
              end;
              if CurRec^.ExtraData and utDeleteFile_GacInstalled <> 0 then
                LoggedUninstallAssembly(CurRecData[4]);
            end;
          end
          else begin
            { This case is handled entirely in Step 3 }
            ShouldDeleteRec := False;
          end;
        except
          Result := False;
          if not(ExceptObject is EAbort) then
            HandleException;
        end;
      end;
      if ShouldDeleteRec then begin
        CurRec := Delete(CurRec);
        StatusUpdate(StartCount, FCount);
      end
      else
        CurRec := CurRec^.Prev;
    end;

    { Step 3 - Remaining entries }
    CurRec := LastList;
    while CurRec <> nil do begin
      SplitData(CurRec);
      try
        case CurRec^.Typ of
          utUserDefined: begin
              {if CurRecData[0] = 'MyRecordType' then begin
                 ... your code here ...
              end
              else}
                raise Exception.Create(FmtSetupMessage1(msgUninstallUnknownEntry,
                  'utUserDefined:' + CurRecData[0]));
            end;
          utStartInstall,
          utEndInstall,
          utCompiledCode: { do nothing on these };
          utRun: begin
              { Will get here if CallFromUninstaller=False; in that case utRun
                entries will still be in the list, unprocessed. Just ignore
                them. }
            end;
          utDeleteDirOrFiles:
            if (CallFromUninstaller or (CurRec^.ExtraData and utDeleteDirOrFiles_Extra = 0)) then begin
              if DelTree(CurRec^.ExtraData and utDeleteDirOrFiles_DisableFsRedir <> 0,
                 CurRecData[0], CurRec^.ExtraData and utDeleteDirOrFiles_IsDir <> 0,
                 CurRec^.ExtraData and utDeleteDirOrFiles_DeleteFiles <> 0,
                 CurRec^.ExtraData and utDeleteDirOrFiles_DeleteSubdirsAlso <> 0,
                 False, LoggedDeleteDirProc, LoggedDeleteFileProc, @DeleteDirData) then begin
                if (CurRec^.ExtraData and utDeleteDirOrFiles_IsDir <> 0) and
                   (CurRec^.ExtraData and utDeleteDirOrFiles_CallChangeNotify <> 0) then begin
                  SHChangeNotify(SHCNE_RMDIR, SHCNF_PATH, CurRecDataPChar[0], nil);
                  ChangeNotifyList.AddIfDoesntExist(PathExtractDir(CurRecData[0]));
                end;
              end;
            end;
          utDeleteFile: begin
              { Note: Some of this code is duplicated in Step 2 }
              Filename := CurRecData[1];
              if CallFromUninstaller or (Filename = '') then
                Filename := CurRecData[0];
              if CallFromUninstaller or (CurRec^.ExtraData and utDeleteFile_ExistedBeforeInstall = 0) then begin
                { Note: We handled utDeleteFile_SharedFile already }
                if CallFromUninstaller or (CurRec^.ExtraData and utDeleteFile_Extra = 0) then
                  if not LoggedFileDelete(Filename, CurRec^.ExtraData and utDeleteFile_DisableFsRedir <> 0,
                     CurRec^.ExtraData and utDeleteFile_CallChangeNotify <> 0,
                     CurRec^.ExtraData and utDeleteFile_RestartDelete <> 0,
                     CurRec^.ExtraData and utDeleteFile_RemoveReadOnly <> 0) then
                    Result := False;
              end
              else begin
                { We're running from Setup, and the file existed before
                  installation... }
                if CurRec^.ExtraData and utDeleteFile_SharedFile <> 0 then
                  LoggedDecrementSharedCount(CurRecData[0],
                    CurRec^.ExtraData and utDeleteFile_SharedFileIn64BitKey <> 0);
                { Delete file only if it's a temp file }
                if Filename <> CurRecData[0] then
                  if not LoggedFileDelete(Filename, CurRec^.ExtraData and utDeleteFile_DisableFsRedir <> 0,
                     CurRec^.ExtraData and utDeleteFile_CallChangeNotify <> 0,
                     CurRec^.ExtraData and utDeleteFile_RestartDelete <> 0,
                     CurRec^.ExtraData and utDeleteFile_RemoveReadOnly <> 0) then
                    Result := False;
              end;
            end;
          utDeleteGroupOrItem: ;   { dummy - no longer supported }
          utIniDeleteEntry: begin
              Section := CurRecData[1];
              Key := CurRecData[2];
              Filename := CurRecData[0];
              LogFmt('Deleting INI entry: %s in section %s in %s', [Key, Section, GetLogIniFilename(Filename)]);
              DeleteIniEntry(Section, Key, Filename);
            end;
          utIniDeleteSection: begin
              Section := CurRecData[1];
              Filename := CurRecData[0];
              if (CurRec^.ExtraData and utIniDeleteSection_OnlyIfEmpty = 0) or
                 IsIniSectionEmpty(Section, Filename) then begin
                LogFmt('Deleting INI section: %s in %s', [Section, GetLogIniFilename(Filename)]);
                DeleteIniSection(Section, Filename);
              end;
            end;
          utRegDeleteEntireKey: begin
              CrackRegExtraData(CurRec^.ExtraData, RegView, RootKey);
              Subkey := CurRecDataPChar[0];
              LogFmt('Deleting registry key: %s\%s', [GetRegRootKeyName(RootKey), Subkey]);
              ErrorCode := RegDeleteKeyIncludingSubkeys(RegView, RootKey, Subkey);
              if not (ErrorCode in [ERROR_SUCCESS, ERROR_FILE_NOT_FOUND]) then begin
                LogFmt('Deletion failed (%d).', [ErrorCode]);
                Result := False;
              end;
            end;
          utRegClearValue: begin
              CrackRegExtraData(CurRec^.ExtraData, RegView, RootKey);
              Subkey := CurRecDataPChar[0];
              ValueName := CurRecDataPChar[1];
              LogFmt('Clearing registry value: %s\%s\%s', [GetRegRootKeyName(RootKey), Subkey, ValueName]);
              if RegOpenKeyExView(RegView, RootKey, Subkey, 0, KEY_SET_VALUE, K) = ERROR_SUCCESS then begin
                ErrorCode := RegSetValueEx(K, ValueName, 0, REG_SZ, @NullChar, SizeOf(NullChar));
                if ErrorCode <> ERROR_SUCCESS then begin
                  LogFmt('RegSetValueEx failed (%d).', [ErrorCode]);
                  Result := False;
                end;
                RegCloseKey(K);
              end;
            end;
          utRegDeleteKeyIfEmpty: begin
              CrackRegExtraData(CurRec^.ExtraData, RegView, RootKey);
              Subkey := CurRecDataPChar[0];
              LogFmt('Deleting empty registry key: %s\%s', [GetRegRootKeyName(RootKey), Subkey]);
              ErrorCode := RegDeleteKeyIfEmpty(RegView, RootKey, Subkey);
              if ErrorCode = ERROR_DIR_NOT_EMPTY then
                Log('Deletion skipped (not empty).')
              else if not (ErrorCode in [ERROR_SUCCESS, ERROR_FILE_NOT_FOUND]) then begin
                LogFmt('Deletion failed (%d).', [ErrorCode]);
                Result := False;
              end;
            end;
          utRegDeleteValue: begin
              CrackRegExtraData(CurRec^.ExtraData, RegView, RootKey);
              Subkey := CurRecDataPChar[0];
              ValueName := CurRecDataPChar[1];
              LogFmt('Deleting registry value: %s\%s\%s', [GetRegRootKeyName(RootKey), Subkey, ValueName]);
              if RegOpenKeyExView(RegView, RootKey, Subkey, 0, KEY_QUERY_VALUE or KEY_SET_VALUE, K) = ERROR_SUCCESS then begin
                if RegValueExists(K, ValueName) then begin
                  ErrorCode := RegDeleteValue(K, ValueName);
                  if ErrorCode <> ERROR_SUCCESS then begin
                    LogFmt('RegDeleteValue failed (%d).', [ErrorCode]);
                    Result := False;
                  end;
                end;
                RegCloseKey(K);
              end;
            end;
          utDecrementSharedCount: begin
              LoggedDecrementSharedCount(CurRecData[0],
                CurRec^.ExtraData and utDecrementSharedCount_64BitKey <> 0);
            end;
          utRefreshFileAssoc:
            RefreshFileAssoc := True;
          utMutexCheck: ;    { do nothing; utMutexChecks aren't processed here }
        else
          raise Exception.Create(FmtSetupMessage1(msgUninstallUnknownEntry,
            Format('$%x', [CurRec^.Typ])));
        end;
      except
        Result := False;
        if not(ExceptObject is EAbort) then
          HandleException;
      end;
      CurRec := Delete(CurRec);
      StatusUpdate(StartCount, FCount);
    end;

    if RefreshFileAssoc then
      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
    if ufChangesEnvironment in Flags then
      RefreshEnvironment;
    if Assigned(DeleteUninstallDataFilesProc) then begin
      DeleteUninstallDataFilesProc;
      { Now that uninstall data is deleted, try removing the directories it
        was in that couldn't be deleted before. }
      LoggedProcessDirsNotRemoved;
    end;
  finally
    DeleteDirData.DirsNotRemoved.Free;
    RestartDeleteDirList[True].Free;
    RestartDeleteDirList[False].Free;
    for P := 0 to ChangeNotifyList.Count-1 do
      if DirExists(ChangeNotifyList[P]) then
        SHChangeNotify(SHCNE_UPDATEDIR, SHCNF_PATH or SHCNF_FLUSH,
          PChar(ChangeNotifyList[P]), nil);
    UnregisteredServersList[True].Free;
    UnregisteredServersList[False].Free;
    RunOnceList.Free;
    ChangeNotifyList.Free;
  end;
  Log('Uninstallation process succeeded.');
end;

function TUninstallLog.ShouldRemoveSharedFile(const Filename: String): Boolean;
begin
  Result := True;
end;

procedure TUninstallLog.StatusUpdate(StartingCount, CurCount: Integer);
begin
end;

class function TUninstallLog.WriteSafeHeaderString(Dest: PAnsiChar;
 const Source: String; MaxDestBytes: Cardinal): Cardinal;
{ Copies a string into a PAnsiChar including null terminator, either directly
  if Source only contains ASCII characters, or else UTF-8-encoded with a special
  #1 marker. If MaxDestBytes = 0 it returns the amount of bytes needed. }
var
  N: Integer;
  I: Integer;
begin
  N := Length(Source);
  { Only UTF-8-encode when non-ASCII characters are present }
  for I := 1 to N do begin
    if Ord(Source[I]) > 126 then begin
      if MaxDestBytes <> 0 then begin
        Dest^ := #1;
        Inc(Dest);
        Dec(MaxDestBytes);
      end;
      Result := SizeOf(Dest^) + UnicodeToUtf8(Dest, MaxDestBytes, PWideChar(Source), N + 1);
      Exit;
    end;
  end;
  if MaxDestBytes <> 0 then
    AnsiStrings.StrPLCopy(Dest, AnsiString(Source), MaxDestBytes - 1);
  Result := (N + 1) * SizeOf(Dest^);
end;

class function TUninstallLog.ReadSafeHeaderString(const Source: AnsiString): String;
begin
  if (Source <> '') and (Source[1] = #1) then
    Result := UTF8ToString(Copy(Source, 2, Maxint))
  else
    Result := String(Source);
end;

procedure TUninstallLog.Save(const Filename: String;
  const Append, UpdateUninstallLogAppName: Boolean);
{ Saves all undo data to Filename. If Append is True, it appends the current
  undo data to the end of the existing file. When Append is True, it assumes
  compatibility has already been verified with the Test method. }
var
  F: TFile;
  Buffer: array[0..4095] of Byte;
  BufCount: Cardinal;

  procedure Flush;
  var
    CrcHeader: TUninstallCrcHeader;
  begin
    if BufCount <> 0 then begin
      CrcHeader.Size := BufCount;
      CrcHeader.NotSize := not CrcHeader.Size;
      CrcHeader.CRC := GetCRC32(Buffer, BufCount);
      F.WriteBuffer(CrcHeader, SizeOf(CrcHeader));
      F.WriteBuffer(Buffer, BufCount);
      BufCount := 0;
    end;
  end;

  procedure WriteBuf(const Buf; Size: Cardinal);
  var
    P: Pointer;
    S: Cardinal;
  begin
    P := @Buf;
    while Size <> 0 do begin
      S := Size;
      if S > SizeOf(Buffer) - BufCount then
        S := SizeOf(Buffer) - BufCount;
      Move(P^, Buffer[BufCount], S);
      Inc(BufCount, S);
      if BufCount = SizeOf(Buffer) then
        Flush;
      Inc(Cardinal(P), S);
      Dec(Size, S);
    end;
  end;

var
  Header: TUninstallLogHeader;
  FileRec: TUninstallFileRec;
  CurRec: PUninstallRec;
begin
  BufCount := 0;
  if not Append then
    F := TFile.Create(Filename, fdCreateAlways, faReadWrite, fsNone)
  else
    F := TFile.Create(Filename, fdOpenExisting, faReadWrite, fsNone);
  try
    if not Append then begin
      FillChar(Header, SizeOf(Header), 0);
      F.WriteBuffer(Header, SizeOf(Header));
      { Note: It will go back and fill in the correct values later }
    end
    else begin
      F.ReadBuffer(Header, SizeOf(Header));
      F.Seek(Header.EndOffset);
      { If there's anything past EndOffset (only possible if some kind of
        fatal error occurred while updating the file last time), clear it out }
      F.Truncate;
    end;

    CurRec := List;
    while CurRec <> nil do begin
      FileRec.Typ := Ord(CurRec^.Typ);
      FileRec.ExtraData := CurRec^.ExtraData;
      FileRec.DataSize := CurRec^.DataSize;
      WriteBuf(FileRec, SizeOf(FileRec));
      WriteBuf(CurRec^.Data, CurRec^.DataSize);
      if (Header.NumRecs < 0) or (Header.NumRecs >= High(Header.NumRecs)) then
        InternalError('NumRecs range exceeded');
      Inc(Header.NumRecs);

      CurRec := CurRec^.Next;
    end;
    Flush;

    if F.Position.Hi <> 0 then
      InternalError('EndOffset range exceeded');
    Header.EndOffset := F.Position.Lo;
    F.Seek(0);
    Header.ID := UninstallLogID[InstallMode64Bit];
    WriteSafeHeaderString(Header.AppId, AppId, SizeOf(Header.AppId));
    if not Append or UpdateUninstallLogAppName then
      WriteSafeHeaderString(Header.AppName, AppName, SizeOf(Header.AppName));
    if Version > Header.Version then
      Header.Version := Version;
    TUninstallLogFlags((@Header.Flags)^) := TUninstallLogFlags((@Header.Flags)^) - [ufModernStyle] + Flags;
    Header.CRC := GetCRC32(Header, SizeOf(Header)-SizeOf(Longint));
    { Prior to rewriting the header with the new EndOffset value, ensure the
      records we wrote earlier are flushed to disk. This should prevent the
      file from ever becoming corrupted/unreadable in the event the system
      crashes a split second from now. At worst, EndOffset will have the old
      value and any extra bytes past EndOffset will be ignored/discarded when
      the file is read at uninstall time, or appended to the next time Setup
      is run. }
    FlushFileBuffers(F.Handle);
    F.WriteBuffer(Header, SizeOf(Header));
  finally
    F.Free;
  end;
end;

procedure TUninstallLog.Load(const F: TFile; const Filename: String);
{ Loads all undo data from the open file F. The Filename parameter is only
  used when generating exception error messages.
  Note: The position of the file pointer after calling this function is
  undefined. }
var
  Buffer: array[0..4095] of Byte;
  BufPos, BufLeft: Cardinal;
  Header: TUninstallLogHeader;

  procedure Corrupt;
  begin
    raise Exception.Create(FmtSetupMessage1(msgUninstallDataCorrupted, Filename));
  end;

  procedure FillBuffer;
  var
    EndOffset, Ofs: Integer64;
    CrcHeader: TUninstallCrcHeader;
  begin
    EndOffset.Lo := Header.EndOffset;
    EndOffset.Hi := 0;
    while BufLeft = 0 do begin
      Ofs := F.Position;
      Inc64(Ofs, SizeOf(CrcHeader));
      if Compare64(Ofs, EndOffset) > 0 then
        Corrupt;
      if F.Read(CrcHeader, SizeOf(CrcHeader)) <> SizeOf(CrcHeader) then
        Corrupt;
      Ofs := F.Position;
      Inc64(Ofs, CrcHeader.Size);
      if (CrcHeader.Size <> not CrcHeader.NotSize) or
         (Cardinal(CrcHeader.Size) > Cardinal(SizeOf(Buffer))) or
         (Compare64(Ofs, EndOffset) > 0) then
        Corrupt;
      if F.Read(Buffer, CrcHeader.Size) <> CrcHeader.Size then
        Corrupt;
      if not(ufDontCheckRecCRCs in Flags) and
        (CrcHeader.CRC <> GetCRC32(Buffer, CrcHeader.Size)) then
        Corrupt;
      BufPos := 0;
      BufLeft := CrcHeader.Size;
    end;
  end;

  procedure ReadBuf(var Buf; Size: Cardinal);
  var
    P: Pointer;
    S: Cardinal;
  begin
    P := @Buf;
    while Size <> 0 do begin
      if BufLeft = 0 then
        FillBuffer;
      S := Size;
      if S > BufLeft then
        S := BufLeft;
      Move(Buffer[BufPos], P^, S);
      Inc(BufPos, S);
      Dec(BufLeft, S);
      Inc(Cardinal(P), S);
      Dec(Size, S);
    end;
  end;

var
  FileRec: TUninstallFileRec;
  I: Integer;
  NewRec: PUninstallRec;
begin
  BufPos := 0;
  BufLeft := 0;

  ReadUninstallLogHeader(F, Filename, Header, InstallMode64Bit);
  if Header.Version > HighestSupportedVersion then
    raise Exception.Create(FmtSetupMessage1(msgUninstallUnsupportedVer, Filename));
  AppId := ReadSafeHeaderString(Header.AppId);
  AppName := ReadSafeHeaderString(Header.AppName);
  Flags := TUninstallLogFlags((@Header.Flags)^);

  for I := 1 to Header.NumRecs do begin
    ReadBuf(FileRec, SizeOf(FileRec));
    NewRec := AllocRec(FileRec.Typ, FileRec.ExtraData, FileRec.DataSize);
    if NewRec = nil then
      Corrupt;  { DataSize was out of range }
    try
      ReadBuf(NewRec.Data, NewRec.DataSize);
    except
      FreeMem(NewRec);
      raise;
    end;
    InternalAdd(NewRec);
  end;
end;

function TUninstallLog.CanAppend(const Filename: String;
  var ExistingFlags: TUninstallLogFlags): Boolean;
{ Returns True if Filename is a recognized uninstall log format, and its header
  matches our AppId and InstallMode64Bit settings. When True is returned,
  the existing log's flags are assigned to ExistingFlags. }
var
  F: TFile;
  Header: TUninstallLogHeader;
begin
  Result := False;
  try
    F := TFile.Create(Filename, fdOpenExisting, faRead, fsRead);
    try
      if F.Read(Header, SizeOf(Header)) <> SizeOf(Header) then
        Exit;
      if ((Header.CRC <> $11111111) and
          { ^ for debugging purposes, you can change the CRC field in the file to
            $11111111 to disable CRC checking on the header}
          (Header.CRC <> GetCRC32(Header, SizeOf(Header)-SizeOf(Longint)))) or
         (Header.ID <> UninstallLogID[InstallMode64Bit]) or
         (ReadSafeHeaderString(Header.AppId) <> AppId) then
        Exit;
      ExistingFlags := TUninstallLogFlags((@Header.Flags)^);
      Result := True;
    finally
      F.Free;
    end;
  except
  end;
end;

end.
