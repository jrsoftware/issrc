unit Setup.PathRedir;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  The ApplyPathRedirRules function rewrites paths containing System32,
  SysWOW64, and Sysnative to achieve the same effect as disabling/enabling
  WOW64 file system redirection.

  Before rewriting, the path is expanded and converted to a super
  (extended-length) path.

  Used only by the Setup project.
}

interface

uses
  Windows, SysUtils;

type
  TPathRedirTargetProcess = (tpCurrent, tpNativeBit, tp32Bit);

function ApplyPathRedirRules(const A64Bit: Boolean; const APath: String;
  const ATargetProcess: TPathRedirTargetProcess = tpCurrent): String;
procedure InitializePathRedir(const AWindows64Bit: Boolean;
  const ASystem32Path, ASysWow64Path, ASysNativePath: String);

implementation

uses
  PathFunc,
  Setup.InstFunc;

type
  TPathRedir = class
  strict private
    FWindows64Bit: Boolean;
    FSystem32Path, FSysWow64Path, FSysNativePath: String;
  public
    constructor Create(const AWindows64Bit: Boolean;
      const ASystem32Path, ASysWow64Path, ASysNativePath: String);
    function ApplyRules(const A64Bit: Boolean; const APath: String;
      const ATargetProcess: TPathRedirTargetProcess): String;
  end;

var
  [volatile] PathRedirInstance: TPathRedir;
  [volatile] PathRedirActiveUseCount: Integer;

procedure InitializePathRedir(const AWindows64Bit: Boolean;
  const ASystem32Path, ASysWow64Path, ASysNativePath: String);
begin
  const LInstance = TPathRedir.Create(AWindows64Bit, ASystem32Path,
    ASysWow64Path, ASysNativePath);
  MemoryBarrier;
  if AtomicCmpExchange(Pointer(PathRedirInstance), Pointer(LInstance), nil) <> nil then begin
    LInstance.Free;
    InternalError('PathRedir: Already initialized');
  end;
end;

function ApplyPathRedirRules(const A64Bit: Boolean; const APath: String;
  const ATargetProcess: TPathRedirTargetProcess = tpCurrent): String;
begin
  while True do begin
    const CurCount = PathRedirActiveUseCount;
    if CurCount < 0 then
      InternalError('PathRedir: Unit was finalized');
    if AtomicCmpExchange(PathRedirActiveUseCount, CurCount + 1, CurCount) = CurCount then
      Break;
  end;
  MemoryBarrier;
  try
    if PathRedirInstance = nil then
      InternalError('PathRedir: Not initialized');
    Result := PathRedirInstance.ApplyRules(A64Bit, APath, ATargetProcess);
  finally
    MemoryBarrier;
    AtomicDecrement(PathRedirActiveUseCount);
  end;
end;

function ConvertToSuperPath(var Path: String): Boolean;
begin
  if Length(Path) >= 3 then begin
    if PathStartsWith(Path, '\\?\') then
      Exit(True);

    if PathStartsWith(Path, '\\.\') then begin
      Path[3] := '?';
      Exit(True);
    end;

    if CharInSet(UpCase(Path[1]), ['A'..'Z']) and
       (Path[2] = ':') and (Path[3] = '\') then begin
      Insert('\\?\', Path, 1);
      Exit(True);
    end;

    if (Path[1] = '\') and (Path[2] = '\') then begin
      Path := '\\?\UNC\' + Copy(Path, 3, Maxint);
      Exit(True);
    end;
  end;
  Result := False;
end;

{ TPathRedir }

constructor TPathRedir.Create(const AWindows64Bit: Boolean;
  const ASystem32Path, ASysWow64Path, ASysNativePath: String);

  procedure CheckAndAssignPath(var OutPath: String; const Dir, Title: String);
  begin
    var TestExpandedPath: String;
    if (Length(Dir) >= 4) and CharInSet(UpCase(Dir[1]), ['A'..'Z']) and
       (Dir[2] = ':') and (Dir[3] = '\') and
       PathExpand(Dir, TestExpandedPath) and
       PathSame(Dir, TestExpandedPath) and
       not PathCharIsSlash(Dir[High(Dir)]) then begin
      OutPath := '\\?\' + Dir;
      Exit;
    end;
    InternalErrorFmt('Path for %s directory is invalid: "%s"', [Title, Dir]);
  end;

begin
  inherited Create;
  if AWindows64Bit then begin
    CheckAndAssignPath(FSystem32Path, ASystem32Path, 'System32');
    CheckAndAssignPath(FSysWow64Path, ASysWow64Path, 'SysWOW64');
    CheckAndAssignPath(FSysNativePath, ASysNativePath, 'Sysnative');
  end;
  FWindows64Bit := AWindows64Bit;
end;

function TPathRedir.ApplyRules(const A64Bit: Boolean; const APath: String;
  const ATargetProcess: TPathRedirTargetProcess): String;

  procedure SubstitutePath(var Path: String; const FromDir, ToDir: String);
  begin
    { Just an extra layer of safety }
    if (FromDir = '') or (ToDir = '') then
      InternalError('PathRedir: SubstitutePath received invalid parameter');

    const PathLen = Length(Path);
    const FromDirLen = Length(FromDir);
    if (PathLen = FromDirLen) or
       ((PathLen > FromDirLen) and (Path[Low(Path) + FromDirLen] = '\')) then
      if PathStartsWith(Path, FromDir) then
        Path := ToDir + Copy(Path, FromDirLen+1, Maxint);
  end;

begin
  if APath = '' then
    InternalError('PathRedir: Called with empty path string');
  { Windows supports an undocumented "\??\" prefix that works like "\\?\".
    However, PathExpand (GetFullPathName) doesn't understand it and will
    prepend the current drive (e.g., "C:\??\"). So don't allow it. }
  if PathStartsWith(APath, '\??\') then
    InternalError('PathRedir: "\??\" prefix not allowed');

  var NewPath: String;
  if not PathExpand(APath, NewPath) then
    InternalError('PathRedir: PathExpand failed');
  if not ConvertToSuperPath(NewPath) then
    InternalError('PathRedir: ConvertToSuperPath failed');

  if FWindows64Bit then begin
    { Running on 64-bit Windows }
    const TargetProcess64Bit =
      {$IFDEF WIN64} (ATargetProcess = tpCurrent) or {$ENDIF}
      (ATargetProcess = tpNativeBit);

    if A64Bit then begin
      { System32 -> Sysnative: When path is 64-bit and process is 32-bit. }
      if not TargetProcess64Bit then
        SubstitutePath(NewPath, FSystem32Path, FSysNativePath);
    end else begin
      { System32 -> SysWOW64: When path is 32-bit and process is 64-bit.
        In the future, this may be done for 32-bit processes as well. }
      if TargetProcess64Bit then
        SubstitutePath(NewPath, FSystem32Path, FSysWow64Path);
    end;

    { Sysnative -> System32: When process is 64-bit, regardless of path
      bitness (because the Sysnative alias never works in 64-bit processes). }
    if TargetProcess64Bit then
      SubstitutePath(NewPath, FSysNativePath, FSystem32Path);
  end else begin
    { Running on 32-bit Windows; no substitutions are made }
    if A64Bit then
      InternalError('PathRedir: A64Bit=True but not running 64-bit Windows');
  end;

  Result := NewPath;
end;

initialization
finalization
  if AtomicExchange(PathRedirActiveUseCount, -1) = 0 then begin
    MemoryBarrier;
    FreeAndNil(PathRedirInstance);
  end;
end.
