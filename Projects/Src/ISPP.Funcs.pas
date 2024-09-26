{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff

  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

unit ISPP.Funcs;

interface

uses
  Windows, Classes, ISPP.VarUtils, ISPP.Intf, ISPP.Preprocessor, ISPP.Parser;

procedure RegisterFunctions(Preproc: TPreprocessor);

implementation

uses
  SysUtils, IniFiles, Registry, Math, ISPP.Consts, ISPP.Base, ISPP.IdentMan,
  ISPP.Sessions, DateUtils, Shared.FileClass, MD5, SHA1, SHA256, PathFunc, Shared.CommonFunc,
  Shared.Int64Em;
  
var
  IsWin64: Boolean;

function PrependPath(const Ext: Longint; const Filename: String): String;
begin
  var Preprocessor := TObject(Ext) as TPreprocessor;
  Result := PathExpand(Preprocessor.PrependDirName(Filename,
    Preprocessor.SourcePath));
end;

function CheckParams(const Params: IIsppFuncParams;
  Types: array of TIsppVarType; Minimum: Byte; var Error: TIsppFuncResult): Boolean;
var
  I: Integer;
begin
  FillChar(Error, SizeOf(TIsppFuncResult), 0);
  Result := False;
  if Params.GetCount < Minimum then
  begin
    Error.ErrParam := Minimum;
    Error.Error := ISPPFUNC_INSUFARGS;
    Exit;
  end
  else if Params.GetCount > (High(Types) + 1) then
  begin
    Error.ErrParam := High(Types) + 1;
    Error.Error := ISPPFUNC_MANYARGS;
    Exit;
  end
  else
    with IInternalFuncParams(Params) do
      for I := 0 to Params.GetCount - 1 do
      begin
        if (Types[I] = evSpecial) or (Get(I)^.Typ = evNull) then Continue;
        if Types[I] <> Get(I)^.Typ then
        begin
          if Types[I] = evStr then
            Error.Error := ISPPFUNC_STRWANTED
          else
            Error.Error := ISPPFUNC_INTWANTED;
          Error.ErrParam := I;
          Exit;
        end;
      end;
  Result := True;
end;

function Int(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;

  procedure MakeError(E: Exception);
  begin
    FuncResult.Error(PChar(E.Message));
    Result.Error := ISPPFUNC_FAIL;
  end;

begin
  if CheckParams(Params, [evSpecial, evInt], 1, Result) then
  try
    with IInternalFuncParams(Params) do
      ResPtr^ := ToInt(Get(0)^);
  except
    on E: EConvertError do
      with IInternalFuncParams(Params) do
      begin
        if GetCount > 1 then
          ResPtr^ := Get(1)^
        else
          MakeError(E);
      end;
    on E: Exception do
      MakeError(E);
  end;
end;

function Str(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evSpecial], 1, Result) then
  try
    with IInternalFuncParams(Params) do
      ResPtr^ := ToStr(Get(0)^);
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

{FileExists(<filename>)}
function FileExists(Ext: Longint; const Params: IIsppFuncParams; const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
      MakeBool(ResPtr^, NewFileExists(PrependPath(Ext, Get(0).AsStr)));
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function DirExists(Ext: Longint; const Params: IIsppFuncParams; const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
      MakeBool(ResPtr^, Shared.CommonFunc.DirExists(PrependPath(Ext, Get(0).AsStr)));
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function ForceDirectoriesFunc(Ext: Longint; const Params: IIsppFuncParams; const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
      MakeBool(ResPtr^, ForceDirectories(PrependPath(Ext, Get(0).AsStr)));
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

{FileSize(<filename>)}
function FileSize(Ext: Longint; const Params: IIsppFuncParams; const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  SearchRec: TSearchRec;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      if FindFirst(PrependPath(Ext, Get(0).AsStr), faAnyFile, SearchRec) = 0 then begin
        try
          MakeInt(ResPtr^, SearchRec.Size);
        finally
          FindClose(SearchRec);
        end;
      end else
        MakeInt(ResPtr^, -1);
    end
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

{ReadIni(<file:str>,<section:str>,<name:str>,[<default:str>])}
function ReadIni(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  Default: string;
begin
  if CheckParams(Params, [evStr, evStr, evStr, evStr], 3, Result) then
  try
    with IInternalFuncParams(Params) do
      with TIniFile.Create(Get(0).AsStr) do
      try
        if GetCount < 4 then Default := '' else Default := Get(3).AsStr;
        MakeStr(ResPtr^, ReadString(Get(1).AsStr, Get(2).AsStr, Default));
      finally
        Free
      end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function WriteIni(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr, evStr, evStr, evSpecial], 4, Result) then
  try
    with IInternalFuncParams(Params) do
      with TIniFile.Create(Get(0).AsStr) do
      try
        case Get(3).Typ of
          evInt: WriteInteger(Get(1).AsStr, Get(2).AsStr, Get(3).AsInt);
          evStr: WriteString(Get(1).AsStr, Get(2).AsStr, Get(3).AsStr);
        else
          WriteString(Get(1).AsStr, Get(2).AsStr, '');
        end;
        ResPtr^ := NULL;
      finally
        Free;
      end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

{ReadReg(<root:int>,<key:str>,[<name:str>,<default:str>])}
function ReadReg(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
const
  ISPPRootKeyFlagMask  = $7F000000;
  ISPPRootKeyFlag64Bit = $02000000;
  ISPPRootKeyValidFlags = ISPPRootKeyFlag64Bit;

  procedure CrackISPPRootKey(const ISPPRootKey: Longint; var RegView64: Boolean;
    var RootKey: HKEY);
  begin
    { Allow only predefined key handles (8xxxxxxx). Can't accept handles to
      open keys because they might have our special flag bits set.
      Also reject unknown flags which may have a meaning in the future. }
    if (ISPPRootKey shr 31 <> 1) or
       ((ISPPRootKey and ISPPRootKeyFlagMask) and not ISPPRootKeyValidFlags <> 0) then
      raise Exception.Create('Invalid root key value');

    if ISPPRootKey and ISPPRootKeyFlag64Bit <> 0 then begin
      if not IsWin64 then
        raise Exception.Create('Cannot access 64-bit registry keys on this version of Windows');
      RegView64 := True
    end
    else
      RegView64 := False;
    RootKey := ISPPRootKey and not ISPPRootKeyFlagMask;
  end;

var
  Name: string;
  Default: TIsppVariant;
  RegView64: Boolean;
  ARootKey: HKEY;
  AAccess: LongWord;
begin
  if CheckParams(Params, [evInt, evStr, evStr, evSpecial], 2, Result) then
  try
    with IInternalFuncParams(Params) do begin
      CrackISPPRootKey(Get(0).AsInt, RegView64, ARootKey);
      AAccess := KEY_QUERY_VALUE;
      if RegView64 then
        AAccess := AAccess or KEY_WOW64_64KEY;
      with TRegistry.Create(AAccess) do
      try
        RootKey := ARootKey;
        if GetCount < 3 then Name := '' else Name := Get(2).AsStr;
        if GetCount < 4 then Default := NULL else Default := Get(3)^;
        if OpenKey(Get(1).AsStr, False) and ((Name = '') or ValueExists(Name)) then
          case GetDataType(Name) of
            rdString, rdExpandString: MakeStr(ResPtr^, ReadString(Name));
            rdInteger: MakeInt(ResPtr^, ReadInteger(Name));
          else
            CopyExpVar(Default, ResPtr^);
          end
        else
          CopyExpVar(Default, ResPtr^);
      finally
        Free
      end;
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetEnvFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
      MakeStr(ResPtr^, GetEnv(Get(0).AsStr));
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

const
  SSetup = '[SETUP]';

function SetupSetting(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;

  function Find(L: TStrings; const S: string): string;
  var
    I, J: Integer;
    InSetupSection: Boolean;
    N: string;
  begin
    InSetupSection := False;
    Result := '';
    with L do
      for I := 0 to Count - 1 do
      begin
        if Trim(Strings[I]) = '' then Continue;
        if InSetupSection then
        begin
          if (Trim(Strings[I])[1] = '[') then
          begin
            if CompareText(Trim(Strings[I]), SSetup) <> 0 then
              InSetupSection := False;
            Continue;
          end;
          J := Pos('=', Strings[I]);
          if J > 0 then N := Trim(Copy(Strings[I], 1, J - 1));
          if CompareText(N, S) = 0 then
          begin
            Result := Trim(Copy(Strings[I], J + 1, MaxInt));
            Break;
          end;
        end
        else
          if CompareText(Trim(Strings[I]), SSetup) = 0 then InSetupSection := True;
      end;
  end;

begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      MakeStr(ResPtr^, Find(TPreprocessor(Ext).StringList, Get(0).AsStr));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

{SetSetupSetting(<SetupSectionParameterName>,<ParameterValue>)}
function SetSetupSetting(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;

  procedure DoSet(L: TStrings; const S, V: string);
  var
    I, J, FirstSetupSectionLine: Integer;
    InSetupSection: Boolean;
    N: string;
  begin
    FirstSetupSectionLine := -1;
    InSetupSection := False;
    with L do
    begin
      for I := 0 to Count - 1 do
      begin
        if Trim(Strings[I]) = '' then Continue;
        if InSetupSection then
        begin
          if (Trim(Strings[I])[1] = '[') then
          begin
            if CompareText(Trim(Strings[I]), SSetup) <> 0 then
              InSetupSection := False;
            Continue;
          end;
          J := Pos('=', Strings[I]);
          if J > 0 then N := Trim(Copy(Strings[I], 1, J - 1));
          if CompareText(N, S) = 0 then
          begin
            Strings[I] := S + '=' + V;
            Exit;
          end;
        end
        else
          if CompareText(Trim(Strings[I]), SSetup) = 0 then
          begin
            InSetupSection := True;
            if FirstSetupSectionLine < 0 then
              FirstSetupSectionLine := I;
          end;
      end;
      if FirstSetupSectionLine < 0 then
        FirstSetupSectionLine := L.Add(SSetup);
      L.Insert(FirstSetupSectionLine + 1, S + '=' + V);
    end;
  end;

begin
  if CheckParams(Params, [evStr, evStr], 2, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      ResPtr^.Typ := evNull;
      DoSet(TPreprocessor(Ext).StringList, Get(0).AsStr, Get(1).AsStr);
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

{EntryCount(<SectionName>)}
function EntryCountFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  I, J: Integer;
  DoCount: Boolean;
  N, S: string;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    J := 0;
    DoCount := False;
    with IInternalFuncParams(Params), TStringList(TPreprocessor(Ext).StringList) do
    begin
      S := Get(0).AsStr;
      for I := 0 to Count - 1 do
      begin
        N := Trim(Strings[I]);
        if (N <> '') and (N[1] <> ';') and (N[1] = '[') then
        begin
          if DoCount then
            DoCount := False
          else
            if CompareText(Copy(N, 2, Length(N) - 2), S) = 0 then
              DoCount := True;
          Continue;
        end;
        if DoCount and (N <> '') and (N[1] <> ';') then Inc(J);
      end;
      MakeInt(ResPtr^, J);
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

{SaveToFile(<Filename>)}
function SaveToFile(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
      TPreprocessor(Ext).SaveToFile(PrependPath(Ext, Get(0).AsStr));
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

{Find(<what>[,<contains>[,<what>,<contains>[,<what>[,<contains>]]]])}
function FindLine(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
const
  FIND_WHEREMASK  = $01 or $02;
  FIND_SENSITIVE  = $04;
  FIND_OR         = $08;
  FIND_NOT        = $10;
  FIND_TRIM       = $20;
type
  TFindWhere = (fwMatch, fwBegin, fwEnd, fwContains);
var
  I: Integer;
  StartFromLine: Integer;
  Found, MoreFound, Second, Third: Boolean;
  Flags: array[0..2] of Integer;
  Strs: array[0..2] of string;
  Str: string;

  function Compare(const S1, S2: string; Sensitive: Boolean): Boolean;
  begin
    if Sensitive then
      Result := AnsiCompareStr(S1, S2) = 0
    else
      Result := AnsiCompareText(S1, S2) = 0;
  end;

  function Contains(const Substr: string; Sensitive: Boolean): Boolean;
  var
    L, I: Integer;
  begin
    Result := True;
    L := Length(Substr);
    for I := 1 to Length(Str) - L + 1 do
      if Compare(Substr, Copy(Str, I, L), Sensitive) then Exit;
    Result := False;
  end;

  function Meets(const Substr: string; Sensitive: Boolean; Where: Byte): Boolean;
  begin
    Result := False;
    case Where of
      1: if Length(Substr) <= Length(Str) then
            Result := Compare(Substr, Copy(Str, 1, Length(Substr)), Sensitive);
      2: if Length(Substr) <= Length(Str) then
            Result := Compare(Substr, Copy(Str, Length(Str) - Length(Substr) + 1, Length(Substr)), Sensitive);
      3: if Length(Substr) <= Length(Str) then
            Result := Contains(Substr, Sensitive);
      else Result := Compare(Substr, Str, Sensitive);
    end;
  end;

begin
  if CheckParams(Params, [evInt, evStr, evInt, evStr, evInt, evStr, evInt], 2, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      FillChar(Flags, SizeOf(Flags), 0);
      Strs[0] := Get(1).AsStr;
      Second := False;
      Third := False;
      if GetCount > 2 then
      begin
        Flags[0] := Get(2).AsInt;
        if GetCount > 3 then
        begin
          Strs[1] := Get(3).AsStr;
          Second := True;
          if GetCount > 4 then
          begin
            Flags[1] := Get(4).AsInt;
            if GetCount > 5 then
            begin
              Strs[2] := Get(5).AsStr;
              Third := True;
              if GetCount > 6 then Flags[2] := Get(6).AsInt;
            end
          end;
        end
      end;
      StartFromLine := Get(0).AsInt;
      if StartFromLine < 0 then StartFromLine := 0;
      with TStringList(TPreprocessor(Ext).StringList) do
        for I := StartFromLine to Count - 1 do
        begin
          Str := Strings[I];
          if Flags[0] and FIND_TRIM <> 0 then
            Str := Trim(Str);
          Found := Meets(Strs[0], Flags[0] and FIND_SENSITIVE <> 0,
            Flags[0] and FIND_WHEREMASK) xor (Flags[0] and FIND_NOT <> 0);

          if Second and (((Flags[1] and FIND_OR <> 0{OR}) and not Found) or
            ((Flags[1] and FIND_OR = 0{AND}) and Found)) then
          begin
            MoreFound := Meets(Strs[1], Flags[1] and FIND_SENSITIVE <> 0,
              Flags[1] and FIND_WHEREMASK) xor (Flags[1] and FIND_NOT <> 0);
            if Flags[1] and FIND_OR <> 0 then
              Found := Found or MoreFound
            else
              Found := Found and MoreFound;
          end;

          if Third and (((Flags[2] and FIND_OR <> 0{OR}) and not Found) or
            ((Flags[2] and FIND_OR = 0{AND}) and Found)) then
          begin
            MoreFound := Meets(Strs[2], Flags[2] and FIND_SENSITIVE <> 0,
              Flags[2] and FIND_WHEREMASK) xor (Flags[2] and FIND_NOT <> 0);
            if Flags[2] and FIND_OR <> 0 then
              Found := Found or MoreFound
            else
              Found := Found and MoreFound;
          end;

          if Found then
          begin
            MakeInt(ResPtr^, I);
            Exit;
          end;
        end;
      MakeInt(ResPtr^, -2);
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function Exec(const Filename, Params: String; WorkingDir: String;
  const WaitUntilTerminated: Boolean; const ShowCmd: Integer;
  const Preprocessor: TPreprocessor; const OutputReader: TCreateProcessOutputReader;
  var ResultCode: Integer): Boolean;
var
  CmdLine: String;
  WorkingDirP: PChar;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  {This function is a combination of InstFuncs' InstExec and Compile's InternalSignCommand }

  if Filename = '>' then
    CmdLine := Params
  else begin
    if (Filename = '') or (Filename[1] <> '"') then
      CmdLine := '"' + Filename + '"'
    else
      CmdLine := Filename;
    if Params <> '' then
      CmdLine := CmdLine + ' ' + Params;
    if SameText(PathExtractExt(Filename), '.bat') or
       SameText(PathExtractExt(Filename), '.cmd') then begin
      { See InstExec for explanation }
      CmdLine := '"' + AddBackslash(GetSystemDir) + 'cmd.exe" /C "' + CmdLine + '"'
    end;
    if WorkingDir = '' then
      WorkingDir := PathExtractDir(Filename);
  end;

  FillChar (StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := ShowCmd;
  if WorkingDir <> '' then
    WorkingDirP := PChar(WorkingDir)
  else
    WorkingDirP := nil;

  var InheritHandles := False;
  var dwCreationFlags: DWORD := CREATE_DEFAULT_ERROR_MODE;

  if (OutputReader <> nil) and WaitUntilTerminated then begin
    OutputReader.UpdateStartupInfo(StartupInfo);
    InheritHandles := True;
    dwCreationFlags := dwCreationFlags or CREATE_NO_WINDOW;
  end;

  Result := CreateProcess(nil, PChar(CmdLine), nil, nil, InheritHandles,
    dwCreationFlags, nil, WorkingDirP, StartupInfo, ProcessInfo);
  if not Result then begin
    ResultCode := GetLastError;
    Exit;
  end;

  { Don't need the thread handle, so close it now }
  CloseHandle(ProcessInfo.hThread);
  if OutputReader <> nil then
    OutputReader.NotifyCreateProcessDone;

  try
    if WaitUntilTerminated then begin
      while True do begin
        case WaitForSingleObject(ProcessInfo.hProcess, 50) of
          WAIT_OBJECT_0: Break;
          WAIT_TIMEOUT:
            begin
              if OutputReader <> nil then
                OutputReader.Read(False);
              Preprocessor.CallIdleProc; { Doesn't allow an Abort }
            end;
        else
          Preprocessor.RaiseError('Exec: WaitForSingleObject failed');
        end;
      end;
      if OutputReader <> nil then
        OutputReader.Read(True);
    end;
    { Get the exit code. Will be set to STILL_ACTIVE if not yet available }
    if not GetExitCodeProcess(ProcessInfo.hProcess, DWORD(ResultCode)) then
      ResultCode := -1;  { just in case }
  finally
    CloseHandle(ProcessInfo.hProcess);
  end;
end;

procedure ExecLog(const S: String; const Error, FirstLine: Boolean; const Data: NativeInt);
begin
  var Preprocessor := TPreprocessor(Data);
  if Error then
    Preprocessor.WarningMsg(S)
  else
    Preprocessor.StatusMsg('Exec output: %s', [S]);
end;

{
  int Exec(str FileName, str Params, str WorkingDir, int Wait, int ShowCmd, int Log)
}

function ExecFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr, evStr, evStr, evInt, evInt, evInt], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      var ParamsS, WorkingDir: String;
      var WaitUntilTerminated := True;
      var ShowCmd := SW_SHOWNORMAL;
      if GetCount > 1 then ParamsS := Get(1).AsStr;
      if GetCount > 2 then WorkingDir := PrependPath(Ext, Get(2).AsStr);
      if (GetCount > 3) and (Get(3).Typ <> evNull) then WaitUntilTerminated := Get(3).AsInt <> 0;
      if (GetCount > 4) and (Get(4).Typ <> evNull) then ShowCmd := Get(4).AsInt;
      var Preprocessor := TPreprocessor(Ext);
      var ResultCode: Integer;
      var OutputReader := TCreateProcessOutputReader.Create(ExecLog, NativeInt(Preprocessor));
      try
        var Success := Exec(Get(0).AsStr, ParamsS, WorkingDir, WaitUntilTerminated,
          ShowCmd, Preprocessor, OutputReader, ResultCode);
        if not WaitUntilTerminated then
          MakeBool(ResPtr^, Success)
        else
          MakeInt(ResPtr^, ResultCode);
      finally
        OutputReader.Free;
      end;
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

type
  PExecAndGetFirstLineLogData = ^TExecAndGetFirstLineLogData;
  TExecAndGetFirstLineLogData = record
    Preprocessor: TPreprocessor;
    Line: String;
  end;

procedure ExecAndGetFirstLineLog(const S: String; const Error, FirstLine: Boolean; const Data: NativeInt);
begin
  var Data2 := PExecAndGetFirstLineLogData(Data);
  if not Error and (Data2.Line = '') and (S.Trim <> '') then
    Data2.Line := S;
  ExecLog(S, Error, FirstLine, NativeInt(Data2.Preprocessor));
end;

{
  str ExecAndGetFirstLine(str FileName, str Params, str WorkingDir,)
}

function ExecAndGetFirstLineFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr, evStr, evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      var ParamsS, WorkingDir: String;
      if GetCount > 1 then ParamsS := Get(1).AsStr;
      if GetCount > 2 then WorkingDir := PrependPath(Ext, Get(2).AsStr);
      var Data: TExecAndGetFirstLineLogData;
      Data.Preprocessor := TPreprocessor(Ext);
      Data.Line := '';
      var ResultCode: Integer;
      var OutputReader := TCreateProcessOutputReader.Create(ExecAndGetFirstLineLog, NativeInt(@Data));
      try
        var Success := Exec(Get(0).AsStr, ParamsS, WorkingDir, True,
          SW_SHOWNORMAL, Data.Preprocessor, OutputReader, ResultCode);
        if Success then
          MakeStr(ResPtr^, Data.Line)
        else begin
          Data.Preprocessor.WarningMsg('CreateProcess failed (%d).', [ResultCode]);
          ResPtr^.Typ := evNull;
        end;
      finally
        OutputReader.Free;
      end;
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function LenFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
      MakeInt(ResPtr^, Length(Get(0).AsStr));
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function CopyFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  S: string;
  B, C: Int64;
begin
  if CheckParams(Params, [evStr, evInt, evInt], 2, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      S := Get(0).AsStr;
      B := Get(1).AsInt;
      if GetCount > 2 then C := Get(2).AsInt else C := MaxInt;

      { Constrain 64-bit arguments to 32 bits without truncating them }
      if B < 1 then
        B := 1;
      if C > Maxint then
        C := Maxint;
      if (B > Maxint) or (C < 0) then begin
        { Result should be empty in these cases }
        B := 1;
        C := 0;
      end;

      MakeStr(ResPtr^, Copy(S, Integer(B), Integer(C)));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function PosFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr, evStr], 2, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      MakeInt(ResPtr^, Pos(Get(0).AsStr, Get(1).AsStr));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function LowerCaseFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
      MakeStr(ResPtr^, LowerCase(Get(0).AsStr));
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function UpperCaseFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
      MakeStr(ResPtr^, UpperCase(Get(0).AsStr));
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function RPosFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;

  function RPos(const Substr, S: string): Integer;
  begin
    for Result := Length(S) - Length(Substr) + 1 downto 1 do
      if Copy(S, Result, Length(Substr)) = Substr then
        Exit;
    Result := 0;
  end;

begin
  if CheckParams(Params, [evStr, evStr], 2, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      MakeInt(ResPtr^, RPos(Get(0).AsStr, Get(1).AsStr));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetVersionNumbersStringFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  Filename: string;
  VersionHandle: Cardinal;
  SIZE: Cardinal;
  S: UINT;
  Buf: Pointer;
  FI: PVSFixedFileInfo;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      ResPtr^.Typ := evNull;
      Filename := PrependPath(Ext, Get(0).AsStr);
      Size := GetFileVersionInfoSize(PChar(Filename), VersionHandle);
      if Size > 0 then
      begin
        GetMem(Buf, Size);
        try
          GetFileVersionInfo(PChar(Filename), VersionHandle, Size, Buf);
          if VerQueryValue(Buf, '\', Pointer(FI), S) then
          begin
            MakeStr(ResPtr^,
              IntToStr((FI.dwFileVersionMS and $FFFF0000) shr 16) + '.' +
              IntToStr(FI.dwFileVersionMS and $FFFF) + '.' +
              IntToStr((FI.dwFileVersionLS and $FFFF0000) shr 16) + '.' +
              IntToStr(FI.dwFileVersionLS and $FFFF)
              );
          end;
        finally
          FreeMem(Buf)
        end;
      end
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function ComparePackedVersionFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evInt, evInt], 2, Result) then
  try
    with IInternalFuncParams(Params) do
      MakeInt(ResPtr^, Compare64(Integer64(Get(0).AsInt), Integer64(Get(1).AsInt)));
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function SamePackedVersionFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evInt, evInt], 2, Result) then
  try
    with IInternalFuncParams(Params) do
      if Compare64(Integer64(Get(0).AsInt), Integer64(Get(1).AsInt)) = 0 then
        MakeInt(ResPtr^, 1)
      else
        MakeInt(ResPtr^, 0)
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

{str GetStringFileInfo(str FileName, str StringName, int Lang)}
function GetFileVersionInfoItem(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  Buf: Pointer;

  function GetStringFileInfo(Lang: UINT; const Name: string; var Value: string): Boolean;
  var
    InfoBuf: Pointer;
    InfoBufSize: Longword;
  begin
    Result := VerQueryValue(Buf, PChar('\StringFileInfo\' + IntToHex(LoWord(Lang), 4) +
      IntToHex(HiWord(Lang), 4) +
      '\' + Name), InfoBuf, InfoBufSize) and (InfoBufSize > 0);
    if Result then SetString(Value, PChar(InfoBuf), InfoBufSize - 1)
  end;

type
  TUINTArray = array[0..$100] of UINT;
  PUINTArray = ^TUINTArray;
var
  Filename: string;
  VersionHandle: Cardinal;
  Size: Integer;
  Langs: PUINTArray;
  LangCount, I: Integer;
  Lang, LangsSize: UINT;
  Value: string;
  Success: Boolean;
begin
  if CheckParams(Params, [evStr, evStr, evInt], 2, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      Success := False;
      ResPtr^.Typ := evNull;
      Filename := PrependPath(Ext, Get(0).AsStr);
      Size := GetFileVersionInfoSize(PChar(Filename), VersionHandle);
      if Size > 0 then
      begin
        GetMem(Buf, Size);
        try
          GetFileVersionInfo(PChar(Filename), VersionHandle, Size, Buf);
          if GetCount > 2 then
          begin
            Lang := Get(2).AsInt;
            Success := GetStringFileInfo(Lang, Get(1).AsStr, Value);
          end
          else
          begin
            if VerQueryValue(Buf, PChar('\VarFileInfo\Translation'), Pointer(Langs),
              LangsSize) then
            begin
              LangCount := LangsSize div 4;
              for I := 0 to LangCount - 1 do
              begin
                Success := GetStringFileInfo(Langs[I], Get(1).AsStr, Value);
                if Success then Break;
              end;
            end;
          end;
          if Success then
            MakeStr(ResPtr^, Value);
        finally
          FreeMem(Buf)
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function DelFileFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      QueueFileForDeletion(PrependPath(Ext, Get(0).AsStr));
      ResPtr^.Typ := evNull;
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function DelFileNowFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      DeleteFile(PChar(PrependPath(Ext, Get(0).AsStr)));
      ResPtr^.Typ := evNull;
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function CopyFileFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr, evStr], 2, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      CopyFile(PChar(PrependPath(Ext, Get(0).AsStr)), PChar(PrependPath(Ext, Get(1).AsStr)), False);
      ResPtr^.Typ := evNull;
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

type
  PSearchRec = ^TSearchRec;

procedure GarbageCloseFind(Item: Pointer);
begin
  FindClose(PSearchRec(Item)^);
  Dispose(Item);
end;

function FindFirstFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  Filename: string;
  F: PSearchRec;
begin
  if CheckParams(Params, [evStr, evInt], 2, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      Filename := PrependPath(Ext, Get(0).AsStr);
      New(F);
      ResPtr^.Typ := evInt;
      if FindFirst(Filename, Get(1).AsInt, F^) = 0 then
      begin
        ResPtr^.AsInt := Integer(F);
        TPreprocessor(Ext).CollectGarbage(F, @GarbageCloseFind);
      end
      else
      begin
        ResPtr^.AsInt := 0;
        Dispose(F);
      end;
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function FindNextFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evInt], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      ResPtr.Typ := evInt;
      if FindNext(PSearchRec(Get(0).AsInt)^) = 0 then
        ResPtr^.AsInt := 1
      else
        ResPtr^.AsInt := 0;
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function FindGetFileName(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evInt], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      MakeStr(ResPtr^, PSearchRec(Get(0).AsInt)^.Name);
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function FindCloseFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evInt], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      FindClose(PSearchRec(Get(0).AsInt)^);
      Dispose(PSearchRec(Get(0).AsInt));
      TPreprocessor(Ext).UncollectGarbage(Pointer(Get(0).AsInt));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

procedure GarbageCloseFile(Item: Pointer);
var
  F: ^TextFile;
begin
  F := Item;
  Close(F^);
  Dispose(F);
end;

function FileOpenFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  F: ^TextFile;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    New(F);
    try
      with IInternalFuncParams(Params) do
      begin
        FileMode := fmOpenRead or fmShareDenyWrite;
        AssignFile(F^, PrependPath(Ext, Get(0).AsStr));
        {$I-}
        Reset(F^);
        {$I+}
        if IOResult <> 0 then
        begin
          Dispose(F);
          MakeInt(ResPtr^, 0)
        end
        else
        begin
          MakeInt(ResPtr^, Integer(F));
          TPreprocessor(Ext).CollectGarbage(F, @GarbageCloseFile);
        end;
      end;
    except
      Dispose(F);
      raise
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function FileReadFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  F: ^TextFile;
  S: string;
begin
  if CheckParams(Params, [evInt], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      Integer(F) := Get(0).AsInt;
      if Integer(F) = 0 then
        raise Exception.Create('Invalid file handle');
      {$I-}
      Readln(F^, S);
      {$I+}
      if IOResult <> 0 then
        ResPtr^ := NULL
      else
        MakeStr(ResPtr^, S);
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function FileResetFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  F: ^TextFile;
begin
  if CheckParams(Params, [evInt], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      Integer(F) := Get(0).AsInt;
      if Integer(F) = 0 then
        raise Exception.Create('Invalid file handle');
      {$I-}
      Reset(F^);
      {$I+}
      if IOResult <> 0 then
        raise Exception.Create('Failed to reset a file')
      else
        ResPtr^ := NULL
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function FileEofFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  F: ^TextFile;
  IsEof: Boolean;
begin
  if CheckParams(Params, [evInt], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      Integer(F) := Get(0).AsInt;
      if Integer(F) = 0 then
        raise Exception.Create('Invalid file handle');
      {$I-}
      IsEof := Eof(F^);
      {$I+}
      if IOResult <> 0 then
        ResPtr^ := NULL
      else
        MakeBool(ResPtr^, IsEof);
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function FileCloseFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  F: ^TextFile;
begin
  if CheckParams(Params, [evInt], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      Integer(F) := Get(0).AsInt;
      if Integer(F) = 0 then
        raise Exception.Create('Invalid file handle');
      {$I-}
      Close(F^);
      {$I+}
      ResPtr^ := NULL;
      Dispose(F);
      TPreprocessor(Ext).UncollectGarbage(Pointer(F));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function SaveStringToFileFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  Filename: String;
  F: TextFile;
  DoAppend: Boolean;
  CodePage: Word;
begin
  if CheckParams(Params, [evStr, evStr, evInt, evInt], 2, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      Filename := PrependPath(Ext, Get(0).AsStr);
      if (GetCount < 3) or (Get(2).AsInt <> 0) then DoAppend := True else DoAppend := False;
      if (GetCount < 4) or (Get(3).AsInt <> 0) then CodePage := CP_UTF8 else CodePage := 0;
      DoAppend := DoAppend and NewFileExists(Filename);
      AssignFile(F, FileName, CodePage);
      {$I-}
      if DoAppend then
        Append(F)
      else
        Rewrite(F);
      {$I+}
      if IOResult <> 0 then
        MakeInt(ResPtr^, 0)
      else begin
        try
          MakeInt(ResPtr^, 1);
          if not DoAppend and (CodePage = CP_UTF8) then
            Write(F, #$FEFF); //Strings are UTF-16 so this UTF-16 BOM will actually be saved as an UTF-8 BOM
          Write(F, Get(1).AsStr);
        finally
          CloseFile(F);
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

type
  PDateTime = ^TDateTime;

procedure GarbageReleaseDateTime(Item: Pointer);
begin
  Dispose(Item);
end;

function FileGetDate(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  FileDate: PDateTime;
  Age: TDateTime;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      if FileAge(PrependPath(Ext, Get(0).AsStr), Age) then
      begin
        New(FileDate);
        FileDate^ := Age;
        TPreprocessor(Ext).CollectGarbage(FileDate, GarbageReleaseDateTime);
        MakeInt(ResPtr^, Int64(FileDate));
      end
      else
        MakeInt(ResPtr^, -1);
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetNow(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  DateTime: PDateTime;
begin
  if CheckParams(Params, [], 0, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      New(DateTime);
      DateTime^ := Now;
      TPreprocessor(Ext).CollectGarbage(DateTime, GarbageReleaseDateTime);
      MakeInt(ResPtr^, Int64(DateTime));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetDateFromDT(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evInt], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      MakeInt(ResPtr^, DateTimeToTimeStamp(PDateTime(Get(0).AsInt)^).Date);
    end;
  except
    on E: EAccessViolation do
    begin
      FuncResult.Error('Invalid datetime value');
      Result.Error := ISPPFUNC_FAIL;
    end;
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetTimeFromDT(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evInt], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      MakeInt(ResPtr^, DateTimeToTimeStamp(PDateTime(Get(0).AsInt)^).Time);
    end;
  except
    on E: EAccessViolation do
    begin
      FuncResult.Error('Invalid datetime value');
      Result.Error := ISPPFUNC_FAIL;
    end;
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetDateTimeString(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  NewDateSeparatorString, NewTimeSeparatorString: String;
  OldDateSeparator, OldTimeSeparator: Char;
begin
  if CheckParams(Params, [evStr, evStr, evStr], 3, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      OldDateSeparator := FormatSettings.DateSeparator;
      OldTimeSeparator := FormatSettings.TimeSeparator;
      try
        NewDateSeparatorString := Get(1).AsStr;
        NewTimeSeparatorString := Get(2).AsStr;
        if NewDateSeparatorString <> '' then
          FormatSettings.DateSeparator := NewDateSeparatorString[1];
        if NewTimeSeparatorString <> '' then
          FormatSettings.TimeSeparator := NewTimeSeparatorString[1];
        MakeStr(ResPtr^, FormatDateTime(Get(0).AsStr, Now()));
      finally
        FormatSettings.TimeSeparator := OldTimeSeparator;
        FormatSettings.DateSeparator := OldDateSeparator;
      end;
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetFileDateTimeString(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  NewDateSeparatorString, NewTimeSeparatorString: String;
  OldDateSeparator, OldTimeSeparator: Char;
  Age: TDateTime;
begin
  if CheckParams(Params, [evStr, evStr, evStr, evStr], 4, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      OldDateSeparator := FormatSettings.DateSeparator;
      OldTimeSeparator := FormatSettings.TimeSeparator;
      try
        NewDateSeparatorString := Get(2).AsStr;
        NewTimeSeparatorString := Get(3).AsStr;
        if NewDateSeparatorString <> '' then
          FormatSettings.DateSeparator := NewDateSeparatorString[1];
        if NewTimeSeparatorString <> '' then
          FormatSettings.TimeSeparator := NewTimeSeparatorString[1];
        if not FileAge(PrependPath(Ext, Get(0).AsStr), Age) then begin
          FuncResult.Error('Invalid file name');
          Result.Error := ISPPFUNC_FAIL
        end else
          MakeStr(ResPtr^, FormatDateTime(Get(1).AsStr, Age));
      finally
        FormatSettings.TimeSeparator := OldTimeSeparator;
        FormatSettings.DateSeparator := OldDateSeparator;
      end;
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetMD5OfFile(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  Buf: array[0..65535] of Byte;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      var Context: TMD5Context;
      MD5Init(Context);
      var F := TFile.Create(PrependPath(Ext, Get(0).AsStr), fdOpenExisting, faRead, fsReadWrite);
      try
        while True do begin
          var NumRead := F.Read(Buf, SizeOf(Buf));
          if NumRead = 0 then
            Break;
          MD5Update(Context, Buf, NumRead);
        end;
      finally
        F.Free;
      end;
      MakeStr(ResPtr^, MD5DigestToString(MD5Final(Context)));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetMD5OfString(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      var S := AnsiString(Get(0).AsStr);
      MakeStr(ResPtr^, MD5DigestToString(MD5Buf(Pointer(S)^, Length(S)*SizeOf(S[1]))));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetMD5OfUnicodeString(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      var S := Get(0).AsStr;
      MakeStr(ResPtr^, MD5DigestToString(MD5Buf(Pointer(S)^, Length(S)*SizeOf(S[1]))));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetSHA1OfFile(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  Buf: array[0..65535] of Byte;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      var Context: TSHA1Context;
      SHA1Init(Context);
      var F := TFile.Create(PrependPath(Ext, Get(0).AsStr), fdOpenExisting, faRead, fsReadWrite);
      try
        while True do begin
          var NumRead := F.Read(Buf, SizeOf(Buf));
          if NumRead = 0 then
            Break;
          SHA1Update(Context, Buf, NumRead);
        end;
      finally
        F.Free;
      end;
      MakeStr(ResPtr^, SHA1DigestToString(SHA1Final(Context)));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetSHA1OfString(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      var S := AnsiString(Get(0).AsStr);
      MakeStr(ResPtr^, SHA1DigestToString(SHA1Buf(Pointer(S)^, Length(S)*SizeOf(S[1]))));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetSHA1OfUnicodeString(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      var S := Get(0).AsStr;
      MakeStr(ResPtr^, SHA1DigestToString(SHA1Buf(Pointer(S)^, Length(S)*SizeOf(S[1]))));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetSHA256OfFile(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  Buf: array[0..65535] of Byte;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      var Context: TSHA256Context;
      SHA256Init(Context);
      var F := TFile.Create(PrependPath(Ext, Get(0).AsStr), fdOpenExisting, faRead, fsReadWrite);
      try
        while True do begin
          var NumRead := F.Read(Buf, SizeOf(Buf));
          if NumRead = 0 then
            Break;
          SHA256Update(Context, Buf, NumRead);
        end;
      finally
        F.Free;
      end;
      MakeStr(ResPtr^, SHA256DigestToString(SHA256Final(Context)));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetSHA256OfString(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      var S := AnsiString(Get(0).AsStr);
      MakeStr(ResPtr^, SHA256DigestToString(SHA256Buf(Pointer(S)^, Length(S)*SizeOf(S[1]))));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function GetSHA256OfUnicodeString(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      var S := Get(0).AsStr;
      MakeStr(ResPtr^, SHA256DigestToString(SHA256Buf(Pointer(S)^, Length(S)*SizeOf(S[1]))));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function TrimFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
      MakeStr(ResPtr^, Trim(Get(0).AsStr));
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function StringChangeFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  S: String;
begin
  if CheckParams(Params, [evStr, evStr, evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      S := Get(0).AsStr;
      StringChangeEx(S, Get(1).AsStr, Get(2).AsStr, True);
      MakeStr(ResPtr^, S);
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function IsWin64Func(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [], 0, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      MakeBool(ResPtr^, IsWin64);
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function MessageFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do begin
      { Also see Pragma in IsppPreprocessor }
      TPreprocessor(Ext).StatusMsg(Get(0).AsStr);
      ResPtr^ := NULL;
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function WarningFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do begin
      { Also see Pragma in IsppPreprocessor }
      TPreprocessor(Ext).WarningMsg(Get(0).AsStr);
      ResPtr^ := NULL;
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

function ErrorFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
var
  CatchException: Boolean;
  ErrorMsg: String;
begin
  CatchException := True;
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do begin
      { Also see Pragma and pcErrorDir in IsppPreprocessor }
      ErrorMsg := Get(0).AsStr;
      if ErrorMsg = '' then ErrorMsg := 'Error';
      CatchException := False;
      TPreprocessor(Ext).RaiseError(ErrorMsg);
    end;
  except
    on E: Exception do
    begin
      if CatchException then begin
        FuncResult.Error(PChar(E.Message));
        Result.Error := ISPPFUNC_FAIL
      end else
        raise;
    end;
  end;
end;

function AddQuotesFunc(Ext: Longint; const Params: IIsppFuncParams;
  const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;
begin
  if CheckParams(Params, [evStr], 1, Result) then
  try
    with IInternalFuncParams(Params) do
    begin
      MakeStr(ResPtr^, AddQuotes(Get(0).AsStr));
    end;
  except
    on E: Exception do
    begin
      FuncResult.Error(PChar(E.Message));
      Result.Error := ISPPFUNC_FAIL
    end;
  end;
end;

procedure RegisterFunctions(Preproc: TPreprocessor);
begin
  with Preproc do
  begin
    { -1 as Ext parameter means that function will be called with Ext set to
      preprocessor instance instead of -1. }
    RegisterFunction('Int', Int, -1);
    RegisterFunction('Str', Str, -1);
    RegisterFunction('FileExists', FileExists, -1);
    RegisterFunction('DirExists', DirExists, -1);
    RegisterFunction('ForceDirectories', ForceDirectoriesFunc, -1);
    RegisterFunction('FileSize', FileSize, -1);
    RegisterFunction('ReadIni', ReadIni, -1);
    RegisterFunction('WriteIni', WriteIni, -1);
    RegisterFunction('ReadReg', ReadReg, -1);
    RegisterFunction('Exec', ExecFunc, -1);
    RegisterFunction('ExecAndGetFirstLine', ExecAndGetFirstLineFunc, -1);
    RegisterFunction('Copy', CopyFunc, -1);
    RegisterFunction('Pos', PosFunc, -1);
    RegisterFunction('RPos', RPosFunc, -1);
    RegisterFunction('Len', LenFunc, -1);
    RegisterFunction('GetVersionNumbersString', GetVersionNumbersStringFunc, -1);
    RegisterFunction('ComparePackedVersion', ComparePackedVersionFunc, -1);
    RegisterFunction('SamePackedVersion', SamePackedVersionFunc, -1);
    RegisterFunction('GetStringFileInfo', GetFileVersionInfoItem, -1);
    RegisterFunction('SaveToFile', ISPP.Funcs.SaveToFile, -1);
    RegisterFunction('Find', FindLine, -1);
    RegisterFunction('SetupSetting', SetupSetting, -1);
    RegisterFunction('SetSetupSetting', SetSetupSetting, -1);
    RegisterFunction('LowerCase', LowerCaseFunc, -1);
    RegisterFunction('UpperCase', UpperCaseFunc, -1);
    RegisterFunction('EntryCount', EntryCountFunc, -1);
    RegisterFunction('GetEnv', GetEnvFunc, -1);
    RegisterFunction('DeleteFile', DelFileFunc, -1);
    RegisterFunction('DeleteFileNow', DelFileNowFunc, -1);
    RegisterFunction('CopyFile', CopyFileFunc, -1);
    RegisterFunction('ReadEnv', GetEnvFunc, -1);
    RegisterFunction('FindFirst', FindFirstFunc, -1);
    RegisterFunction('FindNext', FindNextFunc, -1);
    RegisterFunction('FindGetFileName', FindGetFileName, -1);
    RegisterFunction('FindClose', FindCloseFunc, -1);
    RegisterFunction('FileOpen', FileOpenFunc, -1);
    RegisterFunction('FileRead', FileReadFunc, -1);
    RegisterFunction('FileReset', FileResetFunc, -1);
    RegisterFunction('FileEof', FileEofFunc, -1);
    RegisterFunction('FileClose', FileCloseFunc, -1);
    RegisterFunction('SaveStringToFile', SaveStringToFileFunc, -1);
    RegisterFunction('FileGetDateTime', FileGetDate, -1);
    RegisterFunction('Now', GetNow, -1);
    RegisterFunction('DateTimeToDate', GetDateFromDT, -1);
    RegisterFunction('DateTimeToTime', GetTimeFromDT, -1);
    RegisterFunction('GetDateTimeString', GetDateTimeString, -1);
    RegisterFunction('GetFileDateTimeString', GetFileDateTimeString, -1);
    RegisterFunction('GetMD5OfFile', GetMD5OfFile, -1);
    RegisterFunction('GetMD5OfString', GetMD5OfString, -1);
    RegisterFunction('GetMD5OfUnicodeString', GetMD5OfUnicodeString, -1);
    RegisterFunction('GetSHA1OfFile', GetSHA1OfFile, -1);
    RegisterFunction('GetSHA1OfString', GetSHA1OfString, -1);
    RegisterFunction('GetSHA1OfUnicodeString', GetSHA1OfUnicodeString, -1);
    RegisterFunction('GetSHA256OfFile', GetSHA256OfFile, -1);
    RegisterFunction('GetSHA256OfString', GetSHA256OfString, -1);
    RegisterFunction('GetSHA256OfUnicodeString', GetSHA256OfUnicodeString, -1);
    RegisterFunction('Trim', TrimFunc, -1);
    RegisterFunction('StringChange', StringChangeFunc, -1);
    RegisterFunction('IsWin64', IsWin64Func, -1);
    RegisterFunction('Message', MessageFunc, -1);
    RegisterFunction('Warning', WarningFunc, -1);
    RegisterFunction('Error', ErrorFunc, -1);
    RegisterFunction('AddQuotes', AddQuotesFunc, -1)
  end;
end;

procedure InitIsWin64;
var
  IsWow64ProcessFunc: function(hProcess: THandle; var Wow64Process: BOOL): BOOL; stdcall;
  Wow64Process: BOOL;
begin
  IsWow64ProcessFunc := GetProcAddress(GetModuleHandle(kernel32), 'IsWow64Process');
  IsWin64 := Assigned(IsWow64ProcessFunc) and
             IsWow64ProcessFunc(GetCurrentProcess, Wow64Process) and
             Wow64Process;
end;

initialization
  InitIsWin64;

end.

