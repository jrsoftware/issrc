unit Setup.ScriptRunner;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Script runner
}

interface

uses
  uPSRuntime, uPSDebugger, uPSUtils;

type
  TScriptRunnerOnLog = procedure(const S: String);
  TScriptRunnerOnLogFmt = procedure(const S: String; const Args: array of const);
  TScriptRunnerOnDllImport = procedure(var DllName: String; var ForceDelayLoad: Boolean);
  TScriptRunnerOnDebug = function(const Position: LongInt; var ContinueStepOver: Boolean): Boolean;
  TScriptRunnerOnDebugIntermediate = function(const Position: LongInt; var ContinueStepOver: Boolean): Boolean;
  TScriptRunnerOnException = procedure(const Exception: AnsiString; const Position: LongInt);

  TBreakCondition = (bcNone, bcTrue, bcFalse, bcNonZero, bcNonEmpty);

  TScriptRunner = class
    private
      FNamingAttribute: String;
      FPSExec: TPSDebugExec;
      FClassImporter: TPSRuntimeClassImporter;
      FOnLog: TScriptRunnerOnLog;
      FOnLogFmt: TScriptRunnerOnLogFmt;
      FOnDllImport: TScriptRunnerOnDllImport;
      FOnDebug: TScriptRunnerOnDebug;
      FOnDebugIntermediate: TScriptRunnerOnDebugIntermediate;
      FOnException: TScriptRunnerOnException;
      function GetProcNos(const Name: AnsiString; const CheckNamingAttribute: Boolean; const ProcNos: TPSList): Integer;
      procedure InternalRunProcedure(const Name: AnsiString; const Parameters: array of Const; const CheckNamingAttribute, MustExist: Boolean);
      function InternalRunBooleanFunction(const Name: AnsiString; const Parameters: array of Const; const CheckNamingAttribute: Boolean; const BreakCondition: TBreakCondition; const MustExist, Default: Boolean): Boolean;
      function InternalRunIntegerFunction(const Name: AnsiString; const Parameters: array of Const; const CheckNamingAttribute: Boolean; const BreakCondition: TBreakCondition; const MustExist: Boolean; const Default: Integer): Integer;
      function InternalRunStringFunction(const Name: AnsiString; const Parameters: array of Const; const CheckNamingAttribute: Boolean; const BreakCondition: TBreakCondition; const MustExist: Boolean; const Default: String): String;
      procedure Log(const S: String);
      procedure LogFmt(const S: String; const Args: array of const);
      procedure RaisePSExecException;
      procedure SetPSExecParameters(const Parameters: array of Const; Params: TPSList);
      procedure SetPSExecReturnValue(Params: TPSList; BaseType: TPSBaseType; var Res: PPSVariant);
    public
      constructor Create;
      destructor Destroy; override;
      procedure LoadScript(const CompiledScriptText, CompiledScriptDebugInfo: AnsiString);
      function FunctionExists(const Name: AnsiString; const CheckNamingAttribute: Boolean): Boolean;
      procedure RunProcedure(const Name: AnsiString; const Parameters: array of Const; const MustExist: Boolean);
      procedure RunProcedures(const Name: AnsiString; const Parameters: array of Const; const MustExist: Boolean);
      function RunBooleanFunction(const Name: AnsiString; const Parameters: array of Const; const MustExist, Default: Boolean): Boolean;
      function RunBooleanFunctions(const Name: AnsiString; const Parameters: array of Const; const BreakCondition: TBreakCondition; const MustExist, Default: Boolean): Boolean;
      function RunIntegerFunction(const Name: AnsiString; const Parameters: array of Const; const MustExist: Boolean; const Default: Integer): Integer;
      function RunIntegerFunctions(const Name: AnsiString; const Parameters: array of Const; const BreakCondition: TBreakCondition; const MustExist: Boolean; Default: Integer): Integer;
      function RunStringFunction(const Name: AnsiString; const Parameters: array of Const; const MustExist: Boolean; const Default: String): String;
      function RunStringFunctions(const Name: AnsiString; const Parameters: array of Const; const BreakCondition: TBreakCondition; const MustExist: Boolean; Default: String): String;
      function EvaluateUsedVariable(const Param1, Param2, Param3: LongInt; const Param4: AnsiString): String;
      function GetCallStack(var CallStackCount: Cardinal): String;
      property NamingAttribute: String write FNamingAttribute;
      property OnLog: TScriptRunnerOnLog read FOnLog write FOnLog;
      property OnLogFmt: TScriptRunnerOnLogFmt read FOnLogFmt write FOnLogFmt;
      property OnDllImport: TScriptRunnerOnDllImport read FOnDllImport write FOnDllImport;
      property OnDebug: TScriptRunnerOnDebug read FOnDebug write FOnDebug;
      property OnDebugIntermediate: TScriptRunnerOnDebugIntermediate read FOnDebugIntermediate write FOnDebugIntermediate;
      property OnException: TScriptRunnerOnException read FOnException write FOnException;
    end;

implementation

uses
  Windows,
  Forms, SysUtils,
  uPSR_dll,
  Setup.ScriptClasses, Setup.ScriptFunc;

{---}

{ Note: Originally this unit used String() casts to avoid "Implicit string
  cast" warnings on Delphi 2009, but the casts were found to cause non-Unicode
  Setup to crash during tooltip variable evaluation due to some kind of code
  generation bug in Delphi 2. Removed all casts, and added the following to
  simply disable the warning. }
{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
{$ENDIF}

procedure TScriptRunner.Log(const S: String);
begin
  if Assigned(FOnLog) then
    FOnLog(S);
end;

procedure TScriptRunner.LogFmt(const S: String; const Args: array of const);
begin
  if Assigned(FOnLogFmt) then
    FOnLogFmt(S, Args);
end;

procedure ShowError(const Error: String);
begin
  raise Exception.Create(Error);
end;

procedure ShowPSExecError(const Error: TPSError);
begin
  ShowError('Script error: ' + PSErrorToString(Error, ''));
end;

procedure TScriptRunner.RaisePSExecException;
var
  E: TObject;
begin
  try
    FPSExec.RaiseCurrentException;
  except
    { Note: Don't use 'on E: Exception do' since that will also match
      'Exception' objects raised from other modules (which we mustn't modify) }
    E := ExceptObject;
    if E is Exception then begin
      Exception(E).Message := Format('Runtime error (at %d:%d):'#13#10#13#10,
        [FPSExec.ExceptionProcNo, FPSExec.ExceptionPos]) + Exception(E).Message;
      raise;
    end
    else begin
      { If we don't see it as an Exception, it was likely raised by another
        module }
      raise Exception.CreateFmt('Runtime error (at %d:%d):'#13#10#13#10 +
        'Exception "%s" at address %p',
        [FPSExec.ExceptionProcNo, FPSExec.ExceptionPos, E.ClassName, ExceptAddr]);
    end;
  end;
end;

procedure TScriptRunner.SetPSExecParameters(const Parameters: array of Const; Params: TPSList);
var
  Param: PPSVariant;
  I: Integer;
begin
  for I := High(Parameters) downto Low(Parameters) do begin
    case Parameters[I].vType of
      vtAnsiString:
        begin
          Param := CreateHeapVariant(FPSExec.FindType2(btString));
          PPSVariantAString(Param).Data := AnsiString(Parameters[I].vAnsiString);
        end;
      vtWideString:
        begin
          Param := CreateHeapVariant(FPSExec.FindType2(btWideString));
          PPSVariantWString(Param).Data := WideString(Parameters[I].VWideString);
        end;
      vtUnicodeString:
        begin
          Param := CreateHeapVariant(FPSExec.FindType2(btUnicodeString));
          PPSVariantUString(Param).Data := UnicodeString(Parameters[I].VUnicodeString);
        end;
      vtInteger:
        begin
          Param := CreateHeapVariant(FPSExec.FindType2(btS32));
          PPSVariantS32(Param).Data := Parameters[I].vInteger;
        end;
      vtBoolean:
        begin
          Param := CreateHeapVariant(FPSExec.FindType2(btU8));
          PPSVariantU8(Param).Data := Byte(Parameters[I].vBoolean);
        end;
      vtPointer:
        begin
          { Pointers are assumed to be pointers to Booleans }
          Param := CreateHeapVariant(FPSExec.FindType2(btU8));
          PPSVariantU8(Param).Data := Byte(Boolean(Parameters[I].VPointer^));
        end;
    else
      raise Exception.Create('TScriptRunner.SetPSExecParameters: Invalid type');
    end;
    Params.Add(Param);
  end;
end;

procedure TScriptRunner.SetPSExecReturnValue(Params: TPSList; BaseType: TPSBaseType; var Res: PPSVariant);
begin
  Res := CreateHeapVariant(FPSExec.FindType2(BaseType));
  Params.Add(Res);
end;

{---}

function EncodeDLLFilenameForROPS(const Filename: String): AnsiString;
begin
  Result := '';
  if Filename <> '' then
      Result := AnsiString('<utf8>') + UTF8Encode(Filename);
end;

function NewUnloadDLLProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  UnloadDLL(Caller, EncodeDLLFilenameForROPS(Stack.GetString(-1)));
  Result := True;
end;

function PSExecOnSpecialProcImport(Sender: TPSExec; p: TPSExternalProcRec; Tag: Pointer): Boolean;
const
  SYesNo: array[Boolean] of String = ('No', 'Yes');
var
  ScriptRunner: TScriptRunner;
  S, DllName, FunctionName: AnsiString;
  UnicodeDllName: String;
  I: Integer;
  ForceDelayLoad: Boolean;
  ErrorCode: LongInt;
begin
  ScriptRunner := Sender.ID;
  ForceDelayLoad := False;

  ScriptRunner.Log('-- DLL function import --');

  S := p.Decl;

  I := Pos(AnsiString('dll:'), S);
  if I <> 1 then begin
    Result := False;
    Exit;
  end;
  Delete(S, 1, Length('dll:'));

  I := Pos(AnsiString(#0), S);
  if I = 0 then begin
    Result := False;
    Exit;
  end;
  DllName := Copy(S, 1, I-1);
  Delete(S, 1, I);

  I := Pos(AnsiString(#0), S);
  if I = 0 then begin
    Result := False;
    Exit;
  end;
  FunctionName := Copy(S, 1, I-1);

  UnicodeDllName := UTF8ToString(DllName);
  ScriptRunner.LogFmt('Function and DLL name: %s@%s', [FunctionName, UnicodeDllName]);

  if Assigned(ScriptRunner.FOnDllImport) then begin
    ScriptRunner.FOnDllImport(UnicodeDllName, ForceDelayLoad);
    DllName := EncodeDLLFilenameForROPS(UnicodeDllName);
    p.Decl := AnsiString('dll:') + DllName + Copy(p.Decl, Pos(AnsiString(#0), p.Decl), MaxInt);
  end;

  if DllName <> '' then
    ScriptRunner.LogFmt('Importing the DLL function. Dest DLL name: %s', [UnicodeDllName])
  else
    ScriptRunner.Log('Skipping.'); { We're actually still going to call ProcessDllImport but this doesn't matter to the user. }

  var DelayLoaded: Boolean;
  Result := ProcessDllImportEx2(Sender, p, ForceDelayLoad, DelayLoaded, ErrorCode);

  if DllName <> '' then begin
    if Result then
      ScriptRunner.LogFmt('Successfully imported the DLL function. Delay loaded? %s', [SYesNo[DelayLoaded]])
    else
      ScriptRunner.LogFmt('Failed to import the DLL function (%d).', [ErrorCode]);
  end;
end;

procedure PSExecOnSourceLine(Sender: TPSDebugExec; const Name: AnsiString; Position, Row, Col: Cardinal);
var
  ScriptRunner: TScriptRunner;
  ContinueStepOver, NeedToResume: Boolean;
begin
  ScriptRunner := Sender.ID;

  ContinueStepOver := False;
  if Sender.DebugMode = dmPaused then begin
    if Assigned(ScriptRunner.FOnDebug) then
      ScriptRunner.FOnDebug(Position, ContinueStepOver);
    NeedToResume := True;
  end else begin
    { Normally the debugger does not pause when it receives an 'intermediate'
      notification. However, it can happen if the user clicks Step Over and
      then Pause before the function call being stepped over has returned. }
    NeedToResume := False;
    if Assigned(ScriptRunner.FOnDebugIntermediate) then
      NeedToResume := ScriptRunner.FOnDebugIntermediate(Position, ContinueStepOver);
  end;
  if NeedToResume then begin
    if ContinueStepOver then
      Sender.StepOver()
    else
      Sender.StepInto();
  end;
end;

procedure PSExecOnException(Sender: TPSExec; ExError: TPSError; const ExParam: AnsiString; ExObject: TObject; ProcNo, Position: Cardinal);
var
  ScriptRunner: TScriptRunner;
begin
  ScriptRunner := Sender.ID;

  if Assigned(ScriptRunner.FOnException) then
    ScriptRunner.FOnException(PSErrorToString(ExError, ExParam), ScriptRunner.FPSExec.TranslatePosition(ProcNo, Position));

  { Clear any previous 'step over' state after an exception. Like Delphi,
    when F8 is pressed after an exception it should go to the first line of
    the nearest 'except' handler, not to the next line of some higher-level
    function that the user was stepping over prior to the exception. }
  ScriptRunner.FPSExec.StepInto();
end;

{---}

constructor TScriptRunner.Create();
begin
  FPSExec := TPSDebugExec.Create();
  FPSExec.ID := Self;

  FPSExec.AddSpecialProcImport('dll', @PSExecOnSpecialProcImport, nil);
  FPSExec.OnSourceLine := PSExecOnSourceLine;
  FPSExec.OnException := PSExecOnException;

  RegisterDLLRuntimeEx(FPSExec, False, False);
  FPSExec.RegisterFunctionName('UNLOADDLL', NewUnloadDLLProc, nil, nil);
  FClassImporter := ScriptClassesLibraryRegister_R(FPSExec);
  ScriptFuncLibraryRegister_R(FPSExec);
end;

destructor TScriptRunner.Destroy;
begin
  FPSExec.Free();
  FClassImporter.Free();
end;

procedure TScriptRunner.LoadScript(const CompiledScriptText, CompiledScriptDebugInfo: AnsiString);
begin
  if FPSExec.LoadData(CompiledScriptText) then begin
    FPSExec.DebugEnabled := CompiledScriptDebugInfo <> '';
    if FPSExec.DebugEnabled then
      FPSExec.LoadDebugData(CompiledScriptDebugInfo);
    FPSExec.StepInto();
  end else begin
    RaisePSExecException;
    { In the case the above for some reason doesn't raise an exception, raise
      our own: }
    raise Exception.Create('TScriptRunner.LoadScript failed');
  end;
end;

function TScriptRunner.GetProcNos(const Name: AnsiString; const CheckNamingAttribute: Boolean; const ProcNos: TPSList): Integer;
var
  MainProcNo, ProcNo: Cardinal;
  Proc: PIFProcRec;
  Attr: TPSRuntimeAttribute;
begin
  Result := 0;

  { Locate main implementation. Will add later. }
  MainProcNo := FPSExec.GetProc(Name);
  
  { Locate other implementations using attributes. }
  if CheckNamingAttribute and (FNamingAttribute <> '') then begin
    for ProcNo := 0 to FPSExec.GetProcCount-1 do begin
      if ProcNo <> MainProcNo then begin
        Proc := FPSExec.GetProcNo(ProcNo);
        if Proc.Attributes.Count > 0 then begin
          Attr := Proc.Attributes.FindAttribute(AnsiString(FNamingAttribute));
          if (Attr <> nil) and (Attr.ValueCount = 1) and
             (((Attr.Value[0].FType.BaseType = btUnicodeString) and (CompareText(PPSVariantUString(Attr.Value[0]).Data, Name) = 0)) or
              ((Attr.Value[0].FType.BaseType = btString) and (CompareText(PPSVariantAString(Attr.Value[0]).Data, Name) = 0))) then begin
            if ProcNos <> nil then
              ProcNos.Add(Pointer(ProcNo));
            Inc(Result);
          end;
        end;
      end;
    end;
  end;

  { Add main implementation. Doing this last so it will be called last always. }
  if MainProcNo <> Cardinal(-1) then begin
    if ProcNos <> nil then
      ProcNos.Add(Pointer(MainProcNo));
    Inc(Result);
  end;
end;

function TScriptRunner.FunctionExists(const Name: AnsiString; const CheckNamingAttribute: Boolean): Boolean;
begin
  Result := GetProcNos(Name, CheckNamingAttribute, nil) <> 0;
end;

procedure WriteBackParameters(const Parameters: array of Const; const Params: TPSList);
var
  I: Integer;
begin
  { Write back new Boolean values to vtPointer-type parameters }
  for I := 0 to High(Parameters) do
    if Parameters[I].vType = vtPointer then
      Boolean(Parameters[I].VPointer^) := (PPSVariantU8(Params[High(Parameters)-I]).Data = 1);
end;

procedure TScriptRunner.InternalRunProcedure(const Name: AnsiString; const Parameters: array of Const; const CheckNamingAttribute, MustExist: Boolean);
var
  ProcNos, Params: TPSList;
  I: Integer;
begin
  ProcNos := TPSList.Create;
  try
    if GetProcNos(Name, CheckNamingAttribute, ProcNos) <> 0 then begin
      ScriptClassesLibraryUpdateVars(FPSExec);
      for I := 0 to ProcNos.Count-1 do begin
        Params := TPSList.Create();
        try
          SetPSExecParameters(Parameters, Params);
          FPSExec.RunProc(Params, Cardinal(ProcNos[I]));
          WriteBackParameters(Parameters, Params);

          RaisePSExecException;
        finally
          FreePSVariantList(Params);
        end;
      end;
    end else begin
      if MustExist then
        ShowPSExecError(erCouldNotCallProc);
    end;
  finally
    ProcNos.Free;
  end;
end;

procedure TScriptRunner.RunProcedure(const Name: AnsiString; const Parameters: array of Const; const MustExist: Boolean);
begin
  InternalRunProcedure(Name, Parameters, False, MustExist);
end;

procedure TScriptRunner.RunProcedures(const Name: AnsiString; const Parameters: array of Const; const MustExist: Boolean);
begin
  InternalRunProcedure(Name, Parameters, True, MustExist);
end;

function TScriptRunner.InternalRunBooleanFunction(const Name: AnsiString; const Parameters: array of Const; const CheckNamingAttribute: Boolean; const BreakCondition: TBreakCondition; const MustExist, Default: Boolean): Boolean;
var
  ProcNos, Params: TPSList;
  Res: PPSVariant;
  I: Integer;
begin
  ProcNos := TPSList.Create;
  try
    if GetProcNos(Name, CheckNamingAttribute, ProcNos) <> 0 then begin
      if not (BreakCondition in [bcNone, bcTrue, bcFalse]) or
         ((BreakCondition = bcNone) and (ProcNos.Count > 1)) then
        ShowError('Internal error: InternalRunBooleanFunction: invalid BreakCondition');
      Result := True; { Silence compiler }
      ScriptClassesLibraryUpdateVars(FPSExec);
      for I := 0 to ProcNos.Count-1 do begin
        Params := TPSList.Create();
        try
          SetPSExecParameters(Parameters, Params);
          SetPSExecReturnValue(Params, btU8, Res);
          FPSExec.RunProc(Params, Cardinal(ProcNos[I]));
          WriteBackParameters(Parameters, Params);

          RaisePSExecException;
          Result := PPSVariantU8(Res).Data = 1;
          if (Result and (BreakCondition = bcTrue)) or
             (not Result and (BreakCondition = bcFalse)) then
            Exit;
        finally
          FreePSVariantList(Params);
        end;
      end;
    end else begin
      if MustExist then
        ShowPSExecError(erCouldNotCallProc);
      Result := Default;
    end;
  finally
    ProcNos.Free;
  end;
end;

function TScriptRunner.RunBooleanFunction(const Name: AnsiString; const Parameters: array of Const; const MustExist, Default: Boolean): Boolean;
begin
  Result := InternalRunBooleanFunction(Name, Parameters, False, bcNone, MustExist, Default);
end;

function TScriptRunner.RunBooleanFunctions(const Name: AnsiString; const Parameters: array of Const; const BreakCondition: TBreakCondition; const MustExist, Default: Boolean): Boolean;
begin
  Result := InternalRunBooleanFunction(Name, Parameters, True, BreakCondition, MustExist, Default);
end;

function TScriptRunner.InternalRunIntegerFunction(const Name: AnsiString; const Parameters: array of Const; const CheckNamingAttribute: Boolean; const BreakCondition: TBreakCondition; const MustExist: Boolean; const Default: Integer): Integer;
var
  ProcNos, Params: TPSList;
  Res: PPSVariant;
  I: Integer;
begin
  ProcNos := TPSList.Create;
  try
    if GetProcNos(Name, CheckNamingAttribute, ProcNos) <> 0 then begin
      if not (BreakCondition in [bcNone, bcNonZero]) or
         ((BreakCondition = bcNone) and (ProcNos.Count > 1)) then
        ShowError('Internal error: InternalRunIntegerFunction: invalid BreakCondition');
      Result := 0; { Silence compiler }
      ScriptClassesLibraryUpdateVars(FPSExec);
      for I := 0 to ProcNos.Count-1 do begin
        Params := TPSList.Create();
        try
          SetPSExecParameters(Parameters, Params);
          SetPSExecReturnValue(Params, btS32, Res);
          FPSExec.RunProc(Params, Cardinal(ProcNos[I]));
          WriteBackParameters(Parameters, Params);

          RaisePSExecException;
          Result := PPSVariantS32(Res).Data;
          if (Result <> 0) and (BreakCondition = bcNonZero) then
            Exit;
        finally
          FreePSVariantList(Params);
        end;
      end;
    end else begin
      if MustExist then
        ShowPSExecError(erCouldNotCallProc);
      Result := Default;
    end;
  finally
    ProcNos.Free;
  end;
end;

function TScriptRunner.RunIntegerFunction(const Name: AnsiString; const Parameters: array of Const; const MustExist: Boolean; const Default: Integer): Integer;
begin
  Result := InternalRunIntegerFunction(Name, Parameters, False, bcNone, MustExist, Default);
end;

function TScriptRunner.RunIntegerFunctions(const Name: AnsiString; const Parameters: array of Const; const BreakCondition: TBreakCondition; const MustExist: Boolean; Default: Integer): Integer;
begin
  Result := InternalRunIntegerFunction(Name, Parameters, True, BreakCondition, MustExist, Default);
end;

function TScriptRunner.InternalRunStringFunction(const Name: AnsiString; const Parameters: array of Const; const CheckNamingAttribute: Boolean; const BreakCondition: TBreakCondition; const MustExist: Boolean; const Default: String): String;
var
  ProcNos, Params: TPSList;
  Res: PPSVariant;
  I: Integer;
begin
  ProcNos := TPSList.Create;
  try
    if GetProcNos(Name, CheckNamingAttribute, ProcNos) <> 0 then begin
      if not (BreakCondition in [bcNone, bcNonEmpty]) or
         ((BreakCondition = bcNone) and (ProcNos.Count > 1)) then
        ShowError('Internal error: InternalRunStringFunction: invalid BreakCondition');
      Result := ''; { Silence compiler }
      ScriptClassesLibraryUpdateVars(FPSExec);
      for I := 0 to ProcNos.Count-1 do begin
        Params := TPSList.Create();
        try
          SetPSExecParameters(Parameters, Params);
          SetPSExecReturnValue(Params, btUnicodeString, Res);
          FPSExec.RunProc(Params, Cardinal(ProcNos[I]));
          WriteBackParameters(Parameters, Params);
          
          RaisePSExecException;
          Result := PPSVariantUString(Res).Data;
          if (Result <> '') and (BreakCondition = bcNonEmpty) then
            Exit;
        finally
          FreePSVariantList(Params);
        end;
      end;
    end else begin
      if MustExist then
        ShowPSExecError(erCouldNotCallProc);
      Result := Default;
    end;
  finally
    ProcNos.Free;
  end;
end;

function TScriptRunner.RunStringFunction(const Name: AnsiString; const Parameters: array of Const; const MustExist: Boolean; const Default: String): String;
begin
  Result := InternalRunStringFunction(Name, Parameters, False, bcNone, MustExist, Default);
end;

function TScriptRunner.RunStringFunctions(const Name: AnsiString; const Parameters: array of Const; const BreakCondition: TBreakCondition; const MustExist: Boolean; Default: String): String;
begin
  Result := InternalRunStringFunction(Name, Parameters, True, BreakCondition, MustExist, Default);
end;

function TScriptRunner.EvaluateUsedVariable(const Param1, Param2, Param3: LongInt; const Param4: AnsiString): String;

  function VariantToString(const p: TPSVariantIFC; const ClassProperties: AnsiString): String;
  begin
    //PSVariantToString isn't Unicode enabled, handle strings ourselves
    //doesn't handle more complex types as records, arrays and objects
    if p.Dta <> nil then begin
      case p.aType.BaseType of
        btWideChar: Result := '''' + tbtWideChar(p.Dta^) + '''';
        btWideString: Result :=  '''' + tbtWideString(p.Dta^) + '''';
        btUnicodeString: Result :=  '''' + tbtUnicodeString(p.Dta^) + '''';
      else
        Result := PSVariantToString(p, ClassProperties);
      end;
    end else
      Result := PSVariantToString(p, ClassProperties);
  end;

begin
  case TPSVariableType(Param1) of
    ivtGlobal:
      begin
        Result := FPSExec.GlobalVarNames[Param3];
        if Param4 <> '' then
          Result := Result + '.' + Param4;
        Result := Result + ' = ' + VariantToString(NewTPSVariantIFC(FPSExec.GetGlobalVar(Param3), False), Param4);
      end;
    ivtParam:
      begin
        if Param2 = LongInt(FPSExec.GetCurrentProcNo) then begin
          Result := FPSExec.CurrentProcParams[Param3];
          if Param4 <> '' then
            Result := Result + '.' + Param4;
          Result := Result + ' = ' + VariantToString(NewTPSVariantIFC(FPSExec.GetProcParam(Param3), False), Param4);
        end else
          Result := '';
      end;
    ivtVariable:
      begin
        if Param2 = LongInt(FPSExec.GetCurrentProcNo) then begin
          Result := FPSExec.CurrentProcVars[Param3];
          if Param4 <> '' then
            Result := Result + '.' + Param4;
          Result := Result + ' = ' + VariantToString(NewTPSVariantIFC(FPSExec.GetProcVar(Param3), False), Param4);
        end else
          Result := '';
      end;
  end;
end;

function TScriptRunner.GetCallStack(var CallStackCount: Cardinal): String;
begin
  Result := FPSExec.GetCallStack(CallStackCount);
end;

end.

