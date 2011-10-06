unit ScriptRunner;

{
  Inno Setup
  Copyright (C) 1997-2011 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Script runner

  $jrsoftware: issrc/Projects/ScriptRunner.pas,v 1.34 2011/01/11 05:30:39 jr Exp $
}

interface

uses
  uPSRuntime, uPSDebugger, uPSUtils;

type
  TScriptRunnerOnDllImport = procedure(var DllName: String; var ForceDelayLoad: Boolean);
  TScriptRunnerOnDebug = function(const Position: LongInt; var ContinueStepOver: Boolean): Boolean;
  TScriptRunnerOnDebugIntermediate = function(const Position: LongInt; var ContinueStepOver: Boolean): Boolean;
  TScriptRunnerOnException = procedure(const Exception: AnsiString; const Position: LongInt);

  TScriptRunner = class
    private
      FPSExec: TPSDebugExec;
      FClassImporter: TPSRuntimeClassImporter;
      FOnDllImport: TScriptRunnerOnDllImport;
      FOnDebug: TScriptRunnerOnDebug;
      FOnDebugIntermediate: TScriptRunnerOnDebugIntermediate;
      FOnException: TScriptRunnerOnException;
      procedure RaisePSExecException;
      procedure SetPSExecParameters(const Parameters: array of Const; Params: TPSList);
      procedure SetPSExecReturnValue(Params: TPSList; BaseType: TPSBaseType; var Res: PPSVariant);
      procedure ShowPSExecError(const Error: TPSError);
    public
      constructor Create;
      destructor Destroy; override;
      procedure LoadScript(const CompiledScriptText, CompiledScriptDebugInfo: AnsiString);
      function FunctionExists(const Name: AnsiString): Boolean;
      procedure RunProcedure(const Name: AnsiString; const Parameters: array of Const; const MustExist: Boolean);
      function RunBooleanFunction(const Name: AnsiString; const Parameters: array of Const; const MustExist, Default: Boolean): Boolean;
      function RunIntegerFunction(const Name: AnsiString; const Parameters: array of Const; const MustExist: Boolean; const Default: Integer): Integer;
      function RunStringFunction(const Name: AnsiString; const Parameters: array of Const; const MustExist: Boolean; const Default: String): String;
      function EvaluateUsedVariable(const Param1, Param2, Param3: LongInt; const Param4: AnsiString): String;
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
  ScriptClasses_R, ScriptFunc_R;

{---}

{ Note: Originally this unit used String() casts to avoid "Implicit string
  cast" warnings on Delphi 2009, but the casts were found to cause non-Unicode
  Setup to crash during tooltip variable evaluation due to some kind of code
  generation bug in Delphi 2. Removed all casts, and added the following to
  simply disable the warning. }
{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
{$ENDIF}

procedure TScriptRunner.ShowPSExecError(const Error: TPSError);
begin
  raise Exception.Create('Script error: ' + PSErrorToString(Error, ''));
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
      Exception(E).Message := Format('Runtime Error (at %d:%d):'#13#10#13#10,
        [FPSExec.ExceptionProcNo, FPSExec.ExceptionPos]) + Exception(E).Message;
      raise;
    end
    else begin
      { If we don't see it as an Exception, it was likely raised by another
        module }
      raise Exception.CreateFmt('Runtime Error (at %d:%d):'#13#10#13#10 +
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
{$IFDEF UNICODE}
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
{$ENDIF}
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

{$IFDEF UNICODE}
function EncodeDLLFilename(const Filename: String): AnsiString;
begin
  Result := '';
  if Filename <> '' then
    Result := AnsiString('<utf8>') + UTF8Encode(Filename);
end;

function NewUnloadDLLProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  UnloadDLL(Caller, EncodeDLLFilename(Stack.GetString(-1)));
  Result := True;
end;
{$ENDIF}

function PSExecOnSpecialProcImport(Sender: TPSExec; p: TPSExternalProcRec; Tag: Pointer): Boolean;
var
  ScriptRunner: TScriptRunner;
  DllName: AnsiString;
{$IFDEF UNICODE}
  UnicodeDllName: String;
{$ENDIF}
  I: Integer;
  ForceDelayLoad: Boolean;
begin
  ScriptRunner := Sender.ID;
  ForceDelayLoad := False;

  if Assigned(ScriptRunner.FOnDllImport) then begin
    DllName := p.Decl;

    I := Pos(AnsiString('dll:'), DllName);
    if I <> 1 then begin
      Result := False;
      Exit;
    end;
    Delete(DllName, 1, Length('dll:'));

    I := Pos(AnsiString(#0), DllName);
    if I = 0 then begin
      Result := False;
      Exit;
    end;
    Delete(DllName, I, MaxInt);

{$IFDEF UNICODE}
    UnicodeDllName := String(DllName);
    ScriptRunner.FOnDllImport(UnicodeDllName, ForceDelayLoad);
    DllName := EncodeDLLFilename(UnicodeDllName);
{$ELSE}
    ScriptRunner.FOnDllImport(DllName, ForceDelayLoad);
{$ENDIF}

    p.Decl := AnsiString('dll:') + DllName + Copy(p.Decl, Pos(AnsiString(#0), p.Decl), MaxInt);
  end;

  Result := ProcessDllImportEx(Sender, p, ForceDelayLoad);
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

{$IFNDEF UNICODE}
  RegisterDLLRuntimeEx(FPSExec, False);
{$ELSE}
  RegisterDLLRuntimeEx(FPSExec, False, False);
  FPSExec.RegisterFunctionName('UNLOADDLL', NewUnloadDLLProc, nil, nil);
{$ENDIF}
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

function TScriptRunner.FunctionExists(const Name: AnsiString): Boolean;
begin
  Result := FPSExec.GetProc(Name) <> Cardinal(-1);
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

procedure TScriptRunner.RunProcedure(const Name: AnsiString; const Parameters: array of Const; const MustExist: Boolean);
var
  ProcNo: Cardinal;
  Params: TPSList;
begin
  ProcNo := FPSExec.GetProc(Name);
  if ProcNo <> Cardinal(-1) then begin
    Params := TPSList.Create();
    try
      SetPSExecParameters(Parameters, Params);
      FPSExec.RunProc(Params, ProcNo);
      WriteBackParameters(Parameters, Params);
    finally
      FreePSVariantList(Params);
    end;

    RaisePSExecException;
  end else begin
    if MustExist then
      ShowPSExecError(erCouldNotCallProc);
  end;
end;

function TScriptRunner.RunBooleanFunction(const Name: AnsiString; const Parameters: array of Const; const MustExist, Default: Boolean): Boolean;
var
  ProcNo: Cardinal;
  Params: TPSList;
  Res: PPSVariant;
begin
  Result := Default;

  ProcNo := FPSExec.GetProc(Name);
  if ProcNo <> Cardinal(-1) then begin
    Params := TPSList.Create();
    try
      SetPSExecParameters(Parameters, Params);
      SetPSExecReturnValue(Params, btU8, Res);
      FPSExec.RunProc(Params, ProcNo);
      WriteBackParameters(Parameters, Params);

      RaisePSExecException;
      Result := PPSVariantU8(Res).Data = 1;
    finally
      FreePSVariantList(Params);
    end;
  end else begin
    if MustExist then
      ShowPSExecError(erCouldNotCallProc);
  end;
end;

function TScriptRunner.RunIntegerFunction(const Name: AnsiString; const Parameters: array of Const; const MustExist: Boolean; const Default: Integer): Integer;
var
  ProcNo: Cardinal;
  Params: TPSList;
  Res: PPSVariant;
begin
  Result := Default;

  ProcNo := FPSExec.GetProc(Name);
  if ProcNo <> Cardinal(-1) then begin
    Params := TPSList.Create();
    try
      SetPSExecParameters(Parameters, Params);
      SetPSExecReturnValue(Params, btS32, Res);
      FPSExec.RunProc(Params, ProcNo);
      WriteBackParameters(Parameters, Params);

      RaisePSExecException;
      Result := PPSVariantS32(Res).Data;
    finally
      FreePSVariantList(Params);
    end;
  end else begin
    if MustExist then
      ShowPSExecError(erCouldNotCallProc);
  end;
end;

function TScriptRunner.RunStringFunction(const Name: AnsiString; const Parameters: array of Const; const MustExist: Boolean; const Default: String): String;
var
  ProcNo: Cardinal;
  Params: TPSList;
  Res: PPSVariant;
begin
  Result := Default;

  ProcNo := FPSExec.GetProc(Name);
  if ProcNo <> Cardinal(-1) then begin
    Params := TPSList.Create();
    try
      SetPSExecParameters(Parameters, Params);
{$IFDEF UNICODE}
      SetPSExecReturnValue(Params, btUnicodeString, Res);
{$ELSE}
      SetPSExecReturnValue(Params, btString, Res);
{$ENDIF}
      FPSExec.RunProc(Params, ProcNo);
      WriteBackParameters(Parameters, Params);

      RaisePSExecException;
{$IFDEF UNICODE}
      Result := PPSVariantUString(Res).Data;
{$ELSE}
      Result := PPSVariantAString(Res).Data;
{$ENDIF}
    finally
      FreePSVariantList(Params);
    end;
  end else begin
    if MustExist then
      ShowPSExecError(erCouldNotCallProc);
  end;
end;

function TScriptRunner.EvaluateUsedVariable(const Param1, Param2, Param3: LongInt; const Param4: AnsiString): String;

  function VariantToString(const p: TPSVariantIFC; const ClassProperties: AnsiString): String;
  begin
{$IFDEF UNICODE}
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
{$ENDIF}
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

end.

