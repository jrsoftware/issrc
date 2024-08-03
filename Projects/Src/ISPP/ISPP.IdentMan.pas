{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff

  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

unit ISPP.IdentMan;

interface

uses
  SysUtils, Classes, ISPP.Intf, ISPP.Base;

type

  EIdentError = class(Exception);
  EMacroError = class(EIdentError);

  TIdentManager = class;

  PIdent = ^TIdent;
  TIdent = object
    Name: string;
    Hash: Longint;
    IdentType: TIdentType;
  end;

  PDefinable = ^TDefinable;
  TDefinable = object(TIdent)
    Scope: packed record
      Locality: Word;        // 0 means public
      IsProtected: WordBool; // False means private,  not used if Locality = 0
    end;
  end;

  PVariable = ^TVariable;
  TVariable = object(TDefinable)
    Dim: Longint;
    Value: array[0..0] of TIsppVariant;
  end;

  TExprPosition = packed record
    FileIndex, Line, Column: Word;
  end;

  PMacro = ^TMacro;
  TMacro = object(TDefinable)
    Expression: string;
    DeclPos: TExprPosition;
    ParserOptions: TIsppParserOptions;
    ParamCount: Integer;
    Params: array[0..0] of TIsppMacroParam;
  end;

  PFunc = ^TFunc;
  TFunc = object(TIdent)
    Code: TIsppFunction;
    Ext: Longint;
  end;

  PActualParams = ^TActualParams;
  TActualParams = array of TVariable;

  IInternalFuncParams = interface(IIsppFuncParams)
    function Get(Index: Integer): PIsppVariant;
    function ResPtr: PIsppVariant;
  end;

  TDefineScope = (dsAny, dsPublic, dsProtected, dsPrivate); // order matters

  TIdentManager = class(TObject, IIdentManager)
  private
    FCustomIdents: IIdentManager;
    FFuncSender: Longint;
    FRefCount: Integer;
    FVarMan: TList;
    FLocalLevel: Integer;
    function FindIndex(const Name: string; AScope: TDefineScope): Integer;
    function Find(const Name: string; AScope: TDefineScope): PIdent;
    procedure FreeItem(Item: Pointer);
    function MacroIdents: IIdentManager;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
  public
    constructor Create(const CustomIdents: IIdentManager; FuncSender: Longint);
    destructor Destroy; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure BeginLocal;
    procedure EndLocal;
    function Defined(const Name: string): Boolean;
    procedure DefineFunction(const Name: string; Handler: TIsppFunction;
      Ext: Longint);
    procedure DefineMacro(const Name, Expression: string; ExprPos: TExprPosition;
      const ParserOptions: TIsppParserOptions; Params: array of TIsppMacroParam;
      Scope: TDefineScope);
    procedure DefineVariable(const Name: string; Index: Integer;
      const Value: TIsppVariant; Scope: TDefineScope);
    procedure Delete(const Name: string; Scope: TDefineScope);
    procedure DimVariable(const Name: string; Length: Integer; Scope: TDefineScope; var ReDim: Boolean);
    function GetIdent(const Name: string; out CallContext: ICallContext): TIdentType;
    function TypeOf(const Name: string): Byte;
    function DimOf(const Name: String): Integer;
  end;

const

  TYPE_ERROR    = 0;
  TYPE_NULL     = 1;
  TYPE_INTEGER  = 2;
  TYPE_STRING   = 3;
  TYPE_MACRO    = 4;
  TYPE_FUNC     = 5;
  TYPE_ARRAY    = 6;

implementation

uses
  Windows, Types, ISPP.Preprocessor, ISPP.CTokenizer, ISPP.Parser,
  ISPP.VarUtils, ISPP.Consts, ISPP.Sessions;

const
  MaxLocalArraySize = 16;
  GL: array[TDefineScope] of string = ('Public', 'Public', 'Protected', 'Private');

function MakeHash(const S: string): Longint;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    Result := ((Result shl 7) or (Result shr 25)) + Ord(UpCase(S[I]));
end;

{ TCallContext }

type

  TCallContext = class(TInterfacedObject)
  private
    procedure ErrorDefined(const ArgName: string);
    procedure ErrorNotSpecified(const ArgName: string);
    procedure ErrorTooMany;
    procedure ErrorTooFew;
    procedure ErrorWrongType(const ArgName: string);
  protected
    function GroupingStyle: TArgGroupingStyle;
  end;

procedure TCallContext.ErrorDefined(const ArgName: string);
begin
  raise EIdentError.CreateFmt(SParamSpecifiedTwice, [ArgName])
end;

procedure TCallContext.ErrorNotSpecified(const ArgName: string);
begin
  raise EIdentError.CreateFmt(SRequiredParamMissing, [ArgName])
end;

procedure TCallContext.ErrorTooMany;
begin
  raise EIdentError.Create(STooManyActualParams);
end;

procedure TCallContext.ErrorTooFew;
begin
  raise EIdentError.Create(SInsufficientParams)
end;

procedure TCallContext.ErrorWrongType(const ArgName: string);
begin
  raise EIdentError.CreateFmt(SWrongParamType, [ArgName])
end;

function TCallContext.GroupingStyle: TArgGroupingStyle;
begin
  Result := agsParenteses;
end;

{ TVarCallContext }

type

  TVarCallContext = class(TCallContext, ICallContext)
  private
    FVariable: PVariable;
    FIndex: Integer;
  protected
    constructor Create(Variable: PVariable);
    { ICallContext }
    procedure Add(const Name: string; const Value: TIsppVariant);
    function Call: TIsppVariant;
    function GroupingStyle: TArgGroupingStyle;
    procedure Clone(out NewContext: ICallContext);
  end;

constructor TVarCallContext.Create(Variable: PVariable);
begin
  FVariable := Variable;
  FIndex := -1;
end;

procedure TVarCallContext.Add(const Name: string;
  const Value: TIsppVariant);
begin
  if FVariable.Dim <> 0 then
  begin
    if (Name = '') or (CompareText(Name, 'INDEX') = 0) then
    begin
      if FIndex <> -1 then ErrorDefined('Index');
      FIndex := ToInt(Value).AsInt;
    end
    else
      raise EIdentError.CreateFmt(SUnknownParam, [Name]);
    if (FIndex < 0) or (FIndex >= FVariable.Dim) then
      raise EIdentError.CreateFmt(SIndexIsOutOfArraySize, [FIndex, FVariable.Name]);
  end
  else
    raise EIdentError.Create(SParameterlessVariable);
end;

function TVarCallContext.Call: TIsppVariant;
begin
  if FIndex < 0 then
    if FVariable.Dim <> 0 then
      raise EIdentError.CreateFmt(SIndexNotSpecifiedForArray, [FVariable.Name])
    else
      FIndex := 0;
  Result.Typ := evLValue;
  Result.AsPtr := @(FVariable^.Value[FIndex]);
  SimplifyLValue(Result);
end;

function TVarCallContext.GroupingStyle: TArgGroupingStyle;
begin
  if FVariable.Dim <> 0 then
    Result := agsBrackets
  else
    Result := agsNone
end;

{ TMacroCallContext }

var
  MacroStack: TStrings;

procedure PushMacro(const Name: string);
begin
  if MacroStack = nil then
    MacroStack := TStringList.Create
  else ;
    {if MacroStack.IndexOf(UpperCase(Name)) >= 0 then
      raise EMacroError.CreateFmt(SRecursiveMacroCall, [Name]);}
  MacroStack.Add(UpperCase(Name));
end;

procedure PopMacro;
begin
  MacroStack.Delete(MacroStack.Count - 1);
  if MacroStack.Count = 0 then
  begin
    MacroStack.Free;
    MacroStack := nil
  end;
end;

type

{ TMacroLocalArrayCallContext }

  TMacroCallContext = class;

  TMacroLocalArrayCallContext = class(TCallContext, ICallContext)
  private
    FMacroContext: TMacroCallContext;
    FIndex: Integer;
  public
    constructor Create(MacroContext: TMacroCallContext);
    procedure Add(const Name: String; const Value: TIsppVariant);
    function Call: TIsppVariant;
    function GroupingStyle: TArgGroupingStyle;
    procedure Clone(out NewContext: ICallContext);
  end;

{ TMacroCallContext }

  TMacroArgument = record
    Value: TVariable;
    Defined: Boolean;
  end;

  PMacroArgArray = ^TMacroArgArray;
  TMacroArgArray = array[0..0] of TMacroArgument;

  TMacroCallContext = class(TCallContext, ICallContext, IIdentManager)
  private
    FIdentManager: IIdentManager;
    FMacro: PMacro;
    FList: PMacroArgArray;
    FCurrentParam: Integer;
    FLocalVars: TList;
    procedure AdjustLocalArray(Index: Integer);
    function FindFormalParam(const Name: string): Integer;
  protected
    constructor Create(const IdentManager: IIdentManager; Macro: PMacro);
    destructor Destroy; override;
    { IIdentManager}
    function GetIdent(const Name: string; out CallContext: ICallContext): TIdentType;
    function Defined(const Name: string): Boolean;
    function TypeOf(const Name: string): Byte;
    function DimOf(const Name: string): Integer;
    { ICallContext }
    procedure Add(const Name: string; const Value: TIsppVariant);
    function Call: TIsppVariant;
    procedure Clone(out NewContext: ICallContext);
  end;

constructor TMacroLocalArrayCallContext.Create(MacroContext: TMacroCallContext);
begin
  FMacroContext := MacroContext;
  FIndex := -1;
end;

procedure TMacroLocalArrayCallContext.Add(const Name: String;
  const Value: TIsppVariant);
begin
  if (Name = '') or (CompareText(Name, 'INDEX') = 0) then
  begin
    if FIndex <> -1 then ErrorDefined('Index');
    FIndex := ToInt(Value).AsInt;
  end
  else
    raise EIdentError.CreateFmt(SUnknownParam, [Name]);
  if (FIndex < 0) or (FIndex >= MaxLocalArraySize) then
    raise EIdentError.Create(SLocalArraysIndexError);
end;

function TMacroLocalArrayCallContext.Call: TIsppVariant;
begin
  if FIndex = -1 then FIndex := 0;
  FMacroContext.AdjustLocalArray(FIndex);
  Result.Typ := evLValue;
  Result.AsPtr := FMacroContext.FLocalVars[FIndex];
end;

function TMacroLocalArrayCallContext.GroupingStyle: TArgGroupingStyle;
begin
  Result := agsBrackets;
end;

constructor TMacroCallContext.Create(const IdentManager: IIdentManager;
  Macro: PMacro);
begin
  FIdentManager := IdentManager;
  FMacro := Macro;
  FList := AllocMem(SizeOf(TMacroArgument) * Macro^.ParamCount);
end;

destructor TMacroCallContext.Destroy;
var
  I: Integer;
begin
  if Assigned(FLocalVars) then
  begin
    for I := 0 to FLocalVars.Count - 1 do
      Dispose(PIsppVariant(FLocalVars[I]));
    FLocalVars.Free;
  end;
  FreeMem(FList)
end;

procedure TMacroCallContext.Add(const Name: string;
  const Value: TIsppVariant);
var
  ParamIndex: Integer;
begin
  if Name <> '' then
    ParamIndex := FindFormalParam(Name)
  else
    ParamIndex := FCurrentParam;
  if ParamIndex >= FMacro.ParamCount then
    ErrorTooMany;
  if FList[ParamIndex].Defined then
    ErrorDefined(FMacro.Params[ParamIndex].Name);

  if Value.Typ = evSpecial then //parser is in "skip" state
  else
  if Value.Typ = evNull then
    if pfHasDefault in FMacro.Params[ParamIndex].ParamFlags then
      FList[ParamIndex].Value.Value[0] := FMacro.Params[ParamIndex].DefValue
    else
      ErrorNotSpecified(FMacro.Params[ParamIndex].Name)
  else
    if (pfByRef in FMacro.Params[ParamIndex].ParamFlags) and
      (Value.Typ <> evLValue) then
       raise EIdentError.CreateFmt(SLValueRequiredForByRefParam, [FMacro.Params[ParamIndex].Name])
    else
      if (pfTypeDefined in FMacro.Params[ParamIndex].ParamFlags) and
        (GetRValue(Value).Typ <> FMacro.Params[ParamIndex].DefValue.Typ) then
        ErrorWrongType(FMacro.Params[ParamIndex].Name)
      else
        if pfByRef in FMacro.Params[ParamIndex].ParamFlags then
        begin
          FList[ParamIndex].Value.Value[0] := Value;
          SimplifyLValue(FList[ParamIndex].Value.Value[0]);
        end
        else
        begin
          if FMacro.Params[ParamIndex].DefValue.Typ = evCallContext then
          begin
            if (pfFunc in FMacro.Params[ParamIndex].ParamFlags) and
              (Value.AsCallContext.GroupingStyle <> agsParenteses) or
              not (pfFunc in FMacro.Params[ParamIndex].ParamFlags) and
              (Value.AsCallContext.GroupingStyle <> agsBrackets) then
              ErrorWrongType(FMacro.Params[ParamIndex].Name);
          end;
          FList[ParamIndex].Value.Value[0] := GetRValue(Value);
        end;
  FList[ParamIndex].Defined := True;
  FList[ParamIndex].Value.Name := FMacro.Params[ParamIndex].Name;
  FList[ParamIndex].Value.Dim := 0;
  Inc(FCurrentParam);
end;

function TMacroCallContext.Call: TIsppVariant;
var
  I: Integer;
  Msg: string;
begin
  PushMacro(FMacro.Name);
  try
    for I := 0 to FMacro.ParamCount - 1 do
      if not FList[I].Defined then
        if not (pfHasDefault in FMacro.Params[I].ParamFlags) then
          ErrorNotSpecified(FMacro.Params[I].Name)
          //raise EMacroError.CreateFmt(SNoReqParam, [FMacro.Params[I].Name])
        else
        begin
          FList[I].Value.Name := FMacro.Params[I].Name;
          FList[I].Value.Dim := 0;
          FList[I].Value.Value[0] := FMacro.Params[I].DefValue;
          FList[I].Defined := True;
        end;
    try
      Result := Parse(Self, FMacro.Expression, FMacro.DeclPos.Column,
        @FMacro.ParserOptions);
    except
      on E: EParsingError do
      begin
        if E.Position > 0 then
        begin
          if FMacro.DeclPos.FileIndex > 0 then
            Msg := Format(SErrorExecutingMacroFile, [FMacro.Name,
              PeekPreproc.IncludedFiles[FMacro.DeclPos.FileIndex],
              FMacro.DeclPos.Line, E.Position, E.Message])
          else
            Msg := Format(SErrorExecutingMacro, [FMacro.Name,
              FMacro.DeclPos.Line, E.Position, E.Message]);
          E.Message := Msg;
          E.Position := 0;
        end;
        raise;
      end;
      on E: Exception do
      begin
        E.Message := Format(SErrorExecutingMacroUnexpected, [FMacro.Name, E.Message]);
        raise;
      end;
    end;
    VerboseMsg(9, SSuccessfullyCalledMacro, [FMacro.Name]);
  finally
    PopMacro;
  end;
end;

function TMacroCallContext.Defined(const Name: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  if CompareText(Name, SLocal) = 0 then Exit;
  for I := 0 to FMacro^.ParamCount - 1 do
    if CompareText(FMacro^.Params[I].Name, Name) = 0 then
      Exit;
  Result := FIdentManager.Defined(Name)
end;

function TMacroCallContext.FindFormalParam(const Name: string): Integer;
begin
  for Result := 0 to FMacro.ParamCount - 1 do
    if CompareText(FMacro.Params[Result].Name, Name) = 0 then Exit;
  raise EMacroError.CreateFmt(SUnknownParam, [Name]);
end;

function TMacroCallContext.GetIdent(const Name: string;
  out CallContext: ICallContext): TIdentType;
var
  I: Integer;
begin
  Result := itVariable;
  if CompareText(SLocal, Name) = 0 then
  begin
    CallContext := TMacroLocalArrayCallContext.Create(Self);
    Exit;
  end
  else
    for I := 0 to FMacro^.ParamCount - 1 do
      if CompareText(FMacro^.Params[I].Name, Name) = 0 then
      begin
        if FMacro^.Params[I].DefValue.Typ = evCallContext then
          FList[I].Value.Value[0].AsCallContext.Clone(CallContext)
        else
          CallContext := TVarCallContext.Create(@FList[I]);
        Exit;
      end;
  Result := FIdentManager.GetIdent(Name, CallContext)
end;

function TMacroCallContext.TypeOf(const Name: string): Byte;
var
  I: Integer;
begin
  if CompareText(Name, SLocal) = 0 then
  begin
    Result := TYPE_ARRAY;
    Exit;
  end;
  for I := 0 to FMacro^.ParamCount - 1 do
    if CompareText(FMacro^.Params[I].Name, Name) = 0 then
    begin
      case GetRValue(FList[I].Value.Value[0]).Typ of
        evNull: Result := TYPE_NULL;
        evInt: Result := TYPE_INTEGER
      else
        Result := TYPE_STRING
      end;
      Exit;
    end;
  Result := FIdentManager.TypeOf(Name)
end;

{TFuncParam}

type

  TFuncParam = class(TInterfacedObject, IIsppFuncParam)
  private
    FValue: PIsppVariant;
  protected
    constructor Create(Value: PIsppVariant);
    function GetType: TIsppVarType; stdcall;
    function GetAsInt: Int64; stdcall;
    function GetAsString(Buf: PChar; BufSize: Integer): Integer; stdcall;
  end;

constructor TFuncParam.Create(Value: PIsppVariant);
begin
  FValue := Value
end;

function TFuncParam.GetAsInt: Int64;
begin
  Result := FValue^.AsInt
end;

function TFuncParam.GetAsString(Buf: PChar; BufSize: Integer): Integer;
begin
  StrLCopy(Buf, PChar(FValue^.AsStr), BufSize);
  Result := Length(FValue^.AsStr)
end;

function TFuncParam.GetType: TIsppVarType;
begin
  Result := FValue^.Typ
end;

{ TFuncCallContext }

type

  TFuncCallContext = class(TCallContext, ICallContext, IInternalFuncParams,
    IIsppFuncResult)
  private
    FSender: Longint;
    FFunc: PFunc;
    FResult: TIsppVariant;
    FParams: TList;
  protected
    constructor Create(Sender: Longint; Func: PFunc);
    destructor Destroy; override;
    { IIsppFuncParams }
    function Get(Index: Integer): IIsppFuncParam; stdcall;
    function GetCount: Integer; stdcall;
    { IInternalFuncParams }
    function IInternalFuncParams.Get = InternalGet;
    function InternalGet(Index: Integer): PIsppVariant;
    function ResPtr: PIsppVariant;
    { IIsppFuncResult }
    procedure SetAsInt(Value: Int64); stdcall;
    procedure SetAsString(Value: PChar); stdcall;
    procedure SetAsNull; stdcall;
    procedure Error(Message: PChar); stdcall;
    { ICallContext }
    procedure Add(const Name: string; const Value: TIsppVariant);
    function Call: TIsppVariant;
    procedure Clone(out NewContext: ICallContext);
  end;

constructor TFuncCallContext.Create(Sender: Longint; Func: PFunc);
begin
  FSender := Sender;
  FFunc := Func;
  FParams := TList.Create;
end;

destructor TFuncCallContext.Destroy;
begin
  FParams.Free;
end;

procedure TFuncCallContext.Add(const Name: string;
  const Value: TIsppVariant);
var
  V: PIsppVariant;
begin
  if Name <> '' then
    raise EIdentError.Create(SFuncsNoSupportNamedParams);
  New(V);
  CopyExpVar(Value, V^);
  FParams.Add(V);
end;

function TFuncCallContext.Call: TIsppVariant;
var
  InternalParams: IInternalFuncParams;
  Error: TIsppFuncResult;
  Ext: Longint;
begin
  InternalParams := Self;
  if FFunc.Ext = -1 then
    Ext := FSender
  else
    Ext := FFunc.Ext;
  Error := FFunc.Code(Ext, InternalParams, Self);
  case Error.Error of
    ISPPFUNC_FAIL: raise EIdentError.CreateFmt(SFuncError, [FFunc^.Name]);
    ISPPFUNC_MANYARGS: ErrorTooMany;
    ISPPFUNC_INSUFARGS: ErrorTooFew;
    ISPPFUNC_INTWANTED: raise EIdentError.Create(SIntegerExpressionExpected);
    ISPPFUNC_STRWANTED: raise EIdentError.Create(SStringExpressionExpected);
  end;
  Result := FResult;
  VerboseMsg(9, SSuccessfullyCalledFunction, [FFunc.Name]);
end;

procedure TFuncCallContext.Error(Message: PChar);
begin
  raise Exception.Create(Message)
end;

function TFuncCallContext.Get(Index: Integer): IIsppFuncParam;
begin
  Result := TFuncParam.Create(FParams[Index]);
end;

function TFuncCallContext.GetCount: Integer;
begin
  Result := FParams.Count
end;

function TFuncCallContext.InternalGet(Index: Integer): PIsppVariant;
begin
  Result := FParams[Index]
end;

function TFuncCallContext.ResPtr: PIsppVariant;
begin
  Result := @FResult
end;

procedure TFuncCallContext.SetAsInt(Value: Int64);
begin
  MakeInt(FResult, Value)
end;

procedure TFuncCallContext.SetAsNull;
begin
  FResult := NULL
end;

procedure TFuncCallContext.SetAsString(Value: PChar);
begin
  MakeStr(FResult, Value)
end;

{ TIdentManager }

constructor TIdentManager.Create(const CustomIdents: IIdentManager; FuncSender: Longint);
begin
  FCustomIdents := CustomIdents;
  FVarMan := TList.Create;
  FFuncSender := FuncSender;
end;

destructor TIdentManager.Destroy;
var
  I: Integer;
begin
  for I := 0 to FVarMan.Count - 1 do
    FreeItem(FVarMan[I]);
  FVarMan.Free;
end;

function TIdentManager.Defined(const Name: string): Boolean;
begin
  Result := Find(Name, dsAny) <> nil
end;

procedure TIdentManager.DefineFunction(const Name: string;
  Handler: TIsppFunction; Ext: Integer);
var
  F: PFunc;
begin
  if Find(Name, dsAny) <> nil then Exit;
  F := AllocMem(SizeOf(TFunc));
  F.Name := Name;
  F.Hash := MakeHash(Name);
  F.IdentType := itFunc;
  F.Code := Handler;
  F.Ext := Ext;
  FVarMan.Add(F);
end;

procedure TIdentManager.DefineMacro(const Name, Expression: string;
  ExprPos: TExprPosition; const ParserOptions: TIsppParserOptions;
  Params: array of TIsppMacroParam; Scope: TDefineScope);
var
  P: PMacro;
  ArrSize, I, J: Integer;
begin
  if Scope = dsAny then Scope := dsPublic;
  Delete(Name, Scope);
  ArrSize := SizeOf(TIsppMacroParam) * (Length(Params));

  for I := 1 to High(Params) do
    for J := 0 to I - 1 do
      if CompareText(Params[I].Name, Params[J].Name) = 0 then
        raise EIdentError.CreateFmt(SRedeclaredIdentifier, [Params[I].Name]);

  P := AllocMem(SizeOf(TMacro) + ArrSize);
  try
    P^.Name := Name;
    P^.Hash := MakeHash(Name);
    P^.IdentType := itMacro;
    P^.Scope.IsProtected := Scope = dsProtected;
    if Scope >= dsProtected then P^.Scope.Locality := FLocalLevel;
    P^.Expression := Expression;
    P^.DeclPos := ExprPos;
    P^.ParserOptions := ParserOptions;
    P^.ParamCount := Length(Params);
    for I := 0 to High(Params) do
      P^.Params[I] := Params[I];
    FVarMan.Add(P);
  except
    FreeMem(P)
  end;
  VerboseMsg(4, SMacroDefined, [GL[Scope], Name]);
end;

procedure TIdentManager.DefineVariable(const Name: string; Index: Integer;
  const Value: TIsppVariant; Scope: TDefineScope);
var
  V: PVariable;
  Ident: PIdent;
begin
  if Scope = dsAny then Scope := dsPublic;
  Ident := Find(Name, Scope);
  if (Ident <> nil) and (Ident.IdentType = itVariable) and (PVariable(Ident).Dim <> 0) then
  begin
    V := PVariable(Ident);
    if (Index < 0) or (Index >= V.Dim) then
      raise EIdentError.CreateFmt(SIndexIsOutOfArraySize, [Index, Name]);
    V.Value[Index] := Value;
  end
  else
  begin
    if Index <> -1 then
      raise EIdentError.CreateFmt(SUndeclaredIdentifier, [Name]);
    Delete(Name, Scope);
    V := AllocMem(SizeOf(TVariable));
    V^.Name := Name;
    V^.Hash := MakeHash(Name);
    V^.IdentType := itVariable;
    V^.Scope.IsProtected := Scope = dsProtected;
    if Scope >= dsProtected then V^.Scope.Locality := FLocalLevel;
    V^.Dim := 0;
    V^.Value[0] := Value;
    FVarMan.Add(V);
  end;
  VerboseMsg(4, SVariableDefined, [GL[Scope], Name]);
end;

procedure TIdentManager.Delete(const Name: string; Scope: TDefineScope);
var
  P: PIdent;
  S: TDefineScope;
const
  VM: array[itVariable..itMacro] of string = ('variable', 'macro');
begin
  {if Scope = dsAny then
  begin
    P := Find(Name, dsPrivate);
    if P = nil then P := Find(Name, dsProtected);
    if P = nil then P := Find(Name, dsPublic)
  end
  else}
    P := Find(Name, Scope);
  if (P <> nil) and (P.IdentType in [itVariable, itMacro]) then
  begin
    //if PDefinable(P).Scope.Locality <> FLocalLevel then Exit;
    S := dsPublic;
    with PDefinable(P).Scope do
      if Locality <> 0 then
        if IsProtected then
          S := dsProtected
        else
          S := dsPrivate;
    VerboseMsg(4, SUndefined, [GL[S],
      VM[P.IdentType], P.Name]);
    FVarMan.Remove(P);
    FreeItem(P);
  end
end;

procedure TIdentManager.DimVariable(const Name: string; Length: Integer;
  Scope: TDefineScope; var ReDim: Boolean);
var
  V, VOld: PVariable;
  I, ReDimIndex: Integer;
  Msg: String;
begin
  if Length > 0 then begin
    if Scope = dsAny then Scope := dsPublic;

    if ReDim then begin
      ReDimIndex := FindIndex(Name, Scope);
      if (ReDimIndex <> -1) and
         ((PIdent(FVarMan[ReDimIndex]).IdentType <> itVariable) or
          (PVariable(FVarMan[ReDimIndex]).Dim = 0)) then
        ReDimIndex := -1; //not a variable or not an array, #dim normally
      ReDim := ReDimIndex <> -1;
    end else
      ReDimIndex := -1;

    V := AllocMem(SizeOf(TVariable) + SizeOf(TIsppVariant) * (Length - 1));
    V.Name := Name;
    V.Hash := MakeHash(Name);
    V.IdentType := itVariable;
    V.Dim := Length;
    V^.Scope.IsProtected := Scope = dsProtected;
    if Scope >= dsProtected then V^.Scope.Locality := FLocalLevel;

    if ReDimIndex = -1 then begin
      Delete(Name, Scope);
      for I := 0 to Length - 1 do
        V.Value[I] := NULL;
      FVarMan.Add(V);
      Msg := SArrayDeclared;
    end else begin
      VOld := PVariable(FVarMan[ReDimIndex]);
      for I := 0 to VOld.Dim - 1 do
        if I < Length then
          V.Value[I] := VOld.Value[I];
      for I := VOld.Dim to Length - 1 do
        V.Value[I] := NULL;
      FVarMan[ReDimIndex] := V;
      FreeItem(VOld);
      Msg := SArrayReDimmed;
    end;
    VerboseMsg(4, Msg, [GL[Scope], Name]);
 end else
    raise EIdentError.Create(SBadLength);
end;

function TIdentManager.FindIndex(const Name: string; AScope: TDefineScope): Integer;
var
  I: Integer;
  H: Longint;
begin
  Result := -1;
  H := MakeHash(Name);
  for I := FVarMan.Count - 1 downto 0 do
    if (H = PIdent(FVarMan[I]).Hash) and (
      CompareText(PIdent(FVarMan[I]).Name, Name) = 0) then
    begin
      if (PIdent(FVarMan[I]).IdentType in [itVariable, itMacro]) then
        with PDefinable(FVarMan[I])^.Scope do
          case AScope of
            dsAny:
              if not ((Locality = 0) or (Locality = FLocalLevel) or IsProtected) then Continue;
            dsPublic:
              if Locality <> 0 then Continue;
            dsProtected:
              if not (IsProtected and (Locality <= FLocalLevel)) then Continue;
          else
              if IsProtected or (Locality <> FLocalLevel) then Continue;
          end;
      Result := I;
      Exit
    end;
end;

function TIdentManager.Find(const Name: string; AScope: TDefineScope): PIdent;
var
  I: Integer;
begin
  I := FindIndex(Name, AScope);
  if I >= 0 then
    Result := FVarMan[I]
  else
    Result := nil;
end;

function TIdentManager.GetIdent(const Name: string;
  out CallContext: ICallContext): TIdentType;
var
  P: PIdent;
begin
  if CompareText(Name, 'DEFINED') = 0 then
    Result := itDefinedFunc
  else if CompareText(Name, 'TYPEOF') = 0 then
    Result := itTypeOfFunc
  else if CompareText(Name, 'DIMOF') = 0 then
    Result := itDimOfFunc
  else
  begin
    P := Find(Name, dsAny);
    if P <> nil then
    begin
      Result := P.IdentType;
      case P.IdentType of
        itVariable: CallContext := TVarCallContext.Create(PVariable(P));
        itMacro: CallContext := TMacroCallContext.Create(MacroIdents, PMacro(P));
        itFunc: CallContext := TFuncCallContext.Create(FFuncSender, PFunc(P));
      else
        Assert(False)
      end;
    end
    else
      Result := itUnknown;
  end;
end;

function TIdentManager.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TIdentManager.TypeOf(const Name: string): Byte;
var
  P: PIdent;
begin
  Result := TYPE_ERROR;
  P := Find(Name, dsAny);
  if P <> nil then
    case P.IdentType of
      itVariable:
        if PVariable(P).Dim > 0 then
          Result := TYPE_ARRAY
        else
          case PVariable(P).Value[0].Typ of
            evNull: Result := TYPE_NULL;
            evInt: Result := TYPE_INTEGER;
            evStr: Result := TYPE_STRING
          end;
      itMacro: Result := TYPE_MACRO;
      itFunc: Result := TYPE_FUNC
    end
end;

function TIdentManager._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount)
end;

function TIdentManager._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

procedure TIdentManager.BeginLocal;
begin
  Inc(FLocalLevel);
end;

procedure TIdentManager.EndLocal;
var
  I: Integer;
begin
  for I := FVarMan.Count - 1 downto 0 do
    if (PIdent(FVarMan.Items[I]).IdentType in [itVariable, itMacro]) and
      (PDefinable(FVarMan.Items[I]).Scope.Locality = FLocalLevel) then
    begin
      FreeItem(FVarMan[I]);
      FVarMan.Delete(I);
    end;
  Dec(FLocalLevel);
end;

procedure TIdentManager.FreeItem(Item: Pointer);

  function ZeroToOne(A: Integer): Integer;
  begin
    if A = 0 then Result := 1 else Result := A
  end;

begin
  with PIdent(Item)^ do
  begin
    Finalize(Name);
    case IdentType of
      itVariable: with PVariable(Item)^ do Finalize(Value[0], ZeroToOne(Dim));
      itMacro:
        with PMacro(Item)^ do
        begin
          Finalize(Params[0], ParamCount);
          Finalize(Expression);
        end;
    end;
  end;
  FreeMem(Item);
end;

function TIdentManager.MacroIdents: IIdentManager;
begin
  if FCustomIdents <> nil then
    Result := FCustomIdents
  else
    Result := Self
end;

procedure TMacroCallContext.AdjustLocalArray(Index: Integer);
var
  I: Integer;
  V: PIsppVariant;
begin
  if not Assigned(FLocalVars) then
    FLocalVars := TList.Create;
  if FLocalVars.Count > Index then Exit;
  VerboseMsg(10, SAllocatingMacroLocalArrayUpToEle, [FMacro.Name, Index]);
  for I := FLocalVars.Count to Index do
  begin
    New(V);
    V.Typ := evNull;
    FLocalVars.Add(V);
  end;
end;

procedure TVarCallContext.Clone(out NewContext: ICallContext);
begin
  if FVariable.Dim = 0 then
    NewContext := Self
  else
    NewContext := TVarCallContext.Create(FVariable);
end;

procedure TMacroLocalArrayCallContext.Clone(out NewContext: ICallContext);
begin
  NewContext := TMacroLocalArrayCallContext.Create(FMacroContext);
end;

procedure TMacroCallContext.Clone(out NewContext: ICallContext);
begin
  NewContext := TMacroCallContext.Create(FIdentManager, FMacro);
end;

procedure TFuncCallContext.Clone(out NewContext: ICallContext);
begin
  NewContext := TFuncCallContext.Create(FSender, FFunc);
end;

function TIdentManager.DimOf(const Name: String): Integer;
var
  Ident: PIdent;
begin
  Ident := Find(Name, dsAny);
  if Assigned(Ident) and (Ident.IdentType = itVariable) then
    Result := PVariable(Ident)^.Dim
  else
    Result := 0;
end;

function TMacroCallContext.DimOf(const Name: string): Integer;
begin
  if CompareText(Name, SLocal) = 0 then
    Result := MaxLocalArraySize
  else
    Result := FIdentManager.DimOf(Name);
end;

end.


