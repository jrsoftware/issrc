unit IDE.ScriptModel.Metadata.Extra.FunctionDefinitions;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Function definitions for call tips and hover hints, not used by IDE.ScriptModel.pas
}

interface

uses
  Generics.Collections,
  Shared.ScriptFunc;

type
  TFunctionDefinition = record
    ScriptFuncWithoutHeader: AnsiString;
    HeaderKind: TScriptFuncHeaderKind;
    HasParameters: Boolean;
    constructor Create(const ScriptFunc: AnsiString);
    {$WARN DUPLICATE_CTOR_DTOR OFF} { Don't care about C++ }
    constructor CreateISPP(const ISPPScriptFunc: AnsiString);
    constructor CreateUserDefined(const ScriptFunc: String;
      const AHeaderKind: TScriptFuncHeaderKind; const AHasParameters: Boolean);
    {.$WARN DUPLICATE_CTOR_DTOR ON} { Restoring doesn't work }
  end;
  TFunctionDefinitions = array of TFunctionDefinition;
  TFunctionDefinitionsByName = TDictionary<String, TFunctionDefinitions>;

  { Use when the name is known without having to call ExtractScriptFuncName }
  TFunctionDefinitionWithName = record
    Name: String;
    Definition: TFunctionDefinition;
  end;
  TFunctionDefinitionsWithName = array of TFunctionDefinitionWithName;

var
  ISPPFunctionsByName: TFunctionDefinitionsByName;
  ScriptFunctionsByName: array[Boolean] of TFunctionDefinitionsByName;

function GetISPPFunctionDefinition(const Name: String;
  const Index: Integer; out Count: Integer): TFunctionDefinition;
function GetScriptFunctionDefinition(const ClassMember: Boolean;
  const Name: String; const Index: Integer;
  const UserDefined: TFunctionDefinitionsWithName; out Count: Integer): TFunctionDefinition; overload;
function GetScriptFunctionDefinition(const ClassMember: Boolean;
  const Name: String; const Index: Integer;
  const UserDefined: TFunctionDefinitionsWithName): TFunctionDefinition; overload;

procedure InitializeFunctionDefinitions;

implementation

uses
  SysUtils, Generics.Defaults,
  IDE.ScriptModel.Metadata.Extra,
  isxclasses_wordlists_generated;

var
  FunctionDefinitionsInitialized: Boolean;

{ TFunctionDefinition }

constructor TFunctionDefinition.Create(const ScriptFunc: AnsiString);
begin
  ScriptFuncWithoutHeader := RemoveScriptFuncHeader(ScriptFunc, HeaderKind);
  HasParameters := ScriptFuncHasParameters(ScriptFunc);
end;

constructor TFunctionDefinition.CreateISPP(const ISPPScriptFunc: AnsiString);
begin
  ScriptFuncWithoutHeader := RemoveISPPScriptFuncHeader(ISPPScriptFunc, HeaderKind);
  HasParameters := ScriptFuncHasParameters(ISPPScriptFunc);
end;

constructor TFunctionDefinition.CreateUserDefined(const ScriptFunc: String;
  const AHeaderKind: TScriptFuncHeaderKind; const AHasParameters: Boolean);
begin
  var HeaderLength: Integer;
  case AHeaderKind of
    hkFunction: HeaderLength := Length('function');
    hkProcedure: HeaderLength := Length('procedure');
  else
    raise Exception.CreateFmt('Invalid user-defined header kind: %d', [Ord(AHeaderKind)]);
  end;

  ScriptFuncWithoutHeader := AnsiString(TrimLeft(Copy(ScriptFunc, HeaderLength+1, MaxInt)));
  HeaderKind := AHeaderKind;
  HasParameters := AHasParameters;
end;

{ --- }

{ Result is undefined if out Count = 0 }
function GetFunctionDefinitionFromList(const FunctionDefinitions: TFunctionDefinitions;
  const Index: Integer; out Count: Integer): TFunctionDefinition;
begin
  Count := Integer(Length(FunctionDefinitions));
  if Count > 0 then begin
    var ResultIndex := Index;
    if ResultIndex >= Count then
      ResultIndex := Count-1;
    Result := FunctionDefinitions[ResultIndex];
  end;
end;

{ Result is undefined if out Count = 0 }
function GetISPPFunctionDefinition(const Name: String;
  const Index: Integer; out Count: Integer): TFunctionDefinition;
begin
  var FunctionDefinitions: TFunctionDefinitions;
  if ISPPFunctionsByName.TryGetValue(Name, FunctionDefinitions) then
    Result := GetFunctionDefinitionFromList(FunctionDefinitions, Index, Count)
  else
    Count := 0;
end;

{ Result is undefined if out Count = 0 }
function GetScriptFunctionDefinition(const ClassMember: Boolean;
  const Name: String; const Index: Integer;
  const UserDefined: TFunctionDefinitionsWithName; out Count: Integer): TFunctionDefinition;
begin
  var Definitions: TFunctionDefinitions := [];
  { Add matching from UserDefined }
  for var UserDefinedFunctionDefinition in UserDefined do
    if SameText(UserDefinedFunctionDefinition.Name, Name) then
      Definitions := Definitions + [UserDefinedFunctionDefinition.Definition];
  { Add matching from built-ins }
  var BuiltInDefinitions: TFunctionDefinitions;
  if ScriptFunctionsByName[ClassMember].TryGetValue(Name, BuiltInDefinitions) then
    Definitions := Definitions + BuiltInDefinitions;
  Result := GetFunctionDefinitionFromList(Definitions, Index, Count);
end;

function GetScriptFunctionDefinition(const ClassMember: Boolean;
  const Name: String; const Index: Integer;
  const UserDefined: TFunctionDefinitionsWithName): TFunctionDefinition;
begin
  var Count: Integer;
  Result := GetScriptFunctionDefinition(ClassMember, Name, Index, UserDefined, Count);
end;

procedure InitializeFunctionDefinitions;

  procedure BuildScriptFunctionsByName(const ScriptFuncTable: TScriptTable;
    const ClassMembers: Boolean);
  begin
    for var ScriptFunc in ScriptFuncTable do begin
      const FunctionDefinition = TFunctionDefinition.Create(ScriptFunc);
      const ScriptFuncName = ExtractScriptFuncWithoutHeaderName(FunctionDefinition.ScriptFuncWithoutHeader);
      const Key = String(ScriptFuncName);
      if not ScriptFunctionsByName[ClassMembers].TryAdd(Key, [FunctionDefinition]) then begin
        { Function has multiple prototypes }
        var ScriptFunctions := ScriptFunctionsByName[ClassMembers][Key];
        const N = Length(ScriptFunctions);
        SetLength(ScriptFunctions, N+1);
        ScriptFunctions[N] := FunctionDefinition;
        ScriptFunctionsByName[ClassMembers][Key] := ScriptFunctions;
      end;
    end;
  end;

begin
  if FunctionDefinitionsInitialized then
    Exit;
  FunctionDefinitionsInitialized := True;

  ISPPFunctionsByName := TFunctionDefinitionsByName.Create(TIStringComparer.Ordinal);
  for var ISPPFunction in ISPPFunctions do begin
    const FunctionDefinition = TFunctionDefinition.CreateISPP(ISPPFunction);
    const ISPPScriptFuncName = ExtractISPPScriptFuncWithoutHeaderName(FunctionDefinition.ScriptFuncWithoutHeader);
    const Key = String(ISPPScriptFuncName);
    if not ISPPFunctionsByName.TryAdd(Key, [FunctionDefinition]) then
      raise Exception.CreateFmt('Internal error: duplicate ISPP function "%s"', [ISPPScriptFuncName]);
  end;

  ScriptFunctionsByName[False] := TFunctionDefinitionsByName.Create(TIStringComparer.Ordinal);
  ScriptFunctionsByName[True] := TFunctionDefinitionsByName.Create(TIStringComparer.Ordinal);
  var ClassMembers := False;
  for var ScriptFuncTable in ScriptFuncTables do
    BuildScriptFunctionsByName(ScriptFuncTable, ClassMembers);
  BuildScriptFunctionsByName(DelphiScriptFuncTable, ClassMembers);
  BuildScriptFunctionsByName(ROPSScriptFuncTable, ClassMembers);
  ClassMembers := True;
  BuildScriptFunctionsByName(PascalMembers_Isxclasses, ClassMembers);
end;

initialization
finalization
  ScriptFunctionsByName[False].Free;
  ScriptFunctionsByName[True].Free;
  ISPPFunctionsByName.Free;
end.
