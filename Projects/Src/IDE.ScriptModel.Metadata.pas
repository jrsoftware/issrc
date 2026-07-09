unit IDE.ScriptModel.Metadata;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Per-section metadata tables for the script model: parameter names, value
  kinds, and flag lists, plus the rules layer registrations.
}

interface

type
  TScriptParameterValueKind = (pvkString, pvkBoolean, pvkInteger, pvkVersion,
    pvkChoice, pvkFlags);

  TScriptParameterDefinition = record
    Name: String;
    ValueKind: TScriptParameterValueKind;
    FlagNames: TArray<String>; { For pvkFlags: the known tokens; for
                                 pvkChoice: the known single values }
    Obsolete: Boolean;         { Documented as obsolete: to be hidden unless explicitly present in the script }
  end;

  { Rule: including a flag in a delimited parameter must also include other flags,
    for example checking extractarchive on a [Files] entry also checks
    external and ignoreversion }
  TScriptFlagIncludesRule = record
    ParameterName: String;
    FlagName: String;
    AlsoIncludedFlagNames: TArray<String>; { the other flags to include }
  end;

  { Rule: giving a parameter a non-empty value must also include a flag, for example
    setting Verb on a [Run] entry also checks the shellexec flag }
  TScriptParameterIncludesFlagRule = record
    ParameterName: String;      { the parameter whose value triggers the rule }
    FlagParameterName: String;  { the flag-list parameter that holds the flag }
    FlagName: String;           { the flag to include }
  end;

  TScriptSectionMetadata = class
  private
    FSectionName: String;
    FParameters: TArray<TScriptParameterDefinition>;
    FFlagIncludesRules: TArray<TScriptFlagIncludesRule>;
    FParameterIncludesFlagRules: TArray<TScriptParameterIncludesFlagRule>;
  public
    constructor Create(const ASectionName: String;
      const AParameters: TArray<TScriptParameterDefinition>;
      const AFlagIncludesRules: TArray<TScriptFlagIncludesRule>;
      const AParameterIncludesFlagRules: TArray<TScriptParameterIncludesFlagRule> = nil);
    function TryGetParameter(const AName: String;
      out ADefinition: TScriptParameterDefinition): Boolean;
    property SectionName: String read FSectionName;
    property Parameters: TArray<TScriptParameterDefinition> read FParameters;
    property FlagIncludesRules: TArray<TScriptFlagIncludesRule> read FFlagIncludesRules;
    property ParameterIncludesFlagRules: TArray<TScriptParameterIncludesFlagRule>
      read FParameterIncludesFlagRules;
  end;

function TryGetScriptSectionMetadata(const ASectionName: String;
  out AMetadata: TScriptSectionMetadata): Boolean;

implementation

uses
  SysUtils, Generics.Collections;

var
  SectionMetadataList: TObjectList<TScriptSectionMetadata>;

{ TScriptSectionMetadata }

constructor TScriptSectionMetadata.Create(const ASectionName: String;
  const AParameters: TArray<TScriptParameterDefinition>;
  const AFlagIncludesRules: TArray<TScriptFlagIncludesRule>;
  const AParameterIncludesFlagRules: TArray<TScriptParameterIncludesFlagRule>);
begin
  inherited Create;
  FSectionName := ASectionName;
  FParameters := AParameters;
  FFlagIncludesRules := AFlagIncludesRules;
  FParameterIncludesFlagRules := AParameterIncludesFlagRules;
end;

function TScriptSectionMetadata.TryGetParameter(const AName: String;
  out ADefinition: TScriptParameterDefinition): Boolean;
begin
  for var Parameter in FParameters do
    if SameText(Parameter.Name, AName) then begin
      ADefinition := Parameter;
      Exit(True);
    end;
  Result := False;
end;

function TryGetScriptSectionMetadata(const ASectionName: String;
  out AMetadata: TScriptSectionMetadata): Boolean;
begin
  for var Metadata in SectionMetadataList do
    if SameText(Metadata.SectionName, ASectionName) then begin
      AMetadata := Metadata;
      Exit(True);
    end;
  Result := False;
end;

initialization
  SectionMetadataList := TObjectList<TScriptSectionMetadata>.Create;
finalization
  SectionMetadataList.Free;
end.
