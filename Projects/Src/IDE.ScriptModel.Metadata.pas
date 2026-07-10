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
  TScriptParameterValueKind = (pvkString, pvkInteger, pvkVersion,
    pvkChoice, pvkFlags);

  TScriptParameterDefinition = record
    Name: String;
    ValueKind: TScriptParameterValueKind;
    KnownValues: TArray<String>; { For pvkFlags: the known tokens; for pvkChoice: the known single values }
    Obsolete: Boolean;      { Documented as obsolete: to be hidden unless explicitly present in the script }
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
  for var Parameter in FParameters do begin
    if SameText(Parameter.Name, AName) then begin
      ADefinition := Parameter;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TryGetScriptSectionMetadata(const ASectionName: String;
  out AMetadata: TScriptSectionMetadata): Boolean;
begin
  for var Metadata in SectionMetadataList do begin
    if SameText(Metadata.SectionName, ASectionName) then begin
      AMetadata := Metadata;
      Exit(True);
    end;
  end;
  Result := False;
end;

function PD(const AName: String; const AValueKind: TScriptParameterValueKind;
  const AKnownValues: TArray<String> = nil;
  const AObsolete: Boolean = False): TScriptParameterDefinition;
begin
  Result.Name := AName;
  Result.ValueKind := AValueKind;
  Result.KnownValues := AKnownValues;
  Result.Obsolete := AObsolete;
end;

function FIR(const AParameterName, AFlagName: String;
  const AAlsoIncludedFlagNames: TArray<String>): TScriptFlagIncludesRule;
begin
  Result.ParameterName := AParameterName;
  Result.FlagName := AFlagName;
  Result.AlsoIncludedFlagNames := AAlsoIncludedFlagNames;
end;

function PIF(const AParameterName, AFlagParameterName,
  AFlagName: String): TScriptParameterIncludesFlagRule;
begin
  Result.ParameterName := AParameterName;
  Result.FlagParameterName := AFlagParameterName;
  Result.FlagName := AFlagName;
end;

procedure InitializeSectionMetadata;
begin
  { Parameter names match TInnoSetupStyler's per-section lists, value kinds
    follow the documentation in ISHelp\isetup.xml }

  const AttribsFlagNames: TArray<String> = ['readonly', 'hidden', 'system',
    'notcontentindexed'];

  const FilesFlagNames: TArray<String> = [
    '32bit', '64bit', 'allowunsafefiles', 'comparetimestamp', 'confirmoverwrite',
    'createallsubdirs', 'deleteafterinstall', 'dontcopy', 'dontverifychecksum', 'download',
    'external', 'extractarchive', 'fontisnttruetype', 'gacinstall', 'ignoreversion',
    'isreadme', 'issigverify', 'nocompression', 'noencryption', 'notimestamp', 'noregerror',
    'onlyifdestfileexists', 'onlyifdoesntexist', 'overwritereadonly', 'promptifolder',
    'recursesubdirs', 'regserver', 'regtypelib', 'replacesameversion', 'restartreplace',
    'setntfscompression', 'sharedfile', 'sign', 'signcheck', 'signonce',
    'skipifsourcedoesntexist', 'solidbreak', 'sortfilesbyextension',
    'sortfilesbyname', 'touch', 'uninsnosharedfileprompt', 'uninsremovereadonly',
    'uninsrestartdelete', 'uninsneveruninstall', 'unsetntfscompression'];

  SectionMetadataList.Add(TScriptSectionMetadata.Create('Files',
    [PD('AfterInstall', pvkString),
     PD('Attribs', pvkFlags, AttribsFlagNames),
     PD('BeforeInstall', pvkString),
     PD('Check', pvkString),
     PD('Components', pvkString),
     PD('CopyMode', pvkString, nil, True),
     PD('DestDir', pvkString),
     PD('DestName', pvkString),
     PD('DownloadISSigSource', pvkString),
     PD('DownloadPassword', pvkString),
     PD('DownloadUserName', pvkString),
     PD('Excludes', pvkString),
     PD('ExternalSize', pvkInteger),
     PD('ExtractArchivePassword', pvkString),
     PD('Flags', pvkFlags, FilesFlagNames),
     PD('FontInstall', pvkString),
     PD('Hash', pvkString),
     PD('ISSigAllowedKeys', pvkString),
     PD('Languages', pvkString),
     PD('MinVersion', pvkVersion),
     PD('OnlyBelowVersion', pvkVersion),
     PD('Permissions', pvkString),
     PD('Source', pvkString),
     PD('StrongAssemblyName', pvkString),
     PD('Tasks', pvkString)],
    [FIR('Flags', 'extractarchive', ['external', 'ignoreversion']),
     FIR('Flags', 'download', ['external', 'ignoreversion']),
     FIR('Flags', 'createallsubdirs', ['recursesubdirs']),
     FIR('Flags', 'dontverifychecksum', ['nocompression']),
     FIR('Flags', 'uninsnosharedfileprompt', ['sharedfile'])],
    [PIF('ExternalSize', 'Flags', 'external'),
     PIF('ISSigAllowedKeys', 'Flags', 'issigverify')]));
end;

initialization
  SectionMetadataList := TObjectList<TScriptSectionMetadata>.Create;
  InitializeSectionMetadata;
finalization
  SectionMetadataList.Free;
end.
