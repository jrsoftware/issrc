unit CompTypes;

{
  Inno Setup
  Copyright (C) 1997-2014 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Types and functions used by both IDE and ISCC units
}

interface

uses
  Windows, SysUtils, Registry, Classes, IsppBase, IsppIntf;

type
  TConfigIniFile = class(TRegIniFile)
  private
    FMutex: THandle;
    FAcquiredMutex: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

procedure PopulateOptions(var Options: TOptions; Symbol: Char);
function IsParam(const S: String): Boolean;
function FindParam(var Index: Integer; Symbols: String): String;
function GetParam(var S: String; Symbols: String): Boolean;
function ConvertOptionsToString(const Options: TOptions): String;
procedure AppendOption(var Opts: String; const OptName, OptValue: String);
procedure ReadSignTools(SignTools: TStringList);
function AddSignToolParam(Sign: string): string;
procedure InitIsppOptions(var Opt: TIsppOptions; var Definitions, IncludePath: String);
procedure IsppOptionsToString(var S: String; Opt: TIsppOptions; Definitions, IncludePath: String);

implementation

uses CmnFunc2;

procedure PopulateOptions(var Options: TOptions; Symbol: Char);
var
  I: Integer;
  S: String;
begin
  for I := 1 to NewParamCount do
  begin
    S := NewParamStr(I);
    if Length(S) = 4 then
      if ((S[1] = '/') or (S[1] = '-')) and (UpCase(S[2]) = Symbol) then
        case S[4] of
          '-': SetOption(Options, S[3], False);
          '+': SetOption(Options, S[3], True)
        else
          raise Exception.CreateFmt('Invalid command line option: %s', [S]);
        end;
  end;
end;

function IsParam(const S: String): Boolean;
begin
  Result := (Length(S) >= 2) and ((S[1] = '/') or (S[1] = '-'));
end;

function GetParam(var S: String; Symbols: String): Boolean;
begin
  Result := IsParam(S) and
    (CompareText(Copy(S, 2, Length(Symbols)), Symbols) = 0);
  if Result then
    S := Copy(S, 2 + Length(Symbols), MaxInt);
end;

function FindParam(var Index: Integer; Symbols: String): String;
var
  I: Integer;
  S: String;
begin
  for I := Index to NewParamCount do
  begin
    S := NewParamStr(I);
    if IsParam(S) and (CompareText(Copy(S, 2, Length(Symbols)), Symbols) = 0) then
    begin
      Result := Copy(S, 2 + Length(Symbols), MaxInt);
      Index := I + 1;
      Exit;
    end;
  end;
  Index := MaxInt;
  Result := '';
end;

function ConvertOptionsToString(const Options: TOptions): String;
var
  I: TOptionID;
begin
  Result := '';
  for I := 0 to 25 do
    if I in Options then
      Result := Result + Chr(Ord('a') + I);
end;

procedure AppendOption(var Opts: String; const OptName, OptValue: String);
begin
  Opts := Opts + OptName + '=' + OptValue + #0;
end;

procedure ReadSignTools(SignTools: TStringList);
var
  Ini: TConfigIniFile;
  I: Integer;
  S: String;
begin
  Ini := TConfigIniFile.Create;
  try
    { Sign tools }
    SignTools.Clear();
    I := 0;
    repeat
      S := Ini.ReadString('SignTools', 'SignTool' + IntToStr(I), '');
      if S <> '' then
        SignTools.Add(S);
      Inc(I);
    until S = '';
  finally
    Ini.Free;
  end;
end;

function AddSignToolParam(Sign: string): string;
begin
  Result := 'SignTool-' + Sign + #0;
end;

procedure InitIsppOptions(var Opt: TIsppOptions; var Definitions, IncludePath: String);
begin
  with Opt do
  begin
    SetOption(Options, 'C', True);
    SetOption(ParserOptions.Options, 'B', True);
    SetOption(ParserOptions.Options, 'P', True);
    VerboseLevel := 0;
    InlineStart := '{#';
    InlineEnd := '}';

    PopulateOptions(Options, '$');
    PopulateOptions(ParserOptions.Options, 'p');
  end;

  Definitions := 'ISPPCC_INVOKED';
  IncludePath := ExtractFileDir(NewParamStr(0));
end;

procedure IsppOptionsToString(var S: String; Opt: TIsppOptions; Definitions, IncludePath: String);
begin
  with Opt do
  begin
    AppendOption(S, 'ISPP:ParserOptions', ConvertOptionsToString(ParserOptions.Options));
    AppendOption(S, 'ISPP:Options', ConvertOptionsToString(Options));
    AppendOption(S, 'ISPP:VerboseLevel', IntToStr(VerboseLevel));
    AppendOption(S, 'ISPP:InlineStart', String(InlineStart));
    AppendOption(S, 'ISPP:InlineEnd', String(InlineEnd));
  end;

  AppendOption(S, 'ISPP:IncludePath', IncludePath);
  AppendOption(S, 'ISPP:Definitions', Definitions);
end;

{ TConfigIniFile }

constructor TConfigIniFile.Create;
begin
  inherited Create('Software\Jordan Russell\Inno Setup');
  { Paranoia: Use a mutex to prevent multiple instances from reading/writing
    to the registry simultaneously }
  FMutex := CreateMutex(nil, False, 'Inno-Setup-IDE-Config-Mutex');
  if FMutex <> 0 then
    if WaitForSingleObject(FMutex, INFINITE) <> WAIT_FAILED then
      FAcquiredMutex := True;
end;

destructor TConfigIniFile.Destroy;
begin
  if FMutex <> 0 then begin
    if FAcquiredMutex then
      ReleaseMutex(FMutex);
    CloseHandle(FMutex);
  end;
  inherited;
end;

end.
