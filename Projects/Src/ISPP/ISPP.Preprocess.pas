{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff
 
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

unit ISPP.Preprocess;

interface

uses
  Shared.PreprocInt;

function ISPreprocessScript(var Params: TPreprocessScriptParams): Integer; stdcall;

implementation

uses
  SysUtils, Shared.CommonFunc, PathFunc,
  ISPP.Base, ISPP.Preprocessor, ISPP.Sessions, ISPP.Intf,
  ISPP.IdentMan, ISPP.VarUtils, ISPP.Consts;

procedure ReadScript(const Params: TPreprocessScriptParams;
  const Preprocessor: TPreprocessor);
var
  I: Integer;
  LineText: PChar;
  LineTextStr: String;
begin
  I := 0;
  while True do
  begin
    LineText := Params.LineInProc(Params.CompilerData, 0, I);
    if LineText = nil then
      Break;
    LineTextStr := LineText;
    Preprocessor.QueueLine(LineTextStr);
    Inc(I);
  end;
end;

function CleanupProc(CleanupProcData: Pointer): Integer; stdcall;
begin
  if PopPreproc = nil then
    Result := 0
  else
    Result := 1;  { should never get here }
end;

function DecodeStringOptions(const S: String; var Options: TOptions): Boolean;
var
  I: Integer;
begin
  Options := [];
  Result := True;
  for I := 1 to Length(S) do begin
    case S[I] of
      'a'..'z': Include(Options, Ord(S[I]) - Ord('a'));
    else
      Result := False;
    end;
  end;
end;

function ISPreprocessScript(var Params: TPreprocessScriptParams): Integer; stdcall;
const
  Delta = Ord('a');
  DefaultOptions: TIsppOptions =
    (ParserOptions: (Options: [Ord('b') - Delta, Ord('p') - Delta]);
     Options: [Ord('c') - Delta, Ord('e') - Delta]; VerboseLevel: 0;
      InlineStart: '{#'; InlineEnd: '}'; SpanSymbol: #0);
var
  ISPPOptions: TIsppOptions;
  Definitions, IncludePath, IncludeFiles: String;
  Preprocessor: TPreprocessor;
  V: TIsppVariant;

  function ParseOption(const OptName, OptValue: String): Boolean;
  begin
    Result := True;
    if OptName = 'ISPP:ParserOptions' then
      Result := DecodeStringOptions(OptValue, ISPPOptions.ParserOptions.Options)
    else if OptName = 'ISPP:Options' then
      Result := DecodeStringOptions(OptValue, ISPPOptions.Options)
    else if OptName = 'ISPP:VerboseLevel' then
      ISPPOptions.VerboseLevel := StrToIntDef(OptValue, 0)
    else if OptName = 'ISPP:InlineStart' then
      ISPPOptions.InlineStart := OptValue
    else if OptName = 'ISPP:InlineEnd' then
      ISPPOptions.InlineEnd := OptValue
    else if OptName = 'ISPP:Definitions' then
      Definitions := OptValue
    else if OptName = 'ISPP:IncludePath' then
      IncludePath := OptValue
    else if OptName = 'ISPP:IncludeFiles' then
      IncludeFiles := OptValue
    else
      Result := False;
  end;

  function ParseOptions(P: PChar): Boolean;
  var
    EqPos: PChar;
    OptName: String;
  begin
    Result := True;
    if P = nil then
      Exit;
    while P^ <> #0 do begin
      EqPos := StrScan(P, '=');
      if EqPos = nil then begin
        Result := False;
        Break;
      end;
      SetString(OptName, P, EqPos - P);
      P := EqPos + 1;
      if not ParseOption(OptName, P) then begin
        Result := False;
        Break;
      end;
      Inc(P, StrLen(P) + 1);
    end;
  end;

  function ParseDefinitions(Definitions: PChar; VarMan: TIdentManager): Boolean;

    procedure ParseDefinition(const S: string);
    var
      I: Integer;
      Name, V: string;
      Value: TIsppVariant;
    begin
      Value := NULL;
      I := Pos('=', S);
      if I > 0 then
      begin
        Name := Copy(S, 1, I - 1);
        V := Copy(S, I + 1, MaxInt);
        if V <> '' then
          MakeStr(Value, V)
      end
      else
        Name := Trim(S);
      VarMan.DefineVariable(Name, -1, Value, dsPublic);
    end;

  var
    DelimPos: PChar;
    N: Integer;
    Definition: string;
  begin
    Result := True;
    while Definitions^ <> #0 do begin
      DelimPos := StrScan(Definitions, #1);
      if DelimPos = nil then begin
        Result := False;
        Break;
      end;
      N := DelimPos - Definitions;
      if N > 0 then begin
        SetString(Definition, Definitions, N);
        ParseDefinition(Definition);
      end;
      Inc(Definitions, N + 1);
    end;
  end;

  function IncludeBuiltinsAndParseIncludeFiles(BuiltinsDir: String; IncludeFiles: PChar; Options: TOptions): Boolean;

    function Escape(const S: string): string;
    var
      I: Integer;
    begin
      Result := '';
      for I := 1 to Length(S) do
      begin
        Result := Result + S[I];
        if S[I] = '\' then Result := Result + '\';
      end;
    end;

    procedure Include(FileName: String; Builtins: Boolean);
    begin
      if not GetOption(Options, 'P') then
        FileName := Escape(FileName);
      Preprocessor.IncludeFile(FileName, Builtins, False, True);
    end;

  const
    SBuiltins = 'ISPPBuiltins.iss';
  var
    DelimPos: PChar;
    N: Integer;
    IncludeFile: String;
  begin
    Result := True;
    IncludeFile := BuiltinsDir + SBuiltins;
    if FileExists(IncludeFile) then
      Include(IncludeFile, True)
    else
      Preprocessor.WarningMsg(SFileNotFound, [SBuiltins]);
    while IncludeFiles^ <> #0 do begin
      DelimPos := StrScan(IncludeFiles, #1);
      if DelimPos = nil then begin
        Result := False;
        Break;
      end;
      N := DelimPos - IncludeFiles;
      if N > 0 then begin
        SetString(IncludeFile, IncludeFiles, N);
        Include(IncludeFile, False);
      end;
      Inc(IncludeFiles, N + 1);
    end;
  end;

var
  SourcePath, CompilerPath, LineFilename, LineText: string;
  LineNumber: Integer;
begin
  if (Params.Size <> SizeOf(Params)) or
     (Params.InterfaceVersion <> 3) then
  begin
    Result := ispeInvalidParam;
    Exit;
  end;

  SourcePath := Params.SourcePath;
  CompilerPath := Params.CompilerPath;

  ISPPOptions := DefaultOptions;
  Definitions := '';
  IncludePath := RemoveBackslashUnlessRoot(CompilerPath);
  IncludeFiles := '';
  if not ParseOptions(Params.Options) then
  begin
    Result := ispeInvalidParam;
    Exit;
  end;

  { Hack: push a dummy item onto the stack to defer deletion of temp. files }
  PushPreproc(nil);
  try
    Preprocessor := TPreprocessor.Create(Params, nil, ISPPOptions, SourcePath,
      CompilerPath, Params.Filename);
    try
      Preprocessor.IncludePath := IncludePath;

      MakeStr(V, SourcePath);
      Preprocessor.VarMan.DefineVariable('SourcePath', -1, V, dsPublic);

      MakeStr(V, CompilerPath);
      Preprocessor.VarMan.DefineVariable('CompilerPath', -1, V, dsPublic);

      MakeInt(V, Params.CompilerBinVersion);
      Preprocessor.VarMan.DefineVariable('Ver', -1, V, dsPublic);

      if not ParseDefinitions(PChar(Definitions), Preprocessor.VarMan) or
         not IncludeBuiltinsAndParseIncludeFiles(Params.CompilerPath, PChar(IncludeFiles),
           Preprocessor.FOptions.ParserOptions.Options) then
      begin
        Result := ispeInvalidParam;
        Exit;
      end;

      ReadScript(Params, Preprocessor);
      Preprocessor.Stack.Resolved;

      if not GetOption(Preprocessor.FOptions.Options, 'C') then
        Result := ispeSilentAbort
      else
      begin
        Preprocessor.GetNextOutputLineReset;
        while Preprocessor.GetNextOutputLine(LineFilename, LineNumber, LineText) do
          Params.LineOutProc(Params.CompilerData, PChar(LineFilename),
            LineNumber, PChar(LineText));
        Result := ispeSuccess;
      end;
    finally
      Preprocessor.Free;
    end;
  except
    on E: EPreprocError do {preprocessor (syntax most likely) error}
    begin
      Params.ErrorProc(Params.CompilerData, PChar(E.Message),
        PChar(E.FileName), E.LineNumber, E.ColumnNumber);
      Result := ispePreprocessError;
    end;
    on E: Exception do
    begin
      Params.ErrorProc(Params.CompilerData,
        PChar(Format('Unexpected exception of class %s in ISPP.' +
        #13#10#13#10'%s.', [E.ClassName, E.Message])), nil, 0, 0);
      Result := ispePreprocessError;
    end;
  end;
  if Result = ispeSuccess then
    Params.PreprocCleanupProc := CleanupProc
  else
    PopPreproc;
end;

end.
