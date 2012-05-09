{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff
  $Id: IsppPreprocess.pas,v 1.3 2010/12/30 15:26:34 mlaan Exp $
}

unit IsppPreprocess;

interface

uses
  CompPreprocInt;

function ISPreprocessScript(var Params: TPreprocessScriptParams): Integer; stdcall;

implementation

uses
  SysUtils, CmnFunc2, PathFunc,
  IsppBase, IsppTranslate, IsppSessions, IsppIntf, IsppIdentMan, IsppVarUtils;

//type TPreprocProtectedMethods = class(TPreprocessor);

var
  BuiltinsDir: string;

procedure ReadScript(const Params: TPreprocessScriptParams;
  const Preprocessor: TPreprocessor);

  procedure BuiltIns;

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

  const
    SBuiltins = 'ISPPBuiltins.iss';
  var
    BuiltinsFileName: string;
  begin
    if FileExists(BuiltinsDir + SBuiltins) then
    begin
      BuiltinsFileName := BuiltinsDir + SBuiltins;
      if not GetOption(Preprocessor.FOptions.ParserOptions.Options, 'P') then
        BuiltinsFileName := Escape(BuiltinsFileName);
      Preprocessor.IncludeFile(BuiltinsFileName, False);
      //Preprocessor.IncludeFile('D:\PROGRA~1\INNOSE~1\BUILTINS.ISS', False);
      {TPreprocProtectedMethods(Preprocessor).InternalAddLine
        (Format('#include "%s"', [BuiltinsFileName]), 0, Word(-1), False)}
    end
    else
      Preprocessor.IssueMessage(SBuiltins + ' file was not found', imtWarning);
  end;

var
  I: Integer;
  LineText: PChar;
  LineTextStr: String;
begin
  BuiltIns;
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
  IncludePath, Definitions: string;
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
      ISPPOptions.InlineStart := AnsiString(OptValue)
    else if OptName = 'ISPP:InlineEnd' then
      ISPPOptions.InlineEnd := AnsiString(OptValue)
    else if OptName = 'ISPP:IncludePath' then
      IncludePath := OptValue
    else if OptName = 'ISPP:Definitions' then
      Definitions := OptValue
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

  procedure ParseDefinitions(Definitions: PChar; VarMan: TIdentManager);

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

  const
    QuoteChar = Char('"');
    Delimiter = Char(';');
  var
    P: PChar;
    S: string;
  begin
    if Definitions = nil then Exit;
    while CharInSet(Definitions^, [#1..' ']) do Inc(Definitions);
    while Definitions^ <> #0 do
    begin
      if Definitions^ = QuoteChar then
        S := AnsiExtractQuotedStr(Definitions, QuoteChar)
      else
      begin
        P := Definitions;
        while (Definitions^ > ' ') and (Definitions^ <> Delimiter) do Inc(Definitions);
        SetString(S, P, Definitions - P);
      end;
      ParseDefinition(S);
      while CharInSet(Definitions^, [#1..' ']) do Inc(Definitions);
      if Definitions^ = Delimiter then
      repeat
        Inc(Definitions);
      until not CharInSet(Definitions^, [#1..' ']);
    end;
  end;

var
  SourcePath, CompilerPath, LineFilename, LineText: string;
  LineNumber: Integer;
begin
  if (Params.Size <> SizeOf(Params)) or
     (Params.InterfaceVersion <> 1) then
  begin
    Result := ispeInvalidParam;
    Exit;
  end;

  SourcePath := Params.SourcePath;
  CompilerPath := Params.CompilerPath;

  ISPPOptions := DefaultOptions;
  IncludePath := RemoveBackslashUnlessRoot(CompilerPath);
  Definitions := '';
  if not ParseOptions(Params.Options) then
  begin
    Result := ispeInvalidParam;
    Exit;
  end;

  BuiltinsDir := Params.CompilerPath;
  { Hack: push a dummy item onto the stack to defer deletion of temp. files }
  PushPreproc(nil);
  try
    Preprocessor := TPreprocessor.Create(Params, nil, ISPPOptions, SourcePath,
      CompilerPath, Params.Filename);
    try
      Preprocessor.IssueMessage('Preprocessing', imtStatus);

      Preprocessor.IncludePath := IncludePath;

      MakeStr(V, SourcePath);
      Preprocessor.VarMan.DefineVariable('SourcePath', -1, V, dsPublic);

      MakeStr(V, CompilerPath);
      Preprocessor.VarMan.DefineVariable('CompilerPath', -1, V, dsPublic);

      MakeInt(V, Params.CompilerBinVersion);
      Preprocessor.VarMan.DefineVariable('Ver', -1, V, dsPublic);

      ParseDefinitions(PChar(Definitions), Preprocessor.VarMan);

      ReadScript(Params, Preprocessor);
      Preprocessor.Stack.Resolved;
      Preprocessor.IssueMessage('Preprocessed', imtStatus);
      Preprocessor.IssueMessage('', imtBlank);

      if not GetOption(Preprocessor.FOptions.Options, 'C') then
        Result := ispeSilentAbort
      else
      begin
        while Preprocessor.GetNext(LineFilename, LineNumber, LineText) do
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
      Params.ErrorProc(Params.CompilerData, PChar('[ISPP] ' + E.Message),
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
