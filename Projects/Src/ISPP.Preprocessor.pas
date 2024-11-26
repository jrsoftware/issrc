{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff
  
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

unit ISPP.Preprocessor;

interface

uses
  Windows, SysUtils, Classes, Shared.PreprocInt, IniFiles, Registry, ISPP.Intf,
  ISPP.Base, ISPP.Stack, ISPP.IdentMan, ISPP.Parser;

type

  TPreprocessor = class;

  EPreprocError = class(Exception)
    FileName: string;
    LineNumber: Integer;
    ColumnNumber: Integer;
    constructor Create(Preproc: TPreprocessor; const Msg: string);
  end;

  TConditionalBlockInfo = packed record
    BlockState, Fired, HadElse, Reserved: Boolean;
  end;

  TConditionalVerboseMsg = (cvmIf, cvmElif, cvmElse, cvmEndif);

  TConditionalTranslationStack = class(TStack)
  private
    FPreproc: TPreprocessor;
    FCache: Boolean;
    FCacheValid: Boolean;
    procedure VerboseMsg(Msg: TConditionalVerboseMsg; Eval: Boolean);
  protected
    function Last: TConditionalBlockInfo;
    procedure UpdateLast(const Value: TConditionalBlockInfo);
  public
    constructor Create(Preproc: TPreprocessor);
    procedure IfInstruction(Eval: Boolean);
    procedure ElseIfInstruction(Eval: Boolean);
    procedure ElseInstruction;
    procedure EndIfInstruction;
    function Include: Boolean;
    procedure Resolved;
  end;

  TPreprocessorCommand = (pcError, pcIf, pcIfDef, pcIfNDef, pcIfExist,
    pcIfNExist, pcElseIf, pcElse, pcEndIf, pcDefine, pcUndef, pcInclude,
    pcErrorDir, pcPragma, pcLine, pcImport, pcPrint, pcPrintEnv, pcFile,
    pcExecute, pcGlue, pcEndGlue, pcDim, pcProcedure, pcEndProc, pcEndLoop,
    pcFor, pcReDim);

  TDropGarbageProc = procedure(Item: Pointer);

  TIsppMessageType = (imtStatus, imtWarning);

  TPreprocessor = class(TObject, IIdentManager)
  private
    FCompilerParams: TPreprocessScriptParams;
    FCompilerPath: string;
    FCounter: Integer;
    FCurrentFile: Word;
    FCurrentLine: Word;
    FDefaultScope: TDefineScope;
    FFileStack: TStringList;   { strs: files being included }
    FIncludes: TStringList;     { strs: files been included, for error msgs }
    FIncludePath: string;
    FInsertionPoint: Integer;
    FLinePointer: Integer;
    FMainCounter: Word;
    FOutput: TStringList;       { strs: translation }
    FQueuedLine: string;
    FQueuedLineCount: Integer;
    FSourcePath: string;
    FStack: TConditionalTranslationStack;
    FIdentManager: TIdentManager;
    FInProcBody: Boolean;
    FInForBody: Boolean;
    FProcs: TStringList;
    FGarbageCollection: TList;
    procedure DropGarbage;
    function ProcessInlineDirectives(P: PChar): string;
    function ProcessPreprocCommand(Command: TPreprocessorCommand;
      var Params: string; ParamsOffset: Integer): Boolean;
    procedure PushFile(const FileName: string);
    procedure PopFile;
    function CheckFile(const FileName: string): Boolean;
    function EmitDestination: TStringList;
    procedure SendMsg(Msg: string; Typ: TIsppMessageType);
    function GetFileName(Code: Integer): string;
    function GetLineNumber(Code: Integer): Word;
    procedure RaiseErrorEx(const Message: string; Column: Integer);
    procedure ExecProc(Body: TStrings);
  protected
    function GetDefaultScope: TDefineScope;
    procedure SetDefaultScope(Scope: TDefineScope);
    procedure InternalAddLine(const LineRead: string; FileIndex, LineNo: Word;
      NonISS: Boolean);
    function InternalQueueLine(const LineRead: string; FileIndex, LineNo: Word;
      NonISS: Boolean): Integer;
    function ParseFormalParams(Parser: TParser; var ParamList: PParamList): Integer;
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IIdentManager }
    function LookupPredefined(Name: string; Value: PIsppVariant): Boolean;
    function Defined(const Name: String): Boolean;
    function GetIdent(const Name: String;
      out CallContext: ICallContext): TIdentType;
    function TypeOf(const Name: String): Byte;
    function DimOf(const Name: String): Integer;
  public
    FOptions: TISPPOptions;
    constructor Create(const CompilerParams: TPreprocessScriptParams;
      VarManager: TIdentManager; const Options: TIsppOptions;
      const SourcePath: string; const CompilerPath: string; const FileName: string = '');
    destructor Destroy; override;
    procedure CallIdleProc;
    procedure VerboseMsg(Level: Byte; const Msg: string); overload;
    procedure VerboseMsg(Level: Byte; const Msg: string; const Args: array of const); overload;
    procedure StatusMsg(const Msg: string); overload;
    procedure StatusMsg(const Msg: string; const Args: array of const); overload;
    procedure WarningMsg(const Msg: string); overload;
    procedure WarningMsg(const Msg: string; const Args: array of const); overload;
    function GetNextOutputLine(var LineFilename: string; var LineNumber: Integer;
      var LineText: string): Boolean;
    procedure GetNextOutputLineReset;
    procedure IncludeFile(FileName: string; Builtins, UseIncludePathOnly, ResetCurrentFile: Boolean);
    procedure QueueLine(const LineRead: string);
    function PrependDirName(const FileName, Dir: string): string;
    procedure RegisterFunction(const Name: string; Handler: TIsppFunction; Ext: Longint);
    procedure RaiseError(const Message: string);
    procedure SaveToFile(const FileName: string);
    procedure CollectGarbage(Item: Pointer; Proc: TDropGarbageProc);
    procedure UncollectGarbage(Item: Pointer);
    property IncludedFiles: TStringList read FIncludes;
    property IncludePath: string read FIncludePath write FIncludePath;
    property SourcePath: string read FSourcePath;
    property StringList: TStringList read FOutput;
    property Stack: TConditionalTranslationStack read FStack;
    property VarMan: TIdentManager read FIdentManager;
  end;

implementation

uses
  ISPP.Consts, ISPP.Funcs, ISPP.VarUtils, ISPP.Sessions, ISPP.CTokenizer, PathFunc,
  Shared.CommonFunc, Shared.FileClass, Shared.Struct;

const
  PreprocCommands: array[TPreprocessorCommand] of String =
    ('', 'if', 'ifdef', 'ifndef', 'ifexist', 'ifnexist', 'elif', 'else',
    'endif', 'define', 'undef', 'include', 'error', 'pragma', 'line', 'import',
    'emit', 'env', 'file', 'expr', 'insert', 'append', 'dim', 'sub', 'endsub',
    'endloop', 'for', 'redim');
  PpCmdSynonyms: array[TPreprocessorCommand] of Char =
    (#0, '?', #0, #0, #0, #0, #0, '^', '.', ':', #0, '+', #0, #0, #0, #0,
    '=', '%', #0, '!', #0, #0, #0, #0, #0, #0, #0, #0);

function GetEnv(const EnvVar: String): String;

  function AdjustLength(var S: String; const Res: Cardinal): Boolean;
  begin
    Result := Integer(Res) < Length(S);
    SetLength (S, Res);
  end;

var
  Res: DWORD;
begin
  SetLength(Result, 255);
  repeat
    Res := GetEnvironmentVariable(PChar(EnvVar), PChar(Result), Length(Result));
    if Res = 0 then begin
      Result := '';
      Break;
    end;
  until AdjustLength(Result, Res);
end;

function ParsePreprocCommand(var P: PChar; ExtraTerminator: Char): TPreprocessorCommand;
begin
  for Result := TPreprocessorCommand(1) to High(TPreprocessorCommand) do
  begin
    if (P^ = PpCmdSynonyms[Result]) then
      Inc(P)
    else if (StrLIComp(P, @PreprocCommands[Result][1], Length(PreprocCommands[Result])) = 0) and
      CharInSet(P[Length(PreprocCommands[Result])], [#0..#32, ExtraTerminator]) then
      Inc(P, Length(PreprocCommands[Result]))
    else
      Continue;
    Exit;
  end;
  if StrLIComp('echo', P, 4) = 0 then
  begin
    Result := pcPrint;
    Inc(P, 4)
  end
  else if StrLIComp('call', P, 4) = 0 then
  begin
    Result := pcExecute;
    Inc(P, 4);
  end
  else
    Result := pcError;
end;

{ EPreprocError }

constructor EPreprocError.Create(Preproc: TPreprocessor; const Msg: string);
begin
  inherited Create(Msg + '.');
  FileName := Preproc.GetFileName(-1);
  LineNumber := Preproc.GetLineNumber(-1);
end;

{ TPreprocessor }

function CheckReservedIdent(const Ident: string): string;
begin
  Result := UpperCase(Ident);
  if (Result = SLocal) or
    (Result = SGlobal) or
    (Result = SInt) or
    (Result = SStr) or
    (Result = SAny) then
    raise EParsingError.CreateFmt(SExpectedButFound, [SIdent, '''' + Result + '''']);
  Result := Ident;
end;

constructor TPreprocessor.Create(const CompilerParams: TPreprocessScriptParams;
  VarManager: TIdentManager; const Options: TIsppOptions;
  const SourcePath, CompilerPath, FileName: string);
begin
  PushPreproc(Self);
  if VarManager = nil then
    FIdentManager := TIdentManager.Create(Self, Longint(Self))
  else
    FIdentManager := VarManager;
  FOptions := Options;
  FIdentManager._AddRef;
  FIdentManager.BeginLocal;
  FCompilerParams := CompilerParams;
  FCompilerPath := CompilerPath;
  FSourcePath := SourcePath;
  FFileStack := TStringList.Create;
  FIncludes := TStringList.Create;
  FIncludes.Add(FileName);  //main file - no name
  FInsertionPoint := -1;
  FOutput := TStringList.Create;
  FProcs := TStringList.Create;
  FStack := TConditionalTranslationStack.Create(Self);
  if VarManager = nil then ISPP.Funcs.RegisterFunctions(Self);
end;

destructor TPreprocessor.Destroy;
begin
  DropGarbage;
  if PopPreproc <> Self then
    RaiseError('Internal error: FSP');
  FStack.Free;
  FProcs.Free;
  FOutput.Free;
  FIncludes.Free;
  if FFileStack.Count <> 0 then
    RaiseError('Internal error: FNE');
  FFileStack.Free;
  FIdentManager.EndLocal;
  FIdentManager._Release;
end;

function TPreprocessor.GetFileName(Code: Integer): string;
begin
  if Code = -1 then
    Result := FIncludes[FCurrentFile]
  else
    Result := FIncludes[Longint(FOutput.Objects[Code]) shr 16];
end;

function TPreprocessor.GetLineNumber(Code: Integer): Word;
begin
  if Code = -1 then
    Result := FCurrentLine
  else
    Result := Word(FOutput.Objects[Code]) and $FFFF
end;

function TPreprocessor.GetNextOutputLine(var LineFilename: string; var LineNumber: Integer;
  var LineText: string): Boolean;
begin
  Result := False;
  if FLinePointer < FOutput.Count then
  begin
    LineFilename := GetFileName(FLinePointer);
    LineNumber := GetLineNumber(FLinePointer);
    LineText := FOutput[FLinePointer];
    Inc(FLinePointer);
    Result := True;
  end;
end;

procedure TPreprocessor.GetNextOutputLineReset;
begin
  FLinePointer := 0;
end;

procedure TPreprocessor.InternalAddLine(const LineRead: string; FileIndex, LineNo: Word;
  NonISS: Boolean);
var
  IncludeLine: Boolean;
  P, P1: PChar;
  Command: TPreprocessorCommand;
  DirectiveOffset: Integer;
  State: Boolean;
  S, S1: string;
begin
  try
    Inc(LineNo);
    FCurrentFile := FileIndex;
    FCurrentLine := LineNo;
    P := PChar(LineRead);
    IncludeLine := True;
    if P^ <> #0 then
    begin
      P1 := P;
      while CharInSet(P^, [#1..#32]) do Inc(P);
      if P^ = '#' then
      begin
        Inc(P);
        while CharInSet(P^, [#1..#32]) do Inc(P);
        IncludeLine := FInProcBody;
        Command := ParsePreprocCommand(P, #0);
        if FInProcBody then
        begin
          case Command of
            pcError: RaiseError(SUnknownPreprocessorDirective);
            pcProcedure: RaiseError('Nested procedure declaration not allowed');
            pcEndProc:
              begin
                S := P;
                ProcessPreprocCommand(Command, S, P - P1);
                IncludeLine := False;
              end
          else
            S := LineRead;
          end;
        end
        else
        begin
          State := FStack.Include;
          DirectiveOffset := P - P1;
          //S := Copy(LineRead, DirectiveOffset + 1, MaxInt);
          S := P;
          case Command of
            pcIf..pcIfNExist:
              FStack.IfInstruction(FStack.Include and
                ProcessPreprocCommand(Command, S, DirectiveOffset));
            pcElseIf:
              FStack.ElseIfInstruction(FStack.Last.Fired or
                (FStack.Include or not FStack.Last.BlockState) and
                ProcessPreprocCommand(Command, S, DirectiveOffset));
            pcElse: FStack.ElseInstruction;
            pcEndIf: FStack.EndIfInstruction
            else
              if State then
                case Command of
                  pcPrint, pcPrintEnv:
                    begin
                      ProcessPreprocCommand(Command, S, DirectiveOffset);
                      VerboseMsg(8, SLineEmitted, [S]);
                      IncludeLine := True
                    end;
                  pcFile: RaiseError(SFileDirectiveCanBeOnlyInline);
                else
                  ProcessPreprocCommand(Command, S, DirectiveOffset);
                end;
          end
        end;
      end
      else
        if not FInProcBody and not FStack.Include then
          IncludeLine := False
        else
          if ((P^ = '/') and (P[1] = '/')) or
             ((P^ = #0) and not (optEmitEmptyLines in FOptions.Options)) then //P^ is #0 if the line was all whitespace
            IncludeLine := False
          else
            if (P^ <> #0) and (P^ <> ';') and not FInProcBody then
              S := PChar(ProcessInlineDirectives(P1))
            else
              S := P1;
    end
    else
    begin
      S := '';
      IncludeLine := optEmitEmptyLines in FOptions.Options
    end;
    if IncludeLine then
    begin
      P := PChar(S);
      repeat
        P1 := P;
        while not CharInSet(P^, [#0, #10, #13]) do Inc(P);
        SetString(S1, P1, P - P1);
        if FInsertionPoint >= 0 then
        begin
          EmitDestination.InsertObject(FInsertionPoint, S1,
            TObject(FileIndex shl 16 or LineNo));
          Inc(FInsertionPoint);
        end
        else
          EmitDestination.AddObject(S1, TObject(FileIndex shl 16 or LineNo));
        while CharInSet(P^, [#10, #13]) do Inc(P);
      until P^ = #0;
    end;
  except
    on E: EParsingError do
      RaiseErrorEx(E.Message, E.Position);
    on E: EPreprocError do
      raise;
    on E: Exception do
      RaiseError(E.Message);
  end;
end;

function TPreprocessor.ProcessInlineDirectives(P: PChar): string;
var
  S: string;
  Command: TPreprocessorCommand;
  LineStack: TConditionalTranslationStack;
  LineStart, P1, DStart, DEnd: PChar;

  function ScanForInlineStart(var P, D: PChar): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    while P^ <> #0 do
    begin
      if P^ = FOptions.InlineStart[1] then
      begin
        D := P;
        Result := True;
        for I := 2 to Length(FOptions.InlineStart) do
        begin
          Inc(D);
          if D^ <> FOptions.InlineStart[I] then
          begin
            Result := False;
            Break;
          end;
        end;
        Inc(D);
      end;
      if Result then Break;
      Inc(P);
    end;
  end;

  function ScanForInlineEnd(var P: PChar): PChar;
  var
    I: Integer;
  begin
    Result := nil;
    while P^ <> #0 do
    begin
      if P^ = FOptions.InlineEnd[1] then
      begin
        Result := P;
        for I := 2 to Length(FOptions.InlineEnd) do
        begin
          Inc(P);
          if P^ <> FOptions.InlineEnd[I] then
          begin
            Result := nil;
            Break;
          end;
        end;
        Inc(P);
      end;
      if Result <> nil then Exit;
      Inc(P);
    end;
    RaiseError(SUnterminatedPreprocessorDirectiv);
  end;

begin
  LineStack := TConditionalTranslationStack.Create(Self);
  try
    Result := '';
    LineStart := P;
    P1 := P;
    while ScanForInlineStart(P, DStart) do
    begin
      SetString(S, P1, P - P1);
      if LineStack.Include then Result := Result + S;
      Command := ParsePreprocCommand(DStart, Char(FOptions.InlineEnd[1]));
      if Command = pcError then
        Command := pcPrint;
      DEnd := DStart;
      SetString(S, DStart, ScanForInlineEnd(DEnd) - DStart);

      case Command of
        pcError: RaiseError(SUnknownPreprocessorDirective);
        pcIf..pcIfNExist:
          LineStack.IfInstruction(LineStack.Include and
            ProcessPreprocCommand(Command, S, DStart - LineStart));
        pcElseIf:
          LineStack.ElseIfInstruction(LineStack.Last.Fired or
            (LineStack.Include or not LineStack.Last.BlockState) and
            ProcessPreprocCommand(Command, S, DStart - LineStart));
        pcElse: LineStack.ElseInstruction;
        pcEndIf: LineStack.EndIfInstruction;
      else
        if LineStack.Include then
          case Command of
            pcInclude, pcGlue..pcEndLoop:
              RaiseError(Format(SDirectiveCannotBeInline,
                [PreprocCommands[Command]]));
            pcPrint, pcPrintEnv, pcFile:
              begin
                ProcessPreprocCommand(Command, S, DStart - LineStart);
                Result := Result + S;
              end;
            else
              ProcessPreprocCommand(Command, S, DStart - LineStart)
          end;
      end;
      P1 := DEnd;
      P := DEnd;
      //Inc(P);
    end;
    Result := Result + P1;
    LineStack.Resolved;
  finally
    LineStack.Free
  end;
end;

function TPreprocessor.GetDefaultScope: TDefineScope;
begin
  if FFileStack.Count > 0 then
    Result := TDefineScope(FFileStack.Objects[FFileStack.Count - 1])
  else
    Result := FDefaultScope;
end;

procedure TPreprocessor.SetDefaultScope(Scope: TDefineScope);
begin
  if Scope = dsAny then Scope := dsPublic;
  if FFileStack.Count > 0 then
    FFileStack.Objects[FFileStack.Count - 1] := TObject(Scope)
  else
    FDefaultScope := Scope;
end;

type
  TParserAccess = class(TParser);

function TPreprocessor.ProcessPreprocCommand(Command: TPreprocessorCommand;
  var Params: string; ParamsOffset: Integer): Boolean;

  function ParseScope(Parser: TParser; ExpectedTokens: TTokenKinds = [tkIdent]): TDefineScope;
  const
    ScopeClauses: array[dsPublic..dsPrivate] of string =
      ('public', 'protected', 'private');
  begin
    Parser.NextTokenExpect([tkIdent]);
    for Result := Low(ScopeClauses) to High(ScopeClauses) do
      if CompareText(Parser.TokenString, ScopeClauses[Result]) = 0 then
      begin
        Parser.NextTokenExpect(ExpectedTokens);
        Exit;
      end;
    Result := dsAny;
  end;

  function GetScope(Parser: TParser): TDefineScope;
  begin
    Result := ParseScope(Parser);
    if Result = dsAny then Result := GetDefaultScope;
  end;

  procedure ParseDim(Parser: TParserAccess; ReDim: Boolean);
  var
    Name: string;
    N, NValues, I: Integer;
    Scope: TDefineScope;
    Values: array of TIsppVariant;
  begin
    with Parser do
    try
      Scope := GetScope(Parser);
      Name := CheckReservedIdent(TokenString);
      NextTokenExpect([tkOpenBracket]);
      N := IntExpr(True);
      NValues := 0;
      NextTokenExpect([tkCloseBracket]);
      if PeekAtNextToken = tkOpenBrace then
        begin
          NextToken;
          SetLength(Values, N);
          NValues := 0;
          while True do begin
            if NValues >= N then
              raise EIdentError.CreateFmt(SIndexIsOutOfArraySize, [NValues, Name]);
            Values[NValues] := Expr(True);
            MakeRValue(Values[NValues]);
            Inc(NValues);
            if PeekAtNextToken <> tkComma then
              Break;
            NextToken;
          end;
          NextTokenExpect([tkCloseBrace]);
        end;
      FIdentManager.DimVariable(Name, N, Scope, ReDim);
      if ReDim and (NValues <> 0) then
        Error('Initializers not allowed on #redim of existing array');
      for I := 0 to NValues-1 do
        FIdentManager.DefineVariable(Name, I, Values[I], Scope);
    finally
      //Free
    end;
  end;

  procedure ParseDefine(Parser: TParserAccess);
  var
    Name: string;
    Start, P: PChar;
    IsMacroDefine: Boolean;
    //Ident: string;
    //Param: TIsppMacroParam;
    ParamList: PParamList;
    AParamCount: Byte;
    AExpr: string;
    VarIndex: Integer;
    Scope: TDefineScope;
    MacroExprPos: TExprPosition;

  begin
    with Parser do
    begin
      Start := FExpr;
      Scope := ParseScope(Parser, [tkEOF, tkIdent, tkSemicolon]);
      if Scope = dsAny then
        Scope := GetDefaultScope
      else
        if Token <> tkIdent then
        begin
          SetDefaultScope(Scope);
          Exit;
        end;
      Name := CheckReservedIdent(TokenString);
      IsMacroDefine := FExpr^ = '(';
      if IsMacroDefine then
      begin
        NextToken;
        AParamCount := ParseFormalParams(Parser, ParamList);
        try
          Inc(FExpr);
          P := FExpr;
          MacroExprPos.FileIndex := FCurrentFile;
          MacroExprPos.Line := FCurrentLine;
          MacroExprPos.Column := (FExpr - Start) + ParamsOffset;
          while P^ <> #0 do Inc(P);
          SetString(AExpr, FExpr, P - FExpr);
          AExpr := Trim(AExpr);
          if AExpr = '' then RaiseError(SMacroExpressionExpected);
          FIdentManager.DefineMacro(Name, AExpr, MacroExprPos, FOptions.ParserOptions,
            Slice(ParamList^, AParamCount), Scope);
        finally
          Finalize(ParamList^[0], AParamCount);
          FreeMem(ParamList)
        end;
      end
      else
      begin
        VarIndex := -1;
        if PeekAtNextToken = tkOpenBracket then
        begin
          NextToken;
          VarIndex := IntExpr(True);
          NextTokenExpect([tkCloseBracket]);
        end;
        case PeekAtNextToken of
          opAssign: NextToken;
          tkEOF:
            begin
              FIdentManager.DefineVariable(Name, VarIndex, NULL, Scope);
              Exit;
            end
        end;
        FIdentManager.DefineVariable(Name, VarIndex, Evaluate, Scope);
      end;
    end;
  end;

  procedure ParseUndef(Parser: TParserAccess);
  var
    Scope: TDefineScope;
  begin
    with Parser do
    begin
      Scope := GetScope(Parser);
      FIdentManager.Delete(CheckReservedIdent(TokenString), Scope);
      EndOfExpr;
    end
  end;

  procedure IncludeFile(const Params: string);
  var
    FileName: string;

    function TryPascal: Boolean;
    begin
      Result := not (optPascalStrings in FOptions.ParserOptions.Options);
      if Result then
      begin
        Include(FOptions.ParserOptions.Options, optPascalStrings);
        try
          try
            FileName := ParseStr(Self, Params, ParamsOffset,
              @FOptions.ParserOptions);
          except
            Result := False
          end;
        finally
          Exclude(FOptions.ParserOptions.Options, optPascalStrings);
        end;
      end
    end;

  var
    IncludePathOnly: Boolean;

  begin
    FileName := Params;
    if Pos(';', FileName) > 0 then
      Delete(FileName, Pos(';', FileName), MaxInt);
    FileName := Trim(FileName);
    if (FileName <> '') and (FileName[1] = '<') and
      (FileName[Length(FileName)] = '>') then
    begin
      FileName := Copy(FileName, 2, Length(FileName) - 2);
      IncludePathOnly := True;
    end
    else
    begin
      try
        FileName := ParseStr(Self, Params, ParamsOffset, @FOptions.ParserOptions);
      except
        if not TryPascal then
          raise
      end;
      IncludePathOnly := False;
    end;

    Self.IncludeFile(FileName, False, IncludePathOnly, False);
  end;

  procedure Pragma(Parser: TParserAccess);
  var
    P: string;

    function StrPragma(AllowEmpty: Boolean): string;
    begin
      Result := Parser.StrExpr(True);
      if (Result = '') and not AllowEmpty then
        RaiseError(SNonEmptyStringExpected);
      Parser.EndOfExpr;
    end;

    procedure OptionPragma(var Options: TOptions);
    var
      C: Char;
      V: Boolean;
    begin
      with Parser do
      begin
        NextTokenExpect([opSubtract]);
        repeat
          NextTokenExpect([tkIdent]);
          if Length(TokenString) > 1 then
            RaiseError(SInvalidOptionName);
          C := TokenString[1];
          V := NextTokenExpect([opAdd, opSubtract]) = opAdd;
          SetOption(Options, C, V);
        until NextTokenExpect([tkEOF, opSubtract, tkSemicolon]) <> opSubtract;
      end;
    end;

  var
    CatchException: Boolean;
    ErrorMsg: string;
  begin
    CatchException := True;
    try
      with Parser do
      begin
        NextTokenExpect([tkIdent]);
        P := LowerCase(TokenString);
        if P = 'include' then
          FIncludePath := StrPragma(True)
        else if P = 'inlinestart' then
          FOptions.InlineStart := StrPragma(False)
        else if P = 'inlineend' then
          FOptions.InlineEnd := StrPragma(False)
        else if P = 'spansymbol' then
          FOptions.SpanSymbol := StrPragma(False)[1]
        else if P = 'parseroption' then
          OptionPragma(FOptions.ParserOptions.Options)
        else if P = 'option' then
          OptionPragma(FOptions.Options)
        else if P = 'verboselevel' then
        begin
          Include(FOptions.Options, optVerbose);
          FOptions.VerboseLevel := IntExpr(True);
          VerboseMsg(0, SChangedVerboseLevel, [FOptions.VerboseLevel]);
          EndOfExpr;
        end
        else if P = 'warning' then begin
          { Also see WarningFunc in IsppFuncs }
          WarningMsg(StrPragma(True))
        end else if P = 'message' then begin
          { Also see MessageFunc in IsppFuncs }
          StatusMsg(StrPragma(True))
        end else if P = 'error' then begin
          { Also see ErrorFunc in IsppFuncs }
          ErrorMsg := StrPragma(True);
          if ErrorMsg = '' then ErrorMsg := 'Error';
          CatchException := False;
          RaiseError(ErrorMsg)
        end
        else
          WarningMsg(SFailedToParsePragmaDirective);
      end;
    except
      if CatchException then
        WarningMsg(SFailedToParsePragmaDirective)
      else
        raise
    end;
  end;

  function DoFile(FileName: string): string;

    function GetTempFileName(const Original: string): string;
    var
      Path: string;
    begin
      SetLength(Path, MAX_PATH);
      SetLength(Path, GetTempPath(MAX_PATH, PChar(Path)));
      SetLength(Result, MAX_PATH);
      if Windows.GetTempFileName(PChar(Path), PChar(UpperCase(Original)), 0, PChar(Result)) <> 0 then
        SetLength(Result, StrLen(PChar(Result)))
      else
        RaiseLastOSError;
    end;

  var
    F: TTextFileReader;
    ALine: string;
    Preprocessor: TPreprocessor;
    NewOptions: TIsppOptions;
  begin
    FileName := PrependDirName(FileName, FSourcePath);
    if FileExists(FileName) then
    begin
      Result := GetTempFileName(ExtractFileName(FileName));
      StatusMsg(SProcessingExternalFile, [FileName]);
      NewOptions := FOptions;
      Preprocessor := TPreprocessor.Create(FCompilerParams, FIdentManager,
        NewOptions, FSourcePath, FCompilerPath, FileName);
      try
        F := TTextFileReader.Create(Filename, fdOpenExisting, faRead, fsRead);
        try
          while not F.Eof do begin
            ALine := F.ReadLine;
            Preprocessor.QueueLine(ALine);
          end;
        finally
          F.Free;
        end;
        Preprocessor.SaveToFile(Result);
        QueueFileForDeletion(Result);
        VerboseMsg(1, STemporaryFileCreated, [Result]);
      finally
        Preprocessor.Free;
      end;
    end
    else
      RaiseError(Format(SFileNotFound, [FileName]));
  end;

  procedure ParseFor(Parser: TParserAccess);
  var
    Condition, Action, Body: PChar;
  begin
    Parser.NextTokenExpect([tkOpenBrace]);
    Parser.Expr(False);
    Parser.NextTokenExpect([tkSemicolon]);
    { Skip condition and remember it }
    Condition := Parser.FExpr;
    Parser.Sequentional(False);
    Parser.NextTokenExpect([tkSemicolon]);
    Action := Parser.FExpr;
    Parser.Sequentional(False);
    Parser.NextTokenExpect([tkCloseBrace]);
    Body := Parser.FExpr;
    Parser.Sequentional(False);
    Parser.EndOfExpr;
    Parser.SetPos(Condition);
    while Parser.IntExpr(False) <> 0 do
    begin
      Parser.SetPos(Body);
      Parser.Sequentional(True);
      Parser.SetPos(Action);
      Parser.Sequentional(True);
      Parser.SetPos(Condition);
    end;
  end;

  procedure Glue(LineNo: Integer);
  begin
    if LineNo > FOutput.Count then
      RaiseError(Format(SInsertLineNoTooBig, [LineNo]));
    FInsertionPoint := LineNo;
    VerboseMsg(2, SChangingInsertionPointToLine, [FInsertionPoint]);
  end;

  procedure EndGlue;
  begin
    VerboseMsg(2, SResettingInsertionPoint);
    FInsertionPoint := -1;
  end;

  procedure BeginProcDecl(Parser: TParserAccess);
  var
    ProcName: string;
  begin
    if FInForBody or FInProcBody then
      RaiseError('Nested procedure declaration and compound loops not allowed');
    FInProcBody := True;
    Parser.NextTokenExpect([tkIdent]);
    ProcName := Parser.TokenString;
    Parser.EndOfExpr;
    FProcs.AddObject(ProcName, TStringList.Create);
    EmitDestination.Add('#define private');
  end;

  procedure EndProcDecl;
  begin
    if not FInProcBody then
      RaiseError('''endproc'' without ''procedure''');
    FInProcBody := False;
  end;

var
  IfCondition: TIsppVariant;
  DummyContext: ICallContext;
  Parser: TParserAccess;
begin
  Result := False;
  Parser := TParserAccess.Create(Self, Params, ParamsOffset, @FOptions.ParserOptions);
  with Parser do
  try
    case Command of
      pcError: RaiseError(SUnknownPreprocessorDirective);
      pcIf, pcElseIf:
        begin
          IfCondition := Evaluate;
          case IfCondition.Typ of
            evInt: Result := IfCondition.AsInt <> 0;
            evStr: Result := IfCondition.AsStr <> ''
          else
            WarningMsg(SSpecifiedConditionEvalatedToVoid);
            Result := False
          end;
        end;
      pcIfdef, pcIfndef:
        begin
          NextTokenExpect([tkIdent]);
          case GetIdent(TokenString, DummyContext) of
            itUnknown: Result := Command = pcIfNDef;
            itVariable, itMacro: Result := Command = pcIfDef;
            itFunc:
            begin
              Result := Command = pcIfDef;
              WarningMsg(SFuncIdentForIfdef);
            end;
            else
            begin
              Result := Command = pcIfNDef;
              WarningMsg(SSpecFuncIdentForIfdef);
            end;
          end;
          EndOfExpr;
        end;
      pcIfExist, pcIfNExist:
        Result := FileExists(PrependDirName(StrExpr(False), FSourcePath)) xor (Command = pcIfNExist);
      pcDefine: ParseDefine(Parser);
      pcDim: ParseDim(Parser, False);
      pcReDim: ParseDim(Parser, True);
      pcUndef: ParseUndef(Parser);
      pcInclude: IncludeFile(Params);
      pcErrorDir:
        begin
          { Also see ErrorFunc in IsppFuncs }
          if Params = '' then Params := 'Error';
          RaiseError(Params.Trim);
        end;
      pcPragma: Pragma(Parser);
      pcPrint: Params := ToStr(Evaluate).AsStr;
      pcPrintEnv:
        begin
          NextTokenExpect([tkIdent]);
          Params := GetEnv(TokenString);
          EndOfExpr;
        end;
      pcFile: Params := DoFile(StrExpr(False));
      pcExecute: Evaluate;
      pcGlue: Glue(IntExpr(False));
      pcEndGlue: EndGlue;
      pcFor: ParseFor(Parser);
      pcProcedure: BeginProcDecl(Parser);
      pcEndProc: EndProcDecl;
    else
      WarningMsg(SDirectiveNotYetSupported, [PreprocCommands[Command]])
    end;
  finally
    Free
  end;
end;

function TPreprocessor.InternalQueueLine(const LineRead: string;
  FileIndex, LineNo: Word; NonISS: Boolean): Integer; //how many just been added
var
  L: Integer;
begin
  L := Length(LineRead);
  if (L > 2) and (LineRead[L] = FOptions.SpanSymbol) and (LineRead[L - 1] <= #32) then
  begin
    FQueuedLine := FQueuedLine + TrimLeft(Copy(LineRead, 1, L - 1));
    Inc(FQueuedLineCount);
    Result := 0;
  end
  else
    if FQueuedLineCount > 0 then
    begin
      InternalAddLine(FQueuedLine + TrimLeft(LineRead), FileIndex, LineNo, NonISS);
      FQueuedLine := '';
      Result := FQueuedLineCount + 1;
      FQueuedLineCount := 0;
    end
    else
    begin
      InternalAddLine(LineRead, FileIndex, LineNo, NonISS);
      Result := 1;
    end;
end;

procedure TPreprocessor.QueueLine(const LineRead: string);
begin
  Inc(FMainCounter, InternalQueueLine(LineRead, 0, FMainCounter, False));
end;

procedure TPreprocessor.RegisterFunction(const Name: string; Handler: TIsppFunction; Ext: Longint);
begin
  FIdentManager.DefineFunction(Name, Handler, Ext);
end;

procedure TPreprocessor.SaveToFile(const FileName: string);
begin
  var OldWriteBOM := FOutput.WriteBOM;
  try
    FOutput.WriteBOM := False;
    FOutput.SaveToFile(FileName, TEncoding.UTF8);
  finally
    FOutput.WriteBOM := OldWriteBOM;
  end;
end;

function TPreprocessor.CheckFile(const FileName: string): Boolean;
begin
  Result := FFileStack.IndexOf(ExpandFileName(FileName)) < 0;
end;

procedure TPreprocessor.PopFile;
begin
  FFileStack.Delete(FFileStack.Count - 1);
end;

procedure TPreprocessor.PushFile(const FileName: string);
begin
  FFileStack.AddObject(ExpandFileName(FileName), TObject(dsPublic));
end;

procedure TPreprocessor.CallIdleProc;
begin
  FCompilerParams.IdleProc(FCompilerParams.CompilerData);
end;

procedure TPreprocessor.VerboseMsg(Level: Byte; const Msg: string);
begin
  if (optVerbose in FOptions.Options) and (FOptions.VerboseLevel >= Level) then
    StatusMsg(Msg);
end;

procedure TPreprocessor.VerboseMsg(Level: Byte; const Msg: string;
  const Args: array of const);
begin
  VerboseMsg(Level, Format(Msg, Args));
end;

procedure TPreprocessor.StatusMsg(const Msg: string);
begin
  SendMsg(Msg, imtStatus);
end;

procedure TPreprocessor.StatusMsg(const Msg: string; const Args: array of const);
begin
  StatusMsg(Format(Msg, Args));
end;

procedure TPreprocessor.WarningMsg(const Msg: string);
begin
  SendMsg(Msg, imtWarning);
end;

procedure TPreprocessor.WarningMsg(const Msg: string; const Args: array of const);
begin
  WarningMsg(Format(Msg, Args));
end;

procedure TPreprocessor.SendMsg(Msg: string; Typ: TIsppMessageType);
const
  MsgPrefixes: array[TIsppMessageType] of string = ('', 'Warning: ');
var
  LineNumber: Word;
  FileName: String;
begin
  Msg := MsgPrefixes[Typ] + Msg;

  LineNumber := GetLineNumber(-1);
  if LineNumber <> 0 then begin
    FileName := GetFileName(-1);
    if FileName <> '' then
      Msg := Format('Line %d of %s: %s', [LineNumber, PathExtractName(FileName), Msg])
    else
      Msg := Format('Line %d: %s', [LineNumber, Msg]);
  end;

  FCompilerParams.StatusProc(FCompilerParams.CompilerData, PChar(Msg), Typ = imtWarning);
end;

function TPreprocessor.DimOf(const Name: String): Integer;
begin
  Result := FIdentManager.DimOf(Name)
end;

function TPreprocessor.EmitDestination: TStringList;
begin
  if FInProcBody then
    Result := TStringList(FProcs.Objects[FProcs.Count - 1])
  else
    Result := FOutput;
end;

procedure TPreprocessor.ExecProc(Body: TStrings);
var
  I: Integer;
begin
  for I := 0 to Body.Count - 1 do
    InternalAddLine(Body[I], Integer(Body.Objects[I]) shr 16,
      Integer(Body.Objects[I]) and $FFFF - 1, False);
end;

{ TConditionalTranslationStack }

constructor TConditionalTranslationStack.Create(Preproc: TPreprocessor);
begin
  inherited Create;
  FPreproc := Preproc;
  FCache := True;
end;

procedure TConditionalTranslationStack.IfInstruction(Eval: Boolean);
var
  A: TConditionalBlockInfo;
begin
  A.BlockState := Eval;
  A.Fired := Eval;
  A.HadElse := False;
  PushItem(Pointer(A));
  FCacheValid := False;
  VerboseMsg(cvmIf, Eval);
end;

procedure TConditionalTranslationStack.ElseIfInstruction(Eval: Boolean);
var
  A: TConditionalBlockInfo;
begin
  if AtLeast(1) then
  begin
    A := Last;
    with A do
    begin
      if HadElse then FPreproc.RaiseError(SElifAfterElse);
      BlockState := not Fired and Eval;
      Fired := Fired or Eval;
      FCacheValid := False;
    end;
    UpdateLast(A);
    VerboseMsg(cvmElif, Eval);
  end
  else
    FPreproc.RaiseError(SElseWithoutIf);
end;

procedure TConditionalTranslationStack.ElseInstruction;
var
  A: TConditionalBlockInfo;
begin
  if AtLeast(1) then
  begin
    A := Last;
    with A do
    begin
      if HadElse then FPreproc.RaiseError(SDoubleElse);
      BlockState := not Fired;
      Fired := True;
      HadElse := True;
      FCacheValid := False;
    end;
    UpdateLast(A);
    VerboseMsg(cvmElse, False);
  end
  else
    FPreproc.RaiseError(SElseWithoutIf);
end;

procedure TConditionalTranslationStack.EndIfInstruction;
begin
  if AtLeast(1) then
  begin
    PopItem;
    FCacheValid := False;
    VerboseMsg(cvmEndif, False);
  end
  else
    FPreproc.RaiseError(SEndifWithoutIf);
end;

function TConditionalTranslationStack.Include: Boolean;
var
  I: Integer;
begin
  if FCacheValid then
    Result := FCache
  else
  begin
    FCacheValid := True;
    if Count > 0 then
    begin
      Result := False;
      FCache := False;
      for I := Count - 1 downto 0 do
        if not TConditionalBlockInfo(List[I]).BlockState then Exit;
    end;
    Result := True;
    FCache := True;
  end;
end;

procedure TConditionalTranslationStack.Resolved;
begin
  if Count > 0 then FPreproc.RaiseError(SEndifExpected);
end;

function TConditionalTranslationStack.Last: TConditionalBlockInfo;
begin
  Result := TConditionalBlockInfo(Longint(List.Last))
end;

procedure TConditionalTranslationStack.UpdateLast(
  const Value: TConditionalBlockInfo);
begin
  List.Items[List.Count - 1] := Pointer(Value)
end;

procedure TConditionalTranslationStack.VerboseMsg(
  Msg: TConditionalVerboseMsg; Eval: Boolean);
const
  B: array[Boolean] of string = ('false', 'true');
var
  M: string;
begin
  case Msg of
    cvmIf: M := SStartingConditionalInclusionIf;
    cvmElif: M := SUpdatingConditionalInclusionElif;
    cvmElse: M := SUpdatingConditionalInclusionElse;
  else
    begin
      FPreproc.VerboseMsg(6, SFinishedConditionalInclusion);
      Exit;
    end;
  end;
  FPreproc.VerboseMsg(6, M);
end;

{ TPreprocessor }

function TPreprocessor._AddRef: Integer;
begin
  Result := -1
end;

function TPreprocessor._Release: Integer;
begin
  Result := -1;
end;

function TPreprocessor.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE
end;

procedure TPreprocessor.RaiseError(const Message: string);
begin
  RaiseErrorEx(Message, 0);
end;

procedure TPreprocessor.RaiseErrorEx(const Message: string; Column: Integer);
var
  E: EPreprocError;
begin
  E := EPreprocError.Create(Self, Message);
  E.ColumnNumber := Column;
  raise E;
end;

{ TPredefinedVarCallContext }

type

  TPredefinedVarCallContext = class(TInterfacedObject, ICallContext)
  private
    FValue: TIsppVariant;
  public
    constructor Create(const Value: TIsppVariant);
    procedure Add(const Name: String; const Value: TIsppVariant);
    function Call: TIsppVariant; dynamic;
    function GroupingStyle: TArgGroupingStyle;
    procedure Clone(out NewCallContext: ICallContext);
  end;

  TCounterCallContext = class(TPredefinedVarCallContext)
  private
    FCounter: PInteger;
  public
    constructor Create(Counter: PInteger);
    function Call: TIsppVariant; override;
  end;

  TProcCallContext = class(TInterfacedObject, ICallContext)
  private
    FPreproc: TPreprocessor;
    FBody: TStrings;
    FScopeUpdated: Boolean;
    FIndex: Integer;
    procedure UpdateScope;
  public
    constructor Create(Proprocessor: TPreprocessor; ProcBody: TStrings);
    procedure Add(const Name: String; const Value: TIsppVariant);
    function Call: TIsppVariant;
    procedure Clone(out NewContext: ICallContext);
    function GroupingStyle: TArgGroupingStyle;
  end;

constructor TCounterCallContext.Create(Counter: PInteger);
begin
  FCounter := Counter;
end;

function TCounterCallContext.Call: TIsppVariant;
begin
  MakeInt(Result, FCounter^);
  Inc(FCounter^);
end;

constructor TPredefinedVarCallContext.Create(const Value: TIsppVariant);
begin
  FValue := Value;
end;

procedure TPredefinedVarCallContext.Add(const Name: String;
  const Value: TIsppVariant);
begin
  raise EIdentError.Create(SParameterlessVariable);
end;

function TPredefinedVarCallContext.Call: TIsppVariant;
begin
  Result := FValue;
end;

function TPredefinedVarCallContext.GroupingStyle: TArgGroupingStyle;
begin
  Result := agsNone;
end;

{ IIdentManager }

function LookupAlwaysDefined(const Name: string): Boolean;
const
  AlwaysDefined: array[0..3] of string =
    ('ISPP_INVOKED', 'WINDOWS', '__WIN32__', 'UNICODE');
var
  I: Integer;
begin
  Result := True;
  for I := Low(AlwaysDefined) to High(AlwaysDefined) do
    if CompareText(AlwaysDefined[I], Name) = 0 then Exit;
  Result := False;
end;

const
  SCounter = '__COUNTER__';

function TPreprocessor.Defined(const Name: String): Boolean;
begin
  Result := LookupAlwaysDefined(Name) or LookupPredefined(Name, nil) or
    (CompareText(Name, SCounter) = 0) or FIdentManager.Defined(Name);
end;

function TPreprocessor.GetIdent(const Name: String;
  out CallContext: ICallContext): TIdentType;
var
  V: TIsppVariant;
  I: Integer;
begin
  Result := itVariable;
  I := FProcs.IndexOf(Name);
  if I >= 0 then
  begin
    Result := itFunc;
    CallContext := TProcCallContext.Create(Self, TStrings(FProcs.Objects[I]));
  end
  else
    if LookupAlwaysDefined(Name) then
      CallContext := TPredefinedVarCallContext.Create(NULL)
    else
      if LookupPredefined(Name, @V) then
        CallContext := TPredefinedVarCallContext.Create(V)
      else
        if CompareText(Name, SCounter) = 0 then
          CallContext := TCounterCallContext.Create(@FCounter)
        else
          Result := FIdentManager.GetIdent(Name, CallContext)
end;

function TPreprocessor.TypeOf(const Name: String): Byte;
var
  V: TIsppVariant;
begin
  if LookupAlwaysDefined(Name) then
    Result := TYPE_NULL
  else
    if LookupPredefined(Name, @V) then
      case V.Typ of
        evInt: Result := TYPE_INTEGER;
        evStr: Result := TYPE_STRING
      else
        Result := TYPE_NULL
      end
    else
      if CompareText(Name, SCounter) = 0 then
        Result := TYPE_INTEGER
      else
        Result := FIdentManager.TypeOf(Name)
end;

function TPreprocessor.LookupPredefined(Name: string;
  Value: PIsppVariant): Boolean;
begin
  Result := True;
  Name := UpperCase(Name);
  if Name = '__FILE__' then
  begin
    if Value <> nil then MakeStr(Value^, ExtractFileName(FIncludes[FCurrentFile]))
  end
  else if Name = '__PATHFILENAME__' then
  begin
    if Value <> nil then MakeStr(Value^, FIncludes[FCurrentFile])
  end
  else if Name = '__LINE__' then
  begin
    if Value <> nil then MakeInt(Value^, FCurrentLine)
  end
  else if Name = 'PREPROCVER' then
  begin
    if Value <> nil then MakeInt(Value^, SetupBinVersion)
  end
  else if Name = '__INCLUDE__' then
  begin
    if Value <> nil then MakeStr(Value^, FIncludePath);
  end
  else if (Length(Name) = 9) and (Copy(Name, 1, 6) = '__OPT_') and
    (Copy(Name, 8, 2) = '__') then
  begin
    if Value <> nil then Value^ := NULL;
    Result := GetOption(FOptions.Options, Name[7]);
  end
  else if (Length(Name) = 10) and (Copy(Name, 1, 7) = '__POPT_') and
    (Copy(Name, 9, 2) = '__') then
  begin
    if Value <> nil then Value^ := NULL;
    Result := GetOption(FOptions.ParserOptions.Options, Name[8]);
  end
  else
    Result := False;
end;

procedure TPredefinedVarCallContext.Clone(
  out NewCallContext: ICallContext);
begin
  NewCallContext := Self
end;

procedure TPreprocessor.CollectGarbage(Item: Pointer;
  Proc: TDropGarbageProc);
begin
  if (Item = nil) or (@Proc = nil) then Exit;
  if FGarbageCollection = nil then
    FGarbageCollection := TList.Create;
  FGarbageCollection.Add(Item);
  FGarbageCollection.Add(@Proc);
end;

procedure TPreprocessor.UncollectGarbage(Item: Pointer);
var
  I: Integer;
begin
  if FGarbageCollection = nil then Exit;
  for I := 0 to FGarbageCollection.Count div 2 - 1 do
    if FGarbageCollection.Items[I * 2] = Item then
    begin
      FGarbageCollection.Items[I * 2] := nil;
      FGarbageCollection.Items[I * 2 + 1] := nil;
    end;
  FGarbageCollection.Pack;
  if FGarbageCollection.Count = 0 then FreeAndNil(FGarbageCollection);
end;

procedure TPreprocessor.DropGarbage;
var
  I: Integer;
  Proc: TDropGarbageProc;
  Item: Pointer;
begin
  if FGarbageCollection <> nil then
  try
    for I := 0 to FGarbageCollection.Count div 2 - 1 do
    begin
      Item := FGarbageCollection.Items[I * 2];
      Proc := FGarbageCollection.Items[I * 2 + 1];
      try
        if @Proc <> nil then
        try
          Proc(Item);
        except
        end
        else
          if Item <> nil then
          begin
            try
              TObject(Item).Free
            except
              try Dispose(Item) except end;
            end;
          end;
      finally
        FGarbageCollection.Items[I * 2] := nil;
        FGarbageCollection.Items[I * 2 + 1] := nil;
      end;
    end;
  finally
    FreeAndNil(FGarbageCollection);
  end;
end;

function TPreprocessor.PrependDirName(const FileName, Dir: string): string;
var
  P: PChar;
begin
  P := FCompilerParams.PrependDirNameProc(FCompilerParams.CompilerData,
    PChar(FileName), PChar(Dir), PChar(GetFileName(-1)), GetLineNumber(-1), 0);
  if P = nil then
    RaiseError('PrependDirNameProc failed');
  Result := P;
end;

procedure TPreprocessor.IncludeFile(FileName: string;
  Builtins, UseIncludePathOnly, ResetCurrentFile: Boolean);

  function IsDotRelativePath(const Filename: String): Boolean;
  begin
    { Check for '.\' and '..\' }
    if (Length(Filename) >= 2) and (Filename[1] = '.') and PathCharIsSlash(Filename[2]) then
      Result := True
    else if (Length(Filename) >= 3) and (Filename[1] = '.') and (Filename[2] = '.') and
       PathCharIsSlash(Filename[3]) then
      Result := True
    else
      Result := False;
  end;

  procedure AddToPath(var Path: string; const Dir: string);
  begin
    if (Dir <> '') and (Pos(';' + Dir + ';', ';' + Path + ';') = 0) then
    begin
      if Path <> '' then Path := Path + ';';
      Path := Path + Dir;
    end;
  end;

  function RemoveSlash(const S: string): string;
  begin
    Result := S;
    if (Length(Result) > 3) and (Result[Length(Result)] = '\') then
      Delete(Result, Length(Result), 1);
  end;

  function DoSearch(const SearchDirs: String): String;
  var
    FilePart: PChar;
  begin
    SetLength(Result, MAX_PATH);
    SetLength(Result, SearchPath(PChar(SearchDirs), PChar(FileName), nil, MAX_PATH,
      PChar(Result), FilePart));
  end;

var
  CurPath, SearchDirs, FullFileName: String;
  FileHandle: TPreprocFileHandle;
  I, FileIndex: Integer;
  J: Word;
  LineText: PChar;
  LineTextStr: string;
begin
  if ResetCurrentFile then begin
    FCurrentFile := 0;
    FCurrentLine := 0;
  end;
  
  { Expand any prefix on the filename (e.g. 'compiler:') }
  FileName := PrependDirName(FileName, '');

  if IsDotRelativePath(FileName) then
  begin
    { Make filenames beginning with '.\' and '..\' relative to the directory
      containing the current file }
    CurPath := PathExtractPath(FIncludes[FCurrentFile]);
    if CurPath = '' then
      CurPath := FSourcePath;
    FileName := PathCombine(CurPath, FileName);
  end
  else if not PathIsRooted(FileName) then
  begin
    if not UseIncludePathOnly then
    begin
      for I := FFileStack.Count - 1 downto 0 do
        AddToPath(SearchDirs, ExtractFileDir(FFileStack[I]));
      if FIncludes[0] <> '' then
        AddToPath(SearchDirs, ExtractFileDir(FIncludes[0]));
      AddToPath(SearchDirs, RemoveSlash(FSourcePath));
    end;

    AddToPath(SearchDirs, FIncludePath);
    AddToPath(SearchDirs, GetEnv('INCLUDE'));

    if not UseIncludePathOnly then
      AddToPath(SearchDirs, RemoveSlash(FCompilerPath));
  end;

  FullFileName := DoSearch(SearchDirs);

  if FullFileName <> '' then
  begin
    if not CheckFile(FullFileName) then
      RaiseError(Format(SFileIsAlreadyBeingIncluded, [FullFileName]));
    if not Builtins then
      StatusMsg(SIncludingFile, [FullFileName]);
    PushFile(FullFileName);
    try
      FileHandle := FCompilerParams.LoadFileProc(FCompilerParams.CompilerData,
        PChar(FullFileName), PChar(GetFileName(-1)), GetLineNumber(-1), 0);
      if FileHandle < 0 then
        RaiseError('LoadFileProc failed');
      FileIndex := FIncludes.Add(FullFileName);
      FIdentManager.BeginLocal;
      try
        I := 0;
        J := 0;
        while True do
        begin
          LineText := FCompilerParams.LineInProc(FCompilerParams.CompilerData,
            FileHandle, I);
          if LineText = nil then
            Break;
          LineTextStr := LineText;
          Inc(J, InternalQueueLine(LineTextStr, FileIndex, J, False));
          Inc(I);
        end;
      finally
        FIdentManager.EndLocal
      end;
    finally
      PopFile;
    end;
  end
  else
    RaiseError(Format(SFileNotFound, [FileName]));
end;

// ParseFormalParams
//   Parser must be behind the opening parenthesis

function TPreprocessor.ParseFormalParams(Parser: TParser;
  var ParamList: PParamList): Integer;
var
  Param: TIsppMacroParam;
  Ident: string;

  procedure Grow;
  var
    OldCapacity, NewCapacity: Integer;
  begin
    OldCapacity := ((Result div 4) * 4) * SizeOf(TIsppMacroParam);
    NewCapacity := ((Result div 4 + 1) * 4);
    if NewCapacity > High(Byte) then RaiseError(STooManyFormalParams);
    NewCapacity := NewCapacity * SizeOf(TIsppMacroParam);
    ReallocMem(ParamList, NewCapacity);
    { Initilizing to zeroes is required to prevent compiler's attempts to
      finilize not existing strings }
    FillChar(ParamList^[Result], NewCapacity - OldCapacity, 0)
  end;

begin
  with Parser do
  begin
    Result := 0;
    ParamList := AllocMem(SizeOf(TIsppMacroParam) * 4);
    while not (PeekAtNextToken in [tkEOF, tkCloseParen]) do
    begin
      Param.Name := '';
      Param.DefValue.AsStr := '';
      FillChar(Param, SizeOf(Param), 0);
      Param.ParamFlags := [];
      if NextTokenExpect([tkIdent, opMul]) = tkIdent then
      begin
        Ident := TokenString;
        if not (PeekAtNextToken in [tkEOF, tkComma, tkCloseParen, opAssign]) then
        begin
          Ident := UpperCase(Ident);
          if Ident = sAny then {do nothing }
          else if Ident = sInt then Param.DefValue.Typ := evInt
          else if Ident = sStr then Param.DefValue.Typ := evStr
          else if Ident = 'FUNC' then
            begin
              Param.DefValue.Typ := evCallContext;
              Include(Param.ParamFlags, pfFunc)
            end
          else if Ident = 'ARRAY' then Param.DefValue.Typ := evCallContext
          else RaiseError(Format(SInvalidTypeId, [Ident]));
          if Param.DefValue.Typ <> evSpecial then
            Include(Param.ParamFlags, pfTypeDefined);
          if NextTokenExpect([tkIdent, opMul]) = opMul then
          begin
            Include(Param.ParamFlags, pfByRef);
            NextTokenExpect([tkIdent]);
          end;
        end;
      end
      else
      begin
        Include(Param.ParamFlags, pfByRef);
        NextTokenExpect([tkIdent]);
      end;
      Ident := TokenString;
      Param.Name := CheckReservedIdent(Ident);
      if PeekAtNextToken = opAssign then
      begin
        if pfByRef in Param.ParamFlags then
          RaiseError(SByRefNoDefault);
        NextToken;
        case Param.DefValue.Typ of
          evSpecial: Param.DefValue := GetRValue(Expr(True));
          evInt: Param.DefValue.AsInt := IntExpr(True);
          evStr: Param.DefValue.AsStr := StrExpr(True);
        end;
        Include(Param.ParamFlags, pfHasDefault);
      end;
      ParamList^[Result] := Param;
      Inc(Result);
      if Result mod 4 = 0 then
        Grow;
      if NextTokenExpect([tkComma, tkCloseParen]) = tkCloseParen then Break;
    end;
  end;
end;

{ TProcCallContext }

procedure TProcCallContext.Add(const Name: String;
  const Value: TIsppVariant);
begin
  UpdateScope;
  if Name <> '' then
    FPreproc.FIdentManager.DefineVariable(Name, -1, Value, dsPrivate);
  FPreproc.FIdentManager.DefineVariable(SLocal, FIndex, Value, dsPrivate);
  Inc(FIndex);
end;

function TProcCallContext.Call: TIsppVariant;
begin
  UpdateScope;
  try
    FPreproc.ExecProc(FBody);
  finally
    FPreproc.FIdentManager.EndLocal
  end;
end;

procedure TProcCallContext.Clone(out NewContext: ICallContext);
begin
  NewContext := TProcCallContext.Create(FPreproc, FBody);
end;

constructor TProcCallContext.Create(Proprocessor: TPreprocessor;
  ProcBody: TStrings);
begin
  FPreproc := Proprocessor;
  FBody := ProcBody
end;

function TProcCallContext.GroupingStyle: TArgGroupingStyle;
begin
  Result := agsParenteses;
end;

procedure TProcCallContext.UpdateScope;
var
  ReDim: Boolean;
begin
  if not FScopeUpdated then
  begin
    FPreproc.FIdentManager.BeginLocal;
    ReDim := False;
    FPreproc.FIdentManager.DimVariable(SLocal, 16, dsPrivate, ReDim);
    FScopeUpdated := True;
  end;
end;

end.
