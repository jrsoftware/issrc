{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff

  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

unit ISPP.Intf;

interface

type
  
  TOptionID = 0..25;

  TOptions = set of TOptionID;

  PIsppParserOptions = ^TIsppParserOptions;
  TIsppParserOptions = record
    Options: TOptions;
  end;

  TIsppOptions = record
    ParserOptions: TIsppParserOptions;
    Options: TOptions;
    VerboseLevel: Integer;
    InlineStart: String;
    InlineEnd: String;
    SpanSymbol: Char;
  end;

  TIsppVarType = (evSpecial, evNull, evInt, evStr, evLValue, evCallContext);

  IIsppFuncParam = interface
    function GetType: TIsppVarType; stdcall;
    function GetAsInt64: Int64; stdcall;
    function GetAsString(Buf: PChar; BufSize: Cardinal): Integer; stdcall;
  end;

  IIsppFuncResult = interface
    procedure SetAsInt(Value: Int64); stdcall;
    procedure SetAsString(Value: PChar); stdcall;
    procedure SetAsNull; stdcall;
    procedure Error(Message: PChar); stdcall;
  end;

  IIsppFuncParams = interface
    function Get(Index: Integer): IIsppFuncParam; stdcall;
    function GetCount: Integer; stdcall;
  end;

  TIsppFuncResult = record
    ErrParam: Integer;
    Error: Integer;
  end;

  TIsppFunction = function (Ext: NativeInt; const Params: IIsppFuncParams;
    const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;

  IPreprocessor = interface
    procedure DefineVariable(Name: PChar; Typ: TIsppVarType; Value: Int64);
    procedure QueueLine(Line: PChar);
  end;

const

  { TIsppFuncResult.Error values }

  // Function executed successfully
  ISPPFUNC_SUCCESS    = 0;
  // Unexpected failure
  ISPPFUNC_FAIL       = 1;
  // Too many arguments passed, ErrParam contains maximal number of arguments needed
  ISPPFUNC_MANYARGS   = 2;
  // Insufficient required arguments, ErrParam contains minimal number of arguments
  ISPPFUNC_INSUFARGS  = 3;
  // Wrong type of argument passed, ErrParam is the index of the argument
  ISPPFUNC_INTWANTED  = 4;
  // Wrong type of argument passed, ErrParam is the index of the argument
  ISPPFUNC_STRWANTED  = 5;

const

  { Parser options }

  optSCBE             = TOptionID(Ord('B') - Ord('A'));
  optSCME             = TOptionID(Ord('M') - Ord('A'));
  optPassNulls        = TOptionID(Ord('N') - Ord('A'));
  optPascalStrings    = TOptionID(Ord('P') - Ord('A'));
  optAllowUndeclared  = TOptionID(Ord('U') - Ord('A'));

  { Preprocessor options }

  optPassToCompiler   = TOptionID(Ord('C') - Ord('A'));
  optEmitEmptyLines   = TOptionID(Ord('E') - Ord('A'));
  optCircMacroCall    = TOptionID(Ord('R') - Ord('A'));
  optVerbose          = TOptionID(Ord('V') - Ord('A'));

implementation

end.
