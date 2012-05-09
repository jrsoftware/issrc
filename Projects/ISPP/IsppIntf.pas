{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff
  $Id: IsppIntf.pas,v 1.2 2009/04/02 14:20:59 mlaan Exp $
}

unit IsppIntf;

interface

type
  
  TOptionID = 0..25;

  TOptions = packed set of TOptionID;

  PIsppParserOptions = ^TIsppParserOptions;
  TIsppParserOptions = packed record
    Options: TOptions;
  end;

  TIsppOptions = packed record
    ParserOptions: TIsppParserOptions;
    Options: TOptions;
    VerboseLevel: Byte;
    InlineStart: string[7];
    InlineEnd: string[7];
    SpanSymbol: AnsiChar;
  end;

  TIsppVarType = (evSpecial, evNull, evInt, evStr, evLValue, evCallContext);

  IIsppFuncParam = interface
    function GetType: TIsppVarType; stdcall;
    function GetAsInt: Integer; stdcall;
    function GetAsString(Buf: PChar; BufSize: Integer): Integer; stdcall;
  end;

  IIsppFuncResult = interface
    procedure SetAsInt(Value: Integer); stdcall;
    procedure SetAsString(Value: PChar); stdcall;
    procedure SetAsNull; stdcall;
    procedure Error(Message: PChar); stdcall;
  end;

  IIsppFuncParams = interface
    function Get(Index: Integer): IIsppFuncParam; stdcall;
    function GetCount: Integer; stdcall;
  end;

  TIsppFuncResult = packed record
    Reserved: Byte;
    ErrParam: Word;
    Error: Byte;
  end;

  TIsppFunction = function (Ext: Longint; const Params: IIsppFuncParams;
    const FuncResult: IIsppFuncResult): TIsppFuncResult; stdcall;

  IPreprocessor = interface
    procedure DefineVariable(Name: PChar; Typ: TIsppVarType; Value: Longint);
    procedure QueueLine(Line: PChar);
  end;

const

  { TIsppFuncResult.Error values }

  // Function executed successfully
  ISPPFUNC_SUCCESS    = Byte($00);
  // Unexpected failure
  ISPPFUNC_FAIL       = Byte($01);
  // Too many arguments passed, ErrParam contains maximal number of arguments needed
  ISPPFUNC_MANYARGS   = Byte($02);
  // Insufficient required arguments, ErrParam contains minimal number of arguments
  ISPPFUNC_INSUFARGS  = Byte($03);
  // Wrong type of argument passed, ErrParam is the index of the argument
  ISPPFUNC_INTWANTED  = Byte($04);
  // Wrong type of argument passed, ErrParam is the index of the argument
  ISPPFUNC_STRWANTED  = Byte($05);

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
