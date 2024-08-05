{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff

  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

unit ISPP.Base;

interface

uses ISPP.Intf, SysUtils;

type

  ICallContext = interface;

  PIsppVariant = ^TIsppVariant;
  TIsppVariant = record
    Typ: TIsppVarType;
    AsStr: string;
    AsCallContext: ICallContext;
    case TIsppVarType of
      evInt: (AsInt: Int64);
      evLValue: (AsPtr: PIsppVariant);
  end;

  TIsppParamFlags = set of (pfTypeDefined, pfHasDefault, pfByRef, pfFunc);

  TIsppMacroParam = record
    Name: string;
    ParamFlags: TIsppParamFlags;
    DefValue: TIsppVariant;
  end;

  PParamList = ^TParamList;
  TParamList = array[Byte] of TIsppMacroParam;

  TArgGroupingStyle = (agsNone, agsParenteses, agsBrackets, agsBraces);

  ICallContext = interface
    procedure Add(const Name: string; const Value: TIsppVariant);
    function Call: TIsppVariant;
    function GroupingStyle: TArgGroupingStyle;
    procedure Clone(out NewContext: ICallContext);
  end;

  TIdentType = (itUnknown, itVariable, itMacro, itFunc, itDefinedFunc,
    itTypeOfFunc, itDimOfFunc);

  IIdentManager = interface
    function GetIdent(const Name: string; out CallContext: ICallContext): TIdentType;
    function Defined(const Name: string): Boolean;
    function TypeOf(const Name: string): Byte;
    function DimOf(const Name: string): Integer;
  end;

function GetOption(const Options: TOptions; Option: Char): Boolean;
procedure SetOption(var Options: TOptions; Option: Char; Value: Boolean);

implementation

function GetOption(const Options: TOptions; Option: Char): Boolean;
begin
  Result := (Ord(UpCase(Option)) - Ord('A')) in Options
end;

procedure SetOption(var Options: TOptions; Option: Char; Value: Boolean);
begin
  if Value then
    Include(Options, Ord(UpCase(Option)) - Ord('A'))
  else
    Exclude(Options, Ord(UpCase(Option)) - Ord('A'))
end;

end.
