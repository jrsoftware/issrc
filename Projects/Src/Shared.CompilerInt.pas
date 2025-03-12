unit Shared.CompilerInt;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler interface
}

interface

uses
  Shared.CompilerInt.Struct;

const
  ISCmplrDLL = 'ISCmplr.dll';

{ The ISDllCompileScript function begins compilation of a script. See the above
  description of the TCompileScriptParams record. Return value is one of the
  isce* constants. }
function ISDllCompileScript(const Params: TCompileScriptParamsEx): Integer;
  stdcall; external ISCmplrDLL name 'ISDllCompileScriptW';

{ The ISDllGetVersion returns a pointer to a TCompilerVersionInfo record which
  contains information about the compiler version. }
function ISDllGetVersion: PCompilerVersionInfo; stdcall; external ISCmplrDLL;

implementation

end.
