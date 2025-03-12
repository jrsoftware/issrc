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

var
  ISCmplrLibary: HMODULE;

{ The ISDllCompileScript function begins compilation of a script. See the above
  description of the TCompileScriptParams record. Return value is one of the
  isce* constants. }
  ISDllCompileScript: function(const Params: TCompileScriptParamsEx): Integer; stdcall;

{ The ISDllGetVersion returns a pointer to a TCompilerVersionInfo record which
  contains information about the compiler version. }
  ISDllGetVersion: function: PCompilerVersionInfo; stdcall;

implementation

uses
  Windows,
  SysUtils,
  PathFunc, TrustFunc;

initialization
  var FileName := AddBackslash(PathExtractPath(ParamStr(0))) + ISCmplrDLL;
  if TrustedFile(FileName) then begin
    ISCmplrLibary := SafeLoadLibrary(PChar(FileName), SEM_NOOPENFILEERRORBOX);
    if ISCmplrLibary <> 0 then begin
      ISDllCompileScript := GetProcAddress(ISCmplrLibary, 'ISDllCompileScriptW');
      ISDllGetVersion := GetProcAddress(ISCmplrLibary, 'ISDllGetVersion');
      if not Assigned(ISDllCompileScript) or not Assigned(ISDllGetVersion) then begin
        FreeLibrary(ISCmplrLibary);
        ISCmplrLibary := 0;
        ISDllCompileScript := nil;
        ISDllGetVersion := nil;
      end;
    end;
  end;
end.
