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
  ISCmplrLibrary: HMODULE;

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
  if TrustedFileExists(FileName) then begin
    ISCmplrLibrary := SafeLoadLibrary(PChar(FileName), SEM_NOOPENFILEERRORBOX);
    if ISCmplrLibrary <> 0 then begin
      ISDllCompileScript := GetProcAddress(ISCmplrLibrary, 'ISDllCompileScriptW');
      ISDllGetVersion := GetProcAddress(ISCmplrLibrary, 'ISDllGetVersion');
      if not Assigned(ISDllCompileScript) or not Assigned(ISDllGetVersion) then begin
        FreeLibrary(ISCmplrLibrary);
        ISCmplrLibrary := 0;
        ISDllCompileScript := nil;
        ISDllGetVersion := nil;
      end;
    end;
  end;
end.
