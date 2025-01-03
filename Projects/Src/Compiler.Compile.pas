unit Compiler.Compile;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler DLL interface functions which wrap TSetupCompiler
}

interface

uses
  Shared.CompilerInt;

function ISCompileScript(const Params: TCompileScriptParamsEx;
  const PropagateExceptions: Boolean): Integer;
function ISGetVersion: PCompilerVersionInfo;

implementation

uses
  Windows, SysUtils, Classes, PathFunc,
  Shared.Struct, Shared.CommonFunc, Compiler.SetupCompiler;

function GetSelfFilename: String;
{ Returns Filename of the calling DLL or application. (ParamStr(0) can only
  return the filename of the calling application.) }
var
  Buf: array[0..MAX_PATH-1] of Char;
begin
  SetString(Result, Buf, GetModuleFileName(HInstance, Buf, SizeOf(Buf)))
end;

function ISCompileScript(const Params: TCompileScriptParamsEx;
  const PropagateExceptions: Boolean): Integer;

  function CheckParams(const Params: TCompileScriptParamsEx): Boolean;
  begin
    Result := ((Params.Size = SizeOf(Params)) or
               (Params.Size = SizeOf(TCompileScriptParams))) and
              Assigned(Params.CallbackProc);
  end;

  procedure InitializeSetupCompiler(const SetupCompiler: TSetupCompiler;
    const Params: TCompileScriptParamsEx);
  begin
    SetupCompiler.AppData := Params.AppData;
    SetupCompiler.CallbackProc := Params.CallbackProc;
    if Assigned(Params.CompilerPath) then
      SetupCompiler.CompilerDir := Params.CompilerPath
    else
      SetupCompiler.CompilerDir := PathExtractPath(GetSelfFilename);
    SetupCompiler.SourceDir := Params.SourcePath;
  end;

  function EncodeIncludedFilenames(const IncludedFilenames: TStringList): String;
  var
    S: String;
    I: Integer;
  begin
    S := '';
    for I := 0 to IncludedFilenames.Count-1 do
     S := S + IncludedFilenames[I] + #0;
    Result := S;
  end;

  procedure NotifyPreproc(const SetupCompiler: TSetupCompiler);
  var
    Data: TCompilerCallbackData;
    S: String;
  begin
    Data.PreprocessedScript := PChar(SetupCompiler.GetPreprocOutput);
    S := EncodeIncludedFilenames(SetupCompiler.GetPreprocIncludedFilenames);
    Data.IncludedFilenames := PChar(S);
    Params.CallbackProc(iscbNotifyPreproc, Data, Params.AppData);
  end;

  procedure NotifySuccess(const SetupCompiler: TSetupCompiler);
  var
    Data: TCompilerCallbackData;
  begin
    Data.OutputExeFilename := PChar(SetupCompiler.GetExeFilename);
    var DebugInfo := SetupCompiler.GetDebugInfo;
    Data.DebugInfo := DebugInfo.Memory;
    Data.DebugInfoSize := DebugInfo.Size;
    Params.CallbackProc(iscbNotifySuccess, Data, Params.AppData);
  end;

  procedure NotifyError(const SetupCompiler: TSetupCompiler);
  var
    Data: TCompilerCallbackData;
    S: String;
  begin
    Data.ErrorMsg := nil;
    Data.ErrorFilename := nil;
    Data.ErrorLine := 0;
    if not(ExceptObject is EAbort) then begin
      S := GetExceptMessage;
      Data.ErrorMsg := PChar(S);
      { use a Pointer cast instead of PChar so that we'll get a null
        pointer if the string is empty }
      Data.ErrorFilename := Pointer(SetupCompiler.GetLineFilename);
      Data.ErrorLine := SetupCompiler.GetLineNumber;
    end;
    Params.CallbackProc(iscbNotifyError, Data, Params.AppData);
  end;

var
  SetupCompiler: TSetupCompiler;
  P: PChar;
  P2: Integer;
begin
  if not CheckParams(Params) then begin
    Result := isceInvalidParam;
    Exit;
  end;
  SetupCompiler := TSetupCompiler.Create(nil);
  try
    InitializeSetupCompiler(SetupCompiler, Params);

    { Parse Options (only present in TCompileScriptParamsEx) }
    if (Params.Size <> SizeOf(TCompileScriptParams)) and Assigned(Params.Options) then begin
      P := Params.Options;
      while P^ <> #0 do begin
        if StrLIComp(P, 'Output=', Length('Output=')) = 0 then begin
          Inc(P, Length('Output='));
          var Output: Boolean;
          if TryStrToBoolean(P, Output) then
            SetupCompiler.SetOutput(Output)
          else begin
            { Bad option }
            Result := isceInvalidParam;
            Exit;
          end;
        end else if StrLIComp(P, 'OutputDir=', Length('OutputDir=')) = 0 then begin
          Inc(P, Length('OutputDir='));
          SetupCompiler.SetOutputDir(P);
        end else if StrLIComp(P, 'OutputBaseFilename=', Length('OutputBaseFilename=')) = 0 then begin
          Inc(P, Length('OutputBaseFilename='));
          SetupCompiler.SetOutputBaseFilename(P);
        end else if StrLIComp(P, 'SignTool-', Length('SignTool-')) = 0 then begin
          Inc(P, Length('SignTool-'));
          P2 := Pos('=', P);
          if (P2 <> 0) then
            SetupCompiler.AddSignTool(Copy(P, 1, P2-1), Copy(P, P2+1, MaxInt))
          else begin
            { Bad option }
            Result := isceInvalidParam;
            Exit;
          end;
        end else if StrLIComp(P, 'ISPP:', Length('ISPP:')) = 0 then
          SetupCompiler.AddPreprocOption(P)
        else begin
          { Unknown option }
          Result := isceInvalidParam;
          Exit;
        end;
        Inc(P, StrLen(P) + 1);
      end;
    end;

    try
      try
        SetupCompiler.Compile;
      finally
        NotifyPreproc(SetupCompiler);
      end;
      Result := isceNoError;
      NotifySuccess(SetupCompiler);
    except
      Result := isceCompileFailure;
      NotifyError(SetupCompiler);
      if PropagateExceptions then
        raise;
    end;
  finally
    SetupCompiler.Free;
  end;
end;

function ISGetVersion: PCompilerVersionInfo;
const
  Ver: TCompilerVersionInfo =
   (Title: SetupTitle; Version: SetupVersion; BinVersion: SetupBinVersion);
begin
  Result := @Ver;
end;

end.
