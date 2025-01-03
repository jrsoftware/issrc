library ISCmplr;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler DLL
}

uses
  SafeDLLPath in '..\Components\SafeDLLPath.pas',
  SysUtils,
  Shared.CompilerInt in 'Src\Shared.CompilerInt.pas',
  Shared.PreprocInt in 'Src\Shared.PreprocInt.pas',
  Compiler.Compile in 'Src\Compiler.Compile.pas',
  Compiler.SetupCompiler in 'Src\Compiler.SetupCompiler.pas',
  Compiler.Messages in 'Src\Compiler.Messages.pas',
  Compiler.StringLists in 'Src\Compiler.StringLists.pas',
  Compiler.CompressionHandler in 'Src\Compiler.CompressionHandler.pas',
  Compiler.HelperFunc in 'Src\Compiler.HelperFunc.pas',
  Compiler.BuiltinPreproc in 'Src\Compiler.BuiltinPreproc.pas',
  Shared.Struct in 'Src\Shared.Struct.pas',
  Shared.ScriptFunc in 'Src\Shared.ScriptFunc.pas',
  Compiler.ScriptFunc in 'Src\Compiler.ScriptFunc.pas',
  Compiler.ScriptCompiler in 'Src\Compiler.ScriptCompiler.pas',
  Compiler.ScriptClasses in 'Src\Compiler.ScriptClasses.pas',
  Shared.ResUpdateFunc in 'Src\Shared.ResUpdateFunc.pas',
  Compiler.ExeUpdateFunc in 'Src\Compiler.ExeUpdateFunc.pas',
  Compression.Base in 'Src\Compression.Base.pas',
  Compression.Zlib in 'Src\Compression.Zlib.pas',
  Compression.bzlib in 'Src\Compression.bzlib.pas',
  Compression.LZMACompressor in 'Src\Compression.LZMACompressor.pas',
  Shared.FileClass in 'Src\Shared.FileClass.pas',
  ChaCha20 in '..\Components\ChaCha20.pas',
  Shared.VerInfoFunc in 'Src\Shared.VerInfoFunc.pas',
  PathFunc in '..\Components\PathFunc.pas',
  Shared.CommonFunc in 'Src\Shared.CommonFunc.pas',
  Shared.Int64Em in 'Src\Shared.Int64Em.pas',
  SHA256 in '..\Components\SHA256.pas',
  Shared.DebugStruct in 'Src\Shared.DebugStruct.pas',
  Shared.LangOptionsSectionDirectives in 'Src\Shared.LangOptionsSectionDirectives.pas',
  Shared.SetupMessageIDs in 'Src\Shared.SetupMessageIDs.pas',
  Shared.SetupEntFunc in 'Src\Shared.SetupEntFunc.pas',
  Shared.SetupSectionDirectives in 'Src\Shared.SetupSectionDirectives.pas',
  Shared.SetupTypes in 'Src\Shared.SetupTypes.pas',
  Shared.SetupSteps in 'Src\Shared.SetupSteps.pas',
  SimpleExpression in '..\Components\SimpleExpression.pas',
  Shared.DotNetVersion in 'Src\Shared.DotNetVersion.pas',
  PBKDF2 in '..\Components\PBKDF2.pas';

{$IMAGEBASE $00800000}
{$SETPEOSVERSION 6.1}
{$SETPESUBSYSVERSION 6.1}
{$WEAKLINKRTTI ON}

{$R Res\ISCmplr.images.res}
{$R Res\ISCmplr.version.res}

function ISDllCompileScript(const Params: TCompileScriptParamsEx): Integer;
stdcall;
begin
  Result := ISCompileScript(Params, False);
end;

type
  PWrapperData = ^TWrapperData;
  TWrapperData = record
    CallerParams: PCompileScriptParamsEx;
    LastLineRead: String;
  end;

{ Does not support iscbNotifyPreproc }
function WrapperCallbackProc(Code: Integer; var Data: TCompilerCallbackData;
  AppData: Longint): Integer;
stdcall;
var
  WrapperData: PWrapperData;
  CallerParams: PCompileScriptParamsEx;
  AnsiStatusMsg, AnsiOutputExeFilename, AnsiErrorMsg, AnsiErrorFilename: AnsiString;
begin
  WrapperData := PWrapperData(AppData);
  CallerParams := WrapperData.CallerParams;

  case Code of
    iscbReadScript:
      begin
        Result := CallerParams.CallbackProc(Code, Data, CallerParams.AppData);
        if Assigned(Data.LineRead) then begin
          WrapperData.LastLineRead := String(PAnsiChar(Data.LineRead));
          Data.LineRead := PWideChar(WrapperData.LastLineRead);
        end;
      end;
    iscbNotifyStatus:
      begin
        if Assigned(Data.StatusMsg) then begin
          AnsiStatusMsg := AnsiString(Data.StatusMsg);
          Data.StatusMsg := PWideChar(PAnsiChar(AnsiStatusMsg));
        end;
        Result := CallerParams.CallbackProc(Code, Data, CallerParams.AppData);
      end;
    iscbNotifySuccess:
      begin
        if Assigned(Data.OutputExeFilename) then begin
          AnsiOutputExeFilename := AnsiString(Data.OutputExeFilename);
          Data.OutputExeFilename := PWideChar(PAnsiChar(AnsiOutputExeFilename));
        end;
        Result := CallerParams.CallbackProc(Code, Data, CallerParams.AppData);
      end;
    iscbNotifyError:
      begin
        if Assigned(Data.ErrorMsg) then begin
          AnsiErrorMsg := AnsiString(Data.ErrorMsg);
          Data.ErrorMsg := PWideChar(PAnsiChar(AnsiErrorMsg));
        end;
        if Assigned(Data.ErrorFilename) then begin
          AnsiErrorFilename := AnsiString(Data.ErrorFilename);
          Data.ErrorFilename := PWideChar(PAnsiChar(AnsiErrorFilename));
        end;
        Result := CallerParams.CallbackProc(Code, Data, CallerParams.AppData);
      end;
  else
    Result := CallerParams.CallbackProc(Code, Data, CallerParams.AppData);
  end;
end;

function ISDllCompileScriptA(const Params: TCompileScriptParamsEx): Integer;
stdcall;
var
  WrapperData: TWrapperData;
  WrapperParams: PCompileScriptParamsEx;
  P: PAnsiChar;
  Options: String;
begin
  if ((Params.Size <> SizeOf(Params)) and
      (Params.Size <> SizeOf(TCompileScriptParams))) or
     not Assigned(Params.CallbackProc) then begin
    Result := isceInvalidParam;
    Exit;
  end;
  WrapperData.CallerParams := @Params;
  GetMem(WrapperParams, Params.Size);
  try
    Move(Params, WrapperParams^, Params.Size);
    WrapperParams.CallbackProc := WrapperCallbackProc;
    WrapperParams.AppData := Integer(@WrapperData);
    if Assigned(Params.CompilerPath) then
      WrapperParams.CompilerPath := PWideChar(String(PAnsiChar(Params.CompilerPath)));
    if Assigned(Params.SourcePath) then
      WrapperParams.SourcePath := PWideChar(String(PAnsiChar(Params.SourcePath)));
    if (Params.Size <> SizeOf(TCompileScriptParams)) and Assigned(Params.Options) then begin
      P := PAnsiChar(Params.Options);
      while P^ <> #0 do begin
        Options := Options + String(P) + #0;
        Inc(P, StrLen(P) + 1);
      end;
      WrapperParams.Options := PWideChar(Options);
    end;
    Result := ISCompileScript(WrapperParams^, False);
  finally
    FreeMem(WrapperParams);
  end;
end;

function ISDllGetVersion: PCompilerVersionInfo; stdcall;
begin
  Result := ISGetVersion;
end;

exports
  ISDllCompileScript name 'ISDllCompileScriptW',
  ISDllCompileScriptA name 'ISDllCompileScript',
  ISDllGetVersion;

begin
  { The user might call ISDllCompileScript from multiple threads
    simultaneously, so set our instance of the Delphi memory manager to
    thread-safe mode }
  IsMultiThread := True;
end.
