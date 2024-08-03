library ISCmplr;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler DLL
}

uses
  SafeDLLPath in 'Src\SafeDLLPath.pas',
  SysUtils,
  CompInt in 'Src\CompInt.pas',
  CompPreprocInt in 'Src\CompPreprocInt.pas',
  Compile in 'Src\ISCmplr\ISCmplr.Compile.pas',
  CompMsgs in 'Src\ISCmplr\ISCmplr.CompMsgs.pas',
  Struct in 'Src\Struct.pas',
  ScriptFunc in 'Src\ScriptFunc.pas',
  ScriptFunc_C in 'Src\ISCmplr\ISCmplr.ScriptFunc_C.pas',
  ScriptCompiler in 'Src\ISCmplr\ISCmplr.ScriptCompiler.pas',
  ScriptClasses_C in 'Src\ISCmplr\ISCmplr.ScriptClasses_C.pas',
  ResUpdate in 'Src\ResUpdate.pas',
  ExeUpdate in 'Src\ISCmplr\ISCmplr.ExeUpdate.pas',
  Compress in 'Src\Compress.pas',
  CompressZlib in 'Src\CompressZlib.pas',
  bzlib in 'Src\bzlib.pas',
  LZMACompressor in 'Src\ISCmplr\ISCmplr.LZMACompressor.pas',
  FileClass in 'Src\FileClass.pas',
  ArcFour in 'Src\ArcFour.pas',
  VerInfo in 'Src\VerInfo.pas',
  PathFunc in '..\Components\PathFunc.pas',
  CmnFunc2 in 'Src\CmnFunc2.pas',
  Int64Em in 'Src\Int64Em.pas',
  SHA1 in '..\Components\SHA1.pas',
  DebugStruct in 'Src\DebugStruct.pas',
  LangOptionsSectionDirectives in 'Src\LangOptionsSectionDirectives.pas',
  MsgIDs in 'Src\MsgIDs.pas',
  SetupEnt in 'Src\SetupEnt.pas',
  SetupSectionDirectives in 'Src\SetupSectionDirectives.pas',
  SetupTypes in 'Src\SetupTypes.pas',
  SimpleExpression in 'Src\SimpleExpression.pas',
  MD5 in '..\Components\MD5.pas',
  DotNetVersion in 'Src\DotNetVersion.pas';

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
