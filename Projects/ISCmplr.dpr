library ISCmplr;
{$IMAGEBASE $00800000}

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler DLL
}

uses
  SafeDLLPath in 'SafeDLLPath.pas',
  SysUtils,
  CompInt in 'CompInt.pas',
  CompPreprocInt in 'CompPreprocInt.pas',
  Compile in 'Compile.pas',
  CompMsgs in 'CompMsgs.pas',
  Struct in 'Struct.pas',
  ScriptFunc in 'ScriptFunc.pas',
  ScriptFunc_C in 'ScriptFunc_C.pas',
  ScriptCompiler in 'ScriptCompiler.pas',
  ScriptClasses_C in 'ScriptClasses_C.pas',
  ResUpdate in 'ResUpdate.pas',
  CompResUpdate in 'CompResUpdate.pas',
  Compress in 'Compress.pas',
  CompressZlib in 'CompressZlib.pas',
  bzlib in 'bzlib.pas',
  LZMA in 'LZMA.pas',
  FileClass in 'FileClass.pas',
  ArcFour in 'ArcFour.pas',
  VerInfo in 'VerInfo.pas';

{$R *.RES}

function ISDllCompileScript(const Params: TCompileScriptParamsEx): Integer;
stdcall;
begin
  Result := ISCompileScript(Params, False);
end;

{$IFDEF UNICODE}
type
  PWrapperData = ^TWrapperData;
  TWrapperData = record
    CallerParams: PCompileScriptParamsEx;
    LastLineRead: String;
  end;

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
{$ENDIF}

function ISDllGetVersion: PCompilerVersionInfo; stdcall;
begin
  Result := ISGetVersion;
end;

exports
  ISDllCompileScript{$IFDEF UNICODE} name 'ISDllCompileScriptW'{$ENDIF},
{$IFDEF UNICODE}
  ISDllCompileScriptA name 'ISDllCompileScript',
{$ENDIF}
  ISDllGetVersion;

begin
  { The user might call ISDllCompileScript from multiple threads
    simultaneously, so set our instance of the Delphi memory manager to
    thread-safe mode }
  IsMultiThread := True;
end.
