library ISCmplr;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler DLL
}

uses
  SafeDLLPath in '..\Components\SafeDLLPath.pas',
  SysUtils,
  Shared.CompilerInt.Struct in 'Src\Shared.CompilerInt.Struct.pas',
  Shared.PreprocInt in 'Src\Shared.PreprocInt.pas',
  Compiler.Compile in 'Src\Compiler.Compile.pas',
  Compiler.SetupCompiler in 'Src\Compiler.SetupCompiler.pas',
  Compiler.Messages in 'Src\Compiler.Messages.pas',
  Compiler.StringLists in 'Src\Compiler.StringLists.pas',
  Compiler.StringLists.Test in 'Src\Compiler.StringLists.Test.pas',
  Compiler.CompressionHandler in 'Src\Compiler.CompressionHandler.pas',
  Compiler.HelperFunc in 'Src\Compiler.HelperFunc.pas',
  Compiler.BuiltinPreproc in 'Src\Compiler.BuiltinPreproc.pas',
  Shared.Struct in 'Src\Shared.Struct.pas',
  Shared.ScriptFunc in 'Src\Shared.ScriptFunc.pas',
  Compiler.ScriptFunc in 'Src\Compiler.ScriptFunc.pas',
  Compiler.ScriptCompiler in 'Src\Compiler.ScriptCompiler.pas',
  Compiler.ScriptClasses in 'Src\Compiler.ScriptClasses.pas',
  Compiler.ExeUpdateFunc in 'Src\Compiler.ExeUpdateFunc.pas',
  Compression.Base in 'Src\Compression.Base.pas',
  Compression.Zlib in 'Src\Compression.Zlib.pas',
  Compression.bzlib in 'Src\Compression.bzlib.pas',
  Compression.LZMACompressor in 'Src\Compression.LZMACompressor.pas',
  Shared.FileClass in 'Src\Shared.FileClass.pas',
  ChaCha20 in '..\Components\ChaCha20.pas',
  Shared.VerInfoFunc in 'Src\Shared.VerInfoFunc.pas',
  PathFunc in '..\Components\PathFunc.pas',
  TrustFunc in '..\Components\TrustFunc.pas',
  Shared.CommonFunc in 'Src\Shared.CommonFunc.pas',
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
  PBKDF2 in '..\Components\PBKDF2.pas',
  ECDSA in '..\Components\ECDSA.pas',
  ISSigFunc in '..\Components\ISSigFunc.pas',
  StringScanner in '..\Components\StringScanner.pas',
  Shared.EncryptionFunc in 'Src\Shared.EncryptionFunc.pas',
  UnsignedFunc in '..\Components\UnsignedFunc.pas',
  uPSC_classes in '..\Components\UniPs\Source\uPSC_classes.pas',
  uPSC_comobj in '..\Components\UniPs\Source\uPSC_comobj.pas',
  uPSC_controls in '..\Components\UniPs\Source\uPSC_controls.pas',
  uPSC_dll in '..\Components\UniPs\Source\uPSC_dll.pas',
  uPSC_extctrls in '..\Components\UniPs\Source\uPSC_extctrls.pas',
  uPSC_forms in '..\Components\UniPs\Source\uPSC_forms.pas',
  uPSC_graphics in '..\Components\UniPs\Source\uPSC_graphics.pas',
  uPSC_std in '..\Components\UniPs\Source\uPSC_std.pas',
  uPSC_stdctrls in '..\Components\UniPs\Source\uPSC_stdctrls.pas',
  uPSCompiler in '..\Components\UniPs\Source\uPSCompiler.pas',
  uPSUtils in '..\Components\UniPs\Source\uPSUtils.pas';

{$IMAGEBASE $00800000}
{$SETPEOSVERSION 6.1}
{$SETPESUBSYSVERSION 6.1}
{$WEAKLINKRTTI ON}

{$R Res\ISCmplr.images.res}
{$R Res\ISCmplr.images.dark.res}
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
  AppData: NativeInt): Integer;
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
          Data.LineRead := PChar(WrapperData.LastLineRead);
        end;
      end;
    iscbNotifyStatus:
      begin
        if Assigned(Data.StatusMsg) then begin
          AnsiStatusMsg := AnsiString(Data.StatusMsg);
          Data.StatusMsg := PChar(PAnsiChar(AnsiStatusMsg));
        end;
        Result := CallerParams.CallbackProc(Code, Data, CallerParams.AppData);
      end;
    iscbNotifySuccess:
      begin
        if Assigned(Data.OutputExeFilename) then begin
          AnsiOutputExeFilename := AnsiString(Data.OutputExeFilename);
          Data.OutputExeFilename := PChar(PAnsiChar(AnsiOutputExeFilename));
        end;
        Result := CallerParams.CallbackProc(Code, Data, CallerParams.AppData);
      end;
    iscbNotifyError:
      begin
        if Assigned(Data.ErrorMsg) then begin
          AnsiErrorMsg := AnsiString(Data.ErrorMsg);
          Data.ErrorMsg := PChar(PAnsiChar(AnsiErrorMsg));
        end;
        if Assigned(Data.ErrorFilename) then begin
          AnsiErrorFilename := AnsiString(Data.ErrorFilename);
          Data.ErrorFilename := PChar(PAnsiChar(AnsiErrorFilename));
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
  CompilerPath, SourcePath, Options: String;
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
    UMove(Params, WrapperParams^, Params.Size);
    WrapperParams.CallbackProc := WrapperCallbackProc;
    WrapperParams.AppData := NativeInt(@WrapperData);
    if Assigned(Params.CompilerPath) then begin
      CompilerPath := String(PAnsiChar(Params.CompilerPath));
      WrapperParams.CompilerPath := PChar(CompilerPath);
    end;
    if Assigned(Params.SourcePath) then begin
      SourcePath := String(PAnsiChar(Params.SourcePath));
      WrapperParams.SourcePath := PChar(SourcePath);
    end;
    if (Params.Size <> SizeOf(TCompileScriptParams)) and Assigned(Params.Options) then begin
      P := PAnsiChar(Params.Options);
      while P^ <> #0 do begin
        Options := Options + String(P) + #0;
        Inc(P, StrLen(P) + 1);
      end;
      WrapperParams.Options := PChar(Options);
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
