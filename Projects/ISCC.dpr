program ISCC;
{$APPTYPE CONSOLE}

{
  Inno Setup
  Copyright (C) 1997-2011 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Command-line compiler
}

{x$DEFINE STATICCOMPILER}
{ For debugging purposes, remove the 'x' to have it link the compiler code
  into this program and not depend on ISCmplr.dll. }

uses
  SafeDLLPath in 'SafeDLLPath.pas',
  Windows, SysUtils,
  {$IFDEF STATICCOMPILER} Compile, {$ENDIF}
  PathFunc, CmnFunc2, CompInt, FileClass;

{$R *.res}
{$R ISCC.manifest.res}

{$I VERSION.INC}

type
  PScriptLine = ^TScriptLine;
  TScriptLine = record
    LineText: String;
    Next: PScriptLine;
  end;

var
  StdOutHandle, StdErrHandle: THandle;
  ScriptFilename: String;
  OutputPath, OutputFilename, SignTool: String;
  ScriptLines, NextScriptLine: PScriptLine;
  CurLine: String;
  StartTime, EndTime: DWORD;
  Quiet, WantAbort: Boolean;

procedure WriteToStdHandle(const H: THandle; S: AnsiString);
var
  BytesWritten: DWORD;
begin
  S := S + #13#10;
  WriteFile(H, S[1], Length(S), BytesWritten, nil);
end;

procedure WriteStdOut(const S: String);
begin
  WriteToStdHandle(StdOutHandle, AnsiString(S));
end;

procedure WriteStdErr(const S: String);
begin
  WriteToStdHandle(StdErrHandle, AnsiString(S));
end;

function ConsoleCtrlHandler(dwCtrlType: DWORD): BOOL; stdcall;
begin
  { Abort gracefully when Ctrl+C/Break is pressed }
  WantAbort := True;
  Result := True;
end;

procedure ReadScriptLines(const F: TTextFileReader);
var
  LineNumber: Integer;
  PrevLine, L: PScriptLine;
begin
  LineNumber := 1;
  PrevLine := nil;
  while not F.Eof do begin
    New(L);
    try
      L.LineText := F.ReadLine;
      if Pos(#0, L.LineText) <> 0 then
        raise Exception.CreateFmt('Illegal null character on line %d', [LineNumber]); 
      L.Next := nil;
    except
      Dispose(L);
      raise;
    end;
    if Assigned(PrevLine) then
      PrevLine.Next := L
    else begin
      ScriptLines := L;
      NextScriptLine := L;
    end;
    PrevLine := L;
    Inc(LineNumber);
  end;
end;

procedure FreeScriptLines;
var
  L, NextLine: PScriptLine;
begin
  L := ScriptLines;
  ScriptLines := nil;
  NextScriptLine := nil;
  while Assigned(L) do begin
    NextLine := L.Next;
    Dispose(L);
    L := NextLine;
  end;
end;

function CompilerCallbackProc(Code: Integer; var Data: TCompilerCallbackData;
  AppData: Longint): Integer; stdcall;
var
  S: String;
begin
  if WantAbort then begin
    Result := iscrRequestAbort;
    Exit;
  end;
  Result := iscrSuccess;
  case Code of
    iscbReadScript: begin
        { Note: In Inno Setup 3.0.1 and later we can ignore Data.Reset since
          it is only True once (when reading the first line). }
        if Assigned(NextScriptLine) then begin
          CurLine := NextScriptLine.LineText;
          NextScriptLine := NextScriptLine.Next;
          Data.LineRead := PChar(CurLine);
        end;
      end;
    iscbNotifyStatus:
      if not Quiet then
        WriteStdOut(Data.StatusMsg);
    iscbNotifySuccess: begin
        EndTime := GetTickCount;
        if not Quiet then begin
          WriteStdOut('');
          WriteStdOut(Format('Successful compile (%.3f sec). ' +
            'Resulting Setup program filename is:',
            [(EndTime - StartTime) / 1000]));
          WriteStdOut(Data.OutputExeFilename);
        end;
      end;
    iscbNotifyError:
      if Assigned(Data.ErrorMsg) then begin
        S := 'Error';
        if Data.ErrorLine <> 0 then
          S := S + Format(' on line %d', [Data.ErrorLine]);
        if Assigned(Data.ErrorFilename) then
          S := S + ' in ' + Data.ErrorFilename
        else if ScriptFilename <> '' then
          S := S + ' in ' + ScriptFilename;
        S := S + ': ' + Data.ErrorMsg;
        WriteStdErr(S);
      end;
  end;
end;

procedure ProcessCommandLine;

  procedure ShowBanner;
  begin
    WriteStdOut('Inno Setup 5 Command-Line Compiler');
    WriteStdOut('Copyright (C) 1997-2012 Jordan Russell. All rights reserved.');
    WriteStdOut('Portions Copyright (C) 2000-2012 Martijn Laan');
    WriteStdOut('');
  end;

  procedure ShowUsage;
  begin
    WriteStdErr('Usage:  iscc [options] scriptfile.iss');
    WriteStdErr('or to read from standard input:  iscc [options] -');
    WriteStdErr('Options:  /Oc:\path      Output files to specified path (overrides OutputDir)');
    WriteStdErr('          /Ffilename     Overrides OutputBaseFilename with the specified filename');
    WriteStdErr('          /Sname=command Sets a SignTool with the specified name and command');
    WriteStdErr('          /Q             Quiet compile (print error messages only)');
    WriteStdErr('          /?             Show this help screen');
  end;

var
  I: Integer;
  S: String;
begin
  for I := 1 to NewParamCount do begin
    S := NewParamStr(I);
    if (S = '') or (S[1] = '/') then begin
      if CompareText(S, '/Q') = 0 then
        Quiet := True
      else if CompareText(Copy(S, 1, 2), '/O') = 0 then
        OutputPath := Copy(S, 3, MaxInt)
      else if CompareText(Copy(S, 1, 2), '/F') = 0 then
        OutputFilename := Copy(S, 3, MaxInt)
      else if CompareText(Copy(S, 1, 2), '/S') = 0 then begin
        SignTool := Copy(S, 3, MaxInt);
        if Pos('=', SignTool) = 0 then begin
          ShowBanner;
          WriteStdErr('Invalid option: ' + S);
          Halt(1);
        end;
      end
      else if S = '/?' then begin
        ShowBanner;
        ShowUsage;
        Halt(1);
      end
      else begin
        ShowBanner;
        WriteStdErr('Unknown option: ' + S);
        Halt(1);
      end;
    end
    else begin
      { Not a switch; must be the script filename }
      if ScriptFilename <> '' then begin
        ShowBanner;
        WriteStdErr('You may not specify more than one script filename.');
        Halt(1);
      end;
      ScriptFilename := S;
    end;
  end;

  if ScriptFilename = '' then begin
    ShowBanner;
    ShowUsage;
    Halt(1);
  end;

  if not Quiet then
    ShowBanner;
end;

procedure Go;
var
  ScriptPath: String;
  ExitCode: Integer;
  Ver: PCompilerVersionInfo;
  F: TTextFileReader;
  Params: TCompileScriptParamsEx;
  Options: String;
  Res: Integer;
begin
  if ScriptFilename <> '-' then begin
    ScriptFilename := PathExpand(ScriptFilename);
    ScriptPath := PathExtractPath(ScriptFilename);
  end
  else begin
    { Read from standard input }
    ScriptFilename := '<stdin>';
    ScriptPath := GetCurrentDir;
  end;

  {$IFNDEF STATICCOMPILER}
  Ver := ISDllGetVersion;
  {$ELSE}
  Ver := ISGetVersion;
  {$ENDIF}
  if Ver.BinVersion < $05000500 then begin
    { 5.0.5 or later is required since we use TCompileScriptParamsEx }
    WriteStdErr('Incompatible compiler engine version.');
    Halt(1);
  end;

  ExitCode := 0;
  try
    if ScriptFilename <> '<stdin>' then
      F := TTextFileReader.Create(ScriptFilename, fdOpenExisting, faRead, fsRead)
    else
      F := TTextFileReader.CreateWithExistingHandle(GetStdHandle(STD_INPUT_HANDLE));
    try
      ReadScriptLines(F);
    finally
      F.Free;
    end;

    if not Quiet then begin
      WriteStdOut('Compiler engine version: ' + String(Ver.Title) + ' ' + String(Ver.Version));
      WriteStdOut('');
    end;

    FillChar(Params, SizeOf(Params), 0);
    Params.Size := SizeOf(Params);
    Params.SourcePath := PChar(ScriptPath);
    Params.CallbackProc := CompilerCallbackProc;
    Options := '';
    if OutputPath <> '' then
      Options := Options + 'OutputDir=' + OutputPath + #0;
    if OutputFilename <> '' then
      Options := Options + 'OutputBaseFilename=' + OutputFilename + #0;
    if SignTool <> '' then
      Options := Options + 'SignTool-' + SignTool + #0;
    Params.Options := PChar(Options);

    StartTime := GetTickCount;
    {$IFNDEF STATICCOMPILER}
    Res := ISDllCompileScript(Params);
    {$ELSE}
    Res := ISCompileScript(Params, False);
    {$ENDIF}
    case Res of
      isceNoError: ;
      isceCompileFailure: begin
          ExitCode := 2;
          WriteStdErr('Compile aborted.');
        end;
    else
      ExitCode := 1;
      WriteStdErr(Format('Internal error: ISDllCompileScript returned ' +
        'unexpected result (%d).', [Res]));
    end;
  finally
    FreeScriptLines;
  end;
  if ExitCode <> 0 then
    Halt(ExitCode);
end;

begin
  StdOutHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  StdErrHandle := GetStdHandle(STD_ERROR_HANDLE);
  SetConsoleCtrlHandler(@ConsoleCtrlHandler, True);
  try
    ProcessCommandLine;
    Go;
  except
    { Show a friendlier exception message. (By default, Delphi prints out
      the exception class and address.) }
    WriteStdErr(GetExceptMessage);
    Halt(2);
  end;
end.
