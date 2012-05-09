{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff
  $Id: ISPPCC.dpr,v 1.29 2011/03/16 08:43:35 mlaan Exp $
}

program ISPPCC;
{$APPTYPE CONSOLE}

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Command-line compiler

  $xId: ISCC.dpr,v 1.8 2002/04/11 00:06:52 jr Exp $
}

uses
  SafeDLLPath in '..\SafeDLLPath.pas',
  Windows,
  SysUtils,
  PathFunc in '..\..\Components\PathFunc.pas',
  CmnFunc2 in '..\CmnFunc2.pas',
  FileClass in '..\FileClass.pas',
  IsppIntf in 'IsppIntf.pas',
  IsppBase in 'IsppBase.pas',
  CompInt in '..\CompInt.pas',
  Int64Em in '..\Int64Em.pas';

{$R *.res}
{$R ISPPCC.manifest.res}

{$I ..\VERSION.INC}

type
  PScriptLine = ^TScriptLine;
  TScriptLine = record
    LineText: String;
    Next: PScriptLine;
  end;

var
  StdOutHandle, StdErrHandle: THandle;
  ScriptFilename: String;
  ScriptLines, NextScriptLine: PScriptLine;
  CurLine: String;
  StartTime, EndTime: DWORD;
  Quiet, WantAbort: Boolean;
  Options: TIsppOptions;

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

procedure PopulateOptions(var Options: TOptions; Symbol: Char);
var
  I: Integer;
  S: string;
begin
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if Length(S) = 4 then
    if ((S[1] = '/') or (S[1] = '-')) and (UpCase(S[2]) = Symbol) then
      case S[4] of
        '-': SetOption(Options, S[3], False);
        '+': SetOption(Options, S[3], True)
      else
        raise Exception.CreateFmt('Invalid command line option: %s', [S]);
      end;
  end;
end;

function IsParam(const S: String): Boolean;
begin
  Result := (Length(S) >= 2) and ((S[1] = '/') or (S[1] = '-'));
end;

function FindParam(var Index: Integer; Symbols: String): String;
var
  I: Integer;
  S: string;
begin
  for I := Index to ParamCount do
  begin
    S := ParamStr(I);
    if IsParam(S) and (CompareText(Copy(S, 2, Length(Symbols)), Symbols) = 0) then
    begin
      Result := Copy(S, 2 + Length(Symbols), MaxInt);
      Index := I + 1;
      Exit;
    end;
  end;
  Index := MaxInt;
  Result := '';
end;

function ConvertOptionsToString(const Options: TOptions): String;
var
  I: TOptionID;
begin
  Result := '';
  for I := 0 to 25 do
    if I in Options then
      Result := Result + Chr(Ord('a') + I);
end;

procedure AppendOption(var Opts: String; const OptName, OptValue: String);
begin
  Opts := Opts + OptName + '=' + OptValue + #0;
end;

procedure Go;

  procedure ShowBanner;
  begin
    WriteStdOut('Inno Setup 5 Command-Line Compiler');
    WriteStdOut('Copyright (C) 1997-2012 Jordan Russell. All rights reserved.');
    WriteStdOut('Portions Copyright (C) 2000-2012 Martijn Laan');
    WriteStdOut('Inno Setup Preprocessor');
    WriteStdOut('Copyright (C) 2001-2004 Alex Yackimoff. All rights reserved.');
    WriteStdOut('');
  end;

  procedure ShowUsage;
  begin
    WriteStdErr('Usage:  iscc [options] scriptfile.iss');
    WriteStdErr('or to read from standard input:  iscc [options] -');
    WriteStdErr('Options:  Emulate a #define or #pragma directive:');
    WriteStdErr('            /d<name>[=<value>]   #define public <name> <value>');
    WriteStdErr('            /$<letter>(+|-)      #pragma option -<letter>(+|-)');
    WriteStdErr('            /p<letter>(+|-)      #pragma parseroption -<letter>(+|-)');
    WriteStdErr('            /i<paths>            #pragma include <paths>');
    WriteStdErr('            /{#<string>          #pragma inlinestart <string>');
    WriteStdErr('            /}<string>           #pragma inlineend <string>');
    WriteStdErr('            /v<number>           #pragma verboselevel <number>');
    WriteStdErr('          Output files to specified path (overrides OutputDir):');
    WriteStdErr('            /o<path>');
    WriteStdErr('          Override OutputBaseFilename with the specified filename:');
    WriteStdErr('            /f<filename>');
    WriteStdErr('          Set a SignTool with the specified name and command:');
    WriteStdErr('            /s<name>=<command>');
    WriteStdErr('          Quiet compile (print error messages only):');
    WriteStdErr('            /q');
    WriteStdErr('');
    WriteStdErr('Example: iscc /$c- /pu+ "/dLic=Trial Lic.txt" /iC:\INC;D:\INC scriptfile.iss');
  end;

var
  ScriptPath: String;
  ExitCode: Integer;
  Ver: PCompilerVersionInfo;
  F: TTextFileReader;
  Params: TCompileScriptParamsEx;
  I: Integer;
  S, IncludePath, Definitions, OutputPath, OutputFilename, SignTool: string;
  Res: Integer;
begin
  I := 1;
  FindParam(I, 'Q');
  Quiet := I <> MaxInt;

  if (ParamCount < 1) or (ParamStr(1) = '/?') then begin
    ShowBanner;
    ShowUsage;
    Halt(1);
  end;

  if not Quiet then
    ShowBanner;

  ScriptFilename := '';

  for I := 1 to ParamCount do begin
    S := ParamStr(I);
    if not IsParam(S) then begin
      if ScriptFilename <> '' then
        raise Exception.Create('Script file name specified more than once');
      ScriptFileName := S;
    end;
  end;

  if ScriptFilename = '' then
    raise Exception.Create('Script file name not specified');

  if ScriptFilename <> '-' then begin
    ScriptFilename := PathExpand(ScriptFilename);
    ScriptPath := PathExtractPath(ScriptFilename);
  end
  else begin
    { Read from standard input }
    ScriptFilename := '<stdin>';
    ScriptPath := GetCurrentDir;
  end;

  Ver := ISDllGetVersion;
  if Ver.BinVersion < $05000500 then begin
    { 5.0.5 or later is required since we use TCompileScriptParamsEx }
    WriteStdErr('Incompatible compiler engine version.');
    Halt(1);
  end;

  SetOption(Options.Options, 'C', True);
  SetOption(Options.ParserOptions.Options, 'B', True);
  SetOption(Options.ParserOptions.Options, 'P', True);
  Options.VerboseLevel := 0;
  Options.InlineStart := '{#';
  Options.InlineEnd := '}';

  PopulateOptions(Options.Options, '$');
  PopulateOptions(Options.ParserOptions.Options, 'p');

  I := 1;
  Definitions := 'ISPPCC_INVOKED';
  S := FindParam(I, 'D');
  while S <> '' do
  begin
    if (Pos(';', S) > 0) or (Pos(' ', S) > 0) then
      S := AnsiQuotedStr(S, '"');
    Definitions := Definitions + ';' + S;
    S := FindParam(I, 'D')
  end;
  I := 1;
  IncludePath := ExtractFileDir(ParamStr(0));
  S := FindParam(I, 'I');
  while S <> '' do
  begin
    IncludePath := IncludePath + ';' + S;
    S := FindParam(I, 'I');
  end;
  I := 1; S := FindParam(I, '{#');
  if S <> '' then Options.InlineStart := AnsiString(S);
  I := 1; S := FindParam(I, '}');
  if S <> '' then Options.InlineEnd := AnsiString(S);
  I := 1; S := FindParam(I, 'V');
  if S <> '' then Options.VerboseLevel := StrToIntDef(S, 0);
  I := 1; OutputPath := FindParam(I, 'O');
  I := 1; OutputFileName := FindParam(I, 'F');
  I := 1; SignTool := FindParam(I, 'S');

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
    S := '';
    if OutputPath <> '' then
      AppendOption(S, 'OutputDir', OutputPath);
    if OutputFilename <> '' then
      AppendOption(S, 'OutputBaseFilename', OutputFilename);
    if SignTool <> '' then
      S := S + 'SignTool-' + SignTool + #0;
    AppendOption(S, 'ISPP:ParserOptions', ConvertOptionsToString(Options.ParserOptions.Options));
    AppendOption(S, 'ISPP:Options', ConvertOptionsToString(Options.Options));
    AppendOption(S, 'ISPP:VerboseLevel', IntToStr(Options.VerboseLevel));
    AppendOption(S, 'ISPP:InlineStart', String(Options.InlineStart));
    AppendOption(S, 'ISPP:InlineEnd', String(Options.InlineEnd));
    AppendOption(S, 'ISPP:IncludePath', IncludePath);
    AppendOption(S, 'ISPP:Definitions', Definitions);
    Params.Options := PChar(S);

    StartTime := GetTickCount;
    Res := ISDllCompileScript(Params);
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
    Go;
  except
    { Show a friendlier exception message. (By default, Delphi prints out
      the exception class and address.) }
    WriteStdErr(GetExceptMessage);
    Halt(2);
  end;
end.
