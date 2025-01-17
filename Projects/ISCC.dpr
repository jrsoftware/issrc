program ISCC;
{$APPTYPE CONSOLE}

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Command-line compiler
}

{x$DEFINE STATICCOMPILER}
{ For debugging purposes, remove the 'x' to have it link the compiler code
  into this program and not depend on ISCmplr.dll. You will also need to add the
  ..\Components and Src folders to the Delphi Compiler Search path in the project
  options. Also see IDE.MainForm's STATICCOMPILER and Compiler.Compile's STATICPREPROC. }

uses
  SafeDLLPath in '..\Components\SafeDLLPath.pas',
  Windows,
  SysUtils,
  Classes,
  {$IFDEF STATICCOMPILER} Compiler.Compile, {$ENDIF}
  PathFunc in '..\Components\PathFunc.pas',
  Shared.CommonFunc in 'Src\Shared.CommonFunc.pas',
  Shared.CompilerInt in 'Src\Shared.CompilerInt.pas',
  Shared.FileClass in 'Src\Shared.FileClass.pas',
  Shared.ConfigIniFile in 'Src\Shared.ConfigIniFile.pas',
  Shared.SignToolsFunc in 'Src\Shared.SignToolsFunc.pas',
  Shared.Int64Em in 'Src\Shared.Int64Em.pas';

{$SETPEOSVERSION 6.1}
{$SETPESUBSYSVERSION 6.1}
{$WEAKLINKRTTI ON}

{$R Res\ISCC.manifest.res}
{$R Res\ISCC.versionandicon.res}

type
  PScriptLine = ^TScriptLine;
  TScriptLine = record
    LineText: String;
    Next: PScriptLine;
  end;

  TOptionID = 0..25;

  TOptions = packed set of TOptionID;

  TIsppOptions = packed record
    ParserOptions: TOptions;
    Options: TOptions;
    VerboseLevel: Byte;
    InlineStart: string[7];
    InlineEnd: string[7];
    SpanSymbol: AnsiChar;
  end;

var
  StdOutHandle, StdErrHandle: THandle;
  StdOutHandleIsConsole, StdErrHandleIsConsole: Boolean;
  ScriptFilename: String;
  Definitions, IncludePath, IncludeFiles, Output, OutputPath, OutputFilename: String;
  SignTools: TStringList;
  ScriptLines, NextScriptLine: PScriptLine;
  CurLine: String;
  StartTime, EndTime: DWORD;
  Quiet, ShowProgress, WantAbort: Boolean;
  ProgressPoint: TPoint;
  LastProgress: String;
  IsppOptions: TIsppOptions;
  IsppMode: Boolean;

procedure WriteToStdHandle(const Handle: THandle; const HandleIsConsole: Boolean; S: String);
begin
  if Copy(S, 1, 1) <> #13 then
    S := S + #13#10;

  if HandleIsConsole then begin
    var CharsWritten: DWORD;
    WriteConsole(Handle, @S[1], Length(S), CharsWritten, nil);
  end else begin
    var Utf8S := Utf8Encode(S);
    var BytesWritten: DWORD;
    WriteFile(Handle, Utf8S[1], Length(Utf8S), BytesWritten, nil);
  end;
end;

procedure WriteStdOut(const S: String; const Warning: Boolean = False);
var
  CSBI: TConsoleScreenBufferInfo;
  DidSetColor: Boolean;
begin
  DidSetColor := Warning and StdOutHandleIsConsole and GetConsoleScreenBufferInfo(StdOutHandle, CSBI) and
                 SetConsoleTextAttribute(StdOutHandle, FOREGROUND_INTENSITY or FOREGROUND_RED or FOREGROUND_GREEN);
  try
    WriteToStdHandle(StdOutHandle, StdOutHandleIsConsole, S);
  finally
    if DidSetColor then
      SetConsoleTextAttribute(StdOutHandle, CSBI.wAttributes);
  end;
end;

procedure WriteStdErr(const S: String; const Error: Boolean = False);
var
  CSBI: TConsoleScreenBufferInfo;
  DidSetColor: Boolean;
begin
  DidSetColor := Error and StdErrHandleIsConsole and GetConsoleScreenBufferInfo(StdErrHandle, CSBI) and
                 SetConsoleTextAttribute(StdErrHandle, FOREGROUND_INTENSITY or FOREGROUND_RED);
  try
    WriteToStdHandle(StdErrHandle, StdErrHandleIsConsole, S);
  finally
    if DidSetColor then
      SetConsoleTextAttribute(StdErrHandle, CSBI.wAttributes);
  end;
end;

function GetCursorPos: TPoint;
var
  CSBI: TConsoleScreenBufferInfo;
begin
  if not StdOutHandleIsConsole or not GetConsoleScreenBufferInfo(StdOutHandle, CSBI) then
    Exit;
  Result.X := CSBI.dwCursorPosition.X;
  Result.Y := CSBI.dwCursorPosition.Y;
end;

procedure SetCursorPos(const P: TPoint);
var
  Coords: TCoord;
  CSBI: TConsoleScreenBufferInfo;
begin
  if not StdOutHandleIsConsole or not GetConsoleScreenBufferInfo(StdOutHandle, CSBI) then
    Exit;
  if P.X < 0 then Exit;
  if P.Y < 0 then Exit;
  if P.X > CSBI.dwSize.X then Exit;
  if P.Y > CSBI.dwSize.Y then Exit;
  Coords.X := P.X;
  Coords.Y := P.Y;
  SetConsoleCursorPosition(StdOutHandle, Coords);
end;

procedure WriteProgress(const S: String);
var
  CSBI: TConsoleScreenBufferInfo;
  Str: String;
begin
  if StdOutHandleIsConsole and GetConsoleScreenBufferInfo(StdOutHandle, CSBI) then begin
    if Length(S) > CSBI.dwSize.X then
      Str := Copy(S, 1, CSBI.dwSize.X)
    else
      Str := Format('%-' + IntToStr(CSBI.dwSize.X) + 's', [S]);
  end else
    Str := S;

  WriteToStdHandle(StdOutHandle, StdOutHandleIsConsole, Str);
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

  procedure PrintProgress(Progress: String);
  var
    Pt: TPoint;
  begin
    if (Progress = '') or (LastProgress = Progress) then
      Exit;

    Pt := GetCursorPos;

    if Pt.Y <= ProgressPoint.Y then
      Exit
    else if ProgressPoint.X < 0 then begin
      ProgressPoint := Pt;
      WriteStdOut('');
      Pt := GetCursorPos;
    end;

    SetCursorPos(ProgressPoint);
    WriteProgress(#13 + Progress);
    LastProgress := Progress;
    SetCursorPos(Pt);
  end;

var
  S, BytesCompressedPerSecond, SecondsRemaining: String;
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
        WriteStdOut(Data.StatusMsg, Data.Warning)
      else if ShowProgress then
        PrintProgress(Trim(Data.StatusMsg));
    iscbNotifySuccess: begin
        EndTime := GetTickCount;
        if not Quiet then begin
          WriteStdOut('');
          if Data.OutputExeFilename <> '' then begin
            WriteStdOut(Format('Successful compile (%.3f sec). ' +
              'Resulting Setup program filename is:',
              [(EndTime - StartTime) / 1000]));
            WriteStdOut(Data.OutputExeFilename);
          end else
            WriteStdOut(Format('Successful compile (%.3f sec). ' +
              'Output was disabled.',
              [(EndTime - StartTime) / 1000]));
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
        WriteStdErr(S, True);
      end;
    iscbNotifyIdle:
      if ShowProgress and (Data.CompressProgress <> 0) then begin
        if Data.BytesCompressedPerSecond <> 0 then
          BytesCompressedPerSecond := Format(' at %.2f kb/s', [Data.BytesCompressedPerSecond / 1024])
        else
          BytesCompressedPerSecond := '';
        if Data.SecondsRemaining <> -1 then
          SecondsRemaining := Format(', %d seconds remaining', [Data.SecondsRemaining])
        else
          SecondsRemaining := '';
        PrintProgress(Format('Compressing: %.2f%% done%s%s', [Data.CompressProgress / Data.CompressProgressMax * 100, BytesCompressedPerSecond, SecondsRemaining]));
      end;
  end;
end;

procedure ProcessCommandLine;

  procedure SetOption(var Options: TOptions; Option: Char; Value: Boolean);
  begin
    if Value then
      Include(Options, Ord(UpCase(Option)) - Ord('A'))
    else
      Exclude(Options, Ord(UpCase(Option)) - Ord('A'))
  end;

  procedure InitIsppOptions(var Opt: TIsppOptions; var Definitions, IncludePath, IncludeFiles: String);
  begin
    with Opt do begin
      SetOption(Options, 'C', True);
      SetOption(ParserOptions, 'B', True);
      SetOption(ParserOptions, 'P', True);
      VerboseLevel := 0;
      InlineStart := '{#';
      InlineEnd := '}';
    end;

    Definitions := 'ISPPCC_INVOKED'#1;
    IncludePath := ExtractFileDir(NewParamStr(0));
    IncludeFiles := '';
  end;

  procedure ReadOptionsParam(var Options: TOptions; Symbol: Char);
  var
    I: Integer;
    S: String;
  begin
    for I := 1 to NewParamCount do
    begin
      S := NewParamStr(I);
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

  function GetParam(var S: String; Symbols: String): Boolean;
  begin
    Result := IsParam(S) and
      (CompareText(Copy(S, 2, Length(Symbols)), Symbols) = 0);
    if Result then
      S := Copy(S, 2 + Length(Symbols), MaxInt);
  end;

  function FindParam(var Index: Integer; Symbols: String): String;
  var
    I: Integer;
    S: String;
  begin
    for I := Index to NewParamCount do
    begin
      S := NewParamStr(I);
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

  procedure ShowBanner;
  begin
    WriteStdOut('Inno Setup 6 Command-Line Compiler');
    WriteStdOut('Copyright (C) 1997-2025 Jordan Russell. All rights reserved.');
    WriteStdOut('Portions Copyright (C) 2000-2025 Martijn Laan. All rights reserved.');
    if IsppMode then
      WriteStdOut('Portions Copyright (C) 2001-2004 Alex Yackimoff. All rights reserved.');
    WriteStdOut('https://www.innosetup.com');
    WriteStdOut('');
  end;

  procedure ShowUsage;
  begin
    WriteStdErr('Usage:  iscc [options] scriptfile.iss');
    WriteStdErr('or to read from standard input:  iscc [options] -');
    WriteStdErr('Options:');
    WriteStdErr('  /O(+|-)            Enable or disable output (overrides Output)');
    WriteStdErr('  /O<path>           Output files to specified path (overrides OutputDir)');
    WriteStdErr('  /F<filename>       Specifies an output filename (overrides OutputBaseFilename)');
    WriteStdErr('  /S<name>=<command> Sets a SignTool with the specified name and command');
    WriteStdErr('                     (Any Sign Tools configured using the Compiler IDE will be specified automatically)');
    WriteStdErr('  /Q                 Quiet compile (print error messages only)');
    WriteStdErr('  /Qp                Enable quiet compile while still displaying progress');
    if IsppMode then begin
      WriteStdErr('  /D<name>[=<value>] Emulate #define public <name> <value>');
      WriteStdErr('  /$<letter>(+|-)    Emulate #pragma option -<letter>(+|-)');
      WriteStdErr('  /P<letter>(+|-)    Emulate #pragma parseroption -<letter>(+|-)');
      WriteStdErr('  /I<paths>          Emulate #pragma include <paths>');
      WriteStdErr('  /J<filename>       Emulate #include <filename>');
      WriteStdErr('  /{#<string>        Emulate #pragma inlinestart <string>');
      WriteStdErr('  /}<string>         Emulate #pragma inlineend <string>');
      WriteStdErr('  /V<number>         Emulate #pragma verboselevel <number>');
    end;
    WriteStdErr('  /?                 Show this help screen');
    WriteStdErr('');
    WriteStdErr('Examples: iscc "c:\isetup\samples\my script.iss"');
    WriteStdErr('          iscc /Qp /O"My Output" /F"MyProgram-1.0" /Sbyparam=$p "c:\isetup\samples\my script.iss"');
    if IsppMode then begin
      WriteStdErr('          iscc /$c- /Pu+ "/DLic=Trial Lic.txt" /IC:\INC;D:\INC scriptfile.iss');
      WriteStdErr('');
    end;
  end;

var
  I: Integer;
  S: String;
begin
  if IsppMode then begin
    InitIsppOptions(IsppOptions, Definitions, IncludePath, IncludeFiles);
    { Also see below }
    ReadOptionsParam(IsppOptions.Options, '$');
    ReadOptionsParam(IsppOptions.ParserOptions, 'P');
  end;

  for I := 1 to NewParamCount do begin
    S := NewParamStr(I);
    if (S = '') or IsParam(S) then begin
      if GetParam(S, 'Q') then begin
        Quiet := True;
        ShowProgress := CompareText(S, 'P') = 0;
      end
      else if GetParam(S, 'O') then begin
        if S = '-' then Output := 'no'
        else if S = '+' then Output := 'yes'
        else OutputPath := S;
      end
      else if GetParam(S, 'F') then
        OutputFilename := S
      else if GetParam(S, 'S') then begin
        if Pos('=', S) = 0 then begin
          ShowBanner;
          WriteStdErr('Invalid option: ' + S, True);
          Halt(1);
        end;
        SignTools.Add(S);
      end else if IsppMode and GetParam(S, 'D') then begin
        Definitions := Definitions + S + #1;
      end
      else if IsppMode and GetParam(S, 'I') then begin
        IncludePath := IncludePath + ';' + S;
      end
      else if IsppMode and GetParam(S, 'J') then begin
        IncludeFiles := IncludeFiles + S + #1;
      end
      else if IsppMode and GetParam(S, '{#') then begin
        if S <> '' then IsppOptions.InlineStart := AnsiString(S);
      end
      else if IsppMode and GetParam(S, '}') then begin
        if S <> '' then IsppOptions.InlineEnd := AnsiString(S);
      end
      else if IsppMode and GetParam(S, 'V') then begin
        if S <> '' then IsppOptions.VerboseLevel := StrToIntDef(S, 0);
      end
      else if IsppMode and (GetParam(S, '$') or GetParam(S, 'P')) then begin
        { Already handled above }
      end
      else if S = '/?' then begin
        ShowBanner;
        ShowUsage;
        Halt(1);
      end
      else begin
        ShowBanner;
        WriteStdErr('Unknown option: ' + S, True);
        Halt(1);
      end;
    end
    else begin
      { Not a switch; must be the script filename }
      if ScriptFilename <> '' then begin
        ShowBanner;
        WriteStdErr('You may not specify more than one script filename.', True);
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

  procedure AppendOption(var Opts: String; const OptName, OptValue: String);
  begin
    Opts := Opts + OptName + '=' + OptValue + #0;
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

  procedure IsppOptionsToString(var S: String; Opt: TIsppOptions; Definitions, IncludePath, IncludeFiles: String);
  begin
    with Opt do begin
      AppendOption(S, 'ISPP:ParserOptions', ConvertOptionsToString(ParserOptions));
      AppendOption(S, 'ISPP:Options', ConvertOptionsToString(Options));
      AppendOption(S, 'ISPP:VerboseLevel', IntToStr(VerboseLevel));
      AppendOption(S, 'ISPP:InlineStart', String(InlineStart));
      AppendOption(S, 'ISPP:InlineEnd', String(InlineEnd));
    end;

    AppendOption(S, 'ISPP:Definitions', Definitions);
    AppendOption(S, 'ISPP:IncludePath', IncludePath);
    AppendOption(S, 'ISPP:IncludeFiles', IncludeFiles);
  end;

var
  ScriptPath: String;
  ExitCode: Integer;
  Ver: PCompilerVersionInfo;
  F: TTextFileReader;
  Params: TCompileScriptParamsEx;
  Options: String;
  Res: Integer;
  I: Integer;
  IDESignTools: TStringList;
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
    WriteStdErr('Incompatible compiler engine version.', True);
    Halt(1);
  end;

  ProgressPoint.X := -1;
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
    if Output <> '' then
      AppendOption(Options, 'Output', Output);
    if OutputPath <> '' then
      AppendOption(Options, 'OutputDir', OutputPath);
    if OutputFilename <> '' then
      AppendOption(Options, 'OutputBaseFilename', OutputFilename);

    for I := 0 to SignTools.Count-1 do
      Options := Options + AddSignToolParam(SignTools[I]);

    IDESignTools := TStringList.Create;
    try
      { Also automatically read and add SignTools defined using the IDE. Adding
        these after the command line SignTools so that the latter are always
        found first by the compiler. }
      ReadSignTools(IDESignTools);
      for I := 0 to IDESignTools.Count-1 do
        Options := Options + AddSignToolParam(IDESignTools[I]);
    finally
      IDESignTools.Free;
    end;

    if IsppMode then
      IsppOptionsToString(Options, IsppOptions, Definitions, IncludePath, IncludeFiles);

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
          WriteStdErr('Compile aborted.', True);
        end;
    else
      ExitCode := 1;
      WriteStdErr(Format('Internal error: ISDllCompileScript returned ' +
        'unexpected result (%d).', [Res]), True);
    end;
  finally
    FreeScriptLines;
  end;
  if ExitCode <> 0 then
    Halt(ExitCode);
end;

begin
  SignTools := TStringList.Create;
  try
    StdOutHandle := GetStdHandle(STD_OUTPUT_HANDLE);
    StdErrHandle := GetStdHandle(STD_ERROR_HANDLE);
    var Mode: DWORD;
    StdOutHandleIsConsole := GetConsoleMode(StdOutHandle, Mode);
    StdErrHandleIsConsole := GetConsoleMode(StdErrHandle, Mode);
    SetConsoleCtrlHandler(@ConsoleCtrlHandler, True);
    try
      IsppMode := FileExists(ExtractFilePath(NewParamStr(0)) + 'ispp.dll');
      ProcessCommandLine;
      Go;
    except
      { Show a friendlier exception message. (By default, Delphi prints out
        the exception class and address.) }
      WriteStdErr(GetExceptMessage, True);
      Halt(2);
    end;
  finally
    SignTools.Free;
  end;
end.
