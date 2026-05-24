program ISCC;
{$APPTYPE CONSOLE}

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
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
  JSON,
  {$IFDEF STATICCOMPILER} Compiler.Compile, {$ENDIF}
  PathFunc in '..\Components\PathFunc.pas',
  TrustFunc in '..\Components\TrustFunc.pas',
  Shared.CommonFunc in 'Src\Shared.CommonFunc.pas',
  Shared.CompilerInt in 'Src\Shared.CompilerInt.pas',
  Shared.CompilerInt.Struct in 'Src\Shared.CompilerInt.Struct.pas',
  Shared.FileClass in 'Src\Shared.FileClass.pas',
  Shared.ConfigIniFile in 'Src\Shared.ConfigIniFile.pas',
  Shared.SignToolsFunc in 'Src\Shared.SignToolsFunc.pas',
  Shared.LicenseFunc in 'Src\Shared.LicenseFunc.pas',
  SHA256 in '..\Components\SHA256.pas',
  ECDSA in '..\Components\ECDSA.pas',
  ISSigFunc in '..\Components\ISSigFunc.pas',
  StringScanner in '..\Components\StringScanner.pas',
  UnsignedFunc in '..\Components\UnsignedFunc.pas';

{$SETPEOSVERSION 6.1}
{$SETPESUBSYSVERSION 6.1}
{$WEAKLINKRTTI ON}

{$R Res\ConsoleApp.manifest.res}
{$R Res\ISCC.versionandicon.res}

type
  PScriptLine = ^TScriptLine;
  TScriptLine = record
    LineText: String;
    Next: PScriptLine;
  end;

  TOptionID = 0..25;

  TOptions = set of TOptionID;

  TIsppOptions = record
    ParserOptions: TOptions;
    Options: TOptions;
    VerboseLevel: Integer;
    InlineStart: String;
    InlineEnd: String;
  end;

var
  Options: record
    ScriptFilename: String;
    Definitions, IncludePath, IncludeFiles, Output, OutputPath, OutputFilename: String;
    Quiet, ShowProgress, MessagesJsonl, NoIDESignTools, NoCompression, NoSigning, NoSignCheck, OutputPreprocessed: Boolean;
    IsppOptions: TIsppOptions;
  end;

  StdOutHandle, StdErrHandle: THandle;
  StdOutHandleIsConsole, StdErrHandleIsConsole: Boolean;
  SignTools: TStringList;
  ScriptLines, NextScriptLine: PScriptLine;
  CurLine: String;
  StartTime, EndTime: DWORD;
  WantAbort: Boolean;
  ProgressPoint: TPoint;
  LastProgress: String;
  IsppMode: Boolean;
  CompilerVersionInfo: PCompilerVersionInfo;

procedure WriteToStdHandle(const Handle: THandle; const HandleIsConsole: Boolean; S: String);
begin
  if Copy(S, 1, 1) <> #13 then
    S := S + #13#10;

  if HandleIsConsole then begin
    var CharsWritten: DWORD;
    WriteConsole(Handle, @S[1], ULength(S), CharsWritten, nil);
  end else begin
    var Utf8S := Utf8Encode(S);
    var BytesWritten: DWORD;
    WriteFile(Handle, Utf8S[1], ULength(Utf8S), BytesWritten, nil);
  end;
end;

function GetErrorOrWarningTextAttribute(const Error: Boolean): Word;
begin
  Result := FOREGROUND_INTENSITY or FOREGROUND_RED;
  if not Error then
    Result := Result or FOREGROUND_GREEN;
end;

procedure WriteStdOut(const S: String);
begin
  WriteToStdHandle(StdOutHandle, StdOutHandleIsConsole, S);
end;

procedure WriteStdErr(const S: String; const Error: Boolean = False; const Warning: Boolean = False);
begin
  var CSBI: TConsoleScreenBufferInfo;
  const DidSetColor = (Error or Warning) and StdErrHandleIsConsole and GetConsoleScreenBufferInfo(StdErrHandle, CSBI) and
                      SetConsoleTextAttribute(StdErrHandle, GetErrorOrWarningTextAttribute(Error));
  try
    WriteToStdHandle(StdErrHandle, StdErrHandleIsConsole, S);
  finally
    if DidSetColor then
      SetConsoleTextAttribute(StdErrHandle, CSBI.wAttributes);
  end;
end;

procedure WriteJsonlMessage(const Handle: THandle; const HandleIsConsole: Boolean;
  const Line: Integer; const Filename, Message: String; const Error, Warning: Boolean);
begin
  var CSBI: TConsoleScreenBufferInfo;
  const DidSetColor = (Error or Warning) and HandleIsConsole and GetConsoleScreenBufferInfo(Handle, CSBI) and
                      SetConsoleTextAttribute(Handle, GetErrorOrWarningTextAttribute(Error));
  try
    const JsonObject = TJSONObject.Create;
    try
      if Line <> 0 then
        JsonObject.AddPair('line', TJSONNumber.Create(Line)); { Delphi 10.4 does not support passing Line directly }
      if Filename <> '' then
        JsonObject.AddPair('filename', Filename);
      JsonObject.AddPair('message', Message);
      if Error then
        JsonObject.AddPair('severity', 'error')
      else if Warning then
        JsonObject.AddPair('severity', 'warning');
      WriteToStdHandle(Handle, HandleIsConsole, JsonObject.ToJSON);
    finally
      JsonObject.Free;
    end;
  finally
    if DidSetColor then
      SetConsoleTextAttribute(Handle, CSBI.wAttributes);
  end;
end;

function GetCursorPos: TPoint;
var
  CSBI: TConsoleScreenBufferInfo;
begin
  if not StdOutHandleIsConsole or not GetConsoleScreenBufferInfo(StdOutHandle, CSBI) then begin
    Result.X := -1;
    Result.Y := -1;
  end else begin
    Result.X := CSBI.dwCursorPosition.X;
    Result.Y := CSBI.dwCursorPosition.Y;
  end;
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
  if P.X >= CSBI.dwSize.X then Exit;
  if P.Y >= CSBI.dwSize.Y then Exit;
  Coords.X := SHORT(P.X);
  Coords.Y := SHORT(P.Y);
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
  AppData: NativeInt): Integer; stdcall;

  procedure PrintProgress(Progress: String);
  var
    Pt: TPoint;
  begin
    if (Progress = '') or (LastProgress = Progress) then
      Exit;

    Pt := GetCursorPos;
    if (Pt.X < 0) or (Pt.Y < 0) then
      Exit;

    if ProgressPoint.X < 0 then begin
      ProgressPoint := Pt;
      WriteStdOut('');
      Pt := GetCursorPos;
    end else if Pt.Y < ProgressPoint.Y then
      ProgressPoint.Y := Pt.Y; { Window was resized }

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
      if Data.Warning then begin
        if Options.MessagesJsonl then
          WriteJsonlMessage(StdErrHandle, StdErrHandleIsConsole, 0, '', Data.StatusMsg, False, True)
        else if not Options.Quiet or Options.OutputPreprocessed then
          WriteStdErr(Data.StatusMsg, False, True);
      end else if not Options.Quiet then
        WriteStdOut(Data.StatusMsg)
      else if Options.ShowProgress then
        PrintProgress(Trim(Data.StatusMsg));
    iscbNotifySuccess: begin
        EndTime := GetTickCount;
        if not Options.Quiet then begin
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
        if Options.MessagesJsonl then begin
          if Assigned(Data.ErrorFilename) then
            S := Data.ErrorFilename
          else
            S := Options.ScriptFilename;
          WriteJsonlMessage(StdErrHandle, StdErrHandleIsConsole, Data.ErrorLine, S, Data.ErrorMsg, True, False);
        end else begin
          S := 'Error';
          if Data.ErrorLine <> 0 then
            S := S + Format(' on line %d', [Data.ErrorLine]);
          if Assigned(Data.ErrorFilename) then
            S := S + ' in ' + Data.ErrorFilename
          else if Options.ScriptFilename <> '' then
            S := S + ' in ' + Options.ScriptFilename;
          S := S + ': ' + Data.ErrorMsg;
          WriteStdErr(S, True);
        end;
      end;
    iscbNotifyIdle:
      if Options.ShowProgress and (Data.CompressProgress <> 0) then begin
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
    iscbNotifyPreproc:
      if Options.OutputPreprocessed and (Data.PreprocessedScript <> '') then
        WriteStdOut(Data.PreprocessedScript);
  end;
end;

var InitializedCompiler: Boolean;

procedure InitCompiler;
begin
  if InitializedCompiler then
    Exit;

  {$IFNDEF STATICCOMPILER}
  try
    InitISCmplrLibrary;
  except
    begin
      WriteStdErr(Format('Could not load %s: %s', [ISCmplrDLL, GetExceptMessage]), True);
      Halt(1);
    end;
  end;
  CompilerVersionInfo := ISDllGetVersion;
  {$ELSE}
  CompilerVersionInfo := ISGetVersion;
  {$ENDIF}

  InitializedCompiler := True;
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

    Definitions := 'ISCC_INVOKED'#1'ISPPCC_INVOKED'#1;
    IncludePath := PathExtractDir(NewParamStr(0));
    IncludeFiles := '';
  end;

  procedure ShowBanner;
  begin
    WriteStdOut('Inno Setup 7' {$IFNDEF WIN64} + ' (32-bit)' {$ENDIF} + ' Command-Line Compiler');
    WriteStdOut('Copyright (C) 1997-2026 Jordan Russell. All rights reserved.');
    WriteStdOut('Portions Copyright (C) 2000-2026 Martijn Laan. All rights reserved.');
    if IsppMode then
      WriteStdOut('Portions Copyright (C) 2001-2004 Alex Yackimoff. All rights reserved.');
    WriteStdOut('https://www.innosetup.com');
    WriteStdOut('');
  end;

  procedure ReadOptionsParam(var Options: TOptions; Symbol: Char);
  var
    I: Integer;
    S: String;
  begin
    for I := 1 to NewParamCount do
    begin
      S := NewParamStr(I);
      if (Length(S) >= 2) and ((S[1] = '/') or (S[1] = '-')) and (UpCase(S[2]) = Symbol) then begin
        if (Length(S) <> 4) or not CharInSet(UpCase(S[3]), ['A'..'Z']) then begin
          ShowBanner;
          WriteStdErr('Invalid option: ' + S, True);
          Halt(1);
        end;
        case S[4] of
          '-': SetOption(Options, S[3], False);
          '+': SetOption(Options, S[3], True)
        else
          ShowBanner;
          WriteStdErr('Invalid option: ' + S, True);
          Halt(1);
        end;
      end;
    end;
  end;

  function IsParam(const S: String): Boolean;
  begin
    Result := (Length(S) >= 2) and ((S[1] = '/') or (S[1] = '-'));
  end;

  function IsLongParam(const S: String): Boolean;
  begin
    Result := (Length(S) >= 3) and (S[1] = '-') and (S[2] = '-');
  end;

  function GetParam(var S: String; const Symbols: String; const LongSymbols: String = ''): Boolean;
  begin
    Result := IsParam(S) and (CompareText(Copy(S, 2, Length(Symbols)), Symbols) = 0);
    if Result then
      S := Copy(S, 2 + Length(Symbols), MaxInt)
    else if LongSymbols <> '' then begin
      Result := IsLongParam(S) and
        (Length(S) >= 3 + Length(LongSymbols)) and
        (Copy(S, 3, Length(LongSymbols)) = LongSymbols) and
        (S[3 + Length(LongSymbols)] = '=');
      if Result then
        S := Copy(S, 3 + Length(LongSymbols) + 1, MaxInt);
    end;
  end;

  function GetFlagParam(const S: String; const Symbols: String; const LongSymbols: String = ''): Boolean;
  begin
    Result := (IsParam(S) and (CompareText(Copy(S, 2, MaxInt), Symbols) = 0)) or
      ((LongSymbols <> '') and IsLongParam(S) and (Copy(S, 3, MaxInt) = LongSymbols));
  end;

  procedure RejectSingleDashLongParam(const S: String);

    function StartsWithSingleDashLongParam(const S, LongSymbols: String): Boolean;
    begin
      const Symbols = Copy(S, 2, MaxInt);
      Result := (Length(Symbols) > Length(LongSymbols)) and
                (CompareText(Copy(Symbols, 1, Length(LongSymbols)), LongSymbols) = 0) and
                (Symbols[Length(LongSymbols) + 1] = '=');
    end;

  begin
    if not IsParam(S) or IsLongParam(S) then
      Exit;

    { Reject for example -signtool= which matches -s but was meant as --signtool=,
      and cross-collisions like -include= matching -i which is --include-dirs=.
      Without this, -signtool=x would define a signtool with name "igntool" and
      command "x". }

    if StartsWithSingleDashLongParam(S, 'output-filename') or
       StartsWithSingleDashLongParam(S, 'output-dir') or
       StartsWithSingleDashLongParam(S, 'output') or
       StartsWithSingleDashLongParam(S, 'signtool') or
       (IsppMode and (
         StartsWithSingleDashLongParam(S, 'include-dirs') or
         StartsWithSingleDashLongParam(S, 'inline-start') or
         StartsWithSingleDashLongParam(S, 'inline-end') or
         StartsWithSingleDashLongParam(S, 'include') or
         StartsWithSingleDashLongParam(S, 'define') or
         StartsWithSingleDashLongParam(S, 'verbose'))) then begin
      ShowBanner;
      const EqualsPos = Pos('=', S);
      const SuggestedParam = LowerCase(Copy(S, 2, EqualsPos - 2)) + Copy(S, EqualsPos, MaxInt);
      { The suggestion may still be invalid (for example '--output=Yes') but that's ok }
      WriteStdErr(Format('Invalid option: %s (did you mean --%s?)', [S, SuggestedParam]), True);
      Halt(1);
    end;
  end;

  procedure ShowUsage;
  begin
    WriteStdErr('Usage:  iscc [options] scriptfile.iss');
    WriteStdErr('or to read from standard input:  iscc [options] -');
    WriteStdErr('Options:');
    WriteStdErr('  --output=(yes|no), -o(+|-)           Enables or disables output (overrides Output)');
    WriteStdErr('  --output-dir=<path>, -o<path>        Outputs files to specified path (overrides OutputDir)');
    WriteStdErr('  --output-filename=<filename>, -f<filename>');
    WriteStdErr('                                       Specifies an output filename (overrides OutputBaseFilename)');
    WriteStdErr('  --signtool=<name>=<command>, -s<name>=<command>');
    WriteStdErr('                                       Sets a SignTool with the specified name and command');
    WriteStdErr('                                       (any Sign Tools configured using the Compiler IDE will be specified automatically)');
    WriteStdErr('  --no-ide-signtools, -ni              Do not auto-specify Sign Tools configured using the Compiler IDE');
    WriteStdErr('  --no-compression, -nc                Disables compression (overrides Compression and InternalCompressLevel)');
    WriteStdErr('  --no-signing, -ns                    Disables signing (overrides SignTool and SignedUninstaller)');
    WriteStdErr('  --no-signcheck, -nsc                 Disables signcheck validation');
    WriteStdErr('  --messages-jsonl, -mj                Outputs errors and warnings in JSONL format');
    WriteStdErr('                                       (warnings are not suppressed by --quiet when --messages-jsonl is active)');
    WriteStdErr('  --preprocess, -e                     Preprocesses to stdout and suppresses compilation');
    WriteStdErr('  --quiet, -q                          Suppresses messages that are normally printed to standard output (see --messages-jsonl)');
    WriteStdErr('  --quiet-progress, -qp                Same as --quiet while still displaying progress');
    if IsppMode then begin
      WriteStdErr('  --define=<name>[=<value>], -d<name>[=<value>]');
      WriteStdErr('                                       Emulates #define public <name> <value>');
      WriteStdErr('  -$<letter>(+|-)                      Emulates #pragma option -<letter>(+|-)');
      WriteStdErr('  -p<letter>(+|-)                      Emulates #pragma parseroption -<letter>(+|-)');
      WriteStdErr('  --include-dirs=<paths>, -i<paths>    Emulates #pragma include <paths>');
      WriteStdErr('                                       (multiple paths can be delimited with semicolons)');
      WriteStdErr('  --include=<filename>, -j<filename>   Emulates #include <filename>');
      WriteStdErr('  --inline-start=<string>, -{#<string> Emulates #pragma inlinestart <string>');
      WriteStdErr('  --inline-end=<string>, -}<string>    Emulates #pragma inlineend <string>');
      WriteStdErr('  --verbose=<number>, -v<number>       Emulates #pragma verboselevel <number>');
    end;
    WriteStdErr('  --help, -?                           Prints this information');
    WriteStdErr('  --version                            Prints the compiler engine version');
    WriteStdErr('');
    WriteStdErr('Examples: iscc "c:\isetup\samples\my script.iss"');
    WriteStdErr('          iscc --quiet-progress --output-dir="My Output" -f"MyProgram-1.0" -sbyparam=$p "c:\isetup\samples\my script.iss"');
    if IsppMode then
      WriteStdErr('          iscc -$c- -pu+ "--define=Lic=Trial Lic.txt" --include-dirs=C:\INC;D:\INC scriptfile.iss');
    WriteStdErr('');
  end;

var
  I: Integer;
  S: String;
begin
  if IsppMode then begin
    InitIsppOptions(Options.IsppOptions, Options.Definitions, Options.IncludePath, Options.IncludeFiles);
    { Also see below }
    ReadOptionsParam(Options.IsppOptions.Options, '$');
    ReadOptionsParam(Options.IsppOptions.ParserOptions, 'P');
  end;

  for I := 1 to NewParamCount do begin
    S := NewParamStr(I);
    if (S = '') or IsParam(S) or IsLongParam(S) then begin
      RejectSingleDashLongParam(S);
      if GetFlagParam(S, 'MJ', 'messages-jsonl') then
        Options.MessagesJsonl := True
      else if GetFlagParam(S, 'Q', 'quiet') then
        Options.Quiet := True
      else if GetFlagParam(S, 'QP', 'quiet-progress') then begin
        Options.Quiet := True;
        Options.ShowProgress := True;
      end else if GetFlagParam(S, 'O+', 'output=yes') then
        Options.Output := 'yes'
      else if GetFlagParam(S, 'O-', 'output=no') then
        Options.Output := 'no'
      else if GetParam(S, 'O', 'output-dir') then
        Options.OutputPath := S
      else if GetParam(S, 'F', 'output-filename') then
        Options.OutputFilename := S
      else if GetParam(S, 'S', 'signtool') then begin
        if Pos('=', S) = 0 then begin
          ShowBanner;
          WriteStdErr('Invalid option: ' + NewParamStr(I), True);
          Halt(1);
        end;
        SignTools.Add(S);
      end else if GetFlagParam(S, 'NC', 'no-compression') then
        Options.NoCompression := True
      else if GetFlagParam(S, 'NI', 'no-ide-signtools') then
        Options.NoIDESignTools := True
      else if GetFlagParam(S, 'NSC', 'no-signcheck') then
        Options.NoSignCheck := True
      else if GetFlagParam(S, 'NS', 'no-signing') then
        Options.NoSigning := True
      else if GetFlagParam(S, 'E', 'preprocess') then
        Options.OutputPreprocessed := True
      else if IsppMode and GetParam(S, 'D', 'define') then
        Options.Definitions := Options.Definitions + S + #1
      else if IsppMode and GetParam(S, 'I', 'include-dirs') then
        Options.IncludePath := Options.IncludePath + ';' + S
      else if IsppMode and GetParam(S, 'J', 'include') then
        Options.IncludeFiles := Options.IncludeFiles + S + #1
      else if IsppMode and GetParam(S, '{#', 'inline-start') then begin
        if S <> '' then
          Options.IsppOptions.InlineStart := S;
      end else if IsppMode and GetParam(S, '}', 'inline-end') then begin
        if S <> '' then
          Options.IsppOptions.InlineEnd := S;
      end else if IsppMode and GetParam(S, 'V', 'verbose') then begin
        if S <> '' then
          Options.IsppOptions.VerboseLevel := StrToIntDef(S, 0);
      end else if IsppMode and (GetParam(S, '$') or GetParam(S, 'P')) then begin
        { Already handled above }
      end else if GetFlagParam(S, '?', 'help') then begin
        ShowBanner;
        ShowUsage;
        Halt(0);
      end else if GetFlagParam(S, '', 'version') then begin
        InitCompiler;
        WriteStdOut(String(CompilerVersionInfo.Version));
        Halt(0);
      end else begin
        ShowBanner;
        WriteStdErr('Unknown option: ' + NewParamStr(I), True);
        Halt(1);
      end;
    end else begin
      { Not a switch; must be the script filename }
      if Options.ScriptFilename <> '' then begin
        ShowBanner;
        WriteStdErr('You may not specify more than one script filename.', True);
        Halt(1);
      end;
      Options.ScriptFilename := S;
    end;
  end;

  if Options.ScriptFilename = '' then begin
    ShowBanner;
    ShowUsage;
    Halt(1);
  end;

  if Options.OutputPreprocessed then begin
    Options.Quiet := True;
    Options.ShowProgress := False;
  end;

  if not Options.Quiet then
    ShowBanner;
end;

procedure Go;

  procedure AppendCompilerOption(var Opts: String; const OptName, OptValue: String);
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
      AppendCompilerOption(S, 'ISPP:ParserOptions', ConvertOptionsToString(ParserOptions));
      AppendCompilerOption(S, 'ISPP:Options', ConvertOptionsToString(Options));
      AppendCompilerOption(S, 'ISPP:VerboseLevel', IntToStr(VerboseLevel));
      AppendCompilerOption(S, 'ISPP:InlineStart', InlineStart);
      AppendCompilerOption(S, 'ISPP:InlineEnd', InlineEnd);
    end;

    AppendCompilerOption(S, 'ISPP:Definitions', Definitions);
    AppendCompilerOption(S, 'ISPP:IncludePath', IncludePath);
    AppendCompilerOption(S, 'ISPP:IncludeFiles', IncludeFiles);
  end;

var
  ScriptPath: String;
  ExitCode: Word;
  F: TTextFileReader;
  Params: TCompileScriptParamsEx;
  CompilerOptions: String;
  Res: Integer;
  I: Integer;
begin
  if Options.ScriptFilename <> '-' then begin
    Options.ScriptFilename := PathExpand(Options.ScriptFilename);
    ScriptPath := PathExtractPath(Options.ScriptFilename);
  end else begin
    { Read from standard input }
    Options.ScriptFilename := '<stdin>';
    ScriptPath := GetCurrentDir;
  end;

  InitCompiler;

  if CompilerVersionInfo.BinVersion < $05000500 then begin
    { 5.0.5 or later is required since we use TCompileScriptParamsEx }
    WriteStdErr('Incompatible compiler engine version.', True);
    Halt(1);
  end;

  ProgressPoint.X := -1;
  ExitCode := 0;
  try
    if Options.ScriptFilename <> '<stdin>' then
      F := TTextFileReader.Create(Options.ScriptFilename, fdOpenExisting, faRead, fsRead)
    else
      F := TTextFileReader.CreateWithExistingHandle(GetStdHandle(STD_INPUT_HANDLE));
    try
      ReadScriptLines(F);
    finally
      F.Free;
    end;

    if not Options.Quiet then begin
      WriteStdOut('Compiler engine version: ' + String(CompilerVersionInfo.Title) + ' ' + String(CompilerVersionInfo.Version));
      if IsLicensed then
        WriteStdOut('Licensee name: ' + GetLicenseeDescription)
      else
        WriteStdOut(GetLicenseeDescription);
      WriteStdOut('');
    end;

    FillChar(Params, SizeOf(Params), 0);
    Params.Size := SizeOf(Params);
    Params.SourcePath := PChar(ScriptPath);
    Params.CallbackProc := CompilerCallbackProc;
    CompilerOptions := '';
    if Options.Output <> '' then
      AppendCompilerOption(CompilerOptions, 'Output', Options.Output);
    if Options.OutputPath <> '' then
      AppendCompilerOption(CompilerOptions, 'OutputDir', Options.OutputPath);
    if Options.OutputFilename <> '' then
      AppendCompilerOption(CompilerOptions, 'OutputBaseFilename', Options.OutputFilename);

    for I := 0 to SignTools.Count-1 do
      CompilerOptions := CompilerOptions + AddSignToolParam(SignTools[I]);

    if not Options.NoIDESignTools then begin
      const IDESignTools = TStringList.Create;
      try
        { Also automatically read and add SignTools defined using the IDE. Adding
          these after the command line SignTools so that the latter are always
          found first by the compiler. }
        ReadSignTools(IDESignTools);
        for I := 0 to IDESignTools.Count-1 do
          CompilerOptions := CompilerOptions + AddSignToolParam(IDESignTools[I]);
      finally
        IDESignTools.Free;
      end;
    end;

    if Options.NoCompression then
      AppendCompilerOption(CompilerOptions, 'NoCompression', 'true');

    if Options.NoSigning then
      AppendCompilerOption(CompilerOptions, 'NoSigning', 'true');

    if Options.NoSignCheck then
      AppendCompilerOption(CompilerOptions, 'NoSignCheck', 'true');

    if Options.OutputPreprocessed then
      AppendCompilerOption(CompilerOptions, 'PreprocessOnly', 'true');

    if IsppMode then
      IsppOptionsToString(CompilerOptions, Options.IsppOptions, Options.Definitions, Options.IncludePath, Options.IncludeFiles);

    Params.Options := PChar(CompilerOptions);

    StartTime := GetTickCount;
    {$IFNDEF STATICCOMPILER}
    Res := ISDllCompileScript(Params);
    {$ELSE}
    Res := ISCompileScript(Params, False);
    {$ENDIF}
    if Options.ShowProgress and (ProgressPoint.X >= 0) then
      WriteStdOut('');

    case Res of
      isceNoError: ;
      isceCompileFailure: begin
          ExitCode := 2;
          if Options.MessagesJsonl then
            WriteJsonlMessage(StdErrHandle, StdErrHandleIsConsole, 0, '', 'Compile aborted.', True, False)
          else
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

function ISPPInstalled: Boolean;
begin
  Result := NewFileExists(PathExtractPath(NewParamStr(0)) + 'ISPP.dll');
end;

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  SignTools := TStringList.Create;
  try
    StdOutHandle := GetStdHandle(STD_OUTPUT_HANDLE);
    StdErrHandle := GetStdHandle(STD_ERROR_HANDLE);
    var Mode: DWORD;
    StdOutHandleIsConsole := GetConsoleMode(StdOutHandle, Mode);
    StdErrHandleIsConsole := GetConsoleMode(StdErrHandle, Mode);
    SetConsoleCtrlHandler(@ConsoleCtrlHandler, True);
    try
      ReadLicense;
      IsppMode := ISPPInstalled;
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
