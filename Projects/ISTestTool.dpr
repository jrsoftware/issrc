program ISTestTool;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Internal "istesttool" utility
}

uses
  SafeDLLPath in '..\Components\SafeDLLPath.pas',
  SysUtils,
  Classes,
  Windows,
  Compression.Base in 'Src\Compression.Base.pas',
  ISPP.Consts in 'Src\ISPP.Consts.pas',
  Shared.FileClass in 'Src\Shared.FileClass.pas',
  Shared.SetupTypes in 'Src\Shared.SetupTypes.pas',
  Shared.Struct in 'Src\Shared.Struct.pas',
  ECDSA in '..\Components\ECDSA.pas',
  BidiUtils in '..\Components\BidiUtils.pas',
  BidiUtils.Test in '..\Components\BidiUtils.Test.pas',
  ChaCha20 in '..\Components\ChaCha20.pas',
  ChaCha20.Test in '..\Components\ChaCha20.Test.pas',
  Compiler.StringLists in 'Src\Compiler.StringLists.pas',
  Compiler.StringLists.Test in 'Src\Compiler.StringLists.Test.pas',
  ISPP.CTokenizer in 'Src\ISPP.CTokenizer.pas',
  ISPP.CTokenizer.Test in 'Src\ISPP.CTokenizer.Test.pas',
  ISSigFunc in '..\Components\ISSigFunc.pas',
  ISSigFunc.Test in '..\Components\ISSigFunc.Test.pas',
  MD5 in '..\Components\MD5.pas',
  MD5.Test in '..\Components\MD5.Test.pas',
  ModernColors in '..\Components\ModernColors.pas',
  ModernColors.Test in '..\Components\ModernColors.Test.pas',
  PathFunc in '..\Components\PathFunc.pas',
  PathFunc.Test in '..\Components\PathFunc.Test.pas',
  PBKDF2 in '..\Components\PBKDF2.pas',
  PBKDF2.Test in '..\Components\PBKDF2.Test.pas',
  Setup.PathRedir in 'Src\Setup.PathRedir.pas',
  Setup.PathRedir.Test in 'Src\Setup.PathRedir.Test.pas',
  SHA1 in '..\Components\SHA1.pas',
  SHA1.Test in '..\Components\SHA1.Test.pas',
  SHA256 in '..\Components\SHA256.pas',
  SHA256.Test in '..\Components\SHA256.Test.pas',
  Shared.CommonFunc in 'Src\Shared.CommonFunc.pas',
  Shared.CommonFunc.Test in 'Src\Shared.CommonFunc.Test.pas',
  Shared.EncryptionFunc in 'Src\Shared.EncryptionFunc.pas',
  Shared.EncryptionFunc.Test in 'Src\Shared.EncryptionFunc.Test.pas',
  SimpleExpression in '..\Components\SimpleExpression.pas',
  SimpleExpression.Test in '..\Components\SimpleExpression.Test.pas',
  StringScanner in '..\Components\StringScanner.pas',
  StringScanner.Test in '..\Components\StringScanner.Test.pas',
  UnsignedFunc in '..\Components\UnsignedFunc.pas',
  UnsignedFunc.Test in '..\Components\UnsignedFunc.Test.pas';

{$APPTYPE CONSOLE}
{$SETPEOSVERSION 6.1}
{$SETPESUBSYSVERSION 6.1}
{$WEAKLINKRTTI ON}

{$R Res\ConsoleApp.manifest.res}
{$R Res\ISTestTool.versionandicon.res}

var
  Options: record
    Quiet: Boolean;
  end;

  StdOutHandle, StdErrHandle: THandle;
  StdOutHandleIsConsole, StdErrHandleIsConsole: Boolean;

procedure RaiseFatalError(const Msg: String);
begin
  raise Exception.Create(Msg);
end;

procedure RaiseFatalErrorFmt(const Msg: String; const Args: array of const);
begin
  raise Exception.CreateFmt(Msg, Args);
end;

procedure Print(const Handle: THandle; const HandleIsConsole: Boolean;
  S: String; const IncludeNewLine: Boolean); overload;
begin
  if IncludeNewLine then
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

procedure Print(const S: String; const IncludeNewLine: Boolean = True); overload;
begin
  Print(StdOutHandle, StdOutHandleIsConsole, S, IncludeNewLine);
end;

procedure PrintErrOutput(const S: String; const IncludeNewLine: Boolean = True); overload;
begin
  Print(StdErrHandle, StdErrHandleIsConsole, S, IncludeNewLine);
end;

procedure PrintUnlessQuiet(const S: String;
  const IncludeNewLine: Boolean = True);
begin
  if not Options.Quiet then
    Print(S, IncludeNewLine);
end;

procedure PrintFmtUnlessQuiet(const S: String; const Args: array of const;
  const IncludeNewLine: Boolean = True);
begin
  if not Options.Quiet then
    Print(Format(S, Args), IncludeNewLine);
end;

procedure ShowUsage;
begin
  PrintErrOutput('Inno Setup Test Tool');
  PrintErrOutput('Copyright (C) 1997-2026 Jordan Russell. All rights reserved.');
  PrintErrOutput('Portions Copyright (C) 2000-2026 Martijn Laan. All rights reserved.');
  PrintErrOutput('https://www.innosetup.com');
  PrintErrOutput('');
  PrintErrOutput('Usage:  istesttool [options] [<test-script-filename>]');
  PrintErrOutput('Options:');
  PrintErrOutput('  --quiet, -q Suppresses status messages that are normally printed to standard output');
  PrintErrOutput('  --help, -?  Prints this information');
  PrintErrOutput('');
end;

procedure RunAndWaitLog(const S: String; const Error, FirstLine: Boolean;
  const Data: NativeInt);
begin
  if S <> '' then
    PrintUnlessQuiet('      ' + S);
end;

procedure CommandTest(const ATestScriptFilename: String);

  procedure ScriptRunTests;

    procedure StrictDeleteFile(const Path: String);
    begin
      if not Windows.DeleteFile(PChar(Path)) then begin
        const LastError = GetLastError;
        if LastError <> ERROR_FILE_NOT_FOUND then
          RaiseFatalErrorFmt('StrictDeleteFile(%s): DeleteFile failed (Error %d: %s)',
            [Path, LastError, Win32ErrorString(LastError)]);
      end;
    end;

    function RunAndWait(const CmdLine: String): DWORD;
    begin
      var StartupInfo: TStartupInfo;
      FillChar(StartupInfo, SizeOf(StartupInfo), 0);
      StartupInfo.cb := SizeOf(StartupInfo);
      StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
      StartupInfo.wShowWindow := SW_SHOWNORMAL;

      var OutputReader := TCreateProcessOutputReader.Create(RunAndWaitLog, 0);
      try
        const InheritHandles = True;
        const dwCreationFlags: DWORD = CREATE_DEFAULT_ERROR_MODE or CREATE_NO_WINDOW;
        OutputReader.UpdateStartupInfo(StartupInfo);

        var ProcessInfo: TProcessInformation;
        if not CreateProcess(nil, PChar(CmdLine), nil, nil, InheritHandles,
           dwCreationFlags, nil, nil, StartupInfo, ProcessInfo) then begin
          const LastError = GetLastError;
          RaiseFatalErrorFmt('RunAndWait(%s): CreateProcess failed (Error %d: %s)',
            [CmdLine, LastError, Win32ErrorString(LastError)]);
        end;
        CloseHandle(ProcessInfo.hThread);
        OutputReader.NotifyCreateProcessDone;
        try
          while True do begin
            case WaitForSingleObject(ProcessInfo.hProcess, 50) of
              WAIT_OBJECT_0: Break;
              WAIT_TIMEOUT: OutputReader.Read(False);
            else
              RaiseFatalError('RunAndWait: WaitForSingleObject failed');
            end;
          end;
          OutputReader.Read(True);
          if not GetExitCodeProcess(ProcessInfo.hProcess, Result) then
            RaiseFatalError('RunAndWait: GetExitCodeProcess failed');
        finally
          CloseHandle(ProcessInfo.hProcess);
        end;
      finally
        OutputReader.Free;
      end;
    end;

    function ReadFileContents(const Path: String): String;
    begin
      const F = TStringList.Create;
      try
        F.LoadFromFile(Path);
        Result := Trim(F.Text);
      finally
        F.Free;
      end;
    end;

  begin
    const ExePath = AddBackslash(PathExtractPath(ParamStr(0)));
    const TestSetupExeFilename = ExePath + 'Script.Test-Setup.exe';
    const TestSetupLogFilename = ExePath + 'Script.Test-Setup.log';
    const TestSetupResultFilename = ExePath + 'Script.Test-Result.txt';

    try
      PrintUnlessQuiet('   Compiling test Setup');

      StrictDeleteFile(TestSetupExeFilename);

      var TestScriptFilename := ATestScriptFilename;
      if TestScriptFilename = '' then
        TestScriptFilename := ExePath + 'Script.Test.iss';
      const Arch = {$IFDEF CPUX64} 'x64' {$ELSE} 'x86' {$ENDIF};

      const CompileExit = RunAndWait(AddQuotes(ExePath + 'ISCC.exe') + ' /Darch=' + Arch + ' ' +
        '/O' + AddQuotes(ExePath) + ' ' + AddQuotes(TestScriptFilename));
      if CompileExit <> 0 then
        RaiseFatalErrorFmt('Compilation failed (exit %d).', [CompileExit]);

      PrintUnlessQuiet('   Running test Setup');

      StrictDeleteFile(TestSetupLogFilename);
      StrictDeleteFile(TestSetupResultFilename);
      RunAndWait(AddQuotes(TestSetupExeFilename) + ' /LOG=' + AddQuotes(TestSetupLogFilename));

      PrintUnlessQuiet('   Reading test Setup log');

      if not NewFileExists(TestSetupLogFilename) then
        RaiseFatalError('Test Setup log file not found');

      if not Options.Quiet then begin
        const TestSetupLog = TStringList.Create;
        try
          TestSetupLog.LoadFromFile(TestSetupLogFilename);
          for var I := 0 to TestSetupLog.Count - 1 do
            Print('      ' + TestSetupLog[I]);
        finally
          TestSetupLog.Free;
        end;
      end;

      if not NewFileExists(TestSetupResultFilename) then
        RaiseFatalError('Test Setup result file not found');

      PrintUnlessQuiet('   Reading test Setup result');

      const TestResult = ReadFileContents(TestSetupResultFilename);
      if TestResult = '' then
        RaiseFatalError('Test Setup result is empty');

      PrintUnlessQuiet('      ' + TestResult);

      if TestResult <> 'OK' then
        RaiseFatalErrorFmt('Test failed: %s', [TestResult]);
    finally
      Windows.DeleteFile(PChar(TestSetupExeFilename));
      Windows.DeleteFile(PChar(TestSetupLogFilename));
      Windows.DeleteFile(PChar(TestSetupResultFilename));
    end;
  end;

begin
  try
    PrintUnlessQuiet('Running native tests');

    BidiUtilsRunTests;
    ChaCha20RunTests;
    CompilerStringListsRunTests;
    ISPPCTokenizerRunTests;
    ISSigFuncRunTests;
    MD5RunTests;
    ModernColorsRunTests;
    PathFuncRunTests;
    PBKDF2RunTests;
    SetupPathRedirRunTests;
    SHA1RunTests;
    SHA256RunTests;
    SharedCommonFuncRunTests;
    SharedEncryptionFuncRunTests;
    SimpleExpressionRunTests;
    StringScannerRunTests;
    UnsignedFuncRunTests;

    PrintUnlessQuiet('OK');

    PrintUnlessQuiet('Running ROPS script tests');

    ScriptRunTests;

    PrintUnlessQuiet('OK');
  except
    PrintErrOutput('test failed: ' + GetExceptMessage);
    Halt(1);
  end;
end;

procedure Go;
begin
  const ArgList = TStringList.Create;
  try
    for var I := 1 to NewParamCount do
      ArgList.Add(NewParamStr(I));

    const InitialArgListCount = ArgList.Count;
    var J := 0;
    while J < ArgList.Count do begin
      const S = ArgList[J];
      if S.StartsWith('-') then begin
        if (S = '--help') or (S = '-?') then begin
          ShowUsage;
          if InitialArgListCount <> 1 then
            RaiseFatalErrorFmt('"%s" option cannot be combined with other arguments', [S]);
          Exit;
        end else if (S = '--quiet') or (S = '-q') then begin
          Options.Quiet := True;
        end else
          RaiseFatalErrorFmt('Unknown option "%s"', [S]);
        ArgList.Delete(J);
      end else begin
        if S = '' then
          RaiseFatalError('Empty arguments not allowed');
        Inc(J);
      end;
    end;

    if ArgList.Count > 1 then
      RaiseFatalError('Too many arguments');

    if ArgList.Count = 1 then
      CommandTest(ArgList[0])
    else
      CommandTest('');
  finally
    ArgList.Free;
  end;
end;

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  StdOutHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  StdErrHandle := GetStdHandle(STD_ERROR_HANDLE);
  var Mode: DWORD;
  StdOutHandleIsConsole := GetConsoleMode(StdOutHandle, Mode);
  StdErrHandleIsConsole := GetConsoleMode(StdErrHandle, Mode);
  try
    Go;
  except
    PrintErrOutput('istesttool fatal error: ' + GetExceptMessage);
    Halt(2);
  end;
end.
