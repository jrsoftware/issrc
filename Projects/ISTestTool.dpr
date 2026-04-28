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
  Shared.CommonFunc in 'Src\Shared.CommonFunc.pas',
  Shared.FileClass in 'Src\Shared.FileClass.pas',
  Shared.SetupTypes in 'Src\Shared.SetupTypes.pas',
  Shared.Struct in 'Src\Shared.Struct.pas',
  ECDSA in '..\Components\ECDSA.pas',
  BidiUtils in '..\Components\BidiUtils.pas',
  BidiUtils.Test in '..\Components\BidiUtils.Test.pas',
  ChaCha20 in '..\Components\ChaCha20.pas',
  ChaCha20.Test in '..\Components\ChaCha20.Test.pas',
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
// {$R Res\ISTestTool.versionandicon.res}

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
  PrintErrOutput('Usage:  istesttool [options]');
  PrintErrOutput('Options:');
  PrintErrOutput('  --quiet, -q Suppresses status messages that are normally printed to standard output');
  PrintErrOutput('  --help, -?  Prints this information');
  PrintErrOutput('');
end;

procedure CommandTest;
begin
  try
    BidiUtilsRunTests;
    ChaCha20RunTests;
    ISSigFuncRunTests;
    MD5RunTests;
    ModernColorsRunTests;
    PathFuncRunTests;
    PBKDF2RunTests;
    SetupPathRedirRunTests;
    SHA1RunTests;
    SHA256RunTests;
    SharedEncryptionFuncRunTests;
    SimpleExpressionRunTests;
    StringScannerRunTests;
    UnsignedFuncRunTests;
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

    if ArgList.Count <> 0 then
      RaiseFatalError('Too many arguments');

    CommandTest;
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
