unit Setup.PathRedir.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for Setup.PathRedir

  Runs a self-test if DEBUG is defined
}

interface

procedure SetupPathRedirRunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, {$ENDIF} System.SysUtils, Shared.SetupTypes, Setup.PathRedir;

{$C+}

procedure SetupPathRedirRunTests;
const
  Sys32 = 'C:\FakeWin\System32';
  Sys32OtherCase = 'C:\FakeWin\system32';
  Wow64 = 'C:\FakeWin\SysWOW64';
  Native = 'C:\FakeWin\Sysnative';
  SuperSys32 = '\\?\' + Sys32;
  SuperWow64 = '\\?\' + Wow64;
  SuperNative = '\\?\' + Native;

  procedure TestRedir(const AWindows64Bit, A64Bit: Boolean;
    const APath: String; const ATargetProcess: TPathRedirTargetProcess;
    const AFlags: TPathRedirFlags; const AExpected: String);
  begin
    Assert(TestApplyPathRedirRules(AWindows64Bit, A64Bit,
      Sys32, Wow64, Native, APath, ATargetProcess, AFlags) = AExpected);
  end;

  {$IFDEF ISTESTTOOLPROJ}
  procedure TestRedirRaises(const AWindows64Bit, A64Bit: Boolean;
    const APath: String; const ATargetProcess: TPathRedirTargetProcess);
  begin
    var Caught := False;
    try
      TestApplyPathRedirRules(AWindows64Bit, A64Bit,
        Sys32, Wow64, Native, APath, ATargetProcess, []);
    except
      on Exception do Caught := True;
    end;
    Assert(Caught);
  end;
  {$ENDIF}

begin
  { === Running on 64-bit Windows === }

  { Pass-through: a path under no system directory just gets the super prefix }
  TestRedir(True, False, 'C:\Other\Path', tpNativeBit, [], '\\?\C:\Other\Path');

  { rfNormalPath strips the \\?\ added by super conversion when no rewrite happened }
  TestRedir(True, False, 'C:\Other\Path', tpNativeBit, [rfNormalPath], 'C:\Other\Path');

  { A64Bit=False (32-bit path): System32 -> SysWOW64 for any target other than
    tp32BitPreferSystem32. Both a 64-bit and a 32-bit target hit this rewrite. }
  TestRedir(True, False, Sys32 + '\foo.exe', tpNativeBit, [], SuperWow64 + '\foo.exe');
  TestRedir(True, False, Sys32 + '\foo.exe', tp32Bit, [], SuperWow64 + '\foo.exe');
  TestRedir(True, False, Sys32 + '\foo.exe', tp32BitPreferSystem32, [], SuperSys32 + '\foo.exe');

  { Case-insensitive prefix match: input is matched against System32 regardless
    of case, and the canonical case from the stored prefix is used in the result }
  TestRedir(True, False, Sys32OtherCase + '\foo.exe', tpNativeBit, [],
    SuperWow64 + '\foo.exe');

  { A64Bit=True (64-bit path): System32 -> Sysnative when the target is 32-bit.
    When the target is 64-bit, System32 is left alone. }
  TestRedir(True, True, Sys32 + '\foo.exe', tpNativeBit, [], SuperSys32 + '\foo.exe');
  TestRedir(True, True, Sys32 + '\foo.exe', tp32Bit, [], SuperNative + '\foo.exe');
  TestRedir(True, True, Sys32 + '\foo.exe', tp32BitPreferSystem32, [], SuperNative + '\foo.exe');

  { SysWOW64 -> System32 fires for tp32BitPreferSystem32, regardless of A64Bit.
    For all other targets, SysWOW64 is left alone. }
  TestRedir(True, True, Wow64 + '\foo.exe', tpNativeBit, [], SuperWow64 + '\foo.exe');
  TestRedir(True, False, Wow64 + '\foo.exe', tpNativeBit, [], SuperWow64 + '\foo.exe');
  TestRedir(True, True, Wow64 + '\foo.exe', tp32Bit, [], SuperWow64 + '\foo.exe');
  TestRedir(True, False, Wow64 + '\foo.exe', tp32Bit, [], SuperWow64 + '\foo.exe');
  TestRedir(True, True, Wow64 + '\foo.exe', tp32BitPreferSystem32, [], SuperSys32 + '\foo.exe');
  TestRedir(True, False, Wow64 + '\foo.exe', tp32BitPreferSystem32, [], SuperSys32 + '\foo.exe');

  { Sysnative -> System32 fires for any 64-bit target, regardless of A64Bit.
    When the target is 32-bit, Sysnative is left alone. }
  TestRedir(True, True, Native + '\foo.exe', tpNativeBit, [], SuperSys32 + '\foo.exe');
  TestRedir(True, False, Native + '\foo.exe', tpNativeBit, [], SuperSys32 + '\foo.exe');
  TestRedir(True, True, Native + '\foo.exe', tp32Bit, [], SuperNative + '\foo.exe');
  TestRedir(True, False, Native + '\foo.exe', tp32Bit, [], SuperNative + '\foo.exe');
  TestRedir(True, True, Native + '\foo.exe', tp32BitPreferSystem32, [], SuperNative + '\foo.exe');
  TestRedir(True, False, Native + '\foo.exe', tp32BitPreferSystem32, [], SuperNative + '\foo.exe');

  { Boundary: a path equal to the system directory itself (no trailing chars)
    is rewritten - the SubstitutePath check accepts PathLen = FromDirLen }
  TestRedir(True, False, Sys32, tpNativeBit, [], SuperWow64);

  { Boundary: a path that has the system directory as a prefix but no separator
    after must NOT be rewritten. 'System32extra' shares 8 chars with 'System32'
    but the next char is 'e', not '\' }
  TestRedir(True, False, Sys32 + 'extra\foo', tpNativeBit, [], SuperSys32 + 'extra\foo');

  { tpCurrent reflects the build target: a 64-bit build treats it as a 64-bit
    target, a 32-bit build treats it as a 32-bit target. Tested with A64Bit=True
    on System32 because the result differs between the two cases. }
  {$IFDEF WIN64}
  TestRedir(True, True, Sys32 + '\foo.exe', tpCurrent, [], SuperSys32 + '\foo.exe');
  {$ELSE}
  TestRedir(True, True, Sys32 + '\foo.exe', tpCurrent, [], SuperNative + '\foo.exe');
  {$ENDIF}

  { rfNormalPath also strips \\?\ from a substituted result }
  TestRedir(True, False, Sys32 + '\foo.exe', tpNativeBit, [rfNormalPath], Wow64 + '\foo.exe');

  { Already-super input is accepted without double-prefixing, and the
    substitution still applies (the unit comment documents this case) }
  TestRedir(True, False, SuperSys32 + '\foo.exe', tpNativeBit, [rfNormalPath],
    Wow64 + '\foo.exe');

  {$IFDEF ISTESTTOOLPROJ}
  { Empty input is rejected }
  TestRedirRaises(True, False, '', tpNativeBit);

  { The "\??\" prefix is rejected (PathExpand does not understand it and would
    prepend the current drive) }
  TestRedirRaises(True, False, '\??\C:\foo', tpNativeBit);
  {$ENDIF}

  { === Running on 32-bit Windows === }

  { No substitutions are made; the path is just super-converted (the System32
    rewrite that would have applied on 64-bit Windows must NOT happen) }
  TestRedir(False, False, Sys32 + '\foo.exe', tpNativeBit, [], SuperSys32 + '\foo.exe');

  { rfNormalPath round-trip: super added then stripped, leaving the input }
  TestRedir(False, False, Sys32 + '\foo.exe', tpNativeBit, [rfNormalPath], Sys32 + '\foo.exe');

  {$IFDEF ISTESTTOOLPROJ}
  { A64Bit=True is rejected when running 32-bit Windows }
  TestRedirRaises(False, True, 'C:\foo', tpNativeBit);
  {$ENDIF}
end;

{$IFDEF DEBUG}
{$IFNDEF ISTESTTOOLPROJ}
initialization
  try
    SetupPathRedirRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}
{$ENDIF}

end.
