unit PathFunc.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for PathFunc

  Runs a self-test if DEBUG is defined
}

interface

procedure PathFuncRunTests;

implementation

uses
  Windows, SysUtils, PathFunc;

procedure PathFuncRunTests;

  procedure TestPartLengths(const Filename: String;
    const DrivePartFalse, DrivePartTrue, PathPartFalse, PathPartTrue: Integer);
  begin
    if PathDrivePartLengthEx(Filename, False) <> DrivePartFalse then
      raise Exception.CreateFmt('"%s" drive part(False) test failed', [Filename]);
    if PathDrivePartLengthEx(Filename, True) <> DrivePartTrue then
      raise Exception.CreateFmt('"%s" drive part(True) test failed', [Filename]);
    if PathPathPartLength(Filename, False) <> PathPartFalse then
      raise Exception.CreateFmt('"%s" path part(False) test failed', [Filename]);
    if PathPathPartLength(Filename, True) <> PathPartTrue then
      raise Exception.CreateFmt('"%s" path part(True) test failed', [Filename]);

    if PathIsRooted(Filename) <> (PathDrivePartLengthEx(Filename, True) <> 0) then
      raise Exception.CreateFmt('"%s" PathIsRooted test failed', [Filename]);
  end;

  procedure TestRemoveBackslash(const Filename, ExpectedResult: String);
  begin
    if RemoveBackslash(Filename) <> ExpectedResult then
      raise Exception.Create('RemoveBackslash test failed');
  end;

  procedure TestRemoveBackslashUnlessRoot(const Filename, ExpectedResult: String);
  begin
    if RemoveBackslashUnlessRoot(Filename) <> ExpectedResult then
      raise Exception.Create('RemoveBackslashUnlessRoot test failed');
  end;

  procedure TestPathChangeExt(const Filename, Extension, ExpectedResult: String);
  begin
    if PathChangeExt(Filename, Extension) <> ExpectedResult then
      raise Exception.Create('PathChangeExt test failed');
  end;

  procedure TestPathExtractExt(const Filename, ExpectedResult: String);
  begin
    if PathExtractExt(Filename) <> ExpectedResult then
      raise Exception.Create('PathExtractExt test failed');
  end;

  procedure TestPathCombine(const Dir, Filename, ExpectedResult: String);
  begin
    if PathCombine(Dir, Filename) <> ExpectedResult then
      raise Exception.Create('PathCombine test failed');
  end;

  procedure TestPathStartsWith(const S, AStartsWith: String; const ExpectedResult: Boolean);
  begin
    if PathStartsWith(S, AStartsWith) <> ExpectedResult then
      raise Exception.Create('PathStartsWith test failed');
  end;

  procedure TestPathEndsWith(const IgnoreCase: Boolean;
    const S, AEndsWith: String; const ExpectedResult: Boolean);
  begin
    if PathEndsWith(S, AEndsWith, IgnoreCase) <> ExpectedResult then
      raise Exception.Create('PathEndsWith test failed');
  end;

  procedure TestPathExpand(const S, ExpectedResult: String;
    const ExpectedResultFromTwoParamOverload: Boolean);
  begin
    if PathExpand(S) <> ExpectedResult then
      raise Exception.Create('PathExpand test failed');

    var PathExpandResult: String;
    if PathExpand(S, PathExpandResult) <> ExpectedResultFromTwoParamOverload then
      raise Exception.Create('PathExpand test failed');
    if ExpectedResultFromTwoParamOverload and (PathExpandResult <> ExpectedResult) then
      raise Exception.Create('PathExpand test failed');
  end;

  procedure TestPathExpandAndNormalizeSlashes(const S, ExpectedResult: String);
  begin
    { PathExpand's work is done by Windows' GetFullPathName, while
      PathNormalizeSlashes uses our own code. They should produce the same
      result when the path is fully qualified and has no '.' or '..'
      components. }
    var PathExpandResult: String;
    if (PathExpand(S) <> ExpectedResult) or
       not PathExpand(S, PathExpandResult) or
       (PathExpandResult <> ExpectedResult) then
      raise Exception.Create('PathExpand test failed');

    if PathNormalizeSlashes(S) <> ExpectedResult then
      raise Exception.Create('PathNormalizeSlashes test failed');
  end;

  function CompareResultSign(const Value: Integer): Integer;
  begin
    if Value < 0 then
      Result := -1
    else if Value > 0 then
      Result := 1
    else
      Result := 0;
  end;

  procedure TestPathStrCompare(const S1, S2: String;
    const IgnoreCase: Boolean; const ExpectedSign: Integer;
    const UseNullTerminatedLengths: Boolean = False);
  begin
    var S1Length, S2Length: Integer;
    if UseNullTerminatedLengths then begin
      S1Length := -1;
      S2Length := -1;
    end else begin
      S1Length := Length(S1);
      S2Length := Length(S2);
    end;

    if CompareResultSign(PathStrCompare(PChar(S1), S1Length, PChar(S2), S2Length, IgnoreCase)) <> ExpectedSign then
      raise Exception.Create('PathStrCompare test failed');
  end;

  procedure TestPathStrFind(const Source, Value: String;
    const IgnoreCase: Boolean; const ExpectedIndex: Integer;
    const UseNullTerminatedLengths: Boolean = False);
  begin
    var SourceLength, ValueLength: Integer;
    if UseNullTerminatedLengths then begin
      SourceLength := -1;
      ValueLength := -1;
    end else begin
      SourceLength := Length(Source);
      ValueLength := Length(Value);
    end;

    if PathStrFind(PChar(Source), SourceLength, PChar(Value), ValueLength, IgnoreCase) <> ExpectedIndex then
      raise Exception.Create('PathStrFind test failed');
  end;

begin
  { * = Bogus path case. What the "correct" result should be is debatable. }
  { ** = Possible access to NTFS alternate data stream. The characters before
         and after the colon must be kept together as a single component. }

  TestPartLengths('', 0, 0, 0, 0);
  TestPartLengths('\', 0, 1, 1, 1);
  TestPartLengths('\a', 0, 1, 1, 1);
  TestPartLengths('\a\', 0, 1, 2, 3);
  TestPartLengths('\a\b', 0, 1, 2, 3);
  TestPartLengths('a', 0, 0, 0, 0);
  TestPartLengths('a\', 0, 0, 1, 2);
  TestPartLengths('a\\', 0, 0, 1, 3);
  TestPartLengths('a\\\', 0, 0, 1, 4);
  TestPartLengths('a\b', 0, 0, 1, 2);
  TestPartLengths('a\b:c', 0, 0, 1, 2); {**}

  { Drive "letters" can technically be any character other than '\'. See
    comment in PathDrivePartLengthEx. }
  TestPartLengths('1:', 2, 2, 2, 2);
  TestPartLengths('@:', 2, 2, 2, 2);
  TestPartLengths('\:', 0, 1, 1, 1); {*}
  { Yes, the following is a valid path -- it specifies a stream named 'stream'
    on the root directory of the current drive. (Yes, directories can have
    named streams.) }
  TestPartLengths('\:stream', 0, 1, 1, 1); {**}

  TestPartLengths('c:', 2, 2, 2, 2);
  TestPartLengths('c:a', 2, 2, 2, 2);
  TestPartLengths('c:\', 2, 3, 3, 3);
  TestPartLengths('c:\\', 2, 3, 3, 4);
  TestPartLengths('c:\\\', 2, 3, 3, 5);
  TestPartLengths('c:\a', 2, 3, 3, 3);
  TestPartLengths('c:\a\', 2, 3, 4, 5);
  TestPartLengths('c:\a\\', 2, 3, 4, 6);
  TestPartLengths('c:\a\\\', 2, 3, 4, 7);
  TestPartLengths('c:\a\b', 2, 3, 4, 5);
  TestPartLengths('c:\a\b:c', 2, 3, 4, 5); {**}

  TestPartLengths('\\', 2, 2, 2, 2); {*}
  { Odd cases follow: The extra slashes are considered to be in the drive part
    since PathDrivePartLength keeps slurping slashes looking for a share name
    that doesn't exist. }
  TestPartLengths('\\\', 3, 3, 3, 3); {*}
  TestPartLengths('\\\\', 4, 4, 4, 4); {*}
  TestPartLengths('\\\\\', 5, 5, 5, 5); {*}
  TestPartLengths('\\a', 3, 3, 3, 3); {*}
  TestPartLengths('\\a\', 4, 4, 4, 4); {*}
  TestPartLengths('\\a\b', 5, 5, 5, 5);
  TestPartLengths('\\a\b\', 5, 5, 5, 6);
  TestPartLengths('\\a\b\c', 5, 5, 5, 6);
  TestPartLengths('\\a\b\c\', 5, 5, 7, 8);
  TestPartLengths('\\a\b\c\d', 5, 5, 7, 8);
  TestPartLengths('\\a\b\c\d:e', 5, 5, 7, 8); {**}
  TestPartLengths('\\a\\\b', 7, 7, 7, 7);
  TestPartLengths('\\a\\\b\\\', 7, 7, 7, 10);
  TestPartLengths('\\a\\\b\\\c', 7, 7, 7, 10);
  TestPartLengths('\\a\\\b\\\c\\\', 7, 7, 11, 14);

  TestPartLengths('\\?\C:', 6, 6, 6, 6);
  TestPartLengths('\\?\C:\', 6, 7, 7, 7);
  TestPartLengths('\\?\C:\a', 6, 7, 7, 7);
  TestPartLengths('\\?\C:\a\b', 6, 7, 8, 9);
  TestPartLengths('\\.\C:', 6, 6, 6, 6);
  TestPartLengths('\\.\C:\a', 6, 7, 7, 7);
  TestPartLengths('\\?\UNC\server\share', 20, 20, 20, 20);
  TestPartLengths('\\?\UNC\server\share\', 20, 20, 20, 21);
  TestPartLengths('\\?\UNC\server\share\dir', 20, 20, 20, 21);
  TestPartLengths('\\.\UNC\server\share', 20, 20, 20, 20);

  TestRemoveBackslash('', '');
  TestRemoveBackslash('\', '');
  TestRemoveBackslash('\\', '');
  TestRemoveBackslash('\\\', '');
  TestRemoveBackslash('c:', 'c:');
  TestRemoveBackslash('c:\', 'c:');
  TestRemoveBackslash('c:\\', 'c:');
  TestRemoveBackslash('c:\\\', 'c:');

  TestRemoveBackslashUnlessRoot('', '');
  TestRemoveBackslashUnlessRoot('\', '\');
  TestRemoveBackslashUnlessRoot('\\', '\\'); {*}
  TestRemoveBackslashUnlessRoot('\\\', '\\\'); {*}
  TestRemoveBackslashUnlessRoot('\\\\', '\\\\'); {*}
  TestRemoveBackslashUnlessRoot('a', 'a');
  TestRemoveBackslashUnlessRoot('a\', 'a');
  TestRemoveBackslashUnlessRoot('a\\', 'a');
  TestRemoveBackslashUnlessRoot('c:', 'c:');
  TestRemoveBackslashUnlessRoot('c:\', 'c:\');
  TestRemoveBackslashUnlessRoot('c:\a', 'c:\a');
  TestRemoveBackslashUnlessRoot('c:\a\', 'c:\a');
  TestRemoveBackslashUnlessRoot('c:\a\\', 'c:\a');
  TestRemoveBackslashUnlessRoot('\\a\b', '\\a\b');
  TestRemoveBackslashUnlessRoot('\\a\b\', '\\a\b');
  TestRemoveBackslashUnlessRoot('\\a\b\\', '\\a\b');

  TestPathChangeExt('c:', '.txt', 'c:.txt'); {*}  { weird, but same as Delphi's ChangeFileExt }
  TestPathChangeExt('c:\', '.txt', 'c:\.txt'); {*}
  TestPathChangeExt('c:\a', '.txt', 'c:\a.txt');
  TestPathChangeExt('c:\a.', '.txt', 'c:\a.txt');
  TestPathChangeExt('c:\a.tar', '.txt', 'c:\a.txt');
  TestPathChangeExt('c:\a.tar.gz', '.txt', 'c:\a.tar.txt');
  TestPathChangeExt('c:\x.y\a', '.txt', 'c:\x.y\a.txt');
  TestPathChangeExt('\\x.y\a', '.txt', '\\x.y\a.txt'); {*}  { ditto above }
  TestPathChangeExt('\\x.y\a\', '.txt', '\\x.y\a\.txt'); {*}

  TestPathExtractExt('c:', '');
  TestPathExtractExt('c:\', '');
  TestPathExtractExt('c:\a', '');
  TestPathExtractExt('c:\a.', '.');
  TestPathExtractExt('c:\a.txt', '.txt');
  TestPathExtractExt('c:\a.txt.gz', '.gz');
  TestPathExtractExt('c:\x.y\a', '');
  TestPathExtractExt('\\x.y\a', '');
  TestPathExtractExt('\\x.y\a.b', '');
  TestPathExtractExt('\\x.y\a.b\c', '');
  TestPathExtractExt('\\x.y\a.b\c.txt', '.txt');

  TestPathCombine('', 'x', 'x');
  TestPathCombine('a', 'x', 'a\x');
  TestPathCombine('a\', 'x', 'a\x');
  TestPathCombine('a\\', 'x', 'a\\x');
  TestPathCombine('c:', 'x', 'c:x');
  TestPathCombine('c:\', 'x', 'c:\x');
  TestPathCombine('c:\\', 'x', 'c:\\x');
  TestPathCombine('c:\a', 'x', 'c:\a\x');
  TestPathCombine('\', 'x', '\x');
  TestPathCombine('c:\', '', '');
  TestPathCombine('c:\', 'e:x', 'e:x');
  TestPathCombine('c:\', 'e:\x', 'e:\x');
  TestPathCombine('c:\', '\x', '\x');
  TestPathCombine('c:\', '\\a\b\c', '\\a\b\c');
  TestPathCombine('c:\', 'ee:x', 'c:\ee:x'); {**}

  TestPathStartsWith('', '', True);
  TestPathStartsWith('TestingAbc', '', True);
  TestPathStartsWith('C:', 'c:\', False);
  TestPathStartsWith('C:\', 'c:\', True);
  TestPathStartsWith('C:\test', 'c:\', True);

  TestPathEndsWith(False, '', '', True);
  TestPathEndsWith(True, '', '', True);
  TestPathEndsWith(True, 'TestingAbc', '', True);
  TestPathEndsWith(True, 'TestingAbc', 'gabc', True);
  TestPathEndsWith(False, 'TestingAbc', 'gabc', False);
  TestPathEndsWith(True, 'TestingAbc', 'zabc', False);
  TestPathEndsWith(True, 'TestingAbc', 'testingABC', True);
  TestPathEndsWith(True, 'TestingAbc', 'xTestingAbc', False);

  TestPathExpand('', '', False);
  TestPathExpand(' ', '', False);
  TestPathExpand('   ', '', False);
  { This odd behavior comes from GetFullPathName. You'd think they would fail
    like in the above cases. Only testing to see if the behavior changes. }
  TestPathExpand('...', AddBackslash(GetCurrentDir), True);
  TestPathExpand('.. ', AddBackslash(GetCurrentDir), True);

  TestPathExpandAndNormalizeSlashes('C:\abc\def', 'C:\abc\def');
  TestPathExpandAndNormalizeSlashes('C:\abc\def\', 'C:\abc\def\');
  TestPathExpandAndNormalizeSlashes('C:\abc\def\\', 'C:\abc\def\');
  TestPathExpandAndNormalizeSlashes('C:/abc\def', 'C:\abc\def');
  TestPathExpandAndNormalizeSlashes('C:\\\abc////def', 'C:\abc\def');
  { Windows' GetFullPathName doesn't collapse 3+ leading slashes down to 2;
    instead, it collapses 4+ leading slashes down to 3. (The resulting path
    doesn't actually work with that extra 3rd slash.) }
  TestPathExpandAndNormalizeSlashes('\\?\C:\Windows', '\\?\C:\Windows');
  TestPathExpandAndNormalizeSlashes('\\\?\C:\Windows', '\\\?\C:\Windows');
  TestPathExpandAndNormalizeSlashes('\\?\\C:\\Windows', '\\?\C:\Windows');
  {$IFDEF ISTESTTOOLPROJ}
  { These fail on Wine which is why they are excluded from self-test }
  TestPathExpandAndNormalizeSlashes('\\\\?\C:\Windows', '\\\?\C:\Windows');
  TestPathExpandAndNormalizeSlashes('\\\\\?\C:\Windows', '\\\?\C:\Windows');
  TestPathExpandAndNormalizeSlashes('\\\?\\C:\\Windows', '\\\?\C:\Windows');
  {$ENDIF}

  TestPathStrCompare('Test', 'test', True, 0);
  TestPathStrCompare('Test', 'test', False, -1);
  TestPathStrCompare('Test', 'Te', False, 1);
  TestPathStrCompare('Test', 'Tex', False, -1);
  TestPathStrCompare('Hello'+#0+'World', 'Hello', False, 0, True);

  TestPathStrFind('abcABC', 'ABC', True, 0);
  TestPathStrFind('abcABC', 'ABC', False, 3);
  TestPathStrFind('abcABC', 'AbC', False, -1);
  TestPathStrFind('abcABC', 'AbC', True, 0);
  TestPathStrFind('abcABC', 'xyz', True, -1);
  TestPathStrFind('abc'+#0+'ABC', 'ABC', False, -1, True);
  TestPathStrFind('abc'+#0+'ABC', 'ABC', False, 4);
end;

{$IFDEF DEBUG}
initialization
  try
    PathFuncRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}

end.
