unit PathFuncTest;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for PathFunc
}

interface

procedure PathFuncRunTests(const AlsoTestJapaneseDBCS: Boolean);

implementation

uses
  Windows, SysUtils, PathFunc;

procedure PathFuncRunTests(const AlsoTestJapaneseDBCS: Boolean);

  procedure Test(const Filename: String;
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

const
  DBChar = #131'\'; { a double-byte character whose 2nd byte happens to be a backslash }
begin
  {$IFDEF UNICODE}
  if AlsoTestJapaneseDBCS then
    raise Exception.Create('DBCS tests not supported in Unicode build');
  {$ENDIF}
  if AlsoTestJapaneseDBCS and (GetACP <> 932) then
    raise Exception.Create('Must be running in Japanese code page to run these tests');

  { * = Bogus path case. What the "correct" result should be is debatable. }
  { ** = Possible access to NTFS alternate data stream. The characters before
         and after the colon must be kept together as a single component. }

  Test('', 0, 0, 0, 0);
  Test('\', 0, 1, 1, 1);
  Test('\a', 0, 1, 1, 1);
  Test('\a\', 0, 1, 2, 3);
  Test('\a\b', 0, 1, 2, 3);
  Test('a', 0, 0, 0, 0);
  Test('a\', 0, 0, 1, 2);
  Test('a\\', 0, 0, 1, 3);
  Test('a\\\', 0, 0, 1, 4);
  Test('a\b', 0, 0, 1, 2);
  Test('a\b:c', 0, 0, 1, 2); {**}

  Test('1:', 2, 2, 2, 2); {*}

  Test('\:', 0, 1, 1, 1); {*}
  { Yes, the following is a valid path -- it specifies a stream named 'stream'
    on the root directory of the current drive. (Yes, directories can have
    named streams.) }
  Test('\:stream', 0, 1, 1, 1); {**}

  Test('c:', 2, 2, 2, 2);
  Test('c:a', 2, 2, 2, 2);
  Test('c:\', 2, 3, 3, 3);
  Test('c:\\', 2, 3, 3, 4);
  Test('c:\\\', 2, 3, 3, 5);
  Test('c:\a', 2, 3, 3, 3);
  Test('c:\a\', 2, 3, 4, 5);
  Test('c:\a\\', 2, 3, 4, 6);
  Test('c:\a\\\', 2, 3, 4, 7);
  Test('c:\a\b', 2, 3, 4, 5);
  Test('c:\a\b:c', 2, 3, 4, 5); {**}

  Test('\\', 2, 2, 2, 2); {*}
  { Odd cases follow: The extra slashes are considered to be in the drive part
    since PathDrivePartLength keeps slurping slashes looking for a share name
    that doesn't exist. }
  Test('\\\', 3, 3, 3, 3); {*}
  Test('\\\\', 4, 4, 4, 4); {*}
  Test('\\\\\', 5, 5, 5, 5); {*}
  Test('\\a', 3, 3, 3, 3); {*}
  Test('\\a\', 4, 4, 4, 4); {*}
  Test('\\a\b', 5, 5, 5, 5);
  Test('\\a\b\', 5, 5, 5, 6);
  Test('\\a\b\c', 5, 5, 5, 6);
  Test('\\a\b\c\', 5, 5, 7, 8);
  Test('\\a\b\c\d', 5, 5, 7, 8);
  Test('\\a\b\c\d:e', 5, 5, 7, 8); {**}
  Test('\\a\\\b', 7, 7, 7, 7);
  Test('\\a\\\b\\\', 7, 7, 7, 10);
  Test('\\a\\\b\\\c', 7, 7, 7, 10);
  Test('\\a\\\b\\\c\\\', 7, 7, 11, 14);

  if AlsoTestJapaneseDBCS then begin
    Test('\\'+DBChar+DBChar+'\b', 8, 8, 8, 8);
    Test('\\'+DBChar+DBChar+'\b\c', 8, 8, 8, 9);
    Test('\\'+DBChar+DBChar+'\b\c\', 8, 8, 10, 11);
    Test('c:\'+DBChar+'\b', 2, 3, 5, 6);
    Test(DBChar+':', 3, 3, 3, 3); {*}  { double-byte drive letter? bogus, but be like Windows... }
  end;

  TestRemoveBackslash('', '');
  TestRemoveBackslash('\', '');
  TestRemoveBackslash('\\', '');
  TestRemoveBackslash('\\\', '');
  TestRemoveBackslash('c:', 'c:');
  TestRemoveBackslash('c:\', 'c:');
  TestRemoveBackslash('c:\\', 'c:');
  TestRemoveBackslash('c:\\\', 'c:');

  if AlsoTestJapaneseDBCS then begin
    TestRemoveBackslash(DBChar, DBChar);
    TestRemoveBackslash(DBChar+'\', DBChar);
    TestRemoveBackslash(DBChar+'\\', DBChar);
  end;

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
  if AlsoTestJapaneseDBCS then begin
    TestPathCombine(DBChar+':', 'x', DBChar+':x'); {*}  { double-byte drive letter? bogus, but be like Windows... }
    TestPathCombine('c:\'+DBChar, 'x', 'c:\'+DBChar+'\x');
  end;
  TestPathCombine('c:\', '', '');
  TestPathCombine('c:\', 'e:x', 'e:x');
  TestPathCombine('c:\', 'e:\x', 'e:\x');
  TestPathCombine('c:\', '\x', '\x');
  TestPathCombine('c:\', '\\a\b\c', '\\a\b\c');
  TestPathCombine('c:\', 'ee:x', 'c:\ee:x'); {**}

  TestPathStartsWith('C:', 'c:\', False);
  TestPathStartsWith('C:\', 'c:\', True);
  TestPathStartsWith('C:\test', 'c:\', True);
  if AlsoTestJapaneseDBCS then begin
    { Test PathStartsWith's PathCharIsTrailByte call; it shouldn't chop a
      double-byte character in half }
    TestPathStartsWith('C:'+DBChar, 'c:\', False);
    TestPathStartsWith('C:'+DBChar, 'c:'+DBChar[1], False);
  end;
end;

end.
