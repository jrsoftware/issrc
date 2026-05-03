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

  procedure TestAddBackslash(const S, ExpectedResult: String);
  begin
    if AddBackslash(S) <> ExpectedResult then
      raise Exception.Create('AddBackslash test failed');
  end;

  procedure TestPathSame(const S1, S2: String; const ExpectedResult: Boolean);
  begin
    if PathSame(S1, S2) <> ExpectedResult then
      raise Exception.Create('PathSame test failed');
  end;

  procedure TestPathCompare(const S1, S2: String; const IgnoreCase: Boolean;
    const ExpectedSign: Integer);
  begin
    if CompareResultSign(PathCompare(S1, S2, IgnoreCase)) <> ExpectedSign then
      raise Exception.Create('PathCompare test failed');
  end;

  procedure TestPathCompareDefault(const S1, S2: String; const ExpectedSign: Integer);
  begin
    if CompareResultSign(PathCompare(S1, S2)) <> ExpectedSign then
      raise Exception.Create('PathCompare test failed');
  end;

  procedure TestPathExtracts(const Filename, ExpectedName, ExpectedPath, ExpectedDir, ExpectedDrive: String);
  begin
    if PathExtractName(Filename) <> ExpectedName then
      raise Exception.Create('PathExtractName test failed');
    if PathExtractPath(Filename) <> ExpectedPath then
      raise Exception.Create('PathExtractPath test failed');
    if PathExtractDir(Filename) <> ExpectedDir then
      raise Exception.Create('PathExtractDir test failed');
    if PathExtractDrive(Filename) <> ExpectedDrive then
      raise Exception.Create('PathExtractDrive test failed');
  end;

  procedure TestPathExtensionPos(const Filename: String; const ExpectedResult: Integer);
  begin
    if PathExtensionPos(Filename) <> ExpectedResult then
      raise Exception.Create('PathExtensionPos test failed');
  end;

  procedure TestPathHasSubstringAt(const S, Substring: String; const Offset: Integer;
    const ExpectedResult: Boolean);
  begin
    if PathHasSubstringAt(S, Substring, Offset) <> ExpectedResult then
      raise Exception.Create('PathHasSubstringAt test failed');
  end;

  procedure TestPathHasInvalidCharacters(const S: String;
    const AllowDriveLetterColon, ExpectedResult: Boolean);
  begin
    if PathHasInvalidCharacters(S, AllowDriveLetterColon) <> ExpectedResult then
      raise Exception.Create('PathHasInvalidCharacters test failed');
  end;

  procedure TestPathComponentIsReservedName(const SingleComponent: String;
    const ExpectedResult: Boolean);
  begin
    if PathComponentIsReservedName(SingleComponent) <> ExpectedResult then
      raise Exception.Create('PathComponentIsReservedName test failed');
  end;

  procedure TestValidateAndCombinePath(const ADestDir, AFilename, ExpectedResultingPath: String;
    const ExpectedResult: Boolean);
  begin
    var ResultingPath: String;
    if ValidateAndCombinePath(ADestDir, AFilename, ResultingPath) <> ExpectedResult then
      raise Exception.Create('ValidateAndCombinePath test failed');
    if ExpectedResult then begin
      if ResultingPath <> ExpectedResultingPath then
        raise Exception.Create('ValidateAndCombinePath test failed');
    end else begin
      { Failure should not expose a partial path. Since AResultingPath is an out
        string, this cannot distinguish no assignment from assigning ''. }
      if ResultingPath <> '' then
        raise Exception.Create('ValidateAndCombinePath test failed');
    end;
    if ValidateAndCombinePath(ADestDir, AFilename) <> ExpectedResult then
      raise Exception.Create('ValidateAndCombinePath test failed');
  end;

  procedure TestPathConvertNormalToSuper(const Filename, ExpectedSuper: String;
    const ExpectedResult: Boolean);
  begin
    var SuperFilename: String;
    if PathConvertNormalToSuper(Filename, SuperFilename) <> ExpectedResult then
      raise Exception.Create('PathConvertNormalToSuper test failed');
    if ExpectedResult and (SuperFilename <> ExpectedSuper) then
      raise Exception.Create('PathConvertNormalToSuper test failed');
  end;

  procedure TestPathConvertSuperToNormal(const Filename, ExpectedResult: String);
  begin
    if PathConvertSuperToNormal(Filename) <> ExpectedResult then
      raise Exception.Create('PathConvertSuperToNormal test failed');
  end;

  procedure TestPathLastDelimiter(const Delimiters, S: String; const ExpectedResult: Integer);
  begin
    if PathLastDelimiter(Delimiters, S) <> ExpectedResult then
      raise Exception.Create('PathLastDelimiter test failed');
  end;

  procedure TestPathLowercase(const S, ExpectedResult: String);
  begin
    if PathLowercase(S) <> ExpectedResult then
      raise Exception.Create('PathLowercase test failed');
  end;

  procedure TestPathPos(const C: Char; const S: String;
    const Offset, ExpectedResult: Integer);
  begin
    if PathPos(C, S, Offset) <> ExpectedResult then
      raise Exception.Create('PathPos test failed');
  end;

  procedure TestPathLastChar(const S: String; const ExpectedChar: Char);
  begin
    const Ptr = PathLastChar(S);
    if (Ptr = nil) or (Ptr^ <> ExpectedChar) then
      raise Exception.Create('PathLastChar test failed');
  end;

  procedure TestPathLastCharNil(const S: String);
  begin
    if PathLastChar(S) <> nil then
      raise Exception.Create('PathLastChar test failed');
  end;

  procedure TestPathCharIsSlash(const C: Char; const ExpectedResult: Boolean);
  begin
    if PathCharIsSlash(C) <> ExpectedResult then
      raise Exception.Create('PathCharIsSlash test failed');
  end;

  procedure TestPathCharIsDriveLetter(const C: Char; const ExpectedResult: Boolean);
  begin
    if PathCharIsDriveLetter(C) <> ExpectedResult then
      raise Exception.Create('PathCharIsDriveLetter test failed');
  end;

  procedure TestPathNormalizeSlashes(const S, ExpectedResult: String);
  begin
    if PathNormalizeSlashes(S) <> ExpectedResult then
      raise Exception.Create('PathNormalizeSlashes test failed');
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
  TestPathCombine('c:a', 'x', 'c:a\x');
  TestPathCombine('c:\', 'x', 'c:\x');
  TestPathCombine('c:\\', 'x', 'c:\\x');
  TestPathCombine('c:\a', 'x', 'c:\a\x');
  TestPathCombine('\', 'x', '\x');
  TestPathCombine('\:', 'x', '\:\x'); {*}
  TestPathCombine('\:a', 'x', '\:a\x'); {*}
  TestPathCombine('\:\a', 'x', '\:\a\x'); {*}
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

  { Edge cases not covered through PathStartsWith/PathEndsWith }
  TestPathHasSubstringAt('abc', 'a', -1, False);
  TestPathHasSubstringAt('abc', '', 3, True);
  TestPathHasSubstringAt('abc', 'a', 99, False);

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

  TestPathNormalizeSlashes('a/\b', 'a\b');
  TestPathNormalizeSlashes('a\\b', 'a\b');
  TestPathNormalizeSlashes('\\a\\\b', '\\a\b');
  TestPathNormalizeSlashes('a/b', 'a\b');
  TestPathNormalizeSlashes('a//b', 'a\b');
  TestPathNormalizeSlashes('///', '\\\'); { 3+ leading slash quirk preserved }
  TestPathNormalizeSlashes('', '');

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

  TestAddBackslash('', '');
  TestAddBackslash('a', 'a\');
  TestAddBackslash('a\', 'a\');
  TestAddBackslash('a/', 'a/');

  TestPathSame('abc', 'abc', True);
  TestPathSame('abc', 'ABC', True);
  TestPathSame('abc', 'abcd', False);
  TestPathSame('', '', True);
  TestPathSame('', 'a', False);
  { Super path and its normal version are not equivalent }
  TestPathSame('C:\x', '\\?\C:\x', False);

  TestPathCompare('abc', 'ABC', True, 0);
  TestPathCompareDefault('abc', 'ABC', 0);
  TestPathCompare('abc', 'ABC', False, 1);
  TestPathCompare('abc', 'abd', True, -1);
  TestPathCompare('abd', 'abc', True, 1);
  TestPathCompare('abc'+#0+'def', 'abc'+#0+'def', True, 0);
  TestPathCompare('abc'+#0+'def', 'abc', True, 1);

  TestPathExtracts('c:\dir\file.txt', 'file.txt', 'c:\dir\', 'c:\dir', 'c:');
  TestPathExtracts('c:\dir\', '', 'c:\dir\', 'c:\dir', 'c:');
  TestPathExtracts('c:\', '', 'c:\', 'c:\', 'c:');
  TestPathExtracts('C:file', 'file', 'C:', 'C:', 'C:');
  TestPathExtracts('\\s\sh\file', 'file', '\\s\sh\', '\\s\sh', '\\s\sh');
  TestPathExtracts('\\s\sh', '', '\\s\sh', '\\s\sh', '\\s\sh');
  TestPathExtracts('\\?\C:\x', 'x', '\\?\C:\', '\\?\C:\', '\\?\C:');
  TestPathExtracts('\\.\C:\x', 'x', '\\.\C:\', '\\.\C:\', '\\.\C:');
  TestPathExtracts('file.txt', 'file.txt', '', '', '');
  TestPathExtracts('\file', 'file', '\', '\', '');
  TestPathExtracts('', '', '', '', '');
  TestPathExtracts('a\b:c', 'b:c', 'a\', 'a', ''); {**}

  TestPathExtensionPos('', 0);
  TestPathExtensionPos('file', 0);
  TestPathExtensionPos('file.txt', 5);
  TestPathExtensionPos('a.b.c', 4); { Last dot wins }
  TestPathExtensionPos('c:\dir.x\a', 0); { Dot in path part is ignored }
  TestPathExtensionPos('c:\dir.x\a.txt', 11);
  TestPathExtensionPos('.hidden', 1); { Leading dot is the extension }
  TestPathExtensionPos('a.', 2);

  TestPathHasInvalidCharacters('', False, False);
  TestPathHasInvalidCharacters('', True, False);
  TestPathHasInvalidCharacters('file.txt', False, False);
  TestPathHasInvalidCharacters('C:\dir\file', True, False);
  TestPathHasInvalidCharacters('\\server\share\a', False, False);
  TestPathHasInvalidCharacters('C:', True, False);
  TestPathHasInvalidCharacters('C:\dir', True, False);

  TestPathHasInvalidCharacters('C:\dir\file', False, True);
  TestPathHasInvalidCharacters('a'+#1+'b', False, True);
  TestPathHasInvalidCharacters('a/b', False, True);
  TestPathHasInvalidCharacters('a*b', False, True);
  TestPathHasInvalidCharacters('a?b', False, True);
  TestPathHasInvalidCharacters('a"b', False, True);
  TestPathHasInvalidCharacters('a<b', False, True);
  TestPathHasInvalidCharacters('a>b', False, True);
  TestPathHasInvalidCharacters('a|b', False, True);
  TestPathHasInvalidCharacters('\\?\C:\x', False, True);
  TestPathHasInvalidCharacters('\??\C:\x', False, True);
  TestPathHasInvalidCharacters('file.', False, True);
  TestPathHasInvalidCharacters('file ', False, True);
  TestPathHasInvalidCharacters('file. . .', False, True);
  TestPathHasInvalidCharacters('file . . ', False, True);
  TestPathHasInvalidCharacters('dir.\x', False, True);
  TestPathHasInvalidCharacters('dir \x', False, True);
  TestPathHasInvalidCharacters('.', False, True);
  TestPathHasInvalidCharacters('..', False, True);
  TestPathHasInvalidCharacters('C:', False, True);
  TestPathHasInvalidCharacters('a:b', False, True);
  TestPathHasInvalidCharacters('ab:', True, True);
  TestPathHasInvalidCharacters('.:stream', True, True);
  TestPathHasInvalidCharacters(' :stream', True, True);
  TestPathHasInvalidCharacters('\:stream', True, True);
  TestPathHasInvalidCharacters('C:\a:b', True, True);

  TestPathComponentIsReservedName('', False);
  TestPathComponentIsReservedName('CON', True);
  TestPathComponentIsReservedName('Con', True);
  TestPathComponentIsReservedName('CONSOLE', False);
  TestPathComponentIsReservedName('PRN', True);
  TestPathComponentIsReservedName('AUX', True);
  TestPathComponentIsReservedName('NUL', True);
  TestPathComponentIsReservedName('NULL', False);
  TestPathComponentIsReservedName('NUL ', True);
  TestPathComponentIsReservedName('NUL  ', True);
  TestPathComponentIsReservedName('NUL:', True);
  TestPathComponentIsReservedName('NUL: ', True);
  TestPathComponentIsReservedName('NUL:xxx', True);
  TestPathComponentIsReservedName('NUL :', True);
  TestPathComponentIsReservedName(' NUL', False);
  TestPathComponentIsReservedName('CONIN$', True);
  TestPathComponentIsReservedName('CONOUT$', True);
  TestPathComponentIsReservedName('COM0', False);
  TestPathComponentIsReservedName('COM1', True);
  TestPathComponentIsReservedName('COM9', True);
  TestPathComponentIsReservedName('COM'+#$00B9, True);
  TestPathComponentIsReservedName('COM'+#$00B2, True);
  TestPathComponentIsReservedName('COM'+#$00B3, True);
  TestPathComponentIsReservedName('COM'+#$00B4, False);
  TestPathComponentIsReservedName('COM10', False);
  TestPathComponentIsReservedName('LPT1', True);
  TestPathComponentIsReservedName('LPT9', True);
  TestPathComponentIsReservedName('LPT'+#$00B9, True);
  TestPathComponentIsReservedName('LPTA', False);
  TestPathComponentIsReservedName('NUL.txt', True);
  TestPathComponentIsReservedName('NUL.txt:', True);
  TestPathComponentIsReservedName('NUL.tar.gz', True);
  TestPathComponentIsReservedName('NUL .txt', True);
  TestPathComponentIsReservedName('NUL X.txt', False);
  TestPathComponentIsReservedName('COM1.anything', True);
  TestPathComponentIsReservedName('sub\NUL', False);

  TestValidateAndCombinePath('c:\dest\', 'sub\file.txt', 'c:\dest\sub\file.txt', True); { success }
  TestValidateAndCombinePath('c:\dest\', '', '', False);                                { empty }
  TestValidateAndCombinePath('c:\dest\', 'c:\file', '', False);                         { rooted }
  TestValidateAndCombinePath('c:\dest\', 'a/b', '', False);                             { not normalized }
  TestValidateAndCombinePath('c:\dest\', 'sub\', '', False);                            { trailing slash }
  TestValidateAndCombinePath('c:\dest\', '..\x', '', False);                            { invalid chars (incl. ..) }
  TestValidateAndCombinePath('c:\dest\', 'sub\NUL', '', False);                         { reserved name }

  TestPathConvertNormalToSuper('C:\dir\file', '\\?\C:\dir\file', True);
  TestPathConvertNormalToSuper('C:', '\\?\' + PathExpand('C:'), True);
  TestPathConvertNormalToSuper('\\server\share\x', '\\?\UNC\server\share\x', True);
  TestPathConvertNormalToSuper('\\.\C:\x', '\\?\C:\x', True);
  TestPathConvertNormalToSuper('\\?\C:\x', '\\?\C:\x', True);
  TestPathConvertNormalToSuper('sub\file', '\\?\' + AddBackslash(GetCurrentDir) + 'sub\file', True);
  TestPathConvertNormalToSuper('\\', '\\?\UNC\', True);
  TestPathConvertNormalToSuper('', '', False);

  TestPathConvertSuperToNormal('\\?\C:', 'C:\');
  TestPathConvertSuperToNormal('\\?\C:\', 'C:\');
  TestPathConvertSuperToNormal('\\?\C:\x', 'C:\x');
  TestPathConvertSuperToNormal('\\?\UNC\s\sh', '\\s\sh');
  TestPathConvertSuperToNormal('C:\x', 'C:\x');
  TestPathConvertSuperToNormal('\\s\sh', '\\s\sh');
  TestPathConvertSuperToNormal('', '');

  TestPathLastDelimiter('\/', 'a\b/c', 4);
  TestPathLastDelimiter('\/', 'abc', 0);
  TestPathLastDelimiter('', 'abc\/', 0);
  TestPathLastDelimiter('\/', '', 0);
  TestPathLastDelimiter('', '', 0);
  { Search continues past #0 in S, regardless of scan direction }
  TestPathLastDelimiter('\/', 'a'#0'\b/'#0'c', 5);
  { #0 in Delimiters is allowed; it doesn't end the delimiter set }
  TestPathLastDelimiter('x'#0'y', 'a\'#0'/c', 3);
  TestPathLastDelimiter('\'#0'/', 'a\b/c', 4);

  TestPathLowercase('ABC', 'abc');
  TestPathLowercase('abc', 'abc');
  TestPathLowercase('', '');

  TestPathPos('b', 'abc', 1, 2);
  TestPathPos('b', 'bcb', 2, 3);
  TestPathPos('z', 'abc', 1, 0);
  TestPathPos('a', '', 1, 0);
  { Bad offset }
  TestPathPos('a', 'abc', 0, 0);
  TestPathPos('a', 'abc', 99, 0);

  TestPathLastChar('abc', 'c');
  TestPathLastCharNil('');

  TestPathCharIsSlash('\', True);
  TestPathCharIsSlash('/', True);
  TestPathCharIsSlash('a', False);
  TestPathCharIsSlash(':', False);

  TestPathCharIsDriveLetter('a', True);
  TestPathCharIsDriveLetter('Z', True);
  TestPathCharIsDriveLetter('1', False);
end;

{$IFDEF DEBUG}
{$IFNDEF ISTESTTOOLPROJ}
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
{$ENDIF}

end.
