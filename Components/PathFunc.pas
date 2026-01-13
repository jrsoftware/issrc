unit PathFunc;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  This unit provides some path-related functions.

  The string comparison functions (including PathCompare, PathEndsWith, PathHasSubstringAt,
  PathSame, PathStartsWith, PathStrCompare, PathStrFind) all ignore case by
  default, and use a locale-independent "ordinal" comparison, which is important
  when comparing filenames/paths. Despite the "Path" prefix, however, the
  functions can be used to compare any kind of text, not just filenames/paths.

  About super paths (extended-length paths):

  PathHasInvalidCharacters considers all super paths as invalid.

  PathSame and Path(Str)Compare do not consider a super path and its normal
  version to be the same.
}

interface

function AddBackslash(const S: String): String;
function PathChangeExt(const Filename, Extension: String): String;
function PathCharCompare(const S1, S2: PChar): Boolean;
function PathCharIsDriveLetter(const C: Char): Boolean;
function PathCharIsSlash(const C: Char): Boolean;
function PathCharIsTrailByte(const S: String; const Index: Integer): Boolean;
function PathCharLength(const S: String; const Index: Integer): Integer;
function PathCombine(const Dir, Filename: String): String;
function PathCompare(const S1, S2: String; const IgnoreCase: Boolean = True): Integer;
function PathConvertNormalToSuper(var Filename: String): Boolean;
function PathConvertSuperToNormal(const Filename: String): String;
function PathDrivePartLength(const Filename: String): Integer;
function PathDrivePartLengthEx(const Filename: String;
  const IncludeSignificantSlash: Boolean): Integer;
function PathEndsWith(const S, AEndsWith: String;
  const IgnoreCase: Boolean = True): Boolean;
function PathExpand(const Filename: String): String; overload;
function PathExpand(const Filename: String; out ExpandedFilename: String): Boolean; overload;
function PathExtensionPos(const Filename: String): Integer;
function PathExtractDir(const Filename: String): String;
function PathExtractDrive(const Filename: String): String;
function PathExtractExt(const Filename: String): String;
function PathExtractName(const Filename: String): String;
function PathExtractPath(const Filename: String): String;
function PathHasInvalidCharacters(const S: String;
  const AllowDriveLetterColon: Boolean): Boolean;
function PathHasSubstringAt(const S, Substring: String; const Offset: Integer;
  const IgnoreCase: Boolean = True): Boolean;
function PathIsRooted(const Filename: String): Boolean;
function PathLastChar(const S: String): PChar;
function PathLastDelimiter(const Delimiters, S: string): Integer;
function PathLowercase(const S: String): String;
function PathNormalizeSlashes(S: String): String;
function PathPathPartLength(const Filename: String;
  const IncludeSlashesAfterPath: Boolean): Integer;
function PathPos(Ch: Char; const S: String): Integer;
function PathSame(const S1, S2: String): Boolean;
function PathStartsWith(const S, AStartsWith: String;
  const IgnoreCase: Boolean = True): Boolean;
function PathStrCompare(const S1: PChar; const S1Length: Integer;
  const S2: PChar; const S2Length: Integer;
  const IgnoreCase: Boolean = True): Integer;
function PathStrFind(const SSource: PChar; const SSourceLength: Integer;
  const SValue: PChar; const SValueLength: Integer;
  const IgnoreCase: Boolean = True): Integer;
function PathStrNextChar(const S: PChar): PChar;
function PathStrPrevChar(const Start, Current: PChar): PChar;
function PathStrScan(const S: PChar; const C: Char): PChar;
function RemoveBackslash(const S: String): String;
function RemoveBackslashUnlessRoot(const S: String): String;
function ValidateAndCombinePath(const ADestDir, AFilename: String;
  out AResultingPath: String): Boolean; overload;
function ValidateAndCombinePath(const ADestDir, AFilename: String): Boolean; overload;

implementation

{$ZEROBASEDSTRINGS OFF}

uses
  Windows, SysUtils;

function AddBackslash(const S: String): String;
{ Returns S plus a trailing backslash, unless S is an empty string or already
  ends in a backslash/slash. }
begin
  if (S <> '') and not PathCharIsSlash(PathLastChar(S)^) then
    Result := S + '\'
  else
    Result := S;
end;

function PathCharLength(const S: String; const Index: Integer): Integer;
{ Returns the length in characters of the character at Index in S. }
begin
  Result := 1;
end;

function PathCharIsDriveLetter(const C: Char): Boolean;
begin
  Result := CharInSet(C, ['A'..'Z', 'a'..'z']);
end;

function PathCharIsSlash(const C: Char): Boolean;
{ Returns True if C is a backslash or slash. }
begin
  Result := (C = '\') or (C = '/');
end;

function PathCharIsTrailByte(const S: String; const Index: Integer): Boolean;
{ Returns False if S[Index] is a single byte character or a lead byte.
  Returns True otherwise (i.e. it must be a trail byte). }
var
  I: Integer;
begin
  I := 1;
  while I <= Index do begin
    if I = Index then begin
      Result := False;
      Exit;
    end;
    Inc(I, PathCharLength(S, I));
  end;
  Result := True;
end;

function PathCharCompare(const S1, S2: PChar): Boolean;
{ Compares two first characters, and returns True if they are equal. }
begin
  const N = PathStrNextChar(S1) - S1;
  if N = PathStrNextChar(S2) - S2 then begin
    for var I := 0 to N-1 do begin
      if S1[I] <> S2[I] then begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end else
    Result := False;
end;

function PathChangeExt(const Filename, Extension: String): String;
{ Takes Filename, removes any existing extension, then adds the extension
  specified by Extension and returns the resulting string. }
var
  I: Integer;
begin
  I := PathExtensionPos(Filename);
  if I = 0 then
    Result := Filename + Extension
  else
    Result := Copy(Filename, 1, I - 1) + Extension;
end;

function PathCombine(const Dir, Filename: String): String;
{ Combines a directory and filename into a path.
  If Dir is empty, it just returns Filename.
  If Filename is empty, it returns an empty string (ignoring Dir).
  If Filename begins with a drive letter or slash, it returns Filename
  (ignoring Dir).
  If Dir specifies only a drive letter and colon ('c:'), it returns
  Dir + Filename.
  Otherwise, it returns the equivalent of AddBackslash(Dir) + Filename. }
var
  I: Integer;
begin
  if (Dir = '') or (Filename = '') or PathIsRooted(Filename) then
    Result := Filename
  else begin
    I := PathCharLength(Dir, 1) + 1;
    if ((I = Length(Dir)) and (Dir[I] = ':')) or
       PathCharIsSlash(PathLastChar(Dir)^) then
      Result := Dir + Filename
    else
      Result := Dir + '\' + Filename;
  end;
end;

function PathCompare(const S1, S2: String; const IgnoreCase: Boolean = True): Integer;
{ Compares two strings (typically filenames, but they don't have to be) and
  returns 0 for "equal", <0 for "less than", or >0 for "greater than".
  An ordinal comparison is used, ignoring case by default. }
begin
  Result := PathStrCompare(PChar(S1), Length(S1), PChar(S2), Length(S2),
    IgnoreCase);
end;

function PathConvertNormalToSuper(var Filename: String): Boolean;
begin
  if Length(Filename) >= 3 then begin
    if PathStartsWith(Filename, '\\?\') then
      Exit(True);

    if PathStartsWith(Filename, '\\.\') then begin
      Filename[3] := '?';
      Exit(True);
    end;

    if CharInSet(UpCase(Filename[1]), ['A'..'Z']) and
       (Filename[2] = ':') and (Filename[3] = '\') then begin
      Insert('\\?\', Filename, 1);
      Exit(True);
    end;

    if (Filename[1] = '\') and (Filename[2] = '\') then begin
      Filename := '\\?\UNC\' + Copy(Filename, 3, Maxint);
      Exit(True);
    end;
  end;
  Result := False;
end;


function PathConvertSuperToNormal(const Filename: String): String;
{ Attempts to convert a "\\?\"-prefixed path to normal form, and returns the
  new path. If the path cannot be converted, then Filename is returned
  unchanged.
  Reasons why a path cannot be converted include:
  - The path doesn't start with "\\?\" (i.e., it's already in normal form)
  - The prefix isn't followed by a drive letter and colon, or "UNC\".
    ("\\?\GLOBALROOT\" isn't supported.)
  - The path contains forward slashes or unnecessarily repeated backslashes.
    This function doesn't support them.
  Examples of conversions:
    \\?\C:               -> C:\
    \\?\C:\              -> C:\
    \\?\C:\xxx           -> C:\xxx
    \\?\UNC\server\share -> \\server\share
}
begin
  if PathStartsWith(Filename, '\\?\UNC\') then
    Exit('\\' + Copy(Filename, 9, Maxint));

  const Len = Length(Filename);
  if (Len >= 6) and PathStartsWith(Filename, '\\?\') and
     CharInSet(UpCase(Filename[5]), ['A'..'Z']) and
     (Filename[6] = ':') then begin
    { "\\?\C:\" or "\\?\C:\xxx" }
    if (Len >= 7) and (Filename[7] = '\') then
      Exit(Copy(Filename, 5, Maxint));
    { "\\?\C:" -- in this case we need to append "\" so the result is "C:\" }
    if Len = 6 then
      Exit(Copy(Filename, 5, Maxint) + '\');
    { "\\?\C:xxx" -- not valid, can't convert }
  end;

  Result := Filename;
end;

function PathDrivePartLength(const Filename: String): Integer;
begin
  Result := PathDrivePartLengthEx(Filename, False);
end;

function PathDrivePartLengthEx(const Filename: String;
  const IncludeSignificantSlash: Boolean): Integer;
{ Returns length of the drive portion of Filename, or 0 if there is no drive
  portion.
  If IncludeSignificantSlash is True, the drive portion can include a trailing
  slash if it is significant to the meaning of the path (i.e. 'x:' and 'x:\'
  are not equivalent, nor are '\' and '').
  If IncludeSignificantSlash is False, the function works as follows:
    'x:file'              -> 2  ('x:')
    'x:\file'             -> 2  ('x:')
    '\\server\share\file' -> 14 ('\\server\share')
    '\file'               -> 0  ('')
  If IncludeSignificantSlash is True, the function works as follows:
    'x:file'              -> 2  ('x:')
    'x:\file'             -> 3  ('x:\')
    '\\server\share\file' -> 14 ('\\server\share')
    '\file'               -> 1  ('\')
}
var
  Len, I: Integer;
begin
  Len := Length(Filename);

  { Standard UNC path:
      \\server\share
    Super/device-namespace drives:
      \\?\C:
      \\.\C:
      \\?\C:\   (trailing slash kept when IncludeSignificantSlash is True)
      \\.\C:\
    Super/device-namespace UNC paths (4 components):
      \\?\UNC\server\share
      \\.\UNC\server\share }
  if (Len >= 2) and PathCharIsSlash(Filename[1]) and PathCharIsSlash(Filename[2]) then begin
    { For consistency with the behavior of Windows' GetFullPathName function,
      we don't collapse 3 leading slashes into 2. Instead, a leading "\\\" is
      treated like "\\<empty computer name>\".
      PathNormalizeSlashes works the same way. }
    var FirstComponentIsNamespace := False;
    var SecondComponentIsDrive := False;
    var MaxComponents := 2;
    var CurComponent := 0;
    var ComponentStartIndex := 3;
    I := ComponentStartIndex;
    while I <= Len do begin
      if PathCharIsSlash(Filename[I]) then begin
        const ComponentLen = I - ComponentStartIndex;
        case CurComponent of
          0: begin
               if (ComponentLen = 1) and
                  CharInSet(Filename[ComponentStartIndex], ['?', '.']) then
                 FirstComponentIsNamespace := True;
             end;
          1: if FirstComponentIsNamespace then begin
               if ComponentLen = 2 then begin
                 if PathCharIsDriveLetter(Filename[ComponentStartIndex]) and
                    (Filename[ComponentStartIndex+1] = ':') then
                   SecondComponentIsDrive := True;
               end
               else if ComponentLen = 3 then begin
                 if PathHasSubstringAt(Filename, 'UNC', ComponentStartIndex - Low(Filename)) then
                   Inc(MaxComponents, 2);
               end;
             end;
        end;
        Inc(CurComponent);
        if CurComponent >= MaxComponents then begin
          if SecondComponentIsDrive and IncludeSignificantSlash then
            Inc(I);
          Break;
        end;
        repeat
          Inc(I);
          { And skip any additional consecutive slashes: }
        until (I > Len) or not PathCharIsSlash(Filename[I]);
        ComponentStartIndex := I;
      end
      else
        Inc(I, PathCharLength(Filename, I));
    end;
    Result := I - 1;
    Exit;
  end;

  { \ }
  { Note: Test this before 'x:' since '\:stream' means access stream 'stream'
    on the root directory of the current drive, not access drive '\:' }
  if (Len >= 1) and PathCharIsSlash(Filename[1]) then begin
    if IncludeSignificantSlash then
      Result := 1
    else
      Result := 0;
    Exit;
  end;

  { x: }
  if Len > 0 then begin
    I := PathCharLength(Filename, 1) + 1;
    if (I <= Len) and (Filename[I] = ':') then begin
      if IncludeSignificantSlash and (I < Len) and PathCharIsSlash(Filename[I+1]) then
        Result := I+1
      else
        Result := I;
      Exit;
    end;
  end;

  Result := 0;
end;

function PathIsRooted(const Filename: String): Boolean;
{ Returns True if Filename begins with a slash or drive ('x:').
  Equivalent to: PathDrivePartLengthEx(Filename, True) <> 0 }
var
  Len, I: Integer;
begin
  Result := False;
  Len := Length(Filename);
  if Len > 0 then begin
    { \ or \\ }
    if PathCharIsSlash(Filename[1]) then
      Result := True
    else begin
      { x: }
      I := PathCharLength(Filename, 1) + 1;
      if (I <= Len) and (Filename[I] = ':') then
        Result := True;
    end;
  end;
end;

function PathPathPartLength(const Filename: String;
  const IncludeSlashesAfterPath: Boolean): Integer;
{ Returns length of the path portion of Filename, or 0 if there is no path
  portion.
  Note these differences from Delphi's ExtractFilePath function:
  - The result will never be less than what PathDrivePartLength returns.
    If you pass a UNC root path, e.g. '\\server\share', it will return the
    length of the entire string, NOT the length of '\\server\'.
  - If you pass in a filename with a reference to an NTFS alternate data
    stream, e.g. 'abc:def', it will return the length of the entire string,
    NOT the length of 'abc:'. }
var
  LastCharToKeep, Len, I: Integer;
begin
  Result := PathDrivePartLengthEx(Filename, True);
  LastCharToKeep := Result;
  Len := Length(Filename);
  I := Result + 1;
  while I <= Len do begin
    if PathCharIsSlash(Filename[I]) then begin
      if IncludeSlashesAfterPath then
        Result := I
      else
        Result := LastCharToKeep;
      Inc(I);
    end
    else begin
      Inc(I, PathCharLength(Filename, I));
      LastCharToKeep := I-1;
    end;
  end;
end;

function PathEndsWith(const S, AEndsWith: String;
  const IgnoreCase: Boolean = True): Boolean;
{ Returns True if S ends with (or is equal to) AEndsWith.
  An ordinal comparison is used, ignoring case by default. }
begin
  Result := PathHasSubstringAt(S, AEndsWith, Length(S) - Length(AEndsWith),
    IgnoreCase);
end;

function PathExpand(const Filename: String; out ExpandedFilename: String): Boolean;
{ Like Delphi's ExpandFileName, but does proper error checking. }
var
  Res: Integer;
  FilePart: PChar;
  Buf: array[0..4095] of Char;
begin
  DWORD(Res) := GetFullPathName(PChar(Filename), SizeOf(Buf) div SizeOf(Buf[0]),
    Buf, FilePart);
  Result := (Res > 0) and (Res < SizeOf(Buf) div SizeOf(Buf[0]));
  if Result then begin
    SetString(ExpandedFilename, Buf, Res);
    { Memory usage optimization: Most of the time, no changes are made to the
      path. When that is the case, return a reference to the passed-in string
      so that there aren't two identical strings on the heap. }
    if ExpandedFilename = Filename then
      ExpandedFilename := Filename;
  end;
end;

function PathExpand(const Filename: String): String;
begin
  if not PathExpand(Filename, Result) then
    Result := Filename;
end;

function PathExtensionPos(const Filename: String): Integer;
{ Returns index of the last '.' character in the filename portion of Filename,
  or 0 if there is no '.' in the filename portion.
  Note: Filename is assumed to NOT include an NTFS alternate data stream name
  (i.e. 'filename:stream'). }
var
  Len, I: Integer;
begin
  Result := 0;
  Len := Length(Filename);
  I := PathPathPartLength(Filename, True) + 1;
  while I <= Len do begin
    if Filename[I] = '.' then begin
      Result := I;
      Inc(I);
    end
    else
      Inc(I, PathCharLength(Filename, I));
  end;
end;

function PathExtractDir(const Filename: String): String;
{ Like PathExtractPath, but strips any trailing slashes, unless the resulting
  path is the root directory of a drive (i.e. 'C:\' or '\'). }
var
  I: Integer;
begin
  I := PathPathPartLength(Filename, False);
  Result := Copy(Filename, 1, I);
end;

function PathExtractDrive(const Filename: String): String;
{ Returns the drive portion of Filename (either 'x:' or '\\server\share'),
  or an empty string if there is no drive portion. }
var
  L: Integer;
begin
  L := PathDrivePartLength(Filename);
  if L = 0 then
    Result := ''
  else
    Result := Copy(Filename, 1, L);
end;

function PathExtractExt(const Filename: String): String;
{ Returns the extension portion of the last component of Filename (e.g. '.txt')
  or an empty string if there is no extension. }
var
  I: Integer;
begin
  I := PathExtensionPos(Filename);
  if I = 0 then
    Result := ''
  else
    Result := Copy(Filename, I, Maxint);
end;

function PathExtractName(const Filename: String): String;
{ Returns the filename portion of Filename (e.g. 'filename.txt'). If Filename
  ends in a slash or consists only of a drive part or is empty, the result will
  be an empty string.
  This function is essentially the opposite of PathExtractPath. }
var
  I: Integer;
begin
  I := PathPathPartLength(Filename, True);
  Result := Copy(Filename, I + 1, Maxint);
end;

function PathExtractPath(const Filename: String): String;
{ Returns the path portion of Filename (e.g. 'c:\dir\'). If Filename contains
  no drive part or slash, the result will be an empty string.
  This function is essentially the opposite of PathExtractName. }
var
  I: Integer;
begin
  I := PathPathPartLength(Filename, True);
  Result := Copy(Filename, 1, I);
end;

function PathHasInvalidCharacters(const S: String;
  const AllowDriveLetterColon: Boolean): Boolean;
{ Checks the specified path for characters that are never allowed in paths,
  or characters and path components that are accepted by the system but might
  present a security problem (such as '..' and sometimes ':').
  Specifically, True is returned if S includes any of the following:
  - Control characters (0-31)
  - One of these characters: /*?"<>|
    (This means forward slashes and the prefixes '\\?\' and '\??\' are never
    allowed.)
  - Colons (':'), except when AllowDriveLetterColon=True and the string's
    first character is a letter and the second character is the only colon.
    (This blocks NTFS alternate data stream names.)
  - A component with a trailing dot or space

  Due to the last rule above, '.' and '..' components are never allowed, nor
  are components like these:
    'file '
    'file.'
    'file. . .'
    'file . . '
  When expanding paths (with no '\\?\' prefix used), Windows 11 23H2 silently
  removes all trailing dots and spaces from the end of the string. Therefore,
  if used at the end of a path, all of the above cases yield just 'file'.
  On preceding components of the path, nothing is done with spaces; if there
  is exactly one dot at the end, it is removed (e.g., 'dir.\file' becomes
  'dir\file'), while multiple dots are left untouched ('dir..\file' doesn't
  change).
  By rejecting trailing dots and spaces up front, we avoid all that weirdness
  and the problems that could arise from it.

  Since ':' is considered invalid (except in the one case noted above), it's
  not possible to sneak in disallowed dots/spaces by including an NTFS
  alternate data stream name. The function will return True in these cases:
    '..:streamname'
    'file :streamname'
}
begin
  Result := True;
  for var I := Low(S) to High(S) do begin
    var C := S[I];
    if Ord(C) < 32 then
      Exit;
    case C of
      #32, '.':
        begin
          if (I = High(S)) or PathCharIsSlash(S[I+1]) then
            Exit;
        end;
      ':':
        begin
          { The A-Z check ensures that '.:streamname', ' :streamname', and
            '\:streamname' are disallowed. }
          if not AllowDriveLetterColon or (I <> Low(S)+1) or
             not CharInSet(S[Low(S)], ['A'..'Z', 'a'..'z']) then
            Exit;
        end;
      '/', '*', '?', '"', '<', '>', '|': Exit;
    end;
  end;
  Result := False;
end;

function PathHasSubstringAt(const S, Substring: String; const Offset: Integer;
  const IgnoreCase: Boolean = True): Boolean;
{ Returns True if Substring exists in S at the specified zero-based offset
  from the beginning of S.
  An ordinal comparison is used, ignoring case by default.
  Passing an out-of-range Offset value is allowed/safe. False is returned if
  Offset is negative or if checking for Substring at Offset would go beyond
  the end of S (partially or fully).
  If Substring is empty and Offset = Length(S), then True is returned because
  that is not considered going *beyond* the end of S. }
begin
  if Offset < 0 then
    Exit(False);
  const SubstringLen = Length(Substring);
  if Offset > Length(S) - SubstringLen then
    Exit(False);

  Result := (PathStrCompare(PChar(S) + Offset, SubstringLen, PChar(Substring),
    SubstringLen, IgnoreCase) = 0);
end;

function PathLastChar(const S: String): PChar;
{ Returns pointer to last character in the string. Returns nil if the string is
  empty. }
begin
  if S = '' then
    Result := nil
  else
    Result := @S[High(S)];
end;

function PathLastDelimiter(const Delimiters, S: string): Integer;
{ Returns the index of the last occurrence in S of one of the characters in
  Delimiters, or 0 if none were found.
  Note: S is allowed to contain null characters. }
var
  P, E: PChar;
begin
  Result := 0;
  if (S = '') or (Delimiters = '') then
    Exit;
  P := Pointer(S);
  E := P + Length(S);
  while P < E do begin
    if P^ <> #0 then begin
      if StrScan(PChar(Pointer(Delimiters)), P^) <> nil then
        Result := Integer((P - PChar(Pointer(S))) + 1);
      P := PathStrNextChar(P);
    end
    else
      Inc(P);
  end;
end;

function PathLowercase(const S: String): String;
{ Converts the specified path name to lowercase }
begin
  Result := AnsiLowerCase(S);
end;

function PathPos(Ch: Char; const S: String): Integer;
var
  Len, I: Integer;
begin
  Len := Length(S);
  I := 1;
  while I <= Len do begin
    if S[I] = Ch then begin
      Result := I;
      Exit;
    end;
    Inc(I, PathCharLength(S, I));
  end;
  Result := 0;
end;

function PathNormalizeSlashes(S: String): String;
{ Returns S minus any superfluous slashes, and with any forward slashes
  converted to backslashes. For example, if S is 'C:\\\some//path', it returns
  'C:\some\path'.
  If the string starts with two slashes ('\\') then those two characters are
  ignored when collapsing repeated slashes. So:
    \\server\share   -> \\server\share   (unchanged)
    \\\server\share  -> \\\server\share  (unchanged)
    \\\\server\share -> \\\server\share  (one backslash removed)
  Note that paths with 3+ leading slashes don't actually work. But Windows'
  GetFullPathName function, used by PathExpand, collapses slashes the same
  way. Best to be consistent. }
begin
  const Len = Length(S);
  var I: Integer;
  for I := 1 to Len do
    if S[I] = '/' then
      S[I] := '\';

  var EndIndex := 2;
  if (Len >= 2) and (S[1] = '\') and (S[2] = '\') then
    Inc(EndIndex, 2);

  for I := Len downto EndIndex do
    if (S[I] = '\') and (S[I-1] = '\') then
      Delete(S, I, 1);

  Result := S;
end;

function PathSame(const S1, S2: String): Boolean;
{ Returns True if the specified strings (typically filenames) are equal.
  An ordinal comparison is used, ignoring case.
  Like PathCompare, but faster for checking equality as it returns False
  immediately if the strings are different lengths. }
begin
  Result := (Length(S1) = Length(S2)) and (PathCompare(S1, S2) = 0);
end;

function PathStartsWith(const S, AStartsWith: String;
  const IgnoreCase: Boolean = True): Boolean;
{ Returns True if S starts with (or is equal to) AStartsWith.
  An ordinal comparison is used, ignoring case by default. }
begin
  Result := PathHasSubstringAt(S, AStartsWith, 0, IgnoreCase);
end;

{ Use our own CompareStringOrdinal declaration. The one in the Windows unit is
  "delayload" (yuck), and the bIgnoreCase parameter type differs between
  Delphi 11 and 12 (BOOL vs. DWORD). }
function CompareStringOrdinal_static(lpString1: LPCWSTR; cchCount1: Integer;
  lpString2: LPCWSTR; cchCount2: Integer; bIgnoreCase: BOOL): Integer; stdcall;
  external kernel32 name 'CompareStringOrdinal';

function PathStrCompare(const S1: PChar; const S1Length: Integer;
  const S2: PChar; const S2Length: Integer;
  const IgnoreCase: Boolean = True): Integer;
{ Compares two strings and returns 0 for "equal", <0 for "less than", or
  >0 for "greater than".
  An ordinal comparison is used, ignoring case by default.
  A length of -1 may be passed if a string is null-terminated; in that case,
  the length is determined automatically. }
begin
  { As documented, CompareStringOrdinal only allows 1 for TRUE in the
    bIgnoreCase parameter. "BOOL(Byte(IgnoreCase))" ensures we pass 1, not the
    usual -1 Delphi passes when a Boolean is implicitly converted to BOOL. }
  const CompareResult = CompareStringOrdinal_static(S1, S1Length, S2, S2Length,
    BOOL(Byte(IgnoreCase)));
  case CompareResult of
    0: raise Exception.CreateFmt('PathStrCompare: CompareStringOrdinal failed (%u)',
         [GetLastError]);
    1..3: ;
  else
    raise Exception.CreateFmt('PathStrCompare: CompareStringOrdinal result invalid (%d)',
      [CompareResult]);
  end;
  Result := CompareResult - 2;
end;

{ Use our own FindStringOrdinal declaration. The one in the Windows unit is
  "delayload" (yuck). }
function FindStringOrdinal_static(dwFindStringOrdinalFlags: DWORD;
  lpStringSource: LPCWSTR; cchSource: Integer;
  lpStringValue: LPCWSTR; cchValue: Integer; bIgnoreCase: BOOL): Integer; stdcall;
  external kernel32 name 'FindStringOrdinal';

function PathStrFind(const SSource: PChar; const SSourceLength: Integer;
  const SValue: PChar; const SValueLength: Integer;
  const IgnoreCase: Boolean = True): Integer;
{ Locates a value in a string, starting with the first character of the string.
  Returns a 0-based index if found, and -1 otherwise.
  An ordinal comparison is used, ignoring case by default.
  A length of -1 may be passed if a string is null-terminated; in that case,
  the length is determined automatically. }
begin
  { This is not documented for FindStringOrdinal, but like CompareStringOrdinal
    it only allows 1 for TRUE in the bIgnoreCase parameter. See above. }
  const CompareResult = FindStringOrdinal_static(FIND_FROMSTART, SSource, SSourceLength,
    SValue, SValueLength, BOOL(Byte(IgnoreCase)));
  if CompareResult = -1 then begin
    const LastError = GetLastError;
    if LastError <> ERROR_SUCCESS then
      raise Exception.CreateFmt('PathStrFind: FindStringOrdinal failed (%u)',
        [LastError]);
  end else if not ((CompareResult >= 0) and (CompareResult < SSourceLength)) then
    raise Exception.CreateFmt('PathStrFind: FindStringOrdinal result invalid (%d)',
      [CompareResult]);
  Result := CompareResult;
end;

function PathStrNextChar(const S: PChar): PChar;
{ Returns pointer to the character after S, unless S points to a null (#0). }
begin
  Result := S;
  if Result^ <> #0 then
    Inc(Result);
end;

function PathStrPrevChar(const Start, Current: PChar): PChar;
{ Returns pointer to the character before Current, unless Current = Start. }
begin
  Result := Current;
  if Result > Start then
    Dec(Result);
end;

function PathStrScan(const S: PChar; const C: Char): PChar;
{ Returns pointer to first occurrence of C in S, or nil if there are no
  occurrences. As with StrScan, specifying #0 for the search character is legal. }
begin
  Result := S;
  while Result^ <> C do begin
    if Result^ = #0 then begin
      Result := nil;
      Break;
    end;
    Result := PathStrNextChar(Result);
  end;
end;

function RemoveBackslash(const S: String): String;
{ Returns S minus any trailing slashes. Use of this function is discouraged;
  use RemoveBackslashUnlessRoot instead when working with file system paths. }
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and PathCharIsSlash(PathStrPrevChar(Pointer(S), @S[I+1])^) do
    Dec(I);
  if I = Length(S) then
    Result := S
  else
    Result := Copy(S, 1, I);
end;

function RemoveBackslashUnlessRoot(const S: String): String;
{ Returns S minus any trailing slashes, unless S specifies the root directory
  of a drive (i.e. 'C:\' or '\'), in which case it leaves 1 slash. }
var
  DrivePartLen, I: Integer;
begin
  DrivePartLen := PathDrivePartLengthEx(S, True);
  I := Length(S);
  while (I > DrivePartLen) and PathCharIsSlash(PathStrPrevChar(Pointer(S), @S[I+1])^) do
    Dec(I);
  if I = Length(S) then
    Result := S
  else
    Result := Copy(S, 1, I);
end;

function ValidateAndCombinePath(const ADestDir, AFilename: String;
  out AResultingPath: String): Boolean;
{ Combines ADestDir and AFilename without allowing a result outside of
  ADestDir and without allowing other security problems.
  Returns True if all security checks pass, with the combination of ADestDir
  and AFilename in AResultingPath.
  ADestDir is assumed to be normalized already and have a trailing backslash.
  AFilename may be a file or directory name. }
begin
  { - Don't allow empty names
    - Don't allow forward slashes or repeated slashes
    - Don't allow rooted (non-relative to current directory) names
    - Don't allow trailing slash
    - Don't allow invalid characters/dots/spaces (this catches '..') }
  Result := False;
  if (AFilename <> '') and
     (AFilename = PathNormalizeSlashes(AFilename)) and
     not PathIsRooted(AFilename) and
     not PathCharIsSlash(AFilename[High(AFilename)]) and
     not PathHasInvalidCharacters(AFilename, False) then begin
    { Our validity checks passed. Now pass the combined path to PathExpand
      (GetFullPathName) to see if it thinks the path needs normalization.
      If the returned path isn't exactly what was passed in, then consider
      the name invalid.
      One way that can happen is if the path ends in an MS-DOS device name:
      PathExpand('c:\path\NUL') returns '\\.\NUL'. Obviously we don't want
      devices being opened, so that must be rejected. }
    var CombinedPath := ADestDir + AFilename;
    var TestExpandedPath: String;
    if PathExpand(CombinedPath, TestExpandedPath) and
       (CombinedPath = TestExpandedPath) then begin
      AResultingPath := CombinedPath;
      Result := True;
    end;
  end;
end;

function ValidateAndCombinePath(const ADestDir, AFilename: String): Boolean;
begin
  var ResultingPath: String;
  Result := ValidateAndCombinePath(ADestDir, AFilename, ResultingPath);
end;

end.
