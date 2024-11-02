unit PathFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  This unit provides some path-related functions.
}

interface

function AddBackslash(const S: String): String;
function PathChangeExt(const Filename, Extension: String): String;
function PathCharCompare(const S1, S2: PChar): Boolean;
function PathCharIsSlash(const C: Char): Boolean;
function PathCharIsTrailByte(const S: String; const Index: Integer): Boolean;
function PathCharLength(const S: String; const Index: Integer): Integer;
function PathCombine(const Dir, Filename: String): String;
function PathCompare(const S1, S2: String): Integer;
function PathDrivePartLength(const Filename: String): Integer;
function PathDrivePartLengthEx(const Filename: String;
  const IncludeSignificantSlash: Boolean): Integer;
function PathExpand(const Filename: String): String; overload;
function PathExpand(const Filename: String; out ExpandedFilename: String): Boolean; overload;
function PathExtensionPos(const Filename: String): Integer;
function PathExtractDir(const Filename: String): String;
function PathExtractDrive(const Filename: String): String;
function PathExtractExt(const Filename: String): String;
function PathExtractName(const Filename: String): String;
function PathExtractPath(const Filename: String): String;
function PathIsRooted(const Filename: String): Boolean;
function PathLastChar(const S: String): PChar;
function PathLastDelimiter(const Delimiters, S: string): Integer;
function PathLowercase(const S: String): String;
function PathNormalizeSlashes(const S: String): String;
function PathPathPartLength(const Filename: String;
  const IncludeSlashesAfterPath: Boolean): Integer;
function PathPos(Ch: Char; const S: String): Integer;
function PathStartsWith(const S, AStartsWith: String): Boolean;
function PathStrNextChar(const S: PChar): PChar;
function PathStrPrevChar(const Start, Current: PChar): PChar;
function PathStrScan(const S: PChar; const C: Char): PChar;
function RemoveBackslash(const S: String): String;
function RemoveBackslashUnlessRoot(const S: String): String;

implementation

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
var
  N, I: Integer;
begin
  N := PathStrNextChar(S1) - S1;
  if N = PathStrNextChar(S2) - S2 then begin
    for I := 0 to N-1 do begin
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

function PathCompare(const S1, S2: String): Integer;
{ Compares two filenames, and returns 0 if they are equal. }
begin
  Result := CompareStr(PathLowercase(S1), PathLowercase(S2));
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
  Len, I, C: Integer;
begin
  Len := Length(Filename);

  { \\server\share }
  if (Len >= 2) and PathCharIsSlash(Filename[1]) and PathCharIsSlash(Filename[2]) then begin
    I := 3;
    C := 0;
    while I <= Len do begin
      if PathCharIsSlash(Filename[I]) then begin
        Inc(C);
        if C >= 2 then
          Break;
        repeat
          Inc(I);
          { And skip any additional consecutive slashes: }
        until (I > Len) or not PathCharIsSlash(Filename[I]);
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
  if Result then
    SetString(ExpandedFilename, Buf, Res)
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
  ends in a slash or consists only of a drive part, the result will be an empty
  string.
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

function PathLastChar(const S: String): PChar;
{ Returns pointer to last character in the string. Returns nil if the string is
  empty. }
begin
  if S = '' then
    Result := nil
  else
    Result := PathStrPrevChar(Pointer(S), @S[Length(S)+1]);
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
  E := @P[Length(S)];
  while P < E do begin
    if P^ <> #0 then begin
      if StrScan(PChar(Pointer(Delimiters)), P^) <> nil then
        Result := (P - PChar(Pointer(S))) + 1;
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

function PathNormalizeSlashes(const S: String): String;
{ Returns S minus any superfluous slashes, and with any forward slashes
  converted to backslashes. For example, if S is 'C:\\\some//path', it returns
  'C:\some\path'. Does not remove a double backslash at the beginning of the
  string, since that signifies a UNC path. }
var
  Len, I: Integer;
begin
  Result := S;
  Len := Length(Result);
  I := 1;
  while I <= Len do begin
    if Result[I] = '/' then
      Result[I] := '\';
    Inc(I, PathCharLength(Result, I));
  end;
  I := 1;
  while I < Length(Result) do begin
    if (Result[I] = '\') and (Result[I+1] = '\') and (I > 1) then
      Delete(Result, I+1, 1)
    else
      Inc(I, PathCharLength(Result, I));
  end;
end;

function PathStartsWith(const S, AStartsWith: String): Boolean;
{ Returns True if S starts with (or is equal to) AStartsWith. Uses path casing
  rules. }
var
  AStartsWithLen: Integer;
begin
  AStartsWithLen := Length(AStartsWith);
  if Length(S) = AStartsWithLen then
    Result := (PathCompare(S, AStartsWith) = 0)
  else if (Length(S) > AStartsWithLen) and not PathCharIsTrailByte(S, AStartsWithLen+1) then
    Result := (PathCompare(Copy(S, 1, AStartsWithLen), AStartsWith) = 0)
  else
    Result := False;
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

end.
