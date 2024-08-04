unit Shared.VerInfoFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Version info functions
}

interface

uses
  Windows, SysUtils, Shared.Int64Em;

type
  TFileVersionNumbers = record
    MS, LS: LongWord;
  end;

function GetVersionInfo(const Filename: String;
  var VersionInfo: TVSFixedFileInfo): Boolean;
function GetVersionNumbers(const Filename: String;
  var VersionNumbers: TFileVersionNumbers): Boolean;
function StrToVersionNumbers(const S: String;
  var Version: TFileVersionNumbers): Boolean;

implementation

uses
  Shared.CommonFunc, Shared.FileClass;

function GetVersionInfo(const Filename: String;
  var VersionInfo: TVSFixedFileInfo): Boolean;
var
  VersionSize: Integer;
  VersionHandle: DWORD;
  VersionBuf: PChar;
  VerInfo: PVSFixedFileInfo;
  VerInfoSize: UINT;
begin
  Result := False;

  VersionSize := GetFileVersionInfoSize(PChar(Filename), VersionHandle);
  if VersionSize > 0 then begin
    GetMem(VersionBuf, VersionSize);
    try
      if GetFileVersionInfo(PChar(Filename), VersionHandle, VersionSize, VersionBuf) then begin
        if VerQueryValue(VersionBuf, '\', Pointer(VerInfo), VerInfoSize) then begin
          VersionInfo := VerInfo^;
          Result := True;
        end;
      end;
    finally
      FreeMem(VersionBuf);
    end;
  end;
end;

function GetVersionNumbers(const Filename: String;
  var VersionNumbers: TFileVersionNumbers): Boolean;
var
  VerInfo: TVSFixedFileInfo;
begin
  Result := GetVersionInfo(Filename, VerInfo);
  if Result then begin
    VersionNumbers.MS := VerInfo.dwFileVersionMS;
    VersionNumbers.LS := VerInfo.dwFileVersionLS;
  end;
end;

function StrToVersionNumbers(const S: String; var Version: TFileVersionNumbers): Boolean;

  function SplitNextNumber(var Z: String): Word;
  var
    I, N: Integer;
  begin
    if Trim(Z) <> '' then begin
      I := Pos('.', Z);
      if I = 0 then
        I := Length(Z)+1;
      N := StrToInt(Trim(Copy(Z, 1, I-1)));
      if (N < Low(Word)) or (N > High(Word)) then
        Abort;
      Result := N;
      Z := Copy(Z, I+1, Maxint);
    end else
      Result := 0;
  end;

var
  Z: String;
  W: Word;
begin
  try
    Z := S;
    W := SplitNextNumber(Z);
    Version.MS := (DWord(W) shl 16) or SplitNextNumber(Z);
    W := SplitNextNumber(Z);
    Version.LS := (DWord(W) shl 16) or SplitNextNumber(Z);
    Result := True;
  except
    Result := False;
  end;
end;

end.
