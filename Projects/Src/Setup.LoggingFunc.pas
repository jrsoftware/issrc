unit Setup.LoggingFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Logging functions
}

interface

procedure Log(const S: String);
procedure LogFmt(const S: String; const Args: array of const);
procedure StartLogging(const Prefix: String);
procedure StartLoggingWithFixedFilename(const Filename: String);
function GetLogFileName: String;
function GetLogActive: Boolean; { Returns True if logging was started or when debugging from the IDE }

const
  SYesNo: array[Boolean] of String = ('No', 'Yes');

implementation

uses
  Windows, SysUtils, Shared.CommonFunc, Shared.FileClass, Setup.DebugClient;

var
  LogFile: TTextFileWriter;
  LogFileName: String;
  LocalTimeBias: Int64;

procedure InitLocalTimeBias;
var
  UTCTime, LocalTime: Int64;
begin
  GetSystemTimeAsFileTime(TFileTime(UTCTime));
  if FileTimeToLocalFileTime(TFileTime(UTCTime), TFileTime(LocalTime)) then begin
    Dec(LocalTime, UTCTime);
    LocalTimeBias := LocalTime;
  end;
end;

procedure GetFixedLocalTime(var ST: TSystemTime);
{ Like GetLocalTime, but uses our LocalTimeBias as the offset, which cannot
  change while the program is running } 
var
  FT: Int64;
begin
  GetSystemTimeAsFileTime(TFileTime(FT));
  Inc(FT, LocalTimeBias);
  FileTimeToSystemTime(TFileTime(FT), ST);
end;

procedure LogLogOpened;
var
  PlusOrMinus: Char;
begin
  var Offset := LocalTimeBias;
  if Offset >= 0 then
    PlusOrMinus := '+'
  else begin
    PlusOrMinus := '-';
    Offset := -Offset;
  end;
  Offset := Offset div (60 * 10000000);
  LogFmt('Log opened. (Time zone: UTC%s%.2u:%.2u)', [PlusOrMinus,
    Offset div 60, Offset mod 60]);
end;

procedure StartLogging(const Prefix: String);
var
  Dir, DateStr, Filename: String;
  I: Cardinal;
  ST: TSystemTime;
  F: TTextFileWriter;
begin
  if Assigned(LogFile) then
    Exit;  { logging was already started }
  Dir := GetTempDir;
  GetFixedLocalTime(ST);
  DateStr := Format('%.4u-%.2u-%.2u', [ST.wYear, ST.wMonth, ST.wDay]);
  I := 1;
  while True do begin
    Filename := Dir + Format('%s Log %s #%.3u.txt', [Prefix, DateStr, I]);
    if not FileOrDirExists(Filename) then begin
      F := nil;
      try
        F := TTextFileWriter.Create(Filename, fdCreateNew, faWrite, fsRead);
      except
        on E: EFileError do begin
          { Don't propagate ERROR_FILE_EXISTS errors; just try again.
            (Yes, we already checked if the file existed first, but this helps
            to make it race-proof.) }
          if E.ErrorCode <> ERROR_FILE_EXISTS then
            raise;
        end;
      end;
      if Assigned(F) then begin
        LogFile := F;
        LogFileName := FileName;
        Break;
      end;
    end;
    Inc(I);
  end;
  LogLogOpened;
end;

procedure StartLoggingWithFixedFilename(const Filename: String);
begin
  if Assigned(LogFile) then
    Exit;  { logging was already started }
  LogFile := TTextFileWriter.Create(Filename, fdCreateAlways, faWrite, fsRead);
  LogFileName := FileName;
  LogLogOpened;
end;

function GetLogFileName: String;
begin
  Result := LogFileName;
end;

function GetLogActive: Boolean;
begin
  Result := Assigned(LogFile) or Debugging;
end;

procedure Log(const S: String);

  procedure WriteStr(const S: String);
  begin
    LogFile.Write(S);
  end;

var
  ST: TSystemTime;
  LineStart, I: Integer;
begin
  if Assigned(LogFile) then begin
    GetFixedLocalTime(ST);
    try
      WriteStr(Format('%.4u-%.2u-%.2u %.2u:%.2u:%.2u.%.3u   ',
        [ST.wYear, ST.wMonth, ST.wDay, ST.wHour, ST.wMinute, ST.wSecond,
         ST.wMilliseconds]));
      LineStart := 1;
      { Lines except for last line }
      for I := 1 to Length(S) do begin
        if S[I] = #10 then begin
          WriteStr(Copy(S, LineStart, I - LineStart + 1));
          LineStart := I + 1;
          { Indent }
          WriteStr('                          ');
        end;
      end;
      { Last line }
      if LineStart <= Length(S) then
        WriteStr(Copy(S, LineStart, Length(S) - LineStart + 1));
      WriteStr(#13#10);
    except
      { Failed to write? Close the file and don't log anything further. }
      try
        FreeAndNil(LogFile);
      except
      end;
    end;
  end;
  if Debugging then
    DebugNotifyLogMessage(S);
end;

procedure LogFmt(const S: String; const Args: array of const);
begin
  if GetLogActive then
    Log(Format(S, Args));
end;

initialization
  InitLocalTimeBias;
finalization
  if Assigned(LogFile) then begin
    Log('Log closed.');
    FreeAndNil(LogFile);
  end;
end.
