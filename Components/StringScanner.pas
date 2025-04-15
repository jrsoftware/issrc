unit StringScanner;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TStringScanner
}

interface

uses
  SysUtils;

type
  TStringScanner = record
  strict private
    FStr: String;
    FPosition: Integer;
    function GetReachedEnd: Boolean;
    function GetRemainingCount: Integer;
  public
    class function Create(const AString: String): TStringScanner; static;
    function Consume(const C: Char): Boolean; overload;
    function Consume(const S: String): Boolean; overload;
    function ConsumeMulti(const C: TSysCharSet; const AMinChars: Integer = 1;
      const AMaxChars: Integer = Maxint): Integer;
    function ConsumeMultiToString(const C: TSysCharSet;
      var ACapturedString: String; const AMinChars: Integer = 1;
      const AMaxChars: Integer = Maxint): Integer;
    property ReachedEnd: Boolean read GetReachedEnd;
    property RemainingCount: Integer read GetRemainingCount;
    property Str: String read FStr;
  end;

implementation

{$ZEROBASEDSTRINGS OFF}

{ TStringScanner }

class function TStringScanner.Create(const AString: String): TStringScanner;
begin
  Result.FPosition := 1;
  Result.FStr := AString;
end;

function TStringScanner.Consume(const C: Char): Boolean;
begin
  Result := (GetRemainingCount > 0) and (FStr[FPosition] = C);
  if Result then
    Inc(FPosition);
end;

function TStringScanner.Consume(const S: String): Boolean;
begin
  const SLen = Length(S);
  if SLen > GetRemainingCount then
    Exit(False);

  for var I := 0 to SLen-1 do
    if FStr[FPosition + I] <> S[I+1] then
      Exit(False);

  Inc(FPosition, SLen);
  Result := True;
end;

function TStringScanner.ConsumeMulti(const C: TSysCharSet;
  const AMinChars: Integer = 1; const AMaxChars: Integer = Maxint): Integer;
begin
  if (AMinChars <= 0) or (AMinChars > AMaxChars) then
    raise Exception.Create('TStringScanner.ConsumeMulti: Invalid parameter');

  const Remain = GetRemainingCount;
  if Remain < AMinChars then
    Exit(0);

  Result := 0;
  while (Result < AMaxChars) and (Result < Remain) and
     CharInSet(FStr[FPosition + Result], C) do
    Inc(Result);

  if Result < AMinChars then
    Result := 0
  else
    Inc(FPosition, Result);
end;

function TStringScanner.ConsumeMultiToString(const C: TSysCharSet;
  var ACapturedString: String; const AMinChars: Integer = 1;
  const AMaxChars: Integer = Maxint): Integer;
begin
  const StartPos = FPosition;
  Result := ConsumeMulti(C, AMinChars, AMaxChars);
  if Result > 0 then
    ACapturedString := Copy(FStr, StartPos, Result)
  else
    ACapturedString := '';
end;

function TStringScanner.GetReachedEnd: Boolean;
begin
  Result := (GetRemainingCount = 0);
end;

function TStringScanner.GetRemainingCount: Integer;
begin
  { The "<= 0" check exists to protect against OOB reads in case someone calls
    into an instance that was never properly initialized (via Create).
    Inside TStringScanner, FStr[FPosition] must not be accessed unless
    GetRemainingCount is called first and returns a nonzero value. }

  const Len = Length(FStr);
  if (FPosition <= 0) or (FPosition > Len) then
    Result := 0
  else
    Result := Len - FPosition + 1;
end;

end.
