unit Shared.Int64Em;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Int64 helper functions
}

interface

function Compare64(const N1, N2: Int64): Integer;
function StrToInteger64(const S: String; var X: Int64): Boolean; overload;

implementation

uses
  SysUtils;

function Compare64(const N1, N2: Int64): Integer;
begin
  if N1 = N2 then
    Result := 0
  else if N1 > N2 then
    Result := 1
  else
    Result := -1;
end;

function StrToInteger64(const S: String; var X: Int64): Boolean;
{ Converts a string containing an unsigned decimal number, or hexadecimal
  number prefixed with '$', into an Integer64. Returns True if successful,
  or False if invalid characters were encountered or an overflow occurred.
  Supports digits separators. }
var
  Len, Base, StartIndex, I: Integer;
  V: Int64;
  C: Char;
begin
  Result := False;

  Len := Length(S);
  Base := 10;
  StartIndex := 1;
  if Len > 0 then begin
    if S[1] = '$' then begin
      Base := 16;
      Inc(StartIndex);
    end else if S[1] = '_' then
      Exit;
  end;

  if (StartIndex > Len) or (S[StartIndex] = '_') then
    Exit;
  V := 0;

  try
    for I := StartIndex to Len do begin
      C := UpCase(S[I]);
      case C of
        '0'..'9':
          begin
            V := V * Base;
            V := V + (Ord(C) - Ord('0'));
          end;
        'A'..'F':
          begin
            if Base <> 16 then
              Exit;
            V := V * Base;
            V := V + (Ord(C) - (Ord('A') - 10));
          end;
        '_':
          { Ignore }
      else
        Exit;
      end;
    end;
    X := V;
    Result := True;
  except on E: EOverflow do
    ;
  end;
end;

end.
