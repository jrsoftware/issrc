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

end.
