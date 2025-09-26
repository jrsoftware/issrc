unit UnsignedFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Provides unsigned alternatives to Delphi functions that use signed types for parameters or return
  values where unsigned types would have been more appropriate
}

interface

uses
  SysUtils;

function ULength(const S: String): Cardinal; overload;
function ULength(const S: AnsiString): Cardinal; overload;
function ULength(const S: TBytes): Cardinal; overload;

implementation

function ULength(const S: String): Cardinal;
begin
  Result := Cardinal(Length(S));
end;

function ULength(const S: AnsiString): Cardinal;
begin
  Result := Cardinal(Length(S));
end;

function ULength(const S: TBytes): Cardinal;
begin
  Result := Cardinal(Length(S));
end;

end.
