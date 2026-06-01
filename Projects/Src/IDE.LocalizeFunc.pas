unit IDE.LocalizeFunc;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE localization functions
}

interface

function LStr(const Str: String; const AllowEmpty: Boolean = False): String;
function LStrFmt(const Str: String; const Args: array of const;
  const AllowEmpty: Boolean = False): String;

implementation

uses
  SysUtils;

function LStr(const Str: String; const AllowEmpty: Boolean): String;
begin
  if Str = '' then begin
    if AllowEmpty then
      Exit('')
    else
      raise Exception.Create('Internal error: LStr called with empty string');
  end;
  Result := Str; { Temporary }
end;

function LStrFmt(const Str: String; const Args: array of const;
  const AllowEmpty: Boolean): String;
begin
  Result := Format(LStr(Str, AllowEmpty), Args);
end;

end.
