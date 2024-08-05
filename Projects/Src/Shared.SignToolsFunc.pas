unit Shared.SignToolsFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  SignTools functions used by both IDE and ISCC units
}

interface

uses
  SysUtils, Classes, Shared.ConfigIniFile;

procedure ReadSignTools(SignTools: TStringList);
function AddSignToolParam(Sign: string): string;

implementation

procedure ReadSignTools(SignTools: TStringList);
var
  Ini: TConfigIniFile;
  I: Integer;
  S: String;
begin
  Ini := TConfigIniFile.Create;
  try
    { Sign tools }
    SignTools.Clear();
    I := 0;
    repeat
      S := Ini.ReadString('SignTools', 'SignTool' + IntToStr(I), '');
      if S <> '' then
        SignTools.Add(S);
      Inc(I);
    until S = '';
  finally
    Ini.Free;
  end;
end;

function AddSignToolParam(Sign: string): string;
begin
  Result := 'SignTool-' + Sign + #0;
end;

end.
