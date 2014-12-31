unit CompTypes;

{
  Inno Setup
  Copyright (C) 1997-2014 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Types and functions used by both IDE and ISCC units
}

interface

uses
  Windows, SysUtils, Registry, Classes;

type
  TConfigIniFile = class(TRegIniFile)
  private
    FMutex: THandle;
    FAcquiredMutex: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

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

{ TConfigIniFile }

constructor TConfigIniFile.Create;
begin
  inherited Create('Software\Jordan Russell\Inno Setup');
  { Paranoia: Use a mutex to prevent multiple instances from reading/writing
    to the registry simultaneously }
  FMutex := CreateMutex(nil, False, 'Inno-Setup-IDE-Config-Mutex');
  if FMutex <> 0 then
    if WaitForSingleObject(FMutex, INFINITE) <> WAIT_FAILED then
      FAcquiredMutex := True;
end;

destructor TConfigIniFile.Destroy;
begin
  if FMutex <> 0 then begin
    if FAcquiredMutex then
      ReleaseMutex(FMutex);
    CloseHandle(FMutex);
  end;
  inherited;
end;

end.
