unit Shared.ConfigIniFile;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  ConfigIniFile class used by both IDE and ISCC units
}

interface

uses
  Windows, Registry;

type
  TConfigIniFile = class(TRegIniFile)
  private
    FMutex: THandle;
    FAcquiredMutex: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    class function Exists: Boolean; static;
  end;

implementation

{ TConfigIniFile }

const
  SubKeyName = 'Software\Jordan Russell\Inno Setup';

constructor TConfigIniFile.Create;
begin
  inherited Create(SubKeyName); { This always creates the key }
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

{ When reading, use this function before calling Create to
  prevent it from always creating the key }
class function TConfigIniFile.Exists: Boolean;
begin
  const Registry = TRegistry.Create;
  try
    Result := Registry.OpenKeyReadOnly(SubKeyName);
  finally
    Registry.Free;
  end;
end;

end.
