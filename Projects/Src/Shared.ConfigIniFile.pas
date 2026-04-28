unit Shared.ConfigIniFile;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
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
    procedure AcquireMutex;
  public
    constructor Create;
    constructor CreateReadOnly;
    destructor Destroy; override;
  end;

implementation

const
  ConfigRegKey = 'Software\Jordan Russell\Inno Setup';

{ TConfigIniFile }

procedure TConfigIniFile.AcquireMutex;
begin
  { Paranoia: Use a mutex to prevent multiple instances from reading/writing
    to the registry simultaneously }
  FMutex := CreateMutex(nil, False, 'Inno-Setup-IDE-Config-Mutex');
  if FMutex <> 0 then
    if WaitForSingleObject(FMutex, INFINITE) <> WAIT_FAILED then
      FAcquiredMutex := True;
end;

constructor TConfigIniFile.Create;
begin
  inherited Create(ConfigRegKey);
  AcquireMutex;
end;

constructor TConfigIniFile.CreateReadOnly;
begin
  inherited Create(ConfigRegKey, KEY_READ);
  AcquireMutex;
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
