;Set Compil32 to high prio as well before doing compilation speed test (done automatically for installation speed test)

[Setup]
AppName=My Program
AppVerName=My Program version 1.5
#define DefaultDirName "{autopf}\My Program"
DefaultDirName={#DefaultDirName}
UseSetupLdr=0
OutputDir=.
AppVersion=1.2.3
OutputBaseFilename=Setup
PrivilegesRequired=lowest
MergeDuplicateFiles=no
DisableDirPage=yes

[Files]
#define i
#sub AddFiles
Source: c:\Program Files\Git\usr\bin\*; Flags: ignoreversion; DestDir: "{app}\{#i}"
#endsub
#for {i = 0; i < 10; i++} AddFiles

[Code]
function SetPriorityClass(hProcess: THandle; dwPriorityClass: DWORD): BOOL; external 'SetPriorityClass@kernel32.dll stdcall';
function GetCurrentProcess: THandle; external 'GetCurrentProcess@kernel32.dll stdcall';
function GetTickCount: DWORD; external 'GetTickCount@kernel32.dll stdcall';
  
function InitializeSetup: Boolean;
var
  S: String;
  ResultCode: Integer;
begin
  Result := not Debugging;
  if Result then
    Result := SetPriorityClass(GetCurrentProcess, $00000080);
  if Result then begin  
    S := ExpandConstant('{#DefaultDirName}\unins000.exe');
    if FileExists(S) then
      Exec(S, '/silent', '', SW_SHOW, ewWaitUntilTerminated, ResultCode);
  end;
end;

var
  Time: Integer;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    Time := GetTickCount
  else if CurStep = ssPostInstall then begin
    Time := GetTickCount - Time;
    MsgBox(FloatToStr(Time / 1000.0), mbInformation, MB_OK);
  end;
end;