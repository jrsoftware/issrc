; -- PowerShell.iss --
; Demonstrates calling Powershell at compile time and at run time.
; At compile time it first generates a random password and then it shows it and copies it to the clipboard.
; At run time it shows the serial number of the system.

; SEE THE DOCUMENTATION FOR DETAILS ON CREATING .ISS SCRIPT FILES!

#define PowerShellExe "powershell.exe"
#define PowerShellCommandParam "-ExecutionPolicy Bypass -Command"

#define ExecPowerShell(str Command) \
  Local[0] = PowerShellCommandParam + " " + AddQuotes(Command), \
  Message("Executing PowerShell command: " + Local[0]), \
  ExecAndGetFirstLine(PowerShellExe, Local[0])

#define Password ExecPowerShell( \
  "Add-Type -AssemblyName 'System.Web';" + \
  "[System.Web.Security.Membership]::GeneratePassword(12, 4);")

#expr ExecPowerShell( \
  "$Password = '" + Password + "';" + \
  "Set-Clipboard -Value $Password;" + \
  "Add-Type -AssemblyName System.Windows.Forms;" + \
  "[System.Windows.Forms.MessageBox]::Show(" + \
    "'The generated password (copied to clipboard) is: ' + $Password + '" + NewLine + NewLine + \
    "Click OK to continue.', 'ISPP')")

[Setup]
AppName=My Program
AppVersion=1.5
WizardStyle=modern
DefaultDirName={autopf}\My Program
DefaultGroupName=My Program
UninstallDisplayIcon={app}\MyProg.exe
Compression=lzma2
SolidCompression=yes
OutputDir=userdocs:Inno Setup Examples Output
Password={#Password}

[Files]
Source: "MyProg.exe"; DestDir: "{app}"
Source: "MyProg.chm"; DestDir: "{app}"
Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme

[Icons]
Name: "{group}\My Program"; Filename: "{app}\MyProg.exe"

[Code]
var
  Line: String;

procedure ExecAndGetFirstLineLog(const S: String; const Error, FirstLine: Boolean);
begin
  if not Error and (Line = '') and (Trim(S) <> '') then
    Line := S; { First non-empty line found, store it }
  Log('Exec output: ' + S);
end;

function ExecAndGetFirstLine(const Filename, Params, WorkingDir: String; var ResultCode: Integer): String;
begin
  Line := '';
  try
    ExecAndLogOutput(Filename, Params, WorkingDir, SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode, @ExecAndGetFirstLineLog);
  except
    Log(GetExceptionMessage);
  end;
  Result := Line;
end;

function ExecPowerShell(const Command: String): String;
var
  FullCommand: String;
  ResultCode: Integer;
begin
  FullCommand := '{#PowerShellCommandParam} ' + AddQuotes(Command);
  Log('Executing PowerShell command: ' + FullCommand);
  Result := ExecAndGetFirstLine('{#PowerShellExe}', FullCommand, '', ResultCode);
end;

function InitializeSetup: Boolean;
var
  SerialNumber: String;
begin
  SerialNumber := ExecPowerShell('Get-WmiObject -Class Win32_BIOS | Select-Object -ExpandProperty SerialNumber');
  MsgBox(Format('Serial number: %s'#10#10'Click OK to continue.', [SerialNumber]), mbInformation, MB_OK);
  Result := True;
end;