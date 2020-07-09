; -- CodeDownloadFiles.iss --
;
; This script shows how the PrepareToInstall event function can be used to
; download temporary files.

[Setup]
AppName=My Program
AppVersion=1.5
WizardStyle=modern
DefaultDirName={autopf}\My Program
DefaultGroupName=My Program
UninstallDisplayIcon={app}\MyProg.exe
OutputDir=userdocs:Inno Setup Examples Output

[Files]
; Place any regular files here
Source: "MyProg.exe"; DestDir: "{app}";
Source: "MyProg.chm"; DestDir: "{app}";
Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme;
; These files will be downloaded
Source: "{tmp}\innosetup-latest.exe"; DestDir: "{app}"; Flags: external
Source: "{tmp}\ISCrypt.dll"; DestDir: "{app}"; Flags: external

[Icons]
Name: "{group}\My Program"; Filename: "{app}\MyProg.exe"

[Code]
var
  DownloadStatusLabel, DownloadFilenameLabel: TNewStaticText;
  DownloadProgressBar: TNewProgressBar;

procedure SetupDownloadControl(const Dest, Src: TControl; const Parent: TWinControl);
begin
  Dest.Left := Src.Left;
  Dest.Top := Src.Top;
  Dest.Width := Src.Width;
  Dest.Height := Src.Height;
  if Src is TNewStaticText then
    TNewStaticText(Dest).Anchors := TNewStaticText(Src).Anchors
  else if Src is TNewProgressBar then
    TNewProgressBar(Dest).Anchors := TNewProgressBar(Src).Anchors;
  Dest.Visible := False;
  Dest.Parent := Parent;
end;

procedure CreateDownloadControls;
var
  Page: TWizardPage;
begin
  Page := PageFromID(wpPreparing);

  DownloadStatusLabel := TNewStaticText.Create(Page);
  SetupDownloadControl(DownloadStatusLabel, WizardForm.StatusLabel, Page.Surface);
  DownloadFilenameLabel := TNewStaticText.Create(Page);
  SetupDownloadControl(DownloadFilenameLabel, WizardForm.FilenameLabel, Page.Surface);
  DownloadProgressBar:= TNewProgressBar.Create(Page);
  SetupDownloadControl(DownloadProgressBar, WizardForm.ProgressGauge, Page.Surface);
end;

procedure InitializeWizard;
begin
  CreateDownloadControls;
end;

function OnDownloadProgress(const Url, FileName: String; const Progress, ProgressMax: Int64): Boolean;
begin
  if ProgressMax <> 0 then
    Log(Format('  %d of %d bytes done.', [Progress, ProgressMax]))
  else
    Log(Format('  %d bytes done.', [Progress]));
  
  DownloadFilenameLabel.Caption := Url;
  DownloadFilenameLabel.Update;

  if ProgressMax <> 0 then begin
    DownloadProgressBar.Style := npbstNormal;
    DownloadProgressBar.Max := ProgressMax;
    DownloadProgressBar.Position := Progress;
  end else
    DownloadProgressBar.Style := npbstMarquee;
  DownloadProgressBar.Update;

  Result := True;
end;

procedure DownloadFiles;
begin
  try
    DownloadStatusLabel.Visible := True;
    DownloadStatusLabel.Caption := 'Downloading additional files...';
    DownloadStatusLabel.Update;
    DownloadFilenameLabel.Visible := True;
    DownloadProgressBar.Visible := True;
    
    DownloadTemporaryFile('https://jrsoftware.org/download.php/is.exe', 'innosetup-latest.exe', '', @OnDownloadProgress);
    DownloadTemporaryFile('https://jrsoftware.org/download.php/iscrypt.dll', 'ISCrypt.dll', '2f6294f9aa09f59a574b5dcd33be54e16b39377984f3d5658cda44950fa0f8fc', @OnDownloadProgress);
  finally
    DownloadStatusLabel.Visible := False;
    DownloadFilenameLabel.Visible := False;
    DownloadProgressBar.Visible := False;
  end;
end;

function PrepareToInstall(var NeedsRestart: Boolean): String;
begin
  try
    DownloadFiles;
    Result := '';
  except
    Result := 'Failed to download files: ' + GetExceptionMessage;
  end;
end;
