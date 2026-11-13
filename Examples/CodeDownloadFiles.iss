; -- CodeDownloadFiles.iss --
;
; SEE DOWNLOADFILES.ISS FIRST!
;
; This script shows how the CreateDownloadPage support function can be used to
; download and verify files while showing the download progress to the user.

[Setup]
AppName=My Program
AppVersion=1.5
WizardStyle=modern dynamic
DefaultDirName={autopf}\My Program
DefaultGroupName=My Program
UninstallDisplayIcon={app}\MyProg.exe
OutputDir=userdocs:Inno Setup Examples Output
ArchiveExtraction=auto

[ISSigKeys]
Name: mykey; RuntimeID: def01; \
  KeyID:   "def0147c3bbc17ab99bf7b7a9c2de1390283f38972152418d7c2a4a7d7131a38"; \
  PublicX: "e3e943066aff8f28d2219fd71c9ffff4c8d1aa26bc4225434be67180ab5e242d"; \
  PublicY: "e419041c3f54551e86a1c47f387005cd535dfc9d64339b30d37f9a4f7866b650"

[Files]
; Place any regular files here
Source: "MyProg.exe"; DestDir: "{app}"
Source: "MyProg.chm"; DestDir: "{app}"
Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme
; These files will be downloaded
Source: "{tmp}\ISSigTool-latest.exe"; DestDir: "{app}"; Flags: external ignoreversion
Source: "{tmp}\MyProg-ExtraReadmes.7z"; DestDir: "{app}"; Flags: external extractarchive recursesubdirs ignoreversion
Source: "{tmp}\ISCrypt.dll"; DestDir: "{app}"; Flags: external ignoreversion

[Icons]
Name: "{group}\My Program"; Filename: "{app}\MyProg.exe"

[Code]
var
  DownloadPage: TDownloadWizardPage;
  AllowedKeysRuntimeIDs: TStringList;

procedure InitializeWizard;
begin
  DownloadPage := CreateDownloadPage(SetupMessage(msgWizardPreparing), SetupMessage(msgPreparingDesc), nil);
  DownloadPage.ShowBaseNameInsteadOfUrl := True;
  
  // To allow all keys you can also just pass nil instead of this list to AddWithISSigVerify 
  AllowedKeysRuntimeIDs := TStringList.Create;
  AllowedKeysRuntimeIDs.Add('def01');
end;

procedure DeinitializeSetup;
begin
  if AllowedKeysRuntimeIDs <> nil then
    AllowedKeysRuntimeIDs.Free;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
var
  Error: String;
begin
  if CurPageID = wpReady then begin
    DownloadPage.Clear;
    // Use AddEx or AddExWithISSigVerify to specify a username and password
    DownloadPage.AddWithISSigVerify(
      'https://files.jrsoftware.org/is/misc/ISSigTool.exe', '',
      'ISSigTool-latest.exe', AllowedKeysRuntimeIDs);
    DownloadPage.AddWithISSigVerify(
      'https://files.jrsoftware.org/is/misc/myprog-extrareadmes-01.7z', '',
      'MyProg-ExtraReadmes.7z', AllowedKeysRuntimeIDs);
    DownloadPage.Add(
      'https://jrsoftware.org/download/iscrypt.dll',
      'ISCrypt.dll', '2f6294f9aa09f59a574b5dcd33be54e16b39377984f3d5658cda44950fa0f8fc');
    DownloadPage.Show;
    try
      try
        DownloadPage.Download; // This downloads the files to {tmp}
        Result := True;
      except
        if DownloadPage.AbortedByUser then
          Log('Aborted by user.')
        else begin
          Error := Format('%s: %s', [DownloadPage.LastBaseNameOrUrl, GetExceptionMessage]);
          SuppressibleMsgBox(AddPeriod(Error), mbCriticalError, MB_OK, IDOK);
        end;
        Result := False;
      end;
    finally
      DownloadPage.Hide;
    end;
  end else
    Result := True;
end;