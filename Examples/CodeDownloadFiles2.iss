; -- CodeDownloadFiles2.iss --
;
; This script shows how the CreateDownloadPage support function can be used to
; download files and archives while showing the download and extraction
; progress to the user.
;
; To verify the downloaded files, this script shows two methods:
; -For innosetup-latest.exe and MyProg-ExtraReadmes.7z: using Inno Setup
;  Signature Tool, the [ISSigKeys] section, and the AddWithISSigVerify support
;  function
; -For iscrypt.dll: using a simple SHA256 check
; Using the Inno Setup Signature Tool has the benefit that the script does not
; need to be changed when the downloaded file changes, so any installers built
; will also keep working

[Setup]
AppName=My Program
AppVersion=1.5
WizardStyle=modern
DefaultDirName={autopf}\My Program
DefaultGroupName=My Program
UninstallDisplayIcon={app}\MyProg.exe
OutputDir=userdocs:Inno Setup Examples Output
;Use "ArchiveExtraction=enhanced" if your archive has a password
;Use "ArchiveExtraction=full" if your archive is not a .7z file but for example a .zip file
ArchiveExtraction=enhanced/nopassword

[ISSigKeys]
Name: "mykey"; RuntimeID: "def02"; \
  KeyID:   "def020edee3c4835fd54d85eff8b66d4d899b22a777353ca4a114b652e5e7a28"; \
  PublicX: "515dc7d6c16d4a46272ceb3d158c5630a96466ab4d948e72c2029d737c823097"; \
  PublicY: "f3c21f6b5156c52a35f6f28016ee3e31a3ded60c325b81fb7b1f88c221081a61"

[Files]
; Place any regular files here
Source: "MyProg.exe"; DestDir: "{app}"
Source: "MyProg.chm"; DestDir: "{app}"
Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme
; These files will be downloaded. If you include flag issigverify here the file will be verified
; a second time while copying. Verification while copying is efficient, except for archives.
Source: "{tmp}\innosetup-latest.exe"; DestDir: "{app}"; Flags: external ignoreversion issigverify
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
  AllowedKeysRuntimeIDs.Add('def02');
end;

procedure DeinitializeSetup;
begin
  if AllowedKeysRuntimeIDs <> nil then
    AllowedKeysRuntimeIDs.Free;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  if CurPageID = wpReady then begin
    DownloadPage.Clear;
    // Use AddEx or AddExWithISSigVerify to specify a username and password
    DownloadPage.AddWithISSigVerify(
      'https://jrsoftware.org/download.php/is.exe?dontcount=1', '',
      'innosetup-latest.exe', AllowedKeysRuntimeIDs);
    DownloadPage.AddWithISSigVerify(
      'https://jrsoftware.org/download.php/myprog-extrareadmes.7z', '',
      'MyProg-ExtraReadmes.7z', AllowedKeysRuntimeIDs);
    DownloadPage.Add(
      'https://jrsoftware.org/download.php/iscrypt.dll?dontcount=1',
      'ISCrypt.dll', '2f6294f9aa09f59a574b5dcd33be54e16b39377984f3d5658cda44950fa0f8fc');
    DownloadPage.Show;
    try
      try
        // Downloads the files to {tmp}
        DownloadPage.Download;
        Result := True;
      except
        if DownloadPage.AbortedByUser then
          Log('Aborted by user.')
        else
          SuppressibleMsgBox(AddPeriod(GetExceptionMessage), mbCriticalError, MB_OK, IDOK);
        Result := False;
      end;
    finally
      DownloadPage.Hide;
    end;
  end else
    Result := True;
end;