; -- CodeDownloadFiles2.iss --
; Same as CodeDownloadFiles1.iss but additionally downloads a 7-Zip archive and
; shows how to verify and extract it.

[Setup]
AppName=My Program
AppVersion=1.5
WizardStyle=modern
DefaultDirName={autopf}\My Program
DefaultGroupName=My Program
UninstallDisplayIcon={app}\MyProg.exe
OutputDir=userdocs:Inno Setup Examples Output
;Use "ArchiveExtraction=enhanced/nopassword" if your archive has large files
;Use "ArchiveExtraction=enhanced" if your archive has large files *and* a password
;Use "ArchiveExtraction=full" if your archive is not a .7z file but for example a .zip file
ArchiveExtraction=basic

[ISSigKeys]
Name: "mykey"; \
  KeyID:   "def020edee3c4835fd54d85eff8b66d4d899b22a777353ca4a114b652e5e7a28"; \
  PublicX: "515dc7d6c16d4a46272ceb3d158c5630a96466ab4d948e72c2029d737c823097"; \
  PublicY: "f3c21f6b5156c52a35f6f28016ee3e31a3ded60c325b81fb7b1f88c221081a61"

[Files]
; Place any regular files here
Source: "MyProg.exe"; DestDir: "{app}";
Source: "MyProg.chm"; DestDir: "{app}";
Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme;
; These files will be downloaded
Source: "{tmp}\innosetup-latest.exe"; DestDir: "{app}"; Flags: external ignoreversion issigverify
Source: "{tmp}\ISCrypt.dll"; DestDir: "{app}"; Flags: external ignoreversion
Source: "{tmp}\MyProg-ExtraReadmes\*"; Excludes: "*.issig"; DestDir: "{app}"; Flags: external recursesubdirs ignoreversion issigverify

[Icons]
Name: "{group}\My Program"; Filename: "{app}\MyProg.exe"

[Code]
var
  DownloadPage: TDownloadWizardPage;
  ExtractionPage: TExtractionWizardPage;

procedure InitializeWizard;
begin
  DownloadPage := CreateDownloadPage(SetupMessage(msgWizardPreparing), SetupMessage(msgPreparingDesc), nil);
  DownloadPage.ShowBaseNameInsteadOfUrl := True;
  ExtractionPage := CreateExtractionPage(SetupMessage(msgWizardPreparing), SetupMessage(msgPreparingDesc), nil);
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  if CurPageID = wpReady then begin
    DownloadPage.Clear;
    // Use AddEx to specify a username and password
    DownloadPage.Add('https://jrsoftware.org/download.php/is.exe?dontcount=1', 'innosetup-latest.exe', '');
    DownloadPage.Add('https://jrsoftware.org/download.php/is.exe.issig?dontcount=1', 'innosetup-latest.exe.issig', '');
    DownloadPage.Add('https://jrsoftware.org/download.php/iscrypt.dll?dontcount=1', 'ISCrypt.dll', '2f6294f9aa09f59a574b5dcd33be54e16b39377984f3d5658cda44950fa0f8fc');
    DownloadPage.Add('https://jrsoftware.org/download.php/myprog-extrareadmes.7z?dontcount=1', 'MyProg-ExtraReadmes.7z', '');
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

    if not Result then
      Exit;

    ExtractionPage.Clear;
    // Use AddEx to specify a password
    ExtractionPage.Add(ExpandConstant('{tmp}\MyProg-ExtraReadmes.7z'), ExpandConstant('{tmp}\MyProg-ExtraReadmes'), True);
    ExtractionPage.Show;
    try
      try
        // Extracts the archive to {tmp}\MyProg-ExtraReadmes
        // Note that each file in the MyProg-ExtraReadmes.7z example archive comes with an .issig signature file
        // These signature files are used by the [Files] section to verify the archive's content
        ExtractionPage.Extract;
        Result := True;
      except
        if ExtractionPage.AbortedByUser then
          Log('Aborted by user.')
        else
          SuppressibleMsgBox(AddPeriod(GetExceptionMessage), mbCriticalError, MB_OK, IDOK);
        Result := False;
      end;
    finally
      ExtractionPage.Hide;
    end;
  end else
    Result := True;
end;