; -- ISPPExample1.iss --
;
; This script shows various basic things you can achieve using Inno Setup Preprocessor (ISPP).
; To enable commented #define's, either remove the ';' or use ISCC with the /D switch.

#pragma option -v+
#pragma verboselevel 9

;#define Debug

;#define AppEnterprise

#ifdef AppEnterprise
  #define AppName "My Program Enterprise Edition"
#else
  #define AppName "My Program"
#endif

#define AppVersion GetFileVersion(AddBackslash(SourcePath) + "MyProg.exe")

[Setup]
AppName={#AppName}
AppVersion={#AppVersion}
DefaultDirName={pf}\{#AppName}
DefaultGroupName={#AppName}
UninstallDisplayIcon={app}\MyProg.exe
LicenseFile={#file AddBackslash(SourcePath) + "ISPPExample1License.txt"}
VersionInfoVersion={#AppVersion}
OutputDir=userdocs:Inno Setup Examples Output

[Files]
Source: "MyProg.exe"; DestDir: "{app}"
#ifdef AppEnterprise
Source: "MyProg.chm"; DestDir: "{app}"
#endif
Source: "Readme.txt"; DestDir: "{app}"; \
  Flags: isreadme

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\MyProg.exe"

#ifdef Debug
  #expr SaveToFile(AddBackslash(SourcePath) + "Preprocessed.iss"), \
        Exec(AddBackslash(CompilerPath) + "Compil32.exe", """" + AddBackslash(SourcePath) + "Preprocessed.iss""")
#endif
