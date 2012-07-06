; -- CodeDll.iss --
;
; This script shows how to call DLL functions at runtime from a [Code] section.

[Setup]
AppName=My Program
AppVersion=1.5
DefaultDirName={pf}\My Program
DisableProgramGroupPage=yes
UninstallDisplayIcon={app}\MyProg.exe
OutputDir=userdocs:Inno Setup Examples Output

[Files]
Source: "MyProg.exe"; DestDir: "{app}"
Source: "MyProg.chm"; DestDir: "{app}"
Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme
; Install our DLL to {app} so we can access it at uninstall time
; Use "Flags: dontcopy" if you don't need uninstall time access
Source: "MyDll.dll"; DestDir: "{app}"

[Code]
const
  MB_ICONINFORMATION = $40;

//importing an ANSI Windows API function
function MessageBox(hWnd: Integer; lpText, lpCaption: AnsiString; uType: Cardinal): Integer;
external 'MessageBoxA@user32.dll stdcall';

//importing a Windows API function, automatically choosing ANSI or Unicode (requires ISPP)
//function MessageBox(hWnd: Integer; lpText, lpCaption: String; uType: Cardinal): Integer;
//#ifdef UNICODE
//external 'MessageBoxW@user32.dll stdcall';
//#else
//external 'MessageBoxA@user32.dll stdcall';
//#endif

//importing an ANSI custom DLL function, first for Setup, then for uninstall
procedure MyDllFuncSetup(hWnd: Integer; lpText, lpCaption: AnsiString; uType: Cardinal);
external 'MyDllFunc@files:MyDll.dll stdcall setuponly';

procedure MyDllFuncUninstall(hWnd: Integer; lpText, lpCaption: AnsiString; uType: Cardinal);
external 'MyDllFunc@{app}\MyDll.dll stdcall uninstallonly';

//importing an ANSI function for a DLL which might not exist at runtime
procedure DelayLoadedFunc(hWnd: Integer; lpText, lpCaption: AnsiString; uType: Cardinal);
external 'DllFunc@DllWhichMightNotExist.dll stdcall delayload';

function NextButtonClick(CurPage: Integer): Boolean;
var
  hWnd: Integer;
begin
  if CurPage = wpWelcome then begin
    hWnd := StrToInt(ExpandConstant('{wizardhwnd}'));

    MessageBox(hWnd, 'Hello from Windows API function', 'MessageBoxA', MB_OK or MB_ICONINFORMATION);

    MyDllFuncSetup(hWnd, 'Hello from custom DLL function', 'MyDllFunc', MB_OK or MB_ICONINFORMATION);

    try
      //if this DLL does not exist (it shouldn't), an exception will be raised
      DelayLoadedFunc(hWnd, 'Hello from delay loaded function', 'DllFunc', MB_OK or MB_ICONINFORMATION);
    except
      //handle missing dll here
    end;
  end;
  Result := True;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  // Call our function just before the actual uninstall process begins
  if CurUninstallStep = usUninstall then
  begin
    MyDllFuncUninstall(0, 'Hello from custom DLL function', 'MyDllFunc', MB_OK or MB_ICONINFORMATION);
    
    // Now that we're finished with it, unload MyDll.dll from memory.
    // We have to do this so that the uninstaller will be able to remove the DLL and the {app} directory.
    UnloadDLL(ExpandConstant('{app}\MyDll.dll'));
  end;
end;
