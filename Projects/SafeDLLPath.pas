unit SafeDLLPath;

{
  Inno Setup
  Copyright (C) 1997-2016 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  To provide protection against "DLL preloading" attacks, this unit calls
  SetDefaultDllDirectories. SetDefaultDllDirectories is available on Windows 8
  and newer, and on previous versions that have the KB2533623 update installed.
  
  If SetDefaultDllDirectories is not available:
  -It calls SetDllDirectory('') to prevent LoadLibrary from searching the current
   directory for DLLs. (Has no effect on Windows versions prior to XP SP1.)
  -It then preloads a list of system DLLs which are known to be loaded unsafely
   by older or unpatched versions of Windows.

  Also see:
  -http://wixtoolset.org/development/wips/5184-burn-clean-room/
  -https://github.com/firegiant/wix3/blob/master/src/libs/dutil/apputil.cpp
  -https://github.com/firegiant/wix3/blob/master/src/burn/stub/stub.cpp
  -https://sourceforge.net/p/nsis/code/HEAD/tree/NSIS/trunk/Source/exehead/Main.c

  It also calls SetSearchPathMode to enable "safe search mode", which causes
  SearchPath, and callers of SearchPath such as CreateProcess, to search the
  current directory after the system directories (rather than before).
  SetSearchPathMode is available in Windows 7 and newer, and on previous
  versions that have the KB959426 update installed.

  Finally, it calls SetProcessDEPPolicy (where available) to enable DEP for
  the lifetime of the process. (This has nothing to do with search paths;
  it's just convenient to put the call here.)

  This unit should be listed at the top of the program's "uses" clause to
  ensure that it runs prior to any LoadLibrary calls that other units might
  make during their initialization. (The System unit will always initialize
  first, though.)
}

interface

implementation

uses
  Windows;

const
  LOAD_LIBRARY_SEARCH_SYSTEM32 = $00000800;

  BASE_SEARCH_PATH_ENABLE_SAFE_SEARCHMODE = $00000001;
  BASE_SEARCH_PATH_PERMANENT              = $00008000;

  PROCESS_DEP_ENABLE = $00000001;

var
  KernelModule: HMODULE;
  WinVer: WORD;
  SystemDir: String;
  SetDefaultDllDirectoriesFunc: function(DirectoryFlags: DWORD): BOOL; stdcall;
  DidSetDefaultDllDirectories: Boolean;
  SetDllDirectoryFunc: function(lpPathName: PWideChar): BOOL; stdcall;
  SetSearchPathModeFunc: function(Flags: DWORD): BOOL; stdcall;
  SetProcessDEPPolicyFunc: function(dwFlags: DWORD): BOOL; stdcall;
  
function StrPas(Str: PChar): string;
begin
  Result := Str;
end;

function GetSystemDir: String;
var
  Buf: array[0..MAX_PATH-1] of Char;
begin
  GetSystemDirectory(Buf, SizeOf(Buf) div SizeOf(Buf[0]));
  Result := StrPas(Buf);
end;

function SafeLoadLibrary(const Filename: String): HMODULE;
var
  SaveErrorMode: UINT;
  SaveFPUControlWord: Word;
begin
  SaveErrorMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    asm
      FNSTCW SaveFPUControlWord
    end;
    try
      Result := LoadLibrary(PChar(Filename));
    finally
      asm
        FNCLEX
        FLDCW SaveFPUControlWord
      end;
    end;
  finally
    SetErrorMode(SaveErrorMode);
  end;
end;

initialization
  KernelModule := GetModuleHandle(kernel32);
  WinVer := Swap(Word(GetVersion()));

  DidSetDefaultDllDirectories := False;
  if WinVer <> $0600 then begin //see NSIS link above: CoCreateInstance(CLSID_ShellLink, ...) fails on Vista if SetDefaultDllDirectories is called
    SetDefaultDllDirectoriesFunc := GetProcAddress(KernelModule, PAnsiChar('SetDefaultDllDirectories'));
    if Assigned(SetDefaultDllDirectoriesFunc) then
      DidSetDefaultDllDirectories := SetDefaultDllDirectoriesFunc(LOAD_LIBRARY_SEARCH_SYSTEM32);
  end;
    
  if not DidSetDefaultDllDirectories then begin
    SetDllDirectoryFunc := GetProcAddress(KernelModule, PAnsiChar('SetDllDirectoryW'));
    if Assigned(SetDllDirectoryFunc) then
      SetDllDirectoryFunc('');

    SystemDir := GetSystemDir;
    if SystemDir <> '' then begin
      if SystemDir[Length(SystemDir)] <> '\' then
        SystemDir := SystemDir + '\';
       //list of system dlls to preload including source:
      // NSIS: Vista: OleInitialize calls NtUserCreateWindowEx and that pulls in UXTheme.dll
      SafeLoadLibrary(SystemDir + 'uxtheme.dll');
      // NSIS: Vista: SHGetFileInfo ends up in SHELL32.kfapi::GetUserProfileDir and that pulls in UserEnv.dll
      SafeLoadLibrary(SystemDir + 'userenv.dll');
      // NSIS: XP: SHGetFileInfo ends up in CMountPoint::_InitLocalDriveHelper and that pulls in SetupAPI.dll
      SafeLoadLibrary(SystemDir + 'setupapi.dll');
      // NSIS: Vista: SHGetFileInfo ... SHELL32.SHILAliasTranslate ... SHELL32.ApphelpCheckShellObject;
      SafeLoadLibrary(SystemDir + 'apphelp.dll');
      // NSIS: Vista: SHGetFileInfo ... SHELL32.SHILAliasTranslate ... SHLWAPI.#187 ... SHLWAPI.#505/SHPropertyBag_ReadGUID
      SafeLoadLibrary(SystemDir + 'propsys.dll');
      // NSIS: Win7 without KB2533623: UXTheme pulls in DWMAPI.dll
      // Mail: Windows 7 SP1: combase.dll -> ole32.dll -> shell32.dll -> dwmapi.dll
      SafeLoadLibrary(SystemDir + 'dwmapi.dll');
      // NSIS: Win7 without KB2533623: OleInitialize ... RPCRT4.UuidCreate ... RPCRT4.GenerateRandomNumber
      // Mail: oleaut32.dll -> rpcrt4.dll -> cryptbase.dll
      SafeLoadLibrary(SystemDir + 'cryptbase.dll');
      // NSIS: Vista: SHFileOperation ... SHELL32.CProgressDialogUI::_Setup ... SHELL32.GetRoleTextW        
      SafeLoadLibrary(SystemDir + 'oleacc.dll');    
      // Mail: Windows 7 SP1: oleaut32.dll -> ole32.dll -> crypt32.dll -> version.dll
      // WIX3: required by Burn
      SafeLoadLibrary(SystemDir + 'version.dll');
      // Mail: Windows 7 SP1: oleaut32.dll -> ole32.dll -> crypt32.dll -> profapi.dll
      SafeLoadLibrary(SystemDir + 'profapi.dll');
      // WIX3: required by CLSIDFromProgID() when loading clbcatq.dll
      SafeLoadLibrary(SystemDir + 'comres.dll');
      // WIX3: required by CLSIDFromProgID() when loading msxml?.dll
      // NSIS: XP.SP2&SP3: SHAutoComplete ... OLE32!InitializeCatalogIfNecessary ... OLE32!CComCatalog::TryToLoadCLB
      SafeLoadLibrary(SystemDir + 'clbcatq.dll');
      // NSIS: Win7 without KB2533623: SHGetFileInfo ... SetEntriesInAcl ... ADVAPI32!AccProvpLoadMartaFunctions
      SafeLoadLibrary(SystemDir + 'ntmarta.dll');
{
      // WIX3: required by Burn
      SafeLoadLibrary(SystemDir + 'cabinet.dll');
      // WIX3: required by Burn
      SafeLoadLibrary(SystemDir + 'msi.dll');
      // WIX3: required by Burn
      SafeLoadLibrary(SystemDir + 'wininet.dll');
      // WIX3: required by DecryptFile() when loading crypt32.dll
      SafeLoadLibrary(SystemDir + 'msasn1.dll');
      // WIX3: required by DecryptFile() when loading feclient.dll
      SafeLoadLibrary(SystemDir + 'crypt32.dll');
      // WIX3: unsafely loaded by DecryptFile()
      SafeLoadLibrary(SystemDir + 'feclient.dll');
}      
    end;
  end;

  SetSearchPathModeFunc := GetProcAddress(KernelModule, PAnsiChar('SetSearchPathMode'));
  if Assigned(SetSearchPathModeFunc) then
    SetSearchPathModeFunc(BASE_SEARCH_PATH_ENABLE_SAFE_SEARCHMODE or
      BASE_SEARCH_PATH_PERMANENT);

  SetProcessDEPPolicyFunc := GetProcAddress(KernelModule, PAnsiChar('SetProcessDEPPolicy'));
  if Assigned(SetProcessDEPPolicyFunc) then
    SetProcessDEPPolicyFunc(PROCESS_DEP_ENABLE);
end.
