unit SafeDLLPath;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  To provide protection against "DLL preloading" attacks, this unit calls
  SetDllDirectory('') to prevent LoadLibrary from searching the current
  directory for DLLs. (Has no effect on Windows versions prior to XP SP1.)

  It also calls SetSearchPathMode to enable "safe search mode", which causes
  SearchPath, and callers of SearchPath such as CreateProcess, to search the
  current directory after the system directories (rather than before).
  SetSearchPathMode is available in Windows 7, and on previous versions that
  have the KB959426 update installed.

  Finally, it calls SetProcessDEPPolicy (where available) to enable DEP for
  the lifetime of the process. (This has nothing to do with search paths;
  it's just convenient to put the call here.)

  This unit should be listed at the top of the program's "uses" clause to
  ensure that it runs prior to any LoadLibrary calls that other units might
  make during their initialization. (The System unit will always initialize
  first, though.)

  $jrsoftware: issrc/Projects/SafeDLLPath.pas,v 1.3 2010/09/21 03:32:28 jr Exp $
}

interface

implementation

uses
  Windows;

const
  BASE_SEARCH_PATH_ENABLE_SAFE_SEARCHMODE = $00000001;
  BASE_SEARCH_PATH_PERMANENT              = $00008000;

  PROCESS_DEP_ENABLE = $00000001;

var
  KernelModule: HMODULE;
  SetDllDirectoryFunc: function(lpPathName: PWideChar): BOOL; stdcall;
  SetSearchPathModeFunc: function(Flags: DWORD): BOOL; stdcall;
  SetProcessDEPPolicyFunc: function(dwFlags: DWORD): BOOL; stdcall;

initialization
  KernelModule := GetModuleHandle(kernel32);

  SetDllDirectoryFunc := GetProcAddress(KernelModule, PAnsiChar('SetDllDirectoryW'));
  if Assigned(SetDllDirectoryFunc) then
    SetDllDirectoryFunc('');

  SetSearchPathModeFunc := GetProcAddress(KernelModule, PAnsiChar('SetSearchPathMode'));
  if Assigned(SetSearchPathModeFunc) then
    SetSearchPathModeFunc(BASE_SEARCH_PATH_ENABLE_SAFE_SEARCHMODE or
      BASE_SEARCH_PATH_PERMANENT);

  SetProcessDEPPolicyFunc := GetProcAddress(KernelModule, PAnsiChar('SetProcessDEPPolicy'));
  if Assigned(SetProcessDEPPolicyFunc) then
    SetProcessDEPPolicyFunc(PROCESS_DEP_ENABLE);
end.
