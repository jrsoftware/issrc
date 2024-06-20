unit SafeDLLPath;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  To provide protection against "DLL preloading" attacks, this unit calls
  SetDefaultDllDirectories. SetDefaultDllDirectories is available on Windows 8
  and newer, and on previous versions that have the KB2533623 update installed
  which was released in July 2011.

  It also calls SetSearchPathMode to enable "safe search mode", which causes
  SearchPath, and callers of SearchPath such as CreateProcess, to search the
  current directory after the system directories (rather than before).

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

var
  KernelModule: HMODULE;
  SetDefaultDllDirectoriesFunc: function(DirectoryFlags: DWORD): BOOL; stdcall;
  SetSearchPathModeFunc: function(Flags: DWORD): BOOL; stdcall;
  SetProcessDEPPolicyFunc: function(dwFlags: DWORD): BOOL; stdcall;

initialization
  KernelModule := GetModuleHandle(kernel32);

  SetDefaultDllDirectoriesFunc := GetProcAddress(KernelModule, PAnsiChar('SetDefaultDllDirectories'));
  if Assigned(SetDefaultDllDirectoriesFunc) then
    SetDefaultDllDirectoriesFunc(LOAD_LIBRARY_SEARCH_SYSTEM32);

  SetSearchPathModeFunc := GetProcAddress(KernelModule, PAnsiChar('SetSearchPathMode'));
  if Assigned(SetSearchPathModeFunc) then
    SetSearchPathModeFunc(BASE_SEARCH_PATH_ENABLE_SAFE_SEARCHMODE or
      BASE_SEARCH_PATH_PERMANENT);

  SetProcessDEPPolicyFunc := GetProcAddress(KernelModule, PAnsiChar('SetProcessDEPPolicy'));
  if Assigned(SetProcessDEPPolicyFunc) then
    SetProcessDEPPolicyFunc(PROCESS_DEP_ENABLE);
end.
