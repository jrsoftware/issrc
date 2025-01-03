unit Setup.InstFunc.Ole;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  OLE-related installation functions
}

interface

function CreateShellLink(const Filename, Description, ShortcutTo, Parameters,
  WorkingDir: String; IconFilename: String; const IconIndex, ShowCmd: Integer;
  const HotKey: Word; const AppUserModelID: String;
  const AppUserModelToastActivatorCLSID: PGUID;
  const ExcludeFromShowInNewInstall, PreventPinning: Boolean): String;
procedure RegisterTypeLibrary(const Filename: String);
procedure UnregisterTypeLibrary(const Filename: String);
function UnpinShellLink(const Filename: String): Boolean;

implementation

uses
  Windows, SysUtils, PathFunc, Shared.CommonFunc, Setup.InstFunc, Setup.MainFunc,
  SetupLdrAndSetup.Messages, Shared.SetupMessageIDs,
  ActiveX, ComObj, PropSys, ShellAPI, ShlObj;

procedure AssignWorkingDir(const SL: IShellLink; const WorkingDir: String);
{ Assigns the specified working directory to SL. If WorkingDir is empty then
  we select one ourself as best we can. (Leaving the working directory field
  empty is a security risk.) Note: SL.SetPath must be called first. }
var
  Dir: String;
  Buf: array[0..1023] of Char;
begin
  { Try any caller-supplied WorkingDir first }
  if WorkingDir <> '' then
    { SetWorkingDirectory *shouldn't* fail, but we might as well check }
    if SL.SetWorkingDirectory(PChar(WorkingDir)) = S_OK then
      Exit;

  { Otherwise, try to extract a directory name from the shortcut's target
    filename. We use GetPath to retrieve the filename as it will expand any
    environment strings. }
  if SL.GetPath(Buf, SizeOf(Buf) div SizeOf(Buf[0]), TWin32FindData(nil^), 0) = S_OK then begin
    Dir := PathExtractDir(PathExpand(Buf));
    if SL.SetWorkingDirectory(PChar(Dir)) = S_OK then
      Exit;
  end;

  { As a last resort, use the system directory }
  Dir := GetSystemDir;
  SL.SetWorkingDirectory(PChar(Dir));
end;

function GetResultingFilename(const PF: IPersistFile;
  const OriginalFilename: String): String;
{ Determines the actual resulting filename. IPersistFile::Save doesn't always
  save to the specified filename; it may rename the extension to .pif if the
  shortcut points to an MS-DOS application. }
var
  CurFilename: PWideChar;
  OleResult: HRESULT;
begin
  Result := '';
  CurFilename := nil;
  OleResult := PF.GetCurFile(CurFilename);
  if SUCCEEDED(OleResult) and Assigned(CurFilename) then begin
    if OleResult = S_OK then
      Result := WideCharToString(CurFilename);
    CoTaskMemFree(CurFilename);
  end;
  { If GetCurFile didn't work, we have no choice but to try to guess the filename }
  if Result = '' then begin
    if NewFileExists(OriginalFilename) then
      Result := OriginalFilename
    else if NewFileExists(PathChangeExt(OriginalFilename, '.pif')) then
      Result := PathChangeExt(OriginalFilename, '.pif')
    else begin
      { Neither exist? Shouldn't happen, but return something anyway }
      Result := OriginalFilename;
    end;
  end;
end;

function CreateShellLink(const Filename, Description, ShortcutTo, Parameters,
  WorkingDir: String; IconFilename: String; const IconIndex, ShowCmd: Integer;
  const HotKey: Word; const AppUserModelID: String;
  const AppUserModelToastActivatorCLSID: PGUID;
  const ExcludeFromShowInNewInstall, PreventPinning: Boolean): String;
{ Creates a lnk file named Filename, with a description of Description, with a
  HotKey hotkey, which points to ShortcutTo. Filename should be a full path.
  NOTE! If you want to copy this procedure for use in your own application
  be sure to call CoInitialize at application startup and CoUninitialize at
  application shutdown. See the bottom of this unit for an example. But this
  is not necessary if you are using Delphi 3 and your project already 'uses'
  the ComObj RTL unit. }
const
  PKEY_AppUserModel_ID: TPropertyKey = (
    fmtid: (D1:$9F4C2855; D2:$9F79; D3:$4B39; D4:($A8,$D0,$E1,$D4,$2D,$E1,$D5,$F3));
    pid: 5);
  PKEY_AppUserModel_ExcludeFromShowInNewInstall: TPropertyKey = (
    fmtid: (D1:$9F4C2855; D2:$9F79; D3:$4B39; D4:($A8,$D0,$E1,$D4,$2D,$E1,$D5,$F3));
    pid: 8);
  PKEY_AppUserModel_PreventPinning: TPropertyKey = (
    fmtid: (D1:$9F4C2855; D2:$9F79; D3:$4B39; D4:($A8,$D0,$E1,$D4,$2D,$E1,$D5,$F3));
    pid: 9);
  PKEY_AppUserModel_StartPinOption: TPropertyKey = (
    fmtid: (D1:$9F4C2855; D2:$9F79; D3:$4B39; D4:($A8,$D0,$E1,$D4,$2D,$E1,$D5,$F3));
    pid: 12);
  PKEY_AppUserModel_ToastActivatorCLSID: TPropertyKey = (
    fmtid: (D1:$9F4C2855; D2:$9F79; D3:$4B39; D4:($A8,$D0,$E1,$D4,$2D,$E1,$D5,$F3));
    pid: 26);
  APPUSERMODEL_STARTPINOPTION_NOPINONINSTALL = 1;

var
  OleResult: HRESULT;
  Obj: IUnknown;
  SL: IShellLink;
  PS: PropSys.IPropertyStore;
  PV: TPropVariant;
  PF: IPersistFile;
  WideAppUserModelID, WideFilename: WideString;
begin
  Obj := CreateComObject(CLSID_ShellLink);
  SL := Obj as IShellLink;
  SL.SetPath(PChar(ShortcutTo));
  SL.SetArguments(PChar(Parameters));
  AssignWorkingDir(SL, WorkingDir);
  if IconFilename <> '' then begin
    { Work around a 64-bit Windows bug. It replaces pf32 with %ProgramFiles%
      which is wrong. This causes an error when the user tries to change the
      icon of the installed shortcut. Note that the icon does actually display
      fine because it *also* stores the original 'non replaced' path in the
      shortcut. } 
    if IsWin64 then
      StringChangeEx(IconFileName, ExpandConst('{pf32}\'), '%ProgramFiles(x86)%\', True);
    SL.SetIconLocation(PChar(IconFilename), IconIndex);
  end;
  SL.SetShowCmd(ShowCmd);
  if Description <> '' then
    SL.SetDescription(PChar(Description));
  if HotKey <> 0 then
    SL.SetHotKey(HotKey);

  if (AppUserModelID <> '') or (AppUserModelToastActivatorCLSID <> nil) or ExcludeFromShowInNewInstall or PreventPinning then begin
    PS := Obj as PropSys.IPropertyStore;
    { According to MSDN the PreventPinning property should be set before the ID property. In practice
      this doesn't seem to matter - at least not for shortcuts - but do it first anyway. }
    if PreventPinning then begin
      PV.vt := VT_BOOL;
      PV.boolVal := True;
      OleResult := PS.SetValue(PKEY_AppUserModel_PreventPinning, PV);
      if OleResult <> S_OK then
        RaiseOleError('IPropertyStore::SetValue(PKEY_AppUserModel_PreventPinning)', OleResult);
    end;
    if AppUserModelID <> '' then begin
      WideAppUserModelID := AppUserModelID;
      PV.vt := VT_BSTR;
      PV.bstrVal := PWideChar(WideAppUserModelID);
      OleResult := PS.SetValue(PKEY_AppUserModel_ID, PV);
      if OleResult <> S_OK then
        RaiseOleError('IPropertyStore::SetValue(PKEY_AppUserModel_ID)', OleResult);
    end;
    if IsWindows10 and (AppUserModelToastActivatorCLSID <> nil) then begin
      PV.vt := VT_CLSID;
      PV.puuid := AppUserModelToastActivatorCLSID;
      OleResult := PS.SetValue(PKEY_AppUserModel_ToastActivatorCLSID, PV);
      if OleResult <> S_OK then
        RaiseOleError('IPropertyStore::SetValue(PKEY_AppUserModel_ToastActivatorCLSID)', OleResult);
    end;
    if ExcludeFromShowInNewInstall then begin
      PV.vt := VT_BOOL;
      PV.boolVal := True;
      OleResult := PS.SetValue(PKEY_AppUserModel_ExcludeFromShowInNewInstall, PV);
      if OleResult <> S_OK then
        RaiseOleError('IPropertyStore::SetValue(PKEY_AppUserModel_ExcludeFromShowInNewInstall)', OleResult);
      if IsWindows8 then begin
        PV.vt := VT_UI4;
        PV.ulVal := APPUSERMODEL_STARTPINOPTION_NOPINONINSTALL;
        OleResult := PS.SetValue(PKEY_AppUserModel_StartPinOption, PV);
        if OleResult <> S_OK then
          RaiseOleError('IPropertyStore::SetValue(PKEY_AppUserModel_StartPinOption)', OleResult);
      end;
    end;
    OleResult := PS.Commit;
    if OleResult <> S_OK then
      RaiseOleError('IPropertyStore::Commit', OleResult);
  end;

  PF := SL as IPersistFile;
  WideFilename := Filename;
  OleResult := PF.Save(PWideChar(WideFilename), True);
  if OleResult <> S_OK then
    RaiseOleError('IPersistFile::Save', OleResult);

  Result := GetResultingFilename(PF, Filename);
end;

procedure RegisterTypeLibrary(const Filename: String);
var
  WideFilename: WideString;
  OleResult: HRESULT;
  TypeLib: ITypeLib;
begin
  WideFilename := PathExpand(Filename);
  OleResult := LoadTypeLib(PWideChar(WideFilename), TypeLib);
  if OleResult <> S_OK then
    RaiseOleError('LoadTypeLib', OleResult);
  OleResult := RegisterTypeLib(TypeLib, PWideChar(WideFilename), nil);
  if OleResult <> S_OK then
    RaiseOleError('RegisterTypeLib', OleResult);
end;

procedure UnregisterTypeLibrary(const Filename: String);
type
  TUnRegTlbProc = function(const libID: TGUID; wVerMajor, wVerMinor: Word;
    lcid: TLCID; syskind: TSysKind): HResult; stdcall;
var
  UnRegTlbProc: TUnRegTlbProc;
  WideFilename: WideString;
  OleResult: HRESULT;
  TypeLib: ITypeLib;
  LibAttr: PTLibAttr;
begin
  { Dynamically import UnRegisterTypeLib since older OLEAUT32.DLL versions
    don't have this function }
  @UnRegTlbProc := GetProcAddress(GetModuleHandle('OLEAUT32.DLL'),
    'UnRegisterTypeLib');
  if @UnRegTlbProc = nil then
    Win32ErrorMsg('GetProcAddress');
  WideFilename := PathExpand(Filename);
  OleResult := LoadTypeLib(PWideChar(WideFilename), TypeLib);
  if OleResult <> S_OK then
    RaiseOleError('LoadTypeLib', OleResult);
  OleResult := TypeLib.GetLibAttr(LibAttr);
  if OleResult <> S_OK then
    RaiseOleError('ITypeLib::GetLibAttr', OleResult);
  try
    with LibAttr^ do
      OleResult := UnRegTlbProc(Guid, wMajorVerNum, wMinorVerNum, LCID, SysKind);
    if OleResult <> S_OK then
      RaiseOleError('UnRegisterTypeLib', OleResult);
  finally
    TypeLib.ReleaseTLibAttr(LibAttr);
  end;
end;

const
  CLSID_StartMenuPin: TGUID = (
    D1:$a2a9545d; D2:$a0c2; D3:$42b4; D4:($97,$08,$a0,$b2,$ba,$dd,$77,$c8));

  IID_StartMenuPinnedList: TGUID = (
    D1:$4CD19ADA; D2:$25A5; D3:$4A32; D4:($B3,$B7,$34,$7B,$EE,$5B,$E3,$6B));

  IID_ShellItem: TGUID = (
    D1:$43826D1E; D2:$E718; D3:$42EE; D4:($BC,$55,$A1,$E2,$61,$C3,$7B,$FE));

type
  IStartMenuPinnedList = interface(IUnknown)
    ['{4CD19ADA-25A5-4A32-B3B7-347BEE5BE36B}']
    function RemoveFromList(const pitem: IShellItem): HRESULT; stdcall;
  end;

var
  SHCreateItemFromParsingNameFunc: function(pszPath: LPCWSTR; const pbc: IBindCtx;
    const riid: TIID; var ppv): HResult; stdcall;

{ Attempt to unpin a shortcut. Returns True if the shortcut was successfully
  removed from the list of pinned items and/or the taskbar, or if the shortcut
  was not pinned at all. http://msdn.microsoft.com/en-us/library/bb774817.aspx }
function UnpinShellLink(const Filename: String): Boolean;
var
  WideFileName: WideString;
  ShellItem: IShellItem;
  StartMenuPinnedList: IStartMenuPinnedList;
begin
  WideFilename := PathExpand(Filename);
  if Assigned(SHCreateItemFromParsingNameFunc) and
     SUCCEEDED(SHCreateItemFromParsingNameFunc(PWideChar(WideFilename), nil, IID_ShellItem, ShellItem)) and
     SUCCEEDED(CoCreateInstance(CLSID_StartMenuPin, nil, CLSCTX_INPROC_SERVER, IID_StartMenuPinnedList, StartMenuPinnedList)) then
    Result := StartMenuPinnedList.RemoveFromList(ShellItem) = S_OK
  else
    Result := True;
end;

procedure InitOle;
var
  OleResult: HRESULT;
begin
  OleResult := CoInitialize(nil);
  if FAILED(OleResult) then
    raise Exception.CreateFmt('CoInitialize failed (0x%.8x)', [OleResult]);
    { ^ doesn't use a SetupMessage since messages probably aren't loaded
      during 'initialization' section below, which calls this procedure }
end;

initialization
  InitOle;
  SHCreateItemFromParsingNameFunc := GetProcAddress(SafeLoadLibrary(AddBackslash(GetSystemDir) + shell32,
    SEM_NOOPENFILEERRORBOX), 'SHCreateItemFromParsingName');

finalization
  CoUninitialize;
end.
