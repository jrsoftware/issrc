unit BrowseFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Functions for browsing for folders/files
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms;

function BrowseForFolder(const Prompt: String; var Directory: String;
  const ParentWnd: HWND; const NewFolderButton: Boolean): Boolean;
function NewGetOpenFileName(const Prompt: String; var FileName: String;
  const InitialDirectory, Filter, DefaultExtension: String;
  const ParentWnd: HWND): Boolean;
function NewGetOpenFileNameMulti(const Prompt: String; const FileNameList: TStrings;
  const InitialDirectory, Filter, DefaultExtension: String;
  const ParentWnd: HWND): Boolean;
function NewGetSaveFileName(const Prompt: String; var FileName: String;
  const InitialDirectory, Filter, DefaultExtension: String;
  const ParentWnd: HWND): Boolean;

implementation

uses
  CommDlg, ShlObj, ActiveX,
  PathFunc;

function BrowseCallback(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;
var
  ShouldEnable: Boolean;
  Path: array[0..MAX_PATH-1] of Char;
begin
  case uMsg of
    BFFM_INITIALIZED:
      begin
        if lpData <> 0 then
          SendMessage(Wnd, BFFM_SETSELECTION, 1, lpData);
      end;
    BFFM_SELCHANGED:
      begin
        { In a BIF_NEWDIALOGSTYLE dialog, BIF_RETURNONLYFSDIRS does not cause
          the OK button to be disabled automatically when the user clicks a
          non-FS item (e.g. My Computer), so do that ourself. }
        ShouldEnable := SHGetPathFromIDList(PItemIDList(lParam), Path);
        SendMessage(Wnd, BFFM_ENABLEOK, 0, Ord(ShouldEnable));
      end;
  end;
  Result := 0;
end;

function BrowseForFolder(const Prompt: String; var Directory: String;
  const ParentWnd: HWND; const NewFolderButton: Boolean): Boolean;
const
  BIF_NONEWFOLDERBUTTON = $200;
  BIF_NEWDIALOGSTYLE = $0040;
var
  InitialDir: String;
  Malloc: IMalloc;
  BrowseInfo: TBrowseInfo;
  DisplayName, Path: array[0..MAX_PATH-1] of Char;
  ActiveWindow: HWND;
  WindowList: Pointer;
  IDList: PItemIDList;
begin
  Result := False;
  InitialDir := RemoveBackslashUnlessRoot(Directory);  { Win95 doesn't allow trailing backslash }
  if FAILED(SHGetMalloc(Malloc)) then
    Malloc := nil;
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  with BrowseInfo do begin
    hwndOwner := ParentWnd;
    pszDisplayName := @DisplayName;
    lpszTitle := PChar(Prompt);
    ulFlags := BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE;
    if not NewFolderButton then
      ulFlags := ulFlags or BIF_NONEWFOLDERBUTTON;
    lpfn := BrowseCallback;
    if InitialDir <> '' then
      Pointer(lParam) := PChar(InitialDir);
  end;
  ActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(ParentWnd);
  CoInitialize(nil);
  try
    IDList := SHBrowseForFolder(BrowseInfo);
  finally
    CoUninitialize();
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
  end;
  try
    if (IDList = nil) or not SHGetPathFromIDList(IDList, Path) then
      Exit;
    Directory := Path;
  finally
    if Assigned(Malloc) then
      Malloc.Free(IDList);
  end;
  Result := True;
end;

type
  TGetOpenOrSaveFileNameFunc = function(var OpenFile: TOpenFilename): Bool; stdcall;

function NewGetOpenOrSaveFileName(const Prompt: String; var FileName: String;
  const InitialDirectory, Filter, DefaultExtension: String;
  const ParentWnd: HWND; const GetOpenOrSaveFileNameFunc: TGetOpenOrSaveFileNameFunc;
  const Flags: DWORD): Boolean;

  function AllocFilterStr(const S: string): string;
  var
    P: PChar;
  begin
    Result := '';
    if S <> '' then
    begin
      Result := S + #0;  // double null terminators
      P := PathStrScan(PChar(Result), '|');
      while P <> nil do
      begin
        P^ := #0;
        Inc(P);
        P := PathStrScan(P, '|');
      end;
    end;
  end;

  function GetMultiSelectString(const P: PChar): String;
  var
    E: PChar;
  begin
    E := P;
    while E^ <> #0 do
      Inc(E, StrLen(E) + 1);
    SetString(Result, P, E - P);
  end;

var
  ofn: TOpenFileName;
  lpstrFile: array[0..8191] of Char;
  TempFilter: String;
  SaveCurDir: String;
  ActiveWindow: HWND;
  WindowList: Pointer;
  FPUControlWord: Word;
begin
  StrPLCopy(lpstrFile, FileName, (SizeOf(lpstrFile) div SizeOf(lpstrFile[0])) - 1);

  FillChar(ofn, SizeOf(ofn), 0);
  ofn.lStructSize := SizeOf(ofn);
  ofn.hwndOwner := ParentWnd;
  TempFilter := AllocFilterStr(Filter);
  ofn.lpstrFilter := PChar(TempFilter);
  ofn.lpstrFile := lpstrFile;
  ofn.nMaxFile := SizeOf(lpstrFile) div SizeOf(lpstrFile[0]);
  ofn.lpstrInitialDir := PChar(InitialDirectory);
  ofn.lpstrTitle := PChar(Prompt);
  ofn.Flags := Flags or OFN_NOCHANGEDIR;
  ofn.lpstrDefExt := Pointer(DefaultExtension);

  ActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(ParentWnd);
  try
    asm
      // Avoid FPU control word change in NETRAP.dll, NETAPI32.dll, etc
      FNSTCW  FPUControlWord
    end;
    try
      SaveCurDir := GetCurrentDir;
      if GetOpenOrSaveFileNameFunc(ofn) then begin
        if Flags and OFN_ALLOWMULTISELECT <> 0 then
          FileName := GetMultiSelectString(lpstrFile)
        else
          FileName := lpstrFile;
        Result := True;
      end else
        Result := False;
      { Restore current directory. The OFN_NOCHANGEDIR flag we pass is
        supposed to do that, but the MSDN docs claim: "This flag is
        ineffective for GetOpenFileName." I see no such problem on
        Windows 2000 or XP, but to be safe... }
      SetCurrentDir(SaveCurDir);
    finally
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
    end;
  finally
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
  end;
end;

function NewGetOpenFileName(const Prompt: String; var FileName: String;
  const InitialDirectory, Filter, DefaultExtension: String;
  const ParentWnd: HWND): Boolean;
begin
  Result := NewGetOpenOrSaveFileName(Prompt, FileName, InitialDirectory, Filter, DefaultExtension,
    ParentWnd, GetOpenFileName, OFN_HIDEREADONLY or OFN_PATHMUSTEXIST or OFN_FILEMUSTEXIST);
end;

function NewGetOpenFileNameMulti(const Prompt: String; const FileNameList: TStrings;
  const InitialDirectory, Filter, DefaultExtension: String;
  const ParentWnd: HWND): Boolean;

  function ExtractStr(var P: PChar): String;
  var
    L: Integer;
  begin
    L := StrLen(P);
    SetString(Result, P, L);
    if L > 0 then
      Inc(P, L + 1);
  end;

var
  Files, Dir, FileName: String;
  P: PChar;
begin
  Result := NewGetOpenOrSaveFileName(Prompt, Files, InitialDirectory, Filter, DefaultExtension,
    ParentWnd, GetOpenFileName, OFN_HIDEREADONLY or OFN_PATHMUSTEXIST or OFN_FILEMUSTEXIST or
    OFN_ALLOWMULTISELECT or OFN_EXPLORER);
  if Result then begin
    FileNameList.Clear;
    P := PChar(Files);
    Dir := ExtractStr(P);
    FileName := ExtractStr(P);
    if FileName = '' then begin
      { When only one file is selected, the list contains just a file name }
      FileNameList.Add(Dir);
    end
    else begin
      repeat
        { The filenames can include paths if the user typed them in manually }
        FileNameList.Add(PathCombine(Dir, FileName));
        FileName := ExtractStr(P);
      until FileName = '';
    end;
  end;
end;

function NewGetSaveFileName(const Prompt: String; var FileName: String;
  const InitialDirectory, Filter, DefaultExtension: String;
  const ParentWnd: HWND): Boolean;
begin
  Result := NewGetOpenOrSaveFileName(Prompt, FileName, InitialDirectory, Filter, DefaultExtension,
    ParentWnd, GetSaveFileName, OFN_OVERWRITEPROMPT or OFN_HIDEREADONLY or OFN_PATHMUSTEXIST);
end;

end.
