unit BrowseFunc;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Functions for browsing for folders/files
}

interface

uses
  Windows, Classes, Forms;

function BrowseForFolder(const Prompt: String; var Directory: String;
  const ParentWnd: HWND): Boolean;
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
  SysUtils, Themes,
  PathFunc;

function BrowseForFolder(const Prompt: String; var Directory: String;
  const ParentWnd: HWND): Boolean;

  function RemoveSinglePeriod(const S: String): String;
  begin
    Result := S;
    const FirstPeriodPos = Pos('.', S);
    if FirstPeriodPos <> 0 then begin
      const LastPeriodPos = LastDelimiter('.', S);
      if (FirstPeriodPos = LastPeriodPos) and (LastPeriodPos = Length(S)) then
        Delete(Result, LastPeriodPos, 1);
    end;
  end;

var
  InitialDir: String;
  Options: DWORD;
  ActiveWindow: HWND;
  WindowList: Pointer;
  ResultPath: PChar;
begin
  Result := False;
  InitialDir := RemoveBackslashUnlessRoot(Directory);  { Win95 doesn't allow trailing backslash }

  if Failed(CoInitialize(nil)) then
    Exit;
  ActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(ParentWnd);
  const SaveHooks = TStyleManager.SystemHooks;
  TStyleManager.SystemHooks := []; { See below }
  try
    var FileDialog: IFileDialog;
    if Failed(CoCreateInstance(CLSID_FileOpenDialog, nil, CLSCTX_INPROC_SERVER, IID_IFileDialog, FileDialog)) or
       Failed(FileDialog.GetOptions(Options)) then { On Windows 11 this returned FOS_NOCHANGEDIR or FOS_PATHMUSTEXIST or FOS_FILEMUSTEXIST }
      Exit;

    Options := Options or FOS_PICKFOLDERS or FOS_FORCEFILESYSTEM;
    if Failed(FileDialog.SetOptions(Options)) then
      Exit;

    if Prompt <> '' then
      FileDialog.SetTitle(PChar(RemoveSinglePeriod(Prompt)));

    var ShellItem: IShellItem;
    if (InitialDir <> '') and Succeeded(SHCreateItemFromParsingName(PChar(InitialDir), nil, IID_IShellItem, ShellItem)) then
      FileDialog.SetFolder(ShellItem);

    if Failed(FileDialog.Show(ParentWnd)) or
       Failed(FileDialog.GetResult(ShellItem)) or
       Failed(ShellItem.GetDisplayName(SIGDN_FILESYSPATH, ResultPath)) then
      Exit;

    try
      Directory := ResultPath;
    finally
      CoTaskMemFree(ResultPath);
    end;
  finally
    TStyleManager.SystemHooks := SaveHooks;
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
    CoUninitialize();
  end;
  Result := True;
end;

type
  TGetOpenOrSaveFileNameFunc = function(var OpenFile: TOpenFilename): Bool; stdcall;

function NewGetOpenOrSaveFileName(const Prompt: String; var FileName: String;
  const InitialDirectory, Filter, DefaultExtension: String;
  const ParentWnd: HWND; const GetOpenOrSaveFileNameFunc: TGetOpenOrSaveFileNameFunc;
  const FunctionName: String; const Flags: DWORD): Boolean;

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

  function ShowDialog(var ofn: TOpenFileName; out ExtendedError: DWORD): Boolean;
  begin
    Result := GetOpenOrSaveFileNameFunc(ofn);
    if Result then
      ExtendedError := 0
    else
      ExtendedError := CommDlgExtendedError; { 0 if the user cancelled }
  end;

var
  ofn: TOpenFileName;
  FileBuffer: array of Char;
  TempFilter: String;
  SaveCurDir: String;
  ActiveWindow: HWND;
  WindowList: Pointer;
begin
  const MultiSelect = Flags and OFN_ALLOWMULTISELECT <> 0;
  var MaxFile: Cardinal;
  if MultiSelect then
    MaxFile := High(Word) - 16 { Same as TOpenDialog }
  else
    MaxFile := 8192;
  SetLength(FileBuffer, MaxFile); { Zero-filled, which GetMultiSelectString needs after a single selection }
  StrPLCopy(@FileBuffer[0], FileName, MaxFile - 1);

  FillChar(ofn, SizeOf(ofn), 0);
  ofn.lStructSize := SizeOf(ofn);
  ofn.hwndOwner := ParentWnd;
  TempFilter := AllocFilterStr(Filter);
  ofn.lpstrFilter := PChar(TempFilter);
  ofn.lpstrFile := @FileBuffer[0];
  ofn.nMaxFile := MaxFile;
  ofn.lpstrInitialDir := PChar(InitialDirectory);
  ofn.lpstrTitle := PChar(Prompt);
  ofn.Flags := Flags or OFN_NOCHANGEDIR;
  ofn.lpstrDefExt := Pointer(DefaultExtension);

  ActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(ParentWnd);
  { Temporarily clear SystemHooks to make it support dark mode if Windows is in dark mode.
    Taken from Vcl.Dialogs' TCustomFileDialog.Execute. }
  const SaveHooks = TStyleManager.SystemHooks;
  TStyleManager.SystemHooks := [];
  try
    {$IFDEF CPUX86}
    var FPUControlWord: Word;
    asm
      // Avoid FPU control word change in NETRAP.dll, NETAPI32.dll, etc
      FNSTCW  FPUControlWord
    end;
    {$ENDIF}
    try
      SaveCurDir := GetCurrentDir;
      var ExtendedError: DWORD;
      Result := ShowDialog(ofn, ExtendedError);
      if (ExtendedError = FNERR_INVALIDFILENAME) and (ofn.lpstrFile^ <> #0) then begin
        { Windows rejects an invalid initial file name without showing the dialog }
        FillChar(FileBuffer[0], MaxFile * SizeOf(Char), 0); { See above, full fill for GetMultiSelectString }
        Result := ShowDialog(ofn, ExtendedError);
      end;
      if Result then begin
        if MultiSelect then
          FileName := GetMultiSelectString(ofn.lpstrFile)
        else
          FileName := ofn.lpstrFile;
      end;
      { Restore current directory. The OFN_NOCHANGEDIR flag we pass is
        supposed to do that, but the MSDN docs claim: "This flag is
        ineffective for GetOpenFileName." I see no such problem on
        Windows 2000 or XP, but to be safe... }
      SetCurrentDir(SaveCurDir);
      if ExtendedError <> 0 then
        raise Exception.CreateFmt('%s failed with error code 0x%x',
          [FunctionName, ExtendedError]);
    finally
      {$IFDEF CPUX86}
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
      {$ENDIF}
    end;
  finally
    TStyleManager.SystemHooks := SaveHooks;
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
  end;
end;

function NewGetOpenFileName(const Prompt: String; var FileName: String;
  const InitialDirectory, Filter, DefaultExtension: String;
  const ParentWnd: HWND): Boolean;
begin
  Result := NewGetOpenOrSaveFileName(Prompt, FileName, InitialDirectory, Filter, DefaultExtension,
    ParentWnd, GetOpenFileName, 'GetOpenFileName', OFN_HIDEREADONLY or OFN_PATHMUSTEXIST or OFN_FILEMUSTEXIST);
end;

function NewGetOpenFileNameMulti(const Prompt: String; const FileNameList: TStrings;
  const InitialDirectory, Filter, DefaultExtension: String;
  const ParentWnd: HWND): Boolean;

  function ExtractStr(var P: PChar): String;
  begin
    const L = Integer(StrLen(P));
    SetString(Result, P, L);
    if L > 0 then
      Inc(P, L + 1);
  end;

var
  Files, Dir, FileName: String;
  P: PChar;
begin
  Result := NewGetOpenOrSaveFileName(Prompt, Files, InitialDirectory, Filter, DefaultExtension,
    ParentWnd, GetOpenFileName, 'GetOpenFileName', OFN_HIDEREADONLY or OFN_PATHMUSTEXIST or OFN_FILEMUSTEXIST or
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
    ParentWnd, GetSaveFileName, 'GetSaveFileName', OFN_OVERWRITEPROMPT or OFN_HIDEREADONLY or OFN_PATHMUSTEXIST);
end;

end.
