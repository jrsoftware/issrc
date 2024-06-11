unit CompFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Additional Compiler IDE functions
}

interface

uses
  Windows,
  Classes, Forms, Dialogs, Menus, Controls, StdCtrls,
  ScintEdit, CompScintEdit, ModernColors;

const
  MRUListMaxCount = 10;

type
  TMRUItemCompareProc = function(const S1, S2: String): Integer;
  TAddLinesPrefix = (alpNone, alpTimestamp, alpCountdown);
  TKeyMappingType = (kmtDelphi, kmtVisualStudio);

procedure InitFormFont(Form: TForm);
procedure SetControlWindowTheme(const WinControl: TWinControl; const Dark: Boolean);
procedure InitFormThemeInit(const ATheme: TTheme);
procedure InitFormTheme(Form: TForm);
function GetDisplayFilename(const Filename: String): String;
function GetFileTitle(const Filename: String): String;
function GetCleanFileNameOfFile(const Filename: String): String;
function GetLastWriteTimeOfFile(const Filename: String;
  LastWriteTime: PFileTime): Boolean;
procedure AddFileToRecentDocs(const Filename: String);
function GenerateGuid: String;
function ISPPInstalled: Boolean;
function IsISPPBuiltins(const Filename: String): Boolean;
function ISCryptInstalled: Boolean;
function WindowsVersionAtLeast(const AMajor, AMinor: Byte; const ABuild: Word = 0): Boolean;
function IsWindows10: Boolean;
function IsWindows11: Boolean;
function GetDefaultThemeType: TThemeType;
function GetDefaultKeyMappingType: TKeyMappingType;
procedure OpenDonateSite;
procedure OpenMailingListSite;
procedure ReadMRUList(const MRUList: TStringList; const Section, Ident: String);
procedure ModifyMRUList(const MRUList: TStringList; const Section, Ident: String;
  const AItem: String; const AddNewItem: Boolean; CompareProc: TMRUItemCompareProc;
  const CleanMRUList: Boolean = False);
procedure LoadKnownIncludedAndHiddenFiles(const AFilename: String; const IncludedFiles, HiddenFiles: TStringList);
procedure SaveKnownIncludedAndHiddenFiles(const AFilename: String; const IncludedFiles, HiddenFiles: TStringList);
procedure DeleteKnownIncludedAndHiddenFiles(const AFilename: String);
procedure LoadBreakPointLines(const AFilename: String; const BreakPointLines: TStringList);
procedure SaveBreakPointLines(const AFilename: String; const BreakPointLines: TStringList);
procedure DeleteBreakPointLines(const AFilename: String);
procedure SetFakeShortCutText(const MenuItem: TMenuItem; const S: String);
procedure SetFakeShortCut(const MenuItem: TMenuItem; const Key: Word;
  const Shift: TShiftState);
procedure SaveTextToFile(const Filename: String;
  const S: String; const SaveEncoding: TSaveEncoding);
procedure AddLines(const ListBox: TListBox; const S: String; const AObject: TObject; const LineBreaks: Boolean; const Prefix: TAddLinesPrefix; const PrefixParam: Cardinal);
procedure SetLowPriority(ALowPriority: Boolean; var SavePriorityClass: DWORD);
function GetHelpFile: String;
function FindOptionsToSearchOptions(const FindOptions: TFindOptions): TScintFindOptions;
procedure StartAddRemovePrograms;
function GetSourcePath(const AFilename: String): String;
function ReadScriptLines(const ALines: TStringList; const ReadFromFile: Boolean;
  const ReadFromFileFilename: String; const NotReadFromFileMemo: TScintEdit): Integer;
function CreateBitmapInfo(const Width, Height, BitCount: Integer): TBitmapInfo;

implementation

uses
  ActiveX, ShlObj, ShellApi, CommDlg, SysUtils, IOUtils,
  Messages, DwmApi,
  CmnFunc2, PathFunc, FileClass, NewUxTheme,
  CompForm, CompMsgs, CompTypes;

procedure InitFormFont(Form: TForm);
var
  FontName: String;
  Metrics: TNonClientMetrics;
begin
  begin
    Metrics.cbSize := SizeOf(Metrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(Metrics),
       @Metrics, 0) then
      FontName := Metrics.lfMessageFont.lfFaceName;
    { Only allow fonts that we know will fit the text correctly }
    if not SameText(FontName, 'Microsoft Sans Serif') and
       not SameText(FontName, 'Segoe UI') then
      FontName := 'Tahoma';
  end;
  Form.Font.Name := FontName;
  Form.Font.Size := 8;
end;

procedure SetControlWindowTheme(const WinControl: TWinControl; const Dark: Boolean);
begin
  if UseThemes then begin
    if Dark then
      SetWindowTheme(WinControl.Handle, 'DarkMode_Explorer', nil)
    else
      SetWindowTheme(WinControl.Handle, nil, nil);
  end;
end;

var
  FormTheme: TTheme;

procedure InitFormThemeInit(const ATheme: TTheme);
begin
  FormTheme := ATheme;
end;

procedure InitFormTheme(Form: TForm);

  procedure InitListBoxDarkTheme(const ListBox: TListBox);
  begin
    ListBox.Font.Color := FormTheme.Colors[tcFore];
    ListBox.Color := FormTheme.Colors[tcBack];
    ListBox.Invalidate;
    SetControlWindowTheme(ListBox, FormTheme.Dark);
  end;

  procedure InitWinControlTheme(const ParentControl: TWinControl);
  begin
    for var I := 0 to ParentControl.ControlCount-1 do begin
      var Control := ParentControl.Controls[I];
      if Control is TListBox then
        InitListBoxDarkTheme(Control as TListBox)
      else if Control is TWinControl then
        InitWinControlTheme(Control as TWinControl);
    end;
  end;

begin
  if (Form = CompileForm) or FormTheme.Dark then begin
    Form.Color := FormTheme.Colors[tcBack];
  
    { Based on https://learn.microsoft.com/en-us/windows/apps/desktop/modernize/apply-windows-themes
      Unlike this article we check for Windows 10 Version 2004 because that's the first version
      that introduced DWMWA_USE_IMMERSIVE_DARK_MODE as 20 (the now documented value) instead of 19 }
    if WindowsVersionAtLeast(10, 0, 19041) then begin
      const DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
      var value: BOOL := FormTheme.Dark;
      DwmSetWindowAttribute(Form.Handle, DWMWA_USE_IMMERSIVE_DARK_MODE, @value, SizeOf(value));
    end;
  
    InitWinControlTheme(Form);
  end;
end;

function GetDisplayFilename(const Filename: String): String;
var
  Buf: array[0..MAX_PATH-1] of Char;
begin
  if CommDlg.GetFileTitle(PChar(Filename), Buf, SizeOf(Buf) div SizeOf(Buf[0])) = 0 then
    Result := Buf
  else
    Result := Filename;
end;

function GetFileTitle(const Filename: String): String;
begin
  if Filename = '' then
    Result := 'Untitled'
  else
    Result := Filename;
end;

function GetCleanFileNameOfFile(const Filename: String): String;
begin
  var Files := TDirectory.GetFiles(PathExtractDir(Filename), PathExtractName(Filename));
  if Length(Files) = 1 then
    Result := Files[0]
  else
    Result := Filename;
end;

function GetLastWriteTimeOfFile(const Filename: String;
  LastWriteTime: PFileTime): Boolean;
var
  H: THandle;
begin
  H := CreateFile(PChar(Filename), 0, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, 0, 0);
  if H <> INVALID_HANDLE_VALUE then begin
    Result := GetFileTime(H, nil, nil, LastWriteTime);
    CloseHandle(H);
  end
  else
    Result := False;
end;

procedure AddFileToRecentDocs(const Filename: String);
{ Notifies the shell that a document has been opened. This will
  add the file to the Recent section of the app's Jump List.
  It is only necessary to call this function when the shell is unaware that
  a file is being opened. Files opened through Explorer or common dialogs get
  added to the Jump List automatically. }
begin
  SHAddToRecentDocs(SHARD_PATHW, PChar(Filename));
end;

function GenerateGuid: String;
var
  Guid: TGUID;
  P: PWideChar;
begin
  if CoCreateGuid(Guid) <> S_OK then
    raise Exception.Create('CoCreateGuid failed');
  if StringFromCLSID(Guid, P) <> S_OK then
    raise Exception.Create('StringFromCLSID failed');
  try
    Result := P;
  finally
    CoTaskMemFree(P);
  end;
end;

function ISPPInstalled: Boolean;
begin
  Result := NewFileExists(PathExtractPath(NewParamStr(0)) + 'ISPP.dll');
end;

function IsISPPBuiltins(const Filename: String): Boolean;
begin
  Result := PathCompare(PathExtractName(Filename), 'ISPPBuiltins.iss') = 0;
end;

function ISCryptInstalled: Boolean;
begin
  Result := NewFileExists(PathExtractPath(NewParamStr(0)) + 'iscrypt.dll');
end;

var
  WindowsVersion: Cardinal;

function WindowsVersionAtLeast(const AMajor, AMinor: Byte; const ABuild: Word): Boolean;
begin
  Result := WindowsVersion >= Cardinal((AMajor shl 24) or (AMinor shl 16) or ABuild);
end;

function IsWindows10: Boolean;
begin
  Result := WindowsVersionAtLeast(10, 0);
end;

function IsWindows11: Boolean;
begin
  Result := WindowsVersionAtLeast(10, 0, 22000);
end;

function GetDefaultThemeType: TThemeType;
var
  K: HKEY;
  Size, AppsUseLightTheme: DWORD;
begin
  Result := ttModernLight;
  if IsWindows10 and (RegOpenKeyExView(rvDefault, HKEY_CURRENT_USER, 'SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize', 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS) then begin
    Size := SizeOf(AppsUseLightTheme);
    if (RegQueryValueEx(K, 'AppsUseLightTheme', nil, nil, @AppsUseLightTheme, @Size) = ERROR_SUCCESS) and (AppsUseLightTheme = 0) then
      Result := ttModernDark;
    RegCloseKey(K);
  end;
end;

function GetDefaultKeyMappingType: TKeyMappingType;
begin
  Result := kmtDelphi;
end;

procedure OpenDonateSite;
begin
  ShellExecute(Application.Handle, 'open', 'https://jrsoftware.org/isdonate.php', nil,
    nil, SW_SHOWNORMAL);
end;

procedure OpenMailingListSite;
begin
  ShellExecute(Application.Handle, 'open', 'https://jrsoftware.org/ismail.php', nil,
    nil, SW_SHOWNORMAL);
end;

procedure ReadMRUList(const MRUList: TStringList; const Section, Ident: String);
{ Loads a list of MRU items from the registry }
var
  Ini: TConfigIniFile;
  I: Integer;
  S: String;
begin
  Ini := TConfigIniFile.Create;
  try
    MRUList.Clear;
    for I := 0 to MRUListMaxCount-1 do begin
      S := Ini.ReadString(Section, Ident + IntToStr(I), '');
      if S <> '' then MRUList.Add(S);
    end;
  finally
    Ini.Free;
  end;
end;

procedure ModifyMRUList(const MRUList: TStringList; const Section, Ident: String;
  const AItem: String; const AddNewItem: Boolean; CompareProc: TMRUItemCompareProc;
  const CleanMRUList: Boolean);
var
  I: Integer;
  Ini: TConfigIniFile;
  S: String;
begin
  I := 0;
  while I < MRUList.Count do begin
    if not CleanMRUList then begin
      if CompareProc(MRUList[I], AItem) = 0 then
        MRUList.Delete(I)
      else
        Inc(I);
    end else
      MRUList.Delete(I);
  end;
  if AddNewItem then
    MRUList.Insert(0, AItem);
  while MRUList.Count > MRUListMaxCount do
    MRUList.Delete(MRUList.Count-1);

  { Save new MRU items }
  Ini := TConfigIniFile.Create;
  try
    { MRU list }
    for I := 0 to MRUListMaxCount-1 do begin
      if (I < MRUList.Count) and not CleanMRUList then
        S := MRUList[I]
      else
        S := '';
      Ini.WriteString(Section, Ident + IntToStr(I), S);
    end;
  finally
    Ini.Free;
  end;
end;

procedure LoadConfigIniList(const AIni: TConfigIniFile; const ASection, AIdent: String;
  const AList: TStringList; const ADelimiter: Char);
begin
  if ASection = '' then
    raise Exception.Create('ASection must be set');

  var OldDelimiter := AList.Delimiter;
  AList.Delimiter := ADelimiter;
  try
    AList.DelimitedText := AIni.ReadString(ASection, AIdent, '');
  finally
    AList.Delimiter := OldDelimiter;
  end;
end;

procedure DeleteConfigIniList(const AIni: TConfigIniFile; const ASection, AIdent: String);
begin
  if ASection = '' then
    raise Exception.Create('ASection must be set');

  AIni.DeleteKey(ASection, AIdent);
end;

procedure SaveConfigIniList(const AIni: TConfigIniFile; const ASection, AIdent: String;
  const AList: TStringList; const ADelimiter: Char);
begin
  if AList.Count = 0 then begin
    DeleteConfigIniList(AIni, ASection, AIdent);
    Exit;
  end;

  if ASection = '' then
    raise Exception.Create('ASection must be set');

  var OldDelimiter := AList.Delimiter;
  AList.Delimiter := ADelimiter;
  try
    AIni.WriteString(ASection, AIdent, AList.DelimitedText);
  finally
    AList.Delimiter := OldDelimiter;
  end;
end;

procedure LoadKnownIncludedAndHiddenFiles(const AFilename: String; const IncludedFiles, HiddenFiles: TStringList);
begin
  var Ini := TConfigIniFile.Create;
  try
    LoadConfigIniList(Ini, 'IncludedFilesHistory', AFilename, IncludedFiles, '*');
    LoadConfigIniList(Ini, 'HiddenFilesHistory', AFilename, HiddenFiles, '*');
  finally
    Ini.Free;
  end;
end;

procedure SaveKnownIncludedAndHiddenFiles(const AFilename: String; const IncludedFiles, HiddenFiles: TStringList);
begin
  var Ini := TConfigIniFile.Create;
  try
    SaveConfigIniList(Ini, 'IncludedFilesHistory', AFilename, IncludedFiles, '*');
    SaveConfigIniList(Ini, 'HiddenFilesHistory', AFilename, HiddenFiles, '*');
  finally
    Ini.Free;
  end;
end;

procedure DeleteKnownIncludedAndHiddenFiles(const AFilename: String);
begin
  var Ini := TConfigIniFile.Create;
  try
    DeleteConfigIniList(Ini, 'IncludedFilesHistory', AFilename);
    DeleteConfigIniList(Ini, 'HiddenFilesHistory', AFilename);
  finally
    Ini.Free;
  end;
end;

procedure LoadBreakPointLines(const AFilename: String; const BreakPointLines: TStringList);
begin
  var Ini := TConfigIniFile.Create;
  try
    LoadConfigIniList(Ini, 'BreakPointLines', AFilename, BreakPointLines, ',');
  finally
    Ini.Free;
  end;
end;

procedure SaveBreakPointLines(const AFilename: String; const BreakPointLines: TStringList);
begin
  var Ini := TConfigIniFile.Create;
  try
    SaveConfigIniList(Ini, 'BreakPointLines', AFilename, BreakPointLines, ',');
  finally
    Ini.Free;
  end;
end;

procedure DeleteBreakPointLines(const AFilename: String);
begin
  var Ini := TConfigIniFile.Create;
  try
    DeleteConfigIniList(Ini, 'BreakPointLines', AFilename);
  finally
    Ini.Free;
  end;
end;

procedure SetFakeShortCutText(const MenuItem: TMenuItem; const S: String);
begin
  MenuItem.Caption := MenuItem.Caption + #9 + S;
end;

procedure SetFakeShortCut(const MenuItem: TMenuItem; const Key: Word;
  const Shift: TShiftState);
begin
  SetFakeShortCutText(MenuItem, ShortCutToText(ShortCut(Key, Shift)));
end;

procedure SaveTextToFile(const Filename: String;
  const S: String; const SaveEncoding: TSaveEncoding);
var
  AnsiMode: Boolean;
  AnsiStr: AnsiString;
  F: TTextFileWriter;
begin
  AnsiMode := False;
  if SaveEncoding = seAuto then begin
    AnsiStr := AnsiString(S);
    if S = String(AnsiStr) then
      AnsiMode := True;
  end;

  F := TTextFileWriter.Create(Filename, fdCreateAlways, faWrite, fsNone);
  try
    if AnsiMode then
      F.WriteAnsi(AnsiStr)
    else begin
      F.UTF8WithoutBOM := SaveEncoding <> seUTF8WithBOM;
      F.Write(S);
    end;
  finally
    F.Free;
  end;
end;

procedure AddLines(const ListBox: TListBox; const S: String; const AObject: TObject; const LineBreaks: Boolean; const Prefix: TAddLinesPrefix; const PrefixParam: Cardinal);
var
  ST: TSystemTime;
  LineNumber: Cardinal;

  procedure AddLine(S: String);
  var
    TimestampPrefixTab: Boolean;
    DC: HDC;
    Size: TSize;
  begin
    TimestampPrefixTab := False;
    case Prefix of
      alpTimestamp:
        begin
          if LineNumber = 0 then begin
            { Don't forget about ListBox's DrawItem if you change the format of the following timestamp. }
            Insert(Format('[%.2u%s%.2u%s%.2u%s%.3u]   ', [ST.wHour, FormatSettings.TimeSeparator,
              ST.wMinute, FormatSettings.TimeSeparator, ST.wSecond, FormatSettings.DecimalSeparator,
              ST.wMilliseconds]), S, 1);
          end else begin
            Insert(#9, S, 1); { Not actually painted - just for Ctrl+C }
            TimestampPrefixTab := True;
          end;
        end;
      alpCountdown:
        begin
          Insert(Format('[%.2d]   ', [PrefixParam-LineNumber]), S, 1);
        end;
    end;
    try
      ListBox.TopIndex := ListBox.Items.AddObject(S, AObject);
    except
      on EOutOfResources do begin
        ListBox.Clear;
        SendMessage(ListBox.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
        ListBox.Items.Add(SCompilerStatusReset);
        ListBox.TopIndex := ListBox.Items.AddObject(S, AObject);
      end;
    end;
    DC := GetDC(0);
    try
      SelectObject(DC, ListBox.Font.Handle);
      if TimestampPrefixTab then
        GetTextExtentPoint(DC, PChar(S)+1, Length(S)-1, Size)
      else
        GetTextExtentPoint(DC, PChar(S), Length(S), Size);
    finally
      ReleaseDC(0, DC);
    end;
    Inc(Size.cx, 5);
    if TimestampPrefixTab then
      Inc(Size.cx, PrefixParam);
    if Size.cx > SendMessage(ListBox.Handle, LB_GETHORIZONTALEXTENT, 0, 0) then
      SendMessage(ListBox.Handle, LB_SETHORIZONTALEXTENT, Size.cx, 0);
    Inc(LineNumber);
  end;

var
  LineStart, I: Integer;
  LastWasCR: Boolean;
begin
  GetLocalTime(ST);
  if LineBreaks then begin
    LineNumber := 0;
    LineStart := 1;
    LastWasCR := False;
    { Call AddLine for each line. CR, LF, and CRLF line breaks are supported. }
    for I := 1 to Length(S) do begin
      if S[I] = #13 then begin
        AddLine(Copy(S, LineStart, I - LineStart));
        LineStart := I + 1;
        LastWasCR := True;
      end
      else begin
        if S[I] = #10 then begin
          if not LastWasCR then
            AddLine(Copy(S, LineStart, I - LineStart));
          LineStart := I + 1;
        end;
        LastWasCR := False;
      end;
    end;
    AddLine(Copy(S, LineStart, Maxint));
  end else
    AddLine(S);
end;

procedure SetLowPriority(ALowPriority: Boolean; var SavePriorityClass: DWORD);
begin
  if ALowPriority then begin
    { Save current priority and change to 'low' }
    if SavePriorityClass = 0 then
      SavePriorityClass := GetPriorityClass(GetCurrentProcess);
    SetPriorityClass(GetCurrentProcess, IDLE_PRIORITY_CLASS);
  end
  else begin
    { Restore original priority }
    if SavePriorityClass <> 0 then begin
      SetPriorityClass(GetCurrentProcess, SavePriorityClass);
      SavePriorityClass := 0;
    end;
  end;
end;

function GetHelpFile: String;
begin
  Result := PathExtractPath(NewParamStr(0)) + 'isetup.chm';
end;

function FindOptionsToSearchOptions(const FindOptions: TFindOptions): TScintFindOptions;
begin
  Result := [];
  if frMatchCase in FindOptions then
    Include(Result, sfoMatchCase);
  if frWholeWord in FindOptions then
    Include(Result, sfoWholeWord);
end;

procedure StartAddRemovePrograms;
var
  Dir: String;
  Wow64DisableWow64FsRedirectionFunc: function(var OldValue: Pointer): BOOL; stdcall;
  Wow64RevertWow64FsRedirectionFunc: function(OldValue: Pointer): BOOL; stdcall;
  RedirDisabled: Boolean;
  RedirOldValue: Pointer;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  Dir := GetSystemDir;

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  { Have to disable file system redirection because the 32-bit version of
    appwiz.cpl is buggy on XP x64 RC2 -- it doesn't show any Change/Remove
    buttons on 64-bit MSI entries, and it doesn't list non-MSI 64-bit apps
    at all. }
  Wow64DisableWow64FsRedirectionFunc := GetProcAddress(GetModuleHandle(kernel32),
    'Wow64DisableWow64FsRedirection');
  Wow64RevertWow64FsRedirectionFunc := GetProcAddress(GetModuleHandle(kernel32),
    'Wow64RevertWow64FsRedirection');
  RedirDisabled := Assigned(Wow64DisableWow64FsRedirectionFunc) and
    Assigned(Wow64RevertWow64FsRedirectionFunc) and
    Wow64DisableWow64FsRedirectionFunc(RedirOldValue);
  try
    Win32Check(CreateProcess(nil, PChar('"' + AddBackslash(Dir) + 'control.exe" appwiz.cpl'),
       nil, nil, False, 0, nil, PChar(Dir), StartupInfo, ProcessInfo));
  finally
    if RedirDisabled then
      Wow64RevertWow64FsRedirectionFunc(RedirOldValue);
  end;
  CloseHandle(ProcessInfo.hProcess);
  CloseHandle(ProcessInfo.hThread);
end;

function GetSourcePath(const AFilename: String): String;
begin
  if AFilename <> '' then
    Result := PathExtractPath(AFilename)
  else begin
    { If the script was not saved, default to My Documents }
    Result := GetShellFolderPath(CSIDL_PERSONAL);
    if Result = '' then
      raise Exception.Create('GetShellFolderPath failed');
  end;
end;

function ReadScriptLines(const ALines: TStringList; const ReadFromFile: Boolean;
  const ReadFromFileFilename: String; const NotReadFromFileMemo: TScintEdit): Integer;

  function ContainsNullChar(const S: String): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to Length(S) do
      if S[I] = #0 then begin
        Result := True;
        Break;
      end;
  end;

var
  F: TTextFileReader;
  I: Integer;
begin
  if ReadFromFile then begin
    F := TTextFileReader.Create(ReadFromFileFilename, fdOpenExisting, faRead, fsRead);
    try
      while not F.Eof do
        ALines.Add(F.ReadLine);
    finally
      F.Free;
    end;
  end
  else begin
    ALines.Capacity := NotReadFromFileMemo.Lines.Count;
    ALines.Assign(NotReadFromFileMemo.Lines);
  end;

  { Check for null characters }
  for I := 0 to ALines.Count-1 do begin
    if ContainsNullChar(ALines[I]) then begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function CreateBitmapInfo(const Width, Height, BitCount: Integer): TBitmapInfo;
begin
  ZeroMemory(@Result, SizeOf(Result));
  Result.bmiHeader.biSize := SizeOf(Result.bmiHeader);
  Result.bmiHeader.biWidth := Width;
  Result.bmiHeader.biHeight := Height;
  Result.bmiHeader.biPlanes := 1;
  Result.bmiHeader.biBitCount := BitCount;
  Result.bmiHeader.biCompression := BI_RGB;
end;

initialization
  var OSVersionInfo: TOSVersionInfo;
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  WindowsVersion := (Byte(OSVersionInfo.dwMajorVersion) shl 24) or (Byte(OSVersionInfo.dwMinorVersion) shl 16) or Word(OSVersionInfo.dwBuildNumber);

end.
