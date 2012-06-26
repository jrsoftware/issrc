program SetupLdr;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Setup Loader
}

uses
  SafeDLLPath in 'SafeDLLPath.pas',
  XPTheme in 'XPTheme.pas',
  Windows,
  Messages,
  SysUtils,
  Compress in 'Compress.pas',
  LZMADecompSmall in 'LZMADecompSmall.pas',
  SetupEnt in 'SetupEnt.pas',
  PathFunc,
  CmnFunc2 in 'CmnFunc2.pas',
  Msgs in 'Msgs.pas',
  MsgIDs in 'MsgIDs.pas',
  Struct in 'Struct.pas',
  InstFunc in 'InstFunc.pas',
  FileClass in 'FileClass.pas';

{$R *.RES}
{$R SetupLdrVersion.res}
{$R SetupLdrOffsetTable.res}

procedure RaiseLastError(const Msg: TSetupMessageID);
var
  ErrorCode: DWORD;
begin
  ErrorCode := GetLastError;
  raise Exception.Create(FmtSetupMessage(msgLastErrorMessage,
    [SetupMessages[Msg], IntToStr(ErrorCode), Win32ErrorString(ErrorCode)]));
end;

procedure ShowExceptionMsg;
begin
  if ExceptObject is EAbort then
    Exit;
  MessageBox(0, PChar(GetExceptMessage), Pointer(SetupMessages[msgErrorTitle]),
    MB_OK or MB_ICONSTOP);
    { ^ use a Pointer cast instead of a PChar cast so that it will use "nil"
      if SetupMessages[msgErrorTitle] is empty due to the messages not being
      loaded yet. MessageBox displays 'Error' as the caption if the lpCaption
      parameter is nil. }
end;

const
  { Exit codes that are returned by SetupLdr.
    Note: Setup also returns exit codes with the same numbers. }
  ecInitializationError = 1;     { Setup failed to initialize. }
  ecCancelledBeforeInstall = 2;  { User clicked Cancel before the actual
                                   installation started. }

type
  PLanguageEntryArray = ^TLanguageEntryArray;
  TLanguageEntryArray = array[0..999999] of TSetupLanguageEntry;

var
  InitDisableStartupPrompt: Boolean = False;
  InitLang: String;
  ActiveLanguage: Integer = -1;
  PendingNewLanguage: Integer = -1;
  SetupHeader: TSetupHeader;
  LanguageEntries: PLanguageEntryArray;
  LanguageEntryCount: Integer;
  SetupLdrExitCode: Integer = ecInitializationError;
  SetupLdrWnd: HWND = 0;
  OrigWndProc: Pointer;
  RestartSystem: Boolean = False;

procedure ProcessCommandLine;
var
  I: Integer;
  Name: String;
begin
  for I := 1 to NewParamCount do begin
    Name := NewParamStr(I);
    if (CompareText(Name, '/SP-') = 0) or
       (CompareText(Copy(Name, 1, 10), '/SPAWNWND=') = 0) then
      InitDisableStartupPrompt := True
    else if CompareText(Copy(Name, 1, 6), '/Lang=') = 0 then
      InitLang := Copy(Name, 7, Maxint);
  end;
end;

procedure SetActiveLanguage(const I: Integer);
{ Activates the specified language }
begin
  if (I >= 0) and (I < LanguageEntryCount) and (I <> ActiveLanguage) then begin
    AssignSetupMessages(LanguageEntries[I].Data[1], Length(LanguageEntries[I].Data));
    ActiveLanguage := I;
  end;
end;

function GetLanguageEntryProc(Index: Integer; var Entry: PSetupLanguageEntry): Boolean;
begin
  Result := False;
  if Index < LanguageEntryCount then begin
    Entry := @LanguageEntries[Index];
    Result := True;
  end;
end;

procedure ActivateDefaultLanguage;
{ Auto-detects the most appropriate language and activates it.
  Note: A like-named version of this function is also present in Main.pas. }
var
  I: Integer;
begin
  DetermineDefaultLanguage(GetLanguageEntryProc, SetupHeader.LanguageDetectionMethod,
    InitLang, I);
  SetActiveLanguage(I);
end;

function SetupLdrWndProc(Wnd: HWND; Msg: UINT; WParam: WPARAM; LParam: LPARAM): LRESULT;
stdcall;
begin
  Result := 0;
  case Msg of
    WM_QUERYENDSESSION: begin
        { Return zero so that a shutdown attempt can't kill SetupLdr }
      end;
    WM_USER + 150: begin
        if WParam = 10000 then begin
          { Setup wants SetupLdr to restart the computer before it exits }
          RestartSystem := True;
        end
        else if WParam = 10001 then begin
          { Setup wants SetupLdr to change its active language }
          PendingNewLanguage := LParam;
        end;
      end;
  else
    Result := CallWindowProc(OrigWndProc, Wnd, Msg, WParam, LParam);
  end;
end;

procedure ProcessMessages;
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;

procedure ExecAndWait(const Filename, Parms: String; var ExitCode: Integer);
var
  CmdLine: String;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  CmdLine := '"' + Filename + '" ' + Parms;

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  if not CreateProcess(nil, PChar(CmdLine), nil, nil, False, 0, nil, nil,
     StartupInfo, ProcessInfo) then
    RaiseLastError(msgLdrCannotExecTemp);
  CloseHandle(ProcessInfo.hThread);
  { Wait for the process to terminate, processing messages in the meantime }
  repeat
    ProcessMessages;
  until MsgWaitForMultipleObjects(1, ProcessInfo.hProcess, False, INFINITE,
    QS_ALLINPUT) <> WAIT_OBJECT_0+1;
  { Now that the process has exited, process any remaining messages.
    (There may be an asynchronously-sent "restart request" message still
    queued if MWFMO saw the process terminate before checking for new
    messages.) }
  ProcessMessages;
  GetExitCodeProcess(ProcessInfo.hProcess, DWORD(ExitCode));
  CloseHandle(ProcessInfo.hProcess);
end;

procedure SetupCorruptError;
begin
  if SetupMessages[msgSetupFileCorrupt] <> '' then
    raise Exception.Create(SetupMessages[msgSetupFileCorrupt])
  else
    { In case the messages haven't been loaded yet, use the constant }
    raise Exception.Create(SSetupFileCorrupt);
end;

procedure RunImageLocally(const Module: HMODULE);
{ Force all of the specified module to be paged in to ensure subsequent
  accesses don't cause the disk image to be read.
  Based on code from http://www.microsoft.com/msj/0398/win320398.htm, with
  some fixes incorporated. }

  procedure Touch(var X: DWORD);
  { Note: Uses asm to ensure it isn't optimized away }
  asm
    xor edx, edx
    lock or [eax], edx
  end;

var
  SysInfo: TSystemInfo;
  CurAddr: Pointer;
  MemInfo: TMemoryBasicInformation;
  ChangedProtection: Boolean;
  OrigProtect: DWORD;
  Offset: Cardinal;
begin
  { Get system's page size }
  GetSystemInfo(SysInfo);

  CurAddr := Pointer(Module);
  if VirtualQuery(CurAddr, MemInfo, SizeOf(MemInfo)) = 0 then
    Exit;

  { Perform this loop until we find a region beyond the end of the file }
  while MemInfo.AllocationBase = Pointer(Module) do begin
    { We can only force committed pages into RAM.
      We do not want to trigger guard pages and confuse the application. }
    if (MemInfo.State = MEM_COMMIT) and (MemInfo.Protect and PAGE_GUARD = 0) then begin
      ChangedProtection := False;

      { Determine if the pages in this region are nonwriteable }
      if (MemInfo.Protect = PAGE_NOACCESS) or
         (MemInfo.Protect = PAGE_READONLY) or
         (MemInfo.Protect = PAGE_EXECUTE) or
         (MemInfo.Protect = PAGE_EXECUTE_READ) then begin
        { Nonwriteable region, make it writeable (with the least protection) }
        if VirtualProtect(MemInfo.BaseAddress, MemInfo.RegionSize,
           PAGE_EXECUTE_READWRITE, @OrigProtect) then
          ChangedProtection := True;
      end;

      { Write to every page in the region.
        This forces the page to be in RAM and swapped to the paging file. }
      Offset := 0;
      while Offset < Cardinal(MemInfo.RegionSize) do begin
        Touch(PDWORD(Cardinal(MemInfo.BaseAddress) + Offset)^);
        Inc(Offset, SysInfo.dwPageSize);
      end;

      { If we changed the protection, change it back }
      if ChangedProtection then
        VirtualProtect(MemInfo.BaseAddress, MemInfo.RegionSize, OrigProtect,
          @OrigProtect);
    end;

    { Get next region }
    Cardinal(CurAddr) := Cardinal(MemInfo.BaseAddress) + MemInfo.RegionSize;
    if VirtualQuery(CurAddr, MemInfo, SizeOf(MemInfo)) = 0 then
      Break;
  end;
end;

function GetSetupLdrOffsetTable: PSetupLdrOffsetTable;
{ Locates the offset table resource, and returns a pointer to it }
var
  Rsrc: HRSRC;
  ResData: HGLOBAL;
begin
  Rsrc := FindResource(0, MAKEINTRESOURCE(SetupLdrOffsetTableResID), RT_RCDATA);
  if Rsrc = 0 then
    SetupCorruptError;
  if SizeofResource(0, Rsrc) <> SizeOf(Result^) then
    SetupCorruptError;
  ResData := LoadResource(0, Rsrc);
  if ResData = 0 then
    SetupCorruptError;
  Result := LockResource(ResData);
  if Result = nil then
    SetupCorruptError;
end;

var
  SelfFilename: String;
  SourceF, DestF: TFile;
  OffsetTable: PSetupLdrOffsetTable;
  TempDir: String;
  S: String;
  TempFile: String;
  TestID: TSetupID;
  P: Pointer;
  Reader: TCompressedBlockReader;
  I: Integer;
begin
  try
    { Ensure all of SetupLdr is paged in so that in the case of a disk spanning
      install Windows will never complain when the disk/CD containing SetupLdr
      is ejected. }
    RunImageLocally(HInstance);

    ProcessCommandLine;

    SelfFilename := NewParamStr(0);
    SourceF := TFile.Create(SelfFilename, fdOpenExisting, faRead, fsRead);
    try
      OffsetTable := GetSetupLdrOffsetTable;
      { Note: We don't check the OffsetTable.ID here because it would put a
        copy of the ID in the data section, and that would confuse external
        programs that search for the offset table by ID. } 
      if (OffsetTable.Version <> SetupLdrOffsetTableVersion) or
         (GetCRC32(OffsetTable^, SizeOf(OffsetTable^) - SizeOf(OffsetTable.TableCRC)) <> OffsetTable.TableCRC) or
         ((SourceF.Size.Hi = 0) and (SourceF.Size.Lo < OffsetTable.TotalSize)) then
        SetupCorruptError;

      SourceF.Seek(OffsetTable.Offset0);
      SourceF.ReadBuffer(TestID, SizeOf(TestID));
      if TestID <> SetupID then
        SetupCorruptError;
      try
        Reader := TCompressedBlockReader.Create(SourceF, TLZMA1SmallDecompressor);
        try
          SECompressedBlockRead(Reader, SetupHeader, SizeOf(SetupHeader),
            SetupHeaderStrings, SetupHeaderAnsiStrings);

          LanguageEntryCount := SetupHeader.NumLanguageEntries;
          LanguageEntries := AllocMem(LanguageEntryCount * SizeOf(TSetupLanguageEntry));
          for I := 0 to LanguageEntryCount-1 do
            SECompressedBlockRead(Reader, LanguageEntries[I], SizeOf(LanguageEntries[I]),
              SetupLanguageEntryStrings, SetupLanguageEntryAnsiStrings);
        finally
          Reader.Free;
        end;
      except
        on ECompressDataError do
          SetupCorruptError;
      end;

      ActivateDefaultLanguage;

      { Show the startup prompt. If this is enabled, SetupHeader.AppName won't
        have constants. }
      if not(shDisableStartupPrompt in SetupHeader.Options) and
         not InitDisableStartupPrompt and
         (MessageBox(0, PChar(FmtSetupMessage1(msgSetupLdrStartupMessage, SetupHeader.AppName)),
           PChar(SetupMessages[msgSetupAppTitle]), MB_YESNO or MB_ICONQUESTION) <> IDYES) then begin
        SetupLdrExitCode := ecCancelledBeforeInstall;
        Abort;
      end;

      { Create a temporary directory, and extract the embedded setup program
        there }
      Randomize;
      TempDir := CreateTempDir;
      S := AddBackslash(TempDir) + PathChangeExt(PathExtractName(SelfFilename), '.tmp');
      TempFile := S;  { assign only if string was successfully constructed }

      SourceF.Seek(OffsetTable.OffsetEXE);

      try
        P := nil;
        DestF := TFile.Create(TempFile, fdCreateAlways, faWrite, fsNone);
        try
          GetMem(P, OffsetTable.UncompressedSizeEXE);
          FillChar(P^, OffsetTable.UncompressedSizeEXE, 0);
          try
            Reader := TCompressedBlockReader.Create(SourceF, TLZMA1SmallDecompressor);
            try
              Reader.Read(P^, OffsetTable.UncompressedSizeEXE);
            finally
              Reader.Free;
            end;
          except
            on ECompressDataError do
              SetupCorruptError;
          end;
          TransformCallInstructions(P^, OffsetTable.UncompressedSizeEXE, False, 0);
          if GetCRC32(P^, OffsetTable.UncompressedSizeEXE) <> OffsetTable.CRCEXE then
            SetupCorruptError;
          { Preallocate the bytes to avoid file system fragmentation }
          DestF.Seek(OffsetTable.UncompressedSizeEXE);
          DestF.Truncate;
          DestF.Seek(0);
          DestF.WriteBuffer(P^, OffsetTable.UncompressedSizeEXE);
        finally
          FreeMem(P);
          DestF.Free;
        end;
      except
        on E: EFileError do begin
          SetLastError(E.ErrorCode);
          RaiseLastError(msgLdrCannotCreateTemp);
        end;
      end;

      FreeAndNil(SourceF);

      { Create SetupLdrWnd, which is used by Setup to communicate with
        SetupLdr }
      SetupLdrWnd := CreateWindowEx(0, 'STATIC', 'InnoSetupLdrWindow', 0,
        0, 0, 0, 0, HWND_DESKTOP, 0, HInstance, nil);
      Longint(OrigWndProc) := SetWindowLong(SetupLdrWnd, GWL_WNDPROC,
        Longint(@SetupLdrWndProc));

      { Now execute Setup. Use the exit code it returns as our exit code. }
      ExecAndWait(TempFile, Format('/SL5="$%x,%d,%d,',
        [SetupLdrWnd, OffsetTable.Offset0, OffsetTable.Offset1]) +
        SelfFilename + '" ' + GetCmdTail, SetupLdrExitCode);

      { Synchronize our active language with Setup's, in case we need to
        display any messages below } 
      if PendingNewLanguage <> -1 then
        SetActiveLanguage(PendingNewLanguage);
    finally
      SourceF.Free;
      if TempFile <> '' then
        { Even though Setup has terminated by now, the system may still have
          the file locked for a short period of time (esp. on multiprocessor
          systems), so use DelayDeleteFile to delete it. }
        DelayDeleteFile(False, TempFile, 13, 50, 250);
      if TempDir <> '' then
        RemoveDirectory(PChar(TempDir));
      if SetupLdrWnd <> 0 then
        { SetupLdrWnd must be destroyed before RestartComputer is called,
          otherwise SetupLdrWndProc would deny the shutdown request }
        DestroyWindow(SetupLdrWnd);
      if Assigned(LanguageEntries) then begin
        Finalize(LanguageEntries[0], LanguageEntryCount);
        FreeMem(LanguageEntries);
        LanguageEntries := nil;
      end;
    end;
    if RestartSystem then begin
      if not RestartComputer then
        MessageBox(0, PChar(SetupMessages[msgErrorRestartingComputer]),
          PChar(SetupMessages[msgErrorTitle]), MB_OK or MB_ICONEXCLAMATION or
          MB_SETFOREGROUND);
    end;
  except
    ShowExceptionMsg;
  end;
  Halt(SetupLdrExitCode);
end.
