program SetupLdr;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Setup Loader
}

uses
  SafeDLLPath in '..\Components\SafeDLLPath.pas',
  SetupLdrAndSetup.XPTheme in 'Src\SetupLdrAndSetup.XPTheme.pas',
  Windows,
  Messages,
  SysUtils,
  Compression.Base in 'Src\Compression.Base.pas',
  Compression.LZMA1SmallDecompressor in 'Src\Compression.LZMA1SmallDecompressor.pas',
  Shared.SetupEntFunc in 'Src\Shared.SetupEntFunc.pas',
  PathFunc in '..\Components\PathFunc.pas',
  Shared.CommonFunc in 'Src\Shared.CommonFunc.pas',
  SetupLdrAndSetup.Messages in 'Src\SetupLdrAndSetup.Messages.pas',
  Shared.SetupMessageIDs in 'Src\Shared.SetupMessageIDs.pas',
  Shared.Struct in 'Src\Shared.Struct.pas',
  SetupLdrAndSetup.InstFunc in 'Src\SetupLdrAndSetup.InstFunc.pas',
  Shared.FileClass in 'Src\Shared.FileClass.pas',
  SHA256 in '..\Components\SHA256.pas',
  Shared.VerInfoFunc in 'Src\Shared.VerInfoFunc.pas',
  Shared.EncryptionFunc in 'Src\Shared.EncryptionFunc.pas',
  ChaCha20 in '..\Components\ChaCha20.pas',
  PBKDF2 in '..\Components\PBKDF2.pas',
  UnsignedFunc in '..\Components\UnsignedFunc.pas';

{$SETPEOSVERSION 6.1}
{$SETPESUBSYSVERSION 6.1}
{$WEAKLINKRTTI ON}

{ The compiler may delete one of the icons included here }
{$R Res\Setup.icon.res}
{$R Res\Setup.icon.dark.res}
{$R Res\SetupLdr.version.res}
{$R Res\SetupLdr.offsettable.res}

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
  InitShowHelp: Boolean = False;
  InitDisableStartupPrompt: Boolean = False;
  InitSuppressMsgBoxes: Boolean = False;
  InitLang, InitPassword: String;
  ActiveLanguage: Integer = -1;
  PendingNewLanguage: Integer = -1;
  SetupEncryptionHeader: TSetupEncryptionHeader;
  SetupHeader: TSetupHeader;
  LanguageEntries: PLanguageEntryArray;
  LanguageEntryCount: Integer;
  SetupLdrExitCode: Integer = ecInitializationError;
  SetupLdrWnd: HWND = 0;
  OrigWndProc: Pointer;
  RestartSystem: Boolean = False;

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
  if not InitSuppressMsgBoxes then
    MessageBox(0, PChar(GetExceptMessage), Pointer(SetupMessages[msgErrorTitle]),
      MB_OK or MB_ICONSTOP);
      { ^ use a Pointer cast instead of a PChar cast so that it will use "nil"
        if SetupMessages[msgErrorTitle] is empty due to the messages not being
        loaded yet. MessageBox displays 'Error' as the caption if the lpCaption
        parameter is nil. }
end;

procedure ProcessCommandLine(var SelfFilename: String);
begin
  var SilentOrVerySilent := False;
  var WantToSuppressMsgBoxes := False;
  for var I := 1 to NewParamCount do begin
    var ParamName, ParamValue: String;
    SplitNewParamStr(I, ParamName, ParamValue);
    if SameText(ParamName, '/SP-') or SameText(ParamName, '/SPAWNWND=') then
      InitDisableStartupPrompt := True
    else if SameText(ParamName, '/Lang=') then
      InitLang := ParamValue
    else if SameText(ParamName, '/Password=') then
      InitPassword := ParamValue
    else if SameText(ParamName, '/HELP') or SameText(ParamName, '/?') then
      InitShowHelp := True
    else if SameText(ParamName, '/Silent') or SameText(ParamName, '/VerySilent') then
      SilentOrVerySilent := True
    else if SameText(ParamName, '/SuppressMsgBoxes') then
      WantToSuppressMsgBoxes := True
    {$IFDEF DEBUG}
    else if SameText(ParamName, '/SELFFILENAME=') then
      SelfFilename := PathExpand(ParamValue);
    {$ENDIF};
  end;

  if WantToSuppressMsgBoxes and SilentOrVerySilent then
    InitSuppressMsgBoxes := True;
end;

procedure SetActiveLanguage(const I: Integer);
{ Activates the specified language }
begin
  if (I >= 0) and (I < LanguageEntryCount) and (I <> ActiveLanguage) then begin
    AssignSetupMessages(LanguageEntries[I].Data[1], ULength(LanguageEntries[I].Data));
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
          PendingNewLanguage := Integer(LParam);
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

  { Pass current directory in "final" reparsed form so that Setup won't have
    trouble accessing the directory after enabling RedirectionGuard if there's
    an untrusted redirect in the path. }
  const WorkingDir = GetFinalCurrentDir;

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  if not CreateProcess(nil, PChar(CmdLine), nil, nil, False, 0, nil,
     PChar(WorkingDir), StartupInfo, ProcessInfo) then
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

  procedure Touch(var X: Integer);
  begin
    { An atomic operation is used to ensure we can't corrupt a simultaneous
      write by another thread (though there shouldn't be other threads) }
    InterlockedExchangeAdd(X, 0);
  end;

var
  SysInfo: TSystemInfo;
  CurAddr: Pointer;
  MemInfo: TMemoryBasicInformation;
  ChangedProtection: Boolean;
  OrigProtect: DWORD;
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
      var Offset: SIZE_T := 0;
      while Offset < MemInfo.RegionSize do begin
        Touch(PInteger(PByte(MemInfo.BaseAddress) + Offset)^);
        Inc(Offset, SysInfo.dwPageSize);
      end;

      { If we changed the protection, change it back }
      if ChangedProtection then
        VirtualProtect(MemInfo.BaseAddress, MemInfo.RegionSize, OrigProtect,
          @OrigProtect);
    end;

    { Get next region }
    PByte(CurAddr) := PByte(MemInfo.BaseAddress) + MemInfo.RegionSize;
    if VirtualQuery(CurAddr, MemInfo, SizeOf(MemInfo)) = 0 then
      Break;
  end;
end;

function GetSetupLdrOffsetTable(const M: HMODULE): PSetupLdrOffsetTable;
{ Locates the offset table resource, and returns a pointer to it }
var
  Rsrc: HRSRC;
  ResData: HGLOBAL;
begin
  Rsrc := FindResource(M, MAKEINTRESOURCE(SetupLdrOffsetTableResID), RT_RCDATA);
  if Rsrc = 0 then
    SetupCorruptError;
  if SizeofResource(M, Rsrc) <> SizeOf(Result^) then
    SetupCorruptError;
  ResData := LoadResource(M, Rsrc);
  if ResData = 0 then
    SetupCorruptError;
  Result := LockResource(ResData);
  if Result = nil then
    SetupCorruptError;
end;

procedure ShowHelp(const CustomNote: String);
const
  SNewLine = #13#10;
var
  PrNote, Help: String;
begin
  { do not localize }
  
  if proCommandLine in SetupHeader.PrivilegesRequiredOverridesAllowed then begin
    PrNote := '/ALLUSERS' + SNewLine +
              'Instructs Setup to install in administrative install mode.' + SNewLine +
              '/CURRENTUSER' + SNewLine +
              'Instructs Setup to install in non administrative install mode.' + SNewLine;
  end else
    PrNote := '';

  Help := 'The Setup program accepts optional command line parameters.' + SNewLine +
          SNewLine +
          '/HELP, /?' + SNewLine +
           'Shows this information.' + SNewLine +
          '/SP-' + SNewLine +
          'Disables the "This will install... Do you wish to continue?" message box at the beginning of Setup.' + SNewLine +
          '/SILENT, /VERYSILENT' + SNewLine +
          'Instructs Setup to be silent or very silent.' + SNewLine +
          '/NOSTYLE' + SNewLine +
          'Prevents Setup from activating custom styles (including the built-in custom dark style).' + SNewLine +
          '/SUPPRESSMSGBOXES' + SNewLine +
          'Instructs Setup to suppress message boxes.' + SNewLine +
          '/LOG' + SNewLine +
          'Causes Setup to create a log file in the user''s TEMP directory.' + SNewLine +
          '/LOG="filename"' + SNewLine +
          'Same as /LOG, except it allows you to specify a fixed path/filename to use for the log file.' + SNewLine +
          '/NOCANCEL' + SNewLine +
          'Prevents the user from cancelling during the installation process.' + SNewLine +
          '/NORESTART' + SNewLine +
          'Prevents Setup from restarting the system following a successful installation, or after a Preparing to Install failure that requests a restart.' + SNewLine +
          '/RESTARTEXITCODE=exit code' + SNewLine +
          'Specifies a custom exit code that Setup is to return when the system needs to be restarted.' + SNewLine +
          '/CLOSEAPPLICATIONS, /NOCLOSEAPPLICATIONS' + SNewLine +
          'Instructs Setup to attempt closing applications using files that need to be updated, or prevents it from doing so.' + SNewLine +
          '/FORCECLOSEAPPLICATIONS, /FORCENOCLOSEAPPLICATIONS' + SNewLine +
          'Instructs Setup to force close when closing applications, or prevents it from doing so.' + SNewLine +
          '/LOGCLOSEAPPLICATIONS' + SNewLine +
          'Instructs Setup to create extra logging when closing applications for debugging purposes.' + SNewLine +
          '/RESTARTAPPLICATIONS, /NORESTARTAPPLICATIONS' + SNewLine +
          'Instructs Setup to attempt restarting applications, or prevents it from doing so.' + SNewLine +
          '/REDIRECTIONGUARD, /NOREDIRECTIONGUARD' + SNewLine +
          'Instructs Setup to attempt enabling RedirectionGuard, or prevents it from doing so.' + SNewLine +
          '/LOADINF="filename", /SAVEINF="filename"' + SNewLine +
          'Instructs Setup to load the settings from the specified file after having checked the command line, or to save them to it.' + SNewLine +
          '/LANG=language' + SNewLine +
          'Specifies the internal name of the language to use.' + SNewLine +
          '/DIR="x:\dirname"' + SNewLine +
          'Overrides the default directory name.' + SNewLine +
          '/GROUP="folder name"' + SNewLine +
          'Overrides the default folder name.' + SNewLine +
          '/NOICONS' + SNewLine +
          'Instructs Setup to initially check the Don''t create a Start Menu folder check box.' + SNewLine +
          '/TYPE=type name' + SNewLine +
          'Overrides the default setup type.' + SNewLine +
          '/COMPONENTS="comma separated list of component names"' + SNewLine +
          'Overrides the default component settings.' + SNewLine +
          '/TASKS="comma separated list of task names"' + SNewLine +
          'Specifies a list of tasks that should be initially selected.' + SNewLine +
          '/MERGETASKS="comma separated list of task names"' + SNewLine +
          'Like the /TASKS parameter, except the specified tasks will be merged with the set of tasks that would have otherwise been selected by default.' + SNewLine +
          '/PASSWORD=password' + SNewLine +
          'Specifies the password to use.' + SNewLine +
          PrNote +
          CustomNote +
          SNewLine +
          'For more detailed information, please visit https://jrsoftware.org/ishelp/index.php?topic=setupcmdline';

  MessageBox(0, PChar(Help), 'Setup', MB_OK or MB_ICONSTOP);
end;

var
  SourceF, DestF: TFile;
  OffsetTable: TSetupLdrOffsetTable;
  TempDir: String;
  S: String;
  TempFile: String;
  TestID: TSetupID;
  P: Pointer;
  Reader: TCompressedBlockReader;
  I: Integer;
begin
  try
    {$IFDEF DEBUG}
    ReportMemoryLeaksOnShutdown := True;
    {$ENDIF}

    { Ensure all of SetupLdr is paged in so that in the case of a disk spanning
      install Windows will never complain when the disk/CD containing SetupLdr
      is ejected. }
    RunImageLocally(HInstance);

    var SelfFilename := NewParamStr(0);

    ProcessCommandLine(SelfFilename);

    SourceF := TFile.Create(SelfFilename, fdOpenExisting, faRead, fsRead);
    try
      var M: HMODULE := 0;
      if not PathSame(SelfFilename, NewParamStr(0)) then begin { Can only happen on DEBUG }
        M := LoadLibraryEx(PChar(SelfFilename), 0, LOAD_LIBRARY_AS_DATAFILE);
        if M = 0 then
          raise Exception.Create('LoadLibraryEx failed');
      end;
      try
        OffsetTable := GetSetupLdrOffsetTable(M)^;
      finally
        if M <> 0 then
          FreeLibrary(M);
      end;

      { Note: We don't check the OffsetTable.ID here because it would put a
        copy of the ID in the data section, and that would confuse external
        programs that search for the offset table by ID. }
      if (OffsetTable.Version <> SetupLdrOffsetTableVersion) or
         (GetCRC32(OffsetTable, SizeOf(OffsetTable) - SizeOf(OffsetTable.TableCRC)) <> OffsetTable.TableCRC) or
         (SourceF.Size < OffsetTable.TotalSize) then
        SetupCorruptError;

      SourceF.Seek(OffsetTable.Offset0);
      SourceF.ReadBuffer(TestID, SizeOf(TestID));
      if TestID <> SetupID then
        SetupCorruptError;

      var SetupEncryptionHeaderCRC: Longint;
      SourceF.Read(SetupEncryptionHeaderCRC, SizeOf(SetupEncryptionHeaderCRC));
      SourceF.Read(SetupEncryptionHeader, SizeOf(SetupEncryptionHeader));
      if SetupEncryptionHeaderCRC <> GetCRC32(SetupEncryptionHeader, SizeOf(SetupEncryptionHeader)) then
        SetupCorruptError;

      var CryptKey: TSetupEncryptionKey;
      if SetupEncryptionHeader.EncryptionUse = euFull then begin
        if InitPassword = '' then
          raise Exception.Create(SMissingPassword);
        GenerateEncryptionKey(InitPassword, SetupEncryptionHeader.KDFSalt, SetupEncryptionHeader.KDFIterations, CryptKey);
        if not TestPassword(CryptKey, SetupEncryptionHeader.BaseNonce, SetupEncryptionHeader.PasswordTest) then
          raise Exception.Create(SIncorrectPassword);
      end;

      try
        Reader := TCompressedBlockReader.Create(SourceF, TLZMA1SmallDecompressor);
        try
          if SetupEncryptionHeader.EncryptionUse = euFull then
            Reader.InitDecryption(CryptKey, SetupEncryptionHeader.BaseNonce, sccCompressedBlocks1);

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

      if InitShowHelp then begin
        { Show the command line help. }
        ShowHelp(SetupMessages[msgHelpTextNote]);
        SetupLdrExitCode := 0;
      end else begin
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
        TempDir := CreateTempDir('.tmp', IsAdminLoggedOn);
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
        LONG_PTR(OrigWndProc) := SetWindowLongPtr(SetupLdrWnd, GWLP_WNDPROC,
          LONG_PTR(@SetupLdrWndProc));

        { Now execute Setup. Use the exit code it returns as our exit code.
          The UInt32 cast prevents sign extension. Also see
          https://learn.microsoft.com/en-us/windows/win32/winprog64/interprocess-communication
          SelfFilename is passed in "final" reparsed form so that Setup won't
          have trouble accessing the file after enabling RedirectionGuard if
          there's an untrusted redirect in the path. }
        ExecAndWait(TempFile, Format('/SL5="$%x,%d,%d,',
          [UInt32(SetupLdrWnd), OffsetTable.Offset0, OffsetTable.Offset1]) +
          GetFinalFileName(SelfFilename) + '" ' + GetCmdTail, SetupLdrExitCode);

        { Synchronize our active language with Setup's, in case we need to
          display any messages below } 
        if PendingNewLanguage <> -1 then
          SetActiveLanguage(PendingNewLanguage);
      end;
    finally
      SourceF.Free;
      if TempFile <> '' then
        { Even though Setup has terminated by now, the system may still have
          the file locked for a short period of time (esp. on multiprocessor
          systems), so use DelayDeleteFile to delete it. }
        DelayDeleteFile(TempFile, 13, 50, 250);
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
      if not RestartComputer and not InitSuppressMsgBoxes then
        MessageBox(0, PChar(SetupMessages[msgErrorRestartingComputer]),
          PChar(SetupMessages[msgErrorTitle]), MB_OK or MB_ICONEXCLAMATION or
          MB_SETFOREGROUND);
    end;
  except
    ShowExceptionMsg;
  end;
  System.ExitCode := SetupLdrExitCode;
end.
