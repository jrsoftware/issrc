unit Setup.Install;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Installation procedures
}

interface

procedure PerformInstall(var Succeeded: Boolean; const ChangesEnvironment,
  ChangesAssociations: Boolean);

implementation

uses
  Windows, Messages, ShlObj, RegStr, Classes, SysUtils, Forms,
  ISSigFunc, PathFunc, SHA256,
  Shared.CommonFunc, Shared.CommonFunc.Vcl, Shared.FileClass, Shared.Int64Em,
  Shared.SetupMessageIDs, Shared.SetupTypes, Shared.Struct, Shared.VerInfoFunc,
  Compression.Base, Compression.SevenZipDLLDecoder,
  SetupLdrAndSetup.InstFunc, SetupLdrAndSetup.Messages, SetupLdrAndSetup.RedirFunc,
  Setup.DebugClient, Setup.DotNetFunc, Setup.DownloadFileFunc, Setup.InstFunc, Setup.InstFunc.Ole,
  Setup.ISSigVerifyFunc, Setup.FileExtractor, Setup.Install.HelperFunc, Setup.Helper,
  Setup.MainFunc, Setup.LoggingFunc, Setup.RegDLL, Setup.SecurityFunc,
  Setup.UninstallLog, Setup.WizardForm;

type
  PRegisterFilesListRec = ^TRegisterFilesListRec;
  TRegisterFilesListRec = record
    Filename: String;
    Is64Bit, TypeLib, NoErrorMessages: Boolean;
  end;
var
  UninstallTempExeFilename, UninstallDataFilename, UninstallMsgFilename: String;
  UninstallExeCreated: (ueNone, ueNew, ueReplaced);
  UninstallDataCreated, AppendUninstallData: Boolean;
  
procedure RecordStartInstall(const UninstLog: TUninstallLog);
var
  AppDir: String;
begin
  if shCreateAppDir in SetupHeader.Options then
    AppDir := WizardDirValue
  else
    AppDir := '';

  UninstLog.Add(utStartInstall, [GetComputerNameString, GetUserNameString,
    AppDir, GetLocalTimeAsStr], 0);
end;

procedure RecordCompiledCode(const UninstLog: TUninstallLog);
var
  LeadBytesStr, ExpandedApp, ExpandedGroup, CustomMessagesStr: String;
begin
  { Only use app if Setup creates one }
  if shCreateAppDir in SetupHeader.Options then
    ExpandedApp := ExpandConst('{app}')
  else
    ExpandedApp := '';

  try
    ExpandedGroup := ExpandConst('{group}');
  except
    { Yep, expanding "group" might fail with an exception }
    ExpandedGroup := '';
  end;

  if SetupHeader.CompiledCodeText <> '' then
    PackCustomMessagesIntoString(CustomMessagesStr);

  { Record [Code] even if empty to 'overwrite' old versions }
  UninstLog.Add(utCompiledCode, [PackCompiledCodeTextIntoString(SetupHeader.CompiledCodeText),
    LeadBytesStr, ExpandedApp, ExpandedGroup, WizardGroupValue,
    ExpandConst('{language}'), CustomMessagesStr], SetupBinVersion or Longint($80000000));
end;

procedure RegisterUninstallInfo(const UninstLog: TUninstallLog; const UninstallRegKeyBaseName: String;
  const AfterInstallFilesSize: Int64);
{ Stores uninstall information in the Registry so that the program can be
  uninstalled through the Control Panel Add/Remove Programs applet. }
const
  AdminInstallModeNames: array [Boolean] of String =
    ('non administrative', 'administrative');
  BitInstallModeNames: array [Boolean] of String =
    ('32-bit', '64-bit');
var
  RegView, OppositeRegView: TRegView;
  RegViewIs64Bit, OppositeRegViewIs64Bit: Boolean;
  RootKey, OppositeRootKey: HKEY;
  RootKeyIsHKLM, OppositeRootKeyIsHKLM: Boolean;
  SubkeyName: String;

  procedure SetStringValue(const K: HKEY; const ValueName: PChar;
    const Data: String);
  var
    ErrorCode: Longint;
  begin
    ErrorCode := RegSetValueEx(K, ValueName, 0, REG_SZ, PChar(Data),
       (Length(Data)+1)*SizeOf(Data[1]));
    if ErrorCode <> ERROR_SUCCESS then
      RegError(reRegSetValueEx, RootKey, SubkeyName, ErrorCode);
  end;

  procedure SetStringValueUnlessEmpty(const K: HKEY; const ValueName: PChar;
    const Data: String);
  begin
    if Data <> '' then
      SetStringValue(K, ValueName, Data);
  end;

  procedure SetDWordValue(const K: HKEY; const ValueName: PChar;
    const Data: DWord);
  var
    ErrorCode: Longint;
  begin
    ErrorCode := RegSetValueEx(K, ValueName, 0, REG_DWORD, @Data,
       SizeOf(Data));
    if ErrorCode <> ERROR_SUCCESS then
      RegError(reRegSetValueEx, RootKey, SubkeyName, ErrorCode);
  end;

  function GetInstallDateString: String;
  var
    ST: TSystemTime;
  begin
    GetLocalTime(ST);
    Result := Format('%.4u%.2u%.2u', [ST.wYear, ST.wMonth, ST.wDay]);
  end;

  function ExtractMajorMinorVersion(Version: String; var Major, Minor: Integer): Boolean;
  var
    P, I: Integer;
  begin
    P := Pos('.', Version);
    if P <> 0 then begin
      Val(Copy(Version, 1, P-1), Major, I);
      if I = 0 then begin
        Delete(Version, 1, P);
        P := Pos('.', Version);
        if P <> 0 then
          Val(Copy(Version, 1, P-1), Minor, I)
        else
          Val(Version, Minor, I);
      end;
    end else begin
      Val(Version, Major, I);
      Minor := 0;
    end;
    Result := I = 0;
  end;

  { Also see Main.pas }
  function ExistingInstallationAt(const RegView: TRegView; const RootKey: HKEY): Boolean;
  var
    K: HKEY;
  begin
    if RegOpenKeyExView(RegView, RootKey, PChar(SubkeyName), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      Result := True;
      RegCloseKey(K);
    end else
      Result := False;
  end;

  procedure HandleDuplicateDisplayNames(var DisplayName: String);
  const
    UninstallDisplayNameMarksUser: array [Boolean] of TSetupMessageId =
      (msgUninstallDisplayNameMarkCurrentUser, msgUninstallDisplayNameMarkAllUsers);
    UninstallDisplayNameMarksBits: array [Boolean] of TSetupMessageId =
      (msgUninstallDisplayNameMark32Bit, msgUninstallDisplayNameMark64Bit);
  var
    ExistingAtOppositeAdminInstallMode, ExistingAtOpposite64BitInstallMode: Boolean;
  begin
    { Check opposite administrative install mode. }
    ExistingAtOppositeAdminInstallMode := ExistingInstallationAt(RegView, OppositeRootKey);
    if RootKeyIsHKLM or not IsWin64 then begin
      { Opposite (HKCU) is shared for 32-bit and 64-bit so don't log bitness. Also don't log bitness on a 32-bit system. }
      LogFmt('Detected previous %s install? %s',
        [AdminInstallModeNames[OppositeRootKeyIsHKLM {always False}], SYesNo[ExistingAtOppositeAdminInstallMode]])
    end else begin
      { Opposite (HKLM) is not shared for 32-bit and 64-bit so log bitness. }
      LogFmt('Detected previous %s %s install? %s',
        [AdminInstallModeNames[OppositeRootKeyIsHKLM {always True}], BitInstallModeNames[RegViewIs64Bit], SYesNo[ExistingAtOppositeAdminInstallMode]]);
    end;

    if IsWin64 then begin
      { Check opposite 32-bit or 64-bit install mode. }
      if RootKeyIsHKLM then begin
        { HKLM is not shared for 32-bit and 64-bit so check it for opposite 32-bit or 64-bit install mode. Not checking HKCU
          since HKCU is shared for 32-bit and 64-bit mode and we already checked HKCU above. }
        ExistingAtOpposite64BitInstallMode := ExistingInstallationAt(OppositeRegView, RootKey {always HKLM});
        LogFmt('Detected previous %s %s install? %s',
          [AdminInstallModeNames[RootKeyIsHKLM {always True}], BitInstallModeNames[OppositeRegViewIs64Bit], SYesNo[ExistingAtOpposite64BitInstallMode]]);
      end else begin
        { HKCU is shared for 32-bit and 64-bit so not checking it but we do still need to check HKLM for opposite 32-bit or
          64-bit install mode since we haven't already done that. }
        ExistingAtOpposite64BitInstallMode := ExistingInstallationAt(OppositeRegView, OppositeRootKey {always HKLM});
        if ExistingAtOpposite64BitInstallMode then
          ExistingAtOppositeAdminInstallMode := True;
        LogFmt('Detected previous %s %s install? %s',
          [AdminInstallModeNames[OppositeRootKeyIsHKLM {always True}], BitInstallModeNames[OppositeRegViewIs64Bit], SYesNo[ExistingAtOpposite64BitInstallMode]]);
      end;
    end else
      ExistingAtOpposite64BitInstallMode := False;
      
    { Mark new display name if needed. Note: currently we don't attempt to mark existing display names as well. }
    if ExistingAtOppositeAdminInstallMode or ExistingAtOpposite64BitInstallMode then begin
      if ExistingAtOppositeAdminInstallMode and ExistingAtOpposite64BitInstallMode then
        DisplayName := FmtSetupMessage(msgUninstallDisplayNameMarks,
          [DisplayName, SetupMessages[UninstallDisplayNameMarksUser[RootKeyIsHKLM]],
                        SetupMessages[UninstallDisplayNameMarksBits[RegViewIs64Bit]]])
      else if ExistingAtOppositeAdminInstallMode then
        DisplayName := FmtSetupMessage(msgUninstallDisplayNameMark,
          [DisplayName, SetupMessages[UninstallDisplayNameMarksUser[RootKeyIsHKLM]]])
      else
        DisplayName := FmtSetupMessage(msgUninstallDisplayNameMark,
          [DisplayName, SetupMessages[UninstallDisplayNameMarksBits[RegViewIs64Bit]]]);
      LogFmt('Marked uninstall display name to avoid duplicate entries. New display name: %s', [DisplayName]);
    end;
  end;

var
  H2: HKEY;
  ErrorCode: Longint;
  Z: String;
  MajorVersion, MinorVersion, I: Integer;
  EstimatedSize: Integer64;
begin
  RegView := InstallDefaultRegView;
  RegViewIs64Bit := RegView = rv64Bit;
  if RegViewIs64Bit then
    OppositeRegView := rv32Bit
  else
    OppositeRegView := rv64Bit;
  OppositeRegViewIs64Bit := not RegViewIs64Bit;

  RootKey := InstallModeRootKey;
  RootKeyIsHKLM := RootKey = HKEY_LOCAL_MACHINE;
  if RootKeyIsHKLM then
    OppositeRootKey := HKEY_CURRENT_USER
  else
    OppositeRootKey := HKEY_LOCAL_MACHINE;
  OppositeRootKeyIsHKLM := not RootKeyIsHKLM;

  SubkeyName := GetUninstallRegSubkeyName(UninstallRegKeyBaseName);

  if ExistingInstallationAt(RegView, RootKey) then begin
    if RootKeyIsHKLM then begin
      { HKLM is not shared for 32-bit and 64-bit so log bitness. }
      LogFmt('Deleting uninstall key left over from previous %s %s install.',
        [AdminInstallModeNames[RootKeyIsHKLM {always True}], BitInstallModeNames[RegViewIs64Bit]]);
    end else begin
      { HKCU is shared for 32-bit and 64-bit so don't log bitness. }
      LogFmt('Deleting uninstall key left over from previous %s install.',
        [AdminInstallModeNames[RootKeyIsHKLM {always False}]])
    end;

    RegDeleteKeyIncludingSubkeys(RegView, RootKey, PChar(SubkeyName));
  end;

  LogFmt('Creating new uninstall key: %s\%s', [GetRegRootKeyName(RootKey), SubkeyName]);

  { Create uninstall key }
  ErrorCode := RegCreateKeyExView(RegView, RootKey, PChar(SubkeyName),
    0, nil, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, nil, H2, nil);
  if ErrorCode <> ERROR_SUCCESS then
    RegError(reRegCreateKeyEx, RootKey, SubkeyName, ErrorCode);
 
  try
    Log('Writing uninstall key values.');
    
    { do not localize or change any of the following strings }
    SetStringValue(H2, 'Inno Setup: Setup Version', SetupVersion);
    if shCreateAppDir in SetupHeader.Options then
      Z := WizardDirValue
    else
      Z := '';
    SetStringValue(H2, 'Inno Setup: App Path', Z);
    SetStringValueUnlessEmpty(H2, 'InstallLocation', AddBackslash(Z));
    SetStringValue(H2, 'Inno Setup: Icon Group', WizardGroupValue);
    if WizardNoIcons then
      SetDWordValue(H2, 'Inno Setup: No Icons', 1);
    SetStringValue(H2, 'Inno Setup: User', GetUserNameString);
    if WizardSetupType <> nil then begin
      SetStringValue(H2, 'Inno Setup: Setup Type', WizardSetupType.Name);
      SetStringValue(H2, 'Inno Setup: Selected Components', StringsToCommaString(WizardComponents));
      SetStringValue(H2, 'Inno Setup: Deselected Components', StringsToCommaString(WizardDeselectedComponents));
    end;
    if HasTasks then begin
      SetStringValue(H2, 'Inno Setup: Selected Tasks', StringsToCommaString(WizardTasks));
      SetStringValue(H2, 'Inno Setup: Deselected Tasks', StringsToCommaString(WizardDeselectedTasks));
    end;
    if shUserInfoPage in SetupHeader.Options then begin
      SetStringValue(H2, 'Inno Setup: User Info: Name', WizardUserInfoName);
      SetStringValue(H2, 'Inno Setup: User Info: Organization', WizardUserInfoOrg);
      SetStringValue(H2, 'Inno Setup: User Info: Serial', WizardUserInfoSerial);
    end;
    SetStringValue(H2, 'Inno Setup: Language', PSetupLanguageEntry(Entries[seLanguage][ActiveLanguage]).Name);

    if SetupHeader.UninstallDisplayName <> '' then
      Z := ExpandConst(SetupHeader.UninstallDisplayName)
    else
      Z := ExpandedAppVerName;
    HandleDuplicateDisplayNames(Z);
    { For the entry to appear in ARP, DisplayName cannot exceed 259 characters
      on Windows 2000 and later. }
    SetStringValue(H2, 'DisplayName', Copy(Z, 1, 259));
    SetStringValueUnlessEmpty(H2, 'DisplayIcon', ExpandConst(SetupHeader.UninstallDisplayIcon));
    var ExtraUninstallString: String;
    if shUninstallLogging in SetupHeader.Options then
      ExtraUninstallString := ' /LOG'
    else
      ExtraUninstallString := '';
    SetStringValue(H2, 'UninstallString', '"' + UninstallExeFilename + '"' + ExtraUninstallString);
    SetStringValue(H2, 'QuietUninstallString', '"' + UninstallExeFilename + '" /SILENT' + ExtraUninstallString);
    SetStringValueUnlessEmpty(H2, 'DisplayVersion', ExpandConst(SetupHeader.AppVersion));
    SetStringValueUnlessEmpty(H2, 'Publisher', ExpandConst(SetupHeader.AppPublisher));
    SetStringValueUnlessEmpty(H2, 'URLInfoAbout', ExpandConst(SetupHeader.AppPublisherURL));
    SetStringValueUnlessEmpty(H2, 'HelpTelephone', ExpandConst(SetupHeader.AppSupportPhone));
    SetStringValueUnlessEmpty(H2, 'HelpLink', ExpandConst(SetupHeader.AppSupportURL));
    SetStringValueUnlessEmpty(H2, 'URLUpdateInfo', ExpandConst(SetupHeader.AppUpdatesURL));
    SetStringValueUnlessEmpty(H2, 'Readme', ExpandConst(SetupHeader.AppReadmeFile));
    SetStringValueUnlessEmpty(H2, 'Contact', ExpandConst(SetupHeader.AppContact));
    SetStringValueUnlessEmpty(H2, 'Comments', ExpandConst(SetupHeader.AppComments));
    Z := ExpandConst(SetupHeader.AppModifyPath);
    if Z <> '' then
      SetStringValue(H2, 'ModifyPath', Z)
    else
      SetDWordValue(H2, 'NoModify', 1);
    SetDWordValue(H2, 'NoRepair', 1);
    SetStringValue(H2, 'InstallDate', GetInstallDateString);
    if ExtractMajorMinorVersion(ExpandConst(SetupHeader.AppVersion), MajorVersion, MinorVersion) then begin
      { Originally MSDN said to write to Major/MinorVersion, now it says to write to VersionMajor/Minor. So write to both. }
      SetDWordValue(H2, 'MajorVersion', MajorVersion);
      SetDWordValue(H2, 'MinorVersion', MinorVersion);
      SetDWordValue(H2, 'VersionMajor', MajorVersion);
      SetDWordValue(H2, 'VersionMinor', MinorVersion);
    end;
    { Note: Windows 7 (and later?) doesn't automatically calculate sizes so set EstimatedSize ourselves. }
    if SetupHeader.UninstallDisplaySize = 0 then begin
      { Estimate the size by taking the size of all files and adding any ExtraDiskSpaceRequired. }
      EstimatedSize := AfterInstallFilesSize;
      Inc6464(EstimatedSize, SetupHeader.ExtraDiskSpaceRequired);
      for I := 0 to Entries[seComponent].Count-1 do begin
        with PSetupComponentEntry(Entries[seComponent][I])^ do begin
          if ShouldProcessEntry(WizardComponents, nil, Name, '', Languages, '') then
            Inc6464(EstimatedSize, ExtraDiskSpaceRequired);
        end;
      end;
    end else
      EstimatedSize := SetupHeader.UninstallDisplaySize;
    { ARP on Windows 7 without SP1 only pays attention to the lower 6 bytes of EstimatedSize and
      throws away the rest. For example putting in $4000001 (=4GB + 1KB) displays as 1 KB.
      So we need to check for this. }
    if (Hi(NTServicePackLevel) > 0) or IsWindows8 or (EstimatedSize.Hi = 0) then begin
      Div64(EstimatedSize, 1024);
      SetDWordValue(H2, 'EstimatedSize', EstimatedSize.Lo)
    end;

    { Also see SetPreviousData in ScriptFunc.pas }
    if CodeRunner <> nil then begin
      try
        CodeRunner.RunProcedures('RegisterPreviousData', [Integer(H2)], False);
      except
        Log('RegisterPreviousData raised an exception.');
        Application.HandleException(nil);
      end;
    end;
  finally
    RegCloseKey(H2);
  end;

  UninstLog.AddReg(utRegDeleteEntireKey, RegView, RootKey,
    [SubkeyName]);
end;

type
  TMakeDirFlags = set of (mdNoUninstall, mdAlwaysUninstall, mdDeleteAfterInstall,
    mdNotifyChange);

function MakeDir(const UninstLog: TUninstallLog; const DisableFsRedir: Boolean; Dir: String;
  const Flags: TMakeDirFlags): Boolean;
{ Returns True if a new directory was created.
  Note: If DisableFsRedir is True, the mdNotifyChange flag should not be
  specified; it won't work properly. }
var
  ErrorCode: DWORD;
  UninstFlags: Longint;
begin
  Result := False;
  Dir := RemoveBackslashUnlessRoot(PathExpand(Dir));
  if PathExtractName(Dir) = '' then  { reached root? }
    Exit;
  if DirExistsRedir(DisableFsRedir, Dir) then begin
    if not(mdAlwaysUninstall in Flags) then
      Exit;
  end
  else begin
    MakeDir(UninstLog, DisableFsRedir, PathExtractDir(Dir), Flags - [mdAlwaysUninstall]);
    LogFmt('Creating directory: %s', [Dir]);
    if not CreateDirectoryRedir(DisableFsRedir, Dir) then begin
      ErrorCode := GetLastError;
      raise Exception.Create(FmtSetupMessage(msgLastErrorMessage,
        [FmtSetupMessage1(msgErrorCreatingDir, Dir), IntToStr(ErrorCode),
         Win32ErrorString(ErrorCode)]));
    end;
    Result := True;
    if mdNotifyChange in Flags then begin
      SHChangeNotify(SHCNE_MKDIR, SHCNF_PATH, PChar(Dir), nil);
      SHChangeNotify(SHCNE_UPDATEDIR, SHCNF_PATH or SHCNF_FLUSH,
        PChar(PathExtractDir(Dir)), nil);
    end;
  end;
  if mdDeleteAfterInstall in Flags then
    DeleteDirsAfterInstallList.AddObject(Dir, Pointer(Ord(DisableFsRedir)))
  else begin
    if not(mdNoUninstall in Flags) then begin
      UninstFlags := utDeleteDirOrFiles_IsDir;
      if DisableFsRedir then
        UninstFlags := UninstFlags or utDeleteDirOrFiles_DisableFsRedir;
      if mdNotifyChange in Flags then
        UninstFlags := UninstFlags or utDeleteDirOrFiles_CallChangeNotify;
      UninstLog.Add(utDeleteDirOrFiles, [Dir], UninstFlags);
    end;
  end;
end;

procedure CreateDirs(const UninstLog: TUninstallLog);
{ Creates the application's directories }

  procedure ApplyPermissions(const DisableFsRedir: Boolean;
    const Filename: String; const PermsEntry: Integer);
  var
    P: PSetupPermissionEntry;
  begin
    if PermsEntry <> -1 then begin
      LogFmt('Setting permissions on directory: %s', [Filename]);
      P := Entries[sePermission][PermsEntry];
      if not GrantPermissionOnFile(DisableFsRedir, Filename,
         TGrantPermissionEntry(Pointer(P.Permissions)^),
         Length(P.Permissions) div SizeOf(TGrantPermissionEntry)) then
        LogFmt('Failed to set permissions on directory (%d).', [GetLastError]);
    end;
  end;

  procedure ApplyNTFSCompression(const DisableFsRedir: Boolean;
    const Filename: String; const Compress: Boolean);
  begin
    if Compress then
      LogFmt('Setting NTFS compression on directory: %s', [Filename])
    else
      LogFmt('Unsetting NTFS compression on directory: %s', [Filename]);
    if not SetNTFSCompressionRedir(DisableFsRedir, Filename, Compress) then
      LogFmt('Failed to set NTFS compression state (%d).', [GetLastError]);
  end;

var
  CurDirNumber: Integer;
  Flags: TMakeDirFlags;
  N: String;
begin
  { Create main application directory }
  MakeDir(UninstLog, InstallDefaultDisableFsRedir, WizardDirValue, []);

  { Create the rest of the directories, if any }
  for CurDirNumber := 0 to Entries[seDir].Count-1 do
    with PSetupDirEntry(Entries[seDir][CurDirNumber])^ do begin
      if ShouldProcessEntry(WizardComponents, WizardTasks, Components, Tasks, Languages, Check) then begin
        DebugNotifyEntry(seDir, CurDirNumber);
        NotifyBeforeInstallEntry(BeforeInstall);
        Flags := [];
        if doUninsNeverUninstall in Options then Include(Flags, mdNoUninstall);
        if doDeleteAfterInstall in Options then Include(Flags, mdDeleteAfterInstall);
        if doUninsAlwaysUninstall in Options then Include(Flags, mdAlwaysUninstall);
        N := RemoveBackslashUnlessRoot(PathExpand(ExpandConst(DirName)));
        MakeDir(UninstLog, InstallDefaultDisableFsRedir, N, Flags);
        AddAttributesToFile(InstallDefaultDisableFsRedir, N, Attribs);
        ApplyPermissions(InstallDefaultDisableFsRedir, N, PermissionsEntry);
        if (doSetNTFSCompression in Options) or (doUnsetNTFSCompression in Options) then
          ApplyNTFSCompression(InstallDefaultDisableFsRedir, N, doSetNTFSCompression in Options);
        NotifyAfterInstallEntry(AfterInstall);
      end;
    end;
end;

procedure BindUninstallMsgDataToExe(const ExpandedAppId: String; const F: TFile);
var
  UniqueValue: TSHA256Digest;
  UninstallerMsgTail: TUninstallerMsgTail;
begin
  F.SeekToEnd;

  { First append the hash of AppId so that unins*.exe files from different
    applications won't have the same file hash. This is done to combat broken
    anti-spyware programs that catch all unins*.exe files with certain hash
    sums just because some piece of spyware was deployed with Inno Setup and
    had the unins*.exe file in its directory. }
  UniqueValue := GetSHA256OfUnicodeString(ExpandedAppId);
  F.WriteBuffer(UniqueValue, SizeOf(UniqueValue));

  UninstallerMsgTail.ID := UninstallerMsgTailID;
  UninstallerMsgTail.Offset := F.Position;
  WriteMsgData(F);
  F.WriteBuffer(UninstallerMsgTail, SizeOf(UninstallerMsgTail));
end;

type
  TOverwriteAll = (oaUnknown, oaOverwrite, oaKeep);

procedure ProcessFileEntry(const UninstLog: TUninstallLog; const ExpandedAppId: String;
  const RegisterFilesList: TList; const CurFile: PSetupFileEntry;
  const DisableFsRedir: Boolean; AExternalSourceFile, ADestFile: String;
  const FileLocationFilenames: TStringList; const AExternalSize: Int64;
  var ConfirmOverwriteOverwriteAll, PromptIfOlderOverwriteAll: TOverwriteAll;
  var WarnedPerUserFonts: Boolean; const AExternalFileDate: PFileTime);
{ Not external: AExternalSourceFile and ADestFile should be empty strings,
                FileLocationFilenames should be set, AExternalSize is unused,
                AExternalFileDate should not be set
  External    : Opposite except AExternalFileDate still not set
  Ext. Archive: Same as external except AExternalFileDate set and
                AExternalSourceFile should be set to ArchiveFindHandle as a string
  Ext. Downl. : Same as external except
                AExternalSourceFile should be set to an URL }

  procedure InstallFont(const Filename, FontName: String;
    const PerUserFont, AddToFontTableNow: Boolean; var WarnedPerUserFonts: Boolean);
  var
    RootKey, K: HKEY;
  begin
    if PerUserFont and not WindowsVersionAtLeast(10, 0, 17134) then begin
      { Per-user fonts require Windows 10 Version 1803 (10.0.17134) or newer. }
      if not WarnedPerUserFonts then begin
        Log('Failed to set value in Fonts registry key: per-user fonts are not supported by this version of Windows.');
        WarnedPerUserFonts := True;
      end;
    end else begin
      { 64-bit Windows note: The Fonts key is evidently exempt from registry
        redirection. When a 32-bit app writes to the Fonts key, it's the main
        64-bit key that is modified. (There is actually a Fonts key under
        Wow6432Node but it appears it's never used or updated.)
        Also: We don't bother with any FS redirection stuff here. I'm not sure
        it's safe to disable FS redirection when calling AddFontResource, or
        if it would even work. Users should be installing their fonts to the
        Fonts directory instead of the System directory anyway. }
      if PerUserFont then
        RootKey := HKEY_CURRENT_USER
      else
        RootKey := HKEY_LOCAL_MACHINE;
      if RegOpenKeyExView(rvDefault, RootKey, 'Software\Microsoft\Windows NT\CurrentVersion\Fonts', 0,
         KEY_SET_VALUE, K) = ERROR_SUCCESS then begin
        if RegSetValueEx(K, PChar(FontName), 0, REG_SZ, PChar(Filename),
           (Length(Filename)+1)*SizeOf(Filename[1])) <> ERROR_SUCCESS then
          Log('Failed to set value in Fonts registry key.');
        RegCloseKey(K);
      end
      else
        Log('Failed to open Fonts registry key.');
    end;
      
    if AddToFontTableNow then begin
      repeat
        { Note: AddFontResource doesn't set the thread's last error code }
        if AddFontResource(PChar(Filename)) <> 0 then begin
          SendNotifyMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
          Break;
        end;
      until AbortRetryIgnoreTaskDialogMsgBox(
              AddPeriod(FmtSetupMessage1(msgErrorFunctionFailedNoCode, 'AddFontResource')),
              [SetupMessages[msgAbortRetryIgnoreRetry], SetupMessages[msgAbortRetryIgnoreIgnore], SetupMessages[msgAbortRetryIgnoreCancel]]);
    end;
  end;

  procedure SetFileLocationFilename(const LocationEntry: Integer;
    Filename: String);
  var
    LowercaseFilename: String;
    Hash: Longint;
    I: Integer;
  begin
    Filename := PathExpand(Filename);
    LowercaseFilename := PathLowercase(Filename);
    Hash := GetCRC32(LowercaseFilename[1], Length(LowercaseFilename)*SizeOf(LowercaseFilename[1]));
    { If Filename was already associated with another LocationEntry,
      disassociate it. If we *don't* do this, then this script won't
      produce the expected result:
        [Files]
        Source: "fileA"; DestName: "file2"
        Source: "fileB"; DestName: "file2"
        Source: "fileA"; DestName: "file1"
      1. It extracts fileA under the name "file2"
      2. It extracts fileB under the name "file2"
      3. It copies file2 to file1, thinking a copy of fileA was still
         stored in file2.
    }
    for I := 0 to FileLocationFilenames.Count-1 do
      if (Longint(FileLocationFilenames.Objects[I]) = Hash) and
         (PathLowercase(FileLocationFilenames[I]) = LowercaseFilename) then begin
        FileLocationFilenames[I] := '';
        FileLocationFilenames.Objects[I] := nil;
        Break;
      end;
    FileLocationFilenames[LocationEntry] := Filename;
    FileLocationFilenames.Objects[LocationEntry] := Pointer(Hash);
  end;

  procedure ApplyPermissions(const DisableFsRedir: Boolean;
    const Filename: String; const PermsEntry: Integer);
  var
    Attr: DWORD;
    P: PSetupPermissionEntry;
  begin
    if PermsEntry <> -1 then begin
      Attr := GetFileAttributesRedir(DisableFsRedir, Filename);
      if (Attr <> INVALID_FILE_ATTRIBUTES) and (Attr and FILE_ATTRIBUTE_DIRECTORY = 0) then begin
        LogFmt('Setting permissions on file: %s', [Filename]);
        P := Entries[sePermission][PermsEntry];
        if not GrantPermissionOnFile(DisableFsRedir, Filename,
           TGrantPermissionEntry(Pointer(P.Permissions)^),
           Length(P.Permissions) div SizeOf(TGrantPermissionEntry)) then
          LogFmt('Failed to set permissions on file (%d).', [GetLastError]);
      end;
    end;
  end;

  procedure ApplyNTFSCompression(const DisableFsRedir: Boolean;
    const Filename: String; const Compress: Boolean);
  begin
    if Compress then
      LogFmt('Setting NTFS compression on file: %s', [Filename])
    else
      LogFmt('Unsetting NTFS compression on file: %s', [Filename]);
    if not SetNTFSCompressionRedir(DisableFsRedir, Filename, Compress) then
      LogFmt('Failed to set NTFS compression state (%d).', [GetLastError]);
  end;

  procedure DoHandleFailedDeleteOrMoveFileTry(const Func, TempFile, DestFile: String;
    const LastError: DWORD; var RetriesLeft: Integer; var LastOperation: String;
    var NeedsRestart, ReplaceOnRestart, DoBreak, DoContinue: Boolean);
  begin
    { Automatically retry. Wait with replace on restart until no
      retries left, unless we already know we're going to restart. }
    if ((RetriesLeft = 0) or NeedsRestart) and
       (foRestartReplace in CurFile^.Options) and IsAdmin then begin
      LogFmt('%s: The existing file appears to be in use (%d). ' +
        'Will replace on restart.', [Func, LastError]);
      LastOperation := SetupMessages[msgErrorRestartReplace];
      NeedsRestart := True;
      RestartReplace(DisableFsRedir, TempFile, DestFile);
      ReplaceOnRestart := True;
      DoBreak := True;
      DoContinue := False;
    end else if RetriesLeft > 0 then begin
      LogFmt('%s: The existing file appears to be in use (%d). ' +
        'Retrying.', [Func, LastError]);
      Dec(RetriesLeft);
      Sleep(1000);
      ProcessEvents;
      DoBreak := False;
      DoContinue := True;
    end else begin
      DoBreak := False;
      DoContinue := False;
    end;
  end;

  function AskOverwrite(const DestFile, Instruction, Caption: string; const ButtonLabels: array of String;
    const VerificationText: String; const Typ: TMsgBoxType; const Default, Overwrite: Integer;
    var OverwriteAll: TOverwriteAll): Boolean;
  var
    VerificationFlagChecked: BOOL;
  begin
    if OverwriteAll = oaKeep then
      Result := False { The user already said to keep (=not overwrite) all }
    else begin
      Result := LoggedTaskDialogMsgBox('', Instruction, DestFile + SNewLine2 + Caption, '',
        Typ, MB_YESNO, ButtonLabels, 0, True, Default, VerificationText, @VerificationFlagChecked) = Overwrite;
      if VerificationFlagChecked then begin
        if Result then
          OverwriteAll := oaOverwrite
        else
          OverwriteAll := oaKeep;
      end;
    end;
  end;

var
  ProgressUpdated: Boolean;
  LastOperation: String;
  CurFileLocation: PSetupFileLocationEntry;
  SourceFile, DestFile, TempFile, FontFilename: String;
  DestFileExists, DestFileExistedBefore, CheckedDestFileExistedBefore,
    TempFileLeftOver, AllowFileToBeDuplicated, ReplaceOnRestart, DoBreak,
    DoContinue: Boolean;
  Failed: String;
  CurFileVersionInfoValid: Boolean;
  CurFileVersionInfo, ExistingVersionInfo: TFileVersionNumbers;
  CurFileDateValid, ExistingFileDateValid: Boolean;
  IsProtectedFile, AllowTimeStampComparison: Boolean;
  DeleteFlags: Longint;
  CurFileDate, ExistingFileDate: TFileTime;
  RegisterRec: PRegisterFilesListRec;
  RetriesLeft: Integer;
  LastError: DWORD;
  DestF, SourceF: TFile;
  Flags: TMakeDirFlags;
  Overwrite, PerUserFont: Boolean;
label Retry, Skip;
begin
  Log('-- File entry --');

  CheckedDestFileExistedBefore := False;
  DestFileExistedBefore := False;  { prevent warning }

  if CurFile^.LocationEntry <> -1 then
    CurFileLocation := PSetupFileLocationEntry(Entries[seFileLocation][CurFile^.LocationEntry])
  else
    CurFileLocation := nil;

Retry:
  DestFile := '';
  TempFile := '';
  TempFileLeftOver := False;
  ProgressUpdated := False;
  var PreviousProgress := CurProgress;
  LastOperation := '';
  Failed := '';
  try
    try
      ReplaceOnRestart := False;
      DeleteFlags := 0;
      if DisableFsRedir then
        DeleteFlags := DeleteFlags or utDeleteFile_DisableFsRedir;
      if foRegisterServer in CurFile^.Options then
        DeleteFlags := DeleteFlags or utDeleteFile_RegisteredServer;
      if foRegisterTypeLib in CurFile^.Options then
        DeleteFlags := DeleteFlags or utDeleteFile_RegisteredTypeLib;
      if foUninsRestartDelete in CurFile^.Options then
        DeleteFlags := DeleteFlags or utDeleteFile_RestartDelete;
      if foUninsRemoveReadOnly in CurFile^.Options then
        DeleteFlags := DeleteFlags or utDeleteFile_RemoveReadOnly;
      if foGacInstall in CurFile^.Options then
        DeleteFlags := DeleteFlags or utDeleteFile_GacInstalled;
      FontFilename := '';

      { Determine the destination filename }
      try
        case CurFile^.FileType of
          ftUninstExe: DestFile := UninstallExeFilename;
        else
          if ADestFile = '' then
            DestFile := ExpandConst(CurFile^.DestName)
          else
            DestFile := ADestFile;
        end;
        DestFile := PathExpand(DestFile);
      except
        { If an exception occurred, reset DestFile back to an empty string
          so the error message doesn't show an unexpanded name }
        DestFile := '';
        raise;
      end;

      { Update the status and filename labels }
      if foDownload in CurFile^.Options then
        SetStatusLabelText(SetupMessages[msgStatusDownloadFiles], False)
      else
        SetStatusLabelText(SetupMessages[msgStatusExtractFiles], False);
      SetFilenameLabelText(DestFile, True);
      LogFmt('Dest filename: %s', [DestFile]);
      if DisableFsRedir <> InstallDefaultDisableFsRedir then begin
        if DisableFsRedir then
          Log('Non-default bitness: 64-bit')
        else
          Log('Non-default bitness: 32-bit');
      end;

      { See if it's a protected system file.  }
      if IsProtectedSystemFile(DisableFsRedir, DestFile) then begin
        Log('Dest file is protected by Windows File Protection.');
        IsProtectedFile := (CurFile^.FileType = ftUserFile);
      end else
        IsProtectedFile := False;

      DestFileExists := NewFileExistsRedir(DisableFsRedir, DestFile);
      if not CheckedDestFileExistedBefore then begin
        DestFileExistedBefore := DestFileExists;
        CheckedDestFileExistedBefore := True;
      end;
      if DestFileExistedBefore then
        DeleteFlags := DeleteFlags or utDeleteFile_ExistedBeforeInstall;

      var CurFileDateDidRead := True; { Set to False later if needed }
      if Assigned(CurFileLocation) then begin
        if floTimeStampInUTC in CurFileLocation^.Flags then
          CurFileDate := CurFileLocation^.SourceTimeStamp
        else
          LocalFileTimeToFileTime(CurFileLocation^.SourceTimeStamp, CurFileDate);
        CurFileDateValid := True;
      end else if Assigned(AExternalFileDate) then begin
        CurFileDate := AExternalFileDate^;
        CurFileDateValid := CurFileDate.HasTime;
      end else if not(foDownload in CurFile^.Options) then
        CurFileDateValid := GetFileDateTime(DisableFsRedir, AExternalSourceFile, CurFileDate)
      else begin
        CurFileDateValid := False;
        CurFileDateDidRead := False;
      end;
      if CurFileDateValid then
        LogFmt('Time stamp of our file: %s', [FileTimeToStr(CurFileDate)])
      else if CurFileDateDidRead then
        Log('Time stamp of our file: (failed to read)');

      if DestFileExists then begin
        Log('Dest file exists.');
        if foOnlyIfDoesntExist in CurFile^.Options then begin
          Log('Skipping due to "onlyifdoesntexist" flag.');
          goto Skip;
        end;

        LastOperation := SetupMessages[msgErrorReadingExistingDest];

        ExistingFileDateValid := GetFileDateTime(DisableFsRedir, DestFile, ExistingFileDate);
        if ExistingFileDateValid then
          LogFmt('Time stamp of existing file: %s', [FileTimeToStr(ExistingFileDate)])
        else
          Log('Time stamp of existing file: (failed to read)');

        { Compare version info }
        if not(foIgnoreVersion in CurFile^.Options) then begin
          AllowTimeStampComparison := False;
          { Read version info of file being installed }
          if foDownload in CurFile^.Options then
            InternalError('Unexpected Download flag');
          if foExtractArchive in CurFile^.Options then
            InternalError('Unexpected ExtractArchive flag');
          if Assigned(CurFileLocation) then begin
            CurFileVersionInfoValid := floVersionInfoValid in CurFileLocation^.Flags;
            CurFileVersionInfo.MS := CurFileLocation^.FileVersionMS;
            CurFileVersionInfo.LS := CurFileLocation^.FileVersionLS;
          end
          else
            CurFileVersionInfoValid := GetVersionNumbersRedir(DisableFsRedir,
                PathExpand(AExternalSourceFile), CurFileVersionInfo);
          if CurFileVersionInfoValid then
            LogFmt('Version of our file: %u.%u.%u.%u',
              [LongRec(CurFileVersionInfo.MS).Hi, LongRec(CurFileVersionInfo.MS).Lo,
               LongRec(CurFileVersionInfo.LS).Hi, LongRec(CurFileVersionInfo.LS).Lo])
          else
            Log('Version of our file: (none)');
          { Does the existing file have version info? }
          if GetVersionNumbersRedir(DisableFsRedir, PathExpand(DestFile), ExistingVersionInfo) then begin
            { If the file being installed has no version info, or the existing
              file is a newer version... }
            LogFmt('Version of existing file: %u.%u.%u.%u',
              [LongRec(ExistingVersionInfo.MS).Hi, LongRec(ExistingVersionInfo.MS).Lo,
               LongRec(ExistingVersionInfo.LS).Hi, LongRec(ExistingVersionInfo.LS).Lo]);
            if not CurFileVersionInfoValid or
               ((ExistingVersionInfo.MS > CurFileVersionInfo.MS) or
                ((ExistingVersionInfo.MS = CurFileVersionInfo.MS) and
                 (ExistingVersionInfo.LS > CurFileVersionInfo.LS))) then begin
              { No version info, or existing file is newer, ask user what to do unless we shouldn't }
              if (foPromptIfOlder in CurFile^.Options) and not IsProtectedFile then begin
                if PromptIfOlderOverwriteAll <> oaOverwrite then begin
                  Overwrite := AskOverwrite(DestFile, SetupMessages[msgExistingFileNewerSelectAction],
                    SetupMessages[msgExistingFileNewer2],
                    [SetupMessages[msgExistingFileNewerKeepExisting], SetupMessages[msgExistingFileNewerOverwriteExisting]],
                    SetupMessages[msgExistingFileNewerOverwriteOrKeepAll],
                   mbError, IDYES, IDNO, PromptIfOlderOverwriteAll);
                  if not Overwrite then begin
                    Log('User opted not to overwrite the existing file. Skipping.');
                    goto Skip;
                  end;
                end;
              end else begin
                Log('Existing file is a newer version. Skipping.');
                goto Skip;
              end;
            end
            else begin
              { If the existing file and the file being installed are the same
                version... }
              if (ExistingVersionInfo.MS = CurFileVersionInfo.MS) and
                 (ExistingVersionInfo.LS = CurFileVersionInfo.LS) and
                 not(foOverwriteSameVersion in CurFile^.Options) then begin
                if foReplaceSameVersionIfContentsDiffer in CurFile^.Options then begin
                  { Get the two files' SHA-256 hashes and compare them }
                  var ExistingFileHash: TSHA256Digest;
                  if TryToGetSHA256OfFile(DisableFsRedir, DestFile, ExistingFileHash) then begin
                    var CurFileHash: TSHA256Digest;
                    if Assigned(CurFileLocation) then
                      CurFileHash := CurFileLocation^.SHA256Sum
                    else begin
                      LastOperation := SetupMessages[msgErrorReadingSource];
                      { This GetSHA256OfFile call could raise an exception, but
                        it's very unlikely since we were already able to
                        successfully read the file's version info. }
                      CurFileHash := GetSHA256OfFile(DisableFsRedir, AExternalSourceFile);
                      LastOperation := SetupMessages[msgErrorReadingExistingDest];
                    end;
                    { If the two files' SHA-256 hashes are equal, skip the file }
                    if SHA256DigestsEqual(ExistingFileHash, CurFileHash) then begin
                      Log('Existing file''s SHA-256 hash matches our file. Skipping.');
                      goto Skip;
                    end;
                    Log('Existing file''s SHA-256 hash is different from our file. Proceeding.');
                  end
                  else
                    Log('Failed to read existing file''s SHA-256 hash. Proceeding.');
                end
                else begin
                  { Skip the file or fall back to time stamp comparison }
                  if not(foCompareTimeStamp in CurFile^.Options) then begin
                    Log('Same version. Skipping.');
                    goto Skip;
                  end;
                  AllowTimeStampComparison := True;
                end;
              end;
            end;
          end
          else begin
            Log('Version of existing file: (none)');
            { If neither the existing file nor our file have version info,
              allow time stamp comparison }
            if not CurFileVersionInfoValid then
              AllowTimeStampComparison := True;
          end;
        end
        else begin
          { When foIgnoreVersion is in Options, always allow time stamp
            comparison }
          AllowTimeStampComparison := True;
        end;

        { Fall back to comparing time stamps if needed }
        if AllowTimeStampComparison and
           (foCompareTimeStamp in CurFile^.Options) then begin
          if foDownload in CurFile^.Options then
            InternalError('Unexpected Download flag');
          if not CurFileDateValid or not ExistingFileDateValid then begin
            { If we failed to read one of the time stamps, do the safe thing
              and just skip the file }
            Log('Couldn''t read time stamp. Skipping.');
            goto Skip;
          end;
          if CompareFileTime(ExistingFileDate, CurFileDate) = 0 then begin
            { Same time stamp }
            Log('Same time stamp. Skipping.');
            goto Skip;
          end;
          if CompareFileTime(ExistingFileDate, CurFileDate) > 0 then begin
            { Existing file has a later time stamp, ask user what to do unless we shouldn't }
            if (foPromptIfOlder in CurFile^.Options) and not IsProtectedFile then begin
              if PromptIfOlderOverwriteAll <> oaOverwrite then begin
                Overwrite := AskOverwrite(DestFile, SetupMessages[msgExistingFileNewerSelectAction],
                  SetupMessages[msgExistingFileNewer2],
                  [SetupMessages[msgExistingFileNewerKeepExisting], SetupMessages[msgExistingFileNewerOverwriteExisting]],
                  SetupMessages[msgExistingFileNewerOverwriteOrKeepAll],
                  mbError, IDYES, IDNO, PromptIfOlderOverwriteAll);
                if not Overwrite then begin
                  Log('User opted not to overwrite the existing file. Skipping.');
                  goto Skip;
                end;
              end;
            end else begin
              Log('Existing file has a later time stamp. Skipping.');
              goto Skip;
            end;
          end;
        end;

        LastOperation := '';

        { Don't attempt to replace an existing protected system file.
          (Do this *after* the version numbers of the new & existing files
          have been logged.) }
        if IsProtectedFile then begin
          Log('Existing file is protected by Windows File Protection. Skipping.');
          goto Skip;
        end;

        { If file already exists and foConfirmOverwrite is in Options, ask the user what to do }
        if foConfirmOverwrite in CurFile^.Options then begin
          if ConfirmOverwriteOverwriteAll <> oaOverwrite then begin
            Overwrite := AskOverwrite(DestFile, SetupMessages[msgFileExistsSelectAction],
              SetupMessages[msgFileExists2],
              [SetupMessages[msgFileExistsOverwriteExisting], SetupMessages[msgFileExistsKeepExisting]],
              SetupMessages[msgFileExistsOverwriteOrKeepAll],
              mbConfirmation, IDNO, IDYES, ConfirmOverwriteOverwriteAll);
            if not Overwrite then begin
              Log('User opted not to overwrite the existing file. Skipping.');
              goto Skip;
            end;
          end;
        end;

        { Check if existing file is read-only }
        while True do begin
          var ExistingFileAttr := GetFileAttributesRedir(DisableFsRedir, DestFile);
          if (ExistingFileAttr <> INVALID_FILE_ATTRIBUTES) and
             (ExistingFileAttr and FILE_ATTRIBUTE_READONLY <> 0) then begin
            if not(foOverwriteReadOnly in CurFile^.Options) and
               AbortRetryIgnoreTaskDialogMsgBox(
                 DestFile + SNewLine2 + SetupMessages[msgExistingFileReadOnly2],
                 [SetupMessages[msgExistingFileReadOnlyRetry], SetupMessages[msgExistingFileReadOnlyKeepExisting], SetupMessages[msgAbortRetryIgnoreCancel]]) then begin
              Log('User opted not to strip the existing file''s read-only attribute. Skipping.');
              goto Skip;
            end;
            LastOperation := SetupMessages[msgErrorChangingAttr];
            if SetFileAttributesRedir(DisableFsRedir, DestFile,
               ExistingFileAttr and not FILE_ATTRIBUTE_READONLY) then
              Log('Stripped read-only attribute.')
            else
              Log('Failed to strip read-only attribute.');
            if foOverwriteReadOnly in CurFile^.Options then
              Break;  { don't retry }
          end
          else
            Break;
        end;
      end
      else begin
        if (foOnlyIfDestFileExists in CurFile^.Options) and not DestFileExistedBefore then begin
          Log('Skipping due to "onlyifdestfileexists" flag.');
          goto Skip;
        end;
      end;

      Log('Installing the file.');

      { Locate source file }
      SourceFile := AExternalSourceFile; { Empty string if not external }
      if DisableFsRedir = InstallDefaultDisableFsRedir then begin
        { If the file is compressed in the setup package, has the same file
          already been copied somewhere else? If so, just make a duplicate of
          that file instead of extracting it over again. }
        if (SourceFile = '') and (FileLocationFilenames <> nil) and
           (FileLocationFilenames[CurFile^.LocationEntry] <> '') and
           NewFileExistsRedir(DisableFsRedir, FileLocationFilenames[CurFile^.LocationEntry]) then
          SourceFile := FileLocationFilenames[CurFile^.LocationEntry];
        AllowFileToBeDuplicated := (SourceFile = '');
      end
      else begin
        { This file uses a non-default FS redirection setting. Files in
          FileLocationFilenames are assumed to have been installed with the
          default FS redirection setting, so we can't use a file in
          FileLocationFilenames as the source, or put this file there. }
        AllowFileToBeDuplicated := False;
      end;

      { Download or extract or copy the file to a temporary file. Create the destination
        file's directory if it didn't already exist. }
      LastOperation := SetupMessages[msgErrorCreatingTemp];
      TempFile := GenerateUniqueName(DisableFsRedir, PathExtractPath(DestFile), '.tmp');
      Flags := [];
      if foUninsNeverUninstall in CurFile^.Options then Include(Flags, mdNoUninstall);
      if foDeleteAfterInstall in CurFile^.Options then Include(Flags, mdDeleteAfterInstall);
      MakeDir(UninstLog, DisableFsRedir, PathExtractDir(TempFile), Flags);
      DestF := TFileRedir.Create(DisableFsRedir, TempFile, fdCreateAlways, faReadWrite, fsNone);
      try
        TempFileLeftOver := True;
        try
          ProgressUpdated := True;
          LastOperation := SetupMessages[msgErrorReadingSource];
          if SourceFile = '' then begin
            { Decompress a file }
            FileExtractor.SeekTo(CurFileLocation^, InternalProgressProc);
            LastOperation := SetupMessages[msgErrorCopying];
            FileExtractor.DecompressFile(CurFileLocation^, DestF, InternalProgressProc,
              not (foDontVerifyChecksum in CurFile^.Options));
          end
          else if foExtractArchive in CurFile^.Options then begin
            { Extract a file from archive. Note: ISSigVerify for archive has
              already been handled by RecurseExternalArchiveCopyFiles. }
            LastOperation := SetupMessages[msgErrorExtracting];
            var MaxProgress := CurProgress;
            Inc(MaxProgress, AExternalSize);
            ArchiveFindExtract(StrToInt(SourceFile), DestF, ExternalProgressProc64, MaxProgress);
          end
          else if foDownload in CurFile^.Options then begin
            { Download a file with or without ISSigVerify. Note: estimate of
              extra .issig size has already been added to CurFile's ExternalSize. }
            LastOperation := SetupMessages[msgErrorDownloading];
            const DownloadUserName = ExpandConst(CurFile^.DownloadUserName);
            const DownloadPassword = ExpandConst(CurFile^.DownloadPassword);
            var MaxProgress := CurProgress;
            Inc(MaxProgress, AExternalSize);
            if CurFile^.Verification.Typ = fvISSig then begin
              const ISSigTempFile = TempFile + ISSigExt;
              const ISSigDestF = TFileRedir.Create(DisableFsRedir, ISSigTempFile, fdCreateAlways, faReadWrite, fsNone);
              try
                { Download the .issig file }
                const ISSigUrl = GetISSigUrl(SourceFile, ExpandConst(CurFile^.DownloadISSigSource));
                DownloadFile(ISSigUrl, DownloadUserName, DownloadPassword,
                  ISSigDestF, NoVerification, '', JustProcessEventsProc64, 0);
                FreeAndNil(ISSigDestF);
                { Download and verify the actual file }
                DownloadFile(SourceFile, DownloadUserName, DownloadPassword,
                  DestF, CurFile^.Verification, TempFile, ExternalProgressProc64, MaxProgress);
              finally
                ISSigDestF.Free;
                { Delete the .issig file }
                DeleteFileRedir(DisableFsRedir, ISSigTempFile);
              end;
            end else
              DownloadFile(SourceFile, DownloadUserName, DownloadPassword,
                DestF, CurFile^.Verification, '', ExternalProgressProc64, MaxProgress);
          end
          else begin
            { Copy a duplicated non-external file, or an external file }
            SourceF := TFileRedir.Create(DisableFsRedir, SourceFile, fdOpenExisting, faRead, fsRead);
            try
              LastOperation := SetupMessages[msgErrorCopying];
              if Assigned(CurFileLocation) then
                CopySourceFileToDestFile(SourceF, DestF, NoVerification,
                  '', CurFileLocation^.OriginalSize)
              else
                CopySourceFileToDestFile(SourceF, DestF, CurFile^.Verification,
                  SourceFile, AExternalSize);
            finally
              SourceF.Free;
            end;
          end;
        except
          { If an exception occurred, put progress meter back to where it was }
          ProgressUpdated := False;
          SetProgress(PreviousProgress);
          raise;
        end;
        { Set time/date stamp }
        if CurFileDateValid then
          SetFileTime(DestF.Handle, nil, nil, @CurFileDate);
        { If it's the uninstall program, bind the messages }
        if CurFile^.FileType = ftUninstExe then begin
          AllowFileToBeDuplicated := False;
          MarkExeHeader(DestF, SetupExeModeUninstaller);
          if not(shSignedUninstaller in SetupHeader.Options) and
             not DetachedUninstMsgFile then
            BindUninstallMsgDataToExe(ExpandedAppId, DestF);
        end;
      finally
        DestF.Free;
      end;

      { If it's a font, unregister the existing one to ensure that Windows
        'notices' the file is being replaced, and to increase the chances
        of the file being unlocked/closed before we replace it. }
      if CurFile^.InstallFontName <> '' then begin
        LastOperation := '';
        FontFilename := ShortenOrExpandFontFilename(DestFile);
        if DestFileExistedBefore then
          RemoveFontResource(PChar(FontFilename));
      end;

      { Delete existing version of file, if any. If it can't be deleted
        because it's in use and the "restartreplace" flag was specified
        on the entry, register it to be replaced when the system is
        restarted. Do retry deletion before doing this. }
      if DestFileExists and (CurFile^.FileType <> ftUninstExe) then begin
        LastOperation := SetupMessages[msgErrorReplacingExistingFile];
        RetriesLeft := 4;
        while not DeleteFileRedir(DisableFsRedir, DestFile) do begin
          { Couldn't delete the existing file... }
          LastError := GetLastError;
          { If the file inexplicably vanished, it's not a problem }
          if LastError = ERROR_FILE_NOT_FOUND then
            Break;
          { Does the error code indicate that it is possibly in use? }
          if LastErrorIndicatesPossiblyInUse(LastError, False) then begin
            DoHandleFailedDeleteOrMoveFileTry('DeleteFile', TempFile, DestFile,
              LastError, RetriesLeft, LastOperation, NeedsRestart, ReplaceOnRestart,
              DoBreak, DoContinue);
            if DoBreak then
              Break
            else if DoContinue then
              Continue;
          end;
          { Some other error occurred, or we ran out of tries }
          SetLastError(LastError);
          Win32ErrorMsg('DeleteFile');
        end;
      end;

      { Rename the temporary file to the new name now, unless the file is
        to be replaced when the system is restarted, or if the file is the
        uninstall program and an existing uninstall program already exists.
        If it can't be renamed and the "restartreplace" flag was specified
        on the entry, register it to be replaced when the system is
        restarted. Do retry renaming before doing this. }
      if not (ReplaceOnRestart or
              ((CurFile^.FileType = ftUninstExe) and DestFileExistedBefore)) then begin
        LastOperation := SetupMessages[msgErrorRenamingTemp];
        { Since the DeleteFile above succeeded you would expect the rename to
          also always succeed, but if it doesn't retry anyway. }
        RetriesLeft := 4;
        while not MoveFileRedir(DisableFsRedir, TempFile, DestFile) do begin
          { Couldn't rename the temporary file... }
          LastError := GetLastError;
          { Does the error code indicate that it is possibly in use? }
          if LastErrorIndicatesPossiblyInUse(LastError, True) then begin
            DoHandleFailedDeleteOrMoveFileTry('MoveFile', TempFile, DestFile,
              LastError, RetriesLeft, LastOperation, NeedsRestart, ReplaceOnRestart,
              DoBreak, DoContinue);
            if DoBreak then
              Break
            else if DoContinue then
              Continue;
          end;
          { Some other error occurred, or we ran out of tries }
          SetLastError(LastError);
          Win32ErrorMsg('MoveFile'); { Throws an exception }
        end;

        { If ReplaceOnRestart is still False the rename succeeded so handle this.
          Then set any file attributes. }
        if not ReplaceOnRestart then begin
          TempFileLeftOver := False;
          TempFile := '';
          LastOperation := '';
          Log('Successfully installed the file.');
          if AllowFileToBeDuplicated then
            SetFileLocationFilename(CurFile^.LocationEntry, DestFile);
          if foDeleteAfterInstall in CurFile^.Options then
            DeleteFilesAfterInstallList.AddObject(DestFile, Pointer(Ord(DisableFsRedir)));
          { Set file attributes *after* renaming the file since Novell
            reportedly can't rename read-only files. }
          AddAttributesToFile(DisableFsRedir, DestFile, CurFile^.Attribs);
        end;
      end;

      { Leave the temporary file in place for now if the file is to be
        replaced when the system is restarted, or if the file is the uninstall
        program and an existing uninstall program already exists. }
      if ReplaceOnRestart or
         ((CurFile^.FileType = ftUninstExe) and DestFileExistedBefore) then begin
        if CurFile^.FileType = ftUninstExe then
          UninstallTempExeFilename := TempFile;
        TempFileLeftOver := False;
        LastOperation := '';
        Log('Leaving temporary file in place for now.');
        if AllowFileToBeDuplicated then
          SetFileLocationFilename(CurFile^.LocationEntry, TempFile);
        AddAttributesToFile(DisableFsRedir, TempFile, CurFile^.Attribs);
      end;

      { If it's a font, register it }
      if CurFile^.InstallFontName <> '' then begin
        LastOperation := '';
        LogFmt('Registering file as a font ("%s")', [CurFile^.InstallFontName]);
        PerUserFont := not IsAdminInstallMode;
        InstallFont(FontFilename, CurFile^.InstallFontName, PerUserFont, not ReplaceOnRestart, WarnedPerUserFonts);
        DeleteFlags := DeleteFlags or utDeleteFile_IsFont;
        if PerUserFont then
          DeleteFlags := DeleteFlags or utDeleteFile_PerUserFont;
      end;

      { There were no errors so add the uninstall log entry, unless the file
        is the uninstall program, or if it has the foSharedFile flag; shared
        files are handled below. }
      LastOperation := '';
      if CurFile^.FileType <> ftUninstExe then begin
        if not(foUninsNeverUninstall in CurFile^.Options) and
           not(foSharedFile in CurFile^.Options) then begin
          UninstLog.Add(utDeleteFile, [DestFile, TempFile,
            CurFile^.InstallFontName, FontFilename,
            CurFile^.StrongAssemblyName], DeleteFlags);
        end;
      end
      else begin
        if UninstallTempExeFilename = '' then
          UninstallExeCreated := ueNew
        else
          UninstallExeCreated := ueReplaced;
      end;

    Skip:
      { If foRegisterServer or foRegisterTypeLib is in Options, add the
        file to RegisterFilesList for registering later.
        Don't attempt to register if the file doesn't exist (which can
        happen if the foOnlyIfDestFileExists flag is used). }
      if ((foRegisterServer in CurFile^.Options) or
          (foRegisterTypeLib in CurFile^.Options)) and
         NewFileExistsRedir(DisableFsRedir, DestFile) then begin
        LastOperation := '';
        if foRegisterTypeLib in CurFile^.Options then
          Log('Will register the file (a type library) later.')
        else
          Log('Will register the file (a DLL/OCX) later.');
        New(RegisterRec);
        RegisterRec^.Filename := DestFile;
        RegisterRec^.Is64Bit := DisableFsRedir;
        RegisterRec^.TypeLib := foRegisterTypeLib in CurFile^.Options;
        RegisterRec^.NoErrorMessages := foNoRegError in CurFile^.Options;
        RegisterFilesList.Add(RegisterRec);
      end;

      { If foSharedFile is in Options, increment the reference count in the
        registry for the file, then add the uninstall log entry (which,
        unlike non-shared files, must be done on skipped files as well;
        that's why there are two places where utDeleteFile entries are
        added). }
      if foSharedFile in CurFile^.Options then begin
        LastOperation := '';
        if DisableFsRedir then begin
          Log('Incrementing shared file count (64-bit).');
          IncrementSharedCount(rv64Bit, DestFile, DestFileExistedBefore);
        end
        else begin
          Log('Incrementing shared file count (32-bit).');
          IncrementSharedCount(rv32Bit, DestFile, DestFileExistedBefore);
        end;
        if not(foUninsNeverUninstall in CurFile^.Options) then begin
          DeleteFlags := DeleteFlags or utDeleteFile_SharedFile;
          if DisableFsRedir then
            DeleteFlags := DeleteFlags or utDeleteFile_SharedFileIn64BitKey;
          if foUninsNoSharedFilePrompt in CurFile^.Options then
            DeleteFlags := DeleteFlags or utDeleteFile_NoSharedFilePrompt;
          UninstLog.Add(utDeleteFile, [DestFile, TempFile,
            CurFile^.InstallFontName, FontFilename,
            CurFile^.StrongAssemblyName], DeleteFlags);
        end
        else begin
          if DisableFsRedir then
            UninstLog.Add(utDecrementSharedCount, [DestFile],
              utDecrementSharedCount_64BitKey)
          else
            UninstLog.Add(utDecrementSharedCount, [DestFile], 0);
        end;
      end;

      { Apply permissions (even if the file wasn't replaced) }
      LastOperation := '';
      if TempFile <> '' then
        ApplyPermissions(DisableFsRedir, TempFile, CurFile^.PermissionsEntry)
      else
        ApplyPermissions(DisableFsRedir, DestFile, CurFile^.PermissionsEntry);

      { Set NTFS compression (even if the file wasn't replaced) }
      if (foSetNTFSCompression in CurFile^.Options) or (foUnsetNTFSCompression in CurFile^.Options) then begin
        LastOperation := '';
        if TempFile <> '' then
          ApplyNTFSCompression(DisableFsRedir, TempFile, foSetNTFSCompression in CurFile^.Options)
        else
          ApplyNTFSCompression(DisableFsRedir, DestFile, foSetNTFSCompression in CurFile^.Options);
      end;

      { Install into GAC (even if the file wasn't replaced) }
      if foGacInstall in CurFile^.Options then begin
        Log('Installing into GAC');
        with TAssemblyCacheInfo.Create(rvDefault) do try
          if TempFile <> '' then
            InstallAssembly(TempFile)
          else
            InstallAssembly(DestFile);
        finally
          Free;
        end;
      end;
    except
      if ExceptObject is EAbort then
        raise;
      Failed := GetExceptMessage;
    end;
  finally
    { If an exception occurred before TempFile was cleaned up, delete it now }
    if TempFileLeftOver then
      DeleteFileRedir(DisableFsRedir, TempFile);
  end;

  { Was there an exception? Display error message and offer to retry }
  if Failed <> '' then begin
    if (CurFile^.FileType = ftUninstExe) and (UninstallTempExeFilename <> '') then begin
      DeleteFile(UninstallTempExeFilename);
      UninstallTempExeFilename := '';
      UninstallExeCreated := ueNone;
    end;

    if LastOperation <> '' then
      LastOperation := LastOperation + SNewLine;
    if not AbortRetryIgnoreTaskDialogMsgBox(
             DestFile + SNewLine2 + LastOperation + Failed,
             [SetupMessages[msgAbortRetryIgnoreRetry], SetupMessages[msgFileAbortRetryIgnoreSkipNotRecommended], SetupMessages[msgAbortRetryIgnoreCancel]]) then begin
      if ProgressUpdated then
        SetProgress(PreviousProgress);
      goto Retry;
    end;
  end;

  { Increment progress meter, if not already done so }
  if not ProgressUpdated then begin
    if Assigned(CurFileLocation) then  { not an "external" file }
      IncProgress(CurFileLocation^.OriginalSize)
    else
      IncProgress(AExternalSize);
  end;

  { Process any events between copying files }
  ProcessEvents;
  { Clear previous filename label in case an exception or debugger break
    occurs between now and when the label for the next entry is set }
  SetFilenameLabelText('', False);
end;

procedure CopyFiles(const UninstLog: TUninstallLog; const ExpandedAppId: String;
  const RegisterFilesList: TList; Uninstallable: Boolean);
{ Copies all the application's files }

  function RecurseExternalCopyFiles(const DisableFsRedir: Boolean;
    const SearchBaseDir, SearchSubDir, SearchWildcard: String; const SourceIsWildcard: Boolean;
    const Excludes: TStrings; const CurFile: PSetupFileEntry; var ExpectedBytesLeft: Int64;
    var ConfirmOverwriteOverwriteAll, PromptIfOlderOverwriteAll: TOverwriteAll;
    var WarnedPerUserFonts: Boolean): Boolean;
  begin
    { Also see RecurseExternalFiles and RecurseExternalGetSizeOfFiles in Setup.MainFunc
      Also see RecurseExternalArchiveCopyFiles directly below }

    Result := False;

    var FindData: TWin32FindData;
    var H := FindFirstFileRedir(DisableFsRedir, SearchBaseDir + SearchSubDir + SearchWildcard, FindData);
    if H <> INVALID_HANDLE_VALUE then begin
      try
        repeat
          if FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then begin

            var FileName: String;
            if SourceIsWildcard then begin
              if FindData.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN <> 0 then
                Continue;
              FileName := FindData.cFileName;
            end
            else
              FileName := SearchWildcard;  { use the case specified in the script }

            if IsExcluded(SearchSubDir + FileName, Excludes) then
              Continue;

            Result := True;
            var SourceFile := SearchBaseDir + SearchSubDir + FileName;
            { Note: CurFile^.DestName only includes a a filename if foCustomDestName is set,
              see TSetupCompiler.EnumFilesProc.ProcessFileList }
            var DestFile := ExpandConst(CurFile^.DestName);
            if not(foCustomDestName in CurFile^.Options) then
              DestFile := DestFile + SearchSubDir + FileName
            else if SearchSubDir <> '' then
              DestFile := PathExtractPath(DestFile) + SearchSubDir + PathExtractName(DestFile);
            var Size: Integer64;
            Size.Hi := FindData.nFileSizeHigh;
            Size.Lo := FindData.nFileSizeLow;
            if Compare64(Size, ExpectedBytesLeft) > 0 then begin
              { Don't allow the progress bar to overflow if the size of the
                files is greater than when we last checked }
              Size := ExpectedBytesLeft;
            end;
            ProcessFileEntry(UninstLog, ExpandedAppId, RegisterFilesList,
              CurFile, DisableFsRedir, SourceFile, DestFile, nil,
              Size, ConfirmOverwriteOverwriteAll, PromptIfOlderOverwriteAll,
              WarnedPerUserFonts, nil);
            Dec(ExpectedBytesLeft, Size);
          end;
        until not FindNextFile(H, FindData);
      finally
        Windows.FindClose(H);
      end;
    end;

    if foRecurseSubDirsExternal in CurFile^.Options then begin
      H := FindFirstFileRedir(DisableFsRedir, SearchBaseDir + SearchSubDir + '*', FindData);
      if H <> INVALID_HANDLE_VALUE then begin
        try
          repeat
            if IsRecurseableDirectory(FindData) then
              Result := RecurseExternalCopyFiles(DisableFsRedir, SearchBaseDir,
                SearchSubDir + FindData.cFileName + '\', SearchWildcard,
                SourceIsWildcard, Excludes, CurFile, ExpectedBytesLeft,
                ConfirmOverwriteOverwriteAll, PromptIfOlderOverwriteAll,
                WarnedPerUserFonts) or Result;
          until not FindNextFile(H, FindData);
        finally
          Windows.FindClose(H);
        end;
      end;
    end;

    if SearchSubDir <> '' then begin
      { If Result is False this subdir won't be created, so create it now if
        CreateAllSubDirs was set }
      if not Result and (foCreateAllSubDirs in CurFile.Options) then begin
        var DestName := ExpandConst(CurFile^.DestName); { See above }
        if not(foCustomDestName in CurFile^.Options) then
          DestName := DestName + SearchSubDir
        else
          DestName := PathExtractPath(DestName) + SearchSubDir;
        var Flags: TMakeDirFlags := [];
        if foUninsNeverUninstall in CurFile^.Options then Include(Flags, mdNoUninstall);
        if foDeleteAfterInstall in CurFile^.Options then Include(Flags, mdDeleteAfterInstall);
        MakeDir(UninstLog, DisableFsRedir, DestName, Flags);
        Result := True;
      end;
    end;

    { When recursively searching but not picking up every file, we could
      be frozen for a long time when installing from a network. Calling
      ProcessEvents after every directory helps. }
    ProcessEvents;
  end;

  function RecurseExternalArchiveCopyFiles(const DisableFsRedir: Boolean;
    const ArchiveFilename: String; const Excludes: TStrings;
    const CurFile: PSetupFileEntry; var ExpectedBytesLeft: Int64;
    var ConfirmOverwriteOverwriteAll, PromptIfOlderOverwriteAll: TOverwriteAll;
    var WarnedPerUserFonts: Boolean): Boolean;
  begin
    { See above }

    { If the archive doesn't exist then the caller should handle this with
      a msgSourceDoesntExist message. All other errors we handle ourselves
      with a msgErrorExtracting message, without informing the caller, unless
      you count EAbort. }
    Result := NewFileExistsRedir(DisableFsRedir, ArchiveFilename);
    if not Result then
      Exit;

    if foCustomDestName in CurFile^.Options then
      InternalError('Unexpected custom DestName');
    const DestDir = ExpandConst(CurFile^.DestName);

    Log('-- Archive entry --');

    var VerifySourceF: TFile := nil;
    try
      var FindData: TWin32FindData;
      var H: TArchiveFindHandle := INVALID_HANDLE_VALUE;
      var Failed: String;
      repeat
        try
          if CurFile^.Verification.Typ <> fvNone then begin
            if VerifySourceF = nil then
              VerifySourceF := TFileRedir.Create(DisableFsRedir, ArchiveFilename, fdOpenExisting, faRead, fsRead);
            var ExpectedFileHash: TSHA256Digest;
            if CurFile^.Verification.Typ = fvHash then
              ExpectedFileHash := CurFile^.Verification.Hash
            else begin
              DoISSigVerify(VerifySourceF, nil, ArchiveFilename, True, CurFile^.Verification.ISSigAllowedKeys,
                ExpectedFileHash);
            end;
            { Can't get the SHA-256 while extracting so need to get and check it now }
            const ActualFileHash = GetSHA256OfFile(VerifySourceF);
            if not SHA256DigestsEqual(ActualFileHash, ExpectedFileHash) then
              VerificationError(veFileHashIncorrect);
            Log(VerificationSuccessfulLogMessage);
            { Keep VerifySourceF open until extraction has completed to prevent TOCTOU problem }
          end;

          H := ArchiveFindFirstFileRedir(DisableFsRedir, ArchiveFilename, DestDir,
            ExpandConst(CurFile^.ExtractArchivePassword), foRecurseSubDirsExternal in CurFile^.Options,
            True, FindData);
          Failed := '';
        except
          if ExceptObject is EAbort then
            raise;
          Failed := GetExceptMessage;
        end;
      until (Failed = '') or
            AbortRetryIgnoreTaskDialogMsgBox(
              ArchiveFilename + SNewLine2 + SetupMessages[msgErrorExtracting] + SNewLine + Failed,
              [SetupMessages[msgAbortRetryIgnoreRetry], SetupMessages[msgFileAbortRetryIgnoreSkipNotRecommended], SetupMessages[msgAbortRetryIgnoreCancel]]);
      if H <> INVALID_HANDLE_VALUE then begin
        try
          repeat
            if FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then begin

              if IsExcluded(FindData.cFileName, Excludes) then
                Continue;

              var SourceFile := IntToStr(H);
              const DestFile = DestDir + FindData.cFileName;
              var Size: Integer64;
              Size.Hi := FindData.nFileSizeHigh;
              Size.Lo := FindData.nFileSizeLow;
              if Compare64(Size, ExpectedBytesLeft) > 0 then begin
                { Don't allow the progress bar to overflow if the size of the
                  files is greater than when we last checked }
                Size := ExpectedBytesLeft;
              end;
              ProcessFileEntry(UninstLog, ExpandedAppId, RegisterFilesList,
                CurFile, DisableFsRedir, SourceFile, DestFile,
                nil, Size, ConfirmOverwriteOverwriteAll, PromptIfOlderOverwriteAll,
                WarnedPerUserFonts, @FindData.ftLastWriteTime);
              Dec(ExpectedBytesLeft, Size);
            end else if foCreateAllSubDirs in CurFile.Options then begin
              var Flags: TMakeDirFlags := [];
              if foUninsNeverUninstall in CurFile^.Options then Include(Flags, mdNoUninstall);
              if foDeleteAfterInstall in CurFile^.Options then Include(Flags, mdDeleteAfterInstall);
              MakeDir(UninstLog, DisableFsRedir, DestDir + FindData.cFileName, Flags);
              Result := True;
            end;
          until not ArchiveFindNextFile(H, FindData);
        finally
          ArchiveFindClose(H);
        end;

        Log('Successfully extracted the archive.');
      end else
        Log('Found no files to extract.');
    finally
      VerifySourceF.Free;
    end;
  end;

var
  I: Integer;
  CurFileNumber: Integer;
  CurFile: PSetupFileEntry;
  SourceWildcard: String;
  DisableFsRedir, FoundFiles: Boolean;
  ConfirmOverwriteOverwriteAll, PromptIfOlderOverwriteAll: TOverwriteAll;
  WarnedPerUserFonts: Boolean;
begin
  ConfirmOverwriteOverwriteAll := oaUnknown;
  PromptIfOlderOverwriteAll := oaUnknown;
  WarnedPerUserFonts := False;

  var FileLocationFilenames: TStringList := nil;
  var Excludes: TStringList := nil;
  try
    FileLocationFilenames := TStringList.Create;
    for I := 0 to Entries[seFileLocation].Count-1 do
      FileLocationFilenames.Add('');

    Excludes := TStringList.Create;
    Excludes.StrictDelimiter := True;
    Excludes.Delimiter := ',';

    for CurFileNumber := 0 to Entries[seFile].Count-1 do begin
      CurFile := PSetupFileEntry(Entries[seFile][CurFileNumber]);
      if ((CurFile^.FileType <> ftUninstExe) or Uninstallable) and
         ShouldProcessFileEntry(WizardComponents, WizardTasks, CurFile, False) then begin
        DebugNotifyEntry(seFile, CurFileNumber);
        NotifyBeforeInstallFileEntry(CurFile);

        DisableFsRedir := InstallDefaultDisableFsRedir;
        if fo32Bit in CurFile^.Options then
          DisableFsRedir := False;
        if fo64Bit in CurFile^.Options then begin
          if not IsWin64 then
            InternalError('Cannot install files to 64-bit locations on this version of Windows');
          DisableFsRedir := True;
        end;

        if CurFile^.LocationEntry <> -1 then begin
          ProcessFileEntry(UninstLog, ExpandedAppId, RegisterFilesList,
            CurFile, DisableFsRedir, '', '', FileLocationFilenames, 0,
            ConfirmOverwriteOverwriteAll, PromptIfOlderOverwriteAll, WarnedPerUserFonts, nil);
        end
        else begin
          { File is an 'external' file }
          if CurFile^.FileType = ftUninstExe then begin
            { This is the file entry for the uninstaller program }
            SourceWildcard := NewParamStr(0);
            DisableFsRedir := False;
          end
          else
            SourceWildcard := ExpandConst(CurFile^.SourceFilename);
          Excludes.DelimitedText := CurFile^.Excludes;
          var ProgressBefore := CurProgress;
          repeat
            SetProgress(ProgressBefore);
            var ExpectedBytesLeft := CurFile^.ExternalSize;
            if foDownload in CurFile^.Options then begin
              { Archive download should have been done already by Setup.WizardForm's DownloadArchivesToExtract }
              if foExtractArchive in CurFile^.Options then
                InternalError('Unexpected Download flag');
              if foSkipIfSourceDoesntExist in CurFile^.Options then
                InternalError('Unexpected SkipIfSourceDoesntExist flag');
              if not(foCustomDestName in CurFile^.Options) then
                InternalError('Expected CustomDestName flag');
              { CurFile^.DestName now includes a filename, see TSetupCompiler.EnumFilesProc.ProcessFileList }
              ProcessFileEntry(UninstLog, ExpandedAppId, RegisterFilesList,
                CurFile, DisableFsRedir, SourceWildcard, ExpandConst(CurFile^.DestName),
                nil, ExpectedBytesLeft, ConfirmOverwriteOverwriteAll, PromptIfOlderOverwriteAll,
                WarnedPerUserFonts, nil);
              FoundFiles := True;
            end else if foExtractArchive in CurFile^.Options then
              FoundFiles := RecurseExternalArchiveCopyFiles(DisableFsRedir,
                SourceWildcard, Excludes, CurFile,
                ExpectedBytesLeft, ConfirmOverwriteOverwriteAll, PromptIfOlderOverwriteAll,
                WarnedPerUserFonts)
            else
              FoundFiles := RecurseExternalCopyFiles(DisableFsRedir,
                PathExtractPath(SourceWildcard), '', PathExtractName(SourceWildcard),
                IsWildcard(SourceWildcard), Excludes, CurFile,
                ExpectedBytesLeft, ConfirmOverwriteOverwriteAll, PromptIfOlderOverwriteAll,
                WarnedPerUserFonts);
          until FoundFiles or
                (foSkipIfSourceDoesntExist in CurFile^.Options) or
                AbortRetryIgnoreTaskDialogMsgBox(
                  SetupMessages[msgErrorReadingSource] + SNewLine + AddPeriod(FmtSetupMessage(msgSourceDoesntExist, [SourceWildcard])),
                  [SetupMessages[msgAbortRetryIgnoreRetry], SetupMessages[msgFileAbortRetryIgnoreSkipNotRecommended], SetupMessages[msgAbortRetryIgnoreCancel]]);
          { In case we didn't end up copying all the expected bytes, bump
            the progress bar up to the expected amount }
          Inc(ProgressBefore, CurFile^.ExternalSize);
          SetProgress(ProgressBefore);
        end;

        NotifyAfterInstallFileEntry(CurFile);
      end;
    end;
  finally
    Excludes.Free;
    FileLocationFilenames.Free;
  end;
end;

procedure CreateIcons(const UninstLog: TUninstallLog);

  function IsPathURL(const S: String): Boolean;
  { Returns True if S begins with a scheme name and colon. Should be
    compliant with RFC 2396 section 3.1. }
  const
    SchemeAlphaChars = ['A'..'Z', 'a'..'z'];
    SchemeAllChars = SchemeAlphaChars + ['0'..'9', '+', '-', '.'];
  var
    P, I: Integer;
  begin
    Result := False;
    P := PathPos(':', S);
    if (P > 2) and CharInSet(S[1], SchemeAlphaChars) then begin
      for I := 2 to P-1 do
        if not CharInSet(S[I], SchemeAllChars) then
          Exit;
      Result := True;
    end;
  end;

  procedure CreateURLFile(const Filename, URL, IconFilename: String;
    const IconIndex: Integer);
  var
    S: String;
    F: TTextFileWriter;
  begin
    S := '[InternetShortcut]' + SNewLine + 'URL=' + URL + SNewLine;
    if IconFilename <> '' then
      S := S + 'IconFile=' + IconFilename + SNewLine +
        'IconIndex=' + IntToStr(IconIndex) + SNewLine;
    F := TTextFileWriter.Create(Filename, fdCreateAlways, faWrite, fsNone);
    try
      if SameText(S, String(AnsiString(S))) then
        F.WriteAnsi(AnsiString(S))
      else
        F.Write(S);
    finally
      F.Free;
    end;
  end;

  procedure DeleteFolderShortcut(const Dir: String);
  var
    Attr: DWORD;
    DesktopIniFilename, S: String;
  begin
    Attr := GetFileAttributes(PChar(Dir));
    if (Attr <> INVALID_FILE_ATTRIBUTES) and (Attr and FILE_ATTRIBUTE_DIRECTORY <> 0) then begin
      { To be sure this is really a folder shortcut and not a regular folder,
        look for a desktop.ini file specifying CLSID_FolderShortcut }
      DesktopIniFilename := PathCombine(Dir, 'desktop.ini');
      S := GetIniString('.ShellClassInfo', 'CLSID2', '', DesktopIniFilename);
      if CompareText(S, '{0AFACED1-E828-11D1-9187-B532F1E9575D}') = 0 then begin
        DeleteFile(DesktopIniFilename);
        DeleteFile(PathCombine(Dir, 'target.lnk'));
        SetFileAttributes(PChar(Dir), Attr and not FILE_ATTRIBUTE_READONLY);
        RemoveDirectory(PChar(Dir));
      end;
    end;
  end;

  procedure CreateAnIcon(Name: String; const Description, Path, Parameters,
    WorkingDir, IconFilename: String; const IconIndex, ShowCmd: Integer;
    const NeverUninstall: Boolean; const CloseOnExit: TSetupIconCloseOnExit;
    const HotKey: Word; const AppUserModelID: String;
    const AppUserModelToastActivatorCLSID: PGUID;
    const ExcludeFromShowInNewInstall, PreventPinning: Boolean);
  var
    BeginsWithGroup: Boolean;
    LinkFilename, PifFilename, UrlFilename, DirFilename, ProbableFilename,
      ResultingFilename: String;
    Flags: TMakeDirFlags;
    URLShortcut: Boolean;
  begin
    BeginsWithGroup := Copy(Name, 1, 8) = '{group}\';
    { Note: PathExpand removes trailing spaces, so it can't be called on
      Name before the extensions are appended }
    Name := ExpandConst(Name);
    LinkFilename := PathExpand(Name + '.lnk');
    PifFilename := PathExpand(Name + '.pif');
    UrlFilename := PathExpand(Name + '.url');
    DirFilename := PathExpand(Name);

    Flags := [mdNotifyChange];
    if NeverUninstall then
      Include(Flags, mdNoUninstall)
    else if BeginsWithGroup then
      Include(Flags, mdAlwaysUninstall);

    URLShortcut := IsPathURL(Path);
    if URLShortcut then
      ProbableFilename := UrlFilename
    else
      ProbableFilename := LinkFilename;
    LogFmt('Dest filename: %s', [ProbableFilename]);
    SetFilenameLabelText(ProbableFilename, True);
    MakeDir(UninstLog, False, PathExtractDir(ProbableFilename), Flags);

    { Delete any old files first }
    DeleteFile(LinkFilename);
    DeleteFile(PifFilename);
    if NewFileExists(UrlFilename) then begin
      { Flush out any pending writes by other apps before deleting }
      WritePrivateProfileString(nil, nil, nil, PChar(UrlFilename));
    end;
    DeleteFile(UrlFilename);
    DeleteFolderShortcut(DirFilename);

    Log('Creating the icon.');

    if not URLShortcut then begin
      { Create the shortcut.
        Note: Don't call PathExpand on any of the paths since they may contain
        environment-variable strings (e.g. %SystemRoot%\...) }
      ResultingFilename := CreateShellLink(LinkFilename, Description, Path,
        Parameters, WorkingDir, IconFilename, IconIndex, ShowCmd, HotKey,
        AppUserModelID, AppUserModelToastActivatorCLSID,
        ExcludeFromShowInNewInstall, PreventPinning);

      { If a .pif file was created, apply the "Close on exit" setting }
      if (CloseOnExit <> icNoSetting) and
         SameText(PathExtractExt(ResultingFilename), '.pif') then begin
        try
          ModifyPifFile(ResultingFilename, CloseOnExit = icYes);
        except
          { Failure isn't important here. Ignore exceptions }
        end;
      end;
    end
    else begin
      { Create an Internet Shortcut (.url) file }
      CreateURLFile(UrlFilename, Path, IconFilename, IconIndex);
      ResultingFilename := UrlFilename;
    end;

    Log('Successfully created the icon.');

    { Set the global flag that is checked by the Finished wizard page }
    CreatedIcon := True;

    { Notify shell of the change }
    SHChangeNotify(SHCNE_CREATE, SHCNF_PATH, PChar(ResultingFilename), nil);
    SHChangeNotify(SHCNE_UPDATEDIR, SHCNF_PATH or SHCNF_FLUSH,
      PChar(PathExtractDir(ResultingFilename)), nil);

    { Add uninstall log entries }
    if not NeverUninstall then begin
      if URLShortcut then
        UninstLog.Add(utDeleteFile, [ResultingFilename], utDeleteFile_CallChangeNotify)
      else begin
        { Even though we only created one file, go ahead and try deleting
          both a .lnk and .pif file at uninstall time, in case the user
          alters the shortcut after installation }
        UninstLog.Add(utDeleteFile, [LinkFilename], utDeleteFile_CallChangeNotify);
        UninstLog.Add(utDeleteFile, [PifFilename], utDeleteFile_CallChangeNotify);
      end;
    end;
  end;

  function ExpandAppPath(const Filename: String): String;
  var
    K: HKEY;
    Found: Boolean;
  begin
    if RegOpenKeyExView(InstallDefaultRegView, HKEY_LOCAL_MACHINE,
       PChar(REGSTR_PATH_APPPATHS + '\' + Filename), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      Found := RegQueryStringValue(K, '', Result);
      RegCloseKey(K);
      if Found then
        Exit;
    end;
    Result := Filename;
  end;

var
  CurIconNumber: Integer;
  CurIcon: PSetupIconEntry;
  FN: String;
  TACLSID: PGUID;
begin
  for CurIconNumber := 0 to Entries[seIcon].Count-1 do begin
    try
      CurIcon := PSetupIconEntry(Entries[seIcon][CurIconNumber]);
      with CurIcon^ do begin
        if ShouldProcessIconEntry(WizardComponents, WizardTasks, WizardNoIcons, CurIcon) then begin
          DebugNotifyEntry(seIcon, CurIconNumber);
          NotifyBeforeInstallEntry(BeforeInstall);
          Log('-- Icon entry --');
          FN := ExpandConst(Filename);
          if ioUseAppPaths in Options then
            FN := ExpandAppPath(FN);
          if not(ioCreateOnlyIfFileExists in Options) or NewFileExistsRedir(IsWin64, FN) then begin
            if ioHasAppUserModelToastActivatorCLSID in Options then
              TACLSID := @AppUserModelToastActivatorCLSID
            else
              TACLSID := nil;
            CreateAnIcon(IconName, ExpandConst(Comment), FN,
              ExpandConst(Parameters), ExpandConst(WorkingDir),
              ExpandConst(IconFilename), IconIndex, ShowCmd,
              ioUninsNeverUninstall in Options, CloseOnExit, HotKey,
              ExpandConst(AppUserModelID), TACLSID,
              ioExcludeFromShowInNewInstall in Options,
              ioPreventPinning in Options)
          end else
            Log('Skipping due to "createonlyiffileexists" flag.');

          { Increment progress meter }
          IncProgress(1000);
            
          NotifyAfterInstallEntry(AfterInstall);
        end;
      end;
    except
      if not(ExceptObject is EAbort) then
        Application.HandleException(nil)
      else
        raise;
    end;
    ProcessEvents;
    { Clear previous filename label in case an exception or debugger break
      occurs between now and when the label for the next entry is set }
    SetFilenameLabelText('', False);
  end;
end;

procedure CreateIniEntries(const UninstLog: TUninstallLog);
var
  CurIniNumber: Integer;
  CurIni: PSetupIniEntry;
  IniSection, IniEntry, IniValue, IniFilename, IniDir: String;
  Skip: Boolean;
begin
  for CurIniNumber := 0 to Entries[seIni].Count-1 do begin
    CurIni := PSetupIniEntry(Entries[seIni][CurIniNumber]);
    with CurIni^ do begin
      if ShouldProcessEntry(WizardComponents, WizardTasks, Components, Tasks, Languages, Check) then begin
        DebugNotifyEntry(seIni, CurIniNumber);
        NotifyBeforeInstallEntry(BeforeInstall);
        Log('-- INI entry --');
        IniSection := ExpandConst(Section);
        IniEntry := ExpandConst(Entry);
        IniValue := ExpandConst(Value);
        IniFilename := ExpandConst(Filename);
        LogFmt('Dest filename: %s', [IniFilename]);
        LogFmt('Section: %s', [IniSection]);
        if IniEntry <> '' then
          LogFmt('Entry: %s', [IniEntry]);
        if ioHasValue in Options then
          LogFmt('Value: %s', [IniValue]);

        if (IniEntry <> '') and (ioHasValue in Options) and
           (not(ioCreateKeyIfDoesntExist in Options) or
            not IniKeyExists(IniSection, IniEntry, IniFilename)) then begin
          Skip := False;
          IniDir := PathExtractDir(IniFilename);
          if IniDir <> '' then begin
            while True do begin
              try
                MakeDir(UninstLog, False, IniDir, []);
                Break;
              except
                if AbortRetryIgnoreTaskDialogMsgBox(
                     GetExceptMessage,
                     [SetupMessages[msgAbortRetryIgnoreRetry], SetupMessages[msgAbortRetryIgnoreIgnore], SetupMessages[msgAbortRetryIgnoreCancel]]) then begin
                  Skip := True;
                  Break;
                end;
              end;
            end;
          end;
          if not Skip then
            Log('Updating the .INI file.');
            repeat
              if SetIniString(IniSection, IniEntry, IniValue, IniFilename) then begin
                Log('Successfully updated the .INI file.');
                Break;
              end;
             until AbortRetryIgnoreTaskDialogMsgBox(
                     FmtSetupMessage1(msgErrorIniEntry, IniFilename),
                     [SetupMessages[msgAbortRetryIgnoreRetry], SetupMessages[msgAbortRetryIgnoreIgnore], SetupMessages[msgAbortRetryIgnoreCancel]]);
        end else
          Log('Skipping updating the .INI file, only updating uninstall log.');

        if ioUninsDeleteEntireSection in Options then
          UninstLog.Add(utIniDeleteSection, [IniFilename, IniSection], 0);
        if ioUninsDeleteSectionIfEmpty in Options then
          UninstLog.Add(utIniDeleteSection, [IniFilename, IniSection],
            utIniDeleteSection_OnlyIfEmpty);
        if (ioUninsDeleteEntry in Options) and (IniEntry <> '') then
          UninstLog.Add(utIniDeleteEntry, [IniFilename, IniSection, IniEntry], 0);
        { ^ add utIniDeleteEntry last since we want it done first by the
          uninstaller (in case the entry's also got the
          "uninsdeletesectionifempty" flag) }

        NotifyAfterInstallEntry(AfterInstall);
      end;
    end;
  end;

  { Increment progress meter }
  IncProgress(1000);
end;

procedure CreateRegistryEntries(const UninstLog: TUninstallLog);
  
  function IsDeletableSubkey(const S: String): Boolean;
  { A sanity check to prevent people from shooting themselves in the foot by
    using
      Root: HKLM; Subkey: ""; Flags: [unins]deletekey
    or a 'code' constant in Subkey that returns a blank string or only
    backslashes. }
  var
    P: PChar;
  begin
    Result := False;
    P := PChar(S);
    while P^ <> #0 do begin
      if P^ <> '\' then begin
        Result := True;
        Break;
      end;
      Inc(P);
    end;
  end;

  procedure ApplyPermissions(const RegView: TRegView; const RootKey: HKEY;
    const Subkey: String; const PermsEntry: Integer);
  var
    P: PSetupPermissionEntry;
  begin
    LogFmt('Setting permissions on key: %s\%s',
      [GetRegRootKeyName(RootKey), Subkey]);
    P := Entries[sePermission][PermsEntry];
    if not GrantPermissionOnKey(RegView, RootKey, Subkey,
       TGrantPermissionEntry(Pointer(P.Permissions)^),
       Length(P.Permissions) div SizeOf(TGrantPermissionEntry)) then begin
      if GetLastError = ERROR_FILE_NOT_FOUND then
        Log('Could not set permissions on the key because it currently does not exist.')
      else
        LogFmt('Failed to set permissions on the key (%d).', [GetLastError]);
    end;
  end;

const
  REG_QWORD = 11;
var
  RK, K: HKEY;
  Disp: DWORD;
  N, V, ExistingData: String;
  ExistingType, NewType, DV: DWORD;
  S: String;
  RV: TRegView;
  CurRegNumber: Integer;
  NeedToRetry, DidDeleteKey: Boolean;
  ErrorCode: Longint;
  I: Integer;
  AnsiS: AnsiString;
begin
  for CurRegNumber := 0 to Entries[seRegistry].Count-1 do begin
    with PSetupRegistryEntry(Entries[seRegistry][CurRegNumber])^ do begin
      if ShouldProcessEntry(WizardComponents, WizardTasks, Components, Tasks, Languages, Check) then begin
        DebugNotifyEntry(seRegistry, CurRegNumber);
        NotifyBeforeInstallEntry(BeforeInstall);
        Log('-- Registry entry --');
        RK := RootKey;
        if RK = HKEY_AUTO then
          RK := InstallModeRootKey;
        S := ExpandConst(Subkey);
        LogFmt('Key: %s\%s', [GetRegRootKeyName(RK), Subkey]);
        N := ExpandConst(ValueName);
        if N <> '' then
          LogFmt('Value name: %s', [N]);
        RV := InstallDefaultRegView;
        if (ro32Bit in Options) and (RV <> rv32Bit) then begin
          Log('Non-default bitness: 32-bit');
          RV := rv32Bit;
        end;
        if ro64Bit in Options then begin
          if not IsWin64 then
            InternalError('Cannot access 64-bit registry keys on this version of Windows');
          if RV <> rv64Bit then begin
            Log('Non-default bitness: 64-bit');
            RV := rv64Bit;
          end;
        end;

        repeat
          NeedToRetry := False;

          try
            DidDeleteKey := False;
            if roDeleteKey in Options then begin
              if IsDeletableSubkey(S) then begin
                Log('Deleting the key.');
                RegDeleteKeyIncludingSubkeys(RV, RK, PChar(S));
                DidDeleteKey := True;
              end else
                Log('Key to delete is not deletable.');
            end;
            if (roDeleteKey in Options) and (Typ = rtNone) then begin
              { We've deleted the key, and no value is to be created.
                Our work is done. }
              if DidDeleteKey then
                Log('Successfully deleted the key.');
            end else if (roDeleteValue in Options) and (Typ = rtNone) then begin
              { We're going to delete a value with no intention of creating
                another, so don't create the key if it didn't exist. }
              if RegOpenKeyExView(RV, RK, PChar(S), 0, KEY_SET_VALUE, K) = ERROR_SUCCESS then begin
                Log('Deleting the value.');
                RegDeleteValue(K, PChar(N));
                RegCloseKey(K);
                Log('Successfully deleted the value.');
                { Our work is done. }
              end else
                Log('Key of value to delete does not exist.');
            end
            else begin
              { Apply any permissions *before* calling RegCreateKeyExView or
                RegOpenKeyExView, since we may (in a rather unlikely scenario)
                need those permissions in order for those calls to succeed }
              if PermissionsEntry <> -1 then
                ApplyPermissions(RV, RK, S, PermissionsEntry);
              { Create or open the key }
              if not(roDontCreateKey in Options) then begin
                Log('Creating or opening the key.');
                ErrorCode := RegCreateKeyExView(RV, RK, PChar(S), 0, nil,
                  REG_OPTION_NON_VOLATILE, KEY_QUERY_VALUE or KEY_SET_VALUE,
                  nil, K, @Disp);
                if ErrorCode = ERROR_SUCCESS then begin
                  { Apply permissions again if a new key was created }
                  if (Disp = REG_CREATED_NEW_KEY) and (PermissionsEntry <> -1) then begin
                    Log('New key created, need to set permissions again.');
                    ApplyPermissions(RV, RK, S, PermissionsEntry);
                  end;
                end
                else begin
                  if not(roNoError in Options) then
                    RegError(reRegCreateKeyEx, RK, S, ErrorCode);
                end;
              end
              else begin
                if Typ <> rtNone then begin
                  Log('Opening the key.');
                  ErrorCode := RegOpenKeyExView(RV, RK, PChar(S), 0,
                    KEY_QUERY_VALUE or KEY_SET_VALUE, K);
                  if (ErrorCode <> ERROR_SUCCESS) and (ErrorCode <> ERROR_FILE_NOT_FOUND) then
                    if not(roNoError in Options) then
                      RegError(reRegOpenKeyEx, RK, S, ErrorCode);
                end
                else begin
                  { We're not creating a value, and we're not just deleting a
                    value (that was checked above), so there is no reason to
                    even open the key }
                  Log('Not creating the key or a value, skipping the key and only updating uninstall log.');
                  ErrorCode := ERROR_FILE_NOT_FOUND;
                end;
              end;
              { If there was no error opening the key, proceed with deleting
                and/or creating the value }
              if ErrorCode = ERROR_SUCCESS then
              try
                if roDeleteValue in Options then begin
                  Log('Deleting the value.');
                  RegDeleteValue(K, PChar(N));
                end;
                if (Typ <> rtNone) and
                   (not(roCreateValueIfDoesntExist in Options) or
                    not RegValueExists(K, PChar(N))) then begin
                  Log('Creating or setting the value.');
                  case Typ of
                    rtString, rtExpandString, rtMultiString: begin
                        NewType := REG_SZ;
                        case Typ of
                          rtExpandString: NewType := REG_EXPAND_SZ;
                          rtMultiString: NewType := REG_MULTI_SZ;
                        end;
                        if Typ <> rtMultiString then begin
                          if (Pos('{olddata}', ValueData) <> 0) and
                             RegQueryStringValue(K, PChar(N), ExistingData) then
                            { successful }
                          else
                            ExistingData := '';
                          if roPreserveStringType in Options then begin
                            if (RegQueryValueEx(K, PChar(N), nil, @ExistingType, nil, nil) = ERROR_SUCCESS) and
                               ((ExistingType = REG_SZ) or (ExistingType = REG_EXPAND_SZ)) then
                              NewType := ExistingType;
                          end;
                          V := ExpandConstEx(ValueData, ['olddata', ExistingData])
                        end
                        else begin
                          if (Pos('{olddata}', ValueData) <> 0) and
                             RegQueryMultiStringValue(K, PChar(N), ExistingData) then
                            { successful }
                          else
                            ExistingData := '';
                          V := ExpandConstEx(ValueData, ['olddata', ExistingData,
                            'break', #0]);
                          { Multi-string data requires two null terminators:
                            one after the last string, and one to mark the end.
                            Delphi's String type is implicitly null-terminated,
                            so only one null needs to be added to the end. }
                          if (V <> '') and (V[Length(V)] <> #0) then
                            V := V + #0;
                        end;
                        ErrorCode := RegSetValueEx(K, PChar(N), 0, NewType,
                          PChar(V), (Length(V)+1)*SizeOf(V[1]));
                        if (ErrorCode <> ERROR_SUCCESS) and
                           not(roNoError in Options) then
                          RegError(reRegSetValueEx, RK, S, ErrorCode);
                      end;
                    rtDWord: begin
                        DV := StrToInt(ExpandConst(ValueData));
                        ErrorCode := RegSetValueEx(K, PChar(N), 0, REG_DWORD,
                          @DV, SizeOf(DV));
                        if (ErrorCode <> ERROR_SUCCESS) and
                           not(roNoError in Options) then
                          RegError(reRegSetValueEx, RK, S, ErrorCode);
                      end;
                    rtQWord: begin
                        const QV: UInt64 = StrToUInt64(ExpandConst(ValueData));
                        ErrorCode := RegSetValueEx(K, PChar(N), 0, REG_QWORD,
                          @QV, SizeOf(QV));
                        if (ErrorCode <> ERROR_SUCCESS) and
                           not(roNoError in Options) then
                          RegError(reRegSetValueEx, RK, S, ErrorCode);
                      end;
                    rtBinary: begin
                        AnsiS := '';
                        for I := 1 to Length(ValueData) do
                          AnsiS := AnsiS + AnsiChar(Ord(ValueData[I]));
                        ErrorCode := RegSetValueEx(K, PChar(N), 0, REG_BINARY,
                          PAnsiChar(AnsiS), Length(AnsiS));
                        if (ErrorCode <> ERROR_SUCCESS) and
                           not(roNoError in Options) then
                          RegError(reRegSetValueEx, RK, S, ErrorCode);
                      end;
                    end;
                  Log('Successfully created or set the value.');
                end else if roDeleteValue in Options then
                  Log('Successfully deleted the value.')
                else
                  Log('Successfully created the key.')
                { Our work is done. }
              finally
                RegCloseKey(K);
              end;
            end;
          except
            if not AbortRetryIgnoreTaskDialogMsgBox(
                     GetExceptMessage,
                     [SetupMessages[msgAbortRetryIgnoreRetry], SetupMessages[msgAbortRetryIgnoreIgnore], SetupMessages[msgAbortRetryIgnoreCancel]]) then begin
              Log('Retrying.');
              NeedToRetry := True;
            end;
          end;
        until not NeedToRetry;
          
        if roUninsDeleteEntireKey in Options then
          if IsDeletableSubkey(S) then
            UninstLog.AddReg(utRegDeleteEntireKey, RV, RK, [S]);
        if roUninsDeleteEntireKeyIfEmpty in Options then
          if IsDeletableSubkey(S) then
            UninstLog.AddReg(utRegDeleteKeyIfEmpty, RV, RK, [S]);
        if roUninsDeleteValue in Options then
          UninstLog.AddReg(utRegDeleteValue, RV, RK, [S, N]);
          { ^ must add roUninsDeleteValue after roUninstDeleteEntireKey*
            since the entry may have both the roUninsDeleteValue and
            roUninsDeleteEntireKeyIfEmpty options }
        if roUninsClearValue in Options then
          UninstLog.AddReg(utRegClearValue, RV, RK, [S, N]);

        NotifyAfterInstallEntry(AfterInstall);
      end;
    end;
  end;

  { Increment progress meter }
  IncProgress(1000);
end;

procedure RegisterFiles(const RegisterFilesList: TList);

  procedure RegisterServersOnRestart;

    function CreateRegSvrExe(const Dir: String): String;
    var
      ExeFilename: String;
      SourceF, DestF: TFile;
      NumRead: Cardinal;
      Buf: array[0..16383] of Byte;
    begin
      ExeFilename := GenerateUniqueName(False, Dir, '.exe');
      DestF := nil;
      SourceF := TFile.Create(NewParamStr(0), fdOpenExisting, faRead, fsRead);
      try
        DestF := TFile.Create(ExeFilename, fdCreateAlways, faWrite, fsNone);
        try
          DestF.Seek(SourceF.Size);
          DestF.Truncate;
          DestF.Seek(0);
          while True do begin
            NumRead := SourceF.Read(Buf, SizeOf(Buf));
            if NumRead = 0 then
              Break;
            DestF.WriteBuffer(Buf, NumRead);
          end;
          if not(shSignedUninstaller in SetupHeader.Options) then
            MarkExeHeader(DestF, SetupExeModeRegSvr);
        except
          FreeAndNil(DestF);
          DeleteFile(ExeFilename);
          raise;
        end;
      finally
        DestF.Free;
        SourceF.Free;
      end;
      Result := ExeFilename;
    end;

    procedure CreateRegSvrMsg(const Filename: String);
    var
      F: TFile;
    begin
      F := TFile.Create(Filename, fdCreateAlways, faWrite, fsNone);
      try
        WriteMsgData(F);
      finally
        F.Free;
      end;
    end;

  const
    Chars: array[Boolean, Boolean] of Char = (('s', 't'), ('S', 'T'));
  var
    RegSvrExeFilename: String;
    F: TTextFileWriter;
    Rec: PRegisterFilesListRec;
    RootKey, H: HKEY;
    I, J: Integer;
    Disp: DWORD;
    ValueName, Data: String;
    ErrorCode: Longint;
  begin
    { Create RegSvr program used to register OLE servers & type libraries on
      the next reboot }
    if IsAdmin then begin
      try
        RegSvrExeFilename := CreateRegSvrExe(WinDir);
      except
        { In case Windows directory is write protected, try the Temp directory.
          Windows directory is our first choice since some people (ignorantly)
          put things like "DELTREE C:\WINDOWS\TEMP\*.*" in their AUTOEXEC.BAT.
          Also, each user has his own personal Temp directory which may not
          be accessible by other users. }
        RegSvrExeFilename := CreateRegSvrExe(GetTempDir);
      end;
    end
    else begin
      { Always use Temp directory when user doesn't have admin privileges }
      RegSvrExeFilename := CreateRegSvrExe(GetTempDir);
    end;
    LogFmt('Registration executable created: %s', [RegSvrExeFilename]);

    try
      CreateRegSvrMsg(PathChangeExt(RegSvrExeFilename, '.msg'));

      F := TTextFileWriter.Create(PathChangeExt(RegSvrExeFilename, '.lst'),
        fdCreateAlways, faWrite, fsNone);
      try
        F.WriteLine('; This file was created by the installer for:');
        F.WriteLine(';   ' + ExpandedAppVerName);
        F.WriteLine(';   Location: ' + SetupLdrOriginalFilename);
        F.WriteLine('');
        F.WriteLine('; List of files to be registered on the next reboot. DO NOT EDIT!');
        F.WriteLine('');
        for I := 0 to RegisterFilesList.Count-1 do begin
          Rec := RegisterFilesList[I];
          Data := '[..]' + Rec.Filename;
          Data[2] := Chars[Rec.Is64Bit, Rec.TypeLib];
          if Rec.NoErrorMessages then
            Data[3] := 'q';
          F.WriteLine(Data);
        end;
      finally
        F.Free;
      end;

      if IsAdmin then
        RootKey := HKEY_LOCAL_MACHINE
      else
        RootKey := HKEY_CURRENT_USER;
      ErrorCode := RegCreateKeyExView(rvDefault, RootKey, REGSTR_PATH_RUNONCE, 0, nil,
        REG_OPTION_NON_VOLATILE, KEY_SET_VALUE or KEY_QUERY_VALUE,
        nil, H, @Disp);
      if ErrorCode <> ERROR_SUCCESS then
        RegError(reRegCreateKeyEx, RootKey, REGSTR_PATH_RUNONCE, ErrorCode);
      try
        J := 0;
        while True do begin
          Inc(J);
          ValueName := Format('InnoSetupRegFile.%.10d', [J]);  { don't localize }
          { ^ Note: Names of values written to the "RunOnce" key cannot
              exceed 31 characters! Otherwise the original Windows
              Explorer 4.0 will not process them. }
          if not RegValueExists(H, PChar(ValueName)) then begin
            Data := '"' + RegSvrExeFilename + '" /REG';
            if not IsAdmin then
              Data := Data + 'U';  { /REG -> /REGU when not running as admin }
            { Note: RegSvr expects /REG(U) to be the first parameter }
            Data := Data + ' /REGSVRMODE';
            ErrorCode := RegSetValueEx(H, PChar(ValueName), 0, REG_SZ, PChar(Data),
              (Length(Data)+1)*SizeOf(Data[1]));
            if ErrorCode <> ERROR_SUCCESS then
              RegError(reRegSetValueEx, RootKey, REGSTR_PATH_RUNONCE, ErrorCode);
            Break;
          end;
        end;
      finally
        RegCloseKey(H);
      end;
    except
      DeleteFile(PathChangeExt(RegSvrExeFilename, '.lst'));
      DeleteFile(PathChangeExt(RegSvrExeFilename, '.msg'));
      DeleteFile(RegSvrExeFilename);
      raise;
    end;
  end;

  procedure RegisterSvr(const Is64Bit: Boolean; const Filename: String;
    const NoErrorMessages: Boolean);
  var
    NeedToRetry: Boolean;
  begin
    repeat
      if Is64Bit then
        LogFmt('Registering 64-bit DLL/OCX: %s', [Filename])
      else
        LogFmt('Registering 32-bit DLL/OCX: %s', [Filename]);
      NeedToRetry := False;
      try
        RegisterServer(False, Is64Bit, Filename, NoErrorMessages);
        Log('Registration successful.');
      except
        Log('Registration failed:' + SNewLine + GetExceptMessage);
        if not NoErrorMessages then
          if not AbortRetryIgnoreTaskDialogMsgBox(
                   Filename + SNewLine2 + FmtSetupMessage1(msgErrorRegisterServer, GetExceptMessage),
                   [SetupMessages[msgAbortRetryIgnoreRetry], SetupMessages[msgFileAbortRetryIgnoreIgnoreNotRecommended], SetupMessages[msgAbortRetryIgnoreCancel]]) then
            NeedToRetry := True;
      end;
    until not NeedToRetry;
  end;

  procedure RegisterTLib(const Is64Bit: Boolean; const Filename: String;
    const NoErrorMessages: Boolean);
  var
    NeedToRetry: Boolean;
  begin
    repeat
      if Is64Bit then
        LogFmt('Registering 64-bit type library: %s', [Filename])
      else
        LogFmt('Registering 32-bit type library: %s', [Filename]);
      NeedToRetry := False;
      try
        if Is64Bit then
          HelperRegisterTypeLibrary(False, Filename)
        else
          RegisterTypeLibrary(Filename);
        Log('Registration successful.');
      except
        Log('Registration failed:' + SNewLine + GetExceptMessage);
        if not NoErrorMessages then
          if not AbortRetryIgnoreTaskDialogMsgBox(
                   Filename + SNewLine2 + FmtSetupMessage1(msgErrorRegisterTypeLib, GetExceptMessage),
                   [SetupMessages[msgAbortRetryIgnoreRetry], SetupMessages[msgFileAbortRetryIgnoreIgnoreNotRecommended], SetupMessages[msgAbortRetryIgnoreCancel]]) then
            NeedToRetry := True;
      end;
    until not NeedToRetry;
  end;

var
  I: Integer;
begin
  if not NeedsRestart then
    for I := 0 to RegisterFilesList.Count-1 do begin
      with PRegisterFilesListRec(RegisterFilesList[I])^ do
        if not TypeLib then
          RegisterSvr(Is64Bit, Filename, NoErrorMessages)
        else
          RegisterTLib(Is64Bit, Filename, NoErrorMessages);
    end
  else begin
    { When a restart is needed, all "regserver" & "regtypelib" files will get
      registered on the next logon }
    Log('Delaying registration of all files until the next logon since a restart is needed.');
    try
      RegisterServersOnRestart;
    except
      Application.HandleException(nil);
    end;
  end;
end;

procedure RecordUninstallDeleteEntries(const UninstLog: TUninstallLog);
const
  DefFlags: array[TSetupDeleteType] of Longint = (
    utDeleteDirOrFiles_Extra or utDeleteDirOrFiles_DeleteFiles,
    utDeleteDirOrFiles_Extra or utDeleteDirOrFiles_DeleteFiles or
      utDeleteDirOrFiles_DeleteSubdirsAlso,
    utDeleteDirOrFiles_Extra or utDeleteDirOrFiles_IsDir);
var
  I: Integer;
  Flags: Longint;
begin
  for I := Entries[seUninstallDelete].Count-1 downto 0 do
    { ^ process backwards so the uninstaller will process them in the order
        they appear in the script }
    with PSetupDeleteEntry(Entries[seUninstallDelete][I])^ do
      if ShouldProcessEntry(WizardComponents, WizardTasks, Components, Tasks, Languages, Check) then begin
        DebugNotifyEntry(seUninstallDelete, I);
        NotifyBeforeInstallEntry(BeforeInstall);
        Flags := DefFlags[DeleteType];
        if InstallDefaultDisableFsRedir then
          Flags := Flags or utDeleteDirOrFiles_DisableFsRedir;
        UninstLog.Add(utDeleteDirOrFiles, [ExpandConst(Name)], Flags);
        NotifyAfterInstallEntry(AfterInstall);
      end;
end;

procedure RecordUninstallRunEntries(const UninstLog: TUninstallLog);
var
  I: Integer;
  RunEntry: PSetupRunEntry;
  Flags: Longint;
begin
  for I := Entries[seUninstallRun].Count-1 downto 0 do begin
    { ^ process backwards so the uninstaller will process them in the order
        they appear in the script }
    RunEntry := PSetupRunEntry(Entries[seUninstallRun][I]);
    if ShouldProcessEntry(WizardComponents, WizardTasks, RunEntry.Components,
       RunEntry.Tasks, RunEntry.Languages, RunEntry.Check) then begin
      DebugNotifyEntry(seUninstallRun, I);
      NotifyBeforeInstallEntry(RunEntry.BeforeInstall);
      Flags := 0;
      case RunEntry.Wait of
        rwNoWait: Flags := Flags or utRun_NoWait;
        rwWaitUntilIdle: Flags := Flags or utRun_WaitUntilIdle;
      end;
      if roShellExec in RunEntry.Options then
        Flags := Flags or (utRun_ShellExec or utRun_ShellExecRespectWaitFlags)
      else begin
        if ShouldDisableFsRedirForRunEntry(RunEntry) then
          Flags := Flags or utRun_DisableFsRedir;
      end;
      if roSkipIfDoesntExist in RunEntry.Options then
        Flags := Flags or utRun_SkipIfDoesntExist;
      case RunEntry.ShowCmd of
        SW_SHOWMINNOACTIVE: Flags := Flags or utRun_RunMinimized;
        SW_SHOWMAXIMIZED: Flags := Flags or utRun_RunMaximized;
        SW_HIDE: Flags := Flags or utRun_RunHidden;
      end;
      if roDontLogParameters in RunEntry.Options then
        Flags := Flags or utRun_DontLogParameters;
      if roLogOutput in RunEntry.Options then
        Flags := Flags or utRun_LogOutput;
      UninstLog.Add(utRun, [ExpandConst(RunEntry.Name),
        ExpandConst(RunEntry.Parameters), ExpandConst(RunEntry.WorkingDir),
        ExpandConst(RunEntry.RunOnceId), ExpandConst(RunEntry.Verb)],
        Flags);
      NotifyAfterInstallEntry(RunEntry.AfterInstall);
    end;
  end;
end;

procedure GenerateUninstallInfoFilename(const UninstLog: TUninstallLog);
var
  ExistingFiles: array[0..999] of Boolean;
  BaseDir: String;

  procedure FindFiles;
  var
    H: THandle;
    FindData: TWin32FindData;
    S: String;
  begin
    H := FindFirstFile(PChar(AddBackslash(BaseDir) + 'unins???.*'),
      FindData);
    if H <> INVALID_HANDLE_VALUE then begin
      repeat
        S := FindData.cFilename;
        if (Length(S) >= 9) and (CompareText(Copy(S, 1, 5), 'unins') = 0) and
           CharInSet(S[6], ['0'..'9']) and CharInSet(S[7], ['0'..'9']) and CharInSet(S[8], ['0'..'9']) and
           (S[9] = '.') then
          ExistingFiles[StrToInt(Copy(S, 6, 3))] := True;
      until not FindNextFile(H, FindData);
      Windows.FindClose(H);
    end;
  end;

  procedure GenerateFilenames(const I: Integer);
  var
    BaseFilename: String;
  begin
    BaseFilename := AddBackslash(BaseDir) + Format('unins%.3d', [I]);
    UninstallExeFilename := BaseFilename + '.exe';
    UninstallDataFilename := BaseFilename + '.dat';
    UninstallMsgFilename := BaseFilename + '.msg';
  end;

  procedure ReserveDataFile;
  var
    H: THandle;
  begin
    { Create an empty .dat file to reserve the filename. }
    H := CreateFile(PChar(UninstallDataFilename), GENERIC_READ or GENERIC_WRITE,
      0, nil, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
    if H = INVALID_HANDLE_VALUE then
      Win32ErrorMsg('CreateFile');
    CloseHandle(H);
    UninstallDataCreated := True;
  end;

var
  I: Integer;
  ExistingFlags: TUninstallLogFlags;
begin
  { Note: We never disable FS redirection when writing to UninstallFilesDir.
    If someone sets UninstallFilesDir to "sys", we can't place a 32-bit
    uninstaller in the 64-bit system directory, because it wouldn't see its
    .dat file -- it would try to open 'windows\system32\unins???.dat' but
    fail because system32 maps to syswow64 by default.
    Not to mention, 32-bit EXEs really have no business being in the 64-bit
    system directory, and vice versa. Might result in undefined behavior? }

  { Because we don't disable FS redirection, we have to change any system32
    to syswow64, otherwise Add/Remove Programs would look for the
    UninstallString executable in the 64-bit system directory (at least
    when using a 64-bit Uninstall key) }
  BaseDir := ReplaceSystemDirWithSysWow64(PathExpand(ExpandConst(SetupHeader.UninstallFilesDir)));
  LogFmt('Directory for uninstall files: %s', [BaseDir]);
  MakeDir(UninstLog, False, BaseDir, []);

  FillChar(ExistingFiles, SizeOf(ExistingFiles), 0);  { set all to False }
  FindFiles;

  { Look for an existing .dat file to append to or overwrite }
  if SetupHeader.UninstallLogMode <> lmNew then
    for I := 0 to 999 do
      if ExistingFiles[I] then begin
        GenerateFilenames(I);
        if NewFileExists(UninstallDataFilename) and
           UninstLog.CanAppend(UninstallDataFilename, ExistingFlags) then begin
          if SetupHeader.UninstallLogMode = lmAppend then begin
            LogFmt('Will append to existing uninstall log: %s', [UninstallDataFilename]);
            AppendUninstallData := True;
          end
          else
            LogFmt('Will overwrite existing uninstall log: %s', [UninstallDataFilename]);
          Exit;
        end;
      end;
  { None found; use a new .dat file }
  for I := 0 to 999 do
    if not ExistingFiles[I] then begin
      GenerateFilenames(I);
      LogFmt('Creating new uninstall log: %s', [UninstallDataFilename]);
      ReserveDataFile;
      Exit;
    end;
  raise Exception.Create(FmtSetupMessage1(msgErrorTooManyFilesInDir,
    BaseDir));
end;

procedure RenameUninstallExe;
begin
  { If the uninstall EXE wasn't extracted to a .tmp file because it isn't
    replacing an existing uninstall EXE, exit. }
  if UninstallTempExeFilename = '' then
    Exit;
  Log('Renaming uninstaller.');
  var Timer: TOneShotTimer;
  var RetriesLeft := 4;
  while True do begin
    Timer.Start(1000);
    if MoveFileReplace(UninstallTempExeFilename, UninstallExeFilename) then
      Break;
    var LastError := GetLastError;
    { Does the error code indicate that the file is possibly in use? }
    if LastErrorIndicatesPossiblyInUse(LastError, False) then begin
      if RetriesLeft > 0 then begin
        LogFmt('The existing file appears to be in use (%d). ' +
          'Retrying.', [LastError]);
        Dec(RetriesLeft);
        Timer.SleepUntilExpired;
        ProcessEvents;
        Continue;
      end;
    end;

    const LastOperation = SetupMessages[msgErrorReplacingExistingFile];
    const Failed = AddPeriod(FmtSetupMessage(msgErrorFunctionFailedWithMessage,
      ['MoveFileEx', IntToStr(LastError), Win32ErrorString(LastError)]));
    const Text = UninstallExeFilename + SNewLine2 + LastOperation + SNewLine + Failed;
    case LoggedTaskDialogMsgBox('',  SetupMessages[msgRetryCancelSelectAction], Text, '',
       mbError, MB_RETRYCANCEL, [SetupMessages[msgRetryCancelRetry], SetupMessages[msgRetryCancelCancel]],
       0, True, IDCANCEL) of
      IDRETRY: ;
      IDCANCEL: Abort;
    else
      Log('LoggedTaskDialogMsgBox returned an unexpected value. Assuming Cancel.');
      Abort;
    end;
  end;
  UninstallTempExeFilename := '';
end;

function CreateUninstallMsgFile: Boolean;
{ If the uninstaller EXE has a digital signature, or if Setup was started
  with /DETACHEDMSG, create the unins???.msg file }
var
  F: TFile;
begin
  { If this installation didn't create or replace an unins???.exe file,
    do nothing }
  Result := False;
  if (UninstallExeCreated <> ueNone) and
     ((shSignedUninstaller in SetupHeader.Options) or DetachedUninstMsgFile) then begin
    LogFmt('Writing uninstaller messages: %s', [UninstallMsgFilename]);
    F := TFile.Create(UninstallMsgFilename, fdCreateAlways, faWrite, fsNone);
    try
      if UninstallExeCreated = ueNew then
        Result := True;
      WriteMsgData(F);
    finally
      F.Free;
    end;
  end;
end;

procedure PerformInstall(var Succeeded: Boolean; const ChangesEnvironment,
  ChangesAssociations: Boolean);
var
  Uninstallable, UninstLogCleared: Boolean;
  I: Integer;
  UninstallRegKeyBaseName: String;
begin
  Succeeded := False;
  Log('Starting the installation process.');
  SetCurrentDir(WinSystemDir);
  var InstallFilesSize, AfterInstallFilesSize: Int64;
  CalcFilesSize(InstallFilesSize, AfterInstallFilesSize);
  InitProgressGauge(InstallFilesSize);
  UninstallExeCreated := ueNone;
  UninstallDataCreated := False;
  var UninstallMsgCreated := False;
  AppendUninstallData := False;
  UninstLogCleared := False;
  var RegisterFilesList: TList := nil;
  const UninstLog = TSetupUninstallLog.Create;
  try
    try
      { Get AppId, UninstallRegKeyBaseName, and Uninstallable now so the user
        can't change them while we're installing }
      const ExpandedAppId = ExpandConst(SetupHeader.AppId);
      if ExpandedAppId = '' then
        InternalError('Failed to get a non empty installation "AppId"');
      if TUninstallLog.WriteSafeHeaderString(nil, ExpandedAppId, 0) > 128 then
        InternalError('"AppId" cannot exceed 128 bytes (encoded)');
      UninstallRegKeyBaseName := GetUninstallRegKeyBaseName(ExpandedAppId);
      Uninstallable := EvalDirectiveCheck(SetupHeader.Uninstallable);

      { Init }
      UninstLog.InstallMode64Bit := Is64BitInstallMode;
      UninstLog.AppName := ExpandedAppName;
      UninstLog.AppId := ExpandedAppId;
      if IsAdminInstallMode then
        Include(UninstLog.Flags, ufAdminInstallMode);
      if IsWin64 then
        Include(UninstLog.Flags, ufWin64);
      if IsAdmin then { Setup or [Code] might have done administrative actions, even if IsAdminInstallMode is False }
        Include(UninstLog.Flags, ufAdminInstalled)
      else if IsPowerUserOrAdmin then
        { Note: This flag is only set in 5.1.9 and later }
        Include(UninstLog.Flags, ufPowerUserInstalled);
      if SetupHeader.WizardStyle = wsModern then
        Include(UninstLog.Flags, ufModernStyle);
      if shUninstallRestartComputer in SetupHeader.Options then
        Include(UninstLog.Flags, ufAlwaysRestart);
      if ChangesEnvironment then
        Include(UninstLog.Flags, ufChangesEnvironment);
      RecordStartInstall(UninstLog);
      RecordCompiledCode(UninstLog);

      RegisterFilesList := TList.Create;

      { Process Component entries, if any }
      ProcessComponentEntries;
      ProcessEvents;

      { Process Tasks entries, if any }
      ProcessTasksEntries;
      ProcessEvents;

      { Shutdown applications, if any }
      if RmSessionStarted and RmFoundApplications then begin
        if WizardPreparingYesRadio then begin
          SetStatusLabelText(SetupMessages[msgStatusClosingApplications]);
          ShutdownApplications;
          ProcessEvents;
        end else
          Log('User chose not to shutdown applications using our files.');
      end;

      { Process InstallDelete entries, if any }
      ProcessInstallDeleteEntries;
      ProcessEvents;

      if ExpandedAppMutex <> '' then
        UninstLog.Add(utMutexCheck, [ExpandedAppMutex], 0);
      if ChangesAssociations then
        UninstLog.Add(utRefreshFileAssoc, [''], 0);

      { Record UninstallDelete entries, if any }
      RecordUninstallDeleteEntries(UninstLog);
      ProcessEvents;

      { Create the application directory and extra dirs }
      SetStatusLabelText(SetupMessages[msgStatusCreateDirs]);
      CreateDirs(UninstLog);
      ProcessEvents;

      if Uninstallable then begin
        { Generate the filenam(UninstLog)es for the uninstall info in the application
          directory }
        SetStatusLabelText(SetupMessages[msgStatusSavingUninstall]);
        GenerateUninstallInfoFilename(UninstLog);
      end;

      { Copy the files }
      SetStatusLabelText(SetupMessages[msgStatusExtractFiles]);
      CopyFiles(UninstLog, ExpandedAppId, RegisterFilesList, Uninstallable);
      ProcessEvents;

      { Create program icons, if any }
      if HasIcons then begin
        SetStatusLabelText(SetupMessages[msgStatusCreateIcons]);
        CreateIcons(UninstLog);
        ProcessEvents;
      end;

      { Create INI entries, if any }
      if Entries[seIni].Count <> 0 then begin
        SetStatusLabelText(SetupMessages[msgStatusCreateIniEntries]);
        CreateIniEntries(UninstLog);
        ProcessEvents;
      end;

      { Create registry entries, if any }
      if Entries[seRegistry].Count <> 0 then begin
        SetStatusLabelText(SetupMessages[msgStatusCreateRegistryEntries]);
        CreateRegistryEntries(UninstLog);
        ProcessEvents;
      end;

      { Call the NeedRestart event function now.
        Note: This can't be done after RegisterFiles, since RegisterFiles
        relies on the setting of the NeedsRestart variable. }
      SetStatusLabelText('');
      ProcessNeedRestartEvent;
      ProcessEvents;

      { Register files, if any }
      if RegisterFilesList.Count <> 0 then begin
        SetStatusLabelText(SetupMessages[msgStatusRegisterFiles]);
        RegisterFiles(RegisterFilesList);
        ProcessEvents;
      end;

      { Save uninstall information. After uninstall info is saved, you cannot
        make any more modifications to the user's system. Any additional
        modifications you want to add must be done before this is called. }
      if Uninstallable then begin
        SetStatusLabelText(SetupMessages[msgStatusSavingUninstall]);
        Log('Saving uninstall information.');
        RenameUninstallExe;
        UninstallMsgCreated := CreateUninstallMsgFile;
        { Register uninstall information so the program can be uninstalled
          through the Add/Remove Programs Control Panel applet. This is done
          on NT 3.51 too, so that the uninstall entry for the app will appear
          if the user later upgrades to NT 4.0+. }
        if EvalDirectiveCheck(SetupHeader.CreateUninstallRegKey) then
          RegisterUninstallInfo(UninstLog, UninstallRegKeyBaseName, AfterInstallFilesSize);
        RecordUninstallRunEntries(UninstLog);
        UninstLog.Add(utEndInstall, [GetLocalTimeAsStr], 0);
        UninstLog.Save(UninstallDataFilename, AppendUninstallData,
          shUpdateUninstallLogAppName in SetupHeader.Options);

        if Debugging then
          DebugNotifyUninstExe(UninstallExeFileName);
      end;
      SetStatusLabelText('');
      UninstLogCleared := True;
      UninstLog.Clear;
    except
      try
        { Show error message, if any, and set the exit code we'll be returning }
        if not(ExceptObject is EAbort) then begin
          Log(Format('Fatal exception during installation process (%s):' + SNewLine,
            [ExceptObject.ClassName]) + GetExceptMessage);
          SetupExitCode := ecInstallationError;
          Application.HandleException(nil);
          LoggedMsgBox(SetupMessages[msgSetupAborted], '', mbCriticalError, MB_OK, True, IDOK);
        end
        else begin
          Log('User canceled the installation process.');
          SetupExitCode := ecInstallationCancelled;
        end;
        { Undo any changes it's made so far }
        if not UninstLogCleared then begin
          Log('Rolling back changes.');
          try
            SetStatusLabelText(SetupMessages[msgStatusRollback]);
            WizardForm.ProgressGauge.Visible := False;
            FinishProgressGauge(True);
            WizardForm.CancelButton.Enabled := False;
            WizardForm.Update;
          except
            { ignore any exceptions, just in case... }
          end;
          if UninstallTempExeFilename <> '' then
            DeleteFile(UninstallTempExeFilename);
          if UninstallExeCreated = ueNew then
            DeleteFile(UninstallExeFilename);
          if UninstallDataCreated then
            DeleteFile(UninstallDataFilename);
          if UninstallMsgCreated then
            DeleteFile(UninstallMsgFilename);
          UninstLog.PerformUninstall(False, nil);
          { Sleep for a bit so that the user has time to read the "Rolling
            back changes" message }
          if WizardForm.Visible then
            Sleep(1500);
        end;
      except
        { No exception should be generated by the above code, but just in
          case, handle any exception now so that Application.Terminate is
          always called below.
          Note that we can't just put Application.Terminate in a finally
          section, because it would prevent the display of an exception
          message box later (MessageBox() dislikes WM_QUIT). }
        Application.HandleException(nil);
      end;
      Exit;
    end;
  finally
    if Assigned(RegisterFilesList) then begin
      for I := RegisterFilesList.Count-1 downto 0 do
        Dispose(PRegisterFilesListRec(RegisterFilesList[I]));
      RegisterFilesList.Free;
    end;
    UninstLog.Free;
    FinishProgressGauge(False);
  end;
  Log('Installation process succeeded.');
  Succeeded := True;
end;

end.
