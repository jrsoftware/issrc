unit Setup.ScriptFunc.HelperFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Helper functions for the script support functions (run time - used by Setup)
}

interface

uses
  Windows,
  uPSRuntime, MD5, SHA1,
  Shared.CommonFunc, Shared.FileClass, Setup.MainForm, Setup.WizardForm,
  Setup.UninstallProgressForm;

type
  { Must keep this in synch with Compiler.ScriptFunc.pas }
  TOnLog = procedure(const S: String; const Error, FirstLine: Boolean) of object;

  { Must keep this in synch with Compiler.ScriptFunc.pas }
  TFindRec = record
    Name: String;
    Attributes: LongWord;
    SizeHigh: LongWord;
    SizeLow: LongWord;
    CreationTime: TFileTime;
    LastAccessTime: TFileTime;
    LastWriteTime: TFileTime;
    AlternateName: String;
    FindHandle: THandle;
  end;

  { Must keep this in synch with Compiler.ScriptFunc.pas }
  TWindowsVersion = packed record
    Major: Cardinal;
    Minor: Cardinal;
    Build: Cardinal;
    ServicePackMajor: Cardinal;
    ServicePackMinor: Cardinal;
    NTPlatform: Boolean;
    ProductType: Byte;
    SuiteMask: Word;
  end;

var
  ScaleBaseUnitX, ScaleBaseUnitY: Integer;

procedure NoUninstallFuncError(const C: AnsiString); overload;
procedure OnlyUninstallFuncError(const C: AnsiString); overload;
function GetWizardForm: TWizardForm;
function GetUninstallProgressForm: TUninstallProgressForm;
function GetMsgBoxCaption: String;
procedure InitializeScaleBaseUnits;
function IsProtectedSrcExe(const Filename: String): Boolean;
function FindFirstHelper(const FileName: String; var FindRec: TFindRec): Boolean;
function FindNextHelper(var FindRec: TFindRec): Boolean;
procedure FindCloseHelper(var FindRec: TFindRec);
function FmtMessageHelper(const S: String; const Args: array of String): String;
procedure GetWindowsVersionExHelper(var Version: TWindowsVersion);
procedure CrackCodeRootKey(CodeRootKey: HKEY; var RegView: TRegView;
  var RootKey: HKEY);
function GetSubkeyOrValueNames(const RegView: TRegView; const RootKey: HKEY;
  const SubKeyName: String; const Stack: TPSStack; const ItemNo: Longint; const Subkey: Boolean): Boolean;
function GetMD5OfFile(const DisableFsRedir: Boolean; const Filename: String): TMD5Digest;
function GetSHA1OfFile(const DisableFsRedir: Boolean; const Filename: String): TSHA1Digest;
function GetMD5OfAnsiString(const S: AnsiString): TMD5Digest;
function GetMD5OfUnicodeString(const S: UnicodeString): TMD5Digest;
function GetSHA1OfAnsiString(const S: AnsiString): TSHA1Digest;
function GetSHA1OfUnicodeString(const S: UnicodeString): TSHA1Digest;
procedure ProcessMessagesProc; far;
procedure ExecAndLogOutputLog(const S: String; const Error, FirstLine: Boolean; const Data: NativeInt);
procedure ExecAndLogOutputLogCustom(const S: String; const Error, FirstLine: Boolean; const Data: NativeInt);
function CustomMessage(const MsgName: String): String;
function NewExtractRelativePath(BaseName, DestName: string): string;
function NewFileSearch(const DisableFsRedir: Boolean;
  const Name, DirList: String): String;
function GetExceptionMessage(const Caller: TPSExec): String;
function GetCodePreviousData(const ExpandedAppID, ValueName, DefaultValueData: String): String;
function SetCodePreviousData(const PreviousDataKey: HKEY; const ValueName, ValueData: String): Boolean;
function LoadStringFromFile(const FileName: String; var S: AnsiString;
  const Sharing: TFileSharing): Boolean;
function LoadStringsFromFile(const FileName: String; const Stack: TPSStack;
  const ItemNo: Longint; const Sharing: TFileSharing): Boolean;
function SaveStringToFile(const FileName: String; const S: AnsiString; Append: Boolean): Boolean;
function SaveStringsToFile(const FileName: String; const Stack: TPSStack;
  const ItemNo: Longint; Append, UTF8, UTF8WithoutBOM: Boolean): Boolean;
function CreateCallback(const Caller: TPSExec; const P: PPSVariantProcPtr): LongWord;

implementation

uses
  Forms, SysUtils, Graphics,
  uPSUtils, PathFunc, ASMInline, PSStackHelper,
  Setup.MainFunc, SetupLdrAndSetup.RedirFunc, Setup.InstFunc,
  SetupLdrAndSetup.Messages, Shared.SetupMessageIDs,
  Shared.SetupTypes, Shared.SetupSteps, Setup.LoggingFunc, Setup.SetupForm;

procedure NoUninstallFuncError(const C: AnsiString); overload;
begin
  InternalError(Format('Cannot call "%s" function during Uninstall', [C]));
end;

procedure OnlyUninstallFuncError(const C: AnsiString); overload;
begin
  InternalError(Format('Cannot call "%s" function during Setup', [C]));
end;

function GetWizardForm: TWizardForm;
begin
  Result := WizardForm;
  if Result = nil then
    InternalError('An attempt was made to access WizardForm before it has been created');
end;

function GetUninstallProgressForm: TUninstallProgressForm;
begin
  Result := UninstallProgressForm;
  if Result = nil then
    InternalError('An attempt was made to access UninstallProgressForm before it has been created');
end;

function GetMsgBoxCaption: String;
var
  ID: TSetupMessageID;
begin
  if IsUninstaller then
    ID := msgUninstallAppTitle
  else
    ID := msgSetupAppTitle;
  Result := SetupMessages[ID];
end;

var
  ScaleBaseUnitsInitialized: Boolean;

procedure InitializeScaleBaseUnits;
var
  Font: TFont;
begin
  if ScaleBaseUnitsInitialized then
    Exit;
  Font := TFont.Create;
  try
    SetFontNameSize(Font, LangOptions.DialogFontName, LangOptions.DialogFontSize,
      '', 8);
    CalculateBaseUnitsFromFont(Font, ScaleBaseUnitX, ScaleBaseUnitY);
  finally
    Font.Free;
  end;
  ScaleBaseUnitsInitialized := True;
end;

function IsProtectedSrcExe(const Filename: String): Boolean;
begin
  if (MainForm = nil) or (MainForm.CurStep < ssInstall) then begin
    var ExpandedFilename := PathExpand(Filename);
    Result := PathCompare(ExpandedFilename, SetupLdrOriginalFilename) = 0;
  end else
    Result := False;
end;

procedure FindDataToFindRec(const FindData: TWin32FindData;
  var FindRec: TFindRec);
begin
  FindRec.Name := FindData.cFileName;
  FindRec.Attributes := FindData.dwFileAttributes;
  FindRec.SizeHigh := FindData.nFileSizeHigh;
  FindRec.SizeLow := FindData.nFileSizeLow;
  FindRec.CreationTime := FindData.ftCreationTime;
  FindRec.LastAccessTime := FindData.ftLastAccessTime;
  FindRec.LastWriteTime := FindData.ftLastWriteTime;
  FindRec.AlternateName := FindData.cAlternateFileName;
end;

function FindFirstHelper(const FileName: String; var FindRec: TFindRec): Boolean;
var
  FindHandle: THandle;
  FindData: TWin32FindData;
begin
  FindHandle := FindFirstFileRedir(ScriptFuncDisableFsRedir, FileName, FindData);
  if FindHandle <> INVALID_HANDLE_VALUE then begin
    FindRec.FindHandle := FindHandle;
    FindDataToFindRec(FindData, FindRec);
    Result := True;
  end
  else begin
    FindRec.FindHandle := 0;
    Result := False;
  end;
end;

function FindNextHelper(var FindRec: TFindRec): Boolean;
var
  FindData: TWin32FindData;
begin
  Result := (FindRec.FindHandle <> 0) and FindNextFile(FindRec.FindHandle, FindData);
  if Result then
    FindDataToFindRec(FindData, FindRec);
end;

procedure FindCloseHelper(var FindRec: TFindRec);
begin
  if FindRec.FindHandle <> 0 then begin
    Windows.FindClose(FindRec.FindHandle);
    FindRec.FindHandle := 0;
  end;
end;

function FmtMessageHelper(const S: String; const Args: array of String): String;
begin
  Result := FmtMessage(PChar(S), Args);
end;

procedure GetWindowsVersionExHelper(var Version: TWindowsVersion);
begin
  Version.Major := WindowsVersion shr 24;
  Version.Minor := (WindowsVersion shr 16) and $FF;
  Version.Build := WindowsVersion and $FFFF;
  Version.ServicePackMajor := Hi(NTServicePackLevel);
  Version.ServicePackMinor := Lo(NTServicePackLevel);
  Version.NTPlatform := True;
  Version.ProductType := WindowsProductType;
  Version.SuiteMask := WindowsSuiteMask;
end;

procedure CrackCodeRootKey(CodeRootKey: HKEY; var RegView: TRegView;
  var RootKey: HKEY);
begin
  if (CodeRootKey and not CodeRootKeyValidFlags) = HKEY_AUTO then begin
    { Change HKA to HKLM or HKCU, keeping our special flag bits. }
    CodeRootKey := (CodeRootKey and CodeRootKeyValidFlags) or InstallModeRootKey;
  end else begin
    { Allow only predefined key handles (8xxxxxxx). Can't accept handles to
      open keys because they might have our special flag bits set.
      Also reject unknown flags which may have a meaning in the future. }
    if (CodeRootKey shr 31 <> 1) or
       ((CodeRootKey and CodeRootKeyFlagMask) and not CodeRootKeyValidFlags <> 0) then
      InternalError('Invalid RootKey value');
  end;

  if CodeRootKey and CodeRootKeyFlag32Bit <> 0 then
    RegView := rv32Bit
  else if CodeRootKey and CodeRootKeyFlag64Bit <> 0 then begin
    if not IsWin64 then
      InternalError('Cannot access 64-bit registry keys on this version of Windows');
    RegView := rv64Bit;
  end
  else
    RegView := InstallDefaultRegView;
  RootKey := CodeRootKey and not CodeRootKeyFlagMask;
end;

function GetSubkeyOrValueNames(const RegView: TRegView; const RootKey: HKEY;
  const SubKeyName: String; const Stack: TPSStack; const ItemNo: Longint; const Subkey: Boolean): Boolean;
const
  samDesired: array [Boolean] of REGSAM = (KEY_QUERY_VALUE, KEY_ENUMERATE_SUB_KEYS);
var
  K: HKEY;
  Buf, S: String;
  BufSize, R: DWORD;
begin
  Result := False;
  SetString(Buf, nil, 512);
  if RegOpenKeyExView(RegView, RootKey, PChar(SubKeyName), 0, samDesired[Subkey], K) <> ERROR_SUCCESS then
    Exit;
  try
    var ArrayBuilder := Stack.InitArrayBuilder(ItemNo);
    while True do begin
      BufSize := Length(Buf);
      if Subkey then
        R := RegEnumKeyEx(K, ArrayBuilder.I, @Buf[1], BufSize, nil, nil, nil, nil)
      else
        R := RegEnumValue(K, ArrayBuilder.I, @Buf[1], BufSize, nil, nil, nil, nil);
      case R of
        ERROR_SUCCESS: ;
        ERROR_NO_MORE_ITEMS: Break;
        ERROR_MORE_DATA:
          begin
            { Double the size of the buffer and try again }
            if Length(Buf) >= 65536 then begin
              { Sanity check: If we tried a 64 KB buffer and it's still saying
                there's more data, something must be seriously wrong. Bail. }
              Exit;
            end;
            SetString(Buf, nil, Length(Buf) * 2);
            Continue;
          end;
      else
        Exit;  { unknown failure... }
      end;
      SetString(S, PChar(@Buf[1]), BufSize);
      ArrayBuilder.Add(S);
    end;
  finally
    RegCloseKey(K);
  end;
  Result := True;
end;

function GetMD5OfFile(const DisableFsRedir: Boolean; const Filename: String): TMD5Digest;
{ Gets MD5 sum of the file Filename. An exception will be raised upon
  failure. }
var
  Buf: array[0..65535] of Byte;
begin
  var Context: TMD5Context;
  MD5Init(Context);
  var F := TFileRedir.Create(DisableFsRedir, Filename, fdOpenExisting, faRead, fsReadWrite);
  try
    while True do begin
      var NumRead := F.Read(Buf, SizeOf(Buf));
      if NumRead = 0 then
        Break;
      MD5Update(Context, Buf, NumRead);
    end;
  finally
    F.Free;
  end;
  Result := MD5Final(Context);
end;

function GetSHA1OfFile(const DisableFsRedir: Boolean; const Filename: String): TSHA1Digest;
{ Gets SHA-1 sum of the file Filename. An exception will be raised upon
  failure. }
var
  Buf: array[0..65535] of Byte;
begin
  var Context: TSHA1Context;
  SHA1Init(Context);
  var F := TFileRedir.Create(DisableFsRedir, Filename, fdOpenExisting, faRead, fsReadWrite);
  try
    while True do begin
      var NumRead := F.Read(Buf, SizeOf(Buf));
      if NumRead = 0 then
        Break;
      SHA1Update(Context, Buf, NumRead);
    end;
  finally
    F.Free;
  end;
  Result := SHA1Final(Context);
end;

function GetMD5OfAnsiString(const S: AnsiString): TMD5Digest;
begin
  Result := MD5Buf(Pointer(S)^, Length(S)*SizeOf(S[1]));
end;

function GetMD5OfUnicodeString(const S: UnicodeString): TMD5Digest;
begin
  Result := MD5Buf(Pointer(S)^, Length(S)*SizeOf(S[1]));
end;

function GetSHA1OfAnsiString(const S: AnsiString): TSHA1Digest;
begin
  Result := SHA1Buf(Pointer(S)^, Length(S)*SizeOf(S[1]));
end;

function GetSHA1OfUnicodeString(const S: UnicodeString): TSHA1Digest;
begin
  Result := SHA1Buf(Pointer(S)^, Length(S)*SizeOf(S[1]));
end;

procedure ProcessMessagesProc; far;
begin
  Application.ProcessMessages;
end;

procedure ExecAndLogOutputLog(const S: String; const Error, FirstLine: Boolean; const Data: NativeInt);
begin
  Log(S);
end;

procedure ExecAndLogOutputLogCustom(const S: String; const Error, FirstLine: Boolean; const Data: NativeInt);
begin
  var OnLog := TOnLog(PMethod(Data)^);
  OnLog(S, Error, FirstLine);
end;

function CustomMessage(const MsgName: String): String;
begin
  if not GetCustomMessageValue(MsgName, Result) then
    InternalError(Format('Unknown custom message name "%s"', [MsgName]));
end;

{ ExtractRelativePath is not in Delphi 2's SysUtils. Use the one from Delphi 7.01. }
function NewExtractRelativePath(BaseName, DestName: string): string;
var
  BasePath, DestPath: string;
  BaseLead, DestLead: PChar;
  BasePtr, DestPtr: PChar;

  function ExtractFilePathNoDrive(const FileName: string): string;
  begin
    Result := PathExtractPath(FileName);
    Delete(Result, 1, Length(PathExtractDrive(FileName)));
  end;

  function Next(var Lead: PChar): PChar;
  begin
    Result := Lead;
    if Result = nil then Exit;
    Lead := PathStrScan(Lead, '\');
    if Lead <> nil then
    begin
      Lead^ := #0;
      Inc(Lead);
    end;
  end;

begin
  { For consistency with the PathExtract* functions, normalize slashes so
    that forward slashes and multiple slashes work with this function also }
  BaseName := PathNormalizeSlashes(BaseName);
  DestName := PathNormalizeSlashes(DestName);

  if PathCompare(PathExtractDrive(BaseName), PathExtractDrive(DestName)) = 0 then
  begin
    BasePath := ExtractFilePathNoDrive(BaseName);
    UniqueString(BasePath);
    DestPath := ExtractFilePathNoDrive(DestName);
    UniqueString(DestPath);
    BaseLead := Pointer(BasePath);
    BasePtr := Next(BaseLead);
    DestLead := Pointer(DestPath);
    DestPtr := Next(DestLead);
    while (BasePtr <> nil) and (DestPtr <> nil) and (PathCompare(BasePtr, DestPtr) = 0) do
    begin
      BasePtr := Next(BaseLead);
      DestPtr := Next(DestLead);
    end;
    Result := '';
    while BaseLead <> nil do
    begin
      Result := Result + '..\';             { Do not localize }
      Next(BaseLead);
    end;
    if (DestPtr <> nil) and (DestPtr^ <> #0) then
      Result := Result + DestPtr + '\';
    if DestLead <> nil then
      Result := Result + DestLead;     // destlead already has a trailing backslash
    Result := Result + PathExtractName(DestName);
  end
  else
    Result := DestName;
end;

{ Use our own FileSearch function which includes these improvements over
  Delphi's version:
  - it supports MBCS and uses Path* functions
  - it uses NewFileExistsRedir instead of FileExists
  - it doesn't search the current directory unless it's told to
  - it always returns a fully-qualified path }
function NewFileSearch(const DisableFsRedir: Boolean;
  const Name, DirList: String): String;
var
  I, P, L: Integer;
begin
  { If Name is absolute, drive-relative, or root-relative, don't search DirList }
  if PathDrivePartLengthEx(Name, True) <> 0 then begin
    Result := PathExpand(Name);
    if NewFileExistsRedir(DisableFsRedir, Result) then
      Exit;
  end
  else begin
    P := 1;
    L := Length(DirList);
    while True do begin
      while (P <= L) and (DirList[P] = ';') do
        Inc(P);
      if P > L then
        Break;
      I := P;
      while (P <= L) and (DirList[P] <> ';') do
        Inc(P, PathCharLength(DirList, P));
      Result := PathExpand(PathCombine(Copy(DirList, I, P - I), Name));
      if NewFileExistsRedir(DisableFsRedir, Result) then
        Exit;
    end;
  end;
  Result := '';
end;

function GetExceptionMessage(const Caller: TPSExec): String;
var
  Code: TPSError;
  E: TObject;
begin
  Code := Caller.LastEx;
  if Code = erNoError then
    Result := '(There is no current exception)'
  else begin
    E := Caller.LastExObject;
    if Assigned(E) and (E is Exception) then
      Result := Exception(E).Message
    else
      Result := String(PSErrorToString(Code, Caller.LastExParam));
  end;
end;

function GetCodePreviousData(const ExpandedAppID, ValueName, DefaultValueData: String): String;
begin
  { do not localize or change the following string }
  Result := GetPreviousData(ExpandedAppId, 'Inno Setup CodeFile: ' + ValueName, DefaultValueData);
end;

{ Also see RegisterUninstallInfo in Install.pas }
function SetCodePreviousData(const PreviousDataKey: HKEY; const ValueName, ValueData: String): Boolean;
begin
  if ValueData <> '' then begin
    { do not localize or change the following string }
    Result := RegSetValueEx(PreviousDataKey, PChar('Inno Setup CodeFile: ' + ValueName), 0, REG_SZ, PChar(ValueData), (Length(ValueData)+1)*SizeOf(ValueData[1])) = ERROR_SUCCESS
  end else
    Result := True;
end;

function LoadStringFromFile(const FileName: String; var S: AnsiString;
  const Sharing: TFileSharing): Boolean;
var
  F: TFile;
  N: Cardinal;
begin
  try
    F := TFileRedir.Create(ScriptFuncDisableFsRedir, FileName, fdOpenExisting, faRead, Sharing);
    try
      N := F.CappedSize;
      SetLength(S, N);
      F.ReadBuffer(S[1], N);
    finally
      F.Free;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function LoadStringsFromFile(const FileName: String; const Stack: TPSStack;
  const ItemNo: Longint; const Sharing: TFileSharing): Boolean;
var
  F: TTextFileReader;
begin
  try
    F := TTextFileReaderRedir.Create(ScriptFuncDisableFsRedir, FileName, fdOpenExisting, faRead, Sharing);
    try
      var ArrayBuilder := Stack.InitArrayBuilder(ItemNo);
      while not F.Eof do
        ArrayBuilder.Add(F.ReadLine);
    finally
      F.Free;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function SaveStringToFile(const FileName: String; const S: AnsiString; Append: Boolean): Boolean;
var
  F: TFile;
begin
  try
    if Append then
      F := TFileRedir.Create(ScriptFuncDisableFsRedir, FileName, fdOpenAlways, faWrite, fsNone)
    else
      F := TFileRedir.Create(ScriptFuncDisableFsRedir, FileName, fdCreateAlways, faWrite, fsNone);
    try
      F.SeekToEnd;
      F.WriteAnsiString(S);
    finally
      F.Free;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function SaveStringsToFile(const FileName: String; const Stack: TPSStack;
  const ItemNo: Longint; Append, UTF8, UTF8WithoutBOM: Boolean): Boolean;
var
  F: TTextFileWriter;
begin
  try
    if Append then
      F := TTextFileWriterRedir.Create(ScriptFuncDisableFsRedir, FileName, fdOpenAlways, faWrite, fsNone)
    else
      F := TTextFileWriterRedir.Create(ScriptFuncDisableFsRedir, FileName, fdCreateAlways, faWrite, fsNone);
    try
      if UTF8 and UTF8WithoutBOM then
        F.UTF8WithoutBOM := UTF8WithoutBOM;
      var ArrayEnumerator := Stack.InitArrayEnumerator(ItemNo);
      while ArrayEnumerator.HasNext do begin
        var S := ArrayEnumerator.Next;
        if not UTF8 then
          F.WriteAnsiLine(AnsiString(S))
        else
          F.WriteLine(S);
      end;
    finally
      F.Free;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

var
  ASMInliners: array of Pointer;

function CreateCallback(const Caller: TPSExec; const P: PPSVariantProcPtr): LongWord;
var
  ProcRec: TPSInternalProcRec;
  Method: TMethod;
  Inliner: TASMInline;
  ParamCount, SwapFirst, SwapLast: Integer;
  S: tbtstring;
begin
  { ProcNo 0 means nil was passed by the script }
  if P.ProcNo = 0 then
    InternalError('Invalid Method value');

  { Calculate parameter count of our proc, will need this later. }
  ProcRec := Caller.GetProcNo(P.ProcNo) as TPSInternalProcRec;
  S := ProcRec.ExportDecl;
  GRFW(S);
  ParamCount := 0;
  while S <> '' do begin
    Inc(ParamCount);
    GRFW(S);
  end;

  { Turn our proc into a callable TMethod - its Code will point to
    ROPS' MyAllMethodsHandler and its Data to a record identifying our proc.
    When called, MyAllMethodsHandler will use the record to call our proc. }
  Method := MkMethod(Caller, P.ProcNo);

  { Wrap our TMethod with a dynamically generated stdcall callback which will
    do two things:
    -Remember the Data pointer which MyAllMethodsHandler needs.
    -Handle the calling convention mismatch.

    Based on InnoCallback by Sherlock Software, see
    http://www.sherlocksoftware.org/page.php?id=54 and
    https://github.com/thenickdude/InnoCallback. }
  Inliner := TASMInline.create;
  try
    Inliner.Pop(EAX); //get the retptr off the stack

    SwapFirst := 2;
    SwapLast := ParamCount-1;

    //Reverse the order of parameters from param3 onwards in the stack
    while SwapLast > SwapFirst do begin
      Inliner.Mov(ECX, Inliner.Addr(ESP, SwapFirst * 4)); //load the first item of the pair
      Inliner.Mov(EDX, Inliner.Addr(ESP, SwapLast * 4)); //load the last item of the pair
      Inliner.Mov(Inliner.Addr(ESP, SwapFirst * 4), EDX);
      Inliner.Mov(Inliner.Addr(ESP, SwapLast * 4), ECX);
      Inc(SwapFirst);
      Dec(SwapLast);
    end;

    if ParamCount >= 1 then
      Inliner.Pop(EDX); //load param1
    if ParamCount >= 2 then
      Inliner.Pop(ECX); //load param2

    Inliner.Push(EAX); //put the retptr back onto the stack

    Inliner.Mov(EAX, LongWord(Method.Data)); //Load the self ptr

    Inliner.Jmp(Method.Code); //jump to the wrapped proc

    SetLength(ASMInliners, Length(ASMInliners) + 1);
    ASMInliners[High(ASMInliners)] := Inliner.SaveAsMemory;
    Result := LongWord(ASMInliners[High(ASMInliners)]);
  finally
    Inliner.Free;
  end;
end;

procedure FreeASMInliners;
var
  I: Integer;
begin
  for I := 0 to High(ASMInliners) do
    FreeMem(ASMInliners[I]);
  SetLength(ASMInliners, 0);
end;

initialization
finalization
  FreeASMInliners;
end.
