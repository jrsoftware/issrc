unit Setup.ScriptFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Script support functions (run time - used by Setup)
}

interface

uses
  uPSRuntime;

procedure ScriptFuncLibraryRegister_R(ScriptInterpreter: TPSExec);

implementation

uses
  Windows, Shared.ScriptFunc,
  Forms, uPSUtils, SysUtils, Classes, Graphics, Controls, TypInfo, ActiveX, Generics.Collections,
  PathFunc, BrowseFunc, MD5, SHA1, SHA256, ASMInline, BitmapImage,
  Shared.Struct, Setup.ScriptDlg, Setup.MainForm, Setup.MainFunc, Shared.CommonFunc.Vcl,
  Shared.CommonFunc, Shared.FileClass, SetupLdrAndSetup.RedirFunc,
  Setup.Install, SetupLdrAndSetup.InstFunc, Setup.InstFunc, Setup.InstFunc.Ole,
  SetupLdrAndSetup.Messages, Shared.SetupMessageIDs, Setup.NewDiskForm,
  Setup.WizardForm, Shared.VerInfoFunc, Shared.SetupTypes, Shared.SetupSteps,
  Shared.Int64Em, Setup.LoggingFunc, Setup.SetupForm, Setup.RegDLL, Setup.Helper,
  Setup.SpawnClient, Setup.UninstallProgressForm, Setup.DotNetFunc,
  Shared.DotNetVersion, Setup.MsiFunc, Compression.SevenZipDecoder,
  Setup.DebugClient;

var
  ScaleBaseUnitsInitialized: Boolean;
  ScaleBaseUnitX, ScaleBaseUnitY: Integer;

procedure NoSetupFuncError(const C: AnsiString); overload;
begin
  InternalError(Format('Cannot call "%s" function during Setup', [C]));
end;

procedure NoUninstallFuncError(const C: AnsiString); overload;
begin
  InternalError(Format('Cannot call "%s" function during Uninstall', [C]));
end;

procedure NoSetupFuncError(const C: UnicodeString); overload;
begin
  InternalError(Format('Cannot call "%s" function during Setup', [C]));
end;

procedure NoUninstallFuncError(const C: UnicodeString); overload;
begin
  InternalError(Format('Cannot call "%s" function during Uninstall', [C]));
end;

function GetMainForm: TMainForm;
begin
  Result := MainForm;
  if Result = nil then
    InternalError('An attempt was made to access MainForm before it has been created'); 
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

{---}

type
  { *Must* keep this in synch with ScriptFunc_C }
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

function _FindFirst(const FileName: String; var FindRec: TFindRec): Boolean;
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

function _FindNext(var FindRec: TFindRec): Boolean;
var
  FindData: TWin32FindData;
begin
  Result := (FindRec.FindHandle <> 0) and FindNextFile(FindRec.FindHandle, FindData);
  if Result then
    FindDataToFindRec(FindData, FindRec);
end;

procedure _FindClose(var FindRec: TFindRec);
begin
  if FindRec.FindHandle <> 0 then begin
    Windows.FindClose(FindRec.FindHandle);
    FindRec.FindHandle := 0;
  end;
end;

function _FmtMessage(const S: String; const Args: array of String): String;
begin
  Result := FmtMessage(PChar(S), Args);
end;

type
  { *Must* keep this in synch with ScriptFunc_C }
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

procedure _GetWindowsVersionEx(var Version: TWindowsVersion);
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

{---}

type
  TPSStackHelper = class helper for TPSStack
  private
    function GetArray(const ItemNo, FieldNo: Longint; out N: Integer): TPSVariantIFC;
    function SetArray(const ItemNo, FieldNo: Longint; const N: Integer): TPSVariantIFC; overload;
  public
    type
      TArrayOfInteger = array of Integer;
      TArrayOfString = array of String;
      TArrayBuilder = record
        Arr: TPSVariantIFC;
        I: Integer;
        procedure Add(const Data: String);
      end;
      TArrayEnumerator = record
        Arr: TPSVariantIFC;
        N, I: Integer;
        function HasNext: Boolean;
        function Next: String;
      end;
    function GetIntArray(const ItemNo: Longint; const FieldNo: Longint = -1): TArrayOfInteger;
    function GetProc(const ItemNo: Longint; const Exec: TPSExec): TMethod;
    function GetStringArray(const ItemNo: Longint; const FieldNo: Longint = -1): TArrayOfString;
    function InitArrayBuilder(const ItemNo: LongInt; const FieldNo: Longint = -1): TArrayBuilder;
    function InitArrayEnumerator(const ItemNo: LongInt; const FieldNo: Longint = -1): TArrayEnumerator;
    procedure SetArray(const ItemNo: Longint; const Data: TArray<String>; const FieldNo: Longint = -1); overload;
    procedure SetArray(const ItemNo: Longint; const Data: TStrings; const FieldNo: Longint = -1); overload;
    procedure SetInt(const ItemNo: Longint; const Data: Integer; const FieldNo: Longint = -1);
  end;

function TPSStackHelper.GetArray(const ItemNo, FieldNo: Longint;
  out N: Integer): TPSVariantIFC;
begin
  if FieldNo >= 0 then
    Result := NewTPSVariantRecordIFC(Items[ItemNo], FieldNo)
  else
    Result := NewTPSVariantIFC(Items[ItemNo], True);
  N := PSDynArrayGetLength(Pointer(Result.Dta^), Result.aType);
end;

function TPSStackHelper.SetArray(const ItemNo, FieldNo: Longint;
  const N: Integer): TPSVariantIFC;
begin
  if FieldNo >= 0 then
    Result := NewTPSVariantRecordIFC(Items[ItemNo], FieldNo)
  else
    Result := NewTPSVariantIFC(Items[ItemNo], True);
  PSDynArraySetLength(Pointer(Result.Dta^), Result.aType, N);
end;

function TPSStackHelper.GetIntArray(const ItemNo, FieldNo: Longint): TArrayOfInteger;
begin
  var N: Integer;
  var Arr := GetArray(ItemNo, FieldNo, N);
  SetLength(Result, N);
  for var I := 0 to N-1 do
    Result[I] := VNGetInt(PSGetArrayField(Arr, I));
end;

function TPSStackHelper.GetProc(const ItemNo: Longint; const Exec: TPSExec): TMethod;
begin
  var P := PPSVariantProcPtr(Items[ItemNo]);
  { ProcNo 0 means nil was passed by the script and GetProcAsMethod will then return a (nil, nil) TMethod }
  Result := Exec.GetProcAsMethod(P.ProcNo);
end;

function TPSStackHelper.GetStringArray(const ItemNo, FieldNo: Longint): TArrayOfString;
begin
  var N: Integer;
  var Arr := GetArray(ItemNo, FieldNo, N);
  SetLength(Result, N);
  for var I := 0 to N-1 do
    Result[I] := VNGetString(PSGetArrayField(Arr, I));
end;

function TPSStackHelper.InitArrayBuilder(const ItemNo, FieldNo: Longint): TArrayBuilder;
begin
  Result.Arr := SetArray(ItemNo, FieldNo, 0);
  Result.I := 0;
end;

procedure TPSStackHelper.TArrayBuilder.Add(const Data: String);
begin
  PSDynArraySetLength(Pointer(Arr.Dta^), Arr.aType, I+1);
  VNSetString(PSGetArrayField(Arr, I), Data);
  Inc(I);
end;

function TPSStackHelper.InitArrayEnumerator(const ItemNo, FieldNo: Longint): TArrayEnumerator;
begin
  Result.Arr := GetArray(ItemNo, FieldNo, Result.N);
  Result.I := 0;
end;

function TPSStackHelper.TArrayEnumerator.HasNext: Boolean;
begin
  Result := I < N;
end;

function TPSStackHelper.TArrayEnumerator.Next: String;
begin
  Result := VNGetString(PSGetArrayField(Arr, I));
  Inc(I);
end;

procedure TPSStackHelper.SetArray(const ItemNo: Longint; const Data: TArray<String>; const FieldNo: Longint);
begin
  var N := System.Length(Data);
  var Arr := SetArray(ItemNo, FieldNo, N);
  for var I := 0 to N-1 do
    VNSetString(PSGetArrayField(Arr, I), Data[I]);
end;

procedure TPSStackHelper.SetArray(const ItemNo: Longint; const Data: TStrings; const FieldNo: Longint);
begin
  var N := Data.Count;
  var Arr := SetArray(ItemNo, FieldNo, N);
  for var I := 0 to N-1 do
    VNSetString(PSGetArrayField(Arr, I), Data[I]);
end;

procedure TPSStackHelper.SetInt(const ItemNo: Longint; const Data: Integer;
  const FieldNo: Longint);
begin
  if FieldNo = -1 then
    inherited SetInt(ItemNo, Data)
  else begin
    var PSVariantIFC := NewTPSVariantRecordIFC(Items[ItemNo], FieldNo);
    VNSetInt(PSVariantIFC, Data);
  end;
end;

{---}

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

type
  { These must keep this in synch with Compiler.ScriptFunc.pas }
  TOnLog = procedure(const S: String; const Error, FirstLine: Boolean) of object;

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

{---}

type
  TScriptFunc = reference to procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal);
  TScriptFuncs = TDictionary<AnsiString, TScriptFunc>;
var
  ScriptFuncs: TScriptFuncs;

function ScriptFuncPSProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  var ScriptFunc: TScriptFunc;
  Result := ScriptFuncs.TryGetValue(Proc.Name, ScriptFunc);
  if Result then
    ScriptFunc(Caller, Proc.Name, Stack, Stack.Count-1);
end;

procedure ScriptFuncLibraryRegister_R(ScriptInterpreter: TPSExec);
{$IFDEF DEBUG}
var
  Count: Integer;
{$ENDIF}

  procedure RegisterScriptFunc(const Name: AnsiString; const ScriptFunc: TScriptFunc); overload;
  begin
    ScriptFuncs.Add(FastUpperCase(Name), ScriptFunc);
    ScriptInterpreter.RegisterFunctionName(Name, ScriptFuncPSProc, nil, nil);
    {$IFDEF DEBUG}
    Inc(Count);
    {$ENDIF}
  end;

  procedure RegisterScriptFunc(const Names: array of AnsiString; const ScriptFunc: TScriptFunc); overload;
  begin
    for var Name in Names do
      RegisterScriptFunc(Name, ScriptFunc);
  end;

  procedure RegisterScriptDlgScriptFuncs;
  begin
    RegisterScriptFunc('PAGEFROMID', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      Stack.SetClass(PStart, GetWizardForm.PageFromID(Stack.GetInt(PStart-1)));
    end);
    RegisterScriptFunc('PAGEINDEXFROMID', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      Stack.SetInt(PStart, GetWizardForm.PageIndexFromID(Stack.GetInt(PStart-1)));
    end);
    RegisterScriptFunc('CREATECUSTOMPAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      var NewPage := TWizardPage.Create(GetWizardForm);
      try
        NewPage.Caption := Stack.GetString(PStart-2);
        NewPage.Description := Stack.GetString(PStart-3);
        GetWizardForm.AddPage(NewPage, Stack.GetInt(PStart-1));
      except
        NewPage.Free;
        raise;
      end;
      Stack.SetClass(PStart, NewPage);
    end);
    RegisterScriptFunc('CREATEINPUTQUERYPAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      var NewInputQueryPage := TInputQueryWizardPage.Create(GetWizardForm);
      try
        NewInputQueryPage.Caption := Stack.GetString(PStart-2);
        NewInputQueryPage.Description := Stack.GetString(PStart-3);
        GetWizardForm.AddPage(NewInputQueryPage, Stack.GetInt(PStart-1));
        NewInputQueryPage.Initialize(Stack.GetString(PStart-4));
      except
        NewInputQueryPage.Free;
        raise;
      end;
      Stack.SetClass(PStart, NewInputQueryPage);
    end);
    RegisterScriptFunc('CREATEINPUTOPTIONPAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      var NewInputOptionPage := TInputOptionWizardPage.Create(GetWizardForm);
      try
        NewInputOptionPage.Caption := Stack.GetString(PStart-2);
        NewInputOptionPage.Description := Stack.GetString(PStart-3);
        GetWizardForm.AddPage(NewInputOptionPage, Stack.GetInt(PStart-1));
        NewInputOptionPage.Initialize(Stack.GetString(PStart-4),
          Stack.GetBool(PStart-5), Stack.GetBool(PStart-6));
      except
        NewInputOptionPage.Free;
        raise;
      end;
      Stack.SetClass(PStart, NewInputOptionPage);
    end);
    RegisterScriptFunc('CREATEINPUTDIRPAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      var NewInputDirPage := TInputDirWizardPage.Create(GetWizardForm);
      try
        NewInputDirPage.Caption := Stack.GetString(PStart-2);
        NewInputDirPage.Description := Stack.GetString(PStart-3);
        GetWizardForm.AddPage(NewInputDirPage, Stack.GetInt(PStart-1));
        NewInputDirPage.Initialize(Stack.GetString(PStart-4), Stack.GetBool(PStart-5),
           Stack.GetString(PStart-6));
      except
        NewInputDirPage.Free;
        raise;
      end;
      Stack.SetClass(PStart, NewInputDirPage);
    end);
    RegisterScriptFunc('CREATEINPUTFILEPAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      var NewInputFilePage := TInputFileWizardPage.Create(GetWizardForm);
      try
        NewInputFilePage.Caption := Stack.GetString(PStart-2);
        NewInputFilePage.Description := Stack.GetString(PStart-3);
        GetWizardForm.AddPage(NewInputFilePage, Stack.GetInt(PStart-1));
        NewInputFilePage.Initialize(Stack.GetString(PStart-4));
      except
        NewInputFilePage.Free;
        raise;
      end;
      Stack.SetClass(PStart, NewInputFilePage);
    end);
    RegisterScriptFunc('CREATEOUTPUTMSGPAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      var NewOutputMsgPage := TOutputMsgWizardPage.Create(GetWizardForm);
      try
        NewOutputMsgPage.Caption := Stack.GetString(PStart-2);
        NewOutputMsgPage.Description := Stack.GetString(PStart-3);
        GetWizardForm.AddPage(NewOutputMsgPage, Stack.GetInt(PStart-1));
        NewOutputMsgPage.Initialize(Stack.GetString(PStart-4));
      except
        NewOutputMsgPage.Free;
        raise;
      end;
      Stack.SetClass(PStart, NewOutputMsgPage);
    end);
    RegisterScriptFunc('CREATEOUTPUTMSGMEMOPAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      var NewOutputMsgMemoPage := TOutputMsgMemoWizardPage.Create(GetWizardForm);
      try
        NewOutputMsgMemoPage.Caption := Stack.GetString(PStart-2);
        NewOutputMsgMemoPage.Description := Stack.GetString(PStart-3);
        GetWizardForm.AddPage(NewOutputMsgMemoPage, Stack.GetInt(PStart-1));
        NewOutputMsgMemoPage.Initialize(Stack.GetString(PStart-4),
           Stack.GetAnsiString(PStart-5));
      except
        NewOutputMsgMemoPage.Free;
        raise;
      end;
      Stack.SetClass(PStart, NewOutputMsgMemoPage);
    end);
    RegisterScriptFunc('CREATEOUTPUTPROGRESSPAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      var NewOutputProgressPage := TOutputProgressWizardPage.Create(GetWizardForm);
      try
        NewOutputProgressPage.Caption := Stack.GetString(PStart-1);
        NewOutputProgressPage.Description := Stack.GetString(PStart-2);
        GetWizardForm.AddPage(NewOutputProgressPage, -1);
        NewOutputProgressPage.Initialize;
      except
        NewOutputProgressPage.Free;
        raise;
      end;
      Stack.SetClass(PStart, NewOutputProgressPage);
    end);
    RegisterScriptFunc('CREATEOUTPUTMARQUEEPROGRESSPAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      var NewOutputMarqueeProgressPage := TOutputMarqueeProgressWizardPage.Create(GetWizardForm);
      try
        NewOutputMarqueeProgressPage.Caption := Stack.GetString(PStart-1);
        NewOutputMarqueeProgressPage.Description := Stack.GetString(PStart-2);
        GetWizardForm.AddPage(NewOutputMarqueeProgressPage, -1);
        NewOutputMarqueeProgressPage.Initialize;
      except
        NewOutputMarqueeProgressPage.Free;
        raise;
      end;
      Stack.SetClass(PStart, NewOutputMarqueeProgressPage);
    end);
    RegisterScriptFunc('CREATEDOWNLOADPAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      var NewDownloadPage := TDownloadWizardPage.Create(GetWizardForm);
      try
        NewDownloadPage.Caption := Stack.GetString(PStart-1);
        NewDownloadPage.Description := Stack.GetString(PStart-2);
        GetWizardForm.AddPage(NewDownloadPage, -1);
        NewDownloadPage.Initialize;
        NewDownloadPage.OnDownloadProgress := TOnDownloadProgress(Stack.GetProc(PStart-3, Caller));
      except
        NewDownloadPage.Free;
        raise;
      end;
      Stack.SetClass(PStart, NewDownloadPage);
    end);
    RegisterScriptFunc('CREATEEXTRACTIONPAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      var NewExtractionPage := TExtractionWizardPage.Create(GetWizardForm);
      try
        NewExtractionPage.Caption := Stack.GetString(PStart-1);
        NewExtractionPage.Description := Stack.GetString(PStart-2);
        GetWizardForm.AddPage(NewExtractionPage, -1);
        NewExtractionPage.Initialize;
        NewExtractionPage.OnExtractionProgress := TOnExtractionProgress(Stack.GetProc(PStart-3, Caller));
      except
        NewExtractionPage.Free;
        raise;
      end;
      Stack.SetClass(PStart, NewExtractionPage);
    end);
    RegisterScriptFunc('SCALEX', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      InitializeScaleBaseUnits;
      Stack.SetInt(PStart, MulDiv(Stack.GetInt(PStart-1), ScaleBaseUnitX, OrigBaseUnitX));
    end);
    RegisterScriptFunc('SCALEY', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      InitializeScaleBaseUnits;
      Stack.SetInt(PStart, MulDiv(Stack.GetInt(PStart-1), ScaleBaseUnitY, OrigBaseUnitY));
    end);
    RegisterScriptFunc('CREATECUSTOMFORM', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var NewSetupForm := TSetupForm.CreateNew(nil);
      try
        NewSetupForm.AutoScroll := False;
        NewSetupForm.BorderStyle := bsDialog;
        NewSetupForm.InitializeFont;
      except
        NewSetupForm.Free;
        raise;
      end;
      Stack.SetClass(PStart, NewSetupForm);
    end);
  end;

  procedure RegisterNewDiskFormScriptFuncs;
  var
    S: String;
  begin
    RegisterScriptFunc('SELECTDISK', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      S := Stack.GetString(PStart-3);
      Stack.SetBool(PStart, SelectDisk(Stack.GetInt(PStart-1), Stack.GetString(PStart-2), S));
      Stack.SetString(PStart-3, S);
    end);
  end;

  procedure RegisterBrowseFuncScriptFuncs;
  var
    S: String;
    ParentWnd: HWND;
  begin
    RegisterScriptFunc('BROWSEFORFOLDER', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if Assigned(WizardForm) then
        ParentWnd := WizardForm.Handle
      else
        ParentWnd := 0;
      S := Stack.GetString(PStart-2);
      Stack.SetBool(PStart, BrowseForFolder(Stack.GetString(PStart-1), S, ParentWnd, Stack.GetBool(PStart-3)));
      Stack.SetString(PStart-2, S);
    end);
    RegisterScriptFunc('GETOPENFILENAME', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if Assigned(WizardForm) then
        ParentWnd := WizardForm.Handle
      else
        ParentWnd := 0;
      S := Stack.GetString(PStart-2);
      Stack.SetBool(PStart, NewGetOpenFileName(Stack.GetString(PStart-1), S, Stack.GetString(PStart-3), Stack.GetString(PStart-4), Stack.GetString(PStart-5), ParentWnd));
      Stack.SetString(PStart-2, S);
    end);
    RegisterScriptFunc('GETOPENFILENAMEMULTI', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if Assigned(WizardForm) then
        ParentWnd := WizardForm.Handle
      else
        ParentWnd := 0;
      Stack.SetBool(PStart, NewGetOpenFileNameMulti(Stack.GetString(PStart-1), TStrings(Stack.GetClass(PStart-2)), Stack.GetString(PStart-3), Stack.GetString(PStart-4), Stack.GetString(PStart-5), ParentWnd));
    end);
    RegisterScriptFunc('GETSAVEFILENAME', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if Assigned(WizardForm) then
        ParentWnd := WizardForm.Handle
      else
        ParentWnd := 0;
      S := Stack.GetString(PStart-2);
      Stack.SetBool(PStart, NewGetSaveFileName(Stack.GetString(PStart-1), S, Stack.GetString(PStart-3), Stack.GetString(PStart-4), Stack.GetString(PStart-5), ParentWnd));
      Stack.SetString(PStart-2, S);
    end);
  end;

  procedure RegisterCommonFuncVclScriptFuncs;
  begin
    RegisterScriptFunc('MINIMIZEPATHNAME', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, MinimizePathName(Stack.GetString(PStart-1), TFont(Stack.GetClass(PStart-2)), Stack.GetInt(PStart-3)));
    end);
  end;

  procedure RegisterCommonFuncScriptFuncs;
  var
    ExistingFilename: String;
    RegView: TRegView;
    K, RootKey: HKEY;
    S, N, V: String;
    DataS: AnsiString;
    Typ, ExistingTyp, Data, Size: DWORD;
    I: Integer;
  begin
    RegisterScriptFunc('FILEEXISTS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, NewFileExistsRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('DIREXISTS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, DirExistsRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('FILEORDIREXISTS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, FileOrDirExistsRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('GETINISTRING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetIniString(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetString(PStart-3), Stack.GetString(PStart-4)));
    end);
    RegisterScriptFunc('GETINIINT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, GetIniInt(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetInt(PStart-3), Stack.GetInt(PStart-4), Stack.GetInt(PStart-5), Stack.GetString(PStart-6)));
    end);
    RegisterScriptFunc('GETINIBOOL', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, GetIniBool(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetBool(PStart-3), Stack.GetString(PStart-4)));
    end);
    RegisterScriptFunc('INIKEYEXISTS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IniKeyExists(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetString(PStart-3)));
    end);
    RegisterScriptFunc('ISINISECTIONEMPTY', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsIniSectionEmpty(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('SETINISTRING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SetIniString(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetString(PStart-3), Stack.GetString(PStart-4)));
    end);
    RegisterScriptFunc('SETINIINT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SetIniInt(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetInt(PStart-3), Stack.GetString(PStart-4)));
    end);
    RegisterScriptFunc('SETINIBOOL', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SetIniBool(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetBool(PStart-3), Stack.GetString(PStart-4)));
    end);
    RegisterScriptFunc('DELETEINIENTRY', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      DeleteIniEntry(Stack.GetString(PStart), Stack.GetString(PStart-1), Stack.GetString(PStart-2));
    end);
    RegisterScriptFunc('DELETEINISECTION', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      DeleteIniSection(Stack.GetString(PStart), Stack.GetString(PStart-1));
    end);
    RegisterScriptFunc('GETENV', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetEnv(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('GETCMDTAIL', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetCmdTail());
    end);
    RegisterScriptFunc('PARAMCOUNT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if NewParamsForCode.Count = 0 then
        InternalError('NewParamsForCode not set');
      Stack.SetInt(PStart, NewParamsForCode.Count-1);
    end);
    RegisterScriptFunc('PARAMSTR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      I := Stack.GetInt(PStart-1);
      if (I >= 0) and (I < NewParamsForCode.Count) then
        Stack.SetString(PStart, NewParamsForCode[I])
      else
        Stack.SetString(PStart, '');
    end);
    RegisterScriptFunc('ADDBACKSLASH', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, AddBackslash(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('REMOVEBACKSLASH', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, RemoveBackslash(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('REMOVEBACKSLASHUNLESSROOT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, RemoveBackslashUnlessRoot(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('ADDQUOTES', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, AddQuotes(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('REMOVEQUOTES', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, RemoveQuotes(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('GETSHORTNAME', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetShortNameRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('GETWINDIR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetWinDir());
    end);
    RegisterScriptFunc('GETSYSTEMDIR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetSystemDir());
    end);
    RegisterScriptFunc('GETSYSWOW64DIR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetSysWow64Dir());
    end);
    RegisterScriptFunc('GETSYSNATIVEDIR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetSysNativeDir(IsWin64));
    end);
    RegisterScriptFunc('GETTEMPDIR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetTempDir());
    end);
    RegisterScriptFunc('STRINGCHANGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      S := Stack.GetString(PStart-1);
      Stack.SetInt(PStart, StringChange(S, Stack.GetString(PStart-2), Stack.GetString(PStart-3)));
      Stack.SetString(PStart-1, S);
    end);
    RegisterScriptFunc('STRINGCHANGEEX', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      S := Stack.GetString(PStart-1);
      Stack.SetInt(PStart, StringChangeEx(S, Stack.GetString(PStart-2), Stack.GetString(PStart-3), Stack.GetBool(PStart-4)));
      Stack.SetString(PStart-1, S);
    end);
    RegisterScriptFunc('USINGWINNT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, True);
    end);
    RegisterScriptFunc(['COPYFILE', 'FILECOPY'], procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      ExistingFilename := Stack.GetString(PStart-1);
      if not IsProtectedSrcExe(ExistingFilename) then
        Stack.SetBool(PStart, CopyFileRedir(ScriptFuncDisableFsRedir,
          ExistingFilename, Stack.GetString(PStart-2), Stack.GetBool(PStart-3)))
      else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('CONVERTPERCENTSTR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      S := Stack.GetString(PStart-1);
      Stack.SetBool(PStart, ConvertPercentStr(S));
      Stack.SetString(PStart-1, S);
    end);
    RegisterScriptFunc('REGKEYEXISTS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      S := Stack.GetString(PStart-2);
      if RegOpenKeyExView(RegView, RootKey, PChar(S), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        Stack.SetBool(PStart, True);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGVALUEEXISTS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      S := Stack.GetString(PStart-2);
      if RegOpenKeyExView(RegView, RootKey, PChar(S), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        N := Stack.GetString(PStart-3);
        Stack.SetBool(PStart, RegValueExists(K, PChar(N)));
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGDELETEKEYINCLUDINGSUBKEYS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      S := Stack.GetString(PStart-2);
      Stack.SetBool(PStart, RegDeleteKeyIncludingSubkeys(RegView, RootKey, PChar(S)) = ERROR_SUCCESS);
    end);
    RegisterScriptFunc('REGDELETEKEYIFEMPTY', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      S := Stack.GetString(PStart-2);
      Stack.SetBool(PStart, RegDeleteKeyIfEmpty(RegView, RootKey, PChar(S)) = ERROR_SUCCESS);
    end);
    RegisterScriptFunc('REGDELETEVALUE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      S := Stack.GetString(PStart-2);
      if RegOpenKeyExView(RegView, RootKey, PChar(S), 0, KEY_SET_VALUE, K) = ERROR_SUCCESS then begin
        N := Stack.GetString(PStart-3);
        Stack.SetBool(PStart, RegDeleteValue(K, PChar(N)) = ERROR_SUCCESS);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGGETSUBKEYNAMES', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      Stack.SetBool(PStart, GetSubkeyOrValueNames(RegView, RootKey,
        Stack.GetString(PStart-2), Stack, PStart-3, True));
    end);
    RegisterScriptFunc('REGGETVALUENAMES', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      Stack.SetBool(PStart, GetSubkeyOrValueNames(RegView, RootKey,
        Stack.GetString(PStart-2), Stack, PStart-3, False));
    end);
    RegisterScriptFunc('REGQUERYSTRINGVALUE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      S := Stack.GetString(PStart-2);
      if RegOpenKeyExView(RegView, RootKey, PChar(S), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        N := Stack.GetString(PStart-3);
        S := Stack.GetString(PStart-4);
        Stack.SetBool(PStart, RegQueryStringValue(K, PChar(N), S));
        Stack.SetString(PStart-4, S);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGQUERYMULTISTRINGVALUE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      S := Stack.GetString(PStart-2);
      if RegOpenKeyExView(RegView, RootKey, PChar(S), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        N := Stack.GetString(PStart-3);
        S := Stack.GetString(PStart-4);
        Stack.SetBool(PStart, RegQueryMultiStringValue(K, PChar(N), S));
        Stack.SetString(PStart-4, S);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGQUERYDWORDVALUE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      S := Stack.GetString(PStart-2);
      if RegOpenKeyExView(RegView, RootKey, PChar(S), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        N := Stack.GetString(PStart-3);
        Size := SizeOf(Data);
        if (RegQueryValueEx(K, PChar(N), nil, @Typ, @Data, @Size) = ERROR_SUCCESS) and (Typ = REG_DWORD) then begin
          Stack.SetInt(PStart-4, Data);
          Stack.SetBool(PStart, True);
        end else
          Stack.SetBool(PStart, False);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGQUERYBINARYVALUE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      S := Stack.GetString(PStart-2);
      if RegOpenKeyExView(RegView, RootKey, PChar(S), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        N := Stack.GetString(PStart-3);
        if RegQueryValueEx(K, PChar(N), nil, @Typ, nil, @Size) = ERROR_SUCCESS then begin
          SetLength(DataS, Size);
          if RegQueryValueEx(K, PChar(N), nil, @Typ, @DataS[1], @Size) = ERROR_SUCCESS then begin
            Stack.SetAnsiString(PStart-4, DataS);
            Stack.SetBool(PStart, True);
          end else
            Stack.SetBool(PStart, False);
        end else
          Stack.SetBool(PStart, False);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGWRITESTRINGVALUE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      S := Stack.GetString(PStart-2);
      if RegCreateKeyExView(RegView, RootKey, PChar(S), 0, nil, REG_OPTION_NON_VOLATILE, KEY_QUERY_VALUE or KEY_SET_VALUE, nil, K, nil) = ERROR_SUCCESS then begin
        N := Stack.GetString(PStart-3);
        V := Stack.GetString(PStart-4);
        if (RegQueryValueEx(K, PChar(N), nil, @ExistingTyp, nil, nil) = ERROR_SUCCESS) and (ExistingTyp = REG_EXPAND_SZ) then
          Typ := REG_EXPAND_SZ
        else
          Typ := REG_SZ;
        if RegSetValueEx(K, PChar(N), 0, Typ, PChar(V), (Length(V)+1)*SizeOf(V[1])) = ERROR_SUCCESS then
          Stack.SetBool(PStart, True)
        else
          Stack.SetBool(PStart, False);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGWRITEEXPANDSTRINGVALUE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      S := Stack.GetString(PStart-2);
      if RegCreateKeyExView(RegView, RootKey, PChar(S), 0, nil, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, nil, K, nil) = ERROR_SUCCESS then begin
        N := Stack.GetString(PStart-3);
        V := Stack.GetString(PStart-4);
        if RegSetValueEx(K, PChar(N), 0, REG_EXPAND_SZ, PChar(V), (Length(V)+1)*SizeOf(V[1])) = ERROR_SUCCESS then
          Stack.SetBool(PStart, True)
        else
          Stack.SetBool(PStart, False);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGWRITEMULTISTRINGVALUE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      S := Stack.GetString(PStart-2);
      if RegCreateKeyExView(RegView, RootKey, PChar(S), 0, nil, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, nil, K, nil) = ERROR_SUCCESS then begin
        N := Stack.GetString(PStart-3);
        V := Stack.GetString(PStart-4);
        { Multi-string data requires two null terminators: one after the last
          string, and one to mark the end.
          Delphi's String type is implicitly null-terminated, so only one null
          needs to be added to the end. }
        if (V <> '') and (V[Length(V)] <> #0) then
          V := V + #0;
        if RegSetValueEx(K, PChar(N), 0, REG_MULTI_SZ, PChar(V), (Length(V)+1)*SizeOf(V[1])) = ERROR_SUCCESS then
          Stack.SetBool(PStart, True)
        else
          Stack.SetBool(PStart, False);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGWRITEDWORDVALUE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      S := Stack.GetString(PStart-2);
      if RegCreateKeyExView(RegView, RootKey, PChar(S), 0, nil, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, nil, K, nil) = ERROR_SUCCESS then begin
        N := Stack.GetString(PStart-3);
        Data := Stack.GetInt(PStart-4);
        if RegSetValueEx(K, PChar(N), 0, REG_DWORD, @Data, SizeOf(Data)) = ERROR_SUCCESS then
          Stack.SetBool(PStart, True)
        else
          Stack.SetBool(PStart, False);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGWRITEBINARYVALUE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      S := Stack.GetString(PStart-2);
      if RegCreateKeyExView(RegView, RootKey, PChar(S), 0, nil, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, nil, K, nil) = ERROR_SUCCESS then begin
        N := Stack.GetString(PStart-3);
        DataS := Stack.GetAnsiString(PStart-4);
        if RegSetValueEx(K, PChar(N), 0, REG_BINARY, @DataS[1], Length(DataS)) = ERROR_SUCCESS then
          Stack.SetBool(PStart, True)
        else
          Stack.SetBool(PStart, False);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc(['ISADMIN', 'ISADMINLOGGEDON'], procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsAdmin);
    end);
    RegisterScriptFunc('ISPOWERUSERLOGGEDON', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsPowerUserLoggedOn());
    end);
    RegisterScriptFUnc('ISADMININSTALLMODE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsAdminInstallMode);
    end);
    RegisterScriptFunc('FONTEXISTS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, FontExists(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('GETUILANGUAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, GetUILanguage);
    end);
    RegisterScriptFunc('ADDPERIOD', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, AddPeriod(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('CHARLENGTH', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, PathCharLength(Stack.GetString(PStart-1), Stack.GetInt(PStart-2)));
    end);
    RegisterScriptFunc('SETNTFSCOMPRESSION', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SetNTFSCompressionRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), Stack.GetBool(PStart-2)));
    end);
    RegisterScriptFunc('ISWILDCARD', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsWildcard(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('WILDCARDMATCH', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      S := Stack.GetString(PStart-1);
      N := Stack.GetString(PStart-2);
      Stack.SetBool(PStart, WildcardMatch(PChar(S), PChar(N)));
    end);
  end;

  procedure RegisterInstallScriptFuncs;
  begin
    RegisterScriptFunc('EXTRACTTEMPORARYFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      ExtractTemporaryFile(Stack.GetString(PStart));
    end);
    RegisterScriptFunc('EXTRACTTEMPORARYFILES', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      Stack.SetInt(PStart, ExtractTemporaryFiles(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('DOWNLOADTEMPORARYFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      Stack.SetInt64(PStart, DownloadTemporaryFile(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetString(PStart-3), TOnDownloadProgress(Stack.GetProc(PStart-4, Caller))));
    end);
    RegisterScriptFunc('SETDOWNLOADCREDENTIALS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      SetDownloadCredentials(Stack.GetString(PStart),Stack.GetString(PStart-1));
    end);
    RegisterScriptFunc('DOWNLOADTEMPORARYFILESIZE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      Stack.SetInt64(PStart, DownloadTemporaryFileSize(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('DOWNLOADTEMPORARYFILEDATE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      Stack.SetString(PStart, DownloadTemporaryFileDate(Stack.GetString(PStart-1)));
    end);
  end;

  procedure RegisterInstFuncScriptFuncs;
  var
    Filename: String;
    WindowDisabler: TWindowDisabler;
    ResultCode, ErrorCode: Integer;
    FreeBytes, TotalBytes: Integer64;
  begin
    RegisterScriptFunc('CHECKFORMUTEXES', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, CheckForMutexes(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('DECREMENTSHAREDCOUNT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if Stack.GetBool(PStart-1) then begin
        if not IsWin64 then
          InternalError('Cannot access 64-bit registry keys on this version of Windows');
        Stack.SetBool(PStart, DecrementSharedCount(rv64Bit, Stack.GetString(PStart-2)));
      end
      else
        Stack.SetBool(PStart, DecrementSharedCount(rv32Bit, Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('DELAYDELETEFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      DelayDeleteFile(ScriptFuncDisableFsRedir, Stack.GetString(PStart), Stack.GetInt(PStart-1), 250, 250);
    end);
    RegisterScriptFunc('DELTREE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, DelTree(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), Stack.GetBool(PStart-2), Stack.GetBool(PStart-3), Stack.GetBool(PStart-4), False, nil, nil, nil));
    end);
    RegisterScriptFunc('GENERATEUNIQUENAME', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GenerateUniqueName(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('GETCOMPUTERNAMESTRING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetComputerNameString());
    end);
    RegisterScriptFunc('GETMD5OFFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, MD5DigestToString(GetMD5OfFile(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('GETMD5OFSTRING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, MD5DigestToString(GetMD5OfAnsiString(Stack.GetAnsiString(PStart-1))));
    end);
    RegisterScriptFunc('GETMD5OFUNICODESTRING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, MD5DigestToString(GetMD5OfUnicodeString(Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('GETSHA1OFFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SHA1DigestToString(GetSHA1OfFile(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('GETSHA1OFSTRING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SHA1DigestToString(GetSHA1OfAnsiString(Stack.GetAnsiString(PStart-1))));
    end);
    RegisterScriptFunc('GETSHA1OFUNICODESTRING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SHA1DigestToString(GetSHA1OfUnicodeString(Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('GETSHA256OFFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SHA256DigestToString(GetSHA256OfFile(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('GETSHA256OFSTRING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SHA256DigestToString(GetSHA256OfAnsiString(Stack.GetAnsiString(PStart-1))));
    end);
    RegisterScriptFunc('GETSHA256OFUNICODESTRING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SHA256DigestToString(GetSHA256OfUnicodeString(Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('GETSPACEONDISK', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if GetSpaceOnDisk(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), FreeBytes, TotalBytes) then begin
        if Stack.GetBool(PStart-2) then begin
          Div64(FreeBytes, 1024*1024);
          Div64(TotalBytes, 1024*1024);
        end;
        { Cap at 2 GB, as [Code] doesn't support 64-bit integers }
        if (FreeBytes.Hi <> 0) or (FreeBytes.Lo and $80000000 <> 0) then
          FreeBytes.Lo := $7FFFFFFF;
        if (TotalBytes.Hi <> 0) or (TotalBytes.Lo and $80000000 <> 0) then
          TotalBytes.Lo := $7FFFFFFF;
        Stack.SetUInt(PStart-3, FreeBytes.Lo);
        Stack.SetUInt(PStart-4, TotalBytes.Lo);
        Stack.SetBool(PStart, True);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('GETSPACEONDISK64', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if GetSpaceOnDisk(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), FreeBytes, TotalBytes) then begin
        Stack.SetInt64(PStart-2, Int64(FreeBytes.Hi) shl 32 + FreeBytes.Lo);
        Stack.SetInt64(PStart-3, Int64(TotalBytes.Hi) shl 32 + TotalBytes.Lo);
        Stack.SetBool(PStart, True);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('GETUSERNAMESTRING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetUserNameString());
    end);
    RegisterScriptFunc('INCREMENTSHAREDCOUNT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if Stack.GetBool(PStart) then begin
        if not IsWin64 then
          InternalError('Cannot access 64-bit registry keys on this version of Windows');
        IncrementSharedCount(rv64Bit, Stack.GetString(PStart-1), Stack.GetBool(PStart-2));
      end
      else
        IncrementSharedCount(rv32Bit, Stack.GetString(PStart-1), Stack.GetBool(PStart-2));
    end);
    RegisterScriptFunc(['EXEC', 'EXECASORIGINALUSER', 'EXECANDLOGOUTPUT', 'EXECANDCAPTUREOUTPUT'], procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RunAsOriginalUser := Name = 'EXECASORIGINALUSER';
      var Method: TMethod; { Must stay alive until OutputReader is freed }
      var OutputReader: TCreateProcessOutputReader := nil;
      try
        if Name = 'EXECANDLOGOUTPUT' then begin
          Method := Stack.GetProc(PStart-7, Caller);
          if Method.Code <> nil then
            OutputReader := TCreateProcessOutputReader.Create(ExecAndLogOutputLogCustom, NativeInt(@Method))
          else if GetLogActive then
            OutputReader := TCreateProcessOutputReader.Create(ExecAndLogOutputLog, 0);
        end else if Name = 'EXECANDCAPTUREOUTPUT' then
          OutputReader := TCreateProcessOutputReader.Create(ExecAndLogOutputLog, 0, omCapture);
        var ExecWait := TExecWait(Stack.GetInt(PStart-5));
        if IsUninstaller and RunAsOriginalUser then
          NoUninstallFuncError(Name)
        else if (OutputReader <> nil) and (ExecWait <> ewWaitUntilTerminated) then
          InternalError(Format('Must call "%s" function with Wait = ewWaitUntilTerminated', [Name]));

        Filename := Stack.GetString(PStart-1);
        if not IsProtectedSrcExe(Filename) then begin
          { Disable windows so the user can't utilize our UI during the InstExec
            call }
          WindowDisabler := TWindowDisabler.Create;
          try
            Stack.SetBool(PStart, InstExecEx(RunAsOriginalUser,
              ScriptFuncDisableFsRedir, Filename, Stack.GetString(PStart-2),
              Stack.GetString(PStart-3), ExecWait,
              Stack.GetInt(PStart-4), ProcessMessagesProc, OutputReader, ResultCode));
          finally
            WindowDisabler.Free;
          end;
          Stack.SetInt(PStart-6, ResultCode);
          if Name = 'EXECANDCAPTUREOUTPUT' then begin
            { Set the three TExecOutput fields }
            Stack.SetArray(PStart-7, OutputReader.CaptureOutList, 0);
            Stack.SetArray(PStart-7, OutputReader.CaptureErrList, 1);
            Stack.SetInt(PStart-7, OutputReader.CaptureError.ToInteger, 2);
          end;
        end else begin
          Stack.SetBool(PStart, False);
          Stack.SetInt(PStart-6, ERROR_ACCESS_DENIED);
        end;
      finally
        OutputReader.Free;
      end;
    end);
    RegisterScriptFunc(['SHELLEXEC', 'SHELLEXECASORIGINALUSER'], procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RunAsOriginalUser := Name = 'SHELLEXECASORIGINALUSER';
      if IsUninstaller and RunAsOriginalUser then
        NoUninstallFuncError(Name);

      Filename := Stack.GetString(PStart-2);
      if not IsProtectedSrcExe(Filename) then begin
        { Disable windows so the user can't utilize our UI during the
          InstShellExec call }
        WindowDisabler := TWindowDisabler.Create;
        try
          Stack.SetBool(PStart, InstShellExecEx(RunAsOriginalUser,
            Stack.GetString(PStart-1), Filename, Stack.GetString(PStart-3),
            Stack.GetString(PStart-4), TExecWait(Stack.GetInt(PStart-6)),
            Stack.GetInt(PStart-5), ProcessMessagesProc, ErrorCode));
        finally
          WindowDisabler.Free;
        end;
        Stack.SetInt(PStart-7, ErrorCode);
      end else begin
        Stack.SetBool(PStart, False);
        Stack.SetInt(PStart-7, ERROR_ACCESS_DENIED);
      end;
    end);
    RegisterScriptFunc('ISPROTECTEDSYSTEMFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsProtectedSystemFile(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('MAKEPENDINGFILERENAMEOPERATIONSCHECKSUM', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SHA256DigestToString(MakePendingFileRenameOperationsChecksum));
    end);
    RegisterScriptFunc('MODIFYPIFFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, ModifyPifFile(Stack.GetString(PStart-1), Stack.GetBool(PStart-2)));
    end);
    RegisterScriptFunc('REGISTERSERVER', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      RegisterServer(False, Stack.GetBool(PStart), Stack.GetString(PStart-1), Stack.GetBool(PStart-2));
    end);
    RegisterScriptFunc('UNREGISTERSERVER', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      try
        RegisterServer(True, Stack.GetBool(PStart-1), Stack.GetString(PStart-2), Stack.GetBool(PStart-3));
        Stack.SetBool(PStart, True);
      except
        Stack.SetBool(PStart, False);
      end;
    end);
    RegisterScriptFunc('UNREGISTERFONT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      UnregisterFont(Stack.GetString(PStart), Stack.GetString(PStart-1), Stack.GetBool(PStart-2));
    end);
    RegisterScriptFunc('RESTARTREPLACE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      RestartReplace(ScriptFuncDisableFsRedir, Stack.GetString(PStart), Stack.GetString(PStart-1));
    end);
    RegisterScriptFunc('FORCEDIRECTORIES', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, ForceDirectories(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
  end;

  procedure RegisterInstFuncOleScriptFuncs;
  begin
    RegisterScriptFunc('CREATESHELLLINK', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, CreateShellLink(Stack.GetString(PStart-1),
        Stack.GetString(PStart-2), Stack.GetString(PStart-3),
        Stack.GetString(PStart-4), Stack.GetString(PStart-5),
        Stack.GetString(PStart-6), Stack.GetInt(PStart-7),
        Stack.GetInt(PStart-8), 0, '', nil, False, False));
    end);
    RegisterScriptFunc('REGISTERTYPELIBRARY', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if Stack.GetBool(PStart) then
        HelperRegisterTypeLibrary(False, Stack.GetString(PStart-1))
      else
        RegisterTypeLibrary(Stack.GetString(PStart-1));
    end);
    RegisterScriptFunc('UNREGISTERTYPELIBRARY', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      try
        if Stack.GetBool(PStart-1) then
          HelperRegisterTypeLibrary(True, Stack.GetString(PStart-2))
        else
          UnregisterTypeLibrary(Stack.GetString(PStart-2));
        Stack.SetBool(PStart, True);
      except
        Stack.SetBool(PStart, False);
      end;
    end);
    RegisterScriptFunc('UNPINSHELLLINK', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, UnpinShellLink(Stack.GetString(PStart-1)));
    end);
  end;

  procedure RegisterMainFuncScriptFuncs;
  var
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    StringList: TStringList;
    S: String;
    Components, Suppressible: Boolean;
    Default: Integer;
  begin
    RegisterScriptFunc('ACTIVELANGUAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, ExpandConst('{language}'));
    end);
    RegisterScriptFunc('EXPANDCONSTANT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, ExpandConst(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXPANDCONSTANTEX', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, ExpandConstEx(Stack.GetString(PStart-1), [Stack.GetString(PStart-2), Stack.GetString(PStart-3)]));
    end);
    RegisterScriptFunc('EXITSETUPMSGBOX', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, ExitSetupMsgBox());
    end);
    RegisterScriptFunc('GETSHELLFOLDERBYCSIDL', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetShellFolderByCSIDL(Stack.GetInt(PStart-1), Stack.GetBool(PStart-2)));
    end);
    RegisterScriptFunc('INSTALLONTHISVERSION', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if not StrToSetupVersionData(Stack.GetString(PStart-1), MinVersion) then
        InternalError('InstallOnThisVersion: Invalid MinVersion string')
      else if not StrToSetupVersionData(Stack.GetString(PStart-2), OnlyBelowVersion) then
        InternalError('InstallOnThisVersion: Invalid OnlyBelowVersion string')
      else
        Stack.SetBool(PStart, (InstallOnThisVersion(MinVersion, OnlyBelowVersion) = irInstall));
    end);
    RegisterScriptFunc('GETWINDOWSVERSION', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetUInt(PStart, WindowsVersion);
    end);
    RegisterScriptFunc('GETWINDOWSVERSIONSTRING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, Format('%u.%.2u.%u', [WindowsVersion shr 24,
        (WindowsVersion shr 16) and $FF, WindowsVersion and $FFFF]));
    end);
    RegisterScriptFunc(['MSGBOX', 'SUPPRESSIBLEMSGBOX'], procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if Name = 'MSGBOX' then begin
        Suppressible := False;
        Default := 0;
      end else begin
        Suppressible := True;
        Default := Stack.GetInt(PStart-4);
      end;
      Stack.SetInt(PStart, LoggedMsgBox(Stack.GetString(PStart-1), GetMsgBoxCaption, TMsgBoxType(Stack.GetInt(PStart-2)), Stack.GetInt(PStart-3), Suppressible, Default));
    end);
    RegisterScriptFunc(['TASKDIALOGMSGBOX', 'SUPPRESSIBLETASKDIALOGMSGBOX'], procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if Name = 'TASKDIALOGMSGBOX' then begin
        Suppressible := False;
        Default := 0;
      end else begin
        Suppressible := True;
        Default := Stack.GetInt(PStart-7);
      end;
      var ButtonLabels := Stack.GetStringArray(PStart-5);
      Stack.SetInt(PStart, LoggedTaskDialogMsgBox('', Stack.GetString(PStart-1), Stack.GetString(PStart-2), GetMsgBoxCaption, TMsgBoxType(Stack.GetInt(PStart-3)), Stack.GetInt(PStart-4), ButtonLabels, Stack.GetInt(PStart-6), Suppressible, Default));
    end);
    RegisterScriptFunc('ISWIN64', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsWin64);
    end);
    RegisterScriptFunc('IS64BITINSTALLMODE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, Is64BitInstallMode);
    end);
    RegisterScriptFunc('PROCESSORARCHITECTURE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, Integer(ProcessorArchitecture));
    end);
    RegisterScriptFunc(['ISARM32COMPATIBLE', 'ISARM64', 'ISX64', 'ISX64OS', 'ISX64COMPATIBLE', 'ISX86', 'ISX86OS', 'ISX86COMPATIBLE'], procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var ArchitectureIdentifier := LowerCase(Copy(String(Name), 3, MaxInt));
      Stack.SetBool(PStart, EvalArchitectureIdentifier(ArchitectureIdentifier));
    end);
    RegisterScriptFunc('CUSTOMMESSAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, CustomMessage(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('RMSESSIONSTARTED', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, RmSessionStarted);
    end);
    RegisterScriptFunc('REGISTEREXTRACLOSEAPPLICATIONSRESOURCE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, CodeRegisterExtraCloseApplicationsResource(Stack.GetBool(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('GETMAINFORM', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetClass(PStart, GetMainForm);
    end);
    RegisterScriptFunc('GETWIZARDFORM', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetClass(PStart, GetWizardForm);
    end);
    RegisterScriptFunc(['WIZARDISCOMPONENTSELECTED', 'ISCOMPONENTSELECTED', 'WIZARDISTASKSELECTED', 'ISTASKSELECTED'], procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      StringList := TStringList.Create();
      try
        Components := (Name = 'WIZARDISCOMPONENTSELECTED') or (Name = 'ISCOMPONENTSELECTED');
        if Components then
          GetWizardForm.GetSelectedComponents(StringList, False, False)
        else
          GetWizardForm.GetSelectedTasks(StringList, False, False, False);
        S := Stack.GetString(PStart-1);
        StringChange(S, '/', '\');
        if Components then
          Stack.SetBool(PStart, ShouldProcessEntry(StringList, nil, S, '', '', ''))
        else
          Stack.SetBool(PStart, ShouldProcessEntry(nil, StringList, '', S, '', ''));
      finally
        StringList.Free();
      end;
    end);
  end;

  procedure RegisterMessagesScriptFuncs;
  begin
    RegisterScriptFunc('SETUPMESSAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SetupMessages[TSetupMessageID(Stack.GetInt(PStart-1))]);
    end);
  end;

  procedure RegisterSystemScriptFuncs;
  var
    F: TFile;
    TmpFileSize: Integer64;
  begin
    RegisterScriptFunc('RANDOM', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, Random(Stack.GetInt(PStart-1)));
    end);
    RegisterScriptFunc('FILESIZE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      try
        F := TFileRedir.Create(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), fdOpenExisting, faRead, fsReadWrite);
        try
          Stack.SetInt(PStart-2, F.CappedSize);
          Stack.SetBool(PStart, True);
        finally
          F.Free;
        end;
      except
        Stack.SetBool(PStart, False);
      end;
    end);
    RegisterScriptFunc('FILESIZE64', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      try
        F := TFileRedir.Create(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), fdOpenExisting, faRead, fsReadWrite);
        try
          TmpFileSize := F.Size; { Make sure we access F.Size only once }
          Stack.SetInt64(PStart-2, Int64(TmpFileSize.Hi) shl 32 + TmpFileSize.Lo);
          Stack.SetBool(PStart, True);
        finally
          F.Free;
        end;
      except
        Stack.SetBool(PStart, False);
      end;
    end);
    RegisterScriptFunc('SET8087CW', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Set8087CW(Stack.GetInt(PStart));
    end);
    RegisterScriptFunc('GET8087CW', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, Get8087CW);
    end);
    RegisterScriptFunc('UTF8ENCODE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetAnsiString(PStart, Utf8Encode(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('UTF8DECODE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, UTF8ToString(Stack.GetAnsiString(PStart-1)));
    end);
  end;

  procedure RegisterSysUtilsScriptFuncs;
  var
    OldName: String;
    NewDateSeparator, NewTimeSeparator: Char;
    OldDateSeparator, OldTimeSeparator: Char;
  begin
    RegisterScriptFunc('BEEP', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Beep;
    end);
    RegisterScriptFunc('TRIMLEFT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, TrimLeft(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('TRIMRIGHT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, TrimRight(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('GETCURRENTDIR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetCurrentDir());
    end);
    RegisterScriptFunc('SETCURRENTDIR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SetCurrentDir(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXPANDFILENAME', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, PathExpand(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXPANDUNCFILENAME', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, ExpandUNCFileName(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXTRACTRELATIVEPATH', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, NewExtractRelativePath(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('EXTRACTFILEDIR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, PathExtractDir(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXTRACTFILEDRIVE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, PathExtractDrive(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXTRACTFILEEXT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, PathExtractExt(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXTRACTFILENAME', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, PathExtractName(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXTRACTFILEPATH', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, PathExtractPath(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('CHANGEFILEEXT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, PathChangeExt(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('FILESEARCH', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, NewFileSearch(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('RENAMEFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      OldName := Stack.GetString(PStart-1);
      if not IsProtectedSrcExe(OldName) then
        Stack.SetBool(PStart, MoveFileRedir(ScriptFuncDisableFsRedir, OldName, Stack.GetString(PStart-2)))
      else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('DELETEFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, DeleteFileRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('CREATEDIR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, CreateDirectoryRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('REMOVEDIR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, RemoveDirectoryRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('COMPARESTR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, CompareStr(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('COMPARETEXT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, CompareText(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('SAMESTR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, CompareStr(Stack.GetString(PStart-1), Stack.GetString(PStart-2)) = 0);
    end);
    RegisterScriptFunc('SAMETEXT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, CompareText(Stack.GetString(PStart-1), Stack.GetString(PStart-2)) = 0);
    end);
    RegisterScriptFunc('GETDATETIMESTRING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      OldDateSeparator := FormatSettings.DateSeparator;
      OldTimeSeparator := FormatSettings.TimeSeparator;
      try
        NewDateSeparator := Stack.GetString(PStart-2)[1];
        NewTimeSeparator := Stack.GetString(PStart-3)[1];
        if NewDateSeparator <> #0 then
          FormatSettings.DateSeparator := NewDateSeparator;
        if NewTimeSeparator <> #0 then
          FormatSettings.TimeSeparator := NewTimeSeparator;
        Stack.SetString(PStart, FormatDateTime(Stack.GetString(PStart-1), Now()));
      finally
        FormatSettings.TimeSeparator := OldTimeSeparator;
        FormatSettings.DateSeparator := OldDateSeparator;
      end;
    end);
    RegisterScriptFunc('SYSERRORMESSAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, Win32ErrorString(Stack.GetInt(PStart-1)));
    end);
  end;

  procedure RegisterVerInfoFuncScriptFuncs;
  var
    VersionNumbers: TFileVersionNumbers;
  begin
    RegisterScriptFunc('GETVERSIONNUMBERS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if GetVersionNumbersRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), VersionNumbers) then begin
        Stack.SetInt(PStart-2, VersionNumbers.MS);
        Stack.SetInt(PStart-3, VersionNumbers.LS);
        Stack.SetBool(PStart, True);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('GETVERSIONCOMPONENTS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if GetVersionNumbersRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), VersionNumbers) then begin
        Stack.SetUInt(PStart-2, VersionNumbers.MS shr 16);
        Stack.SetUInt(PStart-3, VersionNumbers.MS and $FFFF);
        Stack.SetUInt(PStart-4, VersionNumbers.LS shr 16);
        Stack.SetUInt(PStart-5, VersionNumbers.LS and $FFFF);
        Stack.SetBool(PStart, True);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('GETVERSIONNUMBERSSTRING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if GetVersionNumbersRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), VersionNumbers) then begin
        Stack.SetString(PStart-2, Format('%u.%u.%u.%u', [VersionNumbers.MS shr 16,
          VersionNumbers.MS and $FFFF, VersionNumbers.LS shr 16, VersionNumbers.LS and $FFFF]));
        Stack.SetBool(PStart, True);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('GETPACKEDVERSION', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if GetVersionNumbersRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), VersionNumbers) then begin
        Stack.SetInt64(PStart-2, (Int64(VersionNumbers.MS) shl 32) or VersionNumbers.LS);
        Stack.SetBool(PStart, True);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('PACKVERSIONNUMBERS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt64(PStart, Int64((UInt64(Stack.GetUInt(PStart-1)) shl 32) or Stack.GetUInt(PStart-2)));
    end);
    RegisterScriptFunc('PACKVERSIONCOMPONENTS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      VersionNumbers.MS := (Stack.GetUInt(PStart-1) shl 16) or (Stack.GetUInt(PStart-2) and $FFFF);
      VersionNumbers.LS := (Stack.GetUInt(PStart-3) shl 16) or (Stack.GetUInt(PStart-4) and $FFFF);
      Stack.SetInt64(PStart, Int64((UInt64(VersionNumbers.MS) shl 32) or VersionNumbers.LS));
    end);
    RegisterScriptFunc('COMPAREPACKEDVERSION', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, Compare64(Integer64(Stack.GetInt64(PStart-1)), Integer64(Stack.GetInt64(PStart-2))));
    end);
    RegisterScriptFunc('SAMEPACKEDVERSION', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, Compare64(Integer64(Stack.GetInt64(PStart-1)), Integer64(Stack.GetInt64(PStart-2))) = 0);
    end);
    RegisterScriptFunc('UNPACKVERSIONNUMBERS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      VersionNumbers.MS := UInt64(Stack.GetInt64(PStart)) shr 32;
      VersionNumbers.LS := UInt64(Stack.GetInt64(PStart)) and $FFFFFFFF;
      Stack.SetUInt(PStart-1, VersionNumbers.MS);
      Stack.SetUInt(PStart-2, VersionNumbers.LS);
    end);
    RegisterScriptFunc('UNPACKVERSIONCOMPONENTS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      VersionNumbers.MS := UInt64(Stack.GetInt64(PStart)) shr 32;
      VersionNumbers.LS := UInt64(Stack.GetInt64(PStart)) and $FFFFFFFF;
      Stack.SetUInt(PStart-1, VersionNumbers.MS shr 16);
      Stack.SetUInt(PStart-2, VersionNumbers.MS and $FFFF);
      Stack.SetUInt(PStart-3, VersionNumbers.LS shr 16);
      Stack.SetUInt(PStart-4, VersionNumbers.LS and $FFFF);
    end);
    RegisterScriptFunc('VERSIONTOSTR', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      VersionNumbers.MS := UInt64(Stack.GetInt64(PStart-1)) shr 32;
      VersionNumbers.LS := UInt64(Stack.GetInt64(PStart-1)) and $FFFFFFFF;
      Stack.SetString(PStart, Format('%u.%u.%u.%u', [VersionNumbers.MS shr 16,
        VersionNumbers.MS and $FFFF, VersionNumbers.LS shr 16, VersionNumbers.LS and $FFFF]));
    end);
    RegisterScriptFunc('STRTOVERSION', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if StrToVersionNumbers(Stack.GetString(PStart-1), VersionNumbers) then begin
        Stack.SetInt64(PStart-2, (Int64(VersionNumbers.MS) shl 32) or VersionNumbers.LS);
        Stack.SetBool(PStart, True);
      end else
        Stack.SetBool(PStart, False);
    end);
  end;

  type
    TDllProc = function(const Param1, Param2: Longint): Longint; stdcall;

  procedure RegisterWindowsScriptFuncs;
  var
    DllProc: TDllProc;
    DllHandle: THandle;
    S: AnsiString;
  begin
    RegisterScriptFunc('SLEEP', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Sleep(Stack.GetInt(PStart));
    end);
    RegisterScriptFunc('FINDWINDOWBYCLASSNAME', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, FindWindow(PChar(Stack.GetString(PStart-1)), nil));
    end);
    RegisterScriptFunc('FINDWINDOWBYWINDOWNAME', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, FindWindow(nil, PChar(Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('SENDMESSAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, SendMessage(Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3), Stack.GetInt(PStart-4)));
    end);
    RegisterScriptFunc('POSTMESSAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, PostMessage(Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3), Stack.GetInt(PStart-4)));
    end);
    RegisterScriptFunc('SENDNOTIFYMESSAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SendNotifyMessage(Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3), Stack.GetInt(PStart-4)));
    end);
    RegisterScriptFunc('REGISTERWINDOWMESSAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, RegisterWindowMessage(PChar(Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('SENDBROADCASTMESSAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, SendMessage(HWND_BROADCAST, Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3)));
    end);
    RegisterScriptFunc('POSTBROADCASTMESSAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, PostMessage(HWND_BROADCAST, Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3)));
    end);
    RegisterScriptFunc('SENDBROADCASTNOTIFYMESSAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SendNotifyMessage(HWND_BROADCAST, Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3)));
    end);
    RegisterScriptFunc('LOADDLL', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      DllHandle := SafeLoadLibrary(Stack.GetString(PStart-1), SEM_NOOPENFILEERRORBOX);
      if DllHandle <> 0 then
        Stack.SetInt(PStart-2, 0)
      else
        Stack.SetInt(PStart-2, GetLastError());
      Stack.SetInt(PStart, DllHandle);
    end);
    RegisterScriptFunc('CALLDLLPROC', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      @DllProc := GetProcAddress(Stack.GetInt(PStart-1), PChar(Stack.GetString(PStart-2)));
      if Assigned(DllProc) then begin
        Stack.SetInt(PStart-5, DllProc(Stack.GetInt(PStart-3), Stack.GetInt(PStart-4)));
        Stack.SetBool(PStart, True);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('FREEDLL', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, FreeLibrary(Stack.GetInt(PStart-1)));
    end);
    RegisterScriptFunc('CREATEMUTEX', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Windows.CreateMutex(nil, False, PChar(Stack.GetString(PStart)));
    end);
    RegisterScriptFunc('OEMTOCHARBUFF', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      S := Stack.GetAnsiString(PStart);
      OemToCharBuffA(PAnsiChar(S), PAnsiChar(S), Length(S));
      Stack.SetAnsiString(PStart, S);
    end);
    RegisterScriptFunc('CHARTOOEMBUFF', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      S := Stack.GetAnsiString(PStart);
      CharToOemBuffA(PAnsiChar(S), PAnsiChar(S), Length(S));
      Stack.SetAnsiString(PStart, S);
    end);
  end;

  procedure RegisterOle2ScriptFuncs;
  begin
    RegisterScriptFunc('COFREEUNUSEDLIBRARIES', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CoFreeUnusedLibraries;
    end);
  end;

  procedure RegisterLoggingFuncScriptFuncs;
  begin
    RegisterScriptFunc('LOG', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Log(Stack.GetString(PStart));
    end);
  end;

  procedure RegisterOtherScriptFuncs;
  var
    TypeEntry: PSetupTypeEntry;
    StringList: TStringList;
    S: String;
    AnsiS: AnsiString;
    ErrorCode: Cardinal;
  begin
    RegisterScriptFunc('BRINGTOFRONTANDRESTORE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Application.BringToFront();
      Application.Restore();
    end);
    RegisterScriptFunc('WIZARDDIRVALUE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      Stack.SetString(PStart, RemoveBackslashUnlessRoot(GetWizardForm.DirEdit.Text));
    end);
    RegisterScriptFunc('WIZARDGROUPVALUE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      Stack.SetString(PStart, RemoveBackslashUnlessRoot(GetWizardForm.GroupEdit.Text));
    end);
    RegisterScriptFunc('WIZARDNOICONS', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      Stack.SetBool(PStart, GetWizardForm.NoIconsCheck.Checked);
    end);
    RegisterScriptFunc('WIZARDSETUPTYPE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      TypeEntry := GetWizardForm.GetSetupType();
      if TypeEntry <> nil then begin
        if Stack.GetBool(PStart-1) then
          Stack.SetString(PStart, TypeEntry.Description)
        else
          Stack.SetString(PStart, TypeEntry.Name);
      end
      else
        Stack.SetString(PStart, '');
    end);
    RegisterScriptFunc(['WIZARDSELECTEDCOMPONENTS', 'WIZARDSELECTEDTASKS'], procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      StringList := TStringList.Create();
      try
        if Name = 'WIZARDSELECTEDCOMPONENTS' then
          GetWizardForm.GetSelectedComponents(StringList, Stack.GetBool(PStart-1), False)
        else
          GetWizardForm.GetSelectedTasks(StringList, Stack.GetBool(PStart-1), False, False);
        Stack.SetString(PStart, StringsToCommaString(StringList));
      finally
        StringList.Free();
      end;
    end);
    RegisterScriptFunc(['WIZARDSELECTCOMPONENTS', 'WIZARDSELECTTASKS'], procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      StringList := TStringList.Create();
      try
        S := Stack.GetString(PStart);
        StringChange(S, '/', '\');
        SetStringsFromCommaString(StringList, S);
        if Name = 'WIZARDSELECTCOMPONENTS' then
          GetWizardForm.SelectComponents(StringList)
        else
          GetWizardForm.SelectTasks(StringList);
      finally
        StringList.Free();
      end;
    end);
    RegisterScriptFunc('WIZARDSILENT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      Stack.SetBool(PStart, InstallMode <> imNormal);
    end);
    RegisterScriptFunc('ISUNINSTALLER', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsUninstaller);
    end);
    RegisterScriptFunc('UNINSTALLSILENT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if not IsUninstaller then
        NoSetupFuncError(Name);
      Stack.SetBool(PStart, UninstallSilent);
    end);
    RegisterScriptFunc('CURRENTFILENAME', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      if CheckOrInstallCurrentFilename <> '' then
        Stack.SetString(PStart, CheckOrInstallCurrentFilename)
      else
        InternalError('An attempt was made to call the "CurrentFilename" function from outside a "Check", "BeforeInstall" or "AfterInstall" event function belonging to a "[Files]" entry');
    end);
    RegisterScriptFunc('CURRENTSOURCEFILENAME', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        NoUninstallFuncError(Name);
      if CheckOrInstallCurrentSourceFilename <> '' then
        Stack.SetString(PStart, CheckOrInstallCurrentSourceFilename)
      else
        InternalError('An attempt was made to call the "CurrentSourceFilename" function from outside a "Check", "BeforeInstall" or "AfterInstall" event function belonging to a "[Files]" entry with flag "external"');
    end);
    RegisterScriptFunc('CASTSTRINGTOINTEGER', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, Integer(PChar(Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('CASTINTEGERTOSTRING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, String(PChar(Stack.GetInt(PStart-1))));
    end);
    RegisterScriptFunc('ABORT', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Abort;
    end);
    RegisterScriptFunc('GETEXCEPTIONMESSAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetExceptionMessage(Caller));
    end);
    RegisterScriptFunc('RAISEEXCEPTION', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      raise Exception.Create(Stack.GetString(PStart));
    end);
    RegisterScriptFunc('SHOWEXCEPTIONMESSAGE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      TMainForm.ShowExceptionMsg(AddPeriod(GetExceptionMessage(Caller)));
    end);
    RegisterScriptFunc('TERMINATED', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, Application.Terminated);
    end);
    RegisterScriptFunc('GETPREVIOUSDATA', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        Stack.SetString(PStart, GetCodePreviousData(UninstallExpandedAppId, Stack.GetString(PStart-1), Stack.GetString(PStart-2)))
      else
        Stack.SetString(PStart, GetCodePreviousData(ExpandConst(SetupHeader.AppId), Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('SETPREVIOUSDATA', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SetCodePreviousData(Stack.GetInt(PStart-1), Stack.GetString(PStart-2), Stack.GetString(PStart-3)));
    end);
    RegisterScriptFunc('LOADSTRINGFROMFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      AnsiS := Stack.GetAnsiString(PStart-2);
      Stack.SetBool(PStart, LoadStringFromFile(Stack.GetString(PStart-1), AnsiS, fsRead));
      Stack.SetAnsiString(PStart-2, AnsiS);
    end);
    RegisterScriptFunc('LOADSTRINGFROMLOCKEDFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      AnsiS := Stack.GetAnsiString(PStart-2);
      Stack.SetBool(PStart, LoadStringFromFile(Stack.GetString(PStart-1), AnsiS, fsReadWrite));
      Stack.SetAnsiString(PStart-2, AnsiS);
    end);
    RegisterScriptFunc('LOADSTRINGSFROMFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, LoadStringsFromFile(Stack.GetString(PStart-1), Stack, PStart-2, fsRead));
    end);
    RegisterScriptFunc('LOADSTRINGSFROMLOCKEDFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, LoadStringsFromFile(Stack.GetString(PStart-1), Stack, PStart-2, fsReadWrite));
    end);
    RegisterScriptFunc('SAVESTRINGTOFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SaveStringToFile(Stack.GetString(PStart-1), Stack.GetAnsiString(PStart-2), Stack.GetBool(PStart-3)));
    end);
    RegisterScriptFunc('SAVESTRINGSTOFILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SaveStringsToFile(Stack.GetString(PStart-1), Stack, PStart-2, Stack.GetBool(PStart-3), False, False));
    end);
    RegisterScriptFunc('SAVESTRINGSTOUTF8FILE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SaveStringsToFile(Stack.GetString(PStart-1), Stack, PStart-2, Stack.GetBool(PStart-3), True, False));
    end);
    RegisterScriptFunc('SAVESTRINGSTOUTF8FILEWITHOUTBOM', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SaveStringsToFile(Stack.GetString(PStart-1), Stack, PStart-2, Stack.GetBool(PStart-3), True, True));
    end);
    RegisterScriptFunc('ENABLEFSREDIRECTION', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, not ScriptFuncDisableFsRedir);
      if Stack.GetBool(PStart-1) then
        ScriptFuncDisableFsRedir := False
      else begin
        if not IsWin64 then
          InternalError('Cannot disable FS redirection on this version of Windows');
        ScriptFuncDisableFsRedir := True;
      end;
    end);
    RegisterScriptFunc('GETUNINSTALLPROGRESSFORM', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetClass(PStart, GetUninstallProgressForm);
    end);
    RegisterScriptFunc('CREATECALLBACK', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, CreateCallback(Caller, Stack.Items[PStart-1]));
    end);
    RegisterScriptFunc('ISDOTNETINSTALLED', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsDotNetInstalled(InstallDefaultRegView, TDotNetVersion(Stack.GetInt(PStart-1)), Stack.GetInt(PStart-2)));
    end);
    RegisterScriptFunc('ISMSIPRODUCTINSTALLED', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsMsiProductInstalled(Stack.GetString(PStart-1), Stack.GetInt64(PStart-2), ErrorCode));
      if ErrorCode <> 0 then
        raise Exception.Create(Win32ErrorString(ErrorCode));
    end);
    RegisterScriptFunc('INITIALIZEBITMAPIMAGEFROMICON', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var AscendingTrySizes := Stack.GetIntArray(PStart-4);
      Stack.SetBool(PStart, TBitmapImage(Stack.GetClass(PStart-1)).InitializeFromIcon(0, PChar(Stack.GetString(PStart-2)), Stack.GetInt(PStart-3), AscendingTrySizes));
    end);
    RegisterScriptFunc('EXTRACT7ZIPARCHIVE', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Extract7ZipArchive(Stack.GetString(PStart), Stack.GetString(PStart-1), Stack.GetBool(PStart-2), TOnExtractionProgress(Stack.GetProc(PStart-3, Caller)));
    end);
    RegisterScriptFunc('DEBUGGING', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, Debugging);
    end);
    RegisterScriptFunc('StringJoin', procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var Values := Stack.GetStringArray(PStart-2);
      Stack.SetString(PStart, String.Join(Stack.GetString(PStart-1), Values));
    end);
    RegisterScriptFunc(['StringSplit', 'StringSplitEx'], procedure(const Caller: TPSExec; const Name: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var Separators := Stack.GetStringArray(PStart-2);
      var Parts: TArray<String>;
      if Name = 'STRINGSPLITEX' then begin
        var Quote := Stack.GetString(PStart-3)[1];
        Parts := Stack.GetString(PStart-1).Split(Separators, Quote, Quote, TStringSplitOptions(Stack.GetInt(PStart-4)))
      end else
        Parts := Stack.GetString(PStart-1).Split(Separators, TStringSplitOptions(Stack.GetInt(PStart-3)));
      Stack.SetArray(PStart, Parts);
    end);
  end;

  procedure RegisterDelphiFunction(ProcPtr: Pointer; const Name: AnsiString);
  begin
    ScriptInterpreter.RegisterDelphiFunction(ProcPtr, Name, cdRegister);
    {$IFDEF DEBUG}
    Inc(Count);
    {$ENDIF}
  end;

begin
  if ScriptFuncs <> nil then
    ScriptFuncs.Free;
  ScriptFuncs := TScriptFuncs.Create;

  { The following should register all tables in ScriptFuncTables }
  {$IFDEF DEBUG}
  Count := 0;
  {$ENDIF}
  RegisterScriptDlgScriptFuncs;
  RegisterNewDiskFormScriptFuncs;
  RegisterBrowseFuncScriptFuncs;
  RegisterCommonFuncVclScriptFuncs;
  RegisterCommonFuncScriptFuncs;
  RegisterInstallScriptFuncs;
  RegisterInstFuncScriptFuncs;
  RegisterInstFuncOleScriptFuncs;
  RegisterMainFuncScriptFuncs;
  RegisterMessagesScriptFuncs;
  RegisterSystemScriptFuncs;
  RegisterSysUtilsScriptFuncs;
  RegisterVerInfoFuncScriptFuncs;
  RegisterWindowsScriptFuncs;
  RegisterOle2ScriptFuncs;
  RegisterLoggingFuncScriptFuncs;
  RegisterOtherScriptFuncs;
  {$IFDEF DEBUG}
  for var ScriptFuncTable in ScriptFuncTables do
    for var ScriptFunc in ScriptFuncTable do
      Dec(Count);
  if Count <> 0 then
    raise Exception.Create('Count <> 0');
  {$ENDIF}

  { The following should register all functions in ScriptDelphiFuncTable }
  {$IFDEF DEBUG}
  Count := 0;
  {$ENDIF}
  RegisterDelphiFunction(@_FindFirst, 'FindFirst');
  RegisterDelphiFunction(@_FindNext, 'FindNext');
  RegisterDelphiFunction(@_FindClose, 'FindClose');
  RegisterDelphiFunction(@_FmtMessage, 'FmtMessage');
  RegisterDelphiFunction(@Format, 'Format');
  RegisterDelphiFunction(@_GetWindowsVersionEx, 'GetWindowsVersionEx');
  {$IFDEF DEBUG}
  if Count <> Length(DelphiScriptFuncTable) then
    raise Exception.Create('Count <> Length(DelphiScriptFuncTable)');
  {$ENDIF}
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
  ScriptFuncs.Free;
  FreeASMInliners;
end.
