unit ScriptFunc_R;

{
  Inno Setup
  Copyright (C) 1997-2012 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Script support functions (run time)
}

interface

{$I VERSION.INC}

uses
  uPSRuntime;

procedure ScriptFuncLibraryRegister_R(ScriptInterpreter: TPSExec);

implementation

uses
  Windows, ScriptFunc,
  Forms, uPSUtils, SysUtils, Classes, Graphics, Controls, TypInfo,
  {$IFNDEF Delphi3orHigher} Ole2, {$ELSE} ActiveX, {$ENDIF}
  Struct, ScriptDlg, Main, PathFunc, CmnFunc, CmnFunc2, FileClass, RedirFunc,
  Install, InstFunc, InstFnc2, Msgs, MsgIDs, BrowseFunc, Wizard, VerInfo,
  SetupTypes, Int64Em, MD5, SHA1, Logging, SetupForm, RegDLL, Helper,
  SpawnClient, UninstProgressForm, Download;

var
  ScaleBaseUnitsInitialized: Boolean;
  ScaleBaseUnitX, ScaleBaseUnitY: Integer;

procedure NoSetupFuncError(const C: AnsiString);{$IFDEF UNICODE} overload;{$ENDIF}
begin
  InternalError(Format('Cannot call "%s" function during Setup', [C]));
end;

procedure NoUninstallFuncError(const C: AnsiString);{$IFDEF UNICODE} overload;{$ENDIF}
begin
  InternalError(Format('Cannot call "%s" function during Uninstall', [C]));
end;

{$IFDEF UNICODE}
procedure NoSetupFuncError(const C: UnicodeString); overload;
begin
  InternalError(Format('Cannot call "%s" function during Setup', [C]));
end;

procedure NoUninstallFuncError(const C: UnicodeString); overload;
begin
  InternalError(Format('Cannot call "%s" function during Uninstall', [C]));
end;
{$ENDIF}

{$IFNDEF UNICODE}
procedure NoNonUnicodeFuncError(const C: String);
begin
  InternalError(Format('Cannot call "%s" function during non Unicode Setup or Uninstall', [C]));
end;
{$ENDIF}

function StackGetAnsiString(Stack: TPSStack; ItemNo: LongInt): AnsiString;
begin
{$IFDEF UNICODE}
  Result := Stack.GetAnsiString(ItemNo);
{$ELSE}
  Result := Stack.GetString(ItemNo);
{$ENDIF}
end;

procedure StackSetAnsiString(Stack: TPSStack; ItemNo: LongInt; const Data: AnsiString);
begin
{$IFDEF UNICODE}
  Stack.SetAnsiString(ItemNo, Data);
{$ELSE}
  Stack.SetString(ItemNo, Data);
{$ENDIF}
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

{---}

{ ScriptDlg }
function ScriptDlgProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;

  function ArrayToStringList(Arr: PPSVariantIFC): TStringList;
  var
    StringList: TStringList;
    I, N: Integer;
  begin
    StringList := TStringList.Create();
    N := PSDynArrayGetLength(Pointer(Arr.Dta^), Arr.aType);
    for I := 0 to N-1 do
      StringList.Append(VNGetString(PSGetArrayField(Arr^, I)));
    Result := StringList;
  end;

  procedure StringListToArray(StringList: TStringList; Arr: PPSVariantIFC);
  var
    I, N: Integer;
  begin
    N := StringList.Count;
    for I := 0 to N-1 do
      VNSetString(PSGetArrayField(Arr^, I), StringList[I]);
  end;

var
  PStart: Cardinal;
  NewPage: TWizardPage;
  NewInputQueryPage: TInputQueryWizardPage;
  NewInputOptionPage: TInputOptionWizardPage;
  NewInputDirPage: TInputDirWizardPage;
  NewInputFilePage: TInputFileWizardPage;
  NewOutputMsgPage: TOutputMsgWizardPage;
  NewOutputMsgMemoPage: TOutputMsgMemoWizardPage;
  NewOutputProgressPage: TOutputProgressWizardPage;
  NewSetupForm: TSetupForm;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'PAGEFROMID' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    Stack.SetClass(PStart, GetWizardForm.PageFromID(Stack.GetInt(PStart-1)));
  end else if Proc.Name = 'PAGEINDEXFROMID' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    Stack.SetInt(PStart, GetWizardForm.PageIndexFromID(Stack.GetInt(PStart-1)));
  end else if Proc.Name = 'CREATECUSTOMPAGE' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    NewPage := TWizardPage.Create(GetWizardForm);
    try
      NewPage.Caption := Stack.GetString(PStart-2);
      NewPage.Description := Stack.GetString(PStart-3);
      GetWizardForm.AddPage(NewPage, Stack.GetInt(PStart-1));
    except
      NewPage.Free;
      raise;
    end;
    Stack.SetClass(PStart, NewPage);
  end else if Proc.Name = 'CREATEINPUTQUERYPAGE' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    NewInputQueryPage := TInputQueryWizardPage.Create(GetWizardForm);
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
  end else if Proc.Name = 'CREATEINPUTOPTIONPAGE' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    NewInputOptionPage := TInputOptionWizardPage.Create(GetWizardForm);
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
  end else if Proc.Name = 'CREATEINPUTDIRPAGE' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    NewInputDirPage := TInputDirWizardPage.Create(GetWizardForm);
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
  end else if Proc.Name = 'CREATEINPUTFILEPAGE' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    NewInputFilePage := TInputFileWizardPage.Create(GetWizardForm);
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
  end else if Proc.Name = 'CREATEOUTPUTMSGPAGE' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    NewOutputMsgPage := TOutputMsgWizardPage.Create(GetWizardForm);
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
  end else if Proc.Name = 'CREATEOUTPUTMSGMEMOPAGE' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    NewOutputMsgMemoPage := TOutputMsgMemoWizardPage.Create(GetWizardForm);
    try
      NewOutputMsgMemoPage.Caption := Stack.GetString(PStart-2);
      NewOutputMsgMemoPage.Description := Stack.GetString(PStart-3);
      GetWizardForm.AddPage(NewOutputMsgMemoPage, Stack.GetInt(PStart-1));
      NewOutputMsgMemoPage.Initialize(Stack.GetString(PStart-4),
         StackGetAnsiString(Stack, PStart-5));
    except
      NewOutputMsgMemoPage.Free;
      raise;
    end;
    Stack.SetClass(PStart, NewOutputMsgMemoPage);
  end else if Proc.Name = 'CREATEOUTPUTPROGRESSPAGE' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    NewOutputProgressPage := TOutputProgressWizardPage.Create(GetWizardForm);
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
  end else if Proc.Name = 'SCALEX' then begin
    InitializeScaleBaseUnits;
    Stack.SetInt(PStart, MulDiv(Stack.GetInt(PStart-1), ScaleBaseUnitX, OrigBaseUnitX));
  end else if Proc.Name = 'SCALEY' then begin
    InitializeScaleBaseUnits;
    Stack.SetInt(PStart, MulDiv(Stack.GetInt(PStart-1), ScaleBaseUnitY, OrigBaseUnitY));
  end else if Proc.Name = 'CREATECUSTOMFORM' then begin
    NewSetupForm := TSetupForm.CreateNew(nil);
    try
      NewSetupForm.AutoScroll := False;
      NewSetupForm.BorderStyle := bsDialog;
      NewSetupForm.InitializeFont;
    except
      NewSetupForm.Free;
      raise;
    end;
    Stack.SetClass(PStart, NewSetupForm);
  end else
    Result := False;
end;

{ NewDisk }
function NewDiskProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  PStart: Cardinal;
  S: String;
  ParentWnd: HWND;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'BROWSEFORFOLDER' then begin
    if Assigned(WizardForm) then
      ParentWnd := WizardForm.Handle
    else
      ParentWnd := 0;
    S := Stack.GetString(PStart-2);
    Stack.SetBool(PStart, BrowseForFolder(Stack.GetString(PStart-1), S, ParentWnd, Stack.GetBool(PStart-3)));
    Stack.SetString(PStart-2, S);
  end else if Proc.Name = 'GETOPENFILENAME' then begin
    if Assigned(WizardForm) then
      ParentWnd := WizardForm.Handle
    else
      ParentWnd := 0;
    S := Stack.GetString(PStart-2);
    Stack.SetBool(PStart, NewGetOpenFileName(Stack.GetString(PStart-1), S, Stack.GetString(PStart-3), Stack.GetString(PStart-4), Stack.GetString(PStart-5), ParentWnd));
    Stack.SetString(PStart-2, S);
  end else if Proc.Name = 'GETSAVEFILENAME' then begin
    if Assigned(WizardForm) then
      ParentWnd := WizardForm.Handle
    else
      ParentWnd := 0;
    S := Stack.GetString(PStart-2);
    Stack.SetBool(PStart, NewGetSaveFileName(Stack.GetString(PStart-1), S, Stack.GetString(PStart-3), Stack.GetString(PStart-4), Stack.GetString(PStart-5), ParentWnd));
    Stack.SetString(PStart-2, S);
  end else
    Result := False;
end;

{ CmnFunc }
function CmnFuncProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  PStart: Cardinal;
  ID: TSetupMessageID;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'MSGBOX' then begin
    if IsUninstaller then
      ID := msgUninstallAppTitle
    else
      ID := msgSetupAppTitle;
    Stack.SetInt(PStart, LoggedMsgBox(Stack.GetString(PStart-1), SetupMessages[ID], TMsgBoxType(Stack.GetInt(PStart-2)), Stack.GetInt(PStart-3), False, 0));
  end else if Proc.Name = 'MINIMIZEPATHNAME' then begin
    Stack.SetString(PStart, MinimizePathName(Stack.GetString(PStart-1), TFont(Stack.GetClass(PStart-2)), Stack.GetInt(PStart-3)));
  end else
    Result := False;
end;

{ CmnFunc2 }
function CmnFunc2Proc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;

  procedure CrackCodeRootKey(const CodeRootKey: Longint; var RegView: TRegView;
    var RootKey: HKEY);
  begin
    { Allow only predefined key handles (8xxxxxxx). Can't accept handles to
      open keys because they might have our special flag bits set.
      Also reject unknown flags which may have a meaning in the future. }
    if (CodeRootKey shr 31 <> 1) or
       ((CodeRootKey and CodeRootKeyFlagMask) and not CodeRootKeyValidFlags <> 0) then
      InternalError('Invalid RootKey value');

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
    const SubKeyName: String; Arr: PPSVariantIFC; const Subkey: Boolean): Boolean;
  const
    samDesired: array [Boolean] of REGSAM = (KEY_QUERY_VALUE, KEY_ENUMERATE_SUB_KEYS);
  var
    K: HKEY;
    I: Cardinal;
    Buf, S: String;
    BufSize, R: DWORD;
  begin
    Result := False;
    SetString(Buf, nil, 512);
    if RegOpenKeyExView(RegView, RootKey, PChar(SubKeyName), 0, samDesired[Subkey], K) <> ERROR_SUCCESS then
      Exit;
    try
      PSDynArraySetLength(Pointer(Arr.Dta^), Arr.aType, 0);
      I := 0;
      while True do begin
        BufSize := Length(Buf);
        if Subkey then
          R := RegEnumKeyEx(K, I, @Buf[1], BufSize, nil, nil, nil, nil)
        else
          R := RegEnumValue(K, I, @Buf[1], BufSize, nil, nil, nil, nil);
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
        PSDynArraySetLength(Pointer(Arr.Dta^), Arr.aType, I+1);
        SetString(S, PChar(@Buf[1]), BufSize);
        VNSetString(PSGetArrayField(Arr^, I), S);
        Inc(I);
      end;
    finally
      RegCloseKey(K);
    end;
    Result := True;
  end;

var
  PStart: Cardinal;
  ExistingFilename: String;
  RegView: TRegView;
  K, RootKey: HKEY;
  S, N, V: String;
  DataS: AnsiString;
  Typ, ExistingTyp, Data, Size: DWORD;
  Arr: TPSVariantIFC;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'FILEEXISTS' then begin
    Stack.SetBool(PStart, NewFileExistsRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
  end else if Proc.Name = 'DIREXISTS' then begin
    Stack.SetBool(PStart, DirExistsRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
  end else if Proc.Name = 'FILEORDIREXISTS' then begin
    Stack.SetBool(PStart, FileOrDirExistsRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
  end else if Proc.Name = 'GETINISTRING' then begin
    Stack.SetString(PStart, GetIniString(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetString(PStart-3), Stack.GetString(PStart-4)));
  end else if Proc.Name = 'GETINIINT' then begin
    Stack.SetInt(PStart, GetIniInt(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetInt(PStart-3), Stack.GetInt(PStart-4), Stack.GetInt(PStart-5), Stack.GetString(PStart-6)));
  end else if Proc.Name = 'GETINIBOOL' then begin
    Stack.SetBool(PStart, GetIniBool(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetBool(PStart-3), Stack.GetString(PStart-4)));
  end else if Proc.Name = 'INIKEYEXISTS' then begin
    Stack.SetBool(PStart, IniKeyExists(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetString(PStart-3)));
  end else if Proc.Name = 'ISINISECTIONEMPTY' then begin
    Stack.SetBool(PStart, IsIniSectionEmpty(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
  end else if Proc.Name = 'SETINISTRING' then begin
    Stack.SetBool(PStart, SetIniString(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetString(PStart-3), Stack.GetString(PStart-4)));
  end else if Proc.Name = 'SETINIINT' then begin
    Stack.SetBool(PStart, SetIniInt(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetInt(PStart-3), Stack.GetString(PStart-4)));
  end else if Proc.Name = 'SETINIBOOL' then begin
    Stack.SetBool(PStart, SetIniBool(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetBool(PStart-3), Stack.GetString(PStart-4)));
  end else if Proc.Name = 'DELETEINIENTRY' then begin
    DeleteIniEntry(Stack.GetString(PStart), Stack.GetString(PStart-1), Stack.GetString(PStart-2));
  end else if Proc.Name = 'DELETEINISECTION' then begin
    DeleteIniSection(Stack.GetString(PStart), Stack.GetString(PStart-1));
  end else if Proc.Name = 'GETENV' then begin
    Stack.SetString(PStart, GetEnv(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'GETCMDTAIL' then begin
    Stack.SetString(PStart, GetCmdTail());
  end else if Proc.Name = 'PARAMCOUNT' then begin
    Stack.SetInt(PStart, NewParamCount());
  end else if Proc.Name = 'PARAMSTR' then begin
    Stack.SetString(PStart, NewParamStr(Stack.GetInt(PStart-1)));
  end else if Proc.Name = 'ADDBACKSLASH' then begin
    Stack.SetString(PStart, AddBackslash(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'REMOVEBACKSLASH' then begin
    Stack.SetString(PStart, RemoveBackslash(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'REMOVEBACKSLASHUNLESSROOT' then begin
    Stack.SetString(PStart, RemoveBackslashUnlessRoot(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'ADDQUOTES' then begin
    Stack.SetString(PStart, AddQuotes(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'REMOVEQUOTES' then begin
    Stack.SetString(PStart, RemoveQuotes(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'GETSHORTNAME' then begin
    Stack.SetString(PStart, GetShortNameRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
  end else if Proc.Name = 'GETWINDIR' then begin
    Stack.SetString(PStart, GetWinDir());
  end else if Proc.Name = 'GETSYSTEMDIR' then begin
    Stack.SetString(PStart, GetSystemDir());
  end else if Proc.Name = 'GETSYSWOW64DIR' then begin
    Stack.SetString(PStart, GetSysWow64Dir());
  end else if Proc.Name = 'GETSYSNATIVEDIR' then begin
    Stack.SetString(PStart, GetSysNativeDir(IsWin64));
  end else if Proc.Name = 'GETTEMPDIR' then begin
    Stack.SetString(PStart, GetTempDir());
  end else if Proc.Name = 'STRINGCHANGE' then begin
    S := Stack.GetString(PStart-1);
    Stack.SetInt(PStart, StringChange(S, Stack.GetString(PStart-2), Stack.GetString(PStart-3)));
    Stack.SetString(PStart-1, S);
  end else if Proc.Name = 'STRINGCHANGEEX' then begin
    S := Stack.GetString(PStart-1);
    Stack.SetInt(PStart, StringChangeEx(S, Stack.GetString(PStart-2), Stack.GetString(PStart-3), Stack.GetBool(PStart-4)));
    Stack.SetString(PStart-1, S);
  end else if Proc.Name = 'USINGWINNT' then begin
    Stack.SetBool(PStart, UsingWinNT());
  end else if Proc.Name = 'FILECOPY' then begin
    ExistingFilename := Stack.GetString(PStart-1);
    if PathCompare(ExistingFilename, SetupLdrOriginalFilename) <> 0 then
      Stack.SetBool(PStart, CopyFileRedir(ScriptFuncDisableFsRedir,
        ExistingFilename, Stack.GetString(PStart-2), Stack.GetBool(PStart-3)))
    else
      Stack.SetBool(PStart, False);
  end else if Proc.Name = 'CONVERTPERCENTSTR' then begin
    S := Stack.GetString(PStart-1);
    Stack.SetBool(PStart, ConvertPercentStr(S));
    Stack.SetString(PStart-1, S);
  end else if Proc.Name = 'REGKEYEXISTS' then begin
    CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
    S := Stack.GetString(PStart-2);
    if RegOpenKeyExView(RegView, RootKey, PChar(S), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      Stack.SetBool(PStart, True);
      RegCloseKey(K);
    end else
      Stack.SetBool(PStart, False);
  end else if Proc.Name = 'REGVALUEEXISTS' then begin
    CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
    S := Stack.GetString(PStart-2);
    if RegOpenKeyExView(RegView, RootKey, PChar(S), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      N := Stack.GetString(PStart-3);
      Stack.SetBool(PStart, RegValueExists(K, PChar(N)));
      RegCloseKey(K);
    end else
      Stack.SetBool(PStart, False);
  end else if Proc.Name = 'REGDELETEKEYINCLUDINGSUBKEYS' then begin
    CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
    S := Stack.GetString(PStart-2);
    Stack.SetBool(PStart, RegDeleteKeyIncludingSubkeys(RegView, RootKey, PChar(S)) = ERROR_SUCCESS);
  end else if Proc.Name = 'REGDELETEKEYIFEMPTY' then begin
    CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
    S := Stack.GetString(PStart-2);
    Stack.SetBool(PStart, RegDeleteKeyIfEmpty(RegView, RootKey, PChar(S)) = ERROR_SUCCESS);
  end else if Proc.Name = 'REGDELETEVALUE' then begin
    CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
    S := Stack.GetString(PStart-2);
    if RegOpenKeyExView(RegView, RootKey, PChar(S), 0, KEY_SET_VALUE, K) = ERROR_SUCCESS then begin
      N := Stack.GetString(PStart-3);
      Stack.SetBool(PStart, RegDeleteValue(K, PChar(N)) = ERROR_SUCCESS);
      RegCloseKey(K);
    end else
      Stack.SetBool(PStart, False);
  end else if Proc.Name = 'REGGETSUBKEYNAMES' then begin
    CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
    Arr := NewTPSVariantIFC(Stack[PStart-3], True);
    Stack.SetBool(PStart, GetSubkeyOrValueNames(RegView, RootKey,
      Stack.GetString(PStart-2), @Arr, True));
  end else if Proc.Name = 'REGGETVALUENAMES' then begin
    CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
    Arr := NewTPSVariantIFC(Stack[PStart-3], True);
    Stack.SetBool(PStart, GetSubkeyOrValueNames(RegView, RootKey,
      Stack.GetString(PStart-2), @Arr, False));
  end else if Proc.Name = 'REGQUERYSTRINGVALUE' then begin
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
  end else if Proc.Name = 'REGQUERYMULTISTRINGVALUE' then begin
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
  end else if Proc.Name = 'REGQUERYDWORDVALUE' then begin
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
  end else if Proc.Name = 'REGQUERYBINARYVALUE' then begin
    CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
    S := Stack.GetString(PStart-2);
    if RegOpenKeyExView(RegView, RootKey, PChar(S), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      N := Stack.GetString(PStart-3);
      if (RegQueryValueEx(K, PChar(N), nil, @Typ, nil, @Size) = ERROR_SUCCESS) and (Typ = REG_BINARY) then begin
        SetLength(DataS, Size);
        if (RegQueryValueEx(K, PChar(N), nil, @Typ, @DataS[1], @Size) = ERROR_SUCCESS) and (Typ = REG_BINARY) then begin
          StackSetAnsiString(Stack, PStart-4, DataS);
          Stack.SetBool(PStart, True);
        end else
          Stack.SetBool(PStart, False);
      end else
        Stack.SetBool(PStart, False);
      RegCloseKey(K);
    end else
      Stack.SetBool(PStart, False);
  end else if Proc.Name = 'REGWRITESTRINGVALUE' then begin
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
  end else if Proc.Name = 'REGWRITEEXPANDSTRINGVALUE' then begin
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
  end else if Proc.Name = 'REGWRITEMULTISTRINGVALUE' then begin
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
  end else if Proc.Name = 'REGWRITEDWORDVALUE' then begin
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
  end else if Proc.Name = 'REGWRITEBINARYVALUE' then begin
    CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
    S := Stack.GetString(PStart-2);
    if RegCreateKeyExView(RegView, RootKey, PChar(S), 0, nil, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, nil, K, nil) = ERROR_SUCCESS then begin
      N := Stack.GetString(PStart-3);
      DataS := StackGetAnsiString(Stack, PStart-4);
      if RegSetValueEx(K, PChar(N), 0, REG_BINARY, @DataS[1], Length(DataS)) = ERROR_SUCCESS then
        Stack.SetBool(PStart, True)
      else
        Stack.SetBool(PStart, False);
      RegCloseKey(K);
    end else
      Stack.SetBool(PStart, False);
  end else if Proc.Name = 'ISADMINLOGGEDON' then begin
    Stack.SetBool(PStart, IsAdminLoggedOn());
  end else if Proc.Name = 'ISPOWERUSERLOGGEDON' then begin
    Stack.SetBool(PStart, IsPowerUserLoggedOn());
  end else if Proc.Name = 'FONTEXISTS' then begin
    Stack.SetBool(PStart, FontExists(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'GETUILANGUAGE' then begin
    Stack.SetInt(PStart, GetUILanguage);
  end else if Proc.Name = 'ADDPERIOD' then begin
    Stack.SetString(PStart, AddPeriod(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'CHARLENGTH' then begin
    Stack.SetInt(PStart, PathCharLength(Stack.GetString(PStart-1), Stack.GetInt(PStart-2)));
  end else if Proc.Name = 'SETNTFSCOMPRESSION' then begin
    Stack.SetBool(PStart, SetNTFSCompressionRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), Stack.GetBool(PStart-2)));
  end else
    Result := False;
end;

{ Install }
function InstallProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  PStart: Cardinal;
begin
  if IsUninstaller then
    NoUninstallFuncError(Proc.Name);

  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'EXTRACTTEMPORARYFILE' then begin
    ExtractTemporaryFile(Stack.GetString(PStart));
  end else
    Result := False;
end;

{ InstFunc }
procedure ProcessMessagesProc; far;
begin
  Application.ProcessMessages;
end;

function InstFuncProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  PStart: Cardinal;
  Filename: String;
  WindowDisabler: TWindowDisabler;
  ResultCode, ErrorCode: Integer;
  FreeBytes, TotalBytes: Integer64;
  RunAsOriginalUser: Boolean;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'CHECKFORMUTEXES' then begin
    Stack.SetBool(PStart, CheckForMutexes(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'DECREMENTSHAREDCOUNT' then begin
    if Stack.GetBool(PStart-1) then begin
      if not IsWin64 then
        InternalError('Cannot access 64-bit registry keys on this version of Windows');
      Stack.SetBool(PStart, DecrementSharedCount(rv64Bit, Stack.GetString(PStart-2)));
    end
    else
      Stack.SetBool(PStart, DecrementSharedCount(rv32Bit, Stack.GetString(PStart-2)));
  end else if Proc.Name = 'DELAYDELETEFILE' then begin
    DelayDeleteFile(ScriptFuncDisableFsRedir, Stack.GetString(PStart), Stack.GetInt(PStart-1), 250, 250);
  end else if Proc.Name = 'DELTREE' then begin
    Stack.SetBool(PStart, DelTree(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), Stack.GetBool(PStart-2), Stack.GetBool(PStart-3), Stack.GetBool(PStart-4), False, nil, nil, nil));
  end else if Proc.Name = 'GENERATEUNIQUENAME' then begin
    Stack.SetString(PStart, GenerateUniqueName(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
  end else if Proc.Name = 'GETCOMPUTERNAMESTRING' then begin
    Stack.SetString(PStart, GetComputerNameString());
  end else if Proc.Name = 'GETMD5OFFILE' then begin
    Stack.SetString(PStart, MD5DigestToString(GetMD5OfFile(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1))));
  end else if Proc.Name = 'GETMD5OFSTRING' then begin
    Stack.SetString(PStart, MD5DigestToString(GetMD5OfAnsiString(StackGetAnsiString(Stack, PStart-1))));
  end else if Proc.Name = 'GETMD5OFUNICODESTRING' then begin
{$IFDEF UNICODE}
    Stack.SetString(PStart, MD5DigestToString(GetMD5OfUnicodeString(Stack.GetString(PStart-1))));
{$ELSE}
    NoNonUnicodeFuncError(Proc.Name);
{$ENDIF}
  end else if Proc.Name = 'GETSHA1OFFILE' then begin
    Stack.SetString(PStart, SHA1DigestToString(GetSHA1OfFile(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1))));
  end else if Proc.Name = 'GETSHA1OFSTRING' then begin
    Stack.SetString(PStart, SHA1DigestToString(GetSHA1OfAnsiString(StackGetAnsiString(Stack, PStart-1))));
  end else if Proc.Name = 'GETSHA1OFUNICODESTRING' then begin
{$IFDEF UNICODE}
    Stack.SetString(PStart, SHA1DigestToString(GetSHA1OfUnicodeString(Stack.GetString(PStart-1))));
{$ELSE}
    NoNonUnicodeFuncError(Proc.Name);
{$ENDIF}
  end else if Proc.Name = 'GETSPACEONDISK' then begin
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
  end else if Proc.Name = 'GETUSERNAMESTRING' then begin
    Stack.SetString(PStart, GetUserNameString());
  end else if Proc.Name = 'INCREMENTSHAREDCOUNT' then begin
    if Stack.GetBool(PStart) then begin
      if not IsWin64 then
        InternalError('Cannot access 64-bit registry keys on this version of Windows');
      IncrementSharedCount(rv64Bit, Stack.GetString(PStart-1), Stack.GetBool(PStart-2));
    end
    else
      IncrementSharedCount(rv32Bit, Stack.GetString(PStart-1), Stack.GetBool(PStart-2));
  end else if (Proc.Name = 'EXEC') or (Proc.Name = 'EXECASORIGINALUSER') then begin
    RunAsOriginalUser := Proc.Name = 'EXECASORIGINALUSER';
    if IsUninstaller and RunAsOriginalUser then
      NoUninstallFuncError(Proc.Name);

    Filename := Stack.GetString(PStart-1);
    if PathCompare(Filename, SetupLdrOriginalFilename) <> 0 then begin
      { Disable windows so the user can't utilize our UI during the InstExec
        call }
      WindowDisabler := TWindowDisabler.Create;
      try
        Stack.SetBool(PStart, InstExecEx(RunAsOriginalUser,
          ScriptFuncDisableFsRedir, Filename, Stack.GetString(PStart-2),
          Stack.GetString(PStart-3), TExecWait(Stack.GetInt(PStart-5)),
          Stack.GetInt(PStart-4), ProcessMessagesProc, ResultCode));
      finally
        WindowDisabler.Free;
      end;
      Stack.SetInt(PStart-6, ResultCode);
    end else begin
      Stack.SetBool(PStart, False);
      Stack.SetInt(PStart-6, ERROR_ACCESS_DENIED);
    end;
  end else if (Proc.Name = 'SHELLEXEC') or (Proc.Name = 'SHELLEXECASORIGINALUSER') then begin
    RunAsOriginalUser := Proc.Name = 'SHELLEXECASORIGINALUSER';
    if IsUninstaller and RunAsOriginalUser then
      NoUninstallFuncError(Proc.Name);

    Filename := Stack.GetString(PStart-2);
    if PathCompare(Filename, SetupLdrOriginalFilename) <> 0 then begin
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
  end else if Proc.Name = 'ISPROTECTEDSYSTEMFILE' then begin
    Stack.SetBool(PStart, IsProtectedSystemFile(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
  end else if Proc.Name = 'MAKEPENDINGFILERENAMEOPERATIONSCHECKSUM' then begin
    Stack.SetString(PStart, MD5DigestToString(MakePendingFileRenameOperationsChecksum));
  end else if Proc.Name = 'MODIFYPIFFILE' then begin
    Stack.SetBool(PStart, ModifyPifFile(Stack.GetString(PStart-1), Stack.GetBool(PStart-2)));
  end else if Proc.Name = 'REGISTERSERVER' then begin
    RegisterServer(False, Stack.GetBool(PStart), Stack.GetString(PStart-1), Stack.GetBool(PStart-2));
  end else if Proc.Name = 'UNREGISTERSERVER' then begin
    try
      RegisterServer(True, Stack.GetBool(PStart-1), Stack.GetString(PStart-2), Stack.GetBool(PStart-3));
      Stack.SetBool(PStart, True);
    except
      Stack.SetBool(PStart, False);
    end;
  end else if Proc.Name = 'UNREGISTERFONT' then begin
    UnregisterFont(Stack.GetString(PStart), Stack.GetString(PStart-1));
  end else if Proc.Name = 'RESTARTREPLACE' then begin
    RestartReplace(ScriptFuncDisableFsRedir, Stack.GetString(PStart), Stack.GetString(PStart-1));
  end else
    Result := False;
end;

{ InstFnc2 }
function InstFnc2Proc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  PStart: Cardinal;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'CREATESHELLLINK' then begin
    Stack.SetString(PStart, CreateShellLink(Stack.GetString(PStart-1),
      Stack.GetString(PStart-2), Stack.GetString(PStart-3),
      Stack.GetString(PStart-4), Stack.GetString(PStart-5),
      Stack.GetString(PStart-6), Stack.GetInt(PStart-7),
      Stack.GetInt(PStart-8), 0, False, '', False, False));
  end else if Proc.Name = 'REGISTERTYPELIBRARY' then begin
    if Stack.GetBool(PStart) then
      HelperRegisterTypeLibrary(False, Stack.GetString(PStart-1))
    else
      RegisterTypeLibrary(Stack.GetString(PStart-1));
  end else if Proc.Name = 'UNREGISTERTYPELIBRARY' then begin
    try
      if Stack.GetBool(PStart-1) then
        HelperRegisterTypeLibrary(True, Stack.GetString(PStart-2))
      else
        UnregisterTypeLibrary(Stack.GetString(PStart-2));
      Stack.SetBool(PStart, True);
    except
      Stack.SetBool(PStart, False);
    end;
  end else
    Result := False;
end;

{ Main }
function MainProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;

  function CustomMessage(const MsgName: String): String;
  begin
    if not GetCustomMessageValue(MsgName, Result) then
      InternalError(Format('Unknown custom message name "%s"', [MsgName]));
  end;

var
  PStart: Cardinal;
  MinVersion, OnlyBelowVersion: TSetupVersionData;
  WizardComponents, WizardTasks: TStringList;
  ID: TSetupMessageID;
  S: String;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'WIZARDFORM' then begin
    Stack.SetClass(PStart, GetWizardForm);
  end else if Proc.Name = 'MAINFORM' then begin
    Stack.SetClass(PStart, GetMainForm);
  end else if Proc.Name = 'ACTIVELANGUAGE' then begin
    Stack.SetString(PStart, ExpandConst('{language}'));
  end else if Proc.Name = 'ISCOMPONENTSELECTED' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    WizardComponents := TStringList.Create();
    try
      GetWizardForm.GetSelectedComponents(WizardComponents, False, False);
      S := Stack.GetString(PStart-1);
      StringChange(S, '/', '\');
      Stack.SetBool(PStart, ShouldProcessEntry(WizardComponents, nil, S, '', '', ''));
    finally
      WizardComponents.Free();
    end;
  end else if Proc.Name = 'ISTASKSELECTED' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    WizardTasks := TStringList.Create();
    try
      GetWizardForm.GetSelectedTasks(WizardTasks, False, False, False);
      S := Stack.GetString(PStart-1);
      StringChange(S, '/', '\');
      Stack.SetBool(PStart, ShouldProcessEntry(nil, WizardTasks, '', S, '', ''));
    finally
      WizardTasks.Free();
    end;
  end else if Proc.Name = 'EXPANDCONSTANT' then begin
    Stack.SetString(PStart, ExpandConst(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'EXPANDCONSTANTEX' then begin
    Stack.SetString(PStart, ExpandConstEx(Stack.GetString(PStart-1), [Stack.GetString(PStart-2), Stack.GetString(PStart-3)]));
  end else if Proc.Name = 'EXITSETUPMSGBOX' then begin
    Stack.SetBool(PStart, ExitSetupMsgBox());
  end else if Proc.Name = 'GETSHELLFOLDER' then begin
    Stack.SetString(PStart, GetShellFolder(Stack.GetBool(PStart-1), TShellFolderID(Stack.GetInt(PStart-2)), False));
  end else if Proc.Name = 'GETSHELLFOLDERBYCSIDL' then begin
    Stack.SetString(PStart, GetShellFolderByCSIDL(Stack.GetInt(PStart-1), Stack.GetBool(PStart-2)));
  end else if Proc.Name = 'INSTALLONTHISVERSION' then begin
    if not StrToVersionNumbers(Stack.GetString(PStart-1), MinVersion) then
      InternalError('InstallOnThisVersion: Invalid MinVersion string')
    else if not StrToVersionNumbers(Stack.GetString(PStart-2), OnlyBelowVersion) then
      InternalError('InstallOnThisVersion: Invalid OnlyBelowVersion string')
    else
      Stack.SetBool(PStart, (InstallOnThisVersion(MinVersion, OnlyBelowVersion) = irInstall));
  end else if Proc.Name = 'GETWINDOWSVERSION' then begin
    Stack.SetUInt(PStart, WindowsVersion);
  end else if Proc.Name = 'GETWINDOWSVERSIONSTRING' then begin
    Stack.SetString(PStart, Format('%u.%.2u.%u', [WindowsVersion shr 24,
      (WindowsVersion shr 16) and $FF, WindowsVersion and $FFFF]));
  end else if Proc.Name = 'SUPPRESSIBLEMSGBOX' then begin
    if IsUninstaller then
      ID := msgUninstallAppTitle
    else
      ID := msgSetupAppTitle;
    Stack.SetInt(PStart, LoggedMsgBox(Stack.GetString(PStart-1), SetupMessages[ID], TMsgBoxType(Stack.GetInt(PStart-2)), Stack.GetInt(PStart-3), True, Stack.GetInt(PStart-4)));
  end else if Proc.Name = 'ISWIN64' then begin
    Stack.SetBool(PStart, IsWin64);
  end else if Proc.Name = 'IS64BITINSTALLMODE' then begin
    Stack.SetBool(PStart, Is64BitInstallMode);
  end else if Proc.Name = 'PROCESSORARCHITECTURE' then begin
    Stack.SetInt(PStart, Integer(ProcessorArchitecture));
  end else if Proc.Name = 'CUSTOMMESSAGE' then begin
    Stack.SetString(PStart, CustomMessage(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'RMSESSIONSTARTED' then begin
    Stack.SetBool(PStart, RmSessionStarted);
  end else if Proc.Name = 'REGISTEREXTRACLOSEAPPLICATIONSRESOURCE' then begin
    Stack.SetBool(PStart, CodeRegisterExtraCloseApplicationsResource(Stack.GetBool(PStart-1), Stack.GetString(PStart-2)));
  end else
    Result := False;
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
  Version.NTPlatform := IsNT;
  Version.ProductType := WindowsProductType;
  Version.SuiteMask := WindowsSuiteMask;
end;

{ Msgs }
function MsgsProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  PStart: Cardinal;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'SETUPMESSAGE' then begin
    Stack.SetString(PStart, SetupMessages[TSetupMessageID(Stack.GetInt(PStart-1))]);
  end else
    Result := False;
end;

function _FmtMessage(const S: String; const Args: array of String): String;
begin
  Result := FmtMessage(PChar(S), Args);
end;

{ System }
function SystemProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  PStart: Cardinal;
  F: TFile;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'RANDOM' then begin
    Stack.SetInt(PStart, Random(Stack.GetInt(PStart-1)));
  end else if Proc.Name = 'FILESIZE' then begin
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
  end else
    Result := False;
end;

{ SysUtils }

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

function SysUtilsProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;

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

var
  PStart: Cardinal;
  OldName: String;
  NewDateSeparator, NewTimeSeparator: Char;
  OldDateSeparator, OldTimeSeparator: Char;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'BEEP' then begin
    Beep();
  end else if Proc.Name = 'TRIM' then begin
    Stack.SetString(PStart, Trim(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'TRIMLEFT' then begin
    Stack.SetString(PStart, TrimLeft(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'TRIMRIGHT' then begin
    Stack.SetString(PStart, TrimRight(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'GETCURRENTDIR' then begin
    Stack.SetString(PStart, GetCurrentDir());
  end else if Proc.Name = 'SETCURRENTDIR' then begin
    Stack.SetBool(PStart, SetCurrentDir(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'EXPANDFILENAME' then begin
    Stack.SetString(PStart, PathExpand(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'EXPANDUNCFILENAME' then begin
    Stack.SetString(PStart, ExpandUNCFileName(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'EXTRACTRELATIVEPATH' then begin
    Stack.SetString(PStart, NewExtractRelativePath(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
  end else if Proc.Name = 'EXTRACTFILEDIR' then begin
    Stack.SetString(PStart, PathExtractDir(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'EXTRACTFILEDRIVE' then begin
    Stack.SetString(PStart, PathExtractDrive(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'EXTRACTFILEEXT' then begin
    Stack.SetString(PStart, PathExtractExt(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'EXTRACTFILENAME' then begin
    Stack.SetString(PStart, PathExtractName(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'EXTRACTFILEPATH' then begin
    Stack.SetString(PStart, PathExtractPath(Stack.GetString(PStart-1)));
  end else if Proc.Name = 'CHANGEFILEEXT' then begin
    Stack.SetString(PStart, PathChangeExt(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
  end else if Proc.Name = 'FILESEARCH' then begin
    Stack.SetString(PStart, NewFileSearch(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
  end else if Proc.Name = 'RENAMEFILE' then begin
    OldName := Stack.GetString(PStart-1);
    if PathCompare(OldName, SetupLdrOriginalFilename) <> 0 then
      Stack.SetBool(PStart, MoveFileRedir(ScriptFuncDisableFsRedir, OldName, Stack.GetString(PStart-2)))
    else
      Stack.SetBool(PStart, False);
  end else if Proc.Name = 'DELETEFILE' then begin
    Stack.SetBool(PStart, DeleteFileRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
  end else if Proc.Name = 'CREATEDIR' then begin
    Stack.SetBool(PStart, CreateDirectoryRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
  end else if Proc.Name = 'REMOVEDIR' then begin
    Stack.SetBool(PStart, RemoveDirectoryRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
  end else if Proc.Name = 'COMPARESTR' then begin
    Stack.SetInt(PStart, CompareStr(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
  end else if Proc.Name = 'COMPARETEXT' then begin
    Stack.SetInt(PStart, CompareText(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
  end else if Proc.Name = 'GETDATETIMESTRING' then begin
    OldDateSeparator := DateSeparator;
    OldTimeSeparator := TimeSeparator;
    try
      NewDateSeparator := Stack.GetString(PStart-2)[1];
      NewTimeSeparator := Stack.GetString(PStart-3)[1];
      if NewDateSeparator <> #0 then
        DateSeparator := NewDateSeparator;
      if NewTimeSeparator <> #0 then
        TimeSeparator := NewTimeSeparator;
      Stack.SetString(PStart, FormatDateTime(Stack.GetString(PStart-1), Now()));
    finally
      TimeSeparator := OldTimeSeparator;
      DateSeparator := OldDateSeparator;
    end;
  end else if Proc.Name = 'SYSERRORMESSAGE' then begin
    Stack.SetString(PStart, Win32ErrorString(Stack.GetInt(PStart-1)));
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

{$IFNDEF IS_D7}
procedure _FmtStr(var Result: string; const Format: string;
  const Args: array of const);
var
  Len, BufLen: Integer;
  Buffer: array[0..4095] of Char;
begin
  BufLen := SizeOf(Buffer);
  if Length(Format) < (sizeof(Buffer) - (sizeof(Buffer) div 4)) then
    Len := FormatBuf(Buffer, sizeof(Buffer) - 1, Pointer(Format)^, Length(Format), Args)
  else
  begin
    BufLen := Length(Format);
    Len := BufLen;
  end;
  if Len >= BufLen - 1 then
  begin
    while Len >= BufLen - 1 do
    begin
      Inc(BufLen, BufLen);
      Result := '';          // prevent copying of existing data, for speed
      SetLength(Result, BufLen);
      Len := FormatBuf(Pointer(Result)^, BufLen - 1, Pointer(Format)^,
      Length(Format), Args);
    end;
    SetLength(Result, Len);
  end
  else
    SetString(Result, Buffer, Len);
end;

{ We use the Format/FmtStr functions from Delphi 7 because Delphi 2's Format
  raises an exception if the result is more than 4096 characters. }
function _Format(const Format: string; const Args: array of const): string;
begin
  _FmtStr(Result, Format, Args);
end;
{$ENDIF}

{ FileCtrl }
function FileCtrlProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;

  function ForceDirectories(const DisableFsRedir: Boolean; Dir: String): Boolean;
  begin
    Dir := RemoveBackslashUnlessRoot(Dir);
    if (PathExtractPath(Dir) = Dir) or DirExistsRedir(DisableFsRedir, Dir) then
      Result := True
    else
      Result := ForceDirectories(DisableFsRedir, PathExtractPath(Dir)) and
        CreateDirectoryRedir(DisableFsRedir, Dir);
  end;

var
  PStart: Cardinal;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'FORCEDIRECTORIES' then begin
    Stack.SetBool(PStart, ForceDirectories(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
  end else
    Result := False;
end;

{ VerInfo }
function VerInfoProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  PStart: Cardinal;
  VersionNumbers: TFileVersionNumbers;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'GETVERSIONNUMBERS' then begin
    if GetVersionNumbersRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), VersionNumbers) then begin
      Stack.SetInt(PStart-2, VersionNumbers.MS);
      Stack.SetInt(PStart-3, VersionNumbers.LS);
      Stack.SetBool(PStart, True);
    end else
      Stack.SetBool(PStart, False);
  end else if Proc.Name = 'GETVERSIONNUMBERSSTRING' then begin
    if GetVersionNumbersRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), VersionNumbers) then begin
      Stack.SetString(PStart-2, Format('%u.%u.%u.%u', [VersionNumbers.MS shr 16,
        VersionNumbers.MS and $FFFF, VersionNumbers.LS shr 16, VersionNumbers.LS and $FFFF]));
      Stack.SetBool(PStart, True);
    end else
      Stack.SetBool(PStart, False);
  end else
    Result := False;
end;

type
  TDllProc = function(const Param1, Param2: Longint): Longint; stdcall;

{ Windows }
function WindowsProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  PStart: Cardinal;
  DllProc: TDllProc;
  DllHandle: THandle;
  S: AnsiString;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'SLEEP' then begin
    Sleep(Stack.GetInt(PStart));
  end else if Proc.Name = 'FINDWINDOWBYCLASSNAME' then begin
    Stack.SetInt(PStart, FindWindow(PChar(Stack.GetString(PStart-1)), nil));
  end else if Proc.Name = 'FINDWINDOWBYWINDOWNAME' then begin
    Stack.SetInt(PStart, FindWindow(nil, PChar(Stack.GetString(PStart-1))));
  end else if Proc.Name = 'SENDMESSAGE' then begin
    Stack.SetInt(PStart, SendMessage(Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3), Stack.GetInt(PStart-4)));
  end else if Proc.Name = 'POSTMESSAGE' then begin
    Stack.SetBool(PStart, PostMessage(Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3), Stack.GetInt(PStart-4)));
  end else if Proc.Name = 'SENDNOTIFYMESSAGE' then begin
    Stack.SetBool(PStart, SendNotifyMessage(Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3), Stack.GetInt(PStart-4)));
  end else if Proc.Name = 'REGISTERWINDOWMESSAGE' then begin
    Stack.SetInt(PStart, RegisterWindowMessage(PChar(Stack.GetString(PStart-1))));
  end else if Proc.Name = 'SENDBROADCASTMESSAGE' then begin
    Stack.SetInt(PStart, SendMessage(HWND_BROADCAST, Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3)));
  end else if Proc.Name = 'POSTBROADCASTMESSAGE' then begin
    Stack.SetBool(PStart, PostMessage(HWND_BROADCAST, Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3)));
  end else if Proc.Name = 'SENDBROADCASTNOTIFYMESSAGE' then begin
    Stack.SetBool(PStart, SendNotifyMessage(HWND_BROADCAST, Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3)));
  end else if Proc.Name = 'LOADDLL' then begin
    DllHandle := SafeLoadLibrary(Stack.GetString(PStart-1), SEM_NOOPENFILEERRORBOX);
    if DllHandle <> 0 then
      Stack.SetInt(PStart-2, 0)
    else
      Stack.SetInt(PStart-2, GetLastError());
    Stack.SetInt(PStart, DllHandle);
  end else if Proc.Name = 'CALLDLLPROC' then begin
    @DllProc := GetProcAddress(Stack.GetInt(PStart-1), PChar(Stack.GetString(PStart-2)));
    if Assigned(DllProc) then begin
      Stack.SetInt(PStart-5, DllProc(Stack.GetInt(PStart-3), Stack.GetInt(PStart-4)));
      Stack.SetBool(PStart, True);
    end else
      Stack.SetBool(PStart, False);
  end else if Proc.Name = 'FREEDLL' then begin
    Stack.SetBool(PStart, FreeLibrary(Stack.GetInt(PStart-1)));
  end else if Proc.Name = 'CREATEMUTEX' then begin
    CreateMutex(nil, False, PChar(Stack.GetString(PStart)));
  end else if Proc.Name = 'OEMTOCHARBUFF' then begin
    S := StackGetAnsiString(Stack, PStart);
    OemToCharBuffA(PAnsiChar(S), PAnsiChar(S), Length(S));
    StackSetAnsiString(Stack, PStart, S);
  end else if Proc.Name = 'CHARTOOEMBUFF' then begin
    S := StackGetAnsiString(Stack, PStart);
    CharToOemBuffA(PAnsiChar(S), PAnsiChar(S), Length(S));
    StackSetAnsiString(Stack, PStart, S);
  end else
    Result := False;
end;

{ Ole2 }
function Ole2Proc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result := True;

  if Proc.Name = 'COFREEUNUSEDLIBRARIES' then begin
    CoFreeUnusedLibraries;
  end else
    Result := False;
end;

{ Logging }
function LoggingProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  PStart: Cardinal;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'LOG' then begin
    Log(Stack.GetString(PStart));
  end else
    Result := False;
end;

{ Download }
function DownloadProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  PStart: Cardinal;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'DOWNLOADWEBFILE' then begin
    DownloadWebFile(Stack.GetString(PStart), Stack.GetString(PStart - 1),
      Stack.GetString(PStart - 2), Stack.GetString(PStart - 3),
      Stack.GetString(PStart - 4), Stack.GetString(PStart - 5),
      Stack.GetBool(PStart - 6), Stack.GetBool(PStart - 7),
      Stack.GetBool(PStart - 8), False);
  end else
    Result := False;
end;

{ Other }
function OtherProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;

  function GetExceptionMessage: String;
  var
    Code: TPSError;
    E: TObject;
  begin
    Code := Caller.{$IFDEF UNICODE} LastEx {$ELSE} ExceptionCode {$ENDIF};
    if Code = erNoError then
      Result := '(There is no current exception)'
    else begin
      E := Caller.{$IFDEF UNICODE} LastExObject {$ELSE} ExceptionObject {$ENDIF};
      if Assigned(E) and (E is Exception) then
        Result := Exception(E).Message
      else
        Result := String(PSErrorToString(Code, Caller.
          {$IFDEF UNICODE} LastExParam {$ELSE} ExceptionString {$ENDIF}));
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

  function LoadStringFromFile(const FileName: String; var S: AnsiString): Boolean;
  var
    F: TFile;
    N: Cardinal;
  begin
    try
      F := TFileRedir.Create(ScriptFuncDisableFsRedir, FileName, fdOpenExisting, faRead, fsRead);
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

  function LoadStringsFromFile(const FileName: String; Arr: PPSVariantIFC): Boolean;
  var
    F: TTextFileReader;
    I: Integer;
    S: String;
  begin
    try
      F := TTextFileReaderRedir.Create(ScriptFuncDisableFsRedir, FileName, fdOpenExisting, faRead, fsRead);
      try
        PSDynArraySetLength(Pointer(Arr.Dta^), Arr.aType, 0);
        I := 0;
        while not F.Eof do begin
          S := F.ReadLine;
          PSDynArraySetLength(Pointer(Arr.Dta^), Arr.aType, I+1);
          VNSetString(PSGetArrayField(Arr^, I), S);
          Inc(I);
        end;
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

  function SaveStringsToFile(const FileName: String; const Arr: PPSVariantIFC; Append, UTF8: Boolean): Boolean;
  var
    F: TTextFileWriter;
    I, N: Integer;
    S: String;
  begin
{$IFNDEF UNICODE}
    if UTF8 then
      NoNonUnicodeFuncError('SAVESTRINGSTOUTF8FILE');
{$ENDIF}
    try
      if Append then
        F := TTextFileWriterRedir.Create(ScriptFuncDisableFsRedir, FileName, fdOpenAlways, faWrite, fsNone)
      else
        F := TTextFileWriterRedir.Create(ScriptFuncDisableFsRedir, FileName, fdCreateAlways, faWrite, fsNone);
      try
        N := PSDynArrayGetLength(Pointer(Arr.Dta^), Arr.aType);
        for I := 0 to N-1 do begin
          S := VNGetString(PSGetArrayField(Arr^, I));
{$IFDEF UNICODE}
          if not UTF8 then
            F.WriteAnsiLine(AnsiString(S))
          else
            F.WriteLine(S);
{$ELSE}
          F.WriteLine(S);
{$ENDIF}
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
  PStart: Cardinal;
  TypeEntry: PSetupTypeEntry;
  StringList: TStringList;
  S: AnsiString;
  Arr: TPSVariantIFC;
begin
  PStart := Stack.Count-1;
  Result := True;

  if Proc.Name = 'BRINGTOFRONTANDRESTORE' then begin
    Application.BringToFront();
    Application.Restore();
  end else if Proc.Name = 'WIZARDDIRVALUE' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    Stack.SetString(PStart, RemoveBackslashUnlessRoot(GetWizardForm.DirEdit.Text));
  end else if Proc.Name = 'WIZARDGROUPVALUE' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    Stack.SetString(PStart, RemoveBackslashUnlessRoot(GetWizardForm.GroupEdit.Text));
  end else if Proc.Name = 'WIZARDNOICONS' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    Stack.SetBool(PStart, GetWizardForm.NoIconsCheck.Checked);
  end else if Proc.Name = 'WIZARDSETUPTYPE' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    TypeEntry := GetWizardForm.GetSetupType();
    if TypeEntry <> nil then begin
      if Stack.GetBool(PStart-1) then
        Stack.SetString(PStart, TypeEntry.Description)
      else
        Stack.SetString(PStart, TypeEntry.Name);
    end
    else
      Stack.SetString(PStart, '');
  end else if Proc.Name = 'WIZARDSELECTEDCOMPONENTS' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    StringList := TStringList.Create();
    try
      GetWizardForm.GetSelectedComponents(StringList, Stack.GetBool(PStart-1), False);
      Stack.SetString(PStart, StringsToCommaString(StringList));
    finally
      StringList.Free();
    end;
  end else if Proc.Name = 'WIZARDSELECTEDTASKS' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    StringList := TStringList.Create();
    try
      GetWizardForm.GetSelectedTasks(StringList, Stack.GetBool(PStart-1), False, False);
      Stack.SetString(PStart, StringsToCommaString(StringList));
    finally
      StringList.Free();
    end;
  end else if Proc.Name = 'WIZARDSILENT' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    Stack.SetBool(PStart, InstallMode <> imNormal);
  end else if Proc.Name = 'ISUNINSTALLER' then begin
    Stack.SetBool(PStart, IsUninstaller);
  end else if Proc.Name = 'UNINSTALLSILENT' then begin
    if not IsUninstaller then
      NoSetupFuncError(Proc.Name);
    Stack.SetBool(PStart, UninstallSilent);
  end else if Proc.Name = 'CURRENTFILENAME' then begin
    if IsUninstaller then
      NoUninstallFuncError(Proc.Name);
    if CheckOrInstallCurrentFileName <> '' then
      Stack.SetString(PStart, CheckOrInstallCurrentFileName)
    else
      InternalError('An attempt was made to call the "CurrentFileName" function from outside a "Check", "BeforeInstall" or "AfterInstall" event function belonging to a "[Files]" entry');
  end else if Proc.Name = 'CASTSTRINGTOINTEGER' then begin
    Stack.SetInt(PStart, Integer(PChar(Stack.GetString(PStart-1))));
  end else if Proc.Name = 'CASTINTEGERTOSTRING' then begin
    Stack.SetString(PStart, String(PChar(Stack.GetInt(PStart-1))));
  end else if Proc.Name = 'ABORT' then begin
    Abort;
  end else if Proc.Name = 'GETEXCEPTIONMESSAGE' then begin
    Stack.SetString(PStart, GetExceptionMessage);
  end else if Proc.Name = 'RAISEEXCEPTION' then begin
    raise Exception.Create(Stack.GetString(PStart));
  end else if Proc.Name = 'SHOWEXCEPTIONMESSAGE' then begin
    TMainForm.ShowExceptionMsg(AddPeriod(GetExceptionMessage));
  end else if Proc.Name = 'TERMINATED' then begin
    Stack.SetBool(PStart, Application.Terminated);
  end else if Proc.Name = 'GETPREVIOUSDATA' then begin
    if IsUninstaller then
      Stack.SetString(PStart, GetCodePreviousData(UninstallExpandedAppId, Stack.GetString(PStart-1), Stack.GetString(PStart-2)))
    else
      Stack.SetString(PStart, GetCodePreviousData(ExpandConst(SetupHeader.AppId), Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
  end else if Proc.Name = 'SETPREVIOUSDATA' then begin
    Stack.SetBool(PStart, SetCodePreviousData(Stack.GetInt(PStart-1), Stack.GetString(PStart-2), Stack.GetString(PStart-3)));
  end else if Proc.Name = 'LOADSTRINGFROMFILE' then begin
    S := StackGetAnsiString(Stack, PStart-2);
    Stack.SetBool(PStart, LoadStringFromFile(Stack.GetString(PStart-1), S));
    StackSetAnsiString(Stack, PStart-2, S);
  end else if Proc.Name = 'LOADSTRINGSFROMFILE' then begin
    Arr := NewTPSVariantIFC(Stack[PStart-2], True);
    Stack.SetBool(PStart, LoadStringsFromFile(Stack.GetString(PStart-1), @Arr));
  end else if Proc.Name = 'SAVESTRINGTOFILE' then begin
    Stack.SetBool(PStart, SaveStringToFile(Stack.GetString(PStart-1), StackGetAnsiString(Stack, PStart-2), Stack.GetBool(PStart-3)));
  end else if Proc.Name = 'SAVESTRINGSTOFILE' then begin
    Arr := NewTPSVariantIFC(Stack[PStart-2], True);
    Stack.SetBool(PStart, SaveStringsToFile(Stack.GetString(PStart-1), @Arr, Stack.GetBool(PStart-3), False));
  end else if Proc.Name = 'SAVESTRINGSTOUTF8FILE' then begin
    Arr := NewTPSVariantIFC(Stack[PStart-2], True);
    Stack.SetBool(PStart, SaveStringsToFile(Stack.GetString(PStart-1), @Arr, Stack.GetBool(PStart-3), True));
  end else if Proc.Name = 'ENABLEFSREDIRECTION' then begin
    Stack.SetBool(PStart, not ScriptFuncDisableFsRedir);
    if Stack.GetBool(PStart-1) then
      ScriptFuncDisableFsRedir := False
    else begin
      if not IsWin64 then
        InternalError('Cannot disable FS redirection on this version of Windows');
      ScriptFuncDisableFsRedir := True;
    end;
  end else if Proc.Name = 'UNINSTALLPROGRESSFORM' then begin
    Stack.SetClass(PStart, GetUninstallProgressForm);
  end else
    Result := False;
end;

{---}

procedure ScriptFuncLibraryRegister_R(ScriptInterpreter: TPSExec);

  function ExtractName(const S: String): String;
  var
    P: Integer;
  begin
    Result := S;

    if CompareText(Copy(Result, 1, Length('function')), 'function') = 0 then
      Delete(Result, 1, Length('function'))
    else if CompareText(Copy(Result, 1, Length('procedure')), 'procedure') = 0 then
      Delete(Result, 1, Length('procedure'));

    P := Pos('(', Result);
    if P = 0 then
      P := Pos(':', Result);
    if P = 0 then
      P := Pos(';', Result);
    Delete(Result, P, Maxint);

    Result := Trim(Result);
  end;

  procedure RegisterFunctionTable(const FunctionTable: array of AnsiString;
    const ProcPtr: TPSProcPtr);
  var
    I: Integer;
  begin
    for I := Low(FunctionTable) to High(FunctionTable) do
      ScriptInterpreter.RegisterFunctionName(AnsiString(ExtractName(String(FunctionTable[I]))),
        ProcPtr, nil, nil);
  end;

begin
  RegisterFunctionTable(ScriptDlgTable, @ScriptDlgProc);
  RegisterFunctionTable(NewDiskTable, @NewDiskProc);
  RegisterFunctionTable(CmnFuncTable, @CmnFuncProc);
  RegisterFunctionTable(CmnFunc2Table, @CmnFunc2Proc);
  RegisterFunctionTable(InstallTable, @InstallProc);
  RegisterFunctionTable(InstFuncTable, @InstFuncProc);
  RegisterFunctionTable(InstFnc2Table, @InstFnc2Proc);
  RegisterFunctionTable(MainTable, @MainProc);
  RegisterFunctionTable(MsgsTable, @MsgsProc);
  RegisterFunctionTable(SystemTable, @SystemProc);
  RegisterFunctionTable(SysUtilsTable, @SysUtilsProc);
  RegisterFunctionTable(FileCtrlTable, @FileCtrlProc);
  RegisterFunctionTable(VerInfoTable, @VerInfoProc);
  RegisterFunctionTable(WindowsTable, @WindowsProc);
  RegisterFunctionTable(Ole2Table, @Ole2Proc);
  RegisterFunctionTable(LoggingTable, @LoggingProc);
  RegisterFunctionTable(DownloadTable, @DownloadProc);
  RegisterFunctionTable(OtherTable, @OtherProc);

  ScriptInterpreter.RegisterDelphiFunction(@_FindFirst, 'FindFirst', cdRegister);
  ScriptInterpreter.RegisterDelphiFunction(@_FindNext, 'FindNext', cdRegister);
  ScriptInterpreter.RegisterDelphiFunction(@_FindClose, 'FindClose', cdRegister);
  ScriptInterpreter.RegisterDelphiFunction(@_FmtMessage, 'FmtMessage', cdRegister);
  ScriptInterpreter.RegisterDelphiFunction({$IFNDEF IS_D7} @_Format {$ELSE} @Format {$ENDIF}, 'Format', cdRegister);
  ScriptInterpreter.RegisterDelphiFunction(@_GetWindowsVersionEx, 'GetWindowsVersionEx', cdRegister); 
end;

end.
