unit Setup.ScriptFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
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
  Windows,
  Forms, SysUtils, Classes, Graphics, ActiveX, Generics.Collections,
  uPSUtils, PathFunc, BrowseFunc, MD5, SHA1, SHA256, BitmapImage, PSStackHelper,
  Shared.Struct, Setup.ScriptDlg, Setup.MainFunc, Shared.CommonFunc.Vcl,
  Shared.CommonFunc, Shared.FileClass, SetupLdrAndSetup.RedirFunc,
  Setup.Install, SetupLdrAndSetup.InstFunc, Setup.InstFunc, Setup.InstFunc.Ole,
  SetupLdrAndSetup.Messages, Shared.SetupMessageIDs, Setup.NewDiskForm,
  Setup.WizardForm, Shared.VerInfoFunc, Shared.SetupTypes,
  Shared.Int64Em, Setup.LoggingFunc, Setup.SetupForm, Setup.RegDLL, Setup.Helper,
  Setup.SpawnClient, Setup.DotNetFunc, Setup.MainForm,
  Shared.DotNetVersion, Setup.MsiFunc, Compression.SevenZipDecoder,
  Setup.DebugClient, Shared.ScriptFunc, Setup.ScriptFunc.HelperFunc;

type
  TScriptFunc = reference to procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal);

  TScriptFuncTyp = (sfNormal, sfNoUninstall, sfOnlyUninstall);

  TScriptFuncEx = record
    OrgName: AnsiString;
    ScriptFunc: TScriptFunc;
    Typ: TScriptFuncTyp;
    constructor Create(const AOrgName: AnsiString; const AScriptFunc: TScriptFunc; const ATyp: TScriptFuncTyp);
    procedure Run(const Caller: TPSExec; const Stack: TPSStack);
  end;

  TScriptFuncs = TDictionary<AnsiString, TScriptFuncEx>;

var
  ScriptFuncs: TScriptFuncs;

constructor TScriptFuncEx.Create(const AOrgName: AnsiString; const AScriptFunc: TScriptFunc; const ATyp: TScriptFuncTyp);
begin
  OrgName := AOrgName;
  ScriptFunc := AScriptFunc;
  Typ := ATyp;
end;

procedure TScriptFuncEx.Run(const Caller: TPSExec; const Stack: TPSStack);
begin
  if (Typ = sfNoUninstall) and IsUninstaller then
    NoUninstallFuncError(OrgName)
  else if (Typ = sfOnlyUninstall) and not IsUninstaller then
    OnlyUninstallFuncError(OrgName)
  else
    ScriptFunc(Caller, OrgName, Stack, Stack.Count-1);
end;

{ Called by ROPS }
function ScriptFuncPSProc(Caller: TPSExec; Proc: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  var ScriptFuncEx: TScriptFuncEx;
  Result := ScriptFuncs.TryGetValue(Proc.Name, ScriptFuncEx);
  if Result then
    ScriptFuncEx.Run(Caller, Stack);
end;

procedure ScriptFuncLibraryRegister_R(ScriptInterpreter: TPSExec);
{$IFDEF DEBUG}
var
  Count: Integer;
{$ENDIF}

  procedure RegisterScriptFunc(const Name: AnsiString; const ScriptFuncTyp: TScriptFuncTyp; const ScriptFunc: TScriptFunc); overload;
  begin
    var ScriptFuncEx: TScriptFuncEx;
    ScriptFuncs.Add(FastUpperCase(Name), TScriptFuncEx.Create(Name, ScriptFunc, ScriptFuncTyp));
    ScriptInterpreter.RegisterFunctionName(Name, ScriptFuncPSProc, nil, nil);
    {$IFDEF DEBUG}
    Inc(Count);
    {$ENDIF}
  end;

  procedure RegisterScriptFunc(const Names: array of AnsiString; const ScriptFuncTyp: TScriptFuncTyp; const ScriptFunc: TScriptFunc); overload;
  begin
    for var Name in Names do
      RegisterScriptFunc(Name, ScriptFuncTyp, ScriptFunc);
  end;

  procedure RegisterScriptFunc(const Name: AnsiString; const ScriptFunc: TScriptFunc); overload;
  begin
    RegisterScriptFunc(Name, sfNormal, ScriptFunc);
  end;

  procedure RegisterScriptFunc(const Names: array of AnsiString; const ScriptFunc: TScriptFunc); overload;
  begin
    for var Name in Names do
      RegisterScriptFunc(Name, ScriptFunc);
  end;

  procedure RegisterScriptDlgScriptFuncs;
  begin
    RegisterScriptFunc('PageFromID', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetClass(PStart, GetWizardForm.PageFromID(Stack.GetInt(PStart-1)));
    end);
    RegisterScriptFunc('PageIndexFromID', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, GetWizardForm.PageIndexFromID(Stack.GetInt(PStart-1)));
    end);
    RegisterScriptFunc('CreateCustomPage', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
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
    RegisterScriptFunc('CreateInputQueryPage', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
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
    RegisterScriptFunc('CreateInputOptionPage', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
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
    RegisterScriptFunc('CreateInputDirPage', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
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
    RegisterScriptFunc('CreateInputFilePage', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
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
    RegisterScriptFunc('CreateOutputMsgPage', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
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
    RegisterScriptFunc('CreateOutputMsgMemoPage', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
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
    RegisterScriptFunc('CreateOutputProgressPage', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
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
    RegisterScriptFunc('CreateOutputMarqueeProgressPage', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
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
    RegisterScriptFunc('CreateDownloadPage', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin;
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
    RegisterScriptFunc('CreateExtractionPage', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
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
    RegisterScriptFunc('SCALEX', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      InitializeScaleBaseUnits;
      Stack.SetInt(PStart, MulDiv(Stack.GetInt(PStart-1), ScaleBaseUnitX, OrigBaseUnitX));
    end);
    RegisterScriptFunc('SCALEY', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      InitializeScaleBaseUnits;
      Stack.SetInt(PStart, MulDiv(Stack.GetInt(PStart-1), ScaleBaseUnitY, OrigBaseUnitY));
    end);
    RegisterScriptFunc('CREATECUSTOMFORM', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var NewSetupForm := TSetupForm.CreateNew(nil);
      try
        NewSetupForm.PopupMode := pmAuto;
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
  begin
    RegisterScriptFunc('SELECTDISK', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var S := Stack.GetString(PStart-3);
      Stack.SetBool(PStart, SelectDisk(Stack.GetInt(PStart-1), Stack.GetString(PStart-2), S));
      Stack.SetString(PStart-3, S);
    end);
  end;

  procedure RegisterBrowseFuncScriptFuncs;
  begin
    RegisterScriptFunc('BROWSEFORFOLDER', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var S := Stack.GetString(PStart-2);
      Stack.SetBool(PStart, BrowseForFolder(Stack.GetString(PStart-1), S, GetOwnerWndForMessageBox, Stack.GetBool(PStart-3)));
      Stack.SetString(PStart-2, S);
    end);
    RegisterScriptFunc('GETOPENFILENAME', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var S := Stack.GetString(PStart-2);
      Stack.SetBool(PStart, NewGetOpenFileName(Stack.GetString(PStart-1), S, Stack.GetString(PStart-3), Stack.GetString(PStart-4), Stack.GetString(PStart-5), GetOwnerWndForMessageBox));
      Stack.SetString(PStart-2, S);
    end);
    RegisterScriptFunc('GETOPENFILENAMEMULTI', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, NewGetOpenFileNameMulti(Stack.GetString(PStart-1), TStrings(Stack.GetClass(PStart-2)), Stack.GetString(PStart-3), Stack.GetString(PStart-4), Stack.GetString(PStart-5), GetOwnerWndForMessageBox));
    end);
    RegisterScriptFunc('GETSAVEFILENAME', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var S := Stack.GetString(PStart-2);
      Stack.SetBool(PStart, NewGetSaveFileName(Stack.GetString(PStart-1), S, Stack.GetString(PStart-3), Stack.GetString(PStart-4), Stack.GetString(PStart-5), GetOwnerWndForMessageBox));
      Stack.SetString(PStart-2, S);
    end);
  end;

  procedure RegisterCommonFuncVclScriptFuncs;
  begin
    RegisterScriptFunc('MINIMIZEPATHNAME', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, MinimizePathName(Stack.GetString(PStart-1), TFont(Stack.GetClass(PStart-2)), Stack.GetInt(PStart-3)));
    end);
  end;

  procedure RegisterCommonFuncScriptFuncs;
  begin
    RegisterScriptFunc('FILEEXISTS', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, NewFileExistsRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('DIREXISTS', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, DirExistsRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('FILEORDIREXISTS', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, FileOrDirExistsRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('GETINISTRING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetIniString(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetString(PStart-3), Stack.GetString(PStart-4)));
    end);
    RegisterScriptFunc('GETINIINT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, GetIniInt(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetInt(PStart-3), Stack.GetInt(PStart-4), Stack.GetInt(PStart-5), Stack.GetString(PStart-6)));
    end);
    RegisterScriptFunc('GETINIBOOL', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, GetIniBool(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetBool(PStart-3), Stack.GetString(PStart-4)));
    end);
    RegisterScriptFunc('INIKEYEXISTS', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IniKeyExists(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetString(PStart-3)));
    end);
    RegisterScriptFunc('ISINISECTIONEMPTY', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsIniSectionEmpty(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('SETINISTRING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SetIniString(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetString(PStart-3), Stack.GetString(PStart-4)));
    end);
    RegisterScriptFunc('SETINIINT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SetIniInt(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetInt(PStart-3), Stack.GetString(PStart-4)));
    end);
    RegisterScriptFunc('SETINIBOOL', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SetIniBool(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetBool(PStart-3), Stack.GetString(PStart-4)));
    end);
    RegisterScriptFunc('DELETEINIENTRY', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      DeleteIniEntry(Stack.GetString(PStart), Stack.GetString(PStart-1), Stack.GetString(PStart-2));
    end);
    RegisterScriptFunc('DELETEINISECTION', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      DeleteIniSection(Stack.GetString(PStart), Stack.GetString(PStart-1));
    end);
    RegisterScriptFunc('GETENV', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetEnv(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('GETCMDTAIL', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetCmdTail);
    end);
    RegisterScriptFunc('PARAMCOUNT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if NewParamsForCode.Count = 0 then
        InternalError('NewParamsForCode not set');
      Stack.SetInt(PStart, NewParamsForCode.Count-1);
    end);
    RegisterScriptFunc('PARAMSTR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var I := Stack.GetInt(PStart-1);
      if (I >= 0) and (I < NewParamsForCode.Count) then
        Stack.SetString(PStart, NewParamsForCode[I])
      else
        Stack.SetString(PStart, '');
    end);
    RegisterScriptFunc('ADDBACKSLASH', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, AddBackslash(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('REMOVEBACKSLASH', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, RemoveBackslash(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('REMOVEBACKSLASHUNLESSROOT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, RemoveBackslashUnlessRoot(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('ADDQUOTES', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, AddQuotes(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('REMOVEQUOTES', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, RemoveQuotes(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('GETSHORTNAME', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetShortNameRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('GETWINDIR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetWinDir);
    end);
    RegisterScriptFunc('GETSYSTEMDIR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetSystemDir);
    end);
    RegisterScriptFunc('GETSYSWOW64DIR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetSysWow64Dir);
    end);
    RegisterScriptFunc('GETSYSNATIVEDIR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetSysNativeDir(IsWin64));
    end);
    RegisterScriptFunc('GETTEMPDIR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetTempDir);
    end);
    RegisterScriptFunc('STRINGCHANGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var S := Stack.GetString(PStart-1);
      Stack.SetInt(PStart, StringChange(S, Stack.GetString(PStart-2), Stack.GetString(PStart-3)));
      Stack.SetString(PStart-1, S);
    end);
    RegisterScriptFunc('STRINGCHANGEEX', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var S := Stack.GetString(PStart-1);
      Stack.SetInt(PStart, StringChangeEx(S, Stack.GetString(PStart-2), Stack.GetString(PStart-3), Stack.GetBool(PStart-4)));
      Stack.SetString(PStart-1, S);
    end);
    RegisterScriptFunc('USINGWINNT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, True);
    end);
    RegisterScriptFunc(['COPYFILE', 'FILECOPY'], procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var ExistingFilename := Stack.GetString(PStart-1);
      if not IsProtectedSrcExe(ExistingFilename) then
        Stack.SetBool(PStart, CopyFileRedir(ScriptFuncDisableFsRedir,
          ExistingFilename, Stack.GetString(PStart-2), Stack.GetBool(PStart-3)))
      else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('CONVERTPERCENTSTR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var S := Stack.GetString(PStart-1);
      Stack.SetBool(PStart, ConvertPercentStr(S));
      Stack.SetString(PStart-1, S);
    end);
    RegisterScriptFunc('REGKEYEXISTS', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      var SubKeyName := Stack.GetString(PStart-2);
      var K: HKEY;
      if RegOpenKeyExView(RegView, RootKey, PChar(SubKeyName), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        Stack.SetBool(PStart, True);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGVALUEEXISTS', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      var SubKeyName := Stack.GetString(PStart-2);
      var K: HKEY;
      if RegOpenKeyExView(RegView, RootKey, PChar(SubKeyName), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        var ValueName := Stack.GetString(PStart-3);
        Stack.SetBool(PStart, RegValueExists(K, PChar(ValueName)));
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGDELETEKEYINCLUDINGSUBKEYS', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      var SubKey := Stack.GetString(PStart-2);
      Stack.SetBool(PStart, RegDeleteKeyIncludingSubkeys(RegView, RootKey, PChar(SubKey)) = ERROR_SUCCESS);
    end);
    RegisterScriptFunc('REGDELETEKEYIFEMPTY', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      var SubKeyName := Stack.GetString(PStart-2);
      Stack.SetBool(PStart, RegDeleteKeyIfEmpty(RegView, RootKey, PChar(SubKeyName)) = ERROR_SUCCESS);
    end);
    RegisterScriptFunc('REGDELETEVALUE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      var SubKeyName := Stack.GetString(PStart-2);
      var K: HKEY;
      if RegOpenKeyExView(RegView, RootKey, PChar(SubKeyName), 0, KEY_SET_VALUE, K) = ERROR_SUCCESS then begin
        var ValueName := Stack.GetString(PStart-3);
        Stack.SetBool(PStart, RegDeleteValue(K, PChar(ValueName)) = ERROR_SUCCESS);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGGETSUBKEYNAMES', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      Stack.SetBool(PStart, GetSubkeyOrValueNames(RegView, RootKey,
        Stack.GetString(PStart-2), Stack, PStart-3, True));
    end);
    RegisterScriptFunc('REGGETVALUENAMES', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      Stack.SetBool(PStart, GetSubkeyOrValueNames(RegView, RootKey,
        Stack.GetString(PStart-2), Stack, PStart-3, False));
    end);
    RegisterScriptFunc('REGQUERYSTRINGVALUE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      var SubKeyName := Stack.GetString(PStart-2);
      var K: HKEY;
      if RegOpenKeyExView(RegView, RootKey, PChar(SubKeyName), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        var ValueName := Stack.GetString(PStart-3);
        var S := Stack.GetString(PStart-4);
        Stack.SetBool(PStart, RegQueryStringValue(K, PChar(ValueName), S));
        Stack.SetString(PStart-4, S);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGQUERYMULTISTRINGVALUE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      var SubKeyName := Stack.GetString(PStart-2);
      var K: HKEY;
      if RegOpenKeyExView(RegView, RootKey, PChar(SubKeyName), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        var ValueName := Stack.GetString(PStart-3);
        var S := Stack.GetString(PStart-4);
        Stack.SetBool(PStart, RegQueryMultiStringValue(K, PChar(ValueName), S));
        Stack.SetString(PStart-4, S);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGQUERYDWORDVALUE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      var SubKeyName := Stack.GetString(PStart-2);
      var K: HKEY;
      if RegOpenKeyExView(RegView, RootKey, PChar(SubKeyName), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        var ValueName := Stack.GetString(PStart-3);
        var Typ, Data: DWORD;
        var Size: DWORD := SizeOf(Data);
        if (RegQueryValueEx(K, PChar(ValueName), nil, @Typ, @Data, @Size) = ERROR_SUCCESS) and (Typ = REG_DWORD) then begin
          Stack.SetInt(PStart-4, Data);
          Stack.SetBool(PStart, True);
        end else
          Stack.SetBool(PStart, False);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGQUERYBINARYVALUE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      var SubKeyName := Stack.GetString(PStart-2);
      var K: HKEY;
      if RegOpenKeyExView(RegView, RootKey, PChar(SubKeyName), 0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        var ValueName := Stack.GetString(PStart-3);
        var Typ, Size: DWORD;
        if RegQueryValueEx(K, PChar(ValueName), nil, @Typ, nil, @Size) = ERROR_SUCCESS then begin
          var Data: AnsiString;
          SetLength(Data, Size);
          if RegQueryValueEx(K, PChar(ValueName), nil, @Typ, @Data[1], @Size) = ERROR_SUCCESS then begin
            Stack.SetAnsiString(PStart-4, Data);
            Stack.SetBool(PStart, True);
          end else
            Stack.SetBool(PStart, False);
        end else
          Stack.SetBool(PStart, False);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGWRITESTRINGVALUE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      var SubKeyName := Stack.GetString(PStart-2);
      var K: HKEY;
      if RegCreateKeyExView(RegView, RootKey, PChar(SubKeyName), 0, nil, REG_OPTION_NON_VOLATILE, KEY_QUERY_VALUE or KEY_SET_VALUE, nil, K, nil) = ERROR_SUCCESS then begin
        var ValueName := Stack.GetString(PStart-3);
        var Data := Stack.GetString(PStart-4);
        var Typ, ExistingTyp: DWORD;
        if (RegQueryValueEx(K, PChar(ValueName), nil, @ExistingTyp, nil, nil) = ERROR_SUCCESS) and (ExistingTyp = REG_EXPAND_SZ) then
          Typ := REG_EXPAND_SZ
        else
          Typ := REG_SZ;
        if RegSetValueEx(K, PChar(ValueName), 0, Typ, PChar(Data), (Length(Data)+1)*SizeOf(Data[1])) = ERROR_SUCCESS then
          Stack.SetBool(PStart, True)
        else
          Stack.SetBool(PStart, False);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGWRITEEXPANDSTRINGVALUE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      var SubKeyName := Stack.GetString(PStart-2);
      var K: HKEY;
      if RegCreateKeyExView(RegView, RootKey, PChar(SubKeyName), 0, nil, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, nil, K, nil) = ERROR_SUCCESS then begin
        var ValueName := Stack.GetString(PStart-3);
        var Data := Stack.GetString(PStart-4);
        if RegSetValueEx(K, PChar(ValueName), 0, REG_EXPAND_SZ, PChar(Data), (Length(Data)+1)*SizeOf(Data[1])) = ERROR_SUCCESS then
          Stack.SetBool(PStart, True)
        else
          Stack.SetBool(PStart, False);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGWRITEMULTISTRINGVALUE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      var SubKeyName := Stack.GetString(PStart-2);
      var K: HKEY;
      if RegCreateKeyExView(RegView, RootKey, PChar(SubKeyName), 0, nil, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, nil, K, nil) = ERROR_SUCCESS then begin
        var ValueName := Stack.GetString(PStart-3);
        var Data := Stack.GetString(PStart-4);
        { Multi-string data requires two null terminators: one after the last
          string, and one to mark the end.
          Delphi's String type is implicitly null-terminated, so only one null
          needs to be added to the end. }
        if (Data <> '') and (Data[Length(Data)] <> #0) then
          Data := Data + #0;
        if RegSetValueEx(K, PChar(ValueName), 0, REG_MULTI_SZ, PChar(Data), (Length(Data)+1)*SizeOf(Data[1])) = ERROR_SUCCESS then
          Stack.SetBool(PStart, True)
        else
          Stack.SetBool(PStart, False);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGWRITEDWORDVALUE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      var SubKeyName := Stack.GetString(PStart-2);
      var K: HKEY;
      if RegCreateKeyExView(RegView, RootKey, PChar(SubKeyName), 0, nil, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, nil, K, nil) = ERROR_SUCCESS then begin
        var ValueName := Stack.GetString(PStart-3);
        var Data: DWORD := Stack.GetInt(PStart-4);
        if RegSetValueEx(K, PChar(ValueName), 0, REG_DWORD, @Data, SizeOf(Data)) = ERROR_SUCCESS then
          Stack.SetBool(PStart, True)
        else
          Stack.SetBool(PStart, False);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('REGWRITEBINARYVALUE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RegView: TRegView;
      var RootKey: HKEY;
      CrackCodeRootKey(Stack.GetInt(PStart-1), RegView, RootKey);
      var SubKeyName := Stack.GetString(PStart-2);
      var K: HKEY;
      if RegCreateKeyExView(RegView, RootKey, PChar(SubKeyName), 0, nil, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, nil, K, nil) = ERROR_SUCCESS then begin
        var ValueName := Stack.GetString(PStart-3);
        var Data := Stack.GetAnsiString(PStart-4);
        if RegSetValueEx(K, PChar(ValueName), 0, REG_BINARY, @Data[1], Length(Data)) = ERROR_SUCCESS then
          Stack.SetBool(PStart, True)
        else
          Stack.SetBool(PStart, False);
        RegCloseKey(K);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc(['ISADMIN', 'ISADMINLOGGEDON'], procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsAdmin);
    end);
    RegisterScriptFunc('ISPOWERUSERLOGGEDON', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsPowerUserLoggedOn);
    end);
    RegisterScriptFUnc('ISADMININSTALLMODE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsAdminInstallMode);
    end);
    RegisterScriptFunc('FONTEXISTS', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, FontExists(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('GETUILANGUAGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, GetUILanguage);
    end);
    RegisterScriptFunc('ADDPERIOD', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, AddPeriod(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('CHARLENGTH', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, PathCharLength(Stack.GetString(PStart-1), Stack.GetInt(PStart-2)));
    end);
    RegisterScriptFunc('SETNTFSCOMPRESSION', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SetNTFSCompressionRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), Stack.GetBool(PStart-2)));
    end);
    RegisterScriptFunc('ISWILDCARD', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsWildcard(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('WILDCARDMATCH', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var S := Stack.GetString(PStart-1);
      var N := Stack.GetString(PStart-2);
      Stack.SetBool(PStart, WildcardMatch(PChar(S), PChar(N)));
    end);
  end;

  procedure RegisterInstallScriptFuncs;
  begin
    RegisterScriptFunc('ExtractTemporaryFile', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      ExtractTemporaryFile(Stack.GetString(PStart));
    end);
    RegisterScriptFunc('ExtractTemporaryFiles', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, ExtractTemporaryFiles(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('DownloadTemporaryFile', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt64(PStart, DownloadTemporaryFile(Stack.GetString(PStart-1), Stack.GetString(PStart-2), Stack.GetString(PStart-3), TOnDownloadProgress(Stack.GetProc(PStart-4, Caller))));
    end);
    RegisterScriptFunc('SetDownloadCredentials', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      SetDownloadCredentials(Stack.GetString(PStart),Stack.GetString(PStart-1));
    end);
    RegisterScriptFunc('DownloadTemporaryFileSize', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt64(PStart, DownloadTemporaryFileSize(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('DownloadTemporaryFileDate', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, DownloadTemporaryFileDate(Stack.GetString(PStart-1)));
    end);
  end;

  procedure RegisterInstFuncScriptFuncs;
  begin
    RegisterScriptFunc('CHECKFORMUTEXES', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, CheckForMutexes(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('DECREMENTSHAREDCOUNT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if Stack.GetBool(PStart-1) then begin
        if not IsWin64 then
          InternalError('Cannot access 64-bit registry keys on this version of Windows');
        Stack.SetBool(PStart, DecrementSharedCount(rv64Bit, Stack.GetString(PStart-2)));
      end
      else
        Stack.SetBool(PStart, DecrementSharedCount(rv32Bit, Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('DELAYDELETEFILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      DelayDeleteFile(ScriptFuncDisableFsRedir, Stack.GetString(PStart), Stack.GetInt(PStart-1), 250, 250);
    end);
    RegisterScriptFunc('DELTREE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, DelTree(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), Stack.GetBool(PStart-2), Stack.GetBool(PStart-3), Stack.GetBool(PStart-4), False, nil, nil, nil));
    end);
    RegisterScriptFunc('GENERATEUNIQUENAME', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GenerateUniqueName(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('GETCOMPUTERNAMESTRING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetComputerNameString);
    end);
    RegisterScriptFunc('GETMD5OFFILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, MD5DigestToString(GetMD5OfFile(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('GETMD5OFSTRING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, MD5DigestToString(GetMD5OfAnsiString(Stack.GetAnsiString(PStart-1))));
    end);
    RegisterScriptFunc('GETMD5OFUNICODESTRING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, MD5DigestToString(GetMD5OfUnicodeString(Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('GETSHA1OFFILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SHA1DigestToString(GetSHA1OfFile(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('GETSHA1OFSTRING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SHA1DigestToString(GetSHA1OfAnsiString(Stack.GetAnsiString(PStart-1))));
    end);
    RegisterScriptFunc('GETSHA1OFUNICODESTRING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SHA1DigestToString(GetSHA1OfUnicodeString(Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('GETSHA256OFFILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SHA256DigestToString(GetSHA256OfFile(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('GETSHA256OFSTRING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SHA256DigestToString(GetSHA256OfAnsiString(Stack.GetAnsiString(PStart-1))));
    end);
    RegisterScriptFunc('GETSHA256OFUNICODESTRING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SHA256DigestToString(GetSHA256OfUnicodeString(Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('GETSPACEONDISK', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var FreeBytes, TotalBytes: Integer64;
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
    RegisterScriptFunc('GETSPACEONDISK64', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var FreeBytes, TotalBytes: Integer64;
      if GetSpaceOnDisk(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), FreeBytes, TotalBytes) then begin
        Stack.SetInt64(PStart-2, Int64(FreeBytes.Hi) shl 32 + FreeBytes.Lo);
        Stack.SetInt64(PStart-3, Int64(TotalBytes.Hi) shl 32 + TotalBytes.Lo);
        Stack.SetBool(PStart, True);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('GETUSERNAMESTRING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetUserNameString);
    end);
    RegisterScriptFunc('INCREMENTSHAREDCOUNT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if Stack.GetBool(PStart) then begin
        if not IsWin64 then
          InternalError('Cannot access 64-bit registry keys on this version of Windows');
        IncrementSharedCount(rv64Bit, Stack.GetString(PStart-1), Stack.GetBool(PStart-2));
      end
      else
        IncrementSharedCount(rv32Bit, Stack.GetString(PStart-1), Stack.GetBool(PStart-2));
    end);
    RegisterScriptFunc(['Exec', 'ExecAsOriginalUser', 'ExecAndLogOutput', 'ExecAndCaptureOutput'], procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RunAsOriginalUser := OrgName = 'ExecAsOriginalUser';
      if IsUninstaller and RunAsOriginalUser then
        NoUninstallFuncError(OrgName);
      var Method: TMethod; { Must stay alive until OutputReader is freed }
      var OutputReader: TCreateProcessOutputReader := nil;
      try
        if OrgName = 'ExecAndLogOutput' then begin
          Method := Stack.GetProc(PStart-7, Caller);
          if Method.Code <> nil then
            OutputReader := TCreateProcessOutputReader.Create(ExecAndLogOutputLogCustom, NativeInt(@Method))
          else if GetLogActive then
            OutputReader := TCreateProcessOutputReader.Create(ExecAndLogOutputLog, 0);
        end else if OrgName = 'ExecAndCaptureOutput' then
          OutputReader := TCreateProcessOutputReader.Create(ExecAndLogOutputLog, 0, omCapture);
        var ExecWait := TExecWait(Stack.GetInt(PStart-5));
        if (OutputReader <> nil) and (ExecWait <> ewWaitUntilTerminated) then
          InternalError(Format('Must call "%s" function with Wait = ewWaitUntilTerminated', [OrgName]));

        var Filename := Stack.GetString(PStart-1);
        if not IsProtectedSrcExe(Filename) then begin
          { Disable windows so the user can't utilize our UI during the InstExec
            call }
          var WindowDisabler := TWindowDisabler.Create;
          var ResultCode: Integer;
          try
            Stack.SetBool(PStart, InstExecEx(RunAsOriginalUser,
              ScriptFuncDisableFsRedir, Filename, Stack.GetString(PStart-2),
              Stack.GetString(PStart-3), ExecWait,
              Stack.GetInt(PStart-4), ProcessMessagesProc, OutputReader, ResultCode));
          finally
            WindowDisabler.Free;
          end;
          Stack.SetInt(PStart-6, ResultCode);
          if OrgName = 'ExecAndCaptureOutput' then begin
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
    RegisterScriptFunc(['ShellExec', 'ShellExecAsOriginalUser'], procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var RunAsOriginalUser := OrgName = 'ShellExecAsOriginalUser';
      if IsUninstaller and RunAsOriginalUser then
        NoUninstallFuncError(OrgName);
      var Filename := Stack.GetString(PStart-2);
      if not IsProtectedSrcExe(Filename) then begin
        { Disable windows so the user can't utilize our UI during the
          InstShellExec call }
        var WindowDisabler := TWindowDisabler.Create;
        var ErrorCode: Integer;
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
    RegisterScriptFunc('ISPROTECTEDSYSTEMFILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsProtectedSystemFile(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('MAKEPENDINGFILERENAMEOPERATIONSCHECKSUM', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SHA256DigestToString(MakePendingFileRenameOperationsChecksum));
    end);
    RegisterScriptFunc('MODIFYPIFFILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, ModifyPifFile(Stack.GetString(PStart-1), Stack.GetBool(PStart-2)));
    end);
    RegisterScriptFunc('REGISTERSERVER', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      RegisterServer(False, Stack.GetBool(PStart), Stack.GetString(PStart-1), Stack.GetBool(PStart-2));
    end);
    RegisterScriptFunc('UNREGISTERSERVER', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      try
        RegisterServer(True, Stack.GetBool(PStart-1), Stack.GetString(PStart-2), Stack.GetBool(PStart-3));
        Stack.SetBool(PStart, True);
      except
        Stack.SetBool(PStart, False);
      end;
    end);
    RegisterScriptFunc('UNREGISTERFONT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      UnregisterFont(Stack.GetString(PStart), Stack.GetString(PStart-1), Stack.GetBool(PStart-2));
    end);
    RegisterScriptFunc('RESTARTREPLACE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      RestartReplace(ScriptFuncDisableFsRedir, Stack.GetString(PStart), Stack.GetString(PStart-1));
    end);
    RegisterScriptFunc('FORCEDIRECTORIES', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, ForceDirectories(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
  end;

  procedure RegisterInstFuncOleScriptFuncs;
  begin
    RegisterScriptFunc('CREATESHELLLINK', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, CreateShellLink(Stack.GetString(PStart-1),
        Stack.GetString(PStart-2), Stack.GetString(PStart-3),
        Stack.GetString(PStart-4), Stack.GetString(PStart-5),
        Stack.GetString(PStart-6), Stack.GetInt(PStart-7),
        Stack.GetInt(PStart-8), 0, '', nil, False, False));
    end);
    RegisterScriptFunc('REGISTERTYPELIBRARY', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if Stack.GetBool(PStart) then
        HelperRegisterTypeLibrary(False, Stack.GetString(PStart-1))
      else
        RegisterTypeLibrary(Stack.GetString(PStart-1));
    end);
    RegisterScriptFunc('UNREGISTERTYPELIBRARY', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
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
    RegisterScriptFunc('UNPINSHELLLINK', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, UnpinShellLink(Stack.GetString(PStart-1)));
    end);
  end;

  procedure RegisterMainFuncScriptFuncs;
  begin
    RegisterScriptFunc('ACTIVELANGUAGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, ExpandConst('{language}'));
    end);
    RegisterScriptFunc('EXPANDCONSTANT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, ExpandConst(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXPANDCONSTANTEX', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, ExpandConstEx(Stack.GetString(PStart-1), [Stack.GetString(PStart-2), Stack.GetString(PStart-3)]));
    end);
    RegisterScriptFunc('EXITSETUPMSGBOX', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, ExitSetupMsgBox);
    end);
    RegisterScriptFunc('GETSHELLFOLDERBYCSIDL', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetShellFolderByCSIDL(Stack.GetInt(PStart-1), Stack.GetBool(PStart-2)));
    end);
    RegisterScriptFunc('INSTALLONTHISVERSION', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var MinVersion, OnlyBelowVersion: TSetupVersionData;
      if not StrToSetupVersionData(Stack.GetString(PStart-1), MinVersion) then
        InternalError(Format('%s: Invalid MinVersion string', [OrgName]))
      else if not StrToSetupVersionData(Stack.GetString(PStart-2), OnlyBelowVersion) then
        InternalError(Format('%s: Invalid OnlyBelowVersion string', [OrgName]))
      else
        Stack.SetBool(PStart, (InstallOnThisVersion(MinVersion, OnlyBelowVersion) = irInstall));
    end);
    RegisterScriptFunc('GETWINDOWSVERSION', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetUInt(PStart, WindowsVersion);
    end);
    RegisterScriptFunc('GETWINDOWSVERSIONSTRING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, Format('%u.%.2u.%u', [WindowsVersion shr 24,
        (WindowsVersion shr 16) and $FF, WindowsVersion and $FFFF]));
    end);
    RegisterScriptFunc(['MsgBox', 'SuppressibleMsgBox'], procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var Suppressible: Boolean;
      var Default: Integer;
      if OrgName = 'MsgBox' then begin
        Suppressible := False;
        Default := 0;
      end else begin
        Suppressible := True;
        Default := Stack.GetInt(PStart-4);
      end;
      Stack.SetInt(PStart, LoggedMsgBox(Stack.GetString(PStart-1), GetMsgBoxCaption, TMsgBoxType(Stack.GetInt(PStart-2)), Stack.GetInt(PStart-3), Suppressible, Default));
    end);
    RegisterScriptFunc(['TaskDialogMsgBox', 'SuppressibleTaskDialogMsgBox'], procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var Suppressible: Boolean;
      var Default: Integer;
      if OrgName = 'TaskDialogMsgBox' then begin
        Suppressible := False;
        Default := 0;
      end else begin
        Suppressible := True;
        Default := Stack.GetInt(PStart-7);
      end;
      var ButtonLabels := Stack.GetStringArray(PStart-5);
      Stack.SetInt(PStart, LoggedTaskDialogMsgBox('', Stack.GetString(PStart-1), Stack.GetString(PStart-2), GetMsgBoxCaption, TMsgBoxType(Stack.GetInt(PStart-3)), Stack.GetInt(PStart-4), ButtonLabels, Stack.GetInt(PStart-6), Suppressible, Default));
    end);
    RegisterScriptFunc('ISWIN64', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsWin64);
    end);
    RegisterScriptFunc('IS64BITINSTALLMODE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, Is64BitInstallMode);
    end);
    RegisterScriptFunc('PROCESSORARCHITECTURE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, Integer(ProcessorArchitecture));
    end);
    RegisterScriptFunc(['IsArm32Compatible', 'IsArm64', 'IsX64', 'IsX64OS', 'IsX64Compatible', 'IsX86', 'IsX86OS', 'IsX86Compatible'], procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var ArchitectureIdentifier := LowerCase(Copy(String(OrgName), 3, MaxInt));
      Stack.SetBool(PStart, EvalArchitectureIdentifier(ArchitectureIdentifier));
    end);
    RegisterScriptFunc('CUSTOMMESSAGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, CustomMessage(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('RMSESSIONSTARTED', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, RmSessionStarted);
    end);
    RegisterScriptFunc('REGISTEREXTRACLOSEAPPLICATIONSRESOURCE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, CodeRegisterExtraCloseApplicationsResource(Stack.GetBool(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('GETWIZARDFORM', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetClass(PStart, GetWizardForm);
    end);
    RegisterScriptFunc(['WizardIsComponentSelected', 'IsComponentSelected', 'WizardIsTaskSelected', 'IsTaskSelected'], sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var StringList := TStringList.Create;
      try
        var Components := (OrgName = 'WizardIsComponentSelected') or (OrgName = 'IsComponentSelected');
        if Components then
          GetWizardForm.GetSelectedComponents(StringList, False, False)
        else
          GetWizardForm.GetSelectedTasks(StringList, False, False, False);
        var S := Stack.GetString(PStart-1);
        StringChange(S, '/', '\');
        if Components then
          Stack.SetBool(PStart, ShouldProcessEntry(StringList, nil, S, '', '', ''))
        else
          Stack.SetBool(PStart, ShouldProcessEntry(nil, StringList, '', S, '', ''));
      finally
        StringList.Free;
      end;
    end);
  end;

  procedure RegisterMessagesScriptFuncs;
  begin
    RegisterScriptFunc('SETUPMESSAGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, SetupMessages[TSetupMessageID(Stack.GetInt(PStart-1))]);
    end);
  end;

  procedure RegisterSystemScriptFuncs;
  begin
    RegisterScriptFunc('RANDOM', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, Random(Stack.GetInt(PStart-1)));
    end);
    RegisterScriptFunc('FILESIZE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      try
        var F := TFileRedir.Create(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), fdOpenExisting, faRead, fsReadWrite);
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
    RegisterScriptFunc('FILESIZE64', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      try
        var F := TFileRedir.Create(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), fdOpenExisting, faRead, fsReadWrite);
        try
          var TmpFileSize := F.Size; { Make sure we access F.Size only once }
          Stack.SetInt64(PStart-2, Int64(TmpFileSize.Hi) shl 32 + TmpFileSize.Lo);
          Stack.SetBool(PStart, True);
        finally
          F.Free;
        end;
      except
        Stack.SetBool(PStart, False);
      end;
    end);
    RegisterScriptFunc('SET8087CW', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Set8087CW(Stack.GetInt(PStart));
    end);
    RegisterScriptFunc('GET8087CW', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, Get8087CW);
    end);
    RegisterScriptFunc('UTF8ENCODE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetAnsiString(PStart, Utf8Encode(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('UTF8DECODE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, UTF8ToString(Stack.GetAnsiString(PStart-1)));
    end);
  end;

  procedure RegisterSysUtilsScriptFuncs;
  begin
    RegisterScriptFunc('BEEP', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Beep;
    end);
    RegisterScriptFunc('TRIMLEFT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, TrimLeft(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('TRIMRIGHT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, TrimRight(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('GETCURRENTDIR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetCurrentDir);
    end);
    RegisterScriptFunc('SETCURRENTDIR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SetCurrentDir(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXPANDFILENAME', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, PathExpand(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXPANDUNCFILENAME', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, ExpandUNCFileName(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXTRACTRELATIVEPATH', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, NewExtractRelativePath(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('EXTRACTFILEDIR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, PathExtractDir(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXTRACTFILEDRIVE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, PathExtractDrive(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXTRACTFILEEXT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, PathExtractExt(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXTRACTFILENAME', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, PathExtractName(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('EXTRACTFILEPATH', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, PathExtractPath(Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('CHANGEFILEEXT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, PathChangeExt(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('FILESEARCH', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, NewFileSearch(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('RENAMEFILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var OldName := Stack.GetString(PStart-1);
      if not IsProtectedSrcExe(OldName) then
        Stack.SetBool(PStart, MoveFileRedir(ScriptFuncDisableFsRedir, OldName, Stack.GetString(PStart-2)))
      else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('DELETEFILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, DeleteFileRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('CREATEDIR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, CreateDirectoryRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('REMOVEDIR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, RemoveDirectoryRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1)));
    end);
    RegisterScriptFunc('COMPARESTR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, CompareStr(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('COMPARETEXT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, CompareText(Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('SAMESTR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, CompareStr(Stack.GetString(PStart-1), Stack.GetString(PStart-2)) = 0);
    end);
    RegisterScriptFunc('SAMETEXT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, CompareText(Stack.GetString(PStart-1), Stack.GetString(PStart-2)) = 0);
    end);
    RegisterScriptFunc('GETDATETIMESTRING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var OldDateSeparator := FormatSettings.DateSeparator;
      var OldTimeSeparator := FormatSettings.TimeSeparator;
      try
        var NewDateSeparator := Stack.GetChar(PStart-2);
        var NewTimeSeparator := Stack.GetChar(PStart-3);
        if NewDateSeparator <> #0 then
          FormatSettings.DateSeparator := NewDateSeparator;
        if NewTimeSeparator <> #0 then
          FormatSettings.TimeSeparator := NewTimeSeparator;
        Stack.SetString(PStart, FormatDateTime(Stack.GetString(PStart-1), Now));
      finally
        FormatSettings.TimeSeparator := OldTimeSeparator;
        FormatSettings.DateSeparator := OldDateSeparator;
      end;
    end);
    RegisterScriptFunc('SYSERRORMESSAGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, Win32ErrorString(Stack.GetInt(PStart-1)));
    end);
  end;

  procedure RegisterVerInfoFuncScriptFuncs;
  begin
    RegisterScriptFunc('GETVERSIONNUMBERS', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var VersionNumbers: TFileVersionNumbers;
      if GetVersionNumbersRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), VersionNumbers) then begin
        Stack.SetInt(PStart-2, VersionNumbers.MS);
        Stack.SetInt(PStart-3, VersionNumbers.LS);
        Stack.SetBool(PStart, True);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('GETVERSIONCOMPONENTS', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var VersionNumbers: TFileVersionNumbers;
      if GetVersionNumbersRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), VersionNumbers) then begin
        Stack.SetUInt(PStart-2, VersionNumbers.MS shr 16);
        Stack.SetUInt(PStart-3, VersionNumbers.MS and $FFFF);
        Stack.SetUInt(PStart-4, VersionNumbers.LS shr 16);
        Stack.SetUInt(PStart-5, VersionNumbers.LS and $FFFF);
        Stack.SetBool(PStart, True);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('GETVERSIONNUMBERSSTRING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var VersionNumbers: TFileVersionNumbers;
      if GetVersionNumbersRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), VersionNumbers) then begin
        Stack.SetString(PStart-2, Format('%u.%u.%u.%u', [VersionNumbers.MS shr 16,
          VersionNumbers.MS and $FFFF, VersionNumbers.LS shr 16, VersionNumbers.LS and $FFFF]));
        Stack.SetBool(PStart, True);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('GETPACKEDVERSION', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var VersionNumbers: TFileVersionNumbers;
      if GetVersionNumbersRedir(ScriptFuncDisableFsRedir, Stack.GetString(PStart-1), VersionNumbers) then begin
        Stack.SetInt64(PStart-2, (Int64(VersionNumbers.MS) shl 32) or VersionNumbers.LS);
        Stack.SetBool(PStart, True);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('PACKVERSIONNUMBERS', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt64(PStart, Int64((UInt64(Stack.GetUInt(PStart-1)) shl 32) or Stack.GetUInt(PStart-2)));
    end);
    RegisterScriptFunc('PACKVERSIONCOMPONENTS', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var VersionNumbers: TFileVersionNumbers;
      VersionNumbers.MS := (Stack.GetUInt(PStart-1) shl 16) or (Stack.GetUInt(PStart-2) and $FFFF);
      VersionNumbers.LS := (Stack.GetUInt(PStart-3) shl 16) or (Stack.GetUInt(PStart-4) and $FFFF);
      Stack.SetInt64(PStart, Int64((UInt64(VersionNumbers.MS) shl 32) or VersionNumbers.LS));
    end);
    RegisterScriptFunc('COMPAREPACKEDVERSION', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, Compare64(Integer64(Stack.GetInt64(PStart-1)), Integer64(Stack.GetInt64(PStart-2))));
    end);
    RegisterScriptFunc('SAMEPACKEDVERSION', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, Compare64(Integer64(Stack.GetInt64(PStart-1)), Integer64(Stack.GetInt64(PStart-2))) = 0);
    end);
    RegisterScriptFunc('UNPACKVERSIONNUMBERS', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var VersionNumbers: TFileVersionNumbers;
      VersionNumbers.MS := UInt64(Stack.GetInt64(PStart)) shr 32;
      VersionNumbers.LS := UInt64(Stack.GetInt64(PStart)) and $FFFFFFFF;
      Stack.SetUInt(PStart-1, VersionNumbers.MS);
      Stack.SetUInt(PStart-2, VersionNumbers.LS);
    end);
    RegisterScriptFunc('UNPACKVERSIONCOMPONENTS', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var VersionNumbers: TFileVersionNumbers;
      VersionNumbers.MS := UInt64(Stack.GetInt64(PStart)) shr 32;
      VersionNumbers.LS := UInt64(Stack.GetInt64(PStart)) and $FFFFFFFF;
      Stack.SetUInt(PStart-1, VersionNumbers.MS shr 16);
      Stack.SetUInt(PStart-2, VersionNumbers.MS and $FFFF);
      Stack.SetUInt(PStart-3, VersionNumbers.LS shr 16);
      Stack.SetUInt(PStart-4, VersionNumbers.LS and $FFFF);
    end);
    RegisterScriptFunc('VERSIONTOSTR', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var VersionNumbers: TFileVersionNumbers;
      VersionNumbers.MS := UInt64(Stack.GetInt64(PStart-1)) shr 32;
      VersionNumbers.LS := UInt64(Stack.GetInt64(PStart-1)) and $FFFFFFFF;
      Stack.SetString(PStart, Format('%u.%u.%u.%u', [VersionNumbers.MS shr 16,
        VersionNumbers.MS and $FFFF, VersionNumbers.LS shr 16, VersionNumbers.LS and $FFFF]));
    end);
    RegisterScriptFunc('STRTOVERSION', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var VersionNumbers: TFileVersionNumbers;
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
  begin
    RegisterScriptFunc('SLEEP', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Sleep(Stack.GetInt(PStart));
    end);
    RegisterScriptFunc('FINDWINDOWBYCLASSNAME', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, FindWindow(PChar(Stack.GetString(PStart-1)), nil));
    end);
    RegisterScriptFunc('FINDWINDOWBYWINDOWNAME', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, FindWindow(nil, PChar(Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('SENDMESSAGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, SendMessage(Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3), Stack.GetInt(PStart-4)));
    end);
    RegisterScriptFunc('POSTMESSAGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, PostMessage(Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3), Stack.GetInt(PStart-4)));
    end);
    RegisterScriptFunc('SENDNOTIFYMESSAGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SendNotifyMessage(Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3), Stack.GetInt(PStart-4)));
    end);
    RegisterScriptFunc('REGISTERWINDOWMESSAGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, RegisterWindowMessage(PChar(Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('SENDBROADCASTMESSAGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, SendMessage(HWND_BROADCAST, Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3)));
    end);
    RegisterScriptFunc('POSTBROADCASTMESSAGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, PostMessage(HWND_BROADCAST, Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3)));
    end);
    RegisterScriptFunc('SENDBROADCASTNOTIFYMESSAGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SendNotifyMessage(HWND_BROADCAST, Stack.GetInt(PStart-1), Stack.GetInt(PStart-2), Stack.GetInt(PStart-3)));
    end);
    RegisterScriptFunc('LOADDLL', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var DllHandle := SafeLoadLibrary(Stack.GetString(PStart-1), SEM_NOOPENFILEERRORBOX);
      if DllHandle <> 0 then
        Stack.SetInt(PStart-2, 0)
      else
        Stack.SetInt(PStart-2, GetLastError);
      Stack.SetInt(PStart, DllHandle);
    end);
    RegisterScriptFunc('CALLDLLPROC', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var DllProc: TDllProc;
      @DllProc := GetProcAddress(Stack.GetInt(PStart-1), PChar(Stack.GetString(PStart-2)));
      if Assigned(DllProc) then begin
        Stack.SetInt(PStart-5, DllProc(Stack.GetInt(PStart-3), Stack.GetInt(PStart-4)));
        Stack.SetBool(PStart, True);
      end else
        Stack.SetBool(PStart, False);
    end);
    RegisterScriptFunc('FREEDLL', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, FreeLibrary(Stack.GetInt(PStart-1)));
    end);
    RegisterScriptFunc('CREATEMUTEX', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Windows.CreateMutex(nil, False, PChar(Stack.GetString(PStart)));
    end);
    RegisterScriptFunc('OEMTOCHARBUFF', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var S := Stack.GetAnsiString(PStart);
      OemToCharBuffA(PAnsiChar(S), PAnsiChar(S), Length(S));
      Stack.SetAnsiString(PStart, S);
    end);
    RegisterScriptFunc('CHARTOOEMBUFF', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var S := Stack.GetAnsiString(PStart);
      CharToOemBuffA(PAnsiChar(S), PAnsiChar(S), Length(S));
      Stack.SetAnsiString(PStart, S);
    end);
  end;

  procedure RegisterActiveXScriptFuncs;
  begin
    RegisterScriptFunc('COFREEUNUSEDLIBRARIES', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      CoFreeUnusedLibraries;
    end);
  end;

  procedure RegisterLoggingFuncScriptFuncs;
  begin
    RegisterScriptFunc('LOG', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Log(Stack.GetString(PStart));
    end);
  end;

  procedure RegisterOtherScriptFuncs;
  begin
    RegisterScriptFunc('BRINGTOFRONTANDRESTORE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      { Must be in this order to work around VCL bug }
      Application.Restore;
      Application.BringToFront;
    end);
    RegisterScriptFunc('WizardDirValue', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, RemoveBackslashUnlessRoot(GetWizardForm.DirEdit.Text));
    end);
    RegisterScriptFunc('WizardGroupValue', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, RemoveBackslashUnlessRoot(GetWizardForm.GroupEdit.Text));
    end);
    RegisterScriptFunc('WizardNoIcons', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, GetWizardForm.NoIconsCheck.Checked);
    end);
    RegisterScriptFunc('WizardSetupType', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var TypeEntry := GetWizardForm.GetSetupType;
      if TypeEntry <> nil then begin
        if Stack.GetBool(PStart-1) then
          Stack.SetString(PStart, TypeEntry.Description)
        else
          Stack.SetString(PStart, TypeEntry.Name);
      end
      else
        Stack.SetString(PStart, '');
    end);
    RegisterScriptFunc(['WizardSelectedComponents', 'WizardSelectedTasks'], sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var StringList := TStringList.Create;
      try
        if OrgName = 'WizardSelectedComponents' then
          GetWizardForm.GetSelectedComponents(StringList, Stack.GetBool(PStart-1), False)
        else
          GetWizardForm.GetSelectedTasks(StringList, Stack.GetBool(PStart-1), False, False);
        Stack.SetString(PStart, StringsToCommaString(StringList));
      finally
        StringList.Free;
      end;
    end);
    RegisterScriptFunc(['WizardSelectComponents', 'WizardSelectTasks'], sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var StringList := TStringList.Create;
      try
        var S := Stack.GetString(PStart);
        StringChange(S, '/', '\');
        SetStringsFromCommaString(StringList, S);
        if OrgName = 'WizardSelectComponents' then
          GetWizardForm.SelectComponents(StringList)
        else
          GetWizardForm.SelectTasks(StringList);
      finally
        StringList.Free;
      end;
    end);
    RegisterScriptFunc('WizardSilent', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, InstallMode <> imNormal);
    end);
    RegisterScriptFunc('ISUNINSTALLER', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsUninstaller);
    end);
    RegisterScriptFunc('UninstallSilent', sfOnlyUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, UninstallSilent);
    end);
    RegisterScriptFunc('CurrentFilename', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if CheckOrInstallCurrentFilename <> '' then
        Stack.SetString(PStart, CheckOrInstallCurrentFilename)
      else
        InternalError(Format('An attempt was made to call the "%s" function from outside a "Check", "BeforeInstall" or "AfterInstall" event function belonging to a "[Files]" entry', [OrgName]));
    end);
    RegisterScriptFunc('CurrentSourceFilename', sfNoUninstall, procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if CheckOrInstallCurrentSourceFilename <> '' then
        Stack.SetString(PStart, CheckOrInstallCurrentSourceFilename)
      else
        InternalError(Format('An attempt was made to call the "%s" function from outside a "Check", "BeforeInstall" or "AfterInstall" event function belonging to a "[Files]" entry with flag "external"', [OrgName]));
    end);
    RegisterScriptFunc('CASTSTRINGTOINTEGER', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, Integer(PChar(Stack.GetString(PStart-1))));
    end);
    RegisterScriptFunc('CASTINTEGERTOSTRING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, String(PChar(Stack.GetInt(PStart-1))));
    end);
    RegisterScriptFunc('ABORT', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Abort;
    end);
    RegisterScriptFunc('GETEXCEPTIONMESSAGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetString(PStart, GetExceptionMessage(Caller));
    end);
    RegisterScriptFunc('RAISEEXCEPTION', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      raise Exception.Create(Stack.GetString(PStart));
    end);
    RegisterScriptFunc('SHOWEXCEPTIONMESSAGE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      TMainForm.ShowExceptionMsg(AddPeriod(GetExceptionMessage(Caller)));
    end);
    RegisterScriptFunc('TERMINATED', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, Application.Terminated);
    end);
    RegisterScriptFunc('GETPREVIOUSDATA', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      if IsUninstaller then
        Stack.SetString(PStart, GetCodePreviousData(UninstallExpandedAppId, Stack.GetString(PStart-1), Stack.GetString(PStart-2)))
      else
        Stack.SetString(PStart, GetCodePreviousData(ExpandConst(SetupHeader.AppId), Stack.GetString(PStart-1), Stack.GetString(PStart-2)));
    end);
    RegisterScriptFunc('SETPREVIOUSDATA', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SetCodePreviousData(Stack.GetInt(PStart-1), Stack.GetString(PStart-2), Stack.GetString(PStart-3)));
    end);
    RegisterScriptFunc('LOADSTRINGFROMFILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var S := Stack.GetAnsiString(PStart-2);
      Stack.SetBool(PStart, LoadStringFromFile(Stack.GetString(PStart-1), S, fsRead));
      Stack.SetAnsiString(PStart-2, S);
    end);
    RegisterScriptFunc('LOADSTRINGFROMLOCKEDFILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var S := Stack.GetAnsiString(PStart-2);
      Stack.SetBool(PStart, LoadStringFromFile(Stack.GetString(PStart-1), S, fsReadWrite));
      Stack.SetAnsiString(PStart-2, S);
    end);
    RegisterScriptFunc('LOADSTRINGSFROMFILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, LoadStringsFromFile(Stack.GetString(PStart-1), Stack, PStart-2, fsRead));
    end);
    RegisterScriptFunc('LOADSTRINGSFROMLOCKEDFILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, LoadStringsFromFile(Stack.GetString(PStart-1), Stack, PStart-2, fsReadWrite));
    end);
    RegisterScriptFunc('SAVESTRINGTOFILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SaveStringToFile(Stack.GetString(PStart-1), Stack.GetAnsiString(PStart-2), Stack.GetBool(PStart-3)));
    end);
    RegisterScriptFunc('SAVESTRINGSTOFILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SaveStringsToFile(Stack.GetString(PStart-1), Stack, PStart-2, Stack.GetBool(PStart-3), False, False));
    end);
    RegisterScriptFunc('SAVESTRINGSTOUTF8FILE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SaveStringsToFile(Stack.GetString(PStart-1), Stack, PStart-2, Stack.GetBool(PStart-3), True, False));
    end);
    RegisterScriptFunc('SAVESTRINGSTOUTF8FILEWITHOUTBOM', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, SaveStringsToFile(Stack.GetString(PStart-1), Stack, PStart-2, Stack.GetBool(PStart-3), True, True));
    end);
    RegisterScriptFunc('ENABLEFSREDIRECTION', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
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
    RegisterScriptFunc('GETUNINSTALLPROGRESSFORM', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetClass(PStart, GetUninstallProgressForm);
    end);
    RegisterScriptFunc('CREATECALLBACK', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetInt(PStart, CreateCallback(Caller, Stack.Items[PStart-1]));
    end);
    RegisterScriptFunc('ISDOTNETINSTALLED', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, IsDotNetInstalled(InstallDefaultRegView, TDotNetVersion(Stack.GetInt(PStart-1)), Stack.GetInt(PStart-2)));
    end);
    RegisterScriptFunc('ISMSIPRODUCTINSTALLED', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var ErrorCode: Cardinal;
      Stack.SetBool(PStart, IsMsiProductInstalled(Stack.GetString(PStart-1), Stack.GetInt64(PStart-2), ErrorCode));
      if ErrorCode <> 0 then
        raise Exception.Create(Win32ErrorString(ErrorCode));
    end);
    RegisterScriptFunc('INITIALIZEBITMAPIMAGEFROMICON', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var AscendingTrySizes := Stack.GetIntArray(PStart-4);
      Stack.SetBool(PStart, TBitmapImage(Stack.GetClass(PStart-1)).InitializeFromIcon(0, PChar(Stack.GetString(PStart-2)), Stack.GetInt(PStart-3), AscendingTrySizes));
    end);
    RegisterScriptFunc('EXTRACT7ZIPARCHIVE', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Extract7ZipArchive(Stack.GetString(PStart), Stack.GetString(PStart-1), Stack.GetBool(PStart-2), TOnExtractionProgress(Stack.GetProc(PStart-3, Caller)));
    end);
    RegisterScriptFunc('DEBUGGING', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      Stack.SetBool(PStart, Debugging);
    end);
    RegisterScriptFunc('StringJoin', procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var Values := Stack.GetStringArray(PStart-2);
      Stack.SetString(PStart, String.Join(Stack.GetString(PStart-1), Values));
    end);
    RegisterScriptFunc(['StringSplit', 'StringSplitEx'], procedure(const Caller: TPSExec; const OrgName: AnsiString; const Stack: TPSStack; const PStart: Cardinal)
    begin
      var Separators := Stack.GetStringArray(PStart-2);
      var Parts: TArray<String>;
      if OrgName = 'StringSplitEx' then begin
        var Quote := Stack.GetChar(PStart-3);
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
  RegisterActiveXScriptFuncs;
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
  RegisterDelphiFunction(@FindFirstHelper, 'FindFirst');
  RegisterDelphiFunction(@FindNextHelper, 'FindNext');
  RegisterDelphiFunction(@FindCloseHelper, 'FindClose');
  RegisterDelphiFunction(@FmtMessageHelper, 'FmtMessage');
  RegisterDelphiFunction(@Format, 'Format');
  RegisterDelphiFunction(@GetWindowsVersionExHelper, 'GetWindowsVersionEx');
  {$IFDEF DEBUG}
  if Count <> Length(DelphiScriptFuncTable) then
    raise Exception.Create('Count <> Length(DelphiScriptFuncTable)');
  {$ENDIF}
end;

initialization
finalization
  ScriptFuncs.Free;
end.
