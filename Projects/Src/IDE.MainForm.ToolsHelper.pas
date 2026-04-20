unit IDE.MainForm.ToolsHelper;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler form - Tools helper which has the menu update helper as ancestor

  Not used by MainForm: it uses IDE.MainForm.FinalHelper instead
}

interface

uses
  Menus,
  ScintEdit,
  IDE.MainForm, IDE.MainForm.UpdateMenuHelper;

type
  TMainFormToolsHelper = class helper(TMainFormUpdateMenuHelper) for TMainForm
    procedure StartAddRemovePrograms;
    procedure InsertGeneratedGuid(const AMemo: TScintEdit);
    procedure ShowMsgBoxDesignerForm(const AMemo: TScintEdit);
    procedure ShowRegistryDesignerForm(const AMemo: TScintEdit);
    procedure ShowFilesDesignerForm(const AMemo: TScintEdit);
    procedure ShowSignToolsForm;
  end;

implementation

uses
  Windows,
  SysUtils, Forms, UITypes,
  PathFunc,
  Shared.CommonFunc, Shared.CommonFunc.Vcl, Shared.ConfigIniFile,
  IDE.Messages, IDE.HelperFunc, IDE.ScintStylerInnoSetup, IDE.SignToolsForm, IDE.MsgBoxDesignerForm,
  IDE.FilesDesignerForm, IDE.RegistryDesignerForm, IDE.Wizard.WizardFormRegistryHelper;

{$IFNDEF WIN64}
function Wow64DisableWow64FsRedirection_static(var OldValue: PVOID): BOOL; stdcall;
  external kernel32 name 'Wow64DisableWow64FsRedirection';
function Wow64RevertWow64FsRedirection_static(OldValue: PVOID): BOOL; stdcall;
  external kernel32 name 'Wow64RevertWow64FsRedirection';
{$ENDIF}

procedure TMainFormToolsHelper.StartAddRemovePrograms;
var
  Dir: String;
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
  {$IFNDEF WIN64}
  var RedirOldValue: PVOID;
  const RedirDisabled = Wow64DisableWow64FsRedirection_static(RedirOldValue);
  try
  {$ENDIF}
    Win32Check(CreateProcess(nil, PChar('"' + AddBackslash(Dir) + 'control.exe" appwiz.cpl'),
       nil, nil, False, 0, nil, PChar(Dir), StartupInfo, ProcessInfo));
  {$IFNDEF WIN64}
  finally
    if RedirDisabled then
      Wow64RevertWow64FsRedirection_static(RedirOldValue);
  end;
  {$ENDIF}
  CloseHandle(ProcessInfo.hProcess);
  CloseHandle(ProcessInfo.hThread);
end;

procedure TMainFormToolsHelper.InsertGeneratedGuid(const AMemo: TScintEdit);
begin
  if MsgBox(SToolsInsertGuidConfirm,
     SCompilerFormCaption, mbConfirmation, MB_YESNO) = IDYES then
    AMemo.MainSelText := GenerateGuid;
end;

procedure TMainFormToolsHelper.ShowMsgBoxDesignerForm(const AMemo: TScintEdit);
begin
  if (TInnoSetupStyler.GetSectionFromLineState(AMemo.Lines.State[AMemo.CaretLine]) <> scCode) and
     (MsgBox(SToolsNotInCodeSectionConfirm,
      SCompilerFormCaption, mbConfirmation, MB_YESNO) = IDNO) then
    Exit;

  var MsgBoxForm := TMsgBoxDesignerForm.Create(Application);
  try
    if MsgBoxForm.ShowModal = mrOk then
      AMemo.MainSelText := MsgBoxForm.GetText(FOptions.TabWidth, FOptions.UseTabCharacter);
  finally
    MsgBoxForm.Free;
  end;
end;

procedure TMainFormToolsHelper.ShowRegistryDesignerForm(const AMemo: TScintEdit);
begin
  var RegistryDesignerForm := TRegistryDesignerForm.Create(Application);
  try
    var PrivilegesRequired := FindSetupDirectiveValue('PrivilegesRequired', 'admin');
    var PrivilegesRequiredOverridesAllowed := FindSetupDirectiveValue('PrivilegesRequiredOverridesAllowed', '');
    if PrivilegesRequiredOverridesAllowed = '' then begin
      if SameText(PrivilegesRequired, 'admin') then
        RegistryDesignerForm.PrivilegesRequired := prAdmin
      else
        RegistryDesignerForm.PrivilegesRequired := prLowest
    end else
      RegistryDesignerForm.PrivilegesRequired := prDynamic;
    if RegistryDesignerForm.ShowModal = mrOk then
    begin
      AMemo.CaretColumn := 0;
      var Text := RegistryDesignerForm.Text;
      if TInnoSetupStyler.GetSectionFromLineState(AMemo.Lines.State[AMemo.CaretLine]) <> scRegistry then
        Text := '[Registry]' + SNewLine + Text;
      AMemo.MainSelText := Text;
    end;
  finally
    RegistryDesignerForm.Free;
  end;
end;

procedure TMainFormToolsHelper.ShowFilesDesignerForm(const AMemo: TScintEdit);
begin
  var FilesDesignerForm := TFilesDesignerForm.Create(Application);
  try
    FilesDesignerForm.CreateAppDir := FindSetupDirectiveValue('CreateAppDir', True);
    if FilesDesignerForm.ShowModal = mrOk then begin
      AMemo.CaretColumn := 0;
      var Text := FilesDesignerForm.Text;
      if TInnoSetupStyler.GetSectionFromLineState(AMemo.Lines.State[AMemo.CaretLine]) <> scFiles then
        Text := '[Files]' + SNewLine + Text;
      AMemo.MainSelText := Text;
    end;
  finally
    FilesDesignerForm.Free;
  end;
end;

procedure TMainFormToolsHelper.ShowSignToolsForm;
var
  SignToolsForm: TSignToolsForm;
  Ini: TConfigIniFile;
  I: Integer;
begin
  SignToolsForm := TSignToolsForm.Create(Application);
  try
    SignToolsForm.SignTools := FSignTools;

    if SignToolsForm.ShowModal = mrOk then begin
      FSignTools.Assign(SignToolsForm.SignTools);

      { Save new options }
      Ini := TConfigIniFile.Create;
      try
        Ini.EraseSection('SignTools');
        for I := 0 to FSignTools.Count-1 do
          Ini.WriteString('SignTools', 'SignTool' + IntToStr(I), FSignTools[I]);
      finally
        Ini.Free;
      end;
    end;
  finally
    SignToolsForm.Free;
  end;
end;

end.
