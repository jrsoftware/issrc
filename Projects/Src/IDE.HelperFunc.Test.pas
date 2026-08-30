unit IDE.HelperFunc.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for IDE.HelperFunc

  Runs a self-test if DEBUG is defined
}

interface

procedure IDEHelperFuncRunTests;

implementation

uses
  Windows,
  SysUtils, Classes, StrUtils, Dialogs, Menus,
  ScintEdit,
  IDE.Messages, IDE.LocalizeFunc, IDE.HelperFunc;

{$C+}

procedure TestStringHelpers;
begin
  { DoubleAmp }
  Assert(DoubleAmp('') = '');
  Assert(DoubleAmp('a & b && c') = 'a && b &&&& c');

  { GetFileTitle }
  Assert(GetFileTitle('') = LFmtMessage(SCompilerUntitledFile));
  Assert(GetFileTitle('C:\x.iss') = 'C:\x.iss');

  { GetDisplayFilename: strips the path. Extensionless to keep the result
    independent of the user's hide-extensions preference }
  Assert(GetDisplayFilename('C:\dir\name') = 'name');

  { IsISPPBuiltins: matches the name only, ignoring case }
  Assert(IsISPPBuiltins('C:\any\ISPPBuiltins.iss'));
  Assert(IsISPPBuiltins('isppbuiltins.ISS'));
  Assert(not IsISPPBuiltins('C:\any\Other.iss'));

  { FormatFileFilter }
  Assert(FormatFileFilter('My files', ['iss', 'isl']) =
    'My files (*.iss,*.isl)|*.iss;*.isl|' + LFmtMessage(SAllFiles) + '|*.*');
end;

procedure TestSearchOptionHelpers;
begin
  Assert(FindOptionsToSearchOptions([frMatchCase, frWholeWord], True) =
    [sfoMatchCase, sfoWholeWord, sfoRegEx]);
  Assert(FindOptionsToSearchOptions([frDown], False) = []);
  Assert(FindOptionsToSearchOptions(True, False) = [sfoMatchCase]);
  Assert(FindOptionsToSearchOptions(False, True) = [sfoRegEx]);
  Assert(RegExToReplaceMode(True) = srmRegEx);
  Assert(RegExToReplaceMode(False) = srmMinimal);
end;

procedure TestWindowsVersionHelpers;
begin
  Assert(WindowsVersionAtLeast(6, 1)); { Windows 7 is the minimum supported }
  Assert(not WindowsVersionAtLeast(255, 255, 65535));
  if IsWindows11 then
    Assert(IsWindows10);
end;

procedure TestMiscHelpers;
begin
  { GenerateGuid }
  const Guid = GenerateGuid;
  Assert((Length(Guid) = 38) and (Guid[1] = '{') and (Guid[38] = '}'));
  Assert(GenerateGuid <> Guid);

  { CreateBitmapInfo }
  const BitmapInfo = CreateBitmapInfo(16, -32, 24);
  Assert(BitmapInfo.bmiHeader.biSize = SizeOf(BitmapInfo.bmiHeader));
  Assert((BitmapInfo.bmiHeader.biWidth = 16) and (BitmapInfo.bmiHeader.biHeight = -32));
  Assert((BitmapInfo.bmiHeader.biPlanes = 1) and (BitmapInfo.bmiHeader.biBitCount = 24));
  Assert(BitmapInfo.bmiHeader.biCompression = BI_RGB);
  Assert(BitmapInfo.bmiHeader.biSizeImage = 0); { Zeroed }

  { GetPreferredMemoFont }
  const FontName = GetPreferredMemoFont;
  Assert((FontName = 'Consolas') or (FontName = 'Courier New'));

  { GetSourcePath: My Documents if the script was not saved }
  Assert(GetSourcePath('C:\foo\bar.iss') = 'C:\foo\');
  Assert(GetSourcePath('') <> '');

  { GetHelpFile follows SetHelpFileDark; not dark initially }
  Assert(EndsText('isetup.chm', GetHelpFile));
  SetHelpFileDark(True);
  Assert(EndsText('isetup-dark.chm', GetHelpFile));
  SetHelpFileDark(False); { Back to the default }
end;

procedure TestShortCutHelpers;
begin
  { ShortCutUsesOEMKey }
  Assert(ShortCutUsesOEMKey(ShortCut(VK_OEM_2, [ssCtrl])));
  Assert(not ShortCutUsesOEMKey(ShortCut(Ord('A'), [ssCtrl])));

  { NewShortCutToText: 'Ctrl+Shift+Alt' ordering, and '' for a key without a
    scan code }
  const CtrlText = LFmtMessage(SShortCutCtrl);
  Assert(NewShortCutToText(ShortCut(Ord('A'), [ssCtrl])) = CtrlText + 'A');
  Assert(NewShortCutToText(ShortCut(Ord('A'), [ssCtrl, ssShift, ssAlt])) =
    CtrlText + LFmtMessage(SShortCutShift) + LFmtMessage(SShortCutAlt) + 'A');
  Assert(NewShortCutToText(0) = '');

  { SetFakeShortCutText and SetFakeShortCut }
  const MenuItem = TMenuItem.Create(nil);
  try
    MenuItem.Caption := '&Open';
    SetFakeShortCutText(MenuItem, 'Ctrl+O');
    Assert(MenuItem.Caption = '&Open'#9'Ctrl+O');
    SetFakeShortCutText(MenuItem, 'F1'); { Replaces the old shortcut text }
    Assert(MenuItem.Caption = '&Open'#9'F1');
    SetFakeShortCutText(MenuItem, '');
    Assert(MenuItem.Caption = '&Open');
    SetFakeShortCut(MenuItem, Ord('A'), [ssCtrl]);
    Assert(MenuItem.Caption = '&Open'#9 + CtrlText + 'A');
  finally
    MenuItem.Free;
  end;
end;

procedure TestSetLowPriority;
begin
  const OriginalPriorityClass = GetPriorityClass(GetCurrentProcess);
  var SavePriorityClass: DWORD := 0;
  SetLowPriority(True, SavePriorityClass);
  Assert(GetPriorityClass(GetCurrentProcess) = IDLE_PRIORITY_CLASS);
  Assert(SavePriorityClass = OriginalPriorityClass);
  SetLowPriority(False, SavePriorityClass);
  Assert(GetPriorityClass(GetCurrentProcess) = OriginalPriorityClass);
  Assert(SavePriorityClass = 0);
end;

procedure TestGetSetupDirectiveDefaultValue;
begin
  Assert(GetSetupDirectiveDefaultValue('WizardStyle') = 'classic');
  Assert(GetSetupDirectiveDefaultValue('NoSuchDirective') = '');
end;

procedure TestTryStrToLanguage;
begin
  var Language: TIDELanguage;

  { Every tag maps back to its own language }
  for var ExpectedLanguage := Low(TIDELanguage) to High(TIDELanguage) do begin
    Assert(TryStrToLanguage(IDELanguageTags[ExpectedLanguage], Language));
    Assert(Language = ExpectedLanguage);
  end;

  { Case-insensitive }
  Assert(TryStrToLanguage('nL', Language));
  Assert(Language = ilDutch);

  { Extra subtags are dropped until a tag matches, or nothing is left }
  Assert(TryStrToLanguage('en-US', Language));
  Assert(Language = ilEnglish);
  Assert(TryStrToLanguage('ja-Jpan-JP', Language));
  Assert(Language = ilJapanese);
  Assert(not TryStrToLanguage('-en', Language));
  Assert(not TryStrToLanguage('-', Language));

  { Unknown tags, an unknown name, a full language name, and an empty tag are rejected }
  Assert(not TryStrToLanguage('tlh', Language));
  Assert(not TryStrToLanguage('tlh-Latn', Language));
  Assert(not TryStrToLanguage('klingon', Language));
  Assert(not TryStrToLanguage('dutch', Language));
  Assert(not TryStrToLanguage('', Language));
end;

procedure IDEHelperFuncRunTests;
begin
  TestStringHelpers;
  TestSearchOptionHelpers;
  TestWindowsVersionHelpers;
  TestMiscHelpers;
  TestShortCutHelpers;
  TestSetLowPriority;
  TestGetSetupDirectiveDefaultValue;
  TestTryStrToLanguage;
end;

{$IFDEF DEBUG}
initialization
  try
    IDEHelperFuncRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}

end.
