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
  SysUtils, Classes, StrUtils, Math, Dialogs, Menus, Graphics,
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
  Assert(RegExToReplaceMode(True) = srmRegEx);
  Assert(RegExToReplaceMode(False) = srmMinimal);
end;

procedure TestWindowsVersionHelpers;
begin
  Assert(WindowsVersionAtLeast(6, 1)); { Windows 7 is the minimum supported }
  Assert(not WindowsVersionAtLeast(255, 255, 65535));
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

procedure TestCalculateMinimapWidth;
begin
  { Floor((1017 - 17 - 2) * 2 / (8 + 2)) = 199, plus the minimap gutter }
  Assert(CalculateMinimapWidth(1017, 17, 8, 2, 120, 2, 8) = 207);

  { A wider editor character, such as after zooming in, leaves fewer columns }
  Assert(CalculateMinimapWidth(1017, 17, 16, 2, 120, 2, 8) = 118);

  { The cap is Floor(120 * 2) = 240, minimap gutter included }
  Assert(CalculateMinimapWidth(1178, 17, 8, 2, 120, 2, 8) = 239);
  Assert(CalculateMinimapWidth(1179, 17, 8, 2, 120, 2, 8) = 240);
  Assert(CalculateMinimapWidth(2017, 17, 8, 2, 120, 2, 8) = 240);

  { A fractional minimap character width floors the cap: Floor(120 * 1.9375) = 232 }
  Assert(CalculateMinimapWidth(4017, 17, 8, 1.9375, 120, 2, 8) = 232);

  { Without room for any column, only the minimap gutter is left }
  Assert(CalculateMinimapWidth(19, 17, 8, 2, 120, 2, 8) = 8);
  Assert(CalculateMinimapWidth(17, 17, 8, 2, 120, 2, 8) = 8);
end;

procedure TestCalculateMinimapTopLine;
begin
  { A minimap which shows all lines doesn't scroll }
  Assert(CalculateMinimapTopLine(80, 100, 40, 30) = 0);
  Assert(CalculateMinimapTopLine(100, 100, 50, 30) = 0);

  { Neither does it when the memo shows all lines }
  Assert(CalculateMinimapTopLine(200, 100, 0, 200) = 0);

  { The minimap reaches its top and its end when the memo does: 950 * 900 / 950 = 900 }
  Assert(CalculateMinimapTopLine(1000, 100, 0, 50) = 0);
  Assert(CalculateMinimapTopLine(1000, 100, 950, 50) = 900);

  { In between it rounds down: 1 * 900 / 950 = 0.95 and 500 * 900 / 950 = 473.7 }
  Assert(CalculateMinimapTopLine(1000, 100, 1, 50) = 0);
  Assert(CalculateMinimapTopLine(1000, 100, 500, 50) = 473);

  { A large document doesn't overflow SliderTop's 2400000 * 950:
    2400000 * 2999050 / 3000000 = 2399240 }
  Assert(CalculateMinimapTopLine(3000050, 1000, 2400000, 50) = 2399240);

  { Scrolling down one line at a time never moves the top line up, also while
    ViewportLineCount shrinks, as when a long wrapped line comes into view at
    the memo's bottom }
  var Previous := 0;
  for var ViewportStartLine := 0 to 999 do begin
    const ViewportLineCount = Max(1, 50 - ViewportStartLine);
    const TopLine = CalculateMinimapTopLine(1000, 100, ViewportStartLine, ViewportLineCount);
    Assert(TopLine >= Previous);
    Previous := TopLine;
  end;
  Assert(Previous = 900);
end;

procedure TestBoostTextContrast;

  function Boost(const BackgroundColor, PixelColor: TColor;
    const ContrastBoost: Double = 3): TColor;
  begin
    var Pixel: TRGBQuad;
    Pixel.rgbBlue := GetBValue(DWORD(PixelColor));
    Pixel.rgbGreen := GetGValue(DWORD(PixelColor));
    Pixel.rgbRed := GetRValue(DWORD(PixelColor));
    Pixel.rgbReserved := 0;
    BoostTextContrast(@Pixel, 1, 1, 0, BackgroundColor, ContrastBoost);
    Assert(Pixel.rgbReserved = 0);
    Result := TColor(RGB(Pixel.rgbRed, Pixel.rgbGreen, Pixel.rgbBlue));
  end;

var
  Pixels: array[0..1, 0..2] of TRGBQuad;
begin
  { The background stays as it is }
  Assert(Boost($FFFFFF, $FFFFFF) = $FFFFFF);
  Assert(Boost($1F1F1F, $1F1F1F) = $1F1F1F);

  { Fully saturated pixels stay as they are, also when not all channels are,
    such as the error line's background, or the step line's with only one }
  Assert(Boost($FFFFFF, $000000) = $000000);
  Assert(Boost($FFFFFF, $000080) = $000080);
  Assert(Boost($FFFFFF, $D47800) = $D47800);
  Assert(Boost($808080, $FFFFFF) = $FFFFFF);

  { Pixels lighter than the background are boosted too: 64 * 4 / (128 / 255 * 3 + 1)
    rounds to 102 }
  Assert(Boost($808080, $C0C0C0) = $E6E6E6);

  { Faint pixels get about k+1 times stronger: 1 * 4 / (1 / 255 * 3 + 1) and
    10 * 4 / (10 / 255 * 3 + 1) round to 4 and 36 }
  Assert(Boost($FFFFFF, $FEFEFE) = $FBFBFB);
  Assert(Boost($FFFFFF, $F5F5F5) = $DBDBDB);

  { Light theme gray text at full coverage: 143 * 3 / (143 / 255 * 2 + 1) rounds to 202 }
  Assert(Boost($FFFFFF, $707070, 2) = $353535);

  { The hue stays: the channel differences 100, 50, and 0 become 184, 92, and 0 }
  Assert(Boost($FFFFFF, $FFCD9B) = $FFA347);

  { A stronger pixel stays stronger, and no pixel gets weaker }
  var Previous := 255;
  for var I := 255 downto 0 do begin
    const Boosted = GetRValue(DWORD(Boost($FFFFFF, I * $010101)));
    Assert((Boosted <= I) and (Boosted <= Previous));
    Previous := Boosted;
  end;

  { A negative k weakens faint pixels instead, but leaves the background and
    saturated pixels alone: 32 * 0.5 / (1 - 36 / 255 * 0.5) rounds to 17 }
  Assert(Boost($1F1F1F, $1F1F1F, -0.5) = $1F1F1F);
  Assert(Boost($1F1F1F, $000080, -0.5) = $000080);
  Assert(Boost($1F1F1F, $3F3F3F, -0.5) = $303030);

  { With a negative k a stronger pixel stays stronger too, and no pixel gets
    stronger }
  Previous := $1F;
  for var I := $1F to 255 do begin
    const Weakened = GetRValue(DWORD(Boost($1F1F1F, I * $010101, -0.5)));
    Assert((Weakened >= $1F) and (Weakened <= I) and (Weakened >= Previous));
    Previous := Weakened;
  end;

  { Every row is boosted, but only from Left on }
  const Left = 1;
  for var Y := Low(Pixels) to High(Pixels) do begin
    for var X := Low(Pixels[Y]) to High(Pixels[Y]) do begin
      Pixels[Y, X].rgbBlue := $70;
      Pixels[Y, X].rgbGreen := $70;
      Pixels[Y, X].rgbRed := $70;
      Pixels[Y, X].rgbReserved := 0;
    end;
  end;
  BoostTextContrast(@Pixels[0, 0], Length(Pixels[0]), Length(Pixels), Left, $FFFFFF, 3);
  for var Y := Low(Pixels) to High(Pixels) do begin
    for var X := Low(Pixels[Y]) to High(Pixels[Y]) do begin
      var Expected := $2A;
      if X < Left then
        Expected := $70;
      Assert((Pixels[Y, X].rgbBlue = Expected) and (Pixels[Y, X].rgbGreen = Expected) and
        (Pixels[Y, X].rgbRed = Expected));
    end;
  end;
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
  TestCalculateMinimapWidth;
  TestCalculateMinimapTopLine;
  TestBoostTextContrast;
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
