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

procedure TestCalculateMinimapWidths;
const
  MemoGutterWidth = 50;
  VerticalScrollbarWidth = 17;
  MinimapMaxColumn = 120;
  MinimapGutterWidth = 8;

  procedure Check(const MemoAndMinimapWidth: Integer; const EditorCharWidth, MinimapCharWidth: Double;
    const ExpectedMinimapWidth, ExpectedBlankWidth: Integer; const EditorFontItalic: Boolean = False);
  begin
    const MinimapWidths = CalculateMinimapWidths(MemoAndMinimapWidth, MemoGutterWidth,
      VerticalScrollbarWidth, EditorCharWidth, MinimapCharWidth, EditorFontItalic, MinimapMaxColumn,
      MinimapGutterWidth);
    Assert(MinimapWidths.MinimapWidth = ExpectedMinimapWidth);
    Assert(MinimapWidths.BlankWidth = ExpectedBlankWidth);
  end;

  function GetColumnCount(const TextAreaWidth: Integer; const CharWidth: Double): Integer;
  begin
    { Scintilla wraps a character unless it ends before the text area's edge }
    Result := Ceil(TextAreaWidth / CharWidth) - 1;
  end;

  procedure CheckWidthsBelowCap(const EditorCharWidth, MinimapCharWidth: Double;
    const EditorFontItalic: Boolean);
  begin
    { Scintilla adds this to the end of a line that ends in italic text }
    const LastSegmentItalicsOffset = IfThen(EditorFontItalic, 2, 0);
    for var MemoAndMinimapWidth := MemoGutterWidth + 50 to MemoGutterWidth + 800 do begin
      const MinimapWidths = CalculateMinimapWidths(MemoAndMinimapWidth, MemoGutterWidth,
        VerticalScrollbarWidth, EditorCharWidth, MinimapCharWidth, EditorFontItalic, MinimapMaxColumn,
        MinimapGutterWidth);
      const EditorTextAreaWidth = MemoAndMinimapWidth - MemoGutterWidth - VerticalScrollbarWidth -
        MinimapWidths.MinimapWidth - MinimapWidths.BlankWidth;
      const MinimapTextAreaWidth = MinimapWidths.MinimapWidth - MinimapGutterWidth;
      const ColumnCount = GetColumnCount(MinimapTextAreaWidth, MinimapCharWidth);
      { The memo and the minimap hold the same columns, also on the last
        display line of a wrapped line }
      Assert(GetColumnCount(EditorTextAreaWidth, EditorCharWidth) = ColumnCount);
      Assert(GetColumnCount(EditorTextAreaWidth - LastSegmentItalicsOffset, EditorCharWidth) = ColumnCount);
      { No more columns would fit }
      Assert(Floor((ColumnCount + 1) * EditorCharWidth) + Floor((ColumnCount + 1) * MinimapCharWidth) + 2 +
        LastSegmentItalicsOffset > MemoAndMinimapWidth - MemoGutterWidth - VerticalScrollbarWidth -
        MinimapGutterWidth);
      { The minimap's text area is the narrowest that holds the columns, and
        the memo's text area gets all the width it can use }
      Assert(GetColumnCount(MinimapTextAreaWidth - 1, MinimapCharWidth) < ColumnCount);
      Assert((MinimapWidths.BlankWidth = 0) or
        (GetColumnCount(EditorTextAreaWidth + 1, EditorCharWidth) > ColumnCount));
      Assert((MinimapWidths.BlankWidth >= 0) and
        (MinimapWidths.BlankWidth <= Ceil(MinimapCharWidth) + LastSegmentItalicsOffset));
    end;
  end;

begin
  { 968 - 50 - 17 - 8 = 893 pixels hold 99 columns in the memo and the
    minimap: 99 * 8 + 1 = 793 for the memo and 99 + 1 = 100 for the minimap }
  Check(968, 8, 1, 108, 0);

  { 901 pixels don't hold 100 columns, and 800 pixels are the most that hold
    the memo's 99, so 1 pixel stays blank }
  Check(976, 8, 1, 108, 1);

  { A minimap gutter wider than a memo column: 867 - 75 = 792 pixels hold 98
    columns, 98 * 7 + 1 = 687 and 98 + 1 = 99, and the memo gets
    792 - 99 = 693, the most that hold 98 }
  Check(867, 7, 1, 107, 0);

  { Wider minimap characters: 977 - 75 = 902 pixels hold 50 columns,
    50 * 16 + 1 = 801 and 50 * 2 + 1 = 101 }
  Check(977, 16, 2, 109, 0);

  { Fractional character widths: 175 - 75 = 100 pixels hold 11 columns,
    Floor(11 * 7.5) + 1 = 83 and Floor(11 * 1.5) + 1 = 17, although
    11 * (7.5 + 1.5) + 2 is 101 }
  Check(175, 7.5, 1.5, 25, 0);

  { The cap is 120, minimap gutter included: 112 pixels hold 111 columns, and
    112 columns would need 113. While the cap limits the minimap's width, the
    memo gets the rest. }
  Check(1076, 8, 1, 120, 0);
  Check(1085, 8, 1, 120, 0);
  Check(2067, 8, 1, 120, 0);

  { A fractional minimap character width floors the cap: Floor(120 * 1.9375) = 232 }
  Check(4067, 8, 1.9375, 232, 0);

  { An italic memo font needs 2 more pixels for the last display line of a
    wrapped line, so 893 pixels hold only 98 columns: 98 * 8 + 1 + 2 = 787 for
    the memo and 98 + 1 = 99 for the minimap. The memo gets 792, the most that
    hold 98, so 2 pixels stay blank. }
  Check(968, 8, 1, 107, 2, True);

  { Without room for any column, the minimap gets its gutter and the narrowest
    text area for 0 columns, 1 pixel }
  Check(75, 8, 1, 9, 0);
  Check(65, 8, 1, 9, 0);

  for var EditorFontItalic := False to True do begin
    CheckWidthsBelowCap(7, 1, EditorFontItalic);
    CheckWidthsBelowCap(8, 1, EditorFontItalic);
    CheckWidthsBelowCap(11, 1, EditorFontItalic);
    CheckWidthsBelowCap(16, 2, EditorFontItalic);
    CheckWidthsBelowCap(7.5, 1.5, EditorFontItalic);
  end;
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
    ViewportLineCount shrinks, as when the minimap wraps the line at the
    memo's bottom into fewer display lines than the memo }
  var Previous := 0;
  for var ViewportStartLine := 0 to 999 do begin
    const ViewportLineCount = Max(1, 50 - ViewportStartLine);
    const TopLine = CalculateMinimapTopLine(1000, 100, ViewportStartLine, ViewportLineCount);
    Assert(TopLine >= Previous);
    Previous := TopLine;
  end;
  Assert(Previous = 900);
end;

procedure TestCalculateMinimapViewportStartLineFromDelta;
begin
  { A click without a move doesn't scroll }
  Assert(CalculateMinimapViewportStartLineFromDelta(1000, 100, 500, 50, 0) = 500);

  { The slider follows the mouse: the slider moves 950 / 50 = 19 times slower
    than the memo, and CalculateMinimapTopLine puts it at 190 - 180 = 10 }
  Assert(CalculateMinimapViewportStartLineFromDelta(1000, 100, 0, 50, 10) = 190);
  Assert(CalculateMinimapTopLine(1000, 100, 190, 50) = 180);

  { It rounds to nearest: 19 / 3 = 6.3 and 38 / 3 = 12.7 }
  Assert(CalculateMinimapViewportStartLineFromDelta(1000, 100, 0, 50, 1/3) = 6);
  Assert(CalculateMinimapViewportStartLineFromDelta(1000, 100, 0, 50, 2/3) = 13);

  { Halves round up, not to the nearest even line: 20.5, 21.5, and when moving
    up, 19.5 }
  Assert(CalculateMinimapViewportStartLineFromDelta(80, 100, 20, 30, 0.5) = 21);
  Assert(CalculateMinimapViewportStartLineFromDelta(80, 100, 21, 30, 0.5) = 22);
  Assert(CalculateMinimapViewportStartLineFromDelta(80, 100, 20, 30, -0.5) = 20);

  { A minimap which shows all lines moves the slider as fast as the memo }
  Assert(CalculateMinimapViewportStartLineFromDelta(80, 100, 20, 30, 5) = 25);
  Assert(CalculateMinimapViewportStartLineFromDelta(80, 100, 20, 30, -5) = 15);
  Assert(CalculateMinimapViewportStartLineFromDelta(80, 100, 20, 30, 0) = 20);
  Assert(CalculateMinimapViewportStartLineFromDelta(80, 100, 20, 30, 2/3) = 21);

  { It clamps to the minimap's lines, not to the last start line the memo
    allows }
  Assert(CalculateMinimapViewportStartLineFromDelta(1000, 100, 900, 50, 10) = 999);
  Assert(CalculateMinimapViewportStartLineFromDelta(1000, 100, 10, 50, -10) = 0);
  Assert(CalculateMinimapViewportStartLineFromDelta(80, 100, 20, 30, 100) = 79);

  { A slider which can't move doesn't scroll, also when the memo shows all lines }
  Assert(CalculateMinimapViewportStartLineFromDelta(1000, 100, 500, 100, 5) = 500);
  Assert(CalculateMinimapViewportStartLineFromDelta(1000, 100, 500, 120, -5) = 500);
  Assert(CalculateMinimapViewportStartLineFromDelta(80, 100, 0, 80, 5) = 0);
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
  TestCalculateMinimapWidths;
  TestCalculateMinimapTopLine;
  TestCalculateMinimapViewportStartLineFromDelta;
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
