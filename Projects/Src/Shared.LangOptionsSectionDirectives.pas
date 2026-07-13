unit Shared.LangOptionsSectionDirectives;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  LangOptions section directives
}

interface

const
  LangOptionsSectionDirectivePrefixLength = 2;
type
  TLangOptionsSectionDirective = (
    lsCopyrightFontName,
    lsCopyrightFontSize,
    lsDialogFontBaseScaleHeight,
    lsDialogFontBaseScaleWidth,
    lsDialogFontName,
    lsDialogFontSize,
    lsDialogFontStandardHeight,
    lsLanguageCodePage,
    lsLanguageID,
    lsLanguageName,
    lsRightToLeft,
    lsTitleFontName,
    lsTitleFontSize,
    lsWelcomeFontName,
    lsWelcomeFontSize);

const
  LangOptionsSectionDirectivesObsolete: set of TLangOptionsSectionDirective = [
    lsCopyrightFontName, lsCopyrightFontSize, lsDialogFontStandardHeight,
    lsTitleFontName, lsTitleFontSize];

implementation

end.
