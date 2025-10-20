unit Shared.LangOptionsSectionDirectives;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  LangOptions section directives
}

interface

const
  LangOptionsSectionDirectivePrefixLength = 2;
type
  TLangOptionsSectionDirective = (
    lsCopyrightFontName, { obsolete }
    lsCopyrightFontSize, { obsolete }
    lsDialogFontBaseScaleHeight,
    lsDialogFontBaseScaleWidth,
    lsDialogFontName,
    lsDialogFontSize,
    lsDialogFontStandardHeight,
    lsLanguageCodePage,
    lsLanguageID,
    lsLanguageName,
    lsRightToLeft,
    lsTitleFontName, { obsolete }
    lsTitleFontSize, { obsolete }
    lsWelcomeFontName,
    lsWelcomeFontSize);

implementation

end.
