unit Shared.LangOptionsSectionDirectives;

{
  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
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
    
implementation

end.
