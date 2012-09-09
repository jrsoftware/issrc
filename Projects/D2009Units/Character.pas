unit Character;

{
  Inno Setup
  Copyright (C) 1997-2012 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Allows Delphi 2009 to to not include the Characters.res.

  Note: The interface section must match the original RTL Characters.pas unit, so that SysUtils
  does not need to be recompiled.
}


interface

uses
  SysUtils;

{$SCOPEDENUMS ON}

type
  ENotImplemented = class(Exception);

  TUnicodeCategory = (
    ucControl,
    ucFormat,
    ucUnassigned,
    ucPrivateUse,
    ucSurrogate,
    ucLowercaseLetter,
    ucModifierLetter,
    ucOtherLetter,
    ucTitlecaseLetter,
    ucUppercaseLetter,
    ucCombiningMark,
    ucEnclosingMark,
    ucNonSpacingMark,
    ucDecimalNumber,
    ucLetterNumber,
    ucOtherNumber,
    ucConnectPunctuation,
    ucDashPunctuation,
    ucClosePunctuation,
    ucFinalPunctuation,
    ucInitialPunctuation,
    ucOtherPunctuation,
    ucOpenPunctuation,
    ucCurrencySymbol,
    ucModifierSymbol,
    ucMathSymbol,
    ucOtherSymbol,
    ucLineSeparator,
    ucParagraphSeparator,
    ucSpaceSeparator
  );

  TUnicodeBreak = (
    ubMandatory,
    ubCarriageReturn,
    ubLineFeed,
    ubCombiningMark,
    ubSurrogate,
    ubZeroWidthSpace,
    ubInseparable,
    ubNonBreakingGlue,
    ubContingent,
    ubSpace,
    ubAfter,
    ubBefore,
    ubBeforeAndAfter,
    ubHyphen,
    ubNonStarter,
    ubOpenPunctuation,
    ubClosePunctuation,
    ubQuotation,
    ubExclamation,
    ubIdeographic,
    ubNumeric,
    ubInfixSeparator,
    ubSymbol,
    ubAlphabetic,
    ubPrefix,
    ubPostfix,
    ubComplexContext,
    ubAmbiguous,
    ubUnknown,
    ubNextLine,
    ubWordJoiner,
    ubHangulLJamo,
    ubHangulVJamo,
    ubHangulTJamo,
    ubHangulLvSyllable,
    ubHangulLvtSyllable
  );

type
  TCharacter = class sealed
  {$HINTS OFF}
  private
    class procedure Initialize; static;
    class function IsLatin1(C: Char): Boolean; inline; static;
    class function IsAscii(C: Char): Boolean; inline; static;
    class function CheckLetter(uc: TUnicodeCategory): Boolean; inline; static;
    class function CheckLetterOrDigit(uc: TUnicodeCategory): Boolean; inline; static;
    class function CheckNumber(uc: TUnicodeCategory): Boolean; inline; static;
    class function CheckPunctuation(uc: TUnicodeCategory): Boolean; inline; static;
    class function CheckSymbol(uc: TUnicodeCategory): Boolean; inline; static;
    class function CheckSeparator(uc: TUnicodeCategory): Boolean; inline; static;
  {$HINTS ON}
  public
    constructor Create;
    class function ConvertFromUtf32(C: UCS4Char): string; static;
    class function ConvertToUtf32(const S: string; Index: Integer): UCS4Char; overload; inline; static;
    class function ConvertToUtf32(const S: string; Index: Integer; out CharLength: Integer): UCS4Char; overload; static;
    class function ConvertToUtf32(const HighSurrogate, LowSurrogate: Char): UCS4Char; overload; static;
    class function GetNumericValue(C: Char): Double; overload; static;
    class function GetNumericValue(const S: string; Index: Integer): Double; overload; static;
    class function GetUnicodeCategory(C: Char): TUnicodeCategory; overload; static;
    class function GetUnicodeCategory(const S: string; Index: Integer): TUnicodeCategory; overload; static;
    class function IsControl(C: Char): Boolean; overload; static;
    class function IsControl(const S: string; Index: Integer): Boolean; overload; static;
    class function IsDigit(C: Char): Boolean; overload; static;
    class function IsDigit(const S: string; Index: Integer): Boolean; overload; static;
    class function IsHighSurrogate(C: Char): Boolean; overload; inline; static;
    class function IsHighSurrogate(const S: string; Index: Integer): Boolean; overload; inline; static;
    class function IsLetter(C: Char): Boolean; overload; static;
    class function IsLetter(const S: string; Index: Integer): Boolean; overload; static;
    class function IsLetterOrDigit(C: Char): Boolean; overload; static;
    class function IsLetterOrDigit(const S: string; Index: Integer): Boolean; overload; static;
    class function IsLower(C: Char): Boolean; overload; static;
    class function IsLower(const S: string; Index: Integer): Boolean; overload; static;
    class function IsLowSurrogate(C: Char): Boolean; overload; inline; static;
    class function IsLowSurrogate(const S: string; Index: Integer): Boolean; overload; inline; static;
    class function IsNumber(C: Char): Boolean; overload; static;
    class function IsNumber(const S: string; Index: Integer): Boolean; overload; static;
    class function IsPunctuation(C: Char): Boolean; overload; static;
    class function IsPunctuation(const S: string; Index: Integer): Boolean; overload; static;
    class function IsSeparator(C: Char): Boolean; overload; static;
    class function IsSeparator(const S: string; Index: Integer): Boolean; overload; static;
    class function IsSurrogate(Surrogate: Char): Boolean; overload; inline; static;
    class function IsSurrogate(const S: string; Index: Integer): Boolean; overload; static;
    class function IsSurrogatePair(const HighSurrogate, LowSurrogate: Char): Boolean; overload; inline; static;
    class function IsSurrogatePair(const S: string; Index: Integer): Boolean; overload; static;
    class function IsSymbol(C: Char): Boolean; overload; static;
    class function IsSymbol(const S: string; Index: Integer): Boolean; overload; static;
    class function IsUpper(C: Char): Boolean; overload; static;
    class function IsUpper(const S: string; Index: Integer): Boolean; overload; static;
    class function IsWhiteSpace(C: Char): Boolean; overload; static;
    class function IsWhiteSpace(const S: string; Index: Integer): Boolean; overload; static;
    class function ToLower(C: Char): Char; overload; static;
    class function ToLower(const S: string): string; overload; static;
    class function ToUpper(C: Char): Char; overload; static;
    class function ToUpper(const S: string): string; overload; static;
  end;

implementation

{ TCharacter }

class function TCharacter.CheckLetter(uc: TUnicodeCategory): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.CheckLetterOrDigit(uc: TUnicodeCategory): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.CheckNumber(uc: TUnicodeCategory): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.CheckPunctuation(uc: TUnicodeCategory): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.CheckSeparator(uc: TUnicodeCategory): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.CheckSymbol(uc: TUnicodeCategory): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsLatin1(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsLetter(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsLetter(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsLetterOrDigit(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsLetterOrDigit(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsAscii(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsControl(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsControl(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsDigit(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsDigit(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsHighSurrogate(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsLowSurrogate(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsSurrogate(Surrogate: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsSurrogatePair(const HighSurrogate, LowSurrogate: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.GetUnicodeCategory(C: Char): TUnicodeCategory;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.ConvertToUtf32(const S: string; Index: Integer; out CharLength: Integer): UCS4Char;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.ConvertToUtf32(const S: string; Index: Integer): UCS4Char;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.ConvertFromUtf32(C: UCS4Char): string;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.ConvertToUtf32(const HighSurrogate, LowSurrogate: Char): UCS4Char;
begin
  raise ENotImplemented.Create('');
end;

constructor TCharacter.Create;
begin
  raise ENotImplemented.Create('');
end;

class procedure TCharacter.Initialize;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.GetNumericValue(C: Char): Double;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.GetNumericValue(const S: string; Index: Integer): Double;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.GetUnicodeCategory(const S: string; Index: Integer): TUnicodeCategory;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsHighSurrogate(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsLower(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsLower(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsLowSurrogate(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsNumber(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsNumber(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsPunctuation(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsPunctuation(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsSeparator(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsSeparator(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsSurrogate(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsSurrogatePair(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsSymbol(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsSymbol(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsUpper(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsUpper(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsWhiteSpace(const S: string; Index: Integer): Boolean;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.ToLower(C: Char): Char;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.ToLower(const S: string): string;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.ToUpper(C: Char): Char;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.ToUpper(const S: string): string;
begin
  raise ENotImplemented.Create('');
end;

class function TCharacter.IsWhiteSpace(C: Char): Boolean;
begin
  raise ENotImplemented.Create('');
end;

end.
