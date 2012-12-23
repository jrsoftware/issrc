unit Character;

{
  Inno Setup
  Copyright (C) 1997-2012 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Dummy unit to allow Delphi 2009 to not include Character.res, saving 32 KB.

  Note: The interface section must match the original RTL Character.pas unit, so that SysUtils
  does not need to be recompiled.
}

{$IFNDEF VER200}
  {$MESSAGE ERROR 'Only Delphi 2009 is supported by this unit.'}
{$ENDIF}

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

uses
  Windows;

const
  // Runlength encoded array[0..65535] of TUnicodeCategory.
  UnicodeCategorieBytes: array[0..2351 - 1] of Byte = (
    $9F, $00, $1D, $55, $17, $55, $16, $12, $15, $19, $15, $11, $35, $89, $0D, $35,
    $59, $35, $99, $09, $16, $15, $12, $18, $10, $18, $99, $05, $16, $19, $12, $19,
    $A0, $00, $1D, $15, $77, $3A, $18, $1A, $05, $14, $19, $11, $1A, $18, $1A, $19,
    $2F, $18, $05, $1A, $15, $18, $0F, $05, $13, $4F, $15, $96, $09, $19, $86, $09,
    $97, $05, $19, $87, $05, $9B, $89, $05, $87, $85, $09, $25, $96, $89, $05, $29,
    $81, $85, $09, $45, $29, $81, $85, $09, $09, $05, $49, $25, $69, $05, $29, $05,
    $49, $45, $29, $05, $29, $82, $85, $09, $81, $89, $05, $81, $85, $09, $09, $05,
    $49, $81, $85, $09, $09, $25, $07, $09, $45, $67, $09, $08, $05, $09, $08, $05,
    $09, $08, $87, $85, $09, $25, $88, $89, $05, $05, $09, $08, $81, $85, $09, $29,
    $9C, $85, $09, $86, $05, $29, $05, $29, $25, $09, $05, $69, $83, $85, $09, $C4,
    $05, $07, $9A, $05, $91, $06, $78, $8B, $06, $8D, $18, $84, $06, $86, $18, $81,
    $86, $18, $8F, $18, $EF, $0C, $81, $89, $05, $06, $18, $09, $05, $22, $06, $45,
    $15, $84, $02, $38, $09, $15, $49, $81, $82, $09, $09, $05, $90, $09, $02, $88,
    $09, $A2, $05, $09, $25, $49, $45, $8B, $89, $05, $65, $09, $05, $19, $09, $05,
    $29, $25, $B2, $09, $AF, $05, $90, $89, $05, $1A, $84, $0C, $2B, $9A, $89, $05,
    $29, $85, $85, $09, $25, $AA, $89, $05, $8A, $02, $A5, $09, $22, $06, $85, $15,
    $02, $A6, $05, $02, $15, $11, $85, $02, $AC, $0C, $11, $0C, $15, $2C, $15, $2C,
    $15, $0C, $87, $02, $9A, $07, $84, $02, $47, $35, $8A, $02, $61, $22, $59, $35,
    $17, $35, $3A, $8A, $0C, $15, $22, $35, $02, $9E, $07, $06, $89, $07, $93, $0C,
    $02, $89, $0D, $75, $27, $0C, $E2, $07, $15, $07, $86, $0C, $01, $0B, $85, $0C,
    $26, $2C, $1A, $6C, $27, $89, $0D, $47, $3A, $07, $8D, $15, $02, $01, $07, $0C,
    $9D, $07, $9A, $0C, $22, $D8, $07, $8A, $0C, $07, $8D, $02, $89, $0D, $A0, $07,
    $88, $0C, $26, $1A, $55, $06, $84, $02, $95, $07, $6C, $06, $88, $0C, $06, $4C,
    $06, $84, $0C, $22, $8E, $15, $C0, $22, $4C, $0A, $B5, $07, $22, $0C, $07, $4A,
    $87, $0C, $6A, $0C, $0A, $02, $07, $84, $0C, $22, $89, $07, $2C, $35, $89, $0D,
    $15, $06, $07, $85, $02, $86, $07, $02, $0C, $2A, $02, $87, $07, $22, $27, $22,
    $95, $07, $02, $86, $07, $02, $07, $42, $67, $22, $0C, $07, $4A, $6C, $22, $2A,
    $22, $2A, $0C, $07, $87, $02, $0A, $62, $27, $02, $47, $2C, $22, $89, $0D, $27,
    $37, $85, $0F, $1A, $17, $84, $02, $2C, $0A, $02, $85, $07, $62, $27, $22, $95,
    $07, $02, $86, $07, $02, $27, $02, $27, $02, $27, $22, $0C, $02, $4A, $2C, $62,
    $2C, $22, $4C, $42, $0C, $86, $02, $67, $02, $07, $86, $02, $89, $0D, $2C, $47,
    $0C, $8A, $02, $2C, $0A, $02, $88, $07, $02, $47, $02, $95, $07, $02, $86, $07,
    $02, $27, $02, $84, $07, $22, $0C, $07, $4A, $84, $0C, $02, $2C, $0A, $02, $2A,
    $0C, $22, $07, $8E, $02, $27, $2C, $22, $89, $0D, $02, $17, $8E, $02, $0C, $2A,
    $02, $87, $07, $22, $27, $22, $95, $07, $02, $86, $07, $02, $27, $02, $84, $07,
    $22, $0C, $07, $81, $8A, $0C, $4C, $22, $2A, $22, $2A, $0C, $87, $02, $0C, $0A,
    $62, $27, $02, $47, $2C, $22, $89, $0D, $1A, $07, $8F, $02, $0C, $07, $02, $85,
    $07, $42, $47, $02, $67, $42, $27, $81, $82, $07, $07, $42, $27, $42, $47, $42,
    $8B, $07, $62, $2A, $0C, $2A, $42, $4A, $02, $4A, $0C, $22, $07, $85, $02, $0A,
    $8D, $02, $89, $0D, $4F, $85, $1A, $17, $1A, $85, $02, $4A, $02, $87, $07, $02,
    $47, $02, $96, $07, $02, $89, $07, $02, $84, $07, $42, $07, $4C, $6A, $02, $4C,
    $02, $6C, $86, $02, $2C, $02, $27, $85, $02, $27, $2C, $22, $89, $0D, $87, $02,
    $86, $0F, $1A, $22, $2A, $02, $87, $07, $02, $47, $02, $96, $07, $02, $89, $07,
    $02, $84, $07, $22, $0C, $07, $0A, $0C, $84, $0A, $02, $0C, $2A, $02, $2A, $2C,
    $86, $02, $2A, $86, $02, $07, $02, $27, $2C, $22, $89, $0D, $02, $3A, $8E, $02,
    $2A, $02, $87, $07, $02, $47, $02, $96, $07, $02, $8F, $07, $42, $07, $4A, $6C,
    $02, $4A, $02, $4A, $0C, $88, $02, $0A, $87, $02, $27, $2C, $22, $89, $0D, $85,
    $0F, $42, $1A, $85, $07, $22, $2A, $02, $91, $07, $42, $97, $07, $02, $88, $07,
    $02, $07, $22, $86, $07, $42, $0C, $62, $4A, $4C, $02, $0C, $02, $87, $0A, $91,
    $02, $2A, $15, $8B, $02, $AF, $07, $0C, $27, $86, $0C, $62, $17, $85, $07, $06,
    $87, $0C, $15, $89, $0D, $35, $A4, $02, $27, $02, $07, $22, $27, $02, $07, $22,
    $07, $85, $02, $67, $02, $86, $07, $02, $47, $81, $82, $07, $22, $27, $02, $67,
    $0C, $27, $85, $0C, $02, $2C, $07, $22, $84, $07, $02, $06, $02, $85, $0C, $22,
    $89, $0D, $22, $27, $A1, $02, $07, $5A, $8E, $15, $84, $1A, $2C, $85, $1A, $89,
    $0D, $89, $0F, $82, $9A, $0C, $81, $96, $12, $2A, $87, $07, $02, $A3, $07, $62,
    $8D, $0C, $0A, $84, $0C, $15, $2C, $67, $62, $87, $0C, $02, $A3, $0C, $02, $87,
    $1A, $0C, $85, $1A, $02, $3A, $84, $15, $7A, $A6, $02, $AA, $07, $2A, $6C, $0A,
    $85, $0C, $0A, $2C, $2A, $2C, $07, $89, $0D, $85, $15, $85, $07, $2A, $2C, $67,
    $4C, $07, $4A, $27, $86, $0A, $47, $6C, $8C, $07, $0C, $2A, $2C, $85, $0A, $0C,
    $07, $0A, $89, $0D, $4A, $0C, $3A, $A5, $09, $89, $02, $AA, $07, $15, $06, $42,
    $C8, $47, $02, $67, $22, $86, $07, $81, $82, $07, $47, $22, $A8, $07, $02, $67,
    $22, $A0, $07, $02, $67, $22, $86, $07, $81, $82, $07, $47, $22, $8E, $07, $02,
    $B8, $07, $02, $67, $22, $C2, $07, $62, $0C, $1A, $87, $15, $93, $0F, $42, $8F,
    $07, $89, $1A, $85, $02, $D4, $07, $8A, $02, $11, $EB, $87, $3F, $35, $90, $07,
    $1D, $99, $07, $16, $12, $42, $CA, $07, $55, $4E, $8E, $02, $8C, $07, $02, $67,
    $4C, $8A, $02, $91, $07, $4C, $35, $88, $02, $91, $07, $2C, $8B, $02, $8C, $07,
    $02, $47, $02, $2C, $8B, $02, $B3, $07, $21, $0A, $86, $0C, $87, $0A, $0C, $2A,
    $8A, $0C, $55, $06, $55, $17, $07, $0C, $22, $89, $0D, $85, $02, $89, $0F, $85,
    $02, $85, $15, $11, $75, $4C, $1D, $02, $89, $0D, $85, $02, $A2, $07, $06, $B3,
    $07, $87, $02, $A8, $07, $0C, $07, $84, $02, $C5, $07, $89, $02, $9C, $07, $42,
    $4C, $6A, $2C, $4A, $62, $2A, $0C, $85, $0A, $4C, $62, $1A, $42, $35, $89, $0D,
    $9D, $07, $22, $84, $07, $8A, $02, $AB, $07, $62, $90, $0A, $86, $07, $2A, $85,
    $02, $8A, $0D, $42, $35, $9F, $1A, $96, $07, $2C, $4A, $22, $35, $B4, $07, $81,
    $8A, $0C, $85, $0C, $02, $81, $8C, $0A, $0A, $87, $0C, $85, $0A, $89, $0C, $22,
    $0C, $89, $0D, $85, $02, $89, $0D, $85, $02, $86, $15, $06, $85, $15, $D1, $02,
    $6C, $0A, $AE, $07, $0C, $0A, $84, $0C, $0A, $0C, $84, $0A, $0C, $2A, $86, $07,
    $62, $89, $0D, $86, $15, $89, $1A, $88, $0C, $88, $1A, $42, $2C, $0A, $9D, $07,
    $0A, $6C, $2A, $2C, $0A, $42, $27, $89, $0D, $C5, $02, $A3, $07, $87, $0A, $87,
    $0C, $2A, $2C, $42, $84, $15, $89, $0D, $42, $47, $89, $0D, $9D, $07, $85, $06,
    $35, $CF, $02, $4C, $15, $8C, $0C, $0A, $86, $0C, $67, $0C, $67, $0A, $8C, $02,
    $AB, $05, $B5, $06, $95, $05, $06, $A1, $05, $A4, $06, $A6, $0C, $95, $02, $4C,
    $CA, $89, $05, $87, $05, $B0, $89, $05, $87, $05, $87, $09, $85, $05, $22, $85,
    $09, $22, $87, $05, $87, $09, $87, $05, $87, $09, $85, $05, $22, $85, $09, $22,
    $87, $05, $83, $82, $09, $87, $05, $87, $09, $8D, $05, $22, $87, $05, $87, $08,
    $87, $05, $87, $08, $87, $05, $87, $08, $84, $05, $02, $25, $69, $08, $18, $05,
    $58, $45, $02, $25, $69, $08, $58, $65, $22, $25, $69, $02, $58, $87, $05, $84,
    $09, $58, $22, $45, $02, $25, $69, $08, $38, $02, $8A, $1D, $84, $01, $85, $11,
    $35, $14, $13, $16, $34, $13, $16, $14, $87, $15, $1B, $1C, $84, $01, $1D, $88,
    $15, $14, $13, $75, $30, $55, $19, $16, $12, $8A, $15, $19, $15, $10, $89, $15,
    $1D, $84, $01, $84, $02, $85, $01, $0F, $06, $22, $85, $0F, $59, $16, $12, $06,
    $89, $0F, $59, $16, $12, $02, $84, $06, $8A, $02, $98, $17, $96, $02, $8C, $0C,
    $6B, $0C, $4B, $8B, $0C, $8E, $02, $3A, $09, $7A, $09, $3A, $05, $49, $25, $49,
    $05, $1A, $09, $5A, $84, $09, $85, $1A, $82, $89, $1A, $69, $1A, $05, $69, $05,
    $67, $05, $3A, $25, $29, $84, $19, $09, $65, $1A, $19, $3A, $05, $1A, $8F, $0F,
    $A2, $0E, $09, $05, $6E, $0F, $85, $02, $84, $19, $84, $1A, $39, $7A, $19, $3A,
    $19, $3A, $19, $86, $1A, $19, $9E, $1A, $39, $3A, $81, $99, $1A, $9D, $1A, $8B,
    $59, $87, $1A, $79, $93, $1A, $39, $86, $1A, $16, $12, $D0, $1A, $19, $9D, $1A,
    $98, $19, $A7, $1A, $85, $19, $86, $1A, $96, $02, $A6, $1A, $98, $02, $8A, $1A,
    $94, $02, $BB, $0F, $CD, $1A, $95, $0F, $B6, $3A, $19, $88, $1A, $19, $B5, $1A,
    $87, $19, $EE, $1A, $19, $DD, $1A, $02, $92, $1A, $02, $1A, $62, $97, $1A, $02,
    $7A, $02, $7A, $22, $9B, $1A, $02, $A2, $1A, $81, $82, $1A, $5A, $42, $88, $1A,
    $22, $86, $1A, $86, $96, $12, $9D, $0F, $1A, $42, $97, $1A, $02, $8D, $1A, $02,
    $84, $19, $16, $12, $79, $02, $19, $42, $95, $19, $84, $96, $12, $8F, $19, $FF,
    $3A, $82, $39, $8A, $96, $12, $BE, $19, $81, $96, $12, $9F, $19, $16, $12, $81,
    $59, $AF, $1A, $94, $19, $3A, $85, $19, $42, $89, $1A, $A5, $22, $AE, $09, $02,
    $AE, $05, $02, $09, $05, $49, $25, $82, $89, $05, $69, $05, $09, $25, $09, $86,
    $05, $06, $49, $B0, $85, $09, $25, $85, $1A, $81, $89, $05, $4C, $86, $02, $75,
    $0F, $35, $A5, $05, $89, $02, $B5, $07, $88, $02, $06, $8F, $02, $96, $07, $88,
    $02, $86, $07, $02, $86, $07, $02, $86, $07, $02, $86, $07, $02, $86, $07, $02,
    $86, $07, $02, $86, $07, $02, $86, $07, $02, $9F, $0C, $35, $81, $94, $13, $55,
    $14, $13, $15, $14, $13, $88, $15, $11, $35, $11, $15, $14, $13, $35, $14, $13,
    $83, $96, $12, $84, $15, $06, $35, $CD, $02, $99, $1A, $02, $D8, $1A, $8B, $02,
    $D5, $3A, $99, $02, $8B, $1A, $62, $1D, $55, $1A, $06, $07, $0E, $84, $96, $12,
    $3A, $83, $96, $12, $11, $16, $32, $1A, $88, $0E, $85, $0C, $11, $84, $06, $3A,
    $4E, $06, $07, $15, $3A, $02, $D5, $07, $22, $2C, $38, $26, $07, $11, $D9, $07,
    $15, $46, $07, $84, $02, $A8, $07, $42, $DD, $07, $02, $3A, $6F, $89, $1A, $97,
    $07, $87, $02, $A3, $1A, $8B, $02, $8F, $07, $9E, $1A, $02, $89, $0F, $A6, $1A,
    $8E, $0F, $9F, $1A, $89, $0F, $A6, $1A, $8E, $0F, $BE, $1A, $02, $FF, $3A, $FF,
    $E7, $FF, $B5, $E7, $9F, $89, $02, $BF, $1A, $FF, $E7, $FF, $FF, $E7, $FF, $FF,
    $E7, $FF, $FF, $E7, $FF, $FF, $E7, $FF, $CB, $67, $B3, $02, $94, $07, $06, $F6,
    $87, $5F, $42, $B6, $1A, $88, $02, $A7, $07, $85, $06, $35, $8B, $47, $06, $55,
    $8F, $07, $89, $0D, $27, $93, $02, $8F, $89, $05, $22, $85, $89, $05, $07, $0C,
    $4B, $15, $87, $02, $2C, $15, $06, $8B, $89, $05, $87, $02, $C5, $07, $89, $0E,
    $2C, $85, $15, $87, $02, $96, $18, $88, $06, $38, $86, $89, $05, $25, $9E, $89,
    $05, $06, $87, $05, $81, $89, $05, $29, $83, $85, $09, $05, $06, $38, $09, $05,
    $ED, $02, $86, $07, $0C, $47, $0C, $67, $0C, $96, $07, $2A, $2C, $0A, $7A, $62,
    $85, $0F, $3A, $17, $1A, $85, $02, $B3, $07, $75, $87, $02, $2A, $B1, $07, $8F,
    $0A, $0C, $88, $02, $35, $89, $0D, $85, $02, $91, $0C, $85, $07, $55, $07, $62,
    $89, $0D, $9B, $07, $87, $0C, $35, $96, $07, $8A, $0C, $2A, $8A, $02, $15, $9C,
    $07, $42, $4C, $0A, $AE, $07, $0C, $2A, $6C, $2A, $0C, $6A, $8C, $15, $02, $06,
    $89, $0D, $62, $35, $9F, $02, $A8, $07, $85, $0C, $2A, $2C, $2A, $2C, $88, $02,
    $47, $0C, $87, $07, $0C, $0A, $22, $89, $0D, $22, $75, $8F, $07, $06, $85, $07,
    $5A, $07, $0A, $62, $AF, $07, $0C, $07, $4C, $27, $2C, $84, $07, $2C, $07, $0C,
    $07, $97, $02, $27, $06, $35, $DF, $22, $A2, $07, $2A, $0C, $2A, $0C, $2A, $15,
    $0A, $0C, $22, $89, $0D, $85, $02, $FF, $E7, $FF, $FF, $E7, $FF, $A3, $E7, $BF,
    $8B, $02, $96, $07, $62, $B0, $07, $62, $FF, $E4, $7F, $FF, $E3, $FF, $FF, $A3,
    $9F, $AD, $47, $22, $BD, $07, $22, $E9, $07, $A5, $02, $86, $05, $8B, $02, $84,
    $05, $84, $02, $07, $0C, $89, $07, $19, $8C, $07, $02, $84, $07, $81, $82, $07,
    $07, $02, $27, $02, $EB, $07, $A0, $02, $EA, $47, $16, $12, $8F, $02, $BF, $07,
    $22, $B5, $07, $A7, $02, $8B, $07, $17, $1A, $22, $8F, $0C, $86, $15, $16, $12,
    $15, $85, $02, $86, $0C, $88, $02, $15, $31, $30, $87, $96, $12, $35, $16, $12,
    $75, $50, $55, $02, $75, $11, $82, $96, $12, $55, $19, $11, $59, $02, $15, $17,
    $35, $62, $84, $07, $02, $86, $27, $22, $01, $02, $55, $17, $55, $16, $12, $15,
    $19, $15, $11, $35, $89, $0D, $35, $59, $35, $99, $09, $16, $15, $12, $18, $10,
    $18, $99, $05, $16, $19, $12, $19, $16, $12, $15, $16, $12, $35, $89, $07, $06,
    $AC, $07, $26, $9E, $07, $42, $85, $07, $22, $85, $07, $22, $85, $07, $22, $47,
    $42, $37, $19, $18, $1A, $37, $02, $1A, $79, $3A, $89, $02, $41, $3A, $22
  );

type
  TCharCategories = array[WideChar] of TUnicodeCategory;

var
  CharCategories: TCharCategories;

procedure ExtractUnicodeCategories(P: PByte; Len: Integer; var CharCategories: TCharCategories);
var
  RepeatCount: Integer;
  Cat, Cat2: TUnicodeCategory;
  PCat: ^TUnicodeCategory;
  Alternating: Boolean;
begin
  PCat := @CharCategories[#0];
  while Len > 0 do
  begin
    if P^ and $80 <> 0 then // large RepeatCount (4..511/4095)
    begin
      RepeatCount := P^ and $7F;
      Inc(P);
      Dec(Len);
      RepeatCount := RepeatCount or (((P^ and not $80) shr 5) shl 7);
      Alternating := P^ and $80 <> 0; // two alternating categories or really large RepeatCount (4..4095)
    end
    else // small RepeatCount (0..3)
    begin
      RepeatCount := P^ shr 5;
      Alternating := False;
    end;

    Cat := TUnicodeCategory(P^ and $1F);
    if Alternating then
    begin
      Inc(P);
      Dec(Len);
      Cat2 := TUnicodeCategory(P^ and $1F);
      if Cat2 = TUnicodeCategory($1F) then
      begin
        // really large RepeatCount
        Alternating := False;
        RepeatCount := RepeatCount or (Integer(P^ shr 5) shl 9);
      end
      else
      begin
        // two alternating categories
        while RepeatCount >= 0 do
        begin
          PCat^ := Cat;
          Inc(PCat);
          PCat^ := Cat2;
          Inc(PCat);
          Dec(RepeatCount);
        end;
      end;
    end;

    if not Alternating then
    begin
      while RepeatCount >= 0 do
      begin
        PCat^ := Cat;
        Inc(PCat);
        Dec(RepeatCount);
      end;
    end;
    Inc(P);
    Dec(Len);
  end;
end;

function GetCharCategory(C: Char): TUnicodeCategory;
begin
  if CharCategories['A'] = TUnicodeCategory.ucControl then
    ExtractUnicodeCategories(@UnicodeCategorieBytes[0], Length(UnicodeCategorieBytes), CharCategories);
  Result := CharCategories[C];
end;

{ TCharacter }

class function TCharacter.CheckLetter(uc: TUnicodeCategory): Boolean;
begin
  case uc of
    TUnicodeCategory.ucUppercaseLetter,
    TUnicodeCategory.ucLowercaseLetter,
    TUnicodeCategory.ucTitlecaseLetter,
    TUnicodeCategory.ucModifierLetter,
    TUnicodeCategory.ucOtherLetter:
      Result := True;
  else
    Result := False;
  end;
end;

class function TCharacter.CheckLetterOrDigit(uc: TUnicodeCategory): Boolean;
begin
  case uc of
    TUnicodeCategory.ucUppercaseLetter,
    TUnicodeCategory.ucLowercaseLetter,
    TUnicodeCategory.ucTitlecaseLetter,
    TUnicodeCategory.ucModifierLetter,
    TUnicodeCategory.ucOtherLetter,
    TUnicodeCategory.ucDecimalNumber:
      Result := True;
  else
    Result := False;
  end;
end;

class function TCharacter.CheckNumber(uc: TUnicodeCategory): Boolean;
begin
  case uc of
    TUnicodeCategory.ucOtherNumber,
    TUnicodeCategory.ucLetterNumber,
    TUnicodeCategory.ucDecimalNumber:
      Result := True;
  else
    Result := False;
  end;
end;

class function TCharacter.CheckPunctuation(uc: TUnicodeCategory): Boolean;
begin
  case uc of
    TUnicodeCategory.ucConnectPunctuation,
    TUnicodeCategory.ucDashPunctuation,
    TUnicodeCategory.ucClosePunctuation,
    TUnicodeCategory.ucFinalPunctuation,
    TUnicodeCategory.ucInitialPunctuation,
    TUnicodeCategory.ucOtherPunctuation,
    TUnicodeCategory.ucOpenPunctuation:
      Result := True;
  else
    Result := False;
  end;
end;

class function TCharacter.CheckSeparator(uc: TUnicodeCategory): Boolean;
begin
  case uc of
    TUnicodeCategory.ucLineSeparator,
    TUnicodeCategory.ucParagraphSeparator,
    TUnicodeCategory.ucSpaceSeparator:
      Result := True;
  else
    Result := False;
  end;
end;

class function TCharacter.CheckSymbol(uc: TUnicodeCategory): Boolean;
begin
  case uc of
    TUnicodeCategory.ucCurrencySymbol,
    TUnicodeCategory.ucModifierSymbol,
    TUnicodeCategory.ucMathSymbol,
    TUnicodeCategory.ucOtherSymbol:
      Result := True;
  else
    Result := False;
  end;
end;

class function TCharacter.IsLatin1(C: Char): Boolean;
begin
  Result := Word(C) <= $00FF;
end;

class function TCharacter.IsLetter(C: Char): Boolean;
begin
  Result := CheckLetter(GetCharCategory(C));
end;

class function TCharacter.IsLetter(const S: string; Index: Integer): Boolean;
begin
  Result := IsLetter(S[Index]);
end;

class function TCharacter.IsLetterOrDigit(const S: string; Index: Integer): Boolean;
begin
  Result := IsLetterOrDigit(S[Index]);
end;

class function TCharacter.IsLetterOrDigit(C: Char): Boolean;
begin
  Result := CheckLetterOrDigit(GetCharCategory(C));
end;

class function TCharacter.IsAscii(C: Char): Boolean;
begin
  Result := Word(C) < 128;
end;

class function TCharacter.IsControl(const S: string; Index: Integer): Boolean;
begin
  Result := IsControl(S[Index]);
end;

class function TCharacter.IsControl(C: Char): Boolean;
begin
  Result := GetCharCategory(C) = TUnicodeCategory.ucControl;
end;

class function TCharacter.IsDigit(C: Char): Boolean;
begin
  Result := GetCharCategory(C) = TUnicodeCategory.ucDecimalNumber;
end;

class function TCharacter.IsDigit(const S: string; Index: Integer): Boolean;
begin
  Result := IsDigit(S[Index]);
end;

class function TCharacter.IsHighSurrogate(C: Char): Boolean;
begin
  Result := (Word(C) >= $D800) and (Word(C) <= $DBFF);
end;

class function TCharacter.IsLowSurrogate(C: Char): Boolean;
begin
  Result := (Word(C) >= $DC00) and (Word(C) <= $DFFF);
end;

class function TCharacter.IsSurrogate(Surrogate: Char): Boolean;
begin
  Result := (Word(Surrogate) >= $D800) and (Word(Surrogate) <= $DFFF);
end;

class function TCharacter.IsSurrogatePair(const HighSurrogate, LowSurrogate: Char): Boolean;
begin
  Result := IsHighSurrogate(HighSurrogate) and IsLowSurrogate(LowSurrogate);
end;

class function TCharacter.GetUnicodeCategory(C: Char): TUnicodeCategory;
begin
  Result := GetCharCategory(C);
end;

class function TCharacter.ConvertToUtf32(const S: string; Index: Integer; out CharLength: Integer): UCS4Char;
var
  LowSurrogate, HighSurrogate: LongWord;
begin
  CharLength := 1;
  HighSurrogate := LongWord(S[Index]) - $D800;
  if HighSurrogate > $7FF then
    Result := UCS4Char(S[Index])
  else
  begin
    CharLength := 2;
    LowSurrogate := LongWord(S[Index + 1]) - $DC00;
    Result := (HighSurrogate shl 10) + LowSurrogate + $10000;
  end;
end;

class function TCharacter.ConvertToUtf32(const S: string; Index: Integer): UCS4Char;
var
  CharLength: Integer;
begin
  Result := ConvertToUtf32(S, Index, CharLength);
end;

class function TCharacter.ConvertFromUtf32(C: UCS4Char): string;
begin
  if C < $10000 then
    Result := Char(C)
  else
  begin
    Dec(C, $10000);
    Result := Char(C shr 10 + $D800) + Char(C and $3FF + $DC00);
  end;
end;

class function TCharacter.ConvertToUtf32(const HighSurrogate, LowSurrogate: Char): UCS4Char;
begin
  Result := ((LongWord(HighSurrogate) - $D800) shl 10) + (LongWord(LowSurrogate) - $DC00) + $10000;
end;

constructor TCharacter.Create;
begin
end;

class procedure TCharacter.Initialize;
begin
end;

class function TCharacter.GetNumericValue(C: Char): Double;
begin
  case C of
    '0'..'9':
      Result := Ord(C) - Ord('0');
  else
    Result := -1;
  end;
end;

class function TCharacter.GetNumericValue(const S: string; Index: Integer): Double;
begin
  Result := GetNumericValue(S[Index]);
end;

class function TCharacter.GetUnicodeCategory(const S: string; Index: Integer): TUnicodeCategory;
begin
  Result := GetUnicodeCategory(S[Index]);
end;

class function TCharacter.IsHighSurrogate(const S: string; Index: Integer): Boolean;
begin
  Result := IsHighSurrogate(S[Index]);
end;

class function TCharacter.IsLower(C: Char): Boolean;
begin
  Result := GetCharCategory(C) = TUnicodeCategory.ucLowercaseLetter;
end;

class function TCharacter.IsLower(const S: string; Index: Integer): Boolean;
begin
  Result := IsLower(S[Index]);
end;

class function TCharacter.IsLowSurrogate(const S: string; Index: Integer): Boolean;
begin
  Result := IsLowSurrogate(S[Index]);
end;

class function TCharacter.IsNumber(C: Char): Boolean;
begin
  Result := CheckNumber(GetCharCategory(C));
end;

class function TCharacter.IsNumber(const S: string; Index: Integer): Boolean;
begin
  Result := IsNumber(S[Index]);
end;

class function TCharacter.IsPunctuation(const S: string; Index: Integer): Boolean;
begin
  Result := IsPunctuation(S[Index]);
end;

class function TCharacter.IsPunctuation(C: Char): Boolean;
begin
  Result := CheckPunctuation(GetCharCategory(C));
end;

class function TCharacter.IsSeparator(C: Char): Boolean;
begin
  Result := CheckSeparator(GetCharCategory(C));
end;

class function TCharacter.IsSeparator(const S: string; Index: Integer): Boolean;
begin
  Result := IsSeparator(S[Index]);
end;

class function TCharacter.IsSurrogate(const S: string; Index: Integer): Boolean;
begin
  Result := IsSurrogate(S[Index]);
end;

class function TCharacter.IsSurrogatePair(const S: string; Index: Integer): Boolean;
begin
  Result := (Index < Length(S) + 1) and IsSurrogatePair(S[Index], S[Index + 1]);
end;

class function TCharacter.IsSymbol(C: Char): Boolean;
begin
  Result := CheckSymbol(GetCharCategory(C));
end;

class function TCharacter.IsSymbol(const S: string; Index: Integer): Boolean;
begin
  Result := IsSymbol(S[Index]);
end;

class function TCharacter.IsUpper(const S: string; Index: Integer): Boolean;
begin
  Result := IsUpper(S[Index]);
end;

class function TCharacter.IsUpper(C: Char): Boolean;
begin
  Result := GetCharCategory(C) = TUnicodeCategory.ucUppercaseLetter;
end;

class function TCharacter.IsWhiteSpace(const S: string; Index: Integer): Boolean;
begin
  Result := IsWhiteSpace(S[Index]);
end;

class function TCharacter.ToLower(C: Char): Char;
begin
  Result := Char(Word(CharLower(PChar(Word(C)))));
end;

class function TCharacter.ToLower(const S: string): string;
begin
  Result := AnsiLowerCase(S);
end;

class function TCharacter.ToUpper(C: Char): Char;
begin
  Result := Char(Word(CharUpper(PChar(Word(C)))));
end;

class function TCharacter.ToUpper(const S: string): string;
begin
  Result := AnsiUpperCase(S);
end;

class function TCharacter.IsWhiteSpace(C: Char): Boolean;
begin
  if C <= #127 then
  begin
    case C of
      #$0009..#$000D, ' ', #$0085, #$00A0:
        Result := True;
    else
      Result := False;
    end;
  end
  else
    Result := CheckSeparator(GetCharCategory(C));
end;

end.
