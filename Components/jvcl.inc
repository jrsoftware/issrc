{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Minimal jvcl.inc for use of the extracted JvInspector.pas and JvAutoComplete.pas
  in Inno Setup, replacing the original jvcl.inc.
}

{$TYPEDADDRESS OFF}

{$WARN IMPLICIT_INTEGER_CAST_LOSS OFF}
{$WARN IMPLICIT_CONVERSION_LOSS OFF}

{$DEFINE RTL230_UP}
{$IF CompilerVersion >= 36.0}
{$DEFINE RTL360_UP}
{$IFEND}
{$DEFINE HAS_UNIT_SYSTEM_UITYPES}
{$DEFINE JVCLThemesEnabled}
{$DEFINE SUPPORTS_UNICODE}
