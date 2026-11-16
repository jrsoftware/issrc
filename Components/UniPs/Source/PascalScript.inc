{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{                                                                            }
{ compiler: Delphi 2 and up, Kylix 3 and up                                  }
{ platform: Win32, Linux                                                     }
{                                                                            }
{ (c)opyright RemObjects Software. all rights reserved.                      }
{                                                                            }
{----------------------------------------------------------------------------}


{$INCLUDE eDefines.inc}

{$DEFINE PS_HAVEVARIANT}
{$DEFINE PS_DYNARRAY}

{$B-}{$X+}{$T-}{$H+}

{$R-}{$Q-}


{
Defines:
  PS_NOSMARTLIST    - Don't use the smart list option
  PS_NOIDISPATCH
  PS_NOWIDESTRING
  PS_NOINT64
  PS_DELPHIDIV
}

{$UNDEF DEBUG}

{$DEFINE TExtended80Rec_present}

{$WARN IMPLICIT_INTEGER_CAST_LOSS OFF}
{$WARN IMPLICIT_CONVERSION_LOSS OFF}
{$WARN COMBINING_SIGNED_UNSIGNED64 OFF}