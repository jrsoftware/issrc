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

{$IFDEF FPC}{$MODE DELPHI}{$H+}{$ENDIF}

{$IFDEF VER125}{C4}{$B-}{$X+}{$T-}{$H+}{$ENDIF}
{$IFDEF VER110}{C3}{$B-}{$X+}{$T-}{$H+}{$ENDIF}
{$IFDEF VER93}{C1}{$B-}{$X+}{$T-}{$H+}{$ENDIF}

{$IFDEF DELPHI4UP}
  {$DEFINE PS_HAVEVARIANT}
  {$DEFINE PS_DYNARRAY}
{$ENDIF}

{$IFNDEF FPC}
  {$B-}{$X+}{$T-}{$H+}
{$ELSE}
  {$R-}{$Q-}
{$ENDIF}

{$IFNDEF FPC}
{$IFNDEF DELPHI4UP}
{$IFNDEF LINUX}
  {$DEFINE PS_NOINT64}
{$ENDIF}
{$ENDIF}

{$IFDEF DELPHI2}
  {$DEFINE PS_NOINT64}
  {$DEFINE PS_NOWIDESTRING}
  {$B-}{$X+}{$T-}{$H+}
{$ENDIF}

{$IFDEF LINUX}{KYLIX}{$DEFINE CLX}{$DEFINE DELPHI3UP}{$DEFINE DELPHI6UP}{$ENDIF}
{$ENDIF}
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

{$IFDEF CLX}
{$DEFINE PS_NOIDISPATCH} // not implemented
{$ENDIF}

{$IFDEF FPC}
  {$I PascalScriptFPC.inc}
{$ENDIF}
