{----------------------------------------------------------------------------}
{file:      eDefines.inc                                                     }
{type:      Delphi include file                                              }
{                                                                            }
{compiler:  Borland Pascal 7,                                                }
{           Delphi 1-7, 2005-2007 for Win32                                  }
{           Kylix 1-3,                                                       }
{           C++Builder 1-6, 2006-2007                                        }
{           Free Pascal Compiler 2.x                                         }
{                                                                            }
{platforms: DOS, DPMI, Win16, Win32, Win64, Linux, Mac OS X                  }
{                                                                            }
{author:    mh@elitedev.com                                                  }
{                                                                            }
{contents:  Defines that can be flexibily used to determine the exact        }
{           compiler version used.                                           }
{                                                                            }
{(c)opyright elitedevelopments software. all rights reserved.                }
{            http://www.elitedev.com                                         }
{                                                                            }
{ Third Party component developers are encouraged to use the set of defines  }
{ established in this file, rather then their own system, for checking their }
{ component libraries agains different versions of Delphi and C++Builder.    }
{                                                                            }
{ This file may be distributed freely with both free and commercial source   }
{ libraries, but you are asked to please leave this comment in place, and    }
{ to return any improvements you make to this file to the maintainer that    }
{ is noted above.                                                            }
{----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ Compiler and OS version defines:                                           }
{                                                                            }
{ exact compiler versions:                                                   }
{                                                                            }
{ BP7             Borland Pascal 7.0                                         }
{ DELPHI1         Delphi 1.0 (any Delphi)                                    }
{ DELPHI2         Delphi 2.0                                                 }
{ DELPHI3         Delphi 3.0                                                 }
{ DELPHI4         Delphi 4.0                                                 }
{ DELPHI5         Delphi 5.0                                                 }
{ DELPHI6         Delphi 6.0                                                 }
{ DELPHI7         Delphi 7.0                                                 }
{ DELPHI9         Delphi 2005                                                }
{ DELPHI2005      Delphi 2005                                                }
{ DELPHI2006      Delphi 2006                                                }
{ DELPHI2007      Delphi 2007                                                }
{ KYLIX1          Kylix 1.0                                                  }
{ KYLIX2          Kylix 2.0                                                  }
{ KYLIX3          Kylix 3.0                                                  }
{ CBUILDER1       C++Builder 1.0                                             }
{ CBUILDER3       C++Builder 3.0                                             }
{ CBUILDER4       C++Builder 4.0                                             }
{ CBUILDER5       C++Builder 5.0                                             }
{                                                                            }
{                                                                            }
{ minimum compiler versions:                                                 }
{                                                                            }
{ DELPHI1UP       Delphi 1.0 and above (any Delphi)                          }
{ DELPHI2UP       Delphi 2.0 and above                                       }
{ DELPHI3UP       Delphi 3.0 and above                                       }
{ DELPHI4UP       Delphi 4.0 and above                                       }
{ DELPHI5UP       Delphi 5.0 and above                                       }
{ DELPHI6UP       Delphi 6.0 and above                                       }
{ DELPHI7UP       Delphi 7.0 and above                                       }
{ DELPHI9UP       Delphi 9.0 (2005) and above                                }
{ DELPHI10UP      Delphi 10.0 (2006) and above                               }
{ DELPHI11UP      Delphi 11.0 (2007) and above                               }
{ DELPHI2005UP    Delphi 2005 and above                                      }
{ DELPHI2006UP    Delphi 2006 and above                                      }
{ DELPHI2007UP    Delphi 2007 and above                                      }
{ KYLIX1UP        Kylix 1.0 and above (any Kylix)                            }
{ KYLIX2UP        Kylix 2.0 and above (any Kylix)                            }
{ KYLIX3UP        Kylix 3.0 and above (any Kylix)                            }
{ CBUILDER1UP     C++Builder 1.0 and above or Delphi 2 and above             }
{ CBUILDER3UP     C++Builder 3.0 and above or Delphi 3.0 and above           }
{ CBUILDER4UP     C++Builder 4.0 and above or Delphi 4.0 and above           }
{ CBUILDER5UP     C++Builder 5.0 and above or Delphi 5.0 and above           }
{ CBUILDER6UP     C++Builder 5.0 and above or Delphi 5.0 and above           }
{                                                                            }
{                                                                            }
{ compiler types:                                                            }
{                                                                            }
{ BP              Borland Pascal (not Delphi or C++Builder)                  }
{ DELPHI          any Delphi version (but not C++Builder or Kylix)           }
{ KYLIX           any Kylix version (not Delphi or C++Builder for Windows)   }
{ CBUILDER        any C++Builder for Windows (Pascal)                        }
{                                                                            }
{                                                                            }
{ target platforms compiler types:                                           }
{                                                                            }
{ DELPHI_16BIT    16bit Delphi (but not C++Builder!)                         }
{ DELPHI_32BIT    32bit Delphi (but not C++Builder)                          }
{ KYLIX_32BIT     32bit Kylix (but not C++Builder)                           }
{ CBUILDER_32BIT  32bit C++Builer's Pascal (but not Delphi)                  }
{                                                                            }
{                                                                            }
{ target cpu types                                                           }
{                                                                            }
{ CPU16           16bit Delphi or Borland Pascal                             }
{ CPU32           32bit Delphi or Free Pascal                                }
{ CPU64           64bit Free Pascal                                          }
{                                                                            }
{ target platforms                                                           }
{                                                                            }
{ DOS             any DOS (plain and DPMI)                                   }
{ REALMODE        16bit realmode DOS                                         }
{ PROTECTEDMODE   16bit DPMI DOS                                             }
{                                                                            }
{ MSWINDOWS       any Windows platform                                       }
{ WIN16           16bit Windows                                              }
{ WIN32           32bit Windows                                              }
{ WIN64           64bit Windows                                              }
{ DOTNET          .NET                                                       }
{                                                                            }
{ LINUX           any Linux platform                                         }
{ LINUX32         32bit Linux                                                }
{ LINUX64         64bit Linux                                                }
{                                                                            }
{ DARWIN          Any Mac OS X                                               }
{ DARWIN32        32bit Mac OS X                                             }
{ DARWIN64        64bit Mac OS X                                             }
{----------------------------------------------------------------------------}

{ defines for Delphi XE16 (Delphi 13) }
{$IFDEF VER370}
  {$DEFINE BDS37}{$DEFINE DELPHIXE16}{$DEFINE DELPHI37}
  {$DEFINE DELPHI37UP}
{$ENDIF}

{$IFDEF DELPHI37UP}
  {$DEFINE DELPHI29UP}
  {$DEFINE BDS37UP}{$DEFINE DELPHIXE16UP}
{$ENDIF}

{ defines for Delphi XE15 (Delphi 12) }
{$IFDEF VER360}
  {$DEFINE BDS23}{$DEFINE DELPHIXE15}{$DEFINE DELPHI_YUKON}{$DEFINE DELPHI29}
  {$DEFINE DELPHI29UP}
{$ENDIF}

{$IFDEF DELPHI29UP}
  {$DEFINE DELPHI28UP}
  {$DEFINE BDS23UP}{$DEFINE DELPHIXE15UP}{$DEFINE DELPHI_YUKON_UP}
{$ENDIF}

{ defines for Delphi XE14 (Delphi 11 Alexandria) }
{$IFDEF VER350}
  {$DEFINE BDS22}{$DEFINE DELPHIXE14}{$DEFINE DELPHI_ALEXANDRIA}{$DEFINE DELPHI28}
  {$DEFINE DELPHI28UP}
{$ENDIF}

{$IFDEF DELPHI28UP}
  {$DEFINE DELPHI27UP}
  {$DEFINE BDS22UP}{$DEFINE DELPHIXE14UP}{$DEFINE DELPHI_ALEXANDRIA_UP}
{$ENDIF}

{ defines for Delphi XE13 (Delphi 10.4 Sydney) }
{$IFDEF VER340}
  {$DEFINE BDS21}{$DEFINE DELPHIXE13}{$DEFINE DELPHI_SYDNEY}{$DEFINE DELPHI27}
  {$DEFINE DELPHI27UP}
{$ENDIF}

{$IFDEF DELPHI27UP}
  {$DEFINE DELPHI26UP}
  {$DEFINE BDS21UP}{$DEFINE DELPHIXE13UP}{$DEFINE DELPHI_SYDNEY_UP}
{$ENDIF}

{ defines for Delphi XE12 (Delphi 10.3 Rio) }
{$IFDEF VER330}
  {$DEFINE BDS20}{$DEFINE DELPHIXE12}{$DEFINE DELPHI_RIO}{$DEFINE DELPHI26}
  {$DEFINE DELPHI26UP}
{$ENDIF}

{$IFDEF DELPHI26UP}
  {$DEFINE DELPHI25UP}
  {$DEFINE BDS20UP}{$DEFINE DELPHIXE12UP}{$DEFINE DELPHI_RIO_UP}
{$ENDIF}

{ defines for Delphi XE11 (Delphi 10.2 Tokyo) }
{$IFDEF VER320}
  {$DEFINE BDS19}{$DEFINE DELPHIXE11}{$DEFINE DELPHI_TOKYO}{$DEFINE DELPHI25}
  {$DEFINE DELPHI25UP}
{$ENDIF}

{$IFDEF DELPHI25UP}
  {$DEFINE DELPHI24UP}
  {$DEFINE BDS19UP}{$DEFINE DELPHIXE11UP}{$DEFINE DELPHI_TOKYO_UP}
{$ENDIF}

{ defines for Delphi XE10 (10.1 Berlin) }
{$IFDEF VER310}
  {$DEFINE BDS18}{$DEFINE DELPHIXE10}{$DEFINE DELPHI_BERLIN}{$DEFINE DELPHI24}
  {$DEFINE DELPHI24UP}
{$ENDIF}

{$IFDEF DELPHI24UP}
  {$DEFINE DELPHI23UP}
  {$DEFINE BDS18UP}{$DEFINE DELPHIXE10UP}{$DEFINE DELPHI_BERLIN_UP}
{$ENDIF}

{ defines for Delphi XE9 (Delphi 10 Seattle) }
{$IFDEF VER300}
  {$DEFINE BDS17}{$DEFINE DELPHIXE9}{$DEFINE DELPHI_SEATTLE}{$DEFINE DELPHI23}
  {$DEFINE DELPHI23UP}
{$ENDIF}

{$IFDEF DELPHI23UP}
  {$DEFINE DELPHI22UP}
  {$DEFINE BDS17UP}{$DEFINE DELPHIXE9UP}{$DEFINE DELPHI_SEATTLE_UP}
{$ENDIF}

{ defines for Delphi XE8 }
{$IFDEF VER290}
  {$DEFINE BDS16}{$DEFINE DELPHIXE8}{$DEFINE DELPHI22}
  {$DEFINE DELPHI22UP}
{$ENDIF}

{$IFDEF DELPHI22UP}
  {$DEFINE DELPHI21UP}
  {$DEFINE BDS16UP}{$DEFINE DELPHIXE8UP}
{$ENDIF}

{ defines for Delphi XE7 }
{$IFDEF VER280}
  {$DEFINE BDS15}{$DEFINE DELPHIXE7}{$DEFINE DELPHI21}
  {$DEFINE DELPHI21UP}
{$ENDIF}

{$IFDEF DELPHI21UP}
  {$DEFINE DELPHI20UP}
  {$DEFINE BDS15UP}{$DEFINE DELPHIXE7UP}
{$ENDIF}

{ defines for Delphi XE6 }
{$IFDEF VER270}
  {$DEFINE BDS14}{$DEFINE DELPHIXE6}{$DEFINE DELPHI20}
  {$DEFINE DELPHI20UP}
{$ENDIF}

{$IFDEF DELPHI20UP}
  {$DEFINE DELPHI19UP}
  {$DEFINE BDS14UP}{$DEFINE DELPHIXE6UP}
{$ENDIF}

{ defines for Delphi XE5 }
{$IFDEF VER260}
  {$DEFINE BDS12}{$DEFINE DELPHIXE5}{$DEFINE DELPHI19}
  {$DEFINE DELPHI19UP}
{$ENDIF}

{$IFDEF DELPHI19UP}
  {$DEFINE DELPHI18UP}
  {$DEFINE BDS12UP}{$DEFINE DELPHIXE5UP}
{$ENDIF}

{ defines for Delphi XE4 }
{$IFDEF VER250}
  {$DEFINE BDS11}{$DEFINE DELPHIXE4}{$DEFINE DELPHI18}
  {$DEFINE DELPHI18UP}
{$ENDIF}

{$IFDEF DELPHI18UP}
  {$DEFINE DELPHI17UP}
  {$DEFINE BDS11UP}{$DEFINE DELPHIXE4UP}
{$ENDIF}

{ defines for Delphi XE3 }
{$IFDEF VER240}
  {$DEFINE BDS10}{$DEFINE DELPHIXE3}{$DEFINE DELPHI17}
  {$DEFINE DELPHI17UP}
{$ENDIF}

{$IFDEF DELPHI17UP}
  {$DEFINE DELPHI16UP}
  {$DEFINE BDS10UP}{$DEFINE DELPHIXE3UP}
{$ENDIF}

{ defines for Delphi XE2 }
{$IFDEF VER230}
  {$DEFINE BDS9}{$DEFINE DELPHIXE2}{$DEFINE DELPHI16}
  {$DEFINE DELPHI16UP}
{$ENDIF}

{$IFDEF DELPHI16UP}
  {$DEFINE DELPHI15UP}
  {$DEFINE BDS9UP}{$DEFINE DELPHIXE2UP}
  {$IFDEF CPUX64}
    {$DEFINE 64BIT}
    {$DEFINE CPU64}
  {$ELSE}
    {$DEFINE 32BIT}
    {$DEFINE CPU32}
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
  { defines for Delphi XE }
  {$IFDEF VER220}
    {$DEFINE BDS8}{$DEFINE DELPHIXE}{$DEFINE DELPHI15}
    {$DEFINE DELPHI15UP}
    {$DEFINE 32BIT}
    {$DEFINE CPU32}
  {$ENDIF}

  { defines for Delphi 2010 }
  {$IFDEF VER210}
    {$DEFINE BDS7}{$DEFINE DELPHI2010}{$DEFINE DELPHI14}
    {$DEFINE DELPHI14UP}
  {$ENDIF}

  { defines for Delphi 2009 }
  {$IFDEF VER200}
    {$DEFINE BDS6}{$DEFINE DELPHI2009}{$DEFINE DELPHI12}
    {$DEFINE DELPHI12UP}
  {$ENDIF}

  { defines for Delphi 2007 }
  {$IFDEF VER185}
    {$DEFINE BDS5}{$DEFINE DELPHI2007}{$DEFINE DELPHI11}
    {$DEFINE DELPHI11UP}
  {$ENDIF}

  { defines for Delphi 2006 }
  {$IFDEF VER180}
    {$IFNDEF VER185}
      {$DEFINE BDS4}{$DEFINE DELPHI2006}{$DEFINE DELPHI10}
      {$DEFINE DELPHI10UP}
    {$ENDIF}
  {$ENDIF}

  { defines for Delphi 2005 }
  {$IFDEF VER170}
    {$DEFINE BDS3}{$DEFINE DELPHI2005}{$DEFINE DELPHI9}
    {$DEFINE DELPHI9UP}
  {$ENDIF}

  { defines for Delphi 7.0 }
  {$IFDEF VER150}
    {$DEFINE DELPHI7}{$DEFINE DELPHI7UP}
  {$ENDIF}

  { defines for Delphi 6.0 }
  {$IFDEF VER140}
    {$DEFINE DELPHI6}{$DEFINE DELPHI6UP}
  {$ENDIF}

  { defines for Delphi 5.0 }
  {$IFDEF VER130}
    {$DEFINE DELPHI5}{$DEFINE DELPHI5UP}
  {$ENDIF}

  { defines for Delphi 4.0 }
  {$IFDEF VER120}
    {$DEFINE DELPHI4}{$DEFINE DELPHI4UP}
  {$ENDIF}

  { defines for Delphi 3.0 }
  {$IFDEF VER100}
    {$DEFINE DELPHI3}{$DEFINE DELPHI3UP}
  {$ENDIF}

  { defines for Delphi 2.0 }
  {$IFDEF VER90}
    {$DEFINE DELPHI2}{$DEFINE DELPHI2UP}
  {$ENDIF}

  { defines for Delphi 1.0 }
  {$IFDEF VER80}
    {$DEFINE DELPHI1}{$DEFINE DELPHI1UP}
    {$DEFINE DELPHI_16BIT}{$DEFINE WIN16}
    {$DEFINE 16BIT}{$DEFINE CPU16}
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHI15UP}
  {$DEFINE DELPHI14UP}
  {$DEFINE BDS8UP}{$DEFINE DELPHIXEUP}
{$ENDIF}

{$IFDEF DELPHI14UP}
  {$DEFINE DELPHI12UP}
  {$DEFINE BDS7UP}{$DEFINE DELPHI2010UP}
{$ENDIF}

{$IFDEF DELPHI12UP}
  {$DEFINE DELPHI11UP}
  {$DEFINE BDS6UP}{$DEFINE DELPHI2009UP}
{$ENDIF}

{$IFDEF DELPHI11UP}
  {$DEFINE DELPHI10UP}
  {$DEFINE BDS5UP}{$DEFINE DELPHI2007UP}
{$ENDIF}

{$IFDEF DELPHI10UP}
  {$DEFINE DELPHI9UP}
  {$DEFINE BDS4UP}{$DEFINE DELPHI2006UP}
{$ENDIF}

{$IFDEF DELPHI9UP}
  {$DEFINE DELPHI7UP}
  {$DEFINE BDS3UP}{$DEFINE DELPHI2005UP}
  {$DEFINE BDS}
{$ENDIF}

{$IFDEF DELPHI7UP}
  {$DEFINE DELPHI6UP}
{$ENDIF}

{$IFDEF DELPHI6UP}
  {$DEFINE DELPHI5UP}
  {$DEFINE VER140UP}
{$ENDIF}

{$IFDEF DELPHI5UP}
  {$DEFINE DELPHI4UP}
{$ENDIF}

{$IFDEF DELPHI4UP}
  {$DEFINE DELPHI3UP}
{$ENDIF}

{$IFDEF DELPHI3UP}
  {$DEFINE DELPHI2UP}
{$ENDIF}

{$IFDEF DELPHI2UP}
  {$DEFINE DELPHI1UP}
{$ENDIF}

{$IFDEF DELPHI1UP}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
  {$DEFINE 32BIT}
  {$DEFINE CPU32}
{$ENDIF}

{ defines for Borland Pascal 7.0 }
{$IFDEF VER70}
  {$DEFINE BP}
  {$DEFINE BP7}
  {$DEFINE 16BIT}
  {$DEFINE CPU16}

  { defines for BP7 DOS real mode }
  {$IFDEF MSDOS}
    {$DEFINE DOS}
    {$DEFINE REALMODE}
  {$ENDIF}

  { defines for BP7 DOS protected mode }
  {$IFDEF DPMI}
    {$DEFINE DOS}
    {$DEFINE PROTECTEDMODE}
  {$ENDIF}

  { defines for BP7 Windows }
  {$IFDEF WINDOWS}
    {$DEFINE MSWINDOWS}
    {$DEFINE WIN16}
  {$ENDIF}
{$ENDIF}
{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
{$ENDIF}
{ defines for Delphi 1.0 thru 7.0 }
{$IFDEF MSWINDOWS}
  { defines for C++Builder 1.0 }
  {$IFDEF VER93}
    {$DEFINE DELPHI1UP}
    {$DEFINE DELPHI2UP}
    {$DEFINE CBUILDER}
    {$DEFINE CBUILDER1}
    {$DEFINE CBUILDER1UP}
  {$ENDIF}

  { defines for C++Builder 3.0 }
  {$IFDEF VER110}
    {$DEFINE DELPHI1UP}
    {$DEFINE DELPHI2UP}
    {$DEFINE DELPHI3UP}
    {$DEFINE CBUILDER}
    {$DEFINE CBUILDER3}
    {$DEFINE CBUILDER1UP}
    {$DEFINE CBUILDER3UP}
  {$ENDIF}

  { defines for C++Builder 4.0 }
  {$IFDEF VER125}
    {$DEFINE DELPHI1UP}
    {$DEFINE DELPHI2UP}
    {$DEFINE DELPHI3UP}
    {$DEFINE DELPHI4UP}
    {$DEFINE CBUILDER}
    {$DEFINE CBUILDER4}
    {$DEFINE CBUILDER1UP}
    {$DEFINE CBUILDER3UP}
    {$DEFINE CBUILDER4UP}
  {$ENDIF}

  { defines for C++Builder 5.0 }
  {$IFDEF VER135}
    {$DEFINE DELPHI1UP}
    {$DEFINE DELPHI2UP}
    {$DEFINE DELPHI3UP}
    {$DEFINE DELPHI4UP}
    {$DEFINE DELPHI5UP}
    {$DEFINE CBUILDER}
    {$DEFINE CBUILDER5}
    {$DEFINE CBUILDER1UP}
    {$DEFINE CBUILDER3UP}
    {$DEFINE CBUILDER4UP}
    {$DEFINE CBUILDER5UP}
  {$ENDIF}

  {$IFDEF WIN32}
    {$DEFINE MSWINDOWS} //not automatically defined for Delphi 2 thru 5
    {$DEFINE 32BIT}
    {$DEFINE CPU32}
  {$ENDIF}
{$ENDIF MSWINDOWS}

{ defines for "Delphi for .NET" }
{$IFDEF CLR}
  {$DEFINE DOTNET}
{$ENDIF}

{$IFDEF DELPHI}
  {$IFNDEF DELPHIXE2UP}
    {$IFDEF DELPHI2UP}
      {$DEFINE DELPHI_32BIT}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF CBUILDER}
  {$IFNDEF DELPHIXE2UP}
    {$DEFINE CBUILDER_32BIT}
  {$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
  {$IFNDEF DelphiXE11UP}
    { Kylix 1.0 thru 3.0 }
    {$IFDEF LINUX}

      {$DEFINE VER140UP}

      { Any Kylix }
      {$DEFINE 32BIT}
      {$DEFINE LINUX32}
      {$DEFINE KYLIX_32BIT}
      {$DEFINE KYLIX}
      {$DEFINE KYLIX1UP}

      {$IFDEF CONDITIONALEXPRESSIONS}
        {$IF Declared(CompilerVersion)}

           { Kylix 2.0 }
           {$IF Declared(RTLVersion) and (RTLVersion = 14.1)}
             {$DEFINE KYLIX2}
             {$DEFINE KYLIX1UP}
             {$DEFINE KYLIX2UP}
           {$IFEND}

           { Kylix 3.0 - Delphi portion }
           {$IF Declared(RTLVersion) and (RTLVersion = 14.5)}
             {$DEFINE KYLIX3}
             {$DEFINE KYLIX1UP}
             {$DEFINE KYLIX2UP}
             {$DEFINE KYLIX3UP}
           {$IFEND}

        { Kylix 1.0 }
        {$ELSE}
          {$DEFINE KYLIX1}
        {$IFEND}
      {$ENDIF CONDITIONALEXPRESSIONS}
    {$ENDIF LINUX}
  {$ENDIF DelphiXE11UP}
{$ENDIF}

{ CPU }

{$IFDEF FPC}
  {$IFDEF MSWINDOWS}
    {$IFDEF CPU64}
      {$DEFINE WIN64}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF LINUX}
    {$DEFINE FPC_LINUX}
    {$IFDEF CPU32}
      {$DEFINE LINUX32}
    {$ENDIF}
    {$IFDEF CPU64}
      {$DEFINE LINUX64}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF DARWIN}
    {$DEFINE FPC_OSX}
    {$IFDEF CPU32}
      {$DEFINE DARWIN32}
    {$ENDIF}
    {$IFDEF CPU64}
      {$DEFINE DARWIN64}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF FPC_WIDESTRING_EQUAL_UNICODESTRING}
    {$DEFINE WIDESTRING_EQUAL_UNICODESTRING}
  {$ENDIF}
  {$DEFINE UNICODE_SUPPORT}
{$ELSE}
  {$DEFINE ENDIAN_LITTLE} // To have endianness symbol defined in Delphi too
{$ENDIF}

{$IFDEF DELPHI}
  {$DEFINE DELPHI_or_MSWINDOWS}
  {$IFNDEF UNICODE}
    {$DEFINE WIDESTRING_EQUAL_UNICODESTRING}
  {$ELSE}
    {$DEFINE UNICODE_SUPPORT}
  {$ENDIF}
  {$IFDEF NEXTGEN}
    {$DEFINE WIDESTRING_EQUAL_UNICODESTRING}
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$DEFINE DELPHI_or_MSWINDOWS}
{$ENDIF}

{$IFDEF DELPHI}
  {$IFDEF MACOS}
    {$DEFINE DELPHI_OSX}
    {$IFDEF CPUX64}
      {$DEFINE DELPHI_OSX64}
    {$ELSE}
      {$DEFINE DELPHI_OSX32}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF ANDROID}
    {$DEFINE DELPHI_ANDROID}
    {$IFDEF CPUX64}
      {$DEFINE DELPHI_ANDROID64}
    {$ELSE}
      {$DEFINE DELPHI_ANDROID32}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF LINUX}
    {$DEFINE DELPHI_LINUX}
    {$IFDEF LINUX32}
      {$DEFINE DELPHI_LINUX32}
    {$ENDIF}
    {$IFDEF LINUX64}
      {$DEFINE DELPHI_LINUX64}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF NEXTGEN}
  {$DEFINE NEXTGEN_or_DELPHILINUX}
{$ENDIF}
{$IFDEF DELPHI_LINUX}
  {$DEFINE NEXTGEN_or_DELPHILINUX}
{$ENDIF}

{$IFDEF NEXTGEN_or_DELPHILINUX}
  {$DEFINE FPC_SAFECALL_BUG}          // nextgen&linux compiler causes bug with safecall declarations
{$ENDIF}

{$IFDEF DELPHI2005UP}
  {$DEFINE INLINE_SUPPORT}
{$ENDIF}
{$IFDEF FPC}
  {$DEFINE INLINE_SUPPORT}
{$ENDIF}