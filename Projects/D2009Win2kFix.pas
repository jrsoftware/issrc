unit D2009Win2kFix;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  When Windows 2000 with SP<4 is detected, this unit reverts the change in
  Delphi 2009 Update 3 that causes VCL apps (even new, empty projects) to
  crash on startup when run on Windows 2000 with no SP/SP1/SP2/sometimes SP3.

  This should be at the top of the .dpr's "uses" clause to ensure it runs
  before any VCL code.

  $jrsoftware: issrc/Projects/D2009Win2kFix.pas,v 1.2 2010/03/05 08:42:04 mlaan Exp $
}

interface

implementation

{$IFDEF VER200}
  {$DEFINE Delphi2009Or2010}
{$ENDIF}
{$IFDEF VER210}
  {$DEFINE Delphi2009Or2010}
{$ENDIF}

{$IFDEF Delphi2009Or2010}   { Only Delphi 2009/2010 }
uses
  Windows, SysUtils;

{
  Details:
  In Delphi 2009 Update 3 (or possibly one of the previous updates),
  TUTF8Encoding.Create in SysUtils was changed to set the
  MB_ERR_INVALID_CHARS flag:

         original: inherited Create(CP_UTF8);
    with Update 3: inherited Create(CP_UTF8, MB_ERR_INVALID_CHARS, 0);

  It appears that when used with CP_UTF8, the MB_ERR_INVALID_CHARS flag is
  only supported beginning with Windows 2000 SP4 and Windows XP. On Windows
  2000 with no SP, MultiByteToWideChar() fails with ERROR_INVALID_FLAGS.

  In Delphi 2010 Update 1 this change is still present.

  This code changes TEncoding.UTF8's private FMBToWCharFlags field from
  MB_ERR_INVALID_CHARS back to 0 when Windows 2000 (5.0) with SP<4 is
  detected.

  Note: This won't fix any other instances of TUTF8Encoding, but there
  shouldn't be more than just the one. (Inside the Delphi RTL/VCL,
  TEncoding.GetUTF8 is the only place where TUTF8Encoding.Create is called.)
}

function NeedWin2kFix: Boolean;
var
  Info: TOSVersionInfoEx;
begin
  Result := False;
  Info.dwOSVersionInfoSize := SizeOf(Info);
  if GetVersionEx(Info) then
    if (Info.dwMajorVersion = 5) and (Info.dwMinorVersion = 0) and
       (Info.wServicePackMajor < 4) then
      Result := True;
end;

procedure ApplyWin2kFix;
type
  PLongWordArray = ^TLongWordArray;
  TLongWordArray = array[0..6] of LongWord;  { 28 bytes }
var
  U: SysUtils.TEncoding;
begin
  U := SysUtils.TEncoding.UTF8;
  if (U.ClassType = SysUtils.TUTF8Encoding) and
     (U.InstanceSize = 28) and
     (U is SysUtils.TMBCSEncoding) and
     (SysUtils.TMBCSEncoding.InstanceSize = 28) then begin
     if (PLongWordArray(U)[3] = 65001) and  { verify that FCodePage = CP_UTF8 }
        (PLongWordArray(U)[4] = 8) and      { verify that FMBToWCharFlags = MB_ERR_INVALID_CHARS }
        (PLongWordArray(U)[5] = 0) then     { verify that FWCharToMBFlags = 0 }
       PLongWordArray(U)[4] := 0;           { change FMBToWCharFlags to 0 }
  end;
end;

initialization
  if NeedWin2kFix then
    ApplyWin2kFix;
{$ENDIF}
end.
