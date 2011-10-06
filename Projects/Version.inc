{ $jrsoftware: issrc/Projects/Version.inc,v 1.11 2010/12/29 13:22:17 mlaan Exp $ }

{ Determine Delphi/C++Builder version }
{$IFNDEF VER90}  { if it's not Delphi 2.0 }
  {$IFNDEF VER93}  { and it's not C++Builder 1.0 }
    {$DEFINE IS_D3}  { then it must be at least Delphi 3 or C++Builder 3 }
    {$DEFINE Delphi3orHigher}
    {$IFNDEF VER100}  { if it's not Delphi 3.0 }
      {$IFNDEF VER110}  { and it's not C++Builder 3.0 }
        {$DEFINE IS_D4}  { then it must be at least Delphi 4 or C++Builder 4 }
        {$IFNDEF VER120} {$IFNDEF VER125}  { if it's not Delphi 4 or C++Builder 4 }
          {$DEFINE IS_D5}  { then it must be at least Delphi 5 or C++Builder 5 }
          {$IFNDEF VER130}  { if it's not Delphi 5 or C++Builder 5 }
            {$DEFINE IS_D6}  { then it must be at least Delphi 6 or C++Builder 6 }
            {$IFNDEF VER140}  { if it's not Delphi 6 or C++Builder 6 }
              {$DEFINE IS_D7}  { then it must be at least Delphi 7 }
              {$IFNDEF VER150}  { if it's not Delphi 7 }
                {$DEFINE IS_D8}  { then it must be at least Delphi 8 }
                {$IFNDEF VER160}  { if it's not Delphi 8 }
                  {$DEFINE IS_D9}  { then it must be at least Delphi 9 (2005) }
                  {$IFNDEF VER170}  { if it's not Delphi 9 (2005) }
                    {$DEFINE IS_D10}  { then it must be at least Delphi 10 (2006) }
                    { Delphi 11 (2007) is an odd case: it defines VER180 and VER185 on Win32, and VER190 on .NET }
                    {$IFDEF VER185}  { if it's Win32 Delphi 11 (2007) exactly }
                      {$DEFINE IS_D11}  { then it must be at least Delphi 11 (2007) }
                    {$ENDIF}
                    {$IFNDEF VER180}  { if it's neither Delphi 10 (2006) nor Win32 Delphi 11 (2007) }
                      {$DEFINE IS_D11}  { then it must be at least Delphi 11 (2007) }
                      {$IFNDEF VER190}  { if it's not .NET Delphi 11 (2007) }
                        {$DEFINE IS_D12}  { then it must be at least Delphi 12 (2009) }
                        {$IFNDEF VER200}  { if it's not Delphi 12 (2009) }
                          {$DEFINE IS_D14}  { then it must be at least Delphi 14 (2010) }
                        {$ENDIF}
                      {$ENDIF}
                    {$ENDIF}
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF} {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFNDEF IS_ALLOWD7}
{ Check for Delphi 7 and its WideString bug }
{$IFDEF IS_D6}
  {$IF Trunc(RTLVersion) = 15}
    { Delphi 7's WideString type is fundamentally flawed and should not be
      used; see QC #8565. If possible, you should compile with another version
      of Delphi (< 7 or 9).
      The bug is that it handles ANSI->Wide and Wide->ANSI conversions
      incorrectly; it uses the thread code page (CP_THREAD_ACP) instead of the
      system code page (CP_ACP). This is wrong because all of the "A" Windows
      APIs expect strings to use the system code page.
      In Inno Setup, this can result in shortcuts being created with incorrect
      names, since it converts ANSI->Wide when calling IPersistFile::Save.
      If you *really* want to compile under Delphi 7, you can remove this
      check. But expect breakage. }
    {$MESSAGE ERROR 'Delphi 7''s WideString type is broken. See comments in source code.'}
  {$IFEND}
{$ENDIF}
{$ENDIF}

{ Check for Delphi 2007 and its "Halt in except block" bug }
{$IFDEF VER185}
{ On Delphi 2007, if Halt is called inside an 'except' or 'finally' block, the
  current exception's message is forcibly displayed on the screen (even if
  it's an EAbort exception), and the process returns with an exit code of 1
  instead of the exit code specified in the Halt call. See QC #51427.
  There are many places in Inno Setup that are affected by this issue (see
  the Halt calls in Setup.dpr for example). }
{$MESSAGE ERROR 'Delphi 2007''s Halt is broken. See comments in source code.'}
{$ENDIF}
