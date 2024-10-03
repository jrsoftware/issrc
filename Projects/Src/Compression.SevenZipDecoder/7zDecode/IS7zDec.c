/*
  IS7zDec.c, by Martijn Laan for Inno Setup
  This file is public domain (like the LZMA SDK)
*/

#ifdef _MSC_VER

/* Stop 7-Zip from using stdcall functions which will get unavoidable decorated names from
   MSVC's cl.exe which Delphi can't handle: first include windows.h and then hide the
   functions 7-Zip wants to use with macros pointing to cdecl prototypes. This will enable
   is us to call the stdcall function from a cdecl implementation in Delphi and keeps the
   rest of windows.h available to 7-Zip. */

#include "../../../../Components/Lzma2/Util/7z/Precomp.h" /* Says it must be included first */
#include "../../../../Components/Lzma2/7zWindows.h"

BOOL _CreateDirectoryW(LPCWSTR lpPathName, LPSECURITY_ATTRIBUTES lpSecurityAttributes);
#define CreateDirectoryW _CreateDirectoryW

#endif

#include "../../../../Components/Lzma2/Util/7z/7zMain.c"

#include "../../../../Components/Lzma2/7zAlloc.c"
#include "../../../../Components/Lzma2/7zArcIn.c"
#include "../../../../Components/Lzma2/7zBuf.c"
#include "../../../../Components/Lzma2/7zCrc.c"
#include "../../../../Components/Lzma2/7zCrcOpt.c"
#include "../../../../Components/Lzma2/7zDec.c"
#include "../../../../Components/Lzma2/7zFile.c"
#include "../../../../Components/Lzma2/7zStream.c"
#include "../../../../Components/Lzma2/Bcj2.c"
#include "../../../../Components/Lzma2/Bra.c"
#include "../../../../Components/Lzma2/Bra86.c"
#include "../../../../Components/Lzma2/Delta.c"
#include "../../../../Components/Lzma2/LzmaDec.c"
#include "../../../../Components/Lzma2/Lzma2Dec.c"

int IS_7zDec(WCHAR *fileName, BOOL fullPaths)
{
  WCHAR* args[3];
  args[0] = L"";
  args[1] = fullPaths?L"x":L"e";
  args[2] = fileName;
  return mainW(3, args);
}