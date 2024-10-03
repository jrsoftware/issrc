/*
  IS7ZipDec.c, by Martijn Laan for Inno Setup
  This file is public domain (like the LZMA SDK)
*/

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

int IS_7ZipDec(WCHAR *fileName, BOOL fullPaths)
{
  WCHAR* args[3];
  args[0] = L"";
  args[1] = fullPaths?L"x":L"e";
  args[2] = fileName;
  return mainW(3, args);
}