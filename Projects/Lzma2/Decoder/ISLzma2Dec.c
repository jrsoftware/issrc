/*
  ISLzma2Dec.c, by Jordan Russell for Inno Setup
  This file is public domain (like the LZMA SDK)

  Lzma2Dec.c + additional helper functions used by LZMADecomp.pas

  $jrsoftware: issrc/Projects/lzma2/Decoder/ISLzma2Dec.c,v 1.3 2010/03/14 20:31:27 jr Exp $
*/

#include "../C/Lzma2Dec.c"

SRes IS_Lzma2Dec_Init(CLzma2Dec *state, size_t stateSize, Byte prop,
	ISzAlloc *alloc)
{
	if (stateSize != sizeof(*state)) {
		return SZ_ERROR_PARAM;
	}

	// Not needed; just sets fields to 0, which will leak memory if Init was already called previously
	//Lzma2Dec_Construct(state);

	RINOK(Lzma2Dec_Allocate(state, prop, alloc));
	Lzma2Dec_Init(state);

	return SZ_OK;
}

void IS_Lzma2Dec_Free(CLzma2Dec *state, ISzAlloc *alloc)
{
	// This exists because Lzma2Dec_Free is a macro

	Lzma2Dec_Free(state, alloc);
}
