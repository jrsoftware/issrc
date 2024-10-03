/*
  ISLzmaDec.c, by Jordan Russell for Inno Setup
  This file is public domain (like the LZMA SDK)

  LzmaDec.c + Lzma2Dec.c + additional helper functions used by Compression.LZMADecompressor.pas
*/

#include "../../../../Components/Lzma2/LzmaDec.c"
#include "../../../../Components/Lzma2/Lzma2Dec.c"

SRes IS_LzmaDec_Init(CLzmaDec *state, size_t stateSize, const Byte *props,
	unsigned propsSize, ISzAlloc *alloc)
{
	if (stateSize != sizeof(*state)) {
		return SZ_ERROR_PARAM;
	}

	// Not needed; just sets fields to 0, which will leak memory if Init was already called previously
	//LzmaDec_Construct(state);

	RINOK(LzmaDec_Allocate(state, props, propsSize, alloc));
	LzmaDec_Init(state);

	return SZ_OK;
}

size_t IS_LzmaDec_StateSize()
{
  return sizeof(CLzmaDec);
}

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

size_t IS_Lzma2Dec_StateSize()
{
  return sizeof(CLzma2Dec);
}

void IS_Lzma2Dec_Free(CLzma2Dec *state, ISzAlloc *alloc)
{
	// This exists because Lzma2Dec_Free is a macro

	Lzma2Dec_Free(state, alloc);
}
