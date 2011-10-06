/*
  ISLzmaDec.c, by Jordan Russell for Inno Setup
  This file is public domain (like the LZMA SDK)

  LzmaDec.c + additional helper functions used by LZMADecomp.pas

  $jrsoftware: issrc/Projects/lzma2/Decoder/ISLzmaDec.c,v 1.3 2010/03/14 20:31:27 jr Exp $
*/

#include "../C/LzmaDec.c"

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
