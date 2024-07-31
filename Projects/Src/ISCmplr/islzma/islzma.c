/*
  islzma.c, by Jordan Russell for Inno Setup
  This file is public domain (like the LZMA SDK)
*/

#include <stddef.h>
#include <windows.h>
#include "../../../../Components/Lzma2/Alloc.h"
#include "../../../../Components/Lzma2/LzmaEnc.h"
#include "../../../../Components/Lzma2/Lzma2Enc.h"
#include "islzma.h"

// Private definition of a handle; callers of the DLL should use void*
struct LZMAHandle {
	int marker;
	BOOL LZMA2;
	CLzmaEncHandle encoder1;
	CLzma2EncHandle encoder2;
};
#define LZMA_HANDLE_MARKER 0x3E1A981F
#define LZMA_HANDLE_VALID(h) ((h) && (h)->marker == LZMA_HANDLE_MARKER)

SRes __stdcall LZMA_Init(BOOL LZMA2, struct LZMAHandle **handle)
{
	struct LZMAHandle *h = calloc(1, sizeof(*h));
	if (!h) return SZ_ERROR_MEM;

	h->marker = LZMA_HANDLE_MARKER;
	h->LZMA2 = LZMA2;

	if (LZMA2) {
		if (!(h->encoder2 = Lzma2Enc_Create(&g_Alloc, &g_BigAlloc))) {
			free(h);
			return SZ_ERROR_MEM;
		}
	} else {
		if (!(h->encoder1 = LzmaEnc_Create(&g_Alloc))) {
			free(h);
			return SZ_ERROR_MEM;
		}
	}

	*handle = h;
	return SZ_OK;
}

SRes __stdcall LZMA_SetProps(struct LZMAHandle *handle,
	struct LZMAEncoderProps *encProps, size_t encPropsSize)
{
	CLzmaEncProps props1;
	CLzma2EncProps props2;
	CLzmaEncProps *props;

	if (!LZMA_HANDLE_VALID(handle) || encPropsSize != sizeof(*encProps)) {
		return SZ_ERROR_PARAM;
	}

	if (handle->LZMA2) {
		Lzma2EncProps_Init(&props2);
		props = &props2.lzmaProps;
	} else {
		LzmaEncProps_Init(&props1);
		props = &props1;
	}

	props->algo = encProps->Algorithm;
	props->dictSize = encProps->DictionarySize;
	props->fb = encProps->NumFastBytes;
	props->btMode = encProps->BTMode;
	props->numHashBytes = encProps->NumHashBytes;
	props->numThreads = encProps->NumThreads;

	if (handle->LZMA2) {
		props2.numBlockThreads_Max = encProps->NumBlockThreads;
		props2.blockSize = encProps->BlockSize;
		return Lzma2Enc_SetProps(handle->encoder2, &props2);
	} else {
		props1.writeEndMark = 1;
		return LzmaEnc_SetProps(handle->encoder1, &props1);
	}
}

SRes __stdcall LZMA_Encode(struct LZMAHandle *handle, ISeqInStream *inStream,
	ISeqOutStream *outStream, ICompressProgress *progress)
{
	if (!LZMA_HANDLE_VALID(handle)) return SZ_ERROR_PARAM;

	if (handle->LZMA2) {
		Byte props;
		SizeT propsSize = sizeof(props);

		props = Lzma2Enc_WriteProperties(handle->encoder2);
		if (outStream->Write(outStream, &props, propsSize) != propsSize) {
			return SZ_ERROR_WRITE;
		};
		return Lzma2Enc_Encode2(handle->encoder2, outStream, NULL, 0, inStream, NULL, 0, progress);
	} else {
		Byte props[LZMA_PROPS_SIZE];
		SizeT propsSize = sizeof(props);

		RINOK(LzmaEnc_WriteProperties(handle->encoder1, props, &propsSize));
		if (outStream->Write(outStream, &props, propsSize) != propsSize) {
			return SZ_ERROR_WRITE;
		};
		return LzmaEnc_Encode(handle->encoder1, outStream, inStream, progress,
			&g_Alloc, &g_BigAlloc);
	}
}

SRes __stdcall LZMA_End(struct LZMAHandle *handle)
{
	if (!LZMA_HANDLE_VALID(handle)) return SZ_ERROR_PARAM;

	handle->marker = 0;

	if (handle->LZMA2) {
		if (handle->encoder2) {
			Lzma2Enc_Destroy(handle->encoder2);
		}
	} else {
		if (handle->encoder1) {
			LzmaEnc_Destroy(handle->encoder1, &g_Alloc, &g_BigAlloc);
		}
	}

	free(handle);
	return SZ_OK;
}
