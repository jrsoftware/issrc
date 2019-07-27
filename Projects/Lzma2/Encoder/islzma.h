/*
  islzma.h, by Jordan Russell for Inno Setup
  This file is public domain (like the LZMA SDK)

  $jrsoftware: issrc/Projects/lzma2/Encoder/islzma.h,v 1.2 2010/03/24 19:55:40 jr Exp $
*/

struct LZMAEncoderProps {
	int Algorithm;
	int BlockSize;
	int BTMode;
	int DictionarySize;
	int NumBlockThreads;
	int NumFastBytes;
	int NumThreads;
};

SRes __stdcall LZMA_Init(BOOL LZMA2, struct LZMAHandle **handle);
SRes __stdcall LZMA_SetProps(struct LZMAHandle *handle,
	struct LZMAEncoderProps *encProps, size_t encPropsSize);
SRes __stdcall LZMA_Encode(struct LZMAHandle *handle, ISeqInStream *inStream,
	ISeqOutStream *outStream, ICompressProgress *progress);
SRes __stdcall LZMA_End(struct LZMAHandle *handle);
