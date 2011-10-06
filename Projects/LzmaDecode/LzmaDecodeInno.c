/*
  LzmaDecodeInno.c = LzmaDecodeSize.c + additional helper functions used by
  Inno Setup's LZMA.pas

  $jrsoftware: issrc/Projects/LzmaDecode/LzmaDecodeInno.c,v 1.1 2006/09/05 01:34:28 jr Exp $
*/

#include "LzmaDecodeSize.c"

int LzmaMyDecodeProperties(CLzmaDecoderState *vs, int vsSize,
  const unsigned char *propsData, int propsDataSize, UInt32 *outProbsSize,
  UInt32 *outDictionarySize)
{
  int retval;

  /*
    First verify that the state structure passed by the caller is the
    correct size.
  */
  if (sizeof(*vs) != vsSize)
    return LZMA_RESULT_DATA_ERROR;   /* for lack of a better error code */

  retval = LzmaDecodeProperties(&vs->Properties, propsData, propsDataSize);
  if (retval == LZMA_RESULT_OK)
  {
    *outProbsSize = LzmaGetNumProbs(&vs->Properties) * sizeof(CProb);
    *outDictionarySize = vs->Properties.DictionarySize;
  }
  return retval;
}

void LzmaMyDecoderInit(CLzmaDecoderState *vs, void *probsPtr,
  void *dictionaryPtr)
{
  vs->Probs = (CProb*)probsPtr;
  vs->Dictionary = (unsigned char*)dictionaryPtr;
  LzmaDecoderInit(vs);
}
