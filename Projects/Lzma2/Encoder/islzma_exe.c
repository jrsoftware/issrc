/*
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  External EXE-based LZMA encoder
  Built on Visual Studio 2005 SP1

  $jrsoftware: issrc/Projects/lzma2/Encoder/islzma_exe.c,v 1.2 2010/03/24 19:55:40 jr Exp $

  Structures and functions in this file are derived from
  LZMA.pas revision 1.49.2.3.

  Intentional deviations from the original Pascal code:
  - The WaitForMultipleObjects() calls in WakeMainAndWaitUntil and
    BeginEncode additionally wait on ProcessData.ParentProcess.
  Everything else *should* be 100% consistent.
*/

#include <windows.h>
#include <shlwapi.h>
#include "../C/Types.h"
#include "islzma.h"

#define ISLZMA_EXE_VERSION 101

typedef BYTE Byte;
typedef LONG Longint;
typedef ULONG LongWord;
typedef LongWord THandle32;

#define THandle32ToHandle(h) ULongToHandle(h)

struct TLZMACompressorRingBuffer {
	volatile Longint Count;   // updated by reader and writer using InterlockedExchangeAdd only
	Longint WriterOffset;     // accessed only by writer thread
	Longint ReaderOffset;     // accessed only by reader thread
	Byte Buf[0x100000];
};

struct TLZMACompressorSharedEvents {
	THandle32 TerminateWorkerEvent;
	THandle32 StartEncodeEvent;
	THandle32 EndWaitOnInputEvent;
	THandle32 EndWaitOnOutputEvent;
	THandle32 EndWaitOnProgressEvent;
	THandle32 WorkerWaitingOnInputEvent;
	THandle32 WorkerWaitingOnOutputEvent;
	THandle32 WorkerHasProgressEvent;
	THandle32 WorkerEncodeFinishedEvent;
};

struct TLZMACompressorSharedData {
	volatile BOOL NoMoreInput;
	volatile LongWord ProgressKB;
	volatile SRes EncodeResult;
	struct TLZMACompressorRingBuffer InputBuffer;
	struct TLZMACompressorRingBuffer OutputBuffer;
};

struct TLZMACompressorProcessData {
	LongWord StructSize;
	THandle32 ParentProcess;
	BOOL LZMA2;
	struct LZMAEncoderProps EncoderProps;
	struct TLZMACompressorSharedEvents Events;
	LongWord SharedDataStructSize;
	THandle32 SharedDataMapping;
};

static struct TLZMACompressorProcessData ProcessData;
static struct TLZMACompressorSharedEvents *FEvents;
static struct TLZMACompressorSharedData *FShared;
static volatile LONG FReadLock, FWriteLock, FProgressLock;
static volatile DWORD FLastProgressTick;

static Longint RingBufferInternalWriteOrRead(struct TLZMACompressorRingBuffer *Ring,
	const BOOL AWrite, Longint *Offset, void *Data, Longint Size)
{
	Byte *P = Data;
	Longint Bytes;
	Longint Result = 0;

	while (Size > 0) {
		if (AWrite) {
			Bytes = sizeof(Ring->Buf) - Ring->Count;
		} else {
			Bytes = Ring->Count;
		}
		if (Bytes == 0) {
			/* Buffer is full (write) or empty (read) */
			break;
		}
		if (Bytes > Size) {
			Bytes = Size;
		}
		if (Bytes > (Longint)sizeof(Ring->Buf) - *Offset) {
			Bytes = (Longint)sizeof(Ring->Buf) - *Offset;
		}

		if (AWrite) {
			memcpy(&Ring->Buf[*Offset], P, Bytes);
			InterlockedExchangeAdd(&Ring->Count, Bytes);
		} else {
			memcpy(P, &Ring->Buf[*Offset], Bytes);
			InterlockedExchangeAdd(&Ring->Count, -Bytes);
		}
		if (*Offset + Bytes == sizeof(Ring->Buf)) {
			*Offset = 0;
		} else {
			*Offset += Bytes;
		}

		Size -= Bytes;
		Result += Bytes;
		P += Bytes;
	}

	return Result;
}

static Longint RingBufferRead(struct TLZMACompressorRingBuffer *Ring,
	void *Buf, Longint Size)
{
	return RingBufferInternalWriteOrRead(Ring, FALSE, &Ring->ReaderOffset,
		Buf, Size);
}

static Longint RingBufferWrite(struct TLZMACompressorRingBuffer *Ring,
	void *Buf, Longint Size)
{
	return RingBufferInternalWriteOrRead(Ring, TRUE, &Ring->WriterOffset,
		Buf, Size);
}

static HRESULT WakeMainAndWaitUntil(HANDLE AWakeEvent, HANDLE AWaitEvent)
{
	HANDLE H[3];

	if (!SetEvent(AWakeEvent)) {
		SetEvent(THandle32ToHandle(FEvents->TerminateWorkerEvent));
		return E_FAIL;
	}
	H[0] = THandle32ToHandle(FEvents->TerminateWorkerEvent);
	H[1] = THandle32ToHandle(ProcessData.ParentProcess);
	H[2] = AWaitEvent;
	switch (WaitForMultipleObjects(3, H, FALSE, INFINITE)) {
		case WAIT_OBJECT_0 + 0:
		case WAIT_OBJECT_0 + 1:
			return E_ABORT;
		case WAIT_OBJECT_0 + 2:
			return S_OK;
		default:
			SetEvent(THandle32ToHandle(FEvents->TerminateWorkerEvent));
			return E_FAIL;
	}
}

static HRESULT FillBuffer(const BOOL AWrite, void *Data, size_t Size,
	size_t *ProcessedSize)
/* Called from worker thread (or a thread spawned by the worker thread) */
{
	Byte *P;
	Longint Bytes;
	HRESULT Result;

	*ProcessedSize = 0;
	if (Size > MAXLONG) {
		return E_INVALIDARG;
	}
	P = Data;
	while (Size != 0) {
		if (AWrite) {
			Bytes = RingBufferWrite(&FShared->OutputBuffer, P, (Longint)Size);
		} else {
			Bytes = RingBufferRead(&FShared->InputBuffer, P, (Longint)Size);
		}
		if (Bytes == 0) {
			if (AWrite) {
				/* Output buffer full; wait for the main thread to flush it */
				Result = WakeMainAndWaitUntil(
					THandle32ToHandle(FEvents->WorkerWaitingOnOutputEvent),
					THandle32ToHandle(FEvents->EndWaitOnOutputEvent));
				if (Result != S_OK) {
					return Result;
				}
			} else {
				/* Input buffer empty; wait for the main thread to fill it */
				if (FShared->NoMoreInput) {
					break;
				}
				Result = WakeMainAndWaitUntil(
					THandle32ToHandle(FEvents->WorkerWaitingOnInputEvent),
					THandle32ToHandle(FEvents->EndWaitOnInputEvent));
				if (Result != S_OK) {
					return Result;
				}
			}
		} else {
			*ProcessedSize += Bytes;
			Size -= Bytes;
			P += Bytes;
		}
	}
	return S_OK;
}

static HRESULT Read(void *Data, size_t Size, size_t *ProcessedSize)
/* Called from worker thread (or a thread spawned by the worker thread) */
{
	HRESULT Result;

	/* Sanity check: Make sure we're the only thread inside Read */
	if (InterlockedExchange(&FReadLock, 1) != 0) {
		return E_FAIL;
	}
	Result = FillBuffer(FALSE, Data, Size, ProcessedSize);
	FReadLock = 0;

	return Result;
}

static HRESULT Write(const void *Data, size_t Size, size_t *ProcessedSize)
/* Called from worker thread (or a thread spawned by the worker thread) */
{
	HRESULT Result;

	/* Sanity check: Make sure we're the only thread inside Write */
	if (InterlockedExchange(&FWriteLock, 1) != 0) {
		return E_FAIL;
	}
	Result = FillBuffer(TRUE, (void *)Data, Size, ProcessedSize);
	FWriteLock = 0;

	return Result;
}

static HRESULT ProgressMade(const UInt64 TotalBytesProcessed)
/* Called from worker thread (or a thread spawned by the worker thread) */
{
	DWORD T;
	UInt64 KBProcessed;
	HRESULT Result;

	T = GetTickCount();
	if (T - FLastProgressTick >= 100) {
		/* Sanity check: Make sure we're the only thread inside Progress */
		if (InterlockedExchange(&FProgressLock, 1) != 0) {
			return E_FAIL;
		}
		FLastProgressTick = T;
		/* Make sure TotalBytesProcessed isn't negative. LZMA's Types.h says
		   "-1 for size means unknown value", though I don't see any place
		   where LzmaEnc actually does call Progress with inSize = -1. */
		if ((Int64)TotalBytesProcessed >= 0) {
			KBProcessed = TotalBytesProcessed;
			KBProcessed /= 1024;
			FShared->ProgressKB = (LongWord)KBProcessed;
		}
		Result = WakeMainAndWaitUntil(
			THandle32ToHandle(FEvents->WorkerHasProgressEvent),
			THandle32ToHandle(FEvents->EndWaitOnProgressEvent));
		FProgressLock = 0;
	} else {
		Result = S_OK;
	}

	return Result;
}

static SRes LZMASeqInStreamReadWrapper(void *p, void *buf, size_t *size)
{
	if (Read(buf, *size, size) == S_OK) {
		return SZ_OK;
	} else {
		return SZ_ERROR_READ;
	}
}

static size_t LZMASeqOutStreamWriteWrapper(void *p, const void *buf, size_t size)
{
	size_t Result;

	if (Write(buf, size, &Result) != S_OK) {
		return 0;
	}
	return Result;
}

static SRes LZMACompressProgressProgressWrapper(void *p, UInt64 inSize, UInt64 outSize)
{
	if (ProgressMade(inSize) == S_OK) {
		return SZ_OK;
	} else {
		return SZ_ERROR_PROGRESS;
	}
}

static int BeginEncode(void)
{
	static ISeqInStream InStream = { LZMASeqInStreamReadWrapper };
	static ISeqOutStream OutStream = { LZMASeqOutStreamWriteWrapper };
	static ICompressProgress CompressProgress = { LZMACompressProgressProgressWrapper };

	struct LZMAHandle *FLZMAHandle;
	SRes res;
	HANDLE H[3];

	res = LZMA_Init(ProcessData.LZMA2, &FLZMAHandle);
	if (res != S_OK) {
		return MAKELONG(res, 20);
	}

	res = LZMA_SetProps(FLZMAHandle, &ProcessData.EncoderProps,
		sizeof(ProcessData.EncoderProps));
	if (res != S_OK) {
		return MAKELONG(res, 21);
	}

	// WorkerThreadProc:
	H[0] = THandle32ToHandle(FEvents->TerminateWorkerEvent);
	H[1] = THandle32ToHandle(ProcessData.ParentProcess);
	H[2] = THandle32ToHandle(FEvents->StartEncodeEvent);
	while (WaitForMultipleObjects(3, H, FALSE, INFINITE) == WAIT_OBJECT_0 + 2) {
		FShared->EncodeResult = LZMA_Encode(FLZMAHandle, &InStream, &OutStream,
			&CompressProgress);
		if (!SetEvent(THandle32ToHandle(FEvents->WorkerEncodeFinishedEvent))) {
			break;
		}
	}

	res = LZMA_End(FLZMAHandle);
	return MAKELONG(res, 22);
}

int WINAPI wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
	LPWSTR lpCmdLine, int nCmdShow)
{
	int Version;
	THandle32 ProcessDataMapping;
	struct TLZMACompressorProcessData *ProcessDataView;

	if (__argc < 3) {
		MessageBox(NULL, TEXT("This program is used internally by ")
			TEXT("the Inno Setup Compiler for LZMA compression. ")
			TEXT("It cannot be started directly."),
			TEXT("LZMA Compression Helper"), MB_OK | MB_ICONINFORMATION);
		return MAKELONG(0, 1);
	}
	if (!StrToIntEx(__wargv[1], STIF_DEFAULT, &Version) ||
			Version != ISLZMA_EXE_VERSION) {
		return MAKELONG(0, 2);
	}
	if (!StrToIntEx(__wargv[2], STIF_SUPPORT_HEX, &ProcessDataMapping)) {
		return MAKELONG(0, 3);
	}

	ProcessDataView = MapViewOfFile(THandle32ToHandle(ProcessDataMapping),
		FILE_MAP_READ, 0, 0, sizeof(ProcessData));
	if (!ProcessDataView) {
		return MAKELONG(GetLastError(), 4);
	}
	ProcessData = *ProcessDataView;
	UnmapViewOfFile(ProcessDataView);
	CloseHandle(THandle32ToHandle(ProcessDataMapping));

	if (ProcessData.StructSize != sizeof(ProcessData)) {
		return MAKELONG(0, 5);
	}
	if (ProcessData.SharedDataStructSize != sizeof(*FShared)) {
		return MAKELONG(0, 6);
	}

	FEvents = &ProcessData.Events;

	FShared = MapViewOfFile(THandle32ToHandle(ProcessData.SharedDataMapping),
		FILE_MAP_WRITE, 0, 0, sizeof(*FShared));
	if (!FShared) {
		return MAKELONG(GetLastError(), 7);
	}

	return BeginEncode();
}
