/*
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  External EXE-based LZMA encoder

  Structures and functions in this file are derived from
  LZMA.pas revision 1.49.2.3.

  Intentional deviations from the original Pascal code:
  - The WaitForMultipleObjects() calls in WakeMainAndWaitUntil,
    CheckTerminateWorkerEvent, and BeginEncode additionally wait on
    ProcessData.ParentProcess.
  Everything else *should* be 100% consistent.
*/

#include <windows.h>
#include <shlwapi.h>
#include "../../../../Components/Lzma2/7zTypes.h"
#include "islzma.h"

#define ISLZMA_EXE_VERSION 102

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
	THandle32 WorkerWaitingOnInputEvent;
	THandle32 WorkerWaitingOnOutputEvent;
	THandle32 WorkerEncodeFinishedEvent;
};

struct TLZMACompressorSharedData {
	volatile Int64 ProgressBytes;
	volatile BOOL NoMoreInput;
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

		/* On a weakly-ordered CPU, the read of Count above must happen before
		   Buf content is read below (otherwise the content could be stale) */
		MemoryBarrier();

		if (AWrite) {
			memcpy(&Ring->Buf[*Offset], P, Bytes);
			InterlockedExchangeAdd(&Ring->Count, Bytes);  /* full barrier */
		} else {
			memcpy(P, &Ring->Buf[*Offset], Bytes);
			InterlockedExchangeAdd(&Ring->Count, -Bytes);  /* full barrier */
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

static HRESULT CheckTerminateWorkerEvent(void)
{
	HANDLE H[2];

	H[0] = THandle32ToHandle(FEvents->TerminateWorkerEvent);
	H[1] = THandle32ToHandle(ProcessData.ParentProcess);
	switch (WaitForMultipleObjects(2, H, FALSE, 0)) {
		case WAIT_OBJECT_0 + 0:
		case WAIT_OBJECT_0 + 1:
			return E_ABORT;
		case WAIT_TIMEOUT:
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
	P = Data;
	while (Size != 0) {
		Longint LimitedSize = Size > MAXLONG ? MAXLONG : (Longint)Size;
		if (AWrite) {
			Bytes = RingBufferWrite(&FShared->OutputBuffer, P, LimitedSize);
		} else {
			if (FShared->NoMoreInput) {
				/* If NoMoreInput=True and *then* we see that the input buffer is
				   empty (ordering matters!), we know that all input has been
				   processed and that the input buffer will stay empty */
				MemoryBarrier();
				if (FShared->InputBuffer.Count == 0) {
					break;
				}
			}
			Bytes = RingBufferRead(&FShared->InputBuffer, P, LimitedSize);
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
	InterlockedExchange(&FReadLock, 0);

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
	InterlockedExchange(&FWriteLock, 0);

	return Result;
}

static HRESULT ProgressMade(const UInt64 TotalBytesProcessed)
/* Called from worker thread (or a thread spawned by the worker thread) */
{
	HRESULT Result;

	/* Sanity check: Make sure we're the only thread inside Progress */
	if (InterlockedExchange(&FProgressLock, 1) != 0) {
		return E_FAIL;
	}
	/* An Interlocked function is used to ensure the 64-bit value is written
	   atomically (not with two separate 32-bit writes).
	   TLZMACompressor will ignore negative values. LZMA SDK's 7zTypes.h says
	   "-1 for size means unknown value", though I don't see any place
	   where LzmaEnc actually does call Progress with inSize = -1. */
	InterlockedExchange64(&FShared->ProgressBytes, (Int64)TotalBytesProcessed);
	Result = CheckTerminateWorkerEvent();
	InterlockedExchange(&FProgressLock, 0);

	return Result;
}

static SRes LZMASeqInStreamReadWrapper(ISeqInStreamPtr p, void *buf, size_t *size)
{
	if (Read(buf, *size, size) == S_OK) {
		return SZ_OK;
	} else {
		return SZ_ERROR_READ;
	}
}

static size_t LZMASeqOutStreamWriteWrapper(ISeqOutStreamPtr p, const void *buf, size_t size)
{
	size_t Result;

	if (Write(buf, size, &Result) != S_OK) {
		return 0;
	}
	return Result;
}

static SRes LZMACompressProgressProgressWrapper(ICompressProgressPtr p, UInt64 inSize, UInt64 outSize)
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
