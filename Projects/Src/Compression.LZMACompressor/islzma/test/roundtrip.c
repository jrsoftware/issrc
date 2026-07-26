/*
 * Round-trip and speed test for the Inno Setup LZMA compression binaries.
 *
 * Compresses via three methods: islzma.dll in-process, and the islzma32.exe
 * and islzma64.exe worker processes, driven with the same shared-memory
 * protocol as Compression.LZMACompressor.pas. Decompresses with the linked
 * ISLzmaDec OBJ (the decoder Setup links) and verifies the data.
 *
 * Build and run via test.bat, which copies the release binaries next to the
 * EXE and links the matching ISLzmaDec-x86.obj or ISLzmaDec-x64.obj.
 */

#define _CRT_SECURE_NO_WARNINGS
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define N        200000u                  /* round-trip buffer size */
#define SPEED_N  (16u * 1000u * 1000u)    /* speed test buffer size */
#define MB       1000000.0
#define MIN_PASS 3                        /* never time fewer passes than this */
#define MIN_SECS 0.5                      /* sample for at least this long in total */
#define MAX_SECS 3.0                      /* ... but no longer, once MIN_PASS is met */
#define SETTLED  2                        /* passes the best must survive to be believed */

/* Compressed-output capacity; LZMA expands incompressible data by far less */
#define COMPCAP  (SPEED_N + SPEED_N / 50u + 4096u)

#define ISLZMA_EXE_VERSION 102

typedef int SRes;
#define SZ_OK 0

/* ELzmaFinishMode / ELzmaStatus */
#define LZMA_FINISH_END                1
#define LZMA_STATUS_FINISHED_WITH_MARK 1

typedef BYTE Byte;
typedef LONG Longint;
typedef ULONG THandle32;

struct LZMAEncoderProps {
    int Algorithm;
    int BlockSize;
    int BTMode;
    int NumHashBytes;
    UINT32 DictionarySize;
    int NumBlockThreads;
    int NumFastBytes;
    int NumThreads;
    int NumThreadGroups;
};

/* Same settings as the compiler's Compression=lzma/normal */
static const struct LZMAEncoderProps enc_props =
    { 1, 0, 1, 4, 2u << 20, -1, 32, -1, 0 };

/* Build a buffer that mixes compressible repetition with incompressible LCG
   noise, so both match-copying and entropy coding get exercised. */
static void make_buffer(unsigned char *b, size_t n)
{
    size_t i;
    unsigned int lcg = 12345u;
    for (i = 0; i < n; i++) {
        lcg = lcg * 1103515245u + 12345u;
        b[i] = (i % 61 < 40) ? (unsigned char)('A' + (i % 26))
                             : (unsigned char)(lcg >> 24);
    }
}

/* One compression or decompression pass. Returns 0 on success, 1 after
   printing what went wrong. */
typedef int (*fn_pass)(const unsigned char *src, size_t src_size,
                       unsigned char *dst, size_t dst_cap, size_t *dst_size);

/* ----------------------------------------------- ISLzmaDec OBJ decoder ---- */

typedef struct ISzAlloc ISzAlloc;
struct ISzAlloc {
    void *(*Alloc)(const ISzAlloc *p, size_t size);
    void (*Free)(const ISzAlloc *p, void *address);
};

SRes IS_LzmaDec_Init(void *state, size_t stateSize, const Byte *props,
                     unsigned propsSize, const ISzAlloc *alloc);
size_t IS_LzmaDec_StateSize(void);
SRes LzmaDec_DecodeToBuf(void *state, Byte *dest, size_t *destLen,
                         const Byte *src, size_t *srcLen, int finishMode,
                         int *status);
void LzmaDec_Free(void *state, const ISzAlloc *alloc);

SRes IS_Lzma2Dec_Init(void *state, size_t stateSize, Byte prop,
                      const ISzAlloc *alloc);
size_t IS_Lzma2Dec_StateSize(void);
SRes Lzma2Dec_DecodeToBuf(void *state, Byte *dest, size_t *destLen,
                          const Byte *src, size_t *srcLen, int finishMode,
                          int *status);
void IS_Lzma2Dec_Free(void *state, const ISzAlloc *alloc);

static void *SzAlloc(const ISzAlloc *p, size_t size) { return malloc(size); }
static void SzFree(const ISzAlloc *p, void *address) { free(address); }
static const ISzAlloc dec_alloc = { SzAlloc, SzFree };

static int lzma1_decompress(const unsigned char *src, size_t src_size,
                            unsigned char *dst, size_t dst_cap, size_t *dst_size)
{
    void *state;
    size_t state_size = IS_LzmaDec_StateSize();
    size_t in_pos = 5, out_pos = 0;  /* 5-byte props header first */
    int status = -1;
    SRes res;

    if (src_size < 5) { printf("  lzma1 header missing\n"); return 1; }
    state = calloc(1, state_size);
    if (!state) { printf("  state allocation failed\n"); return 1; }
    res = IS_LzmaDec_Init(state, state_size, src, 5, &dec_alloc);
    if (res != SZ_OK) {
        printf("  IS_LzmaDec_Init failed (%d)\n", res);
        free(state);
        return 1;
    }

    while (status != LZMA_STATUS_FINISHED_WITH_MARK) {
        size_t destLen = dst_cap - out_pos, srcLen = src_size - in_pos;
        res = LzmaDec_DecodeToBuf(state, dst + out_pos, &destLen,
                                  src + in_pos, &srcLen, LZMA_FINISH_END, &status);
        if (res != SZ_OK || (destLen == 0 && srcLen == 0)) {
            printf("  LzmaDec_DecodeToBuf failed (%d, status %d)\n", res, status);
            LzmaDec_Free(state, &dec_alloc);
            free(state);
            return 1;
        }
        out_pos += destLen;
        in_pos += srcLen;
    }
    LzmaDec_Free(state, &dec_alloc);
    free(state);

    *dst_size = out_pos;
    return 0;
}

static int lzma2_decompress(const unsigned char *src, size_t src_size,
                            unsigned char *dst, size_t dst_cap, size_t *dst_size)
{
    void *state;
    size_t state_size = IS_Lzma2Dec_StateSize();
    size_t in_pos = 1, out_pos = 0;  /* 1-byte prop header first */
    int status = -1;
    SRes res;

    if (src_size < 1) { printf("  lzma2 header missing\n"); return 1; }
    state = calloc(1, state_size);
    if (!state) { printf("  state allocation failed\n"); return 1; }
    res = IS_Lzma2Dec_Init(state, state_size, src[0], &dec_alloc);
    if (res != SZ_OK) {
        printf("  IS_Lzma2Dec_Init failed (%d)\n", res);
        free(state);
        return 1;
    }

    while (status != LZMA_STATUS_FINISHED_WITH_MARK) {
        size_t destLen = dst_cap - out_pos, srcLen = src_size - in_pos;
        res = Lzma2Dec_DecodeToBuf(state, dst + out_pos, &destLen,
                                   src + in_pos, &srcLen, LZMA_FINISH_END, &status);
        if (res != SZ_OK || (destLen == 0 && srcLen == 0)) {
            printf("  Lzma2Dec_DecodeToBuf failed (%d, status %d)\n", res, status);
            IS_Lzma2Dec_Free(state, &dec_alloc);
            free(state);
            return 1;
        }
        out_pos += destLen;
        in_pos += srcLen;
    }
    IS_Lzma2Dec_Free(state, &dec_alloc);
    free(state);

    *dst_size = out_pos;
    return 0;
}

/* ------------------------------------------------ islzma.dll in-process ---- */

/* The Win32 DLL is built /Gz, so its exports and stream callbacks are
   __stdcall (meaningless but harmless on x64) */
typedef struct { SRes (__stdcall *Read)(void *p, void *buf, size_t *size); } TSeqInStream;
typedef struct { size_t (__stdcall *Write)(void *p, const void *buf, size_t size); } TSeqOutStream;
typedef struct { SRes (__stdcall *Progress)(void *p, unsigned __int64 inSize, unsigned __int64 outSize); } TCompressProgress;

typedef SRes (__stdcall *fn_LZMA_Init)(BOOL LZMA2, void **handle);
typedef SRes (__stdcall *fn_LZMA_SetProps)(void *handle,
    const struct LZMAEncoderProps *encProps, size_t encPropsSize);
typedef SRes (__stdcall *fn_LZMA_Encode)(void *handle, TSeqInStream *inStream,
    TSeqOutStream *outStream, TCompressProgress *progress);
typedef SRes (__stdcall *fn_LZMA_End)(void *handle);

static fn_LZMA_Init     LZMA_Init;
static fn_LZMA_SetProps LZMA_SetProps;
static fn_LZMA_Encode   LZMA_Encode;
static fn_LZMA_End      LZMA_End;

static void *dll_handle;
static const unsigned char *dll_src;
static size_t dll_src_size, dll_src_pos;
static unsigned char *dll_dst;
static size_t dll_dst_cap, dll_dst_pos;

#define GET(h, type, var, name) \
    var = (type)GetProcAddress(h, name); \
    if (!var) { printf("missing export %s\n", name); return 1; }

static int dll_load(void)
{
    HMODULE h = LoadLibraryA("islzma.dll");
    if (!h) { printf("LoadLibrary failed (islzma.dll)\n"); return 1; }
    GET(h, fn_LZMA_Init,     LZMA_Init,     "LZMA_Init3")
    GET(h, fn_LZMA_SetProps, LZMA_SetProps, "LZMA_SetProps3")
    GET(h, fn_LZMA_Encode,   LZMA_Encode,   "LZMA_Encode3")
    GET(h, fn_LZMA_End,      LZMA_End,      "LZMA_End3")
    return 0;
}

static SRes __stdcall dll_read(void *p, void *buf, size_t *size)
{
    size_t n = dll_src_size - dll_src_pos;
    if (n > *size)
        n = *size;
    memcpy(buf, dll_src + dll_src_pos, n);
    dll_src_pos += n;
    *size = n;
    return SZ_OK;
}

static size_t __stdcall dll_write(void *p, const void *buf, size_t size)
{
    if (size > dll_dst_cap - dll_dst_pos)
        return 0;
    memcpy(dll_dst + dll_dst_pos, buf, size);
    dll_dst_pos += size;
    return size;
}

static SRes __stdcall dll_progress(void *p, unsigned __int64 inSize,
    unsigned __int64 outSize)
{
    return SZ_OK;
}

static int dll_start(BOOL lzma2)
{
    SRes res = LZMA_Init(lzma2, &dll_handle);
    if (res != SZ_OK) { printf("  LZMA_Init failed (%d)\n", res); return 1; }
    res = LZMA_SetProps(dll_handle, &enc_props, sizeof(enc_props));
    if (res != SZ_OK) { printf("  LZMA_SetProps failed (%d)\n", res); return 1; }
    return 0;
}

static int dll_compress(const unsigned char *src, size_t src_size,
                        unsigned char *dst, size_t dst_cap, size_t *dst_size)
{
    static TSeqInStream in = { dll_read };
    static TSeqOutStream out = { dll_write };
    static TCompressProgress progress = { dll_progress };
    SRes res;

    dll_src = src; dll_src_size = src_size; dll_src_pos = 0;
    dll_dst = dst; dll_dst_cap = dst_cap; dll_dst_pos = 0;
    res = LZMA_Encode(dll_handle, &in, &out, &progress);
    if (res != SZ_OK) { printf("  LZMA_Encode failed (%d)\n", res); return 1; }

    *dst_size = dll_dst_pos;
    return 0;
}

static int dll_stop(void)
{
    SRes res;
    if (!dll_handle)
        return 0;
    res = LZMA_End(dll_handle);
    dll_handle = NULL;
    if (res != SZ_OK) { printf("  LZMA_End failed (%d)\n", res); return 1; }
    return 0;
}

/* ------------------------------------------- islzma exe worker process ---- */

/* Structures shared with islzma_exe.c / Compression.LZMACompressor.pas */

struct TLZMACompressorRingBuffer {
    volatile Longint Count;
    Longint WriterOffset;
    Longint ReaderOffset;
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
    volatile __int64 ProgressBytes;
    volatile BOOL NoMoreInput;
    volatile SRes EncodeResult;
    struct TLZMACompressorRingBuffer InputBuffer;
    struct TLZMACompressorRingBuffer OutputBuffer;
};

struct TLZMACompressorProcessData {
    ULONG StructSize;
    THandle32 ParentProcess;
    BOOL LZMA2;
    struct LZMAEncoderProps EncoderProps;
    struct TLZMACompressorSharedEvents Events;
    ULONG SharedDataStructSize;
    THandle32 SharedDataMapping;
};

static void RingBufferReset(struct TLZMACompressorRingBuffer *Ring)
{
    Ring->Count = 0;
    Ring->WriterOffset = 0;
    Ring->ReaderOffset = 0;
}

static Longint RingBufferInternalWriteOrRead(struct TLZMACompressorRingBuffer *Ring,
    const BOOL AWrite, Longint *Offset, void *Data, Longint Size)
{
    Byte *P = Data;
    Longint Bytes;
    Longint Result = 0;

    while (Size > 0) {
        if (AWrite) {
            Bytes = (Longint)sizeof(Ring->Buf) - Ring->Count;
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

        /* The read of Count above must happen before Buf content is read */
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

static struct {
    HANDLE process;
    HANDLE shared_mapping;
    struct TLZMACompressorSharedData *shared;
    HANDLE terminate_evt;    /* manual reset */
    HANDLE start_evt;        /* auto reset */
    HANDLE end_in_evt;       /* auto reset */
    HANDLE end_out_evt;      /* auto reset */
    HANDLE waiting_in_evt;   /* manual reset */
    HANDLE waiting_out_evt;  /* manual reset */
    HANDLE finished_evt;     /* manual reset */
} w;

static unsigned char *exe_dst;
static size_t exe_dst_cap, exe_dst_pos;

static int satisfy_worker_wait(HANDLE worker_evt, HANDLE end_evt)
{
    DWORD r = WaitForSingleObject(worker_evt, 0);
    if (r == WAIT_TIMEOUT)
        return 0;
    if (r != WAIT_OBJECT_0 || !ResetEvent(worker_evt) || !SetEvent(end_evt)) {
        printf("  satisfy_worker_wait failed\n");
        return 1;
    }
    return 0;
}

static int flush_output(void)
{
    for (;;) {
        Longint bytes = w.shared->OutputBuffer.Count;
        if (bytes == 0)
            break;
        if ((size_t)bytes > exe_dst_cap - exe_dst_pos) {
            printf("  output overflow\n");
            return 1;
        }
        RingBufferRead(&w.shared->OutputBuffer, exe_dst + exe_dst_pos, bytes);
        exe_dst_pos += (size_t)bytes;
        if (satisfy_worker_wait(w.waiting_out_evt, w.end_out_evt))
            return 1;
    }
    /* Also satisfy when nothing was flushed (see FlushOutputBuffer in
       Compression.LZMACompressor.pas for why this avoids a deadlock) */
    return satisfy_worker_wait(w.waiting_out_evt, w.end_out_evt);
}

static int wait_worker(int *finished)
{
    HANDLE h[4];

    /* Process handle first so unexpected termination takes precedence */
    h[0] = w.process;
    h[1] = w.finished_evt;
    h[2] = w.waiting_in_evt;
    h[3] = w.waiting_out_evt;
    switch (WaitForMultipleObjects(4, h, FALSE, INFINITE)) {
        case WAIT_OBJECT_0 + 0: {
            DWORD code = 0;
            GetExitCodeProcess(w.process, &code);
            printf("  worker terminated unexpectedly (0x%lx)\n", code);
            return 1;
        }
        case WAIT_OBJECT_0 + 1:
            *finished = 1;
            return 0;
        case WAIT_OBJECT_0 + 2:
        case WAIT_OBJECT_0 + 3:
            return 0;
    }
    printf("  WaitForMultipleObjects failed\n");
    return 1;
}

static BOOL dupe_handle(HANDLE src, HANDLE process, THandle32 *dest, DWORD access)
{
    HANDLE h;
    if (!DuplicateHandle(GetCurrentProcess(), src, process, &h, access, FALSE, 0))
        return FALSE;
    *dest = (THandle32)(ULONG_PTR)h;
    return TRUE;
}

static int exe_start(const char *exe, BOOL lzma2)
{
    SECURITY_ATTRIBUTES inheritable = { sizeof(SECURITY_ATTRIBUTES), NULL, TRUE };
    HANDLE pd_mapping;
    struct TLZMACompressorProcessData *pd;
    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    char cmdline[64];
    BOOL ok;

    memset(&w, 0, sizeof(w));
    w.terminate_evt = CreateEventA(NULL, TRUE, FALSE, NULL);
    w.start_evt = CreateEventA(NULL, FALSE, FALSE, NULL);
    w.end_in_evt = CreateEventA(NULL, FALSE, FALSE, NULL);
    w.end_out_evt = CreateEventA(NULL, FALSE, FALSE, NULL);
    w.waiting_in_evt = CreateEventA(NULL, TRUE, FALSE, NULL);
    w.waiting_out_evt = CreateEventA(NULL, TRUE, FALSE, NULL);
    w.finished_evt = CreateEventA(NULL, TRUE, FALSE, NULL);
    if (!w.terminate_evt || !w.start_evt || !w.end_in_evt || !w.end_out_evt ||
            !w.waiting_in_evt || !w.waiting_out_evt || !w.finished_evt) {
        printf("  CreateEvent failed\n");
        return 1;
    }

    w.shared_mapping = CreateFileMappingA(INVALID_HANDLE_VALUE, NULL,
        PAGE_READWRITE, 0, sizeof(*w.shared), NULL);
    if (!w.shared_mapping) { printf("  CreateFileMapping failed\n"); return 1; }
    w.shared = MapViewOfFile(w.shared_mapping, FILE_MAP_WRITE, 0, 0,
        sizeof(*w.shared));
    if (!w.shared) { printf("  MapViewOfFile failed\n"); return 1; }

    pd_mapping = CreateFileMappingA(INVALID_HANDLE_VALUE, &inheritable,
        PAGE_READWRITE, 0, sizeof(*pd), NULL);
    if (!pd_mapping) { printf("  CreateFileMapping failed\n"); return 1; }
    pd = MapViewOfFile(pd_mapping, FILE_MAP_WRITE, 0, 0, sizeof(*pd));
    if (!pd) {
        printf("  MapViewOfFile failed\n");
        CloseHandle(pd_mapping);
        return 1;
    }

    pd->StructSize = sizeof(*pd);
    pd->LZMA2 = lzma2;
    pd->EncoderProps = enc_props;
    pd->SharedDataStructSize = sizeof(*w.shared);

    memset(&si, 0, sizeof(si));
    si.cb = sizeof(si);
    sprintf(cmdline, "islzma_exe %d 0x%x", ISLZMA_EXE_VERSION,
        (unsigned)(ULONG_PTR)pd_mapping);
    if (!CreateProcessA(exe, cmdline, NULL, NULL, TRUE,
            CREATE_DEFAULT_ERROR_MODE | CREATE_SUSPENDED, NULL, NULL, &si, &pi)) {
        printf("  CreateProcess failed (%s, error %lu)\n", exe, GetLastError());
        UnmapViewOfFile(pd);
        CloseHandle(pd_mapping);
        return 1;
    }

    ok = dupe_handle(GetCurrentProcess(), pi.hProcess, &pd->ParentProcess,
             SYNCHRONIZE) &&
         dupe_handle(w.shared_mapping, pi.hProcess, &pd->SharedDataMapping,
             FILE_MAP_WRITE) &&
         dupe_handle(w.terminate_evt, pi.hProcess,
             &pd->Events.TerminateWorkerEvent, SYNCHRONIZE | EVENT_MODIFY_STATE) &&
         dupe_handle(w.start_evt, pi.hProcess,
             &pd->Events.StartEncodeEvent, SYNCHRONIZE | EVENT_MODIFY_STATE) &&
         dupe_handle(w.end_in_evt, pi.hProcess,
             &pd->Events.EndWaitOnInputEvent, SYNCHRONIZE | EVENT_MODIFY_STATE) &&
         dupe_handle(w.end_out_evt, pi.hProcess,
             &pd->Events.EndWaitOnOutputEvent, SYNCHRONIZE | EVENT_MODIFY_STATE) &&
         dupe_handle(w.waiting_in_evt, pi.hProcess,
             &pd->Events.WorkerWaitingOnInputEvent, SYNCHRONIZE | EVENT_MODIFY_STATE) &&
         dupe_handle(w.waiting_out_evt, pi.hProcess,
             &pd->Events.WorkerWaitingOnOutputEvent, SYNCHRONIZE | EVENT_MODIFY_STATE) &&
         dupe_handle(w.finished_evt, pi.hProcess,
             &pd->Events.WorkerEncodeFinishedEvent, SYNCHRONIZE | EVENT_MODIFY_STATE) &&
         ResumeThread(pi.hThread) != (DWORD)-1;

    UnmapViewOfFile(pd);
    CloseHandle(pd_mapping);
    CloseHandle(pi.hThread);
    if (!ok) {
        printf("  worker setup failed\n");
        TerminateProcess(pi.hProcess, 1);
        WaitForSingleObject(pi.hProcess, INFINITE);
        CloseHandle(pi.hProcess);
        return 1;
    }
    w.process = pi.hProcess;
    return 0;
}

static int exe_compress(const unsigned char *src, size_t src_size,
                        unsigned char *dst, size_t dst_cap, size_t *dst_size)
{
    const unsigned char *p = src;
    size_t remaining = src_size;
    Longint chunk, n;
    int finished = 0;

    exe_dst = dst; exe_dst_cap = dst_cap; exe_dst_pos = 0;

    w.shared->NoMoreInput = FALSE;
    w.shared->ProgressBytes = 0;
    w.shared->EncodeResult = -1;
    RingBufferReset(&w.shared->InputBuffer);
    RingBufferReset(&w.shared->OutputBuffer);
    if (!ResetEvent(w.finished_evt) || !SetEvent(w.start_evt)) {
        printf("  start encode failed\n");
        return 1;
    }

    while (remaining > 0) {
        if (finished) {
            printf("  LZMA_Encode failed (%d)\n", w.shared->EncodeResult);
            return 1;
        }
        if (flush_output())
            return 1;
        chunk = remaining > MAXLONG ? MAXLONG : (Longint)remaining;
        n = RingBufferWrite(&w.shared->InputBuffer, (void *)p, chunk);
        if (n == 0) {
            /* Input buffer full; unblock worker Read, then wait */
            if (satisfy_worker_wait(w.waiting_in_evt, w.end_in_evt))
                return 1;
            if (wait_worker(&finished))
                return 1;
        } else {
            remaining -= n;
            p += n;
            if (satisfy_worker_wait(w.waiting_in_evt, w.end_in_evt))
                return 1;
        }
    }

    MemoryBarrier();
    w.shared->NoMoreInput = TRUE;
    while (!finished) {
        if (satisfy_worker_wait(w.waiting_in_evt, w.end_in_evt))
            return 1;
        if (flush_output())
            return 1;
        if (wait_worker(&finished))
            return 1;
    }
    if (flush_output())
        return 1;

    if (w.shared->EncodeResult != SZ_OK) {
        printf("  LZMA_Encode failed (%d)\n", w.shared->EncodeResult);
        return 1;
    }
    if (w.shared->InputBuffer.Count != 0) {
        printf("  input not fully consumed\n");
        return 1;
    }

    *dst_size = exe_dst_pos;
    return 0;
}

static int exe_stop(void)
{
    if (w.process) {
        SetEvent(w.terminate_evt);
        WaitForSingleObject(w.process, INFINITE);
        CloseHandle(w.process);
        w.process = NULL;
    }
    if (w.shared) { UnmapViewOfFile((void *)w.shared); w.shared = NULL; }
    if (w.shared_mapping) { CloseHandle(w.shared_mapping); w.shared_mapping = NULL; }
    if (w.terminate_evt) { CloseHandle(w.terminate_evt); w.terminate_evt = NULL; }
    if (w.start_evt) { CloseHandle(w.start_evt); w.start_evt = NULL; }
    if (w.end_in_evt) { CloseHandle(w.end_in_evt); w.end_in_evt = NULL; }
    if (w.end_out_evt) { CloseHandle(w.end_out_evt); w.end_out_evt = NULL; }
    if (w.waiting_in_evt) { CloseHandle(w.waiting_in_evt); w.waiting_in_evt = NULL; }
    if (w.waiting_out_evt) { CloseHandle(w.waiting_out_evt); w.waiting_out_evt = NULL; }
    if (w.finished_evt) { CloseHandle(w.finished_evt); w.finished_evt = NULL; }
    return 0;
}

/* -------------------------------------------------------------- methods ---- */

typedef struct {
    const char *name;
    const char *exe;   /* NULL = in-process islzma.dll */
    BOOL lzma2;
    fn_pass decompress;
} method;

static const method methods[] = {
    { "dll/lzma1",   NULL,           FALSE, lzma1_decompress },
    { "dll/lzma2",   NULL,           TRUE,  lzma2_decompress },
    { "exe32/lzma1", "islzma32.exe", FALSE, lzma1_decompress },
    { "exe32/lzma2", "islzma32.exe", TRUE,  lzma2_decompress },
    { "exe64/lzma1", "islzma64.exe", FALSE, lzma1_decompress },
    { "exe64/lzma2", "islzma64.exe", TRUE,  lzma2_decompress }
};
#define NMETHODS (sizeof methods / sizeof methods[0])

static int method_start(const method *m)
{
    return m->exe ? exe_start(m->exe, m->lzma2) : dll_start(m->lzma2);
}

static int method_stop(const method *m)
{
    return m->exe ? exe_stop() : dll_stop();
}

static fn_pass method_compress(const method *m)
{
    return m->exe ? exe_compress : dll_compress;
}

/* ---------------------------------------------------------- round-trip ---- */

static int roundtrip(const method *m, const unsigned char *orig,
                     unsigned char *comp, unsigned char *deco)
{
    size_t comp_size, deco_size;
    int ok;

    if (method_compress(m)(orig, N, comp, COMPCAP, &comp_size)) {
        printf("  %-11s compress FAILED\n", m->name);
        return 1;
    }
    if (m->decompress(comp, comp_size, deco, N, &deco_size)) {
        printf("  %-11s decompress FAILED\n", m->name);
        return 1;
    }

    ok = (deco_size == N) && (memcmp(orig, deco, N) == 0);
    printf("  %-11s orig=%u compress=%u decompress=%u %s\n",
           m->name, (unsigned)N, (unsigned)comp_size, (unsigned)deco_size,
           ok ? "OK" : "FAIL");
    return ok ? 0 : 1;
}

/* --------------------------------------------------------------- speed ---- */

static double seconds_now(void)
{
    LARGE_INTEGER freq, counter;
    QueryPerformanceFrequency(&freq);
    QueryPerformanceCounter(&counter);
    return (double)counter.QuadPart / (double)freq.QuadPart;
}

/* Time one pass repeatedly and keep the fastest; background load only ever
   makes a pass slower, so the fastest is the reproducible number */
static int measure(fn_pass pass, const unsigned char *src, size_t src_size,
                   unsigned char *dst, size_t dst_cap, size_t *dst_size,
                   double *secs)
{
    double start = seconds_now(), best = 0.0, total;
    unsigned passes = 0, since_best = 0;

    for (;;) {
        double t0 = seconds_now(), elapsed;
        if (pass(src, src_size, dst, dst_cap, dst_size))
            return 1;
        elapsed = seconds_now() - t0;
        if (passes == 0 || elapsed < best) { best = elapsed; since_best = 0; }
        else since_best++;
        passes++;

        total = seconds_now() - start;
        if (passes < MIN_PASS) continue;
        if (total >= MAX_SECS) break;
        if (total >= MIN_SECS && since_best >= SETTLED) break;
    }

    *secs = best;
    return 0;
}

static int speed(const method *m, const unsigned char *orig,
                 unsigned char *comp, unsigned char *deco)
{
    size_t comp_size, deco_size;
    double comp_secs, deco_secs;

    if (measure(method_compress(m), orig, SPEED_N, comp, COMPCAP, &comp_size, &comp_secs)) {
        printf("  %-11s compress FAILED\n", m->name);
        return 1;
    }
    if (measure(m->decompress, comp, comp_size, deco, SPEED_N, &deco_size, &deco_secs)) {
        printf("  %-11s decompress FAILED\n", m->name);
        return 1;
    }

    if (deco_size != SPEED_N || memcmp(orig, deco, SPEED_N) != 0) {
        printf("  %-11s data mismatch\n", m->name);
        return 1;
    }

    printf("  %-11s ratio=%.2f compress=%7.1f MB/s decompress=%7.1f MB/s\n",
           m->name, (double)SPEED_N / comp_size,
           SPEED_N / MB / comp_secs, SPEED_N / MB / deco_secs);
    return 0;
}

/* ---------------------------------------------------------------- main ---- */

int main(void)
{
    unsigned char *orig = (unsigned char *)malloc(SPEED_N);
    unsigned char *comp = (unsigned char *)malloc(COMPCAP);
    unsigned char *deco = (unsigned char *)malloc(SPEED_N);
    size_t i;
    int fails = 0;

    setvbuf(stdout, NULL, _IONBF, 0);
    if (!orig || !comp || !deco) { printf("buffer allocation failed\n"); return 2; }
    make_buffer(orig, SPEED_N);

    if (dll_load()) { printf("\n*** DLL COULD NOT BE LOADED ***\n"); return 1; }

    printf("Round-trips (%u bytes)\n", (unsigned)N);
    for (i = 0; i < NMETHODS; i++) {
        const method *m = &methods[i];
        if (method_start(m)) {
            printf("  %-11s start FAILED\n", m->name);
            method_stop(m);
            fails++;
            continue;
        }
        fails += roundtrip(m, orig, comp, deco);
        fails += method_stop(m);
    }
    if (fails) { printf("\n*** SOME ROUND-TRIPS FAILED ***\n"); return 1; }

    printf("\nSpeed (%u MB, lzma2, high priority)\n", (unsigned)(SPEED_N / 1000000u));
    if (!SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS))
        printf("  note: could not raise process priority (%lu)\n", GetLastError());
    SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
    for (i = 0; i < NMETHODS; i++) {
        const method *m = &methods[i];
        if (!m->lzma2)
            continue;
        if (method_start(m)) {
            printf("  %-11s start FAILED\n", m->name);
            method_stop(m);
            fails++;
            continue;
        }
        fails += speed(m, orig, comp, deco);
        fails += method_stop(m);
    }
    SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_NORMAL);
    SetPriorityClass(GetCurrentProcess(), NORMAL_PRIORITY_CLASS);

    free(orig); free(comp); free(deco);

    printf("\n%s\n", fails ? "*** SOME TESTS FAILED ***" : "All tests OK");
    return fails ? 1 : 0;
}
