/*
  Inno Setup
  Copyright (C) 1997-2012 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  RegDLL process, called by RegDLL unit

  Compiled on Visual Studio 2005
*/

#include <windows.h>
#include <commctrl.h>

typedef struct {
	DWORD Version;
	DWORD Size;
	DWORD Result;
	DWORD ResultCode;
	BOOL Unregister;
	BOOL FailCriticalErrors;
	CHAR Filename[4096];
	CHAR Directory[4096];
} REG_PROCESS_DATA;

#define REG_PROCESS_DATA_VERSION 3

// REG_PROCESS_DATA Result values
#define rrOleInitializeFailed 1
#define rrLoadLibraryFailed 2
#define rrGetProcAddressFailed 3
#define rrDllFunctionCalled 4

// Process exit codes, full DWORD
#define ecSuccess 0x1C9B28DA

// Process exit codes, high word
#define ecMapFailed 1
#define ecMutexWaitFailed 2
#define ecMutexWaitUnexpectedResult 3
#define ecWrongVersionOrSize 4

LPTSTR GetCommandTail(void)
{
	LPTSTR CmdLine = GetCommandLine();
	BOOL InQuote = FALSE;
	while (*CmdLine) {
		if (!InQuote && *CmdLine <= ' ') break;
		if (*CmdLine == '"') InQuote = !InQuote;
		CmdLine++;
	}
	while (*CmdLine && *CmdLine <= ' ') {
		CmdLine++;
	}
	return CmdLine;
}

LPTSTR ExtractHandleValue(LPTSTR CmdLine, HANDLE *OutHandle)
{
	UINT_PTR Result = 0;
	while (*CmdLine >= '0' && *CmdLine <= '9') {
		Result *= 10;
		Result += (*CmdLine - '0');
		CmdLine++;
	}
	while (*CmdLine && *CmdLine <= ' ') {
		CmdLine++;
	}
	*OutHandle = (HANDLE)Result;
	return CmdLine;
}

LONG Main(void)
{
	LPTSTR Args;
	HANDLE MapHandle, MutexHandle;
	REG_PROCESS_DATA *Data;
	DWORD WaitResult;
	HRESULT OleInitResult;
	HMODULE LibHandle;

	// Work around bug in Windows XP Gold & SP1: If the application manifest
	// specifies COMCTL32.DLL version 6.0 (to enable visual styles), we must
	// call InitCommonControls() to ensure that we actually link to
	// COMCTL32.DLL, otherwise calls to MessageBox() fail. (XP SP2 appears
	// to fix this.)
	InitCommonControls();

	// Extract the two handle values from the command line
	Args = GetCommandTail();
	Args = ExtractHandleValue(Args, &MapHandle);
	ExtractHandleValue(Args, &MutexHandle);

	// Map shared section
	Data = MapViewOfFile(MapHandle, FILE_MAP_WRITE, 0, 0, sizeof(*Data));
	if (Data == NULL) {
		return MAKELONG(GetLastError(), ecMapFailed);
	}

	// Acquire mutex
	WaitResult = WaitForSingleObject(MutexHandle, INFINITE);
	if (WaitResult == WAIT_FAILED) {
		return MAKELONG(GetLastError(), ecMutexWaitFailed);
	}
	if (WaitResult != WAIT_OBJECT_0) {
		return MAKELONG(WaitResult, ecMutexWaitUnexpectedResult);
	}

	// Check structure version number and size
	if (Data->Version != REG_PROCESS_DATA_VERSION ||
		Data->Size != sizeof(*Data)) {
		return MAKELONG(0, ecWrongVersionOrSize);
	}

	// Initialize OLE. Regsvr32 does this, and it's needed when registering
	// comcat.dll on stock Windows 98. Its DllRegisterServer export
	// forwards to ole32.dll; if ole32.dll isn't loaded, GetProcAddress
	// fails.
	OleInitResult = OleInitialize(NULL);
	if (SUCCEEDED(OleInitResult)) {
		// Set error mode
		SetErrorMode(Data->FailCriticalErrors ?
			SEM_NOOPENFILEERRORBOX | SEM_FAILCRITICALERRORS :
			SEM_NOOPENFILEERRORBOX);

		// Initialize current directory
		if (*Data->Directory) {
			SetCurrentDirectoryA(Data->Directory);
		}

		// Load & register the DLL
		LibHandle = LoadLibraryExA(Data->Filename, NULL, LOAD_WITH_ALTERED_SEARCH_PATH);
		if (LibHandle) {
			typedef HRESULT (STDAPICALLTYPE *DLLREGISTERSERVER)(void);
			DLLREGISTERSERVER RegisterServerProc;

			RegisterServerProc = (DLLREGISTERSERVER)GetProcAddress(LibHandle,
				Data->Unregister ? "DllUnregisterServer" : "DllRegisterServer");
			if (RegisterServerProc) {
				Data->ResultCode = (*RegisterServerProc)();
				Data->Result = rrDllFunctionCalled;
			} else {
				Data->ResultCode = GetLastError();
				Data->Result = rrGetProcAddressFailed;
			}

			FreeLibrary(LibHandle);
		} else {
			Data->ResultCode = GetLastError();
			Data->Result = rrLoadLibraryFailed;
		}

		OleUninitialize();
	} else {
		Data->ResultCode = OleInitResult;
		Data->Result = rrOleInitializeFailed;
	}

	// Release the mutex after all changes have been made to the record. This
	// should impose a "release" memory barrier on architectures that require
	// it.	
	ReleaseMutex(MutexHandle);

	return ecSuccess;
}

int WinMainCRTStartup(void)
{
	LONG Result = Main();

	// Good idea to call ExitProcess because it'll terminate any other
	// threads the system might've created. A simple "return" won't.
	ExitProcess(Result);

	return Result;
}