/*
  Inno Setup
  Copyright (C) 1997-2008 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  64-bit helper process

  Compiled on Visual Studio 2005 SP1
  Tested on x64 and IA-64 architectures (Athlon 64 and Merced specifically).
*/

#define _WIN32_IE 0x0600
#include <windows.h>
#include <commctrl.h>
#include <shlwapi.h>
#include <aclapi.h>

#ifndef UNICODE
#error UNICODE isn't defined
#endif

// As of Inno Setup 5.1.11, the helper is no longer used to register
// DLLs, so avoid linking in support for REQUEST_REGISTER_SERVER.
#undef IMPLEMENT_REGISTER_SERVER

#define HELPER_VERSION 105

// Request commands
#define REQUEST_PING 1
#define REQUEST_GRANT_PERMISSION 2
#ifdef IMPLEMENT_REGISTER_SERVER
#define REQUEST_REGISTER_SERVER 3
#endif
#define REQUEST_REGISTER_TYPE_LIBRARY 4

// These must be kept in synch with Struct.pas:
typedef struct {
	SID_IDENTIFIER_AUTHORITY Authority;
	BYTE SubAuthCount;
	DWORD SubAuth[2];
} TGrantPermissionSid;
typedef struct {
	TGrantPermissionSid Sid;
    DWORD AccessMask;
} TGrantPermissionEntry;

// This value must be kept in synch with Struct.pas:
#define MAX_GRANT_PERMISSION_ENTRIES 32

// These must be kept in synch with Helper.pas:
typedef struct {
	DWORD ObjectType;
	DWORD EntryCount;
	DWORD Inheritance;
	WCHAR ObjectName[4096];
	TGrantPermissionEntry Entries[MAX_GRANT_PERMISSION_ENTRIES];
} REQUEST_GRANT_PERMISSION_DATA;

typedef struct {
	BOOL Unregister;
	BOOL FailCriticalErrors;
	WCHAR Filename[4096];
	WCHAR Directory[4096];
} REQUEST_REGISTER_SERVER_DATA;

typedef struct {
	BOOL Unregister;
	WCHAR Filename[4096];
} REQUEST_REGISTER_TYPE_LIBRARY_DATA;

typedef struct {
	DWORD SequenceNumber;
	DWORD Command;
	DWORD DataSize;
	union {
		BYTE Data[65536];
		REQUEST_GRANT_PERMISSION_DATA GrantPermissionData;
		REQUEST_REGISTER_SERVER_DATA RegisterServerData;
		REQUEST_REGISTER_TYPE_LIBRARY_DATA RegisterTypeLibraryData;
	};
} REQUEST_DATA;

typedef struct {
	DWORD SequenceNumber;
	DWORD StatusCode;
	DWORD ErrorCode;
	DWORD DataSize;
	BYTE Data[65536];
} RESPONSE_DATA;

static TCHAR SystemDir[MAX_PATH+1];

static DWORD GrantPermission(const DWORD ObjectType, const LPWSTR ObjectName,
	TGrantPermissionEntry *Entries, const DWORD EntryCount,
	const DWORD Inheritance)
{
	DWORD ErrorCode;
	PSECURITY_DESCRIPTOR SD;
	PACL Dacl, NewDacl;
	EXPLICIT_ACCESSW ExplicitAccess[MAX_GRANT_PERMISSION_ENTRIES];
	DWORD I;

	// Note: SecureZeroMemory is used because memset/ZeroMemory aren't available
	SecureZeroMemory(&ExplicitAccess, sizeof(ExplicitAccess));

	if (EntryCount > ARRAYSIZE(ExplicitAccess)) {
		return ERROR_INVALID_PARAMETER;
	}

	ErrorCode = GetNamedSecurityInfoW(ObjectName, ObjectType,
		DACL_SECURITY_INFORMATION, NULL, NULL, &Dacl, NULL, &SD);
	if (ErrorCode != ERROR_SUCCESS) {
		return ErrorCode;
	}

	// From here on, use "goto out" instead of "return"

	for (I = 0; I < EntryCount; I++) {
		TGrantPermissionEntry *E = &Entries[I];
		PSID Sid;

		if (!AllocateAndInitializeSid(&E->Sid.Authority, E->Sid.SubAuthCount,
				E->Sid.SubAuth[0], E->Sid.SubAuth[1], 0, 0, 0, 0, 0, 0, &Sid)) {
			ErrorCode = GetLastError();
			if (ErrorCode == 0) ErrorCode = ERROR_INVALID_PARAMETER;  // just in case
			goto out;
		}
		ExplicitAccess[I].grfAccessPermissions = E->AccessMask;
		ExplicitAccess[I].grfAccessMode = GRANT_ACCESS;
		ExplicitAccess[I].grfInheritance = Inheritance;
		ExplicitAccess[I].Trustee.TrusteeForm = TRUSTEE_IS_SID;
		ExplicitAccess[I].Trustee.TrusteeType = TRUSTEE_IS_UNKNOWN;
		ExplicitAccess[I].Trustee.ptstrName = Sid;
	}

	ErrorCode = SetEntriesInAclW(EntryCount, ExplicitAccess, Dacl, &NewDacl);
	if (ErrorCode == ERROR_SUCCESS) {
		ErrorCode = SetNamedSecurityInfoW(ObjectName, ObjectType,
			DACL_SECURITY_INFORMATION, NULL, NULL, NewDacl, NULL);
		LocalFree(NewDacl);
	}

out:
	for (I = 0; I < EntryCount; I++) {
		PSID Sid = ExplicitAccess[I].Trustee.ptstrName;
		if (Sid) FreeSid(Sid);
	}
	LocalFree(SD);

	return ErrorCode;
}

#ifdef IMPLEMENT_REGISTER_SERVER
static DWORD RegisterServer(const BOOL Unregister, const LPWSTR Filename,
	const LPWSTR Directory, const BOOL FailCriticalErrors, DWORD *ErrorCode)
{
	DWORD retval = 0;
	HRESULT OleInitResult;
	UINT SaveErrorMode;
	HANDLE LibHandle;

	// Initialize OLE. Regsvr32 does this.
	OleInitResult = OleInitialize(NULL);
	if (FAILED(OleInitResult)) {
		*ErrorCode = OleInitResult;
		return 4;
	}
	
	SaveErrorMode = SetErrorMode(FailCriticalErrors ?
		SEM_NOOPENFILEERRORBOX | SEM_FAILCRITICALERRORS :
		SEM_NOOPENFILEERRORBOX);

	// If no directory was supplied, we use the system directory.
	// Relative paths are assumed to be relative to the system directory.
	SetCurrentDirectory(SystemDir);
	if (*Directory) {
		SetCurrentDirectoryW(Directory);
	}

	LibHandle = LoadLibraryExW(Filename, NULL, LOAD_WITH_ALTERED_SEARCH_PATH);
	if (LibHandle) {
		typedef HRESULT (STDAPICALLTYPE *DLLREGISTERSERVER)(void);
		DLLREGISTERSERVER RegisterServerProc;

		RegisterServerProc = (DLLREGISTERSERVER)GetProcAddress(LibHandle,
			Unregister ? "DllUnregisterServer" : "DllRegisterServer");
		if (RegisterServerProc) {
			*ErrorCode = (*RegisterServerProc)();
			retval = 3;
		} else {
			*ErrorCode = GetLastError();
			retval = 2;
		}

		FreeLibrary(LibHandle);
	} else {
		*ErrorCode = GetLastError();
		retval = 1;
	}

	// Revert the current directory and error mode changes
	SetCurrentDirectory(SystemDir);
	SetErrorMode(SaveErrorMode);

	OleUninitialize();

	return retval;
}
#endif

static DWORD RegisterTypeLibrary(const BOOL Unregister,
	const LPWSTR Filename, DWORD *ErrorCode)
{
	DWORD retval;
	HRESULT hr;
	ITypeLib *TypeLib;
	TLIBATTR *LibAttr;

	retval = 1;
	hr = LoadTypeLib(Filename, &TypeLib);
	if (hr == S_OK) {
		if (!Unregister) {
			retval = 2;
			hr = RegisterTypeLib(TypeLib, Filename, NULL);			
		} else {
			retval = 3;
			hr = TypeLib->lpVtbl->GetLibAttr(TypeLib, &LibAttr);
			if (hr == S_OK) {
				retval = 4;
				hr = UnRegisterTypeLib(&LibAttr->guid, LibAttr->wMajorVerNum,
					LibAttr->wMinorVerNum, LibAttr->lcid, LibAttr->syskind);
				TypeLib->lpVtbl->ReleaseTLibAttr(TypeLib, LibAttr);
			}
		}
		TypeLib->lpVtbl->Release(TypeLib);
	}

	*ErrorCode = hr;
	return retval;
}

static void ProcessRequest(REQUEST_DATA *request, RESPONSE_DATA *response)
{
	response->SequenceNumber = request->SequenceNumber;
	response->StatusCode = 0;  // 0 means "didn't execute command"
	response->ErrorCode = 0;
	response->DataSize = 0;

	switch (request->Command) {
		case REQUEST_PING:
			response->StatusCode = 1;
			break;
		case REQUEST_GRANT_PERMISSION:
			{
				// response:
				//   StatusCode    1 if request was valid
				//   ErrorCode     Error code from GrantPermission()
				
				REQUEST_GRANT_PERMISSION_DATA *data = &request->GrantPermissionData;

				if (request->DataSize == sizeof(*data) &&
					data->EntryCount <= ARRAYSIZE(data->Entries)) {
					response->ErrorCode = GrantPermission(data->ObjectType,
						data->ObjectName, data->Entries, data->EntryCount,
						data->Inheritance);
					response->StatusCode = 1;
				}
			}
			break;
#ifdef IMPLEMENT_REGISTER_SERVER
		case REQUEST_REGISTER_SERVER:
			{
				// response:
				//   StatusCode    1 if LoadLibrary failed,
				//                 2 if GetProcAddress failed,
				//                 3 if Dll(Un)RegisterServer called; possibly succeeded,
				//                 4 if OleInitialize failed
				//   ErrorCode     Error code or HRESULT depending on the StatusCode

				REQUEST_REGISTER_SERVER_DATA *data = &request->RegisterServerData;

				if (request->DataSize == sizeof(*data)) {
					response->StatusCode = RegisterServer(data->Unregister,
						data->Filename, data->Directory, data->FailCriticalErrors,
						&response->ErrorCode);
				}
			}
			break;
#endif
		case REQUEST_REGISTER_TYPE_LIBRARY:
			{
				// response:
				//   StatusCode    1 if LoadTypeLib didn't return S_OK
				//                 Register only:
				//                   2 if RegisterTypeLib called; possibly succeeded
				//                 Unregister only:
				//                   3 if ITypeLib::GetLibAttr didn't return S_OK
				//                   4 if UnRegisterTypeLib called; possibly succeeded
				//   ErrorCode     An HRESULT

				REQUEST_REGISTER_TYPE_LIBRARY_DATA *data = &request->RegisterTypeLibraryData;

				if (request->DataSize == sizeof(*data)) {
					response->StatusCode = RegisterTypeLibrary(data->Unregister,
						data->Filename, &response->ErrorCode);
				}
			}
			break;
	}
}

static BOOL WINAPI ConsoleCtrlHandlerRoutine(DWORD dwCtrlType)
{
	// By default, during shutdown, applications without windows are killed
	// unconditionally. This handler suppresses that default handling, and
	// just ignores the termination request.
	//
	// Note: This can cause a "Process not responding" dialog to appear, but
	// that's still better than dying unconditionally. I don't think it's
	// possible to programmatically abort the shutdown sequence in a
	// non-windowed application. (Windowed applications, of course, can
	// return FALSE in response to WM_QUERYENDSESSION. But we have no message
	// loop so creating a window is not an option.)

	return TRUE;
}

/*
void Test(void)
{
	TGrantPermissionEntry Entry = { 0 };
	Entry.Sid.Authority.Value[5] = 1;
	Entry.Sid.SubAuthCount = 1;
	Entry.AccessMask = 0x1200A9;
	GrantPermission(SE_FILE_OBJECT, "c:\\testfile.txt", &Entry, 1, 0);
}
*/

int Main(void)
{
	int argc;
	LPWSTR* argv;
	LONGLONG llHandle;
	HANDLE hPipe;
	LONG retval = 0;
	static REQUEST_DATA request;
	static RESPONSE_DATA response;

	// Work around bug in Windows XP Gold & SP1: If the application manifest
	// specifies COMCTL32.DLL version 6.0 (to enable visual styles), we must
	// call InitCommonControls() to ensure that we actually link to
	// COMCTL32.DLL, otherwise calls to MessageBox() fail. (XP SP2 appears
	// to fix this.)
	// NOTE: This workaround was brought over from RegDLL for consistency,
	// but may not actually be needed here since Setup (currently) refuses
	// to spawn Helper on pre-5.02 SP1 versions of Windows.
	InitCommonControls();

	SetErrorMode(SEM_FAILCRITICALERRORS);

	GetSystemDirectory(SystemDir, ARRAYSIZE(SystemDir));
	SetCurrentDirectory(SystemDir);

	// Give us a lower-than-default shutdown priority so that if for some
	// reason a shutdown is initiated while we're running, Windows
	// "shouldn't" try to kill us before Setup.
	SetProcessShutdownParameters(0x100, 0);

	SetConsoleCtrlHandler(ConsoleCtrlHandlerRoutine, TRUE);

	//
	// Get version and message-mode pipe handle from command line
	//

	argv = CommandLineToArgvW(GetCommandLineW(), &argc);
	if (argv == NULL) {
		return MAKELONG(GetLastError(), 1);
	}
	if (argc != 3) {
		return MAKELONG(0, 2);
	}
	if (StrToIntW(argv[1]) != HELPER_VERSION) {
		return MAKELONG(0, 3);
	}
	if (!StrToInt64ExW(argv[2], STIF_SUPPORT_HEX, &llHandle)) {
		return MAKELONG(0, 4);
	}
	hPipe = (HANDLE)(INT_PTR)llHandle;

	//
	// Wait for and process incoming requests
	//

	while (TRUE) {
		DWORD dwBytesRead, dwResponseLength, dwBytesWritten;

		if (!ReadFile(hPipe, &request, sizeof(request), &dwBytesRead, NULL)) {
			// We can normally expect ReadFile to fail with ERROR_BROKEN_PIPE
			// when the client has disconnected.
			// Note: We don't bother handling ERROR_MORE_DATA because the client
			// shouldn't be sending us oversized requests in the first place.
			if (GetLastError() != ERROR_BROKEN_PIPE) {
				retval = MAKELONG(GetLastError(), 5);
			}
			break;
		}
		if (dwBytesRead < FIELD_OFFSET(REQUEST_DATA, Data) ||
			request.DataSize != dwBytesRead - FIELD_OFFSET(REQUEST_DATA, Data)) {
			// Request message is too short or too long
			retval = MAKELONG(0, 6);
			break;
		}

		ProcessRequest(&request, &response);

		dwResponseLength = FIELD_OFFSET(RESPONSE_DATA, Data) + response.DataSize;
		if (!WriteFile(hPipe, &response, dwResponseLength, &dwBytesWritten, NULL)) {
			// WriteFile could fail if the client disconnected for some reason.
			retval = MAKELONG(GetLastError(), 7);
			break;
		}
		if (dwBytesWritten != dwResponseLength) {
			// Should never get here.
			retval = MAKELONG(0, 8);
			break;
		}
	}

	CloseHandle(hPipe);

	return retval;
}

int mainCRTStartup(void)
{
	int retval;

	retval = Main();
	
	// Good idea to call ExitProcess because it'll terminate any other
	// threads the system might've created. A simple "return" won't.
	ExitProcess(retval);

	return retval;
}