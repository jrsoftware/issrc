#include <windows.h>
#include <commctrl.h>

#if defined(_X86_)
	#define ARCHNOTE TEXT("\n\n(This EXE was compiled for the x86 architecture.)")
#elif defined(_AMD64_)
	#define ARCHNOTE TEXT("\n\n(This EXE was compiled for the x64 architecture.)")
#elif defined(_M_ARM64)
	#define ARCHNOTE TEXT("\n\n(This EXE was compiled for the Arm64 architecture.)")
#else
	#error unknown arch
#endif

int WinMainCRTStartup(void)
{
	// Work around bug in Windows XP Gold & SP1: If the application manifest
	// specifies COMCTL32.DLL version 6.0 (to enable visual styles), we must
	// call InitCommonControls() to ensure that we actually link to
	// COMCTL32.DLL, otherwise calls to MessageBox() fail. (XP SP2 appears
	// to fix this.)
	InitCommonControls();

	MessageBox(NULL, TEXT("Welcome to My Program.") ARCHNOTE,
		TEXT("Hello"), MB_OK);

	MessageBox(NULL, TEXT("Thank you for using My Program.\n\n")
		TEXT("(You can uninstall this by going to Add/Remove Programs in Control Panel.)"),
		TEXT("Goodbye"), MB_OK);

	ExitProcess(0);
	return 0;
}