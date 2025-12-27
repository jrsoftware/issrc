#include <windows.h>

void __stdcall MyDllFunc(HWND hWnd, wchar_t *lpText, wchar_t *lpCaption, UINT uType)
{
  MessageBox(hWnd, lpText, lpCaption, uType);
}