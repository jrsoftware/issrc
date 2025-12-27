library MyDll;

uses
  Windows;

procedure MyDllFunc(hWnd: HWND; lpText, lpCaption: PChar; uType: Cardinal); stdcall;
begin
  MessageBox(hWnd, lpText, lpCaption, uType);
end;

exports MyDllFunc;

begin
end.
