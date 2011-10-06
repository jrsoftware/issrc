{ Original file taken from "Windows 7 Controls for Delphi" by Daniel Wischnewski
  http://www.gumpi.com/Blog/2009/01/20/Alpha1OfWindows7ControlsForDelphi.aspx
  MPL licensed } 

{ D2/D3 support and correct IID consts added by Martijn Laan for Inno Setup }

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  This unit provides the ITaskbarList3 interface for Windows 7 taskbar progress indicators.

  $jrsoftware: issrc/Components/dwTaskbarList.pas,v 1.5 2010/10/21 02:14:14 jr Exp $
}

{$IFDEF VER90}
  {$DEFINE DELPHI2}
{$ENDIF}

unit dwTaskbarList;

interface

uses
  Windows {$IFDEF DELPHI2}, OLE2 {$ENDIF};

const
  CLSID_TaskbarList: TGUID = (
    D1:$56FDF344; D2:$FD6D; D3:$11D0; D4:($95,$8A,$00,$60,$97,$C9,$A0,$90));
  IID_TaskbarList: TGUID = (
    D1:$56FDF342; D2:$FD6D; D3:$11D0; D4:($95,$8A,$00,$60,$97,$C9,$A0,$90));
  IID_TaskbarList2: TGUID = (
    D1:$602D4995; D2:$B13A; D3:$429B; D4:($A6,$6E,$19,$35,$E4,$4F,$43,$17));
  IID_TaskbarList3: TGUID = (
    D1:$EA1AFB91; D2:$9E28; D3:$4B86; D4:($90,$E9,$9E,$9F,$8A,$5E,$EF,$AF));

const
  THBF_ENABLED = $0000;
  THBF_DISABLED = $0001;
  THBF_DISMISSONCLICK = $0002;
  THBF_NOBACKGROUND = $0004;
  THBF_HIDDEN = $0008;

const
  THB_BITMAP = $0001;
  THB_ICON = $0002;
  THB_TOOLTIP = $0004;
  THB_FLAGS = $0008;

const
  THBN_CLICKED = $1800;

const
  TBPF_NOPROGRESS = $00;
  TBPF_INDETERMINATE = $01;
  TBPF_NORMAL = $02;
  TBPF_ERROR = $04;
  TBPF_PAUSED = $08;

const
  TBATF_USEMDITHUMBNAIL: DWORD = $00000001;
  TBATF_USEMDILIVEPREVIEW: DWORD = $00000002;

const
  WM_DWMSENDICONICTHUMBNAIL = $0323;
  WM_DWMSENDICONICLIVEPREVIEWBITMAP = $0326;

type
  TTipString = array[0..259] of WideChar;
  PTipString = ^TTipString;
  tagTHUMBBUTTON = packed record
    dwMask: DWORD;
    iId: UINT;
    iBitmap: UINT;
    hIcon: HICON;
    szTip: TTipString;
    dwFlags: DWORD;
  end;
  THUMBBUTTON = tagTHUMBBUTTON;
  THUMBBUTTONLIST = ^THUMBBUTTON;

  dwInteger64 = record
    Lo, Hi: Cardinal;
  end;

type
{$IFDEF DELPHI2}
  ITaskbarList = class(IUnknown)
    function HrInit: HRESULT; virtual; stdcall; abstract;
    function AddTab(hwnd: Cardinal): HRESULT; virtual; stdcall; abstract;
    function DeleteTab(hwnd: Cardinal): HRESULT; virtual; stdcall; abstract;
    function ActivateTab(hwnd: Cardinal): HRESULT; virtual; stdcall; abstract;
    function SetActiveAlt(hwnd: Cardinal): HRESULT; virtual; stdcall; abstract;
  end;

  ITaskbarList2 = class(ITaskbarList)
    function MarkFullscreenWindow(hwnd: Cardinal; fFullscreen: Bool): HRESULT; virtual; stdcall; abstract;
  end;

  ITaskbarList3 = class(ITaskbarList2)
    function SetProgressValue(hwnd: Cardinal; ullCompleted, ullTotal: dwInteger64): HRESULT; virtual; stdcall; abstract;
    function SetProgressState(hwnd: Cardinal; tbpFlags: DWORD): HRESULT; virtual; stdcall; abstract;
    function RegisterTab(hwndTab: Cardinal; hwndMDI: Cardinal): HRESULT; virtual; stdcall; abstract;
    function UnregisterTab(hwndTab: Cardinal): HRESULT; virtual; stdcall; abstract;
    function SetTabOrder(hwndTab: Cardinal; hwndInsertBefore: Cardinal): HRESULT; virtual; stdcall; abstract;
    function SetTabActive(hwndTab: Cardinal; hwndMDI: Cardinal; tbatFlags: DWORD): HRESULT; virtual; stdcall; abstract;
    function ThumbBarAddButtons(hwnd: Cardinal; cButtons: UINT; Button: THUMBBUTTONLIST): HRESULT; virtual; stdcall; abstract;
    function ThumbBarUpdateButtons(hwnd: Cardinal; cButtons: UINT; pButton: THUMBBUTTONLIST): HRESULT; virtual; stdcall; abstract;
    function ThumbBarSetImageList(hwnd: Cardinal; himl: Cardinal): HRESULT; virtual; stdcall; abstract;
    function SetOverlayIcon(hwnd: Cardinal; hIcon: HICON; pszDescription: LPCWSTR): HRESULT; virtual; stdcall; abstract;
    function SetThumbnailTooltip(hwnd: Cardinal; pszTip: LPCWSTR): HRESULT; virtual; stdcall; abstract;
    function SetThumbnailClip(hwnd: Cardinal; prcClip: PRect): HRESULT; virtual; stdcall; abstract;
  end;
{$ELSE}
  ITaskbarList = interface
    ['{56FDF342-FD6D-11D0-958A-006097C9A090}']
    function HrInit: HRESULT; stdcall;
    function AddTab(hwnd: Cardinal): HRESULT; stdcall;
    function DeleteTab(hwnd: Cardinal): HRESULT; stdcall;
    function ActivateTab(hwnd: Cardinal): HRESULT; stdcall;
    function SetActiveAlt(hwnd: Cardinal): HRESULT; stdcall;
  end;

  ITaskbarList2 = interface(ITaskbarList)
    ['{602D4995-B13A-429B-A66E-1935E44F4317}']
    function MarkFullscreenWindow(hwnd: Cardinal; fFullscreen: Bool): HRESULT; stdcall;
  end;

  ITaskbarList3 = interface(ITaskbarList2)
    ['{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}']
    function SetProgressValue(hwnd: Cardinal; ullCompleted, ullTotal: dwInteger64): HRESULT; stdcall;
    function SetProgressState(hwnd: Cardinal; tbpFlags: DWORD): HRESULT; stdcall;
    function RegisterTab(hwndTab: Cardinal; hwndMDI: Cardinal): HRESULT; stdcall;
    function UnregisterTab(hwndTab: Cardinal): HRESULT; stdcall;
    function SetTabOrder(hwndTab: Cardinal; hwndInsertBefore: Cardinal): HRESULT; stdcall;
    function SetTabActive(hwndTab: Cardinal; hwndMDI: Cardinal; tbatFlags: DWORD): HRESULT; stdcall;
    function ThumbBarAddButtons(hwnd: Cardinal; cButtons: UINT; Button: THUMBBUTTONLIST): HRESULT; stdcall;
    function ThumbBarUpdateButtons(hwnd: Cardinal; cButtons: UINT; pButton: THUMBBUTTONLIST): HRESULT; stdcall;
    function ThumbBarSetImageList(hwnd: Cardinal; himl: Cardinal): HRESULT; stdcall;
    function SetOverlayIcon(hwnd: Cardinal; hIcon: HICON; pszDescription: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailTooltip(hwnd: Cardinal; pszTip: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailClip(hwnd: Cardinal; prcClip: PRect): HRESULT; stdcall;
  end;
{$ENDIF}

implementation

end.
