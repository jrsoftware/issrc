unit RichEditViewer;

{ TRichEditViewer by Jordan Russell and Martijn Laan

  Known problem:
  If, after assigning rich text to a TRichEditViewer component, you change
  a property that causes the component's handle to be recreated, all text
  formatting will be lost (in the interests of code size).
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RichEdit, ActiveX;

type
  IRichEditOleCallback = interface(IUnknown)
    ['{00020d03-0000-0000-c000-000000000046}']
    function GetNewStorage(out stg: IStorage): HResult; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
      out Doc: IOleInPlaceUIWindow;
      lpFrameInfo: POleInPlaceFrameInfo): HResult; stdcall;
    function ShowContainerUI(fShow: BOOL): HResult; stdcall;
    function QueryInsertObject(const clsid: TCLSID; const stg: IStorage;
      cp: Longint): HResult; stdcall;
    function DeleteObject(const oleobj: IOleObject): HResult; stdcall;
    function QueryAcceptData(const dataobj: IDataObject;
      var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL;
      hMetaPict: HGLOBAL): HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HResult; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
      var dwEffect: DWORD): HResult; stdcall;
    function GetContextMenu(seltype: Word; const oleobj: IOleObject;
      const chrg: TCharRange; out menu: HMENU): HResult; stdcall;
  end;
  
  TRichEditViewerCustomShellExecute = procedure(hWnd: HWND; Operation, FileName, Parameters, Directory: LPWSTR; ShowCmd: Integer); stdcall;
  
  TRichEditViewer = class(TMemo)
  private
    class var
      FCustomShellExecute: TRichEditViewerCustomShellExecute;
    var
      FUseRichEdit: Boolean;
      FRichEditLoaded: Boolean;
      FCallback: IRichEditOleCallback;
    procedure SetRTFTextProp(const Value: AnsiString);
    procedure SetUseRichEdit(Value: Boolean);
    procedure UpdateBackgroundColor;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SetRTFText(const Value: AnsiString): Integer;
    property RTFText: AnsiString write SetRTFTextProp;
    class property CustomShellExecute: TRichEditViewerCustomShellExecute read FCustomShellExecute write FCustomShellExecute;
  published
    property UseRichEdit: Boolean read FUseRichEdit write SetUseRichEdit default True;
  end;

procedure Register;

implementation

uses
  ShellApi, BidiUtils, PathFunc, ComObj;

const
  RICHEDIT_CLASSW = 'RichEdit20W';
  MSFTEDIT_CLASS = 'RICHEDIT50W';
  EM_AUTOURLDETECT = WM_USER + 91;
  ENM_LINK = $04000000;
  EN_LINK = $070b;

type
 { Basic implementation of IRichEditOleCallback to enable the viewing of images and other objects. }
  TBasicRichEditOleCallback = class(TInterfacedObject, IRichEditOleCallback)
  public
    function GetNewStorage(out stg: IStorage): HResult; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
      out Doc: IOleInPlaceUIWindow;
      lpFrameInfo: POleInPlaceFrameInfo): HResult; stdcall;
    function ShowContainerUI(fShow: BOOL): HResult; stdcall;
    function QueryInsertObject(const clsid: TCLSID; const stg: IStorage;
      cp: Longint): HResult; stdcall;
    function DeleteObject(const oleobj: IOleObject): HResult; stdcall;
    function QueryAcceptData(const dataobj: IDataObject;
      var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL;
      hMetaPict: HGLOBAL): HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HResult; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
      var dwEffect: DWORD): HResult; stdcall;
    function GetContextMenu(seltype: Word; const oleobj: IOleObject;
      const chrg: TCharRange; out menu: HMENU): HResult; stdcall;
  end;

  PEnLink = ^TEnLink;
  TENLink = record
    nmhdr: TNMHdr;
    msg: UINT;
    wParam: WPARAM;
    lParam: LPARAM;
    chrg: TCharRange;
  end;

  TTextRange = record
    chrg: TCharRange;
    lpstrText: PWideChar;
  end;

var
  RichEditModule: HMODULE;
  RichEditUseCount: Integer = 0;
  RichEditVersion: Integer;

procedure LoadRichEdit;

  function GetSystemDir: String;
  var
    Buf: array[0..MAX_PATH-1] of Char;
  begin
    GetSystemDirectory(Buf, SizeOf(Buf) div SizeOf(Buf[0]));
    Result := StrPas(Buf);
  end;

begin
  if RichEditUseCount = 0 then begin
    RichEditVersion := 4;
    RichEditModule := LoadLibrary(PChar(AddBackslash(GetSystemDir) + 'MSFTEDIT.DLL'));
    if RichEditModule = 0 then begin
      RichEditVersion := 2;
      RichEditModule := LoadLibrary(PChar(AddBackslash(GetSystemDir) + 'RICHED20.DLL'));
    end;
  end;
  Inc(RichEditUseCount);
end;

procedure UnloadRichEdit;
begin
  if RichEditUseCount > 0 then begin
    Dec(RichEditUseCount);
    if RichEditUseCount = 0 then begin
      FreeLibrary(RichEditModule);
      RichEditModule := 0;
    end;
  end;
end;

{ TBasicRichEditOleCallback }

function TBasicRichEditOleCallback.GetNewStorage(out stg: IStorage): HResult; stdcall;
var
  LockBytes: ILockBytes;
begin
  try
    OleCheck(CreateILockBytesOnHGlobal(0, True, LockBytes));
    OleCheck(StgCreateDocfileOnILockBytes(LockBytes, STGM_READWRITE
      or STGM_SHARE_EXCLUSIVE or STGM_CREATE, 0, stg));
    Result := S_OK;
  except
    Result := E_OUTOFMEMORY;
  end;
end;

function TBasicRichEditOleCallback.GetInPlaceContext(out Frame: IOleInPlaceFrame;
  out Doc: IOleInPlaceUIWindow; lpFrameInfo: POleInPlaceFrameInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TBasicRichEditOleCallback.ShowContainerUI(fShow: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;

function TBasicRichEditOleCallback.QueryInsertObject(const clsid: TCLSID; const stg: IStorage;
  cp: Longint): HResult;
begin
  Result := S_OK;
end;

function TBasicRichEditOleCallback.DeleteObject(const oleobj: IOleObject): HResult;
begin
  if Assigned(oleobj) then
    oleobj.Close(OLECLOSE_NOSAVE);
  Result := S_OK;
end;

function TBasicRichEditOleCallback.QueryAcceptData(const dataobj: IDataObject;
  var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL;
  hMetaPict: HGLOBAL): HResult;
begin
  Result := S_OK;
end;

function TBasicRichEditOleCallback.ContextSensitiveHelp(fEnterMode: BOOL): HResult;
begin
  Result := S_OK;
end;

function TBasicRichEditOleCallback.GetClipboardData(const chrg: TCharRange; reco: DWORD;
  out dataobj: IDataObject): HResult;
begin
  Result := E_NOTIMPL;
end;

function TBasicRichEditOleCallback.GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
  var dwEffect: DWORD): HResult;
begin
  Result := E_NOTIMPL;
end;

function TBasicRichEditOleCallback.GetContextMenu(seltype: Word;
  const oleobj: IOleObject; const chrg: TCharRange; out Menu: HMENU): HResult;
begin
  Result := E_NOTIMPL;
end;

{ TRichEditViewer }

constructor TRichEditViewer.Create(AOwner: TComponent);
begin
  inherited;
  FUseRichEdit := True;
  FCallback := TBasicRichEditOleCallback.Create;
end;

destructor TRichEditViewer.Destroy;
begin
  inherited;
  { First do all other deinitialization, then decrement the DLL use count }
  if FRichEditLoaded then begin
    FRichEditLoaded := False;
    UnloadRichEdit;
  end;
end;

procedure TRichEditViewer.CreateParams(var Params: TCreateParams);
{ Based on code from TCustomRichEdit.CreateParams }
begin
  if UseRichEdit and not FRichEditLoaded then begin
    { Increment the DLL use count when UseRichEdit is True, load the DLL }
    FRichEditLoaded := True;
    LoadRichEdit;
  end;
  inherited;
  if UseRichEdit then begin
    if RichEditVersion = 4 then
      CreateSubClass(Params, MSFTEDIT_CLASS)
    else
      CreateSubClass(Params, RICHEDIT_CLASSW);
  end else
    { Inherited handler creates a subclass of 'EDIT'.
      Must have a unique class name since it uses two different classes
      depending on the setting of the UseRichEdit property. }
    StrCat(Params.WinClassName, '/Text');  { don't localize! }
  SetBiDiStyles(Self, Params);
end;

procedure TRichEditViewer.CreateWnd;
var
  Mask: LongInt;
begin
  inherited;
  UpdateBackgroundColor;
  if FUseRichEdit then begin
    if RichEditVersion >= 2 then begin
      Mask := ENM_LINK or SendMessage(Handle, EM_GETEVENTMASK, 0, 0);
      SendMessage(Handle, EM_SETEVENTMASK, 0, LPARAM(Mask));
      SendMessage(Handle, EM_AUTOURLDETECT, WPARAM(True), 0);
    end;
    SendMessage(Handle, EM_SETOLECALLBACK, 0, LPARAM(FCallback));
  end;
end;

procedure TRichEditViewer.UpdateBackgroundColor;
begin
  if FUseRichEdit and HandleAllocated then
    SendMessage(Handle, EM_SETBKGNDCOLOR, 0, ColorToRGB(Color));
end;

procedure TRichEditViewer.SetUseRichEdit(Value: Boolean);
begin
  if FUseRichEdit <> Value then begin
    FUseRichEdit := Value;
    RecreateWnd;
    if not Value and FRichEditLoaded then begin
      { Decrement the DLL use count when UseRichEdit is set to False }
      FRichEditLoaded := False;
      UnloadRichEdit;
    end;
  end;
end;

type
  PStreamLoadData = ^TStreamLoadData;
  TStreamLoadData = record
    Buf: PByte;
    BytesLeft: Integer;
  end;

function StreamLoad(dwCookie: Longint; pbBuff: PByte;
  cb: Longint; var pcb: Longint): Longint; stdcall;
begin
  Result := 0;
  with PStreamLoadData(dwCookie)^ do begin
    if cb > BytesLeft then
      cb := BytesLeft;
    Move(Buf^, pbBuff^, cb);
    Inc(Buf, cb);
    Dec(BytesLeft, cb);
    pcb := cb;
  end;
end;

function TRichEditViewer.SetRTFText(const Value: AnsiString): Integer;

  function StreamIn(AFormat: WPARAM): Integer;
  var
    Data: TStreamLoadData;
    EditStream: TEditStream;
  begin
    Data.Buf := @Value[1];
    Data.BytesLeft := Length(Value);
    { Check for UTF-16 BOM }
    if (AFormat and SF_TEXT <> 0) and (Data.BytesLeft >= 2) and
       (PWord(Pointer(Value))^ = $FEFF) then begin
      AFormat := AFormat or SF_UNICODE;
      Inc(Data.Buf, 2);
      Dec(Data.BytesLeft, 2);
    end;
    EditStream.dwCookie := Longint(@Data);
    EditStream.dwError := 0;
    EditStream.pfnCallback := @StreamLoad;
    SendMessage(Handle, EM_STREAMIN, AFormat, LPARAM(@EditStream));
    Result := EditStream.dwError;
  end;

begin
  if not FUseRichEdit then begin
    Text := String(Value);
    Result := 0;
  end
  else begin
    SendMessage(Handle, EM_EXLIMITTEXT, 0, LParam($7FFFFFFE));
    Result := StreamIn(SF_RTF);
    if Result <> 0 then
      Result := StreamIn(SF_TEXT);
  end;
end;

procedure TRichEditViewer.SetRTFTextProp(const Value: AnsiString);
begin
  SetRTFText(Value);
end;

procedure TRichEditViewer.CMColorChanged(var Message: TMessage);
begin
  inherited;
  UpdateBackgroundColor;
end;

procedure TRichEditViewer.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  UpdateBackgroundColor;
end;

procedure TRichEditViewer.CNNotify(var Message: TWMNotify);
var
  EnLink: PEnLink;
  CharRange: TCharRange;
  TextRange: TTextRange;
  Len: Integer;
  URL: String;
begin
  case Message.NMHdr^.code of
    EN_LINK: begin
      EnLink := PEnLink(Message.NMHdr);
      if EnLink.msg = WM_LBUTTONUP then begin
        CharRange := EnLink.chrg;
        if (CharRange.cpMin >= 0) and (CharRange.cpMax > CharRange.cpMin) then begin
          Len := CharRange.cpMax - CharRange.cpMin;
          Inc(Len);  { for null terminator }
          if Len > 1 then begin
            SetLength(URL, Len);
            TextRange.chrg := CharRange;
            TextRange.lpstrText := PChar(URL);
            SetLength(URL, SendMessage(Handle, EM_GETTEXTRANGE, 0, LParam(@TextRange)));
            if URL <> '' then begin
              if Assigned(FCustomShellExecute) then
                FCustomShellExecute(Handle, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL)
              else
                ShellExecute(Handle, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('JR', [TRichEditViewer]);
end;

end.
