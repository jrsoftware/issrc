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
  {$IFDEF VCLSTYLES} Vcl.Themes, {$ELSE} Themes, {$ENDIF}
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
      cp: Integer): HResult; stdcall;
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
    class constructor Create;
    class destructor Destroy;
    procedure SetRTFTextProp(const Value: AnsiString);
    procedure SetUseRichEdit(Value: Boolean);
    procedure UpdateBackgroundColor;
    procedure RecolorAutoForegroundText(const NewTextColor: Integer);
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

  TRichEditViewerStyleHook = class(TScrollingStyleHook)
{$IFDEF VCLSTYLES}
  private
    procedure EMSetBkgndColor(var Message: TMessage); message EM_SETBKGNDCOLOR;
{$ENDIF}
  end;

procedure Register;

implementation

uses
  ShellApi, PathFunc, ComObj;

{$IF CompilerVersion < 36.0}
const
  MSFTEDIT_CLASS = 'RICHEDIT50W';
{$ENDIF}

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
      cp: Integer): HResult; stdcall;
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

{$IF CompilerVersion < 36.0}
  PEnLink = ^TEnLink;
  TENLink = record
    nmhdr: TNMHdr;
    msg: UINT;
    wParam: WPARAM;
    lParam: LPARAM;
    chrg: TCharRange;
  end;
{$ENDIF}

  TTextRange = record
    chrg: TCharRange;
    lpstrText: PWideChar;
  end;

  { The following interface definitions are simplified to contain only function
    prototypes up to the last one we need }

  IRichEditOle = interface(IUnknown)
    ['{00020D00-0000-0000-C000-000000000046}']
  end;

  ITextFont = interface(IDispatch)
    ['{8CC497C3-A1DF-11CE-8098-00AA0047BE5D}']
    function GetDuplicate(out Font: ITextFont): HResult; stdcall;
    function SetDuplicate(const Font: ITextFont): HResult; stdcall;
    function CanChange(out Value: Integer): HResult; stdcall;
    function IsEqual(const Font: ITextFont; out Value: Integer): HResult; stdcall;
    function Reset(Value: Integer): HResult; stdcall;
    function GetStyle(out Value: Integer): HResult; stdcall;
    function SetStyle(Value: Integer): HResult; stdcall;
    function GetAllCaps(out Value: Integer): HResult; stdcall;
    function SetAllCaps(Value: Integer): HResult; stdcall;
    function GetAnimation(out Value: Integer): HResult; stdcall;
    function SetAnimation(Value: Integer): HResult; stdcall;
    function GetBackColor(out Value: Integer): HResult; stdcall;
    function SetBackColor(Value: Integer): HResult; stdcall;
    function GetBold(out Value: Integer): HResult; stdcall;
    function SetBold(Value: Integer): HResult; stdcall;
    function GetEmboss(out Value: Integer): HResult; stdcall;
    function SetEmboss(Value: Integer): HResult; stdcall;
    function GetForeColor(out Value: Integer): HResult; stdcall;
    function SetForeColor(Value: Integer): HResult; stdcall;
  end;

  ITextPara = interface(IDispatch)
    ['{8CC497C4-A1DF-11CE-8098-00AA0047BE5D}']
  end;

  ITextRange = interface(IDispatch)
    ['{8CC497C2-A1DF-11CE-8098-00AA0047BE5D}']
    function GetText(out Text: WideString): HResult; stdcall;
    function SetText(const Text: WideString): HResult; stdcall;
    function GetChar(out CharCode: Integer): HResult; stdcall;
    function SetChar(CharCode: Integer): HResult; stdcall;
    function GetDuplicate(out Range: ITextRange): HResult; stdcall;
    function GetFormattedText(out Range: ITextRange): HResult; stdcall;
    function SetFormattedText(const Range: ITextRange): HResult; stdcall;
    function GetStart(out cpFirst: Integer): HResult; stdcall;
    function SetStart(cpFirst: Integer): HResult; stdcall;
    function GetEnd(out cpLim: Integer): HResult; stdcall;
    function SetEnd(cpLim: Integer): HResult; stdcall;
    function GetFont(out Font: ITextFont): HResult; stdcall;
    function SetFont(const Font: ITextFont): HResult; stdcall;
    function GetPara(out Para: ITextPara): HResult; stdcall;
    function SetPara(const Para: ITextPara): HResult; stdcall;
    function GetStoryLength(out Count: Integer): HResult; stdcall;
    function GetStoryType(out TypeValue: Integer): HResult; stdcall;
    function Collapse(Start: Integer): HResult; stdcall;
    function Expand(UnitValue: Integer; out Delta: Integer): HResult; stdcall;
    function GetIndex(UnitValue: Integer; out Index: Integer): HResult; stdcall;
    function SetIndex(UnitValue, Index, Extend: Integer): HResult; stdcall;
    function SetRange(Anchor, Active: Integer): HResult; stdcall;
    function InRange(const Range: ITextRange; out InRangeValue: Integer): HResult; stdcall;
    function InStory(const Range: ITextRange; out InStoryValue: Integer): HResult; stdcall;
    function IsEqual(const Range: ITextRange; out Equal: Integer): HResult; stdcall;
    function Select: HResult; stdcall;
    function StartOf(UnitValue, Extend: Integer; out Delta: Integer): HResult; stdcall;
    function EndOf(UnitValue, Extend: Integer; out Delta: Integer): HResult; stdcall;
    function Move(UnitValue, Count: Integer; out Delta: Integer): HResult; stdcall;
    function MoveStart(UnitValue, Count: Integer; out Delta: Integer): HResult; stdcall;
    function MoveEnd(UnitValue, Count: Integer; out Delta: Integer): HResult; stdcall;
  end;

  ITextSelection = interface(ITextRange)
    ['{8CC497C1-A1DF-11CE-8098-00AA0047BE5D}']
  end;

  ITextDocument = interface(IDispatch)
    ['{8CC497C0-A1DF-11CE-8098-00AA0047BE5D}']
    function GetName(out Name: WideString): HResult; stdcall;
    function GetSelection(out Selection: ITextSelection): HResult; stdcall;
    function GetStoryCount(out Count: Integer): HResult; stdcall;
    function GetStoryRanges(out Stories: IDispatch): HResult; stdcall;
    function GetSaved(out Value: Integer): HResult; stdcall;
    function SetSaved(Value: Integer): HResult; stdcall;
    function GetDefaultTabStop(out Value: Single): HResult; stdcall;
    function SetDefaultTabStop(Value: Single): HResult; stdcall;
    function New: HResult; stdcall;
    function Open(var Data: OleVariant; Flags, CodePage: Integer): HResult; stdcall;
    function Save(var Data: OleVariant; Flags, CodePage: Integer): HResult; stdcall;
    function Freeze(out Count: Integer): HResult; stdcall;
    function Unfreeze(out Count: Integer): HResult; stdcall;
    function BeginEditCollection: HResult; stdcall;
    function EndEditCollection: HResult; stdcall;
    function Undo(Count: Integer; out Prop: Integer): HResult; stdcall;
    function Redo(Count: Integer; out Prop: Integer): HResult; stdcall;
    function Range(cp1, cp2: Integer; out Range: ITextRange): HResult; stdcall;
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
  cp: Integer): HResult;
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

class constructor TRichEditViewer.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TRichEditViewer, TRichEditViewerStyleHook);
end;

constructor TRichEditViewer.Create(AOwner: TComponent);
begin
  inherited;
  FUseRichEdit := True;
  FCallback := TBasicRichEditOleCallback.Create;
end;

class destructor TRichEditViewer.Destroy;
begin
  TCustomStyleEngine.UnregisterStyleHook(TRichEditViewer, TRichEditViewerStyleHook);
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

function StreamLoad(dwCookie: DWORD_PTR; pbBuff: PByte;
  cb: Integer; var pcb: Integer): Integer; stdcall;
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
    Data.Buf := PByte(@Value[1]);
    Data.BytesLeft := Length(Value);
    { Check for UTF-16 BOM }
    if (AFormat and SF_TEXT <> 0) and (Data.BytesLeft >= 2) and
       (PWord(Pointer(Value))^ = $FEFF) then begin
      AFormat := AFormat or SF_UNICODE;
      Inc(Data.Buf, 2);
      Dec(Data.BytesLeft, 2);
    end;
    EditStream.dwCookie := DWORD_PTR(@Data);
    EditStream.dwError := 0;
    EditStream.pfnCallback := StreamLoad;
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

    var LStyle := StyleServices(Self);
    if not LStyle.Enabled or LStyle.IsSystemStyle then
      LStyle := nil;

    if (LStyle <> nil) and (seFont in StyleElements) and (seClient in StyleElements) then begin
      const StyleTextColor = ColorToRGB(LStyle.GetStyleFontColor(sfEditBoxTextNormal));
      if StyleTextColor <> ColorToRGB(clWindowText) then
        RecolorAutoForegroundText(StyleTextColor); { Must be done even if SF_TEXT was used above }
    end;
  end;
end;

procedure TRichEditViewer.RecolorAutoForegroundText(const NewTextColor: Integer);
const
  IID_ITextDocument: TGUID = '{8CC497C0-A1DF-11CE-8098-00AA0047BE5D}';
  { See https://learn.microsoft.com/en-us/windows/win32/api/tom/ne-tom-tomconstants }
  tomAutoColor = -9999997;
  tomCharFormat = 13;
begin
  if not FUseRichEdit or not HandleAllocated then
    Exit;

  var RichEditOle: IRichEditOle;
  var TextDocument: ITextDocument;
  var Range: ITextRange;
  var StoryLength: Integer;
  if (SendMessage(Handle, EM_GETOLEINTERFACE, 0, LPARAM(@RichEditOle)) = 0) or
     Failed(RichEditOle.QueryInterface(IID_ITextDocument, TextDocument)) or
     Failed(TextDocument.Range(0, 0, Range)) or
     Failed(Range.GetStoryLength(StoryLength)) or
     (StoryLength < 2) then
    Exit;

  { See https://learn.microsoft.com/en-us/windows/win32/api/tom/nn-tom-itextrange:
    All stories contain an undeletable final CR (0xD) character at the end }
  const TextLength = StoryLength-1;

  SendMessage(Handle, WM_SETREDRAW, 0, 0);
  const SaveReadOnly = ReadOnly;
  try
    ReadOnly := False;
    while True do begin
      { Move the end of the range (which initializes at 0,0) to the end of constant formatting }
      var Delta: Integer;
      if Failed(Range.MoveEnd(tomCharFormat, 1, Delta)) or (Delta = 0) then
        Break;

      { Recolor the range if the foreground color is automatic }
      var Font: ITextFont;
      var TextColor: Integer;
      if Succeeded(Range.GetFont(Font)) and
         Succeeded(Font.GetForeColor(TextColor)) and
         (TextColor = tomAutoColor) then
        Font.SetForeColor(NewTextColor); { Ignore failure }

      { Move the start of the range to the end of it, unless it ends at the end of the text }
      var EndPos: Integer;
      if Failed(Range.GetEnd(EndPos)) or
         (EndPos >= TextLength) or
         Failed(Range.SetStart(EndPos)) then
        Break;
    end;
  finally
    ReadOnly := SaveReadOnly;
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
    Invalidate;
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

{$IFDEF VCLSTYLES}

{ TRichEditViewerStyleHook- same as Vcl.ComCtrls' TRichEditStyleHook except
  that it is reduced to EM_SETBKGNDCOLOR handling only }

procedure TRichEditViewerStyleHook.EMSetBkgndColor(var Message: TMessage);
begin
  if seClient in Control.StyleElements then begin
    Message.LParam := ColorToRGB(StyleServices.GetStyleColor(scEdit));
    Handled := False;
  end;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('JR', [TRichEditViewer]);
end;

end.
