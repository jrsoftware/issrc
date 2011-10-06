unit RichEditViewer;

{ TRichEditViewer v1.12 by Jordan Russell and Martijn Laan

  Known problem:
  If, after assigning rich text to a TRichEditViewer component, you change
  a property that causes the component's handle to be recreated, all text
  formatting will be lost. In the interests of code size, I do not intend
  to work around this.

  $jrsoftware: issrc/Components/RichEditViewer.pas,v 1.12 2011/06/08 10:44:25 mlaan Exp $
}

{$IFDEF VER90}
  {$DEFINE DELPHI2}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TRichEditViewer = class(TMemo)
  private
    FUseRichEdit: Boolean;
    FRichEditLoaded: Boolean;
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
  published
    property UseRichEdit: Boolean read FUseRichEdit write SetUseRichEdit default True;
  end;

procedure Register;

implementation

uses
  RichEdit, ShellApi, BidiUtils;

const
  { Note: There is no 'W' 1.0 class }
  RICHEDIT_CLASS10A = 'RICHEDIT';
  RICHEDIT_CLASSA = 'RichEdit20A';
  RICHEDIT_CLASSW = 'RichEdit20W';
  MSFTEDIT_CLASS = 'RICHEDIT50W';
  EM_AUTOURLDETECT = WM_USER + 91;
  ENM_LINK = $04000000;
  EN_LINK = $070b;

type
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
    lpstrText: {$IFDEF UNICODE} PWideChar {$ELSE} PAnsiChar {$ENDIF};
  end;

var
  RichEditModule: HMODULE;
  RichEditUseCount: Integer = 0;
  RichEditVersion: Integer;

procedure LoadRichEdit;
begin
  if RichEditUseCount = 0 then begin
    {$IFDEF UNICODE}
    RichEditVersion := 4;
    RichEditModule := LoadLibrary('MSFTEDIT.DLL');
    {$ELSE}
    RichEditModule := 0;
    {$ENDIF}
    if RichEditModule = 0 then begin
      RichEditVersion := 2;
      RichEditModule := LoadLibrary('RICHED20.DLL');
    end;
    {$IFNDEF UNICODE}
    if RichEditModule = 0 then begin
      RichEditVersion := 1;
      RichEditModule := LoadLibrary('RICHED32.DLL');
    end;
    {$ENDIF}
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

{ TRichEditViewer }

constructor TRichEditViewer.Create(AOwner: TComponent);
begin
  inherited;
  FUseRichEdit := True;
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
    {$IFDEF UNICODE}
    if RichEditVersion = 4 then
      CreateSubClass(Params, MSFTEDIT_CLASS)
    else
      CreateSubClass(Params, RICHEDIT_CLASSW);
    {$ELSE}
    if RichEditVersion = 2 then
      CreateSubClass(Params, RICHEDIT_CLASSA)
    else
      CreateSubClass(Params, RICHEDIT_CLASS10A);
    {$ENDIF}
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
  if FUseRichEdit and (RichEditVersion >= 2) then begin
    Mask := ENM_LINK or SendMessage(Handle, EM_GETEVENTMASK, 0, 0);
    SendMessage(Handle, EM_SETEVENTMASK, 0, LPARAM(Mask));
    SendMessage(Handle, EM_AUTOURLDETECT, WPARAM(True), 0);
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
{$IFDEF DELPHI2}
  const
    SF_UNICODE = $0010;
{$ENDIF}
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
            if URL <> '' then
              ShellExecute(Handle, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
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
