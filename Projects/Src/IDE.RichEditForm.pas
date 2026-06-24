unit IDE.RichEditForm;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE RTF Editor form
}

interface

uses
  Classes, Controls, ComCtrls, Forms,
  RichEditOleCallback,
  IDE.IDEForm;

type
  TRichEditForm = class(TIDEForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FRichEdit: TCustomRichEdit;
    FCallback: IRichEditOleCallback;
    procedure CreateRichEditControl;
    procedure RichEditLinkClick(Sender: TCustomRichEdit; const URL: String;
      Button: TMouseButton);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  RichEditForm: TRichEditForm;

implementation

uses
  Windows, ShellApi,
  Graphics, StdCtrls, RichEdit, {$IF RtlVersion >= 36.0} Themes, {$ENDIF}
  Shared.CommonFunc,
  IDE.HelperFunc, IDE.MainForm;

{$R *.dfm}

{ TRichEditForm }

constructor TRichEditForm.Create(AOwner: TComponent);
begin
  { Set PopupParent before it applies the dark title bar: setting it
    afterwards would recreate the handle and drop the dark-mode attribute. }
  PopupMode := pmExplicit;
  PopupParent := Application.MainForm;
  inherited;
  {$IF RtlVersion >= 36.0}
  { See MainForm }
  StyleName := TStyleManager.ActiveStyle.Name;
  {$ENDIF}
  LoadWindowState(Self, 'RichEditState');
end;

procedure TRichEditForm.FormCreate(Sender: TObject);
begin
  { Finish localization }
  Caption := RemoveAccelChar(MainForm.TRichEditor.Caption);

  CreateRichEditControl;
end;

procedure TRichEditForm.CreateRichEditControl;
begin
  const RichEditControl = TRichEdit.Create(Self);
  FRichEdit := RichEditControl;
  RichEditControl.Parent := Self;
  RichEditControl.Align := alClient;
  RichEditControl.WordWrap := True;
  RichEditControl.ScrollBars := ssVertical;
  RichEditControl.EnableURLs := True;
  RichEditControl.OnLinkClick := RichEditLinkClick;
  RichEditControl.StyleName := 'Windows'; { We do not support dark mode editing atm }

  { For images }
  FCallback := TBasicRichEditOleCallback.Create;
  SendMessage(FRichEdit.Handle, EM_SETOLECALLBACK, 0, LPARAM(FCallback));

  { Start a new document, same default font & size as Setup uses }
  FRichEdit.DefAttributes.Name := 'Segoe UI';
  FRichEdit.DefAttributes.Size := 9;
  FRichEdit.DefAttributes.Color := clWindowText;   { Automatic }
  FRichEdit.DefAttributes.BackColor := clWindow;   { Automatic }
  FRichEdit.SelAttributes.Assign(FRichEdit.DefAttributes);
  FRichEdit.Modified := False;
end;

procedure TRichEditForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TRichEditForm.FormDestroy(Sender: TObject);
begin
  if RichEditForm = Self then begin
    if HandleAllocated then
      SaveWindowState(Self, 'RichEditState');
    RichEditForm := nil;
  end;
end;

procedure TRichEditForm.RichEditLinkClick(Sender: TCustomRichEdit; const URL: String;
  Button: TMouseButton);
begin
  if (Button = mbLeft) and (GetKeyState(VK_CONTROL) < 0) then
    ShellExecute(Handle, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

end.
