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
  Classes, Controls, ComCtrls, ExtCtrls, ActnList, Actions, StdActns, Forms,
  VirtualImageList, ImageList, ImgList, ToolWin,
  RichEditOleCallback,
  IDE.IDEForm;

type
  TRichEditForm = class(TIDEForm)
    ToolBarPanel: TPanel;
    ToolBar: TToolBar;
    NewButton: TToolButton;
    OpenButton: TToolButton;
    SaveButton: TToolButton;
    SaveAsButton: TToolButton;
    ToolButton3: TToolButton;
    UndoButton: TToolButton;
    RedoButton: TToolButton;
    ToolButton1: TToolButton;
    CutButton: TToolButton;
    CopyButton: TToolButton;
    PasteButton: TToolButton;
    ToolButton2: TToolButton;
    SelectAllButton: TToolButton;
    ActionList: TActionList;
    NewAction: TAction;
    OpenAction: TAction;
    SaveAction: TAction;
    SaveAsAction: TAction;
    UndoAction: TEditUndo;
    RedoAction: TAction;
    CutAction: TEditCut;
    CopyAction: TEditCopy;
    PasteAction: TEditPaste;
    SelectAllAction: TEditSelectAll;
    ThemedToolbarVirtualImageList: TVirtualImageList;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RedoActionExecute(Sender: TObject);
    procedure RedoActionUpdate(Sender: TObject);
    procedure NewActionExecute(Sender: TObject);
    procedure OpenActionExecute(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
  private
    FRichEdit: TRichEdit;
    FCallback: IRichEditOleCallback;
    FFilename: String;
    procedure CreateRichEditControl;
    procedure RichEditLinkClick(Sender: TCustomRichEdit; const URL: String;
      Button: TMouseButton);
    procedure NewDocument;
    procedure LoadDocument(const AFilename: String);
    function SaveDocument(const ASaveAs: Boolean): Boolean;
    procedure UpdateCaption;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateToolbarTheme;
  end;

var
  RichEditForm: TRichEditForm;

implementation

uses
  Windows, ShellApi,
  SysUtils, Graphics, StdCtrls, Menus, RichEdit, {$IF RtlVersion >= 36.0} Themes, {$ENDIF}
  PathFunc, BrowseFunc,
  Shared.CommonFunc, Shared.FileClass,
  IDE.Messages, IDE.ImagesModule, IDE.HelperFunc, IDE.LocalizeFunc, IDE.MainForm;

{$R *.dfm}

type
  PStreamLoadData = ^TStreamLoadData;
  TStreamLoadData = record
    Buffer: PByte;
    BytesLeft: Integer;
  end;

  PStreamSaveData = ^TStreamSaveData;
  TStreamSaveData = record
    Buffer: AnsiString;
  end;

function StreamLoad(dwCookie: DWORD_PTR; pbBuff: PByte; cb: Integer;
  var pcb: Integer): Integer; stdcall;
begin
  const Data = PStreamLoadData(dwCookie);
  if cb > Data.BytesLeft then
    cb := Data.BytesLeft;
  Move(Data.Buffer^, pbBuff^, cb);
  Inc(Data.Buffer, cb);
  Dec(Data.BytesLeft, cb);
  pcb := cb;
  Result := 0;
end;

function StreamSave(dwCookie: DWORD_PTR; pbBuff: PByte; cb: Integer;
  var pcb: Integer): Integer; stdcall;
begin
  const Data = PStreamSaveData(dwCookie);
  const OldLength = Length(Data.Buffer);
  SetLength(Data.Buffer, OldLength + cb);
  Move(pbBuff^, Data.Buffer[OldLength + 1], cb);
  pcb := cb;
  Result := 0;
end;

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
  NewButton.Hint := LFmtMessage(NewButton.Hint, [NewShortCutToText(NewAction.ShortCut)]);
  OpenButton.Hint := LFmtMessage(OpenButton.Hint, [NewShortCutToText(OpenAction.ShortCut)]);
  SaveButton.Hint := LFmtMessage(SaveButton.Hint, [NewShortCutToText(SaveAction.ShortCut)]);
  UndoButton.Hint := LFmtMessage(UndoButton.Hint, [NewShortCutToText(ShortCut(Ord('Z'), [ssCtrl]))]);
  RedoButton.Hint := LFmtMessage(RedoButton.Hint, [NewShortCutToText(ShortCut(Ord('Y'), [ssCtrl]))]);
  CutButton.Hint := LFmtMessage(CutButton.Hint, [NewShortCutToText(ShortCut(Ord('X'), [ssCtrl]))]);
  CopyButton.Hint := LFmtMessage(CopyButton.Hint, [NewShortCutToText(ShortCut(Ord('C'), [ssCtrl]))]);
  PasteButton.Hint := LFmtMessage(PasteButton.Hint, [NewShortCutToText(ShortCut(Ord('V'), [ssCtrl]))]);
  SelectAllButton.Hint := LFmtMessage(SelectAllButton.Hint, [NewShortCutToText(ShortCut(Ord('A'), [ssCtrl]))]);

  { See MainForm }
  ToolBarPanel.ParentBackground := False;

  UpdateToolbarTheme;
  CreateRichEditControl;
end;

procedure TRichEditForm.CreateRichEditControl;
begin
  FRichEdit := TRichEdit.Create(Self);
  FRichEdit.Parent := Self;
  FRichEdit.Align := alClient;
  FRichEdit.WordWrap := True;
  FRichEdit.ScrollBars := ssVertical;
  FRichEdit.EnableURLs := True;
  FRichEdit.OnLinkClick := RichEditLinkClick;
  FRichEdit.StyleName := 'Windows'; { We do not support dark mode editing atm }

  { For images }
  FCallback := TBasicRichEditOleCallback.Create;
  SendMessage(FRichEdit.Handle, EM_SETOLECALLBACK, 0, LPARAM(FCallback));

  NewDocument;

  {$IFDEF DEBUG}
  LoadDocument('Colortest.rtf');
  {$ENDIF}
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

procedure TRichEditForm.UpdateToolbarTheme;
begin
  { See MainForm }
  ToolBarPanel.Color := InitFormThemeGetBkColor(False);
  ThemedToolbarVirtualImageList.ImageCollection := ImagesModule.ToolBarImageCollection[InitFormThemeIsDark];
end;

procedure TRichEditForm.RichEditLinkClick(Sender: TCustomRichEdit; const URL: String;
  Button: TMouseButton);
begin
  if (Button = mbLeft) and (GetKeyState(VK_CONTROL) < 0) then
    ShellExecute(Handle, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

procedure TRichEditForm.NewDocument;
begin
  FRichEdit.Lines.Clear;
  { Start a new document, same default font & size as Setup uses }
  FRichEdit.DefAttributes.Name := 'Segoe UI';
  FRichEdit.DefAttributes.Size := 9;
  FRichEdit.DefAttributes.Color := clWindowText;   { Automatic }
  FRichEdit.DefAttributes.BackColor := clWindow;   { Automatic }
  FRichEdit.SelAttributes.Assign(FRichEdit.DefAttributes);
  FFilename := '';
  FRichEdit.Modified := False;
  UpdateCaption;
end;

procedure TRichEditForm.LoadDocument(const AFilename: String);

  function StreamIn(const Buffer: AnsiString): Integer;
  begin
    var Data: TStreamLoadData;
    Data.Buffer := PByte(Buffer);
    Data.BytesLeft := Length(Buffer);
    var EditStream: TEditStream;
    EditStream.dwCookie := DWORD_PTR(@Data);
    EditStream.dwError := 0;
    EditStream.pfnCallback := StreamLoad;
    SendMessage(FRichEdit.Handle, EM_EXLIMITTEXT, 0, LPARAM($7FFFFFFE));
    SendMessage(FRichEdit.Handle, EM_STREAMIN, SF_RTF, LPARAM(@EditStream));
    Result := EditStream.dwError;
  end;

begin
  var Buffer: AnsiString;
  const F = TFile.Create(AFilename, fdOpenExisting, faRead, fsRead);
  try
    const Size = F.CappedSize;
    SetLength(Buffer, Size);
    F.ReadBuffer(Buffer[1], Size);
  finally
    F.Free;
  end;
  StreamIn(Buffer);
  FFilename := AFilename;
  FRichEdit.Modified := False;
  UpdateCaption;
end;

function TRichEditForm.SaveDocument(const ASaveAs: Boolean): Boolean;

  function StreamOut: AnsiString;
  begin
    var Data: TStreamSaveData;
    var EditStream: TEditStream;
    EditStream.dwCookie := DWORD_PTR(@Data);
    EditStream.dwError := 0;
    EditStream.pfnCallback := StreamSave;
    SendMessage(FRichEdit.Handle, EM_STREAMOUT, SF_RTF, LPARAM(@EditStream));
    Result := Data.Buffer;
  end;

begin
  Result := False;
  var Filename := FFilename;
  if ASaveAs or (Filename = '') then begin
    if not NewGetSaveFileName('', Filename, '',
             Format(SLitExtAndAllFilter, [LFmtMessage(SRtfFiles), SLitRtfExt, LFmtMessage(SAllFiles)]),
             SLitRtfExt, Handle) then
      Exit;
    Filename := PathExpand(Filename);
  end;
  const F = TFile.Create(Filename, fdCreateAlways, faWrite, fsNone);
  try
    F.WriteAnsiString(StreamOut);
  finally
    F.Free;
  end;
  FFilename := Filename;
  FRichEdit.Modified := False;
  UpdateCaption;
  Result := True;
end;

procedure TRichEditForm.UpdateCaption;

  function GetCaptionFilename: String;
  begin
    if FFilename = '' then
      Result := GetFileTitle(FFilename)
    else if MainForm.FullPathInTitleBar then
      Result := FFilename
    else
      Result := GetDisplayFilename(FFilename);
  end;

begin
  Caption := GetCaptionFilename + ' '#$2013' ' + RemoveAccelChar(MainForm.TRichEditor.Caption);
end;

procedure TRichEditForm.NewActionExecute(Sender: TObject);
begin
  NewDocument;
end;

procedure TRichEditForm.OpenActionExecute(Sender: TObject);
begin
  var Filename := FFilename;
  if NewGetOpenFileName('', Filename, '',
       Format(SLitExtAndAllFilter, [LFmtMessage(SRtfFiles), SLitRtfExt, LFmtMessage(SAllFiles)]),
       SLitRtfExt, Handle) then
    LoadDocument(PathExpand(Filename));
end;

procedure TRichEditForm.SaveActionExecute(Sender: TObject);
begin
  SaveDocument(Sender = SaveAsAction);
end;

procedure TRichEditForm.RedoActionExecute(Sender: TObject);
begin
  SendMessage(FRichEdit.Handle, EM_REDO, 0, 0);
end;

procedure TRichEditForm.RedoActionUpdate(Sender: TObject);
begin
  { Checks Focused just like VCL's standard actions }
  RedoAction.Enabled := (FRichEdit <> nil) and FRichEdit.Focused and
    not FRichEdit.ReadOnly and (SendMessage(FRichEdit.Handle, EM_CANREDO, 0, 0) <> 0);
end;

end.
