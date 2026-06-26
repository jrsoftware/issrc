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
  Messages, Classes, UITypes, Controls, ComCtrls, ExtCtrls, ActnList, Actions,
  StdActns, ExtActns, Dialogs, Forms, VirtualImageList, ImageList, ImgList,
  ToolWin, RichEditOleCallback,
  IDE.IDEForm;

type
  TRichEditForm = class(TIDEForm)
    ToolBarPanel: TPanel;
    ToolBar: TToolBar;
    NewButton: TToolButton;
    OpenButton: TToolButton;
    SaveButton: TToolButton;
    SaveAsButton: TToolButton;
    ToolButton2: TToolButton;
    UndoButton: TToolButton;
    RedoButton: TToolButton;
    ToolButton1: TToolButton;
    CutButton: TToolButton;
    CopyButton: TToolButton;
    PasteButton: TToolButton;
    ToolButton3: TToolButton;
    BoldButton: TToolButton;
    ItalicButton: TToolButton;
    UnderlineButton: TToolButton;
    ToolButton4: TToolButton;
    FontButton: TToolButton;
    IncreaseFontSizeButton: TToolButton;
    DecreaseFontSizeButton: TToolButton;
    ToolButton5: TToolButton;
    TextColorButton: TToolButton;
    BackgroundColorButton: TToolButton;
    ResetColorsButton: TToolButton;
    ToolButton6: TToolButton;
    AlignLeftButton: TToolButton;
    AlignCenterButton: TToolButton;
    AlignRightButton: TToolButton;
    ToolButton7: TToolButton;
    BulletsButton: TToolButton;
    IndentButton: TToolButton;
    OutdentButton: TToolButton;
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
    BoldAction: TRichEditBold;
    ItalicAction: TRichEditItalic;
    UnderlineAction: TRichEditUnderline;
    FontAction: TAction;
    IncreaseFontSizeAction: TAction;
    DecreaseFontSizeAction: TAction;
    TextColorAction: TAction;
    BackgroundColorAction: TAction;
    ResetColorsAction: TAction;
    AlignLeftAction: TRichEditAlignLeft;
    AlignCenterAction: TRichEditAlignCenter;
    AlignRightAction: TRichEditAlignRight;
    BulletsAction: TRichEditBullets;
    IndentAction: TAction;
    OutdentAction: TAction;
    ThemedToolbarVirtualImageList: TVirtualImageList;
    FontDialog: TFontDialog;
    ColorDialog: TColorDialog;
    StatusBar: TStatusBar;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RedoActionExecute(Sender: TObject);
    procedure RedoActionUpdate(Sender: TObject);
    procedure NewActionExecute(Sender: TObject);
    procedure OpenActionExecute(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure FontActionExecute(Sender: TObject);
    procedure ChangeFontSizeActionExecute(Sender: TObject);
    procedure TextColorActionExecute(Sender: TObject);
    procedure BackgroundColorActionExecute(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure IndentActionExecute(Sender: TObject);
    procedure PasteActionUpdate(Sender: TObject);
    procedure ResetColorsActionExecute(Sender: TObject);
  private
    FBaseCaption: String;
    FRichEdit: TRichEdit;
    FCallback: IRichEditOleCallback;
    FFilename: String;
    FMainScriptFilename: String;
    procedure CreateRichEditControl;
    procedure CMAppSysCommand(var Message: TMessage); message CM_APPSYSCOMMAND;
    procedure RichEditLinkClick(Sender: TCustomRichEdit; const URL: String;
      Button: TMouseButton);
    procedure RichEditStateChange(Sender: TObject);
    procedure UpdateCaption;
    procedure UpdateStatusBar;
    procedure NewFile;
    procedure OpenFile(const AFilename: String);
    function SaveFile(const ASaveAs: Boolean): Boolean;
    function ChooseColor(const ACurrentColor, AAutoColor: TColor;
      out AColor: TColor): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTheme;
    procedure NotifyMainScriptRenamed(const AOldFilename, ANewFilename: String);
    function ConfirmCloseFile: Boolean;
  end;

var
  RichEditForm: TRichEditForm;

implementation

uses
  Windows, ShellApi,
  SysUtils, Graphics, StdCtrls, Menus, Clipbrd, RichEdit, {$IF RtlVersion >= 36.0} Themes, {$ENDIF}
  PathFunc, BrowseFunc, ModernColors,
  Shared.CommonFunc, Shared.CommonFunc.Vcl, Shared.FileClass,
  IDE.Messages, IDE.ImagesModule, IDE.HelperFunc, IDE.LocalizeFunc, IDE.MainForm;

{$R *.dfm}

{$IF RtlVersion >= 35.0}
  {$DEFINE HAVEBACKCOLOR}
  {$DEFINE HAVEURLS}
{$ENDIF}

const
  { Status bar panel indexes }
  spCaretPos = 0;
  spModified = 1;

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
  CutButton.Hint := LFmtMessage(CutButton.Hint, [NewShortCutToText(ShortCut(Ord('X'), [ssCtrl]))]);
  CopyButton.Hint := LFmtMessage(CopyButton.Hint, [NewShortCutToText(ShortCut(Ord('C'), [ssCtrl]))]);
  PasteButton.Hint := LFmtMessage(PasteButton.Hint, [NewShortCutToText(ShortCut(Ord('V'), [ssCtrl]))]);
  UndoButton.Hint := LFmtMessage(UndoButton.Hint, [NewShortCutToText(ShortCut(Ord('Z'), [ssCtrl]))]);
  RedoButton.Hint := LFmtMessage(RedoButton.Hint, [NewShortCutToText(ShortCut(Ord('Y'), [ssCtrl]))]);
  BoldButton.Hint := LFmtMessage(BoldButton.Hint, [NewShortCutToText(ShortCut(Ord('B'), [ssCtrl]))]);
  ItalicButton.Hint := LFmtMessage(ItalicButton.Hint, [NewShortCutToText(ShortCut(Ord('I'), [ssCtrl]))]);
  UnderlineButton.Hint := LFmtMessage(UnderlineButton.Hint, [NewShortCutToText(ShortCut(Ord('U'), [ssCtrl]))]);
  { Just like MainForm }
  IncreaseFontSizeButton.Hint := RemoveAccelChar(IncreaseFontSizeAction.Caption);
  DecreaseFontSizeButton.Hint := RemoveAccelChar(DecreaseFontSizeAction.Caption);
  ResetColorsButton.Hint := RemoveAccelChar(ResetColorsAction.Caption);
  AlignLeftButton.Hint := RemoveAccelChar(AlignLeftButton.Caption);
  AlignCenterButton.Hint := RemoveAccelChar(AlignCenterButton.Caption);
  AlignRightButton.Hint := RemoveAccelChar(AlignRightButton.Caption);
  BulletsButton.Hint := RemoveAccelChar(BulletsAction.Caption);
  OutdentButton.Hint := RemoveAccelChar(OutdentAction.Caption);
  IndentButton.Hint := RemoveAccelChar(IndentAction.Caption);

  FBaseCaption := Caption;

  { See MainForm }
  ToolBarPanel.ParentBackground := False;

  CreateRichEditControl;
  UpdateTheme;

  {$IFNDEF HAVEBACKCOLOR}
  BackgroundColorAction.Visible := False;
  {$ENDIF}
end;

procedure TRichEditForm.CreateRichEditControl;
begin
  FRichEdit := TRichEdit.Create(Self);
  FRichEdit.Parent := Self;
  FRichEdit.Align := alClient;
  FRichEdit.WordWrap := True;
  FRichEdit.ScrollBars := ssVertical;
  {$IFDEF HAVEURLS}
  FRichEdit.EnableURLs := True;
  FRichEdit.OnLinkClick := RichEditLinkClick;
  {$ENDIF}
  FRichEdit.OnChange := RichEditStateChange;
  FRichEdit.OnSelectionChange := RichEditStateChange;
  FRichEdit.StyleName := 'Windows'; { We do not support dark mode editing atm }

  { Remove ugly WS_EX_CLIENTEDGE }
  FRichEdit.BorderStyle := bsNone;
  { Replace it with RichEdit's native support for a border }
  const Margin = MulDiv(2, CurrentPPI, 96);
  var R := FRichEdit.ClientRect;
  InflateRect(R, -Margin, -Margin);
  SendMessage(FRichEdit.Handle, EM_SETRECT, 0, LPARAM(@R));

  { For images }
  FCallback := TBasicRichEditOleCallback.Create;
  SendMessage(FRichEdit.Handle, EM_SETOLECALLBACK, 0, LPARAM(FCallback));

  NewFile;

  { Reopen the .rtf last edited for the current main script, ignoring errors }
  const KnownRichEditFile = LoadKnownRichEditFile(FMainScriptFilename);
  if KnownRichEditFile <> '' then begin
    try
      OpenFile(KnownRichEditFile);
    except
    end;
  end;
end;

procedure TRichEditForm.FormShow(Sender: TObject);
begin
  { Just like MainForm }
  ToolBarPanel.AutoSize := True;
end;

procedure TRichEditForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TRichEditForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  { Just like MainForm }
  if IsWindowEnabled(Handle) then
    CanClose := ConfirmCloseFile
  else
    CanClose := False;
end;

procedure TRichEditForm.FormDestroy(Sender: TObject);
begin
  if RichEditForm = Self then begin
    if HandleAllocated then
      SaveWindowState(Self, 'RichEditState');
    SaveKnownRichEditFile(FMainScriptFilename, FFilename); { This clears if FFilename = '' }
    RichEditForm := nil;
  end;
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
  Caption := GetCaptionFilename + ' '#$2013' ' + FBaseCaption;
end;

procedure TRichEditForm.UpdateStatusBar;

  function GetSelectionLineCount: Integer;
  begin
    const SelText = FRichEdit.SelText;
    const L = Length(SelText);
    Result := 1;
    for var I := 1 to L do
      if SelText[I] = #13 then
        Inc(Result);
    { A selection ending with a line break does not count for that line }
    if (Result > 1) and ((SelText[L] = #13) or (SelText[L] = #10)) then
      Dec(Result);
  end;

begin
  if FRichEdit = nil then
    Exit;

  { Just like MainForm }
  if FRichEdit.SelLength > 0 then
    StatusBar.Panels[spCaretPos].Text :=
      Format('%4d|%4d', [FRichEdit.SelLength, GetSelectionLineCount])
  else begin
    const Caret = FRichEdit.CaretPos;
    StatusBar.Panels[spCaretPos].Text :=
      Format('%4d:%4d', [Caret.Y + 1, Caret.X + 1]);
  end;

  if FRichEdit.Modified then
    StatusBar.Panels[spModified].Text := LFmtMessage(SStatusModified)
  else
    StatusBar.Panels[spModified].Text := '';
end;

procedure TRichEditForm.UpdateTheme;
begin
  { See MainForm }
  ToolBarPanel.Color := InitFormThemeGetBkColor(False);
  SetControlWindowTheme(FRichEdit, InitFormThemeIsDark);
  ThemedToolbarVirtualImageList.ImageCollection := ImagesModule.ToolBarImageCollection[InitFormThemeIsDark];
end;

procedure TRichEditForm.CMAppSysCommand(var Message: TMessage);
begin
  { Prevent TCustomForm.CMAppSysCommand from forwarding Alt+X menu messages
    to MainForm if a RichEditForm has focus. }
  Message.Result := 0;
end;

procedure TRichEditForm.RichEditLinkClick(Sender: TCustomRichEdit; const URL: String;
  Button: TMouseButton);
begin
  if (Button = mbLeft) and (GetKeyState(VK_CONTROL) < 0) then
    ShellExecute(Handle, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

procedure TRichEditForm.RichEditStateChange(Sender: TObject);
begin
  UpdateStatusBar;
end;

procedure TRichEditForm.NewFile;
begin
  FRichEdit.Lines.Clear;
  { Start a new document, same default font & size as Setup uses }
  FRichEdit.DefAttributes.Name := 'Segoe UI';
  FRichEdit.DefAttributes.Size := 9;
  FRichEdit.DefAttributes.Color := clWindowText; { Changed to CFE_AUTOCOLOR by VCL }
  {$IFDEF HAVEBACKCOLOR}
  FRichEdit.DefAttributes.BackColor := clWindow; { Changed to CFE_AUTOBACKCOLOR by VCL }
  {$ENDIF}
  FRichEdit.SelAttributes.Assign(FRichEdit.DefAttributes);
  FFilename := '';
  FMainScriptFilename := MainForm.MainFilename;
  FRichEdit.Modified := False;
  UpdateCaption;
  UpdateStatusBar;
end;

procedure TRichEditForm.OpenFile(const AFilename: String);

  procedure StreamIn(const Buffer: AnsiString);
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
    if EditStream.dwError <> 0 then { dwError is not a Win32 error code }
      raise Exception.Create(LFmtMessage(SRichEditStreamInError, [EditStream.dwError]));
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
  try
    StreamIn(Buffer);
  except
    NewFile; { EM_STREAMIN failure also screws current contents, so need to start fresh }
    raise;
  end;
  FFilename := AFilename;
  FMainScriptFilename := MainForm.MainFilename;
  FRichEdit.Modified := False;
  UpdateCaption;
  UpdateStatusBar;
end;

function TRichEditForm.SaveFile(const ASaveAs: Boolean): Boolean;

  function StreamOut: AnsiString;
  begin
    var Data: TStreamSaveData;
    var EditStream: TEditStream;
    EditStream.dwCookie := DWORD_PTR(@Data);
    EditStream.dwError := 0;
    EditStream.pfnCallback := StreamSave;
    SendMessage(FRichEdit.Handle, EM_STREAMOUT, SF_RTF, LPARAM(@EditStream));
    if EditStream.dwError <> 0 then { dwError is not a Win32 error code }
      raise Exception.Create(LFmtMessage(SRichEditStreamOutError, [EditStream.dwError]));
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
  const RtfText = StreamOut;
  const F = TFile.Create(Filename, fdCreateAlways, faWrite, fsNone);
  try
    F.WriteAnsiString(RtfText);
  finally
    F.Free;
  end;
  FFilename := Filename;
  FRichEdit.Modified := False;
  UpdateCaption;
  UpdateStatusBar;
  Result := True;
end;

procedure TRichEditForm.NotifyMainScriptRenamed(const AOldFilename, ANewFilename: String);
begin
  if PathSame(FMainScriptFilename, AOldFilename) then
    FMainScriptFilename := ANewFilename;
end;

function TRichEditForm.ConfirmCloseFile: Boolean;
begin
  { Just like MainForm }
  Result := True;
  if FRichEdit.Modified then begin
    case MsgBox(LFmtMessage(SCompilerFileChangedSavePrompt, [GetFileTitle(FFilename)]),
       FBaseCaption, mbError, MB_YESNOCANCEL) of
      IDYES: Result := SaveFile(False);
      IDNO: ;
    else
      Result := False;
    end;
  end;
end;

procedure TRichEditForm.NewActionExecute(Sender: TObject);
begin
  if ConfirmCloseFile then
    NewFile;
end;

procedure TRichEditForm.OpenActionExecute(Sender: TObject);
begin
  if not ConfirmCloseFile then
    Exit;
  var Filename := FFilename;
  if NewGetOpenFileName('', Filename, '',
       Format(SLitExtAndAllFilter, [LFmtMessage(SRtfFiles), SLitRtfExt, LFmtMessage(SAllFiles)]),
       SLitRtfExt, Handle) then
    OpenFile(PathExpand(Filename));
end;

procedure TRichEditForm.SaveActionExecute(Sender: TObject);
begin
  SaveFile(Sender = SaveAsAction);
end;

procedure TRichEditForm.RedoActionExecute(Sender: TObject);
begin
  SendMessage(FRichEdit.Handle, EM_REDO, 0, 0);
end;

procedure TRichEditForm.ActionUpdate(Sender: TObject);
begin
  { Just like VCL's standard actions }
  (Sender as TAction).Enabled := (FRichEdit <> nil) and FRichEdit.Focused and
    not FRichEdit.ReadOnly;
end;

procedure TRichEditForm.PasteActionUpdate(Sender: TObject);
begin
  { Like TEditPaste, but also knows about images. Note: Ctrl+V will work irregardless. }
  ActionUpdate(Sender);
  if PasteAction.Enabled then
    PasteAction.Enabled := (Clipboard.HasFormat(CF_TEXT) or Clipboard.HasFormat(CF_BITMAP) or Clipboard.HasFormat(CF_DIB));
end;

procedure TRichEditForm.RedoActionUpdate(Sender: TObject);
begin
  ActionUpdate(Sender);
  if RedoAction.Enabled then
    RedoAction.Enabled := SendMessage(FRichEdit.Handle, EM_CANREDO, 0, 0) <> 0;
end;

procedure TRichEditForm.FontActionExecute(Sender: TObject);
begin
  FontDialog.Font.Name := FRichEdit.SelAttributes.Name;
  FontDialog.Font.Size := FRichEdit.SelAttributes.Size;
  FontDialog.Font.Style := FRichEdit.SelAttributes.Style * [fsBold, fsItalic]; { No fdEffects in Options so only bold and italic are offered }
  if FontDialog.Execute(Handle) then begin
    FRichEdit.SelAttributes.Name := FontDialog.Font.Name;
    FRichEdit.SelAttributes.Size := FontDialog.Font.Size;
    FRichEdit.SelAttributes.Style := FRichEdit.SelAttributes.Style -
      [fsBold, fsItalic] + (FontDialog.Font.Style * [fsBold, fsItalic]); { See above }
  end;
end;

procedure TRichEditForm.ChangeFontSizeActionExecute(Sender: TObject);
begin
  const CurrentSize = FRichEdit.SelAttributes.Size;
  if Sender = IncreaseFontSizeAction then
    FRichEdit.SelAttributes.Size := CurrentSize + 1
  else if CurrentSize > 1 then
    FRichEdit.SelAttributes.Size := CurrentSize - 1;
end;

procedure TRichEditForm.IndentActionExecute(Sender: TObject);
const
  { Sees ComCtrls }
  PointsPerInch = 72;
  TwipsPerPoint = 20;
  { One default tab stop: half an inch, in points }
  IndentStep = ((TwipsPerPoint * PointsPerInch) div 2) div TwipsPerPoint;
begin
  var NewIndent := FRichEdit.Paragraph.FirstIndent;
  if Sender = IndentAction then
    Inc(NewIndent, IndentStep)
  else begin
    Dec(NewIndent, IndentStep);
    if NewIndent < 0 then
      NewIndent := 0;
  end;
  FRichEdit.Paragraph.FirstIndent := NewIndent;
end;

function TRichEditForm.ChooseColor(const ACurrentColor, AAutoColor: TColor;
  out AColor: TColor): Boolean;
begin
  ColorDialog.CustomColors.Clear;
  { Add the auto color + R/G/B with proper contrast on both light and dark backgrounds }
  ColorDialog.CustomColors.Add(Format('ColorA=%.6x', [ColorToRGB(AAutoColor)]));
  ColorDialog.CustomColors.Add(Format('ColorB=%.6x', [ColorToRGB(SwapRB($eb0000))]));
  ColorDialog.CustomColors.Add(Format('ColorC=%.6x', [ColorToRGB(SwapRB($008a00))]));
  ColorDialog.CustomColors.Add(Format('ColorD=%.6x', [ColorToRGB(SwapRB($6161ff))]));
  ColorDialog.Color := ACurrentColor;
  Result := ColorDialog.Execute(Handle);
  if Result then begin
    if ColorDialog.Color = ColorToRGB(AAutoColor) then
      AColor := AAutoColor { Changed to CFE_AUTO(BACK)COLOR by VCL }
    else
      AColor := ColorDialog.Color;
  end;
end;

procedure TRichEditForm.TextColorActionExecute(Sender: TObject);
begin
  var NewColor: TColor;
  if ChooseColor(FRichEdit.SelAttributes.Color, clWindowText, NewColor) then
    FRichEdit.SelAttributes.Color := NewColor;
end;

procedure TRichEditForm.BackgroundColorActionExecute(Sender: TObject);
begin
  {$IFDEF HAVEBACKCOLOR}
  var NewColor: TColor;
  if ChooseColor(FRichEdit.SelAttributes.BackColor, clWindow, NewColor) then
    FRichEdit.SelAttributes.BackColor := NewColor;
  {$ENDIF}
end;

procedure TRichEditForm.ResetColorsActionExecute(Sender: TObject);
begin
  FRichEdit.SelAttributes.Color := clWindowText; { Changed to CFE_AUTOCOLOR by VCL }
  {$IFDEF HAVEBACKCOLOR}
  FRichEdit.SelAttributes.BackColor := clWindow; { Changed to CFE_AUTOBACKCOLOR by VCL }
  {$ENDIF}
end;

end.
