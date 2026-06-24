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
    UndoButton: TToolButton;
    RedoButton: TToolButton;
    ToolButton1: TToolButton;
    CutButton: TToolButton;
    CopyButton: TToolButton;
    PasteButton: TToolButton;
    ToolButton2: TToolButton;
    SelectAllButton: TToolButton;
    ActionList: TActionList;
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
  private
    FRichEdit: TRichEdit;
    FCallback: IRichEditOleCallback;
    procedure CreateRichEditControl;
    procedure RichEditLinkClick(Sender: TCustomRichEdit; const URL: String;
      Button: TMouseButton);
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateToolbarTheme;
  end;

var
  RichEditForm: TRichEditForm;

implementation

uses
  Windows, ShellApi,
  Graphics, StdCtrls, Menus, RichEdit, {$IF RtlVersion >= 36.0} Themes, {$ENDIF}
  Shared.CommonFunc,
  IDE.ImagesModule, IDE.HelperFunc, IDE.LocalizeFunc, IDE.MainForm;

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
  { See MainForm }
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

procedure TRichEditForm.UpdateToolbarTheme;
begin
  { See MainForm }
  ToolBarPanel.Color := InitFormThemeGetBkColor(False);
  ThemedToolbarVirtualImageList.ImageCollection := ImagesModule.ToolBarImageCollection[InitFormThemeIsDark];
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

  { Start a new document, same default font & size as Setup uses }
  FRichEdit.DefAttributes.Name := 'Segoe UI';
  FRichEdit.DefAttributes.Size := 9;
  FRichEdit.DefAttributes.Color := clWindowText;   { Automatic }
  FRichEdit.DefAttributes.BackColor := clWindow;   { Automatic }
  FRichEdit.SelAttributes.Assign(FRichEdit.DefAttributes);
  FRichEdit.Modified := False;

  {$IFDEF DEBUG}
  FRichEdit.Lines.LoadFromFile('Colortest.rtf');
  FRichEdit.Modified := False;
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

procedure TRichEditForm.RichEditLinkClick(Sender: TCustomRichEdit; const URL: String;
  Button: TMouseButton);
begin
  if (Button = mbLeft) and (GetKeyState(VK_CONTROL) < 0) then
    ShellExecute(Handle, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
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
