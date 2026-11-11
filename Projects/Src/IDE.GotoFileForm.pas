unit IDE.GotoFileForm;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE Goto File form
}

interface

uses
  Types, Classes, Controls, StdCtrls,
  IDE.IDEForm;

type
  TGotoFileForm = class(TIDEForm)
    OKButton: TButton;
    CancelButton: TButton;
    GotoFileListBox: TListBox;
    GotoFileEdit: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure GotoFileListBoxDblClick(Sender: TObject);
    procedure GotoFileListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure GotoFileEditOrListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GotoFileEditChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FFiles: TStrings;
    FImageNames: TStrings;
    FFileIndex: Integer;
    procedure SetFiles(Value: TStrings);
    procedure UpdateGotoFileListBox;
  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    property Files: TStrings write SetFiles;
    property ImageNames: TStrings write FImageNames;
    property FileIndex: Integer read FFileIndex;
  end;

implementation

uses
  Windows, Messages, Math, Graphics,
  PathFunc,
  IDE.HelperFunc, IDE.ImagesModule;

{$R *.DFM}

procedure TGotoFileForm.FormCreate(Sender: TObject);
begin
  { Finish localization }
  SizeBottomButtons(OKButton, CancelButton);

  { Make items tall enough for the icons }
  GotoFileListBox.Canvas.Font.Assign(GotoFileListBox.Font);
  GotoFileListBox.ItemHeight := Max(GotoFileListBox.Canvas.TextHeight('0') + 1,
    ToCurrentPPI(16) + 2);
end;

procedure TGotoFileForm.SetFiles(Value: TStrings);
begin
  FFiles := Value;
  UpdateGotoFileListBox;
end;

procedure TGotoFileForm.UpdateGotoFileListBox;

  function Match(const Name, Value: String): Boolean;
  begin
    Result := (Value = '') or (PathStrFind(PChar(Name), Length(Name), PChar(Value), Length(Value)) >= 0);
  end;

begin
  GotoFileListBox.Items.BeginUpdate;
  try
    GotoFileListBox.Items.Clear;
    for var I := 0 to FFiles.Count-1 do begin
      const Name = PathExtractName(FFiles[I]);
      if Match(Name, GotoFileEdit.Text) then
        GotoFileListBox.Items.AddObject(Name, TObject(I));
    end;
  finally
    GotoFileListBox.Items.EndUpdate;
  end;

  if GotoFileListBox.Items.Count > 0 then
    GotoFileListBox.ItemIndex := 0;

  OKButton.Enabled := GotoFileListBox.ItemIndex >= 0;
end;

{ This and CreateParams make bsSizeable (which has an unwanted icon) look like bsDialog, see:
  https://stackoverflow.com/questions/32096482/delphi-resizable-bsdialog-form/32098633 }
procedure TGotoFileForm.CreateWnd;
begin
  inherited;
  SendMessage(Handle, WM_SETICON, ICON_BIG, 0);
end;

procedure TGotoFileForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
end;

procedure TGotoFileForm.GotoFileEditChange(Sender: TObject);
begin
  UpdateGotoFileListBox;
end;

procedure TGotoFileForm.GotoFileEditOrListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then begin
    if (Key = VK_DOWN) and (Sender = GotoFileEdit) then begin
      if GotoFileListBox.Items.Count > 0 then
        GotoFileListBox.ItemIndex := 0;
      ActiveControl := GotoFileListBox;
      Key := 0;
    end else if (Key = VK_UP) and (GotoFileListBox.ItemIndex <= 0) then begin
      ActiveControl := GotoFileEdit;
      Key := 0;
    end;
  end;
end;

procedure TGotoFileForm.GotoFileListBoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  const Canvas = GotoFileListBox.Canvas;
  const S = GotoFileListBox.Items[Index];
  const FileIndex = Integer(GotoFileListBox.Items.Objects[Index]);

  Canvas.FillRect(Rect);
  const WH = ToCurrentPPI(16);
  const R = TRect.Create(TPoint.Create(Rect.Left + ToCurrentPPI(2),
    Rect.Top + (Rect.Height - WH) div 2), WH, WH);
  if (FImageNames <> nil) and (FileIndex < FImageNames.Count) then
    ImagesModule.ListImageCollection[InitFormThemeIsDark].Draw(Canvas, R, FImageNames[FileIndex]);
  Canvas.TextOut(R.Right + ToCurrentPPI(4), R.Top + (R.Height - Canvas.TextHeight(S)) div 2, S);
  { TCustomListBox.CNDrawItem doesn't call DrawFocusRect when a custom style is active }
  if (odFocused in State) and not (odNoFocusRect in State) and
     GotoFileListBox.IsCustomStyleActive then
    Canvas.DrawFocusRect(Rect);
end;

procedure TGotoFileForm.GotoFileListBoxDblClick(Sender: TObject);
begin
  if OKButton.Enabled then
    OKButton.Click;
end;

procedure TGotoFileForm.OKButtonClick(Sender: TObject);
begin
  FFileIndex := Integer(GotoFileListBox.Items.Objects[GotoFileListBox.ItemIndex]);
end;

end.
