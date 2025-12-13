unit IDE.GotoFileForm;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE Goto File form
}

interface

uses
  Classes, Controls, StdCtrls, UIStateForm;

type
  TGotoFileForm = class(TUIStateForm)
    OKButton: TButton;
    CancelButton: TButton;
    GotoFileListBox: TListBox;
    GotoFileEdit: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure GotoFileListBoxDblClick(Sender: TObject);
    procedure GotoFileEditOrListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GotoFileEditChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FFiles: TStrings;
    FFileIndex: Integer;
    procedure SetFiles(Value: TStrings);
    procedure UpdateGotoFileListBox;
  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    property Files: TStrings write SetFiles;
    property FileIndex: Integer read FFileIndex;
  end;

implementation

uses
  Windows, Messages,
  PathFunc,
  IDE.HelperFunc;

{$R *.DFM}

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

procedure TGotoFileForm.FormCreate(Sender: TObject);
begin
  InitFormFont(Self);
  InitFormTheme(Self);
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
      GotoFileListBox.ItemIndex := 0;
      ActiveControl := GotoFileListBox;
      Key := 0;
    end else if (Key = VK_UP) and (GotoFileListBox.ItemIndex <= 0) then begin
      ActiveControl := GotoFileEdit;
      Key := 0;
    end;
  end;
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
