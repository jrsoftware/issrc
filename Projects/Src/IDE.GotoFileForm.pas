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
  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

implementation

uses
  Windows, Messages,
  IDE.HelperFunc;

{$R *.DFM}

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

end.
