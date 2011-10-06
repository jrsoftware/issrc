unit SelFolderForm;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  "Select Folder" form

  $jrsoftware: issrc/Projects/SelFolderForm.pas,v 1.15 2010/10/22 10:33:26 mlaan Exp $
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SetupForm, StdCtrls, FolderTreeView, NewStaticText, BidiCtrls; 

type
  TSelectFolderForm = class(TSetupForm)
    BrowseLabel: TNewStaticText;
    PathEdit: TEdit;
    NewFolderButton: TNewButton;
    OKButton: TNewButton;
    CancelButton: TNewButton;
    procedure PathEditChange(Sender: TObject);
    procedure NewFolderButtonClick(Sender: TObject);
  private
    { Private declarations }
    FFolderTreeView: TCustomFolderTreeView;
    FNewFolderName: String;
    FStartMenu, FAppendDir: Boolean;
    procedure FolderTreeViewChange(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    constructor Create2(AOwner: TComponent; AStartMenu: Boolean);
  end;

function ShowSelectFolderDialog(const StartMenu, AppendDir: Boolean;
  var Path: String; const NewFolderName: String): Boolean;

implementation

uses
  PathFunc, Msgs, MsgIDs, Main, SetupTypes, Wizard, CmnFunc2;

{$R *.DFM}

function ShowSelectFolderDialog(const StartMenu, AppendDir: Boolean;
  var Path: String; const NewFolderName: String): Boolean;
var
  Form: TSelectFolderForm;
begin
  Form := TSelectFolderForm.Create2(nil, StartMenu);
  try
    Form.FAppendDir := AppendDir;
    Form.FNewFolderName := NewFolderName;
    Form.NewFolderButton.Visible := not AppendDir and (NewFolderName <> '');
    if StartMenu then begin
      with Form.FFolderTreeView as TStartMenuFolderTreeView do
        if IsNT then
          SetPaths(GetRealShellFolder(False, sfPrograms, False),
            GetRealShellFolder(True, sfPrograms, False),
            GetRealShellFolder(False, sfStartup, False),
            GetRealShellFolder(True, sfStartup, False))
        else
          SetPaths(GetRealShellFolder(False, sfPrograms, False),
            '', GetRealShellFolder(False, sfStartup, False), '');
      TidyUpGroupName(Path);
    end
    else
      TidyUpDirName(Path);

    if AppendDir then begin
      if PathExtractDir(Path) <> Path then
        Form.FFolderTreeView.ChangeDirectory(PathExtractDir(Path), True);
    end
    else
      Form.FFolderTreeView.ChangeDirectory(Path, True);
    if StartMenu or (Form.FFolderTreeView.Directory <> '') then
      Form.ActiveControl := Form.FFolderTreeView;
    Form.PathEdit.Text := Path;

    Result := (Form.ShowModal = mrOK);
    if Result then
      Path := Trim(Form.PathEdit.Text);
  finally
    Form.Free;
  end;
end;

{ TSelectFolderForm }

constructor TSelectFolderForm.Create(AOwner: TComponent);
var
  YDiff, W: Integer;
begin
  inherited;

  if not FStartMenu then begin
    FFolderTreeView := TFolderTreeView.Create(Self);
    TFolderTreeView(FFolderTreeView).OnChange := FolderTreeViewChange;
    TFolderTreeView(FFolderTreeView).OnRename := WizardForm.DirTreeRename;
  end
  else begin
    FFolderTreeView := TStartMenuFolderTreeView.Create(Self);
    TStartMenuFolderTreeView(FFolderTreeView).OnChange := FolderTreeViewChange;
    TStartMenuFolderTreeView(FFolderTreeView).OnRename := WizardForm.GroupTreeRename;
  end;
  FFolderTreeView.SetBounds(16, 64, 317, 229);
  FFolderTreeView.Visible := False;
  FFolderTreeView.Parent := Self;
  PathEdit.BringToFront;     { for MSAA }
  BrowseLabel.BringToFront;  { for MSAA }
  FFolderTreeView.TabOrder := 2;
  FFolderTreeView.Visible := True;

  InitializeFont;

  Caption := SetupMessages[msgBrowseDialogTitle];
  BrowseLabel.Caption := SetupMessages[msgBrowseDialogLabel];
  YDiff := WizardForm.AdjustLabelHeight(BrowseLabel);
  PathEdit.Top := PathEdit.Top + YDiff;
  TryEnableAutoCompleteFileSystem(PathEdit.Handle);
  FFolderTreeView.Top := FFolderTreeView.Top + YDiff;
  NewFolderButton.Caption := SetupMessages[msgButtonNewFolder];
  NewFolderButton.Top := NewFolderButton.Top + YDiff;
  NewFolderButton.Width := CalculateButtonWidth([msgButtonNewFolder]);
  W := CalculateButtonWidth([msgButtonOK, msgButtonCancel]);
  CancelButton.Caption := SetupMessages[msgButtonCancel];
  CancelButton.SetBounds(CancelButton.Left + CancelButton.Width - W,
    CancelButton.Top + YDiff, W, CancelButton.Height);
  OKButton.Caption := SetupMessages[msgButtonOK];
  OKButton.SetBounds(CancelButton.Left - ScalePixelsX(6) - W,
    OKButton.Top + YDiff, W, OKButton.Height);
  ClientHeight := ClientHeight + YDiff;

  CenterInsideControl(WizardForm, False);
end;

constructor TSelectFolderForm.Create2(AOwner: TComponent; AStartMenu: Boolean);
begin
  FStartMenu := AStartMenu;
  Create(AOwner);
end;

procedure TSelectFolderForm.PathEditChange(Sender: TObject);
begin
  OKButton.Enabled := (Trim(PathEdit.Text) <> '');
end;

procedure TSelectFolderForm.FolderTreeViewChange(Sender: TObject);
begin
  if FAppendDir then
    PathEdit.Text := AddBackslash(FFolderTreeView.Directory) + FNewFolderName
  else
    PathEdit.Text := FFolderTreeView.Directory;
  NewFolderButton.Enabled := FStartMenu or (FFolderTreeView.Directory <> '');
end;

procedure TSelectFolderForm.NewFolderButtonClick(Sender: TObject);
begin
  FFolderTreeView.CreateNewDirectory(FNewFolderName);
end;

end.
