unit Setup.UninstallSharedFileForm;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  "Remove Shared File" form
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Setup.SetupForm, StdCtrls, NewStaticText, BidiCtrls;

type
  TUninstSharedFileForm = class(TSetupForm)
    BodyLabel: TNewStaticText;
    FilenameLabel: TNewStaticText;
    FilenameEdit: TEdit;
    LocationLabel: TNewStaticText;
    LocationEdit: TEdit;
    YesButton: TNewButton;
    YesToAllButton: TNewButton;
    NoButton: TNewButton;
    NoToAllButton: TNewButton;
  private
    { Private declarations }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

function ExecuteRemoveSharedFileDlg(const Filename: String;
  var AAll: Boolean): Boolean;

implementation

uses
  PathFunc, Shared.Struct, SetupLdrAndSetup.Messages, Shared.SetupMessageIDs, Setup.MainForm;

{$R *.DFM}

function ExecuteRemoveSharedFileDlg(const Filename: String;
  var AAll: Boolean): Boolean;
var
  Form: TUninstSharedFileForm;
  Res: Integer;
begin
  Form := TUninstSharedFileForm.Create(nil);
  try
    Form.FilenameEdit.Text := PathExtractName(Filename);
    Form.LocationEdit.Text := PathExtractDir(Filename);
    Res := Form.ShowModal;
  finally
    Form.Free;
  end;
  Result := (Res = mrYes) or (Res = mrYesToAll);
  AAll := (Res = mrYesToAll) or (Res = mrNoToAll);
end;

{ TSelectLanguageForm }

constructor TUninstSharedFileForm.Create(AOwner: TComponent);
begin
  inherited;

  InitializeFont;

  Caption := SetupMessages[msgConfirmDeleteSharedFileTitle];
  BodyLabel.Caption := SetupMessages[msgConfirmDeleteSharedFile2];
  FilenameLabel.Caption := SetupMessages[msgSharedFileNameLabel];
  LocationLabel.Caption := SetupMessages[msgSharedFileLocationLabel];
  YesButton.Caption := SetupMessages[msgButtonYes];
  YesToAllButton.Caption := SetupMessages[msgButtonYesToAll];
  NoButton.Caption := SetupMessages[msgButtonNo];
  NoToAllButton.Caption := SetupMessages[msgButtonNoToAll];

  KeepSizeY := True;
end;

procedure TUninstSharedFileForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WindowClass.style := Params.WindowClass.style or CS_NOCLOSE;
end;

end.
