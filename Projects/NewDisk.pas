unit NewDisk;

{
  Inno Setup
  Copyright (C) 1997-2005 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  New Disk form

  $jrsoftware: issrc/Projects/NewDisk.pas,v 1.34 2010/10/22 10:33:26 mlaan Exp $
}

interface

{$I VERSION.INC}

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  SetupForm, StdCtrls, ExtCtrls, NewStaticText, BitmapImage, BidiCtrls;

type
  TNewDiskForm = class(TSetupForm)
    DiskBitmapImage: TBitmapImage;
    SelectDiskLabel: TNewStaticText;
    PathLabel: TNewStaticText;
    PathEdit: TEdit;
    BrowseButton: TNewButton;
    OKButton: TNewButton;
    CancelButton: TNewButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BrowseButtonClick(Sender: TObject);
  private
    { Private declarations }
    Filename: string;
    function GetSanitizedPath: String;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

function SelectDisk(const DiskNumber: Integer; const AFilename: String; var Path: String): Boolean;

implementation

uses
  Msgs, MsgIDs, PathFunc, CmnFunc, CmnFunc2, BrowseFunc,
  Main, Wizard;

{$R *.DFM}

function SelectDisk(const DiskNumber: Integer; const AFilename: String;
  var Path: String): Boolean;
begin
  with TNewDiskForm.Create(Application) do
    try
      Filename := AFilename;
      SelectDiskLabel.Caption := FmtSetupMessage(msgSelectDiskLabel2, [IntToStr(DiskNumber)]);
      PathEdit.Text := Path;
      MessageBeep(0);
      Result := ShowModal = mrOK;
      if Result then
        Path := GetSanitizedPath;
    finally
      Free;
    end;
end;

{ TNewDiskForm }

constructor TNewDiskForm.Create(AOwner: TComponent);
begin
  inherited;

  InitializeFont;
  { WizardForm will not exist yet if we're being called from [Code]'s
    ExtractTemporaryFile in InitializeSetup }
  if Assigned(WizardForm) then
    CenterInsideControl(WizardForm, False)
  else
    Center;

  Caption := SetupMessages[msgChangeDiskTitle];
  PathLabel.Caption := SetupMessages[msgPathLabel];
  BrowseButton.Caption := SetupMessages[msgButtonBrowse];
  OKButton.Caption := SetupMessages[msgButtonOK];
  CancelButton.Caption := SetupMessages[msgButtonCancel];

  DiskBitmapImage.Bitmap.Handle := LoadBitmap(HInstance, 'DISKIMAGE');  {don't localize};
  DiskBitmapImage.ReplaceColor := clBlue;
  DiskBitmapImage.ReplaceWithColor := Color;

  TryEnableAutoCompleteFileSystem(PathEdit.Handle);
end;

function TNewDiskForm.GetSanitizedPath: String;
begin
  Result := PathExpand(RemoveBackslashUnlessRoot(Trim(PathEdit.Text)));
end;

procedure TNewDiskForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Path: String;
begin
  case ModalResult of
    mrOK: begin
        Path := GetSanitizedPath;
        if (Path = '') or not NewFileExists(AddBackslash(Path) + Filename) then begin
          CanClose := False;
          LoggedMsgBox(FmtSetupMessage(msgFileNotInDir2, [Filename, Path]),
            '', mbError, MB_OK, False, 0);
        end;
      end;
    mrCancel: CanClose := ExitSetupMsgBox;
  end;
end;

procedure TNewDiskForm.BrowseButtonClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := GetSanitizedPath;
  if BrowseForFolder(SetupMessages[msgSelectDirectoryLabel], Dir, Handle, False) then
    PathEdit.Text := Dir;
end;

end.
