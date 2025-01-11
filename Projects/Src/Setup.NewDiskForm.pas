unit Setup.NewDiskForm;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  New Disk form
}

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  Setup.SetupForm, StdCtrls, ExtCtrls, NewStaticText, BitmapImage, BidiCtrls;

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
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

function SelectDisk(const DiskNumber: Integer; const AFilename: String; var Path: String): Boolean;

implementation

uses
  SetupLdrAndSetup.Messages, Shared.SetupMessageIDs, PathFunc, Shared.CommonFunc.Vcl, Shared.CommonFunc, BrowseFunc,
  Setup.MainFunc, Setup.MainForm, Setup.WizardForm;

{$R *.DFM}

function SelectDisk(const DiskNumber: Integer; const AFilename: String;
  var Path: String): Boolean;
begin
  Application.Restore;  { see comments in AppMessageBox }

  with TNewDiskForm.Create(Application) do
    try
      Filename := AFilename;
      SelectDiskLabel.Caption := FmtSetupMessage(msgSelectDiskLabel2, [IntToStr(DiskNumber)]);
      PathEdit.Text := Path;
      ActiveControl := OKButton;
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

  Caption := SetupMessages[msgChangeDiskTitle];
  PathLabel.Caption := SetupMessages[msgPathLabel];
  BrowseButton.Caption := SetupMessages[msgButtonBrowse];
  OKButton.Caption := SetupMessages[msgButtonOK];
  CancelButton.Caption := SetupMessages[msgButtonCancel];

  DiskBitmapImage.InitializeFromIcon(HInstance, 'Z_DISKICON', Color, [48, 64]); {don't localize}

  TryEnableAutoCompleteFileSystem(PathEdit.Handle);

  KeepSizeY := True;
  { WizardForm will not exist yet if we're being called from [Code]'s
    ExtractTemporaryFile in InitializeSetup }
  FlipSizeAndCenterIfNeeded(Assigned(WizardForm), WizardForm, False);
end;

procedure TNewDiskForm.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  { This usually just makes the taskbar button flash }
  if Showing then
    SetForegroundWindow(Handle);
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
