unit DownloadForm;

{
  Inno Setup
  Copyright (C) 1997-2011 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Download form
}

interface

uses
  Forms, Classes, NewStaticText, NewProgressBar, BidiCtrls, PathFunc;

type
  TWebDownloadForm = class(TForm)
  private
    FStatusLabel: TNewStaticText;
    FProgressGauge: TNewProgressBar;
    FAllSize: Double;
    FBytesRead: Double;
    FCurrentTotalSize: Cardinal;
    FFilenameLabel: TNewStaticText;
    FCancelButton: TNewButton;
  protected
    procedure CancelButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateProgress(var Status: string; BytesRead, TotalSize: Cardinal);
    procedure NextFile(FileSize: Cardinal);
    procedure Reset;

    property StatusLabel: TNewStaticText read FStatusLabel;
    property FilenameLabel: TNewStaticText read FFilenameLabel;
    property ProgressGauge: TNewProgressBar read FProgressGauge;
    property CancelButton: TNewButton read FCancelButton;
    property AllSize: Double read FAllSize write FAllSize;
  end;

implementation

uses
  Logging, Main, Msgs, MsgIDs, SetupTypes, SysUtils, Wizard;

{ TWebDownloadForm }

procedure TWebDownloadForm.CancelButtonClick(Sender: TObject);
begin
  NeedToAbortInstall := True;
end;

constructor TWebDownloadForm.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);

  Caption := Application.Title;
  Width := 350;
  //Height := 100;
  BorderStyle := bsDialog;
  BorderIcons := [];
  Position := poScreenCenter;

  FStatusLabel := TNewStaticText.Create(Self);
  FStatusLabel.Name := 'StatusLabel';
  FStatusLabel.Parent := Self;
  FStatusLabel.Left := 8;
  FStatusLabel.Top := 8;
  FStatusLabel.Caption := '';

  FFilenameLabel := TNewStaticText.Create(Self);
  FFilenameLabel.Name := 'FilenameLabel';
  FFilenameLabel.Parent := Self;
  FFilenameLabel.Left := 8;
  FFilenameLabel.Top := FStatusLabel.Top + FStatusLabel.Height + 6;
  FFilenameLabel.Caption := '';
  FFilenameLabel.ForceLTRReading := True;

  FProgressGauge := TNewProgressBar.Create(Self);
  FProgressGauge.Name := 'ProgressGauge';
  FProgressGauge.Parent := Self;
  {FProgressGauge.Position := 0;
  FProgressGauge.Min := 0;
  FProgressGauge.Max := 100;}
  FProgressGauge.Left := 8;
  FProgressGauge.Top := FFilenameLabel.Top + FFilenameLabel.Height + 6;
  FProgressGauge.Width := ClientWidth - FProgressGauge.Left * 2;

  FCancelButton := TNewButton.Create(Self);
  FCancelButton.Name := 'CancelButton';
  FCancelButton.Parent := Self;
  FCancelButton.Left := ClientWidth - 8 - FCancelButton.Width;
  FCancelButton.Top := FProgressGauge.Top + FProgressGauge.Height + 4;
  FCancelButton.Caption := SetupMessages[msgButtonCancel];
  FCancelButton.OnClick := CancelButtonClick;

  ClientHeight := FCancelButton.Top + FCancelButton.Height + 8;
end;

procedure TWebDownloadForm.NextFile(FileSize: Cardinal);
begin
  FBytesRead := FBytesRead + FCurrentTotalSize;
  FCurrentTotalSize := FileSize;
end;

procedure TWebDownloadForm.Reset;
begin
  FBytesRead := 0;
  FAllSize := 0.0;
end;

procedure TWebDownloadForm.UpdateProgress(var Status: string; BytesRead, TotalSize: Cardinal);
var
  NewPercentage: Integer;
begin
  if (TotalSize <> 0) and (TotalSize <> FCurrentTotalSize) then begin
    FAllSize := FAllSize - FCurrentTotalSize + TotalSize;
    FCurrentTotalSize := TotalSize;
  end;

  NewPercentage := 0;
  if FAllSize > 0 then
    NewPercentage := Round((FBytesRead + BytesRead) * 100 / FAllSize);
  if NewPercentage <> ProgressGauge.Position then
    ProgressGauge.Position := NewPercentage;
end;

end.
