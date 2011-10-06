unit UninstProgressForm;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Uninstaller progress form

  $jrsoftware: issrc/Projects/UninstProgressForm.pas,v 1.16 2010/10/30 20:26:25 jr Exp $
}

interface

{$I VERSION.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SetupForm, StdCtrls, ExtCtrls, BitmapImage, NewProgressBar, NewStaticText,
  NewNotebook, BidiCtrls;

type
  TUninstallProgressForm = class(TSetupForm)
    OuterNotebook: TNewNotebook;
    InnerPage: TNewNotebookPage;
    InnerNotebook: TNewNotebook;
    InstallingPage: TNewNotebookPage;
    MainPanel: TPanel;
    PageNameLabel: TNewStaticText;
    PageDescriptionLabel: TNewStaticText;
    WizardSmallBitmapImage: TBitmapImage;
    Bevel1: TBevel;
    StatusLabel: TNewStaticText;
    ProgressBar: TNewProgressBar;
    BeveledLabel: TNewStaticText;
    Bevel: TBevel;
    CancelButton: TNewButton;
  private
    { Private declarations }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(const ATitle, AAppName: String);
    procedure UpdateProgress(const AProgress, ARange: Integer);
  end;

var
  UninstallProgressForm: TUninstallProgressForm;

implementation

uses
  TaskbarProgressFunc, Main, Msgs, MsgIDs, CmnFunc;

{$R *.DFM}

{ TUninstallProgressForm }

procedure UninstallMessageBoxCallback(const Flags: LongInt; const After: Boolean;
  const Param: LongInt);
const
  States: array [TNewProgressBarState] of TTaskbarProgressState =
    (tpsNormal, tpsError, tpsPaused);
var
  UninstallProgressForm: TUninstallProgressForm;
  NewState: TNewProgressBarState;
begin
  UninstallProgressForm := TUninstallProgressForm(Param);

  if After then
    NewState := npbsNormal
  else if (Flags and MB_ICONSTOP) <> 0 then
    NewState := npbsError
  else
    NewState := npbsPaused;

  with UninstallProgressForm.ProgressBar do begin
    State := NewState;
    Invalidate;
  end;
  SetAppTaskbarProgressState(States[NewState]);
end;

constructor TUninstallProgressForm.Create(AOwner: TComponent);
begin
  inherited;

  SetMessageBoxCallbackFunc(UninstallMessageBoxCallback, LongInt(Self));

  InitializeFont;
  Center;

{$IFDEF IS_D7}
  MainPanel.ParentBackGround := False;
{$ENDIF}

  PageNameLabel.Font.Style := [fsBold];
  PageNameLabel.Caption := SetupMessages[msgWizardUninstalling];
  WizardSmallBitmapImage.Bitmap.Canvas.Brush.Color := clWindow;
  WizardSmallBitmapImage.Bitmap.Width := Application.Icon.Width;
  WizardSmallBitmapImage.Bitmap.Height := Application.Icon.Height;
  WizardSmallBitmapImage.Bitmap.Canvas.Draw(0, 0, Application.Icon);
  if SetupMessages[msgBeveledLabel] <> '' then begin
    BeveledLabel.Caption := ' ' + SetupMessages[msgBeveledLabel] + ' ';
    BeveledLabel.Visible := True;
  end;
  CancelButton.Caption := SetupMessages[msgButtonCancel];
end;

destructor TUninstallProgressForm.Destroy;
begin
  SetMessageBoxCallbackFunc(nil, 0);
  SetAppTaskbarProgressState(tpsNoProgress);

  inherited;
end;

procedure TUninstallProgressForm.Initialize(const ATitle, AAppName: String);
begin
  Caption := ATitle;
  PageDescriptionLabel.Caption := FmtSetupMessage1(msgUninstallStatusLabel, AAppName);
  StatusLabel.Caption := FmtSetupMessage1(msgStatusUninstalling, AAppName);
end;

procedure TUninstallProgressForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WindowClass.style := Params.WindowClass.style or CS_NOCLOSE;
end;

procedure TUninstallProgressForm.UpdateProgress(const AProgress, ARange: Integer);
var
  NewPos: Integer;
begin
  NewPos := MulDiv(AProgress, ProgressBar.Max, ARange);
  if ProgressBar.Position <> NewPos then begin
    ProgressBar.Position := NewPos;
    SetAppTaskbarProgressValue(NewPos, ProgressBar.Max);
  end;
end;

end.
