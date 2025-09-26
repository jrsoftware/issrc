unit Setup.UninstallProgressForm;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Uninstaller progress form
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Setup.SetupForm, StdCtrls, ExtCtrls, BitmapImage, NewProgressBar, NewStaticText,
  NewNotebook, BidiCtrls;

type
  TUninstallProgressForm = class(TSetupForm)
    FOuterNotebook: TNewNotebook;
    FInnerPage: TNewNotebookPage;
    FInnerNotebook: TNewNotebook;
    FInstallingPage: TNewNotebookPage;
    FMainPanel: TPanel;
    FPageNameLabel: TNewStaticText;
    FPageDescriptionLabel: TNewStaticText;
    FWizardSmallBitmapImage: TBitmapImage;
    FBevel1: TBevel;
    FStatusLabel: TNewStaticText;
    FProgressBar: TNewProgressBar;
    FBeveledLabel: TNewStaticText;
    FBevel: TBevel;
    FCancelButton: TNewButton;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(const ATitle, AAppName: String; const AModernStyle: Boolean;
      const AMainIconPostfix, AWizardIconsPostfix: String);
    procedure UpdateProgress(const AProgress, ARange: Integer);
  published
    property OuterNotebook: TNewNotebook read FOuterNotebook;
    property InnerPage: TNewNotebookPage read FInnerPage;
    property InnerNotebook: TNewNotebook read FInnerNotebook;
    property InstallingPage: TNewNotebookPage read FInstallingPage;
    property MainPanel: TPanel read FMainPanel;
    property PageNameLabel: TNewStaticText read FPageNameLabel;
    property PageDescriptionLabel: TNewStaticText read FPageDescriptionLabel;
    property WizardSmallBitmapImage: TBitmapImage read FWizardSmallBitmapImage;
    property Bevel1: TBevel read FBevel1;
    property StatusLabel: TNewStaticText read FStatusLabel;
    property ProgressBar: TNewProgressBar read FProgressBar;
    property BeveledLabel: TNewStaticText read FBeveledLabel;
    property Bevel: TBevel read FBevel;
    property CancelButton: TNewButton read FCancelButton;
  end;

var
  UninstallProgressForm: TUninstallProgressForm;

implementation

uses
  Themes, TaskbarProgressFunc, Setup.MainForm, SetupLdrAndSetup.Messages,
  Shared.SetupMessageIDs, Shared.CommonFunc.Vcl;

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

  MainPanel.ParentBackGround := False;

  PageNameLabel.Font.Style := [fsBold];
  PageNameLabel.Caption := SetupMessages[msgWizardUninstalling];
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

procedure TUninstallProgressForm.Initialize(const ATitle, AAppName: String; const AModernStyle: Boolean;
  const AMainIconPostfix, AWizardIconsPostfix: String);
begin
  Caption := ATitle;
  PageDescriptionLabel.Caption := FmtSetupMessage1(msgUninstallStatusLabel, AAppName);
  StatusLabel.Caption := FmtSetupMessage1(msgStatusUninstalling, AAppName);

  if not WizardSmallBitmapImage.InitializeFromIcon(HInstance, PChar('Z_UNINSTALLICON' + AWizardIconsPostfix), clNone, [32, 48, 64]) then {don't localize}
    WizardSmallBitmapImage.InitializeFromIcon(HInstance, PChar('MAINICON' + AMainIconPostfix), clNone, [32, 48, 64]); {don't localize}

  { Initialize wizard style - also see TWizardForm.Create }
  var LStyle := StyleServices(Self);
  if not LStyle.Enabled or LStyle.IsSystemStyle then
    LStyle := nil;
  if LStyle <> nil then begin
    { TNewNotebook ignores VCL Styles so it needs a bit of help }
    OuterNotebook.ParentColor := True;
    Color := LStyle.GetStyleColor(scWindow);
  end;
  if AModernStyle then begin
    if LStyle = nil then
      OuterNotebook.Color := clWindow;
    Bevel1.Visible := False;
  end;
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
