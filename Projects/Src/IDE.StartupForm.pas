unit IDE.StartupForm;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE Startup form
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UIStateForm, StdCtrls, ExtCtrls;

type
  TStartupFormResult = (srNone, srEmpty, srWizard, srOpenFile, srOpenDialog,
    srOpenDialogExamples);

  TStartupForm = class(TUIStateForm)
    OKButton: TButton;
    CancelButton: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    EmptyRadioButton: TRadioButton;
    WizardRadioButton: TRadioButton;
    OpenRadioButton: TRadioButton;
    OpenListBox: TListBox;
    StartupCheck: TCheckBox;
    NewImage: TImage;
    OpenImage: TImage;
    DonateImage: TImage;
    MailingListImage: TImage;
    procedure RadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DblClick_(Sender: TObject);
    procedure OpenListBoxClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure DonateImageClick(Sender: TObject);
    procedure MailingListImageClick(Sender: TObject);
  private
    FResult: TStartupFormResult;
    FResultMainFileName: TFileName;
    procedure SetMRUFilesList(const MRUFilesList: TStringList);
    procedure UpdateImages;
  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    property MRUFilesList: TStringList write SetMRUFilesList;
    property Result: TStartupFormResult read FResult;
    property ResultMainFileName: TFileName read FResultMainFileName;
  end;

implementation

uses
  IDE.Messages, Shared.CommonFunc.Vcl, Shared.CommonFunc, IDE.HelperFunc,
  IDE.MainForm, IDE.ImagesModule, ComCtrls;

{$R *.DFM}

procedure TStartupForm.SetMRUFilesList(const MRUFilesList: TStringList);
begin
  OpenListBox.Items.AddStrings(MRUFilesList);
  UpdateHorizontalExtent(OpenListBox);
end;

procedure TStartupForm.UpdateImages;

  function GetImage(const Button: TToolButton; const WH: Integer): TWICImage;
  begin
    Result := ImagesModule.LightToolBarImageCollection.GetSourceImage(Button.ImageIndex, WH, WH)
  end;

begin
  { After a DPI change the button's Width and Height isn't yet updated, so calculate it ourselves }
  var WH := MulDiv(16, CurrentPPI, 96);
  NewImage.Picture.Graphic:= GetImage(MainForm.NewMainFileButton, WH);
  OpenImage.Picture.Graphic := GetImage(MainForm.OpenMainFileButton, WH);
end;

procedure TStartupForm.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  UpdateImages;
end;

procedure TStartupForm.FormCreate(Sender: TObject);
begin
  FResult := srNone;

  InitFormFont(Self);

  DonateImage.Hint := MainForm.UpdatePanelDonateImage.Hint;

  UpdateImages;

  OpenListBox.Items.Add(SCompilerExampleScripts);
  OpenListBox.Items.Add(SCompilerMoreFiles);
  OpenListBox.ItemIndex := 0;
  UpdateHorizontalExtent(OpenListBox);
  ActiveControl := OpenRadioButton;
end;

{ This and CreateParams make bsSizeable (which has an unwanted icon) look like bsDialog, see:
  https://stackoverflow.com/questions/32096482/delphi-resizable-bsdialog-form/32098633 }
procedure TStartupForm.CreateWnd;
begin
  inherited;
  SendMessage(Handle, WM_SETICON, ICON_BIG, 0);
end;

procedure TStartupForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
end;

procedure TStartupForm.RadioButtonClick(Sender: TObject);
begin
  EmptyRadioButton.Checked := Sender = EmptyRadioButton;
  WizardRadioButton.Checked := Sender = WizardRadioButton;
  OpenRadioButton.Checked := Sender = OpenRadioButton;
  if Sender = OpenRadioButton then begin
    if OpenListBox.ItemIndex = -1 then
      OpenListBox.ItemIndex := 0;
  end
  else
    OpenListBox.ItemIndex := -1;
end;

procedure TStartupForm.DblClick_(Sender: TObject);
begin
  if OkButton.Enabled then
    OkButton.Click;
end;

procedure TStartupForm.OpenListBoxClick(Sender: TObject);
begin
  OpenRadioButton.Checked := True;
end;

procedure TStartupForm.DonateImageClick(Sender: TObject);
begin
  OpenDonateSite;
end;

procedure TStartupForm.MailingListImageClick(Sender: TObject);
begin
  OpenMailingListSite;
end;

procedure TStartupForm.OKButtonClick(Sender: TObject);
begin
  if EmptyRadioButton.Checked then
    FResult := srEmpty
  else if WizardRadioButton.Checked then
    FResult := srWizard
  else { if OpenRadioButton.Checked then } begin
    if OpenListBox.ItemIndex = 0 then
      FResult := srOpenDialogExamples
    else if OpenListBox.ItemIndex > 1 then begin
      FResult := srOpenFile;
      FResultMainFileName := OpenListBox.Items[OpenListBox.ItemIndex];
    end else
      FResult := srOpenDialog;
  end;
end;

end.
