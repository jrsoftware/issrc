unit CompStartup;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler Startup form

  $jrsoftware: issrc/Projects/CompStartup.pas,v 1.11 2004/07/22 19:49:39 jr Exp $
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
    procedure RadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DblClick_(Sender: TObject);
    procedure OpenListBoxClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FResult: TStartupFormResult;
    FResultFileName: TFileName;
    procedure SetMRUList(const MRUList: TStringList);
  public
    property MRUList: TStringList write SetMRUList;
    property Result: TStartupFormResult read FResult;
    property ResultFileName: TFileName read FResultFileName;
  end;

implementation

uses
  CompMsgs, CmnFunc, CmnFunc2, CompForm;

{$R *.DFM}

procedure TStartupForm.SetMRUList(const MRUList: TStringList);
var
  I: Integer;
begin
  for I := 0 to MRUList.Count-1 do
    OpenListBox.Items.Add(MRUList[I]);
  UpdateHorizontalExtent(OpenListBox);
end;

procedure TStartupForm.FormCreate(Sender: TObject);
begin
  FResult := srNone;

  InitFormFont(Self);

  OpenListBox.Items.Add(SCompilerExampleScripts);
  OpenListBox.Items.Add(SCompilerMoreFiles);
  OpenListBox.ItemIndex := 0;
  UpdateHorizontalExtent(OpenListBox);
  ActiveControl := OpenRadioButton;
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
      FResultFileName := OpenListBox.Items[OpenListBox.ItemIndex];
    end else
      FResult := srOpenDialog;
  end;
end;

end.
