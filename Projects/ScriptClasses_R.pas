unit ScriptClasses_R;

{
  Inno Setup
  Copyright (C) 1997-2008 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Script support classes (run time)

  $Id: ScriptClasses_R.pas,v 1.61 2012/02/05 18:59:23 mlaan Exp $
}

interface

{$I VERSION.INC}

uses
  uPSRuntime;

function ScriptClassesLibraryRegister_R(ScriptInterpreter: TPSExec): TPSRuntimeClassImporter;

implementation

uses
  Windows, Controls, Forms, StdCtrls, Graphics,
  uPSR_std, uPSR_classes, uPSR_graphics, uPSR_controls, uPSR_forms,
  uPSR_stdctrls, uPSR_extctrls, uPSR_comobj, {$IFNDEF UNICODE} uPSUtils, {$ENDIF}
  NewStaticText, NewCheckListBox, NewProgressBar, RichEditViewer,
  ExtCtrls, UIStateForm, SetupForm, Main, Wizard, SetupTypes, PasswordEdit,
  FolderTreeView, BitmapImage, NewNotebook, ScriptDlg, BidiCtrls,
  UninstProgressForm;

type
  TWinControlAccess = class(TWinControl);

procedure TWinControlParentBackground_R(Self: TWinControl; var T: Boolean); begin {$IFDEF IS_D7} T := TWinControlAccess(Self).ParentBackground {$ELSE} T := False {$ENDIF}; end;
procedure TWinControlParentBackground_W(Self: TWinControl; const T: Boolean); begin {$IFDEF IS_D7} TWinControlAccess(Self).ParentBackground := T; {$ENDIF} end;

procedure RegisterWinControl_R(Cl: TPSRuntimeClassImporter);
begin
  RIRegisterTWinControl(Cl);

{$IFNDEF UNICODE}
  with Cl.FindClass(FastUppercase(TWinControl.ClassName)) do
{$ELSE}
  with Cl.FindClass(AnsiString(TWinControl.ClassName)) do
{$ENDIF}
  begin
    RegisterPropertyHelper(@TWinControlParentBackground_R, @TWinControlParentBackground_W, 'ParentBackground');
  end;
end;

procedure RegisterNewStaticText_R(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TNewStaticText) do
  begin
    RegisterMethod(@TNewStaticText.AdjustHeight, 'AdjustHeight');
  end;
end;

procedure TNewCheckListBoxChecked_R(Self: TNewCheckListBox; var T: Boolean; t1: Integer); begin T := Self.Checked[t1]; end;
procedure TNewCheckListBoxChecked_W(Self: TNewCheckListBox; const T: Boolean; t1: Integer); begin Self.Checked[t1] := T; end;
procedure TNewCheckListBoxState_R(Self: TNewCheckListBox; var T: TCheckBoxState; t1: Integer); begin T := Self.State[t1]; end;
procedure TNewCheckListBoxItemCaption_R(Self: TNewCheckListBox; var T: String; t1: Integer); begin T := Self.ItemCaption[t1]; end;
procedure TNewCheckListBoxItemCaption_W(Self: TNewCheckListBox; const T: String; t1: Integer); begin Self.ItemCaption[t1] := T; end;
procedure TNewCheckListBoxItemEnabled_R(Self: TNewCheckListBox; var T: Boolean; t1: Integer); begin T := Self.ItemEnabled[t1]; end;
procedure TNewCheckListBoxItemEnabled_W(Self: TNewCheckListBox; const T: Boolean; t1: Integer); begin Self.ItemEnabled[t1] := T; end;
procedure TNewCheckListBoxItemLevel_R(Self: TNewCheckListBox; var T: Byte; t1: Integer); begin T := Self.ItemLevel[t1]; end;
procedure TNewCheckListBoxItemObject_R(Self: TNewCheckListBox; var T: TObject; t1: Integer); begin T := Self.ItemObject[t1]; end;
procedure TNewCheckListBoxItemObject_W(Self: TNewCheckListBox; const T: TObject; t1: Integer); begin Self.ItemObject[t1] := T; end;
procedure TNewCheckListBoxItemSubItem_R(Self: TNewCheckListBox; var T: String; t1: Integer); begin T := Self.ItemSubItem[t1]; end;
procedure TNewCheckListBoxItemSubItem_W(Self: TNewCheckListBox; const T: String; t1: Integer); begin Self.ItemSubItem[t1] := T; end;

procedure RegisterNewCheckListBox_R(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TNewCheckListBox) do
  begin
    RegisterMethod(@TNewCheckListBox.AddCheckBox, 'AddCheckBox');
    RegisterMethod(@TNewCheckListBox.AddGroup, 'AddGroup');
    RegisterMethod(@TNewCheckListBox.AddRadioButton, 'AddRadioButton');
    RegisterMethod(@TNewCheckListBox.CheckItem, 'CheckItem');
    RegisterPropertyHelper(@TNewCheckListBoxChecked_R, @TNewCheckListBoxChecked_W, 'Checked');
    RegisterPropertyHelper(@TNewCheckListBoxState_R, nil, 'State');
    RegisterPropertyHelper(@TNewCheckListBoxItemCaption_R, @TNewCheckListBoxItemCaption_W, 'ItemCaption');
    RegisterPropertyHelper(@TNewCheckListBoxItemEnabled_R, @TNewCheckListBoxItemEnabled_W, 'ItemEnabled');
    RegisterPropertyHelper(@TNewCheckListBoxItemLevel_R, nil, 'ItemLevel');
    RegisterPropertyHelper(@TNewCheckListBoxItemObject_R, @TNewCheckListBoxItemObject_W, 'ItemObject');
    RegisterPropertyHelper(@TNewCheckListBoxItemSubItem_R, @TNewCheckListBoxItemSubItem_W, 'ItemSubItem');
  end;
end;

procedure RegisterNewProgressBar_R(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TNewProgressBar);
end;

procedure TRichEditViewerRTFText_W(Self: TRichEditViewer; const T: AnsiString); begin Self.RTFText := T; end;

procedure RegisterRichEditViewer_R(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TRichEditViewer) do
  begin
    RegisterPropertyHelper(nil, @TRichEditViewerRTFText_W, 'RTFText');
  end;
end;

procedure RegisterPasswordEdit_R(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TPasswordEdit);
end;

procedure TCustomFolderTreeViewDirectory_W(Self: TCustomFolderTreeView; const T: String); begin Self.Directory := T; end;
procedure TCustomFolderTreeViewDirectory_R(Self: TCustomFolderTreeView; var T: String); begin T := Self.Directory; end;

procedure RegisterCustomFolderTreeView_R(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCustomFolderTreeView) do
  begin
    RegisterMethod(@TCustomFolderTreeView.ChangeDirectory, 'ChangeDirectory');
    RegisterMethod(@TCustomFolderTreeView.CreateNewDirectory, 'CreateNewDirectory');
    RegisterPropertyHelper(@TCustomFolderTreeViewDirectory_R,@TCustomFolderTreeViewDirectory_W,'Directory');
  end;
end;

procedure RegisterStartMenuFolderTreeView_R(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TStartMenuFolderTreeView) do
  begin
    RegisterMethod(@TStartMenuFolderTreeView.SetPaths, 'SetPaths');
  end;
end;

procedure RegisterFolderTreeView_R(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TFolderTreeView);
end;

procedure RegisterBitmapImage_R(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TBitmapImage);
end;

procedure RegisterBidiCtrls_R(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TNewEdit);
  Cl.Add(TNewMemo);
  Cl.Add(TNewComboBox);
  Cl.Add(TNewListBox);
  Cl.Add(TNewButton);
  Cl.Add(TNewCheckBox);
  Cl.Add(TNewRadioButton);
end;

procedure TNewNotebookPages_R(Self: TNewNotebook; var T: TNewNotebookPage; const t1: Integer); begin T := Self.Pages[t1]; end;
procedure TNewNotebookPageCount_R(Self: TNewNotebook; var T: Integer); begin T := Self.PageCount; end;

procedure RegisterNewNotebook_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TNewNotebook) do
  begin
    RegisterMethod(@TNewNotebook.FindNextPage, 'FindNextPage');
    RegisterPropertyHelper(@TNewNotebookPageCount_R,nil,'PageCount');
    RegisterPropertyHelper(@TNewNotebookPages_R,nil,'Pages');
  end;
end;

procedure TNewNotebookPageNotebook_W(Self: TNewNotebookPage; const T: TNewNotebook); begin Self.Notebook := T; end;
procedure TNewNotebookPageNotebook_R(Self: TNewNotebookPage; var T: TNewNotebook); begin T := Self.Notebook; end;

procedure RegisterNewNotebookPage_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TNewNotebookPage) do
  begin
    RegisterPropertyHelper(@TNewNotebookPageNotebook_R,@TNewNotebookPageNotebook_W,'Notebook');
  end;
end;

procedure RegisterUIStateForm_R(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TUIStateForm);
end;

procedure TSetupFormControlsFlipped_R(Self: TSetupForm; var T: Boolean); begin T := Self.ControlsFlipped; end;
procedure TSetupFormFlipControlsOnShow_W(Self: TSetupForm; const T: Boolean); begin Self.FlipControlsOnShow := T; end;
procedure TSetupFormFlipControlsOnShow_R(Self: TSetupForm; var T: Boolean); begin T := Self.FlipControlsOnShow; end;
procedure TSetupFormRightToLeft_R(Self: TSetupForm; var T: Boolean); begin T := Self.RightToLeft; end;

procedure RegisterSetupForm_R(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSetupForm) do
  begin
    RegisterMethod(@TSetupForm.Center, 'Center');
    RegisterMethod(@TSetupForm.CenterInsideControl, 'CenterInsideControl');
    RegisterPropertyHelper(@TSetupFormControlsFlipped_R, nil, 'ControlsFlipped');
    RegisterPropertyHelper(@TSetupFormFlipControlsOnShow_R, @TSetupFormFlipControlsOnShow_W, 'FlipControlsOnShow');
    RegisterPropertyHelper(@TSetupFormRightToLeft_R, nil, 'RightToLeft');
  end;
end;

procedure RegisterMainForm_R(Cl: TPSRuntimeClassImporter);
begin
  with CL.Add(TMainForm) do
  begin
    RegisterMethod(@TMainForm.ShowAboutBox, 'ShowAboutBox');
  end;
end;

procedure TWizardFormCancelButton_R(Self: TWizardForm; var T: TNewButton); begin T := Self.CancelButton; end;
procedure TWizardFormNextButton_R(Self: TWizardForm; var T: TNewButton); begin T := Self.NextButton; end;
procedure TWizardFormBackButton_R(Self: TWizardForm; var T: TNewButton); begin T := Self.BackButton; end;
procedure TWizardFormOuterNotebook_R(Self: TWizardForm; var T: TNewNotebook); begin T := Self.OuterNotebook; end;
procedure TWizardFormInnerNotebook_R(Self: TWizardForm; var T: TNewNotebook); begin T := Self.InnerNotebook; end;
procedure TWizardFormInfoAfterPage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.InfoAfterPage; end;
procedure TWizardFormInstallingPage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.InstallingPage; end;
procedure TWizardFormPreparingPage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.PreparingPage; end;
procedure TWizardFormReadyPage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.ReadyPage; end;
procedure TWizardFormSelectTasksPage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.SelectTasksPage; end;
procedure TWizardFormSelectProgramGroupPage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.SelectProgramGroupPage; end;
procedure TWizardFormSelectComponentsPage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.SelectComponentsPage; end;
procedure TWizardFormSelectDirPage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.SelectDirPage; end;
procedure TWizardFormUserInfoPage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.UserInfoPage; end;
procedure TWizardFormInfoBeforePage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.InfoBeforePage; end;
procedure TWizardFormPasswordPage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.PasswordPage; end;
procedure TWizardFormLicensePage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.LicensePage; end;
procedure TWizardFormFinishedPage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.FinishedPage; end;
procedure TWizardFormInnerPage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.InnerPage; end;
procedure TWizardFormWelcomePage_R(Self: TWizardForm; var T: TNewNotebookPage); begin T := Self.WelcomePage; end;
procedure TWizardFormDiskSpaceLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.DiskSpaceLabel; end;
procedure TWizardFormDirEdit_R(Self: TWizardForm; var T: TEdit); begin T := Self.DirEdit; end;
procedure TWizardFormGroupEdit_R(Self: TWizardForm; var T: TNewEdit); begin T := Self.GroupEdit; end;
procedure TWizardFormNoIconsCheck_R(Self: TWizardForm; var T: TNewCheckBox); begin T := Self.NoIconsCheck; end;
procedure TWizardFormPasswordLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.PasswordLabel; end;
procedure TWizardFormPasswordEdit_R(Self: TWizardForm; var T: TPASSWORDEDIT); begin T := Self.PasswordEdit; end;
procedure TWizardFormPasswordEditLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.PasswordEditLabel; end;
procedure TWizardFormReadyMemo_R(Self: TWizardForm; var T: TNewMemo); begin T := Self.ReadyMemo; end;
procedure TWizardFormTypesCombo_R(Self: TWizardForm; var T: TNewComboBox); begin T := Self.TypesCombo; end;
procedure TWizardFormBevel_R(Self: TWizardForm; var T: TBevel); begin T := Self.Bevel; end;
procedure TWizardFormWizardBitmapImage_R(Self: TWizardForm; var T: TBitmapImage); begin T := Self.WizardBitmapImage; end;
procedure TWizardFormWelcomeLabel1_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.WelcomeLabel1; end;
procedure TWizardFormInfoBeforeMemo_R(Self: TWizardForm; var T: TRichEditViewer); begin T := Self.InfoBeforeMemo; end;
procedure TWizardFormInfoBeforeClickLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.InfoBeforeClickLabel; end;
procedure TWizardFormMainPanel_R(Self: TWizardForm; var T: TPanel); begin T := Self.MainPanel; end;
procedure TWizardFormBevel1_R(Self: TWizardForm; var T: TBevel); begin T := Self.Bevel1; end;
procedure TWizardFormPageNameLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.PageNameLabel; end;
procedure TWizardFormPageDescriptionLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.PageDescriptionLabel; end;
procedure TWizardFormWizardSmallBitmapImage_R(Self: TWizardForm; var T: TBitmapImage); begin T := Self.WizardSmallBitmapImage; end;
procedure TWizardFormReadyLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.ReadyLabel; end;
procedure TWizardFormFinishedLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.FinishedLabel; end;
procedure TWizardFormYesRadio_R(Self: TWizardForm; var T: TNewRadioButton); begin T := Self.YesRadio; end;
procedure TWizardFormNoRadio_R(Self: TWizardForm; var T: TNewRadioButton); begin T := Self.NoRadio; end;
procedure TWizardFormWizardBitmapImage2_R(Self: TWizardForm; var T: TBitmapImage); begin T := Self.WizardBitmapImage2; end;
procedure TWizardFormWelcomeLabel2_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.WelcomeLabel2; end;
procedure TWizardFormLicenseLabel1_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.LicenseLabel1; end;
procedure TWizardFormLicenseMemo_R(Self: TWizardForm; var T: TRichEditViewer); begin T := Self.LicenseMemo; end;
procedure TWizardFormInfoAfterMemo_R(Self: TWizardForm; var T: TRichEditViewer); begin T := Self.InfoAfterMemo; end;
procedure TWizardFormInfoAfterClickLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.InfoAfterClickLabel; end;
procedure TWizardFormComponentsList_R(Self: TWizardForm; var T: TNewCheckListBox); begin T := Self.ComponentsList; end;
procedure TWizardFormComponentsDiskSpaceLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.ComponentsDiskSpaceLabel; end;
procedure TWizardFormBeveledLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.BeveledLabel; end;
procedure TWizardFormStatusLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.StatusLabel; end;
procedure TWizardFormFilenameLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.FilenameLabel; end;
procedure TWizardFormProgressGauge_R(Self: TWizardForm; var T: TNewProgressBar); begin T := Self.ProgressGauge; end;
procedure TWizardFormWebDownloadStatusLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.WebDownloadStatusLabel; end;
procedure TWizardFormWebDownloadFileNameLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.WebDownloadFileNameLabel; end;
procedure TWizardFormWebDownloadProgressGauge_R(Self: TWizardForm; var T: TNewProgressBar); begin T := Self.WebDownloadProgressGauge; end;
procedure TWizardFormSelectDirLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.SelectDirLabel; end;
procedure TWizardFormSelectStartMenuFolderLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.SelectStartMenuFolderLabel; end;
procedure TWizardFormSelectComponentsLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.SelectComponentsLabel; end;
procedure TWizardFormSelectTasksLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.SelectTasksLabel; end;
procedure TWizardFormLicenseAcceptedRadio_R(Self: TWizardForm; var T: TNewRadioButton); begin T := Self.LicenseAcceptedRadio; end;
procedure TWizardFormLicenseNotAcceptedRadio_R(Self: TWizardForm; var T: TNewRadioButton); begin T := Self.LicenseNotAcceptedRadio; end;
procedure TWizardFormUserInfoNameLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.UserInfoNameLabel; end;
procedure TWizardFormUserInfoNameEdit_R(Self: TWizardForm; var T: TNewEdit); begin T := Self.UserInfoNameEdit; end;
procedure TWizardFormUserInfoOrgLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.UserInfoOrgLabel; end;
procedure TWizardFormUserInfoOrgEdit_R(Self: TWizardForm; var T: TNewEdit); begin T := Self.UserInfoOrgEdit; end;
procedure TWizardFormPreparingErrorBitmapImage_R(Self: TWizardForm; var T: TBitmapImage); begin T := Self.PreparingErrorBitmapImage; end;
procedure TWizardFormPreparingLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.PreparingLabel; end;
procedure TWizardFormFinishedHeadingLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.FinishedHeadingLabel; end;
procedure TWizardFormUserInfoSerialLabel_R(Self: TWizardForm; var T: TNewStaticText); begin T := Self.UserInfoSerialLabel; end;
procedure TWizardFormUserInfoSerialEdit_R(Self: TWizardForm; var T: TNewEdit); begin T := Self.UserInfoSerialEdit; end;
procedure TWizardFormTasksList_R(Self: TWizardForm; var T: TNewCheckListBox); begin T := Self.TasksList; end;
procedure TWizardFormRunList_R(Self: TWizardForm; var T: TNewCheckListBox); begin T := Self.RunList; end;
procedure TWizardFormCurPageID_R(Self: TWizardForm; var T: Integer); begin T := Self.CurPageID; end;
procedure TWizardFormDirBrowseButton_R(Self: TWizardForm; var T: TNewButton); begin T := Self.DirBrowseButton; end;
procedure TWizardFormGroupBrowseButton_R(Self: TWizardForm; var T: TNewButton); begin T := Self.GroupBrowseButton; end;
procedure TWizardFormSelectDirBitmapImage(Self: TWizardForm; var T: TBitmapImage); begin T := Self.SelectDirBitmapImage; end;
procedure TWizardFormSelectGroupBitmapImage(Self: TWizardForm; var T: TBitmapImage); begin T := Self.SelectGroupBitmapImage; end;
procedure TWizardFormSelectDirBrowseLabel(Self: TWizardForm; var T: TNewStaticText); begin T := Self.SelectDirBrowseLabel; end;
procedure TWizardFormSelectStartMenuFolderBrowseLabel(Self: TWizardForm; var T: TNewStaticText); begin T := Self.SelectStartMenuFolderBrowseLabel; end;
procedure TWizardFormPreparingYesRadio_R(Self: TWizardForm; var T: TNewRadioButton); begin T := Self.PreparingYesRadio; end;
procedure TWizardFormPreparingNoRadio_R(Self: TWizardForm; var T: TNewRadioButton); begin T := Self.PreparingNoRadio; end;
procedure TWizardFormPreparingMemo_R(Self: TWizardForm; var T: TNewMemo); begin T := Self.PreparingMemo; end;
procedure TWizardFormPrevAppDir_R(Self: TWizardForm; var T: String); begin T := Self.PrevAppDir; end;

procedure RegisterWizardForm_R(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TWizardForm) do
  begin
    RegisterPropertyHelper(@TWizardFormCancelButton_R, nil, 'CancelButton');
    RegisterPropertyHelper(@TWizardFormNextButton_R, nil, 'NextButton');
    RegisterPropertyHelper(@TWizardFormBackButton_R, nil, 'BackButton');
    RegisterPropertyHelper(@TWizardFormOuterNotebook_R, nil, 'OuterNotebook');
    RegisterPropertyHelper(@TWizardFormInnerNotebook_R, nil, 'InnerNotebook');
    RegisterPropertyHelper(@TWizardFormWelcomePage_R,nil,'WelcomePage');
    RegisterPropertyHelper(@TWizardFormInnerPage_R,nil,'InnerPage');
    RegisterPropertyHelper(@TWizardFormFinishedPage_R,nil,'FinishedPage');
    RegisterPropertyHelper(@TWizardFormLicensePage_R,nil,'LicensePage');
    RegisterPropertyHelper(@TWizardFormPasswordPage_R,nil,'PasswordPage');
    RegisterPropertyHelper(@TWizardFormInfoBeforePage_R,nil,'InfoBeforePage');
    RegisterPropertyHelper(@TWizardFormUserInfoPage_R,nil,'UserInfoPage');
    RegisterPropertyHelper(@TWizardFormSelectDirPage_R,nil,'SelectDirPage');
    RegisterPropertyHelper(@TWizardFormSelectComponentsPage_R,nil,'SelectComponentsPage');
    RegisterPropertyHelper(@TWizardFormSelectProgramGroupPage_R,nil,'SelectProgramGroupPage');
    RegisterPropertyHelper(@TWizardFormSelectTasksPage_R,nil,'SelectTasksPage');
    RegisterPropertyHelper(@TWizardFormReadyPage_R,nil,'ReadyPage');
    RegisterPropertyHelper(@TWizardFormPreparingPage_R,nil,'PreparingPage');
    RegisterPropertyHelper(@TWizardFormInstallingPage_R,nil,'InstallingPage');
    RegisterPropertyHelper(@TWizardFormInfoAfterPage_R,nil,'InfoAfterPage');
    RegisterPropertyHelper(@TWizardFormDiskSpaceLabel_R, nil, 'DiskSpaceLabel');
    RegisterPropertyHelper(@TWizardFormDirEdit_R, nil, 'DirEdit');
    RegisterPropertyHelper(@TWizardFormGroupEdit_R, nil, 'GroupEdit');
    RegisterPropertyHelper(@TWizardFormNoIconsCheck_R, nil, 'NoIconsCheck');
    RegisterPropertyHelper(@TWizardFormPasswordLabel_R, nil, 'PasswordLabel');
    RegisterPropertyHelper(@TWizardFormPasswordEdit_R, nil, 'PasswordEdit');
    RegisterPropertyHelper(@TWizardFormPasswordEditLabel_R, nil, 'PasswordEditLabel');
    RegisterPropertyHelper(@TWizardFormReadyMemo_R, nil, 'ReadyMemo');
    RegisterPropertyHelper(@TWizardFormTypesCombo_R, nil, 'TypesCombo');
    RegisterPropertyHelper(@TWizardFormBevel_R, nil, 'Bevel');
    RegisterPropertyHelper(@TWizardFormWizardBitmapImage_R, nil, 'WizardBitmapImage');
    RegisterPropertyHelper(@TWizardFormWelcomeLabel1_R, nil, 'WelcomeLabel1');
    RegisterPropertyHelper(@TWizardFormInfoBeforeMemo_R, nil, 'InfoBeforeMemo');
    RegisterPropertyHelper(@TWizardFormInfoBeforeClickLabel_R, nil, 'InfoBeforeClickLabel');
    RegisterPropertyHelper(@TWizardFormMainPanel_R, nil, 'MainPanel');
    RegisterPropertyHelper(@TWizardFormBevel1_R, nil, 'Bevel1');
    RegisterPropertyHelper(@TWizardFormPageNameLabel_R, nil, 'PageNameLabel');
    RegisterPropertyHelper(@TWizardFormPageDescriptionLabel_R, nil, 'PageDescriptionLabel');
    RegisterPropertyHelper(@TWizardFormWizardSmallBitmapImage_R, nil, 'WizardSmallBitmapImage');
    RegisterPropertyHelper(@TWizardFormReadyLabel_R, nil, 'ReadyLabel');
    RegisterPropertyHelper(@TWizardFormFinishedLabel_R, nil, 'FinishedLabel');
    RegisterPropertyHelper(@TWizardFormYesRadio_R, nil, 'YesRadio');
    RegisterPropertyHelper(@TWizardFormNoRadio_R, nil, 'NoRadio');
    RegisterPropertyHelper(@TWizardFormWizardBitmapImage2_R, nil, 'WizardBitmapImage2');
    RegisterPropertyHelper(@TWizardFormWelcomeLabel2_R, nil, 'WelcomeLabel2');
    RegisterPropertyHelper(@TWizardFormLicenseLabel1_R, nil, 'LicenseLabel1');
    RegisterPropertyHelper(@TWizardFormLicenseMemo_R, nil, 'LicenseMemo');
    RegisterPropertyHelper(@TWizardFormInfoAfterMemo_R, nil, 'InfoAfterMemo');
    RegisterPropertyHelper(@TWizardFormInfoAfterClickLabel_R, nil, 'InfoAfterClickLabel');
    RegisterPropertyHelper(@TWizardFormComponentsList_R, nil, 'ComponentsList');
    RegisterPropertyHelper(@TWizardFormComponentsDiskSpaceLabel_R, nil, 'ComponentsDiskSpaceLabel');
    RegisterPropertyHelper(@TWizardFormBeveledLabel_R, nil, 'BeveledLabel');
    RegisterPropertyHelper(@TWizardFormStatusLabel_R, nil, 'StatusLabel');
    RegisterPropertyHelper(@TWizardFormFilenameLabel_R, nil, 'FilenameLabel');
    RegisterPropertyHelper(@TWizardFormProgressGauge_R, nil, 'ProgressGauge');
    RegisterPropertyHelper(@TWizardFormWebDownloadStatusLabel_R, nil, 'WebDownloadStatusLabel');
    RegisterPropertyHelper(@TWizardFormWebDownloadFilenameLabel_R, nil, 'WebDownloadFilenameLabel');
    RegisterPropertyHelper(@TWizardFormWebDownloadProgressGauge_R, nil, 'WebDownloadProgressGauge');
    RegisterPropertyHelper(@TWizardFormSelectDirLabel_R, nil, 'SelectDirLabel');
    RegisterPropertyHelper(@TWizardFormSelectStartMenuFolderLabel_R, nil, 'SelectStartMenuFolderLabel');
    RegisterPropertyHelper(@TWizardFormSelectComponentsLabel_R, nil, 'SelectComponentsLabel');
    RegisterPropertyHelper(@TWizardFormSelectTasksLabel_R, nil, 'SelectTasksLabel');
    RegisterPropertyHelper(@TWizardFormLicenseAcceptedRadio_R, nil, 'LicenseAcceptedRadio');
    RegisterPropertyHelper(@TWizardFormLicenseNotAcceptedRadio_R, nil, 'LicenseNotAcceptedRadio');
    RegisterPropertyHelper(@TWizardFormUserInfoNameLabel_R, nil, 'UserInfoNameLabel');
    RegisterPropertyHelper(@TWizardFormUserInfoNameEdit_R, nil, 'UserInfoNameEdit');
    RegisterPropertyHelper(@TWizardFormUserInfoOrgLabel_R, nil, 'UserInfoOrgLabel');
    RegisterPropertyHelper(@TWizardFormUserInfoOrgEdit_R, nil, 'UserInfoOrgEdit');
    RegisterPropertyHelper(@TWizardFormPreparingErrorBitmapImage_R, nil, 'PreparingErrorBitmapImage');
    RegisterPropertyHelper(@TWizardFormPreparingLabel_R, nil, 'PreparingLabel');
    RegisterPropertyHelper(@TWizardFormFinishedHeadingLabel_R, nil, 'FinishedHeadingLabel');
    RegisterPropertyHelper(@TWizardFormUserInfoSerialLabel_R, nil, 'UserInfoSerialLabel');
    RegisterPropertyHelper(@TWizardFormUserInfoSerialEdit_R, nil, 'UserInfoSerialEdit');
    RegisterPropertyHelper(@TWizardFormTasksList_R, nil, 'TasksList');
    RegisterPropertyHelper(@TWizardFormRunList_R, nil, 'RunList');
    RegisterPropertyHelper(@TWizardFormDirBrowseButton_R, nil, 'DirBrowseButton');
    RegisterPropertyHelper(@TWizardFormGroupBrowseButton_R, nil, 'GroupBrowseButton');
    RegisterPropertyHelper(@TWizardFormSelectDirBitmapImage, nil, 'SelectDirBitmapImage');
    RegisterPropertyHelper(@TWizardFormSelectGroupBitmapImage, nil, 'SelectGroupBitmapImage');
    RegisterPropertyHelper(@TWizardFormSelectDirBrowseLabel, nil, 'SelectDirBrowseLabel');
    RegisterPropertyHelper(@TWizardFormSelectStartMenuFolderBrowseLabel, nil,'SelectStartMenuFolderBrowseLabel');
    RegisterPropertyHelper(@TWizardFormPreparingYesRadio_R, nil, 'PreparingYesRadio');
    RegisterPropertyHelper(@TWizardFormPreparingNoRadio_R, nil, 'PreparingNoRadio');
    RegisterPropertyHelper(@TWizardFormPreparingMemo_R, nil, 'PreparingMemo');
    RegisterPropertyHelper(@TWizardFormCurPageID_R, nil, 'CurPageID');
    RegisterMethod(@TWizardForm.AdjustLabelHeight, 'AdjustLabelHeight');
    RegisterMethod(@TWizardForm.IncTopDecHeight, 'IncTopDecHeight');
    RegisterPropertyHelper(@TWizardFormPrevAppDir_R, nil, 'PrevAppDir');
  end;
end;

procedure TUninstallProgressFormOuterNotebook_R(Self: TUninstallProgressForm; var T: TNewNotebook); begin T := Self.OuterNotebook; end;
procedure TUninstallProgressFormInnerPage_R(Self: TUninstallProgressForm; var T: TNewNotebookPage); begin T := Self.InnerPage; end;
procedure TUninstallProgressFormInnerNotebook_R(Self: TUninstallProgressForm; var T: TNewNotebook); begin T := Self.InnerNotebook; end;
procedure TUninstallProgressFormInstallingPage_R(Self: TUninstallProgressForm; var T: TNewNotebookPage); begin T := Self.InstallingPage; end;
procedure TUninstallProgressFormMainPanel_R(Self: TUninstallProgressForm; var T: TPanel); begin T := Self.MainPanel; end;
procedure TUninstallProgressFormPageNameLabel_R(Self: TUninstallProgressForm; var T: TNewStaticText); begin T := Self.PageNameLabel; end;
procedure TUninstallProgressFormPageDescriptionLabel_R(Self: TUninstallProgressForm; var T: TNewStaticText); begin T := Self.PageDescriptionLabel; end;
procedure TUninstallProgressFormWizardSmallBitmapImage_R(Self: TUninstallProgressForm; var T: TBitmapImage); begin T := Self.WizardSmallBitmapImage; end;
procedure TUninstallProgressFormBevel1_R(Self: TUninstallProgressForm; var T: TBevel); begin T := Self.Bevel1; end;
procedure TUninstallProgressFormStatusLabel_R(Self: TUninstallProgressForm; var T: TNewStaticText); begin T := Self.StatusLabel; end;
procedure TUninstallProgressFormProgressBar_R(Self: TUninstallProgressForm; var T: TNewProgressBar); begin T := Self.ProgressBar; end;
procedure TUninstallProgressFormBeveledLabel_R(Self: TUninstallProgressForm; var T: TNewStaticText); begin T := Self.BeveledLabel; end;
procedure TUninstallProgressFormBevel_R(Self: TUninstallProgressForm; var T: TBevel); begin T := Self.Bevel; end;
procedure TUninstallProgressFormCancelButton_R(Self: TUninstallProgressForm; var T: TNewButton); begin T := Self.CancelButton; end;

procedure RegisterUninstallProgressForm_R(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TUninstallProgressForm) do
  begin
    RegisterPropertyHelper(@TUninstallProgressFormOuterNotebook_R, nil, 'OuterNotebook');
    RegisterPropertyHelper(@TUninstallProgressFormInnerPage_R, nil, 'InnerPage');
    RegisterPropertyHelper(@TUninstallProgressFormInnerNotebook_R, nil, 'InnerNotebook');
    RegisterPropertyHelper(@TUninstallProgressFormInstallingPage_R, nil, 'InstallingPage');
    RegisterPropertyHelper(@TUninstallProgressFormMainPanel_R, nil, 'MainPanel');
    RegisterPropertyHelper(@TUninstallProgressFormPageNameLabel_R, nil, 'PageNameLabel');
    RegisterPropertyHelper(@TUninstallProgressFormPageDescriptionLabel_R, nil, 'PageDescriptionLabel');
    RegisterPropertyHelper(@TUninstallProgressFormWizardSmallBitmapImage_R, nil, 'WizardSmallBitmapImage');
    RegisterPropertyHelper(@TUninstallProgressFormBevel1_R, nil, 'Bevel1');
    RegisterPropertyHelper(@TUninstallProgressFormStatusLabel_R, nil, 'StatusLabel');
    RegisterPropertyHelper(@TUninstallProgressFormProgressBar_R, nil, 'ProgressBar');
    RegisterPropertyHelper(@TUninstallProgressFormBeveledLabel_R, nil, 'BeveledLabel');
    RegisterPropertyHelper(@TUninstallProgressFormBevel_R, nil, 'Bevel');
    RegisterPropertyHelper(@TUninstallProgressFormCancelButton_R, nil, 'CancelButton');
  end;
end;

procedure TWizardPageID_R(Self: TWizardPage; var T: Integer); begin T := Self.ID; end;
procedure TWizardPageCaption_R(Self: TWizardPage; var T: String); begin T := Self.Caption; end;
procedure TWizardPageCaption_W(Self: TWizardPage; T: String); begin Self.Caption := T; end;
procedure TWizardPageDescription_R(Self: TWizardPage; var T: String); begin T := Self.Description; end;
procedure TWizardPageDescription_W(Self: TWizardPage; T: String); begin Self.Description := T; end;
procedure TWizardPageSurface_R(Self: TWizardPage; var T: TNewNotebookPage); begin T := Self.Surface; end;
procedure TWizardPageSurfaceHeight_R(Self: TWizardPage; var T: Integer); begin T := Self.SurfaceHeight; end;
procedure TWizardPageSurfaceWidth_R(Self: TWizardPage; var T: Integer); begin T := Self.SurfaceWidth; end;
procedure TWizardPageOnActivate_R(Self: TWizardPage; var T: TWizardPageNotifyEvent); begin T := Self.OnActivate; end;
procedure TWizardPageOnActivate_W(Self: TWizardPage; T: TWizardPageNotifyEvent); begin Self.OnActivate := T; end;
procedure TWizardPageOnBackButtonClick_R(Self: TWizardPage; var T: TWizardPageButtonEvent); begin T := Self.OnBackButtonClick; end;
procedure TWizardPageOnBackButtonClick_W(Self: TWizardPage; T: TWizardPageButtonEvent); begin Self.OnBackButtonClick := T; end;
procedure TWizardPageOnCancelButtonClick_R(Self: TWizardPage; var T: TWizardPageCancelEvent); begin T := Self.OnCancelButtonClick; end;
procedure TWizardPageOnCancelButtonClick_W(Self: TWizardPage; T: TWizardPageCancelEvent); begin Self.OnCancelButtonClick := T; end;
procedure TWizardPageOnNextButtonClick_R(Self: TWizardPage; var T: TWizardPageButtonEvent); begin T := Self.OnNextButtonClick; end;
procedure TWizardPageOnNextButtonClick_W(Self: TWizardPage; T: TWizardPageButtonEvent); begin Self.OnNextButtonClick := T; end;
procedure TWizardPageOnShouldSkipPage_R(Self: TWizardPage; var T: TWizardPageShouldSkipEvent); begin T := Self.OnShouldSkipPage; end;
procedure TWizardPageOnShouldSkipPage_W(Self: TWizardPage; T: TWizardPageShouldSkipEvent); begin Self.OnShouldSkipPage := T; end;

procedure RegisterWizardPage_R(Cl: TIFPSRuntimeClassImporter);
begin
  with Cl.Add(TWizardPage) do
  begin
    RegisterPropertyHelper(@TWizardPageID_R, nil, 'ID');
    RegisterPropertyHelper(@TWizardPageCaption_R, @TWizardPageCaption_W, 'Caption');
    RegisterPropertyHelper(@TWizardPageDescription_R, @TWizardPageDescription_W, 'Description');
    RegisterPropertyHelper(@TWizardPageSurface_R, nil, 'Surface');
    RegisterPropertyHelper(@TWizardPageSurfaceHeight_R, nil, 'SurfaceHeight');
    RegisterPropertyHelper(@TWizardPageSurfaceWidth_R, nil, 'SurfaceWidth');
    RegisterPropertyHelper(@TWizardPageOnActivate_R, @TWizardPageOnActivate_W, 'OnActivate');
    RegisterPropertyHelper(@TWizardPageOnBackButtonClick_R, @TWizardPageOnBackButtonClick_W, 'OnBackButtonClick');
    RegisterPropertyHelper(@TWizardPageOnCancelButtonClick_R, @TWizardPageOnCancelButtonClick_W, 'OnCancelButtonClick');
    RegisterPropertyHelper(@TWizardPageOnNextButtonClick_R, @TWizardPageOnNextButtonClick_W, 'OnNextButtonClick');
    RegisterPropertyHelper(@TWizardPageOnShouldSkipPage_R, @TWizardPageOnShouldSkipPage_W, 'OnShouldSkipPage');
  end;
end;

procedure TInputQueryWizardPageEdits_R(Self: TInputQueryWizardPage; var T: TPasswordEdit; const t1: Integer); begin T := Self.Edits[t1]; end;
procedure TInputQueryWizardPagePromptLabels_R(Self: TInputQueryWizardPage; var T: TNewStaticText; const t1: Integer); begin T := Self.PromptLabels[t1]; end;
procedure TInputQueryWizardPageValues_R(Self: TInputQueryWizardPage; var T: String; const t1: Integer); begin T := Self.Values[t1]; end;
procedure TInputQueryWizardPageSubCaptionLabel_R(Self: TInputQueryWizardPage; var T: TNewStaticText); begin T := Self.SubCaptionLabel; end;
procedure TInputQueryWizardPageValues_W(Self: TInputQueryWizardPage; const T: String; const t1: Integer); begin Self.Values[t1] := T; end;

procedure RegisterInputQueryWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TInputQueryWizardPage) do
  begin
    RegisterMethod(@TInputQueryWizardPage.Add, 'Add');
    RegisterPropertyHelper(@TInputQueryWizardPageEdits_R,nil,'Edits');
    RegisterPropertyHelper(@TInputQueryWizardPagePromptLabels_R,nil,'PromptLabels');
    RegisterPropertyHelper(@TInputQueryWizardPageSubcaptionLabel_R,nil,'SubCaptionLabel');
    RegisterPropertyHelper(@TInputQueryWizardPageValues_R,@TInputQueryWizardPageValues_W,'Values');
  end;
end;

procedure TInputOptionWizardPageCheckListBox_R(Self: TInputOptionWizardPage; var T: TNewCheckListBox); begin T := Self.CheckListBox; end;
procedure TInputOptionWizardPageSelectedValueIndex_R(Self: TInputOptionWizardPage; var T: Integer); begin T := Self.SelectedValueIndex; end;
procedure TInputOptionWizardPageSelectedValueIndex_W(Self: TInputOptionWizardPage; const T: Integer); begin Self.SelectedValueIndex := T; end;
procedure TInputOptionWizardPageSubCaptionLabel_R(Self: TInputOptionWizardPage; var T: TNewStaticText); begin T := Self.SubCaptionLabel; end;
procedure TInputOptionWizardPageValues_W(Self: TInputOptionWizardPage; const T: Boolean; const t1: Integer); begin Self.Values[t1] := T; end;
procedure TInputOptionWizardPageValues_R(Self: TInputOptionWizardPage; var T: Boolean; const t1: Integer); begin T := Self.Values[t1]; end;

procedure RegisterInputOptionWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TInputOptionWizardPage) do
  begin
    RegisterMethod(@TInputOptionWizardPage.Add, 'Add');
    RegisterMethod(@TInputOptionWizardPage.AddEx, 'AddEx');
    RegisterPropertyHelper(@TInputOptionWizardPageCheckListBox_R,nil,'CheckListBox');
    RegisterPropertyHelper(@TInputOptionWizardPageSelectedValueIndex_R,@TInputOptionWizardPageSelectedValueIndex_W,'SelectedValueIndex');
    RegisterPropertyHelper(@TInputOptionWizardPageSubcaptionLabel_R,nil,'SubCaptionLabel');
    RegisterPropertyHelper(@TInputOptionWizardPageValues_R,@TInputOptionWizardPageValues_W,'Values');
  end;
end;

procedure TInputDirWizardPageButtons_R(Self: TInputDirWizardPage; var T: TNewButton; const t1: Integer); begin T := Self.Buttons[t1]; end;
procedure TInputDirWizardPageEdits_R(Self: TInputDirWizardPage; var T: TEdit; const t1: Integer); begin T := Self.Edits[t1]; end;
procedure TInputDirWizardPagePromptLabels_R(Self: TInputDirWizardPage; var T: TNewStaticText; const t1: Integer); begin T := Self.PromptLabels[t1]; end;
procedure TInputDirWizardPageSubCaptionLabel_R(Self: TInputDirWizardPage; var T: TNewStaticText); begin T := Self.SubCaptionLabel; end;
procedure TInputDirWizardPageValues_W(Self: TInputDirWizardPage; const T: String; const t1: Integer); begin Self.Values[t1] := T; end;
procedure TInputDirWizardPageValues_R(Self: TInputDirWizardPage; var T: String; const t1: Integer); begin T := Self.Values[t1]; end;

procedure RegisterInputDirWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TInputDirWizardPage) do
  begin
    RegisterMethod(@TInputDirWizardPage.Add, 'Add');
    RegisterPropertyHelper(@TInputDirWizardPageButtons_R,nil,'Buttons');
    RegisterPropertyHelper(@TInputDirWizardPageEdits_R,nil,'Edits');
    RegisterPropertyHelper(@TInputDirWizardPagePromptLabels_R,nil,'PromptLabels');
    RegisterPropertyHelper(@TInputDirWizardPageSubcaptionLabel_R,nil,'SubCaptionLabel');
    RegisterPropertyHelper(@TInputDirWizardPageValues_R,@TInputDirWizardPageValues_W,'Values');
  end;
end;

procedure TInputFileWizardPageButtons_R(Self: TInputFileWizardPage; var T: TNewButton; const t1: Integer); begin T := Self.Buttons[t1]; end;
procedure TInputFileWizardPagePromptLabels_R(Self: TInputFileWizardPage; var T: TNewStaticText; const t1: Integer); begin T := Self.PromptLabels[t1]; end;
procedure TInputFileWizardPageEdits_R(Self: TInputFileWizardPage; var T: TEdit; const t1: Integer); begin T := Self.Edits[t1]; end;
procedure TInputFileWizardPageSubCaptionLabel_R(Self: TInputFileWizardPage; var T: TNewStaticText); begin T := Self.SubCaptionLabel; end;
procedure TInputFileWizardPageValues_W(Self: TInputFileWizardPage; const T: String; const t1: Integer); begin Self.Values[t1] := T; end;
procedure TInputFileWizardPageValues_R(Self: TInputFileWizardPage; var T: String; const t1: Integer); begin T := Self.Values[t1]; end;
procedure TInputFileWizardPageIsSaveButton_W(Self: TInputFileWizardPage; const T: Boolean; const t1: Integer); begin Self.IsSaveButton[t1] := T; end;
procedure TInputFileWizardPageIsSaveButton_R(Self: TInputFileWizardPage; var T: Boolean; const t1: Integer); begin T := Self.IsSaveButton[t1]; end;

procedure RegisterInputFileWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TInputFileWizardPage) do
  begin
    RegisterMethod(@TInputFileWizardPage.Add, 'Add');
    RegisterPropertyHelper(@TInputFileWizardPageButtons_R,nil,'Buttons');
    RegisterPropertyHelper(@TInputFileWizardPageEdits_R,nil,'Edits');
    RegisterPropertyHelper(@TInputFileWizardPagePromptLabels_R,nil,'PromptLabels');
    RegisterPropertyHelper(@TInputFileWizardPageSubcaptionLabel_R,nil,'SubCaptionLabel');
    RegisterPropertyHelper(@TInputFileWizardPageValues_R,@TInputFileWizardPageValues_W,'Values');
    RegisterPropertyHelper(@TInputFileWizardPageIsSaveButton_R,@TInputFileWizardPageIsSaveButton_W,'IsSaveButton');
  end;
end;

procedure TOutputMsgWizardPageMsgLabel_R(Self: TOutputMsgWizardPage; var T: TNewStaticText); begin T := Self.MsgLabel; end;

procedure RegisterOutputMsgWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TOutputMsgWizardPage) do
  begin
    RegisterPropertyHelper(@TOutputMsgWizardPageMsgLabel_R,nil,'MsgLabel');
  end;
end;

procedure TOutputMsgMemoWizardPageRichEditViewer_R(Self: TOutputMsgMemoWizardPage; var T: TRichEditViewer); begin T := Self.RichEditViewer; end;
procedure TOutputMsgMemoWizardPageSubCaptionLabel_R(Self: TOutputMsgMemoWizardPage; var T: TNewStaticText); begin T := Self.SubCaptionLabel; end;

procedure RegisterOutputMsgMemoWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TOutputMsgMemoWizardPage) do
  begin
    RegisterPropertyHelper(@TOutputMsgMemoWizardPageRichEditViewer_R,nil,'RichEditViewer');
    RegisterPropertyHelper(@TOutputMsgMemoWizardPageSubcaptionLabel_R,nil,'SubCaptionLabel');
  end;
end;

procedure TOutputProgressWizardPageMsg1Label_R(Self: TOutputProgressWizardPage; var T: TNewStaticText); begin T := Self.Msg1Label; end;
procedure TOutputProgressWizardPageMsg2Label_R(Self: TOutputProgressWizardPage; var T: TNewStaticText); begin T := Self.Msg2Label; end;
procedure TOutputProgressWizardPageProgressBar_R(Self: TOutputProgressWizardPage; var T: TNewProgressBar); begin T := Self.ProgressBar; end;

procedure RegisterOutputProgressWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TOutputProgressWizardPage) do
  begin
    RegisterMethod(@TOutputProgressWizardPage.Hide, 'Hide');
    RegisterPropertyHelper(@TOutputProgressWizardPageMsg1Label_R,nil,'Msg1Label');
    RegisterPropertyHelper(@TOutputProgressWizardPageMsg2Label_R,nil,'Msg2Label');
    RegisterPropertyHelper(@TOutputProgressWizardPageProgressBar_R,nil,'ProgressBar');
    RegisterMethod(@TOutputProgressWizardPage.SetProgress, 'SetProgress');
    RegisterMethod(@TOutputProgressWizardPage.SetText, 'SetText');
    RegisterMethod(@TOutputProgressWizardPage.Show, 'Show');
  end;
end;

procedure RegisterHandCursor_R(Cl: TPSRuntimeClassImporter);
const
  IDC_HAND = MakeIntResource(32649);
begin
  Screen.Cursors[crHand] := LoadCursor(0, IDC_HAND);
end;

function ScriptClassesLibraryRegister_R(ScriptInterpreter: TPSExec): TPSRuntimeClassImporter;
var
  Cl: TPSRuntimeClassImporter;
begin
  Cl := TPSRuntimeClassImporter.Create();
  try
    { Std }
    RIRegisterTObject(Cl);
    RIRegisterTPersistent(Cl);
    RIRegisterTComponent(Cl);

    { Classes }
    RIRegisterTStream(Cl);
    RIRegisterTStrings(Cl, True);
    RIRegisterTStringList(Cl);
    RIRegisterTHandleStream(Cl);
    RIRegisterTFileStream(Cl);

    { Graphics }
    RIRegisterTGraphicsObject(Cl);
    RIRegisterTFont(Cl);
    RIRegisterTCanvas(Cl);
    RIRegisterTPen(Cl);
    RIRegisterTBrush(Cl);
    RIRegisterTGraphic(Cl);
    RIRegisterTBitmap(Cl, True);

    { Controls }
    RIRegisterTControl(Cl);
    RegisterWinControl_R(Cl);
    RIRegisterTGraphicControl(Cl);
    RIRegisterTCustomControl(Cl);
    RIRegister_TDragObject(Cl);

    { Forms }
    RIRegisterTScrollingWinControl(Cl);
    RIRegisterTForm(Cl);

    { StdCtrls }
    RIRegisterTCustomLabel(Cl);
    RIRegisterTLabel(Cl);
    RIRegisterTCustomEdit(Cl);
    RIRegisterTEdit(Cl);
    RIRegisterTCustomMemo(Cl);
    RIRegisterTMemo(Cl);
    RIRegisterTCustomComboBox(Cl);
    RIRegisterTComboBox(Cl);
    RIRegisterTButtonControl(Cl);
    RIRegisterTButton(Cl);
    RIRegisterTCustomCheckBox(Cl);
    RIRegisterTCheckBox(Cl);
    RIRegisterTRadioButton(Cl);
    RIRegisterTCustomListBox(Cl);
    RIRegisterTListBox(Cl);

    { ExtCtrls }
    RIRegisterTBevel(Cl);
    RIRegisterTCustomPanel(Cl);
    RIRegisterTPanel(Cl);

    { ComObj }
    RIRegister_ComObj(ScriptInterpreter);

    RegisterNewStaticText_R(Cl);
    RegisterNewCheckListBox_R(Cl);
    RegisterNewProgressBar_R(Cl);
    RegisterRichEditViewer_R(Cl);
    RegisterPasswordEdit_R(Cl);
    RegisterCustomFolderTreeView_R(Cl);
    RegisterFolderTreeView_R(Cl);
    RegisterStartMenuFolderTreeView_R(Cl);
    RegisterBitmapImage_R(Cl);
    RegisterBidiCtrls_R(Cl);

    RegisterNewNotebook_R(Cl);
    RegisterNewNotebookPage_R(Cl);

    RegisterUIStateForm_R(Cl);
    RegisterSetupForm_R(Cl);
    RegisterMainForm_R(Cl);
    RegisterWizardForm_R(Cl);
    RegisterUninstallProgressForm_R(Cl);

    RegisterWizardPage_R(Cl);
    RegisterInputQueryWizardPage_R(Cl);
    RegisterInputOptionWizardPage_R(Cl);
    RegisterInputDirWizardPage_R(Cl);
    RegisterInputFileWizardPage_R(Cl);
    RegisterOutputMsgWizardPage_R(Cl);
    RegisterOutputMsgMemoWizardPage_R(Cl);
    RegisterOutputProgressWizardPage_R(Cl);

    RegisterHandCursor_R(Cl);

    RegisterClassLibraryRuntime(ScriptInterpreter, Cl);
  except
    Cl.Free;
    raise;
  end;

  Result := Cl;
end;

end.
