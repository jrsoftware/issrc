unit Setup.ScriptClasses;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Script support classes (run time)
}

interface

uses
  uPSRuntime;

function ScriptClassesLibraryRegister_R(ScriptInterpreter: TPSExec): TPSRuntimeClassImporter;
procedure ScriptClassesLibraryUpdateVars(ScriptInterpreter: TIFPSExec);

implementation

uses
  Windows, Controls, Forms, StdCtrls, Graphics,
  uPSR_std, uPSR_classes, uPSR_graphics, uPSR_controls, uPSR_forms,
  uPSR_stdctrls, uPSR_extctrls, uPSR_comobj,
  NewStaticText, NewCheckListBox, NewProgressBar, RichEditViewer,
  ExtCtrls, UIStateForm, Setup.SetupForm, Setup.MainForm, Setup.WizardForm, Shared.SetupTypes, PasswordEdit,
  FolderTreeView, BitmapImage, NewNotebook, Setup.ScriptDlg, BidiCtrls,
  Setup.UninstallProgressForm;

type
  TWinControlAccess = class(TWinControl);

procedure TWinControlParentBackground_R(Self: TWinControl; var T: Boolean); begin T := TWinControlAccess(Self).ParentBackground; end;
procedure TWinControlParentBackground_W(Self: TWinControl; const T: Boolean); begin TWinControlAccess(Self).ParentBackground := T; end;

procedure RegisterWinControl_R(Cl: TPSRuntimeClassImporter);
begin
  RIRegisterTWinControl(Cl);

  with Cl.FindClass(AnsiString(TWinControl.ClassName)) do
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
procedure TNewCheckListBoxItemFontStyle_R(Self: TNewCheckListBox; var T: TFontStyles; const t1: Integer); begin T := Self.ItemFontStyle[t1]; end;
procedure TNewCheckListBoxItemFontStyle_W(Self: TNewCheckListBox; const T: TFontStyles; const t1: Integer); begin Self.ItemFontStyle[t1] := T; end;
procedure TNewCheckListBoxSubItemFontStyle_R(Self: TNewCheckListBox; var T: TFontStyles; const t1: Integer); begin T := Self.SubItemFontStyle[t1]; end;
procedure TNewCheckListBoxSubItemFontStyle_W(Self: TNewCheckListBox; const T: TFontStyles; const t1: Integer); begin Self.SubItemFontStyle[t1] := T; end;

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
    RegisterPropertyHelper(@TNewCheckListBoxItemFontStyle_R, @TNewCheckListBoxItemFontStyle_W, 'ItemFontStyle');
    RegisterPropertyHelper(@TNewCheckListBoxSubItemFontStyle_R, @TNewCheckListBoxSubItemFontStyle_W, 'SubItemFontStyle');
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

procedure TBitmapAlphaFormat_W(Self: TBitmap; const T: TAlphaFormat); begin Self.AlphaFormat := T; end;
procedure TBitmapAlphaFormat_R(Self: TBitmap; var T: TAlphaFormat); begin T := Self.AlphaFormat; end;

procedure RegisterBitmapImage_R(Cl: TPSRuntimeClassImporter);
begin
  with Cl.FindClass('TBitmap') do
  begin
    RegisterPropertyHelper(@TBitmapAlphaFormat_R, @TBitmapAlphaFormat_W, 'AlphaFormat');
  end;
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
  with Cl.Add(TNewLinkLabel) do
  begin
    RegisterMethod(@TNewLinkLabel.AdjustHeight, 'AdjustHeight');
  end;
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

procedure RegisterSetupForm_R(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSetupForm) do
  begin
    RegisterMethod(@TSetupForm.CalculateButtonWidth, 'CalculateButtonWidth');
    RegisterMethod(@TSetupForm.ShouldSizeX, 'ShouldSizeX');
    RegisterMethod(@TSetupForm.ShouldSizeY, 'ShouldSizeY');
    RegisterMethod(@TSetupForm.FlipSizeAndCenterIfNeeded, 'FlipSizeAndCenterIfNeeded');
  end;
end;

procedure RegisterWizardForm_R(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TWizardForm) do
  begin
    RegisterMethod(@TWizardForm.AdjustLabelHeight, 'AdjustLabelHeight');
    RegisterMethod(@TWizardForm.AdjustLinkLabelHeight, 'AdjustLinkLabelHeight');
    RegisterMethod(@TWizardForm.IncTopDecHeight, 'IncTopDecHeight');
  end;
end;

procedure RegisterUninstallProgressForm_R(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TUninstallProgressForm);
end;

procedure RegisterWizardPage_R(Cl: TIFPSRuntimeClassImporter);
begin
  Cl.Add(TWizardPage);
end;

procedure TInputQueryWizardPageEdits_R(Self: TInputQueryWizardPage; var T: TPasswordEdit; const t1: Integer); begin T := Self.Edits[t1]; end;
procedure TInputQueryWizardPagePromptLabels_R(Self: TInputQueryWizardPage; var T: TNewStaticText; const t1: Integer); begin T := Self.PromptLabels[t1]; end;
procedure TInputQueryWizardPageValues_R(Self: TInputQueryWizardPage; var T: String; const t1: Integer); begin T := Self.Values[t1]; end;
procedure TInputQueryWizardPageValues_W(Self: TInputQueryWizardPage; const T: String; const t1: Integer); begin Self.Values[t1] := T; end;

procedure RegisterInputQueryWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TInputQueryWizardPage) do
  begin
    RegisterMethod(@TInputQueryWizardPage.Add, 'Add');
    RegisterPropertyHelper(@TInputQueryWizardPageEdits_R,nil,'Edits');
    RegisterPropertyHelper(@TInputQueryWizardPagePromptLabels_R,nil,'PromptLabels');
    RegisterPropertyHelper(@TInputQueryWizardPageValues_R,@TInputQueryWizardPageValues_W,'Values');
  end;
end;

procedure TInputOptionWizardPageSelectedValueIndex_R(Self: TInputOptionWizardPage; var T: Integer); begin T := Self.SelectedValueIndex; end;
procedure TInputOptionWizardPageSelectedValueIndex_W(Self: TInputOptionWizardPage; const T: Integer); begin Self.SelectedValueIndex := T; end;
procedure TInputOptionWizardPageValues_W(Self: TInputOptionWizardPage; const T: Boolean; const t1: Integer); begin Self.Values[t1] := T; end;
procedure TInputOptionWizardPageValues_R(Self: TInputOptionWizardPage; var T: Boolean; const t1: Integer); begin T := Self.Values[t1]; end;

procedure RegisterInputOptionWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TInputOptionWizardPage) do
  begin
    RegisterMethod(@TInputOptionWizardPage.Add, 'Add');
    RegisterMethod(@TInputOptionWizardPage.AddEx, 'AddEx');
    RegisterPropertyHelper(@TInputOptionWizardPageSelectedValueIndex_R,@TInputOptionWizardPageSelectedValueIndex_W,'SelectedValueIndex');
    RegisterPropertyHelper(@TInputOptionWizardPageValues_R,@TInputOptionWizardPageValues_W,'Values');
  end;
end;

procedure TInputDirWizardPageButtons_R(Self: TInputDirWizardPage; var T: TNewButton; const t1: Integer); begin T := Self.Buttons[t1]; end;
procedure TInputDirWizardPageEdits_R(Self: TInputDirWizardPage; var T: TEdit; const t1: Integer); begin T := Self.Edits[t1]; end;
procedure TInputDirWizardPagePromptLabels_R(Self: TInputDirWizardPage; var T: TNewStaticText; const t1: Integer); begin T := Self.PromptLabels[t1]; end;
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
    RegisterPropertyHelper(@TInputDirWizardPageValues_R,@TInputDirWizardPageValues_W,'Values');
  end;
end;

procedure TInputFileWizardPageButtons_R(Self: TInputFileWizardPage; var T: TNewButton; const t1: Integer); begin T := Self.Buttons[t1]; end;
procedure TInputFileWizardPagePromptLabels_R(Self: TInputFileWizardPage; var T: TNewStaticText; const t1: Integer); begin T := Self.PromptLabels[t1]; end;
procedure TInputFileWizardPageEdits_R(Self: TInputFileWizardPage; var T: TEdit; const t1: Integer); begin T := Self.Edits[t1]; end;
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
    RegisterPropertyHelper(@TInputFileWizardPageValues_R,@TInputFileWizardPageValues_W,'Values');
    RegisterPropertyHelper(@TInputFileWizardPageIsSaveButton_R,@TInputFileWizardPageIsSaveButton_W,'IsSaveButton');
  end;
end;

procedure RegisterOutputMsgWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  CL.Add(TOutputMsgWizardPage);
end;

procedure RegisterOutputMsgMemoWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  CL.Add(TOutputMsgMemoWizardPage);
end;

procedure RegisterOutputProgressWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TOutputProgressWizardPage) do
  begin
    RegisterMethod(@TOutputProgressWizardPage.Hide, 'Hide');
    RegisterMethod(@TOutputProgressWizardPage.SetProgress, 'SetProgress');
    RegisterMethod(@TOutputProgressWizardPage.SetText, 'SetText');
    RegisterMethod(@TOutputProgressWizardPage.Show, 'Show');
  end;
end;

procedure RegisterOutputMarqueeProgressWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TOutputMarqueeProgressWizardPage) do
  begin
    RegisterMethod(@TOutputMarqueeProgressWizardPage.Animate, 'Animate');
    RegisterMethod(@TOutputMarqueeProgressWizardPage.SetProgress, 'SetProgress');
  end;
end;

procedure RegisterDownloadWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TDownloadWizardPage) do
  begin
    RegisterMethod(@TDownloadWizardPage.Add, 'Add');
    RegisterMethod(@TDownloadWizardPage.AddEx, 'AddEx');
    RegisterMethod(@TDownloadWizardPage.Clear, 'Clear');
    RegisterMethod(@TDownloadWizardPage.Download, 'Download');
    RegisterMethod(@TDownloadWizardPage.Show, 'Show');
  end;
end;

procedure RegisterExtractionWizardPage_R(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TExtractionWizardPage) do
  begin
    RegisterMethod(@TExtractionWizardPage.Add, 'Add');
    RegisterMethod(@TExtractionWizardPage.Clear, 'Clear');
    RegisterMethod(@TExtractionWizardPage.Extract, 'Extract');
    RegisterMethod(@TExtractionWizardPage.Show, 'Show');
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
    RIRegisterTStringStream(Cl);

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
    RIRegisterTSizeConstraints(cl);

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
    RIRegisterTCustomLinkLabel(Cl);
    RIRegisterTLinkLabel(Cl);

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
    RegisterOutputMarqueeProgressWizardPage_R(Cl);
    RegisterDownloadWizardPage_R(Cl);
    RegisterExtractionWizardPage_R(Cl);

    RegisterHandCursor_R(Cl);

    RegisterClassLibraryRuntime(ScriptInterpreter, Cl);
  except
    Cl.Free;
    raise;
  end;

  Result := Cl;
end;

procedure ScriptClassesLibraryUpdateVars(ScriptInterpreter: TIFPSExec);
begin
  SetVariantToClass(ScriptInterpreter.GetVarNo(ScriptInterpreter.GetVar('WIZARDFORM')), WizardForm);
  SetVariantToClass(ScriptInterpreter.GetVarNo(ScriptInterpreter.GetVar('UNINSTALLPROGRESSFORM')), UninstallProgressForm);
end;

end.
