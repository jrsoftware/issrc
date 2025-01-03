unit Compiler.ScriptClasses;

{
  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Script support classes (compile time)
}

interface

uses
  uPSCompiler;

procedure ScriptClassesLibraryRegister_C(Cl: TPSPascalCompiler);

implementation

uses
  Windows, Shared.SetupTypes,
  uPSC_std, uPSC_classes, uPSC_graphics, uPSC_controls, uPSC_stdctrls,
  uPSC_forms, uPSC_extctrls, uPSC_comobj;

procedure RegisterWinControl_C(Cl: TPSPascalCompiler);
begin
  SIRegisterTWinControl(Cl);

  with Cl.FindClass('TWinControl') do
  begin
    RegisterProperty('ParentBackground', 'Boolean', iptrw);
  end;
end;

procedure RegisterNewStaticText_C(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TWinControl'), 'TNewStaticText') do
  begin
    RegisterMethod('function AdjustHeight: Integer');
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('AutoSize', 'Boolean', iptrw);
    RegisterProperty('Caption', 'String', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('FocusControl', 'TWinControl', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('ShowAccelChar', 'Boolean', iptrw);
    RegisterProperty('WordWrap', 'Boolean', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);

    {$IFNDEF PS_MINIVCL}
    RegisterProperty('DragCursor', 'Longint', iptrw);
    RegisterProperty('DragMode', 'TDragMode', iptrw);
    RegisterProperty('ParentShowHint', 'Boolean', iptrw);
    RegisterProperty('PopupMenu', 'TPopupMenu', iptrw);
    RegisterProperty('OnDragDrop', 'TDragDropEvent', iptrw);
    RegisterProperty('OnDragOver', 'TDragOverEvent', iptrw);
    RegisterProperty('OnEndDrag', 'TEndDragEvent', iptrw);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnStartDrag', 'TStartDragEvent', iptrw);
    {$ENDIF}
  end;
end;

procedure RegisterNewCheckListBox_C(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TCheckItemOperation', '(coUncheck, coCheck, coCheckWithChildren)');
  with Cl.AddClassN(Cl.FindClass('TCustomListBox'), 'TNewCheckListBox') do
  begin
    RegisterMethod('function AddCheckBox(const ACaption, ASubItem: String; ALevel: Byte; AChecked, AEnabled, AHasInternalChildren, ACheckWhenParentChecked: Boolean; AObject: TObject): Integer');
    RegisterMethod('function AddGroup(const ACaption, ASubItem: String; ALevel: Byte; AObject: TObject): Integer');
    RegisterMethod('function AddRadioButton(const ACaption, ASubItem: String; ALevel: Byte; AChecked, AEnabled: Boolean; AObject: TObject): Integer');
    RegisterMethod('function CheckItem(const Index: Integer; const AOperation: TCheckItemOperation): Boolean');
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('Checked', 'Boolean Integer', iptrw);
    RegisterProperty('State', 'TCheckBoxState Integer', iptr);
    RegisterProperty('ItemCaption', 'String Integer', iptrw);
    RegisterProperty('ItemEnabled', 'Boolean Integer', iptrw);
    RegisterProperty('ItemLevel', 'Byte Integer', iptr);
    RegisterProperty('ItemObject', 'TObject Integer', iptrw);
    RegisterProperty('ItemSubItem', 'String Integer', iptrw);
    RegisterProperty('ItemFontStyle', 'TFontStyles Integer', iptrw);
    RegisterProperty('SubItemFontStyle', 'TFontStyles Integer', iptrw);
    RegisterProperty('Flat', 'Boolean', iptrw);
    RegisterProperty('MinItemHeight', 'Integer', iptrw);
    RegisterProperty('Offset', 'Integer', iptrw);
    RegisterProperty('OnClickCheck', 'TNotifyEvent', iptrw);
    RegisterProperty('BorderStyle', 'TBorderStyle', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnKeyDown', 'TKeyEvent', iptrw);
    RegisterProperty('OnKeyPress', 'TKeyPressEvent', iptrw);
    RegisterProperty('OnKeyUp', 'TKeyEvent', iptrw);
    RegisterProperty('ShowLines', 'Boolean', iptrw);
    RegisterProperty('WantTabs', 'Boolean', iptrw);
    RegisterProperty('RequireRadioSelection', 'Boolean', iptrw);
    RegisterProperty('OnEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnExit', 'TNotifyEvent', iptrw);

    {$IFNDEF PS_MINIVCL}
    RegisterProperty('Ctl3D', 'Boolean', iptrw);
    RegisterProperty('DragCursor', 'Longint', iptrw);
    RegisterProperty('DragMode', 'TDragMode', iptrw);
    RegisterProperty('ParentCtl3D', 'Boolean', iptrw);
    RegisterProperty('ParentShowHint', 'Boolean', iptrw);
    RegisterProperty('PopupMenu', 'TPopupMenu', iptrw);
    RegisterProperty('OnDragDrop', 'TDragDropEvent', iptrw);
    RegisterProperty('OnDragOver', 'TDragOverEvent', iptrw);
    RegisterProperty('OnEndDrag', 'TEndDragEvent', iptrw);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnStartDrag', 'TStartDragEvent', iptrw);
    {$ENDIF}
  end;
end;

procedure RegisterNewProgressBar_C(Cl: TPSPascalCompiler);
begin
  cl.AddTypeS('TNewProgressBarState', '(npbsNormal, npbsError, npbsPaused)');
  cl.AddTypeS('TNewProgressBarStyle', '(npbstNormal, npbstMarquee)');
  with Cl.AddClassN(Cl.FindClass('TWinControl'), 'TNewProgressBar') do
  begin
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('Min', 'Longint', iptrw);
    RegisterProperty('Max', 'Longint', iptrw);
    RegisterProperty('Position', 'Longint', iptrw);
    RegisterProperty('State', 'TNewProgressBarState', iptrw);
    RegisterProperty('Style', 'TNewProgressBarStyle', iptrw);
  end;
end;

procedure RegisterRichEditViewer_C(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TMemo'), 'TRichEditViewer') do
  begin
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('BevelKind', 'TBevelKind', iptrw);
    RegisterProperty('BorderStyle', 'TBorderStyle', iptrw);
    RegisterProperty('RTFText', 'AnsiString', iptw);
    RegisterProperty('UseRichEdit', 'Boolean', iptrw);
  end;
end;

procedure RegisterPasswordEdit_C(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomEdit'), 'TPasswordEdit') do
  begin
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('AutoSelect', 'Boolean', iptrw);
    RegisterProperty('AutoSize', 'Boolean', iptrw);
    RegisterProperty('BorderStyle', 'TBorderStyle', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('HideSelection', 'Boolean', iptrw);
    RegisterProperty('MaxLength', 'Integer', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('Password', 'Boolean', iptrw);
    RegisterProperty('ReadOnly', 'Boolean', iptrw);
    RegisterProperty('Text', 'string', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnKeyDown', 'TKeyEvent', iptrw);
    RegisterProperty('OnKeyPress', 'TKeyPressEvent', iptrw);
    RegisterProperty('OnKeyUp', 'TKeyEvent', iptrw);
    RegisterProperty('OnEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnExit', 'TNotifyEvent', iptrw);

    {$IFNDEF PS_MINIVCL}
    RegisterProperty('CharCase', 'TEditCharCase', iptrw);
    RegisterProperty('Ctl3D', 'Boolean', iptrw);
    RegisterProperty('DragCursor', 'Longint', iptrw);
    RegisterProperty('DragMode', 'TDragMode', iptrw);
    RegisterProperty('OEMConvert', 'Boolean', iptrw);
    RegisterProperty('ParentCtl3D', 'Boolean', iptrw);
    RegisterProperty('ParentShowHint', 'Boolean', iptrw);
    RegisterProperty('PopupMenu', 'TPopupMenu', iptrw);
    RegisterProperty('OnDragDrop', 'TDragDropEvent', iptrw);
    RegisterProperty('OnDragOver', 'TDragOverEvent', iptrw);
    RegisterProperty('OnEndDrag', 'TEndDragEvent', iptrw);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnStartDrag', 'TStartDragEvent', iptrw);
    {$ENDIF}
  end;
end;

procedure RegisterCustomFolderTreeView_C(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TWinControl'),'TCustomFolderTreeView') do
  begin
    RegisterMethod('procedure ChangeDirectory(const Value: String; const CreateNewItems: Boolean)');
    RegisterMethod('procedure CreateNewDirectory(const ADefaultName: String)');
    RegisterProperty('Directory', 'String', iptrw);
  end;
  CL.AddTypeS('TFolderRenameEvent', 'procedure(Sender: TCustomFolderTreeView; var NewName: String; var Accept: Boolean)');
end;

procedure RegisterFolderTreeView_C(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TCustomFolderTreeView'),'TFolderTreeView') do
  begin
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
    RegisterProperty('OnRename', 'TFolderRenameEvent', iptrw);
  end;
end;

procedure RegisterStartMenuFolderTreeView_C(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TCustomFolderTreeView'),'TStartMenuFolderTreeView') do
  begin
    RegisterMethod('procedure SetPaths(const AUserPrograms, ACommonPrograms, AUserStartup, ACommonStartup: String)');
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
    RegisterProperty('OnRename', 'TFolderRenameEvent', iptrw);
  end;
end;

procedure RegisterBitmapImage_C(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TAlphaFormat', '(afIgnored, afDefined, afPremultiplied)');
  with Cl.FindClass('TBitmap') do
  begin
    RegisterProperty('AlphaFormat', 'TAlphaFormat', iptrw);
  end;
  with Cl.AddClassN(CL.FindClass('TGraphicControl'),'TBitmapImage') do
  begin
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('AutoSize', 'Boolean', iptrw);
    RegisterProperty('BackColor', 'TColor', iptrw);
    RegisterProperty('Center', 'Boolean', iptrw);
    RegisterProperty('Bitmap', 'TBitmap', iptrw);
    RegisterProperty('ReplaceColor', 'TColor', iptrw);
    RegisterProperty('ReplaceWithColor', 'TColor', iptrw);
    RegisterProperty('Stretch', 'Boolean', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
  end;
end;

procedure RegisterBidiCtrls_C(Cl: TPSPascalCompiler);
begin
  Cl.AddClassN(Cl.FindClass('TEdit'), 'TNewEdit');
  Cl.AddClassN(Cl.FindClass('TMemo'), 'TNewMemo');
  Cl.AddClassN(Cl.FindClass('TComboBox'), 'TNewComboBox');
  Cl.AddClassN(Cl.FindClass('TListBox'), 'TNewListBox');
  Cl.AddClassN(Cl.FindClass('TButton'), 'TNewButton');
  Cl.AddClassN(Cl.FindClass('TCheckBox'), 'TNewCheckBox');
  Cl.AddClassN(Cl.FindClass('TRadioButton'), 'TNewRadioButton');
  with Cl.AddClassN(Cl.FindClass('TLinkLabel'), 'TNewLinkLabel') do
  begin
    RegisterMethod('function AdjustHeight: Integer');
  end;
end;

procedure RegisterNewNotebook_C(Cl: TPSPascalCompiler);
begin
  Cl.AddClassN(Cl.FindClass('TCustomControl'),'TNewNotebookPage');

  with Cl.AddClassN(Cl.FindClass('TWinControl'),'TNewNotebook') do
  begin
    RegisterMethod('function FindNextPage(CurPage: TNewNotebookPage; GoForward: Boolean): TNewNotebookPage');
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('PageCount', 'Integer', iptr);
    RegisterProperty('Pages', 'TNewNotebookPage Integer', iptr);
    RegisterProperty('ActivePage', 'TNewNotebookPage', iptrw);
  end;
end;

procedure RegisterNewNotebookPage_C(Cl: TPSPascalCompiler);
begin
  with Cl.FindClass('TNewNotebookPage') do
  begin
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Notebook', 'TNewNotebook', iptrw);
    RegisterProperty('PageIndex', 'Integer', iptrw);
  end;
end;

procedure RegisterUIStateForm_C(Cl: TPSPascalCompiler);
begin
  Cl.AddClassN(Cl.FindClass('TForm'), 'TUIStateForm');
end;

procedure RegisterSetupForm_C(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TUIStateForm'), 'TSetupForm') do
  begin
    RegisterMethod('function CalculateButtonWidth(const ButtonCaptions: array of String): Integer;');
    RegisterMethod('function ShouldSizeX: Boolean;');
    RegisterMethod('function ShouldSizeY: Boolean;');
    RegisterMethod('procedure FlipSizeAndCenterIfNeeded(const ACenterInsideControl: Boolean; const CenterInsideControlCtl: TWinControl; const CenterInsideControlInsideClientArea: Boolean)');
    RegisterProperty('ControlsFlipped', 'Boolean', iptr);
    RegisterProperty('FlipControlsOnShow', 'Boolean', iptrw);
    RegisterProperty('KeepSizeY', 'Boolean', iptrw);
    RegisterProperty('RightToLeft', 'Boolean', iptr);
    RegisterProperty('SizeAndCenterOnShow', 'Boolean', iptrw);
  end;
end;

procedure RegisterWizardForm_C(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TSetupForm'), 'TWizardForm') do
  begin
    RegisterProperty('CancelButton', 'TNewButton', iptr);
    RegisterProperty('NextButton', 'TNewButton', iptr);
    RegisterProperty('BackButton', 'TNewButton', iptr);
    RegisterProperty('OuterNotebook', 'TNewNotebook', iptr);
    RegisterProperty('InnerNotebook', 'TNewNotebook', iptr);
    RegisterProperty('WelcomePage', 'TNewNotebookPage', iptr);
    RegisterProperty('InnerPage', 'TNewNotebookPage', iptr);
    RegisterProperty('FinishedPage', 'TNewNotebookPage', iptr);
    RegisterProperty('LicensePage', 'TNewNotebookPage', iptr);
    RegisterProperty('PasswordPage', 'TNewNotebookPage', iptr);
    RegisterProperty('InfoBeforePage', 'TNewNotebookPage', iptr);
    RegisterProperty('UserInfoPage', 'TNewNotebookPage', iptr);
    RegisterProperty('SelectDirPage', 'TNewNotebookPage', iptr);
    RegisterProperty('SelectComponentsPage', 'TNewNotebookPage', iptr);
    RegisterProperty('SelectProgramGroupPage', 'TNewNotebookPage', iptr);
    RegisterProperty('SelectTasksPage', 'TNewNotebookPage', iptr);
    RegisterProperty('ReadyPage', 'TNewNotebookPage', iptr);
    RegisterProperty('PreparingPage', 'TNewNotebookPage', iptr);
    RegisterProperty('InstallingPage', 'TNewNotebookPage', iptr);
    RegisterProperty('InfoAfterPage', 'TNewNotebookPage', iptr);
    RegisterProperty('DiskSpaceLabel', 'TNewStaticText', iptr);
    RegisterProperty('DirEdit', 'TEdit', iptr);
    RegisterProperty('GroupEdit', 'TNewEdit', iptr);
    RegisterProperty('NoIconsCheck', 'TNewCheckBox', iptr);
    RegisterProperty('PasswordLabel', 'TNewStaticText', iptr);
    RegisterProperty('PasswordEdit', 'TPasswordEdit', iptr);
    RegisterProperty('PasswordEditLabel', 'TNewStaticText', iptr);
    RegisterProperty('ReadyMemo', 'TNewMemo', iptr);
    RegisterProperty('TypesCombo', 'TNewComboBox', iptr);
    RegisterProperty('Bevel', 'TBevel', iptr);
    RegisterProperty('WizardBitmapImage', 'TBitmapImage', iptr);
    RegisterProperty('WelcomeLabel1', 'TNewStaticText', iptr);
    RegisterProperty('InfoBeforeMemo', 'TRichEditViewer', iptr);
    RegisterProperty('InfoBeforeClickLabel', 'TNewStaticText', iptr);
    RegisterProperty('MainPanel', 'TPanel', iptr);
    RegisterProperty('Bevel1', 'TBevel', iptr);
    RegisterProperty('PageNameLabel', 'TNewStaticText', iptr);
    RegisterProperty('PageDescriptionLabel', 'TNewStaticText', iptr);
    RegisterProperty('WizardSmallBitmapImage', 'TBitmapImage', iptr);
    RegisterProperty('ReadyLabel', 'TNewStaticText', iptr);
    RegisterProperty('FinishedLabel', 'TNewStaticText', iptr);
    RegisterProperty('YesRadio', 'TNewRadioButton', iptr);
    RegisterProperty('NoRadio', 'TNewRadioButton', iptr);
    RegisterProperty('WizardBitmapImage2', 'TBitmapImage', iptr);
    RegisterProperty('WelcomeLabel2', 'TNewStaticText', iptr);
    RegisterProperty('LicenseLabel1', 'TNewStaticText', iptr);
    RegisterProperty('LicenseMemo', 'TRichEditViewer', iptr);
    RegisterProperty('InfoAfterMemo', 'TRichEditViewer', iptr);
    RegisterProperty('InfoAfterClickLabel', 'TNewStaticText', iptr);
    RegisterProperty('ComponentsList', 'TNewCheckListBox', iptr);
    RegisterProperty('ComponentsDiskSpaceLabel', 'TNewStaticText', iptr);
    RegisterProperty('BeveledLabel', 'TNewStaticText', iptr);
    RegisterProperty('StatusLabel', 'TNewStaticText', iptr);
    RegisterProperty('FilenameLabel', 'TNewStaticText', iptr);
    RegisterProperty('ProgressGauge', 'TNewProgressBar', iptr);
    RegisterProperty('SelectDirLabel', 'TNewStaticText', iptr);
    RegisterProperty('SelectStartMenuFolderLabel', 'TNewStaticText', iptr);
    RegisterProperty('SelectComponentsLabel', 'TNewStaticText', iptr);
    RegisterProperty('SelectTasksLabel', 'TNewStaticText', iptr);
    RegisterProperty('LicenseAcceptedRadio', 'TNewRadioButton', iptr);
    RegisterProperty('LicenseNotAcceptedRadio', 'TNewRadioButton', iptr);
    RegisterProperty('UserInfoNameLabel', 'TNewStaticText', iptr);
    RegisterProperty('UserInfoNameEdit', 'TNewEdit', iptr);
    RegisterProperty('UserInfoOrgLabel', 'TNewStaticText', iptr);
    RegisterProperty('UserInfoOrgEdit', 'TNewEdit', iptr);
    RegisterProperty('PreparingErrorBitmapImage', 'TBitmapImage', iptr);
    RegisterProperty('PreparingLabel', 'TNewStaticText', iptr);
    RegisterProperty('FinishedHeadingLabel', 'TNewStaticText', iptr);
    RegisterProperty('UserInfoSerialLabel', 'TNewStaticText', iptr);
    RegisterProperty('UserInfoSerialEdit', 'TNewEdit', iptr);
    RegisterProperty('TasksList', 'TNewCheckListBox', iptr);
    RegisterProperty('RunList', 'TNewCheckListBox', iptr);
    RegisterProperty('DirBrowseButton', 'TNewButton', iptr);
    RegisterProperty('GroupBrowseButton', 'TNewButton', iptr);
    RegisterProperty('SelectDirBitmapImage', 'TBitmapImage', iptr);
    RegisterProperty('SelectGroupBitmapImage', 'TBitmapImage', iptr);
    RegisterProperty('SelectDirBrowseLabel', 'TNewStaticText', iptr);
    RegisterProperty('SelectStartMenuFolderBrowseLabel', 'TNewStaticText', iptr);
    RegisterProperty('PreparingYesRadio', 'TNewRadioButton', iptr);
    RegisterProperty('PreparingNoRadio', 'TNewRadioButton', iptr);
    RegisterProperty('PreparingMemo', 'TNewMemo', iptr);
    RegisterProperty('CurPageID', 'Integer', iptr);
    RegisterMethod('function AdjustLabelHeight(ALabel: TNewStaticText): Integer');
    RegisterMethod('function AdjustLinkLabelHeight(ALinkLabel: TNewLinkLabel): Integer');
    RegisterMethod('procedure IncTopDecHeight(AControl: TControl; Amount: Integer)');
    RegisterProperty('PrevAppDir', 'String', iptr);
  end;
end;

procedure RegisterUninstallProgressForm_C(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TSetupForm'), 'TUninstallProgressForm') do
  begin
    RegisterProperty('OuterNotebook', 'TNewNotebook', iptr);
    RegisterProperty('InnerPage', 'TNewNotebookPage', iptr);
    RegisterProperty('InnerNotebook', 'TNewNotebook', iptr);
    RegisterProperty('InstallingPage', 'TNewNotebookPage', iptr);
    RegisterProperty('MainPanel', 'TPanel', iptr);
    RegisterProperty('PageNameLabel', 'TNewStaticText', iptr);
    RegisterProperty('PageDescriptionLabel', 'TNewStaticText', iptr);
    RegisterProperty('WizardSmallBitmapImage', 'TBitmapImage', iptr);
    RegisterProperty('Bevel1', 'TBevel', iptr);
    RegisterProperty('StatusLabel', 'TNewStaticText', iptr);
    RegisterProperty('ProgressBar', 'TNewProgressBar', iptr);
    RegisterProperty('BeveledLabel', 'TNewStaticText', iptr);
    RegisterProperty('Bevel', 'TBevel', iptr);
    RegisterProperty('CancelButton', 'TNewButton', iptr);
  end;
end;

procedure RegisterWizardPage_C(Cl: TIFPSPascalCompiler);
var
  NewClass: TPSCompileTimeClass;
begin
  NewClass := Cl.AddClassN(Cl.FindClass('TComponent'), 'TWizardPage');
  CL.AddTypeS('TWizardPageNotifyEvent', 'procedure(Sender: TWizardPage)');
  CL.AddTypeS('TWizardPageButtonEvent', 'function(Sender: TWizardPage): Boolean');
  CL.AddTypeS('TWizardPageCancelEvent', 'procedure(Sender: TWizardPage; var ACancel, AConfirm: Boolean)');
  CL.AddTypeS('TWizardPageShouldSkipEvent', 'function(Sender: TWizardPage): Boolean');
  with NewClass do
  begin
    RegisterProperty('ID', 'Integer', iptr);
    RegisterProperty('Caption', 'String', iptrw);
    RegisterProperty('Description', 'String', iptrw);
    RegisterProperty('Surface', 'TNewNotebookPage', iptr);
    RegisterProperty('SurfaceColor', 'TColor', iptr);
    RegisterProperty('SurfaceHeight', 'Integer', iptr);
    RegisterProperty('SurfaceWidth', 'Integer', iptr);
    RegisterProperty('OnActivate', 'TWizardPageNotifyEvent', iptrw);
    RegisterProperty('OnBackButtonClick', 'TWizardPageButtonEvent', iptrw);
    RegisterProperty('OnCancelButtonClick', 'TWizardPageCancelEvent', iptrw);
    RegisterProperty('OnNextButtonClick', 'TWizardPageButtonEvent', iptrw);
    RegisterProperty('OnShouldSkipPage', 'TWizardPageShouldSkipEvent', iptrw);
  end;
end;

procedure RegisterInputQueryWizardPage_C(Cl: TPSPascalCompiler);
begin
  with CL.AddClassN(Cl.FindClass('TWizardPage'),'TInputQueryWizardPage') do
  begin
    RegisterMethod('function Add(const APrompt: String; const APassword: Boolean): Integer');
    RegisterProperty('Edits', 'TPasswordEdit Integer', iptr);
    RegisterProperty('PromptLabels', 'TNewStaticText Integer', iptr);
    RegisterProperty('SubCaptionLabel', 'TNewStaticText', iptr);
    RegisterProperty('Values', 'String Integer', iptrw);
  end;
end;

procedure RegisterInputOptionWizardPage_C(Cl: TPSPascalCompiler);
begin
  with CL.AddClassN(Cl.FindClass('TWizardPage'),'TInputOptionWizardPage') do
  begin
    RegisterMethod('function Add(const ACaption: String): Integer');
    RegisterMethod('function AddEx(const ACaption: String; const ALevel: Byte; const AExclusive: Boolean): Integer');
    RegisterProperty('CheckListBox', 'TNewCheckListBox', iptr);
    RegisterProperty('SelectedValueIndex', 'Integer', iptrw);
    RegisterProperty('SubCaptionLabel', 'TNewStaticText', iptr);
    RegisterProperty('Values', 'Boolean Integer', iptrw);
  end;
end;

procedure RegisterInputDirWizardPage_C(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TWizardPage'),'TInputDirWizardPage') do
  begin
    RegisterMethod('function Add(const APrompt: String): Integer');
    RegisterProperty('Buttons', 'TNewButton Integer', iptr);
    RegisterProperty('Edits', 'TEdit Integer', iptr);
    RegisterProperty('NewFolderName', 'String', iptrw);
    RegisterProperty('PromptLabels', 'TNewStaticText Integer', iptr);
    RegisterProperty('SubCaptionLabel', 'TNewStaticText', iptr);
    RegisterProperty('Values', 'String Integer', iptrw);
  end;
end;

procedure RegisterInputFileWizardPage_C(Cl: TPSPascalCompiler);
begin
  with CL.AddClassN(Cl.FindClass('TWizardPage'),'TInputFileWizardPage') do
  begin
    RegisterMethod('function Add(const APrompt, AFilter, ADefaultExtension: String): Integer');
    RegisterProperty('Buttons', 'TNewButton Integer', iptr);
    RegisterProperty('Edits', 'TEdit Integer', iptr);
    RegisterProperty('PromptLabels', 'TNewStaticText Integer', iptr);
    RegisterProperty('SubCaptionLabel', 'TNewStaticText', iptr);
    RegisterProperty('Values', 'String Integer', iptrw);
    RegisterProperty('IsSaveButton', 'Boolean Integer', iptrw);
  end;
end;

procedure RegisterOutputMsgWizardPage_C(Cl: TPSPascalCompiler);
begin
  with CL.AddClassN(Cl.FindClass('TWizardPage'),'TOutputMsgWizardPage') do
  begin
    RegisterProperty('MsgLabel', 'TNewStaticText', iptr);
  end;
end;

procedure RegisterOutputMsgMemoWizardPage_C(Cl: TPSPascalCompiler);
begin
  with CL.AddClassN(Cl.FindClass('TWizardPage'),'TOutputMsgMemoWizardPage') do
  begin
    RegisterProperty('RichEditViewer', 'TRichEditViewer', iptr);
    RegisterProperty('SubCaptionLabel', 'TNewStaticText', iptr);
  end;
end;

procedure RegisterOutputProgressWizardPage_C(Cl: TPSPascalCompiler);
begin
  with CL.AddClassN(Cl.FindClass('TWizardPage'),'TOutputProgressWizardPage') do
  begin
    RegisterMethod('procedure Hide');
    RegisterProperty('Msg1Label', 'TNewStaticText', iptr);
    RegisterProperty('Msg2Label', 'TNewStaticText', iptr);
    RegisterProperty('ProgressBar', 'TNewProgressBar', iptr);
    RegisterMethod('procedure SetProgress(const Position, Max: Longint)');
    RegisterMethod('procedure SetText(const Msg1, Msg2: String)');
    RegisterMethod('procedure Show');
  end;
end;

procedure RegisterOutputMarqueeProgressWizardPage_C(Cl: TPSPascalCompiler);
begin
  with CL.AddClassN(Cl.FindClass('TOutputProgressWizardPage'),'TOutputMarqueeProgressWizardPage') do
  begin
    RegisterMethod('procedure Animate');
    RegisterMethod('procedure SetProgress(const Position, Max: Longint)'); { Only used to stop the script from called TOutputProgressWizardPage.SetProgress }
  end;
end;

procedure RegisterDownloadWizardPage_C(Cl: TPSPascalCompiler);
begin
  with CL.AddClassN(Cl.FindClass('TOutputProgressWizardPage'),'TDownloadWizardPage') do
  begin
    RegisterProperty('AbortButton', 'TNewButton', iptr);
    RegisterProperty('AbortedByUser', 'Boolean', iptr);
    RegisterProperty('ShowBaseNameInsteadOfUrl', 'Boolean', iptrw);
    RegisterMethod('procedure Add(const Url, BaseName, RequiredSHA256OfFile: String)');
    RegisterMethod('procedure AddEx(const Url, BaseName, RequiredSHA256OfFile, UserName, Password: String)');
    RegisterMethod('procedure Clear');
    RegisterMethod('function Download: Int64');
    RegisterMethod('procedure Show'); { Without this TOutputProgressWizardPage's Show will be called }
  end;
end;

procedure RegisterExtractionWizardPage_C(Cl: TPSPascalCompiler);
begin
  with CL.AddClassN(Cl.FindClass('TOutputProgressWizardPage'),'TExtractionWizardPage') do
  begin
    RegisterProperty('AbortButton', 'TNewButton', iptr);
    RegisterProperty('AbortedByUser', 'Boolean', iptr);
    RegisterProperty('ShowArchiveInsteadOfFile', 'Boolean', iptrw);
    RegisterMethod('procedure Add(const ArchiveFileName, DestDir: String; const FullPaths: Boolean)');
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure Extract');
    RegisterMethod('procedure Show'); { Without this TOutputProgressWizardPage's Show will be called }
  end;
end;

procedure RegisterHandCursor_C(Cl: TPSPascalCompiler);
begin
  cl.AddConstantN('crHand', 'Integer').Value.ts32 := crHand;
end;

procedure ScriptClassesLibraryRegister_C(Cl: TPSPascalCompiler);
const
  clSystemColor = $FF000000;
begin
  { Std }
  SIRegister_Std_TypesAndConsts(Cl);
  SIRegisterTObject(Cl);
  SIRegisterTPersistent(Cl);
  SIRegisterTComponent(Cl);

  { Classes }
  SIRegister_Classes_TypesAndConsts(Cl);
  SIRegisterTStream(Cl);
  SIRegisterTStrings(Cl, True);
  SIRegisterTStringList(Cl);
  SIRegisterTHandleStream(Cl);
  SIRegisterTFileStream(Cl);
  SIRegisterTStringStream(Cl);

  { Graphics }
  SIRegister_Graphics_TypesAndConsts(Cl);
  cl.AddConstantN('clHotLight', 'Integer').Value.ts32 := Integer(clSystemColor or COLOR_HOTLIGHT);
  SIRegisterTGraphicsObject(Cl);
  SIRegisterTFont(Cl);
  SIRegisterTPen(Cl);
  SIRegisterTBrush(Cl);
  SIRegisterTCanvas(Cl);
  SIRegisterTGraphic(Cl);
  SIRegisterTBitmap(Cl, True);

  { Controls }
  SIRegister_Controls_TypesAndConsts(Cl);
  SIRegisterTDragObject(Cl);
  SIRegisterTSizeConstraints(Cl);
  SIRegisterTControl(Cl);
  RegisterWinControl_C(Cl);
  SIRegisterTGraphicControl(Cl);
  SIRegisterTCustomControl(Cl);

  { Forms }
  SIRegister_Forms_TypesAndConsts(Cl);
  SIRegisterTScrollingWinControl(Cl);
  SIRegisterTForm(Cl);

  { StdCtrls }
  SIRegister_StdCtrls_TypesAndConsts(Cl);
  SIRegisterTCustomLabel(Cl);
  SIRegisterTLabel(Cl);
  SIRegisterTCustomEdit(Cl);
  SIRegisterTEdit(Cl);
  SIRegisterTCustomMemo(Cl);
  SIRegisterTMemo(Cl);
  SIRegisterTCustomComboBox(Cl);
  SIRegisterTComboBox(Cl);
  SIRegisterTButtonControl(Cl);
  SIRegisterTButton(Cl);
  SIRegisterTCustomCheckBox(Cl);
  SIRegisterTCheckBox(Cl);
  SIRegisterTRadioButton(Cl);
  SIRegisterTCustomListBox(Cl);
  SIRegisterTListBox(Cl);

  { ExtCtrls }
  SIRegister_ExtCtrls_TypesAndConsts(cl);
  SIRegisterTBevel(Cl);
  SIRegisterTCustomPanel(Cl);
  SIRegisterTPanel(Cl);
  SIRegisterTCustomLinkLabel(Cl);
  SIRegisterTLinkLabel(Cl);

  { ComObj }
  SIRegister_ComObj(Cl);

  RegisterNewStaticText_C(Cl);
  RegisterNewCheckListBox_C(Cl);
  RegisterNewProgressBar_C(Cl);
  RegisterRichEditViewer_C(Cl);
  RegisterPasswordEdit_C(Cl);
  RegisterCustomFolderTreeView_C(Cl);
  RegisterFolderTreeView_C(Cl);
  RegisterStartMenuFolderTreeView_C(Cl);
  RegisterBitmapImage_C(Cl);
  RegisterBidiCtrls_C(Cl);

  RegisterNewNotebook_C(Cl);
  RegisterNewNotebookPage_C(Cl);

  RegisterUIStateForm_C(Cl);
  RegisterSetupForm_C(Cl);
  RegisterWizardForm_C(Cl);
  RegisterUninstallProgressForm_C(Cl);

  RegisterWizardPage_C(Cl);
  RegisterInputQueryWizardPage_C(Cl);
  RegisterInputOptionWizardPage_C(Cl);
  RegisterInputDirWizardPage_C(Cl);
  RegisterInputFileWizardPage_C(Cl);
  RegisterOutputMsgWizardPage_C(Cl);
  RegisterOutputMsgMemoWizardPage_C(Cl);
  RegisterOutputProgressWizardPage_C(Cl);
  RegisterOutputMarqueeProgressWizardPage_C(Cl);
  RegisterDownloadWizardPage_C(Cl);
  RegisterExtractionWizardPage_C(Cl);

  RegisterHandCursor_C(Cl);
  
  AddImportedClassVariable(Cl, 'WizardForm', 'TWizardForm');
  AddImportedClassVariable(Cl, 'UninstallProgressForm', 'TUninstallProgressForm');
end;

end.
