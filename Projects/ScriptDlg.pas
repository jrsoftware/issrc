unit ScriptDlg;

{
  Inno Setup
  Copyright (C) 1997-2012 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Custom wizard pages
}

interface

{$I VERSION.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  Wizard,
  NewCheckListBox, NewStaticText, NewProgressBar, PasswordEdit, RichEditViewer,
  BidiCtrls, TaskbarProgressFunc;

type
  TInputQueryWizardPage = class(TWizardPage)
    private
      FEdits: TList;
      FPromptLabels: TList;
      FSubCaptionLabel: TNewStaticText;
      FY: Integer;
      function GetEdit(Index: Integer): TPasswordEdit;
      function GetPromptLabel(Index: Integer): TNewStaticText;
      function GetValue(Index: Integer): String;
      procedure SetValue(Index: Integer; const Value: String);
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function Add(const APrompt: String; const APassword: Boolean): Integer;
      property Edits[Index: Integer]: TPasswordEdit read GetEdit;
      procedure Initialize(const SubCaption: String);
      property PromptLabels[Index: Integer]: TNewStaticText read GetPromptLabel;
      property SubCaptionLabel: TNewStaticText read FSubCaptionLabel;
      property Values[Index: Integer]: String read GetValue write SetValue;
  end;

  TInputOptionWizardPage = class(TWizardPage)
    private
      FCheckListBox: TNewCheckListBox;
      FExclusive: Boolean;
      FSubCaptionLabel: TNewStaticText;
      function GetSelectedValueIndex: Integer;
      function GetValue(Index: Integer): Boolean;
      procedure SetSelectedValueIndex(Value: Integer);
      procedure SetValue(Index: Integer; Value: Boolean);
    public
      function Add(const ACaption: String): Integer;
      function AddEx(const ACaption: String; const ALevel: Byte; const AExclusive: Boolean): Integer;
      property CheckListBox: TNewCheckListBox read FCheckListBox;
      procedure Initialize(const SubCaption: String; const Exclusive, ListBox: Boolean);
      property SelectedValueIndex: Integer read GetSelectedValueIndex write SetSelectedValueIndex;
      property SubCaptionLabel: TNewStaticText read FSubCaptionLabel;
      property Values[Index: Integer]: Boolean read GetValue write SetValue;
  end;

  TInputDirWizardPage = class(TWizardPage)
    private
      FAppendDir: Boolean;
      FButtons: TList;
      FEdits: TList;
      FNewFolderName: String;
      FPromptLabels: TList;
      FSubCaptionLabel: TNewStaticText;
      FY: Integer;
      procedure ButtonClick(Sender: TObject);
      function GetButton(Index: Integer): TNewButton;
      function GetEdit(Index: Integer): TEdit;
      function GetPromptLabel(Index: Integer): TNewStaticText;
      function GetValue(Index: Integer): String;
      procedure SetValue(Index: Integer; const Value: String);
    protected
      procedure NextButtonClick(var Continue: Boolean); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function Add(const APrompt: String): Integer;
      property Buttons[Index: Integer]: TNewButton read GetButton;
      property Edits[Index: Integer]: TEdit read GetEdit;
      procedure Initialize(const SubCaption: String; const AppendDir: Boolean;
        const NewFolderName: String);
      property PromptLabels[Index: Integer]: TNewStaticText read GetPromptLabel;
      property SubCaptionLabel: TNewStaticText read FSubCaptionLabel;
      property Values[Index: Integer]: String read GetValue write SetValue;
  end;

  TInputFileWizardPage = class(TWizardPage)
    private
      FButtons: TList;
      FEdits: TList;
      FInputFileDefaultExtensions: TStringList;
      FInputFileFilters: TStringList;
      FPromptLabels: TList;
      FSubCaptionLabel: TNewStaticText;
      FY: Integer;
      procedure ButtonClick(Sender: TObject);
      function GetButton(Index: Integer): TNewButton;
      function GetEdit(Index: Integer): TEdit;
      function GetPromptLabel(Index: Integer): TNewStaticText;
      function GetValue(Index: Integer): String;
      procedure SetValue(Index: Integer; const Value: String);
      function GetIsSaveButton(Index: Integer): Boolean;
      procedure SetIsSaveButton(Index: Integer; const IsSaveButton: Boolean);
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function Add(const APrompt, AFilter, ADefaultExtension: String): Integer;
      property Buttons[Index: Integer]: TNewButton read GetButton;
      property Edits[Index: Integer]: TEdit read GetEdit;
      procedure Initialize(const SubCaption: String);
      property PromptLabels[Index: Integer]: TNewStaticText read GetPromptLabel;
      property SubCaptionLabel: TNewStaticText read FSubCaptionLabel;
      property Values[Index: Integer]: String read GetValue write SetValue;
      property IsSaveButton[Index: Integer]: Boolean read GetIsSaveButton write SetIsSaveButton;
  end;

  TOutputMsgWizardPage = class(TWizardPage)
    private
      FMsgLabel: TNewStaticText;
    public
      procedure Initialize(const Msg: String);
      property MsgLabel: TNewStaticText read FMsgLabel;
  end;

  TOutputMsgMemoWizardPage = class(TWizardPage)
    private
      FRichEditViewer: TRichEditViewer;
      FSubCaptionLabel: TNewStaticText;
    public
      procedure Initialize(const SubCaption: String; const Msg: AnsiString);
      property RichEditViewer: TRichEditViewer read FRichEditViewer;
      property SubCaptionLabel: TNewStaticText read FSubCaptionLabel;
  end;

  TOutputProgressWizardPage = class(TWizardPage)
    private
      FMsg1Label: TNewStaticText;
      FMsg2Label: TNewStaticText;
      FProgressBar: TNewProgressBar;
      FSavePageID: Integer;
      procedure ProcessMsgs;
    public
      constructor Create(AOwner: TComponent); override;
      procedure Hide;
      procedure Initialize;
      property Msg1Label: TNewStaticText read FMsg1Label;
      property Msg2Label: TNewStaticText read FMsg2Label;
      property ProgressBar: TNewProgressBar read FProgressBar;
      procedure SetProgress(const Position, Max: Longint);
      procedure SetText(const Msg1, Msg2: String);
      procedure Show;
  end;

implementation

uses
  Struct, Main, SelFolderForm, Msgs, MsgIDs, PathFunc, CmnFunc, CmnFunc2,
  BrowseFunc;

const
  DefaultLabelHeight = 14;
  DefaultBoxTop = 24;       { relative to top of InnerNotebook }
  DefaultBoxBottom = DefaultBoxTop + 205;

{------}

procedure SetCtlParent(const AControl, AParent: TWinControl);
{ Like assigning to AControl.Parent, but puts the control at the *bottom* of
  the z-order instead of the top, for MSAA compatibility. }
var
  OldVisible: Boolean;
begin
  { Hide the control so the handle won't be created yet, so that unnecessary
    "OBJ_REORDER" MSAA events don't get sent }
  OldVisible := AControl.Visible;
  AControl.Visible := False;
  AControl.Parent := AParent;
  AControl.SendToBack;
  AControl.Visible := OldVisible;
end;

{--- InputQuery ---}

constructor TInputQueryWizardPage.Create(AOwner: TComponent);
begin
  inherited;
  FEdits := TList.Create;
  FPromptLabels := TList.Create;
end;

destructor TInputQueryWizardPage.Destroy;
begin
  FPromptLabels.Free;
  FEdits.Free;
  inherited;
end;

procedure TInputQueryWizardPage.Initialize(const SubCaption: String);
begin
  FSubCaptionLabel := TNewStaticText.Create(Self);
  with FSubCaptionLabel do begin
    AutoSize := False;
    Width := SurfaceWidth;
    Height := WizardForm.ScalePixelsY(DefaultLabelHeight);
    WordWrap := True;
    Caption := SubCaption;
    Parent := Surface;
  end;
  FY := WizardForm.AdjustLabelHeight(FSubCaptionLabel) + WizardForm.ScalePixelsY(DefaultBoxTop);
end;

function TInputQueryWizardPage.Add(const APrompt: String;
  const APassword: Boolean): Integer;
var
  PromptLabel: TNewStaticText;
  Edit: TPasswordEdit;
begin
  if APrompt <> '' then begin
    PromptLabel := TNewStaticText.Create(Self);
    with PromptLabel do begin
      AutoSize := False;
      Top := FY;
      Width := SurfaceWidth;
      Height := WizardForm.ScalePixelsY(DefaultLabelHeight);
      WordWrap := True;
      Caption := APrompt;
    end;
    SetCtlParent(PromptLabel, Surface);
    Inc(FY, WizardForm.AdjustLabelHeight(PromptLabel) + WizardForm.ScalePixelsY(16));
  end else
    PromptLabel := nil;

  Edit := TPasswordEdit.Create(Self);
  with Edit do begin
    Password := APassword;
    Top := FY;
    Width := SurfaceWidth;
  end;
  SetCtlParent(Edit, Surface);
  Inc(FY, WizardForm.ScalePixelsY(36));

  if PromptLabel <> nil then
    PromptLabel.FocusControl := Edit;

  FPromptLabels.Add(PromptLabel);
  Result := FEdits.Add(Edit);
end;

function TInputQueryWizardPage.GetEdit(Index: Integer): TPasswordEdit;
begin
  Result := TPasswordEdit(FEdits[Index]);
end;

function TInputQueryWizardPage.GetPromptLabel(Index: Integer): TNewStaticText;
begin
  Result := TNewStaticText(FPromptLabels[Index]);
end;

function TInputQueryWizardPage.GetValue(Index: Integer): String;
begin
  Result := GetEdit(Index).Text;
end;

procedure TInputQueryWizardPage.SetValue(Index: Integer; const Value: String);
begin
  GetEdit(Index).Text := Value;
end;

{--- InputOption ---}

procedure TInputOptionWizardPage.Initialize(const SubCaption: String;
  const Exclusive, ListBox: Boolean);
var
  CaptionYDiff: Integer;
begin
  FSubCaptionLabel := TNewStaticText.Create(Self);
  with SubCaptionLabel do begin
    AutoSize := False;
    Width := SurfaceWidth;
    Height := WizardForm.ScalePixelsY(DefaultLabelHeight);
    WordWrap := True;
    Caption := SubCaption;
    Parent := Surface;
  end;
  CaptionYDiff := WizardForm.AdjustLabelHeight(SubCaptionLabel);

  FCheckListBox := TNewCheckListBox.Create(Self);
  with FCheckListBox do begin
    Top := CaptionYDiff + WizardForm.ScalePixelsY(DefaultBoxTop);
    Width := SurfaceWidth;
    Height := WizardForm.ScalePixelsY(DefaultBoxBottom) - Top;
    Flat := ListBox and (shFlatComponentsList in SetupHeader.Options);
  end;
  SetCtlParent(FCheckListBox, Surface);

  FExclusive := Exclusive;
  if not ListBox then begin
    FCheckListBox.BorderStyle := bsNone;
    FCheckListBox.Color := clBtnFace;
    FCheckListBox.MinItemHeight := WizardForm.ScalePixelsY(22);
    FCheckListBox.WantTabs := True;
  end;
end;

function TInputOptionWizardPage.Add(const ACaption: String): Integer;
begin
  Result := AddEx(ACaption, 0, FExclusive);
end;

function TInputOptionWizardPage.AddEx(const ACaption: String;
  const ALevel: Byte; const AExclusive: Boolean): Integer;
begin
  if AExclusive then
    Result := FCheckListBox.AddRadioButton(ACaption, '', ALevel, False, True, nil)
  else
    Result := FCheckListBox.AddCheckBox(ACaption, '', ALevel, False, True, True,
      True, nil);
end;

function TInputOptionWizardPage.GetSelectedValueIndex: Integer;
var
  I: Integer;
begin
  for I := 0 to FCheckListBox.Items.Count-1 do
    if (FCheckListBox.ItemLevel[I] = 0) and FCheckListBox.Checked[I] then begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TInputOptionWizardPage.GetValue(Index: Integer): Boolean;
begin
  Result := FCheckListBox.Checked[Index];
end;

procedure TInputOptionWizardPage.SetSelectedValueIndex(Value: Integer);
var
  I: Integer;
begin
  for I := 0 to FCheckListBox.Items.Count-1 do
    if FCheckListBox.ItemLevel[I] = 0 then
      FCheckListBox.Checked[I] := (I = Value);
end;

procedure TInputOptionWizardPage.SetValue(Index: Integer; Value: Boolean);
begin
  FCheckListBox.Checked[Index] := Value;
end;

{--- InputDir ---}

constructor TInputDirWizardPage.Create(AOwner: TComponent);
begin
  inherited;
  FButtons := TList.Create;
  FEdits := TList.Create;
  FPromptLabels := TList.Create;
end;

destructor TInputDirWizardPage.Destroy;
begin
  FPromptLabels.Free;
  FEdits.Free;
  FButtons.Free;
  inherited;
end;

procedure TInputDirWizardPage.ButtonClick(Sender: TObject);
var
  I: Integer;
  Edit: TEdit;
  S: String;
begin
  I := FButtons.IndexOf(Sender);
  if I <> -1 then begin
    Edit := TEdit(FEdits[I]);
    S := Edit.Text;
    if ShowSelectFolderDialog(False, FAppendDir, S, FNewFolderName) then
      Edit.Text := S;
  end;
end;

procedure TInputDirWizardPage.NextButtonClick(var Continue: Boolean);
var
  I: Integer;
  Edit: TEdit;
begin
  for I := 0 to FEdits.Count-1 do begin
    Edit := FEdits[I];
    if not ValidateCustomDirEdit(Edit, True, True, True) then begin
      if WizardForm.Visible then
        Edit.SetFocus;
      Continue := False;
      Exit;
    end;
  end;
  inherited;
end;

procedure TInputDirWizardPage.Initialize(const SubCaption: String;
  const AppendDir: Boolean; const NewFolderName: String);
begin
  FSubCaptionLabel := TNewStaticText.Create(Self);
  with FSubCaptionLabel do begin
    AutoSize := False;
    Width := SurfaceWidth;
    Height := WizardForm.ScalePixelsY(DefaultLabelHeight);
    WordWrap := True;
    Caption := SubCaption;
    Parent := Surface;
  end;
  FY := WizardForm.AdjustLabelHeight(FSubCaptionLabel) + WizardForm.ScalePixelsY(DefaultBoxTop);

  FAppendDir := AppendDir;
  FNewFolderName := NewFolderName;
end;

function TInputDirWizardPage.Add(const APrompt: String): Integer;
var
  ButtonWidth: Integer;
  PromptLabel: TNewStaticText;
  Edit: TEdit;
  Button: TNewButton;
begin
  ButtonWidth := WizardForm.CalculateButtonWidth([msgButtonWizardBrowse]);

  if APrompt <> '' then begin
    PromptLabel := TNewStaticText.Create(Self);
    with PromptLabel do begin
      AutoSize := False;
      Top := FY;
      Width := SurfaceWidth;
      Height := WizardForm.ScalePixelsY(DefaultLabelHeight);
      WordWrap := True;
      Caption := APrompt;
    end;
    SetCtlParent(PromptLabel, Surface);
    Inc(FY, WizardForm.AdjustLabelHeight(PromptLabel) + WizardForm.ScalePixelsY(16));
  end else
    PromptLabel := nil;

  Edit := TEdit.Create(Self);
  with Edit do begin
    Top := FY;
    Width := SurfaceWidth-ButtonWidth-WizardForm.ScalePixelsX(10);
  end;
  SetCtlParent(Edit, Surface);
  TryEnableAutoCompleteFileSystem(Edit.Handle);

  if PromptLabel <> nil then
    PromptLabel.FocusControl := Edit;

  Button := TNewButton.Create(Self);
  with Button do begin
    Left := SurfaceWidth-ButtonWidth;
    Top := Edit.Top-1;
    Width := ButtonWidth;
    Height := WizardForm.NextButton.Height;
    if FEdits.Count = 0 then
      Caption := SetupMessages[msgButtonWizardBrowse]
    else
      { Can't use the same accel key for secondary buttons... }
      Caption := RemoveAccelChar(SetupMessages[msgButtonWizardBrowse]);
    OnClick := ButtonClick;
  end;
  SetCtlParent(Button, Surface);
  Inc(FY, WizardForm.ScalePixelsY(36));

  FButtons.Add(Button);
  FPromptLabels.Add(PromptLabel);
  Result := FEdits.Add(Edit);
end;

function TInputDirWizardPage.GetButton(Index: Integer): TNewButton;
begin
  Result := TNewButton(FButtons[Index]);
end;

function TInputDirWizardPage.GetEdit(Index: Integer): TEdit;
begin
  Result := TEdit(FEdits[Index]);
end;

function TInputDirWizardPage.GetPromptLabel(Index: Integer): TNewStaticText;
begin
  Result := TNewStaticText(FPromptLabels[Index]);
end;

function TInputDirWizardPage.GetValue(Index: Integer): String;
begin
  Result := GetEdit(Index).Text;
end;

procedure TInputDirWizardPage.SetValue(Index: Integer; const Value: String);
begin
  GetEdit(Index).Text := RemoveBackslashUnlessRoot(PathExpand(Value));
end;

{--- InputFile ---}

constructor TInputFileWizardPage.Create(AOwner: TComponent);
begin
  inherited;
  FButtons := TList.Create;
  FEdits := TList.Create;
  FInputFileDefaultExtensions := TStringList.Create;
  FInputFileFilters := TStringList.Create;
  FPromptLabels := TList.Create;
end;

destructor TInputFileWizardPage.Destroy;
begin
  FPromptLabels.Free;
  FInputFileFilters.Free;
  FInputFileDefaultExtensions.Free;
  FEdits.Free;
  FButtons.Free;
  inherited;
end;

procedure TInputFileWizardPage.ButtonClick(Sender: TObject);
var
  I: Integer;
  Edit: TEdit;
  FileName: String;
begin
  I := FButtons.IndexOf(Sender);
  if I <> -1 then begin
    Edit := TEdit(FEdits[I]);
    FileName := Edit.Text;
    if (not IsSaveButton[I] and NewGetOpenFileName(RemoveAccelChar(SetupMessages[msgButtonWizardBrowse]),
        FileName, PathExtractPath(FileName), FInputFileFilters[I],
        FInputFileDefaultExtensions[I], Surface.Handle)) or
       (IsSaveButton[I] and NewGetSaveFileName(RemoveAccelChar(SetupMessages[msgButtonWizardBrowse]),
        FileName, PathExtractPath(FileName), FInputFileFilters[I],
        FInputFileDefaultExtensions[I], Surface.Handle)) then
      Edit.Text := FileName;
  end;
end;

procedure TInputFileWizardPage.Initialize(const SubCaption: String);
begin
  FSubCaptionLabel := TNewStaticText.Create(Self);
  with FSubCaptionLabel do begin
    AutoSize := False;
    Width := SurfaceWidth;
    Height := WizardForm.ScalePixelsY(DefaultLabelHeight);
    WordWrap := True;
    Caption := SubCaption;
    Parent := Surface;
  end;
  FY := WizardForm.AdjustLabelHeight(FSubCaptionLabel) + WizardForm.ScalePixelsY(DefaultBoxTop);
end;

function TInputFileWizardPage.Add(const APrompt, AFilter,
  ADefaultExtension: String): Integer;
var
  ButtonWidth: Integer;
  PromptLabel: TNewStaticText;
  Edit: TEdit;
  Button: TNewButton;
begin
  ButtonWidth := WizardForm.CalculateButtonWidth([msgButtonWizardBrowse]);

  if APrompt <> '' then begin
    PromptLabel := TNewStaticText.Create(Self);
    with PromptLabel do begin
      AutoSize := False;
      Top := FY;
      Width := SurfaceWidth;
      Height := WizardForm.ScalePixelsY(DefaultLabelHeight);
      WordWrap := True;
      Caption := APrompt;
    end;
    SetCtlParent(PromptLabel, Surface);
    Inc(FY, WizardForm.AdjustLabelHeight(PromptLabel) + WizardForm.ScalePixelsY(16));
  end else
    PromptLabel := nil;

  Edit := TEdit.Create(Self);
  with Edit do begin
    Top := FY;
    Width := SurfaceWidth-ButtonWidth-WizardForm.ScalePixelsX(10);
  end;
  SetCtlParent(Edit, Surface);
  TryEnableAutoCompleteFileSystem(Edit.Handle);

  if PromptLabel <> nil then
    PromptLabel.FocusControl := Edit;

  Button := TNewButton.Create(Self);
  with Button do begin
    Left := SurfaceWidth-ButtonWidth;
    Top := Edit.Top-1;
    Width := ButtonWidth;
    Height := WizardForm.NextButton.Height;
    if FButtons.Count = 0 then
      Caption := SetupMessages[msgButtonWizardBrowse]
    else
      { Can't use the same accel key for secondary buttons... }
      Caption := RemoveAccelChar(SetupMessages[msgButtonWizardBrowse]);
    OnClick := ButtonClick;
  end;
  SetCtlParent(Button, Surface);
  Inc(FY, WizardForm.ScalePixelsY(36));

  FInputFileFilters.Add(AFilter);
  FInputFileDefaultExtensions.Add(ADefaultExtension);
  FButtons.Add(Button);
  FPromptLabels.Add(PromptLabel);
  Result := FEdits.Add(Edit);
end;

function TInputFileWizardPage.GetButton(Index: Integer): TNewButton;
begin
  Result := TNewButton(FButtons[Index]);
end;

function TInputFileWizardPage.GetEdit(Index: Integer): TEdit;
begin
  Result := TEdit(FEdits[Index]);
end;

function TInputFileWizardPage.GetPromptLabel(Index: Integer): TNewStaticText;
begin
  Result := TNewStaticText(FPromptLabels[Index]);
end;

function TInputFileWizardPage.GetValue(Index: Integer): String;
begin
  Result := GetEdit(Index).Text;
end;

procedure TInputFileWizardPage.SetValue(Index: Integer; const Value: String);
begin
  GetEdit(Index).Text := Value;
end;

function TInputFileWizardPage.GetIsSaveButton(Index: Integer): Boolean;
begin
  Result := GetButton(Index).Tag = 1;
end;

procedure TInputFileWizardPage.SetIsSaveButton(Index: Integer; const IsSaveButton: Boolean);
begin
  if IsSaveButton then
    GetButton(Index).Tag := 1
  else
    GetButton(Index).Tag := 0;
end;


{--- OutputMsg ---}

procedure TOutputMsgWizardPage.Initialize(const Msg: String);
begin
  FMsgLabel := TNewStaticText.Create(Self);
  with FMsgLabel do begin
    AutoSize := False;
    Width := SurfaceWidth;
    Height := WizardForm.ScalePixelsY(DefaultLabelHeight);
    WordWrap := True;
    Caption := Msg;
    Parent := Surface;
  end;
  WizardForm.AdjustLabelHeight(MsgLabel);
end;

{--- OutputMsgMemo ---}

procedure TOutputMsgMemoWizardPage.Initialize(const SubCaption: String; const Msg: AnsiString);
var
  Y: Integer;
begin
  Y := 0;
  if SubCaption <> '' then begin
    FSubCaptionLabel := TNewStaticText.Create(Self);
    with FSubCaptionLabel do begin
      AutoSize := False;
      Width := SurfaceWidth;
      Height := WizardForm.ScalePixelsY(DefaultLabelHeight);
      WordWrap := True;
      Caption := SubCaption;
      Parent := Surface;
    end;
    Inc(Y, WizardForm.ScalePixelsY(DefaultBoxTop) +
      WizardForm.AdjustLabelHeight(FSubCaptionLabel));
  end else
    FSubCaptionLabel := nil;

  FRichEditViewer := TRichEditViewer.Create(Self);
  with FRichEditViewer do begin
    Top := Y;
    Width := SurfaceWidth;
    Height := WizardForm.ScalePixelsY(DefaultBoxBottom) - Y;
    ReadOnly := True;
    ScrollBars := ssVertical;
    WantReturns := False;
  end;
  SetCtlParent(FRichEditViewer, Surface);
  with FRichEditViewer do begin
    UseRichEdit := True;
    RTFText := Msg;
  end;
end;

{--- OutputProgress ---}

constructor TOutputProgressWizardPage.Create(AOwner: TComponent);
begin
  inherited;
  Style := Style + [psAlwaysSkip, psNoButtons];
end;

procedure TOutputProgressWizardPage.Initialize;
begin
  FMsg1Label := TNewStaticText.Create(Self);
  with FMsg1Label do begin
    AutoSize := False;
    ShowAccelChar := False;
    Width := SurfaceWidth;
    Height := WizardForm.StatusLabel.Height;
    WordWrap := WizardForm.StatusLabel.WordWrap;
    Parent := Surface;
  end;

  FMsg2Label := TNewStaticText.Create(Self);
  with FMsg2Label do begin
    AutoSize := False;
    ForceLTRReading := True;
    ShowAccelChar := False;
    Top := WizardForm.ScalePixelsY(16);
    Width := SurfaceWidth;
    Height := WizardForm.FileNameLabel.Height;
  end;
  SetCtlParent(FMsg2Label, Surface);

  FProgressBar := TNewProgressBar.Create(Self);
  with FProgressBar do begin
    Top := WizardForm.ScalePixelsY(42);
    Width := SurfaceWidth;
    Height := WizardForm.ScalePixelsY(21);
    Visible := False;
  end;
  SetCtlParent(FProgressBar, Surface);
end;

procedure TOutputProgressWizardPage.Hide;
begin
  if (WizardForm.CurPageID = ID) and (FSavePageID <> 0) then begin
    SetMessageBoxCallbackFunc(nil, 0);
    SetAppTaskbarProgressState(tpsNoProgress);
    WizardForm.SetCurPage(FSavePageID);
    FSavePageID := 0;
  end;
end;

procedure TOutputProgressWizardPage.ProcessMsgs;
{ Process messages to repaint and keep Windows from thinking the process is
  hung. This is safe; due to the psNoButtons style the user shouldn't be able
  to cancel or do anything else during this time. }
begin
  if WizardForm.CurPageID = ID then
    Application.ProcessMessages;
end;

procedure TOutputProgressWizardPage.SetProgress(const Position, Max: Longint);
begin
  if Max > 0 then begin
    FProgressBar.Max := Max;
    FProgressBar.Position := Position;
    FProgressBar.Visible := True;
    SetAppTaskbarProgressState(tpsNormal);
    SetAppTaskbarProgressValue(Position, Max);
  end
  else begin
    FProgressBar.Visible := False;
    SetAppTaskbarProgressState(tpsNoProgress);
  end;
  ProcessMsgs;
end;

procedure TOutputProgressWizardPage.SetText(const Msg1, Msg2: String);
begin
  FMsg1Label.Caption := Msg1;
  FMsg2Label.Caption := MinimizePathName(Msg2, FMsg2Label.Font,
    FMsg2Label.Width);
  ProcessMsgs;
end;

procedure OutputProgressWizardPageMessageBoxCallback(const Flags: LongInt; const After: Boolean;
  const Param: LongInt);
const
  States: array [TNewProgressBarState] of TTaskbarProgressState =
    (tpsNormal, tpsError, tpsPaused);
var
  OutputProgressWizardPage: TOutputProgressWizardPage;
  NewState: TNewProgressBarState;
begin
  OutputProgressWizardPage := TOutputProgressWizardPage(Param);

  if After then
    NewState := npbsNormal
  else if (Flags and MB_ICONSTOP) <> 0 then
    NewState := npbsError
  else
    NewState := npbsPaused;

  with OutputProgressWizardPage.ProgressBar do begin
    State := NewState;
    Invalidate;
  end;
  SetAppTaskbarProgressState(States[NewState]);
end;

procedure TOutputProgressWizardPage.Show;
begin
  if WizardForm.CurPageID <> ID then begin
    FSavePageID := WizardForm.CurPageID;
    WizardForm.SetCurPage(ID);
    SetMessageBoxCallbackFunc(OutputProgressWizardPageMessageBoxCallback, LongInt(Self));
    ProcessMsgs;
  end;
end;

end.
