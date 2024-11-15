unit Setup.ScriptDlg;

{
  Inno Setup
  Copyright (C) 1997-2012 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Custom wizard pages
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Contnrs, Generics.Collections,
  Setup.WizardForm, Setup.Install, Compression.SevenZipDecoder,
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
      property Values[Index: Integer]: String read GetValue write SetValue;
    published
      property SubCaptionLabel: TNewStaticText read FSubCaptionLabel;
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
      procedure Initialize(const SubCaption: String; const Exclusive, ListBox: Boolean);
      property SelectedValueIndex: Integer read GetSelectedValueIndex write SetSelectedValueIndex;
      property Values[Index: Integer]: Boolean read GetValue write SetValue;
    published
      property CheckListBox: TNewCheckListBox read FCheckListBox;
      property SubCaptionLabel: TNewStaticText read FSubCaptionLabel;
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
      property Values[Index: Integer]: String read GetValue write SetValue;
    published
      property NewFolderName: String read FNewFolderName write FNewFolderName;
      property SubCaptionLabel: TNewStaticText read FSubCaptionLabel;
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
      property Values[Index: Integer]: String read GetValue write SetValue;
      property IsSaveButton[Index: Integer]: Boolean read GetIsSaveButton write SetIsSaveButton;
    published
      property SubCaptionLabel: TNewStaticText read FSubCaptionLabel;
  end;

  TOutputMsgWizardPage = class(TWizardPage)
    private
      FMsgLabel: TNewStaticText;
    public
      procedure Initialize(const Msg: String);
    published
      property MsgLabel: TNewStaticText read FMsgLabel;
  end;

  TOutputMsgMemoWizardPage = class(TWizardPage)
    private
      FRichEditViewer: TRichEditViewer;
      FSubCaptionLabel: TNewStaticText;
    public
      procedure Initialize(const SubCaption: String; const Msg: AnsiString);
    published
      property RichEditViewer: TRichEditViewer read FRichEditViewer;
      property SubCaptionLabel: TNewStaticText read FSubCaptionLabel;
  end;

  TOutputProgressWizardPage = class(TWizardPage)
    private
      FMsg1Label: TNewStaticText;
      FMsg2Label: TNewStaticText;
      FProgressBar: TNewProgressBar;
      FUseMarqueeStyle: Boolean;
      FSavePageID: Integer;
      procedure ProcessMsgs;
    public
      constructor Create(AOwner: TComponent); override;
      procedure Hide;
      procedure Initialize; virtual;
      procedure SetProgress(const Position, Max: Longint);
      procedure SetText(const Msg1, Msg2: String);
      procedure Show; virtual;
    published
      property Msg1Label: TNewStaticText read FMsg1Label;
      property Msg2Label: TNewStaticText read FMsg2Label;
      property ProgressBar: TNewProgressBar read FProgressBar;
  end;

  TOutputMarqueeProgressWizardPage = class(TOutputProgressWizardPage)
    public
      constructor Create(AOwner: TComponent); override;
      procedure Animate;
      procedure Initialize; override;
      procedure SetProgress(const Position, Max: Longint);
  end;

  TDownloadFile = class
    Url, BaseName, RequiredSHA256OfFile, UserName, Password: String;
  end;
  TDownloadFiles = TObjectList<TDownloadFile>;

  TDownloadWizardPage = class(TOutputProgressWizardPage)
    private
      FFiles: TDownloadFiles;
      FOnDownloadProgress: TOnDownloadProgress;
      FShowBaseNameInsteadOfUrl: Boolean;
      FAbortButton: TNewButton;
      FShowProgressControlsOnNextProgress, FAbortedByUser: Boolean;
      procedure AbortButtonClick(Sender: TObject);
      function InternalOnDownloadProgress(const Url, BaseName: string; const Progress, ProgressMax: Int64): Boolean;
      procedure ShowProgressControls(const AVisible: Boolean);
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Initialize; override;
      procedure Add(const Url, BaseName, RequiredSHA256OfFile: String);
      procedure AddEx(const Url, BaseName, RequiredSHA256OfFile, UserName, Password: String);
      procedure Clear;
      function Download: Int64;
      property OnDownloadProgress: TOnDownloadProgress write FOnDownloadProgress;
      procedure Show; override;
    published
      property AbortButton: TNewButton read FAbortButton;
      property AbortedByUser: Boolean read FAbortedByUser;
      property ShowBaseNameInsteadOfUrl: Boolean read FShowBaseNameInsteadOfUrl write FShowBaseNameInsteadOfUrl;
  end;
  
  TArchive = class
    FileName, DestDir: String;
    FullPaths: Boolean;
  end;
  TArchives = TObjectList<TArchive>;

  TExtractionWizardPage = class(TOutputProgressWizardPage)
    private
      FArchives: TArchives;
      FOnExtractionProgress: TOnExtractionProgress;
      FShowArchiveInsteadOfFile: Boolean;
      FAbortButton: TNewButton;
      FShowProgressControlsOnNextProgress, FAbortedByUser: Boolean;
      procedure AbortButtonClick(Sender: TObject);
      function InternalOnExtractionProgress(const ArchiveName, FileName: string; const Progress, ProgressMax: Int64): Boolean;
      procedure ShowProgressControls(const AVisible: Boolean);
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Initialize; override;
      procedure Add(const ArchiveFileName, DestDir: String; const FullPaths: Boolean);
      procedure Clear;
      procedure Extract;
      property OnExtractionProgress: TOnExtractionProgress write FOnExtractionProgress;
      procedure Show; override;
    published
      property AbortButton: TNewButton read FAbortButton;
      property AbortedByUser: Boolean read FAbortedByUser;
      property ShowArchiveInsteadOfFile: Boolean read FShowArchiveInsteadOfFile write FShowArchiveInsteadOfFile;
  end;

implementation

uses
  StrUtils,
  Shared.Struct, Setup.MainFunc, Setup.SelectFolderForm, SetupLdrAndSetup.Messages,
  Shared.SetupMessageIDs, PathFunc, Shared.CommonFunc.Vcl, Shared.CommonFunc,
  BrowseFunc, Setup.LoggingFunc, Setup.InstFunc;

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
      Anchors := [akLeft, akTop, akRight];
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
    Anchors := [akLeft, akTop, akRight];
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
    Anchors := [akLeft, akTop, akRight];
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
    Anchors := [akLeft, akTop, akRight, akBottom];
    Flat := ListBox and (shFlatComponentsList in SetupHeader.Options);
  end;
  SetCtlParent(FCheckListBox, Surface);

  FExclusive := Exclusive;
  if not ListBox then begin
    FCheckListBox.BorderStyle := bsNone;
    FCheckListBox.Color := SurfaceColor;
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
    Anchors := [akLeft, akTop, akRight];
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
  ButtonWidth := WizardForm.CalculateButtonWidth([SetupMessages[msgButtonWizardBrowse]]);

  if APrompt <> '' then begin
    PromptLabel := TNewStaticText.Create(Self);
    with PromptLabel do begin
      AutoSize := False;
      Top := FY;
      Width := SurfaceWidth;
      Height := WizardForm.ScalePixelsY(DefaultLabelHeight);
      Anchors := [akLeft, akTop, akRight];
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
    Anchors := [akLeft, akTop, akRight];
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
    Anchors := [akTop, akRight];
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
    Anchors := [akLeft, akTop, akRight];
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
  ButtonWidth := WizardForm.CalculateButtonWidth([SetupMessages[msgButtonWizardBrowse]]);

  if APrompt <> '' then begin
    PromptLabel := TNewStaticText.Create(Self);
    with PromptLabel do begin
      AutoSize := False;
      Top := FY;
      Width := SurfaceWidth;
      Height := WizardForm.ScalePixelsY(DefaultLabelHeight);
      Anchors := [akLeft, akTop, akRight];
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
    Anchors := [akLeft, akTop, akRight];
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
    Anchors := [akTop, akRight];
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
      Anchors := [akLeft, akTop, akRight];
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
    Anchors := [akLeft, akTop, akRight, akBottom];
    BevelKind := bkFlat;
    BorderStyle := bsNone;
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
    Anchors := [akLeft, akTop, akRight];
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
    Anchors := [akLeft, akTop, akRight];
  end;
  SetCtlParent(FMsg2Label, Surface);

  FProgressBar := TNewProgressBar.Create(Self);
  with FProgressBar do begin
    Top := WizardForm.ScalePixelsY(42);
    Width := SurfaceWidth;
    Height := WizardForm.ScalePixelsY(21);
    Anchors := [akLeft, akTop, akRight];
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
    FProgressBar.Style := npbstNormal;
    FProgressBar.Max := Max;
    FProgressBar.Position := Position;
    FProgressBar.Visible := True;
    SetAppTaskbarProgressState(tpsNormal);
    SetAppTaskbarProgressValue(Position, Max);
  end else begin
    if FUseMarqueeStyle then
      FProgressBar.Style := npbstMarquee
    else
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

constructor TOutputMarqueeProgressWizardPage.Create(AOwner: TComponent);
begin
  inherited;
  FUseMarqueeStyle := True;
end;

procedure TOutputMarqueeProgressWizardPage.Animate;
begin
  ProcessMsgs;
end;

procedure TOutputMarqueeProgressWizardPage.Initialize;
begin
  inherited;
  FProgressBar.Visible := True;
  inherited SetProgress(0, 0);
end;

procedure TOutputMarqueeProgressWizardPage.SetProgress(const Position, Max: Longint);
begin
  InternalError('Cannot call TOutputMarqueeProgressWizardPage.SetProgress');
end;

{--- Download ---}

procedure TDownloadWizardPage.AbortButtonClick(Sender: TObject);
begin
  FAbortedByUser := LoggedMsgBox(SetupMessages[msgStopDownload], '', mbConfirmation, MB_YESNO, True, ID_YES) = IDYES;
end;

function TDownloadWizardPage.InternalOnDownloadProgress(const Url, BaseName: string; const Progress, ProgressMax: Int64): Boolean;
var
  Progress32, ProgressMax32: LongInt;
begin
  if FAbortedByUser then begin
    Log('Need to abort download.');
    Result := False;
  end else begin
    if ProgressMax > 0 then
      Log(Format('  %d of %d bytes done.', [Progress, ProgressMax]))
    else
      Log(Format('  %d bytes done.', [Progress]));

    FMsg2Label.Caption := IfThen(FShowBaseNameInsteadOfUrl, BaseName, Url);
    if ProgressMax > MaxLongInt then begin
      Progress32 := Round((Progress / ProgressMax) * MaxLongInt);
      ProgressMax32 := MaxLongInt;
    end else begin
      Progress32 := Progress;
      ProgressMax32 := ProgressMax;
    end;
    SetProgress(Progress32, ProgressMax32); { This will process messages which we need for the abort button to work }

    if FShowProgressControlsOnNextProgress then begin
      ShowProgressControls(True);
      FShowProgressControlsOnNextProgress := False;
      ProcessMsgs;
    end;

    if Assigned(FOnDownloadProgress) then
      Result := FOnDownloadProgress(Url, BaseName, Progress, ProgressMax)
    else
      Result := True;
  end;
end;

constructor TDownloadWizardPage.Create(AOwner: TComponent);
begin
  inherited;
  FUseMarqueeStyle := True;
  FFiles := TDownloadFiles.Create;
end;

destructor TDownloadWizardPage.Destroy;
begin
  FFiles.Free;
  inherited;
end;

procedure TDownloadWizardPage.Initialize;
begin
  inherited;

  FMsg1Label.Caption := SetupMessages[msgDownloadingLabel];

  FAbortButton := TNewButton.Create(Self);
  with FAbortButton do begin
    Caption := SetupMessages[msgButtonStopDownload];
    Top := FProgressBar.Top + FProgressBar.Height + WizardForm.ScalePixelsY(8);
    Width := WizardForm.CalculateButtonWidth([Caption]);
    Anchors := [akLeft, akTop];
    Height := WizardForm.CancelButton.Height;
    OnClick := AbortButtonClick;
  end;
  SetCtlParent(FAbortButton, Surface);
end;

procedure TDownloadWizardPage.Show;
begin
  if WizardForm.CurPageID <> ID then begin
    ShowProgressControls(False);
    FShowProgressControlsOnNextProgress := True;
  end;
  inherited;
end;

procedure TDownloadWizardPage.ShowProgressControls(const AVisible: Boolean);
begin
  FMsg2Label.Visible := AVisible;
  FProgressBar.Visible := AVisible;
  FAbortButton.Visible := AVisible;
end;

procedure TDownloadWizardPage.Add(const Url, BaseName, RequiredSHA256OfFile: String);
begin
  AddEx(Url, BaseName, RequiredSHA256OfFile, '', '');
end;

procedure TDownloadWizardPage.AddEx(const Url, BaseName, RequiredSHA256OfFile, UserName, Password: String);
begin
  var F := TDownloadFile.Create;
  F.Url := Url;
  F.BaseName := BaseName;
  F.RequiredSHA256OfFile := RequiredSHA256OfFile;
  F.UserName := UserName;
  F.Password := Password;
  FFiles.Add(F);
end;

procedure TDownloadWizardPage.Clear;
begin
  FFiles.Clear;
end;

function TDownloadWizardPage.Download: Int64;
begin
  FAbortedByUser := False;

  Result := 0;
  for var F in FFiles do begin
    { Don't need to set DownloadTemporaryFileOrExtract7ZipArchiveProcessMessages before downloading since we already process messages ourselves. }
    SetDownloadCredentials(F.UserName, F.Password);
    Result := Result + DownloadTemporaryFile(F.Url, F.BaseName, F.RequiredSHA256OfFile, InternalOnDownloadProgress);
  end;
  SetDownloadCredentials('', '');
end;

{--- Extraction ---}

procedure TExtractionWizardPage.AbortButtonClick(Sender: TObject);
begin
  FAbortedByUser := LoggedMsgBox(SetupMessages[msgStopExtraction], '', mbConfirmation, MB_YESNO, True, ID_YES) = IDYES;
end;

function TExtractionWizardPage.InternalOnExtractionProgress(const ArchiveName, FileName: string; const Progress, ProgressMax: Int64): Boolean;
var
  Progress32, ProgressMax32: LongInt;
begin
  if FAbortedByUser then begin
    Log('Need to abort extraction.');
    Result := False;
  end else begin
    { Unlike TDownloadWizardPage we don't log progress here. This is because 7zMain.c already logs output dirs and names. }

    FMsg2Label.Caption := IfThen(FShowArchiveInsteadOfFile, ArchiveName, FileName);
    if ProgressMax > MaxLongInt then begin
      Progress32 := Round((Progress / ProgressMax) * MaxLongInt);
      ProgressMax32 := MaxLongInt;
    end else begin
      Progress32 := Progress;
      ProgressMax32 := ProgressMax;
    end;
    SetProgress(Progress32, ProgressMax32); { This will process messages which we need for the abort button to work }

    if FShowProgressControlsOnNextProgress then begin
      ShowProgressControls(True);
      FShowProgressControlsOnNextProgress := False;
      ProcessMsgs;
    end;

    if Assigned(FOnExtractionProgress) then
      Result := FOnExtractionProgress(ArchiveName, FileName, Progress, ProgressMax)
    else
      Result := True;
  end;
end;

constructor TExtractionWizardPage.Create(AOwner: TComponent);
begin
  inherited;
  FUseMarqueeStyle := True;
  FArchives := TArchives.Create;
end;

destructor TExtractionWizardPage.Destroy;
begin
  FArchives.Free;
  inherited;
end;

procedure TExtractionWizardPage.Initialize;
begin
  inherited;

  FMsg1Label.Caption := SetupMessages[msgExtractionLabel];

  FAbortButton := TNewButton.Create(Self);
  with FAbortButton do begin
    Caption := SetupMessages[msgButtonStopExtraction];
    Top := FProgressBar.Top + FProgressBar.Height + WizardForm.ScalePixelsY(8);
    Width := WizardForm.CalculateButtonWidth([Caption]);
    Anchors := [akLeft, akTop];
    Height := WizardForm.CancelButton.Height;
    OnClick := AbortButtonClick;
  end;
  SetCtlParent(FAbortButton, Surface);
end;

procedure TExtractionWizardPage.Show;
begin
  if WizardForm.CurPageID <> ID then begin
    ShowProgressControls(False);
    FShowProgressControlsOnNextProgress := True;
  end;
  inherited;
end;

procedure TExtractionWizardPage.ShowProgressControls(const AVisible: Boolean);
begin
  FMsg2Label.Visible := AVisible;
  FProgressBar.Visible := AVisible;
  FAbortButton.Visible := AVisible;
end;

procedure TExtractionWizardPage.Add(const ArchiveFileName, DestDir: String; const FullPaths: Boolean);
begin
  var A := TArchive.Create;
  A.FileName := ArchiveFileName;
  A.DestDir := DestDir;
  A.FullPaths := FullPaths;
  FArchives.Add(A);
end;

procedure TExtractionWizardPage.Clear;
begin
  FArchives.Clear;
end;

procedure TExtractionWizardPage.Extract;
begin
  FAbortedByUser := False;

  for var A in FArchives do begin
    { Don't need to set DownloadTemporaryFileOrExtract7ZipArchiveProcessMessages before extraction since we already process messages ourselves. }
    Extract7ZipArchive(A.FileName, A.DestDir, A.FullPaths, InternalOnExtractionProgress);
  end;
end;

end.
