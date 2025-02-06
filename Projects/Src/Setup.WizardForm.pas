unit Setup.WizardForm;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Wizard form
}

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls,
  Setup.SetupForm, Shared.Struct, Shared.Int64Em, NewCheckListBox, RichEditViewer, NewStaticText,
  NewProgressBar, Shared.SetupMessageIDs, PasswordEdit, FolderTreeView, BitmapImage,
  NewNotebook, BidiCtrls;

type
  TWizardForm = class;

  TWizardPage = class;
  TWizardPageClass = class of TWizardPage;
  TWizardPageStyle = set of (psAlwaysSkip, psNoButtons);
  TWizardPageNotifyEvent = procedure(Sender: TWizardPage) of object;
  TWizardPageButtonEvent = function(Sender: TWizardPage): Boolean of object;
  TWizardPageCancelEvent = procedure(Sender: TWizardPage; var ACancel, AConfirm: Boolean) of object;
  TWizardPageShouldSkipEvent = function(Sender: TWizardPage): Boolean of object;
  TWizardPage = class(TComponent)
  private
    FID: Integer;
    FOuterNotebookPage: TNewNotebookPage;
    FInnerNotebookPage: TNewNotebookPage;
    FCaption: String;
    FDescription: String;
    FOnActivate: TWizardPageNotifyEvent;
    FOnBackButtonClick: TWizardPageButtonEvent;
    FOnCancelButtonClick: TWizardPageCancelEvent;
    FOnNextButtonClick: TWizardPageButtonEvent;
    FOnShouldSkipPage: TWizardPageShouldSkipEvent;
    FStyle: TWizardPageStyle;
    FWizardForm: TWizardForm;
    function GetSurface: TNewNotebookPage;
    function GetSurfaceColor: TColor;
    function GetSurfaceHeight: Integer;
    function GetSurfaceWidth: Integer;
    procedure SetCaption(const Value: String);
    procedure SetDescription(const Value: String);
    procedure SyncCaptionAndDescription;
  protected
    procedure Activate; virtual;
    procedure BackButtonClick(var AContinue: Boolean); virtual;
    procedure CancelButtonClick(var ACancel, AConfirm: Boolean); virtual;
    procedure NextButtonClick(var AContinue: Boolean); virtual;
    procedure ShouldSkipPage(var AShouldSkip: Boolean); virtual;
    property InnerNotebookPage: TNewNotebookPage read FInnerNotebookPage;
    property OuterNotebookPage: TNewNotebookPage read FOuterNotebookPage;
    property Style: TWizardPageStyle read FStyle write FStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Caption: String read FCaption write SetCaption;
    property Description: String read FDescription write SetDescription;
    property ID: Integer read FID;
    property Surface: TNewNotebookPage read GetSurface;
    property SurfaceColor: TColor read GetSurfaceColor;
    property SurfaceHeight: Integer read GetSurfaceHeight;
    property SurfaceWidth: Integer read GetSurfaceWidth;
    property OnActivate: TWizardPageNotifyEvent read FOnActivate write FOnActivate;
    property OnBackButtonClick: TWizardPageButtonEvent read FOnBackButtonClick write FOnBackButtonClick;
    property OnCancelButtonClick: TWizardPageCancelEvent read FOnCancelButtonClick write FOnCancelButtonClick;
    property OnNextButtonClick: TWizardPageButtonEvent read FOnNextButtonClick write FOnNextButtonClick;
    property OnShouldSkipPage: TWizardPageShouldSkipEvent read FOnShouldSkipPage write FOnShouldSkipPage;
  end;

  TWizardForm = class(TSetupForm)
    FCancelButton: TNewButton;
    FNextButton: TNewButton;
    FBackButton: TNewButton;
    FOuterNotebook: TNewNotebook;
    FInnerNotebook: TNewNotebook;
    FWelcomePage: TNewNotebookPage;
    FInnerPage: TNewNotebookPage;
    FFinishedPage: TNewNotebookPage;
    FLicensePage: TNewNotebookPage;
    FPasswordPage: TNewNotebookPage;
    FInfoBeforePage: TNewNotebookPage;
    FUserInfoPage: TNewNotebookPage;
    FSelectDirPage: TNewNotebookPage;
    FSelectComponentsPage: TNewNotebookPage;
    FSelectProgramGroupPage: TNewNotebookPage;
    FSelectTasksPage: TNewNotebookPage;
    FReadyPage: TNewNotebookPage;
    FPreparingPage: TNewNotebookPage;
    FInstallingPage: TNewNotebookPage;
    FInfoAfterPage: TNewNotebookPage;
    FDiskSpaceLabel: TNewStaticText;
    FDirEdit: TEdit;
    FGroupEdit: TNewEdit;
    FNoIconsCheck: TNewCheckBox;
    FPasswordLabel: TNewStaticText;
    FPasswordEdit: TPasswordEdit;
    FPasswordEditLabel: TNewStaticText;
    FReadyMemo: TNewMemo;
    FTypesCombo: TNewComboBox;
    FBevel: TBevel;
    FWizardBitmapImage: TBitmapImage;
    FWelcomeLabel1: TNewStaticText;
    FInfoBeforeMemo: TRichEditViewer;
    FInfoBeforeClickLabel: TNewStaticText;
    FMainPanel: TPanel;
    FBevel1: TBevel;
    FPageNameLabel: TNewStaticText;
    FPageDescriptionLabel: TNewStaticText;
    FWizardSmallBitmapImage: TBitmapImage;
    FReadyLabel: TNewStaticText;
    FFinishedLabel: TNewStaticText;
    FYesRadio: TNewRadioButton;
    FNoRadio: TNewRadioButton;
    FWizardBitmapImage2: TBitmapImage;
    FWelcomeLabel2: TNewStaticText;
    FLicenseLabel1: TNewStaticText;
    FLicenseMemo: TRichEditViewer;
    FInfoAfterMemo: TRichEditViewer;
    FInfoAfterClickLabel: TNewStaticText;
    FComponentsList: TNewCheckListBox;
    FComponentsDiskSpaceLabel: TNewStaticText;
    FBeveledLabel: TNewStaticText;
    FStatusLabel: TNewStaticText;
    FFilenameLabel: TNewStaticText;
    FProgressGauge: TNewProgressBar;
    FSelectDirLabel: TNewStaticText;
    FSelectStartMenuFolderLabel: TNewStaticText;
    FSelectComponentsLabel: TNewStaticText;
    FSelectTasksLabel: TNewStaticText;
    FLicenseAcceptedRadio: TNewRadioButton;
    FLicenseNotAcceptedRadio: TNewRadioButton;
    FUserInfoNameLabel: TNewStaticText;
    FUserInfoNameEdit: TNewEdit;
    FUserInfoOrgLabel: TNewStaticText;
    FUserInfoOrgEdit: TNewEdit;
    FPreparingErrorBitmapImage: TBitmapImage;
    FPreparingLabel: TNewStaticText;
    FFinishedHeadingLabel: TNewStaticText;
    FUserInfoSerialLabel: TNewStaticText;
    FUserInfoSerialEdit: TNewEdit;
    FTasksList: TNewCheckListBox;
    FRunList: TNewCheckListBox;
    FDirBrowseButton: TNewButton;
    FGroupBrowseButton: TNewButton;
    FSelectDirBitmapImage: TBitmapImage;
    FSelectGroupBitmapImage: TBitmapImage;
    FSelectDirBrowseLabel: TNewStaticText;
    FSelectStartMenuFolderBrowseLabel: TNewStaticText;
    FPreparingYesRadio: TNewRadioButton;
    FPreparingNoRadio: TNewRadioButton;
    FPreparingMemo: TNewMemo;
    procedure NextButtonClick(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure NoIconsCheckClick(Sender: TObject);
    procedure TypesComboChange(Sender: TObject);
    procedure ComponentsListClickCheck(Sender: TObject);
    procedure LicenseAcceptedRadioClick(Sender: TObject);
    procedure LicenseNotAcceptedRadioClick(Sender: TObject);
    procedure UserInfoEditChange(Sender: TObject);
    procedure DirBrowseButtonClick(Sender: TObject);
    procedure GroupBrowseButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FPageList: TList;
    FCurPageID, FNextPageID: Integer;
    ExpandedDefaultDirName, ExpandedDefaultGroupName: String;
    FPrevAppDir, PrevGroup, PrevSetupType, PrevUserInfoName, PrevUserInfoOrg, PrevUserInfoSerial: String;
    PrevNoIcons: Boolean;
    PrevSelectedComponents, PrevDeselectedComponents: TStringList;
    PrevSelectedTasks, PrevDeselectedTasks: TStringList;
    DisableDirPage, DisableProgramGroupPage: Boolean;
    InitialSelectedComponents: TStringList;
    InitialSetupTypeIndex: Integer;
    HasLargeComponents: Boolean;
    DoneWithWizard: Boolean;
    PrepareToInstallNeedsRestart: Boolean;
    EnableAnchorOuterPagesOnResize: Boolean;
    EnableAdjustReadyLabelHeightOnResize: Boolean;
    procedure AdjustFocus;
    procedure AnchorOuterPages;
    procedure CalcCurrentComponentsSpace;
    procedure ChangeReadyLabel(const S: String);
    function CheckSerialOk: Boolean;
    procedure CreateTaskButtons(const SelectedComponents: TStringList);
    procedure FindPreviousData;
    function GetPreviousPageID: Integer;
    function PrepareToInstall(const WizardComponents, WizardTasks: TStringList): String;
    function QueryRestartManager(const WizardComponents, WizardTasks: TStringList): String;
    procedure RegisterExistingPage(const ID: Integer;
     const AOuterNotebookPage, AInnerNotebookPage: TNewNotebookPage;
     const ACaption, ADescription: String);
    procedure SelectComponents(const SelectComponents, DeselectComponents: TStringList; const KeepFixedComponents: Boolean); overload;
    procedure SelectComponentsFromType(const TypeName: String; const OnlySelectFixedComponents: Boolean);
    procedure SelectTasks(const SelectTasks, DeselectTasks: TStringList); overload;
    function ShouldSkipPage(const PageID: Integer): Boolean;
    procedure UpdateComponentSizes;
    procedure UpdateComponentSizesEnum(Index: Integer; HasChildren: Boolean; Ext: LongInt);
    procedure UpdateCurPageButtonState;
    procedure UpdatePage(const PageID: Integer);
    procedure UpdateSelectTasksPage;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
  public
    { Public declarations }
    PrepareToInstallFailureMessage: String;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddPage(const APage: TWizardPage; const AfterID: Integer);
    function AdjustLabelHeight(const ALabel: TNewStaticText): Integer;
    function AdjustLinkLabelHeight(const ALinkLabel: TNewLinkLabel): Integer;
    procedure CallCancelButtonClick(var ACancel, AConfirm: Boolean);
    procedure ChangeFinishedLabel(const S: String);
    procedure ClickToStartPage;
    procedure ClickThroughPages;
    procedure DirTreeRename(Sender: TCustomFolderTreeView; var NewName: string; var Accept: Boolean);
    procedure GetComponents(SelectedComponents, DeselectedComponents: TStringList);
    procedure GetSelectedComponents(Components: TStringList; const Descriptions, IndentDescriptions: Boolean);
    procedure GetSelectedTasks(Tasks: TStringList; const Descriptions, IndentDescriptions, GroupDescriptions: Boolean);
    function GetSetupType: PSetupTypeEntry;
    procedure GetTasks(SelectedTasks, DeselectedTasks: TStringList);
    procedure GroupTreeRename(Sender: TCustomFolderTreeView; var NewName: string; var Accept: Boolean);
    procedure IncTopDecHeight(const AControl: TControl; const Amount: Integer);
    function PageFromID(const ID: Integer): TWizardPage;
    function PageIndexFromID(const ID: Integer): Integer;
    procedure SetCurPage(const NewPageID: Integer);
    procedure SelectComponents(const ASelectComponents: TStringList); overload;
    procedure SelectTasks(const ASelectTasks: TStringList); overload;
    procedure FlipSizeAndCenterIfNeeded(const ACenterInsideControl: Boolean;
      const CenterInsideControlCtl: TWinControl; const CenterInsideControlInsideClientArea: Boolean); override;
    procedure UpdateRunList(const SelectedComponents, SelectedTasks: TStringList);
    function ValidateDirEdit: Boolean;
    function ValidateGroupEdit: Boolean;
  published
    property CurPageID: Integer read FCurPageID;
    property PrevAppDir: String read FPrevAppDir;
    property CancelButton: TNewButton read FCancelButton;
    property NextButton: TNewButton read FNextButton;
    property BackButton: TNewButton read FBackButton;
    property OuterNotebook: TNewNotebook read FOuterNotebook;
    property InnerNotebook: TNewNotebook read FInnerNotebook;
    property WelcomePage: TNewNotebookPage read FWelcomePage;
    property InnerPage: TNewNotebookPage read FInnerPage;
    property FinishedPage: TNewNotebookPage read FFinishedPage;
    property LicensePage: TNewNotebookPage read FLicensePage;
    property PasswordPage: TNewNotebookPage read FPasswordPage;
    property InfoBeforePage: TNewNotebookPage read FInfoBeforePage;
    property UserInfoPage: TNewNotebookPage read FUserInfoPage;
    property SelectDirPage: TNewNotebookPage read FSelectDirPage;
    property SelectComponentsPage: TNewNotebookPage read FSelectComponentsPage;
    property SelectProgramGroupPage: TNewNotebookPage read FSelectProgramGroupPage;
    property SelectTasksPage: TNewNotebookPage read FSelectTasksPage;
    property ReadyPage: TNewNotebookPage read FReadyPage;
    property PreparingPage: TNewNotebookPage read FPreparingPage;
    property InstallingPage: TNewNotebookPage read FInstallingPage;
    property InfoAfterPage: TNewNotebookPage read FInfoAfterPage;
    property DiskSpaceLabel: TNewStaticText read FDiskSpaceLabel;
    property DirEdit: TEdit read FDirEdit;
    property GroupEdit: TNewEdit read FGroupEdit;
    property NoIconsCheck: TNewCheckBox read FNoIconsCheck;
    property PasswordLabel: TNewStaticText read FPasswordLabel;
    property PasswordEdit: TPasswordEdit read FPasswordEdit;
    property PasswordEditLabel: TNewStaticText read FPasswordEditLabel;
    property ReadyMemo: TNewMemo read FReadyMemo;
    property TypesCombo: TNewComboBox read FTypesCombo;
    property Bevel: TBevel read FBevel;
    property WizardBitmapImage: TBitmapImage read FWizardBitmapImage;
    property WelcomeLabel1: TNewStaticText read FWelcomeLabel1;
    property InfoBeforeMemo: TRichEditViewer read FInfoBeforeMemo;
    property InfoBeforeClickLabel: TNewStaticText read FInfoBeforeClickLabel;
    property MainPanel: TPanel read FMainPanel;
    property Bevel1: TBevel read FBevel1;
    property PageNameLabel: TNewStaticText read FPageNameLabel;
    property PageDescriptionLabel: TNewStaticText read FPageDescriptionLabel;
    property WizardSmallBitmapImage: TBitmapImage read FWizardSmallBitmapImage;
    property ReadyLabel: TNewStaticText read FReadyLabel;
    property FinishedLabel: TNewStaticText read FFinishedLabel;
    property YesRadio: TNewRadioButton read FYesRadio;
    property NoRadio: TNewRadioButton read FNoRadio;
    property WizardBitmapImage2: TBitmapImage read FWizardBitmapImage2;
    property WelcomeLabel2: TNewStaticText read FWelcomeLabel2;
    property LicenseLabel1: TNewStaticText read FLicenseLabel1;
    property LicenseMemo: TRichEditViewer read FLicenseMemo;
    property InfoAfterMemo: TRichEditViewer read FInfoAfterMemo;
    property InfoAfterClickLabel: TNewStaticText read FInfoAfterClickLabel;
    property ComponentsList: TNewCheckListBox read FComponentsList;
    property ComponentsDiskSpaceLabel: TNewStaticText read FComponentsDiskSpaceLabel;
    property BeveledLabel: TNewStaticText read FBeveledLabel;
    property StatusLabel: TNewStaticText read FStatusLabel;
    property FilenameLabel: TNewStaticText read FFileNameLabel;
    property ProgressGauge: TNewProgressBar read FProgressGauge;
    property SelectDirLabel: TNewStaticText read FSelectDirLabel;
    property SelectStartMenuFolderLabel: TNewStaticText read FSelectStartMenuFolderLabel;
    property SelectComponentsLabel: TNewStaticText read FSelectComponentsLabel;
    property SelectTasksLabel: TNewStaticText read FSelectTasksLabel;
    property LicenseAcceptedRadio: TNewRadioButton read FLicenseAcceptedRadio;
    property LicenseNotAcceptedRadio: TNewRadioButton read FLicenseNotAcceptedRadio;
    property UserInfoNameLabel: TNewStaticText read FUserInfoNameLabel;
    property UserInfoNameEdit: TNewEdit read FUserInfoNameEdit;
    property UserInfoOrgLabel: TNewStaticText read FUserInfoOrgLabel;
    property UserInfoOrgEdit: TNewEdit read FUserInfoOrgEdit;
    property PreparingErrorBitmapImage: TBitmapImage read FPreparingErrorBitmapImage;
    property PreparingLabel: TNewStaticText read FPreparingLabel;
    property FinishedHeadingLabel: TNewStaticText read FFinishedHeadingLabel;
    property UserInfoSerialLabel: TNewStaticText read FUserInfoSerialLabel;
    property UserInfoSerialEdit: TNewEdit read FUserInfoSerialEdit;
    property TasksList: TNewCheckListBox read FTasksList;
    property RunList: TNewCheckListBox read FRunList;
    property DirBrowseButton: TNewButton read FDirBrowseButton;
    property GroupBrowseButton: TNewButton read FGroupBrowseButton;
    property SelectDirBitmapImage: TBitmapImage read FSelectDirBitmapImage;
    property SelectGroupBitmapImage: TBitmapImage read FSelectGroupBitmapImage;
    property SelectDirBrowseLabel: TNewStaticText read FSelectDirBrowseLabel;
    property SelectStartMenuFolderBrowseLabel: TNewStaticText read FSelectStartMenuFolderBrowseLabel;
    property PreparingYesRadio: TNewRadioButton read FPreparingYesRadio;
    property PreparingNoRadio: TNewRadioButton read FPreparingNoRadio;
    property PreparingMemo: TNewMemo read FPreparingMemo;
  end;

var
  WizardForm: TWizardForm;

function ExpandSetupMessage(const Msg: String): String; overload;
function ExpandSetupMessage(const ID: TSetupMessageID): String; overload;
function ListContains(const List: TStringList; const S: String): Boolean;
procedure TidyUpDirName(var Path: String);
procedure TidyUpGroupName(var Path: String);
function ValidateCustomDirEdit(const AEdit: TEdit;
  const AllowUNCPath, AllowRootDirectory, AllowNetworkDrive: Boolean): Boolean;

implementation

uses
  ShellApi, ShlObj, Types,
  PathFunc, RestartManager,
  SetupLdrAndSetup.Messages, Setup.MainForm, Setup.MainFunc, Shared.CommonFunc.Vcl,
  Shared.CommonFunc, Setup.InstFunc, Setup.SelectFolderForm, Setup.FileExtractor,
  Setup.LoggingFunc, Setup.ScriptRunner, Shared.SetupTypes, Shared.SetupSteps;

{$R *.DFM}

const
  BadDirChars = '/:*?"<>|';

var
  CurrentComponentsSpace: Integer64;

function IntToKBStr(const I: Integer64): String;
var
  X: Extended;
begin
  X := Comp(I) / 1024;
  if Frac(X) > 0 then
    X := Int(X) + 1;  { always round up }
  Result := Format('%.0n', [X]);
end;

function IntToMBStr(const I: Integer64): String;
var
  X: Extended;
begin
  X := (Comp(I) / 1048576) * 10; { * 10 to include a decimal }
  if Frac(X) > 0 then
    X := Int(X) + 1;  { always round up }
  X := X / 10;
  Result := Format('%.1n', [X]);
end;

function IntToGBStr(const I: Integer64): String;
var
  X: Extended;
begin
  X := (Comp(I) / 1073741824) * 100; { * 100 to include 2 decimals }
  if Frac(X) > 0 then
    X := Int(X) + 1;  { always round up }
  X := X / 100;
  Result := Format('%.2n', [X]);
end;

function ExpandSetupMessageEx(const Msg: String; const Space: Integer64): String; overload;
begin
  Result := Msg;
  {don't localize these}
  StringChange(Result, '[name]', ExpandedAppName);
  StringChange(Result, '[name/ver]', ExpandedAppVerName);
  StringChange(Result, '[kb]', IntToKBStr(Space));
  StringChange(Result, '[mb]', IntToMBStr(Space));
  StringChange(Result, '[gb]', IntToGBStr(Space));
end;

function ExpandSetupMessageEx(const ID: TSetupMessageID; const Space: Integer64): String; overload;
begin
  Result := ExpandSetupMessageEx(SetupMessages[ID], Space);
end;

function ExpandMBOrGBSetupMessage(const MBID, GBID: TSetupMessageID;
  const Space: Integer64): String;
begin
  if Comp(Space) > 1048471142 then begin
    { Don't allow it to display 1000.0 MB or more. Takes the 'always round up' into account:
      1048471142 bytes = 999.8999996185303 MB = '999.9 MB',
      1048471143 bytes = 999.9000005722046 MB = '1,000.0 MB'. }
    Result := ExpandSetupMessageEx(GBID, Space)
  end else
    Result := ExpandSetupMessageEx(MBID, Space);
end;

function ExpandSetupMessage(const Msg: String): String; overload;
begin
  Result := ExpandSetupMessageEx(Msg, MinimumSpace);
end;

function ExpandSetupMessage(const ID: TSetupMessageID): String; overload;
begin
  Result := ExpandSetupMessageEx(ID, MinimumSpace);
end;

function ListContains(const List: TStringList; const S: String): Boolean;
{ Similar to "List.IndexOf(S) <> -1", except it uses CompareText instead of
  AnsiCompareText (which is locale-sensitive and thus unsuitable for our
  purposes). }
var
  I: Integer;
begin
  for I := 0 to List.Count-1 do
    if CompareText(List[I], S) = 0 then begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

procedure TidyUpDirName(var Path: String);
{ Tidies up a directory name }
begin
  { Trim spaces, normalize slashes, remove any trailing backslash, then repeat
    the process if necessary (e.g. in the 'C:\Program Files\My Program\ \'
    case) }
  repeat
    Path := RemoveBackslashUnlessRoot(PathNormalizeSlashes(Trim(Path)));
  until Length(Path) = Length(Trim(Path));
end;

procedure TidyUpGroupName(var Path: String);
{ Tidies up a program group name }
begin
  { Trim spaces, remove leading/extra/trailing backslash(es), then repeat the
    process if necessary (e.g. in the '\ \My Program\ \' case) }
  repeat
    Path := Trim(Path);
    while (Path <> '') and PathCharIsSlash(Path[1]) do
      Delete(Path, 1, 1);
    Path := RemoveBackslash(PathNormalizeSlashes(Path));
  until Length(Path) = Length(Trim(Path));
end;

function ContainsControlCharacters(const S: String): Boolean;
{ Returns True if S contains any control characters (#0..#31) }
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if S[I] <= #31 then begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function PathComponentsContainTrailingSpaces(const S: String): Boolean;
{ Returns True if one or more components of the path contain trailing spaces,
  which are invalid in Win32. }
var
  P: PChar;
begin
  P := PChar(S);
  while P^ <> #0 do begin
    if (P^ = ' ') and ((P[1] = '\') or (P[1] = #0)) then begin
      Result := True;
      Exit;
    end;
    P := PathStrNextChar(P);
  end;
  Result := False;
end;

function PathComponentsContainInvalidDots(const S: String): Boolean;
{ Returns True if one or more components of the path contain only dots,
  i.e. '.', '..', '...', etc. One or two dots represent relative paths; three
  or more dots are invalid. }
var
  P: PChar;
  HasDots: Boolean;
begin
  P := PChar(S);
  while P^ <> #0 do begin
    { Skip over leading spaces; we want ' .' to return True also }
    while P^ = ' ' do
      Inc(P);
    HasDots := False;
    while P^ = '.' do begin
      HasDots := True;
      Inc(P);
    end;
    { Skip over trailing spaces; we want '. ' to return True also }
    while P^ = ' ' do
      Inc(P);
    if HasDots and ((P^ = '\') or (P^ = #0)) then begin
      Result := True;
      Exit;
    end;
    { Skip to next component }
    while (P^ <> #0) and (P^ <> '\') do
      P := PathStrNextChar(P);
    if P^ = '\' then
      Inc(P);
  end;
  Result := False;
end;

function SpaceString(const S: String): String;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do begin
    if S[I] = ' ' then Continue;
    if Result <> '' then Result := Result + ' ';
    Result := Result + S[I];
  end;
end;

function TWizardForm.AdjustLabelHeight(const ALabel: TNewStaticText): Integer;
{ Increases or decreases a label's height so that all text fits.
  Returns the difference in height. }
begin
  Result := ALabel.AdjustHeight;
end;

function TWizardForm.AdjustLinkLabelHeight(const ALinkLabel: TNewLinkLabel): Integer;
begin
  Result := ALinkLabel.AdjustHeight;
end;

procedure TWizardForm.IncTopDecHeight(const AControl: TControl; const Amount: Integer);
begin
  AControl.SetBounds(AControl.Left, AControl.Top + Amount,
    AControl.Width, AControl.Height - Amount);
end;

function TWizardForm.CheckSerialOk(): Boolean;
begin
  if NeedSerial and (CodeRunner <> nil) then begin
    WizardUserInfoName := UserInfoNameEdit.Text;
    WizardUserInfoOrg := UserInfoOrgEdit.Text;
    WizardUserInfoSerial := UserInfoSerialEdit.Text;
    Result := CodeRunner.RunBooleanFunctions('CheckSerial', [UserInfoSerialEdit.Text], bcTrue, True, False)
  end else
    Result := True;
end;

procedure TWizardForm.CalcCurrentComponentsSpace();
var
  SelectedComponents: TStringList;
  I: Integer;
  CurFile: PSetupFileEntry;
begin
  CurrentComponentsSpace := SetupHeader.ExtraDiskSpaceRequired;

  SelectedComponents := TStringList.Create();
  GetSelectedComponents(SelectedComponents, False, False);

  //we can't simply sum component sizes because of shared files -> add file sizes
  for I := 0 to Entries[seFile].Count-1 do begin
    CurFile := PSetupFileEntry(Entries[seFile][I]);
    if (CurFile.Tasks = '') and (CurFile.Check = '') and {don't count tasks or scripted entries}
       ShouldProcessFileEntry(SelectedComponents, nil, CurFile, True) then begin
      with CurFile^ do begin
        if LocationEntry <> -1 then
          Inc6464(CurrentComponentsSpace, PSetupFileLocationEntry(Entries[seFileLocation][LocationEntry])^.OriginalSize)
        else
          Inc6464(CurrentComponentsSpace, ExternalSize)
      end;
    end;
  end;

  //don't forget to add extradiskspacerequired values
  for I := 0 to Entries[seComponent].Count-1 do
    with PSetupComponentEntry(Entries[seComponent][I])^ do
      if ListContains(SelectedComponents, Name) then
        Inc6464(CurrentComponentsSpace, ExtraDiskSpaceRequired);

  SelectedComponents.Free();

  ComponentsDiskSpaceLabel.Caption := ExpandMBOrGBSetupMessage(
    msgComponentsDiskSpaceMBLabel, msgComponentsDiskSpaceGBLabel, CurrentComponentsSpace);
end;

procedure TWizardForm.UpdateComponentSizesEnum(Index: Integer; HasChildren: Boolean; Ext: LongInt);
var
  ComponentEntry: PSetupComponentEntry;
  ComponentSize, ChildrenSize: Integer64;
begin
  ComponentEntry := PSetupComponentEntry(ComponentsList.ItemObject[Index]);

  ChildrenSize.Hi := 0;
  ChildrenSize.Lo := 0;
  if HasChildren then
    ComponentsList.EnumChildrenOf(Index, UpdateComponentSizesEnum, LongInt(@ChildrenSize));
  ComponentSize := ComponentEntry.Size;
  Inc6464(ComponentSize, ChildrenSize);
  if ComponentsList.Checked[Index] then
    Inc6464(Integer64(Pointer(Ext)^), ComponentSize);

  if (ComponentSize.Lo <> 0) or (ComponentSize.Hi <> 0) then begin
    if not HasLargeComponents then
      ComponentsList.ItemSubItem[Index] := FmtSetupMessage1(msgComponentSize1, IntToKBStr(ComponentSize))
    else
      ComponentsList.ItemSubItem[Index] := FmtSetupMessage1(msgComponentSize2, IntToMBStr(ComponentSize));
  end else
    ComponentsList.ItemSubItem[Index] := '';
end;

procedure TWizardForm.UpdateComponentSizes();
var
  Size: Integer64;
begin
  if shShowComponentSizes in SetupHeader.Options then begin
    Size.Hi := 0;
    Size.Lo := 0;
    ComponentsList.EnumChildrenOf(-1, UpdateComponentSizesEnum, LongInt(@Size));
  end;
end;

{ TWizardPage }

constructor TWizardPage.Create(AOwner: TComponent);
begin
  inherited;
  FWizardForm := AOwner as TWizardForm;
end;

destructor TWizardPage.Destroy;
begin
  if Assigned(FWizardForm) and Assigned(FWizardForm.FPageList) then
    FWizardForm.FPageList.Remove(Self);
  inherited;
end;

procedure TWizardPage.Activate;
begin
  if Assigned(FOnActivate) then
    FOnActivate(Self);
end;

procedure TWizardPage.BackButtonClick(var AContinue: Boolean);
begin
  if Assigned(FOnBackButtonClick) then
    AContinue := FOnBackButtonClick(Self);
end;

procedure TWizardPage.CancelButtonClick(var ACancel, AConfirm: Boolean);
begin
  if Assigned(FOnCancelButtonClick) then
    FOnCancelButtonClick(Self, ACancel, AConfirm);
end;

procedure TWizardPage.NextButtonClick(var AContinue: Boolean);
begin
  if Assigned(FOnNextButtonClick) then
    AContinue := FOnNextButtonClick(Self);
end;

procedure TWizardPage.ShouldSkipPage(var AShouldSkip: Boolean);
begin
  if Assigned(FOnShouldSkipPage) then
    AShouldSkip := FOnShouldSkipPage(Self);
end;

function TWizardPage.GetSurface: TNewNotebookPage;
begin
  if FOuterNotebookPage = FWizardForm.InnerPage then
    Result := FInnerNotebookPage
  else
    Result := FOuterNotebookPage;
end;

function TWizardPage.GetSurfaceColor: TColor;
begin
  Result := TNewNotebook(Surface.Parent).Color;
end;

function TWizardPage.GetSurfaceHeight: Integer;
begin
  Result := Surface.Parent.Height;
end;

function TWizardPage.GetSurfaceWidth: Integer;
begin
  Result := Surface.Parent.Width;
end;

procedure TWizardPage.SetCaption(const Value: String);
begin
  FCaption := ExpandSetupMessage(Value);
  SyncCaptionAndDescription;
end;

procedure TWizardPage.SetDescription(const Value: String);
begin
  FDescription := ExpandSetupMessage(Value);
  SyncCaptionAndDescription;
end;

procedure TWizardPage.SyncCaptionAndDescription;
begin
  if FWizardForm.CurPageID = FID then begin
    FWizardForm.PageNameLabel.Caption := FCaption;
    FWizardForm.PageDescriptionLabel.Caption := FDescription;
  end;
end;

{ TWizardForm }

constructor TWizardForm.Create(AOwner: TComponent);
{ Do all initialization of the wizard form. We're overriding Create instead of
  using the FormCreate event, because if an exception is raised in FormCreate
  it's not propagated out. }

  function SelectBestImage(WizardImages: TList; TargetWidth, TargetHeight: Integer): TBitmap;
  var
    TargetArea, Difference, SmallestDifference, I: Integer;
  begin
    if WizardImages.Count <> 1 then begin
      { Find the image with the smallest area difference compared to the target area. }
      TargetArea := TargetWidth*TargetHeight;
      SmallestDifference := -1;
      Result := nil;
      for I := 0 to WizardImages.Count-1 do begin
        Difference := Abs(TargetArea-TBitmap(WizardImages[I]).Width*TBitmap(WizardImages[I]).Height);
        if (SmallestDifference = -1) or (Difference < SmallestDifference) then begin
          Result := WizardImages[I];
          SmallestDifference := Difference;
        end;
      end;
    end else
      Result := WizardImages[0];
  end;

var
  X, W1, W2: Integer;
  SystemMenu: HMENU;
  P: String;
  I, DefaultSetupTypeIndex: Integer;
  IgnoreInitComponents: Boolean;
  TypeEntry: PSetupTypeEntry;
  ComponentEntry: PSetupComponentEntry;
begin
  inherited;

  FPageList := TList.Create;
  InitialSelectedComponents := TStringList.Create();
  PrevSelectedComponents := TStringList.Create();
  PrevDeselectedComponents := TStringList.Create();
  PrevSelectedTasks := TStringList.Create();
  PrevDeselectedTasks := TStringList.Create();

  MainPanel.ParentBackground := False;

  { Not sure why the following is needed but various things related to
    positioning and anchoring don't work without this (you get positions of
    page controls back as if there was no anchoring until the page handle
    is automatically created. Cause might be related to the comment in
    TNewNotebook.AlignControls. }
  for I := 0 to OuterNotebook.PageCount-1 do
    OuterNotebook.Pages[I].HandleNeeded;
  for I := 0 to InnerNotebook.PageCount-1 do
    InnerNotebook.Pages[I].HandleNeeded;

  InitializeFont;
  SetFontNameSize(WelcomeLabel1.Font, LangOptions.WelcomeFontName,
    LangOptions.WelcomeFontSize, '', 12);
  WelcomeLabel1.Font.Style := [fsBold];
  PageNameLabel.Font.Style := [fsBold];

  if shDisableWelcomePage in SetupHeader.Options then
    Caption := FmtSetupMessage1(msgSetupWindowTitle, ExpandedAppVerName)
  else
    Caption := FmtSetupMessage1(msgSetupWindowTitle, ExpandedAppName);

  if shWizardResizable in SetupHeader.Options then begin
    const SaveClientWidth = ClientWidth;
    const SaveClientHeight = ClientHeight;
    BorderStyle := bsSizeable;
    ClientWidth := SaveClientWidth;
    ClientHeight := SaveClientHeight;
    EnableAnchorOuterPagesOnResize := True;
    { Do not allow user to resize it smaller than 100% nor larger than 150%. }
    Constraints.MinHeight := Height;
    Constraints.MinWidth := Width;
    Constraints.MaxHeight := MulDiv(Height, 150, 100);
    Constraints.MaxWidth := MulDiv(Width, 150, 100);
  end;
  
  { Position the buttons, and scale their size }
  W1 := CalculateButtonWidth([SetupMessages[msgButtonBack], SetupMessages[msgButtonCancel],
    SetupMessages[msgButtonFinish], SetupMessages[msgButtonInstall],
    SetupMessages[msgButtonNext]]);  { width of each button }
  W2 := ScalePixelsX(10);  { margin, and space between Next & Cancel }

  BackButton.Width := W1;
  NextButton.Width := W1;
  CancelButton.Width := W1;
  X := ClientWidth - W2 - W1;
  CancelButton.Left := X;
  Dec(X, W2);
  Dec(X, W1);
  NextButton.Left := X;
  Dec(X, W1);
  BackButton.Left := X;

  { Initialize wizard style }
  if SetupHeader.WizardStyle = wsModern then begin
    OuterNotebook.Color := clWindow;
    Bevel1.Visible := False;
  end;

  { Correct aspect ratio of the large wizard images after scaling }
  AnchorOuterPages;

  { Adjust small wizard image's size and position }
  begin
    { Make sure the control is still perfectly square after scaling and flush
      with the right edge of its parent }
    I := WizardSmallBitmapImage.Left;
    WizardSmallBitmapImage.Width := WizardSmallBitmapImage.Height;
    WizardSmallBitmapImage.Left := WizardSmallBitmapImage.Parent.ClientWidth -
      WizardSmallBitmapImage.Width;
    Dec(I, WizardSmallBitmapImage.Left);
    PageNameLabel.Width := PageNameLabel.Width - I;
    PageDescriptionLabel.Width := PageDescriptionLabel.Width - I;

    { Reduce the size of the control if appropriate:
      - If the user supplied a single image AND that image is not larger than
        the default control size before scaling (58x58), then reduce the
        control size to match the image dimensions. That avoids stretching to
        58x58 when the user is purposely using a smaller-than-default image
        (such as 55x55 or 32x32) and WizardImageStretch=yes.
      - Otherwise, it's unclear what size/shape the user prefers for the
        control. Keep the default control size. }
    var NewWidth := TBitmap(WizardSmallImages[0]).Width;
    var NewHeight := TBitmap(WizardSmallImages[0]).Height;
    if (WizardSmallImages.Count > 1) or
       (NewWidth > 58) or
       (NewHeight > 58) then begin
      NewWidth := 58;
      NewHeight := 58;
    end;

    { Scale the new width and height }
    NewWidth := MulDiv(NewWidth, WizardSmallBitmapImage.Width, 58);
    NewHeight := MulDiv(NewHeight, WizardSmallBitmapImage.Height, 58);

    I := WizardSmallBitmapImage.Height - NewHeight;
    if I > 0 then begin
      WizardSmallBitmapImage.Height := WizardSmallBitmapImage.Height - I;
      WizardSmallBitmapImage.Top := WizardSmallBitmapImage.Top + (I div 2);
    end;
    I := WizardSmallBitmapImage.Width - NewWidth;
    if I > 0 then begin
      WizardSmallBitmapImage.Width := WizardSmallBitmapImage.Width - I;
      WizardSmallBitmapImage.Left := WizardSmallBitmapImage.Left + (I div 2);
    end;
  end;

  { Initialize images }
  WizardBitmapImage.Bitmap := SelectBestImage(WizardImages, WizardBitmapImage.Width, WizardBitmapImage.Height);
  WizardBitmapImage.Center := True;
  WizardBitmapImage.Stretch := (shWizardImageStretch in SetupHeader.Options);
  WizardBitmapImage2.Bitmap := WizardBitmapImage.Bitmap;
  WizardBitmapImage2.Center := True;
  WizardBitmapImage2.Stretch := (shWizardImageStretch in SetupHeader.Options);
  WizardSmallBitmapImage.Bitmap := SelectBestImage(WizardSmallImages, WizardSmallBitmapImage.Width, WizardSmallBitmapImage.Height);
  WizardSmallBitmapImage.Stretch := (shWizardImageStretch in SetupHeader.Options);
  SelectDirBitmapImage.InitializeFromIcon(HInstance, 'Z_DIRICON', SelectDirPage.Color, [32, 48, 64]); {don't localize}
  SelectGroupBitmapImage.InitializeFromIcon(HInstance, 'Z_GROUPICON', SelectProgramGroupPage.Color, [32, 48, 64]); {don't localize}
  PreparingErrorBitmapImage.InitializeFromIcon(HInstance, 'Z_STOPICON', PreparingPage.Color, [16, 24, 32]); {don't localize}

  { Initialize wpWelcome page }
  RegisterExistingPage(wpWelcome, WelcomePage, nil, '', '');
  WelcomeLabel1.Caption := ExpandSetupMessage(msgWelcomeLabel1) + SNewLine;
  AdjustLabelHeight(WelcomeLabel1);
  IncTopDecHeight(WelcomeLabel2, (WelcomeLabel1.Top + WelcomeLabel1.Height) -
    WelcomeLabel2.Top);
  WelcomeLabel2.Caption := ExpandSetupMessage(msgWelcomeLabel2) + SNewLine2 +
    SetupMessages[msgClickNext];

  { Initialize wpLicense page }
  RegisterExistingPage(wpLicense, InnerPage, LicensePage,
    SetupMessages[msgWizardLicense], SetupMessages[msgLicenseLabel]);
  LicenseLabel1.Caption := ExpandSetupMessage(msgLicenseLabel3);
  I := AdjustLabelHeight(LicenseLabel1);
  IncTopDecHeight(LicenseMemo, I);
  LicenseAcceptedRadio.Caption := SetupMessages[msgLicenseAccepted];
  LicenseNotAcceptedRadio.Caption := SetupMessages[msgLicenseNotAccepted];

  { Initialize wpPassword page }
  RegisterExistingPage(wpPassword, InnerPage, PasswordPage,
    SetupMessages[msgWizardPassword], SetupMessages[msgPasswordLabel1]);
  PasswordLabel.Caption := SetupMessages[msgPasswordLabel3];
  PasswordEditLabel.Caption := SetupMessages[msgPasswordEditLabel];
  I := AdjustLabelHeight(PasswordLabel);
  PasswordEditLabel.Top := PasswordEditLabel.Top + I;
  Inc(I, AdjustLabelHeight(PasswordEditLabel));
  PasswordEdit.Top := PasswordEdit.Top + I;

  { Initialize wpInfoBefore page }
  RegisterExistingPage(wpInfoBefore, InnerPage, InfoBeforePage,
    SetupMessages[msgWizardInfoBefore], SetupMessages[msgInfoBeforeLabel]);
  InfoBeforeClickLabel.Caption := SetupMessages[msgInfoBeforeClickLabel];
  I := AdjustLabelHeight(InfoBeforeClickLabel);
  IncTopDecHeight(InfoBeforeMemo, I);

  { Initialize wpUserInfo page }
  RegisterExistingPage(wpUserInfo, InnerPage, UserInfoPage,
    SetupMessages[msgWizardUserInfo], SetupMessages[msgUserInfoDesc]);
  UserInfoNameLabel.Caption := SetupMessages[msgUserInfoName];
  I := AdjustLabelHeight(UserInfoNameLabel);
  UserInfoNameEdit.Top := UserInfoNameEdit.Top + I;

  UserInfoOrgLabel.Top := UserInfoOrgLabel.Top + I;
  UserInfoOrgLabel.Caption := SetupMessages[msgUserInfoOrg];
  Inc(I, AdjustLabelHeight(UserInfoOrgLabel));
  UserInfoOrgEdit.Top := UserInfoOrgEdit.Top + I;

  if NeedSerial then begin
    UserInfoSerialLabel.Top := UserInfoSerialLabel.Top + I;
    UserInfoSerialLabel.Caption := SetupMessages[msgUserInfoSerial];
    Inc(I, AdjustLabelHeight(UserInfoSerialLabel));
    UserInfoSerialEdit.Top := UserInfoSerialEdit.Top + I;
  end else begin
    UserInfoSerialLabel.Visible := False;
    UserInfoSerialEdit.Visible := False;
  end;

  { Initialize wpSelectDir page }
  RegisterExistingPage(wpSelectDir, InnerPage, SelectDirPage,
    SetupMessages[msgWizardSelectDir], ExpandSetupMessage(msgSelectDirDesc));
  SelectDirLabel.Caption := ExpandSetupMessage(msgSelectDirLabel3);
  X := SelectDirBitmapImage.Left + SelectDirBitmapImage.Width + ScalePixelsX(12);
  SelectDirLabel.SetBounds(X, SelectDirLabel.Top,
    SelectDirLabel.Width - (X - SelectDirLabel.Left), SelectDirLabel.Height);
  AdjustLabelHeight(SelectDirLabel);
  if SelectDirLabel.Height < SelectDirBitmapImage.Height then
    SelectDirLabel.Top := SelectDirLabel.Top +
      (SelectDirBitmapImage.Height - (SelectDirLabel.Height - 1)) div 2;
  SelectDirBrowseLabel.Caption := ExpandSetupMessage(msgSelectDirBrowseLabel);
  I := IntMax(
    SelectDirBitmapImage.Top + SelectDirBitmapImage.Height + ScalePixelsY(12),
    SelectDirLabel.Top + SelectDirLabel.Height - 1 + ScalePixelsY(13)) -
    SelectDirBrowseLabel.Top;
  SelectDirBrowseLabel.Top := SelectDirBrowseLabel.Top + I;
  Inc(I, AdjustLabelHeight(SelectDirBrowseLabel));
  DirEdit.Top := DirEdit.Top + I;
  TryEnableAutoCompleteFileSystem(DirEdit.Handle);
  DirBrowseButton.Caption := SetupMessages[msgButtonWizardBrowse];
  X := CalculateButtonWidth([SetupMessages[msgButtonWizardBrowse]]);
  DirBrowseButton.SetBounds(InnerNotebook.Width - X,
    DirBrowseButton.Top + I, X, DirBrowseButton.Height);
  DirEdit.Width := DirBrowseButton.Left - ScalePixelsX(10) - DirEdit.Left;
  DiskSpaceLabel.Caption := ExpandMBOrGBSetupMessage(
    msgDiskSpaceMBLabel, msgDiskSpaceGBLabel, MinimumSpace);
  DiskSpaceLabel.Top := DiskSpaceLabel.Top - AdjustLabelHeight(DiskSpaceLabel);

  { Initialize wpSelectComponents page }
  RegisterExistingPage(wpSelectComponents, InnerPage, SelectComponentsPage,
    SetupMessages[msgWizardSelectComponents], ExpandSetupMessage(msgSelectComponentsDesc));
  SelectComponentsLabel.Caption := ExpandSetupMessage(msgSelectComponentsLabel2);
  I := AdjustLabelHeight(SelectComponentsLabel);
  TypesCombo.Top := TypesCombo.Top + I;
  IncTopDecHeight(ComponentsList, I);
  ComponentsDiskSpaceLabel.Caption := ExpandMBOrGBSetupMessage(
    msgComponentsDiskSpaceMBLabel, msgComponentsDiskSpaceGBLabel, MinimumSpace);
  AdjustLabelHeight(ComponentsDiskSpaceLabel);

  if HasCustomType and (Entries[seType].Count = 1) then begin
    TypesCombo.Visible := False;
    IncTopDecHeight(ComponentsList, TypesCombo.Top - ComponentsList.Top);
  end;

  { Initialize wpSelectProgramGroup page }
  RegisterExistingPage(wpSelectProgramGroup, InnerPage, SelectProgramGroupPage,
    SetupMessages[msgWizardSelectProgramGroup], ExpandSetupMessage(msgSelectStartMenuFolderDesc));
  SelectStartMenuFolderLabel.Caption := ExpandSetupMessage(msgSelectStartMenuFolderLabel3);
  X := SelectGroupBitmapImage.Left + SelectGroupBitmapImage.Width + ScalePixelsX(12);
  SelectStartMenuFolderLabel.SetBounds(X, SelectStartMenuFolderLabel.Top,
    SelectStartMenuFolderLabel.Width - (X - SelectStartMenuFolderLabel.Left),
    SelectStartMenuFolderLabel.Height);
  AdjustLabelHeight(SelectStartMenuFolderLabel);
  if SelectStartMenuFolderLabel.Height < SelectGroupBitmapImage.Height then
    SelectStartMenuFolderLabel.Top := SelectStartMenuFolderLabel.Top +
      (SelectGroupBitmapImage.Height - (SelectStartMenuFolderLabel.Height - 1)) div 2;
  SelectStartMenuFolderBrowseLabel.Caption := ExpandSetupMessage(msgSelectStartMenuFolderBrowseLabel);
  I := IntMax(
    SelectGroupBitmapImage.Top + SelectGroupBitmapImage.Height + ScalePixelsY(12),
    SelectStartMenuFolderLabel.Top + SelectStartMenuFolderLabel.Height - 1 + ScalePixelsY(13)) -
    SelectStartMenuFolderBrowseLabel.Top;
  SelectStartMenuFolderBrowseLabel.Top := SelectStartMenuFolderBrowseLabel.Top + I;
  Inc(I, AdjustLabelHeight(SelectStartMenuFolderBrowseLabel));
  GroupEdit.Top := GroupEdit.Top + I;
  GroupBrowseButton.Caption := SetupMessages[msgButtonWizardBrowse];
  X := CalculateButtonWidth([SetupMessages[msgButtonWizardBrowse]]);
  GroupBrowseButton.SetBounds(InnerNotebook.Width - X,
    GroupBrowseButton.Top + I, X, GroupBrowseButton.Height);
  GroupEdit.Width := GroupBrowseButton.Left - ScalePixelsX(10) - GroupEdit.Left;
  NoIconsCheck.Caption := SetupMessages[msgNoProgramGroupCheck2];

  { Initialize wpSelectTasks page }
  RegisterExistingPage(wpSelectTasks, InnerPage, SelectTasksPage,
    SetupMessages[msgWizardSelectTasks], ExpandSetupMessage(msgSelectTasksDesc));
  SelectTasksLabel.Caption := ExpandSetupMessage(msgSelectTasksLabel2);
  I := AdjustLabelHeight(SelectTasksLabel);
  IncTopDecHeight(TasksList, I);
  TasksList.BorderStyle := bsNone;
  TasksList.MinItemHeight := ScalePixelsY(22);
  TasksList.ShowLines := shShowTasksTreeLines in SetupHeader.Options;

  { Initialize wpReady page }
  RegisterExistingPage(wpReady, InnerPage, ReadyPage,
    SetupMessages[msgWizardReady], ExpandSetupMessage(msgReadyLabel1));

  { Initialize wpPreparing page }
  RegisterExistingPage(wpPreparing, InnerPage, PreparingPage,
    SetupMessages[msgWizardPreparing], ExpandSetupMessage(msgPreparingDesc));

  { Initialize wpInstalling page }
  RegisterExistingPage(wpInstalling, InnerPage, InstallingPage,
    SetupMessages[msgWizardInstalling], ExpandSetupMessage(msgInstallingLabel));

  { Initialize wpInfoAfter page }
  RegisterExistingPage(wpInfoAfter, InnerPage, InfoAfterPage,
    SetupMessages[msgWizardInfoAfter], SetupMessages[msgInfoAfterLabel]);
  InfoAfterClickLabel.Caption := SetupMessages[msgInfoAfterClickLabel];
  I := AdjustLabelHeight(InfoAfterClickLabel);
  IncTopDecHeight(InfoAfterMemo, I);

  { Initialize wpFinished page }
  RegisterExistingPage(wpFinished, FinishedPage, nil, '', '');
  SetFontNameSize(FinishedHeadingLabel.Font, LangOptions.WelcomeFontName,
    LangOptions.WelcomeFontSize, '', 12);
  FinishedHeadingLabel.Font.Style := [fsBold];
  FinishedHeadingLabel.Caption := ExpandSetupMessage(msgFinishedHeadingLabel) +
    SNewLine;
  AdjustLabelHeight(FinishedHeadingLabel);
  FinishedLabel.Top := FinishedHeadingLabel.Top + FinishedHeadingLabel.Height;
  YesRadio.Caption := SetupMessages[msgYesRadio];
  NoRadio.Caption := SetupMessages[msgNoRadio];
  RunList.MinItemHeight := ScalePixelsY(22);

  { Initialize BeveledLabel }
  if SetupMessages[msgBeveledLabel] <> '' then
    BeveledLabel.Caption := ' ' + SetupMessages[msgBeveledLabel] + ' '
  else
    BeveledLabel.Caption := '';

  { Don't set UseRichEdit to True on the TRichEditViewers unless they are going
    to be used. There's no need to load riched*.dll unnecessarily. }
  if ActiveLicenseText <> '' then begin
    LicenseMemo.UseRichEdit := True;
    LicenseMemo.RTFText := ActiveLicenseText;
  end;
  if ActiveInfoBeforeText <> '' then begin
    InfoBeforeMemo.UseRichEdit := True;
    InfoBeforeMemo.RTFText := ActiveInfoBeforeText;
  end;
  if ActiveInfoAfterText <> '' then begin
    InfoAfterMemo.UseRichEdit := True;
    InfoAfterMemo.RTFText := ActiveInfoAfterText;
  end;

  { Append an 'About Setup' item to the wizard form also }
  SystemMenu := GetSystemMenu(Handle, False);
  AppendMenu(SystemMenu, MF_SEPARATOR, 0, nil);
  AppendMenu(SystemMenu, MF_STRING, 9999, PChar(SetupMessages[msgAboutSetupMenuItem]));

  { Read settings from a previous install if available }
  FindPreviousData;
  DisableDirPage := (SetupHeader.DisableDirPage = dpYes) or
    ((SetupHeader.DisableDirPage = dpAuto) and (PrevAppDir <> ''));
  DisableProgramGroupPage := (SetupHeader.DisableProgramGroupPage = dpYes) or
    ((SetupHeader.DisableProgramGroupPage = dpAuto) and (PrevGroup <> ''));
  DefaultSetupTypeIndex := -1; //assigned later
  IgnoreInitComponents := False;

  { Assign default user name & organization on User Info page }
  if shUserInfoPage in SetupHeader.Options then begin
    if PrevUserInfoName = '' then begin
      UserInfoNameEdit.Text := ExpandConst(SetupHeader.DefaultUserInfoName);
      UserInfoOrgEdit.Text := ExpandConst(SetupHeader.DefaultUserInfoOrg);
      UserInfoSerialEdit.Text := ExpandConst(SetupHeader.DefaultUserInfoSerial);
    end
    else begin
      UserInfoNameEdit.Text := PrevUserInfoName;
      UserInfoOrgEdit.Text := PrevUserInfoOrg;
      UserInfoSerialEdit.Text := PrevUserInfoSerial;
    end;
  end;

  { Assign default directory name }
  if shCreateAppDir in SetupHeader.Options then begin
    ExpandedDefaultDirName := ExpandConst(SetupHeader.DefaultDirName);
    if InitDir <> '' then
      P := ExpandConstIfPrefixed(InitDir)
    else begin
      P := PrevAppDir;
      if P = '' then
        P := ExpandedDefaultDirName;
    end;
    P := RemoveBackslashUnlessRoot(PathExpand(P));
    DirEdit.Text := P;
  end
  else
    DirEdit.Text := WinDir;

  { Fill types list and assign default type }
  if Entries[seType].Count > 0 then begin
    //first fill list
    TypesCombo.Clear();
    for I := 0 to Entries[seType].Count-1 do begin
      TypeEntry := PSetupTypeEntry(Entries[seType][I]);
      TypesCombo.Items.AddObject(ExpandConst(TypeEntry.Description), TObject(TypeEntry));
      { If a setup type was specified on the command line, use it as default }
      if (DefaultSetupTypeIndex = -1) and (InitSetupType <> '') and
         (CompareText(TypeEntry.Name, InitSetupType) = 0) then begin
        DefaultSetupTypeIndex := I;
        { If components are specified as well, they should be ignored if the
          setup type is non-custom }
        if not (toIsCustom in TypeEntry.Options) then
          IgnoreInitComponents := True;
      end;
    end;
    { Use setup type from previous installation if no type was specified on the
      command line (or if the type specified doesn't exist) }
    if (DefaultSetupTypeIndex = -1) and (PrevSetupType <> '') then begin
      for I := 0 to Entries[seType].Count-1 do begin
        TypeEntry := PSetupTypeEntry(Entries[seType][I]);
        if CompareText(TypeEntry.Name, PrevSetupType) = 0 then begin
          DefaultSetupTypeIndex := I;
          Break;
        end;
      end;
    end;
    //now assign default type
    if DefaultSetupTypeIndex <> -1 then
      TypesCombo.ItemIndex := DefaultSetupTypeIndex
    else
      TypesCombo.ItemIndex := 0;
  end;

  { Fill components list and assign default components}
  //first fill list
  ComponentsList.Clear();
  ComponentsList.Flat := shFlatComponentsList in SetupHeader.Options;
  for I := 0 to Entries[seComponent].Count-1 do begin
    ComponentEntry := PSetupComponentEntry(Entries[seComponent][I]);
    if coExclusive in ComponentEntry.Options then
      ComponentsList.AddRadioButton(ExpandConst(ComponentEntry.Description), '', ComponentEntry.Level,
        False, not (coFixed in ComponentEntry.Options), TObject(ComponentEntry))
    else
      ComponentsList.AddCheckBox(ExpandConst(ComponentEntry.Description), '', ComponentEntry.Level,
        False, not (coFixed in ComponentEntry.Options), ComponentEntry.Used,
        not (coDontInheritCheck in ComponentEntry.Options), TObject(ComponentEntry));
    if (ComponentEntry.Size.Hi <> 0) or (ComponentEntry.Size.Lo >= LongWord(1024*1024)) then
      HasLargeComponents := True;
  end;

  //now assign default components
  if not IgnoreInitComponents and InitComponentsSpecified and HasCustomType then begin
    for I := 0 to Entries[seType].Count-1 do begin
      TypeEntry := PSetupTypeEntry(Entries[seType][I]);
      if toIsCustom in TypeEntry.Options then begin
        TypesCombo.ItemIndex := I;
        SelectComponentsFromType(TypeEntry.Name, True);
        SelectComponents(InitComponents, nil, True);
        Break;
      end;
    end;
  end else begin
    if DefaultSetupTypeIndex <> -1 then begin
      TypeEntry := PSetupTypeEntry(Entries[seType][DefaultSetupTypeIndex]);
      if toIsCustom in TypeEntry.Options then begin
        //the previous setup type is a custom type: first select the default components
        //for the default type (usually the full type). needed for new components.
        SelectComponentsFromType(PSetupTypeEntry(Entries[seType][0]).Name, False);
        //then select/deselect the custom type's fixed components
        SelectComponentsFromType(TypeEntry.Name, True);
        //now restore the customization
        SelectComponents(PrevSelectedComponents, PrevDeselectedComponents, True);
      end else begin
        //this is not a custom type, so just select components based on the previous type
        SelectComponentsFromType(TypeEntry.Name, False);
      end;
    end else if Entries[seType].Count > 0 then begin
      TypeEntry := PSetupTypeEntry(Entries[seType][0]);
      SelectComponentsFromType(TypeEntry.Name, False);
    end;
  end;

  UpdateComponentSizes;
  CalcCurrentComponentsSpace;

  //Show or hide the components list based on the selected type
  if HasCustomType then begin
    TypeEntry := PSetupTypeEntry(Entries[seType][TypesCombo.ItemIndex]);
    if (toIsCustom in TypeEntry.Options) or (shAlwaysShowComponentsList in SetupHeader.Options) then
      ComponentsList.Visible := True
    else
      ComponentsList.Visible := False;
  end else
    ComponentsList.Visible := False;
  ComponentsDiskSpaceLabel.Visible := ComponentsList.Visible;

  //Store the initial setup type and components (only necessary if customizable)
  if HasCustomType then begin
    InitialSetupTypeIndex := TypesCombo.ItemIndex;
    GetSelectedComponents(InitialSelectedComponents, False, False);
  end;

  { Assign default group name }
  ExpandedDefaultGroupName := ExpandConst(SetupHeader.DefaultGroupName);
  if (InitProgramGroup <> '') and not DisableProgramGroupPage then
    { ^ InitProgramGroup currently isn't supported for installations with
      DisableProgramGroupPage set. If the wizard page isn't displayed, it
      doesn't get a chance to validate the program group name specified. }
    P := ExpandConstIfPrefixed(InitProgramGroup)
  else begin
    if (PrevGroup = '') or (PrevGroup = '(Default)') then
      P := ExpandedDefaultGroupName
    else
      P := PrevGroup;
  end;
  GroupEdit.Text := P;

  if shAllowNoIcons in SetupHeader.Options then begin
    if InitNoIcons or PrevNoIcons then
      NoIconsCheck.Checked := True;
    NoIconsCheck.Visible := True;
  end
  else
    NoIconsCheck.Visible := False;
end;

procedure TWizardForm.AnchorOuterPages;

  procedure AnchorOuterPage(const Page: TNewNotebookPage;
    const BitmapImage: TBitmapImage);
  var
    ExpectedAnchors: TAnchors;
    Ctl: TControl;
    I, NewLeft, NewWidth: Integer;
  begin
    { BitmapImage's size is already corrected by the Anchors property but this
      doesn't keep the aspect ratio. Calculate and set new width to restore the
      aspect ratio and update all the other controls in the page for this. Don't
      do this if [Code] made any change to BitmapImage's Visible, Align or Anchors
      signalling that it wants a custom layout. }
    if ControlsFlipped then
      ExpectedAnchors := [akTop, akRight, akBottom]
    else
      ExpectedAnchors := [akLeft, akTop, akBottom];
    if BitmapImage.Visible and (BitmapImage.Align = alNone) and (BitmapImage.Anchors = ExpectedAnchors) then begin
      if BaseUnitX = 0 then
        InternalError('AnchorOuterPage: BaseUnitX = 0');
      NewWidth := MulDiv(BitmapImage.Height, 164, 314); //164x314 is the original bitmapimage size
      if ControlsFlipped then
        BitmapImage.Left := Page.ClientWidth - NewWidth;
      BitmapImage.Width := NewWidth;
      for I := 0 to Page.ControlCount-1 do begin
        Ctl := Page.Controls[I];
        if Ctl <> BitmapImage then begin
          NewLeft := BitmapImage.Width + ScalePixelsX(12); //12 is original space between bitmapimage and controls
          Ctl.Width := Page.ClientWidth - ScalePixelsX(20) - NewLeft; //20 is original space between controls and right border
          if not ControlsFlipped then
            Ctl.Left := NewLeft;
        end;
      end;
    end;
  end;

begin
  AnchorOuterPage(WelcomePage, WizardBitmapImage);
  AnchorOuterPage(FinishedPage, WizardBitmapImage2);
end;

procedure TWizardForm.FormResize(Sender: TObject);
begin
  if EnableAnchorOuterPagesOnResize then
    AnchorOuterPages;
  if EnableAdjustReadyLabelHeightOnResize then
    IncTopDecHeight(ReadyMemo, AdjustLabelHeight(ReadyLabel));
end;

procedure TWizardForm.FlipSizeAndCenterIfNeeded(const ACenterInsideControl: Boolean;
  const CenterInsideControlCtl: TWinControl; const CenterInsideControlInsideClientArea: Boolean);
begin
  if ShouldSizeX or ShouldSizeY then
    EnableAnchorOuterPagesOnResize := True;
  inherited;
end;

destructor TWizardForm.Destroy;
begin
  FreeAndNil(PrevDeselectedComponents);
  FreeAndNil(PrevSelectedTasks);
  FreeAndNil(PrevDeselectedTasks);
  FreeAndNil(PrevSelectedComponents);
  FreeAndNil(InitialSelectedComponents);
  FreeAndNil(FPageList);
  inherited;
end;

function TWizardForm.PageIndexFromID(const ID: Integer): Integer;
{ Given a page ID, returns the index of the page in FPageList. An exception is
  raised if a page with the specified ID is not found. }
var
  I: Integer;
begin
  for I := 0 to FPageList.Count-1 do begin
    if TWizardPage(FPageList[I]).ID = ID then begin
      Result := I;
      Exit;
    end;
  end;
  InternalError(Format('Could not find page with ID %d', [ID]));
  Result := -1;  { avoid compiler warning }
end;

function TWizardForm.PageFromID(const ID: Integer): TWizardPage;
begin
  Result := FPageList[PageIndexFromID(ID)];
end;

procedure TWizardForm.RegisterExistingPage(const ID: Integer;
  const AOuterNotebookPage, AInnerNotebookPage: TNewNotebookPage;
  const ACaption, ADescription: String);
var
  P: TWizardPage;
begin
  FPageList.Expand;
  P := TWizardPage.Create(Self);
  P.FID := ID;
  P.FOuterNotebookPage := AOuterNotebookPage;
  P.FInnerNotebookPage := AInnerNotebookPage;
  P.Caption := ACaption;
  P.Description := ADescription;
  FPageList.Add(P);
end;

procedure TWizardForm.AddPage(const APage: TWizardPage; const AfterID: Integer);
{ Adds a new wizard page entry in FPageList, and an associated page in
  InnerNotebook. AfterID specifies where the page should be inserted, or -1
  which inserts the page at the end. }
var
  InsertIndex: Integer;
  NotebookPage: TNewNotebookPage;
begin
  if AfterID <> -1 then
    InsertIndex := PageIndexFromID(AfterID) + 1
  else
    InsertIndex := FPageList.Count;
  FPageList.Expand;

  Inc(FNextPageID);
  if FNextPageID = 1 then
    FNextPageID := 100;

  NotebookPage := TNewNotebookPage.Create(APage);
  NotebookPage.Notebook := InnerNotebook;
  NotebookPage.HandleNeeded; { See TWizardForm.Create comment }
  APage.FID := FNextPageID;
  APage.FOuterNotebookPage := InnerPage;
  APage.FInnerNotebookPage := NotebookPage;

  FPageList.Insert(InsertIndex, APage);
end;

{ Also see GetPreviousData in Main.pas }
procedure TWizardForm.FindPreviousData;
var
  H: HKEY;
  S, ExpandedAppId: String;
begin
  ExpandedAppId := ExpandConst(SetupHeader.AppId);
  if ExpandedAppId <> '' then begin
    if RegOpenKeyExView(InstallDefaultRegView, InstallModeRootKey,
       PChar(GetUninstallRegSubkeyName(GetUninstallRegKeyBaseName(ExpandedAppId))),
       0, KEY_QUERY_VALUE, H) = ERROR_SUCCESS then begin
      try
        { do not localize or change the following strings }
        if shUsePreviousAppDir in SetupHeader.Options then
          RegQueryStringValue(H, 'Inno Setup: App Path', FPrevAppDir);
        if shUsePreviousGroup in SetupHeader.Options then begin
          RegQueryStringValue(H, 'Inno Setup: Icon Group', PrevGroup);
          if RegValueExists(H, 'Inno Setup: No Icons') then
            PrevNoIcons := True;
        end;
        if shUsePreviousSetupType in SetupHeader.Options then begin
          RegQueryStringValue(H, 'Inno Setup: Setup Type', PrevSetupType);
          if RegQueryStringValue(H, 'Inno Setup: Selected Components', S) then
            SetStringsFromCommaString(PrevSelectedComponents, S);
          if RegQueryStringValue(H, 'Inno Setup: Deselected Components', S) then
            SetStringsFromCommaString(PrevDeselectedComponents, S);
        end;
        if shUsePreviousTasks in SetupHeader.Options then begin
          if RegQueryStringValue(H, 'Inno Setup: Selected Tasks', S) then
            SetStringsFromCommaString(PrevSelectedTasks, S);
          if RegQueryStringValue(H, 'Inno Setup: Deselected Tasks', S) then
            SetStringsFromCommaString(PrevDeselectedTasks, S);
        end;
        if shUsePreviousUserInfo in SetupHeader.Options then begin
          RegQueryStringValue(H, 'Inno Setup: User Info: Name', PrevUserInfoName);
          RegQueryStringValue(H, 'Inno Setup: User Info: Organization', PrevUserInfoOrg);
          RegQueryStringValue(H, 'Inno Setup: User Info: Serial', PrevUserInfoSerial);
        end;
      finally
        RegCloseKey(H);
      end;
    end;
  end;
end;

procedure TWizardForm.ChangeReadyLabel(const S: String);
begin
  ReadyLabel.Caption := S;
  IncTopDecHeight(ReadyMemo, AdjustLabelHeight(ReadyLabel));
  EnableAdjustReadyLabelHeightOnResize := True;
end;

procedure TWizardForm.ChangeFinishedLabel(const S: String);
var
  Y: Integer;
begin
  FinishedLabel.Caption := S + SNewLine;
  AdjustLabelHeight(FinishedLabel);
  Y := FinishedLabel.Top + FinishedLabel.Height;
  IncTopDecHeight(RunList, Y-YesRadio.Top);
  YesRadio.Top := Y;
  NoRadio.Top := Y + ScalePixelsY(22);
end;

procedure TWizardForm.UpdateRunList(const SelectedComponents, SelectedTasks: TStringList);
var
  RunEntry: PSetupRunEntry;
  Caption: String;
  I: Integer;
begin
  RunList.Items.Clear();

  for I := 0 to Entries[seRun].Count-1 do begin
    RunEntry := PSetupRunEntry(Entries[seRun][I]);
    if (roPostInstall in RunEntry.Options) and ShouldProcessRunEntry(SelectedComponents, SelectedTasks, RunEntry) then begin
      try
        if RunEntry.Description <> '' then
          Caption := ExpandConst(RunEntry.Description)
        else if not(roShellExec in RunEntry.Options) then
          Caption := FmtSetupMessage1(msgRunEntryExec, PathExtractName(ExpandConst(RunEntry.Name)))
        else
          Caption := FmtSetupMessage1(msgRunEntryShellExec, PathExtractName(ExpandConst(RunEntry.Name)));
      except
        { An exception here killing the entire Setup is not too desirable,
          as post-install [Run] entries are normally unimportant. Just
          display the message and move on. }
        Application.HandleException(Self);
        Caption := '[Error]';
      end;
      RunList.AddCheckBox(Caption, '', 0, not(roUnchecked in RunEntry.Options), True, True, True, TObject(I));
    end;
  end;
end;

procedure TWizardForm.CreateTaskButtons(const SelectedComponents: TStringList);
var
  SaveSelectedTasks, SaveDeselectedTasks: TStringList;
  LastShownTaskEntry, TaskEntry: PSetupTaskEntry;
  NextAllowedLevel, I: Integer;
  Description, GroupDescription: String;
  LastGroupDescription: String;
begin
  SaveDeselectedTasks := nil;
  SaveSelectedTasks := TStringList.Create;
  try
    SaveDeselectedTasks := TStringList.Create;
    { Save state of current items (if any) }
    GetTasks(SaveSelectedTasks, SaveDeselectedTasks);

    TasksList.Items.Clear();
    LastGroupDescription := '';

    { Create the task items with their default checked states }
    NextAllowedLevel := 0;
    LastShownTaskEntry := nil;
    for I := 0 to Entries[seTask].Count-1 do begin
      TaskEntry := PSetupTaskEntry(Entries[seTask][I]);
      if (TaskEntry.Level <= NextAllowedLevel) and
         (InstallOnThisVersion(TaskEntry.MinVersion, TaskEntry.OnlyBelowVersion) = irInstall) and
         ShouldProcessEntry(SelectedComponents, nil, TaskEntry.Components, '', TaskEntry.Languages, TaskEntry.Check) then begin
        Description := ExpandConst(TaskEntry.Description);
        GroupDescription := ExpandConst(TaskEntry.GroupDescription);

        { See if we should add a group label }
        if (TaskEntry.Level = 0) and (GroupDescription <> LastGroupDescription) then begin
          TasksList.AddGroup(GroupDescription, '', 0, nil);
          LastGroupDescription := GroupDescription;
        end;

        { Create a check box or radio button }
        if toExclusive in TaskEntry.Options then
          TasksList.AddRadioButton(Description, '', TaskEntry.Level,
            not InitDeselectAllTasks and not(toUnchecked in TaskEntry.Options),
            True, TObject(TaskEntry))
        else
          TasksList.AddCheckBox(Description, '', TaskEntry.Level,
            not InitDeselectAllTasks and not(toUnchecked in TaskEntry.Options),
            True, TaskEntry.Used, not(toDontInheritCheck in TaskEntry.Options),
            TObject(TaskEntry));

        NextAllowedLevel := TaskEntry.Level + 1;
        LastShownTaskEntry := TaskEntry;
      end
      else begin
        { Not showing }
        if Assigned(LastShownTaskEntry) and
           (TaskEntry.Level = LastShownTaskEntry.Level) and
           (CompareText(TaskEntry.Name, LastShownTaskEntry.Name) = 0) then begin
          { It's a duplicate of the last shown item. Leave NextAllowedLevel
            alone, so that any child items that follow can attach to the last
            shown item. }
        end
        else begin
          { Not a duplicate of the last shown item, so the next item must be
            at the same level or less }
          if NextAllowedLevel > TaskEntry.Level then
            NextAllowedLevel := TaskEntry.Level;
          { Clear LastShownTaskEntry so that no subsequent item can be
            considered a duplicate of it. Needed in this case:
              foo         (shown)
              foo\childA  (not shown)
              foo         (not shown)
              foo\childB
            "foo\childB" should be hidden, not made a child of "foo" #1. }
          LastShownTaskEntry := nil;
        end;
      end;
    end;

    { Restore the previous checked state of the items we just created }
    if not InitDeselectAllTasks then begin
      for I := 0 to TasksList.Items.Count-1 do begin
        TaskEntry := PSetupTaskEntry(TasksList.ItemObject[I]);
        if TaskEntry <> nil then begin
          if ListContains(PrevSelectedTasks, TaskEntry.Name) then
            TasksList.Checked[I] := not(toCheckedOnce in TaskEntry.Options)
          else if ListContains(PrevDeselectedTasks, TaskEntry.Name) then
            TasksList.Checked[I] := False;
        end;
      end;
    end;

    { Override previous state with tasks specified on the command line }
    if InitTasks.Count > 0 then begin
      for I := 0 to TasksList.Items.Count-1 do begin
        TaskEntry := PSetupTaskEntry(TasksList.ItemObject[I]);
        if TaskEntry <> nil then begin
          if ListContains(InitTasks, '*' + TaskEntry.Name) then
            TasksList.CheckItem(I, coCheckWithChildren)
          else if ListContains(InitTasks, TaskEntry.Name) then
            TasksList.Checked[I] := True
          else if ListContains(InitTasks, '!' + TaskEntry.Name) then
            TasksList.Checked[I] := False;
        end;
      end;
    end;

    { Finally, restore any saved state from when the page was last shown }
    SelectTasks(SaveSelectedTasks, SaveDeselectedTasks);
  finally
    SaveDeselectedTasks.Free;
    SaveSelectedTasks.Free;
  end;
end;

function TWizardForm.GetSetupType(): PSetupTypeEntry;
var
  Index: Integer;
begin
  Index := TypesCombo.ItemIndex;
  if Index <> -1 then
    Result := PSetupTypeEntry(TypesCombo.Items.Objects[TypesCombo.ItemIndex])
  else
    Result := nil;
end;

procedure TWizardForm.SelectComponents(const SelectComponents, DeselectComponents: TStringList; const KeepFixedComponents: Boolean);
var
  I: Integer;
  ComponentEntry: PSetupComponentEntry;
begin
  for I := 0 to Entries[seComponent].Count-1 do begin
    ComponentEntry := PSetupComponentEntry(Entries[seComponent][I]);

    if not (KeepFixedComponents and (coFixed in ComponentEntry.Options)) then begin
      if SelectComponents <> nil then begin
        if ListContains(SelectComponents, '*' + ComponentEntry.Name) then begin
          ComponentsList.CheckItem(I, coCheckWithChildren);
          Continue;
        end;
        if ListContains(SelectComponents, ComponentEntry.Name) then begin
          ComponentsList.Checked[I] := True;
          Continue;
        end;
        if ListContains(SelectComponents, '!' + ComponentEntry.Name) then begin
          ComponentsList.Checked[I] := False;
          Continue;
        end;
      end;

      if DeselectComponents <> nil then begin
        if ListContains(DeselectComponents, ComponentEntry.Name) then
          ComponentsList.Checked[I] := False;
      end;
    end;
  end;
end;

procedure TWizardForm.SelectComponents(const ASelectComponents: TStringList);
begin
  SelectComponents(ASelectComponents, nil, False);
  UpdateComponentSizes;
  CalcCurrentComponentsSpace;
end;

procedure TWizardForm.SelectTasks(const SelectTasks, DeselectTasks: TStringList);
var
  I: Integer;
  TaskEntry: PSetupTaskEntry;
begin
  for I := 0 to TasksList.Items.Count-1 do begin
    TaskEntry := PSetupTaskEntry(TasksList.ItemObject[I]);
    if TaskEntry <> nil then begin
      if SelectTasks <> nil then begin
        if ListContains(SelectTasks, TaskEntry.Name) then begin
          TasksList.Checked[I] := True;
          Continue;
        end;
        if ListContains(SelectTasks, '!' + TaskEntry.Name) then begin
          TasksList.Checked[I] := False;
          Continue;
        end;
      end;
      
      if DeselectTasks <> nil then begin
        if ListContains(DeselectTasks, TaskEntry.Name) then
          TasksList.Checked[I] := False;
      end;
    end;
  end;
end;

procedure TWizardForm.SelectTasks(const ASelectTasks: TStringList);
begin
  SelectTasks(ASelectTasks, nil);
end;

procedure TWizardForm.SelectComponentsFromType(const TypeName: String; const OnlySelectFixedComponents: Boolean);
var
  ComponentTypes: TStringList;
  ComponentEntry: PSetupComponentEntry;
  I: Integer;
begin
  ComponentTypes := TStringList.Create();
  for I := 0 to Entries[seComponent].Count-1 do begin
    ComponentEntry := PSetupComponentEntry(Entries[seComponent][I]);
    if not OnlySelectFixedComponents or (coFixed in ComponentEntry.Options) then begin
      SetStringsFromCommaString(ComponentTypes, ComponentEntry.Types);
      ComponentsList.Checked[I] := ListContains(ComponentTypes, TypeName);
    end;
  end;
  ComponentTypes.Free();
end;

procedure TWizardForm.UpdateSelectTasksPage;
var
  SelectedComponents: TStringList;
begin
  SelectedComponents := TStringList.Create();
  try
    GetSelectedComponents(SelectedComponents, False, False);
    CreateTaskButtons(SelectedComponents);
  finally
    SelectedComponents.Free();
  end;
end;

procedure TWizardForm.GetSelectedComponents(Components: TStringList; const Descriptions, IndentDescriptions: Boolean);

  function GetString(ComponentEntry: PSetupComponentEntry; Descriptions: Boolean): String;
  begin
    if Descriptions then begin
      Result := ExpandConst(ComponentEntry.Description);
      if IndentDescriptions then
        Result := StringOfChar(' ', 3*ComponentEntry.Level) + Result;
    end else
      Result := ComponentEntry.Name;
  end;

var
  ComponentEntry: PSetupComponentEntry;
  I: Integer;
begin
  Components.Clear();
  for I := 0 to ComponentsList.Items.Count-1 do begin
    if ComponentsList.Checked[I] then begin
      ComponentEntry := PSetupComponentEntry(ComponentsList.ItemObject[I]);
      Components.Add(GetString(ComponentEntry, Descriptions));
    end;
  end;
end;

procedure TWizardForm.GetSelectedTasks(Tasks: TStringList; const Descriptions, IndentDescriptions, GroupDescriptions: Boolean);

  function GetString(TaskEntry: PSetupTaskEntry; Descriptions, IndentDescriptions: Boolean; IndentLevel: Integer): String;
  begin
    if Descriptions then begin
      Result := RemoveAccelChar(ExpandConst(TaskEntry.Description));
      if IndentDescriptions then
        Result := StringOfChar(' ', 3*IndentLevel) + Result;
    end else
      Result := TaskEntry.Name;
  end;

var
  TaskEntry: PSetupTaskEntry;
  I, IndentLevel: Integer;
  GroupDescription, LastGroupDescription: String;
begin
  Tasks.Clear();
  if GroupDescriptions then
    LastGroupDescription := '';

  for I := 0 to TasksList.Items.Count-1 do begin
    if TasksList.Checked[I] and (TasksList.ItemObject[I] <> nil) then begin
      TaskEntry := PSetupTaskEntry(TasksList.ItemObject[I]);

      if GroupDescriptions then begin
        GroupDescription := ExpandConst(TaskEntry.GroupDescription);

        if (TaskEntry.Level = 0) and (GroupDescription <> LastGroupDescription) then begin
          if GroupDescription <> '' then
            Tasks.Add(RemoveAccelChar(GroupDescription));
          LastGroupDescription := GroupDescription;
        end;

        IndentLevel := TaskEntry.Level;
        if LastGroupDescription <> '' then
          Inc(IndentLevel);
      end else
        IndentLevel := TaskEntry.Level;

      Tasks.Add(GetString(TaskEntry, Descriptions, IndentDescriptions, IndentLevel));
    end;
  end;
end;

procedure TWizardForm.GetComponents(SelectedComponents, DeselectedComponents: TStringList);
{ Gets names of components that are currently selected and deselected }
var
  I: Integer;
  ComponentEntry: PSetupComponentEntry;
begin
  SelectedComponents.Clear;
  if DeselectedComponents <> nil then
    DeselectedComponents.Clear;
  for I := 0 to ComponentsList.Items.Count-1 do begin
    ComponentEntry := PSetupComponentEntry(ComponentsList.ItemObject[I]);
    if ComponentsList.Checked[I] then
      SelectedComponents.Add(ComponentEntry.Name)
    else if DeselectedComponents <> nil then
      DeselectedComponents.Add(ComponentEntry.Name);
  end;
end;

procedure TWizardForm.GetTasks(SelectedTasks, DeselectedTasks: TStringList);
{ Gets names of tasks that are currently selected and deselected }
var
  I: Integer;
  TaskEntry: PSetupTaskEntry;
begin
  SelectedTasks.Clear;
  if DeselectedTasks <> nil then
    DeselectedTasks.Clear;
  for I := 0 to TasksList.Items.Count-1 do begin
    TaskEntry := PSetupTaskEntry(TasksList.ItemObject[I]);
    if TaskEntry <> nil then begin
      if TasksList.Checked[I] then
        SelectedTasks.Add(TaskEntry.Name)
      else if DeselectedTasks <> nil then
        DeselectedTasks.Add(TaskEntry.Name);
    end;
  end;
end;

function TWizardForm.PrepareToInstall(const WizardComponents, WizardTasks: TStringList): String;
var
  CodeNeedsRestart: Boolean;
  Y: Integer;
begin
  Result := '';
  PrepareToInstallNeedsRestart := False;
  PreparingErrorBitmapImage.Visible := False;
  PreparingLabel.Visible := False;
  PreparingYesRadio.Visible := False;
  PreparingNoRadio.Visible := False;
  PreparingMemo.Visible := False;
  if not PreviousInstallCompleted(WizardComponents, WizardTasks) then begin
    Result := ExpandSetupMessage(msgPreviousInstallNotCompleted);
    PrepareToInstallNeedsRestart := True;
  end else if (CodeRunner <> nil) and CodeRunner.FunctionExists('PrepareToInstall', True) then begin
    SetCurPage(wpPreparing);
    BackButton.Visible := False;
    NextButton.Visible := False;
    CancelButton.Enabled := False;
    if InstallMode = imSilent then
      WizardForm.Visible := True;
    WizardForm.Update;
    try
      DownloadTemporaryFileOrExtract7ZipArchiveProcessMessages := True;
      CodeNeedsRestart := False;
      Result := CodeRunner.RunStringFunctions('PrepareToInstall', [@CodeNeedsRestart], bcNonEmpty, True, '');
      PrepareToInstallNeedsRestart := (Result <> '') and CodeNeedsRestart;
    finally
      DownloadTemporaryFileOrExtract7ZipArchiveProcessMessages := False;
      UpdateCurPageButtonState;
    end;
    if WindowState <> wsMinimized then  { VCL bug workaround }
      Application.BringToFront;
  end;
  if Result <> '' then begin
    if PrepareToInstallNeedsRestart then
      PreparingLabel.Caption := Result +
        SNewLine + SNewLine + SNewLine + ExpandSetupMessage(msgPrepareToInstallNeedsRestart) + SNewLine
    else
      PreparingLabel.Caption := Result +
        SNewLine + SNewLine + SNewLine + SetupMessages[msgCannotContinue];
    AdjustLabelHeight(PreparingLabel);
    PreparingErrorBitmapImage.Visible := True;
    PreparingLabel.Visible := True;
    if PrepareToInstallNeedsRestart then begin
      Y := PreparingLabel.Top + PreparingLabel.Height;
      PreparingYesRadio.Top := Y;
      PreparingYesRadio.Anchors := [akLeft, akTop, akRight];
      PreparingYesRadio.Caption := SetupMessages[msgYesRadio];
      PreparingYesRadio.Visible := True;
      PreparingNoRadio.Top := Y + ScalePixelsY(22);
      PreparingNoRadio.Anchors := [akLeft, akTop, akRight];
      PreparingNoRadio.Caption := SetupMessages[msgNoRadio];
      PreparingNoRadio.Visible := True;
    end;
  end;
end;

function TWizardForm.QueryRestartManager(const WizardComponents, WizardTasks: TStringList): String;

  procedure CheckAndAddRebootReasonToString(var S: String; const RebootReasons, RebootReason: Integer; const RebootReasonString: String);
  begin
    if (RebootReasons and RebootReason) <> 0 then begin
      if S <> '' then
        S := S + '+';
      S := S + RebootReasonString;
    end;
  end;

  function RebootReasonsToString(const RebootReasons: Integer): String;
  var
    UnknownReasons: Integer;
  begin
    Result := '';
    if RebootReasons <> RmRebootReasonNone then begin
      CheckAndAddRebootReasonToString(Result, RebootReasons, RmRebootReasonPermissionDenied, 'Permission Denied');
      CheckAndAddRebootReasonToString(Result, RebootReasons, RmRebootReasonSessionMismatch, 'Session Mismatch');
      CheckAndAddRebootReasonToString(Result, RebootReasons, RmRebootReasonCriticalProcess, 'Critical Process');
      CheckAndAddRebootReasonToString(Result, RebootReasons, RmRebootReasonCriticalService, 'Critical Service');
      CheckAndAddRebootReasonToString(Result, RebootReasons, RmRebootReasonDetectedSelf, 'Detected Self');
      UnknownReasons := RebootReasons and not (RmRebootReasonNone or RmRebootReasonPermissionDenied or
                                               RmRebootReasonSessionMismatch or RmRebootReasonCriticalProcess or
                                               RmRebootReasonCriticalService or RmRebootReasonDetectedSelf);
      CheckAndAddRebootReasonToString(Result, RebootReasons, UnknownReasons, Format('Unknown Reason(s) %d', [UnknownReasons]));
      Result := ': ' + Result;
    end;
    Result := IntToStr(RebootReasons) + Result;
  end;

type
  TArrayOfProcessInfo = array[0..(MaxInt div SizeOf(RM_PROCESS_INFO))-1] of RM_PROCESS_INFO;
  PArrayOfProcessInfo = ^TArrayOfProcessInfo;
var
  Y, I: Integer;
  ProcessInfosCount, ProcessInfosCountNeeded, RebootReasons: Integer;
  ProcessInfos: PArrayofProcessInfo;
  AppName: String;
begin
  { Clear existing registered resources if we get here a second time (user clicked Back after first time). There
    doesn't seem to be function to do this directly, so restart the session instead. }
  if RmRegisteredFilesCount <> 0 then begin
    RmEndSession(RmSessionHandle);
    if RmStartSession(@RmSessionHandle, 0, RmSessionKey) <> ERROR_SUCCESS then
      RmSessionStarted := False;
  end;

  if RmSessionStarted then
    RegisterResourcesWithRestartManager(WizardComponents, WizardTasks); { This will update RmSessionStarted and RmRegisteredFilesCount }

  if RmSessionStarted then begin
    LogFmt('Found %d files to register with RestartManager.', [RmRegisteredFilesCount]);
    if RmRegisteredFilesCount > 0 then begin
      ProcessInfosCount := 0;
      ProcessInfosCountNeeded := 5; { Start with 5 to hopefully avoid a realloc }
      ProcessInfos := nil;
      try
        Log('Calling RestartManager''s RmGetList.');
        while ProcessInfosCount < ProcessInfosCountNeeded do begin
          if ProcessInfos <> nil then
            FreeMem(ProcessInfos);
          GetMem(ProcessInfos, ProcessInfosCountNeeded * SizeOf(ProcessInfos[0]));
          ProcessInfosCount := ProcessInfosCountNeeded;

          if not RmGetList(RmSessionHandle, @ProcessInfosCountNeeded, @ProcessInfosCount, ProcessInfos, @RebootReasons) in [ERROR_SUCCESS, ERROR_MORE_DATA] then begin
            RmEndSession(RmSessionHandle);
            RmSessionStarted := False;
            Break;
          end;
        end;

        if RmSessionStarted then begin
          Log('RmGetList finished successfully.');
          if ProcessInfosCount > 0 then begin
            for I := 0 to ProcessInfosCount-1 do begin
              AppName := WideCharToString(ProcessInfos[I].strAppName);
              LogFmt('RestartManager found an application using one of our files: %s', [AppName]);
              if RebootReasons = RmRebootReasonNone then begin
                if Result <> '' then
                  Result := Result + #13#10;
                Result := Result + AppName;
              end;
            end;
            LogFmt('Can use RestartManager to avoid reboot? %s (%s)', [SYesNo[RebootReasons = RmRebootReasonNone], RebootReasonsToString(RebootReasons)]);
          end else
            Log('RestartManager found no applications using one of our files.');
        end else
          Log('RmGetList failed.');
      finally
        if ProcessInfos <> nil then
          FreeMem(ProcessInfos);
      end;
    end;
  end;

  if Result <> '' then begin
    if InitRestartApplications or
       ((shRestartApplications in SetupHeader.Options) and not InitNoRestartApplications) then
      PreparingLabel.Caption := SetupMessages[msgApplicationsFound2]
    else
      PreparingLabel.Caption := SetupMessages[msgApplicationsFound];
    Y := PreparingLabel.Top + PreparingLabel.Height + ScalePixelsY(12);
    PreparingMemo.Top := Y;
    IncTopDecHeight(PreparingMemo, AdjustLabelHeight(PreparingLabel));
    AdjustLabelHeight(PreparingLabel);
    PreparingErrorBitmapImage.Visible := True;
    PreparingLabel.Visible := True;
    PreparingMemo.Text := Result;
    PreparingMemo.Visible := True;
    Y := PreparingMemo.Top + PreparingMemo.Height + ScalePixelsY(12);
    PreparingYesRadio.Top := Y;
    PreparingYesRadio.Anchors := [akLeft, akRight, akBottom];
    PreparingYesRadio.Caption := SetupMessages[msgCloseApplications];
    PreparingYesRadio.Visible := True;
    PreparingNoRadio.Top := Y + ScalePixelsY(22);
    PreparingNoRadio.Anchors := [akLeft, akRight, akBottom];
    PreparingNoRadio.Caption := SetupMessages[msgDontCloseApplications];
    PreparingNoRadio.Visible := True;
  end;
end;

procedure TWizardForm.UpdatePage(const PageID: Integer);

  procedure ReadyMemoAppend(const Lines: String);
  begin
    if Lines <> '' then begin
      if ReadyMemo.Lines.Count > 0 then
        ReadyMemo.Lines.Append('');
      ReadyMemo.Lines.Append(Lines);
    end;
  end;

  procedure UpdateReadyPage;
  const
    Space = '      ';
  var
    TypeEntry: PSetupTypeEntry;
    SelectedComponents, SelectedTasks: TStringList;
    S, MemoUserInfoInfo, MemoDirInfo, MemoGroupInfo, MemoTypeInfo, MemoComponentsInfo, MemoTasksInfo: String;
    I: Integer;
  begin
    ReadyMemo.Visible := False;
    if not (shDisableReadyMemo in SetupHeader.Options) then begin
      ReadyMemo.Lines.Clear();

      if shUserInfoPage in SetupHeader.Options then begin
        MemoUserInfoInfo := SetupMessages[msgReadyMemoUserInfo];
        MemoUserInfoInfo := MemoUserInfoInfo+SNewLine+Space+UserInfoNameEdit.Text;
        if UserInfoOrgEdit.Text <> '' then
          MemoUserInfoInfo := MemoUserInfoInfo+SNewLine+Space+UserInfoOrgEdit.Text;
      end;

      if (shAlwaysShowDirOnReadyPage in SetupHeader.Options) or
         (not DisableDirPage and
          (shCreateAppDir in SetupHeader.Options)) then begin
        MemoDirInfo := SetupMessages[msgReadyMemoDir];
        MemoDirInfo := MemoDirInfo+SNewLine+Space+DirEdit.Text;
      end else
        MemoDirInfo := '';

      if HasComponents then begin
        TypeEntry := GetSetupType();
        if TypeEntry <> nil then begin
          MemoTypeInfo := SetupMessages[msgReadyMemoType];
          MemoTypeInfo := MemoTypeInfo+SNewLine+Space+ExpandConst(TypeEntry.Description);
        end else
          MemoTypeInfo := '';  { can get here if all types failed their Check }

        SelectedComponents := TStringList.Create();
        GetSelectedComponents(SelectedComponents, True, True);
        if SelectedComponents.Count > 0 then begin
          MemoComponentsInfo := SetupMessages[msgReadyMemoComponents];
          for I := 0 to SelectedComponents.Count-1 do
            MemoComponentsInfo := MemoComponentsInfo+SNewLine+Space+SelectedComponents[I];
        end else
          MemoComponentsInfo := '';
        SelectedComponents.Free();
      end;

      if HasIcons and not NoIconsCheck.Checked and
         ((shAlwaysShowGroupOnReadyPage in SetupHeader.Options) or
          not DisableProgramGroupPage) then begin
        MemoGroupInfo := SetupMessages[msgReadyMemoGroup];
        MemoGroupInfo := MemoGroupInfo+SNewLine+Space+GroupEdit.Text;
      end else
        MemoGroupInfo := '';

      SelectedTasks := TStringList.Create();
      GetSelectedTasks(SelectedTasks, True, True, True);
      if SelectedTasks.Count > 0 then begin
        MemoTasksInfo := SetupMessages[msgReadyMemoTasks];
        for I := 0 to SelectedTasks.Count-1 do
          MemoTasksInfo := MemoTasksInfo+SNewLine+Space+SelectedTasks[I];
      end else
        MemoTasksInfo := '';
      SelectedTasks.Free();

      if (CodeRunner <> nil) and CodeRunner.FunctionExists('UpdateReadyMemo', True) then begin
        try
          ReadyMemo.Lines.Text := CodeRunner.RunStringFunctions('UpdateReadyMemo',
            [Space, SNewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo,
             MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo], bcNonEmpty, True, '');
        except
          Application.HandleException(Self);
        end;
      end else begin
        ReadyMemoAppend(MemoUserInfoInfo);
        ReadyMemoAppend(MemoDirInfo);
        ReadyMemoAppend(MemoTypeInfo);
        ReadyMemoAppend(MemoComponentsInfo);
        ReadyMemoAppend(MemoGroupInfo);
        ReadyMemoAppend(MemoTasksInfo);
      end;

      ReadyMemo.SelStart := 0;
      ReadyMemo.SelLength := 0;
    end;

    if ReadyMemo.Lines.Count > 0 then begin
      S := SetupMessages[msgReadyLabel2a];
      ChangeReadyLabel(S);
      ReadyMemo.Visible := True;
    end else begin
      S := SetupMessages[msgReadyLabel2b];
      ChangeReadyLabel(S);
    end;
  end;

begin
  case PageID of
    wpSelectTasks: UpdateSelectTasksPage;
    wpReady: UpdateReadyPage;
  end;
end;

procedure TWizardForm.AdjustFocus;
var
  NewActiveControl: TWinControl;
begin
  if CurPageID = wpReady then
    NewActiveControl := NextButton
  else if (CurPageID = wpPreparing) and (PrepareToInstallFailureMessage <> '') and not PrepareToInstallNeedsRestart then
    NewActiveControl := CancelButton
  else if (CurPageID = wpPreparing) and (PrepareToInstallFailureMessage = '') and PreparingYesRadio.CanFocus then
    NewActiveControl := PreparingYesRadio
  else
    NewActiveControl := FindNextControl(nil, True, True, False);
  if (NewActiveControl = BackButton) and NextButton.CanFocus then
    NewActiveControl := NextButton;
  ActiveControl := NewActiveControl;
end;

function TWizardForm.GetPreviousPageID: Integer;
{ Finds ID of previous page (not counting skipped pages), or -1 if there is
  no previous page to return to. }
var
  CurPageIndex, I: Integer;
begin
  CurPageIndex := PageIndexFromID(CurPageID);
  for I := CurPageIndex-1 downto 0 do begin
    Result := TWizardPage(FPageList[I]).ID;
    { Never go back to wpInstalling }
    if Result = wpInstalling then
      Break;
    if not ShouldSkipPage(Result) then
      Exit;
  end;
  Result := -1;
end;

procedure TWizardForm.UpdateCurPageButtonState;
var
  PageIndex: Integer;
  Page: TWizardPage;
  Flags: UINT;
begin
  PageIndex := PageIndexFromID(CurPageID);
  Page := FPageList[PageIndex];

  if not(psNoButtons in Page.Style) then begin
    BackButton.Visible := (CurPageID <> wpInstalling) and (GetPreviousPageID <> -1);
    NextButton.Visible := CurPageID <> wpInstalling;
    case CurPageID of
      wpLicense: NextButton.Enabled := LicenseAcceptedRadio.Checked;
      wpPreparing: NextButton.Enabled := (PrepareToInstallFailureMessage = '') or PrepareToInstallNeedsRestart;
    else
      NextButton.Enabled := True;
    end;
    CancelButton.Visible := (PageIndex <= PageIndexFromID(wpInstalling)) and
      not ((CurPageID = wpPreparing) and PrepareToInstallNeedsRestart);
    CancelButton.Enabled := (CurPageID <> wpInstalling) or
      ((shAllowCancelDuringInstall in SetupHeader.Options) and not InitNoCancel);
  end
  else begin
    BackButton.Visible := False;
    NextButton.Visible := False;
    CancelButton.Visible := False;
  end;
  { Set the enabled state of the close button to match the Cancel button }
  if CancelButton.CanFocus then
    Flags := 0
  else
    Flags := MF_GRAYED;
  EnableMenuItem(GetSystemMenu(Handle, False), SC_CLOSE, MF_BYCOMMAND or Flags);
end;

procedure TWizardForm.SetCurPage(const NewPageID: Integer);
{ Changes which page is currently visible }
var
  Page: TWizardPage;
begin
  Page := PageFromID(NewPageID);
  FCurPageID := NewPageID;

  { Select the page in the notebooks }
  if Assigned(Page.InnerNotebookPage) then
    InnerNotebook.ActivePage := Page.InnerNotebookPage;
  OuterNotebook.ActivePage := Page.OuterNotebookPage;

  { Set the page description }
  Page.SyncCaptionAndDescription;

  BeveledLabel.Visible := (BeveledLabel.Caption <> '') and
    not(CurPageID in [wpWelcome, wpFinished]);

  { Set button visibility and captions }
  UpdateCurPageButtonState;

  BackButton.Caption := SetupMessages[msgButtonBack];
  if CurPageID = wpReady then begin
    NextButton.Caption := SetupMessages[msgButtonInstall];
    CancelButton.Caption := SetupMessages[msgButtonCancel];
  end else if ((CurPageID = wpPreparing) and PrepareToInstallNeedsRestart) or (CurPageID = wpFinished) then begin
    NextButton.Caption := SetupMessages[msgButtonFinish];
    CancelButton.Caption := SetupMessages[msgButtonCancel];
  end else begin
    NextButton.Caption := SetupMessages[msgButtonNext];
    CancelButton.Caption := SetupMessages[msgButtonCancel];
  end;

  { Adjust focus }
  AdjustFocus;

  { If on the wpUserInfo page, check the serial now, after the rest of the
    page is initialized in case the event function happens to display a
    message box or raise an exception }
  if CurPageID = wpUserInfo then begin
    try
      NextButton.Enabled := CheckSerialOk();
    except
      NextButton.Enabled := False;
      Application.HandleException(Self);
    end;
  end;

  try
    PageFromID(CurPageID).Activate;
  except
    Application.HandleException(Self);
  end;

  try
    if CodeRunner <> nil then
      CodeRunner.RunProcedures('CurPageChanged', [CurPageID], False);
  except
    Application.HandleException(Self);
  end;
end;

function TWizardForm.ShouldSkipPage(const PageID: Integer): Boolean;
begin
  if (PageID = wpReady) and not Visible then begin
    Result := False;
    Exit;
  end;

  Result :=
    (psAlwaysSkip in PageFromID(PageID).Style) or
    ((PageID = wpWelcome) and (shDisableWelcomePage in SetupHeader.Options)) or
    ((PageID = wpLicense) and ((ActiveLicenseText = '') or (InstallMode <> imNormal))) or
    ((PageID = wpPassword) and not NeedPassword) or
    ((PageID = wpInfoBefore) and (ActiveInfoBeforeText = '')) or
    ((PageID = wpUserInfo) and not(shUserInfoPage in SetupHeader.Options)) or
    ((PageID = wpSelectDir) and (DisableDirPage or not(shCreateAppDir in SetupHeader.Options))) or
    ((PageID = wpSelectComponents) and not HasComponents) or
    ((PageID = wpSelectProgramGroup) and (DisableProgramGroupPage or not HasIcons)) or
    ((PageID = wpSelectTasks) and (TasksList.Items.Count = 0)) or
    ((PageID = wpReady) and (shDisableReadyPage in SetupHeader.Options)) or
    ((PageID = wpPreparing)) or
    ((PageID = wpInfoAfter) and (ActiveInfoAfterText = '')) or
    ((PageID = wpFinished) and (shDisableFinishedPage in SetupHeader.Options) and not (NeedsRestart and not InitNoRestart));

  if not Result and not (PageID in [wpPreparing]) then begin
    try
      PageFromID(PageID).ShouldSkipPage(Result);
    except
      Application.HandleException(Self);
    end;

    if not Result then begin
      try
        if CodeRunner <> nil then
          Result := CodeRunner.RunBooleanFunctions('ShouldSkipPage', [PageID], bcTrue, False, Result);
      except
        Application.HandleException(Self);
      end;
    end;
  end;
end;

procedure TWizardForm.NextButtonClick(Sender: TObject);

  function CheckPassword: Boolean;
  { Also see MainFunc.HandleInitPassword }
  begin
    Result := False;
    var S := PasswordEdit.Text;

    var Timer: TOneShotTimer;
    Timer.Start(750); { See comment below }

    var CryptKey: TSetupEncryptionKey;
    var SaveCursor := GetCursor;
    SetCursor(LoadCursor(0, IDC_WAIT));
    try
      GenerateEncryptionKey(S, SetupHeader.EncryptionKDFSalt, SetupHeader.EncryptionKDFIterations, CryptKey);
    finally
      SetCursor(SaveCursor);
    end;

    if shPassword in SetupHeader.Options then
      Result := TestPassword(CryptKey);
    if not Result and (CodeRunner <> nil) then
      Result := CodeRunner.RunBooleanFunctions('CheckPassword', [S], bcTrue, False, Result);

    if Result then begin
      NeedPassword := False;
      if shEncryptionUsed in SetupHeader.Options then
        FileExtractor.CryptKey := CryptKey;
      PasswordEdit.Text := '';
    end else begin
      { Ensure a total time of 750 ms when an incorrect password is entered to
        discourage brute-force attempts }
      if Visible then begin
        SaveCursor := GetCursor;
        SetCursor(LoadCursor(0, IDC_WAIT));
        try
          Timer.SleepUntilExpired;
        finally
          SetCursor(SaveCursor);
        end;
      end;
      LoggedMsgBox(SetupMessages[msgIncorrectPassword], '', mbError, MB_OK, True, IDOK);
      if Visible then begin
        PasswordEdit.Text := '';
        PasswordEdit.SetFocus;
      end;
    end;
  end;

  function CheckUserInfoPage: Boolean;
  begin
    UserInfoNameEdit.Text := Trim(UserInfoNameEdit.Text);
    UserInfoOrgEdit.Text := Trim(UserInfoOrgEdit.Text);
    { Note: We don't require a user name to be entered on silent installs,
      since the default value in the registry could be blank (at least this
      was the case for one user). }
    Result := (UserInfoNameEdit.Text <> '') or (InstallMode <> imNormal);
    if Result then begin
      WizardUserInfoName := UserInfoNameEdit.Text;
      WizardUserInfoOrg := UserInfoOrgEdit.Text;
      WizardUserInfoSerial := UserInfoSerialEdit.Text;
    end
    else begin
      LoggedMsgBox(SetupMessages[msgUserInfoNameRequired], '', mbError, MB_OK, True, IDOK);
      if Visible then
        UserInfoNameEdit.SetFocus;
    end;
  end;

  function CheckSelectDirPage: Boolean;
  var
    T: String;
    FreeSpace, TotalSpace: Integer64;
  begin
    Result := False;

    if not ValidateDirEdit then
      Exit;

    T := DirEdit.Text;

    if InstallMode = imNormal then begin
      { Check if there's enough free disk space }
      if GetSpaceOnNearestMountPoint(False, T, FreeSpace, TotalSpace) then begin
        if Compare64(FreeSpace, MinimumSpace) < 0 then
          { If not, show warning }
          if LoggedMsgBox(FmtSetupMessage(msgDiskSpaceWarning,
               [IntToKBStr(MinimumSpace), IntToKBStr(FreeSpace)]),
             SetupMessages[msgDiskSpaceWarningTitle],
             mbConfirmation, MB_YESNO or MB_DEFBUTTON2, True, IDYES) <> IDYES then
            Exit;
      end;

      { Check if directory already exists }
      if ((SetupHeader.DirExistsWarning = ddYes) or
          ((SetupHeader.DirExistsWarning = ddAuto) and (T <> PrevAppDir))) and
         DirExists(T) then
        { If so, ask if user wants to install there anyway }
        if LoggedMsgBox(FmtSetupMessage1(msgDirExists, T), SetupMessages[msgDirExistsTitle],
          mbConfirmation, MB_YESNO, True, IDYES) <> IDYES then Exit;

      { Check if directory *doesn't* already exist }
      if (shEnableDirDoesntExistWarning in SetupHeader.Options) and
         not DirExists(T) then
        { If not, ask if user wants to install there anyway }
        if LoggedMsgBox(FmtSetupMessage1(msgDirDoesntExist, T), SetupMessages[msgDirDoesntExistTitle],
          mbConfirmation, MB_YESNO, True, IDYES) <> IDYES then Exit;
    end;

    Result := True;
    WizardDirValue := T;
  end;

  function CheckSelectComponentsPage: Boolean;
  var
    ComponentEntry: PSetupComponentEntry;
    FreeSpace, TotalSpace: Integer64;
    S: String;
    I: Integer;
  begin
    Result := False;

    if InstallMode = imNormal then begin
      if GetSpaceOnNearestMountPoint(False, DirEdit.Text, FreeSpace, TotalSpace) then begin
        if Compare64(FreeSpace, CurrentComponentsSpace) < 0 then
          if LoggedMsgBox(FmtSetupMessage(msgDiskSpaceWarning,
               [IntToKBStr(CurrentComponentsSpace), IntToKBStr(FreeSpace)]),
             SetupMessages[msgDiskSpaceWarningTitle],
             mbConfirmation, MB_YESNO or MB_DEFBUTTON2, True, IDYES) <> IDYES then
            Exit;
      end;

      //now see if there are unchecked components that are already installed
      if PrevSelectedComponents.Count > 0 then begin
        S := '';
        for I := 0 to ComponentsList.Items.Count-1 do begin
          if not ComponentsList.Checked[I] then begin
            ComponentEntry := PSetupComponentEntry(ComponentsList.ItemObject[I]);
            if not (coDisableNoUninstallWarning in ComponentEntry.Options) then begin
              if ListContains(PrevSelectedComponents, ComponentEntry.Name) then begin
                if S <> '' then
                  S := S + #13;
                S := S + ExpandConst(ComponentEntry.Description);
              end;
            end;
          end;
        end;

        if (S <> '') and (LoggedMsgBox(FmtSetupMessage1(msgNoUninstallWarning, S),
           SetupMessages[msgNoUninstallWarningTitle], mbConfirmation, MB_YESNO, True, IDYES) <> IDYES) then
          Exit;
      end;
    end;

    Result := True;
  end;

  function CheckSelectProgramGroupPage: Boolean;
  begin
    Result := ValidateGroupEdit;
    if Result then
      WizardGroupValue := GroupEdit.Text;
  end;

var
  PageIndex: Integer;
  Continue: Boolean;
  NewPageID: Integer;
  WizardComponents, WizardTasks: TStringList;
label Again;
begin
  if CurPageID = wpInstalling then
    Exit;

  case CurPageID of
    wpLicense: if not LicenseAcceptedRadio.Checked then Exit;  { paranoia }
    wpPassword: if not CheckPassword then Exit;
    wpUserInfo: if not CheckUserInfoPage then Exit;
    wpSelectDir: if not CheckSelectDirPage then Exit;
    wpSelectComponents: if not CheckSelectComponentsPage then Exit;
    wpSelectProgramGroup: if not CheckSelectProgramGroupPage then Exit;
    wpReady: if (InstallMode = imNormal) and not Visible then Exit;
  end;

  Continue := True;
  PageFromID(CurPageID).NextButtonClick(Continue);
  if not Continue then
    Exit;

  if CodeRunner <> nil then
    if CodeRunner.RunBooleanFunctions( 'NextButtonClick', [CurPageID], bcFalse, False, True) = False then
      Exit;

  { Go to the next page, or close wizard if it was on the last page }
  Again:
  NewPageID := CurPageID;
  PageIndex := PageIndexFromID(NewPageID);
  repeat
    case NewPageID of
      wpUserInfo: begin
          { Ensure these variables are still set when a user's ShouldSkipPage
            function returns True for the wpUserInfo page }
          WizardUserInfoName := UserInfoNameEdit.Text;
          WizardUserInfoOrg := UserInfoOrgEdit.Text;
          WizardUserInfoSerial := UserInfoSerialEdit.Text;
        end;
      wpSelectDir: WizardDirValue := RemoveBackslashUnlessRoot(DirEdit.Text);
      wpSelectProgramGroup: WizardGroupValue := RemoveBackslashUnlessRoot(GroupEdit.Text);
      wpPreparing, wpFinished: begin
          { Note: wpPreparing only 'gets' here if there was no PrepareToInstall failure or if
            PrepareToInstallNeedsRestart is true, else the Cancel button is used instead of the Next button }
          if (NewPageID <> wpPreparing) or (PrepareToInstallFailureMessage <> '') then begin
            DoneWithWizard := True;
            MainForm.Finish(NewPageID = wpPreparing);
            Exit;
          end;
        end;
    end;

    Inc(PageIndex);
    NewPageID := TWizardPage(FPageList[PageIndex]).ID;
    UpdatePage(NewPageID);

    case NewPageID of
      wpPreparing: begin
          WizardComponents := nil;
          WizardTasks := nil;
          try
            WizardComponents := TStringList.Create;
            WizardForm.GetComponents(WizardComponents, nil);
            WizardTasks := TStringList.Create;
            WizardForm.GetTasks(WizardTasks, nil);

            PrepareToInstallFailureMessage := PrepareToInstall(WizardComponents, WizardTasks);
            if PrepareToInstallFailureMessage <> '' then begin
              LogFmt('PrepareToInstall failed: %s', [PrepareToInstallFailureMessage]);
              LogFmt('Need to restart Windows? %s', [SYesNo[PrepareToInstallNeedsRestart]]);
              Break;  { stop on the page }
            end else if RmSessionStarted then begin
              SetCurPage(wpPreparing); { controls are already hidden by PrepareToInstall }
              BackButton.Visible := False;
              NextButton.Visible := False;
              if InstallMode = imSilent then
                WizardForm.Visible := True;
              try
                WizardForm.Update;
                RmFoundApplications := QueryRestartManager(WizardComponents, WizardTasks) <> '';
                if RmFoundApplications then
                  Break;  { stop on the page }
              finally
                UpdateCurPageButtonState;
              end;
            end;
          finally
            WizardTasks.Free;
            WizardComponents.Free;
          end;
        end;
      wpInstalling: begin
          SetCurPage(NewPageID);
          { Start the actual installation process }
          if not MainForm.Install then begin
            { The installation process failed }
            DoneWithWizard := True;
            Exit;
          end;
          goto Again;
        end;
    end;
  until not ShouldSkipPage(NewPageID);

  SetCurPage(NewPageID);
end;

procedure TWizardForm.BackButtonClick(Sender: TObject);
var
  Continue: Boolean;
  PrevPageID: Integer;
begin
  if CurPageID = wpInstalling then
    Exit;

  Continue := True;
  PageFromID(CurPageID).BackButtonClick(Continue);
  if not Continue then
    Exit;

  if CodeRunner <> nil then
    if CodeRunner.RunBooleanFunctions('BackButtonClick', [CurPageID], bcFalse, False, True) = False then
      Exit;

  PrevPageID := GetPreviousPageID;
  if PrevPageID <> -1 then
    SetCurPage(PrevPageID);
end;

procedure TWizardForm.CancelButtonClick(Sender: TObject);
begin
  { Clicking Cancel will do the same thing as the Close button }
  Close;
end;

procedure TWizardForm.CallCancelButtonClick(var ACancel, AConfirm: Boolean);
begin
  PageFromID(CurPageID).CancelButtonClick(ACancel, AConfirm);
  if not ACancel then
    Exit;

  if CodeRunner <> nil then
    CodeRunner.RunProcedures('CancelButtonClick', [CurPageID, @ACancel,
      @AConfirm], False);
end;

procedure TWizardForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  { Redirect an attempt to close this form to MainForm }
  MainForm.Close;
  Action := caNone;
end;

procedure TWizardForm.TypesComboChange(Sender: TObject);
var
  TypeEntry: PSetupTypeEntry;
begin
  //select the components for this type. if the type is custom only select
  //fixed components
  TypeEntry := PSetupTypeEntry(TypesCombo.Items.Objects[TypesCombo.ItemIndex]);
  SelectComponentsFromType(TypeEntry.Name, (toIsCustom in TypeEntry.Options));

  //if customization is possible remember the type and components that are
  //selected, so that we can reselect the setup type later if after customization
  //the user didn't really change anything
  //also hide the components list if necessary
  if HasCustomType then begin
    InitialSetupTypeIndex := TypesCombo.ItemIndex;
    GetSelectedComponents(InitialSelectedComponents, False, False);
    if not (shAlwaysShowComponentsList in SetupHeader.Options) then begin
      ComponentsList.Visible := toIsCustom in TypeEntry.Options;
      ComponentsDiskSpaceLabel.Visible := ComponentsList.Visible;
    end;
  end;

  UpdateComponentSizes;
  CalcCurrentComponentsSpace;
end;

procedure TWizardForm.ComponentsListClickCheck(Sender: TObject);
var
  SelectedComponents: TStringList;
  TypeEntry: PSetupTypeEntry;
  Equals: Boolean;
  I: Integer;
begin
  //first see if this current selection equals the initial selection
  //if so, reselect the initial setup type
  SelectedComponents := TStringList.Create();
  GetSelectedComponents(SelectedComponents, False, False);
  Equals := SelectedComponents.Equals(InitialSelectedComponents);
  SelectedComponents.Free();

  if Equals then begin
    //select the intial type
    TypesCombo.ItemIndex := InitialSetupTypeIndex;
  end else begin
    //select a custom type
    for I := 0 to Entries[seType].Count-1 do begin
      TypeEntry := Entries[seType][I];
      if (toIsCustom in TypeEntry.Options) then begin
        TypesCombo.ItemIndex := TypesCombo.Items.IndexOfObject(TObject(TypeEntry));
        SelectComponentsFromType(TypeEntry.Name, True);
        Break;
      end;
    end
  end;

  UpdateComponentSizes;
  CalcCurrentComponentsSpace;
end;

procedure TWizardForm.NoIconsCheckClick(Sender: TObject);
const
  ColorChange: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  GroupEdit.Enabled := not NoIconsCheck.Checked;
  GroupEdit.Color := ColorChange[GroupEdit.Enabled];
  GroupBrowseButton.Enabled := not NoIconsCheck.Checked;
end;

procedure TWizardForm.WMSysCommand(var Message: TWMSysCommand);
begin
  if Message.CmdType = 9999 then begin
    { Removing the About box or modifying any existing text inside it is a
      violation of the Inno Setup license agreement; see LICENSE.TXT.
      However, adding additional lines to the end of the About box is
      permitted. }
    var S := SetupTitle + ' version ' + SetupVersion + SNewLine;
    if SetupTitle <> 'Inno Setup' then
      S := S + (SNewLine + 'Based on Inno Setup' + SNewLine);
    S := S + ('Copyright (C) 1997-2025 Jordan Russell' + SNewLine +
      'Portions Copyright (C) 2000-2025 Martijn Laan' + SNewLine +
      'All rights reserved.' + SNewLine2 +
      'Inno Setup home page:' + SNewLine +
      'https://www.innosetup.com/');
    S := S + SNewLine2 + 'RemObjects Pascal Script home page:' + SNewLine +
      'https://www.remobjects.com/ps';
    if SetupMessages[msgAboutSetupNote] <> '' then
      S := S + SNewLine2 + SetupMessages[msgAboutSetupNote];
    if SetupMessages[msgTranslatorNote] <> '' then
      S := S + SNewLine2 + SetupMessages[msgTranslatorNote];
    StringChangeEx(S, '(C)', #$00A9, True);
    LoggedMsgBox(S, SetupMessages[msgAboutSetupTitle], mbInformation, MB_OK, False, 0)
  end else
    inherited;
end;

procedure TWizardForm.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  { Work around a VCL issue (Delphi 11.3) when MainFormOnTaskBar=True:
    If Application.Restore is called while the main form is hidden
    (Visible=False), the window can become visible because of the SW_RESTORE
    command it uses, which both unminimizes and shows a window. Reproducer:
      Application.Minimize;
      Hide;
      Application.Restore;
    This blocks any attempt to show the window while Visible=False.
    (SW_RESTORE will still unminimize the window; it just cannot show it.) }
  inherited;
  if not Visible then
    Message.WindowPos.flags := Message.WindowPos.flags and not SWP_SHOWWINDOW;
end;

procedure TWizardForm.LicenseAcceptedRadioClick(Sender: TObject);
begin
  if CurPageID = wpLicense then
    NextButton.Enabled := True;
end;

procedure TWizardForm.LicenseNotAcceptedRadioClick(Sender: TObject);
begin
  if CurPageID = wpLicense then
    NextButton.Enabled := False;
end;

procedure TWizardForm.UserInfoEditChange(Sender: TObject);
begin
  if CurPageID = wpUserInfo then begin
    try
      NextButton.Enabled := CheckSerialOk();
    except
      NextButton.Enabled := False;
      raise;
    end;
  end;
end;

function TWizardForm.ValidateDirEdit: Boolean;
begin
  Result := ValidateCustomDirEdit(DirEdit, shAllowUNCPath in SetupHeader.Options,
    shAllowRootDirectory in SetupHeader.Options,
    shAllowNetworkDrive in SetupHeader.Options);
end;

const
  SHPPFW_NONE = $00000000;
var
  SHPathPrepareForWriteFunc: function(hwnd: HWND; punkEnableModless: Pointer;
    pszPath: PChar; dwFlags: DWORD): HRESULT; stdcall;

procedure ReconnectPath(const Path: String);
{ Attempts to re-establish the connection to Path if it's on a network drive
  since mapped network drives are initially disconnected in elevated processes. }
var
  WindowList: Pointer;
begin
  { If this fails, we shouldn't display any message boxes since the install
    might be running silently with /SUPPRESSMSGBOXES and this is indeed so:
    The SHPathPrepareForWrite documentation claims that "user interface
    windows will not be created" when hwnd is NULL. }
  if Assigned(SHPathPrepareForWriteFunc) then begin
    { "Just in case" it tries to display UI anyway (it never did in tests),
      disable our windows }
    WindowList := DisableTaskWindows(0);
    try
      SHPathPrepareForWriteFunc(0, nil, PChar(Path), SHPPFW_NONE);
    finally
      EnableTaskWindows(WindowList);
    end;
  end;
end;

function ValidateCustomDirEdit(const AEdit: TEdit;
  const AllowUNCPath, AllowRootDirectory, AllowNetworkDrive: Boolean): Boolean;
{ Checks if AEdit.Text contains a valid-looking pathname, and returns True
  if so. May alter AEdit.Text to remove redundant spaces and backslashes. }
var
  T: String;
  IsUNCPath: Boolean;
  I: Integer;
  P: PChar;
  RootPath: String;
begin
  Result := False;

  T := AEdit.Text;
  TidyUpDirName(T);
  AEdit.Text := T;

  { Check if the path is too long.
    Note: There's no sense in allowing paths to be as long as MAX_PATH (260)
    since there wouldn't be any room left to append a filename. 240 should be
    a reasonable limit. }
  if Length(T) > 240 then begin
    LoggedMsgBox(SetupMessages[msgDirNameTooLong], '', mbError, MB_OK, True, IDOK);
    Exit;
  end;

  { Check for UNC pathname }
  IsUNCPath := (Length(T) >= 2) and (T[1] = '\') and (T[2] = '\');
  if IsUNCPath and not AllowUNCPath then begin
    LoggedMsgBox(SetupMessages[msgCannotInstallToUNCPath], '', mbError, MB_OK, True, IDOK);
    Exit;
  end;

  if not IsUNCPath then begin
    { Check if is at least 4 chars long and it has a drive letter, colon,
      and backslash }
    if not AllowRootDirectory then
      I := 4
    else
      I := 3;
    if (Length(T) < I) or not CharInSet(UpCase(T[1]), ['A'..'Z']) or
       (T[2] <> ':') or (T[3] <> '\') then begin
      LoggedMsgBox(SetupMessages[msgInvalidPath], '', mbError, MB_OK, True, IDOK);
      Exit;
    end;
  end
  else begin
    { Check if there's at least one backslash at least one character past the
      initial '\\' }
    P := @PChar(Pointer(T))[2];  { the casts avoid a UniqueString call... }  
    if PathStrScan(P, '\') <= P then begin
      LoggedMsgBox(SetupMessages[msgInvalidPath], '', mbError, MB_OK, True, IDOK);
      Exit;
    end;
  end;

  { Verify that no path components contain control characters, end in spaces,
    or consist only of dots }
  if ContainsControlCharacters(T) or
     PathComponentsContainTrailingSpaces(T) or
     PathComponentsContainInvalidDots(T) then begin
    LoggedMsgBox(SetupMessages[msgInvalidDirName], '', mbError, MB_OK, True, IDOK);
    Exit;
  end;

  { Check for invalid characters after 'x:' or '\\' }
  if PathLastDelimiter(BadDirChars, Copy(T, 3, Maxint)) <> 0 then begin
    LoggedMsgBox(FmtSetupMessage1(msgBadDirName32, SpaceString(BadDirChars)), '',
      mbError, MB_OK, True, IDOK);
    Exit;
  end;

  { Check if it's a valid drive, reconnecting it first if necessary }
  RootPath := RemoveBackslashUnlessRoot(AddBackslash(PathExtractDrive(T)));
  ReconnectPath(RootPath);
  if not DirExists(RootPath) then begin
    LoggedMsgBox(SetupMessages[msgInvalidDrive], '', mbError, MB_OK, True, IDOK);
    Exit;
  end;

  { After reconnecting, check if it's a disallowed network drive }
  if not IsUNCPath and not AllowNetworkDrive and
     (GetDriveType(PChar(RootPath)) = DRIVE_REMOTE) then begin
    LoggedMsgBox(SetupMessages[msgCannotInstallToNetworkDrive], '', mbError, MB_OK, True, IDOK);
    Exit;
  end;

  Result := True;
end;

function TWizardForm.ValidateGroupEdit: Boolean;
var
  T: String;
begin
  Result := False;

  if not NoIconsCheck.Checked then begin
    T := GroupEdit.Text;
    TidyUpGroupName(T);
    GroupEdit.Text := T;

    { Check if the path is too long }
    if Length(T) > 120 then begin
      LoggedMsgBox(SetupMessages[msgGroupNameTooLong], '', mbError, MB_OK, True, IDOK);
      Exit;
    end;

    { Verify that no path components contain control characters or end in
      spaces }
    if ContainsControlCharacters(T) or
       PathComponentsContainTrailingSpaces(T) then begin
      LoggedMsgBox(SetupMessages[msgInvalidGroupName], '', mbError, MB_OK, True, IDOK);
      Exit;
    end;

    if T = '' then begin
      LoggedMsgBox(SetupMessages[msgMustEnterGroupName], '', mbError, MB_OK, True, IDOK);
      Exit;
    end;

    { Check for invalid characters }
    if PathLastDelimiter(BadDirChars, T) <> 0 then begin
      LoggedMsgBox(FmtSetupMessage1(msgBadGroupName, SpaceString(BadDirChars)),
        '', mbError, MB_OK, True, IDOK);
      Exit;
    end;
  end;

  Result := True;
end;

procedure TWizardForm.DirBrowseButtonClick(Sender: TObject);
var
  NewFolderName, Path: String;
begin
  NewFolderName := Trim(PathExtractName(RemoveBackslashUnlessRoot(ExpandedDefaultDirName)));
  { If ExpandedDefaultDirName is a root directory, there will be no name }
  if NewFolderName = '' then
    NewFolderName := Trim(SetupMessages[msgNewFolderName]);

  Path := DirEdit.Text;
  if ShowSelectFolderDialog(False, shAppendDefaultDirName in SetupHeader.Options,
     Path, NewFolderName) then
    DirEdit.Text := Path;
end;

procedure TWizardForm.GroupBrowseButtonClick(Sender: TObject);
var
  NewFolderName, Path: String;
begin
  NewFolderName := Trim(PathExtractName(ExpandedDefaultGroupName));
  if NewFolderName = '' then
    NewFolderName := Trim(SetupMessages[msgNewFolderName]);

  Path := GroupEdit.Text;
  if ShowSelectFolderDialog(True, shAppendDefaultGroupName in SetupHeader.Options,
     Path, NewFolderName) then
    GroupEdit.Text := Path;
end;

{ also used by ScriptDlg! }
procedure TWizardForm.DirTreeRename(Sender: TCustomFolderTreeView;
  var NewName: string; var Accept: Boolean);
const
  NewFolderBadDirChars = '\' + BadDirChars;
begin
  NewName := Trim(NewName);
  if (NewName = '') or PathComponentsContainInvalidDots(NewName) then begin
    Accept := False;
    LoggedMsgBox(SetupMessages[msgInvalidDirName], '', mbError, MB_OK, True, IDOK);
    Exit;
  end;
  if PathLastDelimiter(NewFolderBadDirChars, NewName) <> 0 then begin
    Accept := False;
    LoggedMsgBox(FmtSetupMessage1(msgBadDirName32, SpaceString(NewFolderBadDirChars)),
      '', mbError, MB_OK, True, IDOK);
    Exit;
  end;
end;

procedure TWizardForm.GroupTreeRename(Sender: TCustomFolderTreeView;
  var NewName: string; var Accept: Boolean);
const
  NewFolderBadDirChars = '\' + BadDirChars;
begin
  NewName := Trim(NewName);
  if (NewName = '') or PathComponentsContainInvalidDots(NewName) then begin
    Accept := False;
    LoggedMsgBox(SetupMessages[msgInvalidGroupName], '', mbError, MB_OK, True, IDOK);
    Exit;
  end;
  if PathLastDelimiter(NewFolderBadDirChars, NewName) <> 0 then begin
    Accept := False;
    LoggedMsgBox(FmtSetupMessage1(msgBadGroupName, SpaceString(NewFolderBadDirChars)),
      '', mbError, MB_OK, True, IDOK);
    Exit;
  end;
end;

procedure TWizardForm.ClickToStartPage;
{ Simulates clicks on the Next button until Setup is ready to start.
  This is called on non-silent installs. }
begin
  while ShouldSkipPage(CurPageID) and NextButton.CanFocus do
    NextButton.Click;
end;

procedure TWizardForm.ClickThroughPages;
{ Simulates clicks on the Next button until Setup is ready to terminate.
  This is called on silent installs. }
var
  BeforeID: Integer;
begin
  while True do begin
    if (CurPageID = wpPreparing) and (PrepareToInstallFailureMessage <> '') and not (PrepareToInstallNeedsRestart and not InitNoRestart) then begin
      { Special handling needed for wpPreparing since it displays its error
        message inline on the wizard. Since the wizard isn't currently visible,
        we have to display the message in a message box if it won't be displayed
        by a reboot confirmation message box later on. }
      LoggedMsgBox(PrepareToInstallFailureMessage, '',
        mbCriticalError, MB_OK, True, IDOK);
      if PrepareToInstallNeedsRestart then
        SetupExitCode := ecPrepareToInstallFailedRestartNeeded
      else
        SetupExitCode := ecPrepareToInstallFailed;
      Abort;

      { Note: no special handling if it stops on wpPreparing because of in-use
        files ((CurPageID = wpPreparing) and (PrepareToInstallFailureMessage = '')),
        instead it will always choose to close applications when running silently
        unless /NOCLOSEAPPLICATIONS was used. }
    end;

    BeforeID := CurPageID;

    { Simulate a click on NextButton if it's enabled & visible }
    try
      if NextButton.CanFocus then
        NextButton.Click;
    except
      { Mustn't propagate post-install exceptions }
      if MainForm.CurStep <= ssInstall then
        raise
      else
        Application.HandleException(Self);
    end;

    if DoneWithWizard then
      Break;

    { If the page didn't change, there must've been an error }
    if CurPageID = BeforeID then begin
      if MainForm.CurStep <= ssInstall then begin
        Log('Failed to proceed to next wizard page; aborting.');
        Abort;
      end
      else begin
        { After installation, we can't abort since e.g. a restart might be
          needed. Instead, to avoid getting stuck in a loop, show the wizard
          (even though this is a silent install) and let the user deal with the
          problem on their own. }
        Log('Failed to proceed to next wizard page; showing wizard.');
        WizardForm.Visible := True;
        Application.Restore;
        Break;
      end;
    end;
  end;
end;

initialization
  SHPathPrepareForWriteFunc := GetProcAddress(SafeLoadLibrary(AddBackslash(GetSystemDir) + shell32,
    SEM_NOOPENFILEERRORBOX), 'SHPathPrepareForWriteW');
end.
