TObject = class
  constructor Create;
  procedure Free;
end;

TPersistent = class(TObject)
  procedure Assign(Source: TPersistent);
end;

TComponent = class(TPersistent)
  function FindComponent(AName: String): TComponent;
  constructor Create(AOwner: TComponent);
  property Owner: TComponent; read write;
  procedure DestroyComponents;
  procedure Destroying;
  procedure FreeNotification(AComponent: TComponent);
  procedure InsertComponent(AComponent: TComponent);
  procedure RemoveComponent(AComponent: TComponent);
  property Components[Index: Integer]: TComponent; read;
  property ComponentCount: Integer; read;
  property ComponentIndex: Integer; read write;
  property ComponentState: Byte; read;
  property DesignInfo: Longint; read write;
  property Name: String; read write;
  property Tag: Longint; read write;
end;

TStrings = class(TPersistent)
  function Add(S: String): Integer;
  procedure Append(S: String);
  procedure AddStrings(Strings: TStrings);
  procedure Clear;
  procedure Delete(Index: Integer);
  function IndexOf(const S: String): Integer;
  procedure Insert(Index: Integer; S: String);
  property Count: Integer; read;
  property Text: String; read write;
  property CommaText: String; read write;
  procedure LoadFromFile(FileName: String);
  procedure SaveToFile(FileName: String);
  property Strings[Index: Integer]: String; read write;
  property Objects[Index: Integer]: TObject; read write;
end;

TNotifyEvent = procedure(Sender: TObject);

TDuplicates = (dupIgnore, dupAccept, dupError);

TStringList = class(TStrings)
  function Find(S: String; var Index: Integer): Boolean;
  procedure Sort;
  property Duplicates: TDuplicates; read write;
  property Sorted: Boolean; read write;
  property OnChange: TNotifyEvent; read write;
  property OnChanging: TNotifyEvent; read write;
end;

{ Seek Origin values: soFromBeginning, soFromCurrent, soFromEnd }

TStream = class(TObject)
  function Read(var Buffer: AnyString; ByteCount: Longint): Longint;
  function Write(const Buffer: AnyString; ByteCount: Longint): Longint;
  function Seek(Offset: Int64; Origin: Word): Int64;
  procedure ReadBuffer(var Buffer: AnyString; ByteCount: Longint);
  procedure WriteBuffer(const Buffer: AnyString; ByteCount: Longint);
  function CopyFrom(Source: TStream; ByteCount: Int64; BufferSize: Integer): Int64;
  property Position: Longint; read write;
  property Size: Longint; read write;
end;

THandleStream = class(TStream)
  constructor Create(AHandle: Integer);
  property Handle: Integer; read;
end;

TFileStream = class(THandleStream)
  constructor Create(Filename: String; Mode: Word);
end;

TStringStream = class(TStream)
  constructor Create(AString: String);
end;

TGraphicsObject = class(TPersistent)
  property OnChange: TNotifyEvent; read write;
end;

TBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);

TBrush = class(TGraphicsObject)
  constructor Create;
  property Color: TColor; read write;
  property Style: TBrushStyle; read write;
end;

TFontStyle = (fsBold, fsItalic, fsUnderline, fsStrikeOut);

TFontStyles = set of TFontStyle;

TColor = Integer;

{ TColor values: clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray, clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clLtGray, clDkGray, clWhite, clNone, clDefault, clScrollBar, clBackground, clActiveCaption, clInactiveCaption, clMenu, clWindow, clWindowFrame, clMenuText, clWindowText, clCaptionText, clActiveBorder, clInactiveBorder, clAppWorkSpace, clHighlight, clHighlightText, clBtnFace, clBtnShadow, clGrayText, clBtnText, clInactiveCaptionText, clBtnHighlight, cl3DDkShadow, cl3DLight, clInfoText, clInfoBk, clHotLight }

TFont = class(TGraphicsObject)
  constructor Create;
  property Handle: Integer; read;
  property Color: TColor; read write;
  property Height: Integer; read write;
  property Name: String; read write;
  property Pitch: Byte; read write;
  property Size: Integer; read write;
  property PixelsPerInch: Integer; read write;
  property Style: TFontStyles; read write;
end;

TPenMode = (pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy, pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge, pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor);

TPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear, psInsideFrame);

TPen = class(TGraphicsObject)
  constructor Create;
  property Color: TColor; read write;
  property Mode: TPenMode; read write;
  property Style: TPenStyle; read write;
  property Width: Integer; read write;
end;

TCanvas = class(TPersistent)
  procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
  procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
  procedure Draw(X, Y: Integer; Graphic: TGraphic);
  procedure Ellipse(X1, Y1, X2, Y2: Integer);
  procedure FloodFill(X, Y: Integer; Color: TColor; FillStyle: Byte);
  procedure LineTo(X, Y: Integer);
  procedure MoveTo(X, Y: Integer);
  procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
  procedure Rectangle(X1, Y1, X2, Y2: Integer);
  procedure Refresh;
  procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
  function TextHeight(Text: String): Integer;
  procedure TextOut(X, Y: Integer; Text: String);
  function TextWidth(Text: String): Integer;
  property Handle: Integer; read write;
  property Pixels: Integer Integer Integer; read write;
  property Brush: TBrush; read;
  property CopyMode: Byte; read write;
  property Font: TFont; read;
  property Pen: TPen; read;
end;

TGraphic = class(TPersistent)
  procedure LoadFromFile(const Filename: String);
  procedure SaveToFile(const Filename: String);
  property Empty: Boolean; read write;
  property Height: Integer; read write;
  property Modified: Boolean; read write;
  property Width: Integer; read write;
  property OnChange: TNotifyEvent; read write;
end;

TAlphaFormat = (afIgnored, afDefined, afPremultiplied);

HBITMAP = Integer;

TBitmap = class(TGraphic)
  procedure LoadFromStream(Stream: TStream);
  procedure SaveToStream(Stream: TStream);
  property AlphaFormat: TAlphaFormat; read write;
  property Canvas: TCanvas; read write;
  property Handle: HBITMAP; read write;
end;

TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient);

TAnchorKind = (akLeft, akTop, akRight, akBottom);

TAnchors = set of TAnchorKind;

TCursor = Integer;

{ TCursor values: crDefault, crNone, crArrow, crCross, crIBeam, crSizeNESW, crSizeNS, crSizeNWSE, crSizeWE, crUpArrow, crHourGlass, crDrag, crNoDrop, crHSplit, crVSplit, crMultiDrag, crSQLWait, crNo, crAppStart, crHelp, crHandPoint, crSizeAll, crHand }

TControl = class(TComponent)
  constructor Create(AOwner: TComponent);
  procedure BringToFront;
  procedure Hide;
  procedure Invalidate;
  procedure Refresh;
  procedure Repaint;
  procedure SendToBack;
  procedure Show;
  procedure Update;
  procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  property Left: Integer; read write;
  property Top: Integer; read write;
  property Width: Integer; read write;
  property Height: Integer; read write;
  property Hint: String; read write;
  property Align: TAlign; read write;
  property ClientHeight: Longint; read write;
  property ClientWidth: Longint; read write;
  property ShowHint: Boolean; read write;
  property Visible: Boolean; read write;
  property Enabled: Boolean; read write;
  property Cursor: TCursor; read write;
end;

TWinControl = class(TControl)
  property Parent: TWinControl; read write;
  property ParentBackground: Boolean; read write;
  property Handle: Longint; read write;
  property Showing: Boolean; read;
  property TabOrder: Integer; read write;
  property TabStop: Boolean; read write;
  function CanFocus: Boolean;
  function Focused: Boolean;
  property Controls[Index: Integer]: TControl; read;
  property ControlCount: Integer; read;
end;

TGraphicControl = class(TControl)
end;

TCustomControl = class(TWinControl)
end;

TScrollingWinControl = class(TWinControl)
  procedure ScrollInView(AControl: TControl);
end;

TFormBorderStyle = (bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow, bsSizeToolWin);

TBorderIcon = (biSystemMenu, biMinimize, biMaximize, biHelp);

TBorderIcons = set of TBorderIcon;

TConstraintSize = 0..MaxInt;

TSizeConstraints = class(TPersistent);
  property MaxHeight: TConstraintSize; read write;
  property MaxWidth: TConstraintSize; read write;
  property MinHeight: TConstraintSize; read write;
  property MinWidth: TConstraintSize; read write;
end;

TFormStyle = (fsNormal, fsMDIChild, fsMDIForm, fsStayOnTop);

TPopupMode = (pmNone, pmAuto, pmExplicit);

TPosition = (poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly, poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter);

TCloseAction = (caNone, caHide, caFree, caMinimize);

TCloseEvent = procedure(Sender: TObject; var Action: TCloseAction);

TCloseQueryEvent = procedure(Sender: TObject; var CanClose: Boolean);

TEShiftState = (ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble);

TShiftState = set of TEShiftState;

TKeyEvent = procedure(Sender: TObject; var Key: Word; Shift: TShiftState);

TKeyPressEvent = procedure(Sender: TObject; var Key: Char);

TForm = class(TScrollingWinControl)
  constructor CreateNew(AOwner: TComponent);
  procedure Close;
  procedure Hide;
  procedure Show;
  function ShowModal: Integer;
  procedure Release;
  property Active: Boolean; read;
  property ActiveControl: TWinControl; read write;
  property Anchors: TAnchors; read write;
  property AutoScroll: Boolean; read write;
  property BorderIcons: TBorderIcons; read write;
  property BorderStyle: TFormBorderStyle; read write;
  property Caption: String; read write;
  property Color: TColor; read write;
  property Constraints: TSizeConstraints; read write;
  property Font: TFont; read write;
  property FormStyle: TFormStyle; read write;
  property KeyPreview: Boolean; read write;
  property PopupMode: TPopupMode; read write;
  property PopupParent: TForm; read write;
  property Position: TPosition; read write;
  property OnActivate: TNotifyEvent; read write;
  property OnClick: TNotifyEvent; read write;
  property OnDblClick: TNotifyEvent; read write;
  property OnClose: TCloseEvent; read write;
  property OnCloseQuery: TCloseQueryEvent; read write;
  property OnCreate: TNotifyEvent; read write;
  property OnDestroy: TNotifyEvent; read write;
  property OnDeactivate: TNotifyEvent; read write;
  property OnHide: TNotifyEvent; read write;
  property OnKeyDown: TKeyEvent; read write;
  property OnKeyPress: TKeyPressEvent; read write;
  property OnKeyUp: TKeyEvent; read write;
  property OnResize: TNotifyEvent; read write;
  property OnShow: TNotifyEvent; read write;
end;

TCustomLabel = class(TGraphicControl)
end;

TAlignment = (taLeftJustify, taRightJustify, taCenter);

TLabel = class(TCustomLabel)
  property Alignment: TAlignment; read write;
  property Anchors: TAnchors; read write;
  property AutoSize: Boolean; read write;
  property Caption: String; read write;
  property Color: TColor; read write;
  property FocusControl: TWinControl; read write;
  property Font: TFont; read write;
  property WordWrap: Boolean; read write;
  property OnClick: TNotifyEvent; read write;
  property OnDblClick: TNotifyEvent; read write;
end;

TCustomEdit = class(TWinControl)
  procedure Clear;
  procedure ClearSelection;
  procedure SelectAll;
  property Modified: Boolean; read write;
  property SelLength: Integer; read write;
  property SelStart: Integer; read write;
  property SelText: String; read write;
  property Text: String; read write;
end;

TBorderStyle = TFormBorderStyle;

TEditCharCase = (ecNormal, ecUpperCase, ecLowerCase);

TEdit = class(TCustomEdit)
  property Anchors: TAnchors; read write;
  property AutoSelect: Boolean; read write;
  property AutoSize: Boolean; read write;
  property BorderStyle: TBorderStyle; read write;
  property CharCase: TEditCharCase; read write;
  property Color: TColor; read write;
  property Font: TFont; read write;
  property HideSelection: Boolean; read write;
  property MaxLength: Integer; read write;
  property PasswordChar: Char; read write;
  property ReadOnly: Boolean; read write;
  property Text: String; read write;
  property OnChange: TNotifyEvent; read write;
  property OnClick: TNotifyEvent; read write;
  property OnDblClick: TNotifyEvent; read write;
  property OnKeyDown: TKeyEvent; read write;
  property OnKeyPress: TKeyPressEvent; read write;
  property OnKeyUp: TKeyEvent; read write;
end;

TNewEdit = class(TEdit)
end;

TCustomMemo = class(TCustomEdit)
  property Lines: TStrings; read write;
end;

TScrollStyle = (ssNone, ssHorizontal, ssVertical, ssBoth);

TMemo = class(TCustomMemo)
  property Alignment: TAlignment; read write;
  property Anchors: TAnchors; read write;
  property BorderStyle: TBorderStyle; read write;
  property Color: TColor; read write;
  property Font: TFont; read write;
  property HideSelection: Boolean; read write;
  property Lines: TStrings; read write;
  property MaxLength: Integer; read write;
  property ReadOnly: Boolean; read write;
  property ScrollBars: TScrollStyle; read write;
  property WantReturns: Boolean; read write;
  property WantTabs: Boolean; read write;
  property WordWrap: Boolean; read write;
  property OnChange: TNotifyEvent; read write;
  property OnClick: TNotifyEvent; read write;
  property OnDblClick: TNotifyEvent; read write;
  property OnKeyDown: TKeyEvent; read write;
  property OnKeyPress: TKeyPressEvent; read write;
  property OnKeyUp: TKeyEvent; read write;
end;

TNewMemo = class(TMemo)
end;

TCustomComboBox = class(TWinControl)
  property DroppedDown: Boolean; read write;
  property Items: TStrings; read write;
  property ItemIndex: Integer; read write;
end;

TComboBoxStyle = (csDropDown, csSimple, csDropDownList, csOwnerDrawFixed, csOwnerDrawVariable);

TComboBox = class(TCustomComboBox)
  property Anchors: TAnchors; read write;
  property Color: TColor; read write;
  property DropDownCount: Integer; read write;
  property Font: TFont; read write;
  property MaxLength: Integer; read write;
  property Sorted: Boolean; read write;
  property Style: TComboBoxStyle; read write;
  property Text: String; read write;
  property OnChange: TNotifyEvent; read write;
  property OnClick: TNotifyEvent; read write;
  property OnDblClick: TNotifyEvent; read write;
  property OnDropDown: TNotifyEvent; read write;
  property OnKeyDown: TKeyEvent; read write;
  property OnKeyPress: TKeyPressEvent; read write;
  property OnKeyUp: TKeyEvent; read write;
end;

TNewComboBox = class(TComboBox)
end;

TButtonControl = class(TWinControl)
end;

TButton = class(TButtonControl)
  property Anchors: TAnchors; read write;
  property Cancel: Boolean; read write;
  property Caption: String; read write;
  property Default: Boolean; read write;
  property Font: TFont; read write;
  property ModalResult: Longint; read write;
  property OnClick: TNotifyEvent; read write;
end;

TNewButton = class(TButton)
end;

TCustomCheckBox = class(TButtonControl)
end;

TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);

TCheckBox = class(TCustomCheckBox)
  property Alignment: TAlignment; read write;
  property AllowGrayed: Boolean; read write;
  property Anchors: TAnchors; read write;
  property Caption: String; read write;
  property Checked: Boolean; read write;
  property Color: TColor; read write;
  property Font: TFont; read write;
  property State: TCheckBoxState; read write;
  property OnClick: TNotifyEvent; read write;
end;

TNewCheckBox = class(TCheckBox)
end;

TRadioButton = class(TButtonControl)
  property Alignment: TAlignment; read write;
  property Anchors: TAnchors; read write;
  property Caption: String; read write;
  property Checked: Boolean; read write;
  property Color: TColor; read write;
  property Font: TFont; read write;
  property OnClick: TNotifyEvent; read write;
  property OnDblClick: TNotifyEvent; read write;
end;

TNewRadioButton = class(TRadioButton)
end;

TSysLinkType = (sltURL, sltID);

TSysLinkEvent = procedure(Sender: TObject; const Link: string; LinkType: TSysLinkType);

TCustomLinkLabel = class(TWinControl)
  property Alignment: TAlignment; read write;
  property AutoSize: Boolean; read write;
  property UseVisualStyle: Boolean; read write;
  property OnLinkClick: TSysLinkEvent; read write;
end;

TLinkLabel = class(TCustomLinkLabel)
  property Anchors: TAnchors; read write;
  property Caption: String; read write;
  property Color: TColor; read write;
  property Font: TFont; read write;
end;

TNewLinkLabel = class(TLinkLabel)
  function AdjustHeight: Integer;
end;

TCustomListBox = class(TWinControl)
  property Items: TStrings; read write;
  property ItemIndex: Integer; read write;
  property SelCount: Integer; read;
  property Selected[Index: Integer]: Boolean; read write;
end;

TListBoxStyle = (lbStandard, lbOwnerDrawFixed, lbOwnerDrawVariable);

TListBox = class(TCustomListBox)
  property Anchors: TAnchors; read write;
  property BorderStyle: TBorderStyle; read write;
  property Color: TColor; read write;
  property Font: TFont; read write;
  property MultiSelect: Boolean; read write;
  property Sorted: Boolean; read write;
  property Style: TListBoxStyle; read write;
  property OnClick: TNotifyEvent; read write;
  property OnDblClick: TNotifyEvent; read write;
  property OnKeyDown: TKeyEvent; read write;
  property OnKeyPress: TKeyPressEvent; read write;
  property OnKeyUp: TKeyEvent; read write;
end;

TNewListBox = class(TListBox)
end;

TBevelKind = (bkNone, bkTile, bkSoft, bkFlat);

TBevelShape = (bsBox, bsFrame, bsTopLine, bsBottomLine, bsLeftLine, bsRightLine, bsSpacer);

TBevelStyle = (bsLowered, bsRaised);

TBevel = class(TGraphicControl)
  property Anchors: TAnchors; read write;
  property Shape: TBevelShape; read write;
  property Style: TBevelStyle; read write;
end;

TCustomPanel = class(TCustomControl)
end;

TPanelBevel = (bvNone, bvLowered, bvRaised, bvSpace);

TBevelWidth = Longint;

TBorderWidth = Longint;

TPanel = class(TCustomPanel)
  property Alignment: TAlignment; read write;
  property Anchors: TAnchors; read write;
  property BevelInner: TPanelBevel; read write;
  property BevelKind: TBevelKind; read write;
  property BevelOuter: TPanelBevel; read write;
  property BevelWidth: TBevelWidth; read write;
  property BorderWidth: TBorderWidth; read write;
  property BorderStyle: TBorderStyle; read write;
  property Caption: String; read write;
  property Color: TColor; read write;
  property Font: TFont; read write;
  property OnClick: TNotifyEvent; read write;
  property OnDblClick: TNotifyEvent; read write;
end;

TNewStaticText = class(TWinControl)
  function AdjustHeight: Integer;
  property Anchors: TAnchors; read write;
  property AutoSize: Boolean; read write;
  property Caption: String; read write;
  property Color: TColor; read write;
  property FocusControl: TWinControl; read write;
  property Font: TFont; read write;
  property ForceLTRReading: Boolean; read write;
  property ShowAccelChar: Boolean; read write;
  property WordWrap: Boolean; read write;
  property OnClick: TNotifyEvent; read write;
  property OnDblClick: TNotifyEvent; read write;
end;

TCheckItemOperation = (coUncheck, coCheck, coCheckWithChildren);

TNewCheckListBox = class(TCustomListBox)
  function AddCheckBox(const ACaption, ASubItem: String; ALevel: Byte; AChecked, AEnabled, AHasInternalChildren, ACheckWhenParentChecked: Boolean; AObject: TObject): Integer;
  function AddGroup(ACaption, ASubItem: String; ALevel: Byte; AObject: TObject): Integer;
  function AddRadioButton(const ACaption, ASubItem: String; ALevel: Byte; AChecked, AEnabled: Boolean; AObject: TObject): Integer;
  function CheckItem(const Index: Integer; const AOperation: TCheckItemOperation): Boolean;
  property Anchors: TAnchors; read write;
  property Checked[Index: Integer]: Boolean; read write;
  property State[Index: Integer]: TCheckBoxState; read write;
  property ItemCaption[Index: Integer]: String; read write;
  property ItemEnabled[Index: Integer]: Boolean; read write;
  property ItemFontStyle[Index: Integer]: TFontStyles; read write;
  property ItemLevel[Index: Integer]: Byte; read;
  property ItemObject[Index: Integer]: TObject; read write;
  property ItemSubItem[Index: Integer]: String; read write;
  property SubItemFontStyle[Index: Integer]: TFontStyles; read write;
  property Flat: Boolean; read write;
  property MinItemHeight: Integer; read write;
  property Offset: Integer; read write;
  property OnClickCheck: TNotifyEvent; read write;
  property BorderStyle: TBorderStyle; read write;
  property Color: TColor; read write;
  property Font: TFont; read write;
  property Sorted: Boolean; read write;
  property OnClick: TNotifyEvent; read write;
  property OnDblClick: TNotifyEvent; read write;
  property OnKeyDown: TKeyEvent; read write;
  property OnKeyPress: TKeyPressEvent; read write;
  property OnKeyUp: TKeyEvent; read write;
  property ShowLines: Boolean; read write;
  property WantTabs: Boolean; read write;
  property RequireRadioSelection: Boolean; read write;
end;

TNewProgressBarState = (npbsNormal, npbsError, npbsPaused);

TNewProgressBarStyle = (npbstNormal, npbstMarquee);

TNewProgressBar = class(TWinControl)
  property Anchors: TAnchors; read write;
  property Min: Longint; read write;
  property Max: Longint; read write;
  property Position: Longint; read write;
  property State: TNewProgressBarState; read write;
  property Style: TNewProgressBarStyle; read write;
  property Visible: Boolean; read write;
end;

TRichEditViewer = class(TMemo)
  property Anchors: TAnchors; read write;
  property BevelKind: TBevelKind; read write;
  property BorderStyle: TBorderStyle; read write;
  property RTFText: AnsiString; write;
  property UseRichEdit: Boolean; read write;
end;

TPasswordEdit = class(TCustomEdit)
  property Anchors: TAnchors; read write;
  property AutoSelect: Boolean; read write;
  property AutoSize: Boolean; read write;
  property BorderStyle: TBorderStyle; read write;
  property Color: TColor; read write;
  property Font: TFont; read write;
  property HideSelection: Boolean; read write;
  property MaxLength: Integer; read write;
  property Password: Boolean; read write;
  property ReadOnly: Boolean; read write;
  property Text: String; read write;
  property OnChange: TNotifyEvent; read write;
  property OnClick: TNotifyEvent; read write;
  property OnDblClick: TNotifyEvent; read write;
  property OnKeyDown: TKeyEvent; read write;
  property OnKeyPress: TKeyPressEvent; read write;
  property OnKeyUp: TKeyEvent; read write;
end;

TCustomFolderTreeView = class(TWinControl)
  procedure ChangeDirectory(const Value: String; const CreateNewItems: Boolean);
  procedure CreateNewDirectory(const ADefaultName: String);
  property: Directory: String; read write;
end;

TFolderRenameEvent = procedure(Sender: TCustomFolderTreeView; var NewName: String; var Accept: Boolean);

TFolderTreeView = class(TCustomFolderTreeView)
  property Anchors: TAnchors; read write;
  property OnChange: TNotifyEvent; read write;
  property OnRename: TFolderRenameEvent; read write;
end;

TStartMenuFolderTreeView = class(TCustomFolderTreeView)
  procedure SetPaths(const AUserPrograms, ACommonPrograms, AUserStartup, ACommonStartup: String);
  property Anchors: TAnchors; read write;
  property OnChange: TNotifyEvent; read write;
  property OnRename: TFolderRenameEvent; read write;
end;

TBitmapImage = class(TGraphicControl)
  property Anchors: TAnchors; read write;
  property AutoSize: Boolean; read write;
  property BackColor: TColor; read write;
  property Center: Boolean; read write;
  property Bitmap: TBitmap; read write;
  property ReplaceColor: TColor; read write;
  property ReplaceWithColor: TColor; read write;
  property Stretch: Boolean; read write;
  property OnClick: TNotifyEvent; read write;
  property OnDblClick: TNotifyEvent; read write;
end;

TNewNotebook = class(TWinControl)
  function FindNextPage(CurPage: TNewNotebookPage; GoForward: Boolean): TNewNotebookPage;
  property Anchors: TAnchors; read write;
  property PageCount: Integer; read write;
  property Pages[Index: Integer]: TNewNotebookPage; read;
  property ActivePage: TNewNotebookPage; read write;
end;

TNewNotebookPage = class(TCustomControl)
  property Color: TColor; read write;
  property Notebook: TNewNotebook; read write;
  property PageIndex: Integer; read write;
end;

TWizardPageNotifyEvent = procedure(Sender: TWizardPage);
TWizardPageButtonEvent = function(Sender: TWizardPage): Boolean;
TWizardPageCancelEvent = procedure(Sender: TWizardPage; var ACancel, AConfirm: Boolean);
TWizardPageShouldSkipEvent = function(Sender: TWizardPage): Boolean;

TWizardPage = class(TComponent)
  property ID: Integer; read;
  property Caption: String; read write;
  property Description: String; read write;
  property Surface: TNewNotebookPage; read;
  property SurfaceColor: TColor; read;
  property SurfaceHeight: Integer; read;
  property SurfaceWidth: Integer; read;
  property OnActivate: TWizardPageNotifyEvent; read write;
  property OnBackButtonClick: TWizardPageButtonEvent; read write;
  property OnCancelButtonClick: TWizardPageCancelEvent; read write;
  property OnNextButtonClick: TWizardPageButtonEvent; read write;
  property OnShouldSkipPage: TWizardPageShouldSkipEvent; read write;
end;

TInputQueryWizardPage = class(TWizardPage)
  function Add(const APrompt: String; const APassword: Boolean): Integer;
  property Edits[Index: Integer]: TPasswordEdit; read;
  property PromptLabels[Index: Integer]: TNewStaticText; read;
  property SubCaptionLabel: TNewStaticText; read;
  property Values[Index: Integer]: String; read write;
end;

TInputOptionWizardPage = class(TWizardPage)
  function Add(const ACaption: String): Integer;
  function AddEx(const ACaption: String; const ALevel: Byte; const AExclusive: Boolean): Integer;
  property CheckListBox: TNewCheckListBox; read;
  property SelectedValueIndex: Integer; read write;
  property SubCaptionLabel: TNewStaticText; read;
  property Values[Index: Integer]: Boolean; read write;
end;

TInputDirWizardPage = class(TWizardPage)
  function Add(const APrompt: String): Integer;
  property Buttons[Index: Integer]: TNewButton; read;
  property Edits[Index: Integer]: TEdit; read;
  property NewFolderName: String; read write;
  property PromptLabels[Index: Integer]: TNewStaticText; read;
  property SubCaptionLabel: TNewStaticText; read;
  property Values[Index: Integer]: String; read write;
end;

TInputFileWizardPage = class(TWizardPage)
  function Add(const APrompt, AFilter, ADefaultExtension: String): Integer;
  property Buttons[Index: Integer]: TNewButton; read;
  property Edits[Index: Integer]: TEdit; read;
  property PromptLabels[Index: Integer]: TNewStaticText; read;
  property SubCaptionLabel: TNewStaticText; read;
  property Values[Index: Integer]: String; read write;
  property IsSaveButton[Index: Integer]: Boolean; read write;
end;

TOutputMsgWizardPage = class(TWizardPage)
  property MsgLabel: TNewStaticText; read;
end;

TOutputMsgMemoWizardPage = class(TWizardPage)
  property RichEditViewer: TRichEditViewer; read;
  property SubCaptionLabel: TNewStaticText; read;
end;

TOutputProgressWizardPage = class(TWizardPage)
  procedure Hide;
  property Msg1Label: TNewStaticText; read;
  property Msg2Label: TNewStaticText; read;
  property ProgressBar: TNewProgressBar; read;
  procedure SetProgress(const Position, Max: Longint);
  procedure SetText(const Msg1, Msg2: String);
  procedure Show;
end;

TOutputMarqueeProgressWizardPage = class(TOutputProgressWizardPage)
  procedure Animate;
end;

TDownloadWizardPage = class(TOutputProgressWizardPage)
  property AbortButton: TNewButton; read;
  property AbortedByUser: Boolean; read;
  procedure Add(const Url, BaseName, RequiredSHA256OfFile: String);
  procedure AddEx(const Url, BaseName, RequiredSHA256OfFile, UserName, Password: String);
  procedure Clear;
  function Download: Int64;
  property ShowBaseNameInsteadOfUrl: Boolean; read write;
end;

TExtractionWizardPage = class(TOutputProgressWizardPage)
  property AbortButton: TNewButton; read;
  property AbortedByUser: Boolean; read;
  procedure Add(const ArchiveFileName, DestDir: String; const FullPaths: Boolean);
  procedure Clear;
  procedure Extract;
  property ShowArchiveInsteadOfFile: Boolean; read write;
end;

TUIStateForm = class(TForm)
end;

TSetupForm = class(TUIStateForm)
  function CalculateButtonWidth(const ButtonCaptions: array of String): Integer;
  function ShouldSizeX: Boolean;
  function ShouldSizeY: Boolean;
  procedure FlipSizeAndCenterIfNeeded(const ACenterInsideControl: Boolean; const CenterInsideControlCtl: TWinControl; const CenterInsideControlInsideClientArea: Boolean);
  property ControlsFlipped: Boolean; read;
  property FlipControlsOnShow: Boolean; read write;
  property KeepSizeY: Boolean; read; write;
  property RightToLeft: Boolean; read;
  property SizeAndCenterOnShow: Boolean; read write;
end;

TWizardForm = class(TSetupForm)
  property CancelButton: TNewButton; read;
  property NextButton: TNewButton; read;
  property BackButton: TNewButton; read;
  property OuterNotebook: TNotebook; read;
  property InnerNotebook: TNotebook; read;
  property WelcomePage: TNewNotebookPage; read;
  property InnerPage: TNewNotebookPage; read;
  property FinishedPage: TNewNotebookPage; read;
  property LicensePage: TNewNotebookPage; read;
  property PasswordPage: TNewNotebookPage; read;
  property InfoBeforePage: TNewNotebookPage; read;
  property UserInfoPage: TNewNotebookPage; read;
  property SelectDirPage: TNewNotebookPage; read;
  property SelectComponentsPage: TNewNotebookPage; read;
  property SelectProgramGroupPage: TNewNotebookPage; read;
  property SelectTasksPage: TNewNotebookPage; read;
  property ReadyPage: TNewNotebookPage; read;
  property PreparingPage: TNewNotebookPage; read;
  property InstallingPage: TNewNotebookPage; read;
  property InfoAfterPage: TNewNotebookPage; read;
  property DiskSpaceLabel: TNewStaticText; read;
  property DirEdit: TEdit; read;
  property GroupEdit: TNewEdit; read;
  property NoIconsCheck: TNewCheckBox; read;
  property PasswordLabel: TNewStaticText; read;
  property PasswordEdit: TPasswordEdit; read;
  property PasswordEditLabel: TNewStaticText; read;
  property ReadyMemo: TNewMemo; read;
  property TypesCombo: TNewComboBox; read;
  property Bevel: TBevel; read;
  property WizardBitmapImage: TBitmapImage; read;
  property WelcomeLabel1: TNewStaticText; read;
  property InfoBeforeMemo: TRichEditViewer; read;
  property InfoBeforeClickLabel: TNewStaticText; read;
  property MainPanel: TPanel; read;
  property Bevel1: TBevel; read;
  property PageNameLabel: TNewStaticText; read;
  property PageDescriptionLabel: TNewStaticText; read;
  property WizardSmallBitmapImage: TBitmapImage; read;
  property ReadyLabel: TNewStaticText; read;
  property FinishedLabel: TNewStaticText; read;
  property YesRadio: TNewRadioButton; read;
  property NoRadio: TNewRadioButton; read;
  property WizardBitmapImage2: TBitmapImage; read;
  property WelcomeLabel2: TNewStaticText; read;
  property LicenseLabel1: TNewStaticText; read;
  property LicenseMemo: TRichEditViewer; read;
  property InfoAfterMemo: TRichEditViewer; read;
  property InfoAfterClickLabel: TNewStaticText; read;
  property ComponentsList: TNewCheckListBox; read;
  property ComponentsDiskSpaceLabel: TNewStaticText; read;
  property BeveledLabel: TNewStaticText; read;
  property StatusLabel: TNewStaticText; read;
  property FilenameLabel: TNewStaticText; read;
  property ProgressGauge: TNewProgressBar; read;
  property SelectDirLabel: TNewStaticText; read;
  property SelectStartMenuFolderLabel: TNewStaticText; read;
  property SelectComponentsLabel: TNewStaticText; read;
  property SelectTasksLabel: TNewStaticText; read;
  property LicenseAcceptedRadio: TNewRadioButton; read;
  property LicenseNotAcceptedRadio: TNewRadioButton; read;
  property UserInfoNameLabel: TNewStaticText; read;
  property UserInfoNameEdit: TNewEdit; read;
  property UserInfoOrgLabel: TNewStaticText; read;
  property UserInfoOrgEdit: TNewEdit; read;
  property PreparingErrorBitmapImage: TBitmapImage; read;
  property PreparingLabel: TNewStaticText; read;
  property FinishedHeadingLabel: TNewStaticText; read;
  property UserInfoSerialLabel: TNewStaticText; read;
  property UserInfoSerialEdit: TNewEdit; read;
  property TasksList: TNewCheckListBox; read;
  property RunList: TNewCheckListBox; read;
  property DirBrowseButton: TNewButton; read;
  property GroupBrowseButton: TNewButton; read;
  property SelectDirBitmapImage: TBitmapImage; read;
  property SelectGroupBitmapImage: TBitmapImage; read;
  property SelectDirBrowseLabel: TNewStaticText; read;
  property SelectStartMenuFolderBrowseLabel: TNewStaticText; read;
  property PreparingYesRadio: TNewRadioButton; read;
  property PreparingNoRadio: TNewRadioButton; read;
  property PreparingMemo: TNewMemo; read;
  property CurPageID: Integer; read;
  function AdjustLabelHeight(ALabel: TNewStaticText): Integer;
  function AdjustLinkLabelHeight(ALinkLabel: TNewLinkLabel): Integer;
  procedure IncTopDecHeight(AControl: TControl; Amount: Integer);
  property PrevAppDir: String; read;
end;

TUninstallProgressForm = class(TSetupForm)
  property OuterNotebook: TNewNotebook; read;
  property InnerPage: TNewNotebookPage; read;
  property InnerNotebook: TNewNotebook; read;
  property InstallingPage: TNewNotebookPage; read;
  property MainPanel: TPanel; read;
  property PageNameLabel: TNewStaticText; read;
  property PageDescriptionLabel: TNewStaticText; read;
  property WizardSmallBitmapImage: TBitmapImage; read;
  property Bevel1: TBevel; read;
  property StatusLabel: TNewStaticText; read;
  property ProgressBar: TNewProgressBar; read;
  property BeveledLabel: TNewStaticText; read;
  property Bevel: TBevel; read;
  property CancelButton: TNewButton; read;
end;
