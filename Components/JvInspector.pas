{ This is not the original file: it has been modified for Inno Setup. }

{-----------------------------------------------------------------------------

 Project JEDI Visible Component Library (J-VCL)

 The contents of this file are subject to the Mozilla Public License Version
 1.1 (the "License"); you may not use this file except in compliance with the
 License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS IS" basis,
 WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 the specific language governing rights and limitations under the License.

 The Initial Developer of the Original Code is Marcel Bestebroer
  <jedi_mbe (at) users (dot) sf (dot) net>.
 Portions created by Marcel Bestebroer are Copyright (C) 2000 - 2002 mbeSoft.
 All Rights Reserved.

 ******************************************************************************

 Object Inspector like control which can inspect not only published
 properties, but also variables, string lists (can be parsed as INI files)
 anything you can think of (e.g. DataSet based or event based).

 You may retrieve the latest version of this file at the Project JEDI home
 page, located at http://www.delphi-jedi.org

-----------------------------------------------------------------------------}

unit JvInspector;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Contnrs, TypInfo,
  Windows, Messages, Graphics, Controls, StdCtrls, ExtCtrls,
  JvAutoComplete, JvInspectorSupport;

type
  // early declarations
  TJvCustomInspector = class;
  TJvInspectorPainter = class;
  TJvCustomInspectorItem = class;
  TJvInspectorCustomCategoryItem = class;
  TJvCustomInspectorData = class;
  TJvInspectorRegister = class;
  TJvCustomInspectorRegItem = class;
  TJvInspectorEventData = class;

  TInspectorItemFlag = (iifReadonly, iifHidden, iifExpanded,
    iifValueList, iifAllowNonListValues);
  TInspectorItemFlags = set of TInspectorItemFlag;

  TInspectorPaintRect = (iprItem, iprButtonArea, iprBtnSrcRect, iprBtnDstRect,
    iprNameArea, iprName, iprValueArea, iprValue, iprEditValue, iprEditButton);

  TJvInspectorItemClass = class of TJvCustomInspectorItem;

  TJvInspectorItemInstances = array of TJvCustomInspectorItem;

  TInspectorItemGetValueListEvent = procedure(Item: TJvCustomInspectorItem; Values: TStrings) of object;
  TJvInspAsOrdinal = procedure(Sender: TJvInspectorEventData; var Value: Int64) of object;
  TJvInspAsString = procedure(Sender: TJvInspectorEventData; var Value: string) of object;
  // new event types (sept 2004) -wp
  TInspectorBeforeEditEvent = procedure(Sender: TObject; Item: TJvCustomInspectorItem; Edit: TCustomEdit) of object;

  EJvInspector = class(EJVCLException);
  EJvInspectorItem = class(EJvInspector);
  EJvInspectorData = class(EJvInspector);
  EJvInspectorReg = class(EJvInspector);

  TOnJvInspectorSetItemColors = procedure(Item: TJvCustomInspectorItem; Canvas: TCanvas) of object;

  TJvCustomInspector = class(TJvCustomControl)
  private
    FDivider: Integer;
    FDraggingDivider: Boolean;
    FImageHeight: Integer;
    FItemHeight: Integer;
    FLockCount: Integer;
    FNeedRebuild: Boolean;
    FPainter: TJvInspectorPainter;
    FPaintGen: Integer;
    FReadOnly: Boolean;
    FRoot: TJvCustomInspectorItem;
    FSelectedIndex: Integer;
    FSelecting: Boolean;
    FTopIndex: Integer;
    FVisibleList: TStringList;
    FOnEditorKeyDown: TKeyEvent;
    // BeforeEdit NOTE: - WAP
    //
    // This event fired is when creating TEdit or TMemo objects, and
    // allows end users to customize the properties of the editor
    // objects, or hook event handlers, which were
    // otherwise invisible. This could be used to ill effect, so beware.
    FBeforeEdit: TInspectorBeforeEditEvent;
    FMouseWheelRecursion: Boolean;
  protected
    function CalcImageHeight: Integer; virtual;
    function CalcItemIndex(X, Y: Integer; var Rect: TRect): Integer; virtual;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure CMActivate(var Msg: TCMActivate); message CM_ACTIVATE;
    procedure CMDeactivate(var Msg: TCMActivate); message CM_DEACTIVATE;
    function GetDivider: Integer; virtual;
    function GetImageHeight: Integer; virtual;
    function GetItemHeight: Integer; virtual;
    function GetLastFullVisible: Integer; virtual;
    function GetLockCount: Integer; virtual;
    function GetPainter: TJvInspectorPainter; virtual;
    function GetReadOnly: Boolean; virtual;
    function GetRoot: TJvCustomInspectorItem; virtual;
    function GetSelected: TJvCustomInspectorItem; virtual;
    function GetSelectedIndex: Integer; virtual;
    function GetTopIndex: Integer; virtual;
    function GetVisibleCount: Integer; virtual;
    function GetVisibleItems(const I: Integer): TJvCustomInspectorItem; virtual;
    function IdxToY(const Index: Integer): Integer; virtual;
    procedure IncPaintGeneration; virtual;
    procedure InvalidateItem; virtual;
    procedure InvalidateList; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure RebuildVisible; virtual;
    procedure RemoveVisible(const Item: TJvCustomInspectorItem); virtual;
    procedure BoundsChanged; override;
    function ScrollFactorV: Extended; virtual;
    procedure SetDivider(Value: Integer); virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;
    procedure SetSelected(const Value: TJvCustomInspectorItem); virtual;
    procedure SetSelectedIndex(Value: Integer); virtual;
    procedure SetTopIndex(Value: Integer); virtual;
    procedure UpdateScrollBars; virtual;
    function ViewRect: TRect; virtual;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    procedure GetDlgCode(var Code: TDlgCodes); override;
    procedure FocusSet(PrevWnd: THandle); override;
    procedure FocusKilled(NextWnd: THandle); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure ShowScrollBars(Bar: Integer; Visible: Boolean); virtual;
    function YToIdx(const Y: Integer): Integer; virtual;
    property Divider: Integer read GetDivider write SetDivider;
    property DraggingDivider: Boolean read FDraggingDivider write FDraggingDivider;
    property ItemHeight: Integer read GetItemHeight;
    property ImageHeight: Integer read GetImageHeight;
    property LockCount: Integer read GetLockCount;
    property NeedRebuild: Boolean read FNeedRebuild write FNeedRebuild;
    property Painter: TJvInspectorPainter read GetPainter;
    property PaintGeneration: Integer read FPaintGen;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Root: TJvCustomInspectorItem read GetRoot;
    property Selected: TJvCustomInspectorItem read GetSelected;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
    property Selecting: Boolean read FSelecting write FSelecting;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property VisibleCount: Integer read GetVisibleCount;
    property VisibleItems[const I: Integer]: TJvCustomInspectorItem read GetVisibleItems;
    property BeforeEdit: TInspectorBeforeEditEvent read FBeforeEdit write FBeforeEdit;
    { Standard TCustomControl event - this is really an event fired by
      the TEdit control used when editing in a cell!}
    property OnEditorKeyDown: TKeyEvent read FOnEditorKeyDown write FOnEditorKeyDown;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeforeDestruction; override;
    function BeginUpdate: Integer; virtual;
    function EndUpdate: Integer; virtual;
    function Focused: Boolean; override;
    function VisibleIndex(const AItem: TJvCustomInspectorItem): Integer; virtual;
    procedure RefreshValues;
    procedure Clear;
  end;

  TJvInspector = class(TJvCustomInspector)
  public
    property BevelKind;
    property Divider;
    property Painter;
    property ReadOnly;
    property Root;
    property Selected;
    property BeforeEdit; // Low level hook for customizing TEdit/TMemo after objects are created, just before editing.

    // Standard control event
    property OnKeyDown;

    // Redirected editor event
    property OnEditorKeyDown;
  end;

  TJvInspectorPainter = class(TJvComponent)
  private
    FBackgroundColor: TColor;
    FButtonImage: TBitmap;
    FCanvas: TCanvas;
    FCategoryColor: TColor;
    FCategoryDividerColor: TColor;
    FDividerColor: TColor;
    FInitializing: Boolean;
    FInspector: TJvCustomInspector;
    FInternalButtonBackgroundColor: TColor;
    FInternalButtonPenColor: TColor;
    FInternalButtonSize: Integer;
    FInternalCollapseButton: TBitmap;
    FInternalExpandButton: TBitmap;
    FItem: TJvCustomInspectorItem;
    FItemIndex: Integer;
    FPaintRect: TRect;
    FSelectedColor: TColor;
    FCategoryTextColor: TColor;
    FValueColor: TColor;
    FNameColor: TColor;
    FSelectedTextColor: TColor;
  protected
    procedure ApplyNameFont; virtual;
    procedure ApplyValueFont; virtual;
    procedure CalcButtonBasedRects; virtual;
    procedure CalcEditBasedRects; virtual;
    procedure CalcNameBasedRects; virtual;
    procedure CalcValueBasedRects; virtual;
    function DividerWidth: Integer; virtual;
    procedure DoPaint; virtual;
    function GetBackgroundColor: TColor; virtual;
    function GetCategoryColor: TColor; virtual;
    function GetCategoryDividerColor: TColor; virtual;
    function GetCategoryTextColor: TColor; virtual;
    function GetHideSelectTextColor: TColor; virtual;
    function GetNameColor: TColor; virtual;
    function GetSelectedTextColor: TColor; virtual;
    function GetValueColor: TColor; virtual;
    function GetCollapseImage: TBitmap; virtual;
    function GetDividerColor: TColor; virtual;
    function GetExpandImage: TBitmap; virtual;
    function GetHideSelectColor: TColor; virtual;
    function GetRects(const Index: TInspectorPaintRect): TRect; virtual;
    function GetSelectedColor: TColor; virtual;
    procedure HideEditor; virtual;
    procedure InitializeColors; virtual;
    procedure Paint; virtual;
    procedure PaintDivider(const X, YTop, YBottom: Integer); virtual;
    procedure PaintItem(var ARect: TRect; const AItemIndex: Integer); virtual;
    procedure PrepareInternalImages; virtual;
    procedure SetBackgroundColor(const Value: TColor); virtual;
    procedure SetCategoryColor(const Value: TColor); virtual;
    procedure SetCategoryDividerColor(const Value: TColor); virtual;
    procedure SetCategoryTextColor(const Value: TColor); virtual;
    procedure SetDividerColor(const Value: TColor); virtual;
    procedure SetHideSelectColor(const Value: TColor); virtual;
    procedure SetHideSelectTextColor(const Value: TColor); virtual;
    procedure SetNameColor(const Value: TColor); virtual;
    procedure SetRects(const Index: TInspectorPaintRect; const ARect: TRect); virtual;
    procedure SetSelectedColor(const Value: TColor); virtual;
    procedure SetSelectedTextColor(const Value: TColor); virtual;
    procedure Setup(const ACanvas: TCanvas); virtual;
    procedure SetupItem; virtual;
    procedure SetupRects; virtual;
    procedure SetValueColor(const Value: TColor); virtual;
    procedure TeardownItem; virtual;
    property ButtonImage: TBitmap read FButtonImage write FButtonImage;
    property Canvas: TCanvas read FCanvas write FCanvas;
    property Initializing: Boolean read FInitializing write FInitializing;
    property Inspector: TJvCustomInspector read FInspector;
    property Item: TJvCustomInspectorItem read FItem write FItem;
    property ItemIndex: Integer read FItemIndex write FItemIndex;
    property PaintRect: TRect read FPaintRect write FPaintRect;
    property Rects[const Index: TInspectorPaintRect]: TRect read GetRects write SetRects;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetInspector(const AInspector: TJvCustomInspector); virtual;
    property HideSelectColor: TColor read GetHideSelectColor write SetHideSelectColor;
    property HideSelectTextColor: TColor read GetHideSelectTextColor write SetHideSelectTextColor;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
    property SelectedTextColor: TColor read GetSelectedTextColor write SetSelectedTextColor;
  published
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property CategoryColor: TColor read GetCategoryColor write SetCategoryColor;
    property CategoryDividerColor: TColor read GetCategoryDividerColor write SetCategoryDividerColor;
    property CategoryTextColor: TColor read GetCategoryTextColor write SetCategoryTextColor;
    property DividerColor: TColor read GetDividerColor write SetDividerColor;
    property NameColor: TColor read GetNameColor write SetNameColor;
    property ValueColor: TColor read GetValueColor write SetValueColor;
  end;

  TJvInspectorBorlandNETBasePainter = class(TJvInspectorPainter)
  private
    FRealButtonAreaWidth: Integer;
  protected
    procedure ApplyNameFont; override;
    procedure CalcButtonBasedRects; override;
    procedure CalcEditBasedRects; override;
    procedure CalcNameBasedRects; override;
    procedure CalcValueBasedRects; override;
    procedure SetupRects; override;
    procedure InitializeColors; override;
    property RealButtonAreaWidth: Integer read FRealButtonAreaWidth write FRealButtonAreaWidth;
  published
    property BackgroundColor default clWindow;
    property CategoryColor default clBtnFace;
  end;

  TJvInspectorDotNETPainter = class(TJvInspectorBorlandNETBasePainter)
  private
    FHideSelectColor: TColor;
    FHideSelectTextColor: TColor;
    FOnSetItemColors: TOnJvInspectorSetItemColors;
  protected
    procedure ApplyNameFont; override;
    function GetHideSelectColor: TColor; override;
    function GetHideSelectTextColor: TColor; override;
    procedure DoPaint; override;
    procedure InitializeColors; override;
    procedure PaintDivider(const X, YTop, YBottom: Integer); override;
    procedure SetHideSelectColor(const Value: TColor); override;
    procedure SetHideSelectTextColor(const Value: TColor); override;
  published
    property CategoryDividerColor default clBtnShadow;
    property DividerColor default clBtnFace;
    property HideSelectColor default clBtnFace;
    property HideSelectTextColor;
    property SelectedColor default clHighlight;
    property SelectedTextColor;
    property OnSetItemColors: TOnJvInspectorSetItemColors read FOnSetItemColors write FOnSetItemColors;
  end;

  TJvCustomInspectorItem = class(TPersistent)
  private
    FData: TJvCustomInspectorData;
    FDisplayName: string;
    FDroppedDown: Boolean;
    FEditCtrlDestroying: Boolean;
    FEditCtrl: TCustomEdit;
    FEditWndPrc: TWndMethod;
    FEditing: Boolean;
    FAutoComplete: TJvEditListBoxAutoComplete;
    FFlags: TInspectorItemFlags;
    FInspector: TJvCustomInspector;
    FItems: TObjectList;
    FListBox: TCustomListBox;
    FOnGetValueList: TInspectorItemGetValueListEvent;
    FParent: TJvCustomInspectorItem;
    FLastPaintGen: Integer;
    FPressed: Boolean;
    FRects: array [TInspectorPaintRect] of TRect;
    FTracking: Boolean;
    FUpdateEditCtrl: Integer; // Used to prevent EditCtrl destruction while in Apply().
  protected
    procedure Apply; virtual;
    function CanEdit: Boolean; virtual;
    procedure CloseUp(Accept: Boolean); virtual;
    procedure Deactivate; dynamic;
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState); virtual;
    procedure DoGetValueList(const Strings: TStrings); virtual;
    procedure DropDown; dynamic;
    procedure EditFocusLost(Sender: TObject); dynamic;
    procedure EditKillFocus(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char); dynamic;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); dynamic;
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure EditMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure Edit_WndProc(var Msg: TMessage); virtual;
    procedure AutoCompleteStart(Sender: TObject); dynamic;
    function GetBaseCategory: TJvCustomInspectorItem; virtual;
    function GetCategory: TJvCustomInspectorItem; virtual;
    function GetCount: Integer; virtual;
    function GetData: TJvCustomInspectorData; virtual;
    function GetDisplayName: string; virtual;
    function GetDisplayValue: string; virtual;
    function GetDroppedDown: Boolean; virtual;
    function GetEditCtrl: TCustomEdit; virtual;
    function GetEditCtrlDestroying: Boolean; virtual;
    function GetEditing: Boolean; virtual;
    function GetExpanded: Boolean; virtual;
    function GetFlags: TInspectorItemFlags; virtual;
    function GetHeight: Integer; virtual;
    function GetHidden: Boolean; virtual;
    function GetInspector: TJvCustomInspector; virtual;
    function GetInspectorPaintGeneration: Integer;
    function GetItems(const I: Integer): TJvCustomInspectorItem; virtual;
    function GetLevel: Integer; virtual;
    function GetListBox: TCustomListBox; virtual;
    function GetNextSibling: TJvCustomInspectorItem; virtual;
    function GetParent: TJvCustomInspectorItem; virtual;
    function GetReadOnly: Boolean; virtual;
    function GetRects(const RectKind: TInspectorPaintRect): TRect; virtual;
    procedure GetValueList(const Strings: TStrings); virtual;
    procedure InvalidateItem; virtual;
    procedure InvalidateList; virtual;
    procedure InvalidateMetaData; virtual;
    function IsCategory: Boolean; virtual;
    procedure ListExit(Sender: TObject); virtual;
    procedure ListValueSelect(Sender: TObject); virtual;
    procedure ListDeactivate(Sender: TObject); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure SelectValue(const Delta: Integer); virtual;
    procedure SetDisplayName(Value: string); virtual;
    procedure SetDisplayValue(const Value: string); virtual;
    procedure SetEditCtrl(const Value: TCustomEdit); virtual;
    procedure SetEditing(const Value: Boolean); virtual;
    procedure SetExpanded(Value: Boolean); virtual;
    procedure SetFlags(const Value: TInspectorItemFlags); virtual;
    procedure SetFocus; virtual;
    procedure SetInspector(const AInspector: TJvCustomInspector); virtual;
    procedure SetParent(const Value: TJvCustomInspectorItem); virtual;
    procedure SetRects(const RectKind: TInspectorPaintRect; Value: TRect); virtual;
    procedure StopTracking; dynamic;
    procedure TrackButton(X, Y: Integer); dynamic;
    procedure Undo; virtual;
    procedure UpdateLastPaintGeneration;
    property BaseCategory: TJvCustomInspectorItem read GetBaseCategory;
    property Category: TJvCustomInspectorItem read GetCategory;
    property DroppedDown: Boolean read GetDroppedDown;
    property EditCtrlDestroying: Boolean read GetEditCtrlDestroying;
    property EditCtrl: TCustomEdit read GetEditCtrl;
    property EditWndPrc: TWndMethod read FEditWndPrc;
    property LastPaintGeneration: Integer read FLastPaintGen;
    property ListBox: TCustomListBox read GetListBox;
    property Pressed: Boolean read FPressed write FPressed;
    property Tracking: Boolean read FTracking write FTracking;
  public
    constructor Create(const AParent: TJvCustomInspectorItem; const AData: TJvCustomInspectorData); virtual;
    destructor Destroy; override;
    function Add(const Item: TJvCustomInspectorItem): Integer;
    procedure BeforeDestruction; override;
    procedure Clear;
    procedure Delete(const Index: Integer); overload; virtual;
    procedure DrawEditor(const ACanvas: TCanvas); virtual;
    procedure DrawName(const ACanvas: TCanvas); virtual;
    procedure DrawValue(const ACanvas: TCanvas); virtual;
    function EditFocused: Boolean; dynamic;
    procedure ExpandItems(AExpand: Boolean);
    function HasViewableItems: Boolean; virtual;
    function IndexOf(const Item: TJvCustomInspectorItem): Integer; overload; virtual;
    procedure InitEdit; dynamic;
    procedure DoneEdit(const CancelEdits: Boolean = False); dynamic;
    procedure Insert(const Index: Integer; const Item: TJvCustomInspectorItem);
    procedure ScrollInView;
    property Count: Integer read GetCount;
    property Data: TJvCustomInspectorData read GetData;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property DisplayValue: string read GetDisplayValue write SetDisplayValue;
    property Editing: Boolean read GetEditing;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property Flags: TInspectorItemFlags read GetFlags write SetFlags;
    property Hidden: Boolean read GetHidden;
    property Height: Integer read GetHeight;
    property Inspector: TJvCustomInspector read GetInspector;
    property Items[const I: Integer]: TJvCustomInspectorItem read GetItems; default;
    property Level: Integer read GetLevel;
    property Parent: TJvCustomInspectorItem read GetParent;
    property ReadOnly: Boolean read GetReadOnly;
    property Rects[const RectKind: TInspectorPaintRect]: TRect read GetRects write SetRects;
    property OnGetValueList: TInspectorItemGetValueListEvent read FOnGetValueList write FOnGetValueList;
  end;

  TJvInspectorCustomCategoryItem = class(TJvCustomInspectorItem)
  protected
    function IsCategory: Boolean; override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  end;

  TJvInspectorEnumItem = class(TJvCustomInspectorItem)
  protected
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  end;

  TJvInspectorStringItem = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
  end;

  TJvInspectorBooleanItem = class(TJvInspectorEnumItem)
  private
    FCheckRect: TRect;
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    procedure DoneEdit(const CancelEdits: Boolean = False); override;
    procedure DrawValue(const ACanvas: TCanvas); override;
    procedure InitEdit; override;
  end;

  TJvCustomInspectorData = class(TPersistent)
  private
    FTypeInfo: PTypeInfo;
    FItems: TJvInspectorItemInstances;
    FName: string;
  protected
    constructor CreatePrim(const AName: string; ATypeInfo: PTypeInfo);
    procedure CheckReadAccess; virtual;
    procedure CheckWriteAccess; virtual;
    function GetAsOrdinal: Int64; virtual; abstract;
    function GetAsString: string; virtual; abstract;
    function GetItems(I: Integer): TJvCustomInspectorItem;
    function GetName: string; virtual;
    function GetTypeInfo: PTypeInfo; virtual;
    procedure Invalidate; virtual;
    procedure RemoveItem(const Item: TJvCustomInspectorItem);
    procedure SetAsOrdinal(const Value: Int64); virtual; abstract;
    procedure SetAsString(const Value: string); virtual; abstract;
    procedure SetName(const Value: string); virtual;
    procedure SetTypeInfo(Value: PTypeInfo); virtual;
  public
    procedure BeforeDestruction; override;
    function HasValue: Boolean; virtual; abstract;
    function IsAssigned: Boolean; virtual; abstract;
    function IsInitialized: Boolean; virtual; abstract;
    class function ItemRegister: TJvInspectorRegister; virtual;
    function NewItem(const AParent: TJvCustomInspectorItem): TJvCustomInspectorItem; virtual;
    property AsOrdinal: Int64 read GetAsOrdinal write SetAsOrdinal;
    property AsString: string read GetAsString write SetAsString;
    property Items[I: Integer]: TJvCustomInspectorItem read GetItems;
    property Name: string read GetName write SetName;
    property TypeInfo: PTypeInfo read GetTypeInfo write SetTypeInfo;
  end;

  TJvInspectorEventData = class(TJvCustomInspectorData)
  private
    FOnGetAsOrdinal: TJvInspAsOrdinal;
    FOnGetAsString: TJvInspAsString;
    FOnSetAsOrdinal: TJvInspAsOrdinal;
    FOnSetAsString: TJvInspAsString;

    FParent: TJvCustomInspectorItem;
  protected
    function DoGetAsOrdinal: Int64;
    function DoGetAsString: string;
    procedure DoSetAsOrdinal(Value: Int64);
    procedure DoSetAsString(Value: string);
    function GetAsOrdinal: Int64; override;
    function GetAsString: string; override;
    procedure SetAsOrdinal(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetOnGetAsOrdinal(Value: TJvInspAsOrdinal);
    procedure SetOnGetAsString(Value: TJvInspAsString);
    procedure SetOnSetAsOrdinal(Value: TJvInspAsOrdinal);
    procedure SetOnSetAsString(Value: TJvInspAsString);
  public
    function HasValue: Boolean; override;
    function IsAssigned: Boolean; override;
    function IsInitialized: Boolean; override;
    class function New(const AParent: TJvCustomInspectorItem; const AName: string; ATypeInfo: PTypeInfo):
      TJvCustomInspectorItem;
    property OnGetAsOrdinal: TJvInspAsOrdinal read FOnGetAsOrdinal write SetOnGetAsOrdinal;
    property OnGetAsString: TJvInspAsString read FOnGetAsString write SetOnGetAsString;
    property OnSetAsOrdinal: TJvInspAsOrdinal read FOnSetAsOrdinal write SetOnSetAsOrdinal;
    property OnSetAsString: TJvInspAsString read FOnSetAsString write SetOnSetAsString;
  end;

  TJvInspectorRegister = class(TPersistent)
  private
    FItems: TObjectList;
  protected
    function GetCount: Integer;
    function GetItems(const I: Integer): TJvCustomInspectorRegItem; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const RegItem: TJvCustomInspectorRegItem);
    function FindMatch(const ADataObj: TJvCustomInspectorData): TJvCustomInspectorRegItem;
    property Count: Integer read GetCount;
    property Items[const I: Integer]: TJvCustomInspectorRegItem read GetItems;
  end;

  TJvCustomInspectorRegItem = class(TPersistent)
  private
    FItemClass: TJvInspectorItemClass;
  protected
    function GetItemClass: TJvInspectorItemClass; virtual;
  public
    constructor Create(const AItemClass: TJvInspectorItemClass);
    function IsMatch(const ADataObj: TJvCustomInspectorData): Boolean; virtual;
    function MatchValue(const ADataObj: TJvCustomInspectorData): Integer; virtual; abstract;
    property ItemClass: TJvInspectorItemClass read GetItemClass;
  end;

  TJvInspectorTypeInfoRegItem = class(TJvCustomInspectorRegItem)
  private
    FTypeInfo: PTypeInfo;
  protected
    function GetTypeInfo: PTypeInfo; virtual;
  public
    constructor Create(const AItemClass: TJvInspectorItemClass; ATypeInfo: PTypeInfo);
    function MatchValue(const ADataObj: TJvCustomInspectorData): Integer; override;
    property TypeInfo: PTypeInfo read GetTypeInfo;
  end;

  TJvInspectorTypeKindRegItem = class(TJvCustomInspectorRegItem)
  private
    FTypeKind: TTypeKind;
  protected
    function GetTypeKind: TTypeKind; virtual;
  public
    constructor Create(const AItemClass: TJvInspectorItemClass;
      const ATypeKind: TTypeKind);
    function MatchValue(const ADataObj: TJvCustomInspectorData): Integer; override;
    property TypeKind: TTypeKind read GetTypeKind;
  end;

// (rom) centralized the string literals
const
  cJvInspectorOrdinal = 'Ordinal';
  cJvInspectorString = 'string';

// Canvas State functions used by TJvInspectorPainter & its descendents
function SaveCanvasState(const Canvas: TCanvas): Integer;
procedure ApplyCanvasState(const Canvas: TCanvas; const SavedIdx: Integer);
procedure RestoreCanvasState(const Canvas: TCanvas; const SavedIdx: Integer);

implementation

uses
  System.UITypes,
  RTLConsts, Types, Consts, Forms;

//============================================================================

type
  TCustomEditAccessProtected = class(TCustomEdit);

var
  GlobalGenItemReg: TJvInspectorRegister = nil;

procedure RegisterDataTypeKinds; forward;

//=== { TCanvasStack } =======================================================

type
  TCanvasStack = class(TObjectList)
  private
    FTop: Integer;
    procedure SetCapacity(const Value: {$IFDEF RTL360_UP}NativeInt{$ELSE}Integer{$ENDIF RTL360_UP});
  public
    constructor Create(const ACapacity: Integer);
    function Push(const Canvas: TCanvas): Integer;
    procedure Pop(const Canvas: TCanvas; Index: Integer);
    property Capacity write SetCapacity;
  end;

  TCanvasState = class(TPersistent)
  private
    FBrush: TBrush;
    FPen: TPen;
    FFont: TFont;
  public
    constructor Create(const Canvas: TCanvas);
    destructor Destroy; override;
    procedure ApplyTo(const Canvas: TCanvas);
    procedure SetState(const Canvas: TCanvas);
  end;

var
  GlobalCanvasStack: TCanvasStack = nil;

constructor TCanvasStack.Create(const ACapacity: Integer);
begin
  inherited Create(True);
  FTop := -1;
  Capacity := ACapacity;
end;

procedure TCanvasStack.SetCapacity(const Value: {$IFDEF RTL360_UP}NativeInt{$ELSE}Integer{$ENDIF RTL360_UP});
var
  I: Integer;
begin
  if Capacity <> Value then
  begin
    if Value < Capacity then
    begin
      inherited Capacity := Value;
      if FTop >= Capacity then
        FTop := Capacity - 1;
    end
    else
    begin
      I := Capacity;
      inherited Capacity := Value;
      for I := I to Value - 1 do
        Add(TCanvasState.Create(nil));
    end;
  end;
end;

function TCanvasStack.Push(const Canvas: TCanvas): Integer;
begin
  Inc(FTop);
  if FTop >= Capacity then
    Capacity := Capacity + 128;
  Result := FTop;
  TCanvasState(Items[Result]).SetState(Canvas);
end;

procedure TCanvasStack.Pop(const Canvas: TCanvas; Index: Integer);
begin
  TCanvasState(Items[Index]).ApplyTo(Canvas);
  FTop := Pred(Index);
end;

//=== { TCanvasState } =======================================================

constructor TCanvasState.Create(const Canvas: TCanvas);
begin
  inherited Create;
  FBrush := TBrush.Create;
  FPen := TPen.Create;
  FFont := TFont.Create;
  if Canvas <> nil then
    SetState(Canvas);
end;

destructor TCanvasState.Destroy;
begin
  FFont.Free;
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TCanvasState.ApplyTo(const Canvas: TCanvas);
begin
  Canvas.Brush.Assign(FBrush);
  Canvas.Pen.Assign(FPen);
  Canvas.Font.Assign(FFont);
end;

procedure TCanvasState.SetState(const Canvas: TCanvas);
begin
  FBrush.Assign(Canvas.Brush);
  FPen.Assign(Canvas.Pen);
  FFont.Assign(Canvas.Font);
end;

function CanvasStack: TCanvasStack;
begin
  if GlobalCanvasStack = nil then
    GlobalCanvasStack := TCanvasStack.Create(512);
  Result := GlobalCanvasStack;
end;

function SaveCanvasState(const Canvas: TCanvas): Integer;
begin
  Result := CanvasStack.Push(Canvas);
end;

procedure ApplyCanvasState(const Canvas: TCanvas; const SavedIdx: Integer);
begin
  TCanvasState(CanvasStack[SavedIdx]).ApplyTo(Canvas);
end;

procedure RestoreCanvasState(const Canvas: TCanvas; const SavedIdx: Integer);
begin
  CanvasStack.Pop(Canvas, SavedIdx);
end;

procedure SetDefaultProp(const Instance: TObject; const PropName: string); overload;
var
  Prop: PPropInfo;
begin
  Prop := GetPropInfo(Instance, PropName);
  if (Prop <> nil) and (Prop.Default <> Low(Integer)) then
    SetOrdProp(Instance, Prop, Prop.Default);
end;

procedure SetDefaultProp(const Instance: TObject; const PropNames: array of string); overload;
var
  I: Integer;
begin
  for I := Low(PropNames) to High(PropNames) do
    SetDefaultProp(Instance, PropNames[I]);
end;

//=== { TInspReg } ===========================================================

type
  TInspReg = class(TObject)
  private
    FInspectors: array of TJvCustomInspector;
  protected
    function ApplicationDeactivate(var Msg: TMessage): Boolean;
    function IndexOf(const Inspector: TJvCustomInspector): Integer;
  public
    procedure RegInspector(const Inspector: TJvCustomInspector);
    procedure UnRegInspector(const Inspector: TJvCustomInspector);
  end;

var
  FieldGlobalInspReg: TInspReg = nil;

function GlobalInspReg: TInspReg;
begin
  if not Assigned(FieldGlobalInspReg) then
    FieldGlobalInspReg := TInspReg.Create;
  Result := FieldGlobalInspReg;
end;

function TInspReg.ApplicationDeactivate(var Msg: TMessage): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Msg.Msg = CM_ACTIVATE) or (Msg.Msg = CM_DEACTIVATE) then
    // Post the CM_(DE)ACTIVATE message to all registered inspectors
    for I := High(FInspectors) downto 0 do
      if FInspectors[I].HandleAllocated then
        PostMessage(FInspectors[I].Handle, Msg.Msg, 0, 0);
end;

function TInspReg.IndexOf(const Inspector: TJvCustomInspector): Integer;
begin
  Result := High(FInspectors);
  while (Result >= 0) and (FInspectors[Result] <> Inspector) do
    Dec(Result);
end;

procedure TInspReg.RegInspector(const Inspector: TJvCustomInspector);
begin
  if IndexOf(Inspector) = -1 then
  begin
    SetLength(FInspectors, Length(FInspectors) + 1);
    FInspectors[High(FInspectors)] := Inspector;
    if Length(FInspectors) = 1 then
      Application.HookMainWindow(ApplicationDeactivate);
  end;
end;

procedure TInspReg.UnRegInspector(const Inspector: TJvCustomInspector);
var
  I: Integer;
begin
  I := IndexOf(Inspector);
  if I <> -1 then
  begin
    if I < High(FInspectors) then
      Move(FInspectors[I + 1], FInspectors[I], (High(FInspectors) - I) * SizeOf(TJvCustomInspector));
    SetLength(FInspectors, High(FInspectors));
    if Length(FInspectors) = 0 then
      Application.UnhookMainWindow(ApplicationDeactivate);
  end;
end;

//=== { TJvCustomInspector } =================================================

constructor TJvCustomInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FItemHeight := 18;
  DoubleBuffered := True;
  FVisibleList := TStringList.Create;
  FRoot := TJvCustomInspectorItem.Create(nil, nil);
  Root.SetInspector(Self);
  Root.Flags := [iifHidden, iifExpanded, iifReadonly];
  FSelectedIndex := -1;
  BevelKind := bkTile;
  BevelInner := bvNone;
  BevelOuter := bvLowered;
  TabStop := True;
  Width := 300;
  Height := 100;
  Divider := 75;

  FPainter := TJvInspectorDotNETPainter.Create(Self);
  FPainter.SetInspector(Self);

  GlobalInspReg.RegInspector(Self);
end;

function TJvCustomInspector.CalcImageHeight: Integer;
var
  I: Integer;
begin
  FImageHeight := 0;
  for I := 0 to Pred(VisibleCount) do
    Inc(FImageHeight, VisibleItems[I].Height);
  Result := FImageHeight;
end;

function TJvCustomInspector.CalcItemIndex(X, Y: Integer; var Rect: TRect): Integer;
var
  MaxIdx: Integer;
begin
  Result := TopIndex;
  MaxIdx := VisibleCount;
  while (Result <> -1) and (Result < MaxIdx) and not PtInRect(VisibleItems[Result].Rects[iprItem], Point(X, Y)) do
    Inc(Result);
  if Result >= MaxIdx then
    Result := -1;
  if Result > -1 then
    Rect := VisibleItems[Result].Rects[iprItem];
end;

procedure TJvCustomInspector.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited ChangeScale(M, D, isDpiChange);
  if M <> D then
  begin
    // ItemHeight needs no scaling here: it stores a 96 dpi value which
    // GetItemHeight scales on read
    FDivider := MulDiv(FDivider, M, D);
  end;
end;

procedure TJvCustomInspector.CMActivate(var Msg: TCMActivate);
begin
  Invalidate;
end;

procedure TJvCustomInspector.CMDeactivate(var Msg: TCMActivate);
begin
  inherited;
  if Selected <> nil then
    Selected.Deactivate;
  Invalidate;
end;

function TJvCustomInspector.GetDivider: Integer;
begin
  Result := FDivider;
end;

function TJvCustomInspector.GetImageHeight: Integer;
begin
  if FImageHeight = 0 then
    CalcImageHeight;
  Result := FImageHeight;
end;

function TJvCustomInspector.GetItemHeight: Integer;
begin
  // FItemHeight holds a value for 96 dpi; scale it to the current dpi
  Result := MulDiv(FItemHeight, CurrentPPI, 96);
end;

function TJvCustomInspector.GetLastFullVisible: Integer;
begin
  Result := YToIdx(IdxToY(TopIndex) + Pred(ClientHeight));
  if Result < 0 then
    Result := Pred(VisibleCount)
  else
    while (IdxToY(Result) + VisibleItems[Result].Height) > ClientHeight do
      Dec(Result);
end;

function TJvCustomInspector.GetLockCount: Integer;
begin
  Result := FLockCount;
end;

function TJvCustomInspector.GetRoot: TJvCustomInspectorItem;
begin
  Result := FRoot;
end;

function TJvCustomInspector.GetPainter: TJvInspectorPainter;
begin
  Result := FPainter;
end;

function TJvCustomInspector.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TJvCustomInspector.GetSelected: TJvCustomInspectorItem;
begin
  if (SelectedIndex > -1) and (SelectedIndex < VisibleCount) then
    Result := VisibleItems[SelectedIndex]
  else
    Result := nil;
end;

function TJvCustomInspector.GetSelectedIndex: Integer;
begin
  Result := FSelectedIndex;
end;

function TJvCustomInspector.GetTopIndex: Integer;
begin
  Result := FTopIndex;
end;

function TJvCustomInspector.GetVisibleCount: Integer;
begin
  Result := FVisibleList.Count;
end;

function TJvCustomInspector.GetVisibleItems(const I: Integer): TJvCustomInspectorItem;
begin
  if (I < 0) or (I >= FVisibleList.Count) then
    Result := nil
  else
    Result := TJvCustomInspectorItem(FVisibleList.Objects[I]);
end;

function TJvCustomInspector.IdxToY(const Index: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Pred(Index) do
    if VisibleItems[I] <> nil then
      Inc(Result, VisibleItems[I].Height);
end;

procedure TJvCustomInspector.IncPaintGeneration;
begin
  Inc(FPaintGen);
end;

procedure TJvCustomInspector.InvalidateItem;
begin
  if (LockCount = 0) and HandleAllocated then
    UpdateScrollBars;
end;

procedure TJvCustomInspector.InvalidateList;
begin
  if not (csDestroying in ComponentState) and (LockCount = 0) then
  begin
    if HandleAllocated then
    begin
      RebuildVisible;
      UpdateScrollBars;
    end
    else
      NeedRebuild := True;
  end
  else
    NeedRebuild := True;
end;

procedure TJvCustomInspector.KeyDown(var Key: Word; Shift: TShiftState);
var
  Item: TJvCustomInspectorItem;
  IgnoreKey: Boolean;
  TmpH: Integer;
  TmpIdx: Integer;
begin
  Item := Selected;
  if Shift = [] then
  begin
    IgnoreKey := True;
    case Key of
      VK_UP:
        if SelectedIndex > 0 then
          SelectedIndex := SelectedIndex - 1;
      VK_DOWN:
        if SelectedIndex < Pred(VisibleCount) then
          SelectedIndex := SelectedIndex + 1;
      VK_LEFT:
        if SelectedIndex > 0 then
          SelectedIndex := SelectedIndex - 1;
      VK_RIGHT:
        if SelectedIndex < Pred(VisibleCount) then
          SelectedIndex := SelectedIndex + 1;
      VK_PRIOR:
        begin
          if SelectedIndex > TopIndex then
            SelectedIndex := TopIndex
          else
          if SelectedIndex > 0 then
          begin
            TmpH := VisibleItems[Pred(SelectedIndex)].Height;
            TmpIdx := YToIdx(IdxToY(SelectedIndex) + TmpH - ClientHeight);
            if TmpIdx < 0 then
              TmpIdx := 0;
            SelectedIndex := TmpIdx;
          end;
        end;
      VK_NEXT:
        begin
          TmpIdx := GetLastFullVisible;
          if SelectedIndex < TmpIdx then
            SelectedIndex := TmpIdx
          else
          if SelectedIndex < Pred(VisibleCount) then
          begin
            TmpH := VisibleItems[SelectedIndex].Height;
            TmpIdx := YToIdx(IdxToY(SelectedIndex) + TmpH + ClientHeight);
            if TmpIdx < 0 then
              TmpIdx := Pred(VisibleCount);
            SelectedIndex := TmpIdx;
          end;
        end;
      VK_ADD:
        if Item.HasViewableItems and not Item.Expanded then
          Item.Expanded := True;
      VK_SUBTRACT:
        if Item.Expanded then
          Item.Expanded := False;
    else
      IgnoreKey := False;
    end;
    if IgnoreKey then
      Key := 0;
  end
  else
  if Shift = [ssCtrl] then
  begin
    IgnoreKey := True;
    case Key of
      VK_RIGHT:
        if Item.HasViewableItems and not Item.Expanded then
          Item.Expanded := True;
      VK_LEFT:
        if Item.Expanded then
          Item.Expanded := False;
      VK_RETURN:
        if Item.HasViewableItems and not Item.Expanded then
          Item.Expanded := True
        else
        if Item.Expanded then
          Item.Expanded := False;
    else
      IgnoreKey := False;
    end;
    if IgnoreKey then
      Key := 0;
  end;
  inherited KeyDown(Key, Shift);
  if (SelectedIndex >= 0) and (SelectedIndex < VisibleCount) then
  begin
    Item := Selected;
    if (Item <> nil) and Item.Editing then
    begin
      Item.ScrollInView;
      Item.EditKeyDown(Self, Key, Shift);
    end;
  end;
end;

procedure TJvCustomInspector.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if (Shift = []) and ((Key = VK_DOWN) or (Key = VK_UP) or (Key = VK_ADD) or
    (Key = VK_SUBTRACT) or (Key = VK_PRIOR) or (Key = VK_NEXT)) then
    Key := 0;
end;

procedure TJvCustomInspector.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  XB: Integer;
  ItemIndex: Integer;
  ItemRect: TRect;
  Item: TJvCustomInspectorItem;
  CharPos: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  XB := X;
  ItemIndex := CalcItemIndex(X, Y, ItemRect);
  if (ItemIndex < VisibleCount) and (ItemIndex >= 0) then
    Item := VisibleItems[ItemIndex]
  else
    Item := nil;
  if not Focused and ((Item = nil) or (not Item.Editing)) then
    SetFocus
  else
  if (Item <> nil) and Item.Editing then
    Item.SetFocus;
  if Button = mbLeft then
  begin
    // Check divider dragging
    if (XB >= Pred(Divider)) and (XB <= Succ(Divider)) then
      DraggingDivider := True
    // Check selecting
    else
    if (Item <> nil) and (ItemIndex <> SelectedIndex) then
    begin
      SelectedIndex := ItemIndex;
      if ItemIndex >= 0 then
        Item := VisibleItems[ItemIndex];
    end;
    if not DraggingDivider then
      Selecting := True;
  end;
  if Button in [mbLeft, mbRight] then
  begin
    if (Item <> nil) and
      ((Item.HasViewableItems and not (iifExpanded in Item.Flags)) or
      (iifExpanded in Item.Flags)) then
    begin
      if PtInRect(Item.Rects[iprBtnDstRect], Point(X, Y)) or
        ((ssDouble in Shift) and (Item.IsCategory or (XB < Pred(Divider)))) then
      begin
        Item.Expanded := not Item.Expanded;
        Selecting := False;
        if Button = mbRight then
          Item.ExpandItems(Item.Expanded);
      end;
    end;
  end;
  if Button = mbLeft then
  begin
    if (Item <> nil) and (PtInRect(Item.Rects[iprNameArea], Point(X, Y)) or
      PtInRect(Item.Rects[iprValueArea], Point(X, Y))) then
      Item.MouseDown(Button, Shift, X, Y);
    // A click on the value of an item that was not being edited yet leaves
    // the edit control's text fully selected; move the caret to the clicked
    // character instead, like the Delphi object inspector (when the item was
    // already being edited the click lands on the edit control itself and
    // never gets here)
    if not (ssDouble in Shift) and not DraggingDivider and
      (Item <> nil) and Item.Editing and
      (Item.EditCtrl <> nil) and Item.EditCtrl.HandleAllocated and
      PtInRect(Item.Rects[iprEditValue], Point(X, Y)) then
    begin
      CharPos := Item.EditCtrl.Perform(EM_CHARFROMPOS, 0,
        PointToLParam(Point(X - Item.EditCtrl.Left, Y - Item.EditCtrl.Top)));
      if CharPos <> -1 then
        Item.EditCtrl.Perform(EM_SETSEL, Word(CharPos), Word(CharPos));
    end;
  end;
end;

procedure TJvCustomInspector.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  XB: Integer;
  ItemIndex: Integer;
  ItemRect: TRect;
  Item: TJvCustomInspectorItem;
begin
  inherited MouseMove(Shift, X, Y);
  XB := X;
  if DraggingDivider then
    Divider := XB
  else
  if (XB >= Pred(Divider)) and (XB <= Succ(Divider)) then
    Cursor := crHSplit
  else
  begin
    Cursor := crDefault;
    ItemIndex := CalcItemIndex(X, Y, ItemRect);
    if Selecting then
    begin
      if (ItemIndex < VisibleCount) and (ItemIndex <> SelectedIndex) then
      begin
        if ItemIndex < 0 then
          ItemIndex := SelectedIndex;
        SelectedIndex := ItemIndex;
      end;
      if ItemIndex < VisibleCount then
        Item := VisibleItems[ItemIndex]
      else
        Item := nil;
      if Item <> nil then
        Item.MouseMove(Shift, X, Y);
    end;
  end
end;

procedure TJvCustomInspector.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ItemIndex: Integer;
  ItemRect: TRect;
  Item: TJvCustomInspectorItem;
begin
  inherited MouseUp(Button, Shift, X, Y);
  ItemIndex := CalcItemIndex(X, Y, ItemRect);
  if ItemIndex < VisibleCount then
    Item := VisibleItems[ItemIndex]
  else
    Item := nil;
  if Button = mbLeft then
  begin
    if DraggingDivider then
      DraggingDivider := False
    else
    if Selecting then
      Selecting := False;
  end;
  if (Item <> nil) and (PtInRect(Item.Rects[iprNameArea], Point(X, Y)) or
    PtInRect(Item.Rects[iprValueArea], Point(X, Y))) then
    Item.MouseUp(Button, Shift, X, Y)
  else
  if (Selected <> nil) and Selected.Tracking and not PtInRect(ClientRect, Point(X, Y)) then
    Selected.StopTracking;
end;

procedure TJvCustomInspector.Notification(AComponent: TComponent; Operation: TOperation);
begin
  // Mantis 3424: Required for the application not to hang under BDS2006
  // (and maybe 2005). Does not have any impact under D7 and lower.
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent = Painter) then
      FPainter := nil;
  end;
end;

procedure TJvCustomInspector.Paint;
var
  PaintRect: TRect;
begin
  PaintRect := ClientRect;


  if Painter <> nil then
  begin
    if NeedRebuild then
      InvalidateList;
    IncPaintGeneration;
    Painter.Setup(Canvas);
    Painter.Paint;
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(PaintRect);
  end;
end;

procedure TJvCustomInspector.RebuildVisible;
var
  OldSel: TJvCustomInspectorItem;
  Item: TJvCustomInspectorItem;
  ItemStack: TStack;
begin
  FImageHeight := 0;
  OldSel := Selected;
  FVisibleList.Clear;
  Item := Root;
  ItemStack := TStack.Create;
  try
    // Only Root is hidden, so this depth-first walk already emits rows in their
    // final display order; the list keeps empty keys and is not re-sorted
    while Item <> nil do
    begin
      if not Item.Hidden then
        FVisibleList.AddObject('', Item);
      if Item.Expanded and (Item.Count > 0) then
      begin
        ItemStack.Push(Item);
        Item := Item.Items[0];
      end
      else
      begin
        Item := Item.GetNextSibling;
        while (Item = nil) and (ItemStack.Count > 0) do
        begin
          Item := TJvCustomInspectorItem(ItemStack.Pop);
          Item := Item.GetNextSibling;
        end;
      end;
    end;
  finally
    ItemStack.Free;
  end;
  if OldSel <> nil then
    SelectedIndex := FVisibleList.IndexOfObject(OldSel);
  CalcImageHeight;
  NeedRebuild := False;
end;

procedure TJvCustomInspector.RemoveVisible(const Item: TJvCustomInspectorItem);
var
  Idx: Integer;
begin
  Idx := FVisibleList.IndexOfObject(Item);
  if Idx > -1 then
  begin
    FVisibleList.Delete(Idx);
    if SelectedIndex >= Idx then
      SelectedIndex := SelectedIndex - 1;
    Invalidate;
  end;
end;

procedure TJvCustomInspector.BoundsChanged;
begin
  inherited BoundsChanged;
  if csCreating in ControlState then
    Exit;
  FImageHeight := 0; // Force recalculation of the image height
  TopIndex := TopIndex; // Adapt position
  if HandleAllocated then
    UpdateScrollBars;
end;

function TJvCustomInspector.ScrollFactorV: Extended;
begin
  if ClientHeight > 32767 then
    Result := ClientHeight / 32767.0
  else
    Result := 1.0;
end;

procedure TJvCustomInspector.SetDivider(Value: Integer);
var
  W: Integer;
begin
  if HandleAllocated then
    W := ClientWidth
  else
    W := Width;
  if Value > (W - 2 * ItemHeight) then
    Value := W - 2 * ItemHeight;
  if Value < (2 * ItemHeight) then
    Value := 2 * ItemHeight;
  FDivider := Value;
  if HandleAllocated then
    UpdateScrollBars;
end;

procedure TJvCustomInspector.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TJvCustomInspector.SetSelected(const Value: TJvCustomInspectorItem);
var
  Idx: Integer;
begin
  Idx := FVisibleList.IndexOfObject(Value);
  if Idx > -1 then
    SelectedIndex := Idx;
end;

procedure TJvCustomInspector.SetSelectedIndex(Value: Integer);
begin
  if Value >= VisibleCount then
    Value := Pred(VisibleCount);
  if Value < -1 then
    Value := -1;
  if Value <> SelectedIndex then
  begin
    // bugfix WAP.  Why repaint the screen when the component is going away anyway.
    if not (csDestroying in ComponentState) then
    begin
      if Selected <> nil then
        Selected.DoneEdit(False);
      FSelectedIndex := Value;
      if Selected <> nil then
      begin
        Selected.ScrollInView;
        Selected.InitEdit;
      end;
      InvalidateItem;
    end;
  end;
end;

procedure TJvCustomInspector.SetTopIndex(Value: Integer);
var
  MaxIdx: Integer;
begin
  MaxIdx := Succ(YToIdx(ImageHeight - ClientHeight));
  if MaxIdx < 0 then
    MaxIdx := 0;
  if Value > MaxIdx then
    Value := MaxIdx;
  if Value < 0 then
    Value := 0;
  if TopIndex <> Value then
  begin
    FTopIndex := Value;
    if HandleAllocated then
      UpdateScrollBars;
  end;
end;

procedure TJvCustomInspector.UpdateScrollBars;
var
  DrawHeight: Integer;
  ClHeight: Integer;
  ScFactor: Extended;
  ScrollInfo: TScrollInfo;
  ShowVertSB: Boolean;
begin
  if csDestroying in ComponentState then
    Exit;

  if not HandleAllocated then
    Exit;

  // Cache the image height, client height and scroll factor
  DrawHeight := ImageHeight;
  ClHeight := ClientHeight;
  ScFactor := ScrollFactorV;
  { Needed to redisplay the scrollbar after it's hidden in the CloseUp method
    of an enumerated item's combobox }
  ShowVertSB := Round((DrawHeight) / ScFactor) >= Round(ClHeight / ScFactor);
  if ShowVertSB then
  begin
    with ScrollInfo do
    begin
      cbSize := SizeOf(ScrollInfo);
      fMask := SIF_ALL;
      nMin := 0;
      nMax := Round((IdxToY(Succ(YToIdx(ImageHeight - ClientHeight))) + ClientHeight) / ScFactor);
      nPage := Round(ClHeight / ScFactor);
      nPos := Round(IdxToY(TopIndex) / ScFactor);
      nTrackPos := 0;
    end;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end;
  ShowScrollBars(SB_VERT, ShowVertSB);
  Invalidate;
end;

function TJvCustomInspector.ViewRect: TRect;
begin
  Result := ClientRect;
end;

procedure TJvCustomInspector.GetDlgCode(var Code: TDlgCodes);
begin
  Code := [dcWantArrows];
end;

procedure TJvCustomInspector.FocusSet(PrevWnd: THandle);
begin
  inherited FocusSet(PrevWnd);
  if (Selected <> nil) and not Selected.EditCtrlDestroying then
    Selected.SetFocus;
  Invalidate;
end;

procedure TJvCustomInspector.FocusKilled(NextWnd: THandle);
begin
  inherited FocusKilled(NextWnd);
  Invalidate;
end;

procedure TJvCustomInspector.WMVScroll(var Msg: TWMScroll);
var
  Delta: Integer;
  ScFactor: Extended;
begin
  Delta := 0;
  ScFactor := ScrollFactorV;
  case Msg.ScrollCode of
    SB_BOTTOM:
      Delta := ImageHeight - ClientHeight - IdxToY(TopIndex);
    SB_ENDSCROLL:
      Delta := 0;
    SB_LINEDOWN:
      TopIndex := TopIndex + 1;
    SB_LINEUP:
      TopIndex := TopIndex - 1;
    SB_PAGEDOWN:
      Delta := ClientHeight;
    SB_PAGEUP:
      Delta := -ClientHeight;
    SB_THUMBPOSITION:
      Delta := Round(Msg.Pos * ScFactor) - IdxToY(TopIndex);
    SB_THUMBTRACK:
      Delta := Round(Msg.Pos * ScFactor) - IdxToY(TopIndex);
    SB_TOP:
      Delta := -IdxToY(TopIndex);
  else
    Delta := 0;
  end;
  if Delta <> 0 then
    TopIndex := YToIdx(IdxToY(TopIndex) + Delta);
end;

function TJvCustomInspector.YToIdx(const Y: Integer): Integer;
var
  CurY: Integer;
begin
  Result := 0;
  CurY := 0;
  while (Result < VisibleCount) and (Y > (CurY + VisibleItems[Result].Height)) do
  begin
    Inc(CurY, VisibleItems[Result].Height);
    Inc(Result);
  end;
  if Result >= VisibleCount then
    Result := -1;
end;

procedure TJvCustomInspector.BeforeDestruction;
begin
  inherited BeforeDestruction;
  GlobalInspReg.UnRegInspector(Self);
  FRoot.Free;
  FVisibleList.Free;
end;

function TJvCustomInspector.BeginUpdate: Integer;
begin
  Inc(FLockCount);
  Result := FLockCount;
end;

function TJvCustomInspector.EndUpdate: Integer;
begin
  if LockCount > 0 then
    Dec(FLockCount);
  Result := LockCount;
  if Result = 0 then
  begin
    if NeedRebuild then
      InvalidateList
    else
      InvalidateItem;
  end;
end;

function TJvCustomInspector.Focused: Boolean;
begin
  Result := inherited Focused or ((Selected <> nil) and Selected.EditFocused);
end;

function TJvCustomInspector.VisibleIndex(
  const AItem: TJvCustomInspectorItem): Integer;
begin
  Result := FVisibleList.IndexOfObject(AItem);
end;

procedure TJvCustomInspector.RefreshValues;
begin
  if (Selected <> nil) and Selected.Editing then
  begin
    if (Selected.EditCtrl = nil) or (Selected.DisplayValue <> Selected.EditCtrl.Text) then
    begin
      Selected.DoneEdit(True);
      Selected.InitEdit;
    end;
  end;
  Invalidate;
end;

procedure TJvCustomInspector.Clear;
begin
  BeginUpdate;
  SelectedIndex := -1;
  Root.Clear;
  EndUpdate;
end;

function TJvCustomInspector.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  Count: Integer;
  Index: Integer;
  LbPos: TPoint;
  MinPos, MaxPos: Integer;
begin
  if (Selected <> nil) and Selected.DroppedDown then
  begin
    // If Selected.ListBox gets the WM_MOUSEWHEEL we would run into an infinite recursion
    if not FMouseWheelRecursion then
    begin
      FMouseWheelRecursion := True;
      try
        LbPos := Selected.ListBox.ScreenToClient(ClientToScreen(MousePos));
        Selected.ListBox.Perform(WM_MOUSEWHEEL, WheelDelta shl 16, MakeLong(LbPos.X, LbPos.Y));
      finally
        FMouseWheelRecursion := False;
      end;
    end;
  end
  else
  begin
    GetScrollRange(Handle, SB_VERT, MinPos, MaxPos);
    if MinPos <> MaxPos then // no scroll bar enabled
    begin
      Count := -WheelDelta div (120 div 5); // 5 items per scroll
      Index := TopIndex + Count;
      if Index < 0 then
        Index := 0;
      TopIndex := Index;
    end;
  end;
  Result := True;
end;

procedure TJvCustomInspector.ShowScrollBars(Bar: Integer; Visible: Boolean);
begin
  ShowScrollBar(Handle, Bar, Visible);
end;

//=== { TJvInspectorPainter } ================================================

constructor TJvInspectorPainter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInspector := nil;
  FInternalCollapseButton := TBitmap.Create;
  FInternalExpandButton := TBitmap.Create;

  Initializing := True;
  try
    InitializeColors;
  finally
    Initializing := False;
  end;
  // FInternalCollapseButton and FInternalExpandButton are rendered on demand
  // by PrepareInternalImages
end;

destructor TJvInspectorPainter.Destroy;
begin
  FInternalCollapseButton.Free;
  FInternalExpandButton.Free;
  inherited Destroy;
end;

procedure TJvInspectorPainter.ApplyNameFont;
begin
  Canvas.Font := Inspector.Font;
  if Assigned(Item) and Item.IsCategory then
    Canvas.Font.Color := CategoryTextColor
  else
    Canvas.Font.Color := NameColor;
end;

procedure TJvInspectorPainter.ApplyValueFont;
begin
  Canvas.Font := Inspector.Font;
  Canvas.Font.Color := ValueColor;
end;

procedure TJvInspectorPainter.CalcButtonBasedRects;
begin
end;

procedure TJvInspectorPainter.CalcEditBasedRects;
begin
end;

procedure TJvInspectorPainter.CalcNameBasedRects;
begin
end;

procedure TJvInspectorPainter.CalcValueBasedRects;
begin
end;

function TJvInspectorPainter.DividerWidth: Integer;
begin
  Result := 1;
end;

procedure TJvInspectorPainter.DoPaint;
begin
end;

function TJvInspectorPainter.GetBackgroundColor: TColor;
begin
  Result := FBackgroundColor;
end;

function TJvInspectorPainter.GetCategoryColor: TColor;
begin
  Result := FCategoryColor;
end;

function TJvInspectorPainter.GetCategoryDividerColor: TColor;
begin
  Result := FCategoryDividerColor;
end;

function TJvInspectorPainter.GetCategoryTextColor: TColor;
begin
  Result := FCategoryTextColor;
end;

procedure TJvInspectorPainter.PrepareInternalImages;
var
  Size: Integer;

  procedure PrepareImage(const Image: TBitmap; const Expand: Boolean);
  var
    Margin: Integer;
    Mid: Integer;
  begin
    Margin := MulDiv(2, Size, 9);
    Mid := Size div 2;
    Image.SetSize(Size, Size);
    Image.Canvas.Brush.Color := BackgroundColor;
    Image.Canvas.Pen.Color := NameColor;
    Image.Canvas.Rectangle(0, 0, Size, Size);
    Image.Canvas.MoveTo(Margin, Mid);
    Image.Canvas.LineTo(Size - Margin, Mid);
    if Expand then
    begin
      Image.Canvas.MoveTo(Mid, Margin);
      Image.Canvas.LineTo(Mid, Size - Margin);
    end;
  end;

begin
  // Renders the built-in expand/collapse buttons at the inspector's current
  // dpi using the painter's colors, keeping the last
  // rendering until the size or the colors change
  Size := MulDiv(9, Inspector.CurrentPPI, 96);
  if not Odd(Size) then
    Dec(Size);
  if (Size = FInternalButtonSize) and
    (BackgroundColor = FInternalButtonBackgroundColor) and
    (NameColor = FInternalButtonPenColor) then
    Exit;
  FInternalButtonSize := Size;
  FInternalButtonBackgroundColor := BackgroundColor;
  FInternalButtonPenColor := NameColor;
  PrepareImage(FInternalCollapseButton, False);
  PrepareImage(FInternalExpandButton, True);
end;

function TJvInspectorPainter.GetCollapseImage: TBitmap;
begin
  PrepareInternalImages;
  Result := FInternalCollapseButton;
end;

function TJvInspectorPainter.GetDividerColor: TColor;
begin
  Result := FDividerColor;
end;

function TJvInspectorPainter.GetExpandImage: TBitmap;
begin
  PrepareInternalImages;
  Result := FInternalExpandButton;
end;

function TJvInspectorPainter.GetHideSelectColor: TColor;
begin
  Result := SelectedColor;
end;

function TJvInspectorPainter.GetHideSelectTextColor: TColor;
begin
  Result := SelectedTextColor;
end;

function TJvInspectorPainter.GetNameColor: TColor;
begin
  Result := FNameColor;
end;

function TJvInspectorPainter.GetRects(const Index: TInspectorPaintRect): TRect;
begin
  if Item <> nil then
    Result := Item.Rects[Index]
  else
    Result := Rect(0, 0, 0, 0);
end;

function TJvInspectorPainter.GetSelectedColor: TColor;
begin
  Result := FSelectedColor;
end;

function TJvInspectorPainter.GetSelectedTextColor: TColor;
begin
  Result := FSelectedTextColor;
end;

function TJvInspectorPainter.GetValueColor: TColor;
begin
  Result := FValueColor;
end;

procedure TJvInspectorPainter.HideEditor;
begin
  Inspector.Selected.Rects[iprEditValue] := Rect(0, 0, 0, 0);
end;

procedure TJvInspectorPainter.InitializeColors;
begin
  SetDefaultProp(Self, ['BackgroundColor', 'DividerColor', 'CategoryColor', 'SelectedColor']);
end;

procedure TJvInspectorPainter.Paint;
var
  SelItemVisible: Boolean;
  Rect: TRect;
  ItemIdx: Integer;
  MaxItemIdx: Integer;
begin
  SelItemVisible := False;
  Rect := Inspector.ViewRect;
  Canvas.FillRect(Rect);
  ItemIdx := Inspector.TopIndex;
  MaxItemIdx := Inspector.VisibleCount;
  // Loop through the visible list
  while (Rect.Top < Rect.Bottom) and (ItemIdx < MaxItemIdx) do
  begin
    SelItemVisible := SelItemVisible or (ItemIdx = Inspector.SelectedIndex);
    PaintItem(Rect, ItemIdx);
    Inc(ItemIdx);
  end;
  if not SelItemVisible and (Inspector.Selected <> nil) then
    HideEditor;
end;

procedure TJvInspectorPainter.PaintDivider(const X, YTop, YBottom: Integer);
begin
end;

procedure TJvInspectorPainter.PaintItem(var ARect: TRect;
  const AItemIndex: Integer);
var
  OrgState: Integer;
begin
  OrgState := SaveCanvasState(Canvas);
  try
    // Initialize painter variables
    PaintRect := ARect;
    ItemIndex := AItemIndex;
    SetupItem;

    // Do actual painting
    DoPaint;

    // Finalize painting
    TeardownItem;
    ARect := PaintRect;
  finally
    RestoreCanvasState(Canvas, OrgState);
  end;
end;

procedure TJvInspectorPainter.SetBackgroundColor(const Value: TColor);
begin
  if Value <> BackgroundColor then
  begin
    FBackgroundColor := Value;
    if not Initializing and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetCategoryColor(const Value: TColor);
begin
  if Value <> CategoryColor then
  begin
    FCategoryColor := Value;
    if not Initializing and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetCategoryDividerColor(const Value: TColor);
begin
  if Value <> CategoryDividerColor then
  begin
    FCategoryDividerColor := Value;
    if not Initializing and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetCategoryTextColor(const Value: TColor);
begin
  if Value <> CategoryTextColor then
  begin
    FCategoryTextColor := Value;
    if not Initializing and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetDividerColor(const Value: TColor);
begin
  if DividerColor <> Value then
  begin
    FDividerColor := Value;
    if not Initializing and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetHideSelectColor(const Value: TColor);
begin
end;

procedure TJvInspectorPainter.SetHideSelectTextColor(const Value: TColor);
begin
end;

procedure TJvInspectorPainter.SetNameColor(const Value: TColor);
begin
  if Value <> NameColor then
  begin
    FNameColor := Value;
    if not Initializing and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetRects(const Index: TInspectorPaintRect;
  const ARect: TRect);
begin
  if Item <> nil then
    Item.Rects[Index] := ARect;
end;

procedure TJvInspectorPainter.SetSelectedColor(const Value: TColor);
begin
  if Value <> SelectedColor then
  begin
    FSelectedColor := Value;
    if not Initializing and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetSelectedTextColor(const Value: TColor);
begin
  if Value <> SelectedTextColor then
  begin
    FSelectedTextColor := Value;
    if not Initializing and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.Setup(const ACanvas: TCanvas);
begin
  Canvas := ACanvas;
  Canvas.Brush.Color := BackgroundColor;
end;

procedure TJvInspectorPainter.SetupItem;
begin
  // retrieve item
  if ItemIndex > -1 then
    Item := Inspector.VisibleItems[ItemIndex];

  if Item <> nil then
  begin
    // retrieve button image
    if Item.Expanded then
      ButtonImage := GetCollapseImage
    else
    if Item.HasViewableItems then
      ButtonImage := GetExpandImage
    else
      ButtonImage := nil;
  end
  else
    ButtonImage := nil;

  // calculate rectangles
  if ItemIndex > -1 then
    SetupRects;
end;

procedure TJvInspectorPainter.SetupRects;
begin
  Rects[iprItem] := Rect(PaintRect.Left, PaintRect.Top,
    PaintRect.Right, Pred(PaintRect.Top + Item.Height));
end;

procedure TJvInspectorPainter.SetValueColor(const Value: TColor);
begin
  if Value <> ValueColor then
  begin
    FValueColor := Value;
    if not Initializing and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.TeardownItem;
var
  TmpRect: TRect;
begin
  TmpRect := PaintRect;
  TmpRect.Top := Succ(Rects[iprItem].Bottom);
  PaintRect := TmpRect;
  Item := nil;
  ItemIndex := -1;
end;

procedure TJvInspectorPainter.SetInspector(const AInspector: TJvCustomInspector);
begin
  FInspector := AInspector;
end;

//=== { TJvInspectorBorlandNETBasePainter } ==================================

procedure TJvInspectorBorlandNETBasePainter.ApplyNameFont;
begin
  inherited ApplyNameFont;
  if Assigned(Item) and Item.IsCategory then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
end;

procedure TJvInspectorBorlandNETBasePainter.CalcButtonBasedRects;
var
  BtnSrcRect: TRect;
  BtnDstRect: TRect;
  Y: Integer;
begin
  if (ButtonImage <> nil) and (RectWidth(Rects[iprButtonArea]) > 0) then
  begin
    BtnSrcRect := Rect(0, 0, ButtonImage.Width, ButtonImage.Height);
    BtnDstRect := Rect(0, 0, RealButtonAreaWidth, RectHeight(Rects[iprButtonArea]));
    if BtnSrcRect.Right > BtnDstRect.Right then
    begin
      BtnSrcRect.Left := (BtnDstRect.Right - BtnSrcRect.Right) div 2;
      BtnSrcRect.Right := BtnSrcRect.Left + BtnDstRect.Right;
    end;
    if BtnSrcRect.Bottom > BtnDstRect.Bottom then
    begin
      BtnSrcRect.Top := (BtnDstRect.Bottom - BtnSrcRect.Bottom) div 2;
      BtnSrcRect.Bottom := BtnSrcRect.Top + BtnDstRect.Bottom;
    end;
    if BtnDstRect.Right > RectWidth(BtnSrcRect) then
    begin
      BtnDstRect.Left := (BtnDstRect.Right - RectWidth(BtnSrcRect)) div 2;
      BtnDstRect.Right := BtnDstRect.Left + RectWidth(BtnSrcRect);
    end;
    if BtnDstRect.Bottom > RectHeight(BtnSrcRect) then
    begin
      if (RectHeight(BtnDstRect) div Inspector.ItemHeight) < 2 then
        Y := (RectHeight(BtnDstRect) - RectHeight(BtnSrcRect)) div 2
      else
        Y := (Inspector.ItemHeight - RectHeight(BtnSrcRect)) div 2;
      BtnDstRect.Top := Y;
      BtnDstRect.Bottom := BtnDstRect.Top + RectHeight(BtnSrcRect);
    end;
    OffsetRect(BtnDstRect, Rects[iprButtonArea].Left, Rects[iprButtonArea].Top);
    IntersectRect(BtnDstRect, BtnDstRect, Rects[iprButtonArea]);
  end
  else
  begin
    BtnSrcRect := Rect(0, 0, 0, 0);
    BtnDstRect := Rect(0, 0, 0, 0);
  end;
  Rects[iprBtnSrcRect] := BtnSrcRect;
  Rects[iprBtnDstRect] := BtnDstRect;
end;

procedure TJvInspectorBorlandNETBasePainter.CalcEditBasedRects;
var
  TmpRect: TRect;
begin
  if not (iifValueList in Item.Flags) then
  begin // Value takes up entire edit value rect, there is no edit button:
    Rects[iprEditValue] := Rects[iprValue];
    Rects[iprEditButton] := Rect(0, 0, 0, 0);
  end
  else
  begin // The edit button is on the right of the edit value area:
    TmpRect := Rects[iprValue];
    Dec(TmpRect.Right, Inspector.ItemHeight);
    Rects[iprEditValue] := TmpRect;
    TmpRect := Rects[iprValueArea];
    TmpRect.Left := TmpRect.Right - Inspector.ItemHeight;
    Rects[iprEditButton] := TmpRect;
  end;
end;

procedure TJvInspectorBorlandNETBasePainter.CalcNameBasedRects;
var
  CanvasState: Integer;
  RowHeight: Integer;
  TmpRect: TRect;
begin
  CanvasState := SaveCanvasState(Canvas);
  try
    ApplyNameFont;
    RowHeight := CanvasMaxTextHeight(Canvas);
    TmpRect := Rects[iprNameArea];
    if Item.Level = 0 then
      Inc(TmpRect.Left, 2);
    if RectHeight(TmpRect) div RowHeight < 2 then
      OffsetRect(TmpRect, 0, (RectHeight(TmpRect) - RowHeight) div 2)
    else
    begin
      Inc(TmpRect.Top, 1);
      Dec(TmpRect.Bottom, 1);
    end;
    IntersectRect(TmpRect, TmpRect, Rects[iprNameArea]);
    Rects[iprName] := TmpRect;
  finally
    RestoreCanvasState(Inspector.Canvas, CanvasState);
  end;
end;

procedure TJvInspectorBorlandNETBasePainter.CalcValueBasedRects;
var
  CanvasState: Integer;
  RowHeight: Integer;
  TmpRect: TRect;
begin
  CanvasState := SaveCanvasState(Canvas);
  try
    ApplyValueFont;
    RowHeight := CanvasMaxTextHeight(Canvas);
    TmpRect := Rects[iprValueArea];
    if RectHeight(TmpRect) div RowHeight < 2 then
    begin
      OffsetRect(TmpRect, 0, (RectHeight(TmpRect) - RowHeight) div 2);
      IntersectRect(TmpRect, TmpRect, Rects[iprValueArea]);
    end
    else
    begin
      Inc(TmpRect.Top, 1);
      Dec(TmpRect.Bottom, 1);
      IntersectRect(TmpRect, TmpRect, Rects[iprValueArea]);
    end;
    Rects[iprValue] := TmpRect;
  finally
    RestoreCanvasState(Inspector.Canvas, CanvasState);
  end;
  CalcEditBasedRects;
end;

procedure TJvInspectorBorlandNETBasePainter.InitializeColors;
begin
  inherited InitializeColors;

  CategoryTextColor := clBtnText;
  NameColor := clWindowText;
  ValueColor := clWindowText;
end;

procedure TJvInspectorBorlandNETBasePainter.SetupRects;
var
  ItemRect2: TRect;
  TmpRect: TRect;
begin
  inherited SetupRects;
  ItemRect2 := Rects[iprItem];
  TmpRect := Rect(ItemRect2.Left + (Item.Level * Inspector.ItemHeight), ItemRect2.Top,
    ItemRect2.Left + (Succ(Item.Level) * Inspector.ItemHeight), ItemRect2.Bottom);
  RealButtonAreaWidth := RectWidth(TmpRect);
  if not Item.IsCategory and (TmpRect.Left > Pred(Inspector.Divider)) then
  begin
    TmpRect.Left := 0;
    TmpRect.Right := 0;
  end;
  if not Item.IsCategory and (TmpRect.Right > Pred(Inspector.Divider)) then
    TmpRect.Right := Pred(Inspector.Divider);
  Rects[iprButtonArea] := TmpRect;
  TmpRect := ItemRect2;
  TmpRect.Left := ItemRect2.Left + (Succ(Item.Level) * Inspector.ItemHeight);
  Rects[iprNameArea] := TmpRect;
  if Item.IsCategory then
    Rects[iprValueArea] := Rect(0, 0, 0, 0)
  else
  begin
    if TmpRect.Left > Pred(Inspector.Divider) then
      TmpRect := Rect(0, 0, 0, 0)
    else
      TmpRect.Right := ItemRect2.Left + Pred(Inspector.Divider);
    Rects[iprNameArea] := TmpRect;
    TmpRect := ItemRect2;
    TmpRect.Left := ItemRect2.Left + Inspector.Divider + DividerWidth;
    Rects[iprValueArea] := TmpRect;
  end;
  CalcButtonBasedRects;
  CalcNameBasedRects;
  CalcValueBasedRects;
end;

//=== { TJvInspectorDotNETPainter } ==========================================

procedure TJvInspectorDotNETPainter.ApplyNameFont;
begin
  inherited ApplyNameFont;
  if Item = Inspector.Selected then
  begin
    if Inspector.Focused then
    begin
      Canvas.Brush.Color := SelectedColor;
      Canvas.Font.Color := SelectedTextColor;
    end
    else
    begin
      Canvas.Brush.Color := HideSelectColor;
      Canvas.Font.Color := HideSelectTextColor;
    end;
  end
  else
  if Item.IsCategory and (Item.Level = 0) then
    Canvas.Brush.Color := CategoryColor
  else
    Canvas.Brush.Color := BackgroundColor;
end;

function TJvInspectorDotNETPainter.GetHideSelectColor: TColor;
begin
  Result := FHideSelectColor;
end;

function TJvInspectorDotNETPainter.GetHideSelectTextColor: TColor;
begin
  Result := FHideSelectTextColor;
end;

procedure TJvInspectorDotNETPainter.DoPaint;
var
  EndOfList: Boolean;
  NextItem: TJvCustomInspectorItem;
  EndOfCat: Boolean;
  PreNameRect: TRect;
  CatRect: TRect;
  SaveIdx: Integer;
  LeftX: Integer;
begin
  SaveIdx := SaveCanvasState(Canvas);

  // Determine item type (end of list, end of a level 0 category)
  EndOfList := Succ(ItemIndex) >= Inspector.VisibleCount;
  if not EndOfList then
  begin
    NextItem := Inspector.VisibleItems[Succ(ItemIndex)];
    EndOfCat := (NextItem.BaseCategory <> Item.BaseCategory) and
      (Item.BaseCategory <> nil);
  end
  else
    EndOfCat := Item.BaseCategory <> nil;

  PreNameRect := Rects[iprItem];
  PreNameRect.Left := PreNameRect.Left + (Item.Level * Inspector.ItemHeight) + RealButtonAreaWidth;
  if PreNameRect.Left > Pred(Inspector.Divider) then
    PreNameRect := Rect(0, 0, 0, 0)
  else
  begin
    PreNameRect.Right := PreNameRect.Left + RealButtonAreaWidth;
    if PreNameRect.Right > Pred(Inspector.Divider) then
      PreNameRect.Right := Pred(Inspector.Divider);
  end;
  Inc(PreNameRect.Right);

  CatRect := Rects[iprItem];
  CatRect.Right := CatRect.Left + RealButtonAreaWidth;
  Inc(CatRect.Bottom);
  if Item.BaseCategory <> nil then
  begin
    Canvas.Brush.Color := CategoryColor;
    Canvas.FillRect(CatRect);
    ApplyCanvasState(Canvas, SaveIdx);
  end;

  if not (Item.IsCategory) then
    PaintDivider(Rects[iprItem].Left + Inspector.Divider, Pred(Rects[iprItem].Top),
      Rects[iprItem].Bottom);

  if (Item.IsCategory) and (Item.Level = 0) then
    Canvas.Brush.Color := CategoryColor;
  if (Item = Inspector.Selected) and
    ((Item.Level > 0) or  not (Item.IsCategory)) then
  begin
    if Inspector.Focused then
      Canvas.Brush.Color := SelectedColor
    else
      Canvas.Brush.Color := HideSelectColor;
  end;
  Canvas.FillRect(PreNameRect);
  ApplyNameFont;
  Canvas.FillRect(Rects[iprNameArea]);
  if Assigned(FOnSetItemColors) then
    FOnSetItemColors(Item, Canvas);
  Item.DrawName(Canvas);
  ApplyCanvasState(Canvas, SaveIdx);
  ApplyValueFont;
  if Assigned(FOnSetItemColors) then
    FOnSetItemColors(Item, Canvas); // Custom colors for canvas and font for cells depending on values.
  Item.DrawValue(Canvas);
  RestoreCanvasState(Canvas, SaveIdx);

  if ButtonImage <> nil then
    Canvas.CopyRect(Rects[iprBtnDstRect], ButtonImage.Canvas, Rects[iprBtnSrcRect]);

  SaveIdx := SaveCanvasState(Canvas);
  if EndOfCat or ((Item.IsCategory) and
    (Item.Level = 0)) then
    Canvas.Pen.Color := CategoryDividerColor
  else
    Canvas.Pen.Color := DividerColor;
  if not EndOfList and not EndOfCat then
    LeftX := Rects[iprItem].Left + RealButtonAreaWidth
  else
    LeftX := Rects[iprItem].Left;
  Canvas.MoveTo(Rects[iprItem].Right, Rects[iprItem].Bottom);
  Canvas.LineTo(Pred(LeftX), Rects[iprItem].Bottom);

  if Item <> Item.BaseCategory then
  begin
    if Item.BaseCategory <> nil then
      Canvas.Pen.Color := CategoryDividerColor
    else
      Canvas.Pen.Color := CategoryColor;
    Canvas.MoveTo(Rects[iprItem].Left + RealButtonAreaWidth, Rects[iprItem].Top);
    Canvas.LineTo(Rects[iprItem].Left + RealButtonAreaWidth, Succ(Rects[iprItem].Bottom));
  end;
  RestoreCanvasState(Canvas, SaveIdx);
end;

procedure TJvInspectorDotNETPainter.InitializeColors;
begin
  inherited InitializeColors;

  SetDefaultProp(Self, ['HideSelectColor', 'CategoryDividerColor']);

  HideSelectTextColor := clHighlightText;
  SelectedTextColor := clHighlightText;
end;

procedure TJvInspectorDotNETPainter.PaintDivider(const X, YTop, YBottom: Integer);
begin
  with Canvas do
  begin
    Pen.Color := DividerColor;
    MoveTo(X, YTop);
    LineTo(X, YBottom);
  end
end;

procedure TJvInspectorDotNETPainter.SetHideSelectColor(const Value: TColor);
begin
  if Value <> HideSelectColor then
  begin
    FHideSelectColor := Value;
    if not Initializing and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorDotNETPainter.SetHideSelectTextColor(const Value: TColor);
begin
  if Value <> HideSelectTextColor then
  begin
    FHideSelectTextColor := Value;
    if not Initializing and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

//=== { TJvInspectorEdit } ===================================================

type
  TJvInspectorEdit = class(TEdit)
  private
    FOnKillFocus: TNotifyEvent;
  protected
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
  public
    property OnKillFocus: TNotifyEvent read FOnKillFocus write FOnKillFocus;
  end;

procedure TJvInspectorEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  if Assigned(FOnKillFocus) then
    FOnKillFocus(Self);
end;

//=== { TJvCustomInspectorItem } =============================================

constructor TJvCustomInspectorItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create;
  FData := nil;
  FItems := TObjectList.Create(True);
  Flags := [];
  if AData <> nil then
    FDisplayName := AData.Name;
  if AParent <> nil then
  begin
    FInspector := AParent.Inspector;
    AParent.Add(Self)
  end;
  FData := AData;
end;

destructor TJvCustomInspectorItem.Destroy;
begin
  FAutoComplete.Free;
  inherited Destroy;
end;

procedure TJvCustomInspectorItem.Apply;
var
  TmpOnChange: TNotifyEvent;
  NewValue: string;
begin
  try
    if Editing and (EditCtrl <> nil) then
    begin
      NewValue := EditCtrl.Text;
      if not Data.IsAssigned or (DisplayValue <> NewValue) then
      begin
        Inc(FUpdateEditCtrl);
        try
          DisplayValue := NewValue;
        finally
          Dec(FUpdateEditCtrl);
        end;
        InvalidateItem;
        if EditCtrl <> nil then
        begin
          TmpOnChange := TCustomEditAccessProtected(EditCtrl).OnChange;
          TCustomEditAccessProtected(EditCtrl).OnChange := nil;
          try
            if Data.IsAssigned then
              EditCtrl.Text := DisplayValue
            else
              EditCtrl.Text := '';
          finally
            TCustomEditAccessProtected(EditCtrl).OnChange := TmpOnChange;
          end;
        end;
      end;
    end;
  finally
    if Editing and (EditCtrl <> nil) then
    begin
      EditCtrl.SelectAll;
      EditCtrl.Modified := False;
      EditCtrl.ClearUndo;
    end;
  end;
end;

function TJvCustomInspectorItem.CanEdit: Boolean;
begin
  Result := not IsCategory and not ReadOnly and not Inspector.ReadOnly and Data.IsInitialized and
    Data.HasValue;
end;

procedure TJvCustomInspectorItem.CloseUp(Accept: Boolean);
var
  ListValue: string;
begin
  if DroppedDown then
  begin
    if GetCaptureControl = ListBox then
      SetCaptureControl(nil);
    if Inspector.HandleAllocated then
      Inspector.ShowScrollBars(SB_BOTH, False);
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if ListBox.ItemIndex > -1 then
      ListValue := ListBox.Items[ListBox.ItemIndex];
    SetWindowPos(ListBox.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FDroppedDown := False;
    InvalidateItem;
    if Accept then
    begin
      if Assigned(EditCtrl) then
        EditCtrl.Text := ListValue;
      Apply;
    end;
  end;
end;

procedure TJvCustomInspectorItem.Deactivate;
begin
  if DroppedDown then
    CloseUp(False);
end;

procedure TJvCustomInspectorItem.DoDropDownKeys(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then
      begin
        if DroppedDown then
          CloseUp(True)
        else
          DropDown;
        Key := 0;
      end;
    VK_RETURN, VK_ESCAPE:
      if DroppedDown and not (ssAlt in Shift) then
      begin
        CloseUp(Key = VK_RETURN);
        Key := 0;
      end;
  end;
end;

procedure TJvCustomInspectorItem.DoGetValueList(const Strings: TStrings);
begin
  if Assigned(FOnGetValueList) then
    FOnGetValueList(Self, Strings);
end;

procedure TJvCustomInspectorItem.DropDown;
const
  DropDownCount = 8;
var
  ListCount: Integer;
  P: TPoint;
  Y: Integer;
  J: Integer;
  I: Integer;
  R: TRect;
  EditMonitor: TMonitor;
begin
  if (not DroppedDown) and (ListBox <> nil) then
  begin
    ListBox.Width := RectWidth(Rects[iprValueArea]);
    TListBox(ListBox).Font := TCustomEditAccessProtected(EditCtrl).Font;
    ListBox.Items.Clear;
    GetValueList(ListBox.Items);
    if ListBox.Items.Count < DropDownCount then
      ListCount := ListBox.Items.Count
    else
      ListCount := DropDownCount;
    if ListCount = 0 then
      ListCount := 1;
    TListBox(ListBox).Height := ListCount * TListBox(ListBox).ItemHeight + 4;
    if ListBox.Height > Screen.DesktopHeight then
    begin
      ListCount := (Screen.DesktopHeight - 4) div TListBox(ListBox).ItemHeight;
      TListBox(ListBox).Height := ListCount * TListBox(ListBox).ItemHeight + 4;
    end;
    ListBox.ItemIndex := ListBox.Items.IndexOf(EditCtrl.Text);
    J := ListBox.ClientWidth;
    if ListBox.Items.Count > ListCount then
      Dec(J, GetSystemMetrics(SM_CXVSCROLL));
    for I := 0 to ListBox.Items.Count - 1 do
    begin
      Y := ListBox.Canvas.TextWidth(ListBox.Items[I]) + 4;
      if Y > J then
        J := Y;
    end;
    if ListBox.Items.Count > ListCount then
      Inc(J, GetSystemMetrics(SM_CXVSCROLL));
    ListBox.ClientWidth := J;
    if ListBox.Width > Screen.DesktopWidth then
      ListBox.Width := Screen.DesktopWidth;
    P := Inspector.ClientToScreen(Point(Rects[iprValueArea].Right - ListBox.Width, EditCtrl.Top));
    if P.X < 0 then
      P := Inspector.ClientToScreen(Point(Rects[iprValueArea].Left, EditCtrl.Top));

    Y := P.Y + RectHeight(Rects[iprValueArea]);
    GetWindowRect(EditCtrl.Handle, R);
    EditMonitor := Screen.MonitorFromRect(R);
    if EditMonitor <> nil then
    begin
      R := EditMonitor.BoundsRect;
      if P.X + ListBox.Width > R.Right then
        P.X := R.Right - ListBox.Width;
      if P.X < R.Left then
        P.X := R.Left;
      if Y + ListBox.Height > R.Bottom then
        Y := P.Y - TListBox(ListBox).Height;
      if Y < R.Top then
        Y := R.Top;
    end
    else
    begin
      if Y + ListBox.Height > Screen.DesktopHeight then
        Y := P.Y - TListBox(ListBox).Height;
      if P.X + ListBox.Width > Screen.DesktopWidth then
        P.X := Screen.DesktopWidth - ListBox.Width;
    end;
    SetWindowPos(ListBox.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or {SWP_NOACTIVATE or }SWP_SHOWWINDOW);
    InvalidateItem;
    EditCtrl.SetFocus;
    FDroppedDown := True; // must be after EditCtrl.SetFocus
    Inspector.Selecting := False;
  end;
end;

procedure TJvCustomInspectorItem.EditFocusLost(Sender: TObject);
begin
  if Inspector.HandleAllocated and not Inspector.Focused then
  begin
    // Mantis 3391: When the focus is lost, the editing is finished, so that
    // moving to another item or another control always updates the value.
    try
      Apply;
    except
      Application.HandleException(Self);
      if (EditCtrl <> nil) and EditCtrl.CanFocus then
        EditCtrl.SetFocus;
    end;
    InvalidateItem;

    Inspector.Invalidate;
  end;
end;

procedure TJvCustomInspectorItem.EditKillFocus(Sender: TObject);
begin
  if DroppedDown then
    CloseUp(False);
end;

procedure TJvCustomInspectorItem.AutoCompleteStart(Sender: TObject);
begin
  ListBox.Items.Clear;
  GetValueList(ListBox.Items);
end;

procedure TJvCustomInspectorItem.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if (iifValueList in Flags) and not ReadOnly then
  begin
    if not Assigned(FAutoComplete) then
    begin
      FAutoComplete := TJvEditListBoxAutoComplete.Create(TCustomEdit(EditCtrl), ListBox);
    end
    else
    begin
      // Mantis 3401: AutoComplete component is already created, but the
      // EditCtrl and ListBox properties may have been reset to nil, especially
      // by the DoneEdit call. Hence the need to reaffect them.
      FAutoComplete.EditCtrl := EditCtrl;
      FAutoComplete.ListBox := ListBox;
    end;
    FAutoComplete.OnDropDown := AutoCompleteStart;
    FAutoComplete.AutoComplete(Key);
  end;
end;

procedure TJvCustomInspectorItem.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(Inspector.FOnEditorKeyDown) then
    Inspector.FOnEditorKeyDown(Inspector, Key, Shift);

  if Shift = [] then
  begin
    case Key of
      VK_RETURN:
        Apply;
      VK_ESCAPE:
        Undo;
    end;
    if (Key = VK_RETURN) or (Key = VK_ESCAPE) then
      Key := VK_RIGHT;
  end
  else
  if Shift = [ssCtrl] then
    case Key of
      VK_UP:
        if iifValueList in Flags then
        begin
          SelectValue(-1);
          Key := 0;
        end;
      VK_DOWN:
        if iifValueList in Flags then
        begin
          SelectValue(1);
          Key := 0;
        end;
      VK_RETURN:
        if iifValueList in Flags then
        begin
          SelectValue(1);
          Key := 0;
        end;
    end;
end;

procedure TJvCustomInspectorItem.EditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (ssDouble in Shift) and (iifValueList in Flags) then
    SelectValue(1);
end;

procedure TJvCustomInspectorItem.EditMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  InspCoord: TPoint;
begin
  InspCoord := Inspector.ScreenToClient(EditCtrl.ClientToScreen(Point(X, Y)));
  Inspector.MouseUp(Button, Shift, InspCoord.X, InspCoord.Y);
end;

procedure TJvCustomInspectorItem.Edit_WndProc(var Msg: TMessage);
var
  ExecInherited: Boolean;
  PostToInsp: Boolean;
begin
  ExecInherited := True;
  case Msg.Msg of
    WM_KEYDOWN, WM_SYSKEYDOWN, WM_CHAR:
      begin
        if iifValueList in Flags then
        begin
          DoDropDownKeys(TWMKeyDown(Msg).CharCode, KeyDataToShiftState(TWMKeyDown(Msg).KeyData));
          if TWMKeyDown(Msg).CharCode <> 0 then
          begin
            if DroppedDown then
              SendMessage(ListBox.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
            // Alt keys the drop-down logic did not consume must keep their
            // system meaning, such as Alt+F4
            if (Msg.Msg <> WM_SYSKEYDOWN) and
              (not (iifAllowNonListValues in Flags) or
              ((Msg.Msg = WM_KEYDOWN) and
              (TWMKeyDown(Msg).CharCode in [VK_UP, VK_DOWN]))) then
              ExecInherited := False;
          end;
        end;
        PostToInsp :=
          (Msg.Msg = WM_KEYDOWN) and ((KeyDataToShiftState(Msg.LParam) = []) and
          ((Msg.WParam in [VK_NEXT, VK_PRIOR]) or
            (not DroppedDown and (Msg.WParam in [VK_DOWN, VK_UP]))));
        if PostToInsp then
        begin
          PostMessage(Inspector.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
          Msg.Result := 1;
          ExecInherited := False;
        end;
      end;
  end;

  if (Msg.Msg = WM_CHAR) and (Msg.WParam = VK_RETURN) then
  begin
    ExecInherited := False;
    GetEditCtrl.SelectAll;
  end;
  if Msg.Msg = WM_MOUSEWHEEL then
  begin
    if not DroppedDown then
      PostMessage(Inspector.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
    Msg.Result := 1;
    ExecInherited := False;
  end;
  if ExecInherited and (@EditWndPrc <> nil) then
    EditWndPrc(Msg);
  if Msg.Msg = WM_SETFOCUS then
  begin
    { Changing the focus to another Control in the same form via Mouse-Click, if a
      property-editor is active has no effect until you clicked twice on the control.
      Telling the VCL that this control has the focus, fixes the problem. }
    SetFocus;
  end;
end;

function TJvCustomInspectorItem.GetBaseCategory: TJvCustomInspectorItem;
begin
  if IsCategory and (Level = 0) then
    Result := Self
  else
  begin
    Result := Category;
    while (Result <> nil) and (Result.Level > 0) do
      Result := Result.Category;
  end;
end;

function TJvCustomInspectorItem.GetCategory: TJvCustomInspectorItem;
var
  ParItem: TJvCustomInspectorItem;
begin
  ParItem := Parent;
  while (ParItem <> nil) and not ParItem.IsCategory do
    ParItem := ParItem.Parent;
  if (ParItem <> nil) and ParItem.IsCategory then
    Result := ParItem
  else
    Result := nil;
end;

function TJvCustomInspectorItem.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvCustomInspectorItem.GetData: TJvCustomInspectorData;
begin
  Result := FData;
end;

function TJvCustomInspectorItem.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

function TJvCustomInspectorItem.GetDisplayValue: string;
begin
  Result := '';
end;

function TJvCustomInspectorItem.GetDroppedDown: Boolean;
begin
  Result := FDroppedDown;
end;

function TJvCustomInspectorItem.GetEditCtrl: TCustomEdit;
begin
  Result := FEditCtrl;
end;

function TJvCustomInspectorItem.GetEditCtrlDestroying: Boolean;
begin
  Result := FEditCtrlDestroying;
end;

function TJvCustomInspectorItem.GetEditing: Boolean;
begin
  Result := FEditing;
end;

function TJvCustomInspectorItem.GetExpanded: Boolean;
begin
  Result := iifExpanded in Flags;
end;

function TJvCustomInspectorItem.GetFlags: TInspectorItemFlags;
begin
  Result := FFlags;
end;

function TJvCustomInspectorItem.GetHeight: Integer;
begin
  Result := Inspector.ItemHeight;
end;

function TJvCustomInspectorItem.GetHidden: Boolean;
begin
  Result := iifHidden in Flags;
end;

function TJvCustomInspectorItem.GetInspector: TJvCustomInspector;
begin
  Result := FInspector;
end;

function TJvCustomInspectorItem.GetInspectorPaintGeneration: Integer;
begin
  Result := Inspector.PaintGeneration;
end;

function TJvCustomInspectorItem.GetItems(const I: Integer): TJvCustomInspectorItem;
begin
  Result := TJvCustomInspectorItem(FItems[I]);
end;

function TJvCustomInspectorItem.GetLevel: Integer;
var
  Item: TJvCustomInspectorItem;
begin
  Item := Self;
  Result := -1;
  while Item <> nil do
  begin
    if not (iifHidden in Item.Flags) then
      Inc(Result);
    Item := Item.Parent;
  end;
end;

function TJvCustomInspectorItem.GetListBox: TCustomListBox;
begin
  Result := FListBox;
end;

function TJvCustomInspectorItem.GetNextSibling: TJvCustomInspectorItem;
var
  I: Integer;
begin
  Result := Parent;
  if Result <> nil then
  begin
    I := Succ(Result.IndexOf(Self));
    if (I = 0) or (I >= Result.Count) then
      Result := nil
    else
      Result := Result.Items[I];
  end;
end;

function TJvCustomInspectorItem.GetParent: TJvCustomInspectorItem;
begin
  Result := FParent;
end;

function TJvCustomInspectorItem.GetReadOnly: Boolean;
begin
  Result := (iifReadonly in Flags);
end;

function TJvCustomInspectorItem.GetRects(const RectKind: TInspectorPaintRect): TRect;
begin
  if LastPaintGeneration = GetInspectorPaintGeneration then
    Result := FRects[RectKind]
  else
    Result := Rect(0, 0, 0, 0);
end;

procedure TJvCustomInspectorItem.GetValueList(const Strings: TStrings);
begin
  DoGetValueList(Strings);
end;

procedure TJvCustomInspectorItem.InvalidateItem;
begin
  if Inspector <> nil then
    Inspector.InvalidateItem;
end;

procedure TJvCustomInspectorItem.InvalidateList;
begin
  if Inspector <> nil then
    Inspector.InvalidateList;
end;

procedure TJvCustomInspectorItem.InvalidateMetaData;
begin
  InvalidateItem;
end;

function TJvCustomInspectorItem.IsCategory: Boolean;
begin
  Result := False;
end;

procedure TJvCustomInspectorItem.ListExit(Sender: TObject);
begin
  if DroppedDown then
    CloseUp(False);
end;

procedure TJvCustomInspectorItem.ListValueSelect(Sender: TObject);
begin
  CloseUp(True);
end;

procedure TJvCustomInspectorItem.ListDeactivate(Sender: TObject);
begin
  CloseUp(False);
end;

procedure TJvCustomInspectorItem.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and PtInRect(Rects[iprEditButton], Point(X, Y)) then
  begin
    if DroppedDown then
      CloseUp(False)
    else
    begin
      Tracking := True;
      TrackButton(X, Y);
      DropDown;
    end;
  end
  else
  if (Button = mbLeft) and (ssDouble in Shift) then
    if (iifValueList in Flags) and
       (PtInRect(Rects[iprValueArea], Point(X, Y))) then
      SelectValue(1);
end;

procedure TJvCustomInspectorItem.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if Tracking then
  begin
    TrackButton(X, Y);
    if DroppedDown then
    begin
      ListPos := ListBox.ScreenToClient(Inspector.ClientToScreen(Point(X, Y)));
      if PtInRect(ListBox.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(ListBox.Handle, WM_LBUTTONDOWN, 0, PointToLParam(MousePos));
        Exit;
      end;
    end;
  end;
end;

procedure TJvCustomInspectorItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  StopTracking;
end;

procedure TJvCustomInspectorItem.SelectValue(const Delta: Integer);
var
  SL: TStrings;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    GetValueList(SL);
    if SL.Count > 0 then
    begin
      I := SL.IndexOf(DisplayValue);
      Inc(I, Delta);
      while I < 0 do
        I := I + SL.Count;
      while I >= SL.Count do
        I := I - SL.Count;
      EditCtrl.Text := SL[I];
      Apply;
    end;
  finally
    SL.Free;
  end;
end;

procedure TJvCustomInspectorItem.SetDisplayName(Value: string);
begin
  if Value <> FDisplayName then
  begin
    FDisplayName := Value;
    InvalidateItem;
  end;
end;

procedure TJvCustomInspectorItem.SetDisplayValue(const Value: string);
begin
end;

procedure TJvCustomInspectorItem.SetEditCtrl(const Value: TCustomEdit);
begin
  if EditCtrl <> Value then
  begin
    if EditCtrl <> nil then
    begin
      FEditCtrlDestroying := True;
      try
        if Inspector.CanFocus and (EditCtrl.Focused or Inspector.Focused) then // Without "Inspector.Focused" every second click looses the focus
          Inspector.SetFocus;

        // Following Mantis 3391, setting the Focus may set EditCtrl to nil
        if Assigned(EditCtrl) then
        begin
          // Mantis 3994: Only restore if we actually changed it by our own.
          if TMethod(EditCtrl.WindowProc).Code = @EditWndPrc then
            EditCtrl.WindowProc := FEditWndPrc; //Edit_WndProc;
          EditCtrl.Free;
        end;
      finally
        FEditCtrlDestroying := False;
      end;
    end;
    FEditCtrl := Value;

    if EditCtrl <> nil then
      with TCustomEditAccessProtected(EditCtrl) do
      begin
        Ctl3D := False;
        BorderStyle := bsNone;
        Parent := TWinControl(Owner);
      end;
  end;
end;

procedure TJvCustomInspectorItem.SetEditing(const Value: Boolean);
begin
  FEditing := Value;
end;

procedure TJvCustomInspectorItem.SetExpanded(Value: Boolean);
begin
  if Value <> Expanded then
  begin
    if Value then
      Flags := Flags + [iifExpanded]
    else
      Flags := Flags - [iifExpanded];
  end;
end;

procedure TJvCustomInspectorItem.SetFlags(const Value: TInspectorItemFlags);
var
  NewFlags: TInspectorItemFlags;
  OldFlags: TInspectorItemFlags;
begin
  NewFlags := Value;
  if iifAllowNonListValues in NewFlags then
    Include(NewFlags, iifValueList);
  if Flags <> NewFlags then
  begin
    OldFlags := Flags;
    FFlags := NewFlags;
    OldFlags := OldFlags * [iifExpanded, iifHidden];
    NewFlags := NewFlags * [iifExpanded, iifHidden];
    if NewFlags <> OldFlags then
      InvalidateList
    else
      InvalidateItem;
  end;
end;

procedure TJvCustomInspectorItem.SetFocus;
begin
  if (EditCtrl <> nil) and EditCtrl.CanFocus then
    EditCtrl.SetFocus
  else
    Inspector.SetFocus;
end;

procedure TJvCustomInspectorItem.SetInspector(const AInspector: TJvCustomInspector);
begin
  if Parent = nil then
    FInspector := AInspector;
end;

procedure TJvCustomInspectorItem.SetParent(const Value: TJvCustomInspectorItem);
begin
  if Parent <> Value then
    if Parent = nil then
      FParent := Value
    else
      raise EJvInspectorItem.CreateRes(@RsEJvInspItemHasParent);
end;

procedure TJvCustomInspectorItem.SetRects(const RectKind: TInspectorPaintRect;
  Value: TRect);
begin
  UpdateLastPaintGeneration;
  if not EqualRect(Rects[RectKind], Value) then
  begin
    FRects[RectKind] := Value;
    if (RectKind = iprEditValue) and (EditCtrl <> nil) then
    begin
      EditCtrl.BoundsRect := Rects[iprEditValue];
      if DroppedDown then
        CloseUp(False);
    end;
  end;
end;

procedure TJvCustomInspectorItem.StopTracking;
begin
  if Tracking then
  begin
    TrackButton(-1, -1);
    Tracking := False;
    Inspector.MouseCapture := False;
  end;
end;

procedure TJvCustomInspectorItem.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  R := Rects[iprEditButton];
  NewState := PtInRect(R, Point(X, Y));
  if Pressed <> NewState then
  begin
    Pressed := NewState;
    Windows.InvalidateRect(Inspector.Handle, @R, False);
  end;
end;

procedure TJvCustomInspectorItem.Undo;
begin
  if Editing and Assigned(EditCtrl) then
  begin
    if Data.IsAssigned then
      EditCtrl.Text := DisplayValue
    else
      EditCtrl.Text := '';
    EditCtrl.Modified := False;
    EditCtrl.SelectAll;
  end;
end;

procedure TJvCustomInspectorItem.UpdateLastPaintGeneration;
begin
  FLastPaintGen := GetInspectorPaintGeneration;
end;

function TJvCustomInspectorItem.Add(const Item: TJvCustomInspectorItem): Integer;
begin
  Result := Count;
  Insert(Result, Item);
end;

procedure TJvCustomInspectorItem.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if Parent <> nil then
    Parent.FItems.Remove(Self);
  if (Inspector <> nil) and (Inspector.Root <> Self) then
    DoneEdit(True);
  if Inspector <> nil then
    Inspector.RemoveVisible(Self);
  FItems.Free;
  if Data <> nil then
    FData.RemoveItem(Self);
  FItems := nil;
end;

procedure TJvCustomInspectorItem.Clear;
begin
  Inspector.BeginUpdate;
  try
    while Count > 0 do
      Delete(Count - 1);
  finally
    Inspector.EndUpdate;
  end;
end;

procedure TJvCustomInspectorItem.Delete(const Index: Integer);
begin
  if Inspector.Selected = Items[Index] then
  begin
    Inspector.SetSelected(Self);
    if Inspector.Selected = Items[Index] then
      Inspector.SelectedIndex := -1;
  end;
  FItems.Delete(Index);
  InvalidateList;
end;

procedure TJvCustomInspectorItem.DrawEditor(const ACanvas: TCanvas);
var
  R: TRect;
  BFlags: Integer;
begin
  // This reduces the flickering when dragging the divider bar
    if EditCtrl <> nil then
    begin
      ACanvas.Lock;
      try
        EditCtrl.PaintTo(ACanvas.Handle, EditCtrl.Left, EditCtrl.Top);
      finally
        ACanvas.Unlock;
      end;
    end;
    R := Rects[iprEditButton];
    if not IsRectEmpty(R) then
    begin
      BFlags := 0;
      if Assigned(EditCtrl) and (not EditCtrl.Enabled) then
        BFlags := DFCS_INACTIVE
      else
      if Pressed then
        BFlags := DFCS_FLAT or DFCS_PUSHED;
      DrawThemedFrameControl(ACanvas.Handle, R, DFC_SCROLL, BFlags or DFCS_SCROLLCOMBOBOX, Inspector.CurrentPPI);
    end;
end;

procedure TJvCustomInspectorItem.DrawName(const ACanvas: TCanvas);
var
  ARect: TRect;
begin
  ARect := Rects[iprName];
  ACanvas.TextRect(ARect, ARect.Left, ARect.Top, DisplayName);
end;

procedure TJvCustomInspectorItem.DrawValue(const ACanvas: TCanvas);
var
  S: string;
  ARect: TRect;
  SafeColor: TColor;
begin
  if Data = nil then
    S := RsJvInspItemUnInitialized
  else
  try
    if not Data.IsInitialized then
      S := RsJvInspItemUnInitialized
    else
    if not Data.HasValue then
      S := RsJvInspItemNoValue
    else
    if not Data.IsAssigned then
      S := RsJvInspItemUnassigned
    else
      S := DisplayValue;
  except
    S := RsJvInspItemValueException + ExceptObject.ClassName + ': ' +
      Exception(ExceptObject).Message;
  end;
  ARect := Rects[iprValue];
  SafeColor := ACanvas.Brush.Color;
  if Editing then
  begin
    if Inspector.Painter <> nil then
      ACanvas.Brush.Color := Inspector.Painter.BackgroundColor
    else
      ACanvas.Brush.Color := clWindow;
  end;
  try
    if not Editing then
      ACanvas.TextRect(ARect, ARect.Left, ARect.Top, S)
    else
    begin
      ARect := Rects[iprValueArea];
      ACanvas.FillRect(ARect);
      // Reposition the editor if the layout changed since InitEdit placed it,
      // for example when InitEdit ran before the item was first painted
      if (EditCtrl <> nil) and (EditCtrl.BoundsRect <> Rects[iprEditValue]) then
        EditCtrl.BoundsRect := Rects[iprEditValue];
      DrawEditor(ACanvas);
    end;
  finally
    if Editing then
      ACanvas.Brush.Color := SafeColor;
  end;
end;

function TJvCustomInspectorItem.EditFocused: Boolean;
begin
  Result := (EditCtrl <> nil) and EditCtrl.Focused;
end;

procedure TJvCustomInspectorItem.ExpandItems(AExpand: Boolean);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].HasViewableItems then
    begin
      Items[i].Expanded := AExpand;
      Items[i].ExpandItems(AExpand);
    end;
end;

function TJvCustomInspectorItem.HasViewableItems: Boolean;
var
  I: Integer;
begin
  Result := False;
  I := 0;
  while (I < Count) and not Result do
  begin
    Result := not (iifHidden in Items[I].Flags) or
      ((iifExpanded in Items[I].Flags) and Items[I].HasViewableItems);
    Inc(I);
  end;
end;

function TJvCustomInspectorItem.IndexOf(const Item: TJvCustomInspectorItem): Integer;
begin
  Result := Pred(Count);
  while (Result > -1) and (Items[Result] <> Item) do
    Dec(Result);
end;

//=== { TJvInspectorListBox } ================================================

type
  TJvInspectorListBox = class(TJvPopupListBox)
  private
    FOnValueSelect: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FNCClick: Boolean;
    FClicking: Boolean;
    FItem: TJvCustomInspectorItem;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    property OnValueSelect: TNotifyEvent read FOnValueSelect write FOnValueSelect;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property Item: TJvCustomInspectorItem read FItem write FItem;
  end;

procedure TJvInspectorListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
  Pt: TPoint;
begin
  R := Rect(0, 0, Width, Height);
  Pt := Point(X, Y);

  if PtInRect(R, Pt) then
  begin
    if not PtInRect(ClientRect, Pt) then
      FNCClick := True;
    FClicking := True;
    inherited MouseDown(Button, Shift, X, Y);
  end
  else
    FOnDeactivate(Self);
end;

procedure TJvInspectorListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
  Pt: TPoint;
begin
  R := Rect(0, 0, Width, Height);
  Pt := Point(X, Y);

  if FNCClick then
    inherited MouseUp(Button, Shift, X, Y)
  else
  if FClicking then
  begin
    if PtInRect(ClientRect, Pt) then
      FOnValueSelect(Self)
    else
      FOnDeactivate(Self);
  end
  else
    // MouseUps where FClicking is False
    // have originated in the item that
    // opened the list, let it know that
    // the mouse has gone up again.
    FItem.MouseUp(Button, Shift, X, Y);

  FClicking := False;
  FNCClick := False;
end;

procedure TJvCustomInspectorItem.InitEdit;
var
  Edit: TEdit;
begin
  SetEditing(CanEdit);
  if Editing and (FUpdateEditCtrl = 0) then
  begin
    Edit := TJvInspectorEdit.Create(Inspector);
    Edit.OnKeyPress := EditKeyPress;
    Edit.OnExit := EditFocusLost;
    TJvInspectorEdit(Edit).OnKillFocus := EditKillFocus;
    SetEditCtrl(Edit);
    if Inspector.Painter <> nil then
      TCustomEditAccessProtected(EditCtrl).Color := Inspector.Painter.BackgroundColor
    else
      TCustomEditAccessProtected(EditCtrl).Color := clWindow;
    FEditWndPrc := EditCtrl.WindowProc;
    EditCtrl.WindowProc := Edit_WndProc;
    TCustomEditAccessProtected(EditCtrl).AutoSize := False;
    if iifValueList in Flags then
    begin
      FListBox := TJvInspectorListBox.Create(Inspector);
      ListBox.Parent := EditCtrl;
      ListBox.Visible := False;
      TListBox(ListBox).IntegralHeight := True;
      TJvInspectorListBox(ListBox).OnValueSelect := ListValueSelect;
      TJvInspectorListBox(ListBox).OnDeactivate := ListDeactivate;
      TJvInspectorListBox(ListBox).Item := Self;

      TListBox(ListBox).ItemHeight := 11;
      TListBox(ListBox).OnExit := ListExit;
    end;
    // The editor shows the text the value was painted with, so it must use
    // the exact font it was painted with: any metric difference makes the
    // text visibly shift when editing starts or ends
    TCustomEditAccessProtected(EditCtrl).Font.Assign(Inspector.Font);
    if Inspector.Painter <> nil then
      TCustomEditAccessProtected(EditCtrl).Font.Color := Inspector.Painter.ValueColor;
    // BeforeEdit is fired here, after the editor's font has been assigned, so a
    // handler can still customize that font (moved down from just after the
    // editor was created, where any font change was overwritten just above)
    if Assigned(Inspector.BeforeEdit) then
      Inspector.BeforeEdit(Inspector as TObject, Self, EditCtrl);
    EditCtrl.BoundsRect := Rects[iprEditValue];
    TCustomEditAccessProtected(EditCtrl).OnKeyDown := EditKeyDown;
    TCustomEditAccessProtected(EditCtrl).OnKeyPress := EditKeyPress;
    TCustomEditAccessProtected(EditCtrl).OnMouseDown := EditMouseDown;
    TCustomEditAccessProtected(EditCtrl).OnMouseUp := EditMouseUp;
    EditCtrl.Visible := True;
    if Data.IsAssigned then
      EditCtrl.Text := DisplayValue
    else
      EditCtrl.Text := '';
    EditCtrl.Modified := False;
    EditCtrl.SelectAll;
    if EditCtrl.CanFocus and Inspector.Focused then
      EditCtrl.SetFocus;
  end;
end;

procedure TJvCustomInspectorItem.DoneEdit(const CancelEdits: Boolean);
begin
  if Editing and (FUpdateEditCtrl = 0) then
  begin
    if DroppedDown then
      CloseUp(False);
    if not CancelEdits and
       (not Data.IsAssigned or (DisplayValue <> EditCtrl.Text)) then
    begin
      Apply;
      InvalidateItem;
    end;
    FreeAndNil(FListBox);

    SetEditCtrl(nil);
    FEditWndPrc := nil;
  end;
  FEditing := False;
end;

procedure TJvCustomInspectorItem.Insert(const Index: Integer; const Item: TJvCustomInspectorItem);
begin
  Item.SetParent(Self);
  FItems.Insert(Index, Item);
  InvalidateList;
end;

procedure TJvCustomInspectorItem.ScrollInView;
var
  ViewIdx: Integer;
  Item: TJvCustomInspectorItem;
  YDelta: Integer;
begin
  if not Assigned(Inspector) then
    Exit;
  if csDestroying in Inspector.ComponentState then
    Exit; // bugfix attempt. WAP.Self

  ViewIdx := Inspector.VisibleIndex(Self);
  if ViewIdx < 0 then
  begin
    { Find visible parent }
    Item := Parent;
    while (Item <> nil) and (ViewIdx < 0) do
    begin
      ViewIdx := Inspector.VisibleIndex(Item);
      if ViewIdx < 0 then
        Item := Item.Parent;
    end;
  end;
  if ViewIdx > -1 then
  begin
    if Inspector.TopIndex > ViewIdx then
      Inspector.TopIndex := ViewIdx
    else
    if (Inspector.IdxToY(ViewIdx) - Inspector.IdxToY(Inspector.TopIndex) + Height) > Inspector.ClientHeight then
    begin
      YDelta := (Inspector.IdxToY(ViewIdx) + Height - Inspector.ClientHeight - Inspector.IdxToY(Inspector.TopIndex));
      ViewIdx := Inspector.TopIndex;
      while (YDelta > 0) and (ViewIdx < Inspector.VisibleCount) do
      begin
        Dec(YDelta, Inspector.VisibleItems[ViewIdx].Height);
        Inc(ViewIdx);
      end;
      if ViewIdx < Inspector.VisibleCount then
        Inspector.TopIndex := ViewIdx;
    end;
  end;
end;

//=== { TJvInspectorCustomCategoryItem } =====================================

function TJvInspectorCustomCategoryItem.IsCategory: Boolean;
begin
  Result := True;
end;

procedure TJvInspectorCustomCategoryItem.SetFlags(const Value: TInspectorItemFlags);
var
  NewFlags: TInspectorItemFlags;
begin
  NewFlags := Value - [iifValueList, iifAllowNonListValues] +
    [iifReadonly];
  inherited SetFlags(NewFlags);
end;

//=== { TJvInspectorEnumItem } ===============================================

procedure TJvInspectorEnumItem.SetFlags(const Value: TInspectorItemFlags);
var
  TmpFlags: TInspectorItemFlags;
begin
  TmpFlags := Value;
  Include(TmpFlags, iifValueList);
  inherited SetFlags(TmpFlags);
end;

//=== { TJvInspectorStringItem } =============================================

function TJvInspectorStringItem.GetDisplayValue: string;
begin
  Result := Data.AsString;
end;

procedure TJvInspectorStringItem.SetDisplayValue(const Value: string);
begin
  Data.AsString := Value;
end;

//=== { TJvInspectorBooleanItem } ============================================

procedure TJvInspectorBooleanItem.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Bool: Boolean;
begin
  Bool := not (Data.AsOrdinal <> Ord(False));
  if Editing and (Shift = []) and (Key = VK_SPACE) then
  begin
    Data.AsOrdinal := Ord(Bool);
    InvalidateItem;
  end;
end;

procedure TJvInspectorBooleanItem.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Bool: Boolean;
begin
  if Data.IsAssigned then
    Bool := not (Data.AsOrdinal <> Ord(False))
  else
    Bool := True;
  // Treat the second click of a double-click as a normal click, so two quick
  // clicks toggle the box twice like a standard Windows check box
  if ssDouble in Shift then
    Shift := Shift - [ssDouble];
  if PtInRect(FCheckRect, Point(X, Y)) and (Shift = [ssLeft]) and Editing then
  begin
    Data.AsOrdinal := Ord(Bool);
    InvalidateItem;
  end;
end;

procedure TJvInspectorBooleanItem.DoneEdit(const CancelEdits: Boolean = False);
begin
  SetEditing(False);
end;

procedure TJvInspectorBooleanItem.DrawValue(const ACanvas: TCanvas);
var
  Bool: Boolean;
  ARect: TRect;
  Rgn, SaveRgn: HRGN;
  HasRgn: Boolean;
  ClipRect: TRect;
  BoxSize: Integer;
  BFlags: UINT;
  LabelText: string;
  LabelRect: TRect;
begin
  if Data.IsInitialized and Data.IsAssigned and Data.HasValue then
    Bool := Data.AsOrdinal <> Ord(False)
  else
    Bool := False;

  if Editing and Data.IsAssigned then
  begin
    if Inspector.Painter <> nil then
      ACanvas.Brush.Color := Inspector.Painter.BackgroundColor
    else
      ACanvas.Brush.Color := clWindow;
  end;
  ACanvas.FillRect(Rects[iprValueArea]);
  // The check box is drawn themed, scaled to the inspector's current dpi,
  // and centered in the row
  BoxSize := MulDiv(13, Inspector.CurrentPPI, 96);
  ARect := Rects[iprValueArea];
  Inc(ARect.Left, MulDiv(2, Inspector.CurrentPPI, 96));
  Inc(ARect.Top, (RectHeight(ARect) - BoxSize) div 2);
  ARect.Right := ARect.Left + BoxSize;
  ARect.Bottom := ARect.Top + BoxSize;
  { Remember current clipping region }
  SaveRgn := CreateRectRgn(0, 0, 1, 1);
  HasRgn := GetClipRgn(ACanvas.Handle, SaveRgn) > 0;
  { Clip all outside of the item rectangle }
  IntersectRect(ClipRect, ARect, Rects[iprValueArea]);
  FCheckRect := ClipRect;
  Rgn := CreateRectRgn(ClipRect.Left, ClipRect.Top, ClipRect.Right, ClipRect.Bottom);
  SelectClipRgn(ACanvas.Handle, Rgn);
  DeleteObject(Rgn);
  try
    BFlags := DFCS_BUTTONCHECK;
    if Bool then
      BFlags := BFlags or DFCS_CHECKED;
    DrawThemedFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, BFlags, Inspector.CurrentPPI);
  finally
    { restore previous clipping region }
    if HasRgn then
      SelectClipRgn(ACanvas.Handle, SaveRgn)
    else
      SelectClipRgn(ACanvas.Handle, 0);
    DeleteObject(SaveRgn);
  end;
  // Draw a yes/no label after the check box, vertically centered like the box
  // and drawn with the canvas font so it picks up any bold style the painter
  // applied for a value present in the script (like the Delphi object
  // inspector's True/False label after its check box)
  if Bool then
    LabelText := 'yes'
  else
    LabelText := 'no';
  LabelRect := Rects[iprValueArea];
  LabelRect.Left := ARect.Right + MulDiv(4, Inspector.CurrentPPI, 96);
  ACanvas.Brush.Style := bsClear;
  try
    ACanvas.TextOut(LabelRect.Left,
      LabelRect.Top + (RectHeight(LabelRect) - ACanvas.TextHeight(LabelText)) div 2,
      LabelText);
  finally
    ACanvas.Brush.Style := bsSolid;
  end;
end;

procedure TJvInspectorBooleanItem.InitEdit;
begin
  SetEditing(CanEdit);
end;

//=== { TJvCustomInspectorData } =============================================

constructor TJvCustomInspectorData.CreatePrim(const AName: string;
  ATypeInfo: PTypeInfo);
begin
  inherited Create;
  Name := AName;
  TypeInfo := ATypeInfo;
end;

procedure TJvCustomInspectorData.CheckReadAccess;
begin
  if not IsInitialized then
    raise EJvInspectorData.CreateRes(@RsEJvInspDataNotInit);
end;

procedure TJvCustomInspectorData.CheckWriteAccess;
begin
  if not IsInitialized then
    raise EJvInspectorData.CreateRes(@RsEJvInspDataNotInit);
end;

function TJvCustomInspectorData.GetItems(I: Integer): TJvCustomInspectorItem;
begin
  if (I < Low(FItems)) or (I > High(FItems)) then
    TList.Error(SListIndexError, I);
  Result := FItems[I];
end;

function TJvCustomInspectorData.GetName: string;
begin
  Result := FName;
end;

function TJvCustomInspectorData.GetTypeInfo: PTypeInfo;
begin
  Result := FTypeInfo;
end;

procedure TJvCustomInspectorData.Invalidate;
var
  I: Integer;
begin
  for I := High(FItems) downto Low(FItems) do
    FItems[I].InvalidateItem;
end;

procedure TJvCustomInspectorData.RemoveItem(const Item: TJvCustomInspectorItem);
var
  I: Integer;
begin
  I := High(FItems);
  while (I >= 0) do
  begin
    if Items[I] = Item then
      Break;
    Dec(I);
  end;
  if I >= 0 then
  begin
    if I < High(FItems) then
      Move(FItems[I + 1], FItems[I], (High(FItems) - I) * SizeOf(TJvCustomInspectorItem));
    SetLength(FItems, High(FItems));
  end;
  if Length(FItems) = 0 then
    Destroy;
end;

procedure TJvCustomInspectorData.SetName(const Value: string);
begin
  if Value <> Name then
  begin
    FName := Value;
    Invalidate;
  end;
end;

procedure TJvCustomInspectorData.SetTypeInfo(Value: PTypeInfo);
begin
  if Value <> TypeInfo then
  begin
    FTypeInfo := Value;
    Invalidate;
  end;
end;

procedure TJvCustomInspectorData.BeforeDestruction;
var
  I: Integer;
begin
  for I := High(FItems) downto Low(FItems) do
    Items[I].Free;
  inherited BeforeDestruction;
end;

class function TJvCustomInspectorData.ItemRegister: TJvInspectorRegister;
begin
  if GlobalGenItemReg = nil then
  begin
    GlobalGenItemReg := TJvInspectorRegister.Create;
   // register
    RegisterDataTypeKinds;
  end;
  Result := GlobalGenItemReg;
end;

function TJvCustomInspectorData.NewItem(const AParent: TJvCustomInspectorItem): TJvCustomInspectorItem;
var
  ItemClass: TJvInspectorItemClass;
  RegItem: TJvCustomInspectorRegItem;
begin
  Result := nil;
  RegItem := ItemRegister.FindMatch(Self);
  if RegItem <> nil then
  begin
    ItemClass := RegItem.ItemClass;
    if ItemClass <> nil then
    begin
      Result := ItemClass.Create(AParent, Self);
      if Result <> nil then
      begin
        SetLength(FItems, Length(FItems) + 1);
        FItems[High(FItems)] := Result;
        Result.InvalidateMetaData;
      end;
    end;
  end;
end;

//=== { TJvInspectorEventData } ==============================================

function TJvInspectorEventData.DoGetAsOrdinal: Int64;
begin
  if Assigned(FOnGetAsOrdinal) then
    FOnGetAsOrdinal(Self, Result)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
end;

function TJvInspectorEventData.DoGetAsString: string;
begin
  if Assigned(FOnGetAsString) then
    FOnGetAsString(Self, Result)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
end;

procedure TJvInspectorEventData.DoSetAsOrdinal(Value: Int64);
begin
  if Assigned(FOnSetAsOrdinal) then
    FOnSetAsOrdinal(Self, Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
end;

procedure TJvInspectorEventData.DoSetAsString(Value: string);
begin
  if Assigned(FOnSetAsString) then
    FOnSetAsString(Self, Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
end;

function TJvInspectorEventData.GetAsOrdinal: Int64;
begin
  CheckReadAccess;
  if TypeInfo.Kind in [tkInteger, tkChar, tkEnumeration, tkSet, tkWChar] then
  begin
    case GetTypeData(TypeInfo).OrdType of
      otSByte:
        Result := Shortint(DoGetAsOrdinal);
      otUByte:
        Result := Byte(DoGetAsOrdinal);
      otSWord:
        Result := Smallint(DoGetAsOrdinal);
      otUWord:
        Result := Word(DoGetAsOrdinal);
      otSLong:
        Result := Longint(DoGetAsOrdinal);
      otULong:
        Result := Longword(DoGetAsOrdinal);
    else
      Result := 0;
    end;
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
end;

function TJvInspectorEventData.GetAsString: string;
begin
  CheckReadAccess;
  if TypeInfo.Kind in tkStrings then
    Result := DoGetAsString
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
end;

procedure TJvInspectorEventData.SetAsOrdinal(const Value: Int64);
var
  MinValue: Int64;
  MaxValue: Int64;
begin
  CheckWriteAccess;
  if TypeInfo.Kind in [tkInteger, tkChar, tkEnumeration, tkWChar] then
  begin
    if GetTypeData(TypeInfo).OrdType <> otULong then
    begin
      MinValue := GetTypeData(TypeInfo).MinValue;
      MaxValue := GetTypeData(TypeInfo).MaxValue;
    end
    else
    begin
      MinValue := Longword(GetTypeData(TypeInfo).MinValue);
      MaxValue := Longword(GetTypeData(TypeInfo).MaxValue);
    end;
    if (Value < MinValue) or (Value > MaxValue) then
      raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
    case GetTypeData(TypeInfo).OrdType of
      otSByte:
        DoSetAsOrdinal(Shortint(Value));
      otUByte:
        DoSetAsOrdinal(Byte(Value));
      otSWord:
        DoSetAsOrdinal(Smallint(Value));
      otUWord:
        DoSetAsOrdinal(Word(Value));
      otSLong:
        DoSetAsOrdinal(Longint(Value));
      otULong:
        DoSetAsOrdinal(Longword(Value));
    end;
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
  Invalidate;
end;

procedure TJvInspectorEventData.SetAsString(const Value: string);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkUString then
    DoSetAsString(Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
  Invalidate;
end;

procedure TJvInspectorEventData.SetOnGetAsOrdinal(Value: TJvInspAsOrdinal);
begin
  if @FOnGetAsOrdinal <> @Value then
  begin
    FOnGetAsOrdinal := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnGetAsString(Value: TJvInspAsString);
begin
  if @FOnGetAsString <> @Value then
  begin
    FOnGetAsString := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnSetAsOrdinal(Value: TJvInspAsOrdinal);
begin
  if @FOnSetAsOrdinal <> @Value then
  begin
    FOnSetAsOrdinal := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnSetAsString(Value: TJvInspAsString);
begin
  if @FOnSetAsString <> @Value then
  begin
    FOnSetAsString := Value;
    Invalidate;
  end;
end;

function TJvInspectorEventData.HasValue: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorEventData.IsAssigned: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorEventData.IsInitialized: Boolean;
begin
  Result := (TypeInfo <> nil) and (Assigned(OnGetAsOrdinal) or Assigned(OnGetAsString));
end;

class function TJvInspectorEventData.New(const AParent: TJvCustomInspectorItem;
  const AName: string; ATypeInfo: PTypeInfo): TJvCustomInspectorItem;
var
  Data: TJvInspectorEventData;
begin
  Data := TJvInspectorEventData(CreatePrim(AName, ATypeInfo));
  Data.FParent := AParent;
  Result := Data.NewItem(AParent);
end;

//=== { TJvInspectorRegister } ===============================================

constructor TJvInspectorRegister.Create;
begin
  inherited Create;
  FItems := TObjectList.Create(True);
end;

destructor TJvInspectorRegister.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJvInspectorRegister.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvInspectorRegister.GetItems(const I: Integer): TJvCustomInspectorRegItem;
begin
  Result := TJvCustomInspectorRegItem(FItems[I]);
end;

procedure TJvInspectorRegister.Add(const RegItem: TJvCustomInspectorRegItem);
begin
  FItems.Add(RegItem);
end;

function TJvInspectorRegister.FindMatch(const ADataObj: TJvCustomInspectorData): TJvCustomInspectorRegItem;
var
  I: Integer;
begin
  Result := nil;
  for I := Pred(Count) downto 0 do
    if Items[I].IsMatch(ADataObj) then
    begin
      Result := Items[I];
      Break;
    end;
end;

//=== { TJvCustomInspectorRegItem } ==========================================

constructor TJvCustomInspectorRegItem.Create(const AItemClass: TJvInspectorItemClass);
begin
  inherited Create;
  FItemClass := AItemClass;
end;

function TJvCustomInspectorRegItem.GetItemClass: TJvInspectorItemClass;
begin
  Result := FItemClass;
end;

function TJvCustomInspectorRegItem.IsMatch(const ADataObj: TJvCustomInspectorData): Boolean;
begin
  Result := MatchValue(ADataObj) <> 0;
end;

//=== { TJvInspectorTypeInfoRegItem } ========================================

constructor TJvInspectorTypeInfoRegItem.Create(const AItemClass: TJvInspectorItemClass;
   ATypeInfo: PTypeInfo);
begin
  inherited Create(AItemClass);
  FTypeInfo := ATypeInfo;
end;

function TJvInspectorTypeInfoRegItem.GetTypeInfo: PTypeInfo;
begin
  Result := FTypeInfo;
end;

function TJvInspectorTypeInfoRegItem.MatchValue(const ADataObj: TJvCustomInspectorData): Integer;
begin
  if ADataObj.TypeInfo = TypeInfo then
    Result := 100
  else
    Result := 0;
end;

//=== { TJvInspectorTypeKindRegItem } ========================================

constructor TJvInspectorTypeKindRegItem.Create(const AItemClass: TJvInspectorItemClass;
  const ATypeKind: TTypeKind);
begin
  inherited Create(AItemClass);
  FTypeKind := ATypeKind;
end;

function TJvInspectorTypeKindRegItem.GetTypeKind: TTypeKind;
begin
  Result := FTypeKind;
end;

function TJvInspectorTypeKindRegItem.MatchValue(const ADataObj: TJvCustomInspectorData): Integer;
begin
  if ADataObj.TypeInfo.Kind = TypeKind then
    Result := 100
  else
    Result := 0;
end;

procedure RegisterDataTypeKinds;
begin
  if TJvCustomInspectorData.ItemRegister = nil then
    raise EJvInspectorReg.CreateRes(@RsEJvInspNoGenReg);
  with TJvCustomInspectorData.ItemRegister do
  begin
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorStringItem, tkUString));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorBooleanItem, TypeInfo(Boolean)));
  end;
end;

initialization

finalization
  FreeAndNil(GlobalCanvasStack);
  FreeAndNil(FieldGlobalInspReg);
  FreeAndNil(GlobalGenItemReg);

end.
