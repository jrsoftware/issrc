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
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Contnrs, TypInfo, IniFiles,
  Windows, Messages, Graphics, Controls, StdCtrls, ExtCtrls,
  JvAutoComplete, JvInspectorSupport;

const
  { Inspector Row Size constants }
  irsNoReSize = $00000000;
  irsNameHeight = $10000000;
  irsValueHeight = $20000000;
  irsItemHeight = $40000000;
  irsValueMask = $0FFFFFFF;

type
  // early declarations
  TJvCustomInspector = class;
  TJvInspectorPainter = class;
  TJvInspectorItemSizing = class;
  TJvCustomInspectorItem = class;
  TJvInspectorCustomCategoryItem = class;
  TJvCustomInspectorData = class;
  TJvInspectorRegister = class;
  TJvCustomInspectorRegItem = class;
  TJvInspectorEventData = class;

  // For some reason, the hpp generator won't recognize our early
  // declarations just yet, so we output them manually.
  // In the process, we are careful to enclose them in a namespace
  // declaration or we would create two ambiguities for those classes.
  // The first one being between TJvCustomInspectorItem and
  // Jvinspector::TJvCustomInspectorItem.
  // This would puzzle the users quite a bit when they use the header
  // and would force them to use an ugly #define to lift the ambiguity.
  // And even so, this would trigger other problems.
  // So we'd better be careful here.
  {$HPPEMIT 'namespace Jvinspector'}
  {$HPPEMIT '{'}
  {$HPPEMIT 'class TJvCustomInspectorItem;'}
  {$HPPEMIT 'class TJvCustomInspectorData;'}
  (*$HPPEMIT '}'*)
  {$HPPEMIT ''}

  TInspectorItemFlag = (iifReadonly, iifHidden, iifExpanded, iifVisible,
    iifQualifiedNames, iifAutoUpdate, iifMultiLine, iifValueList,
    iifAllowNonListValues, iifOwnerDrawListFixed, iifOwnerDrawListVariable,
    iifEditButton, iifEditFixed, iifOwnerDrawListMaxHeight);
  TInspectorItemFlags = set of TInspectorItemFlag;

  TInspectorPaintRect = (iprItem, iprButtonArea, iprBtnSrcRect, iprBtnDstRect,
    iprNameArea, iprName, iprValueArea, iprValue, iprEditValue, iprEditButton,
    iprUser1, iprUser2, iprUser3, iprUser4, iprUser5, iprUser6);

  TItemRowSizing = type Integer;

  TInspectorItemSortKind = (iskNone, iskName, iskManual, iskCustom);

  TJvInspectorItemClass = class of TJvCustomInspectorItem;
  TJvInspectorDataClass = class of TJvCustomInspectorData;
  TJvInspectorPainterClass = class of TJvInspectorPainter;

  TJvInspectorItemInstances = array of TJvCustomInspectorItem;
  TJvInspectorDataInstances = array of TJvCustomInspectorData;

  TInspectorItemGetValueListEvent = procedure(Item: TJvCustomInspectorItem; Values: TStrings) of object;
  TInspectorItemSortCompare = function(Item1, Item2: TJvCustomInspectorItem): Integer of object;
  TJvInspAsFloat = procedure(Sender: TJvInspectorEventData; var Value: Extended) of object;
  TJvInspAsInt64 = procedure(Sender: TJvInspectorEventData; var Value: Int64) of object;
  TJvInspAsMethod = procedure(Sender: TJvInspectorEventData; var Value: TMethod) of object;
  TJvInspAsString = procedure(Sender: TJvInspectorEventData; var Value: string) of object;
  TJvInspAsSet = procedure(Sender: TJvInspectorEventData; var Value; var BufSize: Integer) of object;
  TJvInspSupportsMethodPointers = procedure(Sender: TJvInspectorEventData; var SupportsTMethod: Boolean) of object;
  // new event types (sept 2004) -wp
  TInspectorBeforeEditEvent = procedure(Sender: TObject; Item: TJvCustomInspectorItem; Edit: TCustomEdit) of object;

  EJvInspector = class(EJVCLException);
  EJvInspectorItem = class(EJvInspector);
  EJvInspectorData = class(EJvInspector);
  EJvInspectorReg = class(EJvInspector);

  TOnJvInspectorSetItemColors = procedure(Item: TJvCustomInspectorItem; Canvas: TCanvas) of object;

  TJvCustomInspectorBase = TJvCustomControl;

  TJvCustomInspector = class(TJvCustomInspectorBase)
  private
    FCollapseButton: TBitmap;
    FDivider: Integer;
    FDraggingDivider: Boolean;
    FExpandButton: TBitmap;
    FImageHeight: Integer;
    FItemHeight: Integer;
    FLockCount: Integer;
    FNeedRebuild: Boolean;
    FNeedRedraw: Boolean;
    FSortNotificationList: TList;
    FPainter: TJvInspectorPainter;
    FPaintGen: Integer;
    FReadOnly: Boolean;
    FRoot: TJvCustomInspectorItem;
    FRowSizing: Boolean;
    FRowSizingItem: TJvCustomInspectorItem;
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
    //    FOnMouseDown: TInspectorMouseDownEvent;
  protected
    function CalcImageHeight: Integer; virtual;
    function CalcItemIndex(X, Y: Integer; var Rect: TRect): Integer; virtual;
    function CalcItemRect(const Item: TJvCustomInspectorItem): TRect; virtual;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure CMActivate(var Msg: TCMActivate); message CM_ACTIVATE;
    procedure CMDeactivate(var Msg: TCMActivate); message CM_DEACTIVATE;
    function GetButtonRect(const ItemIndex: Integer): TRect; virtual;
    function GetCollapseButton: TBitmap; virtual;
    function GetDivider: Integer; virtual;
    function GetExpandButton: TBitmap; virtual;
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
    procedure InvalidateHeight; virtual;
    procedure InvalidateItem; virtual;
    procedure InvalidateList; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifySort(const Item: TJvCustomInspectorItem); virtual;
    procedure Paint; override;
    procedure RebuildVisible; virtual;
    procedure RemoveNotifySort(const Item: TJvCustomInspectorItem); virtual;
    procedure RemoveVisible(const Item: TJvCustomInspectorItem); virtual;
    procedure BoundsChanged; override;
    function ScrollFactorV: Extended; virtual;
    procedure SetCollapseButton(const Value: TBitmap); virtual;
    procedure SetDivider(Value: Integer); virtual;
    procedure SetExpandButton(const Value: TBitmap); virtual;
    procedure SetItemHeight(Value: Integer); virtual;
    procedure SetLockCount(const Value: Integer); virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;
    procedure SetSelected(const Value: TJvCustomInspectorItem); virtual;
    procedure SetSelectedIndex(Value: Integer); virtual;
    procedure SetTopIndex(Value: Integer); virtual;
    procedure UpdateScrollBars; virtual;
    function ViewHeight: Integer;
    function ViewRect: TRect; virtual;
    function ViewWidth: Integer;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    procedure GetDlgCode(var Code: TDlgCodes); override;
    procedure FocusSet(PrevWnd: THandle); override;
    procedure FocusKilled(NextWnd: THandle); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure ShowScrollBars(Bar: Integer; Visible: Boolean); virtual;
    function YToIdx(const Y: Integer): Integer; virtual;
    property CollapseButton: TBitmap read GetCollapseButton write SetCollapseButton;
    property ExpandButton: TBitmap read GetExpandButton write SetExpandButton;
    property Divider: Integer read GetDivider write SetDivider;
    property DraggingDivider: Boolean read FDraggingDivider write FDraggingDivider;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ImageHeight: Integer read GetImageHeight;
    property LockCount: Integer read GetLockCount;
    property NeedRebuild: Boolean read FNeedRebuild write FNeedRebuild;
    property NeedRedraw: Boolean read FNeedRedraw write FNeedRedraw;
    property SortNotificationList: TList read FSortNotificationList;
    property BevelKind default bkTile;
    property Painter: TJvInspectorPainter read GetPainter;
    property PaintGeneration: Integer read FPaintGen;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Root: TJvCustomInspectorItem read GetRoot;
    property RowSizing: Boolean read FRowSizing write FRowSizing;
    property RowSizingItem: TJvCustomInspectorItem read FRowSizingItem write FRowSizingItem;
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
    function FocusedItem: TJvCustomInspectorItem; virtual;
    function VisibleIndex(const AItem: TJvCustomInspectorItem): Integer; virtual;
    procedure RefreshValues;
    procedure SaveValues;
    procedure Clear;
  end;

  TJvInspector = class(TJvCustomInspector)
  public
    property LockCount;
    property Root;
    property Selected;
    property SelectedIndex;
    property TopIndex;
    property VisibleCount;
    property VisibleItems;
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelKind;
    property BevelInner default bvNone;
    property BevelOuter;
    property BevelWidth;
    property CollapseButton;
    property Divider default 75;
    property ExpandButton;
    property Font;
    property ItemHeight;
    property Painter;
    property PopupMenu;
    property ReadOnly default False;
    property TabStop;
    property TabOrder;
    property OnContextPopup;
    property BeforeEdit; // Low level hook for customizing TEdit/TMemo after objects are created, just before editing.

    // Standard control events
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

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
    FDrawNameEndEllipsis: Boolean;
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
    function GetNameHeight(const AItem: TJvCustomInspectorItem): Integer; virtual;
    function GetRects(const Index: TInspectorPaintRect): TRect; virtual;
    function GetSelectedColor: TColor; virtual;
    function GetDrawNameEndEllipsis: Boolean; virtual;
    function GetValueHeight(const AItem: TJvCustomInspectorItem): Integer; virtual;
    procedure HideEditor; virtual;
    procedure InitializeColors; virtual;
    function Loading: Boolean;
    procedure Paint; virtual;
    procedure PaintDivider(const X, YTop, YBottom: Integer); virtual;
    procedure PaintItem(var ARect: TRect; const AItemIndex: Integer); overload; virtual;
    procedure PaintItem(const AItem: TJvCustomInspectorItem); overload; virtual;
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
    procedure SetDrawNameEndEllipsis(Value: Boolean); virtual;
    procedure TeardownItem; virtual;
    property ButtonImage: TBitmap read FButtonImage write FButtonImage;
    property Canvas: TCanvas read FCanvas write FCanvas;
    property Initializing: Boolean read FInitializing write FInitializing;
    property Inspector: TJvCustomInspector read FInspector;
    property InternalCollapseButton: TBitmap read FInternalCollapseButton;
    property InternalExpandButton: TBitmap read FInternalExpandButton;
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
    property DrawNameEndEllipsis: Boolean read GetDrawNameEndEllipsis write SetDrawNameEndEllipsis;
  end;

  TJvInspectorBorlandNETBasePainter = class(TJvInspectorPainter)
  private
    FRealButtonAreaWidth: Integer;
  protected
    procedure ApplyNameFont; override;
    procedure ApplyValueFont; override;
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

  TJvInspectorItemSizing = class(TPersistent)
  private
    FMinHeight: TItemRowSizing;
    FSizable: Boolean;
    FSizingFactor: TItemRowSizing;
  protected
    Item: TJvCustomInspectorItem;
    function GetMinHeight: TItemRowSizing;
    function GetSizable: Boolean;
    function GetSizingFactor: TItemRowSizing;
    procedure SetMinHeight(Value: TItemRowSizing);
    procedure SetSizable(Value: Boolean);
    procedure SetSizingFactor(Value: TItemRowSizing);
  public
    constructor Create(const AItem: TJvCustomInspectorItem);
    procedure Assign(Source: TPersistent); override;
    property MinHeight: TItemRowSizing read GetMinHeight write SetMinHeight;
    property Sizable: Boolean read GetSizable write SetSizable;
    property SizingFactor: TItemRowSizing read GetSizingFactor write SetSizingFactor;
  end;

  TJvCustomInspectorItem = class(TPersistent)
  private
    FData: TJvCustomInspectorData;
    FDisplayIndex: Integer;
    FDisplayName: string;
    FDroppedDown: Boolean;
    FEditCtrlDestroying: Boolean;
    FEditCtrl: TCustomEdit;
    FEditWndPrc: TWndMethod;
    FEditing: Boolean;
    FAutoComplete: TJvEditListBoxAutoComplete;
    FFlags: TInspectorItemFlags;
    FHeight: Integer;
    FInspector: TJvCustomInspector;
    FItems: TObjectList;
    FListBox: TCustomListBox;
    FOnCompare: TInspectorItemSortCompare;
    FOnGetValueList: TInspectorItemGetValueListEvent;
    FParent: TJvCustomInspectorItem;
    FLastPaintGen: Integer;
    FPressed: Boolean;
    FRects: array [TInspectorPaintRect] of TRect;
    FRowSizing: TJvInspectorItemSizing;
    FSortKind: TInspectorItemSortKind;
    FTracking: Boolean;
    FUserData: Pointer;
    FDropDownCount: Integer;
    FUpdateEditCtrl: Integer; // Used to prevent EditCtrl destruction while in Apply().
    FLastEditCtrlText: string;
  protected
    function GetName: string; virtual; // NEW: Warren added.
    procedure AlphaSort;
    procedure Apply; virtual;
    procedure ApplyDisplayIndices(const ItemList: TList); virtual;
    procedure BuildDisplayableList(const ItemList: TList); virtual;
    function CanEdit: Boolean; virtual;
    procedure CloseUp(Accept: Boolean); virtual;
    procedure DataSort;
    procedure Deactivate; dynamic;
    function DoCompare(const Item: TJvCustomInspectorItem): Integer; virtual;
    procedure DoDefaultDrawListItem(ACanvas: TCanvas; Rect: TRect; const AText: string); virtual;
    procedure DoDrawListItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState); virtual;
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState); virtual;
    procedure DoGetValueList(const Strings: TStrings); virtual;
    procedure DoMeasureListItem(Control: TWinControl; Index: Integer;
      var Height: Integer); virtual;
    procedure DoMeasureListItemWidth(Control: TWinControl; Index: Integer;
      var Width: Integer); virtual;
    procedure DropDown; dynamic;
    procedure EditChange(Sender: TObject); virtual;
    procedure EditFocusLost(Sender: TObject); dynamic;
    procedure EditKillFocus(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char); dynamic;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); dynamic;
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure EditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
    procedure EditMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure Edit_WndProc(var Msg: TMessage); virtual;
    procedure AutoCompleteStart(Sender: TObject); dynamic;
    function GetAutoUpdate: Boolean; virtual;
    function GetBaseCategory: TJvCustomInspectorItem; virtual;
    function GetCategory: TJvCustomInspectorItem; virtual;
    function GetCount: Integer; virtual;
    function GetData: TJvCustomInspectorData; virtual;
    function GetDisplayIndex: Integer; virtual;
    function GetDisplayName: string; virtual; // NOTE THIS USES DISPLAY NAME PROPERTIES TO BUILD ITS RESULT
    function GetFullName: string; // NOTE THIS USES THE INTERNAL NAME properties to build its result.
    function GetDisplayParent: TJvCustomInspectorItem; virtual;
    function GetDisplayValue: string; virtual;
    function GetDroppedDown: Boolean; virtual;
    function GetEditCtrl: TCustomEdit; virtual;
    function GetEditCtrlDestroying: Boolean; virtual;
    function GetEditing: Boolean; virtual;
    function GetExpanded: Boolean; virtual;
    function GetFlags: TInspectorItemFlags; virtual;
    function GetHeight: Integer; virtual;
    function GetHeightFactor: Integer; virtual;
    function GetHidden: Boolean; virtual;
    function GetInspector: TJvCustomInspector; virtual;
    function GetInspectorPaintGeneration: Integer;
    function GetItems(const I: Integer): TJvCustomInspectorItem; virtual;
    function GetLevel: Integer; virtual;
    function GetListBox: TCustomListBox; virtual;
    function GetMultiline: Boolean; virtual;
    function GetNextSibling: TJvCustomInspectorItem; virtual;
    function GetParent: TJvCustomInspectorItem; virtual;
    function GetQualifiedNames: Boolean; virtual;
    function GetReadOnly: Boolean; virtual;
    function GetRects(const RectKind: TInspectorPaintRect): TRect; virtual;
    function GetRowSizing: TJvInspectorItemSizing; virtual;
    function GetSortKind: TInspectorItemSortKind; virtual;
    function GetSortName: string; virtual;
    procedure GetValueList(const Strings: TStrings); virtual;
    function GetVisible: Boolean; virtual;
    procedure InvalidateItem; virtual;
    procedure InvalidateList; virtual;
    procedure InvalidateSort; virtual;
    procedure InvalidateMetaData; virtual;
    function IsCategory: Boolean; virtual;
    procedure ListExit(Sender: TObject); virtual;
    {procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;}
    procedure ListValueSelect(Sender: TObject); virtual;
    procedure ListDeactivate(Sender: TObject); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure NaturalSort;
    procedure SelectValue(const Delta: Integer); virtual;
    procedure SetAutoUpdate(const Value: Boolean); virtual;
    procedure SetDisplayIndex(const Value: Integer); virtual;
    procedure SetDisplayIndexValue(const Value: Integer); virtual;
    procedure SetDisplayName(Value: string); virtual;
    procedure SetDisplayValue(const Value: string); virtual;
    procedure SetEditCtrl(const Value: TCustomEdit); virtual;
    procedure SetEditing(const Value: Boolean); virtual;
    procedure SetExpanded(Value: Boolean); virtual;
    procedure SetFlags(const Value: TInspectorItemFlags); virtual;
    procedure SetFocus; virtual;
    procedure SetHeight(Value: Integer); virtual;
    procedure SetHeightFactor(Value: Integer); virtual;
    procedure SetHidden(Value: Boolean); virtual;
    procedure SetInspector(const AInspector: TJvCustomInspector); virtual;
    procedure SetMultiline(const Value: Boolean); virtual;
    procedure SetOnCompare(const Value: TInspectorItemSortCompare); virtual;
    procedure SetParent(const Value: TJvCustomInspectorItem); virtual;
    procedure SetQualifiedNames(const Value: Boolean); virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;
    procedure SetRects(const RectKind: TInspectorPaintRect; Value: TRect); virtual;
    procedure SetRowSizing(Value: TJvInspectorItemSizing); virtual;
    procedure SetSortKind(Value: TInspectorItemSortKind); virtual;
    procedure SetVisible(Value: Boolean); virtual;
    procedure StopTracking; dynamic;
    procedure TrackButton(X, Y: Integer); dynamic;
    procedure Undo; virtual;
    procedure UpdateDisplayOrder(const Item: TJvCustomInspectorItem; const NewIndex: Integer); virtual;
    procedure UpdateLastPaintGeneration;
    property BaseCategory: TJvCustomInspectorItem read GetBaseCategory;
    property Category: TJvCustomInspectorItem read GetCategory;
    property DroppedDown: Boolean read GetDroppedDown;
    property EditCtrlDestroying: Boolean read GetEditCtrlDestroying;
    property EditCtrl: TCustomEdit read GetEditCtrl;
    property EditWndPrc: TWndMethod read FEditWndPrc;
    property LastPaintGeneration: Integer read FLastPaintGen;
    property ListBox: TCustomListBox read GetListBox;
    //promoted: property OnGetValueList: TInspectorItemGetValueListEvent read FOnGetValueList write FOnGetValueList;
    property Pressed: Boolean read FPressed write FPressed;
    property Tracking: Boolean read FTracking write FTracking;
  public
    constructor Create(const AParent: TJvCustomInspectorItem; const AData: TJvCustomInspectorData); virtual;
    destructor Destroy; override;
    function Add(const Item: TJvCustomInspectorItem): Integer;
    procedure BeforeDestruction; override;
    procedure Clear;
    procedure Delete(const Index: Integer); overload; virtual;
    procedure Delete(const Item: TJvCustomInspectorItem); overload; virtual;
    procedure Delete(const Data: TJvCustomInspectorData); overload; virtual;
    procedure DrawEditor(const ACanvas: TCanvas); virtual;
    procedure DrawName(const ACanvas: TCanvas); virtual;
    procedure DrawValue(const ACanvas: TCanvas); virtual;
    function EditFocused: Boolean; dynamic;
    procedure ExpandItems(AExpand: Boolean);
    function HasViewableItems: Boolean; virtual;
    function IndexOf(const Item: TJvCustomInspectorItem): Integer; overload; virtual;
    function IndexOf(const Data: TJvCustomInspectorData): Integer; overload; virtual;
    procedure InitEdit; dynamic;
    procedure DoneEdit(const CancelEdits: Boolean = False); dynamic;
    procedure Insert(const Index: Integer; const Item: TJvCustomInspectorItem);
    procedure ScrollInView;
    procedure Sort;
    function GetEditorText: string;
    property AutoUpdate: Boolean read GetAutoUpdate write SetAutoUpdate;
    property Count: Integer read GetCount;
    property Data: TJvCustomInspectorData read GetData;
    property DisplayIndex: Integer read GetDisplayIndex write SetDisplayIndex;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property FullName: string read GetFullName;
    property DisplayValue: string read GetDisplayValue write SetDisplayValue;
    property Editing: Boolean read GetEditing;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property Flags: TInspectorItemFlags read GetFlags write SetFlags;
    property Hidden: Boolean read GetHidden write SetHidden;
    property Height: Integer read GetHeight write SetHeight;
    property HeightFactor: Integer read GetHeightFactor write SetHeightFactor;
    property Inspector: TJvCustomInspector read GetInspector;
    property Items[const I: Integer]: TJvCustomInspectorItem read GetItems; default;
    property Level: Integer read GetLevel;
    property Multiline: Boolean read GetMultiline write SetMultiline;
    property Name: string read GetName;
    property Parent: TJvCustomInspectorItem read GetParent;
    property QualifiedNames: Boolean read GetQualifiedNames write SetQualifiedNames;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Rects[const RectKind: TInspectorPaintRect]: TRect read GetRects write SetRects;
    property RowSizing: TJvInspectorItemSizing read GetRowSizing write SetRowSizing;
    property SortKind: TInspectorItemSortKind read GetSortKind write SetSortKind;
    property UserData: Pointer read FUserData write FUserData;
    property Visible: Boolean read GetVisible write SetVisible;
    property OnCompare: TInspectorItemSortCompare read FOnCompare write SetOnCompare;
    property OnGetValueList: TInspectorItemGetValueListEvent read FOnGetValueList write FOnGetValueList;
    property DropDownCount: Integer read FDropDownCount write FDropDownCount;
  end;

  TJvInspectorCustomCategoryItem = class(TJvCustomInspectorItem)
  private
    FName: string;
  protected
    function GetName: string; override;
    function IsCategory: Boolean; override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  public
    property Name write FName;
  end;

  TJvInspectorEnumItem = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure GetValueList(const Strings: TStrings); override;
    procedure SetDisplayValue(const Value: string); override;
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
    FShowAsCheckBox: Boolean;
  protected
    function GetShowAsCheckBox: Boolean; virtual;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure SetShowAsCheckBox(Value: Boolean); virtual;
  public
    procedure DoneEdit(const CancelEdits: Boolean = False); override;
    procedure DrawValue(const ACanvas: TCanvas); override;
    procedure InitEdit; override;
    property ShowAsCheckBox: Boolean read GetShowAsCheckBox write SetShowAsCheckBox;
  end;

  TJvCustomInspectorData = class(TPersistent)
  private
    FTypeInfo: PTypeInfo;
    FItems: TJvInspectorItemInstances;
    FName: string;
    FRegistered: Boolean;
  protected
    constructor CreatePrim(const AName: string; ATypeInfo: PTypeInfo);
    procedure CheckReadAccess; virtual;
    procedure CheckWriteAccess; virtual;
    procedure DoneEdits(const CancelEdits: Boolean = False);
    function GetAsFloat: Extended; virtual; abstract;
    function GetAsInt64: Int64; virtual; abstract;
    function GetAsMethod: TMethod; virtual; abstract;
    function GetAsOrdinal: Int64; virtual; abstract;
    function GetAsString: string; virtual; abstract;
    function GetAsVariant: Variant; virtual; abstract;
    function GetItemCount: Integer;
    function GetItems(I: Integer): TJvCustomInspectorItem;
    function GetName: string; virtual;
    function GetTypeInfo: PTypeInfo; virtual;
    procedure InitEdits;
    procedure Invalidate; virtual;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; virtual;
    procedure NotifyRemoveData(const Instance: TJvCustomInspectorData); virtual;
    procedure RefreshEdits;
    class function RegisterInstance(const Instance: TJvCustomInspectorData): TJvCustomInspectorData;
    procedure RemoveItem(const Item: TJvCustomInspectorItem);
    procedure SetAsFloat(const Value: Extended); virtual; abstract;
    procedure SetAsInt64(const Value: Int64); virtual; abstract;
    procedure SetAsMethod(const Value: TMethod); virtual; abstract;
    procedure SetAsOrdinal(const Value: Int64); virtual; abstract;
    procedure SetAsString(const Value: string); virtual; abstract;
    procedure SetAsVariant(const Value: Variant); virtual; abstract;
    procedure SetName(const Value: string); virtual;
    procedure SetTypeInfo(Value: PTypeInfo); virtual;
    function SupportsMethodPointers: Boolean; virtual;
  public
    constructor Create;
    procedure BeforeDestruction; override;
    procedure GetAsSet(var Buf); virtual; abstract;
    function HasValue: Boolean; virtual; abstract;
    function IsAssigned: Boolean; virtual; abstract;
    function IsInitialized: Boolean; virtual; abstract;
    function IsReadOnlyProperty: Boolean; virtual; abstract;
    class function ItemRegister: TJvInspectorRegister; virtual;
    class function New: TJvCustomInspectorData;
    function NewItem(const AParent: TJvCustomInspectorItem): TJvCustomInspectorItem; virtual;
    procedure SetAsSet(const Buf); virtual; abstract;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsMethod: TMethod read GetAsMethod write SetAsMethod;
    property AsOrdinal: Int64 read GetAsOrdinal write SetAsOrdinal;
    property AsString: string read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property ItemCount: Integer read GetItemCount;
    property Items[I: Integer]: TJvCustomInspectorItem read GetItems;
    property Name: string read GetName write SetName;
    property TypeInfo: PTypeInfo read GetTypeInfo write SetTypeInfo;
  end;

  TJvInspectorEventData = class(TJvCustomInspectorData)
  private
    FOnGetAsFloat: TJvInspAsFloat;
    FOnGetAsInt64: TJvInspAsInt64;
    FOnGetAsMethod: TJvInspAsMethod;
    FOnGetAsOrdinal: TJvInspAsInt64;
    FOnGetAsString: TJvInspAsString;
    FOnGetAsSet: TJvInspAsSet;
    FOnSetAsFloat: TJvInspAsFloat;
    FOnSetAsInt64: TJvInspAsInt64;
    FOnSetAsMethod: TJvInspAsMethod;
    FOnSetAsOrdinal: TJvInspAsInt64;
    FOnSetAsString: TJvInspAsString;
    FOnSetAsSet: TJvInspAsSet;
    FOnSupportsMethodPointers: TJvInspSupportsMethodPointers;

    FParent: TJvCustomInspectorItem;
  protected
    function DoGetAsFloat: Extended;
    function DoGetAsInt64: Int64;
    function DoGetAsMethod: TMethod;
    function DoGetAsOrdinal: Int64;
    function DoGetAsString: string;
    procedure DoGetAsSet(out Buf; var BufSize: Integer);
    procedure DoSetAsFloat(Value: Extended);
    procedure DoSetAsInt64(Value: Int64);
    procedure DoSetAsMethod(Value: TMethod);
    procedure DoSetAsOrdinal(Value: Int64);
    procedure DoSetAsString(Value: string);
    procedure DoSetAsSet(const Buf; var BufSize: Integer);
    function DoSupportsMethodPointers: Boolean;
    function GetAsFloat: Extended; override;
    function GetAsInt64: Int64; override;
    function GetAsMethod: TMethod; override;
    function GetAsOrdinal: Int64; override;
    function GetAsString: string; override;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; override;
    procedure SetAsFloat(const Value: Extended); override;
    procedure SetAsInt64(const Value: Int64); override;
    procedure SetAsMethod(const Value: TMethod); override;
    procedure SetAsOrdinal(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetOnGetAsFloat(Value: TJvInspAsFloat);
    procedure SetOnGetAsInt64(Value: TJvInspAsInt64);
    procedure SetOnGetAsMethod(Value: TJvInspAsMethod);
    procedure SetOnGetAsOrdinal(Value: TJvInspAsInt64);
    procedure SetOnGetAsString(Value: TJvInspAsString);
    procedure SetOnGetAsSet(Value: TJvInspAsSet);
    procedure SetOnSetAsFloat(Value: TJvInspAsFloat);
    procedure SetOnSetAsInt64(Value: TJvInspAsInt64);
    procedure SetOnSetAsMethod(Value: TJvInspAsMethod);
    procedure SetOnSetAsOrdinal(Value: TJvInspAsInt64);
    procedure SetOnSetAsString(Value: TJvInspAsString);
    procedure SetOnSetAsSet(Value: TJvInspAsSet);
    procedure SetOnSupportsMethodPointers(Value: TJvInspSupportsMethodPointers);
    function SupportsMethodPointers: Boolean; override;
  public
    procedure GetAsSet(var Buf); override;
    function HasValue: Boolean; override;
    function IsAssigned: Boolean; override;
    function IsInitialized: Boolean; override;
    class function New(const AParent: TJvCustomInspectorItem; const AName: string; ATypeInfo: PTypeInfo):
      TJvCustomInspectorItem; reintroduce; overload;
    procedure SetAsSet(const Buf); override;
    property OnGetAsFloat: TJvInspAsFloat read FOnGetAsFloat write SetOnGetAsFloat;
    property OnGetAsInt64: TJvInspAsInt64 read FOnGetAsInt64 write SetOnGetAsInt64;
    property OnGetAsMethod: TJvInspAsMethod read FOnGetAsMethod write SetOnGetAsMethod;
    property OnGetAsOrdinal: TJvInspAsInt64 read FOnGetAsOrdinal write SetOnGetAsOrdinal;
    property OnGetAsString: TJvInspAsString read FOnGetAsString write SetOnGetAsString;
    property OnGetAsSet: TJvInspAsSet read FOnGetAsSet write SetOnGetAsSet;
    property OnSetAsFloat: TJvInspAsFloat read FOnSetAsFloat write SetOnSetAsFloat;
    property OnSetAsInt64: TJvInspAsInt64 read FOnSetAsInt64 write SetOnSetAsInt64;
    property OnSetAsMethod: TJvInspAsMethod read FOnSetAsMethod write SetOnSetAsMethod;
    property OnSetAsOrdinal: TJvInspAsInt64 read FOnSetAsOrdinal write SetOnSetAsOrdinal;
    property OnSetAsString: TJvInspAsString read FOnSetAsString write SetOnSetAsString;
    property OnSetAsSet: TJvInspAsSet read FOnSetAsSet write SetOnSetAsSet;
    property OnSupportsMethodPointers: TJvInspSupportsMethodPointers read FOnSupportsMethodPointers write
      SetOnSupportsMethodPointers;
  end;

  TJvInspectorRegister = class(TPersistent)
  private
    FDataClass: TJvInspectorDataClass;
    FItems: TObjectList;
  protected
    function Compare(const ADataObj: TJvCustomInspectorData;
      const Item1, Item2: TJvCustomInspectorRegItem): Integer;
    function GetCount: Integer;
    function GetItems(const I: Integer): TJvCustomInspectorRegItem; virtual;
  public
    constructor Create(const ADataClass: TJvInspectorDataClass);
    destructor Destroy; override;
    procedure Add(const RegItem: TJvCustomInspectorRegItem);
    procedure Delete(const RegItem: TJvCustomInspectorRegItem); overload;
    procedure Delete(const ItemClass: TJvInspectorItemClass); overload;
    procedure Delete(const Index: Integer); overload;
    function FindMatch(const ADataObj: TJvCustomInspectorData): TJvCustomInspectorRegItem;
    function IndexOf(const RegItem: TJvCustomInspectorRegItem): Integer; overload;
    function IndexOf(const ItemClass: TJvInspectorItemClass): Integer; overload;
    property Count: Integer read GetCount;
    property DataClass: TJvInspectorDataClass read FDataClass;
    property Items[const I: Integer]: TJvCustomInspectorRegItem read GetItems;
  end;

  TJvCustomInspectorRegItem = class(TPersistent)
  private
    FItemClass: TJvInspectorItemClass;
  protected
    function CompareTo(const ADataObj: TJvCustomInspectorData;
      const Item: TJvCustomInspectorRegItem): Integer; virtual;
    function GetItemClass: TJvInspectorItemClass; virtual;
    procedure SetItemClass(const Value: TJvInspectorItemClass); virtual;
  public
    constructor Create(const AItemClass: TJvInspectorItemClass);
    procedure ApplyDefaults(const Item: TJvCustomInspectorItem); virtual;
    function Compare(const ADataObj: TJvCustomInspectorData;
      const Item: TJvCustomInspectorRegItem): Integer; virtual;
    function IsMatch(const ADataObj: TJvCustomInspectorData): Boolean; virtual;
    function MatchValue(const ADataObj: TJvCustomInspectorData): Integer; virtual; abstract;
    function MatchPercent(const ADataObj: TJvCustomInspectorData): Integer; virtual; abstract;
    property ItemClass: TJvInspectorItemClass read GetItemClass;
  end;

  TJvInspectorTypeInfoRegItem = class(TJvCustomInspectorRegItem)
  private
    FTypeInfo: PTypeInfo;
  protected
    function GetTypeInfo: PTypeInfo; virtual;
    procedure SetTypeInfo(Value: PTypeInfo); virtual;
  public
    constructor Create(const AItemClass: TJvInspectorItemClass; ATypeInfo: PTypeInfo);
    function MatchValue(const ADataObj: TJvCustomInspectorData): Integer; override;
    function MatchPercent(const ADataObj: TJvCustomInspectorData): Integer; override;
    property TypeInfo: PTypeInfo read GetTypeInfo;
  end;

  TJvInspectorTCaptionRegItem = class(TJvInspectorTypeInfoRegItem)
  public
    procedure ApplyDefaults(const Item: TJvCustomInspectorItem); override;
  end;

  TJvInspectorTypeKindRegItem = class(TJvCustomInspectorRegItem)
  private
    FTypeKind: TTypeKind;
  protected
    function CompareTo(const ADataObj: TJvCustomInspectorData;
      const Item: TJvCustomInspectorRegItem): Integer; override;
    function GetTypeKind: TTypeKind; virtual;
    procedure SetTypeKind(const Value: TTypeKind); virtual;
  public
    constructor Create(const AItemClass: TJvInspectorItemClass;
      const ATypeKind: TTypeKind);
    function Compare(const ADataObj: TJvCustomInspectorData;
      const Item: TJvCustomInspectorRegItem): Integer; override;
    function MatchValue(const ADataObj: TJvCustomInspectorData): Integer; override;
    function MatchPercent(const ADataObj: TJvCustomInspectorData): Integer; override;
    property TypeKind: TTypeKind read GetTypeKind;
  end;

// (rom) centralized the string literals
const
  cJvInspectorFloat = 'Float';
  cJvInspectorInt64 = 'Int64';
  cJvInspectorTMethod = 'TMethod';
  cJvInspectorOrdinal = 'Ordinal';
  cJvInspectorString = 'string';
  cJvInspectorSet = 'set';
  cJvInspectorVariant = 'variant';

// All the declarations below are to help support Type Info under C++ Builder

// we add missing typedefs for some Delphi types
{$HPPEMIT 'typedef __int64 Int64;'}
{$HPPEMIT 'typedef double Real;'}
{$HPPEMIT ''}

//Inspector Data Register
type
  TJvInspDataReg = class(TPersistent)
  private
    FInstanceList: TJvInspectorDataInstances;
    FClearing: Boolean;
  protected
    function GetCount: Integer;
    function GetItems(I: Integer): TJvCustomInspectorData;
  public
    constructor Create;
    destructor Destroy; override;
    // Adds a new data instance. If an instance pointing to the same data exists the given instance is destroyed and the registered instance returned
    function Add(Instance: TJvCustomInspectorData): TJvCustomInspectorData;
    // Deletes a data instance and all items referencing it. All other data instances are notified.
//    procedure Delete(Instance: TJvCustomInspectorData); make Delphi 5 compiler happy // andreas
    // Deletes all data instances and items referencing them. No notification is issued to the data instances as they will be removed also.
    procedure Clear;
    // Locates a data instance that references the same data as the given instance. The index is returned or -1 if no instance was found.
    function Locate(Instance: TJvCustomInspectorData): Integer;
    // Removes a data instance from the list. All other data instances are notified.
    procedure Remove(Instance: TJvCustomInspectorData);
    property Count: Integer read GetCount;
    property Items[I: Integer]: TJvCustomInspectorData read GetItems;
  end;

// Access to the GlobalDataRegister
function DataRegister: TJvInspDataReg;

// Canvas State functions used by TJvInspectorPainter & its descendents
function SaveCanvasState(const Canvas: TCanvas): Integer;
procedure ApplyCanvasState(const Canvas: TCanvas; const SavedIdx: Integer);
procedure RestoreCanvasState(const Canvas: TCanvas; const SavedIdx: Integer);

// We define here a set of macros to help C++ Builder programmers
// gather Type Info by typing code very similar to Delphi code where
// one only has to type TypeInfo(typename) to get the correct result

// Those first two are required to convert a macro argument
// to a string. Hence STR(hello) is equivalent to "hello"
{$HPPEMIT '#define _STR(x) #x'}
{$HPPEMIT '#define STR(x) _STR(x)'}

// This macro gives an expression that gives the TypeInfo for a given
// type, using the given class. It will look for a published property
// named type + "Prop" in the given class.
{$HPPEMIT '#define TypeInfoFromClass(class, type) *(GetPropInfo(__typeinfo(class), STR(type) "Prop" )->PropType)'}

{$HPPEMIT ''}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  RTLConsts, Types, Variants, Consts, Dialogs, Forms, Buttons, Themes;

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
    procedure Pop(const Canvas: TCanvas; Index: Integer = -2);
    //    procedure Peek(const Canvas: TCanvas; Index: Integer = -2); make Delphi 5 compiler happy // andreas
    property Capacity write SetCapacity;
    property Top: Integer read FTop write FTop;
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

procedure TCanvasStack.Pop(const Canvas: TCanvas; Index: Integer = -2);
begin
  if Index = -1 then
    Index := FTop;
  TCanvasState(Items[Index]).ApplyTo(Canvas);
  FTop := Pred(Index);
end;

(* make Delphi 5 compiler happy // andreas
procedure TCanvasStack.Peek(const Canvas: TCanvas; Index: Integer = -2);
begin
  if Index = -1 then
    Index := FTop;
  TCanvasState(Items[Index]).ApplyTo(Canvas);
end;*)

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

//=== { TJvInspDataReg } =====================================================

constructor TJvInspDataReg.Create;
begin
  inherited Create;
  SetLength(FInstanceList, 0);
end;

destructor TJvInspDataReg.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJvInspDataReg.GetCount: Integer;
begin
  Result := Length(FInstanceList);
end;

function TJvInspDataReg.GetItems(I: Integer): TJvCustomInspectorData;
begin
  if (I < Low(FInstanceList)) or (I > High(FInstanceList)) then
    TList.Error(SListIndexError, I);
  Result := FInstanceList[I];
end;

function TJvInspDataReg.Add(Instance: TJvCustomInspectorData): TJvCustomInspectorData;
var
  I: Integer;
begin
  I := Locate(Instance);
  if I = -1 then
  begin
    SetLength(FInstanceList, Count + 1);
    FInstanceList[High(FInstanceList)] := Instance;
    Result := Instance;
    Result.FRegistered := True;
  end
  else
  begin
    if Items[I] <> Instance then
      Instance.Free;
    Result := Items[I];
  end;
end;

(* make Delphi 5 compiler happy // andreas
procedure TJvInspDataReg.Delete(Instance: TJvCustomInspectorData);
begin
  Instance.Free;
end;
*)

procedure TJvInspDataReg.Clear;
var
  I: Integer;
begin
  FClearing := True;
  try
    for I := High(FInstanceList) downto Low(FInstanceList) do
      Items[I].Free;
  finally
    FClearing := False;
  end;
end;

function TJvInspDataReg.Locate(Instance: TJvCustomInspectorData): Integer;
begin
  Result := High(FInstanceList);
  while Result > -1 do
  begin
    if (Instance = Items[Result]) or Instance.IsEqualReference(Items[Result]) then
      Break;
    Dec(Result);
  end;
end;

procedure TJvInspDataReg.Remove(Instance: TJvCustomInspectorData);
var
  I: Integer;
begin
  I := Locate(Instance);
  if I > -1 then
  begin
    if Items[I] <> Instance then
      raise EJvInspectorData.CreateRes(@RsEInspectorInternalError);
    if I < High(FInstanceList) then
      Move(FInstanceList[I + 1], FInstanceList[I], (High(FInstanceList) - I) * SizeOf(TJvCustomInspectorData));
    SetLength(FInstanceList, High(FInstanceList));
    if not FClearing then
    begin
      I := High(FInstanceList);
      while I >= 0 do
      begin
        Items[I].NotifyRemoveData(Instance);
        Dec(I);
        { Additional safety: more than 1 instance might have been removed at this point; make sure
          I stays in range. }
        if I > High(FInstanceList) then
          I := High(FInstanceList);
      end;
    end;
  end;
end;

//=== { TJvCustomInspector } =================================================

var
  GlobalDataRegister: TJvInspDataReg = nil;

function DataRegister: TJvInspDataReg;
begin
  if not Assigned(GlobalDataRegister) then
    GlobalDataRegister := TJvInspDataReg.Create;
  Result := GlobalDataRegister;
end;

constructor TJvCustomInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FExpandButton := TBitmap.Create;
  FCollapseButton := TBitmap.Create;

  FSortNotificationList := TList.Create;
  // Upstream used 16, tuned for the old 8pt MS Sans Serif default font;
  // with Segoe UI that cuts off descenders and bracket glyphs
  FItemHeight := 18;
  DoubleBuffered := True;
  FVisibleList := TStringList.Create;
  FRoot := TJvCustomInspectorItem.Create(nil, nil);
  Root.SetInspector(Self);
  Root.Flags := [iifHidden, iifExpanded, iifReadonly, iifVisible];
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

function TJvCustomInspector.CalcItemRect(const Item: TJvCustomInspectorItem): TRect;
begin
  Result := Item.Rects[iprItem];
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

function TJvCustomInspector.GetButtonRect(const ItemIndex: Integer): TRect;
var
  Item: TJvCustomInspectorItem;
begin
  // retrieve item
  Item := VisibleItems[ItemIndex];

  // retrieve button rectangle
  if Item.Expanded or Item.HasViewableItems then
    Result := Item.Rects[iprBtnDstRect]
  else
    Result := Rect(0, 0, 0, 0);
end;

function TJvCustomInspector.GetCollapseButton: TBitmap;
begin
  Result := FCollapseButton;
end;

function TJvCustomInspector.GetDivider: Integer;
begin
  Result := FDivider;
end;

function TJvCustomInspector.GetExpandButton: TBitmap;
begin
  Result := FExpandButton;
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

procedure TJvCustomInspector.InvalidateHeight;
begin
  FImageHeight := 0;
  TopIndex := TopIndex; // Adapt position
end;

procedure TJvCustomInspector.InvalidateItem;
begin
  if (LockCount = 0) and HandleAllocated then
    UpdateScrollBars
  else
  if not NeedRebuild then
    NeedRedraw := True;
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
    // Check row sizing
    else
    if (Item <> nil) and (Y >= Pred(ItemRect.Bottom)) and
      (Y <= Succ(ItemRect.Bottom)) and (Item.RowSizing.SizingFactor <> irsNoReSize) and
      Item.RowSizing.Sizable then
    begin
      RowSizing := True;
      RowSizingItem := Item;
    end
    // Check selecting
    else
    if (Item <> nil) and (ItemIndex <> SelectedIndex) then
    begin
      SelectedIndex := ItemIndex;
      if ItemIndex >= 0 then
        Item := VisibleItems[ItemIndex];
    end;
    if not DraggingDivider and not RowSizing then
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
    if not (ssDouble in Shift) and not DraggingDivider and not RowSizing and
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
    if RowSizing then
    begin
      if RowSizingItem <> nil then
      begin
        ItemRect := CalcItemRect(RowSizingItem);
        RowSizingItem.Height := Y - ItemRect.Top
      end;
    end
    else
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
    end
    else
    begin
      if (ItemIndex < VisibleCount) and (ItemIndex > -1) then
        Item := VisibleItems[ItemIndex]
      else
        Item := nil;
      if (Item <> nil) and (Y >= Pred(ItemRect.Bottom)) and
        (Y <= Succ(ItemRect.Bottom)) and (Item.RowSizing.SizingFactor <> irsNoReSize) and
        Item.RowSizing.Sizable then
        Cursor := crVSplit
      else
        Cursor := crDefault;
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
    if RowSizing then
      RowSizing := False
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

procedure TJvCustomInspector.NotifySort(const Item: TJvCustomInspectorItem);
begin
  if LockCount = 0 then
    Item.Sort
  else
  if (Item <> nil) and (SortNotificationList.IndexOf(Item) = -1) then
    SortNotificationList.Add(Item);
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

function ListCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareText(List[Index1], List[Index2]);
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
    while Item <> nil do
    begin
      if not Item.Hidden then
        FVisibleList.AddObject(Item.GetSortName, Item);
      if Item.Visible and Item.Expanded and (Item.Count > 0) then
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
  FVisibleList.CustomSort(ListCompare);
  if OldSel <> nil then
    SelectedIndex := FVisibleList.IndexOfObject(OldSel);
  CalcImageHeight;
  NeedRebuild := False;
end;

procedure TJvCustomInspector.RemoveNotifySort(const Item: TJvCustomInspectorItem);
begin
  SortNotificationList.Remove(Item);
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

procedure TJvCustomInspector.SetCollapseButton(const Value: TBitmap);
begin
  if Value <> FCollapseButton then
  begin
    FCollapseButton.Assign(Value);
    if HandleAllocated then
      UpdateScrollBars;
  end;
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

procedure TJvCustomInspector.SetExpandButton(const Value: TBitmap);
begin
  if Value <> FExpandButton then
  begin
    FExpandButton.Assign(Value);
    if HandleAllocated then
      UpdateScrollBars;
  end;
end;

procedure TJvCustomInspector.SetItemHeight(Value: Integer);
begin
  if Value <> ItemHeight then
  begin
    FItemHeight := Value;
    if HandleAllocated then
      UpdateScrollBars;
  end;
end;

procedure TJvCustomInspector.SetLockCount(const Value: Integer);
begin
  if Value <> LockCount then
  begin
    FLockCount := Value;
    if LockCount = 0 then
      if NeedRebuild then
        InvalidateList
      else
        InvalidateItem;
  end;
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

function TJvCustomInspector.ViewHeight: Integer;
begin
  Result := RectHeight(ViewRect);
end;

function TJvCustomInspector.ViewRect: TRect;
begin
  Result := ClientRect;
end;

function TJvCustomInspector.ViewWidth: Integer;
begin
  Result := RectWidth(ViewRect);
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
  FSortNotificationList.Free;
  FVisibleList.Free;
  FExpandButton.Free;
  FCollapseButton.Free;
end;

function TJvCustomInspector.BeginUpdate: Integer;
begin
  Inc(FLockCount);
  Result := FLockCount;
end;

function TJvCustomInspector.EndUpdate: Integer;
var
  I: Integer;
begin
  if LockCount > 0 then
    Dec(FLockCount);
  Result := LockCount;
  if Result = 0 then
  begin
    I := 0;
    FLockCount := -1; // Keep InvalidateSort from calling InvalidateList
    try
      while I < SortNotificationList.Count do
      begin
        TJvCustomInspectorItem(SortNotificationList[I]).InvalidateSort;
        Inc(I);
      end;
    finally
      FLockCount := 0;
      if SortNotificationList.Count > 0 then
        NeedRebuild := True;
      if NeedRebuild then
        InvalidateList
      else
        InvalidateItem;
      SortNotificationList.Clear;
    end;
  end;
end;

function TJvCustomInspector.Focused: Boolean;
begin
  Result := inherited Focused or ((Selected <> nil) and Selected.EditFocused);
end;

function TJvCustomInspector.FocusedItem: TJvCustomInspectorItem;
begin
  Result := Selected;
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

procedure TJvCustomInspector.SaveValues;
begin
  if (Selected <> nil) and Selected.Editing then
  begin
    Selected.DoneEdit(False);
    Selected.InitEdit;
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
  // dpi using the painter's colors (upstream used fixed 9x9 black on white
  // bitmaps, which are wrong at high dpi and in dark mode), keeping the last
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
  if not Inspector.CollapseButton.Empty then
    Result := Inspector.CollapseButton
  else
  begin
    PrepareInternalImages;
    Result := FInternalCollapseButton;
  end;
end;

function TJvInspectorPainter.GetDividerColor: TColor;
begin
  Result := FDividerColor;
end;

function TJvInspectorPainter.GetExpandImage: TBitmap;
begin
  if not Inspector.ExpandButton.Empty then
    Result := Inspector.ExpandButton
  else
  begin
    PrepareInternalImages;
    Result := FInternalExpandButton;
  end;
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

function TJvInspectorPainter.GetNameHeight(const AItem: TJvCustomInspectorItem): Integer;
var
  TmpCanvas: TCanvas;
begin
  TmpCanvas := Canvas;
  try
    Canvas := TControlCanvas.Create;
    TControlCanvas(Canvas).Control := Inspector;
    ApplyNameFont;
    Result := CanvasMaxTextHeight(Canvas);
  finally
    if TmpCanvas <> Canvas then
      Canvas.Free;
    Canvas := TmpCanvas;
  end;
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

function TJvInspectorPainter.GetDrawNameEndEllipsis: Boolean;
begin
  Result := FDrawNameEndEllipsis;
end;

function TJvInspectorPainter.GetValueColor: TColor;
begin
  Result := FValueColor;
end;

function TJvInspectorPainter.GetValueHeight(const AItem: TJvCustomInspectorItem): Integer;
var
  TmpCanvas: TCanvas;
begin
  TmpCanvas := Canvas;
  try
    Canvas := TControlCanvas.Create;
    TControlCanvas(Canvas).Control := Inspector;
    ApplyValueFont;
    Result := CanvasMaxTextHeight(Canvas);
  finally
    if TmpCanvas <> Canvas then
      Canvas.Free;
    Canvas := TmpCanvas;
  end;
end;

procedure TJvInspectorPainter.HideEditor;
begin
  Inspector.Selected.Rects[iprEditValue] := Rect(0, 0, 0, 0);
end;

procedure TJvInspectorPainter.InitializeColors;
begin
  SetDefaultProp(Self, ['BackgroundColor', 'DividerColor', 'CategoryColor', 'SelectedColor']);
end;

function TJvInspectorPainter.Loading: Boolean;
begin
  Result := csLoading in ComponentState;
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

procedure TJvInspectorPainter.PaintItem(const AItem: TJvCustomInspectorItem);
var
  OrgState: Integer;
begin
  OrgState := SaveCanvasState(Canvas);
  try
    // Initialize painter variables
    ItemIndex := -1;
    Item := AItem;
    SetupItem;

    // Do actual painting
    DoPaint;

    // Finalize painting
    TeardownItem;
  finally
    RestoreCanvasState(Canvas, OrgState);
  end;
end;

procedure TJvInspectorPainter.SetBackgroundColor(const Value: TColor);
begin
  if Value <> BackgroundColor then
  begin
    FBackgroundColor := Value;
    if not Initializing and not Loading and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetCategoryColor(const Value: TColor);
begin
  if Value <> CategoryColor then
  begin
    FCategoryColor := Value;
    if not Initializing and not Loading and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetCategoryDividerColor(const Value: TColor);
begin
  if Value <> CategoryDividerColor then
  begin
    FCategoryDividerColor := Value;
    if not Initializing and not Loading and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetCategoryTextColor(const Value: TColor);
begin
  if Value <> CategoryTextColor then
  begin
    FCategoryTextColor := Value;
    if not Initializing and not Loading and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetDividerColor(const Value: TColor);
begin
  if DividerColor <> Value then
  begin
    FDividerColor := Value;
    if not Initializing and not Loading and Assigned(Inspector) then
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
    if not Initializing and not Loading and Assigned(Inspector) then
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
    if not Initializing and not Loading and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetSelectedTextColor(const Value: TColor);
begin
  if Value <> SelectedTextColor then
  begin
    FSelectedTextColor := Value;
    if not Initializing and not Loading and Assigned(Inspector) then
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
    if not Initializing and not Loading and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetDrawNameEndEllipsis(Value: Boolean);
begin
  if Value <> DrawNameEndEllipsis then
  begin
    FDrawNameEndEllipsis := Value;
    if not Initializing and not Loading and Assigned(Inspector) then
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

procedure TJvInspectorBorlandNETBasePainter.ApplyValueFont;
begin
  inherited ApplyValueFont;
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
  if [iifValueList, iifEditButton] * Item.Flags = [] then
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
    if not Initializing and not Loading and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorDotNETPainter.SetHideSelectTextColor(const Value: TColor);
begin
  if Value <> HideSelectTextColor then
  begin
    FHideSelectTextColor := Value;
    if not Initializing and not Loading and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

//=== { TJvInspectorItemSizing } =============================================

constructor TJvInspectorItemSizing.Create(const AItem: TJvCustomInspectorItem);
begin
  inherited Create;
  Item := AItem;
end;

function TJvInspectorItemSizing.GetMinHeight: TItemRowSizing;
begin
  Result := FMinHeight;
end;

function TJvInspectorItemSizing.GetSizable: Boolean;
begin
  Result := FSizable;
end;

function TJvInspectorItemSizing.GetSizingFactor: TItemRowSizing;
begin
  Result := FSizingFactor;
end;

procedure TJvInspectorItemSizing.SetMinHeight(Value: TItemRowSizing);
var
  CurHeight: Integer;
begin
  CurHeight := Item.Height;
  if Value = irsNoReSize then
  begin
    if SizingFactor <> Value then
      SizingFactor := Value
    else
    if MinHeight <> irsItemHeight then
    begin
      FMinHeight := irsItemHeight;
      Item.Height := CurHeight;
    end;
  end
  else
  if MinHeight <> Value then
  begin
    if SizingFactor = irsNoReSize then
      FSizingFactor := irsValueMask;
    FMinHeight := Value;
    Item.Height := CurHeight;
  end;
end;

procedure TJvInspectorItemSizing.SetSizable(Value: Boolean);
begin
  if Sizable <> Value then
    FSizable := Value;
end;

procedure TJvInspectorItemSizing.SetSizingFactor(Value: TItemRowSizing);
var
  CurHeight: Integer;
begin
  CurHeight := Item.Height;
  if SizingFactor <> Value then
  begin
    FSizingFactor := Value;
    if SizingFactor = irsNoReSize then
      FMinHeight := irsItemHeight
    else
      Item.Height := CurHeight;
  end;
end;

procedure TJvInspectorItemSizing.Assign(Source: TPersistent);
begin
  if Source is TJvInspectorItemSizing then
  begin
    MinHeight := TJvInspectorItemSizing(Source).MinHeight;
    SizingFactor := TJvInspectorItemSizing(Source).SizingFactor;
  end
  else
    inherited Assign(Source);
end;

{ Item sorting functions }

function AlphaSortCompare(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareText(TJvCustomInspectorItem(Item1).DisplayName,
    TJvCustomInspectorItem(Item2).DisplayName);
end;

var // maybe a threadvar would be better? OTOH, VCL is not threadsafe anyway so why bother?
  DataSortCompareEvent: TInspectorItemSortCompare;

function DataSortCompare(Item1, Item2: Pointer): Integer;
begin
  if Assigned(DataSortCompareEvent) then
    Result := DataSortCompareEvent(Item1, Item2)
  else
    Result := 0;
end;

function DisplayIndexSortCompare(Item1, Item2: Pointer): Integer;
var
  Idx1: Integer;
  Idx2: Integer;
begin
  Idx1 := TJvCustomInspectorItem(Item1).DisplayIndex;
  Idx2 := TJvCustomInspectorItem(Item2).DisplayIndex;
  if (Idx1 <> -1) and (Idx2 <> -1) then
    Result := Idx1 - Idx2
  else
  begin
    if Idx1 = -1 then
      if Idx2 = -1 then
        Result := 0
      else
        Result := 1
    else
      Result := -1;
  end;
end;

//=== { TJvInspectorMemo } ===================================================

type
  TJvInspectorMemo = class(TMemo)
  private
    FOnKillFocus: TNotifyEvent;
  protected
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
  public
    property OnKillFocus: TNotifyEvent read FOnKillFocus write FOnKillFocus;
  end;

procedure TJvInspectorMemo.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  if Assigned(FOnKillFocus) then
    FOnKillFocus(Self);
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
  Flags := [iifVisible];
  FRowSizing := TJvInspectorItemSizing.Create(Self);
  FSortKind := iskName;
  FDisplayIndex := -1;
  if AData <> nil then
    FDisplayName := AData.Name;
  if AParent <> nil then
  begin
    FInspector := AParent.Inspector;
    AParent.Add(Self)
  end;
  FData := AData;
  FDropDownCount := 8;
end;

destructor TJvCustomInspectorItem.Destroy;
begin
  FAutoComplete.Free;
  inherited Destroy;
end;

procedure TJvCustomInspectorItem.AlphaSort;
var
  ItemList: TList;
begin
  ItemList := TList.Create;
  try
    BuildDisplayableList(ItemList);
    ItemList.Sort(AlphaSortCompare);
    ApplyDisplayIndices(ItemList);
  finally
    ItemList.Free;
  end;
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
      if not Data.IsAssigned or (DisplayValue <> NewValue) or
         (AutoUpdate and (FLastEditCtrlText <> NewValue)) then
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
            FLastEditCtrlText := EditCtrl.Text;
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

procedure TJvCustomInspectorItem.ApplyDisplayIndices(const ItemList: TList);
var
  I: Integer;
begin
  for I := ItemList.Count - 1 downto 0 do
    TJvCustomInspectorItem(ItemList[I]).SetDisplayIndexValue(I);
end;

procedure TJvCustomInspectorItem.BuildDisplayableList(const ItemList: TList);
var
  TempList: TList;
  I: Integer;
  Item: TJvCustomInspectorItem;
begin
  TempList := TList.Create;
  try
    if ItemList.Capacity < 64 then
      ItemList.Capacity := 64; // Avoid small growth steps
    I := 0;
    while I < Count do
    begin
      Item := Items[I];
      if not Item.Hidden then
        ItemList.Add(Item)
      else
      begin
        Item.BuildDisplayableList(TempList);
        ItemList.Assign(TempList, laOr);
        TempList.Clear;
      end;
      Inc(I);
    end;
  finally
    TempList.Free;
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

procedure TJvCustomInspectorItem.DataSort;
var
  ItemList: TList;
begin
  ItemList := TList.Create;
  try
    BuildDisplayableList(ItemList);
    DataSortCompareEvent := OnCompare;
    ItemList.Sort(DataSortCompare);
    ApplyDisplayIndices(ItemList);
  finally
    ItemList.Free;
  end;
end;

procedure TJvCustomInspectorItem.Deactivate;
begin
  if DroppedDown then
    CloseUp(False);
end;

function TJvCustomInspectorItem.DoCompare(const Item: TJvCustomInspectorItem): Integer;
begin
  if Assigned(FOnCompare) then
    Result := OnCompare(Self, Item)
  else
    Result := 0;
end;

procedure TJvCustomInspectorItem.DoDefaultDrawListItem(ACanvas: TCanvas; Rect: TRect; const AText: string);
var
  h: Integer;
begin
  ACanvas.FillRect(Rect);
  h := ACanvas.TextHeight(AText);
  Rect.Left := Rect.Left + 2;
  Rect.Top := Rect.Top + (Rect.Bottom - Rect.Top - h) div 2;
  ACanvas.TextRect(Rect, Rect.Left, Rect.Top, AText);
end;

procedure TJvCustomInspectorItem.DoDrawListItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  DoDefaultDrawListItem(TListBox(Control).Canvas, Rect, TListBox(Control).Items[Index]);
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

procedure TJvCustomInspectorItem.DoMeasureListItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
end;

procedure TJvCustomInspectorItem.DoMeasureListItemWidth(Control: TWinControl;
  Index: Integer; var Width: Integer);
begin
end;

procedure TJvCustomInspectorItem.DropDown;
var
  ListCount: Integer;
  P: TPoint;
  Y: Integer;
  J: Integer;
  I: Integer;
  IH: Integer;
  MH: Integer;
  R: TRect;
  EditMonitor: TMonitor;
begin
  if (not DroppedDown) and (ListBox <> nil) then
  begin
    ListBox.Width := RectWidth(Rects[iprValueArea]);
    TListBox(ListBox).Font := TCustomEditAccessProtected(EditCtrl).Font;
    ListBox.Items.Clear;
    GetValueList(ListBox.Items);
    if [iifOwnerDrawListFixed, iifOwnerDrawListVariable, iifOwnerDrawListMaxHeight] * Flags <> [] then
    begin
      ListBox.Canvas.Font := TListBox(ListBox).Font;
      IH := CanvasMaxTextHeight(ListBox.Canvas);
      if iifOwnerDrawListFixed in Flags then
      begin
        DoMeasureListItem(ListBox, -1, IH);
        MH := IH;
      end
      else
      if iifOwnerDrawListMaxHeight in Flags then
      begin
        MH := IH;
        for I := 0 to (ListBox.Items.Count-1) do
        begin
          DoMeasureListItem(ListBox, I, IH);
          if MH < IH then
            MH := IH;
        end;
      end
      else
        MH := IH;
      TListBox(ListBox).ItemHeight := MH;
    end;
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
      if TListBox(ListBox).Style <> lbStandard then
        DoMeasureListItemWidth(ListBox, I, Y);
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

procedure TJvCustomInspectorItem.EditChange(Sender: TObject);
begin
  if AutoUpdate then
  begin
    DisplayValue := EditCtrl.Text;
    InvalidateItem;
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

procedure TJvCustomInspectorItem.EditMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
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
  // Key: Word;

  function LeftRightCanNavigate: Boolean;
  begin
{    Result :=
      ((Msg.WParam = VK_LEFT) and ((EditCtrl.SelLength = Length(EditCtrl.Text)) or (EditCtrl.SelStart < 1))) or
      ((Msg.WParam = VK_RIGHT) and ((EditCtrl.SelLength = Length(EditCtrl.Text)) or (EditCtrl.SelStart >= Length(EditCtrl.Text))));}
    Result := False;
  end;

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
            // system meaning, such as Alt+F4 (upstream swallowed them here)
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
            (not DroppedDown and (Msg.WParam in [VK_DOWN, VK_UP])) or LeftRightCanNavigate));
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
//    FEditChanged := True; // sets a flag that a change should be accepted whenever focus shifts away!
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

function TJvCustomInspectorItem.GetAutoUpdate: Boolean;
begin
  Result := (iifAutoUpdate in Flags);
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

function TJvCustomInspectorItem.GetDisplayIndex: Integer;
begin
  Result := FDisplayIndex;
end;

function TJvCustomInspectorItem.GetDisplayName: string;
begin
  Result := FDisplayName;
  if (Parent <> nil) and (iifQualifiedNames in Parent.Flags) then
    Result := Parent.DisplayName + '.' + Result;
end;

// NEW: TJvCustomInspectorItem.GetFullName
// This allows us to internally fetch the fully qualified INTERNAL
// names of any item using ONLY their internal names, NOT their display
// names.
// NOTE THIS USES INTERNAL NAME PROPERTIES (NOT DISPLAY NAME PROPERTIES)
// TO BUILD ITS RESULT, UNLIKE GetDisplayName. It would do the same thing
// as GetDisplayName, if and only if (a) the parents have iifQualifiedNames
// in their parent flags, and (b) if the display names and internal names
// are the same.

function TJvCustomInspectorItem.GetFullName: string;
var
  Tmp: string;
begin
  Result := GetName;
  if Parent <> nil then
  begin
    Tmp := Parent.GetFullName;
    if Tmp <> '' then
      Result := Tmp + '.' + Result;
  end;
end;

function TJvCustomInspectorItem.GetDisplayParent: TJvCustomInspectorItem;
begin
  Result := Parent;
  while (Result <> nil) and Result.Hidden do
    Result := Result.Parent;
  if Result = nil then
    Result := Inspector.Root;
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

function TJvCustomInspectorItem.GetEditorText: string; {NEW:WAP}
begin
  if Assigned(FEditCtrl) then
    Result := FEditCtrl.Text;
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
  if RowSizing.SizingFactor = irsNoReSize then
    Result := Inspector.ItemHeight
  else
  begin
    case RowSizing.MinHeight of
      irsNameHeight:
        Result := Inspector.Painter.GetNameHeight(Self);
      irsValueHeight:
        Result := Inspector.Painter.GetValueHeight(Self);
      irsItemHeight:
        Result := Inspector.ItemHeight;
    else
      Result := RowSizing.MinHeight;
    end;
    case RowSizing.SizingFactor of
      irsNameHeight:
        Result := Result + HeightFactor * Inspector.Painter.GetNameHeight(Self);
      irsValueHeight:
        Result := Result + HeightFactor * Inspector.Painter.GetValueHeight(Self);
      irsItemHeight:
        Result := Result + HeightFactor * Inspector.ItemHeight;
    else
      Result := Result + HeightFactor * RowSizing.SizingFactor;
    end;
  end;
end;

function TJvCustomInspectorItem.GetHeightFactor: Integer;
begin
  Result := FHeight;
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

function TJvCustomInspectorItem.GetMultiline: Boolean;
begin
  Result := (iifMultiLine in Flags);
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

function TJvCustomInspectorItem.GetQualifiedNames: Boolean;
begin
  Result := (iifQualifiedNames in Flags);
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

function TJvCustomInspectorItem.GetRowSizing: TJvInspectorItemSizing;
begin
  Result := FRowSizing;
end;

function TJvCustomInspectorItem.GetSortKind: TInspectorItemSortKind;
begin
  Result := FSortKind;
end;

function TJvCustomInspectorItem.GetSortName: string;
var
  DisplayParent: TJvCustomInspectorItem;
begin
  Result := Format('%.7d', [DisplayIndex]);
  DisplayParent := GetDisplayParent;
  if (DisplayParent <> nil) and (DisplayParent <> Inspector.Root) then
    Result := DisplayParent.GetSortName + #31 + Result;
end;

procedure TJvCustomInspectorItem.GetValueList(const Strings: TStrings);
begin
  DoGetValueList(Strings);
end;

function TJvCustomInspectorItem.GetVisible: Boolean;
begin
  Result := iifVisible in Flags;
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

procedure TJvCustomInspectorItem.InvalidateSort;
begin
  if Inspector.LockCount > 0 then
    Inspector.NotifySort(Self)
  else
  begin
    if SortKind in [iskNone, iskName, iskCustom] then
      Sort;
    if Inspector.LockCount = 0 then // LockCount will be -1 if called from EndUpdate
      InvalidateList;
  end;
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

{procedure TJvCustomInspectorItem.ListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(ListBox.ClientRect, Point(X, Y)));
end;}

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
      if iifValueList in Flags then
        DropDown
      else
        Inspector.MouseCapture := True;
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
        SendMessage(ListBox.Handle, WM_LBUTTONDOWN, 0, {$IFDEF RTL230_UP}PointToLParam{$ELSE}LPARAM{$ENDIF RTL230_UP}(MousePos));
        Exit;
      end;
    end;
  end;
end;

procedure TJvCustomInspectorItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  StopTracking;
end;

procedure TJvCustomInspectorItem.NaturalSort;
var
  ItemList: TList;
begin
  ItemList := TList.Create;
  try
    BuildDisplayableList(ItemList);
    ApplyDisplayIndices(ItemList);
  finally
    ItemList.Free;
  end;
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

procedure TJvCustomInspectorItem.SetAutoUpdate(const Value: Boolean);
begin
  if Value <> AutoUpdate then
  begin
    if Value then
      Flags := Flags + [iifAutoUpdate]
    else
      Flags := Flags - [iifAutoUpdate];
  end;
end;

procedure TJvCustomInspectorItem.SetDisplayIndex(const Value: Integer);
var
  DisplayParent: TJvCustomInspectorItem;
begin
  if Value <> DisplayIndex then
  begin
    DisplayParent := GetDisplayParent;
    if DisplayParent <> nil then
      DisplayParent.UpdateDisplayOrder(Self, Value);
  end;
  SortKind := iskManual;
end;

procedure TJvCustomInspectorItem.SetDisplayIndexValue(const Value: Integer);
begin
  FDisplayIndex := Value;
end;

procedure TJvCustomInspectorItem.SetDisplayName(Value: string);
var
  S: string;
begin
  if (Parent <> nil) and (iifQualifiedNames in Parent.Flags) then
    S := Parent.DisplayName + '.';
  if S <> Copy(Value, 1, Length(S)) then
    System.Delete(Value, 1, Length(S));
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
  if (iifOwnerDrawListFixed in NewFlags) and (iifOwnerDrawListMaxHeight in NewFlags) then
    Exclude(NewFlags, iifOwnerDrawListFixed);
  if (iifOwnerDrawListFixed in NewFlags) and (iifOwnerDrawListVariable in NewFlags) then
    Exclude(NewFlags, iifOwnerDrawListFixed);
  if [iifAllowNonListValues, iifOwnerDrawListFixed, iifOwnerDrawListVariable,
      iifOwnerDrawListMaxHeight] * NewFlags <> [] then
    Include(NewFlags, iifValueList);
  if Flags <> NewFlags then
  begin
    OldFlags := Flags;
    FFlags := NewFlags;
    OldFlags := OldFlags * [iifExpanded, iifHidden, iifVisible];
    NewFlags := NewFlags * [iifExpanded, iifHidden, iifVisible];
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

procedure TJvCustomInspectorItem.SetHeight(Value: Integer);
var
  Factor: Integer;
begin
  case RowSizing.MinHeight of
    irsNameHeight:
      Dec(Value, Inspector.Painter.GetNameHeight(Self));
    irsValueHeight:
      Dec(Value, Inspector.Painter.GetValueHeight(Self));
    irsItemHeight:
      Dec(Value, Inspector.ItemHeight);
  else
    Dec(Value, RowSizing.MinHeight);
  end;
  if Value < 0 then
    Value := 0;
  case RowSizing.SizingFactor of
    irsNoReSize:
      Factor := 0;
    irsNameHeight:
      Factor := Value div Inspector.Painter.GetNameHeight(Self);
    irsValueHeight:
      Factor := Value div Inspector.Painter.GetValueHeight(Self);
    irsItemHeight:
      Factor := Value div Inspector.ItemHeight;
  else
    Factor := Value div RowSizing.SizingFactor;
  end;

  if Factor <> HeightFactor then
  begin
    HeightFactor := Factor;
    InvalidateItem;
    Inspector.CalcImageHeight;
  end;
end;

procedure TJvCustomInspectorItem.SetHeightFactor(Value: Integer);
begin
  FHeight := Value;
  Inspector.InvalidateHeight;
  InvalidateItem;
end;

procedure TJvCustomInspectorItem.SetHidden(Value: Boolean);
begin
  if Value <> Hidden then
    if Value then
      Flags := Flags + [iifHidden]
    else
      Flags := Flags - [iifHidden];
end;

procedure TJvCustomInspectorItem.SetInspector(const AInspector: TJvCustomInspector);
begin
  if Parent = nil then
    FInspector := AInspector;
end;

procedure TJvCustomInspectorItem.SetMultiline(const Value: Boolean);
begin
  if Value <> Multiline then
    if Value then
      Flags := Flags + [iifMultiLine]
    else
      Flags := Flags - [iifMultiLine];
end;

procedure TJvCustomInspectorItem.SetOnCompare(const Value: TInspectorItemSortCompare);
begin
  if @Value <> @OnCompare then
  begin
    FOnCompare := Value;
    if @Value = nil then
      SortKind := iskNone;
    InvalidateSort;
  end;
end;

procedure TJvCustomInspectorItem.SetParent(const Value: TJvCustomInspectorItem);
begin
  if Parent <> Value then
    if Parent = nil then
      FParent := Value
    else
      raise EJvInspectorItem.CreateRes(@RsEJvInspItemHasParent);
end;

procedure TJvCustomInspectorItem.SetQualifiedNames(const Value: Boolean);
begin
  if Value <> QualifiedNames then
    if Value then
      Flags := Flags + [iifQualifiedNames]
    else
      Flags := Flags - [iifQualifiedNames];
end;

procedure TJvCustomInspectorItem.SetReadOnly(const Value: Boolean);
begin
  if Value <> ReadOnly then
    if Value then
      Flags := Flags + [iifReadonly]
    else
      Flags := Flags - [iifReadonly];
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

procedure TJvCustomInspectorItem.SetRowSizing(Value: TJvInspectorItemSizing);
begin
  if (Value <> nil) and (Value <> RowSizing) then
    RowSizing.Assign(Value);
end;

procedure TJvCustomInspectorItem.SetSortKind(Value: TInspectorItemSortKind);
begin
  if (Value = iskCustom) and (@OnCompare = nil) then
    Value := iskNone;
  if Value <> SortKind then
  begin
    FSortKind := Value;
    InvalidateSort;
  end;
end;

procedure TJvCustomInspectorItem.SetVisible(Value: Boolean);
begin
  if Value <> Visible then
    if Value then
      Flags := Flags + [iifVisible]
    else
      Flags := Flags - [iifVisible];
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

procedure TJvCustomInspectorItem.UpdateDisplayOrder(const Item: TJvCustomInspectorItem;
  const NewIndex: Integer);
var
  L: TList;
begin
  L := TList.Create;
  try
    BuildDisplayableList(L);
    L.Sort(DisplayIndexSortCompare);
    L.Remove(Item);
    L.Insert(NewIndex, Item);
    ApplyDisplayIndices(L);
  finally
    L.Free;
  end;
end;

procedure TJvCustomInspectorItem.UpdateLastPaintGeneration;
begin
  FLastPaintGen := GetInspectorPaintGeneration;
end;

//NEW: Allow us to read the Name of an attribute from the
// inspector item, since sometimes the data item is nil. Also make it virtual.
// We override this in Category objects.

function TJvCustomInspectorItem.GetName: string;
begin
  if Assigned(FData) then
    Result := Self.FData.Name
  else
    Result := '';
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
  begin
    Inspector.RemoveNotifySort(Self);
    Inspector.RemoveVisible(Self);
    if Inspector.RowSizingItem = Self then
    begin
      Inspector.RowSizing := False;
      Inspector.RowSizingItem := nil;
    end;
  end;
  FItems.Free;
  if Data <> nil then
    FData.RemoveItem(Self);
  FreeAndNil(FRowSizing);
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
var
  Disp: TJvCustomInspectorItem;
begin
  Disp := Items[Index].GetDisplayParent;
  if Inspector.Selected = Items[Index] then
  begin
    Inspector.SetSelected(Disp);
    if Inspector.Selected = Items[Index] then
      Inspector.SelectedIndex := -1;
  end;
  FItems.Delete(Index);
  if Disp <> nil then
    Disp.InvalidateSort
  else
    InvalidateSort;
end;

procedure TJvCustomInspectorItem.Delete(const Item: TJvCustomInspectorItem);
var
  Idx: Integer;
begin
  Idx := IndexOf(Item);
  if Idx > -1 then
    Delete(Idx);
end;

procedure TJvCustomInspectorItem.Delete(const Data: TJvCustomInspectorData);
var
  Idx: Integer;
begin
  Idx := IndexOf(Data);
  if Idx > -1 then
    Delete(Idx);
end;

procedure TJvCustomInspectorItem.DrawEditor(const ACanvas: TCanvas);
const
  LeftOffs = 3;
var
  R: TRect;
  BFlags: Integer;
  W, G, I: Integer;
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
      if iifValueList in Flags then
      begin
        if Assigned(EditCtrl) and (not EditCtrl.Enabled) then
          BFlags := DFCS_INACTIVE
        else
        if Pressed then
          BFlags := DFCS_FLAT or DFCS_PUSHED;
        DrawThemedFrameControl(ACanvas.Handle, R, DFC_SCROLL, BFlags or DFCS_SCROLLCOMBOBOX, Inspector.CurrentPPI);
      end
      else
      if iifEditButton in Flags then
      begin
        if Pressed then
          BFlags := BF_FLAT;
        {$IFDEF JVCLThemesEnabled}
        if StyleServices.Enabled then
          DrawThemedButtonFace(Inspector, ACanvas, R, 0, bsNew, False, Pressed, False, False)
        else
        {$ENDIF JVCLThemesEnabled}
          DrawEdge(ACanvas.Handle, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or BFlags);
        W := 2;
        G := (RectWidth(R) - 2 * Ord(Pressed) - (3 * W)) div 4;
        if G < 1 then
        begin
          W := 1;
          G := (RectWidth(R) - 2 * Ord(Pressed) - (3 * W)) div 4;
        end;
        if G < 1 then
          G := 1;
        if G > 3 then
          G := 3;

        BFlags := R.Left + (RectWidth(R) - 3 * W - 2 * G) div 2 + Ord(Pressed);
        I := R.Top + (RectHeight(R) - W) div 2;
        PatBlt(ACanvas.Handle, BFlags, I, W, W, BLACKNESS);
        PatBlt(ACanvas.Handle, BFlags + G + W, I, W, W, BLACKNESS);
        PatBlt(ACanvas.Handle, BFlags + 2 * G + 2 * W, I, W, W, BLACKNESS);
      end;
    end;
end;

procedure TJvCustomInspectorItem.DrawName(const ACanvas: TCanvas);
var
  ARect: TRect;
begin
  ARect := Rects[iprName];
  if (Inspector.Painter <> nil) and (Inspector.Painter.DrawNameEndEllipsis) then
  begin
    ARect.Right := ARect.Right - 2;
    DrawText(ACanvas, PChar(DisplayName), -1, ARect, DT_END_ELLIPSIS);
  end
  else
  begin
    ACanvas.TextRect(ARect, ARect.Left, ARect.Top, DisplayName);
  end;
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
    begin
      if not (iifMultiLine in Flags) then
        ACanvas.TextRect(ARect, ARect.Left, ARect.Top, S)
      else
      begin
        DrawTextEx(ACanvas, PChar(S), Length(S), ARect, DT_EDITCONTROL or DT_WORDBREAK, nil);
      end;
    end
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
    Result := (iifVisible in Items[I].Flags) and (not (iifHidden in Items[I].Flags) or
      ((iifExpanded in Items[I].Flags) and Items[I].HasViewableItems));
    Inc(I);
  end;
end;

function TJvCustomInspectorItem.IndexOf(const Item: TJvCustomInspectorItem): Integer;
begin
  Result := Pred(Count);
  while (Result > -1) and (Items[Result] <> Item) do
    Dec(Result);
end;

function TJvCustomInspectorItem.IndexOf(const Data: TJvCustomInspectorData): Integer;
begin
  Result := Pred(Count);
  while (Result > -1) and (Items[Result].Data <> Data) do
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
  Memo: TMemo;
begin
  SetEditing(CanEdit);
  if Editing and (FUpdateEditCtrl = 0) then
  begin
    if Multiline then
    begin
      Memo := TJvInspectorMemo.Create(Inspector);
      Memo.OnKeyPress := EditKeyPress;
      Memo.WordWrap := True;
      Memo.WantReturns := False;
      Memo.ScrollBars := ssVertical;
      Memo.OnExit := EditFocusLost;
      TJvInspectorMemo(Memo).OnKillFocus := EditKillFocus;
      SetEditCtrl(Memo);
    end
    else
    begin
      Edit := TJvInspectorEdit.Create(Inspector);
      Edit.OnKeyPress := EditKeyPress;
      Edit.OnExit := EditFocusLost;
      TJvInspectorEdit(Edit).OnKillFocus := EditKillFocus;
      SetEditCtrl(Edit);
    end;
    if iifEditFixed in Flags then
    begin
      TCustomEditAccessProtected(EditCtrl).ReadOnly := True;
      TCustomEditAccessProtected(EditCtrl).TabStop := False;
      TCustomEditAccessProtected(EditCtrl).Color := Inspector.Canvas.Brush.Color;
    end
    else if Inspector.Painter <> nil then
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
      TListBox(ListBox).IntegralHeight := not (iifOwnerDrawListVariable in Flags);
      //TJvInspectorListBox(ListBox).OnMouseUp := ListMouseUp;
      TJvInspectorListBox(ListBox).OnValueSelect := ListValueSelect;
      TJvInspectorListBox(ListBox).OnDeactivate := ListDeactivate;
      TJvInspectorListBox(ListBox).Item := Self;

      TListBox(ListBox).ItemHeight := 11;
      if (iifOwnerDrawListFixed in Flags) or (iifOwnerDrawListMaxHeight in Flags) then
        TListBox(ListBox).Style := lbOwnerDrawFixed
      else
       if iifOwnerDrawListVariable in Flags then
         TListBox(ListBox).Style := lbOwnerDrawVariable;
      TListBox(ListBox).OnDrawItem := DoDrawListItem;
      TListBox(ListBox).OnMeasureItem := DoMeasureListItem;
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
    TCustomEditAccessProtected(EditCtrl).OnMouseMove := EditMouseMove;
    TCustomEditAccessProtected(EditCtrl).OnMouseUp := EditMouseUp;
    TCustomEditAccessProtected(EditCtrl).OnChange := EditChange;
    EditCtrl.Visible := True;
    if Data.IsAssigned then
      EditCtrl.Text := DisplayValue
    else
      EditCtrl.Text := '';
    FLastEditCtrlText := EditCtrl.Text;
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
       (not Data.IsAssigned or (DisplayValue <> EditCtrl.Text) or
       (AutoUpdate and (FLastEditCtrlText <> EditCtrl.Text))) then
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
var
  Disp: TJvCustomInspectorItem;
begin
  Item.SetParent(Self);
  FItems.Insert(Index, Item);
  Disp := Item.GetDisplayParent;
  if Disp <> nil then
    Disp.InvalidateSort
  else
    InvalidateSort;
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

  {$IFDEF MSWINDOWS}
  //  OutputDebugString(PChar('ScrollIntoView: FDisplayName'));
  {$ENDIF MSWINDOWS}
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

procedure TJvCustomInspectorItem.Sort;
begin
  case SortKind of
    iskNone:
      NaturalSort;
    iskName:
      AlphaSort;
    iskCustom:
      DataSort;
  end;
end;

//=== { TJvInspectorCustomCategoryItem } =====================================

function TJvInspectorCustomCategoryItem.GetName: string;
begin
  Result := FName;
end;

function TJvInspectorCustomCategoryItem.IsCategory: Boolean;
begin
  Result := True;
end;

procedure TJvInspectorCustomCategoryItem.SetFlags(const Value: TInspectorItemFlags);
var
  NewFlags: TInspectorItemFlags;
begin
  NewFlags := Value - [iifAutoUpdate, iifMultiLine, iifValueList,
    iifAllowNonListValues, iifOwnerDrawListFixed, iifOwnerDrawListVariable,
    iifOwnerDrawListMaxHeight, iifEditButton] + [iifReadonly, iifEditFixed];
  inherited SetFlags(NewFlags);
end;

//=== { TJvInspectorEnumItem } ===============================================

function TJvInspectorEnumItem.GetDisplayValue: string;
var
  IntVal: Integer;
begin
  IntVal := Ord(Data.AsOrdinal);
  if IntVal < 0 then // prevent GetEnumName crash. WAP.
    Result := IntToStr(IntVal)
  else
    Result := GetEnumName(Data.TypeInfo, IntVal);
end;

procedure TJvInspectorEnumItem.GetValueList(const Strings: TStrings);
var
  EnumInfo: IJclEnumerationTypeInfo;
  I: Integer;
begin
  EnumInfo := JclTypeInfo(Data.TypeInfo) as IJclEnumerationTypeInfo;
  for I := EnumInfo.MinValue to EnumInfo.MaxValue do
    if Trim(EnumInfo.Names[I]) <> '' then
      Strings.Add(EnumInfo.Names[I]);
end;

procedure TJvInspectorEnumItem.SetDisplayValue(const Value: string);
var
  OrdVal: Integer;
begin
  OrdVal := GetEnumValue(Data.TypeInfo, Value);
  if OrdVal <> -1 then
    Data.AsOrdinal := GetEnumValue(Data.TypeInfo, Value)
  else
  begin
    OrdVal := StrToIntDef(Value, -1);
    if (OrdVal >= 0) and (Length(GetEnumName(Data.TypeInfo, OrdVal)) > 0) then
      Data.AsOrdinal := OrdVal
    else
      raise EJvInspectorItem.CreateResFmt(@RsEJvInspItemInvalidPropValue, [AnsiQuotedStr(Value, '''')]);
  end;
end;

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

function TJvInspectorBooleanItem.GetShowAsCheckBox: Boolean;
begin
  Result := FShowAsCheckBox;
end;

procedure TJvInspectorBooleanItem.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Bool: Boolean;
begin
  if ShowAsCheckBox then
  begin
    Bool := not (Data.AsOrdinal <> Ord(False));
    if Editing and (Shift = []) and (Key = VK_SPACE) then
    begin
      Data.AsOrdinal := Ord(Bool);
      InvalidateItem;
    end;
  end
  else
    inherited EditKeyDown(Sender, Key, Shift)
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
  // clicks toggle the box twice like a standard Windows check box (upstream
  // dropped the double-click's message, leaving the box toggled only once)
  if (ssDouble in Shift) and ShowAsCheckBox then
    Shift := Shift - [ssDouble];
  if PtInRect(FCheckRect, Point(X, Y)) and (Shift = [ssLeft]) and
    Editing and ShowAsCheckBox then
  begin
    Data.AsOrdinal := Ord(Bool);
    InvalidateItem;
  end
  else
  begin
    if not ShowAsCheckBox then
      inherited MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TJvInspectorBooleanItem.SetShowAsCheckBox(Value: Boolean);
var
  WasEditing: Boolean;
begin
  if Value <> ShowAsCheckBox then
  begin
    WasEditing := Editing;
    DoneEdit(False);
    FShowAsCheckBox := Value;
    InvalidateMetaData;
    if WasEditing then
      InitEdit;
  end;
end;

procedure TJvInspectorBooleanItem.DoneEdit(const CancelEdits: Boolean = False);
begin
  if ShowAsCheckBox then
    SetEditing(False)
  else
    inherited DoneEdit(CancelEdits);
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
  if not ShowAsCheckBox then
    inherited DrawValue(ACanvas)
  else
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
    // and centered in the row (upstream hand-drew a fixed 13x13 check box in
    // raw system colors at the top of the row, which is wrong at high dpi
    // and in dark mode)
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
end;

procedure TJvInspectorBooleanItem.InitEdit;
begin
  if ShowAsCheckBox then
    SetEditing(CanEdit)
  else
    inherited InitEdit;
end;

//=== { TJvCustomInspectorData } =============================================

constructor TJvCustomInspectorData.Create;
begin
  raise EJvInspectorData.CreateResFmt(@RsENotSeparately, [ClassName]);
end;

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
  if not IsAssigned then
    raise EJvInspectorData.CreateRes(@RsEJvInspDataNotAssigned);
  if not HasValue then
    raise EJvInspectorData.CreateRes(@RsEJvInspDataNoValue);
end;

procedure TJvCustomInspectorData.CheckWriteAccess;
begin
  if not IsInitialized then
    raise EJvInspectorData.CreateRes(@RsEJvInspDataNotInit);
  if not HasValue then
    raise EJvInspectorData.CreateRes(@RsEJvInspDataNoValue);
end;

procedure TJvCustomInspectorData.DoneEdits(const CancelEdits: Boolean = False);
var
  I: Integer;
begin
  for I := Low(FItems) to High(FItems) do
    if Items[I].Editing then
      Items[I].DoneEdit(CancelEdits);
end;

function TJvCustomInspectorData.GetItemCount: Integer;
begin
  Result := Length(FItems);
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

procedure TJvCustomInspectorData.InitEdits;
var
  I: Integer;
begin
  for I := Low(FItems) to High(FItems) do
    if Items[I].Inspector.FocusedItem = Items[I] then
      Items[I].InitEdit;
end;

procedure TJvCustomInspectorData.Invalidate;
var
  I: Integer;
begin
  for I := High(FItems) downto Low(FItems) do
    FItems[I].InvalidateItem;
end;

function TJvCustomInspectorData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := False;
end;

procedure TJvCustomInspectorData.NotifyRemoveData(const Instance: TJvCustomInspectorData);
begin
end;

procedure TJvCustomInspectorData.RefreshEdits;
var
  I: Integer;
begin
  for I := High(FItems) downto Low(FItems) do
    if Items[I].Editing then
    begin
      Items[I].DoneEdit(True);
      Items[I].InitEdit;
    end;
end;

class function TJvCustomInspectorData.RegisterInstance(const Instance: TJvCustomInspectorData): TJvCustomInspectorData;
begin
  Result := DataRegister.Add(Instance);
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

function TJvCustomInspectorData.SupportsMethodPointers: Boolean;
begin
  Result := False;
end;

procedure TJvCustomInspectorData.BeforeDestruction;
var
  I: Integer;
begin
  for I := High(FItems) downto Low(FItems) do
    Items[I].Free;
  if FRegistered and (GlobalDataRegister <> nil) then
    DataRegister().Remove(Self);
  inherited BeforeDestruction;
end;

class function TJvCustomInspectorData.ItemRegister: TJvInspectorRegister;
begin
  if GlobalGenItemReg = nil then
  begin
    GlobalGenItemReg := TJvInspectorRegister.Create(TJvCustomInspectorData);
   // register
    RegisterDataTypeKinds;
  end;
  Result := GlobalGenItemReg;
end;

class function TJvCustomInspectorData.New: TJvCustomInspectorData;
begin
  raise EJvInspectorData.CreateResFmt(@RsENoNewInstance, [ClassName]);
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
        RegItem.ApplyDefaults(Result);
        SetLength(FItems, Length(FItems) + 1);
        FItems[High(FItems)] := Result;
        Result.InvalidateMetaData;
      end;
    end;
  end;
end;

//=== { TJvInspectorEventData } ==============================================

function TJvInspectorEventData.DoGetAsFloat: Extended;
begin
  if Assigned(FOnGetAsFloat) then
    FOnGetAsFloat(Self, Result)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
end;

function TJvInspectorEventData.DoGetAsInt64: Int64;
begin
  if Assigned(FOnGetAsInt64) then
    FOnGetAsInt64(Self, Result)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
end;

function TJvInspectorEventData.DoGetAsMethod: TMethod;
begin
  if Assigned(FOnGetAsMethod) then
    FOnGetAsMethod(Self, Result)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
end;

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

procedure TJvInspectorEventData.DoGetAsSet(out Buf; var BufSize: Integer);
begin
  if Assigned(FOnGetAsSet) then
    FOnGetAsSet(Self, Buf, BufSize)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorSet]);
end;

procedure TJvInspectorEventData.DoSetAsFloat(Value: Extended);
begin
  if Assigned(FOnSetAsFloat) then
    FOnSetAsFloat(Self, Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
end;

procedure TJvInspectorEventData.DoSetAsInt64(Value: Int64);
begin
  if Assigned(FOnSetAsInt64) then
    FOnSetAsInt64(Self, Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
end;

procedure TJvInspectorEventData.DoSetAsMethod(Value: TMethod);
begin
  if Assigned(FOnSetAsMethod) then
    FOnSetAsMethod(Self, Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
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

function TJvInspectorEventData.DoSupportsMethodPointers: Boolean;
begin
  Result := False;
  if Assigned(FOnSupportsMethodPointers) then
    FOnSupportsMethodPointers(Self, Result);
end;

procedure TJvInspectorEventData.DoSetAsSet(const Buf; var BufSize: Integer);
var
  TmpBuf: PChar;
begin
  TmpBuf := @Buf;
  if Assigned(FOnSetAsSet) then
    FOnSetAsSet(Self, TmpBuf[0], BufSize)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorSet]);
end;

function TJvInspectorEventData.GetAsFloat: Extended;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkFloat then
    Result := DoGetAsFloat
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
end;

function TJvInspectorEventData.GetAsInt64: Int64;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkInt64 then
    Result := DoGetAsInt64
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
end;

function TJvInspectorEventData.GetAsMethod: TMethod;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkMethod then
    Result := DoGetAsMethod
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
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
  if TypeInfo.Kind = tkClass then
    Result := Longword(DoGetAsOrdinal)
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

function TJvInspectorEventData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := (Ref is TJvInspectorEventData) and (TJvInspectorEventData(Ref).Name = Name) and
    (TJvInspectorEventData(Ref).TypeInfo = TypeInfo) and (TJvInspectorEventData(Ref).FParent = FParent);
end;

procedure TJvInspectorEventData.SetAsFloat(const Value: Extended);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkFloat then
    DoSetAsFloat(Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
  Invalidate;
end;

procedure TJvInspectorEventData.SetAsInt64(const Value: Int64);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkInt64 then
  begin
    if (Value < GetTypeData(TypeInfo).MinInt64Value) or
      (Value > GetTypeData(TypeInfo).MaxInt64Value) then
      raise ERangeError.CreateResFmt(@SOutOfRange, [GetTypeData(TypeInfo).MinValue,
        GetTypeData(TypeInfo).MaxValue]);
    DoSetAsInt64(Value);
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
  Invalidate;
end;

procedure TJvInspectorEventData.SetAsMethod(const Value: TMethod);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkMethod then
    DoSetAsMethod(Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
  Invalidate;
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
  if TypeInfo.Kind = tkClass then
    DoSetAsOrdinal(Longword(Value))
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
  Invalidate;
end;

procedure TJvInspectorEventData.SetAsString(const Value: string);
begin
  CheckWriteAccess;
  if TypeInfo.Kind in tkStrings then
  begin
    case TypeInfo.Kind of
      {$IFDEF UNICODE}
      tkUString:
        DoSetAsString(Value);
      {$ENDIF UNICODE}
      tkLString:
        DoSetAsString(Value);
      tkWString:
        DoSetAsString(Value);
      tkString:
        if Length(Value) < GetTypeData(TypeInfo).MaxLength then
          DoSetAsString(Value)
        else
          raise EJvInspectorData.CreateRes(@RsEJVInspDataStrTooLong);
    end;
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
  Invalidate;
end;

procedure TJvInspectorEventData.SetOnGetAsFloat(Value: TJvInspAsFloat);
begin
  if @FOnGetAsFloat <> @Value then
  begin
    FOnGetAsFloat := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnGetAsInt64(Value: TJvInspAsInt64);
begin
  if @FOnGetAsInt64 <> @Value then
  begin
    FOnGetAsInt64 := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnGetAsMethod(Value: TJvInspAsMethod);
begin
  if @FOnGetAsMethod <> @Value then
  begin
    FOnGetAsMethod := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnGetAsOrdinal(Value: TJvInspAsInt64);
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

procedure TJvInspectorEventData.SetOnGetAsSet(Value: TJvInspAsSet);
begin
  if @FOnGetAsSet <> @Value then
  begin
    FOnGetAsSet := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnSetAsFloat(Value: TJvInspAsFloat);
begin
  if @FOnSetAsFloat <> @Value then
  begin
    FOnSetAsFloat := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnSetAsInt64(Value: TJvInspAsInt64);
begin
  if @FOnSetAsInt64 <> @Value then
  begin
    FOnSetAsInt64 := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnSetAsMethod(Value: TJvInspAsMethod);
begin
  if @FOnSetAsMethod <> @Value then
  begin
    FOnSetAsMethod := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnSetAsOrdinal(Value: TJvInspAsInt64);
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

procedure TJvInspectorEventData.SetOnSetAsSet(Value: TJvInspAsSet);
begin
  if @FOnSetAsSet <> @Value then
  begin
    FOnSetAsSet := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnSupportsMethodPointers(Value: TJvInspSupportsMethodPointers);
begin
  if @FOnSupportsMethodPointers <> @Value then
  begin
    FOnSupportsMethodPointers := Value;
    Invalidate;
  end;
end;

function TJvInspectorEventData.SupportsMethodPointers: Boolean;
begin
  Result := DoSupportsMethodPointers;
end;

procedure TJvInspectorEventData.GetAsSet(var Buf);
var
  CompType: PTypeInfo;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkSet then
  begin
    CompType := GetTypeData(TypeInfo).CompType^;
    EnumMin := GetTypeData(CompType).MinValue;
    EnumMax := GetTypeData(CompType).MaxValue;
    ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
    DoGetAsSet(Buf, ResBytes);
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorSet]);
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
  Result := (TypeInfo <> nil) and (Assigned(OnGetAsFloat) or Assigned(OnGetAsInt64) or
    Assigned(OnGetAsMethod) or Assigned(OnGetAsOrdinal) or Assigned(OnGetAsString) or
    Assigned(OnGetAsSet));
end;

class function TJvInspectorEventData.New(const AParent: TJvCustomInspectorItem;
  const AName: string; ATypeInfo: PTypeInfo): TJvCustomInspectorItem;
var
  Data: TJvInspectorEventData;
begin
  Data := TJvInspectorEventData(DataRegister.Add(CreatePrim(AName, ATypeInfo)));
  if Data <> nil then
  begin
    Data.FParent := AParent;
    Result := Data.NewItem(AParent)
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TJvInspectorEventData.SetAsSet(const Buf);
var
  CompType: PTypeInfo;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkSet then
  begin
    CompType := GetTypeData(TypeInfo).CompType^;
    EnumMin := GetTypeData(CompType).MinValue;
    EnumMax := GetTypeData(CompType).MaxValue;
    ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
    DoSetAsSet(Buf, ResBytes);
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorSet]);
  Invalidate;
end;

//=== { TJvInspectorRegister } ===============================================

constructor TJvInspectorRegister.Create(const ADataClass: TJvInspectorDataClass);
begin
  inherited Create;
  FDataClass := ADataClass;
  FItems := TObjectList.Create(True);
end;

destructor TJvInspectorRegister.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJvInspectorRegister.Compare(const ADataObj: TJvCustomInspectorData;
  const Item1, Item2: TJvCustomInspectorRegItem): Integer;
begin
  Result := Item1.Compare(ADataObj, Item2);
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

procedure TJvInspectorRegister.Delete(const RegItem: TJvCustomInspectorRegItem);
begin
  FItems.Remove(RegItem);
end;

procedure TJvInspectorRegister.Delete(const ItemClass: TJvInspectorItemClass);
var
  Idx: Integer;
begin
  Idx := IndexOf(ItemClass);
  if Idx > -1 then
    Delete(Idx);
end;

procedure TJvInspectorRegister.Delete(const Index: Integer);
begin
  FItems.Delete(Index);
end;

function TJvInspectorRegister.FindMatch(const ADataObj: TJvCustomInspectorData): TJvCustomInspectorRegItem;
var
  I: Integer;
  ParDataClass: TJvInspectorDataClass;
  ParResult: TJvCustomInspectorRegItem;
begin
  Result := nil;
  for I := Pred(Count) downto 0 do
  begin
    if Items[I].IsMatch(ADataObj) then
    begin
      if Result = nil then
        Result := Items[I]
      else
      if Compare(ADataObj, Result, Items[I]) < 0 then
        Result := Items[I];
    end;
  end;
  if (Result = nil) or (Result.MatchPercent(ADataObj) <> 100) then
  begin
    ParDataClass := TJvInspectorDataClass(DataClass.ClassParent);
    while (ParDataClass <> nil) and
      ParDataClass.InheritsFrom(TJvCustomInspectorData) and
      (ParDataClass.ItemRegister = Self) do
      ParDataClass := TJvInspectorDataClass(ParDataClass.ClassParent);
    if (ParDataClass <> nil) and
      ParDataClass.InheritsFrom(TJvCustomInspectorData) and
      (ParDataClass.ItemRegister <> Self) then
    begin
      ParResult := ParDataClass.ItemRegister.FindMatch(ADataObj);
      if (ParResult <> nil) and (((Result <> nil) and
        (Result.Compare(ADataObj, ParResult) < 0)) or (Result = nil)) then
        Result := ParResult;
    end;
  end;
end;

function TJvInspectorRegister.IndexOf(const RegItem: TJvCustomInspectorRegItem): Integer;
begin
  Result := FItems.IndexOf(RegItem);
end;

function TJvInspectorRegister.IndexOf(const ItemClass: TJvInspectorItemClass): Integer;
begin
  Result := FItems.Count - 1;
  while (Result > -1) and (Items[Result].ItemClass <> ItemClass) do
    Dec(Result);
end;

//=== { TJvCustomInspectorRegItem } ==========================================

constructor TJvCustomInspectorRegItem.Create(const AItemClass: TJvInspectorItemClass);
begin
  inherited Create;
  FItemClass := AItemClass;
end;

function TJvCustomInspectorRegItem.CompareTo(const ADataObj: TJvCustomInspectorData;
  const Item: TJvCustomInspectorRegItem): Integer;
begin
  if MatchPercent(ADataObj) > Item.MatchPercent(ADataObj) then
    Result := MatchPercent(ADataObj)
  else
    Result := -Item.MatchPercent(ADataObj)
end;

function TJvCustomInspectorRegItem.GetItemClass: TJvInspectorItemClass;
begin
  Result := FItemClass;
end;

procedure TJvCustomInspectorRegItem.SetItemClass(const Value: TJvInspectorItemClass);
begin
  FItemClass := Value;
end;

procedure TJvCustomInspectorRegItem.ApplyDefaults(const Item: TJvCustomInspectorItem);
begin
  { Override in descendants to apply special defaults }
end;

function TJvCustomInspectorRegItem.Compare(const ADataObj: TJvCustomInspectorData;
  const Item: TJvCustomInspectorRegItem): Integer;
begin
  if ClassType = Item.ClassType then
  begin
    if MatchValue(ADataObj) >= Item.MatchValue(ADataObj) then
      Result := MatchValue(ADataObj)
    else
      Result := -Item.MatchValue(ADataObj);
  end
  else
    Result := -Item.CompareTo(ADataObj, Self);
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

procedure TJvInspectorTypeInfoRegItem.SetTypeInfo(Value: PTypeInfo);
begin
  FTypeInfo := Value;
end;

function TJvInspectorTypeInfoRegItem.MatchValue(const ADataObj: TJvCustomInspectorData): Integer;
begin
  if ADataObj.TypeInfo = TypeInfo then
    Result := 100
  else
  if (TypeInfo.Kind = tkClass) and (ADataObj.TypeInfo.Kind = tkClass) and
    (GetTypeData(ADataObj.TypeInfo).ClassType.InheritsFrom(GetTypeData(TypeInfo).ClassType)) then
    Result := 50
  else
    Result := 0;
end;

function TJvInspectorTypeInfoRegItem.MatchPercent(const ADataObj: TJvCustomInspectorData): Integer;
begin
  { Matching TypeInfo is a perfect match. Since MatchValue already returns a
    percentage, just return that value. }
  Result := MatchValue(ADataObj);
end;

//=== { TJvInspectorTCaptionRegItem } ========================================

procedure TJvInspectorTCaptionRegItem.ApplyDefaults(const Item: TJvCustomInspectorItem);
begin
  if Item <> nil then
    with Item do
    begin
      AutoUpdate := True;
      Flags := Item.Flags + [iifMultiLine];
      RowSizing.SizingFactor := irsValueHeight;
      RowSizing.MinHeight := irsItemHeight;
      RowSizing.Sizable := True;
    end;
end;

//=== { TJvInspectorTypeKindRegItem } ========================================

constructor TJvInspectorTypeKindRegItem.Create(const AItemClass: TJvInspectorItemClass;
  const ATypeKind: TTypeKind);
begin
  inherited Create(AItemClass);
  FTypeKind := ATypeKind;
end;

function TJvInspectorTypeKindRegItem.CompareTo(const ADataObj: TJvCustomInspectorData;
  const Item: TJvCustomInspectorRegItem): Integer;
begin
  if Item is TJvInspectorTypeInfoRegItem then
    Result := -Item.MatchValue(ADataObj)
  else
    Result := inherited CompareTo(ADataObj, Item);
end;

function TJvInspectorTypeKindRegItem.GetTypeKind: TTypeKind;
begin
  Result := FTypeKind;
end;

procedure TJvInspectorTypeKindRegItem.SetTypeKind(const Value: TTypeKind);
begin
  FTypeKind := Value;
end;

function TJvInspectorTypeKindRegItem.Compare(const ADataObj: TJvCustomInspectorData;
  const Item: TJvCustomInspectorRegItem): Integer;
begin
  if Item is TJvInspectorTypeInfoRegItem then
  begin
    if MatchValue(ADataObj) >= Item.MatchValue(ADataObj) then
      Result := MatchValue(ADataObj)
    else
      Result := -Item.MatchValue(ADataObj);
  end
  else
    Result := inherited Compare(ADataObj, Item);
end;

function TJvInspectorTypeKindRegItem.MatchValue(const ADataObj: TJvCustomInspectorData): Integer;
begin
  if ADataObj.TypeInfo.Kind = TypeKind then
    Result := 100
  else
    Result := 0;
end;

function TJvInspectorTypeKindRegItem.MatchPercent(const ADataObj: TJvCustomInspectorData): Integer;
begin
  { Matching TypeKind is 50% match. Since MatchValue returns either 0 or 100,
    devide it by two to get 0 or 50. }
  Result := MatchValue(ADataObj) div 2;
end;

procedure RegisterDataTypeKinds;
begin
  if TJvCustomInspectorData.ItemRegister = nil then
    raise EJvInspectorReg.CreateRes(@RsEJvInspNoGenReg);
  with TJvCustomInspectorData.ItemRegister do
  begin
    {$IFDEF UNICODE}
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorStringItem, tkUString));
    {$ENDIF UNICODE}
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorStringItem, tkLString));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorStringItem, tkWString));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorStringItem, tkString));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorEnumItem, tkEnumeration));
    Add(TJvInspectorTCaptionRegItem.Create(TJvInspectorStringItem, TypeInfo(TCaption)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorBooleanItem, TypeInfo(Boolean)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorBooleanItem, TypeInfo(ByteBool)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorBooleanItem, TypeInfo(WordBool)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorBooleanItem, TypeInfo(LongBool)));
  end;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  FreeAndNil(GlobalCanvasStack);
  FreeAndNil(FieldGlobalInspReg);
  FreeAndNil(GlobalDataRegister);
  FreeAndNil(GlobalGenItemReg);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
