{ Modification notice: this unit was taken from JVCL
  (github.com/project-jedi/jvcl), commit
  b045b99e132c325a25b28769ae5db1b81b2234ef, for inclusion with Inno Setup.
  Changes:
  - jvcl.inc replaced by a minimal Inno Setup version
  - the JVCL/JCL dependencies replaced by JvInspectorSupport.pas
  - ItemHeight and the built-in expand/collapse buttons and the boolean check
    boxes now scale to the inspector's current dpi
  - the ItemHeight default was raised from 16 to 18 so the default font's
    descenders and bracket glyphs fit
  - the built-in expand/collapse buttons now follow the painter's colors
    instead of hard-coded black on white
  - the boolean check boxes are now drawn themed through
    DrawThemedFrameControl and centered in the row
  - the inspector now rescales its divider, band width and the painters'
    fonts when its dpi changes }

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

 RECENT CHANGES:
    May 23, 2004, Markus Spoettl:
      - Added DrawNameEndEllipsis property to Painter (1745)
      - Fixed list-deactivate bugs (1651) and list drop-down bug 1672
    May 3, 2004, Marcel Bestebroer:
      - Additional checks for value list location and size.
      - Correction list width calculation for non-ownerdrawn value lists.
    May 3, 2004, Markus Spoettl:
      - Right align value list instead of left align (compatible with both
        Borland inspector and VS.NET property grid).
      - Fixed width measurement for font name item (used empty font name if
        fonts where not displayed in the actual font).
    May 2, 2004, Markus Spoettl:
      - Added iifOwnerDrawListMaxHeight flag; using this flag will result in
        a fixed height owner draw list; the item height used will be that of
        the tallest item in the list (i.e. DoMeasureListItem is called on each
        item before the list is shown; the largest Height value returned will
        be used as the list box's ItemHeight value).
      - Font name item will use the new iifOwnerDrawListMaxHeight flag instead
        of iifOwnerDrawListVariable.
    Apr 30, 2004, Marcel Bestebroer:
      - Using the MouseWheel during drop down, will no longer result in the
        scrolling of the inspector. Unfortunately, it will also not scroll
        the drop down list.
      - Mantis 1617: Allow Ctrl+Enter to toggle Expanded state, Ctrl+Left to
        collapse and Ctrl+Right to expand.
      - Added UseFont property to TJvInspectorFontNameItem. When set to True
        the actualy font represented is used to render the font name, \
        otherwise the standard font is used. Note: the property defaults to
        False which breaks backwards compatability. This is deliberate, since
        the previous mechanism was to slow on slower machines to be usable.
        Borland has the same default settings for the Object Inspector.
    Apr 28, 2004, Markus Spoettl:
      - Added rectangle around check mark boolean items (Mantis #1645).
      - Exposed DropDownCount to specify the number of rows in a drop down
        list (Mantis #1646).
      - Fixed minor issue regarding item heights (Mantis #1647).
      - Added default bkTile to TJvCustomInspector.BevelKind property.
    Apr 23, 2004, Marcel Bestebroer:
      - Added OnItemValueError event, which is fired when an exception occurs
        during the Item's Apply method. If no handler is assigned, the
        exception will be raised, otherwise the event handler is called.
    Apr 16, 2004, Marcel Bestebroer:
      - Fixed an issue regarding resizable items or items with non-default
        sizes in combination with the .Net painter.
    Apr 15, 2004, Marcel Bestebroer:
      - Type mapper also mapped all types of the same class to the first
        mapping of that class (as in all enums mapped to the first added
        enum type mapping, all sets mapped to the first added set type
        mapping, etc). Will be changed to allow mapping of descendants of
        the specified class instead (or ranges of the integer types?) at a
        later date.
    Apr 11, 2004, Marcel Bestebroer:
      - Index out of range errors and/or AV could show up when closing an
        application. This happened mostly in cases where you had a number
        of class items with sub items for the properties of that class.
      - Corrected OnEnter/OnExit behavior of the inspector (often got fired
        when switching from edit control back to inspector or edit control
        of next/previous item).
      - Added 'hide selection' support. The DotNET painter is currently the
        only painter that supports this. When focus is moved out of the
        inspector (and not to an inline edit control) the HideSelectColor and
        HideSelectTextColor properties are used instead of the SelectedColor
        and SelectedTextColor properties.
      - .Net painter issue: divider line between two categories were missing
        the pixels where the divider between the name and value should have
        been.
      - Various paint issues when the divider was dragged further left than
        where the name started (expand/collapse button drawn over the value,
        name selection rectangle partly visible above/below and to the left
        of the value and other minor visual side effects).
      - Class item editor can now be treated as a category.
    Apr 10, 2004, Marcel Bestebroer:
      - Double clicking a category item will now expand/collapse regardless
        of the position of the mouse (used to work only when clicking left of
        the divider bar). See mantis issue 1610.
      - Changed the MatchPercentage of the type mapper (changing the weights
        of the various parts so that a type info match will always override
        a class+name match).
      - Removed the mechanism used to save the edited value before the focus
        changes or the editor button is clicked; the mechanism used would
        change the selected item which is bad. Besides that, the saving can
        be accomplished by either calling Apply on the item or use SaveValues
        of the inspector.
      - Editor events exposed at the inspector are renamed to OnEditorXxxx as
        to not interfere (or to be confused) with the standard events
        supported by the inspector (the inspector key and mouse events are now
        also exposed by TJvInspector).
      - Property Name was redeclared in TJvInspectorCustomCategory. It is now
        properly overridden (with only a new write acces specifier specified).
    Apr 9, 2004, Marcel Bestebroer:
      - Any item can now be treated like a category item (not just
        TJvInspectorCustomCategoryItem and descendants), using the IsCategory
        virtual protected method. As a result the (Get)BaseCategory and
        (Get)Category properties/methods will return a TJvCustomInspectorItem
        instance.
      - Set items main class can now be displayed as a category item; when the
        new isfRenderAsCategory flag is specified, the set members are always
        created as sub items (i.e. isfCreateMembers is implied to be set).
      - AddComponent can now add any object instance (not only TComponent
        instances). If not category name is specified, properties are added to
        the root (Expanded parameter is ignored in this case).
      - Add type mapper for TJvInspectorPropData. The mapper allows to map the
        properties actual type to a custom type (e.g. a type generated by
        JclGenerateEnumType). Mappings can be based on the class of the
        instance, the name of the property and/or the type of the property.
    Apr 8, 2004, Marcel Bestebroer:
      - trigger the AfterDataCreate event in TJvCustomInspectorData.NewItem.

    Mar 16, 2004, anonymous:
      - do not show own class for TControl selection in property.
        make sure that you set ComponentIndex to DisplayIndex

    Feb 8, 2004, Olivier Sannier obones att altern dott org
      - Introduced the TJvTypeInfoHelper class to help C++ Builder
        users to get Type Information
      - Corrected heaps of C++ Builder compatibility problems, especially
        with parameters that are const pointers
    Oct 10, 2003, Andreas Hausladen Andreas dott Hausladen att gmx dott de
      - implemented Theming and MouseWheel
    Oct 1, 2003, Warren Postma warrenpstma att hotmail dott com
      - New Name, UserData properties in TJvInspectorCustomCategoryItem
    September 30, Warren Postma warrenpstma att hotmail dott com
      - New string property Name, in inspector and category items
        (TJvCustomInspectorItem, and descendants, ie TJvInspectorCustomCategoryItem )
        holds the variable name or property name or ini file entry name, whereas
        the DisplayName is a description for the end-user only. Note that this is
        sometimes duplicated by the Item.Data.Name, but sometimes Item.Data is nil,
        so this becomes important as a backup.
      - System Sound (Beep) on enter key removed.

-----------------------------------------------------------------------------}
// $Id$

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
  TJvInspectorCompoundColumn = class;
  TJvInspectorCustomCompoundItem = class;
  TJvInspectorCustomCategoryItem = class;
  TJvCustomInspectorData = class;
  TJvInspectorRegister = class;
  TJvCustomInspectorRegItem = class;
  TJvInspectorEventData = class;
  TJvInspectorPropData = class;

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
  TInspectorSetFlag = (isfEditString, isfCreateMemberItems, isfRenderAsCategory);
  TInspectorSetFlags = set of TInspectorSetFlag;
  TInspectorClassFlag = (icfCreateMemberItems, icfShowClassName, icfRenderAsCategory);
  TInspectorClassFlags = set of TInspectorClassFlag;
  TInspectorComponentFlag = (icfShowOwnerNames, icfNoShowFirstOwnerName, icfSortComponents,
    icfSortOwners, icfKeepFirstOwnerAsFirst);
  TInspectorComponentFlags = set of TInspectorComponentFlag;
  TInspectorCompoundItemFlag = (icifSingleName, icifSingleNameUseFirstCol);
  TInspectorCompoundItemFlags = set of TInspectorCompoundItemFlag;
  TInspectorTMethodFlag = (imfShowInstanceNames, imfNoShowFirstInstanceName, imfSortMethods,
    imfSortInstances, imfKeepFirstInstanceAsFirst);
  TInspectorTMethodFlags = set of TInspectorTMethodFlag;
  TJvInspectorStyle = (isBorland, isDotNet, isItemPainter);

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

  // Don't use the const qualifier on events when compiling for BCB
  // because this would lead to the generation of a parameter that is
  // a non const pointer to a const object.
  // Then this would trigger warnings about using non const methods on
  // a const object when we modify the properties of the object.
  // We would have liked to be able to generate a const pointer to a
  // non const object (which is what the Delphi declaration is) but the
  // HPP Generator is compeletely flawed in this area
  TInspectorItemEvent = procedure(Sender: TObject; Item: TJvCustomInspectorItem) of object;
  TInspectorItemBeforeCreateEvent = procedure(Sender: TObject; Data: TJvCustomInspectorData;
    var ItemClass: TJvInspectorItemClass) of object;
  TInspectorItemBeforeSelectEvent = procedure(Sender: TObject; NewItem: TJvCustomInspectorItem;
    var Allow: Boolean) of object;
  TInspectorDataEvent = procedure(Sender: TObject; Data: TJvCustomInspectorData) of object;
  TInspectorItemGetValueListEvent = procedure(Item: TJvCustomInspectorItem; Values: TStrings) of object;
  TInspectorItemSortCompare = function(Item1, Item2: TJvCustomInspectorItem): Integer of object;
  TJvInspAsFloat = procedure(Sender: TJvInspectorEventData; var Value: Extended) of object;
  TJvInspAsInt64 = procedure(Sender: TJvInspectorEventData; var Value: Int64) of object;
  TJvInspAsMethod = procedure(Sender: TJvInspectorEventData; var Value: TMethod) of object;
  TJvInspAsString = procedure(Sender: TJvInspectorEventData; var Value: string) of object;
  TJvInspAsSet = procedure(Sender: TJvInspectorEventData; var Value; var BufSize: Integer) of object;
  TJvInspSupportsMethodPointers = procedure(Sender: TJvInspectorEventData; var SupportsTMethod: Boolean) of object;
  TJvInspConfSectionEvent = procedure(var SectionName: string; var Parse: Boolean) of object;
  TJvInspConfKeyEvent = procedure(const SectionName: string; var ItemName: string; var ATypeInfo: PTypeInfo;
    var Allow: Boolean) of object;
  TInspectorValueErrorEvent = procedure(Sender: TObject; Item: TJvCustomInspectorItem;
    ExceptObject: Exception) of object;
  TInspectorValueChangingEvent = procedure(Sender: TObject; Item: TJvCustomInspectorItem; var NewValue: string; var AllowChange: Boolean) of object;
  // new event types (sept 2004) -wp
  TInspectorBeforeEditEvent = procedure(Sender: TObject; Item: TJvCustomInspectorItem; Edit: TCustomEdit) of object;

  EJvInspector = class(EJVCLException);
  EJvInspectorItem = class(EJvInspector);
  EJvInspectorData = class(EJvInspector);
  EJvInspectorReg = class(EJvInspector);

  TOnJvInspectorSetItemColors = procedure(Item: TJvCustomInspectorItem; Canvas: TCanvas) of object;

  TOnJvInspectorMouseDown = procedure(Sender: TJvCustomInspector; Item: TJvCustomInspectorItem;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  TOnJvInspectorItemEdit = procedure(Sender: TJvCustomInspector;
    Item: TJvCustomInspectorItem; var DisplayStr: string) of object;

  TJvCustomInspectorBase = TJvCustomControl;

  TJvCustomInspector = class(TJvCustomInspectorBase)
  private
    FAfterDataCreate: TInspectorDataEvent;
    FAfterItemCreate: TInspectorItemEvent;
    FBandSizing: Boolean;
    FBandSizingBand: Integer;
    FBandStartsSB: TList;
    FBandStartsNoSB: TList;
    FBandWidth: Integer;
    FBeforeItemCreate: TInspectorItemBeforeCreateEvent;
    FBeforeSelection: TInspectorItemBeforeSelectEvent;
    FCollapseButton: TBitmap;
    FDivider: Integer;
    FDraggingDivider: Boolean;
    FDividerDragBandX: Integer;
    FExpandButton: TBitmap;
    FImageHeight: Integer;
    FItemHeight: Integer;
    FLockCount: Integer;
    FNeedRebuild: Boolean;
    FNeedRedraw: Boolean;
    FSortNotificationList: TList;
    FOnDataValueChanged: TInspectorDataEvent;
    FOnItemSelected: TNotifyEvent;
    FOnItemValueChanged: TInspectorItemEvent;
    FPainter: TJvInspectorPainter;
    FPaintGen: Integer;
    FReadOnly: Boolean;
    FRelativeDivider: Boolean;
    FRoot: TJvCustomInspectorItem;
    FRowSizing: Boolean;
    FRowSizingItem: TJvCustomInspectorItem;
    FSelectedIndex: Integer;
    FSelecting: Boolean;
    FTopIndex: Integer;
    FUseBands: Boolean;
    FVisibleList: TStringList;
    FWantTabs: Boolean;
    FAutoComplete: Boolean;
    FAutoDropDown: Boolean; // depends on AutoComplete
    FOnEditorContextPopup: TContextPopupEvent;
    FOnEditorKeyDown: TKeyEvent;
    FOnEditorKeyPress: TKeyPressEvent;
    FOnEditorKeyUp: TKeyEvent;
    FOnEditorMouseDown: TOnJvInspectorMouseDown;
    FOnItemDoubleClicked: TInspectorItemEvent;
    FOnItemEdit: TOnJvInspectorItemEdit; // User clicks Ellipsis button.
    FOnItemValueError: TInspectorValueErrorEvent;
    FOnItemValueChanging: TInspectorValueChangingEvent;
    FInspectObject: TObject;
    // BeforeEdit NOTE: - WAP
    //
    // This event fired is when creating TEdit or TMemo objects, and
    // allows end users to customize the properties of the editor
    // objects, or hook event handlers, which were
    // otherwise invisible. This could be used to ill effect, so beware.
    // We already expose some critical events in a nicer way,
    // so only use BeforeEdit as a last-resort. Instead consider using:
    // BeforeSelection if you want to prevent the editing from ocurring,
    // or if you need to handle mouse and keyboard events in the editor
    // use one of these:
    //   OnEditorKeyDown, OnEditorKeyUp,
    //   OnEditorKeyPress, OnEditorMouseDown,
    //   OnEditorContextPopup.etc.
    // Also, If you want the event that occurs when the user clicks the ellipsis
    // button, you want OnItemEdit, not BeforeEdit.
    FBeforeEdit: TInspectorBeforeEditEvent;
    FStyle: TJvInspectorStyle;
    FStylePainter: TJvInspectorPainter;
    FSettingStyle: Boolean;
    FMouseWheelRecursion: Boolean;
    procedure SetInspectObject(const Value: TObject);
    procedure SetStyle(const Value: TJvInspectorStyle);
    function GetActivePainter: TJvInspectorPainter;
    //    FOnMouseDown: TInspectorMouseDownEvent;
  protected
    function CalcImageHeight: Integer; virtual;
    function CalcItemIndex(X, Y: Integer; var Rect: TRect): Integer; virtual;
    function CalcItemRect(const Item: TJvCustomInspectorItem): TRect; virtual;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure CMActivate(var Msg: TCMActivate); message CM_ACTIVATE;
    procedure CMDeactivate(var Msg: TCMActivate); message CM_DEACTIVATE;
    procedure DoAfterDataCreate(const Data: TJvCustomInspectorData); virtual;
    procedure DoAfterItemCreate(const Item: TJvCustomInspectorItem); virtual;
    procedure DoBeforeItemCreate(const Data: TJvCustomInspectorData;
      var ItemClass: TJvInspectorItemClass); virtual;
    function DoBeforeItemSelect(const NewItem: TJvCustomInspectorItem): Boolean; virtual;
    procedure DoDataValueChanged(const Data: TJvCustomInspectorData); virtual;
    procedure DoItemSelected; virtual;
    procedure DoItemValueChanged(const Item: TJvCustomInspectorItem); virtual;
    function DoItemValueChanging(const Item: TJvCustomInspectorItem; var NewValue: string): Boolean; virtual;
    function DoItemValueError(Item: TJvCustomInspectorItem): Boolean; virtual;
    function GetAfterDataCreate: TInspectorDataEvent; virtual;
    function GetAfterItemCreate: TInspectorItemEvent; virtual;
    function GetBandFor(const ItemIdx: Integer): Integer; virtual;
    function GetBandStarts: TList; virtual;
    function GetBandWidth: Integer; virtual;
    function GetBeforeItemCreate: TInspectorItemBeforeCreateEvent; virtual;
    function GetBeforeSelection: TInspectorItemBeforeSelectEvent; virtual;
    function GetButtonRect(const ItemIndex: Integer): TRect; virtual;
    function GetCollapseButton: TBitmap; virtual;
    function GetDivider: Integer; virtual;
    function GetDividerAbs: Integer; virtual;
    function GetExpandButton: TBitmap; virtual;
    function GetImageHeight: Integer; virtual;
    function GetItemHeight: Integer; virtual;
    function GetLastFullVisible: Integer; virtual;
    function GetLockCount: Integer; virtual;
    function GetOnItemSelected: TNotifyEvent; virtual;
    function GetPainter: TJvInspectorPainter; virtual;
    function GetReadOnly: Boolean; virtual;
    function GetRelativeDivider: Boolean; virtual;
    function GetRoot: TJvCustomInspectorItem; virtual;
    function GetSelected: TJvCustomInspectorItem; virtual;
    function GetSelectedIndex: Integer; virtual;
    function GetTopIndex: Integer; virtual;
    function GetUseBands: Boolean; virtual;
    function GetVisibleCount: Integer; virtual;
    function GetVisibleItems(const I: Integer): TJvCustomInspectorItem; virtual;
    function GetWantTabs: Boolean; virtual;
    procedure HandleBandResize(X: Integer); virtual;
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
    procedure SetAfterDataCreate(const Value: TInspectorDataEvent); virtual;
    procedure SetAfterItemCreate(const Value: TInspectorItemEvent); virtual;
    procedure SetBandWidth(Value: Integer); virtual;
    procedure SetBeforeItemCreate(const Value: TInspectorItemBeforeCreateEvent); virtual;
    procedure SetBeforeSelection(const Value: TInspectorItemBeforeSelectEvent); virtual;
    procedure SetCollapseButton(const Value: TBitmap); virtual;
    procedure SetDivider(Value: Integer); virtual;
    procedure SetDividerAbs(Value: Integer); virtual;
    procedure SetExpandButton(const Value: TBitmap); virtual;
    procedure SetItemHeight(Value: Integer); virtual;
    procedure SetLockCount(const Value: Integer); virtual;
    procedure SetOnItemSelected(const Value: TNotifyEvent); virtual;
    procedure SetPainter(const Value: TJvInspectorPainter); virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;
    procedure SetRelativeDivider(Value: Boolean); virtual;
    procedure SetSelected(const Value: TJvCustomInspectorItem); virtual;
    procedure SetSelectedIndex(Value: Integer); virtual;
    procedure SetTopIndex(Value: Integer); virtual;
    procedure SetUseBands(Value: Boolean); virtual;
    procedure SetWantTabs(Value: Boolean); virtual;
    procedure UpdateScrollBars; virtual;
    function ViewHeight: Integer;
    function ViewRect: TRect; virtual;
    function ViewWidth: Integer;
    procedure WMHScroll(var Msg: TWMScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    procedure GetDlgCode(var Code: TDlgCodes); override;
    procedure FocusSet(PrevWnd: THandle); override;
    procedure FocusKilled(NextWnd: THandle); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure ShowScrollBars(Bar: Integer; Visible: Boolean); virtual;
    function YToIdx(const Y: Integer): Integer; virtual;
    property AutoComplete: Boolean read FAutoComplete write FAutoComplete;
    property AutoDropDown: Boolean read FAutoDropDown write FAutoDropDown;
    property BandSizing: Boolean read FBandSizing write FBandSizing;
    property BandSizingBand: Integer read FBandSizingBand write FBandSizingBand;
    property BandStarts: TList read GetBandStarts;
    property BandWidth: Integer read GetBandWidth write SetBandWidth;
    property CollapseButton: TBitmap read GetCollapseButton write SetCollapseButton;
    property ExpandButton: TBitmap read GetExpandButton write SetExpandButton;
    property Divider: Integer read GetDivider write SetDivider;
    property DividerAbs: Integer read GetDividerAbs write SetDividerAbs;
    property DraggingDivider: Boolean read FDraggingDivider write FDraggingDivider;
    property DividerDragBandX: Integer read FDividerDragBandX write FDividerDragBandX;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ImageHeight: Integer read GetImageHeight;
    property LockCount: Integer read GetLockCount;
    property NeedRebuild: Boolean read FNeedRebuild write FNeedRebuild;
    property NeedRedraw: Boolean read FNeedRedraw write FNeedRedraw;
    property SortNotificationList: TList read FSortNotificationList;
    property OnDataValueChanged: TInspectorDataEvent read FOnDataValueChanged write FOnDataValueChanged;
    property OnItemSelected: TNotifyEvent read GetOnItemSelected write SetOnItemSelected;
    property OnItemValueChanged: TInspectorItemEvent read FOnItemValueChanged write FOnItemValueChanged;
    property OnItemValueChanging: TInspectorValueChangingEvent read FOnItemValueChanging write FOnItemValueChanging;
    property AfterDataCreate: TInspectorDataEvent read GetAfterDataCreate write SetAfterDataCreate;
    property AfterItemCreate: TInspectorItemEvent read GetAfterItemCreate write SetAfterItemCreate;
    property BeforeItemCreate: TInspectorItemBeforeCreateEvent read GetBeforeItemCreate write SetBeforeItemCreate;
    property BevelKind default bkTile;
    property BeforeSelection: TInspectorItemBeforeSelectEvent read GetBeforeSelection write SetBeforeSelection;
    property Painter: TJvInspectorPainter read GetPainter write SetPainter;
    property PaintGeneration: Integer read FPaintGen;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property RelativeDivider: Boolean read GetRelativeDivider write SetRelativeDivider;
    property Root: TJvCustomInspectorItem read GetRoot;
    property InspectObject: TObject read FInspectObject write SetInspectObject;
    property RowSizing: Boolean read FRowSizing write FRowSizing;
    property RowSizingItem: TJvCustomInspectorItem read FRowSizingItem write FRowSizingItem;
    property Selected: TJvCustomInspectorItem read GetSelected;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
    property Selecting: Boolean read FSelecting write FSelecting;
    property Style: TJvInspectorStyle read FStyle write SetStyle default isBorland;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property UseBands: Boolean read GetUseBands write SetUseBands;
    property VisibleCount: Integer read GetVisibleCount;
    property VisibleItems[const I: Integer]: TJvCustomInspectorItem read GetVisibleItems;
    property WantTabs: Boolean read GetWantTabs write SetWantTabs;
    property BeforeEdit: TInspectorBeforeEditEvent read FBeforeEdit write FBeforeEdit;
    { Standard TCustomControl events - these are really events fired by
      the TEdit control used when editing in a cell!}
    property OnEditorContextPopup: TContextPopupEvent read FOnEditorContextPopup write FOnEditorContextPopup;
    property OnEditorKeyDown: TKeyEvent read FOnEditorKeyDown write FOnEditorKeyDown;
    property OnEditorKeyPress: TKeyPressEvent read FOnEditorKeyPress write FOnEditorKeyPress;
    property OnEditorKeyUp: TKeyEvent read FOnEditorKeyUp write FOnEditorKeyUp;
    property OnEditorMouseDown: TOnJvInspectorMouseDown read FOnEditorMouseDown write FOnEditorMouseDown;
    property OnItemDoubleClicked: TInspectorItemEvent read FOnItemDoubleClicked write FOnItemDoubleClicked;
    property OnItemEdit: TOnJvInspectorItemEdit read FOnItemEdit write FOnItemEdit; // User clicks Ellipsis button.
    property OnItemValueError: TInspectorValueErrorEvent read FOnItemValueError write FOnItemValueError;
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
    procedure AddComponent(Instance: TObject; const CategoryName: string = ''; Expanded: Boolean = True);
    procedure Clear;
    property ActivePainter: TJvInspectorPainter read GetActivePainter;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvInspector = class(TJvCustomInspector)
  public
    property LockCount;
    property Root;
    property Selected;
    property SelectedIndex;
    property TopIndex;
    property VisibleCount;
    property VisibleItems;
    property InspectObject;
  published
    property Style;  // Must be BEFORE painter to ensure everithing is read correctly
    property Align;
    property Anchors;
    property AutoComplete default True;
    property AutoDropDown default False;
    property BandWidth default 150;
    property BevelEdges;
    property BevelKind;
    property BevelInner default bvNone;
    property BevelOuter;
    property BevelWidth;
    property CollapseButton;
    // (rom) this is usually handled in an overwritten Loaded
    property RelativeDivider default False; // Must be defined before Divider
    property Divider default 75;
    property ExpandButton;
    property Font;
    property ItemHeight;
    property Painter;
    property PopupMenu;
    property ReadOnly default False;
    property UseBands default False;
    property WantTabs default False;
    property AfterDataCreate;
    property AfterItemCreate;
    property BeforeItemCreate;
    property BeforeSelection;
    property TabStop;
    property TabOrder;
    property OnDataValueChanged;
    property OnItemSelected;
    property OnItemValueChanged;
    property OnItemValueChanging;
    property OnItemValueError;
    property OnItemDoubleClicked;
    property OnItemEdit; // User clicks Ellipsis button.
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

    // Redirected editor events
    property OnEditorContextPopup;
    property OnEditorKeyDown;
    property OnEditorKeyPress;
    property OnEditorKeyUp;
    property OnEditorMouseDown;
  end;

  TJvInspectorPainter = class(TJvComponent)
  private
    FBackgroundColor: TColor;
    FButtonImage: TBitmap;
    FCanvas: TCanvas;
    FCategoryColor: TColor;
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
    FCategoryFont: TFont;
    FValueFont: TFont;
    FNameFont: TFont;
    FSelectedFont: TFont;
    procedure FontChange(Sender: TObject);

    procedure ReadCategoryTextColor(Reader: TReader);
    procedure ReadNameColor(Reader: TReader);
    procedure ReadValueColor(Reader: TReader);
    procedure ReadSelectedTextColor(Reader: TReader);
    procedure ReadHideSelectTextColor(Reader: TReader);
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
    function GetCategoryFont: TFont; virtual;
    function GetHideSelectFont: TFont; virtual;
    function GetNameFont: TFont; virtual;
    function GetSelectedFont: TFont; virtual;
    function GetValueFont: TFont; virtual;
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
    procedure ScaleFonts(const M, D: Integer); virtual;
    procedure SetBackgroundColor(const Value: TColor); virtual;
    procedure SetCategoryColor(const Value: TColor); virtual;
    procedure SetCategoryFont(const Value: TFont); virtual;
    procedure SetDividerColor(const Value: TColor); virtual;
    procedure SetHideSelectColor(const Value: TColor); virtual;
    procedure SetHideSelectFont(const Value: TFont); virtual;
    procedure SetNameFont(const Value: TFont); virtual;
    procedure SetRects(const Index: TInspectorPaintRect; const ARect: TRect); virtual;
    procedure SetSelectedColor(const Value: TColor); virtual;
    procedure SetSelectedFont(const Value: TFont); virtual;
    procedure Setup(const ACanvas: TCanvas); virtual;
    procedure SetupItem; virtual;
    procedure SetupRects; virtual;
    procedure SetValueFont(const Value: TFont); virtual;
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
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetInspector(const AInspector: TJvCustomInspector); virtual;
    property HideSelectColor: TColor read GetHideSelectColor write SetHideSelectColor;
    property HideSelectFont: TFont read GetHideSelectFont write SetHideSelectFont;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
    property SelectedFont: TFont read GetSelectedFont write SetSelectedFont;
  published
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property CategoryColor: TColor read GetCategoryColor write SetCategoryColor;
    property CategoryFont: TFont read GetCategoryFont write SetCategoryFont;
    property DividerColor: TColor read GetDividerColor write SetDividerColor;
    property NameFont: TFont read GetNameFont write SetNameFont;
    property ValueFont: TFont read GetValueFont write SetValueFont;
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

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvInspectorBorlandPainter = class(TJvInspectorBorlandNETBasePainter)
  private
    FDividerLightColor: TColor;
    FOnSetItemColors: TOnJvInspectorSetItemColors;
  protected
    function DividerWidth: Integer; override;
    procedure DoPaint; override;
    function GetDividerLightColor: TColor; virtual;
    function GetSelectedColor: TColor; override;
    function GetSelectedFont: TFont; override;
    procedure InitializeColors; override;
    procedure PaintDivider(const X, YTop, YBottom: Integer); override;
    procedure SetDividerLightColor(const Value: TColor); virtual;
    procedure Setup(const ACanvas: TCanvas); override;
  published
    property BackgroundColor default clBtnFace;
    property DividerColor default clBtnShadow;
    property DividerLightColor: TColor read GetDividerLightColor write SetDividerLightColor default clBtnHighlight;
    property OnSetItemColors: TOnJvInspectorSetItemColors read FOnSetItemColors write FOnSetItemColors;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvInspectorDotNETPainter = class(TJvInspectorBorlandNETBasePainter)
  private
    FHideSelectColor: TColor;
    FHideSelectFont: TFont;
    FOnSetItemColors: TOnJvInspectorSetItemColors;
  protected
    procedure ApplyNameFont; override;
    function GetHideSelectColor: TColor; override;
    function GetHideSelectFont: TFont; override;
    procedure DoPaint; override;
    procedure InitializeColors; override;
    procedure PaintDivider(const X, YTop, YBottom: Integer); override;
    procedure ScaleFonts(const M, D: Integer); override;
    procedure SetHideSelectColor(const Value: TColor); override;
    procedure SetHideSelectFont(const Value: TFont); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DividerColor default clBtnFace;
    property HideSelectColor default clBtnFace;
    property HideSelectFont;
    property SelectedColor default clHighlight;
    property SelectedFont;
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
    FOnValueChanged: TNotifyEvent;
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
    procedure ButtonClick(Sender: TObject); virtual;
    function CanEdit: Boolean; virtual;
    procedure CloseUp(Accept: Boolean); virtual;
    procedure DataSort;
    procedure Deactivate; dynamic;
    procedure DoAfterItemCreate; virtual;
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
    procedure DoValueChanged; virtual;
    procedure DropDown; dynamic;
    // Defines what to do when the property editor of this inspector item is invoked.  Ie, '...' button is clicked on items with iifEdit in their flags.
    procedure Edit; virtual;
    procedure EditChange(Sender: TObject); virtual;
    procedure EditFocusLost(Sender: TObject); dynamic;
    procedure EditKillFocus(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char); dynamic;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); dynamic;
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); dynamic;
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
    function GetIsCompoundColumn: Boolean; virtual;
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
    procedure InvalidateValue; virtual;
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
    property IsCompoundColumn: Boolean read GetIsCompoundColumn;
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
    property OnValueChanged: TNotifyEvent read FOnValueChanged write FOnValueChanged;
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

  TJvInspectorCompoundColumn = class(TPersistent)
  private
    FItem: TJvCustomInspectorItem;
    FParent: TJvInspectorCustomCompoundItem;
    FWidth: Integer;
    FWidthSet: Integer;
  protected
    function GetItem: TJvCustomInspectorItem;
    function GetWidth: Integer;
    function GetWidthSet: Integer;
    procedure SetItem(Value: TJvCustomInspectorItem);
    procedure SetWidth(Value: Integer);
    procedure SetWidthExternal(Value: Integer);
    procedure SetWidthSet(Value: Integer);
    property Parent: TJvInspectorCustomCompoundItem read FParent;
  public
    constructor Create(const AParent: TJvInspectorCustomCompoundItem; const AItem: TJvCustomInspectorItem);
    procedure BeforeDestruction; override;
    property Item: TJvCustomInspectorItem read GetItem write SetItem;
    property Width: Integer read GetWidth write SetWidthExternal;
    property WidthSet: Integer read GetWidthSet;
  end;

  TJvInspectorCustomCompoundItem = class(TJvCustomInspectorItem)
  private
    FCompoundItemFlags: TInspectorCompoundItemFlags;
    FColumns: TObjectList;
    FSelectedColumnIdx: Integer;
  protected
    function AddColumnPrim(const Item: TJvCustomInspectorItem): Integer; overload; virtual;
    function AddColumnPrim(const ItemIndex: Integer): Integer; overload; virtual;
    procedure DeleteColumnPrim(const Column: TJvInspectorCompoundColumn); overload; virtual;
    procedure DeleteColumnPrim(const Index: Integer); overload; virtual;
    procedure DeleteColumnPrim(const Item: TJvCustomInspectorItem); overload; virtual;
    procedure DivideRect(const RectKind: TInspectorPaintRect; const Value: TRect); virtual;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    function GetColumnCount: Integer; virtual;
    function GetColumns(I: Integer): TJvInspectorCompoundColumn; virtual;
    function GetDisplayName: string; override;
    function GetEditCtrl: TCustomEdit; override;
    function GetEditCtrlDestroying: Boolean; override;
    function GetEditing: Boolean; override;
    function GetSelectedColumn: TJvInspectorCompoundColumn; virtual;
    function GetSelectedColumnIndex: Integer; virtual;
    function GetSingleName: Boolean;
    function GetSingleNameUseFirstCol: Boolean;
    function IndexOfColumnPrim(const Col: TJvInspectorCompoundColumn): Integer; overload; virtual;
    function IndexOfColumnPrim(const Item: TJvCustomInspectorItem): Integer; overload; virtual;
    procedure InsertColumnPrim(const Index: Integer; const Item: TJvCustomInspectorItem); overload; virtual;
    procedure InsertColumnPrim(const Index, ItemIndex: Integer); overload; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure RecalcColumnWidths(const SetColumn: TJvInspectorCompoundColumn = nil); virtual;
    procedure SetCompoundItemFlags(Value: TInspectorCompoundItemFlags);
    procedure SetDisplayName(Value: string); override;
    procedure SetEditing(const Value: Boolean); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
    procedure SetFocus; override;
    procedure SetRects(const RectKind: TInspectorPaintRect; Value: TRect); override;
    procedure SetSelectedColumn(Value: TJvInspectorCompoundColumn); virtual;
    procedure SetSelectedColumnIndex(Value: Integer); virtual;
    procedure SetSingleName(Value: Boolean);
    procedure SetSingleNameUseFirstCol(Value: Boolean);
    property ColumnCount: Integer read GetColumnCount;
    property Columns[I: Integer]: TJvInspectorCompoundColumn read GetColumns;
    property CompoundItemFlags: TInspectorCompoundItemFlags read FCompoundItemFlags write SetCompoundItemFlags;
    property SelectedColumn: TJvInspectorCompoundColumn read GetSelectedColumn write SetSelectedColumn;
    property SelectedColumnIndex: Integer read GetSelectedColumnIndex write SetSelectedColumnIndex;
    property SingleName: Boolean read GetSingleName write SetSingleName;
    property SingleNameUseFirstCol: Boolean read GetSingleNameUseFirstCol write SetSingleNameUseFirstCol;
  public
    constructor Create(const AParent: TJvCustomInspectorItem; const AData: TJvCustomInspectorData); override;
    procedure BeforeDestruction; override;
    procedure DoneEdit(const CancelEdits: Boolean = False); override;
    procedure DrawEditor(const ACanvas: TCanvas); override;
    procedure DrawName(const ACanvas: TCanvas); override;
    procedure DrawValue(const ACanvas: TCanvas); override;
    function EditFocused: Boolean; override;
    procedure InitEdit; override;
  end;

  TJvInspectorCompoundItem = class(TJvInspectorCustomCompoundItem)
  public
    function AddColumn(const Item: TJvCustomInspectorItem): Integer; overload;
    function AddColumn(const ItemIndex: Integer): Integer; overload;
    procedure DeleteColumn(const Column: TJvInspectorCompoundColumn); overload;
    procedure DeleteColumn(const Index: Integer); overload;
    procedure DeleteColumn(const Item: TJvCustomInspectorItem); overload;
    function IndexOfColumn(const Col: TJvInspectorCompoundColumn): Integer; overload;
    function IndexOfColumn(const Item: TJvCustomInspectorItem): Integer; overload;
    procedure InsertColumn(const Index: Integer; const Item: TJvCustomInspectorItem); overload;
    procedure InsertColumn(const Index, ItemIndex: Integer); overload;
    property ColumnCount;
    property Columns;
    property CompoundItemFlags;
    property SelectedColumn;
    property SelectedColumnIndex;
    property SingleName;
    property SingleNameUseFirstCol;
  end;

  TJvInspectorIntegerItem = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
  end;

  TJvInspectorEnumItem = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure GetValueList(const Strings: TStrings); override;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  end;

  TJvInspectorFloatItem = class(TJvCustomInspectorItem)
  protected
    FFormat: string;
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
  published
    property Format: string read FFormat write FFormat;
  end;

  TJvInspectorSetItem = class(TJvCustomInspectorItem)
  private
    FItemSetFlags: TInspectorSetFlags;
  protected
    function CanEdit: Boolean; override;
    procedure CreateMembers; virtual;
    procedure DeleteMembers; virtual;
    function GetCreateMemberItems: Boolean; virtual;
    function GetDisplayValue: string; override;
    function GetEditString: Boolean; virtual;
    function GetRenderAsCategory: Boolean; virtual;
    function GetItemSetFlags: TInspectorSetFlags; virtual;
    procedure InvalidateMetaData; override;
    function IsCategory: Boolean; override;
    procedure SetCreateMemberItems(const Value: Boolean); virtual;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetEditString(const Value: Boolean); virtual;
    procedure SetRenderAsCategory(const Value: Boolean); virtual;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
    procedure SetItemSetFlags(const Value: TInspectorSetFlags); virtual;
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
  published
    property ItemSetFlags: TInspectorSetFlags read GetItemSetFlags
      write SetItemSetFlags;
    property CreateMemberItems: Boolean read GetCreateMemberItems
      write SetCreateMemberItems;
    property EditString: Boolean read GetEditString write SetEditString;
    property RenderAsCategory: Boolean read GetRenderAsCategory write SetRenderAsCategory;
  end;

  TJvInspectorCharItem = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
  end;

  TJvInspectorInt64Item = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
  end;

  TJvInspectorStringItem = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
  end;

  TJvInspectorClassItem = class(TJvCustomInspectorItem)
  private
    FItemClassFlags: TInspectorClassFlags;
    FLastMemberInstance: TObject;
  protected
    procedure CreateMembers; virtual;
    function CanEdit: Boolean; override;
    procedure DeleteMembers; virtual;
    function GetCreateMemberItems: Boolean; virtual;
    function GetDisplayValue: string; override;
    function GetItemClassFlags: TInspectorClassFlags; virtual;
    function GetRenderAsCategory: Boolean; virtual;
    function GetShowClassName: Boolean; virtual;
    procedure InvalidateItem; override;
    procedure InvalidateMetaData; override;
    function IsCategory: Boolean; override;
    procedure SetCreateMemberItems(const Value: Boolean); virtual;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetItemClassFlags(Value: TInspectorClassFlags); virtual;
    procedure SetRenderAsCategory(const Value: Boolean); virtual;
    procedure SetShowClassName(const Value: Boolean); virtual;
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
    property CreateMemberItems: Boolean read GetCreateMemberItems write SetCreateMemberItems;
    property ItemClassFlags: TInspectorClassFlags read GetItemClassFlags write SetItemClassFlags;
    property OnGetValueList;
    property RenderAsCategory: Boolean read GetRenderAsCategory write SetRenderAsCategory;
    property ShowClassName: Boolean read GetShowClassName write SetShowClassName;
  end;

  TJvInspectorComponentItem = class(TJvInspectorClassItem)
  private
    FItemComponentFlags: TInspectorComponentFlags;
    FOwners: TList;
  protected
    function GetItemComponentFlags: TInspectorComponentFlags;
    function GetKeepFirstOwnerAsFirst: Boolean;
    function GetNoShowFirstOwnerName: Boolean;
    function GetOwnerCount: Integer;
    function GetOwners(I: Integer): TComponent;
    function GetShowOwnerNames: Boolean;
    function GetSortComponents: Boolean;
    function GetSortOwners: Boolean;
    procedure GetValueList(const Strings: TStrings); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
    procedure SetItemClassFlags(Value: TInspectorClassFlags); override;
    procedure SetItemComponentFlags(Value: TInspectorComponentFlags); virtual;
    procedure SetKeepFirstOwnerAsFirst(Value: Boolean);
    procedure SetNoShowFirstOwnerName(Value: Boolean);
    procedure SetOwners(I: Integer; Value: TComponent);
    procedure SetShowOwnerNames(Value: Boolean);
    procedure SetSortComponents(Value: Boolean);
    procedure SetSortOwners(Value: Boolean);
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
    procedure BeforeDestruction; override;
    procedure AddOwner(const AOwner: TComponent);
    procedure DeleteOwner(const AOwner: TComponent); overload;
    procedure DeleteOwner(const Index: Integer); overload;
    property ItemComponentFlags: TInspectorComponentFlags read GetItemComponentFlags write SetItemComponentFlags;
    property KeepFirstOwnerAsFirst: Boolean read GetKeepFirstOwnerAsFirst write SetKeepFirstOwnerAsFirst;
    property NoShowFirstOwnerName: Boolean read GetNoShowFirstOwnerName write SetNoShowFirstOwnerName;
    property OwnerCount: Integer read GetOwnerCount;
    property Owners[I: Integer]: TComponent read GetOwners write SetOwners;
    property ShowOwnerNames: Boolean read GetShowOwnerNames write SetShowOwnerNames;
    property SortComponents: Boolean read GetSortComponents write SetSortComponents;
    property SortOwners: Boolean read GetSortOwners write SetSortOwners;
  end;

  TJvInspectorFontItem = class(TJvInspectorClassItem)
  protected
    procedure Edit; override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  end;

  TJvInspectorFontNameItem = class(TJvInspectorStringItem)
  private
    FUseFont: Boolean;
  protected
    function GetUseFont: Boolean;
    procedure SetUseFont(Value: Boolean);
    procedure DoDrawListItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure DoMeasureListItem(Control: TWinControl; Index: Integer;
      var Height: Integer); override;
    procedure DoMeasureListItemWidth(Control: TWinControl; Index: Integer;
      var Width: Integer); override;
    procedure GetValueList(const Strings: TStrings); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  public
    property UseFont: Boolean read GetUseFont write SetUseFont;
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

  TJvInspectorDateItem = class(TJvInspectorFloatItem)
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetFormat(const Value: string);
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
  published
    property Format: string read FFormat write SetFormat;
  end;

  TJvInspectorTimeItem = class(TJvInspectorFloatItem)
  private
    FShowAMPM: Boolean;
    FShowSeconds: Boolean;
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetFormat;
    procedure SetShowAMPM(Value: Boolean);
    procedure SetShowSeconds(Value: Boolean);
    property Format: string read FFormat;
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
  published
    property ShowAMPM: Boolean read FShowAMPM write SetShowAMPM;
    property ShowSeconds: Boolean read FShowSeconds write SetShowSeconds;
  end;

  TJvInspectorDateTimeItem = class(TJvInspectorCustomCompoundItem)
  private
    FDate: TJvInspectorDateItem;
    FTime: TJvInspectorTimeItem;
  protected
    function GetDateFormat: string;
    function GetTimeShowAMPM: Boolean;
    function GetTimeShowSeconds: Boolean;
    procedure SetDateFormat(const Value: string);
    procedure SetTimeShowAMPM(Value: Boolean);
    procedure SetTimeShowSeconds(Value: Boolean);
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
  published
    property DateFormat: string read GetDateFormat write SetDateFormat;
    property TimeShowAMPM: Boolean read GetTimeShowAMPM write SetTimeShowAMPM;
    property TimeShowSeconds: Boolean read GetTimeShowSeconds write SetTimeShowSeconds;
  end;

  TJvInspectorTStringsItem = class(TJvCustomInspectorItem)
  protected
    procedure ContentsChanged(Sender: TObject);
    function GetDisplayValue: string; override;
    procedure Edit; override;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
  end;

  TJvInspectorTMethodItem = class(TJvCustomInspectorItem)
  private
    FList: TStrings; // list of object instances with list of methods attached.
    FItemTMethodFlags: TInspectorTMethodFlags;
  protected
    function GetInstanceCount: Integer;
    function GetInstances(I: Integer): TObject;
    function GetInstanceNames(I: Integer): string;
    function GetItemTMethodFlags: TInspectorTMethodFlags;
    function GetKeepFirstInstanceAsFirst: Boolean;
    function GetMethodCount(Instance: TObject): Integer;
    function GetMethods(Instance: TObject; I: Integer): TMethod;
    function GetMethodNames(Instance: TObject; I: Integer): string;
    function GetNoShowFirstInstanceName: Boolean;
    function GetShowInstanceNames: Boolean;
    function GetSortMethods: Boolean;
    function GetSortInstances: Boolean;
    procedure SetItemTMethodFlags(Value: TInspectorTMethodFlags);
    procedure SetKeepFirstInstanceAsFirst(Value: Boolean);
    procedure SetNoShowFirstInstanceName(Value: Boolean);
    procedure SetShowInstanceNames(Value: Boolean);
    procedure SetSortMethods(Value: Boolean);
    procedure SetSortInstances(Value: Boolean);
    procedure AddInstancePrim(const Instance: TObject; const InstanceName: string); virtual;
    procedure AddMethodPrim(const Instance: TObject; const MethodAddr: Pointer; const MethodName: string); virtual;
    function MethodFromName(const Name: string): TMethod;
    function MethodFromAbsIndex(const Idx: Integer): TMethod;
    function NameFromMethod(const Method: TMethod): string;
    function AbsIndexFromMethod(const Method: TMethod): Integer;
    function GetDisplayValue: string; override;
    procedure GetValueList(const Strings: TStrings); override;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
    procedure BeforeDestruction; override;
    procedure AddInstance(const Instance: TObject; const InstanceName: string);
    procedure AddMethod(const Method: TMethod; const MethodName: string); overload;
    procedure AddMethod(const Instance: TObject; MethodAddr: Pointer; const MethodName: string); overload;
    procedure DeleteInstance(const Index: Integer); overload;
    procedure DeleteInstance(const Instance: TObject); overload;
    procedure DeleteInstance(const InstanceName: string); overload;
    procedure DeleteMethod(const Method: TMethod); overload;
    procedure DeleteMethod(const InstanceIndex: Integer; const Index: Integer); overload;
    procedure DeleteMethod(const Instance: TObject; const Index: Integer); overload;
    procedure DeleteMethod(const InstanceName: string; const Index: Integer); overload;
    procedure DeleteMethod(const InstanceIndex: Integer; const MethodName: string); overload;
    procedure DeleteMethod(const Instance: TObject; const MethodName: string); overload;
    procedure DeleteMethod(const InstanceName: string; const MethodName: string); overload;
    procedure ClearInstances;
    procedure ClearMethods(const InstanceIndex: Integer); overload;
    procedure ClearMethods(const Instance: TObject); overload;
    procedure ClearMethods(const InstanceName: string); overload;
    function IndexOfInstance(const Instance: TObject): Integer; overload;
    function IndexOfInstance(const InstanceName: string): Integer; overload;
    function IndexOfMethod(const Method: TMethod): Integer; overload;
    function IndexOfMethod(const InstanceIndex: Integer; const MethodName: string): Integer; overload;
    function IndexOfMethod(const Instance: TObject; const MethodName: string): Integer; overload;
    function IndexOfMethod(const InstanceName: string; const MethodName: string): Integer; overload;
    property InstanceCount: Integer read GetInstanceCount;
    property Instances[I: Integer]: TObject read GetInstances;
    property InstanceNames[I: Integer]: string read GetInstanceNames;
    property ItemTMethodFlags: TInspectorTMethodFlags read GetItemTMethodFlags write SetItemTMethodFlags;
    property KeepFirstInstanceAsFirst: Boolean read GetKeepFirstInstanceAsFirst write SetKeepFirstInstanceAsFirst;
    property MethodCount[Instance: TObject]: Integer read GetMethodCount;
    property Methods[Instance: TObject; I: Integer]: TMethod read GetMethods;
    property MethodNames[Instance: TObject; I: Integer]: string read GetMethodNames;
    property NoShowFirstInstanceName: Boolean read GetNoShowFirstInstanceName write SetNoShowFirstInstanceName;
    property ShowInstanceNames: Boolean read GetShowInstanceNames write SetShowInstanceNames;
    property SortInstances: Boolean read GetSortInstances write SetSortInstances;
    property SortMethods: Boolean read GetSortMethods write SetSortMethods;
  end;

  TJvInspectorVariantItem = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
  end;

  TJvCustomInspectorData = class(TPersistent)
  private
    FTypeInfo: PTypeInfo;
    FItems: TJvInspectorItemInstances;
    FName: string;
    FRegistered: Boolean;
    FOnValueChanged: TNotifyEvent;
  protected
    // Remove the const qualifier when compiling with BCB. This is quite
    // similar to the problem aforementioned with events but is more
    // serious as it prevents the program from linking:
    // With the const qualifier, the ATypeInfo parameter gets exported
    // by the linker as a constant pointer to a non constant object
    // (TTypeInfo const *) whereas the HPP generator declares the parameter
    // as a non constant pointer to a constant object (const TTypeInfo *).
    // This leads to the linker not finding the code for the method
    // because the const qualifier is misplaced.
    // The linker is correct in its work because it reflects exactly what
    // the Delphi construct means, but once again the HPP generator is
    // wrong and there is no way to go around this problem but to remove
    // the const qualifier for the parameter in the Delphi source code.
    // The problem arises only when the type of the parameter is a Pointer
    // in Delphi. For instance, a constant parameter of type TForm would
    // be output as 'const TForm*' by both the Linker and HPP generator,
    // thus not triggering any error, even if this doesn't respect the
    // meaning of the Delphi construct which is 'TForm const *'
    constructor CreatePrim(const AName: string; ATypeInfo: PTypeInfo);
    procedure CheckReadAccess; virtual;
    procedure CheckWriteAccess; virtual;
    procedure DoDataChanged;
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
    procedure InvalidateData; virtual;
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
    property OnValueChanged: TNotifyEvent read FOnValueChanged write FOnValueChanged;
    property TypeInfo: PTypeInfo read GetTypeInfo write SetTypeInfo;
  end;

  TJvInspectorSetMemberData = class(TJvCustomInspectorData)
  private
    FBitOffset: Integer;
    FDataParent: TJvCustomInspectorData;
  protected
    function GetAsFloat: Extended; override;
    function GetAsInt64: Int64; override;
    function GetAsMethod: TMethod; override;
    function GetAsOrdinal: Int64; override;
    function GetAsString: string; override;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; override;
    procedure NotifyRemoveData(const Instance: TJvCustomInspectorData); override;
    procedure SetAsFloat(const Value: Extended); override;
    procedure SetAsInt64(const Value: Int64); override;
    procedure SetAsMethod(const Value: TMethod); override;
    procedure SetAsOrdinal(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
  public
    procedure GetAsSet(var Buf); override;
    function HasValue: Boolean; override;
    function IsAssigned: Boolean; override;
    function IsInitialized: Boolean; override;
    class function New(const AParent: TJvCustomInspectorItem; const Ordinal: Integer; const ADataParent:
      TJvCustomInspectorData): TJvCustomInspectorItem; reintroduce; overload;
    procedure SetAsSet(const Buf); override;
    property BitOffset: Integer read FBitOffset;
    property DataParent: TJvCustomInspectorData read FDataParent;
  end;

  TJvInspectorVarData = class(TJvCustomInspectorData)
  private
    FAddress: Pointer;
  protected
    function GetAddress: Pointer; virtual;
    function GetAsFloat: Extended; override;
    function GetAsInt64: Int64; override;
    function GetAsMethod: TMethod; override;
    function GetAsOrdinal: Int64; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; override;
    procedure SetAddress(const Value: Pointer); virtual;
    procedure SetAsFloat(const Value: Extended); override;
    procedure SetAsInt64(const Value: Int64); override;
    procedure SetAsMethod(const Value: TMethod); override;
    procedure SetAsOrdinal(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
    function SupportsMethodPointers: Boolean; override;
  public
    procedure GetAsSet(var Buf); override;
    function HasValue: Boolean; override;
    function IsAssigned: Boolean; override;
    function IsInitialized: Boolean; override;
    class function ItemRegister: TJvInspectorRegister; override;
    class function New(const AParent: TJvCustomInspectorItem; const AName: string; ATypeInfo: PTypeInfo; const
      AAddress: Pointer): TJvCustomInspectorItem; reintroduce; overload;
    // REMOVED BECAUSE OF A BCB INCOMPATIBILITY:
    // Untyped parameters are output as void* which is exactly the same
    // as the output for Pointer, thus leading to the exact same
    // declaration. If you used this version before, simply replace
    // the AVar parameter by @AVar
    //    class function New(const AParent: TJvCustomInspectorItem; const AName: string; const ATypeInfo: PTypeInfo; const AVar): TJvCustomInspectorItem; overload;
    procedure SetAsSet(const Buf); override;
    property Address: Pointer read GetAddress write SetAddress;
  end;

  // Inspector Data Object that Enumerates the Properties of a TPersistent/TComponent/TControl, etc:
  TJvInspectorPropData = class(TJvCustomInspectorData)
  private
    FInstance: TObject;
    FProp: PPropInfo;
  protected
    function GetAsFloat: Extended; override;
    function GetAsInt64: Int64; override;
    function GetAsMethod: TMethod; override;
    function GetAsOrdinal: Int64; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetInstance: TObject; virtual;
    function GetProp: PPropInfo; virtual;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; override;
    procedure NotifyRemoveData(const Instance: TJvCustomInspectorData); override;
    procedure SetAsFloat(const Value: Extended); override;
    procedure SetAsInt64(const Value: Int64); override;
    procedure SetAsMethod(const Value: TMethod); override;
    procedure SetAsOrdinal(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
    procedure SetInstance(const Value: TObject); virtual;
    procedure SetProp(Value: PPropInfo); virtual;
    function SupportsMethodPointers: Boolean; override;
  public
    procedure GetAsSet(var Buf); override;
    function HasValue: Boolean; override;
    function IsAssigned: Boolean; override;
    function IsInitialized: Boolean; override;
    function IsReadOnlyProperty: Boolean; override;
    class function ItemRegister: TJvInspectorRegister; override;
    class function TypeInfoMapRegister: TJvInspectorRegister;
    class procedure AddTypeMapping(Target, Source: PTypeInfo; ObjectClass: TClass = nil;
      const PropertyName: string = '');
    class function New(const AParent: TJvCustomInspectorItem; const AInstance: TObject;
       PropInfo: PPropInfo): TJvCustomInspectorItem; reintroduce; overload;
    class function New(const AParent: TJvCustomInspectorItem; const AInstance: TObject;
      const PropName: string): TJvCustomInspectorItem; reintroduce; overload;
    class function New(const AParent: TJvCustomInspectorItem; const AInstance: TObject;
      const TypeKinds: TTypeKinds = tkProperties): TJvInspectorItemInstances; reintroduce; overload;
    class function NewByNames(const AParent: TJvCustomInspectorItem; const AInstance: TObject;
      const NameList: array of string; const ExcludeList: Boolean = False;
      const TypeKinds: TTypeKinds = tkProperties): TJvInspectorItemInstances;
    class function New(const AParent: TJvCustomInspectorItem; const AInstance: TObject;
       PropInfos: PPropList; const PropCount: Integer): TJvInspectorItemInstances; reintroduce; overload;
    procedure SetAsSet(const Buf); override;
    property Instance: TObject read GetInstance write SetInstance;
    property Prop: PPropInfo read GetProp write SetProp;
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

  // used for inspecting INI and registry file data, validation rules
  // are different than inspecting TComponent properties. -WAP.
  TJvInspectorCustomConfData = class(TJvCustomInspectorData)
  private
    FKey: string;
    FSection: string;
  protected
    constructor CreatePrim(const AName, ASection, AKey: string; ATypeInfo: PTypeInfo);
    function ExistingValue: Boolean; virtual; abstract;
    function GetAsFloat: Extended; override;
    function GetAsInt64: Int64; override;
    function GetAsMethod: TMethod; override;
    function GetAsOrdinal: Int64; override;
    function ForceString: string;
    // NEW: Display something from an INI section that isn't the type it's supposed to be without exceptions and component failures.
    function GetAsString: string; override;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; override;
    procedure SetAsFloat(const Value: Extended); override;
    procedure SetAsInt64(const Value: Int64); override;
    procedure SetAsMethod(const Value: TMethod); override;
    procedure SetAsOrdinal(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetKey(const Value: string);
    procedure SetSection(const Value: string);
    procedure WriteValue(const Value: string); virtual; abstract;
  public
    function ReadValue: string; virtual; abstract; // made public to help fix a bug. WAP.
    procedure GetAsSet(var Buf); override;
    function HasValue: Boolean; override;
    function IsAssigned: Boolean; override;
    function IsInitialized: Boolean; override;
    procedure SetAsSet(const Buf); override;
    property Key: string read FKey write SetKey;
    property Section: string read FSection write SetSection;
  end;

  TJvInspectorINIFileData = class(TJvInspectorCustomConfData)
  private
    FINIFile: TCustomIniFile;
  protected
    function ExistingValue: Boolean; override;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; override;
    procedure WriteValue(const Value: string); override;
  public
    function ReadValue: string; override;
    class function New(const AParent: TJvCustomInspectorItem;
      const AName, ASection, AKey: string; ATypeInfo: PTypeInfo;
      const AINIFile: TCustomIniFile): TJvCustomInspectorItem; reintroduce; overload;
    class function New(const AParent: TJvCustomInspectorItem;
      const ASection: string; const AINIFile: TCustomIniFile;
      const AOnAddKey: TJvInspConfKeyEvent): TJvInspectorItemInstances; reintroduce; overload;
    class function New(const AParent: TJvCustomInspectorItem;
      const AINIFile: TCustomIniFile; const AOnAddSection: TJvInspConfSectionEvent;
      const AOnAddKey: TJvInspConfKeyEvent): TJvInspectorItemInstances; reintroduce; overload;
    property INIFile: TCustomIniFile read FINIFile;
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

  TJvInspectorPropRegItem = class(TJvCustomInspectorRegItem)
  private
    FObjectClass: TClass;
    FName: string;
    FTypeInfo: PTypeInfo;
  public
    constructor Create(const AItemClass: TJvInspectorItemClass; const AObjectClass: TClass;
      const AName: string; ATypeInfo: PTypeInfo);
    function Compare(const ADataObj: TJvCustomInspectorData;
      const Item: TJvCustomInspectorRegItem): Integer; override;
    function MatchValue(const ADataObj: TJvCustomInspectorData): Integer; override;
    function MatchPercent(const ADataObj: TJvCustomInspectorData): Integer; override;
    property Name: string read FName;
    property ObjectClass: TClass read FObjectClass;
    property TypeInfo: PTypeInfo read FTypeInfo;
  end;

  TJvInspectorTypeInfoMapperRegItem = class(TJvCustomInspectorRegItem)
  private
    FObjectClass: TClass;
    FPropertyName: string;
    FPropertyType: PTypeInfo;
    FNewTypeInfo: PTypeInfo;
  public
    constructor Create(AObjectClass: TClass; const APropertyName: string;
      APropertyType: PTypeInfo; ANewTypeInfo: PTypeInfo);
    function Compare(const ADataObj: TJvCustomInspectorData;
      const Item: TJvCustomInspectorRegItem): Integer; override;
    function MatchValue(const ADataObj: TJvCustomInspectorData): Integer; override;
    function MatchPercent(const ADataObj: TJvCustomInspectorData): Integer; override;
    property ObjectClass: TClass read FObjectClass;
    property PropertyName: string read FPropertyName;
    property PropertyType: PTypeInfo read FPropertyType;
    property NewTypeInfo: PTypeInfo read FNewTypeInfo;
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

// The TJvTypeInfoHelper class is provided here to help C++ Builder users
// get type information for base types.
// In Delphi, to get the Type Info for an Integer, we would have done
// TypeInfo(Integer). But with C++ Builder, the TypeInfo function
// doesn't exist. We will then define a macro to do it for us, but with
// the drawback that it will only work with types that have been declared
// in a registered TypeInfo helper class.
// As recommended by the help, we get the value we need by
// calling GetPropInfo on a published property of an existing object.
// But we need a TTypeInfo pointer, so we access the PropType
// member of the PPropInfo returned by GetPropInfo.
// Please see RegisterPropertyEditor in C++ Builder help for
// the example that inspired this bizarre construct.

type
  // The class MUST be a class derived from TPersistent
  // to get the RTTI information
  TJvTypeInfoHelper = class(TPersistent)
  private
    FAnsiCharProp: AnsiChar;
    FAnsiStringProp: AnsiString;
    FBooleanProp: Boolean;
    FByteProp: Byte;
    FByteBoolProp: ByteBool;
    FCardinalProp: Cardinal;
    FCharProp: Char;
    FDoubleProp: Double;
    FExtendedProp: Extended;
    FInt64Prop: Int64;
    FIntegerProp: Integer;
    FLongBoolProp: LongBool;
    FLongintProp: Longint;
    FRealProp: Real;
    FShortintProp: Shortint;
    FSingleProp: Single;
    FSmallintProp: Smallint;
    FTDateTimeProp: TDateTime;
    {$IFDEF UNICODE}
    FUnicodeString: UnicodeString;
    {$ENDIF}
    FWideCharProp: WideChar;
    FWordProp: Word;
    FWordBoolProp: WordBool;
  published
    // These are the base Delphi types
    property AnsiCharProp: AnsiChar read FAnsiCharProp;
    property AnsiStringProp: AnsiString read FAnsiStringProp;
    property BooleanProp: Boolean read FBooleanProp;
    property ByteProp: Byte read FByteProp;
    property ByteBoolProp: ByteBool read FByteBoolProp;
    property CardinalProp: Cardinal read FCardinalProp;
    property CharProp: Char read FCharProp;
    property DoubleProp: Double read FDoubleProp;
    property ExtendedProp: Extended read FExtendedProp;
    property Int64Prop: Int64 read FInt64Prop;
    property IntegerProp: Integer read FIntegerProp;
    property LongBoolProp: LongBool read FLongBoolProp;
    property LongintProp: Longint read FLongintProp;
    property RealProp: Real read FRealProp;
    property ShortintProp: Shortint read FShortintProp;
    property SingleProp: Single read FSingleProp;
    property SmallintProp: Smallint read FSmallintProp;
    property TDateTimeProp: TDateTime read FTDateTimeProp;
    {$IFDEF UNICODE}
    property UnicodeStringProp: UnicodeString read FUnicodeString;
    {$ENDIF}
    property WideCharProp: WideChar read FWideCharProp;
    property WordProp: Word read FWordProp;
    property WordBoolProp: WordBool read FWordBoolProp;
    // These are the C++ Builder types that don't exist in Delphi
    // Some C++ types are different from Delphi types only by case
    // and are not represented here
    property __int64Prop: Int64 read FInt64Prop;
    property boolProp: Boolean read FBooleanProp;
    property floatProp: Single read FSingleProp;
    property intProp: Integer read FIntegerProp;
    property longProp: Integer read FIntegerProp;
    property long_doubleProp: Extended read FExtendedProp;
    property shortProp: Smallint read FSmallintProp;
    property signed_charProp: Shortint read FShortintProp;
    property signed_intProp: Integer read FIntegerProp;
    property signed_longProp: Integer read FIntegerProp;
    property signed_shortProp: Smallint read FSmallintProp;
    property unsigned_charProp: Byte read FByteProp;
    property unsigned_intProp: Cardinal read FCardinalProp;
    property unsigned_longProp: Cardinal read FCardinalProp;
    property unsigned_shortProp: Byte read FByteProp;
  end;
  TJvTypeInfoHelperClass = class of TJvTypeInfoHelper;

// This function returns the type info associated with the given type name
// It will go through the collection of known TypeInfoHelpers and try
// to find one that contains a property named TypeName+'Prop'
// The first one it finds will be used to return the PTypeInfo pointer
function TypeInfoFromName(TypeName: string): PTypeInfo;

// Register the given class as a TypeInfo helper
procedure RegisterTypeInfoHelper(AClass: TJvTypeInfoHelperClass);

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

// This macro is a shortcut for all base types. If you use it for any
// other type, the compilation will work, but an access violation will
// occur at runtime because a property of your type couldn't be found
// in the TJvTypeInfoHelper class declared above.
// You should declare a class with a published property of your type
// and use the TypeInfoFromClass macro.
{$HPPEMIT '#define TypeInfo(type) TypeInfoFromName(STR(type))'}
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

// BCB Type Info support
var
  GlobalTypeInfoHelpersList: TClassList;

function CreatePainterFromStyle(Style: TJvInspectorStyle): TJvInspectorPainter;
begin
  case Style of
    isDotNet:
      Result := TJvInspectorDotNETPainter.Create(nil);
  else
    Result := TJvInspectorBorlandPainter.Create(nil);
  end;
end;

//============================================================================

function TypeInfoHelpersList: TClassList;
begin
  if not Assigned(GlobalTypeInfoHelpersList) then
  begin
    GlobalTypeInfoHelpersList := TClassList.Create;
    // register
    RegisterTypeInfoHelper(TJvTypeInfoHelper);
  end;
  Result := GlobalTypeInfoHelpersList;
end;

function TypeInfoFromName(TypeName: string): PTypeInfo;
var
  I: Integer;
  PropInfo: PPropInfo;
begin
  // replace spaces by underscores
  StrReplace(TypeName, ' ', '_', [rfReplaceAll]);

  I := 0;
  PropInfo := nil;

  while (I < TypeInfoHelpersList.Count) and (PropInfo = nil) do
  begin
    PropInfo := GetPropInfo(TypeInfoHelpersList[I], TypeName + 'Prop');
    Inc(I);
  end;

  if PropInfo <> nil then
    Result := PropInfo.PropType^
  else
    Result := nil;
end;

procedure RegisterTypeInfoHelper(AClass: TJvTypeInfoHelperClass);
begin
  TypeInfoHelpersList.Add(AClass);
end;

type
  PMethod = ^TMethod;
  PComp = ^Comp;
  PPointer = ^Pointer;
  TCustomEditAccessProtected = class(TCustomEdit);

var
  GlobalGenItemReg: TJvInspectorRegister = nil;
  GlobalVarItemReg: TJvInspectorRegister = nil;
  GlobalPropItemReg: TJvInspectorRegister = nil;
  GlobalPropMapReg: TJvInspectorRegister = nil;

procedure RegisterDataTypeKinds; forward;
procedure RegisterPropDataTypeKinds; forward;

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

  FBandStartsNoSB := TList.Create;
  FBandStartsSB := TList.Create;
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
  BandWidth := 150;
  AutoComplete := True;
  AutoDropDown := False;

  // An easy and 'dirty' way to force Style to take into account its value
  // and have the setter do its job
  FStyle := isItemPainter;
  Style := isBorland;

  if not (csDesigning in ComponentState) then
    GlobalInspReg.RegInspector(Self);

  // Mantis 1717: Inspecting self at design time to show effects of painter.
  if (csDesigning in ComponentState) then
    AddComponent(Self, 'Test category for Inspector', True);
end;

function TJvCustomInspector.CalcImageHeight: Integer;
var
  BandHeightNoSB: Integer;
  BandHeightSB: Integer;
  ClHeightNoSB: Integer;
  ClHeightSB: Integer;
  WinStyle: Longint;
  I: Integer;
begin
  BandHeightNoSB := 0;
  BandHeightSB := 0;
  FImageHeight := 0;
  FBandStartsNoSB.Clear;
  FBandStartsNoSB.Add(Pointer(0));
  FBandStartsSB.Clear;
  FBandStartsSB.Add(Pointer(0));
  ClHeightNoSB := ClientHeight;
  WinStyle := GetWindowLong(Handle, GWL_STYLE);
  if (WinStyle and WS_HSCROLL) <> 0 then
  begin
    ClHeightSB := ClHeightNoSB;
    Inc(ClHeightNoSB, GetSystemMetrics(SM_CYHSCROLL));
  end
  else
  begin
    ClHeightSB := ClHeightNoSB;
    Dec(ClHeightSB, GetSystemMetrics(SM_CYHSCROLL));
  end;
  for I := 0 to Pred(VisibleCount) do
  begin
    Inc(FImageHeight, VisibleItems[I].Height);
    if UseBands then
    begin
      if ((BandHeightSB + VisibleItems[I].Height) > ClHeightSB) and (BandHeightSB > 0) then
      begin
        FBandStartsSB.Add(Pointer(I));
        BandHeightSB := 0;
      end;
      if ((BandHeightNoSB + VisibleItems[I].Height) > ClHeightNoSB) and (BandHeightNoSB > 0) then
      begin
        FBandStartsNoSB.Add(Pointer(I));
        BandHeightNoSB := 0;
      end;
    end;
    Inc(BandHeightNoSB, VisibleItems[I].Height);
    Inc(BandHeightSB, VisibleItems[I].Height);
  end;
  Result := FImageHeight;
end;

function TJvCustomInspector.CalcItemIndex(X, Y: Integer; var Rect: TRect): Integer;
var
  BandIdx: Integer;
  MaxIdx: Integer;
begin
  if UseBands then
  begin
    BandIdx := X div BandWidth + BandStarts.IndexOf(Pointer(TopIndex));
    if BandIdx < BandStarts.Count then
      Result := Integer(BandStarts[BandIdx])
    else
      Result := -1;
  end
  else
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
    if not RelativeDivider then
      FDivider := MulDiv(FDivider, M, D);
    FBandWidth := MulDiv(FBandWidth, M, D);
    // The painters' fonts are sized for the dpi the inspector had before
    // this change (they start out at the process's startup dpi through
    // DefFontData)
    if FPainter <> nil then
      FPainter.ScaleFonts(M, D);
    if FStylePainter <> nil then
      FStylePainter.ScaleFonts(M, D);
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

procedure TJvCustomInspector.DoAfterDataCreate(const Data: TJvCustomInspectorData);
begin
  if Assigned(FAfterDataCreate) then
    FAfterDataCreate(Self, Data);
end;

procedure TJvCustomInspector.DoAfterItemCreate(const Item: TJvCustomInspectorItem);
begin
  if Assigned(FAfterItemCreate) then
    FAfterItemCreate(Self, Item);
end;

procedure TJvCustomInspector.DoBeforeItemCreate(const Data: TJvCustomInspectorData;
  var ItemClass: TJvInspectorItemClass);
begin
  if Assigned(FBeforeItemCreate) then
    FBeforeItemCreate(Self, Data, ItemClass);
end;

function TJvCustomInspector.DoBeforeItemSelect(const NewItem: TJvCustomInspectorItem): Boolean;
begin
  Result := True;
  if Assigned(FBeforeSelection) then
    FBeforeSelection(Self, NewItem, Result);
end;

procedure TJvCustomInspector.DoDataValueChanged(const Data: TJvCustomInspectorData);
begin
  if Assigned(FOnDataValueChanged) then
    FOnDataValueChanged(Self, Data);
end;

procedure TJvCustomInspector.DoItemSelected;
begin
  if Assigned(FOnItemSelected) then
    FOnItemSelected(Self);
end;

procedure TJvCustomInspector.DoItemValueChanged(const Item: TJvCustomInspectorItem);
begin
  if Assigned(FOnItemValueChanged) then
    FOnItemValueChanged(Self, Item);
end;

function TJvCustomInspector.DoItemValueChanging(const Item: TJvCustomInspectorItem; var NewValue: string): Boolean;
begin
  Result := True;
  if Assigned(FOnItemValueChanging) then
    FOnItemValueChanging(Self, Item, NewValue, Result);
end;

function TJvCustomInspector.DoItemValueError(Item: TJvCustomInspectorItem): Boolean;
var
  E: Exception;
begin
  Result := True;
  E := ExceptObject as Exception;
  if Assigned(FOnItemValueError) then
    OnItemValueError(Self, Item, E)
  else
    Result := False;
end;

function TJvCustomInspector.GetActivePainter: TJvInspectorPainter;
begin
  if Style = isItemPainter then
    Result := Painter
  else
    Result := FStylePainter;
end;

function TJvCustomInspector.GetAfterDataCreate: TInspectorDataEvent;
begin
  Result := FAfterDataCreate;
end;

function TJvCustomInspector.GetAfterItemCreate: TInspectorItemEvent;
begin
  Result := FAfterItemCreate;
end;

function TJvCustomInspector.GetBandFor(const ItemIdx: Integer): Integer;
begin
  Result := Pred(BandStarts.Count);
  while (Result > -1) and (Integer(BandStarts[Result]) > ItemIdx) do
    Dec(Result);
end;

function TJvCustomInspector.GetBandStarts: TList;
begin
  if FBandStartsNoSB.Count > (ClientWidth div BandWidth) then
    Result := FBandStartsSB
  else
    Result := FBandStartsNoSB;
end;

function TJvCustomInspector.GetBandWidth: Integer;
begin
  Result := FBandWidth;
end;

function TJvCustomInspector.GetBeforeItemCreate: TInspectorItemBeforeCreateEvent;
begin
  Result := FBeforeItemCreate;
end;

function TJvCustomInspector.GetBeforeSelection: TInspectorItemBeforeSelectEvent;
begin
  Result := FBeforeSelection;
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

function TJvCustomInspector.GetDividerAbs: Integer;
begin
  if RelativeDivider then
  begin
    if UseBands then
      Result := (FDivider * BandWidth) div 100
    else
    if HandleAllocated then
      Result := (FDivider * ClientWidth) div 100
    else
      Result := (FDivider * Width) div 100;
  end
  else
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

function TJvCustomInspector.GetRelativeDivider: Boolean;
begin
  Result := FRelativeDivider;
end;

function TJvCustomInspector.GetRoot: TJvCustomInspectorItem;
begin
  Result := FRoot;
end;

function TJvCustomInspector.GetOnItemSelected: TNotifyEvent;
begin
  Result := FOnItemSelected;
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

function TJvCustomInspector.GetUseBands: Boolean;
begin
  Result := FUseBands;
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

function TJvCustomInspector.GetWantTabs: Boolean;
begin
  Result := FWantTabs;
end;

procedure TJvCustomInspector.HandleBandResize(X: Integer);
var
  BSize: Integer;
begin
  BSize := X div Succ(BandSizingBand);
  if BSize < 100 then
    BSize := 100;
  BandWidth := BSize;
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
  if not BandSizing then
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
        begin
          if Item is TJvInspectorCustomCompoundItem then
            with Item as TJvInspectorCustomCompoundItem do
            begin
              if SelectedColumnIndex > 0 then
                SelectedColumnIndex := SelectedColumnIndex - 1
              else
              if SelectedIndex > 0 then
                SelectedIndex := SelectedIndex - 1;
            end
          else
          if SelectedIndex > 0 then
            SelectedIndex := SelectedIndex - 1;
          if Item <> Selected then
          begin
            if Selected is TJvInspectorCustomCompoundItem then
              TJvInspectorCustomCompoundItem(Selected).SelectedColumnIndex :=
                TJvInspectorCustomCompoundItem(Selected).ColumnCount - 1;
          end;
        end;
      VK_RIGHT:
        begin
          if Item is TJvInspectorCustomCompoundItem then
            with Item as TJvInspectorCustomCompoundItem do
            begin
              if SelectedColumnIndex < Pred(ColumnCount) then
                SelectedColumnIndex := SelectedColumnIndex + 1
              else
              if SelectedIndex < Pred(VisibleCount) then
                SelectedIndex := SelectedIndex + 1;
            end
          else
          if SelectedIndex < Pred(VisibleCount) then
            SelectedIndex := SelectedIndex + 1;
          if Item <> Selected then
          begin
            if Selected is TJvInspectorCustomCompoundItem then
              TJvInspectorCustomCompoundItem(Selected).SelectedColumnIndex := 0;
          end;
        end;
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
      VK_TAB:
        if WantTabs then
        begin
          if Item is TJvInspectorCustomCompoundItem then
            with Item as TJvInspectorCustomCompoundItem do
            begin
              if SelectedColumnIndex < Pred(ColumnCount) then
                SelectedColumnIndex := SelectedColumnIndex + 1
              else
              if SelectedIndex < Pred(VisibleCount) then
                SelectedIndex := SelectedIndex + 1;
            end
          else
          if SelectedIndex < Pred(VisibleCount) then
            SelectedIndex := SelectedIndex + 1;
          if Item <> Selected then
          begin
            if Selected is TJvInspectorCustomCompoundItem then
              TJvInspectorCustomCompoundItem(Selected).SelectedColumnIndex := 0;
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
  if Shift = [ssShift] then
  begin
    IgnoreKey := True;
    case Key of
      VK_TAB:
        if WantTabs then
        begin
          if Item is TJvInspectorCustomCompoundItem then
            with Item as TJvInspectorCustomCompoundItem do
            begin
              if SelectedColumnIndex > 0 then
                SelectedColumnIndex := SelectedColumnIndex - 1
              else
              if SelectedIndex > 0 then
                SelectedIndex := SelectedIndex - 1;
            end
          else
          if SelectedIndex > 0 then
            SelectedIndex := SelectedIndex - 1;
          if Item <> Selected then
          begin
            if Selected is TJvInspectorCustomCompoundItem then
              TJvInspectorCustomCompoundItem(Selected).SelectedColumnIndex :=
                TJvInspectorCustomCompoundItem(Selected).ColumnCount - 1;
          end;
        end;
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
  if ((Shift = []) and ((Key = VK_DOWN) or (Key = VK_UP) or (Key = VK_ADD) or
    (Key = VK_SUBTRACT) or (Key = VK_PRIOR) or (Key = VK_NEXT))) or
    ((Key = VK_TAB) and WantTabs) then
    Key := 0;
end;

procedure TJvCustomInspector.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  BWidth: Integer;
  BandIdx: Integer;
  XB: Integer;
  ItemIndex: Integer;
  ItemRect: TRect;
  Item: TJvCustomInspectorItem;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if UseBands then
  begin
    BWidth := BandWidth;
    BandIdx := X div BWidth + BandStarts.IndexOf(Pointer(TopIndex));
  end
  else
  begin
    BWidth := ClientWidth;
    BandIdx := -1;
  end;
  XB := X mod BWidth;
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
    if (XB >= Pred(DividerAbs)) and (XB <= Succ(DividerAbs)) then
    begin
      DraggingDivider := True;
      DividerDragBandX := BandIdx * BWidth;
    end
    // Check row sizing
    else
    if (Item <> nil) and (Y >= Pred(ItemRect.Bottom)) and
      (Y <= Succ(ItemRect.Bottom)) and (Item.RowSizing.SizingFactor <> irsNoReSize) and
      Item.RowSizing.Sizable then
    begin
      RowSizing := True;
      RowSizingItem := Item;
    end
    // Check band sizing
    else
    if (UseBands and (XB >= BWidth - 3)) and (not UseBands or
      (BandIdx < BandStarts.Count)) then
    begin
      BandSizing := True;
      BandSizingBand := BandIdx - BandStarts.IndexOf(Pointer(TopIndex));
    end
    // Check selecting
    else
    if (Item <> nil) and (ItemIndex <> SelectedIndex) then
    begin
      SelectedIndex := ItemIndex;
      if ItemIndex >= 0 then
        Item := VisibleItems[ItemIndex];
    end;
    if not DraggingDivider and not RowSizing and not BandSizing then
      Selecting := True;
  end;
  if Button in [mbLeft, mbRight] then
  begin
    if (Item <> nil) and
      ((Item.HasViewableItems and not (iifExpanded in Item.Flags)) or
      (iifExpanded in Item.Flags)) then
    begin
      if PtInRect(Item.Rects[iprBtnDstRect], Point(X, Y)) or
        ((ssDouble in Shift) and (Item.IsCategory or (XB < Pred(DividerAbs)))) then
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
  end;

  if Assigned(Item) and Assigned(FOnEditorMouseDown) then
    FOnEditorMouseDown(Self, Item, Button, Shift, X, Y);
end;

procedure TJvCustomInspector.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  BWidth: Integer;
  BandIdx: Integer;
  XB: Integer;
  ItemIndex: Integer;
  ItemRect: TRect;
  Item: TJvCustomInspectorItem;
begin
  inherited MouseMove(Shift, X, Y);
  if UseBands then
  begin
    BWidth := BandWidth;
    BandIdx := X div BWidth + BandStarts.IndexOf(Pointer(TopIndex));
  end
  else
  begin
    BWidth := ClientWidth;
    BandIdx := -1;
  end;
  if UseBands and not DraggingDivider then
    XB := X mod BWidth
  else
  if UseBands and DraggingDivider then
    XB := X - DividerDragBandX
  else
    XB := X;
  if DraggingDivider then
    DividerAbs := XB
  else
  if BandSizing then
    HandleBandResize(X)
  else
  if (((XB >= Pred(DividerAbs)) and (XB <= Succ(DividerAbs))) or
    (UseBands and (XB >= BWidth - 3))) and (not UseBands or
    (BandIdx < BandStarts.Count)) then
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
    if BandSizing then
    begin
      BandSizing := False;
      TopIndex := TopIndex; // resync position
    end
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
    if AComponent = FStylePainter then
      FStylePainter := nil;
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


  if ActivePainter <> nil then
  begin
    if NeedRebuild then
      InvalidateList;
    IncPaintGeneration;
    ActivePainter.Setup(Canvas);
    ActivePainter.Paint;
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(PaintRect);
    if csDesigning in Self.ComponentState then
      Canvas.TextOut(10, 10, Name + ':' + ClassName);
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
  if not BandSizing then
  begin
    FImageHeight := 0; // Force recalculation of bands
    if (ImageHeight <= ClientHeight) and UseBands then
      TopIndex := 0
    else
      TopIndex := TopIndex;
  end;
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

procedure TJvCustomInspector.SetAfterDataCreate(const Value: TInspectorDataEvent);
begin
  FAfterDataCreate := Value;
end;

procedure TJvCustomInspector.SetAfterItemCreate(const Value: TInspectorItemEvent);
begin
  FAfterItemCreate := Value;
end;

procedure TJvCustomInspector.SetBandWidth(Value: Integer);
begin
  if Value <> BandWidth then
  begin
    FBandWidth := Value;
    if not RelativeDivider then
      DividerAbs := DividerAbs;
    if HandleAllocated then
    begin
      CalcImageHeight;
      UpdateScrollBars;
    end;
  end;
end;

procedure TJvCustomInspector.SetBeforeItemCreate(const Value: TInspectorItemBeforeCreateEvent);
begin
  FBeforeItemCreate := Value;
end;

procedure TJvCustomInspector.SetBeforeSelection(const Value: TInspectorItemBeforeSelectEvent);
begin
  FBeforeSelection := Value;
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
begin
  if FDivider <> Value then
    if RelativeDivider then
    begin
      if UseBands then
        DividerAbs := (Value * BandWidth) div 100
      else
      if HandleAllocated then
        DividerAbs := (Value * ClientWidth) div 100
      else
        DividerAbs := (Value * Width) div 100;
    end
    else
      DividerAbs := Value;
end;

procedure TJvCustomInspector.SetDividerAbs(Value: Integer);
var
  W: Integer;
begin
  if UseBands then
    W := BandWidth
  else
  if HandleAllocated then
    W := ClientWidth
  else
    W := Width;
  if Value > (W - 2 * ItemHeight) then
    Value := W - 2 * ItemHeight;
  if Value < (2 * ItemHeight) then
    Value := 2 * ItemHeight;
  if RelativeDivider then
  begin
    if UseBands then
      FDivider := (Value * 100) div BandWidth
    else
    if HandleAllocated then
      FDivider := (Value * 100) div ClientWidth
    else
      FDivider := (Value * 100) div Width;
  end
  else
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

procedure TJvCustomInspector.SetOnItemSelected(const Value: TNotifyEvent);
begin
  FOnItemSelected := Value;
end;

procedure TJvCustomInspector.SetPainter(const Value: TJvInspectorPainter);
begin
  if Value <> Painter then
  begin
    if Value <> nil then
      if (Value.Inspector <> nil) and (Value.Inspector <> Self) then
        raise EJvInspector.CreateRes(@RsEJvInspPaintOnlyUsedOnce);

    if Painter <> nil then
      Painter.SetInspector(nil);

    ReplaceComponentReference(Self, Value, TComponent(FPainter));

    if Painter <> nil then
    begin
      Style := isItemPainter;
      Painter.SetInspector(Self);

      if HandleAllocated then
        UpdateScrollBars;
    end
    else
    begin
      if not FSettingStyle then
        Style := isBorland;
    end;
  end;
end;

procedure TJvCustomInspector.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TJvCustomInspector.SetRelativeDivider(Value: Boolean);
var
  OrgPos: Integer;
begin
  if Value <> RelativeDivider then
  begin
    OrgPos := DividerAbs;
    FRelativeDivider := Value;
    DividerAbs := OrgPos;
  end;
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
var
  NewItem: TJvCustomInspectorItem;
begin
  if Value >= VisibleCount then
    Value := Pred(VisibleCount);
  if Value < -1 then
    Value := -1;
  if Value <> SelectedIndex then
  begin
    if Value > -1 then
      NewItem := VisibleItems[Value]
    else
      NewItem := nil;

    if not (csDestroying in ComponentState) then
      // bugfix WAP.  Why repaint the screen when the component is going away anyway.
      if DoBeforeItemSelect(NewItem) then
      begin
        if Selected <> nil then
          Selected.DoneEdit(False);
        FSelectedIndex := Value;
        if Selected <> nil then
        begin
          Selected.ScrollInView;
          Selected.InitEdit;
        end;
        DoItemSelected;
        InvalidateItem;
      end;
  end;
end;

procedure TJvCustomInspector.SetStyle(const Value: TJvInspectorStyle);
begin
  if FStyle <> Value then
  begin
    FSettingStyle := True;
    try
      // Prevent changing the style if getting isItemPainter without a Painter
      // (Mantis 3847)
      if (Value <> isItemPainter) or (Painter <> nil) then
        FStyle := Value;

      // Always remove the current painter
      if FStylePainter <> nil then
      begin
        FStylePainter.SetInspector(nil);
        FStylePainter.Free;
        FStylePainter := nil;
      end;

      if (Style <> isItemPainter) or (Painter = nil) then
      begin
        Painter := nil;

        FStylePainter := CreatePainterFromStyle(Value);
        FStylePainter.SetInspector(Self);

        if HandleAllocated then
          UpdateScrollBars;
      end;
    finally
      FSettingStyle := False;
    end;
  end;
end;

procedure TJvCustomInspector.SetTopIndex(Value: Integer);
var
  MaxIdx: Integer;
begin
  if UseBands then
  begin
    MaxIdx := BandStarts.Count - (ClientWidth div BandWidth);
    if MaxIdx < 0 then
      MaxIdx := 0;
    if MaxIdx >= BandStarts.Count then
      MaxIdx := BandStarts.Count - 1;
    if MaxIdx <> -1 then
      MaxIdx := Integer(BandStarts[MaxIdx]);
  end
  else
    MaxIdx := Succ(YToIdx(ImageHeight - ClientHeight));
  if MaxIdx < 0 then
    MaxIdx := 0;
  if Value > MaxIdx then
    Value := MaxIdx;
  if Value < 0 then
    Value := 0;
  if UseBands and (BandStarts.IndexOf(Pointer(Value)) > -1) then
  begin
    MaxIdx := Pred(BandStarts.Count);
    while (MaxIdx > -1) and (Integer(BandStarts[MaxIdx]) > Value) do
      Dec(MaxIdx);
    if MaxIdx <= -1 then
      raise EJvInspector.CreateRes(@RsEJvAssertSetTopIndex);
    Value := Integer(BandStarts[MaxIdx]);
  end;
  if TopIndex <> Value then
  begin
    FTopIndex := Value;
    if HandleAllocated then
      UpdateScrollBars;
  end;
end;

procedure TJvCustomInspector.SetUseBands(Value: Boolean);
begin
  if UseBands <> Value then
  begin
    FUseBands := Value;
    if not RelativeDivider then
      DividerAbs := DividerAbs;
    FImageHeight := 0;
    if HandleAllocated then
      UpdateScrollBars;
  end;
end;

procedure TJvCustomInspector.SetWantTabs(Value: Boolean);
begin
  if Value <> WantTabs then
  begin
    FWantTabs := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomInspector.UpdateScrollBars;
var
  DrawHeight: Integer;
  ClHeight: Integer;
  ScFactor: Extended;
  ScrollInfo: TScrollInfo;
  BCount: Integer;
  BPerPage: Integer;
  ShowVertSB: Boolean;
  ShowHorzSB: Boolean;
begin
  if csDestroying in ComponentState then
    Exit;

  if not HandleAllocated then
    Exit;

  if not UseBands then
  begin
    ShowScrollBars(SB_HORZ, False);
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
  end
  else
  begin
    ShowScrollBars(SB_VERT, False);
    { Needed to redisplay the scrollbar after it's hidden in the CloseUp method
      of an enumerated item's combobox }
    BCount := BandStarts.Count;
    BPerPage := ClientWidth div BandWidth;
    ShowHorzSB := BCount > BPerPage;
    if ShowHorzSB then
    begin
      with ScrollInfo do
      begin
        cbSize := SizeOf(ScrollInfo);
        fMask := SIF_ALL;
        nMin := 0;
        nMax := BCount - 1;
        nPage := BPerPage;
        nPos := GetBandFor(TopIndex);
        nTrackPos := 0;
      end;
      SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
    end;
    ShowScrollBars(SB_HORZ, ShowHorzSB);
  end;
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
  if WantTabs then
    Include(Code, dcWantTab);
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

procedure TJvCustomInspector.WMHScroll(var Msg: TWMScroll);
var
  CurBand: Integer;
  Delta: Integer;
begin
  CurBand := BandStarts.IndexOf(Pointer(TopIndex));
  case Msg.ScrollCode of
    SB_BOTTOM:
      Delta := BandStarts.Count - 1 - CurBand;
    SB_ENDSCROLL:
      Delta := 0;
    SB_LINEDOWN:
      Delta := 1;
    SB_LINEUP:
      Delta := -1;
    SB_PAGEDOWN:
      Delta := ClientWidth div BandWidth;
    SB_PAGEUP:
      Delta := -ClientWidth div BandWidth;
    SB_THUMBPOSITION:
      Delta := Msg.Pos - CurBand;
    SB_THUMBTRACK:
      Delta := Msg.Pos - CurBand;
    SB_TOP:
      Delta := -CurBand;
  else
    Delta := 0;
  end;
  CurBand := CurBand + Delta;
  if CurBand < 0 then
    CurBand := 0;
  if CurBand >= BandStarts.Count then
    CurBand := BandStarts.Count - 1;
  TopIndex := Integer(BandStarts[CurBand]);
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
  if not (csDesigning in ComponentState) then
    GlobalInspReg.UnRegInspector(Self);
  Painter := nil;
  FRoot.Free;
  FBandStartsSB.Free;
  FBandStartsNoSB.Free;
  FSortNotificationList.Free;
  FVisibleList.Free;
  FExpandButton.Free;
  FCollapseButton.Free;
  FStylePainter.Free;
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
  if (Result <> nil) and (Result is TJvInspectorCustomCompoundItem) then
  begin
    with (Result as TJvInspectorCustomCompoundItem) do
      if SelectedColumn <> nil then
        Result := SelectedColumn.Item;
  end;
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

procedure TJvCustomInspector.SetInspectObject(const Value: TObject);
begin
  Root.Clear;
  if Value <> nil then
    TJvInspectorPropData.New(Root, Value);
  FInspectObject := Value;
end;

procedure TJvCustomInspector.AddComponent(Instance: TObject; const CategoryName: string;
  Expanded: Boolean);
var
  InspCat: TJvCustomInspectorItem;
begin
  BeginUpdate;
  if Instance <> nil then
  begin
    if CategoryName <> '' then
    begin
      InspCat := TJvInspectorCustomCategoryItem.Create(Self.Root, nil);
      InspCat.DisplayName := CategoryName;
    end
    else
      InspCat := Root;
    TJvInspectorPropData.New(InspCat, Instance);
    if InspCat <> Root then
      InspCat.Expanded := Expanded;
  end;
  EndUpdate;
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

  FCategoryFont := TFont.Create;
  FCategoryFont.OnChange := FontChange;
  FNameFont := TFont.Create;
  FNameFont.OnChange := FontChange;
  FValueFont := TFont.Create;
  FValueFont.OnChange := FontChange;
  FSelectedFont := TFont.Create;
  FSelectedFont.OnChange := FontChange;

  Initializing := True;
  try
    InitializeColors;
  finally
    Initializing := False;
  end;
  // FInternalCollapseButton and FInternalExpandButton are rendered on demand
  // by PrepareInternalImages
end;

procedure TJvInspectorPainter.DefineProperties(Filer: TFiler);
begin
  // Here to allow transparent reading of old DFMs following changes
  // introduced for Mantis 1715
  Filer.DefineProperty('CategoryTextColor', ReadCategoryTextColor, nil, False);
  Filer.DefineProperty('NameColor', ReadNameColor, nil, False);
  Filer.DefineProperty('ValueColor', ReadValueColor, nil, False);
  Filer.DefineProperty('SelectedTextColor', ReadSelectedTextColor, nil, False);
  Filer.DefineProperty('HideSelectTextColor', ReadHideSelectTextColor, nil, False);

  inherited DefineProperties(Filer);
end;

destructor TJvInspectorPainter.Destroy;
begin
  FInternalCollapseButton.Free;
  FInternalExpandButton.Free;
  FCategoryFont.Free;
  FNameFont.Free;
  FValueFont.Free;
  FSelectedFont.Free;
  inherited Destroy;
end;

procedure TJvInspectorPainter.ApplyNameFont;
begin
  if Assigned(Item) and Item.IsCategory then
    Canvas.Font := CategoryFont
  else
    Canvas.Font := NameFont;
end;

procedure TJvInspectorPainter.ApplyValueFont;
begin
  Canvas.Font := ValueFont;
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

procedure TJvInspectorPainter.FontChange(Sender: TObject);
begin
  if not Initializing and not Loading and Assigned(Inspector) then
    Inspector.Invalidate;
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

function TJvInspectorPainter.GetCategoryFont: TFont;
begin
  Result := FCategoryFont;
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
    Image.Canvas.Pen.Color := NameFont.Color;
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
    (NameFont.Color = FInternalButtonPenColor) then
    Exit;
  FInternalButtonSize := Size;
  FInternalButtonBackgroundColor := BackgroundColor;
  FInternalButtonPenColor := NameFont.Color;
  PrepareImage(FInternalCollapseButton, False);
  PrepareImage(FInternalExpandButton, True);
end;

procedure TJvInspectorPainter.ScaleFonts(const M, D: Integer);
begin
  // Called when the inspector's dpi changes: the fonts scale the same way
  // TControl.ChangeScale scales a control's font
  FCategoryFont.Height := MulDiv(FCategoryFont.Height, M, D);
  FNameFont.Height := MulDiv(FNameFont.Height, M, D);
  FValueFont.Height := MulDiv(FValueFont.Height, M, D);
  FSelectedFont.Height := MulDiv(FSelectedFont.Height, M, D);
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

function TJvInspectorPainter.GetHideSelectFont: TFont;
begin
  Result := SelectedFont;
end;

function TJvInspectorPainter.GetNameFont: TFont;
begin
  Result := FNameFont;
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

function TJvInspectorPainter.GetSelectedFont: TFont;
begin
  Result := FSelectedFont;
end;

function TJvInspectorPainter.GetDrawNameEndEllipsis: Boolean;
begin
  Result := FDrawNameEndEllipsis;
end;

function TJvInspectorPainter.GetValueFont: TFont;
begin
  Result := FValueFont;
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
  BandIdx: Integer;
  MaxBandItemIdx: Integer;
begin
  SelItemVisible := False;
  Rect := Inspector.ViewRect;
  Canvas.FillRect(Rect);
  ItemIdx := Inspector.TopIndex;
  MaxItemIdx := Inspector.VisibleCount;
  if not Inspector.UseBands then
  begin
    // Loop through the visible list
    while (Rect.Top < Rect.Bottom) and (ItemIdx < MaxItemIdx) do
    begin
      SelItemVisible := SelItemVisible or (ItemIdx = Inspector.SelectedIndex);
      PaintItem(Rect, ItemIdx);
      Inc(ItemIdx);
    end;
  end
  else // if UseBands
  begin
    BandIdx := Inspector.BandStarts.IndexOf(Pointer(ItemIdx));
    Rect.Right := Rect.Left + Inspector.BandWidth - 4;
    while (ItemIdx < MaxItemIdx) and (Rect.Left < Inspector.ClientWidth) do
    begin
      Inc(BandIdx);
      if BandIdx < Inspector.BandStarts.Count then
        MaxBandItemIdx := Integer(Inspector.BandStarts[BandIdx])
      else
        MaxBandItemIdx := MaxItemIdx;
      while (Rect.Top < Rect.Bottom) and (ItemIdx < MaxBandItemIdx) do
      begin
        SelItemVisible := SelItemVisible or (ItemIdx = Inspector.SelectedIndex);
        PaintItem(Rect, ItemIdx);
        Inc(ItemIdx);
      end;
      MaxBandItemIdx := Rect.Right + 4;
      Rect := Inspector.ClientRect;
      Rect.Left := MaxBandItemIdx;
      Rect.Right := Rect.Left + Inspector.BandWidth - 4;
      Canvas.Pen.Color := clBtnShadow;
      Canvas.MoveTo(Rect.Left - 3, Rect.Top);
      Canvas.LineTo(Rect.Left - 3, Rect.Bottom);
      Canvas.MoveTo(Rect.Left - 1, Rect.Top);
      Canvas.LineTo(Rect.Left - 1, Rect.Bottom);
    end;
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

procedure TJvInspectorPainter.ReadCategoryTextColor(Reader: TReader);
begin
  CategoryFont.Color := StringToColor(Reader.ReadIdent);
end;

procedure TJvInspectorPainter.ReadHideSelectTextColor(Reader: TReader);
begin
  HideSelectFont.Color := StringToColor(Reader.ReadIdent);
end;

procedure TJvInspectorPainter.ReadNameColor(Reader: TReader);
begin
  NameFont.Color := StringToColor(Reader.ReadIdent);
end;

procedure TJvInspectorPainter.ReadSelectedTextColor(Reader: TReader);
begin
  SelectedFont.Color := StringToColor(Reader.ReadIdent);
end;

procedure TJvInspectorPainter.ReadValueColor(Reader: TReader);
begin
  ValueFont.Color := StringToColor(Reader.ReadIdent);
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

procedure TJvInspectorPainter.SetCategoryFont(const Value: TFont);
begin
  FCategoryFont.Assign(Value);
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

procedure TJvInspectorPainter.SetHideSelectFont(const Value: TFont);
begin
end;

procedure TJvInspectorPainter.SetNameFont(const Value: TFont);
begin
  FNameFont.Assign(Value);
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

procedure TJvInspectorPainter.SetSelectedFont(const Value: TFont);
begin
  FSelectedFont.Assign(Value);
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

procedure TJvInspectorPainter.SetValueFont(const Value: TFont);
begin
  FValueFont.Assign(Value);
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
  if (AInspector <> nil) and (AInspector.ActivePainter <> Self) then
    raise EJvInspector.CreateRes(@RsEJvInspPaintNotActive);
  if AInspector <> Inspector then
  begin
    if (Inspector <> nil) and (AInspector <> nil) then
      raise EJvInspector.CreateRes(@RsEJvInspPaintOnlyUsedOnce);
    FInspector := AInspector;
  end;
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

  CategoryFont.Color := clBtnText;
  NameFont.Color := clWindowText;
  ValueFont.Color := clWindowText;
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
  if not Item.IsCategory and (TmpRect.Left > Pred(Inspector.DividerAbs)) then
  begin
    TmpRect.Left := 0;
    TmpRect.Right := 0;
  end;
  if not Item.IsCategory and (TmpRect.Right > Pred(Inspector.DividerAbs)) then
    TmpRect.Right := Pred(Inspector.DividerAbs);
  Rects[iprButtonArea] := TmpRect;
  TmpRect := ItemRect2;
  TmpRect.Left := ItemRect2.Left + (Succ(Item.Level) * Inspector.ItemHeight);
  Rects[iprNameArea] := TmpRect;
  if Item.IsCategory then
    Rects[iprValueArea] := Rect(0, 0, 0, 0)
  else
  begin
    if TmpRect.Left > Pred(Inspector.DividerAbs) then
      TmpRect := Rect(0, 0, 0, 0)
    else
      TmpRect.Right := ItemRect2.Left + Pred(Inspector.DividerAbs);
    Rects[iprNameArea] := TmpRect;
    TmpRect := ItemRect2;
    TmpRect.Left := ItemRect2.Left + Inspector.DividerAbs + DividerWidth;
    Rects[iprValueArea] := TmpRect;
  end;
  CalcButtonBasedRects;
  CalcNameBasedRects;
  CalcValueBasedRects;
end;

//=== { TJvInspectorBorlandPainter } =========================================

function TJvInspectorBorlandPainter.DividerWidth: Integer;
begin
  Result := 2;
end;

procedure TJvInspectorBorlandPainter.DoPaint;
var
  TmpRect: TRect;
  X: Integer;
  MaxX: Integer;
begin
  TmpRect := Rects[iprItem];
  if Item = Inspector.Selected then
  begin
    // Selected frame
    InflateRect(TmpRect, 0, 1);
    Dec(TmpRect.Top);
    Inc(TmpRect.Right);
    Frame3D(Canvas, TmpRect, clGray, clWhite, 1);
    Frame3D(Canvas, TmpRect, clGray, cl3DLight, 1);
  end
  else
  begin
    // Dotted line
    X := TmpRect.Left;
    MaxX := TmpRect.Right;
    Canvas.Pen.Color := clGray;
    while X < MaxX do
    begin
      Canvas.Pixels[X, TmpRect.Bottom] := clGray;
      Inc(X, 2);
    end;
  end;

  if not Item.IsCategory then
  begin
    // Draw divider line
    TmpRect := Rects[iprItem];
    PaintDivider(TmpRect.Left + Inspector.DividerAbs, Pred(TmpRect.Top), TmpRect.Bottom);
  end;

  ApplyNameFont;
  Item.DrawName(Canvas);
  ApplyValueFont;
  if Assigned(FOnSetItemColors) then
    FOnSetItemColors(Item, Canvas); // Custom colors for canvas and font for cells depending on values.

  Item.DrawValue(Canvas);

  if ButtonImage <> nil then
    Canvas.CopyRect(Rects[iprBtnDstRect], ButtonImage.Canvas, Rects[iprBtnSrcRect]);
end;

function TJvInspectorBorlandPainter.GetDividerLightColor: TColor;
begin
  Result := FDividerLightColor;
end;

function TJvInspectorBorlandPainter.GetSelectedColor: TColor;
begin
  Result := BackgroundColor;
end;

function TJvInspectorBorlandPainter.GetSelectedFont: TFont;
begin
  Result := NameFont;
end;

procedure TJvInspectorBorlandPainter.InitializeColors;
begin
  inherited InitializeColors;
  SetDefaultProp(Self, 'DividerLightColor');
  ValueFont.Color := clNavy;
end;

procedure TJvInspectorBorlandPainter.PaintDivider(const X, YTop, YBottom: Integer);
begin
  with Canvas do
  begin
    Canvas.Pen.Color := DividerColor;
    MoveTo(X, YTop);
    LineTo(X, YBottom);
    Pen.Color := DividerLightColor;
    MoveTo(Succ(X), YBottom);
    LineTo(Succ(X), YTop);
  end;
end;

procedure TJvInspectorBorlandPainter.SetDividerLightColor(const Value: TColor);
begin
  if DividerLightColor <> Value then
  begin
    FDividerLightColor := Value;
    if not Initializing and not Loading and Assigned(Inspector) then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorBorlandPainter.Setup(const ACanvas: TCanvas);
begin
  inherited Setup(ACanvas);
  Canvas.Brush.Color := clBtnFace;
end;

//=== { TJvInspectorDotNETPainter } ==========================================

procedure TJvInspectorDotNETPainter.ApplyNameFont;
begin
  inherited ApplyNameFont;
  if (Item = Inspector.Selected) and
    not (Item is TJvInspectorCustomCompoundItem) then
  begin
    if Inspector.Focused then
    begin
      Canvas.Brush.Color := SelectedColor;
      Canvas.Font := SelectedFont;
    end
    else
    begin
      Canvas.Brush.Color := HideSelectColor;
      Canvas.Font := HideSelectFont;
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

function TJvInspectorDotNETPainter.GetHideSelectFont: TFont;
begin
  Result := FHideSelectFont;
end;

constructor TJvInspectorDotNETPainter.Create(AOwner: TComponent);
begin
  // inherited Create will call Initialize colors which will use this font.
  FHideSelectFont := TFont.Create;
  FHideSelectFont.OnChange := FontChange;

  inherited Create(AOwner);
end;

destructor TJvInspectorDotNETPainter.Destroy;
begin
  FHideSelectFont.Free;

  inherited Destroy;
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
  if PreNameRect.Left > Pred(Inspector.DividerAbs) then
    PreNameRect := Rect(0, 0, 0, 0)
  else
  begin
    PreNameRect.Right := PreNameRect.Left + RealButtonAreaWidth;
    if PreNameRect.Right > Pred(Inspector.DividerAbs) then
      PreNameRect.Right := Pred(Inspector.DividerAbs);
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
    PaintDivider(Rects[iprItem].Left + Inspector.DividerAbs, Pred(Rects[iprItem].Top),
      Rects[iprItem].Bottom);

  if (Item.IsCategory) and (Item.Level = 0) then
    Canvas.Brush.Color := CategoryColor;
  if (Item = Inspector.Selected) and (not (Item is TJvInspectorCustomCompoundItem) or
    TJvInspectorCustomCompoundItem(Item).SingleName or (TJvInspectorCustomCompoundItem(Item).SelectedColumnIndex = 0)) and
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
    Canvas.Pen.Color := clBtnShadow
  else
    Canvas.Pen.Color := clBtnFace;
  if not EndOfList and not EndOfCat then
    LeftX := Rects[iprItem].Left + RealButtonAreaWidth
  else
    LeftX := Rects[iprItem].Left;
  Canvas.MoveTo(Rects[iprItem].Right, Rects[iprItem].Bottom);
  Canvas.LineTo(Pred(LeftX), Rects[iprItem].Bottom);

  if Item <> Item.BaseCategory then
  begin
    if Item.BaseCategory <> nil then
      Canvas.Pen.Color := clBtnShadow
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

  SetDefaultProp(Self, ['HideSelectColor']);

  HideSelectFont.Color := clHighlightText;
  SelectedFont.Color := clHighlightText;
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

procedure TJvInspectorDotNETPainter.ScaleFonts(const M, D: Integer);
begin
  inherited ScaleFonts(M, D);
  FHideSelectFont.Height := MulDiv(FHideSelectFont.Height, M, D);
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

procedure TJvInspectorDotNETPainter.SetHideSelectFont(const Value: TFont);
begin
  FHideSelectFont.Assign(Value);
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
      if (not Data.IsAssigned or (DisplayValue <> NewValue) or
         (AutoUpdate and (FLastEditCtrlText <> NewValue))) and
         Inspector.DoItemValueChanging(Self, NewValue) then
      begin
        Inc(FUpdateEditCtrl);
        try
          try
            DisplayValue := NewValue;
          except
            if not Inspector.DoItemValueError(Self) then
              raise;
          end;
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
        Inspector.DoItemValueChanged(Self);
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

procedure TJvCustomInspectorItem.ButtonClick(Sender: TObject);
begin
  Edit;
  if EditCtrl <> nil then
    EditCtrl.Text := DisplayValue;
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

procedure TJvCustomInspectorItem.DoAfterItemCreate;
begin
  if Inspector <> nil then
    Inspector.DoAfterItemCreate(Self);
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

procedure TJvCustomInspectorItem.DoValueChanged;
begin
  if Assigned(FOnValueChanged) then
    FOnValueChanged(Self);
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

procedure TJvCustomInspectorItem.Edit;
var
  DisplayStr: string;
begin
  //
  // Override this virtual method to define what happens when item is
  // Edited. If you don't, then this is the default handler.
  // To use it, set iifEdit in one of your item's Flags fields,
  // and then catch the JvInspector.OnItemEdit event.
  //
  if Assigned(FInspector) and Assigned(FInspector.FOnItemEdit) then
  begin
    if Assigned(FEditCtrl) and (FEditCtrl.Text <> FData.AsString) then
      Apply;
    DisplayStr := FData.AsString;
    FInspector.FOnItemEdit(FInspector, Self, DisplayStr);
    if DisplayStr <> Self.FData.AsString then
      FData.SetAsString(DisplayStr); // modified!
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
  if Inspector.AutoDropDown and not DroppedDown then
    DropDown
  else
  begin
    ListBox.Items.Clear;
    GetValueList(ListBox.Items);
  end;
end;

procedure TJvCustomInspectorItem.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(Inspector.FOnEditorKeyPress) then
    Inspector.FOnEditorKeyPress(Inspector, Key);
  if Inspector.AutoComplete and (iifValueList in Flags) and not ReadOnly then
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
        end
        else
        if iifEditButton in Flags then
        begin
          Key := 0;
          ButtonClick(Sender);
        end;
    end;
end;

procedure TJvCustomInspectorItem.EditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(Inspector.FOnEditorKeyUp) then
    Inspector.FOnEditorKeyUp(Inspector, Key, Shift);
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

  function TabNavigate: Boolean;
  begin
    Result := Inspector.WantTabs and (Msg.WParam = VK_TAB);
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
            if not (iifAllowNonListValues in Flags) or
              ((Msg.Msg = WM_KEYDOWN) and
              (TWMKeyDown(Msg).CharCode in [VK_UP, VK_DOWN])) then
              ExecInherited := False;
          end;
        end;
        PostToInsp :=
          (Msg.Msg = WM_KEYDOWN) and ((KeyDataToShiftState(Msg.LParam) = []) and
          ((Msg.WParam in [VK_NEXT, VK_PRIOR]) or
            (not DroppedDown and (Msg.WParam in [VK_DOWN, VK_UP])) or LeftRightCanNavigate)) or TabNavigate;
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
  case Msg.Msg of
    WM_GETDLGCODE:
      begin
        if Inspector.WantTabs then
          Msg.Result := Msg.Result or DLGC_WANTTAB;
      end;
    WM_SETFOCUS:
      begin
        { Changing the focus to another Control in the same form via Mouse-Click, if a
          property-editor is active has no effect until you clicked twice on the control.
          Telling the VCL that this control has the focus, fixes the problem. } 
        SetFocus;
      end;
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
        Result := Inspector.ActivePainter.GetNameHeight(Self);
      irsValueHeight:
        Result := Inspector.ActivePainter.GetValueHeight(Self);
      irsItemHeight:
        Result := Inspector.ItemHeight;
    else
      Result := RowSizing.MinHeight;
    end;
    case RowSizing.SizingFactor of
      irsNameHeight:
        Result := Result + HeightFactor * Inspector.ActivePainter.GetNameHeight(Self);
      irsValueHeight:
        Result := Result + HeightFactor * Inspector.ActivePainter.GetValueHeight(Self);
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

function TJvCustomInspectorItem.GetIsCompoundColumn: Boolean;
begin
  Result := (Parent <> nil) and (Parent is TJvInspectorCustomCompoundItem) and (Parent.IndexOf(Self) < 0);
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

procedure TJvCustomInspectorItem.InvalidateValue;
begin
  DoValueChanged;
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
      SelectValue(1)
    else
    if not Editing and Self.InheritsFrom(TJvInspectorClassItem) and Assigned(Inspector.FOnItemDoubleClicked) then
      Inspector.FOnItemDoubleClicked(Inspector, Self);
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
var
  WasPressed: Boolean;
begin
  WasPressed := FPressed;
  StopTracking;
  if (Button = mbLeft) and WasPressed and (iifEditButton in Flags) then
    ButtonClick(Self);
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
      Dec(Value, Inspector.ActivePainter.GetNameHeight(Self));
    irsValueHeight:
      Dec(Value, Inspector.ActivePainter.GetValueHeight(Self));
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
      Factor := Value div Inspector.ActivePainter.GetNameHeight(Self);
    irsValueHeight:
      Factor := Value div Inspector.ActivePainter.GetValueHeight(Self);
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
  if (Inspector.ActivePainter <> nil) and (Inspector.ActivePainter.DrawNameEndEllipsis) then
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
    ACanvas.Brush.Color := clWindow;
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
      Inc(ARect.Top);
      ACanvas.FillRect(ARect);
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
      Memo.OnContextPopup := Inspector.FOnEditorContextPopup;
      Memo.OnKeyUp := EditKeyUp;
      Memo.OnKeyPress := EditKeyPress;
      Memo.WordWrap := True;
      Memo.WantReturns := False;
      Memo.ScrollBars := ssVertical;
      Memo.OnExit := EditFocusLost;
      TJvInspectorMemo(Memo).OnKillFocus := EditKillFocus;
      SetEditCtrl(Memo);

     if Assigned(Inspector.BeforeEdit) then
       Inspector.BeforeEdit(Inspector as TObject, Self, TCustomEdit(Memo));
    end
    else
    begin
      Edit := TJvInspectorEdit.Create(Inspector);
      Edit.OnContextPopup := Inspector.FOnEditorContextPopup;
      Edit.OnKeyUp := EditKeyUp;
      Edit.OnKeyPress := EditKeyPress;
      Edit.OnExit := EditFocusLost;
      TJvInspectorEdit(Edit).OnKillFocus := EditKillFocus;
      SetEditCtrl(Edit);

      if Assigned(Inspector.BeforeEdit) then
        Inspector.BeforeEdit(Inspector as TObject, Self, Edit as TCustomEdit);
    end;
    if iifEditFixed in Flags then
    begin
      TCustomEditAccessProtected(EditCtrl).ReadOnly := True;
      TCustomEditAccessProtected(EditCtrl).TabStop := False;
      TCustomEditAccessProtected(EditCtrl).Color := Inspector.Canvas.Brush.Color;
    end
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
    TCustomEditAccessProtected(EditCtrl).Font.Assign(Inspector.Font);
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
  BandIdx: Integer;
  FirstBand: Integer;
  BandsVisible: Integer;
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
    if not Inspector.UseBands then
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
    end
    else
    begin
      // Find band and scroll that band into the view
      BandIdx := Inspector.GetBandFor(ViewIdx);
      FirstBand := Inspector.GetBandFor(Inspector.TopIndex);
      BandsVisible := Inspector.ClientWidth div Inspector.BandWidth;
      if (BandIdx < FirstBand) or (BandIdx >= (FirstBand + BandsVisible)) then
        if BandIdx < FirstBand then
          Inspector.TopIndex := Integer(Inspector.BandStarts[BandIdx])
        else
        begin
          FirstBand := BandIdx - BandsVisible + 1;
          if (FirstBand > -1) and (FirstBand < Inspector.BandStarts.Count) then
            Inspector.TopIndex := Integer(Inspector.BandStarts[FirstBand]);
        end;
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

//=== { TJvInspectorCompoundColumn } =========================================

constructor TJvInspectorCompoundColumn.Create(const AParent: TJvInspectorCustomCompoundItem;
  const AItem: TJvCustomInspectorItem);
begin
  inherited Create;
  FParent := AParent;
  Item := AItem;
end;

function TJvInspectorCompoundColumn.GetItem: TJvCustomInspectorItem;
begin
  Result := FItem;
end;

function TJvInspectorCompoundColumn.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TJvInspectorCompoundColumn.GetWidthSet: Integer;
begin
  Result := FWidthSet;
end;

procedure TJvInspectorCompoundColumn.SetItem(Value: TJvCustomInspectorItem);
begin
  if Item <> Value then
  begin
    if (Value <> nil) and (Value.Parent <> Parent) then
      raise EJvInspectorItem.CreateRes(@RsEJvInspItemNotAChild);
    if Item <> nil then
      Parent.Add(Item);
    FItem := Value;
    if Item <> nil then
      Parent.FItems.Extract(Item);
    FWidthSet := 0;
    FWidth := -1;
    Parent.InvalidateList;
  end;
end;

procedure TJvInspectorCompoundColumn.SetWidth(Value: Integer);
begin
  if Value <> Width then
    FWidth := Value;
end;

procedure TJvInspectorCompoundColumn.SetWidthExternal(Value: Integer);
begin
  if Value <> WidthSet then
  begin
    SetWidthSet(Value);
    TJvInspectorCustomCompoundItem(Item.Parent).RecalcColumnWidths(Self);
  end;
end;

procedure TJvInspectorCompoundColumn.SetWidthSet(Value: Integer);
begin
  if Value <> WidthSet then
  begin
    FWidthSet := Value;
    FWidth := -1;
  end;
end;

procedure TJvInspectorCompoundColumn.BeforeDestruction;
begin
  Item := nil;
  inherited BeforeDestruction;
end;

//=== { TJvInspectorCustomCompoundItem } =====================================

constructor TJvInspectorCustomCompoundItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  FColumns := TObjectList.Create;
end;

function TJvInspectorCustomCompoundItem.AddColumnPrim(const Item: TJvCustomInspectorItem): Integer;
begin
  Result := ColumnCount;
  InsertColumnPrim(Result, Item);
end;

function TJvInspectorCustomCompoundItem.AddColumnPrim(const ItemIndex: Integer): Integer;
begin
  Result := ColumnCount;
  InsertColumnPrim(Result, Items[ItemIndex]);
end;

procedure TJvInspectorCustomCompoundItem.DeleteColumnPrim(const Column: TJvInspectorCompoundColumn);
var
  Idx: Integer;
begin
  Idx := IndexOfColumnPrim(Column);
  if Idx > -1 then
    DeleteColumnPrim(Idx)
  else
    raise EJvInspectorItem.CreateRes(@RsEJvInspItemColNotFound);
end;

procedure TJvInspectorCustomCompoundItem.DeleteColumnPrim(const Index: Integer);
begin
  FColumns.Delete(Index);
  if SelectedColumnIndex > ColumnCount then
    SelectedColumnIndex := ColumnCount - 1;
end;

procedure TJvInspectorCustomCompoundItem.DeleteColumnPrim(const Item: TJvCustomInspectorItem);
var
  Idx: Integer;
begin
  Idx := IndexOfColumnPrim(Item);
  if Idx > -1 then
    DeleteColumnPrim(Idx)
  else
    raise EJvInspectorItem.CreateRes(@RsEJvInspItemItemIsNotCol);
end;

procedure TJvInspectorCustomCompoundItem.DivideRect(const RectKind: TInspectorPaintRect; const Value: TRect);
var
  VisibleColCount: Integer;
  I: Integer;
  WidthAvail: Integer;
  CurRect: TRect;
  WidthUsedInt: Integer;
  WidthUsedDbl: Double;
  ColWidth: Double;
  SaveItem: TJvCustomInspectorItem;
begin
  if Inspector.ActivePainter = nil then
    raise EJvInspectorItem.CreateRes(@RsEJvAssertInspectorPainter);
  VisibleColCount := 0;
  for I := 0 to ColumnCount - 1 do
    if Columns[I].Width > 0 then
      Inc(VisibleColCount);
  WidthAvail := RectWidth(Value);
  if VisibleColCount > 1 then
    Dec(WidthAvail, Pred(VisibleColCount) * Inspector.ActivePainter.DividerWidth);
  CurRect := Value;
  WidthUsedInt := 0;
  WidthUsedDbl := 0;
  for I := 0 to ColumnCount - 1 do
  begin
    ColWidth := (Columns[I].Width / 100.0) * WidthAvail;
    WidthUsedDbl := WidthUsedDbl + ColWidth;
    Inc(WidthUsedInt, Trunc(ColWidth));
    if WidthUsedDbl - WidthUsedInt > 1 then
    begin
      Inc(WidthUsedInt);
      ColWidth := ColWidth + 1;
    end;
    CurRect.Right := CurRect.Left + Trunc(ColWidth);
    Columns[I].Item.SetRects(RectKind, CurRect);
    if RectKind = iprValue then
    begin
      SaveItem := Inspector.ActivePainter.Item;
      try
        Inspector.ActivePainter.Item := Columns[I].Item;
        Inspector.ActivePainter.CalcEditBasedRects;
      finally
        Inspector.ActivePainter.Item := SaveItem;
      end;
    end;
    CurRect.Left := CurRect.Right + Inspector.ActivePainter.DividerWidth;
  end;
end;

procedure TJvInspectorCustomCompoundItem.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (SelectedColumn <> nil) and SelectedColumn.Item.Editing then
    SelectedColumn.Item.EditKeyDown(Sender, Key, Shift)
  else
    inherited EditKeyDown(Sender, Key, Shift);
end;

function TJvInspectorCustomCompoundItem.GetColumnCount: Integer;
begin
  if FColumns <> nil then
    Result := FColumns.Count
  else
    Result := 0;
end;

function TJvInspectorCustomCompoundItem.GetColumns(I: Integer): TJvInspectorCompoundColumn;
begin
  Result := TJvInspectorCompoundColumn(FColumns[I]);
end;

function TJvInspectorCustomCompoundItem.GetDisplayName: string;
begin
  if SingleName then
  begin
    if SingleNameUseFirstCol then
    begin
      if ColumnCount > 0 then
        Result := Columns[0].Item.DisplayName
      else
        Result := '';
      if (Parent <> nil) and (iifQualifiedNames in Parent.Flags) then
        Result := Parent.DisplayName + '.' + Result;
    end
    else
      Result := inherited GetDisplayName;
  end;
end;

function TJvInspectorCustomCompoundItem.GetEditCtrl: TCustomEdit;
begin
  if SelectedColumn <> nil then
    Result := SelectedColumn.Item.EditCtrl
  else
    Result := nil;
end;

function TJvInspectorCustomCompoundItem.GetEditCtrlDestroying: Boolean;
begin
  Result := (SelectedColumn <> nil) and SelectedColumn.Item.EditCtrlDestroying;
end;

function TJvInspectorCustomCompoundItem.GetEditing: Boolean;
begin
  Result := (SelectedColumn <> nil) and SelectedColumn.Item.Editing;
end;

function TJvInspectorCustomCompoundItem.GetSelectedColumn: TJvInspectorCompoundColumn;
begin
  if SelectedColumnIndex > -1 then
    Result := Columns[SelectedColumnIndex]
  else
    Result := nil;
end;

function TJvInspectorCustomCompoundItem.GetSelectedColumnIndex: Integer;
begin
  Result := FSelectedColumnIdx;
end;

function TJvInspectorCustomCompoundItem.GetSingleName: Boolean;
begin
  Result := icifSingleName in CompoundItemFlags;
end;

function TJvInspectorCustomCompoundItem.GetSingleNameUseFirstCol: Boolean;
begin
  Result := icifSingleNameUseFirstCol in CompoundItemFlags;
end;

function TJvInspectorCustomCompoundItem.IndexOfColumnPrim(const Col: TJvInspectorCompoundColumn): Integer;
begin
  Result := ColumnCount - 1;
  while (Result >= 0) and (Columns[Result] <> Col) do
    Dec(Result);
end;

function TJvInspectorCustomCompoundItem.IndexOfColumnPrim(const Item: TJvCustomInspectorItem): Integer;
begin
  Result := ColumnCount - 1;
  while (Result >= 0) and (Columns[Result].Item <> Item) do
    Dec(Result);
end;

procedure TJvInspectorCustomCompoundItem.InsertColumnPrim(const Index: Integer; const Item: TJvCustomInspectorItem);
var
  Col: TJvInspectorCompoundColumn;
begin
  Col := TJvInspectorCompoundColumn.Create(Self, Item);
  try
    FColumns.Insert(Index, Col);
    RecalcColumnWidths(Col);
  except
    Col.Free;
    raise;
  end;
end;

procedure TJvInspectorCustomCompoundItem.InsertColumnPrim(const Index, ItemIndex: Integer);
begin
  InsertColumnPrim(Index, Items[ItemIndex]);
end;

procedure TJvInspectorCustomCompoundItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  for I := ColumnCount - 1 downto 0 do
    if PtInRect(Columns[I].Item.Rects[iprName], Point(X, Y)) or
      PtInRect(Columns[I].Item.Rects[iprValue], Point(X, Y)) then
    begin
      SelectedColumnIndex := I;
      Columns[I].Item.MouseDown(Button, Shift, X, Y);
      Break;
    end;
end;

procedure TJvInspectorCustomCompoundItem.RecalcColumnWidths(const SetColumn: TJvInspectorCompoundColumn = nil);
var
  Idx: Integer;
  PercentLeft: Integer;
  I: Integer;
  DivideOver: array of Integer;

  procedure AddDivide(const DivideIndex: Integer);
  begin
    SetLength(DivideOver, Length(DivideOver) + 1);
    DivideOver[High(DivideOver)] := DivideIndex;
  end;

begin
  if SetColumn <> nil then
  begin
    Idx := IndexOfColumnPrim(SetColumn);
    PercentLeft := 100 - SetColumn.WidthSet;
    if SetColumn.WidthSet > 0 then
      SetColumn.SetWidth(SetColumn.WidthSet)
    else
      AddDivide(Idx);
  end
  else
  begin
    Idx := -1;
    PercentLeft := 100;
  end;
  for I := 0 to ColumnCount - 1 do
  begin
    if I <> Idx then
    begin
      if Columns[I].WidthSet <> 0 then
      begin
        if Columns[I].WidthSet <= PercentLeft then
        begin
          Columns[I].SetWidth(Columns[I].WidthSet);
          Dec(PercentLeft, Columns[I].WidthSet);
        end
        else
        begin
          Columns[I].SetWidth(PercentLeft);
          PercentLeft := 0;
        end;
      end
      else
        AddDivide(I);
    end;
  end;
  if Length(DivideOver) > 0 then
  begin
    Idx := PercentLeft mod Length(DivideOver);
    PercentLeft := PercentLeft div Length(DivideOver);
    for I := 0 to High(DivideOver) do
    begin
      if I <> 0 then
        Columns[DivideOver[I]].SetWidth(PercentLeft)
      else
        Columns[DivideOver[I]].SetWidth(PercentLeft + Idx);
    end;
  end;
end;

procedure TJvInspectorCustomCompoundItem.SetCompoundItemFlags(Value: TInspectorCompoundItemFlags);
begin
  // Check the difference: if icifSingleName is removed, remove icifSingleNameUseFirstCol as well
  if ((CompoundItemFlags - Value) * [icifSingleName]) <> [] then
    Exclude(Value, icifSingleNameUseFirstCol)
  else
  if Value = [icifSingleNameUseFirstCol] then
    Include(Value, icifSingleName);
  if Value <> CompoundItemFlags then
  begin
    FCompoundItemFlags := Value;
    InvalidateItem;
  end;
end;

procedure TJvInspectorCustomCompoundItem.SetDisplayName(Value: string);
var
  S: string;
begin
  if SingleName then
  begin
    if SingleNameUseFirstCol then
    begin
      if (Parent <> nil) and (iifQualifiedNames in Parent.Flags) then
        S := Parent.DisplayName + '.';
      if S <> Copy(Value, 1, Length(S)) then
        System.Delete(Value, 1, Length(S));
      if (ColumnCount > 0) and (Columns[0].Item.DisplayName <> Value) then
        Columns[0].Item.DisplayName := Value;
    end
    else
      inherited SetDisplayName(Value);
  end;
end;

procedure TJvInspectorCustomCompoundItem.SetEditing(const Value: Boolean);
begin
  if SelectedColumn <> nil then
    SelectedColumn.Item.SetEditing(Value);
end;

procedure TJvInspectorCustomCompoundItem.SetFlags(const Value: TInspectorItemFlags);
var
  NewFlags: TInspectorItemFlags;
begin
  NewFlags := Value - [iifQualifiedNames, iifAutoUpdate, iifMultiLine,
    iifValueList, iifAllowNonListValues, iifOwnerDrawListFixed,
    iifOwnerDrawListVariable, iifOwnerDrawListMaxHeight, iifEditButton] + [iifReadonly,
    iifEditFixed];
  inherited SetFlags(NewFlags);
end;

procedure TJvInspectorCustomCompoundItem.SetFocus;
begin
  if SelectedColumn <> nil then
    SelectedColumn.Item.SetFocus;
end;

procedure TJvInspectorCustomCompoundItem.SetRects(const RectKind: TInspectorPaintRect; Value: TRect);
begin
  inherited SetRects(RectKind, Value);
  case RectKind of
    iprName, iprValue:
      DivideRect(RectKind, Value);
  end;
end;

procedure TJvInspectorCustomCompoundItem.SetSelectedColumn(Value: TJvInspectorCompoundColumn);
begin
  SelectedColumnIndex := IndexOfColumnPrim(Value);
end;

procedure TJvInspectorCustomCompoundItem.SetSelectedColumnIndex(Value: Integer);
begin
  if Value <> SelectedColumnIndex then
  begin
    DoneEdit(False);
    FSelectedColumnIdx := Value;
    InitEdit;
    InvalidateItem;
  end;
end;

procedure TJvInspectorCustomCompoundItem.SetSingleName(Value: Boolean);
begin
  if Value <> SingleName then
    if Value then
      CompoundItemFlags := CompoundItemFlags + [icifSingleName]
    else
      CompoundItemFlags := CompoundItemFlags - [icifSingleName];
end;

procedure TJvInspectorCustomCompoundItem.SetSingleNameUseFirstCol(Value: Boolean);
begin
  if Value <> SingleNameUseFirstCol then
    if Value then
      CompoundItemFlags := CompoundItemFlags + [icifSingleNameUseFirstCol]
    else
      CompoundItemFlags := CompoundItemFlags - [icifSingleNameUseFirstCol];
end;

procedure TJvInspectorCustomCompoundItem.BeforeDestruction;
begin
  FreeAndNil(FColumns);
  FSelectedColumnIdx := -1;
  inherited BeforeDestruction;
end;

procedure TJvInspectorCustomCompoundItem.DoneEdit(const CancelEdits: Boolean);
begin
  if SelectedColumn <> nil then
    SelectedColumn.Item.DoneEdit(CancelEdits);
end;

procedure TJvInspectorCustomCompoundItem.DrawEditor(const ACanvas: TCanvas);
begin
end;

procedure TJvInspectorCustomCompoundItem.DrawName(const ACanvas: TCanvas);
var
  RTop: Integer;
  RBottom: Integer;
  LastI: Integer;
  I: Integer;
  Col: TJvInspectorCompoundColumn;
begin
  if SingleName then
  begin
    if Inspector.Selected = Self then
    begin
      if Inspector.Focused then
      begin
        ACanvas.Brush.Color := Inspector.ActivePainter.SelectedColor;
        ACanvas.Font := Inspector.ActivePainter.SelectedFont;
      end
      else
      begin
        ACanvas.Brush.Color := Inspector.ActivePainter.HideSelectColor;
        ACanvas.Font := Inspector.ActivePainter.HideSelectFont;
      end;
      with Rects[iprNameArea] do
        ACanvas.FillRect(Rect(Left, Top, Right, Bottom));
    end
    else
    begin
      ACanvas.Brush.Color := Inspector.ActivePainter.BackgroundColor;
      ACanvas.Font := Inspector.ActivePainter.NameFont;
    end;
    inherited DrawName(ACanvas);
  end
  else
  begin
    with Rects[iprNameArea] do
    begin
      RTop := Top;
      RBottom := Bottom;
    end;
    LastI := ColumnCount - 1;
    while (LastI > 0) and (Columns[LastI].Width < 1) do
      Dec(LastI);
    for I := 0 to LastI do
    begin
      Col := Columns[I];
      if Col.Width >= 0 then
      begin
        if (Inspector.Selected = Self) and (I = SelectedColumnIndex) then
        begin
          if Inspector.Focused then
          begin
            ACanvas.Brush.Color := Inspector.ActivePainter.SelectedColor;
            ACanvas.Font := Inspector.ActivePainter.SelectedFont;
          end
          else
          begin
            ACanvas.Brush.Color := Inspector.ActivePainter.HideSelectColor;
            ACanvas.Font := Inspector.ActivePainter.HideSelectFont;
          end;
          with Col.Item.Rects[iprName] do
            ACanvas.FillRect(Rect(Left, RTop, Right, RBottom));
        end
        else
        begin
          ACanvas.Brush.Color := Inspector.ActivePainter.BackgroundColor;
          ACanvas.Font := Inspector.ActivePainter.NameFont;
        end;
        Col.Item.DrawName(ACanvas);
        if I <> LastI then
          with Col.Item.Rects[iprName] do
            Inspector.ActivePainter.PaintDivider(Right - 1, Top + 1, Bottom - 2);
      end;
    end;
  end;
end;

procedure TJvInspectorCustomCompoundItem.DrawValue(const ACanvas: TCanvas);
var
  LastI: Integer;
  I: Integer;
  Col: TJvInspectorCompoundColumn;
begin
  LastI := ColumnCount - 1;
  while (LastI > 0) and (Columns[LastI].Width < 1) do
    Dec(LastI);
  for I := 0 to LastI do
  begin
    Col := Columns[I];
    if Col.Width >= 0 then
    begin
      Col.Item.DrawValue(ACanvas);
      if I <> LastI then
        with Col.Item.Rects[iprValue] do
          Inspector.ActivePainter.PaintDivider(Right - 1, Top + 1, Bottom - 2);
    end;
  end;
end;

function TJvInspectorCustomCompoundItem.EditFocused: Boolean;
begin
  Result := (SelectedColumn <> nil) and (SelectedColumn.Item.EditCtrl <> nil) and
    SelectedColumn.Item.EditCtrl.Focused;
end;

procedure TJvInspectorCustomCompoundItem.InitEdit;
begin
  if SelectedColumn <> nil then
    SelectedColumn.Item.InitEdit;
end;

//=== { TJvInspectorCompoundItem } ===========================================

function TJvInspectorCompoundItem.AddColumn(const Item: TJvCustomInspectorItem): Integer;
begin
  Result := AddColumnPrim(Item);
end;

function TJvInspectorCompoundItem.AddColumn(const ItemIndex: Integer): Integer;
begin
  Result := AddColumnPrim(ItemIndex);
end;

procedure TJvInspectorCompoundItem.DeleteColumn(const Column: TJvInspectorCompoundColumn);
begin
  DeleteColumnPrim(Column);
end;

procedure TJvInspectorCompoundItem.DeleteColumn(const Index: Integer);
begin
  DeleteColumnPrim(Index);
end;

procedure TJvInspectorCompoundItem.DeleteColumn(const Item: TJvCustomInspectorItem);
begin
  DeleteColumnPrim(Item);
end;

function TJvInspectorCompoundItem.IndexOfColumn(const Col: TJvInspectorCompoundColumn): Integer;
begin
  Result := IndexOfColumnPrim(Col);
end;

function TJvInspectorCompoundItem.IndexOfColumn(const Item: TJvCustomInspectorItem): Integer;
begin
  Result := IndexOfColumnPrim(Item);
end;

procedure TJvInspectorCompoundItem.InsertColumn(const Index: Integer; const Item: TJvCustomInspectorItem);
begin
  InsertColumnPrim(Index, Item);
end;

procedure TJvInspectorCompoundItem.InsertColumn(const Index, ItemIndex: Integer);
begin
  InsertColumnPrim(Index, ItemIndex);
end;

//=== { TJvInspectorIntegerItem } ============================================

function TJvInspectorIntegerItem.GetDisplayValue: string;
begin
  Result := JclTypedIntToStr(Integer(Data.AsOrdinal), Data.TypeInfo);
end;

procedure TJvInspectorIntegerItem.SetDisplayValue(const Value: string);
var
  TmpOrd: Integer;
begin
  TmpOrd := JclStrToTypedInt(Value, Data.TypeInfo);
  if (JclTypeInfo(Data.TypeInfo) as IJclOrdinalRangeTypeInfo).OrdinalType = otULong then
    Data.AsOrdinal := Cardinal(TmpOrd)
  else
    Data.AsOrdinal := TmpOrd;
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

//=== { TJvInspectorFloatItem } ==============================================

constructor TJvInspectorFloatItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  FFormat := '';
end;

function TJvInspectorFloatItem.GetDisplayValue: string;
begin
  // WAP: Inspector component doesn't handle exceptions well,
  // so we mask the error nicely here. Ini file data in a float
  // attribute that doesn't convert nicely to a float causes
  // GUI Exception hell.
  try
    Result := FormatFloat(FFormat, Data.AsFloat);
  except
    on E: EConvertError do
      if Data is TJvInspectorCustomConfData then
        Result := (Data as TJvInspectorCustomConfData).ForceString // INI Display Workaround.
      else
        Result := '0'; // Inspector component doesn't handle this exception well, so mask it. workaround. WAP
  end;
end;

procedure TJvInspectorFloatItem.SetDisplayValue(const Value: string);
begin
  Data.AsFloat := StrToFloat(Value);
end;

//=== { TJvInspectorSetMemberData } ==========================================

function TJvInspectorSetMemberData.GetAsFloat: Extended;
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
end;

function TJvInspectorSetMemberData.GetAsInt64: Int64;
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
end;

function TJvInspectorSetMemberData.GetAsMethod: TMethod;
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
end;

function TJvInspectorSetMemberData.GetAsOrdinal: Int64;
var
  Buf: array [0..31] of Byte;
begin
  CheckReadAccess;
  DataParent.GetAsSet(Buf);
  Result := Ord(TestBitBuffer(Buf, BitOffset));
end;

function TJvInspectorSetMemberData.GetAsString: string;
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
end;

function TJvInspectorSetMemberData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := (Ref is TJvInspectorSetMemberData) and
    (TJvInspectorSetMemberData(Ref).DataParent = DataParent) and
    (TJvInspectorSetMemberData(Ref).BitOffset = BitOffset);
end;

procedure TJvInspectorSetMemberData.NotifyRemoveData(const Instance: TJvCustomInspectorData);
begin
  // if the instance to be removed is the data parent of this instance, free this instance as well.
  if Instance = DataParent then
    Free;
end;

procedure TJvInspectorSetMemberData.SetAsFloat(const Value: Extended);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
end;

procedure TJvInspectorSetMemberData.SetAsInt64(const Value: Int64);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
end;

procedure TJvInspectorSetMemberData.SetAsMethod(const Value: TMethod);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
end;

procedure TJvInspectorSetMemberData.SetAsOrdinal(const Value: Int64);
var
  Buf: array [0..31] of Byte;
begin
  CheckWriteAccess;
  DataParent.GetAsSet(Buf);
  if Value <> 0 then
    SetBitBuffer(Buf, BitOffset)
  else
    ClearBitBuffer(Buf, BitOffset);
  DataParent.SetAsSet(Buf);
end;

procedure TJvInspectorSetMemberData.SetAsString(const Value: string);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
end;

procedure TJvInspectorSetMemberData.GetAsSet(var Buf);
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorSet]);
end;

function TJvInspectorSetMemberData.HasValue: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorSetMemberData.IsAssigned: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorSetMemberData.IsInitialized: Boolean;
begin
  Result := True;
end;

class function TJvInspectorSetMemberData.New(const AParent: TJvCustomInspectorItem;
  const Ordinal: Integer; const ADataParent: TJvCustomInspectorData): TJvCustomInspectorItem;
var
  BaseInfo: IJclOrdinalRangeTypeInfo;
  Data: TJvInspectorSetMemberData;
begin
  if ADataParent = nil then
    raise EJvInspectorData.CreateRes(@RsEJvAssertDataParent);
  if AParent = nil then
    raise EJvInspectorData.CreateRes(@RsEJvAssertParent);
  BaseInfo := ((JclTypeInfo(ADataParent.TypeInfo) as IJclSetTypeInfo).
    BaseType as IJclOrdinalRangeTypeInfo);
  if BaseInfo.TypeKind = tkEnumeration then
    Data := CreatePrim(GetEnumName(BaseInfo.TypeInfo, Ordinal), System.TypeInfo(Boolean))
  else
    Data := CreatePrim(IntToStr(Ordinal), System.TypeInfo(Boolean));
  Data.FBitOffset := Ordinal mod 8 + 8 * ((Ordinal div 8) - (BaseInfo.MinValue div 8));
  Data.FDataParent := ADataParent;
  Data := TJvInspectorSetMemberData(DataRegister.Add(Data));
  if Data <> nil then
    Result := Data.NewItem(AParent)
  else
    Result := nil;
end;

procedure TJvInspectorSetMemberData.SetAsSet(const Buf);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorSet]);
end;

//=== { TJvInspectorSetItem } ================================================

constructor TJvInspectorSetItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  ItemSetFlags := [isfCreateMemberItems];
end;

function TJvInspectorSetItem.CanEdit: Boolean;
begin
  Result := inherited CanEdit and (isfEditString in ItemSetFlags);
end;

procedure TJvInspectorSetItem.CreateMembers;
var
  SetInfo: IJclSetTypeInfo;
  BaseInfo: IJclOrdinalRangeTypeInfo;
  OrdVal: Integer;
begin
  Inspector.BeginUpdate;
  try
    DeleteMembers;
    JclTypeInfo(Data.TypeInfo).QueryInterface(IJclSetTypeInfo, SetInfo);
    SetInfo.BaseType.QueryInterface(IJclOrdinalRangeTypeInfo, BaseInfo);
    for OrdVal := Integer(BaseInfo.MinValue) to Integer(BaseInfo.MaxValue) do
      TJvInspectorSetMemberData.New(Self, OrdVal, Data);
  finally
    Inspector.EndUpdate;
  end;
end;

procedure TJvInspectorSetItem.DeleteMembers;
var
  I: Integer;
begin
  Inspector.BeginUpdate;
  try
    I := Pred(Count);
    while (I >= 0) do
    begin
      if Items[I].Data is TJvInspectorSetMemberData then
        Delete(I);
      Dec(I);
    end;
  finally
    Inspector.EndUpdate;
  end;
end;

function TJvInspectorSetItem.GetCreateMemberItems: Boolean;
begin
  Result := (isfCreateMemberItems in ItemSetFlags);
end;

function TJvInspectorSetItem.GetDisplayValue: string;
var
  SetBuf: array [0..31] of Byte;
begin
  Data.GetAsSet(SetBuf);
  Result := JclSetToStr(Data.TypeInfo, SetBuf, True, False);
end;

function TJvInspectorSetItem.GetEditString: Boolean;
begin
  Result := (isfEditString in ItemSetFlags);
end;

function TJvInspectorSetItem.GetRenderAsCategory: Boolean;
begin
  Result := (isfRenderAsCategory in ItemSetFlags);
end;

function TJvInspectorSetItem.GetItemSetFlags: TInspectorSetFlags;
begin
  Result := FItemSetFlags;
end;

procedure TJvInspectorSetItem.InvalidateMetaData;
begin
  if CreateMemberItems or RenderAsCategory then
    CreateMembers
  else
    DeleteMembers;
end;

function TJvInspectorSetItem.IsCategory: Boolean;
begin
  Result := RenderAsCategory;
end;

procedure TJvInspectorSetItem.SetCreateMemberItems(const Value: Boolean);
begin
  if Value <> CreateMemberItems then
    if Value then
      ItemSetFlags := ItemSetFlags + [isfCreateMemberItems]
    else
      ItemSetFlags := ItemSetFlags - [isfCreateMemberItems];
end;

procedure TJvInspectorSetItem.SetDisplayValue(const Value: string);
var
  SetBuf: array [0..31] of Byte;
begin
  JclStrToSet(Data.TypeInfo, SetBuf[0], Value);
  Data.SetAsSet(SetBuf[0]);
end;

procedure TJvInspectorSetItem.SetEditString(const Value: Boolean);
begin
  if Value <> EditString then
    if Value then
      ItemSetFlags := ItemSetFlags + [isfEditString]
    else
      ItemSetFlags := ItemSetFlags - [isfEditString];
end;

procedure TJvInspectorSetItem.SetRenderAsCategory(const Value: Boolean);
begin
  if Value <> RenderAsCategory then
    if Value then
      ItemSetFlags := ItemSetFlags + [isfRenderAsCategory]
    else
      ItemSetFlags := ItemSetFlags - [isfRenderAsCategory];
end;

procedure TJvInspectorSetItem.SetFlags(const Value: TInspectorItemFlags);
var
  OldReadOnly: Boolean;
  I: Integer;
begin
  OldReadOnly := ReadOnly;
  inherited SetFlags(Value);
  if (OldReadOnly <> ReadOnly) and CreateMemberItems then
    for I := 0 to Pred(Count) do
      Items[I].ReadOnly := ReadOnly;
end;

procedure TJvInspectorSetItem.SetItemSetFlags(const Value: TInspectorSetFlags);
begin
  if ItemSetFlags <> Value then
  begin
    FItemSetFlags := Value;
    InvalidateMetaData;
  end;
end;

//=== { TJvInspectorCharItem } ===============================================

function TJvInspectorCharItem.GetDisplayValue: string;
var
  I: Integer;
begin
  I := Data.AsOrdinal;
  if (I <= Ord(' ')) or (I > Ord('~')) then
    Result := '#' + IntToStr(I)
  else
    Result := Chr(Byte(I));
end;

procedure TJvInspectorCharItem.SetDisplayValue(const Value: string);
var
  I: Integer;
begin
  if Length(Value) > 1 then
    I := StrToInt(Copy(Value, 2, Length(Value)))
  else
  if Length(Value) = 1 then
    I := Ord(Value[1])
  else
    I := 0;
  Data.AsOrdinal := I;
end;

//=== { TJvInspectorInt64Item } ==============================================

function TJvInspectorInt64Item.GetDisplayValue: string;
begin
  Result := IntToStr(Data.AsInt64);
end;

procedure TJvInspectorInt64Item.SetDisplayValue(const Value: string);
begin
  // (rom) is this safe? StrToInt64 can throw exceptions.
  // (wpostma) definitely not safe. This is a crap implementation.
  Data.AsInt64 := StrToInt64Def(Value,0);
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

{ TJvInspectorVariantItem }

function TJvInspectorVariantItem.GetDisplayValue: string;
begin
  Result := VarToStr(Data.AsVariant);   // return empty string instead of triggering exception when Data is Null
end;

procedure TJvInspectorVariantItem.SetDisplayValue(const Value: string);
begin
  if Value = '' then
    Data.AsVariant := Unassigned
  else
    Data.AsVariant := Value;
end;

//=== { TJvInspectorClassItem } ==============================================

constructor TJvInspectorClassItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  if GetTypeData(Data.TypeInfo).ClassType.InheritsFrom(Classes.TComponent) then
  begin
    ItemClassFlags := [icfCreateMemberItems];
    Flags := Flags + [iifValueList];
  end
  else
  if GetTypeData(Data.TypeInfo).ClassType.InheritsFrom(TPersistent) then
    ItemClassFlags := [icfCreateMemberItems, icfShowClassName]
  else
    ItemClassFlags := [icfShowClassName];
end;

procedure TJvInspectorClassItem.CreateMembers;
begin
  if Data.IsInitialized and (Data.AsOrdinal <> 0) then
  begin
    Inspector.BeginUpdate;
    try
      DeleteMembers;
      TJvInspectorPropData.New(Self, TObject(Data.AsOrdinal));
      FLastMemberInstance := TObject(Data.AsOrdinal);
    finally
      Inspector.EndUpdate;
    end;
  end;
end;

function TJvInspectorClassItem.CanEdit: Boolean;
begin
  Result := inherited CanEdit and
    ((iifEditButton in Flags) or (iifValueList in Flags));
end;

procedure TJvInspectorClassItem.DeleteMembers;
var
  I: Integer;
begin
  if Data.IsInitialized then
  begin
    Inspector.BeginUpdate;
    try
      for I := Pred(Count) downto 0 do
        if (Items[I].Data is TJvInspectorPropData) and (Items[I].Data.IsInitialized) and
          (TJvInspectorPropData(Items[I].Data).Instance = FLastMemberInstance) then
          Delete(I);
      FLastMemberInstance := nil;
    finally
      Inspector.EndUpdate;
    end;
  end;
end;

function TJvInspectorClassItem.GetCreateMemberItems: Boolean;
begin
  Result := (icfCreateMemberItems in ItemClassFlags);
end;

function TJvInspectorClassItem.GetDisplayValue: string;
var
  Obj: TObject;
  SL: TStringList;
  I: Integer;
begin
  Obj := TObject(Data.AsOrdinal);
  if ShowClassName then
  begin
    if Obj <> nil then
      Result := '(' + Obj.ClassName + ')'
    else
      Result := '(' + GetTypeData(Data.TypeInfo).ClassType.ClassName + ')';
  end
  else
  begin
    if Obj <> nil then
    begin
      SL := TStringList.Create;
      try
        GetValueList(SL);
        I := SL.IndexOfObject(Obj);
        if I > -1 then
          Result := SL[I]
        else
          Result := '';
      finally
        SL.Free;
      end;
    end
    else
      Result := '';
  end;
end;

function TJvInspectorClassItem.GetItemClassFlags: TInspectorClassFlags;
begin
  Result := FItemClassFlags;
end;

function TJvInspectorClassItem.GetRenderAsCategory: Boolean;
begin
  Result := (icfRenderAsCategory in ItemClassFlags);
end;

function TJvInspectorClassItem.GetShowClassName: Boolean;
begin
  Result := (icfShowClassName in ItemClassFlags);
end;

procedure TJvInspectorClassItem.InvalidateItem;
begin
  inherited InvalidateItem;
  if CreateMemberItems or RenderAsCategory then
    CreateMembers;
end;

procedure TJvInspectorClassItem.InvalidateMetaData;
begin
  if CreateMemberItems or RenderAsCategory then
    CreateMembers
  else
    DeleteMembers;
end;

function TJvInspectorClassItem.IsCategory: Boolean;
begin
  Result := RenderAsCategory;
end;

procedure TJvInspectorClassItem.SetCreateMemberItems(const Value: Boolean);
begin
  if Value <> CreateMemberItems then
    if Value then
      ItemClassFlags := ItemClassFlags + [icfCreateMemberItems]
    else
      ItemClassFlags := ItemClassFlags - [icfCreateMemberItems];
end;

procedure TJvInspectorClassItem.SetDisplayValue(const Value: string);
var
  SL: TStrings;
  I: Integer;
begin
  if Value = '' then
    Data.AsOrdinal := 0
  else
  begin
    SL := TStringList.Create;
    try
      GetValueList(SL);
      I := SL.IndexOf(Value);
      if I > -1 then
        Data.AsOrdinal := Integer(SL.Objects[I])
      else
        raise EJvInspectorItem.CreateResFmt(@RsEJvInspItemInvalidPropValue,
          [AnsiQuotedStr(Value, '''')]);
    finally
      SL.Free;
    end;
  end;
end;

procedure TJvInspectorClassItem.SetItemClassFlags(Value: TInspectorClassFlags);
begin
  if Value <> ItemClassFlags then
  begin
    FItemClassFlags := Value;
    InvalidateMetaData;
  end;
end;

procedure TJvInspectorClassItem.SetRenderAsCategory(const Value: Boolean);
begin
  if Value <> RenderAsCategory then
    if Value then
      ItemClassFlags := ItemClassFlags + [icfRenderAsCategory]
    else
      ItemClassFlags := ItemClassFlags - [icfRenderAsCategory];
end;

procedure TJvInspectorClassItem.SetShowClassName(const Value: Boolean);
begin
  if Value <> ShowClassName then
    if Value then
      ItemClassFlags := ItemClassFlags + [icfShowClassName]
    else
      ItemClassFlags := ItemClassFlags - [icfShowClassName];
end;

//=== { TJvInspectorComponentItem } ==========================================

constructor TJvInspectorComponentItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  FOwners := TList.Create;
end;

function TJvInspectorComponentItem.GetItemComponentFlags: TInspectorComponentFlags;
begin
  Result := FItemComponentFlags;
end;

function TJvInspectorComponentItem.GetKeepFirstOwnerAsFirst: Boolean;
begin
  Result := icfKeepFirstOwnerAsFirst in ItemComponentFlags;
end;

function TJvInspectorComponentItem.GetNoShowFirstOwnerName: Boolean;
begin
  Result := icfNoShowFirstOwnerName in ItemComponentFlags;
end;

function TJvInspectorComponentItem.GetOwnerCount: Integer;
begin
  Result := FOwners.Count;
end;

function TJvInspectorComponentItem.GetOwners(I: Integer): TComponent;
begin
  Result := TComponent(FOwners[I]);
end;

function TJvInspectorComponentItem.GetShowOwnerNames: Boolean;
begin
  Result := icfShowOwnerNames in ItemComponentFlags;
end;

function TJvInspectorComponentItem.GetSortComponents: Boolean;
begin
  Result := icfSortComponents in ItemComponentFlags;
end;

function TJvInspectorComponentItem.GetSortOwners: Boolean;
begin
  Result := icfSortOwners in ItemComponentFlags;
end;

procedure TJvInspectorComponentItem.GetValueList(const Strings: TStrings);
var
  MinClass: TClass;
  SL: TStringList;
  OwnerList: TStringList;
  I, J: Integer;
  CurOwner: TComponent;
  PrefixWithOwner: string;
begin
  MinClass := GetTypeData(Data.TypeInfo).ClassType;
  SL := TStringList.Create;
  try
    OwnerList := TStringList.Create;
    try
      for I := 0 to OwnerCount - 1 do
        OwnerList.AddObject(Owners[I].Name, Owners[I]);
      if SortOwners then
        OwnerList.Sort;
      if (OwnerCount > 0) and KeepFirstOwnerAsFirst then
      begin
        I := OwnerList.IndexOfObject(Owners[0]);
        if I > 0 then
        begin
          OwnerList.Delete(I);
          OwnerList.InsertObject(0, Owners[0].Name, Owners[0]);
        end;
      end;
      for I := 0 to OwnerCount - 1 do
      begin
        SL.Clear;
        CurOwner := TComponent(OwnerList.Objects[I]);
        if ShowOwnerNames then
        begin
          if (I > 0) or not NoShowFirstOwnerName then
            PrefixWithOwner := CurOwner.Name + '.';
        end
        else
          PrefixWithOwner := '';
        for J := 0 to CurOwner.ComponentCount - 1 do
          // don't allow setting Self as property
          if (CurOwner.Components[J] is MinClass) and (not (Parent.Data is TJvInspectorPropData) or
              (CurOwner.Components[J] <> TJvInspectorPropData(Parent.Data).Instance)) then
            SL.AddObject(PrefixWithOwner + CurOwner.Components[J].Name, CurOwner.Components[J]);
        if SL.Count > 0 then
        begin
          if SortComponents then
            SL.Sort;
          Strings.AddStrings(SL);
        end;
      end;
      SL.Clear;
      inherited GetValueList(SL);
      if SortComponents then
        SL.Sort;
      if SL.Count > 0 then
        Strings.AddStrings(SL);
    finally
      OwnerList.Free;
    end;
  finally
    SL.Free;
  end;
end;

procedure TJvInspectorComponentItem.SetFlags(const Value: TInspectorItemFlags);
begin
  inherited SetFlags(Value + [iifValueList]);
end;

procedure TJvInspectorComponentItem.SetItemClassFlags(Value: TInspectorClassFlags);
begin
  inherited SetItemClassFlags(Value - [icfShowClassName]);
end;

procedure TJvInspectorComponentItem.SetItemComponentFlags(Value: TInspectorComponentFlags);
begin
  if ItemComponentFlags <> Value then
  begin
    FItemComponentFlags := Value;
    InvalidateMetaData;
  end;
end;

procedure TJvInspectorComponentItem.SetKeepFirstOwnerAsFirst(Value: Boolean);
begin
  if Value <> KeepFirstOwnerAsFirst then
    if Value then
      ItemComponentFlags := ItemComponentFlags + [icfKeepFirstOwnerAsFirst]
    else
      ItemComponentFlags := ItemComponentFlags - [icfKeepFirstOwnerAsFirst];
end;

procedure TJvInspectorComponentItem.SetNoShowFirstOwnerName(Value: Boolean);
begin
  if Value <> NoShowFirstOwnerName then
    if Value then
      ItemComponentFlags := ItemComponentFlags + [icfNoShowFirstOwnerName]
    else
      ItemComponentFlags := ItemComponentFlags - [icfNoShowFirstOwnerName];
end;

procedure TJvInspectorComponentItem.SetOwners(I: Integer; Value: TComponent);
begin
  FOwners[I] := Value;
end;

procedure TJvInspectorComponentItem.SetShowOwnerNames(Value: Boolean);
begin
  if Value <> ShowOwnerNames then
    if Value then
      ItemComponentFlags := ItemComponentFlags + [icfShowOwnerNames]
    else
      ItemComponentFlags := ItemComponentFlags - [icfShowOwnerNames];
end;

procedure TJvInspectorComponentItem.SetSortComponents(Value: Boolean);
begin
  if Value <> SortComponents then
    if Value then
      ItemComponentFlags := ItemComponentFlags + [icfSortComponents]
    else
      ItemComponentFlags := ItemComponentFlags - [icfSortComponents];
end;

procedure TJvInspectorComponentItem.SetSortOwners(Value: Boolean);
begin
  if Value <> SortOwners then
    if Value then
      ItemComponentFlags := ItemComponentFlags + [icfSortOwners]
    else
      ItemComponentFlags := ItemComponentFlags - [icfSortOwners];
end;

procedure TJvInspectorComponentItem.BeforeDestruction;
begin
  FOwners.Free;
  inherited BeforeDestruction;
end;

procedure TJvInspectorComponentItem.AddOwner(const AOwner: TComponent);
begin
  if FOwners.IndexOf(AOwner) < 0 then
    FOwners.Add(AOwner);
end;

procedure TJvInspectorComponentItem.DeleteOwner(const AOwner: TComponent);
begin
  FOwners.Remove(AOwner);
end;

procedure TJvInspectorComponentItem.DeleteOwner(const Index: Integer);
begin
  FOwners.Delete(Index);
end;

//=== { TJvInspectorFontItem } ===============================================

procedure TJvInspectorFontItem.Edit;
begin
  with TFontDialog.Create(GetParentForm(Inspector)) do
    try
      Font.Assign(TFont(Data.AsOrdinal));
      Device := fdScreen;
      if Execute then
      begin
        TFont(Data.AsOrdinal).Assign(Font);
        Data.InvalidateData;
      end;
    finally
      Free;
      Inspector.ShowScrollBars(SB_BOTH, False);
    end;
end;

procedure TJvInspectorFontItem.SetFlags(const Value: TInspectorItemFlags);
var
  NewValue: TInspectorItemFlags;
begin
  NewValue := Value + [iifEditButton, iifEditFixed];
  inherited SetFlags(NewValue);
end;

//=== { TJvInspectorFontNameItem } ===========================================

function TJvInspectorFontNameItem.GetUseFont: Boolean;
begin
  Result := FUseFont;
end;

procedure TJvInspectorFontNameItem.SetUseFont(Value: Boolean);
begin
  if UseFont <> Value then
  begin
    FUseFont := Value;
    InvalidateMetaData;
  end;
end;

procedure TJvInspectorFontNameItem.DoDrawListItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  FontName: string;
begin
  with TListBox(Control) do
  begin
    if UseFont then
    begin
      FontName := Items[Index];
      Canvas.Font.Name := FontName;
    end;
    DoDefaultDrawListItem(TListBox(Control).Canvas, Rect, TListBox(Control).Items[Index]);
  end;
end;

procedure TJvInspectorFontNameItem.DoMeasureListItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
var
  FontName: string;
begin
  if UseFont then
    with TListBox(Control) do
    begin
      FontName := Items[Index];
      Canvas.Font.Name := FontName;
    end;
  Height := CanvasMaxTextHeight(TListBox(Control).Canvas);
end;

procedure TJvInspectorFontNameItem.DoMeasureListItemWidth(Control: TWinControl;
  Index: Integer; var Width: Integer);
var
  FontName: string;
begin
  FontName := TListBox(Control).Items[Index];
  if UseFont then
    TListBox(Control).Canvas.Font.Name := FontName;
  Width := TListBox(Control).Canvas.TextWidth(FontName);
end;

procedure TJvInspectorFontNameItem.GetValueList(const Strings: TStrings);
begin
  Strings.Assign(Screen.Fonts);
end;

procedure TJvInspectorFontNameItem.SetFlags(const Value: TInspectorItemFlags);
var
  NewValue: TInspectorItemFlags;
begin
  NewValue := Value + [iifValueList, iifOwnerDrawListMaxHeight];
  inherited SetFlags(NewValue);
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
  if PtInRect(FCheckRect, Point(X, Y)) and (Shift = [ssLeft]) and
    Editing and ShowAsCheckBox then
  begin
    Data.AsOrdinal := Ord(Bool);
    InvalidateItem;
  end
  else
  begin
    if (ssDouble in Shift) and ShowAsCheckBox then
      Shift := Shift - [ssDouble];
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
      ACanvas.Brush.Color := clWindow;
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
  end;
end;

procedure TJvInspectorBooleanItem.InitEdit;
begin
  if ShowAsCheckBox then
    SetEditing(CanEdit)
  else
    inherited InitEdit;
end;

//=== { TJvInspectorDateItem } ===============================================

constructor TJvInspectorDateItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  FFormat := JclFormatSettings.ShortDateFormat;
end;

function TJvInspectorDateItem.GetDisplayValue: string;
begin
  Result := FormatDateTime(Format, Data.AsFloat);
end;

procedure TJvInspectorDateItem.SetDisplayValue(const Value: string);
begin
  if Data.IsAssigned then
    Data.AsFloat := Trunc(StrToDate(Value)) + Frac(Data.AsFloat)
  else
    Data.AsFloat := Trunc(StrToDate(Value));
end;

procedure TJvInspectorDateItem.SetFormat(const Value: string);
var
  I: Integer;
  MCount: Integer;
  DCount: Integer;
  YCount: Integer;
  SepCount: Integer;
  WasEditing: Boolean;
begin
  // Only allow d, dd, m, mm, yy, yyyy and the date separator characters to ease parsing
  I := 1;
  MCount := 0;
  DCount := 0;
  YCount := 0;
  SepCount := 0;
  while I < Length(Value) do
  begin
    case Value[I] of
      'd':
        begin
          if (DCount = 0) and (I > 1) and (Value[I - 1] <> JclFormatSettings.DateSeparator) then
            raise EJvInspectorData.CreateRes(@RsESpecifierBeforeSeparator);
          if (DCount = 1) and (Value[I - 1] <> 'd') then
            raise EJvInspectorData.CreateRes(@RsEDOrDDOnlyOnce);
          if DCount = 2 then
            raise EJvInspectorData.CreateRes(@RsEOnlyDOrDDAllowed);
          Inc(DCount);
        end;
      'm':
        begin
          if (MCount = 0) and (I > 1) and (Value[I - 1] <> JclFormatSettings.DateSeparator) then
            raise EJvInspectorData.CreateRes(@RsESpecifierBeforeSeparator);
          if (MCount = 1) and (Value[I - 1] <> 'm') then
            raise EJvInspectorData.CreateRes(@RsEMOrMMOnlyOnce);
          if MCount = 2 then
            raise EJvInspectorData.CreateRes(@RsEOnlyMOrMMAllowed);
          Inc(MCount);
        end;
      'y':
        begin
          if (MCount = 0) and (I > 1) and (Value[I - 1] <> JclFormatSettings.DateSeparator) then
            raise EJvInspectorData.CreateRes(@RsESpecifierBeforeSeparator);
          if (YCount > 1) and (YCount < 4) and (Value[I - 1] <> 'y') then
            raise EJvInspectorData.CreateRes(@RsEYYOrYYYYOnlyOnce);
          if YCount = 4 then
            raise EJvInspectorData.CreateRes(@RsEOnlyYYOrYYYYAllowed);
          Inc(YCount);
        end;
    else
      if Value[I] = JclFormatSettings.DateSeparator then
      begin
        if ((SepCount = 0) and (I = 1)) or
          ((SepCount = 1) and ((Value[I - 1]) = JclFormatSettings.DateSeparator) or (I = Length(Value))) then
          raise EJvInspectorData.CreateRes(@RsESpecifierBeforeSeparator);
        if SepCount = 2 then
          raise EJvInspectorData.CreateRes(@RsEOnlyTwoSeparators);
        Inc(SepCount);
      end
      else
        raise EJvInspectorData.CreateResFmt(@RsEOnlyDMYSAllowed, [JclFormatSettings.DateSeparator]);
    end;
    Inc(I);
  end;
  if DCount = 0 then
    raise EJvInspectorData.CreateRes(@RsEDOrDDRequired);
  if MCount = 0 then
    raise EJvInspectorData.CreateRes(@RsEMOrMMRequired);
  if YCount = 0 then
    raise EJvInspectorData.CreateRes(@RsEYYOrYYYYRequired);
  if (YCount = 1) or (YCount = 3) then
    raise EJvInspectorData.CreateRes(@RsEOnlyYYOrYYYYAllowed);
  if Value <> FFormat then
  begin
    WasEditing := Editing;
    if Editing then
      DoneEdit;
    FFormat := Value;
    if WasEditing then
      InitEdit;
  end;
end;

//=== { TJvInspectorTimeItem } ===============================================

constructor TJvInspectorTimeItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  FShowSeconds := True;
  FShowAMPM := False;
  SetFormat;
end;

function TJvInspectorTimeItem.GetDisplayValue: string;
begin
  Result := FormatDateTime(Format, Data.AsFloat);
end;

procedure TJvInspectorTimeItem.SetDisplayValue(const Value: string);
begin
  if Data.IsAssigned then
    Data.AsFloat := Frac(StrToTime(Value)) + Trunc(Data.AsFloat)
  else
    Data.AsFloat := Frac(StrToTime(Value)) + Trunc(Data.AsFloat);
end;

procedure TJvInspectorTimeItem.SetFormat;
begin
  FFormat := 'hh:nn';
  if ShowSeconds then
    FFormat := FFormat + ':ss';
  if ShowAMPM then
    FFormat := FFormat + ' ampm';
end;

procedure TJvInspectorTimeItem.SetShowAMPM(Value: Boolean);
var
  WasEditing: Boolean;
begin
  if Value <> ShowAMPM then
  begin
    WasEditing := Editing;
    DoneEdit;
    FShowAMPM := Value;
    SetFormat;
    if WasEditing then
      InitEdit;
  end;
end;

procedure TJvInspectorTimeItem.SetShowSeconds(Value: Boolean);
var
  WasEditing: Boolean;
begin
  if Value <> ShowSeconds then
  begin
    WasEditing := Editing;
    DoneEdit;
    FShowSeconds := Value;
    SetFormat;
    if WasEditing then
      InitEdit;
  end;
end;

//=== { TJvInspectorDateTimeItem } ===========================================

constructor TJvInspectorDateTimeItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  SingleNameUseFirstCol := True;
  FDate := TJvInspectorDateItem.Create(Self, AData);
  FTime := TJvInspectorTimeItem.Create(Self, AData);
  AddColumnPrim(FDate);
  AddColumnPrim(FTime);
end;

function TJvInspectorDateTimeItem.GetDateFormat: string;
begin
  Result := FDate.Format;
end;

function TJvInspectorDateTimeItem.GetTimeShowAMPM: Boolean;
begin
  Result := FTime.ShowAMPM;
end;

function TJvInspectorDateTimeItem.GetTimeShowSeconds: Boolean;
begin
  Result := FTime.ShowSeconds;
end;

procedure TJvInspectorDateTimeItem.SetDateFormat(const Value: string);
begin
  FDate.Format := Value;
end;

procedure TJvInspectorDateTimeItem.SetTimeShowAMPM(Value: Boolean);
begin
  FTime.ShowAMPM := Value;
end;

procedure TJvInspectorDateTimeItem.SetTimeShowSeconds(Value: Boolean);
begin
  FTime.ShowSeconds := Value;
end;

//=== { TSLEditorForm } ======================================================

type
  TSLEditorForm = class(TCustomForm)
  public
    grp: TGroupBox;
    lbl: TLabel;
    mm: TMemo;
    btnOK: TButton;
    btnCancel: TButton;
    OnContentsChanged: TNotifyEvent;
    constructor CreateNew(AOwner: TComponent); reintroduce;
    procedure MemoChanged(Sender: TObject);
  end;

constructor TSLEditorForm.CreateNew(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Caption := RsStringListEditorCaption;
  Width := 435;
  Height := 305;
  BorderIcons := [biSystemMenu];
  grp := TGroupBox.Create(Self);
  grp.Parent := Self;
  grp.Left := 10;
  grp.Top := 10;
  grp.Width := ClientWidth - 20;
  grp.Height := 230;
  grp.Anchors := [akTop, akLeft, akRight, akBottom];
  lbl := TLabel.Create(Self);
  lbl.Parent := grp;
  lbl.Caption := '';
  lbl.AutoSize := False;
  lbl.Left := 10;
  lbl.Top := 10;
  lbl.Width := grp.ClientWidth - 20;
  lbl.Anchors := [akTop, akLeft, akRight];
  mm := TMemo.Create(Self);
  mm.Parent := grp;
  mm.Left := 10;
  mm.Top := 30;
  mm.Width := grp.ClientWidth - 20;
  mm.Height := grp.ClientHeight - 40;
  mm.Anchors := [akTop, akLeft, akRight, akBottom];
  mm.ScrollBars := ssBoth;
  mm.WordWrap := False;
  mm.WantReturns := True;
  mm.WantTabs := False;
  mm.OnChange := MemoChanged;
  btnOK := TButton.Create(Self);
  btnOK.Parent := Self;
  btnOK.ModalResult := mrOK;
  btnOK.Default := True;
  btnOK.Caption := RsButtonOKCaption;
  btnOK.Left := ClientWidth - 15 - 2 * btnOK.Width;
  btnOK.Top := ClientHeight - 5 - btnOK.Height;
  btnOK.Anchors := [akRight, akBottom];
  btnCancel := TButton.Create(Self);
  btnCancel.Parent := Self;
  btnCancel.ModalResult := mrCancel;
  btnCancel.Cancel := True;
  btnCancel.Caption := RsButtonCancelCaption;
  btnCancel.Left := ClientWidth - 10 - btnCancel.Width;
  btnCancel.Top := ClientHeight - 5 - btnCancel.Height;
  btnCancel.Anchors := [akRight, akBottom];
  Constraints.MinWidth := 2 * btnOK.Width + 25 + (Width - ClientWidth);
  Constraints.MinHeight := (ClientHeight - mm.ClientHeight) + 43 + (Height - ClientHeight);
end;

procedure TSLEditorForm.MemoChanged(Sender: TObject);
var
  I: Integer;
begin
  I := mm.Lines.Count;
  if I <> 1 then
    lbl.Caption := IntToStr(I) + RsXLinesCaption
  else
    lbl.Caption := RsOneLineCaption;
  if Assigned(OnContentsChanged) then
    OnContentsChanged(Sender);
end;

//=== { TJvInspectorTStringsItem } ===========================================

constructor TJvInspectorTStringsItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  RowSizing.MinHeight := irsItemHeight;
  Flags := Flags + [iifEditButton];
end;

procedure TJvInspectorTStringsItem.ContentsChanged(Sender: TObject);
var
  Obj: TStrings;
begin
  Obj := TStrings(Data.AsOrdinal);
  Obj.Text := TMemo(Sender).Lines.Text;
end;

function TJvInspectorTStringsItem.GetDisplayValue: string;
var
  Obj: TObject;
begin
  Obj := TObject(Data.AsOrdinal);
  if not Multiline then
  begin
    if Obj <> nil then
      Result := Result + '(' + Obj.ClassName + ')'
    else
      Result := Result + '(' + GetTypeData(Data.TypeInfo).ClassType.ClassName + ')';
  end
  else
    Result := TStrings(Obj).Text;
end;

procedure TJvInspectorTStringsItem.Edit;
var
  SL: TStrings;
begin
  with TSLEditorForm.CreateNew(Inspector) do
  try
    SL := TStrings(Data.AsOrdinal);
    mm.Lines.Assign(SL);
    if AutoUpdate then
      OnContentsChanged := ContentsChanged;
    if ShowModal = mrOK then
      SL.Assign(mm.Lines);
  finally
    Free;
  end;
end;

procedure TJvInspectorTStringsItem.SetDisplayValue(const Value: string);
var
  Obj: TObject;
begin
  if Multiline then
  begin
    Obj := TObject(Data.AsOrdinal);
    TStrings(Obj).Text := Value;
  end;
end;

procedure TJvInspectorTStringsItem.SetFlags(const Value: TInspectorItemFlags);
var
  OldMask: TInspectorItemFlags;
  NewMask: TInspectorItemFlags;
begin
  { The item has either an edit button or is multiline. If one of them is set,
    the other one will be removed }
  OldMask := Flags * [iifEditButton, iifMultiLine];
  NewMask := Value * [iifEditButton, iifMultiLine];
  if OldMask <> NewMask then
  begin
    if Multiline and not (iifEditButton in OldMask) and (iifEditButton in NewMask) then
      inherited SetFlags(Value - [iifMultiLine]) // iifEditButton has changed
    else
    if not Multiline and (iifEditButton in OldMask) and (iifMultiLine in NewMask) then
      inherited SetFlags(Value - [iifEditButton]) // iifMultiLine has changed
    else
      inherited SetFlags(Value); // Neither flag has changed. Should never occur.
  end
  else // Flags have not changed
    inherited SetFlags(Value);
  if RowSizing <> nil then
  begin
    RowSizing.Sizable := Multiline; // Update sizable state
    if not Multiline then
      RowSizing.SizingFactor := irsNoReSize
    else
      RowSizing.SizingFactor := irsValueHeight;
  end;
end;

//=== { TInstanceItem } ======================================================

type
  TInstanceItem = class(TObject)
  public
    Instance: TObject;
    Methods: TStrings;
    MethodStartIdx: Integer;
    Item: TJvInspectorTMethodItem;
    constructor Create;
    destructor Destroy; override;
    procedure AddMethod(const Name: string; const MethodAddr: Pointer);
    //    procedure DeleteMethod(const Name: string); overload;
    //    procedure DeleteMethod(const MethodAddr: Pointer); overload;
    procedure DeleteMethod(const Index: Integer); overload;
    procedure Clear;
    function IndexOf(const Name: string): Integer; overload;
    function IndexOf(const MethodAddr: Pointer): Integer; overload;
  end;

constructor TInstanceItem.Create;
begin
  inherited Create;
  Methods := TStringList.Create;
end;

destructor TInstanceItem.Destroy;
begin
  Methods.Free;
  inherited Destroy;
end;

procedure TInstanceItem.AddMethod(const Name: string; const MethodAddr: Pointer);
var
  I: Integer;
begin
  I := Methods.IndexOf(Name);
  if I = -1 then
  begin
    I := Methods.IndexOfObject(TObject(MethodAddr));
    if I = -1 then
    begin
      Methods.AddObject(Name, TObject(MethodAddr));
      I := Item.FList.IndexOfObject(Self) + 1;
      while I < Item.InstanceCount do
      begin
        Inc(TInstanceItem(Item.FList.Objects[I]).MethodStartIdx);
        Inc(I);
      end;
    end
    else
      Methods[I] := Name;
  end
  else
    Methods.Objects[I] := TObject(MethodAddr);
end;

(* make Delphi 5 compiler happy // andreas
procedure TInstanceItem.DeleteMethod(const Name: string);
var
  I: Integer;
begin
  I := Methods.IndexOf(Name);
  if I <> -1 then
    DeleteMethod(I);
end;

procedure TInstanceItem.DeleteMethod(const MethodAddr: Pointer);
var
  I: Integer;
begin
  I := Methods.IndexOfObject(TObject(MethodAddr));
  if I <> -1 then
    DeleteMethod(I);
end;
*)

procedure TInstanceItem.DeleteMethod(const Index: Integer);
begin
  Methods.Delete(Index);
end;

procedure TInstanceItem.Clear;
begin
  Methods.Clear;
end;

function TInstanceItem.IndexOf(const Name: string): Integer;
begin
  Result := Methods.IndexOf(Name);
end;

function TInstanceItem.IndexOf(const MethodAddr: Pointer): Integer;
begin
  Result := Methods.IndexOfObject(TObject(MethodAddr));
end;

//=== { TJvInspectorTMethodItem } ============================================

function TJvInspectorTMethodItem.GetInstanceCount: Integer;
begin
  Result := FList.Count;
end;

function TJvInspectorTMethodItem.GetInstances(I: Integer): TObject;
begin
  Result := TInstanceItem(FList.Objects[I]).Instance;
end;

function TJvInspectorTMethodItem.GetInstanceNames(I: Integer): string;
begin
  Result := FList[I];
end;

function TJvInspectorTMethodItem.GetItemTMethodFlags: TInspectorTMethodFlags;
begin
  Result := FItemTMethodFlags;
end;

function TJvInspectorTMethodItem.GetKeepFirstInstanceAsFirst: Boolean;
begin
  Result := imfKeepFirstInstanceAsFirst in FItemTMethodFlags;
end;

function TJvInspectorTMethodItem.GetMethodCount(Instance: TObject): Integer;
begin
  Result := IndexOfInstance(Instance);
  if Result > -1 then
    Result := TInstanceItem(FList.Objects[Result]).Methods.Count
  else
    Result := 0;
end;

function TJvInspectorTMethodItem.GetMethods(Instance: TObject; I: Integer): TMethod;
var
  Idx: Integer;
begin
  Idx := IndexOfInstance(Instance);
  if Idx > -1 then
  begin
    Result.Data := Instance;
    Result.Code := TInstanceItem(FList.Objects[Idx]).Methods.Objects[I];
  end;
end;

function TJvInspectorTMethodItem.GetMethodNames(Instance: TObject; I: Integer): string;
var
  Idx: Integer;
begin
  Idx := IndexOfInstance(Instance);
  if Idx > -1 then
    Result := TInstanceItem(FList.Objects[Idx]).Methods[I];
end;

function TJvInspectorTMethodItem.GetNoShowFirstInstanceName: Boolean;
begin
  Result := imfNoShowFirstInstanceName in FItemTMethodFlags;
end;

function TJvInspectorTMethodItem.GetShowInstanceNames: Boolean;
begin
  Result := imfShowInstanceNames in FItemTMethodFlags;
end;

function TJvInspectorTMethodItem.GetSortMethods: Boolean;
begin
  Result := imfSortMethods in FItemTMethodFlags;
end;

function TJvInspectorTMethodItem.GetSortInstances: Boolean;
begin
  Result := imfSortInstances in FItemTMethodFlags;
end;

procedure TJvInspectorTMethodItem.SetItemTMethodFlags(Value: TInspectorTMethodFlags);
begin
  if ItemTMethodFlags <> Value then
  begin
    FItemTMethodFlags := Value;
    InvalidateMetaData;
  end;
end;

procedure TJvInspectorTMethodItem.SetKeepFirstInstanceAsFirst(Value: Boolean);
begin
  if Value then
    ItemTMethodFlags := ItemTMethodFlags + [imfKeepFirstInstanceAsFirst]
  else
    ItemTMethodFlags := ItemTMethodFlags - [imfKeepFirstInstanceAsFirst];
end;

procedure TJvInspectorTMethodItem.SetNoShowFirstInstanceName(Value: Boolean);
begin
  if Value then
    ItemTMethodFlags := ItemTMethodFlags + [imfNoShowFirstInstanceName]
  else
    ItemTMethodFlags := ItemTMethodFlags - [imfNoShowFirstInstanceName];
end;

procedure TJvInspectorTMethodItem.SetShowInstanceNames(Value: Boolean);
begin
  if Value then
    ItemTMethodFlags := ItemTMethodFlags + [imfShowInstanceNames]
  else
    ItemTMethodFlags := ItemTMethodFlags - [imfShowInstanceNames];
end;

procedure TJvInspectorTMethodItem.SetSortMethods(Value: Boolean);
begin
  if Value then
    ItemTMethodFlags := ItemTMethodFlags + [imfSortMethods]
  else
    ItemTMethodFlags := ItemTMethodFlags - [imfSortMethods];
end;

procedure TJvInspectorTMethodItem.SetSortInstances(Value: Boolean);
begin
  if Value then
    ItemTMethodFlags := ItemTMethodFlags + [imfSortInstances]
  else
    ItemTMethodFlags := ItemTMethodFlags - [imfSortInstances];
end;

procedure TJvInspectorTMethodItem.AddInstancePrim(const Instance: TObject;
  const InstanceName: string);
var
  IdxInst: Integer;
  IdxName: Integer;
begin
  IdxInst := IndexOfInstance(Instance);
  IdxName := IndexOfInstance(InstanceName);
  if (IdxInst <> -1) and (IdxInst <> IdxName) then
    raise EJvInspectorItem.CreateRes(@RsEInstanceAlreadyExists);
  if (IdxName <> -1) and (IdxInst <> IdxName) then
    raise EJvInspectorItem.CreateRes(@RsENameAlreadyExistsForInstance);
  if IdxInst = -1 then
  begin
    IdxInst := FList.AddObject(InstanceName, TInstanceItem.Create);
    TInstanceItem(FList.Objects[IdxInst]).Instance := Instance;
    TInstanceItem(FList.Objects[IdxInst]).Item := Self;
  end;
end;

procedure TJvInspectorTMethodItem.AddMethodPrim(const Instance: TObject;
  const MethodAddr: Pointer; const MethodName: string);
var
  InstIdx: Integer;
  InstItem: TInstanceItem;
  MethodIdx: Integer;
  MethodNameIdx: Integer;
begin
  InstIdx := IndexOfInstance(Instance);
  if InstIdx = -1 then
    raise EJvInspectorItem.CreateRes(@RsEInstanceNonexistent);
  InstItem := TInstanceItem(FList.Objects[InstIdx]);
  MethodIdx := InstItem.IndexOf(MethodAddr);
  MethodNameIdx := InstItem.IndexOf(MethodName);
  if (MethodIdx <> -1) and (MethodNameIdx <> MethodIdx) then
    raise EJvInspectorItem.CreateRes(@RsEMethodAlreadyExists);
  if (MethodNameIdx <> -1) and (MethodNameIdx <> MethodIdx) then
    raise EJvInspectorItem.CreateRes(@RsENameAlreadyExistsForMethod);
  if MethodIdx = -1 then
    InstItem.AddMethod(MethodName, MethodAddr);
end;

function TJvInspectorTMethodItem.MethodFromName(const Name: string): TMethod;
var
  IPeriod: Integer;
  InstIdx: Integer;
  MethodIdx: Integer;
begin
  IPeriod := Pos('.', Name);
  if IPeriod > 0 then
    InstIdx := IndexOfInstance(Copy(Name, 1, IPeriod - 1))
  else
    InstIdx := 0;
  if InstIdx < 0 then
  begin
    Result.Data := nil;
    Result.Code := nil;
  end
  else
  begin
    MethodIdx := IndexOfMethod(InstIdx, Copy(Name, IPeriod + 1, Length(Name) - IPeriod));
    if MethodIdx < 0 then
    begin
      Result.Data := nil;
      Result.Code := nil;
    end
    else
      Result := Methods[Instances[InstIdx], MethodIdx];
  end;
end;

function TJvInspectorTMethodItem.MethodFromAbsIndex(const Idx: Integer): TMethod;
var
  InstIdx: Integer;
  InstItem: TInstanceItem;
begin
  Result.Data := nil;
  Result.Code := nil;
  InstIdx := InstanceCount - 1;
  repeat
    InstItem := TInstanceItem(FList.Objects[InstIdx]);
    if InstItem.MethodStartIdx <= Idx then
    begin
      Result.Data := InstItem.Instance;
      Result.Code := InstItem.Methods.Objects[Idx - InstItem.MethodStartIdx];
      Break;
    end;
  until False;
end;

function TJvInspectorTMethodItem.NameFromMethod(const Method: TMethod): string;
var
  Instance: TObject;
  InstanceIdx: Integer;
  MethodIdx: Integer;
begin
  Instance := Method.Data;
  InstanceIdx := IndexOfInstance(Instance);
  MethodIdx := IndexOfMethod(Method);
  Result := '';
  if (InstanceIdx <> -1) and (MethodIdx <> -1) then
  begin
    if ShowInstanceNames and ((InstanceIdx > 0) or not NoShowFirstInstanceName) then
      Result := InstanceNames[InstanceIdx] + '.';
    Result := Result + MethodNames[Instance, MethodIdx];
  end;
end;

function TJvInspectorTMethodItem.AbsIndexFromMethod(const Method: TMethod): Integer;
var
  InstIdx: Integer;
  MethodIdx: Integer;
begin
  InstIdx := IndexOfInstance(TObject(Method.Data));
  if InstIdx > -1 then
  begin
    MethodIdx := TInstanceItem(FList.Objects[InstIdx]).IndexOf(Method.Code);
    if MethodIdx > -1 then
      Result := TInstanceItem(FList.Objects[InstIdx]).MethodStartIdx + MethodIdx
    else
      Result := -1;
  end
  else
    Result := -1;
end;

function TJvInspectorTMethodItem.GetDisplayValue: string;
begin
  if Data.SupportsMethodPointers then
    Result := NameFromMethod(Data.AsMethod)
  else
    Result := Data.GetAsString;
end;

procedure TJvInspectorTMethodItem.GetValueList(const Strings: TStrings);
var
  SL: TStringList;
  InstanceList: TStringList;
  I: Integer;
  CurInstance: TInstanceItem;
  PrefixWithInstance: string;
  J: Integer;
begin
  SL := TStringList.Create;
  try
    InstanceList := TStringList.Create;
    try
      for I := 0 to InstanceCount - 1 do
        InstanceList.AddObject(InstanceNames[I], FList.Objects[I]);
      if SortInstances then
        InstanceList.Sort;
      if (InstanceCount > 0) and KeepFirstInstanceAsFirst then
      begin
        I := InstanceList.IndexOfObject(FList.Objects[0]);
        if I > 0 then
        begin
          InstanceList.Delete(I);
          InstanceList.InsertObject(0, InstanceNames[0], FList.Objects[0]);
        end;
      end;
      for I := 0 to InstanceCount - 1 do
      begin
        SL.Clear;
        CurInstance := TInstanceItem(InstanceList.Objects[I]);
        if ShowInstanceNames and ((I > 0) or not NoShowFirstInstanceName) then
          PrefixWithInstance := InstanceList[I] + '.'
        else
          PrefixWithInstance := '';
        for J := 0 to CurInstance.Methods.Count - 1 do
          SL.AddObject(PrefixWithInstance + CurInstance.Methods[J], TObject(CurInstance.MethodStartIdx + J));
        if SL.Count > 0 then
        begin
          if SortMethods then
            SL.Sort;
          Strings.AddStrings(SL);
        end;
      end;
      SL.Clear;
      inherited GetValueList(SL);
      if SortMethods then
        SL.Sort;
      if SL.Count > 0 then
        Strings.AddStrings(SL);
    finally
      InstanceList.Free;
    end;
  finally
    SL.Free;
  end;
end;

procedure TJvInspectorTMethodItem.SetDisplayValue(const Value: string);
var
  M: TMethod;
begin
  M := MethodFromName(Value);
  if Data.SupportsMethodPointers then
    Data.AsMethod := M
  else
    Data.AsString := NameFromMethod(M);
end;

procedure TJvInspectorTMethodItem.SetFlags(const Value: TInspectorItemFlags);
begin
  inherited SetFlags(Value + [iifValueList]);
end;

constructor TJvInspectorTMethodItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  FList := TStringList.Create;
  ItemTMethodFlags := [imfShowInstanceNames, imfNoShowFirstInstanceName,
    imfKeepFirstInstanceAsFirst, imfSortInstances, imfSortMethods];
end;

procedure TJvInspectorTMethodItem.BeforeDestruction;
begin
  ClearInstances;
  FreeAndNil(FList);
  inherited BeforeDestruction;
end;

procedure TJvInspectorTMethodItem.AddInstance(const Instance: TObject; const InstanceName: string);
begin
  AddInstancePrim(Instance, InstanceName);
end;

procedure TJvInspectorTMethodItem.AddMethod(const Method: TMethod; const MethodName: string);
begin
  AddMethodPrim(TObject(Method.Data), Method.Code, MethodName);
end;

procedure TJvInspectorTMethodItem.AddMethod(const Instance: TObject; MethodAddr: Pointer;
  const MethodName: string);
begin
  AddMethodPrim(Instance, MethodAddr, MethodName);
end;

procedure TJvInspectorTMethodItem.DeleteInstance(const Index: Integer);
var
  InstItem: TInstanceItem;
begin
  InstItem := TInstanceItem(FList.Objects[Index]);
  InstItem.Free;
  FList.Delete(Index);
end;

procedure TJvInspectorTMethodItem.DeleteInstance(const Instance: TObject);
var
  Idx: Integer;
begin
  Idx := IndexOfInstance(Instance);
  if Idx > -1 then
    DeleteInstance(Idx)
  else
    raise EJvInspectorItem.CreateRes(@RsEInstanceNonexistent);
end;

procedure TJvInspectorTMethodItem.DeleteInstance(const InstanceName: string);
var
  Idx: Integer;
begin
  Idx := IndexOfInstance(InstanceName);
  if Idx > -1 then
    DeleteInstance(Idx)
  else
    raise EJvInspectorItem.CreateResFmt(@RsENamedInstanceNonexistent, [InstanceName]);
end;

procedure TJvInspectorTMethodItem.DeleteMethod(const Method: TMethod);
var
  InstIdx: Integer;
  InstItem: TInstanceItem;
  MethodIdx: Integer;
begin
  InstIdx := IndexOfInstance(TObject(Method.Data));
  if InstIdx > -1 then
  begin
    InstItem := TInstanceItem(FList.Objects[InstIdx]);
    MethodIdx := InstItem.IndexOf(Method.Code);
    if MethodIdx > -1 then
      InstItem.DeleteMethod(MethodIdx)
    else
      raise EJvInspectorItem.CreateRes(@RsEMethodNonexistent);
  end
  else
    raise EJvInspectorItem.CreateRes(@RsEInstanceNonexistent);
end;

procedure TJvInspectorTMethodItem.DeleteMethod(const InstanceIndex: Integer; const Index: Integer);
begin
  TInstanceItem(FList.Objects[InstanceIndex]).DeleteMethod(Index);
end;

procedure TJvInspectorTMethodItem.DeleteMethod(const Instance: TObject; const Index: Integer);
var
  InstIdx: Integer;
begin
  InstIdx := IndexOfInstance(Instance);
  if InstIdx > -1 then
    DeleteMethod(InstIdx, Index)
  else
    raise EJvInspectorItem.CreateRes(@RsEInstanceNonexistent);
end;

procedure TJvInspectorTMethodItem.DeleteMethod(const InstanceName: string; const Index: Integer);
var
  InstIdx: Integer;
begin
  InstIdx := IndexOfInstance(InstanceName);
  if InstIdx > -1 then
    DeleteMethod(InstIdx, Index)
  else
    raise EJvInspectorItem.CreateResFmt(@RsENamedInstanceNonexistent, [InstanceName]);
end;

procedure TJvInspectorTMethodItem.DeleteMethod(const InstanceIndex: Integer; const MethodName: string);
var
  MethodIdx: Integer;
begin
  MethodIdx := TInstanceItem(FList.Objects[InstanceIndex]).IndexOf(MethodName);
  if MethodIdx > -1 then
    DeleteMethod(InstanceIndex, MethodIdx)
  else
    raise EJvInspectorItem.CreateResFmt(@RsENamedMethodNonexistent, [MethodName]);
end;

procedure TJvInspectorTMethodItem.DeleteMethod(const Instance: TObject; const MethodName: string);
var
  InstIdx: Integer;
begin
  InstIdx := IndexOfInstance(Instance);
  if InstIdx > -1 then
    DeleteMethod(InstIdx, MethodName)
  else
    raise EJvInspectorItem.CreateRes(@RsEInstanceNonexistent);
end;

procedure TJvInspectorTMethodItem.DeleteMethod(const InstanceName: string; const MethodName: string);
var
  InstIdx: Integer;
begin
  InstIdx := IndexOfInstance(InstanceName);
  if InstIdx > -1 then
    DeleteMethod(InstIdx, MethodName)
  else
    raise EJvInspectorItem.CreateResFmt(@RsENamedInstanceNonexistent, [InstanceName]);
end;

procedure TJvInspectorTMethodItem.ClearInstances;
var
  I: Integer;
begin
  for I := InstanceCount - 1 downto 0 do
    DeleteInstance(I);
end;

procedure TJvInspectorTMethodItem.ClearMethods(const InstanceIndex: Integer);
begin
  TInstanceItem(FList.Objects[InstanceIndex]).Clear;
end;

procedure TJvInspectorTMethodItem.ClearMethods(const Instance: TObject);
var
  InstIdx: Integer;
begin
  InstIdx := IndexOfInstance(Instance);
  if InstIdx > -1 then
    ClearMethods(InstIdx)
  else
    raise EJvInspectorItem.CreateRes(@RsEInstanceNonexistent);
end;

procedure TJvInspectorTMethodItem.ClearMethods(const InstanceName: string);
var
  InstIdx: Integer;
begin
  InstIdx := IndexOfInstance(InstanceName);
  if InstIdx > -1 then
    ClearMethods(InstIdx)
  else
    raise EJvInspectorItem.CreateResFmt(@RsENamedInstanceNonexistent, [InstanceName]);
end;

function TJvInspectorTMethodItem.IndexOfInstance(const Instance: TObject): Integer;
begin
  Result := InstanceCount - 1;
  while (Result >= 0) and (TInstanceItem(FList.Objects[Result]).Instance <> Instance) do
    Dec(Result);
end;

function TJvInspectorTMethodItem.IndexOfInstance(const InstanceName: string): Integer;
begin
  Result := InstanceCount - 1;
  while (Result >= 0) and not AnsiSameText(FList[Result], InstanceName) do
    Dec(Result);
end;

function TJvInspectorTMethodItem.IndexOfMethod(const Method: TMethod): Integer;
begin
  Result := IndexOfInstance(TObject(Method.Data));
  if Result > -1 then
    Result := TInstanceItem(FList.Objects[Result]).IndexOf(Method.Code);
end;

function TJvInspectorTMethodItem.IndexOfMethod(const InstanceIndex: Integer; const MethodName: string): Integer;
begin
  Result := TInstanceItem(FList.Objects[InstanceIndex]).IndexOf(MethodName);
end;

function TJvInspectorTMethodItem.IndexOfMethod(const Instance: TObject; const MethodName: string): Integer;
begin
  Result := IndexOfInstance(Instance);
  if Result > -1 then
    Result := IndexOfMethod(Result, MethodName);
end;

function TJvInspectorTMethodItem.IndexOfMethod(const InstanceName: string; const MethodName: string): Integer;
begin
  Result := IndexOfInstance(InstanceName);
  if Result > -1 then
    Result := IndexOfMethod(Result, MethodName);
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

procedure TJvCustomInspectorData.DoDataChanged;
begin
  if Assigned(FOnValueChanged) then
    FOnValueChanged(Self);
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

procedure TJvCustomInspectorData.InvalidateData;
var
  InspList: TList;
  I: Integer;
begin
  InspList := TList.Create;
  try
    // Fill list with unique inspector instances for this data instance
    for I := Low(FItems) to High(FItems) do
      if (FItems[I].Inspector <> nil) and (InspList.IndexOf(FItems[I].Inspector) = -1) then
        InspList.Add(FItems[I].Inspector);
    // Generate data changed event on this data instance
    DoDataChanged;
    // Generate data changed events on the inspectors that have a link to this data instance
    for I := 0 to InspList.Count - 1 do
      TJvCustomInspector(InspList[I]).DoDataValueChanged(Self);

    // Generate item changed events for all items for this data instance
    for I := Low(FItems) to High(FItems) do
      FItems[I].InvalidateValue;
  finally
    InspList.Free;
  end;
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
  AParent.Inspector.DoAfterDataCreate(Self);
  RegItem := ItemRegister.FindMatch(Self);
  if RegItem <> nil then
  begin
    ItemClass := RegItem.ItemClass;
    AParent.Inspector.DoBeforeItemCreate(Self, ItemClass);
    if ItemClass <> nil then
    begin
      Result := ItemClass.Create(AParent, Self);
      if Result <> nil then
      begin
        RegItem.ApplyDefaults(Result);
        SetLength(FItems, Length(FItems) + 1);
        FItems[High(FItems)] := Result;
        Result.InvalidateMetaData;
        Result.DoAfterItemCreate;
      end;
    end;
  end;
end;

//=== { TJvInspectorVarData } ================================================

function TJvInspectorVarData.GetAddress: Pointer;
begin
  Result := FAddress;
end;

function TJvInspectorVarData.GetAsFloat: Extended;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkFloat then
    case GetTypeData(TypeInfo).FloatType of
      ftSingle:
        Result := PSingle(Address)^;
      ftDouble:
        Result := PDouble(Address)^;
      ftExtended:
        Result := PExtended(Address)^;
      ftComp:
        Result := PComp(Address)^;
      ftCurr:
        Result := PCurrency(Address)^;
    else
      Result := 0;
    end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
end;

function TJvInspectorVarData.GetAsInt64: Int64;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkInt64 then
    Result := PInt64(Address)^
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
end;

function TJvInspectorVarData.GetAsMethod: TMethod;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkMethod then
    Result := PMethod(Address)^
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
end;

function TJvInspectorVarData.GetAsOrdinal: Int64;
begin
  CheckReadAccess;
  if TypeInfo.Kind in [tkInteger, tkChar, tkEnumeration, tkSet, tkWChar] then
  begin
    case GetTypeData(TypeInfo).OrdType of
      otSByte:
        Result := PShortint(Address)^;
      otUByte:
        Result := PByte(Address)^;
      otSWord:
        Result := PSmallint(Address)^;
      otUWord:
        Result := PWord(Address)^;
      otSLong:
        Result := PLongint(Address)^;
      otULong:
        Result := PLongword(Address)^;
    else
      Result := 0;
    end;
  end
  else
  if TypeInfo.Kind = tkClass then
    Result := PLongword(Address)^
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
end;

function TJvInspectorVarData.GetAsString: string;
begin
  CheckReadAccess;
  if TypeInfo.Kind in tkStrings then
  begin
    case TypeInfo.Kind of
      {$IFDEF UNICODE}
      tkUString:
        Result := PUnicodeString(Address)^;
      {$ENDIF UNICODE}
      tkLString:
        Result := string(PAnsiString(Address)^);
      tkWString:
        Result := PWideString(Address)^;
      tkString:
        Result := string(PShortString(Address)^);
    else
      Result := '';
    end;
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
end;

function TJvInspectorVarData.GetAsVariant: Variant;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkVariant then
  begin
    Result := PVariant(Address)^;
  end
  else
  begin
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorVariant]);
  end;
end;

function TJvInspectorVarData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := (Ref is TJvInspectorVarData) and (TJvInspectorVarData(Ref).Address = Address);
end;

procedure TJvInspectorVarData.SetAddress(const Value: Pointer);
begin
  if Value <> Address then
  begin
    FAddress := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorVarData.SetAsFloat(const Value: Extended);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkFloat then
  begin
    case GetTypeData(TypeInfo).FloatType of
      ftSingle:
        PSingle(Address)^ := Value;
      ftDouble:
        PDouble(Address)^ := Value;
      ftExtended:
        PExtended(Address)^ := Value;
      ftComp:
        PComp(Address)^ := Value;
      ftCurr:
        PCurrency(Address)^ := Value;
    end;
    InvalidateData;
    Invalidate;
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
end;

procedure TJvInspectorVarData.SetAsInt64(const Value: Int64);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkInt64 then
  begin
    if (Value < GetTypeData(TypeInfo).MinInt64Value) or (Value > GetTypeData(TypeInfo).MaxInt64Value) then
      raise ERangeError.CreateResFmt(@SOutOfRange,
        [GetTypeData(TypeInfo).MinValue, GetTypeData(TypeInfo).MaxValue]);
    PInt64(Address)^ := Value;
    InvalidateData;
    Invalidate;
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
end;

procedure TJvInspectorVarData.SetAsMethod(const Value: TMethod);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkMethod then
    PMethod(Address)^ := Value
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
  InvalidateData;
  Invalidate;
end;

procedure TJvInspectorVarData.SetAsOrdinal(const Value: Int64);
var
  MinValue: Int64;
  MaxValue: Int64;
begin
  CheckWriteAccess;
  if TypeInfo.Kind in [tkInteger, tkChar, tkEnumeration, tkWChar] then
  begin
    case GetTypeData(TypeInfo).OrdType of
      otSByte:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          PShortint(Address)^ := Value;
        end;
      otUByte:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          PByte(Address)^ := Value;
        end;
      otSWord:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          PSmallint(Address)^ := Value;
        end;
      otUWord:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          PWord(Address)^ := Value;
        end;
      otSLong:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          PLongint(Address)^ := Value;
        end;
      otULong:
        begin
          MinValue := Longword(GetTypeData(TypeInfo).MinValue);
          MaxValue := Longword(GetTypeData(TypeInfo).MaxValue);
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          PLongword(Address)^ := Value;
        end;
    end;
  end
  else
  if TypeInfo.Kind = tkClass then
    PLongword(Address)^ := Value
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
  InvalidateData;
  Invalidate;
end;

procedure TJvInspectorVarData.SetAsString(const Value: string);
begin
  CheckWriteAccess;
  if TypeInfo.Kind in tkStrings then
  begin
    case TypeInfo.Kind of
      {$IFDEF UNICODE}
      tkUString:
        PUnicodeString(Address)^ := Value;
      {$ENDIF UNICODE}
      tkLString:
        PAnsiString(Address)^ := AnsiString(Value);
      tkWString:
        PWideString(Address)^ := Value;
      tkString:
        if Length(Value) < GetTypeData(TypeInfo).MaxLength then
          PShortString(Address)^ := AnsiString(Value)
        else
          raise EJvInspectorData.CreateRes(@RsEJVInspDataStrTooLong);
    end;
    InvalidateData;
    Invalidate;
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
end;

procedure TJvInspectorVarData.SetAsVariant(const Value: Variant);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkVariant then
  begin
    PVariant(Address)^ := Value;
  end
  else
  begin
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorVariant]);
  end;
end;

function TJvInspectorVarData.SupportsMethodPointers: Boolean;
begin
  Result := True;
end;

procedure TJvInspectorVarData.GetAsSet(var Buf);
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
    Move(PAnsiChar(Address)[0], Buf, ResBytes);
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorSet]);
end;

function TJvInspectorVarData.HasValue: Boolean;
begin
  // Cannot use AsVariant, it calls HasValue
  Result := IsInitialized and
            ((TypeInfo.Kind <> tkVariant) or
             not VarIsNull(PVariant(Address)^));
end;

function TJvInspectorVarData.IsAssigned: Boolean;
begin
  // Cannot use AsVariant, it calls IsAssigned
  Result := IsInitialized and
            ((TypeInfo.Kind <> tkVariant) or
             not VarIsEmpty(PVariant(Address)^));
end;

function TJvInspectorVarData.IsInitialized: Boolean;
begin
  Result := (TypeInfo <> nil) and (Address <> nil);
end;

class function TJvInspectorVarData.ItemRegister: TJvInspectorRegister;
begin
  if GlobalVarItemReg = nil then
    GlobalVarItemReg := TJvInspectorRegister.Create(TJvInspectorVarData);
  Result := GlobalVarItemReg;
end;

class function TJvInspectorVarData.New(const AParent: TJvCustomInspectorItem;
  const AName: string; ATypeInfo: PTypeInfo; const AAddress: Pointer): TJvCustomInspectorItem;
var
  Data: TJvInspectorVarData;
begin
  Data := CreatePrim(AName, ATypeInfo);
  Data.FAddress := AAddress;
  Data := TJvInspectorVarData(DataRegister.Add(Data));
  if Data <> nil then
    Result := Data.NewItem(AParent)
  else
    Result := nil;
end;

(* **REMOVED BECAUSE IT CREATES AN OVERLOADED SITUATION THAT IS INCOMPATIBLE WITH BCB.
   **USE @Var instead when invoking the other method, if you get compilation errors.
  class function TJvInspectorVarData.New(const AParent: TJvCustomInspectorItem; const AName: string; const ATypeInfo: PTypeInfo; const AVar): TJvCustomInspectorItem;
begin
  Result := New(AParent, AName, ATypeInfo, Addr(AVar));
end;
*)

procedure TJvInspectorVarData.SetAsSet(const Buf);
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
    Move(Buf, PAnsiChar(Address)[0], ResBytes);
    InvalidateData;
    Invalidate;
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorSet]);
end;

//=== { TJvInspectorPropData } ===============================================

function TJvInspectorPropData.GetAsFloat: Extended;
begin
  CheckReadAccess;
  if Prop.PropType^.Kind = tkFloat then
    Result := GetFloatProp(Instance, Prop)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
end;

function TJvInspectorPropData.GetAsInt64: Int64;
begin
  CheckReadAccess;
  if Prop.PropType^.Kind = tkInt64 then
    Result := GetInt64Prop(Instance, Prop)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
end;

function TJvInspectorPropData.GetAsMethod: TMethod;
begin
  CheckReadAccess;
  if Prop.PropType^.Kind = tkMethod then
    Result := GetMethodProp(Instance, Prop)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
end;

function TJvInspectorPropData.GetAsOrdinal: Int64;
begin
  CheckReadAccess;
  if Prop.PropType^.Kind in [tkInteger, tkChar, tkEnumeration, tkSet,
    tkWChar, tkClass] then
  begin
    if GetTypeData(Prop.PropType^).OrdType = otULong then
      Result := Cardinal(GetOrdProp(Instance, Prop))
    else
      Result := GetOrdProp(Instance, Prop);
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
end;

function TJvInspectorPropData.GetAsString: string;
begin
  CheckReadAccess;
  if Prop.PropType^.Kind in tkStrings then
    Result := GetStrProp(Instance, Prop)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
end;

function TJvInspectorPropData.GetAsVariant: Variant;
begin
  CheckReadAccess;
  if Prop.PropType^.Kind = tkVariant then
    Result := GetVariantProp(Instance, Prop)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorVariant]);
end;

function TJvInspectorPropData.GetInstance: TObject;
begin
  Result := FInstance;
end;

function TJvInspectorPropData.GetProp: PPropInfo;
begin
  Result := FProp;
end;

function TJvInspectorPropData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := (Ref is TJvInspectorPropData) and (TJvInspectorPropData(Ref).Instance = Instance) and
    (TJvInspectorPropData(Ref).Prop = Prop);
end;

procedure TJvInspectorPropData.NotifyRemoveData(const Instance: TJvCustomInspectorData);
begin
// The following is commented out due to Mantis #3348:
//  if (Instance <> nil) and (Instance <> Self) and (Instance.TypeInfo.Kind = tkClass) and
//    (TObject(Instance.AsOrdinal) = Self.Instance) then
//    Free;
end;

procedure TJvInspectorPropData.SetAsFloat(const Value: Extended);
begin
  CheckWriteAccess;
  if IsReadOnlyProperty then
    Abort;
  if Prop.PropType^.Kind = tkFloat then
    SetFloatProp(Instance, Prop, Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
  InvalidateData;
  Invalidate;
end;

procedure TJvInspectorPropData.SetAsInt64(const Value: Int64);
begin
  CheckWriteAccess;
  if IsReadOnlyProperty then
    Abort;
  if Prop.PropType^.Kind = tkInt64 then
    SetInt64Prop(Instance, Prop, Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
  InvalidateData;
  Invalidate;
end;

procedure TJvInspectorPropData.SetAsMethod(const Value: TMethod);
begin
  CheckWriteAccess;
  if IsReadOnlyProperty then
    Abort;
  if Prop.PropType^.Kind = tkMethod then
    SetMethodProp(Instance, Prop, Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
  InvalidateData;
  Invalidate;
end;

procedure TJvInspectorPropData.SetAsOrdinal(const Value: Int64);
begin
  CheckWriteAccess;
  if IsReadOnlyProperty then
    Abort;
  if Prop.PropType^.Kind in [tkInteger, tkChar, tkEnumeration, tkSet,
    tkWChar, tkClass] then
  begin
    if GetTypeData(Prop.PropType^).OrdType = otULong then
      SetOrdProp(Instance, Prop, Cardinal(Value))
    else
      SetOrdProp(Instance, Prop, Value);
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
  InvalidateData;
  Invalidate;
end;

procedure TJvInspectorPropData.SetAsString(const Value: string);
begin
  CheckWriteAccess;
  if IsReadOnlyProperty then
    Abort;
  if Prop.PropType^.Kind in tkStrings then
    SetStrProp(Instance, Prop, Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
  InvalidateData;
  Invalidate;
end;

procedure TJvInspectorPropData.SetAsVariant(const Value: Variant);
begin
  CheckWriteAccess;
  if IsReadOnlyProperty then
    Abort;
  if TypeInfo.Kind = tkVariant then
    SetVariantProp(Instance, Prop, Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorVariant]);
  InvalidateData;
  Invalidate;
end;

procedure TJvInspectorPropData.SetInstance(const Value: TObject);
begin
  if Instance <> Value then
  begin
    FInstance := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorPropData.SetProp(Value: PPropInfo);
begin
  if Prop <> Value then
  begin
    FProp := Value;
    TypeInfo := Value.PropType^;
    Invalidate;
  end;
end;

function TJvInspectorPropData.SupportsMethodPointers: Boolean;
begin
  Result := True;
end;

procedure TJvInspectorPropData.GetAsSet(var Buf);
var
  I: Integer;
  CompType: PTypeInfo;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
begin
  I := AsOrdinal;
  CompType := GetTypeData(TypeInfo).CompType^;
  EnumMin := GetTypeData(CompType).MinValue;
  EnumMax := GetTypeData(CompType).MaxValue;
  ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
  Move(I, Buf, ResBytes);
end;

function TJvInspectorPropData.HasValue: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorPropData.IsAssigned: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorPropData.IsInitialized: Boolean;
begin
  Result := (Instance <> nil) and (Prop <> nil);
end;

function TJvInspectorPropData.IsReadOnlyProperty: Boolean;
begin
  Result := IsInitialized and (Prop^.SetProc = nil);
end;

class function TJvInspectorPropData.ItemRegister: TJvInspectorRegister;
begin
  if GlobalPropItemReg = nil then
  begin
    GlobalPropItemReg := TJvInspectorRegister.Create(TJvInspectorPropData);
    // register
    RegisterPropDataTypeKinds;
  end;
  Result := GlobalPropItemReg;
end;

class function TJvInspectorPropData.TypeInfoMapRegister: TJvInspectorRegister;
begin
  if GlobalPropMapReg = nil then
    GlobalPropMapReg := TJvInspectorRegister.Create(TJvCustomInspectorData);
  Result := GlobalPropMapReg;
end;

class procedure TJvInspectorPropData.AddTypeMapping(Target, Source: PTypeInfo;
  ObjectClass: TClass; const PropertyName: string);
begin
  TypeInfoMapRegister.Add(TJvInspectorTypeInfoMapperRegItem.Create(ObjectClass,
    PropertyName, Source, Target));
end;

class function TJvInspectorPropData.New(const AParent: TJvCustomInspectorItem;
  const AInstance: TObject; PropInfo: PPropInfo): TJvCustomInspectorItem;
var
  Data: TJvInspectorPropData;
  RegItem: TJvCustomInspectorRegItem;
begin
  if PropInfo = nil then
    raise EJvInspectorData.CreateRes(@RsEJvAssertPropInfo);
  Data := CreatePrim({$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropInfo.Name), PropInfo.PropType^);
  Data.Instance := AInstance;
  Data.Prop := PropInfo;
  Data := TJvInspectorPropData(DataRegister.Add(Data));
  if Data <> nil then
  begin
    RegItem := TypeInfoMapRegister.FindMatch(Data);
    if (RegItem <> nil) and (RegItem is TJvInspectorTypeInfoMapperRegItem) then
      Data.TypeInfo := TJvInspectorTypeInfoMapperRegItem(RegItem).NewTypeInfo;
    Result := Data.NewItem(AParent);
  end
  else
    Result := nil;
end;

class function TJvInspectorPropData.New(const AParent: TJvCustomInspectorItem;
  const AInstance: TObject; const PropName: string): TJvCustomInspectorItem;
var
  PI: PPropInfo;
begin
  PI := GetPropInfo(AInstance, PropName, tkAny);
  if PI <> nil then
    Result := New(AParent, AInstance, PI)
  else
    Result := nil;
end;

class function TJvInspectorPropData.New(const AParent: TJvCustomInspectorItem;
  const AInstance: TObject; const TypeKinds: TTypeKinds): TJvInspectorItemInstances;
var
  PropCount: Integer;
  PropList: PPropList;
begin
  SetLength(Result, 0);

  if AInstance.ClassInfo = nil then
    raise EJvInspectorData.CreateRes(@RsEJvAssertClassInfo);

  PropCount := GetPropList(AInstance.ClassInfo, TypeKinds, nil);
  GetMem(PropList, PropCount * SizeOf(PPropInfo));
  try
    GetPropList(AInstance.ClassInfo, TypeKinds, PropList);
    Result := New(AParent, AInstance, PropList, PropCount); // Generate Items for each Property element.
  finally
    FreeMem(PropList);
  end;
end;

class function TJvInspectorPropData.NewByNames(const AParent: TJvCustomInspectorItem;
  const AInstance: TObject; const NameList: array of string;
  const ExcludeList: Boolean; const TypeKinds: TTypeKinds): TJvInspectorItemInstances;
var
  PropCount: Integer;
  PropList: PPropList;
  I: Integer;
  PropInfo: PPropInfo;
  NameIdx: Integer;
begin
  SetLength(Result, 0);

  if AInstance.ClassInfo = nil then
    raise EJvInspectorData.CreateRes(@RsEJvAssertClassInfo);

  PropCount := GetPropList(AInstance.ClassInfo, TypeKinds, nil);
  GetMem(PropList, PropCount * SizeOf(PPropInfo));
  try
    GetPropList(AInstance.ClassInfo, TypeKinds, PropList);
    for I := 0 to Pred(PropCount) do
    begin
      PropInfo := PropList[I];
      NameIdx := High(NameList);
      while (NameIdx >= 0) and not AnsiSameText(NameList[NameIdx], {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropInfo.Name)) do
        Dec(NameIdx);
      if ((NameIdx < 0) and ExcludeList) or ((NameIdx > -1) and not ExcludeList) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := New(AParent, AInstance, PropInfo);
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;

class function TJvInspectorPropData.New(const AParent: TJvCustomInspectorItem;
  const AInstance: TObject; PropInfos: PPropList;
  const PropCount: Integer): TJvInspectorItemInstances;
var
  I: Integer;
begin
  SetLength(Result, PropCount);
  for I := 0 to Pred(PropCount) do
    Result[I] := New(AParent, AInstance, PropInfos[I]);
end;

procedure TJvInspectorPropData.SetAsSet(const Buf);
begin
  AsOrdinal := Integer(Buf);
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
  InvalidateData;
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
  InvalidateData;
  Invalidate;
end;

procedure TJvInspectorEventData.SetAsMethod(const Value: TMethod);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkMethod then
    DoSetAsMethod(Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
  InvalidateData;
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
  InvalidateData;
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
  InvalidateData;
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
  InvalidateData;
  Invalidate;
end;

//=== { TJvInspectorCustomConfData } =========================================

constructor TJvInspectorCustomConfData.CreatePrim(const AName, ASection, AKey: string;
  ATypeInfo: PTypeInfo);
begin
  inherited CreatePrim(AName, ATypeInfo);
  FKey := AKey;
  FSection := ASection;
end;

function TJvInspectorCustomConfData.GetAsFloat: Extended;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkFloat then
    Result := StrToFloat(Trim(StringReplace(ReadValue, JclFormatSettings.ThousandSeparator, JclFormatSettings.DecimalSeparator,
      [rfReplaceAll, rfIgnoreCase])))
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
end;

function TJvInspectorCustomConfData.GetAsInt64: Int64;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkInt64 then
    Result := StrToInt64(ReadValue)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
end;

function TJvInspectorCustomConfData.GetAsMethod: TMethod;
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
end;

function TJvInspectorCustomConfData.GetAsOrdinal: Int64;
var
  S: string;
begin
  CheckReadAccess;
  S := ReadValue;
  case TypeInfo.Kind of
    tkInteger:
      begin
        case GetTypeData(TypeInfo).OrdType of
          otSByte:
            Result := Shortint(StrToInt(S));
          otUByte:
            Result := Byte(StrToInt(S));
          otSWord:
            Result := Smallint(StrToInt(S));
          otUWord:
            Result := Word(StrToInt(S));
          otSLong:
            Result := Longint(StrToInt(S));
          otULong:
            Result := Longword(StrToInt(S));
        else
          Result := 0;
        end;
      end;
    tkChar, tkWChar:
      begin
      if Length(S) > 1 then
        Result := StrToInt(Copy(S, 2, Length(S)))
      else
      if Length(S) = 1 then
        Result := Ord(S[1])
      else
        Result := 0;
      end;
    tkEnumeration:
      Result := GetEnumValue(TypeInfo, S);
    tkSet:
      GetAsSet(Result);
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
  end;
end;

function TJvInspectorCustomConfData.ForceString: string;
begin
  CheckReadAccess;
  Result := ReadValue;
end;

function TJvInspectorCustomConfData.GetAsString: string;
begin
  CheckReadAccess;
  if TypeInfo.Kind in tkStrings then
    Result := ReadValue
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
end;

function TJvInspectorCustomConfData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := (Ref is TJvInspectorCustomConfData) and
    AnsiSameText(TJvInspectorCustomConfData(Ref).Section, Section) and
    AnsiSameText(TJvInspectorCustomConfData(Ref).Key, Key);
end;

procedure TJvInspectorCustomConfData.SetAsFloat(const Value: Extended);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkFloat then
    WriteValue(FloatToStr(Value))
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
  InvalidateData;
  Invalidate;
end;

procedure TJvInspectorCustomConfData.SetAsInt64(const Value: Int64);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkInt64 then
    WriteValue(IntToStr(Value))
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
  InvalidateData;
  Invalidate;
end;

procedure TJvInspectorCustomConfData.SetAsMethod(const Value: TMethod);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
end;

procedure TJvInspectorCustomConfData.SetAsOrdinal(const Value: Int64);
begin
  CheckWriteAccess;
  case TypeInfo.Kind of
    tkInteger:
      WriteValue(IntToStr(Value));
    tkChar, tkWChar:
      if (Value <= Ord(' ')) or (Value > Ord('~')) then
        WriteValue('#' + IntToStr(Value))
      else
        WriteValue(Chr(Byte(Value)));
    tkEnumeration:
      WriteValue(GetEnumName(TypeInfo, Value));
    tkSet:
      SetAsSet(Value);
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
  end;
  InvalidateData;
  Invalidate;
end;

procedure TJvInspectorCustomConfData.SetAsString(const Value: string);
begin
  CheckWriteAccess;
  case TypeInfo.Kind of
    tkString:
      if Length(Value) < GetTypeData(TypeInfo).MaxLength then
        WriteValue(Value)
      else
        raise EJvInspectorData.CreateRes(@RsEJVInspDataStrTooLong);
    tkLString, {$IFDEF UNICODE} tkUString, {$ENDIF} tkWString:
      WriteValue(Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
  end;
  InvalidateData;
  Invalidate;
end;

procedure TJvInspectorCustomConfData.SetKey(const Value: string);
begin
  if Value <> Key then
  begin
    FKey := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorCustomConfData.SetSection(const Value: string);
begin
  if Value <> Section then
  begin
    FSection := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorCustomConfData.GetAsSet(var Buf);
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkSet then
    JclStrToSet(TypeInfo, Buf, ReadValue)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorSet]);
end;

function TJvInspectorCustomConfData.HasValue: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorCustomConfData.IsAssigned: Boolean;
begin
  Result := IsInitialized and ExistingValue;
end;

function TJvInspectorCustomConfData.IsInitialized: Boolean;
begin
  Result := (Key <> '') and (Section <> '');
end;

procedure TJvInspectorCustomConfData.SetAsSet(const Buf);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkSet then
    WriteValue(JclSetToStr(TypeInfo, Buf, True, False))
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorSet]);
  InvalidateData;
  Invalidate;
end;

//=== { TJvInspectorINIFileData } ============================================

function TJvInspectorINIFileData.ExistingValue: Boolean;
begin
  Result := IsInitialized and INIFile.SectionExists(Section) and INIFile.ValueExists(Section, Key);
end;

function TJvInspectorINIFileData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := (Ref is TJvInspectorINIFileData) and
    (TJvInspectorINIFileData(Ref).INIFile = INIFile) and inherited IsEqualReference(Ref);
end;

function TJvInspectorINIFileData.ReadValue: string;
begin
  Result := INIFile.ReadString(Section, Key, '');
end;

procedure TJvInspectorINIFileData.WriteValue(const Value: string);
begin
  INIFile.WriteString(Section, Key, Value);
end;

class function TJvInspectorINIFileData.New(const AParent: TJvCustomInspectorItem;
  const AName, ASection, AKey: string; ATypeInfo: PTypeInfo;
  const AINIFile: TCustomIniFile): TJvCustomInspectorItem;
var
  Data: TJvInspectorINIFileData;
begin
  if AINIFile = nil then
    raise EJvInspectorData.CreateRes(@RsEJvAssertINIFile);
  Data := CreatePrim(AName, ASection, AKey, ATypeInfo);
  Data.FINIFile := AINIFile;
  Data := TJvInspectorINIFileData(DataRegister.Add(Data));
  if Data <> nil then
    Result := Data.NewItem(AParent)
  else
    Result := nil;
end;

class function TJvInspectorINIFileData.New(const AParent: TJvCustomInspectorItem;
  const ASection: string; const AINIFile: TCustomIniFile;
  const AOnAddKey: TJvInspConfKeyEvent): TJvInspectorItemInstances;
var
  SL: TStringList;
  I: Integer;
  KeyName: string;
  KeyTypeInfo: PTypeInfo;
  TmpItem: TJvCustomInspectorItem;
  //NewFlags: TInspectorItemFlags;

  function AllowAddKey: Boolean;
  begin
    KeyName := SL[I];
    KeyTypeInfo := System.TypeInfo(string);
    Result := True;
    //NewFlags := [iifVisible];
    if Assigned(AOnAddKey) then
      AOnAddKey(ASection, KeyName, KeyTypeInfo, Result {, NewFlags} );
  end;

begin
  if AINIFile = nil then
    raise EJvInspectorData.CreateRes(@RsEJvAssertINIFile);
  SetLength(Result, 0);
  SL := TStringList.Create;
  try
    AINIFile.ReadSection(ASection, SL);
    for I := 0 to SL.Count - 1 do
    begin
      if AllowAddKey then
      begin
        TmpItem := TJvInspectorINIFileData.New(AParent, KeyName, ASection, SL[I], KeyTypeInfo,
          AINIFile);
        //TmpItem.FFlags := NewFlags;
        // XXX Warren's first attempt to make inspector items know their data's names:
        //if (TmpItem.Parent.Name <> ASection) then
        //  TmpItem.Parent.Name := ASection;
        //TmpItem.Name := KeyName;
        if TmpItem <> nil then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := TmpItem;
        end;
      end;
    end;
  finally
    SL.Free;
  end;
end;

class function TJvInspectorINIFileData.New(const AParent: TJvCustomInspectorItem;
  const AINIFile: TCustomIniFile; const AOnAddSection: TJvInspConfSectionEvent;
  const AOnAddKey: TJvInspConfKeyEvent): TJvInspectorItemInstances;
var
  TmpLst: TJvInspectorItemInstances;
  SL: TStringList;
  I: Integer;
  CatName: string;
  CatItem: TJvInspectorCustomCategoryItem;

  function AllowAddSection: Boolean;
  begin
    CatName := SL[I];
    Result := True;
    if Assigned(AOnAddSection) then
      AOnAddSection(CatName, Result);
  end;

begin
  SetLength(TmpLst, 0);
  if AINIFile = nil then
    raise EJvInspectorData.CreateRes(@RsEJvAssertINIFile);
  SL := TStringList.Create;
  try
    AINIFile.ReadSections(SL);
    for I := 0 to SL.Count - 1 do
    begin
      if AllowAddSection then
      begin
        CatItem := TJvInspectorCustomCategoryItem.Create(AParent, nil);
        CatItem.Name := SL[I]; // the internal value.  <BUGFIX OCT 23, 2003: WAP.>
        CatItem.DisplayName := CatName; // The displayed value
        //AParent.Name := SL[I];
        TmpLst := TJvInspectorINIFileData.New(CatItem, SL[I], AINIFile, AOnAddKey);
        SetLength(Result, Length(Result) + Length(TmpLst));
        Move(TmpLst[0], Result[Length(Result) - Length(TmpLst)], Length(TmpLst));
        if CatItem.Count = 0 then
          CatItem.Parent.Delete(CatItem);
      end;
    end;
  finally
    SL.Free;
  end;
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

//=== { TJvInspectorPropRegItem } ============================================

constructor TJvInspectorPropRegItem.Create(const AItemClass: TJvInspectorItemClass;
  const AObjectClass: TClass; const AName: string; ATypeInfo: PTypeInfo);
begin
  inherited Create(AItemClass);
  FObjectClass := AObjectClass;
  FName := AName;
  FTypeInfo := ATypeInfo;
end;

function TJvInspectorPropRegItem.Compare(const ADataObj: TJvCustomInspectorData;
  const Item: TJvCustomInspectorRegItem): Integer;
begin
  if not (Item is TJvInspectorPropRegItem) then
    Result := MatchValue(ADataObj)
  else
    Result := inherited Compare(ADataObj, Item);
end;

function TJvInspectorPropRegItem.MatchValue(const ADataObj: TJvCustomInspectorData): Integer;
var
  GoOn: Boolean;
  ObjParentClass: TClass;
begin
  { Match value will be based on the all set items according to the following
    table:

    Base value is 0
    * ClassType known
      * class type equal:           add 32
      * class type inherits:        add 16
      * class does not match:       return 0
    * Name known
      * Name exact match:           add  8
      * Name matches by mask:       add  4
      * Name does not match:        return 0
    * Type info known
      * Typeinfo exact match:       add  2
      * Typeinfo typekind matches:  add  1
      * Typeinfo does not match:    return 0
     }
  Result := 0;
  GoOn := True;
  if TypeInfo <> nil then
  begin
    if TypeInfo = ADataObj.TypeInfo then
      Result := Result or 2
    else
    if TypeInfo.Kind = ADataObj.TypeInfo.Kind then
    begin
      if (TypeInfo.Kind <> tkClass) or
        (GetTypeData(ADataObj.TypeInfo).ClassType.InheritsFrom(GetTypeData(TypeInfo).ClassType)) then
        Result := Result or 1
      else
        GoOn := False;
    end
    else
      GoOn := False;
  end;

  if GoOn and (Name <> '') then
  begin
    if AnsiSameText(Name, ADataObj.Name) then
      Result := Result or 8
      { Match by mask }
    else
      GoOn := False;
  end;

  if GoOn and (ObjectClass <> nil) then
  begin
    { Class type based on the parent object }
    ObjParentClass := TJvInspectorPropData(ADataObj).Instance.ClassType;
    if ObjParentClass = ObjectClass then
      Result := Result or 32
    else
    if (ObjParentClass <> nil) and ObjParentClass.InheritsFrom(ObjectClass) then
      Result := Result or 16
    else
      GoOn := False;
  end;

  if not GoOn then
    Result := 0;
end;

function TJvInspectorPropRegItem.MatchPercent(const ADataObj: TJvCustomInspectorData): Integer;
var
  MV: Integer;
begin
  { A 100% score would mean that Class, Name and TypeInfo all were a perfect
    match. }
  Result := 100;
  MV := MatchValue(ADataObj);
  if MV = 0 then
    Result := 0
  else
  begin
    if ObjectClass <> nil then
    begin
      if (MV and 16) <> 0 then
        Result := Result div 2;
    end
    else
      Dec(Result, 8);

    if Name <> '' then
    begin
      if (MV and 4) <> 0 then
        Result := Result div 2;
    end
    else
      Dec(Result, 4);

    if TypeInfo <> nil then
    begin
      if (MV and 1) <> 0 then
        Result := Result div 2;
    end
    else
      Dec(Result, 8);
  end;
end;

//=== { TJvInspectorTypeInfoMapperRegItem } ==================================

constructor TJvInspectorTypeInfoMapperRegItem.Create(AObjectClass: TClass;
  const APropertyName: string; APropertyType: PTypeInfo; ANewTypeInfo: PTypeInfo);
begin
  inherited Create(nil);
  FObjectClass := AObjectClass;
  FPropertyName := APropertyName;
  FPropertyType := APropertyType;
  FNewTypeInfo := ANewTypeInfo;
end;

function TJvInspectorTypeInfoMapperRegItem.Compare(const ADataObj: TJvCustomInspectorData;
  const Item: TJvCustomInspectorRegItem): Integer;
begin
  Result := inherited CompareTo(ADataObj, Item);
end;

function TJvInspectorTypeInfoMapperRegItem.MatchValue(const ADataObj: TJvCustomInspectorData): Integer;
var
  RetVal: Integer;
begin
  { ObjectClass known
      Same class:       add 32
      Inherited class:  add 16
      no match:         return 0

    PropertyName known
      Exact match:      add 8
      Masked match:     add 4
      no match:         return 0

    PropertyType
      Exact match:      add 2
      Same type kind:   add 1
      No match:         return 0 }
  Result := 0;
  RetVal := Result;
  if ObjectClass <> nil then
  begin
    if TJvInspectorPropData(ADataObj).Instance.ClassType = ObjectClass then
      Inc(RetVal, 32)
    else
    if TJvInspectorPropData(ADataObj).Instance.InheritsFrom(ObjectClass) then
      Inc(RetVal, 16)
    else
      Exit;
  end;

  if PropertyName <> '' then
  begin
    if AnsiSameText(PropertyName, ADataObj.Name) then
      Inc(RetVal, 8)
    else
      Exit;
  end;

  if PropertyType <> nil then
  begin
    if PropertyType = ADataObj.TypeInfo then
      Inc(RetVal, 2)
{    else
    if PropertyType.Kind = ADataObj.TypeInfo.Kind then
      Inc(RetVal, 1)}
    else
      Exit;
  end;
  Result := RetVal;
end;

function TJvInspectorTypeInfoMapperRegItem.MatchPercent(const ADataObj: TJvCustomInspectorData): Integer;
var
  MV: Integer;
  ClassMatch: Integer;
  NameMatch: Integer;
  TypeMatch: Integer;
begin
  MV := MatchValue(ADataObj);
  if MV = 0 then
    Result := 0
  else
  begin
    if MV and 32 <> 0 then
      ClassMatch := 100
    else
    if MV and 16 <> 0 then
      ClassMatch := 50
    else
      ClassMatch := 0;

    if MV and 8 <> 0 then
      NameMatch := 100
    else
    if MV and 4 <> 0 then
      NameMatch := 50
    else
      NameMatch := 0;

    if MV and 2 <> 0 then
      TypeMatch := 100
    else
    if MV and 1 <> 0 then
      TypeMatch := 50
    else
      TypeMatch := 0;

    Result := ((14 * TypeMatch) + (NameMatch) + (5 * ClassMatch)) div 20;
  end;
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
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorIntegerItem, tkInteger));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorEnumItem, tkEnumeration));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorFloatItem, tkFloat));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorSetItem, tkSet));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorCharItem, tkChar));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorCharItem, tkWChar));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorInt64Item, tkInt64));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorClassItem, tkClass));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorTMethodItem, tkMethod));
    Add(TJvInspectorTCaptionRegItem.Create(TJvInspectorStringItem, TypeInfo(TCaption)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorFontItem, TypeInfo(TFont)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorBooleanItem, TypeInfo(Boolean)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorBooleanItem, TypeInfo(ByteBool)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorBooleanItem, TypeInfo(WordBool)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorBooleanItem, TypeInfo(LongBool)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorTStringsItem, TypeInfo(TStrings)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorComponentItem, TypeInfo(TComponent)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorDateItem, TypeInfo(TDate)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorTimeItem, TypeInfo(TTime)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorDateTimeItem, TypeInfo(TDateTime)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorVariantItem, TypeInfo(Variant)));
  end;
end;

procedure RegisterPropDataTypeKinds;
begin
  if TJvCustomInspectorData.ItemRegister = nil then
    raise EJvInspectorReg.CreateRes(@RsEJvInspNoGenReg);
  with TJvInspectorPropData.ItemRegister do
    Add(TJvInspectorPropRegItem.Create(TJvInspectorFontNameItem, TFont, 'Name', nil));
end;

const
  SizingConsts: array [0..3] of TIdentMapEntry =
   ((Value: irsNoReSize; Name: 'irsNoReSize'),
    (Value: irsNameHeight; Name: 'irsNameHeight'),
    (Value: irsValueHeight; Name: 'irsValueHeight'),
    (Value: irsItemHeight; Name: 'irsItemHeight'));

function IrsToInt(const Ident: string; var Int: Longint): Boolean;
begin
  Result := IdentToInt(Ident, Int, SizingConsts);
end;

function IntToIrs(Int: Longint; var Ident: string): Boolean;
begin
  Result := IntToIdent(Int, Ident, SizingConsts);
end;

procedure RegisterConsts;
begin
  RegisterIntegerConsts(TypeInfo(TItemRowSizing), IrsToInt, IntToIrs);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  RegisterConsts;

finalization
  FreeAndNil(GlobalTypeInfoHelpersList);
  FreeAndNil(GlobalCanvasStack);
  FreeAndNil(FieldGlobalInspReg);
  FreeAndNil(GlobalDataRegister);
  FreeAndNil(GlobalGenItemReg);
  FreeAndNil(GlobalVarItemReg);
  FreeAndNil(GlobalPropItemReg);
  FreeAndNil(GlobalPropMapReg);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
