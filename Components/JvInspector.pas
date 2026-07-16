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
  SysUtils, Classes, Contnrs,
  Windows, Messages, Graphics, Controls, StdCtrls, ExtCtrls,
  JvAutoComplete, JvInspectorSupport;

type
  // early declarations
  TJvInspector = class;
  TJvCustomInspectorItem = class;
  TJvInspectorCustomCategoryItem = class;

  TInspectorItemFlag = (iifReadonly, iifExpanded, iifValueList);
  TInspectorItemFlags = set of TInspectorItemFlag;

  TInspectorPaintRect = (iprItem, iprButtonArea, iprBtnDstRect,
    iprNameArea, iprName, iprValueArea, iprValue, iprEditValue, iprEditButton);

  TInspectorItemGetValueListEvent = procedure(Item: TJvCustomInspectorItem; Values: TStrings) of object;
  TJvInspAsOrdinal = procedure(Sender: TJvCustomInspectorItem; var Value: Int64) of object;
  TJvInspAsString = procedure(Sender: TJvCustomInspectorItem; var Value: string) of object;
  TInspectorBeforeEditEvent = procedure(Sender: TObject; Item: TJvCustomInspectorItem; Edit: TCustomEdit) of object;

  EJvInspector = class(EJVCLException);
  EJvInspectorData = class(EJvInspector);

  TOnJvInspectorSetItemColors = procedure(Item: TJvCustomInspectorItem; Canvas: TCanvas) of object;

  TJvInspector = class(TCustomControl)
  private
    FDivider: Integer;
    FDraggingDivider: Boolean;
    FItemHeight: Integer;
    FLockCount: Integer;
    FNeedRebuild: Boolean;
    FBackgroundColor: TColor;
    FButtonImage: TBitmap;
    FCategoryColor: TColor;
    FCategoryDividerColor: TColor;
    FCategoryTextColor: TColor;
    FDividerColor: TColor;
    FHideSelectColor: TColor;
    FHideSelectTextColor: TColor;
    FInternalButtonBackgroundColor: TColor;
    FInternalButtonPenColor: TColor;
    FInternalButtonSize: Integer;
    FInternalCollapseButton: TBitmap;
    FInternalExpandButton: TBitmap;
    FNameColor: TColor;
    FOnSetItemColors: TOnJvInspectorSetItemColors;
    FPaintItem: TJvCustomInspectorItem;
    FPaintItemIndex: Integer;
    FPaintRect: TRect;
    FSelectedColor: TColor;
    FSelectedTextColor: TColor;
    FValueColor: TColor;
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
    // This event fired is when creating TEdit objects, and
    // allows end users to customize the properties of the editor
    // objects, or hook event handlers, which were
    // otherwise invisible. This could be used to ill effect, so beware.
    FBeforeEdit: TInspectorBeforeEditEvent;
    FMouseWheelRecursion: Boolean;
    function ApplicationHook(var Msg: TMessage): Boolean;
    procedure ApplyNameFont;
    procedure ApplyValueFont;
    procedure CalcButtonBasedRects;
    procedure CalcEditBasedRects;
    procedure CalcNameBasedRects;
    procedure CalcValueBasedRects;
    procedure DoPaintItem;
    function GetCollapseImage: TBitmap;
    function GetExpandImage: TBitmap;
    procedure PaintItem(var ARect: TRect; const AItemIndex: Integer);
    procedure PaintItems;
    procedure PrepareInternalImages;
    procedure SetupItem;
    procedure SetupRects;
    procedure TeardownItem;
  protected
    function CalcItemIndex(const Y: Integer): Integer;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure CMActivate(var Msg: TCMActivate); message CM_ACTIVATE;
    procedure CMDeactivate(var Msg: TCMActivate); message CM_DEACTIVATE;
    function GetImageHeight: Integer;
    function GetItemHeight: Integer;
    function GetLastFullVisible: Integer;
    function GetSelected: TJvCustomInspectorItem;
    function GetVisibleCount: Integer;
    function GetVisibleItems(const I: Integer): TJvCustomInspectorItem;
    function IdxToY(const Index: Integer): Integer;
    procedure InvalidateItem;
    procedure InvalidateList;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure RebuildVisible;
    procedure BoundsChanged;
    procedure SetDivider(Value: Integer);
    procedure SetSelectedIndex(Value: Integer);
    procedure SetTopIndex(Value: Integer);
    procedure UpdateScrollBars;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    procedure WndProc(var Msg: TMessage); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure ShowScrollBars(Bar: Integer; Visible: Boolean);
    function YToIdx(const Y: Integer): Integer;
    property DraggingDivider: Boolean read FDraggingDivider write FDraggingDivider;
    property ItemHeight: Integer read GetItemHeight;
    property ImageHeight: Integer read GetImageHeight;
    property LockCount: Integer read FLockCount;
    property NeedRebuild: Boolean read FNeedRebuild write FNeedRebuild;
    property PaintGeneration: Integer read FPaintGen;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Selecting: Boolean read FSelecting write FSelecting;
    property TopIndex: Integer read FTopIndex write SetTopIndex;
    property VisibleCount: Integer read GetVisibleCount;
    property VisibleItems[const I: Integer]: TJvCustomInspectorItem read GetVisibleItems;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeforeDestruction; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function Focused: Boolean; override;
    procedure RefreshValues;
    procedure Clear;
    property BevelKind;
    property Divider: Integer read FDivider write SetDivider;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Root: TJvCustomInspectorItem read FRoot;
    property Selected: TJvCustomInspectorItem read GetSelected;
    property BeforeEdit: TInspectorBeforeEditEvent read FBeforeEdit write FBeforeEdit; // Low level hook for customizing TEdit after objects are created, just before editing.
    property OnKeyDown; // Standard control event
    { Standard TCustomControl event - this is really an event fired by
      the TEdit control used when editing in a cell!}
    property OnEditorKeyDown: TKeyEvent read FOnEditorKeyDown write FOnEditorKeyDown;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property CategoryColor: TColor read FCategoryColor write FCategoryColor;
    property CategoryDividerColor: TColor read FCategoryDividerColor write FCategoryDividerColor;
    property CategoryTextColor: TColor read FCategoryTextColor write FCategoryTextColor;
    property DividerColor: TColor read FDividerColor write FDividerColor;
    property HideSelectColor: TColor read FHideSelectColor write FHideSelectColor;
    property HideSelectTextColor: TColor read FHideSelectTextColor write FHideSelectTextColor;
    property NameColor: TColor read FNameColor write FNameColor;
    property SelectedColor: TColor read FSelectedColor write FSelectedColor;
    property SelectedTextColor: TColor read FSelectedTextColor write FSelectedTextColor;
    property ValueColor: TColor read FValueColor write FValueColor;
    property OnSetItemColors: TOnJvInspectorSetItemColors read FOnSetItemColors write FOnSetItemColors;
  end;

  TJvCustomInspectorItem = class(TPersistent)
  private
    FDisplayName: string;
    FDroppedDown: Boolean;
    FEditCtrlDestroying: Boolean;
    FEditCtrl: TCustomEdit;
    FEditWndPrc: TWndMethod;
    FEditing: Boolean;
    FAutoComplete: TJvEditListBoxAutoComplete;
    FFlags: TInspectorItemFlags;
    FInspector: TJvInspector;
    FItems: TObjectList;
    FListBox: TCustomListBox;
    FOnGetAsOrdinal: TJvInspAsOrdinal;
    FOnGetAsString: TJvInspAsString;
    FOnSetAsOrdinal: TJvInspAsOrdinal;
    FOnSetAsString: TJvInspAsString;
    FOnGetValueList: TInspectorItemGetValueListEvent;
    FParent: TJvCustomInspectorItem;
    FLastPaintGen: Integer;
    FPressed: Boolean;
    FRects: array [TInspectorPaintRect] of TRect;
    FTracking: Boolean;
    FUpdateEditCtrl: Integer; // Used to prevent EditCtrl destruction while in Apply().
  protected
    procedure Apply;
    function CanEdit: Boolean;
    procedure CheckAccess;
    procedure CloseUp(Accept: Boolean);
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState);
    procedure DropDown;
    procedure EditFocusLost(Sender: TObject);
    procedure EditKillFocus(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); dynamic;
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Edit_WndProc(var Msg: TMessage);
    procedure AutoCompleteStart(Sender: TObject);
    function GetAsOrdinal: Int64;
    function GetAsString: string;
    function GetBaseCategory: TJvCustomInspectorItem;
    function GetCount: Integer;
    function GetDisplayValue: string; virtual;
    function GetExpanded: Boolean;
    function GetItems(const I: Integer): TJvCustomInspectorItem;
    function GetLevel: Integer;
    function GetReadOnly: Boolean;
    function GetRects(const RectKind: TInspectorPaintRect): TRect;
    procedure GetValueList(const Strings: TStrings);
    procedure InvalidateItem;
    procedure SetAsOrdinal(Value: Int64);
    procedure SetAsString(Value: string);
    function IsInitialized: Boolean;
    procedure InvalidateList;
    function IsCategory: Boolean; virtual;
    procedure ListExit(Sender: TObject);
    procedure ListValueSelect(Sender: TObject);
    procedure ListDeactivate(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SelectValue(const Delta: Integer);
    procedure SetDisplayValue(const Value: string); virtual;
    procedure SetEditCtrl(const Value: TCustomEdit);
    procedure SetExpanded(Value: Boolean);
    procedure SetFlags(const Value: TInspectorItemFlags); virtual;
    procedure SetFocus;
    procedure SetRects(const RectKind: TInspectorPaintRect; Value: TRect);
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    procedure Undo;
    property AsOrdinal: Int64 read GetAsOrdinal write SetAsOrdinal;
    property AsString: string read GetAsString write SetAsString;
    property BaseCategory: TJvCustomInspectorItem read GetBaseCategory;
    property DroppedDown: Boolean read FDroppedDown;
    property EditCtrlDestroying: Boolean read FEditCtrlDestroying;
    property EditCtrl: TCustomEdit read FEditCtrl;
    property EditWndPrc: TWndMethod read FEditWndPrc;
    property LastPaintGeneration: Integer read FLastPaintGen;
    property ListBox: TCustomListBox read FListBox;
    property Pressed: Boolean read FPressed write FPressed;
    property Tracking: Boolean read FTracking write FTracking;
  public
    constructor Create(const AParent: TJvCustomInspectorItem);
    destructor Destroy; override;
    procedure Add(const Item: TJvCustomInspectorItem);
    procedure BeforeDestruction; override;
    procedure Clear;
    procedure Delete(const Index: Integer);
    procedure DrawEditor(const ACanvas: TCanvas);
    procedure DrawName(const ACanvas: TCanvas);
    procedure DrawValue(const ACanvas: TCanvas); virtual;
    function EditFocused: Boolean;
    procedure ExpandItems(AExpand: Boolean);
    procedure InitEdit; dynamic;
    procedure DoneEdit(const CancelEdits: Boolean = False); dynamic;
    procedure ScrollInView;
    property Count: Integer read GetCount;
    property DisplayName: string read FDisplayName write FDisplayName;
    property DisplayValue: string read GetDisplayValue write SetDisplayValue;
    property Editing: Boolean read FEditing;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property Flags: TInspectorItemFlags read FFlags write SetFlags;
    property Inspector: TJvInspector read FInspector;
    property Items[const I: Integer]: TJvCustomInspectorItem read GetItems; default;
    property Level: Integer read GetLevel;
    property Parent: TJvCustomInspectorItem read FParent;
    property ReadOnly: Boolean read GetReadOnly;
    property Rects[const RectKind: TInspectorPaintRect]: TRect read GetRects write SetRects;
    property OnGetAsOrdinal: TJvInspAsOrdinal read FOnGetAsOrdinal write FOnGetAsOrdinal;
    property OnGetAsString: TJvInspAsString read FOnGetAsString write FOnGetAsString;
    property OnSetAsOrdinal: TJvInspAsOrdinal read FOnSetAsOrdinal write FOnSetAsOrdinal;
    property OnSetAsString: TJvInspAsString read FOnSetAsString write FOnSetAsString;
    property OnGetValueList: TInspectorItemGetValueListEvent read FOnGetValueList write FOnGetValueList;
  end;

  TJvInspectorCustomCategoryItem = class(TJvCustomInspectorItem)
  protected
    function IsCategory: Boolean; override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  end;

  TJvInspectorStringItem = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
  end;

  TJvInspectorBooleanItem = class(TJvCustomInspectorItem)
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

// (rom) centralized the string literals
const
  cJvInspectorOrdinal = 'Ordinal';
  cJvInspectorString = 'string';

implementation

uses
  System.UITypes,
  Types, Forms;

//============================================================================

type
  TCustomEditAccessProtected = class(TCustomEdit);

//=== { TCanvasState } =======================================================

type
  TCanvasState = class
  private
    FBrush: TBrush;
    FPen: TPen;
    FFont: TFont;
  public
    constructor Create(const Canvas: TCanvas);
    destructor Destroy; override;
    procedure ApplyTo(const Canvas: TCanvas);
  end;

constructor TCanvasState.Create(const Canvas: TCanvas);
begin
  inherited Create;
  FBrush := TBrush.Create;
  FPen := TPen.Create;
  FFont := TFont.Create;
  FBrush.Assign(Canvas.Brush);
  FPen.Assign(Canvas.Pen);
  FFont.Assign(Canvas.Font);
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

//=== { TJvInspector } =================================================

function TJvInspector.ApplicationHook(var Msg: TMessage): Boolean;
begin
  Result := False;
  // Forward the application's activate/deactivate to this inspector so the
  // selected row repaints in its focused/unfocused style and an open dropdown
  // closes
  if (Msg.Msg = CM_ACTIVATE) or (Msg.Msg = CM_DEACTIVATE) then
    if HandleAllocated then
      PostMessage(Handle, Msg.Msg, 0, 0);
end;

constructor TJvInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FItemHeight := 18;
  DoubleBuffered := True;
  FVisibleList := TStringList.Create;
  FRoot := TJvCustomInspectorItem.Create(nil);
  FRoot.FInspector := Self;
  FSelectedIndex := -1;
  BevelKind := bkTile;
  BevelInner := bvNone;
  BevelOuter := bvLowered;
  TabStop := True;
  Width := 300;
  Height := 100;
  Divider := 75;

  FInternalCollapseButton := TBitmap.Create;
  FInternalExpandButton := TBitmap.Create;
  FBackgroundColor := clWindow;
  FCategoryColor := clBtnFace;
  FCategoryDividerColor := clBtnShadow;
  FCategoryTextColor := clBtnText;
  FDividerColor := clBtnFace;
  FNameColor := clWindowText;
  FValueColor := clWindowText;
  FSelectedColor := clHighlight;
  FSelectedTextColor := clHighlightText;
  FHideSelectColor := clBtnFace;
  FHideSelectTextColor := clHighlightText;

  Application.HookMainWindow(ApplicationHook);
end;

function TJvInspector.CalcItemIndex(const Y: Integer): Integer;
begin
  if Y < 0 then
    Result := -1
  else
  begin
    Result := TopIndex + Y div ItemHeight;
    if Result >= VisibleCount then
      Result := -1;
  end;
end;

procedure TJvInspector.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited ChangeScale(M, D, isDpiChange);
  if M <> D then
  begin
    // ItemHeight needs no scaling here: it stores a 96 dpi value which
    // GetItemHeight scales on read
    FDivider := MulDiv(FDivider, M, D);
  end;
end;

procedure TJvInspector.CMActivate(var Msg: TCMActivate);
begin
  Invalidate;
end;

procedure TJvInspector.CMDeactivate(var Msg: TCMActivate);
begin
  inherited;
  Invalidate;
end;

function TJvInspector.GetImageHeight: Integer;
begin
  Result := VisibleCount * ItemHeight;
end;

function TJvInspector.GetItemHeight: Integer;
begin
  // FItemHeight holds a value for 96 dpi; scale it to the current dpi
  Result := MulDiv(FItemHeight, CurrentPPI, 96);
end;

function TJvInspector.GetLastFullVisible: Integer;
begin
  Result := YToIdx(IdxToY(TopIndex) + Pred(ClientHeight));
  if Result < 0 then
    Result := Pred(VisibleCount)
  else if Result > Pred(ClientHeight div ItemHeight) then
    Result := Pred(ClientHeight div ItemHeight);
end;

function TJvInspector.GetSelected: TJvCustomInspectorItem;
begin
  if (SelectedIndex > -1) and (SelectedIndex < VisibleCount) then
    Result := VisibleItems[SelectedIndex]
  else
    Result := nil;
end;

function TJvInspector.GetVisibleCount: Integer;
begin
  Result := FVisibleList.Count;
end;

function TJvInspector.GetVisibleItems(const I: Integer): TJvCustomInspectorItem;
begin
  if (I < 0) or (I >= FVisibleList.Count) then
    Result := nil
  else
    Result := TJvCustomInspectorItem(FVisibleList.Objects[I]);
end;

function TJvInspector.IdxToY(const Index: Integer): Integer;
begin
  Result := Index * ItemHeight;
end;

procedure TJvInspector.InvalidateItem;
begin
  if (LockCount = 0) and HandleAllocated then
    UpdateScrollBars;
end;

procedure TJvInspector.InvalidateList;
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

procedure TJvInspector.KeyDown(var Key: Word; Shift: TShiftState);
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
            TmpH := ItemHeight;
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
            TmpH := ItemHeight;
            TmpIdx := YToIdx(IdxToY(SelectedIndex) + TmpH + ClientHeight);
            if TmpIdx < 0 then
              TmpIdx := Pred(VisibleCount);
            SelectedIndex := TmpIdx;
          end;
        end;
      VK_ADD:
        if (Item.Count > 0) and not Item.Expanded then
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
        if (Item.Count > 0) and not Item.Expanded then
          Item.Expanded := True;
      VK_LEFT:
        if Item.Expanded then
          Item.Expanded := False;
      VK_RETURN:
        if (Item.Count > 0) and not Item.Expanded then
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

procedure TJvInspector.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  XB: Integer;
  ItemIndex: Integer;
  Item: TJvCustomInspectorItem;
  CharPos: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  XB := X;
  ItemIndex := CalcItemIndex(Y);
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
      ((Item.Count > 0) or (iifExpanded in Item.Flags)) then
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

procedure TJvInspector.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  XB: Integer;
  ItemIndex: Integer;
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
    ItemIndex := CalcItemIndex(Y);
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

procedure TJvInspector.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ItemIndex: Integer;
  Item: TJvCustomInspectorItem;
begin
  inherited MouseUp(Button, Shift, X, Y);
  ItemIndex := CalcItemIndex(Y);
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

procedure TJvInspector.Paint;
begin
  if NeedRebuild then
    InvalidateList;
  Inc(FPaintGen);
  Canvas.Brush.Color := FBackgroundColor;
  PaintItems;
end;

procedure TJvInspector.RebuildVisible;
var
  OldSel: TJvCustomInspectorItem;

  procedure AddChildren(const Item: TJvCustomInspectorItem);
  var
    I: Integer;
    Child: TJvCustomInspectorItem;
  begin
    for I := 0 to Item.Count - 1 do
    begin
      Child := Item.Items[I];
      FVisibleList.AddObject('', Child);
      if Child.Expanded then
        AddChildren(Child);
    end;
  end;

begin
  OldSel := Selected;
  FVisibleList.Clear;
  AddChildren(Root);
  if OldSel <> nil then
    SelectedIndex := FVisibleList.IndexOfObject(OldSel);
  NeedRebuild := False;
end;

procedure TJvInspector.BoundsChanged;
begin
  if csCreating in ControlState then
    Exit;
  TopIndex := TopIndex; // Adapt position
  if HandleAllocated then
    UpdateScrollBars;
end;

procedure TJvInspector.SetDivider(Value: Integer);
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

procedure TJvInspector.SetSelectedIndex(Value: Integer);
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

procedure TJvInspector.SetTopIndex(Value: Integer);
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

procedure TJvInspector.UpdateScrollBars;
var
  DrawHeight: Integer;
  ClHeight: Integer;
  ScrollInfo: TScrollInfo;
  ShowVertSB: Boolean;
begin
  if csDestroying in ComponentState then
    Exit;

  if not HandleAllocated then
    Exit;

  // Cache the image height and client height
  DrawHeight := ImageHeight;
  ClHeight := ClientHeight;
  { Needed to redisplay the scrollbar after it's hidden in the CloseUp method
    of an enumerated item's combobox }
  ShowVertSB := DrawHeight >= ClHeight;
  if ShowVertSB then
  begin
    with ScrollInfo do
    begin
      cbSize := SizeOf(ScrollInfo);
      fMask := SIF_ALL;
      nMin := 0;
      nMax := IdxToY(Succ(YToIdx(ImageHeight - ClientHeight))) + ClientHeight;
      nPage := ClHeight;
      nPos := IdxToY(TopIndex);
      nTrackPos := 0;
    end;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end;
  ShowScrollBars(SB_VERT, ShowVertSB);
  Invalidate;
end;

procedure TJvInspector.WndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_SETFOCUS:
      begin
        inherited WndProc(Msg);
        if (Selected <> nil) and not Selected.EditCtrlDestroying then
          Selected.SetFocus;
        Invalidate;
      end;
    WM_KILLFOCUS:
      begin
        inherited WndProc(Msg);
        Invalidate;
      end;
    WM_SIZE, WM_MOVE:
      begin
        inherited WndProc(Msg);
        BoundsChanged;
      end;
    WM_GETDLGCODE:
      begin
        inherited WndProc(Msg);
        Msg.Result := DLGC_WANTARROWS;
      end;
  else
    inherited WndProc(Msg);
  end;
end;

procedure TJvInspector.WMVScroll(var Msg: TWMScroll);
var
  Delta: Integer;
begin
  Delta := 0;
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
      Delta := Msg.Pos - IdxToY(TopIndex);
    SB_THUMBTRACK:
      Delta := Msg.Pos - IdxToY(TopIndex);
    SB_TOP:
      Delta := -IdxToY(TopIndex);
  else
    Delta := 0;
  end;
  if Delta <> 0 then
    TopIndex := YToIdx(IdxToY(TopIndex) + Delta);
end;

function TJvInspector.YToIdx(const Y: Integer): Integer;
begin
  if Y <= 0 then
    Result := 0
  else
    Result := (Y - 1) div ItemHeight;
  if Result >= VisibleCount then
    Result := -1;
end;

procedure TJvInspector.BeforeDestruction;
begin
  inherited BeforeDestruction;
  Application.UnhookMainWindow(ApplicationHook);
  FRoot.Free;
  FVisibleList.Free;
  FInternalCollapseButton.Free;
  FInternalExpandButton.Free;
end;

procedure TJvInspector.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TJvInspector.EndUpdate;
begin
  if FLockCount > 0 then
    Dec(FLockCount);
  if FLockCount = 0 then
  begin
    if NeedRebuild then
      InvalidateList
    else
      InvalidateItem;
  end;
end;

function TJvInspector.Focused: Boolean;
begin
  Result := inherited Focused or ((Selected <> nil) and Selected.EditFocused);
end;

procedure TJvInspector.RefreshValues;
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

procedure TJvInspector.Clear;
begin
  BeginUpdate;
  SelectedIndex := -1;
  Root.Clear;
  EndUpdate;
end;

function TJvInspector.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
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

procedure TJvInspector.ShowScrollBars(Bar: Integer; Visible: Boolean);
begin
  ShowScrollBar(Handle, Bar, Visible);
end;

//=== { TJvInspector painting } ========================================

procedure TJvInspector.ApplyNameFont;
begin
  Canvas.Font := Font;
  if FPaintItem.IsCategory then
  begin
    Canvas.Font.Color := FCategoryTextColor;
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
  end
  else
    Canvas.Font.Color := FNameColor;
  if FPaintItem = Selected then
  begin
    if Focused then
    begin
      Canvas.Brush.Color := FSelectedColor;
      Canvas.Font.Color := FSelectedTextColor;
    end
    else
    begin
      Canvas.Brush.Color := FHideSelectColor;
      Canvas.Font.Color := FHideSelectTextColor;
    end;
  end
  else
  if FPaintItem.IsCategory then
    Canvas.Brush.Color := FCategoryColor
  else
    Canvas.Brush.Color := FBackgroundColor;
end;

procedure TJvInspector.ApplyValueFont;
begin
  Canvas.Font := Font;
  Canvas.Font.Color := FValueColor;
end;

procedure TJvInspector.PrepareInternalImages;
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
    Image.Canvas.Brush.Color := FBackgroundColor;
    Image.Canvas.Pen.Color := FNameColor;
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
  // dpi using the inspector's colors, keeping the last
  // rendering until the size or the colors change
  Size := MulDiv(9, CurrentPPI, 96);
  if not Odd(Size) then
    Dec(Size);
  if (Size = FInternalButtonSize) and
    (FBackgroundColor = FInternalButtonBackgroundColor) and
    (FNameColor = FInternalButtonPenColor) then
    Exit;
  FInternalButtonSize := Size;
  FInternalButtonBackgroundColor := FBackgroundColor;
  FInternalButtonPenColor := FNameColor;
  PrepareImage(FInternalCollapseButton, False);
  PrepareImage(FInternalExpandButton, True);
end;

function TJvInspector.GetCollapseImage: TBitmap;
begin
  PrepareInternalImages;
  Result := FInternalCollapseButton;
end;

function TJvInspector.GetExpandImage: TBitmap;
begin
  PrepareInternalImages;
  Result := FInternalExpandButton;
end;

procedure TJvInspector.PaintItems;
var
  SelItemVisible: Boolean;
  Rect: TRect;
  ItemIdx: Integer;
  MaxItemIdx: Integer;
begin
  SelItemVisible := False;
  Rect := ClientRect;
  Canvas.FillRect(Rect);
  ItemIdx := TopIndex;
  MaxItemIdx := VisibleCount;
  // Loop through the visible list
  while (Rect.Top < Rect.Bottom) and (ItemIdx < MaxItemIdx) do
  begin
    SelItemVisible := SelItemVisible or (ItemIdx = SelectedIndex);
    PaintItem(Rect, ItemIdx);
    Inc(ItemIdx);
  end;
  if not SelItemVisible and (Selected <> nil) then
    Selected.Rects[iprEditValue] := TRect.Empty;
end;

procedure TJvInspector.PaintItem(var ARect: TRect;
  const AItemIndex: Integer);
var
  OrgState: TCanvasState;
begin
  OrgState := TCanvasState.Create(Canvas);
  try
    // Initialize paint variables
    FPaintRect := ARect;
    FPaintItemIndex := AItemIndex;
    SetupItem;

    // Do actual painting
    DoPaintItem;

    // Finalize painting
    TeardownItem;
    ARect := FPaintRect;
  finally
    OrgState.ApplyTo(Canvas);
    OrgState.Free;
  end;
end;

procedure TJvInspector.SetupItem;
begin
  // retrieve item
  FPaintItem := VisibleItems[FPaintItemIndex];

  if FPaintItem <> nil then
  begin
    // retrieve button image
    if FPaintItem.Expanded then
      FButtonImage := GetCollapseImage
    else
    if FPaintItem.Count > 0 then
      FButtonImage := GetExpandImage
    else
      FButtonImage := nil;
  end
  else
    FButtonImage := nil;

  // calculate rectangles
  SetupRects;
end;

procedure TJvInspector.SetupRects;
var
  ItemRect2: TRect;
  TmpRect: TRect;
begin
  FPaintItem.Rects[iprItem] := Rect(FPaintRect.Left, FPaintRect.Top,
    FPaintRect.Right, Pred(FPaintRect.Top + ItemHeight));
  ItemRect2 := FPaintItem.Rects[iprItem];
  TmpRect := Rect(ItemRect2.Left + (FPaintItem.Level * ItemHeight), ItemRect2.Top,
    ItemRect2.Left + (Succ(FPaintItem.Level) * ItemHeight), ItemRect2.Bottom);
  if not FPaintItem.IsCategory and (TmpRect.Left > Pred(Divider)) then
  begin
    TmpRect.Left := 0;
    TmpRect.Right := 0;
  end;
  if not FPaintItem.IsCategory and (TmpRect.Right > Pred(Divider)) then
    TmpRect.Right := Pred(Divider);
  FPaintItem.Rects[iprButtonArea] := TmpRect;
  TmpRect := ItemRect2;
  TmpRect.Left := ItemRect2.Left + (Succ(FPaintItem.Level) * ItemHeight);
  FPaintItem.Rects[iprNameArea] := TmpRect;
  if FPaintItem.IsCategory then
    FPaintItem.Rects[iprValueArea] := Rect(0, 0, 0, 0)
  else
  begin
    if TmpRect.Left > Pred(Divider) then
      TmpRect := Rect(0, 0, 0, 0)
    else
      TmpRect.Right := ItemRect2.Left + Pred(Divider);
    FPaintItem.Rects[iprNameArea] := TmpRect;
    TmpRect := ItemRect2;
    TmpRect.Left := ItemRect2.Left + Divider + 1;
    FPaintItem.Rects[iprValueArea] := TmpRect;
  end;
  CalcButtonBasedRects;
  CalcNameBasedRects;
  CalcValueBasedRects;
end;

procedure TJvInspector.CalcButtonBasedRects;
var
  BtnDstRect: TRect;
begin
  if (FButtonImage <> nil) and (RectWidth(FPaintItem.Rects[iprButtonArea]) > 0) then
  begin
    BtnDstRect := Rect(0, 0, FButtonImage.Width, FButtonImage.Height);
    OffsetRect(BtnDstRect, (ItemHeight - FButtonImage.Width) div 2,
      (RectHeight(FPaintItem.Rects[iprButtonArea]) - FButtonImage.Height) div 2);
    OffsetRect(BtnDstRect, FPaintItem.Rects[iprButtonArea].Left, FPaintItem.Rects[iprButtonArea].Top);
    IntersectRect(BtnDstRect, BtnDstRect, FPaintItem.Rects[iprButtonArea]);
  end
  else
    BtnDstRect := Rect(0, 0, 0, 0);
  FPaintItem.Rects[iprBtnDstRect] := BtnDstRect;
end;

procedure TJvInspector.CalcNameBasedRects;
var
  CanvasState: TCanvasState;
  RowHeight: Integer;
  TmpRect: TRect;
begin
  CanvasState := TCanvasState.Create(Canvas);
  try
    ApplyNameFont;
    RowHeight := CanvasMaxTextHeight(Canvas);
    TmpRect := FPaintItem.Rects[iprNameArea];
    if FPaintItem.Level = 0 then
      Inc(TmpRect.Left, 2);
    if RectHeight(TmpRect) div RowHeight < 2 then
      OffsetRect(TmpRect, 0, (RectHeight(TmpRect) - RowHeight) div 2)
    else
    begin
      Inc(TmpRect.Top, 1);
      Dec(TmpRect.Bottom, 1);
    end;
    IntersectRect(TmpRect, TmpRect, FPaintItem.Rects[iprNameArea]);
    FPaintItem.Rects[iprName] := TmpRect;
  finally
    CanvasState.ApplyTo(Canvas);
    CanvasState.Free;
  end;
end;

procedure TJvInspector.CalcValueBasedRects;
var
  CanvasState: TCanvasState;
  RowHeight: Integer;
  TmpRect: TRect;
begin
  CanvasState := TCanvasState.Create(Canvas);
  try
    ApplyValueFont;
    RowHeight := CanvasMaxTextHeight(Canvas);
    TmpRect := FPaintItem.Rects[iprValueArea];
    if RectHeight(TmpRect) div RowHeight < 2 then
    begin
      OffsetRect(TmpRect, 0, (RectHeight(TmpRect) - RowHeight) div 2);
      IntersectRect(TmpRect, TmpRect, FPaintItem.Rects[iprValueArea]);
    end
    else
    begin
      Inc(TmpRect.Top, 1);
      Dec(TmpRect.Bottom, 1);
      IntersectRect(TmpRect, TmpRect, FPaintItem.Rects[iprValueArea]);
    end;
    FPaintItem.Rects[iprValue] := TmpRect;
  finally
    CanvasState.ApplyTo(Canvas);
    CanvasState.Free;
  end;
  CalcEditBasedRects;
end;

procedure TJvInspector.CalcEditBasedRects;
var
  TmpRect: TRect;
begin
  if not (iifValueList in FPaintItem.Flags) then
  begin // Value takes up entire edit value rect, there is no edit button:
    FPaintItem.Rects[iprEditValue] := FPaintItem.Rects[iprValue];
    FPaintItem.Rects[iprEditButton] := Rect(0, 0, 0, 0);
  end
  else
  begin // The edit button is on the right of the edit value area:
    TmpRect := FPaintItem.Rects[iprValue];
    Dec(TmpRect.Right, ItemHeight);
    FPaintItem.Rects[iprEditValue] := TmpRect;
    TmpRect := FPaintItem.Rects[iprValueArea];
    TmpRect.Left := TmpRect.Right - ItemHeight;
    FPaintItem.Rects[iprEditButton] := TmpRect;
  end;
end;

procedure TJvInspector.DoPaintItem;
var
  EndOfList: Boolean;
  NextItem: TJvCustomInspectorItem;
  EndOfCat: Boolean;
  PreNameRect: TRect;
  CatRect: TRect;
  SaveState: TCanvasState;
  LeftX: Integer;
begin
  SaveState := TCanvasState.Create(Canvas);
  try
    // Determine item type (end of list, end of a category)
    EndOfList := Succ(FPaintItemIndex) >= VisibleCount;
    if not EndOfList then
    begin
      NextItem := VisibleItems[Succ(FPaintItemIndex)];
      EndOfCat := (NextItem.BaseCategory <> FPaintItem.BaseCategory) and
        (FPaintItem.BaseCategory <> nil);
    end
    else
      EndOfCat := FPaintItem.BaseCategory <> nil;

    PreNameRect := FPaintItem.Rects[iprItem];
    PreNameRect.Left := PreNameRect.Left + (FPaintItem.Level * ItemHeight) + ItemHeight;
    if PreNameRect.Left > Pred(Divider) then
      PreNameRect := Rect(0, 0, 0, 0)
    else
    begin
      PreNameRect.Right := PreNameRect.Left + ItemHeight;
      if PreNameRect.Right > Pred(Divider) then
        PreNameRect.Right := Pred(Divider);
    end;
    Inc(PreNameRect.Right);

    CatRect := FPaintItem.Rects[iprItem];
    CatRect.Right := CatRect.Left + ItemHeight;
    Inc(CatRect.Bottom);
    if FPaintItem.BaseCategory <> nil then
    begin
      Canvas.Brush.Color := FCategoryColor;
      Canvas.FillRect(CatRect);
      SaveState.ApplyTo(Canvas);
    end;

    if not FPaintItem.IsCategory then
    begin
      Canvas.Pen.Color := FDividerColor;
      Canvas.MoveTo(FPaintItem.Rects[iprItem].Left + Divider, Pred(FPaintItem.Rects[iprItem].Top));
      Canvas.LineTo(FPaintItem.Rects[iprItem].Left + Divider, FPaintItem.Rects[iprItem].Bottom);
    end;

    if FPaintItem.IsCategory then
      Canvas.Brush.Color := FCategoryColor;
    if (FPaintItem = Selected) and not FPaintItem.IsCategory then
    begin
      if Focused then
        Canvas.Brush.Color := FSelectedColor
      else
        Canvas.Brush.Color := FHideSelectColor;
    end;
    Canvas.FillRect(PreNameRect);
    ApplyNameFont;
    Canvas.FillRect(FPaintItem.Rects[iprNameArea]);
    if Assigned(FOnSetItemColors) then
      FOnSetItemColors(FPaintItem, Canvas);
    FPaintItem.DrawName(Canvas);
    SaveState.ApplyTo(Canvas);
    ApplyValueFont;
    if Assigned(FOnSetItemColors) then
      FOnSetItemColors(FPaintItem, Canvas); // Custom colors for canvas and font for cells depending on values.
    FPaintItem.DrawValue(Canvas);
    SaveState.ApplyTo(Canvas);
  finally
    SaveState.Free;
  end;

  if FButtonImage <> nil then
    Canvas.CopyRect(FPaintItem.Rects[iprBtnDstRect], FButtonImage.Canvas,
      Rect(0, 0, FButtonImage.Width, FButtonImage.Height));

  SaveState := TCanvasState.Create(Canvas);
  try
    if EndOfCat or FPaintItem.IsCategory then
      Canvas.Pen.Color := FCategoryDividerColor
    else
      Canvas.Pen.Color := FDividerColor;
    if not EndOfList and not EndOfCat then
      LeftX := FPaintItem.Rects[iprItem].Left + ItemHeight
    else
      LeftX := FPaintItem.Rects[iprItem].Left;
    Canvas.MoveTo(FPaintItem.Rects[iprItem].Right, FPaintItem.Rects[iprItem].Bottom);
    Canvas.LineTo(Pred(LeftX), FPaintItem.Rects[iprItem].Bottom);

    if FPaintItem <> FPaintItem.BaseCategory then
    begin
      if FPaintItem.BaseCategory <> nil then
        Canvas.Pen.Color := FCategoryDividerColor
      else
        Canvas.Pen.Color := FCategoryColor;
      Canvas.MoveTo(FPaintItem.Rects[iprItem].Left + ItemHeight, FPaintItem.Rects[iprItem].Top);
      Canvas.LineTo(FPaintItem.Rects[iprItem].Left + ItemHeight, Succ(FPaintItem.Rects[iprItem].Bottom));
    end;
    SaveState.ApplyTo(Canvas);
  finally
    SaveState.Free;
  end;
end;

procedure TJvInspector.TeardownItem;
var
  TmpRect: TRect;
begin
  TmpRect := FPaintRect;
  TmpRect.Top := Succ(FPaintItem.Rects[iprItem].Bottom);
  FPaintRect := TmpRect;
  FPaintItem := nil;
  FPaintItemIndex := -1;
end;

//=== { TJvCustomInspectorItem } =============================================

constructor TJvCustomInspectorItem.Create(const AParent: TJvCustomInspectorItem);
begin
  inherited Create;
  FItems := TObjectList.Create(True);
  Flags := [];
  if AParent <> nil then
  begin
    FInspector := AParent.Inspector;
    AParent.Add(Self)
  end;
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
      if not IsInitialized or (DisplayValue <> NewValue) then
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
            if IsInitialized then
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
  Result := not IsCategory and not ReadOnly and not Inspector.ReadOnly and IsInitialized;
end;

procedure TJvCustomInspectorItem.CheckAccess;
begin
  if not IsInitialized then
    raise EJvInspectorData.CreateRes(@RsEJvInspDataNotInit);
end;

function TJvCustomInspectorItem.GetAsOrdinal: Int64;
begin
  CheckAccess;
  if Assigned(FOnGetAsOrdinal) then
    FOnGetAsOrdinal(Self, Result)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
end;

function TJvCustomInspectorItem.GetAsString: string;
begin
  CheckAccess;
  if Assigned(FOnGetAsString) then
    FOnGetAsString(Self, Result)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
end;

procedure TJvCustomInspectorItem.SetAsOrdinal(Value: Int64);
begin
  CheckAccess;
  if Assigned(FOnSetAsOrdinal) then
    FOnSetAsOrdinal(Self, Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
  InvalidateItem;
end;

procedure TJvCustomInspectorItem.SetAsString(Value: string);
begin
  CheckAccess;
  if Assigned(FOnSetAsString) then
    FOnSetAsString(Self, Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
  InvalidateItem;
end;

function TJvCustomInspectorItem.IsInitialized: Boolean;
begin
  Result := Assigned(OnGetAsOrdinal) or Assigned(OnGetAsString);
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
            // Only Up/Down navigate the value list; any other key the
            // drop-down logic did not consume keeps its normal meaning, such
            // as Alt+F4
            if (Msg.Msg = WM_KEYDOWN) and
              (TWMKeyDown(Msg).CharCode in [VK_UP, VK_DOWN]) then
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
    EditCtrl.SelectAll;
  end;
  if Msg.Msg = WM_MOUSEWHEEL then
  begin
    if not DroppedDown then
      PostMessage(Inspector.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
    Msg.Result := 1;
    ExecInherited := False;
  end;
  if ExecInherited then
    EditWndPrc(Msg);
  if Msg.Msg = WM_KILLFOCUS then
    EditKillFocus(Self);
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
  if IsCategory then
    Result := Self
  else
  begin
    Result := Parent;
    while (Result <> nil) and not Result.IsCategory do
      Result := Result.Parent;
  end;
end;

function TJvCustomInspectorItem.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvCustomInspectorItem.GetDisplayValue: string;
begin
  Result := '';
end;

function TJvCustomInspectorItem.GetExpanded: Boolean;
begin
  Result := iifExpanded in Flags;
end;

function TJvCustomInspectorItem.GetItems(const I: Integer): TJvCustomInspectorItem;
begin
  Result := TJvCustomInspectorItem(FItems[I]);
end;

function TJvCustomInspectorItem.GetLevel: Integer;
var
  Item: TJvCustomInspectorItem;
begin
  Result := -1;
  Item := Parent;
  while Item <> nil do
  begin
    Inc(Result);
    Item := Item.Parent;
  end;
end;

function TJvCustomInspectorItem.GetReadOnly: Boolean;
begin
  Result := (iifReadonly in Flags);
end;

function TJvCustomInspectorItem.GetRects(const RectKind: TInspectorPaintRect): TRect;
begin
  if LastPaintGeneration = Inspector.PaintGeneration then
    Result := FRects[RectKind]
  else
    Result := Rect(0, 0, 0, 0);
end;

procedure TJvCustomInspectorItem.GetValueList(const Strings: TStrings);
begin
  if Assigned(FOnGetValueList) then
    FOnGetValueList(Self, Strings);
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
          EditCtrl.Free;
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
  if Flags <> NewFlags then
  begin
    OldFlags := Flags;
    FFlags := NewFlags;
    OldFlags := OldFlags * [iifExpanded];
    NewFlags := NewFlags * [iifExpanded];
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

procedure TJvCustomInspectorItem.SetRects(const RectKind: TInspectorPaintRect;
  Value: TRect);
begin
  FLastPaintGen := Inspector.PaintGeneration;
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
    if IsInitialized then
      EditCtrl.Text := DisplayValue
    else
      EditCtrl.Text := '';
    EditCtrl.Modified := False;
    EditCtrl.SelectAll;
  end;
end;

procedure TJvCustomInspectorItem.Add(const Item: TJvCustomInspectorItem);
begin
  Item.FParent := Self;
  FItems.Add(Item);
  InvalidateList;
end;

procedure TJvCustomInspectorItem.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if Parent <> nil then
    Parent.FItems.Remove(Self);
  if (Inspector <> nil) and (Inspector.Root <> Self) then
    DoneEdit(True);
  FItems.Free;
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
  try
    if not IsInitialized then
      S := RsJvInspItemUnInitialized
    else
      S := DisplayValue;
  except
    S := RsJvInspItemValueException + ExceptObject.ClassName + ': ' +
      Exception(ExceptObject).Message;
  end;
  ARect := Rects[iprValue];
  SafeColor := ACanvas.Brush.Color;
  if Editing then
    ACanvas.Brush.Color := Inspector.BackgroundColor;
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
    if Items[i].Count > 0 then
    begin
      Items[i].Expanded := AExpand;
      Items[i].ExpandItems(AExpand);
    end;
end;

//=== { TJvInspectorListBox } ================================================

type
  TJvInspectorListBox = class(TCustomListBox)
  private
    FOnValueSelect: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FNCClick: Boolean;
    FClicking: Boolean;
    FItem: TJvCustomInspectorItem;
    FSearchText: string;
    FSearchTickCount: Int64;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    property OnValueSelect: TNotifyEvent read FOnValueSelect write FOnValueSelect;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property Item: TJvCustomInspectorItem read FItem write FItem;
  end;

procedure TJvInspectorListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TJvInspectorListBox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
end;

procedure TJvInspectorListBox.KeyPress(var Key: Char);
var
  TickCount: Int64;
begin
  case Key of
    BackSpace, Esc:
      FSearchText := '';
    #32..High(Char):
      begin
        TickCount := GetTickCount64;
        if ((TickCount - FSearchTickCount >= 4000) and (FSearchText <> '')) then
          FSearchText := '';
        FSearchTickCount := TickCount;
        if Length(FSearchText) < 32 then
          FSearchText := FSearchText + Key;
        SendMessage(Handle, LB_SELECTSTRING, -1, LPARAM(PChar(FSearchText)));
        Key := #0;
      end;
  end;
  inherited KeyPress(Key);
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
  FEditing := CanEdit;
  if Editing and (FUpdateEditCtrl = 0) then
  begin
    Edit := TEdit.Create(Inspector);
    Edit.OnKeyPress := EditKeyPress;
    Edit.OnExit := EditFocusLost;
    SetEditCtrl(Edit);
    TCustomEditAccessProtected(EditCtrl).Color := Inspector.BackgroundColor;
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
    TCustomEditAccessProtected(EditCtrl).Font.Color := Inspector.ValueColor;
    // BeforeEdit is fired here, after the editor's font has been assigned, so a
    // handler can still customize that font (moved down from just after the
    // editor was created, where any font change was overwritten just above)
    if Assigned(Inspector.BeforeEdit) then
      Inspector.BeforeEdit(Inspector as TObject, Self, EditCtrl);
    EditCtrl.BoundsRect := Rects[iprEditValue];
    TCustomEditAccessProtected(EditCtrl).OnKeyDown := EditKeyDown;
    TCustomEditAccessProtected(EditCtrl).OnKeyPress := EditKeyPress;
    TCustomEditAccessProtected(EditCtrl).OnMouseDown := EditMouseDown;
    EditCtrl.Visible := True;
    if IsInitialized then
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
       (not IsInitialized or (DisplayValue <> EditCtrl.Text)) then
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

procedure TJvCustomInspectorItem.ScrollInView;
var
  ViewIdx: Integer;
  YDelta: Integer;
begin
  if not Assigned(Inspector) then
    Exit;
  if csDestroying in Inspector.ComponentState then
    Exit; // bugfix attempt. WAP.Self

  ViewIdx := Inspector.FVisibleList.IndexOfObject(Self);
  if Inspector.TopIndex > ViewIdx then
    Inspector.TopIndex := ViewIdx
  else
  if (Inspector.IdxToY(ViewIdx) - Inspector.IdxToY(Inspector.TopIndex) + Inspector.ItemHeight) > Inspector.ClientHeight then
  begin
    YDelta := Inspector.IdxToY(ViewIdx) + Inspector.ItemHeight - Inspector.ClientHeight - Inspector.IdxToY(Inspector.TopIndex);
    ViewIdx := Inspector.TopIndex + (YDelta + Inspector.ItemHeight - 1) div Inspector.ItemHeight;
    if ViewIdx < Inspector.VisibleCount then
      Inspector.TopIndex := ViewIdx;
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
  NewFlags := Value - [iifValueList] + [iifReadonly];
  inherited SetFlags(NewFlags);
end;

//=== { TJvInspectorStringItem } =============================================

function TJvInspectorStringItem.GetDisplayValue: string;
begin
  Result := AsString;
end;

procedure TJvInspectorStringItem.SetDisplayValue(const Value: string);
begin
  AsString := Value;
end;

//=== { TJvInspectorBooleanItem } ============================================

procedure TJvInspectorBooleanItem.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Bool: Boolean;
begin
  Bool := not (AsOrdinal <> Ord(False));
  if Editing and (Shift = []) and (Key = VK_SPACE) then
  begin
    AsOrdinal := Ord(Bool);
    InvalidateItem;
  end;
end;

procedure TJvInspectorBooleanItem.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Bool: Boolean;
begin
  if IsInitialized then
    Bool := not (AsOrdinal <> Ord(False))
  else
    Bool := True;
  if ssDouble in Shift then
    Shift := Shift - [ssDouble];
  if PtInRect(FCheckRect, Point(X, Y)) and (Shift = [ssLeft]) and Editing then
  begin
    AsOrdinal := Ord(Bool);
    InvalidateItem;
  end;
end;

procedure TJvInspectorBooleanItem.DoneEdit(const CancelEdits: Boolean = False);
begin
  FEditing := False;
end;

procedure TJvInspectorBooleanItem.DrawValue(const ACanvas: TCanvas);
var
  Bool: Boolean;
  ARect: TRect;
  SaveIndex: Integer;
  BoxSize: Integer;
  BFlags: UINT;
  LabelText: string;
  LabelRect: TRect;
begin
  if IsInitialized then
    Bool := AsOrdinal <> Ord(False)
  else
    Bool := False;

  if Editing and IsInitialized then
    ACanvas.Brush.Color := Inspector.BackgroundColor;
  ACanvas.FillRect(Rects[iprValueArea]);
  BoxSize := MulDiv(13, Inspector.CurrentPPI, 96);
  ARect := Rects[iprValueArea];
  Inc(ARect.Left, MulDiv(2, Inspector.CurrentPPI, 96));
  Inc(ARect.Top, (RectHeight(ARect) - BoxSize) div 2);
  ARect.Right := ARect.Left + BoxSize;
  ARect.Bottom := ARect.Top + BoxSize;
  { Clip all outside of the item rectangle }
  IntersectRect(FCheckRect, ARect, Rects[iprValueArea]);
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, FCheckRect.Left, FCheckRect.Top,
      FCheckRect.Right, FCheckRect.Bottom);
    BFlags := DFCS_BUTTONCHECK;
    if Bool then
      BFlags := BFlags or DFCS_CHECKED;
    DrawThemedFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, BFlags, Inspector.CurrentPPI);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
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
  FEditing := CanEdit;
end;

end.
