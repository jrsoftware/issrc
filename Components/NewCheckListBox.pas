unit NewCheckListBox;

{ TNewCheckListBox by Martijn Laan for Inno Setup

  Based on TPBCheckListBox by Patrick Brisacier and TCheckListBox by Borland

  Group item support, child item support, exclusive item support,
  ShowLines support and 'WantTabs mode' by Alex Yackimoff.

  Note: TNewCheckListBox uses Items.Objects to store the item state. Don't use
  Item.Objects yourself, use ItemObject instead.

  Define VCLSTYLES for full VCL Styles support.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF VCLSTYLES} Vcl.Themes, {$ELSE} Themes, {$ENDIF}
  StdCtrls, NewUxTheme;

type
  TItemType = (itGroup, itCheck, itRadio);
  TCheckBoxState2 = (cb2Normal, cb2Hot, cb2Pressed, cb2Disabled);

  TItemState = class(TObject)
  public
    Enabled: Boolean;
    HasInternalChildren: Boolean;
    CheckWhenParentChecked: Boolean;
    IsLastChild: Boolean;
    ItemType: TItemType;
    Level: Byte;
    Obj: TObject;
    State: TCheckBoxState;
    SubItem: string;
    ThreadCache: set of Byte;
    MeasuredHeight: Integer;
    ItemFontStyle: TFontStyles;
    SubItemFontStyle: TFontStyles;
  end;

  TCheckItemOperation = (coUncheck, coCheck, coCheckWithChildren); 
  TEnumChildrenProc = procedure(Index: Integer; HasChildren: Boolean; Ext: NativeInt) of object;

  TNewCheckListBox = class(TCustomListBox)
  private
    FCaptureIndex: Integer;
    FSpaceDown: Boolean;
    FCheckHeight: Integer;
    FCheckWidth: Integer;
    FFormFocusChanged: Boolean;
    FFlat: Boolean;
    FLastMouseMoveIndex: Integer;
    FMinItemHeight: Integer;
    FOffset: Integer;
    FOnClickCheck: TNotifyEvent;
    FRequireRadioSelection: Boolean;
    FShowLines: Boolean;
    FStateList: TList;
    FWantTabs: Boolean;
    FThemeData: HTHEME;
    FThreadsUpToDate: Boolean;
    FHotIndex: Integer;
    FDisableItemStateDeletion: Integer;
    FDisableStyledButtons: Boolean;
    class constructor Create;
    class destructor Destroy;
    class var FComplexParentBackground: Boolean;
    procedure UpdateThemeData(const Close, Open: Boolean);
    function CanFocusItem(Item: Integer): Boolean;
    function CheckPotentialRadioParents(Index, ALevel: Integer): Boolean;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMWantSpecialKey(var Message: TMessage); message CM_WANTSPECIALKEY;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure EndCapture(Cancel: Boolean);
    function AddItem2(AType: TItemType; const ACaption, ASubItem: string;
      ALevel: Byte; AChecked, AEnabled, AHasInternalChildren,
      ACheckWhenParentChecked: Boolean; AObject: TObject): Integer;
    function FindAccel(VK: Word): Integer;
    function FindCheckedSibling(const AIndex: Integer): Integer;
    function FindNextItem(StartFrom: Integer; GoForward,
      SkipUncheckedRadios: Boolean): Integer;
    function GetItemState(Index: Integer): TItemState;
    procedure HandleScroll;
    procedure InvalidateCheck(Index: Integer);
    function RemeasureItem(Index: Integer): Integer;
    procedure RemeasureItemAndUpdate(Index: Integer);
    procedure Toggle(Index: Integer);
    procedure UpdateScrollRange;
    procedure LBDeleteString(var Message: TMessage); message LB_DELETESTRING;
    procedure LBResetContent(var Message: TMessage); message LB_RESETCONTENT;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMGetObject(var Message: TMessage); message WM_GETOBJECT;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  protected
    FAccObjectInstance: TObject;
    procedure CreateWnd; override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
      override;
    function GetCaption(Index: Integer): String;
    function GetChecked(Index: Integer): Boolean;
    function GetItemEnabled(Index: Integer): Boolean;
    function GetItemFontStyle(Index: Integer): TFontStyles;
    function GetLevel(Index: Integer): Byte;
    function GetObject(Index: Integer): TObject;
    function GetState(Index: Integer): TCheckBoxState;
    function GetSubItem(Index: Integer): string;
    function GetSubItemFontStyle(Index: Integer): TFontStyles;
    function GetTransparentIfStyled: Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure UpdateHotIndex(NewHotIndex: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetCaption(Index: Integer; const Value: String);
    procedure SetChecked(Index: Integer; const AChecked: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetItemEnabled(Index: Integer; const AEnabled: Boolean);
    procedure SetItemFontStyle(Index: Integer; const AItemFontStyle: TFontStyles);
    procedure SetItemIndex(const Value: Integer); override;
    procedure SetObject(Index: Integer; const AObject: TObject);
    procedure SetOffset(AnOffset: Integer);
    procedure SetShowLines(Value: Boolean);
    procedure SetSubItem(Index: Integer; const ASubItem: String);
    procedure SetSubItemFontStyle(Index: Integer; const ASubItemFontStyle: TFontStyles);
    property ItemStates[Index: Integer]: TItemState read GetItemState;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    destructor Destroy; override;
    function AddCheckBox(const ACaption, ASubItem: string; ALevel: Byte;
      AChecked, AEnabled, AHasInternalChildren, ACheckWhenParentChecked: Boolean;
      AObject: TObject): Integer;
    function AddGroup(const ACaption, ASubItem: string; ALevel: Byte;
      AObject: TObject): Integer;
    function AddRadioButton(const ACaption, ASubItem: string;
      ALevel: Byte; AChecked, AEnabled: Boolean; AObject: TObject): Integer;
    function CheckItem(const Index: Integer; const AOperation: TCheckItemOperation): Boolean;
    procedure EnumChildrenOf(Item: Integer; Proc: TEnumChildrenProc; Ext: NativeInt);
    function GetParentOf(Item: Integer): Integer;
    procedure UpdateThreads;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property DisableStyledButtons: Boolean read FDisableStyledButtons write FDisableStyledButtons;
    property ItemCaption[Index: Integer]: String read GetCaption write SetCaption;
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property ItemFontStyle[Index: Integer]: TFontStyles read GetItemFontStyle write SetItemFontStyle;
    property ItemLevel[Index: Integer]: Byte read GetLevel;
    property ItemObject[Index: Integer]: TObject read GetObject write SetObject;
    property ItemSubItem[Index: Integer]: string read GetSubItem write SetSubItem;
    property State[Index: Integer]: TCheckBoxState read GetState;
    property SubItemFontStyle[Index: Integer]: TFontStyles read GetSubItemFontStyle write SetSubItemFontStyle;
    property TransparentIfStyled: Boolean read GetTransparentIfStyled;
    class property ComplexParentBackground: Boolean read FComplexParentBackground write FComplexParentBackground;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Font;
    property Items;
    property MinItemHeight: Integer read FMinItemHeight write FMinItemHeight default 16;
    property Offset: Integer read FOffset write SetOffset default 4;
    property OnClick;
    property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RequireRadioSelection: Boolean read FRequireRadioSelection write FRequireRadioSelection default False;
    property ShowHint;
    property ShowLines: Boolean read FShowLines write SetShowLines default True;
    property TabOrder;
    property Visible;
    property WantTabs: Boolean read FWantTabs write FWantTabs default False;
  end;

  TNewCheckListBoxStyleHook = class(TScrollingStyleHook)
{$IFDEF VCLSTYLES}
  strict private
    FStyleColorsChecked: Boolean;
    FStyleColorsCheckedWantTabs: Boolean;
    procedure UpdateColors;
  strict protected
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
  public
    constructor Create(AControl: TWinControl); override;
{$ENDIF}
  end;

procedure Register;

implementation

uses
  UITypes, Types,
  NewUxTheme.TmSchema, BidiUtils, UnsignedFunc, NewCheckListBox.MSAA;

const
  sRadioCantHaveDisabledChildren = 'Radio item cannot have disabled child items';

type
  TWinControlAccess = class(TWinControl);

{ TNewCheckListBox }

class constructor TNewCheckListBox.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TNewCheckListBox, TNewCheckListBoxStyleHook);
end;

constructor TNewCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with TBitmap.Create do
  begin
    try
      Handle := LoadBitmap(0, PChar(OBM_CHECKBOXES));
      FCheckWidth := Width div 4;
      FCheckHeight := Height div 3;
    finally
      Free;
    end;
  end;

  FStateList := TList.Create;
  FMinItemHeight := 16;
  FOffset := 4;
  FShowLines := True;
  Style := lbOwnerDrawVariable;
  FHotIndex := -1;
  FCaptureIndex := -1;
end;

procedure TNewCheckListBox.CreateWnd;
begin
  { TCustomListBox.CreateWnd causes a LB_RESETCONTENT message to be sent when
    it's restoring FSaveItems. Increment FDisableItemStateDeletion so that
    our LB_RESETCONTENT handler doesn't delete any item states. }
  Inc(FDisableItemStateDeletion);
  try
    inherited;
  finally
    Dec(FDisableItemStateDeletion);
  end;
end;

procedure TNewCheckListBox.UpdateThemeData(const Close, Open: Boolean);
begin
  if Close then begin
    if FThemeData <> 0 then begin
      CloseThemeData(FThemeData);
      FThemeData := 0;
    end;
  end;

  if Open then begin
    if UseThemes then
      FThemeData := OpenThemeData(Handle, 'Button')
    else
      FThemeData := 0;
  end;
end;

procedure TNewCheckListBox.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  UpdateThemeData(True, True);
end;

class destructor TNewCheckListBox.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TNewCheckListBox, TNewCheckListBoxStyleHook);
end;

destructor TNewCheckListBox.Destroy;
begin
  DisconnectMSAAObject;
  if Assigned(FStateList) then begin
    for var I := FStateList.Count-1 downto 0 do
      TItemState(FStateList[I]).Free;
    FStateList.Free;
  end;
  UpdateThemeData(True, False);
  inherited Destroy;
end;

function TNewCheckListBox.AddCheckBox(const ACaption, ASubItem: string;
  ALevel: Byte; AChecked, AEnabled, AHasInternalChildren,
  ACheckWhenParentChecked: Boolean; AObject: TObject): Integer;
begin
  if not AEnabled and CheckPotentialRadioParents(Items.Count, ALevel) then
    raise Exception.Create(sRadioCantHaveDisabledChildren);
  Result := AddItem2(itCheck, ACaption, ASubItem, ALevel, AChecked, AEnabled,
    AHasInternalChildren, ACheckWhenParentChecked, AObject);
end;

function TNewCheckListBox.AddGroup(const ACaption, ASubItem: string;
  ALevel: Byte; AObject: TObject): Integer;
begin
  Result := AddItem2(itGroup, ACaption, ASubItem, ALevel, False, True, False,
    True, AObject);
end;

function TNewCheckListBox.AddRadioButton(const ACaption, ASubItem: string;
  ALevel: Byte; AChecked, AEnabled: Boolean; AObject: TObject): Integer;
begin
  if not AEnabled then
    AChecked := False;
  Result := AddItem2(itRadio, ACaption, ASubItem, ALevel, AChecked, AEnabled,
    False, True, AObject);
end;

function TNewCheckListBox.CanFocusItem(Item: Integer): Boolean;
begin
  with ItemStates[Item] do
    Result := Self.Enabled and Enabled and (ItemType <> itGroup);
end;

function TNewCheckListBox.CheckPotentialRadioParents(Index, ALevel: Integer): Boolean;
begin
  Result := True;
  Dec(Index);
  Dec(ALevel);
  while Index >= 0 do
  begin
    with ItemStates[Index] do
      if Level = ALevel then
        if ItemType = itRadio then
          Exit
        else
          Break;
    Dec(Index);
  end;
  if Index >= 0 then
  begin
    Index := GetParentOf(Index);
    while Index >= 0 do
    begin
      if ItemStates[Index].ItemType = itRadio then
        Exit;
      Index := GetParentOf(Index);
    end;
  end;
  Result := False;
end;

procedure TNewCheckListBox.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  if FWantTabs and CanFocus then
    with Message do
    begin
      I := FindAccel(CharCode);
      if I >= 0 then
      begin
        SetFocus;
        if FCaptureIndex = I then
          EndCapture(True) { We're about to toggle the captured item, so end capture, else it will be toggled twice }
        else begin
          { We're about to toggle an item which is not the captured item. Before we do
            that, toggle the other item if space is down (same as KeyUp), else cancel
            capture (=dont toggle, same as MouseUp). }
          EndCapture(not FSpaceDown); { Does nothing if there was no capture active }
        end;
        ItemIndex := I;
        Toggle(I);
        Result := 1
      end;
    end;
end;

procedure TNewCheckListBox.CMEnter(var Message: TCMEnter);
var
  GoForward, Arrows: Boolean;
begin
  if FWantTabs and FFormFocusChanged and (GetKeyState(VK_LBUTTON) >= 0) then
  begin
    if GetKeyState(VK_TAB) < 0 then begin
      Arrows := False;
      GoForward := (GetKeyState(VK_SHIFT) >= 0);
    end
    else if (GetKeyState(VK_UP) < 0) or (GetKeyState(VK_LEFT) < 0) then begin
      Arrows := True;
      GoForward := False;
    end
    else if (GetKeyState(VK_DOWN) < 0) or (GetKeyState(VK_RIGHT) < 0) then begin
      Arrows := True;
      GoForward := True;
    end
    else begin
      { Otherwise, just select the first item }
      Arrows := False;
      GoForward := True;
    end;
    if GoForward then
      ItemIndex := FindNextItem(-1, True, not Arrows)
    else
      ItemIndex := FindNextItem(Items.Count, False, not Arrows)
  end;
  inherited;
end;

procedure TNewCheckListBox.CMExit(var Message: TCMExit);
begin
  EndCapture(not FSpaceDown or (GetKeyState(VK_MENU) >= 0));
  inherited;
end;

procedure TNewCheckListBox.CMFocusChanged(var Message: TCMFocusChanged);
begin
  FFormFocusChanged := True;
  inherited;
end;

procedure TNewCheckListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Font;
end;

procedure LineDDAProc(X, Y: Integer; Canvas: TCanvas); stdcall;
begin
  if ((X xor Y) and 1) = 0 then
  begin
    Canvas.MoveTo(X, Y);
    Canvas.LineTo(X + 1, Y);
  end;
end;

procedure TNewCheckListBox.CMWantSpecialKey(var Message: TMessage);
begin
  Message.Result := Ord(FWantTabs and (Message.WParam = VK_TAB));
end;

procedure TNewCheckListBox.CNDrawItem(var Message: TWMDrawItem);
begin
  with Message.DrawItemStruct^ do
  begin
    { Note: itemID is -1 when there are no items }
    if Integer(itemID) >= 0 then begin
      var L := ItemStates[Integer(itemID)].Level;
      if ItemStates[Integer(itemID)].ItemType <> itGroup then Inc(L);
      rcItem.Left := rcItem.Left + (FCheckWidth + 2 * FOffset) * L;
      FlipRect(rcItem, ClientRect, IsRightToLeft);
    end;
    { Don't let TCustomListBox.CNDrawItem draw the focus }
    if FWantTabs or
      (SendMessage(Handle, WM_QUERYUISTATE, 0, 0) and UISF_HIDEFOCUS <> 0) then
      itemState := itemState and not ODS_FOCUS;
    inherited;
  end;
end;

function TNewCheckListBox.RemeasureItem(Index: Integer): Integer;
{ Recalculates an item's height. Does not repaint and does not update the
  vertical scroll range (as the LB_SETITEMHEIGHT message does neither). }
begin
  Result := ItemHeight;
  MeasureItem(Index, Result);
  SendMessage(Handle, LB_SETITEMHEIGHT, Index, Result);
end;

procedure TNewCheckListBox.UpdateScrollRange;
{ Updates the vertical scroll range, hiding/showing the scroll bar if needed.
  This should be called after any RemeasureItem call. }
begin
  { Update the scroll bounds by sending a seemingly-ineffectual LB_SETTOPINDEX
    message. This works on Windows 95 and 2000.
    NOTE: This causes the selected item to be repainted for no apparent reason!
    I wish I knew of a better way to do this... }
  SendMessage(Handle, LB_SETTOPINDEX, SendMessage(Handle, LB_GETTOPINDEX, 0, 0), 0);
end;

procedure TNewCheckListBox.MeasureItem(Index: Integer; var Height: Integer);
var
  Rect, SubItemRect: TRect;
  ItemState: TItemState;
  L, SubItemWidth: Integer;
  S: String;
begin
  with Canvas do begin
    ItemState := ItemStates[Index];
    Rect := Classes.Rect(0, 0, ClientWidth, 0);

    L := ItemState.Level;
    if ItemState.ItemType <> itGroup then
      Inc(L);
    Rect.Left := Rect.Left + (FCheckWidth + 2 * FOffset) * L;
    Inc(Rect.Left);

    if ItemState.SubItem <> '' then begin
      const DrawTextFormat = UDrawTextBiDiModeFlags(Self, DT_CALCRECT or DT_NOCLIP or DT_NOPREFIX or DT_SINGLELINE);
      Font.Style := ItemState.SubItemFontStyle;
      SetRectEmpty(SubItemRect);
      DrawText(Canvas.Handle, PChar(ItemState.SubItem), Length(ItemState.SubItem),
        SubItemRect, DrawTextFormat);
      SubItemWidth := SubItemRect.Right + 2 * FOffset;
      Dec(Rect.Right, SubItemWidth);
    end else
      Dec(Rect.Right, FOffset);

    if not FWantTabs then
      Inc(Rect.Left);

    var DrawTextFormat: UINT := DT_NOCLIP or DT_CALCRECT or DT_WORDBREAK or DT_WORD_ELLIPSIS;
    if not FWantTabs or (ItemState.ItemType = itGroup) then
      DrawTextFormat := DrawTextFormat or DT_NOPREFIX;
    DrawTextFormat := UDrawTextBiDiModeFlags(Self, DrawTextFormat);

    Font.Style := ItemState.ItemFontStyle;
    S := Items[Index]; { Passing Items[Index] directly into DrawText doesn't work on Unicode build. }
    ItemState.MeasuredHeight := DrawText(Canvas.Handle, PChar(S), Length(S), Rect, DrawTextFormat);
    if ItemState.MeasuredHeight < FMinItemHeight then
      Height := FMinItemHeight
    else
      Height := ItemState.MeasuredHeight + 4;

    { The height must be an even number for tree lines to be painted correctly }
    if Odd(Height) then
      Inc(Height);
  end;
end;

const
  ColorStates: array[Boolean] of TStyleColor = (scListBoxDisabled, scListBox);
  TextLabelFontColorStates: array[Boolean] of TStyleFont = (sfTextLabelDisabled, sfTextLabelNormal);
  ListItemFontColorStates: array[Boolean] of TStyleFont = (sfListItemTextDisabled, sfListItemTextNormal);

procedure TNewCheckListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  ButtonStates: array [TItemType] of UINT =
  (
    0,
    DFCS_BUTTONCHECK,
    DFCS_BUTTONRADIO
  );
  ButtonPartIds: array [TItemType] of Integer =
  (
    0,
    BP_CHECKBOX,
    BP_RADIOBUTTON
  );
  ButtonStateIds: array [TCheckBoxState, TCheckBoxState2] of Integer =
  (
    //Can be used for both checkboxes and radiobuttons because RBS_... constants
    //equal CBS_... constants
    (CBS_UNCHECKEDNORMAL, CBS_UNCHECKEDHOT, CBS_UNCHECKEDPRESSED, CBS_UNCHECKEDDISABLED),
    (CBS_CHECKEDNORMAL, CBS_CHECKEDHOT, CBS_CHECKEDPRESSED, CBS_CHECKEDDISABLED),
    (CBS_MIXEDNORMAL, CBS_MIXEDHOT, CBS_MIXEDPRESSED, CBS_MIXEDDISABLED)
  );
  CheckListItemStates: array[Boolean] of TThemedCheckListBox = (tclListItemDisabled, tclListItemNormal);
  CheckBoxCheckedStates: array[Boolean] of TThemedButton = (tbCheckBoxCheckedDisabled, tbCheckBoxCheckedNormal);
  CheckBoxUncheckedStates: array[Boolean] of TThemedButton = (tbCheckBoxUncheckedDisabled, tbCheckBoxUncheckedNormal);
  CheckBoxMixedStates: array[Boolean] of TThemedButton = (tbCheckBoxMixedDisabled, tbCheckBoxMixedNormal);
  RadioButtonCheckedStates: array[Boolean] of TThemedButton = (tbRadioButtonCheckedDisabled, tbRadioButtonCheckedNormal);
  RadioButtonUncheckedStates: array[Boolean] of TThemedButton = (tbRadioButtonUncheckedDisabled, tbRadioButtonUncheckedNormal);
var
  SavedClientRect: TRect;

  function FlipX(const X: Integer): Integer;
  begin
    if IsRightToLeft then
      Result := (SavedClientRect.Right - 1) - X
    else
      Result := X;
  end;

  procedure InternalDrawText(const S: string; var R: TRect; Format: UINT;
    Embossed: Boolean);
  begin
    if Embossed then
    begin
      Canvas.Brush.Style := bsClear;
      OffsetRect(R, 1, 1);
      SetTextColor(Canvas.Handle, GetSysColor(COLOR_BTNHIGHLIGHT));
      DrawText(Canvas.Handle, PChar(S), Length(S), R, Format);
      OffsetRect(R, -1, -1);
      SetTextColor(Canvas.Handle, GetSysColor(COLOR_BTNSHADOW));
      DrawText(Canvas.Handle, PChar(S), Length(S), R, Format);
    end
    else
      DrawText(Canvas.Handle, PChar(S), Length(S), R, Format);
  end;

var
  ItemDisabled: Boolean;
  I, ThreadPosX, ThreadBottom, ThreadLevel, ItemMiddle: Integer;
  CheckRect, SubItemRect, FocusRect: TRect;
  NewTextColor: TColor;
  ItemState: TItemState;
  SubItemWidth: Integer;
  PartId, StateId: Integer;
  Size: TSize;
begin
  if FShowLines and not FThreadsUpToDate then begin
    UpdateThreads;
    FThreadsUpToDate := True;
  end;

  SavedClientRect := ClientRect;
  { Undo flipping performed by TNewCheckListBox.CNDrawItem }
  FlipRect(Rect, SavedClientRect, IsRightToLeft);

  ItemState := ItemStates[Index];
  const UIState = SendMessage(Handle, WM_QUERYUISTATE, 0, 0);
  ItemDisabled := not Enabled or not ItemState.Enabled;

  { Style code below is based on Vcl.StdCtrls' TCustomListBox.CNDrawItem and Vcl.CheckLst's
    TCustomCheckListBox.DrawItem and .DrawCheck }
  var LStyle := StyleServices(Self);
  if not LStyle.Enabled or LStyle.IsSystemStyle then
    LStyle := nil;

  with Canvas do begin { From now on Handle refers to Canvas.Handle! }
    { Initialize colors }
    if not FWantTabs and (odSelected in State) and Focused then begin
      NewTextColor := clHighlightText;
      if (LStyle <> nil) and (seClient in StyleElements) then begin
        Brush.Color := LStyle.GetSystemColor(clHighlight);
        if seFont in StyleElements then
          NewTextColor := LStyle.GetStyleFontColor(sfListItemTextSelected);
      end else
        Brush.Color := clHighlight;
    end else begin
      if ItemDisabled then
        NewTextColor := clGrayText
      else
        NewTextColor := Self.Font.Color;
      if (LStyle <> nil) and (seClient in StyleElements) then begin
        if FWantTabs then
          Brush.Color := LStyle.GetStyleColor(scWindow)
        else
          Brush.Color := LStyle.GetStyleColor(ColorStates[Enabled]);
        if seFont in StyleElements then begin
          if FWantTabs then
            NewTextColor := LStyle.GetStyleFontColor(TextLabelFontColorStates[not ItemDisabled])
          else
            NewTextColor := LStyle.GetStyleFontColor(ListItemFontColorStates[not ItemDisabled]);
          const Details = LStyle.GetElementDetails(CheckListItemStates[not ItemDisabled]);
          var LColor: TColor;
          if LStyle.GetElementColor(Details, ecTextColor, LColor) and (LColor <> clNone) then
            NewTextColor := LColor;
        end;
      end else
        Brush.Color := Self.Color;
    end;
    { Draw threads }
    if FShowLines then begin
      Pen.Color := clGrayText;
      ThreadLevel := ItemLevel[Index];
      for I := 0 to ThreadLevel - 1 do
        if I in ItemStates[Index].ThreadCache then begin
          ThreadPosX := (FCheckWidth + 2 * FOffset) * I + FCheckWidth div 2 + FOffset;
          ItemMiddle := (Rect.Bottom - Rect.Top) div 2 + Rect.Top;
          ThreadBottom := Rect.Bottom;
          if I = ThreadLevel - 1 then begin
            if ItemStates[Index].IsLastChild then
              ThreadBottom := ItemMiddle;
            LineDDA(FlipX(ThreadPosX), ItemMiddle, FlipX(ThreadPosX + FCheckWidth div 2 + FOffset),
              ItemMiddle, @LineDDAProc, LPARAM(Canvas));
          end;
          LineDDA(FlipX(ThreadPosX), Rect.Top, FlipX(ThreadPosX), ThreadBottom,
            @LineDDAProc, LPARAM(Canvas));
        end;
    end;
    { Draw checkmark}
    if ItemState.ItemType <> itGroup then begin
      CheckRect := Bounds(Rect.Left - (FCheckWidth + FOffset),
        Rect.Top + ((Rect.Bottom - Rect.Top - FCheckHeight) div 2),
        FCheckWidth, FCheckHeight);
      FlipRect(CheckRect, SavedClientRect, IsRightToLeft);
      if (LStyle <> nil) and not FDisableStyledButtons then begin
        var Detail: TThemedButton;
        if ItemState.State <> cbGrayed then begin
          if ItemState.ItemType = itCheck then begin
            if ItemState.State = cbChecked then
              Detail := CheckBoxCheckedStates[not ItemDisabled]
            else
              Detail := CheckBoxUncheckedStates[not ItemDisabled];
          end else begin
            if ItemState.State = cbChecked then
              Detail := RadioButtonCheckedStates[not ItemDisabled]
            else
              Detail := RadioButtonUncheckedStates[not ItemDisabled];
          end;
        end else
          Detail := CheckBoxMixedStates[not ItemDisabled];
        const ElementDetails = LStyle.GetElementDetails(Detail);
        const SaveColor = Brush.Color;
        const SaveIndex = SaveDC(Handle); { With VCL Styles active, DrawElement changes the DC's selected objects and colors, and does not restore them }
        try
          LStyle.DrawElement(Handle, ElementDetails, CheckRect, nil, CurrentPPI);
        finally
          RestoreDC(Handle, SaveIndex);
        end;
        Brush.Color := SaveColor;
      end else if FThemeData = 0 then begin
        var uState: UINT;
        case ItemState.State of
          cbChecked: uState := ButtonStates[ItemState.ItemType] or DFCS_CHECKED;
          cbUnchecked: uState := ButtonStates[ItemState.ItemType];
          else
            uState := DFCS_BUTTON3STATE or DFCS_CHECKED;
        end;
        if FFlat then
          uState := uState or DFCS_FLAT;
        if ItemDisabled then
          uState := uState or DFCS_INACTIVE;
        if (FCaptureIndex = Index) and (FSpaceDown or (FLastMouseMoveIndex = Index)) then
          uState := uState or DFCS_PUSHED;
        DrawFrameControl(Handle, CheckRect, DFC_BUTTON, uState);
      end else begin
        PartId := ButtonPartIds[ItemState.ItemType];
        if ItemDisabled then
          StateId := ButtonStateIds[ItemState.State][cb2Disabled]
        else if Index = FCaptureIndex then
          if FSpaceDown or (FLastMouseMoveIndex = Index) then
            StateId := ButtonStateIds[ItemState.State][cb2Pressed]
          else
            StateId := ButtonStateIds[ItemState.State][cb2Hot]
        else if (FCaptureIndex < 0) and (Index = FHotIndex) then
          StateId := ButtonStateIds[ItemState.State][cb2Hot]
        else
          StateId := ButtonStateIds[ItemState.State][cb2Normal];
        if ((GetThemePartSize(FThemeData, Handle, PartId, StateId, @CheckRect, TS_TRUE, Size)) = S_OK) and
           ((Size.cx <> FCheckWidth) or (Size.cy <> FCheckHeight)) then begin
          CheckRect := Bounds(Rect.Left - (Size.cx + FOffset),
            Rect.Top + ((Rect.Bottom - Rect.Top - Size.cy) div 2),
            Size.cx, Size.cy);
          FlipRect(CheckRect, SavedClientRect, IsRightToLeft);
        end;
        //if IsThemeBackgroundPartiallyTransparent(FThemeData, PartId, StateId) then
        //  DrawThemeParentBackground(Self.Handle, Handle, @CheckRect);
        DrawThemeBackGround(FThemeData, Handle, PartId, StateId, CheckRect, @CheckRect);
      end;
    end;
    { Draw background & subitem }
    FlipRect(Rect, SavedClientRect, IsRightToLeft);
    if TransparentIfStyled and (LStyle <> nil) then begin
      { Same method as TTrackBar.CNNotify uses }
      const Rgn = CreateRectRgn(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
      SelectClipRgn(Handle, Rgn);
      LStyle.DrawParentBackground(Self.Handle, Handle, nil, False, Rect);
      DeleteObject(Rgn);
      SelectClipRgn(Handle, 0);
    end else
      FillRect(Rect);
    FlipRect(Rect, SavedClientRect, IsRightToLeft);
    Inc(Rect.Left);
    const OldColor = SetTextColor(Handle, UColorToRGB(NewTextColor));
    if ItemState.SubItem <> '' then
    begin
      const DrawTextFormat = UDrawTextBiDiModeFlags(Self, DT_NOCLIP or DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER);
      Font.Style := ItemState.SubItemFontStyle;
      Font.Color := NewTextColor; { Setting Font.Style may invalidate the font, requiring us to reset Color regardless of the SetTextColor call above }
      SetRectEmpty(SubItemRect);
      InternalDrawText(ItemState.SubItem, SubItemRect, DrawTextFormat or
        DT_CALCRECT, False);
      SubItemWidth := SubItemRect.Right + 2 * FOffset;
      SubItemRect := Rect;
      SubItemRect.Left := SubItemRect.Right - SubItemWidth + FOffset;
      FlipRect(SubItemRect, SavedClientRect, IsRightToLeft);
      InternalDrawText(ItemState.SubItem, SubItemRect, DrawTextFormat,
        FWantTabs and ItemDisabled);
      Dec(Rect.Right, SubItemWidth);
    end
    else
      Dec(Rect.Right, FOffset);
    { Draw item text }
    if not FWantTabs then
      Inc(Rect.Left);
    OffsetRect(Rect, 0, (Rect.Bottom - Rect.Top - ItemState.MeasuredHeight) div 2);
    var DrawTextFormat: UINT := DT_NOCLIP or DT_WORDBREAK or DT_WORD_ELLIPSIS;
    if not FWantTabs or (ItemState.ItemType = itGroup) then
      DrawTextFormat := DrawTextFormat or DT_NOPREFIX;
    if (UIState and UISF_HIDEACCEL) <> 0 then
      DrawTextFormat := DrawTextFormat or DT_HIDEPREFIX;
    DrawTextFormat := UDrawTextBiDiModeFlags(Self, DrawTextFormat);
    Font.Style := ItemState.ItemFontStyle;
    Font.Color := NewTextColor; { See above }
    { When you call DrawText with the DT_CALCRECT flag and there's a word wider
      than the rectangle width, it increases the rectangle width and wraps
      at the new Right point. On the other hand, when you call DrawText
      _without_ the DT_CALCRECT flag, it always wraps at the Right point you
      specify -- it doesn't check for long words first.
      Therefore, to ensure we wrap at the same place when drawing as when
      measuring, pass our rectangle to DrawText with DT_CALCRECT first.
      Wrapping at the same place is important because it can affect how many
      lines are drawn -- and we mustn't draw too many. }
    InternalDrawText(Items[Index], Rect, DrawTextFormat or DT_CALCRECT, False);
    FlipRect(Rect, SavedClientRect, IsRightToLeft);
    const Embossed = FWantTabs and ItemDisabled and (LStyle = nil);
    if TransparentIfStyled and (LStyle <> nil) then begin
      const OldBkMode = SetBkMode(Handle, Windows.TRANSPARENT);
      InternalDrawText(Items[Index], Rect, DrawTextFormat, Embossed);
      SetBkMode(Handle, OldBkMode);
    end else
      InternalDrawText(Items[Index], Rect, DrawTextFormat, Embossed);
    { Draw focus rectangle }
    if FWantTabs and not ItemDisabled and (odSelected in State) and Focused and
      (UIState and UISF_HIDEFOCUS = 0) then
    begin
      FocusRect := Rect;
      InflateRect(FocusRect, 1, 1);
      DrawFocusRect(FocusRect);
    end;
    SetTextColor(Handle, OldColor);
  end;
end;

procedure TNewCheckListBox.EndCapture(Cancel: Boolean);
var
  InvalidateItem: Boolean;
  Item: Integer;
begin
  Item := FCaptureIndex;
  if Item >= 0 then
  begin
    InvalidateItem := FSpaceDown or (FCaptureIndex = FLastMouseMoveIndex) or (FThemeData <> 0);
    FSpaceDown := False;
    FCaptureIndex := -1;
    FLastMouseMoveIndex := -1;
    if not Cancel then
      Toggle(Item);
    if InvalidateItem then
      InvalidateCheck(Item);
  end;
  if MouseCapture then
    MouseCapture := False;
end;

procedure TNewCheckListBox.EnumChildrenOf(Item: Integer; Proc: TEnumChildrenProc;
  Ext: NativeInt);
var
  L: Integer;
begin
  if (Item < -1) or (Item >= Items.Count) then
    Exit;
  if Item = -1 then
  begin
    L := 0;
    Item := 0;
  end
  else
  begin
    L := ItemLevel[Item] + 1;
    Inc(Item);
  end;
  while (Item < Items.Count) and (ItemLevel[Item] >= L) do
  begin
    if ItemLevel[Item] = L then
      Proc(Item, (Item < Items.Count - 1) and (ItemLevel[Item + 1] > L), Ext);
    Inc(Item);
  end;
end;

function TNewCheckListBox.AddItem2(AType: TItemType;
  const ACaption, ASubItem: string; ALevel: Byte;
  AChecked, AEnabled, AHasInternalChildren, ACheckWhenParentChecked: Boolean;
  AObject: TObject): Integer;
var
  ItemState: TItemState;
  I: Integer;
begin
  if Items.Count <> FStateList.Count then  { sanity check }
    raise Exception.Create('List item and state item count mismatch');
  if Items.Count > 0 then
  begin
    if ItemLevel[Items.Count - 1] + 1 < ALevel then
      ALevel := Byte(ItemLevel[Items.Count - 1] + 1);
  end
  else
    ALevel := 0;
  FThreadsUpToDate := False;
  { Use our own grow code to minimize heap fragmentation }
  if FStateList.Count = FStateList.Capacity then begin
    if FStateList.Capacity < 64 then
      FStateList.Capacity := 64
    else
      FStateList.Capacity := FStateList.Capacity * 2;
  end;
  ItemState := TItemState.Create;
  try
    ItemState.ItemType := AType;
    ItemState.Enabled := AEnabled;
    ItemState.Obj := AObject;
    ItemState.Level := ALevel;
    ItemState.SubItem := ASubItem;
    ItemState.HasInternalChildren := AHasInternalChildren;
    ItemState.CheckWhenParentChecked := ACheckWhenParentChecked;
  except
    ItemState.Free;
    raise;
  end;
  FStateList.Add(ItemState);
  try
    Result := Items.Add(ACaption);
  except
    FStateList.Delete(FStateList.Count-1);
    ItemState.Free;
    raise;
  end;
  { If the first item in a radio group is being added, and it is top-level or
    has a checked parent, force it to be checked. (We don't want to allow radio
    groups with no selection.) }
  if (AType = itRadio) and not AChecked and AEnabled then begin
    I := GetParentOf(Result);
    { FRequireRadioSelection only affects top-level items; we never allow
      child radio groups with no selection (because nobody should need that) }
    if FRequireRadioSelection or (I <> -1) then
      if (I = -1) or (GetState(I) <> cbUnchecked) then
        if FindCheckedSibling(Result) = -1 then
          AChecked := True;
  end;
  SetChecked(Result, AChecked);
end;

function TNewCheckListBox.FindAccel(VK: Word): Integer;
begin
  for Result := 0 to Items.Count - 1 do
    if CanFocusItem(Result) and IsAccel(VK, Items[Result]) then
      Exit;
  Result := -1;
end;

function TNewCheckListBox.FindNextItem(StartFrom: Integer; GoForward,
  SkipUncheckedRadios: Boolean): Integer;

  function ShouldSkip(Index: Integer): Boolean;
  begin
    with ItemStates[Index] do
      Result := (ItemType = itRadio) and (State <> cbChecked)
  end;

var
  Delta: Integer;
begin
  if StartFrom < -1 then
    StartFrom := ItemIndex;
  if Items.Count > 0 then
  begin
    Delta := Ord(GoForward) * 2 - 1;
    Result := StartFrom + Delta;
    while (Result >= 0) and (Result < Items.Count) and
      (not CanFocusItem(Result) or SkipUncheckedRadios and ShouldSkip(Result)) do
      Result := Result + Delta;
    if (Result < 0) or (Result >= Items.Count) then
      Result := -1;
  end
  else
    Result := -1;
end;

function TNewCheckListBox.GetCaption(Index: Integer): String;
begin
  Result := Items[Index];
end;

function TNewCheckListBox.GetChecked(Index: Integer): Boolean;
begin
  Result := GetState(Index) <> cbUnchecked;
end;

function TNewCheckListBox.GetItemEnabled(Index: Integer): Boolean;
begin
  Result := ItemStates[Index].Enabled;
end;

function TNewCheckListBox.GetItemFontStyle(Index: Integer): TFontStyles;
begin
  Result := ItemStates[Index].ItemFontStyle;
end;

function TNewCheckListBox.GetItemState(Index: Integer): TItemState;
begin
  Result := FStateList[Index];
end;

function TNewCheckListBox.GetLevel(Index: Integer): Byte;
begin
  Result := ItemStates[Index].Level;
end;

function TNewCheckListBox.GetObject(Index: Integer): TObject;
begin
  Result := ItemStates[Index].Obj;
end;

function TNewCheckListBox.GetParentOf(Item: Integer): Integer;
{ Gets index of Item's parent, or -1 if there is none. }
var
  Level, I: Integer;
begin
  Level := ItemStates[Item].Level;
  if Level > 0 then
    for I := Item-1 downto 0 do begin
      if ItemStates[I].Level < Level then begin
        Result := I;
        Exit;
      end;
    end;
  Result := -1;
end;

function TNewCheckListBox.GetState(Index: Integer): TCheckBoxState;
begin
  Result := ItemStates[Index].State;
end;

function TNewCheckListBox.GetSubItem(Index: Integer): String;
begin
  Result := ItemStates[Index].SubItem;
end;

function TNewCheckListBox.GetSubItemFontStyle(Index: Integer): TFontStyles;
begin
  Result := ItemStates[Index].SubItemFontStyle;
end;

function TNewCheckListBox.GetTransparentIfStyled: Boolean;
begin
  Result := WantTabs;
end;

procedure TNewCheckListBox.HandleScroll;
begin
  { Windows copies item backgrounds when scrolling, but if the listbox is
    transparent and its parent background is complex (such as a bitmap),
    the item backgrounds need to be updated. Can be called even if it's
    not sure the list was actually scrolled. }
  if FComplexParentBackground and TransparentIfStyled and IsCustomStyleActive then begin
    var ScrollBarInfo: TScrollBarInfo;
    ScrollBarInfo.cbSize := SizeOf(ScrollBarInfo);
    if GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), ScrollBarInfo) and
       (ScrollBarInfo.rgstate[0] and STATE_SYSTEM_INVISIBLE = 0) then
      InvalidateRect(Handle, nil, True);
  end;
end;

procedure TNewCheckListBox.InvalidateCheck(Index: Integer);
var
  IRect: TRect;
begin
  IRect := ItemRect(Index);
  Inc(IRect.Left, (FCheckWidth + 2 * Offset) * (ItemLevel[Index]));
  IRect.Right := IRect.Left + (FCheckWidth + 2 * Offset);
  FlipRect(IRect, ClientRect, IsRightToLeft);
  InvalidateRect(Handle, @IRect, FThemeData <> 0);
end;

procedure TNewCheckListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) and not (ssAlt in Shift) and (ItemIndex >= 0) and
    (FCaptureIndex < 0) and CanFocusItem(ItemIndex) then
    if FWantTabs then begin
      if not FSpaceDown then begin
        FCaptureIndex := ItemIndex;
        FSpaceDown := True;
        InvalidateCheck(ItemIndex);
        if (FHotIndex <> ItemIndex) and (FHotIndex <> -1) and (FThemeData <> 0) then
          InvalidateCheck(FHotIndex);
      end;
    end
    else
      Toggle(ItemIndex);
  inherited;
end;

procedure TNewCheckListBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) and FWantTabs and FSpaceDown and (FCaptureIndex >= 0) then begin
    EndCapture(False);
    if (FHotIndex <> -1) and (FThemeData <> 0) then
      InvalidateCheck(FHotIndex);
  end;
  inherited;
end;

procedure TNewCheckListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
begin
  if Button = mbLeft then begin
    Index := ItemAtPos(Point(X, Y), True);
    if (Index <> -1) and CanFocusItem(Index) then
    begin
      if FWantTabs then begin
        if not FSpaceDown then begin
          if not MouseCapture then
            MouseCapture := True;
          FCaptureIndex := Index;
          FLastMouseMoveIndex := Index;
          InvalidateCheck(Index);
          HandleScroll; { Might have scrolled a new item into view }
        end;
      end
      else
        Toggle(Index);
    end;
  end;
  inherited;
end;

procedure TNewCheckListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
begin
  if (Button = mbLeft) and FWantTabs and not FSpaceDown and (FCaptureIndex >= 0) then
  begin
    Index := ItemAtPos(Point(X, Y), True);
    EndCapture(Index <> FCaptureIndex);
    if (FHotIndex <> -1) and (FThemeData <> 0) then
      InvalidateCheck(FHotIndex);
  end;
  inherited;
end;

procedure TNewCheckListBox.UpdateHotIndex(NewHotIndex: Integer);
var
  OldHotIndex: Integer;
begin
  OldHotIndex := FHotIndex;
  if NewHotIndex <> OldHotIndex then
  begin
    FHotIndex := NewHotIndex;
    if FCaptureIndex = -1 then begin
      if (OldHotIndex <> -1) and (FThemeData <> 0) then
        InvalidateCheck(OldHotIndex);
      if (NewHotIndex <> -1) and (FThemeData <> 0) then
        InvalidateCheck(NewHotIndex);
    end;
  end;
end;

procedure TNewCheckListBox.CMMouseLeave(var Message: TMessage);
begin
  UpdateHotIndex(-1);
  inherited;
end;

procedure TNewCheckListBox.SetCaption(Index: Integer; const Value: String);
begin
  { Changing an item's text actually involves deleting and re-inserting the
    item. Increment FDisableItemStateDeletion so the item state isn't lost. }
  Inc(FDisableItemStateDeletion);
  try
    Items[Index] := Value;
  finally
    Dec(FDisableItemStateDeletion);
  end;
end;

procedure TNewCheckListBox.SetChecked(Index: Integer; const AChecked: Boolean);
begin
  if AChecked then
    CheckItem(Index, coCheck)
  else
    CheckItem(Index, coUncheck);
end;

function TNewCheckListBox.FindCheckedSibling(const AIndex: Integer): Integer;
{ Finds a checked sibling of AIndex (which is assumed to be a radio button).
  Returns -1 if no checked sibling was found. }
var
  ThisLevel, I: Integer;
begin
  ThisLevel := ItemStates[AIndex].Level;
  for I := AIndex-1 downto 0 do begin
    if ItemStates[I].Level < ThisLevel then
      Break;
    if ItemStates[I].Level = ThisLevel then begin
      if ItemStates[I].ItemType <> itRadio then
        Break;
      if GetState(I) <> cbUnchecked then begin
        Result := I;
        Exit;
      end;
    end;
  end;
  for I := AIndex+1 to Items.Count-1 do begin
    if ItemStates[I].Level < ThisLevel then
      Break;
    if ItemStates[I].Level = ThisLevel then begin
      if ItemStates[I].ItemType <> itRadio then
        Break;
      if GetState(I) <> cbUnchecked then begin
        Result := I;
        Exit;
      end;
    end;
  end;
  Result := -1;
end;

function TNewCheckListBox.CheckItem(const Index: Integer;
  const AOperation: TCheckItemOperation): Boolean;
{ Tries to update the checked state of Index. Returns True if any changes were
  made to the state of Index or any of its children. }

  procedure SetItemState(const AIndex: Integer; const AState: TCheckBoxState);
  begin
    if ItemStates[AIndex].State <> AState then begin
      ItemStates[AIndex].State := AState;
      InvalidateCheck(AIndex);
      AnnounceStateChangeToMSAA(AIndex);
    end;
  end;

  function CalcState(const AIndex: Integer; ACheck: Boolean): TCheckBoxState;
  { Determines new state for AIndex based on desired checked state (ACheck) and
    current state of the item's immediate children. }
  var
    RootLevel, I: Integer;
    HasChecked, HasUnchecked: Boolean;
  begin
    HasChecked := False;
    HasUnchecked := False;
    RootLevel := ItemStates[AIndex].Level;
    for I := AIndex+1 to Items.Count-1 do begin
      if ItemStates[I].Level <= RootLevel then
        Break;
      if (ItemStates[I].Level = RootLevel+1) and
         (ItemStates[I].ItemType in [itCheck, itRadio]) then begin
        case GetState(I) of
          cbUnchecked: begin
              if (ItemStates[I].ItemType <> itRadio) or
                 (FindCheckedSibling(I) = -1) then
                HasUnchecked := True;
            end;
          cbChecked: begin
              HasChecked := True;
            end;
          cbGrayed: begin
              HasChecked := True;
              HasUnchecked := True;
            end;
        end;
      end;
    end;

    { If the parent is a check box with children, don't allow it to be checked
      if none of its children are checked, unless it "has internal children" }
    if HasUnchecked and not HasChecked and
       (ItemStates[AIndex].ItemType = itCheck) and
       not ItemStates[AIndex].HasInternalChildren then
      ACheck := False;

    if ACheck or HasChecked then begin
      if HasUnchecked and (ItemStates[AIndex].ItemType = itCheck) then
        Result := cbGrayed
      else
        Result := cbChecked;
    end
    else
      Result := cbUnchecked;
  end;

  function RecursiveCheck(const AIndex: Integer;
    const AOperation: TCheckItemOperation): Boolean;
  { Checks or unchecks AIndex and all enabled child items of AIndex at any
    level. In radio button groups, only one item per group is checked.
    Returns True if any of the items' states were changed. }
  var
    RootLevel, I: Integer;
    NewState: TCheckBoxState;
  begin
    Result := False;
    RootLevel := ItemStates[AIndex].Level;
    for I := AIndex+1 to Items.Count-1 do begin
      if ItemStates[I].Level <= RootLevel then
        Break;
      if (ItemStates[I].Level = RootLevel+1) and ItemStates[I].Enabled and
         ((AOperation = coUncheck) or
          ((AOperation = coCheckWithChildren) and ItemStates[I].CheckWhenParentChecked) or
          (ItemStates[I].ItemType = itRadio)) then
        { If checking and I is a radio button, don't recurse if a sibling
          already got checked in a previous iteration of this loop. This is
          needed in the following case to prevent all three radio buttons from
          being checked when "Parent check" is checked. In addition, it
          prevents "Child check" from being checked.
          [ ] Parent check
              ( ) Radio 1
              ( ) Radio 2
              ( ) Radio 3
                  [ ] Child check
        }
        if (AOperation = coUncheck) or (ItemStates[I].ItemType <> itRadio) or
           (FindCheckedSibling(I) = -1) then
          if RecursiveCheck(I, AOperation) then
            Result := True;
    end;
    NewState := CalcState(AIndex, AOperation <> coUncheck);
    if GetState(AIndex) <> NewState then begin
      SetItemState(AIndex, NewState);
      Result := True;
    end;
  end;

  procedure UncheckSiblings(const AIndex: Integer);
  { Unchecks all siblings (and their children) of AIndex, which is assumed to
    be a radio button. }
  var
    I: Integer;
  begin
    while True do begin
      I := FindCheckedSibling(AIndex);
      if (I = -1) or not RecursiveCheck(I, coUncheck) then
        Break;
    end;
  end;

  procedure EnsureChildRadioItemsHaveSelection(const AIndex: Integer);
  { Ensures all radio button groups that are immediate children of AIndex have
    a selected item. }
  var
    RootLevel, I: Integer;
  begin
    RootLevel := ItemStates[AIndex].Level;
    for I := AIndex+1 to Items.Count-1 do begin
      if ItemStates[I].Level <= RootLevel then
        Break;
      if (ItemStates[I].Level = RootLevel+1) and
         (ItemStates[I].ItemType = itRadio) and
         ItemStates[I].Enabled and
         (GetState(I) <> cbChecked) and
         (FindCheckedSibling(I) = -1) then
        { Note: This uses coCheck instead of coCheckWithChildren (or the value
          of AOperation) in order to keep side effects to a minimum. Seems
          like the most logical behavior. For example, in this case:
          [ ] A
              ( ) B
                  [ ] C
              [ ] D
          clicking D will cause the radio button B to be checked (out of
          necessity), but won't automatically check its child check box, C.
          (If C were instead a radio button, it *would* be checked.) }
        RecursiveCheck(I, coCheck);
    end;
  end;

  procedure UpdateParentStates(const AIndex: Integer);
  var
    I: Integer;
    ChildChecked: Boolean;
    NewState: TCheckBoxState;
  begin
    I := AIndex;
    while True do begin
      ChildChecked := (GetState(I) <> cbUnchecked);

      I := GetParentOf(I);
      if I = -1 then
        Break;

      { When a child item is checked, must ensure that all sibling radio button
        groups have selections }
      if ChildChecked then
        EnsureChildRadioItemsHaveSelection(I);

      NewState := CalcState(I, GetState(I) <> cbUnchecked);

      { If a parent radio button is becoming checked, uncheck any previously
        selected sibling of that radio button }
      if (NewState <> cbUnchecked) and (ItemStates[I].ItemType = itRadio) then
        UncheckSiblings(I);

      SetItemState(I, NewState);
    end;
  end;

begin
  if ItemStates[Index].ItemType = itRadio then begin
    { Setting Checked to False on a radio button is a no-op. (A radio button
      may only be unchecked by checking another radio button in the group, or
      by unchecking a parent check box.) }
    if AOperation = coUncheck then begin
      Result := False;
      Exit;
    end;
    { Before checking a new item in a radio group, uncheck any siblings and
      their children }
    UncheckSiblings(Index);
  end;

  { Check or uncheck this item and all its children }
  Result := RecursiveCheck(Index, AOperation);

  { Update state of parents. For example, if a child check box is being
    checked, its parent must also become checked if it isn't already. }
  UpdateParentStates(Index);
end;

procedure TNewCheckListBox.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TNewCheckListBox.SetItemEnabled(Index: Integer; const AEnabled: Boolean);
begin
  if ItemStates[Index].Enabled <> AEnabled then
  begin
    ItemStates[Index].Enabled := AEnabled;
    const R = ItemRect(Index);
    InvalidateRect(Handle, @R, True);
  end;
end;

procedure TNewCheckListBox.RemeasureItemAndUpdate(Index: Integer);
var
  OldHeight, NewHeight: Integer;
  R, R2: TRect;
begin
  OldHeight := Integer(SendMessage(Handle, LB_GETITEMHEIGHT, Index, 0));
  NewHeight := RemeasureItem(Index);
  R := ItemRect(Index);
  { Scroll subsequent items down or up, if necessary }
  if NewHeight <> OldHeight then begin
    if Index >= TopIndex then begin
      R2 := ClientRect;
      R2.Top := R.Top + OldHeight;
      if not IsRectEmpty(R2) then
        ScrollWindowEx(Handle, 0, NewHeight - OldHeight, @R2, nil, 0, nil,
          SW_INVALIDATE or SW_ERASE);
    end;
    UpdateScrollRange;
  end;
  InvalidateRect(Handle, @R, True);
end;

procedure TNewCheckListBox.SetItemFontStyle(Index: Integer; const AItemFontStyle: TFontStyles);
begin
  if ItemStates[Index].ItemFontStyle <> AItemFontStyle then begin
    ItemStates[Index].ItemFontStyle := AItemFontStyle;
    RemeasureItemAndUpdate(Index);
  end;
end;

procedure TNewCheckListBox.SetItemIndex(const Value: Integer);
begin
  const Before = ItemIndex;
  inherited;
  const After = ItemIndex;
  if (Before <> After) then
    HandleScroll; { Might have scrolled a new item into view }
end;

procedure TNewCheckListBox.SetObject(Index: Integer; const AObject: TObject);
begin
  ItemStates[Index].Obj := AObject;
end;

procedure TNewCheckListBox.SetOffset(AnOffset: Integer);
begin
  if FOffset <> AnOffset then
  begin
    FOffset := AnOffset;
    for var I := Items.Count-1 downto 0 do
      RemeasureItem(I);
    UpdateScrollRange;
    Invalidate;
  end;
end;

procedure TNewCheckListBox.SetShowLines(Value: Boolean);
begin
  if FShowLines <> Value then
  begin
    FShowLines := Value;
    Invalidate;
  end;
end;

procedure TNewCheckListBox.SetSubItem(Index: Integer; const ASubItem: String);
begin
  if ItemStates[Index].SubItem <> ASubItem then begin
    ItemStates[Index].SubItem := ASubItem;
    RemeasureItemAndUpdate(Index);
  end;
end;

procedure TNewCheckListBox.SetSubItemFontStyle(Index: Integer; const ASubItemFontStyle: TFontStyles);
begin
  if ItemStates[Index].SubItemFontStyle <> ASubItemFontStyle then begin
    ItemStates[Index].SubItemFontStyle := ASubItemFontStyle;
    RemeasureItemAndUpdate(Index);
  end;
end;

procedure TNewCheckListBox.Toggle(Index: Integer);
begin
  case ItemStates[Index].ItemType of
    itCheck:
      case ItemStates[Index].State of
        cbUnchecked: CheckItem(Index, coCheckWithChildren);
        cbChecked: CheckItem(Index, coUncheck);
        cbGrayed:
          { First try checking, but if that doesn't work because of children
            that are disabled and unchecked, try unchecking }
          if not CheckItem(Index, coCheckWithChildren) then
            CheckItem(Index, coUncheck);
      end;
    itRadio: CheckItem(Index, coCheckWithChildren);
  end;
  if Assigned(FOnClickCheck) then
    FOnClickCheck(Self);
end;

procedure TNewCheckListBox.UpdateThreads;

  function LastImmediateChildOf(Item: Integer): Integer;
  var
    L: Integer;
  begin
    Result := -1;
    L := ItemLevel[Item] + 1;
    Inc(Item);
    while (Item < Items.Count) and (ItemLevel[Item] >= L) do
    begin
      if ItemLevel[Item] = L then
        Result := Item;
      Inc(Item);
    end;
    if Result >= 0 then
      ItemStates[Result].IsLastChild := True;
  end;
var
  I, J, LastChild, L: Integer;
begin
  for I := 0 to Items.Count - 1 do
  begin
    ItemStates[I].ThreadCache := [0];       //Doing ':= []' causes a "F2084 Internal Error: C21846" compiler error on Delphi 10.3 Rio }
    Exclude(ItemStates[I].ThreadCache, 0);  //
    ItemStates[I].IsLastChild := False;
  end;
  for I := 0 to Items.Count - 1 do
  begin
    LastChild := LastImmediateChildOf(I);
    L := ItemLevel[I];
    for J := I + 1 to LastChild do
      Include(ItemStates[J].ThreadCache, L);
  end;
end;

procedure TNewCheckListBox.LBDeleteString(var Message: TMessage);
var
  ItemState: TItemState;
begin
  inherited;
  if FDisableItemStateDeletion = 0 then begin
    const I = Integer(Message.WParam);
    if (I >= 0) and (I < FStateList.Count) then begin
      ItemState := FStateList[I];
      FStateList.Delete(I);
      ItemState.Free;
      FThreadsUpToDate := False;
    end;
  end;
end;

procedure TNewCheckListBox.LBResetContent(var Message: TMessage);
var
  ItemState: TItemState;
begin
  inherited;
  if (FDisableItemStateDeletion = 0) and (FStateList.Count > 0) then begin
    for var I := FStateList.Count-1 downto 0 do begin
      ItemState := FStateList[I];
      FStateList.Delete(I);
      ItemState.Free;
    end;
    FThreadsUpToDate := False;
  end;
end;

procedure TNewCheckListBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if FWantTabs then
    Message.Result := Message.Result and not DLGC_WANTCHARS;
end;

procedure TNewCheckListBox.WMKeyDown(var Message: TWMKeyDown);
var
  GoForward, Arrows: Boolean;
  I: Integer;
  Prnt, Ctrl: TWinControl;
begin
  { If space is pressed, avoid flickering -- exit now. }
  if not FWantTabs or (Message.CharCode = VK_SPACE) then
  begin
    inherited;
    Exit;
  end;
  Arrows := True;
  case Message.CharCode of
    VK_TAB:
      begin
        GoForward := GetKeyState(VK_SHIFT) >= 0;
        Arrows := False;
      end;
    VK_DOWN, VK_RIGHT: GoForward := True;
    VK_UP, VK_LEFT: GoForward := False
  else
    if FSpaceDown then EndCapture(True);
    inherited;
    Exit;
  end;
  EndCapture(not FSpaceDown);
  SendMessage(Handle, WM_CHANGEUISTATE, UIS_CLEAR or (UISF_HIDEFOCUS shl 16), 0);
  if Arrows or TabStop then
    I := FindNextItem(-2, GoForward, not Arrows)
  else
    I := -1;
  if I < 0 then
  begin
    Prnt := nil;
    if not Arrows then
      Prnt := GetParentForm(Self);
    if Prnt = nil then Prnt := Parent;
    if Prnt <> nil then
    begin
      Ctrl := TWinControlAccess(Prnt).FindNextControl(Self, GoForward, True, Arrows);
      if (Ctrl <> nil) and (Ctrl <> Self) then
      begin
        Ctrl.SetFocus;
        Exit;
      end;
    end;
    if GoForward then
      I := FindNextItem(-1, True, not Arrows)
    else
      I := FindNextItem(Items.Count, False, not Arrows);
  end;
  ItemIndex := I;
  if (I <> -1) and (ItemStates[I].ItemType = itRadio) and Arrows then
    Toggle(I);
end;

procedure TNewCheckListBox.WMMouseMove(var Message: TWMMouseMove);
var
  Pos: TPoint;
  Index, NewHotIndex: Integer;
  Rect: TRect;
  Indent: Integer;
begin
  Pos := SmallPointToPoint(Message.Pos);
  Index := ItemAtPos(Pos, True);

  if FCaptureIndex >= 0 then begin
    if not FSpaceDown and (Index <> FLastMouseMoveIndex) then begin
      if (FLastMouseMoveIndex = FCaptureIndex) or (Index = FCaptureIndex) then
        InvalidateCheck(FCaptureIndex);
      FLastMouseMoveIndex := Index;
    end
  end;

  NewHotIndex := -1;
  if (Index <> -1) and CanFocusItem(Index) then
  begin
    Rect := ItemRect(Index);
    Indent := (FOffset * 2 + FCheckWidth);
    if FWantTabs then
      NewHotIndex := Index
    else begin
      var CheckRect := Rect;
      CheckRect.Left := Rect.Left + Indent * ItemLevel[Index];
      CheckRect.Right := CheckRect.Left + Indent;
      FlipRect(CheckRect, ClientRect, IsRightToLeft);
      if (Pos.X >= CheckRect.Left) and (Pos.X < CheckRect.Right) then
        NewHotIndex := Index;
    end;
  end;
  UpdateHotIndex(NewHotIndex);
  inherited;
end;

procedure TNewCheckListBox.WMMouseWheel(var Message: TWMMouseWheel);
begin
  { See TCustomListView.WMVScroll for same code and also see WMVScroll below }
  const Before = GetScrollPos(Handle, SB_VERT);
  inherited;
  const After = GetScrollPos(Handle, SB_VERT);
  if (Before <> After) then
    HandleScroll;
end;

procedure TNewCheckListBox.WMVScroll(var Message: TWMVScroll);
begin
  { Also see WMMouseWheel above }
  const Before = GetScrollPos(Handle, SB_VERT);
  inherited;
  if Message.ScrollCode <> SB_THUMBTRACK then begin
    const After = GetScrollPos(Handle, SB_VERT);
    if (Before <> After) then
      HandleScroll;
  end else
    HandleScroll;
end;

procedure TNewCheckListBox.WMNCHitTest(var Message: TWMNCHitTest);
var
  I: Integer;
begin
  inherited;
  if FWantTabs and not (csDesigning in ComponentState) then
  begin
    if Message.Result = HTCLIENT then
    begin
      I := ItemAtPos(ScreenToClient(SmallPointToPoint(Message.Pos)), True);
      if (I < 0) or not CanFocusItem(I) then
      begin
        UpdateHotIndex(-1);
        Message.Result := 12345;
        Exit;
      end;
    end;
  end;
end;

procedure TNewCheckListBox.WMSize(var Message: TWMSize);
var
  I: Integer;
begin
  inherited;
  { When the scroll bar appears/disappears, the client width changes and we
    must recalculate the height of the items }
  for I := Items.Count-1 downto 0 do
    RemeasureItem(I);
  UpdateScrollRange;
end;

procedure TNewCheckListBox.WMThemeChanged(var Message: TMessage);
begin
  { Do not use Run to Cursor inside this function, it will interrupt the theme change }
  UpdateThemeData(True, True);
  inherited;
end;

procedure TNewCheckListBox.WMGetObject(var Message: TMessage);
begin
  if not HandleMSAAGetObject(Message) then
    inherited;
end;

{$IFDEF VCLSTYLES}

{ TNewCheckListBoxStyleHook - same as Vcl.StdCtrls' TListBoxStyleHook except that it picks the
  correct colors when WantTabs is True }

constructor TNewCheckListBoxStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverrideEraseBkgnd := True;
  UpdateColors;
end;

procedure TNewCheckListBoxStyleHook.WMSetFocus(var Message: TMessage);
begin
  inherited;
  CallDefaultProc(Message);
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
  Handled := True;
end;

procedure TNewCheckListBoxStyleHook.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_ERASEBKGND) and (Control.StyleName <> '') then begin
    const WantTabs = (Control is TNewCheckListBox) and TNewCheckListBox(Control).WantTabs;
    if not FStyleColorsChecked or (FStyleColorsCheckedWantTabs <> WantTabs) then begin
      FStyleColorsChecked := True;
      FStyleColorsCheckedWantTabs := WantTabs;
      UpdateColors;
    end;
  end;

  case Message.Msg of
    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      begin
        UpdateColors;
        SetTextColor(Message.WParam, UColorToRGB(FontColor));
        const Transparent = (Control is TNewCheckListBox) and TNewCheckListBox(Control).TransparentIfStyled;
        if Transparent then begin
          SetBkMode(Message.WParam, Windows.TRANSPARENT);
          Message.Result := LRESULT(GetStockObject(NULL_BRUSH));
        end else begin
          SetBkColor(Message.WParam, UColorToRGB(Brush.Color));
          Message.Result := LRESULT(Brush.Handle);
        end;
        Handled := True;
      end;
    CM_ENABLEDCHANGED:
      begin
        UpdateColors;
        Handled := False; // Allow control to handle message
      end
  else
    inherited WndProc(Message);
  end;
end;

procedure TNewCheckListBoxStyleHook.PaintBackground(Canvas: TCanvas);
begin
  const Transparent = (Control is TNewCheckListBox) and TNewCheckListBox(Control).TransparentIfStyled;
  if Transparent then
    StyleServices.DrawParentBackground(Handle, Canvas.Handle, nil, False)
  else
    inherited;
end;

procedure TNewCheckListBoxStyleHook.UpdateColors;
begin
  const WantTabs = (Control is TNewCheckListBox) and TNewCheckListBox(Control).WantTabs;
  const LStyle = StyleServices;

  { Also see color initialization in TNewCheckListBox.DrawItem }
  if WantTabs then
    Brush.Color := LStyle.GetStyleColor(scWindow)
  else
    Brush.Color := LStyle.GetStyleColor(ColorStates[Control.Enabled]);
  if seFont in Control.StyleElements then begin
    if WantTabs then
      FontColor := LStyle.GetStyleFontColor(TextLabelFontColorStates[Control.Enabled])
    else
      FontColor := LStyle.GetStyleFontColor(ListItemFontColorStates[Control.Enabled])
  end else
    FontColor := TWinControlAccess(Control).Font.Color;
end;

procedure TNewCheckListBoxStyleHook.WMKillFocus(var Message: TMessage);
begin
  inherited;
  CallDefaultProc(Message);
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
  Handled := True;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('JR', [TNewCheckListBox]);
end;

initialization
  InitThemeLibrary;
end.
