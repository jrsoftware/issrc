unit JvInspector.MSAA;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  MSAA support for JvInspector
}

interface

uses
  Windows, Messages,
  JvInspector;

type
  TJvInspectorMSAAHelper = class helper for TJvInspector
  private
    function EditorActive: Boolean;
    function WindowOwnsFocus: Boolean;
  public
    procedure AnnounceFocusToMSAA;
    procedure AnnounceReorderToMSAA;
    procedure AnnounceSelectionToMSAA;
    procedure AnnounceStateChangeToMSAA(const AIndex: Integer);
    procedure DisconnectMSAAObject;
    function HandleMSAAGetObject(var Message: TMessage): Boolean;
  end;

procedure SetOrClearNameForMSAA(const AWindow: HWND; const AName: string);

implementation

uses
  ActiveX, oleacc, SysUtils, Classes, Types, OleAccFunc;

type
  TJvInspectorAccess = class(TJvInspector);
  TJvCustomInspectorItemAccess = class(TJvCustomInspectorItem);

  TAccObject = class(TInterfacedObject, IDispatch, IAccessible)
  private
    FControl: TJvInspectorAccess;
    FStdAcc: IAccessible;
    function CheckChild(const varChild: OleVariant;
      out AIndex: Integer): HRESULT;
    { IDispatch }
    function GetTypeInfoCount(out ctinfo: Integer): HRESULT; stdcall;
    function GetTypeInfo(itinfo: Integer; lcid: Integer; out tinfo): HRESULT; stdcall;
    function GetIDsOfNames(const iid: TIID; rgszNames: Pointer;
      cNames: Integer; lcid: Integer; rgdispid: Pointer): HRESULT; stdcall;
    function Invoke(dispIDMember: TDispID; const iid: TIID; lcid: Integer;
      flags: Word; var dispParams; varResult: Pointer;
      excepInfo: Pointer; argErr: Pointer): HRESULT; stdcall;
    { IAccessible }
    function get_accParent(out ppdispParent: IDispatch): HRESULT; stdcall;
    function get_accChildCount(out pcountChildren: Integer): HRESULT; stdcall;
    function get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HRESULT; stdcall;
    function get_accName(varChild: OleVariant; out pszName: WideString): HRESULT; stdcall;
    function get_accValue(varChild: OleVariant; out pszValue: WideString): HRESULT; stdcall;
    function get_accDescription(varChild: OleVariant; out pszDescription: WideString): HRESULT; stdcall;
    function get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HRESULT; stdcall;
    function get_accState(varChild: OleVariant; out pvarState: OleVariant): HRESULT; stdcall;
    function get_accHelp(varChild: OleVariant; out pszHelp: WideString): HRESULT; stdcall;
    function get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant; out pidTopic: Integer): HRESULT; stdcall;
    function get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HRESULT; stdcall;
    function get_accFocus(out pvarID: OleVariant): HRESULT; stdcall;
    function get_accSelection(out pvarChildren: OleVariant): HRESULT; stdcall;
    function get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HRESULT; stdcall;
    function accSelect(flagsSelect: Integer; varChild: OleVariant): HRESULT; stdcall;
    function accLocation(out pxLeft: Integer; out pyTop: Integer; out pcxWidth: Integer;
      out pcyHeight: Integer; varChild: OleVariant): HRESULT; stdcall;
    function accNavigate(navDir: Integer; varStart: OleVariant; out pvarEnd: OleVariant): HRESULT; stdcall;
    function accHitTest(xLeft: Integer; yTop: Integer; out pvarID: OleVariant): HRESULT; stdcall;
    function accDoDefaultAction(varChild: OleVariant): HRESULT; stdcall;
    function put_accName(varChild: OleVariant; const pszName: WideString): HRESULT; stdcall;
    function put_accValue(varChild: OleVariant; const pszValue: WideString): HRESULT; stdcall;
    function IAccessible.Set_accName = put_accName;
    function IAccessible.Set_accValue = put_accValue;
  public
    constructor Create(const AControl: TJvInspector);
    destructor Destroy; override;
    procedure ControlDestroying;
  end;

procedure SetOrClearNameForMSAA(const AWindow: HWND; const AName: string);
{ Annotates the name property of a control. Call this again with AName set
  to an empty string before destroying the contol. Also see
  https://learn.microsoft.com/en-us/windows/win32/winauto/ensure-that-ui-elements-are-named-correctly }
begin
  var Services: IAccPropServices;
  if (CoCreateInstance(CLSID_AccPropServices, nil, CLSCTX_INPROC_SERVER,
      IAccPropServices, Services) = S_OK) and (Services <> nil) then begin
    if AName <> '' then
      Services.SetHwndPropStr(wireHWND(AWindow)^, DWORD(OBJID_CLIENT),
        CHILDID_SELF, PROPID_ACC_NAME, PChar(AName))
    else begin
      var PropId := PROPID_ACC_NAME;
      Services.ClearHwndProps(wireHWND(AWindow)^, DWORD(OBJID_CLIENT),
        CHILDID_SELF, PropId, 1);
    end;
  end;
end;

{ TJvInspectorMSAAHelper }

function TJvInspectorMSAAHelper.EditorActive: Boolean;
begin
  { Editing cannot be used here: TJvInspectorBooleanItem.InitEdit sets
    Editing without ever creating an editor window }
  Result := (Selected <> nil) and
    (TJvCustomInspectorItemAccess(Selected).EditCtrl <> nil);
end;

function TJvInspectorMSAAHelper.WindowOwnsFocus: Boolean;
begin
  { Focused cannot be used here: it is overridden to also be True while the
    in-place editor child owns the focus }
  Result := HandleAllocated and (GetFocus = Handle);
end;

procedure TJvInspectorMSAAHelper.AnnounceFocusToMSAA;
begin
  { MSAA expects a container which gains focus to also fire a focus event
    for the child that logically has it: without it a screen reader keeps
    treating the container as the focused element, and for example ignores a
    later state change of the row. Skip when the in-place editor took the
    real focus: it announces itself }
  if WindowOwnsFocus and not EditorActive and (SelectedIndex >= 0) and
     Assigned(NotifyWinEventFunc) then
    NotifyWinEventFunc(EVENT_OBJECT_FOCUS, Handle, OBJID_CLIENT,
      1 + SelectedIndex);
end;

procedure TJvInspectorMSAAHelper.AnnounceReorderToMSAA;
{ Call this whenever the visible list has been rebuilt: screen readers cache
  child counts and IDs, and this event makes them look again }
begin
  if HandleAllocated and Assigned(NotifyWinEventFunc) then
    NotifyWinEventFunc(EVENT_OBJECT_REORDER, Handle, OBJID_CLIENT, CHILDID_SELF);
end;

procedure TJvInspectorMSAAHelper.AnnounceSelectionToMSAA;
{ Notify MSAA of the selection change. The focus event is only fired when
  focused and when there's no in-place editor active. }
begin
  if HandleAllocated and Assigned(NotifyWinEventFunc) then begin
    if WindowOwnsFocus and not EditorActive then begin
      if SelectedIndex >= 0 then
        NotifyWinEventFunc(EVENT_OBJECT_FOCUS, Handle, OBJID_CLIENT,
          1 + SelectedIndex)
      else
        NotifyWinEventFunc(EVENT_OBJECT_FOCUS, Handle, OBJID_CLIENT, CHILDID_SELF);
    end;
    { Also fire the selection event which the system fires for standard list
      boxes, even when unfocused: the selection did change. Per docs a
      cleared selection is reported as CHILDID_SELF. }
    if SelectedIndex >= 0 then
      NotifyWinEventFunc(EVENT_OBJECT_SELECTION, Handle, OBJID_CLIENT,
        1 + SelectedIndex)
    else
      NotifyWinEventFunc(EVENT_OBJECT_SELECTION, Handle, OBJID_CLIENT,
        CHILDID_SELF);
  end;
end;

procedure TJvInspectorMSAAHelper.AnnounceStateChangeToMSAA(
  const AIndex: Integer);
{ Notify MSAA of the state change. Works for NVDA, but not for Narrator;
  it requires a focus event as well. }
begin
  if HandleAllocated and (AIndex >= 0) and Assigned(NotifyWinEventFunc) then
    NotifyWinEventFunc(EVENT_OBJECT_STATECHANGE, Handle, OBJID_CLIENT,
      1 + AIndex);
end;

procedure TJvInspectorMSAAHelper.DisconnectMSAAObject;
begin
  if FAccObject <> nil then begin
    { Detach from FAccObject if someone still has a reference to it }
    TAccObject(FAccObject as TObject).ControlDestroying;
    FAccObject := nil;
  end;
end;

function TJvInspectorMSAAHelper.HandleMSAAGetObject(var Message: TMessage): Boolean;
begin
  { Per docs, lParam must be casted to DWORD (32 bits) because it may be
    sign-extended in a 64-bit process. Bail when destroying: the window
    outlives BeforeDestruction, which already disconnected the accessible
    object from the freed model. }
  Result := (DWORD(Message.LParam) = OBJID_CLIENT) and
    not (csDestroying in ComponentState) and InitializeOleAcc;
  if Result then begin
    if FAccObject = nil then begin
      try
        FAccObject := TAccObject.Create(Self);
      except
        Exit(False);
      end;
    end;
    Message.Result := LresultFromObjectFunc(IAccessible, Message.WParam,
      FAccObject);
  end;
end;

{ TAccObject }

procedure VariantInitInteger(var V: OleVariant; const Value: Integer);
begin
  VariantInit(V);
  TVarData(V).VType := VT_I4;
  TVarData(V).VInteger := Value;
end;

constructor TAccObject.Create(const AControl: TJvInspector);
begin
  inherited Create;
  if CreateStdAccessibleObjectFunc(AControl.Handle, Integer(OBJID_CLIENT),
     IAccessible, Pointer(FStdAcc)) <> S_OK then begin
    { Note: The user will never actually see this message since the call to
      TAccObject.Create in HandleMSAAGetObject is protected by a
      try..except. }
    raise Exception.Create('CreateStdAccessibleObject failed');
  end;
  FControl := TJvInspectorAccess(AControl);
end;

destructor TAccObject.Destroy;
begin
  { NewCheckListBox's version clears the control's reference to us here. Not
    needed: FAccObject is a counted reference, so we cannot be destroyed
    while the control still holds it }
  inherited;
end;

procedure TAccObject.ControlDestroying;
begin
  { Set FControl to nil, since it's no longer valid }
  FControl := nil;
  { Take this opportunity to disconnect remote clients, i.e. don't allow them
    to call us anymore. This prevents invalid memory accesses if this unit's
    code is in a DLL, and the application subsequently unloads the DLL while
    remote clients still hold (and are using) references to this TAccObject. }
  CoDisconnectObject(Self, 0);
  { Unlike in NewCheckListBox it's still safe to access Self here: the
    control holds a reference to us until DisconnectMSAAObject clears it }
end;

function TAccObject.CheckChild(const varChild: OleVariant;
  out AIndex: Integer): HRESULT;
{ Shared validation: checks FControl and varChild, and converts varChild to
  an index, with -1 meaning the control itself. NewCheckListBox does these
  checks inline in each accessor instead }
begin
  AIndex := -1;
  if FControl = nil then
    Exit(E_FAIL);
  if TVarData(varChild).VType <> VT_I4 then
    Exit(E_INVALIDARG);
  const ChildID = TVarData(varChild).VInteger;
  if (ChildID < CHILDID_SELF) or (ChildID > FControl.GetVisibleCount) then
    Exit(E_INVALIDARG);
  AIndex := ChildID - 1;
  Result := S_OK;
end;

function TAccObject.GetTypeInfoCount(out ctinfo: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TAccObject.GetTypeInfo(itinfo: Integer; lcid: Integer; out tinfo): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TAccObject.GetIDsOfNames(const iid: TIID; rgszNames: Pointer;
  cNames: Integer; lcid: Integer; rgdispid: Pointer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TAccObject.Invoke(dispIDMember: TDispID; const iid: TIID; lcid: Integer;
  flags: Word; var dispParams; varResult: Pointer;
  excepInfo: Pointer; argErr: Pointer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TAccObject.accDoDefaultAction(varChild: OleVariant): HRESULT;
begin
  { The rows don't have a default action (the reader can't expand or collapse
    them), and DISP_E_MEMBERNOTFOUND is the documented way to report that }
  Result := DISP_E_MEMBERNOTFOUND;
end;

function TAccObject.accHitTest(xLeft, yTop: Integer;
  out pvarID: OleVariant): HRESULT;
begin
  VariantInit(pvarID);
  if FControl = nil then
    Exit(E_FAIL);
  if not FControl.HandleAllocated then
    Exit(S_FALSE);
  const P = FControl.ScreenToClient(Point(xLeft, yTop));
  if not PtInRect(FControl.ClientRect, P) then
    Exit(S_FALSE);
  { CalcItemIndex already applies the TopIndex scroll offset. Below the last
    row the point still is in the control, so report the control itself }
  const Index = FControl.CalcItemIndex(P.Y);
  if Index >= 0 then
    VariantInitInteger(pvarID, 1 + Index)
  else
    VariantInitInteger(pvarID, CHILDID_SELF);
  Result := S_OK;
end;

function TAccObject.accLocation(out pxLeft, pyTop, pcxWidth,
  pcyHeight: Integer; varChild: OleVariant): HRESULT;
begin
  pxLeft := 0;
  pyTop := 0;
  pcxWidth := 0;
  pcyHeight := 0;
  var Index: Integer;
  Result := CheckChild(varChild, Index);
  if Result <> S_OK then
    Exit;
  if Index < 0 then
    Exit(FStdAcc.accLocation(pxLeft, pyTop, pcxWidth, pcyHeight, varChild));
  if not FControl.HandleAllocated then
    Exit(S_FALSE);
  { IdxToY is measured from the virtual top of the item list, so subtract the
    TopIndex scroll offset to get the client position }
  const P = FControl.ClientToScreen(Point(0,
    FControl.IdxToY(Index) - FControl.IdxToY(FControl.TopIndex)));
  pxLeft := P.X;
  pyTop := P.Y;
  pcxWidth := FControl.ClientWidth;
  pcyHeight := FControl.GetItemHeight;
end;

function TAccObject.accNavigate(navDir: Integer; varStart: OleVariant;
  out pvarEnd: OleVariant): HRESULT;
begin
  VariantInit(pvarEnd);
  var Index: Integer;
  Result := CheckChild(varStart, Index);
  if Result <> S_OK then
    Exit;
  if (Index < 0) and not (navDir in [NAVDIR_FIRSTCHILD, NAVDIR_LASTCHILD]) then
    Exit(FStdAcc.accNavigate(navDir, varStart, pvarEnd));
  const Count = FControl.GetVisibleCount;
  var NewChildID := 0; { 0 = no destination }
  case navDir of
    NAVDIR_FIRSTCHILD:
      if (Index < 0) and (Count > 0) then
        NewChildID := 1;
    NAVDIR_LASTCHILD:
      if (Index < 0) and (Count > 0) then
        NewChildID := Count;
    NAVDIR_NEXT, NAVDIR_DOWN:
      if (Index >= 0) and (Index < Count - 1) then
        NewChildID := Index + 2;
    NAVDIR_PREVIOUS, NAVDIR_UP:
      if Index > 0 then
        NewChildID := Index; { = the previous row's index + 1 }
  end;
  if NewChildID > 0 then
    VariantInitInteger(pvarEnd, NewChildID)
  else
    Result := S_FALSE;
end;

function TAccObject.accSelect(flagsSelect: Integer;
  varChild: OleVariant): HRESULT;
begin
  var Index: Integer;
  Result := CheckChild(varChild, Index);
  if Result <> S_OK then
    Exit;
  if Index < 0 then
    Exit(FStdAcc.accSelect(flagsSelect, varChild)); { Focus the control itself }
  if flagsSelect and not (SELFLAG_TAKEFOCUS or SELFLAG_TAKESELECTION) <> 0 then
    Exit(E_INVALIDARG);
  try
    { Select before focusing: WM_SETFOCUS announces the selected row, which
      must already be the new one. Also per docs TAKEFOCUS alone must not
      alter the selection; the focus then lands on the already selected row. }
    if flagsSelect and SELFLAG_TAKESELECTION <> 0 then
      FControl.SelectedIndex := Index;
    if flagsSelect and SELFLAG_TAKEFOCUS <> 0 then
      FControl.SetFocus;
  except
    Result := E_FAIL;
  end;
end;

function TAccObject.get_accChild(varChild: OleVariant;
  out ppdispChild: IDispatch): HRESULT;
begin
  { The rows are simple elements without an IDispatch of their own: per docs
    that means returning S_FALSE with ppdispChild set to nil }
  ppdispChild := nil;
  Result := S_FALSE;
end;

function TAccObject.get_accChildCount(out pcountChildren: Integer): HRESULT;
begin
  pcountChildren := 0;
  if FControl = nil then
    Exit(E_FAIL);
  pcountChildren := FControl.GetVisibleCount;
  Result := S_OK;
end;

function TAccObject.get_accDefaultAction(varChild: OleVariant;
  out pszDefaultAction: WideString): HRESULT;
begin
  { See accDoDefaultAction }
  pszDefaultAction := '';
  Result := S_FALSE;
end;

function TAccObject.get_accDescription(varChild: OleVariant;
  out pszDescription: WideString): HRESULT;
begin
  { Neither the control nor the rows have a description }
  pszDescription := '';
  Result := S_FALSE;
end;

function TAccObject.get_accFocus(out pvarID: OleVariant): HRESULT;
begin
  VariantInit(pvarID);
  if FControl = nil then
    Exit(E_FAIL);
  if FControl.WindowOwnsFocus then begin
    { The selected row, or the container itself when nothing is selected;
      matches the focus event SetSelectedIndex fires }
    if FControl.SelectedIndex >= 0 then
      VariantInitInteger(pvarID, 1 + FControl.SelectedIndex)
    else
      VariantInitInteger(pvarID, CHILDID_SELF);
    Result := S_OK;
  end else if FControl.EditorActive and
    TJvCustomInspectorItemAccess(FControl.Selected).EditCtrl.Focused then begin
    { The standard object resolves the focused child window to the in-place
      editor's accessible }
    Result := FStdAcc.get_accFocus(pvarID);
  end else
    Result := S_FALSE;
end;

function TAccObject.get_accHelp(varChild: OleVariant;
  out pszHelp: WideString): HRESULT;
begin
  { Neither the control nor the rows have help }
  pszHelp := '';
  Result := S_FALSE;
end;

function TAccObject.get_accHelpTopic(out pszHelpFile: WideString;
  varChild: OleVariant; out pidTopic: Integer): HRESULT;
begin
  { No help, see get_accHelp }
  pszHelpFile := '';
  pidTopic := 0;
  Result := S_FALSE;
end;

function TAccObject.get_accKeyboardShortcut(varChild: OleVariant;
  out pszKeyboardShortcut: WideString): HRESULT;
begin
  { Neither the control nor the rows have a keyboard shortcut }
  pszKeyboardShortcut := '';
  Result := S_FALSE;
end;

function TAccObject.get_accName(varChild: OleVariant;
  out pszName: WideString): HRESULT;
begin
  pszName := '';
  var Index: Integer;
  Result := CheckChild(varChild, Index);
  if Result <> S_OK then
    Exit;
  if Index < 0 then begin
    if FControl.AccessibleName <> '' then
      pszName := FControl.AccessibleName
    else
      Result := S_FALSE;
  end else
    pszName := FControl.GetVisibleItems(Index).DisplayName;
end;

function TAccObject.get_accParent(out ppdispParent: IDispatch): HRESULT;
begin
  Result := FStdAcc.get_accParent(ppdispParent);
end;

function TAccObject.get_accRole(varChild: OleVariant;
  out pvarRole: OleVariant): HRESULT;
begin
  VariantInit(pvarRole);
  var Index: Integer;
  Result := CheckChild(varChild, Index);
  if Result <> S_OK then
    Exit;
  { Leaf rows report a list item, not an outline item: an outline item's
    value is by convention its level, so readers suppress a numeric value,
    and a leaf row's value can be numeric }
  if Index < 0 then
    VariantInitInteger(pvarRole, ROLE_SYSTEM_OUTLINE)
  else begin
    const Item = FControl.GetVisibleItems(Index);
    if Item is TJvInspectorBooleanItem then
      VariantInitInteger(pvarRole, ROLE_SYSTEM_CHECKBUTTON)
    else if Item.Count > 0 then
      VariantInitInteger(pvarRole, ROLE_SYSTEM_OUTLINEITEM)
    else
      VariantInitInteger(pvarRole, ROLE_SYSTEM_LISTITEM);
  end;
end;

function TAccObject.get_accSelection(out pvarChildren: OleVariant): HRESULT;
begin
  VariantInit(pvarChildren);
  if FControl = nil then
    Exit(E_FAIL);
  if FControl.SelectedIndex >= 0 then
    VariantInitInteger(pvarChildren, 1 + FControl.SelectedIndex);
  Result := S_OK;
end;

function TAccObject.get_accState(varChild: OleVariant;
  out pvarState: OleVariant): HRESULT;
begin
  VariantInit(pvarState);
  var Index: Integer;
  Result := CheckChild(varChild, Index);
  if Result <> S_OK then
    Exit;
  if Index < 0 then
    Exit(FStdAcc.get_accState(varChild, pvarState));
  var State := STATE_SYSTEM_FOCUSABLE or STATE_SYSTEM_SELECTABLE;
  if Index = FControl.SelectedIndex then begin
    State := State or STATE_SYSTEM_SELECTED;
    { Only claim focus when the window really owns it, so a tool querying an
      unfocused inspector does not see a row claiming focus }
    if FControl.WindowOwnsFocus then
      State := State or STATE_SYSTEM_FOCUSED;
  end;
  { Derive on/off screen from the same scroll-relative position accLocation
    reports }
  const Y = FControl.IdxToY(Index) - FControl.IdxToY(FControl.TopIndex);
  if (Y + FControl.GetItemHeight <= 0) or (Y >= FControl.ClientHeight) then
    State := State or STATE_SYSTEM_OFFSCREEN;
  { Any row with children can expand, not just categories }
  const Item = FControl.GetVisibleItems(Index);
  if Item.Count > 0 then begin
    if Item.Expanded then
      State := State or STATE_SYSTEM_EXPANDED
    else
      State := State or STATE_SYSTEM_COLLAPSED;
  end;
  if Item is TJvInspectorBooleanItem then begin
    try
      if TJvCustomInspectorItemAccess(Item).AsOrdinal <> Ord(False) then
        State := State or STATE_SYSTEM_CHECKED;
    except
      Exit(E_INVALIDARG);
    end;
  end;
  VariantInitInteger(pvarState, State);
end;

function TAccObject.get_accValue(varChild: OleVariant;
  out pszValue: WideString): HRESULT;
begin
  pszValue := '';
  var Index: Integer;
  Result := CheckChild(varChild, Index);
  if Result <> S_OK then
    Exit;
  if Index < 0 then
    Exit(S_FALSE); { The control itself has no value }
  try
    const Item = FControl.GetVisibleItems(Index);
    if Item is TJvInspectorBooleanItem then begin
      { Boolean rows have an empty DisplayValue }
      if TJvCustomInspectorItemAccess(Item).AsOrdinal <> Ord(False) then
        pszValue := 'yes'
      else
        pszValue := 'no';
    end else if Item.Count > 0 then begin
      { Return the level as the value, like standard tree view controls do.
        Not sure if any screen readers will actually use this, seeing as we
        aren't a real tree view control. }
      pszValue := IntToStr(Item.Level);
    end else
      pszValue := Item.DisplayValue;
  except
    Result := E_INVALIDARG;
  end;
end;

function TAccObject.put_accName(varChild: OleVariant;
  const pszName: WideString): HRESULT;
begin
  Result := S_FALSE;
end;

function TAccObject.put_accValue(varChild: OleVariant;
  const pszValue: WideString): HRESULT;
begin
  Result := S_FALSE;
end;

end.
