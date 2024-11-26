unit FolderTreeView;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TFolderTreeView component
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, CommCtrl;

type
  TCustomFolderTreeView = class;

  TFolderRenameEvent = procedure(Sender: TCustomFolderTreeView;
    var NewName: String; var Accept: Boolean) of object;

  TCustomFolderTreeView = class(TWinControl)
  private
    FDestroyingHandle: Boolean;
    FDirectory: String;
    FItemExpanding: Boolean;
    FOnChange: TNotifyEvent;
    FOnRename: TFolderRenameEvent;
    procedure Change;
    procedure DeleteObsoleteNewItems(const ParentItem, ItemToKeep: HTREEITEM);
    function FindItem(const ParentItem: HTREEITEM; const AName: String): HTREEITEM;
    function FindOrCreateItem(const ParentItem: HTREEITEM; const AName: String): HTREEITEM;
    function GetItemFullPath(Item: HTREEITEM): String; virtual;
    function InsertItem(const ParentItem: HTREEITEM; const AName, ACustomDisplayName: String;
      const ANewItem: Boolean): HTREEITEM;
    procedure SelectItem(const Item: HTREEITEM);
    procedure SetItemHasChildren(const Item: HTREEITEM; const AHasChildren: Boolean);
    procedure SetDirectory(const Value: String);
    function TryExpandItem(const Item: HTREEITEM): Boolean;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMCtlColorEdit(var Message: TMessage); message WM_CTLCOLOREDIT;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    function ItemChildrenNeeded(const Item: HTREEITEM): Boolean; virtual; abstract;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GetItemImageIndex(const Item: HTREEITEM;
      const NewItem, SelectedImage: Boolean): Integer; virtual; abstract;
    function GetRootItem: HTREEITEM; virtual;
    function ItemHasChildren(const Item: HTREEITEM): Boolean; virtual; abstract;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnRename: TFolderRenameEvent read FOnRename write FOnRename;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ChangeDirectory(const Value: String; const CreateNewItems: Boolean);
    procedure CreateNewDirectory(const ADefaultName: String);
    property Directory: String read FDirectory write SetDirectory;
  end;

  TFolderTreeView = class(TCustomFolderTreeView)
  private
    procedure RefreshDriveItem(const Item: HTREEITEM; const ANewDisplayName: String);
  protected
    function ItemChildrenNeeded(const Item: HTREEITEM): Boolean; override;
    function ItemHasChildren(const Item: HTREEITEM): Boolean; override;
    function GetItemFullPath(Item: HTREEITEM): String; override;
    function GetItemImageIndex(const Item: HTREEITEM;
      const NewItem, SelectedImage: Boolean): Integer; override;
  published
    property Anchors;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange;
    property OnRename;
  end;

  TStartMenuFolderTreeView = class(TCustomFolderTreeView)
  private
    FUserPrograms, FCommonPrograms: String;
    FUserStartup, FCommonStartup: String;
    FImageIndexes: array[Boolean] of Integer;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function GetRootItem: HTREEITEM; override;
    function ItemChildrenNeeded(const Item: HTREEITEM): Boolean; override;
    function ItemHasChildren(const Item: HTREEITEM): Boolean; override;
    function GetItemImageIndex(const Item: HTREEITEM;
      const NewItem, SelectedImage: Boolean): Integer; override;
  public
    procedure SetPaths(const AUserPrograms, ACommonPrograms,
      AUserStartup, ACommonStartup: String);
  published
    property Anchors;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange;
    property OnRename;
  end;

procedure Register;

implementation

{
  Notes:
  1. Don't call TreeView_SelectItem without calling TreeView_Expand on the
     item's parents first. Otherwise infinite recursion can occur:
     a. TreeView_SelectItem will first set the selected item. It will then try
        to expand the parent node, causing a TVN_ITEMEXPANDING message to be
        sent.
     b. If the TVN_ITEMEXPANDING handler calls TreeView_SortChildren, TV_SortCB
        will call TV_EnsureVisible if the selected item was one of the items
        affected by the sorting (which will always be the case).
     c. TV_EnsureVisible will expand parent nodes if necessary. However, since
        we haven't yet returned from the original TVN_ITEMEXPANDING message
        handler, the parent node doesn't yet have the TVIS_EXPANDED state,
        thus it thinks the node still needs expanding.
     d. Another, nested TVN_ITEMEXPANDING message is sent, bringing us back to
        step b.
     (Reproducible on Windows 95 and 2000.)
     The recursion can be seen if you comment out the ExpandParents call in
     the SelectItem method, then click "New Folder" on a folder with no
     children.
     (Note, however, that because of the ChildrenAdded check in our
     TVN_ITEMEXPANDING handler, it can only recurse once. That won't cause a
     fatal stack overflow (like it did before the ChildrenAdded check was
     added), but it's still wrong to allow that to happen.)
}

uses
  PathFunc, ShellApi, NewUxTheme, Types;

const
  SHPPFW_NONE = $00000000;
var
  SHPathPrepareForWriteFunc: function(hwnd: HWND; punkEnableModless: Pointer;
    pszPath: PChar; dwFlags: DWORD): HRESULT; stdcall;

const
  TVM_SETEXTENDEDSTYLE = TV_FIRST + 44;
  TVS_EX_DOUBLEBUFFER = $0004;

procedure Register;
begin
  RegisterComponents('JR', [TFolderTreeView, TStartMenuFolderTreeView]);
end;

function IsListableDirectory(const FindData: TWin32FindData): Boolean;
begin
  Result := (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and
    (FindData.dwFileAttributes and (FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM) <>
     (FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM)) and
    (StrComp(FindData.cFileName, '.') <> 0) and
    (StrComp(FindData.cFileName, '..') <> 0);
end;

function HasSubfolders(const Path: String): Boolean;
var
  H: THandle;
  FindData: TWin32FindData;
begin
  Result := False;
  H := FindFirstFile(PChar(AddBackslash(Path) + '*'), FindData);
  if H <> INVALID_HANDLE_VALUE then begin
    try
      repeat
        if IsListableDirectory(FindData) then begin
          Result := True;
          Break;
        end;
      until not FindNextFile(H, FindData);
    finally
      Windows.FindClose(H);
    end;
  end;
end;

function GetFileDisplayName(const Filename: String): String;
var
  FileInfo: TSHFileInfo;
begin
  if SHGetFileInfo(PChar(Filename), 0, FileInfo, SizeOf(FileInfo),
     SHGFI_DISPLAYNAME) <> 0 then
    Result := FileInfo.szDisplayName
  else
    Result := '';
end;

function GetFileImageIndex(const Filename: String; const OpenIcon: Boolean): Integer;
const
  OpenFlags: array[Boolean] of UINT = (0, SHGFI_OPENICON);
var
  FileInfo: TSHFileInfo;
begin
  if SHGetFileInfo(PChar(Filename), 0, FileInfo, SizeOf(FileInfo),
     SHGFI_SYSICONINDEX or SHGFI_SMALLICON or OpenFlags[OpenIcon]) <> 0 then
    Result := FileInfo.iIcon
  else
    Result := 0;
end;

function GetDefFolderImageIndex(const OpenIcon: Boolean): Integer;
const
  OpenFlags: array[Boolean] of UINT = (0, SHGFI_OPENICON);
var
  FileInfo: TSHFileInfo;
begin
  if SHGetFileInfo('c:\directory', FILE_ATTRIBUTE_DIRECTORY, FileInfo, SizeOf(FileInfo),
     SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX or SHGFI_SMALLICON or OpenFlags[OpenIcon]) <> 0 then
    Result := FileInfo.iIcon
  else
    Result := 0;
end;

function IsNetworkDrive(const Drive: Char): Boolean;
{ Returns True if Drive is a network drive. Unlike GetLogicalDrives and
  GetDriveType, this will find the drive even if it's currently in an
  unavailable/disconnected state (i.e. showing a red "X" on the drive icon
  in Windows Explorer). }
var
  LocalName: String;
  RemoteName: array[0..MAX_PATH-1] of Char;
  RemoteNameLen, ErrorCode: DWORD;
begin
  LocalName := Drive + ':';
  RemoteNameLen := SizeOf(RemoteName) div SizeOf(RemoteName[0]);
  ErrorCode := WNetGetConnection(PChar(LocalName), RemoteName, RemoteNameLen);
  Result := (ErrorCode = NO_ERROR) or (ErrorCode = ERROR_CONNECTION_UNAVAIL);
end;

function MoveAppWindowToActiveWindowMonitor(var OldRect: TRect): Boolean;
{ This moves the application window (Application.Handle) to the same monitor
  as the active window, so that a subsequent Windows dialog will display on
  the same monitor. Based on code from D4+'s TApplication.MessageBox.
  NOTE: This function was copied from CmnFunc.pas. }
type
  HMONITOR = type THandle;
  TMonitorInfo = record
    cbSize: DWORD;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFlags: DWORD;
  end;
const
  MONITOR_DEFAULTTONEAREST = $00000002;
var
  ActiveWindow: HWND;
  Module: HMODULE;
  MonitorFromWindow: function(hwnd: HWND; dwFlags: DWORD): HMONITOR; stdcall;
  GetMonitorInfo: function(hMonitor: HMONITOR; var lpmi: TMonitorInfo): BOOL; stdcall;
  MBMonitor, AppMonitor: HMONITOR;
  Info: TMonitorInfo;
begin
  Result := False;
  ActiveWindow := GetActiveWindow;
  if ActiveWindow = 0 then Exit;
  Module := GetModuleHandle(user32);
  MonitorFromWindow := GetProcAddress(Module, 'MonitorFromWindow');
  GetMonitorInfo := GetProcAddress(Module, 'GetMonitorInfoA');
  if Assigned(MonitorFromWindow) and Assigned(GetMonitorInfo) then begin
    MBMonitor := MonitorFromWindow(ActiveWindow, MONITOR_DEFAULTTONEAREST);
    AppMonitor := MonitorFromWindow(Application.Handle, MONITOR_DEFAULTTONEAREST);
    if MBMonitor <> AppMonitor then begin
      Info.cbSize := SizeOf(Info);
      if GetMonitorInfo(MBMonitor, Info) then begin
        GetWindowRect(Application.Handle, OldRect);
        SetWindowPos(Application.Handle, 0,
          Info.rcMonitor.Left + ((Info.rcMonitor.Right - Info.rcMonitor.Left) div 2),
          Info.rcMonitor.Top + ((Info.rcMonitor.Bottom - Info.rcMonitor.Top) div 2),
          0, 0, SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOSIZE or SWP_NOZORDER);
        Result := True;
      end;
    end;
  end;
end;

procedure MoveAppWindowBack(const OldRect: TRect);
{ Moves the application window back to its previous position after a
  successful call to MoveAppWindowToActiveWindowMonitor }
begin
  SetWindowPos(Application.Handle, 0,
    OldRect.Left + ((OldRect.Right - OldRect.Left) div 2),
    OldRect.Top + ((OldRect.Bottom - OldRect.Top) div 2),
    0, 0, SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOSIZE or SWP_NOZORDER);
end;

function EnsurePathIsAccessible(const Path: String): Boolean;
{ Calls SHPathPrepareForWrite which ensures the specified path is accessible by
  reconnecting network drives (important) and prompting for media on removable
  drives (not so important for our purposes). (Note that despite its name,
  the function does not test for write access.) }
var
  ActiveWindow: HWND;
  DidMove: Boolean;
  OldRect: TRect;
  WindowList: Pointer;
begin
  { SHPathPrepareForWrite only exists on Windows 2000, Me, and later.
    Do nothing on older versions of Windows. }
  if @SHPathPrepareForWriteFunc = nil then begin
    Result := True;
    Exit;
  end;

  { Note: The SHPathPrepareForWrite documentation claims that "user interface
    windows will not be created" when hwnd is NULL, however I found that on
    Windows 2000, it would still display message boxes for network errors.
    (To reproduce: Disable your Local Area Connection and try expanding a
    network drive.) So to avoid bugs from having unowned message boxes floating
    around, go ahead and pass a proper owner window. }
  ActiveWindow := GetActiveWindow;
  DidMove := MoveAppWindowToActiveWindowMonitor(OldRect);
  WindowList := DisableTaskWindows(0);
  try
    Result := SUCCEEDED(SHPathPrepareForWriteFunc(Application.Handle, nil,
      PChar(Path), SHPPFW_NONE));
  finally
    if DidMove then
      MoveAppWindowBack(OldRect);
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
  end;
end;

{ TCustomFolderTreeView }

type
  PItemData = ^TItemData;
  TItemData = record
    Name: String;
    NewItem: Boolean;
    ChildrenAdded: Boolean;
  end;

constructor TCustomFolderTreeView.Create(AOwner: TComponent);
var
  LogFont: TLogFont;
begin
  inherited;
  ControlStyle := ControlStyle - [csCaptureMouse];
  Width := 121;
  Height := 97;
  ParentColor := False;
  TabStop := True;
  if SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(LogFont), @LogFont, 0) then
    Font.Handle := CreateFontIndirect(LogFont);
end;

procedure TCustomFolderTreeView.CreateParams(var Params: TCreateParams);
const
  TVS_TRACKSELECT = $0200;
  TVS_SINGLEEXPAND = $0400;
begin
  InitCommonControls;
  inherited;
  CreateSubClass(Params, WC_TREEVIEW);
  with Params do begin
    Style := Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or TVS_LINESATROOT or
      TVS_HASBUTTONS or TVS_SHOWSELALWAYS or TVS_EDITLABELS;
    Style := Style or TVS_TRACKSELECT;
    ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TCustomFolderTreeView.CreateWnd;
var
  ImageList: HIMAGELIST;
  FileInfo: TSHFileInfo;
  SaveCursor: HCURSOR;
begin
  FDestroyingHandle := False;
  inherited;
  FDirectory := '';
  if csDesigning in ComponentState then
    Exit;

  { Enable the new Explorer-style look }
  if Assigned(SetWindowTheme) then begin
    SetWindowTheme(Handle, 'Explorer', nil);
    { Like Explorer, enable double buffering to avoid flicker when the mouse
      is moved across the items }
    SendMessage(Handle, TVM_SETEXTENDEDSTYLE, TVS_EX_DOUBLEBUFFER,
      TVS_EX_DOUBLEBUFFER);
  end;

  { Initialize the image list }
  ImageList := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo),
    SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  TreeView_SetImageList(Handle, ImageList, TVSIL_NORMAL);

  { Add the root items }
  SaveCursor := SetCursor(LoadCursor(0, IDC_WAIT));
  try
    ItemChildrenNeeded(nil);
  finally
    SetCursor(SaveCursor);
  end;
end;

procedure TCustomFolderTreeView.WMDestroy(var Message: TWMDestroy);
begin
  { Work around bug in pre-v6 COMCTL32: If we have the TVS_SINGLEEXPAND style
    and there is a selected item when the window is destroyed, we end up
    getting a bunch of TVN_SINGLEEXPAND messages because it keeps moving the
    selection as it's destroying items, resulting in a stream of "Please
    insert a disk in drive X:" message boxes as the selection moves across
    removable drives.
    Currently, however, this problem isn't seen in practice because we don't
    use TVS_SINGLEEXPAND on pre-XP Windows. }
  FDestroyingHandle := True;  { disables our TVN_SELCHANGED handling }
  SelectItem(nil);
  inherited;
end;

procedure TCustomFolderTreeView.KeyDown(var Key: Word; Shift: TShiftState);
var
  Item: HTREEITEM;
begin
  inherited;
  if (Key = VK_F2) and (Shift * [ssShift, ssAlt, ssCtrl] = []) then begin
    Key := 0;
    Item := TreeView_GetSelection(Handle);
    if Assigned(Item) then
      TreeView_EditLabel(Handle, Item);
  end;
end;

procedure TCustomFolderTreeView.CNKeyDown(var Message: TWMKeyDown);
var
  FocusWnd: HWND;
begin
  { On Delphi 5+, if a non-VCL control is focused, TApplication.IsKeyMsg will
    send the CN_KEYDOWN message to the nearest VCL control. This means that
    when the edit control is focused, the tree view itself gets CN_KEYDOWN
    messages. Don't let the VCL handle Enter and Escape; if we're on a dialog,
    those keys will close the window. }
  FocusWnd := GetFocus;
  if (FocusWnd <> 0) and (TreeView_GetEditControl(Handle) = FocusWnd) then
    if (Message.CharCode = VK_RETURN) or (Message.CharCode = VK_ESCAPE) then
      Exit;
  inherited;
end;

procedure TCustomFolderTreeView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  { For TVS_EX_DOUBLEBUFFER to be truly flicker-free, we must use
    comctl32's default WM_ERASEBKGND handling, not the VCL's (which calls
    FillRect). }
  DefaultHandler(Message);
end;

procedure TCustomFolderTreeView.WMCtlColorEdit(var Message: TMessage);
begin
  { We can't let TWinControl.DefaultHandler handle this message. It tries to
    send a CN_CTLCOLOREDIT message to the tree view's internally-created edit
    control, which it won't understand because it's not a VCL control. Without
    this special handling, the border is painted incorrectly on Windows XP
    with themes enabled. }
  Message.Result := DefWindowProc(Handle, Message.Msg, Message.WParam,
    Message.LParam);
end;

function TCustomFolderTreeView.GetItemFullPath(Item: HTREEITEM): String;
var
  TVItem: TTVItem;
begin
  Result := '';
  while Assigned(Item) do begin
    TVItem.mask := TVIF_PARAM;
    TVItem.hItem := Item;
    if not TreeView_GetItem(Handle, TVItem) then begin
      Result := '';
      Exit;
    end;
    if Result = '' then
      Result := PItemData(TVItem.lParam).Name
    else
      Insert(AddBackslash(PItemData(TVItem.lParam).Name), Result, 1);
    Item := TreeView_GetParent(Handle, Item);
  end;
end;

procedure TCustomFolderTreeView.Change;
var
  Item: HTREEITEM;
begin
  Item := TreeView_GetSelection(Handle);
  if Assigned(Item) then
    FDirectory := GetItemFullPath(Item)
  else
    FDirectory := '';
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomFolderTreeView.CNNotify(var Message: TWMNotify);
const
  TVN_SINGLEEXPAND = (TVN_FIRST-15);
  TVNRET_SKIPOLD = 1;
  TVNRET_SKIPNEW = 2;

  procedure HandleClick;
  var
    Item: HTREEITEM;
    HitTestInfo: TTVHitTestInfo;
  begin
    HitTestInfo.pt := ScreenToClient(SmallPointToPoint(TSmallPoint(GetMessagePos())));
    Item := TreeView_HitTest(Handle, HitTestInfo);
    if Assigned(Item) then begin
      if HitTestInfo.flags and TVHT_ONITEMBUTTON <> 0 then
        TreeView_Expand(Handle, Item, TVE_TOGGLE)
      else if TreeView_GetSelection(Handle) <> Item then
        SelectItem(Item);
    end;
  end;

var
  Hdr: PNMTreeView;
  SaveCursor: HCURSOR;
  DispItem: PTVItem;
  TVItem: TTVItem;
  S: String;
  Accept: Boolean;
begin
  inherited;
  case Message.NMHdr.code of
    TVN_DELETEITEM:
      begin
        Dispose(PItemData(PNMTreeView(Message.NMHdr).itemOld.lParam));
      end;
    TVN_ITEMEXPANDING:
      begin
        { Sanity check: Make sure this message isn't sent recursively.
          (See top of source code for details.) }
        if FItemExpanding then
          raise Exception.Create('Internal error: Item already expanding');
        FItemExpanding := True;
        try
          Hdr := PNMTreeView(Message.NMHdr);
          if (Hdr.action = TVE_EXPAND) and
             not PItemData(Hdr.itemNew.lParam).ChildrenAdded and
             not PItemData(Hdr.itemNew.lParam).NewItem then begin
            PItemData(Hdr.itemNew.lParam).ChildrenAdded := True;
            SaveCursor := SetCursor(LoadCursor(0, IDC_WAIT));
            try
              if ItemChildrenNeeded(Hdr.itemNew.hItem) then begin
                { If no subfolders were found, and there are no 'new' items
                  underneath the parent item, remove the '+' sign }
                if TreeView_GetChild(Handle, Hdr.itemNew.hItem) = nil then
                  SetItemHasChildren(Hdr.itemNew.hItem, False);
              end
              else begin
                { A result of False means no children were added due to a
                  temporary error and that it should try again next time }
                PItemData(Hdr.itemNew.lParam).ChildrenAdded := False;
                { Return 1 to cancel the expansion process (although it seems
                  to do that anyway when it sees no children were added) }
                Message.Result := 1;
              end;
            finally
              SetCursor(SaveCursor);
            end;
          end;
        finally
          FItemExpanding := False;
        end;
      end;
    TVN_GETDISPINFO:
      begin
        DispItem := @PTVDispInfo(Message.NMHdr).item;
        if DispItem.mask and TVIF_IMAGE <> 0 then begin
          DispItem.iImage := GetItemImageIndex(DispItem.hItem,
            PItemData(DispItem.lParam).NewItem, False);
        end;
        if DispItem.mask and TVIF_SELECTEDIMAGE <> 0 then begin
          DispItem.iSelectedImage := GetItemImageIndex(DispItem.hItem,
            PItemData(DispItem.lParam).NewItem, True);
        end;
        if DispItem.mask and TVIF_CHILDREN <> 0 then begin
          DispItem.cChildren := Ord(Assigned(TreeView_GetChild(Handle, DispItem.hItem)));
          if (DispItem.cChildren = 0) and not PItemData(DispItem.lParam).NewItem then
            DispItem.cChildren := Ord(ItemHasChildren(DispItem.hItem));
        end;
        { Store the values with the item so the callback isn't called again }
        DispItem.mask := DispItem.mask or TVIF_DI_SETITEM;
      end;
    TVN_SELCHANGED:
      begin
        if not FDestroyingHandle then
          Change;
      end;
    TVN_BEGINLABELEDIT:
      begin
        DispItem := @PTVDispInfo(Message.NMHdr).item;
        { Only 'new' items may be renamed }
        if not PItemData(DispItem.lParam).NewItem then
          Message.Result := 1;
      end;
    TVN_ENDLABELEDIT:
      begin
        DispItem := @PTVDispInfo(Message.NMHdr).item;
        { Only 'new' items may be renamed }
        if PItemData(DispItem.lParam).NewItem and
           Assigned(DispItem.pszText) then begin
          S := DispItem.pszText;
          Accept := True;
          if Assigned(FOnRename) then
            FOnRename(Self, S, Accept);
          if Accept then begin
            PItemData(DispItem.lParam).Name := S;
            { Instead of returning 1 to let the tree view update the text,
              set the text ourself. This will downconvert any Unicode
              characters to ANSI (if we're compiled as an ANSI app). }
            TVItem.mask := TVIF_TEXT;
            TVItem.hItem := DispItem.hItem;
            TVItem.pszText := PChar(S);
            TreeView_SetItem(Handle, TVItem);
            TreeView_SortChildren(Handle, TreeView_GetParent(Handle, DispItem.hItem), False);
            Change;
          end;
        end;
      end;
    NM_CLICK:
      begin
        { Use custom click handler to work more like Windows XP Explorer:
          - Items can be selected by clicking anywhere on their respective
            rows, except for the button.
          - In 'friendly tree' mode, clicking an item's icon or caption causes
            the item to expand, but never to collapse. }
        HandleClick;
        Message.Result := 1;
      end;
  end;
end;

procedure TCustomFolderTreeView.SetItemHasChildren(const Item: HTREEITEM;
  const AHasChildren: Boolean);
var
  TVItem: TTVItem;
begin
  TVItem.mask := TVIF_CHILDREN;
  TVItem.hItem := Item;
  TVItem.cChildren := Ord(AHasChildren);
  TreeView_SetItem(Handle, TVItem);
end;

procedure TCustomFolderTreeView.DeleteObsoleteNewItems(const ParentItem,
  ItemToKeep: HTREEITEM);
{ Destroys all 'new' items except for ItemToKeep and its parents. (ItemToKeep
  doesn't necessarily have to be a 'new' item.) Pass nil in the ParentItem
  parameter when calling this method. }

  function EqualsOrContains(const AParent: HTREEITEM; AChild: HTREEITEM): Boolean;
  begin
    Result := False;
    repeat
      if AChild = AParent then begin
        Result := True;
        Break;
      end;
      AChild := TreeView_GetParent(Handle, AChild);
    until AChild = nil;
  end;

var
  Item, NextItem: HTREEITEM;
  TVItem: TTVItem;
begin
  Item := TreeView_GetChild(Handle, ParentItem);
  while Assigned(Item) do begin
    { Determine the next item in advance since Item might get deleted }
    NextItem := TreeView_GetNextSibling(Handle, Item);
    TVItem.mask := TVIF_PARAM;
    TVItem.hItem := Item;
    if TreeView_GetItem(Handle, TVItem) then begin
      if PItemData(TVItem.lParam).NewItem and not EqualsOrContains(Item, ItemToKeep) then begin
        TreeView_DeleteItem(Handle, Item);
        { If there are no children left on the parent, remove its '+' sign }
        if TreeView_GetChild(Handle, ParentItem) = nil then
          SetItemHasChildren(ParentItem, False);
      end
      else
        DeleteObsoleteNewItems(Item, ItemToKeep);
    end;
    Item := NextItem;
  end;
end;

function TCustomFolderTreeView.InsertItem(const ParentItem: HTREEITEM;
  const AName, ACustomDisplayName: String; const ANewItem: Boolean): HTREEITEM;
var
  InsertStruct: TTVInsertStruct;
  ItemData: PItemData;
begin
  if ANewItem then
    DeleteObsoleteNewItems(nil, ParentItem);
  InsertStruct.hParent := ParentItem;
  if ANewItem then
    InsertStruct.hInsertAfter := TVI_SORT
  else
    InsertStruct.hInsertAfter := TVI_LAST;
  InsertStruct.item.mask := TVIF_TEXT or TVIF_IMAGE or
    TVIF_SELECTEDIMAGE or TVIF_CHILDREN or TVIF_PARAM;
  InsertStruct.item.hItem := nil;  { not used }
  if ANewItem then begin
    InsertStruct.item.mask := InsertStruct.item.mask or TVIF_STATE;
    InsertStruct.item.stateMask := TVIS_CUT;
    InsertStruct.item.state := TVIS_CUT;
  end;
  { Note: There's no performance advantage in using a callback for the text.
    During a TreeView_InsertItem call, the tree view will try to read the
    new item's text in order to update the horizontal scroll bar range.
    (It doesn't wait until the item is painted.)
    In addition, the caller may sort newly-inserted subitems, which obviously
    requires reading their text. }
  if ACustomDisplayName = '' then
    InsertStruct.item.pszText := PChar(AName)
  else
    InsertStruct.item.pszText := PChar(ACustomDisplayName);
  InsertStruct.item.iImage := I_IMAGECALLBACK;
  InsertStruct.item.iSelectedImage := I_IMAGECALLBACK;
  if ANewItem then
    InsertStruct.item.cChildren := 0
  else begin
    if ParentItem = nil then
      InsertStruct.item.cChildren := 1
    else
      InsertStruct.item.cChildren := I_CHILDRENCALLBACK;
  end;
  InsertStruct.item.lParam := 0;
  New(ItemData);
  ItemData.Name := AName;
  ItemData.NewItem := ANewItem;
  ItemData.ChildrenAdded := False;
  Pointer(InsertStruct.item.lParam) := ItemData;
  Result := TreeView_InsertItem(Handle, InsertStruct);
end;

function TCustomFolderTreeView.FindItem(const ParentItem: HTREEITEM;
  const AName: String): HTREEITEM;
var
  TVItem: TTVItem;
begin
  Result := TreeView_GetChild(Handle, ParentItem);
  while Assigned(Result) do begin
    TVItem.mask := TVIF_PARAM;
    TVItem.hItem := Result;
    if TreeView_GetItem(Handle, TVItem) then
      if PathCompare(PItemData(TVItem.lParam).Name, AName) = 0 then
        Break;
    Result := TreeView_GetNextSibling(Handle, Result);
  end;
end;

function TCustomFolderTreeView.FindOrCreateItem(const ParentItem: HTREEITEM;
  const AName: String): HTREEITEM;
begin
  Result := FindItem(ParentItem, AName);
  if Result = nil then begin
    if Assigned(ParentItem) then
      SetItemHasChildren(ParentItem, True);
    Result := InsertItem(ParentItem, AName, '', True);
  end;
end;

function TCustomFolderTreeView.GetRootItem: HTREEITEM;
begin
  Result := nil;
end;

procedure TCustomFolderTreeView.SelectItem(const Item: HTREEITEM);

  procedure ExpandParents(Item: HTREEITEM);
  begin
    Item := TreeView_GetParent(Handle, Item);
    if Assigned(Item) then begin
      ExpandParents(Item);
      TreeView_Expand(Handle, Item, TVE_EXPAND);
    end;
  end;

begin
  { Must manually expand parents prior to calling TreeView_SelectItem;
    see top of source code for details }
  if Assigned(Item) then
    ExpandParents(Item);
  TreeView_SelectItem(Handle, Item);
end;

function TCustomFolderTreeView.TryExpandItem(const Item: HTREEITEM): Boolean;
{ Tries to expand the specified item. Returns True if the item's children were
  initialized (if any), or False if the initialization failed due to a
  temporary error (i.e. ItemChildrenNeeded returned False). }
var
  TVItem: TTVItem;
begin
  TreeView_Expand(Handle, Item, TVE_EXPAND);
  TVItem.mask := TVIF_CHILDREN or TVIF_PARAM;
  TVItem.hItem := Item;
  Result := TreeView_GetItem(Handle, TVItem) and
    (PItemData(TVItem.lParam).ChildrenAdded or (TVItem.cChildren = 0));
end;

procedure TCustomFolderTreeView.ChangeDirectory(const Value: String;
  const CreateNewItems: Boolean);
{ Changes to the specified directory. Value must begin with a drive letter
  (e.g. "C:\directory"); relative paths and UNC paths are not allowed.
  If CreateNewItems is True, new items will be created if one or more elements
  of the path do not exist. }
var
  PStart, PEnd: PChar;
  S: String;
  ParentItem, Item: HTREEITEM;
begin
  SelectItem(nil);

  ParentItem := GetRootItem;
  PStart := PChar(Value);
  while PStart^ <> #0 do begin
    if Assigned(ParentItem) then
      if not TryExpandItem(ParentItem) then
        Break;

    { Extract a single path component }
    PEnd := PStart;
    while (PEnd^ <> #0) and not PathCharIsSlash(PEnd^) do
      PEnd := PathStrNextChar(PEnd);
    SetString(S, PStart, PEnd - PStart);

    { Find that component under ParentItem }
    if CreateNewItems and Assigned(ParentItem) then
      Item := FindOrCreateItem(ParentItem, S)
    else
      Item := FindItem(ParentItem, S);
    if Item = nil then
      Break;
    ParentItem := Item;

    PStart := PEnd;
    while PathCharIsSlash(PStart^) do
      Inc(PStart);
  end;

  if Assigned(ParentItem) then
    SelectItem(ParentItem);
end;

procedure TCustomFolderTreeView.SetDirectory(const Value: String);
begin
  ChangeDirectory(Value, False);
end;

procedure TCustomFolderTreeView.CreateNewDirectory(const ADefaultName: String);
{ Creates a new node named AName underneath the selected node. Does nothing
  if there is no selected node. }
var
  ParentItem, Item: HTREEITEM;
  I: Integer;
  S: String;
begin
  ParentItem := TreeView_GetSelection(Handle);
  if ParentItem = nil then
    Exit;

  DeleteObsoleteNewItems(nil, ParentItem);

  { Expand and find a unique name }
  if not TryExpandItem(ParentItem) then
    Exit;
  I := 0;
  repeat
    Inc(I);
    if I = 1 then
      S := ADefaultName
    else
      S := ADefaultName + Format(' (%d)', [I]);
  until FindItem(ParentItem, S) = nil;

  SetItemHasChildren(ParentItem, True);
  Item := InsertItem(ParentItem, S, '', True);
  SelectItem(Item);

  if CanFocus then
    SetFocus;
  TreeView_EditLabel(Handle, Item);
end;

{ TFolderTreeView }

function TFolderTreeView.ItemChildrenNeeded(const Item: HTREEITEM): Boolean;

  procedure AddDrives;
  var
    Drives: DWORD;
    Drive: Char;
  begin
    Drives := GetLogicalDrives;
    for Drive := 'A' to 'Z' do begin
      if (Drives and 1 <> 0) or IsNetworkDrive(Drive) then
        InsertItem(nil, Drive + ':', GetFileDisplayName(Drive + ':\'), False);
      Drives := Drives shr 1;
    end;
  end;

  function AddSubdirectories(const ParentItem: HTREEITEM;
    const Path: String): Boolean;
  var
    OldErrorMode: UINT;
    H: THandle;
    FindData: TWin32FindData;
    S: String;
  begin
    OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
      { The path might be on a disconnected network drive. Ensure it's
        connected before attempting to enumerate subdirectories. }
      if Length(Path) = 3 then begin  { ...only do this on the root }
        if not EnsurePathIsAccessible(Path) then begin
          Result := False;
          Exit;
        end;
        { Refresh the icon and text in case the drive was indeed reconnected }
        RefreshDriveItem(ParentItem, GetFileDisplayName(Path));
      end;
      Result := True;

      H := FindFirstFile(PChar(AddBackslash(Path) + '*'), FindData);
      if H <> INVALID_HANDLE_VALUE then begin
        try
          repeat
            if IsListableDirectory(FindData) then begin
              S := FindData.cFileName;
              InsertItem(ParentItem, S, GetFileDisplayName(AddBackslash(Path) + S),
                False);
            end;
          until not FindNextFile(H, FindData);
        finally
          Windows.FindClose(H);
        end;
      end;
    finally
      SetErrorMode(OldErrorMode);
    end;
  end;

begin
  if Item = nil then begin
    AddDrives;
    Result := True;
  end
  else begin
    Result := AddSubdirectories(Item, GetItemFullPath(Item));
    if Result then begin
      { When a text callback is used, sorting after all items are inserted is
        exponentially faster than using hInsertAfter=TVI_SORT }
      TreeView_SortChildren(Handle, Item, False);
    end;
  end;
end;

function TFolderTreeView.GetItemFullPath(Item: HTREEITEM): String;
begin
  Result := inherited GetItemFullPath(Item);
  if (Length(Result) = 2) and (Result[2] = ':') then
    Result := Result + '\';
end;

function TFolderTreeView.GetItemImageIndex(const Item: HTREEITEM;
  const NewItem, SelectedImage: Boolean): Integer;
begin
  if NewItem then
    Result := GetDefFolderImageIndex(SelectedImage)
  else
    Result := GetFileImageIndex(GetItemFullPath(Item), SelectedImage);
end;

function TFolderTreeView.ItemHasChildren(const Item: HTREEITEM): Boolean;
var
  Path: String;
  OldErrorMode: UINT;
begin
  Path := GetItemFullPath(Item);
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Result := (GetDriveType(PChar(AddBackslash(PathExtractDrive(Path)))) = DRIVE_REMOTE) or
      HasSubfolders(Path);
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

procedure TFolderTreeView.RefreshDriveItem(const Item: HTREEITEM;
  const ANewDisplayName: String);
var
  TVItem: TTVItem;
begin
  TVItem.mask := TVIF_IMAGE or TVIF_SELECTEDIMAGE;
  TVItem.hItem := Item;
  TVItem.iImage := I_IMAGECALLBACK;
  TVItem.iSelectedImage := I_IMAGECALLBACK;
  if ANewDisplayName <> '' then begin
    TVItem.mask := TVItem.mask or TVIF_TEXT;
    TVItem.pszText := PChar(ANewDisplayName);
  end;
  TreeView_SetItem(Handle, TVItem);
end;

{ TStartMenuFolderTreeView }

procedure TStartMenuFolderTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style and not TVS_LINESATROOT;
end;

function TStartMenuFolderTreeView.GetItemImageIndex(const Item: HTREEITEM;
  const NewItem, SelectedImage: Boolean): Integer;
begin
  Result := FImageIndexes[SelectedImage];
end;

function TStartMenuFolderTreeView.GetRootItem: HTREEITEM;
begin
  { The top item ('Programs') is considered the root }
  Result := TreeView_GetRoot(Handle);
end;

function TStartMenuFolderTreeView.ItemChildrenNeeded(const Item: HTREEITEM): Boolean;

  procedure AddSubfolders(const ParentItem: HTREEITEM; const Path, StartupPath: String);
  var
    StartupName: String;
    OldErrorMode: UINT;
    H: THandle;
    FindData: TWin32FindData;
    S: String;
  begin
    { Determine the name of the Startup folder so that we can hide it from the
      list }
    if StartupPath <> '' then
      if PathCompare(AddBackslash(Path), PathExtractPath(StartupPath)) = 0 then
        StartupName := PathExtractName(StartupPath);

    OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
      H := FindFirstFile(PChar(AddBackslash(Path) + '*'), FindData);
      if H <> INVALID_HANDLE_VALUE then begin
        try
          repeat
            if IsListableDirectory(FindData) then begin
              S := FindData.cFileName;
              if PathCompare(S, StartupName) <> 0 then
                if FindItem(ParentItem, S) = nil then
                  InsertItem(ParentItem, S, GetFileDisplayName(AddBackslash(Path) + S), False);
            end;
          until not FindNextFile(H, FindData);
        finally
          Windows.FindClose(H);
        end;
      end;
    finally
      SetErrorMode(OldErrorMode);
    end;
  end;

var
  Root, S: String;
  NewItem: HTREEITEM;
  Path: String;
begin
  Result := True;
  if Item = nil then begin
    Root := FUserPrograms;
    if Root = '' then begin
      { User programs folder doesn't exist for some reason? }
      Root := FCommonPrograms;
      if Root = '' then
        Exit;
    end;
    FImageIndexes[False] := GetFileImageIndex(Root, False);
    FImageIndexes[True] := FImageIndexes[False];
    S := GetFileDisplayName(Root);
    if S = '' then
      S := PathExtractName(Root);
    NewItem := InsertItem(nil, '', S, False);
    TreeView_Expand(Handle, NewItem, TVE_EXPAND);
  end
  else begin
    Path := GetItemFullPath(Item);
    if FCommonPrograms <> '' then
      AddSubfolders(Item, AddBackslash(FCommonPrograms) + Path, FCommonStartup);
    if FUserPrograms <> '' then
      AddSubfolders(Item, AddBackslash(FUserPrograms) + Path, FUserStartup);
    TreeView_SortChildren(Handle, Item, False);
  end;
end;

function TStartMenuFolderTreeView.ItemHasChildren(const Item: HTREEITEM): Boolean;
var
  Path: String;
begin
  Path := GetItemFullPath(Item);
  if (FCommonPrograms <> '') and HasSubfolders(AddBackslash(FCommonPrograms) + Path) then
    Result := True
  else if (FUserPrograms <> '') and HasSubfolders(AddBackslash(FUserPrograms) + Path) then
    Result := True
  else
    Result := False;
end;

procedure TStartMenuFolderTreeView.SetPaths(const AUserPrograms, ACommonPrograms,
  AUserStartup, ACommonStartup: String);
begin
  FUserPrograms := AUserPrograms;
  FCommonPrograms := ACommonPrograms;
  FUserStartup := AUserStartup;
  FCommonStartup := ACommonStartup;
  RecreateWnd;
end;

function GetSystemDir: String;
var
  Buf: array[0..MAX_PATH-1] of Char;
begin
  GetSystemDirectory(Buf, SizeOf(Buf) div SizeOf(Buf[0]));
  Result := StrPas(Buf);
end;

initialization
  InitThemeLibrary;
  SHPathPrepareForWriteFunc := GetProcAddress(LoadLibrary(PChar(AddBackslash(GetSystemDir) + shell32)),
    'SHPathPrepareForWriteW');
end.
