unit IDE.MainForm.UpdateMenuHelper;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler form - Menu update helper which has the find & replace helper as ancestor

  Not used by MainForm: it uses IDE.MainForm.FinalHelper instead
}

interface

uses
  Menus,
  IDE.MainForm, IDE.MainForm.FindReplaceHelper;

type
  TMainFormUpdateMenuHelper = class helper(TMainFormFindReplaceHelper) for TMainForm
    procedure UpdateFileMenu(const Menu: TMenuItem);
    procedure UpdateEditMenu(const Menu: TMenuItem);
    procedure UpdateViewMenu(const Menu: TMenuItem);
    procedure UpdateBuildMenu(const Menu: TMenuItem);
    procedure UpdateMemosTabSetMenu(const Menu: TMenuItem);
    procedure UpdateHelpMenu(const Menu: TMenuItem);
    procedure UpdateSimpleMenu(const Menu: TMenuItem);
    procedure UpdateToolsMenu(const Menu: TMenuItem);
    procedure UpdateRunMenu2(const Menu: TMenuItem);
    procedure UpdateBreakPointsMenu(const Menu: TMenuItem);
    { Private }
    procedure _UpdateMenuBitmapsIfNeeded;
    procedure _ApplyMenuBitmaps(const ParentMenuItem: TMenuItem);
    procedure _UpdateReopenTabMenu(const Menu: TMenuItem);
    function _AnyMemoHasBreakPoint: Boolean;
  end;

implementation

uses
  Windows, CommCtrl,
  SysUtils, Generics.Collections, VirtualImageList, ComCtrls,
  PathFunc,
  Shared.LicenseFunc,
  IDE.HelperFunc, IDE.IDEScintEdit;

procedure TMainFormUpdateMenuHelper._UpdateMenuBitmapsIfNeeded;

  procedure AddMenuBitmap(const MenuBitmaps: TMenuBitmaps; const DC: HDC; const BitmapInfo: TBitmapInfo;
    const MenuItem: TMenuItem; const ImageList: TVirtualImageList; const ImageIndex: Integer); overload;
  begin
    var pvBits: Pointer;
    var Bitmap := CreateDIBSection(DC, bitmapInfo, DIB_RGB_COLORS, pvBits, 0, 0);
    var OldBitmap := SelectObject(DC, Bitmap);
    if ImageList_Draw(ImageList.Handle, ImageIndex, DC, 0, 0, ILD_TRANSPARENT) then
      MenuBitmaps.Add(MenuItem, Bitmap)
    else begin
      SelectObject(DC, OldBitmap);
      DeleteObject(Bitmap);
    end;
  end;

  procedure AddMenuBitmap(const MenuBitmaps: TMenuBitmaps; const DC: HDC; const BitmapInfo: TBitmapInfo;
    const MenuItem: TMenuItem; const ImageList: TVirtualImageList; const ImageName: String); overload;
  begin
    AddMenuBitmap(MenuBitmaps, DC, BitmapInfo, MenuItem, ImageList, ImageList.GetIndexByName(ImageName));
  end;

type
  TButtonedMenu = TPair<TMenuItem, TToolButton>;
  TNamedMenu = TPair<TMenuItem, String>;

  function BM(const MenuItem: TMenuItem; const ToolButton: TToolButton): TButtonedMenu;
  begin
    Result := TButtonedMenu.Create(MenuItem, ToolButton); { This is a record so no need to free }
  end;

  function NM(const MenuItem: TMenuItem; const Name: String): TNamedMenu;
  begin
    Result := TNamedMenu.Create(MenuItem, Name); { This is a record so no need to free }
  end;

begin
  { This will create bitmaps for the current DPI using ImageList_Draw.

    These draw perfectly even on Windows 7. Other techniques don't work because
    they loose transparency or only look good on Windows 8 and later. Or they do
    work but cause lots more VCL code to be run than just our simple CreateDIB+Draw
    combo.

    ApplyBitmaps will apply them to menu items using SetMenuItemInfo. The menu item
    does not copy the bitmap so they should still be alive after ApplyBitmaps is done.

    Depends on FMenuImageList to pick the best size icons for the current DPI
    from the collection. }

  var ImageList := FMenuImageList;

  var NewSize: TSize;
  NewSize.cx := ImageList.Width;
  NewSize.cy := ImageList.Height;
  if (NewSize.cx <> FMenuBitmapsSize.cx) or (NewSize.cy <> FMenuBitmapsSize.cy) or
     (ImageList.ImageCollection <> FMenuBitmapsSourceImageCollection) then begin

    { Cleanup previous }

    for var Bitmap in FMenuBitmaps.Values do
      DeleteObject(Bitmap);
    FMenuBitmaps.Clear;

    { Create }

    var DC := CreateCompatibleDC(0);
    if DC <> 0 then begin
      try
        var BitmapInfo := CreateBitmapInfo(NewSize.cx, NewSize.cy, 32);

        var ButtonedMenus := [
          BM(FNewMainFile, NewMainFileButton),
          BM(FOpenMainFile, OpenMainFileButton),
          BM(FSave, SaveButton),
          BM(BCompile, CompileButton),
          BM(BStopCompile, StopCompileButton),
          BM(RRun, RunButton),
          BM(RPause, PauseButton),
          BM(RTerminate, TerminateButton),
          BM(HDoc, HelpButton)];

        for var ButtonedMenu in ButtonedMenus do
          AddMenuBitmap(FMenuBitmaps, DC, BitmapInfo, ButtonedMenu.Key, ImageList, ButtonedMenu.Value.ImageIndex);

        var NamedMenus := [
          NM(FClearRecent, 'eraser'),
          NM(FSaveMainFileAs, 'save-as-filled'),
          NM(FSaveAll, 'save-all-filled'),
          NM(FPrint, 'printer'),
          NM(EUndo, 'command-undo-1'),
          NM(ERedo, 'command-redo-1'),
          NM(ECut, 'clipboard-cut'),
          NM(ECopy, 'clipboard-copy'),
          NM(POutputListCopy, 'clipboard-copy'),
          NM(EPaste, 'clipboard-paste'),
          NM(EDelete, 'symbol-cancel'),
          NM(ESelectAll, 'select-all'),
          NM(POutputListSelectAll, 'select-all'),
          NM(EFind, 'find'),
          NM(EFindInFiles, 'folder-open-filled-find'),
          //NM(EFindNext, 'unused\find-arrow-right-2'),
          //NM(EFindPrevious, 'unused\find-arrow-left-2'),
          NM(EReplace, 'replace'),
          NM(EFoldLine, 'symbol-remove'),
          NM(EUnfoldLine, 'symbol-add'),
          NM(VZoomIn, 'zoom-in'),
          NM(VZoomOut, 'zoom-out'),
          NM(VNextTab, 'control-tab-filled-arrow-right-2'),
          NM(VPreviousTab, 'control-tab-filled-arrow-left-2'),
          //NM(VCloseCurrentTab, 'unused\control-tab-filled-cancel-2'),
          NM(VReopenTabs, 'control-tab-filled-redo-1'),
          NM(VReopenTabs2, 'control-tab-filled-redo-1'),
          NM(BOpenOutputFolder, 'folder-open-filled'),
          NM(RParameters, 'control-edit'),
          NM(RRunToCursor, 'debug-start-filled-arrow-right-2'),
          NM(RStepInto, 'debug-step-into'),
          NM(RStepOver, 'debug-step-over'),
          NM(RStepOut, 'debug-step-out'),
          NM(RToggleBreakPoint, 'debug-breakpoint-filled'),
          NM(RToggleBreakPoint2, 'debug-breakpoint-filled'),
          NM(RDeleteBreakPoints, 'debug-breakpoints-filled-eraser'),
          NM(RDeleteBreakPoints2, 'debug-breakpoints-filled-eraser'),
          NM(REvaluate, 'variables'),
          NM(TAddRemovePrograms, 'application'),
          NM(TGenerateGUID, 'tag-script-filled'),
          NM(TFilesDesigner, 'documents-script-filled'),
          NM(TRegistryDesigner, 'control-tree-script-filled'),
          NM(TMsgBoxDesigner, 'comment-text-script-filled'),
          NM(TSignTools, 'padlock-filled'),
          NM(TOptions, 'gear-filled'),
          NM(HPurchase, 'shopping-cart'),
          NM(HRegister, 'key-filled'),
          NM(HDonate, 'heart-filled'),
          NM(HMailingList, 'alert-filled'),
          NM(HWhatsNew, 'announcement'),
          NM(HWebsite, 'home'),
          NM(HAbout, 'button-info')];

        for var NamedMenu in NamedMenus do
          AddMenuBitmap(FMenuBitmaps, DC, BitmapInfo, NamedMenu.Key, ImageList, NamedMenu.Value);
      finally
        DeleteDC(DC);
      end;
    end;

    FMenuBitmapsSize := NewSize;
    FMenuBitmapsSourceImageCollection := FMenuImageList.ImageCollection;
  end;
end;

procedure TMainFormUpdateMenuHelper._ApplyMenuBitmaps(const ParentMenuItem: TMenuItem);
begin
  _UpdateMenuBitmapsIfNeeded;

  { Setting MainMenu1.ImageList or a menu item's .Bitmap to make a menu item
    show a bitmap is not OK: it causes the entire menu to become owner drawn
    which makes it looks different from native menus and additionally the trick
    SetFakeShortCut uses doesn't work with owner drawn menus.

    Instead UpdateMenuBitmapsIfNeeded has prepared images which can be applied
    to native menu items using SetMenuItemInfo and MIIM_BITMAP - which is what we
    do below.

    A problem with this is that Delphi's TMenu likes to constantly recreate the
    underlying native menu items, for example when updating the caption. Sometimes
    it will even destroy and repopulate an entire menu because of a simple change
    like setting the caption of a single item!

    This means the result of our SetMenuItemInfo call (which Delphi doesn't know
    about) will quickly become lost when Delphi recreates the menu item.

    Fixing this in the OnChange event is not possible, this is event is more
    than useless.

    The solution is shown by TMenu.DispatchPopup: in reaction to WM_INITMENUPOPUP
    it calls our Click events right before the menu is shown, giving us the
    opportunity to call SetMenuItemInfo for the menu's items.

    This works unless Delphi decides to destroy and repopulate the menu after
    calling Click. Most amazingly it can do that indeed: it does this if the DPI
    changed since the last popup or if a automatic hotkey change or line reduction
    happens due to the menu's AutoHotkeys or AutoLineReduction properties. To make
    things even worse: for the Run menu it does this each and every time it is
    opened: this menu currently has a 'Step Out' item which has no shortcut but
    also all its letters are taken by another item already. This confuses the
    AutoHotkeys code, making it destroy and repopulate the entire menu over and
    over because it erroneously thinks a hotkey changed.

    To avoid this MainMenu1.AutoHotkeys was set to maManual since we have always
    managed the hotkeys ourselves anyway and .AutoLineReduction was also set to
    maManual and we now manage that ourselves as well.

    This just leave an issue with the icons not appearing on the first popup after
    a DPI change and this seems like a minor issue only.

    For TPopupMenu: calling ApplyMenuBitmaps(PopupMenu.Items) does work but makes
    the popup only show icons without text. This seems to be a limitiation of menus
    created by CreatePopupMenu instead of CreateMenu. This is why our popups with
    icons are all menu items popped using TMainFormPopupMenu. These menu items
    are hidden in the main menu and temporarily shown on popup. Popping an always
    hidden menu item (or a visible one as a child of a hidden parent) doesnt work.  }

  var mmi: TMenuItemInfo;
  mmi.cbSize := SizeOf(mmi);
  mmi.fMask := MIIM_BITMAP;

  for var I := 0 to ParentMenuItem.Count-1 do begin
    var MenuItem := ParentMenuItem.Items[I];
    if MenuItem.Visible then begin
      if FMenuBitmaps.TryGetValue(MenuItem, mmi.hbmpItem) then
        SetMenuItemInfo(ParentMenuItem.Handle, MenuItem.Command, False, mmi);
      if MenuItem.Count > 0 then
        _ApplyMenuBitmaps(MenuItem);
    end;
  end;
end;

procedure TMainFormUpdateMenuHelper.UpdateFileMenu(const Menu: TMenuItem);
var
  I: Integer;
begin
  FSaveMainFileAs.Enabled := FActiveMemo = FMainMemo;
  FSaveEncoding.Enabled := FSave.Enabled; { FSave.Enabled is kept up-to-date by UpdateSaveMenuItemAndButton }
  FSaveEncodingAuto.Checked := FSaveEncoding.Enabled and ((FActiveMemo as TIDEScintFileEdit).SaveEncoding = seAuto);
  FSaveEncodingUTF8WithBOM.Checked := FSaveEncoding.Enabled and ((FActiveMemo as TIDEScintFileEdit).SaveEncoding = seUTF8WithBOM);
  FSaveEncodingUTF8WithoutBOM.Checked := FSaveEncoding.Enabled and ((FActiveMemo as TIDEScintFileEdit).SaveEncoding = seUTF8WithoutBOM);
  FSaveAll.Visible := FOptions.OpenIncludedFiles;
  ReadMRUMainFilesList;
  FRecent.Visible := FMRUMainFilesList.Count <> 0;
  for I := 0 to High(FMRUMainFilesMenuItems) do
    with FMRUMainFilesMenuItems[I] do begin
      if I < FMRUMainFilesList.Count then begin
        Visible := True;
        Caption := '&' + IntToStr((I+1) mod 10) + ' ' + DoubleAmp(FMRUMainFilesList[I]);
      end
      else
        Visible := False;
    end;

  _ApplyMenuBitmaps(Menu);
end;

procedure TMainFormUpdateMenuHelper.UpdateEditMenu(const Menu: TMenuItem);
var
  MemoHasFocus, MemoIsReadOnly: Boolean;
begin
  MemoHasFocus := FActiveMemo.Focused;
  MemoIsReadOnly := FActiveMemo.ReadOnly;
  EUndo.Enabled := MemoHasFocus and FActiveMemo.CanUndo;
  ERedo.Enabled := MemoHasFocus and FActiveMemo.CanRedo;
  ECut.Enabled := MemoHasFocus and not MemoIsReadOnly and not FActiveMemo.SelEmpty;
  ECopy.Enabled := MemoHasFocus and not FActiveMemo.SelEmpty;
  EPaste.Enabled := MemoHasFocus and FActiveMemo.CanPaste;
  EDelete.Enabled := MemoHasFocus and not FActiveMemo.SelEmpty;
  ESelectAll.Enabled := MemoHasFocus;
  ESelectNextOccurrence.Enabled := MemoHasFocus;
  ESelectAllOccurrences.Enabled := MemoHasFocus;
  ESelectAllFindMatches.Enabled := MemoHasFocus and (FLastFindText <> '');
  EFind.Enabled := MemoHasFocus;
  EFindNext.Enabled := MemoHasFocus;
  EFindPrevious.Enabled := MemoHasFocus;
  EReplace.Enabled := MemoHasFocus and not MemoIsReadOnly;
  EFindRegEx.Checked := FOptions.FindRegEx;
  EFoldLine.Visible := FOptions.UseFolding;
  EFoldLine.Enabled := MemoHasFocus;
  EUnfoldLine.Visible := EFoldLine.Visible;
  EUnfoldLine.Enabled := EFoldLine.Enabled;
  EGoto.Enabled := MemoHasFocus;
  EToggleLinesComment.Enabled := not MemoIsReadOnly;
  EBraceMatch.Enabled := MemoHasFocus;

  _ApplyMenuBitmaps(Menu);
end;

procedure TMainFormUpdateMenuHelper._UpdateReopenTabMenu(const Menu: TMenuItem);
begin
  Menu.Clear;
  for var I := 0 to FHiddenFiles.Count-1 do begin
    var MenuItem := TMenuItem.Create(Menu);
    MenuItem.Caption := '&' + IntToStr((I+1) mod 10) + ' ' + DoubleAmp(PathExtractName(FHiddenFiles[I]));
    MenuItem.Tag := I;
    MenuItem.OnClick := ReopenTabClick;
    Menu.Add(MenuItem);
  end;
end;

procedure TMainFormUpdateMenuHelper.UpdateViewMenu(const Menu: TMenuItem);
begin
  VZoomIn.Enabled := (FActiveMemo.Zoom < 20);
  VZoomOut.Enabled := (FActiveMemo.Zoom > -10);
  VZoomReset.Enabled := (FActiveMemo.Zoom <> 0);
  VToolbar.Checked := ToolbarPanel.Visible;
  VStatusBar.Checked := StatusBar.Visible;
  VNextTab.Enabled := MemosTabSet.Visible and (MemosTabSet.Tabs.Count > 1);
  VPreviousTab.Enabled := VNextTab.Enabled;
  VCloseCurrentTab.Enabled := MemosTabSet.Visible and (FActiveMemo <> FMainMemo) and (FActiveMemo <> FPreprocessorOutputMemo);
  VReopenTab.Visible := MemosTabSet.Visible and (FHiddenFiles.Count > 0);
  if VReopenTab.Visible then
    _UpdateReopenTabMenu(VReopenTab);
  VReopenTabs.Visible := VReopenTab.Visible;
  VHide.Checked := not StatusPanel.Visible;
  VCompilerOutput.Checked := StatusPanel.Visible and (OutputTabSet.TabIndex = tiCompilerOutput);
  VDebugOutput.Checked := StatusPanel.Visible and (OutputTabSet.TabIndex = tiDebugOutput);
  VDebugCallStack.Checked := StatusPanel.Visible and (OutputTabSet.TabIndex = tiDebugCallStack);
  VFindResults.Checked := StatusPanel.Visible and (OutputTabSet.TabIndex = tiFindResults);
  VWordWrap.Checked := FOptions.WordWrap;

  _ApplyMenuBitmaps(Menu);
end;

procedure TMainFormUpdateMenuHelper.UpdateBuildMenu(const Menu: TMenuItem);
begin
  BLowPriority.Checked := FOptions.LowPriorityDuringCompile;
  BOpenOutputFolder.Enabled := (FCompiledExe <> '');

  _ApplyMenuBitmaps(Menu);
end;

procedure TMainFormUpdateMenuHelper.UpdateMemosTabSetMenu(const Menu: TMenuItem);
begin
  { Main and preprocessor memos can't be hidden }
  VCloseCurrentTab2.Enabled := (FActiveMemo <> FMainMemo) and (FActiveMemo <> FPreprocessorOutputMemo);

  VReopenTab2.Visible := FHiddenFiles.Count > 0;
  if VReopenTab2.Visible then
    _UpdateReopenTabMenu(VReopenTab2);
  VReopenTabs2.Visible := VReopenTab2.Visible;

  _ApplyMenuBitmaps(Menu);
end;

procedure TMainFormUpdateMenuHelper.UpdateHelpMenu(const Menu: TMenuItem);
begin
  HUnregister.Visible := IsLicensed;
  HDonate.Visible := not HUnregister.Visible;

  _ApplyMenuBitmaps(Menu);
end;

procedure TMainFormUpdateMenuHelper.UpdateSimpleMenu(const Menu: TMenuItem);
begin
  _ApplyMenuBitmaps(Menu);
end;

procedure TMainFormUpdateMenuHelper.UpdateToolsMenu(const Menu: TMenuItem);
var
  MemoIsReadOnly: Boolean;
begin
  MemoIsReadOnly := FActiveMemo.ReadOnly;
  TGenerateGUID.Enabled := not MemoIsReadOnly;
  TMsgBoxDesigner.Enabled := not MemoIsReadOnly;
  TFilesDesigner.Enabled := not MemoIsReadOnly;
  TRegistryDesigner.Enabled := not MemoIsReadOnly;

  _ApplyMenuBitmaps(Menu);
end;

function TMainFormUpdateMenuHelper._AnyMemoHasBreakPoint: Boolean;
begin
  { Also see RDeleteBreakPointsClick }
  for var Memo in FFileMemos do
    if Memo.Used and (Memo.BreakPoints.Count > 0) then
      Exit(True);
  Result := False;
end;

procedure TMainFormUpdateMenuHelper.UpdateRunMenu2(const Menu: TMenuItem);
begin
  RDeleteBreakPoints.Enabled := _AnyMemoHasBreakPoint;
  { See UpdateRunMenu for other menu items }

  _ApplyMenuBitmaps(Menu);
end;

procedure TMainFormUpdateMenuHelper.UpdateBreakPointsMenu(const Menu: TMenuItem);
begin
  RToggleBreakPoint2.Enabled := FActiveMemo is TIDEScintFileEdit;
  RDeleteBreakPoints2.Enabled := _AnyMemoHasBreakPoint;
  { Also see UpdateRunMenu }

  _ApplyMenuBitmaps(Menu);
end;

end.
