unit Themes;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  VCL Styles dummy functions to easily deactivate inclusion of actual VCL Styles code

  Just add it to your .dpr and use Themes instead of Vcl.Themes everywhere. Requires Vcl to
  be in the project's unit scope names list to be able to use same code with real Vcl.Themes.

  In units it must be used after ComCtrls and Forms.
}

interface

uses
  Windows, Controls, Graphics, Types;

type
  TStyleHook = TObject;
  TStyleHookClass = class of TStyleHook;

  TStyleColor = (scBorder, scButtonDisabled, scButtonFocused, scButtonHot,
    scButtonNormal, scButtonPressed, scCategoryButtons, scCategoryButtonsGradientBase,
    scCategoryButtonsGradientEnd, scCategoryPanelGroup, scComboBox,
    scComboBoxDisabled, scEdit, scEditDisabled, scGrid, scGenericBackground,
    scGenericGradientBase, scGenericGradientEnd, scHintGradientBase,
    scHintGradientEnd, scListBox, scListBoxDisabled, scListView, scPanel, scPanelDisabled,
    scSplitter, scToolBarGradientBase, scToolBarGradientEnd, scTreeView, scWindow);

  TStyleFont = (
    sfButtonTextDisabled, sfButtonTextFocused, sfButtonTextHot, sfButtonTextNormal, sfButtonTextPressed,
    sfCaptionTextInactive, sfCaptionTextNormal,
    sfCategoryPanelGroupHeaderHot, sfCategoryPanelGroupHeaderNormal, sfCategoryButtonsCategoryNormal, sfCategoryButtonsCategorySelected,
    sfCategoryButtonsHot, sfCategoryButtonsNormal, sfCategoryButtonsSelected,
    sfCheckBoxTextDisabled, sfCheckBoxTextFocused, sfCheckBoxTextHot, sfCheckBoxTextNormal, sfCheckBoxTextPressed,
    sfComboBoxItemDisabled, sfComboBoxItemFocused, sfComboBoxItemHot, sfComboBoxItemNormal, sfComboBoxItemSelected,
    sfEditBoxTextDisabled, sfEditBoxTextFocused, sfEditBoxTextHot, sfEditBoxTextNormal, sfEditBoxTextSelected,
    sfGridItemFixedHot, sfGridItemFixedNormal, sfGridItemFixedPressed, sfGridItemNormal, sfGridItemSelected,
    sfGroupBoxTextDisabled, sfGroupBoxTextNormal,
    sfHeaderSectionTextDisabled, sfHeaderSectionTextHot, sfHeaderSectionTextNormal, sfHeaderSectionTextPressed,
    sfListItemTextDisabled, sfListItemTextFocused, sfListItemTextHot, sfListItemTextNormal, sfListItemTextSelected,
    sfMenuItemTextDisabled, sfMenuItemTextHot, sfMenuItemTextNormal, sfMenuItemTextSelected,
    sfPanelTextDisabled, sfPanelTextNormal,
    sfPopupMenuItemTextDisabled, sfPopupMenuItemTextHot, sfPopupMenuItemTextNormal, sfPopupMenuItemTextSelected,
    sfRadioButtonTextDisabled, sfRadioButtonTextFocused, sfRadioButtonTextHot, sfRadioButtonTextNormal, sfRadioButtonTextPressed,
    sfSmCaptionTextInactive, sfSmCaptionTextNormal,
    sfStatusPanelTextDisabled, sfStatusPanelTextNormal,
    sfTabTextActiveDisabled, sfTabTextActiveHot, sfTabTextActiveNormal, sfTabTextInactiveDisabled, sfTabTextInactiveHot, sfTabTextInactiveNormal,
    sfTextLabelDisabled, sfTextLabelFocused, sfTextLabelHot, sfTextLabelNormal,
    sfToolItemTextDisabled, sfToolItemTextHot, sfToolItemTextNormal, sfToolItemTextSelected,
    sfTreeItemTextDisabled, sfTreeItemTextFocused, sfTreeItemTextHot, sfTreeItemTextNormal, sfTreeItemTextSelected,
    sfWindowTextDisabled, sfWindowTextNormal
  );

  TThemedButton = (
    tbButtonDontCare,
    tbButtonRoot,
    tbPushButtonNormal, tbPushButtonHot, tbPushButtonPressed, tbPushButtonDisabled, tbPushButtonDefaulted, tbPushButtonDefaultedAnimating {Windows Vista or later},
    tbRadioButtonUncheckedNormal, tbRadioButtonUncheckedHot, tbRadioButtonUncheckedPressed, tbRadioButtonUncheckedDisabled,
    tbRadioButtonCheckedNormal, tbRadioButtonCheckedHot, tbRadioButtonCheckedPressed, tbRadioButtonCheckedDisabled,
    tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedHot, tbCheckBoxUncheckedPressed, tbCheckBoxUncheckedDisabled,
    tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot, tbCheckBoxCheckedPressed, tbCheckBoxCheckedDisabled,
    tbCheckBoxMixedNormal, tbCheckBoxMixedHot, tbCheckBoxMixedPressed, tbCheckBoxMixedDisabled,
    tbCheckBoxImplicitNormal, tbCheckBoxImplicitHot, tbCheckBoxImplicitPressed, tbCheckBoxImplicitDisabled, // Windows Vista or later
    tbCheckBoxExcludedNormal, tbCheckBoxExcludedHot, tbCheckBoxExcludedPressed, tbCheckBoxExcludedDisabled, // Windows Vista or later
    tbGroupBoxNormal, tbGroupBoxDisabled,
    tbUserButton,
    tbCommandLinkNormal, tbCommandLinkHot, tbCommandLinkPressed, tbCommandLinkDisabled, tbCommandLinkDefaulted, tbCommandLinkDefaultedAnimating,
    tbCommandLinkGlyphNormal, tbCommandLinkGlyphHot, tbCommandLinkGlyphPressed, tbCommandLinkGlyphDisabled, tbCommandLinkGlyphDefaulted
  );

  TThemedCheckListBox = (
    tclCheckListBoxDontCare,
    tclCheckListBoxRoot,
    tclListItemNormal, tclListItemDisabled,
    tclHeaderItemNormal, tclHeaderItemDisabled
  );

  TElementColor = (
    ecBorderColor,
    ecFillColor,
    ecTextColor,
    ecEdgeLightColor,
    ecEdgeHighLightColor,
    ecEdgeShadowColor,
    ecEdgeDkShadowColor,
    ecEdgeFillColor,
    ecTransparentColor,
    ecGradientColor1,
    ecGradientColor2,
    ecGradientColor3,
    ecGradientColor4,
    ecGradientColor5,
    ecShadowColor,
    ecGlowColor,
    ecTextBorderColor,
    ecTextShadowColor,
    ecGlyphTextColor,
    ecGlyphTransparentColor,
    ecFillColorHint,
    ecBorderColorHint,
    ecAccentColorHint,
    ecTextColorHint,
    ecHeading1TextColor,
    ecHeading2TextColor,
    ecBodyTextColor
  );

  TCustomStyleEngine = class
    public
      class procedure RegisterStyleHook(ControlClass: TClass; StyleHookClass: TStyleHookClass);
      class procedure UnRegisterStyleHook(ControlClass: TClass; StyleHookClass: TStyleHookClass);
  end;

  TThemedElementDetails = TObject;

  TCustomStyleServices = class
  private
    public
      function DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect;
        ClipRect: PRect = nil; DPI: Integer = 0): Boolean;
      function Enabled: Boolean;
      function GetElementColor(Details: TThemedElementDetails; ElementColor: TElementColor; out Color: TColor): Boolean;
      function GetElementDetails(Detail: TThemedButton): TThemedElementDetails; overload;
      function GetElementDetails(Detail: TThemedCheckListBox): TThemedElementDetails; overload;
      function GetStyleColor(Font: TStyleColor): TColor;
      function GetStyleFontColor(Font: TStyleFont): TColor;
      function GetSystemColor(Color: TColor): TColor;
      function IsSystemStyle: Boolean;
  end;

  TSystemHook = (shMenus, shDialogs, shToolTips);
  TSystemHooks = set of TSystemHook;

  TStyleManager = class
    type TStyleServicesHandle = type Pointer;
    class var AutoDiscoverStyleResources: Boolean;
    class var SystemHooks: TSystemHooks;
    class procedure SetStyle(Handle: TStyleServicesHandle);
    class function TryLoadFromResource(Instance: HINST; const ResourceName: string;
      ResourceType: PChar; var Handle: TStyleServicesHandle): Boolean;
    class function TrySetStyle(const Name: string; ShowErrorDialog: Boolean = True): Boolean;
  end;

  { Override ComCtrls }
  TTabControlStyleHook = TStyleHook;
  TDateTimePickerStyleHook = TStyleHook;
  TTreeViewStyleHook = TStyleHook;
  TListViewStyleHook = TStyleHook;
  TProgressBarStyleHook = TStyleHook;
  TTrackBarStyleHook = TStyleHook;
  TStatusBarStyleHook = TStyleHook;
  TToolBarStyleHook = TStyleHook;
  TCoolBarStyleHook = TStyleHook;
  TUpDownStyleHook = TStyleHook;
  THeaderStyleHook = TStyleHook;
  TPageScrollerStyleHook = TStyleHook;
  TComboBoxExStyleHook = TStyleHook;
  TRichEditStyleHook = TStyleHook;

  { OVerride Forms }
  TScrollingStyleHook = TStyleHook;
  TFormStyleHook = TStyleHook;
  TScrollBoxStyleHook = TStyleHook;

function StyleServices(AControl: TControl = nil): TCustomStyleServices;

implementation

var
  CustomStyleServices: TCustomStyleServices;

function StyleServices(AControl: TControl = nil): TCustomStyleServices;
begin
  if CustomStyleServices = nil then
    CustomStyleServices := TCustomStyleServices.Create;
  Result := CustomStyleServices;
end;

{ TCustomStyleEngine }

class procedure TCustomStyleEngine.RegisterStyleHook(ControlClass: TClass;
  StyleHookClass: TStyleHookClass);
begin
end;

class procedure TCustomStyleEngine.UnRegisterStyleHook(ControlClass: TClass;
  StyleHookClass: TStyleHookClass);
begin
end;

{ TCustomStyleServices }

function TCustomStyleServices.DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect;
  ClipRect: PRect = nil; DPI: Integer = 0): Boolean;
begin
  Result := False;
end;

function TCustomStyleServices.Enabled: Boolean;
begin
  Result := False;
end;

function TCustomStyleServices.GetElementDetails(Detail: TThemedCheckListBox): TThemedElementDetails;
begin
  Result := nil;
end;

function TCustomStyleServices.GetElementColor(Details: TThemedElementDetails;
  ElementColor: TElementColor; out Color: TColor): Boolean;
begin
  Result := False;
end;

function TCustomStyleServices.GetElementDetails(Detail: TThemedButton): TThemedElementDetails;
begin
  Result := nil;
end;

function TCustomStyleServices.GetStyleColor(Font: TStyleColor): TColor;
begin
  Result := clNone;
end;

function TCustomStyleServices.GetStyleFontColor(Font: TStyleFont): TColor;
begin
  Result := clNone;
end;

function TCustomStyleServices.GetSystemColor(Color: TColor): TColor;
begin
  Result := clNone;
end;

function TCustomStyleServices.IsSystemStyle: Boolean;
begin
  Result := True;
end;

{ TStyleManager }

class procedure TStyleManager.SetStyle(Handle: TStyleServicesHandle);
begin
end;

class function TStyleManager.TryLoadFromResource(Instance: HINST; const ResourceName: string;
  ResourceType: PChar; var Handle: TStyleServicesHandle): Boolean;
begin
  Result := False;
end;

class function TStyleManager.TrySetStyle(const Name: string; ShowErrorDialog: Boolean): Boolean;
begin
  Result := False;
end;

initialization

finalization
  CustomStyleServices.Free;

end.
