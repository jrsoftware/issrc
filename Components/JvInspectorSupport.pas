unit JvInspectorSupport;

{-----------------------------------------------------------------------------

 Support unit for the extracted JvInspector included with Inno Setup.

 The contents of this file are subject to the Mozilla Public License Version
 1.1 (the "License"); you may not use this file except in compliance with the
 License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS IS" basis,
 WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 the specific language governing rights and limitations under the License.

 This unit provides the small subset of JVCL machinery consumed by
 JvInspector.pas and JvAutoComplete.pas, rewritten against or copied from the
 Delphi RTL/VCL so that those units compile without JVCL. Declarations
 keep their original JVCL names so the ported units only need their uses
 clauses changed.

 Portions are derived from JVCL (github.com/project-jedi/jvcl, commit
 b045b99e132c325a25b28769ae5db1b81b2234ef): JvTypes.pas, JvConsts.pas,
 JvComponentBase.pas, JvComponent.pas, JvExControls.pas, JvJCLUtils.pas,
 JvJVCLUtils.pas, JvThemes.pas and JvResources.pas, all
 Copyright (c) the Project JEDI contributors named in those files.

-----------------------------------------------------------------------------}

{$I jvcl.inc}

interface

uses
  Winapi.Windows,
  System.SysUtils, System.Classes, System.TypInfo,
  Vcl.Graphics, Vcl.Themes;

//=== Replacements for JvTypes.pas, JvComponentBase.pas and JvConsts.pas =====

type
  EJVCLException = class(Exception);
  TJvComponent = TComponent;

const
  BackSpace = #8;
  Esc = #27;

const
  tkStrings: set of TTypeKind = [tkString, tkLString, tkUString, tkWString];

//=== Replacements for JvJCLUtils.pas (subset) ===============================

function RectWidth(R: TRect): Integer;
function RectHeight(R: TRect): Integer;

//=== Replacements for JvJVCLUtils.pas (subset) ==============================

function CanvasMaxTextHeight(Canvas: TCanvas): Integer;
procedure ReplaceComponentReference(This, NewReference: TComponent;
  var VarReference: TComponent);

//=== Replacements for JvThemes.pas (subset) =================================

function DrawThemedFrameControl(DC: HDC; const Rect: TRect;
  uType, uState: UINT; DPI: Integer = 0): BOOL;

//=== Replacements for JvResources.pas (subset) ==============================

resourcestring
  RsJvInspItemUnInitialized = '(uninitialized)';
  RsJvInspItemValueException = 'Exception ';
  RsEJvInspDataNoAccessAs = 'Data cannot be accessed as %s';
  RsEJvInspDataNotInit = 'Data not initialized';

implementation

//=== Replacements for JvJCLUtils.pas (subset) ===============================

function RectWidth(R: TRect): Integer;
begin
  Result := Abs(R.Right - R.Left);
end;

function RectHeight(R: TRect): Integer;
begin
  Result := Abs(R.Bottom - R.Top);
end;

//=== Replacements for JvJVCLUtils.pas (subset) ==============================

function CanvasMaxTextHeight(Canvas: TCanvas): Integer;
var
  tt: TTextMetric;
begin
  GetTextMetrics(Canvas.Handle, tt);
  Result := tt.tmHeight;
end;

procedure ReplaceComponentReference(This, NewReference: TComponent;
  var VarReference: TComponent);
begin
  if (VarReference <> NewReference) and Assigned(This) then
  begin
    if Assigned(VarReference) then
      VarReference.RemoveFreeNotification(This);
    VarReference := NewReference;
    if Assigned(VarReference) then
      VarReference.FreeNotification(This);
  end;
end;

//=== Replacements for JvThemes.pas (subset) =================================

{ With VCL Styles active, DrawElement changes the DC's selected objects
  and colors, and does not restore them }
procedure DrawElementPreservingDCState(DC: HDC;
  const Details: TThemedElementDetails; const R: TRect; DPI: Integer = 0);
var
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(DC);
  try
    StyleServices.DrawElement(DC, Details, R, nil, DPI);
  finally
    RestoreDC(DC, SaveIndex);
  end;
end;

{ Only the frame control types and states JvInspector.pas draws are themed:
  the combo box drop-down button and the check box, both following upstream
  JvThemes.pas. Everything else falls back to DrawFrameControl }
function DrawThemedFrameControl(DC: HDC; const Rect: TRect;
  uType, uState: UINT; DPI: Integer): BOOL;
const
  Mask = $00FF;
var
  Btn: TThemedButton;
  ComboBox: TThemedComboBox;
  R: TRect;
begin
  Result := False;
  if StyleServices.Enabled and (uType = DFC_SCROLL) and
     ((uState and Mask) = DFCS_SCROLLCOMBOBOX) then
  begin
    R := Rect;
    if uState and DFCS_INACTIVE <> 0 then
      ComboBox := tcDropDownButtonDisabled
    else
    if uState and DFCS_PUSHED <> 0 then
      ComboBox := tcDropDownButtonPressed
    else
    if uState and DFCS_HOT <> 0 then
      ComboBox := tcDropDownButtonHot
    else
      ComboBox := tcDropDownButtonNormal;

    DrawElementPreservingDCState(DC, StyleServices.GetElementDetails(ComboBox), R, DPI);
    Result := True;
  end
  else
  if StyleServices.Enabled and (uType = DFC_BUTTON) and
     ((uState and Mask) = DFCS_BUTTONCHECK) then
  begin
    R := Rect;
    if uState and DFCS_CHECKED <> 0 then
    begin
      if uState and DFCS_INACTIVE <> 0 then
        Btn := tbCheckBoxCheckedDisabled
      else
      if uState and DFCS_PUSHED <> 0 then
        Btn := tbCheckBoxCheckedPressed
      else
      if uState and DFCS_HOT <> 0 then
        Btn := tbCheckBoxCheckedHot
      else
        Btn := tbCheckBoxCheckedNormal;
    end
    else
    begin
      if uState and DFCS_INACTIVE <> 0 then
        Btn := tbCheckBoxUncheckedDisabled
      else
      if uState and DFCS_PUSHED <> 0 then
        Btn := tbCheckBoxUncheckedPressed
      else
      if uState and DFCS_HOT <> 0 then
        Btn := tbCheckBoxUncheckedHot
      else
        Btn := tbCheckBoxUncheckedNormal;
    end;

    DrawElementPreservingDCState(DC, StyleServices.GetElementDetails(Btn), R, DPI);
    Result := True;
  end;

  if not Result then
    Result := DrawFrameControl(DC, Rect, uType, uState);
end;

end.
