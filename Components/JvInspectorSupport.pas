unit JvInspectorSupport;

{-----------------------------------------------------------------------------

 Support unit for the extracted JvInspector included with Inno Setup.

 The contents of this file are subject to the Mozilla Public License Version
 1.1 (the "License"); you may not use this file except in compliance with the
 License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS IS" basis,
 WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 the specific language governing rights and limitations under the License.

 This unit provides the small subset of JVCL and JCL machinery consumed by
 JvInspector.pas and JvAutoComplete.pas, rewritten against or copied from the
 Delphi RTL/VCL so that those units compile without JVCL or JCL. Declarations
 keep their original JVCL/JCL names so the ported units only need their uses
 clauses changed.

 Portions are derived from JVCL (github.com/project-jedi/jvcl, commit
 b045b99e132c325a25b28769ae5db1b81b2234ef): JvTypes.pas, JvConsts.pas,
 JvComponentBase.pas, JvComponent.pas, JvExControls.pas, JvJCLUtils.pas,
 JvJVCLUtils.pas, JvThemes.pas and JvResources.pas, all
 Copyright (c) the Project JEDI contributors named in those files.
 Portions are derived from JCL (github.com/project-jedi/jcl, commit
 7332634269062e46e2f6cf0f4b2a712bf8dc39e6): JclRTTI.pas, JclLogic.pas and
 JclSysUtils.pas, Copyright (c) the Project JEDI contributors named in those
 files.

-----------------------------------------------------------------------------}

{$I jvcl.inc}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.TypInfo,
  Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.Themes;

//=== Replacements for JvTypes.pas, JvComponentBase.pas and JvConsts.pas =====

type
  EJVCLException = class(Exception);
  TJvComponent = TComponent;

const
  BackSpace = #8;
  Tab = #9;
  Esc = #27;

const
  tkStrings: set of TTypeKind = [tkString, tkLString, tkUString, tkWString];

//=== Replacements for JvExControls.pas and JvComponent.pas ==================

type
  TDlgCode =
   (dcWantAllKeys, dcWantArrows, dcWantChars, dcButton, dcHasSetSel, dcWantTab,
    dcNative); // if dcNative is in the set the native allowed keys are used and GetDlgCode is ignored
  TDlgCodes = set of TDlgCode;

const
  dcWantMessage = dcWantAllKeys;

type
  { Minimal stand-in for JVCL's TJvExCustomControl (JvExControls.pas): only
    the virtual hooks that JvInspector.pas actually overrides are provided,
    wired to the corresponding window messages the same way JvExControls
    dispatches them }
  TJvCustomControl = class(TCustomControl)
  private
    function BaseWndProc(Msg: Cardinal; WParam: WPARAM = 0; LParam: LPARAM = 0): LRESULT;
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure BoundsChanged; virtual;
    procedure GetDlgCode(var Code: TDlgCodes); virtual;
    procedure FocusSet(PrevWnd: THandle); virtual;
    procedure FocusKilled(NextWnd: THandle); virtual;
  end;

  { From JvComponent.pas, rebased from TJvExCustomListBox onto TCustomListBox }
  TJvPopupListBox = class(TCustomListBox)
  private
    FSearchText: string;
    FSearchTickCount: Int64;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
  end;

function DlgcToDlgCodes(Value: LPARAM): TDlgCodes;
function DlgCodesToDlgc(const Value: TDlgCodes): LPARAM;

//=== Replacements for JclRTTI.pas (subset) ==================================

type
  IJclTypeInfo = interface
    ['{7DAD5220-46EA-11D5-B0C0-4854E825F345}']
    function GetName: string;
    function GetTypeData: PTypeData;
    function GetTypeInfo: PTypeInfo;
    function GetTypeKind: TTypeKind;

    property Name: string read GetName;
    property TypeData: PTypeData read GetTypeData;
    property TypeInfo: PTypeInfo read GetTypeInfo;
    property TypeKind: TTypeKind read GetTypeKind;
  end;

  IJclOrdinalTypeInfo = interface(IJclTypeInfo)
    ['{7DAD5221-46EA-11D5-B0C0-4854E825F345}']
    function GetOrdinalType: TOrdType;

    property OrdinalType: TOrdType read GetOrdinalType;
  end;

  IJclOrdinalRangeTypeInfo = interface(IJclOrdinalTypeInfo)
    ['{7DAD5222-46EA-11D5-B0C0-4854E825F345}']
    function GetMinValue: Int64;
    function GetMaxValue: Int64;

    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

  IJclEnumerationTypeInfo = interface(IJclOrdinalRangeTypeInfo)
    ['{7DAD5223-46EA-11D5-B0C0-4854E825F345}']
    function GetNames(const I: Integer): string;

    property Names[const I: Integer]: string read GetNames; default;
  end;

  IJclSetTypeInfo = interface(IJclOrdinalTypeInfo)
    ['{7DAD5224-46EA-11D5-B0C0-4854E825F345}']
    function GetBaseType: IJclOrdinalTypeInfo;

    property BaseType: IJclOrdinalTypeInfo read GetBaseType;
  end;

function JclTypeInfo(ATypeInfo: PTypeInfo): IJclTypeInfo;

function JclStrToTypedInt(Value: string; TypeInfo: PTypeInfo): Integer;
function JclTypedIntToStr(Value: Integer; TypeInfo: PTypeInfo): string;

function JclSetToStr(TypeInfo: PTypeInfo; const Value;
  const WantBrackets: Boolean = False; const WantRanges: Boolean = False): string;
procedure JclStrToSet(TypeInfo: PTypeInfo; var SetVar; const Value: string);

//=== Replacements for JclLogic.pas (subset) =================================

procedure ClearBitBuffer(var Value; const Bit: Cardinal);
procedure SetBitBuffer(var Value; const Bit: Cardinal);
function TestBitBuffer(const Value; const Bit: Cardinal): Boolean;

//=== Replacements for JclStrings.pas (subset) ===============================

procedure StrReplace(var S: string; const Search, Replace: string;
  const Flags: TReplaceFlags);

//=== Replacements for JclSysUtils.pas (subset) ==============================

{ Upstream's global TJclFormatSettings variable mirrors the RTL's global
  format settings, which the consumers only read }
function JclFormatSettings: TFormatSettings;

//=== Replacements for JvJCLUtils.pas (subset) ===============================

function RectWidth(R: TRect): Integer;
function RectHeight(R: TRect): Integer;
function DrawText(Canvas: TCanvas; const Text: string; Len: Integer;
  var R: TRect; WinFlags: Integer): Integer;
function DrawTextEx(Canvas: TCanvas; const Text: string; cchText: Integer;
  var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer;

//=== Replacements for JvJVCLUtils.pas (subset) ==============================

function CanvasMaxTextHeight(Canvas: TCanvas): Integer;
function ReplaceComponentReference(This, NewReference: TComponent;
  var VarReference: TComponent): Boolean;

//=== Replacements for JvThemes.pas (subset) =================================

function DrawThemedFrameControl(DC: HDC; const Rect: TRect;
  uType, uState: UINT; DPI: Integer = 0): BOOL;
function DrawThemedButtonFace(Control: TControl; Canvas: TCanvas;
  const Client: TRect; BevelWidth: Integer; Style: TButtonStyle;
  IsRounded, IsDown, IsFocused, IsHot: Boolean): TRect;

//=== Replacements for JvResources.pas (subset) ==============================

resourcestring
  RsEInspectorInternalError = 'Internal error: two data instances pointing to the same data are registered';
  RsEJvInspPaintOnlyUsedOnce = 'Inspector painter can only be linked to one inspector';
  RsEJvAssertSetTopIndex = 'TJvCustomInspector.SetTopIndex: unexpected MaxIdx <= -1';
  RsEJvInspPaintNotActive = 'Painter is not the active painter of the specified inspector';
  RsEJvInspItemHasParent = 'Item already assigned to another parent';
  RsJvInspItemUnInitialized = '(uninitialized)';
  RsJvInspItemNoValue = '(no value)';
  RsJvInspItemUnassigned = '(unassigned)';
  RsJvInspItemValueException = 'Exception ';
  RsEJvInspItemNotAChild = 'Specified Item is not a child of this item';
  RsEJvInspItemColNotFound = 'Specified column does not belong to this compound item';
  RsEJvInspItemItemIsNotCol = 'Specified item is not a column of this compound item';
  RsEJvAssertInspectorPainter = 'TJvInspectorCustomCompoundItem.DivideRect: unexpected Inspector.Painter = nil';
  RsEJvInspItemInvalidPropValue = 'Invalid property value %s';
  RsEJvInspDataNoAccessAs = 'Data cannot be accessed as %s';
  RsEJvAssertDataParent = 'TJvInspectorSetMemberData.New: unexpected ADataParent = nil';
  RsEJvAssertParent = 'TJvInspectorSetMemberData.New: unexpected AParent = nil';
  RsESpecifierBeforeSeparator = 'A specifier should be placed before and after a separator';
  RsEDOrDDOnlyOnce = '''d'' or ''dd'' should appear only once';
  RsEOnlyDOrDDAllowed = 'Only ''d'' or ''dd'' are allowed';
  RsEMOrMMOnlyOnce = '''m'' or ''mm'' should appear only once';
  RsEOnlyMOrMMAllowed = 'Only ''m'' or ''mm'' are allowed';
  RsEYYOrYYYYOnlyOnce = '''yy'' or ''yyyy'' should appear only once';
  RsEOnlyYYOrYYYYAllowed = 'Only ''yy'' or ''yyyy'' are allowed';
  RsEOnlyTwoSeparators = 'Only two separators are allowed';
  RsEOnlyDMYSAllowed = 'Only ''d'', ''m'', ''y'' and ''%s'' are allowed';
  RsEDOrDDRequired = '''d'' or ''dd'' are required';
  RsEMOrMMRequired = '''m'' or ''mm'' are required';
  RsEYYOrYYYYRequired = '''yy'' or ''yyyy'' are required';
  RsStringListEditorCaption = 'String list editor';
  RsButtonOKCaption = '&OK';
  RsButtonCancelCaption = 'Cancel';
  RsXLinesCaption = ' lines';
  RsOneLineCaption = '1 line';
  RsEInstanceAlreadyExists = 'Instance already exists with another name';
  RsENameAlreadyExistsForInstance = 'Name already exists for another instance';
  RsEInstanceNonexistent = 'Instance does not exist';
  RsEMethodAlreadyExists = 'Method already exists with another name';
  RsENameAlreadyExistsForMethod = 'Name already exists for another method';
  RsENamedInstanceNonexistent = 'Instance named ''%s'' does not exist';
  RsEMethodNonexistent = 'Method does not exist';
  RsENamedMethodNonexistent = 'Method named ''%s'' does not exist';
  RsENotSeparately = '%s cannot be created separately';
  RsEJvInspDataNotInit = 'Data not initialized';
  RsEJvInspDataNotAssigned = 'Data not assigned';
  RsEJvInspDataNoValue = 'Data has no value';
  RsENoNewInstance = '%s does not allow a new instance to be created';
  RsEJvInspDataStrTooLong = 'String too long';
  RsEJvAssertPropInfo = 'TJvInspectorPropData.New: unexpected PropInfo = nil';
  RsEJvAssertClassInfo = 'TJvInspectorPropData.New: unexpected ClassInfo = nil';
  RsEJvAssertINIFile = 'TJvInspectorINIFileData.New: unexpected AINIFile = nil';
  RsEJvInspNoGenReg = 'Unable to create generic item registration list';

  { From JclResources.pas, used by JclStrToTypedInt }
  RsRTTIValueOutOfRange = 'Value %s out of range %s..%s.';

implementation

//=== Replacements for JvExControls.pas and JvComponent.pas ==================

function DlgcToDlgCodes(Value: LPARAM): TDlgCodes;
begin
  Result := [];
  if (Value and DLGC_WANTARROWS) <> 0 then
    Include(Result, dcWantArrows);
  if (Value and DLGC_WANTTAB) <> 0 then
    Include(Result, dcWantTab);
  if (Value and DLGC_WANTALLKEYS) <> 0 then
    Include(Result, dcWantAllKeys);
  if (Value and DLGC_WANTCHARS) <> 0 then
    Include(Result, dcWantChars);
  if (Value and DLGC_BUTTON) <> 0 then
    Include(Result, dcButton);
  if (Value and DLGC_HASSETSEL) <> 0 then
    Include(Result, dcHasSetSel);
end;

function DlgCodesToDlgc(const Value: TDlgCodes): LPARAM;
begin
  Result := 0;
  if dcWantAllKeys in Value then
    Result := Result or DLGC_WANTALLKEYS;
  if dcWantArrows in Value then
    Result := Result or DLGC_WANTARROWS;
  if dcWantTab in Value then
    Result := Result or DLGC_WANTTAB;
  if dcWantChars in Value then
    Result := Result or DLGC_WANTCHARS;
  if dcButton in Value then
    Result := Result or DLGC_BUTTON;
  if dcHasSetSel in Value then
    Result := Result or DLGC_HASSETSEL;
end;

//=== { TJvCustomControl } ===================================================

function TJvCustomControl.BaseWndProc(Msg: Cardinal; WParam: WPARAM;
  LParam: LPARAM): LRESULT;
var
  Mesg: TMessage;
begin
  Mesg.Msg := Msg;
  Mesg.WParam := WParam;
  Mesg.LParam := LParam;
  Mesg.Result := 0;
  inherited WndProc(Mesg);
  Result := Mesg.Result;
end;

procedure TJvCustomControl.WndProc(var Msg: TMessage);
var
  DlgCodes: TDlgCodes;
begin
  case Msg.Msg of
    WM_SETFOCUS:
      FocusSet(THandle(Msg.WParam));
    WM_KILLFOCUS:
      FocusKilled(THandle(Msg.WParam));
    WM_SIZE, WM_MOVE:
      begin
        inherited WndProc(Msg);
        BoundsChanged;
      end;
    WM_GETDLGCODE:
      begin
        inherited WndProc(Msg);
        DlgCodes := [dcNative] + DlgcToDlgCodes(Msg.Result);
        GetDlgCode(DlgCodes);
        if not (dcNative in DlgCodes) then
          Msg.Result := DlgCodesToDlgc(DlgCodes);
      end;
  else
    inherited WndProc(Msg);
  end;
end;

procedure TJvCustomControl.BoundsChanged;
begin
end;

procedure TJvCustomControl.GetDlgCode(var Code: TDlgCodes);
begin
end;

procedure TJvCustomControl.FocusSet(PrevWnd: THandle);
begin
  BaseWndProc(WM_SETFOCUS, WPARAM(PrevWnd), 0);
end;

procedure TJvCustomControl.FocusKilled(NextWnd: THandle);
begin
  BaseWndProc(WM_KILLFOCUS, WPARAM(NextWnd), 0);
end;

//=== { TJvPopupListBox } ====================================================

procedure TJvPopupListBox.CreateParams(var Params: TCreateParams);
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

procedure TJvPopupListBox.CreateWnd;
begin
  inherited CreateWnd;
  Winapi.Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
end;

procedure TJvPopupListBox.KeyPress(var Key: Char);
var
  TickCount: Int64;
begin
  case Key of
    BackSpace, Esc:
      FSearchText := '';
    #32..High(Char):
      begin
        TickCount := GetTickCount;
        if TickCount < FSearchTickCount then
          Inc(TickCount, $100000000); // (ahuser) reduces the overflow
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

//=== Replacements for JclRTTI.pas (subset) ==================================

type
  TJclTypeInfo = class(TInterfacedObject, IJclTypeInfo)
  private
    FTypeInfo: PTypeInfo;
  protected
    function GetName: string;
    function GetTypeData: PTypeData;
    function GetTypeInfo: PTypeInfo;
    function GetTypeKind: TTypeKind;
  public
    constructor Create(ATypeInfo: PTypeInfo);
  end;

  TJclOrdinalTypeInfo = class(TJclTypeInfo, IJclOrdinalTypeInfo)
  protected
    function GetOrdinalType: TOrdType;
  end;

  TJclOrdinalRangeTypeInfo = class(TJclOrdinalTypeInfo, IJclOrdinalRangeTypeInfo)
  protected
    function GetMinValue: Int64;
    function GetMaxValue: Int64;
  end;

  TJclEnumerationTypeInfo = class(TJclOrdinalRangeTypeInfo, IJclEnumerationTypeInfo)
  protected
    function GetNames(const I: Integer): string;
  end;

  TJclSetTypeInfo = class(TJclOrdinalTypeInfo, IJclSetTypeInfo)
  protected
    function GetBaseType: IJclOrdinalTypeInfo;
  end;

constructor TJclTypeInfo.Create(ATypeInfo: PTypeInfo);
begin
  inherited Create;
  FTypeInfo := ATypeInfo;
end;

function TJclTypeInfo.GetName: string;
begin
  Result := string(FTypeInfo.Name);
end;

function TJclTypeInfo.GetTypeData: PTypeData;
begin
  Result := System.TypInfo.GetTypeData(FTypeInfo);
end;

function TJclTypeInfo.GetTypeInfo: PTypeInfo;
begin
  Result := FTypeInfo;
end;

function TJclTypeInfo.GetTypeKind: TTypeKind;
begin
  Result := FTypeInfo.Kind;
end;

function TJclOrdinalTypeInfo.GetOrdinalType: TOrdType;
begin
  Result := GetTypeData.OrdType;
end;

function TJclOrdinalRangeTypeInfo.GetMinValue: Int64;
begin
  if GetOrdinalType = otULong then
    Result := Cardinal(GetTypeData.MinValue)
  else
    Result := GetTypeData.MinValue;
end;

function TJclOrdinalRangeTypeInfo.GetMaxValue: Int64;
begin
  if GetOrdinalType = otULong then
    Result := Cardinal(GetTypeData.MaxValue)
  else
    Result := GetTypeData.MaxValue;
end;

function TJclEnumerationTypeInfo.GetNames(const I: Integer): string;
begin
  Result := GetEnumName(FTypeInfo, I);
end;

function TJclSetTypeInfo.GetBaseType: IJclOrdinalTypeInfo;
begin
  Result := JclTypeInfo(GetTypeData.CompType^) as IJclOrdinalTypeInfo;
end;

function JclTypeInfo(ATypeInfo: PTypeInfo): IJclTypeInfo;
begin
  case ATypeInfo.Kind of
    tkInteger, tkChar, tkWChar:
      Result := TJclOrdinalRangeTypeInfo.Create(ATypeInfo);
    tkEnumeration:
      Result := TJclEnumerationTypeInfo.Create(ATypeInfo);
    tkSet:
      Result := TJclSetTypeInfo.Create(ATypeInfo);
  else
    Result := TJclTypeInfo.Create(ATypeInfo);
  end;
end;

function JclStrToTypedInt(Value: string; TypeInfo: PTypeInfo): Integer;
var
  Conv: TIdentToInt;
  HaveConversion: Boolean;
  RangeInfo: IJclOrdinalRangeTypeInfo;
  TmpVal: Int64;
begin
  if TypeInfo <> nil then
    Conv := FindIdentToInt(TypeInfo)
  else
    Conv := nil;
  Result := 0;
  HaveConversion := Assigned(Conv) and Conv(Value, Result);
  if not HaveConversion then
  begin
    if TypeInfo <> nil then
    begin
      if JclTypeInfo(TypeInfo).QueryInterface(IJclOrdinalRangeTypeInfo, RangeInfo) <> S_OK then
        RangeInfo := nil;
      TmpVal := StrToInt64(Value);
      if (RangeInfo <> nil) and ((TmpVal < RangeInfo.MinValue) or
          (TmpVal > RangeInfo.MaxValue)) then
        raise EConvertError.CreateResFmt(@RsRTTIValueOutOfRange,
          [Value, IntToStr(RangeInfo.MinValue), IntToStr(RangeInfo.MaxValue)]);
      Result := Integer(TmpVal);
    end
    else
      Result := StrToInt(Value);
  end;
end;

function JclTypedIntToStr(Value: Integer; TypeInfo: PTypeInfo): string;
var
  Conv: TIntToIdent;
  HaveConversion: Boolean;
begin
  if TypeInfo <> nil then
    Conv := FindIntToIdent(TypeInfo)
  else
    Conv := nil;
  Result := '';
  HaveConversion := Assigned(Conv) and Conv(Value, Result);
  if not HaveConversion then
  begin
    if (TypeInfo <> nil) and (GetTypeData(TypeInfo).OrdType = otULong) then
      Result := IntToStr(Int64(Cardinal(Value)))
    else
      Result := IntToStr(Value);
  end;
end;

{ Returns the base (comp) type of a set type together with its ordinal range.
  The layout of a Delphi set variable starts at byte MinValue div 8, so bit I
  of the set value lives at buffer bit I - 8 * (MinValue div 8) }
procedure GetSetBaseRange(TypeInfo: PTypeInfo; out BaseInfo: PTypeInfo;
  out MinValue, MaxValue: Integer);
var
  RangeInfo: IJclOrdinalRangeTypeInfo;
begin
  if TypeInfo.Kind <> tkSet then
    raise EConvertError.CreateFmt('%s is not a set type', [string(TypeInfo.Name)]);
  BaseInfo := GetTypeData(TypeInfo).CompType^;
  RangeInfo := JclTypeInfo(BaseInfo) as IJclOrdinalRangeTypeInfo;
  MinValue := Integer(RangeInfo.MinValue);
  MaxValue := Integer(RangeInfo.MaxValue);
end;

function JclSetToStr(TypeInfo: PTypeInfo; const Value;
  const WantBrackets: Boolean; const WantRanges: Boolean): string;
var
  BaseInfo: PTypeInfo;
  MinValue, MaxValue: Integer;
  I: Integer;
  Name: string;
begin
  GetSetBaseRange(TypeInfo, BaseInfo, MinValue, MaxValue);
  Result := '';
  for I := MinValue to MaxValue do
  begin
    if TestBitBuffer(Value, Cardinal(I - 8 * (MinValue div 8))) then
    begin
      if BaseInfo.Kind = tkEnumeration then
        Name := GetEnumName(BaseInfo, I)
      else
        Name := IntToStr(I);
      if Result <> '' then
        Result := Result + ', ' + Name
      else
        Result := Name;
    end;
  end;
  if WantBrackets then
    Result := '[' + Result + ']';
end;

procedure JclStrToSet(TypeInfo: PTypeInfo; var SetVar; const Value: string);
var
  BaseInfo: PTypeInfo;
  MinValue, MaxValue: Integer;
  S: TStringList;
  I: Integer;
  Name: string;
  OrdVal: Integer;
begin
  GetSetBaseRange(TypeInfo, BaseInfo, MinValue, MaxValue);
  FillChar(SetVar, (MaxValue div 8) - (MinValue div 8) + 1, 0);
  S := TStringList.Create;
  try
    S.CommaText := Value;
    for I := 0 to S.Count - 1 do
    begin
      Name := Trim(S[I]);
      if (Name <> '') and (Name[1] = '[') then
        Delete(Name, 1, 1);
      if (Name <> '') and (Name[Length(Name)] = ']') then
        Delete(Name, Length(Name), 1);
      Name := Trim(Name);
      if Name = '' then
        Continue;
      if BaseInfo.Kind = tkEnumeration then
      begin
        OrdVal := GetEnumValue(BaseInfo, Name);
        if OrdVal < 0 then
          raise EConvertError.CreateResFmt(@RsEJvInspItemInvalidPropValue, [Name]);
      end
      else
        OrdVal := StrToInt(Name);
      if (OrdVal < MinValue) or (OrdVal > MaxValue) then
        raise EConvertError.CreateResFmt(@RsRTTIValueOutOfRange,
          [Name, IntToStr(MinValue), IntToStr(MaxValue)]);
      SetBitBuffer(SetVar, Cardinal(OrdVal - 8 * (MinValue div 8)));
    end;
  finally
    S.Free;
  end;
end;

//=== Replacements for JclLogic.pas (subset) =================================

procedure ClearBitBuffer(var Value; const Bit: Cardinal);
var
  P: PByte;
begin
  P := Addr(Value);
  Inc(P, Bit div 8);
  P^ := Byte(P^ and not (1 shl (Bit mod 8)));
end;

procedure SetBitBuffer(var Value; const Bit: Cardinal);
var
  P: PByte;
begin
  P := Addr(Value);
  Inc(P, Bit div 8);
  P^ := Byte(P^ or (1 shl (Bit mod 8)));
end;

function TestBitBuffer(const Value; const Bit: Cardinal): Boolean;
var
  P: PByte;
begin
  P := Addr(Value);
  Inc(P, Bit div 8);
  Result := (P^ and (1 shl (Bit mod 8))) <> 0;
end;

//=== Replacements for JclStrings.pas (subset) ===============================

procedure StrReplace(var S: string; const Search, Replace: string;
  const Flags: TReplaceFlags);
begin
  S := StringReplace(S, Search, Replace, Flags);
end;

//=== Replacements for JclSysUtils.pas (subset) ==============================

function JclFormatSettings: TFormatSettings;
begin
  Result := FormatSettings;
end;

//=== Replacements for JvJCLUtils.pas (subset) ===============================

function RectWidth(R: TRect): Integer;
begin
  Result := Abs(R.Right - R.Left);
end;

function RectHeight(R: TRect): Integer;
begin
  Result := Abs(R.Bottom - R.Top);
end;

function DrawText(Canvas: TCanvas; const Text: string; Len: Integer;
  var R: TRect; WinFlags: Integer): Integer;
begin
  // make sure the string cannot be modified
  Result := Winapi.Windows.DrawText(Canvas.Handle, PChar(Text), Len, R,
    UINT(WinFlags and not DT_MODIFYSTRING));
end;

function DrawTextEx(Canvas: TCanvas; const Text: string; cchText: Integer;
  var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer;
begin
  Result := Winapi.Windows.DrawTextEx(Canvas.Handle, PChar(Text), cchText, p4,
    dwDTFormat and not DT_MODIFYSTRING, DTParams);
end;

//=== Replacements for JvJVCLUtils.pas (subset) ==============================

function CanvasMaxTextHeight(Canvas: TCanvas): Integer;
var
  tt: TTextMetric;
begin
  GetTextMetrics(Canvas.Handle, tt);
  Result := tt.tmHeight;
end;

function ReplaceComponentReference(This, NewReference: TComponent;
  var VarReference: TComponent): Boolean;
begin
  Result := (VarReference <> NewReference) and Assigned(This);
  if Result then
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

function DrawThemedButtonFace(Control: TControl; Canvas: TCanvas;
  const Client: TRect; BevelWidth: Integer; Style: TButtonStyle;
  IsRounded, IsDown, IsFocused, IsHot: Boolean): TRect;
var
  Btn: TThemedButton;
  Details: TThemedElementDetails;
begin
  if (Style <> bsWin31) and StyleServices.Enabled then
  begin
    Result := Client;
    if IsDown then
      Btn := tbPushButtonPressed
    else
    if IsFocused then
      Btn := tbPushButtonDefaulted
    else
    if IsHot then
      Btn := tbPushButtonHot
    else
      Btn := tbPushButtonNormal;
    Details := StyleServices.GetElementDetails(Btn);
    DrawElementPreservingDCState(Canvas.Handle, Details, Result);
    StyleServices.GetElementContentRect(Canvas.Handle, Details, Client, Result);
    if IsFocused then
      DrawFocusRect(Canvas.Handle, Result);
    InflateRect(Result, -BevelWidth, -BevelWidth);
  end
  else
    Result := DrawButtonFace(Canvas, Client, BevelWidth, Style, IsRounded,
      IsDown, IsFocused);
end;

end.
