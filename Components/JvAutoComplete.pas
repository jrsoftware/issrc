{ This is not the original file: it has been modified for Inno Setup. }

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAutoComplete.pas, released on 2004-09-04.

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausdaden att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

-----------------------------------------------------------------------------}

unit JvAutoComplete;

{$I jvcl.inc}

interface

uses
  Windows,
  Messages,
  SysUtils, Classes, Controls, StdCtrls;

type
  { TControlAutoComplete implements an autocomplete code for controls. It is an
    abstract base class. After you have created an instance of a derived class
    you must call the AutoComplete method in a KeyPress event handler.

    (ahuser) 2005-01-31: changed from TObject to TComponent due to Notification()
    Do not register this component it is more a "TObject" than a TComponent. }
  TJvControlAutoComplete = class(TComponent)
  private
    FOnDropDown: TNotifyEvent;
  protected
    function GetText: TCaption; virtual; abstract;
    procedure SetText(const Value: TCaption); virtual; abstract;
    procedure GetEditSel(out StartPos, EndPos: Integer); virtual; abstract;
    procedure SetEditSel(StartPos, EndPos: Integer); virtual; abstract;
    procedure SetItemIndex(Index: Integer); virtual; abstract;
    function FindItemPrefix(IndexStart: Integer; const Prefix: string): Integer; virtual; abstract;
    function GetItemAt(Index: Integer): string; virtual; abstract;

    function GetActive: Boolean; virtual;

    procedure DoDropDown; dynamic;
  public
    constructor Create; reintroduce;
    procedure AutoComplete(var Key: Char); virtual;

    property Active: Boolean read GetActive;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
  end;

  TJvBaseEditListAutoComplete = class(TJvControlAutoComplete)
  private
    FEditCtrl: TCustomEdit;
    FList: TStrings;
    procedure SetEditCtrl(Value: TCustomEdit);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetText: TCaption; override;
    procedure SetText(const Value: TCaption); override;
    procedure GetEditSel(out StartPos, EndPos: Integer); override;
    procedure SetEditSel(StartPos, EndPos: Integer); override;
    function FindItemPrefix(IndexStart: Integer; const Prefix: string): Integer; override;
    function GetItemAt(Index: Integer): string; override;
    function GetActive: Boolean; override;
    property List: TStrings read FList write FList;
  public
    constructor Create(AEditCtrl: TCustomEdit; AList: TStrings);
    destructor Destroy; override;
    property EditCtrl: TCustomEdit read FEditCtrl write SetEditCtrl;
  end;

  { TEditListBoxAutoComplete implements an autocomplete code for a Edit/ListBox
    pair. After you have created an instance of this class you must call the
    AutoComplete method in a KeyPress event handler. }
  TJvEditListBoxAutoComplete = class(TJvBaseEditListAutoComplete)
  private
    FListBox: TCustomListBox;
    procedure SetListBox(Value: TCustomListBox);
  protected
    procedure SetItemIndex(Index: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AEditCtrl: TCustomEdit; AListBox: TCustomListBox);
    destructor Destroy; override;
    property ListBox: TCustomListBox read FListBox write SetListBox;
  end;

implementation

uses
  StrUtils,
  JvInspectorSupport;

//=== { TJvControlAutoComplete } =============================================

constructor TJvControlAutoComplete.Create;
begin
  inherited Create(nil);
end;

function TJvControlAutoComplete.GetActive: Boolean;
begin
  Result := True;
end;

procedure TJvControlAutoComplete.DoDropDown;
begin
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
end;

procedure TJvControlAutoComplete.AutoComplete(var Key: Char);
var
  StartPos, EndPos: Integer;
  SaveText, OldText: TCaption;
  LastByte: Integer;
  Filter: string;

  function HasSelectedText(var StartPos, EndPos: Integer): Boolean;
  begin
    GetEditSel(StartPos, EndPos);
    Result := EndPos > StartPos;
  end;

  procedure DeleteSelectedText;
  var
    StartPos, EndPos: Integer;
    OldText: string;
  begin
    OldText := GetText;
    GetEditSel(StartPos, EndPos);
    Delete(OldText, StartPos + 1, EndPos - StartPos);
    SetItemIndex(-1);
    SetText(OldText);
    SetEditSel(StartPos, StartPos);
  end;

  function SelectItem(const AnItem: string): Boolean;
  var
    Idx: Integer;
  begin
    Result := False;
    if AnItem = '' then
    begin
      SetItemIndex(-1);
      Exit;
    end;
    Idx := FindItemPrefix(-1, AnItem);
    if Idx < 0 then
      Exit;
    Result := True;
    SetItemIndex(Idx);
    SetText(AnItem + Copy(GetItemAt(Idx), Length(AnItem) + 1, MaxInt));
    SetEditSel(Length(AnItem), Length(GetText));
  end;

begin
  if not Active then
    Exit;

  Filter := GetText;

  case Key of
    Esc {VK_ESCAPE}:
      Exit;
    BackSpace {VK_BACK}:
      begin
        if HasSelectedText(StartPos, EndPos) then
          DeleteSelectedText
        else
        if GetText <> '' then
        begin
          SaveText := GetText;
          LastByte := StartPos;
          while ByteType(SaveText, LastByte) = mbTrailByte do
            Dec(LastByte);
          OldText := Copy(SaveText, 1, LastByte - 1);
          SetItemIndex(-1);
          SetText(OldText + Copy(SaveText, EndPos + 1, MaxInt));
          SetEditSel(LastByte - 1, LastByte - 1);
        end;
        Key := #0;
      end;
  else
    DoDropDown;

    if HasSelectedText(StartPos, EndPos) then
      SaveText := Copy(Filter, 1, StartPos) + Key
    else
      SaveText := Filter + Key;

    if SelectItem(SaveText) then
      Key := #0;
  end;
end;

//=== { TJvBaseEditListAutoComplete } ========================================

constructor TJvBaseEditListAutoComplete.Create(AEditCtrl: TCustomEdit;
  AList: TStrings);
begin
  inherited Create;
  FList := AList;
  EditCtrl := AEditCtrl;
end;

destructor TJvBaseEditListAutoComplete.Destroy;
begin
  EditCtrl := nil;
  inherited Destroy;
end;

procedure TJvBaseEditListAutoComplete.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FEditCtrl) then
    FEditCtrl := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TJvBaseEditListAutoComplete.SetEditCtrl(Value: TCustomEdit);
begin
  ReplaceComponentReference(Self, Value, TComponent(FEditCtrl));
end;

type
  TCustomEditAccess = class(TCustomEdit);

function TJvBaseEditListAutoComplete.GetText: TCaption;
begin
  Result := EditCtrl.Text;
end;

procedure TJvBaseEditListAutoComplete.SetText(const Value: TCaption);
begin
  EditCtrl.Text := Value;
end;

procedure TJvBaseEditListAutoComplete.GetEditSel(out StartPos, EndPos: Integer);

begin
  SendMessage(EditCtrl.Handle, EM_GETSEL, WPARAM(@StartPos), LPARAM(@EndPos));
end;

procedure TJvBaseEditListAutoComplete.SetEditSel(StartPos, EndPos: Integer);
begin
  EditCtrl.SelStart := StartPos;
  EditCtrl.SelLength := EndPos - StartPos;
end;

function TJvBaseEditListAutoComplete.FindItemPrefix(IndexStart: Integer; const Prefix: string): Integer;
begin
  if List <> nil then
  begin
    for Result := IndexStart + 1 to List.Count - 1 do
      if AnsiStartsText(Prefix, List[Result]) then
        Exit;
    for Result := 0 to IndexStart do
      if AnsiStartsText(Prefix, List[Result]) then
        Exit;
  end;
  Result := -1;
end;

function TJvBaseEditListAutoComplete.GetItemAt(Index: Integer): string;
begin
  Result := List[Index];
end;


function TJvBaseEditListAutoComplete.GetActive: Boolean;
begin
  Result := inherited GetActive and (EditCtrl <> nil) and (List <> nil) and
    not TCustomEditAccess(EditCtrl).ReadOnly;
end;

//=== { TJvEditListBoxAutoComplete } =========================================

constructor TJvEditListBoxAutoComplete.Create(AEditCtrl: TCustomEdit; AListBox: TCustomListBox);
begin
  if AListBox = nil then
    inherited Create(AEditCtrl, nil)
  else
    inherited Create(AEditCtrl, AListBox.Items);
  ListBox := AListBox;
end;

destructor TJvEditListBoxAutoComplete.Destroy;
begin
  ListBox := nil;
  inherited Destroy;
end;

procedure TJvEditListBoxAutoComplete.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FListBox) then
  begin
    FListBox := nil;
    List := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TJvEditListBoxAutoComplete.SetListBox(Value: TCustomListBox);
begin
  ReplaceComponentReference(Self, Value, TComponent(FListBox));

  if FListBox <> nil then
    List := FListBox.Items
  else
    List := nil;
end;

procedure TJvEditListBoxAutoComplete.SetItemIndex(Index: Integer);
begin
  ListBox.ItemIndex := Index;
end;

end.