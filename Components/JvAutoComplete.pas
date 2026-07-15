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
  { TEditListBoxAutoComplete implements an autocomplete code for a Edit/ListBox
    pair. After you have created an instance of this class you must call the
    AutoComplete method in a KeyPress event handler. }
  TJvEditListBoxAutoComplete = class(TComponent)
  private
    FOnDropDown: TNotifyEvent;
    FEditCtrl: TCustomEdit;
    FList: TStrings;
    FListBox: TCustomListBox;
    procedure SetEditCtrl(Value: TCustomEdit);
    procedure SetListBox(Value: TCustomListBox);
    function GetActive: Boolean;
    function GetText: TCaption;
    procedure SetText(const Value: TCaption);
    procedure GetEditSel(out StartPos, EndPos: Integer);
    procedure SetEditSel(StartPos, EndPos: Integer);
    procedure SetItemIndex(Index: Integer);
    function FindItemPrefix(const Prefix: string): Integer;
    function GetItemAt(Index: Integer): string;
    procedure DoDropDown;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AEditCtrl: TCustomEdit; AListBox: TCustomListBox); reintroduce;
    destructor Destroy; override;
    procedure AutoComplete(var Key: Char);
    property EditCtrl: TCustomEdit read FEditCtrl write SetEditCtrl;
    property ListBox: TCustomListBox read FListBox write SetListBox;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
  end;

implementation

uses
  StrUtils,
  JvInspectorSupport;

type
  TCustomEditAccess = class(TCustomEdit);

//=== { TJvEditListBoxAutoComplete } =========================================

constructor TJvEditListBoxAutoComplete.Create(AEditCtrl: TCustomEdit; AListBox: TCustomListBox);
begin
  inherited Create(nil);
  EditCtrl := AEditCtrl;
  ListBox := AListBox;
end;

destructor TJvEditListBoxAutoComplete.Destroy;
begin
  ListBox := nil;
  EditCtrl := nil;
  inherited Destroy;
end;

procedure TJvEditListBoxAutoComplete.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FEditCtrl) then
    FEditCtrl := nil;
  if (Operation = opRemove) and (AComponent = FListBox) then
  begin
    FListBox := nil;
    FList := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TJvEditListBoxAutoComplete.SetEditCtrl(Value: TCustomEdit);
begin
  ReplaceComponentReference(Self, Value, TComponent(FEditCtrl));
end;

procedure TJvEditListBoxAutoComplete.SetListBox(Value: TCustomListBox);
begin
  ReplaceComponentReference(Self, Value, TComponent(FListBox));

  if FListBox <> nil then
    FList := FListBox.Items
  else
    FList := nil;
end;

function TJvEditListBoxAutoComplete.GetActive: Boolean;
begin
  Result := (EditCtrl <> nil) and (FList <> nil) and
    not TCustomEditAccess(EditCtrl).ReadOnly;
end;

function TJvEditListBoxAutoComplete.GetText: TCaption;
begin
  Result := EditCtrl.Text;
end;

procedure TJvEditListBoxAutoComplete.SetText(const Value: TCaption);
begin
  EditCtrl.Text := Value;
end;

procedure TJvEditListBoxAutoComplete.GetEditSel(out StartPos, EndPos: Integer);
begin
  SendMessage(EditCtrl.Handle, EM_GETSEL, WPARAM(@StartPos), LPARAM(@EndPos));
end;

procedure TJvEditListBoxAutoComplete.SetEditSel(StartPos, EndPos: Integer);
begin
  EditCtrl.SelStart := StartPos;
  EditCtrl.SelLength := EndPos - StartPos;
end;

procedure TJvEditListBoxAutoComplete.SetItemIndex(Index: Integer);
begin
  ListBox.ItemIndex := Index;
end;

function TJvEditListBoxAutoComplete.FindItemPrefix(const Prefix: string): Integer;
begin
  if FList <> nil then
    for Result := 0 to FList.Count - 1 do
      if AnsiStartsText(Prefix, FList[Result]) then
        Exit;
  Result := -1;
end;

function TJvEditListBoxAutoComplete.GetItemAt(Index: Integer): string;
begin
  Result := FList[Index];
end;

procedure TJvEditListBoxAutoComplete.DoDropDown;
begin
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
end;

procedure TJvEditListBoxAutoComplete.AutoComplete(var Key: Char);
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
    Idx := FindItemPrefix(AnItem);
    if Idx < 0 then
      Exit;
    Result := True;
    SetItemIndex(Idx);
    SetText(AnItem + Copy(GetItemAt(Idx), Length(AnItem) + 1, MaxInt));
    SetEditSel(Length(AnItem), Length(GetText));
  end;

begin
  if not GetActive then
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

end.