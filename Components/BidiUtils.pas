unit BidiUtils;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Bidi utility functions
}

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

procedure FlipControls(const AParentCtl: TWinControl);
procedure FlipRect(var Rect: TRect; const ParentRect: TRect; const UseRightToLeft: Boolean);
function IsParentFlipped(const AControl: TControl): Boolean;
function IsParentRightToLeft(const AControl: TControl): Boolean;
function SetBiDiStyles(const AControl: TControl; var AParams: TCreateParams): Boolean;

var
  { These two callbacks should be set by the caller. Inno Setup: set by the Setup.SetupForm unit: }
  IsParentFlippedFunc: function(AControl: TControl): Boolean;
  IsParentRightToLeftFunc: function(AControl: TControl): Boolean;

implementation

procedure FlipRect(var Rect: TRect; const ParentRect: TRect; const UseRightToLeft: Boolean);
var
  W: Integer;
begin
  if UseRightToLeft then begin
    W := Rect.Right - Rect.Left;
    Rect.Left := ParentRect.Right - (Rect.Left - ParentRect.Left) - W;
    Rect.Right := Rect.Left + W;
  end;
end;

function IsParentFlipped(const AControl: TControl): Boolean;
begin
  if Assigned(IsParentFlippedFunc) then
    Result := IsParentFlippedFunc(AControl)
  else
    Result := False;
end;

function IsParentRightToLeft(const AControl: TControl): Boolean;
begin
  if Assigned(IsParentRightToLeftFunc) then
    Result := IsParentRightToLeftFunc(AControl)
  else
    Result := False;
end;

function SetBiDiStyles(const AControl: TControl; var AParams: TCreateParams): Boolean;
begin
  Result := IsParentRightToLeft(AControl);
  if Result then
    AParams.ExStyle := AParams.ExStyle or (WS_EX_RTLREADING or WS_EX_LEFTSCROLLBAR or WS_EX_RIGHT);
end;

type
  TControlAccess = class(TControl);

procedure FlipControls(const AParentCtl: TWinControl);
var
  ParentWidth, I: Integer;
  Ctl: TControl;
begin
  if AParentCtl.ControlCount = 0 then
    Exit;
  AParentCtl.DisableAlign;
  try
    ParentWidth := AParentCtl.ClientWidth;
    for I := 0 to AParentCtl.ControlCount-1 do begin
      Ctl := AParentCtl.Controls[I];
      if (akLeft in Ctl.Anchors) and not (akRight in Ctl.Anchors) then
        Ctl.Anchors := Ctl.Anchors - [akLeft] + [akRight]
      else if not (akLeft in Ctl.Anchors) and (akRight in Ctl.Anchors) then begin
        { Before we can set Anchors to [akLeft, akTop] (which has a special
          'no anchors' meaning to VCL), we first need to update the Explicit*
          properties so the control doesn't get moved back to an old position. }
        if Ctl.Anchors = [akTop, akRight] then
          TControlAccess(Ctl).UpdateExplicitBounds;
        Ctl.Anchors := Ctl.Anchors - [akRight] + [akLeft];
      end;
      Ctl.Left := ParentWidth - Ctl.Width - Ctl.Left;
    end;
  finally
    AParentCtl.EnableAlign;
  end;
  for I := 0 to AParentCtl.ControlCount-1 do
    if AParentCtl.Controls[I] is TWinControl then
      FlipControls(TWinControl(AParentCtl.Controls[I]));
end;

end.
