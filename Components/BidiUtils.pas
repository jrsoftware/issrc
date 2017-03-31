unit BidiUtils;

{
  Inno Setup
  Copyright (C) 1997-2007 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Bidi utility functions

  $jrsoftware: issrc/Components/BidiUtils.pas,v 1.2 2007/11/27 04:52:53 jr Exp $
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
  { These are set by the SetupForm unit: }
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
var
  ClassName: String;
  ExStyle: DWORD;

begin
  Result := IsParentRightToLeft(AControl);
  if Result then
  begin
    ClassName := AControl.ClassType.ClassName;
    
    if (ClassName = 'TNewProgressBar') then
       ExStyle := (WS_EX_LAYOUTRTL or WS_EX_NOINHERITLAYOUT)
    else
       ExStyle := (WS_EX_RTLREADING or WS_EX_LEFTSCROLLBAR or WS_EX_RIGHT);

    AParams.ExStyle := AParams.ExStyle or ExStyle;
  end;
end;

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
