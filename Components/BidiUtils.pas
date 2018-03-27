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

procedure FlipRect(var Rect: TRect; const ParentRect: TRect; const UseRightToLeft: Boolean);
function IsParentRightToLeft(const AControl: TControl): Boolean;
function SetBiDiStyles(const AControl: TControl; var AParams: TCreateParams): Boolean;
function MapWindowPoint(const Handle: HWND; const Point: TPoint): TPoint;

var
  { These are set by the SetupForm unit: }
  IsParentRightToLeftFunc: function(AControl: TControl): Boolean;

implementation

uses
  FolderTreeView, NewProgressBar, BidiCtrls, NewNotebook;

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

function IsParentRightToLeft(const AControl: TControl): Boolean;
begin
  if Assigned(IsParentRightToLeftFunc) then
    Result := IsParentRightToLeftFunc(AControl)
  else
    Result := False;
end;

function SetBiDiStyles(const AControl: TControl; var AParams: TCreateParams): Boolean;
var
  ExStyle: DWORD;

begin
  Result := IsParentRightToLeft(AControl);
  if Result then
  begin
    if (AControl is TCustomFolderTreeView)
        or (AControl is TNewProgressBar)
        or (AControl is TNewNotebook)
        or (AControl is TNewNotebookPage)
        or (AControl is TNewPanel) then
       ExStyle := (WS_EX_LAYOUTRTL or WS_EX_NOINHERITLAYOUT)
    else
       ExStyle := (WS_EX_RTLREADING or WS_EX_RIGHT or WS_EX_LEFTSCROLLBAR);

    AParams.ExStyle := AParams.ExStyle or ExStyle;
  end;
end;

{ In mirrord Windows we should use MapWindowPoints instant of ScreenToClient or
  ClientToScreen }
function MapWindowPoint(const Handle: HWND; const Point: TPoint): TPoint;
var
 RPoint: TPoint;
begin
  RPoint := Point;
  MapWindowPoints(0, Handle, RPoint, 1);
  Result := RPoint;
end;

end.
