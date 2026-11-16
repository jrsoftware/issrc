
unit uPSR_controls;

{$I PascalScript.inc}
interface
uses
  uPSRuntime, uPSUtils;




procedure RIRegisterTControl(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTWinControl(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTGraphicControl(cl: TPSRuntimeClassImporter);
procedure RIRegisterTCustomControl(cl: TPSRuntimeClassImporter);
procedure RIRegister_TDragObject(CL: TPSRuntimeClassImporter);
procedure RIRegisterTSizeConstraints(cl: TPSRuntimeClassImporter);

procedure RIRegister_Controls(Cl: TPSRuntimeClassImporter);

implementation
uses
  Classes, Controls, Graphics, Windows;

procedure TControlAlignR(Self: TControl; var T: Byte); begin T := Byte(Self.Align); end;
procedure TControlAlignW(Self: TControl; T: Byte); begin Self.Align:= TAlign(T); end;

procedure TControlClientHeightR(Self: TControl; var T: Longint); begin T := Self.ClientHeight; end;
procedure TControlClientHeightW(Self: TControl; T: Longint); begin Self.ClientHeight := T; end;

procedure TControlClientWidthR(Self: TControl; var T: Longint); begin T := Self.ClientWidth; end;
procedure TControlClientWidthW(Self: TControl; T: Longint); begin Self.ClientWidth:= T; end;

procedure TControlShowHintR(Self: TControl; var T: Boolean); begin T := Self.ShowHint; end;
procedure TControlShowHintW(Self: TControl; T: Boolean); begin Self.ShowHint:= T; end;

procedure TControlVisibleR(Self: TControl; var T: Boolean); begin T := Self.Visible; end;
procedure TControlVisibleW(Self: TControl; T: Boolean); begin Self.Visible:= T; end;

procedure TControlParentR(Self: TControl; var T: TWinControl); begin T := Self.Parent; end;
procedure TControlParentW(Self: TControl; T: TWinControl); begin Self.Parent:= T; end;

procedure TCONTROLENABLED_W(Self: TCONTROL; T: BOOLEAN); begin Self.ENABLED := T; end;
procedure TCONTROLENABLED_R(Self: TCONTROL; var T: BOOLEAN); begin T := Self.ENABLED; end;

procedure TControlStyleElementsR(Self: TControl; var T: TStyleElements); begin T := Self.StyleElements; end;
procedure TControlStyleElementsW(Self: TControl; T: TStyleElements); begin Self.StyleElements:= T; end;

procedure TControlStyleNameR(Self: TControl; var T: string); begin T := Self.StyleName; end;
procedure TControlStyleNameW(Self: TControl; T: string); begin Self.StyleName:= T; end;

procedure RIRegisterTControl(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TControl) do
  begin
    RegisterVirtualConstructor(@TControl.Create, 'Create');
    RegisterMethod(@TControl.BRingToFront, 'BringToFront');
    RegisterMethod(@TControl.Hide, 'Hide');
    RegisterVirtualMethod(@TControl.Invalidate, 'Invalidate');
    RegisterMethod(@TControl.Refresh, 'Refresh');
    RegisterVirtualMethod(@TControl.Repaint, 'Repaint');
    RegisterMethod(@TControl.SendToBack, 'SendToBack');
    RegisterMethod(@TControl.Show, 'Show');
    RegisterVirtualMethod(@TControl.Update, 'Update');
    RegisterVirtualMethod(@TControl.SetBounds, 'SetBounds');

    RegisterPropertyHelper(@TControlShowHintR, @TControlShowHintW, 'ShowHint');
    RegisterPropertyHelper(@TControlAlignR, @TControlAlignW, 'Align');
    RegisterPropertyHelper(@TControlClientHeightR, @TControlClientHeightW, 'ClientHeight');
    RegisterPropertyHelper(@TControlClientWidthR, @TControlClientWidthW, 'ClientWidth');
    RegisterPropertyHelper(@TControlVisibleR, @TControlVisibleW, 'Visible');
    RegisterPropertyHelper(@TCONTROLENABLED_R, @TCONTROLENABLED_W, 'Enabled');

    RegisterPropertyHelper(@TControlStyleElementsR, @TControlStyleElementsW, 'StyleElements');
    RegisterPropertyHelper(@TControlStyleNameR, @TControlStyleNameW, 'StyleName');

    RegisterPropertyHelper(@TControlParentR, @TControlParentW, 'Parent');

    {$IFNDEF PS_MINIVCL}
    RegisterMethod(@TControl.Dragging, 'Dragging');
    RegisterMethod(@TControl.HasParent, 'HasParent');
    RegisterMethod(@TCONTROL.CLIENTTOSCREEN, 'ClientToScreen');
    RegisterMethod(@TCONTROL.BEGINDRAG, 'BeginDrag');
    RegisterMethod(@TCONTROL.ENDDRAG, 'EndDrag');
    RegisterMethod(@TCONTROL.GETTEXTBUF, 'GetTextBuf');
    RegisterMethod(@TCONTROL.GETTEXTLEN, 'GetTextLen');
    RegisterMethod(@TCONTROL.PERFORM, 'Perform');
    RegisterMethod(@TCONTROL.SETTEXTBUF, 'SetTextBuf');
    RegisterMethod(@TCONTROL.SCREENTOCLIENT, 'ScreenToClient');
    {$ENDIF}
  end;
end;
procedure TWinControlHandleR(Self: TWinControl; var T: HWND); begin T := Self.Handle; end;
procedure TWinControlShowingR(Self: TWinControl; var T: Boolean); begin T := Self.Showing; end;


procedure TWinControlTabOrderR(Self: TWinControl; var T: Longint); begin T := Self.TabOrder; end;
procedure TWinControlTabOrderW(Self: TWinControl; T: Longint); begin Self.TabOrder:= T; end;

procedure TWinControlTabStopR(Self: TWinControl; var T: Boolean); begin T := Self.TabStop; end;
procedure TWinControlTabStopW(Self: TWinControl; T: Boolean); begin Self.TabStop:= T; end;
procedure TWINCONTROLBRUSH_R(Self: TWINCONTROL; var T: TBRUSH); begin T := Self.BRUSH; end;
procedure TWINCONTROLCONTROLS_R(Self: TWINCONTROL; var T: TCONTROL; t1: INTEGER); begin t := Self.CONTROLS[t1]; end;
procedure TWINCONTROLCONTROLCOUNT_R(Self: TWINCONTROL; var T: INTEGER); begin t := Self.CONTROLCOUNT; end;

procedure RIRegisterTWinControl(Cl: TPSRuntimeClassImporter); // requires TControl
begin
  with Cl.Add(TWinControl) do
  begin
    RegisterPropertyHelper(@TWinControlHandleR, nil, 'Handle');
    RegisterPropertyHelper(@TWinControlShowingR, nil, 'Showing');
    RegisterPropertyHelper(@TWinControlTabOrderR, @TWinControlTabOrderW, 'TabOrder');
    RegisterPropertyHelper(@TWinControlTabStopR, @TWinControlTabStopW, 'TabStop');
    RegisterMethod(@TWINCONTROL.CANFOCUS, 'CanFocus');
    RegisterMethod(@TWINCONTROL.FOCUSED, 'Focused');
    RegisterPropertyHelper(@TWINCONTROLCONTROLS_R, nil, 'Controls');
    RegisterPropertyHelper(@TWINCONTROLCONTROLCOUNT_R, nil, 'ControlCount');
    {$IFNDEF PS_MINIVCL}
    RegisterMethod(@TWinControl.HandleAllocated, 'HandleAllocated');
    RegisterMethod(@TWinControl.HandleNeeded, 'HandleNeeded');
    RegisterMethod(@TWinControl.EnableAlign, 'EnableAlign');
		RegisterMethod(@TWinControl.RemoveControl, 'RemoveControl');
		RegisterMethod(@TWinControl.InsertControl, 'InsertControl');
		RegisterMethod(@TWinControl.ScaleBy, 'ScaleBy');
		RegisterMethod(@TWinControl.ScrollBy, 'ScrollBy');
		RegisterMethod(@TWINCONTROL.PAINTTO, 'PaintTo');
		RegisterMethod(@TWinControl.Realign, 'Realign');
		RegisterVirtualMethod(@TWinControl.SetFocus, 'SetFocus');
		RegisterMethod(@TWINCONTROL.CONTAINSCONTROL, 'ContainsControl');
		RegisterMethod(@TWINCONTROL.DISABLEALIGN, 'DisableAlign');
		RegisterMethod(@TWINCONTROL.UPDATECONTROLSTATE, 'UpdateControlState');
    RegisterPropertyHelper(@TWINCONTROLBRUSH_R, nil, 'Brush');
    {$ENDIF}
  end;
end;

procedure RIRegisterTGraphicControl(cl: TPSRuntimeClassImporter); // requires TControl
begin
  Cl.Add(TGraphicControl);
end;
procedure RIRegisterTCustomControl(cl: TPSRuntimeClassImporter); // requires TControl
begin
  Cl.Add(TCustomControl);
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure TDragObjectMouseDeltaY_R(Self: TDragObject; var T: Double);
begin T := Self.MouseDeltaY; end;

(*----------------------------------------------------------------------------*)
procedure TDragObjectMouseDeltaX_R(Self: TDragObject; var T: Double);
begin T := Self.MouseDeltaX; end;

(*----------------------------------------------------------------------------*)
procedure TDragObjectDragTargetPos_W(Self: TDragObject; const T: TPoint);
begin Self.DragTargetPos := T; end;

(*----------------------------------------------------------------------------*)
procedure TDragObjectDragTargetPos_R(Self: TDragObject; var T: TPoint);
begin T := Self.DragTargetPos; end;

(*----------------------------------------------------------------------------*)
procedure TDragObjectDragPos_W(Self: TDragObject; const T: TPoint);
begin Self.DragPos := T; end;

(*----------------------------------------------------------------------------*)
procedure TDragObjectDragPos_R(Self: TDragObject; var T: TPoint);
begin T := Self.DragPos; end;

(*----------------------------------------------------------------------------*)
procedure TDragObjectDragHandle_W(Self: TDragObject; const T: HWND);
begin Self.DragHandle := T; end;

(*----------------------------------------------------------------------------*)
procedure TDragObjectDragHandle_R(Self: TDragObject; var T: HWND);
begin T := Self.DragHandle; end;

(*----------------------------------------------------------------------------*)
procedure TDragObjectCancelling_W(Self: TDragObject; const T: Boolean);
begin Self.Cancelling := T; end;

(*----------------------------------------------------------------------------*)
procedure TDragObjectCancelling_R(Self: TDragObject; var T: Boolean);
begin T := Self.Cancelling; end;
(*----------------------------------------------------------------------------*)
procedure RIRegister_TDragObject(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TDragObject) do
  begin
{$IFNDEF PS_MINIVCL}
    RegisterVirtualMethod(@TDragObject.Assign, 'Assign');
    RegisterVirtualMethod(@TDragObject.GetName, 'GetName');
    RegisterVirtualMethod(@TDragObject.Instance, 'Instance');
    RegisterVirtualMethod(@TDragObject.HideDragImage, 'HideDragImage');
    RegisterVirtualMethod(@TDragObject.ShowDragImage, 'ShowDragImage');
    RegisterPropertyHelper(@TDragObjectCancelling_R,@TDragObjectCancelling_W,'Cancelling');
    RegisterPropertyHelper(@TDragObjectDragHandle_R,@TDragObjectDragHandle_W,'DragHandle');
    RegisterPropertyHelper(@TDragObjectDragPos_R,@TDragObjectDragPos_W,'DragPos');
    RegisterPropertyHelper(@TDragObjectDragTargetPos_R,@TDragObjectDragTargetPos_W,'DragTargetPos');
    RegisterPropertyHelper(@TDragObjectMouseDeltaX_R,nil,'MouseDeltaX');
    RegisterPropertyHelper(@TDragObjectMouseDeltaY_R,nil,'MouseDeltaY');
{$ENDIF}
  end;
end;

procedure RIRegisterTSizeConstraints(cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TSizeConstraints);
end;

procedure RIRegister_Controls(Cl: TPSRuntimeClassImporter);
begin
  RIRegisterTControl(Cl);
  RIRegisterTWinControl(Cl);
  RIRegisterTGraphicControl(cl);
  RIRegisterTCustomControl(cl);
  RIRegister_TDragObject(cl);
  RIRegisterTSizeConstraints(cl);
end;

// PS_MINIVCL changes by Martijn Laan


end.
