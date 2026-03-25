
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
{$IFDEF DELPHI4UP}
procedure RIRegisterTSizeConstraints(cl: TPSRuntimeClassImporter);
{$ENDIF}

procedure RIRegister_Controls(Cl: TPSRuntimeClassImporter);

implementation
{$IFNDEF FPC}
uses
  Classes{$IFDEF CLX}, QControls, QGraphics{$ELSE}, Controls, Graphics, Windows{$ENDIF};
{$ELSE}
uses
  Classes, Controls, Graphics;
{$ENDIF}

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


procedure TCONTROLSHOWHINT_W(Self: TCONTROL; T: BOOLEAN); begin Self.SHOWHINT := T; end;
procedure TCONTROLSHOWHINT_R(Self: TCONTROL; var T: BOOLEAN); begin T := Self.SHOWHINT; end;
procedure TCONTROLENABLED_W(Self: TCONTROL; T: BOOLEAN); begin Self.ENABLED := T; end;
procedure TCONTROLENABLED_R(Self: TCONTROL; var T: BOOLEAN); begin T := Self.ENABLED; end;

{$IFDEF DELPHI23UP}
procedure TControlStyleElementsR(Self: TControl; var T: TStyleElements); begin T := Self.StyleElements; end;
procedure TControlStyleElementsW(Self: TControl; T: TStyleElements); begin Self.StyleElements:= T; end;
{$ENDIF}

{$IFDEF DELPHI26UP}
procedure TControlStyleNameR(Self: TControl; var T: string); begin T := Self.StyleName; end;
procedure TControlStyleNameW(Self: TControl; T: string); begin Self.StyleName:= T; end;
{$ENDIF}

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

    {$IFDEF DELPHI23UP}
    RegisterPropertyHelper(@TControlStyleElementsR, @TControlStyleElementsW, 'StyleElements');
    {$ENDIF}
    {$IFDEF DELPHI26UP}
    RegisterPropertyHelper(@TControlStyleNameR, @TControlStyleNameW, 'StyleName');
    {$ENDIF}

    RegisterPropertyHelper(@TControlParentR, @TControlParentW, 'Parent');

    {$IFNDEF PS_MINIVCL}
    RegisterMethod(@TControl.Dragging, 'Dragging');
    RegisterMethod(@TControl.HasParent, 'HasParent');
    RegisterMethod(@TCONTROL.CLIENTTOSCREEN, 'ClientToScreen');
    RegisterMethod(@TCONTROL.DRAGGING, 'Dragging');
   {$IFNDEF FPC} 
    RegisterMethod(@TCONTROL.BEGINDRAG, 'BeginDrag');
    RegisterMethod(@TCONTROL.ENDDRAG, 'EndDrag');
   {$ENDIF}
    {$IFNDEF CLX}
    RegisterMethod(@TCONTROL.GETTEXTBUF, 'GetTextBuf');
    RegisterMethod(@TCONTROL.GETTEXTLEN, 'GetTextLen');
    RegisterMethod(@TCONTROL.PERFORM, 'Perform');
    RegisterMethod(@TCONTROL.SETTEXTBUF, 'SetTextBuf');
    {$ENDIF}
    RegisterMethod(@TCONTROL.SCREENTOCLIENT, 'ScreenToClient');
    {$ENDIF}
  end;
end;
{$IFNDEF CLX}
procedure TWinControlHandleR(Self: TWinControl; var T: HWND); begin T := Self.Handle; end;
{$ENDIF}
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
    {$IFNDEF CLX}
    RegisterPropertyHelper(@TWinControlHandleR, nil, 'Handle');
    {$ENDIF}
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
		{$IFNDEF FPC}
		RegisterMethod(@TWinControl.InsertControl, 'InsertControl');
		RegisterMethod(@TWinControl.ScaleBy, 'ScaleBy');
		RegisterMethod(@TWinControl.ScrollBy, 'ScrollBy');
		{$IFNDEF CLX}
		 RegisterMethod(@TWINCONTROL.PAINTTO, 'PaintTo');
		{$ENDIF}
		{$ENDIF}{FPC}
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

{$IFDEF DELPHI4UP}
(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure TDragObjectMouseDeltaY_R(Self: TDragObject; var T: Double);
begin T := Self.MouseDeltaY; end;

(*----------------------------------------------------------------------------*)
procedure TDragObjectMouseDeltaX_R(Self: TDragObject; var T: Double);
begin T := Self.MouseDeltaX; end;

(*----------------------------------------------------------------------------*)
procedure TDragObjectDragTarget_W(Self: TDragObject; const T: Pointer);
begin Self.DragTarget := T; end;

(*----------------------------------------------------------------------------*)
procedure TDragObjectDragTarget_R(Self: TDragObject; var T: Pointer);
begin T := Self.DragTarget; end;

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
{$ENDIF}
(*----------------------------------------------------------------------------*)
procedure RIRegister_TDragObject(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TDragObject) do
  begin
{$IFNDEF PS_MINIVCL}
{$IFDEF DELPHI4UP}
    RegisterVirtualMethod(@TDragObject.Assign, 'Assign');
{$ENDIF}
{$IFNDEF FPC}
    RegisterVirtualMethod(@TDragObject.GetName, 'GetName');
    RegisterVirtualMethod(@TDragObject.Instance, 'Instance');
{$ENDIF}    
    RegisterVirtualMethod(@TDragObject.HideDragImage, 'HideDragImage');
    RegisterVirtualMethod(@TDragObject.ShowDragImage, 'ShowDragImage');
{$IFDEF DELPHI4UP}
    RegisterPropertyHelper(@TDragObjectCancelling_R,@TDragObjectCancelling_W,'Cancelling');
    RegisterPropertyHelper(@TDragObjectDragHandle_R,@TDragObjectDragHandle_W,'DragHandle');
    RegisterPropertyHelper(@TDragObjectDragPos_R,@TDragObjectDragPos_W,'DragPos');
    RegisterPropertyHelper(@TDragObjectDragTargetPos_R,@TDragObjectDragTargetPos_W,'DragTargetPos');
    RegisterPropertyHelper(@TDragObjectDragTarget_R,@TDragObjectDragTarget_W,'DragTarget');
    RegisterPropertyHelper(@TDragObjectMouseDeltaX_R,nil,'MouseDeltaX');
    RegisterPropertyHelper(@TDragObjectMouseDeltaY_R,nil,'MouseDeltaY');
{$ENDIF}
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
