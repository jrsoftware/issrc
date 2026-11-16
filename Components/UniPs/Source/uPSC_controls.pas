{ Compiletime Controls support }
unit uPSC_controls;
{$I PascalScript.inc}
interface
uses
  uPSCompiler, uPSUtils;

{
  Will register files from:
    Controls
 
  Register the STD, Classes (at least the types&consts) and Graphics libraries first
 
}

procedure SIRegister_Controls_TypesAndConsts(Cl: TPSPascalCompiler);

procedure SIRegisterTControl(Cl: TPSPascalCompiler);
procedure SIRegisterTWinControl(Cl: TPSPascalCompiler); 
procedure SIRegisterTGraphicControl(cl: TPSPascalCompiler); 
procedure SIRegisterTCustomControl(cl: TPSPascalCompiler); 
procedure SIRegisterTDragObject(cl: TPSPascalCompiler);
procedure SIRegisterTSizeConstraints(cl: TPSPascalCompiler);

procedure SIRegister_Controls(Cl: TPSPascalCompiler);


implementation

procedure SIRegisterTControl(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TComponent'), 'TControl') do
  begin
    RegisterMethod('constructor Create(AOwner: TComponent);');
    RegisterMethod('procedure BringToFront;');
    RegisterMethod('procedure Hide;');
    RegisterMethod('procedure Invalidate; virtual;');
    RegisterMethod('procedure Refresh;');
    RegisterMethod('procedure Repaint; virtual;');
    RegisterMethod('procedure SendToBack;');
    RegisterMethod('procedure Show;');
    RegisterMethod('procedure Update; virtual;');
    RegisterMethod('procedure SetBounds(X,Y,w,h: Integer); virtual;');
    RegisterProperty('Left', 'Integer', iptRW);
    RegisterProperty('Top', 'Integer', iptRW);
    RegisterProperty('Width', 'Integer', iptRW);
    RegisterProperty('Height', 'Integer', iptRW);
    RegisterProperty('Hint', 'string', iptRW);
    RegisterProperty('Align', 'TAlign', iptRW);
    RegisterProperty('ClientHeight', 'LongInt', iptRW);
    RegisterProperty('ClientWidth', 'LongInt', iptRW);
    RegisterProperty('ShowHint', 'Boolean', iptRW);
    RegisterProperty('Visible', 'Boolean', iptRW);
    RegisterProperty('Enabled', 'Boolean', iptrw);
    RegisterProperty('Cursor', 'TCursor', iptrw);
  
    RegisterProperty('StyleElements', 'TStyleElements', iptrw);
    RegisterProperty('StyleName', 'string', iptrw);

    {$IFNDEF PS_MINIVCL}
    RegisterMethod('function Dragging: Boolean;');
    RegisterMethod('function HasParent: Boolean');
    RegisterMethod('function ClientToScreen(Point: TPoint): TPoint');
    RegisterMethod('procedure BeginDrag(Immediate: Boolean)');
    RegisterMethod('procedure EndDrag(Drop: Boolean)');
    RegisterMethod('function GetTextBuf(Buffer: PChar; BufSize: Integer): Integer');
    RegisterMethod('function GetTextLen: Integer');
    RegisterMethod('procedure SetTextBuf(Buffer: PChar)');
    RegisterMethod('function Perform(Msg: Cardinal; WParam: NativeUInt; LParam: NativeInt): NativeInt');
    RegisterMethod('function ScreenToClient(Point: TPoint): TPoint');
    {$ENDIF}
  end;
end;

procedure SIRegisterTWinControl(Cl: TPSPascalCompiler); // requires TControl
begin
  with Cl.AddClassN(cl.FindClass('TControl'), 'TWinControl') do
  begin

    with Cl.FindClass('TControl') do
    begin
      RegisterProperty('Parent', 'TWinControl', iptRW);
    end;

    RegisterProperty('Handle', 'HWND', iptR);
    RegisterProperty('Showing', 'Boolean', iptR);
    RegisterProperty('TabOrder', 'Integer', iptRW);
    RegisterProperty('TabStop', 'Boolean', iptRW);
    RegisterMethod('function CanFocus: Boolean');
    RegisterMethod('function Focused: Boolean');
    RegisterProperty('Controls', 'TControl Integer', iptr);
    RegisterProperty('ControlCount', 'Integer', iptr);

    {$IFNDEF PS_MINIVCL}
    RegisterMethod('function HandleAllocated: Boolean;');
    RegisterMethod('procedure HandleNeeded;');
    RegisterMethod('procedure EnableAlign;');
    RegisterMethod('procedure RemoveControl(AControl: TControl);');
    RegisterMethod('procedure InsertControl(AControl: TControl);');
    RegisterMethod('procedure ScaleBy(M, D: Integer);');
    RegisterMethod('procedure ScrollBy(DeltaX, DeltaY: Integer);');
    RegisterMethod('procedure PaintTo(DC: HDC; X,Y: Integer)');
    RegisterMethod('procedure Realign;');
    RegisterMethod('procedure SetFocus; virtual;');

    RegisterMethod('function ContainsControl(Control: TControl): Boolean');
    RegisterMethod('procedure DisableAlign');
    RegisterMethod('procedure UpdateControlState');

    RegisterProperty('Brush', 'TBrush', iptr);
    RegisterProperty('HelpContext', 'LongInt', iptrw);
    {$ENDIF}
  end;
end;
procedure SIRegisterTGraphicControl(cl: TPSPascalCompiler); // requires TControl
begin
  Cl.AddClassN(cl.FindClass('TControl'), 'TGraphicControl');
end;

procedure SIRegisterTCustomControl(cl: TPSPascalCompiler); // requires TWinControl
begin
  Cl.AddClassN(cl.FindClass('TWinControl'), 'TCustomControl');
end;

procedure SIRegister_Controls_TypesAndConsts(Cl: TPSPascalCompiler);
begin
  Cl.addTypeS('TEShiftState','(ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble)');
  Cl.addTypeS('TShiftState','set of TEShiftState');
  cl.AddTypeS('TMouseButton', '(mbLeft, mbRight, mbMiddle)');
  cl.AddTypeS('TDragMode', '(dmManual, dmAutomatic)');
  cl.AddTypeS('TDragState', '(dsDragEnter, dsDragLeave, dsDragMove)');
  cl.AddTypeS('TDragKind', '(dkDrag, dkDock)');
  cl.AddTypeS('TMouseEvent', 'procedure (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);');
  cl.AddTypeS('TMouseMoveEvent', 'procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer);');
  cl.AddTypeS('TKeyEvent', 'procedure (Sender: TObject; var Key: Word; Shift: TShiftState);');
  cl.AddTypeS('TKeyPressEvent', 'procedure(Sender: TObject; var Key: Char);');
  cl.AddTypeS('TDragOverEvent', 'procedure(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean)');
  cl.AddTypeS('TDragDropEvent', 'procedure(Sender, Source: TObject; X, Y: Integer)');
  if cl.FindType('HWND') = nil then
    cl.AddTypeS('HWND', 'NativeUInt');
  if cl.FindType('HDC') = nil then
    cl.AddTypeS('HDC', 'NativeUInt');
  {$IFNDEF PS_MINIVCL}
  if cl.FindType('THandle') = nil then
    cl.AddTypeS('THandle', 'NativeUInt');
  {$ENDIF}

  cl.AddTypeS('TEndDragEvent', 'procedure(Sender, Target: TObject; X, Y: Integer)');

  cl.addTypeS('TAlign', '(alNone, alTop, alBottom, alLeft, alRight, alClient)');

  cl.addTypeS('TStyleElement', '(seFont, seClient, seBorder)');
  cl.addTypeS('TStyleElements', 'set of TStyleElement');

  cl.addTypeS('TAnchorKind', '(akLeft, akTop, akRight, akBottom)');
  cl.addTypeS('TAnchors','set of TAnchorKind');
  cl.AddTypeS('TModalResult', 'Integer');
  cl.AddTypeS('TCursor', 'Integer');
  cl.AddTypeS('TPoint', 'record X,Y: LongInt; end;');

  cl.AddConstantN('mrNone', 'Integer').Value.ts32 := 0;
  cl.AddConstantN('mrOk', 'Integer').Value.ts32 := 1;
  cl.AddConstantN('mrCancel', 'Integer').Value.ts32 := 2;
  cl.AddConstantN('mrAbort', 'Integer').Value.ts32 := 3;
  cl.AddConstantN('mrRetry', 'Integer').Value.ts32 := 4;
  cl.AddConstantN('mrIgnore', 'Integer').Value.ts32 := 5;
  cl.AddConstantN('mrYes', 'Integer').Value.ts32 := 6;
  cl.AddConstantN('mrNo', 'Integer').Value.ts32 := 7;
  cl.AddConstantN('mrAll', 'Integer').Value.ts32 := 8;
  cl.AddConstantN('mrNoToAll', 'Integer').Value.ts32 := 9;
  cl.AddConstantN('mrYesToAll', 'Integer').Value.ts32 := 10;
  cl.AddConstantN('crDefault', 'Integer').Value.ts32 := 0;
  cl.AddConstantN('crNone', 'Integer').Value.ts32 := -1;
  cl.AddConstantN('crArrow', 'Integer').Value.ts32 := -2;
  cl.AddConstantN('crCross', 'Integer').Value.ts32 := -3;
  cl.AddConstantN('crIBeam', 'Integer').Value.ts32 := -4;
  cl.AddConstantN('crSizeNESW', 'Integer').Value.ts32 := -6;
  cl.AddConstantN('crSizeNS', 'Integer').Value.ts32 := -7;
  cl.AddConstantN('crSizeNWSE', 'Integer').Value.ts32 := -8;
  cl.AddConstantN('crSizeWE', 'Integer').Value.ts32 := -9;
  cl.AddConstantN('crUpArrow', 'Integer').Value.ts32 := -10;
  cl.AddConstantN('crHourGlass', 'Integer').Value.ts32 := -11;
  cl.AddConstantN('crDrag', 'Integer').Value.ts32 := -12;
  cl.AddConstantN('crNoDrop', 'Integer').Value.ts32 := -13;
  cl.AddConstantN('crHSplit', 'Integer').Value.ts32 := -14;
  cl.AddConstantN('crVSplit', 'Integer').Value.ts32 := -15;
  cl.AddConstantN('crMultiDrag', 'Integer').Value.ts32 := -16;
  cl.AddConstantN('crSQLWait', 'Integer').Value.ts32 := -17;
  cl.AddConstantN('crNo', 'Integer').Value.ts32 := -18;
  cl.AddConstantN('crAppStart', 'Integer').Value.ts32 := -19;
  cl.AddConstantN('crHelp', 'Integer').Value.ts32 := -20;
  cl.AddConstantN('crHandPoint', 'Integer').Value.ts32 := -21;
  cl.AddConstantN('crSizeAll', 'Integer').Value.ts32 := -22;
  cl.AddTypeS('TBevelKind', '(bkNone, bkTile, bkSoft, bkFlat)');
end;

procedure SIRegisterTDragObject(cl: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TObject'),'TDragObject') do
  begin
{$IFNDEF PS_MINIVCL}
    RegisterMethod('procedure Assign(Source: TDragObject)');
    RegisterMethod('function GetName: string');
    RegisterMethod('function Instance: THandle');
    RegisterMethod('procedure HideDragImage');
    RegisterMethod('procedure ShowDragImage');
    RegisterProperty('Cancelling', 'Boolean', iptrw);
    RegisterProperty('DragHandle', 'HWND', iptrw);
    RegisterProperty('DragPos', 'TPoint', iptrw);
    RegisterProperty('DragTargetPos', 'TPoint', iptrw);
    RegisterProperty('MouseDeltaX', 'Double', iptr);
    RegisterProperty('MouseDeltaY', 'Double', iptr);
{$ENDIF}
  end;
  Cl.AddTypeS('TStartDragEvent', 'procedure (Sender: TObject; var DragObject: TDragObject)');
end;

procedure SIRegisterTSizeConstraints(cl: TPSPascalCompiler);
begin
  cl.AddTypeS('TConstraintSize', 'Integer');
  with CL.AddClassN(CL.FindClass('TPersistent'),'TSizeConstraints') do
  begin
    RegisterProperty('MaxHeight', 'TConstraintSize', iptrw);
    RegisterProperty('MaxWidth', 'TConstraintSize', iptrw);
    RegisterProperty('MinHeight', 'TConstraintSize', iptrw);
    RegisterProperty('MinWidth', 'TConstraintSize', iptrw);
  end;
end;

procedure SIRegister_Controls(Cl: TPSPascalCompiler);
begin
  SIRegister_Controls_TypesAndConsts(cl);
  SIRegisterTDragObject(cl);
  SIRegisterTSizeConstraints(cl);
  SIRegisterTControl(Cl);
  SIRegisterTWinControl(Cl);
  SIRegisterTGraphicControl(cl);
  SIRegisterTCustomControl(cl);
end;

// PS_MINIVCL changes by Martijn Laan

end.
