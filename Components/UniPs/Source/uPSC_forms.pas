{ Compiletime Forms support }
unit uPSC_forms;
{$I PascalScript.inc}

interface
uses
  uPSCompiler, uPSUtils;

procedure SIRegister_Forms_TypesAndConsts(Cl: TPSPascalCompiler);


procedure SIRegisterTCONTROLSCROLLBAR(Cl: TPSPascalCompiler);
procedure SIRegisterTSCROLLINGWINCONTROL(Cl: TPSPascalCompiler);
procedure SIRegisterTSCROLLBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTFORM(Cl: TPSPascalCompiler);
procedure SIRegisterTAPPLICATION(Cl: TPSPascalCompiler);

procedure SIRegister_Forms(Cl: TPSPascalCompiler);

implementation

procedure SIRegisterTCONTROLSCROLLBAR(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TControlScrollBar') do
  begin
    RegisterProperty('Kind', 'TScrollBarKind', iptr);
    RegisterProperty('ScrollPos', 'Integer', iptr);
    RegisterProperty('Margin', 'Word', iptrw);
    RegisterProperty('Increment', 'TScrollBarInc', iptrw);
    RegisterProperty('Range', 'Integer', iptrw);
    RegisterProperty('Position', 'Integer', iptrw);
    RegisterProperty('Tracking', 'Boolean', iptrw);
    RegisterProperty('Visible', 'Boolean', iptrw);
  end;
end;

procedure SIRegisterTSCROLLINGWINCONTROL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TWinControl'), 'TScrollingWinControl') do
  begin
    RegisterMethod('procedure ScrollInView(AControl: TControl)');
    RegisterProperty('HorzScrollBar', 'TControlScrollBar', iptrw);
    RegisterProperty('VertScrollBar', 'TControlScrollBar', iptrw);
  end;
end;

procedure SIRegisterTSCROLLBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TScrollingWinControl'), 'TScrollBox') do
  begin
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('Constraints', 'TSizeConstraints', iptrw);
    RegisterProperty('BorderStyle', 'TBorderStyle', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('AutoScroll', 'Boolean', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnExit', 'TNotifyEvent', iptrw);

    {$IFNDEF PS_MINIVCL}
    RegisterProperty('OnResize', 'TNotifyEvent', iptrw);
    RegisterProperty('DragCursor', 'TCursor', iptrw);
    RegisterProperty('DragMode', 'TDragMode', iptrw);
    RegisterProperty('ParentShowHint', 'Boolean', iptrw);
    RegisterProperty('PopupMenu', 'TPopupMenu', iptrw);
    RegisterProperty('CTL3D', 'Boolean', iptrw);
    RegisterProperty('ParentCtl3D', 'Boolean', iptrw);
    RegisterProperty('OnDragDrop', 'TDragDropEvent', iptrw);
    RegisterProperty('OnDragOver', 'TDragOverEvent', iptrw);
    RegisterProperty('OnEndDrag', 'TEndDragEvent', iptrw);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    {$ENDIF}
  end;
end;

procedure SIRegisterTFORM(Cl: TPSPascalCompiler);
begin
 with Cl.AddClassN(cl.FindClass('TScrollingWinControl'), 'TForm') do
  begin
    RegisterMethod('constructor CreateNew(AOwner: TComponent; Dummy: Integer)');
    RegisterMethod('procedure Close');
    RegisterMethod('procedure Hide');
    RegisterMethod('procedure Show');
    RegisterMethod('function ShowModal: Integer');
    RegisterMethod('procedure Release');
    RegisterProperty('Active', 'Boolean', iptr);
    RegisterProperty('ActiveControl', 'TWinControl', iptrw);
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('Constraints', 'TSizeConstraints', iptrw);
    RegisterProperty('BorderIcons', 'TBorderIcons', iptrw);
    RegisterProperty('BorderStyle', 'TFormBorderStyle', iptrw);
    RegisterProperty('Caption', 'NativeString', iptrw);
    RegisterProperty('AutoScroll', 'Boolean', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('FormStyle', 'TFormStyle', iptrw);
    RegisterProperty('KeyPreview', 'Boolean', iptrw);
    RegisterProperty('PopupMode', 'TPopupMode', iptrw);
    RegisterProperty('PopupParent', 'TForm', iptrw);
    RegisterProperty('Position', 'TPosition', iptrw);
    RegisterProperty('OnActivate', 'TNotifyEvent', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnClose', 'TCloseEvent', iptrw);
    RegisterProperty('OnCloseQuery', 'TCloseQueryEvent', iptrw);
    RegisterProperty('OnCreate', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDestroy', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDeactivate', 'TNotifyEvent', iptrw);
    RegisterProperty('OnHide', 'TNotifyEvent', iptrw);
    RegisterProperty('OnKeyDown', 'TKeyEvent', iptrw);
    RegisterProperty('OnKeyPress', 'TKeyPressEvent', iptrw);
    RegisterProperty('OnKeyUp', 'TKeyEvent', iptrw);
    RegisterProperty('OnResize', 'TNotifyEvent', iptrw);
    RegisterProperty('OnShow', 'TNotifyEvent', iptrw);


    {$IFNDEF PS_MINIVCL}
    RegisterMethod('procedure ArrangeIcons');
    RegisterMethod('procedure Print');
    RegisterMethod('procedure SendCancelMode(Sender: TControl)');
    RegisterProperty('ActiveOleControl', 'TWinControl', iptrw);
    RegisterProperty('ClientHandle', 'HWND', iptr);
    RegisterProperty('TileMode', 'TTileMode', iptrw);
    RegisterMethod('procedure Cascade');
    RegisterMethod('procedure Next');
    RegisterMethod('procedure Previous');
    RegisterMethod('procedure Tile');
    RegisterProperty('ActiveMDIChild', 'TForm', iptr);
    RegisterProperty('DropTarget', 'Boolean', iptrw);
    RegisterProperty('MDIChildCount', 'Integer', iptr);
    RegisterProperty('MDIChildren', 'TForm Integer', iptr);
    RegisterMethod('function CloseQuery: Boolean');
    RegisterMethod('procedure DefocusControl(Control: TWinControl; Removing: Boolean)');
    RegisterMethod('procedure FocusControl(Control: TWinControl)');
    RegisterMethod('function SetFocusedControl(Control: TWinControl): Boolean');
    RegisterProperty('Canvas', 'TCanvas', iptr);
    RegisterProperty('ModalResult', 'LongInt', iptrw);
    RegisterProperty('Icon', 'TIcon', iptrw);
    RegisterProperty('Menu', 'TMainMenu', iptrw);
    RegisterProperty('ObjectMenuItem', 'TMenuItem', iptrw);
    RegisterProperty('PixelsPerInch', 'Integer', iptrw);
    RegisterProperty('PrintScale', 'TPrintScale', iptrw);
    RegisterProperty('Scaled', 'Boolean', iptrw);
    RegisterProperty('WindowState', 'TWindowState', iptrw);
    RegisterProperty('WindowMenu', 'TMenuItem', iptrw);
    RegisterProperty('CTL3D', 'Boolean', iptrw);
    RegisterProperty('PopupMenu', 'TPopupMenu', iptrw);
    RegisterProperty('OnDragDrop', 'TDragDropEvent', iptrw);
    RegisterProperty('OnDragOver', 'TDragOverEvent', iptrw);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnPaint', 'TNotifyEvent', iptrw);
    {$ENDIF}
  end;
end;

procedure SIRegisterTAPPLICATION(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TComponent'), 'TApplication') do
  begin
    RegisterMethod('procedure BringToFront');
{$IFDEF PS_PANSICHAR}
    RegisterMethod('function MessageBox(Text,Caption: PAnsiChar; Flags: Longint): Integer');
{$ELSE}
    RegisterMethod('function MessageBox(Text,Caption: string; Flags: Longint): Integer');
{$ENDIF}
    RegisterMethod('procedure Minimize');
    RegisterMethod('procedure Restore');
    RegisterProperty('Active', 'Boolean', iptr);
    RegisterMethod('procedure ProcessMessages');
    RegisterMethod('procedure Terminate');
    RegisterProperty('ExeName', 'NativeString', iptr);
    RegisterProperty('Handle', 'HWND', iptrw);
    RegisterProperty('UpdateFormatSettings', 'Boolean', iptrw);
    RegisterProperty('Hint', 'NativeString', iptrw);
    RegisterProperty('MainForm', 'TForm', iptr);
    RegisterProperty('ShowHint', 'Boolean', iptrw);
    RegisterProperty('ShowMainForm', 'Boolean', iptrw);
    RegisterProperty('Terminated', 'Boolean', iptr);
    RegisterProperty('Title', 'NativeString', iptrw);
    RegisterProperty('OnActivate', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDeactivate', 'TNotifyEvent', iptrw);
    RegisterProperty('OnIdle', 'TIdleEvent', iptrw);
    RegisterProperty('OnHint', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMinimize', 'TNotifyEvent', iptrw);
    RegisterProperty('OnRestore', 'TNotifyEvent', iptrw);

    {$IFNDEF PS_MINIVCL}
    RegisterMethod('procedure ControlDestroyed(Control: TControl)');
    RegisterMethod('procedure CancelHint');
    RegisterMethod('procedure HandleMessage');
    RegisterMethod('procedure HideHint');
    RegisterMethod('procedure Initialize');
    RegisterMethod('procedure NormalizeTopMosts');
    RegisterMethod('procedure RestoreTopMosts');
    RegisterMethod('procedure Run');
    RegisterMethod('function HelpCommand(Command: Integer; Data: NativeInt): Boolean');
    RegisterMethod('function HelpContext(Context: THelpContext): Boolean');
    RegisterMethod('function HelpJump(JumpID: NativeString): Boolean');
    RegisterProperty('DialogHandle', 'HWND', iptrw);
    RegisterMethod('procedure CreateHandle');
    RegisterProperty('HelpFile', 'NativeString', iptrw);
    RegisterProperty('HintColor', 'TColor', iptrw);
    RegisterProperty('HintPause', 'Integer', iptrw);
    RegisterProperty('HintShortPause', 'Integer', iptrw);
    RegisterProperty('HintHidePause', 'Integer', iptrw);
    RegisterProperty('Icon', 'TIcon', iptrw);
    RegisterProperty('OnHelp', 'THelpEvent', iptrw);
    {$ENDIF}
  end;
end;

procedure SIRegister_Forms_TypesAndConsts(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TIdleEvent', 'procedure (Sender: TObject; var Done: Boolean)');
  cl.AddTypeS('TScrollBarKind', '(sbHorizontal, sbVertical)');
  cl.AddTypeS('TScrollBarInc', 'SmallInt');
  cl.AddTypeS('TFormBorderStyle', '(bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow, bsSizeToolWin)');
  cl.AddTypeS('TBorderStyle', 'TFormBorderStyle');
  cl.AddTypeS('TWindowState', '(wsNormal, wsMinimized, wsMaximized)');
  cl.AddTypeS('TFormStyle', '(fsNormal, fsMDIChild, fsMDIForm, fsStayOnTop)');
  cl.AddTypeS('TPopupMode', '(pmNone, pmAuto, pmExplicit)');
  cl.AddTypeS('TPosition', '(poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly, poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter)');
  cl.AddTypeS('TPrintScale', '(poNone, poProportional, poPrintToFit)');
  cl.AddTypeS('TCloseAction', '(caNone, caHide, caFree, caMinimize)');
  cl.AddTypeS('TCloseEvent' ,'procedure(Sender: TObject; var Action: TCloseAction)');
  cl.AddTypeS('TCloseQueryEvent' ,'procedure(Sender: TObject; var CanClose: Boolean)');
  cl.AddTypeS('TBorderIcon' ,'(biSystemMenu, biMinimize, biMaximize, biHelp)');
  cl.AddTypeS('TBorderIcons', 'set of TBorderIcon');
  cl.AddTypeS('THelpContext', 'LongInt');
  if cl.FindType('HWND') = nil then
    cl.AddTypeS('HWND', 'NativeUInt');
end;

procedure SIRegister_Forms(Cl: TPSPascalCompiler);
begin
  SIRegister_Forms_TypesAndConsts(cl);

  {$IFNDEF PS_MINIVCL}
  SIRegisterTCONTROLSCROLLBAR(cl);
  {$ENDIF}
  SIRegisterTScrollingWinControl(cl);
  {$IFNDEF PS_MINIVCL}
  SIRegisterTSCROLLBOX(cl);
  {$ENDIF}
  SIRegisterTForm(Cl);
  {$IFNDEF PS_MINIVCL}
  SIRegisterTApplication(Cl);
  {$ENDIF}
end;

// PS_MINIVCL changes by Martijn Laan


end.

