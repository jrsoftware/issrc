{ Compiletime Forms support }
unit uPSC_forms;
{$I PascalScript.inc}

interface
uses
  uPSCompiler, uPSUtils;

procedure SIRegister_Forms_TypesAndConsts(Cl: TPSPascalCompiler);


procedure SIRegisterTSCROLLINGWINCONTROL(Cl: TPSPascalCompiler);
procedure SIRegisterTFORM(Cl: TPSPascalCompiler);

procedure SIRegister_Forms(Cl: TPSPascalCompiler);

implementation

procedure SIRegisterTSCROLLINGWINCONTROL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TWinControl'), 'TScrollingWinControl') do
  begin
    RegisterMethod('procedure ScrollInView(AControl: TControl)');
    RegisterProperty('HorzScrollBar', 'TControlScrollBar', iptrw);
    RegisterProperty('VertScrollBar', 'TControlScrollBar', iptrw);
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

  SIRegisterTScrollingWinControl(cl);
  SIRegisterTForm(Cl);
end;

end.

