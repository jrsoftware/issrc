
unit uPSR_forms;

{$I PascalScript.inc}
interface
uses
  uPSRuntime, uPSUtils;

procedure RIRegisterTCONTROLSCROLLBAR(Cl: TPSRuntimeClassImporter);
{$IFNDEF FPC} procedure RIRegisterTSCROLLINGWINCONTROL(Cl: TPSRuntimeClassImporter);{$ENDIF}
procedure RIRegisterTSCROLLBOX(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFORM(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTAPPLICATION(Cl: TPSRuntimeClassImporter);

procedure RIRegister_Forms(Cl: TPSRuntimeClassImporter);

implementation
uses
  sysutils, classes, {$IFDEF CLX}QControls, QForms, QGraphics{$ELSE}Controls, Forms, Graphics{$ENDIF};

procedure TCONTROLSCROLLBARKIND_R(Self: TCONTROLSCROLLBAR; var T: TSCROLLBARKIND); begin T := Self.KIND; end;
procedure TCONTROLSCROLLBARSCROLLPOS_R(Self: TCONTROLSCROLLBAR; var T: INTEGER); begin t := Self.SCROLLPOS; end;

procedure RIRegisterTCONTROLSCROLLBAR(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCONTROLSCROLLBAR) do
  begin
    RegisterPropertyHelper(@TCONTROLSCROLLBARKIND_R, nil, 'Kind');
    RegisterPropertyHelper(@TCONTROLSCROLLBARSCROLLPOS_R, nil, 'ScrollPos');
  end;
end;

{$IFNDEF FPC}
procedure RIRegisterTSCROLLINGWINCONTROL(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSCROLLINGWINCONTROL) do
  begin
    RegisterMethod(@TSCROLLINGWINCONTROL.SCROLLINVIEW, 'ScrollInView');
  end;
end;
{$ENDIF}

procedure RIRegisterTSCROLLBOX(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TSCROLLBOX);
end;
{$IFNDEF FPC}
{$IFNDEF CLX}
procedure TFORMACTIVEOLECONTROL_W(Self: TFORM; T: TWINCONTROL); begin Self.ACTIVEOLECONTROL := T; end;
procedure TFORMACTIVEOLECONTROL_R(Self: TFORM; var T: TWINCONTROL); begin T := Self.ACTIVEOLECONTROL; 
end;
procedure TFORMTILEMODE_W(Self: TFORM; T: TTILEMODE); begin Self.TILEMODE := T; end;
procedure TFORMTILEMODE_R(Self: TFORM; var T: TTILEMODE); begin T := Self.TILEMODE; end;
{$ENDIF}{CLX}
procedure TFORMACTIVEMDICHILD_R(Self: TFORM; var T: TFORM); begin T := Self.ACTIVEMDICHILD; end;
procedure TFORMDROPTARGET_W(Self: TFORM; T: BOOLEAN); begin Self.DROPTARGET := T; end;
procedure TFORMDROPTARGET_R(Self: TFORM; var T: BOOLEAN); begin T := Self.DROPTARGET; end;
procedure TFORMMDICHILDCOUNT_R(Self: TFORM; var T: INTEGER); begin T := Self.MDICHILDCOUNT; end;
procedure TFORMMDICHILDREN_R(Self: TFORM; var T: TFORM; t1: INTEGER); begin T := Self.MDICHILDREN[T1]; 
end;
{$ENDIF}{FPC}

procedure TFORMMODALRESULT_W(Self: TFORM; T: TMODALRESULT); begin Self.MODALRESULT := T; end;
procedure TFORMMODALRESULT_R(Self: TFORM; var T: TMODALRESULT); begin T := Self.MODALRESULT; end;
procedure TFORMACTIVE_R(Self: TFORM; var T: BOOLEAN); begin T := Self.ACTIVE; end;
procedure TFORMCANVAS_R(Self: TFORM; var T: TCANVAS); begin T := Self.CANVAS; end;
{$IFNDEF CLX}
procedure TFORMCLIENTHANDLE_R(Self: TFORM; var T: Longint); begin T := Self.CLIENTHANDLE; end;
{$ENDIF}

{ Innerfuse Pascal Script Class Import Utility (runtime) }

procedure RIRegisterTFORM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TFORM) do
  begin
    {$IFDEF DELPHI4UP}
    RegisterVirtualConstructor(@TFORM.CREATENEW, 'CreateNew');
    {$ELSE}
    RegisterConstructor(@TFORM.CREATENEW, 'CreateNew');
    {$ENDIF}
    RegisterMethod(@TFORM.CLOSE, 'Close');
    RegisterMethod(@TFORM.HIDE, 'Hide');
    RegisterMethod(@TFORM.SHOW, 'Show');
    {$IFDEF DELPHI_SEATTLE_UP}
    RegisterVirtualMethod(@TFORM.SHOWMODAL, 'ShowModal');
    {$ELSE}
    RegisterMethod(@TFORM.SHOWMODAL, 'ShowModal');
    {$ENDIF}
    RegisterMethod(@TFORM.RELEASE, 'Release');
    RegisterPropertyHelper(@TFORMACTIVE_R, nil, 'Active');

    {$IFNDEF PS_MINIVCL}
 {$IFNDEF FPC}
{$IFNDEF CLX} 
    RegisterMethod(@TFORM.ARRANGEICONS, 'ArrangeIcons');
    RegisterMethod(@TFORM.GETFORMIMAGE, 'GetFormImage');
    RegisterMethod(@TFORM.PRINT, 'Print');
    RegisterMethod(@TFORM.SENDCANCELMODE, 'SendCancelMode');
    RegisterPropertyHelper(@TFORMACTIVEOLECONTROL_R, @TFORMACTIVEOLECONTROL_W, 'ActiveOleControl');
    RegisterPropertyHelper(@TFORMCLIENTHANDLE_R, nil, 'ClientHandle');
    RegisterPropertyHelper(@TFORMTILEMODE_R, @TFORMTILEMODE_W, 'TileMode');
{$ENDIF}{CLX}
    RegisterMethod(@TFORM.CASCADE, 'Cascade');
    RegisterMethod(@TFORM.NEXT, 'Next');
    RegisterMethod(@TFORM.PREVIOUS, 'Previous');
    RegisterMethod(@TFORM.TILE, 'Tile');
    RegisterPropertyHelper(@TFORMACTIVEMDICHILD_R, nil, 'ActiveMDIChild');
    RegisterPropertyHelper(@TFORMDROPTARGET_R, @TFORMDROPTARGET_W, 'DropTarget');
    RegisterPropertyHelper(@TFORMMDICHILDCOUNT_R, nil, 'MDIChildCount');
    RegisterPropertyHelper(@TFORMMDICHILDREN_R, nil, 'MDIChildren');
 {$ENDIF}{FPC}
    RegisterMethod(@TFORM.CLOSEQUERY, 'CloseQuery');
    RegisterMethod(@TFORM.DEFOCUSCONTROL, 'DefocusControl');
    RegisterMethod(@TFORM.FOCUSCONTROL, 'FocusControl');
    RegisterMethod(@TFORM.SETFOCUSEDCONTROL, 'SetFocusedControl');
    RegisterPropertyHelper(@TFORMCANVAS_R, nil, 'Canvas');
    RegisterPropertyHelper(@TFORMMODALRESULT_R, @TFORMMODALRESULT_W, 'ModalResult');
    {$ENDIF}{PS_MINIVCL}
  end;
end;

 {$IFNDEF FPC}
procedure TAPPLICATIONACTIVE_R(Self: TAPPLICATION; var T: BOOLEAN); begin T := Self.ACTIVE; end;
{$IFNDEF CLX}
procedure TAPPLICATIONDIALOGHANDLE_R(Self: TAPPLICATION; var T: Longint); begin T := Self.DIALOGHANDLE; end;
procedure TAPPLICATIONDIALOGHANDLE_W(Self: TAPPLICATION; T: Longint); begin Self.DIALOGHANDLE := T; end;
procedure TAPPLICATIONHANDLE_R(Self: TAPPLICATION; var T: Longint); begin T := Self.HANDLE; end;
procedure TAPPLICATIONHANDLE_W(Self: TAPPLICATION; T: Longint); begin Self.HANDLE := T; end;
procedure TAPPLICATIONUPDATEFORMATSETTINGS_R(Self: TAPPLICATION; var T: BOOLEAN); begin T := Self.UPDATEFORMATSETTINGS; end;
procedure TAPPLICATIONUPDATEFORMATSETTINGS_W(Self: TAPPLICATION; T: BOOLEAN); begin Self.UPDATEFORMATSETTINGS := T; end;
{$ENDIF}
{$ENDIF}{FPC}


procedure TAPPLICATIONEXENAME_R(Self: TAPPLICATION; var T: STRING); begin T := Self.EXENAME; end;
procedure TAPPLICATIONHELPFILE_R(Self: TAPPLICATION; var T: STRING); begin T := Self.HELPFILE; end;
procedure TAPPLICATIONHELPFILE_W(Self: TAPPLICATION; T: STRING); begin Self.HELPFILE := T; end;
procedure TAPPLICATIONHINT_R(Self: TAPPLICATION; var T: STRING); begin T := Self.HINT; end;
procedure TAPPLICATIONHINT_W(Self: TAPPLICATION; T: STRING); begin Self.HINT := T; end;
procedure TAPPLICATIONHINTCOLOR_R(Self: TAPPLICATION; var T: TCOLOR); begin T := Self.HINTCOLOR; end;
procedure TAPPLICATIONHINTCOLOR_W(Self: TAPPLICATION; T: TCOLOR); begin Self.HINTCOLOR := T; end;
procedure TAPPLICATIONHINTPAUSE_R(Self: TAPPLICATION; var T: INTEGER); begin T := Self.HINTPAUSE; end;
procedure TAPPLICATIONHINTPAUSE_W(Self: TAPPLICATION; T: INTEGER); begin Self.HINTPAUSE := T; end;
procedure TAPPLICATIONHINTSHORTPAUSE_R(Self: TAPPLICATION; var T: INTEGER); begin T := Self.HINTSHORTPAUSE; end;
procedure TAPPLICATIONHINTSHORTPAUSE_W(Self: TAPPLICATION; T: INTEGER); begin Self.HINTSHORTPAUSE := T; end;
procedure TAPPLICATIONHINTHIDEPAUSE_R(Self: TAPPLICATION; var T: INTEGER); begin T := Self.HINTHIDEPAUSE; end;
procedure TAPPLICATIONHINTHIDEPAUSE_W(Self: TAPPLICATION; T: INTEGER); begin Self.HINTHIDEPAUSE := T; end;
procedure TAPPLICATIONMAINFORM_R(Self: TAPPLICATION; var T: {$IFDEF DELPHI3UP}TCustomForm{$ELSE}TFORM{$ENDIF}); begin T := Self.MAINFORM; end;
procedure TAPPLICATIONSHOWHINT_R(Self: TAPPLICATION; var T: BOOLEAN); begin T := Self.SHOWHINT; end;
procedure TAPPLICATIONSHOWHINT_W(Self: TAPPLICATION; T: BOOLEAN); begin Self.SHOWHINT := T; end;
procedure TAPPLICATIONSHOWMAINFORM_R(Self: TAPPLICATION; var T: BOOLEAN); begin T := Self.SHOWMAINFORM; end;
procedure TAPPLICATIONSHOWMAINFORM_W(Self: TAPPLICATION; T: BOOLEAN); begin Self.SHOWMAINFORM := T; end;
procedure TAPPLICATIONTERMINATED_R(Self: TAPPLICATION; var T: BOOLEAN); begin T := Self.TERMINATED; end;
procedure TAPPLICATIONTITLE_R(Self: TAPPLICATION; var T: STRING); begin T := Self.TITLE; end;
procedure TAPPLICATIONTITLE_W(Self: TAPPLICATION; T: STRING); begin Self.TITLE := T; end;

{$IFNDEF FPC}
procedure TAPPLICATIONONACTIVATE_R(Self: TAPPLICATION; var T: TNOTIFYEVENT); begin T := Self.ONACTIVATE; end;
procedure TAPPLICATIONONACTIVATE_W(Self: TAPPLICATION; T: TNOTIFYEVENT); begin Self.ONACTIVATE := T; end;
procedure TAPPLICATIONONDEACTIVATE_R(Self: TAPPLICATION; var T: TNOTIFYEVENT); begin T := Self.ONDEACTIVATE; end;
procedure TAPPLICATIONONDEACTIVATE_W(Self: TAPPLICATION; T: TNOTIFYEVENT); begin Self.ONDEACTIVATE := T; end;
{$ENDIF}

procedure TAPPLICATIONONIDLE_R(Self: TAPPLICATION; var T: TIDLEEVENT); begin T := Self.ONIDLE; end;
procedure TAPPLICATIONONIDLE_W(Self: TAPPLICATION; T: TIDLEEVENT); begin Self.ONIDLE := T; end;
procedure TAPPLICATIONONHELP_R(Self: TAPPLICATION; var T: THELPEVENT); begin T := Self.ONHELP; end;
procedure TAPPLICATIONONHELP_W(Self: TAPPLICATION; T: THELPEVENT); begin Self.ONHELP := T; end;
procedure TAPPLICATIONONHINT_R(Self: TAPPLICATION; var T: TNOTIFYEVENT); begin T := Self.ONHINT; end;
procedure TAPPLICATIONONHINT_W(Self: TAPPLICATION; T: TNOTIFYEVENT); begin Self.ONHINT := T; end;

{$IFNDEF FPC}
procedure TAPPLICATIONONMINIMIZE_R(Self: TAPPLICATION; var T: TNOTIFYEVENT); begin T := Self.ONMINIMIZE; end;
procedure TAPPLICATIONONMINIMIZE_W(Self: TAPPLICATION; T: TNOTIFYEVENT); begin Self.ONMINIMIZE := T; end;

procedure TAPPLICATIONONRESTORE_R(Self: TAPPLICATION; var T: TNOTIFYEVENT); begin T := Self.ONRESTORE; end;
procedure TAPPLICATIONONRESTORE_W(Self: TAPPLICATION; T: TNOTIFYEVENT); begin Self.ONRESTORE := T; end;
{$ENDIF}

procedure RIRegisterTAPPLICATION(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TAPPLICATION) do
  begin
 {$IFNDEF FPC}
    RegisterMethod(@TAPPLICATION.MINIMIZE, 'Minimize');
    RegisterMethod(@TAPPLICATION.RESTORE, 'Restore');
    RegisterPropertyHelper(@TAPPLICATIONACTIVE_R, nil, 'Active');
    RegisterPropertyHelper(@TAPPLICATIONONACTIVATE_R, @TAPPLICATIONONACTIVATE_W, 'OnActivate');
    RegisterPropertyHelper(@TAPPLICATIONONDEACTIVATE_R, @TAPPLICATIONONDEACTIVATE_W, 'OnDeactivate');
    RegisterPropertyHelper(@TAPPLICATIONONMINIMIZE_R, @TAPPLICATIONONMINIMIZE_W, 'OnMinimize');
    RegisterPropertyHelper(@TAPPLICATIONONRESTORE_R, @TAPPLICATIONONRESTORE_W, 'OnRestore');
    RegisterPropertyHelper(@TAPPLICATIONDIALOGHANDLE_R, @TAPPLICATIONDIALOGHANDLE_W, 'DialogHandle');
    RegisterMethod(@TAPPLICATION.CREATEHANDLE, 'CreateHandle');
    RegisterMethod(@TAPPLICATION.NORMALIZETOPMOSTS, 'NormalizeTopMosts');
    RegisterMethod(@TAPPLICATION.RESTORETOPMOSTS, 'RestoreTopMosts');
    {$IFNDEF CLX}
    RegisterPropertyHelper(@TAPPLICATIONHANDLE_R, @TAPPLICATIONHANDLE_W, 'Handle');
    RegisterPropertyHelper(@TAPPLICATIONUPDATEFORMATSETTINGS_R, @TAPPLICATIONUPDATEFORMATSETTINGS_W, 'UpdateFormatSettings');
    {$ENDIF}
 {$ENDIF}
    RegisterMethod(@TAPPLICATION.BRINGTOFRONT, 'BringToFront');
    RegisterMethod(@TAPPLICATION.MESSAGEBOX, 'MessageBox');
    RegisterMethod(@TAPPLICATION.PROCESSMESSAGES, 'ProcessMessages');
    RegisterMethod(@TAPPLICATION.TERMINATE, 'Terminate');
    RegisterPropertyHelper(@TAPPLICATIONEXENAME_R, nil, 'ExeName');
    RegisterPropertyHelper(@TAPPLICATIONHINT_R, @TAPPLICATIONHINT_W, 'Hint');
    RegisterPropertyHelper(@TAPPLICATIONMAINFORM_R, nil, 'MainForm');
    RegisterPropertyHelper(@TAPPLICATIONSHOWHINT_R, @TAPPLICATIONSHOWHINT_W, 'ShowHint');
    RegisterPropertyHelper(@TAPPLICATIONSHOWMAINFORM_R, @TAPPLICATIONSHOWMAINFORM_W, 'ShowMainForm');
    RegisterPropertyHelper(@TAPPLICATIONTERMINATED_R, nil, 'Terminated');
    RegisterPropertyHelper(@TAPPLICATIONTITLE_R, @TAPPLICATIONTITLE_W, 'Title');
    RegisterPropertyHelper(@TAPPLICATIONONIDLE_R, @TAPPLICATIONONIDLE_W, 'OnIdle');
    RegisterPropertyHelper(@TAPPLICATIONONHINT_R, @TAPPLICATIONONHINT_W, 'OnHint');
    {$IFNDEF PS_MINIVCL}
    RegisterMethod(@TAPPLICATION.CONTROLDESTROYED, 'ControlDestroyed');
    RegisterMethod(@TAPPLICATION.CANCELHINT, 'CancelHint');
    {$IFNDEF CLX}
    {$IFNDEF FPC}
    RegisterMethod(@TAPPLICATION.HELPCOMMAND, 'HelpCommand');
    {$ENDIF}
    RegisterMethod(@TAPPLICATION.HELPCONTEXT, 'HelpContext');
    {$IFNDEF FPC}
    RegisterMethod(@TAPPLICATION.HELPJUMP, 'HelpJump');
    {$ENDIF}
    {$ENDIF}
//    RegisterMethod(@TAPPLICATION.HANDLEEXCEPTION, 'HandleException');
//    RegisterMethod(@TAPPLICATION.HOOKMAINWINDOW, 'HookMainWindow');
//    RegisterMethod(@TAPPLICATION.UNHOOKMAINWINDOW, 'UnhookMainWindow');

    RegisterMethod(@TAPPLICATION.HANDLEMESSAGE, 'HandleMessage');
    RegisterMethod(@TAPPLICATION.HIDEHINT, 'HideHint');
    RegisterMethod(@TAPPLICATION.HINTMOUSEMESSAGE, 'HintMouseMessage');
    RegisterMethod(@TAPPLICATION.INITIALIZE, 'Initialize');
    RegisterMethod(@TAPPLICATION.RUN, 'Run');
//    RegisterMethod(@TAPPLICATION.SHOWEXCEPTION, 'ShowException');
    RegisterPropertyHelper(@TAPPLICATIONHELPFILE_R, @TAPPLICATIONHELPFILE_W, 'HelpFile');
    RegisterPropertyHelper(@TAPPLICATIONHINTCOLOR_R, @TAPPLICATIONHINTCOLOR_W, 'HintColor');
    RegisterPropertyHelper(@TAPPLICATIONHINTPAUSE_R, @TAPPLICATIONHINTPAUSE_W, 'HintPause');
    RegisterPropertyHelper(@TAPPLICATIONHINTSHORTPAUSE_R, @TAPPLICATIONHINTSHORTPAUSE_W, 'HintShortPause');
    RegisterPropertyHelper(@TAPPLICATIONHINTHIDEPAUSE_R, @TAPPLICATIONHINTHIDEPAUSE_W, 'HintHidePause');
    RegisterPropertyHelper(@TAPPLICATIONONHELP_R, @TAPPLICATIONONHELP_W, 'OnHelp');
    {$ENDIF}
  end;
end;

procedure RIRegister_Forms(Cl: TPSRuntimeClassImporter);
begin
  {$IFNDEF PS_MINIVCL}
  RIRegisterTCONTROLSCROLLBAR(cl);
  RIRegisterTSCROLLBOX(cl);
  {$ENDIF}
{$IFNDEF FPC}  RIRegisterTScrollingWinControl(cl);{$ENDIF}
  RIRegisterTForm(Cl);
  {$IFNDEF PS_MINIVCL}
  RIRegisterTApplication(Cl);
  {$ENDIF}
end;


// PS_MINIVCL changes by Martijn Laan
// FPC changes by Boguslaw brandys (brandys at o2 _dot_ pl)

end.





