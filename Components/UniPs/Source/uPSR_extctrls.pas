
unit uPSR_extctrls;

{$I PascalScript.inc}
interface
uses
  uPSRuntime, uPSUtils;


procedure RIRegister_ExtCtrls(cl: TPSRuntimeClassImporter);

procedure RIRegisterTSHAPE(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTIMAGE(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTPAINTBOX(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTBEVEL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTTIMER(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCUSTOMPANEL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTPANEL(Cl: TPSRuntimeClassImporter);
{$IFNDEF CLX}
procedure RIRegisterTPAGE(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTNOTEBOOK(Cl: TPSRuntimeClassImporter);
{$IFNDEF FPC}procedure RIRegisterTHEADER(Cl: TPSRuntimeClassImporter);{$ENDIF}
{$ENDIF}
procedure RIRegisterTCUSTOMRADIOGROUP(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTRADIOGROUP(Cl: TPSRuntimeClassImporter);
{$IFDEF DELPHI14UP}
procedure RIRegisterTCUSTOMLINKLABEL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTLINKLABEL(Cl: TPSRuntimeClassImporter);
{$ENDIF}

implementation

uses
  {$IFDEF CLX}
  QExtCtrls, QGraphics;
  {$ELSE}
  ExtCtrls, Graphics;
  {$ENDIF}

procedure RIRegisterTSHAPE(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSHAPE) do
  begin
    {$IFNDEF PS_MINIVCL}
    RegisterMethod(@TSHAPE.STYLECHANGED, 'StyleChanged');
    {$ENDIF}
  end;
end;

procedure TIMAGECANVAS_R(Self: TIMAGE; var T: TCANVAS); begin T := Self.CANVAS; end;

procedure RIRegisterTIMAGE(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TIMAGE) do
  begin
    RegisterPropertyHelper(@TIMAGECANVAS_R, nil, 'Canvas');
  end;
end;

procedure TPAINTBOXCANVAS_R(Self: TPAINTBOX; var T: TCanvas); begin T := Self.CANVAS; end;

procedure RIRegisterTPAINTBOX(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TPAINTBOX) do
  begin
    RegisterPropertyHelper(@TPAINTBOXCANVAS_R, nil, 'Canvas');
  end;
end;

procedure RIRegisterTBEVEL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TBEVEL);
end;

procedure RIRegisterTTIMER(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TTIMER);
end;

procedure RIRegisterTCUSTOMPANEL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TCUSTOMPANEL);
end;

procedure RIRegisterTPANEL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TPANEL);
end;
{$IFNDEF CLX}
procedure RIRegisterTPAGE(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TPAGE);
end;

procedure RIRegisterTNOTEBOOK(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TNOTEBOOK);
end;

{$IFNDEF FPC}
procedure THEADERSECTIONWIDTH_R(Self: THEADER; var T: INTEGER; t1: INTEGER); begin T := Self.SECTIONWIDTH[t1]; end;
procedure THEADERSECTIONWIDTH_W(Self: THEADER; T: INTEGER; t1: INTEGER); begin Self.SECTIONWIDTH[t1] := T; end;

procedure RIRegisterTHEADER(Cl: TPSRuntimeClassImporter);
begin
	with Cl.Add(THEADER) do
	begin
		RegisterPropertyHelper(@THEADERSECTIONWIDTH_R, @THEADERSECTIONWIDTH_W, 'SectionWidth');
	end;
end;
{$ENDIF}
{$ENDIF}

procedure RIRegisterTCUSTOMRADIOGROUP(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TCUSTOMRADIOGROUP);
end;

procedure RIRegisterTRADIOGROUP(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TRADIOGROUP);
end;

{$IFDEF DELPHI14UP}

procedure TCUSTOMLINKLABELALIGNMENT_R(Self: TCUSTOMLINKLABEL; var T: TCustomLinkLabel.TLinkAlignment); begin T := Self.ALIGNMENT; end;
procedure TCUSTOMLINKLABELALIGNMENT_W(Self: TCUSTOMLINKLABEL; T: TCustomLinkLabel.TLinkAlignment); begin
Self.ALIGNMENT := T;
end;
procedure TCUSTOMLINKLABELAUTOSIZE_R(Self: TCUSTOMLINKLABEL; var T: Boolean); begin T := Self.AUTOSIZE; end;
procedure TCUSTOMLINKLABELAUTOSIZE_W(Self: TCUSTOMLINKLABEL; T: Boolean); begin Self.AUTOSIZE := T; end;
procedure TCUSTOMLINKLABELUSEVISUALSTYLE_R(Self: TCUSTOMLINKLABEL; var T: Boolean); begin T := Self.USEVISUALSTYLE; end;
procedure TCUSTOMLINKLABELUSEVISUALSTYLE_W(Self: TCUSTOMLINKLABEL; T: Boolean); begin Self.USEVISUALSTYLE := T; end;
procedure TCUSTOMLINKLABELONLINKCLICK_R(Self: TCUSTOMLINKLABEL; var T: TSysLinkEvent); begin T := Self.ONLINKCLICK; end;
procedure TCUSTOMLINKLABELONLINKCLICK_W(Self: TCUSTOMLINKLABEL; T: TSysLinkEvent); begin Self.ONLINKCLICK := T; end;

procedure RIRegisterTCUSTOMLINKLABEL(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCUSTOMLINKLABEL) do
  begin
    RegisterPropertyHelper(@TCUSTOMLINKLABELALIGNMENT_R, @TCUSTOMLINKLABELALIGNMENT_W, 'Alignment');
    RegisterPropertyHelper(@TCUSTOMLINKLABELAUTOSIZE_R, @TCUSTOMLINKLABELAUTOSIZE_W, 'AutoSize');
    RegisterPropertyHelper(@TCUSTOMLINKLABELUSEVISUALSTYLE_R, @TCUSTOMLINKLABELUSEVISUALSTYLE_W, 'UseVisualStyle');
    RegisterPropertyHelper(@TCUSTOMLINKLABELONLINKCLICK_R, @TCUSTOMLINKLABELONLINKCLICK_W, 'OnLinkClick');
  end;
end;

procedure RIRegisterTLINKLABEL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TLINKLABEL);
end;

{$ENDIF}

procedure RIRegister_ExtCtrls(cl: TPSRuntimeClassImporter);
begin
  {$IFNDEF PS_MINIVCL}
  RIRegisterTSHAPE(Cl);
  RIRegisterTIMAGE(Cl);
  RIRegisterTPAINTBOX(Cl);
  {$ENDIF}
  RIRegisterTBEVEL(Cl);
  {$IFNDEF PS_MINIVCL}
  RIRegisterTTIMER(Cl);
  {$ENDIF}
  RIRegisterTCUSTOMPANEL(Cl);
  {$IFNDEF CLX}
  RIRegisterTPANEL(Cl);
  {$ENDIF}
  {$IFNDEF PS_MINIVCL}
  {$IFNDEF CLX}
  RIRegisterTPAGE(Cl);
	RIRegisterTNOTEBOOK(Cl);
  {$IFNDEF FPC}
	RIRegisterTHEADER(Cl);
  {$ENDIF}{FPC}
  {$ENDIF}
  RIRegisterTCUSTOMRADIOGROUP(Cl);
  RIRegisterTRADIOGROUP(Cl);
  {$ENDIF}
  {$IFDEF DELPHI14UP}
  RIRegisterTCUSTOMLINKLABEL(Cl);
  RIRegisterTLINKLABEL(Cl);
  {$ENDIF}
end;

end.


