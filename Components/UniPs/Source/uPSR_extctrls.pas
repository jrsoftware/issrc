
unit uPSR_extctrls;

{$I PascalScript.inc}
interface
uses
  uPSRuntime, uPSUtils;


procedure RIRegister_ExtCtrls(cl: TPSRuntimeClassImporter);

procedure RIRegisterTBEVEL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCUSTOMPANEL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTPANEL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCUSTOMLINKLABEL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTLINKLABEL(Cl: TPSRuntimeClassImporter);

implementation

uses
  ExtCtrls;

procedure RIRegisterTBEVEL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TBEVEL);
end;

procedure RIRegisterTCUSTOMPANEL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TCUSTOMPANEL);
end;

procedure RIRegisterTPANEL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TPANEL);
end;

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

procedure RIRegister_ExtCtrls(cl: TPSRuntimeClassImporter);
begin
  RIRegisterTBEVEL(Cl);
  RIRegisterTCUSTOMPANEL(Cl);
  RIRegisterTPANEL(Cl);
  RIRegisterTCUSTOMLINKLABEL(Cl);
  RIRegisterTLINKLABEL(Cl);
end;

end.


