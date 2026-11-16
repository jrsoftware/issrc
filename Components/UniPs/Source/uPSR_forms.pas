
unit uPSR_forms;

{$I PascalScript.inc}
interface
uses
  uPSRuntime, uPSUtils;

procedure RIRegisterTSCROLLINGWINCONTROL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFORM(Cl: TPSRuntimeClassImporter);

procedure RIRegister_Forms(Cl: TPSRuntimeClassImporter);

implementation
uses
  sysutils, Forms;

procedure RIRegisterTSCROLLINGWINCONTROL(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSCROLLINGWINCONTROL) do
  begin
    RegisterMethod(@TSCROLLINGWINCONTROL.SCROLLINVIEW, 'ScrollInView');
  end;
end;

procedure TFORMACTIVE_R(Self: TFORM; var T: BOOLEAN); begin T := Self.ACTIVE; end;

{ Innerfuse Pascal Script Class Import Utility (runtime) }

procedure RIRegisterTFORM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TFORM) do
  begin
    RegisterVirtualConstructor(@TFORM.CREATENEW, 'CreateNew');
    RegisterMethod(@TFORM.CLOSE, 'Close');
    RegisterMethod(@TFORM.HIDE, 'Hide');
    RegisterMethod(@TFORM.SHOW, 'Show');
    RegisterVirtualMethod(@TFORM.SHOWMODAL, 'ShowModal');
    RegisterMethod(@TFORM.RELEASE, 'Release');
    RegisterPropertyHelper(@TFORMACTIVE_R, nil, 'Active');
  end;
end;

procedure RIRegister_Forms(Cl: TPSRuntimeClassImporter);
begin
  RIRegisterTScrollingWinControl(cl);
  RIRegisterTForm(Cl);
end;

end.





