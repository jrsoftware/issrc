

unit uPSR_comobj;

{$I PascalScript.inc}
interface
uses
  uPSRuntime, uPSUtils;


procedure RIRegister_ComObj(cl: TPSExec);

implementation
uses ComObj;


procedure RIRegister_ComObj(cl: TPSExec);
begin
  cl.RegisterDelphiFunction(@OleCheck, 'OleCheck', cdRegister);
{$IFNDEF PS_NOINTERFACES}
  cl.RegisterDelphiFunction(@StringToGUID, 'StringToGUID', cdRegister);
  cl.RegisterDelphiFunction(@CreateComObject, 'CreateComObject', cdRegister);
{$ENDIF}
  cl.RegisterDelphiFunction(@CreateOleObject, 'CreateOleObject', cdRegister);
  cl.RegisterDelphiFunction(@GetActiveOleObject, 'GetActiveOleObject', cdRegister);
end;

end.
