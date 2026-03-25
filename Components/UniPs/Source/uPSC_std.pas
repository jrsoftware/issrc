{ Compiletime TObject, TPersistent and TComponent definitions }
unit uPSC_std;
{$I PascalScript.inc}
interface
uses
  uPSCompiler, uPSUtils;

{
  Will register files from:
    System
    Classes (Only TComponent and TPersistent)

}

procedure SIRegister_Std_TypesAndConsts(Cl: TPSPascalCompiler);
procedure SIRegisterTObject(CL: TPSPascalCompiler);
procedure SIRegisterTPersistent(Cl: TPSPascalCompiler);
procedure SIRegisterTComponent(Cl: TPSPascalCompiler);

procedure SIRegister_Std(Cl: TPSPascalCompiler);

implementation

procedure SIRegisterTObject(CL: TPSPascalCompiler);
begin
  with Cl.AddClassN(nil, 'TObject') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('procedure Free');
  end;
end;

procedure SIRegisterTPersistent(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TObject'), 'TPersistent') do
  begin
    RegisterMethod('procedure Assign(Source: TPersistent)');
  end;
end;

procedure SIRegisterTComponent(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TComponent') do
  begin
    RegisterMethod('function FindComponent(AName: string): TComponent;');
    RegisterMethod('constructor Create(AOwner: TComponent); virtual;');

    RegisterProperty('Owner', 'TComponent', iptRW);
    RegisterMethod('procedure DestroyComponents');
    RegisterMethod('procedure Destroying');
    RegisterMethod('procedure FreeNotification(AComponent: TComponent)');
    RegisterMethod('procedure InsertComponent(AComponent: TComponent)');
    RegisterMethod('procedure RemoveComponent(AComponent: TComponent)');
    RegisterProperty('Components', 'TComponent Integer', iptr);
    RegisterProperty('ComponentCount', 'Integer', iptr);
    RegisterProperty('ComponentIndex', 'Integer', iptrw);
    RegisterProperty('ComponentState', 'Byte', iptr);
    RegisterProperty('DesignInfo', 'LongInt', iptrw);
    RegisterProperty('Name', 'string', iptrw);
    RegisterProperty('Tag', 'LongInt', iptrw);
  end;
end;




procedure SIRegister_Std_TypesAndConsts(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TComponentStateE', '(csLoading, csReading, csWriting, csDestroying, csDesigning, csAncestor, csUpdating, csFixups, csFreeNotification, csInline, csDesignInstance)');
  cl.AddTypeS('TComponentState', 'set of TComponentStateE');
  Cl.AddTypeS('TRect', 'record Left, Top, Right, Bottom: Integer; end;');
end;

procedure SIRegister_Std(Cl: TPSPascalCompiler);
begin
  SIRegister_Std_TypesAndConsts(Cl);
  SIRegisterTObject(CL);
  SIRegisterTPersistent(Cl);
  SIRegisterTComponent(Cl);
end;

// PS_MINIVCL changes by Martijn Laan


End.


