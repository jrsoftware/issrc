{ compiletime ComObj support }
unit uPSC_comobj;

{$I PascalScript.inc}
interface
uses
  uPSCompiler, uPSUtils;

{
 
Will register:
 
function CreateOleObject(const ClassName: String): IDispatch;
function GetActiveOleObject(const ClassName: String): IDispatch;

}

procedure SIRegister_ComObj(cl: TPSPascalCompiler);

implementation

procedure SIRegister_ComObj(cl: TPSPascalCompiler);
begin
{$IFDEF FPC}
    {$IFDEF PS_FPC_HAS_COM}
    cl.AddTypeS('HResult', 'LongInt');
    cl.AddTypeS('TGUID', 'record D1: LongWord; D2: Word; D3: Word; D4: array[0..7] of Byte; end;');
    cl.AddTypeS('TCLSID', 'TGUID');
    cl.AddTypeS('TIID', 'TGUID');
    cl.AddDelphiFunction('procedure OleCheck(Result: HResult);');
    cl.AddDelphiFunction('function StringToGUID(const S: string): TGUID;');
    cl.AddDelphiFunction('function CreateComObject(const ClassID: TGUID): IUnknown;');
    cl.AddDelphiFunction('function CreateOleObject(const ClassName: string): IDispatch;');
    cl.AddDelphiFunction('function GetActiveOleObject(const ClassName: string): IDispatch;');
    {$ENDIF}
{$ELSE}
  cl.AddTypeS('HResult', 'LongInt');
  cl.AddTypeS('TGUID', 'record D1: LongWord; D2: Word; D3: Word; D4: array[0..7] of Byte; end;');
  cl.AddTypeS('TCLSID', 'TGUID');
  cl.AddTypeS('TIID', 'TGUID');
  cl.AddDelphiFunction('procedure OleCheck(Result: HResult);');
{$IFNDEF PS_NOINTERFACES}
{$IFDEF DELPHI3UP}
  cl.AddDelphiFunction('function StringToGUID(const S: string): TGUID;');
  cl.AddDelphiFunction('function CreateComObject(const ClassID: TGUID): IUnknown;');
{$ENDIF}
{$ENDIF}
  cl.AddDelphiFunction('function CreateOleObject(const ClassName: string): IDispatch;');
  cl.AddDelphiFunction('function GetActiveOleObject(const ClassName: string): IDispatch;');
{$ENDIF}  
end;

end.
