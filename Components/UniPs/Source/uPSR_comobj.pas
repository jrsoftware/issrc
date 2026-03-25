

unit uPSR_comobj;

{$I PascalScript.inc}
interface
uses
  uPSRuntime, uPSUtils;


procedure RIRegister_ComObj(cl: TPSExec);

implementation
{$IFDEF FPC}
  {$IFDEF PS_FPC_HAS_COM}
   uses SysUtils, ComObj;
  {$ENDIF}
{$ELSE}
  {$IFDEF DELPHI3UP}
   uses ComObj;
  {$ELSE}
   uses SysUtils, Ole2;
  {$ENDIF}
{$ENDIF}
{$IFNDEF DELPHI3UP}

{$IFDEF DELPHI3UP }
resourceString
{$ELSE }
const
{$ENDIF }

  RPS_OLEError = 'OLE error %.8x';
function OleErrorMessage(ErrorCode: HResult): String;
begin
  Result := SysErrorMessage(ErrorCode);
  if Result = '' then
    Result := Format(RPS_OLEError, [ErrorCode]);
end;

procedure OleError(ErrorCode: HResult);
begin
  raise Exception.Create(OleErrorMessage(ErrorCode));
end;

procedure OleCheck(Result: HResult);
begin
  if Result < 0 then OleError(Result);
end;

procedure CreateOleObject(const ClassName: string; var Disp: IDispatch);
var
  OldDisp: IDispatch;
  ClassID: TCLSID;
  WideCharBuf: array[0..127] of WideChar;
begin
  StringToWideChar(ClassName, WideCharBuf, SizeOf(WideCharBuf) div SizeOf(WideCharBuf[0]));
  OleCheck(CLSIDFromProgID(WideCharBuf, ClassID));
  if Disp <> nil then
  begin
    OldDisp := Disp;
    Disp := nil;
    OldDisp.Release;
  end;
  OleCheck(CoCreateInstance(ClassID, nil, CLSCTX_INPROC_SERVER or
    CLSCTX_LOCAL_SERVER, IID_IDispatch, Disp));
end;

procedure GetActiveOleObject(const ClassName: string; var Disp: IDispatch);
var
  Unknown: IUnknown;
  OldDisp: IDispatch;
  ClassID: TCLSID;
  WideCharBuf: array[0..127] of WideChar;
begin
  StringToWideChar(ClassName, WideCharBuf, SizeOf(WideCharBuf) div SizeOf(WideCharBuf[0]));
  OleCheck(CLSIDFromProgID(WideCharBuf, ClassID));
  OleCheck(GetActiveObject(ClassID, nil, Unknown));
  try
    if Disp <> nil then
    begin
      OldDisp := Disp;
      Disp := nil;
      OldDisp.Release;
    end;
    OleCheck(Unknown.QueryInterface(IID_IDispatch, Disp));
  finally
    Unknown.Release;
  end;
end;

{$ENDIF}


procedure RIRegister_ComObj(cl: TPSExec);
begin
{$IFDEF FPC}
    {$IFDEF PS_FPC_HAS_COM}
    cl.RegisterDelphiFunction(@OleCheck, 'OleCheck', cdRegister);
    cl.RegisterDelphiFunction(@StringToGUID, 'StringToGUID', cdRegister);
    cl.RegisterDelphiFunction(@CreateComObject, 'CreateComObject', cdRegister);
    cl.RegisterDelphiFunction(@CreateOleObject, 'CreateOleObject', cdRegister);
    cl.RegisterDelphiFunction(@GetActiveOleObject, 'GetActiveOleObject', cdRegister);
    {$ENDIF}
{$ELSE}
  cl.RegisterDelphiFunction(@OleCheck, 'OleCheck', cdRegister);
{$IFNDEF PS_NOINTERFACES}
{$IFDEF DELPHI3UP}
  cl.RegisterDelphiFunction(@StringToGUID, 'StringToGUID', cdRegister);
  cl.RegisterDelphiFunction(@CreateComObject, 'CreateComObject', cdRegister);
{$ENDIF}
{$ENDIF}
  cl.RegisterDelphiFunction(@CreateOleObject, 'CreateOleObject', cdRegister);
  cl.RegisterDelphiFunction(@GetActiveOleObject, 'GetActiveOleObject', cdRegister);
{$ENDIF}  
end;

end.
