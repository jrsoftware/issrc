{ Compiletime DLL importing support }
unit uPSC_dll;

{$I PascalScript.inc}
interface
{

  Function FindWindow(c1, c2: PChar): Cardinal; external 'FindWindow@user32.dll stdcall';

}
uses
  uPSCompiler, uPSUtils;

  
{$IFDEF DELPHI3UP }
resourceString
{$ELSE }
const
{$ENDIF }

  RPS_Invalid_External = 'Invalid External';
  RPS_InvalidCallingConvention = 'Invalid Calling Convention';



function DllExternalProc(Sender: TPSPascalCompiler; Decl: TPSParametersDecl; const OriginalName, FExternal: tbtstring): TPSRegProc;
type

  TDllCallingConvention = (clRegister
  , clPascal
  , ClCdecl
  , ClStdCall
  );

var
  DefaultCC: TDllCallingConvention;

procedure RegisterDll_Compiletime(cs: TPSPascalCompiler);

implementation

function rpos(ch: tbtchar; const s: tbtstring): Longint;
var
  i: Longint;
begin
  for i := length(s) downto 1 do
  if s[i] = ch then begin Result := i; exit; end;
  result := 0;
end;

function RemoveQuotes(s: tbtstring): tbtstring;
begin
  result := s;
  if result = '' then exit;
  if Result[1] = '"' then delete(result ,1,1);
  if (Result <> '') and (Result[Length(result)] = '"') then delete(result, length(result), 1);
end;

function DllExternalProc(Sender: TPSPascalCompiler; Decl: TPSParametersDecl; const OriginalName, FExternal: tbtstring): TPSRegProc;
var
  FuncName,
  Name,
  FuncCC, s, s2: AnsiString;
  CC: TDllCallingConvention;
  DelayLoad, LoadWithAlteredSearchPath: Boolean;

begin
  Name := FastUpperCase(OriginalName);
  DelayLoad := False;
  LoadWithAlteredSearchPath := false;
  FuncCC := FExternal;

  if (pos(tbtChar('@'), FuncCC) = 0) then
  begin
    Sender.MakeError('', ecCustomError, tbtString(RPS_Invalid_External));
    Result := nil;
    exit;
  end;
  FuncName := copy(FuncCC, 1, rpos('@', FuncCC)-1)+#0;
  delete(FuncCc, 1, length(FuncName));
  if pos(tbtchar(' '), Funccc) <> 0 then
  begin
    if FuncCC[1] = '"' then
    begin
      Delete(FuncCC, 1, 1);
      FuncName := RemoveQuotes(copy(FuncCC, 1, pos(tbtchar('"'), FuncCC)-1))+#0+FuncName;
      Delete(FuncCC,1, pos(tbtchar('"'), FuncCC));
      if (FuncCC <> '') and( FuncCC[1] = ' ') then delete(FuncCC,1,1);
    end else
    begin
      FuncName := copy(FuncCc, 1, pos(tbtchar(' '),FuncCC)-1)+#0+FuncName;
      Delete(FuncCC, 1, pos(tbtchar(' '), FuncCC));
    end;
    if pos(tbtchar(' '), FuncCC) > 0 then
    begin
      s := Copy(FuncCC, pos(tbtchar(' '), Funccc)+1, MaxInt);
      FuncCC := FastUpperCase(Copy(FuncCC, 1, pos(tbtchar(' '), FuncCC)-1));
      Delete(FuncCC, pos(tbtchar(' '), Funccc), MaxInt);
      repeat
        if pos(tbtchar(' '), s) > 0 then begin
          s2 := Copy(s, 1, pos(tbtchar(' '), s)-1);
          delete(s, 1, pos(tbtchar(' '), s));
        end else begin
          s2 := s;
          s := '';
        end;
        if FastUppercase(s2) = 'DELAYLOAD' then
          DelayLoad := True
        {$IFNDEF LINUX}
        else
        if FastUppercase(s2) = 'LOADWITHALTEREDSEARCHPATH' then
          LoadWithAlteredSearchPath := True
        {$ENDIF}
        else
        begin
          Sender.MakeError('', ecCustomError, 'Invalid External');
          Result := nil;
          exit;
        end;
      until s = '';

    end else
      FuncCC := FastUpperCase(FuncCC);
    if FuncCC = 'STDCALL' then cc := ClStdCall else
    if FuncCC = 'CDECL' then cc := ClCdecl else
    if FuncCC = 'REGISTER' then cc := clRegister else
    if FuncCC = 'PASCAL' then cc := clPascal else
    begin
      Sender.MakeError('', ecCustomError, tbtstring(RPS_InvalidCallingConvention));
      Result := nil;
      exit;
    end;
  end else
  begin
    FuncName := RemoveQuotes(FuncCC)+#0+FuncName;
    FuncCC := '';
    cc := DefaultCC;
  end;
  FuncName := 'dll:'+FuncName+tbtchar(cc)+tbtchar(bytebool(DelayLoad)) +tbtchar(bytebool(LoadWithAlteredSearchPath))+ declToBits(Decl);
  Result := TPSRegProc.Create;
  Result.ImportDecl := FuncName;
  Result.Decl.Assign(Decl);
  Result.Name := Name;
  Result.OrgName := OriginalName;
  Result.ExportName := False;
end;

procedure RegisterDll_Compiletime(cs: TPSPascalCompiler);
begin
  cs.OnExternalProc := DllExternalProc;
  cs.AddFunction('procedure UnloadDll(S: string)');
  cs.AddFunction('function DllGetLastError: LongInt');
end;

begin
  DefaultCc := clRegister;
end.

