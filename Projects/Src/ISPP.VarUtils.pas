{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff

  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

unit ISPP.VarUtils;

interface

uses
  ISPP.Intf, ISPP.Base, ISPP.Preprocessor;

function SimplifyLValue(var Src: TIsppVariant): Boolean;
procedure MakeRValue(var Src: TIsppVariant);
function GetRValue(const Src: TIsppVariant): TIsppVariant;
procedure CopyExpVar(Src: TIsppVariant; var Dest: TIsppVariant);
procedure MakeInt(var Op: TIsppVariant; Value: Int64);
procedure MakeStr(var Op: TIsppVariant; const Value: string);
procedure MakeBool(var Op: TIsppVariant; Value: Boolean);
function TryStrToInt(Str: string; var Int: Integer): Boolean;
function ToInt(Op: TIsppVariant): TIsppVariant;
function ToStr(Op: TIsppVariant): TIsppVariant;

const
  NULL: TIsppVariant = (Typ: evNull;  AsStr: ''; AsInt: 0);

implementation

uses
  SysUtils, ISPP.Consts;

function SimplifyLValue(var Src: TIsppVariant): Boolean;
begin
  Result := Src.Typ = evLValue;
  if Result then
    while Src.AsPtr^.Typ = evLValue do Src := Src.AsPtr^;
end;

procedure MakeRValue(var Src: TIsppVariant);
begin
  while Src.Typ = evLValue do Src := Src.AsPtr^;
end;

function GetRValue(const Src: TIsppVariant): TIsppVariant;
begin
  Result := Src;
  MakeRValue(Result);
end;

procedure CopyExpVar(Src: TIsppVariant; var Dest: TIsppVariant);
begin
  MakeRValue(Src);
  if Src.Typ = evStr then
  begin
    Dest.Typ := evStr;
    Dest.AsInt := 0;
    Dest.AsStr := Src.AsStr;
  end
  else
    Move(Src, Dest, SizeOf(TIsppVariant));
end;

procedure MakeInt(var Op: TIsppVariant; Value: Int64);
begin
  Op.Typ := evInt;
  Op.AsInt := Value;
  Op.AsStr := '';
end;

procedure MakeStr(var Op: TIsppVariant; const Value: string);
begin
  Op.Typ := evStr;
  Op.AsInt := 0;
  Op.AsStr := Value;
end;

procedure MakeBool(var Op: TIsppVariant; Value: Boolean);
begin
  MakeInt(Op, Int64(Value));
end;

function TryStrToInt(Str: string; var Int: Integer): Boolean;
var
  Err: Integer;
begin
  if (Length(Str) > 2) and (Str[1] = '0') and ((Str[2] = 'x') or
    (Str[2] = 'X')) then
    Str := '$' + Copy(Str, 3, MaxInt);
  Val(Str, Int, Err);
  Result := Err = 0
end;

function ToInt(Op: TIsppVariant): TIsppVariant;
var
  I: Integer;
begin
  MakeRValue(Op);
  I := 0;
  if Op.Typ = evStr then
    if (Op.AsStr = '') or TryStrToInt(Op.AsStr, I) then
      MakeInt(Result, I)
    else
      raise EConvertError.CreateFmt('Cannot convert "%s" to integer', [Op.AsStr])
  else
    if Op.Typ = evNull then
      MakeInt(Result, 0)
    else
      CopyExpVar(Op, Result)
end;

function ToStr(Op: TIsppVariant): TIsppVariant;
begin
  MakeRValue(Op);
  if Op.Typ = evInt then
    MakeStr(Result, IntToStr(Op.AsInt))
  else
    if Op.Typ = evNull then
      MakeStr(Result, '')
    else
      MakeStr(Result, Op.AsStr)
end;

end.
