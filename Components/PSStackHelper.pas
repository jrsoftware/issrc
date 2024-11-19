unit PSStackHelper;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  ROPS TPSStack helper class
}

interface

uses
  Classes,
  uPSRuntime;

type
  TPSStackHelper = class helper for TPSStack
  private
    function GetArray(const ItemNo, FieldNo: Longint; out N: Integer): TPSVariantIFC;
    function SetArray(const ItemNo, FieldNo: Longint; const N: Integer): TPSVariantIFC; overload;
  public
    type
      TArrayOfInteger = array of Integer;
      TArrayOfString = array of String;
      TArrayBuilder = record
        Arr: TPSVariantIFC;
        I: Integer;
        procedure Add(const Data: String);
      end;
      TArrayEnumerator = record
        Arr: TPSVariantIFC;
        N, I: Integer;
        function HasNext: Boolean;
        function Next: String;
      end;
    function GetChar(const ItemNo: Longint): Char;
    function GetIntArray(const ItemNo: Longint; const FieldNo: Longint = -1): TArrayOfInteger;
    function GetProc(const ItemNo: Longint; const Exec: TPSExec): TMethod;
    function GetStringArray(const ItemNo: Longint; const FieldNo: Longint = -1): TArrayOfString;
    function InitArrayBuilder(const ItemNo: LongInt; const FieldNo: Longint = -1): TArrayBuilder;
    function InitArrayEnumerator(const ItemNo: LongInt; const FieldNo: Longint = -1): TArrayEnumerator;
    procedure SetArray(const ItemNo: Longint; const Data: TArray<String>; const FieldNo: Longint = -1); overload;
    procedure SetArray(const ItemNo: Longint; const Data: TStrings; const FieldNo: Longint = -1); overload;
    procedure SetInt(const ItemNo: Longint; const Data: Integer; const FieldNo: Longint = -1);
  end;

implementation

function TPSStackHelper.GetArray(const ItemNo, FieldNo: Longint;
  out N: Integer): TPSVariantIFC;
begin
  if FieldNo >= 0 then
    Result := NewTPSVariantRecordIFC(Items[ItemNo], FieldNo)
  else
    Result := NewTPSVariantIFC(Items[ItemNo], True);
  N := PSDynArrayGetLength(Pointer(Result.Dta^), Result.aType);
end;

function TPSStackHelper.SetArray(const ItemNo, FieldNo: Longint;
  const N: Integer): TPSVariantIFC;
begin
  if FieldNo >= 0 then
    Result := NewTPSVariantRecordIFC(Items[ItemNo], FieldNo)
  else
    Result := NewTPSVariantIFC(Items[ItemNo], True);
  PSDynArraySetLength(Pointer(Result.Dta^), Result.aType, N);
end;

function TPSStackHelper.GetChar(const ItemNo: Longint): Char;
begin
  var S := GetString(ItemNo);
  if S <> '' then
    Result := S[1]
  else
    Result := #0;
end;

function TPSStackHelper.GetIntArray(const ItemNo, FieldNo: Longint): TArrayOfInteger;
begin
  var N: Integer;
  var Arr := GetArray(ItemNo, FieldNo, N);
  SetLength(Result, N);
  for var I := 0 to N-1 do
    Result[I] := VNGetInt(PSGetArrayField(Arr, I));
end;

function TPSStackHelper.GetProc(const ItemNo: Longint; const Exec: TPSExec): TMethod;
begin
  var P := PPSVariantProcPtr(Items[ItemNo]);
  { ProcNo 0 means nil was passed by the script and GetProcAsMethod will then return a (nil, nil) TMethod }
  Result := Exec.GetProcAsMethod(P.ProcNo);
end;

function TPSStackHelper.GetStringArray(const ItemNo, FieldNo: Longint): TArrayOfString;
begin
  var N: Integer;
  var Arr := GetArray(ItemNo, FieldNo, N);
  SetLength(Result, N);
  for var I := 0 to N-1 do
    Result[I] := VNGetString(PSGetArrayField(Arr, I));
end;

function TPSStackHelper.InitArrayBuilder(const ItemNo, FieldNo: Longint): TArrayBuilder;
begin
  Result.Arr := SetArray(ItemNo, FieldNo, 0);
  Result.I := 0;
end;

procedure TPSStackHelper.TArrayBuilder.Add(const Data: String);
begin
  PSDynArraySetLength(Pointer(Arr.Dta^), Arr.aType, I+1);
  VNSetString(PSGetArrayField(Arr, I), Data);
  Inc(I);
end;

function TPSStackHelper.InitArrayEnumerator(const ItemNo, FieldNo: Longint): TArrayEnumerator;
begin
  Result.Arr := GetArray(ItemNo, FieldNo, Result.N);
  Result.I := 0;
end;

function TPSStackHelper.TArrayEnumerator.HasNext: Boolean;
begin
  Result := I < N;
end;

function TPSStackHelper.TArrayEnumerator.Next: String;
begin
  Result := VNGetString(PSGetArrayField(Arr, I));
  Inc(I);
end;

procedure TPSStackHelper.SetArray(const ItemNo: Longint; const Data: TArray<String>; const FieldNo: Longint);
begin
  var N := System.Length(Data);
  var Arr := SetArray(ItemNo, FieldNo, N);
  for var I := 0 to N-1 do
    VNSetString(PSGetArrayField(Arr, I), Data[I]);
end;

procedure TPSStackHelper.SetArray(const ItemNo: Longint; const Data: TStrings; const FieldNo: Longint);
begin
  var N := Data.Count;
  var Arr := SetArray(ItemNo, FieldNo, N);
  for var I := 0 to N-1 do
    VNSetString(PSGetArrayField(Arr, I), Data[I]);
end;

procedure TPSStackHelper.SetInt(const ItemNo: Longint; const Data: Integer;
  const FieldNo: Longint);
begin
  if FieldNo >= 0 then begin
    var PSVariantIFC := NewTPSVariantRecordIFC(Items[ItemNo], FieldNo);
    VNSetInt(PSVariantIFC, Data);
  end else
    inherited SetInt(ItemNo, Data)
end;

end.