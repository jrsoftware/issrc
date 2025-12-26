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
      TArrayOfObject = array of TObject;
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
    function GetClassArray(const ItemNo: Longint; const FieldNo: Longint = -1): TArrayOfObject;
    function GetIntArray(const ItemNo: Longint; const FieldNo: Longint = -1): TArrayOfInteger;
    function GetNativeInt(const ItemNo: Longint): NativeInt;
    function GetNativeUInt(const ItemNo: Longint): NativeUInt;
    function GetProc(const ItemNo: Longint; const Exec: TPSExec): TMethod;
    function GetStringArray(const ItemNo: Longint; const FieldNo: Longint = -1): TArrayOfString;
    function InitArrayBuilder(const ItemNo: LongInt; const FieldNo: Longint = -1): TArrayBuilder;
    function InitArrayEnumerator(const ItemNo: LongInt; const FieldNo: Longint = -1): TArrayEnumerator;
    procedure SetArray(const ItemNo: Longint; const Data: TArray<String>; const FieldNo: Longint = -1); overload;
    procedure SetArray(const ItemNo: Longint; const Data: TStrings; const FieldNo: Longint = -1); overload;
    procedure SetInt(const ItemNo: Longint; const Data: Integer; const FieldNo: Longint = -1);
    procedure SetInt64(const ItemNo: Longint; const Data: Int64; const FieldNo: Longint = -1);
    procedure SetNativeInt(const ItemNo: Longint; const Data: NativeInt; const FieldNo: Longint = -1);
    procedure SetNativeUInt(const ItemNo: Longint; const Data: NativeUInt; const FieldNo: Longint = -1);
    procedure SetUInt(const ItemNo: Longint; const Data: Cardinal; const FieldNo: Longint = -1);
    procedure SetUInt64(const ItemNo: Longint; const Data: UInt64; const FieldNo: Longint = -1);
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

function TPSStackHelper.GetClassArray(const ItemNo, FieldNo: Longint): TArrayOfObject;
begin
  var N: Integer;
  var Arr := GetArray(ItemNo, FieldNo, N);
  SetLength(Result, N);
  for var I := 0 to N-1 do
    Result[I] := VNGetObject(PSGetArrayField(Arr, I));
end;

function TPSStackHelper.GetIntArray(const ItemNo, FieldNo: Longint): TArrayOfInteger;
begin
  var N: Integer;
  var Arr := GetArray(ItemNo, FieldNo, N);
  SetLength(Result, N);
  for var I := 0 to N-1 do
    Result[I] := VNGetInt(PSGetArrayField(Arr, I));
end;

function TPSStackHelper.GetNativeInt(const ItemNo: Longint): NativeInt;
begin
{$IFNDEF WIN64}
  Result := GetInt(ItemNo);
{$ELSE}
  Result := GetInt64(ItemNo);
{$ENDIF}
end;

function TPSStackHelper.GetNativeUInt(const ItemNo: Longint): NativeUInt;
begin
{$IFNDEF WIN64}
  Result := GetUInt(ItemNo);
{$ELSE}
  Result := GetUInt64(ItemNo);
{$ENDIF}
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
  var N := Integer(System.Length(Data));
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

procedure TPSStackHelper.SetUInt(const ItemNo: Longint; const Data: Cardinal;
  const FieldNo: Longint);
begin
  if FieldNo >= 0 then begin
    var PSVariantIFC := NewTPSVariantRecordIFC(Items[ItemNo], FieldNo);
    VNSetUInt(PSVariantIFC, Data);
  end else
    inherited SetUInt(ItemNo, Data)
end;

procedure TPSStackHelper.SetInt64(const ItemNo: Longint; const Data: Int64;
  const FieldNo: Longint);
begin
  if FieldNo >= 0 then begin
    var PSVariantIFC := NewTPSVariantRecordIFC(Items[ItemNo], FieldNo);
    VNSetInt64(PSVariantIFC, Data);
  end else
    inherited SetInt64(ItemNo, Data)
end;

procedure TPSStackHelper.SetUInt64(const ItemNo: Longint; const Data: UInt64;
  const FieldNo: Longint);
begin
  if FieldNo >= 0 then begin
    var PSVariantIFC := NewTPSVariantRecordIFC(Items[ItemNo], FieldNo);
    VNSetUInt64(PSVariantIFC, Data);
  end else
    inherited SetUInt64(ItemNo, Data)
end;

procedure TPSStackHelper.SetNativeInt(const ItemNo: Longint; const Data: NativeInt;
  const FieldNo: Longint);
begin
{$IFNDEF WIN64}
  SetInt(ItemNo, Data, FieldNo);
{$ELSE}
  SetInt64(ItemNo, Data, FieldNo);
{$ENDIF}
end;

procedure TPSStackHelper.SetNativeUInt(const ItemNo: Longint; const Data: NativeUInt;
  const FieldNo: Longint);
begin
{$IFNDEF WIN64}
  SetUInt(ItemNo, Data, FieldNo);
{$ELSE}
  SetUInt64(ItemNo, Data, FieldNo);
{$ENDIF}
end;

end.