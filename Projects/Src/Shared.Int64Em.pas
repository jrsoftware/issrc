unit Shared.Int64Em;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Declaration of the Integer64 type - which represents an *unsigned* 64-bit
  integer value - and functions for manipulating Integer64's.
}

interface

type
  Integer64 = record
    Lo, Hi: LongWord;
    class operator Implicit(const A: Integer64): Int64;
    class operator Implicit(const A: Int64): Integer64;
  end;

function Compare64(const N1, N2: Integer64): Integer;
procedure Dec64(var X: Integer64; N: LongWord);
function Div64(var X: Integer64; const Divisor: LongWord): LongWord;
function Inc6464(var X: Integer64; const N: Integer64): Boolean;
function Mod64(const X: Integer64; const Divisor: LongWord): LongWord;
procedure Multiply32x32to64(N1, N2: LongWord; var X: Integer64);
function StrToInteger64(const S: String; var X: Integer64): Boolean; overload;
function StrToInteger64(const S: String; var X: Int64): Boolean; overload;

implementation

uses
  SysUtils;

function Compare64(const N1, N2: Integer64): Integer;
{ If N1 = N2, returns 0.
  If N1 > N2, returns 1.
  If N1 < N2, returns -1. }
asm
  { Compare high words }
  mov  ecx, [eax+4]
  cmp  ecx, [edx+4]
  ja   @@return1
  jb   @@returnminus1
  { High words equal; compare low words }
  mov  ecx, [eax]
  cmp  ecx, [edx]
  ja   @@return1
  jb   @@returnminus1
  jmp  @@return0
@@return1:
  xor  eax, eax
  inc  eax
  jmp  @@exit
@@returnminus1:
  or   eax, -1
  jmp  @@exit
@@return0:
  xor  eax,eax
@@exit:
end;

procedure Dec64(var X: Integer64; N: LongWord);
asm
  sub  [eax], edx
  sbb  dword ptr [eax+4], 0
end;

function Inc64(var X: Integer64; N: LongWord): Boolean;
{ Adds N to X. In case of overflow, False is returned. }
asm
  add  [eax], edx
  adc  dword ptr [eax+4], 0
  setnc al
end;

function Inc6464(var X: Integer64; const N: Integer64): Boolean;
{ Adds N to X. In case of overflow, False is returned. }
asm
  mov  ecx, [edx]
  add  [eax], ecx
  mov  ecx, [edx+4]
  adc  [eax+4], ecx
  setnc al
end;

procedure Multiply32x32to64(N1, N2: LongWord; var X: Integer64);
{ Multiplies two 32-bit unsigned integers together and places the result
  in X. }
asm
  mul  edx    { Multiplies EAX by EDX, places 64-bit result in EDX:EAX }
  mov  [ecx], eax
  mov  [ecx+4], edx
end;

function Mul64(var X: Integer64; N: LongWord): Boolean;
{ Multiplies X by N, and overwrites X with the result. In case of overflow,
  False is returned (X is valid but truncated to 64 bits). }
asm
  push esi
  push ebx
  mov  esi, eax
  mov  ecx, edx

  { Multiply high part }
  mov  eax, [esi+4]
  mul  ecx            { CF set if resulting EDX <> 0 }
  setnc bl
  mov  [esi+4], eax

  { Multiply low part, carry to high part }
  mov  eax, [esi]
  mul  ecx
  mov  [esi], eax
  add  [esi+4], edx   { CF set on overflow }
  setnc al
  and  al, bl

  pop  ebx
  pop  esi
end;

function Div64(var X: Integer64; const Divisor: LongWord): LongWord;
{ Divides X by Divisor, and overwrites X with the quotient. Returns the
  remainder. }
asm
  push ebx
  push esi
  mov  esi, eax
  mov  ecx, edx

  mov  eax, [esi]
  mov  ebx, [esi+4]

  { Divide EBX:EAX by ECX. Quotient is stored in EBX:EAX, remainder in EDX. }
  xchg eax, ebx
  xor  edx, edx
  div  ecx
  xchg eax, ebx
  div  ecx

  mov  [esi], eax
  mov  [esi+4], ebx
  mov  eax, edx

  pop  esi
  pop  ebx
end;

function Mod64(const X: Integer64; const Divisor: LongWord): LongWord;
{ Divides X by Divisor and returns the remainder. Unlike Div64, X is left
  intact. }
asm
  push ebx
  push esi
  mov  esi, eax
  mov  ecx, edx

  mov  eax, [esi]
  mov  ebx, [esi+4]

  { Divide EBX:EAX by ECX. Quotient is stored in EBX:EAX, remainder in EDX. }
  xchg eax, ebx
  xor  edx, edx
  div  ecx
  xchg eax, ebx
  div  ecx

  mov  eax, edx

  pop  esi
  pop  ebx
end;

function StrToInteger64(const S: String; var X: Integer64): Boolean;
{ Converts a string containing an unsigned decimal number, or hexadecimal
  number prefixed with '$', into an Integer64. Returns True if successful,
  or False if invalid characters were encountered or an overflow occurred.
  Supports digits separators. }
var
  Len, Base, StartIndex, I: Integer;
  V: Integer64;
  C: Char;
begin
  Result := False;

  Len := Length(S);
  Base := 10;
  StartIndex := 1;
  if Len > 0 then begin
    if S[1] = '$' then begin
      Base := 16;
      Inc(StartIndex);
    end else if S[1] = '_' then
      Exit;
  end;

  if (StartIndex > Len) or (S[StartIndex] = '_') then
    Exit;
  V := 0;
  for I := StartIndex to Len do begin
    C := UpCase(S[I]);
    case C of
      '0'..'9':
        begin
          if not Mul64(V, Base) then
            Exit;
          if not Inc64(V, Ord(C) - Ord('0')) then
            Exit;
        end;
      'A'..'F':
        begin
          if Base <> 16 then
            Exit;
          if not Mul64(V, Base) then
            Exit;
          if not Inc64(V, Ord(C) - (Ord('A') - 10)) then
            Exit;
        end;
      '_':
        { Ignore }
    else
      Exit;
    end;
  end;
  X := V;
  Result := True;
end;

function StrToInteger64(const S: String; var X: Int64): Boolean;
begin
  var X2: Integer64 := X;
  Result := StrToInteger64(S, X2);
  X := X2;
end;

{ Integer64 }

class operator Integer64.Implicit(const A: Int64): Integer64;
begin
  Int64Rec(Result) := Int64Rec(A);
end;

class operator Integer64.Implicit(const A: Integer64): Int64;
begin
  Int64Rec(Result) := Int64Rec(A);
end;

end.
