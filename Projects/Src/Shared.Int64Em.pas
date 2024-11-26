unit Shared.Int64Em;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Declaration of the Integer64 type - which represents an *unsigned* 64-bit
  integer value - and functions for manipulating Integer64's.
  (We can't use the Int64 type since it's only available in Delphi 4 and
  later.)
}

interface

type
  Integer64 = record
    Lo, Hi: LongWord;
  end;

function Compare64(const N1, N2: Integer64): Integer;
procedure Dec64(var X: Integer64; N: LongWord);
procedure Dec6464(var X: Integer64; const N: Integer64);
function Div64(var X: Integer64; const Divisor: LongWord): LongWord;
function Inc64(var X: Integer64; N: LongWord): Boolean;
function Inc6464(var X: Integer64; const N: Integer64): Boolean;
function Integer64ToStr(X: Integer64): String;
function Mod64(const X: Integer64; const Divisor: LongWord): LongWord;
function Mul64(var X: Integer64; N: LongWord): Boolean;
procedure Multiply32x32to64(N1, N2: LongWord; var X: Integer64);
procedure Shr64(var X: Integer64; Count: LongWord);
function StrToInteger64(const S: String; var X: Integer64): Boolean;

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

procedure Dec6464(var X: Integer64; const N: Integer64);
asm
  mov  ecx, [edx]
  sub  [eax], ecx
  mov  ecx, [edx+4]
  sbb  [eax+4], ecx
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

procedure Shr64(var X: Integer64; Count: LongWord);
{ Unsigned SHR of an Integer64 }
asm
  mov  ecx, edx
  push esi
  mov  esi, eax
  mov  eax, [esi]
  mov  edx, [esi+4]

  cmp  ecx, 32
  jb   @@below32
  cmp  ecx, 64
  jb   @@below64
  xor  edx, edx
  xor  eax, eax
  jmp  @@exit

@@below64:
  mov  eax, edx
  xor  edx, edx
  shr  eax, cl
  jmp  @@exit

@@below32:
  shrd eax, edx, cl
  shr  edx, cl

@@exit:
  mov  [esi], eax
  mov  [esi+4], edx
  pop  esi
end;

function StrToInteger64(const S: String; var X: Integer64): Boolean;
{ Converts a string containing an unsigned decimal number, or hexadecimal
  number prefixed with '$', into an Integer64. Returns True if successful,
  or False if invalid characters were encountered or an overflow occurred. }
var
  Len, Base, StartIndex, I: Integer;
  V: Integer64;
  C: Char;
begin
  Len := Length(S);
  Base := 10;
  StartIndex := 1;
  if (Len > 0) and (S[1] = '$') then begin
    Base := 16;
    Inc(StartIndex);
  end;

  Result := False;
  if StartIndex > Len then
    Exit;
  V.Lo := 0;
  V.Hi := 0;
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
    else
      Exit;
    end;
  end;
  X := V;
  Result := True;
end;

function Integer64ToStr(X: Integer64): String;
var
  I: Integer;
  Buf: array[0..31] of Char;  { need at least 20 characters }
begin
  I := High(Buf) + 1;
  repeat
    Dec(I);
    Buf[I] := Chr(Ord('0') + Div64(X, 10));
  until (X.Lo = 0) and (X.Hi = 0);
  SetString(Result, PChar(@Buf[I]), (High(Buf) + 1) - I);
end;

end.
