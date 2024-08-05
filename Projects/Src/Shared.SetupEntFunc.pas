unit Shared.SetupEntFunc;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Functions for handling records with embedded long strings
}

interface

uses
  Compression.Base;

procedure SEFreeRec(const P: Pointer; const NumStrings, NumAnsiStrings: Integer);
procedure SEDuplicateRec(OldP, NewP: Pointer; Bytes: Cardinal;
  const NumStrings, NumAnsiStrings: Integer);
procedure SECompressedBlockWrite(const W: TCompressedBlockWriter; var Buf;
  const Count: Cardinal; const NumStrings, NumAnsiStrings: Integer);
procedure SECompressedBlockRead(const R: TCompressedBlockReader; var Buf;
  const Count: Cardinal; const NumStrings, NumAnsiStrings: Integer);

implementation

procedure SEFreeRec(const P: Pointer; const NumStrings, NumAnsiStrings: Integer);
var
  AnsiP: Pointer;
begin
  if P = nil then Exit;
  if NumStrings > 0 then  { Finalize in Delphi versions < 5 can't be called with zero count }
    Finalize(String(P^), NumStrings);
  if NumAnsiStrings > 0 then begin
    AnsiP := P;
    Inc(Cardinal(AnsiP), NumStrings*SizeOf(Pointer));
    Finalize(AnsiString(AnsiP^), NumAnsiStrings);
  end;
  FreeMem(P);
end;

procedure SEDuplicateRec(OldP, NewP: Pointer; Bytes: Cardinal;
  const NumStrings, NumAnsiStrings: Integer);
var
  I: Integer;
begin
  for I := 1 to NumStrings do begin
    String(NewP^) := String(OldP^);
    Inc(Cardinal(OldP), SizeOf(Pointer));
    Inc(Cardinal(NewP), SizeOf(Pointer));
    Dec(Bytes, SizeOf(Pointer));
  end;
  for I := 1 to NumAnsiStrings do begin
    AnsiString(NewP^) := AnsiString(OldP^);
    Inc(Cardinal(OldP), SizeOf(Pointer));
    Inc(Cardinal(NewP), SizeOf(Pointer));
    Dec(Bytes, SizeOf(Pointer));
  end;
  Move(OldP^, NewP^, Bytes);
end;

procedure SECompressedBlockWrite(const W: TCompressedBlockWriter; var Buf;
  const Count: Cardinal; const NumStrings, NumAnsiStrings: Integer);
var
  P: Pointer;
  I: Integer;
  Len: Integer;
begin
  P := @Buf;
  for I := 1 to NumStrings do begin
    Len := Length(String(P^))*SizeOf(Char);
    W.Write(Len, SizeOf(Len));
    if Len <> 0 then
      W.Write(Pointer(P^)^, Len);
    Inc(Cardinal(P), SizeOf(Pointer));
  end;
  for I := 1 to NumAnsiStrings do begin
    Len := Length(AnsiString(P^));
    W.Write(Len, SizeOf(Len));
    if Len <> 0 then
      W.Write(Pointer(P^)^, Len);
    Inc(Cardinal(P), SizeOf(Pointer));
  end;
  W.Write(P^, Count - (Cardinal(NumStrings + NumAnsiStrings) * SizeOf(Pointer)));
end;

procedure SECompressedBlockRead(const R: TCompressedBlockReader; var Buf;
  const Count: Cardinal; const NumStrings, NumAnsiStrings: Integer);
var
  P: Pointer;
  I: Integer;
  Len: Integer;
  S: String;
  AnsiS: AnsiString;
begin
  P := @Buf;
  for I := 1 to NumStrings do begin
    R.Read(Len, SizeOf(Len));
    SetLength(S, Len div SizeOf(Char));
    if Len <> 0 then
      R.Read(S[1], Len);
    String(P^) := S;
    Inc(Cardinal(P), SizeOf(Pointer));
  end;
  for I := 1 to NumAnsiStrings do begin
    R.Read(Len, SizeOf(Len));
    SetLength(AnsiS, Len);
    if Len <> 0 then
      R.Read(AnsiS[1], Len);
    AnsiString(P^) := AnsiS;
    Inc(Cardinal(P), SizeOf(Pointer));
  end;
  R.Read(P^, Count - (Cardinal(NumStrings + NumAnsiStrings) * SizeOf(Pointer)));
end;

end.
