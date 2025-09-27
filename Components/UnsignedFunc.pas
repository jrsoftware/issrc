unit UnsignedFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Provides unsigned alternatives to Delphi functions that use signed types for parameters or return
  values where unsigned types would have been more appropriate
}

interface

uses
  SysUtils;

function ULength(const S: String): Cardinal; overload; inline;
function ULength(const S: RawByteString): Cardinal; overload; inline;
function ULength(const S: WideString): Cardinal; overload; inline;
function ULength(const S: TBytes): Cardinal; overload; inline;
procedure UMove(const Source; var Dest; Count: NativeUInt);

implementation

function ULength(const S: String): Cardinal;
begin
  Result := Cardinal(Length(S));
end;

function ULength(const S: RawByteString): Cardinal;
begin
  Result := Cardinal(Length(S));
end;

function ULength(const S: WideString): Cardinal;
begin
  Result := Cardinal(Length(S));
end;

function ULength(const S: TBytes): Cardinal;
begin
  Result := Cardinal(Length(S));
end;

procedure UMove(const Source; var Dest; Count: NativeUInt);
begin
  var SourceBuf: PByte := @Source;
  var DestBuf: PByte := @Dest;
  while Count > 0 do begin
    var MoveCount := High(NativeInt);
    if Count < NativeUInt(MoveCount) then
      MoveCount := NativeInt(Count);
    Move(SourceBuf^, DestBuf^, MoveCount);
    Dec(Count, MoveCount);
    Inc(SourceBuf, MoveCount);
    Inc(DestBuf, MoveCount);
  end;
end;

end.
