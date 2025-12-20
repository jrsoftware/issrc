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

{ FRAMEWORK_VCL is available as of Delphi 11.1, so define it manually here when we need it, to
  support Delphi 10.4 }
{$IF Defined(COMPIL32PROJ) or Defined(SETUPPROJ)}
  {$DEFINE FRAMEWORK_VCL}
{$IFEND}

uses
  {$IFDEF FRAMEWORK_VCL} Windows, UITypes, Controls, Graphics, {$ENDIF} SysUtils;

function ULength(const S: String): Cardinal; overload; inline;
function ULength(const S: RawByteString): Cardinal; overload; inline;
function ULength(const S: WideString): Cardinal; overload; inline;
function ULength(const S: TBytes): Cardinal; overload; inline;
procedure UMove(const Source; var Dest; Count: NativeUInt);
procedure UFillChar(var Dest; Count: NativeUInt; const Value: Integer);
function UCompareMem(P1, P2: Pointer; Length: NativeUInt): Boolean;

{$IFDEF FRAMEWORK_VCL}
function UColorToRGB(Color: TColor): TColorRef;
function UDrawTextBiDiModeFlags(const Control: TControl; const Flags: UINT): UINT;
{$ENDIF}

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
    var SignedCount := High(NativeInt);
    if Count < NativeUInt(SignedCount) then
      SignedCount := NativeInt(Count);
    Move(SourceBuf^, DestBuf^, SignedCount);
    Dec(Count, SignedCount);
    Inc(SourceBuf, SignedCount);
    Inc(DestBuf, SignedCount);
  end;
end;

procedure UFillChar(var Dest; Count: NativeUInt; const Value: Integer);
begin
  var DestBuf: PByte := @Dest;
  while Count > 0 do begin
    var SignedCount := High(NativeInt);
    if Count < NativeUInt(SignedCount) then
      SignedCount := NativeInt(Count);
    FillChar(DestBuf^, SignedCount, Value);
    Dec(Count, SignedCount);
    Inc(DestBuf, SignedCount);
  end;
end;

function UCompareMem(P1, P2: Pointer; Length: NativeUInt): Boolean;
begin
  Result := True;
  if P1 <> P2 then begin
    while Length > 0 do begin
      var SignedLength := High(NativeInt);
      if Length < NativeUInt(SignedLength) then
        SignedLength := NativeInt(Length);
      if not CompareMem(P1, P2, SignedLength) then
        Exit(False);
      Dec(Length, SignedLength);
      Inc(PByte(P1), SignedLength);
      Inc(PByte(P2), SignedLength);
    end;
  end;
end;

{$IFDEF FRAMEWORK_VCL}
function UColorToRGB(Color: TColor): TColorRef;
begin
  Result := TColorRef(ColorToRGB(Color));
end;

function UDrawTextBiDiModeFlags(const Control: TControl; const Flags: UINT): UINT;
begin
  Result := UINT(Control.DrawTextBiDiModeFlags(Integer(Flags)));
end;
{$ENDIF}

end.
