unit ASMInline;

interface

{ASM Inliner
 Nicholas Sherlock

 This is incomplete, I've only implemented enough to support InnoCallback.

 Instructions are stored in a TMemoryStream internally

 Instructions usually accept some combination of Registers, Immediates and
 Memory References. Memory References can either be of the simple form [EAX]
 (Where [EAX] is really a Delphi set), or the user can build the address with
 the TASMInline.addr() function. It'd be nice to have a function which builds
 the address from a string, too, allowing the more intuitive '[EAX+EBX*4+1000]'
 style.

 The generation of instruction-relative addresses generates Relocations, using
 SaveToMemory() automatically rebases using the relocations to make these correct
 in the final block of memory.

  !!!! Not all special cases have been implemented in WriteRegRef().

  Further reduced to just what's needed for Inno Setup by Martijn Laan

  Added x64 support by Martijn Laan
}

{$IFNDEF CPUX86}
{$IFNDEF CPUX64}
{$MESSAGE ERROR 'This needs updating for non-x86/x64 builds'}
{$ENDIF}
{$ENDIF}

{$IFDEF CPUX86}
{$WARN IMPLICIT_INTEGER_CAST_LOSS OFF}
{$WARN IMPLICIT_CONVERSION_LOSS OFF}
{$ENDIF}

uses Sysutils, Windows, Classes, Contnrs;

type
{$IFDEF CPUX86}
  TModMode = (mmNaked, mmDeref, mmDisp8, mmDisp32);
  TRegister32 = (EAX, EBX, ECX, EDX, ESP, EBP, ESI, EDI);
  TMemSize = (ms8, ms16, ms32, ms64);

  TMemoryAddress = record
    Size: TMemSize;
    UseBase: Boolean;
    Base, Index: TRegister32;
    Offset: Integer;
    Scale: Byte;
  end;

  EOperandSizeMismatch = class(Exception)
  public
    constructor Create;
  end;

  TRelocType = (rt32Bit);

  TReloc = class
  public
    Position: Cardinal;
    RelocType: TRelocType;
  end;
{$ELSE}
  TRegister64 = (RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15);
{$ENDIF}

  TASMInline = class
  private
    FBuffer: TMemoryStream;
{$IFDEF CPUX86}
    FRelocs: TObjectList;
    FBase: Cardinal;
    procedure AddRelocation(position: Cardinal; relocType: TRelocType);
    function GetReloc(index: Integer): TReloc;
    function RelocCount: Integer;
    property Relocs[index: Integer]: TReloc read GetReloc;
    procedure WriteRegRef(reg: byte; base: TRegister32; deref: Boolean; index: TRegister32; Offset: Integer; Scale: byte; usebase: Boolean); overload;
    procedure WriteRegRef(mem: TMemoryAddress; reg: TRegister32); overload;
    procedure WriteRegRef(reg: TRegister32; base: TRegister32; deref: Boolean; index: TRegister32 = EAX; Offset: Integer = 0; Scale: byte = 0; usebase: Boolean = true); overload;
    procedure Relocate(base: pointer);
{$ELSE}
    function RegCode(const R: TRegister64): Byte;
    procedure WriteREX(const W, R, X, B: Boolean);
{$ENDIF}
    procedure WriteByte(const B: Byte);
    procedure WriteInteger(const I: Integer);
{$IFNDEF CPUX86}
    procedure WriteUInt64(const U: UInt64);
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function SaveAsMemory: Pointer;
    function Size: Integer;
{$IFDEF CPUX86}
    function Addr(base: TRegister32; offset: Integer; size: TMemSize = ms32): TMemoryAddress; overload;

    //PUSH reg
    procedure Push(reg: TRegister32); overload;

    //POP reg
    procedure Pop(reg: TRegister32);

    //JUMP rel32
    procedure Jmp(target: pointer); overload;

    //MOV reg, imm
    procedure Mov(reg: TRegister32; b: longword); overload;
    //MOV reg, mem and MOV mem, reg
    procedure Mov(mem: TMemoryAddress; reg: TRegister32); overload;
    procedure Mov(reg: TRegister32; mem: TMemoryAddress); overload;
{$ELSE}
    procedure MovRegReg(const Dest, Src: TRegister64);
    procedure MovRegImm64(const Dest: TRegister64; const Value: UInt64);
    procedure MovRegMemRSP(const Dest: TRegister64; const Disp: Integer);
    procedure MovMemRSPReg(const Disp: Integer; const Src: TRegister64);
    procedure SubRsp(const Amount: Integer);
    procedure AddRsp(const Amount: Integer);
    procedure CallReg(const Reg: TRegister64);
    procedure Ret;
{$ENDIF}
  end;

implementation

{$IFDEF CPUX86}

constructor EOperandSizeMismatch.create;
begin
  inherited Create('Operand size mismatch');
end;

{Throw an exception if test<>match. Poor man's assert().
 Could overload to add other sorts of tests}

procedure require(test: TMemSize; match: TMemSize);
begin
  if test <> match then
    raise EOperandSizeMismatch.Create;
end;

function regnum(reg: TRegister32): byte; overload;
begin
  case reg of
    EAX: result := 0;
    EBX: result := 3;
    ECX: result := 1;
    EDX: result := 2;
    ESP: result := 4;
    EBP: result := 5;
    ESI: result := 6;
    EDI: result := 7;
  else
    raise Exception.create('Unknown register...');
  end;
end;

function ModModeNum(m: TModMode): byte;
begin
  case m of
    mmDeref: result := 0;
    mmDisp8: result := 1;
    mmDisp32: result := 2;
    mmNaked: result := 3;
  else
    raise Exception.create('Invalid mod mode: ' + inttostr(ord(m)));
  end;
end;

function EncodeSIB(scale, index, base: byte): byte;
begin
  result := byte(base or (index shl 3) or (scale shl 6));
end;

function EncodeModRM(aMod, aReg, aRM: byte): byte; overload;
begin
  result := byte((aMod shl 6) or (areg shl 3) or aRM);
end;

{$ENDIF}

{ TASMInline }

constructor TASMInline.Create;
begin
  FBuffer := TMemoryStream.Create;
{$IFDEF CPUX86}
  FRelocs := TobjectList.Create;
{$ENDIF}
end;

destructor TASMInline.Destroy;
begin
{$IFDEF CPUX86}
  FRelocs.Free;
{$ENDIF}
  FBuffer.Free;
  inherited;
end;

{$IFDEF CPUX86}

{$IFOPT R+}
{$DEFINE RESTORER}
{$R-}
{$ENDIF}
{$IFOPT Q+}
{$DEFINE RESTOREQ}
{$Q-}
{$ENDIF}
procedure TASMInline.Relocate(base: pointer);
var diff, orig: integer;
  i: integer;
  reloc: TReloc;
begin
  const oldpos = fbuffer.Position;
  try

    diff := -(longword(base) - fbase);

    for i := 0 to RelocCount - 1 do begin
      reloc := Relocs[i];
      case reloc.relocType of
        rt32Bit: begin
            fbuffer.Seek(reloc.position, soBeginning);
            fbuffer.Read(orig, sizeof(orig));
            fbuffer.seek(-sizeof(orig), soCurrent);
            orig := LongWord(orig + diff);
            fbuffer.write(orig, sizeof(orig));
          end;
      end;
    end;
    fbase := longword(base);
  finally
    fbuffer.position := oldpos;
  end;
end;
{$IFDEF RESTORER}
{$R+}
{$ENDIF}
{$IFDEF RESTOREQ}
 {Q+}
{$ENDIF}

function TASMInline.GetReloc(index: integer): TReloc;
begin
  Result := TReloc(frelocs[index]);
end;

function TASMInline.RelocCount: integer;
begin
  Result := frelocs.Count;
end;

procedure TASMInline.AddRelocation(position: longword; relocType: TRelocType);
var reloc: TReloc;
begin
  reloc := TReloc.Create;
  reloc.position := position;
  reloc.relocType := relocType;
  frelocs.add(reloc);
end;

function TASMInline.Addr(base: TRegister32; offset: Integer; size: TMemSize = ms32): TMemoryAddress;
begin
  result.base := base;
  result.scale := 0; //don't use Index
  result.offset := offset;
  result.size := size;
  result.usebase := true;
end;

procedure TASMInline.Pop(reg: TRegister32);
begin
  writebyte($58 + regnum(reg));
end;

procedure TASMInline.Jmp(target: pointer);
begin
  writebyte($E9);
  AddRelocation(fbuffer.position, rt32bit);
  WriteInteger(integer(target) - (integer(fBase) + fbuffer.Position + 4));
end;

procedure TASMInline.Push(reg: TRegister32);
begin
  writebyte($50 + regnum(reg));
end;

procedure TASMInline.WriteRegRef(mem: TMemoryAddress; reg: TRegister32);
begin
  writeregref(reg, mem.base, true, mem.index, mem.offset, mem.scale, mem.usebase);
end;

//Write the MODR/M and SIB byte for the given register or memory reference

procedure TASMInline.WriteRegRef(reg: TRegister32; base: TRegister32; deref: boolean; index: TRegister32 = EAX; Offset: integer = 0; Scale: byte = 0; usebase: boolean = true);
begin
  WriteRegRef(regnum(reg), base, deref, index, Offset, scale, usebase);
end;

procedure TASMInline.WriteRegRef(reg: byte; base: TRegister32; deref: boolean; index: TRegister32; Offset: integer; Scale: byte; usebase: boolean);
type TOffSize = (osNone, os8, os32);
var mode: TModMode;
  offsize: TOffSize;
  useSIB: boolean;
  areg, arm: Byte;
begin
  if not deref then begin
    mode := mmNaked;
    offsize := osNone;
  end else
    if usebase = false then begin
      offsize := os32;
      mode := mmDeref;
      base := EBP; //the "no base" value
    end else
      if Offset = 0 then begin
        mode := mmDeref;
        offsize := osNone;
      end else
        if (offset >= -128) and (offset < 128) then begin //signed byte
          mode := mmDisp8;
          offsize := os8;
        end else begin
          mode := mmDisp32;
          offsize := os32;
        end;

  if (mode <> mmnaked) then begin
    usesib := (Scale > 0) or (base = ESP);
  end else usesib := false;

  if useSIB then begin //calculate scale, easiest just to use a case statement..
    case scale of
      0: begin //dont want an index value
          index := ESP; //"none" value
        end;
      1: scale := 0;
      2: scale := 1;
      4: scale := 2;
      8: scale := 3;
    else raise exception.create('Invalid scale, valid values are 1,2,4,8.');
    end;
  end;

  if (not useSIB) and (mode = mmDeref) and (base = EBP) then begin
  //not available, use [EBP+0] instead
    mode := mmDisp8;
    offsize := os8;
    Offset := 0;
  end;

  arm := regnum(base);
  areg := reg;

  if usesib then
    WriteByte(EncodeModRM(ModModeNum(mode), areg, 4)) else
    WriteByte(EncodeModRM(ModModeNum(mode), areg, arm));

  if usesib then begin
    WriteByte(EncodeSIB(Scale, regnum(index), regnum(base)));
  end;

    //Do we have to append an offset?
  case offsize of
    os8: WriteByte(byte(offset)); //ignore sign..
    os32: WriteInteger(offset);
  end;
end;

procedure TASMInline.Mov(mem: TMemoryAddress; reg: TRegister32);
begin
  require(mem.size, ms32);
  WriteByte($89);
  WriteRegRef(mem, reg);
end;

procedure TASMInline.Mov(reg: TRegister32; mem: TMemoryAddress);
begin
  require(mem.size, ms32);
  WriteByte($8B);
  WriteRegRef(mem, reg);
end;

procedure TASMInline.Mov(reg: TRegister32; b: longword);
begin
  writebyte($B8 + regnum(reg));
  writeInteger(Integer(b));
end;

{$ELSE}

function TASMInline.RegCode(const R: TRegister64): Byte;
begin
  Result := Byte(R);
end;

procedure TASMInline.WriteREX(const W, R, X, B: Boolean);
var
  Prefix: Byte;
begin
  Prefix := $40;
  if W then
    Inc(Prefix, $08);
  if R then
    Inc(Prefix, $04);
  if X then
    Inc(Prefix, $02);
  if B then
    Inc(Prefix, $01);
  WriteByte(Prefix);
end;

procedure TASMInline.MovRegReg(const Dest, Src: TRegister64);
var
  DestCode, SrcCode: Byte;
begin
  DestCode := RegCode(Dest);
  SrcCode := RegCode(Src);
  WriteREX(True, SrcCode >= 8, False, DestCode >= 8);
  WriteByte($89);
  WriteByte($C0 or ((SrcCode and 7) shl 3) or (DestCode and 7));
end;

procedure TASMInline.MovRegImm64(const Dest: TRegister64; const Value: UInt64);
var
  DestCode: Byte;
begin
  DestCode := RegCode(Dest);
  WriteREX(True, False, False, DestCode >= 8);
  WriteByte($B8 + (DestCode and 7));
  WriteUInt64(Value);
end;

procedure TASMInline.MovRegMemRSP(const Dest: TRegister64; const Disp: Integer);
var
  DestCode: Byte;
begin
  DestCode := RegCode(Dest);
  WriteREX(True, DestCode >= 8, False, False);
  WriteByte($8B);
  WriteByte($84 or ((DestCode and 7) shl 3));
  WriteByte($24);
  WriteInteger(Disp);
end;

procedure TASMInline.MovMemRSPReg(const Disp: Integer; const Src: TRegister64);
var
  SrcCode: Byte;
begin
  SrcCode := RegCode(Src);
  WriteREX(True, SrcCode >= 8, False, False);
  WriteByte($89);
  WriteByte($84 or ((SrcCode and 7) shl 3));
  WriteByte($24);
  WriteInteger(Disp);
end;

procedure TASMInline.SubRsp(const Amount: Integer);
begin
  WriteByte($48);
  WriteByte($81);
  WriteByte($EC);
  WriteInteger(Amount);
end;

procedure TASMInline.AddRsp(const Amount: Integer);
begin
  WriteByte($48);
  WriteByte($81);
  WriteByte($C4);
  WriteInteger(Amount);
end;

procedure TASMInline.CallReg(const Reg: TRegister64);
var
  RegValue: Byte;
begin
  RegValue := RegCode(Reg);
  WriteREX(False, False, False, RegValue >= 8);
  WriteByte($FF);
  WriteByte($D0 + (RegValue and 7));
end;

procedure TASMInline.Ret;
begin
  WriteByte($C3);
end;

{$ENDIF}

function TASMInline.SaveAsMemory: Pointer;
var
  buf: Pointer;
  oldprotect: Cardinal;
begin
  GetMem(buf, size);
  VirtualProtect(buf, Size, PAGE_EXECUTE_READWRITE, oldprotect);
{$IFDEF CPUX86}
  Relocate(buf);
{$ENDIF}
  Move(fbuffer.memory^, buf^, size);
  result := buf;
end;

function TASMInline.Size: Integer;
begin
  Result := FBuffer.Size;
end;

procedure TASMInline.WriteByte(const B: Byte);
begin
  FBuffer.Write(B, SizeOf(B));
end;

procedure TASMInline.WriteInteger(const I: Integer);
begin
  FBuffer.Write(I, SizeOf(I));
end;

{$IFNDEF CPUX86}
procedure TASMInline.WriteUInt64(const U: UInt64);
begin
  FBuffer.Write(U, SizeOf(U));
end;
{$ENDIF}

end.

