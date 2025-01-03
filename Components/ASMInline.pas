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
}

uses Sysutils, Windows, Classes, Contnrs;

type
  TModMode = (mmNaked, mmDeref, mmDisp8, mmDisp32);

  TRegister32 = (EAX, EBX, ECX, EDX, ESP, EBP, ESI, EDI);
  TRegister32Set = set of TRegister32;
  TRegister16 = (AX, BX, CX, DX, SP, BP, SI, DI);
  TRegister16Set = set of TRegister16;
  TRegister8 = (AH, AL, BH, BL, CH, CL, DH, DL);

  TCLRegister = CL..CL;

  TMemSize = (ms8, ms16, ms32, ms64);

  TMemoryAddress = record
    size: TMemSize;
    usebase: boolean;
    base, index: TRegister32;
    offset: integer;
    scale: byte;
  end;

  EOperandSizeMismatch = class(exception)
  public
    constructor create;
  end;

  TRelocType = (rt32Bit);

  TReloc = class
  public
    position: longword;
    relocType: TRelocType;
  end;

  TASMInline = class
  private
    fbuffer: TMemoryStream;
    frelocs: TObjectList;
    fbase: longword;

    procedure AddRelocation(position: longword; relocType: TRelocType);

    function GetReloc(index: integer): TReloc;
    function RelocCount: integer;
    property Relocs[index: integer]: TReloc read GetReloc;

    procedure WriteByte(b: byte);
    procedure WriteInteger(i: integer);
    procedure WriteLongWord(l: longword);
    procedure WriteRegRef(reg: byte; base: TRegister32; deref: boolean; index: TRegister32; Offset: integer; Scale: byte; usebase: boolean); overload;
    procedure WriteRegRef(mem: TMemoryAddress; reg: TRegister32); overload;
    procedure WriteRegRef(reg: TRegister32; base: TRegister32; deref: boolean; index: TRegister32 = EAX; Offset: integer = 0; Scale: byte = 0; usebase: boolean = true); overload;
  public
    function Size: integer;

    procedure Relocate(base: pointer);

    function SaveAsMemory: pointer;
    procedure SaveToMemory(target: pointer);

    constructor create;
    destructor Destroy; override;

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
  end;

implementation

constructor EOperandSizeMismatch.create;
begin
  inherited create('Operand size mismatch');
end;

{Throw an exception if test<>match. Poor man's assert().
 Could overload to add other sorts of tests}

procedure require(test: TMemSize; match: TMemSize);
begin
  if test <> match then
    raise EOperandSizeMismatch.create;
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
  else raise exception.create('Unknown register...');
  end;
end;

function ModModeNum(m: TModMode): byte;
begin
  case m of
    mmDeref: result := 0;
    mmDisp8: result := 1;
    mmDisp32: result := 2;
    mmNaked: result := 3;
  else raise exception.create('Invalid mod mode: ' + inttostr(ord(m)));
  end;
end;

function EncodeSIB(scale, index, base: byte): byte;
begin
  result := base or (index shl 3) or (scale shl 6);
end;

function EncodeModRM(aMod, aReg, aRM: byte): byte; overload;
begin
  result := (aMod shl 6) or (areg shl 3) or aRM;
end;

{$IFOPT R+}
{$DEFINE RESTORER}
{$R-}
{$ENDIF}
{$IFOPT Q+}
{$DEFINE RESTOREQ}
{$Q-}
{$ENDIF}
procedure TASMInline.Relocate(base: pointer);
var oldpos, diff, orig: integer;
  i: integer;
  reloc: TReloc;
begin
  oldpos := fbuffer.Position;
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
  result := TReloc(frelocs[index]);
end;

function TASMInline.RelocCount: integer;
begin
  result := frelocs.Count;
end;

procedure TASMInline.AddRelocation(position: longword; relocType: TRelocType);
var reloc: TReloc;
begin
  reloc := TReloc.Create;
  reloc.position := position;
  reloc.relocType := relocType;
  frelocs.add(reloc);
end;

function TASMInline.SaveAsMemory: pointer;
var buf: pointer;
  oldprotect: Cardinal;
begin
  GetMem(buf, size);
  VirtualProtect(buf, Size, PAGE_EXECUTE_READWRITE, oldprotect);
  SaveToMemory(buf);
  result := buf;
end;

procedure TASMInline.SaveToMemory(target: pointer);
begin
  Relocate(target);
  Move(fbuffer.memory^, target^, size);
end;

function TASMInline.Addr(base: TRegister32; offset: Integer; size: TMemSize = ms32): TMemoryAddress;
begin
  result.base := base;
  result.scale := 0; //don't use Index
  result.offset := offset;
  result.size := size;
  result.usebase := true;
end;

function TASMInline.Size: integer;
begin
  result := fbuffer.size;
end;

procedure TASMInline.WriteInteger(i: integer);
begin
  fbuffer.write(i, 4);
end;

procedure TASMInline.writelongword(l: longword);
begin
  fbuffer.write(l, 4);
end;

procedure TASMInline.writebyte(b: byte);
begin
  fbuffer.write(b, 1);
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
    os32: writelongword(longword(offset));
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
  writelongword(b);
end;

constructor TASMInline.create;
begin
  fbuffer := tmemorystream.create;
  frelocs := tobjectlist.create;
end;

destructor TASMInline.destroy;
begin
  fbuffer.free;
  frelocs.free;
  inherited;
end;

end.

